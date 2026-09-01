#include "mrilib.h"
#include "thd_datatable.h"
#include "thd_mapinfer.h"
#include "thd_simmatrix.h"
#include "thd_patterns.h"
#include "thd_phasefft.h"
#include <ctype.h>
#include <errno.h>
#include <unistd.h>

#ifdef USE_OMP
#include <omp.h>
#endif

#define PROGRAM_NAME "3dRSA"

/* Surface geodesic searchlight is optional: build with -DUSE_SUMA and link
   libSUMA to enable it.  Without it, 3dRSA still does volume + surface
   mask/atlas RSA (surface dsets load through THD_open_dataset, no SUMA). */
#ifdef USE_SUMA
#include "SUMA_suma.h"
#endif

/*----------------------------------------------------------------------------
  3dRSA -- Representational Similarity Analysis over an atlas/ROI mask.

  Supports inter-subject RSA (IS-RSA) and classic within-subject RSA, with
  models built from behavioral columns, from explicit matrices, or from a
  second imaging modality, tested one at a time or jointly by regression.
                                                     -- P Molfese, Jul 2026
------------------------------------------------------------------------------*/

/*--- what a feature vector is ---*/
#define MODE_BETA  1    /* voxel pattern within the ROI */
#define MODE_CONT  2    /* ROI-mean time course */
#define MODE_RDM   3    /* subject condition RDM triangle (second-order IS-RSA) */

/*--- null hypothesis for IS-RSA inference ---*/
#define NULL_LABELS    0 /* subject-label permutation (default) */
#define NULL_TIMESHIFT 1 /* circular shift of each subject's time series */
#define NULL_PHASE     2 /* independent Fourier-phase randomization */

/*--- classic-RSA population sampled by the primary null ---*/
#define CLASSIC_NULL_SUBJECTS   0 /* population effect: flip subject effects */
#define CLASSIC_NULL_CONDITIONS 1 /* fixed subjects: relabel condition axis */

/*--- scientific hypothesis requested for a model contrast ---*/
#define CONTRAST_LEGACY      0 /* retain historical mode-dependent behavior */
#define CONTRAST_SUPERIORITY 1 /* H0: paired model-performance difference is zero */
#define CONTRAST_ALIGNMENT   2 /* H0: no alignment with the neural geometry */

/*--- what the rows/columns of the matrices are ---*/
#define RDM_SUBJ  1     /* IS-RSA: rows are subjects */
#define RDM_BRICK 2     /* classic RSA: rows are sub-bricks (conditions) */

/*--- user-facing progress rendering ---*/
#define RSA_PROGRESS_AUTO 0
#define RSA_PROGRESS_BAR  1
#define RSA_PROGRESS_LINE 2
#define RSA_PROGRESS_OFF  3

#define RUN_NORM_NONE    0
#define RUN_NORM_DEMEAN  1
#define RUN_NORM_ZSCORE  2

#define RUN_ANALYSIS_CONCAT   0
#define RUN_ANALYSIS_SEPARATE 1
#define RUN_ANALYSIS_MEAN     2
#define RUL_MATCH             5 /* categorical equality model, local to 3dRSA */

/*! Long-table organization for native continuous runs.  The ordinary data
    table is collapsed to one row per subject after this map is built, so all
    existing behavioral-model code continues to operate on independent units. */
typedef struct {
   int nsub,nrun,nrow,run_col,total_nvals ;
   int *first_row,*row_sub,*row_run,*row_of,*run_nval,*offset ;
   char **run_label,**fname ;
} RSA_series_runs ;

typedef struct {
   char *column ; int icol,nlevel ;
   char **level ;              /* first-run-order level labels */
   int *run_level ;            /* [nrun], factor level for each labeled run */
} RSA_runfactor ;

typedef struct {
   char name[128] ; int ifactor,ipos,ineg ;
   float *weight ;             /* [nrun], +mean(pos) - mean(neg) */
} RSA_runcontrast ;

static void rsa_series_runs_free( RSA_series_runs *rs )
{
   int ii ; if( rs==NULL ) return ;
   for( ii=0 ; ii<rs->nrun ; ii++ ) free(rs->run_label[ii]) ;
   for( ii=0 ; ii<rs->nrow ; ii++ ) free(rs->fname[ii]) ;
   free(rs->run_label); free(rs->fname); free(rs->first_row); free(rs->row_sub);
   free(rs->row_run); free(rs->row_of); free(rs->run_nval); free(rs->offset); free(rs) ;
}

static int rsa_string_index( char **ss, int n, const char *s )
{
   int ii ; for( ii=0 ; ii<n ; ii++ ) if( strcmp(ss[ii],s)==0 ) return ii ;
   return -1 ;
}

static char ** rsa_split_csv_labels( const char *arg, int *nout )
{
   char *copy,*tok,**out=NULL ; int nn=0 ;
   if( nout!=NULL ) *nout=0 ;
   if( arg==NULL || *arg=='\0' ) return NULL ;
   if( arg[0]==',' || arg[strlen(arg)-1]==',' )
     ERROR_exit("3dRSA: empty label in -condition_order '%s'",arg) ;
   { const char *pp ; for( pp=arg ; *pp!='\0' ; pp++ )
       if( pp[0]==',' && pp[1]==',' )
         ERROR_exit("3dRSA: empty label in -condition_order '%s'",arg) ; }
   copy=strdup(arg) ;
   for( tok=strtok(copy,",") ; tok!=NULL ; tok=strtok(NULL,",") ){
     int ii ;
     if( *tok=='\0' ) ERROR_exit("3dRSA: empty label in -condition_order '%s'",arg) ;
     for( ii=0 ; ii<nn ; ii++ ) if( strcmp(out[ii],tok)==0 )
       ERROR_exit("3dRSA: duplicate condition '%s' in -condition_order",tok) ;
     out=(char **)realloc(out,sizeof(char *)*(nn+1)); out[nn++]=strdup(tok) ;
   }
   free(copy) ; if( nout!=NULL ) *nout=nn ; return out ;
}

/*! Validate a complete balanced subject x run long table.  Run labels are
    matched by value, not row position; their canonical order is the order in
    which they occur for the first subject. */
static int rsa_run_model_column( const char *name, char **spec, int nspec )
{
   int ii ; size_t nn=strlen(name) ;
   for( ii=0 ; ii<nspec ; ii++ ){
     char *co=strrchr(spec[ii],':') ;
     if( co!=NULL && (size_t)(co-spec[ii])==nn &&
         strncasecmp(spec[ii],name,nn)==0 ) return 1 ;
   }
   return 0 ;
}

static int rsa_named_column( const char *name, char **column, int ncolumn )
{
   int ii ; for( ii=0 ; ii<ncolumn ; ii++ )
     if( strcasecmp(name,column[ii])==0 ) return 1 ;
   return 0 ;
}

static RSA_series_runs * rsa_series_runs_build( THD_datatable *tab,
                                                const char *run_name,
                                                char **runmodspec, int nrunmodspec,
                                                char **runfactor, int nrunfactor )
{
   RSA_series_runs *rs ; char **subj=NULL,**runs=NULL ; int ns=0,nr=0,rr,ss,uu,jj,fr ;
   int rc=THD_datatable_column(tab,(char *)run_name) ;
   if( tab->icol_subj<0 )
     ERROR_exit("3dRSA: -run_column requires an explicit 'Subj' column") ;
   if( rc<0 ) ERROR_exit("3dRSA: -run_column '%s' is not in the data table",run_name) ;
   if( rc==tab->icol_subj || rc==tab->icol_input )
     ERROR_exit("3dRSA: -run_column must name a separate run-label column") ;
   for( rr=0 ; rr<tab->nrow ; rr++ ){
     if( rsa_string_index(subj,ns,tab->subj[rr])<0 ){
       subj=(char **)realloc(subj,sizeof(char *)*(ns+1)); subj[ns++]=tab->subj[rr] ;
     }
   }
   if( ns<3 ) ERROR_exit("3dRSA: run-wise IS-RSA needs at least 3 subjects; found %d",ns) ;
   for( rr=0 ; rr<tab->nrow ; rr++ ) if( strcmp(tab->subj[rr],subj[0])==0 ){
     char *r=DT_CELL(tab,rr,rc) ;
     if( rsa_string_index(runs,nr,r)>=0 )
       ERROR_exit("3dRSA: duplicate Subj/run row: Subj %s, %s %s",subj[0],run_name,r) ;
     runs=(char **)realloc(runs,sizeof(char *)*(nr+1)); runs[nr++]=r ;
   }
   if( nr<1 ) ERROR_exit("3dRSA: no runs found in -run_column '%s'",run_name) ;
   rs=(RSA_series_runs *)calloc(1,sizeof(*rs)) ;
   rs->nsub=ns; rs->nrun=nr; rs->nrow=tab->nrow; rs->run_col=rc ;
   rs->first_row=(int *)malloc(sizeof(int)*ns) ;
   rs->row_sub=(int *)malloc(sizeof(int)*tab->nrow) ;
   rs->row_run=(int *)malloc(sizeof(int)*tab->nrow) ;
   rs->row_of=(int *)malloc(sizeof(int)*ns*nr) ;
   rs->run_nval=(int *)calloc(nr,sizeof(int)); rs->offset=(int *)calloc(nr,sizeof(int));
   rs->run_label=(char **)calloc(nr,sizeof(char *));
   rs->fname=(char **)calloc(tab->nrow,sizeof(char *));
   for( uu=0 ; uu<nr ; uu++ ) rs->run_label[uu]=strdup(runs[uu]) ;
   for( ss=0 ; ss<ns ; ss++ ) rs->first_row[ss]=-1 ;
   for( rr=0 ; rr<ns*nr ; rr++ ) rs->row_of[rr]=-1 ;
   for( rr=0 ; rr<tab->nrow ; rr++ ){
     ss=rsa_string_index(subj,ns,tab->subj[rr]);
     uu=rsa_string_index(runs,nr,DT_CELL(tab,rr,rc)) ;
     if( uu<0 ) ERROR_exit("3dRSA: Subj %s has unexpected %s label '%s'; all subjects\n"
                           "       must have the same labeled runs as Subj %s",
                           tab->subj[rr],run_name,DT_CELL(tab,rr,rc),subj[0]) ;
     if( rs->row_of[ss*nr+uu]>=0 )
       ERROR_exit("3dRSA: duplicate Subj/run row: Subj %s, %s %s",
                  tab->subj[rr],run_name,DT_CELL(tab,rr,rc)) ;
     rs->row_sub[rr]=ss; rs->row_run[rr]=uu; rs->row_of[ss*nr+uu]=rr ;
     rs->fname[rr]=strdup(tab->fname[rr]); if( rs->first_row[ss]<0 ) rs->first_row[ss]=rr ;
   }
   for( ss=0 ; ss<ns ; ss++ ) for( uu=0 ; uu<nr ; uu++ )
     if( rs->row_of[ss*nr+uu]<0 )
       ERROR_exit("3dRSA: incomplete repeated-run table: Subj %s is missing %s %s",
                  subj[ss],run_name,runs[uu]) ;
   /* A subject contributes one behavioral row.  Repeated copies must really
      be copies; silently choosing one would make row order affect inference. */
   for( ss=0 ; ss<ns ; ss++ ){
     fr=rs->first_row[ss] ;
     for( uu=0 ; uu<nr ; uu++ ){
       rr=rs->row_of[ss*nr+uu] ;
       for( jj=0 ; jj<tab->ncol ; jj++ )
         if( jj!=tab->icol_input && jj!=rc && jj!=tab->icol_subj &&
             !rsa_run_model_column(tab->cname[jj],runmodspec,nrunmodspec) &&
             !rsa_named_column(tab->cname[jj],runfactor,nrunfactor) &&
             strcmp(DT_CELL(tab,rr,jj),DT_CELL(tab,fr,jj))!=0 )
           ERROR_exit("3dRSA: column '%s' changes within Subj %s (%s %s: '%s' vs '%s').\n"
                      "       With -run_column, only -run_model or -run_factor columns may vary by run.",
                      tab->cname[jj],subj[ss],run_name,runs[uu],DT_CELL(tab,fr,jj),DT_CELL(tab,rr,jj)) ;
     }
   }
   free(subj); free(runs); return rs ;
}

static RSA_runfactor rsa_runfactor_build( THD_datatable *tab, RSA_series_runs *rs,
                                          const char *column )
{
   RSA_runfactor rf ; int ru,ss,row,lv ; char *v ; memset(&rf,0,sizeof(rf)) ;
   rf.column=strdup(column); rf.icol=THD_datatable_column(tab,(char *)column) ;
   if( rf.icol<0 ) ERROR_exit("3dRSA: -run_factor column '%s' is not in the data table",column) ;
   if( rf.icol==tab->icol_subj || rf.icol==tab->icol_input || rf.icol==rs->run_col )
     ERROR_exit("3dRSA: -run_factor '%s' must name run metadata, not Subj/InputFile/the run label",column) ;
   rf.run_level=(int *)malloc(sizeof(int)*rs->nrun) ;
   for( ru=0 ; ru<rs->nrun ; ru++ ){
     row=rs->row_of[ru] ; v=DT_CELL(tab,row,rf.icol) ;
     for( ss=1 ; ss<rs->nsub ; ss++ ){
       row=rs->row_of[ss*rs->nrun+ru] ;
       if( strcmp(DT_CELL(tab,row,rf.icol),v)!=0 )
         ERROR_exit("3dRSA: -run_factor %s is not a run-level design: run %s is '%s' for Subj %s but '%s' for Subj %s",
                    column,rs->run_label[ru],v,tab->subj[rs->row_of[ru]],
                    DT_CELL(tab,row,rf.icol),tab->subj[row]) ;
     }
     lv=rsa_string_index(rf.level,rf.nlevel,v) ;
     if( lv<0 ){
       rf.level=(char **)realloc(rf.level,sizeof(char *)*(rf.nlevel+1)) ;
       rf.level[rf.nlevel]=strdup(v); lv=rf.nlevel++ ;
     }
     rf.run_level[ru]=lv ;
   }
   if( rf.nlevel<2 ) ERROR_exit("3dRSA: -run_factor %s has only one level ('%s')",column,rf.level[0]) ;
   return rf ;
}

static RSA_runcontrast rsa_runcontrast_build( const char *spec, RSA_runfactor *rf,
                                              int nrf, RSA_series_runs *rs )
{
   RSA_runcontrast rc ; char *w=strdup(spec),*eq=strchr(w,'='),*co,*mi ;
   char *fac,*pos,*neg ; int ff,lv,ru,np=0,nn=0 ; memset(&rc,0,sizeof(rc)) ;
   if( eq==NULL || eq==w || eq[1]=='\0' )
     ERROR_exit("3dRSA: -run_contrast '%s' must be NAME=FACTOR:POSITIVE-NEGATIVE",spec) ;
   *eq='\0'; co=strchr(eq+1,':') ;
   if( co==NULL || co==eq+1 || co[1]=='\0' )
     ERROR_exit("3dRSA: -run_contrast '%s' needs FACTOR:POSITIVE-NEGATIVE",spec) ;
   *co='\0'; mi=strchr(co+1,'-') ;
   if( mi==NULL || mi==co+1 || mi[1]=='\0' )
     ERROR_exit("3dRSA: -run_contrast '%s' needs two levels separated by '-'",spec) ;
   *mi='\0'; fac=eq+1; pos=co+1; neg=mi+1 ;
   snprintf(rc.name,sizeof(rc.name),"%s",w) ;
   for( ff=0 ; ff<nrf ; ff++ ) if( strcasecmp(rf[ff].column,fac)==0 ) break ;
   if( ff==nrf ) ERROR_exit("3dRSA: -run_contrast '%s' refers to undeclared -run_factor '%s'",spec,fac) ;
   rc.ifactor=ff; rc.ipos=rc.ineg=-1 ;
   for( lv=0 ; lv<rf[ff].nlevel ; lv++ ){
     if( strcasecmp(rf[ff].level[lv],pos)==0 ) rc.ipos=lv ;
     if( strcasecmp(rf[ff].level[lv],neg)==0 ) rc.ineg=lv ;
   }
   if( rc.ipos<0 || rc.ineg<0 || rc.ipos==rc.ineg )
     ERROR_exit("3dRSA: -run_contrast '%s': levels '%s' and '%s' must be distinct levels of %s",
                rc.name,pos,neg,rf[ff].column) ;
   rc.weight=(float *)calloc(rs->nrun,sizeof(float)) ;
   for( ru=0 ; ru<rs->nrun ; ru++ ){
     if( rf[ff].run_level[ru]==rc.ipos ) np++ ;
     if( rf[ff].run_level[ru]==rc.ineg ) nn++ ;
   }
   for( ru=0 ; ru<rs->nrun ; ru++ ){
     if( rf[ff].run_level[ru]==rc.ipos ) rc.weight[ru]=1.0f/np ;
     if( rf[ff].run_level[ru]==rc.ineg ) rc.weight[ru]=-1.0f/nn ;
   }
   free(w); return rc ;
}

/*! Normalize one ROI time course within one run.  Constant finite runs map to
    zero, the same well-defined convention used by common standardizers. */
static void rsa_run_normalize( float *x, int n, int how )
{
   int ii ; double mu=0.0,ss=0.0,sd ;
   if( how==RUN_NORM_NONE || x==NULL || n<1 ) return ;
   for( ii=0 ; ii<n ; ii++ ) mu+=x[ii] ; mu/=n ;
   if( how==RUN_NORM_DEMEAN ){ for( ii=0 ; ii<n ; ii++ ) x[ii]-=(float)mu; return; }
   for( ii=0 ; ii<n ; ii++ ){ double d=x[ii]-mu; ss+=d*d; }
   sd=sqrt(ss/n) ;
   if( !(sd>0.0) || !isfinite(sd) ){ memset(x,0,sizeof(float)*n); return; }
   for( ii=0 ; ii<n ; ii++ ) x[ii]=(float)((x[ii]-mu)/sd) ;
}

static THD_simmat * rsa_simmat_from_labels( THD_datatable *tab, int icol, int n )
{
   THD_simmat *sm=THD_simmat_new(n) ; int ii,jj,ndiff=0 ;
   if( sm==NULL ) return NULL ; sm->is_dist=0 ;
   for( ii=0 ; ii<n ; ii++ ){
     sm->mat[ii*n+ii]=1.0f ;
     for( jj=ii+1 ; jj<n ; jj++ ){
       int same=(strcmp(DT_CELL(tab,ii,icol),DT_CELL(tab,jj,icol))==0) ;
       sm->mat[ii*n+jj]=sm->mat[jj*n+ii]=same?1.0f:0.0f ; if(!same)ndiff=1 ;
     }
   }
   if( !ndiff ){ THD_simmat_free(sm); return NULL; }
   return sm ;
}

/*! One fixed or run-specific subject model against run-specific neural
    geometries. The same subject relabeling is used for every run, so a varying
    behavioral model is permuted as a whole trajectory. Individual run nulls
    and the equal-run mean null remain signed until their two-sided tail and
    max-family statistics have been formed. */
static void rsa_runresolved_mantel(
   THD_simmat **neural, int nrun, THD_simmat *model, THD_simmat **run_model, int cmp,
   PERM_set *pset, THD_rdm_ws *ws, float *ntri,
   float *rstat, float *rpval, float *rzscr,
   float *meanstat, float *meanp, float *meanz,
   float *rnull, float *mnull, int *nge,
   int ncon, float *cweight, float *cstat, float *cpval, float *czscr,
   float *cnull, int *cnge, float *csum )
{
   int ru,pk,cc,np=(pset!=NULL)?pset->nperm:0,m=ws->m ;
   float *mt=ws->yfit ; double sm=0.0 ;
   for( ru=0 ; ru<nrun ; ru++ ){
     THD_simmat_to_tri(neural[ru],ntri+(size_t)ru*m) ;
   }
   for( ru=0 ; ru<nrun ; ru++ ){
     THD_simmat_to_tri(run_model!=NULL?run_model[ru]:model,mt) ;
     rstat[ru]=THD_tri_corr(m,ntri+(size_t)ru*m,mt,cmp,ws->sc1,ws->sc2) ;
     sm+=rstat[ru] ; rpval[ru]=-1.0f ;
     rzscr[ru]=atanhf(fmaxf(-0.999329f,fminf(0.999329f,rstat[ru]))) ;
   }
   *meanstat=(float)(sm/nrun) ; *meanp=-1.0f ;
   *meanz=atanhf(fmaxf(-0.999329f,fminf(0.999329f,*meanstat))) ;
   for( cc=0 ; cc<ncon ; cc++ ){
     double cv=0.0 ;
     for( ru=0 ; ru<nrun ; ru++ ) cv+=cweight[(size_t)cc*nrun+ru]*rstat[ru] ;
     cstat[cc]=(float)cv; cpval[cc]=-1.0f; czscr[cc]=cstat[cc] ;
   }
   if( np<=0 ) return ;
   { int mge=0 ; memset(nge,0,sizeof(int)*nrun);
     if( ncon>0 ) memset(cnge,0,sizeof(int)*ncon) ;
     for( pk=0 ; pk<np ; pk++ ){
       THD_simmat *pm=(run_model!=NULL)?run_model[0]:model ;
       int *perm=pset->perm+(size_t)pk*pm->n ; double av=0.0 ;
       if( ncon>0 ) memset(csum,0,sizeof(float)*ncon) ;
       for( ru=0 ; ru<nrun ; ru++ ){
         THD_simmat_to_tri_perm(run_model!=NULL?run_model[ru]:model,perm,ws->yperm) ;
         float v=THD_tri_corr(m,ntri+(size_t)ru*m,ws->yperm,cmp,ws->sc1,ws->sc2) ;
         av+=v ;
         for( cc=0 ; cc<ncon ; cc++ ) csum[cc]+=cweight[(size_t)cc*nrun+ru]*v ;
         if( fabsf(v)>=fabsf(rstat[ru]) ) nge[ru]++ ;
         if( rnull!=NULL ) rnull[(size_t)ru*np+pk]=fabsf(v) ;
       }
       { float v=(float)(av/nrun) ;
         if( fabsf(v)>=fabsf(*meanstat) ) mge++ ;
         if( mnull!=NULL ) mnull[pk]=fabsf(v) ; }
       for( cc=0 ; cc<ncon ; cc++ ){
         if( fabsf(csum[cc])>=fabsf(cstat[cc]) ) cnge[cc]++ ;
         if( cnull!=NULL ) cnull[(size_t)cc*np+pk]=fabsf(csum[cc]) ;
       }
     }
     for( ru=0 ; ru<nrun ; ru++ ){
       rpval[ru]=(float)nge[ru]/np ;
       rzscr[ru]=THD_perm_signed_z(rpval[ru],rstat[ru],PERM_TAIL_TWO) ;
     }
     *meanp=(float)mge/np ; *meanz=THD_perm_signed_z(*meanp,*meanstat,PERM_TAIL_TWO) ;
     for( cc=0 ; cc<ncon ; cc++ ){
       cpval[cc]=(float)cnge[cc]/np ;
       czscr[cc]=THD_perm_signed_z(cpval[cc],cstat[cc],PERM_TAIL_TWO) ;
     }
   }
}

typedef struct RSA_model {
   int kind ; char *spec ; int rule,icol,ncol,*icols ;
   THD_simmat *mat,**run_mat ; THD_3dim_dataset **dset ;
   float **cmean ; int mvals ; char name[128] ;
} RSA_model ;

/*! Joint, run-resolved standardized dyadic regression.  Every run is fit with
    the fixed and run-varying model RDMs together.  THD_rdm_regress_signed uses
    a model-specific Freedman--Lane reduced model; using the same pset in every
    run preserves each subject's whole behavioral trajectory.  Signed null
    coefficients can therefore be combined into valid equal-run means and
    planned run contrasts before two-sided testing. */
static int rsa_runresolved_regress(
   THD_simmat **neural, int nrun, RSA_model *mod, int nmod, int cmp,
   PERM_set *pset, THD_rdm_ws *ws,
   int ncon, float *cweight,
   float *rbeta, float *rpr, float *rpval, float *rzscr,
   float *mbeta, float *mpr, float *mpval, float *mzscr,
   float *cbeta, float *cpr, float *cpval, float *czscr,
   float *rnull, float *mnull, float *cnull )
{
   int ru,mm,cc,pk,np=(pset!=NULL)?pset->nperm:0,err=0 ;
   THD_simmat **mv=(THD_simmat **)malloc(sizeof(*mv)*nmod) ;
   float *bb=(float *)calloc(nmod,sizeof(float)) ;
   float *pr=(float *)calloc(nmod,sizeof(float)) ;
   float *pv=(float *)calloc(nmod,sizeof(float)) ;
   float *sn=(np>0)?(float *)calloc((size_t)nmod*np,sizeof(float)):NULL ;

   memset(mbeta,0,sizeof(float)*nmod); memset(mpr,0,sizeof(float)*nmod) ;
   if( np>0 && mnull!=NULL ) memset(mnull,0,sizeof(float)*(size_t)nmod*np) ;
   if( ncon>0 ){
     memset(cbeta,0,sizeof(float)*(size_t)nmod*ncon) ;
     memset(cpr,0,sizeof(float)*(size_t)nmod*ncon) ;
     if( np>0 && cnull!=NULL )
       memset(cnull,0,sizeof(float)*(size_t)nmod*ncon*np) ;
   }

   for( ru=0 ; ru<nrun ; ru++ ){
     for( mm=0 ; mm<nmod ; mm++ )
       mv[mm]=(mod[mm].run_mat!=NULL)?mod[mm].run_mat[ru]:mod[mm].mat ;
     err=THD_rdm_regress_signed(neural[ru],nmod,mv,0,NULL,cmp,pset,ws,
                                bb,pr,(np>0)?pv:NULL,sn) ;
     if( err ) break ;
     for( mm=0 ; mm<nmod ; mm++ ){
       size_t ix=(size_t)mm*nrun+ru ;
       rbeta[ix]=bb[mm]; rpr[ix]=pr[mm]; rpval[ix]=(np>0)?pv[mm]:-1.0f ;
       rzscr[ix]=(np>0)?THD_perm_signed_z(rpval[ix],rbeta[ix],PERM_TAIL_TWO)
                       :rbeta[ix] ;
       mbeta[mm]+=bb[mm]/nrun ; mpr[mm]+=pr[mm]/nrun ;
       for( cc=0 ; cc<ncon ; cc++ ){
         size_t ci=(size_t)mm*ncon+cc ; float w=cweight[(size_t)cc*nrun+ru] ;
         cbeta[ci]+=w*bb[mm]; cpr[ci]+=w*pr[mm] ;
       }
       for( pk=0 ; pk<np ; pk++ ){
         float v=sn[(size_t)mm*np+pk] ;
         if( rnull!=NULL ){
           float av=fabsf(v),ao=fabsf(bb[mm]) ;
           if( fabsf(av-ao)<=64.0f*FLT_EPSILON*(1.0f+ao) ) av=ao ;
           rnull[((size_t)mm*nrun+ru)*np+pk]=av ;
         }
         if( mnull!=NULL ) mnull[(size_t)mm*np+pk]+=v/nrun ;
         if( cnull!=NULL ) for( cc=0 ; cc<ncon ; cc++ )
           cnull[((size_t)mm*ncon+cc)*np+pk]
             +=cweight[(size_t)cc*nrun+ru]*v ;
       }
     }
   }

   if( !err ) for( mm=0 ; mm<nmod ; mm++ ){
     int nge=0 ; float ao=fabsf(mbeta[mm]),tol=64.0f*FLT_EPSILON*(1.0f+ao) ;
     if( np>0 ) for( pk=0 ; pk<np ; pk++ ){
       float v=mnull[(size_t)mm*np+pk] ;
       float av=fabsf(v); if(fabsf(av-ao)<=tol)av=ao ;
       if( av>=ao ) nge++ ; mnull[(size_t)mm*np+pk]=av ;
     }
     mpval[mm]=(np>0)?(float)nge/np:-1.0f ;
     mzscr[mm]=(np>0)?THD_perm_signed_z(mpval[mm],mbeta[mm],PERM_TAIL_TWO)
                     :mbeta[mm] ;
     for( cc=0 ; cc<ncon ; cc++ ){
       size_t ci=(size_t)mm*ncon+cc ; nge=0 ;
       ao=fabsf(cbeta[ci]); tol=64.0f*FLT_EPSILON*(1.0f+ao) ;
       if( np>0 ) for( pk=0 ; pk<np ; pk++ ){
         float v=cnull[ci*np+pk] ;
         float av=fabsf(v); if(fabsf(av-ao)<=tol)av=ao ;
         if( av>=ao ) nge++ ; cnull[ci*np+pk]=av ;
       }
       cpval[ci]=(np>0)?(float)nge/np:-1.0f ;
       czscr[ci]=(np>0)?THD_perm_signed_z(cpval[ci],cbeta[ci],PERM_TAIL_TWO)
                       :cbeta[ci] ;
     }
   }
   free(mv); free(bb); free(pr); free(pv); if(sn!=NULL)free(sn) ;
   return err ;
}

typedef struct {
   int enabled, mode, is_tty, stage, total, done, last_bucket ;
   double start, last_report ;
   const char *label, *unit ;
} RSA_progress ;

static void rsa_duration( double sec , char *buf , size_t nbuf )
{
   long ss=(long)((sec>0.0)?sec+0.5:0.0), hh=ss/3600, mm=(ss%3600)/60 ;
   ss %= 60 ;
   if( hh > 0 ) snprintf(buf,nbuf,"%ldh%02ldm%02lds",hh,mm,ss) ;
   else if( mm > 0 ) snprintf(buf,nbuf,"%ldm%02lds",mm,ss) ;
   else snprintf(buf,nbuf,"%lds",ss) ;
}

static void rsa_progress_init( RSA_progress *p, int mode, int quiet, int stage,
                               const char *label, int total, const char *unit )
{
   memset(p,0,sizeof(*p)) ;
   p->enabled = !quiet && mode != RSA_PROGRESS_OFF && total > 0 ;
   p->is_tty = isatty(fileno(stderr)) ;
   p->mode = (mode==RSA_PROGRESS_AUTO)
           ? (p->is_tty ? RSA_PROGRESS_BAR : RSA_PROGRESS_LINE)
           : mode ;
   p->stage=stage ; p->label=label ; p->total=total ; p->unit=unit ;
   p->start=p->last_report=0.001*(double)NI_clock_time() ;
   if( !p->enabled ) return ;
   if( p->mode == RSA_PROGRESS_BAR ){
     fprintf(stderr,"++ 3dRSA [%d/5] %s [--------------------] 0/%d (0.0%%)",
             stage,label,total) ;
     fflush(stderr) ;
   } else {
     INFO_message("3dRSA [%d/5] %s: 0/%d %s complete",stage,label,total,unit) ;
   }
}

static void rsa_progress_advance( RSA_progress *p )
{
   int done,bucket ; double now,elapsed,rate,eta ; char ebuf[32],tbuf[32] ;
   if( !p->enabled ) return ;
#ifdef USE_OMP
#pragma omp atomic capture
   done = ++p->done ;
#else
   done = ++p->done ;
#endif
   if( done > p->total ) done=p->total ;

   /* A location is the existing OpenMP scheduling unit.  Rendering once after
      a location avoids atomics inside permutation/bootstrap kernels. */
#ifdef USE_OMP
#pragma omp critical(rsa_progress_render)
#endif
   {
     now=0.001*(double)NI_clock_time() ; bucket=(10*done)/p->total ;
     if( done==p->total ||
         (p->mode==RSA_PROGRESS_BAR && now-p->last_report>=1.0) ||
         (p->mode==RSA_PROGRESS_LINE && bucket>p->last_bucket) ){
       elapsed=now-p->start ; rate=(elapsed>0.0)?done/elapsed:0.0 ;
       eta=(rate>0.0)?(p->total-done)/rate:0.0 ;
       rsa_duration(elapsed,ebuf,sizeof(ebuf)) ;
       rsa_duration(eta,tbuf,sizeof(tbuf)) ;
       if( p->mode==RSA_PROGRESS_BAR ){
         int ii,fill=(20*done)/p->total ;
         fprintf(stderr,"\r++ 3dRSA [%d/5] %s [",p->stage,p->label) ;
         for( ii=0 ; ii<20 ; ii++ ) fputc((ii<fill)?'#':'-',stderr) ;
         fprintf(stderr,"] %d/%d (%.1f%%) | %.2f %s/s | elapsed %s",
                 done,p->total,100.0*done/p->total,rate,p->unit,ebuf) ;
         if( done<p->total ) fprintf(stderr," | ETA ~%s",tbuf) ;
         if( p->is_tty ) fputs("\033[K",stderr) ; /* erase an older, longer ETA */
         if( done==p->total ) fprintf(stderr,"\n") ;
         fflush(stderr) ;
       } else {
         INFO_message("3dRSA [%d/5] %s: %d/%d %s complete (%.1f%%); "
                      "%.2f %s/s; elapsed %s%s%s",
                      p->stage,p->label,done,p->total,p->unit,
                      100.0*done/p->total,rate,p->unit,ebuf,
                      (done<p->total)?"; ETA ~":"",(done<p->total)?tbuf:"") ;
       }
       p->last_report=now ; p->last_bucket=bucket ;
     }
   }
}

/*--- where a model matrix comes from ---*/
#define MODK_COLUMN 1   /* built from a dataTable column by a rule */
#define MODK_MATRIX 2   /* read from a 1D file */
#define MODK_DSET   3   /* a neural matrix from a second set of datasets */
#define MODK_SEED   4   /* fixed seed ROI from the main input datasets */
#define MODK_RUNCOLUMN 5 /* run-varying dataTable column, one matrix per run */

#undef  MYatanh
#define MYatanh(x) ( ((x)<-0.999329f) ? -4.0f                \
                    :((x)>+0.999329f) ? +4.0f : atanhf(x) )

/*----------------------------------------------------------------------------*/

static int rsa_model_has_loo( RSA_model *m )
{
   return m != NULL && m->kind == MODK_COLUMN && m->ncol > 0 && m->icols != NULL ;
}

/*! Two labeled models may share a LOO computation only when both their target
    columns and their prediction estimand match.  AnnaK typicality is distinct
    from neural-neighbor prediction; scalar NN/euclid/absdiff share the latter.
    Profile euclid/mahal also share one multi-output prediction estimand. */
static int rsa_model_same_loo( RSA_model *a , RSA_model *b )
{
   int cc,amode,bmode ;
   if( !rsa_model_has_loo(a) || !rsa_model_has_loo(b) || a->ncol != b->ncol ) return 0 ;
   amode=(a->ncol==1 && a->rule==RUL_ANNAK) ? 1 : (a->ncol==1 ? 2 : 3) ;
   bmode=(b->ncol==1 && b->rule==RUL_ANNAK) ? 1 : (b->ncol==1 ? 2 : 3) ;
   if( amode != bmode ) return 0 ;
   for( cc=0 ; cc<a->ncol ; cc++ ) if( a->icols[cc] != b->icols[cc] ) return 0 ;
   return 1 ;
}

/*! A requested contrast between two named models, "A minus B". */
typedef struct {
   int  ia , ib ;         /* indices into mod[] */
   char name[160] ;       /* display/label name, e.g. "visual-semantic" */
} RSA_contrast ;

#define RSA_NCOMMON THD_NCOMMON
#define RSA_NCOMMON3 THD_NCOMMON3
#define RSA_MAXCOMMON THD_NCOMMON3

/*! One pairwise or three-predictor commonality request.  qbase maps its local
    quantities into the flattened result arrays shared by output/inference. */
typedef struct {
   int nmodel , imod[3] , nq , qbase ;
   char lab[RSA_MAXCOMMON][200] ;
} RSA_common ;

/*! F7 constrained fitted model: a named nonnegative ridge mixture of existing
    model RDMs.  comp[] indexes mod[].  Fitting is always nested over subjects;
    no dyad belonging to the held subject enters its fitted weights. */
typedef struct {
   int ncomp , *comp , wbase ;
   float ridge ;
   char name[128] ;
} RSA_fitmodel ;

/*! F14 paired comparison of two F7 fitted models, "A minus B".  The statistic
    is the mean held-fold Fisher-z accuracy difference; indices address fit[]. */
typedef struct {
   int  ia , ib ;
   char name[160] ;
} RSA_fitcontrast ;

/*! F22 explicit condition-fold descriptor.  fold[c] is the held-out fold for
    global/model condition c; labels retain first-seen order for provenance. */
typedef struct {
   int ncond , nfold , *fold , *nmember ;
   char **label ;
} RSA_condfold ;

typedef struct {
   int maxtrain , maxtest , ncomp ;
   float *y , *pred , *xflat , **x , *xmean , *xsd , *w , *foldw ;
   float *ty , *tpred ;
} RSA_fitws ;

static void rsa_condfold_free( RSA_condfold *cf )
{
   int ff ;
   if( cf==NULL ) return ;
   if( cf->label!=NULL ){
     for( ff=0 ; ff<cf->nfold ; ff++ ) free(cf->label[ff]) ;
     free(cf->label) ;
   }
   free(cf->fold) ; free(cf->nmember) ; free(cf) ;
}

/*! Read one whitespace-free fold label per condition.  Blank lines and
    comments are ignored.  Strict unseen-stimulus scoring needs at least three
    held conditions (three test dyads) and three remaining training conditions
    in every fold. */
static RSA_condfold * rsa_condfold_read( char *fname, int ncond )
{
   RSA_condfold *cf ; FILE *fp ; char line[4096],tok[512],extra[2] ; int nr=0,ff ;
   if( fname==NULL || ncond<1 ) return NULL ;
   fp=fopen(fname,"r") ;
   if( fp==NULL ){
     ERROR_message("3dRSA: cannot open -fit_condfold file '%s'",fname) ;
     return NULL ;
   }
   cf=(RSA_condfold *)calloc(1,sizeof(RSA_condfold)) ;
   if( cf==NULL ){ fclose(fp) ; return NULL ; }
   cf->ncond=ncond ; cf->fold=(int *)malloc(sizeof(int)*ncond) ;
   if( cf->fold==NULL ){ fclose(fp) ; rsa_condfold_free(cf) ; return NULL ; }
   while( fgets(line,sizeof(line),fp)!=NULL ){
     char *p=line,*hash ;
     while( isspace((unsigned char)*p) ) p++ ;
     if( *p=='\0' || *p=='#' ) continue ;
     hash=strchr(p,'#') ; if( hash!=NULL ) *hash='\0' ;
     if( sscanf(p,"%511s %1s",tok,extra)!=1 ){
       ERROR_message("3dRSA: -fit_condfold '%s' needs one label per line",fname) ;
       goto bad ;
     }
     if( nr>=ncond ){
       ERROR_message("3dRSA: -fit_condfold '%s' has more than %d condition labels",
                     fname,ncond) ;
       goto bad ;
     }
     for( ff=0 ; ff<cf->nfold ; ff++ ) if( strcmp(tok,cf->label[ff])==0 ) break ;
     if( ff==cf->nfold ){
       char **tmp=(char **)realloc(cf->label,sizeof(char *)*(cf->nfold+1)) ;
       if( tmp==NULL ) goto bad ;
       cf->label=tmp ; cf->label[cf->nfold]=strdup(tok) ;
       if( cf->label[cf->nfold]==NULL ) goto bad ;
       cf->nfold++ ;
     }
     cf->fold[nr++]=ff ;
   }
   fclose(fp) ; fp=NULL ;
   if( nr!=ncond ){
     ERROR_message("3dRSA: -fit_condfold '%s' has %d labels; need %d (one per condition)",
                   fname,nr,ncond) ;
     goto bad ;
   }
   if( cf->nfold<2 ){
     ERROR_message("3dRSA: -fit_condfold '%s' defines only %d fold; need at least 2",
                   fname,cf->nfold) ;
     goto bad ;
   }
   cf->nmember=(int *)calloc(cf->nfold,sizeof(int)) ;
   if( cf->nmember==NULL ) goto bad ;
   for( nr=0 ; nr<ncond ; nr++ ) cf->nmember[cf->fold[nr]]++ ;
   for( ff=0 ; ff<cf->nfold ; ff++ ){
     if( cf->nmember[ff]<3 || ncond-cf->nmember[ff]<3 ){
       ERROR_message("3dRSA: -fit_condfold '%s': fold '%s' holds %d and leaves %d conditions;\n"
                     "       strict held-condition fitting needs at least 3 on each side",
                     fname,cf->label[ff],cf->nmember[ff],ncond-cf->nmember[ff]) ;
       goto bad ;
     }
   }
   return cf ;
bad:
   if( fp!=NULL ) fclose(fp) ;
   rsa_condfold_free(cf) ; return NULL ;
}

static RSA_fitws * rsa_fitws_new( int rdm_over, int nsub, int nitem, int ncomp )
{
   RSA_fitws *fw=(RSA_fitws *)calloc(1,sizeof(RSA_fitws)) ; int cc ;
   int ntri=THD_NTRI(nitem) ;
   fw->maxtrain = (rdm_over==RDM_SUBJ) ? THD_NTRI(nsub-1)
                                       : (nsub-1)*ntri ;
   fw->maxtest  = (rdm_over==RDM_SUBJ) ? nsub-1 : ntri ;
   fw->ncomp=ncomp ;
   fw->y=(float *)malloc(sizeof(float)*fw->maxtrain) ;
   fw->pred=(float *)malloc(sizeof(float)*fw->maxtrain) ;
   fw->xflat=(float *)malloc(sizeof(float)*(size_t)ncomp*fw->maxtrain) ;
   fw->x=(float **)malloc(sizeof(float *)*ncomp) ;
   fw->xmean=(float *)malloc(sizeof(float)*ncomp) ;
   fw->xsd=(float *)malloc(sizeof(float)*ncomp) ;
   fw->w=(float *)malloc(sizeof(float)*ncomp) ;
   fw->foldw=(float *)malloc(sizeof(float)*ncomp) ;
   fw->ty=(float *)malloc(sizeof(float)*fw->maxtest) ;
   fw->tpred=(float *)malloc(sizeof(float)*fw->maxtest) ;
   for( cc=0 ; cc<ncomp ; cc++ ) fw->x[cc]=fw->xflat+(size_t)cc*fw->maxtrain ;
   return fw ;
}

static void rsa_fitws_free( RSA_fitws *fw )
{
   if( fw==NULL ) return ;
   free(fw->y) ; free(fw->pred) ; free(fw->xflat) ; free(fw->x) ;
   free(fw->xmean) ; free(fw->xsd) ; free(fw->w) ; free(fw->foldw) ;
   free(fw->ty) ; free(fw->tpred) ; free(fw) ;
}

static float rsa_fit_pearson( int n, float *a, float *b )
{
   int ii ; double am=0.0,bm=0.0,aa=0.0,bb=0.0,ab=0.0 ;
   if( n<3 ) return 0.0f ;
   for( ii=0 ; ii<n ; ii++ ){ am+=a[ii] ; bm+=b[ii] ; }
   am/=n ; bm/=n ;
   for( ii=0 ; ii<n ; ii++ ){
     double da=a[ii]-am,db=b[ii]-bm ; aa+=da*da ; bb+=db*db ; ab+=da*db ;
   }
   if( aa<=0.0 || bb<=0.0 ) return 0.0f ;
   return (float)(ab/sqrt(aa*bb)) ;
}

/*! Fit standardized nonnegative ridge regression by cyclic coordinate descent.
    lambda is scaled by the number of training dyads, so its meaning is stable
    across ROI type and sample size.  Returns 0 for a degenerate fold. */
static int rsa_fit_nnls_ridge( int m, int p, float ridge, RSA_fitws *fw )
{
   int ii,cc,it ; double ym=0.0,ys=0.0 ;
   for( ii=0 ; ii<m ; ii++ ) ym+=fw->y[ii] ;
   ym/=m ;
   for( ii=0 ; ii<m ; ii++ ){ double d=fw->y[ii]-ym ; ys+=d*d ; }
   ys=sqrt(ys/m) ; if( !(ys>1.0e-12) ) return 0 ;
   for( ii=0 ; ii<m ; ii++ ) fw->y[ii]=(float)((fw->y[ii]-ym)/ys) ;
   memset(fw->pred,0,sizeof(float)*m) ; memset(fw->w,0,sizeof(float)*p) ;
   for( cc=0 ; cc<p ; cc++ ){
     double xm=0.0,xs=0.0 ;
     for( ii=0 ; ii<m ; ii++ ) xm+=fw->x[cc][ii] ;
     xm/=m ;
     for( ii=0 ; ii<m ; ii++ ){ double d=fw->x[cc][ii]-xm ; xs+=d*d ; }
     xs=sqrt(xs/m) ; if( !(xs>1.0e-12) ) return 0 ;
     fw->xmean[cc]=(float)xm ; fw->xsd[cc]=(float)xs ;
     for( ii=0 ; ii<m ; ii++ ) fw->x[cc][ii]=(fw->x[cc][ii]-fw->xmean[cc])/fw->xsd[cc] ;
   }
   for( it=0 ; it<1000 ; it++ ){
     float maxchg=0.0f ;
     for( cc=0 ; cc<p ; cc++ ){
       double rho=0.0,den=(double)ridge*m ; float old=fw->w[cc],nw ;
       for( ii=0 ; ii<m ; ii++ ){
         double x=fw->x[cc][ii] ;
         rho += x*(fw->y[ii]-fw->pred[ii]+x*old) ; den += x*x ;
       }
       nw=(rho>0.0 && den>0.0) ? (float)(rho/den) : 0.0f ;
       if( fabsf(nw-old)>maxchg ) maxchg=fabsf(nw-old) ;
       if( nw!=old ) for( ii=0 ; ii<m ; ii++ ) fw->pred[ii]+=fw->x[cc][ii]*(nw-old) ;
       fw->w[cc]=nw ;
     }
     if( maxchg<1.0e-6f ) break ;
   }
   return 1 ;
}

/*! Subject-generalizing CV accuracy for one fitted component model.  perm maps
    destination model labels to source labels.  In IS-RSA, training uses only
    dyads wholly outside the held subject; in classic RSA it uses all condition
    dyads from the other subjects.  The returned effect is tanh(mean fold z). */
static float rsa_fit_subject_cv( int rdm_over, int nsub, int nitem,
                                float *srdm, THD_simmat *neural,
                                RSA_fitmodel *fm, THD_simmat **mv, int *perm,
                                RSA_fitws *fw, float *meanw,
                                float *foldz, unsigned char *valid )
{
   int hold,a,b,s,cc,m,mt,nok=0 ; double zsum=0.0 ;
   if( meanw!=NULL ) memset(meanw,0,sizeof(float)*fm->ncomp) ;
   if( valid!=NULL ) memset(valid,0,(size_t)nsub) ;
   for( hold=0 ; hold<nsub ; hold++ ){
     m=mt=0 ;
     if( rdm_over==RDM_SUBJ ){
       for( a=0 ; a<nsub ; a++ ) if( a!=hold )
         for( b=a+1 ; b<nsub ; b++ ) if( b!=hold ){
           fw->y[m]=neural->mat[a*nsub+b] ;
           for( cc=0 ; cc<fm->ncomp ; cc++ ){
             int ia=perm?perm[a]:a, ib=perm?perm[b]:b ;
             fw->x[cc][m]=mv[fm->comp[cc]]->mat[ia*nsub+ib] ;
           }
           m++ ;
         }
     } else {
       for( s=0 ; s<nsub ; s++ ) if( s!=hold )
         for( a=0 ; a<nitem ; a++ ) for( b=a+1 ; b<nitem ; b++ ){
           fw->y[m]=srdm[(size_t)s*THD_NTRI(nitem)+mt] ;
           for( cc=0 ; cc<fm->ncomp ; cc++ ){
             int ia=perm?perm[a]:a, ib=perm?perm[b]:b ;
             fw->x[cc][m]=mv[fm->comp[cc]]->mat[ia*nitem+ib] ;
           }
           m++ ; mt++ ;
           if( mt==THD_NTRI(nitem) ) mt=0 ;
         }
     }
     if( !rsa_fit_nnls_ridge(m,fm->ncomp,fm->ridge,fw) ) continue ;
     mt=0 ;
     if( rdm_over==RDM_SUBJ ){
       for( a=0 ; a<nsub ; a++ ) if( a!=hold ){
         fw->ty[mt]=neural->mat[hold*nsub+a] ; fw->tpred[mt]=0.0f ;
         for( cc=0 ; cc<fm->ncomp ; cc++ ){
           int ih=perm?perm[hold]:hold, ia=perm?perm[a]:a ;
           float x=mv[fm->comp[cc]]->mat[ih*nsub+ia] ;
           fw->tpred[mt]+=fw->w[cc]*(x-fw->xmean[cc])/fw->xsd[cc] ;
         }
         mt++ ;
       }
     } else {
       for( a=0 ; a<nitem ; a++ ) for( b=a+1 ; b<nitem ; b++ ){
         fw->ty[mt]=srdm[(size_t)hold*THD_NTRI(nitem)+mt] ; fw->tpred[mt]=0.0f ;
         for( cc=0 ; cc<fm->ncomp ; cc++ ){
           int ia=perm?perm[a]:a,ib=perm?perm[b]:b ;
           float x=mv[fm->comp[cc]]->mat[ia*nitem+ib] ;
           fw->tpred[mt]+=fw->w[cc]*(x-fw->xmean[cc])/fw->xsd[cc] ;
         }
         mt++ ;
       }
     }
     { float r=rsa_fit_pearson(mt,fw->ty,fw->tpred),z=MYatanh(r),sw=0.0f ;
       zsum+=z ; nok++ ;
       if( foldz!=NULL ) foldz[hold]=z ;
       if( valid!=NULL ) valid[hold]=1 ;
       for( cc=0 ; cc<fm->ncomp ; cc++ ) sw+=fw->w[cc] ;
       if( meanw!=NULL && sw>0.0f )
         for( cc=0 ; cc<fm->ncomp ; cc++ ) meanw[cc]+=fw->w[cc]/sw ;
     }
   }
   if( nok<1 ) return 0.0f ;
   if( meanw!=NULL ) for( cc=0 ; cc<fm->ncomp ; cc++ ) meanw[cc]/=nok ;
   return tanhf((float)(zsum/nok)) ;
}

/*! F22 two-axis CV for classic RSA.  Each outer fold holds one subject out.
    Each inner descriptor fold then fits only other-subject dyads whose two
    conditions are outside that fold, and scores only held-subject dyads whose
    two conditions are inside it.  Cross-boundary dyads never enter either side.
    The returned effect is tanh(mean z over valid subject x condition folds). */
static float rsa_fit_subject_condition_cv( int nsub, int nitem, float *srdm,
                                           RSA_fitmodel *fm, THD_simmat **mv,
                                           int *perm, RSA_condfold *cf,
                                           RSA_fitws *fw, float *meanw,
                                           float *foldz, unsigned char *valid )
{
   int hold,hf,a,b,s,cc,m,mt,nok=0,ntri=THD_NTRI(nitem) ; double zsum=0.0 ;
   if( !srdm || !fm || !mv || !cf || cf->ncond!=nitem || !fw ) return 0.0f ;
   if( meanw!=NULL ) memset(meanw,0,sizeof(float)*fm->ncomp) ;
   if( valid!=NULL ) memset(valid,0,(size_t)nsub*cf->nfold) ;
   for( hold=0 ; hold<nsub ; hold++ ) for( hf=0 ; hf<cf->nfold ; hf++ ){
     m=0 ;
     for( s=0 ; s<nsub ; s++ ) if( s!=hold ){
       mt=0 ;
       for( a=0 ; a<nitem ; a++ ) for( b=a+1 ; b<nitem ; b++,mt++ ){
         if( cf->fold[a]==hf || cf->fold[b]==hf ) continue ;
         fw->y[m]=srdm[(size_t)s*ntri+mt] ;
         for( cc=0 ; cc<fm->ncomp ; cc++ ){
           int ia=perm?perm[a]:a,ib=perm?perm[b]:b ;
           fw->x[cc][m]=mv[fm->comp[cc]]->mat[ia*nitem+ib] ;
         }
         m++ ;
       }
     }
     if( !rsa_fit_nnls_ridge(m,fm->ncomp,fm->ridge,fw) ) continue ;
     mt=m=0 ;
     for( a=0 ; a<nitem ; a++ ) for( b=a+1 ; b<nitem ; b++,mt++ ){
       if( cf->fold[a]!=hf || cf->fold[b]!=hf ) continue ;
       fw->ty[m]=srdm[(size_t)hold*ntri+mt] ; fw->tpred[m]=0.0f ;
       for( cc=0 ; cc<fm->ncomp ; cc++ ){
         int ia=perm?perm[a]:a,ib=perm?perm[b]:b ;
         float x=mv[fm->comp[cc]]->mat[ia*nitem+ib] ;
         fw->tpred[m]+=fw->w[cc]*(x-fw->xmean[cc])/fw->xsd[cc] ;
       }
       m++ ;
     }
     { int loc=hold*cf->nfold+hf ;
       float r=rsa_fit_pearson(m,fw->ty,fw->tpred),z=MYatanh(r),sw=0.0f ;
       zsum+=z ; nok++ ;
       if( foldz!=NULL ) foldz[loc]=z ;
       if( valid!=NULL ) valid[loc]=1 ;
       for( cc=0 ; cc<fm->ncomp ; cc++ ) sw+=fw->w[cc] ;
       if( meanw!=NULL && sw>0.0f )
         for( cc=0 ; cc<fm->ncomp ; cc++ ) meanw[cc]+=fw->w[cc]/sw ;
     }
   }
   if( nok<1 ) return 0.0f ;
   if( meanw!=NULL ) for( cc=0 ; cc<fm->ncomp ; cc++ ) meanw[cc]/=nok ;
   return tanhf((float)(zsum/nok)) ;
}

static float rsa_fit_cv( int rdm_over, int nsub, int nitem,
                         float *srdm, THD_simmat *neural,
                         RSA_fitmodel *fm, THD_simmat **mv, int *perm,
                         RSA_condfold *cf, RSA_fitws *fw, float *meanw,
                         float *foldz, unsigned char *valid )
{
   if( cf!=NULL )
     return rsa_fit_subject_condition_cv(nsub,nitem,srdm,fm,mv,perm,cf,fw,meanw,
                                         foldz,valid) ;
   return rsa_fit_subject_cv(rdm_over,nsub,nitem,srdm,neural,fm,mv,perm,fw,meanw,
                             foldz,valid) ;
}

/*! Population superiority for two fitted models from their identically indexed
    held-out folds.  Every outer subject contributes the mean difference over
    folds valid for BOTH models; synchronized subject-bootstrap draws then test
    the centered paired effect and supply a spatial max-null. */
static THD_permstat rsa_fit_superiority_test(
       int nsub, int nfold, float *za, unsigned char *va,
       float *zb, unsigned char *vb, THD_resample_set *rset,
       float *draw, float *nullabs )
{
   THD_permstat ps ; float *dsub ; int ss,ff,bb,nok=0,nge=0,B=rset?rset->nresample:0 ;
   double sum=0.0 ;
   ps.stat=0.0f ; ps.pval=-1.0f ; ps.zscr=0.0f ; ps.nperm=B ;
   if( nsub<2 || nfold<1 || !za || !va || !zb || !vb ) return ps ;
   dsub=(float *)malloc(sizeof(float)*(size_t)nsub) ;
   if( dsub==NULL ) return ps ;
   for( ss=0 ; ss<nsub ; ss++ ){
     double ds=0.0 ; int nf=0 ;
     for( ff=0 ; ff<nfold ; ff++ ){
       int q=ss*nfold+ff ;
       if( va[q] && vb[q] ){ ds+=(double)za[q]-zb[q] ; nf++ ; }
     }
     dsub[ss]=(nf>0)?(float)(ds/nf):NAN ;
     if( nf>0 ){ sum+=dsub[ss] ; nok++ ; }
   }
   if( nok>0 ) ps.stat=(float)(sum/nok) ;
   ps.zscr=ps.stat ;
   if( rset==NULL || draw==NULL || B<1 || nok<2 ){ free(dsub) ; return ps ; }
   for( bb=0 ; bb<B ; bb++ ){
     int *ix=rset->index+(size_t)bb*nsub,n=0 ; double db=0.0 ; float dn ;
     for( ss=0 ; ss<nsub ; ss++ ) if( isfinite(dsub[ix[ss]]) ){
       db+=dsub[ix[ss]] ; n++ ;
     }
     draw[bb]=(n>0)?(float)(db/n):0.0f ;
     dn=fabsf(draw[bb]-ps.stat) ;
     if( nullabs!=NULL ) nullabs[bb]=dn ;
     if( dn>=fabsf(ps.stat) ) nge++ ;
   }
   ps.pval=(float)(nge+1)/(float)(B+1) ;
   ps.zscr=THD_perm_signed_z(ps.pval,ps.stat,PERM_TAIL_TWO) ;
   free(dsub) ; return ps ;
}

/*! Per-thread scratch for crossnobis noise normalization (4c).  mode NN_NONE
    means no whitening; then the buffers are unused/NULL. */
typedef struct {
   int    mode ;          /* NN_NONE / NN_DIAG / NN_SHRINK */
   float *residbuf ;      /* [maxNt * maxvox]   one run's residual patterns */
   float *Rmat ;          /* [maxNtot * maxvox] demeaned residuals, stacked   */
   float *Whalf ;         /* [maxvox * maxvox]  Sigma^{-1/2} (NN_SHRINK only)  */
   float *wdiag ;         /* [maxvox]           per-voxel weights (NN_DIAG)    */
   float *wtmp ;          /* [maxvox]           matrix-vector scratch          */
} RSA_whiten ;

/*! Materialized condition-bootstrap samples.  Each draw contains a variable
    number of original condition indices because a sampled descriptor group is
    expanded to all of its member conditions. */
typedef struct {
   int nresample, ncond, ngroup, maxitem, maxtri, nvalid ;
   int *nitem ;             /* [nresample] expanded positions per draw */
   int *offset ;            /* [nresample+1] offsets into index */
   int *index ;             /* concatenated expanded original-condition indices */
   unsigned char *valid ;   /* draw has at least 3 distinct original conditions */
} RSA_cond_resample ;

static double rsa_dset_bytes( THD_3dim_dataset *ds )
{
   double nb=0.0 ; int vv ;
   if( ds == NULL ) return 0.0 ;
   for( vv=0 ; vv < DSET_NVALS(ds) ; vv++ ) nb += (double)DSET_BRICK_BYTES(ds,vv) ;
   return nb ;
}

/*! Strict numeric option readers: consume the whole token and reject overflow
    or non-finite floating-point input. */
static long rsa_parse_long( const char *opt, const char *arg, long lo, long hi )
{
   char *ep=NULL ; long v ; errno=0 ;
   if( arg==NULL || arg[0]=='\0' ) ERROR_exit("3dRSA: %s needs an integer",opt) ;
   v=strtol(arg,&ep,10) ;
   if( errno==ERANGE || ep==arg || *ep!='\0' || v<lo || v>hi )
     ERROR_exit("3dRSA: %s value '%s' is not an integer in [%ld,%ld]",opt,arg,lo,hi) ;
   return v ;
}

static double rsa_parse_double( const char *opt, const char *arg, double lo, double hi )
{
   char *ep=NULL ; double v ; errno=0 ;
   if( arg==NULL || arg[0]=='\0' ) ERROR_exit("3dRSA: %s needs a number",opt) ;
   v=strtod(arg,&ep) ;
   if( errno==ERANGE || ep==arg || *ep!='\0' || !isfinite(v) || v<lo || v>hi )
     ERROR_exit("3dRSA: %s value '%s' is not a finite number in [%.7g,%.7g]",
                opt,arg,lo,hi) ;
   return v ;
}

static void rsa_require_finite_column( THD_datatable *tab, int col, const char *role )
{
   int rr ;
   if( tab==NULL || col<0 || col>=tab->ncol || !tab->isnum[col] ) return ;
   for( rr=0 ; rr<tab->nrow ; rr++ ) if( !isfinite(tab->val[col][rr]) )
     ERROR_exit("3dRSA: non-finite %s value '%s' in column '%s', row %d, Subj %s",
                role,DT_CELL(tab,rr,col),tab->cname[col],rr+1,tab->subj[rr]) ;
}

static void rsa_validate_mask( THD_3dim_dataset *ds, int atlas, const char *role )
{
   int vv,n=DSET_NVOX(ds) ;
   for( vv=0 ; vv<n ; vv++ ){
     float x=THD_get_voxel(ds,vv,0) ;
     if( !isfinite(x) )
       ERROR_exit("3dRSA: %s '%s' has a non-finite value at voxel/node %d",
                  role,DSET_HEADNAME(ds),vv) ;
     if( atlas && x>0.0f && x!=floorf(x) )
       ERROR_exit("3dRSA: atlas %s '%s' has non-integer label %.9g at voxel/node %d",
                  role,DSET_HEADNAME(ds),x,vv) ;
   }
}

/*! Scan only the union of features actually analyzed.  Values outside that
    domain cannot enter an RDM and are intentionally ignored. */
static void rsa_validate_dset_domain( THD_3dim_dataset *ds, THD_roilist *rl,
                                      const char *role, const char *owner )
{
   unsigned char *use ; int kk,ii,bb,nvox,nval,was_loaded ;
   long long qq,bad=LLONG_MAX,ntot ;
   if( ds==NULL || rl==NULL ) return ;
   was_loaded=DSET_LOADED(ds) ;
   DSET_load(ds) ; CHECK_LOAD_ERROR(ds) ; nvox=DSET_NVOX(ds) ;
   use=(unsigned char *)calloc((size_t)nvox,1) ;
   if( use==NULL ) ERROR_exit("3dRSA: cannot allocate finite-data validation mask") ;
   for( kk=0 ; kk<rl->nroi ; kk++ ) for( ii=0 ; ii<rl->vox[kk].nar ; ii++ )
     if( rl->vox[kk].ar[ii]>=0 && rl->vox[kk].ar[ii]<nvox ) use[rl->vox[kk].ar[ii]]=1 ;
   nval=DSET_NVALS(ds) ; ntot=(long long)nval*nvox ;
   /* The arrays are fully resident and read-only here.  Record the earliest
      invalid brick/voxel in a reduction, then issue any AFNI error outside the
      parallel region.  This preserves deterministic diagnostics. */
   AFNI_OMP_START ;
#ifdef USE_OMP
#pragma omp parallel for if(ntot >= 1000000LL) schedule(static) reduction(min:bad)
#endif
   for( qq=0 ; qq<ntot ; qq++ ){
     int iv=(int)(qq%nvox), bv=(int)(qq/nvox) ;
     if( use[iv] && !isfinite(THD_get_voxel(ds,iv,bv)) && qq<bad ) bad=qq ;
   }
   AFNI_OMP_END ;
   if( bad!=LLONG_MAX ){
     const char *lab ; bb=(int)(bad/nvox) ; ii=(int)(bad%nvox) ;
     lab=DSET_BRICK_LABEL(ds,bb) ; free(use) ;
     ERROR_exit("3dRSA: non-finite %s data in '%s'%s%s%s: brick %d%s%s, voxel/node %d",
                role,DSET_HEADNAME(ds),owner?" (":"",owner?owner:"",owner?")":"",bb,
                (lab&&lab[0])?" label ":"",(lab&&lab[0])?lab:"",ii) ;
   }
   free(use) ;
   /* Preserve the caller's residency state: in searchlight mode this check
      deliberately precedes the resident-memory preflight. */
   if( !was_loaded ) DSET_unload(ds) ;
}

/*! Estimate and enforce the searchlight memory contract before the many input
    datasets are loaded.  This is a preflight, not an allocator guarantee:
    AFNI/library bookkeeping and compression buffers vary by file format, so
    the default hard limit leaves 20% of physical RAM outside the estimate. */
static void rsa_searchlight_memory_preflight(
   THD_3dim_dataset *mset, THD_roilist *rl, THD_3dim_dataset **dset,
   THD_runset *runset, RSA_series_runs *series_runs,
   THD_datatable_index *condition_index, int run_analysis, int nruncon,
   RSA_model *mod, int nmod, int nort, int nsub,
   int mode, int rdm_over, int nitem, int nvals, int nperm, int null_mode,
   int nboot, int ncboot, int ncon, int ncomq, int nfit, int nfitw, int nfitcon,
   int do_loo,
   int nloo, int nloofam, int do_nc,
   int noise_norm, int do_dset, int cmp, int joint,
   double limit_gib, int limit_given,
   int memory_override, int quiet )
{
   const double GIB=1073741824.0 , FS=(double)sizeof(float) ;
   THD_memory_plan me ;
   double warn=0.0 , nfwe=(nperm>0)?(double)nperm:0.0 ;
   int jj,mm,kk,nfixed=0,ndset=0,maxvox,nfeat,ncol,ntri,nbrik,maxcond=nvals,maxloo=1 ;
   int dualboot=(nboot>0 && ncboot>0) ;

   memset(&me,0,sizeof(me)) ;
#ifdef USE_OMP
   me.nthread=omp_get_max_threads() ;
#else
   me.nthread=1 ;
#endif
   if( me.nthread < 1 ) me.nthread=1 ;

   maxvox=THD_roilist_maxvox(rl) ; ntri=THD_NTRI(nitem) ;
   ncol = joint ? nmod+nort : ((nort>0) ? 1+nort : 1) ;
   if( ncomq>0 && ncol<3 ) ncol=3 ;
   if( ncol < 1 ) ncol=1 ;
   nfeat = (mode==MODE_CONT) ? nvals
          : (mode==MODE_RDM) ? THD_NTRI(nvals)
          : ((rdm_over==RDM_SUBJ) ? maxvox*nvals : maxvox) ;

   me.input += rsa_dset_bytes(mset) ;
   if( runset != NULL ){
     for( jj=0 ; jj < runset->nrow ; jj++ ){
       me.input += rsa_dset_bytes(runset->betas[jj]) ;
       if( noise_norm != NN_NONE ) me.input += rsa_dset_bytes(runset->resid[jj]) ;
     }
   } else {
     int nn=(series_runs!=NULL)?series_runs->nrow:
            (condition_index!=NULL)?condition_index->ncell:nsub ;
     for( jj=0 ; jj < nn ; jj++ ) me.input += rsa_dset_bytes(dset[jj]) ;
   }
   for( mm=0 ; mm < nmod ; mm++ ){
     if( mod[mm].kind == MODK_DSET ){
       int mf=(mode==MODE_CONT) ? mod[mm].mvals
              : (mode==MODE_RDM) ? THD_NTRI(mod[mm].mvals)
                                 : maxvox*mod[mm].mvals ;
       ndset++ ; if( mf > nfeat ) nfeat=mf ;
       if( mod[mm].mvals > maxcond ) maxcond=mod[mm].mvals ;
       for( jj=0 ; jj < nsub ; jj++ ) me.input += rsa_dset_bytes(mod[mm].dset[jj]) ;
     } else {
       nfixed++ ;
       if( mod[mm].kind==MODK_RUNCOLUMN && mod[mm].run_mat!=NULL )
         me.shared += (double)(series_runs->nrun-1)*nitem*nitem*FS ;
     }
   }

   /* Searchlight neighborhood lists are already resident at preflight time. */
   me.geometry += sizeof(THD_roilist)
                + (double)rl->nroi*(2.0*sizeof(int)+sizeof(intvec)+sizeof(char *)) ;
   for( kk=0 ; kk < rl->nroi ; kk++ )
     me.geometry += (double)rl->vox[kk].nar*sizeof(int) ;

   /* Fixed matrices, shared relabelings/resamples, result columns, and the
      synchronized max-null arrays do not scale with the OpenMP thread count. */
   me.shared += (double)(nfixed+nort)*nitem*nitem*FS ;
   if( nperm > 0 && null_mode != NULL_PHASE )
     me.shared += (double)nperm*nsub*
                  ((null_mode==NULL_TIMESHIFT) ? sizeof(int)
                                               : sizeof(int)+sizeof(signed char)) ;
   if( nperm > 0 && ncomq > 0 && rdm_over==RDM_BRICK )
     me.shared += (double)nperm*nitem*sizeof(int) ; /* F15 condition permutations */
   /* F9/F23: fixed Pearson/Spearman/rho-a model triangles are cached once for every
      label relabeling when separate tests or fixed contrasts will consume
      them.  Include that potentially large speed/memory tradeoff in F11. */
   if( rdm_over==RDM_SUBJ && nperm>0 && null_mode==NULL_LABELS &&
       (cmp==CMP_PEARSON || cmp==CMP_SPEARMAN || cmp==CMP_RHOA) &&
       ((!joint && nort==0) || ncon>0) )
     me.shared += (double)nfixed*nperm*(ntri+1)*FS
                + (double)nmod*(sizeof(int)+sizeof(THD_simmat *)) + 128.0 ;
   if( nboot  > 0 ) me.shared += (double)nboot*nsub*sizeof(int) ;
   if( nperm>0 && ((ncon>0 && rdm_over==RDM_SUBJ) || nfitcon>0) )
     me.shared += (double)nperm*nsub*sizeof(int) ; /* possible superiority draws */
   if( ncboot > 0 ) me.shared += (double)ncboot*nitem*sizeof(int)*1.5 ;
   me.shared += (double)rl->nroi*FS*
       ( nmod*(5 + (nperm>0?2:0) + (nboot>0?2:0)
                    + ((ncboot>0&&!dualboot)?2:0)
                    + (do_loo?4:0) + ((do_loo&&nperm>0)?2:0))
         + ncon*(5+(nperm>0?2:0)+(nboot>0?2:0))
         + ncomq*(4+(nperm>0?2:0)+(nboot>0?2:0))
         + nfitcon*(4+(nperm>0?2:0))
         + (do_nc ? 2 : 0) + ((do_loo&&nboot>0)?2*nloo:0) ) ;
   if( nperm > 0 ){
     me.shared += nfwe*FS*(nmod+nloofam+ncon) ;
     me.shared += (double)nperm*FS*(ncomq+nfitcon) ;
   }

   /* THD_rdm_ws, feature/RDM buffers, inference null scratch, and the
      runwise whitening workspace are allocated once per OpenMP thread. */
   me.per_thread += FS*( (9.0+2.0*ncol)*ntri + ncol+1.0 + nitem*nitem ) ;
   me.per_thread += FS*( (double)nitem*nfeat + (double)(1+ndset)*nitem*nitem ) ;
   if( do_loo ){
     double lpeak=4.0*nitem ; int lp ;
     for( mm=0 ; mm<nmod ; mm++ ) if( rsa_model_has_loo(mod+mm) ){
       double need ;
       if( mod[mm].ncol>maxloo ) maxloo=mod[mm].ncol ;
       if( mod[mm].ncol==1 && mod[mm].rule==RUL_ANNAK )
         need=(double)nitem*nitem+4.0*nitem ;
       else if( mod[mm].ncol>1 ){
         lp=mod[mm].ncol ;
         need=2.0*lp*nitem+2.0*nitem
              +(sizeof(float *)/FS)*(double)lp ;
       } else need=4.0*nitem ;
       if( need>lpeak ) lpeak=need ;
     }
     me.per_thread += FS*lpeak ;
     if( nboot>0 ) me.per_thread += FS*((double)maxloo*nitem+2.0*nitem)
                                  + sizeof(float *)*(double)maxloo ;
   }
   if( mode==MODE_RDM ) me.per_thread += FS*(double)maxcond*maxvox ;
   /* A4c: classic RSA retains one compact neural triangle per subject at the
      current location; joint fits also retain the fixed model triangles. */
   if( rdm_over==RDM_BRICK ){
     me.per_thread += FS*(double)nsub*ntri ;
     if( cmp==CMP_CORR_COV || cmp==CMP_COS_COV )
       me.per_thread += FS*(double)(nsub+nmod)*nitem*nitem
                      + sizeof(float *)*(double)nmod ;
     if( joint ) me.per_thread += FS*(double)nmod*ntri
                                + sizeof(float *)*(double)nmod ;
     if( ncomq > 0 ) me.per_thread += FS*(6.0*ntri
                                  + ((nboot>0)?RSA_MAXCOMMON*nsub:0.0)) ;
   }
   if( nboot > 0 ){
     me.per_thread += FS*nboot + sizeof(int)*(double)nsub ;
     if( rdm_over==RDM_SUBJ && (joint || nort>0) )
       me.per_thread += FS*((1.0+ncol)*ntri + (double)nmod*nboot + ncol)
                      + sizeof(float *)*(double)ncol ;
     if( ncomq > 0 )
       me.per_thread += FS*(4.0*ntri + (double)RSA_MAXCOMMON*nboot) ;
   }
   if( ncboot > 0 ){
     double cbextra=dualboot
       ? (double)(joint?nmod:1)*nsub*ncboot + 4.0*ncboot
       : (double)nmod*ncboot + ncboot ;
     me.per_thread += FS*((10.0+2.0*nmod)*ntri+nmod+1.0+cbextra) ;
   }
   if( nperm > 0 ){
     me.per_thread += nfwe*FS*(2.0*nmod + nloofam + 2.0*ncon) ;
     if( ncon>0 && rdm_over==RDM_SUBJ )
       me.per_thread += nfwe*FS + sizeof(int)*(double)nsub ;
     me.per_thread += (double)nperm*FS*(ncomq+(ncomq>0?RSA_MAXCOMMON:0)) ;
     if( null_mode==NULL_TIMESHIFT ){
       double nlag=0.5*(double)nsub*(nsub-1.0)*nvals ;
       double ntscol=(double)(nmod+nort) ;
       double ndesign=joint ? ntscol*ntri
                     : (nort>0) ? (double)nmod*(1+nort)*ntri : 0.0 ;
       me.shared += nlag*sizeof(unsigned char) ;
       /* F19: main series + prepared series + norms + pair-by-lag table and
          one reusable shifted neural matrix.  F18 adds fixed triangle columns,
          one response triangle, retained regression pseudoinverses, and compact
          coefficient/contrast scratch; none scales with the number of draws. */
       me.per_thread += FS*(2.0*nsub*nvals+nsub+nlag+nitem*nitem) ;
       me.per_thread += FS*((ntscol+1.0)*ntri+ndesign
                           + 2.0*nmod+ntscol+3.0*ncon)
                      + sizeof(int)*(double)(nmod+ncon)
                      + sizeof(float *)*(ntscol+nmod) ;
     } else if( null_mode==NULL_PHASE ){
       double ntscol=(double)(nmod+nort) ;
       double ndesign=joint ? ntscol*ntri
                     : (nort>0) ? (double)nmod*(1+nort)*ntri : 0.0 ;
       /* F5b: one location's subject spectra are retained per worker and
          reused over all draws.  They are replaced, not accumulated, when
          that worker advances to another searchlight center. */
       me.per_thread += FS*(4.0*nsub*nvals+4.0*nvals+nitem*nitem) ;
       me.per_thread += FS*((ntscol+1.0)*ntri+ndesign
                           +2.0*nmod+ntscol+3.0*ncon)
                      + sizeof(int)*(double)(nmod+ncon)
                      + sizeof(float *)*(ntscol+nmod) ;
     }
   }
   if( runset != NULL ){
     int ss,maxrun=0,maxnt=0,maxntot=0 ;
     for( ss=0 ; ss < runset->nsub ; ss++ ){
       int rr,ntot=0 ;
       if( runset->nrun[ss] > maxrun ) maxrun=runset->nrun[ss] ;
       for( rr=0 ; rr < runset->nrun[ss] ; rr++ ){
         int row=runset->row_of[ss][rr] ;
         int nt=(noise_norm!=NN_NONE) ? DSET_NVALS(runset->resid[row]) : 0 ;
         if( nt > maxnt ) maxnt=nt ;
         ntot += nt ;
       }
       if( ntot > maxntot ) maxntot=ntot ;
     }
     me.per_thread += FS*(double)maxrun*nvals*maxvox ;
     if( runset->has_condmap )
       me.per_thread += FS*(double)runset->maxbrick*maxvox ;
     if( noise_norm != NN_NONE )
       me.per_thread += FS*((double)(maxnt+maxntot)*maxvox + 2.0*maxvox) ;
     if( noise_norm == NN_SHRINK ) me.per_thread += FS*(double)maxvox*maxvox ;
   }
   if( series_runs!=NULL && run_analysis!=RUN_ANALYSIS_CONCAT ){
     int ru,maxrv=0 ;
     for( ru=0 ; ru<series_runs->nrun ; ru++ )
       if( series_runs->run_nval[ru]>maxrv ) maxrv=series_runs->run_nval[ru] ;
     me.per_thread += FS*((double)series_runs->nrun*ntri+(double)nsub*maxrv
                          +(nperm>0?(double)(joint?nmod:1)*(series_runs->nrun+nruncon+3)*nperm:0.0)
                          +5.0*nruncon) ;
     me.shared += FS*(double)nmod*nruncon*rl->nroi*(5+(nperm>0?2:0)) ;
   }
   if( do_nc ) me.per_thread += FS*(2.0*ntri + (double)nitem*nfeat) ;
   if( nfit>0 ){
     double mt=(rdm_over==RDM_SUBJ) ? 0.5*(nsub-1.0)*(nsub-2.0)
                                    : (nsub-1.0)*ntri ;
     double mv=(rdm_over==RDM_SUBJ) ? nsub-1.0 : ntri ;
     me.per_thread += FS*((2.0*nfit+nfitw)*mt + 2.0*nfit*mv
                          + 4.0*nfitw + 2.0*(nfit+nfitcon)*nperm) ;
     me.shared += FS*(double)(nfit+nfitcon)*nperm ;
   }

   /* Output bricks coexist with the still-loaded searchlight inputs at write
      time, so count their full grid rather than only in-mask centers. */
   nbrik = 2*nmod + 2*nloo + ((nperm>0)?nmod:0)
           + ((do_loo&&nperm>0)?nloo:0)
           + ((rdm_over==RDM_BRICK)?3:2)*ncon + ((nperm>0)?ncon:0)
           + 2*ncomq + ((nperm>0)?ncomq:0)
           + 2*nfit + ((nperm>0)?nfit:0) + nfitw
           + 2*nfitcon + ((nperm>0)?nfitcon:0)
           + (do_nc ? ((rdm_over==RDM_SUBJ)?1:2) : 0)
           + ((nboot>0)?2*(nmod+ncon+ncomq+nloo):0)
           + ((ncboot>0&&!dualboot)?2*nmod:0) ;
   if( series_runs!=NULL && run_analysis!=RUN_ANALYSIS_CONCAT ){
     int ns=1+((run_analysis==RUN_ANALYSIS_SEPARATE)?series_runs->nrun:0)+nruncon ;
     nbrik=nmod*ns*(2+((nperm>0)?1:0)) ;
   }
   if( do_dset ) me.output=(double)DSET_NVOX(mset)*nbrik*FS ;

   me.system=(double)AFNI_get_memsize() ;
   me.limit=limit_given ? limit_gib*GIB
                        : ((me.system>0.0) ? 0.80*me.system : 0.0) ;
   warn=limit_given ? 0.75*me.limit
                    : ((me.system>0.0) ? 0.50*me.system : 0.0) ;
   THD_memory_plan_finish(&me) ;

   if( !quiet ){
     if( me.limit > 0.0 )
       INFO_message("3dRSA: searchlight memory preflight: estimated peak %.3f GiB\n"
                    "       = %.3f input + %.3f neighborhoods/shared/output"
                    " + %d x %.3f thread scratch; limit %.3f GiB (%s)",
                    me.total/GIB,me.input/GIB,(me.geometry+me.shared+me.output)/GIB,
                    me.nthread,me.per_thread/GIB,me.limit/GIB,
                    limit_given?"-memory_limit":"80% of detected RAM") ;
     else
       INFO_message("3dRSA: searchlight memory preflight: estimated peak %.3f GiB\n"
                    "       = %.3f input + %.3f neighborhoods/shared/output"
                    " + %d x %.3f thread scratch; system RAM is unavailable",
                    me.total/GIB,me.input/GIB,(me.geometry+me.shared+me.output)/GIB,
                    me.nthread,me.per_thread/GIB) ;
   }

   if( me.limit > 0.0 && me.total > me.limit ){
     if( !memory_override )
       ERROR_exit("3dRSA: estimated searchlight peak memory %.3f GiB exceeds the\n"
                  "       %.3f GiB %s limit. Reduce OMP_NUM_THREADS, use a smaller\n"
                  "       mask/input, add -no_dset, or set a suitable -memory_limit.\n"
                  "       If this allocation is intentional, rerun with\n"
                  "       -memory_override to acknowledge the risk.",
                  me.total/GIB,me.limit/GIB,limit_given?"-memory_limit":"system-RAM") ;
     WARNING_message("3dRSA: estimated peak %.3f GiB exceeds the %.3f GiB limit;\n"
                     "       continuing because -memory_override was given",
                     me.total/GIB,me.limit/GIB) ;
   } else if( warn > 0.0 && me.total > warn ){
     WARNING_message("3dRSA: estimated searchlight peak memory is %.3f GiB\n"
                     "       (more than %s of the applicable limit/system RAM).\n"
                     "       Consider fewer OMP_NUM_THREADS or a smaller mask.",
                     me.total/GIB,limit_given?"75%":"50%") ;
   }
}

/*============================================================================*/

/* AFNI command help is intentionally comprehensive.  GCC/Clang both support
   this 40+ KB literal, but -Wpedantic warns because ISO C only guarantees 4095
   characters.  Scope that implementation-limit warning to the help text; do
   not hide it (or any other warning) from the analysis code. */
#if defined(__GNUC__)
# pragma GCC diagnostic push
# pragma GCC diagnostic ignored "-Woverlength-strings"
#endif

void usage_3dRSA(int detail)
{
   (void)detail ;   /* full help is always printed; kept for the AFNI calling convention */
   printf(
"\n"
"Usage: 3dRSA [options]   ~1~\n"
"\n"
"Representational Similarity Analysis (RSA) over atlas ROIs or searchlights.\n"
"\n"
"For each ROI, 3dRSA builds a square NEURAL similarity matrix from your\n"
"imaging data, builds one or more MODEL matrices from behavior (or from\n"
"another modality, or from a file), and asks how well the models explain the\n"
"neural one.  Only the strict upper triangles are compared, since the\n"
"matrices are symmetric and the diagonal carries no information.\n"
"\n"
"Significance always comes from permutation.  This is not a stylistic\n"
"preference: the entries of a matrix triangle are NOT independent, because n\n"
"items give n(n-1)/2 pairs but only n independent units.  Parametric p-values\n"
"over the triangle are therefore badly anticonservative.  The permutation\n"
"relabels the ITEMS, applying the same shuffle to rows and columns together\n"
"-- shuffling triangle entries instead is the classic Mantel error.\n"
"\n"
"Two families of analysis, chosen with '-mode':\n"
"\n"
"  -mode IS-RSA   (inter-subject RSA, the default)   ~2~\n"
"    Rows of the matrix are SUBJECTS.  Each subject contributes one feature\n"
"    vector per ROI (by default the ROI-mean time course), the neural matrix\n"
"    holds subject-by-subject similarity, and the model comes from a\n"
"    behavioral column.  Asks: do people who are behaviorally similar have\n"
"    similar brain responses?\n"
"\n"
"  -mode RSA   (classic within-subject RSA)   ~2~\n"
"    Rows of the matrix are SUB-BRICKS (conditions).  Each subject gets their\n"
"    own condition-by-condition matrix per ROI, compared against a model\n"
"    matrix from '-model_mat' or their own fixed '-seed_mask' RDM. The\n"
"    per-subject correlations are Fisher-z\n"
"    transformed and tested against zero across subjects.\n"
"\n"
"--------------\n"
"Input options:   ~1~\n"
"--------------\n"
"  -mask AAA    = Atlas (or ROI mask) dataset.  REQUIRED, with one exception\n"
"                 (below).  Every distinct non-zero integer value is a separate\n"
"                 ROI.  If the dataset carries a label table (as AFNI atlases\n"
"                 do), those labels appear in the output table.\n"
"                   ++ EXCEPTION: a surface '-searchlight' (with '-surf') may\n"
"                      omit '-mask' to search the WHOLE mesh.  Unlike a volume,\n"
"                      a surface has no voxels that are skull, ventricle or air\n"
"                      -- every node is cortex -- so 'search everywhere' has a\n"
"                      coherent meaning a volumetric searchlight does not have,\n"
"                      where '-mask' stays required.  Expect a large, slow,\n"
"                      genuinely exploratory run.\n"
"\n"
"  -roi_sel LLL = Restrict to this list of ROI values, in the usual AFNI\n"
"                 int-list syntax, e.g. '1,3,7..12'.\n"
"\n"
"  -seed_mask SSS = Seed representational connectivity (S5). SSS is a mask or\n"
"                 atlas on the input grid defining ONE fixed seed ROI. 3dRSA\n"
"                 builds the seed geometry from the same subjects and then\n"
"                 compares it with every target ROI/searchlight from -mask.\n"
"                   ++ If SSS contains multiple positive atlas values, add\n"
"                      '-seed_roi VALUE' to select exactly one. The selector\n"
"                      uses the same AFNI integer-list grammar as -roi_sel but\n"
"                      must resolve to one ROI.\n"
"                   ++ Every target containing ANY seed voxel/node is removed\n"
"                      before BH and max-FWE inference. This is intentional:\n"
"                      shared measured features create seed-to-self noise\n"
"                      correlation. The excluded and searched counts are\n"
"                      recorded in the output.\n"
"                   ++ IS-RSA correlates the seed and target subject geometries\n"
"                      with the ordinary synchronized subject-label/null path.\n"
"                      Classic RSA correlates subject s's seed condition RDM\n"
"                      with subject s's target RDM, reports tanh(mean Fisher z),\n"
"                      and uses subject sign flips or '-classic_null conditions'.\n"
"                   ++ The seed uses the target's -featuretype, neural/condition\n"
"                      metrics, centering, runwise crossnobis, and noise\n"
"                      normalization. A seed is not a separately tuned model.\n"
"                   ++ -seed_mask is the complete one-model analysis: do not\n"
"                      combine it with model/joint/contrast/commonality/fitted/\n"
"                      LOO options. Subject -bootstrap and target reliability/\n"
"                      noise-ceiling output remain available. Condition\n"
"                      bootstrap and corr_cov/cosine_cov are rejected until\n"
"                      their two-noisy-RDM covariance contracts are defined.\n"
"\n"
"  -seed_roi RRR = Select the one positive atlas value used from -seed_mask.\n"
"                 It is an error without -seed_mask or if multiple ROIs remain.\n"
"\n"
"  -searchlight NNN = Instead of atlas parcels, center a moving neighborhood on\n"
"                 every non-zero voxel of the -mask and run RSA in each,\n"
"                 painting the result at the center voxel -- a whole-brain map.\n"
"                 NNN is a radius in mm (a sphere), or an explicit neighborhood\n"
"                 in the same grammar as 3dLocalstat's -nbhd:\n"
"                   -searchlight 6              (sphere, radius 6 mm)\n"
"                   -searchlight 'SPHERE(6)'    (the same)\n"
"                   -searchlight 'RECT(6,6,6)'  (a box)\n"
"                   -searchlight 'RHDD(6)' / 'TOHD(6)'\n"
"                 Supports IS-RSA and both forms of classic RSA:\n"
"                   ++ With '-dataTable', each sphere builds an ORDINARY, SAME-\n"
"                      DATA condition RDM for every subject, just as atlas-mode\n"
"                      RSA does.  The same condition estimates define and are\n"
"                      evaluated through that RDM, so it is not cross-validated\n"
"                      and does not have crossnobis's unbiased zero point.\n"
"                   ++ With '-runwiseTable', each sphere instead builds each\n"
"                      subject's CROSS-VALIDATED crossnobis condition RDM, with\n"
"                      '-noise_norm none', 'diag', or 'shrinkage'.  Prefer this\n"
"                      when conditions have independent repeated estimates.\n"
"                 Both reuse the usual subject test/FDR/max-FWE path.\n"
"                 All input datasets are held in memory, so watch memory on\n"
"                 full-res data -- and doubly so with IS-RSA '-model_dset', which\n"
"                 keeps the second modality resident too.  '-model_dset' IS\n"
"                 supported: the\n"
"                 cross-modal model is rebuilt from that other modality in each\n"
"                 moving sphere, so you get a whole-brain map of where the two\n"
"                 modalities share a subject geometry.  '-save_rdm' is not (it\n"
"                 would write one matrix per voxel).\n"
"\n"
"  -memory_limit G = Searchlight peak-memory limit in GiB.  Before loading all\n"
"                 subject datasets, 3dRSA estimates resident inputs, neighborhood\n"
"                 indices, shared result/null/output arrays, and per-OpenMP-thread\n"
"                 scratch (including crossnobis whitening).  By default it warns\n"
"                 above 50%% of detected physical RAM and refuses above 80%%.\n"
"                 This option replaces the 80%% limit with G GiB, which is useful\n"
"                 under a scheduler/container whose job limit is below host RAM;\n"
"                 with an explicit limit, the warning begins at 75%% of G.\n"
"\n"
"  -memory_override = Continue when the searchlight estimate exceeds its limit.\n"
"                 This explicitly acknowledges overcommit risk; it does not make\n"
"                 the allocation smaller.  Reducing OMP_NUM_THREADS often does.\n"
"\n"
"  -surf SSS    = Run the searchlight on a SURFACE instead of in the volume.\n"
"                 SSS is the mesh geometry (a .gii/.asc/... surface), and the\n"
"                 inputs (and -mask, if given) are surface datasets\n"
"                 (.niml.dset/.gii) on that mesh.  Each in-mask node (every node,\n"
"                 if '-mask' is omitted -- see above) centers a neighborhood of\n"
"                 the nodes within '-searchlight R' mm ALONG THE SURFACE\n"
"                 (geodesic, via SUMA), and the result is painted at the center\n"
"                 node.\n"
"                   ++ With -surf, -searchlight takes a plain geodesic radius\n"
"                      in mm (not a SPHERE()/RECT() shape).\n"
"                   ++ Needs a build with surface support (compile -DUSE_SUMA,\n"
"                      link libSUMA); otherwise 3dRSA still does volume and\n"
"                      surface mask/atlas RSA, just not the surface searchlight.\n"
"\n"
"  -mode MMM    = 'IS-RSA' (the default) or 'RSA', as above.\n"
"\n"
"  -featuretype TTT = For IS-RSA, what each subject's feature vector is:\n"
"                   mean    = the ROI-mean time course [default].  Use for\n"
"                             resting state, movie/story listening, or any\n"
"                             continuously sampled data.\n"
"                   pattern = the ROI's spatial pattern, across voxels and\n"
"                             sub-bricks.  Use when the inputs are condition\n"
"                             betas and you want inter-subject similarity of\n"
"                             the spatial response rather than of a timecourse.\n"
"                             ++ '-noise_ceiling' is rejected for this feature\n"
"                                type: the input has no matched-repetition axis\n"
"                                from which to form a reliability split.\n"
"                   rdm     = SECOND-ORDER task-fMRI IS-RSA.  Build every\n"
"                             subject's condition-by-condition RDM inside this\n"
"                             ROI/searchlight, reduce it to its strict triangle,\n"
"                             then compare those RDM vectors across subjects.\n"
"                             This does not require voxel-by-voxel anatomical\n"
"                             correspondence between subjects.  With -dataTable,\n"
"                             '-condition_metric' builds each condition RDM.\n"
"                             With -runwiseTable, each subject RDM is crossnobis\n"
"                             (and may use '-noise_norm').  '-neural_metric' then\n"
"                             compares subject RDM vectors to build the outer\n"
"                             subject-by-subject neural matrix.\n"
"                 Not used with '-mode RSA', which is always pattern-based.\n"
"\n"
"  -polort m    = Detrend each feature vector with a polynomial of order m,\n"
"                 m = -1..9.  Default -1, meaning NO detrending, assuming\n"
"                 your data are preprocessed.  Only for IS-RSA mean features.\n"
"\n"
"-----------------\n"
"Specifying data:   ~1~\n"
"-----------------\n"
"  -dataTable   = The table of subjects, measures and datasets, in the format\n"
"                 used by 3dMVM and 3dLME: a header row of column names, then\n"
"                 one row per input.  'InputFile' holds the dataset; 'Subj'\n"
"                 labels the subject.  Every other column is available to\n"
"                 '-model' (if numeric) or '-model_dset' (if file names).\n"
"\n"
"                 Three ways to give it:\n"
"                   -dataTable Subj MADRS InputFile s01 22 s01+tlrc ...\n"
"                   -dataTable @table.txt\n"
"                   -dataTableFile table.txt\n"
"\n"
"                 In a FILE the header fixes the column count, so 'InputFile'\n"
"                 may be in any column and a mis-counted row is reported with\n"
"                 its line number.  Blank lines and '#' comments are ignored,\n"
"                 and a line ending in backslash continues onto the next.\n"
"                 Given DIRECTLY on the command line there is no line\n"
"                 structure, so 'InputFile' must be the LAST column.\n"
"\n"
"                 Leave a subject with a missing dataset out of the table.\n"
"                 3dRSA does no guessing about who is present.\n"
"\n"
"  -condition_column CCC = Alternative long-table input for traditional RSA.\n"
"                 CCC names the condition-label column; each InputFile row must\n"
"                 resolve to exactly ONE beta brick (an AFNI selector is fine):\n"
"\n"
"                   Subj cond   InputFile\n"
"                   s01  house  s01_betas+tlrc[house]\n"
"                   s01  face   s01_betas+tlrc[face]\n"
"                   s01  tree   s01_betas+tlrc[tree]\n"
"\n"
"                 Rows may appear in ANY order. They are matched by Subj and\n"
"                 condition values, not row position. Every subject must have\n"
"                 exactly one row for every declared condition; duplicate,\n"
"                 missing, and unexpected subject/condition cells are errors.\n"
"                 Requires '-mode RSA' and '-condition_order'.\n"
"\n"
"  -condition_order L1,L2,... = Row/column order of every fixed -model_mat when\n"
"                 -condition_column is used. This is MODEL-MATRIX order, not\n"
"                 data-table row order. For example:\n"
"\n"
"                   -condition_column cond \\\n"
"                   -condition_order house,face,tree\n"
"\n"
"                 The labels are exact and case-sensitive. A numeric model\n"
"                 matrix is unlabeled, so this explicit binding prevents a\n"
"                 shuffled table from silently changing its interpretation.\n"
"\n"
"  -run_column CCC = Native multiple-run continuous IS-RSA. CCC names a column\n"
"                 in -dataTable identifying repeated run rows for each Subj:\n"
"\n"
"                   Subj Run behavior InputFile\n"
"                   s01  1   22       s01_run1+tlrc\n"
"                   s01  2   22       s01_run2+tlrc\n"
"\n"
"                 Each run is ROI-averaged and -polort detrended separately,\n"
"                 normalized within run, then concatenated in the first\n"
"                 subject's labeled run order. Run labels are matched by value,\n"
"                 not row order. Every subject must have exactly the same runs\n"
"                 and the same number of TRs in each corresponding run. Columns\n"
"                 are subject-level and constant within subject unless explicitly\n"
"                 named by -run_model or -run_factor. Nuisance/block stay constant.\n"
"                 The resulting neural RDM still has SUBJECTS as its items; runs\n"
"                 are repeated measurements, not independent observations.\n"
"                 Currently supported with '-mode IS-RSA -featuretype mean' and\n"
"                 '-null labels'. -model_dset, -seed_mask, and -noise_ceiling are\n"
"                 rejected until their run-aware estimands are defined.\n"
"\n"
"  -run_normalize N = Within-run normalization for -run_column: zscore [default],\n"
"                 demean, or none. zscore uses each ROI/run's population SD; a\n"
"                 constant time course becomes zero. Concatenation weights runs\n"
"                 by their number of TRs. -polort, when used, precedes this step.\n"
"\n"
"  -run_analysis A = How native continuous runs enter IS-RSA:\n"
"                   concatenate [default] = concatenate preprocessed runs, then\n"
"                     build one subject neural RDM (TR-weighted with zscore).\n"
"                   separate = build one neural RDM and model effect per labeled\n"
"                     run, plus an equal-run MEAN summary. FDR/max-FWE cover the\n"
"                     joint run x space family for the separate effects.\n"
"                   mean = report only the equal-run mean of the run-specific\n"
"                     model-association statistics. It is not correlation of\n"
"                     averaged RDMs and is not the concatenated estimator.\n"
"                 All runs use the same synchronized subject-label relabelings.\n"
"                 Fixed -model/-model_mat and -run_model effects are supported;\n"
"                 planned run contrasts use -run_contrast. With -model_joint,\n"
"                 every run is a standardized dyadic regression containing all\n"
"                 fixed and run-varying models. MEAN and planned contrasts then\n"
"                 combine conditional coefficients, not marginal correlations.\n"
"                 -ortvec, between-model contrasts, fitted/LOO, bootstrap,\n"
"                 model_series, and -save_rdm remain later run-model stages.\n"
"\n"
"  -run_model C:R = Build the behavioral model separately in every labeled run\n"
"                 from numeric long-table column C. R is NN or AnnaK and has the\n"
"                 same interpretation as -model. Use once per requested rule:\n"
"\n"
"                   -run_model Happiness:NN\n"
"                   -run_model Happiness:AnnaK\n"
"\n"
"                 Requires -run_column and -run_analysis separate or mean. The\n"
"                 observed effect uses Happiness for the matching run/movie. A\n"
"                 permutation moves each subject's WHOLE Happiness trajectory\n"
"                 together: one subject relabeling is shared over every run and\n"
"                 spatial location. It never shuffles scores independently by\n"
"                 run, and repeated rows are not treated as independent people.\n"
"\n"
"  -run_center C subject = Decompose a matching -run_model column into two named\n"
"                 estimands. C_state_RULE uses C_ir - mean_r(C_ir) within each\n"
"                 run; C_trait_RULE uses each subject's across-run mean and is\n"
"                 constant over runs. Both are reported, so within-person movie\n"
"                 response and stable between-person level are not conflated.\n"
"                 Complete balanced scores are required in this first contract.\n"
"\n"
"  -run_factor F = Declare a run-level design column such as Condition or Movie.\n"
"                 Its value must be identical across subjects for each labeled\n"
"                 run, though it may change between runs. May be repeated.\n"
"\n"
"  -run_contrast NAME=F:POS-NEG = A fixed-run planned contrast. For every model,\n"
"                 compute the equal-run mean association for factor level POS\n"
"                 minus the equal-run mean for NEG. For four movies, for example:\n"
"\n"
"                   -run_factor Condition                                      \\\n"
"                   -run_contrast HappyMinusSad=Condition:happy-sad\n"
"\n"
"                 Runs/movies are fixed design cells in Stage 4, not a random\n"
"                 sample of movies. The same whole-subject trajectory relabeling\n"
"                 forms every run and contrast null. With 'separate', run effects\n"
"                 and contrasts share one run/contrast x space BH/max-FWE family\n"
"                 per model; with 'mean', each model's reported contrasts share\n"
"                 contrast x space. Different behavioral models remain separate\n"
"                 planned families, matching the ordinary 3dRSA convention.\n"
"                 A subject model '-model Group:match' gives same-group similarity;\n"
"                 its HappyMinusSad result is the representational Group x\n"
"                 Condition interaction. Add '-model_joint' with a Happiness\n"
"                 -run_model to adjust that Group coefficient for Happiness in\n"
"                 each movie; the contrast is then the adjusted interaction.\n"
"                 This is fixed-movie conditional RDM regression, not a random-\n"
"                 movie mixed-effects model or Group x Happiness moderation.\n"
"\n"
"  -runwiseTable FILE = A separate input for CROSS-VALIDATED condition RDMs\n"
"                 (crossnobis), one row per subject x RUN:\n"
"\n"
"                   Subj  Run  InputFile          ResidFile\n"
"                   s01   1    s01_r1_betas+tlrc  s01_r1_errts+tlrc\n"
"                   s01   2    s01_r2_betas+tlrc  s01_r2_errts+tlrc\n"
"\n"
"                 Ordinary classic RSA estimates each condition's pattern once\n"
"                 per subject, so a distance measured and judged from the SAME\n"
"                 noisy patterns is positively biased -- conditions look\n"
"                 different even when they are not.  Cross-validation removes\n"
"                 that bias by taking each condition contrast from INDEPENDENT\n"
"                 runs, so the noise averages out.  That needs the data laid out\n"
"                 by run, which -dataTable cannot express -- hence this table.\n"
"                   ++ By default, 'InputFile' has one sub-brick per condition,\n"
"                      in a common order across all rows. 'ResidFile' is OPTIONAL:\n"
"                      the run's\n"
"                      residual time series, needed only for the whitened\n"
"                      (Mahalanobis) crossnobis distance.\n"
"                   ++ F21 unbalanced input: add a 'ConditionFile' column. Each\n"
"                      row names a text file containing one whitespace-free\n"
"                      condition label per InputFile sub-brick. Conditions may\n"
"                      be absent from a run or repeated within it; repeats are\n"
"                      averaged into that run's estimate. The model/RDM order is\n"
"                      the lexical order of all labels and is printed in output.\n"
"                      Each condition PAIR must occur together in at least two\n"
"                      independent runs per subject. Its crossnobis estimate uses\n"
"                      only those valid ordered run pairs and their own\n"
"                      denominator. Blank/comment lines are ignored.\n"
"                   ++ S6 already-estimated trial betas: instead of\n"
"                      ConditionFile, add a mutually exclusive 'TrialFile'\n"
"                      column. Each TrialFile is a two-column table in local\n"
"                      InputFile sub-brick order, for example:\n"
"\n"
"                        Trial       Condition\n"
"                        trial_001   face\n"
"                        trial_002   house\n"
"\n"
"                      Subj and Run come from the containing runwiseTable row.\n"
"                      Trial IDs must be unique within subject. Trial betas for\n"
"                      the same condition are averaged WITHIN RUN, then the\n"
"                      existing independent-run crossnobis estimator operates\n"
"                      on those run-level condition patterns. This reads betas\n"
"                      already estimated by 3dDeconvolve/3dREMLfit/3dLSS; it\n"
"                      does not fit a first-level GLM or construct a trial-by-\n"
"                      trial RDM. Extra TrialFile columns are rejected in this\n"
"                      first contract so unused descriptors cannot be silent.\n"
"                   ++ Use with '-mode RSA' and a '-model_mat' over conditions,\n"
"                      or with '-mode IS-RSA -featuretype rdm' and one or more\n"
"                      subject-by-subject '-model_mat' matrices.  The latter\n"
"                      compares subjects by their crossnobis RDM geometry.\n"
"                      A runwise table has no subject-level behavioral columns,\n"
"                      so runwise second-order IS-RSA currently uses -model_mat\n"
"                      rather than '-model COLUMN:RULE'.\n"
"                      Requires >= 2 runs per subject; all InputFiles must share\n"
"                      the grid. Without ConditionFile/TrialFile they must share the\n"
"                      condition count/order. Run labels must be unique within a\n"
"                      subject. Everything\n"
"                      downstream is the ordinary classic-RSA path: the\n"
"                      cross-validated RDM is correlated with your model(s),\n"
"                      tested across subjects by sign flip, FDR- and FWE-\n"
"                      corrected, and '-model_contrast' works as usual.\n"
"                   ++ The distances are UNBIASED, so a condition pair that does\n"
"                      not truly differ scatters around zero: NEGATIVE distances\n"
"                      are expected and are kept as-is (clipping them at zero\n"
"                      would put the bias back).\n"
"                   ++ For whitened (Mahalanobis) crossnobis, add '-noise_norm'\n"
"                      (below) and a 'ResidFile' column; without it the distance\n"
"                      is cross-validated squared Euclidean (W = I).\n"
"                   ++ Add '-searchlight NNN' to compute the same runwise\n"
"                      crossnobis analysis in every moving neighborhood.\n"
"\n"
"  -noise_norm NNN = Noise-normalize the crossnobis distance using the residuals\n"
"                 (needs '-runwiseTable' with a 'ResidFile' column).  Weighting\n"
"                 each voxel by how noisy it is -- and, for the full version,\n"
"                 decorrelating the voxels -- makes the distance a Mahalanobis\n"
"                 distance and is what most improves crossnobis reliability\n"
"                 (Walther et al. 2016).  NNN is one of:\n"
"                   none      No whitening (the default): plain cross-validated\n"
"                             squared Euclidean.\n"
"                   diag      Univariate: divide each voxel by its residual noise\n"
"                             SD (variances floored to the median, so a silent\n"
"                             voxel is not amplified).\n"
"                   shrinkage Multivariate: whiten by the FULL residual covariance\n"
"                             (Sigma^-1/2), regularized by Ledoit-Wolf shrinkage\n"
"                             toward a scaled identity with small eigenvalues\n"
"                             floored, so it stays invertible even when an ROI has\n"
"                             as many voxels as residual time points.  Recommended.\n"
"                             This still forms and eigendecomposes a dense p x p\n"
"                             covariance per subject and location.  For large\n"
"                             neighborhoods (roughly >128 voxels), benchmark first\n"
"                             and consider 'diag' or a smaller neighborhood.\n"
"                 The covariance is estimated per subject per ROI from that\n"
"                 subject's residual time series (each run demeaned first), never\n"
"                 from the condition betas being compared.\n"
"\n"
"===================================================================\n"
"MODELS -- what you are testing, and how to say it   ~1~\n"
"===================================================================\n"
"\n"
"A model is a square matrix, the same size as the neural one, saying how\n"
"similar each PAIR of items ought to be if your hypothesis is true.  3dRSA\n"
"can build one three ways: from a column of numbers, from a file, or from a\n"
"second set of datasets.\n"
"\n"
"-------------------------------------------------------------------\n"
"  -model CCC:RRR -- build a matrix from a behavioral column   ~2~\n"
"-------------------------------------------------------------------\n"
"Takes dataTable column 'CCC' and turns it into a subject-by-subject matrix\n"
"using rule 'RRR'.  May be given more than once.  Only for '-mode IS-RSA'.\n"
"\n"
"  A model column may also be a COMMA-SEPARATED list of columns, which builds\n"
"  a multivariate profile model: '-model PANAS_PA_Var,PANAS_NA_Var:euclid'.\n"
"  Each column is standardized and the subject-by-subject Euclidean distance\n"
"  over the whole profile is used -- a richer dyadic predictor than any single\n"
"  scalar (Chen et al. 2020).  The rank rules below are for single columns.\n"
"\n"
"  For a profile the rule is 'euclid' or 'mahal':\n"
"    euclid  standardized Euclidean distance -- each measure weighted equally.\n"
"            Correlated measures (say three overlapping mood scales) then count\n"
"            more than once, tilting the distance toward whatever they share.\n"
"    mahal   MAHALANOBIS distance -- whitens by the measures' covariance, so\n"
"            correlated measures are down-weighted and each contributes its\n"
"            independent part.  Reduces to 'euclid' when the measures are\n"
"            uncorrelated.  The covariance is regularized (Ledoit-Wolf shrinkage\n"
"            toward the identity, small eigenvalues floored) so it stays\n"
"            invertible even when there are as many measures as subjects; 3dRSA\n"
"            prints the shrinkage used and the effective rank.  Scale- and\n"
"            column-order-invariant.  A constant column is rejected (it has no\n"
"            variance to whiten).\n"
"\n"
"  annak    Similarity = ( rank(i) + rank(j) ) / 2n.\n"
"           The 'Anna Karenina' model, after the opening line: all happy\n"
"           families are alike, every unhappy family is unhappy in its own\n"
"           way.  Similarity depends on how HIGH the pair scores, not on\n"
"           whether they agree.  High scorers are predicted to look alike;\n"
"           low scorers are predicted to look idiosyncratic, each different\n"
"           from everyone including each other.\n"
"           ++ Use when one end of your scale is a well-defined state and the\n"
"              other is 'anything else' -- e.g. sustained attention, task\n"
"              engagement, or a symptom-free reference state.  Depression and\n"
"              anxiety scales often behave this way: healthy responses are\n"
"              stereotyped, disordered ones are heterogeneous.\n"
"\n"
"  nn       Similarity = 1 - |rank(i) - rank(j)| / max, the 'nearest\n"
"           neighbor' model.  Subjects with SIMILAR scores are predicted to\n"
"           have similar brains, no matter where on the scale they sit.\n"
"           ++ Use when the scale is a graded dimension along which responses\n"
"              vary continuously, and both extremes are equally 'real'.\n"
"\n"
"  match    Categorical similarity: 1 when the two subjects have the same\n"
"           data-table label, 0 otherwise. Labels may be text or numeric. Use\n"
"           '-model Group:match' for a categorical group-geometry hypothesis.\n"
"           A positive effect means within-group brains are more similar than\n"
"           between-group brains; it does not encode which group is higher.\n"
"\n"
"  euclid   Same shape as 'nn' but on the RAW values instead of their ranks.\n"
"           ++ Use when the units are meaningful and the spacing matters --\n"
"              when the gap between 10 and 20 really is twice the gap between\n"
"              10 and 15.  Ranking would throw that away.  It is also more\n"
"              sensitive to outliers than 'nn', for the same reason.\n"
"\n"
"  absdiff  |x(i) - x(j)| on raw values, left as a DISSIMILARITY.\n"
"           ++ Identical information to 'euclid', but with the sign flipped,\n"
"              so a positive result means 'similar scores, DISsimilar\n"
"              brains'.  Use it when that is the natural way to state your\n"
"              hypothesis; otherwise prefer 'euclid'.\n"
"\n"
"  ** annak and nn are EXACTLY orthogonal, for any number of subjects, and\n"
"     independently of your data.  'nn' depends on the rank DIFFERENCE and\n"
"     'annak' on the rank SUM, and over the complete set of pairs those two\n"
"     are uncorrelated.  So running both on one column is two genuinely\n"
"     independent tests, and there is nothing to disentangle between them --\n"
"     they need '-model_joint' no more than two unrelated variables would.\n"
"     What DOES need disentangling is two correlated behavioral scales\n"
"     (MADRS and HAM-D, say), or a behavior and a confound.\n"
"\n"
"-------------------------------------------------------------------\n"
"  -model_mat FFF -- read a matrix from a file   ~2~\n"
"-------------------------------------------------------------------\n"
"Reads a square matrix from the 1D file FFF, which must match the number of\n"
"rows of the neural matrix.  May be given more than once.\n"
"\n"
"Required for '-mode RSA', where the rows are conditions and so cannot\n"
"come from a dataTable column.  This is how the classic designs are\n"
"expressed: 1 for pairs of conditions in the same category and 0 otherwise,\n"
"a matrix of stimulus low-level similarity, a semantic distance matrix from\n"
"word embeddings, and so on.\n"
"\n"
"Also the way to bring in a model 3dRSA cannot build itself -- a matrix you\n"
"computed in R or Python, or one saved earlier by '-save_rdm'.\n"
"\n"
"-------------------------------------------------------------------\n"
"  -model_series LLL -- time-resolved model-RDM series (F20)   ~2~\n"
"-------------------------------------------------------------------\n"
"Reads an ORDERED list of per-timepoint model matrices and evaluates every\n"
"time point at every ROI/searchlight location in one run.  Each non-comment\n"
"row of LLL is:\n"
"\n"
"    Time     ModelFile\n"
"    -100ms   eeg_m100.1D\n"
"       0ms   eeg_000.1D\n"
"     100ms   eeg_p100.1D\n"
"\n"
"The header is optional.  Time labels must be unique single tokens; relative\n"
"ModelFile paths are resolved relative to LLL.  Every file is checked with\n"
"the same finite/symmetric/dimension contract as '-model_mat'.  At least two\n"
"time points are required, and input order is preserved.\n"
"\n"
"The text output is LONG form (one row per time x ROI) and preserves the\n"
"verbatim time label.  Dataset bricks use safe t0000, t0001, ... labels, with\n"
"that mapping recorded in the table.  Crucially, one synchronized relabeling\n"
"drives the complete grid: BH FDR and max-statistic FWE are corrected over the\n"
"JOINT TIME x SPACE family, not separately at each latency.\n"
"\n"
"May be used with classic RSA (condition RDMs), IS-RSA (subject matrices),\n"
"ordinary or runwise/crossnobis input, atlas ROIs, and searchlights.  It defines\n"
"the complete model set, so do not combine it with '-model', '-model_mat',\n"
"'-model_dset', or '-model_label'.  Fitted/joint/nuisance models, contrasts, commonality,\n"
"and LOO need distinct time-series statistics and are rejected for now.\n"
"\n"
"-------------------------------------------------------------------\n"
"  -model_dset CCC -- use another modality as the model   ~2~\n"
"-------------------------------------------------------------------\n"
"Takes dataTable column 'CCC', which must hold DATASET names (one per\n"
"subject, like InputFile), and builds a neural matrix from them for EACH ROI\n"
"-- then uses that as the model for the main data in that same ROI.\n"
"\n"
"This is the cross-modal case, and it is the reason RSA is a natural tool for\n"
"multimodal work.  MEG features (time by sensor) and fMRI features (voxels)\n"
"are not comparable in any direct way.  But once each is reduced to a\n"
"subject-by-subject geometry, the two live in the SAME space and can be\n"
"compared entry for entry.  So you can ask whether MEG and fMRI carry the\n"
"same representational structure in a region, and -- with '-model_joint' --\n"
"whether behavior explains the MEG geometry BEYOND what fMRI already does.\n"
"\n"
"  ** Unlike -model_mat, which is one fixed matrix for every ROI, a\n"
"     -model_dset model is rebuilt per ROI.  That is the point: fMRI in\n"
"     region k is the model for MEG in region k.\n"
"     It therefore requires source-localized data on the SAME voxel grid as\n"
"     InputFile.  Sensor-space M/EEG belongs in fixed '-model_mat' matrices or\n"
"     a time-resolved '-model_series', not in '-model_dset'.\n"
"  ** Only for '-mode IS-RSA'.  Rows must be the same subjects, in the same\n"
"     order as the dataTable.\n"
"  ** The model datasets are reduced with the same '-featuretype' and\n"
"     '-neural_metric' as the main data.  Under '-featuretype rdm', each\n"
"     modality first gets its own per-subject condition RDM using\n"
"     '-condition_metric'; the resulting subject RDM geometries are compared.\n"
"\n"
"-------------------------------------------------------------------\n"
"  -model_fit NAME=A,B,... -- fitted weighted component model (F7)   ~2~\n"
"-------------------------------------------------------------------\n"
"Fits a nonnegative weighted mixture of two or more already named model RDMs.\n"
"Name components with '-model_label', select '-metric pearson', then request,\n"
"for example, '-model_fit combined=visual,semantic'.  Components may be fixed\n"
"matrices or per-location '-model_dset' matrices, but must all be similarities\n"
"or all be distances.  The fit is deliberately constrained and nested:\n"
"  * Each subject is held out in turn.  Classic RSA learns weights from every\n"
"    other subject's condition dyads; IS-RSA learns from dyads containing only\n"
"    the other subjects.  The held subject's dyads are used only for scoring.\n"
"  * Components and training neural values are standardized inside each fold.\n"
"    Nonnegative ridge regression learns weights.  Accuracy is the mean\n"
"    held-subject Fisher-z correlation, returned as '_cvR'.\n"
"  * Every label permutation repeats the COMPLETE nested fit.  IS-RSA jointly\n"
"    relabels subjects in all components; classic RSA jointly relabels their\n"
"    condition labels.  Raw/FDR/max-FWE inference therefore includes fitting.\n"
"  * Weight columns/maps are mean fold weights normalized to sum to one.  They\n"
"    describe component allocation but are not inferential statistics.\n"
"\n"
"  -fit_ridge R       = Nonnegative ridge penalty [default 0.01], scaled by the\n"
"                       number of training dyads.  Zero gives nonnegative least\n"
"                       squares.  Applies to every -model_fit request.\n"
"\n"
"  -fit_condfold FILE = F22 strict subject x condition generalization for\n"
"                       classic RSA. FILE has one whitespace-free fold label per\n"
"                       condition, in model-matrix order; blank/comment lines\n"
"                       are ignored. Each fold must hold >=3 conditions and leave\n"
"                       >=3 for training. Within every held-subject fold, weights\n"
"                       train only on other-subject dyads whose two conditions are\n"
"                       outside the held fold, and score only held-subject dyads\n"
"                       whose two conditions are inside it. Cross-boundary dyads\n"
"                       are excluded. '_cvR' is tanh(mean Fisher z) over all valid\n"
"                       subject x condition folds; weights average those folds.\n"
"                       Every condition-label null draw repeats the complete\n"
"                       two-axis fit. With mapped -runwiseTable input, FILE follows\n"
"                       the lexical condition order printed by 3dRSA.\n"
"\n"
"Fitted-model contrasts support either F7 subject-only or F22 subject x condition\n"
"folds. Nuisance-adjusted fitting and bootstrap weight intervals remain separate\n"
"extensions. A model\n"
"series and temporal nulls are rejected rather than silently approximated.\n"
"\n"
"-------------------------------------------------------------------\n"
"  -model_joint -- test the models together instead of separately   ~2~\n"
"-------------------------------------------------------------------\n"
"By default each model is tested on its own, and you get one correlation per\n"
"model per ROI.  That is the right thing when the models are unrelated, but\n"
"it cannot tell you whether two significant models are two findings or one\n"
"finding counted twice.\n"
"\n"
"With '-model_joint' the neural triangle is regressed on ALL the model\n"
"triangles at once:\n"
"\n"
"    neural = b1*model1 + b2*model2 + ... + error\n"
"\n"
"so each coefficient is that model's contribution with the others held fixed.\n"
"All triangles are z-scored first, so the b's are standardized and comparable\n"
"(with a single model, b is exactly the ordinary correlation).\n"
"\n"
"Two things you get that you cannot get from separate tests:\n"
"\n"
"  1. SEPARATING CORRELATED MODELS.  MADRS, HAM-A and HAM-D intercorrelate\n"
"     heavily; PANAS positive- and negative-affect variance both load on\n"
"     general affective instability.  Tested separately all three may come\n"
"     back significant in the same ROI without any of them being independent.\n"
"     Regress them together and read which one carries the effect.\n"
"\n"
"  2. INTERACTIONS (in due course) and other derived structure, once your\n"
"     models are columns of one design rather than separate runs.\n"
"\n"
"To remove a CONFOUND rather than test it -- head motion, tSNR, age -- use\n"
"'-ortvec' (below), not '-model'.  A confound belongs in the design as a\n"
"regressor of no interest, not as a model you report a coefficient for.\n"
"\n"
"Significance uses the Freedman-Lane scheme: to test model j, the neural\n"
"triangle is first fit by the OTHER columns (other models and every nuisance),\n"
"and only the residual of that reduced fit is permuted before refitting.  That\n"
"tests model j specifically, rather than the joint null that every model is\n"
"zero.  The output reports the standardized b, the partial correlation, and\n"
"the permutation p for each model.\n"
"\n"
"3dRSA prints the correlations AMONG your models before it starts.  Read\n"
"them.  Two models correlated at 0.9 will split their shared effect\n"
"unpredictably between them, and neither may reach significance even when the\n"
"effect is strong -- the familiar collinearity problem, not a bug.\n"
"\n"
"-------------------------------------------------------------------\n"
"  -ortvec CCC -- remove a nuisance without testing it   ~2~\n"
"-------------------------------------------------------------------\n"
"Takes the numeric dataTable column 'CCC' -- head motion, say -- and projects\n"
"it out of the neural matrix before the models are fit.  It gets NO reported\n"
"coefficient and is never tested: a regressor of no interest, exactly like\n"
"motion in an fMRI GLM.  May be given more than once.  IS-RSA only.\n"
"\n"
"The subtlety, and why this is not just '-model Motion:something': IS-RSA\n"
"lives in PAIR space.  The neural outcome is one value per pair of subjects,\n"
"so a per-subject nuisance has to be given a per-pair form before it can be\n"
"removed -- there is no 'raw' version, because similarity is a property of a\n"
"pair, not of a subject.  (This is the one place the fMRI analogy breaks: in\n"
"a GLM everything already lives in time, so motion drops in as-is; here\n"
"nothing lives in pair space until you map it.)\n"
"\n"
"A confound can inflate neural similarity two ways, and you should not have\n"
"to guess which: subjects with SIMILAR motion share artifacts (a difference\n"
"effect), and subjects who BOTH move a lot share artifacts (a level effect).\n"
"So each -ortvec column is removed as TWO pairwise nuisances, |m_i - m_j| and\n"
"m_i + m_j, which together span any pairwise structure linear in the\n"
"covariate.  No annaK/NN shape is assumed -- that is the whole point.\n"
"\n"
"'-ortvec' works with or without '-model_joint'.  Without it, each model is\n"
"still tested on its own, but with the nuisances removed from both sides, so\n"
"the reported effect is a partial correlation.\n"
"\n"
"-------------------------------------------------------------------\n"
"  -model_contrast A-B -- does model A fit better than model B?   ~2~\n"
"-------------------------------------------------------------------\n"
"The most common follow-up question in RSA is not 'is this model significant'\n"
"but 'is THIS model better than THAT one' -- visual vs semantic structure, a\n"
"categorical vs a continuous account, and so on.  Two separate p-values cannot\n"
"answer it: both models can be significant, or the stronger one just miss, and\n"
"neither tells you whether the DIFFERENCE is reliable.  '-model_contrast A-B'\n"
"tests that difference directly.\n"
"\n"
"Name the models with '-model_label' so the contrast reads cleanly:\n"
"\n"
"    -model_label visual   -model_mat visual.1D\n"
"    -model_label semantic -model_mat semantic.1D\n"
"    -model_contrast visual-semantic\n"
"\n"
"(A and B are model names; -model_label sets the name of the NEXT model, else\n"
"the auto-generated name is used.  May be given more than once for several\n"
"contrasts.  Under IS-RSA either side may also be a per-location '-model_dset';\n"
"both modality RDMs are rebuilt in the same ROI/searchlight before comparison.)\n"
"\n"
"For a spatially varying multimodal contrast:\n"
"\n"
"    -model_label EEG  -model_dset EEGfile \\\n"
"    -model_label fMRI -model_dset fMRIfile \\\n"
"    -model_contrast EEG-fMRI\n"
"\n"
"The comparison is always paired, but PAIRING DOES NOT DETERMINE THE NULL.\n"
"Choose the scientific hypothesis explicitly:\n"
"\n"
"  -contrast_hypothesis superiority\n"
"    H0 is equal paired model performance, including when both models have\n"
"    nonzero performance. IS-RSA uses a centered paired subject bootstrap:\n"
"    every draw resamples the neural RDM and both model RDMs together, omits\n"
"    duplicate-copy diagonal dyads, and tests d*-d_obs. Classic RSA with\n"
"    '-classic_null subjects' sign-flips per-subject Fisher-z differences (or\n"
"    uses '-group_test signedrank'). For two fitted models, each outer subject\n"
"    contributes its mean A-B Fisher-z difference over folds valid for BOTH\n"
"    models; a centered paired outer-subject bootstrap tests that held-out\n"
"    performance difference and uses synchronized draws for spatial max-FWE.\n"
"\n"
"  -contrast_hypothesis alignment\n"
"    H0 is the sharp absence of alignment between the neural and model geometry.\n"
"    One shared item relabeling is applied to both models.  This is available for\n"
"    IS-RSA label/temporal nulls and classic '-classic_null conditions'.  It does\n"
"    NOT test equality of two possibly nonzero model performances.\n"
"\n"
"  -contrast_hypothesis legacy [compatibility default]\n"
"    Retains the historical mode-dependent null, labels it in output provenance,\n"
"    and warns so old scripts continue to run without silently claiming one null.\n"
"\n"
"  -classic_null NNN = Which population classic-RSA inference samples:   ~2~\n"
"    subjects   [default] = Population-level random-effects inference. Each\n"
"                 subject's Fisher-z model fit is sign-flipped; paired\n"
"                 contrasts may instead use '-group_test signedrank'. This\n"
"                 supports inference beyond the observed subject sample and\n"
"                 needs multiple independent subjects.\n"
"    conditions = Fixed-effects inference for the OBSERVED subject sample.\n"
"                 One condition relabeling is applied jointly to RDM rows and\n"
"                 columns and shared across every subject, fixed model in a\n"
"                 paired contrast, ROI/searchlight, and OpenMP worker. The\n"
"                 tested statistic is mean subject Fisher z (or the paired\n"
"                 mean Fisher-z difference), so a single-subject analysis is\n"
"                 valid -- but does NOT generalize to a population of subjects.\n"
"                 The first contract covers separately tested fixed model\n"
"                 matrices and their paired contrasts, ordinary or runwise/\n"
"                 crossnobis, including corr_cov/cosine_cov. Joint regression\n"
"                 needs a predictor-specific reduced-model condition null and\n"
"                 is rejected for now. Existing commonality/fitted-model nulls\n"
"                 keep their own documented condition-relabeling estimands.\n"
"\n"
"Classic output reports TWO effects: '_zDiff' is mean_s[atanh(rA_s)-atanh(rB_s)]\n"
"on the inferential scale, while '_rDiff' is the descriptive mean_s[rA_s-rB_s].\n"
"They are not transforms of one another. Bootstrap bounds are explicitly named\n"
"'_zDiff_bootLo/Hi' (or dualLo/Hi). IS-RSA retains '_diff'=rA-rB. Dataset maps\n"
"use rDiff/zDiff/Zstat for classic contrasts and diff/Zdiff for IS-RSA. Each\n"
"contrast has its own max-statistic FWE family.\n"
"\n"
"F14 extends the same syntax to TWO fitted models:\n"
"\n"
"    -model_fit GOOD=visual,semantic\n"
"    -model_fit BAD=visual,nuisance\n"
"    -model_contrast GOOD-BAD\n"
"\n"
"This is a paired held-out comparison, not a comparison of in-sample fits.\n"
"For every outer held-subject fold, both mixtures are learned without that\n"
"subject and scored on that same held subject.  The reported '_cvDiff' is\n"
"mean[ Fisher-z(r_GOOD,fold) - Fisher-z(r_BAD,fold) ] over paired folds.\n"
"With '-fit_condfold', each subject fold expands into the same strict held-\n"
"condition folds for both models, so the paired difference generalizes over\n"
"both axes and never uses cross-boundary condition dyads.\n"
"For alignment/legacy inference, every null draw applies the SAME subject\n"
"(IS-RSA) or condition (classic RSA) relabeling to both models and completely\n"
"refits both. For superiority, the original common held-fold fits supply paired\n"
"outer-subject effects to the centered bootstrap described above. In both cases\n"
"p/q and max-FWE are two-sided. Output columns are '_cvDiff/_cvP/_cvQ'\n"
"and optional '_cvPfwe'; maps are '_cvDiff', '_cvZdiff' (or uncalibrated\n"
"'_cvFZdiff' at -nperm 0), and '_cvZdiffFWE'.  Mixing one fixed and one fitted\n"
"model in a contrast is rejected because their estimands differ.\n"
"\n"
"-------------------------------------------------------------------\n"
"  -model_commonality A,B[,C] -- how does variance SPLIT?   ~2~\n"
"-------------------------------------------------------------------\n"
"Where the contrast asks 'which model wins', commonality analysis asks 'how do\n"
"they divide the work' -- the natural question when two accounts might each add\n"
"something, as when an EEG-derived and an fMRI-derived RDM both describe the\n"
"same conditions.  It reports the raw three-piece partition of the variance the\n"
"two models JOINTLY explain, plus two unique-effect partial R2 values:\n"
"\n"
"    uniq_A  = R2(A,B) - R2(B)      what A explains that B does not\n"
"    uniq_B  = R2(A,B) - R2(A)      what B explains that A does not\n"
"    common  = R2(A) + R2(B) - R2(A,B)     what the two share\n"
"    partialR2_A = uniq_A / (1-R2(B))       A's share of variance left by B\n"
"    partialR2_B = uniq_B / (1-R2(A))       B's share of variance left by A\n"
"\n"
"where R2(A), R2(B) are the single-model fits and R2(A,B) the joint two-model\n"
"fit (all under '-metric', ranks first for Spearman -- the same regression\n"
"'-model_joint' uses).  The three RAW terms sum to the joint R2(A,B); the two\n"
"partial terms are added effect sizes and are not part of that identity.  If a\n"
"reduced fit leaves no residual variance, its corresponding partial R2 is 0.\n"
"\n"
"  'common' CAN BE NEGATIVE.  That is not an error: it means SUPPRESSION -- the\n"
"  pair together explains MORE than the sum of their separate fits (each model\n"
"  soaks up noise the other is confused by).  3dRSA reports it unclipped.\n"
"\n"
"Name the two models (comma-separated), ideally with -model_label:\n"
"\n"
"    -model_label EEG  -model_dset EEGfile\n"
"    -model_label fMRI -model_dset fMRIfile\n"
"    -model_commonality EEG,fMRI\n"
"\n"
"F8 accepts THREE named models in the same option.  A,B,C returns the seven\n"
"exhaustive raw regions: unique A/B/C; common AB excluding C, AC excluding B,\n"
"and BC excluding A; and common ABC.  These seven (shared terms still\n"
"unclipped) sum to R2(A,B,C).  It also returns partialR2_A_given_B_C and its\n"
"B/C counterparts, so conditional effect sizes remain available just as in\n"
"the pairwise analysis.  For example:\n"
"\n"
"    -model_commonality EEG,fMRI,behavior\n"
"\n"
"Unique-effect inference is CONDITIONAL.  To test A, 3dRSA fits the reduced\n"
"neural~B model (or neural~B+C for a triple), relabels only that residual RDM,\n"
"adds it back to the fixed reduced fit, and refits the full model\n"
"(Freedman-Lane); the other unique effects are tested analogously.  Thus each\n"
"uniq/partialR2 pair tests what its model adds beyond all competitors.  Shared\n"
"regions are not added-variable effects and retain the complete neural-item\n"
"relabeling null.  The models stay fixed throughout, so\n"
"their mutual correlation is preserved.  Each quantity gets its own two-sided\n"
"permutation p and max-stat FWE family.\n"
"\n"
"For IS-RSA, items are subjects and those residual/complete RDMs are relabeled\n"
"by subject.  For classic RSA, the decomposition is first computed separately\n"
"for every subject and the group statistic is the mean component.  One shared\n"
"CONDITION permutation relabels every subject's reduced residual RDM (or full\n"
"neural RDM for 'common') in a draw, and that same draw is shared over all\n"
"ROIs/searchlights.  This avoids the invalid subject sign flip that would find\n"
"non-negative squared semipartials trivially, while preserving synchronized\n"
"spatial max-FWE.  It supports ordinary and runwise/crossnobis classic RSA;\n"
"optional subject bootstrap intervals resample the observed subject components.\n"
"\n"
"Output per request: 'uniq_A / uniq_B / common_A_B / partialR2_A /\n"
"partialR2_B', each with '_p / _q' (and '_pfwe'), plus a value + signed-z\n"
"sub-brick for each (and a '_ZFWE' map).  Repeatable, and it composes with\n"
"'-model_contrast': give both to\n"
"get 'which model wins' AND 'how the variance splits' from one run.\n"
"A triple uses explicit labels such as 'uniq_A_given_B_C',\n"
"'common_A_B_not_C', 'common_A_B_C', and 'partialR2_A_given_B_C'.\n"
"\n"
"INTERPRETATION: the raw uniq_* values remain on the joint R2 decomposition\n"
"scale, so their magnitudes shrink as the competing model approaches the\n"
"reliability ceiling.  Prefer partialR2_* when reporting the size of a unique\n"
"effect across fits with differently strong competitors.  Both forms now use\n"
"the matching reduced-model null; keep common_* on its raw decomposition scale.\n"
"\n"
"-----------------------\n"
"Similarity and testing:   ~1~\n"
"-----------------------\n"
"  -neural_metric NNN = How two feature vectors become one matrix entry:\n"
"                         corr   = Pearson correlation [default].  Equivalent\n"
"                                  to 1 minus the 'correlation distance' used\n"
"                                  by most Python RSA packages.\n"
"                         scorr  = Spearman correlation.  Robust to outliers\n"
"                                  and to monotone nonlinearity.\n"
"                         cosine = cosine similarity.  Like Pearson but\n"
"                                  without removing the mean, so overall\n"
"                                  signal level still counts.\n"
"                         euclid = Euclidean distance, a DISSIMILARITY, so it\n"
"                                  flips the sign of the result.\n"
"                       Under '-featuretype rdm', these are subject RDM-triangle\n"
"                       vectors.  The within-subject condition RDM itself is\n"
"                       controlled separately by '-condition_metric'.\n"
"\n"
"  -condition_metric NNN = With '-featuretype rdm' + '-dataTable', how condition\n"
"                       patterns become each subject's inner RDM: corr [default],\n"
"                       scorr, cosine, or euclid.  With '-runwiseTable' the inner\n"
"                       estimator is crossnobis instead, so this option is not\n"
"                       used.\n"
"\n"
"  -center_conditions CCC = Preprocess ORDINARY condition patterns before their\n"
"                       within-subject RDM is built:\n"
"                         none    = keep the raw patterns [compatibility default].\n"
"                         subject = at every ROI/searchlight, subtract that\n"
"                                   subject's voxelwise mean pattern across all\n"
"                                   conditions. This is also called cocktail-\n"
"                                   blank removal or re-meaning. It prevents a\n"
"                                   shared baseline pattern from dominating corr,\n"
"                                   scorr, or cosine condition geometry.\n"
"                       Applies to '-mode RSA -dataTable' and ordinary second-\n"
"                       order '-mode IS-RSA -featuretype rdm'. It is applied to\n"
"                       both modalities when '-model_dset' builds a second-order\n"
"                       model RDM. Euclidean distances are algebraically invariant\n"
"                       to the common subtraction and retain exact legacy values.\n"
"                       With '-runwiseTable', crossnobis condition contrasts already\n"
"                       cancel common within-run patterns; CCC=subject is rejected\n"
"                       rather than pretending to define a partition-wise policy.\n"
"\n"
"  -metric MMM        = How the two triangles are compared:\n"
"                         spearman = rank correlation [default].  The usual\n"
"                                    choice, because a model built from ranks\n"
"                                    is not linearly related to neural\n"
"                                    similarity -- only monotonically.\n"
"                         rhoa     = expected Spearman correlation under\n"
"                                    independent random ordering within ties.\n"
"                                    It equals Spearman when neither triangle\n"
"                                    has ties, but does not reward categorical\n"
"                                    models merely for predicting tied entries.\n"
"                                    Fast alternative to ktaua for tied RDMs.\n"
"                         pearson  = product-moment correlation.\n"
"                         ktaub    = Kendall's tau-b.  Much slower.\n"
"                         ktaua    = Kendall's tau-a (no tie correction).\n"
"                                    Nili et al. (2014) recommend tau-a over\n"
"                                    tau-b for MODEL RDMs with many tied\n"
"                                    entries -- e.g. a category same/different\n"
"                                    matrix, which has only two distinct\n"
"                                    values -- where tau-b's tie correction can\n"
"                                    distort model rankings.  Slowest option.\n"
"                         corr_cov = covariance-whitened Pearson RDM\n"
"                                    correlation.  Removes each triangle mean.\n"
"                         cosine_cov = covariance-whitened RDM cosine (WUC).\n"
"                                    Retains the meaningful crossnobis zero.\n"
"                       corr_cov/cosine_cov use the Diedrichsen et al.\n"
"                       zero-distance covariance V=(C C') o (C C'), exactly\n"
"                       equivalent to comparing centered second-moment matrices.\n"
"                       Their first contract requires classic RSA, balanced\n"
"                       -runwiseTable crossnobis, and fixed dissimilarity\n"
"                       -model_mat/-model_series inputs.  Subject bootstrap,\n"
"                       fixed-model contrasts, noise ceilings, atlas ROIs, and\n"
"                       searchlights are supported.  Condition-mapped/unbalanced\n"
"                       runs and condition bootstrap need an unequal-support\n"
"                       covariance and are rejected rather than misweighted.\n"
"                       rhoa is a scalar RDM comparator; it supports primary\n"
"                       effects, model contrasts, bootstrap intervals, noise\n"
"                       ceilings, LOO, atlas/searchlight inference, and temporal\n"
"                       nulls.  It is not a regression/commonality/fitted-model\n"
"                       objective; use spearman for rank-based regression.\n"
"                       '-model_joint' and '-ortvec' use least squares.  Under\n"
"                       -metric spearman the triangles are ranked first, so\n"
"                       the fit is a Spearman partial correlation and a single\n"
"                       model reproduces its ordinary Mantel result; -metric\n"
"                       pearson leaves the raw similarity values.  ktaub is\n"
"                       not available for the regression paths.\n"
"\n"
"  -nperm N           = Number of permutations [default 5000].  The identity\n"
"                       relabeling is slot 0 of the set and is counted, so the\n"
"                       smallest reachable p is 1/N; N=5000 bottoms out at\n"
"                       0.0002 -- fine for FDR over a few hundred ROIs, but for a\n"
"                       searchlight FWE column push N to 5000-10000+ (the\n"
"                       corrected p floors at 1/N as well).\n"
"                         ++ Exception: IS-RSA '-contrast_hypothesis superiority'\n"
"                            uses N random paired subject-bootstrap draws, not\n"
"                            permutations.  It uses the Monte-Carlo plus-one\n"
"                            correction, so its smallest p is 1/(N+1).\n"
"                         ++ N=0 SKIPS inference.  In '-mode RSA' it then falls\n"
"                            back to a parametric one-sample t across the (\n"
"                            independent) subjects, a legitimate z.  In '-mode\n"
"                            RSA -classic_null conditions', N=0 is rejected:\n"
"                            the fixed-effects null has no parametric replacement.\n"
"                            In '-mode\n"
"                            IS-RSA' there is no valid parametric test -- the\n"
"                            dyads are not independent -- so no p/q/FWE is\n"
"                            produced and the second sub-brick is an UNCALIBRATED\n"
"                            Fisher-z effect map ('_FZ', not FIZT-typed).  Use\n"
"                            N=0 only for a quick effect-size look.\n"
"\n"
"  -null NNN          = Null hypothesis for IS-RSA [default labels]:\n"
"                         labels    = relabel subjects in every model matrix;\n"
"                                     tests behavior-to-brain association.\n"
"                         timeshift = independently circular-shift every\n"
"                                     subject's ROI-mean time series before\n"
"                                     rebuilding the neural matrix; tests\n"
"                                     whether shared temporal alignment matters.\n"
"                         phase     = independently randomize every subject's\n"
"                                     positive-frequency Fourier phases before\n"
"                                     rebuilding the neural matrix; preserves\n"
"                                     the complete series' mean and power spectrum\n"
"                                     while destroying phase-locked alignment.\n"
"                       'timeshift' requires continuous '-featuretype mean',\n"
"                       equal-length gap-free series, and -nperm > 0.  One\n"
"                       immutable offset set is reused at every ROI/searchlight,\n"
"                       preserving thread reproducibility and max-stat FWE.\n"
"                       Per location, all subject-pair similarities are computed\n"
"                       once for every RELATIVE circular lag; each null draw is\n"
"                       then a table lookup rather than a full matrix rebuild.\n"
"                       With '-model_dset', only InputFile is shifted; the model\n"
"                       modality remains unshifted, explicitly testing whether\n"
"                       their shared temporal alignment matters.\n"
"                       Primary effects, paired fixed/per-location model\n"
"                       contrasts, joint regression, and separately fitted\n"
"                       nuisance-adjusted regression are supported.  A\n"
"                       regression coefficient remains conditional on its\n"
"                       other model/nuisance columns, but its complete-series\n"
"                       shift null destroys all cross-subject temporal alignment;\n"
"                       this is not the residual-label (Freedman-Lane) null used\n"
"                       by '-null labels'.  Commonality, LOO, and -block remain\n"
"                       rejected pending distinct time-shift contracts.\n"
"                       'phase' has the same continuous-series statistic and\n"
"                       supported effect families.  DC and the real-signal\n"
"                       Nyquist bin are retained; every other conjugate Fourier\n"
"                       pair receives an independent uniform phase by subject\n"
"                       and draw.  One stateless seeded phase family, including\n"
"                       identity slot 0, is shared across ROIs/searchlights for\n"
"                       reproducible max-FWE.  Each worker Fourier-transforms a\n"
"                       searchlight's local subject means once, reuses that local\n"
"                       spectrum for every draw, then replaces it at the next\n"
"                       center.  Model matrices remain\n"
"                       unrandomized; commonality, LOO, -block, and segmented\n"
"                       inputs remain rejected.\n"
"\n"
"  -min_shift K       = With '-null timeshift', require every random offset to\n"
"                       have circular distance at least K TRs from zero [1].\n"
"                       Choose K from the temporal autocorrelation scale.  The\n"
"                       identity remains slot 0 solely for the finite-sample\n"
"                       empirical-p correction; all other slots obey K.\n"
"\n"
"  -bootstrap N       = Draw N ordinary subject-bootstrap samples (N >= 20) and\n"
"                       report a\n"
"                       percentile confidence interval for every primary model\n"
"                       effect, model contrast, and commonality component.\n"
"                       Subjects are sampled\n"
"                       with replacement; one\n"
"                       shared resample set is used for every ROI/searchlight,\n"
"                       independently of the permutation null.\n"
"                         ++ IS-RSA: when a subject is sampled twice, the dyad\n"
"                            between those two copies is the repeated-subject\n"
"                            diagonal artifact and is omitted.  It is NOT entered\n"
"                            as a zero-distance or unit-similarity observation.\n"
"                         ++ RSA: resamples the independent subject effects; an\n"
"                            ordinary correlation is averaged in Fisher-z space.\n"
"                            A contrast resamples the paired within-subject\n"
"                            Fisher-z differences and reports tanh(mean diff).\n"
"                         ++ IS-RSA contrasts resample both model fits together\n"
"                            and bound their paired correlation difference, with\n"
"                            repeated-subject diagonal artifacts still omitted.\n"
"                         ++ IS-RSA commonality recomputes all pairwise or\n"
"                            three-predictor raw regions and conditional\n"
"                            partial-R2 effects on every compact paired draw.\n"
"                         ++ Adds '<model>_bootLo/_bootHi' and\n"
"                            '<A-B>_bootLo/_bootHi' table columns and\n"
"                            '<component>_bootLo/_bootHi' commonality columns,\n"
"                            matching plain-float dataset sub-bricks.  These are\n"
"                            uncertainty bounds, not p-values or multiplicity\n"
"                            correction.\n"
"                         ++ With '-loo', also adds '<model>_looBootLo/\n"
"                            _looBootHi'.  These resample completed held-out\n"
"                            prediction/target rows, synchronously across a\n"
"                            multivariate profile.  They quantify uncertainty\n"
"                            over the evaluated subjects with predictions held\n"
"                            fixed; they do NOT include fold-refitting or\n"
"                            training-set instability.\n"
"                         ++ IS-RSA joint/nuisance fits are refit on each compact\n"
"                            missing-dyad draw; their intervals bound the reported\n"
"                            standardized coefficient ('_b'), not partial-r.\n"
"                         ++ With '-block', subjects are resampled WITHIN each\n"
"                            block and every block keeps its observed sample size.\n"
"                            This is a stratified subject bootstrap.  It assumes\n"
"                            independence within strata; it is NOT a whole-cluster\n"
"                            bootstrap for families or sites.\n"
"\n"
"  -boot_ci P         = Percent confidence level for -bootstrap [default 95].\n"
"                       Also sets the confidence level for -cond_bootstrap.\n"
"                       With both axes it sets the corrected dual-bootstrap\n"
"                       t interval.\n"
"                       P must be greater than 0 and less than 100.\n"
"\n"
"  -cond_bootstrap N  = Draw N condition/stimulus bootstrap samples (N >= 20)\n"
"                       for classic '-mode RSA'.  The SAME sampled condition\n"
"                       indices subset every subject's neural RDM and every\n"
"                       model RDM, so their axes never lose alignment.\n"
"                         ++ Duplicate copies of one original condition create\n"
"                            a trivial off-diagonal diagonal entry; that dyad is\n"
"                            omitted rather than entered as zero/unit similarity.\n"
"                         ++ Supports ordinary and runwise/crossnobis ROI or\n"
"                            searchlight RSA, tested separately or jointly.\n"
"                         ++ Alone, adds '<model>_cbootLo/_cbootHi' table columns\n"
"                            and matching plain-float dataset sub-bricks.\n"
"                         ++ With '-bootstrap N' using the SAME N, performs the\n"
"                            F6 dual subject x condition bootstrap.  It combines\n"
"                            subject-only, condition-only, and simultaneous-draw\n"
"                            variances with the finite-sample correction, then\n"
"                            reports '<model>_dualLo/_dualHi'.  These are one\n"
"                            joint-generalization interval, not two one-axis CIs.\n"
"                         ++ Dual intervals support fixed primary models,\n"
"                            '-model_joint', fixed '-model_contrast', grouped\n"
"                            conditions, and atlas/searchlight outputs.  They do\n"
"                            not yet cover commonality, fitted models, model\n"
"                            series, or noise ceilings.\n"
"\n"
"  -cond_group FILE   = Optional grouping descriptor for -cond_bootstrap. FILE\n"
"                       has one whitespace-free label per condition, one label\n"
"                       per nonblank/noncomment line, in sub-brick/model order.\n"
"                       Conditions with the same label are sampled in or out as\n"
"                       one unit; groups may have different sizes.\n"
"\n"
"  -block CCC         = Exchangeability/bootstrap strata: dataTable column 'CCC'\n"
"                       groups subjects (by site, scanner, cohort, ...) and the\n"
"                       permutation only swaps subjects WITHIN a block, never\n"
"                       across.  Use it when subjects are not freely\n"
"                       exchangeable -- e.g. multi-site data where site is a\n"
"                       nuisance you have not otherwise modeled.  The column may\n"
"                       be text or numbers; distinct values become blocks.\n"
"                       ++ IS-RSA only.  Classic '-mode RSA' either sign-flips\n"
"                          subject effects or relabels conditions; subject blocks\n"
"                          restrict neither operation, so -block there is an\n"
"                          error, not a silent no-op.\n"
"                       ++ With '-bootstrap', subjects are sampled with replacement\n"
"                          within each block, preserving its original N.  Blocks\n"
"                          therefore mean strata, not dependent clusters.  Do not\n"
"                          use this as a whole-family or whole-site bootstrap.\n"
"\n"
"  -seed S            = Seed the random number generator, to reproduce a run.\n"
"                       Default is to seed from the clock and print the seed.\n"
"                       ++ One relabeling set is built up front and shared by\n"
"                          every ROI/voxel, so a given seed reproduces the same\n"
"                          numbers no matter how many threads the run uses.\n"
"\n"
"---------------\n"
"Output options:   ~1~\n"
"---------------\n"
"  -prefix PPP  = Write 'PPP.rsa.1D' (a text table, one row per ROI) and the\n"
"                 dataset 'PPP', in which each ROI is painted with its own\n"
"                 result: two sub-bricks per model, the effect and a z-score\n"
"                 marked as such so it thresholds in the AFNI GUI.\n"
"                 Default prefix is 'RSA'.\n"
"                 ++ Per-ROI/voxel inference gives three multiplicity handles per\n"
"                    model, from least to most stringent: the raw permutation p\n"
"                    ('_p'), a Benjamini-Hochberg FDR q over the ROIs ('_q'), and\n"
"                    a max-statistic family-wise p ('_pfwe', with signed z map\n"
"                    '_ZFWE').  See 'Family-wise error correction' below for what\n"
"                    the FWE column is and when to prefer it.\n"
"\n"
"  -noise_ceiling = Add a reliability estimate per ROI, so a weak model fit can\n"
"                 be read as 'wrong model' vs 'noisy region'.\n"
"                   IS-RSA mean features: splits the ROI-mean time course,\n"
"                     builds the subject matrix from each half, and correlates\n"
"                     them -- does this region's subject geometry replicate?\n"
"                     Reported as one 'reliability' column.\n"
"                     '-featuretype pattern' is rejected: its flattened\n"
"                     [sub-brick][voxel] vector contains no matched repetitions,\n"
"                     so splitting it would confound condition and/or voxel\n"
"                     identity with reliability.\n"
"                   RSA: the Nili et al. (2014) noise ceiling -- each subject's\n"
"                     condition RDM vs the group-mean RDM (upper bound,\n"
"                     'nc_high') and vs the leave-one-subject-out mean of the\n"
"                     others (lower bound, 'nc_low').  A model reaching nc_low\n"
"                     is doing as well as the data reliability allows.\n"
"                     This applies to ordinary condition-pattern RDMs and to\n"
"                     '-runwiseTable' crossnobis RDMs.  In the runwise case,\n"
"                     independent runs define every subject RDM before the\n"
"                     subject-level ceiling is calculated; residual whitening,\n"
"                     if requested, is applied before crossnobis.  The fixed\n"
"                     model has no fitted condition parameters, so no condition\n"
"                     data enter a fit.  'nc_low' excludes the evaluated subject;\n"
"                     'nc_high' includes it by the conventional upper-bound\n"
"                     definition and is intentionally optimistic.\n"
"                   ++ These also go into the output dataset as sub-bricks\n"
"                      ('reliability', or 'nc_low'/'nc_high'), so under\n"
"                      -searchlight you get a whole-brain reliability map to read\n"
"                      alongside the effect: a null effect where reliability is\n"
"                      high is a real miss, where it is low is just noise.\n"
"\n"
"  -nc_split SSS = How to split continuous IS-RSA mean features for reliability\n"
"                 [default 'half']:\n"
"                   half       = first half of the time course vs the second.\n"
"                                Temporally independent, so not inflated by\n"
"                                autocorrelation, but the halves differ in\n"
"                                stimulus content -- a conservative estimate.\n"
"                   interleave = even samples vs odd samples.  Content matched,\n"
"                                but adjacent-sample correlation inflates it\n"
"                                (more so for fast-sampled data like MEG).\n"
"                 The two carry opposite biases; report which you used.\n"
"\n"
"  -loo         = Leave-one-subject-out prediction (IS-RSA only).  Holds out\n"
"                 each subject, predicts behavior without using that subject's\n"
"                 target, then reports cross-validated accuracy with a subject-\n"
"                 label permutation p.  The predictor follows the model shape:\n"
"                   nn/euclid/absdiff scalar = neural-neighbor rank-weighted\n"
"                         mean of the training subjects' behavior (nearer gets\n"
"                         more weight; neural distances are negated first).\n"
"                   annak scalar = training-only neural typicality regression.\n"
"                         In each fold, the remaining subjects' behavior is fit\n"
"                         from their mean neural closeness to one another; the\n"
"                         held behavior is predicted from its mean closeness to\n"
"                         that training set.  This tests the AnnaK joint-level /\n"
"                         idiosyncrasy hypothesis, not nearest-neighbor proximity.\n"
"                   multivariate COLUMN1,COLUMN2,...:euclid|mahal = the same\n"
"                         outcome-blind neural weights predict every held measure.\n"
"                         looR is the equal-weight mean of the measure-wise\n"
"                         predicted-vs-true correlations.  Null permutations move\n"
"                         complete subject profiles, preserving their covariance.\n"
"                 Because a subject's own behavior never enters their own\n"
"                 prediction, a positive result means the neural geometry\n"
"                 genuinely carries behavior, not that a model was fit to it.\n"
"                   ++ Uses data-table '-model' targets; model_mat and model_dset\n"
"                      are skipped because they contain no held subject outcome.\n"
"                      Exact duplicate targets and estimands keep their own labels\n"
"                      but share one computation/max-FWE family.  AnnaK and NN\n"
"                      models of the same column are distinct estimands/families.\n"
"                   ++ Adds '..._looR/_looP/_looQ' to the table and '_looR/_looZ'\n"
"                      sub-bricks to the dataset, so it maps under -searchlight.\n"
"                   ++ Under -nperm the prediction also gets its OWN max-statistic\n"
"                      FWE ('_looPfwe' column, '_looZFWE' sub-brick).  It is a\n"
"                      separate family from the Mantel/regression statistic --\n"
"                      cross-validated accuracy is a different quantity, on a\n"
"                      different scale -- so it earns its own max-null rather than\n"
"                      borrowing the effect's.  See below.\n"
"                   ++ This is not the leave-one-subject-out lower noise ceiling\n"
"                      for classic/runwise RSA.  Classic RSA has no subject-level\n"
"                      target to predict; use '-noise_ceiling' and read\n"
"                      'nc_low' for that runwise LOO reliability bound.\n"
"\n"
"  -no_dset     = Write only the text table.\n"
"\n"
"  -save_rdm QQQ= Also write the matrices themselves, as 'QQQ_model_<name>.1D'\n"
"                 and 'QQQ_roi<value>.1D'.  Worth doing once on any new\n"
"                 analysis, to see that your models look the way you think.\n"
"                 These files can be fed straight back in via '-model_mat'.\n"
"                 Fixed models get one model file; a '-model_dset' varies by\n"
"                 ROI, so it has no single model file (neural ROI files are\n"
"                 still written).\n"
"\n"
"  -quiet       = Suppress the progress and diagnostic messages.\n"
"  -progress M  = Progress display: auto, bar, line, or off [auto].\n"
"                 'auto' uses a one-line updating bar on a terminal and 10%%\n"
"                 milestone lines when stderr is redirected.  Progress counts\n"
"                 completed ROIs/searchlights and reports throughput, elapsed\n"
"                 time, and an approximate ETA. '-quiet' overrides this option.\n"
"\n"
"------\n"
"Notes:   ~1~\n"
"------\n"
"* On the SIGN of a result.  The 'corr', 'scorr' and 'cosine' neural metrics\n"
"   and the 'annak', 'nn' and 'euclid' model rules all produce SIMILARITIES,\n"
"   so a positive result means 'more similar behavior goes with more similar\n"
"   brain response'.  The 'euclid' neural metric and the 'absdiff' rule\n"
"   produce DISSIMILARITIES; mixing one of those with a similarity flips the\n"
"   sign.  3dRSA prints the sense of each model and will not stop you.\n"
"\n"
"* q-values are Benjamini-Hochberg FDR across the ROIs, computed separately\n"
"   for each model.  FDR controls the expected FRACTION of false positives\n"
"   among the ROIs you call significant -- a good default for an atlas, where\n"
"   you want a trustworthy list of regions and can tolerate a known small\n"
"   share of them being wrong.\n"
"\n"
"* Family-wise error correction (the '_pfwe' column, '_ZFWE' sub-brick).   ~2~\n"
"   For a searchlight -- thousands of overlapping, spatially correlated spheres\n"
"   -- FDR is often not what you want; there you usually want to control the\n"
"   probability of ANY false positive anywhere in the map.  That is family-wise\n"
"   error (FWE), and 3dRSA gets it by the max-statistic permutation method of\n"
"   Nichols & Holmes (2002), essentially for free from the machinery the p\n"
"   already needs:\n"
"     - One relabeling set is drawn up front and SHARED by every ROI/voxel\n"
"       (that is also why a given -seed reproduces a run regardless of thread\n"
"       count).  Call relabeling k applied everywhere the k-th null realization\n"
"       of the WHOLE map.\n"
"     - For each k, 3dRSA records the single largest |statistic| attained by any\n"
"       element under that relabeling.  Those maxima, over all k, are the null\n"
"       distribution of the map maximum.\n"
"     - An element's FWE p is where its observed statistic falls in that\n"
"       max-null: p_fwe = (# relabelings whose map-max >= |observed|) / nperm.\n"
"       Thresholding every element at p_fwe <= 0.05 holds the chance of even one\n"
"       false positive ACROSS THE WHOLE MAP at 0.05.\n"
"       For an IS-RSA fixed-RDM or fitted-model superiority contrast, replace\n"
"       'relabeling statistic' with\n"
"       the centered paired-bootstrap value |d*_k-d_obs|.  Its raw and max-FWE\n"
"       tails use (1 + exceedances)/(1 + N), because no identity draw exists.\n"
"   Because the max-null already carries the spatial correlation of the data\n"
"   (neighboring searchlights move together under a relabeling), this is exact\n"
"   and adaptive -- no Gaussian-field assumption, no smoothness estimate, and\n"
"   never anticonservative the way a Bonferroni count of non-independent\n"
"   searchlights would be.  It is by construction at least as strict as the raw\n"
"   p, so '_pfwe' >= '_p' always.\n"
"     ++ Reported per MODEL, not pooled across models: the max is taken over\n"
"        space for one model at a time (as in FSL randomise / PALM for a single\n"
"        contrast).  If you additionally want to correct across models, treat\n"
"        the several '_pfwe' as their own small family.\n"
"     ++ The max-null resolution floors p_fwe at 1/nperm, so for a searchlight\n"
"        use enough permutations (5000+); the smallest reportable corrected p is\n"
"        1/nperm.\n"
"     ++ Two independent statistics, two independent max-nulls: the Mantel/\n"
"        regression effect ('_pfwe') and, with -loo, the cross-validated\n"
"        prediction ('_looPfwe').  Each is corrected within its own family.\n"
"     ++ The centered superiority bootstrap is a Monte-Carlo approximation,\n"
"        not an exact permutation test; its family-wise construction is still\n"
"        synchronized over space and thread-count reproducible.\n"
"     ++ Computed whenever -nperm > 0.  For a handful of atlas ROIs it is valid\n"
"        but blunt (a max over a few regions is a weak correction); FDR '_q' is\n"
"        usually the better read there.  FWE earns its keep on the searchlight.\n"
"\n"
"* With few items the permutation test has little resolution: 8 subjects\n"
"   allow only 8! = 40320 distinct relabelings.  Below 6 items 3dRSA refuses\n"
"   to run.\n"
"\n"
"* Numerical input and migration contract.  Every numeric option token is\n"
"   consumed in full and must be finite and in range; strings such as '20x',\n"
"   'nan', and 'inf' are errors. Used numeric data-table columns, positive atlas\n"
"   labels, and every data brick/voxel that belongs to a requested ROI, seed, or\n"
"   searchlight domain must be finite. Positive atlas labels must be integers.\n"
"   Values outside the union analysis domain are intentionally ignored because\n"
"   they cannot enter an RDM. This is stricter than early development versions,\n"
"   which could accept trailing option text or propagate a NaN into inference;\n"
"   repair those inputs rather than relying on the old accidental behavior.\n"
"\n"
"* Parallelized with OpenMP over ROIs.  On 16 cores a 200-ROI atlas with 2\n"
"   models and 5000 permutations takes under a second, against about 8\n"
"   seconds on one core.  Since the parallelism is over ROIs, an atlas with\n"
"   only a few will not speed up much.  Set OMP_NUM_THREADS to control it.\n"
"\n"
"--------\n"
"Examples:   ~1~\n"
"--------\n"
"1. IS-RSA on story listening, asking whether subjects with similar mood\n"
"   variance respond similarly, under both model shapes:\n"
"\n"
"     3dRSA -mask Schaefer_200+tlrc  -mode IS-RSA          \\\n"
"           -dataTableFile mood.txt                         \\\n"
"           -model PANAS_Var:annak  -model PANAS_Var:nn     \\\n"
"           -nperm 5000  -prefix rsa_theta\n"
"\n"
"2. The same, but removing head motion as a nuisance and testing two\n"
"   correlated clinical scales against each other:\n"
"\n"
"     3dRSA -mask Schaefer_200+tlrc  -mode IS-RSA          \\\n"
"           -dataTableFile mood.txt  -model_joint           \\\n"
"           -model PANAS_Var:nn  -model MADRS:nn            \\\n"
"           -ortvec MeanFD                                  \\\n"
"           -nperm 5000  -prefix rsa_ctl\n"
"\n"
"   where mood.txt reads\n"
"\n"
"     Subj  PANAS_Var  MADRS  MeanFD  InputFile\n"
"     s01   3.4        22     0.11    s01.theta+tlrc\n"
"     s02   1.1        14     0.28    s02.theta+tlrc\n"
"     ...\n"
"\n"
"3. Cross-modal: does mood explain the MEG geometry beyond what fMRI does?\n"
"\n"
"     3dRSA -mask Schaefer_200+tlrc  -mode IS-RSA          \\\n"
"           -dataTableFile both.txt  -model_joint           \\\n"
"           -model PANAS_Var:nn  -model_dset fMRIFile       \\\n"
"           -nperm 5000  -prefix rsa_cross\n"
"\n"
"   where both.txt has an fMRIFile column of datasets beside InputFile.\n"
"\n"
"4. Classic (traditional within-subject) RSA. Suppose every subject saw the\n"
"   same three conditions -- houses, faces, and trees -- and each subject's\n"
"   first-level dataset contains one beta sub-brick per condition. The data\n"
"   table has ONE ROW PER SUBJECT, not one row per condition:\n"
"\n"
"     Subj  InputFile\n"
"     s01   s01_condition_betas+tlrc\n"
"     s02   s02_condition_betas+tlrc\n"
"     s03   s03_condition_betas+tlrc\n"
"     ...\n"
"\n"
"   In the compact form, every InputFile must contain the same conditions in\n"
"   exactly the same sub-brick order:\n"
"\n"
"     brick 0 = houses\n"
"     brick 1 = faces\n"
"     brick 2 = trees\n"
"\n"
"   In classic RSA the RDM rows are CONDITIONS. Therefore a condition-level\n"
"   behavior hypothesis is supplied as a condition-by-condition -model_mat;\n"
"   it is not a subject column in subs.txt. For example, suppose independent\n"
"   ratings predict happiness 2 for houses, 5 for faces, and 4 for trees. A\n"
"   similarity model (larger = closer predicted happiness), in the SAME order\n"
"   as the beta sub-bricks, could be the numeric file happiness.1D:\n"
"\n"
"     # row/column order: houses faces trees\n"
"     1.000000  0.000000  0.333333\n"
"     0.000000  1.000000  0.666667\n"
"     0.333333  0.666667  1.000000\n"
"\n"
"   (The diagonal is ignored.) 3dRSA builds one neural condition RDM per\n"
"   subject and ROI, correlates each with this fixed happiness model, then\n"
"   performs population inference across subjects:\n"
"\n"
"     3dRSA -mask atlas+tlrc  -mode RSA                         \\\n"
"           -dataTableFile subs.txt                              \\\n"
"           -model_label happiness -model_mat happiness.1D       \\\n"
"           -neural_metric corr -metric spearman                 \\\n"
"           -nperm 5000 -prefix rsa_happiness\n"
"\n"
"   Alternatively, put one selected brick per condition in an arbitrarily\n"
"   ordered long table, subs_by_condition.txt:\n"
"\n"
"     Subj  cond    InputFile\n"
"     s01   tree   s01_condition_betas+tlrc[tree]\n"
"     s01   house  s01_condition_betas+tlrc[house]\n"
"     s01   face   s01_condition_betas+tlrc[face]\n"
"     ...\n"
"\n"
"   Then bind labels to the row/column order of happiness.1D explicitly:\n"
"\n"
"     3dRSA -mask atlas+tlrc -mode RSA                         \\\n"
"           -dataTableFile subs_by_condition.txt                \\\n"
"           -condition_column cond                              \\\n"
"           -condition_order house,face,tree                    \\\n"
"           -model_label happiness -model_mat happiness.1D      \\\n"
"           -nperm 5000 -prefix rsa_happiness_long\n"
"\n"
"   The two input forms are numerically equivalent. The long-table rows need\n"
"   not follow subject or condition order; -condition_order describes only\n"
"   the fixed model matrix.\n"
"\n"
"   Three conditions make the axes easy to see but are too few for a useful\n"
"   real RSA; use many independently defined conditions/stimuli in practice.\n"
"\n"
"5. Second-order task-fMRI IS-RSA: compare subjects by their condition RDMs,\n"
"   avoiding voxelwise correspondence across subjects:\n"
"\n"
"     3dRSA -mask brain+tlrc -mode IS-RSA -featuretype rdm       \\\n"
"           -dataTableFile task.txt -condition_metric corr        \\\n"
"           -model symptoms:nn -searchlight 'SPHERE(6)'           \\\n"
"           -nperm 10000 -prefix rsa_second_order\n"
"\n"
"   For crossnobis inner RDMs, replace -dataTableFile with -runwiseTable,\n"
"   and supply a subject-by-subject -model_mat.\n"
"\n"
"6. Time-resolved M/EEG-fMRI fusion with one correction over time x space:\n"
"\n"
"     3dRSA -mask brain+tlrc -mode RSA -dataTableFile fmri_conditions.txt \\\n"
"           -model_series eeg_rdm_series.txt -searchlight 'SPHERE(6)'      \\\n"
"           -metric spearman -nperm 10000 -prefix eeg_fmri_fusion\n"
"\n"
"7. Searchlight with family-wise correction.  Threshold the '_ZFWE' sub-brick\n"
"   in the GUI (or the '_pfwe' column) at p <= 0.05 to control one false\n"
"   positive over the whole map:\n"
"\n"
"     3dRSA -mask brain+tlrc  -mode IS-RSA  -searchlight 'SPHERE(6)' \\\n"
"           -dataTableFile mood.txt  -model PANAS_Var:nn            \\\n"
"           -loo  -nperm 10000  -prefix rsa_sl\n"
"\n"
"8. Does a visual model fit the patterns better than a semantic one?  A paired\n"
"   contrast (classic within-subject RSA):\n"
"\n"
"     3dRSA -mask atlas+tlrc  -mode RSA  -dataTableFile subs.txt   \\\n"
"           -model_label visual   -model_mat visual.1D             \\\n"
"           -model_label semantic -model_mat semantic.1D           \\\n"
"           -model_contrast visual-semantic                        \\\n"
"           -nperm 5000  -prefix rsa_vs\n"
"\n"
"9. EEG/fMRI fusion: how much of the subject geometry do the two modalities\n"
"   share, and how much does each carry alone?  (Both modalities enter as\n"
"   per-ROI models; the neural side is the behavioral target.)\n"
"\n"
"     3dRSA -mask brain+tlrc  -mode IS-RSA  -dataTableFile fuse.txt \\\n"
"           -model_label EEG  -model_dset EEGrdmFile                \\\n"
"           -model_label fMRI -model_dset fMRIrdmFile               \\\n"
"           -model_commonality EEG,fMRI  -model_contrast EEG-fMRI   \\\n"
"           -nperm 5000  -prefix rsa_fuse\n"
"\n"
"10. Does a continuous naturalistic response depend on subjects sharing the\n"
"   same timeline?  Shift every complete ROI-mean series by at least 10 TRs:\n"
"\n"
"     3dRSA -mask atlas+tlrc  -mode IS-RSA  -featuretype mean       \\\n"
"           -dataTableFile story.txt  -model engagement:nn          \\\n"
"           -null timeshift  -min_shift 10  -nperm 5000             \\\n"
"           -seed 314159  -prefix rsa_story_shift\n"
"\n"
"* https://en.wikipedia.org/wiki/Representational_similarity_analysis\n"
"* https://en.wikipedia.org/wiki/Mantel_test\n"
"* Kriegeskorte, Mur & Bandettini (2008), Front Syst Neurosci 2:4.\n"
"* Finn et al. (2020), NeuroImage 215:116828  [IS-RSA and the AnnaK model]\n"
"* Freedman & Lane (1983), J Bus Econ Stat 1:292  [the permutation scheme]\n"
"* Nichols & Holmes (2002), Hum Brain Mapp 15:1  [max-statistic FWE]\n"
"* Nimon & Oswald (2013), Organ Res Methods 16:650  [commonality analysis]\n"
"* Kauppi et al. (2010), Front Neuroinform 4:5  [circular time shifts]\n"
"* Schuett et al. (2023), eLife 12:e82566  [expected Spearman rho-a]\n"
"\n"
"-- P Molfese, Jul 2026\n"
   ) ;
   PRINT_AFNI_OMP_USAGE(PROGRAM_NAME,NULL);
   PRINT_COMPILE_DATE ;
   return ;
}

#if defined(__GNUC__)
# pragma GCC diagnostic pop
#endif

/*============================================================================*/

/*! Benjamini-Hochberg q-values from p-values.  q[] may not alias p[].

    This deliberately does NOT use AFNI's mri_fdrize().  That routine takes
    parametric statistics plus a statcode, converts to FDR-ized z-scores, and
    applies AFNI-specific adjustments (see AFNI_DONT_ADJUST_FDR and the
    missed-detection-fraction curve).  The p-values here are permutation
    derived, where plain BH is the honest transform. */

/*! BH correction over one time x space family.  The ordinary model interface
    keeps one FDR family per model; -model_series instead declares the complete
    time x location grid as the searched family, matching its max-FWE contract. */

static void bh_fdr_series( int ntime , int nroi , float **p , float **q )
{
   float *pv , *qv ; int tt , kk , n=ntime*nroi ;
   if( n < 1 ) return ;
   pv=(float *)malloc(sizeof(float)*(size_t)n) ;
   qv=(float *)malloc(sizeof(float)*(size_t)n) ;
   for( tt=0 ; tt < ntime ; tt++ )
     for( kk=0 ; kk < nroi ; kk++ ) pv[(size_t)tt*nroi+kk]=p[tt][kk] ;
   THD_bh_fdr(n,pv,qv) ;
   for( tt=0 ; tt < ntime ; tt++ )
     for( kk=0 ; kk < nroi ; kk++ ) q[tt][kk]=qv[(size_t)tt*nroi+kk] ;
   free(pv) ; free(qv) ;
}

/*! Read an ordered time series of fixed model matrices.  Each non-comment row
    is "TIME_LABEL MATRIX_FILE"; an optional "Time ModelFile" header is
    accepted.  Relative matrix paths are resolved beside the list file, which
    makes a series portable across working directories.  Safe t#### model names
    are generated separately from the verbatim time labels used in provenance. */

static void rsa_read_model_series( char *fname , char ***files_out ,
                                   char ***names_out , char ***times_out ,
                                   int *n_out )
{
   FILE *fp ; char line[8192] , lab[512] , ent[4096] , extra[2] ;
   char **files=NULL , **names=NULL , **times=NULL ; int n=0 , lno=0 ;
   char dir[THD_MAX_NAME] ; char *slash ;

   fp=fopen(fname,"r") ;
   if( fp == NULL ) ERROR_exit("3dRSA: can't open -model_series file '%s'",fname) ;
   strncpy(dir,fname,sizeof(dir)-1) ; dir[sizeof(dir)-1]='\0' ;
   slash=strrchr(dir,'/') ;
   if( slash != NULL ) *slash='\0' ; else strcpy(dir,".") ;

   while( fgets(line,sizeof(line),fp) != NULL ){
     char *s=line , *hash ; int got , ii ; char *path , *name ; size_t need ;
     lno++ ;
     if( strchr(line,'\n') == NULL && !feof(fp) )
       ERROR_exit("3dRSA: -model_series '%s' line %d is too long",fname,lno) ;
     while( isspace((unsigned char)*s) ) s++ ;
     if( *s=='\0' || *s=='#' ) continue ;
     hash=strchr(s,'#') ; if( hash != NULL ) *hash='\0' ;
     got=sscanf(s,"%511s %4095s %1s",lab,ent,extra) ;
     if( got < 2 )
       ERROR_exit("3dRSA: -model_series '%s' line %d needs TIME_LABEL and MATRIX_FILE",
                  fname,lno) ;
     if( got > 2 )
       ERROR_exit("3dRSA: -model_series '%s' line %d has extra text; time labels\n"
                  "       and matrix paths must each be one token",fname,lno) ;
     if( n==0 && strcasecmp(lab,"Time")==0 &&
         (strcasecmp(ent,"ModelFile")==0 || strcasecmp(ent,"MatrixFile")==0) )
       continue ;
     for( ii=0 ; ii < n ; ii++ ) if( strcmp(times[ii],lab)==0 )
       ERROR_exit("3dRSA: -model_series '%s' repeats time label '%s'",fname,lab) ;

     if( ent[0]=='/' ) path=strdup(ent) ;
     else {
       need=strlen(dir)+strlen(ent)+2 ;
       if( need >= THD_MAX_NAME )
         ERROR_exit("3dRSA: -model_series matrix path is too long at line %d",lno) ;
       path=(char *)malloc(need) ; snprintf(path,need,"%s/%s",dir,ent) ;
     }
     name=(char *)malloc(16) ; snprintf(name,16,"t%04d",n) ;
     files=(char **)realloc(files,sizeof(char *)*(n+1)) ;
     names=(char **)realloc(names,sizeof(char *)*(n+1)) ;
     times=(char **)realloc(times,sizeof(char *)*(n+1)) ;
     files[n]=path ; names[n]=name ; times[n]=strdup(lab) ; n++ ;
   }
   fclose(fp) ;
   if( n < 2 )
     ERROR_exit("3dRSA: -model_series '%s' has %d time point%s; need at least 2",
                fname,n,(n==1)?"":"s") ;
   *files_out=files ; *names_out=names ; *times_out=times ; *n_out=n ;
}

/*============================================================================*/

/*! Remove every target ROI/searchlight that shares at least one voxel/node
    with the fixed seed.  Representational connectivity from a region to a
    target containing the same measured features has a deterministic shared-
    noise component and is not the seed-to-other-location estimand.  Pruning
    before result/null allocation also keeps the BH and max-FWE families equal
    to the locations that were actually searched.  Returns the number removed. */

static int rsa_roilist_exclude_seed( THD_roilist *rl , intvec *seed , int nvox )
{
   byte *inseed ; int kk,ii,out=0,nold,nex=0 ;
   if( rl == NULL || seed == NULL || seed->nar < 1 || nvox < 1 ) return 0 ;
   inseed=(byte *)calloc(nvox,sizeof(byte)) ;
   for( ii=0 ; ii<seed->nar ; ii++ )
     if( seed->ar[ii]>=0 && seed->ar[ii]<nvox ) inseed[seed->ar[ii]]=1 ;
   nold=rl->nroi ;
   for( kk=0 ; kk<nold ; kk++ ){
     int overlap=0 ;
     for( ii=0 ; ii<rl->vox[kk].nar ; ii++ )
       if( inseed[rl->vox[kk].ar[ii]] ){ overlap=1 ; break ; }
     if( overlap ){
       free(rl->vox[kk].ar) ; rl->vox[kk].ar=NULL ;
       free(rl->lab[kk]) ; rl->lab[kk]=NULL ; nex++ ;
       continue ;
     }
     if( out != kk ){
       rl->val[out]=rl->val[kk] ; rl->vox[out]=rl->vox[kk] ;
       rl->lab[out]=rl->lab[kk] ; rl->center[out]=rl->center[kk] ;
     }
     out++ ;
   }
   rl->nroi=out ; free(inseed) ;
   return nex ;
}

/*! Remove the voxelwise mean pattern over conditions in place.  F is laid out
    [condition][voxel].  This is the subject-wise cocktail-blank/re-meaning
    operation used before ordinary angle-based condition RDMs. */

static void rsa_center_condition_patterns( int ncond , int nvx , float *F )
{
   int c , v ;
   for( v=0 ; v < nvx ; v++ ){
     double mean=0.0 ;
     for( c=0 ; c < ncond ; c++ ) mean += F[(size_t)c*nvx+v] ;
     mean /= (double)ncond ;
     for( c=0 ; c < ncond ; c++ ) F[(size_t)c*nvx+v] -= (float)mean ;
   }
}

/*! Extract one subject's condition patterns in the declared model-matrix
    order.  Ordinary input stores all conditions as bricks in dset[jj].  A
    condition-indexed long table stores one selected brick per canonical
    subject/condition cell in a flattened dset array. */
static void rsa_subject_patterns( int jj, int ncond, int nvx,
                                  THD_3dim_dataset **dset,
                                  THD_datatable_index *condition_index,
                                  intvec *vox, float *out )
{
   int cc ;
   if( condition_index==NULL ){
     THD_roi_pattern(dset[jj],vox,out) ; return ;
   }
   for( cc=0 ; cc<ncond ; cc++ )
     THD_roi_pattern(dset[jj*ncond+cc],vox,out+(size_t)cc*nvx) ;
}

/*! Build subject jj's neural condition RDM for ROI kk.  With a runset this is
    the cross-validated (crossnobis) distance from that subject's runs; otherwise
    it is the ordinary similarity matrix from this subject's condition patterns.
    F is [nitem*nvx] scratch for the plain path; rpat[r] is [nitem*nvx] scratch
    per run for the crossnobis path.  runraw holds the largest local mapped run
    before repeated labels are averaged into canonical condition order. */

static THD_simmat * rsa_subject_rdm( int jj , THD_roilist *rl , int kk ,
                                     int nitem , int nvx , int neu_metric ,
                                     THD_3dim_dataset **dset , THD_runset *runset ,
                                     THD_datatable_index *condition_index ,
                                     int center_conditions ,
                                     float *F , float **rpat , float *runraw,
                                     RSA_whiten *wh )
{
   int nr , r , c , v , t , u ;

   if( runset == NULL ){
     rsa_subject_patterns(jj,nitem,nvx,dset,condition_index,rl->vox+kk,F) ;
     /* Euclidean differences cancel a common mean pattern algebraically. Skip
        the subtraction there to retain exact compatibility rather than add
        harmless floating-point roundoff. */
     if( center_conditions && neu_metric != SIM_EUCLID )
       rsa_center_condition_patterns(nitem,nvx,F) ;
     return THD_simmat_from_features( nitem , nvx , F , neu_metric ) ;
   }

   nr = runset->nrun[jj] ;
   for( r=0 ; r < nr ; r++ ){
     int row=runset->row_of[jj][r] ;
     if( !runset->has_condmap )
       THD_roi_pattern(runset->betas[row],rl->vox+kk,rpat[r]) ;
     else {
       int b ;
       THD_roi_pattern(runset->betas[row],rl->vox+kk,runraw) ;
       memset(rpat[r],0,sizeof(float)*(size_t)nitem*nvx) ;
       for( b=0 ; b<runset->nbrick[row] ; b++ ){
         int gc=runset->cond_of[row][b] ; float *dst=rpat[r]+(size_t)gc*nvx ;
         float *src=runraw+(size_t)b*nvx ;
         for( v=0 ; v<nvx ; v++ ) dst[v]+=src[v] ;
       }
       for( c=0 ; c<nitem ; c++ ) if( runset->nrep[row][c]>1 ){
         float fac=1.0f/(float)runset->nrep[row][c] ;
         for( v=0 ; v<nvx ; v++ ) rpat[r][(size_t)c*nvx+v]*=fac ;
       }
     }
   }

   /* multivariate/univariate noise normalization: estimate the noise covariance
      from THIS subject's residuals in THIS ROI (never from the betas being
      compared), then whiten every run's condition patterns before crossnobis. */
   if( wh != NULL && wh->mode != NN_NONE ){
     int ntot = 0 ;
     for( r=0 ; r < nr ; r++ ){
       int row = runset->row_of[jj][r] , nt = DSET_NVALS(runset->resid[row]) ;
       THD_roi_pattern( runset->resid[row] , rl->vox+kk , wh->residbuf ) ;  /* [nt*nvx] */
       for( v=0 ; v < nvx ; v++ ){        /* demean this run, per voxel */
         double m=0.0 ;
         for( t=0 ; t < nt ; t++ ) m += wh->residbuf[(size_t)t*nvx+v] ;
         m /= nt ;
         for( t=0 ; t < nt ; t++ )
           wh->Rmat[(size_t)(ntot+t)*nvx+v] = wh->residbuf[(size_t)t*nvx+v] - (float)m ;
       }
       ntot += nt ;
     }
     if( wh->mode == NN_DIAG ){
       THD_noise_wdiag( nvx , ntot , wh->Rmat , wh->wdiag ) ;
       for( r=0 ; r < nr ; r++ )
         for( c=0 ; c < nitem ; c++ )
           for( v=0 ; v < nvx ; v++ ) rpat[r][(size_t)c*nvx+v] *= wh->wdiag[v] ;
     } else {
       float shrink ; int erank ;
       THD_noise_whalf( nvx , ntot , wh->Rmat , wh->Whalf , &shrink , &erank ) ;
       for( r=0 ; r < nr ; r++ )
         for( c=0 ; c < nitem ; c++ ){
           float *b = rpat[r] + (size_t)c*nvx ;
           for( v=0 ; v < nvx ; v++ ){
             double s=0.0 ;
             for( u=0 ; u < nvx ; u++ ) s += wh->Whalf[(size_t)v*nvx+u]*b[u] ;
             wh->wtmp[v] = (float)s ;
           }
           memcpy( b , wh->wtmp , sizeof(float)*nvx ) ;
         }
     }
   }

   if( runset->has_condmap ){
     int *rep[nr] ;
     for( r=0 ; r<nr ; r++ ) rep[r]=runset->nrep[runset->row_of[jj][r]] ;
     return THD_simmat_crossnobis_valid(nitem,nr,nvx,rpat,rep) ;
   }
   return THD_simmat_crossnobis( nitem , nr , nvx , rpat ) ;
}

/*! Second-order IS-RSA at one location.  Each subject first gets an inner
    condition RDM; its compact triangle is then one feature vector in the outer
    subject-by-subject neural matrix.  cond_metric applies only to ordinary
    dset input.  A runset supplies crossnobis (optionally whitened) inner RDMs. */

static THD_simmat * rsa_second_order_rdm(
   THD_roilist *rl , int kk , int nsub , int ncond , int nvx ,
   int cond_metric , int outer_metric , THD_3dim_dataset **dset ,
   THD_runset *runset , THD_datatable_index *condition_index,
   int center_conditions , float *triF , float *pat ,
   float **rpat, float *runraw, RSA_whiten *wh )
{
   THD_simmat *sr ; int jj , aa , nctri=THD_NTRI(ncond) ;

   for( jj=0 ; jj < nsub ; jj++ ){
     sr=rsa_subject_rdm(jj,rl,kk,ncond,nvx,cond_metric,dset,runset,condition_index,
                        center_conditions,pat,rpat,runraw,wh) ;
     if( sr == NULL ) return NULL ;
     THD_simmat_to_tri(sr,triF+(size_t)jj*nctri) ;
     /* The outer features are RDM entries in a common dissimilarity sense.
        Correlation/rank/cosine inner constructors return similarities; 1-s
        preserves Pearson/Spearman second-order geometry and makes cosine's
        origin plus exported semantics unambiguous.  Euclid/crossnobis already
        return distances (including valid negative crossnobis estimates). */
     if( !sr->is_dist )
       for( aa=0 ; aa < nctri ; aa++ )
         triF[(size_t)jj*nctri+aa] = 1.0f-triF[(size_t)jj*nctri+aa] ;
     THD_simmat_free(sr) ;
   }
   return THD_simmat_from_features(nsub,nctri,triF,outer_metric) ;
}

/*! Build the one seed model from the main input data.  IS-RSA returns the
    subject-by-subject seed geometry.  Classic RSA additionally returns every
    subject's compact condition RDM through classic_tri; the returned matrix is
    their descriptive mean and is never substituted for those subject-specific
    seed RDMs in inference.  Input datasets must already be loaded. */

static THD_simmat * rsa_build_seed_model(
   THD_roilist *seedrl, int rdm_over, int mode, int nsub, int nvals,
   int neu_metric, int cond_metric, int polort, int center_conditions,
   THD_3dim_dataset **dset, THD_runset *runset,
   THD_datatable_index *condition_index, int noise_norm,
   float **classic_tri )
{
   THD_simmat *out=NULL,*sm=NULL ; RSA_whiten wh ;
   float *F=NULL,*pat=NULL,*triF=NULL,**rpat=NULL,*runraw=NULL,*avgtri=NULL ;
   int svx,jj,rr,maxrun=0,maxnt=0,maxntot=0,ntri=THD_NTRI(nvals) ;

   if( classic_tri != NULL ) *classic_tri=NULL ;
   if( seedrl==NULL || seedrl->nroi!=1 || seedrl->vox[0].nar<1 ) return NULL ;
   svx=seedrl->vox[0].nar ;
   memset(&wh,0,sizeof(wh)) ; wh.mode=noise_norm ;

   if( runset != NULL ){
     int ss ;
     for( ss=0 ; ss<runset->nsub ; ss++ ){
       int ntot=0 ;
       if( runset->nrun[ss]>maxrun ) maxrun=runset->nrun[ss] ;
       for( rr=0 ; rr<runset->nrun[ss] ; rr++ ){
         int row=runset->row_of[ss][rr] ;
         if( noise_norm!=NN_NONE ){
           int nt=DSET_NVALS(runset->resid[row]) ;
           if( nt>maxnt ) maxnt=nt ; ntot+=nt ;
         }
       }
       if( ntot>maxntot ) maxntot=ntot ;
     }
     rpat=(float **)malloc(sizeof(float *)*maxrun) ;
     for( rr=0 ; rr<maxrun ; rr++ )
       rpat[rr]=(float *)malloc(sizeof(float)*(size_t)nvals*svx) ;
     if( runset->has_condmap )
       runraw=(float *)malloc(sizeof(float)*(size_t)runset->maxbrick*svx) ;
     if( noise_norm!=NN_NONE ){
       wh.residbuf=(float *)malloc(sizeof(float)*(size_t)maxnt*svx) ;
       wh.Rmat=(float *)malloc(sizeof(float)*(size_t)maxntot*svx) ;
       wh.wdiag=(float *)malloc(sizeof(float)*svx) ;
       wh.wtmp=(float *)malloc(sizeof(float)*svx) ;
       if( noise_norm==NN_SHRINK )
         wh.Whalf=(float *)malloc(sizeof(float)*(size_t)svx*svx) ;
     }
   }

   if( rdm_over==RDM_SUBJ ){
     if( mode==MODE_CONT ){
       F=(float *)malloc(sizeof(float)*(size_t)nsub*nvals) ;
       for( jj=0 ; jj<nsub ; jj++ )
         THD_roi_mean_ts(dset[jj],seedrl->vox,polort,F+(size_t)jj*nvals) ;
       out=THD_simmat_from_features(nsub,nvals,F,neu_metric) ;
     } else if( mode==MODE_RDM ){
       int nctri=THD_NTRI(nvals) ;
       triF=(float *)malloc(sizeof(float)*(size_t)nsub*nctri) ;
       pat=(float *)malloc(sizeof(float)*(size_t)nvals*svx) ;
       out=rsa_second_order_rdm(seedrl,0,nsub,nvals,svx,cond_metric,
                                neu_metric,dset,runset,condition_index,center_conditions,
                                triF,pat,rpat,runraw,&wh) ;
     } else {
       int nf=svx*nvals ;
       F=(float *)malloc(sizeof(float)*(size_t)nsub*nf) ;
       for( jj=0 ; jj<nsub ; jj++ )
         rsa_subject_patterns(jj,nvals,svx,dset,condition_index,seedrl->vox,
                              F+(size_t)jj*nf) ;
       out=THD_simmat_from_features(nsub,nf,F,neu_metric) ;
     }
   } else {
     float *all=(float *)malloc(sizeof(float)*(size_t)nsub*ntri) ;
     pat=(float *)malloc(sizeof(float)*(size_t)nvals*svx) ;
     for( jj=0 ; jj<nsub ; jj++ ){
       sm=rsa_subject_rdm(jj,seedrl,0,nvals,svx,neu_metric,dset,runset,condition_index,
                          center_conditions,pat,rpat,runraw,&wh) ;
       if( sm==NULL ){ free(all) ; all=NULL ; goto done ; }
       THD_simmat_to_tri(sm,all+(size_t)jj*ntri) ; THD_simmat_free(sm) ;
     }
     avgtri=(float *)calloc(ntri,sizeof(float)) ;
     for( jj=0 ; jj<nsub ; jj++ ){
       int aa ; for( aa=0 ; aa<ntri ; aa++ ) avgtri[aa]+=all[(size_t)jj*ntri+aa] ;
     }
     { int aa ; for( aa=0 ; aa<ntri ; aa++ ) avgtri[aa]/=(float)nsub ; }
     out=THD_simmat_new(nvals) ; THD_tri_to_simmat(nvals,avgtri,out) ;
     out->is_dist=(runset!=NULL || neu_metric==SIM_EUCLID) ;
     if( classic_tri != NULL ) *classic_tri=all ; else free(all) ;
   }

done:
   free(F) ; free(pat) ; free(triF) ; free(runraw) ; free(avgtri) ;
   if( rpat!=NULL ){ for( rr=0 ; rr<maxrun ; rr++ ) free(rpat[rr]) ; free(rpat) ; }
   free(wh.residbuf) ; free(wh.Rmat) ; free(wh.Whalf) ; free(wh.wdiag) ; free(wh.wtmp) ;
   return out ;
}

/*! Ascending float comparison, to sort a max-statistic null before lookup. */

static int flt_cmp_asc( const void *a , const void *b )
{
   float aa = *(const float *)a , bb = *(const float *)b ;
   return (aa > bb) - (aa < bb) ;
}

/*! Monte-Carlo tail probability with the standard plus-one correction for a
    random bootstrap null (unlike permutation arrays, it has no identity slot). */
static float rsa_mc_emp_pvalue( float *sorted_abs, int n, float obsabs )
{
   int lo=0,hi=n ;
   if( sorted_abs==NULL || n<1 ) return -1.0f ;
   while( lo<hi ){ int mid=lo+(hi-lo)/2 ; if( sorted_abs[mid]<obsabs ) lo=mid+1 ; else hi=mid ; }
   return (float)(n-lo+1)/(float)(n+1) ;
}

/*! Linear-interpolated percentile of x[0..n-1].  x is scratch and is sorted. */

static float rsa_percentile( float *x , int n , float prob )
{
   double pos, frac ; int lo, hi ;
   if( x == NULL || n < 1 ) return 0.0f ;
   qsort(x,n,sizeof(float),flt_cmp_asc) ;
   if( prob <= 0.0f ) return x[0] ;
   if( prob >= 1.0f ) return x[n-1] ;
   pos = (double)(n-1)*(double)prob ; lo = (int)floor(pos) ; hi = (int)ceil(pos) ;
   frac = pos-(double)lo ;
   return (float)((1.0-frac)*(double)x[lo] + frac*(double)x[hi]) ;
}

static void rsa_cond_resample_free( RSA_cond_resample *cb )
{
   if( cb == NULL ) return ;
   free(cb->nitem) ; free(cb->offset) ; free(cb->index) ; free(cb->valid) ; free(cb) ;
}

/*! Build synchronized condition samples.  group_file, when present, contains
    exactly one whitespace-free descriptor label per nonblank/noncomment line.
    A group is sampled as a unit and expanded in original condition order. */
static RSA_cond_resample * rsa_cond_resample_build( int ncond, int nresample,
                                                     long seed, char *group_file )
{
   RSA_cond_resample *cb=NULL ; THD_resample_set *gr=NULL ;
   int *gof=NULL, *gsz=NULL, *gitem=NULL, *gidx=NULL, *seen=NULL ;
   char **lab=NULL ; int ng=0, ii, gg, bb, total=0 ;

   gidx = (int *)malloc(sizeof(int)*ncond) ;
   if( gidx == NULL ) return NULL ;
   if( group_file == NULL ){
     ng=ncond ; for( ii=0 ; ii < ncond ; ii++ ) gidx[ii]=ii ;
   } else {
     FILE *fp=fopen(group_file,"r") ; char line[4096], tok[512], extra[2] ; int nr=0 ;
     if( fp == NULL ){
       ERROR_message("3dRSA: cannot open -cond_group file '%s'",group_file) ; goto bad ;
     }
     lab = (char **)calloc(ncond,sizeof(char *)) ;
     while( fgets(line,sizeof(line),fp) != NULL ){
       char *p=line, *hash ;
       while( isspace((unsigned char)*p) ) p++ ;
       if( *p == '\0' || *p == '#' ) continue ;
       hash=strchr(p,'#') ; if( hash != NULL ) *hash='\0' ;
       if( sscanf(p,"%511s %1s",tok,extra) != 1 ){
         ERROR_message("3dRSA: -cond_group '%s' needs one label per line",group_file) ;
         fclose(fp) ; goto bad ;
       }
       if( nr >= ncond ){
         ERROR_message("3dRSA: -cond_group '%s' has more than %d condition labels",
                       group_file,ncond) ; fclose(fp) ; goto bad ;
       }
       lab[nr] = strdup(tok) ; if( lab[nr] == NULL ){ fclose(fp) ; goto bad ; } nr++ ;
     }
     fclose(fp) ;
     if( nr != ncond ){
       ERROR_message("3dRSA: -cond_group '%s' has %d labels; need %d (one per condition)",
                     group_file,nr,ncond) ; goto bad ;
     }
     for( ii=0 ; ii < ncond ; ii++ ){
       for( gg=0 ; gg < ii ; gg++ ) if( strcmp(lab[ii],lab[gg]) == 0 ) break ;
       if( gg < ii ) gidx[ii]=gidx[gg] ; else gidx[ii]=ng++ ;
     }
     if( ng < 3 ){
       ERROR_message("3dRSA: -cond_group '%s' defines only %d groups; need at least 3",
                     group_file,ng) ; goto bad ;
     }
   }

   gsz=(int *)calloc(ng,sizeof(int)) ; gof=(int *)calloc(ng+1,sizeof(int)) ;
   gitem=(int *)malloc(sizeof(int)*ncond) ;
   if( gsz == NULL || gof == NULL || gitem == NULL ) goto bad ;
   for( ii=0 ; ii < ncond ; ii++ ) gsz[gidx[ii]]++ ;
   for( gg=0 ; gg < ng ; gg++ ) gof[gg+1]=gof[gg]+gsz[gg] ;
   memset(gsz,0,sizeof(int)*ng) ;
   for( ii=0 ; ii < ncond ; ii++ ){
     gg=gidx[ii] ; gitem[gof[gg]+gsz[gg]++]=ii ;
   }

   gr=THD_resample_set_build(ng,nresample,seed) ; if( gr == NULL ) goto bad ;
   cb=(RSA_cond_resample *)calloc(1,sizeof(RSA_cond_resample)) ;
   if( cb == NULL ) goto bad ;
   cb->nresample=nresample ; cb->ncond=ncond ; cb->ngroup=ng ;
   cb->nitem=(int *)calloc(nresample,sizeof(int)) ;
   cb->offset=(int *)calloc(nresample+1,sizeof(int)) ;
   cb->valid=(unsigned char *)calloc(nresample,sizeof(unsigned char)) ;
   if( cb->nitem == NULL || cb->offset == NULL || cb->valid == NULL ) goto bad ;
   for( bb=0 ; bb < nresample ; bb++ ){
     int ni=0, *sel=gr->index+(size_t)bb*ng ;
     for( gg=0 ; gg < ng ; gg++ ) ni += gsz[sel[gg]] ;
     if( ni > cb->maxitem ) cb->maxitem=ni ;
     if( (long long)cb->offset[bb]+ni > INT_MAX ){
       ERROR_message("3dRSA: condition-bootstrap index set is too large") ; goto bad ;
     }
     cb->nitem[bb]=ni ; cb->offset[bb+1]=cb->offset[bb]+ni ;
   }
   total=cb->offset[nresample] ;
   cb->index=(int *)malloc(sizeof(int)*(size_t)total) ; seen=(int *)malloc(sizeof(int)*ncond) ;
   if( cb->index == NULL || seen == NULL ) goto bad ;
   for( bb=0 ; bb < nresample ; bb++ ){
     int *sel=gr->index+(size_t)bb*ng, pos=cb->offset[bb], nu=0 ;
     memset(seen,0,sizeof(int)*ncond) ;
     for( ii=0 ; ii < ng ; ii++ ){
       int sg=sel[ii], jj ;
       for( jj=gof[sg] ; jj < gof[sg+1] ; jj++ ){
         int cc=gitem[jj] ; cb->index[pos++]=cc ;
         if( !seen[cc] ){ seen[cc]=1 ; nu++ ; }
       }
     }
     if( nu >= 3 ){ cb->valid[bb]=1 ; cb->nvalid++ ; }
   }
   { long long mt=(long long)cb->maxitem*(cb->maxitem-1)/2 ;
     if( mt > INT_MAX ){
       ERROR_message("3dRSA: -cond_group bootstrap sample is too large (%d positions)",
                     cb->maxitem) ; goto bad ;
     }
     cb->maxtri=(int)mt ; }
   if( cb->nvalid < 10 ){
     ERROR_message("3dRSA: only %d condition-bootstrap draws retained 3 distinct\n"
                   "       conditions; need at least 10 usable draws",cb->nvalid) ;
     goto bad ;
   }

   THD_resample_set_free(gr) ; free(gof) ; free(gsz) ; free(gitem) ; free(gidx) ; free(seen) ;
   if( lab != NULL ){ for( ii=0 ; ii < ncond ; ii++ ) free(lab[ii]) ; free(lab) ; }
   return cb ;
bad:
   if( gr != NULL ) THD_resample_set_free(gr) ;
   rsa_cond_resample_free(cb) ; free(gof) ; free(gsz) ; free(gitem) ; free(gidx) ; free(seen) ;
   if( lab != NULL ){ for( ii=0 ; ii < ncond ; ii++ ) free(lab[ii]) ; free(lab) ; }
   return NULL ;
}

/*! Extract one condition-bootstrap draw into compact, missing-diagonal-free
    neural/model triangles.  Returns the retained dyad count. */
static int rsa_cond_boot_tri( float *neural, int nmod, THD_simmat **model,
                              RSA_cond_resample *cb, int bb, float *y, float **x )
{
   int *ix=cb->index+cb->offset[bb], ni=cb->nitem[bb] ;
   int aa, jj, mm, m=0, n=cb->ncond ;
   for( aa=0 ; aa < ni ; aa++ ) for( jj=aa+1 ; jj < ni ; jj++ ){
     int ia=ix[aa], ib=ix[jj] ;
     if( ia == ib ) continue ;       /* duplicated-condition diagonal artifact */
     { int i0=(ia<ib)?ia:ib , i1=(ia<ib)?ib:ia ;
       size_t ti=(size_t)i0*(2*n-i0-1)/2 + (size_t)(i1-i0-1) ;
       y[m]=neural[ti] ; }
     for( mm=0 ; mm < nmod ; mm++ ) x[mm][m]=model[mm]->mat[ia*n+ib] ;
     m++ ;
   }
   return m ;
}

static void rsa_cond_boot_ci( float *sum, int nsub, RSA_cond_resample *cb,
                              int do_tanh, float alpha, float *draw,
                              float *blo, float *bhi )
{
   int bb, nv=0 ;
   for( bb=0 ; bb < cb->nresample ; bb++ ) if( cb->valid[bb] ){
     float v=sum[bb]/(float)nsub ; draw[nv++]=do_tanh ? tanhf(v) : v ;
   }
   *blo=rsa_percentile(draw,nv,0.5f*alpha) ;
   *bhi=rsa_percentile(draw,nv,1.0f-0.5f*alpha) ;
}

/*! Sample variance on the estimator's working scale. */
static double rsa_sample_variance( float *x, int n )
{
   double av=0.0,ss=0.0,d ; int ii ;
   if( x == NULL || n < 2 ) return 0.0 ;
   for( ii=0 ; ii<n ; ii++ ) av += x[ii] ;
   av /= (double)n ;
   for( ii=0 ; ii<n ; ii++ ){ d=(double)x[ii]-av ; ss += d*d ; }
   return ss/(double)(n-1) ;
}

/*! F6 corrected two-factor bootstrap interval.  vals contains the observed
    subject effects and cval[s,b] the effect for subject s under synchronized
    condition draw b, all on one working scale (Fisher z for correlations and
    paired contrasts, standardized beta for joint regression).  Following the
    dual-bootstrap estimator, subject-only (Vs), condition-only (Vc), and
    simultaneous (Vsc) variances are combined with their finite-sample factors,
    then bounded by the corrected one-factor variances and Vsc. */
static int rsa_dual_boot_ci( int nsub, float *vals, THD_resample_set *sr,
                             RSA_cond_resample *cr, float *cval, int do_tanh,
                             float alpha, float *work, float *blo, float *bhi )
{
   float *sdraw=work,*cdraw=work+sr->nresample,*scdraw=cdraw+cr->nresample ;
   double obs=0.0,Vs,Vc,Vsc,V,fs,fc,se,tcrit ;
   int bb,jj,nv=0,df ;
   if( nsub<2 || cr->ngroup<2 || sr->nresample!=cr->nresample ) return 0 ;
   for( jj=0 ; jj<nsub ; jj++ ) obs += vals[jj] ;
   obs /= (double)nsub ;
   for( bb=0 ; bb<sr->nresample ; bb++ ){
     int *ix=sr->index+(size_t)bb*nsub ; double ss=0.0 ;
     for( jj=0 ; jj<nsub ; jj++ ) ss += vals[ix[jj]] ;
     sdraw[bb]=(float)(ss/(double)nsub) ;
     if( cr->valid[bb] ){
       double cs=0.0,scs=0.0 ;
       for( jj=0 ; jj<nsub ; jj++ ){
         cs  += cval[(size_t)jj*cr->nresample+bb] ;
         scs += cval[(size_t)ix[jj]*cr->nresample+bb] ;
       }
       cdraw[nv]=(float)(cs/(double)nsub) ;
       scdraw[nv]=(float)(scs/(double)nsub) ; nv++ ;
     }
   }
   if( nv<2 ) return 0 ;
   Vs=rsa_sample_variance(sdraw,sr->nresample) ;
   Vc=rsa_sample_variance(cdraw,nv) ; Vsc=rsa_sample_variance(scdraw,nv) ;
   fs=(double)nsub/(double)(nsub-1) ;
   fc=(double)cr->ngroup/(double)(cr->ngroup-1) ;
   V=fs*Vs+fc*Vc-fs*fc*(Vsc-Vs-Vc) ;
   if( V < fs*Vs ) V=fs*Vs ;
   if( V < fc*Vc ) V=fc*Vc ;
   if( V > Vsc ) V=Vsc ;
   if( V < 0.0 ) V=0.0 ;
   df=((nsub<cr->ngroup)?nsub:cr->ngroup)-1 ;
   tcrit=student_p2t((double)alpha,(double)df) ; se=sqrt(V) ;
   *blo=(float)(obs-tcrit*se) ; *bhi=(float)(obs+tcrit*se) ;
   if( do_tanh ){ *blo=tanhf(*blo) ; *bhi=tanhf(*bhi) ; }
   return nv ;
}

/*! Cache one real-series Fourier spectrum per subject. */
static int rsa_phase_prepare( int nsub, int nt, float *series, complex *spec )
{
   int jj,tt ;
   if( series == NULL || spec == NULL ) return 1 ;
   for( jj=0 ; jj<nsub ; jj++ ){
     for( tt=0 ; tt<nt ; tt++ ){
       spec[(size_t)jj*nt+tt].r=series[(size_t)jj*nt+tt] ;
       spec[(size_t)jj*nt+tt].i=0.0f ;
     }
     if( THD_fftnf_OMP(nt,NULL,&spec[(size_t)jj*nt].r,
                       &spec[(size_t)jj*nt].i,-2,0.0) ) return 1 ;
   }
   return 0 ;
}

/*! Build one real-valued phase surrogate and its subject similarity matrix.
    Positive-frequency bins are rotated, their negative partners are filled by
    conjugate symmetry, and DC plus the even-length Nyquist bin are unchanged.
    AFNI's interleaved FFT scaling loop does not visit every complex sample,
    so inverse normalization is applied explicitly after the transform. */
static int rsa_phase_draw( THD_phase_set *pset, int iphase, complex *spec,
                           complex *work, float *series, THD_simmat *sm,
                           int metric, float *sc1, float *sc2 )
{
   int jj,tt,kk,nt=pset->ntime,nsub=pset->nobs ;
   for( jj=0 ; jj<nsub ; jj++ ){
     complex *src=spec+(size_t)jj*nt ;
     memcpy(work,src,sizeof(complex)*nt) ;
     for( kk=1 ; kk<=pset->nfreq ; kk++ ){
       float co,si,ar=src[kk].r,ai=src[kk].i ;
       THD_phase_set_factor(pset,iphase,jj,kk,&co,&si) ;
       work[kk].r=ar*co-ai*si ; work[kk].i=ar*si+ai*co ;
       work[nt-kk].r=work[kk].r ; work[nt-kk].i=-work[kk].i ;
     }
     if( THD_fftnf_OMP(nt,NULL,&work[0].r,&work[0].i,+2,0.0) ) return 1 ;
     for( tt=0 ; tt<nt ; tt++ )
       series[(size_t)jj*nt+tt]=work[tt].r/(float)nt ;
   }
   return THD_simmat_fill_from_features(sm,nt,series,metric,sc1,sc2) ;
}

/*! Test continuous IS-RSA effects against one synchronized temporal null.
    Each draw rebuilds only the neural response matrix; fixed and
    per-location dataset models, nuisance matrices, and their standardized
    regression designs remain unrandomized.  Thus regression coefficients retain
    their usual conditional interpretation while the null asks whether the
    raw-series temporal alignment contributes any model-associated geometry.

    Identity slot 0 and all temporal surrogates feed the same primary and paired
    contrast families.  Regression designs retain their pseudoinverse across
    draws, so neither temporal null reintroduces one SVD per draw. */
static void rsa_temporal_infer( THD_simmat *neural, int nmod,
                                 THD_simmat **model, int nort,
                                 THD_simmat **orts, int joint,
                                 int ncon, RSA_contrast *con, float *Fmain,
                                 int neu_metric, int cmp,
                                 THD_timeshift_set *tset,
                                 THD_phase_set *phset, THD_rdm_ws *ws,
                                 unsigned char *need, float *lagtab,
                                 float *prep, float *norm,
                                 complex *phspec, complex *phwork,
                                 float *phseries, float *phsc1, float *phsc2,
                                 THD_simmat *nullmat, float *stat,
                                 float *effect, float *pval, float *zscr,
                                 float *nullabs, float *cstat, float *cpval,
                                 float *czscr, float *cnullabs )
{
   int ss,mm,cc,oo,nsub=tset?tset->nobs:phset->nobs ;
   int nt=tset?tset->ntime:phset->ntime,ndraw=tset?tset->nshift:phset->nphase ;
   int m=THD_NTRI(nsub) ;
   int ncol=nmod+nort, *nge=NULL, *cnge=NULL ;
   float *xflat=NULL, **x=NULL, *ytri=NULL, *rmod=NULL, *btmp=NULL ;
   THD_tri_design **design=NULL ;

   nge=(int *)calloc(nmod,sizeof(int)) ;
   if( ncon > 0 ) cnge=(int *)calloc(ncon,sizeof(int)) ;
   xflat=(float *)malloc(sizeof(float)*(size_t)ncol*m) ;
   x=(float **)malloc(sizeof(float *)*ncol) ;
   ytri=(float *)malloc(sizeof(float)*m) ;
   rmod=(float *)malloc(sizeof(float)*nmod) ;
   btmp=(float *)malloc(sizeof(float)*ncol) ;
   if( joint || nort > 0 ) design=(THD_tri_design **)calloc(nmod,sizeof(*design)) ;
   if( nge == NULL || (ncon>0 && cnge==NULL) || xflat==NULL || x==NULL ||
       ytri==NULL || rmod==NULL || btmp==NULL || ((joint||nort>0) && design==NULL) ||
       (tset != NULL && THD_simmat_lag_table(nsub,nt,Fmain,neu_metric,
                                             need,lagtab,prep,norm)) ||
       (phset != NULL && rsa_phase_prepare(nsub,nt,Fmain,phspec)) ){
     for( mm=0 ; mm<nmod ; mm++ ){
       stat[mm]=effect[mm]=zscr[mm]=0.0f ; pval[mm]=1.0f ;
       if( nullabs ) memset(nullabs+(size_t)mm*ndraw,0,sizeof(float)*ndraw) ;
     }
     for( cc=0 ; cc<ncon ; cc++ ){
       cstat[cc]=czscr[cc]=0.0f ; cpval[cc]=1.0f ;
       if( cnullabs ) memset(cnullabs+(size_t)cc*ndraw,0,sizeof(float)*ndraw) ;
     }
     goto TS_DONE ;
   }

   for( mm=0 ; mm<nmod ; mm++ ){
     x[mm]=xflat+(size_t)mm*m ; THD_simmat_to_tri(model[mm],x[mm]) ;
   }
   for( oo=0 ; oo<nort ; oo++ ){
     x[nmod+oo]=xflat+(size_t)(nmod+oo)*m ;
     THD_simmat_to_tri(orts[oo],x[nmod+oo]) ;
   }
   for( mm=0 ; mm<nmod ; mm++ ) stat[mm]=effect[mm]=0.0f ;

   /* The observed effect and partial-r reporting columns keep exactly the
      established label-null regression definitions; only their test null is
      replaced below. */
   if( joint ){
     THD_rdm_regress(neural,nmod,model,nort,orts,cmp,NULL,ws,
                     stat,effect,NULL,NULL) ;
     design[0]=THD_tri_design_new(m,ncol,x,cmp,ws) ;
   } else if( nort > 0 ){
     float *xf[1+nort] ;
     for( oo=0 ; oo<nort ; oo++ ) xf[1+oo]=x[nmod+oo] ;
     for( mm=0 ; mm<nmod ; mm++ ){
       float bt[1],pt[1] ; xf[0]=x[mm] ;
       THD_rdm_regress(neural,1,model+mm,nort,orts,cmp,NULL,ws,
                       bt,pt,NULL,NULL) ;
       stat[mm]=bt[0] ; effect[mm]=pt[0] ;
       design[mm]=THD_tri_design_new(m,1+nort,xf,cmp,ws) ;
     }
   } else {
     THD_simmat_to_tri(neural,ytri) ;
     for( mm=0 ; mm<nmod ; mm++ ){
       stat[mm]=THD_tri_corr(m,ytri,x[mm],cmp,ws->sc1,ws->sc2) ;
       effect[mm]=stat[mm] ;
     }
   }

   THD_simmat_to_tri(neural,ytri) ;
   if( ncon > 0 ){
     for( mm=0 ; mm<nmod ; mm++ )
       rmod[mm]=THD_tri_corr(m,ytri,x[mm],cmp,ws->sc1,ws->sc2) ;
     for( cc=0 ; cc<ncon ; cc++ )
       cstat[cc]=rmod[con[cc].ia]-rmod[con[cc].ib] ;
   }

   nullmat->is_dist=(neu_metric == SIM_EUCLID) ;
   for( ss=0 ; ss<ndraw ; ss++ ){
     THD_simmat *sn=neural ;
     if( ss > 0 ){
       if( tset != NULL ){
         int *off=tset->offset+(size_t)ss*nsub ;
         THD_simmat_from_lag_table(nullmat,nt,lagtab,off) ;
       } else if( rsa_phase_draw(phset,ss,phspec,phwork,phseries,nullmat,
                                 neu_metric,phsc1,phsc2) ){
         memset(nullmat->mat,0,sizeof(float)*(size_t)nsub*nsub) ;
       }
       sn=nullmat ;
     }
     THD_simmat_to_tri(sn,ytri) ;

     if( joint ){
       if( design[0] != NULL ) THD_tri_design_apply(design[0],ytri,ws,btmp) ;
       else memset(btmp,0,sizeof(float)*ncol) ;
       for( mm=0 ; mm<nmod ; mm++ ){
         float av=fabsf(btmp[mm]) ;
         if( nullabs ) nullabs[(size_t)mm*ndraw+ss]=av ;
         if( av >= fabsf(stat[mm]) ) nge[mm]++ ;
       }
     } else if( nort > 0 ){
       for( mm=0 ; mm<nmod ; mm++ ){
         float av ;
         if( design[mm] != NULL ) THD_tri_design_apply(design[mm],ytri,ws,btmp) ;
         else btmp[0]=0.0f ;
         av=fabsf(btmp[0]) ;
         if( nullabs ) nullabs[(size_t)mm*ndraw+ss]=av ;
         if( av >= fabsf(stat[mm]) ) nge[mm]++ ;
       }
     }

     if( (!joint && nort==0) || ncon>0 ){
       for( mm=0 ; mm<nmod ; mm++ )
         rmod[mm]=THD_tri_corr(m,ytri,x[mm],cmp,ws->sc1,ws->sc2) ;
     }
     if( !joint && nort==0 ) for( mm=0 ; mm<nmod ; mm++ ){
       float av=fabsf(rmod[mm]) ;
       if( nullabs ) nullabs[(size_t)mm*ndraw+ss]=av ;
       if( av >= fabsf(stat[mm]) ) nge[mm]++ ;
     }
     for( cc=0 ; cc<ncon ; cc++ ){
       float dv=rmod[con[cc].ia]-rmod[con[cc].ib], av=fabsf(dv) ;
       if( cnullabs ) cnullabs[(size_t)cc*ndraw+ss]=av ;
       if( av >= fabsf(cstat[cc]) ) cnge[cc]++ ;
     }
   }

   for( mm=0 ; mm<nmod ; mm++ ){
     pval[mm]=(float)nge[mm]/(float)ndraw ;
     zscr[mm]=THD_p_to_z(pval[mm],stat[mm]) ;
   }
   for( cc=0 ; cc<ncon ; cc++ ){
     cpval[cc]=(float)cnge[cc]/(float)ndraw ;
     czscr[cc]=THD_p_to_z(cpval[cc],cstat[cc]) ;
   }

TS_DONE:
   if( design != NULL ){
     int nd=joint ? 1 : nmod ;
     for( mm=0 ; mm<nd ; mm++ ) THD_tri_design_free(design[mm]) ;
     free(design) ;
   }
   if( nge   ) free(nge) ;
   if( cnge  ) free(cnge) ;
   if( xflat ) free(xflat) ;
   if( x     ) free(x) ;
   if( ytri  ) free(ytri) ;
   if( rmod  ) free(rmod) ;
   if( btmp  ) free(btmp) ;
}

/*! Percentile CI for a classic-RSA group effect.  vals are the independent
    subject effects: Fisher-z correlations for an ordinary model (do_tanh=1),
    or regression coefficients for a joint model (do_tanh=0). */

static void rsa_boot_subject_mean( int nsub , float *vals , THD_resample_set *rset,
                                   int do_tanh , float alpha , float *draw,
                                   float *blo , float *bhi )
{
   int bb, jj ;
   for( bb=0 ; bb < rset->nresample ; bb++ ){
     int *ix = rset->index + (size_t)bb*nsub ; float sm=0.0f ;
     for( jj=0 ; jj < nsub ; jj++ ) sm += vals[ix[jj]] ;
     sm /= (float)nsub ; draw[bb] = do_tanh ? tanhf(sm) : sm ;
   }
   *blo = rsa_percentile(draw,rset->nresample,0.5f*alpha) ;
   *bhi = rsa_percentile(draw,rset->nresample,1.0f-0.5f*alpha) ;
}

/*! Percentile interval for a completed LOO prediction vector.  The fitted
    out-of-sample predictions are held fixed and subject rows (prediction plus
    observed target) are resampled together.  For a profile target, the same
    row draw is used for every measure and the reported statistic remains the
    equal-weight mean measure-wise correlation.  This quantifies uncertainty
    over the evaluated subjects, not instability from refitting every fold. */
static int rsa_boot_loo_predictions( int n, int p, float *pred, float **target,
                                     int cmp, THD_resample_set *rset,
                                     THD_rdm_ws *ws, float alpha, float *draw,
                                     int *seen, float *x, float *y,
                                     float *blo, float *bhi )
{
   int bb,ii,v,ndraw=0 ;
   for( bb=0 ; bb<rset->nresample ; bb++ ){
     int *ix=rset->index+(size_t)bb*n,nu=0 ; double acc=0.0 ;
     memset(seen,0,sizeof(int)*n) ;
     for( ii=0 ; ii<n ; ii++ ) if( !seen[ix[ii]] ){ seen[ix[ii]]=1 ; nu++ ; }
     if( nu<3 ) continue ;
     for( v=0 ; v<p ; v++ ){
       float *pv=pred+(size_t)v*n ;
       for( ii=0 ; ii<n ; ii++ ){ x[ii]=pv[ix[ii]] ; y[ii]=target[v][ix[ii]] ; }
       acc += THD_tri_corr(n,x,y,cmp,ws->sc1,ws->sc2) ;
     }
     draw[ndraw++]=(float)(acc/p) ;
   }
   if( ndraw<1 ){ *blo=*bhi=0.0f ; return 0 ; }
   *blo=rsa_percentile(draw,ndraw,0.5f*alpha) ;
   *bhi=rsa_percentile(draw,ndraw,1.0f-0.5f*alpha) ;
   return ndraw ;
}

/*! Percentile CI for a plain IS-RSA Mantel effect.  Resampling subjects with
    replacement duplicates rows/columns.  A pair of sampled positions that
    both refer to the same original subject is the artificial diagonal created
    by resampling, not an observed dyad, and is therefore omitted.  Other dyads
    retain their bootstrap multiplicity.  Returns the number of usable draws. */

static int rsa_boot_isrsa( THD_simmat *neural , THD_simmat *model , int cmp,
                           THD_resample_set *rset , THD_rdm_ws *ws , float alpha,
                           float *draw , int *seen , float *blo , float *bhi )
{
   int bb, aa, jj, n=rset->nobs, ndraw=0 ;
   for( bb=0 ; bb < rset->nresample ; bb++ ){
     int *ix = rset->index + (size_t)bb*n, m=0, nu=0 ;
     memset(seen,0,sizeof(int)*n) ;
     for( aa=0 ; aa < n ; aa++ ) if( !seen[ix[aa]] ){ seen[ix[aa]]=1 ; nu++ ; }
     if( nu < 3 ) continue ;
     for( aa=0 ; aa < n ; aa++ ) for( jj=aa+1 ; jj < n ; jj++ ){
       int ia=ix[aa], ib=ix[jj] ;
       if( ia == ib ) continue ;       /* repeated-subject diagonal artifact */
       ws->tri[m]   = neural->mat[ia*n+ib] ;
       ws->yperm[m] = model ->mat[ia*n+ib] ;
       m++ ;
     }
     if( m >= 3 )
       draw[ndraw++] = THD_tri_corr(m,ws->tri,ws->yperm,cmp,ws->sc1,ws->sc2) ;
   }
   if( ndraw < 1 ){ *blo = *bhi = 0.0f ; return 0 ; }
   *blo = rsa_percentile(draw,ndraw,0.5f*alpha) ;
   *bhi = rsa_percentile(draw,ndraw,1.0f-0.5f*alpha) ;
   return ndraw ;
}

/*! Percentile CI for an IS-RSA model contrast.  Each subject resample is
    applied jointly to the neural RDM and both model RDMs, so the draw is the
    paired difference r(neural,A)-r(neural,B).  As in rsa_boot_isrsa(), dyads
    between duplicate copies of one original subject are omitted. */

static int rsa_boot_isrsa_contrast( THD_simmat *neural , THD_simmat *model_a,
                                    THD_simmat *model_b , int cmp,
                                    THD_resample_set *rset , THD_rdm_ws *ws,
                                    float alpha , float *draw , int *seen,
                                    float *blo , float *bhi )
{
   int bb, aa, jj, n=rset->nobs, ndraw=0 ;
   for( bb=0 ; bb < rset->nresample ; bb++ ){
     int *ix = rset->index + (size_t)bb*n, m=0, nu=0 ;
     float ra, rb ;
     memset(seen,0,sizeof(int)*n) ;
     for( aa=0 ; aa < n ; aa++ ) if( !seen[ix[aa]] ){ seen[ix[aa]]=1 ; nu++ ; }
     if( nu < 3 ) continue ;
     for( aa=0 ; aa < n ; aa++ ) for( jj=aa+1 ; jj < n ; jj++ ){
       int ia=ix[aa], ib=ix[jj] ;
       if( ia == ib ) continue ;
       ws->tri[m]   = neural ->mat[ia*n+ib] ;
       ws->yperm[m] = model_a->mat[ia*n+ib] ;
       ws->yfit[m]  = model_b->mat[ia*n+ib] ;
       m++ ;
     }
     if( m < 3 ) continue ;
     ra = THD_tri_corr(m,ws->tri,ws->yperm,cmp,ws->sc1,ws->sc2) ;
     rb = THD_tri_corr(m,ws->tri,ws->yfit ,cmp,ws->sc1,ws->sc2) ;
     draw[ndraw++] = ra-rb ;
   }
   if( ndraw < 1 ){ *blo = *bhi = 0.0f ; return 0 ; }
   *blo = rsa_percentile(draw,ndraw,0.5f*alpha) ;
   *bhi = rsa_percentile(draw,ndraw,1.0f-0.5f*alpha) ;
   return ndraw ;
}

/*! Build exactly nresample usable subject draws for dyadic IS-RSA.  Draws with
    fewer than three distinct subjects cannot define a correlation after the
    artificial duplicate-copy diagonals are removed, so generate a reserve and
    compact those draws once, globally, before parallel inference. */
static THD_resample_set * rsa_isrsa_resample_set( int nsub, int nresample,
                                                  long seed, int *block )
{
   THD_resample_set *rs ; int bb,ii,nkeep=0,ncand ; unsigned char *seen ;
   if( nresample<1 || nsub<3 || nresample>INT_MAX/2 ) return NULL ;
   ncand=2*nresample ;
   rs=(block!=NULL) ? THD_resample_set_build_stratified(nsub,ncand,seed,block)
                    : THD_resample_set_build(nsub,ncand,seed) ;
   if( rs==NULL ) return NULL ;
   seen=(unsigned char *)malloc((size_t)nsub) ;
   if( seen==NULL ){ THD_resample_set_free(rs) ; return NULL ; }
   for( bb=0 ; bb<ncand && nkeep<nresample ; bb++ ){
     int *src=rs->index+(size_t)bb*nsub,nu=0 ;
     memset(seen,0,(size_t)nsub) ;
     for( ii=0 ; ii<nsub ; ii++ ) if( !seen[src[ii]] ){ seen[src[ii]]=1 ; nu++ ; }
     if( nu<3 ) continue ;
     if( nkeep!=bb ) memcpy(rs->index+(size_t)nkeep*nsub,src,sizeof(int)*(size_t)nsub) ;
     nkeep++ ;
   }
   free(seen) ;
   if( nkeep<nresample ){ THD_resample_set_free(rs) ; return NULL ; }
   { int *keep=(int *)realloc(rs->index,sizeof(int)*(size_t)nsub*nresample) ;
     if( keep!=NULL ) rs->index=keep ; }
   rs->nresample=nresample ;
   return rs ;
}

/*! Paired subject-bootstrap test of equal IS-RSA model performance.  Both model
    effects are recomputed on each identical subject draw; the null is centered
    as d* - d_obs, so it permits both models to have nonzero common alignment.
    nullabs[] receives the synchronized centered |null| for max-stat FWE. */
static THD_permstat rsa_isrsa_superiority_test(
       THD_simmat *neural, THD_simmat *model_a, THD_simmat *model_b, int cmp,
       THD_resample_set *rset, THD_rdm_ws *ws, float *draw, int *seen,
       float *nullabs )
{
   THD_permstat ps ; int bb,aa,jj,n=rset?rset->nobs:0,B=rset?rset->nresample:0,nge=0 ;
   float obs=THD_mantel_contrast_effect(neural,model_a,model_b,cmp,ws,NULL,NULL) ;
   ps.stat=obs ; ps.pval=-1.0f ; ps.zscr=obs ; ps.nperm=B ;
   if( rset==NULL || draw==NULL || seen==NULL || B<1 ) return ps ;
   for( bb=0 ; bb<B ; bb++ ){
     int *ix=rset->index+(size_t)bb*n,m=0 ; float ra,rb,dn ;
     memset(seen,0,sizeof(int)*(size_t)n) ;
     for( aa=0 ; aa<n ; aa++ ) seen[ix[aa]]=1 ; /* set already guarantees >=3 */
     for( aa=0 ; aa<n ; aa++ ) for( jj=aa+1 ; jj<n ; jj++ ){
       int ia=ix[aa],ib=ix[jj] ; if( ia==ib ) continue ;
       ws->tri[m]=neural->mat[ia*n+ib] ;
       ws->yperm[m]=model_a->mat[ia*n+ib] ;
       ws->yfit[m]=model_b->mat[ia*n+ib] ; m++ ;
     }
     ra=THD_tri_corr(m,ws->tri,ws->yperm,cmp,ws->sc1,ws->sc2) ;
     rb=THD_tri_corr(m,ws->tri,ws->yfit ,cmp,ws->sc1,ws->sc2) ;
     draw[bb]=ra-rb ; dn=fabsf(draw[bb]-obs) ;
     if( nullabs!=NULL ) nullabs[bb]=dn ;
     if( dn>=fabsf(obs) ) nge++ ;
   }
   ps.pval=(float)(nge+1)/(float)(B+1) ;
   ps.zscr=THD_perm_signed_z(ps.pval,obs,PERM_TAIL_TWO) ;
   return ps ;
}

/*! Percentile CIs for IS-RSA regression coefficients.  Every subject draw is
    packed into compact neural/model/nuisance triangles after duplicate-copy
    diagonal dyads are removed, then the standardized regression is refit via
    THD_tri_regress.  joint!=0 fits all reported models together; otherwise
    each reported model is fit separately with all nuisance columns. */

static int rsa_boot_isrsa_regress( THD_simmat *neural, int nmod,
                                   THD_simmat **model, int nort,
                                   THD_simmat **orts, int cmp, int joint,
                                   THD_resample_set *rset, THD_rdm_ws *ws,
                                   float alpha, float *y, float **x,
                                   float *coef, float *beta,
                                   int *seen, float **blo, float **bhi, int loc )
{
   int bb, aa, jj, mm, oo, n=rset->nobs, ncol=nmod+nort, ndraw=0 ;
   float *xfit[ncol] ;

   for( bb=0 ; bb < rset->nresample ; bb++ ){
     int *ix=rset->index+(size_t)bb*n, m=0, nu=0 ;
     memset(seen,0,sizeof(int)*n) ;
     for( aa=0 ; aa < n ; aa++ ) if( !seen[ix[aa]] ){ seen[ix[aa]]=1 ; nu++ ; }
     if( nu < 3 ) continue ;
     for( aa=0 ; aa < n ; aa++ ) for( jj=aa+1 ; jj < n ; jj++ ){
       int ia=ix[aa], ib=ix[jj] ;
       if( ia == ib ) continue ;
       y[m]=neural->mat[ia*n+ib] ;
       for( mm=0 ; mm < nmod ; mm++ ) x[mm][m]=model[mm]->mat[ia*n+ib] ;
       for( oo=0 ; oo < nort ; oo++ ) x[nmod+oo][m]=orts[oo]->mat[ia*n+ib] ;
       m++ ;
     }
     if( m < 3 ) continue ;
     if( joint ){
       THD_tri_regress(m,y,ncol,x,cmp,ws,beta) ;
       for( mm=0 ; mm < nmod ; mm++ )
         coef[(size_t)mm*rset->nresample+ndraw]=beta[mm] ;
     } else {
       for( oo=0 ; oo < nort ; oo++ ) xfit[1+oo]=x[nmod+oo] ;
       for( mm=0 ; mm < nmod ; mm++ ){
         xfit[0]=x[mm] ;
         THD_tri_regress(m,y,1+nort,xfit,cmp,ws,beta) ;
         coef[(size_t)mm*rset->nresample+ndraw]=beta[0] ;
       }
     }
     ndraw++ ;
   }
   if( ndraw < 1 ){
     for( mm=0 ; mm < nmod ; mm++ ) blo[mm][loc]=bhi[mm][loc]=0.0f ;
     return 0 ;
   }
   for( mm=0 ; mm < nmod ; mm++ ){
     float *v=coef+(size_t)mm*rset->nresample ;
     blo[mm][loc]=rsa_percentile(v,ndraw,0.5f*alpha) ;
     bhi[mm][loc]=rsa_percentile(v,ndraw,1.0f-0.5f*alpha) ;
   }
   return ndraw ;
}

/*! Percentile CIs for one IS-RSA commonality request.  The neural and both
    model triangles use the same subject draw, with duplicate-copy diagonal
    dyads omitted, and all raw/partial quantities are recomputed together. */

static int rsa_boot_isrsa_commonality( THD_simmat *neural, THD_simmat *model_a,
                                       THD_simmat *model_b, int cmp,
                                       THD_resample_set *rset, THD_rdm_ws *ws,
                                       float alpha, float *y, float *a, float *b,
                                       float *draw, int *seen,
                                       float **blo, float **bhi, int qbase, int loc )
{
   int bb, aa, jj, cc, n=rset->nobs, ndraw=0 ;
   for( bb=0 ; bb < rset->nresample ; bb++ ){
     int *ix=rset->index+(size_t)bb*n, m=0, nu=0 ;
     float comp[RSA_NCOMMON] ;
     memset(seen,0,sizeof(int)*n) ;
     for( aa=0 ; aa < n ; aa++ ) if( !seen[ix[aa]] ){ seen[ix[aa]]=1 ; nu++ ; }
     if( nu < 3 ) continue ;
     for( aa=0 ; aa < n ; aa++ ) for( jj=aa+1 ; jj < n ; jj++ ){
       int ia=ix[aa], ib=ix[jj] ;
       if( ia == ib ) continue ;
       y[m]=neural ->mat[ia*n+ib] ;
       a[m]=model_a->mat[ia*n+ib] ;
       b[m]=model_b->mat[ia*n+ib] ;
       m++ ;
     }
     if( m < 3 ) continue ;
     THD_tri_commonality(m,y,a,b,cmp,ws,comp) ;
     for( cc=0 ; cc < RSA_NCOMMON ; cc++ )
       draw[(size_t)cc*rset->nresample+ndraw]=comp[cc] ;
     ndraw++ ;
   }
   if( ndraw < 1 ){
     for( cc=0 ; cc < RSA_NCOMMON ; cc++ )
       blo[qbase+cc][loc]=bhi[qbase+cc][loc]=0.0f ;
     return 0 ;
   }
   for( cc=0 ; cc < RSA_NCOMMON ; cc++ ){
     float *v=draw+(size_t)cc*rset->nresample ;
     blo[qbase+cc][loc]=rsa_percentile(v,ndraw,0.5f*alpha) ;
     bhi[qbase+cc][loc]=rsa_percentile(v,ndraw,1.0f-0.5f*alpha) ;
   }
   return ndraw ;
}

/*! F8 counterpart for all seven raw three-model regions and the three
    conditional partial-R2 effects. */
static int rsa_boot_isrsa_commonality3( THD_simmat *neural, THD_simmat *model_a,
                                        THD_simmat *model_b, THD_simmat *model_c,
                                        int cmp, THD_resample_set *rset,
                                        THD_rdm_ws *ws, float alpha, float *y,
                                        float *a, float *b, float *c, float *draw,
                                        int *seen, float **blo, float **bhi,
                                        int qbase, int loc )
{
   int bb,aa,jj,cc,n=rset->nobs,ndraw=0 ;
   for( bb=0 ; bb<rset->nresample ; bb++ ){
     int *ix=rset->index+(size_t)bb*n,m=0,nu=0 ; float comp[RSA_NCOMMON3] ;
     memset(seen,0,sizeof(int)*n) ;
     for( aa=0 ; aa<n ; aa++ ) if( !seen[ix[aa]] ){ seen[ix[aa]]=1 ; nu++ ; }
     if( nu<3 ) continue ;
     for( aa=0 ; aa<n ; aa++ ) for( jj=aa+1 ; jj<n ; jj++ ){
       int ia=ix[aa],ib=ix[jj] ; if( ia==ib ) continue ;
       y[m]=neural->mat[ia*n+ib] ; a[m]=model_a->mat[ia*n+ib] ;
       b[m]=model_b->mat[ia*n+ib] ; c[m]=model_c->mat[ia*n+ib] ; m++ ;
     }
     if( m<3 ) continue ;
     THD_tri_commonality3(m,y,a,b,c,cmp,ws,comp) ;
     for( cc=0 ; cc<RSA_NCOMMON3 ; cc++ )
       draw[(size_t)cc*rset->nresample+ndraw]=comp[cc] ;
     ndraw++ ;
   }
   if( ndraw<1 ){
     for( cc=0 ; cc<RSA_NCOMMON3 ; cc++ ) blo[qbase+cc][loc]=bhi[qbase+cc][loc]=0.0f ;
     return 0 ;
   }
   for( cc=0 ; cc<RSA_NCOMMON3 ; cc++ ){
     float *v=draw+(size_t)cc*rset->nresample ;
     blo[qbase+cc][loc]=rsa_percentile(v,ndraw,0.5f*alpha) ;
     bhi[qbase+cc][loc]=rsa_percentile(v,ndraw,1.0f-0.5f*alpha) ;
   }
   return ndraw ;
}

/*! S1 fixed-effects classic-RSA condition-label test.  One permutation of the
    condition axis is shared across every subject; model_b!=NULL requests the
    paired Fisher-z difference A-B.  The statistic is the mean subject Fisher z
    (or mean within-subject z difference), so nsub=1 is well-defined.  When the
    covariance-weighted comparison is active, srdmcov contains the already
    transformed subject RDMs and covA/covB are per-thread model scratch. */
static THD_permstat rsa_classic_condition_test(
       int nsub, int nitem, float *srdm, float *srdmcov,
       THD_simmat *model_a, THD_simmat *model_b, int cmp,
       PERM_set *cset, THD_rdm_ws *ws, float *covA, float *covB,
       float *permnull )
{
   THD_permstat ps ; int np=(cset!=NULL)?cset->nperm:0,pk,jj,nge=0 ;
   int m=THD_NTRI(nitem),iscov=(cmp==CMP_CORR_COV || cmp==CMP_COS_COV) ;
   float obs=0.0f,*pn=permnull ;

   ps.stat=0.0f ; ps.pval=-1.0f ; ps.zscr=0.0f ; ps.nperm=np ;
   if( nsub<1 || nitem<2 || srdm==NULL || model_a==NULL || ws==NULL ) return ps ;

   /* Normal inference always supplies the per-thread FWE buffer.  Retain a
      private fallback so this primitive is also safe for point/unit callers. */
   if( np>0 && pn==NULL ) pn=(float *)malloc(sizeof(float)*np) ;

   for( pk=0 ; pk<((np>0)?np:1) ; pk++ ){
     int *perm=(np>0)?cset->perm+(size_t)pk*nitem:NULL ; double sum=0.0 ;
     if( perm!=NULL ) THD_simmat_to_tri_perm(model_a,perm,ws->yperm) ;
     else             THD_simmat_to_tri     (model_a,     ws->yperm) ;
     if( model_b!=NULL ){
       if( perm!=NULL ) THD_simmat_to_tri_perm(model_b,perm,ws->yfit) ;
       else             THD_simmat_to_tri     (model_b,     ws->yfit) ;
     }
     if( iscov ){
       THD_rdm_cov_transform(nitem,ws->yperm,cmp==CMP_CORR_COV,covA) ;
       if( model_b!=NULL )
         THD_rdm_cov_transform(nitem,ws->yfit,cmp==CMP_CORR_COV,covB) ;
     }
     for( jj=0 ; jj<nsub ; jj++ ){
       float ra,rb=0.0f ;
       if( iscov ){
         float *cs=srdmcov+(size_t)jj*nitem*nitem ;
         ra=THD_rdm_cov_cosine(nitem,cs,covA) ;
         if( model_b!=NULL ) rb=THD_rdm_cov_cosine(nitem,cs,covB) ;
       } else {
         float *st=srdm+(size_t)jj*m ;
         ra=THD_tri_corr(m,st,ws->yperm,cmp,ws->sc1,ws->sc2) ;
         if( model_b!=NULL )
           rb=THD_tri_corr(m,st,ws->yfit,cmp,ws->sc1,ws->sc2) ;
       }
       sum += MYatanh(ra) - ((model_b!=NULL)?MYatanh(rb):0.0f) ;
     }
     { float stat=(float)(sum/nsub) ;
       if( pk==0 ) obs=stat ;
       if( np>0 ) pn[pk]=fabsf(stat) ; }
   }
   ps.stat=obs ;
   if( np>0 ){
     for( pk=0 ; pk<np ; pk++ ) if( pn[pk]>=fabsf(obs) ) nge++ ;
     ps.pval=(float)nge/(float)np ;
     ps.zscr=THD_perm_signed_z(ps.pval,obs,PERM_TAIL_TWO) ;
   } else ps.zscr=obs ;
   if( pn!=permnull ) free(pn) ;
   return ps ;
}

/*! Relabel both axes of a compact symmetric matrix.  out(i,j)=in(p[i],p[j])
    for i<j, using the same strict-triangle storage as THD_simmat_to_tri. */

static void rsa_tri_perm( int n, float *in, int *p, float *out )
{
   int i,j,m=0 ;
   for( i=0 ; i<n ; i++ ) for( j=i+1 ; j<n ; j++ ){
     int a=p[i],b=p[j],lo=(a<b)?a:b,hi=(a<b)?b:a ;
     size_t q=(size_t)lo*(2*n-lo-1)/2+(size_t)(hi-lo-1) ;
     out[m++]=in[q] ;
   }
}

/*! Fixed-sample condition-label inference for classic representational
    connectivity.  Each subject owns both a seed and target condition RDM. One
    condition relabeling is applied to the seed side and shared across every
    subject and target location.  The statistic is mean subject Fisher z. */

static THD_permstat rsa_classic_seed_condition_test(
       int nsub, int nitem, float *target_tri, float *seed_tri, int cmp,
       PERM_set *cset, THD_rdm_ws *ws, float *permnull )
{
   THD_permstat ps ; int np=(cset!=NULL)?cset->nperm:0,pk,jj,nge=0 ;
   int m=THD_NTRI(nitem) ; float obs=0.0f,*pn=permnull ;
   ps.stat=0.0f ; ps.pval=-1.0f ; ps.zscr=0.0f ; ps.nperm=np ;
   if( nsub<1 || nitem<2 || target_tri==NULL || seed_tri==NULL || ws==NULL ) return ps ;
   if( np>0 && pn==NULL ) pn=(float *)malloc(sizeof(float)*np) ;
   for( pk=0 ; pk<((np>0)?np:1) ; pk++ ){
     int *perm=(np>0)?cset->perm+(size_t)pk*nitem:NULL ; double sum=0.0 ;
     for( jj=0 ; jj<nsub ; jj++ ){
       float *tt=target_tri+(size_t)jj*m,*ss=seed_tri+(size_t)jj*m,rv ;
       if( perm!=NULL ){ rsa_tri_perm(nitem,ss,perm,ws->yperm) ; ss=ws->yperm ; }
       rv=THD_tri_corr(m,tt,ss,cmp,ws->sc1,ws->sc2) ; sum+=MYatanh(rv) ;
     }
     { float stat=(float)(sum/nsub) ;
       if( pk==0 ) obs=stat ;
       if( np>0 ) pn[pk]=fabsf(stat) ; }
   }
   ps.stat=obs ;
   if( np>0 ){
     for( pk=0 ; pk<np ; pk++ ) if( pn[pk]>=fabsf(obs) ) nge++ ;
     ps.pval=(float)nge/(float)np ;
     ps.zscr=THD_perm_signed_z(ps.pval,obs,PERM_TAIL_TWO) ;
   } else ps.zscr=obs ;
   if( pn!=permnull ) free(pn) ;
   return ps ;
}

static char *rsa_options[] = {
   "-mask" , "-roi_sel" , "-seed_mask" , "-seed_roi" ,
   "-searchlight" , "-surf" , "-mode" , "-featuretype" , "-polort" ,
   "-memory_limit" , "-memory_override" ,
   "-dataTable" , "-dataTableFile" , "-condition_column" , "-condition_order" ,
   "-run_column" , "-run_normalize" , "-run_analysis" ,
   "-run_model" , "-run_center" , "-run_factor" , "-run_contrast" ,
   "-runwiseTable" , "-noise_norm" ,
   "-center_conditions" ,
   "-model" , "-model_mat" , "-model_series" , "-model_dset" , "-model_joint" , "-ortvec" ,
   "-model_label" , "-model_fit" , "-fit_ridge" , "-fit_condfold" ,
   "-model_contrast" , "-contrast_hypothesis" , "-model_commonality" , "-group_test" , "-classic_null" ,
   "-noise_ceiling" , "-nc_split" , "-loo" , "-block" ,
   "-neural_metric" , "-condition_metric" , "-metric" , "-nperm" , "-null" , "-min_shift" ,
   "-bootstrap" , "-cond_bootstrap" ,
   "-cond_group" , "-boot_ci" , "-seed" ,
   "-prefix" , "-no_dset" , "-save_rdm" , "-quiet" , "-progress" ,
   "-help" , "-h" , NULL
} ;

/*============================================================================*/

int main( int argc , char *argv[] )
{
   THD_3dim_dataset *mset=NULL , *oset=NULL , **dset=NULL , *seedset=NULL ;
   THD_roilist *rl=NULL , *seedrl=NULL ;
   THD_datatable *tab=NULL , *longtab=NULL ;
   THD_datatable_index *condition_index=NULL ;
   RSA_series_runs *series_runs=NULL ;
   char *maskname=NULL , *prefix="RSA" , *save_rdm=NULL , *roi_sel=NULL ;
   char *seed_mask=NULL , *seed_roi_sel=NULL ;
   float *seed_srdm=NULL ; int seed_excluded=0 ;
   char *runwise_file=NULL ; THD_runset *runset=NULL ;   /* -runwiseTable */
   char *run_column=NULL ; int run_normalize=RUN_NORM_ZSCORE,run_normalize_given=0 ;
   char *condition_column=NULL,*condition_order_arg=NULL,**condition_level=NULL ;
   int ncondition_level=0 ;
   int run_analysis=RUN_ANALYSIS_CONCAT,run_analysis_given=0,run_resolved=0 ;
   char **runmodspec=NULL,**runcenter=NULL ; int nrunmodspec=0,nruncenter=0,nrunmod=0 ;
   char **runfactorspec=NULL,**runconspec=NULL ; int nrunfactorspec=0,nrunconspec=0 ;
   RSA_runfactor *runfactor=NULL ; RSA_runcontrast *runcon=NULL ; float *rcon_weight=NULL ;
   int noise_norm=NN_NONE ;                              /* -noise_norm */
   int center_conditions=0 ;                             /* S2 ordinary RDM re-meaning */
   int nopt=1 , mode=MODE_CONT , rdm_over=RDM_SUBJ , feat_override=-1 ;
   int neu_metric=SIM_PEARSON , cond_metric=SIM_PEARSON , cond_metric_given=0 ;
   int cmp_metric=CMP_SPEARMAN ;
   int nperm=5000 , nboot=0 , ncboot=0 , dualboot=0 , boot_ci_given=0 , polort=-1 ;
   int null_mode=NULL_LABELS , min_shift=1 , min_shift_given=0 ;
   int do_dset=1 , quiet=0 , progress_mode=RSA_PROGRESS_AUTO , joint=0 , regout=0 ;
   float boot_ci=95.0f ;
   int do_nc=0 ;                        /* -noise_ceiling */
#define NC_HALF       0
#define NC_INTERLEAVE 1
   int nc_split=NC_HALF ;               /* -nc_split half|interleave */
   float *ncA=NULL , *ncB=NULL ;        /* IS-RSA: reliability; RSA: low, high */
   int do_loo=0 , nloo=0 , nloofam=0 , maxloocol=1 ; /* LOO outputs/families */
   int *loo_owner=NULL , *loo_fam=NULL ; /* model -> canonical model / FWE family */
   float **lr=NULL , **lp=NULL , **lq=NULL , **lz=NULL ;
   float **lblo=NULL , **lbhi=NULL ;       /* fixed-OOF LOO bootstrap bounds */
   char *block_col=NULL ;               /* -block: exchangeability-block column */
   int *block_lab=NULL , nblock=0 ;      /* normalized bootstrap/permutation strata */
   PERM_set *pset=NULL ;                /* shared relabelings, all ROIs/voxels */
   PERM_set *cpset=NULL ;               /* F15 classic condition relabelings */
   THD_mantel_cache *mcache=NULL ;       /* F9 fixed model x relabeling cache */
   int *mcache_ix=NULL ;                 /* global model index -> cache slot */
   THD_timeshift_set *tset=NULL ;       /* shared circular offsets, all locations */
   THD_phase_set *phset=NULL ;          /* stateless shared Fourier phases */
   unsigned char *tsneed=NULL ;         /* F19 pair x relative lags actually drawn */
   THD_resample_set *rset=NULL ;        /* uncertainty draws; never a null set */
   THD_resample_set *contrast_rset=NULL ; /* fixed-RDM IS-RSA superiority null */
   THD_resample_set *fit_contrast_rset=NULL ; /* fitted common-fold superiority */
   RSA_cond_resample *crset=NULL ;      /* synchronized condition-axis draws */
   char *cond_group_file=NULL ;
   char *sl_nbhd=NULL ; int streaming=0 ;   /* -searchlight: radius or SHAPE() */
   char *surf_file=NULL ;                    /* -surf: mesh for geodesic searchlight */
   double memory_limit_gib=0.0 ; int memory_limit_given=0 , memory_override=0 ;
   long seed=0 ;

   RSA_model *mod=NULL ; int nmod=0 ;
   RSA_contrast *con=NULL ; int ncon=0 ;
   RSA_common *com=NULL ; int nreqcom=0 , ncomq=0 ; /* flattened pair/triple quantities */
   char **comlab=NULL ;
   RSA_fitmodel *fit=NULL ; int nfit=0 , nfitw=0 ;
   RSA_fitcontrast *fcon=NULL ; int nfitcon=0 ;
   char **modspec=NULL ; int nmodspec=0 ;
   char **matspec=NULL ; int nmatspec=0 ;
   char *series_file=NULL , **series_time=NULL ; int nseries=0 ;
   char **dsespec=NULL ; int ndsespec=0 ;
   char **ortspec=NULL ; int nortspec=0 ;    /* -ortvec nuisance columns */
   THD_simmat **ort=NULL ; int nort=0 ;       /* 2 nuisance matrices per ortvec */
   /* per-spec display labels (-model_label sets the name of the NEXT model);
      NULL entries fall back to the auto-generated name */
   char **modlabel=NULL , **matlabel=NULL , **dselabel=NULL , *pending_label=NULL ;
   char **constrspec=NULL ; int nconstrspec=0 ;   /* -model_contrast "A-B" specs */
   char **comspec=NULL ; int ncomspec=0 ;      /* -model_commonality "A,B[,C]" specs */
   char **fitspec=NULL ; int nfitspec=0 ;         /* -model_fit "NAME=A,B" specs */
   float fit_ridge=0.01f ; int fit_ridge_given=0 ;
   char *fit_condfold_file=NULL ; RSA_condfold *fit_condfold=NULL ;
   int group_test=0 ;                             /* 0=signflip, 1=signedrank */
   int classic_null=CLASSIC_NULL_SUBJECTS ;       /* S1 primary classic null */
   int classic_null_given=0 ;
   int contrast_hypothesis=CONTRAST_LEGACY ;
   int contrast_hypothesis_given=0 ;

   int nvox , nvals=0 , nsub , nroi=0 , nitem , ntri ;
   float **cmean=NULL ;
   float **rr=NULL , **ee=NULL , **pp=NULL , **qq=NULL , **zz=NULL ;
   float **run_rr=NULL,**run_ee=NULL,**run_pp=NULL,**run_qq=NULL,**run_zz=NULL ;
   float **run_pf=NULL,**run_zf=NULL,*run_mxflat=NULL ;
   float **rcon_rr=NULL,**rcon_ee=NULL,**rcon_pp=NULL,**rcon_qq=NULL,**rcon_zz=NULL ;
   float **rcon_pf=NULL,**rcon_zf=NULL ;
   float **blo=NULL , **bhi=NULL ;      /* model subject or dual-bootstrap bounds */
   float **dblo=NULL , **dbhi=NULL ;    /* contrast subject or dual-bootstrap bounds */
   float **cblo=NULL , **cbhi=NULL ;    /* condition-bootstrap percentile bounds */
   float **pf=NULL , **zf=NULL ;        /* max-stat FWE: p and signed z per model */
   float  *mxflat=NULL ;                /* [nmod*npfwe] shared max-null over ROIs */
   float **lpf=NULL , **lzf=NULL ;      /* LOO max-stat FWE: separate stat family */
   float  *lmxflat=NULL ;               /* [nloofam*npfwe] shared LOO max-null */
   /* model-contrast results (per contrast, per ROI): observed difference, p, q,
      z, and its own max-stat FWE family */
   float **crd=NULL , **cd=NULL , **ce=NULL , **cp=NULL , **cq=NULL , **cz=NULL ;
   float **cpf=NULL , **czf=NULL ;   /* classic: crd=rDiff, cd=zDiff; ce=test stat */
   float  *cmxflat=NULL ; int do_confwe=0 ;
   /* commonality results, flattened over ncomq quantities (per ROI).
      cav is both the reported component and the statistic the FWE null is on
      (the neural-relabeling null is on the component itself). */
   float **cav=NULL , **cap=NULL , **caq=NULL , **caz=NULL ;
   float **calo=NULL , **cahi=NULL ;  /* commonality subject-bootstrap bounds */
   float **capf=NULL , **cazf=NULL ;
   float  *camx=NULL ; int do_cafwe=0 , ncaperm=0 ;
   float **fr=NULL,**fpv=NULL,**fqv=NULL,**fzv=NULL,**fpf=NULL,**fzf=NULL,**fwgt=NULL ;
   float *fmx=NULL ; int do_fitfwe=0,nfitperm=0 ;
   float **fcd=NULL,**fcp=NULL,**fcq=NULL,**fcz=NULL,**fcpf=NULL,**fczf=NULL ;
   float *fcmx=NULL ; int do_fitconfwe=0 ;
   int do_fwe=0 , do_loofwe=0 , npfwe=0 ;  /* shared null-set length for FWE */
   int ii , jj , kk , mm ;
   double program_start ; RSA_progress progress ;

   if( argc == 1 ){ usage_3dRSA(1) ; exit(0) ; }

   mainENTRY("3dRSA main") ; machdep() ; AFNI_SETUP_OMP(0) ;
   AFNI_logger("3dRSA",argc,argv) ;
   program_start=0.001*(double)NI_clock_time() ;
   PRINT_VERSION("3dRSA") ; THD_check_AFNI_version("3dRSA") ;

   /*================== option processing ==================*/

   while( nopt < argc && argv[nopt][0] == '-' ){

      if( strcasecmp(argv[nopt],"-h")    == 0 ||
          strcasecmp(argv[nopt],"-help") == 0   ){
        usage_3dRSA( strlen(argv[nopt]) > 3 ? 2 : 1 ) ; exit(0) ;
      }

      if( strcasecmp(argv[nopt],"-mask") == 0 ){
        if( ++nopt >= argc ) ERROR_exit("3dRSA: need an argument after -mask") ;
        maskname = argv[nopt] ; nopt++ ; continue ;
      }
      if( strcasecmp(argv[nopt],"-roi_sel") == 0 ){
        if( ++nopt >= argc ) ERROR_exit("3dRSA: need an argument after -roi_sel") ;
        roi_sel = argv[nopt] ; nopt++ ; continue ;
      }
      if( strcasecmp(argv[nopt],"-seed_mask") == 0 ){
        if( ++nopt >= argc ) ERROR_exit("3dRSA: need a dataset after -seed_mask") ;
        if( seed_mask != NULL ) ERROR_exit("3dRSA: -seed_mask was given twice") ;
        seed_mask=argv[nopt] ; nopt++ ; continue ;
      }
      if( strcasecmp(argv[nopt],"-seed_roi") == 0 ){
        if( ++nopt >= argc ) ERROR_exit("3dRSA: need an AFNI integer selector after -seed_roi") ;
        if( seed_roi_sel != NULL ) ERROR_exit("3dRSA: -seed_roi was given twice") ;
        seed_roi_sel=argv[nopt] ; nopt++ ; continue ;
      }
      if( strcasecmp(argv[nopt],"-searchlight") == 0 ){
        if( ++nopt >= argc )
          ERROR_exit("3dRSA: need a radius or SHAPE() after -searchlight") ;
        sl_nbhd = argv[nopt] ; nopt++ ; continue ;
      }
      if( strcasecmp(argv[nopt],"-memory_limit") == 0 ){
        if( ++nopt >= argc ) ERROR_exit("3dRSA: need GiB after -memory_limit") ;
        memory_limit_gib=rsa_parse_double("-memory_limit",argv[nopt],DBL_MIN,DBL_MAX) ;
        memory_limit_given=1 ; nopt++ ; continue ;
      }
      if( strcasecmp(argv[nopt],"-memory_override") == 0 ){
        memory_override=1 ; nopt++ ; continue ;
      }
      if( strcasecmp(argv[nopt],"-surf") == 0 ){
        if( ++nopt >= argc ) ERROR_exit("3dRSA: need a surface file after -surf") ;
        surf_file = argv[nopt] ; nopt++ ; continue ;
      }
      if( strcasecmp(argv[nopt],"-mode") == 0 ){
        if( ++nopt >= argc ) ERROR_exit("3dRSA: need an argument after -mode") ;
             if( strcasecmp(argv[nopt],"IS-RSA")       == 0 ||
                 strcasecmp(argv[nopt],"ISRSA")        == 0 ||
                 strcasecmp(argv[nopt],"intersubject") == 0 ) rdm_over = RDM_SUBJ ;
        else if( strcasecmp(argv[nopt],"RSA")          == 0 ||
                 strcasecmp(argv[nopt],"classic")      == 0 ) rdm_over = RDM_BRICK ;
        else ERROR_exit("3dRSA: -mode must be 'IS-RSA' or 'RSA', not '%s'",argv[nopt]) ;
        nopt++ ; continue ;
      }
      if( strcasecmp(argv[nopt],"-featuretype") == 0 ){
        if( ++nopt >= argc ) ERROR_exit("3dRSA: need an argument after -featuretype") ;
             if( strcasecmp(argv[nopt],"mean")    == 0 ) feat_override = MODE_CONT ;
        else if( strcasecmp(argv[nopt],"pattern") == 0 ) feat_override = MODE_BETA ;
        else if( strcasecmp(argv[nopt],"rdm")     == 0 ) feat_override = MODE_RDM ;
        else ERROR_exit("3dRSA: -featuretype must be mean, pattern, or rdm, not '%s'",
                        argv[nopt]) ;
        nopt++ ; continue ;
      }
      if( strcasecmp(argv[nopt],"-neural_metric") == 0 ){
        if( ++nopt >= argc ) ERROR_exit("3dRSA: need an argument after -neural_metric") ;
             if( strcasecmp(argv[nopt],"corr")   == 0 ) neu_metric = SIM_PEARSON ;
        else if( strcasecmp(argv[nopt],"scorr")  == 0 ) neu_metric = SIM_SPEARMAN ;
        else if( strcasecmp(argv[nopt],"cosine") == 0 ) neu_metric = SIM_COSINE ;
        else if( strcasecmp(argv[nopt],"euclid") == 0 ) neu_metric = SIM_EUCLID ;
        else ERROR_exit("3dRSA: unknown -neural_metric '%s'",argv[nopt]) ;
        nopt++ ; continue ;
      }
      if( strcasecmp(argv[nopt],"-condition_metric") == 0 ){
        if( ++nopt >= argc ) ERROR_exit("3dRSA: need an argument after -condition_metric") ;
             if( strcasecmp(argv[nopt],"corr")   == 0 ) cond_metric = SIM_PEARSON ;
        else if( strcasecmp(argv[nopt],"scorr")  == 0 ) cond_metric = SIM_SPEARMAN ;
        else if( strcasecmp(argv[nopt],"cosine") == 0 ) cond_metric = SIM_COSINE ;
        else if( strcasecmp(argv[nopt],"euclid") == 0 ) cond_metric = SIM_EUCLID ;
        else ERROR_exit("3dRSA: unknown -condition_metric '%s'",argv[nopt]) ;
        cond_metric_given=1 ; nopt++ ; continue ;
      }
      if( strcasecmp(argv[nopt],"-center_conditions") == 0 ){
        if( ++nopt >= argc )
          ERROR_exit("3dRSA: need 'none' or 'subject' after -center_conditions") ;
             if( strcasecmp(argv[nopt],"none")    == 0 ) center_conditions=0 ;
        else if( strcasecmp(argv[nopt],"subject") == 0 ) center_conditions=1 ;
        else ERROR_exit("3dRSA: -center_conditions must be 'none' or 'subject', not '%s'",
                        argv[nopt]) ;
        nopt++ ; continue ;
      }
      if( strcasecmp(argv[nopt],"-metric") == 0 ){
        if( ++nopt >= argc ) ERROR_exit("3dRSA: need an argument after -metric") ;
             if( strcasecmp(argv[nopt],"spearman") == 0 ) cmp_metric = CMP_SPEARMAN ;
        else if( strcasecmp(argv[nopt],"pearson")  == 0 ) cmp_metric = CMP_PEARSON ;
        else if( strcasecmp(argv[nopt],"ktaub")    == 0 ) cmp_metric = CMP_KTAUB ;
        else if( strcasecmp(argv[nopt],"ktaua")    == 0 ) cmp_metric = CMP_KTAUA ;
        else if( strcasecmp(argv[nopt],"rhoa")     == 0 ||
                 strcasecmp(argv[nopt],"rho-a")    == 0 ||
                 strcasecmp(argv[nopt],"rho_a")    == 0 ) cmp_metric = CMP_RHOA ;
        else if( strcasecmp(argv[nopt],"corr_cov") == 0 ) cmp_metric = CMP_CORR_COV ;
        else if( strcasecmp(argv[nopt],"cosine_cov")==0 ) cmp_metric = CMP_COS_COV ;
        else ERROR_exit("3dRSA: unknown -metric '%s'",argv[nopt]) ;
        nopt++ ; continue ;
      }
      if( strcasecmp(argv[nopt],"-nperm") == 0 ){
        if( ++nopt >= argc ) ERROR_exit("3dRSA: need an argument after -nperm") ;
        nperm = (int)rsa_parse_long("-nperm",argv[nopt],0,INT_MAX) ;
        nopt++ ; continue ;
      }
      if( strcasecmp(argv[nopt],"-null") == 0 ){
        if( ++nopt >= argc ) ERROR_exit("3dRSA: need labels, timeshift, or phase after -null") ;
             if( strcasecmp(argv[nopt],"labels")    == 0 ||
                 strcasecmp(argv[nopt],"relabel")   == 0 ) null_mode=NULL_LABELS ;
        else if( strcasecmp(argv[nopt],"timeshift") == 0 ) null_mode=NULL_TIMESHIFT ;
        else if( strcasecmp(argv[nopt],"phase")      == 0 ||
                 strcasecmp(argv[nopt],"phaserandom") == 0 ||
                 strcasecmp(argv[nopt],"phase_randomize") == 0 ) null_mode=NULL_PHASE ;
        else ERROR_exit("3dRSA: -null must be labels, timeshift, or phase, not '%s'",argv[nopt]) ;
        nopt++ ; continue ;
      }
      if( strcasecmp(argv[nopt],"-min_shift") == 0 ){
        if( ++nopt >= argc ) ERROR_exit("3dRSA: need a number of TRs after -min_shift") ;
        min_shift=(int)rsa_parse_long("-min_shift",argv[nopt],1,INT_MAX) ; min_shift_given=1 ;
        nopt++ ; continue ;
      }
      if( strcasecmp(argv[nopt],"-bootstrap") == 0 ){
        if( ++nopt >= argc ) ERROR_exit("3dRSA: need an argument after -bootstrap") ;
        nboot = (int)rsa_parse_long("-bootstrap",argv[nopt],0,INT_MAX) ;
        if( nboot > 0 && nboot < 20 )
          ERROR_exit("3dRSA: -bootstrap needs at least 20 samples (or use 0 to disable)") ;
        nopt++ ; continue ;
      }
      if( strcasecmp(argv[nopt],"-cond_bootstrap") == 0 ){
        if( ++nopt >= argc ) ERROR_exit("3dRSA: need an argument after -cond_bootstrap") ;
        ncboot = (int)rsa_parse_long("-cond_bootstrap",argv[nopt],0,INT_MAX) ;
        if( ncboot > 0 && ncboot < 20 )
          ERROR_exit("3dRSA: -cond_bootstrap needs at least 20 samples (or use 0 to disable)") ;
        nopt++ ; continue ;
      }
      if( strcasecmp(argv[nopt],"-cond_group") == 0 ){
        if( ++nopt >= argc ) ERROR_exit("3dRSA: need a file after -cond_group") ;
        cond_group_file=argv[nopt] ; nopt++ ; continue ;
      }
      if( strcasecmp(argv[nopt],"-boot_ci") == 0 ){
        if( ++nopt >= argc ) ERROR_exit("3dRSA: need an argument after -boot_ci") ;
        boot_ci = (float)rsa_parse_double("-boot_ci",argv[nopt],0.0,100.0) ;
        if( !(boot_ci > 0.0f && boot_ci < 100.0f) )
          ERROR_exit("3dRSA: -boot_ci must be greater than 0 and less than 100") ;
        boot_ci_given = 1 ;
        nopt++ ; continue ;
      }
      if( strcasecmp(argv[nopt],"-seed") == 0 ){
        if( ++nopt >= argc ) ERROR_exit("3dRSA: need an argument after -seed") ;
        seed = rsa_parse_long("-seed",argv[nopt],LONG_MIN,LONG_MAX) ; nopt++ ; continue ;
      }
      if( strcasecmp(argv[nopt],"-polort") == 0 ){
        if( ++nopt >= argc ) ERROR_exit("3dRSA: need an argument after -polort") ;
        polort = (int)rsa_parse_long("-polort",argv[nopt],-1,9) ;
        nopt++ ; continue ;
      }
      if( strcasecmp(argv[nopt],"-prefix") == 0 ){
        if( ++nopt >= argc ) ERROR_exit("3dRSA: need an argument after -prefix") ;
        prefix = argv[nopt] ;
        if( !THD_filename_ok(prefix) )
          ERROR_exit("3dRSA: illegal value '%s' after -prefix",prefix) ;
        nopt++ ; continue ;
      }
      if( strcasecmp(argv[nopt],"-save_rdm") == 0 ){
        if( ++nopt >= argc ) ERROR_exit("3dRSA: need an argument after -save_rdm") ;
        save_rdm = argv[nopt] ; nopt++ ; continue ;
      }
      if( strcasecmp(argv[nopt],"-no_dset")       == 0 ){ do_dset=0 ; nopt++ ; continue ; }
      if( strcasecmp(argv[nopt],"-quiet")         == 0 ){ quiet  =1 ; nopt++ ; continue ; }
      if( strcasecmp(argv[nopt],"-progress") == 0 ){
        if( ++nopt >= argc ) ERROR_exit("3dRSA: need auto, bar, line, or off after -progress") ;
             if( strcasecmp(argv[nopt],"auto")==0 ) progress_mode=RSA_PROGRESS_AUTO ;
        else if( strcasecmp(argv[nopt],"bar" )==0 ) progress_mode=RSA_PROGRESS_BAR ;
        else if( strcasecmp(argv[nopt],"line")==0 ) progress_mode=RSA_PROGRESS_LINE ;
        else if( strcasecmp(argv[nopt],"off" )==0 ) progress_mode=RSA_PROGRESS_OFF ;
        else ERROR_exit("3dRSA: -progress must be auto, bar, line, or off") ;
        nopt++ ; continue ;
      }
      if( strcasecmp(argv[nopt],"-model_joint")   == 0 ){ joint  =1 ; nopt++ ; continue ; }
      if( strcasecmp(argv[nopt],"-noise_ceiling") == 0 ){ do_nc  =1 ; nopt++ ; continue ; }
      if( strcasecmp(argv[nopt],"-loo")           == 0 ){ do_loo =1 ; nopt++ ; continue ; }
      if( strcasecmp(argv[nopt],"-block") == 0 ){
        if( ++nopt >= argc ) ERROR_exit("3dRSA: need a column name after -block") ;
        block_col = argv[nopt] ; nopt++ ; continue ;
      }
      if( strcasecmp(argv[nopt],"-nc_split") == 0 ){
        if( ++nopt >= argc ) ERROR_exit("3dRSA: need an argument after -nc_split") ;
             if( strcasecmp(argv[nopt],"half")       == 0 ) nc_split = NC_HALF ;
        else if( strcasecmp(argv[nopt],"interleave") == 0 ) nc_split = NC_INTERLEAVE ;
        else ERROR_exit("3dRSA: -nc_split must be 'half' or 'interleave'") ;
        nopt++ ; continue ;
      }

      if( strcasecmp(argv[nopt],"-model_label") == 0 ){
        if( ++nopt >= argc ) ERROR_exit("3dRSA: need a name after -model_label") ;
        if( pending_label != NULL )
          ERROR_exit("3dRSA: two -model_label in a row; each names ONE following\n"
                     "       -model / -model_mat / -model_dset") ;
        pending_label = argv[nopt] ; nopt++ ; continue ;
      }
      if( strcasecmp(argv[nopt],"-model") == 0 ){
        if( ++nopt >= argc ) ERROR_exit("3dRSA: need an argument after -model") ;
        modspec  = (char **)realloc(modspec ,sizeof(char *)*(nmodspec+1)) ;
        modlabel = (char **)realloc(modlabel,sizeof(char *)*(nmodspec+1)) ;
        modlabel[nmodspec] = pending_label ; pending_label = NULL ;
        modspec[nmodspec++] = argv[nopt] ; nopt++ ; continue ;
      }
      if( strcasecmp(argv[nopt],"-model_mat") == 0 ){
        if( ++nopt >= argc ) ERROR_exit("3dRSA: need an argument after -model_mat") ;
        matspec  = (char **)realloc(matspec ,sizeof(char *)*(nmatspec+1)) ;
        matlabel = (char **)realloc(matlabel,sizeof(char *)*(nmatspec+1)) ;
        matlabel[nmatspec] = pending_label ; pending_label = NULL ;
        matspec[nmatspec++] = argv[nopt] ; nopt++ ; continue ;
      }
      if( strcasecmp(argv[nopt],"-model_series") == 0 ){
        if( ++nopt >= argc ) ERROR_exit("3dRSA: need a list file after -model_series") ;
        if( series_file != NULL ) ERROR_exit("3dRSA: -model_series was given twice") ;
        series_file=argv[nopt] ; nopt++ ; continue ;
      }
      if( strcasecmp(argv[nopt],"-model_dset") == 0 ){
        if( ++nopt >= argc ) ERROR_exit("3dRSA: need an argument after -model_dset") ;
        dsespec  = (char **)realloc(dsespec ,sizeof(char *)*(ndsespec+1)) ;
        dselabel = (char **)realloc(dselabel,sizeof(char *)*(ndsespec+1)) ;
        dselabel[ndsespec] = pending_label ; pending_label = NULL ;
        dsespec[ndsespec++] = argv[nopt] ; nopt++ ; continue ;
      }
      if( strcasecmp(argv[nopt],"-model_contrast") == 0 ){
        if( ++nopt >= argc ) ERROR_exit("3dRSA: need 'A-B' after -model_contrast") ;
        constrspec = (char **)realloc(constrspec,sizeof(char *)*(nconstrspec+1)) ;
        constrspec[nconstrspec++] = argv[nopt] ; nopt++ ; continue ;
      }
      if( strcasecmp(argv[nopt],"-contrast_hypothesis") == 0 ){
        if( ++nopt >= argc )
          ERROR_exit("3dRSA: need superiority, alignment, or legacy after -contrast_hypothesis") ;
             if( strcasecmp(argv[nopt],"superiority") == 0 ) contrast_hypothesis=CONTRAST_SUPERIORITY ;
        else if( strcasecmp(argv[nopt],"alignment")   == 0 ) contrast_hypothesis=CONTRAST_ALIGNMENT ;
        else if( strcasecmp(argv[nopt],"legacy")      == 0 ) contrast_hypothesis=CONTRAST_LEGACY ;
        else ERROR_exit("3dRSA: -contrast_hypothesis must be superiority, alignment, or legacy") ;
        contrast_hypothesis_given=1 ; nopt++ ; continue ;
      }
      if( strcasecmp(argv[nopt],"-model_commonality") == 0 ){
        if( ++nopt >= argc ) ERROR_exit("3dRSA: need 'A,B' or 'A,B,C' after -model_commonality") ;
        comspec = (char **)realloc(comspec,sizeof(char *)*(ncomspec+1)) ;
        comspec[ncomspec++] = argv[nopt] ; nopt++ ; continue ;
      }
      if( strcasecmp(argv[nopt],"-model_fit") == 0 ){
        if( ++nopt >= argc ) ERROR_exit("3dRSA: need 'NAME=A,B,...' after -model_fit") ;
        fitspec=(char **)realloc(fitspec,sizeof(char *)*(nfitspec+1)) ;
        fitspec[nfitspec++]=argv[nopt] ; nopt++ ; continue ;
      }
      if( strcasecmp(argv[nopt],"-fit_ridge") == 0 ){
        double vv ;
        if( ++nopt >= argc ) ERROR_exit("3dRSA: need a nonnegative number after -fit_ridge") ;
        vv=rsa_parse_double("-fit_ridge",argv[nopt],0.0,FLT_MAX) ;
        fit_ridge=(float)vv ; fit_ridge_given=1 ; nopt++ ; continue ;
      }
      if( strcasecmp(argv[nopt],"-fit_condfold") == 0 ){
        if( ++nopt >= argc ) ERROR_exit("3dRSA: need a fold file after -fit_condfold") ;
        fit_condfold_file=argv[nopt] ; nopt++ ; continue ;
      }
      if( strcasecmp(argv[nopt],"-group_test") == 0 ){
        if( ++nopt >= argc ) ERROR_exit("3dRSA: need 'signflip' or 'signedrank' after -group_test") ;
             if( strcasecmp(argv[nopt],"signflip")  == 0 ) group_test = 0 ;
        else if( strcasecmp(argv[nopt],"signedrank") == 0 ) group_test = 1 ;
        else ERROR_exit("3dRSA: -group_test must be 'signflip' or 'signedrank'") ;
        nopt++ ; continue ;
      }
      if( strcasecmp(argv[nopt],"-classic_null") == 0 ){
        if( ++nopt >= argc ) ERROR_exit("3dRSA: need 'subjects' or 'conditions' after -classic_null") ;
             if( strcasecmp(argv[nopt],"subjects")   == 0 ) classic_null=CLASSIC_NULL_SUBJECTS ;
        else if( strcasecmp(argv[nopt],"conditions") == 0 ) classic_null=CLASSIC_NULL_CONDITIONS ;
        else ERROR_exit("3dRSA: -classic_null must be 'subjects' or 'conditions'") ;
        classic_null_given=1 ; nopt++ ; continue ;
      }
      if( strcasecmp(argv[nopt],"-ortvec") == 0 ){
        if( ++nopt >= argc ) ERROR_exit("3dRSA: need a column name after -ortvec") ;
        ortspec = (char **)realloc(ortspec,sizeof(char *)*(nortspec+1)) ;
        ortspec[nortspec++] = argv[nopt] ; nopt++ ; continue ;
      }

      if( strcasecmp(argv[nopt],"-dataTableFile") == 0 ){
        if( ++nopt >= argc ) ERROR_exit("3dRSA: need a file name after -dataTableFile") ;
        if( tab != NULL ) ERROR_exit("3dRSA: the data table was given twice") ;
        tab = THD_read_datatable_file( argv[nopt] ) ; nopt++ ; continue ;
      }
      if( strcasecmp(argv[nopt],"-dataTable") == 0 ){
        int nused=0 ;
        if( ++nopt >= argc ) ERROR_exit("3dRSA: need a table after -dataTable") ;
        if( tab != NULL ) ERROR_exit("3dRSA: the data table was given twice") ;
        tab = THD_read_datatable_args( argc , argv , nopt , rsa_options , &nused ) ;
        nopt += nused ; continue ;
      }
      if( strcasecmp(argv[nopt],"-condition_column") == 0 ){
        if( ++nopt >= argc ) ERROR_exit("3dRSA: need a column name after -condition_column") ;
        if( condition_column!=NULL ) ERROR_exit("3dRSA: -condition_column was given twice") ;
        condition_column=argv[nopt] ; nopt++ ; continue ;
      }
      if( strcasecmp(argv[nopt],"-condition_order") == 0 ){
        if( ++nopt >= argc ) ERROR_exit("3dRSA: need comma-separated labels after -condition_order") ;
        if( condition_order_arg!=NULL ) ERROR_exit("3dRSA: -condition_order was given twice") ;
        condition_order_arg=argv[nopt] ; nopt++ ; continue ;
      }
      if( strcasecmp(argv[nopt],"-runwiseTable") == 0 ){
        if( ++nopt >= argc ) ERROR_exit("3dRSA: need a file name after -runwiseTable") ;
        runwise_file = argv[nopt] ; nopt++ ; continue ;
      }
      if( strcasecmp(argv[nopt],"-run_column") == 0 ){
        if( ++nopt >= argc ) ERROR_exit("3dRSA: need a column name after -run_column") ;
        if( run_column!=NULL ) ERROR_exit("3dRSA: -run_column was given twice") ;
        run_column=argv[nopt] ; nopt++ ; continue ;
      }
      if( strcasecmp(argv[nopt],"-run_normalize") == 0 ){
        if( ++nopt >= argc ) ERROR_exit("3dRSA: need none/demean/zscore after -run_normalize") ;
             if( strcasecmp(argv[nopt],"none")==0 ) run_normalize=RUN_NORM_NONE ;
        else if( strcasecmp(argv[nopt],"demean")==0 ) run_normalize=RUN_NORM_DEMEAN ;
        else if( strcasecmp(argv[nopt],"zscore")==0 ) run_normalize=RUN_NORM_ZSCORE ;
        else ERROR_exit("3dRSA: -run_normalize must be none, demean, or zscore") ;
        run_normalize_given=1 ;
        nopt++ ; continue ;
      }
      if( strcasecmp(argv[nopt],"-run_analysis") == 0 ){
        if( ++nopt >= argc ) ERROR_exit("3dRSA: need concatenate/separate/mean after -run_analysis") ;
             if( strcasecmp(argv[nopt],"concatenate")==0 || strcasecmp(argv[nopt],"concat")==0 )
               run_analysis=RUN_ANALYSIS_CONCAT ;
        else if( strcasecmp(argv[nopt],"separate")==0 ) run_analysis=RUN_ANALYSIS_SEPARATE ;
        else if( strcasecmp(argv[nopt],"mean")==0 ) run_analysis=RUN_ANALYSIS_MEAN ;
        else ERROR_exit("3dRSA: -run_analysis must be concatenate, separate, or mean") ;
        run_analysis_given=1 ; nopt++ ; continue ;
      }
      if( strcasecmp(argv[nopt],"-run_model") == 0 ){
        if( ++nopt >= argc ) ERROR_exit("3dRSA: need COLUMN:NN or COLUMN:AnnaK after -run_model") ;
        runmodspec=(char **)realloc(runmodspec,sizeof(char *)*(nrunmodspec+1)) ;
        runmodspec[nrunmodspec++]=argv[nopt] ; nopt++ ; continue ;
      }
      if( strcasecmp(argv[nopt],"-run_center") == 0 ){
        if( ++nopt >= argc ) ERROR_exit("3dRSA: need COLUMN subject after -run_center") ;
        runcenter=(char **)realloc(runcenter,sizeof(char *)*(nruncenter+1)) ;
        runcenter[nruncenter++]=argv[nopt] ;
        if( ++nopt >= argc || strcasecmp(argv[nopt],"subject")!=0 )
          ERROR_exit("3dRSA: -run_center currently supports exactly 'COLUMN subject'") ;
        nopt++ ; continue ;
      }
      if( strcasecmp(argv[nopt],"-run_factor") == 0 ){
        if( ++nopt >= argc ) ERROR_exit("3dRSA: need a data-table column after -run_factor") ;
        runfactorspec=(char **)realloc(runfactorspec,sizeof(char *)*(nrunfactorspec+1)) ;
        runfactorspec[nrunfactorspec++]=argv[nopt] ; nopt++ ; continue ;
      }
      if( strcasecmp(argv[nopt],"-run_contrast") == 0 ){
        if( ++nopt >= argc )
          ERROR_exit("3dRSA: need NAME=FACTOR:POSITIVE-NEGATIVE after -run_contrast") ;
        runconspec=(char **)realloc(runconspec,sizeof(char *)*(nrunconspec+1)) ;
        runconspec[nrunconspec++]=argv[nopt] ; nopt++ ; continue ;
      }
      if( strcasecmp(argv[nopt],"-noise_norm") == 0 ){
        if( ++nopt >= argc ) ERROR_exit("3dRSA: need none/diag/shrinkage after -noise_norm") ;
             if( strcasecmp(argv[nopt],"none")      == 0 ) noise_norm = NN_NONE ;
        else if( strcasecmp(argv[nopt],"diag")      == 0 ) noise_norm = NN_DIAG ;
        else if( strcasecmp(argv[nopt],"shrinkage") == 0 ||
                 strcasecmp(argv[nopt],"shrink")     == 0 ) noise_norm = NN_SHRINK ;
        else ERROR_exit("3dRSA: -noise_norm must be none, diag or shrinkage") ;
        nopt++ ; continue ;
      }

      ERROR_message("3dRSA: illegal option '%s'",argv[nopt]) ;
      suggest_best_prog_option(argv[0],argv[nopt]) ;
      exit(1) ;
   }

   /*================== validate ==================*/

   if( !quiet && progress_mode!=RSA_PROGRESS_OFF )
     INFO_message("3dRSA [1/5] Validating options and loading inputs...") ;
#ifdef USE_OMP
   if( !quiet )
     INFO_message("3dRSA: using %d OpenMP worker%s (set OMP_NUM_THREADS to override)",
                  omp_get_max_threads(),(omp_get_max_threads()==1)?"":"s") ;
#else
   if( !quiet ) INFO_message("3dRSA: this build has no OpenMP support; using one worker") ;
#endif

   if( seed_roi_sel != NULL && seed_mask == NULL )
     ERROR_exit("3dRSA: -seed_roi selects a value from -seed_mask; give both options") ;
   if( seed_mask != NULL ){
     if( series_file != NULL || nmodspec+nmatspec+ndsespec+nrunmodspec>0 || pending_label!=NULL )
       ERROR_exit("3dRSA: -seed_mask defines the one representational-connectivity\n"
                  "       model. Do not combine it with -model, -model_mat,\n"
                  "       -model_series, -model_dset, or -model_label.") ;
     if( joint || nortspec>0 || nconstrspec>0 || ncomspec>0 || nfitspec>0 || do_loo )
       ERROR_exit("3dRSA: seed connectivity is one seed-to-target effect.\n"
                  "       -model_joint, -ortvec, -model_contrast, -model_commonality,\n"
                  "       -model_fit, and -loo require other model estimands and\n"
                  "       cannot be combined with -seed_mask.") ;
     if( ncboot>0 )
       ERROR_exit("3dRSA: -cond_bootstrap is not yet defined for seed connectivity.\n"
                  "       Both the subject-specific seed and target RDM must be\n"
                  "       resampled together; use -bootstrap for subject uncertainty.") ;
     if( cmp_metric==CMP_CORR_COV || cmp_metric==CMP_COS_COV )
       ERROR_exit("3dRSA: -metric %s assumes a fixed model RDM, but -seed_mask\n"
                  "       estimates both seed and target RDMs from noisy data. Use\n"
                  "       pearson/spearman/ktaub/ktaua/rhoa for seed connectivity.",
                  THD_simmat_cmp_label(cmp_metric)) ;
   }

   if( series_file != NULL ){
     if( nmodspec+nmatspec+ndsespec+nrunmodspec > 0 || pending_label != NULL )
       ERROR_exit("3dRSA: -model_series defines the complete ordered model set;\n"
                  "       do not combine it with -model, -model_mat, -model_dset,\n"
                  "       or -model_label") ;
     if( joint || nortspec>0 || nconstrspec>0 || ncomspec>0 || nfitspec>0 || do_loo )
       ERROR_exit("3dRSA: -model_series currently tests each time point separately\n"
                  "       with joint time x space FDR/FWE.  -model_joint, -ortvec,\n"
                  "       -model_fit, -model_contrast, -model_commonality, and -loo require a\n"
                  "       separately defined time-series statistic and are rejected.") ;
     rsa_read_model_series(series_file,&matspec,&matlabel,&series_time,&nseries) ;
     nmatspec=nseries ;
   }

   /* -mask is required, with one exception: a surface searchlight may omit it
      to search the WHOLE mesh.  Unlike a volume, a surface has no "not brain"
      -- every node is cortex -- so there is a coherent meaning to "search
      everywhere" that a volume (skull, ventricles, air) does not have; a
      volumetric searchlight still requires -mask. */
   if( maskname == NULL && !(surf_file != NULL && sl_nbhd != NULL) )
     ERROR_exit("3dRSA: -mask is required (the one exception is a surface\n"
                "       -searchlight, which may omit -mask to search the\n"
                "       whole mesh)") ;
   if( sl_nbhd == NULL && (memory_limit_given || memory_override) )
     ERROR_exit("3dRSA: -memory_limit/-memory_override apply only with -searchlight") ;

   /* Runwise (cross-validated) classic RSA takes a different input -- one row
      per subject x run -- so it has its own table.  Load and validate that
      table here; the same crossnobis estimator serves atlas ROIs and moving
      searchlight neighborhoods downstream. */
   if( runwise_file != NULL ){
     THD_3dim_dataset *mchk ;
     if( maskname == NULL )
       ERROR_exit("3dRSA: -mask is required with -runwiseTable (the mask-optional\n"
                  "       surface searchlight is IS-RSA only)") ;
     if( tab != NULL )
       ERROR_exit("3dRSA: use EITHER -runwiseTable OR -dataTable/-dataTableFile,\n"
                  "       not both") ;
     if( rdm_over != RDM_BRICK && feat_override != MODE_RDM )
       ERROR_exit("3dRSA: -runwiseTable supplies cross-validated condition RDMs.\n"
                  "       Use '-mode RSA', or '-mode IS-RSA -featuretype rdm'\n"
                  "       for second-order crossnobis IS-RSA.") ;
     runset = THD_runset_read( runwise_file ) ;
     if( runset == NULL )
       ERROR_exit("3dRSA: -runwiseTable '%s' failed to load (see message above)",
                  runwise_file) ;
     if( noise_norm != NN_NONE && !runset->has_resid )
       ERROR_exit("3dRSA: -noise_norm %s needs a 'ResidFile' column in the\n"
                  "       -runwiseTable to estimate the noise covariance",
                  (noise_norm==NN_DIAG)?"diag":"shrinkage") ;
     mchk = THD_open_dataset( maskname ) ;
     if( mchk == NULL ) ERROR_exit("3dRSA: can't open -mask '%s'",maskname) ;
     if( DSET_NVOX(mchk) != runset->nvox )
       ERROR_exit("3dRSA: -mask has %d voxels but the runwise datasets have %d",
                  DSET_NVOX(mchk),runset->nvox) ;
     if( !EQUIV_GRIDS(mchk,runset->betas[0]) )
       ERROR_exit("3dRSA: -mask and runwise datasets are on different grids;\n"
                  "       searchlight neighborhoods require matching geometry") ;
     DSET_delete(mchk) ;
     if( !quiet ){
       THD_runset_print( runset , stdout ) ;
       INFO_message("3dRSA: -runwiseTable -- %d subjects, %d conditions, %d "
                    "subject-runs; crossnobis (%s)",
                    runset->nsub,runset->ncond,runset->nrow,
                    (noise_norm==NN_NONE)   ? "cross-validated squared Euclidean" :
                    (noise_norm==NN_DIAG)   ? "univariate noise-normalized"
                                            : "multivariate noise-normalized (shrinkage)") ;
     }
   }

   if( noise_norm != NN_NONE && runset == NULL )
     ERROR_exit("3dRSA: -noise_norm only applies to cross-validated (crossnobis)\n"
                "       RSA; give it with -runwiseTable.") ;

   if( tab == NULL && runset == NULL )
     ERROR_exit("3dRSA: -dataTable, -dataTableFile or -runwiseTable is required") ;
   if( tab != NULL && tab->fname == NULL )
     ERROR_exit("3dRSA: the data table has no 'InputFile' column, so there are\n"
                "       no datasets to analyze") ;
   if( (condition_column==NULL) != (condition_order_arg==NULL) )
     ERROR_exit("3dRSA: -condition_column and -condition_order must be given together.\n"
                "       -condition_order is the row/column order of -model_mat.") ;
   if( condition_column!=NULL ){
     char *cols[2],**levels[2] ; int nlev[2]={THD_DT_LEVELS_LEXICAL,0},*first,ss,cc,jj2 ;
     THD_datatable *shorttab ;
     if( tab==NULL ) ERROR_exit("3dRSA: -condition_column requires -dataTable") ;
     if( runset!=NULL || run_column!=NULL )
       ERROR_exit("3dRSA: -condition_column is the ordinary classic-RSA long-table\n"
                  "       layout; do not combine it with -runwiseTable or -run_column") ;
     if( rdm_over!=RDM_BRICK )
       ERROR_exit("3dRSA: -condition_column currently applies to traditional '-mode RSA'") ;
     if( tab->icol_subj<0 )
       ERROR_exit("3dRSA: -condition_column requires an explicit 'Subj' column") ;
     cc=THD_datatable_column(tab,condition_column) ;
     if( cc<0 ) ERROR_exit("3dRSA: -condition_column '%s' is not in the data table",
                           condition_column) ;
     if( cc==tab->icol_subj || cc==tab->icol_input )
       ERROR_exit("3dRSA: -condition_column must name a separate condition-label column") ;
     condition_level=rsa_split_csv_labels(condition_order_arg,&ncondition_level) ;
     if( ncondition_level<2 )
       ERROR_exit("3dRSA: -condition_order needs at least two comma-separated labels") ;
     cols[0]="Subj" ; cols[1]=condition_column ;
     levels[0]=NULL ; levels[1]=condition_level ; nlev[1]=ncondition_level ;
     condition_index=THD_datatable_index_columns(tab,2,cols,nlev,levels) ;

     /* Subject-level metadata must not depend on which arbitrary condition row
        happens to be selected when the long table is collapsed. */
     for( ss=0 ; ss<condition_index->nlevel[0] ; ss++ ){
       int fr=condition_index->row_of[ss*ncondition_level] ;
       for( cc=0 ; cc<ncondition_level ; cc++ ){
         int rr0=condition_index->row_of[ss*ncondition_level+cc] ;
         for( jj2=0 ; jj2<tab->ncol ; jj2++ )
           if( jj2!=tab->icol_subj && jj2!=tab->icol_input &&
               jj2!=condition_index->icol[1] &&
               strcmp(DT_CELL(tab,fr,jj2),DT_CELL(tab,rr0,jj2))!=0 )
             ERROR_exit("3dRSA: column '%s' changes within Subj %s across conditions\n"
                        "       ('%s' vs '%s'). Only InputFile and %s may vary.",
                        tab->cname[jj2],condition_index->level[0][ss],
                        DT_CELL(tab,fr,jj2),DT_CELL(tab,rr0,jj2),condition_column) ;
       }
     }
     first=(int *)malloc(sizeof(int)*condition_index->nlevel[0]) ;
     for( ss=0 ; ss<condition_index->nlevel[0] ; ss++ )
       first[ss]=condition_index->row_of[ss*ncondition_level] ;
     longtab=tab ;
     shorttab=THD_datatable_select_rows(tab,first,condition_index->nlevel[0]) ;
     free(first) ;
     if( shorttab==NULL ) ERROR_exit("3dRSA: could not reduce the condition table") ;
     tab=shorttab ;
   }
   if( nrunfactorspec>0 || nrunconspec>0 ){
     int aa,bb ;
     if( run_column==NULL ) ERROR_exit("3dRSA: -run_factor/-run_contrast require -run_column") ;
     if( nrunfactorspec==0 ) ERROR_exit("3dRSA: -run_contrast requires a declared -run_factor") ;
     if( nrunconspec==0 ) ERROR_exit("3dRSA: -run_factor was given without a -run_contrast") ;
     for( aa=0 ; aa<nrunfactorspec ; aa++ ) for( bb=0 ; bb<aa ; bb++ )
       if( strcasecmp(runfactorspec[aa],runfactorspec[bb])==0 )
         ERROR_exit("3dRSA: duplicate -run_factor '%s'",runfactorspec[aa]) ;
     for( aa=0 ; aa<nrunfactorspec ; aa++ )
       if( rsa_run_model_column(runfactorspec[aa],runmodspec,nrunmodspec) )
         ERROR_exit("3dRSA: column '%s' cannot be both -run_factor metadata and a subject-varying -run_model",
                    runfactorspec[aa]) ;
   }
   if( nrunmodspec>0 || nruncenter>0 ){
     int aa,bb ;
     if( run_column==NULL )
       ERROR_exit("3dRSA: -run_model/-run_center require a repeated long table and -run_column") ;
     for( aa=0 ; aa<nrunmodspec ; aa++ ){
       char *co=strrchr(runmodspec[aa],':') ; size_t nc ; int centered=0 ;
       if( co==NULL || co==runmodspec[aa] || co[1]=='\0' )
         ERROR_exit("3dRSA: -run_model '%s' must be COLUMN:NN or COLUMN:AnnaK",runmodspec[aa]) ;
       if( strcasecmp(co+1,"nn")!=0 && strcasecmp(co+1,"annak")!=0 )
         ERROR_exit("3dRSA: Stage-3 -run_model rule '%s' is not supported; use NN or AnnaK",co+1) ;
       nc=(size_t)(co-runmodspec[aa]) ;
       for( bb=0 ; bb<nruncenter ; bb++ )
         if( strlen(runcenter[bb])==nc && strncasecmp(runcenter[bb],runmodspec[aa],nc)==0 ) centered=1 ;
       nrunmod += centered ? 2 : 1 ;
       for( bb=0 ; bb<aa ; bb++ ) if( strcasecmp(runmodspec[aa],runmodspec[bb])==0 )
         ERROR_exit("3dRSA: duplicate -run_model '%s'",runmodspec[aa]) ;
     }
     for( aa=0 ; aa<nruncenter ; aa++ ){
       int found=0 ;
       for( bb=0 ; bb<nrunmodspec ; bb++ ){
         char *co=strrchr(runmodspec[bb],':') ; size_t nc=(size_t)(co-runmodspec[bb]) ;
         if( strlen(runcenter[aa])==nc && strncasecmp(runcenter[aa],runmodspec[bb],nc)==0 ) found=1 ;
       }
       if( !found ) ERROR_exit("3dRSA: -run_center column '%s' has no matching -run_model",runcenter[aa]) ;
       for( bb=0 ; bb<aa ; bb++ ) if( strcasecmp(runcenter[aa],runcenter[bb])==0 )
         ERROR_exit("3dRSA: duplicate -run_center for '%s'",runcenter[aa]) ;
     }
   }
   if( run_column!=NULL ){
     THD_datatable *shorttab ;
     if( runset!=NULL ) ERROR_exit("3dRSA: -run_column applies to -dataTable, not -runwiseTable") ;
     series_runs=rsa_series_runs_build(tab,run_column,runmodspec,nrunmodspec,
                                       runfactorspec,nrunfactorspec) ;
     longtab=tab ;
     if( nrunfactorspec>0 ){
       int rf,rc,old ;
       runfactor=(RSA_runfactor *)calloc(nrunfactorspec,sizeof(RSA_runfactor)) ;
       for( rf=0 ; rf<nrunfactorspec ; rf++ )
         runfactor[rf]=rsa_runfactor_build(longtab,series_runs,runfactorspec[rf]) ;
       runcon=(RSA_runcontrast *)calloc(nrunconspec,sizeof(RSA_runcontrast)) ;
       for( rc=0 ; rc<nrunconspec ; rc++ ){
         runcon[rc]=rsa_runcontrast_build(runconspec[rc],runfactor,nrunfactorspec,series_runs) ;
         { int ru,conflict=(strcasecmp(runcon[rc].name,"MEAN")==0) ;
           for( ru=0 ; ru<series_runs->nrun ; ru++ )
             if( strcasecmp(runcon[rc].name,series_runs->run_label[ru])==0 ) conflict=1 ;
           if( conflict ) ERROR_exit("3dRSA: -run_contrast name '%s' conflicts with a reserved/run summary label",
                                     runcon[rc].name) ; }
         for( old=0 ; old<rc ; old++ ) if( strcasecmp(runcon[old].name,runcon[rc].name)==0 )
           ERROR_exit("3dRSA: duplicate -run_contrast name '%s'",runcon[rc].name) ;
       }
       rcon_weight=(float *)malloc(sizeof(float)*(size_t)nrunconspec*series_runs->nrun) ;
       for( rc=0 ; rc<nrunconspec ; rc++ )
         memcpy(rcon_weight+(size_t)rc*series_runs->nrun,runcon[rc].weight,
                sizeof(float)*series_runs->nrun) ;
     }
     shorttab=THD_datatable_select_rows(tab,series_runs->first_row,series_runs->nsub) ;
     if( shorttab==NULL ) ERROR_exit("3dRSA: could not reduce the repeated-run data table") ;
     tab=shorttab ;
   }
   nsub = (runset != NULL) ? runset->nsub : tab->nrow ;

   /* derive the feature type: RSA always uses voxel patterns; IS-RSA uses the
      ROI-mean time course unless -featuretype pattern asks otherwise */
   if( rdm_over == RDM_BRICK ){
     if( feat_override == MODE_CONT || feat_override == MODE_RDM )
       ERROR_exit("3dRSA: -featuretype %s makes no sense with -mode RSA;\n"
                  "       classic RSA compares conditions by their spatial pattern.",
                  (feat_override==MODE_RDM)?"rdm":"mean") ;
     mode = MODE_BETA ;
   } else {
     mode = (feat_override >= 0) ? feat_override : MODE_CONT ;
   }

   if( series_runs!=NULL ){
     if( rdm_over!=RDM_SUBJ || mode!=MODE_CONT )
       ERROR_exit("3dRSA: -run_column currently supports '-mode IS-RSA -featuretype mean'.\n"
                  "       Run-specific condition estimates use the distinct -runwiseTable contract.") ;
     if( null_mode!=NULL_LABELS )
       ERROR_exit("3dRSA: -run_column currently supports '-null labels'. Timeshift/phase\n"
                  "       nulls must preserve run boundaries and are not silently concatenated.") ;
     if( ndsespec>0 ) ERROR_exit("3dRSA: -model_dset is not yet run-aware; do not combine it with -run_column") ;
     if( seed_mask!=NULL ) ERROR_exit("3dRSA: -seed_mask is not yet run-aware; do not combine it with -run_column") ;
     if( do_nc ) ERROR_exit("3dRSA: -noise_ceiling with -run_column needs a run-aware reliability\n"
                            "       estimand; it is not inferred by splitting the concatenated series") ;
     run_resolved=(run_analysis!=RUN_ANALYSIS_CONCAT) ;
     if( nrunmodspec>0 && !run_resolved )
       ERROR_exit("3dRSA: -run_model needs '-run_analysis separate' or '-run_analysis mean';\n"
                  "       a run-varying model has no single concatenated-run RDM estimand") ;
     if( nrunconspec>0 && !run_resolved )
       ERROR_exit("3dRSA: -run_contrast needs '-run_analysis separate' or '-run_analysis mean'") ;
     if( run_resolved ){
       if( series_runs->nrun<2 )
         ERROR_exit("3dRSA: -run_analysis %s needs at least two labeled runs",
                    run_analysis==RUN_ANALYSIS_MEAN?"mean":"separate") ;
       if( nortspec>0 || nconstrspec>0 || ncomspec>0 || nfitspec>0 ||
           do_loo || nboot>0 || ncboot>0 || series_file!=NULL || save_rdm!=NULL )
         ERROR_exit("3dRSA: run-resolved separate/mean supports fixed models,\n"
                    "       -run_model effects, planned -run_contrast effects, and\n"
                    "       Stage-5 conditional coefficients via -model_joint.\n"
                    "       -ortvec, model contrasts/commonality/fitting/LOO, bootstrap,\n"
                    "       model_series, and -save_rdm need later contracts.") ;
     }
   } else if( run_normalize_given || run_analysis_given || nrunmodspec>0 || nruncenter>0 ||
              nrunfactorspec>0 || nrunconspec>0 ){
     ERROR_exit("3dRSA: run normalization/analysis/model/design options need -run_column") ;
   }

   /* A flattened [sub-brick][voxel] pattern has no scientifically meaningful
      split-half axis: half cuts conditions (or a condition), while interleave
      mostly cuts neighboring voxels.  A valid reliability estimate needs
      matched repetitions/runs, which this input contract does not carry. */
   if( do_nc && rdm_over == RDM_SUBJ && mode == MODE_BETA )
     ERROR_exit("3dRSA: -noise_ceiling has no valid reliability split with\n"
                "       '-featuretype pattern'.  The flattened [sub-brick][voxel]\n"
                "       vector has no matched-repetition axis: 'half' confounds\n"
                "       condition identity and 'interleave' mostly splits\n"
                "       neighboring voxels.  Drop -noise_ceiling (or use\n"
                "       '-featuretype mean' for continuous data).") ;
   if( do_nc && rdm_over == RDM_SUBJ && mode == MODE_RDM )
     ERROR_exit("3dRSA: -noise_ceiling is not yet defined for '-featuretype rdm'.\n"
                "       Its triangle cannot be divided into matched condition\n"
                "       geometries.  Use an explicit repeated-condition contract\n"
                "       rather than splitting arbitrary RDM entries.") ;
   if( do_nc && rdm_over == RDM_BRICK && nsub < 2 )
     ERROR_exit("3dRSA: classic-RSA -noise_ceiling needs at least 2 subjects so\n"
                "       the leave-one-subject-out lower bound is defined") ;

   if( cond_metric_given && mode != MODE_RDM )
     ERROR_exit("3dRSA: -condition_metric applies only to second-order\n"
                "       '-mode IS-RSA -featuretype rdm'.") ;
   if( cond_metric_given && runset != NULL )
     ERROR_exit("3dRSA: -condition_metric is not used with -runwiseTable;\n"
                "       the inner condition RDM estimator there is crossnobis.") ;

   if( center_conditions && runset != NULL )
     ERROR_exit("3dRSA: '-center_conditions subject' currently applies only to\n"
                "       ordinary condition patterns from -dataTable. Crossnobis\n"
                "       already cancels a common pattern within each run's condition\n"
                "       contrasts; a future partition-wise preprocessing contract\n"
                "       will be named separately rather than implied here.") ;
   if( center_conditions && rdm_over != RDM_BRICK && mode != MODE_RDM )
     ERROR_exit("3dRSA: '-center_conditions subject' needs an ordinary condition\n"
                "       RDM: use '-mode RSA', or '-mode IS-RSA -featuretype rdm'.\n"
                "       It does not center continuous or flattened subject features.") ;

   if( runset == NULL && (rdm_over==RDM_BRICK || mode==MODE_RDM) && !quiet ){
     int imetric=(rdm_over==RDM_BRICK) ? neu_metric : cond_metric ;
     if( center_conditions )
       INFO_message("3dRSA: removing each subject's voxelwise mean pattern across\n"
                    "       conditions before ordinary condition-RDM construction%s.",
                    (imetric==SIM_EUCLID) ?
                    " (Euclidean differences are invariant, so no numeric transform is needed)" : "") ;
     else if( imetric==SIM_PEARSON || imetric==SIM_SPEARMAN || imetric==SIM_COSINE )
       INFO_message("3dRSA: ordinary angle-based condition RDMs use raw, uncentered\n"
                    "       patterns (compatibility default). Consider\n"
                    "       '-center_conditions subject' when a shared mean pattern\n"
                    "       is nuisance rather than part of the representation.") ;
   }

   if( runset != NULL && rdm_over == RDM_SUBJ ){
     if( mode != MODE_RDM )
       ERROR_exit("3dRSA: IS-RSA -runwiseTable requires '-featuretype rdm'.") ;
     if( nmodspec > 0 || ndsespec > 0 || nortspec > 0 || block_col != NULL || do_loo )
       ERROR_exit("3dRSA: runwise second-order IS-RSA currently uses fixed\n"
                  "       subject-by-subject -model_mat matrices.  Its runwise\n"
                  "       table has no subject-level columns for -model,\n"
                  "       -model_dset, -ortvec, -block, or -loo.") ;
   }

   if( rdm_over == RDM_BRICK ){
     if( nmatspec == 0 && seed_mask == NULL )
       ERROR_exit("3dRSA: -mode RSA needs -model_mat, -model_series, or -seed_mask.\n"
                  "       The rows are conditions, so a behavioral model cannot\n"
                  "       come from a -dataTable column.") ;
     if( nmodspec > 0 )
       ERROR_exit("3dRSA: -model builds a subject-by-subject matrix, so it\n"
                  "       cannot be used with -mode RSA.  Use -model_mat.") ;
     if( ndsespec > 0 )
       ERROR_exit("3dRSA: -model_dset builds a subject-by-subject matrix, so it\n"
                  "       cannot be used with -mode RSA.") ;
     if( nortspec > 0 )
       ERROR_exit("3dRSA: -ortvec removes a per-subject nuisance, which only\n"
                  "       applies to IS-RSA (-mode IS-RSA), where the matrix rows\n"
                  "       are subjects.") ;
   } else {
     if( nmodspec + nmatspec + ndsespec + nrunmod == 0 && seed_mask == NULL )
       ERROR_exit("3dRSA: no models given.  Use -model COLUMN:RULE, -model_mat\n"
                  "       for an explicit matrix, -model_series for a time-indexed\n"
                  "       matrix stack, -model_dset for another modality, -run_model\n"
                  "       for a run-varying behavioral column, or\n"
                  "       -seed_mask for representational connectivity.") ;
   }

   nmod = nmodspec + nrunmod + nmatspec + ndsespec + ((seed_mask!=NULL)?1:0) ;
   if( fit_ridge_given && nfitspec==0 )
     ERROR_exit("3dRSA: -fit_ridge was given without a -model_fit request") ;
   if( fit_condfold_file!=NULL && nfitspec==0 )
     ERROR_exit("3dRSA: -fit_condfold was given without a -model_fit request") ;
   if( nfitspec>0 ){
     if( cmp_metric != CMP_PEARSON )
       ERROR_exit("3dRSA: -model_fit requires '-metric pearson'.  Fitted component\n"
                  "       weights act on continuous RDM entries; rank metrics do not\n"
                  "       preserve the weighted prediction estimand.") ;
     if( joint || nortspec>0 )
       ERROR_exit("3dRSA: -model_fit is a constrained component-mixture model and\n"
                  "       cannot yet be combined with -model_joint or -ortvec.\n"
                  "       Define nuisance-aware nested fitting as a separate model.") ;
     if( null_mode != NULL_LABELS )
       ERROR_exit("3dRSA: -model_fit currently uses the complete nested label null;\n"
                  "       it cannot be combined with -null timeshift or -null phase.") ;
     if( nsub < 3 )
       ERROR_exit("3dRSA: -model_fit needs at least 3 subjects for held-subject fitting") ;
     if( fit_condfold_file!=NULL ){
       if( rdm_over!=RDM_BRICK )
         ERROR_exit("3dRSA: -fit_condfold applies only to classic '-mode RSA'.\n"
                    "       IS-RSA fitted models have subjects, not stimuli, on their RDM axis.") ;
     }
   }
   if( min_shift_given && null_mode != NULL_TIMESHIFT )
     ERROR_exit("3dRSA: -min_shift only applies with '-null timeshift'") ;
   if( null_mode == NULL_TIMESHIFT ){
     if( rdm_over != RDM_SUBJ || mode != MODE_CONT )
       ERROR_exit("3dRSA: '-null timeshift' requires continuous IS-RSA:\n"
                  "       use '-mode IS-RSA -featuretype mean'") ;
     if( nperm < 20 )
       ERROR_exit("3dRSA: '-null timeshift' needs at least 20 shifts via -nperm") ;
     if( ncomspec > 0 || do_loo )
       ERROR_exit("3dRSA: '-null timeshift' does not yet define commonality or\n"
                  "       LOO shifted-null statistics.  Primary, joint/nuisance\n"
                  "       regression, and model contrasts are supported.") ;
     if( block_col != NULL )
       ERROR_exit("3dRSA: -block restricts subject-label exchangeability, but\n"
                  "       '-null timeshift' does not relabel subjects.  Drop -block;\n"
                  "       a blocked time-shift design needs a separate contract.") ;
   }
   if( null_mode == NULL_PHASE ){
     if( rdm_over != RDM_SUBJ || mode != MODE_CONT )
       ERROR_exit("3dRSA: '-null phase' requires continuous IS-RSA:\n"
                  "       use '-mode IS-RSA -featuretype mean'") ;
     if( nperm < 20 )
       ERROR_exit("3dRSA: '-null phase' needs at least 20 phase sets via -nperm") ;
     if( ncomspec > 0 || do_loo )
       ERROR_exit("3dRSA: '-null phase' does not yet define commonality or LOO\n"
                  "       phase-null statistics.  Primary, joint/nuisance\n"
                  "       regression, and model contrasts are supported.") ;
     if( block_col != NULL )
       ERROR_exit("3dRSA: -block restricts subject-label exchangeability, but\n"
                  "       '-null phase' does not relabel subjects.  Drop -block;") ;
   }
   dualboot=(nboot>0 && ncboot>0) ;
   if( boot_ci_given && nboot == 0 && ncboot == 0 )
     ERROR_exit("3dRSA: -boot_ci changes a bootstrap interval; also give\n"
                "       -bootstrap N or -cond_bootstrap N") ;
   if( dualboot && nboot != ncboot )
     ERROR_exit("3dRSA: dual subject x condition bootstrap needs the same number\n"
                "       of synchronized draws on both axes; give equal N values\n"
                "       to -bootstrap and -cond_bootstrap") ;
   if( ncboot > 0 && rdm_over != RDM_BRICK )
     ERROR_exit("3dRSA: -cond_bootstrap resamples the condition axis and requires\n"
                "       classic '-mode RSA'.  IS-RSA matrix rows are subjects.") ;
   if( cond_group_file != NULL && ncboot == 0 )
     ERROR_exit("3dRSA: -cond_group describes -cond_bootstrap samples; also give\n"
                "       -cond_bootstrap N") ;
   if( dualboot && (ncomspec>0 || nfitspec>0 || series_file!=NULL || do_nc) )
     ERROR_exit("3dRSA: the first dual-bootstrap contract covers fixed primary\n"
                "       models, joint regression, and paired fixed-model contrasts.\n"
                "       Commonality, fitted models, -model_series, and noise-ceiling\n"
                "       intervals need separately defined two-factor estimands") ;
   if( joint && nmod < 2 && nortspec == 0 )
     WARNING_message("3dRSA: -model_joint with only one model is the same as\n"
                     "       testing it on its own") ;

   if( (joint || nortspec > 0) &&
       (cmp_metric == CMP_KTAUB || cmp_metric == CMP_KTAUA) )
     ERROR_exit("3dRSA: Kendall's tau cannot be used with -model_joint or\n"
                "       -ortvec, which fit by least squares.  Use spearman\n"
                "       (ranks, the default) or pearson.") ;
   if( (joint || nortspec > 0) && cmp_metric == CMP_RHOA )
     ERROR_exit("3dRSA: expected Spearman rho-a is a scalar RDM comparator and\n"
                "       cannot be used with -model_joint or -ortvec least-squares\n"
                "       fits.  Use -metric spearman for rank-based regression.") ;
   if( run_resolved && joint && cmp_metric!=CMP_PEARSON && cmp_metric!=CMP_SPEARMAN )
     ERROR_exit("3dRSA: run-resolved -model_joint supports standardized Pearson\n"
                "       or rank-based Spearman regression. Use -metric pearson or spearman.") ;

   if( do_loo && rdm_over != RDM_SUBJ )
     ERROR_exit("3dRSA: -loo predicts subject variables and therefore\n"
                "       needs -mode IS-RSA.  For the leave-one-subject-out lower\n"
                "       noise ceiling in classic/runwise RSA, use -noise_ceiling\n"
                "       and read nc_low.") ;

   if( classic_null_given && rdm_over != RDM_BRICK )
     ERROR_exit("3dRSA: -classic_null applies only to classic '-mode RSA'.\n"
                "       IS-RSA selects subject-label or temporal nulls with -null.") ;
   if( rdm_over == RDM_BRICK && classic_null == CLASSIC_NULL_SUBJECTS && nsub<2 )
     ERROR_exit("3dRSA: '-classic_null subjects' needs at least 2 independent\n"
                "       subjects for population-level inference. For a fixed-effects\n"
                "       analysis of this observed sample, use '-classic_null conditions'.") ;
   if( classic_null == CLASSIC_NULL_CONDITIONS ){
     if( nperm<=0 )
       ERROR_exit("3dRSA: '-classic_null conditions' needs -nperm > 0; there is no\n"
                  "       parametric replacement for the condition-label null.") ;
     if( joint )
       ERROR_exit("3dRSA: '-classic_null conditions' does not yet support\n"
                  "       -model_joint. A conditional coefficient needs a reviewed\n"
                  "       predictor-specific reduced-model condition null. Test fixed\n"
                  "       models separately or use '-classic_null subjects'.") ;
     if( group_test != 0 )
       ERROR_exit("3dRSA: -group_test signedrank applies to subject-level paired\n"
                  "       contrasts and cannot be combined with '-classic_null conditions'.") ;
   }

   /* Classic RSA either sign-flips subject scores or relabels the condition
      axis.  Neither operation permutes subject labels. Exchangeability blocks
      restrict which subject observations may TRADE PLACES, so a -block column
      would be silently ignored here. Reject it rather than let a methods
      section claim a blocking that did nothing. */
   if( block_col != NULL && rdm_over != RDM_SUBJ )
     ERROR_exit("3dRSA: -block is only meaningful for -mode IS-RSA, whose null\n"
                "       permutes subject labels. -mode RSA instead sign-flips subject\n"
                "       scores or relabels conditions; blocks restrict neither null;\n"
                "       drop -block (or switch to -mode IS-RSA).") ;

   /* -group_test picks the classic-RSA paired test; IS-RSA compares the two
      Mantel statistics under the shared relabeling and has no such choice. */
   if( group_test != 0 && rdm_over == RDM_SUBJ && !quiet )
     WARNING_message("3dRSA: -group_test signedrank applies to -mode RSA; IS-RSA\n"
                     "       contrasts use the paired Mantel-difference permutation.") ;

   if( seed == 0 ){
     seed = (long)time(NULL) + (long)getpid() ;
     if( (nperm > 0 || nboot > 0 || ncboot > 0) && !quiet )
       INFO_message("3dRSA: random seed = %ld (pass -seed %ld to repeat)",seed,seed) ;
   }

   /*================== atlas ==================*/

   if( !quiet && progress_mode!=RSA_PROGRESS_OFF )
     INFO_message("3dRSA [2/5] Building the atlas/searchlight domain...") ;

   if( maskname != NULL ){
     mset = THD_open_dataset( maskname ) ;
     if( mset == NULL ) ERROR_exit("3dRSA: can't open -mask dataset '%s'",maskname) ;
   } else {
     /* no -mask: surface searchlight over the whole mesh (validated above to be
        the only case that can reach here).  Borrow the first InputFile purely
        for its node geometry/domain -- the surface builder is told (all_nodes) not
        to read its values as a mask, so what the data happens to contain does
        not matter. */
     mset = THD_open_dataset( tab->fname[0] ) ;
     if( mset == NULL )
       ERROR_exit("3dRSA: no -mask, and can't open '%s' to borrow its surface\n"
                  "       geometry",tab->fname[0]) ;
     if( !quiet )
       INFO_message("3dRSA: no -mask -- searching the WHOLE surface mesh (domain\n"
                    "       taken from '%s')",tab->fname[0]) ;
   }
   DSET_load(mset) ; CHECK_LOAD_ERROR(mset) ;
   if( maskname!=NULL ) rsa_validate_mask(mset,sl_nbhd==NULL,"-mask") ;
   nvox = DSET_NVOX(mset) ;

   if( sl_nbhd != NULL ){
     MCW_cluster *nbhd ;
     streaming = 1 ;
     if( rdm_over != RDM_SUBJ && runset == NULL )
       WARNING_message("3dRSA: ordinary same-data classic-RSA searchlight.  Each\n"
                       "       condition RDM is estimated and evaluated from the\n"
                       "       same condition patterns; it is not cross-validated\n"
                       "       and does not have crossnobis's unbiased zero point.\n"
                       "       Prefer -runwiseTable when independent repeated\n"
                       "       condition estimates are available.") ;
     if( save_rdm != NULL ){
       WARNING_message("3dRSA: -save_rdm is ignored under -searchlight "
                       "(would write one matrix per voxel)") ;
       save_rdm = NULL ;
     }
     if( surf_file != NULL ){                    /* geodesic surface searchlight */
#ifdef USE_SUMA
       float rad = (float)rsa_parse_double("-searchlight",sl_nbhd,DBL_MIN,FLT_MAX) ;
       char FuncName[]="3dRSA" ;
       char spatial_err[1024] ;
       if( rad <= 0.0f )
         ERROR_exit("3dRSA: with -surf, -searchlight takes a geodesic radius in\n"
                    "       mm (a plain number), not '%s'",sl_nbhd) ;
       SUMA_STANDALONE_INIT ;
       SUMAg_DOv = SUMA_Alloc_DisplayObject_Struct( SUMA_MAX_DISPLAYABLE_OBJECTS ) ;
       if( !quiet )
         INFO_message("3dRSA: geodesic surface searchlight, radius %.3g mm on '%s'",
                      rad , surf_file) ;
       rl = THD_roilist_searchlight_surf( surf_file , mset , rad,
                                           (maskname == NULL),
                                           spatial_err,sizeof(spatial_err) ) ;
       if( rl == NULL && spatial_err[0] != '\0' )
         ERROR_exit("3dRSA: %s",spatial_err) ;
       if( rl == NULL ) ERROR_exit("3dRSA: no in-mask nodes for the searchlight") ;
#else
       ERROR_exit("3dRSA: -surf (geodesic surface searchlight) needs a build with\n"
                  "       surface support: compile with -DUSE_SUMA and link libSUMA.") ;
#endif
     } else {                                    /* volumetric sphere searchlight */
       char spatial_err[1024] ;
       nbhd = THD_searchlight_parse( sl_nbhd , fabsf(DSET_DX(mset)) ,
                                     fabsf(DSET_DY(mset)) , fabsf(DSET_DZ(mset)),
                                     spatial_err,sizeof(spatial_err) ) ;
       if( nbhd == NULL ) ERROR_exit("3dRSA: %s",spatial_err) ;
       if( !quiet )
         INFO_message("3dRSA: searchlight neighborhood '%s' = %d voxels",
                      sl_nbhd , nbhd->num_pt) ;
       rl = THD_roilist_searchlight( mset , nbhd ) ;
       KILL_CLUSTER(nbhd) ;
       if( rl == NULL ) ERROR_exit("3dRSA: -mask '%s' has no non-zero voxels",maskname) ;
     }
   } else {
     if( surf_file != NULL )
       ERROR_exit("3dRSA: -surf is only used with -searchlight (geodesic radius)") ;
     rl = THD_roilist_from_dset( mset , roi_sel ) ;
     if( rl == NULL ){
       if( roi_sel != NULL )
         ERROR_exit("3dRSA: -roi_sel '%s' selected no ROIs from '%s'",roi_sel,maskname) ;
       ERROR_exit("3dRSA: -mask dataset '%s' has no positive values",maskname) ;
     }
   }

   /* S5 representational connectivity: a seed is a separate, fixed spatial
      unit on the same grid/domain as the targets.  Select exactly one parcel,
      then remove every target that contains any seed feature before sizing the
      searched family. */
   if( seed_mask != NULL ){
     seedset=THD_open_dataset(seed_mask) ;
     if( seedset==NULL ) ERROR_exit("3dRSA: can't open -seed_mask dataset '%s'",seed_mask) ;
     if( DSET_NVOX(seedset)!=nvox || !EQUIV_GRIDS(mset,seedset) )
       ERROR_exit("3dRSA: -seed_mask '%s' must match the target mask/data grid",seed_mask) ;
     DSET_load(seedset) ; CHECK_LOAD_ERROR(seedset) ;
     rsa_validate_mask(seedset,1,"-seed_mask") ;
     seedrl=THD_roilist_from_dset(seedset,seed_roi_sel) ;
     if( seedrl==NULL )
       ERROR_exit("3dRSA: -seed_mask '%s'%s selected no positive ROI",
                  seed_mask,(seed_roi_sel!=NULL)?" / -seed_roi":"") ;
     if( seedrl->nroi!=1 )
       ERROR_exit("3dRSA: -seed_mask '%s' selected %d ROIs; seed connectivity\n"
                  "       needs exactly one. Add '-seed_roi VALUE' to select it.",
                  seed_mask,seedrl->nroi) ;
     if( seedrl->vox[0].nar<2 )
       ERROR_exit("3dRSA: seed ROI %d in '%s' has only %d voxel/node; need at least\n"
                  "       2 features for the complete RSA metric set.",
                  seedrl->val[0],seed_mask,seedrl->vox[0].nar) ;
     seed_excluded=rsa_roilist_exclude_seed(rl,seedrl->vox,nvox) ;
     if( rl->nroi<1 )
       ERROR_exit("3dRSA: every target ROI/searchlight overlaps seed ROI %d.\n"
                  "       Seed-to-self/shared-feature cells are excluded; provide\n"
                  "       a target -mask with at least one non-overlapping location.",
                  seedrl->val[0]) ;
     if( !quiet )
       INFO_message("3dRSA: seed ROI %d%s%s%s: %d voxels/nodes; excluded %d of %d\n"
                    "       target locations because they overlap the seed",
                    seedrl->val[0],(seedrl->lab[0]!=NULL)?" (":"",
                    (seedrl->lab[0]!=NULL)?seedrl->lab[0]:"",
                    (seedrl->lab[0]!=NULL)?")":"",
                    seedrl->vox[0].nar,seed_excluded,rl->nroi+seed_excluded) ;
   }
   nroi = rl->nroi ;

   if( !quiet ){
     if( series_runs!=NULL )
       INFO_message("3dRSA: %d %s in '%s', %d subjects x %d runs (%d table rows)",
                    nroi,streaming?"searchlight spheres":"ROIs",
                    (maskname!=NULL)?maskname:"(whole surface mesh)",
                    nsub,series_runs->nrun,series_runs->nrow) ;
     else if( condition_index!=NULL )
       INFO_message("3dRSA: %d %s in '%s', %d subjects x %d conditions (%d table rows);\n"
                    "       condition order for -model_mat: %s",
                    nroi,streaming?"searchlight spheres":"ROIs",
                    (maskname!=NULL)?maskname:"(whole surface mesh)",
                    nsub,ncondition_level,condition_index->nrow,condition_order_arg) ;
     else
       INFO_message("3dRSA: %d %s in '%s', %d rows in the data table",
                    nroi , streaming ? "searchlight spheres" : "ROIs" ,
                    (maskname != NULL) ? maskname : "(whole surface mesh)" , nsub ) ;
   }

   /*================== main datasets ==================*/

   /* Runwise input keeps its datasets in the THD_runset (opened and validated
      already); the per-subject dset[] array is only for the -dataTable path. */
   if( runset != NULL ){ nvals = runset->ncond ; goto have_datasets ; }

   dset = (THD_3dim_dataset **)calloc(series_runs?series_runs->nrow:
                                      condition_index?condition_index->ncell:nsub,
                                      sizeof(THD_3dim_dataset *)) ;

   if( condition_index!=NULL ) nvals=ncondition_level ;
   for( jj=0 ; jj < (series_runs?series_runs->nrow:
                     condition_index?condition_index->ncell:nsub) ; jj++ ){
     int source_row=condition_index?condition_index->row_of[jj]:jj ;
     char *fn=series_runs?series_runs->fname[jj]:
              condition_index?longtab->fname[source_row]:tab->fname[jj] ;
     int sj=series_runs?series_runs->row_sub[jj]:
            condition_index?(jj/ncondition_level):jj ;
     int uj=series_runs?series_runs->row_run[jj]:0 ;
     dset[jj] = THD_open_dataset( fn ) ;
     if( dset[jj] == NULL ){
       if( tab->from_argv )
         ERROR_exit("3dRSA: can't open InputFile '%s' (row %d, Subj %s).\n"
                    "       The table came from the command line, where\n"
                    "       'InputFile' must be the LAST column.",
                    fn , jj+1 , tab->subj[sj]) ;
       ERROR_exit("3dRSA: can't open InputFile '%s' (row %d, Subj %s%s%s%s%s)",
                  fn,source_row+1,tab->subj[sj],series_runs?", ":
                  condition_index?", condition ":"",
                  series_runs?run_column:"",series_runs?" ":"",
                  series_runs?series_runs->run_label[uj]:
                  condition_index?condition_index->level[1][jj%ncondition_level]:"") ;
     }
     if( DSET_NVOX(dset[jj]) != nvox )
       ERROR_exit("3dRSA: '%s' has %d voxels but the -mask has %d",
                  fn , DSET_NVOX(dset[jj]) , nvox) ;
     if( !EQUIV_GRIDS(mset,dset[jj]) )
       WARNING_message("3dRSA: grid mismatch between -mask and '%s'",fn) ;

     if( series_runs!=NULL ){
       int nv=DSET_NVALS(dset[jj]) ;
       if( series_runs->run_nval[uj]==0 ) series_runs->run_nval[uj]=nv ;
       else if( series_runs->run_nval[uj]!=nv )
         ERROR_exit("3dRSA: run length mismatch for %s %s: Subj %s has %d TRs; expected %d",
                    run_column,series_runs->run_label[uj],tab->subj[sj],nv,
                    series_runs->run_nval[uj]) ;
     } else if( condition_index!=NULL ){
       if( DSET_NVALS(dset[jj])!=1 )
         ERROR_exit("3dRSA: condition-table InputFile '%s' (row %d, Subj %s, %s %s)\n"
                    "       resolves to %d bricks; each row must resolve to exactly one",
                    fn,source_row+1,tab->subj[sj],condition_column,
                    condition_index->level[1][jj%ncondition_level],DSET_NVALS(dset[jj])) ;
     } else if( jj == 0 ) nvals = DSET_NVALS(dset[jj]) ;
     else if( DSET_NVALS(dset[jj]) != nvals )
       ERROR_exit("3dRSA: '%s' has %d sub-bricks but '%s' has %d.\n"
                  "       All inputs must have the same number.",
                  tab->fname[jj] , DSET_NVALS(dset[jj]) , tab->fname[0] , nvals) ;
   }
   if( series_runs!=NULL ){
     nvals=0 ;
     for( jj=0 ; jj<series_runs->nrun ; jj++ ){
       if( run_resolved && series_runs->run_nval[jj]<3 )
         ERROR_exit("3dRSA: run-resolved IS-RSA needs at least 3 TRs per run; %s has %d",
                    series_runs->run_label[jj],series_runs->run_nval[jj]) ;
       series_runs->offset[jj]=nvals ; nvals+=series_runs->run_nval[jj] ;
     }
     series_runs->total_nvals=nvals ;
     if( !quiet ){
       const char *rn=run_normalize==RUN_NORM_ZSCORE?"zscore":
                      run_normalize==RUN_NORM_DEMEAN?"demean":"no" ;
       if( run_resolved )
         INFO_message("3dRSA: %d subjects x %d runs (%d input rows), %d total TRs;\n"
                      "       per-run %s normalization, equal-run %s",
                      nsub,series_runs->nrun,series_runs->nrow,nvals,rn,
                      run_analysis==RUN_ANALYSIS_SEPARATE?"separate effects + mean":"mean effect") ;
       else
         INFO_message("3dRSA: %d subjects x %d runs (%d input rows), %d concatenated TRs;\n"
                      "       per-run %s normalization, run-length weighted",
                      nsub,series_runs->nrun,series_runs->nrow,nvals,rn) ;
       if( nrunconspec>0 ){
         int cc ;
         for( cc=0 ; cc<nrunconspec ; cc++ )
           INFO_message("3dRSA: fixed-run contrast '%s' = mean(%s:%s) - mean(%s:%s)",
                        runcon[cc].name,runfactor[runcon[cc].ifactor].column,
                        runfactor[runcon[cc].ifactor].level[runcon[cc].ipos],
                        runfactor[runcon[cc].ifactor].column,
                        runfactor[runcon[cc].ifactor].level[runcon[cc].ineg]) ;
       }
     }
   }

   if( mode == MODE_CONT && nvals < 3 )
     ERROR_exit("3dRSA: IS-RSA mean features need a time series, but the inputs\n"
                "       have only %d sub-brick%s",nvals,(nvals==1)?"":"s") ;
   if( mode == MODE_RDM && nvals < 3 )
     ERROR_exit("3dRSA: second-order IS-RSA needs at least 3 conditions to form\n"
                "       an RDM triangle, but the inputs have %d",nvals) ;
   if( mode == MODE_RDM && runset == NULL ){
     for( kk=0 ; kk < nroi ; kk++ )
       if( rl->vox[kk].nar < 2 )
         ERROR_exit("3dRSA: second-order IS-RSA condition-pattern RDMs need at\n"
                    "       least 2 voxels per ROI/neighborhood; location %d has %d.",
                    rl->val[kk],rl->vox[kk].nar) ;
   }

 have_datasets:
   nitem = (rdm_over == RDM_SUBJ) ? nsub : nvals ;
   ntri  = THD_NTRI(nitem) ;

   if( nitem < 6 )
     ERROR_exit("3dRSA: the matrices would be only %d x %d.  With so few items\n"
                "       the permutation test has almost no resolution and the\n"
                "       correlation is not interpretable.  Need at least 6.",
                nitem,nitem) ;

   /* F4 first contract: the published simplified covariance assumes the same
      independent partitions and exchangeable condition noise for every dyad.
      Balanced runwise crossnobis satisfies that estimator contract.  F21's
      pair-specific run sets do not: applying the balanced weight there would
      be precise-looking but wrong.  Keep regression and condition-resampling
      extensions closed until their covariance-weighted estimands are defined. */
   if( cmp_metric==CMP_CORR_COV || cmp_metric==CMP_COS_COV ){
     if( rdm_over!=RDM_BRICK || runset==NULL )
       ERROR_exit("3dRSA: -metric %s currently requires classic '-mode RSA' with\n"
                  "       balanced -runwiseTable crossnobis input.  An ordinary RDM or\n"
                  "       an IS-RSA outer matrix does not carry this distance estimator.",
                  THD_simmat_cmp_label(cmp_metric)) ;
     if( runset->has_condmap )
       ERROR_exit("3dRSA: -metric %s does not yet support ConditionFile/TrialFile mappings.\n"
                  "       Missing conditions or repeated/trial aggregation can change\n"
                  "       dyad support/precision, so the balanced covariance\n"
                  "       V=(C C') o (C C') is not assumed.\n"
                  "       Use a balanced -runwiseTable or another -metric.",
                  THD_simmat_cmp_label(cmp_metric)) ;
     if( joint || nortspec>0 || ncomspec>0 || nfitspec>0 )
       ERROR_exit("3dRSA: -metric %s is currently a fixed-model scalar comparison;\n"
                  "       -model_joint, -ortvec, -model_commonality, and -model_fit\n"
                  "       need separately validated covariance-weighted fits.",
                  THD_simmat_cmp_label(cmp_metric)) ;
     if( ncboot>0 )
       ERROR_exit("3dRSA: -metric %s cannot yet be combined with -cond_bootstrap.\n"
                  "       Resampled/duplicated conditions change the distance covariance.\n"
                  "       Subject -bootstrap is supported.",
                  THD_simmat_cmp_label(cmp_metric)) ;
   }

   if( fit_condfold_file!=NULL ){
     fit_condfold=rsa_condfold_read(fit_condfold_file,nitem) ;
     if( fit_condfold==NULL )
       ERROR_exit("3dRSA: cannot use -fit_condfold '%s'",fit_condfold_file) ;
     if( !quiet )
       INFO_message("3dRSA: F22 strict subject x condition CV: %d explicit held-condition folds",
                    fit_condfold->nfold) ;
   }

   if( null_mode == NULL_TIMESHIFT &&
       (long long)nvals-2LL*min_shift+1LL < 2LL )
     ERROR_exit("3dRSA: -min_shift %d leaves fewer than two permitted circular\n"
                "       offsets for a %d-TR series.  Need 2*K < %d.",
                min_shift,nvals,nvals) ;

   /*================== models ==================*/

   mod = (RSA_model *)calloc(nmod,sizeof(RSA_model)) ;
   mm  = 0 ;

   for( ii=0 ; ii < nmodspec ; ii++,mm++ ){
     /* rule is always set by the if/else chain below (whose else exits), but
        that is opaque to -Wextra's uninitialized analysis; seed a valid default */
     char *spec = strdup(modspec[ii]) , *colon ; int rule = RUL_ANNAK , icol ;

     colon = strrchr(spec,':') ;
     if( colon == NULL )
       ERROR_exit("3dRSA: -model '%s' has no rule.  Write it as COLUMN:RULE,\n"
                  "       e.g. -model %s:annak",spec,spec) ;
     *colon = '\0' ;

     if( strchr(spec,',') != NULL ){          /* multivariate profile model */
       char *tok , *sp ; float **cols=NULL ; int *icols=NULL ;
       int p=0 , is_mahal ; char nm[128] ;

       is_mahal = (strcasecmp(colon+1,"mahal") == 0) ;
       if( !is_mahal && strcasecmp(colon+1,"euclid") != 0 )
         ERROR_exit("3dRSA: a multivariate -model '%s:%s' must use rule 'euclid'\n"
                    "       or 'mahal'.  The rank rules (annak, nn) are only\n"
                    "       defined for a single scalar column.",spec,colon+1) ;

       nm[0] = '\0' ;
       for( tok = strtok_r(spec,",",&sp) ; tok != NULL ; tok = strtok_r(NULL,",",&sp) ){
         int ic = THD_datatable_column( tab , tok ) ;
         if( ic < 0 )
           ERROR_exit("3dRSA: -model profile column '%s' is not in the data table",tok) ;
         if( !tab->isnum[ic] )
           ERROR_exit("3dRSA: -model profile column '%s' is not numeric",tok) ;
         rsa_require_finite_column(tab,ic,"model") ;
         cols = (float **)realloc(cols,sizeof(float *)*(p+1)) ;
         icols = (int *)realloc(icols,sizeof(int)*(p+1)) ;
         cols[p] = tab->val[ic] ; icols[p++] = ic ;
         if( nm[0] ) strncat(nm,"+",sizeof(nm)-strlen(nm)-1) ;
         strncat(nm,tok,sizeof(nm)-strlen(nm)-1) ;
       }
       if( p < 2 )
         ERROR_exit("3dRSA: multivariate -model needs at least two columns") ;

       mod[mm].kind = MODK_COLUMN ; mod[mm].spec = spec ; mod[mm].icol = -1 ;
       mod[mm].ncol = p ; mod[mm].icols = icols ;
       if( is_mahal ){
         float shrink=0.0f ; int erank=0 ;
         mod[mm].mat = THD_simmat_from_profile_mahal( nitem , p , cols , &shrink , &erank ) ;
         if( mod[mm].mat == NULL )
           ERROR_exit("3dRSA: -model '%s:mahal' has a constant or non-finite\n"
                      "       column, so the profile covariance is singular.  Drop\n"
                      "       the offending measure or use ':euclid'.",nm) ;
         snprintf( mod[mm].name , sizeof(mod[mm].name) , "%.112s_mvM" , nm ) ;
         if( !quiet )
           INFO_message("3dRSA: -model '%s:mahal' -- %d measures, effective rank "
                        "%d, Ledoit-Wolf shrinkage %.3f%s",
                        nm , p , erank , shrink ,
                        (erank < p) ? "  (measures are collinear; whitening "
                                      "down-weights the shared direction)" : "") ;
       } else {
         mod[mm].mat = THD_simmat_from_profile( nitem , p , cols ) ;
         if( mod[mm].mat==NULL )
           ERROR_exit("3dRSA: -model '%s:euclid' contains a non-finite value",nm) ;
         snprintf( mod[mm].name , sizeof(mod[mm].name) , "%.115s_mv" , nm ) ;
       }
       free(cols) ;
       continue ;
     }

          if( strcasecmp(colon+1,"annak")   == 0 ) rule = RUL_ANNAK ;
     else if( strcasecmp(colon+1,"nn")      == 0 ) rule = RUL_NN ;
     else if( strcasecmp(colon+1,"euclid")  == 0 ) rule = RUL_EUCLID ;
     else if( strcasecmp(colon+1,"absdiff") == 0 ) rule = RUL_ABSDIFF ;
     else if( strcasecmp(colon+1,"match")   == 0 ) rule = RUL_MATCH ;
     else ERROR_exit("3dRSA: unknown model rule '%s'.  Use annak, nn, euclid\n"
                     "       absdiff, or match.",colon+1) ;

     icol = THD_datatable_column( tab , spec ) ;
     if( icol < 0 )
       ERROR_exit("3dRSA: -model refers to column '%s', not in the data table",spec) ;
     if( rule!=RUL_MATCH && !tab->isnum[icol] )
       ERROR_exit("3dRSA: data table column '%s' is not numeric, so a model\n"
                  "       matrix cannot be built from it",spec) ;
     if( rule!=RUL_MATCH ) rsa_require_finite_column(tab,icol,"model") ;

     mod[mm].kind = MODK_COLUMN ; mod[mm].spec = spec ;
     mod[mm].rule = rule ; mod[mm].icol = icol ;
     if( rule!=RUL_MATCH ){
       mod[mm].ncol = 1 ; mod[mm].icols = (int *)malloc(sizeof(int)) ;
       mod[mm].icols[0] = icol ;
       mod[mm].mat=THD_simmat_from_column(nitem,tab->val[icol],rule) ;
     } else mod[mm].mat=rsa_simmat_from_labels(tab,icol,nitem) ;
     if( mod[mm].mat==NULL )
       ERROR_exit("3dRSA: -model column '%s' could not form a model RDM%s",spec,
                  rule==RUL_MATCH?" (match needs at least two distinct labels)":"") ;
     sprintf( mod[mm].name , "%.100s_%.10s" , spec , colon+1 ) ;
   }

   /* Stage 3: build one subject RDM per labeled run from a long-table measure.
      Subject centering deliberately creates two estimands: within-subject state
      deviations and the between-subject trait mean. */
   for( ii=0 ; ii<nrunmodspec ; ii++ ){
     char *work=strdup(runmodspec[ii]),*colon=strrchr(work,':'),*col=work ;
     int ic,rule,centered=0,ru,ss ; float *x,*mean=NULL,*v ;
     *colon='\0' ;
     rule=(strcasecmp(colon+1,"annak")==0)?RUL_ANNAK:RUL_NN ;
     ic=THD_datatable_column(longtab,col) ;
     if( ic<0 ) ERROR_exit("3dRSA: -run_model refers to column '%s', not in the data table",col) ;
     if( !longtab->isnum[ic] ) ERROR_exit("3dRSA: -run_model column '%s' is not numeric",col) ;
     rsa_require_finite_column(longtab,ic,"run model") ; x=longtab->val[ic] ;
     for( ss=0 ; ss<nruncenter ; ss++ ) if( strcasecmp(runcenter[ss],col)==0 ) centered=1 ;
     if( centered ){
       mean=(float *)calloc(nsub,sizeof(float)) ;
       for( ss=0 ; ss<nsub ; ss++ ){
         for( ru=0 ; ru<series_runs->nrun ; ru++ )
           mean[ss]+=x[series_runs->row_of[ss*series_runs->nrun+ru]] ;
         mean[ss]/=series_runs->nrun ;
       }
       mod[mm].kind=MODK_RUNCOLUMN; mod[mm].spec=runmodspec[ii]; mod[mm].rule=rule;
       mod[mm].icol=ic; mod[mm].run_mat=(THD_simmat **)calloc(series_runs->nrun,sizeof(THD_simmat *)) ;
       v=(float *)malloc(sizeof(float)*nsub) ;
       for( ru=0 ; ru<series_runs->nrun ; ru++ ){
         for( ss=0 ; ss<nsub ; ss++ )
           v[ss]=x[series_runs->row_of[ss*series_runs->nrun+ru]]-mean[ss] ;
         mod[mm].run_mat[ru]=THD_simmat_from_column(nsub,v,rule) ;
         if( mod[mm].run_mat[ru]==NULL )
           ERROR_exit("3dRSA: centered -run_model '%s' could not form a finite state RDM for run %s",
                      col,series_runs->run_label[ru]) ;
       }
       snprintf(mod[mm].name,sizeof(mod[mm].name),"%.96s_state_%s",col,
                rule==RUL_ANNAK?"annak":"nn") ;
       free(v); mm++ ;

       mod[mm].kind=MODK_RUNCOLUMN; mod[mm].spec=runmodspec[ii]; mod[mm].rule=rule;
       mod[mm].icol=ic; mod[mm].mat=THD_simmat_from_column(nsub,mean,rule) ;
       if( mod[mm].mat==NULL )
         ERROR_exit("3dRSA: centered -run_model '%s' could not form a finite trait RDM",col) ;
       snprintf(mod[mm].name,sizeof(mod[mm].name),"%.96s_trait_%s",col,
                rule==RUL_ANNAK?"annak":"nn") ;
       free(mean); mm++ ;
     } else {
       mod[mm].kind=MODK_RUNCOLUMN; mod[mm].spec=runmodspec[ii]; mod[mm].rule=rule;
       mod[mm].icol=ic; mod[mm].run_mat=(THD_simmat **)calloc(series_runs->nrun,sizeof(THD_simmat *)) ;
       v=(float *)malloc(sizeof(float)*nsub) ;
       for( ru=0 ; ru<series_runs->nrun ; ru++ ){
         for( ss=0 ; ss<nsub ; ss++ ) v[ss]=x[series_runs->row_of[ss*series_runs->nrun+ru]] ;
         mod[mm].run_mat[ru]=THD_simmat_from_column(nsub,v,rule) ;
         if( mod[mm].run_mat[ru]==NULL )
           ERROR_exit("3dRSA: -run_model '%s' could not form a finite RDM for run %s",
                      col,series_runs->run_label[ru]) ;
       }
       snprintf(mod[mm].name,sizeof(mod[mm].name),"%.102s_run_%s",col,
                rule==RUL_ANNAK?"annak":"nn") ;
       free(v); mm++ ;
     }
     free(work) ;
   }

   for( ii=0 ; ii < nmatspec ; ii++,mm++ ){
     char *bnam , *dot ;
     mod[mm].kind = MODK_MATRIX ; mod[mm].spec = matspec[ii] ; mod[mm].icol = -1 ;
     mod[mm].mat  = THD_simmat_read_1D( matspec[ii] , nitem ) ;

     bnam = THD_trailname(matspec[ii],0) ;
     strncpy( mod[mm].name , bnam , 120 ) ; mod[mm].name[120] = '\0' ;
     dot = strrchr(mod[mm].name,'.') ; if( dot != NULL ) *dot = '\0' ;
   }

   for( ii=0 ; ii < ndsespec ; ii++,mm++ ){
     int icol = THD_datatable_column( tab , dsespec[ii] ) ;
     if( icol < 0 )
       ERROR_exit("3dRSA: -model_dset refers to column '%s', not in the data table",
                  dsespec[ii]) ;
     if( icol == tab->icol_input )
       ERROR_exit("3dRSA: -model_dset '%s' is the InputFile column -- that would\n"
                  "       just correlate the data with itself",dsespec[ii]) ;

     mod[mm].kind = MODK_DSET ; mod[mm].spec = dsespec[ii] ;
     mod[mm].icol = icol ; mod[mm].mat = NULL ;
     strncpy( mod[mm].name , dsespec[ii] , 120 ) ; mod[mm].name[120] = '\0' ;

     mod[mm].dset = (THD_3dim_dataset **)calloc(nsub,sizeof(THD_3dim_dataset *)) ;
     for( jj=0 ; jj < nsub ; jj++ ){
       char *fn = DT_CELL(tab,jj,icol) ;
       mod[mm].dset[jj] = THD_open_dataset( fn ) ;
       if( mod[mm].dset[jj] == NULL )
         ERROR_exit("3dRSA: -model_dset '%s': can't open '%s' (row %d, Subj %s)",
                    dsespec[ii] , fn , jj+1 , tab->subj[jj]) ;
       if( DSET_NVOX(mod[mm].dset[jj]) != nvox )
         ERROR_exit("3dRSA: -model_dset '%s': '%s' has %d voxels but the -mask has %d",
                    dsespec[ii] , fn , DSET_NVOX(mod[mm].dset[jj]) , nvox) ;
       if( DSET_NVALS(mod[mm].dset[jj]) != DSET_NVALS(mod[mm].dset[0]) )
         ERROR_exit("3dRSA: -model_dset '%s': '%s' has a different number of\n"
                    "       sub-bricks than the first one",dsespec[ii],fn) ;
     }
     mod[mm].mvals = DSET_NVALS(mod[mm].dset[0]) ;
     if( mode == MODE_CONT && mod[mm].mvals < 3 )
       ERROR_exit("3dRSA: -model_dset '%s' needs a time series under -mode\n"
                  "       continuous, but has only %d sub-brick(s)",
                  dsespec[ii] , mod[mm].mvals) ;
     if( mode == MODE_RDM && mod[mm].mvals < 3 )
       ERROR_exit("3dRSA: -model_dset '%s' needs at least 3 conditions under\n"
                  "       '-featuretype rdm', but has only %d sub-brick(s)",
                  dsespec[ii] , mod[mm].mvals) ;
   }

   if( seed_mask != NULL ){
     mod[mm].kind=MODK_SEED ; mod[mm].spec=seed_mask ; mod[mm].icol=-1 ;
     mod[mm].mat=NULL ;
     snprintf(mod[mm].name,sizeof(mod[mm].name),"seedROI%d",seedrl->val[0]) ;
     mm++ ;
   }
   if( mm != nmod ) ERROR_exit("3dRSA internal error: built %d of %d models",mm,nmod) ;

   /*-- apply any -model_label overrides.  Models are built COLUMN, then MATRIX,
        then DSET, so the global index of each spec is a fixed offset. --*/
   { int gi ;
     for( ii=0 ; ii < nmodspec ; ii++ )
       if( modlabel != NULL && modlabel[ii] != NULL ){
         gi = ii ;
         strncpy(mod[gi].name,modlabel[ii],sizeof(mod[gi].name)-1) ;
         mod[gi].name[sizeof(mod[gi].name)-1] = '\0' ;
       }
     for( ii=0 ; ii < nmatspec ; ii++ )
       if( matlabel != NULL && matlabel[ii] != NULL ){
         gi = nmodspec + nrunmod + ii ;
         strncpy(mod[gi].name,matlabel[ii],sizeof(mod[gi].name)-1) ;
         mod[gi].name[sizeof(mod[gi].name)-1] = '\0' ;
       }
     for( ii=0 ; ii < ndsespec ; ii++ )
       if( dselabel != NULL && dselabel[ii] != NULL ){
         gi = nmodspec + nrunmod + nmatspec + ii ;
         strncpy(mod[gi].name,dselabel[ii],sizeof(mod[gi].name)-1) ;
         mod[gi].name[sizeof(mod[gi].name)-1] = '\0' ;
       }
   }

   /* LOO predicts scalar columns or complete multivariate profiles.  Preserve
      one labeled output per model, but share work only when both source columns
      and estimand match (AnnaK typicality is not the NN predictor). */
   if( do_loo ){
     loo_owner = (int *)malloc(sizeof(int)*nmod) ;
     loo_fam   = (int *)malloc(sizeof(int)*nmod) ;
     for( mm=0 ; mm < nmod ; mm++ ){
       loo_owner[mm] = loo_fam[mm] = -1 ;
       if( rsa_model_has_loo(mod+mm) ){
         int aa ; nloo++ ;
         if( mod[mm].ncol>maxloocol ) maxloocol=mod[mm].ncol ;
         for( aa=0 ; aa < mm ; aa++ )
           if( rsa_model_same_loo(mod+aa,mod+mm) ){
             loo_owner[mm]=loo_owner[aa] ; loo_fam[mm]=loo_fam[aa] ; break ;
           }
         if( loo_owner[mm] < 0 ){
           loo_owner[mm] = mm ; loo_fam[mm] = nloofam++ ;
         }
       }
     }
     if( !quiet && nloo > nloofam )
       INFO_message("3dRSA: -loo: %d model outputs share %d distinct target/estimand families",
                    nloo,nloofam) ;
   }

   /* F7: resolve each NAME=A,B,... fitted mixture only after display labels
      have been applied.  Components must be distinct and share one
      similarity/distance sense; mixing opposite senses under nonnegative
      weights would make the sign of the prediction uninterpretable. */
   if( nfitspec>0 ){
     fit=(RSA_fitmodel *)calloc(nfitspec,sizeof(RSA_fitmodel)) ;
     for( ii=0 ; ii<nfitspec ; ii++ ){
       char *buf=strdup(fitspec[ii]),*eq=strchr(buf,'='),*tok,*sp ; int cc=0,sense=-1 ;
       if( eq==NULL || eq==buf || eq[1]=='\0' || strchr(eq+1,'=')!=NULL )
         ERROR_exit("3dRSA: -model_fit '%s' must be NAME=A,B,...",fitspec[ii]) ;
       *eq='\0' ;
       if( strlen(buf)>=sizeof(fit[ii].name) || strchr(buf,',')!=NULL )
         ERROR_exit("3dRSA: invalid or overlong fitted-model name '%s'",buf) ;
       for( mm=0 ; mm<nmod ; mm++ ) if( strcmp(buf,mod[mm].name)==0 )
         ERROR_exit("3dRSA: fitted-model name '%s' duplicates component model '%s'",buf,buf) ;
       for( jj=0 ; jj<ii ; jj++ ) if( strcmp(buf,fit[jj].name)==0 )
         ERROR_exit("3dRSA: fitted-model name '%s' was requested twice",buf) ;
       strcpy(fit[ii].name,buf) ; fit[ii].ridge=fit_ridge ; fit[ii].wbase=nfitw ;
       for( tok=strtok_r(eq+1,",",&sp) ; tok!=NULL ; tok=strtok_r(NULL,",",&sp) ){
         int ix=-1,ss ;
         if( tok[0]=='\0' ) ERROR_exit("3dRSA: empty component in -model_fit '%s'",fitspec[ii]) ;
         for( mm=0 ; mm<nmod ; mm++ ) if( strcmp(tok,mod[mm].name)==0 ){ ix=mm ; break ; }
         if( ix<0 ) ERROR_exit("3dRSA: -model_fit '%s' names unknown component '%s'",
                               fitspec[ii],tok) ;
         for( jj=0 ; jj<cc ; jj++ ) if( fit[ii].comp[jj]==ix )
           ERROR_exit("3dRSA: -model_fit '%s' repeats component '%s'",fitspec[ii],tok) ;
         ss=(mod[ix].mat!=NULL) ? mod[ix].mat->is_dist : (neu_metric==SIM_EUCLID) ;
         if( sense<0 ) sense=ss ; else if( sense!=ss )
           ERROR_exit("3dRSA: -model_fit '%s' mixes similarity and distance components.\n"
                      "       Nonnegative weights require all components to have one sense.",
                      fitspec[ii]) ;
         fit[ii].comp=(int *)realloc(fit[ii].comp,sizeof(int)*(cc+1)) ;
         fit[ii].comp[cc++]=ix ;
       }
       fit[ii].ncomp=cc ;
       if( cc<2 ) ERROR_exit("3dRSA: -model_fit '%s' needs at least two components",
                             fitspec[ii]) ;
       nfitw+=cc ; free(buf) ;
     }
     nfit=nfitspec ;
     if( !quiet ) for( ii=0 ; ii<nfit ; ii++ )
       INFO_message("3dRSA: fitted model '%s': %d nonnegative components, ridge %.6g;\n"
                    "       %s",
                    fit[ii].name,fit[ii].ncomp,fit[ii].ridge,
                    (fit_condfold!=NULL)
                      ? "strict held-subject x held-condition fitting/scoring"
                      : "leave-one-subject-out weights and held-subject scoring") ;
   }

   /*-- Resolve -model_contrast over either two fixed/base models or two F7
        fitted models.  Match A as the longest known name followed by '-'; this
        tolerates '-' inside either namespace.  A mixed fixed/fitted request is
        rejected because an in-sample association and held-out accuracy are not
        comparable estimands. --*/
   if( nconstrspec > 0 ){
     if( pending_label != NULL )
       ERROR_exit("3dRSA: -model_label '%s' was not followed by a model",pending_label) ;
     con = (RSA_contrast *)calloc(nconstrspec,sizeof(RSA_contrast)) ;
     fcon = (RSA_fitcontrast *)calloc(nconstrspec,sizeof(RSA_fitcontrast)) ;
     for( ii=0 ; ii < nconstrspec ; ii++ ){
       char *spec = constrspec[ii] ; int ia=-1 , ib=-1 , ta=-1 , tb=-1 , a , b ;
       size_t best=0 , ls=strlen(spec) ;
       for( a=0 ; a < nmod ; a++ ){
         size_t la = strlen(mod[a].name) ;
         if( la < ls && la > best && strncmp(spec,mod[a].name,la)==0 && spec[la]=='-' ){
           for( b=0 ; b<nmod ; b++ ) if( b!=a && strcmp(spec+la+1,mod[b].name)==0 ){
             ia=a ; ib=b ; ta=tb=0 ; best=la ; break ;
           }
           for( b=0 ; b<nfit ; b++ ) if( strcmp(spec+la+1,fit[b].name)==0 ){
             ia=a ; ib=b ; ta=0 ; tb=1 ; best=la ; break ;
           }
         }
       }
       for( a=0 ; a < nfit ; a++ ){
         size_t la = strlen(fit[a].name) ;
         if( la < ls && la > best && strncmp(spec,fit[a].name,la)==0 && spec[la]=='-' ){
           for( b=0 ; b<nfit ; b++ ) if( b!=a && strcmp(spec+la+1,fit[b].name)==0 ){
             ia=a ; ib=b ; ta=tb=1 ; best=la ; break ;
           }
           for( b=0 ; b<nmod ; b++ ) if( strcmp(spec+la+1,mod[b].name)==0 ){
             ia=a ; ib=b ; ta=1 ; tb=0 ; best=la ; break ;
           }
         }
       }
       if( ia < 0 || ib < 0 ){
         char avail[1024] ; int a2 ; avail[0]='\0' ;
         for( a2=0 ; a2 < nmod ; a2++ ){
           strncat(avail,mod[a2].name,sizeof(avail)-strlen(avail)-2) ;
           if( a2 < nmod-1 ) strncat(avail,", ",sizeof(avail)-strlen(avail)-1) ;
         }
         if( nfit>0 ) for( a2=0 ; a2<nfit ; a2++ ){
           if( avail[0]!='\0' ) strncat(avail,", ",sizeof(avail)-strlen(avail)-1) ;
           strncat(avail,fit[a2].name,sizeof(avail)-strlen(avail)-1) ;
         }
         ERROR_exit("3dRSA: -model_contrast '%s' must be 'A-B' where A and B are\n"
                    "       two fixed models or two fitted models.  Known models: %s\n"
                    "       (name a model with -model_label to make this readable)",
                    spec , avail) ;
       }
       if( ta != tb )
         ERROR_exit("3dRSA: -model_contrast '%s' mixes a fixed-model association with a\n"
                    "       fitted-model held-out accuracy.  Compare two fixed models or two\n"
                    "       -model_fit models; those estimands cannot be paired directly.",spec) ;
       if( ta==0 ){
         con[ncon].ia=ia ; con[ncon].ib=ib ;
         strncpy(con[ncon].name,spec,sizeof(con[ncon].name)-1) ;
         con[ncon].name[sizeof(con[ncon].name)-1]='\0' ; ncon++ ;
       } else {
         fcon[nfitcon].ia=ia ; fcon[nfitcon].ib=ib ;
         strncpy(fcon[nfitcon].name,spec,sizeof(fcon[nfitcon].name)-1) ;
         fcon[nfitcon].name[sizeof(fcon[nfitcon].name)-1]='\0' ; nfitcon++ ;
       }
     }
   }

   /* A null generator is not interchangeable with a scientific null.  Expose
      that choice and reject combinations whose current engine cannot test it. */
   if( ncon+nfitcon > 0 ){
     if( !contrast_hypothesis_given && !quiet )
       WARNING_message("3dRSA: -model_contrast is using -contrast_hypothesis legacy;\n"
                       "       specify superiority or alignment to make the scientific null explicit") ;
     if( contrast_hypothesis==CONTRAST_SUPERIORITY ){
       if( classic_null==CLASSIC_NULL_CONDITIONS )
         ERROR_exit("3dRSA: the shared condition-label contrast null does not test equal nonzero model performance;\n"
                    "       use -contrast_hypothesis alignment, or -classic_null subjects for superiority") ;
     } else if( contrast_hypothesis==CONTRAST_ALIGNMENT ){
       if( rdm_over==RDM_BRICK && classic_null==CLASSIC_NULL_SUBJECTS )
         ERROR_exit("3dRSA: classic -classic_null subjects tests paired superiority, not a sharp alignment null;\n"
                    "       use -classic_null conditions for -contrast_hypothesis alignment") ;
     }
   }

   /*-- Resolve pairwise A,B or F8 three-predictor A,B,C commonality. --*/
   if( ncomspec > 0 ){
     if( pending_label != NULL )
       ERROR_exit("3dRSA: -model_label '%s' was not followed by a model",pending_label) ;
     if( cmp_metric != CMP_PEARSON && cmp_metric != CMP_SPEARMAN )
       ERROR_exit("3dRSA: -model_commonality requires -metric pearson or spearman;\n"
                  "       its R2 decomposition is a regression statistic") ;
     com = (RSA_common *)calloc(ncomspec,sizeof(RSA_common)) ;
     for( ii=0 ; ii < ncomspec ; ii++ ){
       char buf[512],*tok,*sp ; int ix[3]={-1,-1,-1},nn=0,a,j ;
       strncpy(buf,comspec[ii],sizeof(buf)-1) ; buf[sizeof(buf)-1]='\0' ;
       if( buf[0]=='\0' || buf[0]==',' || buf[strlen(buf)-1]==',' || strstr(buf,",") == NULL ||
           strstr(buf,",,") != NULL )
         ERROR_exit("3dRSA: -model_commonality '%s' must be 'A,B' or 'A,B,C'",
                    comspec[ii]) ;
       for( tok=strtok_r(buf,",",&sp) ; tok!=NULL ; tok=strtok_r(NULL,",",&sp) ){
         if( nn>=3 ) ERROR_exit("3dRSA: -model_commonality '%s' names more than three models",
                                comspec[ii]) ;
         for( a=0 ; a<nmod ; a++ ) if( strcmp(tok,mod[a].name)==0 ){ ix[nn]=a ; break ; }
         nn++ ;
       }
       if( nn<2 || ix[0]<0 || ix[1]<0 || (nn==3 && ix[2]<0) ||
           ix[0]==ix[1] || (nn==3 && (ix[0]==ix[2] || ix[1]==ix[2])) ){
         char avail[1024] ; int a2 ; avail[0]='\0' ;
         for( a2=0 ; a2 < nmod ; a2++ ){
           strncat(avail,mod[a2].name,sizeof(avail)-strlen(avail)-2) ;
           if( a2 < nmod-1 ) strncat(avail,", ",sizeof(avail)-strlen(avail)-1) ;
         }
         ERROR_exit("3dRSA: -model_commonality '%s' must name two or three DISTINCT models\n"
                    "       as 'A,B' or 'A,B,C'.  Known models: %s\n"
                    "       (name a model with -model_label to make this readable)",
                    comspec[ii] , avail) ;
       }
       com[ii].nmodel=nn ; com[ii].nq=(nn==2)?RSA_NCOMMON:RSA_NCOMMON3 ;
       com[ii].qbase=ncomq ; for( j=0 ; j<nn ; j++ ) com[ii].imod[j]=ix[j] ;
       if( nn==2 ){
         snprintf(com[ii].lab[0],200,"uniq_%.150s",mod[ix[0]].name) ;
         snprintf(com[ii].lab[1],200,"uniq_%.150s",mod[ix[1]].name) ;
         snprintf(com[ii].lab[2],200,"common_%.70s_%.70s",mod[ix[0]].name,mod[ix[1]].name) ;
         snprintf(com[ii].lab[3],200,"partialR2_%.145s",mod[ix[0]].name) ;
         snprintf(com[ii].lab[4],200,"partialR2_%.145s",mod[ix[1]].name) ;
       } else {
         char *A=mod[ix[0]].name,*B=mod[ix[1]].name,*C=mod[ix[2]].name ;
         snprintf(com[ii].lab[0],200,"uniq_%.45s_given_%.45s_%.45s",A,B,C) ;
         snprintf(com[ii].lab[1],200,"uniq_%.45s_given_%.45s_%.45s",B,A,C) ;
         snprintf(com[ii].lab[2],200,"uniq_%.45s_given_%.45s_%.45s",C,A,B) ;
         snprintf(com[ii].lab[3],200,"common_%.40s_%.40s_not_%.40s",A,B,C) ;
         snprintf(com[ii].lab[4],200,"common_%.40s_%.40s_not_%.40s",A,C,B) ;
         snprintf(com[ii].lab[5],200,"common_%.40s_%.40s_not_%.40s",B,C,A) ;
         snprintf(com[ii].lab[6],200,"common_%.40s_%.40s_%.40s",A,B,C) ;
         snprintf(com[ii].lab[7],200,"partialR2_%.40s_given_%.40s_%.40s",A,B,C) ;
         snprintf(com[ii].lab[8],200,"partialR2_%.40s_given_%.40s_%.40s",B,A,C) ;
         snprintf(com[ii].lab[9],200,"partialR2_%.40s_given_%.40s_%.40s",C,A,B) ;
       }
       ncomq+=com[ii].nq ;
     }
     nreqcom=ncomspec ; comlab=(char **)malloc(sizeof(char *)*ncomq) ;
     for( ii=0 ; ii<nreqcom ; ii++ ) for( jj=0 ; jj<com[ii].nq ; jj++ )
       comlab[com[ii].qbase+jj]=com[ii].lab[jj] ;
   }

   /*-- nuisance (-ortvec) columns.  A per-subject confound has to be given a
        per-pair form to be removed from a per-pair neural matrix, and we do
        not want to assume a shape for it -- so each ortvec contributes TWO
        nuisance columns, the pair |difference| and the pair sum, spanning both
        the "similar-value" and "both-high" ways a confound can act.  They are
        projected out and never reported. --*/

   if( nortspec > 0 ){
     ort = (THD_simmat **)calloc(2*nortspec,sizeof(THD_simmat *)) ;
     for( ii=0 ; ii < nortspec ; ii++ ){
       int icol = THD_datatable_column( tab , ortspec[ii] ) ;
       float *x ; THD_simmat *sd , *ss ; int a , b ;

       if( icol < 0 )
         ERROR_exit("3dRSA: -ortvec refers to column '%s', not in the data table",
                    ortspec[ii]) ;
       if( !tab->isnum[icol] )
         ERROR_exit("3dRSA: -ortvec column '%s' is not numeric",ortspec[ii]) ;
       rsa_require_finite_column(tab,icol,"nuisance") ;
       x = tab->val[icol] ;

       sd = THD_simmat_from_column( nitem , x , RUL_ABSDIFF ) ;   /* |x_i - x_j| */
       if( sd==NULL ) ERROR_exit("3dRSA: -ortvec '%s' could not form a finite RDM",ortspec[ii]) ;
       sprintf(sd->name,"%.100s_diff",ortspec[ii]) ;

       ss = THD_simmat_new( nitem ) ;                             /* x_i + x_j   */
       for( a=0 ; a < nitem ; a++ )
         for( b=0 ; b < nitem ; b++ ) ss->mat[a*nitem+b] = x[a] + x[b] ;
       sprintf(ss->name,"%.100s_sum",ortspec[ii]) ;

       ort[nort++] = sd ; ort[nort++] = ss ;
     }
     if( !quiet )
       INFO_message("3dRSA: %d -ortvec nuisance%s -> %d columns projected out"
                    " (each removed as |diff| and sum)",
                    nortspec,(nortspec==1)?"":"s",nort) ;
   }

   /*-- tell the user what sense each model carries --*/

   if( !quiet ){
     int neu_sim = (neu_metric != SIM_EUCLID) ;
     for( mm=0 ; mm < nmod ; mm++ ){
       if( cmp_metric==CMP_CORR_COV || cmp_metric==CMP_COS_COV ){
         INFO_message("3dRSA: model '%s' is interpreted as a dissimilarity RDM;\n"
                      "       positive means matching representational geometry",
                      mod[mm].name) ;
         continue ;
       }
       if( mod[mm].kind==MODK_COLUMN || mod[mm].kind==MODK_RUNCOLUMN ){
         const char *brain = neu_sim ? "greater brain similarity"
                                     : "greater brain dissimilarity" ;
         if( mod[mm].rule==RUL_ANNAK ){
           INFO_message("3dRSA: model '%s' (AnnaK) -> positive means higher mean "
                        "behavioral rank across a pair goes with %s",
                        mod[mm].name,brain) ;
           continue ;
         }
         if( mod[mm].rule==RUL_NN ){
           INFO_message("3dRSA: model '%s' (NN) -> positive means subjects closer "
                        "in behavioral rank have %s",
                        mod[mm].name,neu_sim?"more similar brains":"more dissimilar brains") ;
           continue ;
         }
         if( mod[mm].rule==RUL_EUCLID ){
           INFO_message("3dRSA: model '%s' (euclid) -> positive means subjects closer "
                        "in raw behavioral value have %s",
                        mod[mm].name,neu_sim?"more similar brains":"more dissimilar brains") ;
           continue ;
         }
         if( mod[mm].rule==RUL_ABSDIFF ){
           INFO_message("3dRSA: model '%s' (absdiff) -> positive means greater raw "
                        "behavioral difference goes with %s",
                        mod[mm].name,brain) ;
           continue ;
         }
         if( mod[mm].rule==RUL_MATCH ){
           INFO_message("3dRSA: model '%s' (match) -> positive means subjects in the same categorical group have more similar brains",
                        mod[mm].name) ;
           continue ;
         }
       }
       int mod_sim = (mod[mm].mat != NULL) ? !mod[mm].mat->is_dist : neu_sim ;
       INFO_message("3dRSA: model '%s' -> positive means %s",
         mod[mm].name ,
         (neu_sim == mod_sim) ? "similar behavior goes with similar brains"
                              : "similar behavior goes with DISsimilar brains"
                                " (a similarity mixed with a distance)" ) ;
     }
   }

   /* Fail before parallel inference if any value that can enter an analyzed
      RDM is NaN or infinite.  The scan is over the union analysis domain;
      values outside all requested targets (and the optional seed) are ignored. */
   { int nscan=0 ;
     if( runset!=NULL )
       nscan=runset->nrow*(1+(seedrl!=NULL)+
                          ((noise_norm!=NN_NONE)?(1+(seedrl!=NULL)):0)) ;
     else nscan=(series_runs?series_runs->nrow:
                 condition_index?condition_index->ncell:nsub)*(1+(seedrl!=NULL)) ;
     for( mm=0 ; mm<nmod ; mm++ ) if( mod[mm].kind==MODK_DSET ) nscan+=nsub ;
     rsa_progress_init(&progress,progress_mode,quiet,3,"Data validation",
                       nscan,"scans") ;
   if( runset!=NULL ){
     int rw ; char who[256] ;
     for( rw=0 ; rw<runset->nrow ; rw++ ){
       snprintf(who,sizeof(who),"Subj %s, Run %s",runset->subj[runset->row_sub[rw]],
                runset->run_lab[rw]) ;
       rsa_validate_dset_domain(runset->betas[rw],rl,"runwise beta",who) ;
       rsa_progress_advance(&progress) ;
       if( seedrl!=NULL ){
         rsa_validate_dset_domain(runset->betas[rw],seedrl,"seed beta",who) ;
         rsa_progress_advance(&progress) ;
       }
       if( noise_norm!=NN_NONE ){
         rsa_validate_dset_domain(runset->resid[rw],rl,"residual",who) ;
         rsa_progress_advance(&progress) ;
         if( seedrl!=NULL ){
           rsa_validate_dset_domain(runset->resid[rw],seedrl,"seed residual",who) ;
           rsa_progress_advance(&progress) ;
         }
       }
     }
   } else {
     int nn=series_runs?series_runs->nrow:
            condition_index?condition_index->ncell:nsub ;
     for( jj=0 ; jj<nn ; jj++ ){
       int sj=series_runs?series_runs->row_sub[jj]:
              condition_index?(jj/ncondition_level):jj ; char who[256] ;
       if( series_runs ) snprintf(who,sizeof(who),"Subj %s, %s %s",tab->subj[sj],
                                  run_column,series_runs->run_label[series_runs->row_run[jj]]) ;
       else if( condition_index ) snprintf(who,sizeof(who),"Subj %s, %s %s",tab->subj[sj],
                                  condition_column,condition_index->level[1][jj%ncondition_level]) ;
       else snprintf(who,sizeof(who),"%s",tab->subj[sj]) ;
       rsa_validate_dset_domain(dset[jj],rl,"neural",who) ;
       rsa_progress_advance(&progress) ;
       if( seedrl!=NULL ){
         rsa_validate_dset_domain(dset[jj],seedrl,"seed neural",who) ;
         rsa_progress_advance(&progress) ;
       }
     }
   }
   for( mm=0 ; mm<nmod ; mm++ ) if( mod[mm].kind==MODK_DSET )
     for( jj=0 ; jj<nsub ; jj++ ){
       rsa_validate_dset_domain(mod[mm].dset[jj],rl,"model dataset",tab->subj[jj]) ;
       rsa_progress_advance(&progress) ;
     }
   }

   if( save_rdm != NULL ){
     char fn[THD_MAX_NAME] ;
     for( mm=0 ; mm < nmod ; mm++ ){
       if( mod[mm].mat == NULL ) continue ;   /* -model_dset varies per ROI */
       sprintf(fn,"%s_model_%s.1D",save_rdm,mod[mm].name) ;
       THD_simmat_write_1D( fn , mod[mm].mat ) ;
     }
   }

   /* F11: estimate the peak while all searchlight input headers are open but
      before their voxel arrays are loaded.  Atlas/ROI mode unloads subjects as
      it reduces them and therefore does not need this resident-input guard. */
   if( streaming )
     rsa_searchlight_memory_preflight(
       mset,rl,dset,runset,series_runs,condition_index,run_analysis,nrunconspec,mod,nmod,nort,nsub,mode,rdm_over,nitem,nvals,
       nperm,null_mode,nboot,ncboot,ncon,ncomq,nfit,nfitw,nfitcon,
       do_loo,nloo,nloofam,do_nc,
       noise_norm,do_dset,
       cmp_metric,joint,
       memory_limit_gib,memory_limit_given,memory_override,quiet ) ;

   /* Build the seed once, before any atlas continuous-data reduction unloads
      subject datasets.  Searchlights retain the fixed seed geometry while
      rebuilding only the target at each center. */
   if( seed_mask != NULL ){
     if( runset != NULL ){
       int rw ;
       for( rw=0 ; rw<runset->nrow ; rw++ ){
         DSET_load(runset->betas[rw]) ; CHECK_LOAD_ERROR(runset->betas[rw]) ;
         if( noise_norm!=NN_NONE ){
           DSET_load(runset->resid[rw]) ; CHECK_LOAD_ERROR(runset->resid[rw]) ;
         }
       }
     } else {
       int nmain=condition_index?condition_index->ncell:nsub ;
       for( jj=0 ; jj<nmain ; jj++ ){
         DSET_load(dset[jj]) ; CHECK_LOAD_ERROR(dset[jj]) ;
       }
     }
     mod[0].mat=rsa_build_seed_model(seedrl,rdm_over,mode,nsub,nvals,
                     neu_metric,cond_metric,polort,center_conditions,
                     dset,runset,condition_index,noise_norm,&seed_srdm) ;
     if( mod[0].mat==NULL || (rdm_over==RDM_BRICK && seed_srdm==NULL) )
       ERROR_exit("3dRSA: could not construct the seed representational geometry") ;
     if( save_rdm != NULL ){
       char fn[THD_MAX_NAME] ;
       snprintf(fn,sizeof(fn),"%s_model_%s.1D",save_rdm,mod[0].name) ;
       THD_simmat_write_1D(fn,mod[0].mat) ;
       if( rdm_over==RDM_BRICK ){
         THD_simmat *ss=THD_simmat_new(nitem) ;
         for( jj=0 ; jj<nsub ; jj++ ){
           THD_tri_to_simmat(nitem,seed_srdm+(size_t)jj*ntri,ss) ;
           ss->is_dist=(runset!=NULL || neu_metric==SIM_EUCLID) ;
           snprintf(fn,sizeof(fn),"%s_seed_subj%04d.1D",save_rdm,jj) ;
           THD_simmat_write_1D(fn,ss) ;
         }
         THD_simmat_free(ss) ;
       }
     }
   }

   /*================== reduce continuous data to ROI means ==================*/

   /* Searchlight streams: there are far too many spheres to precompute a mean
      time course for each, so keep the datasets loaded and reduce each sphere
      on the fly in the analysis loop instead. */
   if( runset != NULL ){
     /* crossnobis reads condition patterns per run per ROI; load every run's
        InputFile, and its ResidFile too when noise-normalizing. */
     int rw ;
     rsa_progress_init(&progress,progress_mode,quiet,3,"Loading runwise data",
                       runset->nrow*(1+(noise_norm!=NN_NONE)),"datasets") ;
     for( rw=0 ; rw < runset->nrow ; rw++ ){
       DSET_load(runset->betas[rw]) ; CHECK_LOAD_ERROR(runset->betas[rw]) ;
       rsa_progress_advance(&progress) ;
       if( noise_norm != NN_NONE ){
         DSET_load(runset->resid[rw]) ; CHECK_LOAD_ERROR(runset->resid[rw]) ;
         rsa_progress_advance(&progress) ;
       }
     }
     if( noise_norm == NN_SHRINK ){
       int mv = THD_roilist_maxvox(rl) ;
       if( mv > runset->resid_nt && !quiet )
         WARNING_message("3dRSA: some ROIs have more voxels (up to %d) than residual\n"
                         "       time points (%d); the shrinkage covariance will lean\n"
                         "       heavily on its target there.  Consider -noise_norm diag\n"
                         "       or smaller ROIs.",mv,runset->resid_nt) ;
       else if( mv > 128 && !quiet )
         WARNING_message("3dRSA: some ROIs have up to %d voxels; shrinkage noise\n"
                         "       normalization forms and eigendecomposes a dense p x p\n"
                         "       covariance per subject and location.  Benchmark this\n"
                         "       analysis, or consider -noise_norm diag or smaller ROIs.",mv) ;
     }
   } else if( streaming ){
     int nmain=series_runs?series_runs->nrow:
               condition_index?condition_index->ncell:nsub, nload=nmain ;
     for( mm=0 ; mm<nmod ; mm++ ) if( mod[mm].kind==MODK_DSET ) nload+=nsub ;
     rsa_progress_init(&progress,progress_mode,quiet,3,"Loading searchlight data",
                       nload,"datasets") ;
     for( jj=0 ; jj < nmain ; jj++ ){
       DSET_load(dset[jj]) ; CHECK_LOAD_ERROR(dset[jj]) ;
       rsa_progress_advance(&progress) ;
     }
     /* keep any -model_dset modality loaded too, to reduce each sphere on the fly */
     for( mm=0 ; mm < nmod ; mm++ ){
       if( mod[mm].kind != MODK_DSET ) continue ;
       for( jj=0 ; jj < nsub ; jj++ ){
         DSET_load(mod[mm].dset[jj]) ; CHECK_LOAD_ERROR(mod[mm].dset[jj]) ;
         rsa_progress_advance(&progress) ;
       }
     }
   } else if( mode == MODE_CONT ){
     int nmain=series_runs?series_runs->nrow:
               condition_index?condition_index->ncell:nsub, nreduce=nmain ;
     for( mm=0 ; mm<nmod ; mm++ ) if( mod[mm].kind==MODK_DSET ) nreduce+=nsub ;
     rsa_progress_init(&progress,progress_mode,quiet,3,"Atlas ROI reduction",
                       nreduce,"datasets") ;
     cmean = (float **)malloc(sizeof(float *)*nroi) ;
     for( kk=0 ; kk < nroi ; kk++ )
       cmean[kk] = (float *)calloc((size_t)nsub*nvals,sizeof(float)) ;

     for( jj=0 ; jj < nmain ; jj++ ){
       int sj=series_runs?series_runs->row_sub[jj]:jj ;
       int uj=series_runs?series_runs->row_run[jj]:0 ;
       int nv=series_runs?series_runs->run_nval[uj]:nvals ;
       int off=series_runs?series_runs->offset[uj]:0 ;
       DSET_load(dset[jj]) ; CHECK_LOAD_ERROR(dset[jj]) ;
       AFNI_OMP_START ;
#ifdef USE_OMP
#pragma omp parallel for if((long long)nroi*nv >= 10000LL) schedule(dynamic,1)
#endif
       for( kk=0 ; kk < nroi ; kk++ ){
         THD_roi_mean_ts( dset[jj] , rl->vox+kk , polort ,
                          cmean[kk] + (size_t)sj*nvals + off ) ;
         if( series_runs ) rsa_run_normalize(cmean[kk]+(size_t)sj*nvals+off,
                                              nv,run_normalize) ;
       }
       AFNI_OMP_END ;
       DSET_delete(dset[jj]) ; dset[jj] = NULL ;
       rsa_progress_advance(&progress) ;
     }

     for( mm=0 ; mm < nmod ; mm++ ){
       int mv ;
       if( mod[mm].kind != MODK_DSET ) continue ;
       mv = DSET_NVALS(mod[mm].dset[0]) ;
       mod[mm].cmean = (float **)malloc(sizeof(float *)*nroi) ;
       for( kk=0 ; kk < nroi ; kk++ )
         mod[mm].cmean[kk] = (float *)calloc((size_t)nsub*mv,sizeof(float)) ;
       for( jj=0 ; jj < nsub ; jj++ ){
         DSET_load(mod[mm].dset[jj]) ; CHECK_LOAD_ERROR(mod[mm].dset[jj]) ;
         AFNI_OMP_START ;
#ifdef USE_OMP
#pragma omp parallel for if((long long)nroi*mv >= 10000LL) schedule(dynamic,1)
#endif
         for( kk=0 ; kk < nroi ; kk++ )
           THD_roi_mean_ts( mod[mm].dset[jj] , rl->vox+kk , polort ,
                            mod[mm].cmean[kk] + (size_t)jj*mv ) ;
         AFNI_OMP_END ;
         DSET_delete(mod[mm].dset[jj]) ; mod[mm].dset[jj] = NULL ;
         rsa_progress_advance(&progress) ;
       }
     }
   } else {
     int nmain=condition_index?condition_index->ncell:nsub, nload=nmain ;
     for( mm=0 ; mm<nmod ; mm++ ) if( mod[mm].kind==MODK_DSET ) nload+=nsub ;
     rsa_progress_init(&progress,progress_mode,quiet,3,"Loading pattern data",
                       nload,"datasets") ;
     for( jj=0 ; jj < nmain ; jj++ ){
       DSET_load(dset[jj]) ; CHECK_LOAD_ERROR(dset[jj]) ;
       rsa_progress_advance(&progress) ;
     }
     for( mm=0 ; mm < nmod ; mm++ ){
       if( mod[mm].kind != MODK_DSET ) continue ;
       for( jj=0 ; jj < nsub ; jj++ ){
         DSET_load(mod[mm].dset[jj]) ; CHECK_LOAD_ERROR(mod[mm].dset[jj]) ;
         rsa_progress_advance(&progress) ;
       }
     }
   }

   /*-- show how correlated the models are; collinearity is the whole reason
        -model_joint exists, so it should not be a hidden quantity --*/

   if( nmod>1 && run_resolved ){
     int ru ; THD_rdm_ws *w0=THD_rdm_ws_new(nitem,nmod) ;
     THD_simmat **rmv=(THD_simmat **)malloc(sizeof(THD_simmat *)*nmod) ;
     float *cm=(float *)malloc(sizeof(float)*nmod*nmod) ;
     for( ru=0 ; ru<series_runs->nrun ; ru++ ){
       for( mm=0 ; mm<nmod ; mm++ )
         rmv[mm]=(mod[mm].run_mat!=NULL)?mod[mm].run_mat[ru]:mod[mm].mat ;
       THD_rdm_model_corr(nmod,rmv,cmp_metric,w0,cm) ;
       if( !quiet ) INFO_message("3dRSA: correlations among models in run %s --",series_runs->run_label[ru]) ;
       for( ii=0 ; ii<nmod ; ii++ ){
         char line[512],bit[64] ; line[0]='\0' ;
         for( jj=0 ; jj<nmod ; jj++ ){
           snprintf(bit,sizeof(bit)," %7.3f",cm[ii*nmod+jj]) ;
           strncat(line,bit,sizeof(line)-strlen(line)-1) ;
         }
         if( !quiet ) INFO_message("         %-24.24s%s",mod[ii].name,line) ;
       }
       for( ii=0 ; ii<nmod ; ii++ ) for( jj=ii+1 ; jj<nmod ; jj++ ){
         if( joint && fabsf(cm[ii*nmod+jj])>0.99999f )
           ERROR_exit("3dRSA: run %s has indistinguishable joint model columns '%s' and '%s' (r=%.6f)",
                      series_runs->run_label[ru],mod[ii].name,mod[jj].name,cm[ii*nmod+jj]) ;
         if( !quiet && fabsf(cm[ii*nmod+jj])>0.7f )
           WARNING_message("3dRSA: in run %s, '%s' and '%s' correlate at %.3f",
                           series_runs->run_label[ru],mod[ii].name,mod[jj].name,
                           cm[ii*nmod+jj]) ;
       }
     }
     free(cm); free(rmv); THD_rdm_ws_free(w0) ;
   }

   if( !quiet && nmod > 1 && series_file == NULL && !run_resolved ){
     int all_fixed=1 ;
     for( mm=0 ; mm < nmod ; mm++ ) if( mod[mm].mat == NULL ) all_fixed = 0 ;
     if( all_fixed ){
       THD_rdm_ws *w0 = THD_rdm_ws_new( nitem , nmod ) ;
       THD_simmat **mv = (THD_simmat **)malloc(sizeof(THD_simmat *)*nmod) ;
       float *cm = (float *)malloc(sizeof(float)*nmod*nmod) ;
       for( mm=0 ; mm < nmod ; mm++ ) mv[mm] = mod[mm].mat ;
       THD_rdm_model_corr( nmod , mv , cmp_metric , w0 , cm ) ;

       INFO_message("3dRSA: correlations among the models --") ;
       for( ii=0 ; ii < nmod ; ii++ ){
         char line[512] , bit[64] ; line[0] = '\0' ;
         for( jj=0 ; jj < nmod ; jj++ ){
           sprintf(bit," %7.3f",cm[ii*nmod+jj]) ; strcat(line,bit) ;
         }
         INFO_message("         %-24.24s%s",mod[ii].name,line) ;
       }
       for( ii=0 ; ii < nmod ; ii++ )
         for( jj=ii+1 ; jj < nmod ; jj++ )
           if( fabsf(cm[ii*nmod+jj]) > 0.7f )
             WARNING_message("3dRSA: '%s' and '%s' correlate at %.3f -- they will\n"
                             "       split their shared effect unpredictably",
                             mod[ii].name,mod[jj].name,cm[ii*nmod+jj]) ;
       free(cm) ; free(mv) ; THD_rdm_ws_free(w0) ;
     } else {
       /* With a -model_dset, the model matrices differ per ROI, so there is no
          single intercorrelation.  Rather than hide the diagnostic -- and this
          is exactly the case where collinearity matters most, a behavior
          competing with a whole modality -- estimate it over a sample of ROIs
          and report the average and the strongest pair. */
       int nsamp = (nroi < 15) ? nroi : 15 , step = nroi/nsamp , ns ;
       int mf=0 , mp=0 ;
       THD_rdm_ws *w0 = THD_rdm_ws_new( nitem , nmod ) ;
       THD_simmat **mv = (THD_simmat **)malloc(sizeof(THD_simmat *)*nmod) ;
       float *cm = (float *)malloc(sizeof(float)*nmod*nmod) ;
       float *acc = (float *)calloc(nmod*nmod,sizeof(float)) ;
       float *Fs ; float mx=0.0f ; int mxi=0 , mxj=1 ;

       for( mm=0 ; mm < nmod ; mm++ ){
         if( mod[mm].kind != MODK_DSET ) continue ;
         { int nf = (mode==MODE_CONT) ? mod[mm].mvals
                  : (mode==MODE_RDM) ? THD_NTRI(mod[mm].mvals)
                                     : THD_roilist_maxvox(rl)*mod[mm].mvals ;
           if( nf > mf ) mf = nf ; }
         if( mode==MODE_RDM ){
           int np=THD_roilist_maxvox(rl)*mod[mm].mvals ;
           if( np > mp ) mp=np ;
         }
       }
       Fs = (float *)malloc(sizeof(float)*(size_t)nitem*(mf>1?mf:1)) ;
       { float *Fspat = (mode==MODE_RDM) ?
                        (float *)malloc(sizeof(float)*(size_t)(mp>1?mp:1)) : NULL ;

       for( ns=0 ; ns < nsamp ; ns++ ){
         int kk2 = ns*step ;
         for( mm=0 ; mm < nmod ; mm++ ){
           if( mod[mm].kind != MODK_DSET ){ mv[mm] = mod[mm].mat ; continue ; }
           if( mode == MODE_CONT ){
             int mvv = mod[mm].mvals ;
             if( streaming )
               for( jj=0 ; jj < nsub ; jj++ )
                 THD_roi_mean_ts( mod[mm].dset[jj] , rl->vox+kk2 , polort ,
                                  Fs + (size_t)jj*mvv ) ;
             else
               memcpy( Fs , mod[mm].cmean[kk2] , sizeof(float)*(size_t)nsub*mvv ) ;
             mv[mm] = THD_simmat_from_features( nitem , mvv , Fs , neu_metric ) ;
           } else if( mode == MODE_RDM ){
             mv[mm] = rsa_second_order_rdm(rl,kk2,nsub,mod[mm].mvals,
                                            rl->vox[kk2].nar,cond_metric,neu_metric,
                                            mod[mm].dset,NULL,NULL,center_conditions,
                                            Fs,Fspat,NULL,NULL,NULL) ;
           } else {
             int mvv = mod[mm].mvals , nf2 = rl->vox[kk2].nar*mvv ;
             for( jj=0 ; jj < nsub ; jj++ )
               THD_roi_pattern( mod[mm].dset[jj] , rl->vox+kk2 , Fs+(size_t)jj*nf2 ) ;
             mv[mm] = THD_simmat_from_features( nitem , nf2 , Fs , neu_metric ) ;
           }
         }
         THD_rdm_model_corr( nmod , mv , cmp_metric , w0 , cm ) ;
         for( ii=0 ; ii < nmod*nmod ; ii++ ) acc[ii] += cm[ii] ;
         for( mm=0 ; mm < nmod ; mm++ )
           if( mod[mm].kind == MODK_DSET ) THD_simmat_free(mv[mm]) ;
       }
       for( ii=0 ; ii < nmod*nmod ; ii++ ) acc[ii] /= (float)nsamp ;

       INFO_message("3dRSA: mean model correlations over %d ROIs (vary per ROI) --",nsamp) ;
       for( ii=0 ; ii < nmod ; ii++ ){
         char line[512] , bit[64] ; line[0]='\0' ;
         for( jj=0 ; jj < nmod ; jj++ ){
           sprintf(bit," %7.3f",acc[ii*nmod+jj]) ; strcat(line,bit) ;
           if( jj>ii && fabsf(acc[ii*nmod+jj])>mx ){ mx=fabsf(acc[ii*nmod+jj]); mxi=ii; mxj=jj; }
         }
         INFO_message("         %-24.24s%s",mod[ii].name,line) ;
       }
       if( mx > 0.7f )
         WARNING_message("3dRSA: '%s' and '%s' correlate at %.3f on average -- they\n"
                         "       will split their shared effect unpredictably",
                         mod[mxi].name,mod[mxj].name,acc[mxi*nmod+mxj]) ;
       free(Fspat) ; }
       free(Fs) ; free(acc) ; free(cm) ; free(mv) ; THD_rdm_ws_free(w0) ;
     }
   }

   /*================== the analysis ==================*/

   rr = (float **)malloc(sizeof(float *)*nmod) ;
   ee = (float **)malloc(sizeof(float *)*nmod) ;
   pp = (float **)malloc(sizeof(float *)*nmod) ;
   qq = (float **)malloc(sizeof(float *)*nmod) ;
   zz = (float **)malloc(sizeof(float *)*nmod) ;
   if( nboot > 0 ){
     blo = (float **)malloc(sizeof(float *)*nmod) ;
     bhi = (float **)malloc(sizeof(float *)*nmod) ;
   }
   if( ncboot > 0 && !dualboot ){
     cblo = (float **)malloc(sizeof(float *)*nmod) ;
     cbhi = (float **)malloc(sizeof(float *)*nmod) ;
   }
   if( do_nc ){
     ncA = (float *)calloc(nroi,sizeof(float)) ;
     ncB = (float *)calloc(nroi,sizeof(float)) ;
   }
   if( do_loo ){
     lr = (float **)malloc(sizeof(float *)*nmod) ;
     lp = (float **)malloc(sizeof(float *)*nmod) ;
     lq = (float **)malloc(sizeof(float *)*nmod) ;
     lz = (float **)malloc(sizeof(float *)*nmod) ;
     if( nboot>0 ){
       lblo=(float **)malloc(sizeof(float *)*nmod) ;
       lbhi=(float **)malloc(sizeof(float *)*nmod) ;
     }
     for( mm=0 ; mm < nmod ; mm++ ){
       lr[mm] = (float *)calloc(nroi,sizeof(float)) ;
       lp[mm] = (float *)calloc(nroi,sizeof(float)) ;
       lq[mm] = (float *)calloc(nroi,sizeof(float)) ;
       lz[mm] = (float *)calloc(nroi,sizeof(float)) ;
       if( nboot>0 ){
         lblo[mm]=(float *)calloc(nroi,sizeof(float)) ;
         lbhi[mm]=(float *)calloc(nroi,sizeof(float)) ;
       }
     }
     if( nloo == 0 )
       WARNING_message("3dRSA: -loo has no data-table -model target to predict\n"
                       "       (model_mat and model_dset are skipped)") ;
   }
   for( mm=0 ; mm < nmod ; mm++ ){
     rr[mm] = (float *)calloc(nroi,sizeof(float)) ;
     ee[mm] = (float *)calloc(nroi,sizeof(float)) ;
     pp[mm] = (float *)calloc(nroi,sizeof(float)) ;
     qq[mm] = (float *)calloc(nroi,sizeof(float)) ;
     zz[mm] = (float *)calloc(nroi,sizeof(float)) ;
     if( nboot > 0 ){
       blo[mm] = (float *)calloc(nroi,sizeof(float)) ;
       bhi[mm] = (float *)calloc(nroi,sizeof(float)) ;
     }
     if( ncboot > 0 && !dualboot ){
       cblo[mm] = (float *)calloc(nroi,sizeof(float)) ;
       cbhi[mm] = (float *)calloc(nroi,sizeof(float)) ;
     }
   }
   if( run_resolved ){
     int nrout=nmod*series_runs->nrun ;
     run_rr=(float **)malloc(sizeof(float *)*nrout) ;
     run_ee=(float **)malloc(sizeof(float *)*nrout) ;
     run_pp=(float **)malloc(sizeof(float *)*nrout) ;
     run_qq=(float **)malloc(sizeof(float *)*nrout) ;
     run_zz=(float **)malloc(sizeof(float *)*nrout) ;
     for( ii=0 ; ii<nrout ; ii++ ){
       run_rr[ii]=(float *)calloc(nroi,sizeof(float)) ;
       run_ee[ii]=(float *)calloc(nroi,sizeof(float)) ;
       run_pp[ii]=(float *)calloc(nroi,sizeof(float)) ;
       run_qq[ii]=(float *)calloc(nroi,sizeof(float)) ;
       run_zz[ii]=(float *)calloc(nroi,sizeof(float)) ;
     }
     if( nrunconspec>0 ){
       int nrc=nmod*nrunconspec ;
       rcon_rr=(float **)malloc(sizeof(float *)*nrc) ;
       rcon_ee=(float **)malloc(sizeof(float *)*nrc) ;
       rcon_pp=(float **)malloc(sizeof(float *)*nrc) ;
       rcon_qq=(float **)malloc(sizeof(float *)*nrc) ;
       rcon_zz=(float **)malloc(sizeof(float *)*nrc) ;
       for( ii=0 ; ii<nrc ; ii++ ){
         rcon_rr[ii]=(float *)calloc(nroi,sizeof(float)) ;
         rcon_ee[ii]=(float *)calloc(nroi,sizeof(float)) ;
         rcon_pp[ii]=(float *)calloc(nroi,sizeof(float)) ;
         rcon_qq[ii]=(float *)calloc(nroi,sizeof(float)) ;
         rcon_zz[ii]=(float *)calloc(nroi,sizeof(float)) ;
       }
     }
   }
   if( ncon > 0 ){
     int cc ;
     crd = (float **)malloc(sizeof(float *)*ncon) ;
     cd = (float **)malloc(sizeof(float *)*ncon) ;
     ce = (float **)malloc(sizeof(float *)*ncon) ;
     cp = (float **)malloc(sizeof(float *)*ncon) ;
     cq = (float **)malloc(sizeof(float *)*ncon) ;
     cz = (float **)malloc(sizeof(float *)*ncon) ;
     if( nboot > 0 ){
       dblo = (float **)malloc(sizeof(float *)*ncon) ;
       dbhi = (float **)malloc(sizeof(float *)*ncon) ;
     }
     for( cc=0 ; cc < ncon ; cc++ ){
       crd[cc] = (float *)calloc(nroi,sizeof(float)) ;
       cd[cc] = (float *)calloc(nroi,sizeof(float)) ;
       ce[cc] = (float *)calloc(nroi,sizeof(float)) ;
       cp[cc] = (float *)calloc(nroi,sizeof(float)) ;
       cq[cc] = (float *)calloc(nroi,sizeof(float)) ;
       cz[cc] = (float *)calloc(nroi,sizeof(float)) ;
       if( nboot > 0 ){
         dblo[cc] = (float *)calloc(nroi,sizeof(float)) ;
         dbhi[cc] = (float *)calloc(nroi,sizeof(float)) ;
       }
     }
   }
   if( ncomq > 0 ){
     int qq2 ;
     cav = (float **)malloc(sizeof(float *)*ncomq) ;
     cap = (float **)malloc(sizeof(float *)*ncomq) ;
     caq = (float **)malloc(sizeof(float *)*ncomq) ;
     caz = (float **)malloc(sizeof(float *)*ncomq) ;
     if( nboot > 0 ){
       calo = (float **)malloc(sizeof(float *)*ncomq) ;
       cahi = (float **)malloc(sizeof(float *)*ncomq) ;
     }
     for( qq2=0 ; qq2 < ncomq ; qq2++ ){
       cav[qq2] = (float *)calloc(nroi,sizeof(float)) ;
       cap[qq2] = (float *)calloc(nroi,sizeof(float)) ;
       caq[qq2] = (float *)calloc(nroi,sizeof(float)) ;
       caz[qq2] = (float *)calloc(nroi,sizeof(float)) ;
       if( nboot > 0 ){
         calo[qq2] = (float *)calloc(nroi,sizeof(float)) ;
         cahi[qq2] = (float *)calloc(nroi,sizeof(float)) ;
       }
     }
   }
   if( nfit>0 ){
     fr=(float **)malloc(sizeof(float *)*nfit) ;
     fpv=(float **)malloc(sizeof(float *)*nfit) ;
     fqv=(float **)malloc(sizeof(float *)*nfit) ;
     fzv=(float **)malloc(sizeof(float *)*nfit) ;
     for( ii=0 ; ii<nfit ; ii++ ){
       fr[ii]=(float *)calloc(nroi,sizeof(float)) ;
       fpv[ii]=(float *)calloc(nroi,sizeof(float)) ;
       fqv[ii]=(float *)calloc(nroi,sizeof(float)) ;
       fzv[ii]=(float *)calloc(nroi,sizeof(float)) ;
     }
     fwgt=(float **)malloc(sizeof(float *)*nfitw) ;
     for( ii=0 ; ii<nfitw ; ii++ ) fwgt[ii]=(float *)calloc(nroi,sizeof(float)) ;
   }
   if( nfitcon>0 ){
     fcd=(float **)malloc(sizeof(float *)*nfitcon) ;
     fcp=(float **)malloc(sizeof(float *)*nfitcon) ;
     fcq=(float **)malloc(sizeof(float *)*nfitcon) ;
     fcz=(float **)malloc(sizeof(float *)*nfitcon) ;
     for( ii=0 ; ii<nfitcon ; ii++ ){
       fcd[ii]=(float *)calloc(nroi,sizeof(float)) ;
       fcp[ii]=(float *)calloc(nroi,sizeof(float)) ;
       fcq[ii]=(float *)calloc(nroi,sizeof(float)) ;
       fcz[ii]=(float *)calloc(nroi,sizeof(float)) ;
     }
   }

   regout = joint || (nort > 0) ;   /* output reports b/partial-r, not plain r */

   if( !quiet && progress_mode!=RSA_PROGRESS_OFF )
     INFO_message("3dRSA [3/5] Preparing synchronized null and bootstrap work...") ;

   /* Normalize -block once for both the label null and the stratified subject
      bootstrap.  The labels are strata: resampling remains at subject level
      inside each label and preserves the original label counts. */
   if( block_col != NULL ){
     int bcol=THD_datatable_column(tab,block_col),jj2,kk2 ;
     if( bcol<0 ) ERROR_exit("3dRSA: -block column '%s' is not in the data table",block_col) ;
     block_lab=(int *)malloc(sizeof(int)*nsub) ;
     if( block_lab==NULL ) ERROR_exit("3dRSA: cannot allocate -block labels") ;
     for( jj2=0 ; jj2<nsub ; jj2++ ){
       char *cj=DT_CELL(tab,jj2,bcol) ; block_lab[jj2]=-1 ;
       for( kk2=0 ; kk2<jj2 ; kk2++ )
         if( strcmp(DT_CELL(tab,kk2,bcol),cj)==0 ){
           block_lab[jj2]=block_lab[kk2] ; break ;
         }
       if( block_lab[jj2]<0 ) block_lab[jj2]=nblock++ ;
     }
     if( !quiet )
       INFO_message("3dRSA: exchangeability/bootstrap strata from '%s' (%d blocks)",
                    block_col,nblock) ;
   }
   if( nperm>0 && contrast_hypothesis==CONTRAST_SUPERIORITY &&
       ncon>0 && rdm_over==RDM_SUBJ ){
     contrast_rset=rsa_isrsa_resample_set(nsub,nperm,seed,block_lab) ;
     if( contrast_rset==NULL )
       ERROR_exit("3dRSA: cannot build %d valid paired subject-bootstrap null draws",nperm) ;
     if( !quiet )
       INFO_message("3dRSA: %d centered paired subject-bootstrap draws for IS-RSA superiority",
                    nperm) ;
   }
   if( nperm>0 && contrast_hypothesis==CONTRAST_SUPERIORITY && nfitcon>0 ){
     fit_contrast_rset=(block_lab!=NULL)
       ? THD_resample_set_build_stratified(nsub,nperm,seed,block_lab)
       : THD_resample_set_build(nsub,nperm,seed) ;
     if( fit_contrast_rset==NULL )
       ERROR_exit("3dRSA: cannot build %d paired outer-subject fitted-model bootstrap draws",nperm) ;
     if( !quiet )
       INFO_message("3dRSA: %d centered paired outer-subject bootstrap draws for fitted-model superiority",
                    nperm) ;
   }

   /* Build ONE null descriptor, shared read-only by every ROI/voxel.  The
      temporal nulls get circular offsets or a stateless phase family;
      otherwise IS-RSA permutes subject labels and classic RSA sign-flips
      per-subject correlations. */
   if( nperm > 0 ){
     if( null_mode == NULL_TIMESHIFT ){
       tset=THD_timeshift_set_build(nsub,nvals,nperm,min_shift,seed) ;
       if( tset == NULL ) ERROR_exit("3dRSA: cannot build the circular time-shift set") ;
       tsneed=(unsigned char *)calloc((size_t)ntri*nvals,sizeof(unsigned char)) ;
       if( tsneed == NULL ) ERROR_exit("3dRSA: cannot allocate the relative-lag index") ;
       { int ss2,ii2,jj2,pair2,lag2 ;
         for( ss2=0 ; ss2<tset->nshift ; ss2++ ){
           int *off=tset->offset+(size_t)ss2*nsub ; pair2=0 ;
           for( ii2=0 ; ii2<nsub ; ii2++ ) for( jj2=ii2+1 ; jj2<nsub ; jj2++,pair2++ ){
             lag2=(off[jj2]-off[ii2])%nvals ; if( lag2<0 ) lag2+=nvals ;
             tsneed[(size_t)pair2*nvals+lag2]=1 ;
           }
         }
       }
       if( !quiet )
         INFO_message("3dRSA: circular-shift null: %d sets, minimum %d TR%s",
                      nperm,min_shift,(min_shift==1)?"":"s") ;
     } else if( null_mode == NULL_PHASE ){
       phset=THD_phase_set_build(nsub,nvals,nperm,seed) ;
       if( phset == NULL ) ERROR_exit("3dRSA: cannot build the phase-randomization set") ;
       if( !quiet )
         INFO_message("3dRSA: phase-randomization null: %d sets, %d randomized Fourier bins",
                      nperm,phset->nfreq) ;
     } else if( !(rdm_over==RDM_BRICK &&
                  classic_null==CLASSIC_NULL_CONDITIONS) ){
       PERM_scheme *psch = THD_perm_scheme_new( nsub ) ;
       if( psch == NULL ) ERROR_exit("3dRSA: cannot build a permutation scheme") ;
       psch->exchange = (rdm_over == RDM_SUBJ) ? PERM_EE : PERM_ISE ;
       psch->exact    = 0 ;          /* respect -nperm; auto-exact only if it is free */
       psch->nperm    = nperm ;
       psch->seed     = seed ;

       if( block_lab != NULL ) THD_perm_scheme_set_blocks(psch,block_lab) ;

       pset = THD_perm_set_build( psch ) ;
       THD_perm_scheme_free( psch ) ;
       if( pset == NULL ) ERROR_exit("3dRSA: cannot build the permutation set") ;
       if( pset->nperm != nperm && !quiet )
         INFO_message("3dRSA: using %d relabelings (from -nperm %d)",pset->nperm,nperm) ;
     }

     /* One max-statistic null per model, pooled over ROIs/voxels for FWE.  Slot
        mxflat[mm*npfwe + pk] holds the largest |statistic| any element reached
        under relabeling pk; sorted after the sweep it turns each element's
        observed statistic into an FWE-corrected p (Nichols-Holmes 2002).  This
        is the correct multiple-comparisons correction for the searchlight. */
     do_fwe = 1 ;
     /* F15/F7 and S1: commonality, fitted models, and the optional classic
        fixed-effects primary null use one immutable condition-label set shared
        across every subject and location.  Its size can differ from the
        subject sign-flip set, so establish it before sizing the primary FWE. */
     if( (classic_null==CLASSIC_NULL_CONDITIONS || ncomq>0 || nfit>0) &&
         rdm_over == RDM_BRICK ){
       PERM_scheme *csch=THD_perm_scheme_new(nitem) ;
       if( csch == NULL ) ERROR_exit("3dRSA: cannot build the condition-label scheme") ;
       csch->exchange=PERM_EE ; csch->exact=0 ; csch->nperm=nperm ; csch->seed=seed ;
       cpset=THD_perm_set_build(csch) ; THD_perm_scheme_free(csch) ;
       if( cpset == NULL ) ERROR_exit("3dRSA: cannot build condition relabelings") ;
       if( !quiet )
         INFO_message("3dRSA: classic %s uses %d synchronized condition relabelings",
                      (classic_null==CLASSIC_NULL_CONDITIONS)?
                        ((ncomq>0 || nfit>0)?"primary/follow-on inference":"primary inference"):
                      (ncomq>0 && nfit>0)?"commonality/fitted models":
                      (ncomq>0)?"commonality":"fitted models",cpset->nperm) ;
     }
     npfwe = (tset != NULL) ? tset->nshift
            : (phset != NULL) ? phset->nphase
            : (classic_null==CLASSIC_NULL_CONDITIONS) ? cpset->nperm
                                                       : pset->nperm ;
     ncaperm=(cpset!=NULL && rdm_over==RDM_BRICK)?cpset->nperm:npfwe ;
     mxflat = (float *)malloc(sizeof(float)*(size_t)nmod*npfwe) ;
     for( ii=0 ; ii < nmod*npfwe ; ii++ ) mxflat[ii] = -FLT_MAX ;
     pf = (float **)malloc(sizeof(float *)*nmod) ;
     zf = (float **)malloc(sizeof(float *)*nmod) ;
     for( mm=0 ; mm < nmod ; mm++ ){
       pf[mm] = (float *)calloc(nroi,sizeof(float)) ;
       zf[mm] = (float *)calloc(nroi,sizeof(float)) ;
     }
     if( run_resolved ){
       int nrout=nmod*series_runs->nrun ;
       run_mxflat=(float *)malloc(sizeof(float)*(size_t)nmod*npfwe) ;
       run_pf=(float **)malloc(sizeof(float *)*nrout) ;
       run_zf=(float **)malloc(sizeof(float *)*nrout) ;
       for( ii=0 ; ii<nmod*npfwe ; ii++ ) run_mxflat[ii]=-FLT_MAX ;
       for( ii=0 ; ii<nrout ; ii++ ){
         run_pf[ii]=(float *)calloc(nroi,sizeof(float)) ;
         run_zf[ii]=(float *)calloc(nroi,sizeof(float)) ;
       }
       if( nrunconspec>0 ){
         int nrc=nmod*nrunconspec ;
         rcon_pf=(float **)malloc(sizeof(float *)*nrc) ;
         rcon_zf=(float **)malloc(sizeof(float *)*nrc) ;
         for( ii=0 ; ii<nrc ; ii++ ){
           rcon_pf[ii]=(float *)calloc(nroi,sizeof(float)) ;
           rcon_zf[ii]=(float *)calloc(nroi,sizeof(float)) ;
         }
       }
     }

     /* LOO prediction is a different statistic (cross-validated accuracy), so it
        gets its OWN max-null family; only scalar-column models are predicted. */
     do_loofwe = ( do_loo && nloo > 0 ) ;
     if( do_loofwe ){
       lmxflat = (float *)malloc(sizeof(float)*(size_t)nloofam*npfwe) ;
       for( ii=0 ; ii < nloofam*npfwe ; ii++ ) lmxflat[ii] = -FLT_MAX ;
       lpf = (float **)malloc(sizeof(float *)*nmod) ;
       lzf = (float **)malloc(sizeof(float *)*nmod) ;
       for( mm=0 ; mm < nmod ; mm++ ){
         lpf[mm] = (float *)calloc(nroi,sizeof(float)) ;
         lzf[mm] = (float *)calloc(nroi,sizeof(float)) ;
       }
     }

     /* each model contrast is its own statistic (a difference), so its own
        max-null family, driven by the same shared relabelings */
     do_confwe = ( ncon > 0 ) ;
     if( do_confwe ){
       int cc ;
       cmxflat = (float *)malloc(sizeof(float)*(size_t)ncon*npfwe) ;
       for( ii=0 ; ii < ncon*npfwe ; ii++ ) cmxflat[ii] = -FLT_MAX ;
       cpf = (float **)malloc(sizeof(float *)*ncon) ;
       czf = (float **)malloc(sizeof(float *)*ncon) ;
       for( cc=0 ; cc < ncon ; cc++ ){
         cpf[cc] = (float *)calloc(nroi,sizeof(float)) ;
         czf[cc] = (float *)calloc(nroi,sizeof(float)) ;
       }
     }

     /* each commonality QUANTITY (three raw plus two partial-R2 per request) is
        its own statistic, so its own max-null family */
     do_cafwe = ( ncomq > 0 ) ;
     if( do_cafwe ){
       int qq2 ;
       camx = (float *)malloc(sizeof(float)*(size_t)ncomq*ncaperm) ;
       for( ii=0 ; ii < ncomq*ncaperm ; ii++ ) camx[ii] = -FLT_MAX ;
       capf = (float **)malloc(sizeof(float *)*ncomq) ;
       cazf = (float **)malloc(sizeof(float *)*ncomq) ;
       for( qq2=0 ; qq2 < ncomq ; qq2++ ){
         capf[qq2] = (float *)calloc(nroi,sizeof(float)) ;
         cazf[qq2] = (float *)calloc(nroi,sizeof(float)) ;
       }
     }
     do_fitfwe=(nfit>0) ;
     if( do_fitfwe ){
       nfitperm=(rdm_over==RDM_BRICK)?cpset->nperm:pset->nperm ;
       fmx=(float *)malloc(sizeof(float)*(size_t)nfit*nfitperm) ;
       fpf=(float **)malloc(sizeof(float *)*nfit) ;
       fzf=(float **)malloc(sizeof(float *)*nfit) ;
       for( ii=0 ; ii<nfit*nfitperm ; ii++ ) fmx[ii]=-FLT_MAX ;
       for( ii=0 ; ii<nfit ; ii++ ){
         fpf[ii]=(float *)calloc(nroi,sizeof(float)) ;
         fzf[ii]=(float *)calloc(nroi,sizeof(float)) ;
       }
     }
     do_fitconfwe=(nfitcon>0) ;
     if( do_fitconfwe ){
       fcmx=(float *)malloc(sizeof(float)*(size_t)nfitcon*nfitperm) ;
       fcpf=(float **)malloc(sizeof(float *)*nfitcon) ;
       fczf=(float **)malloc(sizeof(float *)*nfitcon) ;
       for( ii=0 ; ii<nfitcon*nfitperm ; ii++ ) fcmx[ii]=-FLT_MAX ;
       for( ii=0 ; ii<nfitcon ; ii++ ){
         fcpf[ii]=(float *)calloc(nroi,sizeof(float)) ;
         fczf[ii]=(float *)calloc(nroi,sizeof(float)) ;
       }
     }
   }

   /* F9/F23: in a label-null searchlight, fixed model matrices and the shared
      relabeling set never change.  Materialize their centered/ranked permuted
      triangles once, then let every sphere and OpenMP worker read that cache.
      Per-location -model_dset matrices and other comparison/null paths retain
      their ordinary implementation. */
   if( streaming && !run_resolved && rdm_over==RDM_SUBJ && pset != NULL &&
       (cmp_metric==CMP_PEARSON || cmp_metric==CMP_SPEARMAN ||
        cmp_metric==CMP_RHOA) &&
       ((!joint && nort==0) || ncon>0) ){
     THD_simmat **cmat ; int nf=0 ; size_t cb ;
     mcache_ix=(int *)malloc(sizeof(int)*nmod) ;
     cmat=(THD_simmat **)malloc(sizeof(THD_simmat *)*nmod) ;
     for( mm=0 ; mm < nmod ; mm++ ){
       mcache_ix[mm]=-1 ;
       if( mod[mm].kind != MODK_DSET ){
         mcache_ix[mm]=nf ; cmat[nf++]=mod[mm].mat ;
       }
     }
     if( nf > 0 ) mcache=THD_mantel_cache_build(nf,cmat,cmp_metric,pset) ;
     free(cmat) ;
     if( mcache == NULL ){
       free(mcache_ix) ; mcache_ix=NULL ;
       if( nf > 0 && !quiet )
         WARNING_message("3dRSA: could not allocate the fixed-model Mantel cache;\n"
                         "       continuing with the exact uncached path") ;
     } else if( !quiet ){
       cb=THD_mantel_cache_bytes(mcache) ;
       INFO_message("3dRSA: cached %d fixed model%s x %d relabelings (%.3f GiB)\n"
                    "       for Pearson/Spearman/rho-a searchlight Mantel tests",
                    nf,(nf==1)?"":"s",pset->nperm,
                    (double)cb/1073741824.0) ;
     }
   }

   /* Bootstrap uncertainty is materialized independently of the permutation
      null.  The same immutable subject draws are shared across all map elements
      for thread-count reproducibility and spatially comparable intervals. */
   if( nboot > 0 ){
     rset = (block_lab != NULL)
              ? THD_resample_set_build_stratified(nsub,nboot,seed,block_lab)
              : THD_resample_set_build(nsub,nboot,seed) ;
     if( rset == NULL ) ERROR_exit("3dRSA: cannot build %d subject-bootstrap samples",nboot) ;
     if( !quiet && !dualboot )
       INFO_message("3dRSA: %d %ssubject-bootstrap samples, %.3g%% percentile CI",
                    nboot,(block_lab!=NULL)?"within-stratum ":"",boot_ci) ;
   }
   if( ncboot > 0 ){
     crset = rsa_cond_resample_build(nitem,ncboot,seed,cond_group_file) ;
     if( crset == NULL ) ERROR_exit("3dRSA: cannot build condition-bootstrap samples") ;
     if( !quiet ){
       if( dualboot )
         INFO_message("3dRSA: %d synchronized subject x condition draws over %d "
                      "condition group%s; %.3g%% corrected-variance t CI (df=%d)%s",
                      ncboot,crset->ngroup,(crset->ngroup==1)?"":"s",boot_ci,
                      ((nsub<crset->ngroup)?nsub:crset->ngroup)-1,
                      (crset->nvalid<ncboot)?"; invalid condition draws omitted":"") ;
       else
         INFO_message("3dRSA: %d condition-bootstrap samples over %d group%s, "
                      "%.3g%% percentile CI%s",
                      ncboot,crset->ngroup,(crset->ngroup==1)?"":"s",boot_ci,
                      (crset->nvalid<ncboot)?" (draws with <3 distinct conditions omitted)":"") ;
     }
   }

   if( !quiet ){
     if( run_resolved ){
       int nrp=(pset!=NULL)?pset->nperm:nperm ;
       INFO_message("3dRSA: %d run-specific %d x %d matrices, %d model%s, %d permutation%s;\n"
                    "       %s with synchronized whole-subject trajectories across run x space",
                    series_runs->nrun,nitem,nitem,nmod,(nmod==1)?"":"s",
                    nrp,(nrp==1)?"":"s",
                    run_analysis==RUN_ANALYSIS_SEPARATE?"separate effects + equal-run mean":"equal-run mean") ;
       if( joint )
         INFO_message("3dRSA: run-resolved joint model uses standardized coefficients\n"
                      "       (and partial r) conditional on every other model per run;\n"
                      "       model-specific Freedman-Lane nulls are synchronized across runs/space") ;
     } else if( series_file != NULL )
       INFO_message("3dRSA: %d x %d matrices, %d time points, %d permutation%s;\n"
                    "       primary effects with joint time x space FDR/FWE",
                    nitem,nitem,nseries,nperm,(nperm==1)?"":"s") ;
     else
       INFO_message("3dRSA: %d x %d matrices, %d model%s, %d permutation%s, %s",
                    nitem,nitem,nmod,(nmod==1)?"":"s",nperm,(nperm==1)?"":"s",
                    joint ? "joint regression"
                          : (nort>0) ? "separate + nuisances removed"
                                     : "models tested separately") ;
   }

   if( nperm <= 0 && rdm_over == RDM_SUBJ ){
     if( nrunconspec>0 )
       WARNING_message("3dRSA: -nperm 0 with run contrasts does NO inference -- the dyads\n"
                       "       are not independent. p/q are -1; MEAN/run _FZ maps are\n"
                       "       uncalibrated Fisher transforms and contrast _U maps are raw,\n"
                       "       uncalibrated effect differences. Use -nperm > 0 for reporting.") ;
     else WARNING_message("3dRSA: -nperm 0 with IS-RSA does NO inference -- the dyads\n"
                     "       are not independent, so there is no valid parametric\n"
                     "       p/z.  p/q columns are -1 and the z sub-brick is an\n"
                     "       uncalibrated Fisher-z effect map ('_FZ', not FIZT).\n"
                     "       Use -nperm > 0 for any reportable statistic.") ;
   }

   if( ncon > 0 && !quiet ){
     int cc ;
     for( cc=0 ; cc < ncon ; cc++ )
      INFO_message("3dRSA: contrast '%s' = %s - %s%s",
                    con[cc].name , mod[con[cc].ia].name , mod[con[cc].ib].name ,
                    (rdm_over==RDM_SUBJ && contrast_hypothesis==CONTRAST_SUPERIORITY && nperm>0)
                                          ? " (centered paired subject-bootstrap superiority)"
                       : (rdm_over==RDM_SUBJ && contrast_hypothesis==CONTRAST_SUPERIORITY)
                                          ? " (superiority point estimate; no inference)"
                       : (rdm_over==RDM_SUBJ) ? " (shared-relabeling alignment)"
                       : (classic_null==CLASSIC_NULL_CONDITIONS)
                           ? " (paired fixed-subject condition relabeling)"
                       : group_test ? " (paired signed-rank)" : " (paired sign-flip)") ;
   }

   rsa_progress_init(&progress,progress_mode,quiet,4,"Inference",nroi,
                     streaming ? "searchlights" : "ROIs") ;

 AFNI_OMP_START ;
#pragma omp parallel
 {
   THD_rdm_ws *ws , *cws=NULL ;
   THD_simmat *neural=NULL , *tsneural=NULL , **mv ;
   THD_simmat **rneural=NULL ;
   float *F , *tsmain=NULL , *tslag=NULL , *tsprep=NULL , *tsnorm=NULL ;
   float *rF=NULL,*rtri=NULL,*rstat=NULL,*rprstat=NULL,*rpval=NULL,*rzscr=NULL,*rnull=NULL,*mnull=NULL,*jmz=NULL ;
   float *rcstat=NULL,*rcpr=NULL,*rcpval=NULL,*rczscr=NULL,*rcnull=NULL,*rcsum=NULL ;
   int *rnge=NULL,*rcnge=NULL ;
   complex *phspec=NULL , *phwork=NULL ;
   float *phseries=NULL , *phsc1=NULL , *phsc2=NULL ;
   float *rsub=NULL , *beta , *prtl , *pv ;
   float *tsz=NULL , *tscd=NULL , *tscp=NULL , *tscz=NULL ;
   float *bdraw=NULL,*hdraw=NULL ; int *bseen=NULL ; /* subject-bootstrap scratch */
   float *br_y=NULL , *br_xflat=NULL , **brx=NULL , *brcoef=NULL , *brbeta=NULL ;
   float *bca_y=NULL , *bca_a=NULL , *bca_b=NULL , *bca_c=NULL , *bcadraw=NULL ;
   float *cb_y=NULL , *cb_xflat=NULL , **cbx=NULL , *cbsum=NULL ;
   float *cbdraw=NULL , *cbbeta=NULL ;           /* condition-bootstrap scratch */
   float *dualval=NULL , *dualwork=NULL ;         /* F6 subject x condition scratch */
   float *Fh=NULL , *triA=NULL , *triB=NULL,*covA=NULL,*covB=NULL ; /* noise ceiling */
   float *ipat=NULL ;                    /* inner condition-pattern RDM scratch */
   float *srdm=NULL , *mtriflat=NULL , **mtri=NULL ; /* cached classic-RSA triangles */
   float *srdmcov=NULL,*mcovflat=NULL,**mcov=NULL ;   /* F4 second-moment forms */
   float *my_mx=NULL , *pnull=NULL ;    /* per-thread FWE max-null + scratch */
   float *run_my_mx=NULL ;              /* per-thread max over run x space */
   float *l_mx=NULL ;                   /* per-thread LOO max-null */
   float *lbpred=NULL,*lbx=NULL,*lby=NULL ; float **lbtarg=NULL ;
   float *c_mx=NULL , *cpnull=NULL , *dsub=NULL ;  /* per-thread contrast scratch */
   float *ca_mx=NULL , *capnull=NULL , *casub=NULL ; /* commonality null/subject scratch */
   RSA_fitws **fitws=NULL ; float *f_mx=NULL,*fnull=NULL,*fc_mx=NULL,*fcnull=NULL ;
   float *fitfoldz=NULL ; unsigned char *fitvalid=NULL ; int nfitfold=0 ;
   float **rpat=NULL,*runraw=NULL ; int maxrun_l=0 ; /* crossnobis mapped/run patterns */
   RSA_whiten wh ;                                 /* per-thread noise-whitening scratch */
   int nfeat_max , npat_max=0 , maxvox_l , kk_ , mm_ , jj_ , nvx ;

   { int wncol=joint ? nmod+nort : ((nort>0) ? 1+nort : 1) ;
     if( ncomq>0 && wncol<3 ) wncol=3 ;
     ws=THD_rdm_ws_new(nitem,wncol) ; }
   mv   = (THD_simmat **)malloc(sizeof(THD_simmat *)*nmod) ;
   beta = (float *)malloc(sizeof(float)*nmod) ;
   prtl = (float *)malloc(sizeof(float)*nmod) ;
   pv   = (float *)malloc(sizeof(float)*nmod) ;
   if( tset != NULL || phset != NULL ){
     tsz=(float *)malloc(sizeof(float)*nmod) ;
     if( ncon > 0 ){
       tscd=(float *)malloc(sizeof(float)*ncon) ;
       tscp=(float *)malloc(sizeof(float)*ncon) ;
       tscz=(float *)malloc(sizeof(float)*ncon) ;
     }
   }
   if( nboot > 0 ){
     bdraw = (float *)malloc(sizeof(float)*nboot) ;
     if( rdm_over == RDM_SUBJ ) bseen = (int *)malloc(sizeof(int)*nsub) ;
     if( do_loo ){
       lbpred=(float *)malloc(sizeof(float)*(size_t)maxloocol*nsub) ;
       lbx=(float *)malloc(sizeof(float)*nsub) ;
       lby=(float *)malloc(sizeof(float)*nsub) ;
       lbtarg=(float **)malloc(sizeof(float *)*maxloocol) ;
     }
     if( rdm_over == RDM_SUBJ && (joint || nort > 0) ){
       int bc, bncol=nmod+nort ;
       br_y=(float *)malloc(sizeof(float)*ntri) ;
       br_xflat=(float *)malloc(sizeof(float)*(size_t)bncol*ntri) ;
       brx=(float **)malloc(sizeof(float *)*bncol) ;
       brcoef=(float *)malloc(sizeof(float)*(size_t)nmod*nboot) ;
       brbeta=(float *)malloc(sizeof(float)*bncol) ;
       for( bc=0 ; bc < bncol ; bc++ ) brx[bc]=br_xflat+(size_t)bc*ntri ;
     }
     if( ncomq > 0 ){
       bca_y=(float *)malloc(sizeof(float)*ntri) ;
       bca_a=(float *)malloc(sizeof(float)*ntri) ;
       bca_b=(float *)malloc(sizeof(float)*ntri) ;
       bca_c=(float *)malloc(sizeof(float)*ntri) ;
       bcadraw=(float *)malloc(sizeof(float)*(size_t)RSA_MAXCOMMON*nboot) ;
     }
   }
   if( contrast_rset!=NULL || fit_contrast_rset!=NULL ){
     int nh=(contrast_rset!=NULL)?contrast_rset->nresample:0 ;
     if( fit_contrast_rset!=NULL && fit_contrast_rset->nresample>nh )
       nh=fit_contrast_rset->nresample ;
     hdraw=(float *)malloc(sizeof(float)*(size_t)nh) ;
   }
   if( contrast_rset!=NULL ){
     if( bseen==NULL ) bseen=(int *)malloc(sizeof(int)*nsub) ;
   }
   if( ncboot > 0 ){
     cws=THD_rdm_ws_new(crset->maxitem,nmod) ;
     cb_y=(float *)malloc(sizeof(float)*crset->maxtri) ;
     cb_xflat=(float *)malloc(sizeof(float)*(size_t)nmod*crset->maxtri) ;
     cbx=(float **)malloc(sizeof(float *)*nmod) ;
     if( !dualboot ) cbsum=(float *)malloc(sizeof(float)*(size_t)nmod*ncboot) ;
     cbdraw=(float *)malloc(sizeof(float)*ncboot) ; cbbeta=(float *)malloc(sizeof(float)*nmod) ;
     if( dualboot ){
       size_t ndv=(size_t)(joint?nmod:1)*nsub*ncboot ;
       dualval=(float *)malloc(sizeof(float)*ndv) ;
       dualwork=(float *)malloc(sizeof(float)*(size_t)(nboot+2*ncboot)) ;
     }
     for( mm_=0 ; mm_ < nmod ; mm_++ ) cbx[mm_]=cb_xflat+(size_t)mm_*crset->maxtri ;
   }
   if( ncon > 0 ) dsub = (float *)malloc(sizeof(float)*nsub) ;
   if( do_fwe ){
     int fi ;
     my_mx = (float *)malloc(sizeof(float)*(size_t)nmod*npfwe) ;
     pnull = (float *)malloc(sizeof(float)*(size_t)nmod*npfwe) ;
     for( fi=0 ; fi < nmod*npfwe ; fi++ ) my_mx[fi] = -FLT_MAX ;
     if( do_loofwe ){
       l_mx = (float *)malloc(sizeof(float)*(size_t)nloofam*npfwe) ;
       for( fi=0 ; fi < nloofam*npfwe ; fi++ ) l_mx[fi] = -FLT_MAX ;
     }
     if( do_confwe ){
       int fj ;
       c_mx   = (float *)malloc(sizeof(float)*(size_t)ncon*npfwe) ;
       cpnull = (float *)malloc(sizeof(float)*(size_t)ncon*npfwe) ;
       for( fj=0 ; fj < ncon*npfwe ; fj++ ) c_mx[fj] = -FLT_MAX ;
     }
     if( do_cafwe ){
       int fj ;
       ca_mx   = (float *)malloc(sizeof(float)*(size_t)ncomq*ncaperm) ;
       capnull = (float *)malloc(sizeof(float)*(size_t)RSA_MAXCOMMON*ncaperm) ;
       for( fj=0 ; fj < ncomq*ncaperm ; fj++ ) ca_mx[fj] = -FLT_MAX ;
     }
     if( do_fitfwe ){
       int fj ;
       f_mx=(float *)malloc(sizeof(float)*(size_t)nfit*nfitperm) ;
       fnull=(float *)malloc(sizeof(float)*(size_t)nfit*nfitperm) ;
       for( fj=0 ; fj<nfit*nfitperm ; fj++ ) f_mx[fj]=-FLT_MAX ;
     }
     if( do_fitconfwe ){
       int fj ;
       fc_mx=(float *)malloc(sizeof(float)*(size_t)nfitcon*nfitperm) ;
       fcnull=(float *)malloc(sizeof(float)*(size_t)nfitcon*nfitperm) ;
       for( fj=0 ; fj<nfitcon*nfitperm ; fj++ ) fc_mx[fj]=-FLT_MAX ;
     }
   }
   if( nfit>0 ){
     int fi ;
     fitws=(RSA_fitws **)malloc(sizeof(RSA_fitws *)*nfit) ;
     for( fi=0 ; fi<nfit ; fi++ ) fitws[fi]=rsa_fitws_new(rdm_over,nsub,nitem,fit[fi].ncomp) ;
     if( nfitcon>0 ){
       nfitfold=nsub*((fit_condfold!=NULL)?fit_condfold->nfold:1) ;
       fitfoldz=(float *)malloc(sizeof(float)*(size_t)nfit*nfitfold) ;
       fitvalid=(unsigned char *)malloc((size_t)nfit*nfitfold) ;
     }
   }

   maxvox_l=THD_roilist_maxvox(rl) ;
   nfeat_max = 0 ;
   for( kk_=0 ; kk_ < nroi ; kk_++ ){
     int nf = (mode == MODE_CONT) ? nvals
            : (mode == MODE_RDM) ? THD_NTRI(nvals)
            : ( (rdm_over == RDM_SUBJ) ? rl->vox[kk_].nar*nvals : rl->vox[kk_].nar ) ;
     if( nf > nfeat_max ) nfeat_max = nf ;
   }
   if( mode == MODE_RDM ) npat_max=maxvox_l*nvals ;
   for( mm_=0 ; mm_ < nmod ; mm_++ ){
     if( mod[mm_].kind != MODK_DSET ) continue ;
     /* use the saved mvals: in continuous mode these datasets have already
        been reduced to ROI means and unloaded, so dset[0] is NULL by now */
     { int nf = (mode == MODE_CONT) ? mod[mm_].mvals
              : (mode == MODE_RDM) ? THD_NTRI(mod[mm_].mvals)
                                   : maxvox_l * mod[mm_].mvals ;
       if( nf > nfeat_max ) nfeat_max = nf ; }
     if( mode == MODE_RDM && maxvox_l*mod[mm_].mvals > npat_max )
       npat_max=maxvox_l*mod[mm_].mvals ;
   }

   F = (float *)malloc(sizeof(float)*(size_t)nitem*nfeat_max) ;
   if( run_resolved ){
     int ru,maxrv=0,nre=joint?nmod*series_runs->nrun:series_runs->nrun ;
     for( ru=0 ; ru<series_runs->nrun ; ru++ )
       if( series_runs->run_nval[ru]>maxrv ) maxrv=series_runs->run_nval[ru] ;
     rneural=(THD_simmat **)calloc(series_runs->nrun,sizeof(THD_simmat *)) ;
     rF=(float *)malloc(sizeof(float)*(size_t)nsub*maxrv) ;
     rtri=(float *)malloc(sizeof(float)*(size_t)series_runs->nrun*ntri) ;
     rstat=(float *)malloc(sizeof(float)*nre) ;
     rprstat=(float *)malloc(sizeof(float)*nre) ;
     rpval=(float *)malloc(sizeof(float)*nre) ;
     rzscr=(float *)malloc(sizeof(float)*nre) ;
     if( joint ) jmz=(float *)malloc(sizeof(float)*nmod) ;
     rnge=(int *)malloc(sizeof(int)*series_runs->nrun) ;
     if( nrunconspec>0 ){
       int nce=joint?nmod*nrunconspec:nrunconspec ;
       rcstat=(float *)malloc(sizeof(float)*nce) ;
       rcpr=(float *)malloc(sizeof(float)*nce) ;
       rcpval=(float *)malloc(sizeof(float)*nce) ;
       rczscr=(float *)malloc(sizeof(float)*nce) ;
       rcsum=(float *)malloc(sizeof(float)*nrunconspec) ;
       rcnge=(int *)malloc(sizeof(int)*nrunconspec) ;
     }
     if( pset!=NULL ){
       mnull=(float *)malloc(sizeof(float)*(size_t)(joint?nmod:1)*npfwe) ;
       if( nrunconspec>0 )
         rcnull=(float *)malloc(sizeof(float)*(size_t)(joint?nmod:1)*nrunconspec*npfwe) ;
     }
     if( do_fwe ){
       rnull=(float *)malloc(sizeof(float)*(size_t)(joint?nmod:1)*series_runs->nrun*npfwe) ;
       run_my_mx=(float *)malloc(sizeof(float)*(size_t)nmod*npfwe) ;
       for( ru=0 ; ru<nmod*npfwe ; ru++ ) run_my_mx[ru]=-FLT_MAX ;
     }
   }
   if( mode == MODE_RDM ) ipat=(float *)malloc(sizeof(float)*(size_t)npat_max) ;
   if( tset != NULL || phset != NULL ){
     tsmain =(float *)malloc(sizeof(float)*(size_t)nsub*nvals) ;
     tsneural=THD_simmat_new(nsub) ;
     if( tset != NULL ){
       tsprep =(float *)malloc(sizeof(float)*(size_t)nsub*nvals) ;
       tsnorm =(float *)malloc(sizeof(float)*nsub) ;
       tslag  =(float *)malloc(sizeof(float)*(size_t)ntri*nvals) ;
     } else {
       phspec=(complex *)malloc(sizeof(complex)*(size_t)nsub*nvals) ;
       phwork=(complex *)malloc(sizeof(complex)*nvals) ;
       phseries=(float *)malloc(sizeof(float)*(size_t)nsub*nvals) ;
       phsc1=(float *)malloc(sizeof(float)*nvals) ;
       phsc2=(float *)malloc(sizeof(float)*nvals) ;
     }
   }
   if( rdm_over == RDM_BRICK ){
     rsub = (float *)malloc(sizeof(float)*nsub) ;
     srdm = (float *)malloc(sizeof(float)*(size_t)nsub*ntri) ;
     if( cmp_metric==CMP_CORR_COV || cmp_metric==CMP_COS_COV ){
       size_t nn=(size_t)nitem*nitem ;
       srdmcov=(float *)malloc(sizeof(float)*(size_t)nsub*nn) ;
       mcovflat=(float *)malloc(sizeof(float)*(size_t)nmod*nn) ;
       mcov=(float **)malloc(sizeof(float *)*nmod) ;
       for( mm_=0 ; mm_<nmod ; mm_++ ){
         mcov[mm_]=mcovflat+(size_t)mm_*nn ;
         THD_simmat_to_tri(mod[mm_].mat,ws->yperm) ;
         THD_rdm_cov_transform(nitem,ws->yperm,cmp_metric==CMP_CORR_COV,mcov[mm_]) ;
       }
     }
     if( ncomq > 0 && nboot > 0 )
       casub=(float *)malloc(sizeof(float)*(size_t)RSA_MAXCOMMON*nsub) ;
     if( joint ){
       mtriflat = (float *)malloc(sizeof(float)*(size_t)nmod*ntri) ;
       mtri = (float **)malloc(sizeof(float *)*nmod) ;
       for( mm_=0 ; mm_ < nmod ; mm_++ ){
         mtri[mm_] = mtriflat + (size_t)mm_*ntri ;
         THD_simmat_to_tri( mod[mm_].mat , mtri[mm_] ) ;
       }
     }
   }

   /* crossnobis: one condition-pattern buffer per run of the busiest subject */
   wh.mode = NN_NONE ;
   wh.residbuf = wh.Rmat = wh.Whalf = wh.wdiag = wh.wtmp = NULL ;
   if( runset != NULL ){
     int s , rr_ ;
     for( s=0 ; s < runset->nsub ; s++ ) if( runset->nrun[s] > maxrun_l ) maxrun_l = runset->nrun[s] ;
     rpat = (float **)malloc(sizeof(float *)*maxrun_l) ;
     for( rr_=0 ; rr_ < maxrun_l ; rr_++ )
       rpat[rr_] = (float *)malloc(sizeof(float)*(size_t)nvals*maxvox_l) ;
     if( runset->has_condmap )
       runraw=(float *)malloc(sizeof(float)*(size_t)runset->maxbrick*maxvox_l) ;

     if( noise_norm != NN_NONE ){
       int maxnt=0 , maxntot=0 ;
       for( s=0 ; s < runset->nsub ; s++ ){
         int rr2 , ntsum=0 ;
         for( rr2=0 ; rr2 < runset->nrun[s] ; rr2++ ){
           int nt = DSET_NVALS(runset->resid[ runset->row_of[s][rr2] ]) ;
           if( nt > maxnt ) maxnt = nt ;
           ntsum += nt ;
         }
         if( ntsum > maxntot ) maxntot = ntsum ;
       }
       wh.mode     = noise_norm ;
       wh.residbuf = (float *)malloc(sizeof(float)*(size_t)maxnt  *maxvox_l) ;
       wh.Rmat     = (float *)malloc(sizeof(float)*(size_t)maxntot*maxvox_l) ;
       wh.wdiag    = (float *)malloc(sizeof(float)*maxvox_l) ;
       wh.wtmp     = (float *)malloc(sizeof(float)*maxvox_l) ;
       if( noise_norm == NN_SHRINK )
         wh.Whalf  = (float *)malloc(sizeof(float)*(size_t)maxvox_l*maxvox_l) ;
     }
   }

   if( do_nc ){
     triA = (float *)malloc(sizeof(float)*ws->m) ;
     triB = (float *)malloc(sizeof(float)*ws->m) ;
     if( cmp_metric==CMP_CORR_COV || cmp_metric==CMP_COS_COV ){
       covA=(float *)malloc(sizeof(float)*(size_t)nitem*nitem) ;
       covB=(float *)malloc(sizeof(float)*(size_t)nitem*nitem) ;
     }
     if( rdm_over == RDM_SUBJ )
       Fh = (float *)malloc(sizeof(float)*(size_t)nitem*nfeat_max) ;
   }

#pragma omp for schedule(dynamic,1)
   for( kk_=0 ; kk_ < nroi ; kk_++ ){

     nvx = rl->vox[kk_].nar ;

     /* the relabeling set (pset) is shared read-only across all ROIs -- no
        per-ROI RNG, which is what makes results thread-count independent and
        lets one common null serve every element */

     /*----------------------------------------------------------------*/
     if( rdm_over == RDM_SUBJ ){
       int nfeat ;

       if( mode == MODE_CONT ){
         nfeat = nvals ;
         if( streaming ){             /* reduce this sphere's mean on the fly */
           if( series_runs!=NULL ){
             int rw_ ;
             for( rw_=0 ; rw_<series_runs->nrow ; rw_++ ){
               int sj_=series_runs->row_sub[rw_],uj_=series_runs->row_run[rw_] ;
               int nv_=series_runs->run_nval[uj_],off_=series_runs->offset[uj_] ;
               THD_roi_mean_ts(dset[rw_],rl->vox+kk_,polort,
                               F+(size_t)sj_*nvals+off_) ;
               rsa_run_normalize(F+(size_t)sj_*nvals+off_,nv_,run_normalize) ;
             }
           } else for( jj_=0 ; jj_ < nsub ; jj_++ )
               THD_roi_mean_ts( dset[jj_] , rl->vox+kk_ , polort ,
                                F + (size_t)jj_*nvals ) ;
         }
         else
           memcpy( F , cmean[kk_] , sizeof(float)*(size_t)nsub*nvals ) ;
         if( !run_resolved )
           neural = THD_simmat_from_features( nitem , nfeat , F , neu_metric ) ;
       } else if( mode == MODE_RDM ){
         nfeat = THD_NTRI(nvals) ;
         neural = rsa_second_order_rdm(rl,kk_,nsub,nvals,nvx,
                                        cond_metric,neu_metric,dset,runset,condition_index,
                                        center_conditions,F,ipat,rpat,runraw,&wh) ;
       } else {
         nfeat = nvx * nvals ;
         for( jj_=0 ; jj_ < nsub ; jj_++ )
           THD_roi_pattern( dset[jj_] , rl->vox+kk_ , F + (size_t)jj_*nfeat ) ;
         neural = THD_simmat_from_features( nitem , nfeat , F , neu_metric ) ;
       }
       if( tset != NULL || phset != NULL )
         memcpy(tsmain,F,sizeof(float)*(size_t)nsub*nvals) ;

       /* reliability: split the feature vector into two halves, build the
          matrix from each, correlate the triangles.  For -mode IS-RSA that
          splits the time course; it tells whether the region's subject geometry
          is stable, so a failing model can be read as wrong rather than noise.
          '-nc_split half' (default) takes the first vs second half -- temporally
          independent, so not inflated by autocorrelation, but the halves differ
          in stimulus content.  '-nc_split interleave' takes even vs odd samples
          -- content matched, but adjacent-sample correlation inflates it. */
       if( do_nc ){
         int nhA = (nc_split==NC_INTERLEAVE) ? (nfeat+1)/2 : nfeat/2 ;
         int nhB = (nc_split==NC_INTERLEAVE) ? nfeat/2     : nfeat-nfeat/2 ;
         if( nhA >= 2 && nhB >= 2 ){
           THD_simmat *sA , *sB ; int t ;
           for( jj_=0 ; jj_ < nsub ; jj_++ ){
             float *src = F+(size_t)jj_*nfeat , *dst = Fh+(size_t)jj_*nhA ;
             for( t=0 ; t < nhA ; t++ )
               dst[t] = src[ (nc_split==NC_INTERLEAVE) ? 2*t : t ] ;
           }
           sA = THD_simmat_from_features( nitem , nhA , Fh , neu_metric ) ;
           THD_simmat_to_tri( sA , triA ) ;
           for( jj_=0 ; jj_ < nsub ; jj_++ ){
             float *src = F+(size_t)jj_*nfeat , *dst = Fh+(size_t)jj_*nhB ;
             for( t=0 ; t < nhB ; t++ )
               dst[t] = src[ (nc_split==NC_INTERLEAVE) ? 2*t+1 : nfeat/2 + t ] ;
           }
           sB = THD_simmat_from_features( nitem , nhB , Fh , neu_metric ) ;
           THD_simmat_to_tri( sB , triB ) ;
           ncA[kk_] = THD_tri_corr( ws->m , triA , triB , cmp_metric ,
                                    ws->sc1 , ws->sc2 ) ;
           THD_simmat_free(sA) ; THD_simmat_free(sB) ;
         } else {
           ncA[kk_] = -2.0f ;              /* too few features to split */
         }
       }

       /* Leave-one-subject-out prediction, once per distinct target/estimand.
          Exact duplicates retain separately labeled outputs below, copied from
          their canonical owner. */
       if( do_loo ){
         for( mm_=0 ; mm_ < nmod ; mm_++ ){
           if( rsa_model_has_loo(mod+mm_) ){
             int own=loo_owner[mm_] , fam=loo_fam[mm_] ;
             if( own == mm_ ){
               /* pnull is scratch here (the primary-stat calls below reuse it,
                  but only after this LOO null has been folded into l_mx) */
               float *pn = do_loofwe ? pnull + (size_t)fam*npfwe : NULL ;
               THD_permstat ps ;
               if( mod[mm_].ncol > 1 ){
                 int lc ; float *bcol[mod[mm_].ncol] ;
                 for( lc=0 ; lc<mod[mm_].ncol ; lc++ )
                   bcol[lc]=tab->val[mod[mm_].icols[lc]] ;
                 ps=THD_isrsa_loo_profile_pred(neural,mod[mm_].ncol,bcol,cmp_metric,
                                                pset,ws,pn,
                                                (nboot>0)?lbpred:NULL) ;
                 if( nboot>0 ) for( lc=0 ; lc<mod[mm_].ncol ; lc++ ) lbtarg[lc]=bcol[lc] ;
               } else if( mod[mm_].rule == RUL_ANNAK ){
                 float *bt=tab->val[mod[mm_].icols[0]] ;
                 if( nboot>0 ) lbtarg[0]=bt ;
                 ps=THD_isrsa_loo_annak_pred(neural,bt,cmp_metric,pset,ws,pn,
                                              (nboot>0)?lbpred:NULL) ;
               } else {
                 float *bt=tab->val[mod[mm_].icols[0]] ;
                 if( nboot>0 ) lbtarg[0]=bt ;
                 ps=THD_isrsa_loo_pred(neural,bt,cmp_metric,pset,ws,pn,
                                        (nboot>0)?lbpred:NULL) ;
               }
               if( nboot>0 )
                 rsa_boot_loo_predictions(nsub,mod[mm_].ncol,lbpred,lbtarg,
                                           cmp_metric,rset,ws,
                                           1.0f-boot_ci/100.0f,bdraw,bseen,
                                           lbx,lby,lblo[mm_]+kk_,lbhi[mm_]+kk_) ;
               if( do_loofwe )
                 THD_max_accum( npfwe , l_mx + (size_t)fam*npfwe , pn ) ;
               lr[mm_][kk_] = ps.stat ;
               lp[mm_][kk_] = ps.pval ;
               lz[mm_][kk_] = ps.zscr ;
             } else {
               lr[mm_][kk_] = lr[own][kk_] ;
               lp[mm_][kk_] = lp[own][kk_] ;
               lz[mm_][kk_] = lz[own][kk_] ;
               if( nboot>0 ){
                 lblo[mm_][kk_]=lblo[own][kk_] ;
                 lbhi[mm_][kk_]=lbhi[own][kk_] ;
               }
             }
           } else {
             lr[mm_][kk_] = lz[mm_][kk_] = 0.0f ; lp[mm_][kk_] = -1.0f ;
           }
         }
       }

       if( save_rdm != NULL ){
         char fn[THD_MAX_NAME] ;
         sprintf(fn,"%s_roi%04d.1D",save_rdm,rl->val[kk_]) ;
         THD_simmat_write_1D( fn , neural ) ;
       }

       /* Assemble this ROI's models.  Fixed ones are shared across ROIs; a
          -model_dset model is rebuilt HERE, from the second modality's data
          in this same ROI -- that per-ROI rebuild is the whole point of it.
          F is free to reuse now, since 'neural' owns its own copy. */
       for( mm_=0 ; mm_ < nmod ; mm_++ ){
         if( mod[mm_].kind != MODK_DSET ){
           mv[mm_] = mod[mm_].mat ;
         } else if( mode == MODE_CONT ){
           int mv2 = mod[mm_].mvals ;
           if( streaming )              /* reduce this sphere's model mean on the fly */
             for( jj_=0 ; jj_ < nsub ; jj_++ )
               THD_roi_mean_ts( mod[mm_].dset[jj_] , rl->vox+kk_ , polort ,
                                F + (size_t)jj_*mv2 ) ;
           else
             memcpy( F , mod[mm_].cmean[kk_] , sizeof(float)*(size_t)nsub*mv2 ) ;
           mv[mm_] = THD_simmat_from_features( nitem , mv2 , F , neu_metric ) ;
         } else if( mode == MODE_RDM ){
           mv[mm_] = rsa_second_order_rdm(rl,kk_,nsub,mod[mm_].mvals,nvx,
                                           cond_metric,neu_metric,mod[mm_].dset,NULL,NULL,
                                           center_conditions,F,ipat,NULL,NULL,NULL) ;
         } else {
           int mv2 = mod[mm_].mvals , nf2 = nvx*mv2 ;
           for( jj_=0 ; jj_ < nsub ; jj_++ )
             THD_roi_pattern( mod[mm_].dset[jj_] , rl->vox+kk_ ,
                              F + (size_t)jj_*nf2 ) ;
           mv[mm_] = THD_simmat_from_features( nitem , nf2 , F , neu_metric ) ;
         }
       }

       if( run_resolved ){
         int ru,pk ;
         for( ru=0 ; ru<series_runs->nrun ; ru++ ){
           int nv=series_runs->run_nval[ru],off=series_runs->offset[ru] ;
           for( jj_=0 ; jj_<nsub ; jj_++ )
             memcpy(rF+(size_t)jj_*nv,F+(size_t)jj_*nvals+off,sizeof(float)*nv) ;
           rneural[ru]=THD_simmat_from_features(nitem,nv,rF,neu_metric) ;
         }
         if( joint ){
           if( rsa_runresolved_regress(rneural,series_runs->nrun,mod,nmod,cmp_metric,
                   pset,ws,nrunconspec,rcon_weight,
                   rstat,rprstat,rpval,rzscr,beta,prtl,pv,jmz,
                   rcstat,rcpr,rcpval,rczscr,rnull,mnull,rcnull) )
             ERROR_exit("3dRSA: singular run-resolved joint design at ROI %d",rl->val[kk_]) ;
           for( mm_=0 ; mm_<nmod ; mm_++ ){
             rr[mm_][kk_]=beta[mm_]; ee[mm_][kk_]=prtl[mm_] ;
             pp[mm_][kk_]=pv[mm_]; zz[mm_][kk_]=jmz[mm_] ;
             for( ru=0 ; ru<series_runs->nrun ; ru++ ){
               int sx=mm_*series_runs->nrun+ru,ix=sx ;
               run_rr[ix][kk_]=rstat[sx]; run_ee[ix][kk_]=rprstat[sx] ;
               run_pp[ix][kk_]=rpval[sx]; run_zz[ix][kk_]=rzscr[sx] ;
             }
             for( ru=0 ; ru<nrunconspec ; ru++ ){
               int sx=mm_*nrunconspec+ru,ix=sx ;
               rcon_rr[ix][kk_]=rcstat[sx]; rcon_ee[ix][kk_]=rcpr[sx] ;
               rcon_pp[ix][kk_]=rcpval[sx]; rcon_zz[ix][kk_]=rczscr[sx] ;
             }
             if( do_fwe ){
               THD_max_accum(npfwe,my_mx+(size_t)mm_*npfwe,
                             mnull+(size_t)mm_*npfwe) ;
               for( pk=0 ; pk<npfwe ; pk++ ){
                 float mx=0.0f ;
                 if( run_analysis==RUN_ANALYSIS_SEPARATE )
                   for( ru=0 ; ru<series_runs->nrun ; ru++ ){
                     float v=rnull[((size_t)mm_*series_runs->nrun+ru)*npfwe+pk] ;
                     if(v>mx)mx=v ;
                   }
                 for( ru=0 ; ru<nrunconspec ; ru++ ){
                   float v=rcnull[((size_t)mm_*nrunconspec+ru)*npfwe+pk] ;
                   if(v>mx)mx=v ;
                 }
                 if(mx>run_my_mx[(size_t)mm_*npfwe+pk])
                   run_my_mx[(size_t)mm_*npfwe+pk]=mx ;
               }
             }
           }
         } else for( mm_=0 ; mm_<nmod ; mm_++ ){
           float ms,mp,mz ;
           rsa_runresolved_mantel(rneural,series_runs->nrun,mv[mm_],mod[mm_].run_mat,cmp_metric,
                                  pset,ws,rtri,rstat,rpval,rzscr,&ms,&mp,&mz,
                                  do_fwe?rnull:NULL,do_fwe?mnull:NULL,rnge,
                                  nrunconspec,rcon_weight,rcstat,rcpval,rczscr,
                                  do_fwe?rcnull:NULL,rcnge,rcsum) ;
           rr[mm_][kk_]=ee[mm_][kk_]=ms ; pp[mm_][kk_]=mp ; zz[mm_][kk_]=mz ;
           for( ru=0 ; ru<series_runs->nrun ; ru++ ){
             int ix=mm_*series_runs->nrun+ru ;
             run_rr[ix][kk_]=run_ee[ix][kk_]=rstat[ru] ; run_pp[ix][kk_]=rpval[ru] ;
             run_zz[ix][kk_]=rzscr[ru] ;
           }
           for( ru=0 ; ru<nrunconspec ; ru++ ){
             int ix=mm_*nrunconspec+ru ;
             rcon_rr[ix][kk_]=rcon_ee[ix][kk_]=rcstat[ru]; rcon_pp[ix][kk_]=rcpval[ru] ;
             rcon_zz[ix][kk_]=rczscr[ru] ;
           }
           if( do_fwe ){
             THD_max_accum(npfwe,my_mx+(size_t)mm_*npfwe,mnull) ;
             for( pk=0 ; pk<npfwe ; pk++ ){
               float mx=0.0f ;
               if( run_analysis==RUN_ANALYSIS_SEPARATE )
                 for( ru=0 ; ru<series_runs->nrun ; ru++ )
                   if( rnull[(size_t)ru*npfwe+pk]>mx ) mx=rnull[(size_t)ru*npfwe+pk] ;
               for( ru=0 ; ru<nrunconspec ; ru++ )
                 if( rcnull[(size_t)ru*npfwe+pk]>mx ) mx=rcnull[(size_t)ru*npfwe+pk] ;
               if( mx>run_my_mx[(size_t)mm_*npfwe+pk] )
                 run_my_mx[(size_t)mm_*npfwe+pk]=mx ;
             }
           }
         }
         for( ru=0 ; ru<series_runs->nrun ; ru++ ){
           THD_simmat_free(rneural[ru]); rneural[ru]=NULL ;
         }
       } else if( tset != NULL || phset != NULL ){ /* one temporal family for all effects */
         rsa_temporal_infer(neural,nmod,mv,nort,ort,joint,ncon,con,
                            tsmain,neu_metric,cmp_metric,tset,phset,ws,
                            tsneed,tslag,tsprep,tsnorm,
                            phspec,phwork,phseries,phsc1,phsc2,tsneural,
                            beta,prtl,pv,tsz,pnull,tscd,tscp,tscz,cpnull) ;
         if( do_fwe ) THD_max_accum(nmod*npfwe,my_mx,pnull) ;
         for( mm_=0 ; mm_ < nmod ; mm_++ ){
           rr[mm_][kk_]=beta[mm_] ; ee[mm_][kk_]=prtl[mm_] ;
           pp[mm_][kk_]=pv[mm_] ; zz[mm_][kk_]=tsz[mm_] ;
         }
         if( nboot > 0 ){
           if( joint || nort > 0 )
             rsa_boot_isrsa_regress(neural,nmod,mv,nort,ort,cmp_metric,joint,
                                     rset,ws,1.0f-boot_ci/100.0f,
                                     br_y,brx,brcoef,brbeta,bseen,blo,bhi,kk_) ;
           else for( mm_=0 ; mm_ < nmod ; mm_++ )
             rsa_boot_isrsa(neural,mv[mm_],cmp_metric,rset,ws,
                            1.0f-boot_ci/100.0f,bdraw,bseen,
                            blo[mm_]+kk_,bhi[mm_]+kk_) ;
         }
         for( mm_=0 ; mm_ < ncon ; mm_++ ){
           if( contrast_rset!=NULL ){
             float *pn=do_confwe ? cpnull+(size_t)mm_*npfwe : NULL ;
             THD_permstat hs=rsa_isrsa_superiority_test(
                 neural,mv[con[mm_].ia],mv[con[mm_].ib],cmp_metric,
                 contrast_rset,ws,hdraw,bseen,pn) ;
             cd[mm_][kk_]=ce[mm_][kk_]=hs.stat ;
             cp[mm_][kk_]=hs.pval ; cz[mm_][kk_]=hs.zscr ;
           } else {
             cd[mm_][kk_]=ce[mm_][kk_]=tscd[mm_] ;
             cp[mm_][kk_]=tscp[mm_] ; cz[mm_][kk_]=tscz[mm_] ;
           }
           if( nboot > 0 )
             rsa_boot_isrsa_contrast(neural,mv[con[mm_].ia],mv[con[mm_].ib],
                                      cmp_metric,rset,ws,
                                      1.0f-boot_ci/100.0f,bdraw,bseen,
                                      dblo[mm_]+kk_,dbhi[mm_]+kk_) ;
         }
         if( do_confwe ) THD_max_accum(ncon*npfwe,c_mx,cpnull) ;
       } else if( joint ){              /* all models in one regression */
         THD_rdm_regress( neural , nmod , mv , nort , ort , cmp_metric , pset ,
                          ws , beta , prtl , (nperm > 0) ? pv : NULL ,
                          do_fwe ? pnull : NULL ) ;
         if( do_fwe ) THD_max_accum( nmod*npfwe , my_mx , pnull ) ;
         for( mm_=0 ; mm_ < nmod ; mm_++ ){
           rr[mm_][kk_] = beta[mm_] ;
           ee[mm_][kk_] = prtl[mm_] ;
           pp[mm_][kk_] = (nperm > 0) ? pv[mm_] : -1.0f ;
           zz[mm_][kk_] = (nperm > 0) ? THD_p_to_z(pv[mm_],beta[mm_])
                                      : MYatanh(beta[mm_]) ;
         }
         if( nboot > 0 )
           rsa_boot_isrsa_regress( neural,nmod,mv,nort,ort,cmp_metric,1,
                                    rset,ws,1.0f-boot_ci/100.0f,
                                    br_y,brx,brcoef,brbeta,bseen,
                                    blo,bhi,kk_ ) ;
       } else if( nort > 0 ){           /* each model separately, nuisances out */
         for( mm_=0 ; mm_ < nmod ; mm_++ ){
           float *pn = do_fwe ? pnull + (size_t)mm_*npfwe : NULL ;
           THD_rdm_regress( neural , 1 , mv+mm_ , nort , ort , cmp_metric , pset ,
                            ws , beta , prtl , (nperm > 0) ? pv : NULL , pn ) ;
           if( do_fwe ) THD_max_accum( npfwe , my_mx + (size_t)mm_*npfwe , pn ) ;
           rr[mm_][kk_] = beta[0] ;
           ee[mm_][kk_] = prtl[0] ;
           pp[mm_][kk_] = (nperm > 0) ? pv[0] : -1.0f ;
           zz[mm_][kk_] = (nperm > 0) ? THD_p_to_z(pv[0],beta[0])
                                      : MYatanh(beta[0]) ;
         }
         if( nboot > 0 )
           rsa_boot_isrsa_regress( neural,nmod,mv,nort,ort,cmp_metric,0,
                                    rset,ws,1.0f-boot_ci/100.0f,
                                    br_y,brx,brcoef,brbeta,bseen,
                                    blo,bhi,kk_ ) ;
       } else {                         /* subject-label Mantel null */
         for( mm_=0 ; mm_ < nmod ; mm_++ ){
           float *pn = do_fwe ? pnull + (size_t)mm_*npfwe : NULL ;
           THD_permstat ps = (mcache != NULL && mcache_ix[mm_] >= 0)
             ? THD_mantel_corr_cached( neural , mv[mm_] , mcache,
                                       mcache_ix[mm_] , ws , pn )
             : THD_mantel_corr( neural , mv[mm_] , cmp_metric , pset , ws , pn ) ;
           if( do_fwe ) THD_max_accum( npfwe , my_mx + (size_t)mm_*npfwe , pn ) ;
           rr[mm_][kk_] = ps.stat ;
           ee[mm_][kk_] = ps.stat ;
           pp[mm_][kk_] = ps.pval ;
           zz[mm_][kk_] = ps.zscr ;
           if( nboot > 0 )
             rsa_boot_isrsa( neural,mv[mm_],cmp_metric,rset,ws,
                              1.0f-boot_ci/100.0f,bdraw,bseen,
                              blo[mm_]+kk_,bhi[mm_]+kk_ ) ;
         }
       }

       /* IS-RSA model contrasts: paired Mantel difference under the SAME
          relabelings.  mv[ia]/mv[ib] may be fixed or may both have just been
          rebuilt for this ROI/searchlight from -model_dset modalities. */
       if( ncon > 0 && tset == NULL && phset == NULL ){
         int cc ;
         for( cc=0 ; cc < ncon ; cc++ ){
           float *pn = do_confwe ? cpnull + (size_t)cc*npfwe : NULL ;
           THD_permstat ps = (contrast_rset!=NULL)
             ? rsa_isrsa_superiority_test(neural,mv[con[cc].ia],mv[con[cc].ib],
                                           cmp_metric,contrast_rset,ws,hdraw,bseen,pn)
             : (mcache != NULL && mcache_ix[con[cc].ia] >= 0 &&
                                  mcache_ix[con[cc].ib] >= 0)
                 ? THD_mantel_contrast_cached( neural , mv[con[cc].ia] ,
                                               mv[con[cc].ib] , mcache,
                                               mcache_ix[con[cc].ia],
                                               mcache_ix[con[cc].ib],ws,pn )
                 : THD_mantel_contrast( neural , mv[con[cc].ia] ,
                                        mv[con[cc].ib] , cmp_metric , pset , ws , pn ) ;
           if( do_confwe ) THD_max_accum( npfwe , c_mx + (size_t)cc*npfwe , pn ) ;
           cd[cc][kk_] = ps.stat ;   /* IS-RSA: the difference IS the statistic */
           ce[cc][kk_] = ps.stat ;
           cp[cc][kk_] = ps.pval ;
           cz[cc][kk_] = ps.zscr ;
           if( nboot > 0 )
             rsa_boot_isrsa_contrast( neural,mv[con[cc].ia],mv[con[cc].ib],
                                       cmp_metric,rset,ws,
                                       1.0f-boot_ci/100.0f,bdraw,bseen,
                                       dblo[cc]+kk_,dbhi[cc]+kk_ ) ;
         }
       }

       /* IS-RSA pairwise or three-predictor variance partition. Unique/partial
          effects use model-specific reduced-residual nulls; shared regions use
          complete neural-item relabeling. */
       if( ncomq > 0 ){
         int rq ;
         for( rq=0 ; rq < nreqcom ; rq++ ){
           int nq=com[rq].nq,qb=com[rq].qbase ;
           float outv[RSA_MAXCOMMON],pvv[RSA_MAXCOMMON],zvv[RSA_MAXCOMMON] ;
           float *pn = do_cafwe ? capnull : NULL ;
           if( com[rq].nmodel==2 )
             THD_commonality(neural,mv[com[rq].imod[0]],mv[com[rq].imod[1]],
                              cmp_metric,pset,ws,outv,pvv,zvv,pn) ;
           else
             THD_commonality3(neural,mv[com[rq].imod[0]],mv[com[rq].imod[1]],
                               mv[com[rq].imod[2]],cmp_metric,pset,ws,
                               outv,pvv,zvv,pn) ;
           if( do_cafwe )
             THD_max_accum(nq*npfwe,ca_mx+(size_t)qb*npfwe,pn) ;
           { int comp ; for( comp=0 ; comp<nq ; comp++ ){
               int q=qb+comp ;
               cav[q][kk_] = outv[comp] ;
               cap[q][kk_] = pvv[comp] ;
               caz[q][kk_] = zvv[comp] ;
           } }
           if( nboot>0 ){
             if( com[rq].nmodel==2 )
               rsa_boot_isrsa_commonality(neural,mv[com[rq].imod[0]],
                                            mv[com[rq].imod[1]],cmp_metric,rset,ws,
                                            1.0f-boot_ci/100.0f,bca_y,bca_a,bca_b,
                                            bcadraw,bseen,calo,cahi,qb,kk_) ;
             else
               rsa_boot_isrsa_commonality3(neural,mv[com[rq].imod[0]],
                                             mv[com[rq].imod[1]],mv[com[rq].imod[2]],
                                             cmp_metric,rset,ws,1.0f-boot_ci/100.0f,
                                             bca_y,bca_a,bca_b,bca_c,bcadraw,bseen,
                                             calo,cahi,qb,kk_) ;
           }
         }
       }

       /* F7 fitted components: every subject-label draw repeats the complete
          outer held-subject fit.  Slot 0 is identity and supplies both the
          observed CV accuracy and the fold-mean normalized weights. */
       if( nfit>0 ){
         int fi,pk,cc ;
         for( fi=0 ; fi<nfit ; fi++ ){
           float obs ;
           if( do_fitfwe ){
             int exceed=0 ; float *fn=fnull+(size_t)fi*nfitperm ;
             for( pk=0 ; pk<nfitperm ; pk++ ){
               float *mw=(pk==0)?fitws[fi]->foldw:NULL ;
               fn[pk]=rsa_fit_cv(rdm_over,nsub,nitem,NULL,neural,fit+fi,mv,
                                  pset->perm+(size_t)pk*nsub,fit_condfold,
                                  fitws[fi],mw,
                                  (pk==0 && fitfoldz)?fitfoldz+(size_t)fi*nfitfold:NULL,
                                  (pk==0 && fitvalid)?fitvalid+(size_t)fi*nfitfold:NULL) ;
             }
             obs=fn[0] ;
             for( pk=0 ; pk<nfitperm ; pk++ ) if( fabsf(fn[pk])>=fabsf(obs) ) exceed++ ;
             fpv[fi][kk_]=(float)exceed/nfitperm ;
             fzv[fi][kk_]=THD_perm_signed_z(fpv[fi][kk_],obs,PERM_TAIL_TWO) ;
           } else {
             obs=rsa_fit_cv(rdm_over,nsub,nitem,NULL,neural,fit+fi,mv,NULL,
                            fit_condfold,fitws[fi],fitws[fi]->foldw,
                            fitfoldz?fitfoldz+(size_t)fi*nfitfold:NULL,
                            fitvalid?fitvalid+(size_t)fi*nfitfold:NULL) ;
             fpv[fi][kk_]=-1.0f ; fzv[fi][kk_]=MYatanh(obs) ;
           }
           fr[fi][kk_]=obs ;
           for( cc=0 ; cc<fit[fi].ncomp ; cc++ )
             fwgt[fit[fi].wbase+cc][kk_]=fitws[fi]->foldw[cc] ;
         }
         /* F14: pair the two complete held-subject fits draw by draw.  Since
            each F7 score is tanh(mean fold z), subtracting their atanh values
            is exactly the mean paired held-fold Fisher-z accuracy difference. */
         for( cc=0 ; cc<nfitcon ; cc++ ){
           int ia=fcon[cc].ia,ib=fcon[cc].ib ;
           if( fit_contrast_rset!=NULL ){
             float *pn=do_fitconfwe?fcnull+(size_t)cc*nfitperm:NULL ;
             THD_permstat hs=rsa_fit_superiority_test(
               nsub,(fit_condfold!=NULL)?fit_condfold->nfold:1,
               fitfoldz+(size_t)ia*nfitfold,fitvalid+(size_t)ia*nfitfold,
               fitfoldz+(size_t)ib*nfitfold,fitvalid+(size_t)ib*nfitfold,
               fit_contrast_rset,hdraw,pn) ;
             fcd[cc][kk_]=hs.stat ; fcp[cc][kk_]=hs.pval ; fcz[cc][kk_]=hs.zscr ;
             if( do_fitconfwe ) THD_max_accum(nfitperm,fc_mx+(size_t)cc*nfitperm,pn) ;
           } else if( do_fitconfwe ){
             int exceed=0 ; float *dn=fcnull+(size_t)cc*nfitperm ;
             for( pk=0 ; pk<nfitperm ; pk++ )
               dn[pk]=MYatanh(fnull[(size_t)ia*nfitperm+pk])
                     -MYatanh(fnull[(size_t)ib*nfitperm+pk]) ;
             fcd[cc][kk_]=dn[0] ;
             for( pk=0 ; pk<nfitperm ; pk++ ) if( fabsf(dn[pk])>=fabsf(dn[0]) ) exceed++ ;
             fcp[cc][kk_]=(float)exceed/nfitperm ;
             fcz[cc][kk_]=THD_perm_signed_z(fcp[cc][kk_],dn[0],PERM_TAIL_TWO) ;
             for( pk=0 ; pk<nfitperm ; pk++ ) dn[pk]=fabsf(dn[pk]) ;
             THD_max_accum(nfitperm,fc_mx+(size_t)cc*nfitperm,dn) ;
           } else {
             fcd[cc][kk_]=MYatanh(fr[ia][kk_])-MYatanh(fr[ib][kk_]) ;
             fcp[cc][kk_]=-1.0f ; fcz[cc][kk_]=fcd[cc][kk_] ;
           }
         }
         /* Preserve the signed fitted null until every paired F14 contrast has
            consumed it; F7's individual max family is on absolute accuracy. */
         if( do_fitfwe ) for( fi=0 ; fi<nfit ; fi++ ){
           float *fn=fnull+(size_t)fi*nfitperm ;
           for( pk=0 ; pk<nfitperm ; pk++ ) fn[pk]=fabsf(fn[pk]) ;
           THD_max_accum(nfitperm,f_mx+(size_t)fi*nfitperm,fn) ;
         }
       }

       if( neural!=NULL ) THD_simmat_free(neural) ;
       for( mm_=0 ; mm_ < nmod ; mm_++ )
         if( mod[mm_].kind == MODK_DSET ) THD_simmat_free(mv[mm_]) ;

     /*----------------------------------------------------------------*/
     } else {   /* classic RSA: one matrix per subject, rows are conditions */

       for( mm_=0 ; mm_ < nmod ; mm_++ ) mv[mm_] = mod[mm_].mat ;

       /* Every classic statistic at this location consumes the same subject
          RDMs.  Build each once, retain only its compact upper triangle, and
          reuse it across primary models, contrasts, ceilings, and condition
          bootstraps.  This matters most when construction includes runwise
          crossnobis and residual-covariance whitening. */
       for( jj_=0 ; jj_ < nsub ; jj_++ ){
         neural = rsa_subject_rdm( jj_,rl,kk_,nitem,nvx,neu_metric,
                                   dset,runset,condition_index,center_conditions,F,rpat,runraw,&wh ) ;
         THD_simmat_to_tri( neural , srdm + (size_t)jj_*ntri ) ;
         if( srdmcov!=NULL )
           THD_rdm_cov_transform(nitem,srdm+(size_t)jj_*ntri,
                                 cmp_metric==CMP_CORR_COV,
                                 srdmcov+(size_t)jj_*nitem*nitem) ;
         THD_simmat_free(neural) ;
       }

       /* Nili noise ceiling: correlate each subject's condition RDM with the
          group-mean RDM (upper bound) and with the leave-one-subject-out mean
          of the others (lower bound), then average over subjects.  With a
          runset, srdm already contains unbiased crossnobis estimates whose two
          contrast factors came from independent runs (after optional residual
          whitening).  Thus no run data are fitted here: the lower template also
          excludes the evaluated subject, while the conventional upper template
          intentionally includes it as an optimistic attainable bound. */
       if( do_nc ){
         int a ; float hi=0.0f , lo=0.0f , mfac=1.0f/(float)nsub ;
         if( srdmcov!=NULL ){
           size_t q,nn=(size_t)nitem*nitem ;
           for( q=0 ; q<nn ; q++ ){
             double s=0.0 ;
             for( jj_=0 ; jj_<nsub ; jj_++ ) s+=srdmcov[(size_t)jj_*nn+q] ;
             covA[q]=(float)(s*mfac) ;
           }
           for( jj_=0 ; jj_<nsub ; jj_++ ){
             float *cs=srdmcov+(size_t)jj_*nn ;
             for( q=0 ; q<nn ; q++ )
               covB[q]=(covA[q]*(float)nsub-cs[q])/(float)(nsub-1) ;
             hi+=THD_rdm_cov_cosine(nitem,cs,covA) ;
             lo+=THD_rdm_cov_cosine(nitem,cs,covB) ;
           }
           ncA[kk_]=lo*mfac ; ncB[kk_]=hi*mfac ;
         } else {
         for( a=0 ; a < ws->m ; a++ ){            /* mean triangle into triA */
           float s=0.0f ;
           for( jj_=0 ; jj_ < nsub ; jj_++ ) s += srdm[(size_t)jj_*ws->m + a] ;
           triA[a] = s*mfac ;
         }
         for( jj_=0 ; jj_ < nsub ; jj_++ ){
           float *ts = srdm + (size_t)jj_*ws->m ;
           for( a=0 ; a < ws->m ; a++ )           /* leave-one-out mean */
             triB[a] = ( triA[a]*(float)nsub - ts[a] ) / (float)(nsub-1) ;
           hi += THD_tri_corr( ws->m , ts , triA , cmp_metric , ws->sc1 , ws->sc2 ) ;
           lo += THD_tri_corr( ws->m , ts , triB , cmp_metric , ws->sc1 , ws->sc2 ) ;
         }
         ncA[kk_] = lo*mfac ; ncB[kk_] = hi*mfac ;
         }
       }

       if( joint ){
         float *bsub = (float *)malloc(sizeof(float)*(size_t)nsub*nmod) ;
         if( ncboot > 0 && !dualboot )
           memset(cbsum,0,sizeof(float)*(size_t)nmod*ncboot) ;
         for( jj_=0 ; jj_ < nsub ; jj_++ ){
           float *st = srdm + (size_t)jj_*ntri ;
           THD_tri_regress( ntri , st , nmod , mtri , cmp_metric , ws , beta ) ;
           for( mm_=0 ; mm_ < nmod ; mm_++ ) bsub[ jj_*nmod + mm_ ] = beta[mm_] ;
           if( ncboot > 0 ){
             int bb_ ;
             for( bb_=0 ; bb_ < ncboot ; bb_++ ) if( crset->valid[bb_] ){
               int mt_=rsa_cond_boot_tri(st,nmod,mv,crset,bb_,cb_y,cbx) ;
               THD_tri_regress(mt_,cb_y,nmod,cbx,cmp_metric,cws,cbbeta) ;
               for( mm_=0 ; mm_ < nmod ; mm_++ ){
                 if( dualboot )
                   dualval[((size_t)mm_*nsub+jj_)*ncboot+bb_]=cbbeta[mm_] ;
                 else cbsum[(size_t)mm_*ncboot+bb_] += cbbeta[mm_] ;
               }
             }
           }
         }
         for( mm_=0 ; mm_ < nmod ; mm_++ ){
           THD_permstat ps ; float *pn = do_fwe ? pnull + (size_t)mm_*npfwe : NULL ;
           for( jj_=0 ; jj_ < nsub ; jj_++ ) rsub[jj_] = bsub[ jj_*nmod + mm_ ] ;
           ps = THD_signflip_t( nsub , rsub , pset , pn ) ;
           if( do_fwe ) THD_max_accum( npfwe , my_mx + (size_t)mm_*npfwe , pn ) ;
           { float bbar=0.0f ;
             for( jj_=0 ; jj_ < nsub ; jj_++ ) bbar += rsub[jj_] ;
             rr[mm_][kk_] = bbar/(float)nsub ; }
           if( dualboot )
             rsa_dual_boot_ci(nsub,rsub,rset,crset,
                              dualval+(size_t)mm_*nsub*ncboot,0,
                              1.0f-boot_ci/100.0f,dualwork,
                              blo[mm_]+kk_,bhi[mm_]+kk_) ;
           else if( nboot > 0 )
             rsa_boot_subject_mean( nsub,rsub,rset,0,1.0f-boot_ci/100.0f,
                                    bdraw,blo[mm_]+kk_,bhi[mm_]+kk_ ) ;
           if( ncboot > 0 && !dualboot )
             rsa_cond_boot_ci( cbsum+(size_t)mm_*ncboot,nsub,crset,0,
                               1.0f-boot_ci/100.0f,cbdraw,
                               cblo[mm_]+kk_,cbhi[mm_]+kk_ ) ;
           ee[mm_][kk_] = ps.stat ;
           pp[mm_][kk_] = ps.pval ;
           zz[mm_][kk_] = ps.zscr ;
         }
         free(bsub) ;
       } else {
         for( mm_=0 ; mm_ < nmod ; mm_++ ){
           THD_permstat ps ; float zbar=0.0f ;
           if( ncboot > 0 && !dualboot )
             memset(cbsum+(size_t)mm_*ncboot,0,sizeof(float)*ncboot) ;

           for( jj_=0 ; jj_ < nsub ; jj_++ ){
             float *st = srdm + (size_t)jj_*ntri ;
             if( seed_mask!=NULL ){
               float *ss=seed_srdm+(size_t)jj_*ntri ;
               rsub[jj_]=MYatanh(THD_tri_corr(ntri,st,ss,cmp_metric,
                                               ws->sc1,ws->sc2)) ;
             } else if( srdmcov!=NULL )
               rsub[jj_]=MYatanh(THD_rdm_cov_cosine(
                                  nitem,srdmcov+(size_t)jj_*nitem*nitem,mcov[mm_])) ;
             else {
               THD_simmat_to_tri( mv[mm_] , ws->yperm ) ;
               rsub[jj_] = MYatanh( THD_tri_corr( ntri , st , ws->yperm ,
                                                  cmp_metric , ws->sc1 , ws->sc2 ) ) ;
             }
             if( ncboot > 0 ){
               int bb_ ;
               for( bb_=0 ; bb_ < ncboot ; bb_++ ) if( crset->valid[bb_] ){
                 int mt_=rsa_cond_boot_tri(st,1,mv+mm_,crset,bb_,cb_y,cbx) ;
                 float rv_=THD_tri_corr(mt_,cb_y,cbx[0],cmp_metric,cws->sc1,cws->sc2) ;
                 if( dualboot ) dualval[(size_t)jj_*ncboot+bb_]=MYatanh(rv_) ;
                 else cbsum[(size_t)mm_*ncboot+bb_] += MYatanh(rv_) ;
               }
             }
           }
           for( jj_=0 ; jj_ < nsub ; jj_++ ) zbar += rsub[jj_] ;
           zbar /= (float)nsub ;

           { float *pn = do_fwe ? pnull + (size_t)mm_*npfwe : NULL ;
             if( classic_null==CLASSIC_NULL_CONDITIONS )
               ps=(seed_mask!=NULL)
                    ? rsa_classic_seed_condition_test(nsub,nitem,srdm,seed_srdm,
                                                       cmp_metric,cpset,ws,pn)
                    : rsa_classic_condition_test(nsub,nitem,srdm,srdmcov,
                                                  mv[mm_],NULL,cmp_metric,cpset,
                                                  ws,covA,covB,pn) ;
             else ps=THD_signflip_t(nsub,rsub,pset,pn) ;
             if( do_fwe ) THD_max_accum( npfwe , my_mx + (size_t)mm_*npfwe , pn ) ; }
           rr[mm_][kk_] = tanhf(zbar) ;
           if( dualboot )
             rsa_dual_boot_ci(nsub,rsub,rset,crset,dualval,1,
                              1.0f-boot_ci/100.0f,dualwork,
                              blo[mm_]+kk_,bhi[mm_]+kk_) ;
           else if( nboot > 0 )
             rsa_boot_subject_mean( nsub,rsub,rset,1,1.0f-boot_ci/100.0f,
                                    bdraw,blo[mm_]+kk_,bhi[mm_]+kk_ ) ;
           if( ncboot > 0 && !dualboot )
             rsa_cond_boot_ci( cbsum+(size_t)mm_*ncboot,nsub,crset,1,
                               1.0f-boot_ci/100.0f,cbdraw,
                               cblo[mm_]+kk_,cbhi[mm_]+kk_ ) ;
           ee[mm_][kk_] = ps.stat ;
           pp[mm_][kk_] = ps.pval ;
           zz[mm_][kk_] = ps.zscr ;
         }
       }

       /* classic-RSA model contrasts: the paired within-subject Fisher-z
          difference.  The default population test sign-flips subjects (or uses
          signed rank); S1 fixed effects jointly relabels both model condition
          axes and tests the mean z difference for the observed subject sample. */
       if( ncon > 0 ){
         int cc ;
         for( cc=0 ; cc < ncon ; cc++ ){
           int ia = con[cc].ia , ib = con[cc].ib ; float dbar=0.0f,rdbar=0.0f ;
           THD_permstat ps ; float *pn = do_confwe ? cpnull + (size_t)cc*npfwe : NULL ;
           for( jj_=0 ; jj_ < nsub ; jj_++ ){
             float *st = srdm + (size_t)jj_*ntri ;
             float rA , rB , zA , zB ;
             if( srdmcov!=NULL ){
               float *cs=srdmcov+(size_t)jj_*nitem*nitem ;
               rA=THD_rdm_cov_cosine(nitem,cs,mcov[ia]) ;
               rB=THD_rdm_cov_cosine(nitem,cs,mcov[ib]) ;
             } else {
               THD_simmat_to_tri( mod[ia].mat , ws->yperm ) ;
               rA = THD_tri_corr( ntri , st , ws->yperm ,
                                  cmp_metric , ws->sc1 , ws->sc2 ) ;
               THD_simmat_to_tri( mod[ib].mat , ws->yperm ) ;
               rB = THD_tri_corr( ntri , st , ws->yperm ,
                                  cmp_metric , ws->sc1 , ws->sc2 ) ;
             }
             zA=MYatanh(rA) ; zB=MYatanh(rB) ;
             dsub[jj_] = zA - zB ;
             rdbar += rA-rB ;
             if( dualboot ){
               THD_simmat *cmv[2] ; int bb_ ; cmv[0]=mv[ia] ; cmv[1]=mv[ib] ;
               for( bb_=0 ; bb_<ncboot ; bb_++ ) if( crset->valid[bb_] ){
                 int mt_=rsa_cond_boot_tri(st,2,cmv,crset,bb_,cb_y,cbx) ;
                 float ca_=THD_tri_corr(mt_,cb_y,cbx[0],cmp_metric,cws->sc1,cws->sc2) ;
                 float cb_=THD_tri_corr(mt_,cb_y,cbx[1],cmp_metric,cws->sc1,cws->sc2) ;
                 dualval[(size_t)jj_*ncboot+bb_]=MYatanh(ca_)-MYatanh(cb_) ;
               }
             }
           }
           for( jj_=0 ; jj_ < nsub ; jj_++ ) dbar += dsub[jj_] ;
           dbar /= (float)nsub ;
           rdbar /= (float)nsub ;
           if( classic_null==CLASSIC_NULL_CONDITIONS )
             ps=rsa_classic_condition_test(nsub,nitem,srdm,srdmcov,
                                            mv[ia],mv[ib],cmp_metric,cpset,ws,
                                            covA,covB,pn) ;
           else
             ps = group_test ? THD_signrank_signflip( nsub , dsub , pset , pn )
                             : THD_signflip_t      ( nsub , dsub , pset , pn ) ;
           if( do_confwe ) THD_max_accum( npfwe , c_mx + (size_t)cc*npfwe , pn ) ;
           crd[cc][kk_] = rdbar ;        /* descriptive mean raw-correlation difference */
           cd[cc][kk_] = dbar ;          /* inferential mean Fisher-z difference */
           ce[cc][kk_] = ps.stat ;       /* the t or W actually tested (for FWE)  */
           cp[cc][kk_] = ps.pval ;
           cz[cc][kk_] = ps.zscr ;
           if( dualboot )
             rsa_dual_boot_ci(nsub,dsub,rset,crset,dualval,0,
                              1.0f-boot_ci/100.0f,dualwork,
                              dblo[cc]+kk_,dbhi[cc]+kk_) ;
           else if( nboot > 0 )
             rsa_boot_subject_mean( nsub,dsub,rset,0,1.0f-boot_ci/100.0f,
                                    bdraw,dblo[cc]+kk_,dbhi[cc]+kk_ ) ;
         }
       }

       /* F15 classic commonality.  The group statistic is the mean subject
          component.  Unique/partial effects use per-subject reduced-model
          residuals under one synchronized CONDITION relabeling; common uses
          the complete condition null.  Subject sign flips are deliberately not
          used because the squared unique effects are non-negative. */
       if( ncomq > 0 ){
         int rq ;
         for( rq=0 ; rq < nreqcom ; rq++ ){
           int nq=com[rq].nq,qb=com[rq].qbase,comp ;
           float outv[RSA_MAXCOMMON],pvv[RSA_MAXCOMMON],zvv[RSA_MAXCOMMON] ;
           float *pn=do_cafwe ? capnull : NULL ;
           if( com[rq].nmodel==2 )
             THD_classic_commonality(nsub,nitem,srdm,mv[com[rq].imod[0]],
                                      mv[com[rq].imod[1]],cmp_metric,cpset,ws,
                                      outv,pvv,zvv,pn,casub) ;
           else
             THD_classic_commonality3(nsub,nitem,srdm,mv[com[rq].imod[0]],
                                       mv[com[rq].imod[1]],mv[com[rq].imod[2]],
                                       cmp_metric,cpset,ws,outv,pvv,zvv,pn,casub) ;
           if( do_cafwe )
             THD_max_accum(nq*ncaperm,ca_mx+(size_t)qb*ncaperm,pn) ;
           for( comp=0 ; comp<nq ; comp++ ){
             int q=qb+comp ;
             cav[q][kk_]=outv[comp] ; cap[q][kk_]=pvv[comp] ;
             caz[q][kk_]=zvv[comp] ;
             if( nboot > 0 )
               rsa_boot_subject_mean(nsub,casub+(size_t)comp*nsub,rset,0,
                                      1.0f-boot_ci/100.0f,bdraw,
                                      calo[q]+kk_,cahi[q]+kk_) ;
           }
         }
       }
       /* F7 classic RSA: condition labels are jointly relabeled across all
          components, and the model is refit on N-1 subjects before scoring
          the held subject.  This is distinct from the primary sign-flip null. */
       if( nfit>0 ){
         int fi,pk,cc ;
         for( fi=0 ; fi<nfit ; fi++ ){
           float obs ;
           if( do_fitfwe ){
             int exceed=0 ; float *fn=fnull+(size_t)fi*nfitperm ;
             for( pk=0 ; pk<nfitperm ; pk++ ){
               float *mw=(pk==0)?fitws[fi]->foldw:NULL ;
               fn[pk]=rsa_fit_cv(rdm_over,nsub,nitem,srdm,NULL,fit+fi,mv,
                                  cpset->perm+(size_t)pk*nitem,fit_condfold,
                                  fitws[fi],mw,
                                  (pk==0 && fitfoldz)?fitfoldz+(size_t)fi*nfitfold:NULL,
                                  (pk==0 && fitvalid)?fitvalid+(size_t)fi*nfitfold:NULL) ;
             }
             obs=fn[0] ;
             for( pk=0 ; pk<nfitperm ; pk++ ) if( fabsf(fn[pk])>=fabsf(obs) ) exceed++ ;
             fpv[fi][kk_]=(float)exceed/nfitperm ;
             fzv[fi][kk_]=THD_perm_signed_z(fpv[fi][kk_],obs,PERM_TAIL_TWO) ;
           } else {
             obs=rsa_fit_cv(rdm_over,nsub,nitem,srdm,NULL,fit+fi,mv,NULL,
                            fit_condfold,fitws[fi],fitws[fi]->foldw,
                            fitfoldz?fitfoldz+(size_t)fi*nfitfold:NULL,
                            fitvalid?fitvalid+(size_t)fi*nfitfold:NULL) ;
             fpv[fi][kk_]=-1.0f ; fzv[fi][kk_]=MYatanh(obs) ;
           }
           fr[fi][kk_]=obs ;
           for( cc=0 ; cc<fit[fi].ncomp ; cc++ )
             fwgt[fit[fi].wbase+cc][kk_]=fitws[fi]->foldw[cc] ;
         }
         /* F14 paired held-out fitted-model difference, under the exact same
            condition relabeling and complete refit on both sides. */
         for( cc=0 ; cc<nfitcon ; cc++ ){
           int ia=fcon[cc].ia,ib=fcon[cc].ib ;
           if( fit_contrast_rset!=NULL ){
             float *pn=do_fitconfwe?fcnull+(size_t)cc*nfitperm:NULL ;
             THD_permstat hs=rsa_fit_superiority_test(
               nsub,(fit_condfold!=NULL)?fit_condfold->nfold:1,
               fitfoldz+(size_t)ia*nfitfold,fitvalid+(size_t)ia*nfitfold,
               fitfoldz+(size_t)ib*nfitfold,fitvalid+(size_t)ib*nfitfold,
               fit_contrast_rset,hdraw,pn) ;
             fcd[cc][kk_]=hs.stat ; fcp[cc][kk_]=hs.pval ; fcz[cc][kk_]=hs.zscr ;
             if( do_fitconfwe ) THD_max_accum(nfitperm,fc_mx+(size_t)cc*nfitperm,pn) ;
           } else if( do_fitconfwe ){
             int exceed=0 ; float *dn=fcnull+(size_t)cc*nfitperm ;
             for( pk=0 ; pk<nfitperm ; pk++ )
               dn[pk]=MYatanh(fnull[(size_t)ia*nfitperm+pk])
                     -MYatanh(fnull[(size_t)ib*nfitperm+pk]) ;
             fcd[cc][kk_]=dn[0] ;
             for( pk=0 ; pk<nfitperm ; pk++ ) if( fabsf(dn[pk])>=fabsf(dn[0]) ) exceed++ ;
             fcp[cc][kk_]=(float)exceed/nfitperm ;
             fcz[cc][kk_]=THD_perm_signed_z(fcp[cc][kk_],dn[0],PERM_TAIL_TWO) ;
             for( pk=0 ; pk<nfitperm ; pk++ ) dn[pk]=fabsf(dn[pk]) ;
             THD_max_accum(nfitperm,fc_mx+(size_t)cc*nfitperm,dn) ;
           } else {
             fcd[cc][kk_]=MYatanh(fr[ia][kk_])-MYatanh(fr[ib][kk_]) ;
             fcp[cc][kk_]=-1.0f ; fcz[cc][kk_]=fcd[cc][kk_] ;
           }
         }
         if( do_fitfwe ) for( fi=0 ; fi<nfit ; fi++ ){
           float *fn=fnull+(size_t)fi*nfitperm ;
           for( pk=0 ; pk<nfitperm ; pk++ ) fn[pk]=fabsf(fn[pk]) ;
           THD_max_accum(nfitperm,f_mx+(size_t)fi*nfitperm,fn) ;
         }
       }
     }

     rsa_progress_advance(&progress) ;
   } /* end ROI loop */

   /* fold this thread's private max-null into the shared one (same reduction
      the thd_permute drivers use), so the FWE null is thread-count independent */
   if( do_fwe )
#pragma omp critical
   { THD_max_accum( nmod*npfwe , mxflat , my_mx ) ;
     if( run_resolved ) THD_max_accum(nmod*npfwe,run_mxflat,run_my_mx) ;
     if( do_loofwe ) THD_max_accum( nloofam*npfwe , lmxflat , l_mx ) ;
     if( do_confwe ) THD_max_accum( ncon*npfwe , cmxflat , c_mx ) ;
     if( do_cafwe  ) THD_max_accum( ncomq*ncaperm , camx , ca_mx ) ; }
   if( do_fitfwe )
#pragma omp critical
   { THD_max_accum(nfit*nfitperm,fmx,f_mx) ; }
   if( do_fitconfwe )
#pragma omp critical
   { THD_max_accum(nfitcon*nfitperm,fcmx,fc_mx) ; }

   free(F) ; free(ipat) ; free(tsmain) ; free(tslag) ; free(tsprep) ; free(tsnorm) ;
   free(rneural) ; free(rF) ; free(rtri) ; free(rstat) ; free(rprstat) ;
   free(rpval) ; free(rzscr) ; free(jmz) ;
   free(rnull) ; free(mnull) ; free(rnge) ; free(run_my_mx) ;
   free(rcstat); free(rcpr); free(rcpval); free(rczscr); free(rcnull); free(rcsum); free(rcnge) ;
   free(phspec) ; free(phwork) ; free(phseries) ; free(phsc1) ; free(phsc2) ;
   if( tsneural != NULL ) THD_simmat_free(tsneural) ;
   free(mv) ; free(beta) ; free(prtl) ; free(pv) ;
   if( bdraw != NULL ) free(bdraw) ;
   if( hdraw != NULL ) free(hdraw) ;
   if( bseen != NULL ) free(bseen) ;
   if( br_y != NULL ) free(br_y) ;
   if( br_xflat != NULL ) free(br_xflat) ;
   if( brx != NULL ) free(brx) ;
   if( brcoef != NULL ) free(brcoef) ;
   if( brbeta != NULL ) free(brbeta) ;
   if( bca_y != NULL ) free(bca_y) ;
   if( bca_a != NULL ) free(bca_a) ;
   if( bca_b != NULL ) free(bca_b) ;
   if( bca_c != NULL ) free(bca_c) ;
   if( bcadraw != NULL ) free(bcadraw) ;
   if( cb_y != NULL ) free(cb_y) ;
   if( cb_xflat != NULL ) free(cb_xflat) ;
   if( cbx != NULL ) free(cbx) ;
   if( cbsum != NULL ) free(cbsum) ;
   if( cbdraw != NULL ) free(cbdraw) ;
   if( cbbeta != NULL ) free(cbbeta) ;
   if( dualval != NULL ) free(dualval) ;
   if( dualwork != NULL ) free(dualwork) ;
   if( cws != NULL ) THD_rdm_ws_free(cws) ;
   if( rsub != NULL ) free(rsub) ;
   if( dsub != NULL ) free(dsub) ;
   if( tsz  != NULL ) free(tsz) ;
   if( tscd != NULL ) free(tscd) ;
   if( tscp != NULL ) free(tscp) ;
   if( tscz != NULL ) free(tscz) ;
   if( srdm != NULL ) free(srdm) ;
   free(srdmcov) ; free(mcovflat) ; free(mcov) ;
   if( mtriflat != NULL ) free(mtriflat) ;
   if( mtri != NULL ) free(mtri) ;
   if( triA  != NULL ) free(triA) ;
   if( triB  != NULL ) free(triB) ;
   free(covA) ; free(covB) ;
   if( Fh    != NULL ) free(Fh) ;
   if( my_mx != NULL ) free(my_mx) ;
   if( pnull != NULL ) free(pnull) ;
   if( l_mx  != NULL ) free(l_mx) ;
   free(lbpred) ; free(lbx) ; free(lby) ; free(lbtarg) ;
   if( c_mx  != NULL ) free(c_mx) ;
   if( cpnull!= NULL ) free(cpnull) ;
   if( ca_mx != NULL ) free(ca_mx) ;
   if( capnull!= NULL ) free(capnull) ;
   if( casub != NULL ) free(casub) ;
   if( fitws != NULL ){ int fi ; for( fi=0 ; fi<nfit ; fi++ ) rsa_fitws_free(fitws[fi]) ; free(fitws) ; }
   free(fitfoldz) ; free(fitvalid) ;
   free(f_mx) ; free(fnull) ; free(fc_mx) ; free(fcnull) ;
   if( rpat  != NULL ){ int rr_ ; for( rr_=0 ; rr_ < maxrun_l ; rr_++ ) free(rpat[rr_]) ; free(rpat) ; }
   free(runraw) ;
   if( wh.residbuf != NULL ) free(wh.residbuf) ;
   if( wh.Rmat     != NULL ) free(wh.Rmat) ;
   if( wh.Whalf    != NULL ) free(wh.Whalf) ;
   if( wh.wdiag    != NULL ) free(wh.wdiag) ;
   if( wh.wtmp     != NULL ) free(wh.wtmp) ;
   THD_rdm_ws_free(ws) ;
 }
 AFNI_OMP_END ;

   if( !quiet && progress_mode!=RSA_PROGRESS_OFF )
     INFO_message("3dRSA [5/5] Reducing FWE/FDR results and writing outputs...") ;

   /* F20: a model series declares time x space as one searched family.  Each
      per-time max-null above already spans space; collapse those synchronized
      nulls across time before lookup.  Retaining the per-time arrays during the
      parallel sweep keeps the existing estimator/cache paths unchanged. */
   if( series_file != NULL && do_fwe ){
     int pk ;
     for( pk=0 ; pk < npfwe ; pk++ ){
       float mx=-FLT_MAX ;
       for( mm=0 ; mm < nmod ; mm++ )
         if( mxflat[(size_t)mm*npfwe+pk] > mx ) mx=mxflat[(size_t)mm*npfwe+pk] ;
       mxflat[pk]=mx ;
     }
   }

   /*----- FDR across ROIs, or jointly across time x space for F20 -----*/

   if( nperm > 0 || rdm_over == RDM_BRICK ){
     if( series_file != NULL ) bh_fdr_series(nmod,nroi,pp,qq) ;
     else for( mm=0 ; mm < nmod ; mm++ ) THD_bh_fdr( nroi , pp[mm] , qq[mm] ) ;
   } else {
     for( mm=0 ; mm < nmod ; mm++ )
       for( kk=0 ; kk < nroi ; kk++ ) qq[mm][kk] = -1.0f ;
   }
   if( run_resolved ){
     for( mm=0 ; mm<nmod ; mm++ ){
       if( nperm>0 && nrunconspec>0 ){
         int ru,nfam=(run_analysis==RUN_ANALYSIS_SEPARATE?series_runs->nrun:0)+nrunconspec ;
         float **pvx=(float **)malloc(sizeof(float *)*nfam) ;
         float **qvx=(float **)malloc(sizeof(float *)*nfam) ; int nx=0 ;
         if( run_analysis==RUN_ANALYSIS_SEPARATE ) for( ru=0 ; ru<series_runs->nrun ; ru++ ){
           pvx[nx]=run_pp[mm*series_runs->nrun+ru]; qvx[nx]=run_qq[mm*series_runs->nrun+ru]; nx++ ;
         }
         for( ru=0 ; ru<nrunconspec ; ru++ ){
           pvx[nx]=rcon_pp[mm*nrunconspec+ru]; qvx[nx]=rcon_qq[mm*nrunconspec+ru]; nx++ ;
         }
         bh_fdr_series(nfam,nroi,pvx,qvx); free(pvx); free(qvx) ;
       } else if( nperm>0 )
         bh_fdr_series(series_runs->nrun,nroi,
                       run_pp+(size_t)mm*series_runs->nrun,
                       run_qq+(size_t)mm*series_runs->nrun) ;
       else {
         int ru ; for( ru=0 ; ru<series_runs->nrun ; ru++ )
           for( kk=0 ; kk<nroi ; kk++ )
             run_qq[mm*series_runs->nrun+ru][kk]=-1.0f ;
         for( ru=0 ; ru<nrunconspec ; ru++ )
           for( kk=0 ; kk<nroi ; kk++ ) rcon_qq[mm*nrunconspec+ru][kk]=-1.0f ;
       }
     }
   }
   if( do_loo && nperm > 0 ){
     for( mm=0 ; mm < nmod ; mm++ )
       if( rsa_model_has_loo(mod+mm) ){
         int own=loo_owner[mm] ;
         if( own == mm ) THD_bh_fdr( nroi , lp[mm] , lq[mm] ) ;
         else memcpy( lq[mm] , lq[own] , sizeof(float)*nroi ) ;
       }
   } else if( do_loo ){
     for( mm=0 ; mm < nmod ; mm++ )
       for( kk=0 ; kk < nroi ; kk++ ) lq[mm][kk] = -1.0f ;
   }
   if( ncon > 0 ){
     int cc ;
     if( nperm > 0 )
       for( cc=0 ; cc < ncon ; cc++ ) THD_bh_fdr( nroi , cp[cc] , cq[cc] ) ;
     else
       for( cc=0 ; cc < ncon ; cc++ )
         for( kk=0 ; kk < nroi ; kk++ ) cq[cc][kk] = -1.0f ;
   }
   if( ncomq > 0 ){
     int q ;
     if( nperm > 0 )
       for( q=0 ; q < ncomq ; q++ ) THD_bh_fdr( nroi , cap[q] , caq[q] ) ;
     else
       for( q=0 ; q < ncomq ; q++ )
         for( kk=0 ; kk < nroi ; kk++ ) caq[q][kk] = -1.0f ;
   }
   if( nfit>0 ){
     if( nperm>0 ) for( ii=0 ; ii<nfit ; ii++ ) THD_bh_fdr(nroi,fpv[ii],fqv[ii]) ;
     else for( ii=0 ; ii<nfit ; ii++ ) for( kk=0 ; kk<nroi ; kk++ ) fqv[ii][kk]=-1.0f ;
   }
   if( nfitcon>0 ){
     if( nperm>0 ) for( ii=0 ; ii<nfitcon ; ii++ ) THD_bh_fdr(nroi,fcp[ii],fcq[ii]) ;
     else for( ii=0 ; ii<nfitcon ; ii++ ) for( kk=0 ; kk<nroi ; kk++ ) fcq[ii][kk]=-1.0f ;
   }

   /*----- max-statistic FWE p and z: per model, or joint time x space -----*/

   if( do_fwe ){
     int pk ;
     for( mm=0 ; mm < nmod ; mm++ ){
       float *mn = (series_file != NULL) ? mxflat
                                         : mxflat + (size_t)mm*npfwe ;
       /* an untouched slot means every element was masked/degenerate here */
       for( pk=0 ; pk < npfwe ; pk++ ) if( mn[pk] == -FLT_MAX ) mn[pk] = 0.0f ;
       qsort( mn , npfwe , sizeof(float) , flt_cmp_asc ) ;
       for( kk=0 ; kk < nroi ; kk++ ){
         /* observed statistic on the same scale as the null: r/beta for IS-RSA,
            mean Fisher z for the classic condition null, otherwise one-sample t */
         float obs = (rdm_over == RDM_SUBJ) ? rr[mm][kk] : ee[mm][kk] ;
         pf[mm][kk] = THD_perm_emp_pvalue( mn , npfwe , fabsf(obs) ) ;
         zf[mm][kk] = THD_perm_signed_z( pf[mm][kk] , obs , PERM_TAIL_TWO ) ;
       }
     }
     if( run_resolved ){
       int ru ;
       for( mm=0 ; mm<nmod ; mm++ ){
         float *mn=run_mxflat+(size_t)mm*npfwe ;
         for( pk=0 ; pk<npfwe ; pk++ ) if( mn[pk]==-FLT_MAX ) mn[pk]=0.0f ;
         qsort(mn,npfwe,sizeof(float),flt_cmp_asc) ;
         for( ru=0 ; ru<series_runs->nrun ; ru++ ){
           int ix=mm*series_runs->nrun+ru ;
           for( kk=0 ; kk<nroi ; kk++ ){
             run_pf[ix][kk]=THD_perm_emp_pvalue(mn,npfwe,fabsf(run_rr[ix][kk])) ;
             run_zf[ix][kk]=THD_perm_signed_z(run_pf[ix][kk],run_rr[ix][kk],PERM_TAIL_TWO) ;
           }
         }
         for( ru=0 ; ru<nrunconspec ; ru++ ){
           int ix=mm*nrunconspec+ru ;
           for( kk=0 ; kk<nroi ; kk++ ){
             rcon_pf[ix][kk]=THD_perm_emp_pvalue(mn,npfwe,fabsf(rcon_rr[ix][kk])) ;
             rcon_zf[ix][kk]=THD_perm_signed_z(rcon_pf[ix][kk],rcon_rr[ix][kk],PERM_TAIL_TWO) ;
           }
         }
       }
     }
   }

   /* LOO prediction FWE: its own max-null family (cross-validated accuracy),
      computed only for data-table models that LOO can actually predict */
   if( do_loofwe ){
     int pk ;
     for( mm=0 ; mm < nmod ; mm++ ){
       if( !rsa_model_has_loo(mod+mm) ){
         for( kk=0 ; kk < nroi ; kk++ ){ lpf[mm][kk] = -1.0f ; lzf[mm][kk] = 0.0f ; }
         continue ;
       }
       if( loo_owner[mm] == mm ){
         float *mn = lmxflat + (size_t)loo_fam[mm]*npfwe ;
         for( pk=0 ; pk < npfwe ; pk++ ) if( mn[pk] == -FLT_MAX ) mn[pk] = 0.0f ;
         qsort( mn , npfwe , sizeof(float) , flt_cmp_asc ) ;
         for( kk=0 ; kk < nroi ; kk++ ){
           lpf[mm][kk] = THD_perm_emp_pvalue( mn , npfwe , fabsf(lr[mm][kk]) ) ;
           lzf[mm][kk] = THD_perm_signed_z( lpf[mm][kk] , lr[mm][kk] , PERM_TAIL_TWO ) ;
         }
       } else {
         int own=loo_owner[mm] ;
         memcpy( lpf[mm] , lpf[own] , sizeof(float)*nroi ) ;
         memcpy( lzf[mm] , lzf[own] , sizeof(float)*nroi ) ;
       }
     }
   }

   /* model-contrast FWE: each contrast its own max-null family.  The observed
      value is the TEST statistic ce (the difference for IS-RSA, the t/W for
      classic RSA) -- the same scale the null was built on. */
   if( do_confwe ){
     int pk , cc ;
     for( cc=0 ; cc < ncon ; cc++ ){
       float *mn = cmxflat + (size_t)cc*npfwe ;
       for( pk=0 ; pk < npfwe ; pk++ ) if( mn[pk] == -FLT_MAX ) mn[pk] = 0.0f ;
       qsort( mn , npfwe , sizeof(float) , flt_cmp_asc ) ;
       for( kk=0 ; kk < nroi ; kk++ ){
         cpf[cc][kk] = (contrast_rset!=NULL && rdm_over==RDM_SUBJ)
                         ? rsa_mc_emp_pvalue(mn,npfwe,fabsf(ce[cc][kk]))
                         : THD_perm_emp_pvalue(mn,npfwe,fabsf(ce[cc][kk])) ;
         czf[cc][kk] = THD_perm_signed_z( cpf[cc][kk] , cd[cc][kk] , PERM_TAIL_TWO ) ;
       }
     }
   }

   /* commonality FWE: each raw/partial quantity has its own family */
   if( do_cafwe ){
     int pk , q ;
     for( q=0 ; q < ncomq ; q++ ){
       float *mn = camx + (size_t)q*ncaperm ;
       for( pk=0 ; pk < ncaperm ; pk++ ) if( mn[pk] == -FLT_MAX ) mn[pk] = 0.0f ;
       qsort( mn , ncaperm , sizeof(float) , flt_cmp_asc ) ;
       for( kk=0 ; kk < nroi ; kk++ ){
         capf[q][kk] = THD_perm_emp_pvalue( mn , ncaperm , fabsf(cav[q][kk]) ) ;
         cazf[q][kk] = THD_perm_signed_z( capf[q][kk] , cav[q][kk] , PERM_TAIL_TWO ) ;
       }
     }
   }
   if( do_fitfwe ){
     int pk,fi ;
     for( fi=0 ; fi<nfit ; fi++ ){
       float *mn=fmx+(size_t)fi*nfitperm ;
       for( pk=0 ; pk<nfitperm ; pk++ ) if( mn[pk]==-FLT_MAX ) mn[pk]=0.0f ;
       qsort(mn,nfitperm,sizeof(float),flt_cmp_asc) ;
       for( kk=0 ; kk<nroi ; kk++ ){
         fpf[fi][kk]=THD_perm_emp_pvalue(mn,nfitperm,fabsf(fr[fi][kk])) ;
         fzf[fi][kk]=THD_perm_signed_z(fpf[fi][kk],fr[fi][kk],PERM_TAIL_TWO) ;
       }
     }
   }
   if( do_fitconfwe ){
     int pk,cc ;
     for( cc=0 ; cc<nfitcon ; cc++ ){
       float *mn=fcmx+(size_t)cc*nfitperm ;
       for( pk=0 ; pk<nfitperm ; pk++ ) if( mn[pk]==-FLT_MAX ) mn[pk]=0.0f ;
       qsort(mn,nfitperm,sizeof(float),flt_cmp_asc) ;
       for( kk=0 ; kk<nroi ; kk++ ){
         fcpf[cc][kk]=(fit_contrast_rset!=NULL)
                        ? rsa_mc_emp_pvalue(mn,nfitperm,fabsf(fcd[cc][kk]))
                        : THD_perm_emp_pvalue(mn,nfitperm,fabsf(fcd[cc][kk])) ;
         fczf[cc][kk]=THD_perm_signed_z(fcpf[cc][kk],fcd[cc][kk],PERM_TAIL_TWO) ;
       }
     }
   }

   /*================== text table ==================*/

   {  char fn[THD_MAX_NAME] ; FILE *fp ;

      sprintf(fn,"%s.rsa.1D",prefix) ;
      fp = fopen(fn,"w") ;
      if( fp == NULL ) ERROR_exit("3dRSA: can't write '%s'",fn) ;

      fprintf(fp,"# 3dRSA output\n") ;
      fprintf(fp,"# output schema: 3dRSA-rsa-v2\n") ;
      fprintf(fp,"# numerical input contract: finite values required in every used table column, mask label, and analyzed data brick; data outside the union analysis domain are ignored\n#\n") ;
      fprintf(fp,"# mask     : %s\n",
              (maskname != NULL) ? maskname : "(none -- whole surface mesh)") ;
      fprintf(fp,"# mode     : %s%s\n",
              (rdm_over==RDM_SUBJ)?"IS-RSA":"RSA",
              (rdm_over==RDM_SUBJ && mode==MODE_BETA)?" (feature=pattern)":
              (rdm_over==RDM_SUBJ && mode==MODE_RDM)?" (feature=rdm)":"") ;
      if( mode == MODE_RDM )
        fprintf(fp,"# condition estimator: %s\n",
                (runset != NULL) ?
                  ((noise_norm==NN_NONE) ? "crossnobis (unwhitened)" :
                   (noise_norm==NN_DIAG) ? "crossnobis (diagonal noise normalization)" :
                                           "crossnobis (shrinkage noise normalization)")
                                     : THD_simmat_metric_label(cond_metric)) ;
      if( rdm_over == RDM_BRICK )
        fprintf(fp,"# estimator: %s\n",
                (runset != NULL) ?
                  ((noise_norm==NN_NONE) ? "crossnobis (unwhitened)" :
                   (noise_norm==NN_DIAG) ? "crossnobis (diagonal noise normalization)" :
                                           "crossnobis (shrinkage noise normalization)")
                                     : "same-data condition-pattern RDM") ;
      if( rdm_over==RDM_BRICK || mode==MODE_RDM ){
        int imetric=(rdm_over==RDM_BRICK) ? neu_metric : cond_metric ;
        if( runset != NULL )
          fprintf(fp,"# condition centering: not applied; crossnobis uses within-run "
                     "condition contrasts (common offsets cancel)\n") ;
        else if( center_conditions && imetric==SIM_EUCLID )
          fprintf(fp,"# condition centering: subject requested; Euclidean distances "
                     "are invariant (exact legacy computation retained)\n") ;
        else if( center_conditions )
          fprintf(fp,"# condition centering: subject-wise voxel mean across conditions "
                     "removed before each ordinary RDM\n") ;
        else
          fprintf(fp,"# condition centering: none (raw-pattern compatibility default)\n") ;
      }
      if( runset != NULL && runset->has_condmap ){
        int cc ;
        if( runset->has_trialmap )
          fprintf(fp,"# trial-beta descriptor: TrialFile (Subj/Run inherited); "
                     "trial IDs unique within subject; condition trials averaged within run; "
                     "pair-specific valid-run denominators\n") ;
        else
          fprintf(fp,"# runwise condition mapping: ConditionFile; repeated labels averaged; "
                     "pair-specific valid-run denominators\n") ;
        fprintf(fp,"# condition order:") ;
        for( cc=0 ; cc<runset->ncond ; cc++ ) fprintf(fp," %s",runset->cond_lab[cc]) ;
        fprintf(fp,"\n") ;
      }
      fprintf(fp,"# matrices : %d x %d (rows are %s)\n",nitem,nitem,
              (rdm_over==RDM_SUBJ)?"subjects":"conditions") ;
      fprintf(fp,"# models   : %s%s\n",
              joint?"joint regression":(regout?"separate, nuisance-adjusted":"tested separately"),
              (nort>0)?"  [with -ortvec nuisances projected out]":"") ;
      if( ncon+nfitcon > 0 ){
        const char *ch=(contrast_hypothesis==CONTRAST_SUPERIORITY)?"superiority":
                       (contrast_hypothesis==CONTRAST_ALIGNMENT)?"alignment":"legacy" ;
        fprintf(fp,"# contrast hypothesis: %s\n",ch) ;
        if( ncon>0 && rdm_over==RDM_BRICK )
          fprintf(fp,"# contrast estimand: zDiff=mean subject Fisher-z difference; rDiff=mean subject raw-correlation difference\n") ;
        else if( ncon>0 )
          fprintf(fp,"# contrast estimand: rDiff=r(neural,A)-r(neural,B)\n") ;
        if( contrast_hypothesis==CONTRAST_SUPERIORITY && ncon>0 &&
            rdm_over==RDM_SUBJ && nperm>0 ){
          fprintf(fp,"# contrast null construction: centered paired subject bootstrap; neural/A/B resampled together; duplicate-copy diagonal dyads omitted (equal-performance null)\n") ;
          fprintf(fp,"# contrast tail calculation: Monte-Carlo (1 + exceedances)/(1 + draws); max-FWE uses synchronized centered draws over space\n") ;
        }
        else if( contrast_hypothesis==CONTRAST_SUPERIORITY && ncon>0 &&
                 rdm_over==RDM_SUBJ )
          fprintf(fp,"# contrast null construction: none (-nperm 0; point estimate only)\n") ;
        else if( contrast_hypothesis==CONTRAST_SUPERIORITY && ncon>0 )
          fprintf(fp,"# contrast null construction: paired subject %s (equal-performance null)\n",
                  group_test?"signed-rank sign flips":"sign flips") ;
        else if( contrast_hypothesis==CONTRAST_ALIGNMENT && rdm_over==RDM_BRICK )
          fprintf(fp,"# contrast null construction: shared condition-label relabeling (sharp alignment null; not an equal-performance null)\n") ;
        else if( contrast_hypothesis==CONTRAST_ALIGNMENT )
          fprintf(fp,"# contrast null construction: shared subject-label/temporal relabeling (sharp alignment null; not an equal-performance null)\n") ;
        else
          fprintf(fp,"# contrast null construction: legacy mode-dependent relabeling/sign-flip behavior\n") ;
      }
      if( seed_mask != NULL ){
        fprintf(fp,"# representational connectivity: fixed seed ROI from %s\n",seed_mask) ;
        fprintf(fp,"# seed ROI: value %d; label %s; voxels/nodes %d\n",
                seedrl->val[0],(seedrl->lab[0]!=NULL)?seedrl->lab[0]:"-",
                seedrl->vox[0].nar) ;
        fprintf(fp,"# seed-target overlap: excluded %d location%s before inference; "
                   "BH/max-FWE family contains %d non-overlapping target%s\n",
                seed_excluded,(seed_excluded==1)?"":"s",nroi,(nroi==1)?"":"s") ;
        if( rdm_over==RDM_BRICK )
          fprintf(fp,"# seed estimand: subject-specific seed vs target condition-RDM "
                     "correlation; effect=tanh(mean subject Fisher z)\n") ;
        else
          fprintf(fp,"# seed estimand: seed vs target subject-geometry Mantel "
                     "correlation; seed held fixed under target relabeling\n") ;
      }
      if( nfit>0 ){
        fprintf(fp,"# fitted models: nonnegative ridge components; ridge %.7g; %s\n",
                fit_ridge,(fit_condfold!=NULL)
                  ? "strict held-subject x held-condition fit and scoring"
                  : "leave-one-subject-out fit and scoring") ;
        if( fit_condfold!=NULL ){
          int ff ;
          fprintf(fp,"# fitted condition folds: %s; train=train/train, test=held/held; "
                     "cross-boundary dyads excluded\n",fit_condfold_file) ;
          fprintf(fp,"# fitted fold labels/sizes:") ;
          for( ff=0 ; ff<fit_condfold->nfold ; ff++ )
            fprintf(fp," %s:%d",fit_condfold->label[ff],fit_condfold->nmember[ff]) ;
          fprintf(fp,"\n") ;
        }
        if( contrast_hypothesis!=CONTRAST_SUPERIORITY || nfitcon==0 )
          fprintf(fp,"# fitted null: %s; the complete nested fit is repeated per relabeling%s\n",
                  (rdm_over==RDM_SUBJ)?"joint subject-label relabeling":"joint condition-label relabeling",
                  (nperm>0)?"":" (point estimates only)") ;
      }
      if( nfitcon>0 ){
        fprintf(fp,"# fitted contrasts: paired mean held-fold Fisher-z accuracy differences; same folds valid for both models\n") ;
        if( contrast_hypothesis==CONTRAST_SUPERIORITY && nperm>0 ){
          fprintf(fp,"# fitted contrast null construction: centered paired outer-subject bootstrap of common-valid held-fold effects (equal-performance null)\n") ;
          fprintf(fp,"# fitted contrast tail calculation: Monte-Carlo (1 + exceedances)/(1 + draws); max-FWE uses synchronized centered draws over space\n") ;
        } else if( contrast_hypothesis==CONTRAST_SUPERIORITY )
          fprintf(fp,"# fitted contrast null construction: none (-nperm 0; point estimate only)\n") ;
        else
          fprintf(fp,"# fitted contrast null construction: same shared relabeling and complete refit for both models%s\n",
                  (nperm>0)?"":" (point estimates only)") ;
      }
      if( ncomq > 0 ){
        int rq,npair=0,ntriple=0 ;
        for( rq=0 ; rq<nreqcom ; rq++ )
          if( com[rq].nmodel==2 ) npair++ ; else ntriple++ ;
        fprintf(fp,"# commonality requests: %d pairwise, %d three-predictor; "
                   "triple output is 7 raw regions + 3 conditional partial-R2 effects\n",
                npair,ntriple) ;
        fprintf(fp,"# commonality null: %s%s\n",
                (rdm_over==RDM_SUBJ) ?
                  "subject-label Freedman-Lane / complete relabeling" :
                  "shared condition-label Freedman-Lane / complete relabeling",
                (nperm>0) ? "" : " (point estimates only)") ;
      }
      if( ncomq > 0 && nperm > 0 )
        fprintf(fp,"# commonality relabelings: %d\n",ncaperm) ;
      fprintf(fp,"# metric   : %s neural, %s comparison\n",
              THD_simmat_metric_label(neu_metric),
              THD_simmat_cmp_label(cmp_metric)) ;
      if( cmp_metric==CMP_CORR_COV || cmp_metric==CMP_COS_COV )
        fprintf(fp,"# RDM covariance: V=(C C') o (C C'); exchangeable-condition "
                   "zero-distance approximation; balanced runwise crossnobis\n") ;
      if( cmp_metric==CMP_CORR_COV || cmp_metric==CMP_COS_COV )
        fprintf(fp,"# covariance-model contract: fixed model matrices are interpreted "
                   "as dissimilarity RDMs%s\n",
                (cmp_metric==CMP_COS_COV)?" with a meaningful zero":"") ;
      if( cmp_metric==CMP_RHOA )
        fprintf(fp,"# rho-a contract: expected Spearman under independent random tie breaking; scalar comparison only\n") ;
      { int na=(run_resolved && pset!=NULL)?pset->nperm:nperm ;
        fprintf(fp,"# nperm    : %d   seed: %ld",na,seed) ;
        if( na!=nperm ) fprintf(fp,"   requested: %d (exact group enumerated)",nperm) ;
        fprintf(fp,"\n") ; }
      fprintf(fp,"# null     : %s%s\n",
              (null_mode==NULL_TIMESHIFT)?"circular timeshift":
              (null_mode==NULL_PHASE)?"Fourier phase randomization":
              (rdm_over==RDM_BRICK && classic_null==CLASSIC_NULL_CONDITIONS)?
                "condition labels (fixed observed subjects)":
              (rdm_over==RDM_BRICK)?"subject sign flips (population subjects)":"subject labels",
              (null_mode==NULL_TIMESHIFT)?" (see min_shift below)":"") ;
      if( rdm_over==RDM_BRICK && classic_null==CLASSIC_NULL_CONDITIONS )
        fprintf(fp,"# classic condition-null statistic: mean subject Fisher z; "
                   "one row+column relabeling shared over subjects and space\n") ;
      if( seed_mask!=NULL && rdm_over==RDM_BRICK &&
          classic_null==CLASSIC_NULL_CONDITIONS )
        fprintf(fp,"# seed condition null: relabel seed condition axes relative to "
                   "each subject's target RDM; same relabeling over subjects/space\n") ;
      if( null_mode==NULL_TIMESHIFT )
        fprintf(fp,"# min_shift: %d TR%s\n",min_shift,(min_shift==1)?"":"s") ;
      if( null_mode==NULL_TIMESHIFT )
        fprintf(fp,"# time-shift engine: subject-pair relative-lag lookup; model side unshifted\n") ;
      if( null_mode==NULL_TIMESHIFT && regout )
        fprintf(fp,"# time-shift regression null: fixed conditional design; complete neural-series alignment null\n") ;
      if( null_mode==NULL_PHASE )
        fprintf(fp,"# phase engine: local-spectrum real FFT cache; DC/Nyquist and model side unrandomized; power spectrum preserved\n") ;
      if( null_mode==NULL_PHASE )
        fprintf(fp,"# phase family: stateless subject x frequency draws shared over ROIs/searchlights; identity slot 0\n") ;
      if( null_mode==NULL_PHASE && regout )
        fprintf(fp,"# phase regression null: fixed conditional design; complete neural-series phase-alignment null\n") ;
      if( dualboot )
        fprintf(fp,"# dual_bootstrap: %d synchronized subject x condition draws; "
                   "groups: %d; usable condition draws: %d; corrected variance t CI: %.6g%%; df: %d\n",
                nboot,crset->ngroup,crset->nvalid,boot_ci,
                ((nsub<crset->ngroup)?nsub:crset->ngroup)-1) ;
      if( dualboot )
        fprintf(fp,"# dual estimator: finite-sample corrected Vs + Vc - interaction; "
                   "bounded by corrected one-axis variances and simultaneous Vsc; scale: %s\n",
                joint?"standardized beta":"Fisher z (tanh endpoints)") ;
      else if( nboot > 0 )
        fprintf(fp,"# bootstrap: %d   percentile CI: %.6g%%\n",nboot,boot_ci) ;
      if( nboot>0 && block_lab!=NULL )
        fprintf(fp,"# bootstrap strata: %s; within-stratum subject resampling; "
                   "original stratum sizes preserved; not a cluster bootstrap\n",
                block_col) ;
      if( ncboot > 0 && !dualboot )
        fprintf(fp,"# cond_bootstrap: %d   groups: %d   usable: %d   percentile CI: %.6g%%\n",
                ncboot,crset->ngroup,crset->nvalid,boot_ci) ;
      if( do_loo )
        fprintf(fp,"# loo      : %d model output%s from %d distinct target/estimand famil%s\n"
                   "# loo estimands: AnnaK=foldwise neural typicality regression; "
                   "scalar NN=rank-weighted neighbors; profile=mean measure-wise correlation\n",
                nloo,(nloo==1)?"":"s",nloofam,(nloofam==1)?"y":"ies") ;
      if( do_loo && nboot>0 )
        fprintf(fp,"# loo bootstrap: completed out-of-sample prediction/target rows; "
                   "predictions held fixed (no fold-refitting uncertainty)\n") ;
      if( do_nc && rdm_over == RDM_BRICK )
        fprintf(fp,"# noise ceiling: Nili subject LOO lower / inclusive upper on %s RDMs\n",
                (runset != NULL) ? "run-independent crossnobis" :
                                   "same-data condition-pattern") ;
      if( series_file != NULL ){
        fprintf(fp,"# model_series: %s\n",series_file) ;
        fprintf(fp,"# timepoints: %d (input order preserved; t#### maps to time_label)\n",
                nseries) ;
        fprintf(fp,"# multiplicity: BH FDR and max-statistic FWE use one joint "
                   "time x space family\n") ;
      }
      if( series_runs!=NULL ){
        int ru,rm ;
        fprintf(fp,"# repeated runs: column %s; %d runs; %d total TRs; "
                   "within-run %s; analysis=%s\n",run_column,series_runs->nrun,nvals,
                run_normalize==RUN_NORM_ZSCORE?"zscore":
                run_normalize==RUN_NORM_DEMEAN?"demean":"none",
                run_analysis==RUN_ANALYSIS_SEPARATE?"separate+equal-run-mean":
                run_analysis==RUN_ANALYSIS_MEAN?"equal-run-mean":"concatenate (TR-weighted)") ;
        fprintf(fp,"# run order/length:") ;
        for( ru=0 ; ru<series_runs->nrun ; ru++ )
          fprintf(fp," %s=%d",series_runs->run_label[ru],series_runs->run_nval[ru]) ;
        fprintf(fp,"\n") ;
        for( rm=0 ; rm<nrunmodspec ; rm++ ){
          char *co=strrchr(runmodspec[rm],':') ; int centered=0,cc ;
          size_t nc=(size_t)(co-runmodspec[rm]) ;
          for( cc=0 ; cc<nruncenter ; cc++ )
            if( strlen(runcenter[cc])==nc && strncasecmp(runcenter[cc],runmodspec[rm],nc)==0 ) centered=1 ;
          fprintf(fp,"# run model: %s; %s; whole subject trajectories relabeled together across runs/space\n",
                  runmodspec[rm],centered?"subject-centered state + trait decomposition":"raw run-specific values") ;
        }
        for( rm=0 ; rm<nrunfactorspec ; rm++ ){
          fprintf(fp,"# run factor: %s; fixed labeled runs:",runfactor[rm].column) ;
          for( ru=0 ; ru<series_runs->nrun ; ru++ )
            fprintf(fp," %s=%s",series_runs->run_label[ru],
                    runfactor[rm].level[runfactor[rm].run_level[ru]]) ;
          fprintf(fp,"\n") ;
        }
        for( rm=0 ; rm<nrunconspec ; rm++ )
          fprintf(fp,"# run contrast: %s = mean(%s:%s) - mean(%s:%s); equal run weights; fixed-run estimand\n",
                  runcon[rm].name,runfactor[runcon[rm].ifactor].column,
                  runfactor[runcon[rm].ifactor].level[runcon[rm].ipos],
                  runfactor[runcon[rm].ifactor].column,
                  runfactor[runcon[rm].ifactor].level[runcon[rm].ineg]) ;
      }
      fprintf(fp,"# nsubj    : %d\n#\n",nsub) ;

      if( run_resolved ){
        int ru,nsum,ix,nbase ;
        fprintf(fp,"# run-resolved estimator: %s is computed within each run;\n"
                   "# MEAN is the equal-run arithmetic mean of signed %s\n",
                   joint?"a standardized joint-regression coefficient and partial r":"model association",
                   joint?"coefficients/partial correlations":"association statistics") ;
        if( joint )
          fprintf(fp,"# joint null: model-specific Freedman-Lane reduced residuals; fixed and run-varying models held conditional\n"
                     "# planned contrasts: differences of standardized coefficients (partial-r differences are descriptive)\n") ;
        fprintf(fp,
                   "# run null synchronization: one subject relabeling is shared by every run and location\n") ;
        if( run_analysis==RUN_ANALYSIS_SEPARATE )
          fprintf(fp,"# multiplicity: per model, run effects and planned contrasts share joint run/contrast x space BH FDR and max-FWE; MEAN uses space\n") ;
        else fprintf(fp,"# multiplicity: per model, planned contrasts share contrast x space BH FDR/max-FWE; MEAN uses space\n") ;
        fprintf(fp,"#%-6s %-28s %7s %-18s %-20s %14s",
                "ROI","label","nvox","summary","model",joint?"beta":"effect") ;
        if( joint ) fprintf(fp," %14s","partial_r") ;
        fprintf(fp," %14s %14s","p","q") ;
        if( do_fwe ) fprintf(fp," %14s","pfwe") ;
        fprintf(fp," %14s",(nperm>0)?"z":"uncalibrated") ;
        if( do_fwe ) fprintf(fp," %14s","zfwe") ;
        fprintf(fp,"\n") ;
        nbase=1+((run_analysis==RUN_ANALYSIS_SEPARATE)?series_runs->nrun:0) ;
        nsum=nbase+nrunconspec ;
        for( mm=0 ; mm<nmod ; mm++ ) for( kk=0 ; kk<nroi ; kk++ ){
          for( ru=0 ; ru<nsum ; ru++ ){
            const char *slab ; float ev,pev=0.0f,pv2,qv2,zv2,pfv2=-1.0f,zfv2=0.0f ;
            if( ru==0 ){ slab="MEAN"; ev=rr[mm][kk]; pv2=pp[mm][kk];
              pev=ee[mm][kk];
              qv2=qq[mm][kk]; zv2=zz[mm][kk];
              if( do_fwe ){ pfv2=pf[mm][kk]; zfv2=zf[mm][kk]; }
            } else if( ru<nbase ){ int rrn=ru-1; ix=mm*series_runs->nrun+rrn; slab=series_runs->run_label[rrn];
              ev=run_rr[ix][kk]; pv2=run_pp[ix][kk]; qv2=run_qq[ix][kk]; zv2=run_zz[ix][kk];
              pev=run_ee[ix][kk];
              if( do_fwe ){ pfv2=run_pf[ix][kk]; zfv2=run_zf[ix][kk]; }
            } else { int cc=ru-nbase; ix=mm*nrunconspec+cc; slab=runcon[cc].name;
              ev=rcon_rr[ix][kk]; pv2=rcon_pp[ix][kk]; qv2=rcon_qq[ix][kk]; zv2=rcon_zz[ix][kk];
              pev=rcon_ee[ix][kk];
              if( do_fwe ){ pfv2=rcon_pf[ix][kk]; zfv2=rcon_zf[ix][kk]; }
            }
            fprintf(fp," %-6d %-28s %7d %-18s %-20s %14.6f",
                    rl->val[kk],(rl->lab[kk]!=NULL)?rl->lab[kk]:"-",rl->vox[kk].nar,
                    slab,mod[mm].name,ev) ;
            if( joint ) fprintf(fp," %14.6f",pev) ;
            fprintf(fp," %14.6g %14.6g",pv2,qv2) ;
            if( do_fwe ) fprintf(fp," %14.6g",pfv2) ;
            fprintf(fp," %14.6f",zv2) ; if( do_fwe ) fprintf(fp," %14.6f",zfv2) ;
            fprintf(fp,"\n") ;
          }
        }
      } else if( series_file != NULL ){
        fprintf(fp,"#%-6s %-28s %7s %10s %-20s %14s %14s %14s %14s",
                "ROI","label","nvox","time_index","time_label","effect","stat","p","q") ;
        if( do_fwe ) fprintf(fp," %14s","pfwe") ;
        fprintf(fp," %14s","z") ;
        if( do_fwe ) fprintf(fp," %14s","zfwe") ;
        if( nboot > 0 ) fprintf(fp," %14s %14s",dualboot?"dualLo":"bootLo",
                                                     dualboot?"dualHi":"bootHi") ;
        if( ncboot > 0 && !dualboot ) fprintf(fp," %14s %14s","cbootLo","cbootHi") ;
        if( do_nc ){
          if( rdm_over == RDM_SUBJ ) fprintf(fp," %14s","reliability") ;
          else                       fprintf(fp," %14s %14s","nc_low","nc_high") ;
        }
        fprintf(fp,"\n") ;
        for( mm=0 ; mm < nmod ; mm++ )
          for( kk=0 ; kk < nroi ; kk++ ){
            fprintf(fp," %-6d %-28s %7d %10d %-20s %14.6f %14.6f %14.6g %14.6g",
                    rl->val[kk],(rl->lab[kk]!=NULL)?rl->lab[kk]:"-",
                    rl->vox[kk].nar,mm,series_time[mm],
                    rr[mm][kk],ee[mm][kk],pp[mm][kk],qq[mm][kk]) ;
            if( do_fwe ) fprintf(fp," %14.6g",pf[mm][kk]) ;
            fprintf(fp," %14.6f",zz[mm][kk]) ;
            if( do_fwe ) fprintf(fp," %14.6f",zf[mm][kk]) ;
            if( nboot > 0 ) fprintf(fp," %14.6f %14.6f",blo[mm][kk],bhi[mm][kk]) ;
            if( ncboot > 0 && !dualboot )
              fprintf(fp," %14.6f %14.6f",cblo[mm][kk],cbhi[mm][kk]) ;
            if( do_nc ){
              if( rdm_over == RDM_SUBJ ) fprintf(fp," %14.6f",ncA[kk]) ;
              else                       fprintf(fp," %14.6f %14.6f",ncA[kk],ncB[kk]) ;
            }
            fprintf(fp,"\n") ;
          }
      } else {
      fprintf(fp,"#%-6s %-28s %7s","ROI","label","nvox") ;
      for( mm=0 ; mm < nmod ; mm++ ){
        if( regout )
          fprintf(fp," %14s_b %14s_pr %15s_p %15s_q",
                  mod[mm].name,mod[mm].name,mod[mm].name,mod[mm].name) ;
        else
          fprintf(fp," %14s_r %15s_p %15s_q",
                  mod[mm].name,mod[mm].name,mod[mm].name) ;
        if( do_fwe ) fprintf(fp," %12s_pfwe",mod[mm].name) ;
        if( nboot > 0 )
          fprintf(fp,dualboot?" %14s_dualLo %14s_dualHi":" %14s_bootLo %14s_bootHi",
                  mod[mm].name,mod[mm].name) ;
        if( ncboot > 0 && !dualboot )
          fprintf(fp," %14s_cbootLo %14s_cbootHi",mod[mm].name,mod[mm].name) ;
      }
      { int fi,cc ;
        for( fi=0 ; fi<nfit ; fi++ ){
          fprintf(fp," %13s_cvR %14s_cvP %14s_cvQ",fit[fi].name,fit[fi].name,fit[fi].name) ;
          if( do_fitfwe ) fprintf(fp," %11s_cvPfwe",fit[fi].name) ;
          for( cc=0 ; cc<fit[fi].ncomp ; cc++ )
            fprintf(fp," %12s_w_%s",fit[fi].name,mod[fit[fi].comp[cc]].name) ;
        }
      }
      { int cc ;
        for( cc=0 ; cc<nfitcon ; cc++ ){
          fprintf(fp," %11s_cvDiff %14s_cvP %14s_cvQ",
                  fcon[cc].name,fcon[cc].name,fcon[cc].name) ;
          if( do_fitconfwe ) fprintf(fp," %11s_cvPfwe",fcon[cc].name) ;
        }
      }
      if( do_loo )
        for( mm=0 ; mm < nmod ; mm++ )
          if( rsa_model_has_loo(mod+mm) ){
            fprintf(fp," %12s_looR %13s_looP %13s_looQ",
                    mod[mm].name,mod[mm].name,mod[mm].name) ;
            if( do_loofwe ) fprintf(fp," %10s_looPfwe",mod[mm].name) ;
            if( nboot>0 ) fprintf(fp," %10s_looBootLo %10s_looBootHi",
                                  mod[mm].name,mod[mm].name) ;
          }
      { int cc ;
        for( cc=0 ; cc < ncon ; cc++ ){
          if( rdm_over==RDM_BRICK )
            fprintf(fp," %13s_rDiff %13s_zDiff %14s_p %14s_q",
                    con[cc].name,con[cc].name,con[cc].name,con[cc].name) ;
          else
            fprintf(fp," %13s_diff %14s_p %14s_q",
                    con[cc].name,con[cc].name,con[cc].name) ;
          if( do_confwe ) fprintf(fp," %11s_pfwe",con[cc].name) ;
          if( nboot > 0 )
            fprintf(fp,(rdm_over==RDM_BRICK)
                         ? (dualboot?" %14s_zDiff_dualLo %14s_zDiff_dualHi":" %14s_zDiff_bootLo %14s_zDiff_bootHi")
                         : (dualboot?" %14s_dualLo %14s_dualHi":" %14s_bootLo %14s_bootHi"),
                    con[cc].name,con[cc].name) ;
        }
      }
      { int q ;
        for( q=0 ; q < ncomq ; q++ ){
          const char *nm = comlab[q] ;
          fprintf(fp," %18s %18s_p %18s_q",nm,nm,nm) ;
          if( do_cafwe ) fprintf(fp," %15s_pfwe",nm) ;
          if( nboot > 0 )
            fprintf(fp," %14s_bootLo %14s_bootHi",nm,nm) ;
        }
      }
      if( do_nc ){
        if( rdm_over == RDM_SUBJ ) fprintf(fp," %14s","reliability") ;
        else                       fprintf(fp," %14s %14s","nc_low","nc_high") ;
      }
      fprintf(fp,"\n") ;

      for( kk=0 ; kk < nroi ; kk++ ){
        fprintf(fp," %-6d %-28s %7d",
                rl->val[kk] , (rl->lab[kk] != NULL) ? rl->lab[kk] : "-" ,
                rl->vox[kk].nar) ;
        for( mm=0 ; mm < nmod ; mm++ ){
          if( regout )
            fprintf(fp," %16.6f %17.6f %17.6g %17.6g",
                    rr[mm][kk] , ee[mm][kk] , pp[mm][kk] , qq[mm][kk]) ;
          else
            fprintf(fp," %16.6f %17.6g %17.6g",
                    rr[mm][kk] , pp[mm][kk] , qq[mm][kk]) ;
          if( do_fwe ) fprintf(fp," %17.6g",pf[mm][kk]) ;
          if( nboot > 0 ) fprintf(fp," %20.6f %20.6f",blo[mm][kk],bhi[mm][kk]) ;
          if( ncboot > 0 && !dualboot )
            fprintf(fp," %20.6f %20.6f",cblo[mm][kk],cbhi[mm][kk]) ;
        }
        { int fi,cc ;
          for( fi=0 ; fi<nfit ; fi++ ){
            fprintf(fp," %17.6f %17.6g %17.6g",fr[fi][kk],fpv[fi][kk],fqv[fi][kk]) ;
            if( do_fitfwe ) fprintf(fp," %17.6g",fpf[fi][kk]) ;
            for( cc=0 ; cc<fit[fi].ncomp ; cc++ )
              fprintf(fp," %17.6f",fwgt[fit[fi].wbase+cc][kk]) ;
          }
        }
        { int cc ;
          for( cc=0 ; cc<nfitcon ; cc++ ){
            fprintf(fp," %18.6f %17.6g %17.6g",
                    fcd[cc][kk],fcp[cc][kk],fcq[cc][kk]) ;
            if( do_fitconfwe ) fprintf(fp," %17.6g",fcpf[cc][kk]) ;
          }
        }
        if( do_loo )
          for( mm=0 ; mm < nmod ; mm++ )
            if( rsa_model_has_loo(mod+mm) ){
              fprintf(fp," %17.6f %17.6g %17.6g",
                      lr[mm][kk] , lp[mm][kk] , lq[mm][kk]) ;
              if( do_loofwe ) fprintf(fp," %17.6g",lpf[mm][kk]) ;
              if( nboot>0 ) fprintf(fp," %20.6f %20.6f",
                                    lblo[mm][kk],lbhi[mm][kk]) ;
            }
        { int cc ;
          for( cc=0 ; cc < ncon ; cc++ ){
            if( rdm_over==RDM_BRICK )
              fprintf(fp," %18.6f %18.6f %16.6g %16.6g",
                      crd[cc][kk],cd[cc][kk],cp[cc][kk],cq[cc][kk]) ;
            else
              fprintf(fp," %18.6f %16.6g %16.6g",
                      cd[cc][kk],cp[cc][kk],cq[cc][kk]) ;
            if( do_confwe ) fprintf(fp," %16.6g",cpf[cc][kk]) ;
            if( nboot > 0 )
              fprintf(fp," %20.6f %20.6f",dblo[cc][kk],dbhi[cc][kk]) ;
          }
        }
        { int q ;
          for( q=0 ; q < ncomq ; q++ ){
            fprintf(fp," %18.6f %20.6g %20.6g",
                    cav[q][kk] , cap[q][kk] , caq[q][kk]) ;
            if( do_cafwe ) fprintf(fp," %20.6g",capf[q][kk]) ;
            if( nboot > 0 )
              fprintf(fp," %20.6f %20.6f",calo[q][kk],cahi[q][kk]) ;
          }
        }
        if( do_nc ){
          if( rdm_over == RDM_SUBJ ) fprintf(fp," %14.6f",ncA[kk]) ;
          else                       fprintf(fp," %14.6f %14.6f",ncA[kk],ncB[kk]) ;
        }
        fprintf(fp,"\n") ;
      }
      }
      fclose(fp) ;
      if( !quiet ) INFO_message("3dRSA: wrote table %s",fn) ;
   }

   /*================== dataset ==================*/

   if( do_dset && run_resolved ){
      int nbase=1+((run_analysis==RUN_ANALYSIS_SEPARATE)?series_runs->nrun:0) ;
      int nsumm=nbase+nrunconspec ;
      int per=2+(do_fwe?1:0),nbrik=nmod*nsumm*per,bs=0,su ;
      int z_is_fizt=(nperm>0) ;
      oset=EDIT_empty_copy(mset) ;
      EDIT_dset_items(oset,ADN_prefix,prefix,ADN_nvals,nbrik,ADN_ntt,0,
                      ADN_type,HEAD_FUNC_TYPE,ADN_func_type,FUNC_BUCK_TYPE,ADN_none) ;
      if( THD_deathcon() && THD_is_file(DSET_HEADNAME(oset)) )
        ERROR_exit("3dRSA: output dataset %s already exists",DSET_HEADNAME(oset)) ;
      for( mm=0 ; mm<nmod ; mm++ ) for( su=0 ; su<nsumm ; su++ ){
        int ru=su-1,ix=-1 ; const char *slab,*zsuf,*esuf ; float *er,*zr,*zfr=NULL ; char lab[220] ;
        if( su==0 ){ slab="MEAN"; er=rr[mm]; zr=zz[mm]; if(do_fwe)zfr=zf[mm]; }
        else if( su<nbase ){
          ix=mm*series_runs->nrun+ru; slab=series_runs->run_label[ru] ;
          er=run_rr[ix]; zr=run_zz[ix]; if(do_fwe)zfr=run_zf[ix] ;
        } else {
          int cc=su-nbase; ix=mm*nrunconspec+cc; slab=runcon[cc].name ;
          er=rcon_rr[ix]; zr=rcon_zz[ix]; if(do_fwe)zfr=rcon_zf[ix] ;
        }
        zsuf=z_is_fizt?"Z":(joint?"U":((su>=nbase)?"U":"FZ")) ;
        esuf=joint?((su>=nbase)?"bDiff":"b"):((su>=nbase)?"diff":"r") ;
        EDIT_substitute_brick(oset,bs,MRI_float,NULL) ;
        EDIT_substitute_brick(oset,bs+1,MRI_float,NULL) ;
        THD_roilist_paint(DSET_ARRAY(oset,bs),rl,er) ;
        THD_roilist_paint(DSET_ARRAY(oset,bs+1),rl,zr) ;
        snprintf(lab,sizeof(lab),"%.90s_%.76s_%s",mod[mm].name,slab,esuf); EDIT_BRICK_LABEL(oset,bs,lab) ;
        snprintf(lab,sizeof(lab),"%.90s_%.78s_%s",mod[mm].name,slab,zsuf); EDIT_BRICK_LABEL(oset,bs+1,lab) ;
        if( z_is_fizt ) EDIT_BRICK_TO_FIZT(oset,bs+1) ;
        EDIT_BRICK_FACTOR(oset,bs,0.0); EDIT_BRICK_FACTOR(oset,bs+1,0.0); bs+=2 ;
        if( do_fwe ){
          EDIT_substitute_brick(oset,bs,MRI_float,NULL) ;
          THD_roilist_paint(DSET_ARRAY(oset,bs),rl,zfr) ;
          snprintf(lab,sizeof(lab),"%.90s_%.74s_ZFWE",mod[mm].name,slab); EDIT_BRICK_LABEL(oset,bs,lab) ;
          EDIT_BRICK_TO_FIZT(oset,bs); EDIT_BRICK_FACTOR(oset,bs,0.0); bs++ ;
        }
      }
      THD_copy_labeltable_atr(oset->dblk,mset->dblk) ;
      if( (DBLK_IS_NI_SURF_DSET(mset->dblk)||DBLK_IS_GIFTI(mset->dblk)) &&
          mset->dblk->nnodes>0 && mset->dblk->node_list!=NULL ){
        int nn=mset->dblk->nnodes ;
        oset->dblk->node_list=(int *)RwcMalloc(sizeof(int)*nn); oset->dblk->nnodes=nn ;
        memcpy(oset->dblk->node_list,mset->dblk->node_list,sizeof(int)*nn) ;
      }
      tross_Copy_History(mset,oset); tross_Make_History("3dRSA",argc,argv,oset) ;
      DSET_write(oset); if( !quiet ) INFO_message("3dRSA: wrote dataset %s",DSET_BRIKNAME(oset)) ;
      do_dset=0 ;                    /* dedicated run-resolved writer handled it */
   }

   if( do_dset ){
      int nc_nbr = do_nc ? ((rdm_over == RDM_SUBJ) ? 1 : 2) : 0 ;  /* NC maps */
      int nconbrick = ((rdm_over==RDM_BRICK)?3:2)*ncon ;
      int base_nbrik = 2*nmod + 2*nloo + (do_fwe ? nmod : 0)
                                  + (do_loofwe ? nloo : 0)
                                  + nconbrick + (do_confwe ? ncon : 0)
                                  + 2*ncomq + (do_cafwe ? ncomq : 0)
                                  + nc_nbr ;
      int nbrik = base_nbrik + ((nboot > 0) ? 2*(nmod+ncon+ncomq+nloo) : 0)
                              + ((ncboot > 0 && !dualboot) ? 2*nmod : 0)
                              + 2*nfit + (do_fitfwe?nfit:0) + nfitw
                              + 2*nfitcon + (do_fitconfwe?nfitcon:0) , bslot ;
      /* The second brick per model is a calibrated z ONLY when a real test ran:
         a permutation z (nperm>0), or the classic-RSA parametric one-sample t
         across independent subjects (RDM_BRICK, even at nperm=0).  For IS-RSA at
         nperm=0 it is just atanh(effect) over non-independent dyads -- a useful
         effect map but NOT a standard normal, so it must not be FIZT-typed. */
      int z_is_fizt = (nperm > 0) || (rdm_over == RDM_BRICK) ;
      char *zsuf = z_is_fizt ? "Z" : "FZ" ;

      oset = EDIT_empty_copy( mset ) ;
      EDIT_dset_items( oset ,
                         ADN_prefix    , prefix         ,
                         ADN_nvals     , nbrik          ,
                         ADN_ntt       , 0              ,
                         ADN_type      , HEAD_FUNC_TYPE ,
                         ADN_func_type , FUNC_BUCK_TYPE ,
                       ADN_none ) ;

      if( THD_deathcon() && THD_is_file(DSET_HEADNAME(oset)) )
        ERROR_exit("3dRSA: output dataset %s already exists",DSET_HEADNAME(oset)) ;

      for( mm=0 ; mm < nmod ; mm++ ){
        char lab[160] ;
        EDIT_substitute_brick( oset , 2*mm   , MRI_float , NULL ) ;
        EDIT_substitute_brick( oset , 2*mm+1 , MRI_float , NULL ) ;
        THD_roilist_paint( DSET_ARRAY(oset,2*mm  ) , rl , rr[mm] ) ;
        THD_roilist_paint( DSET_ARRAY(oset,2*mm+1) , rl , zz[mm] ) ;

        sprintf(lab,"%.140s_%s",mod[mm].name,regout?"b":"r") ;
        EDIT_BRICK_LABEL(oset,2*mm,lab) ;
        sprintf(lab,"%.139s_%s",mod[mm].name,zsuf) ;
        EDIT_BRICK_LABEL(oset,2*mm+1,lab) ;
        if( z_is_fizt ) EDIT_BRICK_TO_FIZT(oset,2*mm+1) ;
        EDIT_BRICK_FACTOR(oset,2*mm  ,0.0) ;
        EDIT_BRICK_FACTOR(oset,2*mm+1,0.0) ;
      }

      /* LOO prediction maps, per predictable data-table model */
      bslot = 2*nmod ;
      if( do_loo )
        for( mm=0 ; mm < nmod ; mm++ ){
          char lab[160] ;
          if( !rsa_model_has_loo(mod+mm) ) continue ;
          EDIT_substitute_brick( oset , bslot   , MRI_float , NULL ) ;
          EDIT_substitute_brick( oset , bslot+1 , MRI_float , NULL ) ;
          THD_roilist_paint( DSET_ARRAY(oset,bslot  ) , rl , lr[mm] ) ;
          THD_roilist_paint( DSET_ARRAY(oset,bslot+1) , rl , lz[mm] ) ;
          sprintf(lab,"%.135s_looR",mod[mm].name) ;
          EDIT_BRICK_LABEL(oset,bslot,lab) ;
          sprintf(lab,"%.134s_loo%s",mod[mm].name,zsuf) ;
          EDIT_BRICK_LABEL(oset,bslot+1,lab) ;
          if( z_is_fizt ) EDIT_BRICK_TO_FIZT(oset,bslot+1) ;
          EDIT_BRICK_FACTOR(oset,bslot  ,0.0) ;
          EDIT_BRICK_FACTOR(oset,bslot+1,0.0) ;
          bslot += 2 ;
        }

      /* FWE-corrected z maps follow the fixed 2-per-model and LOO slots, whose
         established layout remains undisturbed. */
      if( do_fwe ){
        int fslot = 2*nmod + 2*nloo ;
        for( mm=0 ; mm < nmod ; mm++ ){
          char lab[160] ;
          EDIT_substitute_brick( oset , fslot , MRI_float , NULL ) ;
          THD_roilist_paint( DSET_ARRAY(oset,fslot) , rl , zf[mm] ) ;
          sprintf(lab,"%.133s_ZFWE",mod[mm].name) ;
          EDIT_BRICK_LABEL(oset,fslot,lab) ;
          EDIT_BRICK_TO_FIZT(oset,fslot) ;
          EDIT_BRICK_FACTOR(oset,fslot,0.0) ;
          fslot++ ;
        }
        /* then LOO-prediction FWE z maps for predictable data-table models */
        if( do_loofwe )
          for( mm=0 ; mm < nmod ; mm++ ){
            char lab[160] ;
            if( !rsa_model_has_loo(mod+mm) ) continue ;
            EDIT_substitute_brick( oset , fslot , MRI_float , NULL ) ;
            THD_roilist_paint( DSET_ARRAY(oset,fslot) , rl , lzf[mm] ) ;
            sprintf(lab,"%.130s_looZFWE",mod[mm].name) ;
            EDIT_BRICK_LABEL(oset,fslot,lab) ;
            EDIT_BRICK_TO_FIZT(oset,fslot) ;
            EDIT_BRICK_FACTOR(oset,fslot,0.0) ;
            fslot++ ;
          }
      }

      /* Model-contrast maps.  Classic RSA exposes both descriptive raw-r and
         inferential Fisher-z differences; IS-RSA's direct r difference is one
         effect brick.  Each is followed by its signed test-z map. */
      if( ncon > 0 ){
        int cc , cslot = 2*nmod + 2*nloo + (do_fwe ? nmod : 0)
                                         + (do_loofwe ? nloo : 0) ;
        for( cc=0 ; cc < ncon ; cc++ ){
          char lab[200] ;
          if( rdm_over==RDM_BRICK ){
            EDIT_substitute_brick(oset,cslot,MRI_float,NULL) ;
            EDIT_substitute_brick(oset,cslot+1,MRI_float,NULL) ;
            EDIT_substitute_brick(oset,cslot+2,MRI_float,NULL) ;
            THD_roilist_paint(DSET_ARRAY(oset,cslot),rl,crd[cc]) ;
            THD_roilist_paint(DSET_ARRAY(oset,cslot+1),rl,cd[cc]) ;
            THD_roilist_paint(DSET_ARRAY(oset,cslot+2),rl,cz[cc]) ;
            sprintf(lab,"%.148s_rDiff",con[cc].name) ; EDIT_BRICK_LABEL(oset,cslot,lab) ;
            sprintf(lab,"%.148s_zDiff",con[cc].name) ; EDIT_BRICK_LABEL(oset,cslot+1,lab) ;
            sprintf(lab,"%.147s_Zstat",con[cc].name) ; EDIT_BRICK_LABEL(oset,cslot+2,lab) ;
            EDIT_BRICK_TO_FIZT(oset,cslot+2) ;
            EDIT_BRICK_FACTOR(oset,cslot,0.0) ; EDIT_BRICK_FACTOR(oset,cslot+1,0.0) ;
            EDIT_BRICK_FACTOR(oset,cslot+2,0.0) ; cslot += 3 ;
          } else {
            EDIT_substitute_brick(oset,cslot,MRI_float,NULL) ;
            EDIT_substitute_brick(oset,cslot+1,MRI_float,NULL) ;
            THD_roilist_paint(DSET_ARRAY(oset,cslot),rl,cd[cc]) ;
            THD_roilist_paint(DSET_ARRAY(oset,cslot+1),rl,cz[cc]) ;
            sprintf(lab,"%.150s_diff",con[cc].name) ; EDIT_BRICK_LABEL(oset,cslot,lab) ;
            sprintf(lab,"%.150s_%sdiff",con[cc].name,z_is_fizt?"Z":"FZ") ;
            EDIT_BRICK_LABEL(oset,cslot+1,lab) ;
            if( z_is_fizt ) EDIT_BRICK_TO_FIZT(oset,cslot+1) ;
            EDIT_BRICK_FACTOR(oset,cslot,0.0) ; EDIT_BRICK_FACTOR(oset,cslot+1,0.0) ;
            cslot += 2 ;
          }
        }
        if( do_confwe )
          for( cc=0 ; cc < ncon ; cc++ ){
            char lab[200] ;
            EDIT_substitute_brick( oset , cslot , MRI_float , NULL ) ;
            THD_roilist_paint( DSET_ARRAY(oset,cslot) , rl , czf[cc] ) ;
            sprintf(lab,"%.146s_ZdiffFWE",con[cc].name) ;
            EDIT_BRICK_LABEL(oset,cslot,lab) ;
            EDIT_BRICK_TO_FIZT(oset,cslot) ;
            EDIT_BRICK_FACTOR(oset,cslot,0.0) ;
            cslot++ ;
          }
      }

      /* commonality maps: per quantity a value brick and its signed z, then the
         FWE-corrected z grouped at the end */
      if( ncomq > 0 ){
        int q , qslot = 2*nmod + 2*nloo + (do_fwe ? nmod : 0)
                                        + (do_loofwe ? nloo : 0)
                                        + nconbrick + (do_confwe ? ncon : 0) ;
        int ca_z_is_fizt = (nperm > 0) ;
        for( q=0 ; q < ncomq ; q++ ){
          char lab[220] ;
          const char *nm = comlab[q] ;
          EDIT_substitute_brick( oset , qslot   , MRI_float , NULL ) ;
          EDIT_substitute_brick( oset , qslot+1 , MRI_float , NULL ) ;
          THD_roilist_paint( DSET_ARRAY(oset,qslot  ) , rl , cav[q] ) ;
          THD_roilist_paint( DSET_ARRAY(oset,qslot+1) , rl , caz[q] ) ;
          snprintf(lab,sizeof(lab),"%.200s",nm) ;
          EDIT_BRICK_LABEL(oset,qslot,lab) ;
          snprintf(lab,sizeof(lab),"%.196s_%s",nm,ca_z_is_fizt?"Z":"FZ") ;
          EDIT_BRICK_LABEL(oset,qslot+1,lab) ;
          if( ca_z_is_fizt ) EDIT_BRICK_TO_FIZT(oset,qslot+1) ;
          EDIT_BRICK_FACTOR(oset,qslot  ,0.0) ;
          EDIT_BRICK_FACTOR(oset,qslot+1,0.0) ;
          qslot += 2 ;
        }
        if( do_cafwe )
          for( q=0 ; q < ncomq ; q++ ){
            char lab[220] ;
            const char *nm = comlab[q] ;
            EDIT_substitute_brick( oset , qslot , MRI_float , NULL ) ;
            THD_roilist_paint( DSET_ARRAY(oset,qslot) , rl , cazf[q] ) ;
            snprintf(lab,sizeof(lab),"%.192s_ZFWE",nm) ;
            EDIT_BRICK_LABEL(oset,qslot,lab) ;
            EDIT_BRICK_TO_FIZT(oset,qslot) ;
            EDIT_BRICK_FACTOR(oset,qslot,0.0) ;
            qslot++ ;
          }
      }

      /* Noise-ceiling maps follow the inferential/commonality maps.  These are
         correlations, not test statistics, so they are plain effect bricks
         (no FIZT typing).  IS-RSA
         has one 'reliability' map (split-half geometry stability); classic RSA
         has the Nili lower and upper bounds. */
      if( do_nc ){
        int nslot = 2*nmod + 2*nloo + (do_fwe ? nmod : 0)
                                    + (do_loofwe ? nloo : 0)
                                    + nconbrick + (do_confwe ? ncon : 0)
                                    + 2*ncomq + (do_cafwe ? ncomq : 0) ;
        if( rdm_over == RDM_SUBJ ){
          EDIT_substitute_brick( oset , nslot , MRI_float , NULL ) ;
          THD_roilist_paint( DSET_ARRAY(oset,nslot) , rl , ncA ) ;
          EDIT_BRICK_LABEL( oset , nslot , "reliability" ) ;
          EDIT_BRICK_FACTOR( oset , nslot , 0.0 ) ;
        } else {
          EDIT_substitute_brick( oset , nslot   , MRI_float , NULL ) ;
          EDIT_substitute_brick( oset , nslot+1 , MRI_float , NULL ) ;
          THD_roilist_paint( DSET_ARRAY(oset,nslot  ) , rl , ncA ) ;
          THD_roilist_paint( DSET_ARRAY(oset,nslot+1) , rl , ncB ) ;
          EDIT_BRICK_LABEL( oset , nslot   , "nc_low" ) ;
          EDIT_BRICK_LABEL( oset , nslot+1 , "nc_high" ) ;
          EDIT_BRICK_FACTOR( oset , nslot   , 0.0 ) ;
          EDIT_BRICK_FACTOR( oset , nslot+1 , 0.0 ) ;
        }
      }

      /* Bootstrap bounds are appended after all inferential/diagnostic maps.
         They are ordinary effect-size bounds, never FIZT-typed statistics. */
      if( nboot > 0 ){
        int bs=base_nbrik ;
        for( mm=0 ; mm < nmod ; mm++ ){
          char lab[180] ;
          EDIT_substitute_brick( oset , bs   , MRI_float , NULL ) ;
          EDIT_substitute_brick( oset , bs+1 , MRI_float , NULL ) ;
          THD_roilist_paint( DSET_ARRAY(oset,bs  ) , rl , blo[mm] ) ;
          THD_roilist_paint( DSET_ARRAY(oset,bs+1) , rl , bhi[mm] ) ;
          sprintf(lab,dualboot?"%.137s_dualLo":"%.137s_bootLo",mod[mm].name) ;
          EDIT_BRICK_LABEL(oset,bs,lab) ;
          sprintf(lab,dualboot?"%.137s_dualHi":"%.137s_bootHi",mod[mm].name) ;
          EDIT_BRICK_LABEL(oset,bs+1,lab) ;
          EDIT_BRICK_FACTOR(oset,bs,0.0) ; EDIT_BRICK_FACTOR(oset,bs+1,0.0) ;
          bs += 2 ;
        }
        { int cc ;
          for( cc=0 ; cc < ncon ; cc++ ){
            char lab[200] ;
            EDIT_substitute_brick( oset , bs   , MRI_float , NULL ) ;
            EDIT_substitute_brick( oset , bs+1 , MRI_float , NULL ) ;
            THD_roilist_paint( DSET_ARRAY(oset,bs  ) , rl , dblo[cc] ) ;
            THD_roilist_paint( DSET_ARRAY(oset,bs+1) , rl , dbhi[cc] ) ;
            sprintf(lab,(rdm_over==RDM_BRICK)
                          ? (dualboot?"%.151s_zDiff_dualLo":"%.151s_zDiff_bootLo")
                          : (dualboot?"%.157s_dualLo":"%.157s_bootLo"),con[cc].name) ;
            EDIT_BRICK_LABEL(oset,bs,lab) ;
            sprintf(lab,(rdm_over==RDM_BRICK)
                          ? (dualboot?"%.151s_zDiff_dualHi":"%.151s_zDiff_bootHi")
                          : (dualboot?"%.157s_dualHi":"%.157s_bootHi"),con[cc].name) ;
            EDIT_BRICK_LABEL(oset,bs+1,lab) ;
            EDIT_BRICK_FACTOR(oset,bs,0.0) ; EDIT_BRICK_FACTOR(oset,bs+1,0.0) ;
            bs += 2 ;
          }
        }
        { int q ;
          for( q=0 ; q < ncomq ; q++ ){
            char lab[240] ;
            const char *nm=comlab[q] ;
            EDIT_substitute_brick( oset , bs   , MRI_float , NULL ) ;
            EDIT_substitute_brick( oset , bs+1 , MRI_float , NULL ) ;
            THD_roilist_paint( DSET_ARRAY(oset,bs  ) , rl , calo[q] ) ;
            THD_roilist_paint( DSET_ARRAY(oset,bs+1) , rl , cahi[q] ) ;
            snprintf(lab,sizeof(lab),"%.216s_bootLo",nm) ;
            EDIT_BRICK_LABEL(oset,bs,lab) ;
            snprintf(lab,sizeof(lab),"%.216s_bootHi",nm) ;
            EDIT_BRICK_LABEL(oset,bs+1,lab) ;
            EDIT_BRICK_FACTOR(oset,bs,0.0) ; EDIT_BRICK_FACTOR(oset,bs+1,0.0) ;
            bs += 2 ;
          }
        }
        if( do_loo ) for( mm=0 ; mm<nmod ; mm++ ){
          char lab[180] ;
          if( !rsa_model_has_loo(mod+mm) ) continue ;
          EDIT_substitute_brick(oset,bs,MRI_float,NULL) ;
          EDIT_substitute_brick(oset,bs+1,MRI_float,NULL) ;
          THD_roilist_paint(DSET_ARRAY(oset,bs),rl,lblo[mm]) ;
          THD_roilist_paint(DSET_ARRAY(oset,bs+1),rl,lbhi[mm]) ;
          sprintf(lab,"%.132s_looBootLo",mod[mm].name) ; EDIT_BRICK_LABEL(oset,bs,lab) ;
          sprintf(lab,"%.132s_looBootHi",mod[mm].name) ; EDIT_BRICK_LABEL(oset,bs+1,lab) ;
          EDIT_BRICK_FACTOR(oset,bs,0.0) ; EDIT_BRICK_FACTOR(oset,bs+1,0.0) ;
          bs+=2 ;
        }
      }

      if( ncboot > 0 && !dualboot ){
        int bs=base_nbrik + ((nboot > 0) ? 2*(nmod+ncon+ncomq+nloo) : 0) ;
        for( mm=0 ; mm < nmod ; mm++ ){
          char lab[180] ;
          EDIT_substitute_brick( oset , bs   , MRI_float , NULL ) ;
          EDIT_substitute_brick( oset , bs+1 , MRI_float , NULL ) ;
          THD_roilist_paint( DSET_ARRAY(oset,bs  ) , rl , cblo[mm] ) ;
          THD_roilist_paint( DSET_ARRAY(oset,bs+1) , rl , cbhi[mm] ) ;
          sprintf(lab,"%.136s_cbootLo",mod[mm].name) ; EDIT_BRICK_LABEL(oset,bs,lab) ;
          sprintf(lab,"%.136s_cbootHi",mod[mm].name) ; EDIT_BRICK_LABEL(oset,bs+1,lab) ;
          EDIT_BRICK_FACTOR(oset,bs,0.0) ; EDIT_BRICK_FACTOR(oset,bs+1,0.0) ;
          bs += 2 ;
        }
      }

      /* F7 fitted-model effects and diagnostics are appended, preserving every
         established brick offset above.  Weights are fold-mean L1-normalized
         component weights and are descriptive rather than test statistics. */
      if( nfit>0 ){
        int fi,cc,bs=nbrik-(2*nfit+(do_fitfwe?nfit:0)+nfitw
                            +2*nfitcon+(do_fitconfwe?nfitcon:0)) ;
        for( fi=0 ; fi<nfit ; fi++ ){
          char lab[220] ;
          EDIT_substitute_brick(oset,bs,MRI_float,NULL) ;
          EDIT_substitute_brick(oset,bs+1,MRI_float,NULL) ;
          THD_roilist_paint(DSET_ARRAY(oset,bs),rl,fr[fi]) ;
          THD_roilist_paint(DSET_ARRAY(oset,bs+1),rl,fzv[fi]) ;
          snprintf(lab,sizeof(lab),"%.190s_cvR",fit[fi].name) ; EDIT_BRICK_LABEL(oset,bs,lab) ;
          snprintf(lab,sizeof(lab),"%.188s_cv%s",fit[fi].name,(nperm>0)?"Z":"FZ") ;
          EDIT_BRICK_LABEL(oset,bs+1,lab) ;
          if( nperm>0 ) EDIT_BRICK_TO_FIZT(oset,bs+1) ;
          EDIT_BRICK_FACTOR(oset,bs,0.0) ; EDIT_BRICK_FACTOR(oset,bs+1,0.0) ; bs+=2 ;
        }
        if( do_fitfwe ) for( fi=0 ; fi<nfit ; fi++ ){
          char lab[220] ; EDIT_substitute_brick(oset,bs,MRI_float,NULL) ;
          THD_roilist_paint(DSET_ARRAY(oset,bs),rl,fzf[fi]) ;
          snprintf(lab,sizeof(lab),"%.184s_cvZFWE",fit[fi].name) ; EDIT_BRICK_LABEL(oset,bs,lab) ;
          EDIT_BRICK_TO_FIZT(oset,bs) ; EDIT_BRICK_FACTOR(oset,bs,0.0) ; bs++ ;
        }
        for( fi=0 ; fi<nfit ; fi++ ) for( cc=0 ; cc<fit[fi].ncomp ; cc++ ){
          char lab[220] ; EDIT_substitute_brick(oset,bs,MRI_float,NULL) ;
          THD_roilist_paint(DSET_ARRAY(oset,bs),rl,fwgt[fit[fi].wbase+cc]) ;
          snprintf(lab,sizeof(lab),"%.80s_w_%.100s",fit[fi].name,
                   mod[fit[fi].comp[cc]].name) ; EDIT_BRICK_LABEL(oset,bs,lab) ;
          EDIT_BRICK_FACTOR(oset,bs,0.0) ; bs++ ;
        }
        /* F14 maps follow the F7 fits and descriptive weights.  The effect is
           in held-fold Fisher-z accuracy units; only permutation-derived z
           bricks receive AFNI's FIZT statistical code. */
        for( cc=0 ; cc<nfitcon ; cc++ ){
          char lab[220] ;
          EDIT_substitute_brick(oset,bs,MRI_float,NULL) ;
          EDIT_substitute_brick(oset,bs+1,MRI_float,NULL) ;
          THD_roilist_paint(DSET_ARRAY(oset,bs),rl,fcd[cc]) ;
          THD_roilist_paint(DSET_ARRAY(oset,bs+1),rl,fcz[cc]) ;
          snprintf(lab,sizeof(lab),"%.185s_cvDiff",fcon[cc].name) ;
          EDIT_BRICK_LABEL(oset,bs,lab) ;
          snprintf(lab,sizeof(lab),"%.181s_cv%sdiff",fcon[cc].name,
                   (nperm>0)?"Z":"FZ") ;
          EDIT_BRICK_LABEL(oset,bs+1,lab) ;
          if( nperm>0 ) EDIT_BRICK_TO_FIZT(oset,bs+1) ;
          EDIT_BRICK_FACTOR(oset,bs,0.0) ; EDIT_BRICK_FACTOR(oset,bs+1,0.0) ; bs+=2 ;
        }
        if( do_fitconfwe ) for( cc=0 ; cc<nfitcon ; cc++ ){
          char lab[220] ; EDIT_substitute_brick(oset,bs,MRI_float,NULL) ;
          THD_roilist_paint(DSET_ARRAY(oset,bs),rl,fczf[cc]) ;
          snprintf(lab,sizeof(lab),"%.177s_cvZdiffFWE",fcon[cc].name) ;
          EDIT_BRICK_LABEL(oset,bs,lab) ; EDIT_BRICK_TO_FIZT(oset,bs) ;
          EDIT_BRICK_FACTOR(oset,bs,0.0) ; bs++ ;
        }
      }

      THD_copy_labeltable_atr( oset->dblk , mset->dblk ) ;

      /* Preserve the surface node index so a sparse-node result maps back onto
         the right nodes -- EDIT_empty_copy drops it.  We copy ONLY the node
         list, deliberately NOT THD_copy_datablock_auxdata(), which would delete
         our brick labels and FIZT stat codes and overwrite them from the mask. */
      if( ( DBLK_IS_NI_SURF_DSET(mset->dblk) || DBLK_IS_GIFTI(mset->dblk) ) &&
          mset->dblk->nnodes > 0 && mset->dblk->node_list != NULL ){
        int nn = mset->dblk->nnodes ;
        oset->dblk->node_list = (int *)RwcMalloc( sizeof(int)*nn ) ;
        oset->dblk->nnodes    = nn ;
        memcpy( oset->dblk->node_list , mset->dblk->node_list , sizeof(int)*nn ) ;
      }

      tross_Copy_History( mset , oset ) ;
      tross_Make_History( "3dRSA" , argc , argv , oset ) ;

      DSET_write(oset) ;
      if( !quiet ) INFO_message("3dRSA: wrote dataset %s",DSET_BRIKNAME(oset)) ;
   }

   /*================== suggest 1dplot.py commands ==================*/

   if( !quiet && series_file == NULL && !run_resolved ){
      int gsize = (regout ? 4 : 3) + (do_fwe ? 1 : 0)
                                      + ((nboot > 0) ? 2 : 0)
                                      + ((ncboot > 0 && !dualboot) ? 2 : 0) ; /* cols/model */
      char rfn[THD_MAX_NAME] , efn[THD_MAX_NAME] ;

      sprintf(rfn,"%s.rsa.1D",prefix) ;

      printf("\n"
             "++ 3dRSA: some 1dplot.py commands to visualize the output --\n") ;

      for( mm=0 ; mm < nmod ; mm++ ){
        float msum=0.0f ; int kk , col = 4 + mm*gsize ;
        for( kk=0 ; kk < nroi ; kk++ ) msum += rr[mm][kk] ;
        msum /= (nroi > 0) ? nroi : 1 ;
        sprintf(efn,"%s_%s_%s.1D",prefix,mod[mm].name,regout?"b":"r") ;
        printf("\n"
               "   # %s across ROIs\n"
               "   grep -v '^#' %s | awk '{print $%d}' > %s\n"
               "   1dplot.py -histogram -vline 0 -vline_hl %.4g 'mean %.3f' \\\n"
               "             -xlabel %s_%s -title '%s: %s across ROIs' \\\n"
               "             -prefix %s_hist_%s.png -infiles %s\n" ,
               mod[mm].name , rfn , col , efn ,
               msum , msum , mod[mm].name , regout?"b":"r" ,
               mod[mm].name , regout?"b":"r" ,
               prefix , mod[mm].name , efn ) ;
      }

      if( nmod > 1 ){
        char afn[THD_MAX_NAME] , bfn[THD_MAX_NAME] ;
        int cola = 4 , colb = 4 + gsize ;
        sprintf(afn,"%s_%s_%s.1D",prefix,mod[0].name,regout?"b":"r") ;
        sprintf(bfn,"%s_%s_%s.1D",prefix,mod[1].name,regout?"b":"r") ;
        printf("\n"
               "   # %s vs %s effect, across ROIs (extracted above)\n"
               "   1dplot.py -scatter -diagonal -xfile %s -xlabel %s_%s \\\n"
               "             -ylabels %s_%s -prefix %s_compare_%s_vs_%s.png \\\n"
               "             -infiles %s\n" ,
               mod[0].name , mod[1].name , afn , mod[0].name , regout?"b":"r" ,
               mod[1].name , regout?"b":"r" , prefix , mod[0].name , mod[1].name ,
               bfn ) ;
        (void)cola ; (void)colb ; /* already encoded via the per-model extraction above */
      }

      if( save_rdm != NULL ){
        for( mm=0 ; mm < nmod ; mm++ ){
          char mfn[THD_MAX_NAME] ;
          if( mod[mm].mat == NULL ){
            printf("\n"
                   "   # model RDM: %s varies by ROI; no single model RDM file was written\n",
                   mod[mm].name ) ;
            continue ;
          }
          sprintf(mfn,"%s_model_%s.1D",save_rdm,mod[mm].name) ;
          printf("\n"
                 "   # model RDM: %s\n"
                 "   1dplot.py -heat -title 'model: %s' -prefix %s_model_%s.png \\\n"
                 "             -infiles %s\n" ,
                 mod[mm].name , mod[mm].name , prefix , mod[mm].name , mfn ) ;
        }
        {
          char nfn[THD_MAX_NAME] ;
          sprintf(nfn,"%s_roi%04d.1D",save_rdm,rl->val[0]) ;
          printf("\n"
                 "   # neural RDM, ROI %d (repeat for other ROIs' %s_roi####.1D files)\n"
                 "   1dplot.py -heat -zerocenter -cbar Reds_and_Blues_Inv \\\n"
                 "             -title 'neural RDM, ROI %d' -prefix %s_neural_roi%04d.png \\\n"
                 "             -infiles %s\n" ,
                 rl->val[0] , save_rdm , rl->val[0] , prefix , rl->val[0] , nfn ) ;
        }
      } else {
        printf("\n"
               "   (add '-save_rdm QQQ' to also get commands for the RDM heatmaps)\n") ;
      }
      printf("\n") ;
   } else if( !quiet && run_resolved ){
      printf("\n++ 3dRSA: run-resolved output is in long ROI x summary x model form in\n"
             "   %s.rsa.1D; MEAN is an equal-run mean of signed %s%s.\n\n",
             prefix,joint?"conditional standardized coefficients":"association statistics",
             nrunconspec?", and named rows are fixed-run planned contrasts":"") ;
   } else if( !quiet ){
      printf("\n"
             "++ 3dRSA: -model_series output is in long time x ROI form in\n"
             "   %s.rsa.1D; dataset bricks use t#### labels mapped to the\n"
             "   verbatim time labels in that table.\n\n",prefix) ;
   }

   THD_mantel_cache_free(mcache) ; free(mcache_ix) ;
   free(loo_owner) ; free(loo_fam) ;
   free(block_lab) ;
   if( pset != NULL ) THD_perm_set_free(pset) ;
   if( cpset != NULL ) THD_perm_set_free(cpset) ;
   if( tset != NULL ) THD_timeshift_set_free(tset) ;
   if( phset != NULL ) THD_phase_set_free(phset) ;
   free(tsneed) ;
   if( rset != NULL ) THD_resample_set_free(rset) ;
   if( contrast_rset != NULL ) THD_resample_set_free(contrast_rset) ;
   if( fit_contrast_rset != NULL ) THD_resample_set_free(fit_contrast_rset) ;
   if( crset != NULL ) rsa_cond_resample_free(crset) ;
   rsa_condfold_free(fit_condfold) ;
   free(seed_srdm) ;
   if( run_rr!=NULL ){
     int nrout=nmod*series_runs->nrun ;
     for( ii=0 ; ii<nrout ; ii++ ){
       free(run_rr[ii]); free(run_ee[ii]); free(run_pp[ii]); free(run_qq[ii]); free(run_zz[ii]) ;
       if( run_pf!=NULL ){ free(run_pf[ii]); free(run_zf[ii]); }
     }
     free(run_rr); free(run_ee); free(run_pp); free(run_qq); free(run_zz); free(run_pf); free(run_zf) ;
   }
   if( rcon_rr!=NULL ){
     int nrc=nmod*nrunconspec ;
     for( ii=0 ; ii<nrc ; ii++ ){
       free(rcon_rr[ii]); free(rcon_ee[ii]); free(rcon_pp[ii]); free(rcon_qq[ii]); free(rcon_zz[ii]) ;
       if( rcon_pf!=NULL ){ free(rcon_pf[ii]); free(rcon_zf[ii]); }
     }
     free(rcon_rr); free(rcon_ee); free(rcon_pp); free(rcon_qq); free(rcon_zz); free(rcon_pf); free(rcon_zf) ;
   }
   free(run_mxflat) ;
   if( mod!=NULL && series_runs!=NULL ) for( mm=0 ; mm<nmod ; mm++ )
     if( mod[mm].run_mat!=NULL ){
       for( ii=0 ; ii<series_runs->nrun ; ii++ ) THD_simmat_free(mod[mm].run_mat[ii]) ;
       free(mod[mm].run_mat) ;
     }
   for( ii=0 ; ii<nrunconspec ; ii++ ) free(runcon[ii].weight) ;
   for( ii=0 ; ii<nrunfactorspec ; ii++ ){
     int lv ; free(runfactor[ii].column); free(runfactor[ii].run_level) ;
     for( lv=0 ; lv<runfactor[ii].nlevel ; lv++ ) free(runfactor[ii].level[lv]) ;
     free(runfactor[ii].level) ;
   }
   free(runcon); free(runfactor); free(rcon_weight) ;
   rsa_series_runs_free(series_runs) ;
   THD_free_datatable_index(condition_index) ;
   for( ii=0 ; ii<ncondition_level ; ii++ ) free(condition_level[ii]) ;
   free(condition_level) ;
   THD_free_datatable(tab) ;
   THD_free_datatable(longtab) ;
   if( !quiet && progress_mode!=RSA_PROGRESS_OFF ){
     char tbuf[32] ;
     rsa_duration(0.001*(double)NI_clock_time()-program_start,tbuf,sizeof(tbuf)) ;
     INFO_message("3dRSA completed in %s",tbuf) ;
   }
   exit(0) ;
}
