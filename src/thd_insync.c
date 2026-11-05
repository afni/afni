/* Dataset loading, inference, and output for the 3dInSync command. */
#include "mrilib.h"
#include "thd_datatable.h"
#include "thd_insync.h"
#include "thd_insync_select.h"
#include "thd_insync_model.h"
#include "thd_insync_infer.h"
#include "thd_mapinfer.h"
#include "thd_patterns.h"
#include "thd_phasefft.h"
#include "thd_permute.h"
#include "thd_simmatrix.h"

#include <float.h>
#include <limits.h>
#include <stdint.h>
#include <unistd.h>

#ifdef USE_OMP
#include <omp.h>
#endif

#define PROGRAM_NAME "3dInSync"

/*! Keep command-line defaults in the same module as the analysis contract. */
void INSYNC_options_default( INSYNC_options *opts )
{
   *opts=(INSYNC_options){
     .method=INSYNC_METHOD_PAIRWISE,
     .summary=INSYNC_SUMMARY_MEDIAN,
     .show_table=0,
     .corr_metric=SIM_PEARSON,
     .missing_policy=INSYNC_MISSING_ERROR,
     .quiet=0,
     .progress_mode=INSYNC_PROGRESS_AUTO,
     .memory_override=0,
     .memory_limit_given=0,
     .memory_limit_gib=0.0,
     .step=1,
     .min_valid=3,
     .block_summary=INSYNC_SUMMARY_MEDIAN,
     .min_blocks=1,
     .nboot=0,
     .nperm=0,
     .do_exact=0,
     .tail=PERM_TAIL_TWO,
     .temporal_mode=INSYNC_TNULL_NONE,
     .temporal_censor=0,
     .nnull=0,
     .min_shift=1,
     .min_shift_given=0,
     .temporal_tail=PERM_TAIL_ONE,
     .temporal_tail_given=0,
     .ncondperm=0,
     .condition_exact=0,
     .condition_tail=PERM_TAIL_TWO,
     .seed=1234567L,
     .boot_alpha=0.05f,
     .condition_column_given=0,
     .have_contrast=0,
     .prefix="InSync",
     .maskname=NULL,
     .group_column="Group",
     .condition_column="Condition",
     .contrast_name0=NULL,
     .contrast_name1=NULL,
     .pair_prefix=NULL,
     .censor_name=NULL,
     .censor_column="Censor",
     .do_zcensor=0,
     .zcensor_warn_given=0,
     .zcensor_warn=20.0f,
     .atlas_name=NULL,
     .roi_sel=NULL,
     .matrix_name=NULL,
     .roi_only=0,
     .use_half=0,
     .eo={ .event_frac=0.10f, .min_shift=1 }
   } ;
}

/*! Append one newly encountered group label to the owned label array.
    The caller is responsible for eventually freeing the duplicated string. */
static void insync_add_group( char *label, char ***labels, int *nlabel )
{
   *labels=(char **)realloc(*labels,sizeof(char *)*(*nlabel+1)) ;
   (*labels)[*nlabel]=strdup(label) ; (*nlabel)++ ;
}

/*! Return the zero-based index of label, or -1 when it has not been seen. */
static int insync_find_group( char *label, char **labels, int nlabel )
{
   int ii ;
   for( ii=0 ; ii<nlabel ; ii++ ) if( strcmp(label,labels[ii])==0 ) return ii ;
   return -1 ;
}

/*! Build an AFNI-safe sub-brick label from a statistic stem and effect label. */
static void insync_brick_label( char *dst, size_t ndst, const char *stem, const char *label )
{
   size_t ii,jj=0 ; char tmp[128] ;
   snprintf(tmp,sizeof(tmp),"%s_%s",stem,label) ;
   for( ii=0 ; tmp[ii]!='\0' && jj+1<ndst ; ii++ ){
     unsigned char cc=(unsigned char)tmp[ii] ;
     dst[jj++]=(isalnum(cc) || cc=='_' || cc=='-' || cc=='.') ? (char)cc : '_' ;
   }
   dst[jj]='\0' ;
}

/*! Copy src into a whitespace-free token suitable for labels and filenames. */
static void insync_safe_token( char *dst, size_t ndst, const char *src )
{
   size_t ii,jj=0 ;
   if( ndst<1 ) return ;
   for( ii=0 ; src!=NULL && src[ii]!='\0' && jj+1<ndst ; ii++ ){
     unsigned char cc=(unsigned char)src[ii] ;
     dst[jj++]=(isalnum(cc) || cc=='_' || cc=='-' || cc=='.') ? (char)cc : '_' ;
   }
   dst[jj]='\0' ;
}

/*! Convert a correlation to Fisher z after clipping exact endpoint values. */
static float insync_fisher_z( float rr )
{
   if( !isfinite(rr) ) return 0.0f ;
   if( rr>=1.0f ) rr=0.999999f ; else if( rr<=-1.0f ) rr=-0.999999f ;
   return atanhf(rr) ;
}

/*! Write the 3dISC-compatible pair-table header, including user covariates. */
static void insync_3disc_header( FILE *fp, THD_datatable *tab,
                                 int icol_group, int icol_condition, int icol_censor )
{
   int jj ;
   fprintf(fp,"Subj1 Subj2 Group1 Group2 GroupPair cond cond1 cond2") ;
   for( jj=0 ; jj<tab->ncol ; jj++ )
     if( jj!=tab->icol_subj && jj!=tab->icol_input &&
         jj!=icol_group && jj!=icol_condition && jj!=icol_censor )
       fprintf(fp," %s1 %s2",tab->cname[jj],tab->cname[jj]) ;
   fprintf(fp," InputFile\n") ;
}

/*! Write one subject-pair/condition-pair row pointing at a pair-map brick.
    Subject-level covariates are repeated with 1/2 suffixes for 3dISC. */
static void insync_3disc_row( FILE *fp, THD_datatable *tab,
                              THD_datatable_index *cindex,
                              int *rowmap, int ncond, char **clabel,
                              int *group, char **glabel,
                              int c1, int c2, int s1, int s2,
                              int icol_group, int icol_censor, const char *dataset_name,
                              size_t brick )
{
   const char *a=cindex?cindex->level[0][s1]:tab->subj[s1] ;
   const char *b=cindex?cindex->level[0][s2]:tab->subj[s2] ;
   int ra=rowmap[s1*ncond],rb=rowmap[s2*ncond],jj ;
   int icol_condition=cindex?cindex->icol[1]:-1 ;
   char gp[256],cp[256] ;
   snprintf(gp,sizeof(gp),"%s__%s",glabel[group[s1]],glabel[group[s2]]) ;
   if( c1<=c2 ) snprintf(cp,sizeof(cp),"%s__%s",clabel[c1],clabel[c2]) ;
   else         snprintf(cp,sizeof(cp),"%s__%s",clabel[c2],clabel[c1]) ;
   fprintf(fp,"%s %s %s %s %s %s %s %s",a,b,glabel[group[s1]],
           glabel[group[s2]],gp,cp,clabel[c1],clabel[c2]) ;
   for( jj=0 ; jj<tab->ncol ; jj++ )
     if( jj!=tab->icol_subj && jj!=tab->icol_input &&
         jj!=icol_group && jj!=icol_condition && jj!=icol_censor )
       fprintf(fp," %s %s",DT_CELL(tab,ra,jj),DT_CELL(tab,rb,jj)) ;
   fprintf(fp," %s[%lu]\n",dataset_name,(unsigned long)brick) ;
}

/*! Thread-safe progress state shared by the OpenMP voxel workers. */
typedef struct {
   int enabled,mode,is_tty,last_bucket ;
   long long total,done;
   const char *phase,*unit;
   double start,last_report ;
} INSYNC_progress ;

/*! Initialize progress rendering and choose bar versus line output for auto. */
static void insync_progress_init( INSYNC_progress *p, int mode, int quiet,
                                  long long total,const char *phase,const char *unit )
{
   memset(p,0,sizeof(*p)) ; p->enabled=!quiet && mode!=INSYNC_PROGRESS_OFF && total>0 ;
   p->is_tty=isatty(fileno(stderr)) ; p->total=total ; p->phase=phase; p->unit=unit;
   p->mode=(mode==INSYNC_PROGRESS_AUTO)?(p->is_tty?INSYNC_PROGRESS_BAR:INSYNC_PROGRESS_LINE):mode ;
   p->start=p->last_report=0.001*(double)NI_clock_time() ;
   if( !p->enabled ) return ;
   if( p->mode==INSYNC_PROGRESS_BAR ){
     fprintf(stderr,"++ 3dInSync [--------------------] 0/%lld",total) ;
     fflush(stderr) ;
   } else INFO_message(PROGRAM_NAME ": %s 0/%lld %s complete",phase,total,unit) ;
}

/*! Atomically count a completed voxel and occasionally render an update. */
static void insync_progress_advance( INSYNC_progress *p )
{
   long long done; int bucket ; double now,elapsed,rate ;
   if( !p->enabled ) return ;
#ifdef USE_OMP
#pragma omp atomic capture
   done=++p->done ;
#else
   done=++p->done ;
#endif
#ifdef USE_OMP
#pragma omp critical(insync_progress_render)
#endif
   {
     now=0.001*(double)NI_clock_time() ; bucket=(10*done)/p->total ;
     if( done==p->total || (p->mode==INSYNC_PROGRESS_BAR && now-p->last_report>=1.0) ||
         (p->mode==INSYNC_PROGRESS_LINE && bucket>p->last_bucket) ){
       int ii,fill=(20*done)/p->total ;
       elapsed=now-p->start ; rate=(elapsed>0.0)?done/elapsed:0.0 ;
       if( p->mode==INSYNC_PROGRESS_BAR ){
         fprintf(stderr,"\r++ 3dInSync [") ;
         for( ii=0 ; ii<20 ; ii++ ) fputc(ii<fill?'#':'-',stderr) ;
         fprintf(stderr,"] %lld/%lld (%.1f%%), %.2f %s/s",done,p->total,
                 100.0*done/p->total,rate,p->unit) ;
         if( p->is_tty ) fputs("\033[K",stderr) ;
         if( done==p->total ) fputc('\n',stderr) ;
         fflush(stderr) ;
       } else INFO_message(PROGRAM_NAME ": %s %lld/%lld %s complete "
                           "(%.1f%%, %.2f %s/s)",p->phase,done,p->total,p->unit,
                           100.0*done/p->total,rate,p->unit) ;
       p->last_report=now ; p->last_bucket=bucket ;
     }
   }
}

/*---------------------------------------------------------------------------*/
/* Compact in-memory subject store.  The subject datasets are by far the
   largest allocation.  Rather than keeping every dataset resident on the full
   grid, a store holds only the work units the analysis needs: the in-mask
   voxels, or one mean time series per atlas ROI (-roi_only), optionally in
   16-bit floating point (-float16).  Each subject/condition array is
   voxel-major, nwork x ntime_input, so one voxel's series is contiguous. */

/*! IEEE-754 binary16 from float, round to nearest even.  Overflow becomes
    infinity; NaN and infinity are preserved so nonfinite input is still seen. */
static unsigned short insync_f2h( float f )
{
   union { float f ; unsigned u ; } v ;
   unsigned x, sign, m, hm, rem, h ;
   int e, ne ;
   v.f=f ; x=v.u ; sign=(x>>16)&0x8000u ; e=(int)((x>>23)&0xffu) ; m=x&0x7fffffu ;
   if( e==0xff ) return (unsigned short)(sign|0x7c00u|(m?0x200u:0u)) ;
   ne=e-127+15 ;
   if( ne>=0x1f ) return (unsigned short)(sign|0x7c00u) ;
   if( ne<=0 ){                                   /* subnormal half, or zero */
     int shift ; unsigned halfway ;
     if( ne<-10 ) return (unsigned short)sign ;
     m|=0x800000u ; shift=14-ne ;
     hm=m>>shift ; rem=m&((1u<<shift)-1u) ; halfway=1u<<(shift-1) ;
     if( rem>halfway || (rem==halfway && (hm&1u)) ) hm++ ;
     return (unsigned short)(sign|hm) ;
   }
   hm=m>>13 ; rem=m&0x1fffu ;
   h=sign|((unsigned)ne<<10)|hm ;
   if( rem>0x1000u || (rem==0x1000u && (hm&1u)) ) h++ ;  /* carry may bump exponent */
   return (unsigned short)h ;
}

/*! Float from IEEE-754 binary16. */
static float insync_h2f( unsigned short h )
{
   union { float f ; unsigned u ; } v ;
   unsigned sign=((unsigned)h&0x8000u)<<16, e=((unsigned)h>>10)&0x1fu, m=(unsigned)h&0x3ffu ;
   if( e==0 ){
     if( m==0 ) v.u=sign ;
     else {
       int s=0 ;
       while( !(m&0x400u) ){ m<<=1 ; s++ ; }
       m&=0x3ffu ; v.u=sign|((unsigned)(113-s)<<23)|(m<<13) ;
     }
   } else if( e==0x1f ) v.u=sign|0x7f800000u|(m<<13) ;
   else v.u=sign|((e+112u)<<23)|(m<<13) ;
   return v.f ;
}

typedef struct {
   int ndset ;                 /* subject x condition arrays                */
   int ntime_input ;           /* time points held per work unit            */
   int nwork ;                 /* voxels (or ROIs) per array                */
   int half ;                  /* 1 if stored as binary16                   */
   float **fst ;               /* [ndset] -> nwork*ntime_input floats       */
   unsigned short **hst ;      /* [ndset] -> nwork*ntime_input halves       */
   float **hpar ;              /* [ndset] -> 2*nwork: mean, SD per work unit */
   const int *vmap ;           /* grid voxel -> work unit, or -1; NULL=dense */
   THD_3dim_dataset **dset ;   /* dense mode only                           */
} INSYNC_src ;

/*! One value from the compact store, by work-unit index.  A 16-bit value is
    a z-score; its voxel's mean and SD (kept in 32 bits) restore the scale, so
    the rounding error is relative to each voxel's own variability. */
static inline float insync_store_get( const INSYNC_src *s, int dd, int w, int ot )
{
   size_t k=(size_t)w*s->ntime_input+ot ;
   if( !s->half ) return s->fst[dd][k] ;
   return insync_h2f(s->hst[dd][k])*s->hpar[dd][2*(size_t)w+1]+s->hpar[dd][2*(size_t)w] ;
}

/*! One value by full-grid voxel index, from whichever representation holds
    the data.  Voxels outside the store read as zero; callers pass in-mask
    voxels only. */
static float insync_src_get( const INSYNC_src *s, int dd, int iv, int ot )
{
   if( s->vmap!=NULL ){
     int w=s->vmap[iv] ;
     return (w<0) ? 0.0f : insync_store_get(s,dd,w,ot) ;
   }
   return THD_get_voxel(s->dset[dd],iv,ot) ;
}

/*! Spread work-unit values onto the full grid.  Voxels without a work unit
    receive outside (0, or 1 for p-value bricks, as when the analysis ran on
    the whole grid); with an ROI map every voxel of an ROI gets its ROI value. */
static float * insync_expand( const float *wv, const int *vmap, int nvox, float outside )
{
   float *full=(float *)malloc(sizeof(float)*(size_t)nvox) ;
   int gv ;
   if( full==NULL ) ERROR_exit(PROGRAM_NAME ": cannot allocate a full-grid output map") ;
   for( gv=0 ; gv<nvox ; gv++ ) full[gv]=(vmap[gv]>=0) ? wv[vmap[gv]] : outside ;
   return full ;
}

/*---------------------------------------------------------------------------*/

/*! View (+orig/+acpc/+tlrc) and template-space name of a dataset, read from
    its header only. */
static int insync_template_space( const char *name, int *view, char *space )
{
   THD_3dim_dataset *ds=THD_open_dataset((char *)name) ;
   if( ds==NULL ) return 0 ;
   *view=ds->view_type ;
   MCW_strncpy(space,ds->atlas_space,THD_MAX_NAME) ;
   DSET_delete(ds) ;
   return 1 ;
}

/*! Release the compact store. */
static void insync_free_store( INSYNC_src *s )
{
   int dd ;
   for( dd=0 ; dd<s->ndset ; dd++ ){
     if( s->fst!=NULL ) free(s->fst[dd]) ;
     if( s->hst!=NULL ) free(s->hst[dd]) ;
     if( s->hpar!=NULL ) free(s->hpar[dd]) ;
   }
   free(s->fst) ; free(s->hst) ; free(s->hpar) ; s->fst=NULL ; s->hst=NULL ; s->hpar=NULL ;
}

/*! Reduce one loaded dataset to its work units and write them into the store
    at array index dd.  rvox!=NULL builds one mean series per ROI (the first
    nonfinite ROI input under MISSING_ERROR is reported through bad_w/bad_t,
    earliest time then lowest ROI); otherwise the in-mask voxels widx[] are
    copied.  With 16-bit storage each voxel series is stored as a z-score and
    its mean and SD are kept in 32 bits, so every voxel gets the same relative
    precision whatever its variance. */
static void insync_fill_one( THD_3dim_dataset *ds, int dd, INSYNC_src *s,
                             const int *widx, int **rvox, const int *rnv,
                             int missing_policy, int *bad_w, int *bad_t )
{
   const int nti=s->ntime_input, nw=s->nwork ;
   int tt ;
   *bad_w=-1 ; *bad_t=-1 ;

   if( rvox!=NULL ){                                   /* ROI mean series */
     float *dst=(float *)malloc(sizeof(float)*(size_t)nw*nti) ;
     if( dst==NULL ) ERROR_exit(PROGRAM_NAME ": cannot allocate the ROI store") ;
     s->fst[dd]=dst ;
#ifdef USE_OMP
#pragma omp parallel for schedule(dynamic,8)
#endif
     for( tt=0 ; tt<nti ; tt++ ){
       int w ;
       for( w=0 ; w<nw ; w++ ){
         double sum=0.0 ; int bad=0, j ;
         for( j=0 ; j<rnv[w] ; j++ ){
           float x=THD_get_voxel(ds,rvox[w][j],tt) ;
           if( !isfinite(x) ){ bad=1 ; break ; }
           sum+=x ;
         }
         dst[(size_t)w*nti+tt]=bad ? NAN : (float)(sum/rnv[w]) ;
         if( bad && missing_policy==INSYNC_MISSING_ERROR ){
#ifdef USE_OMP
#pragma omp critical(insync_fill_bad)
#endif
           { if( *bad_t<0 || tt<*bad_t || (tt==*bad_t && w<*bad_w) ){ *bad_t=tt ; *bad_w=w ; } }
         }
       }
     }
     return ;
   }

   if( !s->half ){                                     /* in-mask, float32 */
     float *dst=(float *)malloc(sizeof(float)*(size_t)nw*nti) ;
     if( dst==NULL ) ERROR_exit(PROGRAM_NAME ": cannot allocate the voxel store") ;
     s->fst[dd]=dst ;
#ifdef USE_OMP
#pragma omp parallel for schedule(static)
#endif
     for( tt=0 ; tt<nti ; tt++ ){
       int w ;
       for( w=0 ; w<nw ; w++ ) dst[(size_t)w*nti+tt]=THD_get_voxel(ds,widx[w],tt) ;
     }
     return ;
   }

   {                                                   /* in-mask, binary16 */
     float *tmp=(float *)malloc(sizeof(float)*(size_t)nw*nti) ;
     unsigned short *hd=(unsigned short *)malloc(sizeof(unsigned short)*(size_t)nw*nti) ;
     float *par=(float *)malloc(sizeof(float)*2*(size_t)nw) ;
     int bad16=0,w ;
     if( tmp==NULL || hd==NULL || par==NULL ) ERROR_exit(PROGRAM_NAME ": cannot allocate the 16-bit voxel store") ;
#ifdef USE_OMP
#pragma omp parallel for schedule(static)
#endif
     for( tt=0 ; tt<nti ; tt++ ){
       int w ;
       for( w=0 ; w<nw ; w++ ) tmp[(size_t)w*nti+tt]=THD_get_voxel(ds,widx[w],tt) ;
     }
     /* Each voxel series becomes a z-score using its finite values; nonfinite
        entries pass through unchanged so they are still seen downstream.  A
        constant series stores zeros with unit scale. */
#ifdef USE_OMP
#pragma omp parallel for schedule(static) reduction(+:bad16)
#endif
     for( w=0 ; w<nw ; w++ ){
       float *row=tmp+(size_t)w*nti ;
       unsigned short *hrow=hd+(size_t)w*nti ;
       double m=0.0, ss=0.0, sd ; int nf=0, t2 ;
       for( t2=0 ; t2<nti ; t2++ ) if( isfinite(row[t2]) ){ m+=row[t2] ; nf++ ; }
       if( nf>0 ) m/=nf ;
       for( t2=0 ; t2<nti ; t2++ ) if( isfinite(row[t2]) ){ double y=(double)row[t2]-m ; ss+=y*y ; }
       sd=(nf>1) ? sqrt(ss/nf) : 0.0 ;
       if( !(sd>0.0) ) sd=1.0 ;
       par[2*(size_t)w]=(float)m ; par[2*(size_t)w+1]=(float)sd ;
       for( t2=0 ; t2<nti ; t2++ ){
         float x=row[t2], z=isfinite(x) ? (float)(((double)x-m)/sd) : x ;
         hrow[t2]=insync_f2h(z) ;
         if( isfinite(x) && !isfinite(insync_h2f(hrow[t2])) ) bad16++ ;
       }
     }
     free(tmp) ;
     if( bad16>0 )
       ERROR_exit(PROGRAM_NAME ": -float16 cannot represent %d values of this dataset; "
                  "omit -float16",bad16) ;
     s->hst[dd]=hd ; s->hpar[dd]=par ;
   }
}

/*! Average input data within each selected atlas parcel and write descriptive
    group ISC summaries plus, when requested, subject correlation matrices.
    Censoring, missing-data policy, correlation metric, and mask intersection
    are kept identical to the voxelwise analysis. */
static void insync_write_roi_outputs(
   const char *atlas_name, const char *roi_sel, const char *prefix,
   const char *matrix_name, THD_3dim_dataset *first,
   const INSYNC_src *src, int nsub, int ncond,
   int ntime_input, int ntime, int *time_index, byte *mask,
   int missing_policy, int corr_metric, int method, int summary,
   int ngroup, int *group, int *gcount, int **gmember, char **glabel,
   THD_datatable *tab, THD_datatable_index *cindex, int *rowmap,
   char **clabel, int quiet )
{
   THD_3dim_dataset *aset ; THD_roilist *rl ; THD_simmat *sm ;
   FILE *sfp,*mfp=NULL ; char sname[THD_MAX_NAME] ;
   int nvox,ndset=nsub*ncond,rr,cc,ss,tt,gg,aa,bb,vv,nwritten=0 ;
   size_t nedge=(size_t)nsub*(nsub-1)/2,nscratch=(nedge>(size_t)nsub)?nedge:(size_t)nsub ;
   float *data,*ref,*values,*scratch,*rank1=NULL,*rank2=NULL ;
   byte *goodtime=(missing_policy==INSYNC_MISSING_COMMON)?(byte *)malloc((size_t)ntime):NULL ;

   aset=THD_open_dataset((char *)atlas_name) ; CHECK_OPEN_ERROR(aset,atlas_name) ;
   DSET_load(aset) ; CHECK_LOAD_ERROR(aset) ;
   if( !EQUIV_GRIDS(first,aset) )
     ERROR_exit(PROGRAM_NAME ": -atlas is not on the input grid") ;
   nvox=DSET_NVOX(aset) ;
   for( vv=0 ; vv<nvox ; vv++ ){
     float x=THD_get_voxel(aset,vv,0) ;
     if( !isfinite(x) || (x>0.0f && x!=floorf(x)) )
       ERROR_exit(PROGRAM_NAME ": atlas '%s' has invalid label %.7g at voxel %d; "
                  "positive labels must be finite integers",atlas_name,x,vv) ;
   }
   rl=THD_roilist_from_dset(aset,(char *)roi_sel) ;
   if( rl==NULL ) ERROR_exit(PROGRAM_NAME ": -atlas/-roi_sel selected no positive ROIs") ;
   snprintf(sname,sizeof(sname),"%s.roi.1D",prefix) ;
   if( !THD_ok_overwrite() && THD_is_file(sname) )
     ERROR_exit(PROGRAM_NAME ": ROI summary '%s' already exists",sname) ;
   sfp=fopen(sname,"w") ;
   if( sfp==NULL ) ERROR_exit(PROGRAM_NAME ": cannot write ROI summary '%s'",sname) ;
   if( matrix_name!=NULL ){
     if( !THD_ok_overwrite() && THD_is_file((char *)matrix_name) )
       ERROR_exit(PROGRAM_NAME ": ROI matrix '%s' already exists",matrix_name) ;
     mfp=fopen(matrix_name,"w") ;
     if( mfp==NULL ) ERROR_exit(PROGRAM_NAME ": cannot write ROI matrix '%s'",matrix_name) ;
   }
   fprintf(sfp,"Condition ROI ROI_Label Group ISC N ValidTR NVoxel Correlation Estimator Summary\n") ;
   if( mfp!=NULL )
     fprintf(mfp,"Condition ROI ROI_Label Subj1 Subj2 Group1 Group2 Correlation\n") ;

   data=(float *)malloc(sizeof(float)*(size_t)ndset*ntime) ;
   ref=(float *)malloc(sizeof(float)*(size_t)ntime) ;
   values=(float *)malloc(sizeof(float)*(size_t)nsub) ;
   scratch=(float *)malloc(sizeof(float)*nscratch) ;
   if( corr_metric==SIM_SPEARMAN ){
     rank1=(float *)malloc(sizeof(float)*(size_t)ntime) ;
     rank2=(float *)malloc(sizeof(float)*(size_t)ntime) ;
   }
   sm=THD_simmat_new(nsub) ;
   if( data==NULL || ref==NULL || values==NULL || scratch==NULL || sm==NULL ||
       (missing_policy==INSYNC_MISSING_COMMON && goodtime==NULL) ||
       (corr_metric==SIM_SPEARMAN && (rank1==NULL || rank2==NULL)) )
     ERROR_exit(PROGRAM_NAME ": cannot allocate atlas ROI buffers") ;

   /* Each ROI is reduced to one mean time series per subject and condition.
      The mask is intersected here rather than modifying the atlas labels. */
   for( rr=0 ; rr<rl->nroi ; rr++ ){
     int nrv=0,nt=ntime ; char rlab[128] ;
     for( vv=0 ; vv<rl->vox[rr].nar ; vv++ ){
       int iv=rl->vox[rr].ar[vv] ;
       if( mask==NULL || mask[iv] ) nrv++ ;
     }
     if( nrv<1 ) continue ;
     if( rl->lab!=NULL && rl->lab[rr]!=NULL )
       insync_safe_token(rlab,sizeof(rlab),rl->lab[rr]) ;
     else snprintf(rlab,sizeof(rlab),"ROI%d",rl->val[rr]) ;

     for( cc=0 ; cc<ncond ; cc++ ) for( ss=0 ; ss<nsub ; ss++ ){
       int dd=cc*nsub+ss ;
       for( tt=0 ; tt<ntime ; tt++ ){
         double sum=0.0 ; int bad=0,nv=0,ot=time_index[tt] ;
         for( vv=0 ; vv<rl->vox[rr].nar ; vv++ ){
           int iv=rl->vox[rr].ar[vv] ; float x ;
           if( mask!=NULL && !mask[iv] ) continue ;
           x=insync_src_get(src,ss*ncond+cc,iv,ot) ;
           if( !isfinite(x) ){ bad=1 ; break ; }
           sum+=x ; nv++ ;
         }
         if( bad && missing_policy==INSYNC_MISSING_ERROR )
           ERROR_exit(PROGRAM_NAME ": nonfinite atlas input for ROI %d, Subj %s, "
                      "condition %s, time %d",rl->val[rr],
                      cindex?cindex->level[0][ss]:tab->subj[ss],clabel[cc],ot+1) ;
         data[(size_t)dd*ntime+tt]=(bad || nv<1)?NAN:(float)(sum/nv) ;
       }
     }
     if( missing_policy==INSYNC_MISSING_COMMON ){
       int dd,ngood=0 ;

       /* Compress all ROI series with one shared complete-case time index;
          this prevents subjects from being correlated on different samples. */
       for( tt=0 ; tt<ntime ; tt++ ){
         int good=1 ;
         for( dd=0 ; dd<ndset ; dd++ )
           if( !isfinite(data[(size_t)dd*ntime+tt]) ){ good=0 ; break ; }
         goodtime[tt]=(byte)good ; if( good ) ngood++ ;
       }
       if( ngood>=1 ) for( dd=0 ; dd<ndset ; dd++ ){
         int pos=0 ; float *src=data+(size_t)dd*ntime,*dst=data+(size_t)dd*ngood ;
         for( tt=0 ; tt<ntime ; tt++ ) if( goodtime[tt] ) dst[pos++]=src[tt] ;
       }
       nt=ngood ;
     }

     /* Compute the same estimator used voxelwise.  Matrix export remains
        pairwise descriptive output even when the requested summary is LOO. */
     for( cc=0 ; cc<ncond ; cc++ ){
       float *block=data+(size_t)cc*nsub*nt ; int pair_ok=0 ;
       if( nt>=3 )
         pair_ok=!THD_simmat_fill_from_features(sm,nt,block,corr_metric,rank1,rank2) ;
       for( gg=0 ; gg<ngroup ; gg++ ){
         float stat=NAN ; int nv=0 ;
         if( nt>=3 && method==INSYNC_METHOD_PAIRWISE && pair_ok )
           stat=INSYNC_pairwise_indexed(nsub,sm->mat,gcount[gg],gmember[gg],
                                        summary,scratch,&nv) ;
         else if( nt>=3 && method==INSYNC_METHOD_LOO )
           stat=INSYNC_loo_indexed_metric(nsub,nt,block,gcount[gg],gmember[gg],
                  summary,corr_metric,ref,values,rank1,rank2,&nv) ;
         fprintf(sfp,"%s %d %s %s %.9g %d %d %d %s %s %s\n",
                 clabel[cc],rl->val[rr],rlab,glabel[gg],
                 isfinite(stat)?stat:0.0f,nv,nt,nrv,
                 corr_metric==SIM_PEARSON?"pearson":"spearman",
                 method==INSYNC_METHOD_PAIRWISE?"pairwise":"loo",
                 summary==INSYNC_SUMMARY_MEDIAN?"median":"fisher_mean") ;
       }
       if( mfp!=NULL && pair_ok ){
         for( aa=0 ; aa<nsub ; aa++ ) for( bb=aa+1 ; bb<nsub ; bb++ ){
           const char *sa=cindex?cindex->level[0][aa]:tab->subj[aa] ;
           const char *sb=cindex?cindex->level[0][bb]:tab->subj[bb] ;
           fprintf(mfp,"%s %d %s %s %s %s %s %.9g\n",clabel[cc],
                   rl->val[rr],rlab,sa,sb,glabel[group[aa]],glabel[group[bb]],
                   sm->mat[(size_t)aa*nsub+bb]) ;
         }
       }
     }
     nwritten++ ;
   }
   fclose(sfp) ; if( mfp!=NULL ) fclose(mfp) ;
   if( !quiet ) INFO_message(PROGRAM_NAME ": wrote %d atlas ROI summaries to %s%s%s",
                 nwritten,sname,matrix_name?" and matrices to ":"",
                 matrix_name?matrix_name:"") ;
   free(data); free(ref); free(values); free(scratch); free(rank1); free(rank2);
   free(goodtime); THD_simmat_free(sm); THD_roilist_free(rl); DSET_delete(aset);
   (void)ntime_input ; (void)rowmap ;
}

/*! Cache the Fourier spectrum of every subject's real time series. */
static int insync_phase_prepare( int nsub, int ntime, float *series,
                                 complex *spectrum )
{
   int ss,tt ;
   if( series==NULL || spectrum==NULL ) return 1 ;
   for( ss=0 ; ss<nsub ; ss++ ){
     for( tt=0 ; tt<ntime ; tt++ ){
       spectrum[(size_t)ss*ntime+tt].r=series[(size_t)ss*ntime+tt] ;
       spectrum[(size_t)ss*ntime+tt].i=0.0f ;
     }
     if( THD_fftnf_OMP(ntime,NULL,&spectrum[(size_t)ss*ntime].r,
                       &spectrum[(size_t)ss*ntime].i,-2,0.0) ) return 1 ;
   }
   return 0 ;
}

/*! Materialize one real-valued phase surrogate from cached spectra. */
static int insync_phase_draw( THD_phase_set *pset, int iphase,
                              complex *spectrum, complex *work, float *series )
{
   int ss,tt,ff,ntime,nsub ;
   if( pset==NULL || spectrum==NULL || work==NULL || series==NULL ) return 1 ;
   ntime=pset->ntime ; nsub=pset->nobs ;
   for( ss=0 ; ss<nsub ; ss++ ){
     complex *src=spectrum+(size_t)ss*ntime ;
     memcpy(work,src,sizeof(complex)*(size_t)ntime) ;
     /* Apply seeded unit-complex factors to positive frequencies and mirror
        them onto negative frequencies to preserve a real inverse transform. */
     for( ff=1 ; ff<=pset->nfreq ; ff++ ){
       float co,si,ar=src[ff].r,ai=src[ff].i ;
       THD_phase_set_factor(pset,iphase,ss,ff,&co,&si) ;
       work[ff].r=ar*co-ai*si ; work[ff].i=ar*si+ai*co ;
       work[ntime-ff].r=work[ff].r ; work[ntime-ff].i=-work[ff].i ;
     }
     if( THD_fftnf_OMP(ntime,NULL,&work[0].r,&work[0].i,+2,0.0) ) return 1 ;
     for( tt=0 ; tt<ntime ; tt++ )
       series[(size_t)ss*ntime+tt]=work[tt].r/(float)ntime ;
   }
   return 0 ;
}

/*! Materialize one circularly shifted subject-major data block. */
static void insync_shift_draw( int nsub, int ntime, const float *data,
                               const int *offset, float *series )
{
   int ss,tt ;
   for( ss=0 ; ss<nsub ; ss++ ) for( tt=0 ; tt<ntime ; tt++ )
     series[(size_t)ss*ntime+tt]
       =data[(size_t)ss*ntime+(tt+offset[ss])%ntime] ;
}

/*! qsort comparator for ascending float-valued empirical null arrays. */
static int insync_float_compare( const void *a, const void *b )
{
   float aa=*((const float *)a),bb=*((const float *)b) ;
   return (aa>bb)-(aa<bb) ;
}

/*! Finish an AFNI-style bisided test.  Each sign is compared with its own
    max-statistic null, then the single-tail probabilities are doubled so the
    reported values stay on the conventional two-sided p-value scale. */
static void insync_finish_bisided( PERM_result *pr, byte *valid,
                                   float *max_null_neg )
{
   int ii ;
   if( pr==NULL || max_null_neg==NULL ) return ;
   qsort(pr->max_null,pr->nperm,sizeof(float),insync_float_compare) ;
   qsort(max_null_neg,pr->nperm,sizeof(float),insync_float_compare) ;
   for( ii=0 ; ii<pr->nelem ; ii++ ){
     float cmp,punc,pfwe,*null_dist ;
     if( valid!=NULL && !valid[ii] ){
       pr->p_unc[ii]=pr->p_fwe[ii]=1.0f ;
       pr->z_unc[ii]=pr->z_fwe[ii]=0.0f ;
       continue ;
     }
     cmp=(pr->stat[ii]<0.0f)?-pr->stat[ii]:pr->stat[ii] ;
     null_dist=(pr->stat[ii]<0.0f)?max_null_neg:pr->max_null ;
     punc=2.0f*(float)pr->cnt_unc[ii]/(float)pr->nperm ;
     pfwe=2.0f*THD_perm_emp_pvalue(null_dist,pr->nperm,cmp) ;
     pr->p_unc[ii]=(punc>1.0f)?1.0f:punc ;
     pr->p_fwe[ii]=(pfwe>1.0f)?1.0f:pfwe ;
     pr->z_unc[ii]=THD_perm_signed_z(pr->p_unc[ii],pr->stat[ii],PERM_TAIL_TWO) ;
     pr->z_fwe[ii]=THD_perm_signed_z(pr->p_fwe[ii],pr->stat[ii],PERM_TAIL_TWO) ;
   }
}

/*----------------------------------------------------------------------------
  Edge (cofluctuation) ISC.  Each subject's atlas-ROI series are z-scored
  (sample SD, T-1) so the mean of the edge series z_i(t)*z_j(t) is exactly the
  Pearson r of the pair.  The ordinary ISC estimators are then applied to the
  edge series, to their root-sum-of-squares amplitude, and to the frames in
  which that amplitude is highest.
------------------------------------------------------------------------------*/

typedef struct { float v ; int t ; } INSYNC_vt ;

/*! Descending amplitude, earliest frame first among ties. */
static int insync_vt_compare( const void *a, const void *b )
{
   const INSYNC_vt *x=(const INSYNC_vt *)a,*y=(const INSYNC_vt *)b ;
   if( x->v>y->v ) return -1 ;
   if( x->v<y->v ) return 1 ;
   return (x->t>y->t)-(x->t<y->t) ;
}

/*! Per-worker scratch for the group ISC estimators. */
typedef struct {
   THD_simmat *sm ;
   float *scratch,*ref,*values,*rank1,*rank2 ;
} INSYNC_ws ;

static int insync_ws_init( INSYNC_ws *w, int nsub, int nt, int spearman )
{
   size_t nedge=(size_t)nsub*(nsub-1)/2,nscratch=(nedge>(size_t)nsub)?nedge:(size_t)nsub ;
   memset(w,0,sizeof(*w)) ;
   w->sm=THD_simmat_new(nsub) ;
   w->scratch=(float *)malloc(sizeof(float)*nscratch) ;
   w->ref=(float *)malloc(sizeof(float)*(size_t)nt) ;
   w->values=(float *)malloc(sizeof(float)*(size_t)nsub) ;
   if( spearman ){
     w->rank1=(float *)malloc(sizeof(float)*(size_t)nt) ;
     w->rank2=(float *)malloc(sizeof(float)*(size_t)nt) ;
   }
   return !(w->sm && w->scratch && w->ref && w->values &&
            (!spearman || (w->rank1 && w->rank2))) ;
}

static void insync_ws_free( INSYNC_ws *w )
{
   THD_simmat_free(w->sm) ; free(w->scratch) ; free(w->ref) ; free(w->values) ;
   free(w->rank1) ; free(w->rank2) ;
}

/*! Group ISC of one subject-major block (nsub series of nt points), by the
    same pairwise or leave-one-out estimator used voxelwise.  stat[g] is NaN
    when the group estimate is invalid. */
static void insync_block_stats( float *block, int nsub, int nt, int corr_metric,
                                int method, int summary, int ngroup,
                                const int *gcount, int **gmember, INSYNC_ws *w,
                                float *stat, int *nv )
{
   int gg ;
   for( gg=0 ; gg<ngroup ; gg++ ){ stat[gg]=NAN ; nv[gg]=0 ; }
   if( nt<3 ) return ;
   if( method==INSYNC_METHOD_PAIRWISE ){
     if( THD_simmat_fill_from_features(w->sm,nt,block,corr_metric,w->rank1,w->rank2) ) return ;
     for( gg=0 ; gg<ngroup ; gg++ )
       stat[gg]=INSYNC_pairwise_indexed(nsub,w->sm->mat,gcount[gg],gmember[gg],
                                        summary,w->scratch,&nv[gg]) ;
   } else for( gg=0 ; gg<ngroup ; gg++ )
       stat[gg]=INSYNC_loo_indexed_metric(nsub,nt,block,gcount[gg],gmember[gg],
                  summary,corr_metric,w->ref,w->values,w->rank1,w->rank2,&nv[gg]) ;
}

/*! Open the atlas, check that positive labels are integers, and select ROIs. */
static THD_roilist * insync_open_atlas( const char *atlas_name, const char *roi_sel,
                                        THD_3dim_dataset *first,
                                        THD_3dim_dataset **aset_out )
{
   THD_3dim_dataset *aset ; THD_roilist *rl ; int vv,nvox ;
   aset=THD_open_dataset((char *)atlas_name) ; CHECK_OPEN_ERROR(aset,atlas_name) ;
   DSET_load(aset) ; CHECK_LOAD_ERROR(aset) ;
   if( !EQUIV_GRIDS(first,aset) )
     ERROR_exit(PROGRAM_NAME ": -atlas is not on the input grid") ;
   nvox=DSET_NVOX(aset) ;
   for( vv=0 ; vv<nvox ; vv++ ){
     float x=THD_get_voxel(aset,vv,0) ;
     if( !isfinite(x) || (x>0.0f && x!=floorf(x)) )
       ERROR_exit(PROGRAM_NAME ": atlas '%s' has invalid label %.7g at voxel %d; "
                  "positive labels must be finite integers",atlas_name,x,vv) ;
   }
   rl=THD_roilist_from_dset(aset,(char *)roi_sel) ;
   if( rl==NULL ) ERROR_exit(PROGRAM_NAME ": -atlas/-roi_sel selected no positive ROIs") ;
   *aset_out=aset ; return rl ;
}

/*! Name of one edge output.  kind is edge or frames for the PREFIX.kind.1D
    tables, or netcc for one per-condition/group matrix. */
static void insync_edge_fname( char *dst, size_t ndst, const char *prefix,
                               const char *kind, const char *cond,
                               const char *group )
{
   char c[128],g[128] ;
   if( strcmp(kind,"netcc")!=0 ){ snprintf(dst,ndst,"%s.%s.1D",prefix,kind) ; return ; }
   insync_safe_token(g,sizeof(g),group) ;
   if( cond!=NULL ){
     insync_safe_token(c,sizeof(c),cond) ;
     snprintf(dst,ndst,"%s_edge_%s_%s.netcc",prefix,c,g) ;
   } else snprintf(dst,ndst,"%s_edge_%s.netcc",prefix,g) ;
}

static FILE * insync_open_text( const char *name )
{
   FILE *fp ;
   if( !THD_ok_overwrite() && THD_is_file((char *)name) )
     ERROR_exit(PROGRAM_NAME ": output '%s' already exists",name) ;
   fp=fopen(name,"w") ;
   if( fp==NULL ) ERROR_exit(PROGRAM_NAME ": cannot write '%s'",name) ;
   return fp ;
}

/*! Write one square ROI matrix in 3dNetCorr's .netcc layout. */
static void insync_write_netcc( const char *name, int nroi, int nmat, char **mlabel,
                                float **mat, char **rlab, const int *rval )
{
   FILE *fp=insync_open_text(name) ;
   int mm,ii,jj ;
   fprintf(fp,"# %d  # Number of network ROIs\n",nroi) ;
   fprintf(fp,"# %d  # Number of netcc matrices\n",nmat) ;
   fprintf(fp,"# WITH_ROI_LABELS\n") ;
   for( ii=0 ; ii<nroi-1 ; ii++ ) fprintf(fp," %10s \t",rlab[ii]) ;
   fprintf(fp,"  %10s\n",rlab[nroi-1]) ;
   for( ii=0 ; ii<nroi-1 ; ii++ ) fprintf(fp," %10d \t",rval[ii]) ;
   fprintf(fp,"  %10d\n",rval[nroi-1]) ;
   for( mm=0 ; mm<nmat ; mm++ ){
     fprintf(fp,"# %s\n",mlabel[mm]) ;
     for( ii=0 ; ii<nroi ; ii++ ){
       for( jj=0 ; jj<nroi-1 ; jj++ ) fprintf(fp,"%12.4f\t",mat[mm][(size_t)ii*nroi+jj]) ;
       fprintf(fp,"%12.4f\n",mat[mm][(size_t)ii*nroi+jj]) ;
     }
   }
   fclose(fp) ;
}

/*! Edge ISC, cofluctuation-amplitude ISC, and shared-event outputs from atlas
    ROI series.  All ROIs share one retained timeline.  The edge null shifts
    ROI j of every pair by one offset common to all subjects, so each node's
    own ISC is untouched while the within-subject i-j coupling is broken; edge
    ISC beyond that null is synchrony the nodes alone do not explain.  The
    amplitude and event nulls shift each subject independently. */
static void insync_write_edge_outputs(
   const char *atlas_name, const char *roi_sel, const char *prefix,
   THD_3dim_dataset *first, const INSYNC_src *src, int nsub, int ncond,
   int ntime, int *time_index, byte *mask, int missing_policy, int corr_metric,
   int method, int summary, int ngroup, int *gcount, int **gmember,
   char **glabel, THD_datatable *tab, THD_datatable_index *cindex,
   char **clabel, INSYNC_edge_opts *eo, int quiet )
{
   THD_3dim_dataset *aset=NULL ; THD_roilist *rl ;
   THD_timeshift_set *eset=NULL,*sset=NULL ;
   PERM_result *eres=NULL,*vres=NULL ;
   byte *evalid=NULL,*vvalid=NULL ;
   int nroi=0,rr,cc,ss,tt,gg,k,nt=ntime,ngood,spear=(corr_metric==SIM_SPEARMAN) ;
   int *ridx,*rval,*ei=NULL,*ej=NULL,*tmap ;
   char **rlab ;
   size_t E,nser,e,zsize ;
   float *zdat,*rss=NULL,*eisc=NULL,*en=NULL,*enullsum=NULL,*node_isc,*node_n ;
   long long ninvalid=0 ;
   byte *goodtime ;
   int usenull=(eo->nnull>0) ;

   rl=insync_open_atlas(atlas_name,roi_sel,first,&aset) ;
   ridx=(int *)malloc(sizeof(int)*(size_t)rl->nroi) ;
   rval=(int *)malloc(sizeof(int)*(size_t)rl->nroi) ;
   rlab=(char **)calloc((size_t)rl->nroi,sizeof(char *)) ;
   for( rr=0 ; rr<rl->nroi ; rr++ ){
     int vv,nrv=0 ; char lab[128] ;
     for( vv=0 ; vv<rl->vox[rr].nar ; vv++ ){
       int iv=rl->vox[rr].ar[vv] ;
       if( mask==NULL || mask[iv] ) nrv++ ;
     }
     if( nrv<1 ) continue ;
     if( rl->lab!=NULL && rl->lab[rr]!=NULL ) insync_safe_token(lab,sizeof(lab),rl->lab[rr]) ;
     else snprintf(lab,sizeof(lab),"ROI%d",rl->val[rr]) ;
     ridx[nroi]=rr ; rval[nroi]=rl->val[rr] ; rlab[nroi]=strdup(lab) ; nroi++ ;
   }
   if( nroi<2 ) ERROR_exit(PROGRAM_NAME ": edge analysis needs at least 2 ROIs; found %d",nroi) ;
   E=(size_t)nroi*(nroi-1)/2 ; nser=(size_t)ncond*nroi*nsub ;
   zsize=nser*(size_t)ntime ;
   if( usenull && (size_t)ncond*ngroup*E>(size_t)INT_MAX )
     ERROR_exit(PROGRAM_NAME ": too many edges (%lu) for a null family",(unsigned long)E) ;
   zdat=(float *)malloc(sizeof(float)*zsize) ;
   if( zdat==NULL )
     ERROR_exit(PROGRAM_NAME ": cannot allocate %.2f GiB for ROI series; restrict "
                "with -roi_sel",(double)zsize*sizeof(float)/1073741824.0) ;
   if( !quiet ) INFO_message(PROGRAM_NAME ": edge analysis: %d ROIs, %lu edges, %.2f GiB of ROI series",
                  nroi,(unsigned long)E,(double)zsize*sizeof(float)/1073741824.0) ;

   /* ROI-mean series for every subject and condition. */
   {
     int *bad_c=(int *)malloc(sizeof(int)*(size_t)nroi) ;
     int *bad_s=(int *)malloc(sizeof(int)*(size_t)nroi) ;
     int *bad_t=(int *)malloc(sizeof(int)*(size_t)nroi) ;
     for( k=0 ; k<nroi ; k++ ) bad_t[k]=-1 ;
#ifdef USE_OMP
#pragma omp parallel for schedule(dynamic)
#endif
     for( k=0 ; k<nroi ; k++ ){
       int c2,s2,t2,v2,r2=ridx[k] ;
       for( c2=0 ; c2<ncond ; c2++ ) for( s2=0 ; s2<nsub ; s2++ ){
         float *dst=zdat+(((size_t)c2*nroi+k)*nsub+s2)*ntime ;
         for( t2=0 ; t2<ntime ; t2++ ){
           double sum=0.0 ; int bad=0,nv=0,ot=time_index[t2] ;
           for( v2=0 ; v2<rl->vox[r2].nar ; v2++ ){
             int iv=rl->vox[r2].ar[v2] ; float x ;
             if( mask!=NULL && !mask[iv] ) continue ;
             x=insync_src_get(src,s2*ncond+c2,iv,ot) ;
             if( !isfinite(x) ){ bad=1 ; break ; }
             sum+=x ; nv++ ;
           }
           if( bad && missing_policy==INSYNC_MISSING_ERROR && bad_t[k]<0 ){
             bad_c[k]=c2 ; bad_s[k]=s2 ; bad_t[k]=ot ;
           }
           dst[t2]=(bad || nv<1)?NAN:(float)(sum/nv) ;
         }
       }
     }
     for( k=0 ; k<nroi ; k++ ) if( bad_t[k]>=0 )
       ERROR_exit(PROGRAM_NAME ": nonfinite atlas input for ROI %d, Subj %s, "
                  "condition %s, time %d",rval[k],
                  cindex?cindex->level[0][bad_s[k]]:tab->subj[bad_s[k]],
                  clabel[bad_c[k]],bad_t[k]+1) ;
     free(bad_c) ; free(bad_s) ; free(bad_t) ;
   }

   /* One retained timeline for every ROI, subject, and condition, so each
      product z_i(t)z_j(t) is defined on the same frames everywhere. */
   goodtime=(byte *)malloc((size_t)ntime) ; tmap=(int *)malloc(sizeof(int)*(size_t)ntime) ;
   ngood=0 ;
   for( tt=0 ; tt<ntime ; tt++ ){
     int good=1 ; size_t q ;
     for( q=0 ; q<nser && good ; q++ )
       if( !isfinite(zdat[q*ntime+tt]) ) good=0 ;
     goodtime[tt]=(byte)good ; if( good ){ tmap[ngood]=time_index[tt] ; ngood++ ; }
   }
   if( ngood<3 ) ERROR_exit(PROGRAM_NAME ": edge analysis retains %d common time points; need at least 3",ngood) ;
   if( ngood<ntime ){
     size_t q ;
     for( q=0 ; q<nser ; q++ ){
       int pos=0 ; float *src=zdat+q*ntime,*dst=zdat+q*ngood ;
       for( tt=0 ; tt<ntime ; tt++ ) if( goodtime[tt] ) dst[pos++]=src[tt] ;
     }
     if( !quiet ) INFO_message(PROGRAM_NAME ": edge analysis keeps %d of %d time points common to all ROIs",ngood,ntime) ;
   }
   nt=ngood ; free(goodtime) ;

   /* z-score with the sample SD, so mean(z_i z_j) is Pearson r.  A constant
      series becomes all zero and gives a zero edge, never a NaN. */
#ifdef USE_OMP
#pragma omp parallel for schedule(static)
#endif
   for( k=0 ; k<(int)nser ; k++ ){
     float *z=zdat+(size_t)k*nt ; double m=0.0,v=0.0 ; int t2 ;
     for( t2=0 ; t2<nt ; t2++ ) m+=z[t2] ;
     m/=nt ;
     for( t2=0 ; t2<nt ; t2++ ){ double d=z[t2]-m ; v+=d*d ; }
     v=sqrt(v/(nt-1)) ;
     for( t2=0 ; t2<nt ; t2++ ) z[t2]=(v>0.0)?(float)((z[t2]-m)/v):0.0f ;
   }

   if( usenull ){
     eset=THD_timeshift_set_build(2,nt,eo->nnull,eo->min_shift,eo->seed) ;  /* 2 slots: the builder needs >=2; slot 0 is used */
     sset=THD_timeshift_set_build(nsub,nt,eo->nnull,eo->min_shift,eo->seed+7919L) ;
     if( eset==NULL || sset==NULL )
       ERROR_exit(PROGRAM_NAME ": cannot build edge null shifts; -min_shift %d is too large for %d time points",
                  eo->min_shift,nt) ;
   }

   /* ---------------- node ISC and edge ISC ---------------- */
   if( eo->edges ){
     FILE *efp ;
     char name[THD_MAX_NAME] ;
     int *vcount=NULL ;
     float *qval=NULL ;
     size_t nelem=(size_t)ncond*ngroup*E ;

     node_isc=(float *)calloc((size_t)ncond*ngroup*nroi,sizeof(float)) ;
     node_n=(float *)calloc((size_t)ncond*ngroup*nroi,sizeof(float)) ;
     eisc=(float *)calloc(nelem,sizeof(float)) ; en=(float *)calloc(nelem,sizeof(float)) ;
     ei=(int *)malloc(sizeof(int)*E) ; ej=(int *)malloc(sizeof(int)*E) ;
     if( node_isc==NULL || node_n==NULL || eisc==NULL || en==NULL || ei==NULL || ej==NULL )
       ERROR_exit(PROGRAM_NAME ": cannot allocate edge output arrays") ;
     { int a,b ; e=0 ; for( a=0 ; a<nroi ; a++ ) for( b=a+1 ; b<nroi ; b++,e++ ){ ei[e]=a ; ej[e]=b ; } }
     if( usenull ){
       evalid=(byte *)calloc(nelem,sizeof(byte)) ; enullsum=(float *)calloc(nelem,sizeof(float)) ;
       eres=THD_perm_result_new((int)nelem,eo->nnull) ;
       if( evalid==NULL || enullsum==NULL || eres==NULL )
         ERROR_exit(PROGRAM_NAME ": cannot allocate edge null results") ;
       eres->tail=PERM_TAIL_ONE ;
       for( ss=0 ; ss<eo->nnull ; ss++ ) eres->max_null[ss]=-FLT_MAX ;
     }
     (void)vcount ;

#ifdef USE_OMP
#pragma omp parallel reduction(+:ninvalid)
#endif
     {
       INSYNC_ws ws ;
       float *eblock=(float *)malloc(sizeof(float)*(size_t)nsub*nt) ;
       float *tnull=usenull?(float *)malloc(sizeof(float)*(size_t)ngroup*eo->nnull):NULL ;
       float *mymax=usenull?(float *)malloc(sizeof(float)*(size_t)eo->nnull):NULL ;
       float stat[64] ; int nv[64] ;
       int s3,t3,ss,cc,gg ;
       if( insync_ws_init(&ws,nsub,nt,spear) || eblock==NULL ||
           (usenull && (tnull==NULL || mymax==NULL)) || ngroup>64 )
         ERROR_exit(PROGRAM_NAME ": cannot allocate edge workers (or more than 64 groups)") ;
       if( usenull ) for( ss=0 ; ss<eo->nnull ; ss++ ) mymax[ss]=-FLT_MAX ;

       /* node ISC: same estimator on the ROI series themselves */
#ifdef USE_OMP
#pragma omp for schedule(dynamic)
#endif
       for( k=0 ; k<nroi ; k++ ) for( cc=0 ; cc<ncond ; cc++ ){
         float *blk=zdat+(((size_t)cc*nroi+k)*nsub)*nt ;
         insync_block_stats(blk,nsub,nt,corr_metric,method,summary,ngroup,gcount,
                            gmember,&ws,stat,nv) ;
         for( gg=0 ; gg<ngroup ; gg++ ){
           size_t o=((size_t)cc*ngroup+gg)*nroi+k ;
           if( isfinite(stat[gg]) ){ node_isc[o]=stat[gg] ; node_n[o]=(float)nv[gg] ; }
         }
       }

#ifdef USE_OMP
#pragma omp for schedule(dynamic,16)
#endif
       for( k=0 ; k<(int)E ; k++ ){
         int a=ei[k],b=ej[k] ;
         for( cc=0 ; cc<ncond ; cc++ ){
           float *zi=zdat+(((size_t)cc*nroi+a)*nsub)*nt ;
           float *zj=zdat+(((size_t)cc*nroi+b)*nsub)*nt ;
           int nrun=usenull?eo->nnull:1 ;
           int good[64] ;
           for( ss=0 ; ss<nrun ; ss++ ){
             int off=(ss==0)?0:eset->offset[(size_t)ss*2] ;
             for( s3=0 ; s3<nsub ; s3++ ){
               float *pi=zi+(size_t)s3*nt,*pj=zj+(size_t)s3*nt,*po=eblock+(size_t)s3*nt ;
               for( t3=0 ; t3<nt ; t3++ ){
                 int u=t3+off ; if( u>=nt ) u-=nt ;
                 po[t3]=pi[t3]*pj[u] ;
               }
             }
             insync_block_stats(eblock,nsub,nt,corr_metric,method,summary,ngroup,
                                gcount,gmember,&ws,stat,nv) ;
             if( ss==0 ){
               for( gg=0 ; gg<ngroup ; gg++ ){
                 size_t o=((size_t)cc*ngroup+gg)*E+k ;
                 good[gg]=isfinite(stat[gg]) ;
                 if( good[gg] ){ eisc[o]=stat[gg] ; en[o]=(float)nv[gg] ; }
                 else ninvalid++ ;
               }
             }
             if( usenull ) for( gg=0 ; gg<ngroup ; gg++ ){
               if( !isfinite(stat[gg]) ) good[gg]=0 ;
               tnull[(size_t)gg*eo->nnull+ss]=stat[gg] ;
             }
             if( ss==0 ){   /* nothing valid to test: skip the surrogates */
               int any=0 ; for( gg=0 ; gg<ngroup ; gg++ ) any|=good[gg] ;
               if( !any ) break ;
             }
           }
           if( usenull ) for( gg=0 ; gg<ngroup ; gg++ ){
             size_t o=((size_t)cc*ngroup+gg)*E+k ;
             float obs=tnull[(size_t)gg*eo->nnull],sum=0.0f ;
             if( !good[gg] ) continue ;
             evalid[o]=1 ; eres->stat[o]=obs ;
             for( ss=0 ; ss<eo->nnull ; ss++ ){
               float st=tnull[(size_t)gg*eo->nnull+ss] ;
               if( st>=obs ) eres->cnt_unc[o]++ ;
               if( st>mymax[ss] ) mymax[ss]=st ;
               if( ss>0 ) sum+=st ;
             }
             enullsum[o]=sum/(float)(eo->nnull-1) ;
           }
         }
       }
       if( usenull ){
#ifdef USE_OMP
#pragma omp critical(insync_edge_max)
#endif
         { for( ss=0 ; ss<eo->nnull ; ss++ )
             if( mymax[ss]>eres->max_null[ss] ) eres->max_null[ss]=mymax[ss] ; }
       }
       insync_ws_free(&ws) ; free(eblock) ; free(tnull) ; free(mymax) ;
     }

     if( usenull ){
       size_t nvalid=0 ;
       for( e=0 ; e<nelem ; e++ ) if( evalid[e] ) nvalid++ ;
       if( nvalid<1 ) ERROR_exit(PROGRAM_NAME ": no valid edge-null cells") ;
       for( ss=0 ; ss<eo->nnull ; ss++ )
         if( eres->max_null[ss]==-FLT_MAX ) eres->max_null[ss]=0.0f ;
       THD_perm_result_finish(eres,evalid) ;
       qval=(float *)malloc(sizeof(float)*nelem) ;
       THD_bh_fdr_masked((int)nelem,eres->p_unc,evalid,qval) ;
       if( !quiet ) INFO_message(PROGRAM_NAME ": edge-null family contains %lu valid "
                                 "condition-by-group-by-edge cells",(unsigned long)nvalid) ;
     }

     /* one long table: each edge row also carries the ISC of its two nodes */
     snprintf(name,sizeof(name),"%s.edge.1D",prefix) ;
     efp=insync_open_text(name) ;
     fprintf(efp,"Condition Group ROI1 ROI2 ROI1_Label ROI2_Label ISC N Node1ISC Node2ISC") ;
     if( usenull ) fprintf(efp," NullMean Excess P Z Q PFWE ZFWE") ;
     fprintf(efp,"\n") ;
     for( cc=0 ; cc<ncond ; cc++ ) for( gg=0 ; gg<ngroup ; gg++ ) for( e=0 ; e<E ; e++ ){
       size_t o=((size_t)cc*ngroup+gg)*E+e ;
       size_t no=((size_t)cc*ngroup+gg)*nroi ;
       fprintf(efp,"%s %s %d %d %s %s %.9g %d %.9g %.9g",clabel[cc],glabel[gg],rval[ei[e]],
               rval[ej[e]],rlab[ei[e]],rlab[ej[e]],eisc[o],(int)en[o],
               node_isc[no+ei[e]],node_isc[no+ej[e]]) ;
       if( usenull ){
         if( evalid[o] )
           fprintf(efp," %.9g %.9g %.9g %.9g %.9g %.9g %.9g",enullsum[o],eisc[o]-enullsum[o],
                   eres->p_unc[o],eres->z_unc[o],qval[o],eres->p_fwe[o],eres->z_fwe[o]) ;
         else fprintf(efp," 0 0 1 0 1 1 0") ;
       }
       fprintf(efp,"\n") ;
     }
     fclose(efp) ;

     /* optional .netcc matrices: node ISC on the diagonal, edge ISC off it */
     if( eo->matrix ) for( cc=0 ; cc<ncond ; cc++ ) for( gg=0 ; gg<ngroup ; gg++ ){
       int nmat=usenull?5:2,mm,a,b ;
       char *mlab[5]={"CC","FZ","EX","P","PFWE"} ;
       float *mat[5] ;
       size_t eo2=((size_t)cc*ngroup+gg)*E,no=((size_t)cc*ngroup+gg)*nroi ;
       for( mm=0 ; mm<nmat ; mm++ ){
         mat[mm]=(float *)calloc((size_t)nroi*nroi,sizeof(float)) ;
         if( mat[mm]==NULL ) ERROR_exit(PROGRAM_NAME ": cannot allocate edge matrix") ;
       }
       for( a=0 ; a<nroi ; a++ ){
         mat[0][(size_t)a*nroi+a]=node_isc[no+a] ;
         mat[1][(size_t)a*nroi+a]=insync_fisher_z(node_isc[no+a]) ;
         if( usenull ) mat[3][(size_t)a*nroi+a]=mat[4][(size_t)a*nroi+a]=1.0f ;
       }
       for( e=0 ; e<E ; e++ ){
         size_t o=eo2+e ;
         a=ei[e] ; b=ej[e] ;
         mat[0][(size_t)a*nroi+b]=mat[0][(size_t)b*nroi+a]=eisc[o] ;
         mat[1][(size_t)a*nroi+b]=mat[1][(size_t)b*nroi+a]=insync_fisher_z(eisc[o]) ;
         if( usenull ){
           float ex=evalid[o]?eisc[o]-enullsum[o]:0.0f ;
           float pu=evalid[o]?eres->p_unc[o]:1.0f,pf=evalid[o]?eres->p_fwe[o]:1.0f ;
           mat[2][(size_t)a*nroi+b]=mat[2][(size_t)b*nroi+a]=ex ;
           mat[3][(size_t)a*nroi+b]=mat[3][(size_t)b*nroi+a]=pu ;
           mat[4][(size_t)a*nroi+b]=mat[4][(size_t)b*nroi+a]=pf ;
         }
       }
       insync_edge_fname(name,sizeof(name),prefix,"netcc",ncond>1?clabel[cc]:NULL,glabel[gg]) ;
       insync_write_netcc(name,nroi,nmat,mlab,mat,rlab,rval) ;
       for( mm=0 ; mm<nmat ; mm++ ) free(mat[mm]) ;
     }
     if( !quiet ){
       if( eo->matrix )
         INFO_message(PROGRAM_NAME ": wrote edge ISC for %lu edges to %s.edge.1D and %d .netcc matri%s",
                      (unsigned long)E,prefix,ncond*ngroup,(ncond*ngroup==1)?"x":"ces") ;
       else INFO_message(PROGRAM_NAME ": wrote edge ISC for %lu edges to %s.edge.1D",(unsigned long)E,prefix) ;
     }
     free(node_isc) ; free(node_n) ; free(eisc) ; free(en) ; free(qval) ;
     free(evalid) ; free(enullsum) ; THD_perm_result_free(eres) ;
   }

   /* ---------------- amplitude and shared events ---------------- */
   if( eo->rss || eo->events ){
     size_t rsz=(size_t)ncond*nsub*nt ;
     double *s2=(double *)calloc(rsz,sizeof(double)),*s4=(double *)calloc(rsz,sizeof(double)) ;
     INSYNC_ws ws ;
     float stat[64] ; int nv[64] ;
     float *r_obs=NULL,*r_nmean=NULL,*r_p=NULL,*v_q=NULL,*v_nmean=NULL ;
     int *r_n=NULL,K=0 ;
     FILE *ffp ; char name[THD_MAX_NAME] ;
     rss=(float *)malloc(sizeof(float)*rsz) ;
     if( s2==NULL || s4==NULL || rss==NULL || ngroup>64 || insync_ws_init(&ws,nsub,nt,spear) )
       ERROR_exit(PROGRAM_NAME ": cannot allocate amplitude arrays") ;

     /* RSS^2 = sum_{i<j} (z_i z_j)^2 = ((sum z^2)^2 - sum z^4)/2 */
     for( cc=0 ; cc<ncond ; cc++ ) for( k=0 ; k<nroi ; k++ ) for( ss=0 ; ss<nsub ; ss++ ){
       float *z=zdat+(((size_t)cc*nroi+k)*nsub+ss)*nt ;
       size_t o=((size_t)cc*nsub+ss)*nt ;
       for( tt=0 ; tt<nt ; tt++ ){ double q=(double)z[tt]*z[tt] ; s2[o+tt]+=q ; s4[o+tt]+=q*q ; }
     }
     { size_t q ; for( q=0 ; q<rsz ; q++ ){
         double v=0.5*(s2[q]*s2[q]-s4[q]) ; rss[q]=(float)sqrt(v>0.0?v:0.0) ; } }
     free(s2) ; free(s4) ;

     if( eo->rss ){
       size_t nel=(size_t)ncond*ngroup ;
       float *obs,*nmean,*pval,*sur=(float *)malloc(sizeof(float)*(size_t)nsub*nt) ;
       int *nn,*cnt=(int *)calloc(nel,sizeof(int)) ;
       obs=r_obs=(float *)calloc(nel,sizeof(float)) ; nmean=r_nmean=(float *)calloc(nel,sizeof(float)) ;
       pval=r_p=(float *)calloc(nel,sizeof(float)) ; nn=r_n=(int *)calloc(nel,sizeof(int)) ;
       if( obs==NULL || nmean==NULL || pval==NULL || sur==NULL || nn==NULL || cnt==NULL )
         ERROR_exit(PROGRAM_NAME ": cannot allocate amplitude results") ;
       for( cc=0 ; cc<ncond ; cc++ ){
         float *blk=rss+(size_t)cc*nsub*nt ;
         insync_block_stats(blk,nsub,nt,corr_metric,method,summary,ngroup,gcount,gmember,&ws,stat,nv) ;
         for( gg=0 ; gg<ngroup ; gg++ ){
           size_t o=(size_t)cc*ngroup+gg ;
           if( isfinite(stat[gg]) ){ obs[o]=stat[gg] ; nn[o]=nv[gg] ; }
         }
         if( usenull ){
           for( gg=0 ; gg<ngroup ; gg++ ) cnt[(size_t)cc*ngroup+gg]=1 ;  /* identity slot */
           for( ss=1 ; ss<eo->nnull ; ss++ ){
             insync_shift_draw(nsub,nt,blk,sset->offset+(size_t)ss*nsub,sur) ;
             insync_block_stats(sur,nsub,nt,corr_metric,method,summary,ngroup,gcount,gmember,&ws,stat,nv) ;
             for( gg=0 ; gg<ngroup ; gg++ ){
               size_t o=(size_t)cc*ngroup+gg ;
               if( isfinite(stat[gg]) ){
                 nmean[o]+=stat[gg]/(float)(eo->nnull-1) ;
                 if( stat[gg]>=obs[o] ) cnt[o]++ ;
               }
             }
           }
           for( gg=0 ; gg<ngroup ; gg++ ){
             size_t o=(size_t)cc*ngroup+gg ; pval[o]=(float)cnt[o]/(float)eo->nnull ;
           }
         }
       }
       free(sur) ; free(cnt) ;
     }

     if( eo->events ){
       size_t nel=(size_t)ncond*ngroup*nt ;
       byte *hi=(byte *)calloc((size_t)ncond*nsub*nt,sizeof(byte)) ;
       INSYNC_vt *vt=(INSYNC_vt *)malloc(sizeof(INSYNC_vt)*(size_t)nt) ;
       K=(int)ceil((double)eo->event_frac*nt-1.0e-9) ;
       if( K<1 ) K=1 ;
       if( K>nt-1 ) K=nt-1 ;
       if( hi==NULL || vt==NULL ) ERROR_exit(PROGRAM_NAME ": cannot allocate event arrays") ;
       for( cc=0 ; cc<ncond ; cc++ ) for( ss=0 ; ss<nsub ; ss++ ){
         float *r0=rss+((size_t)cc*nsub+ss)*nt ; byte *h0=hi+((size_t)cc*nsub+ss)*nt ;
         for( tt=0 ; tt<nt ; tt++ ){ vt[tt].v=r0[tt] ; vt[tt].t=tt ; }
         qsort(vt,(size_t)nt,sizeof(INSYNC_vt),insync_vt_compare) ;
         for( k=0 ; k<K ; k++ ) h0[vt[k].t]=1 ;
       }
       vres=THD_perm_result_new((int)nel,usenull?eo->nnull:20) ;
       vvalid=(byte *)malloc(nel) ; v_q=(float *)malloc(sizeof(float)*nel) ;
       v_nmean=(float *)calloc(nel,sizeof(float)) ;
       if( vres==NULL || vvalid==NULL || v_q==NULL || v_nmean==NULL ) ERROR_exit(PROGRAM_NAME ": cannot allocate event results") ;
       memset(vvalid,1,nel) ; vres->tail=PERM_TAIL_ONE ;
       for( ss=0 ; ss<vres->nperm ; ss++ ) vres->max_null[ss]=-FLT_MAX ;
       for( cc=0 ; cc<ncond ; cc++ ){
         for( gg=0 ; gg<ngroup ; gg++ ) for( tt=0 ; tt<nt ; tt++ ){
           int c2=0,j2 ;
           for( j2=0 ; j2<gcount[gg] ; j2++ ) c2+=hi[((size_t)cc*nsub+gmember[gg][j2])*nt+tt] ;
           vres->stat[((size_t)cc*ngroup+gg)*nt+tt]=(float)c2 ;
         }
         if( usenull ) for( ss=0 ; ss<eo->nnull ; ss++ ){
           int *off=sset->offset+(size_t)ss*nsub ;
           float mx=-FLT_MAX ;
           for( gg=0 ; gg<ngroup ; gg++ ) for( tt=0 ; tt<nt ; tt++ ){
             int c2=0,j2 ; size_t o=((size_t)cc*ngroup+gg)*nt+tt ;
             for( j2=0 ; j2<gcount[gg] ; j2++ ){
               int sj=gmember[gg][j2],u=tt+off[sj] ;
               if( u>=nt ) u-=nt ;
               c2+=hi[((size_t)cc*nsub+sj)*nt+u] ;
             }
             if( (float)c2>=vres->stat[o] ) vres->cnt_unc[o]++ ;
             if( ss>0 ) v_nmean[o]+=(float)c2/(float)(eo->nnull-1) ;
             if( (float)c2>mx ) mx=(float)c2 ;
           }
           if( mx>vres->max_null[ss] ) vres->max_null[ss]=mx ;
         }
       }
       if( usenull ){
         THD_perm_result_finish(vres,vvalid) ;
         THD_bh_fdr_masked((int)nel,vres->p_unc,vvalid,v_q) ;
       }
       free(hi) ; free(vt) ;
     }

     /* One per-frame table: subject-mean amplitude, then whichever amplitude-ISC
        and shared-event columns were requested.  Group-level ISC values repeat
        down each condition/group so the file stays one tidy table. */
     snprintf(name,sizeof(name),"%s.frames.1D",prefix) ; ffp=insync_open_text(name) ;
     fprintf(ffp,"Condition Group TR OrigTR MeanRSS") ;
     if( eo->rss ){ fprintf(ffp," RSS_ISC RSS_N") ; if( usenull ) fprintf(ffp," RSS_NullMean RSS_P") ; }
     if( eo->events ){
       fprintf(ffp," NHigh NSubj Frac") ;
       if( usenull ) fprintf(ffp," EvNullMean EvP EvZ EvQ EvPFWE EvZFWE") ;
     }
     fprintf(ffp,"\n") ;
     for( cc=0 ; cc<ncond ; cc++ ) for( gg=0 ; gg<ngroup ; gg++ ) for( tt=0 ; tt<nt ; tt++ ){
       size_t og=(size_t)cc*ngroup+gg,qq=og*nt+tt ;
       double m=0.0 ; int j2 ;
       for( j2=0 ; j2<gcount[gg] ; j2++ ) m+=rss[((size_t)cc*nsub+gmember[gg][j2])*nt+tt] ;
       fprintf(ffp,"%s %s %d %d %.9g",clabel[cc],glabel[gg],tt,tmap[tt],m/gcount[gg]) ;
       if( eo->rss ){
         fprintf(ffp," %.9g %d",r_obs[og],r_n[og]) ;
         if( usenull ) fprintf(ffp," %.9g %.9g",r_nmean[og],r_p[og]) ;
       }
       if( eo->events ){
         fprintf(ffp," %d %d %.6g",(int)vres->stat[qq],gcount[gg],vres->stat[qq]/gcount[gg]) ;
         if( usenull ) fprintf(ffp," %.9g %.9g %.9g %.9g %.9g %.9g",v_nmean[qq],vres->p_unc[qq],
                 vres->z_unc[qq],v_q[qq],vres->p_fwe[qq],vres->z_fwe[qq]) ;
       }
       fprintf(ffp,"\n") ;
     }
     fclose(ffp) ;
     if( !quiet ){
       INFO_message(PROGRAM_NAME ": wrote per-frame table to %s",name) ;
       if( eo->events )
         INFO_message(PROGRAM_NAME ": events are the top %d of %d frames in each subject",K,nt) ;
     }
     free(r_obs) ; free(r_nmean) ; free(r_p) ; free(r_n) ; free(v_q) ; free(v_nmean) ;
     if( eo->events ){ free(vvalid) ; THD_perm_result_free(vres) ; }
     insync_ws_free(&ws) ; free(rss) ;
   }

   if( ninvalid>0 ) WARNING_message(PROGRAM_NAME ": %lld edge group estimates were invalid and stored as zero",ninvalid) ;
   for( k=0 ; k<nroi ; k++ ) free(rlab[k]) ;
   free(rlab) ; free(ridx) ; free(rval) ; free(ei) ; free(ej) ; free(tmap) ; free(zdat) ;
   THD_timeshift_set_free(eset) ; THD_timeshift_set_free(sset) ;
   THD_roilist_free(rl) ; DSET_delete(aset) ;
}

/* M3 descriptive selection driver. The source store is loaded once; one
   selection's output maps and one persistent kernel per worker bound memory. */
static void *insync_select_alloc(size_t n,size_t width)
{
   void *p;
   if(width && n>SIZE_MAX/width) ERROR_exit("3dInSync: selection allocation overflow");
   p=calloc(n?n:1,width);
   if(!p) ERROR_exit("3dInSync: cannot allocate selection workspace");
   return p;
}

static void insync_select_path(char *path,size_t size,const char *prefix,int id,int oneD)
{
   if(snprintf(path,size,"%s.sel%06d.%s",prefix,id,oneD?"1D":"nii.gz")>=(int)size)
      ERROR_exit("3dInSync: selection output path too long");
}

/* Manual aggregates must retain their constituent estimates too. Xmat by_stim
   already supplies those. Components are keyed by parent/run/segment, with
   nominal geometry still allowed to differ between input conditions. */
static void insync_selection_children(INSYNC_schedule *s,const INSYNC_options *o)
{
   int original=s->nsegment,i,j,k,component=0;
   if(o->xmat_blocks) return;
   for(i=0;i<original;i++){
      INSYNC_segment base=s->segment[i]; int pieces=0,id;
      for(j=0;j<original;j++) if(s->segment[j].source==base.source && s->segment[j].selection_id==base.selection_id) pieces++;
      if(pieces<2) continue;
      for(j=0;j<i;j++) if(s->segment[j].selection_id==base.selection_id && s->segment[j].run==base.run && !strcmp(s->segment[j].segment,base.segment)) break;
      if(j<i) continue;
      /* Reuse an existing child only when its original geometry matches
         for every source/condition. Aggregate rows retain all parent links. */
      for(k=original;k<s->nsegment;k++){
         int candidate=s->segment[k].selection_id,match=1,q;
         if(s->segment[k].source!=base.source || s->segment[k].run!=base.run || s->segment[k].original_start!=base.original_start || s->segment[k].original_stop!=base.original_stop) continue;
         for(j=0;j<original && match;j++) if(s->segment[j].selection_id==base.selection_id && s->segment[j].run==base.run && !strcmp(s->segment[j].segment,base.segment)){
            for(q=original;q<s->nsegment;q++) if(s->segment[q].selection_id==candidate && s->segment[q].source==s->segment[j].source && s->segment[q].run==s->segment[j].run && s->segment[q].original_start==s->segment[j].original_start && s->segment[q].original_stop==s->segment[j].original_stop) break;
            if(q==s->nsegment) match=0;
         }
         if(match) break;
      }
      if(k<s->nsegment){
         id=s->segment[k].selection_id;
         for(j=0;j<original;j++) if(s->segment[j].selection_id==base.selection_id && s->segment[j].run==base.run && !strcmp(s->segment[j].segment,base.segment)) s->segment[j].component_id=id;
         continue;
      }
      if(s->nselection==INT_MAX) ERROR_exit("3dInSync: too many pool components");
      id=++s->nselection; component++;
      for(j=0;j<original;j++) if(s->segment[j].selection_id==base.selection_id && s->segment[j].run==base.run && !strcmp(s->segment[j].segment,base.segment)){
         INSYNC_segment e=s->segment[j]; char label[96]; void *grown;
         snprintf(label,sizeof(label),"sel%06d_component%06d",base.selection_id,component);
         s->segment[j].component_id=id;
         e.selection_id=id; e.parent_id=base.selection_id; e.aggregate=0; e.component_id=0;
         e.label=strdup(label); e.segment=strdup(e.segment);
         if(s->nsegment==INT_MAX || (size_t)s->nsegment+1>SIZE_MAX/sizeof(e)) ERROR_exit("3dInSync: too many pool rows");
         grown=realloc(s->segment,((size_t)s->nsegment+1)*sizeof(e));
         if(!grown || !e.label || !e.segment) ERROR_exit("3dInSync: cannot allocate pool components");
         s->segment=grown; s->segment[s->nsegment++]=e;
      }
   }
   for(i=0;i<original;i++){
      for(k=0,j=0;j<original;j++) if(s->segment[j].source==s->segment[i].source && s->segment[j].selection_id==s->segment[i].selection_id) k++;
      if(k>1) s->segment[i].aggregate=1;
   }
}

/* One worker retains one selection's corresponding edge/LOO values across
   blocks. A block is admitted only if all required components in all groups
   are finite; groups never silently summarize different block sets. */
typedef struct { float *value, *pair, *scratch, *values; unsigned char *keep; } INSYNC_block_work;

static void insync_block_selection(const INSYNC_options *o,const INSYNC_schedule *s,
   const INSYNC_src *src,INSYNC_kernel *kernel,INSYNC_block_work *work,float *data,
   int voxel,int nsub,int ngroup,const int *gcount,int **gmember,const int *sources,
   const byte *common,const int *rows,int nblock,float *stat,int *nv,int *complete,
   float **support,int support_offset,int gridvoxel,int *eligible,int *used,int *retained,float *pairout)
{
   int b,si,t,nkeep,g,j,good,ns=s->nsource,nt=s->ntime;
   size_t stride=o->method==INSYNC_METHOD_PAIRWISE?(size_t)nsub*nsub:(size_t)nsub;
   *eligible=*used=*retained=0;
   for(g=0;g<ngroup;g++){ stat[g]=NAN; nv[g]=complete[g]=0; }
   for(b=0;b<nblock;b++){
      const INSYNC_segment *e=s->segment+rows[b];
      nkeep=0; work->keep[b]=0;
      for(t=e->original_start;t<=e->original_stop;t++) if(common[t]){
         good=1;
         if(o->missing_policy==INSYNC_MISSING_COMMON) for(si=0;si<ns;si++) if(!isfinite(insync_store_get(src,si,voxel,t))){ good=0; break; }
         if(!good) continue;
         for(si=0;si<nsub;si++) data[(size_t)si*nt+nkeep]=insync_store_get(src,sources[si],voxel,t);
         nkeep++;
      }
      support[support_offset+3*b][gridvoxel]=(float)nkeep;
      if(nkeep<o->min_valid || (double)nkeep/(e->stop-e->start+1)<o->min_valid_frac) continue;
      for(si=0;si<nsub;si++) memmove(data+(size_t)si*nkeep,data+(size_t)si*nt,nkeep*sizeof(float));
      INSYNC_kernel_eval(kernel,nkeep,data,o->corr_metric,o->method,o->summary,ngroup,gcount,gmember,stat,nv,complete);
      good=1;
      for(g=0;g<ngroup;g++){
         size_t expected=o->method==INSYNC_METHOD_PAIRWISE?(size_t)gcount[g]*(gcount[g]-1)/2:(size_t)gcount[g];
         if((size_t)nv[g]!=expected) good=0;
      }
      if(!good) continue;
      if(pairout) memcpy(work->pair+(size_t)b*nsub*nsub,INSYNC_kernel_matrix(kernel),(size_t)nsub*nsub*sizeof(float));
      work->keep[b]=1; (*eligible)++; *retained+=nkeep;
      support[support_offset+3*b+1][gridvoxel]=1;
      memcpy(work->value+(size_t)b*stride,o->method==INSYNC_METHOD_PAIRWISE?INSYNC_kernel_matrix(kernel):INSYNC_kernel_loo(kernel),stride*sizeof(float));
   }
   /* Clear the last individual-block result, including when the aggregate
      fails. No stale statistics can leak into invalid aggregate maps. */
   for(g=0;g<ngroup;g++){ stat[g]=NAN; nv[g]=complete[g]=0; }
   if(!nblock || *eligible<o->min_blocks || (double)*eligible/nblock<o->min_block_frac || (!o->block_drop_common && *eligible!=nblock)){
      *retained=0;
      INSYNC_kernel_aggregate(kernel,0,work->value,work->keep,o->method,o->block_summary,o->summary,ngroup,gcount,gmember,stat,nv,complete);
      return;
   }
   *used=*eligible;
   for(j=0;j<nblock;j++) if(work->keep[j]){
      support[support_offset+3*j+2][gridvoxel]=1.0f/(*used);
   }
   INSYNC_kernel_aggregate(kernel,nblock,work->value,work->keep,o->method,o->block_summary,o->summary,ngroup,gcount,gmember,stat,nv,complete);
   if(pairout) for(si=0;si<nsub;si++) for(j=si+1;j<nsub;j++){
      int n=0,valid=1;
      for(b=0;b<nblock;b++) if(work->keep[b]){
         float x=work->pair[(size_t)b*nsub*nsub+(size_t)si*nsub+j];
         if(!isfinite(x)) valid=0;
         work->values[n++]=x;
      }
      pairout[(size_t)si*nsub+j]=valid?(n==1?work->values[0]:INSYNC_summary(n,work->values,o->block_summary,work->scratch,NULL)):NAN;
   }
}

static void insync_support_path(char *path,size_t size,const char *prefix,int id,int oneD)
{
   if(snprintf(path,size,"%s.sel%06d.blocks.%s",prefix,id,oneD?"1D":"nii.gz")>=(int)size) ERROR_exit("3dInSync: block support output path too long");
}

/* Numeric selection IDs avoid filename sanitization collisions. Brick labels
   are still checked because AFNI selectors can otherwise resolve ambiguously. */
static void insync_selection_labels_check(THD_3dim_dataset *d)
{
   int i,j;
   for(i=0;i<DSET_NVALS(d);i++) for(j=0;j<i;j++)
      if(!strcmp(DSET_BRICK_LABEL(d,i),DSET_BRICK_LABEL(d,j)))
         ERROR_exit("3dInSync: output brick label collision after sanitization/truncation: %s",DSET_BRICK_LABEL(d,i));
}

static void insync_selection_expand(float **maps,int nb,int nvox,int nr,THD_roilist *rl,const int *ids)
{
   int b,r,v;
   for(b=0;b<nb;b++) for(r=0;r<nr;r++) for(v=0;v<rl->vox[ids[r]].nar;v++)
      maps[b][rl->vox[ids[r]].ar[v]]=maps[b][nvox+r];
}

/* A selection may span runs or stimulus classes. Never label a multi-run
   aggregate with the run of an arbitrary first segment. */
static void insync_selection_identity(FILE *fp,const INSYNC_schedule *s,int id,int source)
{
   int i,run=-1,whole=0; const char *stim=NULL;
   for(i=0;i<s->nsegment;i++) if(s->segment[i].selection_id==id && s->segment[i].source==source){
      const INSYNC_segment *e=s->segment+i;
      if(e->run==0) whole=1;
      if(run==-1) run=e->run; else if(run!=e->run) run=0;
      if(!stim) stim=e->stimulus>=0?s->stimlabel[e->stimulus]:"NA"; else if(strcmp(stim,e->stimulus>=0?s->stimlabel[e->stimulus]:"NA")) stim="mixed";
   }
   fprintf(fp,"sel%06d\t",id);
   if(whole) fprintf(fp,"all\t"); else if(run>0) fprintf(fp,"%d\t",run); else fprintf(fp,"mixed\t");
   INSYNC_selection_token(fp,stim && *stim?stim:"NA"); fputc('\t',fp);
}

static void insync_selection_describe(FILE *bf,FILE *rf,FILE *mf,const INSYNC_schedule *s,
   int id,THD_3dim_dataset *d,const char *path,const char *kind,float **maps,
   int nr,int nvox,THD_roilist *rl,const int *ids,int nc,char **clabel,
   const float *pairs,int ns,const int *sources,int ng)
{
   int b,r,c,a,j,base=2*nc*ng,diff=ng==2?nc:0;
   for(b=0;b<DSET_NVALS(d);b++){
      fprintf(bf,"sel%06d\t%s\t",id,kind); INSYNC_selection_token(bf,path);
      fprintf(bf,"\t%d\t",b); INSYNC_selection_token(bf,DSET_BRICK_LABEL(d,b)); fputc('\n',bf);
      if(!rf) continue;
      if(!strcmp(kind,"block_support")){
         int off=0,i; c=0;
         for(c=0;c<nc;c++){
            for(i=0;i<s->nsegment;i++) if(s->segment[i].selection_id==id && s->segment[i].source==sources[c*ns]) off+=3;
            if(b<off) break;
         }
      } else if(b<base) c=b/(2*ng);
      else if(b<base+diff+nc) c=(b-base)%nc;
      else if(b<base+diff+nc+2*nc*ng) c=((b-base-diff-nc)%(nc*ng))/ng;
      else c=(b-base-diff-nc-2*nc*ng)%nc;
      for(r=0;r<nr;r++){
         int ri=ids[r]; char label[128];
         snprintf(label,sizeof(label),"ROI%d",rl->val[ri]);
         insync_selection_identity(rf,s,id,sources[c*ns]); INSYNC_selection_token(rf,clabel[c]);
         fprintf(rf,"\t%d\t",rl->val[ri]); INSYNC_selection_token(rf,rl->lab && rl->lab[ri]?rl->lab[ri]:label);
         fprintf(rf,"\t%d\t%s\t%d\t",rl->vox[ri].nar,kind,b); INSYNC_selection_token(rf,DSET_BRICK_LABEL(d,b));
         fprintf(rf,"\t%.9g\n",maps[b][nvox+r]);
      }
   }
   if(mf && pairs) for(r=0;r<nr;r++) for(c=0;c<nc;c++) for(a=0;a<ns;a++) for(j=a+1;j<ns;j++){
      const INSYNC_source *sa=s->source+sources[c*ns+a],*sb=s->source+sources[c*ns+j];
      int ri=ids[r]; char label[128]; float x=pairs[((size_t)r*nc+c)*ns*ns+(size_t)a*ns+j];
      snprintf(label,sizeof(label),"ROI%d",rl->val[ri]);
      insync_selection_identity(mf,s,id,sources[c*ns]); INSYNC_selection_token(mf,clabel[c]);
      fprintf(mf,"\t%d\t",rl->val[ri]); INSYNC_selection_token(mf,rl->lab && rl->lab[ri]?rl->lab[ri]:label);
      fputc('\t',mf); INSYNC_selection_token(mf,sa->subject); fputc('\t',mf); INSYNC_selection_token(mf,sb->subject);
      fputc('\t',mf); INSYNC_selection_token(mf,sa->group); fputc('\t',mf); INSYNC_selection_token(mf,sb->group);
      fprintf(mf,"\t%.9g\t%d\n",x,isfinite(x)!=0);
   }
   if(ferror(bf) || (rf && ferror(rf)) || (mf && ferror(mf))) ERROR_exit("3dInSync: failed writing selection tables");
}

static void insync_selection_label_bucket(THD_3dim_dataset *out,int ncond,int ngroup,char **clabel,char **glabel,int aggregate,int nout,int vslot,int pslot,int trslot,int dslot,int dvslot)
{
   int c,g;
      for(c=0;c<ncond;c++){
         char who[256],label[64];
         for(g=0;g<ngroup;g++){
            int cell=c*ngroup+g;
            if(ncond>1) snprintf(who,sizeof(who),"%s_%s",clabel[c],glabel[g]); else snprintf(who,sizeof(who),"%s",glabel[g]);
            insync_brick_label(label,sizeof(label),"ISC",who); EDIT_BRICK_LABEL(out,2*cell,label);
            insync_brick_label(label,sizeof(label),"N",who); EDIT_BRICK_LABEL(out,2*cell+1,label);
            insync_brick_label(label,sizeof(label),"Valid",who); EDIT_BRICK_LABEL(out,vslot+cell,label);
            insync_brick_label(label,sizeof(label),"PairComplete",who); EDIT_BRICK_LABEL(out,pslot+cell,label);
         }
         if(aggregate){
            const char *stem[]={"BlocksNominal","BlocksEligible","BlocksUsed"}; int k;
            for(k=0;k<3;k++){ if(ncond>1) insync_brick_label(label,sizeof(label),stem[k],clabel[c]); else MCW_strncpy(label,stem[k],sizeof(label)); EDIT_BRICK_LABEL(out,nout+k*ncond+c,label); }
         }
         if(ncond>1) insync_brick_label(label,sizeof(label),"ValidTR",clabel[c]); else strcpy(label,"ValidTR");
         EDIT_BRICK_LABEL(out,trslot+c,label);
         if(ngroup==2){
            if(ncond>1) snprintf(who,sizeof(who),"%s_%s-%s",clabel[c],glabel[0],glabel[1]); else snprintf(who,sizeof(who),"%s-%s",glabel[0],glabel[1]);
            insync_brick_label(label,sizeof(label),"ISC",who); EDIT_BRICK_LABEL(out,dslot+c,label);
            insync_brick_label(label,sizeof(label),"Valid",who); EDIT_BRICK_LABEL(out,dvslot+c,label);
         }
      }
   insync_selection_labels_check(out);
}

static void insync_selection_output_check(const char *path)
{
   struct stat st;
   if(!stat(path,&st)){
      if(!S_ISREG(st.st_mode)) ERROR_exit("3dInSync: output is not a regular file: %s",path);
      if(!THD_ok_overwrite()) ERROR_exit("3dInSync: output already exists: %s",path);
      if(access(path,W_OK)) ERROR_exit("3dInSync: output is not writable: %s",path);
   }
}

/* Compare destinations through their existing parent directory, including
   ./, absolute aliases, and symlinks. This is also valid for new files. */
static int insync_selection_same_path(const char *a,const char *b)
{
   char resolved[2][PATH_MAX],parent[PATH_MAX],*slash;
   const char *names[2]={a,b}; int i;
   for(i=0;i<2;i++){
      if(realpath(names[i],resolved[i])) continue;
      if(strlen(names[i])>=sizeof(parent)) ERROR_exit("3dInSync: output path too long");
      strcpy(parent,names[i]); slash=strrchr(parent,'/');
      if(slash){
         char base[PATH_MAX]; strcpy(base,slash+1);
         if(slash==parent) slash[1]=0; else *slash=0;
         if(!realpath(parent,resolved[i])) return !strcmp(a,b);
         if(strlen(resolved[i])+strlen(base)+2>sizeof(resolved[i])) ERROR_exit("3dInSync: output path too long");
         strcat(resolved[i],"/"); strcat(resolved[i],base);
      } else {
         if(!getcwd(resolved[i],sizeof(resolved[i]))) ERROR_exit("3dInSync: cannot resolve output directory");
         if(strlen(resolved[i])+strlen(parent)+2>sizeof(resolved[i])) ERROR_exit("3dInSync: output path too long");
         strcat(resolved[i],"/"); strcat(resolved[i],parent);
      }
   }
   return !strcmp(resolved[0],resolved[1]);
}

/* Full pooled-subject pair exports use their own all-modeled-edge validity.
   They do not inherit the descriptive LOO statistic or its reference failures.
   A single job's pair/support maps is resident; source samples are never reloaded. */
static void insync_selection_models(INSYNC_model *model,const INSYNC_options *o,
   const INSYNC_schedule *s,const INSYNC_src *src,const byte *common,int id,int aggregate,
   int nsub,int ncond,const int *sources,int nvox,int nvoxelwork,int nroi,
   const int *widx,THD_roilist *rois,const int *roi_ids,THD_3dim_dataset *reference,int nthread)
{
   int i,c,d,nb=0,nt=s->ntime,nseries=2*nsub,nedge=nsub*(nsub-1)/2;
   int nwork=o->roi_only?nroi:nvoxelwork,nmap=nvox+(o->roi_only?nroi:0);
   int *rows=insync_select_alloc(s->nsegment,sizeof(int)),*members=insync_select_alloc(nseries,sizeof(int));
   INSYNC_kernel **kernel=insync_select_alloc(nthread,sizeof(*kernel));
   float **data=insync_select_alloc(nthread,sizeof(*data)),**values=insync_select_alloc(nthread,sizeof(*values)),**scratch=insync_select_alloc(nthread,sizeof(*scratch));
   int **kept=insync_select_alloc(nthread,sizeof(*kept));
   for(i=0;i<s->nsegment;i++) if(s->segment[i].selection_id==id && s->segment[i].source==sources[0]) rows[nb++]=i;
   for(i=0;i<nseries;i++) members[i]=i;
   for(i=0;i<nthread;i++){
      kernel[i]=INSYNC_kernel_new(nseries,nt); if(!kernel[i]) ERROR_exit("3dInSync: cannot allocate export kernel");
      data[i]=insync_select_alloc((size_t)nseries*nt,sizeof(float));
      values[i]=insync_select_alloc((size_t)(aggregate?nb:1)*nedge,sizeof(float));
      scratch[i]=insync_select_alloc(2*(size_t)nb,sizeof(float)); kept[i]=insync_select_alloc(nb,sizeof(int));
   }
   for(c=0;c<ncond;c++) for(d=0;d<ncond;d++){
      int w,nvalid=0,nsupport=aggregate?3*nb:1;
      float **maps=insync_select_alloc(nedge,sizeof(*maps)),*mask=insync_select_alloc(nmap,sizeof(float)),**support=insync_select_alloc(nsupport,sizeof(*support));
      for(i=0;i<nedge;i++) maps[i]=insync_select_alloc(nmap,sizeof(float));
      for(i=0;i<nsupport;i++) support[i]=insync_select_alloc(nmap,sizeof(float));
#ifdef USE_OMP
#pragma omp parallel for reduction(+:nvalid) schedule(dynamic,8)
#endif
      for(w=0;w<nwork;w++){
         int th=0,block,si,t,a,j,p,eligible=0,nkeep=0,totalnom=0,nominal=0,firstpiece=0;
         int unit=w,grid=widx[w];
#ifdef USE_OMP
         th=omp_get_thread_num();
#endif
         /* ROI-only source/work indices already start at zero. */
         for(block=0;block<nb;block++){
            const INSYNC_segment *e=s->segment+rows[block];
            int good=1,nv,pc; float statistic; const float *matrix;
            nominal=e->original_stop-e->original_start+1; totalnom+=nominal;
            if(aggregate) nkeep=0;
            firstpiece=nkeep; kept[th][block]=0;
            for(t=e->original_start;t<=e->original_stop;t++) if(common[t]){
               int finite=1;
               if(o->missing_policy==INSYNC_MISSING_COMMON) for(si=0;si<s->nsource;si++) if(!isfinite(insync_store_get(src,si,unit,t))){ finite=0; break; }
               if(!finite) continue;
               for(si=0;si<nsub;si++){
                  data[th][(size_t)si*nt+nkeep]=insync_store_get(src,sources[c*nsub+si],unit,t);
                  data[th][(size_t)(nsub+si)*nt+nkeep]=insync_store_get(src,sources[d*nsub+si],unit,t);
               }
               nkeep++;
            }
            if(!aggregate && o->segment_center && nkeep>firstpiece) for(si=0;si<nseries;si++){
               double mean=0;
               for(t=firstpiece;t<nkeep;t++) mean+=data[th][(size_t)si*nt+t];
               mean/=nkeep-firstpiece;
               for(t=firstpiece;t<nkeep;t++) data[th][(size_t)si*nt+t]-=(float)mean;
            }
            if(!aggregate && block<nb-1) continue;
            support[aggregate?3*block:0][grid]=nkeep;
            if(nkeep<o->min_valid || (double)nkeep/(aggregate?nominal:totalnom)<o->min_valid_frac) continue;
            for(si=0;si<nseries;si++) memmove(data[th]+(size_t)si*nkeep,data[th]+(size_t)si*nt,nkeep*sizeof(float));
            INSYNC_kernel_eval(kernel[th],nkeep,data[th],o->corr_metric,INSYNC_METHOD_PAIRWISE,INSYNC_SUMMARY_MEDIAN,1,&nseries,&members,&statistic,&nv,&pc);
            matrix=INSYNC_kernel_matrix(kernel[th]);
            for(p=0,a=0;a<nsub;a++) for(j=a+1;j<nsub;j++,p++){
               float r=matrix[(size_t)a*nseries+nsub+j];
               values[th][(size_t)(aggregate?block:0)*nedge+p]=r;
               if(!isfinite(r)) good=0;
            }
            if(good){
               kept[th][aggregate?block:0]=1; eligible++;
               if(aggregate) support[3*block+1][grid]=1;
            }
         }
         if(!eligible || (aggregate && (eligible<o->min_blocks || (double)eligible/nb<o->min_block_frac || (!o->block_drop_common && eligible!=nb)))) continue;
         for(p=0;p<nedge;p++){
            float r;
            if(aggregate){
               int n=0;
               for(block=0;block<nb;block++) if(kept[th][block]) scratch[th][n++]=values[th][(size_t)block*nedge+p];
               r=n==1?scratch[th][0]:INSYNC_summary(n,scratch[th],o->block_summary,scratch[th]+nb,NULL);
            } else r=values[th][p];
            maps[p][grid]=insync_fisher_z(r);
         }
         mask[grid]=1;
         if(aggregate) for(block=0;block<nb;block++) if(kept[th][block]) support[3*block+2][grid]=1.f/eligible;
         if(o->roi_only) nvalid+=rois->vox[roi_ids[w]].nar; else nvalid++;
      }
      if(o->roi_only){
         float *mm[1]={mask};
         insync_selection_expand(maps,nedge,nvox,nroi,rois,roi_ids);
         insync_selection_expand(mm,1,nvox,nroi,rois,roi_ids);
         insync_selection_expand(support,nsupport,nvox,nroi,rois,roi_ids);
      }
      INSYNC_model_job(model,id,c,d,reference,maps,mask,support,nsupport,nvalid,rows);
      if(!o->quiet) INFO_message("3dInSync: model sel%06d conditions %d/%d: %d pairs, %d/%d grid voxels valid",id,c+1,d+1,nedge,nvalid,nvox);
      for(i=0;i<nedge;i++) free(maps[i]);
      for(i=0;i<nsupport;i++) free(support[i]);
      free(maps); free(support); free(mask);
   }
   for(i=0;i<nthread;i++){ INSYNC_kernel_free(kernel[i]); free(data[i]); free(values[i]); free(scratch[i]); free(kept[i]); }
   free(kernel); free(data); free(values); free(scratch); free(kept); free(members); free(rows);
}

static float insync_inference_read(const void *source,int subject,int unit,int time)
{ return insync_store_get((const INSYNC_src *)source,subject,unit,time); }

static int insync_run_selections(const INSYNC_options *o,THD_datatable *tab,int argc,char **argv)
{
   INSYNC_infer *inference=NULL;
   INSYNC_model *model=NULL;
   INSYNC_progress selection_progress;
   INSYNC_options effective=*o;
   INSYNC_schedule *s;
   INSYNC_src src={0};
   THD_roilist *rois=NULL;
   int nroi=0,nvoxelwork=0,nmap,*roi_ids=NULL,**workvox=NULL,*worknv=NULL;
   char roipath[THD_MAX_NAME],brickpath[THD_MAX_NAME];
   FILE *roifp=NULL,*matrixfp=NULL,*brickfp=NULL;
   /* ROI means are computed from original float input, consistently in both
      atlas modes. Retain one float store rather than quantizing parcel means. */
   if(o->atlas_name && o->use_half){
      effective.use_half=0; o=&effective;
      if(!o->quiet) INFO_message("3dInSync: atlas selection analysis stores voxel/ROI series as float32 (ignoring -float16)");
   }
   INSYNC_kernel **worker;
   INSYNC_block_work *block_worker;
   int maxblocks=1,has_aggregates=0,*is_aggregate,*block_rows,*block_count,*block_offset;
   size_t component_stride;
   char supportpath[THD_MAX_NAME],supporttable[THD_MAX_NAME];
   FILE *supportfp=NULL;
   THD_3dim_dataset *first,*d,*out;
   char **glabel=NULL,**clabel=NULL,path[THD_MAX_NAME],indexpath[THD_MAX_NAME],out_space[THD_MAX_NAME];
   int out_view=-1;
   int ngroup=0,ncond=0,nsub,nvox,nwork=0,ns,nt,nthread=1,nout,oneD;
   int i,j,c,g,t,w,id,basecount,trslot,vslot,pslot,dslot=-1,dvslot=-1;
   int *widx,*vmap,*source_at,*gcount,**gmember,*time_at,*piece_at,*nominal,*length;
   byte *common,*used,*mask=NULL;
   float **packed,**stats; int **counts,**complete;
   FILE *index;
   double estimate,limit,system,schedule_bytes=0;
   long long bad=LLONG_MAX;
   if(o->zcensor_warn_given && !o->do_zcensor) ERROR_exit("3dInSync: -zcensor_warn requires -zcensor");
   s=INSYNC_schedule_build(o,tab); /* header, schedule and censor preflight */
   insync_selection_children(s,o);
   ns=s->nsource; nt=s->ntime;
   is_aggregate=insync_select_alloc((size_t)s->nselection+1,sizeof(int));
   for(i=0;i<s->nsegment;i++) if(s->segment[i].aggregate && !o->combine_pool) is_aggregate[s->segment[i].selection_id]=1;
   for(id=1;id<=s->nselection;id++) if(is_aggregate[id]){
      has_aggregates=1;
      for(i=0;i<ns;i++){
         int nb=0; for(j=0;j<s->nsegment;j++) if(s->segment[j].selection_id==id && s->segment[j].source==i) nb++;
         if(nb>maxblocks) maxblocks=nb;
      }
   }
   if(!has_aggregates && (o->block_summary_given || o->block_missing_given || o->min_blocks_given || o->min_block_frac_given || o->block_weight_given)) ERROR_exit("3dInSync: block-summary options require an aggregate selection (multiple manual segments or Xmat by_stim)");
   /* Preserve first-appearance condition/group order, including difference sign. */
   for(i=0;i<tab->nrow;i++) for(j=0;j<ns;j++) if(s->source[j].row==i){
      if(insync_find_group(s->source[j].group,glabel,ngroup)<0) insync_add_group(s->source[j].group,&glabel,&ngroup);
      if(insync_find_group(s->source[j].condition,clabel,ncond)<0) insync_add_group(s->source[j].condition,&clabel,&ncond);
   }
   nsub=ns/ncond;
   if((size_t)nsub*(nsub-1)/2>INT_MAX) ERROR_exit("3dInSync: subject-pair count exceeds supported integer range");
   component_stride=o->method==INSYNC_METHOD_PAIRWISE?(size_t)nsub*nsub:(size_t)nsub;
   if(component_stride>SIZE_MAX/sizeof(float)/(size_t)maxblocks) ERROR_exit("3dInSync: block workspace size overflow");
   source_at=insync_select_alloc(ns,sizeof(int));
   gcount=insync_select_alloc(ngroup,sizeof(int)); gmember=insync_select_alloc(ngroup,sizeof(int *));
   for(g=0;g<ngroup;g++) gmember[g]=insync_select_alloc(nsub,sizeof(int));
   for(i=0;i<ns;i++){
      c=insync_find_group(s->source[i].condition,clabel,ncond);
      source_at[c*nsub+i/ncond]=i;
      if(i%ncond==0){ g=insync_find_group(s->source[i].group,glabel,ngroup); gmember[g][gcount[g]++]=i/ncond; }
   }
   if((size_t)ncond*(5*(size_t)ngroup+6)>INT_MAX || (size_t)ncond*maxblocks>INT_MAX/3) ERROR_exit("3dInSync: too many output bricks");
   basecount=2*ncond*ngroup;
   nout=basecount;
   if(ngroup==2){ dslot=nout; nout+=ncond; }
   trslot=nout; nout+=ncond; vslot=nout; nout+=ncond*ngroup;
   pslot=nout; nout+=ncond*ngroup;
   if(ngroup==2){ dvslot=nout; nout+=ncond; }
   first=THD_open_dataset(s->source[0].file); CHECK_OPEN_ERROR(first,s->source[0].file);
   oneD=DSET_IS_1D(first) || (DSET_NY(first)==1 && DSET_NZ(first)==1);
   { THD_3dim_dataset *labels=EDIT_empty_copy(first);
      EDIT_dset_items(labels,ADN_nvals,nout+(has_aggregates?3*ncond:0),ADN_none);
      insync_selection_label_bucket(labels,ncond,ngroup,clabel,glabel,has_aggregates,nout,vslot,pslot,trslot,dslot,dvslot);
      DSET_delete(labels);
   }
   /* Validate all result paths before loading data. */
   if(snprintf(indexpath,sizeof(indexpath),"%s.results.tsv",o->prefix)>=(int)sizeof(indexpath)) ERROR_exit("3dInSync: output path too long");
   insync_selection_output_check(indexpath);
   for(id=1;id<=s->nselection;id++){
      insync_select_path(path,sizeof(path),o->prefix,id,oneD);
      insync_selection_output_check(path);
      if(is_aggregate[id]){
         insync_support_path(path,sizeof(path),o->prefix,id,oneD);
         insync_selection_output_check(path);
      }
   }
   if(has_aggregates){
      if(snprintf(supporttable,sizeof(supporttable),"%s.block_support.tsv",o->prefix)>=(int)sizeof(supporttable)) ERROR_exit("3dInSync: block support path too long");
      insync_selection_output_check(supporttable);
   }
   {
      const char *suffix[]={"selections.tsv","selection_summary.txt","common_censor.1D","inference.tsv","inference_draws.tsv","inference_roi.tsv","temporal_draws.tsv","temporal_support.tsv"};
      for(i=0;i<3;i++){
         if(snprintf(path,sizeof(path),"%s.%s",o->prefix,suffix[i])>=(int)sizeof(path)) ERROR_exit("3dInSync: output path too long");
         insync_selection_output_check(path);
      }
      /* Existing parent directory must be writable; no result is created yet. */
      MCW_strncpy(path,indexpath,sizeof(path));
      { char *slash=strrchr(path,'/'); if(slash){ if(slash==path) slash[1]=0; else *slash=0; } else strcpy(path,"."); }
      if(access(path,W_OK|X_OK)) ERROR_exit("3dInSync: output directory is not writable: %s",path);
   }
   if(snprintf(brickpath,sizeof(brickpath),"%s.bricks.tsv",o->prefix)>=(int)sizeof(brickpath)) ERROR_exit("3dInSync: brick index path too long");
   insync_selection_output_check(brickpath);
   if(o->atlas_name){
      if(snprintf(roipath,sizeof(roipath),"%s.roi.tsv",o->prefix)>=(int)sizeof(roipath)) ERROR_exit("3dInSync: ROI path too long");
      insync_selection_output_check(roipath);
   }
   if(o->matrix_name){
      if(strlen(o->matrix_name)>=sizeof(path)) ERROR_exit("3dInSync: matrix output path too long");
      for(i=0;i<ns;i++) if(insync_selection_same_path(s->source[i].file,o->matrix_name)) ERROR_exit("3dInSync: matrix output collides with source input");
      if(insync_selection_same_path(o->atlas_name,o->matrix_name) || (o->maskname && insync_selection_same_path(o->maskname,o->matrix_name))) ERROR_exit("3dInSync: matrix output collides with atlas/mask input");
      const char *suffix[]={"results.tsv","bricks.tsv","roi.tsv","block_support.tsv","selections.tsv","selection_summary.txt","common_censor.1D","inference.tsv","inference_draws.tsv","inference_roi.tsv","temporal_draws.tsv","temporal_support.tsv"};
      insync_selection_output_check(o->matrix_name);
      for(i=0;i<(int)(sizeof(suffix)/sizeof(suffix[0]));i++){
         snprintf(path,sizeof(path),"%s.%s",o->prefix,suffix[i]);
         if(insync_selection_same_path(path,o->matrix_name)) ERROR_exit("3dInSync: matrix output collides with bundle path: %s",path);
      }
      for(id=1;id<=s->nselection;id++){
         insync_select_path(path,sizeof(path),o->prefix,id,oneD);
         if(insync_selection_same_path(path,o->matrix_name)) ERROR_exit("3dInSync: matrix output collides with bucket");
         insync_support_path(path,sizeof(path),o->prefix,id,oneD);
         if(insync_selection_same_path(path,o->matrix_name)) ERROR_exit("3dInSync: matrix output collides with block support");
         snprintf(path,sizeof(path),"%s.sel%06d.infer.nii.gz",o->prefix,id);
         if(insync_selection_same_path(path,o->matrix_name)) ERROR_exit("3dInSync: matrix output collides with inference bucket");
      }
      MCW_strncpy(path,o->matrix_name,sizeof(path));
      { char *slash=strrchr(path,'/'); if(slash){ if(slash==path) slash[1]=0; else *slash=0; } else strcpy(path,"."); }
      if(access(path,W_OK|X_OK)) ERROR_exit("3dInSync: matrix output directory is not writable: %s",path);
   }
   model=INSYNC_model_preflight(o,tab,s,nsub,ncond,source_at,clabel);
   nvox=DSET_NVOX(first);
   if(o->maskname){
      int view; char space[THD_MAX_NAME];
      if(insync_template_space(o->maskname,&view,space) && view!=first->view_type){ out_view=view; MCW_strncpy(out_space,space,sizeof(out_space)); }

      d=THD_open_dataset(o->maskname); CHECK_OPEN_ERROR(d,o->maskname); DSET_load(d); CHECK_LOAD_ERROR(d);
      mask=THD_makemask(d,0,0.5f,0.0f); DSET_delete(d);
      if(!mask) ERROR_exit("3dInSync: cannot build mask");
   }
   vmap=insync_select_alloc(nvox,sizeof(int)); widx=insync_select_alloc(nvox,sizeof(int));
   for(w=0;w<nvox;w++){ vmap[w]=-1; if(!mask || mask[w]){ vmap[w]=nwork; widx[nwork++]=w; } }
   if(!nwork) ERROR_exit("3dInSync: empty analysis mask");
   nvoxelwork=nwork;
   if(o->atlas_name){
      THD_3dim_dataset *aset=NULL;
      int r,k,view; char space[THD_MAX_NAME];
      rois=insync_open_atlas(o->atlas_name,o->roi_sel,first,&aset);
      if(!o->maskname && insync_template_space(o->atlas_name,&view,space) && view!=first->view_type){ out_view=view; MCW_strncpy(out_space,space,sizeof(out_space)); }
      roi_ids=insync_select_alloc(rois->nroi,sizeof(int));
      for(r=0;r<rois->nroi;r++){
         int kept=0;
         for(k=0;k<rois->vox[r].nar;k++) if(!mask || mask[rois->vox[r].ar[k]]) rois->vox[r].ar[kept++]=rois->vox[r].ar[k];
         rois->vox[r].nar=kept;
         if(kept) roi_ids[nroi++]=r;
      }
      DSET_delete(aset);
      if(!nroi) ERROR_exit("3dInSync: no atlas ROI overlaps the analysis mask");
      if(nroi>INT_MAX-nvox) ERROR_exit("3dInSync: too many ROI work units");
      if(o->roi_only) nvoxelwork=0;
      nwork=nvoxelwork+nroi;
      widx=realloc(widx,(size_t)(nvox+nroi)*sizeof(int));
      if(!widx) ERROR_exit("3dInSync: cannot allocate ROI work index");
      workvox=insync_select_alloc(nwork,sizeof(int *)); worknv=insync_select_alloc(nwork,sizeof(int));
      for(w=0;w<nvoxelwork;w++){ workvox[w]=widx+w; worknv[w]=1; }
      for(r=0;r<nroi;r++){
         w=nvoxelwork+r; widx[w]=nvox+r;
         workvox[w]=rois->vox[roi_ids[r]].ar; worknv[w]=rois->vox[roi_ids[r]].nar;
      }
   }
   nmap=nvox+nroi;
   if(o->matrix_name && (size_t)nroi>SIZE_MAX/sizeof(float)/(size_t)ncond/(size_t)nsub/(size_t)nsub) ERROR_exit("3dInSync: ROI matrix workspace overflow");

#ifdef USE_OMP
   nthread=omp_get_max_threads();
#endif
   { INSYNC_infer_grid grid={.nsub=nsub,.ncond=ncond,.ngroup=ngroup,.nwork=nwork,.nvox=nvox,.nthread=nthread,
        .nvoxelwork=nvoxelwork,.nroi=nroi,.source_at=source_at,.gcount=gcount,.widx=widx,.roi_ids=roi_ids,
        .gmember=gmember,.clabel=clabel,.glabel=glabel,.rois=rois};
     inference=INSYNC_infer_plan(o,s,&grid);
   }
   /* Include one full input while loading and one full-grid result while
      writing. Unlike per-window driver calls, input storage is shared. */
   estimate=(double)ns*nwork*(nt*(o->use_half?2.:4.)+(o->use_half?8.:0.))
            +(double)nvox*nt*16+(double)nwork*nt*4
            +(double)nout*nmap*4+(double)nvox*8
            +(double)ns*nt+(double)ncond*nt*8
            +(double)nthread*(8.*nsub*nt+8.*nsub*nsub+32.*nt+20.*ngroup+8.*nsub)
            +(has_aggregates?((double)nthread*maxblocks*(component_stride*sizeof(float)+1.)+12.*nmap*ncond*(maxblocks+1)):0.)
            +(double)s->nsegment*sizeof(INSYNC_segment)+(double)s->nsource*sizeof(INSYNC_source)
            +(double)nwork*(sizeof(int *)+sizeof(int))
            +(o->matrix_name?4.*nroi*ncond*nsub*nsub+4.*nthread*(maxblocks*(double)nsub*nsub+2.*maxblocks):0.);
   schedule_bytes=(double)s->requested_capacity*sizeof(INSYNC_segment);
   for(i=0;i<s->nsegment;i++) schedule_bytes+=strlen(s->segment[i].label)+strlen(s->segment[i].segment)+2;
   for(i=0;i<s->nrequested;i++) schedule_bytes+=strlen(s->requested[i].label)+strlen(s->requested[i].segment)+2;
   for(i=0;i<ns;i++) schedule_bytes+=strlen(s->source[i].file)+strlen(s->source[i].subject)+strlen(s->source[i].condition)+strlen(s->source[i].group)+4;
   for(i=0;i<tab->nrow;i++) for(j=0;j<tab->ncol;j++) schedule_bytes+=2.*(strlen(DT_CELL(tab,i,j))+1+sizeof(char *));
   for(i=0;i<s->ndesign;i++){
      THD_xmat *x=s->design[i];
      schedule_bytes+=sizeof(*x)+4.*x->nrow*x->ncol+4.*(x->nrow+x->nrun)+x->nstim*sizeof(THD_xmat_stim);
      for(j=0;j<x->ncol;j++) schedule_bytes+=sizeof(char *)+strlen(x->collab[j])+1;
      for(j=0;j<x->nattr;j++) schedule_bytes+=2*sizeof(char *)+strlen(x->attr_name[j])+strlen(x->attr_value[j])+2;
   }
   estimate+=schedule_bytes+INSYNC_infer_memory(inference);
   if(model){
      double pairs=(double)nsub*(nsub-1)/2;
      double paircost=4.*(nvox+(o->roi_only?nroi:0))*(pairs+1.+3.*maxblocks)
         +nthread*(16.*nsub*nt+32.*nsub*nsub+32.*nt+maxblocks*(4.*pairs+12.));
      estimate+=paircost;
      if(!o->quiet) INFO_message("3dInSync: model export memory %.0f bytes; one selection/condition-pair job resident",paircost);
   }
   system=(double)AFNI_get_memsize();
   limit=o->memory_limit_given?o->memory_limit_gib*1073741824.:(system>0?.80*system:0);
   if(!o->quiet){
      INFO_message("3dInSync: selection ledger: sources %.0f bytes; result/support %.0f bytes; ROI matrices %.0f bytes; schedule rows %d (metadata estimate %.0f bytes); batch size 1; voxel pair-map batch size %d",
         (double)ns*nwork*(nt*(o->use_half?2.:4.)+(o->use_half?8.:0.)),
         4.*nmap*(nout+(has_aggregates?3.*ncond*(maxblocks+1):0.)),
         o->matrix_name?4.*nroi*ncond*nsub*nsub:0.,s->nsegment,(double)s->nsegment*sizeof(INSYNC_segment)+schedule_bytes,model?1:0);
   }
   if(!o->quiet) INFO_message("3dInSync: selection memory estimate %.3f GiB; %d worker(s), one result bucket at a time",estimate/1073741824.,nthread);
   if(limit>0 && estimate>limit){
      if(!o->memory_override) ERROR_exit("3dInSync: selection memory estimate exceeds limit; reduce threads or use -memory_override");
      WARNING_message("3dInSync: selection memory limit exceeded; continuing with -memory_override");
   }
   if((size_t)nwork>SIZE_MAX/(size_t)nt/sizeof(float)) ERROR_exit("3dInSync: source store size overflow");
   src.ndset=ns; src.ntime_input=nt; src.nwork=nwork; src.half=o->use_half;
   if(src.half){ src.hst=insync_select_alloc(ns,sizeof(*src.hst)); src.hpar=insync_select_alloc(ns,sizeof(*src.hpar)); }
   else src.fst=insync_select_alloc(ns,sizeof(*src.fst));
   common=insync_select_alloc(nt,1); used=insync_select_alloc(nt,1); memset(common,1,nt);
   for(i=0;i<ns;i++) for(t=0;t<nt;t++) common[t]&=s->keep[(size_t)i*nt+t];
   for(i=0;i<ns;i++){
      int badw,badt,nzero=0;
      d=THD_open_dataset(s->source[i].file); CHECK_OPEN_ERROR(d,s->source[i].file); DSET_load(d); CHECK_LOAD_ERROR(d);
      if(o->do_zcensor) for(t=0;t<nt;t++){
         for(w=0;w<nvox;w++) if((!mask || mask[w]) && THD_get_voxel(d,w,t)!=0) break;
         if(w==nvox){ common[t]=0; nzero++; }
      }
      if(o->do_zcensor && !o->quiet) INFO_message("3dInSync: Subj %s condition %s: %d/%d all-zero volumes",s->source[i].subject,s->source[i].condition,nzero,nt);
      if(o->do_zcensor && 100.*nzero/nt>o->zcensor_warn) WARNING_message("3dInSync: Subj %s condition %s exceeds -zcensor_warn",s->source[i].subject,s->source[i].condition);
      insync_fill_one(d,i,&src,widx,workvox,worknv,o->missing_policy,&badw,&badt);
      DSET_delete(d);
   }
   for(i=0;i<s->nsegment;i++) for(t=s->segment[i].original_start;t<=s->segment[i].original_stop;t++) if(common[t]) used[t]=1;
   /* A temporal null can move any participating-run sample into a window. */
   if(o->temporal_mode) for(i=0;i<s->nsegment;i++){
      int r;
      for(r=0;r<s->nrun;r++){
         int begin=s->runstart[r],end=r+1<s->nrun?s->runstart[r+1]:nt;
         if(s->segment[i].original_start<end && s->segment[i].original_stop>=begin)
            for(t=begin;t<end;t++) used[t]=1;
      }
   }
   /* Strict missingness is checked across the requested union before any
      bucket is written, and only after common/zero-volume censoring. */
   if(o->missing_policy==INSYNC_MISSING_ERROR){
#ifdef USE_OMP
#pragma omp parallel for reduction(min:bad) schedule(static)
#endif
      for(w=0;w<nwork;w++){
         int source,time;
         for(source=0;source<ns;source++) for(time=0;time<nt;time++) if(used[time] && (!o->temporal_censor || s->keep[(size_t)source*nt+time]) && !isfinite(insync_store_get(&src,source,w,time))){
            long long code=((long long)w*ns+source)*nt+time;
            if(code<bad) bad=code;
         }
      }
      if(bad!=LLONG_MAX){
         int time=bad%nt,source=(bad/nt)%ns,voxel=widx[bad/nt/ns];
         if(voxel>=nvox) ERROR_exit("3dInSync: nonfinite selected input: ROI %d original TR %d Subj %s condition %s",rois->val[roi_ids[voxel-nvox]],time,s->source[source].subject,s->source[source].condition);
         ERROR_exit("3dInSync: nonfinite selected input: voxel %d original TR %d Subj %s condition %s",voxel,time,s->source[source].subject,s->source[source].condition);
      }
   }
   worker=insync_select_alloc(nthread,sizeof(*worker)); packed=insync_select_alloc(nthread,sizeof(*packed));
   stats=insync_select_alloc(nthread,sizeof(*stats)); counts=insync_select_alloc(nthread,sizeof(*counts)); complete=insync_select_alloc(nthread,sizeof(*complete));
   block_worker=insync_select_alloc(nthread,sizeof(*block_worker));
   for(i=0;i<nthread;i++){
      if(has_aggregates){ block_worker[i].value=insync_select_alloc((size_t)maxblocks*component_stride,sizeof(float)); block_worker[i].keep=insync_select_alloc(maxblocks,1); }
      if(o->matrix_name && has_aggregates){
         block_worker[i].pair=insync_select_alloc((size_t)maxblocks*nsub*nsub,sizeof(float));
         block_worker[i].scratch=insync_select_alloc(maxblocks,sizeof(float)); block_worker[i].values=insync_select_alloc(maxblocks,sizeof(float));
      }
      worker[i]=INSYNC_kernel_new(nsub,nt); if(!worker[i]) ERROR_exit("3dInSync: cannot allocate selection kernel");
      packed[i]=insync_select_alloc((size_t)nsub*nt,sizeof(float)); stats[i]=insync_select_alloc(ngroup,sizeof(float));
      counts[i]=insync_select_alloc(ngroup,sizeof(int)); complete[i]=insync_select_alloc(ngroup,sizeof(int));
   }
   time_at=insync_select_alloc((size_t)ncond*nt,sizeof(int)); piece_at=insync_select_alloc((size_t)ncond*nt,sizeof(int));
   nominal=insync_select_alloc(ncond,sizeof(int)); length=insync_select_alloc(ncond,sizeof(int));
   block_rows=insync_select_alloc((size_t)ncond*maxblocks,sizeof(int));
   block_count=insync_select_alloc(ncond,sizeof(int)); block_offset=insync_select_alloc(ncond,sizeof(int));
   index=fopen(indexpath,"w"); if(!index) ERROR_exit("3dInSync: cannot write %s",indexpath);
   fprintf(index,"# Status: running; only a final '# Status: complete' marks a finished bundle\n");
   if(fflush(index) || ferror(index)) ERROR_exit("3dInSync: failed initializing result index");
   if(inference) INSYNC_infer_begin(inference);
   if(model) INSYNC_model_begin(model);
   INSYNC_schedule_report(o,tab,s);
   snprintf(path,sizeof(path),"%s.common_censor.1D",o->prefix);
   { FILE *fp=insync_open_text(path); int failed;
      for(t=0;t<nt;t++) fprintf(fp,"%d\n",common[t]!=0);
      failed=ferror(fp); if(fclose(fp)) failed=1;
      if(failed) ERROR_exit("3dInSync: failed writing common censor");
   }
   brickfp=insync_open_text(brickpath);
   fprintf(brickfp,"SelectionID\tKind\tFile\tBrick\tLabel\n");
   if(o->atlas_name){
      roifp=insync_open_text(roipath);
      fprintf(roifp,"SelectionID\tRun\tStimulus\tCondition\tROI\tROI_Label\tNVoxel\tKind\tBrick\tLabel\tValue\n");
   }
   if(o->matrix_name){
      matrixfp=insync_open_text(o->matrix_name);
      fprintf(matrixfp,"SelectionID\tRun\tStimulus\tCondition\tROI\tROI_Label\tSubj1\tSubj2\tGroup1\tGroup2\tCorrelation\tValid\n");
   }
   fprintf(index,"# Method: %s; Metric: %s; Summary: %s; Combine: %s; SegmentCenter: %s\n",o->method==INSYNC_METHOD_PAIRWISE?"pairwise":"loo",o->corr_metric==SIM_SPEARMAN?"spearman":"pearson",o->summary==INSYNC_SUMMARY_MEDIAN?"median":"fisher_mean",o->combine_pool?"pool":"block_summary",o->segment_center?"mean":"none");
   fprintf(index,"# MinValid: %d; MinValidFraction: %.17g; fraction denominator: nominal selection length\n",o->min_valid,o->min_valid_frac);
   fprintf(index,"# Valid marks a finite descriptive group estimate; PairComplete marks a complete within-group pair graph, not cross-group export eligibility\n");
   fprintf(index,"# ValidTR reflects censoring and local common finite samples; manifest CensorKeptN excludes voxel-dependent missingness and zcensor; common_censor.1D includes zcensor\n");
   fprintf(index,"# BlockSummary: %s; BlockMissing: %s; BlockWeight: equal; MinBlocks: %d; MinBlockFraction: %.17g\n",o->block_summary==INSYNC_SUMMARY_MEDIAN?"median":"fisher_mean",o->block_drop_common?"drop_common":"require_all",o->min_blocks,o->min_block_frac);
   fprintf(index,"# Block summaries aggregate corresponding pair-r or LOO-subject values first, then group summary. Pair export transformation is Fisher-z of the final pair aggregate; 3dISC bundles are separate from native selection inference.\n");
   fprintf(index,"# BlocksUsed is the block count for EVERY contributing edge/LOO subject; all groups within a condition share the same support. Weights are 1/BlocksUsed on admitted blocks.\n");
   fprintf(index,"# BrickIndex: %s; ROI table: %s; ROI matrices: %s; N counts %s\n",brickpath,roifp?roipath:"NA",o->matrix_name?o->matrix_name:"NA",o->method==INSYNC_METHOD_PAIRWISE?"finite within-group pairs":"finite LOO subjects");
   if(model){ fprintf(index,"# ModelBundle: "); INSYNC_selection_token(index,model->directory); fputc('\n',index); }
   if(inference) fprintf(index,"# Inference: %s.inference.tsv; per-row completion describes descriptive results; global inference is pending until final Status: complete.\n",o->prefix);
   fprintf(index,"SelectionID\tBucket\tStatus\tBlockSupportBucket\tInferenceBucket\n"); fflush(index);
   if(has_aggregates){
      supportfp=insync_open_text(supporttable);
      fprintf(supportfp,"# Original-time block IDs and zero-based support brick selectors; maps are voxel-dependent. Eligible precedes aggregate gates; Weight is zero if the aggregate fails.\n");
      fprintf(supportfp,"SelectionID\tCondition\tBlockOrdinal\tSegmentID\tRun\tOriginalStartTR\tOriginalStopTR\tNominalN\tSupportFile\tRetainedTRBrick\tEligibleBrick\tWeightBrick\n");
   }
   insync_progress_init(&selection_progress,o->progress_mode,o->quiet,(long long)nwork*s->nselection,"selections","voxel/ROI work units");
   for(id=1;id<=s->nselection;id++){
      int aggregate=is_aggregate[id],nbout=nout+(aggregate?3*ncond:0),nsupport=0;
      float **maps=insync_select_alloc(nbout,sizeof(*maps)),**support=NULL;
      float *roipairs=NULL;
      int pieces=0;
      if(matrixfp){
         size_t z,n=(size_t)nroi*ncond*nsub*nsub;
         roipairs=insync_select_alloc(n,sizeof(float)); for(z=0;z<n;z++) roipairs[z]=NAN;
      }
      memset(block_count,0,ncond*sizeof(int));
      if(aggregate){
         for(c=0;c<ncond;c++){
            block_offset[c]=nsupport;
            for(i=0;i<s->nsegment;i++) if(s->segment[i].selection_id==id && s->segment[i].source==source_at[c*nsub]) block_rows[(size_t)c*maxblocks+block_count[c]++]=i;
            nsupport+=3*block_count[c];
         }
         support=insync_select_alloc(nsupport,sizeof(*support));
         for(i=0;i<nsupport;i++) support[i]=insync_select_alloc(nmap,sizeof(float));
      }
      for(i=0;i<nbout;i++) maps[i]=insync_select_alloc(nmap,sizeof(float));
      memset(nominal,0,ncond*sizeof(int)); memset(length,0,ncond*sizeof(int));
      for(c=0;c<ncond;c++) for(i=0;i<s->nsegment;i++){
         INSYNC_segment *e=s->segment+i;
         if(e->selection_id!=id || e->source!=source_at[c*nsub]) continue;
         pieces++; nominal[c]+=e->stop-e->start+1;
         for(t=e->original_start;t<=e->original_stop;t++) if(common[t]){
            size_t pos=(size_t)c*nt+length[c]++; time_at[pos]=t; piece_at[pos]=pieces;
         }
      }
#ifdef USE_OMP
#pragma omp parallel for schedule(dynamic,8)
#endif
      for(w=0;w<nwork;w++){
         int thread=0,ci,gg,si,k,pos,nkeep,previous,pbegin;
#ifdef USE_OMP
         thread=omp_get_thread_num();
#endif
         for(ci=0;ci<ncond;ci++){
            float *data=packed[thread],*st=stats[thread]; int *nv=counts[thread],*pc=complete[thread];
            float *pairout=(roipairs && w>=nvoxelwork)?roipairs+((size_t)(w-nvoxelwork)*ncond+ci)*nsub*nsub:NULL;
            if(aggregate){
               int eligible,used;
               insync_block_selection(o,s,&src,worker[thread],block_worker+thread,data,w,nsub,ngroup,gcount,gmember,source_at+ci*nsub,common,block_rows+(size_t)ci*maxblocks,block_count[ci],st,nv,pc,support,block_offset[ci],widx[w],&eligible,&used,&nkeep,pairout);
               maps[nout+ci][widx[w]]=block_count[ci];
               maps[nout+ncond+ci][widx[w]]=eligible; maps[nout+2*ncond+ci][widx[w]]=used;
               maps[trslot+ci][widx[w]]=nkeep;
            }else{
               nkeep=0; previous=-1; pbegin=0;
               /* Keep rows at capacity stride while filtering. Pack to nkeep
                  stride afterward, in ascending subject order (overlap safe). */
               for(k=0;k<length[ci];k++){
                  int original=time_at[(size_t)ci*nt+k],piece=piece_at[(size_t)ci*nt+k],finite=1;
                  if(o->missing_policy==INSYNC_MISSING_COMMON) for(si=0;si<ns;si++) if(!isfinite(insync_store_get(&src,si,w,original))){ finite=0; break; }
                  if(!finite) continue;
                  if(piece!=previous){
                     if(o->segment_center && nkeep>pbegin) for(si=0;si<nsub;si++){
                        double mean=0; for(pos=pbegin;pos<nkeep;pos++) mean+=data[(size_t)si*nt+pos]; mean/=nkeep-pbegin;
                        for(pos=pbegin;pos<nkeep;pos++) data[(size_t)si*nt+pos]-=(float)mean;
                     }
                     previous=piece; pbegin=nkeep;
                  }
                  for(si=0;si<nsub;si++) data[(size_t)si*nt+nkeep]=insync_store_get(&src,source_at[ci*nsub+si],w,original);
                  nkeep++;
               }
               if(o->segment_center && nkeep>pbegin) for(si=0;si<nsub;si++){
                  double mean=0; for(pos=pbegin;pos<nkeep;pos++) mean+=data[(size_t)si*nt+pos]; mean/=nkeep-pbegin;
                  for(pos=pbegin;pos<nkeep;pos++) data[(size_t)si*nt+pos]-=(float)mean;
               }
               maps[trslot+ci][widx[w]]=nkeep;
               if(nkeep<o->min_valid || !nominal[ci] || (double)nkeep/nominal[ci]<o->min_valid_frac) continue;
               for(si=0;si<nsub;si++) memmove(data+(size_t)si*nkeep,data+(size_t)si*nt,nkeep*sizeof(float));
               INSYNC_kernel_eval(worker[thread],nkeep,data,o->corr_metric,o->method,o->summary,ngroup,gcount,gmember,st,nv,pc);
               if(pairout) memcpy(pairout,INSYNC_kernel_matrix(worker[thread]),(size_t)nsub*nsub*sizeof(float));
            }
            for(gg=0;gg<ngroup;gg++){
               int cell=ci*ngroup+gg;
               if(isfinite(st[gg])){ maps[2*cell][widx[w]]=st[gg]; maps[vslot+cell][widx[w]]=1; }
               maps[2*cell+1][widx[w]]=nv[gg]; maps[pslot+cell][widx[w]]=pc[gg];
            }
            if(ngroup==2 && isfinite(st[0]) && isfinite(st[1])){
               maps[dslot+ci][widx[w]]=st[0]-st[1]; maps[dvslot+ci][widx[w]]=1;
            }
         }
         insync_progress_advance(&selection_progress);
      }
      if(aggregate && !o->quiet) for(c=0;c<ncond;c++){
         int elo=INT_MAX,ehi=0,ulo=INT_MAX,uhi=0;
         for(w=0;w<nwork;w++){
            int eligible=(int)maps[nout+ncond+c][widx[w]],used=(int)maps[nout+2*ncond+c][widx[w]];
            if(eligible<elo) elo=eligible;
            if(eligible>ehi) ehi=eligible;
            if(used<ulo) ulo=used;
            if(used>uhi) uhi=used;
         }
         INFO_message("3dInSync: sel%06d condition %s: %d nominal blocks; eligible %d..%d, used %d..%d across in-mask voxels (%s; equal weights)",id,clabel[c],block_count[c],elo,ehi,ulo,uhi,o->block_drop_common?"drop_common":"require_all");
      }
      insync_select_path(path,sizeof(path),o->prefix,id,oneD);
      out=EDIT_empty_copy(first);
      if(out_view>=0){ EDIT_dset_items(out,ADN_view_type,out_view,ADN_none); MCW_strncpy(out->atlas_space,out_space,THD_MAX_NAME); }
      EDIT_dset_items(out,ADN_prefix,path,ADN_nvals,nbout,ADN_ntt,0,ADN_brick_fac,NULL,
                      ADN_type,HEAD_FUNC_TYPE,ADN_func_type,FUNC_BUCK_TYPE,ADN_datum_all,MRI_float,ADN_none);
      tross_Copy_History(first,out); tross_Make_History(PROGRAM_NAME,argc,argv,out);
      for(i=0;i<nbout;i++){ EDIT_substitute_brick(out,i,MRI_float,maps[i]); EDIT_BRICK_TO_NOSTAT(out,i); }
      insync_selection_label_bucket(out,ncond,ngroup,clabel,glabel,aggregate,nout,vslot,pslot,trslot,dslot,dvslot);
      insync_selection_labels_check(out);
      insync_selection_describe(brickfp,roifp,matrixfp,s,id,out,path,"result",maps,nroi,nvox,rois,roi_ids,ncond,clabel,roipairs,nsub,source_at,ngroup);
      if(o->roi_only) insync_selection_expand(maps,nbout,nvox,nroi,rois,roi_ids);
      if(!DSET_write(out)) ERROR_exit("3dInSync: failed writing selection %d",id);
      if(!THD_is_file(path)) ERROR_exit("3dInSync: failed writing selection %d: %s",id,path);
      if(aggregate){
         THD_3dim_dataset *supportset=EDIT_empty_copy(out);
         insync_support_path(supportpath,sizeof(supportpath),o->prefix,id,oneD);
         EDIT_dset_items(supportset,ADN_prefix,supportpath,ADN_nvals,nsupport,ADN_ntt,0,ADN_brick_fac,NULL,ADN_datum_all,MRI_float,ADN_none);
         tross_Copy_History(out,supportset);
         for(i=0;i<nsupport;i++){ EDIT_substitute_brick(supportset,i,MRI_float,support[i]); EDIT_BRICK_TO_NOSTAT(supportset,i); }
         for(c=0;c<ncond;c++) for(j=0;j<block_count[c];j++){
            INSYNC_segment *e=s->segment+block_rows[(size_t)c*maxblocks+j]; int offset=block_offset[c]+3*j,k; char label[64];
            const char *stem[]={"RetainedTR","Eligible","Weight"};
            for(k=0;k<3;k++){ snprintf(label,sizeof(label),"C%d_B%d_%s",c+1,j+1,stem[k]); EDIT_BRICK_LABEL(supportset,offset+k,label); }
            fprintf(supportfp,"sel%06d\t",id); INSYNC_selection_token(supportfp,clabel[c]); fprintf(supportfp,"\t%d\t",j+1); INSYNC_selection_token(supportfp,e->segment);
            fprintf(supportfp,"\t%d\t%d\t%d\t%d\t",e->run,e->original_start,e->original_stop,e->stop-e->start+1); INSYNC_selection_token(supportfp,supportpath);
            fprintf(supportfp,"\t%d\t%d\t%d\n",offset,offset+1,offset+2);
         }
         insync_selection_describe(brickfp,roifp,NULL,s,id,supportset,supportpath,"block_support",support,nroi,nvox,rois,roi_ids,ncond,clabel,NULL,nsub,source_at,ngroup);
         if(o->roi_only) insync_selection_expand(support,nsupport,nvox,nroi,rois,roi_ids);
         if(!DSET_write(supportset) || !THD_is_file(supportpath)) ERROR_exit("3dInSync: failed writing block support for selection %d",id);
         DSET_delete(supportset); free(support);
         fflush(supportfp); if(ferror(supportfp)) ERROR_exit("3dInSync: failed writing block support table");
      }
      fprintf(index,"sel%06d\t",id); INSYNC_selection_token(index,path); fprintf(index,"\tcomplete\t"); INSYNC_selection_token(index,aggregate?supportpath:"NA"); fputc('\t',index);
      if(inference){ char inferpath[THD_MAX_NAME]; snprintf(inferpath,sizeof(inferpath),"%s.sel%06d.infer.nii.gz",o->prefix,id); INSYNC_selection_token(index,inferpath); } else fputs("NA",index);
      fputc('\n',index); fflush(index);
      if(ferror(index)) ERROR_exit("3dInSync: failed writing result index");
      if(!o->quiet) for(c=0;c<ncond;c++){
         float lo=FLT_MAX,hi=0;
         for(w=0;w<nwork;w++){
            float value=maps[trslot+c][widx[w]];
            if(value<lo) lo=value;
            if(value>hi) hi=value;
         }
         INFO_message("3dInSync: sel%06d condition %s: nominal %d TRs, common-censor retained %d TRs, final ValidTR %.0f..%.0f; work %lld/%lld",id,clabel[c],nominal[c],length[c],lo,hi,(long long)id*nwork*ncond,(long long)s->nselection*nwork*ncond);
      }
      if(inference) INSYNC_infer_selection(inference,id,aggregate,&src,insync_inference_read,common,out);
      if(model){
         THD_3dim_dataset *reference=EDIT_empty_copy(out); tross_Copy_History(out,reference);
         DSET_delete(out); free(maps); free(roipairs);
         insync_selection_models(model,o,s,&src,common,id,aggregate,nsub,ncond,source_at,nvox,nvoxelwork,nroi,widx,rois,roi_ids,reference,nthread);
         DSET_delete(reference);
      } else { DSET_delete(out); free(maps); free(roipairs); }
      if(!o->quiet) INFO_message("3dInSync: selection %d/%d wrote %s",id,s->nselection,path);
   }
   if(supportfp){ int failed=ferror(supportfp); if(fclose(supportfp)) failed=1; if(failed) ERROR_exit("3dInSync: failed closing block support table"); }
   if(inference) INSYNC_infer_finish(inference,brickfp);
   { FILE *fp[3]={brickfp,roifp,matrixfp}; int k;
      for(k=0;k<3;k++) if(fp[k]){ int failed=ferror(fp[k]); if(fclose(fp[k])) failed=1; if(failed) ERROR_exit("3dInSync: failed closing selection table"); }
   }
   if(model) INSYNC_model_finish(model);
   fprintf(index,"# Status: complete\n");
   { int failed=ferror(index); if(fclose(index)) failed=1; if(failed) ERROR_exit("3dInSync: failed closing result index"); }
   for(i=0;i<nthread;i++){ free(block_worker[i].value); free(block_worker[i].keep); free(block_worker[i].pair); free(block_worker[i].scratch); free(block_worker[i].values); INSYNC_kernel_free(worker[i]); free(packed[i]); free(stats[i]); free(counts[i]); free(complete[i]); }
   for(g=0;g<ngroup;g++){ free(gmember[g]); free(glabel[g]); } for(c=0;c<ncond;c++) free(clabel[c]);
   free(block_worker); free(block_rows); free(block_count); free(block_offset); free(is_aggregate);
   free(worker); free(packed); free(stats); free(counts); free(complete); free(gmember); free(gcount); free(glabel); free(clabel);
   free(time_at); free(piece_at); free(nominal); free(length); free(source_at); free(common); free(used); free(mask); free(vmap); free(widx);
   free(workvox); free(worknv); free(roi_ids); if(rois) THD_roilist_free(rois);
   insync_free_store(&src); DSET_delete(first); INSYNC_schedule_free(s); return 0;
}

/*! Run the analysis using a local snapshot of the supplied options. */
int INSYNC_run( const INSYNC_options *opts, THD_datatable *tab,
                int argc, char **argv )
{
   /* Core analysis and reporting options. */
   int method=opts->method ;
   int summary=opts->summary ;
   int show_table=opts->show_table ;
   int corr_metric=opts->corr_metric ;
   int missing_policy=opts->missing_policy ;
   int quiet=opts->quiet ;
   int progress_mode=opts->progress_mode ;
   int memory_override=opts->memory_override ;
   int memory_limit_given=opts->memory_limit_given ;
   double memory_limit_gib=opts->memory_limit_gib ;

   /* Resampling and inference options. */
   int nboot=opts->nboot ;
   int nperm=opts->nperm ;
   int do_exact=opts->do_exact ;
   int tail=opts->tail ;
   int temporal_mode=opts->temporal_mode ;
   int nnull=opts->nnull ;
   int min_shift=opts->min_shift ;
   int min_shift_given=opts->min_shift_given ;
   int temporal_tail=opts->temporal_tail ;
   int temporal_tail_given=opts->temporal_tail_given ;
   int ncondperm=opts->ncondperm ;
   int condition_exact=opts->condition_exact ;
   int condition_tail=opts->condition_tail ;
   long seed=opts->seed ;
   float boot_alpha=opts->boot_alpha ;

   /* Input column names, contrasts, and optional output names. */
   int condition_column_given=opts->condition_column_given ;
   int have_contrast=opts->have_contrast ;
   char *prefix=opts->prefix ;
   char *maskname=opts->maskname ;
   char *group_column=opts->group_column ;
   char *condition_column=opts->condition_column ;
   char *contrast_name0=opts->contrast_name0 ;
   char *contrast_name1=opts->contrast_name1 ;
   char *pair_prefix=opts->pair_prefix ;
   char *censor_name=opts->censor_name ;
   int icol_censor=tab?THD_datatable_column(tab,opts->censor_column):-1;
   int *source_censor_count=NULL;
   int do_zcensor=opts->do_zcensor ;
   int zcensor_warn_given=opts->zcensor_warn_given ;
   float zcensor_warn=opts->zcensor_warn ;
   char *atlas_name=opts->atlas_name ;
   char *roi_sel=opts->roi_sel ;
   char *matrix_name=opts->matrix_name ;
   int roi_only=opts->roi_only ;
   int use_half=opts->use_half ;
   INSYNC_edge_opts eo=opts->eo ;

   int contrast_c0=-1 ;
   int contrast_c1=-1 ;

   /* Parsed table, dataset, and mask state. */
   THD_datatable_index *cindex=NULL ;
   THD_3dim_dataset **dset=NULL ;
   THD_3dim_dataset *first=NULL ;
   THD_3dim_dataset *mset=NULL ;
   THD_3dim_dataset *out=NULL ;
   THD_3dim_dataset *pairout=NULL ;
   byte *mask=NULL ;
   int *rowmap=NULL ;
   int *time_index=NULL ;
   byte *censor_keep=NULL ;

   /* Subject, condition, and group organization. */
   int *group=NULL ;
   int *gcount=NULL ;
   int **gmember=NULL ;
   char **glabel=NULL ;
   char **clabel=NULL ;
   int nsub ;
   int ngroup=0 ;
   int ncond=1 ;
   int ndset ;
   int ntime=0 ;
   int ntime_input ;
   int nvox=0 ;
   int icol_group ;
   size_t nedge=0 ;

   /* Bootstrap, permutation, and temporal-null state. */
   THD_resample_set *bset=NULL ;
   PERM_scheme *pscheme=NULL ;
   PERM_scheme *cscheme=NULL ;
   PERM_set *pset=NULL ;
   PERM_set *cset=NULL ;
   PERM_result *presult=NULL ;
   PERM_result *cresult=NULL ;
   PERM_result *tresult=NULL ;
   THD_timeshift_set *tset=NULL ;
   THD_phase_set *phset=NULL ;
   unsigned char *tsneed=NULL ;
   byte *infer_valid=NULL ;
   byte *temporal_valid=NULL ;
   byte *condition_valid=NULL ;
   float *group_max_neg=NULL ;

   /* Output layout and backing arrays. */
   int nout ;
   int validtr_slot=-1 ;
   int diff_slot=-1 ;
   int boot_slot=-1 ;
   int perm_slot=-1 ;
   int temporal_slot=-1 ;
   int contrast_slot=-1 ;
   int condperm_slot=-1 ;
   int neffect=0 ;
   float **outval=NULL ;
   float **outwork=NULL ;
   int store_needed_late=0 ;
   float *qval=NULL ;
   float *temporal_qval=NULL ;
   float *condition_qval=NULL ;
   float *pairval=NULL ;
   size_t npairmap=0 ;

   /* Shared loop indices, progress state, and diagnostic counters. */
   int ii ;
   int jj ;
   int vv ;
   int kk ;
   int cc ;
   int ci ;
   int si ;
   int tt ;
   int nmask ;
   int nwork ;                 /* voxels, or ROIs with -roi_only, analyzed */
   int use_store=0 ;           /* 1: compact store; 0: datasets on full grid */
   int native_bytes=0 ;
   int *vmap=NULL ;            /* grid voxel -> work unit (-1 = none)      */
   int *widx=NULL ;            /* work unit -> grid voxel (compact mode)   */
   int nroi=0 ;
   int **roi_vox=NULL ;
   int *roi_nv=NULL ;
   int *roi_val=NULL ;
   char **roi_lab=NULL ;
   INSYNC_src src ;
   int out_view=-1 ;                     /* view of the output, see below */
   char out_space[THD_MAX_NAME] ;
   int *nzero=NULL ;
   byte *zero_time=NULL ;
   long long invalid_total=0 ;
   long long boot_invalid_total=0 ;
   long long temporal_invalid_total=0 ;
   long long condition_invalid_total=0 ;
   long long bad_data=LLONG_MAX ;
   INSYNC_progress progress ;

   /* Selection requests are never silently passed to static estimation. */
   if( opts->check_table ) return INSYNC_check_table(opts,tab) ;
   if( INSYNC_selection_requested(opts,tab) )
     return insync_run_selections(opts,tab,argc,argv) ;

   if(icol_censor>=0) for(jj=0;jj<tab->ncol;jj++) if(jj!=icol_censor && !strcasecmp(tab->cname[jj],opts->censor_column)) ERROR_exit("3dInSync: duplicate censor column '%s'",opts->censor_column);
   if(opts->censor_column_given && icol_censor<0) ERROR_exit("3dInSync: censor column '%s' absent",opts->censor_column);
   if(tab && icol_censor>=0 && (icol_censor==tab->icol_subj || icol_censor==tab->icol_input || icol_censor==THD_datatable_column(tab,group_column) || icol_censor==THD_datatable_column(tab,condition_column))) ERROR_exit("3dInSync: Censor must be a separate source column");
   if(icol_censor>=0 && !THD_ok_overwrite()){
      const char *suffix[]={"censor.tsv","common_censor.1D"}; char name[THD_MAX_NAME]; int q;
      for(q=0;q<2;q++){
         if(snprintf(name,sizeof(name),"%s.%s",prefix,suffix[q])>=(int)sizeof(name)) ERROR_exit("3dInSync: censor output path too long");
         if(THD_is_file(name)) ERROR_exit("3dInSync: censor output already exists: %s",name);
      }
   }
   /* Cross-option constraints protect estimands that require a common intact
      timeline or a paired condition design. */
   if( temporal_mode==INSYNC_TNULL_NONE && nnull>0 ) ERROR_exit(PROGRAM_NAME ": -nnull requires -temporal_null") ;
   if( temporal_mode!=INSYNC_TNULL_NONE && nnull<20 ) ERROR_exit(PROGRAM_NAME ": -temporal_null requires -nnull >= 20") ;
   if( temporal_mode!=INSYNC_TNULL_TIMESHIFT && min_shift_given && eo.nnull==0 ) ERROR_exit(PROGRAM_NAME ": -min_shift applies only to timeshift") ;
   if( (eo.edges || eo.rss || eo.events) && atlas_name==NULL )
     ERROR_exit(PROGRAM_NAME ": -edges/-edges_rss/-edges_events require -atlas") ;
   if( eo.matrix && !eo.edges ) ERROR_exit(PROGRAM_NAME ": -edges_matrix requires -edges") ;
   if( eo.nnull>0 && !(eo.edges || eo.rss || eo.events) )
     ERROR_exit(PROGRAM_NAME ": -edges_nnull requires -edges, -edges_rss, or -edges_events") ;
   if( eo.nnull>0 && (censor_name!=NULL || icol_censor>=0 || do_zcensor || missing_policy==INSYNC_MISSING_COMMON) )
     ERROR_exit(PROGRAM_NAME ": Censor/-censor/-zcensor/-missing common cannot be combined with "
                "-edges_nnull; circular shifts require an intact timeline") ;
   if( min_shift_given ) eo.min_shift=min_shift ;
   eo.seed=seed ;
   if( temporal_mode==INSYNC_TNULL_NONE && temporal_tail_given ) ERROR_exit(PROGRAM_NAME ": -temporal_tail requires -temporal_null") ;
   if( temporal_mode!=INSYNC_TNULL_NONE && (censor_name!=NULL || icol_censor>=0 || do_zcensor) )
     ERROR_exit(PROGRAM_NAME ": Censor/-censor/-zcensor cannot be combined with a temporal null; "
                "circular shifts/phases require an intact timeline") ;
   if( !isfinite(zcensor_warn) || zcensor_warn<0.0f || zcensor_warn>100.0f )
     ERROR_exit(PROGRAM_NAME ": -zcensor_warn must be between 0 and 100") ;
   if( !do_zcensor && zcensor_warn_given )
     ERROR_exit(PROGRAM_NAME ": -zcensor_warn requires -zcensor") ;
   if( temporal_mode!=INSYNC_TNULL_NONE && missing_policy==INSYNC_MISSING_COMMON )
     ERROR_exit(PROGRAM_NAME ": -missing common cannot be combined with a temporal null") ;
   if( roi_sel!=NULL && atlas_name==NULL )
     ERROR_exit(PROGRAM_NAME ": -roi_sel requires -atlas") ;
   if( matrix_name!=NULL && atlas_name==NULL )
     ERROR_exit(PROGRAM_NAME ": -save_matrix requires -atlas") ;
   if( roi_only && atlas_name==NULL )
     ERROR_exit(PROGRAM_NAME ": -roi_only requires -atlas") ;
   if( roi_only && (matrix_name!=NULL || eo.edges || eo.rss || eo.events) )
     ERROR_exit(PROGRAM_NAME ": -roi_only cannot be combined with -save_matrix or -edges*; "
                "run them without -roi_only") ;
   if( roi_only && use_half ){
     /* ROI series are a few megabytes at most; keep them in full precision. */
     if( !quiet ) INFO_message(PROGRAM_NAME ": -float16 is not needed with -roi_only; storing ROI series as 32-bit") ;
     use_half=0 ;
   }
   if( (ncondperm>0 || condition_exact) && !have_contrast )
     ERROR_exit(PROGRAM_NAME ": -ncondperm/-condition_exact require -condition_contrast C1 C2") ;
   if( !THD_filename_ok(prefix) ) ERROR_exit(PROGRAM_NAME ": illegal -prefix '%s'",prefix) ;
   if( pair_prefix!=NULL && !THD_filename_ok(pair_prefix) )
     ERROR_exit(PROGRAM_NAME ": illegal -save_pairwise prefix '%s'",pair_prefix) ;
   if( tab==NULL ) ERROR_exit(PROGRAM_NAME ": -dataTable or -dataTableFile is required") ;
   if( tab->icol_input<0 || tab->fname==NULL ) ERROR_exit(PROGRAM_NAME ": data table requires InputFile") ;
   if( show_table ) THD_datatable_print(tab,stdout) ;

   /* Convert either table shape into the same subject-major, condition-minor
      row map.  Long tables must be a complete Cartesian product. */
   cc=THD_datatable_column(tab,condition_column) ;
   if( cc>=0 ){
     char *cols[2]={"Subj",condition_column} ;
     if( tab->icol_subj<0 ) ERROR_exit(PROGRAM_NAME ": a long condition table requires Subj") ;
     if( cc==tab->icol_subj || cc==tab->icol_input )
       ERROR_exit(PROGRAM_NAME ": -condition_column must name a separate column") ;
     cindex=THD_datatable_index_columns(tab,2,cols,NULL,NULL) ;
     nsub=cindex->nlevel[0] ; ncond=cindex->nlevel[1] ; clabel=cindex->level[1] ;
     if( ncond<2 ) ERROR_exit(PROGRAM_NAME ": condition column '%s' has only one level",condition_column) ;
     rowmap=(int *)malloc(sizeof(int)*(size_t)cindex->ncell) ;
     memcpy(rowmap,cindex->row_of,sizeof(int)*(size_t)cindex->ncell) ;
     for( ii=0 ; ii<nsub ; ii++ ){
       int rr0=rowmap[ii*ncond] ;
       for( cc=1 ; cc<ncond ; cc++ ){
         int rr=rowmap[ii*ncond+cc] ;
         for( jj=0 ; jj<tab->ncol ; jj++ )
           if( jj!=tab->icol_subj && jj!=tab->icol_input && jj!=cindex->icol[1] && jj!=icol_censor &&
               strcmp(DT_CELL(tab,rr0,jj),DT_CELL(tab,rr,jj))!=0 )
             ERROR_exit(PROGRAM_NAME ": column '%s' changes within Subj %s across conditions",
                        tab->cname[jj],cindex->level[0][ii]) ;
       }
     }
   } else {
     if( condition_column_given ) ERROR_exit(PROGRAM_NAME ": -condition_column '%s' is absent",condition_column) ;
     nsub=tab->nrow ; ncond=1 ; clabel=(char **)calloc(1,sizeof(char *)) ;
     clabel[0]=strdup("All") ; rowmap=(int *)malloc(sizeof(int)*(size_t)nsub) ;
     for( ii=0 ; ii<nsub ; ii++ ) rowmap[ii]=ii ;
     for( ii=0 ; ii<nsub ; ii++ ) for( jj=ii+1 ; jj<nsub ; jj++ )
       if( strcmp(tab->subj[ii],tab->subj[jj])==0 )
         ERROR_exit(PROGRAM_NAME ": duplicate Subj '%s' in rows %d and %d",tab->subj[ii],ii+1,jj+1) ;
   }
   if( nsub<3 ) ERROR_exit(PROGRAM_NAME ": need at least 3 subjects; found %d",nsub) ;
   ndset=nsub*ncond ;

   if( have_contrast ){
     for( cc=0 ; cc<ncond ; cc++ ){
       if( strcmp(clabel[cc],contrast_name0)==0 ) contrast_c0=cc ;
       if( strcmp(clabel[cc],contrast_name1)==0 ) contrast_c1=cc ;
     }
     if( contrast_c0<0 || contrast_c1<0 || contrast_c0==contrast_c1 )
       ERROR_exit(PROGRAM_NAME ": condition contrast '%s' vs '%s' does not name two table levels",
                  contrast_name0,contrast_name1) ;
   }
   /* Text outputs are not protected by DSET_write(), so fail early under
      AFNI's normal no-overwrite policy before doing expensive computation. */
   if( !THD_ok_overwrite() ){
     char fname[THD_MAX_NAME] ;
     if( atlas_name!=NULL ){
       snprintf(fname,sizeof(fname),"%s.roi.1D",prefix) ;
       if( THD_is_file(fname) )
         ERROR_exit(PROGRAM_NAME ": ROI summary '%s' already exists",fname) ;
       if( matrix_name!=NULL && THD_is_file(matrix_name) )
         ERROR_exit(PROGRAM_NAME ": ROI matrix '%s' already exists",matrix_name) ;
     }
     if( eo.edges || eo.rss || eo.events ){
       if( eo.edges ){
         insync_edge_fname(fname,sizeof(fname),prefix,"edge",NULL,NULL) ;
         if( THD_is_file(fname) ) ERROR_exit(PROGRAM_NAME ": edge output '%s' already exists",fname) ;
       }
       if( eo.rss || eo.events ){
         insync_edge_fname(fname,sizeof(fname),prefix,"frames",NULL,NULL) ;
         if( THD_is_file(fname) ) ERROR_exit(PROGRAM_NAME ": edge output '%s' already exists",fname) ;
       }
     }
     if( pair_prefix!=NULL ){
       snprintf(fname,sizeof(fname),ncond==1?"%s.3dISC.txt":"%s.3dISC.all.txt",
                pair_prefix) ;
       if( THD_is_file(fname) )
         ERROR_exit(PROGRAM_NAME ": 3dISC table '%s' already exists",fname) ;
       if( ncond>1 ) for( ii=0 ; ii<ncond ; ii++ ) for( jj=0 ; jj<ncond ; jj++ ){
         char ca[64] ;
         char cb[64] ;
         insync_safe_token(ca,sizeof(ca),clabel[ii]) ;
         insync_safe_token(cb,sizeof(cb),clabel[jj]) ;
         snprintf(fname,sizeof(fname),"%s.%s__%s.3dISC.txt",pair_prefix,ca,cb) ;
         if( THD_is_file(fname) )
           ERROR_exit(PROGRAM_NAME ": 3dISC table '%s' already exists",fname) ;
       }
     }
   }

   /* Preserve order of first appearance for group labels.  That order also
      defines the sign of all two-group effects: first minus second. */
   group=(int *)calloc((size_t)nsub,sizeof(int)) ;
   icol_group=THD_datatable_column(tab,group_column) ;
   if( icol_group<0 ) insync_add_group("All",&glabel,&ngroup) ;
   else for( ii=0 ; ii<nsub ; ii++ ){
     char *lab=DT_CELL(tab,rowmap[ii*ncond],icol_group) ;
     int gg=insync_find_group(lab,glabel,ngroup) ;
     if( gg<0 ){ insync_add_group(lab,&glabel,&ngroup) ; gg=ngroup-1 ; }
     group[ii]=gg ;
   }
   gcount=(int *)calloc((size_t)ngroup,sizeof(int)) ;
   for( ii=0 ; ii<nsub ; ii++ ) gcount[group[ii]]++ ;
   for( kk=0 ; kk<ngroup ; kk++ ){
     if( gcount[kk]<3 ) ERROR_exit(PROGRAM_NAME ": group '%s' has %d subjects; need at least 3",glabel[kk],gcount[kk]) ;
     if( !quiet ) INFO_message(PROGRAM_NAME ": group %d '%s': %d subjects",
                               kk+1,glabel[kk],gcount[kk]) ;
   }
   if( (nperm>0 || do_exact) && ngroup!=2 ) ERROR_exit(PROGRAM_NAME ": group permutation requires exactly two groups") ;
   if( eo.edges && eo.matrix && !THD_ok_overwrite() ) for( cc=0 ; cc<ncond ; cc++ ) for( kk=0 ; kk<ngroup ; kk++ ){
     char fname[THD_MAX_NAME] ;
     insync_edge_fname(fname,sizeof(fname),prefix,"netcc",ncond>1?clabel[cc]:NULL,glabel[kk]) ;
     if( THD_is_file(fname) )
       ERROR_exit(PROGRAM_NAME ": edge matrix '%s' already exists",fname) ;
   }
   gmember=(int **)calloc((size_t)ngroup,sizeof(int *)) ;
   for( kk=0 ; kk<ngroup ; kk++ ){
     int pos=0 ;
     gmember[kk]=(int *)malloc(sizeof(int)*(size_t)gcount[kk]) ;
     for( ii=0 ; ii<nsub ; ii++ ) if( group[ii]==kk ) gmember[kk][pos++]=ii ;
   }

   /* Build synchronized resampling sets once.  Reusing the same draws at
      every voxel makes the max-statistic family correction well defined. */
   if( nboot>0 ){
     bset=THD_resample_set_build_stratified(nsub,nboot,seed,group) ;
     if( bset==NULL ) ERROR_exit(PROGRAM_NAME ": cannot build bootstrap draws") ;
   }
   if( nperm>0 || do_exact ){
     pscheme=THD_perm_scheme_new(nsub) ; pscheme->exchange=PERM_EE ; pscheme->seed=seed ;
     pscheme->exact=do_exact ; pscheme->nperm=(nperm>0)?nperm:10000 ;
     if( THD_perm_scheme_set_eqclass(pscheme,group)!=ngroup ) ERROR_exit(PROGRAM_NAME ": cannot encode groups") ;
     pset=THD_perm_set_build(pscheme) ;
     if( pset==NULL ) ERROR_exit(PROGRAM_NAME ": cannot build group permutations") ;
     /* eqclass enumerates labels in subject slots; invert to obtain subjects
        in fixed group slots. Otherwise exact assignments repeat/omit groups. */
     { int *inverse=(int *)malloc(sizeof(int)*(size_t)nsub),d,k;
       if(!inverse) ERROR_exit(PROGRAM_NAME ": cannot invert group assignments");
       for(d=0;d<pset->nperm;d++){
         int *p=pset->perm+(size_t)d*nsub;
         for(k=0;k<nsub;k++) inverse[p[k]]=k;
         memcpy(p,inverse,sizeof(int)*(size_t)nsub);
       }
       free(inverse);
     }
     if( !quiet ) INFO_message(PROGRAM_NAME ": using %d %s group assignments",
                               pset->nperm,pset->is_exact?"exact":"sampled") ;
   }
   if( ncondperm>0 || condition_exact ){
     cscheme=THD_perm_scheme_new(nsub) ; cscheme->exchange=PERM_ISE ;
     cscheme->seed=seed+104729L ; cscheme->exact=condition_exact ;
     cscheme->nperm=(ncondperm>0)?ncondperm:10000 ; cset=THD_perm_set_build(cscheme) ;
     if( cset==NULL ) ERROR_exit(PROGRAM_NAME ": cannot build within-subject condition swaps") ;
     if( !quiet ) INFO_message(PROGRAM_NAME ": using %d %s within-subject condition swaps",
                               cset->nperm,cset->is_exact?"exact":"sampled") ;
   }

   dset=(THD_3dim_dataset **)calloc((size_t)ndset,sizeof(THD_3dim_dataset *)) ;
   for( ii=0 ; ii<nsub ; ii++ ) for( cc=0 ; cc<ncond ; cc++ ){
     int dd=ii*ncond+cc ;
     int rr=rowmap[dd] ;
     const char *who=cindex?cindex->level[0][ii]:tab->subj[ii] ;
     dset[dd]=THD_open_dataset(tab->fname[rr]) ; CHECK_OPEN_ERROR(dset[dd],tab->fname[rr]) ;
     if( dd==0 ){
       first=dset[dd] ; ntime=DSET_NVALS(first) ; nvox=DSET_NVOX(first) ;
       if( ntime<3 ) ERROR_exit(PROGRAM_NAME ": input datasets need at least 3 time points") ;
     } else {
       if( !EQUIV_GRIDS(first,dset[dd]) ) ERROR_exit(PROGRAM_NAME ": input for Subj %s, condition %s is off-grid",who,clabel[cc]) ;
       if( DSET_NVALS(dset[dd])!=ntime ) ERROR_exit(PROGRAM_NAME ": input for Subj %s, condition %s has %d time points; expected %d",who,clabel[cc],DSET_NVALS(dset[dd]),ntime) ;
     }
   }

   /* Represent common censoring as a retained-time lookup.  Subject datasets
      remain uncompressed on disk and are sampled through this index. */
   ntime_input=ntime ; time_index=(int *)malloc(sizeof(int)*(size_t)ntime_input) ;
   censor_keep=(byte *)calloc((size_t)ntime_input,sizeof(byte)) ;
   if( time_index==NULL || censor_keep==NULL )
     ERROR_exit(PROGRAM_NAME ": cannot allocate common-censor index") ;
   memset(censor_keep,1,ntime_input);
   if(censor_name) INSYNC_apply_censor(censor_keep,ntime_input,censor_name);
   if(icol_censor>=0){
      byte *keep=insync_select_alloc(ntime_input,1);
      source_censor_count=insync_select_alloc(ndset,sizeof(int));
      for(ii=0;ii<ndset;ii++){
         memset(keep,1,ntime_input);
         INSYNC_apply_censor(keep,ntime_input,DT_CELL(tab,rowmap[ii],icol_censor));
         for(tt=0;tt<ntime_input;tt++){ source_censor_count[ii]+=keep[tt]; censor_keep[tt]&=keep[tt]; }
         if(!quiet) INFO_message("3dInSync: Subj %s condition %s censor retains %d/%d (%.1f%%)",tab->subj[rowmap[ii]],clabel[ii%ncond],source_censor_count[ii],ntime_input,100.*source_censor_count[ii]/ntime_input);
      }
      free(keep);
   }
   { int nkeep=0;
      for(tt=0;tt<ntime_input;tt++) if(censor_keep[tt]) time_index[nkeep++]=tt;
      if(nkeep<3) ERROR_exit("3dInSync: common censor retains %d time points; need at least 3",nkeep);
      ntime=nkeep;
      if(!quiet && (censor_name || icol_censor>=0)) INFO_message("3dInSync: common censor retains %d/%d (%.1f%%)",ntime,ntime_input,100.*ntime/ntime_input);
   }

   if( eo.nnull>0 && ntime-2*eo.min_shift+1<2 )
     ERROR_exit(PROGRAM_NAME ": -min_shift %d is too large for %d time points",
                eo.min_shift,ntime) ;

   /* Temporal null sets preserve each subject as the exchangeable unit.  For
      pairwise timeshifts, pre-index only lags that a requested draw uses. */
   nedge=(size_t)nsub*(nsub-1)/2 ;
   if( temporal_mode==INSYNC_TNULL_TIMESHIFT ){
     tset=THD_timeshift_set_build(nsub,ntime,nnull,min_shift,seed) ;
     if( tset==NULL ) ERROR_exit(PROGRAM_NAME ": cannot build temporal shifts") ;
     if( method==INSYNC_METHOD_PAIRWISE ){
       tsneed=(unsigned char *)calloc(nedge*(size_t)ntime,sizeof(unsigned char)) ;
       if( tsneed==NULL ) ERROR_exit(PROGRAM_NAME ": cannot allocate temporal lag index") ;
       for( ii=0 ; ii<nnull ; ii++ ){
         int aa ;
         int bb ;
         int pair=0 ;
         int *off=tset->offset+(size_t)ii*nsub ;
         for( aa=0 ; aa<nsub ; aa++ ) for( bb=aa+1 ; bb<nsub ; bb++,pair++ ){
           int lag=(off[bb]-off[aa])%ntime ;
           if( lag<0 ) lag+=ntime ;
           tsneed[(size_t)pair*ntime+lag]=1 ;
         }
       }
     }
   } else if( temporal_mode==INSYNC_TNULL_PHASE ){
     phset=THD_phase_set_build(nsub,ntime,nnull,seed) ;
     if( phset==NULL ) ERROR_exit(PROGRAM_NAME ": cannot build phase-randomization set") ;
   }

   if( maskname!=NULL ){
     mset=THD_open_dataset(maskname) ; CHECK_OPEN_ERROR(mset,maskname) ; DSET_load(mset) ; CHECK_LOAD_ERROR(mset) ;
     if( !EQUIV_GRIDS(first,mset) ) ERROR_exit(PROGRAM_NAME ": mask is not on the input grid") ;
     mask=THD_makemask(mset,0,0.5f,0.0f) ;
     if( mask==NULL || THD_countmask(nvox,mask)<1 ) ERROR_exit(PROGRAM_NAME ": mask is empty") ;
     DSET_delete(mset) ; mset=NULL ;
   }
   nmask=(mask!=NULL)?THD_countmask(nvox,mask):nvox ;

   /* The output is a map on the grid of the -mask or, without one, of the
      -atlas, so when that dataset is in a different view than the first input
      the output takes the mask's (or atlas's) view and template space, as 3dRSA
      does.  This covers template-space data whose header carries no space
      (e.g. a NIfTI without a space code, read as +orig) analyzed with a
      template-space mask or atlas.  When the views agree, the first input's
      header is kept. */
   out_space[0]='\0' ;
   {
     const char *tname=(maskname!=NULL)?maskname:atlas_name ;
     int v ;
     char sp[THD_MAX_NAME] ;
     if( tname!=NULL && insync_template_space(tname,&v,sp) && v!=first->view_type ){
       out_view=v ; MCW_strncpy(out_space,sp,THD_MAX_NAME) ;
       if( !quiet )
         INFO_message(PROGRAM_NAME ": output view %s taken from the %s; the first input is %s",
                      VIEW_typestr[v],(maskname!=NULL)?"-mask":"-atlas",
                      VIEW_typestr[first->view_type]) ;
     }
   }

   /* Choose how the subject data are held.  Dense mode keeps each dataset
      resident on the full grid, exactly as stored on disk.  Compact mode
      copies only the work units the analysis uses -- the in-mask voxels, or
      with -roi_only one mean time series per atlas ROI -- into a voxel-major
      store (optionally 16-bit) and releases each dataset after it is read.
      Compact mode is chosen when it is requested or is the smaller of the
      two.  -zcensor is applied after the data are read (see below). */
   nwork=nvox ;
   {
     int nbytes_native=0 ;
     double dense_bytes, compact_bytes ;
     for( ii=0 ; ii<ndset ; ii++ ){
       int nb=(int)mri_datum_size(DSET_BRICK_TYPE(dset[ii],0)) ;
       if( nb>nbytes_native ) nbytes_native=nb ;
     }
     dense_bytes=(double)ndset*nvox*ntime_input*nbytes_native ;
     compact_bytes=(double)ndset*nmask*ntime_input*(use_half?2.0:4.0) ;
     use_store=(roi_only || use_half || (mask!=NULL && compact_bytes<dense_bytes)) ;
     native_bytes=nbytes_native ;

     if( roi_only ){
       THD_3dim_dataset *aset=NULL ;
       THD_roilist *rl=insync_open_atlas(atlas_name,roi_sel,first,&aset) ;
       int rr, k ;
       vmap=(int *)malloc(sizeof(int)*(size_t)nvox) ;
       roi_vox=(int **)calloc((size_t)rl->nroi,sizeof(int *)) ;
       roi_nv=(int *)calloc((size_t)rl->nroi,sizeof(int)) ;
       roi_val=(int *)calloc((size_t)rl->nroi,sizeof(int)) ;
       roi_lab=(char **)calloc((size_t)rl->nroi,sizeof(char *)) ;
       if( vmap==NULL || roi_vox==NULL || roi_nv==NULL || roi_val==NULL || roi_lab==NULL )
         ERROR_exit(PROGRAM_NAME ": cannot allocate ROI index") ;
       for( vv=0 ; vv<nvox ; vv++ ) vmap[vv]=-1 ;
       for( rr=0 ; rr<rl->nroi ; rr++ ){
         int nrv=0 ;
         char lab[128] ;
         for( jj=0 ; jj<rl->vox[rr].nar ; jj++ ){
           int iv=rl->vox[rr].ar[jj] ;
           if( mask==NULL || mask[iv] ) nrv++ ;
         }
         if( nrv<1 ) continue ;
         k=nroi++ ;
         roi_vox[k]=(int *)malloc(sizeof(int)*(size_t)nrv) ;
         roi_nv[k]=nrv ; roi_val[k]=rl->val[rr] ;
         if( rl->lab!=NULL && rl->lab[rr]!=NULL ) insync_safe_token(lab,sizeof(lab),rl->lab[rr]) ;
         else snprintf(lab,sizeof(lab),"ROI%d",rl->val[rr]) ;
         roi_lab[k]=strdup(lab) ;
         nrv=0 ;
         for( jj=0 ; jj<rl->vox[rr].nar ; jj++ ){
           int iv=rl->vox[rr].ar[jj] ;
           if( mask==NULL || mask[iv] ){ roi_vox[k][nrv++]=iv ; vmap[iv]=k ; }
         }
       }
       if( nroi<1 ) ERROR_exit(PROGRAM_NAME ": no atlas ROI overlaps the analysis mask") ;
       nwork=nroi ;
       THD_roilist_free(rl) ; DSET_delete(aset) ;
     } else if( use_store ){
       vmap=(int *)malloc(sizeof(int)*(size_t)nvox) ;
       widx=(int *)malloc(sizeof(int)*(size_t)nmask) ;
       if( vmap==NULL || widx==NULL ) ERROR_exit(PROGRAM_NAME ": cannot allocate voxel index") ;
       for( vv=0,jj=0 ; vv<nvox ; vv++ ){
         if( mask==NULL || mask[vv] ){ vmap[vv]=jj ; widx[jj++]=vv ; } else vmap[vv]=-1 ;
       }
       nwork=nmask ;
     }
   }
   src.ndset=ndset ; src.ntime_input=ntime_input ; src.nwork=nwork ;
   src.half=use_half ; src.vmap=vmap ; src.dset=dset ;

   /* Assign every optional output family a contiguous sub-brick range.  The
      same offsets are later used both for computation and AFNI labels. */
   nout=2*ncond*ngroup ;
   if( ngroup==2 ){ diff_slot=nout ; nout+=ncond ; }
   if( nboot>0 ){ boot_slot=nout ; nout+=2*ncond*ngroup ; }
   if( pset!=NULL ){ perm_slot=nout ; nout+=5*ncond ; }
   if( tset!=NULL || phset!=NULL ){ temporal_slot=nout ; nout+=5*ncond*ngroup ; }
   if( have_contrast ){
     neffect=ngroup+((ngroup==2)?1:0) ; contrast_slot=nout ; nout+=neffect ;
     if( cset!=NULL ){ condperm_slot=nout ; nout+=5*neffect ; }
   }
   if( censor_name!=NULL || icol_censor>=0 || do_zcensor || missing_policy==INSYNC_MISSING_COMMON ){
     validtr_slot=nout ; nout++ ;
   }
   npairmap=(pair_prefix!=NULL)?(size_t)ncond*ncond*nedge:0 ;

   {
     const double GIB=1073741824.0 ;
     const double FS=(double)sizeof(float) ;
     THD_memory_plan me ;
     double warn ;

     /* Refuse an unsafe allocation plan before loading the large subject
        datasets.  Per-worker scratch is multiplied by the OpenMP thread cap. */
     memset(&me,0,sizeof(me)) ;
#ifdef USE_OMP
     me.nthread=omp_get_max_threads() ;
#else
     me.nthread=1 ;
#endif
     if( me.nthread<1 ) me.nthread=1 ;
     /* Resident subject data: the compact store, or the datasets themselves
        at the datatype actually stored on disk (not assumed to be float). */
     me.input=use_store ? (double)ndset*nwork*((double)ntime_input*(use_half?2.0:4.0)+(use_half?8.0:0.0))
                        : (double)ndset*nvox*ntime_input*native_bytes ;
     if( vmap!=NULL ) me.geometry+=(double)nvox*sizeof(int)+(widx?(double)nwork*sizeof(int):0.0) ;
     if( atlas_name!=NULL ){
       me.geometry+=(double)nvox*FS ;
       me.shared+=FS*((double)ndset*ntime+nsub*nsub+2.0*nsub+3.0*ntime) ;
     }
     me.shared+=(double)nsub*((bset?bset->nresample:0)*sizeof(int)
              +(pset?pset->nperm:0)*(sizeof(int)+sizeof(signed char))
              +(cset?cset->nperm:0)*(sizeof(int)+sizeof(signed char))) ;
     if( pset!=NULL && tail==INSYNC_TAIL_BI )
       me.shared+=FS*pset->nperm ;
     me.output=((double)nout+(double)npairmap)*((double)nwork+(vmap!=NULL?(double)nvox:0.0))*FS ;
     me.per_thread=FS*((double)ndset*ntime+2.0*nsub*nsub+
                       2.0*nsub*ntime+2.0*nsub+2.0*ntime+
                       (double)nedge*ntime) ;
     if( pset ) me.per_thread+=FS*(2.0+(tail==INSYNC_TAIL_BI))*pset->nperm ;
     if( cset ) me.per_thread+=FS*((double)neffect*cset->nperm+cset->nperm) ;
     if( tset || phset ) me.per_thread+=FS*((double)ngroup*nnull+nnull+
                                            (double)nsub*ntime) ;
     me.system=(double)AFNI_get_memsize() ;
     me.limit=memory_limit_given?memory_limit_gib*GIB:
              ((me.system>0.0)?0.80*me.system:0.0) ;
     THD_memory_plan_finish(&me) ;
     if( use_store ){
       /* While loading, one full dataset (plus a float buffer for -float16) is
          resident beside the growing store; take the larger of the two peaks. */
       double load_peak=me.input+me.geometry
                       +(double)nvox*ntime_input*native_bytes
                       +(use_half?(double)nwork*ntime_input*FS:0.0) ;
       if( load_peak>me.total ) me.total=load_peak ;
       if( !quiet ) INFO_message(PROGRAM_NAME ": subject data held as %s: %d %s x %d time points "
                                 "x %d arrays = %.3f GiB (full-grid datasets would need %.3f GiB)",
                                 use_half?"16-bit floats":"32-bit floats",nwork,
                                 roi_only?"ROIs":"in-mask voxels",ntime_input,ndset,
                                 me.input/GIB,(double)ndset*nvox*ntime_input*native_bytes/GIB) ;
     }
     warn=(me.system>0.0)?0.50*me.system:0.0 ;
     if( !quiet && (me.total>warn || memory_limit_given || pair_prefix!=NULL) ){
       if( me.limit>0.0 )
         INFO_message(PROGRAM_NAME ": memory preflight estimates %.3f GiB peak "
                      "(%.3f input + %.3f output/shared + %d x %.3f worker); "
                      "limit %.3f GiB",me.total/GIB,me.input/GIB,
                      (me.output+me.shared)/GIB,me.nthread,me.per_thread/GIB,
                      me.limit/GIB) ;
       else
         INFO_message(PROGRAM_NAME ": memory preflight estimates %.3f GiB peak; "
                      "physical RAM is unavailable",me.total/GIB) ;
     }
     if( me.limit>0.0 && me.total>me.limit ){
       if( !memory_override )
         ERROR_exit(PROGRAM_NAME ": estimated peak %.3f GiB exceeds %.3f GiB limit; "
                    "reduce threads/output, set -memory_limit, or acknowledge with "
                    "-memory_override",me.total/GIB,me.limit/GIB) ;
       WARNING_message(PROGRAM_NAME ": estimated peak exceeds memory limit; "
                       "continuing because -memory_override was given") ;
     }
   }

   /* The potentially dominant subject datasets are intentionally loaded only
      after the memory contract has been checked.  They are read one at a time:
      in compact mode each is reduced to its work units and released, so peak
      memory is the store plus a single dataset. */
   if( use_store ){
     src.fst=NULL ; src.hst=NULL ; src.hpar=NULL ;
     if( use_half ){
       src.hst=(unsigned short **)calloc((size_t)ndset,sizeof(unsigned short *)) ;
       src.hpar=(float **)calloc((size_t)ndset,sizeof(float *)) ;
     }
     else           src.fst=(float **)calloc((size_t)ndset,sizeof(float *)) ;
     if( (use_half?(void *)src.hst:(void *)src.fst)==NULL || (use_half && src.hpar==NULL) )
       ERROR_exit(PROGRAM_NAME ": cannot allocate the subject store") ;
   }
   if( do_zcensor ){
     nzero=(int *)calloc((size_t)ndset,sizeof(int)) ;
     zero_time=(byte *)calloc((size_t)ntime_input,sizeof(byte)) ;
     if( nzero==NULL || zero_time==NULL ) ERROR_exit(PROGRAM_NAME ": cannot allocate -zcensor counts") ;
   }
   for( ii=0 ; ii<ndset ; ii++ ){
     DSET_load(dset[ii]) ; CHECK_LOAD_ERROR(dset[ii]) ;

     /* Generalize 3dTcorrelate's -zcensor rule to every input dataset: a
        time point is unusable if any subject/condition volume is all zero in
        the analysis mask.  Judged on the raw data, before any storage change. */
     if( do_zcensor ){
       byte *zflag=(byte *)calloc((size_t)ntime_input,sizeof(byte)) ;
       if( zflag==NULL ) ERROR_exit(PROGRAM_NAME ": cannot allocate -zcensor flags") ;
#ifdef USE_OMP
#pragma omp parallel for schedule(dynamic,16)
#endif
       for( tt=0 ; tt<ntime_input ; tt++ ){
         int gv, nonzero=0 ;
         for( gv=0 ; gv<nvox ; gv++ ) if( mask==NULL || mask[gv] ){
           if( THD_get_voxel(dset[ii],gv,tt)!=0.0f ){ nonzero=1 ; break ; }
         }
         zflag[tt]=(byte)!nonzero ;
       }
       for( tt=0 ; tt<ntime_input ; tt++ ) if( zflag[tt] ){ nzero[ii]++ ; zero_time[tt]=1 ; }
       free(zflag) ;
     }

     if( use_store ){
       int bad_w=-1, bad_t=-1 ;
       insync_fill_one(dset[ii],ii,&src,widx,roi_only?roi_vox:NULL,roi_nv,
                       missing_policy,&bad_w,&bad_t) ;
       if( bad_t>=0 ){
         int si2=ii/ncond, ci2=ii%ncond ;
         ERROR_exit(PROGRAM_NAME ": nonfinite atlas input for ROI %d, Subj %s, "
                    "condition %s, time %d",roi_val[bad_w],
                    cindex?cindex->level[0][si2]:tab->subj[si2],clabel[ci2],bad_t+1) ;
       }
       DSET_unload(dset[ii]) ;
     }
   }

   /* Apply -zcensor now that every dataset has been judged: the retained
      timeline is common to all subjects, conditions, voxels, and ROIs. */
   if( do_zcensor ){
     int nzero_time=0, nremoved=0, nkeep=0 ;
     for( tt=0 ; tt<ntime_input ; tt++ ) if( zero_time[tt] ){
       nzero_time++ ;
       if( censor_keep[tt] ){ censor_keep[tt]=0 ; nremoved++ ; }
     }
     for( tt=0 ; tt<ntime_input ; tt++ ) if( censor_keep[tt] ) time_index[nkeep++]=tt ;
     if( nkeep<3 ) ERROR_exit(PROGRAM_NAME ": -zcensor retains %d time points; need at least 3",nkeep) ;
     ntime=nkeep ;
     if( !quiet ) INFO_message(PROGRAM_NAME ": -zcensor found %d all-zero time points; "
                               "removed %d and retained %d of %d common time points",
                               nzero_time,nremoved,ntime,ntime_input) ;
     for( si=0 ; si<nsub ; si++ ) for( ci=0 ; ci<ncond ; ci++ ){
       int dd=si*ncond+ci ;
       float pct=100.0f*(float)nzero[dd]/(float)ntime_input ;
       if( pct>zcensor_warn ){
         int rr=rowmap[dd] ;
         const char *who=cindex?cindex->level[0][si]:tab->subj[si] ;
         WARNING_message(PROGRAM_NAME ": -zcensor: Subj %s, condition %s, dataset '%s' "
                         "has %d/%d all-zero volumes (%.1f%%; warning level %.1f%%)",
                         who,clabel[ci],tab->fname[rr],nzero[dd],ntime_input,pct,zcensor_warn) ;
       }
     }
     free(nzero) ; free(zero_time) ; nzero=NULL ; zero_time=NULL ;
   }

   if(icol_censor>=0){
      char name[THD_MAX_NAME]; FILE *fp;
      snprintf(name,sizeof(name),"%s.common_censor.1D",prefix); fp=insync_open_text(name);
      for(tt=0;tt<ntime_input;tt++) fprintf(fp,"%d\n",censor_keep[tt]!=0);
      { int fail=ferror(fp); if(fclose(fp)) fail=1; if(fail) ERROR_exit("3dInSync: cannot write common censor"); }
      snprintf(name,sizeof(name),"%s.censor.tsv",prefix); fp=insync_open_text(name);
      fprintf(fp,"# Common retained after Censor, global censor and zcensor: %d/%d (%.9g); voxel missingness is separate\nSubj\tCondition\tRetained\tNominal\tFraction\n",ntime,ntime_input,(double)ntime/ntime_input);
      for(ii=0;ii<ndset;ii++) fprintf(fp,"%s\t%s\t%d\t%d\t%.9g\n",tab->subj[rowmap[ii]],clabel[ii%ncond],source_censor_count[ii],ntime_input,(double)source_censor_count[ii]/ntime_input);
      { int fail=ferror(fp); if(fclose(fp)) fail=1; if(fail) ERROR_exit("3dInSync: cannot write censor report"); }
      free(source_censor_count);
   }

   /* Allocate shared output maps and empirical-null accumulators. */
   outval=(float **)calloc((size_t)nout,sizeof(float *)) ;
   for( kk=0 ; kk<nout ; kk++ ){
     outval[kk]=(float *)calloc((size_t)nwork,sizeof(float)) ;
     if( outval[kk]==NULL ) ERROR_exit(PROGRAM_NAME ": cannot allocate output map %d",kk) ;
   }
   if( pset!=NULL ){
     int nelem=ncond*nwork ;
     infer_valid=(byte *)calloc((size_t)nelem,sizeof(byte)) ;
     presult=THD_perm_result_new(nelem,pset->nperm) ;
     if( tail==INSYNC_TAIL_BI )
       group_max_neg=(float *)malloc(sizeof(float)*(size_t)pset->nperm) ;
     if( presult==NULL || infer_valid==NULL ||
         (tail==INSYNC_TAIL_BI && group_max_neg==NULL) )
       ERROR_exit(PROGRAM_NAME ": cannot allocate group-permutation results") ;
     presult->tail=tail ;
     for( ii=0 ; ii<pset->nperm ; ii++ ) presult->max_null[ii]=-FLT_MAX ;
     if( group_max_neg!=NULL ) for( ii=0 ; ii<pset->nperm ; ii++ )
       group_max_neg[ii]=-FLT_MAX ;
   }
   if( tset!=NULL || phset!=NULL ){
     int nelem=ncond*ngroup*nwork ;
     temporal_valid=(byte *)calloc((size_t)nelem,sizeof(byte)) ;
     tresult=THD_perm_result_new(nelem,nnull) ; tresult->tail=temporal_tail ;
     for( ii=0 ; ii<nnull ; ii++ ) tresult->max_null[ii]=-FLT_MAX ;
   }
   if( cset!=NULL ){
     int nelem=neffect*nwork ;
     condition_valid=(byte *)calloc((size_t)nelem,sizeof(byte)) ;
     cresult=THD_perm_result_new(nelem,cset->nperm) ; cresult->tail=condition_tail ;
     for( ii=0 ; ii<cset->nperm ; ii++ ) cresult->max_null[ii]=-FLT_MAX ;
   }
   if( pair_prefix!=NULL ){
     size_t total ;
     npairmap=(size_t)ncond*ncond*nedge ;
     if( npairmap>(size_t)INT_MAX ) ERROR_exit(PROGRAM_NAME ": too many pair-map bricks") ;
     if( npairmap>0 && (size_t)nwork>((size_t)-1)/npairmap ) ERROR_exit(PROGRAM_NAME ": pair-map array is too large") ;
     total=npairmap*(size_t)nwork ; pairval=(float *)calloc(total,sizeof(float)) ;
     if( pairval==NULL ) ERROR_exit(PROGRAM_NAME ": cannot allocate %lu pair-map values",(unsigned long)total) ;
   }
   insync_progress_init(&progress,progress_mode,quiet,use_store?nwork:nmask,"inference","voxels") ;

#ifdef USE_OMP
#pragma omp parallel reduction(+:invalid_total,boot_invalid_total,temporal_invalid_total,condition_invalid_total) reduction(min:bad_data)
#endif
   {
     /* Every worker owns all temporary arrays used for one voxel.  Only final
        image writes target disjoint voxels; max-null arrays merge afterward. */
     int si ;
     int tt ;
     int gg ;
     int ci ;
     size_t nscratch=(nedge>(size_t)nsub)?nedge:(size_t)nsub ;
     float *data=(float *)malloc(sizeof(float)*(size_t)ndset*ntime) ;
     float *scratch=(float *)malloc(sizeof(float)*nscratch) ;
     float *ref=(float *)malloc(sizeof(float)*(size_t)ntime) ;
     float *values=(float *)malloc(sizeof(float)*(size_t)nsub) ;
     float *rank1=(corr_metric==SIM_SPEARMAN)?(float *)malloc(sizeof(float)*(size_t)ntime):NULL ;
     float *rank2=(corr_metric==SIM_SPEARMAN)?(float *)malloc(sizeof(float)*(size_t)ntime):NULL ;
     int *sample=(int *)malloc(sizeof(int)*(size_t)nsub) ;
     int *sel0=(int *)malloc(sizeof(int)*(size_t)nsub) ;
     int *sel1=(int *)malloc(sizeof(int)*(size_t)nsub) ;
     byte *goodtime=(missing_policy==INSYNC_MISSING_COMMON)?(byte *)malloc((size_t)ntime):NULL ;
     float *bootdraw=(nboot>0)?(float *)malloc(sizeof(float)*(size_t)nboot):NULL ;
     float *permnull=(pset!=NULL)?(float *)malloc(sizeof(float)*(size_t)pset->nperm):NULL ;
     float *my_max=(pset!=NULL)?(float *)malloc(sizeof(float)*(size_t)pset->nperm):NULL ;
     float *my_max_neg=(pset!=NULL && tail==INSYNC_TAIL_BI)
                        ?(float *)malloc(sizeof(float)*(size_t)pset->nperm):NULL ;
     float *tnull=(tresult!=NULL)?(float *)malloc(sizeof(float)*(size_t)ngroup*nnull):NULL ;
     float *t_mymax=(tresult!=NULL)?(float *)malloc(sizeof(float)*(size_t)nnull):NULL ;
     float *surrogate=(tresult!=NULL)?(float *)malloc(sizeof(float)*(size_t)nsub*ntime):NULL ;
     float *lagtab=(tset!=NULL && method==INSYNC_METHOD_PAIRWISE)?(float *)malloc(sizeof(float)*nedge*ntime):NULL ;
     float *lagprep=(tset!=NULL && method==INSYNC_METHOD_PAIRWISE)?(float *)malloc(sizeof(float)*(size_t)nsub*ntime):NULL ;
     float *lagnorm=(tset!=NULL && method==INSYNC_METHOD_PAIRWISE)?(float *)malloc(sizeof(float)*(size_t)nsub):NULL ;
     complex *phspec=(phset!=NULL)?(complex *)malloc(sizeof(complex)*(size_t)nsub*ntime):NULL ;
     complex *phwork=(phset!=NULL)?(complex *)malloc(sizeof(complex)*(size_t)ntime):NULL ;
     float *cdata=(have_contrast)?(float *)malloc(sizeof(float)*(size_t)2*nsub*ntime):NULL ;
     float *ceffect=(have_contrast)?(float *)malloc(sizeof(float)*(size_t)neffect):NULL ;
     float *geffect=(have_contrast)?(float *)malloc(sizeof(float)*(size_t)ngroup):NULL ;
     float *condnull=(cset!=NULL)?(float *)malloc(sizeof(float)*(size_t)neffect*cset->nperm):NULL ;
     float *c_mymax=(cset!=NULL)?(float *)malloc(sizeof(float)*(size_t)cset->nperm):NULL ;
     THD_simmat *sm=THD_simmat_new(nsub) ;
     THD_simmat *nullsm=(tresult!=NULL && method==INSYNC_METHOD_PAIRWISE)?THD_simmat_new(nsub):NULL ;
     THD_simmat *csm=(have_contrast && method==INSYNC_METHOD_PAIRWISE)?THD_simmat_new(2*nsub):NULL ;
     THD_simmat *pairsm=(pairval!=NULL)?THD_simmat_new(ndset):NULL ;

     if( data==NULL || scratch==NULL || ref==NULL || values==NULL || sample==NULL ||
         sel0==NULL || sel1==NULL || sm==NULL || (nboot>0 && bootdraw==NULL) ||
         (corr_metric==SIM_SPEARMAN && (rank1==NULL || rank2==NULL)) ||
         (missing_policy==INSYNC_MISSING_COMMON && goodtime==NULL) ||
         (pset!=NULL && (permnull==NULL || my_max==NULL ||
                          (tail==INSYNC_TAIL_BI && my_max_neg==NULL))) ||
         (tresult!=NULL && (tnull==NULL || t_mymax==NULL || surrogate==NULL)) ||
         (tset!=NULL && method==INSYNC_METHOD_PAIRWISE && (lagtab==NULL || lagprep==NULL || lagnorm==NULL || nullsm==NULL)) ||
         (phset!=NULL && (phspec==NULL || phwork==NULL || (method==INSYNC_METHOD_PAIRWISE && nullsm==NULL))) ||
         (have_contrast && (cdata==NULL || ceffect==NULL || geffect==NULL ||
                            (method==INSYNC_METHOD_PAIRWISE && csm==NULL))) ||
         (cset!=NULL && (condnull==NULL || c_mymax==NULL)) || (pairval!=NULL && pairsm==NULL) )
       ERROR_exit(PROGRAM_NAME ": cannot allocate per-worker buffers") ;
     if( pset!=NULL ) for( si=0 ; si<pset->nperm ; si++ ) my_max[si]=-FLT_MAX ;
     if( my_max_neg!=NULL ) for( si=0 ; si<pset->nperm ; si++ )
       my_max_neg[si]=-FLT_MAX ;
     if( tresult!=NULL ) for( si=0 ; si<nnull ; si++ ) t_mymax[si]=-FLT_MAX ;
     if( cset!=NULL ) for( si=0 ; si<cset->nperm ; si++ ) c_mymax[si]=-FLT_MAX ;

#ifdef USE_OMP
#pragma omp for schedule(dynamic,32)
#endif
     for( vv=0 ; vv<nwork ; vv++ ){
       int nt=ntime ;
       int hasbad=0 ;
       if( !use_store && mask!=NULL && !mask[vv] ) continue ;

       /* Materialize this voxel as condition-major, subject-major time
          series so all estimators and null models share one memory layout. */
       for( ci=0 ; ci<ncond ; ci++ ) for( si=0 ; si<nsub ; si++ ) for( tt=0 ; tt<ntime ; tt++ )
       {
         int dd=ci*nsub+si ;
         int ot=time_index[tt] ;
         float x=use_store ? insync_store_get(&src,si*ncond+ci,vv,ot)
                           : THD_get_voxel(dset[si*ncond+ci],vv,ot) ;
         data[(size_t)dd*ntime+tt]=x ;
         if( missing_policy==INSYNC_MISSING_ERROR && !isfinite(x) ){
           long long code=((long long)vv*ndset+dd)*ntime_input+ot ;
           if( code<bad_data ) bad_data=code ;
           hasbad=1 ;
         }
       }
       if( hasbad ){
         invalid_total+=(long long)ncond*ngroup ;
         insync_progress_advance(&progress) ;
         continue ;
       }
       if( missing_policy==INSYNC_MISSING_COMMON ){
         int dd ;
         int ngood=0 ;

         /* A time point survives only if it is finite for every subject and
            condition, keeping every correlation on the same local timeline. */
         for( tt=0 ; tt<ntime ; tt++ ){
           int good=1 ;
           for( dd=0 ; dd<ndset ; dd++ )
             if( !isfinite(data[(size_t)dd*ntime+tt]) ){ good=0 ; break ; }
           goodtime[tt]=(byte)good ; if( good ) ngood++ ;
         }
         if( ngood>=1 ) for( dd=0 ; dd<ndset ; dd++ ){
           int pos=0 ;
           float *src=data+(size_t)dd*ntime ;
           float *dst=data+(size_t)dd*ngood ;
           for( tt=0 ; tt<ntime ; tt++ ) if( goodtime[tt] ) dst[pos++]=src[tt] ;
         }
         nt=ngood ;
       }
       if( validtr_slot>=0 ) outval[validtr_slot][vv]=(float)nt ;
       if( nt<3 ){
         invalid_total+=(long long)ncond*ngroup ;
         insync_progress_advance(&progress) ;
         continue ;
       }

       if( pairval!=NULL && !THD_simmat_fill_from_features(pairsm,nt,data,corr_metric,rank1,rank2) ){
         int aa ;
         int bb ;
         int pair=0 ;
         for( aa=0 ; aa<nsub ; aa++ ) for( bb=aa+1 ; bb<nsub ; bb++,pair++ )
           for( ci=0 ; ci<ncond ; ci++ ) for( cc=0 ; cc<ncond ; cc++ ){
             size_t pm=((size_t)ci*ncond+cc)*nedge+pair ;
             float rr=pairsm->mat[((size_t)ci*nsub+aa)*ndset+(size_t)cc*nsub+bb] ;
             pairval[pm*(size_t)nwork+vv]=insync_fisher_z(rr) ;
           }
       }

       for( ci=0 ; ci<ncond ; ci++ ){
         float *cblock=data+(size_t)ci*nsub*nt ;
         float gstat[2]={NAN,NAN} ;
         int pair_ok=1 ;
         if( (method==INSYNC_METHOD_PAIRWISE || pairval!=NULL) &&
             THD_simmat_fill_from_features(sm,nt,cblock,corr_metric,rank1,rank2) ) pair_ok=0 ;
         for( gg=0 ; gg<ngroup ; gg++ ){
           float stat ;
           int nv=0 ;
           int bs=2*(ci*ngroup+gg) ;
           if( method==INSYNC_METHOD_PAIRWISE && pair_ok ) stat=INSYNC_pairwise_indexed(nsub,sm->mat,gcount[gg],gmember[gg],summary,scratch,&nv) ;
           else if( method==INSYNC_METHOD_PAIRWISE ) stat=NAN ;
           else stat=INSYNC_loo_indexed_metric(nsub,nt,cblock,gcount[gg],gmember[gg],
                     summary,corr_metric,ref,values,rank1,rank2,&nv) ;
           if( isfinite(stat) ) outval[bs][vv]=stat ; else invalid_total++ ;
           outval[bs+1][vv]=(float)nv ; if( tresult!=NULL ) tnull[(size_t)gg*nnull]=stat ;
           if( gg<2 ) gstat[gg]=stat ;
         }
         if( diff_slot>=0 && isfinite(gstat[0]) && isfinite(gstat[1]) ) outval[diff_slot+ci][vv]=gstat[0]-gstat[1] ;

         if( nboot>0 ) for( gg=0 ; gg<ngroup ; gg++ ){
           int bb ;
           int ndraw=0 ;
           int nm=gcount[gg] ;
           if( method==INSYNC_METHOD_PAIRWISE && !pair_ok ){ boot_invalid_total++ ; continue ; }
           for( bb=0 ; bb<nboot ; bb++ ){
             int aa ;
             int bb2 ;
             int nu=0 ;
             int *ix=bset->index+(size_t)bb*nsub ;
             float st ;

             /* Pairwise bootstrap samples may repeat subjects.  Count unique
                identities and never create a subject's self-edge. */
             for( aa=0 ; aa<nm ; aa++ ) sample[aa]=ix[gmember[gg][aa]] ;
             for( aa=0 ; aa<nm ; aa++ ){
               for( bb2=0 ; bb2<aa ; bb2++ ) if( sample[bb2]==sample[aa] ) break ;
               if( bb2==aa ) nu++ ;
             }
             if( nu<2 ) continue ;
             if( method==INSYNC_METHOD_PAIRWISE ) st=INSYNC_pairwise_indexed(nsub,sm->mat,nm,sample,summary,scratch,NULL) ;
             else st=INSYNC_loo_indexed_metric(nsub,nt,cblock,nm,sample,summary,
                      corr_metric,ref,values,rank1,rank2,NULL) ;
             if( isfinite(st) ) bootdraw[ndraw++]=st ;
           }
           if( ndraw>=10 ){
             int bs=boot_slot+2*(ci*ngroup+gg) ;
             outval[bs][vv]=INSYNC_percentile(bootdraw,ndraw,0.5f*boot_alpha) ;
             outval[bs+1][vv]=INSYNC_percentile(bootdraw,ndraw,1.0f-0.5f*boot_alpha) ;
           } else boot_invalid_total++ ;
         }

         if( tresult!=NULL ){
           int ss ;
           int prep_good=!(method==INSYNC_METHOD_PAIRWISE && !pair_ok) ;

           /* The identity surrogate occupies slot zero.  Remaining slots are
              generated from the same synchronized null set at every voxel. */
           if( prep_good && tset!=NULL && method==INSYNC_METHOD_PAIRWISE &&
               THD_simmat_lag_table(nsub,nt,cblock,corr_metric,tsneed,lagtab,lagprep,lagnorm) ) prep_good=0 ;
           if( prep_good && phset!=NULL && insync_phase_prepare(nsub,nt,cblock,phspec) ) prep_good=0 ;
           for( ss=1 ; prep_good && ss<nnull ; ss++ ){
             if( tset!=NULL ){
               int *off=tset->offset+(size_t)ss*nsub ;
               if( method==INSYNC_METHOD_PAIRWISE ){
                 if( THD_simmat_from_lag_table(nullsm,nt,lagtab,off) ){ prep_good=0 ; break ; }
               } else insync_shift_draw(nsub,nt,cblock,off,surrogate) ;
             } else {
               if( insync_phase_draw(phset,ss,phspec,phwork,surrogate) ){ prep_good=0 ; break ; }
               if( method==INSYNC_METHOD_PAIRWISE && THD_simmat_fill_from_features(nullsm,nt,surrogate,corr_metric,rank1,rank2) ){ prep_good=0 ; break ; }
             }
             for( gg=0 ; gg<ngroup ; gg++ )
               tnull[(size_t)gg*nnull+ss]=(method==INSYNC_METHOD_PAIRWISE)
                 ?INSYNC_pairwise_indexed(nsub,nullsm->mat,gcount[gg],gmember[gg],summary,scratch,NULL)
                 :INSYNC_loo_indexed_metric(nsub,nt,surrogate,gcount[gg],gmember[gg],
                    summary,corr_metric,ref,values,rank1,rank2,NULL) ;
           }
           for( gg=0 ; gg<ngroup ; gg++ ){
             int good=prep_good ;
             int e=(ci*ngroup+gg)*nwork+vv ;
             float obs=tnull[(size_t)gg*nnull] ;
             if( !isfinite(obs) ) good=0 ;
             for( ss=1 ; good && ss<nnull ; ss++ ) if( !isfinite(tnull[(size_t)gg*nnull+ss]) ) good=0 ;
             if( good ){
               float ocmp=(temporal_tail==PERM_TAIL_TWO)?fabsf(obs):obs ;
               temporal_valid[e]=1 ; tresult->stat[e]=obs ;
               for( ss=0 ; ss<nnull ; ss++ ){
                 float st=tnull[(size_t)gg*nnull+ss] ;
                 float cmp=(temporal_tail==PERM_TAIL_TWO)?fabsf(st):st ;
                 if( cmp>=ocmp ) tresult->cnt_unc[e]++ ; if( cmp>t_mymax[ss] ) t_mymax[ss]=cmp ;
               }
             } else temporal_invalid_total++ ;
           }
         }

         if( pset!=NULL && isfinite(gstat[0]) && isfinite(gstat[1]) ){
           int ip ;
           int good=1 ;
           int e=ci*nwork+vv ;
           float obs=gstat[0]-gstat[1] ;

           /* Relabel whole subjects, then reconstruct each group from the
              pooled matrix/time-series block.  Pairwise edges are not the
              exchangeable observations. */
           for( ip=0 ; ip<pset->nperm ; ip++ ){
             int pos0=0 ;
             int pos1=gcount[0] ;
             int *pp=pset->perm+(size_t)ip*nsub ;
             float st0 ;
             float st1 ;
             for( si=0 ; si<nsub ; si++ ) if( group[si]==0 ) sample[pos0++]=pp[si] ; else sample[pos1++]=pp[si] ;
             if( method==INSYNC_METHOD_PAIRWISE ){
               st0=INSYNC_pairwise_indexed(nsub,sm->mat,gcount[0],sample,summary,scratch,NULL) ;
               st1=INSYNC_pairwise_indexed(nsub,sm->mat,gcount[1],sample+gcount[0],summary,scratch,NULL) ;
             } else {
               st0=INSYNC_loo_indexed_metric(nsub,nt,cblock,gcount[0],sample,summary,
                       corr_metric,ref,values,rank1,rank2,NULL) ;
               st1=INSYNC_loo_indexed_metric(nsub,nt,cblock,gcount[1],sample+gcount[0],
                       summary,corr_metric,ref,values,rank1,rank2,NULL) ;
             }
             if( !isfinite(st0) || !isfinite(st1) ){ good=0 ; break ; } permnull[ip]=st0-st1 ;
           }
           if( good ){
             int neg=(tail==INSYNC_TAIL_BI && obs<0.0f) ;
             float ocmp=(tail==PERM_TAIL_TWO)?fabsf(obs):(neg?-obs:obs) ;
             infer_valid[e]=1 ; presult->stat[e]=obs ;
             for( ip=0 ; ip<pset->nperm ; ip++ ){
               float st=permnull[ip] ;
               float cmp=(tail==PERM_TAIL_TWO)?fabsf(st):(neg?-st:st) ;
               if( cmp>=ocmp ) presult->cnt_unc[e]++ ;
               if( tail==INSYNC_TAIL_BI ){
                 /* AFNI bisided inference keeps direction-specific maxima;
                    a negative observed effect is ranked against -minimum. */
                 if( st>my_max[ip] ) my_max[ip]=st ;
                 if( -st>my_max_neg[ip] ) my_max_neg[ip]=-st ;
               } else if( cmp>my_max[ip] ) my_max[ip]=cmp ;
             }
           }
         }
       }

       if( have_contrast ){
         int ip ;
         int e ;

         /* Condition effects retain subject pairing.  The optional null
            swaps C1/C2 within each subject using synchronized sign patterns. */
         for( e=0 ; e<neffect ; e++ ) ceffect[e]=NAN ;
         memcpy(cdata,data+(size_t)contrast_c0*nsub*nt,sizeof(float)*(size_t)nsub*nt) ;
         memcpy(cdata+(size_t)nsub*nt,data+(size_t)contrast_c1*nsub*nt,sizeof(float)*(size_t)nsub*nt) ;
         if( method!=INSYNC_METHOD_PAIRWISE || !THD_simmat_fill_from_features(csm,nt,cdata,corr_metric,rank1,rank2) ){
           for( gg=0 ; gg<ngroup ; gg++ ){
             int mm=gcount[gg] ;
             float a ;
             float b ;
             for( si=0 ; si<mm ; si++ ){ sel0[si]=gmember[gg][si] ; sel1[si]=nsub+gmember[gg][si] ; }
             if( method==INSYNC_METHOD_PAIRWISE ){
               a=INSYNC_pairwise_indexed(2*nsub,csm->mat,mm,sel0,summary,scratch,NULL) ;
               b=INSYNC_pairwise_indexed(2*nsub,csm->mat,mm,sel1,summary,scratch,NULL) ;
             } else {
               a=INSYNC_loo_indexed_metric(2*nsub,nt,cdata,mm,sel0,summary,
                    corr_metric,ref,values,rank1,rank2,NULL) ;
               b=INSYNC_loo_indexed_metric(2*nsub,nt,cdata,mm,sel1,summary,
                    corr_metric,ref,values,rank1,rank2,NULL) ;
             }
             ceffect[gg]=a-b ; if( isfinite(ceffect[gg]) ) outval[contrast_slot+gg][vv]=ceffect[gg] ;
             else condition_invalid_total++ ;
           }
           if( ngroup==2 ){
             ceffect[ngroup]=ceffect[0]-ceffect[1] ;
             if( isfinite(ceffect[ngroup]) ) outval[contrast_slot+ngroup][vv]=ceffect[ngroup] ;
           }
           if( cset!=NULL ){
             int allgood=1 ;
             for( ip=0 ; ip<cset->nperm ; ip++ ){
               signed char *sgn=cset->sign+(size_t)ip*nsub ;
               for( gg=0 ; gg<ngroup ; gg++ ) geffect[gg]=NAN ;
               for( gg=0 ; gg<ngroup ; gg++ ){
                 int mm=gcount[gg] ;
                 float a ;
                 float b ;
                 for( si=0 ; si<mm ; si++ ){
                   int sj=gmember[gg][si] ;
                   int sw=(sgn[sj]<0) ;
                   sel0[si]=sj+sw*nsub ; sel1[si]=sj+(1-sw)*nsub ;
                 }
                 if( method==INSYNC_METHOD_PAIRWISE ){
                   a=INSYNC_pairwise_indexed(2*nsub,csm->mat,mm,sel0,summary,scratch,NULL) ;
                   b=INSYNC_pairwise_indexed(2*nsub,csm->mat,mm,sel1,summary,scratch,NULL) ;
                 } else {
                   a=INSYNC_loo_indexed_metric(2*nsub,nt,cdata,mm,sel0,summary,
                        corr_metric,ref,values,rank1,rank2,NULL) ;
                   b=INSYNC_loo_indexed_metric(2*nsub,nt,cdata,mm,sel1,summary,
                        corr_metric,ref,values,rank1,rank2,NULL) ;
                 }
                 geffect[gg]=a-b ;
                 condnull[(size_t)gg*cset->nperm+ip]=geffect[gg] ;
                 if( !isfinite(geffect[gg]) ) allgood=0 ;
               }
               if( ngroup==2 )
                 condnull[(size_t)ngroup*cset->nperm+ip]=geffect[0]-geffect[1] ;
             }
             for( e=0 ; e<neffect ; e++ ){
               int good=allgood ;
               int elem=e*nwork+vv ;
               float obs=ceffect[e] ;
               if( !isfinite(obs) ) good=0 ;
               if( good ){
                 float ocmp=(condition_tail==PERM_TAIL_TWO)?fabsf(obs):obs ;
                 condition_valid[elem]=1 ; cresult->stat[elem]=obs ;
                 for( ip=0 ; ip<cset->nperm ; ip++ ){
                   float st=condnull[(size_t)e*cset->nperm+ip] ;
                   float cmp=(condition_tail==PERM_TAIL_TWO)?fabsf(st):st ;
                   if( cmp>=ocmp ) cresult->cnt_unc[elem]++ ; if( cmp>c_mymax[ip] ) c_mymax[ip]=cmp ;
                 }
               }
             }
           }
         }
       }
       insync_progress_advance(&progress) ;
     }

     if( pset!=NULL ){
#ifdef USE_OMP
#pragma omp critical(insync_group_max)
#endif
       { for( si=0 ; si<pset->nperm ; si++ ){
           if( my_max[si]>presult->max_null[si] ) presult->max_null[si]=my_max[si] ;
           if( my_max_neg!=NULL && my_max_neg[si]>group_max_neg[si] )
             group_max_neg[si]=my_max_neg[si] ;
         } }
     }
     if( tresult!=NULL ){
#ifdef USE_OMP
#pragma omp critical(insync_temporal_max)
#endif
       { for( si=0 ; si<nnull ; si++ ) if( t_mymax[si]>tresult->max_null[si] ) tresult->max_null[si]=t_mymax[si] ; }
     }
     if( cset!=NULL ){
#ifdef USE_OMP
#pragma omp critical(insync_condition_max)
#endif
       { for( si=0 ; si<cset->nperm ; si++ ) if( c_mymax[si]>cresult->max_null[si] ) cresult->max_null[si]=c_mymax[si] ; }
     }
     free(data); free(scratch); free(ref); free(values); free(rank1); free(rank2);
     free(sample); free(sel0); free(sel1); free(goodtime);
     free(bootdraw); free(permnull); free(my_max); free(my_max_neg);
     free(tnull); free(t_mymax); free(surrogate);
     free(lagtab); free(lagprep); free(lagnorm); free(phspec); free(phwork);
     free(cdata); free(ceffect); free(geffect); free(condnull); free(c_mymax);
     THD_simmat_free(sm); THD_simmat_free(nullsm); THD_simmat_free(csm); THD_simmat_free(pairsm);
   }

   if( bad_data!=LLONG_MAX ){
     long long q=bad_data ;
     int ot=(int)(q%ntime_input) ;
     int dd ;
     int ci ;
     int si ;
     int rr ;
     q/=ntime_input ; dd=(int)(q%ndset) ; ci=dd/nsub ; si=dd%nsub ;
     rr=rowmap[si*ncond+ci] ; q/=ndset ;
     if( roi_only )
       ERROR_exit(PROGRAM_NAME ": nonfinite retained input at ROI %d, time %d, "
                  "Subj %s, condition %s, dataset '%s'; use -missing common only "
                  "when a synchronized complete-case estimand is intended",
                  roi_val[q],ot+1,cindex?cindex->level[0][si]:tab->subj[si],
                  clabel[ci],tab->fname[rr]) ;
     ERROR_exit(PROGRAM_NAME ": nonfinite retained input at voxel %lld, time %d, "
                "Subj %s, condition %s, dataset '%s'; use -missing common only "
                "when a synchronized complete-case estimand is intended",
                widx?(long long)widx[q]:q,ot+1,cindex?cindex->level[0][si]:tab->subj[si],
                clabel[ci],tab->fname[rr]) ;
   }

   /* Convert empirical exceedance counts and synchronized max-null arrays to
      uncorrected p, signed z, BH-FDR q, and max-FWE output maps. */
   if( presult!=NULL ){
     int nelem=ncond*nwork ;
     int nvalid=0 ;
     for( ii=0 ; ii<nelem ; ii++ ) if( infer_valid[ii] ) nvalid++ ;
     if( nvalid<1 ) ERROR_exit(PROGRAM_NAME ": no valid group-permutation cells") ;
     for( ii=0 ; ii<pset->nperm ; ii++ ) if( presult->max_null[ii]==-FLT_MAX ) presult->max_null[ii]=0.0f ;
     if( tail==INSYNC_TAIL_BI ){
       for( ii=0 ; ii<pset->nperm ; ii++ )
         if( group_max_neg[ii]==-FLT_MAX ) group_max_neg[ii]=0.0f ;
       insync_finish_bisided(presult,infer_valid,group_max_neg) ;
     } else THD_perm_result_finish(presult,infer_valid) ;
     qval=(float *)malloc(sizeof(float)*(size_t)nelem) ;
     THD_bh_fdr_masked(nelem,presult->p_unc,infer_valid,qval) ;
     for( cc=0 ; cc<ncond ; cc++ ){
       size_t off=(size_t)cc*nwork ;
       size_t nb=sizeof(float)*(size_t)nwork ;
       int bs=perm_slot+5*cc ;
       memcpy(outval[bs],presult->p_unc+off,nb); memcpy(outval[bs+1],presult->z_unc+off,nb);
       memcpy(outval[bs+2],qval+off,nb); memcpy(outval[bs+3],presult->p_fwe+off,nb); memcpy(outval[bs+4],presult->z_fwe+off,nb);
     }
     if( !quiet ) INFO_message(PROGRAM_NAME ": group-permutation family contains "
                               "%d valid condition-by-voxel cells",nvalid) ;
   }
   if( tresult!=NULL ){
     int nelem=ncond*ngroup*nwork ;
     int nvalid=0 ;
     for( ii=0 ; ii<nelem ; ii++ ) if( temporal_valid[ii] ) nvalid++ ;
     if( nvalid<1 ) ERROR_exit(PROGRAM_NAME ": no valid temporal-null cells") ;
     for( ii=0 ; ii<nnull ; ii++ ) if( tresult->max_null[ii]==-FLT_MAX ) tresult->max_null[ii]=0.0f ;
     THD_perm_result_finish(tresult,temporal_valid) ; temporal_qval=(float *)malloc(sizeof(float)*(size_t)nelem) ;
     THD_bh_fdr_masked(nelem,tresult->p_unc,temporal_valid,temporal_qval) ;
     for( cc=0 ; cc<ncond ; cc++ ) for( kk=0 ; kk<ngroup ; kk++ ){
       size_t off=(size_t)(cc*ngroup+kk)*nwork ;
       size_t nb=sizeof(float)*(size_t)nwork ;
       int bs=temporal_slot+5*(cc*ngroup+kk) ;
       memcpy(outval[bs],tresult->p_unc+off,nb); memcpy(outval[bs+1],tresult->z_unc+off,nb);
       memcpy(outval[bs+2],temporal_qval+off,nb); memcpy(outval[bs+3],tresult->p_fwe+off,nb); memcpy(outval[bs+4],tresult->z_fwe+off,nb);
     }
     if( !quiet ) INFO_message(PROGRAM_NAME ": temporal family contains %d valid "
                               "condition-by-group-by-voxel cells",nvalid) ;
   }
   if( cresult!=NULL ){
     int nelem=neffect*nwork ;
     int nvalid=0 ;
     for( ii=0 ; ii<nelem ; ii++ ) if( condition_valid[ii] ) nvalid++ ;
     if( nvalid<1 ) ERROR_exit(PROGRAM_NAME ": no valid condition-swap cells") ;
     for( ii=0 ; ii<cset->nperm ; ii++ ) if( cresult->max_null[ii]==-FLT_MAX ) cresult->max_null[ii]=0.0f ;
     THD_perm_result_finish(cresult,condition_valid) ; condition_qval=(float *)malloc(sizeof(float)*(size_t)nelem) ;
     THD_bh_fdr_masked(nelem,cresult->p_unc,condition_valid,condition_qval) ;
     for( kk=0 ; kk<neffect ; kk++ ){
       size_t off=(size_t)kk*nwork ;
       size_t nb=sizeof(float)*(size_t)nwork ;
       int bs=condperm_slot+5*kk ;
       memcpy(outval[bs],cresult->p_unc+off,nb); memcpy(outval[bs+1],cresult->z_unc+off,nb);
       memcpy(outval[bs+2],condition_qval+off,nb); memcpy(outval[bs+3],cresult->p_fwe+off,nb); memcpy(outval[bs+4],cresult->z_fwe+off,nb);
     }
     if( !quiet ) INFO_message(PROGRAM_NAME ": condition-swap family contains "
                               "%d valid effect-by-voxel cells",nvalid) ;
   }

   /* The subject data are no longer needed; release them before the output
      maps are spread back onto the full grid. */
   store_needed_late=(use_store && !roi_only && (atlas_name!=NULL || eo.edges || eo.rss || eo.events)) ;
   if( use_store && !store_needed_late ) insync_free_store(&src) ;
   /* Results so far live in work-unit space (in-mask voxels, or ROIs).  Paint
      them onto the input grid; everything below assembles full-grid bricks.
      The work-space copies are kept for the -roi_only table. */
   outwork=outval ;
   if( vmap!=NULL ){
     float *outside=(float *)calloc((size_t)nout,sizeof(float)) ;
     /* P, Q and FWE-P bricks read 1 (not 0) where no test was run. */
     if( pset!=NULL ) for( cc=0 ; cc<ncond ; cc++ )
       { outside[perm_slot+5*cc]=outside[perm_slot+5*cc+2]=outside[perm_slot+5*cc+3]=1.0f ; }
     if( tresult!=NULL ) for( cc=0 ; cc<ncond*ngroup ; cc++ )
       { outside[temporal_slot+5*cc]=outside[temporal_slot+5*cc+2]=outside[temporal_slot+5*cc+3]=1.0f ; }
     if( cset!=NULL ) for( cc=0 ; cc<neffect ; cc++ )
       { outside[condperm_slot+5*cc]=outside[condperm_slot+5*cc+2]=outside[condperm_slot+5*cc+3]=1.0f ; }
     outval=(float **)calloc((size_t)nout,sizeof(float *)) ;
     for( kk=0 ; kk<nout ; kk++ ) outval[kk]=insync_expand(outwork[kk],vmap,nvox,outside[kk]) ;
     free(outside) ;
     if( !roi_only ){ for( kk=0 ; kk<nout ; kk++ ){ free(outwork[kk]) ; outwork[kk]=NULL ; } }
   }
   out=EDIT_empty_copy(first) ;
   if( out_view>=0 ){
     EDIT_dset_items(out,ADN_view_type,out_view,ADN_none) ;
     MCW_strncpy(out->atlas_space,out_space,THD_MAX_NAME) ;
   }
   EDIT_dset_items(out,ADN_prefix,prefix,ADN_nvals,nout,ADN_ntt,0,ADN_brick_fac,NULL,
                   ADN_type,HEAD_FUNC_TYPE,ADN_func_type,FUNC_BUCK_TYPE,ADN_datum_all,MRI_float,ADN_none) ;
   tross_Copy_History(first,out) ; tross_Make_History(PROGRAM_NAME,argc,argv,out) ;
   for( cc=0 ; cc<ncond ; cc++ ) for( kk=0 ; kk<ngroup ; kk++ ){
     char who[256] ;
     char label[64] ;
     int bs=2*(cc*ngroup+kk) ;
     if( ncond>1 ) snprintf(who,sizeof(who),"%s_%s",clabel[cc],glabel[kk]) ; else snprintf(who,sizeof(who),"%s",glabel[kk]) ;
     insync_brick_label(label,sizeof(label),"ISC",who); EDIT_substitute_brick(out,bs,MRI_float,outval[bs]); EDIT_BRICK_LABEL(out,bs,label); EDIT_BRICK_TO_NOSTAT(out,bs);
     insync_brick_label(label,sizeof(label),"N",who); EDIT_substitute_brick(out,bs+1,MRI_float,outval[bs+1]); EDIT_BRICK_LABEL(out,bs+1,label); EDIT_BRICK_TO_NOSTAT(out,bs+1);
   }
   if( diff_slot>=0 ) for( cc=0 ; cc<ncond ; cc++ ){
     char who[256] ;
     char label[64] ;
     char both[160] ;
     snprintf(both,sizeof(both),"%s-%s",glabel[0],glabel[1]) ;
     if( ncond>1 ) snprintf(who,sizeof(who),"%s_%s",clabel[cc],both); else snprintf(who,sizeof(who),"%s",both);
     insync_brick_label(label,sizeof(label),"ISC",who); EDIT_substitute_brick(out,diff_slot+cc,MRI_float,outval[diff_slot+cc]); EDIT_BRICK_LABEL(out,diff_slot+cc,label); EDIT_BRICK_TO_NOSTAT(out,diff_slot+cc);
   }
   if( nboot>0 ) for( cc=0 ; cc<ncond ; cc++ ) for( kk=0 ; kk<ngroup ; kk++ ){
     char who[256] ;
     char label[64] ;
     int bs=boot_slot+2*(cc*ngroup+kk) ;
     if( ncond>1 ) snprintf(who,sizeof(who),"%s_%s",clabel[cc],glabel[kk]); else snprintf(who,sizeof(who),"%s",glabel[kk]);
     insync_brick_label(label,sizeof(label),"BootLo",who); EDIT_substitute_brick(out,bs,MRI_float,outval[bs]); EDIT_BRICK_LABEL(out,bs,label); EDIT_BRICK_TO_NOSTAT(out,bs);
     insync_brick_label(label,sizeof(label),"BootHi",who); EDIT_substitute_brick(out,bs+1,MRI_float,outval[bs+1]); EDIT_BRICK_LABEL(out,bs+1,label); EDIT_BRICK_TO_NOSTAT(out,bs+1);
   }
   if( pset!=NULL ) for( cc=0 ; cc<ncond ; cc++ ){
     static char *stem[5]={"P","Z","Q","PFWE","ZFWE"} ;
     char who[256] ;
     char label[64] ;
     int bs=perm_slot+5*cc ;
     if( ncond>1 ) snprintf(who,sizeof(who),"%s_%s-%s",clabel[cc],glabel[0],glabel[1]); else snprintf(who,sizeof(who),"%s-%s",glabel[0],glabel[1]);
     for( kk=0 ; kk<5 ; kk++ ){ insync_brick_label(label,sizeof(label),stem[kk],who); EDIT_substitute_brick(out,bs+kk,MRI_float,outval[bs+kk]); EDIT_BRICK_LABEL(out,bs+kk,label); if(kk==1||kk==4)EDIT_BRICK_TO_FIZT(out,bs+kk);else EDIT_BRICK_TO_NOSTAT(out,bs+kk); }
   }
   if( tresult!=NULL ) for( cc=0 ; cc<ncond ; cc++ ) for( kk=0 ; kk<ngroup ; kk++ ){
     static char *stem[5]={"TP","TZ","TQ","TPFWE","TZFWE"} ;
     char who[256] ;
     char label[64] ;
     int jj2 ;
     int bs=temporal_slot+5*(cc*ngroup+kk) ;
     if( ncond>1 ) snprintf(who,sizeof(who),"%s_%s",clabel[cc],glabel[kk]); else snprintf(who,sizeof(who),"%s",glabel[kk]);
     for( jj2=0 ; jj2<5 ; jj2++ ){ insync_brick_label(label,sizeof(label),stem[jj2],who); EDIT_substitute_brick(out,bs+jj2,MRI_float,outval[bs+jj2]); EDIT_BRICK_LABEL(out,bs+jj2,label); if(jj2==1||jj2==4)EDIT_BRICK_TO_FIZT(out,bs+jj2);else EDIT_BRICK_TO_NOSTAT(out,bs+jj2); }
   }
   if( have_contrast ) for( kk=0 ; kk<neffect ; kk++ ){
     char who[320] ;
     char label[64] ;
     if( kk<ngroup ) snprintf(who,sizeof(who),"%s-%s_%s",contrast_name0,contrast_name1,glabel[kk]) ;
     else snprintf(who,sizeof(who),"%s-%s_%s-%s",glabel[0],glabel[1],contrast_name0,contrast_name1) ;
     insync_brick_label(label,sizeof(label),(kk<ngroup)?"Cond":"GxCond",who); EDIT_substitute_brick(out,contrast_slot+kk,MRI_float,outval[contrast_slot+kk]); EDIT_BRICK_LABEL(out,contrast_slot+kk,label); EDIT_BRICK_TO_NOSTAT(out,contrast_slot+kk);
     if( cset!=NULL ){
       static char *stem[5]={"CP","CZ","CQ","CPFWE","CZFWE"} ;
       int jj2 ;
       int bs=condperm_slot+5*kk ;
       for( jj2=0 ; jj2<5 ; jj2++ ){ insync_brick_label(label,sizeof(label),stem[jj2],who); EDIT_substitute_brick(out,bs+jj2,MRI_float,outval[bs+jj2]); EDIT_BRICK_LABEL(out,bs+jj2,label); if(jj2==1||jj2==4)EDIT_BRICK_TO_FIZT(out,bs+jj2);else EDIT_BRICK_TO_NOSTAT(out,bs+jj2); }
     }
   }
   if( validtr_slot>=0 ){
     EDIT_substitute_brick(out,validtr_slot,MRI_float,outval[validtr_slot]) ;
     EDIT_BRICK_LABEL(out,validtr_slot,"ValidTR") ;
     EDIT_BRICK_TO_NOSTAT(out,validtr_slot) ;
   }
   DSET_write(out) ; if( !quiet ) WROTE_DSET(out) ;

   if( pairval!=NULL ){
     FILE *fp ;
     char table_name[THD_MAX_NAME] ;
     char dataset_name[THD_MAX_NAME] ;
     size_t pm=0 ;

     if( vmap!=NULL ){
       /* Paint each pair map onto the full grid, one brick at a time. */
       float *pfull=(float *)calloc(npairmap*(size_t)nvox,sizeof(float)) ;
       size_t pb ;
       if( pfull==NULL ) ERROR_exit(PROGRAM_NAME ": cannot allocate %lu full-grid pair-map values",
                                    (unsigned long)(npairmap*(size_t)nvox)) ;
       for( pb=0 ; pb<npairmap ; pb++ ){
         const float *wv=pairval+pb*(size_t)nwork ;
         float *dv=pfull+pb*(size_t)nvox ;
         for( vv=0 ; vv<nvox ; vv++ ) if( vmap[vv]>=0 ) dv[vv]=wv[vmap[vv]] ;
       }
       free(pairval) ; pairval=pfull ;
     }
     /* Pair maps are stored in condition-pair-major, subject-edge-minor
        order.  The emitted tables point directly to those brick selectors. */
     pairout=EDIT_empty_copy(first) ;
     if( out_view>=0 ){
       EDIT_dset_items(pairout,ADN_view_type,out_view,ADN_none) ;
       MCW_strncpy(pairout->atlas_space,out_space,THD_MAX_NAME) ;
     }
     EDIT_dset_items(pairout,ADN_prefix,pair_prefix,ADN_nvals,(int)npairmap,ADN_ntt,0,ADN_brick_fac,NULL,
                     ADN_type,HEAD_FUNC_TYPE,ADN_func_type,FUNC_BUCK_TYPE,ADN_datum_all,MRI_float,ADN_none) ;
     tross_Copy_History(first,pairout); tross_Make_History(PROGRAM_NAME,argc,argv,pairout);
     for( cc=0 ; cc<ncond ; cc++ ) for( jj=0 ; jj<ncond ; jj++ ){
       int aa ;
       int bb ;
       int pair=0 ;
       for( aa=0 ; aa<nsub ; aa++ ) for( bb=aa+1 ; bb<nsub ; bb++,pair++,pm++ ){
         char la[64] ;
         char lb[64] ;
         char lc0[64] ;
         char lc1[64] ;
         char label[64] ;
         char who[256] ;
         const char *sa=cindex?cindex->level[0][aa]:tab->subj[aa] ;
         const char *sb=cindex?cindex->level[0][bb]:tab->subj[bb] ;
         insync_safe_token(la,sizeof(la),sa); insync_safe_token(lb,sizeof(lb),sb); insync_safe_token(lc0,sizeof(lc0),clabel[cc]); insync_safe_token(lc1,sizeof(lc1),clabel[jj]);
         snprintf(who,sizeof(who),"%s_%s_%s_%s",lc0,lc1,la,lb); insync_brick_label(label,sizeof(label),"Z",who);
         EDIT_substitute_brick(pairout,(int)pm,MRI_float,pairval+pm*(size_t)nvox); EDIT_BRICK_LABEL(pairout,(int)pm,label); EDIT_BRICK_TO_NOSTAT(pairout,(int)pm);
       }
     }
     DSET_write(pairout); if( !quiet ) WROTE_DSET(pairout);
     snprintf(dataset_name,sizeof(dataset_name),"%s",DSET_BRIKNAME(pairout));
     snprintf(table_name,sizeof(table_name),ncond==1?"%s.3dISC.txt":"%s.3dISC.all.txt",
              pair_prefix) ;
     if( !THD_ok_overwrite() && THD_is_file(table_name) )
       ERROR_exit(PROGRAM_NAME ": 3dISC table '%s' already exists",table_name) ;
     fp=fopen(table_name,"w");
     if( fp==NULL ) ERROR_exit(PROGRAM_NAME ": cannot write '%s'",table_name) ;
     insync_3disc_header(fp,tab,icol_group,cindex?cindex->icol[1]:-1,icol_censor) ;
     pm=0 ;
     for( cc=0 ; cc<ncond ; cc++ ) for( jj=0 ; jj<ncond ; jj++ ){
       int aa ;
       int bb ;
       for( aa=0 ; aa<nsub ; aa++ ) for( bb=aa+1 ; bb<nsub ; bb++,pm++ ){
         insync_3disc_row(fp,tab,cindex,rowmap,ncond,clabel,group,glabel,
                          cc,jj,aa,bb,icol_group,icol_censor,dataset_name,pm) ;
       }
     }
     fclose(fp) ;

     /* Current 3dISC requires exactly one row for every unique subject pair.
        A repeated-condition table therefore has to be split by ordered
        condition pairing. Keep the complete table above for provenance and
        write directly runnable nchoose2-row tables here. */
     if( ncond>1 ) for( cc=0 ; cc<ncond ; cc++ ) for( jj=0 ; jj<ncond ; jj++ ){
       int aa ;
       int bb ;
       char ca[64] ;
       char cb[64] ;
       insync_safe_token(ca,sizeof(ca),clabel[cc]) ;
       insync_safe_token(cb,sizeof(cb),clabel[jj]) ;
       snprintf(table_name,sizeof(table_name),"%s.%s__%s.3dISC.txt",
                pair_prefix,ca,cb) ;
       if( !THD_ok_overwrite() && THD_is_file(table_name) )
         ERROR_exit(PROGRAM_NAME ": 3dISC table '%s' already exists",table_name) ;
       fp=fopen(table_name,"w") ;
       if( fp==NULL ) ERROR_exit(PROGRAM_NAME ": cannot write '%s'",table_name) ;
       insync_3disc_header(fp,tab,icol_group,cindex->icol[1],icol_censor) ;
       pm=((size_t)cc*ncond+jj)*nedge ;
       for( aa=0 ; aa<nsub ; aa++ ) for( bb=aa+1 ; bb<nsub ; bb++,pm++ )
         insync_3disc_row(fp,tab,cindex,rowmap,ncond,clabel,group,glabel,
                          cc,jj,aa,bb,icol_group,icol_censor,dataset_name,pm) ;
       fclose(fp) ;
     }
     if( !quiet ) INFO_message(PROGRAM_NAME ": wrote %lu Fisher-z pair maps and %s",
                    (unsigned long)npairmap,
                    (ncond==1)?"one ready-to-run 3dISC table":
                               "one ready-to-run 3dISC table per condition pairing") ;
   }

   if( roi_only ){
     /* One row per ROI with every statistic in the bucket, in brick order. */
     char sname[THD_MAX_NAME] ;
     FILE *sfp ;
     int bk, r ;
     snprintf(sname,sizeof(sname),"%s.roi.1D",prefix) ;
     sfp=insync_open_text(sname) ;
     fprintf(sfp,"ROI ROI_Label NVoxel") ;
     for( bk=0 ; bk<nout ; bk++ ) fprintf(sfp," %s",DSET_BRICK_LABEL(out,bk)) ;
     fprintf(sfp,"\n") ;
     for( r=0 ; r<nroi ; r++ ){
       fprintf(sfp,"%d %s %d",roi_val[r],roi_lab[r],roi_nv[r]) ;
       for( bk=0 ; bk<nout ; bk++ ) fprintf(sfp," %.9g",outwork[bk][r]) ;
       fprintf(sfp,"\n") ;
     }
     fclose(sfp) ;
     if( !quiet ) INFO_message(PROGRAM_NAME ": wrote %d ROI rows of %d statistics to %s",nroi,nout,sname) ;
     for( bk=0 ; bk<nout ; bk++ ) free(outwork[bk]) ;
   }

   if( atlas_name!=NULL && !roi_only )
     insync_write_roi_outputs(atlas_name,roi_sel,prefix,matrix_name,first,&src,
       nsub,ncond,ntime_input,ntime,time_index,mask,missing_policy,corr_metric,
       method,summary,ngroup,group,gcount,gmember,glabel,tab,cindex,rowmap,
       clabel,quiet) ;

   if( eo.edges || eo.rss || eo.events )
     insync_write_edge_outputs(atlas_name,roi_sel,prefix,first,&src,nsub,ncond,
       ntime,time_index,mask,missing_policy,corr_metric,method,summary,ngroup,
       gcount,gmember,glabel,tab,cindex,clabel,&eo,quiet) ;

   if( store_needed_late ) insync_free_store(&src) ;

   if( !quiet ) INFO_message(PROGRAM_NAME ": estimator=%s correlation=%s "
                "summary=%s, %d subjects, %d condition%s, %d retained time points",
                (method==INSYNC_METHOD_PAIRWISE)?"pairwise":"loo",
                (corr_metric==SIM_PEARSON)?"pearson":"spearman",
                (summary==INSYNC_SUMMARY_MEDIAN)?"median":"fisher_mean",
                nsub,ncond,(ncond==1)?"":"s",ntime) ;
   if( invalid_total>0 ) WARNING_message(PROGRAM_NAME ": %lld group estimates were invalid and stored as zero",invalid_total) ;
   if( boot_invalid_total>0 ) WARNING_message(PROGRAM_NAME ": %lld bootstrap intervals had fewer than 10 usable draws",boot_invalid_total) ;
   if( temporal_invalid_total>0 ) WARNING_message(PROGRAM_NAME ": %lld temporal tests were invalid",temporal_invalid_total) ;
   if( condition_invalid_total>0 ) WARNING_message(PROGRAM_NAME ": %lld planned condition effects were invalid",condition_invalid_total) ;
   return 0 ;
}
