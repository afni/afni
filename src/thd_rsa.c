#include "thd_rsa.h"
#include "mrilib.h"
#include "thd_phasefft.h"

#include <limits.h>
#include <float.h>
#include <math.h>
#include <stdint.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

struct THD_rsa_rdm_workspace {
   int max_subject, max_condition, max_triangle ;
   int nsubject, ncondition, ntriangle, nstored, is_distance ;
   float *triangle ;
   float *distance ;
   unsigned char *stored ;
} ;

static void thd_rsa_error( char *err, size_t errlen, const char *fmt, ... )
{
   va_list ap ;
   if( err==NULL || errlen==0 ) return ;
   va_start(ap,fmt) ; vsnprintf(err,errlen,fmt,ap) ; va_end(ap) ;
}

THD_rsa_rdm_workspace *
THD_rsa_rdm_workspace_new( int max_subject, int max_condition )
{
   THD_rsa_rdm_workspace *ws ; size_t nval,ntri ; int maxtri ;
   if( max_subject<2 || max_condition<2 ) return NULL ;
   ntri=(size_t)max_condition*(size_t)(max_condition-1)/2u ;
   if( ntri<1 || ntri>INT_MAX || (size_t)max_subject>SIZE_MAX/ntri ) return NULL ;
   maxtri=(int)ntri ;
   nval=(size_t)max_subject*maxtri ;
   if( nval>SIZE_MAX/sizeof(float) ) return NULL ;
   ws=(THD_rsa_rdm_workspace *)calloc(1,sizeof(*ws)) ;
   if( ws==NULL ) return NULL ;
   ws->triangle=(float *)malloc(sizeof(float)*nval) ;
   ws->distance=(float *)malloc(sizeof(float)*nval) ;
   ws->stored=(unsigned char *)calloc((size_t)max_subject,1) ;
   if( ws->triangle==NULL || ws->distance==NULL || ws->stored==NULL ){
     THD_rsa_rdm_workspace_free(ws) ; return NULL ;
   }
   ws->max_subject=max_subject ; ws->max_condition=max_condition ;
   ws->max_triangle=maxtri ; ws->is_distance=-1 ;
   return ws ;
}

void THD_rsa_rdm_workspace_free( THD_rsa_rdm_workspace *ws )
{
   if( ws==NULL ) return ;
   free(ws->triangle) ; free(ws->distance) ; free(ws->stored) ; free(ws) ;
}

int THD_rsa_rdm_workspace_reset( THD_rsa_rdm_workspace *ws,
                                 int nsubject, int ncondition,
                                 char *err, size_t errlen )
{
   if( err!=NULL && errlen>0 ) err[0]='\0' ;
   if( ws==NULL ){
     thd_rsa_error(err,errlen,"null RSA RDM workspace") ; return 0 ;
   }
   if( nsubject<2 || nsubject>ws->max_subject ||
       ncondition<2 || ncondition>ws->max_condition ){
     thd_rsa_error(err,errlen,
       "RSA RDM workspace capacity is %d subjects x %d conditions, not %d x %d",
       ws->max_subject,ws->max_condition,nsubject,ncondition) ;
     return 0 ;
   }
   ws->nsubject=nsubject ; ws->ncondition=ncondition ;
   ws->ntriangle=THD_NTRI(ncondition) ; ws->nstored=0 ; ws->is_distance=-1 ;
   memset(ws->stored,0,(size_t)nsubject) ;
   return 1 ;
}

int THD_rsa_center_condition_patterns( int ncondition, int nfeature,
                                       float *patterns,
                                       char *err, size_t errlen )
{
   int cc,vv ; size_t qq,nval ;
   if( err!=NULL && errlen>0 ) err[0]='\0' ;
   if( ncondition<2 || nfeature<1 || patterns==NULL ){
     thd_rsa_error(err,errlen,"condition centering needs at least 2 conditions and 1 feature") ;
     return 0 ;
   }
   nval=(size_t)ncondition*nfeature ;
   for( qq=0 ; qq<nval ; qq++ ) if( !isfinite(patterns[qq]) ){
     thd_rsa_error(err,errlen,"condition pattern value %lu is not finite",
                   (unsigned long)(qq+1)) ;
     return 0 ;
   }
   for( vv=0 ; vv<nfeature ; vv++ ){
     double mean=0.0 ;
     for( cc=0 ; cc<ncondition ; cc++ ) mean+=patterns[(size_t)cc*nfeature+vv] ;
     mean/=(double)ncondition ;
     for( cc=0 ; cc<ncondition ; cc++ )
       patterns[(size_t)cc*nfeature+vv]-=(float)mean ;
   }
   return 1 ;
}

THD_simmat * THD_rsa_condition_rdm( int ncondition, int nfeature,
                                    float *patterns, int metric,
                                    int center_conditions,
                                    char *err, size_t errlen )
{
   THD_simmat *out ;
   if( err!=NULL && errlen>0 ) err[0]='\0' ;
   if( ncondition<2 || nfeature<2 || patterns==NULL ){
     thd_rsa_error(err,errlen,
                   "condition RDM construction needs at least 2 conditions and 2 features") ;
     return NULL ;
   }
   if( metric<SIM_PEARSON || metric>SIM_EUCLID ){
     thd_rsa_error(err,errlen,"unknown condition-pattern metric %d",metric) ;
     return NULL ;
   }
   if( center_conditions && metric!=SIM_EUCLID &&
       !THD_rsa_center_condition_patterns(ncondition,nfeature,patterns,err,errlen) )
     return NULL ;
   out=THD_simmat_from_features(ncondition,nfeature,patterns,metric) ;
   if( out==NULL ) thd_rsa_error(err,errlen,
                                 "cannot construct a finite condition RDM") ;
   return out ;
}

int THD_rsa_simmat_to_triangle( const THD_simmat *sm, float *triangle,
                                int as_distance,
                                char *err, size_t errlen )
{
   int ii,jj,kk=0 ;
   if( err!=NULL && errlen>0 ) err[0]='\0' ;
   if( sm==NULL || sm->mat==NULL || sm->n<2 || triangle==NULL ){
     thd_rsa_error(err,errlen,"cannot compact a null or malformed RSA matrix") ;
     return 0 ;
   }
   for( ii=0 ; ii<sm->n ; ii++ ) for( jj=ii+1 ; jj<sm->n ; jj++ ){
     float vv=sm->mat[ii*sm->n+jj] ;
     if( !isfinite(vv) ){
       thd_rsa_error(err,errlen,"RSA matrix entry (%d,%d) is not finite",ii+1,jj+1) ;
       return 0 ;
     }
     triangle[kk++]=(as_distance && !sm->is_dist) ? 1.0f-vv : vv ;
   }
   return 1 ;
}

int THD_rsa_rdm_workspace_store( THD_rsa_rdm_workspace *ws,
                                 int subject, const THD_simmat *rdm,
                                 int as_distance,
                                 char *err, size_t errlen )
{
   int sense ; float *dst ;
   if( err!=NULL && errlen>0 ) err[0]='\0' ;
   if( ws==NULL || ws->nsubject<2 || ws->ncondition<2 ){
     thd_rsa_error(err,errlen,"RSA RDM workspace has not been reset") ; return 0 ;
   }
   if( subject<0 || subject>=ws->nsubject ){
     thd_rsa_error(err,errlen,"subject index %d is outside [0,%d]",
                   subject,ws->nsubject-1) ;
     return 0 ;
   }
   if( rdm==NULL || rdm->n!=ws->ncondition ){
     thd_rsa_error(err,errlen,"subject %d RDM has %d conditions; expected %d",
                   subject+1,rdm==NULL?0:rdm->n,ws->ncondition) ;
     return 0 ;
   }
   sense=as_distance ? 1 : !!rdm->is_dist ;
   if( ws->is_distance>=0 && ws->is_distance!=sense ){
     thd_rsa_error(err,errlen,"subject RDMs mix similarity and distance senses") ;
     return 0 ;
   }
   dst=ws->triangle+(size_t)subject*ws->ntriangle ;
   if( !THD_rsa_simmat_to_triangle(rdm,dst,as_distance,err,errlen) ) return 0 ;
   ws->is_distance=sense ;
   if( !ws->stored[subject] ){ ws->stored[subject]=1 ; ws->nstored++ ; }
   return 1 ;
}

THD_simmat * THD_rsa_second_order_rdm( THD_rsa_rdm_workspace *ws,
                                       int outer_metric,
                                       char *err, size_t errlen )
{
   float *feature ; size_t nval,qq ; THD_simmat *out ;
   if( err!=NULL && errlen>0 ) err[0]='\0' ;
   if( ws==NULL || ws->nstored!=ws->nsubject || ws->ncondition<3 ){
     thd_rsa_error(err,errlen,
       "second-order RSA needs every subject RDM and at least 3 conditions") ;
     return NULL ;
   }
   if( outer_metric<SIM_PEARSON || outer_metric>SIM_EUCLID ){
     thd_rsa_error(err,errlen,"unknown second-order metric %d",outer_metric) ;
     return NULL ;
   }
   nval=(size_t)ws->nsubject*ws->ntriangle ; feature=ws->triangle ;
   if( !ws->is_distance ){
     for( qq=0 ; qq<nval ; qq++ ) ws->distance[qq]=1.0f-ws->triangle[qq] ;
     feature=ws->distance ;
   }
   out=THD_simmat_from_features(ws->nsubject,ws->ntriangle,feature,outer_metric) ;
   if( out==NULL ) thd_rsa_error(err,errlen,
                                 "cannot construct a finite second-order RDM") ;
   return out ;
}

THD_simmat * THD_rsa_mean_subject_rdms( int nsubject, int ncondition,
                                        const float *triangles, int is_distance,
                                        char *err, size_t errlen )
{
   THD_simmat *out ; float *mean ; int ss,aa,ntri ;
   if( err!=NULL && errlen>0 ) err[0]='\0' ;
   if( nsubject<1 || ncondition<2 || triangles==NULL ){
     thd_rsa_error(err,errlen,"mean RDM needs at least 1 subject and 2 conditions") ;
     return NULL ;
   }
   ntri=THD_NTRI(ncondition) ; mean=(float *)calloc((size_t)ntri,sizeof(float)) ;
   if( mean==NULL ){
     thd_rsa_error(err,errlen,"out of memory averaging subject RDMs") ; return NULL ;
   }
   for( ss=0 ; ss<nsubject ; ss++ ) for( aa=0 ; aa<ntri ; aa++ ){
     float vv=triangles[(size_t)ss*ntri+aa] ;
     if( !isfinite(vv) ){
       thd_rsa_error(err,errlen,"subject %d RDM entry %d is not finite",ss+1,aa+1) ;
       free(mean) ; return NULL ;
     }
     mean[aa]+=vv ;
   }
   for( aa=0 ; aa<ntri ; aa++ ) mean[aa]/=(float)nsubject ;
   out=THD_simmat_new(ncondition) ;
   if( out==NULL ){
     thd_rsa_error(err,errlen,"out of memory constructing the mean RDM") ;
     free(mean) ; return NULL ;
   }
   THD_tri_to_simmat(ncondition,mean,out) ; out->is_dist=!!is_distance ;
   free(mean) ; return out ;
}

/* Undefined RDM correlations are missing outcomes, never artificial zero fits.
   Fisher clipping is fixed for all subjects and is reported in provenance. */
float THD_rsa_fisher_corr(int m, float *a, float *b, int metric,
                                 float *sc1, float *sc2)
{
   int i; float amin=FLT_MAX,amax=-FLT_MAX,bmin=FLT_MAX,bmax=-FLT_MAX,r;
   if(m<2 || !a || !b || !sc1 || !sc2 ||
      (metric!=CMP_PEARSON && metric!=CMP_SPEARMAN)) return NAN;
   for(i=0;i<m;i++) {
     if(!isfinite(a[i]) || !isfinite(b[i])) return NAN;
     if(a[i]<amin) amin=a[i]; if(a[i]>amax) amax=a[i];
     if(b[i]<bmin) bmin=b[i]; if(b[i]>bmax) bmax=b[i];
   }
   if(!(amax>amin) || !(bmax>bmin)) return NAN;
   r=THD_tri_corr(m,a,b,metric,sc1,sc2);
   if(!isfinite(r)) return NAN;
   if(r>0.999999f) r=0.999999f;
   if(r< -0.999999f) r= -0.999999f;
   return atanhf(r);
}

/* Check the condition patterns themselves, since angle metrics otherwise
   turn a constant/zero-norm pattern into finite, misleading RDM entries. */
THD_simmat *THD_rsa_condition_rdm_checked(int nc,int nv,float *pat,int metric,int center)
{
   int c,v;
   if(nc<2 || nv<2 || !pat || metric<SIM_PEARSON || metric>SIM_EUCLID) return NULL;
   if(center && metric!=SIM_EUCLID &&
      !THD_rsa_center_condition_patterns(nc,nv,pat,NULL,0)) return NULL;
   for(c=0;c<nc;c++) {
     float lo=FLT_MAX,hi=-FLT_MAX; double norm=0.0;
     for(v=0;v<nv;v++) {
       float x=pat[(size_t)c*nv+v];
       if(!isfinite(x)) return NULL;
       if(x<lo) lo=x; if(x>hi) hi=x; norm+=(double)x*x;
     }
     if((metric==SIM_PEARSON || metric==SIM_SPEARMAN) && !(hi>lo)) return NULL;
     if(metric==SIM_COSINE && !(norm>0.0)) return NULL;
   }
   return THD_rsa_condition_rdm(nc,nv,pat,metric,0,NULL,0);
}

/* Equal-weight mean over unordered repeat pairs, in Fisher-z units. */
float THD_rsa_repeat_stability( int nrepeat, int ntriangle, float *triangles,
                                int metric, float *sc1, float *sc2 )
{
   double sum=0.0; size_t npair=0; int a,b;
   if(nrepeat<2 || ntriangle<2 || !triangles) return NAN;
   for(a=0;a<nrepeat;a++) for(b=a+1;b<nrepeat;b++) {
     float z=THD_rsa_fisher_corr(ntriangle,triangles+(size_t)a*ntriangle,
                               triangles+(size_t)b*ntriangle,metric,sc1,sc2);
     if(!isfinite(z)) return NAN;
     sum+=z; npair++;
   }
   return (float)(sum/npair);
}

/* Fitted RDM mixtures and synchronized RSA inference. */
struct THD_rsa_fitws {
   int maxtrain , maxtest , ncomp ;
   float *y , *pred , *xflat , **x , *xmean , *xsd , *w , *foldw ;
   float *ty , *tpred ;
};

float *THD_rsa_fitws_weights(THD_rsa_fitws *ws)
{ return ws ? ws->foldw : NULL; }


#define MYatanh(x) ( ((x)<-0.999329f) ? -4.0f : ((x)>+0.999329f) ? +4.0f : atanhf(x) )
static int rsa_float_compare(const void *a, const void *b)
{ float x=*(const float *)a,y=*(const float *)b; return (x<y)?-1:(x>y); }

THD_rsa_fitws * THD_rsa_fitws_new( int rdm_over, int nsub, int nitem, int ncomp )
{
   THD_rsa_fitws *fw; int cc; long long ntri,train,test;
   if((rdm_over!=THD_RSA_SUBJECTS && rdm_over!=THD_RSA_CONDITIONS) ||
      nsub<2 || nitem<2 || ncomp<1) return NULL;
   ntri=(long long)nitem*(nitem-1)/2;
   if(ntri>INT_MAX) return NULL;
   train=(rdm_over==THD_RSA_SUBJECTS)?(long long)(nsub-1)*(nsub-2)/2:(nsub-1)*ntri;
   test=(rdm_over==THD_RSA_SUBJECTS)?nsub-1:ntri;
   if(train<1 || train>INT_MAX || test>INT_MAX ||
      (size_t)ncomp>SIZE_MAX/sizeof(float)/(size_t)train) return NULL;
   fw=(THD_rsa_fitws *)calloc(1,sizeof(*fw));
   if(!fw) return NULL;
   fw->maxtrain=(int)train; fw->maxtest=(int)test;
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
   if(!fw->y || !fw->pred || !fw->xflat || !fw->x || !fw->xmean ||
      !fw->xsd || !fw->w || !fw->foldw || !fw->ty || !fw->tpred){
      THD_rsa_fitws_free(fw); return NULL;
   }
   for( cc=0 ; cc<ncomp ; cc++ ) fw->x[cc]=fw->xflat+(size_t)cc*fw->maxtrain ;
   return fw ;
}

void THD_rsa_fitws_free( THD_rsa_fitws *fw )
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
static int rsa_fit_nnls_ridge( int m, int p, float ridge, THD_rsa_fitws *fw )
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
                                THD_rsa_fitmodel *fm, THD_simmat **mv, int *perm,
                                THD_rsa_fitws *fw, float *meanw,
                                float *foldz, unsigned char *valid )
{
   int hold,a,b,s,cc,m,mt,nok=0 ; double zsum=0.0 ;
   if( meanw!=NULL ) memset(meanw,0,sizeof(float)*fm->ncomp) ;
   if( valid!=NULL ) memset(valid,0,(size_t)nsub) ;
   for( hold=0 ; hold<nsub ; hold++ ){
     m=mt=0 ;
     if( rdm_over==THD_RSA_SUBJECTS ){
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
     if( rdm_over==THD_RSA_SUBJECTS ){
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
                                           THD_rsa_fitmodel *fm, THD_simmat **mv,
                                           int *perm, THD_rsa_condfold *cf,
                                           THD_rsa_fitws *fw, float *meanw,
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

float THD_rsa_fit_cv( int rdm_over, int nsub, int nitem,
                         float *srdm, THD_simmat *neural,
                         THD_rsa_fitmodel *fm, THD_simmat **mv, int *perm,
                         THD_rsa_condfold *cf, THD_rsa_fitws *fw, float *meanw,
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
THD_permstat THD_rsa_fit_superiority_test(
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

/*! Linear-interpolated percentile of x[0..n-1].  x is scratch and is sorted. */
float THD_rsa_percentile( float *x , int n , float prob )
{
   double pos, frac ; int lo, hi ;
   if( x == NULL || n < 1 ) return 0.0f ;
   qsort(x,n,sizeof(float),rsa_float_compare) ;
   if( prob <= 0.0f ) return x[0] ;
   if( prob >= 1.0f ) return x[n-1] ;
   pos = (double)(n-1)*(double)prob ; lo = (int)floor(pos) ; hi = (int)ceil(pos) ;
   frac = pos-(double)lo ;
   return (float)((1.0-frac)*(double)x[lo] + frac*(double)x[hi]) ;
}

void THD_rsa_cond_resample_free( THD_rsa_cond_resample *cb )
{
   if( cb == NULL ) return ;
   free(cb->nitem) ; free(cb->offset) ; free(cb->index) ; free(cb->valid) ; free(cb) ;
}

/*! Extract one condition-bootstrap draw into compact, missing-diagonal-free
    neural/model triangles.  Returns the retained dyad count. */
int THD_rsa_cond_boot_tri( float *neural, int nmod, THD_simmat **model,
                              THD_rsa_cond_resample *cb, int bb, float *y, float **x )
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

void THD_rsa_cond_boot_ci( float *sum, int nsub, THD_rsa_cond_resample *cb,
                              int do_tanh, float alpha, float *draw,
                              float *blo, float *bhi )
{
   int bb, nv=0 ;
   for( bb=0 ; bb < cb->nresample ; bb++ ) if( cb->valid[bb] ){
     float v=sum[bb]/(float)nsub ; draw[nv++]=do_tanh ? tanhf(v) : v ;
   }
   *blo=THD_rsa_percentile(draw,nv,0.5f*alpha) ;
   *bhi=THD_rsa_percentile(draw,nv,1.0f-0.5f*alpha) ;
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
int THD_rsa_dual_boot_ci( int nsub, float *vals, THD_resample_set *sr,
                             THD_rsa_cond_resample *cr, float *cval, int do_tanh,
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

/*! Percentile CI for a classic-RSA group effect.  vals are the independent
    subject effects: Fisher-z correlations for an ordinary model (do_tanh=1),
    or regression coefficients for a joint model (do_tanh=0). */
void THD_rsa_boot_subject_mean( int nsub , float *vals , THD_resample_set *rset,
                                   int do_tanh , float alpha , float *draw,
                                   float *blo , float *bhi )
{
   int bb, jj ;
   for( bb=0 ; bb < rset->nresample ; bb++ ){
     int *ix = rset->index + (size_t)bb*nsub ; float sm=0.0f ;
     for( jj=0 ; jj < nsub ; jj++ ) sm += vals[ix[jj]] ;
     sm /= (float)nsub ; draw[bb] = do_tanh ? tanhf(sm) : sm ;
   }
   *blo = THD_rsa_percentile(draw,rset->nresample,0.5f*alpha) ;
   *bhi = THD_rsa_percentile(draw,rset->nresample,1.0f-0.5f*alpha) ;
}

/*! Percentile interval for a completed LOO prediction vector.  The fitted
    out-of-sample predictions are held fixed and subject rows (prediction plus
    observed target) are resampled together.  For a profile target, the same
    row draw is used for every measure and the reported statistic remains the
    equal-weight mean measure-wise correlation.  This quantifies uncertainty
    over the evaluated subjects, not instability from refitting every fold. */
int THD_rsa_boot_loo_predictions( int n, int p, float *pred, float **target,
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
   *blo=THD_rsa_percentile(draw,ndraw,0.5f*alpha) ;
   *bhi=THD_rsa_percentile(draw,ndraw,1.0f-0.5f*alpha) ;
   return ndraw ;
}

/*! Percentile CI for a plain IS-RSA Mantel effect.  Resampling subjects with
    replacement duplicates rows/columns.  A pair of sampled positions that
    both refer to the same original subject is the artificial diagonal created
    by resampling, not an observed dyad, and is therefore omitted.  Other dyads
    retain their bootstrap multiplicity.  Returns the number of usable draws. */
int THD_rsa_boot_isrsa( THD_simmat *neural , THD_simmat *model , int cmp,
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
   *blo = THD_rsa_percentile(draw,ndraw,0.5f*alpha) ;
   *bhi = THD_rsa_percentile(draw,ndraw,1.0f-0.5f*alpha) ;
   return ndraw ;
}

/*! Percentile CI for an IS-RSA model contrast.  Each subject resample is
    applied jointly to the neural RDM and both model RDMs, so the draw is the
    paired difference r(neural,A)-r(neural,B).  As in rsa_boot_isrsa(), dyads
    between duplicate copies of one original subject are omitted. */
int THD_rsa_boot_isrsa_contrast( THD_simmat *neural , THD_simmat *model_a,
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
   *blo = THD_rsa_percentile(draw,ndraw,0.5f*alpha) ;
   *bhi = THD_rsa_percentile(draw,ndraw,1.0f-0.5f*alpha) ;
   return ndraw ;
}

/*! Build exactly nresample usable subject draws for dyadic IS-RSA.  Draws with
    fewer than three distinct subjects cannot define a correlation after the
    artificial duplicate-copy diagonals are removed, so generate a reserve and
    compact those draws once, globally, before parallel inference. */
THD_resample_set * THD_rsa_isrsa_resample_set( int nsub, int nresample,
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
THD_permstat THD_rsa_isrsa_superiority_test(
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
int THD_rsa_boot_isrsa_regress( THD_simmat *neural, int nmod,
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
     blo[mm][loc]=THD_rsa_percentile(v,ndraw,0.5f*alpha) ;
     bhi[mm][loc]=THD_rsa_percentile(v,ndraw,1.0f-0.5f*alpha) ;
   }
   return ndraw ;
}

/*! Percentile CIs for one IS-RSA commonality request.  The neural and both
    model triangles use the same subject draw, with duplicate-copy diagonal
    dyads omitted, and all raw/partial quantities are recomputed together. */
int THD_rsa_boot_isrsa_commonality( THD_simmat *neural, THD_simmat *model_a,
                                       THD_simmat *model_b, int cmp,
                                       THD_resample_set *rset, THD_rdm_ws *ws,
                                       float alpha, float *y, float *a, float *b,
                                       float *draw, int *seen,
                                       float **blo, float **bhi, int qbase, int loc )
{
   int bb, aa, jj, cc, n=rset->nobs, ndraw=0 ;
   for( bb=0 ; bb < rset->nresample ; bb++ ){
     int *ix=rset->index+(size_t)bb*n, m=0, nu=0 ;
     float comp[THD_NCOMMON] ;
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
     for( cc=0 ; cc < THD_NCOMMON ; cc++ )
       draw[(size_t)cc*rset->nresample+ndraw]=comp[cc] ;
     ndraw++ ;
   }
   if( ndraw < 1 ){
     for( cc=0 ; cc < THD_NCOMMON ; cc++ )
       blo[qbase+cc][loc]=bhi[qbase+cc][loc]=0.0f ;
     return 0 ;
   }
   for( cc=0 ; cc < THD_NCOMMON ; cc++ ){
     float *v=draw+(size_t)cc*rset->nresample ;
     blo[qbase+cc][loc]=THD_rsa_percentile(v,ndraw,0.5f*alpha) ;
     bhi[qbase+cc][loc]=THD_rsa_percentile(v,ndraw,1.0f-0.5f*alpha) ;
   }
   return ndraw ;
}

/*! F8 counterpart for all seven raw three-model regions and the three
    conditional partial-R2 effects. */
int THD_rsa_boot_isrsa_commonality3( THD_simmat *neural, THD_simmat *model_a,
                                        THD_simmat *model_b, THD_simmat *model_c,
                                        int cmp, THD_resample_set *rset,
                                        THD_rdm_ws *ws, float alpha, float *y,
                                        float *a, float *b, float *c, float *draw,
                                        int *seen, float **blo, float **bhi,
                                        int qbase, int loc )
{
   int bb,aa,jj,cc,n=rset->nobs,ndraw=0 ;
   for( bb=0 ; bb<rset->nresample ; bb++ ){
     int *ix=rset->index+(size_t)bb*n,m=0,nu=0 ; float comp[THD_NCOMMON3] ;
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
     for( cc=0 ; cc<THD_NCOMMON3 ; cc++ )
       draw[(size_t)cc*rset->nresample+ndraw]=comp[cc] ;
     ndraw++ ;
   }
   if( ndraw<1 ){
     for( cc=0 ; cc<THD_NCOMMON3 ; cc++ ) blo[qbase+cc][loc]=bhi[qbase+cc][loc]=0.0f ;
     return 0 ;
   }
   for( cc=0 ; cc<THD_NCOMMON3 ; cc++ ){
     float *v=draw+(size_t)cc*rset->nresample ;
     blo[qbase+cc][loc]=THD_rsa_percentile(v,ndraw,0.5f*alpha) ;
     bhi[qbase+cc][loc]=THD_rsa_percentile(v,ndraw,1.0f-0.5f*alpha) ;
   }
   return ndraw ;
}

THD_rsa_cond_resample * THD_rsa_cond_resample_build(int ncond, int ng, const int *gidx, int nresample, long seed, char *err, size_t errlen)
{
   THD_rsa_cond_resample *cb=NULL; THD_resample_set *gr=NULL;
   int *gof=NULL,*gsz=NULL,*gitem=NULL,*seen=NULL;
   int ii,gg,bb,total=0;
   thd_rsa_error(err,errlen,"cannot allocate condition bootstrap");
   if(ncond<3 || ng<3 || ng>ncond || !gidx || nresample<10 || nresample==INT_MAX){ thd_rsa_error(err,errlen,"invalid condition resampling dimensions"); return NULL; }
   for(ii=0;ii<ncond;ii++) if(gidx[ii]<0 || gidx[ii]>=ng){ thd_rsa_error(err,errlen,"invalid condition group index"); return NULL; }
   gsz=(int *)calloc(ng,sizeof(int)) ; gof=(int *)calloc(ng+1,sizeof(int)) ;
   gitem=(int *)malloc(sizeof(int)*ncond) ;
   if( gsz == NULL || gof == NULL || gitem == NULL ) goto bad ;
   for( ii=0 ; ii < ncond ; ii++ ) gsz[gidx[ii]]++ ;
   for( gg=0 ; gg < ng ; gg++ ){
     if(!gsz[gg]){ thd_rsa_error(err,errlen,"empty condition group"); goto bad; }
     gof[gg+1]=gof[gg]+gsz[gg];
   }
   memset(gsz,0,sizeof(int)*ng) ;
   for( ii=0 ; ii < ncond ; ii++ ){
     gg=gidx[ii] ; gitem[gof[gg]+gsz[gg]++]=ii ;
   }

   gr=THD_resample_set_build(ng,nresample,seed) ; if( gr == NULL ) goto bad ;
   cb=(THD_rsa_cond_resample *)calloc(1,sizeof(THD_rsa_cond_resample)) ;
   if( cb == NULL ) goto bad ;
   cb->nresample=nresample ; cb->ncond=ncond ; cb->ngroup=ng ;
   cb->nitem=(int *)calloc(nresample,sizeof(int)) ;
   cb->offset=(int *)calloc(nresample+1,sizeof(int)) ;
   cb->valid=(unsigned char *)calloc(nresample,sizeof(unsigned char)) ;
   if( cb->nitem == NULL || cb->offset == NULL || cb->valid == NULL ) goto bad ;
   for( bb=0 ; bb < nresample ; bb++ ){
     long long ni=0; int *sel=gr->index+(size_t)bb*ng ;
     for( gg=0 ; gg < ng ; gg++ ) ni += gsz[sel[gg]] ;

     if( (long long)cb->offset[bb]+ni > INT_MAX ){
       thd_rsa_error(err,errlen,"condition-bootstrap index set is too large") ; goto bad ;
     }
     if( ni > cb->maxitem ) cb->maxitem=(int)ni ;
     cb->nitem[bb]=(int)ni ; cb->offset[bb+1]=cb->offset[bb]+(int)ni ;
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
       thd_rsa_error(err,errlen,"condition bootstrap sample is too large (%d positions)",
                     cb->maxitem) ; goto bad ;
     }
     cb->maxtri=(int)mt ; }
   if( cb->nvalid < 10 ){
     thd_rsa_error(err,errlen,"only %d condition-bootstrap draws retained 3 distinct\n"
                   "       conditions; need at least 10 usable draws",cb->nvalid) ;
     goto bad ;
   }

   THD_resample_set_free(gr) ; free(gof) ; free(gsz) ; free(gitem) ; free(seen) ;
   return cb ;
bad:
   if( gr != NULL ) THD_resample_set_free(gr) ;
   THD_rsa_cond_resample_free(cb) ; free(gof) ; free(gsz) ; free(gitem) ; free(seen) ;
   return NULL ;
}

/*! One fixed or run-specific subject model against run-specific neural
    geometries. The same subject relabeling is used for every run, so a varying
    behavioral model is permuted as a whole trajectory. Individual run nulls
    and the equal-run mean null remain signed until their two-sided tail and
    max-family statistics have been formed. */
void THD_rsa_runresolved_mantel(
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

/*! Joint, run-resolved standardized dyadic regression.  Every run is fit with
    the fixed and run-varying model RDMs together.  THD_rdm_regress_signed uses
    a model-specific Freedman--Lane reduced model; using the same pset in every
    run preserves each subject's whole behavioral trajectory.  Signed null
    coefficients can therefore be combined into valid equal-run means and
    planned run contrasts before two-sided testing. */
int THD_rsa_runresolved_regress(
   THD_simmat **neural, int nrun, const THD_rsa_run_model *mod, int nmod, int cmp,
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

   if(!mv || !bb || !pr || !pv || (np>0 && !sn)){ err=1; goto done; }

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
done:
   free(mv); free(bb); free(pr); free(pv); if(sn!=NULL)free(sn) ;
   return err ;
}

/*! Cache one real-series Fourier spectrum per subject. */
int THD_rsa_phase_prepare( int nsub, int nt, float *series, THD_rsa_complex *spec )
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
int THD_rsa_phase_draw( THD_phase_set *pset, int iphase, THD_rsa_complex *spec,
                           THD_rsa_complex *work, float *series, THD_simmat *sm,
                           int metric, float *sc1, float *sc2 )
{
   int jj,tt,kk,nt=pset->ntime,nsub=pset->nobs ;
   for( jj=0 ; jj<nsub ; jj++ ){
     THD_rsa_complex *src=spec+(size_t)jj*nt ;
     memcpy(work,src,sizeof(THD_rsa_complex)*nt) ;
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
void THD_rsa_temporal_infer( THD_simmat *neural, int nmod,
                                 THD_simmat **model, int nort,
                                 THD_simmat **orts, int joint,
                                 int ncon, const THD_rsa_contrast *con, float *Fmain,
                                 int neu_metric, int cmp,
                                 THD_timeshift_set *tset,
                                 THD_phase_set *phset, THD_rdm_ws *ws,
                                 THD_rsa_temporal_workspace *tw, float *stat,
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
                                             tw->need,tw->lagtab,tw->prep,tw->norm)) ||
       (phset != NULL && THD_rsa_phase_prepare(nsub,nt,Fmain,tw->phspec)) ){
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

   tw->nullmat->is_dist=(neu_metric == SIM_EUCLID) ;
   for( ss=0 ; ss<ndraw ; ss++ ){
     THD_simmat *sn=neural ;
     if( ss > 0 ){
       if( tset != NULL ){
         int *off=tset->offset+(size_t)ss*nsub ;
         THD_simmat_from_lag_table(tw->nullmat,nt,tw->lagtab,off) ;
       } else if( THD_rsa_phase_draw(phset,ss,tw->phspec,tw->phwork,tw->phseries,tw->nullmat,
                                 neu_metric,tw->phsc1,tw->phsc2) ){
         memset(tw->nullmat->mat,0,sizeof(float)*(size_t)nsub*nsub) ;
       }
       sn=tw->nullmat ;
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
