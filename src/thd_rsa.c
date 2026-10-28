#include "thd_rsa.h"

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
