/*
PJM Wants you to know && How to read this file:

Read INSYNC_summary, the pairwise/indexed and LOO routines, then INSYNC_kernel
and its eval/aggregate/export operations. Public contracts live in thd_insync.h.
Inputs are subject-major packed time series, matrices are row-major. The
legacy kernel consumes a common retained timeline. INSYNC_missing below it
implements overlap-specific estimation for pairwise/interp missingness.
INSYNC_interp_linear fills bounded full-run gaps; observed flags then keep
real coverage distinct from augmented correlation samples in eval/aggregate.
Missing and constant-series behavior differs from the legacy
thd_simmatrix helper, so do not substitute kernels without baseline tests.
Pearson r is centered cross-product / sqrt(centered sums of squares).
Spearman is Pearson on average ranks (ties receive their mean rank).
-summary fisher_mean computes tanh(mean(atanh(r))); median sorts finite r's.
-block_summary aggregates each dyad/LOO component before -summary aggregates
components across people. Averaging group maps instead changes the estimand.
INSYNC_missing_between summarizes the rectangular cross-group pair set for
-between_groups, without altering the existing components or inference.
Bootstrap indices refer to original subjects: dyadic self-pairs are omitted.
Legacy LOO resampling excludes a slot. The missing-policy kernel excludes
all copies of the held original subject; tests cover duplicate indices.
Reference: Chen et al. (2016), NeuroImage, doi:10.1016/j.neuroimage.2016.05.023;
Nastase et al. (2019), SCAN, doi:10.1093/scan/nsz037.
*/
/* Dataset-agnostic ISC estimators; the dataset driver is in thd_insync.c. */
#include <math.h>
#include <stddef.h>
#include <stdint.h>
#include <stdlib.h>

#include "thd_insync.h"
#include <string.h>

/* Order finite scalar correlations for median/percentile summaries. */
static int insync_float_cmp( const void *aa, const void *bb )
{
   float a=*(const float *)aa, b=*(const float *)bb ;
   return (a>b) - (a<b) ;
}

typedef struct { float value ; int index ; } insync_rank_pair ;

/* Order value/index pairs by value; equal values receive average ranks later. */
static int insync_rank_cmp( const void *aa, const void *bb )
{
   float a=((const insync_rank_pair *)aa)->value ;
   float b=((const insync_rank_pair *)bb)->value ;
   return (a>b)-(a<b) ;
}

/* -correlation spearman: write one-based average ranks; return nonzero on
   allocation or nonfinite input failure. */
static int insync_rank_avg( int n, const float *x, float *rank )
{
   insync_rank_pair *pair ; int ii,jj ;
   if( n<1 || x==NULL || rank==NULL ) return 1 ;
   pair=(insync_rank_pair *)malloc(sizeof(*pair)*(size_t)n) ;
   if( pair==NULL ) return 1 ;
   for( ii=0 ; ii<n ; ii++ ){
     if( !isfinite(x[ii]) ){ free(pair) ; return 1 ; }
     pair[ii].value=x[ii] ; pair[ii].index=ii ;
   }
   qsort(pair,(size_t)n,sizeof(*pair),insync_rank_cmp) ;
   for( ii=0 ; ii<n ; ii=jj ){
     float rr ;
     for( jj=ii+1 ; jj<n && pair[jj].value==pair[ii].value ; jj++ ) ;
     rr=0.5f*((float)(ii+1)+(float)jj) ;
     for( ; ii<jj ; ii++ ) rank[pair[ii].index]=rr ;
   }
   free(pair) ; return 0 ;
}

/*! LOO needs one subject-to-reference correlation rather than a full
    subject matrix.  Keep its stricter missing/constant-series behavior local;
    the ordinary pairwise path is provided by thd_simmatrix. */
static float insync_pearson( int n, const float *x, const float *y )
{
   int ii ; double sx=0.0,sy=0.0,mx,my,sxx=0.0,syy=0.0,sxy=0.0,r ;
   if( n<2 || x==NULL || y==NULL ) return NAN ;
   for( ii=0 ; ii<n ; ii++ ){
     if( !isfinite(x[ii]) || !isfinite(y[ii]) ) return NAN ;
     sx += x[ii] ; sy += y[ii] ;
   }
   mx=sx/(double)n ; my=sy/(double)n ;
   for( ii=0 ; ii<n ; ii++ ){
     double dx=(double)x[ii]-mx,dy=(double)y[ii]-my ;
     sxx+=dx*dx ; syy+=dy*dy ; sxy+=dx*dy ;
   }
   if( sxx<=0.0 || syy<=0.0 ) return NAN ;
   r=sxy/sqrt(sxx*syy) ;
   if( r>1.0 ) r=1.0 ; else if( r < -1.0 ) r=-1.0 ;
   return (float)r ;
}

/* Select Pearson or average-rank Spearman for one subject/reference pair;
   rank buffers have n entries. */
static float insync_corr( int n, const float *x, const float *y, int metric,
                          float *rank1, float *rank2 )
{
   if( metric==INSYNC_CORR_PEARSON ) return insync_pearson(n,x,y) ;
   if( metric!=INSYNC_CORR_SPEARMAN || rank1==NULL || rank2==NULL )
     return NAN ;
   if( insync_rank_avg(n,x,rank1) || insync_rank_avg(n,y,rank2) ) return NAN ;
   return insync_pearson(n,rank1,rank2) ;
}

/* -summary/-block_summary: summarize finite correlations, report their
   count, and return NaN when none remain. Scratch may alias value. */
float INSYNC_summary( int n, const float *value, int summary,
                      float *scratch, int *nvalid )
{
   int ii,nv=0 ;
   if( nvalid!=NULL ) *nvalid=0 ;
   if( n<1 || value==NULL || scratch==NULL ) return NAN ;
   for( ii=0 ; ii<n ; ii++ ) if( isfinite(value[ii]) ) scratch[nv++]=value[ii] ;
   if( nvalid!=NULL ) *nvalid=nv ;
   if( nv==0 ) return NAN ;

   if( summary==INSYNC_SUMMARY_MEDIAN ){
     qsort(scratch,(size_t)nv,sizeof(float),insync_float_cmp) ;
     if( nv&1 ) return scratch[nv/2] ;
     return 0.5f*(scratch[nv/2-1]+scratch[nv/2]) ;
   }

   if( summary==INSYNC_SUMMARY_FISHER_MEAN ){
     double sum=0.0 ;
     for( ii=0 ; ii<nv ; ii++ ){
       double r=scratch[ii] ;
       if( r>=1.0 ) r=0.999999 ; else if( r<=-1.0 ) r=-0.999999 ;
       sum += atanh(r) ;
     }
     return (float)tanh(sum/(double)nv) ;
   }
   return NAN ;
}

/* -isc_method pairwise: gather the target group upper triangle; each
   unordered pair contributes once. */
float INSYNC_pairwise_group( int nsub, const float *corr,
                             const int *group, int target,
                             int summary, float *scratch, int *nvalid )
{
   int ii,jj,nn=0 ;
   if( nvalid!=NULL ) *nvalid=0 ;
   if( nsub<2 || corr==NULL || group==NULL || scratch==NULL ) return NAN ;
   for( ii=0 ; ii<nsub ; ii++ ) if( group[ii]==target )
     for( jj=ii+1 ; jj<nsub ; jj++ ) if( group[jj]==target )
       scratch[nn++]=corr[ii*nsub+jj] ;
   return INSYNC_summary(nn,scratch,summary,scratch,nvalid) ;
}

/* -nboot: summarize dyads among sampled subject slots, omitting pairs with
   identical original subject IDs. */
float INSYNC_pairwise_indexed( int nsub, const float *corr,
                               int nmember, const int *member,
                               int summary, float *scratch, int *nvalid )
{
   int ii,jj,nn=0 ;
   if( nvalid!=NULL ) *nvalid=0 ;
   if( nsub<2 || corr==NULL || nmember<2 || member==NULL || scratch==NULL )
     return NAN ;
   for( ii=0 ; ii<nmember ; ii++ ){
     if( member[ii]<0 || member[ii]>=nsub ) return NAN ;
     for( jj=ii+1 ; jj<nmember ; jj++ ){
       if( member[jj]<0 || member[jj]>=nsub ) return NAN ;
       if( member[ii]!=member[jj] )
         scratch[nn++]=corr[(size_t)member[ii]*nsub+member[jj]] ;
     }
   }
   return INSYNC_summary(nn,scratch,summary,scratch,nvalid) ;
}

/* Pearson wrapper for -isc_method loo; see the metric-selecting routine for
   workspace and missingness rules. */
float INSYNC_loo_group( int nsub, int ntime, const float *data,
                        const int *group, int target, int summary,
                        float *ref, float *values, int *nvalid )
{
   return INSYNC_loo_group_metric(nsub,ntime,data,group,target,summary,
                                  INSYNC_CORR_PEARSON,ref,values,NULL,NULL,
                                  nvalid) ;
}

/* -isc_method loo: average other group members at each TR, correlate each
   held-out series with its reference, then summarize. Any nonfinite
   contributor invalidates that reference TR; this is not pairwise deletion. */
float INSYNC_loo_group_metric( int nsub, int ntime, const float *data,
                               const int *group, int target, int summary,
                               int metric, float *ref, float *values,
                               float *rank1, float *rank2, int *nvalid )
{
   int ii,jj,tt,nmember=0,nv=0 ;
   if( nvalid!=NULL ) *nvalid=0 ;
   if( nsub<2 || ntime<2 || data==NULL || group==NULL ||
       ref==NULL || values==NULL ) return NAN ;
   for( ii=0 ; ii<nsub ; ii++ ) if( group[ii]==target ) nmember++ ;
   if( nmember<2 ) return NAN ;

   for( ii=0 ; ii<nsub ; ii++ ) if( group[ii]==target ){
     for( tt=0 ; tt<ntime ; tt++ ){
       double sum=0.0 ; int good=1 ;
       for( jj=0 ; jj<nsub ; jj++ ) if( jj!=ii && group[jj]==target ){
         float x=data[(size_t)jj*ntime+tt] ;
         if( !isfinite(x) ){ good=0 ; break ; }
         sum+=x ;
       }
       ref[tt] = good ? (float)(sum/(double)(nmember-1)) : NAN ;
     }
     values[nv++]=insync_corr(ntime,data+(size_t)ii*ntime,ref,metric,
                              rank1,rank2) ;
   }
   return INSYNC_summary(nv,values,summary,values,nvalid) ;
}

/* Pearson wrapper for indexed LOO subject resampling; repeated subject slots
   retain their multiplicity. */
float INSYNC_loo_indexed( int nsub, int ntime, const float *data,
                          int nmember, const int *member, int summary,
                          float *ref, float *values, int *nvalid )
{
   return INSYNC_loo_indexed_metric(nsub,ntime,data,nmember,member,summary,
                                    INSYNC_CORR_PEARSON,ref,values,NULL,NULL,
                                    nvalid) ;
}

/* -nboot with LOO: rebuild references from sampled slots. Exclude the held-
   out slot only; another copy of the same original subject can contribute.
   Return the finite-value summary. */
float INSYNC_loo_indexed_metric( int nsub, int ntime, const float *data,
                                 int nmember, const int *member, int summary,
                                 int metric, float *ref, float *values,
                                 float *rank1, float *rank2, int *nvalid )
{
   int ii,jj,tt,nv=0 ;
   if( nvalid!=NULL ) *nvalid=0 ;
   if( nsub<2 || ntime<2 || data==NULL || nmember<2 || member==NULL ||
       ref==NULL || values==NULL ) return NAN ;
   for( ii=0 ; ii<nmember ; ii++ ){
     int si=member[ii] ;
     if( si<0 || si>=nsub ) return NAN ;
     for( tt=0 ; tt<ntime ; tt++ ){
       double sum=0.0 ; int good=1 ;
       for( jj=0 ; jj<nmember ; jj++ ) if( jj!=ii ){
         int sj=member[jj] ; float x ;
         if( sj<0 || sj>=nsub ) return NAN ;
         x=data[(size_t)sj*ntime+tt] ;
         if( !isfinite(x) ){ good=0 ; break ; }
         sum+=x ;
       }
       ref[tt]=good ? (float)(sum/(double)(nmember-1)) : NAN ;
     }
     values[nv++]=insync_corr(ntime,data+(size_t)si*ntime,ref,metric,
                              rank1,rank2) ;
   }
   return INSYNC_summary(nv,values,summary,values,nvalid) ;
}

/* -nboot: sort x in place and linearly interpolate the requested empirical
   quantile for percentile confidence limits. */
float INSYNC_percentile( float *x, int n, float probability )
{
   double pos,frac ; int lo,hi ;
   if( x==NULL || n<1 ) return NAN ;
   qsort(x,(size_t)n,sizeof(float),insync_float_cmp) ;
   if( probability<=0.0f ) return x[0] ;
   if( probability>=1.0f ) return x[n-1] ;
   pos=(double)(n-1)*(double)probability ;
   lo=(int)floor(pos) ; hi=(int)ceil(pos) ; frac=pos-(double)lo ;
   return (float)((1.0-frac)*(double)x[lo]+frac*(double)x[hi]) ;
}

/* Reusable selection workspace. Unlike the legacy similarity helper, constant
   series produce invalid correlations here, never artificial zero edges. */
struct INSYNC_kernel {
   int nsub, capacity;
   float *matrix, *ranked, *ref, *rankref, *values, *scratch, *loo;
   insync_rank_pair *order;
};

/* Allocate an owned reusable selection kernel for nsub subjects and at most
   capacity TRs; return NULL for invalid sizes or allocation failure. */
INSYNC_kernel *INSYNC_kernel_new(int nsub,int capacity)
{
   INSYNC_kernel *w;
   size_t ns=(size_t)nsub,nt=(size_t)capacity;
   if(nsub<3 || capacity<3 || ns>SIZE_MAX/ns/sizeof(float) || ns>SIZE_MAX/nt/sizeof(float)) return NULL;
   w=calloc(1,sizeof(*w)); if(!w) return NULL;
   w->nsub=nsub; w->capacity=capacity;
   w->matrix=malloc(ns*ns*sizeof(float)); w->ranked=malloc(ns*nt*sizeof(float));
   w->ref=malloc(nt*sizeof(float)); w->rankref=malloc(nt*sizeof(float));
   w->loo=malloc(ns*sizeof(float)); w->values=malloc(ns*sizeof(float)); w->scratch=malloc(ns*ns*sizeof(float));
   w->order=malloc(nt*sizeof(*w->order));
   if(!w->matrix || !w->ranked || !w->ref || !w->rankref || !w->values || !w->scratch || !w->order || !w->loo){ INSYNC_kernel_free(w); return NULL; }
   return w;
}

/* Release all owned numerical workspace; NULL is accepted. */
void INSYNC_kernel_free(INSYNC_kernel *w)
{
   if(!w) return;
   free(w->matrix); free(w->ranked); free(w->ref); free(w->rankref);
   free(w->values); free(w->scratch); free(w->order); free(w->loo); free(w);
}

/* Allocation-free Spearman ranking in private worker scratch; one nonfinite
   value invalidates the entire ranked row. */
static void insync_kernel_rank(INSYNC_kernel *w,int n,const float *x,float *out)
{
   int i,j,k;
   for(i=0;i<n;i++){
      if(!isfinite(x[i])){ for(j=0;j<n;j++) out[j]=NAN; return; }
      w->order[i]=(insync_rank_pair){x[i],i};
   }
   qsort(w->order,n,sizeof(*w->order),insync_rank_cmp);
   for(i=0;i<n;i=j){
      for(j=i+1;j<n && w->order[j].value==w->order[i].value;j++);
      for(k=i;k<j;k++) out[w->order[k].index]=.5f*((float)(i+1)+(float)j);
   }
}

/* Packed input is subject-major, with exactly nt retained samples per row.
   Matrix remains available until the next evaluation for pair-map export.
   Complete is a pair-graph flag, independent of the descriptive group count. */
void INSYNC_kernel_eval(INSYNC_kernel *w,int nt,const float *data,int metric,
                       int method,int summary,int ngroup,const int *gcount,
                       int **gmember,float *stat,int *nvalid,int *complete)
{
   int i,j,g,t,ns=w->nsub; const float *features=data;
   for(g=0;g<ngroup;g++){ stat[g]=NAN; nvalid[g]=complete[g]=0; }
   for(i=0;i<ns;i++) w->loo[i]=NAN;
   for(i=0;i<ns;i++) for(j=0;j<ns;j++) w->matrix[(size_t)i*ns+j]=NAN;
   if(nt<3 || nt>w->capacity) return;
   if(metric==INSYNC_CORR_SPEARMAN){
      for(i=0;i<ns;i++) insync_kernel_rank(w,nt,data+(size_t)i*nt,w->ranked+(size_t)i*nt);
      features=w->ranked;
   }
   for(i=0;i<ns;i++) for(j=i;j<ns;j++)
      w->matrix[(size_t)i*ns+j]=w->matrix[(size_t)j*ns+i]=insync_pearson(nt,features+(size_t)i*nt,features+(size_t)j*nt);
   for(g=0;g<ngroup;g++){
      int nm=gcount[g],good=1;
      for(i=0;i<nm;i++) for(j=i+1;j<nm;j++)
         if(!isfinite(w->matrix[(size_t)gmember[g][i]*ns+gmember[g][j]])) good=0;
      complete[g]=good;
      if(method==INSYNC_METHOD_PAIRWISE)
         stat[g]=INSYNC_pairwise_indexed(ns,w->matrix,nm,gmember[g],summary,w->scratch,nvalid+g);
      else {
         for(i=0;i<nm;i++){
            int subject=gmember[g][i];
            for(t=0;t<nt;t++){
               double sum=0;
               for(j=0;j<nm;j++) if(j!=i) sum+=data[(size_t)gmember[g][j]*nt+t];
               w->ref[t]=(float)(sum/(nm-1));
            }
            if(metric==INSYNC_CORR_SPEARMAN){
               insync_kernel_rank(w,nt,w->ref,w->rankref);
               w->values[i]=insync_pearson(nt,w->ranked+(size_t)subject*nt,w->rankref);
            } else w->values[i]=insync_pearson(nt,data+(size_t)subject*nt,w->ref);
            w->loo[subject]=w->values[i];
         }
         stat[g]=INSYNC_summary(nm,w->values,summary,w->scratch,nvalid+g);
      }
   }
}

const float *INSYNC_kernel_matrix(const INSYNC_kernel *w){ return w?w->matrix:NULL; }

const float *INSYNC_kernel_loo(const INSYNC_kernel *w){ return w?w->loo:NULL; }

/* Each retained block contributes once to every edge/LOO subject. The caller
   admits a common block set after coverage and complete-component checks. */
void INSYNC_kernel_aggregate(INSYNC_kernel *w,int nblock,const float *blocks,
   const unsigned char *keep,int method,int block_summary,int group_summary,
   int ngroup,const int *gcount,int **gmember,float *stat,int *nvalid,int *complete)
{
   int i,j,b,g,n,ns=w->nsub; size_t stride=method==INSYNC_METHOD_PAIRWISE?(size_t)ns*ns:(size_t)ns;
   for(g=0;g<ngroup;g++){ stat[g]=NAN; nvalid[g]=complete[g]=0; }
   for(i=0;i<ns;i++){ w->loo[i]=NAN; for(j=0;j<ns;j++) w->matrix[(size_t)i*ns+j]=NAN; }
   if(nblock<1 || nblock>w->capacity) return;
   /* Do not silently omit a NaN for only one edge/subject. The admission
      policy belongs to the caller, but identical support is enforced here. */
   for(b=0;b<nblock;b++) if(keep[b]) for(g=0;g<ngroup;g++){
      for(i=0;i<gcount[g];i++){
         if(method==INSYNC_METHOD_LOO){ if(!isfinite(blocks[(size_t)b*stride+gmember[g][i]])) return; }
         else for(j=i+1;j<gcount[g];j++) if(!isfinite(blocks[(size_t)b*stride+(size_t)gmember[g][i]*ns+gmember[g][j]])) return;
      }
   }
   if(method==INSYNC_METHOD_PAIRWISE){
      for(i=0;i<ns;i++) for(j=i+1;j<ns;j++){
         for(n=0,b=0;b<nblock;b++) if(keep[b]) w->rankref[n++]=blocks[(size_t)b*stride+(size_t)i*ns+j];
         w->matrix[(size_t)i*ns+j]=w->matrix[(size_t)j*ns+i]=n==1?w->rankref[0]:INSYNC_summary(n,w->rankref,block_summary,w->ref,NULL);
      }
   } else for(i=0;i<ns;i++){
      for(n=0,b=0;b<nblock;b++) if(keep[b]) w->rankref[n++]=blocks[(size_t)b*stride+i];
      w->loo[i]=n==1?w->rankref[0]:INSYNC_summary(n,w->rankref,block_summary,w->ref,NULL);
   }
   for(g=0;g<ngroup;g++){
      if(method==INSYNC_METHOD_PAIRWISE){
         stat[g]=INSYNC_pairwise_indexed(ns,w->matrix,gcount[g],gmember[g],group_summary,w->scratch,nvalid+g);
         complete[g]=(size_t)nvalid[g]==(size_t)gcount[g]*(gcount[g]-1)/2;
      } else {
         for(i=0;i<gcount[g];i++) w->values[i]=w->loo[gmember[g][i]];
         stat[g]=INSYNC_summary(gcount[g],w->values,group_summary,w->scratch,nvalid+g);
         /* Admission requires finite LOO values for all subjects. This also
            implies nonconstant subject series and a complete pair graph. */
         complete[g]=nvalid[g]==gcount[g];
      }
   }
}

/* Export boundary: transform the final pair aggregate, never average z(group
   summaries) or hand 3dISC the mean of untransformed block/group maps. */
void INSYNC_kernel_fisher_matrix(const INSYNC_kernel *w,float *z)
{
   size_t i,n=(size_t)w->nsub*w->nsub;
   for(i=0;i<n;i++){
      float r=w->matrix[i];
      if(!isfinite(r)) z[i]=NAN;
      else { if(r>=1) r=.999999f; else if(r<=-1) r=-.999999f; z[i]=atanhf(r); }
   }
}

/* Missing-data workers keep packed overlap/rank scratch separate from outputs.
   Legacy common-timeline kernels above are unchanged. This separation makes
   support-dependent ranking and centering explicit and prevents stale counts. */
struct INSYNC_missing {
   int nsub, capacity, summary_weight;
   size_t ncomponent;
   float *x,*y,*rx,*ry,*scratch;
   int *piece,*subjects,*weight;
   insync_rank_pair *order;
   INSYNC_component *component;
};

/* Allocate only for the opt-in missing policy. Sizes are checked before all
   products; a single free routine handles partially initialized workers. */
INSYNC_missing *INSYNC_missing_new(int nsub,int capacity)
{
   INSYNC_missing *w; size_t ns=(size_t)nsub,nt=(size_t)capacity,nc;
   if(nsub<3 || capacity<3 || ns>SIZE_MAX/(ns+1)/sizeof(INSYNC_component) ||
      nt>SIZE_MAX/sizeof(insync_rank_pair)) return NULL;
   nc=ns*(ns+1); w=calloc(1,sizeof(*w)); if(!w) return NULL;
   w->nsub=nsub; w->capacity=capacity; w->ncomponent=nc;
   w->x=malloc(nt*sizeof(float)); w->y=malloc(nt*sizeof(float));
   w->rx=malloc(nt*sizeof(float)); w->ry=malloc(nt*sizeof(float));
   w->scratch=malloc((nc>nt?nc:nt)*sizeof(float));
   w->piece=malloc(nt*sizeof(int)); w->subjects=calloc(ns,sizeof(int));
   w->weight=malloc((nc>nt?nc:nt)*sizeof(int));
   w->order=malloc((nc>nt?nc:nt)*sizeof(*w->order)); w->component=calloc(nc,sizeof(*w->component));
   if(!w->x || !w->y || !w->rx || !w->ry || !w->scratch || !w->piece ||
      !w->subjects || !w->weight || !w->order || !w->component){ INSYNC_missing_free(w); return NULL; }
   return w;
}

/* Release worker-owned arrays, including partially allocated workers. */
void INSYNC_missing_free(INSYNC_missing *w)
{
   if(!w) return;
   free(w->x); free(w->y); free(w->rx); free(w->ry); free(w->scratch);
   free(w->piece); free(w->subjects); free(w->weight); free(w->order); free(w->component); free(w);
}

/* Rank only the packed finite overlap; ranking each original series first
   would change Spearman r whenever the two subjects have different masks. */
static void insync_missing_rank(INSYNC_missing *w,int n,const float *x,float *rank)
{
   int i,j,k;
   for(i=0;i<n;i++) w->order[i]=(insync_rank_pair){x[i],i};
   qsort(w->order,n,sizeof(*w->order),insync_rank_cmp);
   for(i=0;i<n;i=j){
      for(j=i+1;j<n && w->order[j].value==w->order[i].value;j++);
      for(k=i;k<j;k++) rank[w->order[k].index]=.5f*((float)(i+1)+(float)j);
   }
}

/* Coverage weighting changes the estimand, not the effective degrees of freedom.
   Weights are positive integer UsedTR counts. The weighted median is the first
   value above half the total weight; exactly half averages adjacent values.
   Fisher inputs are clamped to +/-0.999999 before atanh. Scratch is private. */
static float insync_missing_weighted(INSYNC_missing *w,int n,const float *v,
   const int *weight,int summary,int *nvalid)
{
   int i,k=0; double total=0,sum=0,cumulative=0;
   for(i=0;i<n;i++) if(isfinite(v[i]) && weight[i]>0){
      w->order[k++]=(insync_rank_pair){v[i],weight[i]}; total+=weight[i];
      if(summary==INSYNC_SUMMARY_FISHER_MEAN)
         sum+=weight[i]*atanh(fmax(-.999999,fmin(.999999,v[i])));
   }
   if(nvalid) *nvalid=k;
   if(!k) return NAN;
   if(summary==INSYNC_SUMMARY_FISHER_MEAN) return (float)tanh(sum/total);
   qsort(w->order,k,sizeof(*w->order),insync_rank_cmp);
   for(i=0;i<k;i++){
      cumulative+=w->order[i].index;
      if(2*cumulative>total) return w->order[i].value;
      if(2*cumulative==total && i+1<k) return .5f*(w->order[i].value+w->order[i+1].value);
   }
   return w->order[k-1].value;
}

/* Compute r after REAL overlap gates. n includes fills, real excludes them.
   For pooled, mean-centered segments, center
   BOTH signals on the SAME retained samples of each segment. Single-sample
   pieces contribute zeros; the final variance check rejects constant inputs.
   LOO uses the held-out/reference overlap, after valid-other averaging. */
static float insync_missing_corr(INSYNC_missing *w,int n,int real,int nominal,const INSYNC_options *o)
{
   int i,j,k;
   if(real<o->min_valid || nominal<1 || (double)real/nominal<o->min_valid_frac) return NAN;
   if(o->segment_center) for(i=0;i<n;i=j){
      double mx=0,my=0;
      for(j=i;j<n && w->piece[j]==w->piece[i];j++){ mx+=w->x[j]; my+=w->y[j]; }
      mx/=j-i; my/=j-i;
      for(k=i;k<j;k++){ w->x[k]-=(float)mx; w->y[k]-=(float)my; }
   }
   if(o->corr_metric==INSYNC_CORR_SPEARMAN){
      insync_missing_rank(w,n,w->x,w->rx); insync_missing_rank(w,n,w->y,w->ry);
      return insync_pearson(n,w->rx,w->ry);
   }
   return insync_pearson(n,w->x,w->y);
}

/* Evaluate source-local masks encoded as NaN, retaining each pair's actual
   real overlap independently of eligibility; samples includes fills. LOO
   counts a real TR only if every contributing value is observed. References
   exclude the held original subject and average only finite other subjects at each TR. */
void INSYNC_missing_eval_observed(INSYNC_missing *w,int nt,const float *data,
   const unsigned char *observed,const int *piece,
   int nominal,const INSYNC_options *o,int ngroup,const int *gcount,int **gmember)
{
   int i,j,t,g,k,ns=w->nsub; size_t z;
   w->summary_weight=o->summary_weight;
   for(z=0;z<w->ncomponent;z++) w->component[z]=(INSYNC_component){.r=NAN};
   for(i=0;i<ns;i++) w->subjects[i]=0;
   if(nt<1 || nt>w->capacity) return;
   for(i=0;i<ns;i++) for(t=0;t<nt;t++) w->subjects[i]+=isfinite(data[(size_t)i*nt+t]) && (!observed || observed[(size_t)i*nt+t]);
   for(i=0;i<ns;i++) for(j=i+1;j<ns;j++){
      INSYNC_component result={.r=NAN}; int n=0,real=0;
      for(t=0;t<nt;t++){
         float a=data[(size_t)i*nt+t],b=data[(size_t)j*nt+t];
         if(!isfinite(a) || !isfinite(b)) continue;
         real+=!observed || (observed[(size_t)i*nt+t] && observed[(size_t)j*nt+t]);
         w->x[n]=a; w->y[n]=b; w->piece[n]=piece?piece[t]:0; n++;
      }
      result.overlap=real; result.samples=n; result.r=insync_missing_corr(w,n,real,nominal,o);
      if(isfinite(result.r)){ result.used=real; result.used_samples=n; result.eligible=result.blocks=1; }
      w->component[(size_t)i*ns+j]=w->component[(size_t)j*ns+i]=result;
   }
   if(o->method==INSYNC_METHOD_LOO) for(g=0;g<ngroup;g++) for(k=0;k<gcount[g];k++){
      INSYNC_component *result=w->component+(size_t)ns*ns+gmember[g][k]; int n=0,real=0;
      i=gmember[g][k];
      for(t=0;t<nt;t++){
         double sum=0; int count=0,distinct=0,allreal=!observed || observed[(size_t)i*nt+t]; float a=data[(size_t)i*nt+t];
         if(!isfinite(a)) continue;
         for(j=0;j<gcount[g];j++) if(gmember[g][j]!=i){
            float b=data[(size_t)gmember[g][j]*nt+t];
            if(isfinite(b)){
               int previous; sum+=b; count++;
               if(observed && !observed[(size_t)gmember[g][j]*nt+t]) allreal=0;
               for(previous=0;previous<j;previous++) if(gmember[g][previous]==gmember[g][j]) break;
               if(previous==j) distinct++;
            }
         }
         if(distinct<o->loo_min_others) continue;
         real+=allreal;
         w->x[n]=a; w->y[n]=(float)(sum/count); w->piece[n]=piece?piece[t]:0; n++;
         if(!result->min_others || distinct<result->min_others) result->min_others=distinct;
      }
      result->overlap=real; result->samples=n; result->r=insync_missing_corr(w,n,real,nominal,o);
      if(isfinite(result->r)){ result->used=real; result->used_samples=n; result->eligible=result->blocks=1; }
   }
}

/* Determine common block admission only for explicit drop_common. The default
   require_all and new drop_component policies instead gate each component,
   so an unusable subject does not remove unrelated dyads from every block. */
static int insync_missing_block_complete(const INSYNC_component *b,int ns,
   int method,int ngroup,const int *gcount,int **gmember)
{
   int g,i,j;
   for(g=0;g<ngroup;g++) for(i=0;i<gcount[g];i++){
      int a=gmember[g][i];
      if(method==INSYNC_METHOD_LOO){ if(!isfinite(b[(size_t)ns*ns+a].r)) return 0; }
      else for(j=i+1;j<gcount[g];j++) if(a!=gmember[g][j] && !isfinite(b[(size_t)a*ns+gmember[g][j]].r)) return 0;
   }
   return 1;
}

/* Aggregate component correlations, not group maps. overlap sums raw overlap
   across all nominal nonoverlapping blocks; used sums only admitted samples.
   UsedTR weights are optional, and membership may be resampled. Common-mode kernels keep
   their existing all-component admission contract. */
void INSYNC_missing_aggregate(INSYNC_missing *w,int nblock,const INSYNC_component *block,
   const INSYNC_options *o,int ngroup,const int *gcount,int **gmember)
{
   size_t k; int b,ns=w->nsub;
   w->summary_weight=o->summary_weight;
   if(nblock<1 || nblock>w->capacity){
      for(k=0;k<w->ncomponent;k++) w->component[k]=(INSYNC_component){.r=NAN};
      return;
   }
   for(b=0;b<nblock;b++) w->piece[b]=o->block_drop_common!=1 ||
      insync_missing_block_complete(block+(size_t)b*w->ncomponent,ns,o->method,ngroup,gcount,gmember);
   for(k=0;k<w->ncomponent;k++){
      INSYNC_component r={.r=NAN}; int n=0,used=0,used_samples=0;
      for(b=0;b<nblock;b++){
         const INSYNC_component *x=block+(size_t)b*w->ncomponent+k;
         r.overlap+=x->overlap; r.samples+=x->samples; r.eligible+=isfinite(x->r)!=0;
         if(x->min_others && (!r.min_others || x->min_others<r.min_others)) r.min_others=x->min_others;
         if(w->piece[b] && isfinite(x->r)){ w->weight[n]=x->used; w->x[n++]=x->r; used+=x->used; used_samples+=x->used_samples; }
      }
      if(n>=o->min_blocks && (double)n/nblock>=o->min_block_frac &&
         (o->block_drop_common || n==nblock)){
         r.r=n==1?w->x[0]:o->block_weight?insync_missing_weighted(w,n,w->x,w->weight,o->block_summary,NULL):INSYNC_summary(n,w->x,o->block_summary,w->scratch,NULL);
         if(isfinite(r.r)){ r.used=used; r.used_samples=used_samples; r.blocks=n; }
      }
      w->component[k]=r;
   }
}

/* Borrowed component results; use row-major pairs then one LOO entry/subject. */
const INSYNC_component *INSYNC_missing_components(const INSYNC_missing *w){ return w->component; }
/* Borrowed per-subject finite counts from the last eval, before pair gates. */
const int *INSYNC_missing_subjects(const INSYNC_missing *w){ return w->subjects; }

/* Summarize finite dyads or LOO subjects without requiring a complete group.
   PairComplete is separate, and remains a pair-graph diagnostic in LOO mode. */
void INSYNC_missing_summary(INSYNC_missing *w,int method,int summary,int ngroup,
   const int *gcount,int **gmember,float *stat,int *nvalid,int *complete)
{
   int g,i,j,n,ns=w->nsub;
   for(g=0;g<ngroup;g++){
      n=0; complete[g]=1;
      for(i=0;i<gcount[g];i++){
         int a=gmember[g][i];
         if(method==INSYNC_METHOD_LOO){ w->weight[n]=w->component[(size_t)ns*ns+a].used; w->scratch[n++]=w->component[(size_t)ns*ns+a].r; }
         for(j=i+1;j<gcount[g];j++){
            float r=w->component[(size_t)a*ns+gmember[g][j]].r;
            if(a!=gmember[g][j] && !isfinite(r)) complete[g]=0;
            if(method==INSYNC_METHOD_PAIRWISE && a!=gmember[g][j]){ w->weight[n]=w->component[(size_t)a*ns+gmember[g][j]].used; w->scratch[n++]=r; }
         }
      }
      stat[g]=w->summary_weight?insync_missing_weighted(w,n,w->scratch,w->weight,summary,nvalid+g):INSYNC_summary(n,w->scratch,summary,w->scratch,nvalid+g);
   }
}

/* Compatibility entry point for purely observed, source-local deletion. */
void INSYNC_missing_eval(INSYNC_missing *w,int nt,const float *data,const int *piece,
   int nominal,const INSYNC_options *o,int ngroup,const int *gcount,int **gmember)
{
   INSYNC_missing_eval_observed(w,nt,data,NULL,piece,nominal,o,ngroup,gcount,gmember);
}

/* Linear interpolation uses only clean endpoints in the same original run.
   Work on the full timeline before selecting windows, so window boundaries do
   not change the filled series. Double convex weights avoid float overflow. */
int INSYNC_interp_linear(int nt,float *data,const unsigned char *keep,
   int nrun,const int *runstart,int maxgap,unsigned char *filled)
{
   int r,t,count=0;
   if(nt<1 || !data || !filled || nrun<1 || !runstart || runstart[0]!=0 || maxgap<1) return -1;
   for(r=0;r<nrun;r++) if(runstart[r]<0 || runstart[r]>=nt || (r && runstart[r]<=runstart[r-1])) return -1;
   memset(filled,0,nt);
   for(t=0;t<nt;t++) if((keep && !keep[t]) || !isfinite(data[t])) data[t]=NAN;
   for(r=0;r<nrun;r++){
      int begin=runstart[r],end=r+1<nrun?runstart[r+1]:nt;
      for(t=begin;t<end;){
         int start,j;
         if(isfinite(data[t])){ t++; continue; }
         start=t;
         while(t<end && !isfinite(data[t])) t++;
         if(start==begin || t==end || t-start>maxgap) continue;
         for(j=start;j<t;j++){
            double fraction=(double)(j-start+1)/(t-start+1);
            data[j]=(float)((1-fraction)*data[start-1]+fraction*data[t]);
            filled[j]=1; count++;
         }
      }
   }
   return count;
}

/* Cross-group ISC is a summary of subject dyads, not a correlation between
   group means or a difference of within-group summaries. Reuse the declared
   summary/UsedTR weights and retain coverage of failed dyads in the minima. */
void INSYNC_missing_between(INSYNC_missing *w,int na,const int *a,int nb,
   const int *b,int summary,float out[6])
{
   int i,j,n=0,valid=0,complete=1,minraw=0,minused=0;
   for(i=0;i<na;i++) for(j=0;j<nb;j++){
      const INSYNC_component *c=w->component+(size_t)a[i]*w->nsub+b[j];
      if(!n || c->overlap<minraw) minraw=c->overlap;
      if(!n || c->used<minused) minused=c->used;
      if(!isfinite(c->r)) complete=0;
      w->scratch[n]=c->r; w->weight[n++]=c->used;
   }
   out[0]=w->summary_weight?
      insync_missing_weighted(w,n,w->scratch,w->weight,summary,&valid):
      INSYNC_summary(n,w->scratch,summary,w->scratch,&valid);
   out[1]=valid; out[2]=isfinite(out[0])!=0; out[3]=n>0 && complete;
   out[4]=minraw; out[5]=minused;
}
