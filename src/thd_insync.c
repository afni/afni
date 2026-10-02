#include <math.h>
#include <stddef.h>
#include <stdlib.h>

#include "thd_insync.h"

static int insync_float_cmp( const void *aa, const void *bb )
{
   float a=*(const float *)aa, b=*(const float *)bb ;
   return (a>b) - (a<b) ;
}

typedef struct { float value ; int index ; } insync_rank_pair ;

static int insync_rank_cmp( const void *aa, const void *bb )
{
   float a=((const insync_rank_pair *)aa)->value ;
   float b=((const insync_rank_pair *)bb)->value ;
   return (a>b)-(a<b) ;
}

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

static float insync_corr( int n, const float *x, const float *y, int metric,
                          float *rank1, float *rank2 )
{
   if( metric==INSYNC_CORR_PEARSON ) return insync_pearson(n,x,y) ;
   if( metric!=INSYNC_CORR_SPEARMAN || rank1==NULL || rank2==NULL )
     return NAN ;
   if( insync_rank_avg(n,x,rank1) || insync_rank_avg(n,y,rank2) ) return NAN ;
   return insync_pearson(n,rank1,rank2) ;
}

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

float INSYNC_loo_group( int nsub, int ntime, const float *data,
                        const int *group, int target, int summary,
                        float *ref, float *values, int *nvalid )
{
   return INSYNC_loo_group_metric(nsub,ntime,data,group,target,summary,
                                  INSYNC_CORR_PEARSON,ref,values,NULL,NULL,
                                  nvalid) ;
}

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

float INSYNC_loo_indexed( int nsub, int ntime, const float *data,
                          int nmember, const int *member, int summary,
                          float *ref, float *values, int *nvalid )
{
   return INSYNC_loo_indexed_metric(nsub,ntime,data,nmember,member,summary,
                                    INSYNC_CORR_PEARSON,ref,values,NULL,NULL,
                                    nvalid) ;
}

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
