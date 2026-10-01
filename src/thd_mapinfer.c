#include "thd_mapinfer.h"

#include <stdlib.h>

typedef struct { float value ; int index ; } THD_fdr_pair ;

static int thd_fdr_pair_cmp( const void *aa, const void *bb )
{
   float a=((const THD_fdr_pair *)aa)->value ;
   float b=((const THD_fdr_pair *)bb)->value ;
   return (a>b)-(a<b) ;
}

void THD_bh_fdr_masked( int n, const float *p,
                        const unsigned char *valid, float *q )
{
   THD_fdr_pair *pair ;
   float qmin=1.0f ;
   int ii,nvalid=0 ;

   if( n <= 0 || q == NULL ) return ;
   if( p == NULL ){
     for( ii=0 ; ii<n ; ii++ ) q[ii]=1.0f ;
     return ;
   }

   for( ii=0 ; ii<n ; ii++ ) if( valid==NULL || valid[ii] ) nvalid++ ;
   if( nvalid == 0 ){
     for( ii=0 ; ii<n ; ii++ ) q[ii]=1.0f ;
     return ;
   }

   pair=(THD_fdr_pair *)malloc(sizeof(THD_fdr_pair)*(size_t)nvalid) ;
   if( pair == NULL ){
     /* The void API cannot report allocation failure.  Returning the most
        conservative valid q-values is preferable to partial/uninitialized
        inference. */
     for( ii=0 ; ii<n ; ii++ ) q[ii]=1.0f ;
     return ;
   }

   for( ii=0,nvalid=0 ; ii<n ; ii++ ){
     if( valid!=NULL && !valid[ii] ) continue ;
     pair[nvalid].value=p[ii] ; pair[nvalid].index=ii ; nvalid++ ;
   }
   qsort(pair,(size_t)nvalid,sizeof(THD_fdr_pair),thd_fdr_pair_cmp) ;

   /* Copying all inputs above makes p==q safe, including in masked mode. */
   if( valid != NULL )
     for( ii=0 ; ii<n ; ii++ ) if( !valid[ii] ) q[ii]=1.0f ;
   for( ii=nvalid-1 ; ii>=0 ; ii-- ){
     float qv=pair[ii].value*(float)nvalid/(float)(ii+1) ;
     if( qv > qmin ) qv=qmin ; else qmin=qv ;
     if( qv > 1.0f ) qv=1.0f ;
     q[pair[ii].index]=qv ;
   }
   free(pair) ;
}

void THD_bh_fdr( int n, const float *p, float *q )
{
   THD_bh_fdr_masked(n,p,NULL,q) ;
}

void THD_max_accum( int n, float *dst, const float *src )
{
   int ii ;
   if( n <= 0 || dst == NULL || src == NULL ) return ;
   for( ii=0 ; ii<n ; ii++ ) if( src[ii] > dst[ii] ) dst[ii]=src[ii] ;
}

void THD_memory_plan_finish( THD_memory_plan *plan )
{
   if( plan == NULL ) return ;
   plan->total = plan->input + plan->geometry + plan->shared + plan->output
               + (double)plan->nthread*plan->per_thread ;
}
