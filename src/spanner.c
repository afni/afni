#include "mrilib.h"

typedef struct {
  int bot , top ;
  float pval ;
} spanfit ;

float evaluate_span( int , int , int , int , float * , float ** ) ;

/*-------------------------------------------------------------------*/

spanfit find_best_span( int ndim , int nvec , int minspan ,
                        float * cvec , float ** bvec       )
{
   spanfit result = {0,0,0.0} ;
   int ii,kk , bot,top , bot_best,top_best ;
   float val , val_best ;

   if( minspan < 3 || ndim < minspan || nvec < 100 ) return result ;
   if( cvec == NULL || bvec == NULL )                return result ;

   val_best = -1.0 ;
   for( bot=0 ; bot < ndim+1-minspan ; bot++ ){
      for( top=bot+minspan-1 ; top < ndim ; top++ ){
         val = evaluate_span( ndim,nvec , bot,top , cvec,bvec ) ;
         if( val > val_best ){
            val_best = val ; bot_best = bot ; top_best = top ;
         }
      }
   }

   evaluate_span( 0,0,0,0,NULL,NULL ) ;

   result.bot = bot_best ; result.top = top_best ; result.pval = val_best ;
   return result ;
}

/*-------------------------------------------------------------------*/

float evaluate_span( int ndim, int nvec,
                            int bot , int top , float * cvec , float ** bvec )
{
   int   kk , ibot=bot,itop=top , nneg ;
   float cbar, *qvec ;
   register int ii ;
   register float sum ;

   static float * bsum=NULL , * cnorm=NULL ;
   static int    nbsum=0    ,  ncnorm=0    ;

   if( nvec > nbsum ){
      if( bsum != NULL ) free(bsum) ;
      bsum  = (float *) malloc(sizeof(float)*nvec) ;
      nbsum = nvec ;
   } else if( nvec <= 0 ){
      if( bsum  != NULL ){ free(bsum) ; bsum  = NULL; nbsum  = 0; }
      if( cnorm != NULL ){ free(cnorm); cnorm = NULL; ncnorm = 0; }
      return 0.0 ;
   }

   if( ndim > ncnorm ){
      if( cnorm != NULL ) free(cnorm) ;
      cnorm  = (float *) malloc(sizeof(float)*ndim) ;
      ncnorm = ndim ;
   }

   /* compute cnorm = cvec-cbar */

   sum = 0.0 ;
   for( ii=ibot ; ii <= itop ; ii++ ) sum += cvec[ii] ;
   cbar = sum/(itop-ibot+1) ; sum = 0.0 ;
   for( ii=ibot ; ii <= itop ; ii++ ){
      cnorm[ii] = cvec[ii] - cbar ; sum += cnorm[ii]*cnorm[ii] ;
   }
   if( sum <= 0.0 ) return 1.0 ;   /* [cvec-cbar]=0 is perfect */

   /* project each bvec onto cnorm */

   for( kk=0 ; kk < nvec ; kk++ ){
      qvec = bvec[kk] ; sum = 0.0 ;
      for( ii=ibot ; ii <= itop ; ii++ ) sum += qvec[ii] * cnorm[ii] ;
      bsum[kk] = sum ;
   }

   /* find number of bsums less than 0 */

   for( nneg=ii=0 ; ii < nvec ; ii++ ) if( bsum[ii] <= 0.0 ) nneg++ ;

   /* return value is fraction of negative bsum values */

   sum = (float)nneg / (float)nvec ;
   return sum ;
}
