#include "mrilib.h"

/*---------------------------------------------------------------
   10 Jul 2001: sort a cluster by its "mag" array
-----------------------------------------------------------------*/

typedef struct { short i , j , k ; } short3 ;

void MCW_sort_cluster( MCW_cluster * cl )
{
   int nn , ii ;
   float *aa ;
   short3 ** ss ;

ENTRY("MCW_sort_cluster") ;

   if( cl == NULL || cl->num_pt < 2 ) EXRETURN ;

   nn = cl->num_pt ;
   aa = (float *  ) malloc(sizeof(float)   *nn) ;
   ss = (short3 **) malloc(sizeof(short3 *)*nn) ;
   for( ii=0 ; ii < nn ; ii++ ){
      aa[ii] = cl->mag[ii] ;
      ss[ii] = (short3 *) malloc(sizeof(short3)) ;
      ss[ii]->i = cl->i[ii] ;
      ss[ii]->j = cl->j[ii] ;
      ss[ii]->k = cl->k[ii] ;
   }

   qsort_floatstuff( nn , aa , (void **) ss ) ;

   for( ii=0 ; ii < nn ; ii++ ){
      cl->mag[ii] = aa[ii] ;
      cl->i[ii] = ss[ii]->i ;
      cl->j[ii] = ss[ii]->j ;
      cl->k[ii] = ss[ii]->k ;
      free(ss[ii]) ;
   }

   free(ss) ; free(aa) ; EXRETURN ;
}
