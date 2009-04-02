#include "mrilib.h"

/*---------------------------------------------------------------
   10 Jul 2001: sort a cluster by its "mag" array
-----------------------------------------------------------------*/

typedef struct { short i, j, k; } short3 ;

typedef struct { float v; short i, j, k; } fl_sh3 ;

void MCW_sort_cluster( MCW_cluster *cl )
{
   int nn , ii ;
   float   *aa ;
   short3 **ss ;

ENTRY("MCW_sort_cluster") ;

   if( cl == NULL || cl->num_pt < 2 ) EXRETURN ;

   nn = cl->num_pt ;
   aa = (float *  ) malloc(sizeof(float)   *nn) ;
   ss = (short3 **) malloc(sizeof(short3 *)*nn) ;
   for( ii=0 ; ii < nn ; ii++ ){
      aa[ii]    = cl->mag[ii] ;
      ss[ii]    = (short3 *) malloc(sizeof(short3)) ;
      ss[ii]->i = cl->i[ii] ;
      ss[ii]->j = cl->j[ii] ;
      ss[ii]->k = cl->k[ii] ;
   }

   qsort_floatstuff( nn , aa , (void **)ss ) ;

   for( ii=0 ; ii < nn ; ii++ ){
      cl->mag[ii] = aa[ii] ;
      cl->i[ii]   = ss[ii]->i ;
      cl->j[ii]   = ss[ii]->j ;
      cl->k[ii]   = ss[ii]->k ;
      free(ss[ii]) ;
   }

   free(ss) ; free(aa) ; EXRETURN ;
}

/*----------------------------------------------------------------*/
/*! Sort a cluster by distance from (0,0,0) [e.g., a mask].
------------------------------------------------------------------*/

void MCW_radsort_cluster( MCW_cluster *cl , float dx, float dy, float dz )
{
   int nn , ii ;
   float   *aa , x,y,z ;
   fl_sh3 **ss ;

ENTRY("MCW_radsort_cluster") ;

   if( cl == NULL || cl->num_pt < 2 ) EXRETURN ;

   if( dx == 0.0f ) dx = 1.0f ;
   if( dy == 0.0f ) dy = 1.0f ;
   if( dz == 0.0f ) dz = 1.0f ;

   nn = cl->num_pt ;
   aa = (float *  ) malloc(sizeof(float)   *nn) ;
   ss = (fl_sh3 **) malloc(sizeof(fl_sh3 *)*nn) ;
   for( ii=0 ; ii < nn ; ii++ ){
      x = cl->i[ii]*dx; y = cl->j[ii]*dy; z = cl->k[ii]*dz;
      aa[ii]    = sqrt( x*x + y*y + z*z ) ;
      ss[ii]    = (fl_sh3 *) malloc(sizeof(fl_sh3)) ;
      ss[ii]->i = cl->i[ii] ;
      ss[ii]->j = cl->j[ii] ;
      ss[ii]->k = cl->k[ii] ;
      ss[ii]->v = cl->mag[ii] ;
   }

   qsort_floatstuff( nn , aa , (void **)ss ) ;

   for( ii=0 ; ii < nn ; ii++ ){
      cl->mag[ii] = ss[ii]->v ;
      cl->i[ii]   = ss[ii]->i ;
      cl->j[ii]   = ss[ii]->j ;
      cl->k[ii]   = ss[ii]->k ;
      free(ss[ii]) ;
   }

   free(ss) ; free(aa) ; EXRETURN ;
}
