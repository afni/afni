#include "mrilib.h"

#undef  ERREX
#define ERREX(s) do{ ERROR_message(s); return NULL; } while(0)

floatvec * THD_fitter( int npt , float *far  ,
                       int nref, float *ref[], int meth, int flags )
{
   int jj ;
   float *qfit=NULL, val ;
   floatvec *fv=NULL ;
   int cony = ((flags&1) != 0) ;

   /* check inputs */

   if( npt  <= 1 || far == NULL ||
       nref <= 0 || ref == NULL || nref >= npt-1 )
     ERREX("THD_fitter: bad inputs") ;
   for( jj=0 ; jj < nref ; jj++ )
     if( ref[jj] == NULL ) ERREX("THD_fitter: bad ref") ;

   switch( meth ){

     default: ERREX("THD_fitter: bad meth code") ;
  
     case 2: 
       qfit = lsqfit( npt, far, NULL, nref, ref ) ;
     break ;

     case 1:
       qfit = (float *)malloc(sizeof(float)*nref) ;
       val = cl1_solve( npt, nref, far, ref, qfit, cony ) ;
       if( val < 0.0f ){ free(qfit); qfit = NULL; }
     break ;
   }

   if( qfit == NULL ) return NULL ;  /* bad */

   MAKE_floatvec(fv,nref) ;
   memcpy( fv->ar, qfit, sizeof(float)*nref ) ;
   free(qfit) ;
   return fv ;
}
