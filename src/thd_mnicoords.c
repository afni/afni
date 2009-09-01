#include "mrilib.h"

/*-----------------------------------------------------------------------*/
/*! Convert MNI coordinates (LPI) to TT Atlas coordinates (RAI);
    cf. http://www.mrc-cbu.cam.ac.uk/Imaging/mnispace.html.
-------------------------------------------------------------------------*/

THD_fvec3 THD_mni_to_tta( THD_fvec3 mv )
{
   float mx,my,mz , tx,ty,tz ;
   THD_fvec3 tv ;

   mx = mv.xyz[0] ; my = mv.xyz[1] ; mz = mv.xyz[2] ;

   tx = 0.99 * mx ;

   if( mz > 0.0 ){
     ty =  0.9688 * my + 0.0460 * mz ;
     tz = -0.0485 * my + 0.9189 * mz ;
   } else {
     ty =  0.9688 * my + 0.0420 * mz ;
     tz = -0.0485 * my + 0.8390 * mz ;
   }

   tx = -tx ; ty = -ty ;         /* flip x,y from LPI to RAI */

   LOAD_FVEC3( tv , tx,ty,tz ) ; return tv ;
}

/*-----------------------------------------------------------------------*/
/*! Convert TTA coordinates (RAI) to MNI coordinates (LPI).
-------------------------------------------------------------------------*/

THD_fvec3 THD_tta_to_mni( THD_fvec3 tv )
{
   float mx,my,mz , tx,ty,tz ;
   THD_fvec3 mv ;

   tx = -tv.xyz[0] ; ty = -tv.xyz[1] ;  /* flip xy from RAI to LPI */
   tz =  tv.xyz[2] ;

   mx = 1.01010 * tx ;
   my = 1.02962 * ty - 0.05154 * tz ;
   mz = 0.05434 * ty + 1.08554 * tz ;
   if( mz < 0.0 ) mz *= 1.09523 ;
   LOAD_FVEC3( mv , mx,my,mz ) ; return mv ;
}

/*-----------------------------------------------------------------------*/

void THD_3tta_to_3mni( float *x , float *y , float *z )
{
   THD_fvec3 mv , tv ;
   LOAD_FVEC3( tv , *x,*y,*z ) ;
   mv = THD_tta_to_mni( tv ) ;
   *x = mv.xyz[0] ; *y = mv.xyz[1] ; *z = mv.xyz[2] ; return ;
}

/*-----------------------------------------------------------------------*/

void THD_3mni_to_3tta( float *x , float *y , float *z )
{
   THD_fvec3 mv , tv ;
   LOAD_FVEC3( mv , *x,*y,*z ) ;
   tv = THD_mni_to_tta( mv ) ;
   *x = tv.xyz[0] ; *y = tv.xyz[1] ; *z = tv.xyz[2] ; return ;
}
