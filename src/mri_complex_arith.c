/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/

#include "mrilib.h"

/*** 7D SAFE ***/

/*-------------------------------------
     mode = 0 for normal multiplication
          = 1 for conjugation of g
---------------------------------------*/

MRI_IMAGE *mri_multiply_complex( int mode , MRI_IMAGE *f , MRI_IMAGE* g )
{
   register int ii , npix ;
   MRI_IMAGE *newImg ;
   complex *nar , *gar , *far ;

   if( f->nvox != g->nvox ){
      fprintf( stderr , "mri_multiply_complex shapes imcompatible!\n" ) ;
      MRI_FATAL_ERROR ;
   }

   if( f->kind != MRI_complex  ||  g->kind != MRI_complex ){
      fprintf( stderr , "mri_multiply_complex illegal image type!\n" ) ;
      MRI_FATAL_ERROR ;
   }

   newImg  = mri_new_conforming( f , MRI_complex ) ;
   npix = f->nvox ;
   MRI_COPY_AUX( newImg , f ) ;
   far = MRI_COMPLEX_PTR(f); gar = MRI_COMPLEX_PTR(g); nar = MRI_COMPLEX_PTR(newImg);

   switch( mode ){
     case 0:
        for( ii=0 ; ii < npix ; ii++ ){
           nar[ii] = CMULT( far[ii] , gar[ii] ) ;
        }
        break ;

      case 1:
         for( ii=0 ; ii < npix ; ii++ ){
            nar[ii] = CJMULT( far[ii] , gar[ii] ) ;
         }
         break ;

      default:
         fprintf( stderr , "mri_multiply_complex illegal mode %d\n" , mode ) ;
         MRI_FATAL_ERROR ;
   }
   return newImg ;
}

/****************************************************************************/

MRI_IMAGE *mri_complex_phase( MRI_IMAGE *im )
{
   register int ii , npix ;
   MRI_IMAGE *newImg ;
   float *nar ; complex *iar ;

   if( im->kind != MRI_complex ){
      fprintf( stderr , "mri_complex_phase illegal image type!\n" ) ;
      MRI_FATAL_ERROR ;
   }

   npix = im->nvox ;
   newImg  = mri_new_conforming( im , MRI_float ) ;
   MRI_COPY_AUX( newImg , im ) ;
   iar = MRI_COMPLEX_PTR(im) ; nar = MRI_FLOAT_PTR(newImg) ;

   for( ii=0 ; ii < npix ; ii++ )
     nar[ii] = atan2( iar[ii].i , iar[ii].r ) ;

   return newImg ;
}

/*--------------------------------------------------------------------------*/

float complex_abs( complex z )  /* 24 Aug 2009 */
{
   float x , y , val ;

   x = fabsf(z.r) ; floatfix(x) ;
   y = fabsf(z.i) ; floatfix(y) ;
   if( x > y && x > 0.0f )
     val = x*sqrtf(1.0f+(y*y)/(x*x)) ;
   else if( y > x && y > 0.0f )
     val = y*sqrtf(1.0f+(x*x)/(y*y)) ;
   else
     val = x*1.414214f ;  /* case x==y */

   floatfix(val) ; return val ;
}

/***************************************************************************/

MRI_IMAGE *mri_complex_abs( MRI_IMAGE *im )
{
   register int ii , npix ;
   MRI_IMAGE *newImg ;
   float *nar ; complex *iar ;

   if( im->kind != MRI_complex ){
      fprintf( stderr , "mri_complex_abs illegal type!\n" ) ;
      MRI_FATAL_ERROR ;
   }

   npix = im->nvox ;
   newImg  = mri_new_conforming( im , MRI_float ) ;
   MRI_COPY_AUX( newImg , im ) ;
   iar = MRI_COMPLEX_PTR(im) ; nar = MRI_FLOAT_PTR(newImg) ;

   for( ii=0 ; ii < npix ; ii++ ) nar[ii] = complex_abs(iar[ii]) ;

   return newImg ;
}
