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
   MRI_IMAGE *new ;

WHOAMI ; IMHEADER(f) ; IMHEADER(g) ;

   if( f->nvox != g->nvox ){
      fprintf( stderr , "mri_multiply_complex shapes imcompatible!\n" ) ;
      MRI_FATAL_ERROR ;
   }

   if( f->kind != MRI_complex  ||  g->kind != MRI_complex ){
      fprintf( stderr , "mri_multiply_complex illegal image type!\n" ) ;
      MRI_FATAL_ERROR ;
   }

   new  = mri_new_conforming( f , MRI_complex ) ;
   npix = f->nvox ;
   MRI_COPY_AUX( new , f ) ;

   switch( mode ){
     case 0:
        for( ii=0 ; ii < npix ; ii++ ){
           new->im.complex_data[ii] =
            CMULT( f->im.complex_data[ii] , g->im.complex_data[ii] ) ;
        }
        break ;

      case 1:
         for( ii=0 ; ii < npix ; ii++ ){
            new->im.complex_data[ii] =
             CJMULT( f->im.complex_data[ii] , g->im.complex_data[ii] ) ;
         }
         break ;

      default:
         fprintf( stderr , "mri_multiply_complex illegal mode %d\n" , mode ) ;
         MRI_FATAL_ERROR ;
   }
   return new ;
}

/****************************************************************************/

MRI_IMAGE *mri_complex_phase( MRI_IMAGE *im )
{
   register int ii , npix ;
   MRI_IMAGE *new ;

WHOAMI ; IMHEADER(im) ;

   if( im->kind != MRI_complex ){
      fprintf( stderr , "mri_complex_phase illegal image type!\n" ) ;
      MRI_FATAL_ERROR ;
   }

   npix = im->nvox ;
   new  = mri_new_conforming( im , MRI_float ) ;
   MRI_COPY_AUX( new , im ) ;

   for( ii=0 ; ii < npix ; ii++ )
     new->im.float_data[ii] =
        atan2( im->im.complex_data[ii].i , im->im.complex_data[ii].r ) ;

   return new ;
}

/***************************************************************************/

MRI_IMAGE *mri_complex_abs( MRI_IMAGE *im )
{
   register int ii , npix ;
   MRI_IMAGE *new ;

WHOAMI ; IMHEADER(im) ;

   if( im->kind != MRI_complex ){
      fprintf( stderr , "mri_complex_abs illegal type!\n" ) ;
      MRI_FATAL_ERROR ;
   }

   npix = im->nvox ;
   new  = mri_new_conforming( im , MRI_float ) ;
   MRI_COPY_AUX( new , im ) ;

   for( ii=0 ; ii < npix ; ii++ )
      new->im.float_data[ii] = sqrt( CSQR( im->im.complex_data[ii] ) ) ;

   return new ;
}
