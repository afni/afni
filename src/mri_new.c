/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/
   
#include "mrilib.h"

/*** 7D SAFE ***/

/*** get a new 2D image ***/

MRI_IMAGE *mri_new( int nx , int ny , MRI_TYPE kind )
{
   MRI_IMAGE *newim ;

   newim = mri_new_7D_generic( nx,ny , 1,1,1,1,1 , kind , TRUE ) ;
   return newim ;
}

/*** get a new 3D image, but with no data inside ***/

MRI_IMAGE *mri_new_vol_empty( int nx , int ny , int nz , MRI_TYPE kind )
{
   MRI_IMAGE *newim ;
   newim = mri_new_7D_generic( nx,ny,nz , 1,1,1,1 , kind , FALSE ) ;
   return newim ;
}

/*** get a new 3D image ***/

MRI_IMAGE *mri_new_vol( int nx , int ny , int nz , MRI_TYPE kind )
{
   MRI_IMAGE *newim ;
   newim = mri_new_7D_generic( nx,ny,nz , 1,1,1,1 , kind , TRUE ) ;
   return newim ;
}

/*** make a new 7D image ***/

MRI_IMAGE *mri_new_7D_generic(
            int nx, int ny, int nz, int nt, int nu, int nv, int nw,
            MRI_TYPE kind , int make_space )
{
   MRI_IMAGE *newim ;
   int npix ;

WHOAMI ;

   newim = (MRI_IMAGE *)malloc( sizeof(MRI_IMAGE) ) ;

   if( newim == NULL ){
      fprintf( stderr , "malloc failure for new image pointer\n" ) ;
      MRI_FATAL_ERROR ;
   }

   newim->nx   = nx ;
   newim->ny   = ny ; newim->nxy   = nx*ny ;
   newim->nz   = nz ; newim->nxyz  = nx*ny*nz ;
   newim->nt   = nt ; newim->nxyzt = nx*ny*nz*nt ;
   newim->nu   = nu ;
   newim->nv   = nv ;
   newim->nw   = nw ; newim->nvox  = newim->nxyzt * nu*nv*nw ;

   newim->kind = kind ;
   newim->name = NULL ;

   newim->dx = newim->dy = newim->dz = 
   newim->dt = newim->du = newim->dv = 1.0 ;  /* default dimensions */

   newim->dw = -666.0 ;  /* 05 Feb 2001 - flag that dimensions aren't set */

   newim->xo = newim->yo = newim->zo = 
   newim->to = newim->uo = newim->vo = newim->wo = 0.0 ;  /* default offsets */

#ifdef USE_MRI_LABELS
   newim->xlab[0] = '\0' ;          /* default labels */
   newim->ylab[0] = '\0' ;
   newim->zlab[0] = '\0' ;
   newim->tlab[0] = '\0' ;
   newim->ulab[0] = '\0' ;
   newim->vlab[0] = '\0' ;
   newim->wlab[0] = '\0' ;
#endif

#ifdef USE_MRI_DELAY
   newim->fname   = NULL ;
   newim->foffset = newim->fondisk = 0 ;
#endif

   npix = newim->nvox ;

   switch( kind ){

      case MRI_byte:
         if( make_space )
            newim->im.byte_data = (byte *)malloc( sizeof(byte) * npix ) ;
         else
            newim->im.byte_data = NULL ;
         newim->pixel_size = sizeof(byte) ;
      break ;

      case MRI_short:
         if( make_space )
            newim->im.short_data = (short *)malloc( sizeof(short) * npix ) ;
         else
            newim->im.short_data = NULL ;
         newim->pixel_size = sizeof(short) ;
      break ;

      case MRI_int:
         if( make_space )
            newim->im.int_data = (int *)malloc( sizeof(int) * npix ) ;
         else
            newim->im.int_data = NULL ;
         newim->pixel_size = sizeof(int) ;
      break ;

      case MRI_float:
         if( make_space )
            newim->im.float_data = (float *)malloc( sizeof(float) * npix ) ;
         else
            newim->im.float_data = NULL ;
         newim->pixel_size = sizeof(float) ;
      break ;

      case MRI_double:
         if( make_space )
            newim->im.double_data = (double *)malloc( sizeof(double) * npix ) ;
         else
            newim->im.double_data = NULL ;
         newim->pixel_size = sizeof(double) ;
      break ;

      case MRI_complex:
         if( make_space )
            newim->im.complex_data = (complex *)malloc( sizeof(complex) * npix ) ;
         else
            newim->im.complex_data = NULL ;
         newim->pixel_size = sizeof(complex) ;
      break ;

      case MRI_rgb:
         if( make_space )
            newim->im.rgb_data = (byte *)malloc( sizeof(byte) * npix * 3 ) ;
         else
            newim->im.rgb_data = NULL ;
         newim->pixel_size = 3 * sizeof(byte) ;
      break ;

      default:
         fprintf( stderr , "mri_new: unrecognized image kind %d\n",(int)kind ) ;
         MRI_FATAL_ERROR ;
   }

   if( make_space && mri_data_pointer(newim) == NULL ){
      fprintf( stderr , "malloc failure for image space\n" ) ;
      MRI_FATAL_ERROR ;
   }

   return newim ;
}
