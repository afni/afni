/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/
   
#include "mrilib.h"

/** Only for 2D images **/

MRI_IMAGE * mri_transpose_float( MRI_IMAGE * im )
{
   MRI_IMAGE * om ;
   float * iar , * oar ;
   int ii,jj,nx,ny ;

   if( im == NULL || im->kind != MRI_float ) return NULL ;

   nx  = im->nx ; ny = im->ny ;
   om  = mri_new( ny , nx , MRI_float ) ;
   iar = MRI_FLOAT_PTR(im) ;
   oar = MRI_FLOAT_PTR(om) ;

   for( jj=0 ; jj < ny ; jj++ )
      for( ii=0 ; ii < nx ; ii++ )
         oar[jj+ii*ny] = iar[ii+jj*nx] ;

   MRI_COPY_AUX(om,im) ;
   return om ;
}

MRI_IMAGE * mri_transpose_short( MRI_IMAGE * im )
{
   MRI_IMAGE * om ;
   short * iar , * oar ;
   int ii,jj,nx,ny ;

   if( im == NULL || im->kind != MRI_short ) return NULL ;

   nx  = im->nx ; ny = im->ny ;
   om  = mri_new( ny , nx , MRI_short ) ;
   iar = MRI_SHORT_PTR(im) ;
   oar = MRI_SHORT_PTR(om) ;

   for( jj=0 ; jj < ny ; jj++ )
      for( ii=0 ; ii < nx ; ii++ )
         oar[jj+ii*ny] = iar[ii+jj*nx] ;

   MRI_COPY_AUX(om,im) ;
   return om ;
}

MRI_IMAGE * mri_transpose_byte( MRI_IMAGE * im )
{
   MRI_IMAGE * om ;
   byte * iar , * oar ;
   int ii,jj,nx,ny ;

   if( im == NULL || im->kind != MRI_byte ) return NULL ;

   nx  = im->nx ; ny = im->ny ;
   om  = mri_new( ny , nx , MRI_byte ) ;
   iar = MRI_BYTE_PTR(im) ;
   oar = MRI_BYTE_PTR(om) ;

   for( jj=0 ; jj < ny ; jj++ )
      for( ii=0 ; ii < nx ; ii++ )
         oar[jj+ii*ny] = iar[ii+jj*nx] ;

   MRI_COPY_AUX(om,im) ;
   return om ;
}

MRI_IMAGE * mri_transpose_int( MRI_IMAGE * im )
{
   MRI_IMAGE * om ;
   int * iar , * oar ;
   int ii,jj,nx,ny ;

   if( im == NULL || im->kind != MRI_int ) return NULL ;

   nx  = im->nx ; ny = im->ny ;
   om  = mri_new( ny , nx , MRI_int ) ;
   iar = MRI_INT_PTR(im) ;
   oar = MRI_INT_PTR(om) ;

   for( jj=0 ; jj < ny ; jj++ )
      for( ii=0 ; ii < nx ; ii++ )
         oar[jj+ii*ny] = iar[ii+jj*nx] ;

   MRI_COPY_AUX(om,im) ;
   return om ;
}


MRI_IMAGE * mri_transpose( MRI_IMAGE * im )
{
   if( im == NULL ) return NULL ;

   switch( im->kind ){
      case MRI_float: return mri_transpose_float(im) ;
      case MRI_short: return mri_transpose_short(im) ;
      case MRI_byte : return mri_transpose_byte (im) ;
      case MRI_int  : return mri_transpose_int  (im) ;
   }
   return NULL ;
}
