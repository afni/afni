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

ENTRY("mri_transpose_float") ;

   if( im == NULL || im->kind != MRI_float ) RETURN(NULL) ;

   nx  = im->nx ; ny = im->ny ;
   om  = mri_new( ny , nx , MRI_float ) ;
   iar = MRI_FLOAT_PTR(im) ;
   oar = MRI_FLOAT_PTR(om) ;

   for( jj=0 ; jj < ny ; jj++ )
      for( ii=0 ; ii < nx ; ii++ )
         oar[jj+ii*ny] = iar[ii+jj*nx] ;

   MRI_COPY_AUX(om,im) ;
   RETURN(om) ;
}

MRI_IMAGE * mri_transpose_double( MRI_IMAGE * im )
{
   MRI_IMAGE * om ;
   double * iar , * oar ;
   int ii,jj,nx,ny ;

ENTRY("mri_transpose_double") ;

   if( im == NULL || im->kind != MRI_double ) RETURN(NULL) ;

   nx  = im->nx ; ny = im->ny ;
   om  = mri_new( ny , nx , MRI_double ) ;
   iar = MRI_DOUBLE_PTR(im) ;
   oar = MRI_DOUBLE_PTR(om) ;

   for( jj=0 ; jj < ny ; jj++ )
      for( ii=0 ; ii < nx ; ii++ )
         oar[jj+ii*ny] = iar[ii+jj*nx] ;

   MRI_COPY_AUX(om,im) ;
   RETURN(om) ;
}

MRI_IMAGE * mri_transpose_short( MRI_IMAGE * im )
{
   MRI_IMAGE * om ;
   short * iar , * oar ;
   int ii,jj,nx,ny ;

ENTRY("mri_transpose_short") ;

   if( im == NULL || im->kind != MRI_short ) RETURN(NULL) ;

   nx  = im->nx ; ny = im->ny ;
   om  = mri_new( ny , nx , MRI_short ) ;
   iar = MRI_SHORT_PTR(im) ;
   oar = MRI_SHORT_PTR(om) ;

   for( jj=0 ; jj < ny ; jj++ )
      for( ii=0 ; ii < nx ; ii++ )
         oar[jj+ii*ny] = iar[ii+jj*nx] ;

   MRI_COPY_AUX(om,im) ;
   RETURN(om) ;
}

MRI_IMAGE * mri_transpose_byte( MRI_IMAGE * im )
{
   MRI_IMAGE * om ;
   byte * iar , * oar ;
   int ii,jj,nx,ny ;

ENTRY("mri_transpose_byte") ;

   if( im == NULL || im->kind != MRI_byte ) RETURN(NULL) ;

   nx  = im->nx ; ny = im->ny ;
   om  = mri_new( ny , nx , MRI_byte ) ;
   iar = MRI_BYTE_PTR(im) ;
   oar = MRI_BYTE_PTR(om) ;

   for( jj=0 ; jj < ny ; jj++ )
      for( ii=0 ; ii < nx ; ii++ )
         oar[jj+ii*ny] = iar[ii+jj*nx] ;

   MRI_COPY_AUX(om,im) ;
   RETURN(om) ;
}

MRI_IMAGE * mri_transpose_int( MRI_IMAGE * im )
{
   MRI_IMAGE * om ;
   int * iar , * oar ;
   int ii,jj,nx,ny ;

ENTRY("mri_transpose_int") ;

   if( im == NULL || im->kind != MRI_int ) RETURN(NULL) ;

   nx  = im->nx ; ny = im->ny ;
   om  = mri_new( ny , nx , MRI_int ) ;
   iar = MRI_INT_PTR(im) ;
   oar = MRI_INT_PTR(om) ;

   for( jj=0 ; jj < ny ; jj++ )
      for( ii=0 ; ii < nx ; ii++ )
         oar[jj+ii*ny] = iar[ii+jj*nx] ;

   MRI_COPY_AUX(om,im) ;
   RETURN(om) ;
}

MRI_IMAGE * mri_transpose_complex( MRI_IMAGE * im )
{
   MRI_IMAGE * om ;
   complex * iar , * oar ;
   int ii,jj,nx,ny ;

ENTRY("mri_transpose_complex") ;

   if( im == NULL || im->kind != MRI_complex ) RETURN(NULL) ;

   nx  = im->nx ; ny = im->ny ;
   om  = mri_new( ny , nx , MRI_complex ) ;
   iar = MRI_COMPLEX_PTR(im) ;
   oar = MRI_COMPLEX_PTR(om) ;

   for( jj=0 ; jj < ny ; jj++ )
      for( ii=0 ; ii < nx ; ii++ )
         oar[jj+ii*ny] = iar[ii+jj*nx] ;

   MRI_COPY_AUX(om,im) ;
   RETURN(om) ;
}

MRI_IMAGE * mri_transpose_rgbyte( MRI_IMAGE * im )
{
   MRI_IMAGE * om ;
   rgbyte * iar , * oar ;
   int ii,jj,nx,ny ;

ENTRY("mri_transpose_rgbyte") ;

   if( im == NULL || im->kind != MRI_rgb ) RETURN(NULL) ;

   nx  = im->nx ; ny = im->ny ;
   om  = mri_new( ny , nx , MRI_rgb ) ;
   iar = (rgbyte *) MRI_RGB_PTR(im) ;
   oar = (rgbyte *) MRI_RGB_PTR(om) ;

   for( jj=0 ; jj < ny ; jj++ )
      for( ii=0 ; ii < nx ; ii++ )
         oar[jj+ii*ny] = iar[ii+jj*nx] ;

   MRI_COPY_AUX(om,im) ;
   RETURN(om) ;
}


MRI_IMAGE * mri_transpose( MRI_IMAGE * im )
{
   if( im == NULL ) return NULL ;

   switch( im->kind ){
      case MRI_float  : return mri_transpose_float  (im) ;
      case MRI_short  : return mri_transpose_short  (im) ;
      case MRI_byte   : return mri_transpose_byte   (im) ;
      case MRI_int    : return mri_transpose_int    (im) ;
      case MRI_complex: return mri_transpose_complex(im) ;
      case MRI_rgb:     return mri_transpose_rgbyte (im) ;
      case MRI_double:  return mri_transpose_double (im) ;
      default:          fprintf(stderr,"Cannot transpose type %d.\n", im->kind);
   }
   return NULL ;
}


/* Interleave the columns of data, given nfirst groups of nint columns
 * (nfirst*nint = ny).  Return a new MRI_IMAGE (so we don't have to do
 * this in place.
 *
 *     for ifirst = 0..nfirst
 *        for iint = 0..nint
 *           col[iint + ifirst*nint] <- col[ifirst + iint*nfirst]
 *
 * Each of the ny columns will keep the same nx values, except that
 * they will be moved to a different column.       27 Jul 2009 [rickr]
 */
MRI_IMAGE * mri_interleave_columns( MRI_IMAGE * oldim, int nint )
{
   MRI_IMAGE *newim=NULL;
   char      *dold, *dnew;
   int       ifirst, iint, nfirst, colsize;
    
ENTRY("mri_interleave_columns") ;

   /* check that the inputs seem good */
   if( !oldim ) RETURN(NULL);
   if( nint <= 0 || nint > oldim->ny ) {
      fprintf(stderr,"** interleave_cols: invalid nint=%d (ny=%d)\n",
              nint, oldim->ny);
      RETURN(NULL);
   }
   if( oldim->pixel_size <= 0 || oldim->pixel_size > 8 ) {
      fprintf(stderr,"** interleave_cols: invalid pixel_size %d\n",
              oldim->pixel_size);
      RETURN(NULL);
   }

   nfirst = oldim->ny / nint;
   if( nint * nfirst != oldim->ny ) {
      fprintf(stderr,"** interleave_cols: nint * nfirst != ny (%d,%d,%d)\n",
              nint, nfirst, oldim->ny);
      RETURN(NULL);
   }
   if( oldim->nx * oldim->ny != oldim->nvox ) {
      fprintf(stderr,"** interleave_cols: nx*ny != nvox (%d,%d,%d)\n",
              oldim->nx, oldim->ny, oldim->nvox);
      RETURN(NULL);
   }

   /* all seems well, make a copy image and shuffle the data */
   newim = mri_copy(oldim) ;
   if( !newim ){
      fprintf(stderr,"** mri_interleave_columns: failed to copy old image\n");
      RETURN(NULL);
   }

   /* permute data, ignoring data type (except for pixel_size) */
   dold = (char *)oldim->im;
   dnew = (char *)newim->im;
   colsize = oldim->nx * oldim->pixel_size;
   for( ifirst=0; ifirst < nfirst; ifirst++ )
      for( iint=0; iint < nint; iint++ ) {
         memcpy(dnew + (iint   + ifirst * nint  )*colsize,
                dold + (ifirst + iint   * nfirst)*colsize,
                colsize);
      }

#ifdef USE_MRI_LABELS
   fprintf(stderr,"** mri_interleave_columns: need to process labels\n");
#endif

   RETURN(newim);
}
