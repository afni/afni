/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/
   
#include "mrilib.h"

/*** 7D SAFE ***/

MRI_IMAGE *mri_to_byte( MRI_IMAGE *oldim )
{
   MRI_IMAGE *newim ;
   register int ii , npix ;
   double   imin,imax ;
   register double scale ;
   short  shbot ;
   int    inbot ;
   float  flbot ;
   double dbbot ;
   byte *ar ;

ENTRY("mri_to_byte") ;

   if( oldim == NULL ) RETURN( NULL );  /* 09 Feb 1999 */

   newim = mri_new_conforming( oldim , MRI_byte ) ;
   npix  = oldim->nvox ;

   switch( oldim->kind ){
      case MRI_short:
      case MRI_int:
      case MRI_float:
      case MRI_double:
         imin  = mri_min( oldim ) ;
         imax  = mri_max( oldim ) ;
         imax  = (imax <= imin) ? imin+1 : imax ;
         scale = 255.9 / (imax-imin)  ;
         switch( oldim->kind ){
            case MRI_short:    shbot = imin ; break ;
            case MRI_int:      inbot = imin ; break ;
            case MRI_float:    flbot = imin ; break ;
            case MRI_double:   dbbot = imin ; break ;
         }
         break ;

      case MRI_complex:
         scale = 255.9 / mri_max( oldim ) ;
         break ;
   }

   ar = MRI_BYTE_PTR( newim ) ;  /* fast access to data */

   switch( oldim->kind ){

      case MRI_byte:
#ifdef DONT_USE_MEMCPY
         for( ii=0 ; ii < npix ; ii++ )
            ar[ii] = oldim->im.byte_data[ii] ;
#else
         (void) memcpy( ar , oldim->im.byte_data , sizeof(byte)*npix ) ;
#endif
         break ;

      case MRI_rgb:{                           /* 13 Nov 2002 */
         byte *rgb = oldim->im.rgb_data ;
         float rfac=0.299*scale , gfac=0.587*scale , bfac=0.114*scale , val ;
         for( ii=0 ; ii < npix ; ii++ ){
           val = rfac * rgb[3*ii] + gfac * rgb[3*ii+1] + bfac * rgb[3*ii+2] ;
           ar[ii] = BYTEIZE(val) ;
         }
      }
      break ;

      case MRI_short:
         for( ii=0 ; ii < npix ; ii++ )
            ar[ii] = scale * (oldim->im.short_data[ii]-shbot) ;
         break ;

      case MRI_int:
         for( ii=0 ; ii < npix ; ii++ )
            ar[ii] = scale * (oldim->im.int_data[ii]-inbot) ;
         break ;

      case MRI_float:
         for( ii=0 ; ii < npix ; ii++ )
            ar[ii] = scale * (oldim->im.float_data[ii]-flbot) ;
         break ;

      case MRI_double:
         for( ii=0 ; ii < npix ; ii++ )
            ar[ii] = scale * (oldim->im.double_data[ii]-dbbot) ;
         break ;

      case MRI_complex:
         for( ii=0 ; ii < npix ; ii++ )
            ar[ii] = scale * CABS(oldim->im.complex_data[ii]) ;
         break ;

      default:
         fprintf( stderr , "mri_to_byte:  unrecognized image kind\n" ) ;
         MRI_FATAL_ERROR ;
   }

   MRI_COPY_AUX(newim,oldim) ;
   RETURN( newim );
}

/*----------------------------------------------------------------------------*/

MRI_IMAGE *mri_to_byte_scl( double scl , double lev , MRI_IMAGE *oldim )
{
   MRI_IMAGE *newim ;
   register int ii , npix ;
   double   imin,imax ;
   register double dscale , dbbot ;
   register float  scale  , flbot ;
   register byte *ar ;

ENTRY("mri_to_byte_scl") ;

   if( oldim == NULL ) RETURN( NULL );  /* 09 Feb 1999 */

   newim = mri_new_conforming( oldim , MRI_byte ) ;
   npix  = oldim->nvox ;

   if( scl == 0 ){  /* compute scaling to make (min..max) -> (0..lev) */

      imin = (oldim->kind == MRI_complex) ? (0) : mri_min( oldim ) ;
      imax = mri_max( oldim ) ;
      imax = (imax <= imin) ? imin+1 : imax ;

      scale = dscale = (lev+0.99) / (imax-imin)  ;
      flbot = dbbot  = imin ;

   } else {          /* user controlled scaling, with lev -> 0 */
      scale = dscale = scl ;
      flbot = dbbot  = lev ;
   }

   ar = MRI_BYTE_PTR( newim ) ;  /* fast access to data */

   switch( oldim->kind ){

      case MRI_byte:
         for( ii=0 ; ii < npix ; ii++ )
            ar[ii] = scale * (oldim->im.byte_data[ii]-flbot) ;
         break ;

      case MRI_short:
         for( ii=0 ; ii < npix ; ii++ )
            ar[ii] = scale * (oldim->im.short_data[ii]-flbot) ;
         break ;

      case MRI_int:
         for( ii=0 ; ii < npix ; ii++ )
            ar[ii] = scale * (oldim->im.int_data[ii]-flbot) ;
         break ;

      case MRI_float:
         for( ii=0 ; ii < npix ; ii++ )
            ar[ii] = scale * (oldim->im.float_data[ii]-flbot) ;
         break ;

      case MRI_double:
         for( ii=0 ; ii < npix ; ii++ )
            ar[ii] = dscale * (oldim->im.double_data[ii]-dbbot) ;
         break ;

      case MRI_complex:
         for( ii=0 ; ii < npix ; ii++ )
            ar[ii] = scale * CABS(oldim->im.complex_data[ii]) ;
         break ;

      default:
         fprintf( stderr , "mri_to_byte_scl:  unrecognized image kind\n" ) ;
         MRI_FATAL_ERROR ;
   }

   MRI_COPY_AUX(newim,oldim) ;
   RETURN( newim );
}
