#include "mrilib.h"

/*** 7D SAFE ***/

MRI_IMAGE *mri_to_float( MRI_IMAGE *oldim )
{
   MRI_IMAGE *newim ;
   register int ii , npix ;

WHOAMI ; IMHEADER(oldim) ;

   if( oldim == NULL ) return NULL ;  /* 09 Feb 1999 */

   newim = mri_new_conforming( oldim , MRI_float ) ;
   npix  = oldim->nvox ;

   switch( oldim->kind ){

      case MRI_byte:
         for( ii=0 ; ii < npix ; ii++ )
            newim->im.float_data[ii] = oldim->im.byte_data[ii] ;
      break ;

      case MRI_short:
         for( ii=0 ; ii < npix ; ii++ )
            newim->im.float_data[ii] = oldim->im.short_data[ii] ;
      break ;

      case MRI_int:
         for( ii=0 ; ii < npix ; ii++ )
            newim->im.float_data[ii] = oldim->im.int_data[ii] ;
      break ;

      case MRI_float:
         (void) memcpy( newim->im.float_data ,
                        oldim->im.float_data , sizeof(float) * npix ) ;
      break ;

      case MRI_double:
         for( ii=0 ; ii < npix ; ii++ )
            newim->im.float_data[ii] = oldim->im.double_data[ii] ;
      break ;

      case MRI_complex:
         for( ii=0 ; ii < npix ; ii++ )
            newim->im.float_data[ii] = CABS(oldim->im.complex_data[ii]) ;
      break ;

      case MRI_rgb:{                          /* 11 Feb 1999 */
         byte  * rgb = MRI_RGB_PTR(oldim) ;
         float * far = MRI_FLOAT_PTR(newim) ;
         for( ii=0 ; ii < npix ; ii++ )       /* scale to brightness */
            far[ii] =  0.299 * rgb[3*ii]      /* between 0 and 255     */
                     + 0.587 * rgb[3*ii+1]
                     + 0.114 * rgb[3*ii+2] ;
      }
      break ;

      default:
         fprintf( stderr , "mri_to_float:  unrecognized image kind\n" ) ;
         MRI_FATAL_ERROR ;
   }

   MRI_COPY_AUX(newim,oldim) ;
   return newim ;
}

MRI_IMAGE *mri_scale_to_float( float scl , MRI_IMAGE *oldim )
{
   MRI_IMAGE *newim ;
   register int ii , npix ;
   register float fac ;

WHOAMI ; IMHEADER(oldim) ;

   if( oldim == NULL ) return NULL ;  /* 09 Feb 1999 */

   fac   = scl ;
   newim = mri_new_conforming( oldim , MRI_float ) ;
   npix  = oldim->nvox ;

   switch( oldim->kind ){

      case MRI_byte:
         for( ii=0 ; ii < npix ; ii++ )
            newim->im.float_data[ii] = fac * oldim->im.byte_data[ii] ;
         break ;

      case MRI_short:
         for( ii=0 ; ii < npix ; ii++ )
            newim->im.float_data[ii] = fac * oldim->im.short_data[ii] ;
         break ;

      case MRI_int:
         for( ii=0 ; ii < npix ; ii++ )
            newim->im.float_data[ii] = fac * oldim->im.int_data[ii] ;
         break ;

      case MRI_float:
         for( ii=0 ; ii < npix ; ii++ )
            newim->im.float_data[ii] = fac * oldim->im.float_data[ii] ;
         break ;

      case MRI_double:
         for( ii=0 ; ii < npix ; ii++ )
            newim->im.float_data[ii] = fac * oldim->im.double_data[ii] ;
         break ;

      case MRI_complex:
         for( ii=0 ; ii < npix ; ii++ )
            newim->im.float_data[ii] = fac * CABS(oldim->im.complex_data[ii]) ;
         break ;

      default:
         fprintf( stderr , "mri_to_float:  unrecognized image kind\n" ) ;
         MRI_FATAL_ERROR ;
   }

   MRI_COPY_AUX(newim,oldim) ;
   return newim ;
}

/* 13 Dec 1998 */

#define FAC(q) ( (fac[q] != 0.0) ? fac[q] : 1.0 )

MRI_IMAGE * mri_mult_to_float( float * fac , MRI_IMAGE * oldim )
{
   MRI_IMAGE *newim ;
   register int ii , npix ;

WHOAMI ; IMHEADER(oldim) ;

   newim = mri_new_conforming( oldim , MRI_float ) ;
   npix  = oldim->nvox ;

   switch( oldim->kind ){

      case MRI_byte:
         for( ii=0 ; ii < npix ; ii++ )
            newim->im.float_data[ii] = FAC(ii) * oldim->im.byte_data[ii] ;
         break ;

      case MRI_short:
         for( ii=0 ; ii < npix ; ii++ )
            newim->im.float_data[ii] = FAC(ii) * oldim->im.short_data[ii] ;
         break ;

      case MRI_int:
         for( ii=0 ; ii < npix ; ii++ )
            newim->im.float_data[ii] = FAC(ii) * oldim->im.int_data[ii] ;
         break ;

      case MRI_float:
         for( ii=0 ; ii < npix ; ii++ )
            newim->im.float_data[ii] = FAC(ii) * oldim->im.float_data[ii] ;
         break ;

      case MRI_double:
         for( ii=0 ; ii < npix ; ii++ )
            newim->im.float_data[ii] = FAC(ii) * oldim->im.double_data[ii] ;
         break ;

      case MRI_complex:
         for( ii=0 ; ii < npix ; ii++ )
            newim->im.float_data[ii] = FAC(ii) * CABS(oldim->im.complex_data[ii]) ;
         break ;

      default:
         fprintf( stderr , "mri_to_float:  unrecognized image kind\n" ) ;
         MRI_FATAL_ERROR ;
   }

   MRI_COPY_AUX(newim,oldim) ;
   return newim ;
}
