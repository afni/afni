/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/

#include "mrilib.h"

/*** 7D SAFE ***/

/*---------------------------------------------------------------------------*/

MRI_IMAGE *mri_to_float( MRI_IMAGE *oldim )
{
   MRI_IMAGE *newim ;
   register int ii , npix ;
   register float *far ;

ENTRY("mri_to_float") ;

   if( oldim == NULL || mri_data_pointer(oldim) == NULL ) RETURN(NULL) ;

   if( oldim->kind == MRI_fvect ){              /* 10 Dec 2008: special case */
     newim = mri_fvect_subimage( oldim , 0 ) ;
     RETURN(newim) ;
   }

   newim = mri_new_conforming( oldim , MRI_float ) ;
   npix  = oldim->nvox ;
   far   = MRI_FLOAT_PTR(newim) ;

   switch( oldim->kind ){

      case MRI_byte:{
         byte *qar = MRI_BYTE_PTR(oldim) ;
         for( ii=0 ; ii < npix ; ii++ ) far[ii] = qar[ii] ;
      }
      break ;

      case MRI_short:{
         short *qar = MRI_SHORT_PTR(oldim) ;
         for( ii=0 ; ii < npix ; ii++ ) far[ii] = qar[ii] ;
      }
      break ;

      case MRI_int:{
         int *qar = MRI_INT_PTR(oldim) ;
         for( ii=0 ; ii < npix ; ii++ ) far[ii] = qar[ii] ;
      }
      break ;

      case MRI_float:{
         float *qar = MRI_FLOAT_PTR(oldim) ;
         (void) memcpy( far , qar , sizeof(float) * npix ) ;
      }
      break ;

      case MRI_double:{
         double *qar = MRI_DOUBLE_PTR(oldim) ;
         for( ii=0 ; ii < npix ; ii++ ) far[ii] = qar[ii] ;
      }
      break ;

      case MRI_complex:{
         complex *qar = MRI_COMPLEX_PTR(oldim) ;
         for( ii=0 ; ii < npix ; ii++ ) far[ii] = CABS(qar[ii]) ;
      }
      break ;

      case MRI_rgb:{                          /* 11 Feb 1999 */
         byte *rgb = MRI_RGB_PTR(oldim) ;
         for( ii=0 ; ii < npix ; ii++ )       /* scale to brightness */
            far[ii] =  0.299 * rgb[3*ii]      /* between 0 and 255     */
                     + 0.587 * rgb[3*ii+1]
                     + 0.114 * rgb[3*ii+2] ;
      }
      break ;

      case MRI_rgba:{                         /* 15 Apr 2002 */
         byte *rgb = (byte *)MRI_RGBA_PTR(oldim) ;
         for( ii=0 ; ii < npix ; ii++ )       /* scale to brightness */
            far[ii] =  0.299 * rgb[4*ii]      /* between 0 and 255     */
                     + 0.587 * rgb[4*ii+1]
                     + 0.114 * rgb[4*ii+2] ;
      }
      break ;

      default:
        fprintf(stderr,"mri_to_float: unrecognized image kind %d\n",oldim->kind);
        MRI_FATAL_ERROR ;
   }

   MRI_COPY_AUX(newim,oldim) ;
   RETURN( newim );
}

/*---------------------------------------------------------------------------*/

MRI_IMAGE *mri_scale_to_float( float scl , MRI_IMAGE *oldim )
{
   MRI_IMAGE *newim ;
   register int ii , npix ;
   register float fac ;
   register float *far ;

ENTRY("mri_scale_to_float") ;

   if( oldim == NULL || mri_data_pointer(oldim) == NULL ) RETURN(NULL) ;

   fac = scl ;
   if( fac==0.0f || fac==1.0f ){ newim = mri_to_float(oldim); RETURN(newim); }

   newim = mri_new_conforming( oldim , MRI_float ) ;
   npix  = oldim->nvox ;
   far   = MRI_FLOAT_PTR(newim) ;
   if( far == NULL ){ mri_free(newim); RETURN(NULL); }

   switch( oldim->kind ){

      case MRI_byte:{
         byte *qar = MRI_BYTE_PTR(oldim) ;
         for( ii=0 ; ii < npix ; ii++ ) far[ii] = fac * qar[ii] ;
      }
      break ;

      case MRI_short:{
         short *qar = MRI_SHORT_PTR(oldim) ;
         for( ii=0 ; ii < npix ; ii++ ) far[ii] = fac * qar[ii] ;
      }
      break ;

      case MRI_int:{
         int *qar = MRI_INT_PTR(oldim) ;
         for( ii=0 ; ii < npix ; ii++ ) far[ii] = fac * qar[ii] ;
      }
      break ;

      case MRI_float:{
         float *qar = MRI_FLOAT_PTR(oldim) ;
         for( ii=0 ; ii < npix ; ii++ ) far[ii] = fac * qar[ii] ;
      }
      break ;

      case MRI_double:{
         double *qar = MRI_DOUBLE_PTR(oldim) ;
         for( ii=0 ; ii < npix ; ii++ ) far[ii] = fac * qar[ii] ;
      }
      break ;

      case MRI_complex:{
         complex *qar = MRI_COMPLEX_PTR(oldim) ;
         for( ii=0 ; ii < npix ; ii++ ) far[ii] = fac * CABS(qar[ii]) ;
      }
      break ;

      case MRI_rgb:{
         byte *rgb = MRI_RGB_PTR(oldim) ;
         for( ii=0 ; ii < npix ; ii++ )
            far[ii] =  fac*(  0.299 * rgb[3*ii]
                            + 0.587 * rgb[3*ii+1]
                            + 0.114 * rgb[3*ii+2] ) ;
      }
      break ;

      case MRI_rgba:{
         byte *rgb = (byte *) MRI_RGBA_PTR(oldim) ;
         for( ii=0 ; ii < npix ; ii++ )
            far[ii] = fac *(  0.299 * rgb[4*ii]
                            + 0.587 * rgb[4*ii+1]
                            + 0.114 * rgb[4*ii+2] ) ;
      }
      break ;

      default:
         fprintf( stderr , "mri_to_float:  unrecognized image kind %d\n",oldim->kind ) ;
         MRI_FATAL_ERROR ;
   }

   MRI_COPY_AUX(newim,oldim) ;
   RETURN( newim );
}

/*---------------------------------------------------------------------------*/
/* 13 Dec 1998 */

#define FAC(q) ( (fac[q] != 0.0) ? fac[q] : 1.0 )

MRI_IMAGE * mri_mult_to_float( float * fac , MRI_IMAGE * oldim )
{
   MRI_IMAGE *newim ;
   register int ii , npix ;
   register float *far ;

ENTRY("mri_mult_to_float") ;

   newim = mri_new_conforming( oldim , MRI_float ) ;
   npix  = oldim->nvox ;
   far   = MRI_FLOAT_PTR(newim) ;

   switch( oldim->kind ){

      case MRI_byte:{
         byte *qar = MRI_BYTE_PTR(oldim) ;
         for( ii=0 ; ii < npix ; ii++ ) far[ii] = FAC(ii) * qar[ii] ;
      }
      break ;

      case MRI_short:{
         short *qar = MRI_SHORT_PTR(oldim) ;
         for( ii=0 ; ii < npix ; ii++ ) far[ii] = FAC(ii) * qar[ii] ;
      }
      break ;

      case MRI_int:{
         int *qar = MRI_INT_PTR(oldim) ;
         for( ii=0 ; ii < npix ; ii++ ) far[ii] = FAC(ii) * qar[ii] ;
      }
      break ;

      case MRI_float:{
         float *qar = MRI_FLOAT_PTR(oldim) ;
         for( ii=0 ; ii < npix ; ii++ ) far[ii] = FAC(ii) * qar[ii] ;
      }
      break ;

      case MRI_double:{
         double *qar = MRI_DOUBLE_PTR(oldim) ;
         for( ii=0 ; ii < npix ; ii++ ) far[ii] = FAC(ii) * qar[ii] ;
      }
      break ;

      case MRI_complex:{
         complex *qar = MRI_COMPLEX_PTR(oldim) ;
         for( ii=0 ; ii < npix ; ii++ ) far[ii] = FAC(ii) * CABS(qar[ii]) ;
      }
      break ;

      default:
         fprintf( stderr , "mri_to_float:  unrecognized image kind\n" ) ;
         MRI_FATAL_ERROR ;
   }

   MRI_COPY_AUX(newim,oldim) ;
   RETURN( newim );
}
