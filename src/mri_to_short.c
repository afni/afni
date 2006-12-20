/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/

#include "mrilib.h"

/*** 7D SAFE ***/

MRI_IMAGE *mri_to_short( double scl , MRI_IMAGE *oldim )
{
   MRI_IMAGE *newim ;
   register int ii , npix ;
   register double scale , val ;
   register short *sar ;

ENTRY("mri_to_short") ;

   if( oldim == NULL ) RETURN( NULL );  /* 09 Feb 1999 */

   newim = mri_new_conforming( oldim , MRI_short ) ;
   sar   = MRI_SHORT_PTR(newim) ;
   npix  = oldim->nvox ;

   if( scl == 0.0 ){
      switch( oldim->kind ){
         case MRI_int:
         case MRI_float:
         case MRI_double:
         case MRI_complex:
            scale = mri_maxabs( oldim ) ;
            if( scale != 0.0 ) scale = 10000.0 / scale ;
#ifdef MRI_DEBUG
   fprintf( stderr , "mri_to_short: scale factor = %e\n" , scale ) ;
#endif
         break ;

         default:
            scale = 1.0 ;
         break ;
      }
   } else {
      scale = scl ;
   }

   switch( oldim->kind ){

      case MRI_rgb:{
         byte *rgb = MRI_RGB_PTR(oldim) ;
         float rfac=0.299*scale , gfac=0.587*scale , bfac=0.114*scale ;

         for( ii=0 ; ii < npix ; ii++ )
            sar[ii] = (short)(  rfac * rgb[3*ii]
                              + gfac * rgb[3*ii+1]
                              + bfac * rgb[3*ii+2] ) ;
      }
      break ;

      case MRI_byte:{
         byte *qar = MRI_BYTE_PTR(oldim) ;
         if( scale != 1.0 )
            for( ii=0 ; ii < npix ; ii++ ){
               val = scale * qar[ii] ;
               sar[ii] = SHORTIZE(val) ;
            }
         else
            for( ii=0 ; ii < npix ; ii++ )
               sar[ii] = (short) qar[ii] ;
         break ;
      }

      case MRI_short:{
         short *qar = MRI_SHORT_PTR(oldim) ;
         if( scale != 1.0 )
            for( ii=0 ; ii < npix ; ii++ ){
               val = scale * qar[ii] ;
               sar[ii] = SHORTIZE(val) ;
            }
         else
            (void) memcpy( sar , qar , sizeof(short)*npix ) ;
         break ;
      }

      case MRI_int:{
         int *qar = MRI_INT_PTR(oldim) ;
         if( scale != 1.0 )
            for( ii=0 ; ii < npix ; ii++ ){
               val = scale * qar[ii] ;
               sar[ii] = SHORTIZE(val) ;
            }
         else
            for( ii=0 ; ii < npix ; ii++ )
               sar[ii] = SHORTIZE(qar[ii]) ;
         break ;
      }

      case MRI_float:{
         float *qar = MRI_FLOAT_PTR(oldim) ;
         if( scale != 1.0 )
            for( ii=0 ; ii < npix ; ii++ ){
               val = scale * qar[ii] ;
               sar[ii] = SHORTIZE(val) ;
            }
         else
            for( ii=0 ; ii < npix ; ii++ )
               sar[ii] = SHORTIZE(qar[ii]) ;
         break ;
      }

      case MRI_double:{
         double *qar = MRI_DOUBLE_PTR(oldim) ;
         for( ii=0 ; ii < npix ; ii++ )
            sar[ii] = scale * qar[ii] ;
         break ;
      }

      case MRI_complex:{
        complex *qar = MRI_COMPLEX_PTR(oldim) ;
        for( ii=0 ; ii < npix ; ii++ )
           sar[ii] = scale * CABS(qar[ii]) ;
        break ;
     }

      default:
         fprintf( stderr , "mri_to_short:  unrecognized image kind\n" ) ;
         MRI_FATAL_ERROR ;
   }

   MRI_COPY_AUX(newim,oldim) ;
   RETURN( newim );
}

/*========================================================================*/

MRI_IMAGE *mri_to_short_scl( double scl , double lev , MRI_IMAGE *oldim )
{
   MRI_IMAGE * im ;
   im =  mri_to_short_sclip( scl , lev , 0,0 , oldim ) ;
   return im ;
}


/* scale, lev->0, and clip */

MRI_IMAGE *mri_to_short_sclip( double scl , double lev ,
                               int bot , int top , MRI_IMAGE *oldim )
{
   MRI_IMAGE *newim ;
   register int ii , npix ;
   double   imin,imax ;
   register double dscale , dbbot ;
   register float  scale  , flbot , val ;
   register short * ar ;

ENTRY("mri_to_short_sclip") ;

   if( oldim == NULL ) RETURN( NULL );  /* 09 Feb 1999 */

   newim = mri_new_conforming( oldim , MRI_short ) ;
   npix  = oldim->nvox ;

   if( scl == 0 ){  /* compute scaling to make [min..max] -> [0..lev] */

      imin = (oldim->kind==MRI_complex || oldim->kind==MRI_rgb) ? (0) : mri_min(oldim) ;
      imax = mri_max( oldim ) ;
      imax = (imax <= imin) ? imin+1 : imax ;

      scale = dscale = (lev+0.99) / (imax-imin)  ;
      flbot = dbbot  = imin ;

   } else {          /* user controlled scaling, with lev -> 0 */
      scale = dscale = scl ;
      flbot = dbbot  = lev ;
   }

   ar = mri_data_pointer( newim ) ;  /* fast access to data */

   switch( oldim->kind ){

      case MRI_rgb:{
         register byte * rgb = mri_data_pointer(oldim) ;
         float rfac=0.299*scale , gfac=0.587*scale , bfac=0.114*scale ;
         for( ii=0 ; ii < npix ; ii++ )
            ar[ii] = (short) (  rfac * rgb[3*ii]
                              + gfac * rgb[3*ii+1]
                              + bfac * rgb[3*ii+2] ) ;
      }
      break ;

      case MRI_byte:{
         register byte * oar = mri_data_pointer(oldim) ;
         for( ii=0 ; ii < npix ; ii++ ){
            val = scale * (oar[ii]-flbot) ;
            ar[ii] = BYTEIZE(val) ;
         }
         break ;
      }

      case MRI_short:{
         register short * oar = mri_data_pointer(oldim) ;
         for( ii=0 ; ii < npix ; ii++ ){
            val = scale * (oar[ii]-flbot) ;
            ar[ii] = SHORTIZE(val) ;
         }
         break ;
      }

      case MRI_int:{
         register int * oar = mri_data_pointer(oldim) ;
         for( ii=0 ; ii < npix ; ii++ )
            ar[ii] = scale * (oar[ii]-flbot) ;
         break ;
      }

      case MRI_float:{
         register float * oar = mri_data_pointer(oldim) ;
         for( ii=0 ; ii < npix ; ii++ )
            ar[ii] = scale * (oar[ii]-flbot) ;
         break ;
      }

      case MRI_double:{
         register double * oar = mri_data_pointer(oldim) ;
         for( ii=0 ; ii < npix ; ii++ )
            ar[ii] = dscale * (oar[ii]-dbbot) ;
         break ;
      }

      case MRI_complex:{
         register complex * oar = mri_data_pointer(oldim) ;
         for( ii=0 ; ii < npix ; ii++ )
            ar[ii] = scale * CABS(oar[ii]) ;
         break ;
      }

      default:
         fprintf( stderr , "mri_to_short_scl:  unrecognized image kind\n" ) ;
         MRI_FATAL_ERROR ;
   }

   /* clip, if desired */

   if( bot < top ){
      register short bb = bot , tt = top ;
      for( ii=0 ; ii < npix ; ii++ ){
             if( ar[ii] < bb ) ar[ii] = bb ;
       else if( ar[ii] > tt ) ar[ii] = tt ;
      }
   }

   MRI_COPY_AUX(newim,oldim) ;
   RETURN( newim );
}
