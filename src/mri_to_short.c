#include "mrilib.h"

/*****************************************************************************
  This software is copyrighted and owned by the Medical College of Wisconsin.
  See the file README.Copyright for details.
******************************************************************************/

/*** 7D SAFE ***/

MRI_IMAGE *mri_to_short( double scl , MRI_IMAGE *oldim )
{
   MRI_IMAGE *newim ;
   register int ii , npix ;
   register double scale ;

WHOAMI ; IMHEADER(oldim) ;

   newim = mri_new_conforming( oldim , MRI_short ) ;
   npix  = oldim->nvox ;

   if( scl == 0.0 ){
      switch( oldim->kind ){
         case MRI_int:
         case MRI_float:
         case MRI_double:
         case MRI_complex:
            scale = mri_max( oldim ) ;
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

      case MRI_byte:
         if( scale != 1.0 )
            for( ii=0 ; ii < npix ; ii++ )
               newim->im.short_data[ii] = scale * oldim->im.byte_data[ii] ;
         else
            for( ii=0 ; ii < npix ; ii++ )
               newim->im.short_data[ii] = (short) oldim->im.byte_data[ii] ;
         break ;

      case MRI_short:
#ifndef DONT_USE_MEMCPY       /* this is a double negative! */
         if( scale != 1.0 )
#endif
            for( ii=0 ; ii < npix ; ii++ )
               newim->im.short_data[ii] = scale * oldim->im.short_data[ii] ;
#ifndef DONT_USE_MEMCPY
         else
            (void) memcpy( newim->im.short_data ,
                           oldim->im.short_data , sizeof(short)*npix ) ;
#endif
         break ;

      case MRI_int:
         if( scale != 1.0 )
            for( ii=0 ; ii < npix ; ii++ )
               newim->im.short_data[ii] = scale * oldim->im.int_data[ii] ;
         else
            for( ii=0 ; ii < npix ; ii++ )
               newim->im.short_data[ii] = (int) oldim->im.int_data[ii] ;
         break ;

      case MRI_float:
         if( scale != 1.0 )
            for( ii=0 ; ii < npix ; ii++ )
               newim->im.short_data[ii] = scale * oldim->im.float_data[ii] ;
         else
            for( ii=0 ; ii < npix ; ii++ )
               newim->im.short_data[ii] = (short) oldim->im.float_data[ii] ;
         break ;

      case MRI_double:
         for( ii=0 ; ii < npix ; ii++ )
            newim->im.short_data[ii] = scale * oldim->im.double_data[ii] ;
         break ;

      case MRI_complex:
        for( ii=0 ; ii < npix ; ii++ )
           newim->im.short_data[ii] = scale * CABS(oldim->im.complex_data[ii]) ;
        break ;

      default:
         fprintf( stderr , "mri_to_short:  unrecognized image kind\n" ) ;
         MRI_FATAL_ERROR ;
   }

   MRI_COPY_AUX(newim,oldim) ;
   return newim ;
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
   register float  scale  , flbot ;
   register short * ar ;

WHOAMI ; IMHEADER(oldim) ;

   newim = mri_new_conforming( oldim , MRI_short ) ;
   npix  = oldim->nvox ;

   if( scl == 0 ){  /* compute scaling to make [min..max] -> [0..lev] */

      imin = (oldim->kind == MRI_complex) ? (0) : mri_min( oldim ) ;
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

      case MRI_byte:{
	 register byte * oar = mri_data_pointer(oldim) ;
         for( ii=0 ; ii < npix ; ii++ )
            ar[ii] = scale * (oar[ii]-flbot) ;
         break ;
      }

      case MRI_short:{
	 register short * oar = mri_data_pointer(oldim) ;
         for( ii=0 ; ii < npix ; ii++ )
            ar[ii] = scale * (oar[ii]-flbot) ;
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
   return newim ;
}
