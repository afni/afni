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
   register double scale=1.0 ;
   short  shbot=0 ;
   int    inbot=0 ;
   float  flbot=0.0 ;
   double dbbot=0.0 ;
   byte *ar ;

ENTRY("mri_to_byte") ;

   if( oldim == NULL ) RETURN( NULL );  /* 09 Feb 1999 */

   newim = mri_new_conforming( oldim , MRI_byte ) ;
   npix  = oldim->nvox ;

   switch( oldim->kind ){
      default: break ;
      case MRI_short:
      case MRI_int:
      case MRI_float:
      case MRI_double:
         imin  = mri_min( oldim ) ;
         imax  = mri_max( oldim ) ;
         imax  = (imax <= imin) ? imin+1 : imax ;
         scale = 255.9 / (imax-imin)  ;
         switch( oldim->kind ){
            default:                          break ;
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
        (void) memcpy( ar , MRI_BYTE_PTR(oldim) , sizeof(byte)*npix ) ;
      break ;

      case MRI_rgb:{                           /* 13 Nov 2002 */
        byte *rgb = MRI_RGB_PTR(oldim) ;
        float rfac=0.299*scale , gfac=0.587*scale , bfac=0.114*scale , val ;
        for( ii=0 ; ii < npix ; ii++ ){
          val = rfac * rgb[3*ii] + gfac * rgb[3*ii+1] + bfac * rgb[3*ii+2] ;
          ar[ii] = BYTEIZE(val) ;
        }
      }
      break ;

      case MRI_short:{
         short *qar = MRI_SHORT_PTR(oldim) ;
         for( ii=0 ; ii < npix ; ii++ ) ar[ii] = scale * (qar[ii]-shbot) ;
      }
      break ;

      case MRI_int:{
         int *qar = MRI_INT_PTR(oldim) ;
         for( ii=0 ; ii < npix ; ii++ ) ar[ii] = scale * (qar[ii]-inbot) ;
      }
      break ;

      case MRI_float:{
         float *qar = MRI_FLOAT_PTR(oldim) ;
         for( ii=0 ; ii < npix ; ii++ ) ar[ii] = scale * (qar[ii]-flbot) ;
      }
      break ;

      case MRI_double:{
         double *qar = MRI_DOUBLE_PTR(oldim) ;
         for( ii=0 ; ii < npix ; ii++ ) ar[ii] = scale * (qar[ii]-dbbot) ;
      }
      break ;

      case MRI_complex:{
         complex *qar = MRI_COMPLEX_PTR(oldim) ;
         for( ii=0 ; ii < npix ; ii++ ) ar[ii] = scale * CABS(qar[ii]) ;
      }
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
   register float  scale  , flbot , val ;
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

      case MRI_byte:{
         byte *qar = MRI_BYTE_PTR(oldim) ;
         for( ii=0 ; ii < npix ; ii++ ){
            val = scale * (qar[ii]-flbot) ; ar[ii] = BYTEIZE(val) ;
         }
      }
      break ;

      case MRI_short:{
         short *qar = MRI_SHORT_PTR(oldim) ;
         for( ii=0 ; ii < npix ; ii++ ){
            val = scale * (qar[ii]-flbot) ; ar[ii] = BYTEIZE(val) ;
         }
      }
      break ;

      case MRI_int:{
         int *qar = MRI_INT_PTR(oldim) ;
         for( ii=0 ; ii < npix ; ii++ ){
            val = scale * (qar[ii]-flbot) ; ar[ii] = BYTEIZE(val) ;
         }
      }
      break ;

      case MRI_float:{
         float *qar = MRI_FLOAT_PTR(oldim) ;
         for( ii=0 ; ii < npix ; ii++ ){
            val = scale * (qar[ii]-flbot) ; ar[ii] = BYTEIZE(val) ;
         }
      }
      break ;

      case MRI_double:{
         double *qar = MRI_DOUBLE_PTR(oldim) ;
         for( ii=0 ; ii < npix ; ii++ ) ar[ii] = dscale * (qar[ii]-dbbot) ;
      }
      break ;

      case MRI_complex:{
         complex *qar = MRI_COMPLEX_PTR(oldim) ;
         for( ii=0 ; ii < npix ; ii++ ) ar[ii] = scale * CABS(qar[ii]) ;
      }
      break ;

      default:
         fprintf( stderr , "mri_to_byte_scl:  unrecognized image kind\n" ) ;
         MRI_FATAL_ERROR ;
   }

   MRI_COPY_AUX(newim,oldim) ;
   RETURN( newim );
}

/* compute a byte mask from the input image     11 Jan 2008 [rickr]
 * (this is akin to THD_makemask)                                   */
byte * mri_to_bytemask( MRI_IMAGE * maskim, float mask_bot, float mask_top )
{
   float   maxval;
   byte  * bmask;
   int     nvox, ii;
   int     empty = 0;

ENTRY("mri_to_bytemask") ;

   if( maskim == NULL ) RETURN( NULL );  /* 09 Feb 1999 */

   nvox  = maskim->nvox ;
   bmask = (byte *)calloc( sizeof(byte) * nvox, 1 );
   if( !bmask ) {
        fprintf(stderr,"** failed to alloc %d bytes for mask\n", nvox);
        RETURN(NULL);
   }

   switch( maskim->kind ) {
      default:
         fprintf(stderr,"** mri_to_bytemask: invalid kind %d\n", maskim->kind);
         free(bmask);  RETURN(NULL);

      case MRI_byte: {
         byte mbot, mtop;
         byte *qar = MRI_BYTE_PTR(maskim) ;
         if( mask_bot <= mask_top ) {
            maxval = MRI_TYPE_maxval[MRI_byte] + 0.5;
            if( mask_bot >= maxval || mask_top <= -maxval ) empty = 1;
            mbot = BYTEIZE(mask_bot); mtop = BYTEIZE(mask_top);
         } else {
            mbot = (byte) -MRI_TYPE_maxval[MRI_byte];
            mtop = (byte)  MRI_TYPE_maxval[MRI_byte];
         }
         if( !empty )
            for( ii=0 ; ii < nvox ; ii++ )
               if( qar[ii] >= mbot && qar[ii] <= mtop && qar[ii] != 0 )
                  bmask[ii] = 1;
      }
      break ;

      case MRI_short: {
         short mbot, mtop;
         short *qar = MRI_SHORT_PTR(maskim) ;
         if( mask_bot <= mask_top ) {
            maxval = MRI_TYPE_maxval[MRI_short] + 0.5;
            if( mask_bot >= maxval || mask_top <= -maxval ) empty = 1;
            mbot = SHORTIZE(mask_bot); mtop = SHORTIZE(mask_top);
         } else {
            mbot = (short) -MRI_TYPE_maxval[MRI_short];
            mtop = (short)  MRI_TYPE_maxval[MRI_short];
         }
         if( !empty )
            for( ii=0 ; ii < nvox ; ii++ )
               if( qar[ii] >= mbot && qar[ii] <= mtop && qar[ii] != 0 )
                  bmask[ii] = 1;
      }
      break ;

      case MRI_int: {
         int mbot, mtop;
         int *qar = MRI_INT_PTR(maskim) ;
         if( mask_bot <= mask_top ) {
            maxval = MRI_TYPE_maxval[MRI_int] + 0.5;
            if( mask_bot >= maxval || mask_top <= -maxval ) empty = 1;
            mbot = (int)mask_bot; mtop = (int)mask_top;
         } else {
            mbot = (int) -MRI_TYPE_maxval[MRI_int];
            mtop = (int)  MRI_TYPE_maxval[MRI_int];
         }
         if( !empty )
            for( ii=0 ; ii < nvox ; ii++ )
               if( qar[ii] >= mbot && qar[ii] <= mtop && qar[ii] != 0 )
                  bmask[ii] = 1;
      }
      break ;

      case MRI_float:{
         float *qar = MRI_FLOAT_PTR(maskim) ;
         if( mask_bot <= mask_top ) {   /* check bot/top */
            for( ii=0 ; ii < nvox ; ii++ )
               if( qar[ii]>=mask_bot && qar[ii]<=mask_top && qar[ii]!=0.0 )
                  bmask[ii] = 1;
         } else {                       /* no interval check */
            for( ii=0 ; ii < nvox ; ii++ )
               if( qar[ii] != 0.0 )
                  bmask[ii] = 1;
         }
      }
      break ;
   }

   RETURN(bmask);
}

