#include <stdlib.h>
#include <string.h>
#include "mrilib.h"

#define ABS(x)  ( ((x)>=0) ? (x) : (-(x)) )

int main( int argc , char * argv[] )
{
   int iarg , pos = 0 ;
   float thresh=0.0 ;
   MRI_IMAGE * maskim=NULL , *imin , *imout ;
   float * maskar ;
   int nxim , nyim , ii , npix ;

   if( argc < 3 || strncmp(argv[1],"-help",4) == 0 ){
      printf("Usage: immask [-thresh #] [-mask mask_image] [-pos] input_image output_image\n"
             "* Masks the input_image and produces the output_image;\n"
             "* Use of -thresh # means all pixels with absolute value below # in\n"
             "   input_image will be set to zero in the output_image\n"
             "* Use of -mask mask_image means that only locations that are nonzero\n"
             "   in the mask_image will be nonzero in the output_image\n"
             "* Use of -pos means only positive pixels from input_image will be used\n"
             "* At least one of -thresh, -mask, -pos must be used; more than one is OK.\n"
            ) ;
     exit(0) ;
   }

   iarg = 1 ;
   while( iarg < argc && argv[iarg][0] == '-' ){

      /*** -pos ***/

      if( strncmp(argv[iarg],"-pos",4) == 0 ){
         pos = 1 ;
         iarg++ ; continue ;
      }

      /*** -thresh # ***/

      if( strncmp(argv[iarg],"-thresh",5) == 0 ){
         thresh = strtod( argv[++iarg] , NULL ) ;
         if( iarg >= argc || thresh <= 0.0 ){
            fprintf(stderr,"Illegal -thresh!\a\n") ; exit(1) ;
         }
         iarg++ ; continue ;
      }

      if( strncmp(argv[iarg],"-mask",5) == 0 ){
         maskim = mri_read_just_one( argv[++iarg] ) ;
         if( maskim == NULL || iarg >= argc || ! MRI_IS_2D(maskim) ){
            fprintf(stderr,"Illegal -mask!\a\n") ; exit(1) ;
         }
         if( maskim->kind != MRI_float ){
            imin = mri_to_float( maskim ) ;
            mri_free( maskim ) ;
            maskim = imin ;
         }
         iarg++ ; continue ;
      }

      fprintf(stderr,"** Illegal option: %s\a\n",argv[iarg]) ;
      exit(1) ;
   }
   if( thresh <= 0.0 && maskim == NULL && pos == 0 ){
      fprintf(stderr,"No -thresh, -mask, -pos ==> can't go on!\a\n") ; exit(1) ;
   }
   if( iarg+1 >= argc ){
      fprintf(stderr,"Must have input_image and output_image on command line!\a\n") ;
      exit(1) ;
   }

   imin = mri_read_just_one( argv[iarg++] ) ;
   if( imin == NULL ) exit(1) ;
   if( ! MRI_IS_2D(imin) ){
      fprintf(stderr,"can only process 2D images!\a\n") ;
      exit(1) ;
   }

   nxim = imin->nx ;
   nyim = imin->ny ;
   npix = nxim * nyim ;

   if( maskim == NULL ){
      maskim = mri_new( nxim , nyim , MRI_float ) ;
      maskar = MRI_FLOAT_PTR(maskim) ;
      for( ii=0 ; ii < npix ; ii++ ) maskar[ii] = 1.0 ;
   } else if( maskim->nx != nxim || maskim->ny != nyim ){
      fprintf(stderr,"Mask and input image not same size!\a\n") ;
      exit(1) ;
   } else {
      maskar = MRI_FLOAT_PTR(maskim) ;
   }
   imout = mri_new( nxim , nyim , imin->kind ) ;

   switch( imin->kind ){

      default:
         fprintf(stderr,"Unrecognized input image type!\a\n") ;
         exit(1) ;

      case MRI_byte:{
         byte * arin , * arout , val ;
         arin  = mri_data_pointer(imin) ;
         arout = mri_data_pointer(imout) ;
         for( ii=0 ; ii < npix ; ii++ ){
            val = arin[ii] ;
            if( maskar[ii] != 0.0 && ABS(val) >= thresh ) arout[ii] = val ;
            else                                          arout[ii] = 0 ;
         }
      } break ;

      case MRI_short:{
         short * arin , * arout , val ;
         arin  = mri_data_pointer(imin) ;
         arout = mri_data_pointer(imout) ;
         for( ii=0 ; ii < npix ; ii++ ){
            val = arin[ii] ;
            if( maskar[ii] != 0.0 && ABS(val) >= thresh ) arout[ii] = val ;
            else                                          arout[ii] = 0 ;
         }
         if( pos ) for( ii=0 ; ii < npix ; ii++ ) if( arout[ii] < 0 ) arout[ii] = 0 ;
      } break ;

      case MRI_float:{
         float * arin , * arout , val ;
         arin  = mri_data_pointer(imin) ;
         arout = mri_data_pointer(imout) ;
         for( ii=0 ; ii < npix ; ii++ ){
            val = arin[ii] ;
            if( maskar[ii] != 0.0 && ABS(val) >= thresh ) arout[ii] = val ;
            else                                          arout[ii] = 0 ;
         }
         if( pos ) for( ii=0 ; ii < npix ; ii++ ) if( arout[ii] < 0 ) arout[ii] = 0 ;
      } break ;

      case MRI_int:{
         int * arin , * arout , val ;
         arin  = mri_data_pointer(imin) ;
         arout = mri_data_pointer(imout) ;
         for( ii=0 ; ii < npix ; ii++ ){
            val = arin[ii] ;
            if( maskar[ii] != 0.0 && ABS(val) >= thresh ) arout[ii] = val ;
            else                                          arout[ii] = 0 ;
         }
         if( pos ) for( ii=0 ; ii < npix ; ii++ ) if( arout[ii] < 0 ) arout[ii] = 0 ;
      } break ;

      case MRI_double:{
         double * arin , * arout , val ;
         arin  = mri_data_pointer(imin) ;
         arout = mri_data_pointer(imout) ;
         for( ii=0 ; ii < npix ; ii++ ){
            val = arin[ii] ;
            if( maskar[ii] != 0.0 && ABS(val) >= thresh ) arout[ii] = val ;
            else                                          arout[ii] = 0 ;
         }
         if( pos ) for( ii=0 ; ii < npix ; ii++ ) if( arout[ii] < 0 ) arout[ii] = 0 ;
      } break ;

      case MRI_complex:{
         complex * arin , * arout , val ;
         arin  = mri_data_pointer(imin) ;
         arout = mri_data_pointer(imout) ;
         for( ii=0 ; ii < npix ; ii++ ){
            val = arin[ii] ;
            if( maskar[ii] != 0.0 && CABS(val) >= thresh ) arout[ii] = val ;
            else                                           arout[ii] = CMPLX(0,0) ;
         }
      } break ;
   }

   mri_write( argv[iarg] , imout ) ;
   exit(0) ;
}
