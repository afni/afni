#include <stdlib.h>
#include <string.h>
#include "mrilib.h"

int main( int argc , char * argv[] )
{
   MRI_IMAGE *imin ;
   float * maskar ;
   int nxim , nyim , ii , npix ;

   if( argc < 2 || strncmp(argv[1],"-help",4) == 0 ){
     printf("Usage: imdump input_image\n"
            "* Prints out nonzero pixels in an image;\n"
            "* Results to stdout; redirect (with >) to save to a file;\n"
            "* Format: x-index y-index value, one pixel per line.\n" ) ;
     exit(0) ;
   }

   imin = mri_read_just_one( argv[1] ) ;
   if( imin == NULL ) exit(1) ;
   if( ! MRI_IS_2D(imin) ){
      fprintf(stderr,"can only deal with 2D images!\n") ;
      exit(1) ;
   }

   nxim = imin->nx ;
   nyim = imin->ny ;
   npix = nxim * nyim ;

   switch( imin->kind ){

      default:
         fprintf(stderr,"Unrecognized input image type!\a\n") ;
         exit(1) ;

      case MRI_byte:{
         int ix,jy ;
         byte * arin , val ;
         arin  = mri_data_pointer(imin) ;
         for( jy=0 ; jy < nyim ; jy++ ){
            for( ix=0 ; ix < nxim ; ix++ ){
              val = arin[ix+jy*nxim] ;
              if( val != 0 ) printf("%4d %4d %4d\n",ix,jy,val ) ;
         }  }
      } break ;

      case MRI_short:{
         int ix,jy ;
         short * arin , val ;
         arin  = mri_data_pointer(imin) ;
         for( jy=0 ; jy < nyim ; jy++ ){
            for( ix=0 ; ix < nxim ; ix++ ){
              val = arin[ix+jy*nxim] ;
              if( val != 0 ) printf("%4d %4d %4d\n",ix,jy,val ) ;
         }  }
      } break ;

      case MRI_float:{
         int ix,jy ;
         float * arin , val ;
         arin  = mri_data_pointer(imin) ;
         for( jy=0 ; jy < nyim ; jy++ ){
            for( ix=0 ; ix < nxim ; ix++ ){
              val = arin[ix+jy*nxim] ;
              if( val != 0 ) printf("%4d %4d %g\n",ix,jy,val ) ;
         }  }
      } break ;

      case MRI_int:{
         int ix,jy ;
         int * arin , val ;
         arin  = mri_data_pointer(imin) ;
         for( jy=0 ; jy < nyim ; jy++ ){
            for( ix=0 ; ix < nxim ; ix++ ){
              val = arin[ix+jy*nxim] ;
              if( val != 0 ) printf("%4d %4d %4d\n",ix,jy,val ) ;
         }  }
      } break ;

      case MRI_double:{
         int ix,jy ;
         double * arin , val ;
         arin  = mri_data_pointer(imin) ;
         for( jy=0 ; jy < nyim ; jy++ ){
            for( ix=0 ; ix < nxim ; ix++ ){
              val = arin[ix+jy*nxim] ;
              if( val != 0 ) printf("%4d %4d %g\n",ix,jy,val ) ;
         }  }
      } break ;

      case MRI_complex:{
         int ix,jy ;
         complex * arin , val ;
         arin  = mri_data_pointer(imin) ;
         for( jy=0 ; jy < nyim ; jy++ ){
            for( ix=0 ; ix < nxim ; ix++ ){
              val = arin[ix+jy*nxim] ;
              if( val.r != 0 || val.i != 0 )
                 printf("%4d %4d %g %g\n",ix,jy,val.r,val.i ) ;
         }  }
      } break ;
   }

   exit(0) ;
}
