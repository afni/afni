/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/
   
#include "mrilib.h"

int main( int argc , char *argv[] )
{
   MRI_IMAGE *im , *imb ;
   int narg ;
   float perc=0.0 ;

   if( argc < 2 || strncmp(argv[1],"-help",2) == 0 ){
      printf( "Converts an image to raw pgm format.\n") ;
      printf( "Results go to stdout and should be redirected.\n");
      printf( "Usage:   mritopgm [-pp] input_image\n" ) ;
      printf( "Example: mritopgm fred.001 | ppmtogif > fred.001.gif\n") ;
      printf( "\n"
              "  The '-pp' option expresses a clipping percentage.\n"
              "  That is, if this option is given, the pp%%-brightest\n"
              "  pixel is mapped to white; all above it are also white,\n"
              "  and all below are mapped linearly down to black.\n"
              "  The default is that pp=100; that is, the brightest\n"
              "  pixel is white.  A useful operation for many MR images is\n"
              "    mritopgm -99 fred.001 | ppmtogif > fred.001.gif\n"
              "  This will clip off the top 1%% of voxels, which are often\n"
              "  super-bright due to arterial inflow effects, etc.\n"
            ) ;
      exit(0) ;
   }

   machdep() ;

   narg = 1 ;
   if( argv[narg][0] == '-' ){
     perc = 0.01 * strtod(argv[narg]+1,NULL) ;
     if( perc < 0.01 || perc > 1.00 ){
       fprintf(stderr,"** Illegal percentage: %s\n",argv[narg]+1) ;
     }
     narg++ ;
   }
   im   = mri_read_just_one( argv[narg] ) ;
   if( im == NULL ) exit(1) ;

   if( perc > 0.0 ){
     float val = mri_quantile( im , perc ) ;
     int ii ;
     switch( im->kind ){
        case MRI_short:{
          short *ar = mri_data_pointer(im) ;
          for( ii=0 ; ii < im->nvox ; ii++ ) if( ar[ii] > val ) ar[ii] = val ;
        }
        break ;
        case MRI_byte:{
          byte *ar = mri_data_pointer(im) ;
          for( ii=0 ; ii < im->nvox ; ii++ ) if( ar[ii] > val ) ar[ii] = val ;
        }
        break ;
        case MRI_float:{
          float *ar = mri_data_pointer(im) ;
          for( ii=0 ; ii < im->nvox ; ii++ ) if( ar[ii] > val ) ar[ii] = val ;
        }
        break ;
        default:
          fprintf(stderr,"** Don't know how to percent clip this type of image!\n") ;
        break ;
     }
   }

   imb = mri_to_byte( im ) ;
   mri_free( im ) ;

   printf( "P5 %d %d 255\n" , imb->nx , imb->ny ) ;
   fwrite( imb->im.byte_data , imb->pixel_size , imb->nx * imb->ny , stdout ) ;

   exit(0) ;
}
