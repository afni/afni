#include "mrilib.h"
#include <string.h>

int main( int argc , char *argv[] )
{
   MRI_IMAGE *imin , *imwarp ;
   float aa , bb , phi ;
   char * cp ;
   int kk , nopt , almode = MRI_BICUBIC ;
   double ct ;
   int qq ;

   if( argc < 6 || strncmp(argv[1],"-help",5) == 0 ){
      printf( "Usage: imrotate [-linear | -Fourier] dx dy phi input_image output_image\n"
              "Shifts and rotates an image:\n"
              "  dx pixels rightwards (not necessarily an integer)\n"
              "  dy pixels downwards\n"
              "  phi degrees clockwise\n"
              "  -linear means to use bilinear interpolation (default is bicubic)\n"
              "  -Fourier means to use Fourier interpolaion\n"
              "Values outside the input_image are taken to be zero.\n" ) ;
      exit(0) ;
   }

   /** get parameters **/

#define ERR fprintf(stderr,"Illegal parameter!\n")

   nopt = 1 ;
   if( strncmp(argv[nopt],"-linear",4) == 0 ){
      almode = MRI_BILINEAR ;
      nopt++ ;
   } else if( strncmp(argv[nopt],"-Fourier",4) == 0 ){
      almode = MRI_FOURIER ;
      nopt++ ;
   }

   aa  = strtod( argv[nopt++] , &cp ) ;              if( *cp != '\0' ){ERR;exit(1);}
   bb  = strtod( argv[nopt++] , &cp ) ;              if( *cp != '\0' ){ERR;exit(1);}
   phi = strtod( argv[nopt++] , &cp ) * (PI/180.0) ; if( *cp != '\0' ){ERR;exit(1);}

   imin = mri_read_just_one( argv[nopt++] ) ;
   if( imin == NULL ) exit(1) ;
   if( ! MRI_IS_2D(imin) ){
      fprintf(stderr,"** Input image is not 2D!\a\n") ; exit(1) ;
   }

   kk = imin->kind ;
   if( kk != MRI_float ){
     MRI_IMAGE * qim = mri_to_float(imin) ;
     mri_free(imin) ; imin = qim ;
   }

   ct = COX_cpu_time() ;

   for( qq=0 ; qq < 100 ; qq++ ){
      imwarp = mri_rota_variable( almode , imin,aa,bb,phi ) ;
      mri_free(imwarp) ;
   }
   ct = COX_cpu_time() - ct ;
   printf("CPU time = %g\n",ct) ; exit(0) ;
   exit(0) ;
}
