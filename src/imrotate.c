#include "mrilib.h"
#include <string.h>

int main( int argc , char *argv[] )
{
   MRI_IMAGE *imin , *imwarp ;
   float aa , bb , phi ;
   char * cp ;
   int kk , nopt , use_linear = 0 ;

   if( argc < 6 || strncmp(argv[1],"-help",5) == 0 ){
      printf( "Usage: imrotate [-linear] dx dy phi input_image output_image\n"
              "Shifts and rotates an image:\n"
              "  dx pixels rightwards (not necessarily an integer)\n"
              "  dy pixels downwards\n"
              "  phi degrees clockwise\n"
              "  -linear means to use bilinear interpolation (default is bicubic)\n"
              "Values outside the input_image are taken to be zero.\n" ) ;
      exit(0) ;
   }

   /** get parameters **/

#define ERR fprintf(stderr,"Illegal parameter!\n")

   nopt = 1 ;
   if( strncmp(argv[nopt],"-linear",4) == 0 ){
      use_linear = 1 ;
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

   if( use_linear )
      imwarp = mri_rota_bilinear( imin , aa,bb,phi ) ;
   else
      imwarp = mri_rota( imin , aa,bb,phi ) ;

   mri_free( imin ) ;

   switch( kk ){
      case MRI_byte:    imin = mri_to_byte( imwarp )        ; break ;
      case MRI_short:   imin = mri_to_short( 1.0 , imwarp ) ; break ;
      default:          imin = mri_to_float( imwarp )       ; break ;
      case MRI_float:   imin = imwarp                       ; break ;
      case MRI_complex: imin = imwarp                       ; break ;
   }

   mri_write( argv[nopt++] , imin ) ;
   exit(0) ;
}
