/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/
   
#include "mrilib.h"
#include <string.h>

int main( int argc , char *argv[] )
{
   MRI_IMAGE *imin , *imout ;
   int nx,ny , ii,jj , nopt ;
   float * far , f00,fp0,f0p,fm0,f0m , top,bot,dif , fmax ;
   byte  * bar ;

#define BAR(i,j) bar[(i)+(j)*nx]
#define FAR(i,j) far[(i)+(j)*nx]

   if( argc < 3 || strncmp(argv[1],"-help",5) == 0 ){
      printf( "Usage: impdif image_in image_out\n"
              "Computes the NN difference percentage image\n" ) ;
      exit(0) ;
   }

   machdep() ;

   /** get parameters **/

   nopt = 1 ;
   imin = mri_read_just_one( argv[nopt++] ) ;
   if( imin == NULL ) exit(1) ;
   if( ! MRI_IS_2D(imin) ){fprintf(stderr,"** Input image is not 2D!\a\n"); exit(1);}

   if( imin->kind != MRI_float ){
      imout = mri_to_float(imin);  mri_free(imin) ; imin = imout ;
   }

   nx = imin->nx ; ny = imin->ny ;
   imout = mri_new( nx , ny , MRI_byte ) ;
   bar = MRI_BYTE_PTR(imout) ; far = MRI_FLOAT_PTR(imin) ;
   fmax = mri_max(imin) ;

   for( jj=0 ; jj < ny ; jj++ ){
      for( ii=0 ; ii < nx ; ii++ ){
         f00 = FAR( ii           , jj           ) ;
         fp0 = FAR( (ii+1)%nx    , jj           ) ;
         f0p = FAR( ii           , (jj+1)%ny    ) ;
         fm0 = FAR( (ii+nx-1)%nx , jj           ) ;
         f0m = FAR( ii           , (jj+ny-1)%ny ) ;

         dif = fabs(f00-fp0) ; top = dif ;
         dif = fabs(f00-f0p) ; top = MAX(top,dif) ;
         dif = fabs(f00-f0m) ; top = MAX(top,dif) ;
         dif = fabs(f00-fm0) ; top = MAX(top,dif) ;

         dif = fabs(f00) ; bot = dif ;
         dif = fabs(f0p) ; bot = MAX(bot,dif) ;
         dif = fabs(fp0) ; bot = MAX(bot,dif) ;
         dif = fabs(f0m) ; bot = MAX(bot,dif) ;
         dif = fabs(fm0) ; bot = MAX(bot,dif) ;

         if( bot > 0.05*fmax ) BAR(ii,jj) = (byte) (100.0 * top/bot) ;
         else                  BAR(ii,jj) = 0 ;
      }
   }

   mri_write( argv[nopt++] , imout ) ;
   exit(0) ;
}
