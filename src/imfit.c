/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/
   
#include <string.h>
#include "mrilib.h"

#define DFILT_SIGMA  (4.0*0.42466090)  /* 4.0 = FWHM */
#define MAX_ITER 5
#define THRESH   0.10

int main( int argc , char * argv[] )
{
   MRI_IMAGE * im1 , * im2 , *bim,*xim,*yim,*tim , *bim2 ;
   MRI_IMARR * fitim ;
   float * fit , *tar,*xar,*yar , *fit2 ;
   int nx,ny , ii,jj , joff , iter , good ;
   float hnx,hny,fac ;

   if( argc < 3 || strncmp(argv[1],"-help",4) == 0 ){
      printf("Usage: imfit image1 image2 [output_file]\n"
             " Tries to find shift and rotation parameters to fit\n"
             " image2 to image1.\n"
             " N.B.: the method used is valid only for 1-2 pixel\n"
             "       shifts and 1-2 degree rotations.\n"
             " If 'output_file' is given, image2 will be transformed\n"
             " accordingly and written into this file.\n"
            ) ;
      exit(0) ;
   }

   im1 = mri_read_just_one( argv[1] ) ;
   im2 = mri_read_just_one( argv[2] ) ;
   if( im1 == NULL || im2 == NULL ) exit(1) ;

   if( im1->nx != im2->nx || im1->ny != im2->ny ){
      fprintf(stderr,"*** Input image mismatch: fatal error!\a\n");
      exit(1);
   }
   if( ! MRI_IS_2D(im1) || ! MRI_IS_2D(im2) ){
      fprintf(stderr,"*** Input images are not 2D!\a\n") ;
      exit(1) ;
   }

   im1->dx = im1->dy = 1.0 ;
   nx      = im1->nx ;  hnx = 0.5 * nx ;
   ny      = im1->ny ;  hny = 0.5 * ny ;
   fac     = (PI/180.0) ;

   bim = mri_filt_fft( im1 , DFILT_SIGMA , 0 , 0 , 0 ) ;  /* smooth */
   xim = mri_filt_fft( im1 , DFILT_SIGMA , 1 , 0 , 0 ) ;  /* smooth and d/dx */
   yim = mri_filt_fft( im1 , DFILT_SIGMA , 0 , 1 , 0 ) ;  /* smooth and d/dy */

   tim = mri_new( nx , ny , MRI_float ) ;    /* x * d/dy - y * d/dx */
   tar = mri_data_pointer( tim ) ;           /* which is d/d(theta) */
   xar = mri_data_pointer( xim ) ;
   yar = mri_data_pointer( yim ) ;
   for( jj=0 ; jj < ny ; jj++ ){
      joff = jj * nx ;
      for( ii=0 ; ii < nx ; ii++ ){
         tar[ii+joff] = fac * (  (ii-hnx) * yar[ii+joff]
                               - (jj-hny) * xar[ii+joff] ) ;
      }
   }
   INIT_IMARR ( fitim ) ;
   ADDTO_IMARR( fitim , bim ) ;
   ADDTO_IMARR( fitim , xim ) ;
   ADDTO_IMARR( fitim , yim ) ;
   ADDTO_IMARR( fitim , tim ) ;


   bim2 = mri_filt_fft( im2 , DFILT_SIGMA , 0 , 0 , 0 ) ;  /* smooth input image */
   fit  = mri_lsqfit( bim2 , fitim , bim2 ) ;              /* fit input image to ref images */
   mri_free( bim2 ) ;

#if 0
   printf("Command that transforms second image to first would be\n"
          "  imrotate %g %g %g %s ...\n" ,
          fit[1] , fit[2] , fit[3] , argv[2] ) ;
#endif

   iter = 0 ;
   do {

      tim = mri_rota( im2 , fit[1] , fit[2] , fit[3]*(PI/180.0) ) ;
      bim2 = mri_filt_fft( tim , DFILT_SIGMA , 0 , 0 , 0 ) ;  /* smooth input image */
      fit2 = mri_lsqfit( bim2 , fitim , bim2 ) ;              /* fit input image to ref images */
      mri_free( bim2 ) ; mri_free( tim ) ;

      fit[1] += fit2[1] ;
      fit[2] += fit2[2] ;
      fit[3] += fit2[3] ;

#if 0
      printf("Command that transforms second image to first would be\n"
             "  imrotate %g %g %g %s ...\n" ,
             fit[1] , fit[2] , fit[3] , argv[2] ) ;
#endif

      iter++ ;
      good = (iter < MAX_ITER) &&
               ( fabs(fit2[1]) > THRESH ||
                 fabs(fit2[2]) > THRESH || fabs(fit2[3]) > THRESH ) ;
   } while(good) ;

   if( argc < 4 ){
      printf("  Command that transforms second image to first would be\n"
             "    imrotate %g %g %g %s ...\n" ,
             fit[1] , fit[2] , fit[3] , argv[2] ) ;
   } else {
      tim = mri_rota( im2 , fit[1] , fit[2] , fit[3]*(PI/180.0) ) ;
      switch( im2->kind ){
         default: bim2 = tim ; break ;
         case MRI_short: bim2 = mri_to_short( 1.0 , tim ) ; break ;
         case MRI_byte:  bim2 = mri_to_byte( tim ) ;
      }
      mri_write( argv[3] , bim2 ) ;
      printf("  imrotate %g %g %g %s %s\n",
             fit[1] , fit[2] , fit[3] , argv[2] , argv[3] ) ;
   }

   exit(0) ;
}
