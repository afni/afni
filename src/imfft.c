#include <string.h>
#include "mrilib.h"

/*****************************************************************************
  This software is copyrighted and owned by the Medical College of Wisconsin.
  See the file README.Copyright for details.
******************************************************************************/


#define IMMAX 1024
#define NFMAX 512   /* half of IMMAX */

int main( int argc , char *argv[] )
{
   char prefix[64] = "fft." , fname[128] ;
   int  narg , ii , nx,ny,npix , nimage,nfreq , kim ;

   MRI_IMARR *inimar , *outimar ;

   MRI_IMAGE *tempim , *inim , *outim ;
   float     **inar , **outar , *taper ;
   complex   *dat ;
   float     sum , scale , pbot,ptop ;
   short     *tempar ;
   int       ldecibel = FALSE ;

   /*** deal with command line arguments ***/

   if( argc < 3 || strncmp(argv[1],"-help",5) == 0 ){
      printf( "Computation of |FFT| of image time series.\n"
              "Copyright 1994-6 Medical College of Wisconsin.\n\n"
              "Usage: imfft [-prefix prefix] image_files\n" ) ;
      exit(0) ;
   }

   narg = 1 ;
   kim  = 0 ;

   /*** switches ***/

   while( argv[narg][0] == '-' ){

      if( strncmp(argv[narg],"-prefix",3) == 0 ){
         strncpy( prefix , argv[++narg] , 64 ) ;
         ii = strlen(prefix) ;
         if( prefix[ii] != '.' ){ prefix[ii+1] = '.' ; prefix[ii+2] = '\0' ; }
         narg++ ; continue ;
      }

      if( strncmp(argv[narg],"-",1) == 0 ){
         fprintf( stderr , "*** illegal switch: %s\a\n" , argv[narg] ) ;
         exit(1) ;
      }
   }

   /** read and initialize images **/

   inimar = mri_read_many_files( argc-narg , argv+narg ) ;
   if( inimar == NULL ){
      fprintf(stderr,"*** no input images read!\a\n") ;
      exit(1) ;
   } else if( inimar->num < 32 ){
      fprintf(stderr,"*** less than 32 input images read!\a\n") ;
      exit(1) ;
   }

   nimage = inimar->num ;
   for( kim=0 ; kim < nimage; kim++ ){

      inim = IMAGE_IN_IMARR(inimar,kim) ;

      if( ! MRI_IS_2D(inim) ){
         fprintf(stderr,"*** can only process 2D images!\a\n") ;
         exit(1) ;
      }

      if( kim == 0 ){                        /* check size for compatibility */
         nx = inim->nx ;
         ny = inim->ny ;
      } else if( inim->nx != nx || inim->ny != ny ){
         fprintf( stderr ,
                  "image %d size doesn't match 1st image!\n" , kim+1 ) ;
         exit(1) ;
      }

      if( inim->kind != MRI_float ){  /* convert to floating point */
         tempim = mri_to_float( inim ) ;
         mri_free( inim ) ;
         IMAGE_IN_IMARR(inimar,kim) = inim = tempim ;
      }
   }

   /*** cut image count back to power of 2 ***/

   for( ii=2 ; ii <= nimage ; ii *= 2 ) ; /* null loop */
   kim    = nimage ;
   nimage = ii / 2 ;
   nfreq  = nimage/2 - 1 ;
   if( nimage < kim ){
      fprintf( stderr , "cutting image count back to %d from %d\n" ,
               nimage , kim ) ;
      for( ii=nimage ; ii < kim ; ii++ ){
         mri_free( IMAGE_IN_IMARR(inimar,ii) ) ;
         IMAGE_IN_IMARR(inimar,ii) = NULL ;
      }
   }

   /*** load array of pointers to data arrays,
        create array of output images ,
        load array of pointers to output arrays ,
        create taper array                        ***/

   inar  = (float **)  malloc( sizeof(float *) * nimage ) ;
   outar = (float **)  malloc( sizeof(float *) * nfreq  ) ;
   dat   = (complex *) malloc( sizeof(complex) * nimage ) ;

   if( inar==NULL || outar==NULL || dat==NULL ){
      fprintf(stderr,"*** cannot malloc workspace for FFT!\a\n") ;
      exit(1) ;
   }

   for( kim=0 ; kim < nimage ; kim++ )
      inar[kim] = MRI_FLOAT_PTR( IMAGE_IN_IMARR(inimar,kim) ) ;

   INIT_IMARR(outimar) ;
   for( kim=0 ; kim < nfreq ; kim++ ){
      outim      = mri_new( nx , ny , MRI_float ) ;
      outar[kim] = MRI_FLOAT_PTR( outim ) ;
      ADDTO_IMARR(outimar,outim) ;
   }

   taper = mri_setup_taper( nimage , 0.20 ) ;

   /*** foreach time series:
           remove mean, taper, FFT, square, store in output ***/

   npix = nx * ny ;
   for( ii=0 ; ii < npix ; ii++ ){
      sum = 0.0 ;
      for( kim=0 ; kim < nimage ; kim++ ) sum += inar[kim][ii] ;
      sum /= nimage ;

      for( kim=0 ; kim < nimage ; kim++ ){
         dat[kim].r = (inar[kim][ii] - sum) * taper[kim] ;
         dat[kim].i = 0.0 ;
      }
      csfft_cox( -1 , nimage , dat ) ;

      for( kim=0 ; kim < nfreq ; kim++ )
         outar[kim][ii] = CABS(dat[kim+1]) ;

#if 0
      if( ldecibel ){
         for( kim=0 ; kim < nfreq ; kim++ )
            outar[kim][ii] = 10.0 * log10( outar[kim][ii] + 1.e-30 ) ;
      }
#endif
   }

   /*** toss input ***/

   DESTROY_IMARR( inimar ) ;
   free( taper ) ;

   /*** scale to shorts and write to disk ***/

   ptop = pbot = outar[0][0] ;
   for( kim=0 ; kim < nfreq ; kim++ ){
      sum = mri_max( IMAGE_IN_IMARR(outimar,kim) ) ; if( sum > ptop ) ptop = sum ;
      sum = mri_min( IMAGE_IN_IMARR(outimar,kim) ) ; if( sum < pbot ) pbot = sum ;
   }

   fprintf( stderr , "\n minimum = %g  maximum = %g\n" , pbot,ptop ) ;

#if 0
   if( ldecibel ){
      pbot = ptop - 50.0 ;  /* 50 dB range */
   } else {
      pbot = 0.0 ;
   }
#else
   pbot = 0.0 ;
#endif

   scale  = 30000.0 / (ptop-pbot) ;
   tempim = mri_new( nx , ny , MRI_short ) ;
   tempar = mri_data_pointer( tempim ) ;

   for( kim=0 ; kim < nfreq ; kim++ ){

      for( ii=0 ; ii < npix ; ii++ ){
         tempar[ii] = (outar[kim][ii] < pbot)
		      ? 0
		      : (short)(scale * (outar[kim][ii] - pbot) + 0.499) ;
      }

      mri_free( IMAGE_IN_IMARR(outimar,kim) ) ;

      sprintf( fname , "%s%04d" , prefix , kim ) ;
      mri_write( fname , tempim ) ;

   }

   exit(0) ;
}
