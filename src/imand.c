/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/
   
#include "mrilib.h"
#include <string.h>

#define ABS(x)  ( ((x)>=0) ? (x) : (-(x)) )

int main( int argc , char * argv[] )
{
   MRI_IMAGE *imin , *imout ;
   short *shin , *shout ;
   int nopt=1 , nxim , nyim , ii , kindim , npix , thresh=0 , nout ;

   if( argc < 4 || strncmp(argv[1],"-help",4) == 0 ){
      fprintf(stderr,"Usage: imand [-thresh #] input_images ... output_image\n"
                     "* Only pixels nonzero in all input images\n"
                     "* (and above the threshold, if given) will be output.\n" ) ;
      exit(0) ;
   }

   if( strncmp(argv[1],"-thresh",4) == 0 ){
      thresh = strtol( argv[2] , NULL , 0 ) ;
      thresh = ABS(thresh) ;
      nopt = 3 ;
      if( argc < 5 ){fprintf(stderr,"not enough files!\n");exit(1);}
   }

   imout = mri_read_just_one( argv[nopt++] ) ;
   if( imout == NULL ) exit(1) ;
   nxim   = imout->nx ;
   nyim   = imout->ny ;
   kindim = imout->kind ;
   npix   = nxim * nyim ;
   if( kindim != MRI_short || !MRI_IS_2D(imout) ){
      fprintf(stderr,"imand currently only works for 2D short images!\n") ;
      exit(1) ;
   }
   shout = mri_data_pointer( imout ) ;
   for( ii=0 ; ii < npix ; ii++ )
      if( ABS(shout[ii]) <= thresh ) shout[ii] = 0 ;

   while( nopt < argc-1 ){

      imin = mri_read_just_one( argv[nopt] ) ;
      if( imin == NULL ) exit(1) ;
      if( imin->nx!=nxim || imin->ny!=nyim || imin->kind!=kindim || !MRI_IS_2D(imin) ){
         fprintf(stderr,
                 "image %s doesn't conform to first image!\n",argv[nopt]) ;
         exit(1) ;
      }
      shin = mri_data_pointer( imin ) ;

      for( ii=0 ; ii < npix ; ii++ ){
         if( ABS(shin[ii]) <= thresh ){
            shout[ii] = 0 ;
          } else if( ((shout[ii] >  thresh) && (shin[ii] > shout[ii])) ||
                     ((shout[ii] < -thresh) && (shin[ii] < shout[ii]))   ){
            shout[ii] = shin[ii] ;
          }
      }

      mri_free( imin ) ;
      nopt++ ;

   } /* end of while loop */

   nout = 0 ;
   for( ii=0 ; ii < npix ; ii++ ) if( shout[ii] != 0 ) nout++ ;

   mri_write( argv[argc-1] , imout ) ;

   printf("number of nonzero pixels output = %d\n",nout) ;
   exit(0) ;
}
