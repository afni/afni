#include <mrilib.h>
#include <string.h>

#ifndef MAX
# define MAX(x,y) (((x)>(y))?(x):(y))
#endif

/*---------------------------------------------------------------------------*/

int main( int argc , char * argv[] )
{
   int narg , narg_ave,narg_sig ;
   MRI_IMAGE * imin , ** imstat ;
   int to_short = -1 ;

/*** process command switches ***/

   if( argc < 5 || strncmp(argv[1],"-help",2) == 0 ){
      printf( "Usage: imaver out_ave out_sig input_images ...\n"
              "       (use - to skip output of out_ave and/or out_sig)\n"
              "* Computes the mean and standard deviation, pixel-by-pixel,\n"
              "   of a whole bunch of images.\n"
              "* Write output images in 'short int' format if inputs are\n"
              "   short ints, otherwise output images are floating point.\n"
            ) ;
      exit(0) ;
   }

   narg = 1 ;

/*** process input files ***/

   narg_ave = narg++ ;
   narg_sig = narg++ ;

   do{
      imin = mri_read_just_one( argv[narg] ) ;
      if( imin == NULL ) continue ;   /* skip this image! */

      if( to_short == -1 ) to_short = (imin->kind == MRI_short) ;

      (void) mri_stat_seq( imin ) ;
      mri_free( imin ) ;
   } while( ++narg < argc ) ;

/*** compute output ***/

   imstat = mri_stat_seq( NULL ) ;

   if( strcmp(argv[narg_ave],"-") != 0 ){
      if( to_short ) imin = mri_to_short( 1.0 , imstat[0] ) ;
      else           imin = imstat[0] ;
      
      mri_write( argv[narg_ave] , imin ) ;
      mri_free( imin ) ;
   }

   if( strcmp(argv[narg_sig],"-") != 0 ){
      if( to_short ) imin = mri_to_short( 1.0 , imstat[1] ) ;
      else           imin = imstat[1] ;
      
      mri_write( argv[narg_sig] , imin ) ;
      mri_free( imin ) ;
   }

   exit(0) ;
}
