#include "mrilib.h"

int main( int argc , char * argv[] )
{
   int narg , ndset , nvox ;
   THD_3dim_dataset *xset , *yset , *mask_dset=NULL ;
   byte *mmm=NULL ;
   float met[3] ;

   /*-- read command line arguments --*/

   if( argc < 3 || strncmp(argv[1],"-help",5) == 0 ){
      printf("Usage: 3Hmetric [options] dset1 dset2\n"
             "Output = Histogram metrics between 2 dataset bricks\n"
             "Options:\n"
             "  -mask mset   Means to use the dataset 'mset' as a mask:\n"
             "                 Only voxels with nonzero values in 'mset'\n"
             "                 will be averaged from 'dataset'.\n"
            ) ;
      exit(0) ;
   }

   narg = 1 ;
   while( narg < argc && argv[narg][0] == '-' ){

     if( strncmp(argv[narg],"-mask",5) == 0 ){
       if( mask_dset != NULL ) ERROR_exit("Can't use -mask twice") ;
         if( narg+1 >= argc ) ERROR_exit("Need argument after -mask") ;
         mask_dset = THD_open_dataset( argv[++narg] ) ;
         if( mask_dset == NULL ) ERROR_exit("Can't open -mask %s",argv[narg]) ;
         narg++ ; continue ;
      }

      ERROR_exit("Unknown option: %s",argv[narg]) ;
   }

   /* should have at least 2 more arguments */

   ndset = argc - narg ;
   if( ndset <= 1 ) ERROR_exit("Need two input datasets") ;

   xset = THD_open_dataset( argv[narg++] ) ;
   yset = THD_open_dataset( argv[narg++] ) ;
   if( xset == NULL || yset == NULL )
     ERROR_exit("Cannot open both input datasets!\n") ;

   if( DSET_NVALS(xset) > 1 || DSET_NVALS(yset) > 1 )
     WARNING_message("Will only use sub-brick #0 of input datasets") ;

   nvox = DSET_NVOX(xset) ;
   if( nvox != DSET_NVOX(yset) )
     ERROR_exit("Input datasets grid dimensions don't match!\n") ;

   /* make a byte mask from mask dataset */

   if( mask_dset != NULL ){
     int mcount ;
     if( DSET_NVOX(mask_dset) != nvox )
       ERROR_exit("Input and mask datasets are not same dimensions!\n");
     DSET_load(mask_dset) ;
     if( !DSET_LOADED(mask_dset) ) ERROR_exit("Can't load mask dataset") ;
     mmm = THD_makemask( mask_dset , 0 , 1.0f,-1.0f ) ;
     mcount = THD_countmask( nvox , mmm ) ;
     INFO_message("Have %d voxels in the mask\n",mcount) ;
     if( mcount <= 666 ) ERROR_exit("Mask is too small") ;
     DSET_delete(mask_dset) ;
   }

   DSET_load(xset) ;
   if( !DSET_LOADED(xset) ) ERROR_exit("Can't load 1st dataset") ;
   DSET_load(yset) ;
   if( !DSET_LOADED(yset) ) ERROR_exit("Can't load 2nd dataset") ;

   mri_metrics( DSET_BRICK(xset,0) , DSET_BRICK(yset,0) , met ) ;

   INFO_message("Mutual Info = %f",met[METRIC_KULL]) ;
   INFO_message("Hellinger   = %f",met[METRIC_HELL]) ;
   INFO_message("Triangular  = %f",met[METRIC_TRIA]) ;
   exit(0) ;
}
