#include "mrilib.h"

int main( int argc , char * argv[] )
{
   MRI_IMAGE *xim , *yim ;
   float     *xar , *yar , *war=NULL ;
   int narg , ndset , nvox , ii ;
   THD_3dim_dataset *xset , *yset , * mask_dset=NULL ;
   float mask_bot=666.0 , mask_top=-666.0 , val ;
   byte *mmm=NULL ;

   /*-- read command line arguments --*/

   if( argc < 3 || strncmp(argv[1],"-help",5) == 0 ){
      printf("Usage: 3dAcost [options] dset1 dset2\n"
             "Output = 3dAllineate cost functions between 2 dataset bricks\n"
             "\n"
             "Options:\n"
             "  -mask mset   Means to use the dataset 'mset' as a mask:\n"
             "                 Only voxels with nonzero values in 'mset'\n"
             "                 will be averaged from 'dataset'.  Note\n"
             "                 that the mask dataset and the input dataset\n"
             "                 must have the same number of voxels.\n"
            ) ;
      exit(0) ;
   }

   narg = 1 ;
   while( narg < argc && argv[narg][0] == '-' ){

      if( strncmp(argv[narg],"-mask",5) == 0 ){
         if( mask_dset != NULL ){
            fprintf(stderr,"*** Cannot have two -mask options!\n") ; exit(1) ;
         }
         if( narg+1 >= argc ){
            fprintf(stderr,"*** -mask option requires a following argument!\n");
             exit(1) ;
         }
         mask_dset = THD_open_dataset( argv[++narg] ) ;
         if( mask_dset == NULL ){
            fprintf(stderr,"*** Cannot open mask dataset!\n") ; exit(1) ;
         }
         if( DSET_BRICK_TYPE(mask_dset,0) == MRI_complex ){
            fprintf(stderr,"*** Cannot deal with complex-valued mask dataset!\n");
            exit(1) ;
         }
         narg++ ; continue ;
      }

      fprintf(stderr,"*** Unknown option: %s\n",argv[narg]) ; exit(1) ;
   }

   /* should have at least 2 more arguments */

   ndset = argc - narg ;
   if( ndset <= 1 ){
      fprintf(stderr,"*** No input datasets!?\n") ; exit(1) ;
   }

   xset = THD_open_dataset( argv[narg++] ) ; DSET_load(xset) ;
   yset = THD_open_dataset( argv[narg++] ) ; DSET_load(yset) ;
   if( xset == NULL || yset == NULL )
     ERROR_exit("Can't open both input datasets!") ;
   if( !DSET_LOADED(xset) || !DSET_LOADED(yset) )
     ERROR_exit("Can't load input datasets") ;
   if( DSET_NVALS(xset) > 1 || DSET_NVALS(yset) > 1 )
     WARNING_message("Using only sub-brick [0] of input datasets") ;

   nvox = DSET_NVOX(xset) ;
   if( nvox != DSET_NVOX(yset) )
     ERROR_exit("Input datasets dimensions don't match!") ;

   xim = mri_scale_to_float( DSET_BRICK_FACTOR(xset,0) , DSET_BRICK(xset,0) );
   yim = mri_scale_to_float( DSET_BRICK_FACTOR(yset,0) , DSET_BRICK(yset,0) );
   xar = MRI_FLOAT_PTR(xim) ; yar = MRI_FLOAT_PTR(yim) ;

   DSET_unload(xset); DSET_unload(yset);

   /* make a byte mask from mask dataset */

   if( mask_dset != NULL ){
     int mcount ;
     if( DSET_NVOX(mask_dset) != nvox )
       ERROR_exit("Input and mask datasets are not same dimensions!");

     mmm = THD_makemask( mask_dset , 0 , mask_bot,mask_top ) ;
     mcount = THD_countmask( nvox , mmm ) ;
     INFO_message("%d voxels in the mask",mcount) ;
     if( mcount <= 5 ) ERROR_exit("Mask is too small!") ;
     DSET_delete(mask_dset) ;
     war = (float *)malloc(sizeof(float)*nvox) ;
     for( ii=0 ; ii < nvox ; ii++ ) war[ii] = (float)mmm[ii] ;
     free((void *)mmm) ;
   }

   val = THD_pearson_corr_wt( nvox , xar , yar , war ) ;
   val = 1.0 - fabs(val) ;
   printf("1-Correlation     = %.5f\n",val) ;

   val = -THD_mutual_info_scl( nvox , 0.0,0.0,xar , 0.0,0.0,yar , war ) ;
   printf("-Mutual Info      = %.5f\n",val ) ;

   val = -THD_norm_mutinf_scl( nvox , 0.0,0.0,xar , 0.0,0.0,yar , war ) ;
   printf("Norm Mutual Info  = %.5f\n",val ) ;

   val = THD_corr_ratio_scl( nvox , 0.0,0.0,xar , 0.0,0.0,yar , war ) ;
   val = 1.0 - fabs(val) ;
   printf("1-Correl ratio    = %.5f\n",val) ;

   val = -THD_hellinger_scl( nvox , 0.0,0.0,xar , 0.0,0.0,yar , war ) ;
   printf("-Hellinger metric = %.5f\n",val) ;

   exit(0) ;
}
