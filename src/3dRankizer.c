#include "mrilib.h"
int Help_3dRankizer(TFORM targ, int detail)
{

   if (detail >= 0) {
     sphinx_printf(targ,
"Usage: 3dRankizer [options] dataset\n"
"Output = Rank of each voxel as sorted into increasing value.\n"
"         - Ties get the average rank.\n"
"         - Not the same as 3dRank!\n"
"         - Only sub-brick #0 is processed at this time!\n"
"         - Ranks start at 1 and increase:\n"
"             Input  = 0   3   4   4   7   9\n"
"             Output = 1   2   3.5 3.5 5   6\n"
"Options:\n"
"  -brank bbb   Set the 'base' rank to 'bbb' instead of 1.\n"
"                 (You could also do this with 3dcalc.)\n"
"  -mask mset   Means to use the dataset 'mset' as a mask:\n"
"                 Only voxels with nonzero values in 'mset'\n"
"                 will be used from 'dataset'.  Voxels outside\n"
"                 the mask will get rank 0.\n"
"  -prefix ppp  Write results into float-format dataset 'ppp'\n"
"                 Output is in float format to allow for\n"
"                 non-integer ranks resulting from ties.\n"
"  -percentize : Divide rank by the number of voxels in the dataset x 100.0 \n"
"  -percentize_mask : Divide rank by the number of voxels in the mask x 100.0 \n"
"\n"
"Author: RW Cox  [[a quick hack for his own purposes]]\n"
      );
   }
   if (detail >= 1) {
      PRINT_COMPILE_DATE ;
   }
   return(0);
}

int main( int argc , char * argv[] )
{
   THD_3dim_dataset *inset=NULL , *outset=NULL , *mask_dset=NULL ;
   MRI_IMAGE *fim=NULL ; float *far=NULL ;
   int iarg , ndset , nvox , ii , mcount=0 , percentize = 0;
   char *prefix = "rankizer" ;
   byte *mmm=NULL ;
   float brank=1.0f ;


   /*---- official startup ---*/

   PRINT_VERSION("3dRankizer"); mainENTRY("3dRankizer main"); machdep();
   AFNI_logger("3dRankizer",argc,argv); AUTHOR("Zhark of the Ineffable Rank");

   /*-- read command line arguments --*/
   /*-- command line scan --*/

   iarg = 1 ;
   while( iarg < argc && argv[iarg][0] == '-' ){
     CHECK_HELP(arg,Help_3dRankizer);

     if( strncmp(argv[iarg],"-brank",5) == 0 ){
       if( iarg+1 >= argc )
         ERROR_exit("-brank option requires a following argument!") ;
       brank = (float)strtod(argv[++iarg],NULL) ;
       iarg++ ; continue ;
     }

     if( strncmp(argv[iarg],"-mask",5) == 0 ){
       if( mask_dset != NULL )
         ERROR_exit("Cannot have two -mask options!") ;
       if( iarg+1 >= argc )
         ERROR_exit("-mask option requires a following argument!") ;
       mask_dset = THD_open_dataset( argv[++iarg] ) ;
       if( mask_dset == NULL )
         ERROR_exit("Cannot open mask dataset!") ;
       iarg++ ; continue ;
     }

     if( strcmp(argv[iarg],"-prefix") == 0 ){
       if( ++iarg >= argc ) ERROR_exit("Need argument after '-prefix'") ;
       prefix = strdup(argv[iarg]) ;
       if( !THD_filename_ok(prefix) ) ERROR_exit("Illegal value after '-prefix'") ;
       iarg++ ; continue ;
     }

      if( strcmp(argv[iarg],"-percentize") == 0 ) {
         percentize = 1;
         iarg++ ; continue ;
      }

      if( strcmp(argv[iarg],"-percentize_mask") == 0 ) {
         percentize = 2;
         iarg++ ; continue ;
      }

      ERROR_message("Unknown option: %s\n",argv[iarg]) ;
                suggest_best_prog_option(argv[0], argv[iarg]);
      exit(1);
   }

   if( argc < 3 ){
     Help_3dRankizer(TXT, 0);
     PRINT_COMPILE_DATE ; exit(0) ;
   }

   /* should have 1 more arg */

   ndset = argc - iarg ;
        if( ndset < 1 ) ERROR_exit("No input dataset!?") ;
   else if( ndset > 1 ) WARNING_message("Too many input datasets!") ;

   inset = THD_open_dataset( argv[iarg] ) ; CHECK_OPEN_ERROR(inset,argv[iarg]) ;
   DSET_load(inset)                       ; CHECK_LOAD_ERROR(inset) ;
   fim = THD_extract_float_brick(0,inset) ; DSET_unload(inset) ;
   far = MRI_FLOAT_PTR(fim) ;
   nvox= DSET_NVOX(inset) ;

   /* make a byte mask from mask dataset */

   if( mask_dset != NULL ){
     if( DSET_NVOX(mask_dset) != nvox )
       ERROR_exit("Input and mask datasets are not same dimensions!");
     mmm = THD_makemask( mask_dset , 0 , 1.0f,-1.0f ) ;
     mcount = THD_countmask( nvox , mmm ) ;
     INFO_message("%d voxels in the mask",mcount) ;
     if( mcount <= 5 ) ERROR_exit("Mask is too small!") ;
     DSET_delete(mask_dset) ;
   }

   if( mmm == NULL ){
     mcount = nvox;
     rank_order_float( nvox , far ) ;
     for( ii=0 ; ii < nvox ; ii++ ) far[ii] += brank ;
   } else {
     float fmin=far[0] ;
     for( ii=1 ; ii < nvox ; ii++ ) if( far[ii] < fmin ) fmin = far[ii] ;
          if( fmin >  0.0f ) fmin = 0.0f ;
     else if( fmin == 0.0f ) fmin = -1.0f ;
     else                    fmin = -2.0f*fmin-1.0f ;
     for( ii=0 ; ii < nvox ; ii++ ) if( !mmm[ii] ) far[ii] = fmin ;
     rank_order_float( nvox , far ) ;
     fmin = (nvox-mcount) - brank ;
     for( ii=0 ; ii < nvox ; ii++ ){
       if( mmm[ii] ) far[ii] = far[ii] - fmin ;
       else          far[ii] = 0.0f ;
     }
   }

   if (percentize) {
      float fac=1.0;
      if (percentize == 1) fac = 100.0/nvox;
      else if (percentize == 2) fac = 100.0/mcount;
      for( ii=0 ; ii < nvox ; ii++ ) far[ii] *= fac;
   }

   outset = EDIT_empty_copy( inset ) ;
   EDIT_dset_items( outset ,
                      ADN_prefix    , prefix ,
                      ADN_brick_fac , NULL   ,
                      ADN_nvals     , 1      ,
                      ADN_ntt       , 0      ,
                    ADN_none ) ;
   EDIT_substitute_brick( outset , 0 , MRI_float , far ) ;
   DSET_write(outset) ; WROTE_DSET(outset) ;
   exit(0) ;
}
