#include "mrilib.h"

/*-----------
  A quickie.
-------------*/

int main( int argc , char * argv[] )
{
   int narg , nvox , ii , mcount , iv ;
   THD_3dim_dataset * mask_dset=NULL , * input_dset=NULL ;
   float mask_bot=666.0 , mask_top=-666.0 ;
   byte * mmm = NULL ;

   if( argc < 2 || strcmp(argv[1],"-help") == 0 ){
      printf("Usage: 3dmaskave [options] dataset\n"
             "Computes average of all voxels in the input dataset\n"
             "which satisfy the criterion in the options list.\n"
             "If no options are given, then all voxels are included.\n"
             "Options:\n"
             "  -mask dset   Means to use the dataset 'dset' as a mask:\n"
             "                 Only voxels with nonzero values in 'dset'\n"
             "                 will be averaged from 'dataset'.\n"
             "  -mrange a b  Means to further restrict the voxels from\n"
             "                 'dset' so that only those mask values\n"
             "                 between 'a' and 'b' (inclusive) will\n"
             "                 be used.  If this option is not given,\n"
             "                 all nonzero values from 'dset' are used.\n"
             "                 Note that if a voxel is zero in 'dset', then\n"
             "                 it won't be averaged, even if a < 0 < b.\n"
             "\n"
             "The output is printed to standard output (the screen), and is simply\n"
             "a single number per line, one line per sub-brick from 'dataset'.\n"
            ) ;
      exit(0) ;
   }

   /* scan argument list */

   narg = 1 ;
   while( narg < argc && argv[narg][0] == '-' ){

      if( strncmp(argv[narg],"-mask",5) == 0 ){
         if( mask_dset != NULL ){
            fprintf(stderr,"Cannot have two -mask options!\n") ; exit(1) ;
         }
         if( narg+1 >= argc ){
            fprintf(stderr,"-mask option requires a following argument!\n") ; exit(1) ;
         }
         mask_dset = THD_open_one_dataset( argv[++narg] ) ;
         if( mask_dset == NULL ){
            fprintf(stderr,"Cannot open mask dataset!\n") ; exit(1) ;
         }
         if( DSET_BRICK_TYPE(mask_dset,0) == MRI_complex ){
            fprintf(stderr,"Cannot deal with complex-valued mask dataset!\n") ; exit(1) ;
         }
         narg++ ; continue ;
      }

      if( strncmp(argv[narg],"-mrange",5) == 0 ){
         if( narg+2 >= argc ){
            fprintf(stderr,"-mrange option requires 2 following arguments!\n") ; exit(1) ;
         }
         mask_bot = strtod( argv[++narg] , NULL ) ;
         mask_top = strtod( argv[++narg] , NULL ) ;
         if( mask_top < mask_top ){
            fprintf(stderr,"-mrange inputs are illegal!\n") ; exit(1) ;
         }
         narg++ ; continue ;
      }

      fprintf(stderr,"Unknown option: %s\n",argv[narg]) ; exit(1) ;
   }

   /* should have one more argument */

   if( narg >= argc ){
      fprintf(stderr,"No input dataset!?\n") ; exit(1) ;
   }

   /* read input dataset */

   input_dset = THD_open_one_dataset( argv[narg] ) ;
   if( input_dset == NULL ){
      fprintf(stderr,"Cannot open input dataset!\n") ; exit(1) ;
   }
   if( DSET_BRICK_TYPE(input_dset,0) == MRI_complex ){
      fprintf(stderr,"Cannot deal with complex-valued input dataset!\n") ; exit(1) ;
   }

   nvox = DSET_NVOX(input_dset) ;

   /* make a byte mask from mask dataset */

   if( mask_dset != NULL ){
      if( DSET_NVOX(mask_dset) != nvox ){
         fprintf(stderr,"Input and mask datasets are not same dimensions!\n") ; exit(1) ;
      }
      DSET_load(mask_dset) ;
      if( DSET_ARRAY(mask_dset,0) == NULL ){
         fprintf(stderr,"Cannot read in mask dataset BRIK!\n") ; exit(1) ;
      }
      mmm = (byte *) malloc( sizeof(byte) * nvox ) ;
      if( mmm == NULL ){
         fprintf(stderr,"Cannot malloc workspace!\n") ; exit(1) ;
      }

      switch( DSET_BRICK_TYPE(mask_dset,0) ){
         default:
            fprintf(stderr,"Cannot deal with mask dataset datum!\n") ; exit(1) ;

         case MRI_short:{
            short mbot , mtop ;
            short * mar = (short *) DSET_ARRAY(mask_dset,0) ;
            float mfac = DSET_BRICK_FACTOR(mask_dset,0) ;
            if( mfac == 0.0 ) mfac = 1.0 ;
            if( mask_bot <= mask_top ){
               mbot = (short) (mask_bot/mfac) ;
               mtop = (short) (mask_top/mfac) ;
            } else {
               mbot = (short) -MRI_TYPE_maxval[MRI_short] ;
               mtop = (short)  MRI_TYPE_maxval[MRI_short] ;
            }
            for( mcount=0,ii=0 ; ii < nvox ; ii++ )
               if( mar[ii] >= mbot && mar[ii] <= mtop && mar[ii] != 0 ){ mmm[ii] = 1 ; mcount++ ; }
               else                                                    { mmm[ii] = 0 ; }
         }
         break ;

         case MRI_byte:{
            byte mbot , mtop ;
            byte * mar = (byte *) DSET_ARRAY(mask_dset,0) ;
            float mfac = DSET_BRICK_FACTOR(mask_dset,0) ;
            if( mfac == 0.0 ) mfac = 1.0 ;
            if( mask_bot <= mask_top ){
               mbot = (byte) ((mask_bot > 0.0) ? (mask_bot/mfac) : 0.0) ;
               mtop = (byte) ((mask_top > 0.0) ? (mask_top/mfac) : 0.0) ;
               if( mtop == 0 ){
                  fprintf(stderr,"Illegal mask range for mask dataset of bytes.\n") ; exit(1) ;
               }
            } else {
               mbot = 0 ;
               mtop = (byte) MRI_TYPE_maxval[MRI_short] ;
            }
            for( mcount=0,ii=0 ; ii < nvox ; ii++ )
               if( mar[ii] >= mbot && mar[ii] <= mtop && mar[ii] != 0 ){ mmm[ii] = 1 ; mcount++ ; }
               else                                                    { mmm[ii] = 0 ; }
         }
         break ;

         case MRI_float:{
            float mbot , mtop ;
            float * mar = (float *) DSET_ARRAY(mask_dset,0) ;
            float mfac = DSET_BRICK_FACTOR(mask_dset,0) ;
            if( mfac == 0.0 ) mfac = 1.0 ;
            if( mask_bot <= mask_top ){
               mbot = (float) (mask_bot/mfac) ;
               mtop = (float) (mask_top/mfac) ;
            } else {
               mbot = -WAY_BIG ;
               mtop =  WAY_BIG ;
            }
            for( mcount=0,ii=0 ; ii < nvox ; ii++ )
               if( mar[ii] >= mbot && mar[ii] <= mtop && mar[ii] != 0 ){ mmm[ii] = 1 ; mcount++ ; }
               else                                                    { mmm[ii] = 0 ; }
         }
         break ;
      }
      DSET_unload(mask_dset) ;

      if( mcount == 0 ){
         fprintf(stderr,"No voxels survive the masking operations.\n") ; exit(1) ;
      }
   } else {
      mcount = nvox ;
   }
   fprintf(stderr,"%d voxels being averaged\n",mcount) ;

   DSET_load(input_dset) ;
   if( DSET_ARRAY(input_dset,0) == NULL ){
      fprintf(stderr,"Cannot read in input dataset BRIK!\n") ; exit(1) ;
   }

   /* loop over input sub-bricks */

   for( iv=0 ; iv < DSET_NVALS(input_dset) ; iv++ ){

      switch( DSET_BRICK_TYPE(input_dset,iv) ){

         default:
            printf("Illegal sub-brick datum\n") ;
         break ;

         case MRI_short:{
            short * bar = (short *) DSET_ARRAY(input_dset,iv) ;
            double sum = 0.0 ;
            float mfac = DSET_BRICK_FACTOR(input_dset,iv) ;
            if( mfac == 0.0 ) mfac = 1.0 ;
            if( mmm != NULL ){
               for( ii=0 ; ii < nvox ; ii++ ) if( mmm[ii] ) sum += bar[ii] ;
            } else {
               for( ii=0 ; ii < nvox ; ii++ ) sum += bar[ii] ;
            }
            sum = mfac * sum / mcount ; printf("%g\n",sum) ;
         }
         break ;

         case MRI_byte:{
            byte * bar = (byte *) DSET_ARRAY(input_dset,iv) ;
            double sum = 0.0 ;
            float mfac = DSET_BRICK_FACTOR(input_dset,iv) ;
            if( mfac == 0.0 ) mfac = 1.0 ;
            if( mmm != NULL ){
               for( ii=0 ; ii < nvox ; ii++ ) if( mmm[ii] ) sum += bar[ii] ;
            } else {
               for( ii=0 ; ii < nvox ; ii++ ) sum += bar[ii] ;
            }
            sum = mfac * sum / mcount ; printf("%g\n",sum) ;
         }
         break ;

         case MRI_float:{
            float * bar = (float *) DSET_ARRAY(input_dset,iv) ;
            double sum = 0.0 ;
            float mfac = DSET_BRICK_FACTOR(input_dset,iv) ;
            if( mfac == 0.0 ) mfac = 1.0 ;
            if( mmm != NULL ){
               for( ii=0 ; ii < nvox ; ii++ ) if( mmm[ii] ) sum += bar[ii] ;
            } else {
               for( ii=0 ; ii < nvox ; ii++ ) sum += bar[ii] ;
            }
            sum = mfac * sum / mcount ; printf("%g\n",sum) ;
         }
         break ;
      }
   }
   exit(0) ;
}
