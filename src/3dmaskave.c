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
   int dumpit = 0 , sigmait = 0 ;
   int miv = 0 ;                   /* 06 Aug 1998 */

   if( argc < 2 || strcmp(argv[1],"-help") == 0 ){
      printf("Usage: 3dmaskave [options] dataset\n"
             "Computes average of all voxels in the input dataset\n"
             "which satisfy the criterion in the options list.\n"
             "If no options are given, then all voxels are included.\n"
             "Options:\n"
             "  -sigma       Means to compute the standard deviation as well\n"
             "                 as the mean.\n"
             "  -mask dset   Means to use the dataset 'dset' as a mask:\n"
             "                 Only voxels with nonzero values in 'dset'\n"
             "                 will be averaged from 'dataset'.  Note\n"
             "                 that the mask dataset and the input dataset\n"
             "                 must have the same number of voxels.\n"
             "  -mindex miv  Means to use sub-brick #'miv' from the mask\n"
             "                 dataset.  If not given, miv=0.\n"
             "  -mrange a b  Means to further restrict the voxels from\n"
             "                 'dset' so that only those mask values\n"
             "                 between 'a' and 'b' (inclusive) will\n"
             "                 be used.  If this option is not given,\n"
             "                 all nonzero values from 'dset' are used.\n"
             "                 Note that if a voxel is zero in 'dset', then\n"
             "                 it won't be included, even if a < 0 < b.\n"
             "  -dump        Means to print out all the voxel values that\n"
             "                 go into the average.  This option cannot be\n"
             "                 used unles the -mask option is also used.\n"
             "  -udump       Means to print out all the voxel values that\n"
             "                 go into the average, UNSCALED by any internal\n"
             "                 factors.  Also requires -mask.\n"
             "                 N.B.: the scale factors for a sub-brick\n"
             "                       can be found using program 3dinfo.\n"
             "\n"
             "The output is printed to stdout (the terminal), and can be\n"
             "saved to a file using the usual redirection operation '>'.\n"
            ) ;
      exit(0) ;
   }

   /* scan argument list */

   narg = 1 ;
   while( narg < argc && argv[narg][0] == '-' ){

      if( strncmp(argv[narg],"-mask",5) == 0 ){
         if( mask_dset != NULL ){
            fprintf(stderr,"*** Cannot have two -mask options!\n") ; exit(1) ;
         }
         if( narg+1 >= argc ){
            fprintf(stderr,"*** -mask option requires a following argument!\n") ; exit(1) ;
         }
         mask_dset = THD_open_one_dataset( argv[++narg] ) ;
         if( mask_dset == NULL ){
            fprintf(stderr,"*** Cannot open mask dataset!\n") ; exit(1) ;
         }
         if( DSET_BRICK_TYPE(mask_dset,0) == MRI_complex ){
            fprintf(stderr,"*** Cannot deal with complex-valued mask dataset!\n") ; exit(1) ;
         }
         narg++ ; continue ;
      }

      /* 06 Aug 1998 */

      if( strncmp(argv[narg],"-mindex",5) == 0 ){
         if( narg+1 >= argc ){
            fprintf(stderr,"*** -mindex option needs 1 following argument!\n") ; exit(1) ;
         }
         miv = (int) strtod( argv[++narg] , NULL ) ;
         if( miv < 0 ){
            fprintf(stderr,"*** -mindex value is negative!\n") ; exit(1) ;
         }
         narg++ ; continue ;
      }

      if( strncmp(argv[narg],"-mrange",5) == 0 ){
         if( narg+2 >= argc ){
            fprintf(stderr,"*** -mrange option requires 2 following arguments!\n") ; exit(1) ;
         }
         mask_bot = strtod( argv[++narg] , NULL ) ;
         mask_top = strtod( argv[++narg] , NULL ) ;
         if( mask_top < mask_top ){
            fprintf(stderr,"*** -mrange inputs are illegal!\n") ; exit(1) ;
         }
         narg++ ; continue ;
      }

      if( strncmp(argv[narg],"-dump",5) == 0 ){
         dumpit = 1 ;
         narg++ ; continue ;
      }

      if( strncmp(argv[narg],"-udump",5) == 0 ){
         dumpit = 2 ;
         narg++ ; continue ;
      }

      if( strncmp(argv[narg],"-sigma",5) == 0 ){
         sigmait = 2 ;
         narg++ ; continue ;
      }

      fprintf(stderr,"*** Unknown option: %s\n",argv[narg]) ; exit(1) ;
   }

   /* should have one more argument */

   if( narg >= argc ){
      fprintf(stderr,"*** No input dataset!?\n") ; exit(1) ;
   }

   if( dumpit && mask_dset == NULL ){
      fprintf(stderr,"*** Can't use dump option without -mask!\n") ; exit(1) ;
   }

   if( miv > 0 ){                /* 06 Aug 1998 */
      if( mask_dset == NULL ){
         fprintf(stderr,"*** -mindex option used without -mask!\n") ; exit(1) ;
      }
      if( miv >= DSET_NVALS(mask_dset) ){
         fprintf(stderr,"*** -mindex value is too large!\n") ; exit(1) ;
      }
   }

   /* read input dataset */

   input_dset = THD_open_one_dataset( argv[narg] ) ;
   if( input_dset == NULL ){
      fprintf(stderr,"*** Cannot open input dataset!\n") ; exit(1) ;
   }
   if( DSET_BRICK_TYPE(input_dset,0) == MRI_complex ){
      fprintf(stderr,"*** Cannot deal with complex-valued input dataset!\n") ; exit(1) ;
   }

   nvox = DSET_NVOX(input_dset) ;

   /* make a byte mask from mask dataset */

   if( mask_dset != NULL ){
      if( DSET_NVOX(mask_dset) != nvox ){
         fprintf(stderr,"*** Input and mask datasets are not same dimensions!\n") ; exit(1) ;
      }
      DSET_load(mask_dset) ;
      if( DSET_ARRAY(mask_dset,miv) == NULL ){
         fprintf(stderr,"*** Cannot read in mask dataset BRIK!\n") ; exit(1) ;
      }
      mmm = (byte *) malloc( sizeof(byte) * nvox ) ;
      if( mmm == NULL ){
         fprintf(stderr,"*** Cannot malloc workspace!\n") ; exit(1) ;
      }

      switch( DSET_BRICK_TYPE(mask_dset,miv) ){
         default:
            fprintf(stderr,"*** Cannot deal with mask dataset datum!\n") ; exit(1) ;

         case MRI_short:{
            short mbot , mtop ;
            short * mar = (short *) DSET_ARRAY(mask_dset,miv) ;
            float mfac = DSET_BRICK_FACTOR(mask_dset,miv) ;
            if( mfac == 0.0 ) mfac = 1.0 ;
            if( mask_bot <= mask_top ){
               mbot = SHORTIZE(mask_bot/mfac) ;
               mtop = SHORTIZE(mask_top/mfac) ;
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
            byte * mar = (byte *) DSET_ARRAY(mask_dset,miv) ;
            float mfac = DSET_BRICK_FACTOR(mask_dset,miv) ;
            if( mfac == 0.0 ) mfac = 1.0 ;
            if( mask_bot <= mask_top ){
               mbot = BYTEIZE(mask_bot/mfac) ;
               mtop = BYTEIZE(mask_top/mfac) ;
               if( mtop == 0 ){
                  fprintf(stderr,"*** Illegal mask range for mask dataset of bytes.\n") ; exit(1) ;
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
            float * mar = (float *) DSET_ARRAY(mask_dset,miv) ;
            float mfac = DSET_BRICK_FACTOR(mask_dset,miv) ;
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
         fprintf(stderr,"*** No voxels survive the masking operations.\n") ; exit(1) ;
      }
   } else {
      mcount = nvox ;
   }
   fprintf(stderr,"+++ %d voxels being averaged\n",mcount) ;

   if( mcount < 2 && sigmait ){
      fprintf(stderr,"+++ [cannot compute sigma]\n") ;
      sigmait = 0 ;
   }

   DSET_load(input_dset) ;
   if( DSET_ARRAY(input_dset,0) == NULL ){
      fprintf(stderr,"*** Cannot read in input dataset BRIK!\n") ; exit(1) ;
   }

   /* loop over input sub-bricks */

   for( iv=0 ; iv < DSET_NVALS(input_dset) ; iv++ ){

      switch( DSET_BRICK_TYPE(input_dset,iv) ){

         default:
            printf("*** Illegal sub-brick datum at %d\n",iv) ;
         break ;

         case MRI_short:{
            short * bar = (short *) DSET_ARRAY(input_dset,iv) ;
            double sum = 0.0 , sigma = 0.0 ;
            float mfac = DSET_BRICK_FACTOR(input_dset,iv) ;
            if( mfac == 0.0 || dumpit == 2 ) mfac = 1.0 ;

            if( dumpit ){
               int noscal = (dumpit==2) || (mfac==1.0) ;
               printf("+++ Dump for sub-brick %d:\n",iv) ;
               for( ii=0 ; ii < nvox ; ii++ ) if( mmm[ii] ){
                                                 if( noscal ) printf(" %d\n",bar[ii]) ;
                                                 else         printf(" %g\n",bar[ii]*mfac) ;
                                              }
            }

            if( mmm != NULL ){
               for( ii=0 ; ii < nvox ; ii++ ) if( mmm[ii] ) sum += bar[ii] ;
            } else {
               for( ii=0 ; ii < nvox ; ii++ ) sum += bar[ii] ;
            }
            sum = sum / mcount ;

            if( sigmait ){
               if( mmm != NULL ){
                  for( ii=0 ; ii < nvox ; ii++ ) if( mmm[ii] ) sigma += SQR(bar[ii]-sum) ;
               } else {
                  for( ii=0 ; ii < nvox ; ii++ ) sigma += SQR(bar[ii]-sum) ;
               }
               sigma = mfac * sqrt( sigma/(mcount-1) ) ;
            }
            sum = mfac * sum ;

            if( dumpit ) printf("+++ Average = %g",sum) ;
            else         printf("%g",sum) ;

            if( sigmait ){
               if( dumpit ) printf("  Sigma = %g",sigma) ;
               else         printf("  %g",sigma) ;
            }
            printf("\n") ;
         }
         break ;

         case MRI_byte:{
            byte * bar = (byte *) DSET_ARRAY(input_dset,iv) ;
            double sum = 0.0 , sigma = 0.0 ;
            float mfac = DSET_BRICK_FACTOR(input_dset,iv) ;
            if( mfac == 0.0 || dumpit == 2 ) mfac = 1.0 ;

            if( dumpit ){
               int noscal = (dumpit==2) || (mfac==1.0) ;
               printf("+++ Dump for sub-brick %d:\n",iv) ;
               for( ii=0 ; ii < nvox ; ii++ ) if( mmm[ii] ){
                                                 if( noscal ) printf(" %d\n",bar[ii]) ;
                                                 else         printf(" %g\n",bar[ii]*mfac) ;
                                              }
            }

            if( mmm != NULL ){
               for( ii=0 ; ii < nvox ; ii++ ) if( mmm[ii] ) sum += bar[ii] ;
            } else {
               for( ii=0 ; ii < nvox ; ii++ ) sum += bar[ii] ;
            }
            sum = sum / mcount ;

            if( sigmait ){
               if( mmm != NULL ){
                  for( ii=0 ; ii < nvox ; ii++ ) if( mmm[ii] ) sigma += SQR(bar[ii]-sum) ;
               } else {
                  for( ii=0 ; ii < nvox ; ii++ ) sigma += SQR(bar[ii]-sum) ;
               }
               sigma = mfac * sqrt( sigma/(mcount-1) ) ;
            }
            sum = mfac * sum ;

            if( dumpit ) printf("+++ Average = %g",sum) ;
            else         printf("%g",sum) ;

            if( sigmait ){
               if( dumpit ) printf("  Sigma = %g",sigma) ;
               else         printf("  %g",sigma) ;
            }
            printf("\n") ;
         }
         break ;

         case MRI_float:{
            float * bar = (float *) DSET_ARRAY(input_dset,iv) ;
            double sum = 0.0 , sigma = 0.0 ;
            float mfac = DSET_BRICK_FACTOR(input_dset,iv) ;
            if( mfac == 0.0 || dumpit == 2 ) mfac = 1.0 ;

            if( dumpit ){
               int noscal = (dumpit==2) || (mfac==1.0) ;
               printf("+++ Dump for sub-brick %d:\n",iv) ;
               for( ii=0 ; ii < nvox ; ii++ ) if( mmm[ii] ){
                                                 if( noscal ) printf(" %g\n",bar[ii]) ;
                                                 else         printf(" %g\n",bar[ii]*mfac) ;
                                              }
            }

            if( mmm != NULL ){
               for( ii=0 ; ii < nvox ; ii++ ) if( mmm[ii] ) sum += bar[ii] ;
            } else {
               for( ii=0 ; ii < nvox ; ii++ ) sum += bar[ii] ;
            }
            sum = sum / mcount ;

            if( sigmait ){
               if( mmm != NULL ){
                  for( ii=0 ; ii < nvox ; ii++ ) if( mmm[ii] ) sigma += SQR(bar[ii]-sum) ;
               } else {
                  for( ii=0 ; ii < nvox ; ii++ ) sigma += SQR(bar[ii]-sum) ;
               }
               sigma = mfac * sqrt( sigma/(mcount-1) ) ;
            }
            sum = mfac * sum ;

            if( dumpit ) printf("+++ Average = %g",sum) ;
            else         printf("%g",sum) ;

            if( sigmait ){
               if( dumpit ) printf("  Sigma = %g",sigma) ;
               else         printf("  %g",sigma) ;
            }
            printf("\n") ;
         }
         break ;
      }
   }
   exit(0) ;
}
