#include "mrilib.h"

/*-----------
  A quickie.
-------------*/

int main( int argc , char * argv[] )
{
   int narg , nvox , ii , mcount , iv , mc ;
   THD_3dim_dataset * mask_dset=NULL , * input_dset=NULL ;
   float mask_bot=666.0 , mask_top=-666.0 ;
   byte * mmm = NULL ;
   int dumpit = 0 , sigmait = 0 ;
   int miv = 0 ;                              /* 06 Aug 1998 */
   int div = -1 , div_bot,div_top , drange=0; /* 16 Sep 1998 */
   float data_bot=666.0 , data_top=-666.0 ;
   int indump = 0 ;                           /* 19 Aug 1999 */
   int pslice=-1 , qslice=-1 , nxy,nz ;       /* 15 Sep 1999 */

   if( argc < 2 || strcmp(argv[1],"-help") == 0 ){
      printf("Usage: 3dmaskave [options] dataset\n"
             "Computes average of all voxels in the input dataset\n"
             "which satisfy the criterion in the options list.\n"
             "If no options are given, then all voxels are included.\n"
             "Options:\n"
             "  -mask mset   Means to use the dataset 'mset' as a mask:\n"
             "                 Only voxels with nonzero values in 'mset'\n"
             "                 will be averaged from 'dataset'.  Note\n"
             "                 that the mask dataset and the input dataset\n"
             "                 must have the same number of voxels.\n"
             "  -mindex miv  Means to use sub-brick #'miv' from the mask\n"
             "                 dataset.  If not given, miv=0.\n"
             "  -mrange a b  Means to further restrict the voxels from\n"
             "                 'mset' so that only those mask values\n"
             "                 between 'a' and 'b' (inclusive) will\n"
             "                 be used.  If this option is not given,\n"
             "                 all nonzero values from 'mset' are used.\n"
             "                 Note that if a voxel is zero in 'mset', then\n"
             "                 it won't be included, even if a < 0 < b.\n"
             "\n"
             "  -dindex div  Means to use sub-brick #'div' from the dataset.\n"
             "                 If not given, all sub-bricks will be processed.\n"
             "  -drange a b  Means to only include voxels from the dataset whose\n"
             "                 values fall in the range 'a' to 'b' (inclusive).\n"
             "                 Otherwise, all voxel values are included.\n"
             "\n"
             "  -slices p q  Means to only included voxels from the dataset\n"
             "                 whose slice numbers are in the range 'p' to 'q'\n"
             "                 (inclusive).  Slice numbers range from 0 to\n"
             "                 NZ-1, where NZ can be determined from the output\n"
             "                 of program 3dinfo.  The default is to include\n"
             "                 data from all slices.\n"
             "                 [There is no provision for geometrical voxel]\n"
             "                 [selection except in the slice (z) direction]\n"
             "\n"
             "  -sigma       Means to compute the standard deviation as well\n"
             "                 as the mean.\n"
             "  -dump        Means to print out all the voxel values that\n"
             "                 go into the average.\n"
             "  -udump       Means to print out all the voxel values that\n"
             "                 go into the average, UNSCALED by any internal\n"
             "                 factors.\n"
             "                 N.B.: the scale factors for a sub-brick\n"
             "                       can be found using program 3dinfo.\n"
             "  -indump      Means to print out the voxel indexes (i,j,k) for\n"
             "                 each dumped voxel.  Has no effect if -dump\n"
             "                 or -udump is not also used.\n"
             "                 N.B.: if nx,ny,nz are the number of voxels in\n"
             "                       each direction, then the array offset\n"
             "                       in the brick corresponding to (i,j,k)\n"
             "                       is i+j*nx+k*nx*ny.\n"
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
         mask_dset = THD_open_dataset( argv[++narg] ) ;
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

      /* 16 Sep 1998 */

      if( strncmp(argv[narg],"-dindex",5) == 0 ){
         if( narg+1 >= argc ){
            fprintf(stderr,"*** -dindex option needs 1 following argument!\n") ; exit(1) ;
         }
         div = (int) strtod( argv[++narg] , NULL ) ;
         if( div < 0 ){
            fprintf(stderr,"*** -dindex value is negative!\n") ; exit(1) ;
         }
         narg++ ; continue ;
      }

      if( strncmp(argv[narg],"-drange",5) == 0 ){
         if( narg+2 >= argc ){
            fprintf(stderr,
                    "*** -drange option requires 2 following arguments!\n") ;
            exit(1) ;
         }
         data_bot = strtod( argv[++narg] , NULL ) ;
         data_top = strtod( argv[++narg] , NULL ) ;
         if( data_top < data_top ){
            fprintf(stderr,"*** -mrange inputs are illegal!\n") ; exit(1) ;
         }
         drange = 1 ;
         narg++ ; continue ;
      }

      if( strncmp(argv[narg],"-slices",5) == 0 ){  /* 15 Sep 1999 */
         if( narg+2 >= argc ){
            fprintf(stderr,
                    "*** -slices option requires 2 following arguments!\n") ;
            exit(1) ;
         }
         pslice = (int) strtod( argv[++narg] , NULL ) ;
         qslice = (int) strtod( argv[++narg] , NULL ) ;
         if( pslice < 0 || qslice < 0 || qslice < pslice ){
            fprintf(stderr, "*** Illegal values after -slices!\n") ;
            exit(1) ;
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

      if( strncmp(argv[narg],"-indump",5) == 0 ){  /* 19 Aug 1999 */
         indump = 1 ;
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

#if 0
   if( dumpit && mask_dset == NULL ){
      fprintf(stderr,"*** Can't use dump option without -mask!\n") ; exit(1) ;
   }
#endif

   if( miv > 0 ){                /* 06 Aug 1998 */
      if( mask_dset == NULL ){
         fprintf(stderr,"*** -mindex option used without -mask!\n") ; exit(1) ;
      }
      if( miv >= DSET_NVALS(mask_dset) ){
         fprintf(stderr,"*** -mindex value is too large!\n") ; exit(1) ;
      }
   }

   /* read input dataset */

   input_dset = THD_open_dataset( argv[narg] ) ;
   if( input_dset == NULL ){
      fprintf(stderr,"*** Cannot open input dataset!\n") ; exit(1) ;
   }

   if( DSET_BRICK_TYPE(input_dset,0) == MRI_complex ){
      fprintf(stderr,"*** Cannot deal with complex-valued input dataset!\n") ; exit(1) ;
   }

   if( div >= DSET_NVALS(input_dset) ){
      fprintf(stderr,"*** Not enough sub-bricks in dataset for -dindex %d!\n",div) ; exit(1) ;
   }

   if( pslice >= 0 ){
      nxy = DSET_NX(input_dset) * DSET_NY(input_dset) ;
      nz  = DSET_NZ(input_dset) ;
      if( qslice >= nz ){
         fprintf(stderr,
                 "*** There are only %d slices in the input dataset!\n",nz) ;
         exit(1) ;
      }

      if( pslice == 0 && qslice == nz-1 )
         fprintf(stderr,"+++ -slice option says to use all slices!?\n") ;
   }

   nvox = DSET_NVOX(input_dset) ;

   /* make a byte mask from mask dataset */

   mmm = (byte *) malloc( sizeof(byte) * nvox ) ;
   if( mmm == NULL ){
      fprintf(stderr,"*** Cannot malloc workspace!\n") ; exit(1) ;
   }

   if( mask_dset != NULL ){
      if( DSET_NVOX(mask_dset) != nvox ){
         fprintf(stderr,"*** Input and mask datasets are not same dimensions!\n") ; exit(1) ;
      }
      DSET_load(mask_dset) ;
      if( DSET_ARRAY(mask_dset,miv) == NULL ){
         fprintf(stderr,"*** Cannot read in mask dataset BRIK!\n") ; exit(1) ;
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
         fprintf(stderr,"*** No voxels survive the masking operation\n"); exit(1);
      }

      fprintf(stderr,"+++ %d voxels survive the mask\n",mcount) ;

   } else {
      mcount = nvox ;
      memset( mmm , 1 , mcount ) ;
      fprintf(stderr,"+++ %d voxels in the entire dataset (no mask)\n",mcount) ;
   }

   if( pslice >= 0 ){     /* 15 Sep 1999 */
      int kz , ibot ;
      mcount = 0 ;
      for( kz=0 ; kz < nz ; kz++ ){           /* loop over all slices */
         ibot = kz*nxy ;                      /* base index for this slice */

         if( kz >= pslice && kz <= qslice ){  /* keepers => recount */
            for( ii=0 ; ii < nxy ; ii++ )
               if( mmm[ii+ibot] ) mcount++ ;
         } else {                             /* throw them back */
            for( ii=0 ; ii < nxy ; ii++ )
               mmm[ii+ibot] = 0 ;
         }
      }

      if( mcount == 0 ){
         fprintf(stderr,"*** No voxels survive the slicing operation\n"); exit(1);
      }

      fprintf(stderr,"+++ %d voxels survive the slicing\n",mcount) ;
   }

   if( mcount < 2 && sigmait ){
      fprintf(stderr,"+++ [too few voxels; cannot compute sigma]\n") ;
      sigmait = 0 ;
   }

   DSET_load(input_dset) ;
   if( DSET_ARRAY(input_dset,0) == NULL ){
      fprintf(stderr,"*** Cannot read in input dataset BRIK!\n") ; exit(1) ;
   }

   /* loop over input sub-bricks */

   if( div < 0 ){
      div_bot = 0 ; div_top = DSET_NVALS(input_dset) ;
   } else {
      div_bot = div ; div_top = div+1 ;
   }

   for( iv=div_bot ; iv < div_top ; iv++ ){

      switch( DSET_BRICK_TYPE(input_dset,iv) ){

         default:
            printf("*** Illegal sub-brick datum at %d\n",iv) ;
         break ;

#define INRANGE(i) ( !drange || ( mfac*bar[i] >= data_bot && mfac*bar[i] <= data_top ) )
#define GOOD(i)    ( mmm[i] && INRANGE(i) )

         case MRI_short:{
            short * bar = (short *) DSET_ARRAY(input_dset,iv) ;
            double sum = 0.0 , sigma = 0.0 ;
            float mfac = DSET_BRICK_FACTOR(input_dset,iv) ;
            if( mfac == 0.0 || dumpit == 2 ) mfac = 1.0 ;

            if( dumpit ){
               int noscal = (dumpit==2) || (mfac==1.0) ;
               printf("+++ Dump for sub-brick %d:\n",iv) ;
               for( ii=0 ; ii < nvox ; ii++ ) if( GOOD(ii) ){
                                                 if( noscal ) printf(" %d",bar[ii]) ;
                                                 else         printf(" %g",bar[ii]*mfac) ;

                                                 if( indump )
                                                    printf(" (%d,%d,%d)",
                                                           DSET_index_to_ix(input_dset,ii),
                                                           DSET_index_to_jy(input_dset,ii),
                                                           DSET_index_to_kz(input_dset,ii) ) ;
                                                 printf("\n") ;
                                              }
            }

            for( ii=mc=0 ; ii < nvox ; ii++ )
               if( GOOD(ii) ){ sum += bar[ii] ; mc++ ; }
            if( mc > 0 ) sum = sum / mc ;

            if( sigmait && mc > 1 ){
               for( ii=0 ; ii < nvox ; ii++ ) if( GOOD(ii) ) sigma += SQR(bar[ii]-sum) ;
               sigma = mfac * sqrt( sigma/(mc-1) ) ;
            }
            sum = mfac * sum ;

            if( dumpit ) printf("+++ Average = %g",sum) ;
            else         printf("%g",sum) ;

            if( sigmait ){
               if( dumpit ) printf("  Sigma = %g",sigma) ;
               else         printf("  %g",sigma) ;
            }
            printf(" [%d voxels]\n",mc) ;
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
               for( ii=0 ; ii < nvox ; ii++ ) if( GOOD(ii) ){
                                                 if( noscal ) printf(" %d",bar[ii]) ;
                                                 else         printf(" %g",bar[ii]*mfac) ;

                                                 if( indump )
                                                    printf(" (%d,%d,%d)",
                                                           DSET_index_to_ix(input_dset,ii),
                                                           DSET_index_to_jy(input_dset,ii),
                                                           DSET_index_to_kz(input_dset,ii) ) ;
                                                 printf("\n") ;
                                              }
            }

            for( ii=mc=0 ; ii < nvox ; ii++ )
               if( GOOD(ii) ){ sum += bar[ii] ; mc++ ; }
            if( mc > 0 ) sum = sum / mc ;

            if( sigmait && mc > 1 ){
               for( ii=0 ; ii < nvox ; ii++ ) if( GOOD(ii) ) sigma += SQR(bar[ii]-sum) ;
               sigma = mfac * sqrt( sigma/(mc-1) ) ;
            }
            sum = mfac * sum ;

            if( dumpit ) printf("+++ Average = %g",sum) ;
            else         printf("%g",sum) ;

            if( sigmait ){
               if( dumpit ) printf("  Sigma = %g",sigma) ;
               else         printf("  %g",sigma) ;
            }
            printf(" [%d voxels]\n",mc) ;
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
               for( ii=0 ; ii < nvox ; ii++ ) if( GOOD(ii) ){
                                                 if( noscal ) printf(" %g",bar[ii]) ;
                                                 else         printf(" %g",bar[ii]*mfac) ;

                                                 if( indump )
                                                    printf(" (%d,%d,%d)",
                                                           DSET_index_to_ix(input_dset,ii),
                                                           DSET_index_to_jy(input_dset,ii),
                                                           DSET_index_to_kz(input_dset,ii) ) ;
                                                 printf("\n") ;
                                              }
            }

            for( ii=mc=0 ; ii < nvox ; ii++ )
               if( GOOD(ii) ){ sum += bar[ii] ; mc++ ; }
            if( mc > 0 ) sum = sum / mc ;

            if( sigmait && mc > 1 ){
               for( ii=0 ; ii < nvox ; ii++ ) if( GOOD(ii) ) sigma += SQR(bar[ii]-sum) ;
               sigma = mfac * sqrt( sigma/(mc-1) ) ;
            }
            sum = mfac * sum ;

            if( dumpit ) printf("+++ Average = %g",sum) ;
            else         printf("%g",sum) ;

            if( sigmait ){
               if( dumpit ) printf("  Sigma = %g",sigma) ;
               else         printf("  %g",sigma) ;
            }
            printf(" [%d voxels]\n",mc) ;
         }
         break ;
      }
   }
   exit(0) ;
}
