#include "mrilib.h"
#include <string.h>

double DSET_cor( THD_3dim_dataset *, THD_3dim_dataset *, byte *, int ) ;

int main( int argc , char * argv[] )
{
   double dxy ;
   int narg , ndset , nvox , demean=0 ;
   THD_3dim_dataset * xset , * yset , * mask_dset ;
   float mask_bot=666.0 , mask_top=-666.0 ;
   byte * mmm=NULL ;

   /*-- read command line arguments --*/

   if( argc < 3 || strncmp(argv[1],"-help",5) == 0 ){
      printf("Usage: 3ddot [options] dset1 dset2\n"
             "Output = correlation coefficient between 2 dataset bricks\n"
             "         - you can use sub-brick selectors on the dsets\n"
             "         - the result is a number printed to stdout"
             "Options:\n"
             "  -mask mset   Means to use the dataset 'mset' as a mask:\n"
             "                 Only voxels with nonzero values in 'mset'\n"
             "                 will be averaged from 'dataset'.  Note\n"
             "                 that the mask dataset and the input dataset\n"
             "                 must have the same number of voxels.\n"
             "  -mrange a b  Means to further restrict the voxels from\n"
             "                 'mset' so that only those mask values\n"
             "                 between 'a' and 'b' (inclusive) will\n"
             "                 be used.  If this option is not given,\n"
             "                 all nonzero values from 'mset' are used.\n"
             "                 Note that if a voxel is zero in 'mset', then\n"
             "                 it won't be included, even if a < 0 < b.\n"
             "  -demean      Means to remove the mean from each volume\n"
             "                 prior to computing the correlation.\n"
            ) ;

      printf("\n" MASTER_SHORTHELP_STRING ) ;

      exit(0) ;
   }

   narg = 1 ;
   while( narg < argc && argv[narg][0] == '-' ){

      if( strncmp(argv[narg],"-demean",5) == 0 ){
         demean++ ;
         narg++ ; continue ;
      }

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

      if( strncmp(argv[narg],"-mrange",5) == 0 ){
         if( narg+2 >= argc ){
            fprintf(stderr,"*** -mrange option requires 2 following arguments!\n")
;
             exit(1) ;
         }
         mask_bot = strtod( argv[++narg] , NULL ) ;
         mask_top = strtod( argv[++narg] , NULL ) ;
         if( mask_top < mask_top ){
            fprintf(stderr,"*** -mrange inputs are illegal!\n") ; exit(1) ;
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

   xset = THD_open_dataset( argv[narg++] ) ;
   yset = THD_open_dataset( argv[narg++] ) ;
   if( xset == NULL || yset == NULL ){
      fprintf(stderr,"*** cannot open both input datasets!\n") ; exit(1) ;
   }
   if( DSET_NUM_TIMES(xset) > 1 || DSET_NUM_TIMES(yset) > 1 ){
      fprintf(stderr,"*** cannot use time-dependent datasets!\n") ; exit(1) ;
   }
   nvox = DSET_NVOX(xset) ;
   if( nvox != DSET_NVOX(yset) ){
      fprintf(stderr,"*** input datasets dimensions don't match!\n");exit(1);
   }

   /* make a byte mask from mask dataset */

   if( mask_dset != NULL ){
      int mcount ;
      if( DSET_NVOX(mask_dset) != nvox ){
         fprintf(stderr,"*** Input and mask datasets are not same dimensions!\n");
         exit(1) ;
      }
      mmm = THD_makemask( mask_dset , 0 , mask_bot,mask_top ) ;
      mcount = THD_countmask( nvox , mmm ) ;
      fprintf(stderr,"+++ %d voxels in the mask\n",mcount) ;
      if( mcount <= 5 ){
         fprintf(stderr,"*** Mask is too small!\n");exit(1);
      }
      DSET_delete(mask_dset) ;
   }

   dxy = DSET_cor( xset , yset , mmm , demean ) ;
   printf("%g\n",dxy) ;
   exit(0) ;
}

double DSET_cor( THD_3dim_dataset *xset,
                 THD_3dim_dataset *yset, byte *mmm , int dm )
{
   double sumxx , sumyy , sumxy , tx,ty , dxy ;
   void  *  xar , *  yar ;
   float * fxar , * fyar ;
   int ii , nxyz , ivx,ivy , itypx,itypy , fxar_new,fyar_new , nnn ;

   nxyz = DSET_NVOX(xset) ;

   /* load bricks */

   THD_load_datablock( xset->dblk , NULL ) ;
   ivx   = 0 ;
   itypx = DSET_BRICK_TYPE(xset,ivx) ;
   xar   = DSET_ARRAY(xset,ivx) ; if( xar == NULL ) return 0.0 ;
   if( itypx == MRI_float ){
      fxar = (float *) xar ; fxar_new = 0 ;
   } else {
      fxar = (float *) malloc( sizeof(float) * nxyz ) ; fxar_new = 1 ;
      EDIT_coerce_type( nxyz , itypx,xar , MRI_float,fxar ) ;
      PURGE_DSET( xset ) ;
   }

   THD_load_datablock( yset->dblk , NULL ) ;
   ivy   = 0 ;
   itypy = DSET_BRICK_TYPE(yset,ivy) ;
   yar   = DSET_ARRAY(yset,ivy) ; if( yar == NULL ) return 0.0 ;
   if( itypy == MRI_float ){
      fyar = (float *) yar ; fyar_new = 0 ;
   } else {
      fyar = (float *) malloc( sizeof(float) * nxyz ) ; fyar_new = 1 ;
      EDIT_coerce_type( nxyz , itypy,yar , MRI_float,fyar ) ;
      PURGE_DSET( yset ) ;
   }

   /* 29 Feb 2000: remove mean? */

   if( dm ){
      sumxx = sumyy = 0.0 ;
      for( nnn=ii=0 ; ii < nxyz ; ii++ ){
         if( mmm == NULL || mmm[ii] ){
            sumxx += fxar[ii] ; sumyy += fyar[ii] ; nnn++ ;
         }
      }
      if( nnn < 5 ) return 0.0 ;             /* ERROR */
      sumxx /= nnn ; sumyy /= nnn ;
      for( ii=0 ; ii < nxyz ; ii++ ){
         if( mmm == NULL || mmm[ii] ){
            fxar[ii] -= sumxx ; fyar[ii] -= sumyy ;
         }
      }
   }

   /* compute sums */

   sumxx = sumyy = sumxy = 0.0 ;

   for( ii=0 ; ii < nxyz ; ii++ ){
      if( mmm == NULL || mmm[ii] ){
         tx = fxar[ii] ; ty = fyar[ii] ;
         sumxx += tx * tx ;
         sumyy += ty * ty ;
         sumxy += tx * ty ;
      }
   }

   /* toss trash */

   if( fxar_new ) free( fxar ) ;
   if( fyar_new ) free( fyar ) ;

   /* compute result */

   dxy = sumxx * sumyy ; if( dxy <= 0.0 ) return 0.0 ;
   dxy = sumxy / sqrt(dxy) ;
   return dxy ;
}
