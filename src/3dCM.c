#include "mrilib.h"

THD_fvec3 THD_cmass( THD_3dim_dataset *xset , int iv , byte *mmm ) ;

int main( int argc , char * argv[] )
{
   int narg=1, do_automask=0 , iv , nxyz ;
   THD_3dim_dataset *xset ;
   byte *mmm=NULL ; int nmask=0 , nvox_mask ;
   THD_fvec3 cmv ;

   /*-- read command line arguments --*/

   if( argc < 2 || strncmp(argv[1],"-help",5) == 0 ){
      printf("Usage: 3dCM [options] dset\n"
             "Output = center of mass of dataset, to stdout.\n"
             "  -mask mset   Means to use the dataset 'mset' as a mask:\n"
             "                 Only voxels with nonzero values in 'mset'\n"
             "                 will be averaged from 'dataset'.  Note\n"
             "                 that the mask dataset and the input dataset\n"
             "                 must have the same number of voxels.\n"
             "  -automask    Generate the mask automatically.\n"
            ) ;
      exit(0) ;
   }

   narg = 1 ;
   while( narg < argc && argv[narg][0] == '-' ){

      if( strncmp(argv[narg],"-mask",5) == 0 ){
        THD_3dim_dataset *mask_dset ;
        if( mmm != NULL ){
          fprintf(stderr,"*** Cannot have two -mask options!\n") ; exit(1) ;
        }
        if( do_automask ){
          fprintf(stderr,"*** Can't have -mask and -automask!\n") ; exit(1) ;
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
        mmm = THD_makemask( mask_dset , 0 , 1.0,0.0 ) ;
        nvox_mask = DSET_NVOX(mask_dset) ;
        nmask = THD_countmask( nvox_mask , mmm ) ;
        if( mmm == NULL || nmask <= 0 ){
          fprintf(stderr,"*** Can't make mask from dataset %s\n",argv[narg-1]);
          exit(1) ;
        }
        DSET_delete( mask_dset ) ;
        narg++ ; continue ;
      }

      if( strcmp(argv[narg],"-automask") == 0 ){
        if( mmm != NULL ){
          fprintf(stderr,"*** Can't have -mask and -automask!\n") ; exit(1) ;
        }
        do_automask = 1 ; narg++ ; continue ;
      }

      fprintf(stderr,"*** Unknown option: %s\n",argv[narg]) ; exit(1) ;
   }

   /* should have at least 1 more argument */

   if( argc <= narg ){
     fprintf(stderr,"*** No input dataset!?\n") ; exit(1) ;
   }

   for( ; narg < argc ; narg++ ){
     xset = THD_open_dataset( argv[narg] ) ;
     if( xset == NULL ){
       fprintf(stderr,"+++ Can't open dataset %s -- skipping\n",argv[narg]);
       continue ;
     }
     DSET_load(xset) ;
     if( !DSET_LOADED(xset) ){
       fprintf(stderr,"+++ Can't load dataset %s -- skipping\n",argv[narg]);
       DSET_delete(xset) ; continue ;
     }
     if( do_automask ){
       if( mmm != NULL ){ free(mmm); mmm = NULL; }
       mmm = THD_automask(xset) ;
       nvox_mask = DSET_NVOX(xset) ;
       nmask = THD_countmask( nvox_mask , mmm ) ;
       if( mmm == NULL || nmask <= 0 ){
         fprintf(stderr,"+++ Can't make automask from dataset %s -- skipping\n",argv[narg]) ;
         DSET_delete(xset) ; continue ;
       }
     }
     nxyz = DSET_NVOX(xset) ;
     if( mmm != NULL && nxyz != nvox_mask ){
       fprintf(stderr,"+++ Mask/Dataset grid size mismatch at %s\n -- skipping\n",argv[narg]) ;
       DSET_delete(xset) ; continue ;
     }
     for( iv=0 ; iv < DSET_NVALS(xset) ; iv++ ){
       cmv = THD_cmass( xset , iv , mmm ) ;
       printf("%g  %g  %g\n",cmv.xyz[0],cmv.xyz[1],cmv.xyz[2]) ;
       DSET_unload_one(xset,iv) ;
     }
     DSET_delete(xset) ;
  }
}

/*----------------------------------------------------------------------*/
/*! Get the center of mass of this volume, in DICOM coords.
------------------------------------------------------------------------*/

THD_fvec3 THD_cmass( THD_3dim_dataset *xset , int iv , byte *mmm )
{
   THD_fvec3 cmv ;
   MRI_IMAGE *im ;
   float *far , icm,jcm,kcm ;
   int ii , nvox ;

   LOAD_FVEC3(cmv,0,0,0) ;

   nvox = DSET_NVOX(xset) ;
   im   = mri_to_float( DSET_BRICK(xset,iv) ) ;
                             if( im  == NULL ) return cmv ;
   far = MRI_FLOAT_PTR(im) ; if( far == NULL ) return cmv ;

   if( mmm != NULL ){
     for( ii=0 ; ii < nvox ; ii++ )
       if( mmm[ii] ) far[ii] = 0.0 ;
   }

   mri_get_cmass_3D( im , &icm,&jcm,&kcm ) ; mri_free(im) ;
   LOAD_FVEC3(cmv,icm,jcm,kcm) ;
   cmv = THD_3dfind_to_3dmm( xset , cmv ) ;
   cmv = THD_3dmm_to_dicomm( xset , cmv ) ;
   return cmv ;
}
