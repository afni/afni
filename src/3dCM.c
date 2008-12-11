#include "mrilib.h"

THD_fvec3 THD_cmass( THD_3dim_dataset *xset , int iv , byte *mmm ) ;

int main( int argc , char * argv[] )
{
   int narg=1, do_automask=0 , iv , nxyz , do_set=0 ;
   THD_3dim_dataset *xset ;
   byte *mmm=NULL ; int nmask=0 , nvox_mask=0 ;
   THD_fvec3 cmv , setv ;
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
             "  -set x y z   After computing the CM of the dataset, set the\n"
             "                 origin fields in the header so that the CM\n"
             "                 will be at (x,y,z) in DICOM coords.\n"
            ) ;
      PRINT_COMPILE_DATE ; exit(0) ;
   }

   LOAD_FVEC3(setv,0,0,0) ;   /* ZSS: To quiet init. warnings */
   narg = 1 ;
   while( narg < argc && argv[narg][0] == '-' ){

      if( strcmp(argv[narg],"-set") == 0 ){
        float xset,yset,zset ;
        if( narg+3 >= argc ){
          fprintf(stderr,"*** -set need 3 args following!\n") ; exit(1) ;
        }
        xset = strtod( argv[++narg] , NULL ) ;
        yset = strtod( argv[++narg] , NULL ) ;
        zset = strtod( argv[++narg] , NULL ) ;
        LOAD_FVEC3(setv,xset,yset,zset) ; do_set = 1 ;
        THD_set_write_compression(COMPRESS_NONE); /* do not alter compression*/
        narg++ ; continue ;
      }

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
        CHECK_OPEN_ERROR(mask_dset,argv[narg]) ;
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
         fprintf( stderr,
                  "+++ Can't make automask from dataset %s "
                  "-- skipping\n",argv[narg]) ;
         DSET_delete(xset) ; continue ;
       }
     }
     nxyz = DSET_NVOX(xset) ;
     if( mmm != NULL && nxyz != nvox_mask ){
       fprintf(stderr,"+++ Mask/Dataset grid size mismatch at %s\n -- skipping\n",argv[narg]) ;
       DSET_delete(xset) ; continue ;
     }

     cmv = THD_cmass( xset , 0 , mmm ) ;
     printf("%g  %g  %g\n",cmv.xyz[0],cmv.xyz[1],cmv.xyz[2]) ;
      DSET_unload(xset) ;

     if( do_set ){
       THD_fvec3 dv , ov ;
       if(  DSET_IS_MASTERED(xset) ){
         fprintf(stderr,"+++ Can't modify CM of dataset %s\n",argv[narg]) ;
       } else {
         LOAD_FVEC3(ov,DSET_XORG(xset),DSET_YORG(xset),DSET_ZORG(xset)) ;
         ov = THD_3dmm_to_dicomm( xset , ov ) ;
         dv = SUB_FVEC3(setv,cmv) ;
         ov = ADD_FVEC3(dv,ov) ;
         ov = THD_dicomm_to_3dmm( xset , ov ) ;
         xset->daxes->xxorg = ov.xyz[0] ;
         xset->daxes->yyorg = ov.xyz[1] ;
         xset->daxes->zzorg = ov.xyz[2] ;
         /* allow overwriting header for all types of output data */
         putenv("AFNI_DECONFLICT=OVERWRITE") ;
         tross_Make_History( "3dCM" , argc,argv , xset ) ; /* ZSS   Dec. 09 08 */
	 if(DSET_IS_BRIK(xset)) {
           INFO_message("Rewriting header %s",DSET_HEADNAME(xset)) ;
           DSET_overwrite_header( xset ) ;
	 }
	 else {     /* for other dataset types like NIFTI, rewrite whole dset */
	    DSET_load( xset ) ;
       DSET_overwrite(xset) ;
       INFO_message("Wrote new dataset: %s",DSET_BRIKNAME(xset)) ;
	 }   
      }
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
