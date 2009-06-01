#include "mrilib.h"

#ifdef USE_OMP
#include <omp.h>
#endif

#define MASK_MIN 19

static int verb = 1 ;

/*----------------------------------------------------------------------------*/

int main( int argc , char *argv[] )
{
   int iarg=1 , ii,nvox , nvals ;
   THD_3dim_dataset *inset=NULL, *outset=NULL , *mset=NULL ;
   char *prefix="./blurinmask" ;
   float fwhm_goal=0.0f ; int fwhm_2D=0 ;
   byte *mask=NULL ; int mask_nx=0,mask_ny=0,mask_nz=0 , automask=0 , nmask ;
   float dx,dy,dz=0.0f , *bar , val ;
   int floatize=0 ;    /* 18 May 2009 */

   /*------- help the pitifully ignorant luser? -------*/

   if( argc < 2 || strcmp(argv[1],"-help") == 0 ){
     printf(
      "Usage: 3dBlurInMask [options]\n"
      "Blurs a dataset spatially inside a mask.  That's all.  Experimental.\n"
      "\n"
      "OPTIONS\n"
      "-------\n"
      " -input  ddd = This required 'option' specifies the dataset\n"
      "               that will be smoothed and output.\n"
      " -FWHM   f   = Add this amount of smoothness to the dataset.\n"
      "              **N.B.: This is also a required 'option'.\n"
      " -prefix ppp = Prefix for output dataset will be 'ppp'.\n"
      "              **N.B.: Output dataset is always in float format.\n"
      " -mask   mmm = Mask dataset, if desired.  Blurring will\n"
      "               occur only within the mask.  Voxels NOT in\n"
      "               the mask will be set to zero in the output.\n"
      " -automask   = Create an automask from the input dataset.\n"
      "              **N.B.: -automask and -mask can't be combined.\n"
      " -float      = Save dataset as floats, no matter what the\n"
      "               input data type is.\n"
      "              **N.B.: If the input dataset is unscaled shorts, then\n"
      "                      the default is to save the output in short\n"
      "                      format as well.  In every other case, the\n"
      "                      default is to save the output as floats.\n"
      "                      Thus, the ONLY purpose of the '-float' option\n"
      "                      is to force an all-shorts input dataset to be\n"
      "                      saved as all-floats after blurring.\n"
      "\n"
      "NOTES\n"
      "-----\n"
      " * If you don't provide a mask, then all voxels will be included\n"
      "     in the blurring.  (But then why are you using this program?)\n"
      " * Note that voxels inside the mask that are not contiguous with\n"
      "     any other voxels inside the mask will not be modified at all!\n"
      " * Works iteratively, similarly to 3dBlurToFWHM, but without\n"
      "     the extensive overhead of monitoring the smoothness.\n"
      " * But this program will be faster than 3dBlurToFWHM, and probably\n"
      "     slower than 3dmerge.\n"
      " * Since the blurring is done iteratively, rather than all-at-once as\n"
      "     in 3dmerge, the results will be slightly different than 3dmerge's,\n"
      "     even if no mask is used here (3dmerge, of course, doesn't take a mask).\n"
      " * If the original FWHM of the dataset was 'S' and you input a value\n"
      "     'F' with the '-FWHM' option, then the output dataset's smoothness\n"
      "     will be about sqrt(S*S+F*F).  The number of iterations will be\n"
      "     about (F*F/d*d) where d=grid spacing; this means that a large value\n"
      "     of F might take a lot of CPU time!\n"
      " * The spatial smoothness of a 3D+time dataset can be estimated with a\n"
      "     command similar to the following:\n"
      "          3dFWHMx -detrend -mask mmm+orig -input ddd+orig\n"
     ) ;
     printf(
      " * The minimum number of voxels in the mask is %d\n",MASK_MIN) ;
     printf(
      " * Isolated voxels will be removed from the mask!\n") ;
#ifdef USE_OMP
     printf(
      " * This version of the program is compiled with OpenMP support, which\n"
      "     will multi-thread the blurring across sub-bricks, for speed.\n"
      " * To disable multi-threading, 'setenv OMP_NUM_THREADS 1'\n"
     ) ;
#endif
     PRINT_COMPILE_DATE ; exit(0) ;
   }

   /*---- official startup ---*/

   PRINT_VERSION("3dBlurInMask"); mainENTRY("3dBlurInMask main"); machdep();
   AFNI_logger("3dBlurInMask",argc,argv);

   /*---- loop over options ----*/

   while( iarg < argc && argv[iarg][0] == '-' ){

     if( strncmp(argv[iarg],"-q",2) == 0 ){
       verb = 0 ; iarg++ ; continue ;
     }
     if( strncmp(argv[iarg],"-verb",4) == 0 ){
       verb = 1 ; iarg++ ; continue ;
     }

     if( strcmp(argv[iarg],"-input") == 0 || strcmp(argv[iarg],"-dset") == 0 ){
       if( inset != NULL  ) ERROR_exit("Can't have two -input options") ;
       if( ++iarg >= argc ) ERROR_exit("Need argument after '-input'") ;
       inset = THD_open_dataset( argv[iarg] );
       CHECK_OPEN_ERROR(inset,argv[iarg]) ;
       iarg++ ; continue ;
     }

     if( strcmp(argv[iarg],"-prefix") == 0 ){
       if( ++iarg >= argc ) ERROR_exit("Need argument after '-prefix'") ;
       prefix = strdup(argv[iarg]) ;
       if( !THD_filename_ok(prefix) ) ERROR_exit("Bad name after '-prefix'") ;
       iarg++ ; continue ;
     }

     if( strcmp(argv[iarg],"-mask") == 0 ){
       if( ++iarg >= argc ) ERROR_exit("Need argument after '-mask'") ;
       if( mask != NULL || automask ) ERROR_exit("Can't have two mask inputs") ;
       mset = THD_open_dataset( argv[iarg] ) ;
       CHECK_OPEN_ERROR(mset,argv[iarg]) ;
       DSET_load(mset) ; CHECK_LOAD_ERROR(mset) ;
       mask_nx = DSET_NX(mset); mask_ny = DSET_NY(mset); mask_nz = DSET_NZ(mset);
       mask = THD_makemask( mset , 0 , 0.5f, 0.0f ) ; DSET_unload(mset) ;
       if( mask == NULL ) ERROR_exit("Can't make mask from dataset '%s'",argv[iarg]) ;
       ii = THD_mask_remove_isolas( mask_nx,mask_ny,mask_nz , mask ) ;
       if( ii > 0 ) INFO_message("Removed %d isola%s from mask dataset",ii,(ii==1)?"\0":"s") ;
       nmask = THD_countmask( mask_nx*mask_ny*mask_nz , mask ) ;
       if( verb ) INFO_message("Number of voxels in mask = %d",nmask) ;
       if( nmask < MASK_MIN ) ERROR_exit("Mask is too small to process") ;
       iarg++ ; continue ;
     }

     if( strcmp(argv[iarg],"-automask") == 0 ){
       if( mask != NULL ) ERROR_exit("Can't have -automask and -mask") ;
       automask = 1 ; iarg++ ; continue ;
     }

     if( strcasecmp(argv[iarg],"-FWHM") == 0 || strcasecmp(argv[iarg],"-FHWM") == 0 ){
       if( ++iarg >= argc ) ERROR_exit("Need argument after '%s'",argv[iarg-1]);
       val = (float)strtod(argv[iarg],NULL) ;
       if( val <= 0.0f ) ERROR_exit("Illegal value after '%s': '%s'",
                                    argv[iarg-1],argv[iarg]) ;
       fwhm_goal = val ; fwhm_2D = 0 ;
       iarg++ ; continue ;
     }

     if( strncmp(argv[iarg],"-float",6) == 0 ){    /* 18 May 2009 */
       floatize = 1 ; iarg++ ; continue ;
     }

#if 0
     if( strcmp(argv[iarg],"-FWHMxy") == 0 || strcmp(argv[iarg],"-FHWMxy") == 0 ){
       if( ++iarg >= argc ) ERROR_exit("Need argument after '%s'",argv[iarg-1]);
       val = (float)strtod(argv[iarg],NULL) ;
       if( val <= 0.0f ) ERROR_exit("Illegal value after '%s': '%s'",
                                    argv[iarg-1],argv[iarg]) ;
       fwhm_goal = val ; fwhm_2D = 1 ;
       iarg++ ; continue ;
     }
#endif

     ERROR_exit("Uknown option '%s'",argv[iarg]) ;

   } /*--- end of loop over options ---*/

   /*----- check for stupid inputs, load datasets, et cetera -----*/

   if( fwhm_goal == 0.0f )
     ERROR_exit("No -FWHM option given! What do you want?") ;

   if( inset == NULL ){
     if( iarg >= argc ) ERROR_exit("No input dataset on command line?") ;
     inset = THD_open_dataset( argv[iarg] ) ;
     CHECK_OPEN_ERROR(inset,argv[iarg]) ;
   }
   nvox = DSET_NVOX(inset)     ;
   dx   = fabs(DSET_DX(inset)) ; if( dx == 0.0f ) dx = 1.0f ;
   dy   = fabs(DSET_DY(inset)) ; if( dy == 0.0f ) dy = 1.0f ;
   dz   = fabs(DSET_DZ(inset)) ; if( dz == 0.0f ) dz = 1.0f ;

   if( !floatize ){    /* 18 May 2009 */
     if( !THD_datum_constant(inset->dblk)     ||
         THD_need_brick_factor(inset)         ||
         DSET_BRICK_TYPE(inset,0) != MRI_short  ){
       INFO_message("forcing output to be stored in float format") ;
       floatize = 1 ;
     }
   }

#if 0
   if( DSET_NZ(inset) == 1 && !fwhm_2D ){
     WARNING_message("Dataset is 2D ==> switching from -FWHM to -FWHMxy") ;
     fwhm_2D = 1 ;
   }
#endif

   /*--- deal with mask or automask ---*/

   if( mask != NULL ){
     if( mask_nx != DSET_NX(inset) ||
         mask_ny != DSET_NY(inset) ||
         mask_nz != DSET_NZ(inset)   )

       ERROR_exit("-mask dataset grid doesn't match input dataset") ;

   } else if( automask ){
     mask = THD_automask( inset ) ;
     if( mask == NULL )
       ERROR_message("Can't create -automask from input dataset?") ;
     nmask = THD_countmask( DSET_NVOX(inset) , mask ) ;
     if( verb ) INFO_message("Number of voxels in automask = %d",nmask);
     if( nmask < MASK_MIN ) ERROR_exit("Automask is too small to process") ;

   } else {
     mask = (byte *)malloc(sizeof(byte)*nvox) ; nmask = nvox ;
     memset(mask,1,sizeof(byte)*nvox) ;
     if( verb ) INFO_message("No mask ==> processing all %d voxels",nvox);
   }

   /*--- process input dataset ---*/

   DSET_load(inset) ; CHECK_LOAD_ERROR(inset) ;

   outset = EDIT_empty_copy( inset ) ;   /* moved here 04 Jun 2007 */
   EDIT_dset_items( outset , ADN_prefix , prefix , ADN_none ) ;
   EDIT_dset_items( outset , ADN_brick_fac , NULL , ADN_none ) ;  /* 11 Sep 2007 */
   tross_Copy_History( inset , outset ) ;
   tross_Make_History( "3dBlurInMask" , argc,argv , outset ) ;

   nvals = DSET_NVALS(inset) ;
   verb  = verb && (nvals > 1) ;
   if( verb ) fprintf(stderr,"sub_bricks: ") ;

#pragma omp parallel if( nvals > 1 )
 {
   MRI_IMAGE *dsim ; int ids ;
 AFNI_OMP_START ;
#pragma omp for
   for( ids=0 ; ids < nvals ; ids++ ){
     if( verb ) fprintf(stderr,"%d.",ids) ;
#pragma omp critical (MALLOC)
     { dsim = mri_scale_to_float(DSET_BRICK_FACTOR(inset,ids),DSET_BRICK(inset,ids)); }
     DSET_unload_one(inset,ids) ;
     dsim->dx = dx ; dsim->dy = dy ; dsim->dz = dz ;

     mri_blur3D_addfwhm( dsim , mask , fwhm_goal ) ;  /** all the work **/

     if( floatize ){
       EDIT_substitute_brick( outset , ids , MRI_float , MRI_FLOAT_PTR(dsim) ) ;
     } else {
#pragma omp critical (MALLOC)
       { EDIT_substscale_brick( outset , ids , MRI_float , MRI_FLOAT_PTR(dsim) ,
                                               MRI_short , 1.0f ) ;
         mri_free(dsim) ;
       }
     }
   }
 AFNI_OMP_END ;
 } /* end OpenMP */
   if( verb ) fprintf(stderr,"\n") ;

   DSET_unload(inset) ;
   DSET_write(outset) ;
   WROTE_DSET(outset) ;
   exit(0) ;
}
