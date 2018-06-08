#include "mrilib.h"

#ifdef USE_OMP
#include <omp.h>
#include "mri_blur3d_variable.c"
#endif

#define MASK_MIN 9

#define FFAC 0.045084f

static int verb = 1 ;

extern short * UniqueShort( short *y, int ysz, int *kunq, int Sorted ) ;

/*----------------------------------------------------------------------------*/

int main( int argc , char *argv[] )
{
   int iarg=1 , ii,nvox , nvals ;
   THD_3dim_dataset *inset=NULL, *outset=NULL , *mset=NULL ;
   char *prefix="./blurinmask" ;
   float fwhm_goal=0.0f ; int fwhm_2D=0 ;
   byte *mask=NULL ; int mask_nx=0,mask_ny=0,mask_nz=0 , automask=0 , nmask=0 ;
   float dx,dy,dz=0.0f , *bar , val ;
   int floatize=0 ;    /* 18 May 2009 */

   MRI_IMAGE *immask=NULL ;    /* 07 Oct 2009 */
   short      *mmask=NULL ;
   short      *unval_mmask=NULL ; int nuniq_mmask=0 ;
   int do_preserve=0 , use_qsar ;         /* 19 Oct 2009 */

   THD_3dim_dataset *fwhmset=NULL ;
   MRI_IMAGE *fxim=NULL, *fyim=NULL, *fzim=NULL ; /* 13 Jun 2016 */
   int niter_fxyz=0 ; float dmax=0.0f , dmin=0.0f ;

   /*------- help the pitifully ignorant luser? -------*/

   AFNI_SETUP_OMP(0) ;  /* 24 Jun 2013 */

   if( argc < 2 || strcmp(argv[1],"-help") == 0 ){
     printf(
      "Usage: ~1~\n"
      "3dBlurInMask [options]\n"
      "Blurs a dataset spatially inside a mask. That's all. Experimental.\n"
      "\n"
      "OPTIONS ~1~\n"
      "-------\n"
      " -input  ddd = This required 'option' specifies the dataset\n"
      "               that will be smoothed and output.\n"
      " -FWHM   f   = Add 'f' amount of smoothness to the dataset (in mm).\n"
      "              **N.B.: This is also a required 'option'.\n"
      " -FWHMdset d = Read in dataset 'd' and add the amount of smoothness\n"
      "               given at each voxel -- spatially variable blurring.\n"
      "              ** EXPERIMENTAL EXPERIMENTAL EXPERIMENTAL **\n"
      " -mask   mmm = Mask dataset, if desired.  Blurring will\n"
      "               occur only within the mask.  Voxels NOT in\n"
      "               the mask will be set to zero in the output.\n"
      " -Mmask  mmm = Multi-mask dataset -- each distinct nonzero\n"
      "               value in dataset 'mmm' will be treated as\n"
      "               a separate mask for blurring purposes.\n"
      "              **N.B.: 'mmm' must be byte- or short-valued!\n"
      " -automask   = Create an automask from the input dataset.\n"
      "              **N.B.: only 1 masking option can be used!\n"
      " -preserve   = Normally, voxels not in the mask will be\n"
      "               set to zero in the output.  If you want the\n"
      "               original values in the dataset to be preserved\n"
      "               in the output, use this option.\n"
      " -prefix ppp = Prefix for output dataset will be 'ppp'.\n"
      "              **N.B.: Output dataset is always in float format.\n"
      " -quiet      = Don't be verbose with the progress reports.\n"
      " -float      = Save dataset as floats, no matter what the\n"
      "               input data type is.\n"
      "              **N.B.: If the input dataset is unscaled shorts, then\n"
      "                      the default is to save the output in short\n"
      "                      format as well.  In EVERY other case, the\n"
      "                      program saves the output as floats. Thus,\n"
      "                      the ONLY purpose of the '-float' option is to\n"
      "                      force an all-shorts input dataset to be saved\n"
      "                      as all-floats after blurring.\n"
      "\n"
      "NOTES ~1~\n"
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

     PRINT_AFNI_OMP_USAGE("3dBlurInMask",NULL) ;
     PRINT_COMPILE_DATE ; exit(0) ;
   }

   /*---- official startup ---*/

   PRINT_VERSION("3dBlurInMask"); mainENTRY("3dBlurInMask main"); machdep();
   AFNI_logger("3dBlurInMask",argc,argv); AUTHOR("RW Cox") ;

   /*---- loop over options ----*/

   while( iarg < argc && argv[iarg][0] == '-' ){

     if( strncmp(argv[iarg],"-preserve",5) == 0 ){  /* 19 Oct 2009 */
       do_preserve = 1 ; iarg++ ; continue ;
     }

     if( strncmp(argv[iarg],"-qui",4) == 0 ){
       verb = 0 ; iarg++ ; continue ;
     }
     if( strncmp(argv[iarg],"-ver",4) == 0 ){
       verb++ ; iarg++ ; continue ;
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

     if( strcasecmp(argv[iarg],"-Mmask") == 0 ){   /* 07 Oct 2009 */
       if( ++iarg >= argc ) ERROR_exit("Need argument after '-Mmask'") ;
       if( mmask != NULL || mask != NULL || automask ) ERROR_exit("Can't have two mask inputs") ;
       mset = THD_open_dataset( argv[iarg] ) ;
       CHECK_OPEN_ERROR(mset,argv[iarg]) ;
       DSET_load(mset) ; CHECK_LOAD_ERROR(mset) ;
       mask_nx = DSET_NX(mset); mask_ny = DSET_NY(mset); mask_nz = DSET_NZ(mset);
#if 0
       if( !MRI_IS_INT_TYPE(DSET_BRICK_TYPE(mset,0)) )
         ERROR_exit("-Mmask dataset is not integer type!") ;
#endif
       immask = mri_to_short( 1.0 , DSET_BRICK(mset,0) ) ;
       mmask  = MRI_SHORT_PTR(immask) ;
       unval_mmask = UniqueShort( mmask, mask_nx*mask_ny*mask_nz, &nuniq_mmask, 0 ) ;
       if( unval_mmask == NULL || nuniq_mmask == 0 )
         ERROR_exit("-Mmask dataset cannot be processed!?") ;
       if( nuniq_mmask == 1 && unval_mmask[0] == 0 )
         ERROR_exit("-Mmask dataset is all zeros!?") ;
       if( verb ){
         int qq , ww ;
         for( ii=qq=0 ; ii < nuniq_mmask ; ii++ ) if( unval_mmask[ii] != 0 ) qq++ ;
         for( ii=ww=0 ; ii < immask->nvox ; ii++ ) if( mmask[ii] != 0 ) ww++ ;
         INFO_message("%d unique nonzero values in -Mmask; %d nonzero voxels",qq,ww) ;
       }
       iarg++ ; continue ;
     }

     if( strcmp(argv[iarg],"-mask") == 0 ){
       if( ++iarg >= argc ) ERROR_exit("Need argument after '-mask'") ;
       if( mmask != NULL || mask != NULL || automask ) ERROR_exit("Can't have two mask inputs") ;
       mset = THD_open_dataset( argv[iarg] ) ;
       CHECK_OPEN_ERROR(mset,argv[iarg]) ;
       DSET_load(mset) ; CHECK_LOAD_ERROR(mset) ;
       mask_nx = DSET_NX(mset); mask_ny = DSET_NY(mset); mask_nz = DSET_NZ(mset);
       mask = THD_makemask( mset , 0 , 0.5f, 0.0f ) ; DSET_unload(mset) ;
       if( mask == NULL ) ERROR_exit("Can't make mask from dataset '%s'",argv[iarg]) ;
       ii = THD_mask_remove_isolas( mask_nx,mask_ny,mask_nz , mask ) ;
       if( verb && ii > 0 ) INFO_message("Removed %d isola%s from mask dataset",ii,(ii==1)?"\0":"s") ;
       nmask = THD_countmask( mask_nx*mask_ny*mask_nz , mask ) ;
       if( verb ) INFO_message("Number of voxels in mask = %d",nmask) ;
       if( nmask < MASK_MIN ) ERROR_exit("Mask is too small to process") ;
       iarg++ ; continue ;
     }

     if( strcmp(argv[iarg],"-automask") == 0 ){
       if( mmask != NULL || mask != NULL ) ERROR_exit("Can't have 2 mask inputs") ;
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

     if( strcasecmp(argv[iarg],"-FWHMdset") == 0 ){
       if( ++iarg >= argc ) ERROR_exit("Need argument after '%s'",argv[iarg-1]);
       if( fwhmset != NULL ) ERROR_exit("You can't use option '-FWHMdset' twice :(") ;
       fwhmset = THD_open_dataset( argv[iarg] ) ;
       CHECK_OPEN_ERROR(fwhmset,argv[iarg]) ;
       do_preserve = 1 ; iarg++ ; continue ;
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

   if( fwhmset == NULL && fwhm_goal == 0.0f )
     ERROR_exit("No -FWHM option given! What do you want?") ;

   if( fwhmset != NULL && fwhm_goal > 0.0f ){
     WARNING_message("-FWHMdset option replaces -FWHM value") ;
     fwhm_goal = 0.0f ;
   }

   if( fwhmset != NULL && mmask != NULL )
     ERROR_exit("Sorry: -FWHMdset and -Mmask don't work together (yet)") ;

   if( inset == NULL ){
     if( iarg >= argc ) ERROR_exit("No input dataset on command line?") ;
     inset = THD_open_dataset( argv[iarg] ) ;
     CHECK_OPEN_ERROR(inset,argv[iarg]) ;
   }

   nvox = DSET_NVOX(inset)     ;
   dx   = fabs(DSET_DX(inset)) ; if( dx == 0.0f ) dx = 1.0f ;
   dy   = fabs(DSET_DY(inset)) ; if( dy == 0.0f ) dy = 1.0f ;
   dz   = fabs(DSET_DZ(inset)) ; if( dz == 0.0f ) dz = 1.0f ;

   dmax = MAX(dx,dy) ; if( dmax < dz ) dmax = dz ;  /* 13 Jun 2016 */
   dmin = MIN(dx,dy) ; if( dmin > dz ) dmin = dz ;

   if( !floatize ){    /* 18 May 2009 */
     if( !THD_datum_constant(inset->dblk)     ||
         THD_need_brick_factor(inset)         ||
         DSET_BRICK_TYPE(inset,0) != MRI_short  ){
       if( verb ) INFO_message("forcing output to be stored in float format") ;
       floatize = 1 ;
     } else {
       if( verb ) INFO_message("output dataset will be stored as shorts") ;
     }
   } else {
       if( verb ) INFO_message("output dataset will be stored as floats") ;
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

   } else if( mmask != NULL ){
     if( mask_nx != DSET_NX(inset) ||
         mask_ny != DSET_NY(inset) ||
         mask_nz != DSET_NZ(inset)   )
       ERROR_exit("-Mmask dataset grid doesn't match input dataset") ;

   } else {
     mask = (byte *)malloc(sizeof(byte)*nvox) ; nmask = nvox ;
     memset(mask,1,sizeof(byte)*nvox) ;
     if( verb ) INFO_message("No mask ==> processing all %d voxels",nvox);
   }

   /*--- process FWHMdset [13 Jun 2016] ---*/

   if( fwhmset != NULL ){
     float *fxar,*fyar,*fzar , *fwar ; MRI_IMAGE *fwim ;
     float fwmax=0.0f , fsx,fsy,fsz ; int ii, nfpos=0 ;

     if( DSET_NX(inset) != DSET_NX(fwhmset) ||
         DSET_NY(inset) != DSET_NY(fwhmset) ||
         DSET_NZ(inset) != DSET_NZ(fwhmset)   )
       ERROR_exit("grid dimensions for FWHMdset and input dataset do not match :(") ;

STATUS("get fwim") ;
     DSET_load(fwhmset) ;
     fwim = mri_scale_to_float(DSET_BRICK_FACTOR(fwhmset,0),DSET_BRICK(fwhmset,0));
     fwar = MRI_FLOAT_PTR(fwim);
     DSET_unload(fwhmset) ;
STATUS("process fwar") ;
     for( ii=0 ; ii < nvox ; ii++ ){
       if( mask[ii] && fwar[ii] > 0.0f ){
         nfpos++ ;
         if( fwar[ii] > fwmax ) fwmax = fwar[ii] ;
       } else {
         fwar[ii] = 0.0f ; mask[ii] = 0 ;
       }
     }
     if( nfpos < 100 )
       ERROR_exit("Cannot proceed: too few (%d) voxels are positive in -FWHMdset!",nfpos) ;

     niter_fxyz = (int)rintf(2.0f*fwmax*fwmax*FFAC/(0.05f*dmin*dmin)) + 1 ;

/** INFO_message("niter_fxyz = %d",niter_fxyz) ; **/

STATUS("create fxim etc.") ;

     fxim = mri_new_conforming(fwim,MRI_float); fxar = MRI_FLOAT_PTR(fxim);
     fyim = mri_new_conforming(fwim,MRI_float); fyar = MRI_FLOAT_PTR(fyim);
     fzim = mri_new_conforming(fwim,MRI_float); fzar = MRI_FLOAT_PTR(fzim);

     fsx = FFAC/(dx*dx*niter_fxyz) ;
     fsy = FFAC/(dy*dy*niter_fxyz) ;
     fsz = FFAC/(dz*dz*niter_fxyz) ;
/** INFO_message("fsx=%g fsy=%g fsz=%g",fsx,fsy,fsz) ; **/

     for( ii=0 ; ii < nvox ; ii++ ){
       if( fwar[ii] > 0.0f ){
         fxar[ii] = fwar[ii]*fwar[ii] * fsx ;
         fyar[ii] = fwar[ii]*fwar[ii] * fsy ;
         fzar[ii] = fwar[ii]*fwar[ii] * fsz ;
       } else {
         fxar[ii] = fyar[ii] = fzar[ii] = 0.0f ;
       }
     }

STATUS("free(fwim)") ;
     mri_free(fwim) ;
   }

   /*--- process input dataset ---*/

STATUS("load input") ;

   DSET_load(inset) ; CHECK_LOAD_ERROR(inset) ;

   outset = EDIT_empty_copy( inset ) ;   /* moved here 04 Jun 2007 */
   EDIT_dset_items( outset , ADN_prefix , prefix , ADN_none ) ;
   EDIT_dset_items( outset , ADN_brick_fac , NULL , ADN_none ) ;  /* 11 Sep 2007 */
   tross_Copy_History( inset , outset ) ;
   tross_Make_History( "3dBlurInMask" , argc,argv , outset ) ;

   nvals = DSET_NVALS(inset) ;

   use_qsar = (do_preserve || mmask != NULL) ; /* 19 Oct 20090 */

 AFNI_OMP_START ;
#pragma omp parallel if( nvals > 1 )
 {
   MRI_IMAGE *dsim ; int ids,qit ; byte *qmask=NULL ; register int vv ;
   MRI_IMAGE *qim=NULL, *qsim=NULL; float *qar=NULL, *dsar, *qsar=NULL;
#pragma omp critical (MALLOC)
   { if( use_qsar ){
       qsim  = mri_new_conforming(DSET_BRICK(inset,0),MRI_float); qsar = MRI_FLOAT_PTR(qsim);
     }
     if( mmask != NULL ){
       qmask = (byte *)malloc(sizeof(byte)*nvox) ;
       qim   = mri_new_conforming(immask,MRI_float); qar  = MRI_FLOAT_PTR(qim);
       qim->dx = dx ; qim->dy = dy ; qim->dz = dz ;
     }
   }
#pragma omp for
   for( ids=0 ; ids < nvals ; ids++ ){
#pragma omp critical (MALLOC)
     { dsim = mri_scale_to_float(DSET_BRICK_FACTOR(inset,ids),DSET_BRICK(inset,ids));
       DSET_unload_one(inset,ids) ;
     }
     dsim->dx = dx ; dsim->dy = dy ; dsim->dz = dz ; dsar = MRI_FLOAT_PTR(dsim) ;

     /* if needed, initialize qsar with data to be preserved in output */

     if( do_preserve ){
       for( vv=0 ; vv < nvox ; vv++ ) qsar[vv] = dsar[vv] ;
     } else if( mmask != NULL ){
       for( vv=0 ; vv < nvox ; vv++ ) qsar[vv] = 0.0f ;
     }

     if( fwhmset != NULL ){       /* 13 Jun 2016: spatially variable blurring */

       for( qit=0 ; qit < niter_fxyz ; qit++ ){
         mri_blur3D_variable( dsim , mask , fxim,fyim,fzim ) ;
       }
       if( do_preserve ){
         for( vv=0 ; vv < nvox ; vv++ ) if( mask[vv] ) qsar[vv] = dsar[vv] ;
       }

     } else if( mmask != NULL ){         /* 07 Oct 2009: multiple masks */
       int qq ; register short uval ;
       for( qq=0 ; qq < nuniq_mmask ; qq++ ){
         uval = unval_mmask[qq] ; if( uval == 0 ) continue ;
         for( vv=0 ; vv < nvox ; vv++ ) qmask[vv] = (mmask[vv]==uval) ; /* make mask */
         (void)THD_mask_remove_isolas( mask_nx,mask_ny,mask_nz , qmask ) ;
         nmask = THD_countmask( nvox , qmask ) ;
         if( verb && ids==0 ) ININFO_message("voxels in Mmask[%d] = %d",uval,nmask) ;
         if( nmask >= MASK_MIN ){
           /* copy data from dataset to qar */
           for( vv=0 ; vv < nvox ; vv++ ) if( qmask[vv] ) qar[vv] = dsar[vv] ;
           /* blur qar (output will be zero where qmask==0) */
           mri_blur3D_addfwhm( qim , qmask , fwhm_goal ) ;  /** the real work **/
           /* copy results back to qsar */
           for( vv=0 ; vv < nvox ; vv++ ) if( qmask[vv] ) qsar[vv] = qar[vv] ;
         }
       }

     } else {                      /* the olden way: 1 mask */

       mri_blur3D_addfwhm( dsim , mask , fwhm_goal ) ;  /** all the work **/

       /* dsim will be zero where mask==0;
          if we want to preserve the input values, copy dsar into qsar now
          at all mask!=0 voxels, since qsar contains the original data values */

       if( do_preserve ){
         for( vv=0 ; vv < nvox ; vv++ ) if( mask[vv] ) qsar[vv] = dsar[vv] ;
       }

     }

     /* if necessary, copy combined results in qsar to dsar for output */

     if( use_qsar ){
       for( vv=0 ; vv < nvox ; vv++ ) dsar[vv] = qsar[vv] ;
     }

     if( floatize ){
       EDIT_substitute_brick( outset , ids , MRI_float , dsar ) ;
     } else {
#pragma omp critical (MALLOC)
       { EDIT_substscale_brick( outset , ids , MRI_float , dsar ,
                                               MRI_short , 1.0f  ) ;
         mri_free(dsim) ;
       }
     }
   } /* end of loop over sub-bricks */

#pragma omp critical (MALLOC)
   { if( qsim   != NULL ) mri_free(qsim);
     if( immask != NULL ){ free(qmask); mri_free(qim); }
   }
 } /* end OpenMP */
 AFNI_OMP_END ;

   if(   mask != NULL )     free(  mask) ;
   if( immask != NULL ) mri_free(immask) ;

   DSET_unload(inset) ;
   DSET_write(outset) ;
   WROTE_DSET(outset) ;
   exit(0) ;
}
