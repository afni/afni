#include "mrilib.h"

#define NTYPE_SPHERE 1
#define NTYPE_RECT   2

void estimate_blur_map( MRI_IMARR *bmar , byte *mask  , MCW_cluster *nbhd ,
                        float *fxar     , float *fyar , float *fzar        ) ;

/*----------------------------------------------------------------------------*/

int main( int argc , char *argv[] )
{
   int iarg=1 , ii,nvox ;
   THD_3dim_dataset *bmset=NULL, *inset=NULL, *outset=NULL ;
   char *prefix="./blurto" ;
   float fwhm_goal=0.0f ; int fwhm_2D=0 ;
   MCW_cluster *nbhd=NULL ;
   byte *mask=NULL ; int mask_nx,mask_ny,mask_nz , automask=0 , nmask ;
   int ntype=0 ; float na=0.0f,nb=0.0f,nc=0.0f ;
   MRI_IMARR *bmar ; MRI_IMAGE *bmed , *bmim ; int ibm ;
   MRI_IMARR *dsar ; MRI_IMAGE *dsim ;         int ids ;
   MRI_IMAGE *fxim=NULL , *fyim=NULL , *fzim=NULL ;
   float     *fxar=NULL , *fyar=NULL , *fzar=NULL ;
   float dx,dy,dz ;
   float gx,gy,gz , val , maxfxyz , maxfx,maxfy,maxfz ;
   int   nite , bmeqin , maxite=33 , numfxyz , nd,nblur , xdone,ydone,zdone ;
   float bx,by,bz ;
   float last_fwx=0.0f , last_fwy=0.0f , last_fwz=0.0f ;
   char *bsave_prefix=NULL ;
   THD_fvec3 fw ;

   /*-- help the pitifully ignorant luser? --*/

   if( argc < 2 || strcmp(argv[1],"-help") == 0 ){
     printf(
      "Usage: 3dBlurToFWHM [options]\n"
      "Blurs a 'master' dataset until it reaches a specified FWHM\n"
      "smoothness (approximately).  The same blurring schedule is\n"
      "applied to the input dataset to produce the output.\n"
      "\n"
      "Options\n"
      "-------\n"
      " -blurmaster bbb = This required 'option' specifies the dataset\n"
      "                   whose smoothness controls the process.\n"
      " -input      ddd = This required 'option' specifies the dataset\n"
      "                   that will be smoothed and output.\n"
      " -prefix     ppp = Prefix for output dataset will be 'ppp'.\n"
      " -mask       mmm = Mask dataset, if desired.  Blurring will\n"
      "                   occur only within the mask.  Voxels NOT in\n"
      "                   the mask will be set to zero in the output.\n"
      " -automask       = Create an automask from the input dataset.\n"
      " -FWHM       f   = Blur until the 3D FWHM is 'f'.\n"
      " -FWHMxy     f   = Blur until the 2D (x,y)-plane FWHM is 'f'.\n"
      "                   No blurring is done along the z-axis.\n"
      "           **N.B.: Note that you can't REDUCE the smoothness of a\n"
      "                   dataset.\n"
      "           **N.B.: Here, 'x', 'y', and 'z' refer to the grid/slice\n"
      "                   order as stored in the dataset, not DICOM coordinates!\n"
      " -nbhd       nnn = As in 3dLocalstat, specifies the neighborhood\n"
      "                   used to compute local smoothness.\n"
      "                   [Default = 'SPHERE(10)' = sphere of 10 mm radius.]\n"
      " -maxite     ccc = Set maximum number of iterations to 'ccc' [Default=33].\n"
      " -bsave      bbb = Save the blur map estimates at each iteration\n"
      "                   with dataset prefix 'bbb' [for debugging purposes].\n"
      "\n"
      "-blurmaster FILE RECOMMENDATIONS:\n"
      "For FMRI statistical purposes, you DO NOT want the FWHM to reflect\n"
      "  the spatial structure of the underlying anatomy.  Rather, you want\n"
      "  the FWHM to reflect the spatial structure of the noise.  This means\n"
      "  that the -blurmaster dataset should not have anatomical structure.  One\n"
      "  good form of input is the output of '3dDeconvolve -errts', which is\n"
      "  the residuals left over after the GLM fitted signal model is subtracted\n"
      "  out from each voxel's time series.\n"
      "You CAN give a multi-brick EPI dataset as the -blurmaster dataset; the\n"
      "  median of each voxel's time series will be removed (like the -demed option\n"
      "  in 3dFWHMx) which will tend to remove the spatial structure.  This makes\n"
      "  it practicable to make the input and blurmaster datasets be the same.\n"
      "\n"
      "ALSO SEE:\n"
      " - 3dFWHMx, which estimates smoothness globally\n"
      " - 3dLocalstat -stat FHWM, which estimates smoothness locally\n"
      "\n"
      "METHOD:\n"
      "The blurring is done by a conservative finite difference approximation\n"
      "to the diffusion equation\n"
      "  du/du = d/dx[ D_x(x,y,z) du/dx ] + d/dy[ D_y(x,y,z) du/dy ]\n"
      "                                   + d/dz[ D_z(x,y,z) du/dz ]\n"
      "        = div[ D(x,y,z) grad[u(x,y,z)] ]\n"
      "with Neumann (reflecting) boundary conditions at the edges of the mask\n"
      "(which ensures that voxel data inside and outside the mask don't mix).\n"
      "At each pseudo-time step, the FWHM is estimated globally (like '3dFWHMx')\n"
      "and locally (like '3dLocalstat -stat FWHM').  Voxels where the local FWHM\n"
      "goes past the goal will not be smoothed any more (D gets set to zero).\n"
      "When the global FWHM estimate reaches the goal, the program stops.\n"
      "It will also stop if progress stalls for some reason, or if the maximum\n"
      "iteration count is reached.  The output dataset will NOT have exactly\n"
      "the smoothness you ask for, but it will be close (we hope).\n"
      "\n"
      "-- The Glorious Emperor Zhark - Nov 2006\n"
     ) ;
     exit(0) ;
   }

   /*---- official startup ---*/

   PRINT_VERSION("3dBlurToFWHM"); mainENTRY("3dBlurToFWHM main"); machdep();
   AUTHOR("Emperor Zhark") ; AFNI_logger("3dBlurToFWHM",argc,argv);

   /*---- loop over options ----*/

   while( iarg < argc && argv[iarg][0] == '-' ){

     if( strcmp(argv[iarg],"-maxite") == 0 ){
       if( ++iarg >= argc ) ERROR_exit("Need argument after '-maxite'") ;
       maxite = (int)strtod(argv[iarg],NULL) ;
       if( maxite <= 0 ) maxite = 666 ;
       iarg++ ; continue ;
     }

     if( strcmp(argv[iarg],"-input") == 0 || strcmp(argv[iarg],"-dset") == 0 ){
       if( inset != NULL  ) ERROR_exit("Can't have two -input options") ;
       if( ++iarg >= argc ) ERROR_exit("Need argument after '-input'") ;
       inset = THD_open_dataset( argv[iarg] ) ;
       if( inset == NULL  ) ERROR_exit("Can't open dataset '%s'",argv[iarg]) ;
       iarg++ ; continue ;
     }

     if( strncmp(argv[iarg],"-blurm",6) == 0 ){
       if( bmset != NULL  ) ERROR_exit("Can't have two -blurmaster options") ;
       if( ++iarg >= argc ) ERROR_exit("Need argument after '-blurmaster'") ;
       bmset = THD_open_dataset( argv[iarg] ) ;
       if( bmset == NULL  ) ERROR_exit("Can't open dataset '%s'",argv[iarg]) ;
       iarg++ ; continue ;
     }

     if( strcmp(argv[iarg],"-prefix") == 0 ){
       if( ++iarg >= argc ) ERROR_exit("Need argument after '-prefix'") ;
       prefix = strdup(argv[iarg]) ;
       if( !THD_filename_ok(prefix) ) ERROR_exit("Bad name after '-prefix'") ;
       iarg++ ; continue ;
     }

     if( strcmp(argv[iarg],"-bsave") == 0 ){
       if( ++iarg >= argc ) ERROR_exit("Need argument after '-bsave'") ;
       bsave_prefix = strdup(argv[iarg]) ;
       if( !THD_filename_ok(bsave_prefix) ) ERROR_exit("Bad name after '-bsave'") ;
       iarg++ ; continue ;
     }

     if( strcmp(argv[iarg],"-mask") == 0 ){
       THD_3dim_dataset *mset ;
       if( ++iarg >= argc ) ERROR_exit("Need argument after '-mask'") ;
       if( mask != NULL || automask ) ERROR_exit("Can't have two mask inputs") ;
       mset = THD_open_dataset( argv[iarg] ) ;
       if( mset == NULL ) ERROR_exit("Can't open dataset '%s'",argv[iarg]) ;
       DSET_load(mset) ;
       if( !DSET_LOADED(mset) ) ERROR_exit("Can't load dataset '%s'",argv[iarg]) ;
       mask_nx = DSET_NX(mset); mask_ny = DSET_NY(mset); mask_nz = DSET_NZ(mset);
       mask = THD_makemask( mset , 0 , 0.5f, 0.0f ) ; DSET_delete(mset) ;
       if( mask == NULL ) ERROR_exit("Can't make mask from dataset '%s'",argv[iarg]) ;
       nmask = THD_countmask( mask_nx*mask_ny*mask_nz , mask ) ;
       INFO_message("Number of voxels in mask = %d",nmask) ;
       if( nmask < 333 ) ERROR_exit("Mask is too small to process") ;
       iarg++ ; continue ;
     }

     if( strcmp(argv[iarg],"-automask") == 0 ){
       if( mask != NULL ) ERROR_exit("Can't have -automask and -mask") ;
       automask = 1 ; iarg++ ; continue ;
     }

     if( strcmp(argv[iarg],"-nbhd") == 0 ){
       char *cpt ;
       if( ntype  >  0    ) ERROR_exit("Can't have 2 '-nbhd' options") ;
       if( ++iarg >= argc ) ERROR_exit("Need argument after '-nbhd'") ;

       cpt = argv[iarg] ;
       if( strncasecmp(cpt,"SPHERE",6) == 0 ){
         sscanf( cpt+7 , "%f" , &na ) ;
         if( na == 0.0f ) ERROR_exit("Can't have a SPHERE of radius 0") ;
         ntype = NTYPE_SPHERE ;
       } else if( strncasecmp(cpt,"RECT",4) == 0 ){
         sscanf( cpt+5 , "%f,%f,%f" , &na,&nb,&nc ) ;
         if( na == 0.0f && nb == 0.0f && nc == 0.0f )
           ERROR_exit("'RECT(0,0,0)' is not a legal neighborhood") ;
         ntype = NTYPE_RECT ;
       } else {
           ERROR_exit("Unknown -nbhd shape: '%s'",cpt) ;
       }
       iarg++ ; continue ;
     }

     if( strcmp(argv[iarg],"-FWHM") == 0 || strcmp(argv[iarg],"-FHWM") == 0 ){
       float val ;

       if( ++iarg >= argc ) ERROR_exit("Need argument after '%s'",argv[iarg-1]);
       val = (float)strtod(argv[iarg],NULL) ;
       if( val <= 0.0f ) ERROR_exit("Illegal value after '%s': '%s'",
                                    argv[iarg-1],argv[iarg]) ;
       fwhm_goal = val ; fwhm_2D = 0 ;
       iarg++ ; continue ;
     }

     if( strcmp(argv[iarg],"-FWHMxy") == 0 || strcmp(argv[iarg],"-FHWMxy") == 0 ){
       float val ;

       if( ++iarg >= argc ) ERROR_exit("Need argument after '%s'",argv[iarg-1]);
       val = (float)strtod(argv[iarg],NULL) ;
       if( val <= 0.0f ) ERROR_exit("Illegal value after '%s': '%s'",
                                    argv[iarg-1],argv[iarg]) ;
       fwhm_goal = val ; fwhm_2D = 1 ;
       iarg++ ; continue ;
     }

     ERROR_exit("Uknown option '%s'",argv[iarg]) ;

   } /*--- end of loop over options ---*/

   /*----- check for stupid inputs, load datasets, et cetera -----*/

   if( fwhm_goal == 0.0f )
     ERROR_exit("No -FWHM option given! What do you want?") ;

   if( bmset == NULL ) ERROR_exit("No -blurmaster dataset?!") ;

   if( inset == NULL ){
     if( iarg >= argc ) ERROR_exit("No input dataset on command line?") ;
     inset = THD_open_dataset( argv[iarg] ) ;
     if( inset == NULL  ) ERROR_exit("Can't open dataset '%s'",argv[iarg]) ;
   }
   nvox = DSET_NVOX(inset)     ;
   dx   = fabs(DSET_DX(inset)) ;
   dy   = fabs(DSET_DY(inset)) ;
   dz   = fabs(DSET_DZ(inset)) ;

   bmeqin = EQUIV_DSETS(bmset,inset) ;  /* blurmaster and input the same? */

   if( DSET_NX(inset) != DSET_NX(bmset) ||
       DSET_NY(inset) != DSET_NY(bmset) ||
       DSET_NZ(inset) != DSET_NZ(bmset)   )
     ERROR_exit("-blurmaster dataset grid doesn't match input dataset") ;

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
     INFO_message("Number of voxels in automask = %d",nmask) ;
     if( nmask < 333 ) ERROR_exit("Automask is too small to process") ;

   } else {
     mask = (byte *)malloc(sizeof(byte)*nvox) ; nmask = nvox ;
     memset(mask,1,sizeof(byte)*nvox) ;
     INFO_message("No mask ==> processing all %d voxels",nvox) ;
   }

   /*---- create neighborhood -----*/

   if( ntype == 0 ){
     ntype = NTYPE_SPHERE ; na = 10.0f ;
     INFO_message("Using default neighborhood 'SPHERE(10)'") ;
   }

   switch( ntype ){
     default:
       ERROR_exit("WTF?  ntype=%d",ntype) ;   /* should not happen */

     case NTYPE_SPHERE:{
       float ddx , ddy , ddz ;
       if( na < 0.0f ){ ddx = ddy = ddz = 1.0f ; na = -na ; }
       else           { ddx = dx ; ddy = dy ; ddz = dz ;    }
       if( fwhm_2D ) ddz = 1.e+10 ;   /* 2D only */
       nbhd = MCW_spheremask( ddx,ddy,ddz , na ) ;
     }
     break ;

     case NTYPE_RECT:{
       float ddx , ddy , ddz ;
       if( na < 0.0f ){ ddx = 1.0f; na = -na; } else ddx = dx ;
       if( nb < 0.0f ){ ddy = 1.0f; nb = -nb; } else ddy = dy ;
       if( nc < 0.0f ){ ddz = 1.0f; nc = -nc; } else ddz = dz ;
       nbhd = MCW_rectmask( ddx,ddy,ddz , na,nb,nc ) ;
     }
     break ;
   }

   INFO_message("Neighborhood comprises %d voxels",nbhd->num_pt) ;
   if( nbhd->num_pt < 19 )
     ERROR_exit("FWHM estimation requires neighborhood of at least 19 voxels!") ;

   /*--- process blurmaster dataset to produce blur master image array ---*/

   DSET_load(bmset) ;
   if( !DSET_LOADED(bmset) ) ERROR_exit("Can't load -blurmaster dataset!") ;
   bmed = THD_median_brick(bmset) ; INIT_IMARR(bmar) ;
   if( DSET_NVALS(bmset) == 1 ){
     ADDTO_IMARR(bmar,bmed) ;
   } else {
     float *mar=MRI_FLOAT_PTR(bmed) , *bar ;
     int ntouse = (int)ceil(3456.0/nbhd->num_pt) ;
          if( ntouse < 1                 ) ntouse = 1 ;
     else if( ntouse > DSET_NVALS(bmset) ) ntouse = DSET_NVALS(bmset) ;
     for( ibm=0 ; ibm < ntouse ; ibm++ ){
       bmim = mri_scale_to_float(DSET_BRICK_FACTOR(bmset,ibm),DSET_BRICK(bmset,ibm));
       bar  = MRI_FLOAT_PTR(bmim) ;
       for( ii=0 ; ii < nvox ; ii++ ) bar[ii] -= mar[ii] ;
       ADDTO_IMARR(bmar,bmim) ;
       DSET_unload_one(bmset,ibm) ;
     }
     mri_free(bmed) ;
   }
   DSET_unload(bmset) ;
   for( ibm=0 ; ibm < IMARR_COUNT(bmar) ; ibm++ ){
     IMARR_SUBIM(bmar,ibm)->dx = dx ;
     IMARR_SUBIM(bmar,ibm)->dy = dy ;
     IMARR_SUBIM(bmar,ibm)->dz = dz ;
   }

   /*--- pre-process input dataset ---*/

   DSET_load(inset) ;
   if( !DSET_LOADED(inset) ) ERROR_exit("Can't load input dataset from disk") ;
   INIT_IMARR(dsar) ;
   for( ids=0 ; ids < DSET_NVALS(inset) ; ids++ ){
     dsim = mri_scale_to_float(DSET_BRICK_FACTOR(inset,ids),DSET_BRICK(inset,ids));
     ADDTO_IMARR(dsar,dsim) ; DSET_unload_one(inset,ids) ;
   }
   DSET_unload(inset) ;
   for( ids=0 ; ids < IMARR_COUNT(dsar) ; ids++ ){
     IMARR_SUBIM(dsar,ids)->dx = dx ;
     IMARR_SUBIM(dsar,ids)->dy = dy ;
     IMARR_SUBIM(dsar,ids)->dz = dz ;
   }

   /*--- make arrays for FWHM estimates and then diffusion parameters ---*/

   numfxyz = 0 ;
   fxim = mri_new_conforming( IMARR_SUBIM(bmar,0) , MRI_float ) ;
   fxar = MRI_FLOAT_PTR(fxim) ;
   gx   = fwhm_goal - 0.666*dx ; numfxyz++ ;

   fyim = mri_new_conforming( IMARR_SUBIM(bmar,0) , MRI_float ) ;
   fyar = MRI_FLOAT_PTR(fyim) ;
   gy   = fwhm_goal - 0.666*dy ; numfxyz++ ;

   if( !fwhm_2D ){
     fzim = mri_new_conforming( IMARR_SUBIM(bmar,0) , MRI_float ) ;
     fzar = MRI_FLOAT_PTR(fzim) ;
     gz   = fwhm_goal - 0.666*dz ; numfxyz++ ;
   }
   maxfxyz    = 0.09f / numfxyz ;
   fwhm_goal *= 0.99f ;

   /*----------- Do the work:
                   estimate smoothness of blur master
                     - exit loop when all done
                   set up incremental blurring parameters
                   blur the master and the input
                   loop back to top                       --------------*/

   for( nite=1 ; nite <= maxite ; nite++ ){

     /*--- average blurring estimation ---*/

     fw = mriarr_estimate_FWHM_1dif( bmar , mask ) ;
     UNLOAD_FVEC3(fw,bx,by,bz) ;
     if( bx <= 0.0f ) bx = dx ;
     if( by <= 0.0f ) by = dy ;
     if( bz <= 0.0f ) bz = dz ;
     if( fwhm_2D ){
       INFO_message("-- Iteration #%d: 2D FWHMx=%.4f FWHMy=%.4f",nite,bx,by) ;
       if( sqrt(bx*by) >= fwhm_goal ){
         INFO_message("** Passes 2D area threshold ==> done!") ; break ;
       }
       if( nite > 3 && bx < last_fwx && by < last_fwy ){
         INFO_message("** Bailing out for sluggish progress!") ; break ;
       }
       xdone = (bx >= fwhm_goal || bx < last_fwx) ;
       ydone = (by >= fwhm_goal || by < last_fwy) ;
       zdone = 1 ;
     } else {
       INFO_message("-- Iteration #%d: 3D FWHMx=%.4f FWHMy=%.4f FWHMz=%.4f",
                    nite,bx,by,bz) ;
       if( cbrt(bx*by*bz) >= fwhm_goal ){
         INFO_message("-- Passes 3D volume threshold ==> done!") ; break ;
       }
       if( nite > 3 && bx < last_fwx && by < last_fwy && bz < last_fwz ){
         INFO_message("** Bailing out for sluggish progress!") ; break ;
       }
       xdone = (bx >= fwhm_goal || bx < last_fwx) ;
       ydone = (by >= fwhm_goal || by < last_fwy) ;
       zdone = (bz >= fwhm_goal || bz < last_fwz) ;
     }
     if( xdone && ydone && zdone ){
       INFO_message("** All axes stalled ==> done") ; break ;
     }
     maxfx = (xdone) ? 0.0111f*maxfxyz : maxfxyz ;
     maxfy = (ydone) ? 0.0111f*maxfxyz : maxfxyz ;
     maxfz = (zdone) ? 0.0111f*maxfxyz : maxfxyz ;
     last_fwx = 1.01f*bx; last_fwy = 1.01f*by; last_fwz = 1.01f*bz;

     /*--- blur map estimation ---*/

     ININFO_message(" Estimate blurring at each voxel") ;
     estimate_blur_map( bmar , mask,nbhd , fxar,fyar,fzar ) ;

     if( bsave_prefix != NULL ){
       char bp[1024]; THD_3dim_dataset *bset; float *qqq;
       { float *bbb=MRI_FLOAT_PTR( IMARR_SUBIM(bmar,0) ) ;
         qqq = malloc(sizeof(float)*nvox); memcpy(qqq,bbb,sizeof(float)*nvox);
         bset = EDIT_empty_copy(inset); sprintf(bp,"%s_%04d_bm",bsave_prefix,nite);
         EDIT_dset_items( bset, ADN_prefix,bp, ADN_nvals,1, ADN_ntt,0, ADN_none );
         EDIT_substitute_brick( bset , 0 , MRI_float , qqq ) ;
         EDIT_BRICK_LABEL( bset , 0 , "Bmar0" ) ;
         DSET_write(bset) ; WROTE_DSET(bset) ; DSET_delete(bset) ;
       }
       if( fxar != NULL ){
         qqq = malloc(sizeof(float)*nvox); memcpy(qqq,fxar,sizeof(float)*nvox);
         bset = EDIT_empty_copy(inset); sprintf(bp,"%s_%04dx",bsave_prefix,nite);
         EDIT_dset_items( bset, ADN_prefix,bp, ADN_nvals,1, ADN_ntt,0, ADN_none );
         EDIT_substitute_brick( bset , 0 , MRI_float , qqq ) ;
         EDIT_BRICK_LABEL( bset , 0 , "FWHMx" ) ;
         DSET_write(bset) ; WROTE_DSET(bset) ; DSET_delete(bset) ;
       }
       if( fyar != NULL ){
         qqq = malloc(sizeof(float)*nvox); memcpy(qqq,fyar,sizeof(float)*nvox);
         bset = EDIT_empty_copy(inset); sprintf(bp,"%s_%04dy",bsave_prefix,nite);
         EDIT_dset_items( bset, ADN_prefix,bp, ADN_nvals,1, ADN_ntt,0, ADN_none );
         EDIT_substitute_brick( bset , 0 , MRI_float , qqq ) ;
         EDIT_BRICK_LABEL( bset , 0 , "FWHMy" ) ;
         DSET_write(bset) ; WROTE_DSET(bset) ; DSET_delete(bset) ;
       }
       if( fzar != NULL ){
         qqq = malloc(sizeof(float)*nvox); memcpy(qqq,fzar,sizeof(float)*nvox);
         bset = EDIT_empty_copy(inset); sprintf(bp,"%s_%04dz",bsave_prefix,nite);
         EDIT_dset_items( bset, ADN_prefix,bp, ADN_nvals,1, ADN_ntt,0, ADN_none );
         EDIT_substitute_brick( bset , 0 , MRI_float , qqq ) ;
         EDIT_BRICK_LABEL( bset , 0 , "FWHMz" ) ;
         DSET_write(bset) ; WROTE_DSET(bset) ; DSET_delete(bset) ;
       }
     }

     /*--- compute local blurring parameters where needed ---*/

     nblur = 0 ;
     for( ii=0 ; ii < nvox ; ii++ ){
       if( mask[ii] == 1 ){
         nd = 0 ;
         if( fxar != NULL ){
           fxar[ii] = (fxar[ii] <= 0.0f || fxar[ii] >= gx) ? 0.0f : maxfx ;
           if( fxar[ii] > 0.0f ) nd++ ;
         }
         if( fyar != NULL ){
           fyar[ii] = (fyar[ii] <= 0.0f || fyar[ii] >= gy) ? 0.0f : maxfy ;
           if( fyar[ii] > 0.0f ) nd++ ;
         }
         if( fzar != NULL ){
           fzar[ii] = (fzar[ii] <= 0.0f || fzar[ii] >= gz) ? 0.0f : maxfz ;
           if( fzar[ii] > 0.0f ) nd++ ;
         }
         if( nd == 0 ) mask[ii] = 2 ;   /* turn off future diffusion here */
         else          nblur++ ;
       }
     }
     if( nblur == 0 ) break ;

     /*--- blur the master and the input ---*/

     ININFO_message(" Blurring %d voxels in master",nblur) ;
     for( ii=0 ; ii < IMARR_COUNT(bmar) ; ii++ )
       mri_blur3D_variable( IMARR_SUBIM(bmar,ii) , mask , fxim,fyim,fzim ) ;

     ININFO_message(" Blurring input") ;
     for( ii=0 ; ii < IMARR_COUNT(dsar) ; ii++ )
       mri_blur3D_variable( IMARR_SUBIM(dsar,ii) , mask , fxim,fyim,fzim ) ;

   } /*-- loop back to estimate smoothness, etc --*/

   /*----- toss some trash ---*/

   DESTROY_IMARR(bmar) ;

   if( fxim != NULL ) mri_free(fxim) ;
   if( fyim != NULL ) mri_free(fyim) ;
   if( fzim != NULL ) mri_free(fzim) ;

   /*--- create the output dataset ---*/

   outset = EDIT_empty_copy( inset ) ;
   EDIT_dset_items( outset , ADN_prefix , prefix , ADN_none ) ;
   tross_Copy_History( inset , outset ) ;
   tross_Make_History( "3dLocalstat" , argc,argv , outset ) ;
   for( ii=0 ; ii < DSET_NVALS(outset) ; ii++ )
     EDIT_substitute_brick( outset , ii , MRI_float ,
                            MRI_FLOAT_PTR( IMARR_SUBIM(dsar,ii) ) ) ;
   DSET_write(outset) ;
   WROTE_DSET(outset) ;
   exit(0) ;
}

/*----------------------------------------------------------------------------*/

#undef  ASSIF
#define ASSIF(f,i,v) if(f != NULL)f[i]=v

void estimate_blur_map( MRI_IMARR *bmar , byte *mask  , MCW_cluster *nbhd ,
                        float *fxar     , float *fyar , float *fzar        )
{
   int ibm , ii , nvox , nar , nx,nxy , xx,yy,zz , pp ;
   THD_fvec3 fw ;
   float  fxx ,  fyy , fzz ;
   int   nfxx , nfyy , nfzz;
   float  vxx ,  vyy ,  vzz;

ENTRY("estimate_blur_map") ;

   nvox = IMARR_SUBIM(bmar,0)->nvox ;
   nar  = IMARR_COUNT(bmar) ;
   nx   = IMARR_SUBIM(bmar,0)->nx ;
   nxy  = nx * IMARR_SUBIM(bmar,0)->ny ;

   for( ii=0 ; ii < nvox ; ii++ ){
     vxx = vyy = vzz = 0.0f ;
     if( mask[ii] == 1 ){
       nfxx = nfyy = nfzz = 0 ;
        fxx =  fyy =  fzz = 0.0f ;
       IJK_TO_THREE(ii,xx,yy,zz,nx,nxy) ;
       for( ibm=0 ; ibm < nar ; ibm++ ){
         fw = mri_nstat_fwhmxyz( xx,yy,zz , IMARR_SUBIM(bmar,ibm),mask,nbhd );
         if( fxar != NULL && fw.xyz[0] > 0.0f ){ nfxx++; fxx += fw.xyz[0]; }
         if( fyar != NULL && fw.xyz[1] > 0.0f ){ nfyy++; fyy += fw.xyz[1]; }
         if( fzar != NULL && fw.xyz[2] > 0.0f ){ nfzz++; fzz += fw.xyz[2]; }
       }
       if( nfxx > 0 ) vxx = fxx / nfxx ;
       if( nfyy > 0 ) vyy = fyy / nfyy ;
       if( nfzz > 0 ) vzz = fzz / nfzz ;
     }
     ASSIF(fxar,ii,vxx); ASSIF(fyar,ii,vyy); ASSIF(fzar,ii,vzz);
   }

   EXRETURN;
}
