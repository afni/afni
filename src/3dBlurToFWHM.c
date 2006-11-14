#include "mrilib.h"

#define NTYPE_SPHERE 1
#define NTYPE_RECT   2

void estimate_blur_map( MRI_IMARR *bmar , byte *mask  , MCW_cluster *nbhd ,
                        float *fxar     , float *fyar , float *fzar        ) ;

#undef  INMASK
#define INMASK(i) (mask == NULL || mask[i] != 0)

/*----------------------------------------------------------------------------*/

int main( int argc , char *argv[] )
{
   int iarg=1 , ii,nvox ;
   THD_3dim_dataset *bmset=NULL, *inset=NULL, *outset=NULL ;
   char *prefix="./blurto" ;
   float fwhm_xgoal=0.0f, fwhm_ygoal=0.0f, fwhm_zgoal=0.0f ;
   MCW_cluster *nbhd=NULL ;
   byte *mask=NULL ; int mask_nx,mask_ny,mask_nz , automask=0 ;
   int ntype=0 ; float na=0.0f,nb=0.0f,nc=0.0f ;
   MRI_IMARR *bmar ; MRI_IMAGE *bmed , *bmim ; int ibm ;
   MRI_IMARR *dsar ; MRI_IMAGE *dsim ;         int ids ;
   MRI_IMAGE *fxim=NULL , *fyim=NULL , *fzim=NULL ;
   float     *fxar=NULL , *fyar=NULL , *fzar=NULL ;
   float dx,dy,dz ;
   float gx,gy,gz , val ;
   int   mx,my,mz , nite , bmeqin , maxite=666 ;
   float bx,by,bz ;
   char *bsave_prefix=NULL ;

   /*-- help the pitifully ignorant luser? --*/

   if( argc < 2 || strcmp(argv[1],"-help") == 0 ){
     printf(
      "Usage: 3dBlurToFWHM [options]\n"
      "Blurs a 'master' dataset until it has (about) the same FWHM\n"
      "smoothness at each point.  The same blurring schedule is\n"
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
      "                   occur only within the mask.\n"
      " -automask       = Create an automask from the input dataset.\n"
      " -FWHMx      fwx = Blur until the FWHM in the x-direction is 'fwx' mm.\n"
      " -FWHMy      fwy = Blur until the FWHM in the y-direction is 'fwy' mm.\n"
      " -FWHMz      fwz = Blur until the FWHM in the z-direction is 'fwz' mm.\n"
      " -FWHMxy     fxy = Like '-FWHMx fxy -FWHMy fxy', just simpler.\n"
      " -FWHMxyz    fff = Like '-FWHMx fff -FWHMy fff -FWHMz fff'.\n"
      "           **N.B.: Note that you can't REDUCE the smoothness of a\n"
      "                   dataset.  If a location is already smoother than\n"
      "                   the goal, it won't be smoothed any more, but it\n"
      "                   won't be made less smooth.\n"
      "           **N.B.: Here, 'x', 'y', and 'z' refer to the grid/slice\n"
      "                   order as stored in the dataset, not DICOM coordinates!\n"
      " -nbhd       nnn = As in 3dLocalstat, specifies the neighborhood\n"
      "                   used to compute local smoothness.\n"
      "                   [Default = 'SPHERE(10)' = sphere of 10 mm radius.]\n"
      " -maxite     ccc = Set maximum number of iterations to 'ccc'.\n"
      " -bsave      bbb = Save the blur map estimates at each iteration\n"
      "                   with dataset prefix 'bbb'\n"
      "\n"
      "-- RWCox - Nov 2006\n"
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
       THD_3dim_dataset *mset ; int mmm ;
       if( ++iarg >= argc ) ERROR_exit("Need argument after '-mask'") ;
       if( mask != NULL || automask ) ERROR_exit("Can't have two mask inputs") ;
       mset = THD_open_dataset( argv[iarg] ) ;
       if( mset == NULL ) ERROR_exit("Can't open dataset '%s'",argv[iarg]) ;
       DSET_load(mset) ;
       if( !DSET_LOADED(mset) ) ERROR_exit("Can't load dataset '%s'",argv[iarg]) ;
       mask_nx = DSET_NX(mset); mask_ny = DSET_NY(mset); mask_nz = DSET_NZ(mset);
       mask = THD_makemask( mset , 0 , 0.5f, 0.0f ) ; DSET_delete(mset) ;
       if( mask == NULL ) ERROR_exit("Can't make mask from dataset '%s'",argv[iarg]) ;
       mmm = THD_countmask( mask_nx*mask_ny*mask_nz , mask ) ;
       INFO_message("Number of voxels in mask = %d",mmm) ;
       if( mmm < 333 ) ERROR_exit("Mask is too small to process") ;
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

     if( strncmp(argv[iarg],"-FWHM",5) == 0 ||
         strncmp(argv[iarg],"-FHWM",5) == 0   ){
       char *cpt = argv[iarg]+5 ;
       float val ;

       if( ++iarg >= argc ) ERROR_exit("Need argument after '%s'",argv[iarg-1]);
       val = (float)strtod(argv[iarg],NULL) ;
       if( val < 0.0f ) ERROR_exit("Illegal value after '%s': '%s'",
                                    argv[iarg-1],argv[iarg]) ;

       if( cpt == '\0' || strcasecmp(cpt,"xyz") == 0 ){  /* -FWHM or -FWHMxyz */
         fwhm_xgoal = fwhm_ygoal = fwhm_zgoal = val ;
       } else if( strcasecmp(cpt,"x") == 0 ){
         fwhm_xgoal = val ;
       } else if( strcasecmp(cpt,"y") == 0 ){
         fwhm_ygoal = val ;
       } else if( strcasecmp(cpt,"z") == 0 ){
         fwhm_zgoal = val ;
       } else if( strcasecmp(cpt,"xy") == 0 ){
         fwhm_xgoal = fwhm_ygoal = val ;
       } else {
         ERROR_exit("Don't recognize '%s' as a valid -FWHM option!",argv[iarg-1]);
       }
       iarg++ ; continue ;
     }

     ERROR_exit("Uknown option '%s'",argv[iarg]) ;

   } /*--- end of loop over options ---*/

   /*----- check for stupid inputs, load datasets, et cetera -----*/

   if( fwhm_xgoal == 0.0f && fwhm_ygoal == 0.0f && fwhm_zgoal == 0.0f )
     ERROR_exit("No -FWHM options given! What do you want?") ;

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

   if( DSET_NX(inset) < 2 ) fwhm_xgoal = 0.0f ;
   if( DSET_NY(inset) < 2 ) fwhm_ygoal = 0.0f ;
   if( DSET_NZ(inset) < 2 ) fwhm_zgoal = 0.0f ;
   if( fwhm_xgoal == 0.0f && fwhm_ygoal == 0.0f && fwhm_zgoal == 0.0f )
     ERROR_exit("Can't process with given FWHM params and slice dimensions!");

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
     int mmm ;
     mask = THD_automask( inset ) ;
     if( mask == NULL )
       ERROR_message("Can't create -automask from input dataset?") ;
     mmm = THD_countmask( DSET_NVOX(inset) , mask ) ;
     INFO_message("Number of voxels in automask = %d",mmm) ;
     if( mmm < 333 ) ERROR_exit("Automask is too small to process") ;
   }

   /*---- create neighborhood -----*/

   if( ntype == 0 ){
     ntype = NTYPE_SPHERE ; na = 10.0f ;
     INFO_message("Using default neighborhood 'SPHERE(10)'") ;
   }

   switch( ntype ){
     default:
       ERROR_exit("WTF?  ntype=%d",ntype) ;

     case NTYPE_SPHERE:{
       float ddx , ddy , ddz ;
       if( na < 0.0f ){ ddx = ddy = ddz = 1.0f ; na = -na ; }
       else           { ddx = dx ; ddy = dy ; ddz = dz ;    }
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
     ERROR_exit("FWHM requires neighborhood of at least 19 voxels!") ;

   /*--- process blurmaster dataset to produce blur master image array ---*/

   DSET_load(bmset) ;
   if( !DSET_LOADED(bmset) ) ERROR_exit("Can't load -blurmaster dataset!") ;
   bmed = THD_median_brick(bmset) ; INIT_IMARR(bmar) ;
   if( DSET_NVALS(bmset) == 1 ){
     ADDTO_IMARR(bmar,bmed) ;
   } else {
     float *mar=MRI_FLOAT_PTR(bmed) , *bar ;
     for( ibm=0 ; ibm < DSET_NVALS(bmset) ; ibm++ ){
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

   if( fwhm_xgoal > 0.0f ){
     fxim = mri_new_conforming( IMARR_SUBIM(bmar,0) , MRI_float ) ;
     fxar = MRI_FLOAT_PTR(fxim) ;
     gx   = fwhm_xgoal - 0.222*dx ;
   }
   if( fwhm_ygoal > 0.0f ){
     fyim = mri_new_conforming( IMARR_SUBIM(bmar,0) , MRI_float ) ;
     fyar = MRI_FLOAT_PTR(fyim) ;
     gy   = fwhm_ygoal - 0.222*dy ;
   }
   if( fwhm_zgoal > 0.0f ){
     fzim = mri_new_conforming( IMARR_SUBIM(bmar,0) , MRI_float ) ;
     fzar = MRI_FLOAT_PTR(fzim) ;
     gz   = fwhm_zgoal - 0.222*dz ;
   }

   /*----------- Do the work:
                   estimate smoothness of blur master
                     - exit loop when all done
                   set up incremental blurring parameters
                   blur the master and the input
                   loop back to top                       --------------*/

   for( nite=1 ; nite <= maxite ; nite++ ){

     /*--- blur map estimation ---*/

     INFO_message("Iteration #%d: estimate blur map",nite) ;
     estimate_blur_map( bmar , mask,nbhd , fxar,fyar,fzar ) ;

     if( bsave_prefix != NULL ){
       char bp[1024]; THD_3dim_dataset *bset; int nv=0, iv=0;
       sprintf(bp,"%s_%04d",bsave_prefix,nite) ;
       bset = EDIT_empty_copy( inset ) ;
       if( fxar != NULL ) nv++ ;
       if( fyar != NULL ) nv++ ;
       if( fzar != NULL ) nv++ ;
       EDIT_dset_items( bset ,
                          ADN_prefix , bp ,
                          ADN_nvals  , nv ,
                          ADN_ntt    , 0  ,
                        ADN_none ) ;
       if( fxar != NULL ){
         float *qqq = malloc(sizeof(float)*nvox) ;
         memcpy(qqq,fxar,sizeof(float)*nvox) ;
         for( ii=0 ; ii < nvox ; ii++ ) qqq[ii] = MAX(qqq[ii],0.0f) ;
         EDIT_substitute_brick( bset , iv , MRI_float , qqq ) ;
         EDIT_BRICK_LABEL( bset , iv , "FWHMx" ) ; iv++ ;
       }
       if( fyar != NULL ){
         float *qqq = malloc(sizeof(float)*nvox) ;
         memcpy(qqq,fyar,sizeof(float)*nvox) ;
         for( ii=0 ; ii < nvox ; ii++ ) qqq[ii] = MAX(qqq[ii],0.0f) ;
         EDIT_substitute_brick( bset , iv , MRI_float , qqq ) ;
         EDIT_BRICK_LABEL( bset , iv , "FWHMy" ) ; iv++ ;
       }
       if( fzar != NULL ){
         float *qqq = malloc(sizeof(float)*nvox) ;
         memcpy(qqq,fzar,sizeof(float)*nvox) ;
         for( ii=0 ; ii < nvox ; ii++ ) qqq[ii] = MAX(qqq[ii],0.0f) ;
         EDIT_substitute_brick( bset , iv , MRI_float , qqq ) ;
         EDIT_BRICK_LABEL( bset , iv , "FWHMz" ) ; iv++ ;
       }
       DSET_write(bset) ; WROTE_DSET(bset) ; DSET_delete(bset) ;
     }

     /*--- count how many voxels aren't smooth enuf yet,
           and compute local blurring parameters where needed ---*/

     mx = my = mz = 0 ; bx = by = bz = 1.e+10 ;
     for( ii=0 ; ii < nvox ; ii++ ){
       if( INMASK(ii) ){
         if( fxar != NULL ){
           if( fxar[ii] <= 0.0f || fxar[ii] >= gx ) fxar[ii] = 0.0f ;
           else {
             mx++; val = (fwhm_xgoal-fxar[ii])/dx; val = val*val * 0.045f;
             fxar[ii] = MIN(val,0.04f) ;
           }
         }
         if( fyar != NULL ){
           if( fyar[ii] <= 0.0f || fyar[ii] >= gy ) fyar[ii] = 0.0f ;
           else {
             my++; val = (fwhm_ygoal-fyar[ii])/dy; val = val*val * 0.045f;
             fyar[ii] = MIN(val,0.04f) ;
           }
         }
         if( fzar != NULL ){
           if( fzar[ii] <= 0.0f || fzar[ii] >= gz ) fzar[ii] = 0.0f ;
           else {
             mz++; val = (fwhm_zgoal-fzar[ii])/dz; val = val*val * 0.045f;
             fzar[ii] = MIN(val,0.04f) ;
           }
         }
       }
     }
     ININFO_message(" #Too coarse: x=%d  y=%d  z=%d",mx,my,mz) ;
     if( mx+my+mz == 0 ) break ;  /****** WE WIN!!!!! *****/

     /*--- blur the master and the input ---*/

     ININFO_message(" Blurring master") ;
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
   double fxx ,  fyy , fzz ;
   int   nfxx , nfyy , nfzz;
   float  vxx ,  vyy ,  vzz;

ENTRY("estimate_blur_map") ;

   nvox = IMARR_SUBIM(bmar,0)->nvox ;
   nar  = IMARR_COUNT(bmar) ;
   nx   = IMARR_SUBIM(bmar,0)->nx ;
   nxy  = nx * IMARR_SUBIM(bmar,0)->ny ;

   for( ii=0 ; ii < nvox ; ii++ ){
     vxx = vyy = vzz = 0.0f ;
     if( INMASK(ii) ){
       nfxx = nfyy = nfzz = 0 ;
        fxx =  fyy =  fzz = 1.0 ;
       IJK_TO_THREE(ii,xx,yy,zz,nx,nxy) ;
       for( ibm=0 ; ibm < nar ; ibm++ ){
         fw = mri_nstat_fwhmxyz( xx,yy,zz , IMARR_SUBIM(bmar,ibm),mask,nbhd ) ;
         if( fxar != NULL && fw.xyz[0] > 0.0f ){ nfxx++ ; fxx *= fw.xyz[0] ; }
         if( fyar != NULL && fw.xyz[1] > 0.0f ){ nfyy++ ; fyy *= fw.xyz[1] ; }
         if( fzar != NULL && fw.xyz[2] > 0.0f ){ nfzz++ ; fzz *= fw.xyz[2] ; }
       }
       if( nfxx > 0 ) vxx = (nfxx==1) ? fxx : pow(fxx,1.0/nfxx);
       if( nfyy > 0 ) vyy = (nfyy==1) ? fyy : pow(fyy,1.0/nfyy);
       if( nfzz > 0 ) vzz = (nfzz==1) ? fzz : pow(fzz,1.0/nfzz);
     }
     ASSIF(fxar,ii,vxx) ; ASSIF(fyar,ii,vyy) ; ASSIF(fzar,ii,vzz) ;
   }

   EXRETURN;
}
