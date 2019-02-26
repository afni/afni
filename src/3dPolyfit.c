#include "mrilib.h"

/*------------------------------------------------------------------------*/

int main( int argc , char * argv[] )
{
   float mrad=0.0f , fwhm=0.0f ;
   int nrep=1 ;
   char *prefix = "Polyfit" ;
   char *resid  = NULL ;
   char *cfnam  = NULL ;
   int iarg , verb=0 , do_automask=0 , nord=3 , meth=2 , do_mclip=0 ;
   THD_3dim_dataset *inset ;
   MRI_IMAGE *imout , *imin ;
   byte *mask=NULL ; int nvmask=0 , nmask=0 , do_mone=0 , do_byslice=0 ;
   MRI_IMARR *exar=NULL ;
   floatvec *fvit=NULL ;   /* 26 Feb 2019 */

   if( argc < 2 || strcasecmp(argv[1],"-help") == 0 ){
      printf("\n"
             "Usage: 3dPolyfit [options] dataset\n"
             "\n"
             "Fits a polynomial in space to the input dataset and outputs that fitted dataset.\n"
             "\n"
             "You can also add your own basis datasets to the fitting mix, using the\n"
             "'-base' option.\n"
             "\n"
             "--------\n"
             "Options:\n"
             "--------\n"
             "\n"
             "  -nord n    = Maximum polynomial order (0..9) [default order=3]\n"
             "                [n=0 is the constant 1]\n"
             "                [n=-1 means only use volumes from '-base']\n"
             "\n"
             "  -blur f    = Gaussian blur input dataset (inside mask) with FWHM='f' (mm)\n"
             "\n"
             "  -mrad r    = Radius (voxels) of preliminary median filter of input\n"
             "                [default is no blurring of either type; you can]\n"
             "                [do both types (Gaussian and median), but why??]\n"
             "                [N.B.: median blur is slower than Gaussian]\n"
             "\n"
             "  -prefix pp = Use 'pp' for prefix of output dataset (the fit).\n"
             "                [default prefix is 'Polyfit'; use NULL to skip this output]\n"
             "\n"
             "  -resid  rr = Use 'rr' for the prefix of the residual dataset.\n"
             "                [default is not to output residuals]\n"
             "\n"
             "  -1Dcoef cc = Save coefficients of fit into text file 'cc'.\n"
             "                [default is not to save these coefficients]\n"
             "\n"
             "  -automask  = Create a mask (a la 3dAutomask)\n"
             "  -mask mset = Create a mask from nonzero voxels in 'mset'.\n"
             "                [default is not to use a mask, which is probably a bad idea]\n"
             "\n"
             "  -mone      = Scale the mean value of the fit (inside the mask) to 1.\n"
             "                [probably this option is not useful for anything]\n"
             "\n"
             "  -mclip     = Clip fit values outside the rectilinear box containing the\n"
             "               mask to the edge of that box, to avoid weird artifacts.\n"
             "\n"
             "  -meth mm   = Set 'mm' to 2 for least squares fit;\n"
             "               set it to 1 for L1 fit [default method=2]\n"
             "                [Note that L1 fitting is slower than L2 fitting!]\n"
             "\n"
             "  -base bb   = In addition to the polynomial fit, also use\n"
             "               the volumes in dataset 'bb' as extra basis functions.\n"
             "                [If you use a base dataset, then you can set nord]\n"
             "                [to -1, to skip using any spatial polynomial fit.]\n"
             "\n"
             "  -verb      = Print fun and useful progress reports :-)\n"
             "\n"
             "------\n"
             "Notes:\n"
             "------\n"
             "* Output dataset is always stored in float format.\n"
             "\n"
             "* If the input dataset has more than 1 sub-brick, only sub-brick #0\n"
             "  is processed. To fit more than one volume, you'll have to use a script\n"
             "  to loop over the input sub-bricks, and then glue (3dTcat) the results\n"
             "  together to get a final result.\n"
             "\n"
             "* If the '-base' dataset has multiple sub-bricks, all of them are used.\n"
             "\n"
             "* You can use the '-base' option more than once.\n"
             "\n"
             "* The original motivation for this program was to fit a spatial model\n"
             "  to a field map MRI, but that didn't turn out to be useful. Nevertheless,\n"
             "  I make this program available to someone who might find it beguiling.\n"
             "\n"
             "-- Dec 2010 - RWCox\n"
            ) ;
      PRINT_COMPILE_DATE ; exit(0) ;
   }

   /*-- startup paperwork --*/

   mainENTRY("3dPolyfit main"); machdep(); AFNI_logger("3dPolyfit",argc,argv);
   PRINT_VERSION("3dPolyfit") ;

   /*-- scan command line --*/

   iarg = 1 ;
   while( iarg < argc && argv[iarg][0] == '-' ){

     if( strcasecmp(argv[iarg],"-base") == 0 ){
       THD_3dim_dataset *bset ; int kk ; MRI_IMAGE *bim ;
       if( ++iarg >= argc ) ERROR_exit("Need argument after '-base'") ;
       bset = THD_open_dataset(argv[iarg]) ;
       CHECK_OPEN_ERROR(bset,argv[iarg]) ;
       DSET_load(bset) ; CHECK_LOAD_ERROR(bset) ;
       if( exar == NULL ) INIT_IMARR(exar) ;
       for( kk=0 ; kk < DSET_NVALS(bset) ; kk++ ){
         bim = THD_extract_float_brick(kk,bset) ;
         if( bim != NULL ) ADDTO_IMARR(exar,bim) ;
         DSET_unload_one(bset,kk) ;
       }
       DSET_delete(bset) ;
       iarg++ ; continue ;
     }

     if( strcasecmp(argv[iarg],"-verb") == 0 ){
       verb++ ; iarg++ ; continue ;
     }

     if( strcasecmp(argv[iarg],"-hermite") == 0 ){ /* 25 Mar 2013 [New Year's Day] */
       mri_polyfit_set_basis("hermite") ;          /* HIDDEN */
       iarg++ ; continue ;
     }

     if( strcasecmp(argv[iarg],"-byslice") == 0 ){ /* 25 Mar 2013 [New Year's Day] */
       do_byslice++ ; iarg++ ; continue ;          /* HIDDEN */
     }

     if( strcasecmp(argv[iarg],"-mask") == 0 ){
       THD_3dim_dataset *mset ;
       if( ++iarg >= argc ) ERROR_exit("Need argument after '-mask'") ;
       if( mask != NULL || do_automask ) ERROR_exit("Can't have two mask inputs") ;
       mset = THD_open_dataset(argv[iarg]) ;
       CHECK_OPEN_ERROR(mset,argv[iarg]) ;
       DSET_load(mset) ; CHECK_LOAD_ERROR(mset) ;
       nvmask = DSET_NVOX(mset) ;
       mask = THD_makemask( mset , 0 , 0.5f, 0.0f ) ; DSET_delete(mset) ;
       if( mask == NULL ) ERROR_exit("Can't make mask from dataset '%s'",argv[iarg]) ;
       nmask = THD_countmask( nvmask , mask ) ;
       if( nmask < 99 ) ERROR_exit("Too few voxels in mask (%d)",nmask) ;
       if( verb ) INFO_message("Number of voxels in mask = %d",nmask) ;
       iarg++ ; continue ;
     }

     if( strcasecmp(argv[iarg],"-nord") == 0 ){
       nord = (int)strtol( argv[++iarg], NULL , 10 ) ;
       if( nord < -1 || nord > 9 )
         ERROR_exit("Illegal value after -nord!") ;
       iarg++ ; continue ;
     }

     if( strcasecmp(argv[iarg],"-meth") == 0 ){
       meth = (int)strtol( argv[++iarg], NULL , 10 ) ;
       if( meth < 1 || meth > 2 )
         ERROR_exit("Illegal value after -meth!") ;
       iarg++ ; continue ;
     }

     if( strncmp(argv[iarg],"-automask",5) == 0 ){
       if( mask != NULL ) ERROR_exit("Can't use -mask and -automask together!") ;
       do_automask++ ; iarg++ ; continue ;
     }

     if( strncmp(argv[iarg],"-mclip",5) == 0 ){
       do_mclip++ ; iarg++ ; continue ;
     }

     if( strncmp(argv[iarg],"-mone",5) == 0 ){
       do_mone++ ; iarg++ ; continue ;
     }

     if( strcasecmp(argv[iarg],"-mrad") == 0 ){
       mrad = strtod( argv[++iarg] , NULL ) ; iarg++ ; continue ;
     }

     if( strcasecmp(argv[iarg],"-blur") == 0 ){
       fwhm = strtod( argv[++iarg] , NULL ) ; iarg++ ; continue ;
     }

     if( strcasecmp(argv[iarg],"-prefix") == 0 ){
       prefix = argv[++iarg] ;
       if( !THD_filename_ok(prefix) )
         ERROR_exit("Illegal value after -prefix!\n");
       if( strcasecmp(prefix,"NULL") == 0 ) prefix = NULL ;
       iarg++ ; continue ;
     }

     if( strcasecmp(argv[iarg],"-resid") == 0 ){
       resid = argv[++iarg] ;
       if( !THD_filename_ok(resid) )
         ERROR_exit("Illegal value after -resid!\n");
       if( strcasecmp(resid,"NULL") == 0 ) resid = NULL ;
       iarg++ ; continue ;
     }

     if( strcasecmp(argv[iarg],"-1Dcoef") == 0 ){  /* 26 Feb 2019 */
       cfnam = argv[++iarg] ;
       if( !THD_filename_ok(cfnam) )
         ERROR_exit("Illegal value after -1Dcoef!\n");
       if( strcasecmp(cfnam,"NULL") == 0 ) cfnam = NULL ;
       iarg++ ; continue ;
     }

     ERROR_exit("Unknown option: %s\n",argv[iarg]);
   }

   /*--- check for blatant errors ---*/

   if( iarg >= argc )
     ERROR_exit("No input dataset name on command line?");

   if( prefix == NULL && resid == NULL && cfnam == NULL )
     ERROR_exit("-prefix and -resid and -1Dcoef are all NULL?!") ;

   if( do_byslice && cfnam != NULL ){
     WARNING_message("-byslice does not work with -1Dcoef option :(") ;
     cfnam = NULL ;
   }

   if( nord < 0 && exar == NULL )
     ERROR_exit("no polynomial fit AND no -base option ==> nothing to compute :(") ;

   /*-- read input --*/

   if( verb ) INFO_message("Load input dataset") ;

   inset = THD_open_dataset( argv[iarg] ) ;
   CHECK_OPEN_ERROR(inset,argv[iarg]) ;
   DSET_load(inset) ; CHECK_LOAD_ERROR(inset) ;
   if( DSET_NVALS(inset) > 1 )
     WARNING_message( "Only processing sub-brick #0 (out of %d)" , DSET_NVALS(inset) );

   /* check input mask or create automask */

   if( mask != NULL ){
     if( nvmask != DSET_NVOX(inset) )
      ERROR_exit("-mask and input datasets don't match in voxel counts :-(") ;
   } else if( do_automask ){
     THD_automask_verbose( (verb > 1) ) ;
     THD_automask_extclip( 1 ) ;
     mask = THD_automask( inset ) ; nvmask = DSET_NVOX(inset) ;
     nmask = THD_countmask( nvmask , mask ) ;
     if( nmask < 99 ) ERROR_exit("Too few voxels in automask (%d)",nmask) ;
     if( verb ) ININFO_message("Number of voxels in automask = %d",nmask) ;
   } else {
     WARNING_message("3dPolyfit is running without a mask") ;
   }

#undef  GOOD
#define GOOD(i) (mask == NULL || mask[i])

   /* check -base input datasets */

   if( exar != NULL ){
     int ii,kk , nvbad=0 , nvox=DSET_NVOX(inset),nm ; float *ex , exb ;
     for( kk=0 ; kk < IMARR_COUNT(exar) ; kk++ ){
       if( nvox != IMARR_SUBIM(exar,kk)->nvox ){
         if( IMARR_SUBIM(exar,kk)->nvox != nvbad ){
           ERROR_message("-base volume (%d voxels) doesn't match input dataset grid size (%d voxels)",
                         IMARR_SUBIM(exar,kk)->nvox , nvox ) ;
           nvbad = IMARR_SUBIM(exar,kk)->nvox ;
         }
       }
     }
     if( nvbad != 0 ) ERROR_exit("Cannot continue :-(") ;

     /* subtract mean from each base input, if is a constant polynomial in the fit */

     if( nord >= 0 ){
       if( verb ) INFO_message("subtracting spatial mean from '-base'") ;
       for( kk=0 ; kk < IMARR_COUNT(exar) ; kk++ ){
         exb = 0.0f ; ex = MRI_FLOAT_PTR(IMARR_SUBIM(exar,kk)) ;
         for( nm=ii=0 ; ii < nvox ; ii++ ){ if( GOOD(ii) ){ exb += ex[ii]; nm++; } }
         exb /= nm ;
         for( ii=0 ; ii < nvox ; ii++ ) ex[ii] -= exb ;
       }
     }
   }

   /* if blurring, edit mask a little */

   if( mask != NULL && (fwhm > 0.0f || mrad > 0.0f) ){
     int ii ;
     ii = THD_mask_remove_isolas( DSET_NX(inset),DSET_NY(inset),DSET_NZ(inset),mask ) ;
     if( ii > 0 ){
       nmask = THD_countmask( nvmask , mask ) ;
       if( verb )
         ININFO_message("Removed %d isola%s from mask, leaving %d voxels" ,
                        ii,(ii==1)?"\0":"s" , nmask ) ;
       if( nmask < 99 )
         ERROR_exit("Too few voxels left in mask after isola removal :-(") ;
     }
   }

   /* convert input to float, which is simpler to deal with */

   imin = THD_extract_float_brick(0,inset) ;
   if( imin == NULL ) ERROR_exit("Can't extract input dataset brick?! :-(") ;
   DSET_unload(inset) ;

   if( verb ) INFO_message("Start fitting process") ;

   /* do the Gaussian blurring */

   if( fwhm > 0.0f ){
     if( verb ) ININFO_message("Gaussian blur: FWHM=%g mm",fwhm) ;
     imin->dx = fabsf(DSET_DX(inset)) ;
     imin->dy = fabsf(DSET_DY(inset)) ;
     imin->dz = fabsf(DSET_DZ(inset)) ;
     mri_blur3D_addfwhm( imin , mask , fwhm ) ;
   }

   /* do the fitting */

   mri_polyfit_verb(verb) ;
   if( do_byslice )
     imout = mri_polyfit_byslice( imin , nord , exar , mask , mrad , meth ) ;
   else
     imout = mri_polyfit        ( imin , nord , exar , mask , mrad , meth ) ;

   /* WTF? */

   if( imout == NULL )
     ERROR_exit("Can't compute polynomial fit :-( !?") ;
   if( resid == NULL ) mri_free(imin) ;

   if( ! do_byslice )
     fvit = mri_polyfit_get_fitvec() ; /* get coefficients of fit [26 Feb 2019] */

   /* scale the fit dataset? */

   if( do_mone ){
     float sum=0.0f ; int nsum=0 , ii,nvox ; float *par=MRI_FLOAT_PTR(imout) ;
     nvox = imout->nvox ;
     for( ii=0 ; ii < nvox ; ii++ ){
       if( mask != NULL && mask[ii] == 0 ) continue ;
       sum += par[ii] ; nsum++ ;
     }
     if( nsum > 0 && sum != 0.0f ){
       sum = nsum / sum ;
       if( verb ) ININFO_message("-mone: scaling fit by %g",sum) ;
       for( ii=0 ; ii < nvox ; ii++ ) par[ii] *= sum ;
     }
   }

   /* if there's a mask, clip values outside of its box */

#undef  PF
#define PF(i,j,k) par[(i)+(j)*nx+(k)*nxy]
   if( mask != NULL && do_mclip ){
     int xm,xp,ym,yp,zm,zp , ii,jj,kk , nx,ny,nz,nxy ; float *par ;
     MRI_IMAGE *bim = mri_empty_conforming( imout , MRI_byte ) ;
     mri_fix_data_pointer(mask,bim) ;
     if( verb ) ININFO_message("-mclip: polynomial fit to autobox of mask") ;
     MRI_autobbox( bim , &xm,&xp , &ym,&yp , &zm,&zp ) ;
     mri_clear_data_pointer(bim) ; mri_free(bim) ;
     nx = imout->nx ; ny = imout->ny ; nz = imout->nz ; nxy = nx*ny ;
     par = MRI_FLOAT_PTR(imout) ;
     for( ii=0 ; ii < xm ; ii++ )
      for( kk=0 ; kk < nz ; kk++ )
       for( jj=0 ; jj < ny ; jj++ ) PF(ii,jj,kk) = PF(xm,jj,kk) ;
     for( ii=xp+1 ; ii < nx ; ii++ )
      for( kk=0 ; kk < nz ; kk++ )
       for( jj=0 ; jj < ny ; jj++ ) PF(ii,jj,kk) = PF(xp,jj,kk) ;
     for( jj=0 ; jj < ym ; jj++ )
      for( kk=0 ; kk < nz ; kk++ )
       for( ii=0 ; ii < nx ; ii++ ) PF(ii,jj,kk) = PF(ii,ym,kk) ;
     for( jj=yp+1 ; jj < ny ; jj++ )
      for( kk=0 ; kk < nz ; kk++ )
       for( ii=0 ; ii < nx ; ii++ ) PF(ii,jj,kk) = PF(ii,yp,kk) ;
     for( kk=0 ; kk < zm ; kk++ )
      for( jj=0 ; jj < ny ; jj++ )
       for( ii=0 ; ii < nx ; ii++ ) PF(ii,jj,kk) = PF(ii,jj,zm) ;
     for( kk=zp+1 ; kk < nz ; kk++ )
      for( jj=0 ; jj < ny ; jj++ )
       for( ii=0 ; ii < nx ; ii++ ) PF(ii,jj,kk) = PF(ii,jj,zp) ;
   }

   if( mask != NULL ) free(mask) ;

   /* write outputs */

   if( prefix != NULL ){
     THD_3dim_dataset *outset = EDIT_empty_copy( inset )  ;
     EDIT_dset_items( outset ,
                        ADN_prefix , prefix ,
                        ADN_nvals  , 1 ,
                        ADN_ntt    , 0 ,
                      ADN_none ) ;
     EDIT_substitute_brick( outset , 0 , MRI_float , MRI_FLOAT_PTR(imout) ) ;
     tross_Copy_History( inset , outset ) ;
     tross_Make_History( "3dPolyfit" , argc,argv , outset ) ;
     DSET_write(outset) ;
     WROTE_DSET(outset) ;
   }

   if( resid != NULL ){
     THD_3dim_dataset *outset = EDIT_empty_copy( inset )  ;
     float *inar=MRI_FLOAT_PTR(imin) , *outar=MRI_FLOAT_PTR(imout) ;
     int nx,ny,nz , nxyz , kk ;
     nx = imout->nx ; ny = imout->ny ; nz = imout->nz ; nxyz = nx*ny*nz ;
     for( kk=0 ; kk < nxyz ; kk++ ) outar[kk] = inar[kk] - outar[kk] ;
     mri_free(imin) ;
     EDIT_dset_items( outset ,
                        ADN_prefix , resid ,
                        ADN_nvals  , 1 ,
                        ADN_ntt    , 0 ,
                      ADN_none ) ;
     EDIT_substitute_brick( outset , 0 , MRI_float , MRI_FLOAT_PTR(imout) ) ;
     tross_Copy_History( inset , outset ) ;
     tross_Make_History( "3dPolyfit" , argc,argv , outset ) ;
     DSET_write(outset) ;
     WROTE_DSET(outset) ;
   }

   if( cfnam != NULL && fvit != NULL ){ /* won't work with '-byslice' */
     mri_write_floatvec( modify_afni_prefix(cfnam,NULL,".1D") , fvit ) ;
   }

   exit(0) ;
}
