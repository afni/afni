#include "mrilib.h"
#include "r_new_resam_dset.h"

#ifdef USE_OMP
#include <omp.h>
#include "mri_genalign_util.c"
#endif

/*----------------------------------------------------------------------------*/
/* interpolate from a float image to a set of indexes */

void mri_interp_floatim( MRI_IMAGE *fim ,
                         int np , float *ip , float *jp , float *kp ,
                         int code, float *outar )
{
   ENTRY("mri_interp_floatim") ;

   switch( code ){
     case MRI_NN:      GA_interp_NN     ( fim, np,ip,jp,kp, outar ) ; break ;
     case MRI_LINEAR:  GA_interp_linear ( fim, np,ip,jp,kp, outar ) ; break ;
     case MRI_CUBIC:   GA_interp_cubic  ( fim, np,ip,jp,kp, outar ) ; break ;
     default:
     case MRI_QUINTIC: GA_interp_quintic( fim, np,ip,jp,kp, outar ) ; break ;
     case MRI_WSINC5:  GA_interp_wsinc5 ( fim, np,ip,jp,kp, outar ) ; break ;
   }

   if( MRI_HIGHORDER(code) ){
     int ii,nn=fim->nvox ; float bot,top , *far=MRI_FLOAT_PTR(fim) ;
     bot = top = far[0] ;
     for( ii=1 ; ii < nn ; ii++ ) if( bot > far[ii] ) bot = far[ii] ;
                             else if( top < far[ii] ) top = far[ii] ;
     for( ii=0 ; ii < np ; ii++ ) if( outar[ii] < bot ) outar[ii] = bot ;
                             else if( outar[ii] > top ) outar[ii] = top ;
   }

   EXRETURN ;
}

/*----------------------------------------------------------------------------*/
/* Setup to warp images given
     bimar    = array of DICOM (x,y,z) deltas
     cmat_bim = matrix to transform indexes (ib,jb,kb) to DICOM (xb,yb,zb)
     cmat_src = similar matrix for source dataset to be warped from
     cmat_out = similar matrix for output dataset to be warped to

   foreach (io,jo,ko) in output dataset do {
     (xo,yo,zo) =    [cmat_out](io,jo,ko)
     (ib,jb,kb) = inv[cmat_bim](xo,yo,zo)
     (xs,ys,zs) = (xo,yo,zo) + bimar interpolated at (ib,jb,kb)
     (is,js,ks) = inv[cmat_src](xs,ys,zs)
   }

   The output is the array of images of (is,js,ks) = indexes in the source
   dataset, for each point to interpolated to in the output dataset (io,jo,ko).
   This set of images can be used, in turn, to interpolate a src grid image
   to an out grid warped image via mri_interp_floatim().
*//*--------------------------------------------------------------------------*/

MRI_IMARR * mri_setup_nwarp( MRI_IMARR *bimar, mat44 cmat_bim , int incode ,
                             mat44 cmat_src  , mat44 cmat_out ,
                             int nx_out      , int ny_out     , int nz_out  )
{
   int nx,ny,nz,nxy,nxyz ;
   float *xp, *yp, *zp ;
   MRI_IMAGE *wxim, *wyim, *wzim ; MRI_IMARR *wimar ; mat44 tmat ;

ENTRY("mri_apply_nwarp") ;

   if( bimar == NULL ) RETURN(NULL) ;

   nx = nx_out ; ny = ny_out ; nz = nz_out ; nxy = nx*ny ; nxyz = nxy*nz ;

   /* space for indexes/coordinates */

   xp = (float *)malloc(sizeof(float)*nxyz) ;
   yp = (float *)malloc(sizeof(float)*nxyz) ;
   zp = (float *)malloc(sizeof(float)*nxyz) ;

   if( !MAT44_FLEQ(cmat_bim,cmat_out) ){
     int qq,ii,jj,kk ; mat44 imat_out_to_bim ;

     tmat = MAT44_INV(cmat_bim) ; imat_out_to_bim = MAT44_MUL(tmat,cmat_out) ;

     /* compute indexes of each point in output image
        (the _out grid) in the warp space (the _bim grid),
        using the imat_out_to_bim matrix computed just above */

     for( qq=0 ; qq < nxyz ; qq++ ){
       ii = qq % nx ; kk = qq / nxy ; jj = (qq-kk*nxy) / nx ;
       MAT44_VEC( imat_out_to_bim , ii,jj,kk , xp[qq],yp[qq],zp[qq] ) ;
     }

   } else {   /* case where cmat_bim and cmat_out are equal */
     int qq,ii,jj,kk ;

     for( qq=0 ; qq < nxyz ; qq++ ){
       ii = qq % nx ; kk = qq / nxy ; jj = (qq-kk*nxy) / nx ;
       xp[qq] = ii ; yp[qq] = jj ; zp[qq] = kk ;
     }

   }

   /* now interpolate the warp delta volumes from the bim
      grid to the out grid, using the indexes computed just above */

   wxim = mri_new_vol(nx,ny,nz,MRI_float) ;
   mri_interp_floatim( IMARR_SUBIM(bimar,0), nxyz,xp,yp,zp,
                                             incode, MRI_FLOAT_PTR(wxim) ) ;
   wyim = mri_new_vol(nx,ny,nz,MRI_float) ;
   mri_interp_floatim( IMARR_SUBIM(bimar,1), nxyz,xp,yp,zp,
                                             incode, MRI_FLOAT_PTR(wyim) ) ;
   wzim = mri_new_vol(nx,ny,nz,MRI_float) ;
   mri_interp_floatim( IMARR_SUBIM(bimar,2), nxyz,xp,yp,zp,
                                             incode, MRI_FLOAT_PTR(wzim) ) ;

   free(zp) ; zp = MRI_FLOAT_PTR(wzim) ;
   free(yp) ; yp = MRI_FLOAT_PTR(wyim) ;
   free(xp) ; xp = MRI_FLOAT_PTR(wxim) ;

   /* now convert to index warp from src to out space */

   tmat = MAT44_INV(cmat_src) ;  /* takes (x,y,z) to (i,j,k) in src space */

 AFNI_OMP_START ;
#pragma omp parallel if( nxyz > 33333 )
 { int qq,ii,jj,kk ; float xx,yy,zz ;
#pragma omp for
   for( qq=0 ; qq < nxyz ; qq++ ){
     ii = qq % nx ; kk = qq / nxy ; jj = (qq-kk*nxy) / nx ;
     MAT44_VEC( cmat_out , ii,jj,kk , xx,yy,zz ) ;      /* compute (xo,yo,zo) */
     xx += xp[qq] ; yy += yp[qq] ; zz += zp[qq] ;        /* add in the deltas */
     MAT44_VEC( tmat, xx,yy,zz, xp[qq],yp[qq],zp[qq] ) ; /* ==> to (is,js,ks) */
   }
 }
 AFNI_OMP_END ;

   /* package results for delivery to the (ab)user */

   INIT_IMARR(wimar) ;
   ADDTO_IMARR(wimar,wxim) ; ADDTO_IMARR(wimar,wyim) ; ADDTO_IMARR(wimar,wzim) ;

   RETURN(wimar) ;
}

/*----------------------------------------------------------------------------*/

int main( int argc , char *argv[] )
{
   THD_3dim_dataset *dset_nwarp=NULL , *dset_src=NULL , *dset_mast=NULL ;
   MRI_IMARR *imar_nwarp=NULL , *im_src ;
   char *prefix     = NULL ;
   double dxyz_mast = 0.0 ;
   int interp_code  = MRI_WSINC5 ;
   int iarg , kk , verb=1 , iv ;
   mat44 src_cmat, nwarp_cmat, mast_cmat ;
   THD_3dim_dataset *dset_out ;
   MRI_IMAGE *fim , *wim ; float *ip,*jp,*kp ;
   int nx,ny,nz,nxyz ;

   /**----------------------------------------------------------------------*/
   /**----------------- Help the pitifully ignorant user? -----------------**/

   if( argc < 2 || strcasecmp(argv[1],"-help") == 0 ){
     printf(
      "Usage: 3dNwarpApply [options] sourcedataset\n"
      "\n"
      "Program to apply a nonlinear warp saved from 3dAllineate -nwarp_save\n"
      "to a dataset, to produce a warped version of the source dataset.\n"
      "\n"
      "OPTIONS:\n"
      "--------\n"
      " -nwarp  www  = 'www' is the name of the warp dataset\n"
      " -source sss  = 'sss' is the name of the source dataset\n"
      " -master mmm  = 'mmm  is the name of the master dataset\n"
      " -newgrid dd  = 'dd' is the new grid spacing (in mm)\n"
      " -interp iii  = 'iii' is the interpolation mode\n"
      " -prefix ppp  = 'ppp' is the name of the new output dataset\n"
      " -quiet       = Don't be verbose.\n"
      " -verb        = Be extra verbose.\n"
      "\n"
      "NOTES:\n"
      "------\n"
      "* At present, this program doesn't work with 2D warps, only with 3D.\n"
      "* Default interpolation mode is 'wsinc5'; available modes are the\n"
      "   same as in 3dAllineate:  NN  linear  cubic  quintic  wsinc5\n"
      "* The same interpolation mode is used for the warp itself (if needed)\n"
      "   and then for the data being warped.\n"
      "* At present, the output is always in float format, no matter what\n"
      "   absurd data type the input uses.\n"
      "* Program 3dNwarpCalc could be used to operate on 3D warps:\n"
      "  ++ Catenate them; invert them; pre- or post-apply an affine warp.\n"
      "  ++ Alas!  3dNwarpCalc has yet to be written.  If AFNI survives the\n"
      "     coming budget cuts, then it will be forthcoming later in 2011.\n"
     ) ;
     exit(0) ;
   }

   /**--- bookkeeping and marketing ---**/

   mainENTRY("3dNwarpApply"); machdep();
   AFNI_logger("3dNwarpApply",argc,argv);
   PRINT_VERSION("3dNwarpApply"); AUTHOR("Zhark the Warped");
   (void)COX_clock_time() ;

   /**--- process command line options ---**/

   iarg = 1 ;
   while( iarg < argc && argv[iarg][0] == '-' ){

     /*---------------*/

     if( strcasecmp(argv[iarg],"-quiet") == 0 ){
       verb = 0 ; iarg++ ; continue ;
     }

     if( strcasecmp(argv[iarg],"-verb") == 0 ){
       verb++ ; iarg++ ; continue ;
     }

     /*---------------*/

     if( strncasecmp(argv[iarg],"-prefix",5) == 0 ){
       if( prefix != NULL ) ERROR_exit("Can't have multiple %s options :-(",argv[iarg]) ;
       iarg++ ;
       if( !THD_filename_ok(argv[iarg]) )
         ERROR_exit("badly formed filename: '%s' '%s' :-(",argv[iarg-1],argv[iarg]) ;
       if( strcasecmp(argv[iarg],"NULL") == 0 )
         ERROR_exit("can't use filename: '%s' '%s' :-(",argv[iarg-1],argv[iarg]) ;
       else
         prefix = argv[iarg] ;
       iarg++ ; continue ;
     }

     /*---------------*/

     if( strcasecmp(argv[iarg],"-nwarp") == 0 ){
       if( dset_nwarp != NULL ) ERROR_exit("Can't have multiple %s options :-(",argv[iarg]) ;
       if( ++iarg >= argc ) ERROR_exit("no argument after '%s' :-(",argv[iarg-1]) ;
       dset_nwarp = THD_open_dataset( argv[iarg] ) ;
       if( dset_nwarp == NULL ) ERROR_exit("can't open -nwarp dataset '%s' :-(",argv[iarg]);
       iarg++ ; continue ;
     }

     /*---------------*/

     if( strcasecmp(argv[iarg],"-source") == 0 ){
       if( dset_src != NULL ) ERROR_exit("Can't have multiple %s options :-(",argv[iarg]) ;
       if( ++iarg >= argc ) ERROR_exit("no argument after '%s' :-(",argv[iarg-1]) ;
       dset_src = THD_open_dataset( argv[iarg] ) ;
       if( dset_src == NULL ) ERROR_exit("can't open -source dataset '%s' :-(",argv[iarg]);
       iarg++ ; continue ;
     }

     /*---------------*/

     if( strcasecmp(argv[iarg],"-master") == 0 ){
       if( dset_mast != NULL ) ERROR_exit("Can't have multiple %s options :-(",argv[iarg]) ;
       if( ++iarg >= argc ) ERROR_exit("no argument after '%s' :-(",argv[iarg-1]) ;
       dset_mast = THD_open_dataset( argv[iarg] ) ;
       if( dset_mast == NULL ) ERROR_exit("can't open -master dataset '%s' :-(",argv[iarg]);
       iarg++ ; continue ;
     }

     /*---------------*/

     if( strcasecmp(argv[iarg],"-mast_dxyz") == 0 ||
         strcasecmp(argv[iarg],"-dxyz_mast") == 0 ||
         strcasecmp(argv[iarg],"-newgrid"  ) == 0   ){

       if( ++iarg >= argc ) ERROR_exit("no argument after '%s' :-(",argv[iarg-1]) ;
       dxyz_mast = strtod(argv[iarg],NULL) ;
       if( dxyz_mast <= 0.0 )
         ERROR_exit("Illegal value '%s' after -mast_dxyz :-(",argv[iarg]) ;
       if( dxyz_mast <= 0.5 )
         WARNING_message("Small value %g after -mast_dxyz :-(",dxyz_mast) ;
       iarg++ ; continue ;
     }

     /*---------------*/

     if( strcasecmp(argv[iarg],"-NN") == 0 || strncasecmp(argv[iarg],"-nearest",6) == 0 ){
       interp_code = MRI_NN ; iarg++ ; continue ;
     }
     if( strncasecmp(argv[iarg],"-linear",4)==0 || strncasecmp(argv[iarg],"-trilinear",6)==0 ){
       interp_code = MRI_LINEAR ; iarg++ ; continue ;
     }
     if( strncasecmp(argv[iarg],"-cubic",4)==0 || strncasecmp(argv[iarg],"-tricubic",6)==0 ){
       interp_code = MRI_CUBIC ; iarg++ ; continue ;
     }
     if( strncasecmp(argv[iarg],"-quintic",4)==0 || strncasecmp(argv[iarg],"-triquintic",6)==0 ){
       interp_code = MRI_QUINTIC ; iarg++ ; continue ;
     }
     if( strncasecmp(argv[iarg],"-wsinc",5) == 0 ){
       interp_code = MRI_WSINC5 ; iarg++ ; continue ;
     }
     if( strncasecmp(argv[iarg],"-interp",5)==0 ){
       char *inam ;
       if( ++iarg >= argc ) ERROR_exit("no argument after '%s' :-(",argv[iarg-1]) ;
       inam = argv[iarg] ; if( *inam == '-' ) inam++ ;
       if( strcasecmp(inam,"NN")==0 || strncasecmp(inam,"nearest",5)==0 )
         interp_code = MRI_NN ;
       else
       if( strncasecmp(inam,"linear",3)==0 || strncasecmp(inam,"trilinear",5)==0 )
         interp_code = MRI_LINEAR ;
       else
       if( strncasecmp(inam,"cubic",3)==0 || strncasecmp(inam,"tricubic",5)==0 )
         interp_code = MRI_CUBIC ;
       else
       if( strncasecmp(inam,"quintic",3)==0 || strncasecmp(inam,"triquintic",5)==0 )
         interp_code = MRI_QUINTIC ;
       else
       if( strncasecmp(inam,"WSINC",5)==0 )
         interp_code = MRI_WSINC5 ;
       else
         ERROR_exit("Unknown code '%s' after '%s' :-(",argv[iarg],argv[iarg-1]) ;
       iarg++ ; continue ;
     }

     /*---------------*/

     ERROR_exit("Unknown and Illegal option '%s' :-( :-( :-(",argv[iarg]) ;
   }

   /*-------- check inputs to see if the user is completely demented ---------*/

   if( dset_nwarp == NULL )
     ERROR_exit("No -nwarp option?  How do you want to warp? :-(") ;

   if( dset_src == NULL ){
     if( ++iarg < argc ){
       dset_src = THD_open_dataset( argv[iarg] ) ;
       if( dset_src == NULL )
         ERROR_exit("can't open source dataset '%s' :-(",argv[iarg]);
     } else {
       ERROR_exit("No source dataset?  What do you want to warp? :-(") ;
     }
   }

   if( dset_mast == NULL ) dset_mast = dset_src ;

   if( prefix == NULL ){
     prefix = (char *)malloc(sizeof(char)*THD_MAX_NAME) ;
     strcpy( prefix , DSET_PREFIX(dset_src) ) ;
     strcat( prefix , "_nwarp" ) ;
     INFO_message("No '-prefix' option ==> using '%s'",prefix) ;
   }

   /*---------- manufacture the empty shell of the output dataset ----------*/

   if( !ISVALID_MAT44(dset_src->daxes->ijk_to_dicom) )
     THD_daxes_to_mat44(dset_src->daxes) ;
   src_cmat = dset_src->daxes->ijk_to_dicom ;

   if( !ISVALID_MAT44(dset_nwarp->daxes->ijk_to_dicom) )
     THD_daxes_to_mat44(dset_nwarp->daxes) ;
   nwarp_cmat = dset_nwarp->daxes->ijk_to_dicom ;

   if( dxyz_mast > 0.0 ){
     THD_3dim_dataset *qset ;
     qset = r_new_resam_dset( dset_mast , NULL ,
                              dxyz_mast,dxyz_mast,dxyz_mast ,
                              NULL , RESAM_NN_TYPE , NULL , 0 ) ;
     if( qset != NULL ){
       dset_mast = qset ;
       THD_daxes_to_mat44(dset_mast->daxes) ;
       if( verb )
         INFO_message("changing output grid spacing to %.3f mm",dxyz_mast) ;
     }
   }

   if( !ISVALID_MAT44(dset_mast->daxes->ijk_to_dicom) ) /* make sure have */
     THD_daxes_to_mat44(dset_mast->daxes) ;      /* index-to-DICOM matrix */

   mast_cmat = dset_mast->daxes->ijk_to_dicom ;

   dset_out = EDIT_empty_copy( dset_mast ) ;  /* create the output dataset! */
   EDIT_dset_items( dset_out ,                /* and patch it up */
                      ADN_prefix    , prefix ,
                      ADN_nvals     , DSET_NVALS(dset_src) ,
                      ADN_datum_all , MRI_float ,
                    ADN_none ) ;
   if( DSET_NUM_TIMES(dset_src) > 1 )
     EDIT_dset_items( dset_out ,
                        ADN_ntt   , DSET_NVALS(dset_src) ,
                        ADN_ttdel , DSET_TR(dset_src) ,
                        ADN_tunits, UNITS_SEC_TYPE ,
                        ADN_nsl   , 0 ,
                      ADN_none ) ;
   else
     EDIT_dset_items( dset_out ,
                        ADN_func_type , ISANAT(dset_out) ? ANAT_BUCK_TYPE
                                                         : FUNC_BUCK_TYPE ,
                      ADN_none ) ;

   /* copy brick info into output */

   THD_copy_datablock_auxdata( dset_src->dblk , dset_out->dblk ) ;
   for( kk=0 ; kk < DSET_NVALS(dset_out) ; kk++ )
     EDIT_BRICK_FACTOR(dset_out,kk,0.0) ;

   tross_Copy_History( dset_src , dset_out ) ;        /* hysterical records */
   tross_Make_History( "3dNwarpApply" , argc,argv , dset_out ) ;

   THD_daxes_to_mat44(dset_out->daxes) ;           /* save coord transforms */

   /*----- create warping indexes from warp dataset -----*/

   if( verb )
     INFO_message("Setting up index-to-index warp from xyz-to-xyz warp") ;

   DSET_load(dset_nwarp) ; CHECK_LOAD_ERROR(dset_nwarp) ;
   INIT_IMARR(imar_nwarp) ;
   fim = THD_extract_float_brick(0,dset_nwarp) ; ADDTO_IMARR(imar_nwarp,fim) ;
   fim = THD_extract_float_brick(1,dset_nwarp) ; ADDTO_IMARR(imar_nwarp,fim) ;
   fim = THD_extract_float_brick(2,dset_nwarp) ; ADDTO_IMARR(imar_nwarp,fim) ;
   DSET_unload(dset_nwarp) ;

   nx = DSET_NX(dset_out) ;
   ny = DSET_NY(dset_out) ;
   nz = DSET_NZ(dset_out) ; nxyz = nx*ny*nz ;

   im_src = mri_setup_nwarp( imar_nwarp , nwarp_cmat , interp_code ,
                             src_cmat , mast_cmat , nx , ny , nz    ) ;

   ip = MRI_FLOAT_PTR( IMARR_SUBIM(im_src,0) ) ;
   jp = MRI_FLOAT_PTR( IMARR_SUBIM(im_src,1) ) ;
   kp = MRI_FLOAT_PTR( IMARR_SUBIM(im_src,2) ) ;

   DESTROY_IMARR(imar_nwarp) ;

   /*----- warp each sub-brick of the input -----*/

   DSET_load(dset_src) ; CHECK_LOAD_ERROR(dset_src) ;

   INFO_message("Starting warp of source dataset sub-bricks") ;

   for( iv=0 ; iv < DSET_NVALS(dset_src) ; iv++ ){
     fim = THD_extract_float_brick(iv,dset_src) ; DSET_unload_one(dset_src,iv) ;
     wim = mri_new_conforming( fim , MRI_float ) ;
     mri_interp_floatim( fim, nxyz,ip,jp,kp, interp_code, MRI_FLOAT_PTR(wim) ) ;
     EDIT_substitute_brick( dset_out , iv , MRI_float , MRI_FLOAT_PTR(wim) ) ;
     mri_clear_data_pointer(wim) ; mri_free(wim) ;
   }

   DSET_unload(dset_src) ; DESTROY_IMARR(im_src) ;
   DSET_write(dset_out) ; WROTE_DSET(dset_out) ;
   exit(0) ;
}
