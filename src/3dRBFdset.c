#include "mrilib.h"

/**** Adapted from 3dUndump.c ****/

void Syntax(void)
{
   printf(
    "Usage: 3dRBFdset [options]\n"
    "Uses Radial Basis Functions to fit the values given at a set\n"
    "of points and then creates a 3D dataset by evaluating the\n"
    "fit at each point on a 3D grid.  Output dataset is stored\n"
    "in floating point format.\n"
    "\n"
    "You might ask 'What use is this program?'  The answer is that\n"
    "it is purely for testing the RBF expansion code, in preparation\n"
    "for using those functions for something glorious.\n"
    "\n"
    "Options:\n"
    "  -prefix ppp  = 'ppp' is the prefix for the output dataset\n"
    "                   [default = rbf].\n"
    "  -master mmm  = 'mmm' is the master dataset, whose geometry\n"
    "                   will determine the geometry of the output.\n"
    "                ** This is a MANDATORY option.\n"
    "  -mask kkk    = This option specifies a mask dataset 'kkk', which\n"
    "                   will control which voxels are allowed to get\n"
    "                   values set.  If the mask is present, only\n"
    "                   voxels that are nonzero in the mask will be\n"
    "                   nonzero in the new dataset.\n"
    "  -knotx kfile = 3 column .1D file that lists the 'knot' coordinates\n"
    "                   (center of each radial basis function).\n"
    "                ** This is a MANDATORY option.\n"
    "                ** There must be at least 5 knot locations.\n"
    "  -fitx ffile  = 3 column .1D file that lists the locations of the\n"
    "                   values to be fitted by the RBF expansion.\n"
    "                ** If '-fitx' is not given, then the coordinates\n"
    "                   are assumed to be the same as the knots.\n"
    "                ** If '-fitx' is given, there must be at least\n"
    "                   as many fit locations as there are knots.\n"
    "  -vals vfile  = .1D file that lists the values to be fitted at\n"
    "                   each fit location.\n"
    "                ** This is a MANDATORY option.\n"
    "                ** One sub-brick will be created for each column\n"
    "                   in 'vfile'.\n"
    "                ** 'vfile' must have the same number of values per\n"
    "                   column that the '-fitx' file does (or as the\n"
    "                   '-knotx' file, if '-fitx' isn't being used).\n"
    "\n"
    "-- RWCox -- February 2009\n"
   ) ;

   PRINT_COMPILE_DATE ; exit(0) ;
}

/*---------------------------------------------------------------------------*/

static void ijk_to_xyz( THD_3dim_dataset *dset , int ijk ,
                        float *x , float *y , float *z    )
{
   THD_fvec3 xyz_mm , xyz_dicom ;
   THD_ivec3 ivec ;

   ivec.ijk[0] = DSET_index_to_ix(dset,ijk) ;
   ivec.ijk[1] = DSET_index_to_jy(dset,ijk) ;
   ivec.ijk[2] = DSET_index_to_kz(dset,ijk) ;
   xyz_mm      = THD_3dind_to_3dmm ( dset , ivec ) ;
   xyz_dicom   = THD_3dmm_to_dicomm( dset , xyz_mm ) ;
   UNLOAD_FVEC3( xyz_dicom , *x , *y , *z ) ;
   return ;
}

/*---------------------------------------------------------------------------*/

int main( int argc , char * argv[] )
{
   char *prefix="rbf" ;
   MRI_IMAGE *kfim=NULL , *ffim=NULL , *vfim=NULL ;
   float     *kfar=NULL , *ffar=NULL , *vfar=NULL ;
   int        kfnx      ,  ffnx      ,  vfnx,vfny , coincide=0 ;
   byte *mmask=NULL ;
   THD_3dim_dataset *dset , *maskset=NULL , *mset=NULL ;
   int iarg , ii,ijk , nx,ny,nz , nxyz,ival,nmask ;
   float xx,yy,zz , *vv , *dar ;
   RBF_knots    *rbk ;
   RBF_evalues  *rbe ;
   RBF_evalgrid *rbg ;

   /*------------ help? ------------*/

   if( argc < 3 || strcmp(argv[1],"-help") == 0 ) Syntax() ;

   mainENTRY("3dRBFdset main") ; machdep() ; PRINT_VERSION("3dRBFdset");
   machdep() ; AFNI_logger("3dRBFdset",argc,argv) ;

   /*------------ command line options ------------*/

   iarg = 1 ;
   while( iarg < argc && argv[iarg][0] == '-' ){

      /*-----*/

      if( strncmp(argv[iarg],"-knotx",5) == 0 ){
        if( kfim != NULL )
          ERROR_exit("Can't have 2 %s options!",argv[iarg]) ;
        if( iarg+1 >= argc )
          ERROR_exit("%s: no argument follows!?",argv[iarg]) ;
        kfim = mri_read_1D( argv[++iarg] ) ;
        if( kfim == NULL )
          ERROR_exit("%s %s: can't read file!?",argv[iarg-1],argv[iarg]) ;
        if( kfim->ny != 3 )
          ERROR_exit("%s %s: file must have exactly 3 columns!",argv[iarg-1],argv[iarg]) ;
        if( kfim->nx < 5 )
          ERROR_exit("%s %s: file must have 5 or more rows!",argv[iarg-1],argv[iarg]) ;
        kfnx = kfim->nx ; kfar = MRI_FLOAT_PTR(kfim) ;
        iarg++ ; continue ;
      }

      /*-----*/

      if( strncmp(argv[iarg],"-fitx",4) == 0 ){
        if( ffim != NULL )
          ERROR_exit("Can't have 2 %s options!",argv[iarg]) ;
        if( iarg+1 >= argc )
          ERROR_exit("%s: no argument follows!?",argv[iarg]) ;
        ffim = mri_read_1D( argv[++iarg] ) ;
        if( ffim == NULL )
          ERROR_exit("%s %s: can't read file!?",argv[iarg-1],argv[iarg]) ;
        if( ffim->ny != 3 )
          ERROR_exit("%s %s: file must have exactly 3 columns!",argv[iarg-1],argv[iarg]) ;
        ffnx = ffim->nx ; ffar = MRI_FLOAT_PTR(kfim) ;
        iarg++ ; continue ;
      }

      /*-----*/

      if( strncmp(argv[iarg],"-vals",4) == 0 ){
        if( vfim != NULL )
          ERROR_exit("Can't have 2 %s options!",argv[iarg]) ;
        if( iarg+1 >= argc )
          ERROR_exit("%s: no argument follows!?",argv[iarg]) ;
        vfim = mri_read_1D( argv[++iarg] ) ;
        if( vfim == NULL )
          ERROR_exit("%s %s: can't read file!?",argv[iarg-1],argv[iarg]) ;
        vfnx = vfim->nx ; vfny = vfim->ny ; vfar = MRI_FLOAT_PTR(vfim) ;
        iarg++ ; continue ;
      }

      /*-----*/

      if( strcmp(argv[iarg],"-prefix") == 0 ){
         if( iarg+1 >= argc )
            ERROR_exit("-prefix: no argument follows!?") ;
         else if( !THD_filename_ok(argv[++iarg]) )
            ERROR_exit("-prefix: Illegal prefix given!") ;
         prefix = argv[iarg] ;
         iarg++ ; continue ;
      }

      /*-----*/

      if( strcmp(argv[iarg],"-master") == 0 ){
         if( iarg+1 >= argc )
            ERROR_exit("-master: no argument follows!?") ;
         else if( mset != NULL )
            ERROR_exit("-master: can't have two -master options!") ;
         mset = THD_open_dataset( argv[++iarg] ) ;
         if( mset == NULL )
            ERROR_exit("-master: can't open dataset" ) ;
         iarg++ ; continue ;
      }

      /*-----*/

      if( strcmp(argv[iarg],"-mask") == 0 ){
        if( iarg+1 >= argc )
          ERROR_exit("-mask: no argument follows!?") ;
        else if( maskset != NULL )
          ERROR_exit("-mask: can't have two -mask options!") ;
        maskset = THD_open_dataset( argv[++iarg] ) ;
        if( maskset == NULL )
          ERROR_exit("-mask: can't open dataset" ) ;
        iarg++ ; continue ;
      }

      /*-----*/

      ERROR_exit("Unknown option: %s",argv[iarg]) ;

   } /* end of loop over command line options */

   /*----- check for inconsistencies or criminal negligence -----*/

   if( mset == NULL )
     ERROR_exit("3dRBFdset: -master is a mandatory option!") ;
   if( kfar == NULL )
     ERROR_exit("3dRBFdset: -knotx is a mandatory option!") ;
   if( vfar == NULL )
     ERROR_exit("3dRBFdset: -vals is a mandatory option!") ;

   if( ffar == NULL ){             /* no -fitx ==> use -knotx */
     ffar = kfar ; ffnx = kfnx ; coincide = 1 ;
   } else if( ffnx < kfnx ){
     ERROR_exit("-fitx file must have at least as many rows as -knotx file!") ;
   }

   if( ffnx != vfnx )
     ERROR_exit("-vals file must have same number lines as %s file!",
                (coincide) ? "-knotx" : "-fitx" ) ;

   /*------- make empty dataset -------*/

   dset = EDIT_empty_copy( mset ) ;
   EDIT_dset_items( dset ,
                      ADN_prefix    , prefix ,
                      ADN_datum_all , MRI_float ,
                      ADN_nvals     , vfny ,
                      ADN_ntt       , 0 ,
                      ADN_func_type , ISANAT(mset) ? ANAT_BUCK_TYPE
                                                   : FUNC_BUCK_TYPE ,
                      ADN_directory_name , "./" ,
                    ADN_none ) ;

   if( THD_deathcon() && THD_is_file(DSET_HEADNAME(dset)) )
      ERROR_exit("Output dataset already exists -- can't overwrite") ;


   nx = DSET_NX(dset); ny = DSET_NY(dset); nz = DSET_NZ(dset); nxyz = nx*ny*nz;

   /*--- check and make mask if desired ---*/

   if( maskset != NULL ){
     if( DSET_NX(maskset) != nx || DSET_NY(maskset) != ny || DSET_NZ(maskset) != nz )
       ERROR_exit("-mask dataset doesn't match dimension of output dataset") ;
     mmask = THD_makemask( maskset , 0 , 1.0,-1.0 ) ;
     DSET_delete(maskset) ;
     if( mmask == NULL ){
       WARNING_message("Can't create mask for some unknowable reason!") ;
     } else {
       nmask = THD_countmask( nxyz , mmask ) ;
       if( nmask == 0 ){
         WARNING_message("0 voxels in mask -- ignoring it!") ;
         free((void *)mmask) ; mmask = NULL ;
       } else {
         INFO_message("%d voxels found in mask",nmask) ;
       }
     }
   }
   if( mmask == NULL ){
     mmask = (byte *)malloc(sizeof(byte)*nxyz) ;
     memset( mmask , 1 , sizeof(byte)*nxyz ) ;
     nmask = nxyz ;
   }

   /*----- setup for RBF interpolation at the given set of points -----*/

   if( coincide )
     rbk = RBF_setup_knots( kfnx , kfar+0*kfnx , kfar+1*kfnx , kfar+2*kfnx ,
                            0    , NULL        , NULL        , NULL         ) ;
   else
     rbk = RBF_setup_knots( kfnx , kfar+0*kfnx , kfar+1*kfnx , kfar+2*kfnx ,
                            ffnx , ffar+0*ffnx , ffar+1*ffnx , ffar+2*ffnx  ) ;
   if( rbk == NULL )
     ERROR_exit("Can't setup RBF interpolation for some reason!") ;

   /*-- setup output grid points --*/

   MAKE_RBF_evalgrid(rbg,nmask) ;
   for( ii=ijk=0 ; ijk < nxyz ; ijk++ ){
     if( mmask[ijk] ){
       ijk_to_xyz( dset , ijk , &xx,&yy,&zz ) ;
       rbg->xpt[ii] = xx ;
       rbg->ypt[ii] = yy ;
       rbg->zpt[ii] = zz ; ii++ ;
     }
   }

   /*-- setup space for evaluation --*/

   MAKE_RBF_evalues(rbe,ffnx) ;
   vv = (float *)malloc(sizeof(float)*nmask) ;

   /*-- loop over columns in vfar and compute results --*/

   for( ival=0 ; ival < vfny ; ival++ ){
     memcpy( rbe->val , vfar+ival*ffnx , sizeof(float)*ffnx ) ;
     ii = RBF_setup_evalues( rbk , rbe ) ;
     if( ii == 0 ) ERROR_exit("Can't compute knot coefficients!?") ;
     ii = RBF_evaluate( rbk , rbe , rbg , vv ) ;
     if( ii == 0 ) ERROR_exit("Can't compute RBF expansion!?") ;
     EDIT_substitute_brick( dset , ival , MRI_float , NULL ) ;
     dar = DSET_BRICK_ARRAY(dset,ival) ;
     for( ii=ijk=0 ; ijk < nxyz ; ijk++ ){
       if( mmask[ijk] ) dar[ijk] = vv[ii++] ;
     }
   }

   free(vv) ; free(mmask) ;
   DESTROY_RBF_evalgrid(rbg); DESTROY_RBF_evalues(rbe); DESTROY_RBF_knots(rbk);
   mri_free(vfim) ; mri_free(kfim) ; mri_free(ffim) ;

   dset_floatscan(dset) ;
   tross_Make_History( "3dRBFdset" , argc,argv , dset ) ;
   DSET_write(dset) ;
   WROTE_DSET(dset) ;
   exit(0) ;
}
