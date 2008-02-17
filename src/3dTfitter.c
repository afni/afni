#include "mrilib.h"

int main( int argc , char *argv[] )
{
   int iarg , ii,jj,kk , nx,ny,nz,nvox ;
   THD_3dim_dataset *rhset=NULL ;
   THD_3dim_dataset *lset ; MRI_IMAGE *lim ; int nlset=0 ;
   THD_3dim_dataset *fset ;
   XtPointer_array *dsar ;
   int ntime , nvar=0 ;
   char *prefix="Tfitter" ;
   int meth=2 , flags=0 , nbad=0,ngood=0 ;
   intvec *convec=NULL , *kvec ;
   byte *mask=NULL ; int mnx,mny,mnz ;
   floatvec *bfit ;
   float *dvec , **rvec , *cvec=NULL , *fvec ;

   /*------- help the pitifully ignorant user? -------*/

   if( argc < 2 || strcmp(argv[1],"-help") == 0 ){
     printf(
      "Usage: 3dTfitter [options]\n"
      "At each voxels, assembles and solves a set of linear equations.\n"
      "Output is a bucket dataset with the parameters at each voxel.\n"
      "\n"
      "Options:\n"
      "  -RHS rset = Specifies the right-hand-side 3D+time dataset.\n"
      "\n"
      "  -LHS lset = Specifies a column (or columns) of the left-hand-side matrix.\n"
      "             * More than one 'lset' can follow the '-LHS' option, but each\n"
      "               input filename must NOT start with the '-' character!\n"
      "             * Or you can use multiple '-LHS' options, if you prefer.\n"
      "             * Each 'lset' can be a 3D+time dataset, or a 1D file\n"
      "                 with 1 or more columns.\n"
      "           *** Columns are assembled in the order given on the command line,\n"
      "               which means that parameters will be output in that order!\n"
      "\n"
      "  -lsqfit   = Solve equations via least squares [the default].\n"
      "\n"
      "  -l1fit    = Solve equations via least sum of absolute residuals.\n"
      "\n"
      "  -constrain= Follow this option with a list of parameter indexes\n"
      "              to indicate that some parameters should be constrained\n"
      "              in the solution; for example:\n"
      "                 -constrain 1 -3\n"
      "              which indicates that parameter #1 (from the first -LHS)\n"
      "              must be non-negative, and that parameter #3 must be\n"
      "              non-positive.  Parameter #2 is unconstrained.\n"
      "             * Constraints are not available yet with '-lsqfit';\n"
      "               only with '-l1fit'.\n"
      "\n"
      "  -prefix p = Prefix for the output dataset filename.\n"
      "             * Which is always in float format.\n"
      "\n"
      "  -mask ms  = Read in dataset 'ms' as a mask; only voxels with nonzero\n"
      "              values in the mask will be processed.\n"
      "\n"
      "-- RWCox -- Feb 2008\n"
     ) ;
     PRINT_COMPILE_DATE ; exit(0) ;
   }

   /*------- read command line args -------*/

   iarg = 1 ; INIT_XTARR(dsar) ;
   while( iarg < argc ){

     if( strcasecmp(argv[iarg],"-mask") == 0 ){
       THD_3dim_dataset *mset ;
       if( ++iarg >= argc )
         ERROR_exit("Need argument after %s",argv[iarg-1]);
       if( mask != NULL )
         ERROR_exit("Can't have two -mask arguments!") ;
       mset = THD_open_dataset(argv[iarg]) ;
       CHECK_OPEN_ERROR(mset,argv[iarg]) ;
       DSET_load(mset) ; CHECK_LOAD_ERROR(mset) ;
       mnx = DSET_NX(mset); mny = DSET_NY(mset); mnz = DSET_NZ(mset);
       mask = THD_makemask( mset, 0, 1.0f,0.0f ); DSET_delete(mset);
       if( mask == NULL ) ERROR_exit("Can't make mask") ;
       ii = THD_countmask( mnx*mny*mnz , mask ) ;
       INFO_message("%d voxels in the mask") ;
       if( ii < 1 ) ERROR_exit("mask is empty?!") ;
       iarg++ ; continue ;
     }

     if( strcasecmp(argv[iarg],"-rhs") == 0 ){
       if( ++iarg >= argc )
         ERROR_exit("Need argument after %s",argv[iarg-1]);
       if( rhset != NULL )
         ERROR_exit("Can't have two %s options",argv[iarg-1]);
       rhset = THD_open_dataset( argv[iarg] ) ;
       if( rhset == NULL )
         ERROR_exit("Can't open dataset '%s'",argv[iarg]) ;
       iarg++ ; continue ;
     }

     if( strcasecmp(argv[iarg],"-lhs") == 0 ){
       if( ++iarg >= argc )
         ERROR_exit("Need argument after %s",argv[iarg-1]);
       if( argv[iarg][0] == '-' )
         ERROR_exit("Illegal argument after %s",argv[iarg-1]) ;
       for( ; iarg < argc && argv[iarg][0] != '-' ; iarg++ ){
         if( strstr(argv[iarg],"1D") != NULL ){
           lim = mri_read_1D(argv[iarg]) ;
           if( lim == NULL )
             ERROR_exit("Can't read 1D file '%s'",argv[iarg]) ;
           if( lim->ny > 1 )
             INFO_message("1D file '%s' has %d columns",argv[iarg],lim->ny);
           ADDTO_XTARR(dsar,lim) ;
           ii= XTARR_NUM(dsar)-1 ; XTARR_IC(dsar,ii) = IC_FLIM ;
           nvar += lim->ny ;
         } else {
           lset = THD_open_dataset(argv[iarg]) ;
           if( lset == NULL )
             ERROR_exit("Can't read dataset '%s'",argv[iarg]) ;
           ADDTO_XTARR(dsar,lset) ;
           ii= XTARR_NUM(dsar)-1 ; XTARR_IC(dsar,ii) = IC_DSET ;
           nvar++ ; nlset++ ;
         }
       }
       continue ;
     }

     if( strcasecmp(argv[iarg],"-lsqfit") == 0 ||
         strcasecmp(argv[iarg],"-l2fit")  == 0   ){
       meth = 2 ; iarg++ ; continue ;
     }
     if( strcasecmp(argv[iarg],"-l1fit") == 0 ){
       meth = 1 ; iarg++ ; continue ;
     }

     if( strcasecmp(argv[iarg],"-prefix") == 0 ){
       if( ++iarg >= argc )
         ERROR_exit("Need argument after %s",argv[iarg-1]);
       prefix = argv[iarg] ;
       if( !THD_filename_ok(prefix) )
         ERROR_exit("Illegal string after -prefix: '%s'",prefix) ;
       iarg++ ; continue ;
     }

     if( strcasecmp(argv[iarg],"-constrain") == 0 ){
       char *cpt , nvec ;
       if( ++iarg >= argc )
         ERROR_exit("Need argument after %s",argv[iarg-1]);
       if( convec != NULL )
         ERROR_exit("Can't have 2 -constrain options!") ;
       MAKE_intvec(convec,1) ;
       for( nvec=0 ; iarg < argc ; iarg++ ){
         ii = (int)strtod(argv[iarg],&cpt) ;
         if( ii == 0 || *cpt != '\0' ) break ;  /* bad */
         RESIZE_intvec(convec,nvec+1) ;
         convec->ar[nvec++] = ii ;
       }
       if( nvec < 1 )
         ERROR_exit("No legal values after -constrain?!") ;
       continue ;
     }

     ERROR_exit("Unknown argument on command line: '%s'",argv[iarg]) ;
   }

   /*------- check options for completeness and consistency -----*/

   if( rhset == NULL )
     ERROR_exit("No RHS dataset input!?") ;
   ntime = DSET_NVALS(rhset) ;
   if( ntime < 2 )
     ERROR_exit("RHS dataset has only 1 value per voxel?!") ;

   nx = DSET_NX(rhset); ny = DSET_NY(rhset); nz = DSET_NZ(rhset);
   nvox = nx*ny*nz;

   if( mask != NULL && (mnx != nx || mny != ny || mnz != nz) )
     ERROR_exit("mask and RHS datasets don't match in grid size") ;

   if( nvar < 1 )
     ERROR_exit("no LHS time series input?!") ;
   if( nvar >= ntime )
     ERROR_exit("too many (%d) LHS time series for %d time points",nvar,ntime) ;

   dvec = (float * )malloc(sizeof(float  )*ntime) ;  /* RHS vector */
   rvec = (float **)malloc(sizeof(float *)*nvar ) ;  /* LHS vectors */
   fvec = (float * )malloc(sizeof(float  )*nvar ) ;  /* fit vector */

   MAKE_intvec(kvec,dsar->num) ;
   for( kk=ii=0 ; ii < dsar->num ; ii++ ){ /* check LHS inputs; assign vectors */
     if( XTARR_IC(dsar,ii) == IC_FLIM ){
       float *lar ; int mm ;
       lim = (MRI_IMAGE *)XTARR_XT(dsar,ii) ;
       jj = lim->nx ; lar = MRI_FLOAT_PTR(lim) ;
       if( jj < ntime )
         ERROR_exit("LHS 1D file is too short: %d < %d",jj,ntime) ;
       if( jj > ntime )
         INFO_message("LHS 1D file too long: %d > %d: ignoring extra",jj,ntime);
       for( mm=0 ; mm < lim->ny ; mm++ ) rvec[kk++] = lar + mm*lim->nx ;
       kvec->ar[ii] = -1 ;
     } else if( XTARR_IC(dsar,ii) == IC_DSET ){
       lset = (THD_3dim_dataset *)XTARR_XT(dsar,ii) ;
       if( DSET_NX(lset) != nx || DSET_NY(lset) != ny || DSET_NZ(lset) != nz )
         ERROR_exit("LHS dataset %s doesn't match RHS dataset grid size",
                    DSET_HEADNAME(lset) ) ;
       jj = DSET_NVALS(lset) ;
       if( jj < ntime )
         ERROR_exit("LHS dataset is too short: %d < %d",jj,ntime) ;
       if( jj > ntime )
         INFO_message("LHR dataset too long: %d > %d: ignoring extra",jj,ntime);
       kvec->ar[ii] = kk ;  /* index of vector from this dataset */
       rvec[kk++] = (float *)malloc(sizeof(float)*jj) ;
     } else {
       ERROR_exit("This message should never be seen by mortal eyes!") ;
     }
   }

   if( convec != NULL ){
     cvec = (float *)calloc(sizeof(float),nvar) ;
     for( ii=0 ; ii < convec->nar ; ii++ ){
       kk = convec->ar[ii] ; jj = abs(kk) ;
       if( jj > nvar ) ERROR_exit("Oversized index %d in -constraint",jj) ;
       cvec[jj] = (kk < 0) ? -1.0f : 1.0f ;
     }
     flags = 1 ;
   }

   if( nlset == 0 )
     INFO_message("LHS datasets all 1D files ==> you could use 3dDeconvolve");

   /*----- load input datasets -----*/

   INFO_message("loading datasets") ;

   DSET_load(rhset) ; CHECK_LOAD_ERROR(rhset) ;
   for( ii=0 ; ii < dsar->num ; ii++ ){
     if( XTARR_IC(dsar,ii) == IC_DSET ){
       lset = (THD_3dim_dataset *)XTARR_XT(dsar,ii) ;
       DSET_load(lset) ; CHECK_LOAD_ERROR(lset) ;
     }
   }

   /*------ create output dataset ------*/

   fset = EDIT_empty_copy(rhset) ;
   EDIT_dset_items( fset ,
                      ADN_nvals     , nvar           ,
                      ADN_func_type , FUNC_BUCK_TYPE ,
                      ADN_type      , HEAD_FUNC_TYPE ,
                      ADN_datum_all , MRI_float      ,
                      ADN_brick_fac , NULL           ,
                      ADN_prefix    , prefix         ,
                    ADN_none ) ;
   tross_Copy_History( rhset , fset ) ;
   tross_Make_History( "3dTfitter" , argc,argv , fset ) ;
   for( jj=0 ; jj < nvar ; jj++ )
     EDIT_substitute_brick( fset , jj , MRI_float , NULL ) ;

   /*------- loop over voxels and process them ---------*/

   INFO_message("begin voxel loop: %d time points X %d fit parameters",
                ntime , nvar ) ;

   for( ii=0 ; ii < nvox ; ii++ ){

     if( mask != NULL && mask[ii] == 0 ) continue ;

     THD_extract_array( ii , rhset , 0 , dvec ) ;  /* data vector */

     for( jj=0 ; jj < dsar->num ; jj++ ){
       if( XTARR_IC(dsar,jj) == IC_DSET ){
         lset = (THD_3dim_dataset *)XTARR_XT(dsar,jj) ;
         kk = kvec->ar[jj] ;
         THD_extract_array( ii , lset , 0 , rvec[kk] ) ;
       }
     }

     if( cvec != NULL ) memcpy(fvec,cvec,sizeof(float)*nvar) ;

     bfit = THD_fitter( ntime , dvec , nvar , rvec , meth , flags ) ;

     if( bfit == NULL ){ nbad++; continue; } /* bad */

     THD_insert_series( ii , fset , nvar , MRI_float , bfit->ar , 1 ) ;

     KILL_floatvec(bfit) ; ngood++ ;
   }

   INFO_message("Fit worked in %d voxels; failed in %d",ngood,nbad) ;

   DSET_write(fset); WROTE_DSET(fset); exit(0);
}
