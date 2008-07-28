#include "mrilib.h"

#undef FLOATIZE      /* we will use double precision for matrices */
#include "remla.c"   /* do NOT change this to FLOATIZE !!! */

#undef  INMASK
#define INMASK(i) ( mask == NULL || mask[i] != 0 )

static void vstep_print(void)
{
   static char xx[10] = "0123456789" ; static int vn=0 ;
   fprintf(stderr , "%c" , xx[vn%10] ) ;
   if( vn%10 == 9) fprintf(stderr,".") ;
   vn++ ;
}

/*--------------------------------------------------------------------------*/

static int    Argc ;
static char **Argv ;

THD_3dim_dataset * create_float_dataset( THD_3dim_dataset *tset ,
                                         int nvol , char *prefix )
{
   THD_3dim_dataset *nset ; int kk ;

   if( tset == NULL || nvol < 1 || prefix == NULL ) return NULL ;

   nset = EDIT_empty_copy( tset ) ;
   EDIT_dset_items( nset ,
                      ADN_prefix    , prefix ,
                      ADN_brick_fac , NULL   ,
                      ADN_nvals     , nvol   ,
                      ADN_ntt       , nvol   ,
                    ADN_none ) ;
  tross_Copy_History( tset , nset ) ;
  tross_Make_History( "3dREMLfit" , Argc,Argv , nset ) ;
  for( kk=0 ; kk < nvol ; kk++ )
     EDIT_substitute_brick( nset , kk , MRI_float , NULL ) ;
  return nset ;
}

/*--------------------------------------------------------------------------*/

int main( int argc , char *argv[] )
{
   THD_3dim_dataset *inset=NULL ;
   byte *mask=NULL ; int mask_nx,mask_ny,mask_nz , automask=0 ;
   float *iv ;
   int iarg, ii,jj, nreg, ntime, *tau=NULL, rnum, nfull, nvals,nvox,vv ;
   NI_element *nelmat=NULL ; char *matname=NULL ;
   MTYPE rhomax=0.8 , bmax=0.8 ; int nlevab=3 ;
   char *cgl , *rst ;
   matrix X ; vector y ;
   reml_collection *rrcol ;
   int nprefixO=0 , nprefixR=0 , vstep ;
   char *Rbeta_prefix  = NULL ; THD_3dim_dataset *Rbeta_dset  = NULL ;
   char *Rvar_prefix   = NULL ; THD_3dim_dataset *Rvar_dset   = NULL ;
   char *Rfitts_prefix = NULL ; THD_3dim_dataset *Rfitts_dset = NULL ;
   char *Obeta_prefix  = NULL ; THD_3dim_dataset *Obeta_dset  = NULL ;
   char *Ovar_prefix   = NULL ; THD_3dim_dataset *Ovar_dset   = NULL ;
   char *Ofitts_prefix = NULL ; THD_3dim_dataset *Ofitts_dset = NULL ;
   int Ngoodlist,*goodlist=NULL , Nruns,*runs=NULL ;
   NI_int_array *giar ;
   float mfilt_radius=0.0 , dx,dy,dz ; int do_mfilt=0 , do_dxyz , nx,ny,nz ;
   MRI_IMAGE *aim , *bim ; float *aar , *bar ;

   /**------- help? -------**/

   Argc = argc ; Argv = argv ;

   if( argc < 2 || strcmp(argv[1],"-help") == 0 ){
     printf(
      "Usage: 3dREMLfit [option]\n"
      "Least squares fit with REML estimation of the ARMA(1,1) noise.\n"
      "\n"
      "Uses a matrix .xmat.1D file from 3dDeconvolve, and for each voxel,\n"
      "finds the best ARMA(1,1) model for the noise, and then finds the\n"
      "generalized (prewhitened) least squares fit of the matrix model\n"
      "to the signal in that voxel.  Intended to generalize the results of\n"
      "3dDeconvolve to allow for serial correlation in the timeseries data.\n"
      "\n"
      "Input Options (the first two are mandatory)\n"
      "-------------------------------------------\n"
      " -input ddd  = Read time series dataset 'ddd'.\n"
      " -matrix mmm = Read the matrix 'mmm', which should have been\n"
      "                 output from 3dDeconvolve via the '-x1D' option.\n"
      " -mask kkk   = Read dataset 'kkk' as a mask for the input.\n"
      " -automask   = If you don't know what this does by now, I'm not telling.\n"
      "\n"
      "Output Options (at least one must be given)\n"
      "-------------------------------------------\n"
      " -Rbeta_prefix  = dataset for beta weights from the REML estimation\n"
      " -Rvar_prefix   = dataset for REML variance parameters\n"
#if 0
      " -Rfitts_prefix = dataset for REML fitted model\n"
#endif
      "\n"
      " -Obeta_prefix  = dataset for beta weights from the OLSQ estimation\n"
      " -Ovar_prefix   = dataset for OLSQ variance parameter\n"
#if 0
      " -Ofitts_prefix = dataset for OLSQ fitted model\n"
#endif
      "\n"
      "The following options control the ARMA(1,1)\n"
      "parameter estimation for each voxel time series\n"
      "-----------------------------------------------\n"
      " -MAXa am   = Set max allowed AR a parameter to 'am' (default=0.8).\n"
      " -MAXb bm   = Set max allow MA b parameter to 'bm' (default=0.8).\n"
      "                The range of b values scanned is -bm .. +bm.\n"
      " -Grid pp   = Set the number of grid divisions in the (a,b) grid\n"
      "                to be 2^pp in each direction over the range 0..MAX.\n"
      "                The default and minimum value for 'pp' is 3.\n"
      "                Larger values will provide a finer resolution\n"
      "                in a and b, but at the cost of CPU time.\n"
      "               * To be clear, the default settings use a grid\n"
      "                   with 8 divisions in the a direction and 16 in\n"
      "                   the b direction (since a is non-negative but\n"
      "                   b can be either sign).\n"
      "               * If -NEGcor is used, then -Grid 3 means 16 divisions\n"
      "                   in each direction, so that the grid spacing is 0.1\n"
      "                   if MAX=0.8.  Similarly, -Grid 4 means 32 divisions\n"
      "                   in each direction, -Grid 5 means 64 divisions, etc.\n"
      "\n"
      " -NEGcor    = Allows negative correlations to be used; the default\n"
      "                is that only positive correlations are searched.\n"
      "                When this option is used, the range of a scanned\n"
      "                is -am .. +am; otherwise, it is 0 .. +am.\n"
      "                Note that when -NEGcor is used, the number of grid\n"
      "                points in the a direction doubles to cover the\n"
      "                range -am .. 0; this will slow the program down.\n"
      " -POScor    = Do not allow negative correlations.  Since this is\n"
      "                the default, you don't actually need this option.\n"
      "                [FMRI data doesn't seem to need the modeling  ]\n"
      "                [of negative correlations, but you never know.]\n"
      "\n"
      " -Mfilt mr  = After finding the best fit parameters for each voxel\n"
      "                in the mask, do a 3D median filter to smooth these\n"
      "                parameters over a ball with radius 'mr' mm, and then\n"
      "                use THOSE parameters to compute the final output.\n"
      "                N.B.: If mr < 0, -mr is the ball radius in voxels,\n"
      "                      instead of millimeters.\n"
      "                [No median filtering is done unless -Mfilt is used.]\n"
      "\n"
      " -CORcut cc = The exact ARMA(1,1) correlation matrix (for a != 0)\n"
      "                has no non-zero entries.  The calculations in this\n"
      "                program set correlations below a cutoff to zero.\n"
      "                The default cutoff is %.3f, but can be altered with\n"
      "                this option.  The only reason to use this option is\n"
      "                to test the sensitivity of the results to the cutoff.\n"
      "\n"
      "ARMA(1,1)\n"
      "---------\n"
      "* The correlation coefficient of noise samples 'k' units apart in time,\n"
      "    for k >= 1, is given by r(k) = lam * a^(k-1)\n"
      "    where                   lam  = (b+a)(1+a*b)/(1+2*a*b+b*b).\n"
      "* lam can be bigger or smaller than a, depending on the sign of b.\n"
      "* For the noise model which is the sum of AR(1) and white noise, lam < a.\n"
      "* The natural range of a and b is -1..+1.  However, unless -NEGcor is\n"
      "    given, only non-negative values of a will be used, and only values\n"
      "    of b that give lam > 0 will be allowed.\n"
      "* The program sets up the correlation matrix using the censoring and\n"
      "    run start information in the header of the .xmat.1D matrix file.\n"
      "\n"
      "REML = REsidual (or REstricted) Maximum Likelihood\n"
      "--------------------------------------------------\n"
      "* Ordinary least squares (assuming the noise correlation matrix is the\n"
      "    identity matrix) is consistent for estimating regression parameters,\n"
      "    but is not consistent for estimating the noise variance if the\n"
      "    noise is in fact correlated in time.\n"
      "* Maximum likelihood estimation (ML) of the regression parameters and\n"
      "    variance/correlation together is asymptotically consistent as the\n"
      "    number of samples goes to infinity, but the variance estimates\n"
      "    might still have significant bias at a 'reasonable' number of\n"
      "    data points.\n"
      "* REML estimates the variance/correlation parameters in a space\n"
      "    of residuals -- the part of the data left after the model fit\n"
      "    is subtracted.  The amusing part is that the model fit used is\n"
      "    the one where the variance/correlation matrix is the one found\n"
      "    by the REML fit itself.  This feature makes the process nonlinear,\n"
      "    and the REML equations are usually solved iteratively, to maximize\n"
      "    the log-likelihood in the restricted space.  In this program, the\n"
      "    REML function is instead simply optimized over a finite grid of\n"
      "    the correlation matrix parameters a and b.  The matrices for each\n"
      "    (a,b) pair are pre-calculated in the setup phase, and then are\n"
      "    re-used in the voxel loop.\n"
      "* REML estimates of the variance/correlation parameters are still\n"
      "    biased, but are generally less biased than ML estimates.  Also,\n"
      "    the regression parameters (betas) should be estimated somewhat\n"
      "    more accurately (i.e., with smaller variance than OLSQ).\n"
      "\n"
      "Nota Bene\n"
      "---------\n"
      "* ARMA(1,1) parameters 'a' (AR) and 'b' (MA) are estimated\n"
      "    only on a discrete grid, for the sake of CPU time.\n"
      "* Each voxel gets a separate pair of 'a' and 'b' parameters.\n"
      "* OLSQ = Ordinary Least Squares; these outputs can be used to\n"
      "         compare the REML estimations with the simpler OLSQ results.\n"
      "* All output datasets are in float format.  Calculations internally\n"
      "    are done in double precision.\n"
      "* Despite my best efforts, this program is somewhat slow.\n"
      "    Partly because it solves many linear systems for each voxel,\n"
      "    trying to find the 'best' ARMA(1,1) pre-whitening matrix.\n"
      "\n"
      "Future Dreams\n"
      "-------------\n"
      "* Add a -jobs option to use multiple CPUs.\n"
      "* Add a -Rfitts option to get the fitted time series model for\n"
      "    each voxel.\n"
      "* Compute F (and t?) statistics for the beta parameter collections.\n"
      "* Output variance estimates for the betas, so they can be carried\n"
      "    to the group analysis level.\n"
      "\n"
      "-----------------------\n"
      "-- RWCox - July 2008 --\n"
      "-----------------------\n" , corcut
     ) ;
     PRINT_COMPILE_DATE ; exit(0) ;
   }

   /*------- official startup ------*/

   PRINT_VERSION("3dREMLfit"); mainENTRY("3dREMLfit main"); machdep();
   AFNI_logger("3dREMLfit",argc,argv); AUTHOR("RWCox");

   /**------- scan command line --------**/

   iarg = 1 ;
   while( iarg < argc ){

      /** ARMA params **/

     if( strcmp(argv[iarg],"-MAXrho") == 0 || strcmp(argv[iarg],"-MAXa") == 0 ){
       if( ++iarg >= argc ) ERROR_exit("Need argument after '%s'",argv[iarg-1]) ;
       rhomax = (MTYPE)strtod(argv[iarg],NULL) ;
            if( rhomax < 0.3 ){ rhomax = 0.3; WARNING_message("-MAXa re-set to 0.3"); }
       else if( rhomax > 0.9 ){ rhomax = 0.9; WARNING_message("-MAXa re-set to 0.9"); }
       iarg++ ; continue ;
     }
     if( strcmp(argv[iarg],"-Grid") == 0 ){
       if( ++iarg >= argc ) ERROR_exit("Need argument after '%s'",argv[iarg-1]) ;
       nlevab = (int)strtod(argv[iarg],NULL) ;
            if( nlevab < 3 ){ nlevab = 3; WARNING_message("-Grid re-set to 3"); }
       else if( nlevab > 7 ){ nlevab = 7; WARNING_message("-Grid re-set to 7"); }
       iarg++ ; continue ;
     }
     if( strcmp(argv[iarg],"-MAXb") == 0 ){
       if( ++iarg >= argc ) ERROR_exit("Need argument after '%s'",argv[iarg-1]) ;
       bmax = (MTYPE)strtod(argv[iarg],NULL) ;
            if( bmax < 0.3 ){ bmax = 0.3; WARNING_message("-MAXb re-set to 0.3"); }
       else if( bmax > 0.9 ){ bmax = 0.9; WARNING_message("-MAXb re-set to 0.9"); }
       iarg++ ; continue ;
     }
     if( strcmp(argv[iarg],"-NEGcor") == 0 ){
       REML_allow_negative_correlations(1) ; iarg++ ; continue ;
     }
     if( strcmp(argv[iarg],"-POScor") == 0 ){
       REML_allow_negative_correlations(0) ; iarg++ ; continue ;
     }
     if( strcmp(argv[iarg],"-Mfilt") == 0 ){
       if( ++iarg >= argc ) ERROR_exit("Need argument after '%s'",argv[iarg-1]) ;
       mfilt_radius = (float)strtod(argv[iarg],NULL) ;
       do_mfilt = (mfilt_radius != 0.0f) ;
       do_dxyz  = (mfilt_radius > 0.0f) ;
       mri_medianfilter_usedxyz( do_dxyz ) ;
       if( mfilt_radius < 0.0f ) mfilt_radius = -mfilt_radius ;
       iarg++ ; continue ;
     }
     if( strcmp(argv[iarg],"-CORcut") == 0 ){
       if( ++iarg >= argc ) ERROR_exit("Need argument after '%s'",argv[iarg-1]) ;
       dx = (float)strtod(argv[iarg],NULL) ;
       if( dx > 0.0f && dx <= 0.1f ) corcut = (MTYPE)dx ;
       else WARNING_message("Illegal value after -CORcut -- ignoring it!") ;
       iarg++ ; continue ;
     }

     /** -matrix **/

     if( strcmp(argv[iarg],"-matrix") == 0 ){
       if( ++iarg >= argc ) ERROR_exit("Need argument after '%s'",argv[iarg-1]) ;
       if( nelmat != NULL ) ERROR_exit("More than 1 -matrix option!?");
       nelmat = NI_read_element_fromfile( argv[iarg] ) ; /* read NIML file */
       matname = argv[iarg];
       if( nelmat == NULL || nelmat->type != NI_ELEMENT_TYPE )
         ERROR_exit("Can't process -matrix file!?");
       iarg++ ; continue ;
     }

      /** -input **/

     if( strcmp(argv[iarg],"-input") == 0 ){
       if( inset != NULL  ) ERROR_exit("Can't have two -input options!?") ;
       if( ++iarg >= argc ) ERROR_exit("Need argument after '%s'",argv[iarg-1]) ;
       inset = THD_open_dataset( argv[iarg] ) ;
       CHECK_OPEN_ERROR(inset,argv[iarg]) ;
       iarg++ ; continue ;
     }

     /** -mask **/

     if( strcmp(argv[iarg],"-mask") == 0 ){
       THD_3dim_dataset *mset ; int mmm ;
       if( ++iarg >= argc ) ERROR_exit("Need argument after '-mask'") ;
       if( mask != NULL || automask ) ERROR_exit("Can't have two mask inputs") ;
       mset = THD_open_dataset( argv[iarg] ) ;
       CHECK_OPEN_ERROR(mset,argv[iarg]) ;
       DSET_load(mset) ; CHECK_LOAD_ERROR(mset) ;
       mask_nx = DSET_NX(mset); mask_ny = DSET_NY(mset); mask_nz = DSET_NZ(mset);
       mask = THD_makemask( mset , 0 , 0.5f, 0.0f ) ; DSET_delete(mset) ;
       if( mask == NULL ) ERROR_exit("Can't make mask from dataset '%s'",argv[iarg]) ;
       mmm = THD_countmask( mask_nx*mask_ny*mask_nz , mask ) ;
       INFO_message("Number of voxels in mask = %d",mmm) ;
       if( mmm < 2 ) ERROR_exit("Mask is too small to process") ;
       iarg++ ; continue ;
     }

     if( strcmp(argv[iarg],"-automask") == 0 ){
       if( mask != NULL ) ERROR_exit("Can't have -automask and -mask") ;
       automask = 1 ; iarg++ ; continue ;
     }

     /** prefix options */

     if( strcmp(argv[iarg],"-Rbeta_prefix") == 0 ){
       if( ++iarg >= argc ) ERROR_exit("Need argument after '%s'",argv[iarg-1]) ;
       Rbeta_prefix = strdup(argv[iarg]) ; nprefixR++ ;
       if( !THD_filename_ok(Rbeta_prefix) )
         ERROR_exit("Illegal string after -Rbeta_prefix") ;
       iarg++ ; continue ;
     }

     if( strcmp(argv[iarg],"-Rvar_prefix") == 0 ){
       if( ++iarg >= argc ) ERROR_exit("Need argument after '%s'",argv[iarg-1]) ;
       Rvar_prefix = strdup(argv[iarg]) ; nprefixR++ ;
       if( !THD_filename_ok(Rvar_prefix) )
         ERROR_exit("Illegal string after -Rvar_prefix") ;
       iarg++ ; continue ;
     }

     if( strcmp(argv[iarg],"-Rfitts_prefix") == 0 ){
       if( ++iarg >= argc ) ERROR_exit("Need argument after '%s'",argv[iarg-1]) ;
       Rfitts_prefix = strdup(argv[iarg]) ; nprefixR++ ;
       if( !THD_filename_ok(Rfitts_prefix) )
         ERROR_exit("Illegal string after -Rfitts_prefix") ;
       iarg++ ; continue ;
     }

     if( strcmp(argv[iarg],"-Obeta_prefix") == 0 ){
       if( ++iarg >= argc ) ERROR_exit("Need argument after '%s'",argv[iarg-1]) ;
       Obeta_prefix = strdup(argv[iarg]) ; nprefixO++ ;
       if( !THD_filename_ok(Obeta_prefix) )
         ERROR_exit("Illegal string after -Obeta_prefix") ;
       iarg++ ; continue ;
     }

     if( strcmp(argv[iarg],"-Ovar_prefix") == 0 ){
       if( ++iarg >= argc ) ERROR_exit("Need argument after '%s'",argv[iarg-1]) ;
       Ovar_prefix = strdup(argv[iarg]) ; nprefixO++ ;
       if( !THD_filename_ok(Ovar_prefix) )
         ERROR_exit("Illegal string after -Ovar_prefix") ;
       iarg++ ; continue ;
     }

     if( strcmp(argv[iarg],"-Ofitts_prefix") == 0 ){
       if( ++iarg >= argc ) ERROR_exit("Need argument after '%s'",argv[iarg-1]) ;
       Ofitts_prefix = strdup(argv[iarg]) ; nprefixO++ ;
       if( !THD_filename_ok(Ofitts_prefix) )
         ERROR_exit("Illegal string after -Ofitts_prefix") ;
       iarg++ ; continue ;
     }

     ERROR_exit("Unknown option '%s'",argv[iarg]) ;
   }

   /**-------- sanity checks, dataset input, maskifying --------**/

   if( inset             == NULL ) ERROR_exit("No -input dataset?!") ;
   if( nprefixR+nprefixO == 0    ) ERROR_exit("No output datasets?!") ;
   if( nprefixR          == 0    ) WARNING_message("No REML output datasets?!") ;

   INFO_message("Loading input dataset into memory") ;
   DSET_load(inset) ; CHECK_LOAD_ERROR(inset) ;
   nvals = DSET_NVALS(inset) ; nvox = DSET_NVOX(inset) ;
   dx = fabsf(DSET_DX(inset)) ; nx = DSET_NX(inset) ;
   dy = fabsf(DSET_DY(inset)) ; ny = DSET_NY(inset) ;
   dz = fabsf(DSET_DZ(inset)) ; nz = DSET_NZ(inset) ;

   if( mask != NULL ){
     if( mask_nx != nx || mask_ny != ny || mask_nz != nz )
       ERROR_exit("-mask dataset grid doesn't match input dataset") ;

   } else if( automask ){
     int mmm ;
     mask = THD_automask( inset ) ;
     if( mask == NULL )
       ERROR_message("Can't create -automask from input dataset?") ;
     mmm = THD_countmask( nvox , mask ) ;
     INFO_message("Number of voxels in automask = %d",mmm) ;
     if( mmm < 2 ) ERROR_exit("Automask is too small to process") ;
   }

   /**-------- process the matrix --------**/

   nreg  = nelmat->vec_num ;  /* number of matrix columns */
   ntime = nelmat->vec_len ;  /* number of matrix rows */

   /* number of rows in the full matrix */

   cgl = NI_get_attribute( nelmat , "NRowFull" ) ;
   if( cgl == NULL ) ERROR_exit("Matrix is missing 'NRowFull' attribute!") ;
   nfull = (int)strtod(cgl,NULL) ;
   if( nvals != nfull )
     ERROR_exit("Dataset has %d time points, but matrix indicates %d",
                nvals , nfull ) ;

   /* the goodlist = mapping from  matrix row index to time index */

   cgl = NI_get_attribute( nelmat , "GoodList" ) ;
   if( cgl == NULL ) ERROR_exit("Matrix is missing 'GoodList' attribute!") ;
   giar = NI_decode_int_list( cgl , ";," ) ;
   if( giar == NULL || giar->num < ntime )
     ERROR_exit("Matrix 'GoodList' badly formatted?!") ;
   Ngoodlist = giar->num ; goodlist = giar->ar ;
   if( Ngoodlist != ntime )
     ERROR_exit("Matrix 'GoodList' incorrect length?!") ;

   /* run starting points in time index */

   rst = NI_get_attribute( nelmat , "RunStart" ) ;
   if( rst != NULL ){
     NI_int_array *riar = NI_decode_int_list( rst , ";,") ;
     if( riar == NULL ) ERROR_exit("-matrix 'RunStart' badly formatted?") ;
     Nruns = riar->num ; runs = riar->ar ;
   } else {
     INFO_message("Matrix missing 'RunStart' attribute ==> assuming 1 run");
     Nruns = 1 ; runs = calloc(sizeof(int),1) ;
   }

   /* set up pseudo-time tau[] vector for R matrix formation */

   rnum = 0 ; tau = (int *)malloc(sizeof(int)*ntime) ;
   for( ii=0 ; ii < ntime ; ii++ ){
     jj = goodlist[ii] ;        /* time index of the ii-th matrix row */
                              /* then find which run this point is in */
     for( ; rnum+1 < Nruns && jj >= runs[rnum+1] ; rnum++ ) ;   /*nada*/
     tau[ii] = jj + 10000*rnum ;  /* the 10000 means 'very far apart' */
   }

   /* re-create the regression matrix X */

   matrix_initialize( &X ) ;
   matrix_create( ntime , nreg , &X ) ;
   if( nelmat->vec_typ[0] == NI_FLOAT ){
     float *cd ;
     for( jj=0 ; jj < nreg ; jj++ ){
       cd = (float *)nelmat->vec[jj] ;
       for( ii=0 ; ii < ntime ; ii++ ) X.elts[ii][jj] = (MTYPE)cd[ii] ;
     }
   } else if( nelmat->vec_typ[0] == NI_DOUBLE ){
     double *cd ;
     for( jj=0 ; jj < nreg ; jj++ ){
       cd = (double *)nelmat->vec[jj] ;
       for( ii=0 ; ii < ntime ; ii++ ) X.elts[ii][jj] = (MTYPE)cd[ii] ;
     }
   } else {
     ERROR_exit("-matrix file stored will illegal data type!?") ;
   }

   /**------- set up for REML estimation -------**/

   INFO_message("starting REML setup calculations") ;
   rrcol = REML_setup( &X , tau , nlevab,rhomax,bmax ) ;
   if( rrcol == NULL ) ERROR_exit("REML setup fails?!" ) ;
   INFO_message("REML setup finished: matrix rows=%d cols=%d; %d cases; CPU=%.2f",
                ntime,nreg,rrcol->nset,COX_cpu_time()) ;

   /*----- create output datasets -----*/

   Rbeta_dset = create_float_dataset( inset , nreg , Rbeta_prefix ) ;
   Rvar_dset  = create_float_dataset( inset , 4    , Rvar_prefix  ) ;
   if( Rvar_dset != NULL ){
     EDIT_BRICK_LABEL( Rvar_dset , 0 , "Variance" ) ;
     EDIT_BRICK_LABEL( Rvar_dset , 1 , "a" ) ;
     EDIT_BRICK_LABEL( Rvar_dset , 2 , "b" ) ;
     EDIT_BRICK_LABEL( Rvar_dset , 3 , "lam" ) ;
   }
#if 0
   Rfitts_dset= create_float_dataset( inset , nfull, Rfitts_prefix) ;
#endif

   Obeta_dset = create_float_dataset( inset , nreg , Obeta_prefix ) ;
   Ovar_dset  = create_float_dataset( inset , 1    , Ovar_prefix  ) ;
#if 0
   Ofitts_dset= create_float_dataset( inset , nfull, Ofitts_prefix) ;
#endif

   /***------- loop over voxels and process them ------***/

   vector_initialize( &y ) ; vector_create_noinit( ntime , &y ) ;
   iv = (float *)malloc(sizeof(float)*nvals) ;
   vstep = (nvox > 999) ? nvox/50 : 0 ;
   if( vstep ) fprintf(stderr,"++ voxel loop: ") ;

   if( do_mfilt ){
     aim = mri_new_vol( nx,ny,nz , MRI_float ) ;
     aim->dx = dx ; aim->dy = dy ; aim->dz = dz ; aar = MRI_FLOAT_PTR(aim) ;
     bim = mri_new_vol( nx,ny,nz , MRI_float ) ;
     bim->dx = dx ; bim->dy = dy ; bim->dz = dz ; bar = MRI_FLOAT_PTR(bim) ;
   }

   for( vv=0 ; vv < nvox ; vv++ ){
     if( vstep && vv%vstep==vstep-1 ) vstep_print() ;
     if( !INMASK(vv) ) continue ;
     (void)THD_extract_array( vv , inset , 0 , iv ) ;
     for( ii=0 ; ii < ntime ; ii++ ) y.elts[ii] = (MTYPE)iv[goodlist[ii]] ;
     (void)REML_find_best_case( &y , rrcol ) ;

     if( do_mfilt ){               /* save best REML index */
       aar[vv] = REML_best_rho ; bar[vv] = REML_best_bb ;
     } else {
       if( Rbeta_dset != NULL ){   /* save best REML results */
         for( ii=0 ; ii < nreg ; ii++ ) iv[ii] = REML_best_beta_vector.elts[ii] ;
         THD_insert_series( vv , Rbeta_dset , nreg , MRI_float , iv , 0 ) ;
       }
       if( Rvar_dset != NULL ){
         iv[0] = sqrt( REML_best_ssq / (ntime-nreg) ) ;
         iv[1] = REML_best_rho ; iv[2] = REML_best_bb ; iv[3] = REML_best_lam ;
         THD_insert_series( vv , Rvar_dset , 4 , MRI_float , iv , 0 ) ;
       }
     }

     if( Obeta_dset != NULL ){     /* save OLSQ results regardless */
       for( ii=0 ; ii < nreg ; ii++ ) iv[ii] = REML_olsq_beta_vector.elts[ii] ;
       THD_insert_series( vv , Obeta_dset , nreg , MRI_float , iv , 0 ) ;
     }
     if( Ovar_dset != NULL ){
       iv[0] = sqrt( REML_olsq_ssq / (ntime-nreg) ) ;
       THD_insert_series( vv , Ovar_dset , 1 , MRI_float , iv , 0 ) ;
     }

   }
   if( vstep ) fprintf(stderr,"\n") ;
   INFO_message("REML fitting done: CPU=%.2f",COX_cpu_time()) ;

   /**--- if doing median filter, must re-loop --**/

   if( do_mfilt ){
     MRI_IMAGE *afilt , *bfilt ;
     INFO_message("Median filtering best fit ARMA models") ;
     afilt = mri_medianfilter( aim , mfilt_radius , mask , 0 ) ;
     bfilt = mri_medianfilter( bim , mfilt_radius , mask , 0 ) ;
     if( afilt == NULL || bfilt == NULL ){
       WARNING_message("Median filter failed?! This is weird.") ;
     } else {
       mri_free(aim) ; aim = afilt ; aar = MRI_FLOAT_PTR(aim) ;
       mri_free(bim) ; bim = bfilt ; bar = MRI_FLOAT_PTR(bim) ;
     }
     if( vstep ) fprintf(stderr,"++ voxel loop: ") ;
     for( vv=0 ; vv < nvox ; vv++ ){
       if( vstep && vv%vstep==vstep-1 ) vstep_print() ;
       if( !INMASK(vv) ) continue ;
       (void)THD_extract_array( vv , inset , 0 , iv ) ;
       for( ii=0 ; ii < ntime ; ii++ ) y.elts[ii] = (MTYPE)iv[goodlist[ii]] ;
       jj = IAB(rrcol,aar[vv],bar[vv]) ;  /* closest point in the grid */
       if( rrcol->rs[jj] == NULL ){       /* try to fix up this oversight */
         int ia = jj % (1+rrcol->na); MTYPE aaa = rrcol->abot + ia * rrcol->da;
         int ib = jj / (1+rrcol->na); MTYPE bbb = rrcol->bbot + ib * rrcol->db;
         rrcol->rs[jj] = REML_setup_one( &X , tau , aaa,bbb ) ;
       }
       if( rrcol->rs[jj] == NULL ){ /* should not happen */
         ERROR_message("bad REML: voxel #%d (%d,%d,%d) has a=%.3f b=%.3f lam=%.3f jj=%d",
                         vv, DSET_index_to_ix(inset,vv) ,
                             DSET_index_to_jy(inset,vv) ,
                             DSET_index_to_kz(inset,vv) ,
                         aar[vv],bar[vv],LAMBDA(aar[vv],bar[vv]),jj) ;
       } else {
         (void)reml_func( &y , rrcol->rs[jj] , rrcol->X , rrcol->Xs ) ;
         if( Rbeta_dset != NULL ){
           for( ii=0 ; ii < nreg ; ii++ ) iv[ii] = bb5->elts[ii] ;
           THD_insert_series( vv , Rbeta_dset , nreg , MRI_float , iv , 0 ) ;
         }
         if( Rvar_dset != NULL ){
           iv[0] = sqrt( rsumq / (ntime-nreg) ) ;
           iv[1] = rrcol->rs[jj]->rho ; iv[2] = rrcol->rs[jj]->barm ;
           iv[3] = rrcol->rs[jj]->lam ;
           THD_insert_series( vv , Rvar_dset , 4 , MRI_float , iv , 0 ) ;
         }
       }
     }
     if( vstep ) fprintf(stderr,"\n") ;
     INFO_message("Recalculating done: CPU=%.2f",COX_cpu_time()) ;
   }

   if( Rbeta_dset != NULL ){ DSET_write(Rbeta_dset); WROTE_DSET(Rbeta_dset); }
   if( Rvar_dset  != NULL ){ DSET_write(Rvar_dset) ; WROTE_DSET(Rvar_dset ); }
   if( Obeta_dset != NULL ){ DSET_write(Obeta_dset); WROTE_DSET(Obeta_dset); }
   if( Ovar_dset  != NULL ){ DSET_write(Ovar_dset) ; WROTE_DSET(Ovar_dset ); }

   exit(0) ;
}
