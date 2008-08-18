#include "mrilib.h"

#undef FLOATIZE      /* we will use double precision for matrices */
#include "remla.c"   /* do NOT change this to FLOATIZE !!! */

#undef  INMASK
#define INMASK(i) ( mask == NULL || mask[i] != 0 )

/*--------------------------------------------------------------------------*/
/*! For voxel loop progress report. */

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

/*! To create an empty dataset. */

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
/*! This function creates a G matrix (for GLTs) that selects a subset
    of the full model;
     - mpar = number of parameters in the full model
            = number of columns in the output matrix
     - nrow = number of parameters of the subset model
            = number of rows in the output matrix
     - set  = array of indexes to select (nrow of them)
*//*------------------------------------------------------------------------*/

matrix * create_subset_matrix( int mpar , int nrow , int *set )
{
   matrix *gm ; int ii,jj ;

   if( mpar < 1 || nrow < 1 || nrow > mpar || set == NULL ) return NULL ;

   gm = (matrix *)malloc(sizeof(matrix)) ; matrix_initialize(gm) ;
   matrix_create( nrow , mpar , gm ) ;
   for( ii=0 ; ii < nrow ; ii++ ){    /* zero out entire matrix */
     for( jj=0 ; jj < mpar ; jj++ ) gm->elts[ii][jj] = 0.0 ;
   }
   for( ii=0 ; ii < nrow ; ii++ ){
     if( set[ii] < 0 || set[ii] >= mpar ){ free(gm); return NULL; }
     for( jj=0 ; jj < ii ; jj++ )
       if( set[ii] == set[jj] ){ free(gm); return NULL; }
     gm->elts[ii][set[ii]] = 1.0 ;  /* ii-th row selects set[ii] param */
   }
   return gm ;
}

/*--------------------------------------------------------------------------*/

int main( int argc , char *argv[] )
{
   THD_3dim_dataset *inset=NULL ;
   THD_3dim_dataset *abset=NULL ;
   MRI_IMAGE *aim=NULL, *bim=NULL ; float *aar, *bar ;
   byte *mask=NULL ; int mask_nx,mask_ny,mask_nz , automask=0 ;
   float *iv ;
   int iarg, ii,jj,kk, nreg, ntime, *tau=NULL, rnum, nfull, nvals,nvox,vv ;
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
   char *Rfstat_prefix = NULL ; THD_3dim_dataset *Rfstat_dset = NULL ;
   int Ngoodlist,*goodlist=NULL , Nruns,*runs=NULL ;
   NI_int_array *giar ; NI_str_array *gsar ; NI_float_array *gfar ;
   float mfilt_radius=0.0 , dx,dy,dz ; int do_mfilt=0 , do_dxyz , nx,ny,nz ;

   int glt_num=0 ; matrix **glt_mat=NULL ; char **glt_lab=NULL ;
   int stim_num=0; int *stim_bot , *stim_top ; char **stim_lab ;
   int do_fstat=0 , do_tstat=0 , do_stat=0 , do_stimstat=0 ;
   int num_allstim=0, *allstim=NULL , num_basetim=0, *basestim=NULL ;

   /**------- Get by with a little help from your friends? -------**/

   Argc = argc ; Argv = argv ;

   if( argc < 2 || strcasecmp(argv[1],"-help") == 0 ){
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
      "-------------------------------------------\n"
      "Input Options (the first two are mandatory)\n"
      "-------------------------------------------\n"
      " -input ddd  = Read time series dataset 'ddd'.\n"
      "\n"
      " -matrix mmm = Read the matrix 'mmm', which should have been\n"
      "                 output from 3dDeconvolve via the '-x1D' option.\n"
      "\n"
      " -mask kkk   = Read dataset 'kkk' as a mask for the input.\n"
      " -automask   = If you don't know what this does by now, I'm not telling.\n"
      "\n"
      "------------------------------------------------------------------------\n"
      "Output Options (at least one must be given; 'ppp' = dataset prefix name)\n"
      "------------------------------------------------------------------------\n"
      " -Rvar  ppp  = dataset for REML variance parameters\n"
      " -Rbeta ppp  = dataset for beta weights from the REML estimation\n"
#if 0
      " -Rfitts ppp = dataset for REML fitted model\n"
#endif
      "\n"
      " -Ovar ppp   = dataset for OLSQ variance parameter\n"
      " -Obeta ppp  = dataset for beta weights from the OLSQ estimation\n"
#if 0
      " -Ofitts ppp = dataset for OLSQ fitted model\n"
#endif
      "\n"
      "-------------------------------------------------------------------\n"
      "The following options control the ARMA(1,1) parameter estimation\n"
      "for each voxel time series; normally, you do not need these options\n"
      "-------------------------------------------------------------------\n"
      " -MAXa am   = Set max allowed AR a parameter to 'am' (default=0.8).\n"
      "                The range of a values scanned is 0 .. +am (-POScor)\n"
      "                or is -am .. +am (-NEGcor).\n"
      "\n"
      " -MAXb bm   = Set max allow MA b parameter to 'bm' (default=0.8).\n"
      "                The range of b values scanned is -bm .. +bm.\n"
      "\n"
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
      "                   I see no reason why you would ever use a -Grid size\n"
      "                   greater than 5 (==> parameter resolution = 0.025).\n"
      "\n"
      " -NEGcor    = Allows negative correlations to be used; the default\n"
      "                is that only positive correlations are searched.\n"
      "                When this option is used, the range of a scanned\n"
      "                is -am .. +am; otherwise, it is 0 .. +am.\n"
      "               * Note that when -NEGcor is used, the number of grid\n"
      "                   points in the a direction doubles to cover the\n"
      "                   range -am .. 0; this will slow the program down.\n"
      " -POScor    = Do not allow negative correlations.  Since this is\n"
      "                the default, you don't actually need this option.\n"
      "                [FMRI data doesn't seem to need the modeling  ]\n"
      "                [of negative correlations, but you never know.]\n"
      "\n"
      " -Mfilt mr  = After finding the best fit parameters for each voxel\n"
      "                in the mask, do a 3D median filter to smooth these\n"
      "                parameters over a ball with radius 'mr' mm, and then\n"
      "                use THOSE parameters to compute the final output.\n"
      "               *  N.B.: If mr < 0, -mr is the ball radius in voxels,\n"
      "                        instead of millimeters.\n"
      "                [No median filtering is done unless -Mfilt is used.]\n"
      "\n"
      " -CORcut cc = The exact ARMA(1,1) correlation matrix (for a != 0)\n"
      "                has no non-zero entries.  The calculations in this\n"
      "                program set correlations below a cutoff to zero.\n"
      "                The default cutoff is %.3f, but can be altered with\n"
      "                this option.  The only reason to use this option is\n"
      "                to test the sensitivity of the results to the cutoff.\n"
      "\n"
      " -ABfile ff = Instead of estimating the ARMA(a,b) parameters from the\n"
      "                data, read them from dataset 'ff', which should have\n"
      "                2 float-valued sub-bricks.\n"
      "               * Note that the (a,b) values read from this file will\n"
      "                   be mapped to the nearest ones on the (a,b) grid\n"
      "                   before being used to solve the generalized least\n"
      "                   squares problem.  For this reason, you may want\n"
      "                   to use '-Grid 5' to make the (a,b) grid finer.\n"
      "               * Using this option will skip the slowest part of\n"
      "                   the program, which is the scan (for each voxel)\n"
      "                   to find the optimal (a,b) parameters.\n"
      "\n"
      "==========================================================================\n"
      "=================================  NOTES  ================================\n"
      "==========================================================================\n"
      "\n"
      "------------------\n"
      "What is ARMA(1,1)?\n"
      "------------------\n"
      "* The correlation coefficient r(k) of noise samples k units apart in time,\n"
      "    for k >= 1, is given by r(k) = lam * a^(k-1)\n"
      "    where                   lam  = (b+a)(1+a*b)/(1+2*a*b+b*b)\n"
      "    (N.B.: lam=a when b=0 -- AR(1) noise has r(k)=a^k for k >= 0).\n"
      "    (N.B.: lam=b when a=0 -- MA(1) noise has r(k)=b for k=1, r(k)=0 for k>1.\n"
      "* lam can be bigger or smaller than a, depending on the sign of b.\n"
      "* For the noise model which is the sum of AR(1) and white noise, lam < a.\n"
      "* The natural range of a and b is -1..+1.  However, unless -NEGcor is\n"
      "    given, only non-negative values of a will be used, and only values\n"
      "    of b that give lam > 0 will be allowed.\n"
      "* The program sets up the correlation matrix using the censoring and run\n"
      "    start information saved in the header of the .xmat.1D matrix file, so\n"
      "    that the actual correlation matrix used will not be Toeplitz.\n"
      "* The 'Rvar' dataset has 4 sub-bricks with variance parameter estimates:\n"
      "    #0 = a = factor by which correlations decay from lag k to lag k+1\n"
      "    #1 = b parameter\n"
      "    #2 = lam (see the formula above) = correlation at lag 1\n"
      "    #3 = standard deviation of ARMA(1,1) noise\n"
      "* The 'Rbeta' dataset has the beta (model fit) parameters estimates\n"
      "    computed from the pre-whitened time series data in each voxel.\n"
      "\n"
      "-----------------------------------------------------------\n"
      "What is REML = REsidual (or REstricted) Maximum Likelihood?\n"
      "-----------------------------------------------------------\n"
      "* Ordinary least squares (which assumes the noise correlation matrix is\n"
      "    the identity) is consistent for estimating regression parameters,\n"
      "    but is not consistent for estimating the noise variance if the\n"
      "    noise is significantly correlated in time ('serial correlation').\n"
      "* Maximum likelihood estimation (ML) of the regression parameters and\n"
      "    variance/correlation together is asymptotically consistent as the\n"
      "    number of samples goes to infinity, but the variance estimates\n"
      "    might still have significant bias at a 'reasonable' number of\n"
      "    data points.\n"
      "* REML estimates the variance/correlation parameters in a space\n"
      "    of residuals -- the part of the data left after the model fit\n"
      "    is subtracted.  The amusing/cunning part is that the model fit\n"
      "    used to define the residuals is itself the generalized least\n"
      "    squares fit where the variance/correlation matrix is the one found\n"
      "    by the REML fit itself.  This feature makes REML estimation nonlinear,\n"
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
      "* In the case with b=0 (that is, AR(1) correlations), and if there are\n"
      "    no time gaps (no censoring, no run breaks), then it is possible to\n"
      "    directly estimate the a parameter without using REML.  This program\n"
      "    does not implement this special method.  Don't ask why.\n"
      "\n"
      "--------------\n"
      "Other Comments\n"
      "--------------\n"
      "* ARMA(1,1) parameters 'a' (AR) and 'b' (MA) are estimated\n"
      "    only on a discrete grid, for the sake of CPU time.\n"
      "* Each voxel gets a separate pair of 'a' and 'b' parameters.\n"
      "* OLSQ = Ordinary Least Squares; these outputs can be used to\n"
      "         compare the REML estimations with the simpler OLSQ results.\n"
      "* The OLSQ datasets will NOT be computed if -ABfile is given, since\n"
      "    these results are only calculated during the REML fitting phase.\n"
      "* All output datasets are in float format.  Calculations internally\n"
      "    are done in double precision.\n"
      "* Despite my best efforts, this program is somewhat slow.\n"
      "    Partly because it solves many linear systems for each voxel,\n"
      "    trying to find the 'best' ARMA(1,1) pre-whitening matrix.\n"
      "\n"
      "-------------\n"
      "Future Dreams\n"
      "-------------\n"
      "* Add a -jobs option to use multiple CPUs (or multiple Steves?).\n"
      "* Add a -Rfitts option to get the fitted time series model for\n"
      "    each voxel.\n"
      "* Compute F and t statistics for the beta parameter collections,\n"
      "    and for GLTs.\n"
      "* Output variance estimates for the betas, so they can be carried\n"
      "    to the group analysis level.\n"
      "* Prove that Apery's constant is transcendental.\n"
      "\n"
		"=======================\n"
      "== RWCox - July 2008 ==\n"
      "=======================\n" , corcut
     ) ;
     PRINT_COMPILE_DATE ; exit(0) ;
   }

   /*------- official startup ------*/

   PRINT_VERSION("3dREMLfit"); mainENTRY("3dREMLfit main"); machdep();
   AFNI_logger("3dREMLfit",argc,argv); AUTHOR("RWCox");

   /**------- scan command line --------**/

   iarg = 1 ;
   while( iarg < argc ){

      /** ABfile **/

     if( strcasecmp(argv[iarg],"-ABfile") == 0 ){
       if( abset != NULL ) ERROR_exit("Can't have 2 '%s' options",argv[iarg]) ;
       if( ++iarg >= argc ) ERROR_exit("Need argument after '%s'",argv[iarg-1]) ;
       abset = THD_open_dataset( argv[iarg] ) ;
       CHECK_OPEN_ERROR(abset,argv[iarg]) ;
       iarg++ ; continue ;
     }

      /** ARMA params **/

     if( strcasecmp(argv[iarg],"-MAXrho") == 0 || strcasecmp(argv[iarg],"-MAXa") == 0 ){
       if( ++iarg >= argc ) ERROR_exit("Need argument after '%s'",argv[iarg-1]) ;
       rhomax = (MTYPE)strtod(argv[iarg],NULL) ;
            if( rhomax < 0.3 ){ rhomax = 0.3; WARNING_message("-MAXa re-set to 0.3"); }
       else if( rhomax > 0.9 ){ rhomax = 0.9; WARNING_message("-MAXa re-set to 0.9"); }
       iarg++ ; continue ;
     }
     if( strcasecmp(argv[iarg],"-Grid") == 0 ){
       if( ++iarg >= argc ) ERROR_exit("Need argument after '%s'",argv[iarg-1]) ;
       nlevab = (int)strtod(argv[iarg],NULL) ;
            if( nlevab < 3 ){ nlevab = 3; WARNING_message("-Grid re-set to 3"); }
       else if( nlevab > 7 ){ nlevab = 7; WARNING_message("-Grid re-set to 7"); }
       iarg++ ; continue ;
     }
     if( strcasecmp(argv[iarg],"-MAXb") == 0 ){
       if( ++iarg >= argc ) ERROR_exit("Need argument after '%s'",argv[iarg-1]) ;
       bmax = (MTYPE)strtod(argv[iarg],NULL) ;
            if( bmax < 0.3 ){ bmax = 0.3; WARNING_message("-MAXb re-set to 0.3"); }
       else if( bmax > 0.9 ){ bmax = 0.9; WARNING_message("-MAXb re-set to 0.9"); }
       iarg++ ; continue ;
     }
     if( strcasecmp(argv[iarg],"-NEGcor") == 0 ){
       REML_allow_negative_correlations(1) ; iarg++ ; continue ;
     }
     if( strcasecmp(argv[iarg],"-POScor") == 0 ){
       REML_allow_negative_correlations(0) ; iarg++ ; continue ;
     }
     if( strcasecmp(argv[iarg],"-Mfilt") == 0 ){
       if( ++iarg >= argc ) ERROR_exit("Need argument after '%s'",argv[iarg-1]) ;
       mfilt_radius = (float)strtod(argv[iarg],NULL) ;
       do_mfilt = (mfilt_radius != 0.0f) ;
       do_dxyz  = (mfilt_radius > 0.0f) ;
       mri_medianfilter_usedxyz( do_dxyz ) ;
       if( mfilt_radius < 0.0f ) mfilt_radius = -mfilt_radius ;
       iarg++ ; continue ;
     }
     if( strcasecmp(argv[iarg],"-CORcut") == 0 ){
       if( ++iarg >= argc ) ERROR_exit("Need argument after '%s'",argv[iarg-1]) ;
       dx = (float)strtod(argv[iarg],NULL) ;
       if( dx > 0.0f && dx <= 0.1f ) corcut = (MTYPE)dx ;
       else WARNING_message("Illegal value after -CORcut -- ignoring it!") ;
       iarg++ ; continue ;
     }

     /** -matrix **/

     if( strcasecmp(argv[iarg],"-matrix") == 0 ){
       if( ++iarg >= argc ) ERROR_exit("Need argument after '%s'",argv[iarg-1]) ;
       if( nelmat != NULL ) ERROR_exit("More than 1 -matrix option!?");
       nelmat = NI_read_element_fromfile( argv[iarg] ) ; /* read NIML file */
       matname = argv[iarg];
       if( nelmat == NULL || nelmat->type != NI_ELEMENT_TYPE )
         ERROR_exit("Can't process -matrix file!?");
       iarg++ ; continue ;
     }

      /** -input **/

     if( strcasecmp(argv[iarg],"-input") == 0 ){
       if( inset != NULL  ) ERROR_exit("Can't have two -input options!?") ;
       if( ++iarg >= argc ) ERROR_exit("Need argument after '%s'",argv[iarg-1]) ;
       inset = THD_open_dataset( argv[iarg] ) ;
       CHECK_OPEN_ERROR(inset,argv[iarg]) ;
       iarg++ ; continue ;
     }

     /** -mask **/

     if( strcasecmp(argv[iarg],"-mask") == 0 ){
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

     if( strcasecmp(argv[iarg],"-automask") == 0 ){
       if( mask != NULL ) ERROR_exit("Can't have -automask and -mask") ;
       automask = 1 ; iarg++ ; continue ;
     }

     /** prefix options */

     if( strncasecmp(argv[iarg],"-Rbeta_prefix",5) == 0 ){
       if( ++iarg >= argc ) ERROR_exit("Need argument after '%s'",argv[iarg-1]) ;
       Rbeta_prefix = strdup(argv[iarg]) ; nprefixR++ ;
       if( !THD_filename_ok(Rbeta_prefix) )
         ERROR_exit("Illegal string after -Rbeta_prefix") ;
       iarg++ ; continue ;
     }

     if( strncasecmp(argv[iarg],"-Rvar_prefix",5) == 0 ){
       if( ++iarg >= argc ) ERROR_exit("Need argument after '%s'",argv[iarg-1]) ;
       Rvar_prefix = strdup(argv[iarg]) ; nprefixR++ ;
       if( !THD_filename_ok(Rvar_prefix) )
         ERROR_exit("Illegal string after -Rvar_prefix") ;
       iarg++ ; continue ;
     }

     if( strncasecmp(argv[iarg],"-Rfitts_prefix",5) == 0 ){
       if( ++iarg >= argc ) ERROR_exit("Need argument after '%s'",argv[iarg-1]) ;
       Rfitts_prefix = strdup(argv[iarg]) ; nprefixR++ ;
       if( !THD_filename_ok(Rfitts_prefix) )
         ERROR_exit("Illegal string after -Rfitts_prefix") ;
       iarg++ ; continue ;
     }

     if( strncasecmp(argv[iarg],"-Obeta_prefix",5) == 0 ){
       if( ++iarg >= argc ) ERROR_exit("Need argument after '%s'",argv[iarg-1]) ;
       Obeta_prefix = strdup(argv[iarg]) ; nprefixO++ ;
       if( !THD_filename_ok(Obeta_prefix) )
         ERROR_exit("Illegal string after -Obeta_prefix") ;
       iarg++ ; continue ;
     }

     if( strncasecmp(argv[iarg],"-Ovar_prefix",5) == 0 ){
       if( ++iarg >= argc ) ERROR_exit("Need argument after '%s'",argv[iarg-1]) ;
       Ovar_prefix = strdup(argv[iarg]) ; nprefixO++ ;
       if( !THD_filename_ok(Ovar_prefix) )
         ERROR_exit("Illegal string after -Ovar_prefix") ;
       iarg++ ; continue ;
     }

     if( strncasecmp(argv[iarg],"-Ofitts_prefix",5) == 0 ){
       if( ++iarg >= argc ) ERROR_exit("Need argument after '%s'",argv[iarg-1]) ;
       Ofitts_prefix = strdup(argv[iarg]) ; nprefixO++ ;
       if( !THD_filename_ok(Ofitts_prefix) )
         ERROR_exit("Illegal string after -Ofitts_prefix") ;
       iarg++ ; continue ;
     }

     if( strncasecmp(argv[iarg],"-Rfstat_prefix",5) == 0 ){
       if( ++iarg >= argc ) ERROR_exit("Need argument after '%s'",argv[iarg-1]) ;
       Rfstat_prefix = strdup(argv[iarg]) ; nprefixO++ ;
       if( !THD_filename_ok(Rfstat_prefix) )
         ERROR_exit("Illegal string after -Rfstat_prefix") ;
       do_fstat = 1 ;
       iarg++ ; continue ;
     }

     ERROR_exit("Unknown option '%s'",argv[iarg]) ;
   }

   /**-------- sanity checks, dataset input, maskifying --------**/

   if( inset             == NULL ) ERROR_exit("No -input dataset?!") ;
   if( nprefixR+nprefixO == 0    ) ERROR_exit("No output datasets at all?!") ;
   if( nprefixR          == 0    ) WARNING_message("No REML output datasets?!") ;

   nvals = DSET_NVALS(inset) ; nvox = DSET_NVOX(inset) ;
   dx = fabsf(DSET_DX(inset)) ; nx = DSET_NX(inset) ;
   dy = fabsf(DSET_DY(inset)) ; ny = DSET_NY(inset) ;
   dz = fabsf(DSET_DZ(inset)) ; nz = DSET_NZ(inset) ;

   do_stimstat = do_stat = (do_fstat || do_tstat) ;

   /*-- check out and read the -ABfile, if there is one --*/

   if( abset != NULL ){
     float abot,atop , bbot,btop ;
     if( DSET_NX(abset) != nx || DSET_NY(abset) != ny || DSET_NZ(abset) != nz )
       ERROR_exit("-input and -ABfile datasets don't match grid sizes!") ;
     if( DSET_NVALS(abset) < 2 )
       ERROR_exit("-ABfile must have (at least) 2 sub-bricks!") ;
     else if( DSET_NVALS(abset) > 2 )
       WARNING_message("-ABfile has %d sub-bricks: only using first 2" ,
                       DSET_NVALS(abset) ) ;
     if( DSET_BRICK_TYPE(abset,0) != MRI_float ||
         DSET_BRICK_TYPE(abset,1) != MRI_float   )
       ERROR_exit("-ABfile sub-bricks are not stored as floats!") ;

     DSET_load(abset) ; CHECK_LOAD_ERROR(abset) ;
     aim = DSET_BRICK(abset,0); abot = mri_min(aim); atop = mri_max(aim);
     bim = DSET_BRICK(abset,0); bbot = mri_min(bim); btop = mri_max(bim);
     if( abot < -0.9f || atop > 0.9f || bbot < -0.9f || btop > 0.9f )
       ERROR_exit("-ABfile (a,b) values out of range -0.9..+0.9") ;

     REML_allow_negative_correlations(1) ;
     if( do_mfilt ){
       do_mfilt = 0 ; WARNING_message("-ABfile disables -Mfilt") ;
     }

     atop = fabsf(atop) ; abot = fabsf(abot) ; rhomax = MAX(abot,atop) ;
     btop = fabsf(btop) ; bbot = fabsf(bbot) ; bmax   = MAX(bbot,btop) ;
     if( rhomax < 0.1 ) rhomax = 0.1 ;
     if( bmax   < 0.1 ) bmax   = 0.1 ;
     INFO_message("-ABfile sets (a,b) grid to %+.3f..%+.3f X %+.3f..%+.3f",
                  -rhomax,rhomax , -bmax,bmax ) ;

     aim->dx = dx ; aim->dy = dy ; aim->dz = dz ; aar = MRI_FLOAT_PTR(aim) ;
     bim->dx = dx ; bim->dy = dy ; bim->dz = dz ; bar = MRI_FLOAT_PTR(bim) ;

     if( Obeta_prefix != NULL ){
       WARNING_message("-Obeta file won't be created due to -ABfile option");
       Obeta_prefix = NULL ;
     }
     if( Ovar_prefix != NULL ){
       WARNING_message("-Ovar file won't be created due to -ABfile option");
       Ovar_prefix = NULL ;
     }
     if( Ofitts_prefix != NULL ){
       WARNING_message("-Ofitts file won't be created due to -ABfile option");
       Ofitts_prefix = NULL ;
     }
   }

   /*-- process the mask --*/

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

   /* the goodlist = mapping from matrix row index to time index */

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
     if( riar == NULL ) ERROR_exit("Matrix 'RunStart' badly formatted?") ;
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
     ERROR_exit("Matrix file stored will illegal data type!?") ;
   }

   /* extract stim information from matrix header */

   cgl = NI_get_attribute( nelmat , "Nstim" ) ;
   if( cgl != NULL ){
     stim_num = (int)strtod(cgl,NULL) ;
     if( stim_num <= 0 ) ERROR_exit("Nstim attribute in matrix is not positive!");

     cgl = NI_get_attribute( nelmat , "StimBots" ) ;
     if( cgl == NULL ) ERROR_exit("Matrix is missing 'StimBots' attribute!") ;
     giar = NI_decode_int_list( cgl , ";," ) ;
     if( giar == NULL || giar->num < stim_num )
       ERROR_exit("Matrix 'StimBots' badly formatted?!") ;
     stim_bot = giar->ar ;

     cgl = NI_get_attribute( nelmat , "StimTops" ) ;
     if( cgl == NULL ) ERROR_exit("Matrix is missing 'StimTops' attribute!") ;
     giar = NI_decode_int_list( cgl , ";," ) ;
     if( giar == NULL || giar->num < stim_num )
       ERROR_exit("Matrix 'StimTops' badly formatted?!") ;
     stim_top = giar->ar ;

     cgl = NI_get_attribute( nelmat , "StimLabels" ) ;
     if( cgl == NULL ) ERROR_exit("Matrix is missing 'StimLabels' attribute!");
     gsar = NI_decode_string_list( cgl , ";" ) ;
     if( gsar == NULL || gsar->num < stim_num )
       ERROR_exit("Matrix 'StimLabels' badly formatted?!") ;
     stim_lab = gsar->str ;
   } else {
     WARNING_message("Matrix file is missing Stim attributes") ;
     if( do_stimstat ){
       WARNING_message("==> Can't do statistics on the Stimuli") ;
       do_stimstat = 0 ;
     }
   }

   /* setup to do statistics on the stimuli betas, if desired */

#undef  ADD_GLT
#define ADD_GLT(lb,gg)                                                            \
 do{ glt_lab = (char **  )realloc((void *)glt_lab, sizeof(char *  )*(glt_num+1)); \
     glt_mat = (matrix **)realloc((void *)glt_mat, sizeof(matrix *)*(glt_num+1)); \
     glt_lab[glt_num] = strdup(lb); glt_mat[glt_num] = gg; glt_num++;             \
 } while(0)

   if( do_stimstat ){
     char lnam[32] ; int nn,mm ; matrix *gm ;
     int *set = (int *)malloc(sizeof(int)*nreg) ;  /* list of columns to keep */

     /* first set is all stimuli */

     for( kk=jj=0 ; jj < stim_num ; jj++ ){
       for( ii=stim_bot[jj] ; ii <= stim_top[jj] ; ii++ ) set[kk++] = ii ;
     }
     gm = create_subset_matrix( nreg , kk , set ) ;
     if( gm == NULL ) ERROR_exit("Can't create G matrix for FullModel?!") ;
     ADD_GLT( "FullModel" , gm ) ;

     allstim = (int *)malloc(sizeof(int)*kk) ; num_allstim = kk ;
     memcpy( allstim , set , sizeof(int)*num_allstim ) ;
     qsort_int( num_allstim , allstim ) ;

#if 0
     /* now do each stimulus separately */

     if( stim_num > 0 ){
       for( jj=0 ; jj < stim_num ; jj++ ){
         for( kk=0,ii=stim_bot[jj] ; ii <= stim_top[jj] ; ii++ ) set[kk++] = ii ;
         gm = create_subset_matrix( nreg , kk , set ) ;
         if( gm == NULL ) ERROR_exit("Can't create G matrix for %s?!",stim_lab[jj]);
         ADD_GLT( stim_lab[jj] , gm ) ;
       }
     }
#endif

     free((void *)set) ;  /* not needed no more, no how */
   }

   /* extract other GLT information from matrix header */

#if 0
   cgl = NI_get_attribute( nelmat , "Nglt" ) ;
   if( cgl != NULL && do_stat ){
     char lnam[32] ; int nn,mm,ngl ; matrix *gm ; float *far ;

     ngl = (int)strtod(cgl,NULL) ;
     if( ngl <= 0 ) ERROR_exit("Nglt attribute in matrix is not positive!");

     cgl = NI_get_attribute( nelmat , "GltLabels" ) ;
     if( cgl == NULL ) ERROR_exit("Matrix is missing 'GltLabels' attribute!");
     gsar = NI_decode_string_list( cgl , ";" ) ;
     if( gsar == NULL || gsar->num < ngl )
       ERROR_exit("Matrix 'GltLabels' badly formatted?!") ;

     for( jj=0 ; jj < ngl ; jj++ ){
       sprintf(lnam,"GltMatrix_%06d",jj) ;
       cgl = NI_get_attribute( nelmat , lnam ) ;
       if( cgl == NULL ) ERROR_exit("Matrix is missing '%s' attribute!",lnam) ;
       gfar = NI_decode_float_list( cgl , "," ) ;
       if( gfar == NULL || gfar->num < 3 )
         ERROR_exit("Matrix attribute '%s' is badly formatted?!",lnam) ;
       far = gfar->ar ; nn = (int)far[0] ; mm = (int)far[1] ;
       if( nn <= 0 ) ERROR_exit("GLT '%s' has %d rows?",lnam,nn) ;
       if( mm != nreg )
         ERROR_exit("GLT '%s' has %d columns (should be %d)?",lnam,mm,nreg) ;
       gm = (matrix *)malloc(sizeof(matrix)) ; matrix_initialize(gm) ;
       matrix_create( nn, mm, gm ) ;
       for( ii=0 ; ii < nn ; ii++ ){
         for( jj=0 ; jj < mm ; jj++ ) gm->elts[ii][jj] = far[jj+2+ii*mm] ;
       }
       ADD_GLT( gsar->str[ii] , gm ) ;
     }

     INFO_message("Read %d GLTs from matrix header",ngl) ;
   }
#endif

   /**------- set up for REML estimation -------**/

   INFO_message("Loading input dataset into memory") ;
   DSET_load(inset) ; CHECK_LOAD_ERROR(inset) ;

   INFO_message("starting REML setup calculations") ;
   rrcol = REML_setup_all( &X , tau , nlevab,rhomax,bmax ) ;
   if( rrcol == NULL ) ERROR_exit("REML setup fails?!" ) ;

   if( glt_num > 0 )
     INFO_message("adding %d statistics matri%s to REML setup",
                  glt_num , (glt_num==1)?"x":"ces" ) ;
   for( kk=0 ; kk < glt_num ; kk++ )
     REML_add_glt_to_all( rrcol , glt_mat[kk]) ;

   INFO_message("REML setup finished: matrix rows=%d cols=%d; %d cases; CPU=%.2f",
                ntime,nreg,rrcol->nset,COX_cpu_time()) ;

   /*----- create output datasets -----*/

   Obeta_dset = create_float_dataset( inset , nreg , Obeta_prefix ) ;
   Ovar_dset  = create_float_dataset( inset , 1    , Ovar_prefix  ) ;
   Ofitts_dset= create_float_dataset( inset , nfull, Ofitts_prefix) ;

   /***------- loop over voxels, find best REML values ------***/

   if( aim == NULL ){  /*--- if we don't already have (a,b) ---*/

     aim = mri_new_vol( nx,ny,nz , MRI_float ) ;
     aim->dx = dx ; aim->dy = dy ; aim->dz = dz ; aar = MRI_FLOAT_PTR(aim) ;
     bim = mri_new_vol( nx,ny,nz , MRI_float ) ;
     bim->dx = dx ; bim->dy = dy ; bim->dz = dz ; bar = MRI_FLOAT_PTR(bim) ;

     vector_initialize( &y ) ; vector_create_noinit( ntime , &y ) ;
     iv = (float *)malloc(sizeof(float)*nvals) ;
     vstep = (nvox > 999) ? nvox/50 : 0 ;
     if( vstep ) fprintf(stderr,"++ REML voxel loop: ") ;

     for( vv=0 ; vv < nvox ; vv++ ){
       if( vstep && vv%vstep==vstep-1 ) vstep_print() ;
       if( !INMASK(vv) ) continue ;
       (void)THD_extract_array( vv , inset , 0 , iv ) ;
       for( ii=0 ; ii < ntime ; ii++ ) y.elts[ii] = (MTYPE)iv[goodlist[ii]] ;
       (void)REML_find_best_case( &y , rrcol ) ;
       aar[vv] = REML_best_rho ; bar[vv] = REML_best_bb ;

       if( Obeta_dset != NULL ){     /* save OLSQ results? */
         for( ii=0 ; ii < nreg ; ii++ ) iv[ii] = REML_olsq_beta_vector.elts[ii] ;
         THD_insert_series( vv , Obeta_dset , nreg , MRI_float , iv , 0 ) ;
       }
       if( Ovar_dset != NULL ){
         iv[0] = sqrt( REML_olsq_ssq / (ntime-nreg) ) ;
         THD_insert_series( vv , Ovar_dset , 1 , MRI_float , iv , 0 ) ;
       }

     }
     if( vstep ) fprintf(stderr,"\n") ;
     INFO_message("REML estimation done: CPU=%.2f",COX_cpu_time()) ;

     if( Obeta_dset != NULL ){
       DSET_write(Obeta_dset); WROTE_DSET(Obeta_dset); DSET_delete(Obeta_dset);
     }
     if( Ovar_dset  != NULL ){
       DSET_write(Ovar_dset); WROTE_DSET(Ovar_dset); DSET_delete(Ovar_dset);
     }
     if( Ofitts_dset != NULL ){
       DSET_write(Ofitts_dset); WROTE_DSET(Ofitts_dset); DSET_delete(Ofitts_dset);
     }

     /*-- median filter? --*/

     if( do_mfilt ){
       MRI_IMAGE *afilt , *bfilt ;
       INFO_message("Median filtering best fit ARMA models") ;
       afilt = mri_medianfilter( aim , mfilt_radius , mask , 0 ) ;
       bfilt = mri_medianfilter( bim , mfilt_radius , mask , 0 ) ;
       if( afilt == NULL || bfilt == NULL ){
         WARNING_message("Median filter failed?! This is weird.") ;
         mri_free(afilt) ; mri_free(bfilt) ;
       } else {
         mri_free(aim) ; aim = afilt ; aar = MRI_FLOAT_PTR(aim) ;
         mri_free(bim) ; bim = bfilt ; bar = MRI_FLOAT_PTR(bim) ;
       }
     }

   }

   /*-- at this point, aim and bim contain the (a,b) parameters --*/
   /*-- (either from -ABfile or from REML loop just done above) --*/

   /*-- now, use these values to compute the Generalized Least  --*/
   /*-- Squares (GLSQ) solution at each voxel, and save results --*/

   Rbeta_dset = create_float_dataset( inset , nreg , Rbeta_prefix ) ;
   Rvar_dset  = create_float_dataset( inset , 4    , Rvar_prefix  ) ;
   if( Rvar_dset != NULL ){
     EDIT_BRICK_LABEL( Rvar_dset , 0 , "a" ) ;
     EDIT_BRICK_LABEL( Rvar_dset , 1 , "b" ) ;
     EDIT_BRICK_LABEL( Rvar_dset , 2 , "lam" ) ;
     EDIT_BRICK_LABEL( Rvar_dset , 3 , "StDev" ) ;
   }
   Rfitts_dset = create_float_dataset( inset , nfull, Rfitts_prefix ) ;

   Rfstat_dset = create_float_dataset( inset , glt_num, Rfstat_prefix ) ;
   if( Rfstat_dset != NULL ){
     EDIT_BRICK_LABEL( Rfstat_dset , 0 , "FullF" ) ;
   }

   if( vstep ) fprintf(stderr,"++ GLSQ voxel loop: ") ;
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
       for( kk=0 ; kk < glt_num ; kk++ )
         REML_add_glt_to_one( rrcol->rs[jj] , glt_mat[kk]) ;
     }
     if( rrcol->rs[jj] == NULL ){ /* should never happen */
       ERROR_message("bad REML: voxel #%d (%d,%d,%d) has a=%.3f b=%.3f lam=%.3f jj=%d",
                       vv, DSET_index_to_ix(inset,vv) ,
                           DSET_index_to_jy(inset,vv) ,
                           DSET_index_to_kz(inset,vv) ,
                       aar[vv],bar[vv],LAMBDA(aar[vv],bar[vv]),jj) ;
     } else {
       (void)REML_func( &y , rrcol->rs[jj] , rrcol->X , rrcol->Xs ) ;
       if( Rbeta_dset != NULL ){
         for( ii=0 ; ii < nreg ; ii++ ) iv[ii] = bb5->elts[ii] ;
         THD_insert_series( vv , Rbeta_dset , nreg , MRI_float , iv , 0 ) ;
       }
       if( Rvar_dset != NULL ){
         iv[0] = rrcol->rs[jj]->rho ; iv[1] = rrcol->rs[jj]->barm ;
         iv[2] = rrcol->rs[jj]->lam ; iv[3] = sqrt( rsumq / (ntime-nreg) ) ;
         THD_insert_series( vv , Rvar_dset , 4 , MRI_float , iv , 0 ) ;
       }
       if( glt_num > 0 && Rfstat_dset != NULL ){
         if( rrcol->rs[jj]->glt == NULL )
           REML_add_glt_to_one( rrcol->rs[jj] , glt_mat[0] ) ;

         iv[0] = REML_compute_fstat( &y , bb5 , rsumq ,
                                     rrcol->rs[jj] , rrcol->rs[jj]->glt[0] ,
                                     rrcol->X , rrcol->Xs ) ;
         THD_insert_series( vv , Rfstat_dset , 1 , MRI_float , iv , 0 ) ;
       }
     }
   }
   if( vstep ) fprintf(stderr,"\n") ;
   INFO_message("GLSQ regression done: CPU=%.2f",COX_cpu_time()) ;

   if( Rbeta_dset != NULL ){
     DSET_write(Rbeta_dset); WROTE_DSET(Rbeta_dset); DSET_delete(Rbeta_dset);
   }
   if( Rvar_dset  != NULL ){
     DSET_write(Rvar_dset); WROTE_DSET(Rvar_dset); DSET_delete(Rvar_dset);
   }
   if( Rfitts_dset != NULL ){
     DSET_write(Rfitts_dset); WROTE_DSET(Rfitts_dset); DSET_delete(Rfitts_dset);
   }
   if( Rfstat_dset != NULL ){
     DSET_write(Rfstat_dset); WROTE_DSET(Rfstat_dset); DSET_delete(Rfstat_dset);
   }

   exit(0) ;
}
