#include "mrilib.h"

/***** 3dREMLfit.c *****/

#undef FLOATIZE      /* we will use double precision for matrices */
#include "remla.c"   /* do NOT change this to FLOATIZE !!! */

#undef  INMASK
#define INMASK(i) ( mask == NULL || mask[i] != 0 )

#undef MEMORY_CHECK
#ifdef USING_MCW_MALLOC
# define MEMORY_CHECK                                                  \
   do{ long long nb = mcw_malloc_total() ;                             \
       if( nb > 0 ) ININFO_message("Memory usage now = %lld (%s)" ,    \
                    nb , approximate_number_string((double)nb)     ) ; \
   } while(0)
#else
# define MEMORY_CHECK /*nada*/
#endif


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

/*! To create an empty dataset, with zero-filled sub-bricks. */

THD_3dim_dataset * create_float_dataset( THD_3dim_dataset *tset ,
                                         int nvol , char *prefix, int func )
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
  if( func ) EDIT_TO_FUNC_BUCKET(nset) ;

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
/* struct for putting values from a GLT into a bucket dataset:
     nrow        = number of rows = number of beta and ttst values (each)
     ivbot       = first index in dataset to get a value from this GLT
     ivtop       = last index in dataset to get a value from this GLT
     beta_ind[i] = index in dataset for i-th beta value (i=0..nrow-1)
                   N.B.: beta_ind == NULL if beta values not to be saved
     beta_lab[i] = label for i-th beta value
     ttst_ind[i] = index in dataset for i-th ttst value
                   N.B.: ttst_ind == NULL if ttst values not to be saved
     ttst_lab[i] = label for i-th beta value
     ftst_ind    = index for ftst value (-1 if not to be saved)
     ftst_lab    = label for ftst value
  Note that the 'built-in' GLTs for each stimulus (group of regressors) have
  as their matrices 0-1 callouts of the appropriate regression coefficients.
  So their GLT coefficients returned will just be the regression
  coefficients.
----------------------------------------------------------------------------*/

typedef struct {
  int nrow , ivbot , ivtop ;
  int   *beta_ind ,  *ttst_ind ,  ftst_ind ;
  char **beta_lab , **ttst_lab , *ftst_lab ;
} GLT_index ;

/*-------------------------------------------------------------------------*/
/*! Create a struct for putting values from a GLT into a bucket dataset:
     * ivfirst = index of first sub-brick to get a value
     * nrow    = number of rows in the GLT
     * do_beta = whether to include beta values in the output
     * do_ttst = whether to include t-statistics in the output
     * do_ftst = whether to include F-statistics in the output
     * name    = prefix name for this GLT (for sub-brick labels)
*//*-----------------------------------------------------------------------*/

GLT_index * create_GLT_index( int ivfirst , int nrow ,
                              int do_beta , int do_ttst , int do_ftst ,
                              char *name )
{
   GLT_index *gin ; char lll[256] ; int ii,iv=ivfirst ;

   if( !do_beta && !do_ttst && !do_ftst ) return NULL ; /* bad */
   if( ivfirst < 0 || nrow < 1 || name == NULL ) return NULL ;

   gin = (GLT_index *)calloc(1,sizeof(GLT_index)) ;

   gin->nrow = nrow ;

   if( do_beta ){
     gin->beta_ind = (int *  )calloc( sizeof(int)    , nrow ) ;
     gin->beta_lab = (char **)calloc( sizeof(char *) , nrow ) ;
   }
   if( do_ttst ){
     gin->ttst_ind = (int *  )calloc( sizeof(int)    , nrow ) ;
     gin->ttst_lab = (char **)calloc( sizeof(char *) , nrow ) ;
   }

   /* add Coef and Tstat entries, alternating if both are used */

   if( do_beta || do_ttst ){
     for( ii=0 ; ii < nrow ; ii++ ){
       if( do_beta ){
         gin->beta_ind[ii] = iv++ ;
         sprintf( lll , "%.32s#%d_Coef" , name , ii ) ;
         gin->beta_lab[ii] = strdup(lll) ;
       }
       if( do_ttst ){
         gin->ttst_ind[ii] = iv++ ;
         sprintf( lll , "%.32s#%d_Tstat" , name , ii ) ;
         gin->ttst_lab[ii] = strdup(lll) ;
       }
     }
   }

   if( do_ftst ){
     gin->ftst_ind = iv++ ;
     sprintf( lll , "%.32s_Fstat" , name ) ;
     gin->ftst_lab = strdup(lll) ;
   } else {
     gin->ftst_ind = -1 ;
     gin->ftst_lab = NULL ;
   }

   gin->ivbot = ivfirst ; gin->ivtop = iv-1 ; return gin ;
}

/*==========================================================================*/
/********************************** Main program ****************************/

int main( int argc , char *argv[] )
{
   THD_3dim_dataset *inset=NULL ;
   THD_3dim_dataset *abset=NULL ;
   MRI_IMAGE *aim=NULL, *bim=NULL ; float *aar=NULL, *bar=NULL ;
   byte *mask=NULL ; int mask_nx=0,mask_ny=0,mask_nz=0 , automask=0 ;
   float *iv , *jv ; int niv ;
   int iarg, ii,jj,kk, nreg,ntime,ddof, *tau=NULL, rnum, nfull, nvals,nvox,vv ;
   NI_element *nelmat=NULL ; char *matname=NULL ;
   MTYPE rhomax=0.8 , bmax=0.8 ; int nlevab=3 ;
   char *cgl , *rst ;
   matrix X ; vector y ;
   reml_collection *rrcol ;
   int nprefixO=0 , nprefixR=0 , vstep=0 ;

   char *Rbeta_prefix  = NULL ; THD_3dim_dataset *Rbeta_dset  = NULL ;
   char *Rvar_prefix   = NULL ; THD_3dim_dataset *Rvar_dset   = NULL ;
   char *Rfitts_prefix = NULL ; THD_3dim_dataset *Rfitts_dset = NULL ;
   char *Obeta_prefix  = NULL ; THD_3dim_dataset *Obeta_dset  = NULL ;
   char *Ovar_prefix   = NULL ; THD_3dim_dataset *Ovar_dset   = NULL ;
   char *Ofitts_prefix = NULL ; THD_3dim_dataset *Ofitts_dset = NULL ;

   char *Rbuckt_prefix = NULL ; THD_3dim_dataset *Rbuckt_dset = NULL ;
   char *Obuckt_prefix = NULL ; THD_3dim_dataset *Obuckt_dset = NULL ;
   int nbuckt=0 , do_buckt=0 ;

   char *Rerrts_prefix = NULL ; THD_3dim_dataset *Rerrts_dset = NULL ;
   char *Oerrts_prefix = NULL ; THD_3dim_dataset *Oerrts_dset = NULL ;
   char *Rwherr_prefix = NULL ; THD_3dim_dataset *Rwherr_dset = NULL ;

   int Ngoodlist,*goodlist=NULL , Nruns,*runs=NULL , izero ;
   NI_int_array *giar ; NI_str_array *gsar ; NI_float_array *gfar ;
   float mfilt_radius=0.0 , dx,dy,dz ; int do_mfilt=0 , do_dxyz , nx,ny,nz ;
   int do_Ostuff=0 , do_Rstuff=0 ;

   int glt_num=0, glt_rtot=0 ; matrix **glt_mat=NULL ; char **glt_lab=NULL ;
   sparmat **glt_smat=NULL ;
   GLT_index **glt_ind=NULL ;
   int stim_num=0; int *stim_bot=NULL , *stim_top=NULL ; char **stim_lab=NULL ;
   int do_fstat=0 , do_tstat=0 , do_stat=0 ;
   int num_allstim=0, *allstim=NULL , num_basetim=0, *basetim=NULL ;

   char **beta_lab=NULL ;
   int do_FDR = 1 ;

   MRI_IMARR *imar_addbase=NULL ;
   int        ncol_addbase=0 , nrega=0 , nbad ;

   /**------- Get by with a little help from your friends? -------**/

   Argc = argc ; Argv = argv ;

   if( argc < 2 || strcasecmp(argv[1],"-help") == 0 ){
     printf(
      "Usage: 3dREMLfit [options]\n"
      "\n"
      "Least squares time series fit, with REML estimation of the\n"
      "temporal auto-correlation structure.\n"
      "\n"
      "* This program provides a generalization of 3dDeconvolve:\n"
      "    it allows for serial correlation in the time series noise.\n"
      "* It solves the linear equations for each voxel in the generalized\n"
      "    (prewhitened) least squares sense, using the REML estimation method\n"
      "    to find a best-fit ARMA(1,1) model for the time series noise in\n"
      "    each voxel.\n"
      "* You must run 3dDeconvolve first to generate the input matrix\n"
      "    (.xmat.1D) file, which contains the hemodynamic regression\n"
      "    model, censoring and catenation information, the GLTs, etc.\n"
      "* If you don't want the 3dDeconvolve analysis to run, you can\n"
      "    prevent that by using 3dDeconvolve's '-x1D_stop' option.\n"
      "* 3dDeconvolve also prints out a cognate command line for running\n"
      "    3dREMLfit, which should get you going with relative ease.\n"
      "* The output datasets from 3dREMLfit are structured to resemble\n"
      "    the corresponding results from 3dDeconvolve, to make it\n"
      "    easy to adapt your scripts for further processing.\n"
      "* Is this type of analysis important?\n"
      "    That depends on your point of view, and on your application.\n"
      "    If you really want to know the answer, you should run\n"
      "    your analyses both ways (with 3dDeconvolve and 3dREMLfit),\n"
      "    through to the final step (e.g., group analysis), and then\n"
      "    decide if your neuroscience/brain conclusions depend strongly\n"
      "    on the type of linear regression that was used.\n"
      "\n"
      "-------------------------------------------\n"
      "Input Options (the first two are mandatory)\n"
      "-------------------------------------------\n"
      " -input ddd  = Read time series dataset 'ddd'.\n"
      "\n"
      " -matrix mmm = Read the matrix 'mmm', which should have been\n"
      "                 output from 3dDeconvolve via the '-x1D' option.\n"
      "              * N.B.: 3dREMLfit will NOT work with all zero columns,\n"
      "                      unlike 3dDeconvolve.\n"
      "\n"
      " -mask kkk   = Read dataset 'kkk' as a mask for the input.\n"
      " -automask   = If you don't know what this does by now, I'm not telling.\n"
#if 0
      "\n"
      " -addbase bb = You can add baseline model columns to the matrix with\n"
      "                 this option.  Each column in the .1D file 'bb' will\n"
      "                 be appended to the matrix.  This file must have at\n"
      "                 least as many rows as the matrix does.\n"
      "              * Multiple -addbase options can be used, if needed.\n"
      "              * If the matrix from 3dDeconvolve was censored, then
      "                  this file must be censored to match.  3dREMLfit\n"
      "                  will NOT censor the 'bb' time series for you!\n"
#endif
      "\n"
      "------------------------------------------------------------------------\n"
      "Output Options (at least one must be given; 'ppp' = dataset prefix name)\n"
      "------------------------------------------------------------------------\n"
      " -Rvar  ppp  = dataset for REML variance parameters\n"
      " -Rbeta ppp  = dataset for beta weights from the REML estimation\n"
      "                 (similar to the -cbucket output from 3dDeconvolve)\n"
      " -Rbuck ppp  = dataset for beta + statistics from the REML estimation;\n"
      "                 also contains the results of any GLT analysis requested\n"
      "                 in the 3dDeconvolve setup.\n"
      "                 (similar to the -bucket output from 3dDeconvolve)\n"
      "\n"
      " -fout       = put F-statistics into the bucket dataset\n"
      " -tout       = put t-statistics into the bucket dataset\n"
      "                 (if you use -Rbuck and do not give either -fout or)\n"
      "                 (-tout, then the program assumes -fout is activated)\n"
      " -noFDR      = do NOT add FDR curve data to bucket datasets\n"
      "                 (FDR curves can take a while if -tout is used)\n"
      "\n"
      " -Rfitts ppp = dataset for REML fitted model\n"
      "                 (like 3dDeconvolve, a censored time point gets)\n"
      "                 (the actual data values from that time index!!)\n"
      "\n"
      " -Rerrts ppp = dataset for REML residuals = data - fitted model\n"
      "                 (like 3dDeconvolve,  a censored time)\n"
      "                 (point gets its residual set to zero)\n"
      " -Rwherr ppp = dataset for REML residual, whitened using the\n"
      "                 estimated ARMA(1,1) correlation matrix of the noise\n"
      "                 [Note that the whitening matrix used is the inverse  ]\n"
      "                 [of the Choleski factor of the correlation matrix C; ]\n"
      "                 [however, the whitening matrix isn't uniquely defined]\n"
      "                 [(any matrix W with C=inv(W'W) will work), so other  ]\n"
      "                 [whitening schemes could be used and these would give]\n"
      "                 [different whitened residual time series datasets.   ]\n"
      "\n"
      "The options below let you get the Ordinary Least SQuares outputs\n"
      "(without adjustment for serial correlation), for comparisons.\n"
      "These datasets should be essentially identical to the results\n"
      "you would get by running 3dDeconvolve:\n"
      "\n"
      " -Ovar ppp   = dataset for OLSQ st.dev. parameter (kind of boring)\n"
      " -Obeta ppp  = dataset for beta weights from the OLSQ estimation\n"
      " -Obuck ppp  = dataset for beta + statistics from the OLSQ estimation\n"
      " -Ofitts ppp = dataset for OLSQ fitted model\n"
      " -Oerrts ppp = dataset for OLSQ residuals (data - fitted model)\n"
      "                 (there is no -Owherr option; if you don't)\n"
      "                 (see why, then think about it for a while)\n"
      "\n"
      "-------------------------------------------------------------------\n"
      "The following options control the ARMA(1,1) parameter estimation\n"
      "for each voxel time series; normally, you do not need these options\n"
      "-------------------------------------------------------------------\n"
      " -MAXa am   = Set max allowed AR a parameter to 'am' (default=0.8).\n"
      "                The range of a values scanned is   0 .. +am (-POScor)\n"
      "                                           or is -am .. +am (-NEGcor).\n"
      "\n"
      " -MAXb bm   = Set max allow MA b parameter to 'bm' (default=0.8).\n"
      "                The range of b values scanned is -bm .. +bm.\n"
      "               * The smallest value allowed for am and bm is 0.1.\n"
      "               * For a nearly pure AR(1) model, use '-MAXb 0.1'\n"
      "               * For a nearly pure MA(1) model, use '-MAXa 0.1'\n"
      "\n"
      " -Grid pp   = Set the number of grid divisions in the (a,b) grid\n"
      "                to be 2^pp in each direction over the range 0..MAX.\n"
      "                The default (and minimum) value for 'pp' is 3.\n"
      "                Larger values will provide a finer resolution\n"
      "                in a and b, but at the cost of some CPU time.\n"
      "               * To be clear, the default settings use a grid\n"
      "                   with 8 divisions in the a direction and 16 in\n"
      "                   the b direction (since a is non-negative but\n"
      "                   b can be either sign).\n"
      "               * If -NEGcor is used, then '-Grid 3' means 16 divisions\n"
      "                   in each direction, so that the grid spacing is 0.1\n"
      "                   if MAX=0.8.  Similarly, '-Grid 4' means 32 divisions\n"
      "                   in each direction, '-Grid 5' means 64 divisions, etc.\n"
      "               * I see no reason why you would ever use a -Grid size\n"
      "                   greater than 5 (==> parameter resolution = 0.025).\n"
      "               * In my limited experiments, there was little appreciable\n"
      "                   difference in activation maps between '-Grid 3' and\n"
      "                   '-Grid 5', especially at the group analysis level.\n"
      "               * The program is somewhat slower as the Grid size expands.\n"
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
      "                   the program, which is the scan for each voxel\n"
      "                   to find its optimal (a,b) parameters.\n"
      "               * One possible application of -ABfile:\n"
      "                  + save (a,b) using -Rvar in 3dREMLfit\n"
      "                  + process them in some way (spatial smoothing?)\n"
      "                  + use these modified values for fitting in 3dREMLfit\n"
      "\n"
      "==========================================================================\n"
      "===========  Various Notes (as if this help weren't long enough) =========\n"
      "==========================================================================\n"
      "\n"
      "------------------\n"
      "What is ARMA(1,1)?\n"
      "------------------\n"
      "* The correlation coefficient r(k) of noise samples k units apart in time,\n"
      "    for k >= 1, is given by r(k) = lam * a^(k-1)\n"
      "    where                   lam  = (b+a)(1+a*b)/(1+2*a*b+b*b)\n"
      "    (N.B.: lam=a when b=0 -- AR(1) noise has r(k)=a^k for k >= 0)\n"
      "    (N.B.: lam=b when a=0 -- MA(1) noise has r(k)=b for k=1, r(k)=0 for k>1)\n"
      "* lam can be bigger or smaller than a, depending on the sign of b.\n"
      "* What I call (a,b) here is sometimes called (p,q) in the ARMA literature.\n"
      "* For a noise model which is the sum of AR(1) and white noise, 0 < lam < |a|.\n"
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
      "* The 'Rbuck' dataset has the beta parameters and their statistics\n"
      "    mixed together.\n"
      "\n"
      "-----------------------------------------------------------\n"
      "What is REML = REsidual (or REstricted) Maximum Likelihood?\n"
      "-----------------------------------------------------------\n"
      "* Ordinary Least SQuares (which assumes the noise correlation matrix is\n"
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
      "    re-used in the voxel loop.  The purpose of this grid-based method\n"
      "    is speed -- optimizing iteratively to a highly accurate (a,b)\n"
      "    estimation for each voxel would be very time consuming, and pretty\n"
      "    pointless.  If you are concerned about the sensitivity of the\n"
      "    results to the resolution of the (a,b) grid, you can use the\n"
      "    '-Grid 5' option to increase this resolution and see if your\n"
      "    activation maps change significantly.\n"
      "* REML estimates of the variance/correlation parameters are still\n"
      "    biased, but are generally significantly less biased than ML estimates.\n"
      "    Also, the regression parameters (betas) should be estimated somewhat\n"
      "    more accurately (i.e., with smaller variance than OLSQ).  However,\n"
      "    this effect is generally small in FMRI data, and probably won't affect\n"
      "    your group results noticeably.\n"
      "* After the (a,b) parameters are estimated, then the solution to the\n"
      "    linear system is available via Generalized Least SQuares; that is,\n"
      "    via pre-whitening using the Choleski factor of the estimated\n"
      "    variance/covariance matrix.\n"
      "* In the case with b=0 (that is, AR(1) correlations), and if there are\n"
      "    no time gaps (no censoring, no run breaks), then it is possible to\n"
      "    directly estimate the a parameter without using REML.  This program\n"
      "    does not implement such a special method.  Don't ask why.\n"
      "\n"
      "----------------\n"
      "Other Commentary\n"
      "----------------\n"
      "* ARMA(1,1) parameters 'a' (AR) and 'b' (MA) are estimated\n"
      "    only on a discrete grid, for the sake of CPU time.\n"
      "* Each voxel gets a separate pair of 'a' and 'b' parameters.\n"
      "    There is no option to estimate global values for 'a' and 'b'\n"
      "    and use those for all voxels.  Such an approach might be called\n"
      "    'kindergarten statistics' by some people.\n"
      "* OLSQ = Ordinary Least SQuares; these outputs can be used to compare\n"
      "         the REML/GLSQ estimations with the simpler OLSQ results\n"
      "         (and to test this program vs. 3dDeconvolve).\n"
      "* GLSQ = Generalized Least SQuares = estimated linear system solution\n"
      "         taking into account the variance/covariance matrix of the noise.\n"
      "* The '-matrix' file must be from 3dDeconvolve; besides the regression\n"
      "    matrix itself, the header contains the stimulus labels, the GLTs,\n"
      "    the censoring information, etc.\n"
      "* If you don't actually want the OLSQ results from 3dDeconvolve, you can\n"
      "    make that program stop after the X matrix file is written out by using\n"
      "    the '-x1D_stop' option, and then running 3dREMLfit; something like this:\n"
      "      3dDeconvolve -bucket Fred -input1D '1D: 800@0' -TR_1D 2.5 -x1D_stop ...\n"
      "      3dREMLfit -matrix Fred.xmat.1D -input ...\n"
      "    In the above example, no 3D dataset is input to 3dDeconvolve, so as to\n"
      "    avoid the overhead of having to read it in for no reason.  Instead,\n"
      "    an all-zero time series of the appropriate length (here, 800 points)\n"
      "    and appropriate TR (here, 2.5 seconds) is given to properly establish\n"
      "    the size and timing of the matrix file.\n"
      "* The bucket output datasets are structured to mirror the output\n"
      "    from 3dDeconvolve with the default options below:\n"
      "      -nobout -full_first\n"
      "    Note that you CANNOT use options like '-bout', '-nocout', and\n"
      "    '-nofull_first' with 3dREMLfit -- the bucket datasets are ordered\n"
      "    the way they are and you'll just have to live with it.\n"
      "* All output datasets are in float format.\n"
      "    Internal calculations are done in double precision.\n"
      "* Despite my best efforts, this program is a little sluggish and torpid.\n"
      "    Partly because it solves many linear systems for each voxel,\n"
      "    trying to find the 'best' ARMA(1,1) pre-whitening matrix.\n"
      "    However, a careful choice of algorithms for solving the linear\n"
      "    systems (QR method, sparse matrix operations, etc.) and some\n"
      "    other code optimizations should make running 3dREMLfit tolerable.\n"
      "    Depending on the matrix and the options, you might expect CPU time\n"
      "    to be about 1..3 times that of the corresponding 3dDeconvolve run.\n"
      "\n"
      "-----------------------------------------------------------\n"
      "To Dream the Impossible Dream, to Write the Uncodeable Code\n"
      "-----------------------------------------------------------\n"
      "* Add a -jobs option to use multiple CPUs (or multiple Steves?).\n"
      "* Add options for -iresp/-sresp for -stim_times?\n"
      "* Output variance estimates for the betas, to be carried to the\n"
      "    inter-subject (group) analysis level?\n"
      "* Establish incontrovertibly the nature of quantum mechanical 'observation'!\n"
      "\n"
		"============================\n"
      "== RWCox - July-Sept 2008 ==\n"
      "============================\n" , corcut
     ) ;
     PRINT_COMPILE_DATE ; exit(0) ;
   }

   /*------- official startup ------*/

#ifdef USING_MCW_MALLOC
   enable_mcw_malloc() ;
#endif
   PRINT_VERSION("3dREMLfit"); mainENTRY("3dREMLfit main"); machdep();
   AFNI_logger("3dREMLfit",argc,argv); AUTHOR("RWCox");

   /**------- scan command line --------**/

   iarg = 1 ;
   while( iarg < argc ){

     if( strcasecmp(argv[iarg],"-addbase") == 0 ){
       MRI_IMAGE *im ;
       if( ++iarg >= argc ) ERROR_exit("Need argument after '%s'",argv[iarg-1]) ;
       im = mri_read_1D( argv[iarg] ) ;
       if( im == NULL ) ERROR_exit("Can't read -addbase file '%s'",argv[iarg]) ;
       if( imar_addbase == NULL ) INIT_IMARR(imar_addbase) ;
       mri_add_name( THD_trailname(argv[iarg],0) , im ) ;
       ADDTO_IMARR( imar_addbase , im ) ;
       ncol_addbase += im->ny ;
       iarg++ ; continue ;
     }

     if( strcasecmp(argv[iarg],"-noFDR") == 0 ){
       do_FDR = 0 ; iarg++ ; continue ;
     }
     if( strcasecmp(argv[iarg],"-FDR") == 0 ){
       do_FDR = 1 ; iarg++ ; continue ;
     }

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
            if( rhomax < 0.1 ){ rhomax = 0.1; WARNING_message("-MAXa re-set to 0.1"); }
       else if( rhomax > 0.9 ){ rhomax = 0.9; WARNING_message("-MAXa re-set to 0.9"); }
       iarg++ ; continue ;
     }
     if( strcasecmp(argv[iarg],"-Grid") == 0 ){
       if( ++iarg >= argc ) ERROR_exit("Need argument after '%s'",argv[iarg-1]) ;
       nlevab = (int)strtod(argv[iarg],NULL) ;
            if( nlevab < 3 ){ nlevab = 3; WARNING_message("-Grid reset to 3"); }
       else if( nlevab > 7 ){ nlevab = 7; WARNING_message("-Grid reset to 7"); }
       iarg++ ; continue ;
     }
     if( strcasecmp(argv[iarg],"-MAXb") == 0 ){
       if( ++iarg >= argc ) ERROR_exit("Need argument after '%s'",argv[iarg-1]) ;
       bmax = (MTYPE)strtod(argv[iarg],NULL) ;
            if( bmax < 0.1 ){ bmax = 0.1; WARNING_message("-MAXb re-set to 0.1"); }
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

     if( strcasecmp(argv[iarg],"-fout") == 0 ){
       do_fstat = 1 ; iarg++ ; continue ;
     }
     if( strcasecmp(argv[iarg],"-tout") == 0 ){
       do_tstat = 1 ; iarg++ ; continue ;
     }

     /** prefix options */

#undef  PREFIX_OPTION
#define PREFIX_OPTION(vnam)                                      \
     if( strncasecmp(argv[iarg], "-" #vnam ,5) == 0 ){           \
       if( ++iarg >= argc )                                      \
         ERROR_exit("Need argument after '%s'",argv[iarg-1]) ;   \
       vnam = strdup(argv[iarg]) ;                               \
       if( (#vnam)[0] == 'R' ) nprefixR++ ; else nprefixO++ ;    \
       if( !THD_filename_ok(vnam) )                              \
         ERROR_exit("Illegal string after %s",argv[iarg-1]) ;    \
       iarg++ ; continue ;                                       \
     }

     PREFIX_OPTION(Rbeta_prefix)  ;
     PREFIX_OPTION(Rvar_prefix)   ;
     PREFIX_OPTION(Rbuckt_prefix) ;

     PREFIX_OPTION(Obeta_prefix)  ;
     PREFIX_OPTION(Ovar_prefix)   ;
     PREFIX_OPTION(Obuckt_prefix) ;
     PREFIX_OPTION(Rfitts_prefix) ;
     PREFIX_OPTION(Ofitts_prefix) ;

     PREFIX_OPTION(Rerrts_prefix) ;
     PREFIX_OPTION(Oerrts_prefix) ;
     PREFIX_OPTION(Rwherr_prefix) ;

     ERROR_exit("Unknown option '%s'",argv[iarg]) ;
   }

STATUS("options done") ;

   /**-------- sanity checks, dataset input, maskifying --------**/

   if( inset             == NULL ) ERROR_exit("No -input dataset?!") ;
   if( nelmat            == NULL ) ERROR_exit("No -matrix file?!") ;
   if( nprefixR+nprefixO == 0    ) ERROR_exit("No output datasets at all?!") ;
   if( nprefixR          == 0    ) WARNING_message("No REML output datasets?!") ;

   nvals = DSET_NVALS(inset) ; nvox = DSET_NVOX(inset) ;
   dx = fabsf(DSET_DX(inset)) ; nx = DSET_NX(inset) ;
   dy = fabsf(DSET_DY(inset)) ; ny = DSET_NY(inset) ;
   dz = fabsf(DSET_DZ(inset)) ; nz = DSET_NZ(inset) ;

   do_buckt = (Rbuckt_prefix != NULL) || (Obuckt_prefix != NULL) ;

   if( !do_buckt ){
     if( do_fstat ){
       WARNING_message("-fout disabled because no bucket dataset will be output");
       do_fstat = 0 ;
     }
     if( do_tstat ){
       WARNING_message("-tout disabled because no bucket dataset will be output");
       do_tstat = 0 ;
     }
   }

   if( do_buckt && !do_fstat && !do_tstat ){
     do_fstat = 1 ;
     INFO_message("assuming -fout since you asked for a bucket dataset") ;
   }

   do_stat = (do_fstat || do_tstat) ;

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
       ERROR_exit("-ABfile (a,b) values out of range -0.9..+0.9 ?!") ;

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

   } else {
     INFO_message("No mask ==> computing for all %d voxels",nvox) ;
   }

   /**-------- process the matrix --------**/

STATUS("process matrix") ;

   nreg  = nelmat->vec_num ;  /* number of matrix columns */
   ntime = nelmat->vec_len ;  /* number of matrix rows */
   ddof  = ntime - nreg ;
   if( ddof < 1 ) ERROR_exit("matrix has more columns than rows!") ;

   /* number of rows in the full matrix (without censoring) */

   cgl = NI_get_attribute( nelmat , "NRowFull" ) ;
   if( cgl == NULL ) ERROR_exit("Matrix is missing 'NRowFull' attribute!") ;
   nfull = (int)strtod(cgl,NULL) ;
   if( nvals != nfull )
     ERROR_exit("-input dataset has %d time points, but matrix indicates %d",
                nvals , nfull ) ;

   /* the goodlist = mapping from matrix row index to time index
                     (which allows for possible time point censoring) */

   cgl = NI_get_attribute( nelmat , "GoodList" ) ;
   if( cgl == NULL ) ERROR_exit("Matrix is missing 'GoodList' attribute!") ;
   giar = NI_decode_int_list( cgl , ";," ) ;
   if( giar == NULL || giar->num < ntime )
     ERROR_exit("Matrix 'GoodList' badly formatted?!") ;
   Ngoodlist = giar->num ; goodlist = giar->ar ;
   if( Ngoodlist != ntime )
     ERROR_exit("Matrix 'GoodList' incorrect length?!") ;

   /* run starting points in time indexes */

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
     tau[ii] = jj + 66666*rnum ;  /* the 66666 means 'very far apart' */
   }

   /* re-create the regression matrix X, from the NIML data element */

STATUS("re-create matrix") ;

   matrix_initialize( &X ) ;
   matrix_create( ntime , nreg , &X ) ;
   if( nelmat->vec_typ[0] == NI_FLOAT ){        /* from 3dDeconvolve_f */
     float *cd ;
     for( jj=0 ; jj < nreg ; jj++ ){
       cd = (float *)nelmat->vec[jj] ;
       for( ii=0 ; ii < ntime ; ii++ ) X.elts[ii][jj] = (MTYPE)cd[ii] ;
     }
   } else if( nelmat->vec_typ[0] == NI_DOUBLE ){  /* from 3dDeconvolve */
     double *cd ;
     for( jj=0 ; jj < nreg ; jj++ ){
       cd = (double *)nelmat->vec[jj] ;
       for( ii=0 ; ii < ntime ; ii++ ) X.elts[ii][jj] = (MTYPE)cd[ii] ;
     }
   } else {
     ERROR_exit("Matrix file stored with illegal data type!?") ;
   }

   /* get column labels for the betas */

   cgl = NI_get_attribute( nelmat , "ColumnLabels" ) ;
   if( cgl == NULL ){
     WARNING_message("ColumnLabels attribute in matrix is missing!?") ;
   } else {
     gsar = NI_decode_string_list( cgl , ";" ) ;
     if( gsar == NULL || gsar->num < nreg )
       ERROR_exit("ColumnLabels attribute in matrix is malformed!?") ;
     beta_lab = gsar->str ;
   }

   /**** check -addbase images for column lengths, etc. ****/

   if( imar_addbase != NULL ){
     MRI_IMAGE *im ; int pp ; matrix Xa ; float *iar ;

     nrega = nreg + ncol_addbase ;  /* new number of regressors */

     for( nbad=ii=0 ; ii < IMARR_COUNT(imar_addbase) ; ii++ ){
       im = IMARR_SUBIM( imar_addbase , ii ) ;
       if( im->nx < ntime ){
         ERROR_message("-addbase file '%s' has %d rows, less than matrix's %d",
                       im->name , im->nx , ntime ) ;
         nbad++ ;
       } else if( im->nx > ntime ){
         WARNING_message("-addbase file '%s' has %d rows, more than matrix's %d",
                         im->name , im->nx , ntime ) ;
       }
     }
     if( nbad > 0 ) ERROR_exit("Cannot continue after -addbase errors!") ;

     ddof  = ntime - nrega ;
     if( ddof < 1 )
       ERROR_exit("matrix has more columns than rows after -addbase!") ;

     /* make up extra labels for these columns */

     if( beta_lab != NULL ){
       char lll[32] ;
       beta_lab = (char **)realloc( beta_lab , sizeof(char *)*nrega ) ;
       for( kk=nreg,ii=0 ; ii < IMARR_COUNT(imar_addbase) ; ii++ ){
         im = IMARR_SUBIM( imar_addbase , ii ) ;
         for( jj=0 ; jj < im->ny ; jj++ ){
           if( im->ny > 1 ) sprintf(lll,"%.16s[%d]",im->name,jj) ;
           else             sprintf(lll,"%.16s"    ,im->name   ) ;
           beta_lab[kk++] = strdup(lll) ;
         }
       }
     }

     /* make up a new matrix, add the columns to it, and proceed */

     matrix_initialize( &Xa ) ;
     matrix_enlarge( X , 0 , ncol_addbase , &Xa ) ;
     for( kk=nreg,ii=0 ; ii < IMARR_COUNT(imar_addbase) ; ii++ ){
       im = IMARR_SUBIM(imar_addbase,ii) ; iar = MRI_FLOAT_PTR(im) ;
       for( jj=0 ; jj < im->ny ; jj++,iar+=im->nx ){
         for( pp=0 ; pp < ntime ; pp++ ) Xa.elts[pp][kk] = (MTYPE)iar[pp] ;
       }
     }
     matrix_destroy( &X ) ; X = Xa ; nreg = nrega ;
   }

   /** check matrix for all zero columns **/

   for( nbad=jj=0 ; jj < nreg ; jj++ ){
     for( ii=0 ; ii < ntime && X.elts[ii][jj]==0.0 ; ii++ ) ; /*nada*/
     if( ii==ntime ){
       ERROR_message("matrix column #%d is all zero!?",jj) ; nbad++ ;
     }
   }
   if( nbad > 0 ) ERROR_exit("Cannot continue with all zero columns!") ;

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

   } else {  /* don't have stim info in matrix header?! */

     WARNING_message("-matrix file is missing Stim attributes (old 3dDeconvolve?)") ;
     if( do_stat || do_buckt )
       ERROR_exit(" ==> Can't do statistics on the Stimuli") ;
   }

   /* setup to do statistics on the stimuli betas, if desired */

#define SKIP_SMAT 1
#undef  ADD_GLT
#define ADD_GLT(lb,gg)                                                              \
 do{ glt_lab = (char **   )realloc((void *)glt_lab, sizeof(char *   )*(glt_num+1)); \
     glt_mat = (matrix ** )realloc((void *)glt_mat, sizeof(matrix * )*(glt_num+1)); \
     glt_smat= (sparmat **)realloc((void *)glt_smat,sizeof(sparmat *)*(glt_num+1)); \
     glt_lab[glt_num] = strdup(lb) ; glt_mat[glt_num] = (gg) ;                      \
     glt_smat[glt_num]= (SKIP_SMAT) ? NULL : matrix_to_sparmat(*gg) ;               \
     glt_num++; glt_rtot+= gg->rows ;                                               \
 } while(0)

   if( do_stat ){
     char lnam[32] ; int nn,mm ; matrix *gm ;
     int *set = (int *)malloc(sizeof(int)*nreg) ;  /* list of columns to keep */

STATUS("make stim GLTs") ;
     /* first set of indexes is all stimuli */

     for( kk=jj=0 ; jj < stim_num ; jj++ ){
       for( ii=stim_bot[jj] ; ii <= stim_top[jj] ; ii++ ) set[kk++] = ii ;
     }
     gm = create_subset_matrix( nreg , kk , set ) ;
     if( gm == NULL ) ERROR_exit("Can't create G matrix for FullModel?!") ;
     ADD_GLT( "Full" , gm ) ;

     allstim = (int *)malloc(sizeof(int)*kk) ; num_allstim = kk ;
     memcpy( allstim , set , sizeof(int)*num_allstim ) ;
     qsort_int( num_allstim , allstim ) ;

     /* now do each labeled stimulus separately */

     for( jj=0 ; jj < stim_num ; jj++ ){
       for( kk=0,ii=stim_bot[jj] ; ii <= stim_top[jj] ; ii++ ) set[kk++] = ii ;
       gm = create_subset_matrix( nreg , kk , set ) ;
       if( gm == NULL ) ERROR_exit("Can't create G matrix for %s?!",stim_lab[jj]);
       ADD_GLT( stim_lab[jj] , gm ) ;
     }

     free((void *)set) ;  /* not needed no more, no how, no way */
   }

   /* extract other GLT information from matrix header */

   cgl = NI_get_attribute( nelmat , "Nglt" ) ;
   if( cgl != NULL && do_stat ){
     char lnam[32],*lll ; int nn,mm,ngl ; matrix *gm ; float *far ;

STATUS("make other GLTs") ;

     ngl = (int)strtod(cgl,NULL) ;
     if( ngl <= 0 ) ERROR_exit("Nglt attribute in matrix is not positive!");

     cgl = NI_get_attribute( nelmat , "GltLabels" ) ;
     if( cgl == NULL ) ERROR_exit("Matrix is missing 'GltLabels' attribute!");
     gsar = NI_decode_string_list( cgl , ";" ) ;
     if( gsar == NULL || gsar->num < ngl )
       ERROR_exit("Matrix 'GltLabels' badly formatted?!") ;

     for( kk=0 ; kk < ngl ; kk++ ){
       sprintf(lnam,"GltMatrix_%06d",kk) ;
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
       lll = gsar->str[kk] ; if( lll == NULL || *lll == '\0' ) lll = lnam ;
       ADD_GLT( lll , gm ) ;
     }

#if 0
     INFO_message("Read %d GLT%s from matrix header",ngl,(ngl==1)?"":"s") ;
#endif
   }

   /**------- set up for REML estimation -------**/

   INFO_message("Loading input dataset into memory") ;
   DSET_load(inset) ; CHECK_LOAD_ERROR(inset) ;
   MEMORY_CHECK ;

   INFO_message("starting REML setup calculations") ;
   rrcol = REML_setup_all( &X , tau , nlevab,rhomax,bmax ) ; /* takes a while */
   if( rrcol == NULL ) ERROR_exit("REML setup fails?!" ) ;
   izero = rrcol->izero ;  /* index of (a=0,b=0) case */

   if( glt_num > 0 )
     ININFO_message("adding %d statistics matri%s to REML setup",
                    glt_num , (glt_num==1)?"x":"ces" ) ;
   for( kk=0 ; kk < glt_num ; kk++ )
     REML_add_glt_to_all( rrcol , glt_mat[kk] ) ;

   ININFO_message(
      "REML setup finished: matrix rows=%d cols=%d; %d cases; total CPU=%.2f s",
      ntime,nreg,rrcol->nset,COX_cpu_time()) ;
   MEMORY_CHECK ;

   /***------- loop over voxels, find best REML values ------***/

   vector_initialize( &y ) ; vector_create_noinit( ntime , &y ) ;
   niv = (nvals+nreg+glt_num+9)*2 ;
   iv  = (float *)malloc(sizeof(float)*(niv+1)) ;
   jv  = (float *)malloc(sizeof(float)*(niv+1)) ;

   vstep = (nvox > 999) ? nvox/50 : 0 ;

   if( aim == NULL ){  /*--- if we don't already have (a,b) from -ABfile ---*/

     aim = mri_new_vol( nx,ny,nz , MRI_float ) ;
     aim->dx = dx ; aim->dy = dy ; aim->dz = dz ; aar = MRI_FLOAT_PTR(aim) ;
     bim = mri_new_vol( nx,ny,nz , MRI_float ) ;
     bim->dx = dx ; bim->dy = dy ; bim->dz = dz ; bar = MRI_FLOAT_PTR(bim) ;

     if( vstep ) fprintf(stderr,"++ REML voxel loop: ") ;

     for( vv=0 ; vv < nvox ; vv++ ){    /* this will take a long time */
       if( vstep && vv%vstep==vstep-1 ) vstep_print() ;
       if( !INMASK(vv) ) continue ;
       (void)THD_extract_array( vv , inset , 0 , iv ) ;  /* data vector */
       for( ii=0 ; ii < ntime ; ii++ ) y.elts[ii] = (MTYPE)iv[goodlist[ii]] ;
       (void)REML_find_best_case( &y , rrcol ) ;
       aar[vv] = REML_best_rho ; bar[vv] = REML_best_bb ;
     }
     if( vstep ) fprintf(stderr,"\n") ;

     /*-- median filter (a,b)? --*/

     if( do_mfilt ){
       MRI_IMAGE *afilt , *bfilt ;
       ININFO_message("Median filtering best fit ARMA parameters") ;
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

     ININFO_message("ARMA voxel parameters estimated: total CPU=%.2f s",COX_cpu_time()) ;
     MEMORY_CHECK ;

   } /***** end of REML estimation *****/

   /*-- at this point, aim and bim contain the (a,b) parameters --*/
   /*-- (either from -ABfile or from REML loop just done above) --*/

   /*-- set up indexing and labels needed for bucket dataset creation --*/

   if( do_buckt && glt_num > 0 ){
     glt_ind = (GLT_index **)calloc( sizeof(GLT_index *) , glt_num ) ;
     if( do_fstat ){
       glt_ind[0] = create_GLT_index( 0 , glt_mat[0]->rows ,
                                      0 , 0 , 1 , glt_lab[0] ) ;
       kk = glt_ind[0]->ivtop + 1 ;
     } else {
       glt_ind[0] = NULL ; kk = 0 ;
     }
     for( ii=1 ; ii < glt_num ; ii++ ){
       glt_ind[ii] = create_GLT_index( kk , glt_mat[ii]->rows ,
                                       1 , do_tstat , do_fstat , glt_lab[ii] ) ;
       if( glt_ind[ii] == NULL ) ERROR_exit("Can't create GLT_index[%d]!?",ii) ;
       kk = glt_ind[ii]->ivtop + 1 ;
     }
     nbuckt = glt_ind[glt_num-1]->ivtop + 1 ;  /* number of sub-bricks */
   }

   /*-- now, use these values to compute the Generalized Least  --*/
   /*-- Squares (GLSQ) solution at each voxel, and save results --*/

   Rbeta_dset = create_float_dataset( inset , nreg , Rbeta_prefix,1 ) ;
   if( Rbeta_dset != NULL && beta_lab != NULL ){
     for( ii=0 ; ii < nreg ; ii++ )
       EDIT_BRICK_LABEL( Rbeta_dset , ii , beta_lab[ii] ) ;
   }

   Rvar_dset  = create_float_dataset( inset , 4    , Rvar_prefix,1 ) ;
   if( Rvar_dset != NULL ){
     EDIT_BRICK_LABEL( Rvar_dset , 0 , "a" ) ;
     EDIT_BRICK_LABEL( Rvar_dset , 1 , "b" ) ;
     EDIT_BRICK_LABEL( Rvar_dset , 2 , "lam" ) ;
     EDIT_BRICK_LABEL( Rvar_dset , 3 , "StDev" ) ;
   }

   Rfitts_dset = create_float_dataset( inset , nfull, Rfitts_prefix,0 ) ;
   Rerrts_dset = create_float_dataset( inset , nfull, Rerrts_prefix,0 ) ;
   Rwherr_dset = create_float_dataset( inset , nfull, Rwherr_prefix,0 ) ;

   Rbuckt_dset = create_float_dataset( inset , nbuckt , Rbuckt_prefix,1 ) ;
   if( Rbuckt_dset != NULL ){
     int nr ;
     for( ii=0 ; ii < glt_num ; ii++ ){
       if( glt_ind[ii] == NULL ) continue ;
       nr = glt_ind[ii]->nrow ;
       if( glt_ind[ii]->beta_ind != NULL ){
         for( jj=0 ; jj < nr ; jj++ )
           EDIT_BRICK_LABEL( Rbuckt_dset , glt_ind[ii]->beta_ind[jj] ,
                                           glt_ind[ii]->beta_lab[jj]  ) ;
           EDIT_BRICK_TO_NOSTAT( Rbuckt_dset , glt_ind[ii]->beta_ind[jj] ) ;
       }
       if( glt_ind[ii]->ttst_ind != NULL ){
         for( jj=0 ; jj < nr ; jj++ ){
           EDIT_BRICK_LABEL( Rbuckt_dset , glt_ind[ii]->ttst_ind[jj] ,
                                           glt_ind[ii]->ttst_lab[jj]  ) ;
           EDIT_BRICK_TO_FITT( Rbuckt_dset , glt_ind[ii]->ttst_ind[jj] , ddof ) ;
         }
       }
       if( glt_ind[ii]->ftst_ind >= 0 ){
         EDIT_BRICK_LABEL( Rbuckt_dset , glt_ind[ii]->ftst_ind ,
                                         glt_ind[ii]->ftst_lab  ) ;
         EDIT_BRICK_TO_FIFT( Rbuckt_dset , glt_ind[ii]->ftst_ind , nr , ddof ) ;
       }
     }
/* INFO_message("statcode[0] = %d",DSET_BRICK_STATCODE(Rbuckt_dset,0)) ; */
   }

   do_Rstuff = (Rbeta_dset  != NULL) || (Rvar_dset   != NULL) ||
               (Rfitts_dset != NULL) || (Rbuckt_dset != NULL) ||
               (Rerrts_dset != NULL) || (Rwherr_dset != NULL)   ;

   /*---- and do the second voxel loop ----*/

   if( do_Rstuff ){
     if( vstep ) fprintf(stderr,"++ GLSQ voxel loop: ") ;
     for( vv=0 ; vv < nvox ; vv++ ){
       if( vstep && vv%vstep==vstep-1 ) vstep_print() ;
       if( !INMASK(vv) ) continue ;
       (void)THD_extract_array( vv , inset , 0 , iv ) ;  /* data vector */
       memcpy( jv , iv , sizeof(float)*nfull ) ;
       for( ii=0 ; ii < ntime ; ii++ ) y.elts[ii] = iv[goodlist[ii]] ;

       jj = IAB(rrcol,aar[vv],bar[vv]) ;  /* closest point in the (a,b,) grid */

       if( rrcol->rs[jj] == NULL ){       /* try to fix up this oversight */
         int ia = jj % (1+rrcol->na); MTYPE aaa = rrcol->abot + ia * rrcol->da;
         int ib = jj / (1+rrcol->na); MTYPE bbb = rrcol->bbot + ib * rrcol->db;
         rrcol->rs[jj] = REML_setup_one( &X , tau , aaa,bbb ) ;
         for( kk=0 ; kk < glt_num ; kk++ )
           REML_add_glt_to_one( rrcol->rs[jj] , glt_mat[kk] ) ;
       }
       if( rrcol->rs[jj] == NULL ){ /* should never happen */
         ERROR_message("bad REML! voxel #%d (%d,%d,%d) has a=%.3f b=%.3f lam=%.3f jj=%d",
                         vv, DSET_index_to_ix(inset,vv) ,
                             DSET_index_to_jy(inset,vv) ,
                             DSET_index_to_kz(inset,vv) ,
                         aar[vv],bar[vv],LAMBDA(aar[vv],bar[vv]),jj) ;
       } else {

         /* do the fitting; various results are in the bb? vectors:
              bb5 = estimated betas
              bb6 = fitted model
              bb7 = whitened fitted model [not used below]
              bb1 = whitened data [not used below]
              bb2 = whitened residuals (sum of squares of bb2 ==> noise variance) */

         (void)REML_func( &y , rrcol->rs[jj] , rrcol->X , rrcol->Xs ) ;

         if( Rfitts_dset != NULL ){  /* note that iv still contains original data */
           for( ii=0 ; ii < ntime ; ii++ ) iv[goodlist[ii]] = bb6->elts[ii] ;
           THD_insert_series( vv , Rfitts_dset , nfull , MRI_float , iv , 0 ) ;
         }

         if( Rerrts_dset != NULL ){  /* jv contains copy of original data */
           if( ntime < nfull ) memset( iv , sizeof(float)*nfull , 0 ) ;
           for( ii=0 ; ii < ntime ; ii++ )
             iv[goodlist[ii]] = jv[goodlist[ii]] - bb6->elts[ii] ;
           THD_insert_series( vv , Rerrts_dset , nfull , MRI_float , iv , 0 ) ;
         }

         if( Rwherr_dset != NULL ){  /* note there is no Owherr dataset! */
           if( ntime < nfull ) memset( iv , sizeof(float)*nfull , 0 ) ;
           for( ii=0 ; ii < ntime ; ii++ )
             iv[goodlist[ii]] = bb2->elts[ii] ;
           THD_insert_series( vv , Rwherr_dset , nfull , MRI_float , iv , 0 ) ;
         }

         if( Rbeta_dset != NULL ){
           for( ii=0 ; ii < nreg ; ii++ ) iv[ii] = bb5->elts[ii] ;
           THD_insert_series( vv , Rbeta_dset , nreg , MRI_float , iv , 0 ) ;
         }

         if( Rvar_dset != NULL ){
           iv[0] = rrcol->rs[jj]->rho ; iv[1] = rrcol->rs[jj]->barm ;
           iv[2] = rrcol->rs[jj]->lam ; iv[3] = sqrt( rsumq / ddof ) ;
           THD_insert_series( vv , Rvar_dset , 4 , MRI_float , iv , 0 ) ;
         }

         if( glt_num > 0 && Rbuckt_dset != NULL ){
           MTYPE gv ; GLT_index *gin ; int nr ;
           memset( iv , 0 , sizeof(float)*niv ) ;
           for( kk=0 ; kk < glt_num ; kk++ ){
             gin = glt_ind[kk] ; if( gin == NULL ) continue ; /* skip this'n */
             nr = gin->nrow ;
             gv = REML_compute_gltstat( &y , bb5 , rsumq ,
                                        rrcol->rs[jj], rrcol->rs[jj]->glt[kk],
                                        glt_mat[kk] , glt_smat[kk] ,
                                        rrcol->X , rrcol->Xs        ) ;
             if( gin->ftst_ind >= 0 ) iv[gin->ftst_ind] = gv ;
             if( gin->beta_ind != NULL && betaG->dim >= nr ){
               for( ii=0 ; ii < nr ; ii++ ){
                 iv[gin->beta_ind[ii]] = betaG->elts[ii] ;
               }
             }
             if( gin->ttst_ind != NULL && betaT->dim >= nr ){
               for( ii=0 ; ii < nr ; ii++ ){
                 iv[gin->ttst_ind[ii]] = betaT->elts[ii] ;
               }
             }
           }
           THD_insert_series( vv , Rbuckt_dset , nbuckt , MRI_float , iv , 0 ) ;
         }

       }
     } /* end of voxel loop */
     if( vstep ) fprintf(stderr,"\n") ;
     ININFO_message("GLSQ regression done: total CPU=%.2f s",COX_cpu_time()) ;
     MEMORY_CHECK ;
   }

   /*----- write output REML datasets to disk -----*/

   if( Rbeta_dset != NULL ){
     DSET_write(Rbeta_dset); WROTE_DSET(Rbeta_dset); DSET_delete(Rbeta_dset);
     MEMORY_CHECK ;
   }
   if( Rvar_dset  != NULL ){
     DSET_write(Rvar_dset); WROTE_DSET(Rvar_dset); DSET_delete(Rvar_dset);
     MEMORY_CHECK ;
   }
   if( Rfitts_dset != NULL ){
     DSET_write(Rfitts_dset); WROTE_DSET(Rfitts_dset); DSET_delete(Rfitts_dset);
     MEMORY_CHECK ;
   }
   if( Rerrts_dset != NULL ){
     DSET_write(Rerrts_dset); WROTE_DSET(Rerrts_dset); DSET_delete(Rerrts_dset);
     MEMORY_CHECK ;
   }
   if( Rwherr_dset != NULL ){
     DSET_write(Rwherr_dset); WROTE_DSET(Rwherr_dset); DSET_delete(Rwherr_dset);
     MEMORY_CHECK ;
   }
   if( Rbuckt_dset != NULL ){
/* INFO_message("statcode[0] = %d",DSET_BRICK_STATCODE(Rbuckt_dset,0)) ; */
     if( do_FDR || !AFNI_noenv("AFNI_AUTOMATIC_FDR") )
       ii = THD_create_all_fdrcurves(Rbuckt_dset) ;
     else
       ii = 0 ;
     if( ii > 0 ) ININFO_message("Added %d FDR curve%s to -Rbuck dataset",
                                 ii , (ii==1)?"":"s" ) ;
/* INFO_message("statcode[0] = %d",DSET_BRICK_STATCODE(Rbuckt_dset,0)) ; */
     DSET_write(Rbuckt_dset); WROTE_DSET(Rbuckt_dset); DSET_delete(Rbuckt_dset);
     MEMORY_CHECK ;
   }

   /*-- create OLSQ outputs, if any --*/

   Obeta_dset = create_float_dataset( inset , nreg , Obeta_prefix,1 ) ;
   if( Obeta_dset != NULL && beta_lab != NULL ){
     for( ii=0 ; ii < nreg ; ii++ )
       EDIT_BRICK_LABEL( Obeta_dset , ii , beta_lab[ii] ) ;
   }

   Ovar_dset  = create_float_dataset( inset , 1    , Ovar_prefix,1  ) ;
   if( Ovar_dset != NULL ){
     EDIT_BRICK_LABEL( Ovar_dset , 0 , "StDev" ) ;
   }

   Ofitts_dset = create_float_dataset( inset , nfull, Ofitts_prefix,0 ) ;
   Oerrts_dset = create_float_dataset( inset , nfull, Oerrts_prefix,0 ) ;

   Obuckt_dset = create_float_dataset( inset , nbuckt , Obuckt_prefix,1 ) ;
   if( Obuckt_dset != NULL ){
     int nr ;
     for( ii=0 ; ii < glt_num ; ii++ ){
       if( glt_ind[ii] == NULL ) continue ;
       nr = glt_ind[ii]->nrow ;
       if( glt_ind[ii]->beta_ind != NULL ){
         for( jj=0 ; jj < nr ; jj++ )
           EDIT_BRICK_LABEL( Obuckt_dset , glt_ind[ii]->beta_ind[jj] ,
                                           glt_ind[ii]->beta_lab[jj]  ) ;
           EDIT_BRICK_TO_NOSTAT( Obuckt_dset , glt_ind[ii]->beta_ind[jj] ) ;
       }
       if( glt_ind[ii]->ttst_ind != NULL ){
         for( jj=0 ; jj < nr ; jj++ ){
           EDIT_BRICK_LABEL( Obuckt_dset , glt_ind[ii]->ttst_ind[jj] ,
                                           glt_ind[ii]->ttst_lab[jj]  ) ;
           EDIT_BRICK_TO_FITT( Obuckt_dset , glt_ind[ii]->ttst_ind[jj] , ddof ) ;
         }
       }
       if( glt_ind[ii]->ftst_ind >= 0 ){
         EDIT_BRICK_LABEL( Obuckt_dset , glt_ind[ii]->ftst_ind ,
                                         glt_ind[ii]->ftst_lab  ) ;
         EDIT_BRICK_TO_FIFT( Obuckt_dset , glt_ind[ii]->ftst_ind , nr , ddof ) ;
       }
     }
   }

   do_Ostuff = (Obeta_dset  != NULL) || (Ovar_dset   != NULL) ||
               (Ofitts_dset != NULL) || (Obuckt_dset != NULL) ||
               (Oerrts_dset != NULL)                            ;

   /*---- and do the third voxel loop ----*/

   if( do_Ostuff ){
     if( vstep ) fprintf(stderr,"++ OLSQ voxel loop: ") ;
     for( vv=0 ; vv < nvox ; vv++ ){
       if( vstep && vv%vstep==vstep-1 ) vstep_print() ;
       if( !INMASK(vv) ) continue ;
       (void)THD_extract_array( vv , inset , 0 , iv ) ;  /* data vector */
       memcpy( jv , iv , sizeof(float)*nfull ) ;
       for( ii=0 ; ii < ntime ; ii++ ) y.elts[ii] = (MTYPE)iv[goodlist[ii]] ;

       jj = izero ;                       /* OLSQ (a,b)=(0,0) case */

       if( rrcol->rs[jj] == NULL ){       /* try to fix up this oversight */
         int ia = jj % (1+rrcol->na); MTYPE aaa = rrcol->abot + ia * rrcol->da;
         int ib = jj / (1+rrcol->na); MTYPE bbb = rrcol->bbot + ib * rrcol->db;
         rrcol->rs[jj] = REML_setup_one( &X , tau , aaa,bbb ) ;
         for( kk=0 ; kk < glt_num ; kk++ )
           REML_add_glt_to_one( rrcol->rs[jj] , glt_mat[kk]) ;
       }
       if( rrcol->rs[jj] == NULL ){ /* should never happen */
         ERROR_message("bad OLSQ! voxel #%d (%d,%d,%d) jj=%d",
                         vv, DSET_index_to_ix(inset,vv) ,
                             DSET_index_to_jy(inset,vv) ,
                             DSET_index_to_kz(inset,vv) , jj ) ;
       } else {

         (void)REML_func( &y , rrcol->rs[jj] , rrcol->X , rrcol->Xs ) ;

         if( Ofitts_dset != NULL ){
           for( ii=0 ; ii < ntime ; ii++ ) iv[goodlist[ii]] = bb6->elts[ii] ;
           THD_insert_series( vv , Ofitts_dset , nfull , MRI_float , iv , 0 ) ;
         }

         if( Oerrts_dset != NULL ){  /* jv contains copy of original data */
           if( ntime < nfull ) memset( iv , sizeof(float)*nfull , 0 ) ;
           for( ii=0 ; ii < ntime ; ii++ )
             iv[goodlist[ii]] = jv[goodlist[ii]] - bb6->elts[ii] ;
           THD_insert_series( vv , Oerrts_dset , nfull , MRI_float , iv , 0 ) ;
         }

         if( Obeta_dset != NULL ){
           for( ii=0 ; ii < nreg ; ii++ ) iv[ii] = bb5->elts[ii] ;
           THD_insert_series( vv , Obeta_dset , nreg , MRI_float , iv , 0 ) ;
         }

         if( Ovar_dset != NULL ){
           iv[0] = sqrt( rsumq / ddof ) ;
           THD_insert_series( vv , Ovar_dset , 1 , MRI_float , iv , 0 ) ;
         }

         if( glt_num > 0 && Obuckt_dset != NULL ){
           MTYPE gv ; GLT_index *gin ; int nr ;
           memset( iv , 0 , sizeof(float)*niv ) ;
           for( kk=0 ; kk < glt_num ; kk++ ){
             gin = glt_ind[kk] ; if( gin == NULL ) continue ; /* skip this'n */
             nr = gin->nrow ;
             gv = REML_compute_gltstat( &y , bb5 , rsumq ,
                                        rrcol->rs[jj], rrcol->rs[jj]->glt[kk],
                                        glt_mat[kk] , glt_smat[kk] ,
                                        rrcol->X , rrcol->Xs        ) ;
             if( gin->ftst_ind >= 0 ) iv[gin->ftst_ind] = gv ;
             if( gin->beta_ind != NULL && betaG->dim >= nr ){
               for( ii=0 ; ii < nr ; ii++ ){
                 iv[gin->beta_ind[ii]] = betaG->elts[ii] ;
               }
             }
             if( gin->ttst_ind != NULL && betaT->dim >= nr ){
               for( ii=0 ; ii < nr ; ii++ ){
                 iv[gin->ttst_ind[ii]] = betaT->elts[ii] ;
               }
             }
           }
           THD_insert_series( vv , Obuckt_dset , nbuckt , MRI_float , iv , 0 ) ;
         }

       }
     } /* end of voxel loop */
     if( vstep ) fprintf(stderr,"\n") ;
     ININFO_message("OLSQ regression done: total CPU=%.2f s",COX_cpu_time()) ;
     MEMORY_CHECK ;
   }

   /*----- done with the input dataset -----*/

   DSET_unload(inset) ; free(jv) ; free(iv) ;
   reml_collection_destroy(rrcol) ;

   /*----- write output OLSQ datasets to disk -----*/

   if( Obeta_dset != NULL ){
     DSET_write(Obeta_dset); WROTE_DSET(Obeta_dset); DSET_delete(Obeta_dset);
     MEMORY_CHECK ;
   }
   if( Ovar_dset  != NULL ){
     DSET_write(Ovar_dset); WROTE_DSET(Ovar_dset); DSET_delete(Ovar_dset);
     MEMORY_CHECK ;
   }
   if( Ofitts_dset != NULL ){
     DSET_write(Ofitts_dset); WROTE_DSET(Ofitts_dset); DSET_delete(Ofitts_dset);
     MEMORY_CHECK ;
   }
   if( Oerrts_dset != NULL ){
     DSET_write(Oerrts_dset); WROTE_DSET(Oerrts_dset); DSET_delete(Oerrts_dset);
     MEMORY_CHECK ;
   }
   if( Obuckt_dset != NULL ){
     if( do_FDR || !AFNI_noenv("AFNI_AUTOMATIC_FDR") )
       ii = THD_create_all_fdrcurves(Obuckt_dset) ;
     else
       ii = 0 ;
     if( ii > 0 ) ININFO_message("Added %d FDR curve%s to -Obuck dataset",
                                 ii , (ii==1)?"":"s" ) ;
     DSET_write(Obuckt_dset); WROTE_DSET(Obuckt_dset); DSET_delete(Obuckt_dset);
     MEMORY_CHECK ;
   }

   /*----- Free at last ----*/

   INFO_message("3dREMLfit is all done! total CPU=%.2f s",COX_cpu_time()) ;
   exit(0) ;
}
