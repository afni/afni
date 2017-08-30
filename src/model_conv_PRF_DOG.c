/*****************************************************************************
   Details for this model are in prf_common_circular.c, #incuded.

   This file now just holds functionality that is specific to the
   basic 4-parameter method.

   R. Reynolds      June, 2014
******************************************************************************/
   
#include "NLfit_model.h"

static char  * g_model_ver = "model_conv_PRF_DOG, version 1.0, 10 May, 2017";

/* prototypes */
static int  signal_model( float * , int , float ** , float *, int );
static int  model_help(void);

/* suck in all of the common functions */
#include "prf_common_circular.c"

/*-----------------------------------------------------------------------*/

DEFINE_MODEL_PROTOTYPE

MODEL_interface * initialize_model ()
{
  MODEL_interface * mi = NULL;

  /*----- first, see if the user wants help -----*/
  if ( AFNI_yesenv("AFNI_MODEL_HELP_CONV_PRF_DOG") ||
       AFNI_yesenv("AFNI_MODEL_HELP_ALL") ) model_help();

  /*----- allocate memory space for model interface -----*/

  mi = (MODEL_interface *) XtMalloc (sizeof(MODEL_interface));

  /*----- name of this model -----*/

  strcpy (mi->label, "Conv_PRF_DOG");

  /*----- this is a signal model -----*/

  mi->model_type = MODEL_SIGNAL_TYPE;

  /*----- number of parameters in the model -----*/

  mi->params = 6;

  /*----- parameter labels -----*/

  strcpy (mi->plabel[0], "Amp");
  strcpy (mi->plabel[1], "X");
  strcpy (mi->plabel[2], "Y");
  strcpy (mi->plabel[3], "Sigma");
  strcpy (mi->plabel[4], "Amp2");
  strcpy (mi->plabel[5], "Sigma2");

  /*----- minimum and maximum parameter constraints -----*/

  /* amplitude, x/y ranges, and sigma range */
  mi->min_constr[0] =    -10.0;   mi->max_constr[0] =    10.0;

  mi->min_constr[1] =    -1.0;    mi->max_constr[1] =     1.0;
  mi->min_constr[2] =    -1.0;    mi->max_constr[2] =     1.0;

  mi->min_constr[3] =     0.0;    mi->max_constr[3] =     1.0;

  mi->min_constr[4] =    -10.0;   mi->max_constr[4] =    10.0;
  mi->min_constr[5] =     0.0;    mi->max_constr[5] =     1.0;

  /*----- function which implements the model -----*/
  mi->call_func = conv_model;

  return (mi);
}


/*----------------------------------------------------------------------*/
/*
  Routine to calculate the time series to (hopefully) fit the data.

  Definition of model parameters (gs[2] > 0)

         gs[0] = Amp    = amplitude
         gs[1] = x0     = x-coordinate of gaussian center
         gs[2] = y0     = y-coordinate of gaussian center
         gs[3] = sigma  = "width" of gaussian curve
         gs[4] = amp2   = amplitude of second gaussian
         gs[5] = sigma2 = width of second gaussian

  For each TR, integrate g(x,y) over stim aperture dset.  But as a
  difference of Gaussians.

     given:  g(x,y,sigma) = e^-[((x-x0)^2+(y-y0)^2)/(2*sigma^2)]

     return: G(amp,x,y,sigma,amp2,sigma2) = 

                amp*g(x,y,sigma) = amp2*g(x,y,sigma2)

  The resulting returned time series will be convolved in the 
  parent function.
*/
static int signal_model
(
  float  * gs,          /* parameters for signal model */
  int      ts_length,   /* length of time series data */
  float ** x_array,     /* independent variable matrix */
  float  * ts_array,    /* estimated signal model time series */
  int      debug        /* make some noise */
)
{
  static float * ts2 = NULL;  /* for second curve */
  int    maxind, ind;   /* largest dimension, counter */

  float  A, x, y, sigma;/* model params */
  float  A2, sigma2;

  /* assign parameters */
  A = gs[0];
  x = gs[1]; y = gs[2]; sigma = gs[3];
  A2 = gs[4]; sigma2 = gs[5];

  if( debug ) {
     fprintf(stderr, "-d model_conv_PRF parameters:\n"
                     "   A = %f, x = %f, y = %f, sigma = %f\n"
                     "   A2 = %f, sigma2 = %f\n"
                     "   nz = %d, nvals = %d, ts_len = %d\n",
                     A, x, y, sigma, A2, sigma2,
                     DSET_NZ(g_saset), DSET_NVALS(g_saset), ts_length);
     show_malloc_stats("signal model");
  }

  if( ! ISVALID_3DIM_DATASET(g_saset) ) return 0;

  /* possibly restrict the length, via nx if on grid, else nt */
  /* (was just NX)    30 Aug 2017 */
  maxind = ts_length;
  if( genv_on_grid ) ind = DSET_NX(g_saset);
  else               ind = DSET_NVALS(g_saset);

  if( maxind > ind ) maxind = ind;
  if( maxind == 0 ) return 0;

  if( debug )
      fprintf( stderr,"-d NT orig=%d, applied=%d\n", ts_length, maxind);

  /* make space for computational array */
  if( ts2 == NULL )
     ts2 = malloc(maxind * sizeof(float));

  /* require: A >= A2, same signs, sigma <= sigma2 (else return zeros) */
  if( fabs(A) < fabs(A2) || A*A2 < 0.0 || sigma > sigma2 ) {
     if(debug) fprintf(stderr,"-- params outside valid space, clearing...\n");
     memset(ts_array, 0, maxind*sizeof(float));
     return maxind;
  }

  /* time array must be ordered according to stim dset */
  if( genv_on_grid ) { /* scale data directly from grid */
     get_signal_from_grid(ts_array, maxind, g_saset, x, y, sigma,  A,  debug);
     get_signal_from_grid(ts2,      maxind, g_saset, x, y, sigma2, A2, debug);
  } else {
     get_signal_computed(ts_array, maxind, g_saset, x, y, sigma,  A,  debug);
     get_signal_computed(ts2,      maxind, g_saset, x, y, sigma2, A2, debug);
  }

  /* and subtract the results */
  for(ind=0; ind<maxind; ind++)
     ts_array[ind] -= ts2[ind];

  if( debug )
     disp_floats("+d signal model result : ", ts_array, ts_length);

  return maxind;
}


/*----------------------------------------------------------------------*/
static int model_help(void)
{
   printf(
"----------------------------------------------------------------------\n"
"PRF_DOG    - population receptive field (in visual cortex)\n"
"             (difference of Gaussians)\n"
"\n"
"   This model is from the paper:\n"
"\n"
"      Population receptive field estimates in human visual cortex\n"
"      NeuroImage 39 (2008) 647-660\n"
"      Serge O. Dumoulin, Brian A. Wandell\n"
"\n"
"   The model is made from parameters A, x0, y0, sigma, and from stimulus\n"
"   time series input (visual field masks over time) by:\n"
"\n"
"      1. compute a Gaussian curve centered at x0, y0 of with spread sigma\n"
"             g(x,y) = e^-( [(x-x0)^2+(y-y0)^2] / (2*sigma^2) )\n"
"      2. multiply this 2-D image by each 2-D stimulus mask image\n"
"      3. convolve the result with an ideal HRF\n"
"      4. scale by the amplitude A\n"
"\n"
"   Currently, x0, y0, and sigma are limited to [-1,1], which the stimulus\n"
"   images are evaluated on.  This use may be altered in the future.\n"
"\n"
"\n"
"   Difference of Gaussians:\n"
"\n"
"      G(A1,X,Y,sig,A2,sig2) = A1*g(X,Y,sig) - A2*g(X,Y,sig2)\n"
"\n"
"      where A1 >= A2, sig <= sig2\n"
"\n"
"      So the amplitude of the narrower (inner) Gaussian must be at least\n"
"      as big as that of the wider (outer) Gaussian.\n"
"\n"
"   So the 6 parameters are:\n"
"\n"
"      A1   : amplitude of taller, narrower Gaussian\n"
"      X, Y : fractional grid coord representing focal location (in [-1,1])\n"
"      sig  : sigma - width of taller, narrower Gaussian\n"
"      A2   : amplitude of shorter, wider Gaussian\n"
"      sig2 : sigma - width of shorter, wider Gaussian\n"
"\n"
"--------------------------------------------------\n"
"To use this model function:\n"
"\n"
"   1. Generate the stimulus time series (currently, images must be square).\n"
"\n"
"      This should be a 2D+time dataset of visual stimuli over time.  They\n"
"      are viewed as binary masks by the model function.\n"
"\n"
"    * If results are computed on a restricted grid (which is much faster\n"
"      and is the default (see AFNI_MODEL_PRF_ON_GRID)), the resolution of\n"
"      those X,Y results will come directly from this stimulus dataset.\n"
"      It might be reasonable to have this be 100 or 200 (or 101 or 201)\n"
"      voxels on a side.\n"
"\n"
"    * The amount of memory used for the precomputation should be the size\n"
"      of this dataset (in float format) times AFNI_MODEL_PRF_SIGMA_NSTEPS.\n"
"      It is converted to floats because it is blurred internally.\n"
"      The default AFNI_MODEL_PRF_SIGMA_NSTEPS is 100.\n"
"\n"
"   2. Scale and demean the input EPI time series data.\n"
"\n"
"      Scaling is done to put the amplitude values into a reasonable (i.e.\n"
"      expected) range, such as by scaling it to a fraction of the mean\n"
"      (or maybe twice that).\n"
"\n"
"      Setting the mean to zero is done so that no baseline modeling is\n"
"      needed (though it might be good to model drifts in the future).\n"
"\n"
"   3. Generate a convolution reference time series, such as one for GAM.\n"
"      This should be on the same TR grid, which is 2 in this example.\n"
"\n"
"      3dDeconvolve -nodata 10 2 -polort -1                \\\n"
"                   -num_stimts 1 -stim_times 1 '1D:0' GAM \\\n"
"                   -x1D conv.ref.GAM.1D\n"
"\n"
"   4. Set up environment variables to control execution:\n"
"\n"
"      setenv AFNI_CONVMODEL_REF conv.ref.GAM.1D\n"
"      setenv AFNI_MODEL_PRF_STIM_DSET stim.144.bmask.resam+tlrc\n"
"\n"
"   5. And execute:\n"
"\n"
"      3dNLfim -input epi.scale.demean+tlrc \\\n"
"              -noise Zero                  \\\n"
"              -signal Conv_PRF_DOG         \\\n"
"              -sconstr 0 -10.0 10.0        \\\n"
"              -sconstr 1 -1.0 1.0          \\\n"
"              -sconstr 2 -1.0 1.0          \\\n"
"              -sconstr 3 0.0 1.0           \\\n"
"              -sconstr 4 -10.0 10.0        \\\n"
"              -sconstr 5 0.0 1.0           \\\n"
"              -BOTH                        \\\n"
"              -nrand 10000                 \\\n"
"              -nbest 5                     \\\n"
"              -bucket 0 buck.PRF_DOG       \\\n"
"              -snfit snfit.PRF_DOG\n"
"\n"
"--------------------------------------------------\n"
"environment variables:\n"
"\n"
"   -----------------------------------\n"
"   required:\n"
"\n"
"      AFNI_CONVMODEL_REF          : specify convolution reference file\n"
"\n"
"         e.g. setenv AFNI_CONVMODEL_REF conv.ref.GAM.1D\n"
"\n"
"         The file this specifies should contain a (short?) impulse\n"
"         response function, such as made in step #3, above.\n"
"\n"
"      AFNI_MODEL_PRF_STIM_DSET    : specify visual stimulus dataset\n"
"\n"
"         e.g. setenv AFNI_MODEL_PRF_STIM_DSETstim.144.bmask.resam+tlrc\n"
"\n"
"         This should be a 2D+time dataset of stimulus images over TRs.\n"
"         It will be converted to a byte mask over the visual field.\n"
"\n"
"   -----------------------------------\n"
"   optional (for use with pre-computed grid):\n"
"\n"
"      AFNI_MODEL_PRF_ON_GRID      : Y/N - use pre-computed solutions\n"
"\n"
"         e.g. setenv AFNI_MODEL_PRF_ON_GRID NO\n"
"         e.g. default YES\n"
"\n"
"         Recommended.\n"
"\n"
"         When set, the model function will actually pre-compute all possible\n"
"         (unscaled) fit solutions on the first pass.  Since all of these\n"
"         parameters have a smooth effect on the result, this method should\n"
"         be sufficient.\n"
"\n"
"         Note that the resolution of x0, y0 parameters comes directly from\n"
"         the stimulus dataset (AFNI_MODEL_PRF_STIM_DSET), while the sigma\n"
"         resolution comes from the maximum (AFNI_MODEL_PRF_SIGMA_MAX) and\n"
"         the number of computed values (AFNI_MODEL_PRF_SIGMA_NSTEPS).\n"
"\n"
"         The more voxels to solve for in the input EPI, the more useful this\n"
"         is.  For a single voxel, it is slow.  For a large dataset, it can\n"
"         speed up the solution by a factor of 1000.\n"
"\n"
"      AFNI_MODEL_PRF_SIGMA_MAX    : specify maximum allowable sigma\n"
"\n"
"         e.g. setenv AFNI_MODEL_PRF_SIGMA_MAX 2.0\n"
"         e.g. default 1.0\n"
"\n"
"         Applies directly to AFNI_MODEL_PRF_ON_GRID.\n"
"\n"
"         Use this variable to set the maximum pre-computed sigma.\n"
"         This should probably match the sconstr value for sigma.\n"
"\n"
"      AFNI_MODEL_PRF_SIGMA_NSTEPS : specify number of pre-computed sigmas\n"
"\n"
"         e.g. setenv AFNI_MODEL_PRF_SIGMA_NSTEPS 50\n"
"         e.g. default 100\n"
"\n"
"         Applies directly to AFNI_MODEL_PRF_ON_GRID.\n"
"\n"
"         Use this variable to set the number of pre-computed sigma values.\n"
"         Note that the resolution of pre-computed sigma values will be the\n"
"         ratio: AFNI_MODEL_PRF_SIGMA_MAX/AFNI_MODEL_PRF_SIGMA_NSTEPS.\n"
"\n"
"      AFNI_MODEL_PRF_RAM_STATS VAL : request display of RAM usage\n"
"\n"
"         e.g. setenv AFNI_MODEL_PRF_RAM_STATS Y\n"
"         e.g. default N\n"
"\n"
"         Use this variable to control display of RAM usage.  By default,\n"
"         is it off.  VAL can be one of:\n"
"\n"
"               Y       : yes, show all information\n"
"               N       : no [default], show no information\n"
"               MALLOC  : show only MALLOC information\n"
"               PS      : show only PS information\n"
"               ALL     : same as Y\n"
"               WAIT    : same as Y, and wait after output\n"
"\n"
"   -----------------------------------\n"
"   helpful:\n"
"\n"
"      AFNI_MODEL_HELP_CONV_PRF_DOG : Y/N - output this help\n"
"\n"
"         e.g. setenv AFNI_MODEL_HELP_CONV_PRF_DOG YES\n"
"\n"
"         When set, the model initialization function will output this help.\n"
"\n"
"         Consider:\n"
"\n"
"            3dNLfim -signal Conv_PRF_DOG\n"
"\n"
"         or more directly (without setenv):\n"
"\n"
"            3dNLfim -DAFNI_MODEL_HELP_CONV_PRF_DOG=Y -signal Conv_PRF_DOG\n"
"\n"
"      AFNI_MODEL_DEBUG            : specify debug/verbosity level\n"
"\n"
"         e.g. setenv AFNI_MODEL_DEBUG 2\n"
"\n"
"         Be more verbose.  Valid levels are from 0 to 3, currently.\n"
"\n"
"      AFNI_MODEL_DITER            : specify debug iteration\n"
"\n"
"         e.g. setenv AFNI_MODEL_DITER 999\n"
"\n"
"         Get extra debug info at some iteration.\n"
"\n"
"----------------------------------------------------------------------\n"
"   Written for E Silson and C Baker.\n"
"\n"
"   R. Reynolds                                        10 May, 2017\n"
"----------------------------------------------------------------------\n"
   );

    return 0 ;
}

