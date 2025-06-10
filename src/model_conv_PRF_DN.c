/*****************************************************************************
   Details for this model are in prf_common_circular.c, #included.

   This is for the "Divisive Normalization" form of the PRF methods.

   R. Reynolds      April, 2025
******************************************************************************/
   
#include "NLfit_model.h"

static char  * g_model_ver = "model_conv_PRF_DN, version 0.1, 12 April, 2025";

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
  if ( AFNI_yesenv("AFNI_MODEL_HELP_CONV_PRF_DN") ||
       AFNI_yesenv("AFNI_MODEL_HELP_ALL") ) model_help();

  /*----- allocate memory space for model interface -----*/

  mi = (MODEL_interface *) RwcMalloc (sizeof(MODEL_interface));

  /*----- name of this model -----*/

  strcpy (mi->label, "Conv_PRF_DN");

  /*----- this is a signal model -----*/

  mi->model_type = MODEL_SIGNAL_TYPE;

  /*----- number of parameters in the model -----*/

  mi->params = 8;

  /*----- parameter labels -----*/

  strcpy (mi->plabel[0], "Amp0");
  strcpy (mi->plabel[1], "X");
  strcpy (mi->plabel[2], "Y");
  strcpy (mi->plabel[3], "Sig0");
  strcpy (mi->plabel[4], "B0");
  strcpy (mi->plabel[5], "Amp1");
  strcpy (mi->plabel[6], "Sig1");
  strcpy (mi->plabel[7], "B1");

  /*----- minimum and maximum parameter constraints -----*/

  /* amplitude, x/y, sigma and baseline ranges - numerator */
  mi->min_constr[0] =   -10.0;    mi->max_constr[0] =    10.0;
  mi->min_constr[1] =    -1.0;    mi->max_constr[1] =     1.0;
  mi->min_constr[2] =    -1.0;    mi->max_constr[2] =     1.0;
  mi->min_constr[3] =     0.0;    mi->max_constr[3] =     1.0;
  mi->min_constr[4] =   -10.0;    mi->max_constr[4] =    10.0;

  /* amplitude, (same x/y,) sigma and baseline ranges - denominator */
  mi->min_constr[5] =   -10.0;    mi->max_constr[5] =    10.0;
  mi->min_constr[6] =     0.0;    mi->max_constr[6] =     1.0;
  mi->min_constr[7] =   -10.0;    mi->max_constr[7] =    10.0;

  /*----- function which implements the model -----*/
  mi->call_func = (void_func *)conv_model;

  return (mi);
}


/*----------------------------------------------------------------------*/
/*
  Routine to calculate the time series to (hopefully) fit the data.

  Definition of model parameters (gs[2] > 0)

         gs[0] = Amp0   = amplitude of numerator Gaussian
         gs[1] = x0     = x-coordinate of gaussian center
         gs[2] = y0     = y-coordinate of gaussian center
         gs[3] = Sig0   = "width" of numerator Gaussian curve
         gs[4] = B0     = activation baseline of numerator
         gs[5] = Amp1   = amp of denom Gaussian
         gs[6] = Sig1   = sig of denom Gaussian
         gs[7] = B1     = act base of denom

  For each TR, integrate g(x,y) over stim aperture dset.

         g(x,y,sigma) = e^-[((x-x0)^2+(y-y0)^2)/(2*sigma^2)]

  Here there will be a Sig0 for the numerator and a separate one for the
  denominator.

  The full model will be:

                Amp0 * (G0.Stim) + B0     B0
         p_DN = ---------------------  -  --
                Amp1 * (G1.Stim) + B1     B1

  where Gx.Stim means g(x,y,sigma) integrated over the stim mask.

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
  static float * ts2 = NULL; /* for second curve */
  float  A0, x, y, Sig0, B0; /* model params - numerator */
  float  A1, Sig1, B1;       /* model params - denominator */
  int    ind, maxind;        /* largest dimension */

  /* assign parameters */
  A0   = gs[0];
  x    = gs[1]; y  = gs[2];
  Sig0 = gs[3]; B0 = gs[4];
  A1   = gs[5];
  Sig1 = gs[6]; B1 = gs[7];

  if( debug ) {
     fprintf(stderr, "-d model_conv_PRF_DN parameters: "
                     "   x  %f, y    %f\n"
                     "   A0 %f, Sig0 %f, B0 %f\n"
                     "   A1 %f, Sig1 %f, B1 %f\n"
                     "   nx %d, ny %d, nz %d\n"
                     "   nvals %d, ts_len %d\n",\
                     x, y,   A0, Sig0, B0,
                             A1, Sig1, B1,
                     DSET_NX(g_saset), DSET_NY(g_saset), DSET_NZ(g_saset),
                     DSET_NVALS(g_saset), ts_length);
     show_malloc_stats("signal model");
  }

  if( ! ISVALID_3DIM_DATASET(g_saset) ) return 0;

  /* possibly restrict the length, via nx if on grid, else nt */
  maxind = ts_length;

  if( debug )
      fprintf( stderr,"-d NT orig=%d, applied=%d\n", ts_length, maxind);

  /* make space for computational array */
  if( ts2 == NULL )
     ts2 = malloc(maxind * sizeof(float));

  /* require: Sig1 >= Sig0 */
  if( Sig0 > Sig1 ) {
     if(debug) fprintf(stderr,"-- params outside valid space, clearing...\n");
     memset(ts_array, 0, maxind*sizeof(float));
     return maxind;
  }

  /* for each: A * (G(x,y,sig).Stim) */
  get_signal_computed(ts_array, maxind, g_saset, x, y, Sig0, A0, debug);
  get_signal_computed(ts2,      maxind, g_saset, x, y, Sig1, A1, debug);

  /* and compute final result : (R0+B0)/(R1+B1) - B0/B1  */
  for(ind=0; ind<maxind; ind++)
     ts_array[ind] = (ts_array[ind]+B0)/(ts2[ind]+B1) - B0/B1;

  /* NOTE: this function computes only the reference time series, which will
   *       then be convolved with the HRF back in conv_model().
   */

  if( debug )
     disp_floats("+d signal model result : ", ts_array, ts_length);

  return maxind;
}


/*----------------------------------------------------------------------*/
static int model_help(void)
{
   printf(
"----------------------------------------------------------------------\n"
"PRF    - population receptive field (in visual cortex)\n"
"\n"
"   This model is from the paper:\n"
"\n"
"      Divisive normalization uniﬁes disparate response signatures\n"
"         throughout the human visual hierarchy\n"
"      Proceedings of the National Academy of Sciences, 2021\n"
"      Marco Aqil, Tomas Knapen, and Serge O. Dumoulin\n"
"\n"
"   Which is based on the earlier:\n"
"\n"
"      Population receptive field estimates in human visual cortex\n"
"      NeuroImage 39 (2008) 647-660\n"
"      Serge O. Dumoulin, Brian A. Wandell\n"
"\n"
"\n"
"   The model is made from parameters A0, x, y, Sig0, B0, A1, Sig1, B1, and\n"
"   from stimulus time series input (visual field masks over time) by:\n"
"\n"
"      1. compute a Gaussian curve centered at x0, y0 of with spread sigma\n"
"             g(x0,y0) = e^-( [(x-x0)^2+(y-y0)^2] / (2*sigma^2) )\n"
"      2. multiply this 2-D image with each 2-D stimulus mask image\n"
"         - call this modelI = G(AmpI,x,y,SigmaI), for fixed x,y\n"
"      3. pDN = (model0-B0)/(model1-B1) + B0/B1\n"
"      4. convolve this with a given HRF\n"
"\n"
"   Currently, x0, y0, and sigma are limited to [-1,1], which the stimulus\n"
"   images are evaluated on.  This use may be altered in the future.\n"
"\n"
"--------------------------------------------------\n"
"To use this model function:\n"
"\n"
"   1. Generate the stimulus time series (currently, images must be square).\n"
"\n"
"      This should be a 2D+time dataset of visual stimuli over time.  They\n"
"      are viewed as binary masks by the model function.\n"
"\n"
"   2. Scale and demean the input EPI time series data.\n"
"\n"
"      Scaling is done to put the amplitude values into a reasonable (i.e.\n"
"      expected) range, such as by scaling it to a fraction of the mean\n"
"      (or maybe 100 times that).\n"
"\n"
"      Setting the mean to zero is done so that no baseline modeling is\n"
"      needed (though it might be good to model drifts in the future).\n"
"      It would be efficient to detrend both the input and the model.\n"
"\n"
"   3. Generate a convolution reference time series, such as one for GAM.\n"
"      This should be on the same TR grid, which is 2 in this example:\n"
"\n"
"         3dDeconvolve -nodata 7 2 -polort -1                 \\\n"
"                      -num_stimts 1 -stim_times 1 '1D:0' GAM \\\n"
"                      -x1D conv.ref.GAM.1D\n"
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
"              -signal Conv_PRF_DN          \\\n"
"              -sconstr 0 -10.0 10.0        \\\n"
"              -sconstr 1  -1.0  1.0        \\\n"
"              -sconstr 2  -1.0  1.0        \\\n"
"              -sconstr 3   0.0  1.0        \\\n"
"              -sconstr 4 -10.0 10.0        \\\n"
"              -sconstr 5 -10.0 10.0        \\\n"
"              -sconstr 6   0.0  1.0        \\\n"
"              -sconstr 7 -10.0 10.0        \\\n"
"              -BOTH                        \\\n"
"              -nrand 10000                 \\\n"
"              -nbest 5                     \\\n"
"              -jobs 4                      \\\n"
"              -bucket 0 buck.PRF           \\\n"
"              -snfit snfit.PRF\n"
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
"      AFNI_MODEL_HELP_CONV_PRF_DN  : Y/N - output this help\n"
"\n"
"         e.g. setenv AFNI_MODEL_HELP_CONV_PRF_DN YES\n"
"\n"
"         When set, the model initialization function will output this help.\n"
"\n"
"         Consider:\n"
"\n"
"            3dNLfim -signal does_not_matter\n"
"\n"
"         or more directly (without setenv):\n"
"\n"
"            3dNLfim -DAFNI_MODEL_HELP_CONV_PRF_DN=Y -signal does_not_matter\n"
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
"   Written for Eli Merriam.\n"
"\n"
"   R. Reynolds                                          12 April, 2025\n"
"----------------------------------------------------------------------\n"
   );

    return 0 ;
}

