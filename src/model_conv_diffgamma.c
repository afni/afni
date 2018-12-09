/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.


   Based on model_convgamma, but as a difference between 2 such curves.
   For Rasmus Birn.

   R. Reynolds                                             30 Aug 2007
******************************************************************************/

#include "NLfit_model.h"

static int     refnum  = 0 ;     /* # pts in refts */
static int     refnz   = 0 ;     /* # of nonzero pts */
static int     g_debug = 0 ;     /* debug level */
static int     g_diter = -1 ;    /* debug iteration number */
static float * refts   = NULL ;  /* reference time series */
static int   * refin   = NULL ;  /* indexes of nonzero pts */

void gamma_model( float * , int , float ** , float *, int, int ) ;

static int   disp_floats(char * mesg, float * p, int len);
static int   model_help(void);
static float test_n_truncate(float * irf, int len, int debug);

void conv_model( float *  gs      , int     ts_length ,
                 float ** x_array , float * ts_array   );

#define ERREX(str) ( fprintf(stderr,"\n*** %s\a\n",str) , exit(1) )

/*----------------------------------------------------------------------
   Function to set the reference time series, with which the
   model function is convolved to produce the simulated data.
------------------------------------------------------------------------*/

void conv_set_ref( int num , float * ref )
{
   if( num > 0 && ref != NULL ){ /*** if have inputs, make space & copy in ***/
      int ii ;

      /* get rid of old data */

      if(refts != NULL){ free(refts); refts = NULL; free(refin); refin = NULL; }

      refnum = num ;
      refts  = (float *) malloc( sizeof(float) * num ) ;
      refin  = (int *)   malloc( sizeof(int)   * num ) ;
      memcpy( refts , ref , sizeof(float) * num ) ;
      for( ii=0,refnz=0 ; ii < num ; ii++ )        /* build list of nonzero */
         if( refts[ii] != 0 ) refin[refnz++] = ii ;      /* points in refts */
      if( refnz == 0 )
         ERREX("model_conv_diffgamma: All zero reference timeseries!") ;

      if( g_debug ) {
         fprintf(stderr,"+d conv_set_ref: num=%d nonzero=%d\n",num,refnz) ;
         if( g_debug > 1 ) {
            fprintf(stderr,"  TR locked stimuli :");
            for( ii = 0; ii < refnz; ii++ ) fprintf(stderr," %d", refin[ii]);
            fputc('\n',stderr);
         }
      }

      return ;

   } else { /*** if no inputs, read it from AFNI_CONVMODEL_REF 1D file ***/

     char * cp ;
     MRI_IMAGE * flim ;
     float one = 1.0 ;

     cp = my_getenv("AFNI_CONVMODEL_REF") ;  /* get name of reference file */
     if( cp == NULL )
        ERREX("model_conv_diffgamma: need ref file as AFNI_CONVMODEL_REF") ;

     flim = mri_read_1D(cp) ;      /* 16 Nov 1999: replaces mri_read_ascii */
     if( flim == NULL ){
        char buf[256] ;
        sprintf(buf,"model_conv_diffgamma: Can't read timeseries file %s",cp) ;
        ERREX(buf) ;
     }

     if( g_debug )
        fprintf(stderr,"+d conv_set_ref: refts=%s  nx=%d\n",cp,flim->ny) ;

     conv_set_ref( flim->nx , MRI_FLOAT_PTR(flim) ) ;  /* recursion! */
     mri_free(flim) ;
   }
   return ;
}

/*-----------------------------------------------------------------------
  Function to compute the simulated time series.
-------------------------------------------------------------------------*/

void conv_model( float *  gs      , int     ts_length ,
                 float ** x_array , float * ts_array   )
{
   int ii, jj,jbot,jtop , kk , nid_top,nid_bot ;
   int cur_debug = 0;
   float top , val , max0, max1 ;

   static int     iter = -1;     /* iteration number */
   static int     do_scale = 0;  /* scale the curves by 1/height */

   static int     nid = 0 ;      /* number of pts in impulse */
   static float * fid0 = NULL ;  /* impulse response function */
   static float * fid1 = NULL ;  /* impulse response function */

   /*----- check for env vars -----*/

   iter++ ;
   if( iter == 0 ) {
      double dval = AFNI_numenv("AFNI_MODEL_DITER");
      if( dval >= 1.0 ) g_diter = (int)dval;  /* zero is failure */
      dval = AFNI_numenv("AFNI_MODEL_DEBUG");
      if( dval >= 1.0 ) g_debug = (int)dval;
      if(g_debug) fprintf(stderr,"\n+d TR = %f\n", x_array[1][1]-x_array[0][1]);
      do_scale = ! AFNI_noenv("AFNI_CONVDIFFGAM_DO_SCALE");
   }

   /*** make sure there is a reference function to convolve with ***/

   if( refnum <= 0 ) conv_set_ref( 0 , NULL ) ;

   /* to clean up, particularly as a parameter */
   cur_debug =  (iter == g_diter || (iter == 0 && g_debug > 1));
   if( cur_debug ) disp_floats("+d params: ", gs, 8);

   /*** initialize the output ***/

   for( ii=0 ; ii < ts_length ; ii++ ) ts_array[ii] = 0.0 ;

   /*** initialize the impulse response ***/

   if( nid < ts_length ){              /* make some space for it */
      if( fid0 ) free(fid0) ;
      if( fid1 ) free(fid1) ;
      nid = ts_length ;
      fid0 = (float *) malloc( sizeof(float) * nid ) ;
      fid1 = (float *) malloc( sizeof(float) * nid ) ;
   }

   /* compute first and second impulse functions */
   gamma_model(gs,   ts_length, x_array, fid0, do_scale, cur_debug);
   gamma_model(gs+4, ts_length, x_array, fid1, do_scale, cur_debug);

   max0 = test_n_truncate(fid0, ts_length, cur_debug);
   max1 = test_n_truncate(fid1, ts_length, cur_debug);

   /* find first and last nonzero value */
   for( nid_bot=0 ; nid_bot < ts_length ; nid_bot++ )
      if( fid0[nid_bot] != 0.0 || fid1[nid_bot] != 0.0 ) break ;
   for( nid_top=ts_length-1 ; nid_top > nid_bot ; nid_top-- )
      if( fid0[nid_top] != 0.0 || fid1[nid_top] != 0.0 ) break ;

   /*** loop over each nonzero point in the reference ***/

   for( ii=0 ; ii < refnz ; ii++ ){
      kk  = refin[ii] ; if( kk >= ts_length ) break ;
      val = refts[kk] ;

      /*** for each point in the impulse ***/

      jtop = ts_length - kk ; if( jtop > nid_top ) jtop = nid_top+1 ;
      for( jj=nid_bot ; jj < jtop ; jj++ )
         ts_array[kk+jj] += val * ( fid0[jj] - fid1[jj] ) ;
   }

   if( cur_debug ) disp_floats("+d conv    : ", ts_array, ts_length);

   return ;
}

static int disp_floats(char * mesg, float * p, int len)
{
   int c;
   if( mesg ) fputs(mesg, stderr);
   for( c = 0; c < len; c++ ) fprintf(stderr," %f ", p[c]);
   fprintf(stderr,"\n\n");
   return 0;
}


/* Find max.  If 0, set irf[0] to 1.  Set values less than 0.01*max to 0.0. */
static float test_n_truncate(float * irf, int len, int debug)
{
   float max = 0.0, val;
   int   ind, ccount;

   /* find max */
   for( ind=0 ; ind < len ; ind++ ){
      val = fabs(irf[ind]) ; if( val > max ) max = val ;
   }

   if( debug ) fprintf(stderr,"+d  max = %f, ", max);
   /* if( max == 0.0 ) irf[0] = 1.0 ;   don't do this */

   /* truncate small values */
   max *= 0.0001 ;
   ccount = 0;
   for( ind=0 ; ind < len ; ind++ ){
      if( fabs(irf[ind]) < max ){ irf[ind] = 0.0; ccount++; }
   }
   if( debug ) fprintf(stderr,"%d small vals cleared\n", ccount);

   return max;
}

/*-----------------------------------------------------------------------*/

DEFINE_MODEL_PROTOTYPE

MODEL_interface * initialize_model ()
{
  MODEL_interface * mi = NULL;

  /*----- first, see if the user wants help -----*/
  if( AFNI_yesenv("AFNI_MODEL_HELP_CONVDIFFGAM") ||
        AFNI_yesenv("AFNI_MODEL_HELP_ALL") ) model_help();

  /*----- allocate memory space for model interface -----*/

  mi = (MODEL_interface *) XtMalloc (sizeof(MODEL_interface));

  /*----- name of this model -----*/

  strcpy (mi->label, "ConvDiffGam");

  /*----- this is a signal model -----*/

  mi->model_type = MODEL_SIGNAL_TYPE;

  /*----- number of parameters in the model -----*/

  mi->params = 8;

  /*----- parameter labels -----*/

  strcpy (mi->plabel[0], "A0");
  strcpy (mi->plabel[1], "T0");
  strcpy (mi->plabel[2], "E0");
  strcpy (mi->plabel[3], "D0");
  strcpy (mi->plabel[4], "A1");
  strcpy (mi->plabel[5], "T1");
  strcpy (mi->plabel[6], "E1");
  strcpy (mi->plabel[7], "D1");

  /*----- minimum and maximum parameter constraints -----*/

  mi->min_constr[0] =     0.0;    mi->max_constr[0] =    10.0;
  mi->min_constr[1] =     0.0;    mi->max_constr[1] =    10.0;
  mi->min_constr[2] =     0.0;    mi->max_constr[2] =    10.0;
  mi->min_constr[3] =     0.0;    mi->max_constr[3] =    10.0;

  mi->min_constr[4] =     0.0;    mi->max_constr[4] =    10.0;
  mi->min_constr[5] =     0.0;    mi->max_constr[5] =    10.0;
  mi->min_constr[6] =     0.0;    mi->max_constr[6] =    10.0;
  mi->min_constr[7] =     0.0;    mi->max_constr[7] =    10.0;

  /*----- function which implements the model -----*/
  mi->call_func = conv_model;

  return (mi);
}

/*----------------------------------------------------------------------*/
/*
  Routine to calculate the time series which results from using the
  gamma variate drug response signal model with the specified
  model parameters.

  Note: gs[0,1] were reversed from those in model_convgamma.

  Definition of model parameters:

         gs[0] = multiplicative constant (A)
         gs[1] = time delay of response (T)
         gs[2] = rise rate exponent (E)
         gs[3] = decay rate constant (D)

*/

void gamma_model
(
  float * gs,                /* parameters for signal model */
  int ts_length,             /* length of time series data */
  float ** x_array,          /* independent variable matrix */
  float * ts_array,          /* estimated signal model time series */
  int     do_scale,          /* scale curves by height */
  int     debug              /* make some noise */
)

{
  int it;                           /* time index */
  float t;                          /* time */
  double gsi , fac ;

  if( gs[3] <= 0.0 || gs[2] <= 0.0 || gs[0] == 0.0 ){
     if( debug ) fprintf(stderr,"-d clearing ts_array for small gs[0,2,3]\n");
     for( it=0 ; it < ts_length ; it++ ) ts_array[it] = 0.0 ;
     return ;
  }

  /* fac is chosen to make the peak value equal to gs[0] */

  gsi = 1.0 / gs[3] ;
  if( do_scale ) fac = gs[0] * exp( gs[2] * ( 1.0 - log(gs[2]*gs[3]) ) ) ;
  else           fac = gs[0];

  for( it=0;  it < ts_length;  it++){
     t = x_array[it][1] - gs[1] ;
     ts_array[it] = (t <= 0.0) ? 0.0
                               : fac * exp( log(t) * gs[2] - t * gsi ) ;
  }

  if( debug ){
     if(do_scale) fprintf(stderr,"+d scaling from %f by %f\n", gs[0],fac/gs[0]);
     else         fprintf(stderr,"+d no scaling done\n");
  }

  return ;
}

/*----------------------------------------------------------------------*/
static int model_help(void)
{
    printf(
    "----------------------------------------------------------------------\n"
    "ConvDiffGam        - convolution of the difference between 2 gamma\n"
    "\n"
    "  This model is identical to that in model_convgamma.c, except that it\n"
    "  takes the difference between 2 such gamma terms (scaled separately),\n"
    "  and the order of the parameters has changed.\n"
    "\n"
    "  The benefit of scaling is to make A0/A1 into curve heights.\n"
    "\n"
    "  An almost complete 8 parameter difference is allowed, though in most\n"
    "  cases the user will probably restrict some to a zero-length range on\n"
    "  the command line.\n"
    "\n"
    "  The general model, as defined by Rasmus Birn, is:\n"
    "\n"
    "                    E0     -(t-T0)/D0              E1     -(t-T1)/D1\n"
    "    M(t) = A0*(t-T0)    * e            -  A1*(t-T1)    * e\n"
    "\n"
    "  ------------------------------\n"
    "\n"
    "  fit restriction defaults:\n"
    "\n"
    "      A0 : amplitude #0,   in [%.1f, %.1f]\n"
    "      T0 : time offset #0, in [%.1f, %.1f]\n"
    "      E0 : exponent #0,    in [%.1f, %.1f]\n"
    "      D0 : decay rate #0,  in (%.1f, %.1f]\n"
    "\n"
    "      A1 : amplitude #1,   in [%.1f, %.1f]\n"
    "      T1 : time offset #1, in [%.1f, %.1f]\n"
    "      E1 : exponent #1,    in [%.1f, %.1f]\n"
    "      D1 : decay rate #1,  in (%.1f, %.1f]\n"
    "\n"
    "  ------------------------------\n"
    "\n"
    "  The user must provide a stim_file style stimulus event file, via\n"
    "  the AFNI_CONVMODEL_REF environment variable.  This should be a 0/1\n"
    "  binary file, with a 1 at any TR with a stimulus.\n"
    "\n"
    "  To get a feel for this model, consider a reference file like:\n"
    "\n"
    "      1 0 0 0 ...\n"
    "\n"
    "      e.g.  setenv AFNI_CONVMODEL_REF dgam_stims.1D\n"
    "\n"
    "  ------------------------------\n"
    "\n"
    "  Other valid environment variables:\n"
    "\n"
    "      AFNI_MODEL_HELP_CONVDIFFGAM\n"
    "\n"
    "          e.g.  setenv AFNI_MODEL_HELP_CONVDIFFGAM YES\n"
    "\n"
    "          Set this Y/N variable to get this help, by running either\n"
    "          afni or '3dNLfim -signal ConvDiffGam'\n"
    "\n"
    "      AFNI_MODEL_DEBUG\n"
    "\n"
    "          e.g.  setenv AFNI_MODEL_DEBUG 2\n"
    "\n"
    "          Set this numeric variable to print extra information when\n"
    "          applying the model.\n"
    "\n"
    "      AFNI_MODEL_DITER\n"
    "\n"
    "          e.g.  setenv AFNI_MODEL_DITER 2000\n"
    "\n"
    "          Set this numeric variable to output results at\n"
    "          the given iteration number.\n"
    "\n"
    "      AFNI_CONVDIFFGAM_DO_SCALE\n"
    "\n"
    "          e.g.  setenv AFNI_CONVDIFFGAM_DO_SCALE NO\n"
    "\n"
    "          Set this YES/NO variable to NO to prevent automatic scaling.\n"
    "\n"
    "          Without this, a0/a4 should be on the order of 1.0\n"
    "          (if the input is close to a difference of exponentials).\n"
    "\n"
    "  ------------------------------\n"
    "\n"
    "  sample command script:\n"
    "\n"
    "      setenv AFNI_CONVMODEL_REF ketchup_stims.1D\n"
    "\n"
    "      3dNLfim -input ketchup_data.nii      \\\n"
    "              -signal ConvDiffGam          \\\n"
    "              -noise Linear                \\\n"
    "              -sconstr 0 0 10              \\\n"
    "              -sconstr 1 0  0              \\\n"
    "              -sconstr 2 0 10              \\\n"
    "              -sconstr 3 0 10              \\\n"
    "              -sconstr 4 0 10              \\\n"
    "              -sconstr 5 0  0              \\\n"
    "              -sconstr 6 0 10              \\\n"
    "              -sconstr 7 0 10              \\\n"
    "              -nconstr 0 -1000 1000        \\\n"
    "              -nconstr 1    -1    1        \\\n"
    "              -mask brain_mask+orig        \\\n"
    "              -BOTH                        \\\n"
    "              -ignore 0                    \\\n"
    "              -nrand 10000                 \\\n"
    "              -nbest 10                    \\\n"
    "              -jobs 5                      \\\n"
    "              -voxel_count                 \\\n"
    "              -bucket 0 ketchup_buck       \\\n"
    "              -snfit    ketchup_snfit\n"
    "\n"
    "----------------------------------------------------------------------\n",
    0.0, 10.0, 0.0, 10.0, 0.0, 10.0, 0.0, 10.0,
    0.0, 10.0, 0.0, 10.0, 0.0, 10.0, 0.0, 10.0
    );
    return 0 ;
}

