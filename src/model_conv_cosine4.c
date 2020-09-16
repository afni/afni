/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.

   Based on model_conv_diffgamma.c.

   Written for C Connolly and Felix, requested on the AFNI message board,
   regarding a "hemodynamic model incorporating an initial dip".

   The model is from:

        Fully Bayesian Spatio-Temporal Modeling of FMRI Data
        IEEE Transactions on Medical Imaging,
        Volume 23, Issue 2, February 2004, Pages 213-231
        Woolrich, M.W., Jenkinson, M., Brady, J.M., Smith, S.M.

   The model consists of 4 half-cosines, of duration m1, m2, m3, m4:

        - the first half falling from 0 down to -c1
        - the second half rising from -c1 up to 1
        - the next first half falling from 1 down to -c2
        - the next second half rising from -c2 back to 0

        - plus an overall amplitude A, of course

   This function will process parameters with amplitudes first:

        A, c1, c2, m1, m2, m3, m4

   Note that all parameters returned should be non-negative, except
   possibly A.  For a somewhat standard response to a brief event, one
   might expect parameters near:

        c1 = .25
        c2 = .5
        m1 = 1.0
        m2 = 3.0
        m3 = 4.0
        m4 = 4.0

   But of course, estimating the parameters is the point of this function.

   R. Reynolds                                             6 Sep 2013
******************************************************************************/
   
#include "NLfit_model.h"

static int     refnum  = 0 ;     /* # pts in refts */
static int     refnz   = 0 ;     /* # of nonzero pts */
static int     g_debug = 0 ;     /* debug level */
static int     g_diter = -1 ;    /* debug iteration number */
static float * refts   = NULL ;  /* reference time series */
static int   * refin   = NULL ;  /* indexes of nonzero pts */

int signal_model( float * , int , float ** , float *, int );

static int   disp_floats(char * mesg, float * p, int len);
static int   model_help(void);

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
         if( refts[ii] != 0.0 ) refin[refnz++] = ii ;    /* points in refts */
      if( refnz == 0 )
         ERREX("model_conv_cosine4: All zero reference timeseries!") ;

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

     cp = my_getenv("AFNI_CONVMODEL_REF") ;  /* get name of reference file */
     if( cp == NULL )
        ERREX("model_conv_cosine4: need ref file as AFNI_CONVMODEL_REF") ;

     flim = mri_read_1D(cp) ;      /* 16 Nov 1999: replaces mri_read_ascii */
     if( flim == NULL ){
        char buf[256] ;
        sprintf(buf,"model_conv_cosine4: Can't read timeseries file %s",cp) ;
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
   int ii, jj,jtop , kk;
   int cur_debug = 0, irfdur=0;
   float val;

   static int     iter = -1;     /* iteration number */

   static int     nid = 0 ;      /* number of pts in impulse */
   static float * fid = NULL;    /* impulse response function */

   /*----- check for env vars -----*/

   iter++ ;
   if( iter == 0 ) {
      double dval = AFNI_numenv("AFNI_MODEL_DITER");
      if( dval >= 1.0 ) g_diter = (int)dval;  /* zero is failure */
      dval = AFNI_numenv("AFNI_MODEL_DEBUG");
      if( dval >= 1.0 ) g_debug = (int)dval;
      if(g_debug) fprintf(stderr,"\n+d TR = %f\n", x_array[1][1]-x_array[0][1]);
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
      if( fid ) free(fid) ;
      nid = ts_length ;
      fid = (float *) malloc( sizeof(float) * nid ) ;
   }

   /* compute first and second impulse functions */
   irfdur = signal_model(gs, ts_length, x_array, fid, cur_debug);

   /*** loop over each nonzero point in the reference ***/

   /* TR-locked convolution */
   for( ii=0 ; ii < refnz ; ii++ ){
      kk  = refin[ii] ; if( kk >= ts_length ) break ;
      val = refts[kk] ;

      /* for each point in the impulse, add its val times irf */
      /* (top index offset is min(irfdur, ts_length-kk-1))    */
      jtop = ts_length - kk ; if( jtop > irfdur ) jtop = irfdur ;
      for( jj=0 ; jj < jtop ; jj++ )
         ts_array[kk+jj] += val * fid[jj];
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


/*-----------------------------------------------------------------------*/

DEFINE_MODEL_PROTOTYPE

MODEL_interface * initialize_model ()
{
  MODEL_interface * mi = NULL;

  /*----- first, see if the user wants help -----*/
  if( AFNI_yesenv("AFNI_MODEL_HELP_CONV_COSINE4") ||
        AFNI_yesenv("AFNI_MODEL_HELP_ALL") ) model_help();

  /*----- allocate memory space for model interface -----*/

  mi = (MODEL_interface *) RwcMalloc (sizeof(MODEL_interface));

  /*----- name of this model -----*/

  strcpy (mi->label, "ConvCosine4");

  /*----- this is a signal model -----*/

  mi->model_type = MODEL_SIGNAL_TYPE;

  /*----- number of parameters in the model -----*/

  mi->params = 7;

  /*----- parameter labels -----*/

  strcpy (mi->plabel[0], "A");  /* might get fixed at 1.0 here */
  strcpy (mi->plabel[1], "C1");
  strcpy (mi->plabel[2], "C2");
  strcpy (mi->plabel[3], "M1");
  strcpy (mi->plabel[4], "M2");
  strcpy (mi->plabel[5], "M3");
  strcpy (mi->plabel[6], "M4");

  /*----- minimum and maximum parameter constraints -----*/

  /* first 3 are amplitudes, last 4 are durations */
  /* (first amplitude may be fixed at 1.0 and computed externally) */
  mi->min_constr[0] =  -500.0;    mi->max_constr[0] =   500.0;

  mi->min_constr[1] =     0.0;    mi->max_constr[1] =     1.0;
  mi->min_constr[2] =     0.0;    mi->max_constr[2] =     5.0;

  mi->min_constr[3] =     0.0;    mi->max_constr[3] =     5.0;
  mi->min_constr[4] =     0.0;    mi->max_constr[4] =    20.0;
  mi->min_constr[5] =     0.0;    mi->max_constr[5] =    20.0;
  mi->min_constr[6] =     0.0;    mi->max_constr[6] =    20.0;

  /*----- function which implements the model -----*/
  mi->call_func = conv_model;

  return (mi);
}


/*----------------------------------------------------------------------*
 * for use in signal model
 *----------------------------------------------------------------------*/
#ifdef PI
#undef PI
#endif
#define PI 3.141592653589793238462643

/* F(t) is cosine on one interval:
 *
 *   HEIGHT     : height of single cosine (i.e. cos(t) gets scaled by HEIGHT)
 *   POFF       : PI offset, either 0 or PI, depending on where curve starts
 *   TOFF       : time offset = 0, M1, M1+M2 or M1+M2+M3
 *   DUR        : duration, Mi
 *   NBASE      : negative base value (minimum), i.e. C1 or C2
 *   t          : current time value
 */
#ifdef F
#undef F
#endif
#define F(HEIGHT,POFF,TOFF,DUR,NBASE,t) \
        ((HEIGHT) * cos((POFF) + PI*((t)-(TOFF)) / (DUR)) + (HEIGHT) - (NBASE))

/*----------------------------------------------------------------------*/
/*
  Routine to calculate the time series which results from using the
  gamma variate drug response signal model with the specified
  model parameters.

  Note: gs[0,1] were reversed from those in model_convgamma.

  Definition of model parameters (all >=0, except for gs[0])

         gs[0] = A  = multiplicitive constant
         gs[1] = C1 = magnitude of initial undershoot
         gs[2] = C2 = magnitude of post-undershoot
         gs[3] = M1 = duration of fall from 0 to min pre-undershoot
         gs[4] = M2 = duration of rise from pre-undershoot to peak
         gs[5] = M3 = duration of fall from peak to min post-undershoot
         gs[6] = M4 = duration of rise from post-undershoot back to 0

  Return the non-zero duration of the resulting impulse response function,
  as array length.
*/
int signal_model
(
  float  * gs,          /* parameters for signal model */
  int      ts_length,   /* length of time series data */
  float ** x_array,     /* independent variable matrix */
  float  * ts_array,    /* estimated signal model time series */
  int      debug        /* make some noise */
)
{
  int    it, nt;                        /* time index, num non-zero points */
  double t;                             /* time */
  double A, C1, C2, M1, M2, M3, M4;     /* input parameters */
  double H1, H2, H3, H4;                /* curve heights */
  double T02, T03, T04, T05;            /* time interval dividers */

  /* assign parameters */
  A  = gs[0];
  C1 = gs[1]; C2 = gs[2];
  M1 = gs[3]; M2 = gs[4]; M3 = gs[5]; M4 = gs[6];

  if( debug ) fprintf(stderr,"-d A=%.3f, C1=%.3f, C2=%.3f\n,"
                             "M1=%.3f, M2=%.3f, M3=%.3f, M4=%.3f\n",
                      A, C1, C2, M1, M2, M3, M4);

  /* make a list of t0 values, skipping T01=0.0 */
  T02 = M1;
  T03 = M1 + M2;
  T04 = M1 + M2 + M3;
  T05 = M1 + M2 + M3 + M4;

  /* and heights */
  H1  = C1/2.0;
  H2  = (1.0 + C1)/2.0;
  H3  = (1.0 + C2)/2.0;
  H4  = C2/2.0;

  /* do not assume time array is ordered? */
  nt = 0;       /* largest time point applied */
  for( it=0;  it < ts_length;  it++){
     t = x_array[it][1];

     /* if not in bounds, skip this time */
     if( t < 0 || t > T05 ) { ts_array[it] = 0.0; continue; }

     /* track number of usable indices */
     nt = it + 1;

     /* set values per interval (do not assume time is ordered or regular?) */
     if     ( t <  T02 ) ts_array[it] = F(H1, 0.0, 0.0, M1, C1, t);
     else if( t <  T03 ) ts_array[it] = F(H2,  PI, T02, M2, C1, t);
     else if( t <  T04 ) ts_array[it] = F(H3, 0.0, T03, M3, C2, t);
     else /* t <= T05 */ ts_array[it] = F(H4,  PI, T04, M4, C2, t);
  }

  for( it=0;  it < nt;  it++) ts_array[it] *= A;  /* scale by amplitude */

  if( debug )
     disp_floats("+d signal model  : ", ts_array, ts_length);

  return nt;
}

/*----------------------------------------------------------------------*/
static int model_help(void)
{
   printf(
"----------------------------------------------------------------------\n"
"ConvCosine4    - sum of 4 half-cosine curves\n"
"\n"
"   This model is from the paper:\n"
"\n"
"        Fully Bayesian Spatio-Temporal Modeling of FMRI Data\n"
"        IEEE Transactions on Medical Imaging,\n"
"        Volume 23, Issue 2, February 2004, Pages 213-231\n"
"        Woolrich, M.W., Jenkinson, M., Brady, J.M., Smith, S.M.\n"
"\n"
"   The model consists of 4 half-cosines, of duration m1, m2, m3, m4,\n"
"   respectively, all scaled by an amplitude A:\n"
"\n"
"        - the first half falling from 0 down to -c1\n"
"        - the second half rising from -c1 up to 1\n"
"        - the next first half falling from 1 down to -c2\n"
"        - the next second half rising from -c2 back to 0\n"
"\n"
"     1 |    /\\\n"
"  0___ |   /  \\\n"
"   -c1 | \\/    \\  /     - each of 4 'segments' is a half cosine, either\n"
"       |        \\/ -c2     falling (first half) or rising (second half)\n"
"       +------------\n"
"        ||  |   |  |\n"
"        HI  J   K  L\n"
"\n"
"   time H = 0           (starting time of response)\n"
"        I = m1          (duration of falling segment)\n"
"        J = m1+m2       (plus duration m2 of rising segment)\n"
"        K = m1+m2+m3    (plus duration m3 of falling segment)\n"
"        L = m1+m2+m3+m4 (plus duration m4 of rising segment)\n"
"\n"
"   This function will process parameters with amplitudes first:\n"
"\n"
"        A, c1, c2, m1, m2, m3, m4\n"
"\n"
"   Note that all parameters returned should be positive, except\n"
"   possibly A.  For a somewhat standard response to a brief event,\n"
"   one might expect parameters near or maybe 50%% longer:\n"
"\n"
"        c1 = .25\n"
"        c2 = .5\n"
"        m1 = 1.0\n"
"        m2 = 3.0\n"
"        m3 = 4.0\n"
"        m4 = 4.0\n"
"\n"
"--------------------------------------------------\n"
"To use this model function:\n"
"\n"
"   1. Generate a stimulus event file.  This is like a stimulus timing\n"
"      file but is just a 1D file of TR-locked events, where a 1 denotes\n"
"      a stimulus event.  Note that a 0/1 file is not necessary.  If there\n"
"      is a reason to use a range of stim event magnitudes, they will be\n"
"      convolved into the model, if that is what the user wishes\n"
"\n"
"      This should be a vertical text file of stimulus events or magnitudes.\n"
"      A simple example would be just having an event at the first TR.  The\n"
"      file could be a single line containing '1' in such a case, e.g.\n"
"\n"
"         echo 1 > cos4_stim_file.1D\n"
"\n"
"      If there is interest, using a non-TR-locked stimulus timing file\n"
"      could be added.\n"
"\n"
"   2. Set the environment variable AFNI_CONVMODEL_REF to the name of the\n"
"      stimulus event file from step 1.\n"
"\n"
"        tcsh:  setenv AFNI_CONVMODEL_REF cos4_stim_file.1D\n"
"        bash:  export AFNI_CONVMODEL_REF=cos4_stim_file.1D\n"
"\n"
"   3. Run 3dNLfim as usual, e.g.,\n"
"\n"
"         3dNLfim -input epi_data+tlrc    \\\n"
"                 -noise Quadratic        \\\n"
"                 -signal ConvCosine4     \\\n"
"                 -sconstr 0 0.8 1.2      \\\n"
"                 -sconstr 1 0.2 0.3      \\\n"
"                 -sconstr 2 0.3 0.7      \\\n"
"                 -sconstr 3 0.5 1.5      \\\n"
"                 -sconstr 4 2 4          \\\n"
"                 -sconstr 5 3 5          \\\n"
"                 -sconstr 6 3 5          \\\n"
"                 -BOTH                   \\\n"
"                 -ignore 0               \\\n"
"                 -nrand 10000            \\\n"
"                 -nbest 10               \\\n"
"                 -jobs 4                 \\\n"
"                 -bucket 0 buck.cos4     \\\n"
"                 -snfit snfit.cos4\n"
"\n"
"--------------------------------------------------\n"
"   For C Connolly and Felix, requested on the AFNI message board.\n"
"\n"
"   R. Reynolds                                        10 Sep 2013\n"
"----------------------------------------------------------------------\n"
   );

    return 0 ;
}

