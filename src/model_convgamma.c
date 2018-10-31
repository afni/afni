/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/

/*----------------------------------------------------------------------
 * Fit parameters t0, k, r and b in the equation:
 *
 *    f(t) = k * (t-t0)^r * e^(-(t-t0)/b)
 *
 *       t0 : time offset before response begins
 *       k  : amplitude
 *       r  : rise parameter (exponent on polynomial component)
 *       b  : fall parameter (exponent on decay component)
 *
 * The above reference function is then convolved with the time series
 * specified by AFNI_CONVMODEL_REF, which might just be a TR-locked binary
 * time series specifying onset events.  In such a case, the returned time
 * series would be the sum of f(t), starting at each of the "events" in
 * AFNI_CONVMODEL_REF.
 *
 * Convolving with AFNI_CONVMODEL_REF could also handle events of different
 * durations (spanning multiple consecutive onset time points), as well as
 * events with varying (but known) relative magnitudes, akin to ampiltude
 * modulation.
 *----------------------------------------------------------------------*/
   
#include "NLfit_model.h"

static int     refnum = 0 ;     /* # pts in refts */
static int     refnz  = 0 ;     /* # of nonzero pts */
static float * refts  = NULL ;  /* reference time series */
static int   * refin  = NULL ;  /* indexes of nonzero pts */

void gamma_model( float * , int , float ** , float * ) ;

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

      if( refts != NULL ){ free(refts); refts = NULL; free(refin); refin = NULL; }

      refnum = num ;
      refts  = (float *) malloc( sizeof(float) * num ) ;
      refin  = (int *)   malloc( sizeof(int)   * num ) ;
      memcpy( refts , ref , sizeof(float) * num ) ;
      for( ii=0,refnz=0 ; ii < num ; ii++ )              /* build list of nonzero */
         if( refts[ii] != 0 ) refin[refnz++] = ii ;      /* points in refts */
      if( refnz == 0 )
         ERREX("model_convgamma: All zero reference timeseries!") ;

#if 0
fprintf(stderr,"conv_set_ref: num=%d nonzero=%d\n",num,refnz) ;
#endif

      return ;

   } else { /*** if no inputs, do something special ***/

     char * cp ;
     MRI_IMAGE * flim ;
     float one = 1.0 ;

     cp = my_getenv("AFNI_CONVMODEL_REF") ;  /* get name of reference file */
     if( cp == NULL )
        ERREX("model_convgamma: Can't read AFNI_CONVMODEL_REF from environment") ;

     flim = mri_read_1D(cp) ;            /* 16 Nov 1999: replaces mri_read_ascii */
     if( flim == NULL ){
        char buf[256] ;
        sprintf(buf,"model_convgamma: Can't read timeseries file %s",cp) ;
        ERREX(buf) ;
     }

#if 0
fprintf(stderr,"conv_set_ref: refts=%s  nx=%d\n",cp,flim->ny) ;
#endif

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
   float top , val ;

   static int     nid = 0 ;     /* number of pts in impulse */
   static float * fid = NULL ;  /* impulse response function */

   /*** make sure there is a reference function to convolve with ***/

   if( refnum <= 0 ) conv_set_ref( 0 , NULL ) ;

   /*** initialize the output ***/

   for( ii=0 ; ii < ts_length ; ii++ ) ts_array[ii] = 0.0 ;

   /*** initialize the impulse response ***/

   if( nid < ts_length ){              /* make some space for it */
      if( fid != NULL ) free(fid) ;
      nid = ts_length ;
      fid = (float *) malloc( sizeof(float) * nid ) ;
   }

   gamma_model( gs , ts_length , x_array , fid ) ;  /* compute impulse */

   top = 0.0 ;                                      /* find max value */
   for( jj=0 ; jj < ts_length ; jj++ ){
      val = fabs(fid[jj]) ; if( val > top ) top = val ;
   }
   if( top == 0.0 ) fid[0] = 1.0 ;                  /* very unlikely case */
   top *= 0.001 ;
   for( jj=0 ; jj < ts_length ; jj++ ){             /* discard small values */
      if( fabs(fid[jj]) < top ) fid[jj] = 0.0 ;
   }
   for( nid_bot=0 ; nid_bot < ts_length ; nid_bot++ )         /* find first nonzero */
      if( fid[nid_bot] != 0.0 ) break ;
   for( nid_top=ts_length-1 ; nid_top > nid_bot ; nid_top-- ) /* and last nonzero */
      if( fid[nid_top] != 0.0 ) break ;

   /*** loop over each nonzero point in the reference ***/

   for( ii=0 ; ii < refnz ; ii++ ){
      kk  = refin[ii] ; if( kk >= ts_length ) break ;
      val = refts[kk] ;

      /*** for each point in the impulse ***/

      jtop = ts_length - kk ; if( jtop > nid_top ) jtop = nid_top+1 ;
      for( jj=nid_bot ; jj < jtop ; jj++ )
         ts_array[kk+jj] += val * fid[jj] ;
   }

   return ;
}

/*-----------------------------------------------------------------------*/

DEFINE_MODEL_PROTOTYPE

MODEL_interface * initialize_model ()
{
  MODEL_interface * mi = NULL;

  /*----- allocate memory space for model interface -----*/

  mi = (MODEL_interface *) XtMalloc (sizeof(MODEL_interface));

  /*----- name of this model -----*/

  strcpy (mi->label, "ConvGamma");

  /*----- this is a signal model -----*/

  mi->model_type = MODEL_SIGNAL_TYPE;

  /*----- number of parameters in the model -----*/

  mi->params = 4;

  /*----- parameter labels -----*/

  strcpy (mi->plabel[0], "t0");
  strcpy (mi->plabel[1], "amp");
  strcpy (mi->plabel[2], "r");
  strcpy (mi->plabel[3], "b");

  /*----- minimum and maximum parameter constraints -----*/

  mi->min_constr[0] =     0.0;    mi->max_constr[0] =    10.0;
  mi->min_constr[1] =     0.0;    mi->max_constr[1] =   200.0;
  mi->min_constr[2] =     1.0;    mi->max_constr[2] =    19.0;
  mi->min_constr[3] =     0.1;    mi->max_constr[3] =     5.0;

  /*----- function which implements the model -----*/
  mi->call_func = &conv_model;

  return (mi);
}

/*----------------------------------------------------------------------*/
/*
  Routine to calculate the time series which results from using the
  gamma variate drug response signal model with the specified
  model parameters.

  Definition of model parameters:

	 gs[0] = time delay of response (t0)
	 gs[1] = multiplicative constant (k)
	 gs[2] = rise rate exponent (r)
	 gs[3] = decay rate constant (b)

  f(t) = k * t^r * e^(-t/b)

*/

void gamma_model
(
  float * gs,                /* parameters for signal model */
  int ts_length,             /* length of time series data */
  float ** x_array,          /* independent variable matrix */
  float * ts_array           /* estimated signal model time series */
)

{
  int it;                           /* time index */
  float t;                          /* time */
  float gsi , fac ;

  if( gs[3] <= 0.0 || gs[2] <= 0.0 ){
     for( it=0 ; it < ts_length ; it++ ) ts_array[it] = 0.0 ;
     return ;
  }

  /* fac is chosen to make the peak value equal to gs[1] */

  gsi = 1.0 / gs[3] ;
  fac = gs[1] * exp( gs[2] * ( 1.0 - log(gs[2]*gs[3]) ) ) ;
  for( it=0;  it < ts_length;  it++){
     t = x_array[it][1] - gs[0] ;
     ts_array[it] = (t <= 0.0) ? 0.0
                               : fac * exp( log(t) * gs[2] - t * gsi ) ;
  }
  return ;
}
