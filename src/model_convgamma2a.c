#include "NLfit_model.h"

/****************************************************************************
     Model function for 3dNLfim:
     gamma variate impulse response, convolved with NREF input time series,
     each with an independent scaling amplitude, but all other parameters
     of the gamma variate fixed.  There are NREF+3 parameters:
       t0    = time delay
       r     = power
       b     = time scale
       amp_j = amplitude of j-th reference, for j=1..NREF

     28 July 1998 -- RWCox
****************************************************************************/

#define NREF 2

static int     refnum[NREF] ;  /* # pts in refts */
static int     refnz[NREF]  ;  /* # of nonzero pts */
static float * refts[NREF]  ;  /* reference time series */
static int   * refin[NREF]  ;  /* indexes of nonzero pts */

void gamma_model( float * , int , float ** , float * ) ;

#define ERREX(str) ( fprintf(stderr,"\n*** %s\a\n",str) , exit(1) )

/*----------------------------------------------------------------------
   Function to set the reference time series, with which the
   model function is convolved to produce the simulated data.
     num         = length of input time series
     ref[jv][ii] = ii-th point in jv-th time series,
                   for ii=0..num-1, jv=0..NREF-1
   If num==0, will read the reference time series from a file.
------------------------------------------------------------------------*/

void conv_set_ref( int num , float ** ref )
{
   int jv ;

   if( num > 0 && ref != NULL ){ /*** if have inputs, make space & copy in ***/
      int ii ;

      for( jv=0 ; jv < NREF ; jv++ ){

         /* get rid of old data? */

         if( refts[jv] != NULL ){
            free(refts[jv]); refts[jv] = NULL;
            free(refin[jv]); refin[jv] = NULL;
         }

         /* copy new data */

         refnum[jv] = num ;
         refts[jv]  = (float *) malloc( sizeof(float) * num ) ;
         refin[jv]  = (int *)   malloc( sizeof(int)   * num ) ;
         memcpy( refts[jv] , ref[jv] , sizeof(float) * num ) ;

         /* build a list of nonzero entries in this column */

         for( ii=0,refnz[jv]=0 ; ii < num ; ii++ )
            if( refts[jv][ii] != 0.0 ) refin[jv][refnz[jv]++] = ii ;

         if( refnz[jv] == 0 )
            ERREX(__FILE__ ": All zero reference timeseries column!") ;
      }
      return ;

   } else { /*** if no inputs, do something special ***/

     char * cp ;
     MRI_IMAGE * im , * flim ;
     int jv , nx ;
     float * ref[NREF] ;

     cp = getenv("AFNI_CONVMODEL_REF") ;  /* get name of reference file */
     if( cp == NULL )
        ERREX(__FILE__ ": Can't read AFNI_CONVMODEL_REF from environment") ;

     im = mri_read_ascii(cp) ;            /* read into memory */
     if( im == NULL ){
        char buf[256] ;
        sprintf(buf,__FILE__ ": Can't read timeseries file %s",cp) ;
        ERREX(buf) ;
     } else {
        fprintf(stderr,__FILE__ ": Read reference file %s\n",cp) ;
     }

     if( im->kind != MRI_float ){         /* convert type? */
        flim = mri_to_float(im) ; mri_free(im) ; im = flim ;
     }

     flim = mri_transpose(im) ; mri_free(im) ;         /* flip over */

     if( flim->ny < NREF )
        ERREX(__FILE__ ": reference file has too few columns!") ;
     else if( flim->nv > NREF )
        fprintf(stderr,__FILE__ " WARNING: reference file has too many columns!\n") ;

     nx = flim->nx ;
     for( jv=0 ; jv < NREF ; jv++ )
        ref[jv] = MRI_FLOAT_PTR(flim) + jv*nx ;

     conv_set_ref( nx , ref ) ;  /* recursion! */
     mri_free(flim) ;
   }
   return ;
}

/*-----------------------------------------------------------------------
  Function to compute the simulated time series:
    gs[0..2]      = gamma variate paramters t0, r, b
    gs[3..NREF+3] = amplitudes for each reference time series
-------------------------------------------------------------------------*/

void conv_model( float *  gs      , int     ts_length ,
                 float ** x_array , float * ts_array   )
{
   int ii, jj,jbot,jtop , kk , nid_top,nid_bot , jv ;
   float top , val , amp ;

   static int     nid = 0 ;     /* number of pts in fid array */
   static float * fid = NULL ;  /* impulse response function */

   /*** make sure there is a reference function to convolve with ***/

   if( refnum[0] <= 0 ) conv_set_ref( 0 , NULL ) ;

   /*** initialize the output ***/

   for( ii=0 ; ii < ts_length ; ii++ ) ts_array[ii] = 0.0 ;

   /*** initialize the impulse response ***/

   if( nid < ts_length ){              /* make some space for it */
      if( fid != NULL ) free(fid) ;
      nid = ts_length ;
      fid = (float *) malloc( sizeof(float) * nid ) ;
   }

   gamma_model( gs , ts_length , x_array , fid ) ;  /* compute impulse */

   /*** discard small values from the fid ***/

#define EPS 0.001  /* definition of small */

#undef FIND_TOP  /* don't need this, since our fid amplitude is set to 1.0 */
#ifdef FIND_TOP
   top = 0.0 ;                                      /* find max value */
   for( jj=0 ; jj < ts_length ; jj++ ){
      val = fabs(fid[jj]) ; if( val > top ) top = val ;
   }
   if( top == 0.0 ) fid[0] = 1.0 ;                  /* very unlikely case */
   top *= EPS ;
#else
   top = EPS ;
#endif
   for( jj=0 ; jj < ts_length ; jj++ ){             /* discard small values */
      if( fabs(fid[jj]) < top ) fid[jj] = 0.0 ;
   }
   for( nid_bot=0 ; nid_bot < ts_length ; nid_bot++ )         /* find first nonzero */
      if( fid[nid_bot] != 0.0 ) break ;
   for( nid_top=ts_length-1 ; nid_top > nid_bot ; nid_top-- ) /* and last nonzero */
      if( fid[nid_top] != 0.0 ) break ;

   /*** loop over each reference ***/

   for( jv=0 ; jv < NREF ; jv++ ){

      amp = gs[jv+3] ; if( amp == 0.0 ) continue ;

      /*** loop over each nonzero point in the reference ***/

      for( ii=0 ; ii < refnz[jv] ; ii++ ){
         kk  = refin[jv][ii] ; if( kk >= ts_length ) break ;
         val = amp * refts[jv][kk] ;

         /*** for each point in the impulse ***/

         jtop = ts_length - kk ; if( jtop > nid_top ) jtop = nid_top+1 ;
         for( jj=nid_bot ; jj < jtop ; jj++ )
            ts_array[kk+jj] += val * fid[jj] ;
      }
   }

   return ;
}

/*-----------------------------------------------------------------------*/

MODEL_interface * initialize_model ()
{
  MODEL_interface * mi = NULL;
  int jv ;
  char buf[32] ;

  /*----- allocate memory space for model interface -----*/

  mi = (MODEL_interface *) XtMalloc (sizeof(MODEL_interface));

  /*----- name of this model -----*/

  sprintf( mi->label , "ConvGamma%da" , NREF ) ;

  /*----- this is a signal model -----*/

  mi->model_type = MODEL_SIGNAL_TYPE;

  /*----- number of parameters in the model -----*/

  mi->params = 3 + NREF ;

  /*----- parameter labels -----*/

  strcpy (mi->plabel[0], "t0");  /* impulse response is proportional to */
  strcpy (mi->plabel[1], "r");   /*       r  -(t-t0)/b  */
  strcpy (mi->plabel[2], "b");   /* (t-t0)  e           */

  for( jv=0 ; jv < NREF ; jv++ )
     sprintf( mi->plabel[jv+3] , "amp_%d" , jv+1 ) ;

  /*----- minimum and maximum parameter constraints -----*/

  mi->min_constr[0] = 0.0;  mi->max_constr[0] = 10.0;  /* delay */
  mi->min_constr[1] = 1.0;  mi->max_constr[1] = 19.0;  /* power */
  mi->min_constr[2] = 0.1;  mi->max_constr[2] =  5.0;  /* time scale */

  for( jv=0 ; jv < NREF ; jv++ ){
     mi->min_constr[jv+3] = 0.0;  mi->max_constr[jv+3] = 200.0;
  }

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
	 gs[1] = rise rate exponent (r)
	 gs[2] = decay rate constant (b)

  Time to peak is r * b ;
  FWHM of the peak is about 2.3 * sqrt(r) * b, for r > 1.

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

  if( gs[2] <= 0.0 || gs[1] <= 0.0 ){
     ts_array[0] = 1.0 ;
     for( it=1 ; it < ts_length ; it++ ) ts_array[it] = 0.0 ;
     return ;
  }

  /* fac is chosen to make the peak value equal to 1 (at t = gs[1]*gs[2]) */

  gsi = 1.0 / gs[2] ;
  fac = exp( gs[1] * ( 1.0 - log(gs[1]*gs[2]) ) ) ;
  for( it=0;  it < ts_length;  it++){
     t = x_array[it][1] - gs[0] ;
     ts_array[it] = (t <= 0.0) ? 0.0
                               : fac * exp( log(t) * gs[1] - t * gsi ) ;
  }
  return ;
}
