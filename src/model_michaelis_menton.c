
/*----------------------------------------------------------------------
  This is the Michaelis/Menten model function, for use by 3dNLfim.
  Implemented for Jasmin Salloum and Vijay Ramchandani.

  The differential equations are:

      cp'(t) = rate*0.8/v  - vmax*cp(t)/(km+cp(t))
                           - k12*cp(t) + k21*ct(t)
      ct'(t) = k12 * cp(t) - k21 * ct(t)

  They will be solved using the Modified Euler's method:

      c(t+d) = c(t) + d/2 * (c'(t) + c'(t+d))

  The result is a CP time series.

  Author: Rick Reynolds  19 Jan 2006
 *----------------------------------------------------------------------
*/

#include "NLfit_model.h"
#define  NL_MM_DEF_DT   0.1    /* default dt, in seconds */

void signal_model
(
    float  * params,           /* parameters for signal model */
    int      ts_len,           /* length of time series data */
    float ** x_array,          /* independent variable matrix */
    float  * ts_array          /* estimated signal model time series */
);

/* computation of time series */
int compute_ts( float * rtimes, float * rates, int nrates,
                float * ts_cp, int ts_len,
                float dt, float ** ts_times,
                float v, float vmax, float k12, float k21, float mag, float km);
int get_init_data( float ** rtime, float ** rates, int * len, float * dt );

static int debug = 0;
/*----------------------------------------------------------------------

  Initialize MODEL_interface struct with default values and model name.
*/

DEFINE_MODEL_PROTOTYPE  /* for C++ */

MODEL_interface * initialize_model ()
{
    MODEL_interface * M;

    /* get space for new struct */
    M = (MODEL_interface *)malloc(sizeof(MODEL_interface));

    strcpy(M->label, "michaelis_menton");  /* Michaelis/Menten model    */
    M->model_type = MODEL_SIGNAL_TYPE;     /* signal model (not noise)  */
    M->params = 5;                         /* number of user parameters */

    /* set param labels */
    strcpy(M->plabel[0], "v");
    strcpy(M->plabel[1], "vmax");
    strcpy(M->plabel[2], "k12");
    strcpy(M->plabel[3], "k21");
    strcpy(M->plabel[4], "mag");

    /* min and max constraints will default to the expected mean +/- 20% */
    M->min_constr[0] = 120.000;  M->max_constr[0] = 180.000;  /* mean 150  */
    M->min_constr[1] =   0.880;  M->max_constr[1] =   1.320;  /* mean 1.1  */
    M->min_constr[2] =   0.056;  M->max_constr[2] =   0.084;  /* mean 0.07 */
    M->min_constr[3] =   0.040;  M->max_constr[3] =   0.060;  /* mean 0.05 */
    M->min_constr[4] =   0.080;  M->max_constr[4] =   0.120;  /* mean 0.1  */

    M->call_func = &signal_model; /* set the signal model generator callback */

    return (M);
}


/*----------------------------------------------------------------------
  Compute the time series resulting from the differential equations
  constrained by the 4 parameters : v, vmax, k12, k21.

  On the first call:
        - get the rate time series data via an NLfim aux file
        - check for an aux param, dt, the time step for the diff eq soln.
*/
void signal_model (
    float  * params,           /* parameters for signal model */
    int      ts_len,           /* length of time series data */
    float ** x_array,          /* independent variable matrix */
    float  * ts_array          /* estimated signal model time series */
)
{
    static int     first_call = 1;
    static int     rlen;              /* length of infusion data  */
    static float * rtime, * rates;    /* infusion times and rates */
    static float   dt;                /* computational time delta */
    float          TR, km = 15.0;     /* fixed                    */

    /* first time here, get rate info, along with dt */
    if( first_call )
    {
        if( get_init_data( &rtime, &rates, &rlen, &dt ) != 0 )
            exit(1); /* bad things, man, bad things */
        if( debug )
            fprintf(stderr,"+d init params (v, vmax, k12, k21, mag)\n"
                           "             = (%f, %f, %f, %f, %f)\n",
                    params[0], params[1], params[2], params[3], params[4]);
        if( debug > 2 )
        {
            int c;
            fprintf(stderr,"+d computation times (%d events): \n", ts_len);
            for( c = 0; c < ts_len; c++ )
                fprintf(stderr,"  %.1f", x_array[c][1]);
            fputc('\n', stderr);
        }
        first_call = 0;
    }

    TR = x_array[1][1] - x_array[0][1];  /* assume time delta is constant */
    if( dt > TR )
    {
        fprintf(stderr,"** dt > TR (%f > %f), this is unacceptable\n"
                       "   refusing to go on with life...\n", dt, TR);
        exit(1);
    }

    compute_ts( rtime, rates, rlen, ts_array, ts_len, dt, x_array,
                params[0], params[1], params[2], params[3], params[4], km );
}


/*----------------------------------------------------------------------
 * compute a time series (from time/rate), starting from 0
 *             -- assume a constant rate, until it changes
 *
 * use the modified euler's method: c(t+d) = c(t) + d/2 * (c'(t) + c'(t+d))
 *
 * where    cp'(t) = rate*0.8/v - vmax*cp(t)/(km+cp(t))
 *                              - k12*cp(t) + k21*ct(t)
 * and      ct'(t) = k12 * cp(t) - k21 * ct(t)
 *
 * note that computation assumes that at the first time step after the
 *     current rate time, the subsequent rate is used - i.e. we do not wait
 *     until the time of the next rate to start applying it
*/
int compute_ts( float * rtimes, float * rates, int nrates,
                float * ts_cp, int ts_len, /* result and length */
                float dt, float ** ts_times, /* times are in column 1 */
                float v, float vmax, float k12, float k21, float mag, float km)
{
    /* computation variables */
    double ct = 0.0, cp = 0.0;      /* cur values       */
    double ct0, cp0, ct1, cp1;      /* old values       */
    double dct, dcp;                /* derivatives      */

    double cur_time;                /* current time, in seconds      */
    double rate_time, rate;         /* time and rate from list       */
    double dtm;                     /* dt converted to minutes       */
    double tr_time;                 /* for output time, if tr>0      */
    int    irate, itr;              /* index into TR and rates list  */


    dtm       = dt / 60.0;    /* diff. eq. has time in minutes */
    cur_time  = 0.0;          /* current time, in seconds      */
    tr_time   = 0.0;          /* time at current tr            */
    rate_time = 0.0;          /* finish time for current rate  */
    rate      = 0.0;          /* infusion rate                 */
    irate     = 0;            /* angry index into rate list    */

    for( itr = 0; itr < ts_len; itr++ ) /* loop over result time indices   */
    {
        tr_time = ts_times[itr][1];

        while( cur_time <= tr_time )    /* repeat until next result time   */
        {
            cp0 = cp;                   /* set old values and get new ones */
            ct0 = ct;
            dcp = rate*0.8/v - vmax*cp0/(km+cp0) - k12*cp0 + k21*ct0;
            dct = k12 * cp0 - k21 * ct0;
            cp1 = cp0 + dtm * dcp;    /* approximation for new cp */
            ct1 = ct0 + dtm * dct;    /* approximation for new ct */
            cp  = cp0 + 0.5 * dtm *   /* ave of old and approx new slopes */
                  (dcp + rate*0.8/v - vmax*cp1/(km+cp1) - k12*cp1 + k21*ct1);
            ct  = ct0 + 0.5*dtm*(dct + k12 * cp1 - k21 * ct1);

            cur_time += dt;

            /* if more rates to apply && it is time to apply the next */
            if( irate < nrates && cur_time > rate_time )
            {
                rate = rates[irate];
                rate_time = rtimes[irate] * 60; /* minutes to seconds */
                irate++;
            }
        }

        ts_cp[itr] = cp * mag;
    }

    return 0;
}


/* get and read rate file, and get computational time delta */
int get_init_data( float ** rtime, float ** rates, int * len, float * dt )
{
    MRI_IMAGE * im;
    char      * rate_file;
    char      * dt_text;

    if( !rtime || !rates || !len || !dt )
    {
        fprintf(stderr,"** get_init_data: bad params %p,%p,%p,%p\n",
                rtime, rates, len, dt);
        return 1;
    }

    /* get rate file name, and then try to read it */
    rate_file = my_getenv("AFNI_MM_MODEL_RATE_FILE");
    if( !rate_file )
    {
        fprintf(stderr,"\n** NLfim: need env var AFNI_MM_MODEL_RATE_FILE\n");
        fprintf(stderr,"   (might also want AFNI_MM_MODEL_DT)\n");
        return 1;
    }

    im = mri_read_1D(rate_file);
    if( !im )
    {
        fprintf(stderr,"** failed to open rate file %s\n",rate_file);
        return 1;
    }

    /* set the pointers and let the image dangle...  rates is second row */
    /* (should add cleanup function to MODEL_interface...)               */
    *rtime = MRI_FLOAT_PTR(im);
    *rates = *rtime + im->nx;
    *len   = im->nx;

    /* check to see if the rate times are in seconds */
    dt_text = my_getenv("AFNI_MM_MODEL_RATE_IN_SECS");
    if( dt_text && (*dt_text == 'y' || *dt_text == 'Y') )
    {
        int c;
        fprintf(stderr,"NLfim: rate times are taken in seconds\n");
        /* so convert to minutes */
        for( c = 0; c < *len; c++ ) (*rtime)[c] /= 60.0;
    }

    /* get dt from another env var */
    dt_text = my_getenv("AFNI_MM_MODEL_DT");
    if( dt_text )
        *dt = atof(dt_text);
    else  /* if user did not provide it... */
    {
        fprintf(stderr,"NLfim: MM: using default dt of %.3f s\n", NL_MM_DEF_DT);
        fprintf(stderr,"       (use env var AFNI_MM_MODEL_DT to override)\n");
        *dt = NL_MM_DEF_DT;
    }

    /* get debug level (abuse now unneeded dt_text variable) */
    dt_text = my_getenv("AFNI_MM_MODEL_DEBUG");
    if( dt_text ) debug = atoi( dt_text );
    if( dt_text && debug )
    {
        int c;
        fprintf(stderr,"+d NLfim: debug level set to %d\n", debug);
        fprintf(stderr,"          dt = %f, rate file = %s\n", *dt, rate_file);
        if( debug > 1 )
        {
            fprintf(stderr,"    time        rate\n    --------    --------\n");
            for( c = 0; c < *len; c++ )
                fprintf(stderr, "    %8f    %8f\n", (*rtime)[c], (*rates)[c]);
        }
    }

    return 0;
}

