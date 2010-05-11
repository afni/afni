
/*----------------------------------------------------------------------
  This is for the 3-parameter DEMRI model from Bob Cox and Steve Fung,
  implemented for John Butman and Steve Fung.

  Nov 09 2007: allow for nfirst == 0

  Author: Rick Reynolds, Daniel Glen  22 Oct 2006
 *----------------------------------------------------------------------
*/

#include <stdlib.h>
#include "NLfit_model.h"
extern int  AFNI_needs_dset_ijk(void) ;

#define M_D3_R1_MIN     0.0     /* these are exclusive limits */
#define M_D3_R1_MAX     1000.0
#define M_D3_R_MIN      0.001
#define M_D3_R_MAX      10000.0
#define M_D3_THETA_MIN  0.0
#define M_D3_THETA_MAX  90.0
#define M_D3_TR_MIN     0.0
#define M_D3_TR_MAX     10000.0
#define M_D3_TF_MIN     0.1
#define M_D3_TF_MAX     130.0
#define M_D3_HCT_MIN    0.01
#define M_D3_HCT_MAX    0.99

#define M_D3_NFIRST     0
#define EPSILON         0.0001
#define Mmmmmm_PIiiiii  3.1415926535897932385


void signal_model 
(
    float  * params,           /* parameters for signal model */
    int      ts_len,           /* length of time series data */
    float ** x_array,          /* independent variable matrix */
    float  * ts_array          /* estimated signal model time series */  
);

typedef struct
{
    float    K, kep, ve, fpv;   /* fit params  (one of kep or ve given)   */
    float    r1, RIB, RIT;      /* given params (via env)                 */
    float    theta, TR, TF;     /* TR & inter-frame TR (TR of input dset) */
    float    hct;               /* hematocrit value (via env)             */
    float    rct;               /* residual Ct value (via env dset)       */

    float    cos0;              /* cos(theta)                             */
    int      nfirst;            /* num TRs used to compute mean Mp,0 */
    int      ijk;               /* voxel index*/
    int      debug;
    int      per_min;           /* are parameter rates per minute? */
    int      counter;           /* count iterations */

    double * comp;              /* computation data, and elist */
    double * elist;             /* (for easy allocation, etc.) */
    float  * mcp;               /* (for easy allocation, etc.) */
} demri_params;

static int g_use_ve = 0;        /* can be modified in initialize model() */

/* extra datasets and data pointers */
static THD_3dim_dataset *dset_R1I = NULL;      /* input 3d+time data set */
static float *R1I_data_ptr = NULL;
static THD_3dim_dataset *dset_resid_ct = NULL; /* input 3d+time data set */
static float *resid_ct_ptr = NULL;

static int alloc_param_arrays(demri_params * P, int len);
static int compute_ts       (demri_params *P, float *ts, int ts_len);
static int c_from_ct_cp     (demri_params * P, int len);
static int convert_mp_to_cp (demri_params * P, int mp_len);
static int ct_from_cp       (demri_params * P, double *, float *, int, int);
static int disp_demri_params(char * mesg, demri_params * p );
static int get_env_params   (demri_params * P);
static int get_Mp_array     (demri_params * P, int * mp_len);
static int get_time_in_seconds(char * str, float * time);
static int model_help       (void);
static int Mx_from_R1       (demri_params * P, float * ts, int len);
static int R1_from_c        (demri_params * P, int len);

static void print_doubles(char * mesg, double * list, int len);
static void print_floats (char * mesg, float  * list, int len);

/*----------------------------------------------------------------------

  Initialize MODEL_interface struct with default values and model name.
*/

DEFINE_MODEL_PROTOTYPE  /* for C++ */

MODEL_interface * initialize_model ()
{
    MODEL_interface * M;

    if(AFNI_yesenv("AFNI_MODEL_HELP_DEMRI_3") ||
         AFNI_yesenv("AFNI_MODEL_HELP_ALL")) model_help();

    if( AFNI_yesenv("AFNI_MODEL_D3_USE_VE") )
    {
        fprintf(stderr,"-d will use Ve for param #2, to set k_ep\n");
        g_use_ve = 1;
    }

    /* get space for new struct */
    M = (MODEL_interface *)malloc(sizeof(MODEL_interface));

    strcpy(M->label, "demri_3");           /* 3-param DEMRI             */
    M->model_type = MODEL_SIGNAL_TYPE;     /* signal model (not noise)  */
    M->params = 3;                         /* number of user parameters */

    /* set param labels */
    strcpy(M->plabel[0], "K_trans");
    if( g_use_ve ) strcpy(M->plabel[1], "Ve");
    else           strcpy(M->plabel[1], "k_ep");
    strcpy(M->plabel[2], "f_pv");

    /* set min/max constraints */
    M->min_constr[0] = 0.0;  M->max_constr[0] = 0.99;
    M->min_constr[1] = 0.0;  M->max_constr[1] = 0.99;
    M->min_constr[2] = 0.0;  M->max_constr[2] = 0.99;
  
    M->call_func = &signal_model; /* set the signal model generator callback */

    return (M);
}


/*----------------------------------------------------------------------
  model the time series, called repeatedly
*/
void signal_model (
    float  * params,            /* parameters for signal model */
    int      ts_len,            /* length of time series data */
    float ** x_array,           /* independent variable matrix */
    float  * ts_array           /* estimated signal model time series */  
)
{
    static demri_params P;
    static int          first_call = 1;
    int                 mp_len;      /* length of mcp list */
    int                 rv, errs = 0;

    if( first_call > 1 ) return;    /* fail, and be quick about it */

    /* first time here, get set params, and process Mp data */
    if( first_call )
    {
        memset(&P, 0, sizeof(P));  /* initialize params */

        if( get_env_params( &P )                ||
            get_Mp_array(&P, &mp_len)           ||
            alloc_param_arrays(&P, mp_len)      ||
            convert_mp_to_cp(&P, mp_len) )
                errs++;  /* bad things man, bad things */

        /* verify TF (inter-frame time) (maybe we don't need TF) */
        if( !errs && P.TF != x_array[1][1] - x_array[0][1] )
            fprintf(stderr, 
              "warning: TF (%f) != x_array time (%f), using dataset TR\n",
              P.TF, x_array[1][1] - x_array[0][1]);

        /* verify time series length */
        if( !errs && mp_len != ts_len )
        {
            fprintf(stderr,
                    "** mp_len (%d) != ts_len (%d), going home in snit.\n",
                    mp_len, ts_len);
            errs++;
        }

        if( P.debug ) disp_demri_params("ready to rock: ", &P);

        if( errs )
        {
            first_call = 2;  /* prevent progression, but don't exit */
            memset(ts_array, 0, ts_len*sizeof(float));
            fprintf(stderr,"** MD3: failing out, and clearing time series\n");
            return;
        }
        else
            first_call = 0;
    }

    /* note passed parameters */
    P.K   = params[0];
    P.fpv = params[2];

    if( P.per_min ) P.K /= 60.0;        /* then convert to per minute */

    if( g_use_ve )
    {
        P.ve = params[1];

        /* it is not clear what to do if P.ve is small */
        if( P.ve < 10e-6 )
        {
            static int warn_count = 0;
            static int next_warn = 1;
            warn_count++;

            if( P.debug > 2 && warn_count == next_warn ) {
                fprintf(stderr, "** warning %d, ve, K = %f, %f --> kep = %f\n",
                        warn_count, P.ve, P.K, P.kep);
                next_warn *= 10;
            }
            memset(ts_array, 0, ts_len*sizeof(float));
            return;
        }
        else
            P.kep = P.K / P.ve;

        if( P.kep > 10.0 )
        {
            static int warn_count = 0;
            static int next_warn = 1;
            warn_count++;
            if( P.debug > 2 && warn_count == next_warn ) {
                fprintf(stderr, "** warning %d, ve, K = %f, %f ===> kep = %f\n",
                        warn_count, P.ve, P.K, P.kep);
                next_warn *= 10;
            }
            memset(ts_array, 0, ts_len*sizeof(float));
            return;
        }
    } else {
        P.kep = params[1];
        if( P.per_min ) P.kep /= 60.0;
    }

    if( R1I_data_ptr )
    {
        static int bad_count = 0;       /* print warnings of powers of 10 */
        static int obnox_level = 1;

        P.ijk = AFNI_needs_dset_ijk();
        P.RIT = R1I_data_ptr[P.ijk];  /*  get R1I voxelwise from dataset */
        if( P.RIT < 0.02 || P.RIT > 20 )
        {
            bad_count++;
            if( P.debug > 1 && bad_count == obnox_level ){
                fprintf(stderr,"** warning, bad RIT value %f (# %d)\n",
                        P.RIT, bad_count);
                obnox_level *= 10;
            }
            memset(ts_array, 0, ts_len*sizeof(float));
            return;
            /* do something here?  panic into error?? */
        }
        if(P.debug > 3) printf("Voxel index %d, R1I value %f\n", P.ijk, P.RIT); 
    }

    /* get any residual Ct value */
    if( resid_ct_ptr )
    {
        P.ijk = AFNI_needs_dset_ijk();
        P.rct = resid_ct_ptr[P.ijk];  /*  get residual Ct from dataset */
        if(P.debug > 3) printf("Voxel index %d, rCt value %f\n", P.ijk, P.rct); 
    }

    if(P.debug>1 && P.counter==0) disp_demri_params("before compute_ts: ", &P);
    (void)compute_ts( &P, ts_array, ts_len );
    P.counter++;  /* a valid iteration */

    if( (rv = thd_floatscan(ts_len, ts_array)) > 0 )
    {
        static int nbad = 0;
        static int nprint = 1;
        nbad++;

        if( (nbad % nprint) == 0 )
        {
            char mesg[128];
            sprintf(mesg, "\n** MD3: %d bad results (occurance %d):", rv, nbad);
            disp_demri_params(mesg, &P);
            if( nprint < 10e+6 ) nprint <<= 1;  /* slower and slower */
        }

        memset(ts_array, 0, ts_len*sizeof(float));
    }
}


/*----------------------------------------------------------------------
 * compute a time series
 *
*/
static int compute_ts( demri_params * P, float * ts, int ts_len )
{
    if( ct_from_cp(P, P->comp, P->mcp, P->nfirst, ts_len) ) return 1;

    if( c_from_ct_cp(P, ts_len) ) return 1;

    if( R1_from_c(P, ts_len) ) return 1;

    if( Mx_from_R1(P, ts, ts_len) ) return 1;

    return 0;
}

/*----------------------------------------------------------------------
  compute Ct from Cp and params

    Ct[n] = Ktrans * ( sum [ Cp[k] * exp( -kep(n-k)*TF )
            ------                 * ( exp(kep*TF/2) - exp(-kep*TF/2) ) ]
             kep
                       + (Cp[m] + Cp[n]) * (1 - exp(-kep*TF/2)) )

    where sum is over k=m+1..n-1 (m is nfirst), and n is over m..len-1
    note: Cp[n] = Ct[n] = 0, for n in {0..m}  (m also, since that is time=0)
    note: (Cp[m]+Cp[n])*... reflects the first and last half intervals,
          while the sum reflects all the interior, full intervals

        Let P1  = Ktrans / kep
            P2  = exp(kep*TF/2) - exp(-kep*TF/2)
                = exp(-P3/2) - exp(P3/2)
            P3  = -kep*TF
            P4  = 1 - exp(-kep*TF/2)
                = 1 - exp(P3/2)

    Ct[n] = P1*P2 * sum [ Cp[k] * exp(P3*(n-k)) ] + P1*P4 * (Cp[m]+Cp[n])
    note: in exp_list, max power is (P3*(len-1-m)), m is nfirst
    note: Ct is stored in P->comp
    
    If we have a residual Ct value, rcr, apply it an let it decay as:
    by adding (rct - Cp[0]) * exp( -kep * (n+1) * TF ) for each n.

        Ct[n] += (rct - Cp[0]) * exp( P3 * (n+1) )

        note that n*TR = t (time, in seconds)
        note that Ct has already decayed by 1 TR of time, hence +1
    
    ** This is the only place that the dataset TR (which we label as TF,
       the inter-frame TR) is used in this model.
*/
static int ct_from_cp(demri_params * P, double * ct, float * cp,
                      int nfirst, int len)
{
    double     * elist = P->elist;
    double       P12, P3, P14;
    double       dval, resid;
    int          k, n;

    /* assign P*, and then fill elist[] from P3 */
    P12  = P->K / P->kep;               /* P1 for now */
    P3   = -P->kep * P->TF;             /* P3         */
    dval = exp(P3/2.0);
    P14  = P12 * (1.0 - dval);          /* P1 * P4    */
    P12  = P12 * (1.0/dval - dval);     /* P1 * P2    */

    /* In a test, float accuracy was not lost until ~1 million products.
       Computing powers over the range of a time series should be okay. */

    /* fill elist with powers of e(P3*i), i = 1..len-1 */
    elist[0] = 1.0;
    dval = exp(P3); /* first val, elist[i] is a power of this */
    for( k = 1; k < len; k++ )   /* fill the list */
        elist[k] = dval * elist[k-1];
    
    /* start at 0    8 May 2008 [rickr] */
    for( n = 0; n < len; n++ )
    {
        dval = 0.0;   /* dval is sum here */
        for( k = 1; k < n; k++ )
            dval += cp[k]*elist[n-k];
        ct[n] = P12 * dval + P14 * (cp[n]+cp[0]);
    }

    /* possibly apply the residual Ct */
    if( P->rct > 0 ) {
        dval = exp(P3); /* note: this assumes new kep, not quite accurate */

        /* if resid is C, subtract fpv*cp, but if Ct, just keep it */
        /* also, decay the C value, since it is one TR earlier than Cp */
        resid = P->rct*dval - P->fpv*cp[0];

        if( P->debug > 1 && P->counter == 0 )
            fprintf(stderr,"-- removing residuals, Ct=%f, Cp=%f, fpv=%f,"
                           " resid=%f\n", P->rct, cp[0], P->fpv, resid);

        if ( resid < 0.0 ) resid = 0.0; /* clip it if negative */

        /* elist is not long enough, even if we take it to nfirst-1 (since
           we must start at 1), so be a little slow */
        /* note, Ct has already been decayed from the prior TR */
        for( n = 0; n < len; n++ ) { ct[n] += resid; resid *= dval; }

        /* in debug case, recompute decay curve for display */
        if( P->debug > 2 && P->counter == 0 ) {
            resid = P->rct*dval - P->fpv*cp[0];
            if(resid < 0.0) fprintf(stderr,"-- resid < 0.0, no decay curve\n");
            else {
                fprintf(stderr,"++ decay curve:");
                for(n=0; n<len; n++, resid*=dval) fprintf(stderr,"  %f",resid);
                fputc('\n', stderr);
            }
        }
    }

    /* maybe print out the array */
    if( P->debug > 1 && P->counter == 0)
        print_doubles("+d ct from cp : ", ct, len);

    /* return tissue concentration of Gd over time (in ct[] via P->comp[]) */

    return 0;
}

/*----------------------------------------------------------------------
    C[n] = { 0, for n < nfirst
           { Ct[n] + fpv * Cp[n], n = nfirst..len-1

    note: C will replace Ct in P->comp
*/
static int c_from_ct_cp(demri_params * P, int len)
{
    double * C = P->comp;
    float  * cp = P->mcp;
    int      n;
    for( n = 0; n < len; n++ )
        C[n] += P->fpv * cp[n];         /* C already holds Ct */

    /* maybe print out the array */
    if( P->debug > 1 && P->counter == 0)
        print_doubles("+d c from ct/cp : ", C, len);

    return 0;
}

/*----------------------------------------------------------------------
    R1[n] = RIT + r1 * C[n]

    note: R1 will replace C in P->comp
          R1[i] will be constant P->RIT over i=0..nfirst-1
*/
static int R1_from_c(demri_params * P, int len)
{
    double * R1 = P->comp;
    int      n;
    
    for( n = 0; n < len; n++ ) 
        R1[n] = P->RIT + P->r1 * R1[n];

    /* maybe print out the array */
    if( P->debug > 1 && P->counter == 0)
        print_doubles("+d R1 from C : ", R1, len);

    return 0;
}

/*----------------------------------------------------------------------
  compute the final time series, M_transpose, from the R1 array

    Mx[n] = 1 * (1 - exp(-R1[n] * TR)) * (1 - exp(-RIT*TR)cos0)
                ---------------------------------------------
                (1 - exp(-R1[n] * TR)cos0) * (1 - exp(-RIT*TR))

          =     (1 - e[n]) * P1c) / [(1 - cos0*e[n]) * P1]

        where P1   = 1 - exp(-RIT*TR)
              P1c  = 1 - exp(-RIT*TR)*cos0
              e[n] = exp(-R1[n] * TR)

    notes:  R1 is in P->comp
            The '1' is a placeholder for Mx(t=0), which should have
              been factored out of the input time series.
            I can see no speed-up for e[n].  :'(
*/
static int Mx_from_R1(demri_params * P, float * ts, int len)
{
    double * R1 = P->comp;
    double   P1, P1c, e, cos0;
    int      n;

    cos0 = P->cos0;
    P1   = exp(-P->RIT * P->TR);
    P1c  = 1 - P1 * cos0;
    P1   = 1 - P1;

    /* and compute the last ones */
    for( n = 0; n < len; n++ )
    {
        e = exp(-R1[n] * P->TR);
        ts[n] = (1 - e)*P1c / ( (1-cos0*e) * P1);
    }

    /* maybe print out the array */
    if( P->debug > 1 && P->counter == 0)
        print_floats("+d Mx from R1 : ", ts, len);

    return 0;
}


/* get required environment vars, return 0 on success (number of errors) */
static int get_env_params(demri_params * P)
{
    char * envp;
    int    errs = 0;

    envp = my_getenv("AFNI_MODEL_D3_R1");
    if( !envp )
    {
        fprintf(stderr,"\n** NLfim: need env var AFNI_MODEL_D3_R1\n");
        fprintf(stderr,"          (in 1/(mMol * sec))\n");
        errs++;
    }
    else
    {
        P->r1 = atof(envp);
        if( P->r1 <= M_D3_R1_MIN || P->r1 >= M_D3_R1_MAX )
        {
            fprintf(stderr, "** error: r1 (%f) is not in (%f, %f)\n",
                            P->r1, M_D3_R1_MIN, M_D3_R1_MAX);
            errs++;
        }
    }

    envp = my_getenv("AFNI_MODEL_D3_RIB");
    if( !envp )
    {
        fprintf(stderr,"\n** NLfim: need env var AFNI_MODEL_D3_RIB\n");
        fprintf(stderr,"          (in seconds (reciprocal will be taken))\n");
        errs++;
    }
    else
    {
        if( get_time_in_seconds(envp, &P->RIB) )
        {
            fprintf(stderr,"** cannot process time '%s' for RIB\n", envp);
            errs++;
        }
        else if( P->RIB <= M_D3_R_MIN || P->RIB >= M_D3_R_MAX )
        {
            fprintf(stderr, "** error: RIB (%f) is not in (%f, %f)\n",
                            P->RIB, M_D3_R_MIN, M_D3_R_MAX);
            errs++;
        }
        P->RIB = 1.0 / P->RIB;  /* and take the reciprocal */
    }

    envp = my_getenv("AFNI_MODEL_D3_THETA");
    if( !envp )
    {
        fprintf(stderr,"\n** NLfim: need env var AFNI_MODEL_D3_THETA\n");
        fprintf(stderr,"          (in degrees)\n");
        errs++;
    }
    else
    {
        P->theta = atof(envp);
        P->cos0 = cos(P->theta * Mmmmmm_PIiiiii / 180.0);
        if( P->theta <= M_D3_THETA_MIN || P->theta >= M_D3_THETA_MAX )
        {
            fprintf(stderr, "** error: theta (%f) is not in (%f, %f)\n",
                            P->theta, M_D3_THETA_MIN, M_D3_THETA_MAX);
            errs++;
        }
    }

    envp = my_getenv("AFNI_MODEL_D3_TR");
    if( !envp )
    {
        fprintf(stderr,"\n** NLfim: need env var AFNI_MODEL_D3_TR\n");
        fprintf(stderr,"          (TR in seconds)\n");
        errs++;
    }
    else
    {
        if( get_time_in_seconds(envp, &P->TR) )
        {
            fprintf(stderr,"** cannot process time '%s' for TR\n", envp);
            errs++;
        }
        else if( P->TR <= M_D3_TR_MIN || P->TR >= M_D3_TR_MAX )
        {
            fprintf(stderr, "** error: TR (%f) is not in (%f, %f)\n",
                            P->TR, M_D3_TR_MIN, M_D3_TR_MAX);
            errs++;
        }
    }

    envp = my_getenv("AFNI_MODEL_D3_TF");
    if( !envp )
    {
        fprintf(stderr,"\n** NLfim: need env var AFNI_MODEL_D3_TF\n");
        fprintf(stderr,"          (TF in seconds)\n");
        errs++;
    }
    else
    {
        if( get_time_in_seconds(envp, &P->TF) )
        {
            fprintf(stderr,"** cannot process time '%s' for TF\n", envp);
            errs++;
        }
        else if( P->TF <= M_D3_TF_MIN || P->TF >= M_D3_TF_MAX )
        {
            fprintf(stderr, "** error: TF (%f) is not in (%f, %f)\n",
                            P->TF, M_D3_TF_MIN, M_D3_TF_MAX);
            errs++;
        }
    }

    /* see if the user has set a hematocrit value (should be near 0.5) */
    envp = my_getenv("AFNI_MODEL_D3_HCT");
    if( envp )
    {
        P->hct = atof(envp);
        if( P->hct < M_D3_HCT_MIN || P->hct > M_D3_HCT_MAX ) {
            fprintf(stderr,"** HCT (%f) must be in [%0.2f,%02.f]\n",
                    P->hct, M_D3_HCT_MIN, M_D3_HCT_MAX);
            errs++;
        }
    }

    if( AFNI_yesenv("AFNI_MODEL_D3_PER_MIN") )
        P->per_min = 1;

    envp = my_getenv("AFNI_MODEL_D3_DEBUG");
    if( envp ) P->debug = atoi(envp);

    /* check if non-uniform intrinsic Relaxivity map is assigned */
    envp = my_getenv("AFNI_MODEL_D3_R1I_DSET");  
    if(envp)
    {
        /* verify R1I dataset existence and open dataset */
        dset_R1I = THD_open_one_dataset (envp);
        if (dset_R1I == NULL)  
          { fprintf(stderr,"Unable to open R1I dataset: %s", envp); return 1; }

        DSET_mallocize (dset_R1I);
        DSET_load(dset_R1I);
        if( !DSET_LOADED((dset_R1I)) ) 
            { fprintf(stderr,"Can't load dataset %s",envp) ; return 1; }

        if( DSET_BRICK_TYPE(dset_R1I, 0) != MRI_float )
            { fprintf(stderr,"dset %s is not of type float\n",envp); return 1; }

        R1I_data_ptr = DSET_ARRAY(dset_R1I, 0);
        if(P->debug>0) printf("Set R1I_data_ptr\n");
    }
    else
    {                    /* should I close any open datasets? */
       if(R1I_data_ptr)
       {
           fprintf(stderr,"** MD3 R1I_data: warning, we should not be here\n");
           R1I_data_ptr = NULL;
           dset_R1I = NULL;
       }
    }


    envp = my_getenv("AFNI_MODEL_D3_RIT");
    if( !envp )
    {
        if(R1I_data_ptr) {
           fprintf(stderr,
              "Using intrinsic relaxivity map instead of single value\n");
        }
        else {
           fprintf(stderr,"\n** NLfim: need env var AFNI_MODEL_D3_RIT\n");
           fprintf(stderr,"          (in seconds (reciprocal will be taken))\n");
           errs++;
        }
    }
    else
    {
        if( get_time_in_seconds(envp, &P->RIT) )
        {
            fprintf(stderr,"** cannot process time '%s' for RIT\n", envp);
            errs++;
        }
        else if( P->RIT <= M_D3_R_MIN || P->RIT >= M_D3_R_MAX )
        {
            fprintf(stderr, "** error: RIT (%f) is not in (%f, %f)\n",
                            P->RIT, M_D3_R_MIN, M_D3_R_MAX);
            errs++;
        }
        P->RIT = 1.0 / P->RIT;  /* and take the reciprocal */
    }



    /* check for residual contrast data */
    envp = my_getenv("AFNI_MODEL_D3_RESID_CT_DSET");  
    if(envp)
    {
        /* verify RCT dataset existence and open dataset */
        dset_resid_ct = THD_open_one_dataset (envp);
        if (dset_resid_ct == NULL) {
            fprintf(stderr,"Unable to open residual Ct dataset: %s", envp);
            return 1;
        }
        DSET_mallocize (dset_resid_ct);
        DSET_load(dset_resid_ct);
        if( !DSET_LOADED((dset_resid_ct)) ) 
            { fprintf(stderr,"Can't load dataset %s",envp) ; return 1; }

        if( DSET_BRICK_TYPE(dset_resid_ct, 0) != MRI_float )
            { fprintf(stderr,"dset %s is not of type float\n",envp); return 1; }

        resid_ct_ptr = DSET_ARRAY(dset_resid_ct, 0);
        if(P->debug>0) printf("Set resid_ct_ptr\n");
    }
    else
    {                    /* should I close any open datasets? */
       if(resid_ct_ptr)
       {
           fprintf(stderr,"** MD3 resid Ct: warning, we should not be here\n");
           resid_ct_ptr = NULL;
           dset_resid_ct = NULL;
       }
    }

    if( envp && P->debug>1 && !errs ) disp_demri_params("env params set: ", P);

    return errs;
}

static int get_Mp_array(demri_params * P, int * mp_len)
{
    MRI_IMAGE * im;
    char      * envp;

    envp = my_getenv("AFNI_MODEL_D3_MP_FILE");
    if( !envp )
    {
        fprintf(stderr,"\n** NLfim: need env var AFNI_MODEL_D3_MP_FILE\n");
        fprintf(stderr,"          (a 1D file of Mp values)\n");
        return 1;
    }

    if( P->debug ) fprintf(stderr,"-d reading Mp file, %s...\n", envp);
    im = mri_read_1D(envp);
    if( !im )
    {
        fprintf(stderr,"** failed to open Mp file %s\n", envp);
        return 1;
    }

    /* nx == 1 and ny > 1, take the transpose */
    if( im->nx == 1 && im->ny > 1 )
    {
        MRI_IMAGE * flim = mri_transpose(im);
        mri_free(im);
        im = flim;
        if( !im ) { fprintf(stderr,"** MP trans failure\n"); return 1; }
        fprintf(stderr,"+d taking transpose of MP file, new len = %d\n",im->nx);
    }

    P->mcp = MRI_FLOAT_PTR(im);        /* do not free this */
    *mp_len = im->nx;
    if(P->debug>1) fprintf(stderr,"-d Mp (len, ny) = (%d, %d)\n",im->nx,im->ny);

    if( ! P->mcp )
    {
        fprintf(stderr,"** missing data in Mp file %s\n", envp);
        return 1;
    }

    envp = my_getenv("AFNI_MODEL_D3_NFIRST");
    if( !envp )
    {
        fprintf(stderr,"\nNLfim: no AFNI_MODEL_D3_NFIRST, assuming %d\n",
                       M_D3_NFIRST);
        P->nfirst = M_D3_NFIRST;
    }
    else
    {
        P->nfirst = atoi(envp);
        if( P->nfirst < 0 || P->nfirst > 1000 )
        {
            fprintf(stderr, "** NLfim: nfirst = %d must be in [0,1000]\n",
                    P->nfirst);
            return 1;
        }
    }

    if( P->debug > 1 )
    {
        int c;
        fprintf(stderr,"-d Mp array is:\n  ");
        for( c = 0; c < *mp_len; c++ )
            fprintf(stderr,"  %s", MV_format_fval(P->mcp[c]));
        fputc('\n', stderr);
    }

    return 0;
}

/*  convert Mp array to Cp(t) array

    Cp(t) =   1    * ln[ 1-exp(-RIB*TR)cos0 - (1-exp(-RIB*TR))cos0*Mp(t)/Mp(0) ]
            -----      [ ----------------------------------------------------  ]
            r1*TR      [  1-exp(-RIB*TR)cos0 - (1-exp(-RIB*TR))*Mp(t)/Mp(0)    ]

                   - RIB/r1

    subject to Cp(t) >= 0
*/
static int convert_mp_to_cp(demri_params * P, int mp_len)
{
    float * mp = P->mcp;
    double  m0;                         /* mean of first nfirst values */
    double  rTR, R_r1;                  /* first and last simple term */
    double  ertr, ertr_c0, c0_ertr_c0;  /* three 1-exp terms (1 repeat) */
    double  dval;
    int     c;

    /* use local vars for those in P, for readability */
    float  r1 = P->r1, RIB = P->RIB, TR = P->TR, cos0 = P->cos0;
    int    nfirst = P->nfirst;

    if( nfirst > mp_len ) nfirst = 0;

    /* compute m0 equal to mean of first 'nfirst' values */
    dval = 0.0;
    for(c = 0; c < nfirst; c++)
        dval += mp[c];
    if( nfirst == 0 ) m0 = 1.0;         /* then no scaling */
    else              m0 = dval / nfirst;

    if( m0 < EPSILON ) /* negative is bad, too */
    {
        fprintf(stderr,"** m0 == %s is too small (for my dreams of konquest)\n",
                MV_format_fval(m0));
        m0 = 1.0;
    }

    /* simple terms */
    rTR  = 1.0/(r1*TR);
    R_r1 = RIB/r1;

    /* exponential terms */
    ertr       =  1 - exp(-RIB * TR);
    ertr_c0    =  1 - exp(-RIB * TR)  * cos0;
    c0_ertr_c0 = (1 - exp(-RIB * TR)) * cos0;

    if(P->debug > 1) {
        fflush(stdout);
        fprintf(stderr,"-- applying nfirst = %d\n", nfirst);
        fprintf(stderr,
                "+d mp_len, m0, rTR, R_r1 = %d, %f, %f, %f\n"
                "  ertr, ertr_c0, c0_ertr_c0 = %f, %f, %f\n"
                , mp_len, m0, rTR, R_r1, ertr, ertr_c0, c0_ertr_c0);
    }

    /* we don't have to be too fast here, since this is one-time-only */

    /* start with setting nfirst terms to 0 (compute @nfirst, maybe not 0) */
    for( c = 0; c < nfirst; c++ )
        mp[c] = 0.0;

    /* and compute the remainder of the array */
    for( ; c < mp_len; c++) 
    {
        /* start with ratio */
        dval = (ertr_c0  -  c0_ertr_c0 * mp[c] / m0)  /
               (ertr_c0  -  ertr * mp[c] / m0 );

        if( dval < 1.0 ) mp[c] = 0.0;   /* if ln < 0, then c < 0, so skip */
        else             mp[c] = rTR * log(dval) - R_r1;

        if( mp[c] < 0.0 ) mp[c] = 0.0;  /* don't allow result < 0 */
    }

    /* if we have a hematocrit value, apply it */
    if( P->hct > 0 )
        for( c = 0; c < mp_len; c++ )
            mp[c] /= (1-P->hct);

    if( P->debug > 1 )      /* maybe print out the list */
    {
        fprintf(stderr,"+d HCT = %f (%sapplied, yo)\n",
                P->hct, P->hct > 0 ? "" : "not ");
        fprintf(stderr,"+d Cp =");
        for( c = 0; c < mp_len; c++ )
            fprintf(stderr,"  %s", MV_format_fval(mp[c]));
        fputc('\n', stderr);
    }

    return 0;
}

static int disp_demri_params( char * mesg, demri_params * p )
{
    if( mesg ) fputs( mesg, stderr );

    if( !p ) { fprintf(stderr,"demri_params: p == NULL\n"); return 1; }

    fprintf(stderr, "demri_params struct at %p:\n"
                    "    K      = %f  ( K trans (plasma Gd -> tissue Gd) )\n"
                    "    kep    = %f  ( back-transfer rate ( Gd_t -> Gd_p ) )\n"
                    "    ve     = %f  ( external cellular volume fraction)\n"
                    "    fpv    = %f  ( fraction of voxel occupied by blood )\n"
                    "    r1     = %f  ( 1/(mMol*seconds) )\n"
                    "    RIB    = %f  ( 1/seconds )\n"
                    "    RIT    = %f  ( 1/seconds )\n"
                    "    theta  = %f  ( degrees )\n"
                    "    TR     = %f  ( seconds (~0.007) )\n"
                    "    TF     = %f  ( seconds (~20) )\n"
                    "    hct    = %f  ( hematocrit (~0.5) )\n"
                    "    rct    = %f  ( residual Ct value )\n"
                    "\n"
                    "    cos0    = %f  ( cos(theta) )\n"
                    "    nfirst  = %d\n"
                    "    ijk     = %d\n"
                    "    debug   = %d\n"
                    "    per_min = %d\n"
                    "\n"
                    "    comp    = %p\n"
                    "    elist   = %p\n"
                    "    mcp     = %p\n"
            , p,
            p->K, p->kep, p->ve, p->fpv,
            p->r1, p->RIB, p->RIT, p->theta, p->TR, p->TF, p->hct, p->rct,
            p->cos0, p->nfirst, p->ijk, p->debug, p->per_min,
            p->comp, p->elist, p->mcp);

    return 0;
}

static int model_help(void)
{
    printf( 
    "------------------------------------------------------------\n"
    "DEMRI - Dynamic (contrast) Enhanced MRI\n"
    "\n"
    "   MODEL demri_3: 3-parameter DEMRI model\n"
    "\n"
    "   model parameters to fit:\n"
    "\n"
    "       K_trans   in [0, 0.99]\n"
    "       k_ep      in [0, 0.99]\n"
    "       f_pv      in [0, 0.99]\n"
    "\n"
    "       Note that K_trans and k_ep default to rates per second.\n"
    "       If per minute is desired, use AFNI_MODEL_D3_PER_MIN.\n"
    "\n"
    "   model parameters passed via environment variables:\n"
    "\n"
    "       --- AFNI_MODEL_D3_R1 ---------------------------------\n"
    "\n"
    "           This sets 'r1', the relaxivity rate for the contrast,\n"
    "           measured in 1/(mMol*second), restricted to [%.1f, %.1f].\n"
    "\n"
    "           e.g. 4.8 (@ 1.5T), or 9.6 (@ 3T)\n"
    "\n"
    "       --- AFNI_MODEL_D3_RIB --------------------------------\n"
    "\n"
    "           This sets R_1,i for blood, the intrinsic relaxivity rate\n"
    "           in 1/(mMol*sec), restricted to [%.3f, %.1f].\n"
    "           ** specified as reciprocal, in seconds\n"
    "\n"
    "           e.g. 1.3 or 1.3s or 1300ms\n"
    "\n"
    "       --- AFNI_MODEL_D3_RIT --------------------------------\n"
    "\n"
    "           This sets R_1,i for tissue, the intrinsic relaxivity rate\n"
    "           in 1/(mMol*sec), restricted to [%.3f, %.1f].\n"
    "           ** specified as reciprocal, in seconds\n"
    "\n"
    "           e.g. 0.8 or 0.8s or 800ms\n"
    "\n"
    "       --- AFNI_MODEL_D3_R1I_DSET ---------------------------\n"
    "\n"
    "           This specifies the intrinsic relaxivity volume.\n"
    "\n"
    "           It serves as a replacement for AFNI_MODEL_D3_RIT, except\n"
    "           that the rate values are given in inverse seconds and it is\n"
    "           specified as a dataset, where RIT can vary over space.\n"
    "\n"
    "           ** NOTE: be sure to note that these values are specified in\n"
    "                    _inverse_ seconds, not in seconds as above\n"
    "\n"
    "           e.g. R1Irat+orig\n"
    "\n"
    "       --- AFNI_MODEL_D3_THETA ------------------------------\n"
    "\n"
    "           This sets theta, the flip angle, specified in degrees\n"
    "           and restricted to [%.1f, %.1f].\n"
    "\n"
    "           e.g. 30\n"
    "\n"
    "       --- AFNI_MODEL_D3_TR --------------------------------\n"
    "\n"
    "           This sets the TR, the time between shots, in seconds, and\n"
    "           restricted to [%.1f, %.1f].\n"
    "\n"
    "           e.g. 0.0076 or 0.0076s or 7.6ms\n"
    "\n"
    "       --- AFNI_MODEL_D3_TF ---------------------------------\n"
    "\n"
    "           Set the TF, the time between frames (volumes), in seconds\n"
    "           restricted to [%.1f, %.1f].  This is typically called the TR\n"
    "           in AFNI FMRI datasets.\n"
    "\n"
    "           e.g. 20 or 20sec\n"
    "\n"
    "       --- AFNI_MODEL_D3_HCT --------------------------------\n"
    "\n"
    "           Set the hematocrit value, the proportion of blood volume\n"
    "           that is occupied by red blood cells, in [%.1f, %.1f].\n"
    "\n"
    "           typical values are 0.47 for men and 0.42 for women\n"
    "\n"
    "       --- AFNI_MODEL_D3_PER_MIN ----------------------------\n"
    "\n"
    "           The default rate units of K_trans and k_ep are per second.\n"
    "           If this variable is set to YES, then rates are per minute.\n"
    "\n"
    "     ** note: default units are with respect to seconds\n"
    "\n"
    "       --- AFNI_MODEL_D3_RESID_CT_DSET ---------------------------\n"
    "\n"
    "           In the case of multiple injections, the current data may have\n"
    "           residual contrast from a previous injection.  To let this\n"
    "           model know about it, the user can specify a volume dataset\n"
    "           containing the residual C value (total concentration) from\n"
    "           the previous curve).\n"
    "\n"
    "           This model would then compute { C - fpv*Cp(0) } and let it\n"
    "           decay, multiplying by exp(-kep*t) over time.  This time\n"
    "           series would be added to the resulting C curve.\n"

    "           e.g. residual_Ct+orig\n"
    "\n"
    "        ** Note that in this case, the input time series must be scaled\n"
    "           prior to input.  For example, if a time series consisted of\n"
    "           90 TRs, where there were injections at TR = 5 and at TR = 45,\n"
    "           then the entire 90 TR dataset should be scaled by the mean\n"
    "           (or median) of the first 5 TRs.\n"
    "\n"
    "           After that, the first and last 45 TRs can be given to 3dNLfim\n"
    "           separately.\n"
    "\n"
    "   environment variables to control Mp(t):\n"
    "       AFNI_MODEL_D3_MP_FILE : file containing Mp(t) data\n"
    "       AFNI_MODEL_D3_NFIRST  : to set the number of TRs averaged for M0\n"
    "                               (if data is not scaled, this will do it)\n"
    "\n"
    "   optional environment variables:\n"
    "       AFNI_MODEL_HELP_DEMRI_3 (Y/N) : to get this help\n"
    "       AFNI_MODEL_D3_USE_VE    (Y/N) : param #2 is Ve, not k_ep\n"
    "       AFNI_MODEL_D3_DEBUG     (0..2): to set debug level\n"
    "\n"
    "  -----------------------------------------------\n"
    "\n"
    "  Assumptions:\n"
    "\n"
    "    - nfirst is the number of TRs before Gd injection\n"
    "    - the input M_trans is normalized:\n"
    "        M_trans[0] = mean( M_trans[0..nfirst-1] )\n"
    "        M_trans[k] = M_trans[k] / M_trans[0], k=0..N-1\n"
    "    - m0 is set to average of first nfirst Mp(t) values\n"
    "\n"
    "  -----------------------------------------------\n"
    "  sample command-script:\n"
    "\n"
    "      setenv AFNI_MODEL_D3_R1         4.8\n"
    "      setenv AFNI_MODEL_D3_RIB        1500ms\n"
    "      setenv AFNI_MODEL_D3_RIT        1100ms\n"
    "      setenv AFNI_MODEL_D3_TR         8.0ms\n"
    "      setenv AFNI_MODEL_D3_THETA      30\n"
    "      setenv AFNI_MODEL_D3_TF         20s\n"
    "      \n"
    "      setenv AFNI_MODEL_D3_PER_MIN    Y\n"
    "      setenv AFNI_MODEL_D3_MP_FILE    ROI_mean.1D\n"
    "      setenv AFNI_MODEL_D3_NFIRST     7\n"
    "      \n"
    "      3dNLfim -input scaled_data.nii  \\\n"
    "          -signal demri_3             \\\n"
    "          -noise  Zero                \\\n"
    "          -sconstr 0 0 3              \\\n"
    "          -sconstr 1 0 3              \\\n"
    "          -sconstr 2 0 1              \\\n"
    "          -nconstr 0 0.0 0.0          \\\n"
    "          -SIMPLEX                    \\\n"
    "          -nabs                       \\\n"
    "          -ignore 0                   \\\n"
    "          -nrand  10000               \\\n"
    "          -mask   my_mask+orig        \\\n"
    "          -nbest  10                  \\\n"
    "          -jobs 2                     \\\n"
    "          -voxel_count                \\\n"
    "          -sfit     subj7.sfit        \\\n"
    "          -bucket 0 subj7.buc\n"
    "\n"
    "  -----------------------------------------------\n"
    "  R Reynolds, D Glen, Oct 2006\n"
    "  thanks to RW Cox\n"
    "------------------------------------------------------------\n",
    M_D3_R1_MIN,    M_D3_R1_MAX,
    M_D3_R_MIN,     M_D3_R_MAX,
    M_D3_R_MIN,     M_D3_R_MAX,
    M_D3_TR_MIN,    M_D3_TR_MAX,
    M_D3_THETA_MIN, M_D3_THETA_MAX,
    M_D3_TF_MIN,    M_D3_TF_MAX,
    M_D3_HCT_MIN,   M_D3_HCT_MAX
    );

    return 0;
}

static int alloc_param_arrays(demri_params * P, int len)
{
    P->comp  = (double *)malloc(len * sizeof(double));
    P->elist = (double *)malloc(len * sizeof(double));

    if( !P->comp || !P->elist )
    {
        fprintf(stderr,"** alloc_param_arrays failure for %d doubles\n",len);
        return 1;
    }
    return 0;
}

/* str can be of the form "7.2", "7.2s" or "7.2ms" */
static int get_time_in_seconds( char * str, float * time )
{
    char * cp;
    *time = strtod(str, &cp);
    if( cp == str ) return 1;
    if( *cp && !strncmp(cp, "ms", 2) )
        *time /= 1000.0;
    return 0;
}

static void print_doubles(char * mesg, double * list, int len)
{
    int c;
    fputs(mesg, stderr);
    for( c = 0; c < len; c++ )
        fprintf(stderr,"  %s", MV_format_fval(list[c]));
    fputc('\n', stderr);
}

static void print_floats(char * mesg, float * list, int len)
{
    int c;
    fputs(mesg, stderr);
    for( c = 0; c < len; c++ )
        fprintf(stderr,"  %s", MV_format_fval(list[c]));
    fputc('\n', stderr);
}
