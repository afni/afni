
/*----------------------------------------------------------------------
  This is for the 3-parameter DEMRI model from Bob Cox and Steve Fung,
  implemented for John Butman and Steve Fung.


  Author: Rick Reynolds  22 Oct 2006
 *----------------------------------------------------------------------
*/

#include "NLfit_model.h"

#define M_D3_R1_MIN     0.0     /* these are exclusive limits */
#define M_D3_R1_MAX     1000.0
#define M_D3_R_MIN      1.0     /* require > 1 to exclude reciprocal */
#define M_D3_R_MAX      10000.0
#define M_D3_TR_MIN     0.0
#define M_D3_TR_MAX     10000.0
#define M_D3_THETA_MIN  0.0
#define M_D3_THETA_MAX  90.0

#define M_D3_NFIRST     5
#define EPSILON         0.0001
#define Mmmmmm_PIiiiii  3.1415926535897932385

/* rcr: questions
    Do we get TR from x_array?    (x_array[1][1] - x_array[0][1])
    Should m0 be considered as 1?
    Is nfirst going to equal the infusion TR?
    In eqn 7, should Ct[t] be multiplied by (1-fpv)?
*/

void signal_model 
(
    float  * params,           /* parameters for signal model */
    int      ts_len,           /* length of time series data */
    float ** x_array,          /* independent variable matrix */
    float  * ts_array          /* estimated signal model time series */  
);

typedef struct
{
    float    K, kep, fvp;       /* fit params */
    float    r1, R, TR, theta;  /* given params (via env) */
    int      nfirst;            /* num TRs used to compute mean Mp,0 */
    int      debug;
    double * comp;              /* computation data, and elist */
    double * elist;             /* (for easy allocation, etc.) */
} demri_params;

static int alloc_param_arrays(demri_params * P, int len);
static int compute_ts       (demri_params *P, float *cp, float *ts, int ts_len);
static int c_from_ct_cp     (demri_params * P, float * cp, int len);
static int ct_from_cp       (demri_params * P, float * cp, int len);
static int disp_demri_params(char * mesg, demri_params * p );
static int convert_mp_to_cp (float * mp, int mp_len, demri_params * P);
static int get_env_params   (demri_params * P);
static int get_Mp_array     (float ** mp, int * mp_len, demri_params * P);
static int model_help       (void);
static int Mx_from_R1       (demri_params * P, float * ts, int len);
static int R1_from_c        (demri_params * P, int len);

/*----------------------------------------------------------------------

  Initialize MODEL_interface struct with default values and model name.
*/

DEFINE_MODEL_PROTOTYPE  /* for C++ */

MODEL_interface * initialize_model ()
{
    MODEL_interface * M;

    if( my_getenv("AFNI_MODEL_HELP_DEMRI_3") )
        model_help();

    /* get space for new struct */
    M = (MODEL_interface *)malloc(sizeof(MODEL_interface));

    strcpy(M->label, "demri_3");           /* 3-param DEMRI             */
    M->model_type = MODEL_SIGNAL_TYPE;     /* signal model (not noise)  */
    M->params = 3;                         /* number of user parameters */

    /* set param labels */
    strcpy(M->plabel[0], "K_trans");
    strcpy(M->plabel[1], "k_ep");
    strcpy(M->plabel[2], "f_pv");

    /* set min/max constraints */
    M->min_constr[0] = 0.0;  M->max_constr[0] = 0.05;
    M->min_constr[1] = 0.0;  M->max_constr[1] = 0.05;
    M->min_constr[2] = 0.0;  M->max_constr[2] = 1.00;
  
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
    static demri_params P = {0.0, 0.0, 0.0,   0.0, 0.0, 0.0, 0.0,   0, 0};
    static int          first_call = 1;

    float        * mcp = NULL;  /* from 1D file, via env var */
    int            mp_len;      /* length of mcp list (must equal ts_len) */

    /* first time here, get set params, and process Mp data */
    if( first_call )
    {
        if( get_env_params( &P ) ) exit(1); /* bad things, man, bad things */

        if( get_Mp_array(&mcp, &mp_len, &P) ) exit(1);

        if( alloc_param_arrays(&P, mp_len) ) exit(1);

        /* after this, mp is cp */
        if( convert_mp_to_cp(mcp, mp_len, &P) ) exit(1);

        /* verify TR (maybe we don't need TR) */
        if( P.TR != x_array[1][1] - x_array[1][0] )
            fprintf(stderr, "** warning: TR (%f) != x_array time (%f)\n",
                    P.TR, x_array[1][1] - x_array[1][0]);

        /* verify time series length */
        if( mp_len != ts_len )
        {
            fprintf(stderr,"** mp_len (%d) != ts_len (%d), going home...\n",
                    mp_len, ts_len);
            exit(1);
        }

        first_call = 0;
    }

    /* note passed parameters */
    P.K   = params[0];
    P.kep = params[1];
    P.fvp = params[2];

    (void)compute_ts( &P, mcp, ts_array, ts_len );
}


/*----------------------------------------------------------------------
 * compute a time series
 *
*/
static int compute_ts( demri_params * P, float * cp, float * ts, int ts_len )
{
    if( ct_from_cp(P, cp, ts_len) ) return 1;

    if( c_from_ct_cp(P, cp, ts_len) ) return 1;

    if( R1_from_c(P, ts_len) ) return 1;

    if( Mx_from_R1(P, ts, ts_len) ) return 1;

    return 0;
}

/*----------------------------------------------------------------------
  compute Ct from Cp and params

    Ct[n] = Ktrans * ( sum [ Cp[k] * exp( -kep(n-k)*TR )
            ------                 * ( exp(kep*TR/2) - exp(-kep*TR/2) ) ]
             kep       + Cp[n] * (1 - exp(-kep*TR/2)) )

    where sum is over k=m..n-1 (m is nfirst), and n is over 0..len-1
    note: Cp[n] = Ct[n] = 0, for n in {0..m-1}

        Let P1  = Ktrans / kep
            P2  = exp(kep*TR/2) - exp(-kep*TR/2)
                = exp(-P3/2) - exp(P3/2)
            P3  = -kep*TR
            P4  = 1 - exp(-kep*TR/2)
                = 1 - exp(P3/2)

    Ct[n] = P1*P2 * sum [ Cp[k] * exp(P3*(n-k)) ] + P1*P4 * Cp[n]
    note: in exp_list, max power is (P3*(len-1-m)), m is nfirst
    note: Ct is stored in P->comp
*/
static int ct_from_cp(demri_params * P, float * cp, int len)
{
    static int   first = 1;
    double     * ct = P->comp;
    double     * elist = P->elist;
    double       P12, P3, P14;
    double       dval;
    int          k, n;

    /* we don't need to fill with zeros every time, but be explicit */
    if( first ){ for(n=0; n < P->nfirst; n++) ct[n] = 0.0;  first = 0; }

    /* assign P*, and then fill elist[] from P3 */
    P12  = P->K / P->kep;               /* P1 for now */
    P3   = -P->kep * P->TR;             /* P3         */
    dval = exp(P3/2.0);
    P14  = P12 * (1.0 - dval);          /* P1 * P4    */
    P12  = P12 * (1.0/dval - dval);     /* P1 * P2    */

    /* In a test, float accuracy was not lost until ~1 million products.
       Computing powers over the range of a time series should be okay. */

    /* fill elist with powers of e(P3*i), i = 1..len-1-nfirst */
    elist[0] = 1.0;
    dval = exp(P3); /* first val, elist[i] is a power of this */
    for( k = 1; k < len - P->nfirst; k++ )   /* fill the list */
        elist[k] = dval * elist[k-1];
    
    for( n = P->nfirst; n < len; n++ )   /* ct[k] = 0, for k < nfirst */
    {
        dval = 0;   /* dval is sum here */
        for( k = P->nfirst; k < n; k++ )
            dval += cp[k]*elist[n-k];
        ct[n] = P12 * dval + P14 * cp[n];
    }

    return 0;
}

/*----------------------------------------------------------------------
    C[n] = { 0, for n < nfirst
           { Ct[n] + fpv * Cp[n], n = nfirst..len-1

    note: C will replace Ct in P->comp
*/
static int c_from_ct_cp(demri_params * P, float * cp, int len)
{
    double * C = P->comp;
    int      n;
    for( n = P->nfirst; n < len; n++ )
        C[n] += P->fvp * cp[n];         /* C already holds Ct */
    return 0;
}

/*----------------------------------------------------------------------
    R1[n] = R + r1 * C[n]

    note: R1 will replace C in P->comp
*/
static int R1_from_c(demri_params * P, int len)
{
    double * R1 = P->comp;
    int      n;
    for( n = 0; n < len; n++ )
        R1[n] = P->R + P->r1 * R1[n];
    return 0;
}

/*----------------------------------------------------------------------
  compute the final time series, M_transpose, from the R1 array

    Mx[n] = 1 * (1 - exp(-R1[n] * TR)) * (1 - exp(-R*TR)cos0)
                ---------------------------------------------
                (1 - exp(-R1[n] * TR)cos0) * (1 - exp(-R*TR))

          =     (1 - e[n]) * P1c) / [(1 - cos0*e[n]) * P1]

        where P1   = 1 - exp(-R*TR)
              P1c  = 1 - exp(-R*TR)*cos0
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

    cos0 = cos(P->theta * Mmmmmm_PIiiiii / 180.0);
    P1   = - exp(-P->R * P->TR);
    P1c  = 1 - P1 * cos0;
    P1   = 1 - P1;

    for( n = 0; n < len; n++ )
    {
        e = exp(-R1[n] * P->TR);
        ts[n] = (1 - e)*P1c / ( (1-cos0*e) * P1);
    }
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
        fprintf(stderr,"          (in mMol * sec)\n");
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

    envp = my_getenv("AFNI_MODEL_D3_R");
    if( !envp )
    {
        fprintf(stderr,"\n** NLfim: need env var AFNI_MODEL_D3_R\n");
        fprintf(stderr,"          (in ms (reciprocal will be taken))\n");
        errs++;
    }
    else
    {
        P->R = atof(envp);
        if( P->R <= M_D3_R_MIN || P->R >= M_D3_R_MAX )
        {
            fprintf(stderr, "** error: R (%f) is not in (%f, %f)\n",
                            P->R, M_D3_R_MIN, M_D3_R_MAX);
            errs++;
        }
        P->R = 1.0 / P->R;  /* and take the reciprocal */
    }

    envp = my_getenv("AFNI_MODEL_D3_TR");
    if( !envp )
    {
        fprintf(stderr,"\n** NLfim: need env var AFNI_MODEL_D3_TR\n");
        fprintf(stderr,"          (TR in milliseconds)\n");
        errs++;
    }
    else
    {
        P->TR = atof(envp);
        if( P->TR <= M_D3_TR_MIN || P->TR >= M_D3_TR_MAX )
        {
            fprintf(stderr, "** error: TR (%f) is not in (%f, %f)\n",
                            P->TR, M_D3_TR_MIN, M_D3_TR_MAX);
            errs++;
        }
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
        if( P->theta <= M_D3_THETA_MIN || P->theta >= M_D3_THETA_MAX )
        {
            fprintf(stderr, "** error: theta (%f) is not in (%f, %f)\n",
                            P->theta, M_D3_THETA_MIN, M_D3_THETA_MAX);
            errs++;
        }
    }

    envp = my_getenv("AFNI_MODEL_D3_DEBUG");
    if( envp ) P->debug = atoi(envp);

    if( envp && P->debug ) disp_demri_params("env params set: ", P);

    return errs;
}

static int get_Mp_array(float ** mp, int * mp_len, demri_params * P)
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

    *mp = MRI_FLOAT_PTR(im);        /* do not free this */
    *mp_len = im->nx;
    if(P->debug>1) fprintf(stderr,"-d Mp (len, ny) = (%d, %d)\n",im->nx,im->ny);

    if( ! *mp )
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
            fprintf(stderr,"  %s", MV_format_fval((*mp)[c]));
        fputc('\n', stderr);
    }

    return 0;
}

/*  convert Mp array to Cp(t) array

    Cp(t) =   1/(r1*TR) * ln[ 1-exp(-R*TR)cos0 - (1-exp(-R*TR))cos0*Mp(t)/Mp(0)]
                            [ ------------------------------------------------ ]
                            [ 1-exp(-R*TR)cos0 - (1-exp(-R*TR))*Mp(t)/Mp(0)    ]
                - R/r1

    subject to Cp(t) >= 0
*/
static int convert_mp_to_cp(float * mp, int mp_len, demri_params * P)
{
    float  m0;                          /* should be first nfirst values */
    double cos0;
    double rTR, R_r1;                   /* first and last simple term */
    double ertr, ertr_c0, c0_ertr_c0;   /* three 1-exp terms (1 repeat) */
    double dval;
    int    c;

    /* use local vars for those in P, for readability */
    float  r1 = P->r1, R = P->R, TR = P->TR, theta = P->theta;
    int    nfirst = P->nfirst;

    /* compute m0 equal to mean of first 'nfirst' values */
    dval = 0;
    for(c = 0; c < nfirst; c++)
        dval += mp[c];
    dval /= nfirst;

    if( m0 < EPSILON ) /* negative is bad, too */
    {
        fprintf(stderr,"** m0 == %s is too small (for my dreams of konquest)\n",
                MV_format_fval(m0));
        return 1;
    }

    /* simple terms */
    cos0 = cos(theta * Mmmmmm_PIiiiii / 180.0);
    rTR  = 1.0/(r1*TR);
    R_r1 = R/r1;

    /* exponential terms */
    ertr       =  1 - exp(-R * TR);
    ertr_c0    =  1 - exp(-R * TR)  * cos0;
    c0_ertr_c0 = (1 - exp(-R * TR)) * cos0;

    /* we don't have to be too fast here, since this is one-time-only */

    /* start with setting nfirst terms to 0 */
    for( c = 0; c < nfirst; c++ )
        mp[c] = 0.0;

    /* and compute the remainder of the array */
    for( ; c < mp_len; c++) 
    {
        /* start with ratio */
        dval = (ertr_c0  -  c0_ertr_c0 * mp[c] / m0)  /
               (ertr_c0  -  ertr * mp[c] / m0 );

        if( dval < 1.0 ) mp[c] = 0.0;   /* if ln < 0, then c < 0, so skip */
        mp[c] = rTR * log(dval) - R_r1;

        if( mp[c] < 0.0 ) mp[c] = 0.0;  /* don't allow result < 0 */
    }

    if( P->debug > 1 )      /* maybe print out the list */
    {
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

    fprintf(stderr, "dermi_params struct at %p:\n"
                    "    K      = %f\n"
                    "    kep    = %f\n"
                    "    fvp    = %f\n\n"
                    "    r1     = %f\n"
                    "    R      = %f\n"
                    "    TR     = %f\n"
                    "    theta  = %f\n"
                    "    nfirst = %d\n\n"
                    "    debug  = %d\n"
                    "    comp   = %p\n"
                    "    elist  = %p\n"
            , p,
            p->K, p->kep, p->fvp,
            p->r1, p->R, p->TR, p->theta,
            p->nfirst, p->debug, p->comp, p->elist);

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
        "       K_trans   in [0, 0.05]\n"
        "       k_ep      in [0, 0.05]\n"
        "       f_pv      in [0, 1.00]\n"
        "\n"
        "   model parameters passed via environment variables:\n"
        "       AFNI_MODEL_D3_R1    : to set r1        in [%f, %f]\n"
        "       AFNI_MODEL_D3_R     : to set R_1,i     in [%f, %f]\n"
        "       AFNI_MODEL_D3_TR    : to set TR        in [%f, %f]\n"
        "       AFNI_MODEL_D3_THETA : to set theta     in [%f, %f]\n"
        "\n"
        "       note: R_1,i is applied as its reciprocal (time in ms)\n"
        "\n"
        "   environment variables to control Mp(t):\n"
        "       AFNI_MODEL_D3_MP_FILE : file containing Mp(t) data\n"
        "       AFNI_MODEL_D3_NFIRST  : to set nfirst (injection TR)\n"
        "\n"
        "   optional environment variables:\n"
        "       AFNI_MODEL_HELP_DEMRI_3 : to get this help\n"
        "       AFNI_MODEL_D3_DEBUG     : to set debug level\n"
        "\n"
        "   -----------------\n"
        "\n"
        "Assumptions:\n"
        "\n"
        "  - nfirst is the number of TRs before Gd infusion\n"
        "  - m0 is set to average of first nfirst Mp(t) values\n"
        "\n"
        "  R Reynolds, Oct 2006\n"
        "  thanks to RW Cox, D Glen\n"
        "------------------------------------------------------------\n",
        M_D3_R1_MIN,    M_D3_R1_MAX,
        M_D3_R_MIN,     M_D3_R_MAX,
        M_D3_TR_MIN,    M_D3_TR_MAX,
        M_D3_THETA_MIN, M_D3_THETA_MAX
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

