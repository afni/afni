/*-------------------------------------------------------------------------
  3dShuffle: randomization tests for AFNI datasets.

  Implements paired/repeated-measures and one-sample sign-flip tests,
  plus independent-group two-sample label-shuffle tests, for
  single-brick datasets, or brickwise through multi-brick datasets when
  -brickwise is used.

  Signed z-score bricks derived from the empirical permutation p-values
  are tagged as FIZT stat bricks so AFNI's interactive GUI threshold
  slider displays the permutation-derived p-value. Threshold on z_fwe,
  rather than the parametric-reference t brick, for reported results.
-------------------------------------------------------------------------*/

#include "mrilib.h"
#include <ctype.h>
#include <limits.h>
#include <math.h>
#include <float.h>

#ifdef USE_OMP
#include <omp.h>
#endif

#undef DECLARE_ithr
#ifdef USE_OMP
# define DECLARE_ithr const int ithr = omp_get_thread_num()
#else
# define DECLARE_ithr const int ithr = 0
#endif

#define PROGRAM_NAME "3dShuffle"

/* Number of random relabelings used when -mode random is in effect but
   -niter was not given. Matches PALM's default. It puts the smallest
   reportable p-value at 1/(NITER+1) = 1e-4, far below any threshold in
   practical use, and leaves enough draws that the max-statistic tail behind
   the FWE correction is stably estimated. */
#define DEFAULT_NITER 10000

/* Exact enumeration past this many relabelings is still representable, but
   it costs enormously more than random sampling and resolves p-values that
   nobody reports. See resolve_mode(). */
#define EXACT_WARN_COUNT 1000000LL

/* Exact runs store one max statistic per relabeling in an int-indexed array,
   so the count has to fit in INT_MAX. For sign flips that means 2^N <=
   INT_MAX, i.e. N <= 30; for a balanced two-sample contrast it caps
   choose(NA+NB,NA) and so allows about 33 datasets in total. */
#define EXACT_MAX_SIGNFLIP_N 30

typedef enum {
   TAIL_TWO = 0,
   TAIL_ONE
} tail_code;

typedef enum {
   MODE_AUTO = 0,
   MODE_EXACT,
   MODE_RANDOM
} mode_code;

typedef enum {
   STAT_PAIRED = 0,
   STAT_ONESAMPLE,
   STAT_TWOSAMPLE
} stat_code;

typedef enum {
   METHOD_SIGNFLIP = 0,
   METHOD_SHUFFLE
} method_code;

typedef struct {
   int ia;
   int ib;
   char *a;
   char *b;
   char *name;
} contrast_t;

typedef struct {
   int ncond;
   int ninput;
   int nsubj;
   int ntotal;
   int ncon;
   int auto_mask;
   int brickwise;
   int unpooled;
   int have_niter;     /* -niter was given on the command line */
   int mode_explicit;  /* -mode was given on the command line  */
   int niter;
   long seed;
   char *prefix;
   char *mask_name;
   char **cond_labels;
   char ***input_names;
   int *nsubj_by_cond;
   int *offsets;
   contrast_t *cons;
   tail_code tails;
   mode_code mode;
   stat_code stat;
   method_code method;
   int tfce;            /* -tfce was given */
   float tfce_H;         /* height exponent, default 2.0 */
   float tfce_E;         /* extent exponent, default 0.5 */
   float tfce_dh;        /* integration step; 0 means auto (obs max / 100) */
   int tfce_conn;        /* voxel connectivity: 6, 18, or 26 */
   int nx, ny, nz;       /* grid dimensions, set from the first dataset */
} opts_t;

typedef struct {
   float *mean;
   float *tstat;
   float *p_unc;
   float *p_fwe;
   float *z_unc;
   float *z_fwe;
} test_output_t;

/* Print command-line help and exit. */
static void shuffle_help(void)
{
   printf(
"Usage: 3dShuffle [options]\n"
"\n"
"Permutation/randomization testing for AFNI datasets.\n"
"\n"
"Paired/repeated-measures and one-sample tests use sign flips. Independent\n"
"two-sample tests shuffle group labels while preserving group sizes.\n"
"By default, all inputs must be single-brick datasets on the same grid.\n"
"With -brickwise, each -input takes one multi-brick dataset and each brick\n"
"is treated like one dataset in the corresponding input list.\n"
"\n"
"Example 1 -- paired/repeated-measures t-tests:\n"
"  3dShuffle                                      \\\n"
"    -conditions 3                                \\\n"
"    -cond_labels baseline endsleep longrec       \\\n"
"    -input s01_base+tlrc s02_base+tlrc ...       \\\n"
"    -input s01_end+tlrc  s02_end+tlrc  ...       \\\n"
"    -input s01_ltr+tlrc  s02_ltr+tlrc  ...       \\\n"
"    -contrast baseline endsleep                  \\\n"
"    -contrast baseline longrec                   \\\n"
"    -contrast endsleep longrec                   \\\n"
"    -method signflip                             \\\n"
"    -stat paired_ttest                           \\\n"
"    -tails two                                   \\\n"
"    -mode exact                                  \\\n"
"    -mask sleep_group_mask+tlrc                  \\\n"
"    -prefix sleep_shuffle\n"
"\n"
"Example 2 -- one-sample t-test against zero:\n"
"  3dShuffle                                      \\\n"
"    -conditions 1                                \\\n"
"    -cond_labels activation                      \\\n"
"    -input s01_activation+tlrc s02_activation+tlrc \\\n"
"           s03_activation+tlrc s04_activation+tlrc \\\n"
"           s05_activation+tlrc s06_activation+tlrc \\\n"
"    -method signflip                             \\\n"
"    -stat onesample                              \\\n"
"    -tails two                                   \\\n"
"    -mode exact                                  \\\n"
"    -mask activation_group_mask+tlrc             \\\n"
"    -prefix activation_shuffle\n"
"\n"
"Example 3 -- independent two-sample t-test:\n"
"  3dShuffle                                      \\\n"
"    -conditions 2                                \\\n"
"    -cond_labels patient control                 \\\n"
"    -input pat01+tlrc pat02+tlrc pat03+tlrc      \\\n"
"           pat04+tlrc pat05+tlrc                 \\\n"
"    -input con01+tlrc con02+tlrc con03+tlrc      \\\n"
"           con04+tlrc con05+tlrc con06+tlrc      \\\n"
"    -contrast patient control                    \\\n"
"    -method shuffle                              \\\n"
"    -stat twosample                              \\\n"
"    -tails two                                   \\\n"
"    -mode random                                 \\\n"
"    -niter 10000                                 \\\n"
"    -seed 1234567                                \\\n"
"    -mask patient_control_mask+tlrc              \\\n"
"    -prefix patient_vs_control_shuffle\n"
"\n"
"Example 4 -- paired test with wildcard-expanded -input lists:\n"
"  Wildcards ('*' and '?') in a -input filename are expanded by 3dShuffle\n"
"  itself, the same as -setA/-setB in 3dttest++. Quote each wildcarded\n"
"  argument so the shell passes it through unexpanded, especially when it\n"
"  also carries a sub-brick selector like '[..]'.\n"
"\n"
"  3dShuffle                                      \\\n"
"    -conditions 2                                \\\n"
"    -cond_labels OLS REML                        \\\n"
"    -input 'OLSQ.*.HEAD[Vrel#0_Coef]'             \\\n"
"    -input 'REML.*.HEAD[Vrel#0_Coef]'             \\\n"
"    -contrast OLS REML                            \\\n"
"    -method signflip                              \\\n"
"    -stat paired_ttest                            \\\n"
"    -tails two                                    \\\n"
"    -mode exact                                   \\\n"
"    -mask mask+tlrc.                              \\\n"
"    -prefix olsreml_shuffle\n"
"\n"
"Required options:\n"
"  -conditions N       Number of repeated-measure conditions or one-sample\n"
"                      input lists.\n"
"  -input dset ...     One -input list per condition. Paired and one-sample\n"
"                      lists must have equal counts; two-sample lists may\n"
"                      have different counts.\n"
"  -brickwise          Each -input must contain exactly one dataset. The\n"
"                      program loops over that dataset's sub-bricks as the\n"
"                      samples/subjects for that condition. For paired and\n"
"                      one-sample tests, all inputs must have the same\n"
"                      number of sub-bricks; for two-sample tests, groups\n"
"                      may have different numbers of sub-bricks.\n"
"  -contrast A B       For paired and two-sample tests, test A-B. A and B\n"
"                      can be condition labels or 1-based condition indices.\n"
"                      May be repeated. Not used with -stat onesample.\n"
"  -prefix PREFIX      Output bucket dataset prefix.\n"
"\n"
"Labels:\n"
"  -cond_labels L1 ... LN\n"
"                      Names for the N conditions. Used for contrasts and\n"
"                      output sub-brick labels.\n"
"\n"
"Permutation options:\n"
"  -method signflip    Required for paired and one-sample tests. Default.\n"
"  -method shuffle     Required for two-sample tests. Group labels are\n"
"                      exchanged while the original group sizes are fixed.\n"
"  -stat paired_ttest  Paired t-statistic. Default.\n"
"  -stat onesample     One-sample t-statistic against zero. Each input list\n"
"                      is tested independently; do not use -contrast.\n"
"  -stat twosample     Independent-group t-statistic for each contrast A-B.\n"
"                      The default assumes equal variances (pooled t-test).\n"
"  -unpooled           With -stat twosample, use the unequal-variance Welch\n"
"                      t-statistic. This overrides the pooled assumption.\n"
"  -tails two|one\n"
"                      Default: two.\n"
"                      With -tails one, the tested direction is positive.\n"
"                      For contrasts, -contrast A B tests whether A > B;\n"
"                      reverse the order to test B > A. For one-sample and\n"
"                      two-sample group outputs, positive means group > 0.\n"
"  -mode exact|random  Exact enumerates all 2^N sign patterns for one-sample\n"
"                      tests and all choose(NA+NB,NA) group assignments for\n"
"                      two-sample contrasts. Random uses -niter draws.\n"
"                      Default: exact when that is feasible, otherwise\n"
"                      random. See 'Choosing exact versus random' below.\n"
"  -niter N            Number of random sign-flip or shuffle iterations.\n"
"                      Default: %d. Ignored by -mode exact.\n"
"  -seed S             Random seed for -mode random. Default: 1234567.\n"
"\n"
"Threshold-free cluster enhancement (Smith & Nichols, 2009):\n"
"  -tfce               Replace the raw per-voxel statistic with its TFCE\n"
"                      score before running the max-stat permutation\n"
"                      correction. Works with every -stat. For -stat\n"
"                      twosample this enhances the GrpA/GrpB one-sample\n"
"                      sub-tests and the CON contrast independently, each\n"
"                      with its own six-brick TFCE-based correction. TFCE\n"
"                      aggregates spatial support from neighboring voxels\n"
"                      instead of requiring a single voxel to beat the\n"
"                      maximum across the whole analysis volume, which is\n"
"                      what raw voxel-wise max-stat FWE correction demands\n"
"                      and which makes it extremely conservative at\n"
"                      whole-brain scale. CON_mean and CON_t (and their\n"
"                      GrpA/GrpB equivalents) stay the ordinary observed\n"
"                      values; only p_unc/p_fwe/z_unc/z_fwe are computed\n"
"                      from TFCE scores instead of the raw statistic.\n"
"  -tfce_H h            TFCE height exponent. Default: 2.0 (FSL's default).\n"
"  -tfce_E e            TFCE extent exponent. Default: 0.5 (FSL's default).\n"
"  -tfce_dh d           TFCE integration step size. Default: the observed\n"
"                      map's peak statistic / 100, matching FSL's usual\n"
"                      resolution. A smaller step increases run time.\n"
"  -tfce_conn 6|18|26   Voxel connectivity for cluster extent. Default: 18.\n"
"                      TFCE recomputes a full spatial cluster sweep for\n"
"                      every permutation, which costs meaningfully more than\n"
"                      the ordinary max-stat correction -- expect a run to\n"
"                      take noticeably longer with -tfce than without it.\n"
"\n"
"Masking:\n"
"  -mask MASK          Restrict analysis to nonzero voxels in MASK.\n"
"                      This is strongly recommended for group analyses.\n"
"  -automask           Use AFNI's automask procedure on the mean absolute\n"
"                      value across every input dataset. Cannot be combined\n"
"                      with -mask.\n"
"                      WARNING: Statistical/effect maps generally do not\n"
"                      have the intensity structure expected by automask,\n"
"                      so the resulting coverage can be unreliable. Prefer\n"
"                      a carefully constructed group mask with -mask.\n"
"  Masked voxels and voxels containing NaN/Inf in an automask analysis\n"
"  have statistic bricks set to 0 and p-value bricks set to 1.\n"
"\n"
"Output:\n"
"  Paired and one-sample tests produce 6 sub-bricks per test:\n"
"    CON_mean          observed mean of A-B, or condition mean for onesample\n"
"    CON_t             observed t-statistic. Tagged FITT when its degrees\n"
"                      of freedom are fixed, for parametric reference only;\n"
"                      do not use this brick for permutation inference.\n"
"                      Welch contrast t bricks are not stat-coded because\n"
"                      their degrees of freedom vary by voxel.\n"
"    CON_p_unc         voxelwise empirical p-value (uncorrected)\n"
"    CON_p_fwe         max-stat FWE-corrected empirical p-value\n"
"    CON_z_unc         signed z equivalent of CON_p_unc, tagged FIZT so\n"
"                      the AFNI GUI threshold slider shows a correct\n"
"                      p-value readout. Uncorrected -- exploratory only.\n"
"    CON_z_fwe         signed z equivalent of CON_p_fwe, tagged FIZT.\n"
"                      *** THRESHOLD ON THIS BRICK FOR REPORTED RESULTS ***\n"
"                      It is whole-brain FWE-corrected already; no\n"
"                      further cluster correction is required.\n"
"\n"
"  Each two-sample contrast produces 18 sub-bricks, in this order:\n"
"    GrpA_mean GrpA_t GrpA_p_unc GrpA_p_fwe GrpA_z_unc GrpA_z_fwe\n"
"    GrpB_mean GrpB_t GrpB_p_unc GrpB_p_fwe GrpB_z_unc GrpB_z_fwe\n"
"    CON_mean  CON_t  CON_p_unc  CON_p_fwe  CON_z_unc  CON_z_fwe\n"
"  GrpA and GrpB are one-sample sign-flip tests against zero. CON is the\n"
"  shuffled two-sample test of A-B. Labels use the supplied condition and\n"
"  contrast names. Each six-brick family has its own max-stat correction.\n"
"\n"
"IMPORTANT resolution ceiling:\n"
"  With exact sign-flip enumeration, the smallest achievable p-value is\n"
"  2/2^Nsubj for -tails two and 1/2^Nsubj for -tails one.\n"
"  Exact two-sample contrast resolution is likewise limited by the number\n"
"  of fixed-size assignments: choose(NA+NB,NA).\n"
"\n"
"  With -mode random the floor is 1/(niter+1) instead, so the default\n"
"  -niter %d puts it at 1e-04 no matter how many subjects there are.\n"
"\n"
"Choosing exact versus random:\n"
"  Exact enumeration stores one number per relabeling in an int-indexed\n"
"  array, so the count must fit in 2147483647. That caps sign-flip tests at\n"
"  %d subjects, and a balanced two-sample contrast near 33 datasets in\n"
"  total. Run time doubles with every subject added as well, so exact mode\n"
"  stops being practical long before the cap, at roughly N=18.\n"
"\n"
"  Very little is lost by that. Exact mode earns its cost at SMALL N, where\n"
"  so few relabelings exist that the floor above genuinely constrains what\n"
"  can be reported: at N=6 that floor is 0.031, at N=10 it is 0.002. By\n"
"  N=15 there are 32768 sign patterns and a floor of 6e-05, already finer\n"
"  than -niter %d delivers, and both sit far below any threshold anyone\n"
"  applies. Past that point exact enumeration buys resolution that will\n"
"  never be used, at exponentially growing cost.\n"
"\n"
"  Random sampling is statistically sound at every N, so use it whenever\n"
"  exact mode is slow. 3dShuffle warns when an exact run is large enough\n"
"  that random is the better choice, and falls back to random on its own\n"
"  when no -mode was given and the enumeration cannot be represented.\n"
"\n"
"  The reverse also happens: if -niter is at least the total number of\n"
"  relabelings, the run switches to exact. Drawing 10000 samples from a\n"
"  group of 256 revisits some and misses others, so it costs 39 times as\n"
"  much as enumerating them and returns a noisier p-value.\n"
"\n"
   , DEFAULT_NITER, DEFAULT_NITER, EXACT_MAX_SIGNFLIP_N, DEFAULT_NITER);
   PRINT_AFNI_OMP_USAGE(PROGRAM_NAME,NULL);
   PRINT_COMPILE_DATE;
   exit(0);
}

/* Return whether a string looks like a command-line option. */
static int is_opt(const char *s)
{
   return s != NULL && s[0] == '-';
}

/* Allocate and return a copy of a string. */
static char *copy_string(const char *s)
{
   char *out = NULL;
   if( s == NULL ) return NULL;
   out = (char *)malloc(strlen(s)+1);
   if( out == NULL ) ERROR_exit("malloc failure");
   strcpy(out,s);
   return out;
}

/* Initialize option fields to their default values. */
static void init_opts(opts_t *opts)
{
   memset(opts,0,sizeof(opts_t));
   opts->tails = TAIL_TWO;
   opts->mode = MODE_AUTO;
   opts->stat = STAT_PAIRED;
   opts->method = METHOD_SIGNFLIP;
   opts->seed = 1234567L;
   opts->tfce_H = 2.0f;
   opts->tfce_E = 0.5f;
   opts->tfce_dh = 0.0f;
   opts->tfce_conn = 18;
}

/* Build a sanitized output label for a contrast name. */
static char *safe_contrast_name(const char *a, const char *b)
{
   int ii, jj = 0, n = strlen(a) + strlen(b) + 5;
   char *out = (char *)calloc(n,sizeof(char));
   if( out == NULL ) ERROR_exit("malloc failure");
   for( ii=0 ; a[ii] != '\0' ; ii++ )
      out[jj++] = (isalnum((unsigned char)a[ii])) ? a[ii] : '_';
   out[jj++] = '_'; out[jj++] = 'v'; out[jj++] = 's'; out[jj++] = '_';
   for( ii=0 ; b[ii] != '\0' ; ii++ )
      out[jj++] = (isalnum((unsigned char)b[ii])) ? b[ii] : '_';
   out[jj] = '\0';
   return out;
}

/* Build a sanitized output label from one condition name. */
static char *safe_label_name(const char *a)
{
   int ii, jj = 0, n = strlen(a) + 1;
   char *out = (char *)calloc(n,sizeof(char));
   if( out == NULL ) ERROR_exit("malloc failure");
   for( ii=0 ; a[ii] != '\0' ; ii++ )
      out[jj++] = (isalnum((unsigned char)a[ii])) ? a[ii] : '_';
   out[jj] = '\0';
   return out;
}

/* Parse and validate a positive integer option argument. */
static int parse_int_arg(const char *s, const char *opt)
{
   char *end = NULL;
   long val = strtol(s,&end,10);
   if( end == s || *end != '\0' || val <= 0 || val > INT_MAX )
      ERROR_exit("bad integer after %s: %s",opt,s);
   return (int)val;
}

/* Append one filename to a growable list, expanding it first if it
   contains a wildcard. Mirrors the globbing done by 3dttest++'s short
   form, since the shell alone cannot be trusted to leave AFNI sub-brick
   selectors like '[...]' intact. */
static void append_expanded_name(char ***list, int *nds, int *cap, const char *arg)
{
   if( HAS_WILDCARD(arg) ){
      int nexp, iex;
      char **fexp = NULL;
      char *fin = (char *)arg;
      MCW_file_expand(1,&fin,&nexp,&fexp);
      if( nexp <= 0 ) ERROR_exit("-input: wildcard '%s' matched no files", arg);
      for( iex=0 ; iex < nexp ; iex++ ){
         if( *nds >= *cap ){
            *cap = (*cap)*2 + 8;
            *list = (char **)realloc(*list,sizeof(char *)*(*cap));
            if( *list == NULL ) ERROR_exit("malloc failure");
         }
         (*list)[(*nds)++] = copy_string(fexp[iex]);
      }
      MCW_free_expand(nexp,fexp);
   } else {
      if( *nds >= *cap ){
         *cap = (*cap)*2 + 8;
         *list = (char **)realloc(*list,sizeof(char *)*(*cap));
         if( *list == NULL ) ERROR_exit("malloc failure");
      }
      (*list)[(*nds)++] = copy_string(arg);
   }
}

/* Convert a condition label or 1-based index string to a 0-based index. */
static int label_to_index(opts_t *opts, const char *lab)
{
   int ii;
   char *end = NULL;
   long val = strtol(lab,&end,10);
   if( end != lab && *end == '\0' ){
      if( val < 1 || val > opts->ncond )
         ERROR_exit("condition index '%s' is outside 1..%d", lab, opts->ncond);
      return (int)val - 1;
   }
   for( ii=0 ; ii < opts->ncond ; ii++ )
      if( strcmp(lab,opts->cond_labels[ii]) == 0 ) return ii;
   ERROR_exit("unknown condition label/index '%s'",lab);
   return -1;
}

/* Parse command-line options and finalize derived option state. */
static void parse_opts(int argc, char **argv, opts_t *opts)
{
   int nopt = 1, ii;

   if( argc < 2 || strcmp(argv[1],"-help") == 0 ) shuffle_help();

   init_opts(opts);

   while( nopt < argc ){
      if( strcmp(argv[nopt],"-conditions") == 0 ){
         if( ++nopt >= argc ) ERROR_exit("need an argument after -conditions");
         opts->ncond = parse_int_arg(argv[nopt],"-conditions");
         opts->cond_labels = (char **)calloc(opts->ncond,sizeof(char *));
         opts->input_names = (char ***)calloc(opts->ncond,sizeof(char **));
         opts->nsubj_by_cond = (int *)calloc(opts->ncond,sizeof(int));
         opts->offsets = (int *)calloc(opts->ncond,sizeof(int));
         if( opts->cond_labels == NULL || opts->input_names == NULL ||
             opts->nsubj_by_cond == NULL || opts->offsets == NULL )
            ERROR_exit("malloc failure");
         nopt++; continue;
      }

      if( strcmp(argv[nopt],"-cond_labels") == 0 ){
         if( opts->ncond <= 0 ) ERROR_exit("-conditions must precede -cond_labels");
         for( ii=0 ; ii < opts->ncond ; ii++ ){
            if( ++nopt >= argc || is_opt(argv[nopt]) )
               ERROR_exit("need %d labels after -cond_labels", opts->ncond);
            opts->cond_labels[ii] = copy_string(argv[nopt]);
         }
         nopt++; continue;
      }

      if( strcmp(argv[nopt],"-input") == 0 ){
         int nds = 0, cap = 0, ic = opts->ninput;
         char **list = NULL;
         if( opts->ncond <= 0 ) ERROR_exit("-conditions must precede -input");
         if( ic >= opts->ncond ) ERROR_exit("too many -input lists for -conditions %d", opts->ncond);
         nopt++;
         while( nopt < argc && !is_opt(argv[nopt]) ){
            append_expanded_name(&list,&nds,&cap,argv[nopt]);
            nopt++;
         }
         if( nds <= 0 ) ERROR_exit("need datasets after -input");
         opts->input_names[ic] = list;
         /* Defer count comparisons until -stat is known: independent
            groups may legitimately have different sample sizes. */
         opts->nsubj_by_cond[ic] = nds;
         opts->ninput++;
         continue;
      }

      if( strcmp(argv[nopt],"-brickwise") == 0 ){
         opts->brickwise = 1;
         nopt++; continue;
      }

      if( strcmp(argv[nopt],"-contrast") == 0 ){
         if( nopt+2 >= argc ) ERROR_exit("need 2 arguments after -contrast");
         opts->cons = (contrast_t *)realloc(opts->cons,sizeof(contrast_t)*(opts->ncon+1));
         if( opts->cons == NULL ) ERROR_exit("malloc failure");
         opts->cons[opts->ncon].a = copy_string(argv[nopt+1]);
         opts->cons[opts->ncon].b = copy_string(argv[nopt+2]);
         opts->cons[opts->ncon].ia = -1;
         opts->cons[opts->ncon].ib = -1;
         opts->cons[opts->ncon].name = NULL;
         opts->ncon++;
         nopt += 3; continue;
      }

      if( strcmp(argv[nopt],"-method") == 0 ){
         if( ++nopt >= argc ) ERROR_exit("need an argument after -method");
         if( strcmp(argv[nopt],"signflip") == 0 ) opts->method = METHOD_SIGNFLIP;
         else if( strcmp(argv[nopt],"shuffle") == 0 ) opts->method = METHOD_SHUFFLE;
         else ERROR_exit("-method must be one of: signflip shuffle");
         nopt++; continue;
      }

      if( strcmp(argv[nopt],"-stat") == 0 ){
         if( ++nopt >= argc ) ERROR_exit("need an argument after -stat");
         if( strcmp(argv[nopt],"paired_ttest") == 0 ) opts->stat = STAT_PAIRED;
         else if( strcmp(argv[nopt],"onesample") == 0 ) opts->stat = STAT_ONESAMPLE;
         else if( strcmp(argv[nopt],"twosample") == 0 ) opts->stat = STAT_TWOSAMPLE;
         else ERROR_exit("-stat must be one of: paired_ttest onesample twosample");
         nopt++; continue;
      }

      if( strcmp(argv[nopt],"-unpooled") == 0 ){
         opts->unpooled = 1;
         nopt++; continue;
      }

      if( strcmp(argv[nopt],"-tfce") == 0 ){
         opts->tfce = 1;
         nopt++; continue;
      }

      if( strcmp(argv[nopt],"-tfce_H") == 0 ){
         char *end = NULL;
         if( ++nopt >= argc ) ERROR_exit("need an argument after -tfce_H");
         opts->tfce_H = (float)strtod(argv[nopt],&end);
         if( end == argv[nopt] || opts->tfce_H <= 0.0f )
            ERROR_exit("bad value after -tfce_H: %s",argv[nopt]);
         nopt++; continue;
      }

      if( strcmp(argv[nopt],"-tfce_E") == 0 ){
         char *end = NULL;
         if( ++nopt >= argc ) ERROR_exit("need an argument after -tfce_E");
         opts->tfce_E = (float)strtod(argv[nopt],&end);
         if( end == argv[nopt] || opts->tfce_E <= 0.0f )
            ERROR_exit("bad value after -tfce_E: %s",argv[nopt]);
         nopt++; continue;
      }

      if( strcmp(argv[nopt],"-tfce_dh") == 0 ){
         char *end = NULL;
         if( ++nopt >= argc ) ERROR_exit("need an argument after -tfce_dh");
         opts->tfce_dh = (float)strtod(argv[nopt],&end);
         if( end == argv[nopt] || opts->tfce_dh <= 0.0f )
            ERROR_exit("bad value after -tfce_dh: %s",argv[nopt]);
         nopt++; continue;
      }

      if( strcmp(argv[nopt],"-tfce_conn") == 0 ){
         if( ++nopt >= argc ) ERROR_exit("need an argument after -tfce_conn");
         opts->tfce_conn = parse_int_arg(argv[nopt],"-tfce_conn");
         if( opts->tfce_conn != 6 && opts->tfce_conn != 18 && opts->tfce_conn != 26 )
            ERROR_exit("-tfce_conn must be 6, 18, or 26");
         nopt++; continue;
      }

      if( strcmp(argv[nopt],"-tails") == 0 ){
         if( ++nopt >= argc ) ERROR_exit("need an argument after -tails");
         if( strcmp(argv[nopt],"two") == 0 ) opts->tails = TAIL_TWO;
         else if( strcmp(argv[nopt],"one") == 0 ) opts->tails = TAIL_ONE;
         else if( strcmp(argv[nopt],"upper") == 0 || strcmp(argv[nopt],"lower") == 0 )
            ERROR_exit("-tails upper/lower has been replaced by -tails one. "
                       "Use contrast order to set direction: -contrast A B "
                       "tests A>B with -tails one.");
         else ERROR_exit("-tails must be one of: two one");
         nopt++; continue;
      }

      if( strcmp(argv[nopt],"-mode") == 0 ){
         if( ++nopt >= argc ) ERROR_exit("need an argument after -mode");
         if( strcmp(argv[nopt],"exact") == 0 ) opts->mode = MODE_EXACT;
         else if( strcmp(argv[nopt],"random") == 0 ) opts->mode = MODE_RANDOM;
         else ERROR_exit("-mode must be exact or random");
         /* An explicit choice is honored strictly: an exact run that cannot
            be represented becomes an error rather than a silent fallback. */
         opts->mode_explicit = 1;
         nopt++; continue;
      }

      if( strcmp(argv[nopt],"-niter") == 0 ){
         if( ++nopt >= argc ) ERROR_exit("need an argument after -niter");
         opts->niter = parse_int_arg(argv[nopt],"-niter");
         opts->have_niter = 1;
         nopt++; continue;
      }

      if( strcmp(argv[nopt],"-seed") == 0 ){
         char *end = NULL;
         if( ++nopt >= argc ) ERROR_exit("need an argument after -seed");
         opts->seed = strtol(argv[nopt],&end,10);
         if( end == argv[nopt] || *end != '\0' ) ERROR_exit("bad integer after -seed");
         nopt++; continue;
      }

      if( strcmp(argv[nopt],"-mask") == 0 ){
         if( ++nopt >= argc ) ERROR_exit("need an argument after -mask");
         opts->mask_name = copy_string(argv[nopt]);
         nopt++; continue;
      }

      if( strcmp(argv[nopt],"-automask") == 0 ){
         opts->auto_mask = 1;
         nopt++; continue;
      }

      if( strcmp(argv[nopt],"-prefix") == 0 ){
         if( ++nopt >= argc ) ERROR_exit("need an argument after -prefix");
         opts->prefix = copy_string(argv[nopt]);
         nopt++; continue;
      }

      ERROR_message("unknown option %s", argv[nopt]);
      suggest_best_prog_option(argv[0], argv[nopt]);
      exit(1);
   }

   if( opts->ncond <= 0 ) ERROR_exit("need -conditions N, with N > 0");
   if( opts->stat != STAT_ONESAMPLE && opts->ncond <= 1 )
      ERROR_exit("need -conditions N, with N > 1 for a contrast test");
   if( opts->ninput != opts->ncond )
      ERROR_exit("need exactly %d -input lists, found %d",opts->ncond,opts->ninput);
   if( opts->stat != STAT_ONESAMPLE && opts->ncon <= 0 )
      ERROR_exit("need at least one -contrast A B for this statistic");
   if( opts->stat == STAT_ONESAMPLE && opts->ncon > 0 )
      ERROR_exit("-contrast is not used with -stat onesample");
   if( opts->stat == STAT_TWOSAMPLE && opts->method != METHOD_SHUFFLE )
      ERROR_exit("-stat twosample requires -method shuffle");
   if( opts->stat != STAT_TWOSAMPLE && opts->method != METHOD_SIGNFLIP )
      ERROR_exit("paired and one-sample tests require -method signflip");
   if( opts->unpooled && opts->stat != STAT_TWOSAMPLE )
      ERROR_exit("-unpooled is only valid with -stat twosample");
   if( opts->auto_mask && opts->mask_name != NULL )
      ERROR_exit("-automask and -mask cannot be used together");
   if( opts->prefix == NULL ) ERROR_exit("need -prefix");
   if( !opts->tfce && (opts->tfce_H != 2.0f || opts->tfce_E != 0.5f ||
                       opts->tfce_dh != 0.0f || opts->tfce_conn != 18) )
      WARNING_message("-tfce_H/-tfce_E/-tfce_dh/-tfce_conn have no effect "
                      "without -tfce");

   opts->ntotal = 0;
   for( ii=0 ; ii < opts->ncond ; ii++ ){
      if( opts->brickwise ){
         if( opts->nsubj_by_cond[ii] != 1 )
            ERROR_exit("-brickwise requires exactly one dataset after -input list %d; found %d",
                       ii+1,opts->nsubj_by_cond[ii]);
      } else {
         /* Every group needs variance, so a singleton input list cannot
            support any of the requested t-statistics. */
         if( opts->nsubj_by_cond[ii] < 2 )
            ERROR_exit("-input list %d has %d datasets; need at least 2",
                       ii+1,opts->nsubj_by_cond[ii]);
         opts->offsets[ii] = opts->ntotal;
         opts->ntotal += opts->nsubj_by_cond[ii];
      }
   }

   if( !opts->brickwise && opts->stat != STAT_TWOSAMPLE ){
      opts->nsubj = opts->nsubj_by_cond[0];
      for( ii=1 ; ii < opts->ncond ; ii++ ){
         /* Paired input order and the common sign-flip schedule both
            require a rectangular condition-by-subject input layout. */
         if( opts->nsubj_by_cond[ii] != opts->nsubj )
            ERROR_exit("-input list %d has %d datasets, but expected %d",
                       ii+1,opts->nsubj_by_cond[ii],opts->nsubj);
      }
   }

   for( ii=0 ; ii < opts->ncond ; ii++ ){
      char buf[32];
      if( opts->cond_labels[ii] == NULL ){
         sprintf(buf,"cond%d",ii+1);
         opts->cond_labels[ii] = copy_string(buf);
      }
   }
   if( opts->stat == STAT_ONESAMPLE ){
      int ntest = opts->ncond;
      size_t nalloc = (ntest > 0) ? (size_t)ntest : 1U;
      if( ntest <= 0 ) ERROR_exit("need at least one one-sample test");
      opts->ncon = ntest;
      opts->cons = (contrast_t *)calloc(nalloc,sizeof(contrast_t));
      if( opts->cons == NULL ) ERROR_exit("malloc failure");
      for( ii=0 ; ii < opts->ncon ; ii++ ){
         opts->cons[ii].ia = ii;
         opts->cons[ii].ib = -1;
         opts->cons[ii].a = copy_string(opts->cond_labels[ii]);
         opts->cons[ii].b = copy_string("0");
         opts->cons[ii].name = safe_label_name(opts->cond_labels[ii]);
      }
   } else {
      for( ii=0 ; ii < opts->ncon ; ii++ ){
         opts->cons[ii].ia = label_to_index(opts, opts->cons[ii].a);
         opts->cons[ii].ib = label_to_index(opts, opts->cons[ii].b);
         if( opts->cons[ii].ia == opts->cons[ii].ib )
            ERROR_exit("contrast %s %s uses the same condition twice",opts->cons[ii].a,opts->cons[ii].b);
         opts->cons[ii].name = safe_contrast_name(opts->cond_labels[opts->cons[ii].ia],
                                                  opts->cond_labels[opts->cons[ii].ib]);
      }
   }

   if( opts->mode == MODE_AUTO ){
      if( opts->have_niter ) opts->mode = MODE_RANDOM;
      else opts->mode = MODE_EXACT;
   }
   /* have_niter keeps meaning "the user asked for this count", so that
      print_sanity() can label a defaulted value as such. resolve_mode()
      supplies the same default if it has to fall back to random. */
   if( opts->mode == MODE_RANDOM && !opts->have_niter )
      opts->niter = DEFAULT_NITER;
   if( opts->mode == MODE_EXACT && opts->have_niter )
      WARNING_message("-niter %d is ignored with -mode exact, which enumerates "
                      "every relabeling", opts->niter);
   /* Whether an exact run is actually representable depends on permutation
      counts, so that check lives in resolve_mode(). */
}

/* In -brickwise mode, resolve the sample count from the one dataset named
   by each -input list. This has to happen before resolve_mode(), since the
   permutation counts depend on those Ns. */
static void finalize_brickwise_inputs(opts_t *opts)
{
   THD_3dim_dataset *first = NULL, *dset = NULL;
   int ic, nvals;

   if( !opts->brickwise ) return;

   opts->ntotal = 0;
   for( ic=0 ; ic < opts->ncond ; ic++ ){
      dset = THD_open_dataset(opts->input_names[ic][0]);
      CHECK_OPEN_ERROR(dset, opts->input_names[ic][0]);
      if( first == NULL ) first = dset;
      else if( !EQUIV_GRIDS(first,dset) )
         ERROR_exit("input %s is not on the same grid as %s",
                    opts->input_names[ic][0], opts->input_names[0][0]);

      nvals = DSET_NVALS(dset);
      if( nvals < 2 )
         ERROR_exit("-brickwise input %s has %d sub-brick%s; need at least 2",
                    opts->input_names[ic][0], nvals, nvals == 1 ? "" : "s");

      opts->offsets[ic] = opts->ntotal;
      opts->nsubj_by_cond[ic] = nvals;
      opts->ntotal += nvals;

      if( ic > 0 ) DSET_delete(dset);
   }

   if( opts->stat != STAT_TWOSAMPLE ){
      opts->nsubj = opts->nsubj_by_cond[0];
      for( ic=1 ; ic < opts->ncond ; ic++ ){
         if( opts->nsubj_by_cond[ic] != opts->nsubj )
            ERROR_exit("-brickwise input %s has %d sub-bricks, but expected %d",
                       opts->input_names[ic][0],
                       opts->nsubj_by_cond[ic],opts->nsubj);
      }
   }

   if( first != NULL ) DSET_delete(first);
}

/* Count exact fixed-size group assignments, stopping once int storage is exceeded. */
static long long combination_count(int nn, int kk)
{
   int ii;
   long long out = 1;
   if( kk < 0 || kk > nn ) return 0;
   if( kk > nn-kk ) kk = nn-kk;
   for( ii=1 ; ii <= kk ; ii++ ){
      long long factor = nn-kk+ii;
      /* Exact runs store one max statistic per assignment, so values
         beyond INT_MAX cannot be represented by this implementation. */
      if( out > LLONG_MAX/factor ) return (long long)INT_MAX+1LL;
      out = (out*factor)/ii;
      if( out > INT_MAX ) return (long long)INT_MAX+1LL;
   }
   return out;
}

/* Resolve the number of permutations for one statistical test. */
static long long permutation_count(opts_t *opts, stat_code stat, int na, int nb)
{
   if( opts->mode == MODE_RANDOM ) return opts->niter;
   if( stat == STAT_TWOSAMPLE ) return combination_count(na+nb,na);
   if( na >= (int)(8*sizeof(unsigned long)-1) ) return (long long)INT_MAX+1LL;
   return 1LL << na;
}

/* Find the largest exact relabeling count this run would need, along with the
   test that needs it and its observation count. Returns a value above INT_MAX
   if any single test overflows. */
static long long worst_exact_count(opts_t *opts, const char **which, int *nobs)
{
   long long worst = 0;
   int cc;

   for( cc=0 ; cc < opts->ncon ; cc++ ){
      int ia = opts->cons[cc].ia;
      int na = opts->nsubj_by_cond[ia];
      int nb = opts->stat == STAT_ONESAMPLE ? 0 :
               opts->nsubj_by_cond[opts->cons[cc].ib];
      long long np = permutation_count(opts,opts->stat,na,nb);

      if( np > worst ){
         worst = np; *which = opts->cons[cc].name; *nobs = na+nb;
      }
      if( opts->stat == STAT_TWOSAMPLE ){
         /* Two-sample output also contains a one-sample test per group, so
            those sign-flip spaces have to be feasible as well. */
         long long npa = permutation_count(opts,STAT_ONESAMPLE,na,0);
         long long npb = permutation_count(opts,STAT_ONESAMPLE,nb,0);
         if( npa > worst ){
            worst = npa; *which = opts->cond_labels[ia]; *nobs = na;
         }
         if( npb > worst ){
            worst = npb; *which = opts->cond_labels[opts->cons[cc].ib]; *nobs = nb;
         }
      }
   }
   return worst;
}

/* Decide whether an exact run is actually feasible, and say something useful
   when it is not.

   There are two distinct thresholds here. The hard one is representational:
   relabelings are counted and indexed with int, so a count above INT_MAX
   cannot be enumerated at all. The soft one is practical: an exact run whose
   count is in the millions will take enormously longer than random sampling
   while resolving p-values far past anything anyone reports. Exact mode is
   worth its cost at small N, where so few relabelings exist that the discrete
   p-value floor genuinely constrains the result.

   Mode changes here only ever go one way on purpose. Upgrading random to
   exact yields a strictly better answer for less work, so it happens even
   against an explicit -mode random. Downgrading exact to random yields a
   different, approximate answer, so it happens only when no -mode was
   given. */
static void resolve_mode(opts_t *opts)
{
   const char *which = "this design";
   int nobs = 0;
   mode_code want = opts->mode;
   long long worst;

   /* permutation_count() reports niter once the mode is random, so ask it
      about the enumeration with the mode temporarily set. */
   opts->mode = MODE_EXACT;
   worst = worst_exact_count(opts,&which,&nobs);
   opts->mode = want;

   if( want == MODE_RANDOM ){
      /* Asking for at least as many draws as the group holds is a request for
         the exact answer however it was phrased. Sampling with replacement
         from a group that small revisits some relabelings and misses others,
         so it costs more than enumeration and gives a noisier p-value. */
      if( worst > 0 && worst <= INT_MAX && (long long)opts->niter >= worst ){
         WARNING_message(
            "-niter %d is at least the %lld relabelings that %s (N=%d) has in "
            "total, so this run switches to -mode exact.\n"
            "   Enumerating the whole group costs less than sampling it with "
            "replacement and carries no sampling error.",
            opts->niter, worst, which, nobs);
         opts->mode = MODE_EXACT;
      }
      return;
   }

   if( worst <= 0 || worst > INT_MAX ){
      if( opts->mode_explicit )
         ERROR_exit(
            "-mode exact needs more than %d relabelings for %s (N=%d), which is "
            "more than this program can index.\n"
            "   Exact enumeration tops out at %d subjects for sign-flip tests, "
            "and near 33 datasets in total for a balanced two-sample contrast. "
            "It stops being practical well before that, around N=18.\n"
            "   Use:  -mode random -niter %d",
            INT_MAX, which, nobs, EXACT_MAX_SIGNFLIP_N, DEFAULT_NITER);

      /* No -mode was given, so choosing one is this program's job. */
      WARNING_message(
         "exact enumeration for %s (N=%d) needs more than %d relabelings and "
         "cannot be represented, so this run switches to -mode random -niter %d.\n"
         "   Random sampling is statistically sound at every N. Pass -mode exact "
         "if you would rather this were an error.",
         which, nobs, INT_MAX, DEFAULT_NITER);
      opts->mode = MODE_RANDOM;
      opts->niter = DEFAULT_NITER;
      return;
   }

   if( worst > EXACT_WARN_COUNT )
      WARNING_message(
         "exact enumeration needs %lld relabelings for %s (N=%d).\n"
         "   That is roughly %lld times the work of -mode random -niter %d, and "
         "it buys nothing you can use: both resolve p-values far below any "
         "threshold you would report.\n"
         "   Exact mode earns its cost at SMALL N, where so few relabelings "
         "exist that the p-value floor is coarse. Consider -mode random "
         "-niter %d.",
         worst, which, nobs, worst/(long long)DEFAULT_NITER,
         DEFAULT_NITER, DEFAULT_NITER);
}

/* Report the resolved design, checks, and outputs before running permutations. */
static void print_sanity(opts_t *opts)
{
   int is, ic, cc;
   const char *stat_name =
      opts->stat == STAT_PAIRED ? "paired_ttest" :
      opts->stat == STAT_ONESAMPLE ? "onesample" : "twosample";

   INFO_message("Design:       %s",
                opts->stat == STAT_PAIRED ? "paired/repeated-measures" :
                opts->stat == STAT_ONESAMPLE ? "one-sample" :
                                               "independent groups");
   INFO_message("Conditions:   %d", opts->ncond);
   INFO_message("Method:       %s",
                opts->method == METHOD_SIGNFLIP ? "signflip" : "shuffle");
   INFO_message("Statistic:    %s", stat_name);
   if( opts->stat == STAT_TWOSAMPLE )
      INFO_message("Variance:     %s",
                   opts->unpooled ? "unequal (Welch; -unpooled)" :
                                    "equal (pooled; default)");
   else
      INFO_message("Subjects:     %d per input list", opts->nsubj);
   INFO_message("Tails:        %s", opts->tails == TAIL_TWO ? "two" : "one");
   if( opts->tails == TAIL_ONE )
      INFO_message("One-tailed direction: %s",
                   opts->stat == STAT_ONESAMPLE ? "positive condition mean" :
                                                  "positive for each contrast A-B");
   INFO_message("Mode:         %s", opts->mode == MODE_EXACT ? "exact" : "random");
   if( opts->mode == MODE_RANDOM ){
      INFO_message("Iterations:   %d%s", opts->niter,
                   opts->have_niter ? "" : " (default; set with -niter)");
      INFO_message("Seed:         %ld", opts->seed);
   }
   INFO_message("Grid check:   every input and mask must match the first input");
   INFO_message("Input mode:   %s",
                opts->brickwise ? "brickwise (-input has one multi-brick dataset)"
                                : "dataset lists");
   INFO_message("Mask mode:    %s",
                opts->auto_mask ? "AFNI automask of mean-absolute inputs" :
                opts->mask_name != NULL ? opts->mask_name : "none");

   if( opts->stat == STAT_TWOSAMPLE ){
      INFO_message("Independent input groups:");
      for( ic=0 ; ic < opts->ncond ; ic++ ){
         fprintf(stderr,"++   %s: N=%d\n",
                 opts->cond_labels[ic],opts->nsubj_by_cond[ic]);
         if( opts->brickwise ){
            fprintf(stderr,"++      %s[0..%d]\n",opts->input_names[ic][0],
                    opts->nsubj_by_cond[ic]-1);
         } else {
            for( is=0 ; is < opts->nsubj_by_cond[ic] ; is++ )
               fprintf(stderr,"++      %s\n",opts->input_names[ic][is]);
         }
      }
   } else {
      INFO_message("Input pairing/order:");
      for( is=0 ; is < opts->nsubj ; is++ ){
         fprintf(stderr,"++   subj%d:", is+1);
         for( ic=0 ; ic < opts->ncond ; ic++ ){
            if( opts->brickwise )
               fprintf(stderr," %s=%s[%d]", opts->cond_labels[ic],
                       opts->input_names[ic][0], is);
            else
               fprintf(stderr," %s=%s", opts->cond_labels[ic],
                       opts->input_names[ic][is]);
         }
         fprintf(stderr,"\n");
      }
   }

   INFO_message("%s:", opts->stat == STAT_ONESAMPLE ? "One-sample tests" : "Contrasts");
   for( cc=0 ; cc < opts->ncon ; cc++ ){
      if( opts->stat != STAT_ONESAMPLE ){
         int na = opts->nsubj_by_cond[opts->cons[cc].ia];
         int nb = opts->nsubj_by_cond[opts->cons[cc].ib];
         long long np = permutation_count(opts,opts->stat,na,nb);
         fprintf(stderr,"++   %s = %s - %s\n", opts->cons[cc].name,
                 opts->cond_labels[opts->cons[cc].ia],
                 opts->cond_labels[opts->cons[cc].ib]);
         fprintf(stderr,"++      N=%d versus N=%d; contrast permutations=%lld\n",
                 na,nb,np);
         if( opts->stat == STAT_TWOSAMPLE ){
            long long npa = permutation_count(opts,STAT_ONESAMPLE,na,0);
            long long npb = permutation_count(opts,STAT_ONESAMPLE,nb,0);
            fprintf(stderr,"++      group sign-flips: %s=%lld, %s=%lld\n",
                    opts->cond_labels[opts->cons[cc].ia],npa,
                    opts->cond_labels[opts->cons[cc].ib],npb);
            fprintf(stderr,"++      output order: %s[6], %s[6], %s[6]\n",
                    opts->cond_labels[opts->cons[cc].ia],
                    opts->cond_labels[opts->cons[cc].ib],
                    opts->cons[cc].name);
         }
      } else {
         long long np = permutation_count(opts,STAT_ONESAMPLE,
                                          opts->nsubj_by_cond[opts->cons[cc].ia],0);
         fprintf(stderr,"++   %s = %s vs 0\n", opts->cons[cc].name,
                 opts->cond_labels[opts->cons[cc].ia]);
         fprintf(stderr,"++      N=%d; sign-flip permutations=%lld\n",
                 opts->nsubj_by_cond[opts->cons[cc].ia],np);
      }
   }
}

/* Compute a one-sample t-statistic from values, optionally sign-flipped. */
static float one_sample_t(float *values, int nsubj, byte *flip, float *mean_out)
{
   int ii;
   double sum = 0.0, ss = 0.0, mean, var;
   for( ii=0 ; ii < nsubj ; ii++ ){
      double sgn = (flip != NULL && flip[ii]) ? -1.0 : 1.0;
      sum += sgn * values[ii];
   }
   mean = sum / nsubj;
   for( ii=0 ; ii < nsubj ; ii++ ){
      double sgn = (flip != NULL && flip[ii]) ? -1.0 : 1.0;
      double dd = sgn * values[ii] - mean;
      ss += dd * dd;
   }
   if( mean_out != NULL ) *mean_out = (float)mean;
   if( nsubj < 2 ) return 0.0f;
   var = ss / (nsubj-1);
   if( var <= 0.0 ) return 0.0f;
   return (float)(mean / sqrt(var/nsubj));
}

/* Convert a t-statistic to the comparison value for the requested tail mode. */
static float tail_value(float tt, tail_code tails)
{
   if( tails == TAIL_TWO ) return fabsf(tt);
   return tt;
}

/* Compare two floats for qsort in ascending order. */
static int cmp_float(const void *a, const void *b)
{
   float aa = *((const float *)a), bb = *((const float *)b);
   return (aa > bb) - (aa < bb);
}

/* Print an in-place percentage progress bar on stderr.

   Only redraws when
   the integer percentage changes, so it doesn't spam output for fast
   loops. Pass a pointer to an int initialized to -1 before the loop
   starts (one such tracker per loop/contrast). */
static void print_progress_bar(int current, int total, int *last_pct)
{
   int pct, filled, ii;
   char bar[41];

   if( total <= 0 ) return;
   pct = (int)(100.0 * (current + 1) / (double)total);
   if( pct == *last_pct ) return;
   *last_pct = pct;

   filled = pct * 40 / 100;
   for( ii=0 ; ii < 40 ; ii++ ) bar[ii] = (ii < filled) ? '#' : '-';
   bar[40] = '\0';

   fprintf(stderr, "\r++ permuting [%s] %3d%% (%d/%d)", bar, pct, current+1, total);
   if( pct >= 100 ) fprintf(stderr, "\n");
   fflush(stderr);
}

/* Compute an empirical upper-tail p-value from a sorted null distribution. */
static float emp_p_from_sorted(float *sorted, int nperm, float obs, int exact)
{
   int lo = 0, hi = nperm;
   while( lo < hi ){
      int mid = (lo + hi) / 2;
      if( sorted[mid] >= obs ) hi = mid;
      else lo = mid + 1;
   }
   if( exact ) return (float)(nperm - lo) / (float)nperm;
   return (float)(nperm - lo + 1) / (float)(nperm + 1);
}

/* ---------------------------------------------------------------------
   Convert an empirical p-value into a signed z-score, so
   that AFNI's GUI can be handed a FIZT-tagged brick and display the
   correct p-value on the interactive threshold slider. The sign of
   the observed statistic is preserved so overlay direction (positive
   vs. negative effect) is still visually meaningful.

   qginv(q) returns z such that P(Z > z) = q (upper-tail Gaussian
   inverse) -- the same routine AFNI's own -toz option relies on.
   p is floored away from 0/1 to avoid +-infinity at the extremes.
--------------------------------------------------------------------- */
/* Convert an empirical p-value to a signed z-score for output bricks.

   The clamp away from 0/1 has to be tied to nperm, the number of
   permutations actually drawn. A fixed constant far below 1/nperm (e.g.
   1e-15) is not a real achievable p-value for this test: clamping p=1.0
   (the least significant result the test can report) against it produces
   a huge-magnitude z that looks like a strong result on a |z| threshold,
   when it actually means "did not beat a single permutation." */
static float p_to_signed_z(float pval, float observed_stat, tail_code tails, int nperm)
{
   double p, z;
   double pmin = (nperm > 0) ? 1.0/(2.0*(double)nperm) : 1.0e-15;

   p = (double)pval;
   if( p < pmin )        p = pmin;
   if( p > 1.0 - pmin )  p = 1.0 - pmin;

   if( tails == TAIL_TWO ){
      z = qginv(p / 2.0);
      if( observed_stat < 0.0f ) z = -z;
   } else {
      z = qginv(p);
   }

   return (float)z;
}

/* Advance a sorted k-element combination drawn from 0..n-1. */
static int next_combination(int *comb, int kk, int nn)
{
   int ii = kk-1, jj;
   while( ii >= 0 && comb[ii] == nn-kk+ii ) ii--;
   if( ii < 0 ) return 0;
   comb[ii]++;
   for( jj=ii+1 ; jj < kk ; jj++ ) comb[jj] = comb[jj-1]+1;
   return 1;
}

/* Mark the observations assigned to group A by an exact combination. */
static void membership_from_combination(byte *in_a, int ntot, int *comb, int na)
{
   int ii;
   memset(in_a,0,(size_t)ntot*sizeof(byte));
   for( ii=0 ; ii < na ; ii++ ) in_a[comb[ii]] = 1;
}

/* Draw a random fixed-size group-A assignment using a partial Fisher-Yates
   shuffle. xran is a per-caller erand48() stream, so this is safe to call
   from multiple OpenMP threads concurrently with distinct xran arrays. */
static void random_membership(byte *in_a, int *order, int ntot, int na,
                              unsigned short xran[3])
{
   int ii;
   memset(in_a,0,(size_t)ntot*sizeof(byte));
   for( ii=0 ; ii < ntot ; ii++ ) order[ii] = ii;
   for( ii=0 ; ii < na ; ii++ ){
      int jj = ii + (int)(erand48(xran)*(ntot-ii));
      int tmp = order[ii];
      order[ii] = order[jj];
      order[jj] = tmp;
      in_a[order[ii]] = 1;
   }
}

/* Compute pooled or Welch independent-group t from a membership assignment. */
static float two_sample_t(float **combined, int ntot, int na, byte *in_a,
                          int iv, int unpooled, float *mean_diff)
{
   int ii, nb = ntot-na;
   double suma = 0.0, sumb = 0.0, ssa = 0.0, ssb = 0.0;
   double ma, mb, va, vb, denom;

   for( ii=0 ; ii < ntot ; ii++ ){
      if( in_a[ii] ) suma += combined[ii][iv];
      else           sumb += combined[ii][iv];
   }
   ma = suma/na;
   mb = sumb/nb;
   for( ii=0 ; ii < ntot ; ii++ ){
      double dd = combined[ii][iv] - (in_a[ii] ? ma : mb);
      if( in_a[ii] ) ssa += dd*dd;
      else           ssb += dd*dd;
   }
   va = ssa/(na-1);
   vb = ssb/(nb-1);
   if( mean_diff != NULL ) *mean_diff = (float)(ma-mb);

   if( unpooled ){
      /* Welch's denominator allows each group to retain its own variance. */
      denom = sqrt(va/na + vb/nb);
   } else {
      /* The default follows 3dttest++: combine variance estimates when
         the equal-variance model has been requested. */
      double pooled = ((na-1)*va + (nb-1)*vb)/(ntot-2);
      denom = sqrt(pooled*(1.0/na + 1.0/nb));
   }
   if( denom <= 0.0 || !isfinite(denom) ) return 0.0f;
   return (float)((ma-mb)/denom);
}

/* Convert permutation exceedance counts into p and signed-z output bricks.
   obs_cmp holds the per-voxel value ranked against max_null: tail_value()
   of the raw statistic in the ordinary path, or the TFCE-enhanced score
   when -tfce is in effect. The raw statistic's sign (out->tstat) still
   drives the two-tailed z-score sign, regardless of which value was used
   for ranking. */
static void finish_permutation_output(test_output_t *out, byte *mask, int nvox,
                                      int *unc_count, float *max_null, int nperm,
                                      float *obs_cmp, opts_t *opts)
{
   int iv;
   qsort(max_null,nperm,sizeof(float),cmp_float);
   for( iv=0 ; iv < nvox ; iv++ ){
      if( !mask[iv] ){
         out->p_unc[iv] = out->p_fwe[iv] = 1.0f;
         out->z_unc[iv] = out->z_fwe[iv] = 0.0f;
         continue;
      }
      if( opts->mode == MODE_EXACT )
         out->p_unc[iv] = (float)unc_count[iv]/(float)nperm;
      else
         out->p_unc[iv] = (float)(unc_count[iv]+1)/(float)(nperm+1);
      out->p_fwe[iv] = emp_p_from_sorted(
         max_null,nperm,obs_cmp[iv],opts->mode == MODE_EXACT);
      out->z_unc[iv] = p_to_signed_z(out->p_unc[iv],out->tstat[iv],opts->tails,nperm);
      out->z_fwe[iv] = p_to_signed_z(out->p_fwe[iv],out->tstat[iv],opts->tails,nperm);
   }
}

/* ---------------------------------------------------------------------
   TFCE (threshold-free cluster enhancement, Smith & Nichols 2009).
   Replaces each voxel's statistic with an integral over cluster extent
   at every threshold up to that voxel's own value:
      TFCE(v) = sum_{h=dh,2dh,...} extent(h,v)^E * h^H * dh
   where extent(h,v) is the size of the supra-threshold cluster
   containing v at threshold h (0 if v's own value is below h).

   Implemented as a fixed-height sweep from high to low, with an
   incrementally-maintained union-find tracking cluster membership and
   size as voxels activate. At every height step, every currently-active
   voxel's current cluster size is looked up and its contribution added.
   That makes this O(nsteps * navtive-per-step), worst case O(nsteps *
   nvox) -- adequate for the grid sizes and permutation counts this
   program targets, but the first thing to revisit if TFCE proves too
   slow on very large runs.
--------------------------------------------------------------------- */

/* Union-find root of ii, with path compression. */
static int tfce_find(int *parent, int ii)
{
   int root = ii, tmp;
   while( parent[root] != root ) root = parent[root];
   while( parent[ii] != root ){ tmp = parent[ii]; parent[ii] = root; ii = tmp; }
   return root;
}

/* Union by size; returns the new root. */
static int tfce_union(int *parent, int *size, int aa, int bb)
{
   int ra = tfce_find(parent,aa), rb = tfce_find(parent,bb);
   if( ra == rb ) return ra;
   if( size[ra] < size[rb] ){ int tmp = ra; ra = rb; rb = tmp; }
   parent[rb] = ra;
   size[ra] += size[rb];
   return ra;
}

/* Fill offs[][3] with the (dx,dy,dz) neighbor offsets for 6/18/26-voxel
   connectivity; returns the number of offsets filled. */
static int tfce_neighbor_offsets(int conn, int offs[][3])
{
   int dx, dy, dz, nfaces, n = 0;
   for( dz=-1 ; dz<=1 ; dz++ )
   for( dy=-1 ; dy<=1 ; dy++ )
   for( dx=-1 ; dx<=1 ; dx++ ){
      nfaces = (dx!=0)+(dy!=0)+(dz!=0);
      if( nfaces == 0 ) continue;
      if( conn == 6  && nfaces != 1 ) continue;
      if( conn == 18 && nfaces == 3 ) continue;
      offs[n][0] = dx; offs[n][1] = dy; offs[n][2] = dz; n++;
   }
   return n;
}

typedef struct { float val; int idx; } tfce_pair_t;

/* Sort descending by value, for qsort. */
static int cmp_tfce_pair(const void *a, const void *b)
{
   float aa = ((const tfce_pair_t *)a)->val, bb = ((const tfce_pair_t *)b)->val;
   return (aa < bb) - (aa > bb);
}

/* Enhance the positive side of stat[] into tfce_out[], which the caller
   must have zeroed first. Voxels with stat<=0, or outside mask, never
   activate and are left at 0. */
static void tfce_enhance_positive(float *stat, byte *mask, int nx, int ny, int nz,
                                  float H, float E, float dh, int conn,
                                  float *tfce_out)
{
   int nvox = nx*ny*nz;
   int offs[26][3];
   int noff, ii, jj, kk, np, nsteps, pos, nactive, iv, rr;
   int i0, j0, k0, ni, nj, nk, nb;
   float maxval, h, hH;
   tfce_pair_t *order;
   int *parent, *size, *active_list;
   byte *active;

   noff = tfce_neighbor_offsets(conn,offs);

   maxval = 0.0f;
   for( ii=0 ; ii < nvox ; ii++ )
      if( mask[ii] && stat[ii] > maxval ) maxval = stat[ii];
   if( maxval <= 0.0f ) return;

   if( dh <= 0.0f ) dh = maxval/100.0f;
   nsteps = (int)ceil(maxval/dh);
   if( nsteps < 1 ) nsteps = 1;

   order = (tfce_pair_t *)malloc(sizeof(tfce_pair_t)*nvox);
   parent = (int *)malloc(sizeof(int)*nvox);
   size = (int *)malloc(sizeof(int)*nvox);
   active = (byte *)calloc(nvox,sizeof(byte));
   active_list = (int *)malloc(sizeof(int)*nvox);
   if( order==NULL || parent==NULL || size==NULL || active==NULL || active_list==NULL )
      ERROR_exit("malloc failure in tfce_enhance_positive");

   np = 0;
   for( ii=0 ; ii < nvox ; ii++ ){
      parent[ii] = ii; size[ii] = 0;
      if( mask[ii] && stat[ii] > 0.0f ){ order[np].val = stat[ii]; order[np].idx = ii; np++; }
   }
   qsort(order,np,sizeof(tfce_pair_t),cmp_tfce_pair);

   pos = 0; nactive = 0;
   for( kk=nsteps ; kk>=1 ; kk-- ){
      h = kk*dh;
      while( pos < np && order[pos].val >= h ){
         iv = order[pos].idx;
         i0 = iv % nx; j0 = (iv/nx) % ny; k0 = iv/(nx*ny);
         active[iv] = 1; parent[iv] = iv; size[iv] = 1;
         active_list[nactive++] = iv;
         for( jj=0 ; jj < noff ; jj++ ){
            ni = i0+offs[jj][0]; nj = j0+offs[jj][1]; nk = k0+offs[jj][2];
            if( ni<0 || ni>=nx || nj<0 || nj>=ny || nk<0 || nk>=nz ) continue;
            nb = ni + nx*(nj + ny*nk);
            if( active[nb] ) tfce_union(parent,size,iv,nb);
         }
         pos++;
      }
      hH = powf(h,H);
      for( jj=0 ; jj < nactive ; jj++ ){
         iv = active_list[jj];
         rr = tfce_find(parent,iv);
         tfce_out[iv] += powf((float)size[rr],E) * hH * dh;
      }
   }

   free(active_list); free(active); free(size); free(parent); free(order);
}

/* Compute the TFCE-enhanced value used for permutation-null comparisons.
   For -tails one this is the positive-side score (0 for voxels with no
   positive evidence). For -tails two this is the larger of the positive-
   and negative-side scores, mirroring how tail_value() collapses a raw
   t-statistic to a magnitude for two-sided testing. neg_stat_buf,
   pos_buf, neg_buf are nvox-length scratch buffers owned by the caller;
   neg_stat_buf/neg_buf are unused (may be NULL) for -tails one. */
static void tfce_score(float *stat, byte *mask, int nx, int ny, int nz,
                       tail_code tails, float H, float E, float dh, int conn,
                       float *neg_stat_buf, float *pos_buf, float *neg_buf,
                       float *score_out)
{
   int nvox = nx*ny*nz, iv;
   memset(pos_buf,0,sizeof(float)*nvox);
   tfce_enhance_positive(stat,mask,nx,ny,nz,H,E,dh,conn,pos_buf);
   if( tails == TAIL_ONE ){
      memcpy(score_out,pos_buf,sizeof(float)*nvox);
      return;
   }
   for( iv=0 ; iv < nvox ; iv++ ) neg_stat_buf[iv] = -stat[iv];
   memset(neg_buf,0,sizeof(float)*nvox);
   tfce_enhance_positive(neg_stat_buf,mask,nx,ny,nz,H,E,dh,conn,neg_buf);
   for( iv=0 ; iv < nvox ; iv++ )
      score_out[iv] = (pos_buf[iv] >= neg_buf[iv]) ? pos_buf[iv] : neg_buf[iv];
}

/* Run a one-sample or paired-difference sign-flip test over all voxels. */
static void run_signflip_test(float **group_a, float **group_b, int nsubj,
                              byte *mask, int nvox, opts_t *opts,
                              test_output_t *out, const char *label)
{
   int is, iv, last_pct = -1, completed = 0;
   long long nperm_ll = permutation_count(opts,STAT_ONESAMPLE,nsubj,0);
   int nperm;
   int *unc_count;
   float *max_null, *values, *obs_cmp;

   if( nperm_ll <= 0 || nperm_ll > INT_MAX )
      ERROR_exit("sign-flip count for %s is too large (%lld); use -mode random -niter N",
                 label,nperm_ll);
   nperm = (int)nperm_ll;
   unc_count = (int *)calloc(nvox,sizeof(int));
   max_null = (float *)calloc(nperm,sizeof(float));
   values = (float *)calloc(nsubj,sizeof(float));
   obs_cmp = (float *)calloc(nvox,sizeof(float));
   if( unc_count == NULL || max_null == NULL || values == NULL || obs_cmp == NULL )
      ERROR_exit("malloc failure");

   INFO_message("Computing %s: %d sign-flip permutations%s",label,nperm,
               opts->tfce ? " (TFCE)" : "");
   for( iv=0 ; iv < nvox ; iv++ ){
      if( !mask[iv] ) continue;
      for( is=0 ; is < nsubj ; is++ )
         values[is] = group_a[is][iv] -
                      (group_b != NULL ? group_b[is][iv] : 0.0f);
      out->tstat[iv] = one_sample_t(values,nsubj,NULL,&out->mean[iv]);
   }
   free(values);

   if( opts->tfce ){
      /* tfce_score() needs the raw signed statistic, not tail_value()'s
         fabsf()-collapsed magnitude -- it does its own positive/negative
         sweep internally based on opts->tails. */
      float *curstat = (float *)calloc(nvox,sizeof(float));
      float *pos_buf = (float *)calloc(nvox,sizeof(float));
      float *neg_buf = NULL, *neg_stat = NULL;
      if( curstat == NULL || pos_buf == NULL ) ERROR_exit("malloc failure");
      if( opts->tails == TAIL_TWO ){
         neg_buf = (float *)calloc(nvox,sizeof(float));
         neg_stat = (float *)calloc(nvox,sizeof(float));
         if( neg_buf == NULL || neg_stat == NULL ) ERROR_exit("malloc failure");
      }
      for( iv=0 ; iv < nvox ; iv++ )
         curstat[iv] = mask[iv] ? out->tstat[iv] : 0.0f;
      tfce_score(curstat,mask,opts->nx,opts->ny,opts->nz,opts->tails,
                opts->tfce_H,opts->tfce_E,opts->tfce_dh,opts->tfce_conn,
                neg_stat,pos_buf,neg_buf,obs_cmp);
      free(curstat); free(pos_buf);
      if( neg_buf != NULL ) free(neg_buf);
      if( neg_stat != NULL ) free(neg_stat);
   } else {
      for( iv=0 ; iv < nvox ; iv++ )
         obs_cmp[iv] = tail_value(out->tstat[iv],opts->tails);
   }

   /* Permutations are independent given their own scratch buffers and
      RNG stream, so each thread runs a disjoint slice of them and
      accumulates into its own unc_count[], reduced into the shared array
      at the end. max_null[] needs no reduction: each permutation owns a
      unique slot. */
   AFNI_OMP_START;
#pragma omp parallel
   {
      DECLARE_ithr;
      int th_is, th_iv, ip;
      float *th_values = (float *)calloc(nsubj,sizeof(float));
      byte *th_flip = (byte *)calloc(nsubj,sizeof(byte));
      int *th_unc = (int *)calloc(nvox,sizeof(int));
      unsigned short xran[3];
      float *th_curstat = NULL, *th_score = NULL, *th_pos = NULL,
            *th_neg = NULL, *th_negstat = NULL;

      if( th_values == NULL || th_flip == NULL || th_unc == NULL )
         ERROR_exit("malloc failure");
      if( opts->tfce ){
         th_curstat = (float *)calloc(nvox,sizeof(float));
         th_score = (float *)calloc(nvox,sizeof(float));
         th_pos = (float *)calloc(nvox,sizeof(float));
         if( th_curstat == NULL || th_score == NULL || th_pos == NULL )
            ERROR_exit("malloc failure");
         if( opts->tails == TAIL_TWO ){
            th_neg = (float *)calloc(nvox,sizeof(float));
            th_negstat = (float *)calloc(nvox,sizeof(float));
            if( th_neg == NULL || th_negstat == NULL ) ERROR_exit("malloc failure");
         }
      }
      /* Distinct, reproducible-per-thread-count RNG stream; erand48() is
         reentrant, unlike the drand48() this replaces, which shares one
         global stream that multiple threads calling it concurrently
         would corrupt. */
      xran[0] = (unsigned short)(opts->seed & 0xFFFFL);
      xran[1] = (unsigned short)((opts->seed >> 16) & 0xFFFFL);
      xran[2] = (unsigned short)((ithr+1)*7919 & 0xFFFF);

#pragma omp for schedule(static)
      for( ip=0 ; ip < nperm ; ip++ ){
         float maxv = -FLT_MAX;
         if( opts->mode == MODE_EXACT ){
            unsigned long bits = (unsigned long)ip;
            for( th_is=0 ; th_is < nsubj ; th_is++ )
               th_flip[th_is] = (byte)((bits >> th_is)&1UL);
         } else {
            for( th_is=0 ; th_is < nsubj ; th_is++ )
               th_flip[th_is] = erand48(xran) < 0.5;
         }

         if( opts->tfce ){
            for( th_iv=0 ; th_iv < nvox ; th_iv++ ){
               float tt;
               if( !mask[th_iv] ){ th_curstat[th_iv] = 0.0f; continue; }
               for( th_is=0 ; th_is < nsubj ; th_is++ )
                  th_values[th_is] = group_a[th_is][th_iv] -
                               (group_b != NULL ? group_b[th_is][th_iv] : 0.0f);
               tt = one_sample_t(th_values,nsubj,th_flip,NULL);
               th_curstat[th_iv] = tt;
            }
            tfce_score(th_curstat,mask,opts->nx,opts->ny,opts->nz,opts->tails,
                      opts->tfce_H,opts->tfce_E,opts->tfce_dh,opts->tfce_conn,
                      th_negstat,th_pos,th_neg,th_score);
            for( th_iv=0 ; th_iv < nvox ; th_iv++ ){
               if( !mask[th_iv] ) continue;
               if( th_score[th_iv] > maxv ) maxv = th_score[th_iv];
               if( th_score[th_iv] >= obs_cmp[th_iv] ) th_unc[th_iv]++;
            }
         } else {
            for( th_iv=0 ; th_iv < nvox ; th_iv++ ){
               float tt, tv;
               if( !mask[th_iv] ) continue;
               for( th_is=0 ; th_is < nsubj ; th_is++ )
                  th_values[th_is] = group_a[th_is][th_iv] -
                               (group_b != NULL ? group_b[th_is][th_iv] : 0.0f);
               tt = one_sample_t(th_values,nsubj,th_flip,NULL);
               tv = tail_value(tt,opts->tails);
               if( tv > maxv ) maxv = tv;
               if( tv >= obs_cmp[th_iv] ) th_unc[th_iv]++;
            }
         }
         max_null[ip] = maxv;

#pragma omp critical(shuffle_progress)
         { completed++; print_progress_bar(completed-1,nperm,&last_pct); }
      }

#pragma omp critical(shuffle_reduce)
      { for( th_iv=0 ; th_iv < nvox ; th_iv++ ) unc_count[th_iv] += th_unc[th_iv]; }

      free(th_values); free(th_flip); free(th_unc);
      if( th_curstat != NULL ) free(th_curstat);
      if( th_score != NULL ) free(th_score);
      if( th_pos != NULL ) free(th_pos);
      if( th_neg != NULL ) free(th_neg);
      if( th_negstat != NULL ) free(th_negstat);
   }
   AFNI_OMP_END;

   finish_permutation_output(out,mask,nvox,unc_count,max_null,nperm,obs_cmp,opts);
   free(max_null); free(unc_count); free(obs_cmp);
}

/* Run a fixed-size independent-group label-shuffle test over all voxels. */
static void run_shuffle_test(float **group_a, int na, float **group_b, int nb,
                             byte *mask, int nvox, opts_t *opts,
                             test_output_t *out, const char *label)
{
   int is, iv, ip, last_pct = -1, completed = 0, ntot = na+nb;
   long long nperm_ll = permutation_count(opts,STAT_TWOSAMPLE,na,nb);
   int nperm;
   int *unc_count, *comb;
   float *max_null, *obs_cmp;
   float **combined;
   byte *in_a, *exact_table = NULL;

   if( nperm_ll <= 0 || nperm_ll > INT_MAX )
      ERROR_exit("shuffle count for %s is too large; use -mode random -niter N",
                 label);
   nperm = (int)nperm_ll;
   unc_count = (int *)calloc(nvox,sizeof(int));
   max_null = (float *)calloc(nperm,sizeof(float));
   combined = (float **)calloc(ntot,sizeof(float *));
   in_a = (byte *)calloc(ntot,sizeof(byte));
   comb = (int *)calloc(na,sizeof(int));
   obs_cmp = (float *)calloc(nvox,sizeof(float));
   if( unc_count == NULL || max_null == NULL || combined == NULL ||
       in_a == NULL || comb == NULL || obs_cmp == NULL )
      ERROR_exit("malloc failure");

   for( is=0 ; is < na ; is++ ){
      combined[is] = group_a[is];
      comb[is] = is;
      in_a[is] = 1;
   }
   for( is=0 ; is < nb ; is++ ) combined[na+is] = group_b[is];

   INFO_message("Computing %s: %d fixed-size label shuffles%s",label,nperm,
               opts->tfce ? " (TFCE)" : "");
   for( iv=0 ; iv < nvox ; iv++ ){
      if( !mask[iv] ) continue;
      out->tstat[iv] = two_sample_t(combined,ntot,na,in_a,iv,
                                    opts->unpooled,&out->mean[iv]);
   }
   if( opts->tfce ){
      float *curstat = (float *)calloc(nvox,sizeof(float));
      float *pos_buf = (float *)calloc(nvox,sizeof(float));
      float *neg_buf = NULL, *neg_stat = NULL;
      if( curstat == NULL || pos_buf == NULL ) ERROR_exit("malloc failure");
      if( opts->tails == TAIL_TWO ){
         neg_buf = (float *)calloc(nvox,sizeof(float));
         neg_stat = (float *)calloc(nvox,sizeof(float));
         if( neg_buf == NULL || neg_stat == NULL ) ERROR_exit("malloc failure");
      }
      for( iv=0 ; iv < nvox ; iv++ )
         curstat[iv] = mask[iv] ? out->tstat[iv] : 0.0f;
      tfce_score(curstat,mask,opts->nx,opts->ny,opts->nz,opts->tails,
                opts->tfce_H,opts->tfce_E,opts->tfce_dh,opts->tfce_conn,
                neg_stat,pos_buf,neg_buf,obs_cmp);
      free(curstat); free(pos_buf);
      if( neg_buf != NULL ) free(neg_buf);
      if( neg_stat != NULL ) free(neg_stat);
   } else {
      for( iv=0 ; iv < nvox ; iv++ )
         obs_cmp[iv] = tail_value(out->tstat[iv],opts->tails);
   }

   /* Exact-mode combinations are generated by next_combination(), which
      mutates its state from the previous combination and so can't be
      parallelized directly. Enumerate the whole sequence once up front
      (cheap: O(nperm*na)) into a flat membership table, then let threads
      index into it independently. Random mode needs no such table --
      each permutation draws its own membership from a per-thread RNG. */
   if( opts->mode == MODE_EXACT ){
      exact_table = (byte *)malloc((size_t)nperm*ntot*sizeof(byte));
      if( exact_table == NULL ) ERROR_exit("malloc failure");
      for( ip=0 ; ip < nperm ; ip++ ){
         membership_from_combination(exact_table + (size_t)ip*ntot,ntot,comb,na);
         if( ip+1 < nperm && !next_combination(comb,na,ntot) )
            ERROR_exit("internal error while enumerating group assignments");
      }
   }

   AFNI_OMP_START;
#pragma omp parallel
   {
      DECLARE_ithr;
      int th_iv, th_ip;
      byte *th_in_a = (byte *)calloc(ntot,sizeof(byte));
      int *th_order = (int *)calloc(ntot,sizeof(int));
      int *th_unc = (int *)calloc(nvox,sizeof(int));
      unsigned short xran[3];
      float *th_curstat = NULL, *th_score = NULL, *th_pos = NULL,
            *th_neg = NULL, *th_negstat = NULL;

      if( th_in_a == NULL || th_order == NULL || th_unc == NULL )
         ERROR_exit("malloc failure");
      if( opts->tfce ){
         th_curstat = (float *)calloc(nvox,sizeof(float));
         th_score = (float *)calloc(nvox,sizeof(float));
         th_pos = (float *)calloc(nvox,sizeof(float));
         if( th_curstat == NULL || th_score == NULL || th_pos == NULL )
            ERROR_exit("malloc failure");
         if( opts->tails == TAIL_TWO ){
            th_neg = (float *)calloc(nvox,sizeof(float));
            th_negstat = (float *)calloc(nvox,sizeof(float));
            if( th_neg == NULL || th_negstat == NULL ) ERROR_exit("malloc failure");
         }
      }
      xran[0] = (unsigned short)(opts->seed & 0xFFFFL);
      xran[1] = (unsigned short)((opts->seed >> 16) & 0xFFFFL);
      xran[2] = (unsigned short)((ithr+1)*7919 & 0xFFFF);

#pragma omp for schedule(static)
      for( th_ip=0 ; th_ip < nperm ; th_ip++ ){
         float maxv = -FLT_MAX;
         byte *cur_in_a;
         if( opts->mode == MODE_EXACT ){
            cur_in_a = exact_table + (size_t)th_ip*ntot;
         } else {
            random_membership(th_in_a,th_order,ntot,na,xran);
            cur_in_a = th_in_a;
         }

         if( opts->tfce ){
            for( th_iv=0 ; th_iv < nvox ; th_iv++ ){
               if( !mask[th_iv] ){ th_curstat[th_iv] = 0.0f; continue; }
               th_curstat[th_iv] =
                  two_sample_t(combined,ntot,na,cur_in_a,th_iv,opts->unpooled,NULL);
            }
            tfce_score(th_curstat,mask,opts->nx,opts->ny,opts->nz,opts->tails,
                      opts->tfce_H,opts->tfce_E,opts->tfce_dh,opts->tfce_conn,
                      th_negstat,th_pos,th_neg,th_score);
            for( th_iv=0 ; th_iv < nvox ; th_iv++ ){
               if( !mask[th_iv] ) continue;
               if( th_score[th_iv] > maxv ) maxv = th_score[th_iv];
               if( th_score[th_iv] >= obs_cmp[th_iv] ) th_unc[th_iv]++;
            }
         } else {
            for( th_iv=0 ; th_iv < nvox ; th_iv++ ){
               float tt, tv;
               if( !mask[th_iv] ) continue;
               tt = two_sample_t(combined,ntot,na,cur_in_a,th_iv,opts->unpooled,NULL);
               tv = tail_value(tt,opts->tails);
               if( tv > maxv ) maxv = tv;
               if( tv >= obs_cmp[th_iv] ) th_unc[th_iv]++;
            }
         }
         max_null[th_ip] = maxv;

#pragma omp critical(shuffle2_progress)
         { completed++; print_progress_bar(completed-1,nperm,&last_pct); }
      }

#pragma omp critical(shuffle2_reduce)
      { for( th_iv=0 ; th_iv < nvox ; th_iv++ ) unc_count[th_iv] += th_unc[th_iv]; }

      free(th_in_a); free(th_order); free(th_unc);
      if( th_curstat != NULL ) free(th_curstat);
      if( th_score != NULL ) free(th_score);
      if( th_pos != NULL ) free(th_pos);
      if( th_neg != NULL ) free(th_neg);
      if( th_negstat != NULL ) free(th_negstat);
   }
   AFNI_OMP_END;

   finish_permutation_output(out,mask,nvox,unc_count,max_null,nperm,obs_cmp,opts);
   free(comb); free(in_a); free(combined);
   free(max_null); free(unc_count); free(obs_cmp);
   if( exact_table != NULL ) free(exact_table);
}

/* Build the analysis mask from an explicit mask or AFNI's automask machinery. */
static byte *make_mask(opts_t *opts, THD_3dim_dataset *mset,
                       THD_3dim_dataset *first, float **vals, int nvox)
{
   int iv, iset, ntot = opts->ntotal;
   byte *mask = (byte *)malloc(sizeof(byte)*nvox);
   if( mask == NULL ) ERROR_exit("malloc failure");
   for( iv=0 ; iv < nvox ; iv++ ) mask[iv] = 1;

   if( mset != NULL ){
      byte *mb = THD_makemask(mset,0,1.0f,0.0f);
      if( mb == NULL ) ERROR_exit("failed to make mask from %s", opts->mask_name);
      /* The mask grid was checked before this call; here only its
         nonzero support is intersected with the analysis voxels. */
      for( iv=0 ; iv < nvox ; iv++ ) if( !mb[iv] ) mask[iv] = 0;
      free(mb);
   }

   if( opts->auto_mask ){
      MRI_IMAGE *avgim;
      float *avgar;
      byte *amask;

      WARNING_message(
         "-automask is being estimated from statistical/effect maps, "
         "which may not have suitable intensity structure.");
      WARNING_message(
         "A carefully constructed group mask supplied with -mask is "
         "strongly recommended.");

      /* THD_automask normally averages absolute values across dataset
         bricks before calling mri_automask_image(). Here the inputs are
         separate datasets, so construct the equivalent image directly. */
      avgim = mri_new_vol(DSET_NX(first),DSET_NY(first),DSET_NZ(first),MRI_float);
      if( avgim == NULL ) ERROR_exit("failed to allocate automask image");
      avgar = MRI_FLOAT_PTR(avgim);

      for( iv=0 ; iv < nvox ; iv++ ){
         int allzero = 1, bad = 0;
         double sumabs = 0.0;
         for( iset=0 ; iset < ntot ; iset++ ){
            float vv = vals[iset][iv];
            /* A single nonfinite observation invalidates a voxel because
               every permutation can assign that value to a test group. */
            if( !isfinite(vv) ){ bad = 1; break; }
            if( vv != 0.0f ) allzero = 0;
            sumabs += fabs((double)vv);
         }
         if( bad || allzero ){
            mask[iv] = 0;
            avgar[iv] = 0.0f;
         } else {
            avgar[iv] = (float)(sumabs/ntot);
         }
      }

      amask = mri_automask_image(avgim);
      mri_free(avgim);
      if( amask == NULL )
         ERROR_exit("AFNI automask failed on the mean-absolute input image");

      /* Retain only voxels selected by AFNI and already known to contain
         finite, nonzero information across the input collection. */
      for( iv=0 ; iv < nvox ; iv++ )
         if( !amask[iv] ) mask[iv] = 0;
      free(amask);

      if( THD_countmask(nvox,mask) <= 0 )
         ERROR_exit("-automask produced an empty analysis mask; use -mask");
   }

   return mask;
}

/* Run 3dShuffle from option parsing through permutation testing and output. */
int main(int argc, char **argv)
{
   opts_t opts;
   THD_3dim_dataset ***dsets = NULL, *first = NULL, *mset = NULL, *outset = NULL;
   float **vals = NULL;
   byte *mask = NULL;
   int ic, is, cc, ib, it, nvox, nout, ntest;
   size_t ncond_alloc;
   float **outbr = NULL;
   test_output_t *tests = NULL;
   char **test_names = NULL;
   int *test_df = NULL;

   mainENTRY("3dShuffle main"); machdep(); PRINT_VERSION(PROGRAM_NAME);
   AFNI_SETUP_OMP(0);
   { int new_argc; char **new_argv;
     addto_args(argc,argv,&new_argc,&new_argv);
     if( new_argv != NULL ){ argc = new_argc; argv = new_argv; }
   }
   AFNI_logger(PROGRAM_NAME,argc,argv);

   /* Without this, MCW_file_expand() globs a wildcarded -input argument's
      sub-brick selector (e.g. '[Vrel#0_Coef]') as a literal character
      class instead of stripping and reattaching it. */
   PUTENV("AFNI_GLOB_SELECTORS","YES") ;

   parse_opts(argc,argv,&opts);
   finalize_brickwise_inputs(&opts);

   resolve_mode(&opts);
   print_sanity(&opts);

   /* parse_opts guarantees ncond > 0; the guarded size also makes that
      allocation invariant explicit to compilers that do not know
      ERROR_exit terminates execution. */
   ncond_alloc = opts.ncond > 0 ? (size_t)opts.ncond : 1U;
   dsets = (THD_3dim_dataset ***)calloc(ncond_alloc,sizeof(THD_3dim_dataset **));
   vals = (float **)calloc(opts.ntotal,sizeof(float *));
   if( dsets == NULL || vals == NULL ) ERROR_exit("malloc failure");

   for( ic=0 ; ic < opts.ncond ; ic++ ){
      int ndset = opts.brickwise ? 1 : opts.nsubj_by_cond[ic];
      dsets[ic] = (THD_3dim_dataset **)calloc(ndset,sizeof(THD_3dim_dataset *));
      if( dsets[ic] == NULL ) ERROR_exit("malloc failure");
      if( opts.brickwise ){
         dsets[ic][0] = THD_open_dataset(opts.input_names[ic][0]);
         CHECK_OPEN_ERROR(dsets[ic][0], opts.input_names[ic][0]);
         if( first == NULL ) first = dsets[ic][0];
         if( DSET_NVALS(dsets[ic][0]) != opts.nsubj_by_cond[ic] )
            ERROR_exit("input %s changed from %d to %d sub-bricks while opening",
                       opts.input_names[ic][0], opts.nsubj_by_cond[ic],
                       DSET_NVALS(dsets[ic][0]));
         /* Permutations compare values voxel-for-voxel, so dimensions,
            orientation, spacing, and origin must match exactly. */
         if( !EQUIV_GRIDS(first,dsets[ic][0]) )
            ERROR_exit("input %s is not on the same grid as %s",
                       opts.input_names[ic][0], opts.input_names[0][0]);
         DSET_load(dsets[ic][0]); CHECK_LOAD_ERROR(dsets[ic][0]);
         for( is=0 ; is < opts.nsubj_by_cond[ic] ; is++ ){
            int idx = opts.offsets[ic]+is;
            vals[idx] = THD_extract_to_float(is,dsets[ic][0]);
            if( vals[idx] == NULL )
               ERROR_exit("failed to extract %s[%d]", opts.input_names[ic][0], is);
         }
      } else {
         for( is=0 ; is < opts.nsubj_by_cond[ic] ; is++ ){
            int idx = opts.offsets[ic]+is;
            dsets[ic][is] = THD_open_dataset(opts.input_names[ic][is]);
            CHECK_OPEN_ERROR(dsets[ic][is], opts.input_names[ic][is]);
            if( first == NULL ) first = dsets[ic][is];
            if( DSET_NVALS(dsets[ic][is]) != 1 )
               ERROR_exit("input %s has %d sub-bricks; 3dShuffle requires single-brick inputs",
                          opts.input_names[ic][is], DSET_NVALS(dsets[ic][is]));
            /* Permutations compare values voxel-for-voxel, so dimensions,
               orientation, spacing, and origin must match exactly. */
            if( !EQUIV_GRIDS(first,dsets[ic][is]) )
               ERROR_exit("input %s is not on the same grid as %s",
                          opts.input_names[ic][is], opts.input_names[0][0]);
            DSET_load(dsets[ic][is]); CHECK_LOAD_ERROR(dsets[ic][is]);
            vals[idx] = THD_extract_to_float(0,dsets[ic][is]);
            if( vals[idx] == NULL ) ERROR_exit("failed to extract %s", opts.input_names[ic][is]);
         }
      }
   }
   nvox = DSET_NVOX(first);
   opts.nx = DSET_NX(first); opts.ny = DSET_NY(first); opts.nz = DSET_NZ(first);
   if( opts.mask_name != NULL ){
      mset = THD_open_dataset(opts.mask_name);
      CHECK_OPEN_ERROR(mset, opts.mask_name);
      /* Applying a mask by voxel index is valid only on the input grid. */
      if( !EQUIV_GRIDS(first,mset) ) ERROR_exit("mask is not on the input grid");
      DSET_load(mset); CHECK_LOAD_ERROR(mset);
   }
   mask = make_mask(&opts,mset,first,vals,nvox);
   INFO_message("%d voxels in analysis mask", THD_countmask(nvox,mask));

   /* A two-sample contrast has two group one-sample families plus the
      contrast family; every family contains the same six output bricks. */
   ntest = opts.stat == STAT_TWOSAMPLE ? 3*opts.ncon : opts.ncon;
   nout = ntest*6;
   outbr = (float **)calloc(nout,sizeof(float *));
   tests = (test_output_t *)calloc(ntest,sizeof(test_output_t));
   test_names = (char **)calloc(ntest,sizeof(char *));
   test_df = (int *)calloc(ntest,sizeof(int));
   if( outbr == NULL || tests == NULL || test_names == NULL || test_df == NULL )
      ERROR_exit("malloc failure");
   for( ib=0 ; ib < nout ; ib++ ){
      outbr[ib] = (float *)calloc(nvox,sizeof(float));
      if( outbr[ib] == NULL ) ERROR_exit("malloc failure");
   }
   for( it=0 ; it < ntest ; it++ ){
      tests[it].mean   = outbr[6*it+0];
      tests[it].tstat  = outbr[6*it+1];
      tests[it].p_unc  = outbr[6*it+2];
      tests[it].p_fwe = outbr[6*it+3];
      tests[it].z_unc  = outbr[6*it+4];
      tests[it].z_fwe = outbr[6*it+5];
      test_df[it] = -1;
   }

   if( opts.mode == MODE_RANDOM ) srand48(opts.seed);

   if( opts.stat == STAT_PAIRED ){
      for( cc=0 ; cc < opts.ncon ; cc++ ){
         int ia = opts.cons[cc].ia, ibb = opts.cons[cc].ib;
         test_names[cc] = copy_string(opts.cons[cc].name);
         test_df[cc] = opts.nsubj-1;
         run_signflip_test(vals+opts.offsets[ia],vals+opts.offsets[ibb],
                           opts.nsubj,mask,nvox,&opts,&tests[cc],
                           opts.cons[cc].name);
      }
   } else if( opts.stat == STAT_ONESAMPLE ){
      for( cc=0 ; cc < opts.ncon ; cc++ ){
         int ia = opts.cons[cc].ia;
         test_names[cc] = copy_string(opts.cons[cc].name);
         test_df[cc] = opts.nsubj_by_cond[ia]-1;
         run_signflip_test(vals+opts.offsets[ia],NULL,
                           opts.nsubj_by_cond[ia],mask,nvox,&opts,&tests[cc],
                           opts.cons[cc].name);
      }
   } else {
      for( cc=0 ; cc < opts.ncon ; cc++ ){
         int ia = opts.cons[cc].ia, ibb = opts.cons[cc].ib;
         int na = opts.nsubj_by_cond[ia], nb = opts.nsubj_by_cond[ibb];
         int base = 3*cc;
         char label[THD_MAX_NAME];

         test_names[base] = safe_label_name(opts.cond_labels[ia]);
         test_names[base+1] = safe_label_name(opts.cond_labels[ibb]);
         test_names[base+2] = copy_string(opts.cons[cc].name);
         test_df[base] = na-1;
         test_df[base+1] = nb-1;
         /* Welch degrees of freedom vary by voxel, so an unpooled
            contrast t brick is intentionally left without a FITT code. */
         test_df[base+2] = opts.unpooled ? -1 : na+nb-2;

         snprintf(label,sizeof(label),"group %s vs 0",opts.cond_labels[ia]);
         run_signflip_test(vals+opts.offsets[ia],NULL,na,mask,nvox,
                           &opts,&tests[base],label);
         snprintf(label,sizeof(label),"group %s vs 0",opts.cond_labels[ibb]);
         run_signflip_test(vals+opts.offsets[ibb],NULL,nb,mask,nvox,
                           &opts,&tests[base+1],label);
         snprintf(label,sizeof(label),"contrast %s",opts.cons[cc].name);
         run_shuffle_test(vals+opts.offsets[ia],na,
                          vals+opts.offsets[ibb],nb,mask,nvox,
                          &opts,&tests[base+2],label);
      }
   }

   outset = EDIT_empty_copy(first);
   EDIT_dset_items(outset,
                   ADN_prefix, opts.prefix,
                   ADN_datum_all, MRI_float,
                   ADN_nvals, nout,
                   ADN_ntt, 0,
                   ADN_none);

   for( ib=0 ; ib < nout ; ib++ ){
      char lab[THD_MAX_NAME];
      EDIT_substitute_brick(outset,ib,MRI_float,outbr[ib]);
      outbr[ib] = NULL;
      switch( ib % 6 ){
         case 0: snprintf(lab,sizeof(lab),"%s_mean",  test_names[ib/6]); break;
         case 1: snprintf(lab,sizeof(lab),"%s_t",     test_names[ib/6]); break;
         case 2: snprintf(lab,sizeof(lab),"%s_p_unc", test_names[ib/6]); break;
         case 3: snprintf(lab,sizeof(lab),"%s_p_fwe", test_names[ib/6]); break;
         case 4: snprintf(lab,sizeof(lab),"%s_z_unc", test_names[ib/6]); break;
         default: snprintf(lab,sizeof(lab),"%s_z_fwe",test_names[ib/6]); break;
      }
      EDIT_BRICK_LABEL(outset,ib,lab);

      /* CON_t IS tagged FITT here so it can be thresholded in the GUI
         for direct comparison against CON_z_fwe. This is intentional:
         seeing how much more permissive (liberal) the parametric FITT
         p-value is at small N vs. the permutation-corrected FIZT
         p-value is a useful, concrete illustration of exactly the
         problem this program exists to solve. Do NOT report results
         based on CON_t/FITT -- use it only as a side-by-side reference
         against CON_z_fwe when writing up or sanity-checking findings. */
      if( ib % 6 == 1 && test_df[ib/6] > 0 )
         EDIT_BRICK_TO_FITT(outset,ib,test_df[ib/6]);
      if( ib % 6 == 4 || ib % 6 == 5 )
         EDIT_BRICK_TO_FIZT(outset,ib);
   }

   tross_Copy_History(first,outset);
   tross_Make_History(PROGRAM_NAME,argc,argv,outset);
   THD_load_statistics(outset);
   THD_write_3dim_dataset(NULL,NULL,outset,True);
   WROTE_DSET(outset);

   exit(0);
}
