/*-------------------------------------------------------------------------
  3dShuffle: randomization tests for AFNI datasets.

  V1 implements paired/repeated-measures sign-flip permutation tests for
  single-brick datasets.

  This version adds signed z-score bricks (CON_z_unc / CON_z_fwe) derived
  from the empirical permutation p-values, tagged as FIZT stat bricks so
  that AFNI's interactive GUI threshold slider displays the correct
  permutation-derived p-value rather than a parametric one. Threshold on
  CON_z_fwe (not CON_t) when visualizing results.
-------------------------------------------------------------------------*/

#include "mrilib.h"
#include <ctype.h>
#include <limits.h>
#include <math.h>
#include <float.h>

#define PROGRAM_NAME "3dShuffle"

typedef enum {
   TAIL_TWO = 0,
   TAIL_ONE
} tail_code;

typedef enum {
   MODE_AUTO = 0,
   MODE_EXACT,
   MODE_RANDOM
} mode_code;

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
   int ncon;
   int auto_mask;
   int have_niter;
   int niter;
   long seed;
   char *prefix;
   char *mask_name;
   char **cond_labels;
   char **subj_labels;
   char ***input_names;
   contrast_t *cons;
   tail_code tails;
   mode_code mode;
} opts_t;

static void shuffle_help(void)
{
   printf(
"Usage: 3dShuffle [options]\n"
"\n"
"Permutation/randomization testing for AFNI datasets.\n"
"\n"
"V1 implements paired/repeated-measures sign-flip permutation tests for\n"
"single-brick datasets. This is appropriate for paired contrasts where the\n"
"input order defines subject pairing across repeated-measure conditions.\n"
"\n"
"Example:\n"
"  3dShuffle                                      \\\n"
"    -conditions 3                                \\\n"
"    -cond_labels baseline endsleep longrec       \\\n"
"    -subj_labels s01 s02 s03 s04 s05 s06         \\\n"
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
"    -auto_mask                                   \\\n"
"    -prefix sleep_shuffle\n"
"\n"
"Required options:\n"
"  -conditions N       Number of repeated-measure conditions.\n"
"  -input dset ...     One -input list per condition. Each list must have\n"
"                      the same number of single-brick datasets.\n"
"  -contrast A B       Test contrast A-B. A and B can be condition labels\n"
"                      or 1-based condition indices. May be repeated.\n"
"  -prefix PREFIX      Output bucket dataset prefix.\n"
"\n"
"Labels:\n"
"  -cond_labels L1 ... LN\n"
"                      Names for the N conditions. Used for contrasts and\n"
"                      output sub-brick labels.\n"
"  -subj_labels S1 ... SN\n"
"                      Subject labels. Pairing is still by input order.\n"
"\n"
"Permutation options:\n"
"  -method signflip    Paired sign-flip permutation test. Required in V1.\n"
"  -stat paired_ttest  Paired t-statistic. Required/default in V1.\n"
"  -tails two|one\n"
"                      Default: two.\n"
"                      With -tails one, the tested direction is positive\n"
"                      for the stated contrast A-B. That is, one-tailed\n"
"                      -contrast A B tests whether A > B. To test whether\n"
"                      B > A, reverse the contrast order and use\n"
"                      -contrast B A -tails one.\n"
"  -mode exact|random  Exact enumerates all 2^N sign patterns. Random uses\n"
"                      -niter random sign patterns. Default: exact when\n"
"                      feasible, otherwise random if -niter is supplied.\n"
"  -niter N            Number of random sign-flip iterations.\n"
"  -seed S             Random seed for -mode random. Default: 1234567.\n"
"\n"
"Masking:\n"
"  -mask MASK          Restrict analysis to nonzero voxels in MASK.\n"
"  -auto_mask          Ignore voxels containing NaN/Inf in any input, and\n"
"                      voxels where all values across conditions/subjects\n"
"                      are zero. Invalid stat bricks are written as 0 and\n"
"                      invalid p-value bricks as 1.\n"
"\n"
"Output:\n"
"  One float bucket dataset with 6 sub-bricks per contrast:\n"
"    CON_mean          observed mean of A-B\n"
"    CON_t             observed paired t-statistic (NOT stat-coded --\n"
"                      do not threshold on this; it has no corrected\n"
"                      p-value attached and inviting the AFNI GUI to\n"
"                      read a parametric p off it defeats the purpose\n"
"                      of a permutation test)\n"
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
"IMPORTANT resolution ceiling:\n"
"  With exact sign-flip enumeration, the smallest achievable p-value is\n"
"  2/2^Nsubj for -tails two and 1/2^Nsubj for -tails one. For N=6, these\n"
"  floors are 0.03125 and 0.015625. CON_z_fwe/CON_z_unc will never exceed\n"
"  the |z| corresponding to that floor, regardless of true effect size.\n"
"  This is a property of small-N exact permutation, not a bug.\n"
"\n"
"Important warning:\n"
"  Independent-group label permutation tests are not implemented yet.\n"
"  Do not use -method signflip for unpaired/two-sample designs.\n"
"\n"
   );
   PRINT_COMPILE_DATE;
   exit(0);
}

static int is_opt(const char *s)
{
   return s != NULL && s[0] == '-';
}

static char *copy_string(const char *s)
{
   char *out = NULL;
   if( s == NULL ) return NULL;
   out = (char *)malloc(strlen(s)+1);
   if( out == NULL ) ERROR_exit("malloc failure");
   strcpy(out,s);
   return out;
}

static void init_opts(opts_t *opts)
{
   memset(opts,0,sizeof(opts_t));
   opts->tails = TAIL_TWO;
   opts->mode = MODE_AUTO;
   opts->seed = 1234567L;
}

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

static int parse_int_arg(const char *s, const char *opt)
{
   char *end = NULL;
   long val = strtol(s,&end,10);
   if( end == s || *end != '\0' || val <= 0 || val > INT_MAX )
      ERROR_exit("bad integer after %s: %s",opt,s);
   return (int)val;
}

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
         if( opts->cond_labels == NULL || opts->input_names == NULL )
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

      if( strcmp(argv[nopt],"-subj_labels") == 0 ){
         int start = ++nopt, nlab = 0;
         while( nopt < argc && !is_opt(argv[nopt]) ){ nlab++; nopt++; }
         if( nlab <= 0 ) ERROR_exit("need labels after -subj_labels");
         if( opts->ninput > 0 && opts->nsubj != nlab )
            ERROR_exit("-subj_labels count %d differs from -input count %d",
                       nlab, opts->nsubj);
         opts->subj_labels = (char **)calloc(nlab,sizeof(char *));
         if( opts->subj_labels == NULL ) ERROR_exit("malloc failure");
         for( ii=0 ; ii < nlab ; ii++ ) opts->subj_labels[ii] = copy_string(argv[start+ii]);
         opts->nsubj = nlab;
         continue;
      }

      if( strcmp(argv[nopt],"-input") == 0 ){
         int start = ++nopt, nds = 0, ic = opts->ninput;
         if( opts->ncond <= 0 ) ERROR_exit("-conditions must precede -input");
         if( ic >= opts->ncond ) ERROR_exit("too many -input lists for -conditions %d", opts->ncond);
         while( nopt < argc && !is_opt(argv[nopt]) ){ nds++; nopt++; }
         if( nds <= 0 ) ERROR_exit("need datasets after -input");
         opts->input_names[ic] = (char **)calloc(nds,sizeof(char *));
         if( opts->input_names[ic] == NULL ) ERROR_exit("malloc failure");
         for( ii=0 ; ii < nds ; ii++ ) opts->input_names[ic][ii] = copy_string(argv[start+ii]);
         if( opts->ninput == 0 ){
            if( opts->nsubj == 0 ) opts->nsubj = nds;
            else if( opts->nsubj != nds )
               ERROR_exit("-subj_labels count %d differs from first -input count %d",opts->nsubj,nds);
         } else if( nds != opts->nsubj ){
            ERROR_exit("-input list %d has %d datasets, but expected %d",ic+1,nds,opts->nsubj);
         }
         opts->ninput++;
         continue;
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
         if( strcmp(argv[nopt],"signflip") != 0 )
            ERROR_exit("V1 only implements -method signflip");
         nopt++; continue;
      }

      if( strcmp(argv[nopt],"-stat") == 0 ){
         if( ++nopt >= argc ) ERROR_exit("need an argument after -stat");
         if( strcmp(argv[nopt],"paired_ttest") != 0 )
            ERROR_exit("V1 only implements -stat paired_ttest");
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

      if( strcmp(argv[nopt],"-auto_mask") == 0 ){
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

   if( opts->ncond <= 1 ) ERROR_exit("need -conditions N, with N > 1");
   if( opts->ninput != opts->ncond )
      ERROR_exit("need exactly %d -input lists, found %d",opts->ncond,opts->ninput);
   if( opts->nsubj < 2 ) ERROR_exit("need at least 2 paired subjects");
   if( opts->ncon <= 0 ) ERROR_exit("need at least one -contrast A B");
   if( opts->prefix == NULL ) ERROR_exit("need -prefix");

   for( ii=0 ; ii < opts->ncond ; ii++ ){
      char buf[32];
      if( opts->cond_labels[ii] == NULL ){
         sprintf(buf,"cond%d",ii+1);
         opts->cond_labels[ii] = copy_string(buf);
      }
   }
   if( opts->subj_labels == NULL ){
      opts->subj_labels = (char **)calloc(opts->nsubj,sizeof(char *));
      if( opts->subj_labels == NULL ) ERROR_exit("malloc failure");
      for( ii=0 ; ii < opts->nsubj ; ii++ ){
         char buf[32];
         sprintf(buf,"subj%d",ii+1);
         opts->subj_labels[ii] = copy_string(buf);
      }
   }

   for( ii=0 ; ii < opts->ncon ; ii++ ){
      opts->cons[ii].ia = label_to_index(opts, opts->cons[ii].a);
      opts->cons[ii].ib = label_to_index(opts, opts->cons[ii].b);
      if( opts->cons[ii].ia == opts->cons[ii].ib )
         ERROR_exit("contrast %s %s uses the same condition twice",opts->cons[ii].a,opts->cons[ii].b);
      opts->cons[ii].name = safe_contrast_name(opts->cond_labels[opts->cons[ii].ia],
                                               opts->cond_labels[opts->cons[ii].ib]);
   }

   if( opts->mode == MODE_AUTO ){
      if( opts->have_niter ) opts->mode = MODE_RANDOM;
      else opts->mode = MODE_EXACT;
   }
   if( opts->mode == MODE_RANDOM && !opts->have_niter )
      ERROR_exit("-mode random requires -niter");
   if( opts->mode == MODE_EXACT && opts->nsubj >= (int)(8*sizeof(unsigned long)-1) )
      ERROR_exit("too many subjects for exact sign-flip enumeration on this build; use -mode random -niter N");
}

static void print_sanity(opts_t *opts, long long nperm)
{
   int is, ic, cc;
   INFO_message("paired/repeated-measures sign-flip test");
   INFO_message("Conditions:   %d", opts->ncond);
   INFO_message("Subjects:     %d", opts->nsubj);
   INFO_message("Method:       signflip");
   INFO_message("Statistic:    paired_ttest");
   INFO_message("Tails:        %s", opts->tails == TAIL_TWO ? "two" : "one");
   if( opts->tails == TAIL_ONE )
      INFO_message("One-tailed direction: positive for each contrast A-B");
   INFO_message("Mode:         %s", opts->mode == MODE_EXACT ? "exact" : "random");
   INFO_message("Permutations: %lld", nperm);
   if( opts->mode == MODE_RANDOM ) INFO_message("Seed:         %ld", opts->seed);

   INFO_message("Input pairing:");
   for( is=0 ; is < opts->nsubj ; is++ ){
      fprintf(stderr,"++   %s:", opts->subj_labels[is]);
      for( ic=0 ; ic < opts->ncond ; ic++ )
         fprintf(stderr," %s=%s", opts->cond_labels[ic], opts->input_names[ic][is]);
      fprintf(stderr,"\n");
   }

   INFO_message("Contrasts:");
   for( cc=0 ; cc < opts->ncon ; cc++ )
      fprintf(stderr,"++   %s = %s - %s\n", opts->cons[cc].name,
              opts->cond_labels[opts->cons[cc].ia],
              opts->cond_labels[opts->cons[cc].ib]);
}

static float paired_t_from_diffs(float *diff, int nsubj, unsigned long bits,
                                 int use_bits, float *mean_out)
{
   int ii;
   double sum = 0.0, ss = 0.0, mean, var;
   for( ii=0 ; ii < nsubj ; ii++ ){
      double sgn = 1.0;
      if( use_bits && ((bits >> ii) & 1UL) ) sgn = -1.0;
      sum += sgn * diff[ii];
   }
   mean = sum / nsubj;
   for( ii=0 ; ii < nsubj ; ii++ ){
      double sgn = 1.0, dd;
      if( use_bits && ((bits >> ii) & 1UL) ) sgn = -1.0;
      dd = sgn * diff[ii] - mean;
      ss += dd * dd;
   }
   if( mean_out != NULL ) *mean_out = (float)mean;
   if( nsubj < 2 ) return 0.0f;
   var = ss / (nsubj-1);
   if( var <= 0.0 ) return 0.0f;
   return (float)(mean / sqrt(var/nsubj));
}

static float tail_value(float tt, tail_code tails)
{
   if( tails == TAIL_TWO ) return fabsf(tt);
   return tt;
}

static int cmp_float(const void *a, const void *b)
{
   float aa = *((const float *)a), bb = *((const float *)b);
   return (aa > bb) - (aa < bb);
}

/* Simple in-place percentage progress bar on stderr. Only redraws when
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
static float p_to_signed_z(float pval, float observed_stat, tail_code tails)
{
   double p, z;
   double pmin = 1.0e-15;

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

static byte *make_mask(opts_t *opts, THD_3dim_dataset *mset, float **vals, int nvox)
{
   int iv, iset, ntot = opts->ncond * opts->nsubj;
   byte *mask = (byte *)malloc(sizeof(byte)*nvox);
   if( mask == NULL ) ERROR_exit("malloc failure");
   for( iv=0 ; iv < nvox ; iv++ ) mask[iv] = 1;

   if( mset != NULL ){
      byte *mb = THD_makemask(mset,0,1.0f,0.0f);
      if( mb == NULL ) ERROR_exit("failed to make mask from %s", opts->mask_name);
      for( iv=0 ; iv < nvox ; iv++ ) if( !mb[iv] ) mask[iv] = 0;
      free(mb);
   }

   if( opts->auto_mask ){
      for( iv=0 ; iv < nvox ; iv++ ){
         int allzero = 1, bad = 0;
         for( iset=0 ; iset < ntot ; iset++ ){
            float vv = vals[iset][iv];
            if( !isfinite(vv) ){ bad = 1; break; }
            if( vv != 0.0f ) allzero = 0;
         }
         if( bad || allzero ) mask[iv] = 0;
      }
   }

   return mask;
}

int main(int argc, char **argv)
{
   opts_t opts;
   THD_3dim_dataset ***dsets = NULL, *first = NULL, *mset = NULL, *outset = NULL;
   float **vals = NULL;
   byte *mask = NULL;
   int ic, is, iv, cc, ib, nvox, nout;
   long long nperm_ll;
   int nperm;
   float **outbr = NULL;

   mainENTRY("3dShuffle main"); machdep(); PRINT_VERSION(PROGRAM_NAME);
   { int new_argc; char **new_argv;
     addto_args(argc,argv,&new_argc,&new_argv);
     if( new_argv != NULL ){ argc = new_argc; argv = new_argv; }
   }
   AFNI_logger(PROGRAM_NAME,argc,argv);

   parse_opts(argc,argv,&opts);

   nperm_ll = (opts.mode == MODE_EXACT) ? (1LL << opts.nsubj) : opts.niter;
   if( nperm_ll <= 0 || nperm_ll > INT_MAX )
      ERROR_exit("number of permutations is too large for V1: %lld", nperm_ll);
   nperm = (int)nperm_ll;
   print_sanity(&opts,nperm_ll);

   dsets = (THD_3dim_dataset ***)calloc(opts.ncond,sizeof(THD_3dim_dataset **));
   vals = (float **)calloc(opts.ncond*opts.nsubj,sizeof(float *));
   if( dsets == NULL || vals == NULL ) ERROR_exit("malloc failure");

   for( ic=0 ; ic < opts.ncond ; ic++ ){
      dsets[ic] = (THD_3dim_dataset **)calloc(opts.nsubj,sizeof(THD_3dim_dataset *));
      if( dsets[ic] == NULL ) ERROR_exit("malloc failure");
      for( is=0 ; is < opts.nsubj ; is++ ){
         int idx = ic*opts.nsubj + is;
         dsets[ic][is] = THD_open_dataset(opts.input_names[ic][is]);
         CHECK_OPEN_ERROR(dsets[ic][is], opts.input_names[ic][is]);
         if( first == NULL ) first = dsets[ic][is];
         if( DSET_NVALS(dsets[ic][is]) != 1 )
            ERROR_exit("input %s has %d sub-bricks; V1 requires single-brick inputs",
                       opts.input_names[ic][is], DSET_NVALS(dsets[ic][is]));
         if( !EQUIV_GRIDS(first,dsets[ic][is]) )
            ERROR_exit("input %s is not on the same grid as %s",
                       opts.input_names[ic][is], opts.input_names[0][0]);
         DSET_load(dsets[ic][is]); CHECK_LOAD_ERROR(dsets[ic][is]);
         vals[idx] = THD_extract_to_float(0,dsets[ic][is]);
         if( vals[idx] == NULL ) ERROR_exit("failed to extract %s", opts.input_names[ic][is]);
      }
   }
   nvox = DSET_NVOX(first);
   if( opts.mask_name != NULL ){
      mset = THD_open_dataset(opts.mask_name);
      CHECK_OPEN_ERROR(mset, opts.mask_name);
      if( !EQUIV_GRIDS(first,mset) ) ERROR_exit("mask is not on the input grid");
      DSET_load(mset); CHECK_LOAD_ERROR(mset);
   }
   mask = make_mask(&opts,mset,vals,nvox);
   INFO_message("%d voxels in analysis mask", THD_countmask(nvox,mask));

   /* 6 output bricks per contrast now: mean, t, p_unc, p_fwe, z_unc, z_fwe */
   nout = opts.ncon * 6;
   outbr = (float **)calloc(nout,sizeof(float *));
   if( outbr == NULL ) ERROR_exit("malloc failure");
   for( ib=0 ; ib < nout ; ib++ ){
      outbr[ib] = (float *)calloc(nvox,sizeof(float));
      if( outbr[ib] == NULL ) ERROR_exit("malloc failure");
   }

   if( opts.mode == MODE_RANDOM ) srand48(opts.seed);

   for( cc=0 ; cc < opts.ncon ; cc++ ){
      float *mean_br = outbr[6*cc+0];
      float *t_br    = outbr[6*cc+1];
      float *p_br    = outbr[6*cc+2];
      float *pfwe_br = outbr[6*cc+3];
      float *zunc_br = outbr[6*cc+4];
      float *zfwe_br = outbr[6*cc+5];
      int *unc_count = (int *)calloc(nvox,sizeof(int));
      float *max_null = (float *)calloc(nperm,sizeof(float));
      float *diff = (float *)calloc(opts.nsubj,sizeof(float));
      int last_pct = -1;
      if( unc_count == NULL || max_null == NULL || diff == NULL )
         ERROR_exit("malloc failure");

      INFO_message("Computing contrast %s = %s - %s",
                   opts.cons[cc].name,
                   opts.cond_labels[opts.cons[cc].ia],
                   opts.cond_labels[opts.cons[cc].ib]);

      for( iv=0 ; iv < nvox ; iv++ ){
         if( !mask[iv] ){ p_br[iv] = 1.0f; pfwe_br[iv] = 1.0f; continue; }
         for( is=0 ; is < opts.nsubj ; is++ ){
            float va = vals[opts.cons[cc].ia*opts.nsubj + is][iv];
            float vb = vals[opts.cons[cc].ib*opts.nsubj + is][iv];
            diff[is] = va - vb;
         }
         t_br[iv] = paired_t_from_diffs(diff,opts.nsubj,0UL,0,&mean_br[iv]);
      }

      for( ib=0 ; ib < nperm ; ib++ ){
         unsigned long bits = 0UL;
         float maxv = -1.0e30f;
         if( opts.mode == MODE_EXACT ) bits = (unsigned long)ib;
         else {
            for( is=0 ; is < opts.nsubj ; is++ )
               if( drand48() < 0.5 ) bits |= (1UL << is);
         }
         for( iv=0 ; iv < nvox ; iv++ ){
            float tt, tv;
            if( !mask[iv] ) continue;
            for( is=0 ; is < opts.nsubj ; is++ ){
               float va = vals[opts.cons[cc].ia*opts.nsubj + is][iv];
               float vb = vals[opts.cons[cc].ib*opts.nsubj + is][iv];
               diff[is] = va - vb;
            }
            tt = paired_t_from_diffs(diff,opts.nsubj,bits,1,NULL);
            tv = tail_value(tt,opts.tails);
            if( tv > maxv ) maxv = tv;
            if( tv >= tail_value(t_br[iv],opts.tails) ) unc_count[iv]++;
         }
         max_null[ib] = maxv;
         print_progress_bar(ib,nperm,&last_pct);
      }

      qsort(max_null,nperm,sizeof(float),cmp_float);

      for( iv=0 ; iv < nvox ; iv++ ){
         if( !mask[iv] ) continue;
         if( opts.mode == MODE_EXACT )
            p_br[iv] = (float)unc_count[iv] / (float)nperm;
         else
            p_br[iv] = (float)(unc_count[iv]+1) / (float)(nperm+1);
         pfwe_br[iv] = emp_p_from_sorted(max_null,nperm,tail_value(t_br[iv],opts.tails),
                                         opts.mode == MODE_EXACT);
      }

      /* Convert both p-value bricks to signed z for GUI-thresholdable
         FIZT bricks. Masked-out voxels get z=0 (matches p=1). */
      for( iv=0 ; iv < nvox ; iv++ ){
         if( !mask[iv] ){ zunc_br[iv] = 0.0f; zfwe_br[iv] = 0.0f; continue; }
         zunc_br[iv] = p_to_signed_z(p_br[iv],    t_br[iv], opts.tails);
         zfwe_br[iv] = p_to_signed_z(pfwe_br[iv], t_br[iv], opts.tails);
      }

      free(diff);
      free(max_null);
      free(unc_count);
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
         case 0: snprintf(lab,sizeof(lab),"%s_mean",  opts.cons[ib/6].name); break;
         case 1: snprintf(lab,sizeof(lab),"%s_t",     opts.cons[ib/6].name); break;
         case 2: snprintf(lab,sizeof(lab),"%s_p_unc", opts.cons[ib/6].name); break;
         case 3: snprintf(lab,sizeof(lab),"%s_p_fwe", opts.cons[ib/6].name); break;
         case 4: snprintf(lab,sizeof(lab),"%s_z_unc", opts.cons[ib/6].name); break;
         default: snprintf(lab,sizeof(lab),"%s_z_fwe",opts.cons[ib/6].name); break;
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
      if( ib % 6 == 1 )
         EDIT_BRICK_TO_FITT(outset,ib,opts.nsubj-1);
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
