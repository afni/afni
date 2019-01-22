/* ----------------------------------------------------------------------
 * Do stuff.
 *
 *
 * Author: R Reynolds  18 July 2017
 */

static char * g_history[] =
{
  "----------------------------------------------------------------------\n"
  "history (of 3dTto1D):\n"
  "\n",
  "0.0  18 Jul 2017\n"
  "     (Rick Reynolds of the National Institutes of Health, SSCC/DIRP/NIMH)\n"
  "     -initial version\n"
  "1.0  19 Jul 2017: initial release\n"
  "1.1  18 Aug 2017: modify help\n"
  "1.2  01 Feb 2018: added -4095_count/frac/warn\n"
};

static char g_version[] = "3dTto1D version 1.2, 1 February 2018";

#include "mrilib.h"


/*--------------- method enumeration ---------------*/
#define T21_METH_UNDEF          0
#define T21_METH_ENORM          1       /* enorm      */
#define T21_METH_RMS            2       /* rms        */
#define T21_METH_SRMS           3       /* srms       */
#define T21_METH_S_SRMS         4       /* shift_srms */
#define T21_METH_MDIFF          5       /* mdiff      */
#define T21_METH_SMDIFF         6       /* smdiff     */
#define T21_METH_4095_COUNT     7       /* 4095_count */
#define T21_METH_4095_FRAC      8       /* 4095_frac  */
#define T21_METH_4095_WARN      9       /* 4095_warn  */

/*--------------- global options struct ---------------*/
typedef struct
{
   THD_3dim_dataset * inset;
   char             * mask_name;
   char             * prefix;
   int                method;
   int                automask;
   int                verb;

   /* put parameters here, too */
   int                nt;
   float            * result; /* of length nt */
   byte             * mask;
} options_t;

options_t g_opts;

/*--------------- prototypes ---------------*/

int    show_help           (void);
int    check_dims          (options_t *);
int    compute_results     (options_t *);
int    compute_4095        (options_t *, int);
int    compute_enorm       (options_t *, int);
int    compute_meandiff    (options_t *, int);
int    fill_mask           (options_t *);
int    meth_name_to_index  (char *);
char * meth_index_to_name  (int);
int    process_opts        (options_t *, int, char *[] );
int    write_results       (options_t *);

/*--------------- main routine ---------------*/
int main( int argc, char *argv[] )
{
   options_t * opts = &g_opts;
   int         rv;

   if( argc < 2 ) { show_help();  return 0; }

   mainENTRY("3dTto1D main"); machdep(); AFNI_logger("3dTto1D",argc,argv);

   /* process command line arguments (and read dataset and mask) */
   rv = process_opts(opts, argc, argv);
   if( rv ) RETURN(rv < 0); /* only a negative return is considered failure */

   if( check_dims(opts) ) RETURN(1);

   /* evaluation of rv now depends on the method, usually non-zero is bad */
   rv = compute_results(opts);

   /* for 4095_warn, return now, regardless */
   if( opts->method == T21_METH_4095_WARN ) RETURN(rv);

   /* otherwise, any non-zero return is a failure */
   if ( rv ) RETURN(1);

   if( write_results(opts) ) RETURN(1);

   RETURN(0);
}


/* just make sure we have sufficient data for computations */
int check_dims(options_t * opts)
{
   int nt, nvox, nmask;

   ENTRY("check_dims");

   nt = DSET_NVALS(opts->inset);
   nvox = DSET_NVOX(opts->inset);
   if( opts->mask ) nmask = THD_countmask( nvox, opts->mask );
   else             nmask = nvox;

   /* make sure we have something to compute */
   if( nvox < 1 ) {
      ERROR_message("input dataset must have at least 1 voxel");
      RETURN(1);
   } else if( nmask < 1 ) {
      ERROR_message("input mask must have at least 1 voxel");
      RETURN(1);
   } else if( nt < 2 ) {
      ERROR_message("input dataset must have at least 2 time points");
      RETURN(1);
   }

   RETURN(0);
}


int write_results(options_t * opts)
{
   FILE * fp;

   int c;

   ENTRY("write_results");

   if( ! opts->result ) {
      ERROR_message("no results to write!");
      RETURN(1);
   }

   if( ! opts->prefix )                         fp = stdout;
   else if ( !strcmp(opts->prefix, "-") )       fp = stdout;
   else if ( !strcmp(opts->prefix, "stdout") )  fp = stdout;
   else if ( !strcmp(opts->prefix, "stderr") )  fp = stderr;
   else {
      fp = fopen(opts->prefix, "w");
      if( ! fp ) {
         ERROR_message("failed to open '%s' for writing", opts->prefix);
         RETURN(1);
      }
   }

   if( opts->verb ) {
      if     ( fp == stdout ) INFO_message("writing to stdout...");
      else if( fp == stdout ) INFO_message("writing to stderr...");
      else if( opts->prefix ) INFO_message("writing to '%s'...", opts->prefix);
      else                    INFO_message("writing to unknown?!?");
   }

   /* actually write results */
   for( c=0; c < opts->nt; c++ )
      fprintf(fp, "%f\n", opts->result[c]);

   if( fp != stdout && fp != stderr )
      fclose(fp);

   RETURN(0);
}


int compute_results(options_t * opts)
{
   int method = opts->method;

   ENTRY("compute_results");

   /* the first 3 methods are varieties of enorm/dvars */
   if( method == T21_METH_ENORM ||
       method == T21_METH_RMS   || 
       method == T21_METH_SRMS  ||
       method == T21_METH_S_SRMS)  RETURN(compute_enorm(opts, method));

   if( method == T21_METH_MDIFF ||
       method == T21_METH_SMDIFF ) RETURN(compute_meandiff(opts, method));
   
   if( method == T21_METH_4095_COUNT ||
       method == T21_METH_4095_FRAC  ||
       method == T21_METH_4095_WARN ) RETURN(compute_4095(opts, method));
   
   ERROR_message("unknown method index %d", method);

   RETURN(1);
}


/* this computes the mean abs first (backwards) diff */
int compute_meandiff(options_t * opts, int method)
{
   double * dwork, dscale, gmean, mdiff;
   float  * fdata, fdiff;
   byte   * mask = opts->mask;  /* save 6 characters... */
   int      nt, nvox, vind, tind, nmask;

   ENTRY("compute_meandiff");

   nt = DSET_NVALS(opts->inset);
   nvox = DSET_NVOX(opts->inset);

   /* note how many voxels this is over */
   if( opts->mask ) nmask = THD_countmask( nvox, opts->mask );
   else             nmask = nvox;

   if( opts->verb )
      INFO_message("computing %s, nvox = %d, nmask = %d, nt = %d",
                   meth_index_to_name(method), nvox, nmask, nt);

   /* check_dims() has been called, so we have sufficient data */

   dwork = calloc(nt, sizeof(double));
   fdata = calloc(nt, sizeof(float));

   /* could steal fdata, but that might be unethical (plus, garbage on err) */
   opts->result = calloc(nt, sizeof(float));

   /* use gmean to get mean across all masked voxels and time */
   gmean = 0.0;
   mdiff = 0.0;
   for( vind=0; vind < nvox; vind++ ) {
      if( opts->mask && ! opts->mask[vind] ) continue;

      if( THD_extract_array(vind, opts->inset, 0, fdata) ) {
         ERROR_message("failed to exract data at index %d\n", vind);
         free(dwork);  free(fdata);  RETURN(1);
      }

      /* accumuate squared differences; dwork[0] is already 0 */
      gmean += fdata[0];  /* and accumlate for mean */
      for( tind=1; tind < nt; tind++ ) {
         gmean += fdata[tind];      /* accumlate for mean */
         fdiff = fabs(fdata[tind]-fdata[tind-1]);
         mdiff += fdiff;            /* accumulate for mean diff */
         dwork[tind] += fdiff;
      }
   }

   /* convert dsum to dmean and then a scalar:
    * method = 1 : enorm  sqrt(ss)
    *          2 : rms    sqrt(ss/nmask) = stdev (biased) of first diffs
    *          3 : srms   sqrt(ss/nmask) / abs(grand mean)
    */
   gmean = fabs(gmean/nmask/nt); /* global masked mean */
   mdiff /= nmask*nt;            /* global masked mean first diff */

   /* set the enorm scalar, based on the method */
   if( method == T21_METH_MDIFF ) {
      if( opts->verb ) INFO_message("mean diff uses no scaling");
      dscale = nmask;
   } else if ( method == T21_METH_SMDIFF ) {
      if( opts->verb ) INFO_message("scaled mean diff = mdiff/gmean");
      if( gmean == 0.0 ) {
         ERROR_message("values have zero mean, failing (use mdiff, instead)");
         free(dwork);  free(fdata);  RETURN(1);
      } else if( gmean < 1.0 ) {
         WARNING_message("values might be de-meaned, consider mdiff");
      }
      dscale = nmask*gmean;
   }

   if( opts->verb )
      INFO_message("global mean = %f, mean diff = %f, mdiff/gmean = %f",
                   gmean, mdiff, mdiff/gmean);

   /* scale and store the result */
   for( tind=1; tind < nt; tind++ )
      opts->result[tind] = dwork[tind] / dscale;
   opts->nt = nt;

   free(dwork);
   free(fdata);
  
   if( opts->verb > 1 ) INFO_message("successfully computed mean diff");

   RETURN(0);
}


/* count voxels or mask fraction that hits a global maximum of 4095
 *
 * method = 4095_count  : return masked counts
 *          4095_frac   : return masked fractions
 *          4095_warn   : return limit warning
 *
 * return 1 on error, 0 otherwise
 */
int compute_4095(options_t * opts, int method)
{
   double * dwork;
   float  * fdata, gmax, fval;
   byte   * mask = opts->mask;  /* save 6 characters... */
   int      nt, nvox, vind, tind, nmask, found, nlocal;

   ENTRY("compute_4095");

   nt = DSET_NVALS(opts->inset);
   nvox = DSET_NVOX(opts->inset);

   /* note how many voxels this is over */
   if( opts->mask ) nmask = THD_countmask( nvox, opts->mask );
   else             nmask = nvox;

   if( opts->verb )
      INFO_message("computing %s, nvox = %d, nmask = %d, nt = %d",
                   meth_index_to_name(method), nvox, nmask, nt);

   /* allocate result early, in case there is nothing to do */
   opts->result = calloc(nt, sizeof(float));

   if( nmask == 0 ) {
      ERROR_message("empty mask");
      RETURN(0);
   }

   /* check_dims() has been called, so we have sufficient data */
   fdata = calloc(nt, sizeof(float));

   /* do the work */
   gmax = 0.0;
   found = 0;
   for( vind=0; vind < nvox; vind++ ) {
      if( opts->mask && ! opts->mask[vind] ) continue;

      if( THD_extract_array(vind, opts->inset, 0, fdata) ) {
         ERROR_message("failed to exract data at index %d\n", vind);
         free(fdata);  RETURN(1);
      }

      /* start counting */
      for( tind=0; tind < nt; tind++ ) {
         fval = fdata[tind];

         /* track global max */
         if( fval > gmax ) gmax = fval;

         /* if we pass the limit, quit working */
         if( gmax > 4095.0 ) break;

         /* track hits */
         if( fval == 4095.0 ) opts->result[tind]++;
      }
   }

   /* no longer needed */
   free(fdata);
  
   /* babble to user */
   if( opts->verb )
      INFO_message("global max = %f", gmax);

   /* if warning, decide and return */
   if( method == T21_METH_4095_WARN ) {
      if( gmax != 4095 ) {
        if( opts->verb ) INFO_message("max of %g is okay", gmax);
        else             { printf("0\n");  fflush(stdout); }
        RETURN(0);
      }

      if( opts->verb ) WARNING_message("suspicious max of exactly 4095");
      else             { printf("1\n"); fflush(stdout); }

      RETURN(0);
   }

   /* if frac, scale */
   if( method == T21_METH_4095_FRAC ) {
      for( tind=0; tind < nt; tind++ ) opts->result[tind] /= nmask;
   }

   if( opts->verb > 1 ) INFO_message("successfully ran 4095 test");

   RETURN(0);
}


/* this is basically enorm, with scaling variants 
 *
 * convert dsum to dmean and then a scalar:
 * method = 1 : enorm  sqrt(ss)
 *          2 : rms    sqrt(ss/nvox) = stdev (biased)
 *          3 : srms   sqrt(ss/nvox) / grand mean
 */
int compute_enorm(options_t * opts, int method)
{
   double * dwork, dscale, gmean, mdiff;
   float  * fdata, fdiff;
   byte   * mask = opts->mask;  /* save 6 characters... */
   int      nt, nvox, vind, tind, nmask;

   ENTRY("compute_enorm");

   nt = DSET_NVALS(opts->inset);
   nvox = DSET_NVOX(opts->inset);

   /* note how many voxels this is over */
   if( opts->mask ) nmask = THD_countmask( nvox, opts->mask );
   else             nmask = nvox;

   if( opts->verb )
      INFO_message("computing %s, nvox = %d, nmask = %d, nt = %d",
                   meth_index_to_name(method), nvox, nmask, nt);

   /* check_dims() has been called, so we have sufficient data */

   dwork = calloc(nt, sizeof(double));
   fdata = calloc(nt, sizeof(float));

   /* could steal fdata, but that might be unethical (plus, garbage on err) */
   opts->result = calloc(nt, sizeof(float));

   /* use gmean to get mean across all masked voxels and time */
   gmean = 0.0;
   mdiff = 0.0;
   for( vind=0; vind < nvox; vind++ ) {
      if( opts->mask && ! opts->mask[vind] ) continue;

      if( THD_extract_array(vind, opts->inset, 0, fdata) ) {
         ERROR_message("failed to exract data at index %d\n", vind);
         free(dwork);  free(fdata);  RETURN(1);
      }

      /* accumuate squared differences; dwork[0] is already 0 */
      gmean += fdata[0];  /* and accumlate for mean */
      for( tind=1; tind < nt; tind++ ) {
         gmean += fdata[tind];      /* accumlate for mean */
         fdiff = fdata[tind]-fdata[tind-1];
         mdiff += fabs(fdiff);      /* accumulate for mean diff */
         dwork[tind] += fdiff*fdiff;
      }
   }

   /* convert dsum to dmean and then a scalar:
    * method = 1 : enorm  sqrt(ss)
    *          2 : rms    sqrt(ss/nmask) = stdev (biased) of first diffs
    *          3 : srms   sqrt(ss/nmask) / abs(grand mean)
    */
   gmean = fabs(gmean/nmask/nt); /* global masked mean */
   mdiff /= nmask*nt;            /* global masked mean first diff */

   /* set the enorm scalar, based on the method */
   if( method == T21_METH_ENORM ) {
      if( opts->verb ) INFO_message("writing enorm : uses no scaling");
      dscale = 1.0;
   } else if ( method == T21_METH_RMS ) {
      if( opts->verb ) INFO_message("writing rms = dvars = enorm/sqrt(nvox)");
      dscale = sqrt(nmask);
   } else if ( method == T21_METH_SRMS || method == T21_METH_S_SRMS ) {
      if( opts->verb ) INFO_message("scaled dvars = srms = rms/gmean");
      if( gmean == 0.0 ) {
         ERROR_message("values have zero mean, failing (use rms, instead)");
         free(dwork);  free(fdata);  RETURN(1);
      } else if( gmean < 1.0 ) {
         WARNING_message("values might be de-meaned, consider rms");
      }
      dscale = sqrt(nmask)*gmean;
   }

   /* babble to user */
   if( opts->verb )
      INFO_message("global mean = %f, mean diff = %f, mdiff/gmean = %f",
                   gmean, mdiff, mdiff/gmean);
   if( opts->verb > 2 ) INFO_message("scaling enorm down by %f", dscale);

   /* scale and store the result */
   for( tind=1; tind < nt; tind++ )
      opts->result[tind] = sqrt(dwork[tind]) / dscale;
   opts->nt = nt;

   /* if computing a mean diff, apply the shift */
   if( method == T21_METH_S_SRMS ) {
      mdiff /= gmean;
      if( opts->verb ) INFO_message("shift by scaled mean diff %f", mdiff);
      for( tind=1; tind < nt; tind++ )
         opts->result[tind] -= mdiff;
   }

   free(dwork);
   free(fdata);
  
   if( opts->verb > 1 ) INFO_message("successfully computed enorm");

   RETURN(0);
}


int fill_mask(options_t * opts)
{
   THD_3dim_dataset * mset;
   int nvox;

ENTRY("fill_mask");

   if( opts->automask ) {
      if( opts->verb ) INFO_message("creating automask...");

      opts->mask = THD_automask(opts->inset);
      if( ! opts->mask ) {
         ERROR_message("failed to apply -automask");
         RETURN(1);
      }

      RETURN(0);
   }

   if( opts->mask_name ) {
      if( opts->verb )
         INFO_message("reading mask dset from %s...", opts->mask_name);

      mset = THD_open_dataset( opts->mask_name );
      if( ! mset ) ERROR_exit("cannot open mask dset '%s'", opts->mask_name);
      nvox = DSET_NVOX(opts->inset);
      if( DSET_NVOX(mset) != nvox ) {
         ERROR_message("mask does not have the same voxel count as input");
         RETURN(1);
      }

      /* fill mask array and mask_nxyz, remove mask dset */
      DSET_load(mset); CHECK_LOAD_ERROR(mset);

      opts->mask = THD_makemask(mset, 0, 1, 0);
      DSET_delete(mset);

      if( ! opts->mask ) {
         ERROR_message("cannot make mask from '%s'", opts->mask_name);
         RETURN(1);
      }

      if( opts->verb > 1 )
         INFO_message("have mask with %d voxels", nvox);
   }

   RETURN(0);
}


/* ----------------------------------------------------------------------*/
/* method name to and from index */
int meth_name_to_index(char * name)
{
   if( ! name ) return T21_METH_UNDEF;

   if( ! strcasecmp(name, "enorm") )   return T21_METH_ENORM;

   if( ! strcasecmp(name, "rms") || 
       ! strcasecmp(name, "dvars") )   return T21_METH_RMS;

   if( ! strcasecmp(name, "srms") ||
       ! strcasecmp(name, "cvar") )    return T21_METH_SRMS;

   if( ! strcasecmp(name, "shift_srms") ||
       ! strcasecmp(name, "s_srms") )  return T21_METH_S_SRMS;

   if( ! strcasecmp(name, "mdiff") )   return T21_METH_MDIFF;

   if( ! strcasecmp(name, "smdiff") )  return T21_METH_SMDIFF;

   if( ! strcasecmp(name, "4095_count") ) return T21_METH_4095_COUNT;
   if( ! strcasecmp(name, "4095_frac") )  return T21_METH_4095_FRAC;
   if( ! strcasecmp(name, "4095_warn") )  return T21_METH_4095_WARN;

   /* be explicit, since we have a case for this */
   if( ! strcmp(name, "undefined") ) return T21_METH_UNDEF;

   return T21_METH_UNDEF;
}

char * meth_index_to_name(int method)
{
   if( method == T21_METH_ENORM )   return "enorm";
   if( method == T21_METH_RMS )     return "rms";
   if( method == T21_METH_SRMS )    return "srms";
   if( method == T21_METH_S_SRMS )  return "shift_srms";
   if( method == T21_METH_MDIFF )   return "mdiff";
   if( method == T21_METH_SMDIFF )  return "smdiff";
   if( method == T21_METH_4095_COUNT ) return "4095_count";
   if( method == T21_METH_4095_FRAC )  return "4095_frac";
   if( method == T21_METH_4095_WARN )  return "4095_warn";

   return "undefined";
}

/* ----------------------------------------------------------------------*/
int show_help(void)
{
   ENTRY("show_help");

   printf(
   "-------------------------------------------------------------------------\n"
   "3dTto1D             - collapse a 4D time series to a 1D time series\n"
   "\n"
   "The program takes as input a 4D times series (a list of 1D time series)\n"
   "and optionally a mask, and computes from in a 1D time series using some\n"
   "method applied to the first (backward) differences.  Methods include:\n"
   "\n"
   "    enorm           : the Euclidean norm\n"
   "    rms/dvars       : root mean square (DVARS)\n"
   "    srms            : rms scaled down by global mean \n"
   "    shift_srms      : srms shifted by the global mean\n"
   "    mdiff           : mean abs(diff)\n"
   "    smdiff          : mdiff scaled down by global mean\n"
   "    4095_count      : count voxels with max of exactly 4095\n"
   "    4095_frac       : fraction of masked voxels with max of exactly 4095\n"
   "    4095_warn       : warn if short datum and max of exactly 4095\n"
   "\n"
   "More details are provided after the examples.\n"
   "\n"
   "--------------------------------------------------\n"
   "examples:\n"
   "\n"
   "E1. compute SRMS of EPI time series within a brain mask\n"
   "    (might be good for censoring, and is comparable across subjects)\n"
   "\n"
   "       3dTto1D -input epi_r1+orig -mask mask.auto.nii.gz -method srms \\\n"
   "               -prefix epi_srms.1D\n"
   "\n"
   "E2. compute DVARS of EPI time series within a brain mask\n"
   "    (similarly good for censoring, but not comparable across subjects)\n"
   "\n"
   "       3dTto1D -input epi_r1+orig -mask mask.auto.nii.gz -method dvars \\\n"
   "               -prefix epi_dvars.1D\n"
   "\n"
   "E3. compute ENORM of motion parameters\n"
   "    (as is done by afni_proc.py via 1d_tool.py)\n"
   "\n"
   "    Note that 1D inputs will generally need the transpose operator,\n"
   "    applied by appending an escaped ' to the -input dataset.\n"
   "\n"
   "       3dTto1D -input dfile.r01.1D\\' -method enorm -prefix enorm.r01.1D\n"
   "\n"
   "E4. warn if short data and max is 4095\n"
   "\n"
   "       3dTto1D -input epi+orig -method 4095_warn\n"
   "\n"
   "--------------------------------------------------\n"
   "methods:\n"
   "\n"
   "   Since the initial step is generally to compute the first (backwards)\n"
   "   difference, call that dataset TDIFF.  The value at any voxel of TDIFF\n"
   "   is the same as the input, minus the value at the prior time point.\n"
   "   TDIFF is defined as 0 at time point 0.\n"
   "\n"
   "   method enorm\n"
   "\n"
   "      This is the Euclidean norm.\n"
   "\n"
   "      Starting with the TDIFF dataset, the value at each time point is\n"
   "      the Euclidean norm for that volume (or list of values).  This is\n"
   "      the same as the L2-norm, and which is often applied to the motion\n"
   "      parameters for censoring.\n"
   "\n"
   "         enorm = sqrt(sum squares)\n"
   "\n"
   "\n"
   "   method rms (or dvars)\n"
   "\n"
   "      RMS = DVARS = enorm/sqrt(nvox).\n"
   "\n"
   "      The RMS (root mean square) is the same as the enorm divided by\n"
   "      sqrt(nvox).  It is like a standard deviation, but without removal\n"
   "      of the mean (per time point).\n"
   "\n"
   "         rms = dvars = enorm/sqrt(nvox) = sqrt(sum squares/nvox)\n"
   "\n"
   "      This is the RMS of backward differences first described by Smyser\n"
   "      et. al., 2010, for motion detection, and later renamed to DVARS by\n"
   "      Power et. al., 2012.\n"
   "\n"
   "    * DVARS survives a resampling, where it would be unchanged if every\n"
   "      voxel were listed multiple times, for example.\n"
   "\n"
   "    * DVARS does not survive a scaling, it scales with the data.\n"
   "      This is why the SRMS method was introduced.\n"
   "\n"
   "\n"
   "   method srms (or cvar) (= scaled rms = dvars/mean)\n"
   "\n"
   "      This result is basically the coefficient of variation, but without\n"
   "      removal of each volume mean.\n"
   "\n"
   "      This is the same as dvars divided by the global mean, gmean.\n"
   "\n"
   "         srms = dvars/gmean = enorm/sqrt(nvox)/gmean\n"
   "\n"
   "    * SRMS survives both a resampling and scaling of the data.  Since it\n"
   "      is unchanged with any data scaling (unlike DVARS), values are\n"
   "      comparable across subjects and studies.\n"
   "\n"
   "  *** The above 3 curves will look identical, subject to scaling.\n"
   "\n"
   "\n"
   "   method shift_srms  (= srms - meandiff)\n"
   "\n"
   "      This is simply the SRMS curve shifted down by the global mean of\n"
   "      (the absolute values of) the first differences.  This is probably\n"
   "      useless.\n"
   "\n"
   "\n"
   "   method mdiff (mean diff = mean abs(first diff))\n"
   "\n"
   "      Again, starting with the first backward difference, TDIFF, this\n"
   "      is just the mean absolute value, per time point.\n"
   "\n"
   "\n"
   "   method smdiff (scaled mean diff = mdiff/mean)\n"
   "\n"
   "      This is the mean diff scaled by the global mean.\n"
   "\n"
   "   method 4095_count\n"
   "\n"
   "      At each time point, output the number of (masked) voxels that are\n"
   "      exactly 4095.\n"
   "\n"
   "   method 4095_frac\n"
   "\n"
   "      At each time point, output the fraction of (masked) voxels that\n"
   "      are exactly 4095.\n"
   "\n"
   "   method 4095_warn\n"
   "\n"
   "      Simply warn whether the maximum is exactly 4095 (so no -prefix).\n"
   "\n"
   "--------------------------------------------------\n"
   "informational command arguments:\n"
   "\n"
   "   -help                    : show this help\n"
   "   -hist                    : show program history\n"
   "   -ver                     : show program version\n"
   "\n"
   "--------------------------------------------------\n"
   "required command arguments:\n"
   "\n"
   "   -input DSET              : specify input dataset\n"
   "\n"
   "         e.g. -input epi_r1+orig\n"
   "         e.g. -input dfile.r01.1D\\'\n"
   "\n"
   "      Specify the input dataset to be processed.  This should be a set\n"
   "      of 3D time series.  If the input is in 1D format, a transpose\n"
   "      operator will typically be required.\n"
   "\n"
   "   -method METHOD           : specify 4D to 1D conversion method\n"
   "\n"
   "         e.g. -method srms\n"
   "         e.g. -method DVARS\n"
   "         e.g. -method dvars\n"
   "         e.g. -method enorm\n"
   "\n"
   "      Details of the computational methods are at the top of the help.\n"
   "      The methods (which are case insensitive) include:\n"
   "\n"
   "         enorm      : Euclidean norm of first differences\n"
   "                      = sqrt(sum squares(first diffs))\n"
   "\n"
   "         rms        : RMS (root mean square) of first differences\n"
   "                      = DVARS = enorm/sqrt(nvox)\n"
   "\n"
   "         srms       : scaled (by grand mean) RMS of first differences\n"
   "                      = DVARS/mean\n"
   "\n"
   "                  * seems like the most useful method for censoring\n"
   "\n"
   "         s_srms     : SRMS shifted by grand mean abs of first diffs\n"
   "                      = SRMS - mean(abs(first diffs))\n"
   "\n"
   "         mdiff      : mean absolute first differences\n"
   "                      = mean(abs(first diff))\n"
   "\n"
   "         smdiff     : mdiff scaled by grand mean\n"
   "                      = mdiff/mean\n"
   "\n"
   "         4095_count : count of voxels that are exactly 4095\n"
   "\n"
   "         4095_frac  : fraction of voxels that are exactly 4095\n"
   "                      = 4095_count/(mask size)\n"
   "\n"
   "         4095_warn  : state whether global max is exactly 4095\n"
   "                      (no 1D output)\n"
   "\n"
   "--------------------------------------------------\n"
   "optional command arguments:\n"
   "\n"
   "   -automask        : restrict computation to automask\n"
   "   -mask MSET       : restrict computation to given mask\n"
   "   -prefix PREFIX   : specify output file\n"
   "         e.g.     -prefix SVAR_run1.1D\n"
   "         default: -prefix stdout\n"
   "   -verb LEVEL      : specify verbose level\n"
   "         e.g.     -verb 2\n"
   "         default: -verb 1\n"
   "\n"
   "--------------------------------------------------\n"
   "R Reynolds  July, 2017\n"
   "-------------------------------------------------------------------------\n"
   );

   printf("%s\ncompiled: %s\n\n", g_version, __DATE__);

   RETURN(0);
}


/* ----------------------------------------------------------------------
 * fill the options_t struct
 * return 1 on (acceptable) termination
 *        0 on continue
 *       -1 on error
 */
int process_opts(options_t * opts, int argc, char * argv[] )
{
   int ac;

   ENTRY("process_opts");

   memset(opts, 0, sizeof(options_t));  /* init everything to 0 */
   opts->method = T21_METH_UNDEF;       /* be explicit, required option */
   opts->verb = 1;

   ac = 1;
   while( ac < argc ) {

      /* check for terminal options */
      if( ! strcmp(argv[ac],"-help") || ! strcmp(argv[ac],"-h") ) {
         show_help();
         RETURN(1);
      } else if( strcmp(argv[ac],"-hist") == 0 ) {
         int c, len = sizeof(g_history)/sizeof(char *);
         for( c = 0; c < len; c++) fputs(g_history[c], stdout);
         putchar('\n');
         RETURN(1);
      } else if( strcmp(argv[ac],"-ver") == 0 ) {
         puts(g_version);
         RETURN(1);
      }

      /* the remaining options are alphabetical */

      else if( strcmp(argv[ac],"-automask") == 0 ) {
         opts->automask = 1;
         ac++; continue;
      }
      else if( ! strcmp(argv[ac],"-input") || ! strcmp(argv[ac], "-inset") ) {
         if( opts->inset ) ERROR_exit("-input already applied");

         if( ++ac >= argc ) ERROR_exit("need argument after '-input'");

         opts->inset = THD_open_dataset( argv[ac] );
         opts->nt = DSET_NVALS(opts->inset);
         if( ! opts->inset ) ERROR_exit("cannot open dset '%s'", argv[ac]);

         DSET_load(opts->inset); CHECK_LOAD_ERROR(opts->inset);
         ac++; continue;
      }
      else if( ! strcmp(argv[ac],"-mask") ) {
         if( opts->mask_name ) ERROR_exit("-mask already applied");
         if( ++ac >= argc   ) ERROR_exit("need argument after '-mask'");

         opts->mask_name = argv[ac];
         ac++; continue;
      }

      else if( ! strcmp(argv[ac],"-method") ) {
         if( ++ac >= argc   ) ERROR_exit("need argument after '-method'");

         opts->method = meth_name_to_index(argv[ac]);
         if( opts->method == T21_METH_UNDEF )
            ERROR_exit("illegal -method '%s'", argv[ac]);
         ac++; continue;
      }

      else if( strcmp(argv[ac],"-prefix") == 0 ) {
         if( ++ac >= argc ) ERROR_exit("need argument after '-prefix'");

         opts->prefix = argv[ac];

         if( !THD_filename_ok(opts->prefix) )
            ERROR_exit("Illegal name after '-prefix'");
         ac++; continue;
      }

      else if( strcmp(argv[ac],"-verb") == 0 ) {
         if( ++ac >= argc ) ERROR_exit("need argument after '-verb'");

         opts->verb = atoi(argv[ac]);
         ac++; continue;
      }

      ERROR_message("** unknown option '%s'\n",argv[ac]);
      RETURN(-1);
   }

   /* require data and method */
   if( !opts->inset ) ERROR_exit("missing -input dataset");
   if( opts->method == T21_METH_UNDEF ) ERROR_exit("missing -method option");

   if( opts->verb > 1 ) {
      INFO_message("input dataset loaded\n");
      INFO_message("will apply method %s", meth_index_to_name(opts->method));
   }

   if( opts->automask && opts->mask_name )
      ERROR_exit("cannot apply both -mask and -mset");

   if( opts->automask || opts->mask_name ) {
      if( fill_mask(opts) ) RETURN(1);
   }

   RETURN(0);
}

