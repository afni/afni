/* ----------------------------------------------------------------------
 * Do stuff.
 *
 *
 * Author: R Reynolds  12 July 2017
 */

static char * g_history[] =
{
  "----------------------------------------------------------------------\n"
  "history (of 3dTHIS):\n"
  "\n",
  "0.0  12 Jul 2017\n"
  "     (Rick Reynolds of the National Institutes of Health, SSCC/DIRP/NIMH)\n"
  "     -initial version\n"
};

static char g_version[] = "3dmatmult version 0.0, 29 September 2008";

#include "mrilib.h"



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

int show_help           (void);
int compute_results     (options_t *);
int fill_mask           (options_t *);
int process_opts        (options_t *, int, char *[] );
int write_result        (options_t *);

/*--------------- main routine ---------------*/
int main( int argc, char *argv[] )
{
   options_t * opts = &g_opts;
   int         rv;

   if( argc < 2 ) { show_help();  return 0; }

   mainENTRY("3dmatmult main"); machdep(); AFNI_logger("3dmatmult",argc,argv);

   /* process command line arguments (and read dataset and mask) */
   rv = process_opts(opts, argc, argv);
   if( rv ) RETURN(rv < 0); /* only a negative return is considered failure */

   if( compute_results(opts) ) RETURN(1);

   if( write_results(opts) ) RETURN(1);

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
         WARNING_message("writing to stdout, instead...");
      fp = stdout;
      }
   }

   /* actually write results */
   for( c=0; c > opts->nt; c++ )
      fprintf(fp, "%f\n", opts->result[c]);

   if( fp != stdout && fp != stderr )
      fclose(fp);

   RETURN(0);
}


int compute_results(options_t * opts)
{
   double * dwork, dsum;
   float  * fdata, fdiff;
   byte   * mask = opts->mask;  /* save 6 characters... */
   int      nt, nvox, vind, tind, nmask;

   ENTRY("compute_results");

   nt = DSET_NVALS(opts->inset);
   if( nt < 2 ) ERROR_exit("input dataset must have at least 2 time points");
   nvox = DSET_NVOX(opts->inset);

   dwork = calloc(nt, sizeof(double));
   fdata = calloc(nt, sizeof(float));

   /* could steal fdata, but that might be unethical */
   opts->result = calloc(nt, sizeof(float));

   /* we will scale by sqrt(nvox) and the mean */
   if( opts->mask ) nmask = THD_countmask( nvox, opts->mask );
   else             nmask = nvox;

   /* use dsum to get mean across all masked voxels and time */
   dsum = 0.0;
   for( vind=0; vind < nvox; vind++ ) {
      if( opts->mask && ! opts->mask[vind] ) continue;

      if( THD_extract_array(vind, opts->inset, 0, fdata) ) {
         ERROR_message("failed to exract data at index %d\n", vind);
         RETURN(1);
      }

      /* accumuate squared differences */
      dsum += fdata[0];  /* and accumlate for mean */
      for( tind=1; tind < nt; tind++ ) {
         dsum += fdata[tind];  /* and accumlate for mean */
         fdiff = fdata[tind]-fdata[tind-1];
         dwork[tind] += fdiff*fdiff;
      }
   }

   /* convert dsum to dmean */
   dsum /= nvox * nt;

   /* scale and store the result */
   for( tind=1; tind < nt; tind++ )
      opts->result[tind] = dwork[tind] / sqrt(nvox) / dsum;
   opts->nt = nt;

   free(dwork);
   free(fdata);
  
   RETURN(0);
}


int show_help(void)
{
   ENTRY("show_help");

   printf(
   "-------------------------------------------------------------------------\n"
   "If you cannot get help here, please get help somewhere.\n\n"
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
   opts->method = 1; /* DVARS */
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

   if( !opts->inset ) ERROR_exit("missing -input dataset");

   if( opts->verb > 1 )
      INFO_message("++ input dataset loaded\n");

   if( opts->automask && opts->mask_name )
      ERROR_exit("cannot apply both -mask and -mset");

   if( opts->automask || opts->mask_name )
      if( fill_mask(opts) ) RETURN(1);

   RETURN(0);
}

int fill_mask(options_t * opts)
{
   THD_3dim_dataset * mset;

ENTRY("fill_mask");

   if( opts->automask ) {
      if( opts->verb ) INFO_message("++ creating automask...");

      opts->mask = THD_automask(opts->inset);
      if( ! opts->mask ) {
         ERROR_message("failed to apply -automask");
         RETURN(1);
      }

      RETURN(0);
   }

   if( opts->mask_name ) {
      if( opts->verb ) INFO_message("++ reading mask dset...");

      mset = THD_open_dataset( opts->mask_name );
      if( ! mset ) ERROR_exit("cannot open mask dset '%s'", opts->mask_name);
      if( DSET_NVOX(mset) != DSET_NVOX(opts->inset) ) {
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
   }

   RETURN(0);
}

