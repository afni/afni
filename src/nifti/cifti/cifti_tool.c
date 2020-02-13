/* ----------------------------------------------------------------------
 * A basic example to read/write a cifti dataset (e.g. cp command).
 *
 * compile example (consider -pedantic or -Wall):
 *
 * gcc -o clib_02_nifti2 clib_02.nifti2.c        \
 *     -I../include -L../lib -lniftiio -lznz -lz -lm
 *
 * OR
 *
 * gcc -o clib_02_nifti2 clib_02.nifti2.c -I ../niftilib        \
 *     -I ../znzlib ../niftilib/nifti2_io.o ../znzlib/znzlib.o -lz -lm
 *
 * R Reynolds   19 Jun 2015
 *----------------------------------------------------------------------
 */
#include <stdio.h>
#include <inttypes.h>

#define USE_NIFTI2
#include <nifti2_io.h>
#include "afni_xml_io.h"

static const char * g_history[] =
{
  "----------------------------------------------------------------------\n"
  "cifti_tool modification history:\n",
  "0.0  16 Jun 2015 [rickr]\n"
  "     (Rick Reynolds of the National Institutes of Health, SSCC/DIRP/NIMH)\n"
  "     - initial version\n"
  "0.1   8 Jul 2015 [rickr]\n"
  "     - recur and up through axio_read_cifti_file\n"
  "0.2  17 Aug 2015 [rickr]\n"
  "     - new eval_type: 'show_summary'\n"
  "0.3  21 Aug 2015 [rickr]\n"
  "     - add more help and -hist\n"
  "----------------------------------------------------------------------\n"
};

static char g_version[] = "cifti_tool version 0.3, 21 August, 2015";

/* ----------------------------------------------------------------- */
/* define and declare main option struct */
typedef struct {
   char * fin;
   char * fout;
   char * eval_type;
   int    verb;
   int    vread;
   int    as_cext;
   int    eval_cext;
   int    disp_cext;

} opts_t;

opts_t gopt;


/* ----------------------------------------------------------------- */
/* protos */

/* processing */
int disp_cifti_extension(nifti_image * nim, opts_t * opts);
int disp_hex_data       (const char *mesg, const void *data, int len, FILE *fp);
int eval_cifti_extension(afni_xml_t * ax, opts_t * opts);
int show_cifti_summary  (FILE * fp, afni_xml_t * ax, int verb);

/* main */
int process_args        (int argc, char * argv[], opts_t * opts);
int process             (opts_t * opts);
int show_help           (void);
int show_hist           (void);
int write_extension     (FILE * fp, nifti1_extension * ext, int maxlen);

/* recur */
int ax_has_data         (FILE * fp, afni_xml_t * ax, int depth);
int ax_has_bdata        (FILE * fp, afni_xml_t * ax, int depth);
int ax_num_tokens       (FILE * fp, afni_xml_t * ax, int depth);
int ax_show_text_data   (FILE * fp, afni_xml_t * ax, int depth);
int ax_show_names       (FILE * fp, afni_xml_t * ax, int depth);

/* stream */
int          close_stream        (FILE * fp);
FILE       * open_write_stream   (char * fname);

/* ----------------------------------------------------------------- */
int main(int argc, char * argv[])
{
   int rv;

   memset(&gopt, 0, sizeof(gopt));
   gopt.verb = 1;
   gopt.vread = 1;

   rv = process_args(argc, argv, &gopt);
   if( rv < 0 ) return 1;  /* error */
   if( rv > 0 ) return 0;  /* non-error termination */

   /* rv == 0, continue... */
   return process(&gopt);
}

/* ----------------------------------------------------------------- */
int process_args(int argc, char * argv[], opts_t * opts)
{
   int ac;

   if( argc < 2 ) return show_help();   /* typing '-help' is sooo much work */

   /* process user options */
   for( ac = 1; ac < argc; ac++ ) {
      if( ! strcmp(argv[ac], "-h") || ! strcmp(argv[ac], "-help") ) {
         return show_help();
      } else if( ! strcmp(argv[ac], "-hist") ){
         show_hist();
         return 1;
      } else if( ! strcmp(argv[ac], "-as_cext") ||
               ! strcmp(argv[ac], "-as_cifti_ext") ) {
         opts->as_cext = 1;
      } else if( ! strcmp(argv[ac], "-disp_cext") ) {
         opts->disp_cext = 1;
      } else if( ! strcmp(argv[ac], "-eval_cext") ) {
         opts->eval_cext = 1;
      } else if( ! strcmp(argv[ac], "-eval_type") ) {
         if( ++ac >= argc ) {
            fprintf(stderr, "** missing argument for -eval_type\n");
            return -1;
         }
         opts->eval_type = argv[ac];
      } else if( ! strcmp(argv[ac], "-input") ) {
         if( ++ac >= argc ) {
            fprintf(stderr, "** missing argument for -input\n");
            return -1;
         }
         opts->fin = argv[ac];  /* no string copy, just pointer assignment */
      } else if( ! strcmp(argv[ac], "-output") ) {
         if( ++ac >= argc ) {
            fprintf(stderr, "** missing argument for -output\n");
            return -1;
         }
         opts->fout = argv[ac];
      } else if( ! strcmp(argv[ac], "-verb") ) {
         if( ++ac >= argc ) {
            fprintf(stderr, "** missing argument for -verb\n");
            return -1;
         }
         opts->verb = atoi(argv[ac]);
      } else if( ! strcmp(argv[ac], "-verb_read") ) {
         if( ++ac >= argc ) {
            fprintf(stderr, "** missing argument for -verb\n");
            return -1;
         }
         opts->vread = atoi(argv[ac]);
      } else if( ! strcmp(argv[ac], "-vboth") ) {
         if( ++ac >= argc ) {
            fprintf(stderr, "** missing argument for -verb\n");
            return -1;
         }
         opts->verb = atoi(argv[ac]);
         opts->vread = atoi(argv[ac]);
      } else if( ! strcmp(argv[ac], "-ver") ) {
         puts(g_version);
         return 1;
      } else {
         fprintf(stderr,"** invalid option, '%s'\n", argv[ac]);
         return -1;
      }
   }

   return 0;
}

/* ----------------------------------------------------------------- */
int process(opts_t * opts)
{
   nifti_image * nim = NULL;
   afni_xml_t  * ax;

   if( !opts->fin ){ fprintf(stderr, "** missing option '-input'\n"); return 1;}

   axml_set_verb(opts->vread);
   nifti_set_debug_level(opts->vread);

   /* get xml pointer from file or extension */
   if( opts->as_cext ) ax = axio_read_file(opts->fin);
   else                axio_read_cifti_file(opts->fin, 0, &nim, &ax);

   if( ! ax ) {
      fprintf(stderr,"** failed to read CIFTI ext from %s\n", opts->fin);
      return 1;
   }

   if( opts->disp_cext ) disp_cifti_extension(nim, opts);
   if( opts->eval_cext ) eval_cifti_extension(ax, opts);

   return 0;
}

int disp_cifti_extension(nifti_image * nim, opts_t * opts)
{
   nifti1_extension * ext;
   FILE             * fp;
   int                ind;

   if(gopt.verb > 1)
      fprintf(stderr,"-- displaying CIFTI extension to %s\n",
              opts->fout ? opts->fout : "DEFAULT" );

   if( !nim ) return 1;
   ext = nim->ext_list;
   for( ind = 0; ind < nim->num_ext; ind++ )
      if( ext->ecode == NIFTI_ECODE_CIFTI ) break;

   fp = open_write_stream(opts->fout);
   if( ext && ext->ecode != NIFTI_ECODE_CIFTI ) {
      fprintf(fp, "** no CIFTI extension in %s\n",nim->fname?nim->fname:"NULL");
      return 1;
   }

   if(ext) {
     fprintf(fp, "%.*s\n", ext->esize-8, ext->edata);
   }

   /* possibly close file */
   close_stream(fp);

   return 0;
}

int eval_cifti_extension(afni_xml_t * ax, opts_t * opts)
{
   FILE * fp;

   if(gopt.verb > 1)
      fprintf(stderr,"-- evaluating CIFTI extension to %s\n",
              opts->fout ? opts->fout : "DEFAULT" );

   fp = open_write_stream(opts->fout);
   axml_set_wstream(fp);

   if( opts->verb > 1 ) fprintf(stderr, "-- recursive eval from %s\n",
                                opts->eval_type ? opts->eval_type : "NULL");
   axml_set_verb(opts->verb);

   if( axio_text_to_binary(ax) )
      fprintf(stderr,"** errors converting text to data\n");

   if( ! opts->eval_type )
      axml_recur(ax_show_names, ax);
   else if( ! strcmp(opts->eval_type, "has_data" ) )
      axml_recur(ax_has_data, ax);
   else if( ! strcmp(opts->eval_type, "has_bdata" ) )
      axml_recur(ax_has_bdata, ax);
   else if( ! strcmp(opts->eval_type, "num_tokens" ) )
      axml_recur(ax_num_tokens, ax);
   else if( ! strcmp(opts->eval_type, "show" ) )
      axml_disp_xml_t("CIFTI extension ", ax, 0, opts->verb);
   else if( ! strcmp(opts->eval_type, "show_summary" ) )
      show_cifti_summary(fp, ax, opts->verb);
   else if( ! strcmp(opts->eval_type, "show_text_data" ) )
      axml_recur(ax_show_text_data, ax);
   else /* show_names is default */
      axml_recur(ax_show_names, ax);

   /* possibly close file */
   close_stream(fp);

   return 0;
}

/* look for stdin/stdout */
FILE * open_write_stream(char * fname)
{
   FILE * fp = NULL;

   if      ( ! fname )                   fp = stdout;
   else if ( ! strcmp(fname, "stdout") ) fp = stdout;
   else if ( ! strcmp(fname, "-")      ) fp = stdout;
   else if ( ! strcmp(fname, "stderr") ) fp = stderr;
   else {
      fp = fopen(fname, "w");
      if( !fp ) fprintf(stderr,"** failed to open '%s' for writing\n", fname);
   }

   return fp;
}

/* look for stdin/stdout (do not close them) */
int close_stream(FILE * fp)
{
   if( !fp ) return 1;

   if( fp == stdin || fp == stdout || fp == stderr ) return 0;

   fclose(fp);

   return 0;
}

int show_cifti_summary(FILE * fp, afni_xml_t * ax, int verb)
{
   if( !ax || !fp ) return 1;
   return axio_show_mim_summary(fp, "CIFTI extension summary\n", ax, verb);
}


int ax_has_data(FILE * fp, afni_xml_t * ax, int depth)
{
   if( !ax ) return 1;

   /* if no data (xtext or bdata), blow out of here */
   if( !ax->xtext && ax->xlen <= 0  && !ax->bdata && ax->blen <= 0 ) return 0;

   if( gopt.verb > 2 ) {
      fprintf(fp,"%*sdata in depth %d %s : ", depth*3, "", depth, ax->name);
      fprintf(fp,"xtext[%d], bdata[%" PRId64 "]\n", ax->xlen, ax->blen);
   } else if( gopt.verb > 1 )
      fprintf(fp,"%*sdata in depth %d %s\n", depth*3, "", depth, ax->name);
   else
      fprintf(fp,"%s\n", ax->name);

   return 0;
}

int ax_has_bdata(FILE * fp, afni_xml_t * ax, int depth)
{
   if( !ax ) return 1;

   /* if no bdata blow out of here */
   if( ! ax->bdata && ax->blen <= 0 ) return 0;

   if( gopt.verb > 1 ) fprintf(fp,"%*sdata in depth %d ", depth*3, "", depth);
   fprintf(fp, "%s : bdata[%" PRId64 "]", ax->name, ax->blen);

   if( gopt.verb > 2 && ax->blen > 1 ) {
      if( ax->btype == NIFTI_TYPE_FLOAT64 ) {
         double * dp = (double *)ax->bdata;
         if(dp) { /* repetetive checks to appease compilers */
           fprintf(fp, " = %lf  %lf  ...\n", dp[0], dp[1]);
         }
      } else if( ax->btype == NIFTI_TYPE_INT64 ) {
         int64_t * dp = (int64_t *)ax->bdata;
         if(dp) {
           fprintf(fp, " = %" PRId64 "  %" PRId64 "  ...\n", dp[0], dp[1]);
         }
      }
   } else fputc('\n', fp);

   return 0;
}

int ax_num_tokens(FILE * fp, afni_xml_t * ax, int depth)
{
   int64_t nt;

   if( !ax ) return 1;

   /* if no data, blow out of here */
   if( gopt.verb < 3 ) {
      if( !ax->xtext || ax->xlen <= 0 ) return 0;
   }

   nt = axio_num_tokens(ax->xtext, ax->xlen);

   if( gopt.verb > 1 )
      fprintf(fp,"%*stokens in depth %d %s: %" PRId64 "\n",
              depth*3, "", depth, ax->name, nt);
   else
      fprintf(fp,"%s %" PRId64 "\n", ax->name, nt);

   return 0;
}

int ax_show_text_data(FILE * fp, afni_xml_t * ax, int depth)
{
   int    len;

   if( !ax ) return 1;

   /* if no data, blow out of here */
   if( !ax->xtext || ax->xlen <= 0 ) return 0;

   if( gopt.verb > 1 )
      fprintf(fp,"%*sdata in depth %d %s : ", depth*3, "", depth, ax->name);
   else
      fprintf(fp,"%s : ", ax->name);

   /* first choose max length to display */
   if( gopt.verb > 2 )       len = 128;
   else if ( gopt.verb > 1 ) len = 64;
   else                      len = 32;

   /* restrict to actual length */
   if( len > ax->xlen ) len = ax->xlen;

   /* display */
   fprintf(fp, "%.*s\n", len, ax->xtext);

   return 0;
}

int ax_show_names(FILE * fp, afni_xml_t * ax, int depth)
{
   if( !ax ) return 1;

   if( gopt.verb > 1 ) fprintf(fp,"%*s%s\n", depth*3, "", ax->name);
   else                fprintf(fp,"%s\n", ax->name);

   return 0;
}

/*----------------------------------------------------------------------
 *! display data in hexidecimal, on one line
 *
 *  if mesg is set, print the message first
 *  if fp is not set, print to stdout
*//*-------------------------------------------------------------------*/
int disp_hex_data(const char *mesg, const void *data, int len, FILE *fp)
{
    const char * dp = (const char *)data;
    FILE       * stream;
    int          c;

    stream = fp ? fp : stdout;

    if( !data || len < 1 ) return -1;

    if( mesg ) fputs(mesg, stream);

    for( c = 0; c < len; c++ )
        fprintf(stream, " %02x", dp[c]);

    return 0;
}


int show_hist( void )
{
   int c, len = sizeof(g_history)/sizeof(char *);
   for( c = 0; c < len; c++)
      fputs(g_history[c], stdout);
   putchar('\n');
   return 0;
}


int show_help( void )
{
   printf(
      "ct : short example of reading/writing CIFTI-2 datasets\n"
      "\n"
      "    This program is to demonstrate how to read a CIFTI-2 dataset.\n"
      "\n"
      "    basic usage: cifti_tool -input FILE [other options]\n"
      "\n"
      "    examples:\n"
      "\n"
      "       cifti_tool -input FILE -disp_cext\n"
      "       cifti_tool -input FILE -disp_cext -as_cext\n"
      "       cifti_tool -input FILE -disp_cext -output cifti.txt\n"
      "\n"
      "       cifti_tool -input FILE -eval_cext\n"
      "       cifti_tool -input FILE -eval_cext -verb 2\n"
      "       cifti_tool -input FILE -eval_cext -eval_type show_summary\n"
      "\n"
      "       cifti_tool -input FILE -eval_cext -eval_type show_name\n"
      "       cifti_tool -input FILE -eval_cext -eval_type has_data\n"
      "       cifti_tool -input FILE -eval_cext -eval_type show_text_data\n"
      "\n"
      "    get a list of unique element types with attached data\n"
      "\n"
      "       cifti_tool -input FILE -eval_cext -eval_type has_data \\\n"
      "                  | sort | uniq\n"
      "\n"
      "    options:\n"
      "\n"
      "       -help               : show this help\n"
      "\n"
      "       -input  INFILE      : specify input dataset\n"
      "       -output OUTFILE     : where to write output\n"
      "\n"
      "       -as_cext            : process the input as just an extension\n"
      "       -disp_cext          : display the CIFTI extension\n"
      "       -eval_cext          : evaluate the CIFTI extension\n"
      "       -eval_type ETYPE    : method for evaluation of axml elements\n"
      "\n"
      "          valid ETYPES:\n"
      "             has_data       - show elements with attached text data\n"
      "             has_bdata      - show elements with attached binary data\n"
      "             num_tokens     - show the number of tokens in such text\n"
      "             show           - like -disp_cext\n"
      "             show_names     - show element names, maybe depth indented\n"
      "             show_summary   - summarize contents of dataset\n"
      "             show_text_data - show the actual text data\n"
      "\n"
      "       -verb LEVEL         : set the verbose level to LEVEL\n"
      "       -verb_read LEVEL    : set verbose level when reading\n"
      "       -vboth LEVEL        : apply both -verb options\n"
      "\n");
   return 1;
}
