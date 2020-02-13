/* ----------------------------------------------------------------------
 * A basic example to read/write an XML file via afni_xml.
 *
 * R Reynolds   24 Jun 2015
 *----------------------------------------------------------------------
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "afni_xml.h"

/* ----------------------------------------------------------------- */
/* define and declare main option struct */
typedef struct {
   char * fin;
   char * fout;
   int    disp_xlist;
   int    verb;
   int    xverb;
} opts_t;

opts_t gopt;


/* ----------------------------------------------------------------- */
/* protos */
int process_args         (int argc, char * argv[], opts_t * opts);
int process              (opts_t * opts);
int show_help            (void);

/* ----------------------------------------------------------------- */
int main(int argc, char * argv[])
{
   int rv;

   memset(&gopt, 0, sizeof(gopt));
   gopt.xverb = 1;

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

   /* process user options: 4 are valid presently */
   for( ac = 1; ac < argc; ac++ ) {
      if( ! strncmp(argv[ac], "-h", 2) ) {
         return show_help();
      } else if( ! strcmp(argv[ac], "-disp_xml") ) {
         opts->disp_xlist = 1;
      } else if( ! strcmp(argv[ac], "-input") ) {
         if( ++ac >= argc ) {
            fprintf(stderr, "** missing argument for -input\n");
            return 1;
         }
         opts->fin = argv[ac];  /* no string copy, just pointer assignment */
      } else if( ! strcmp(argv[ac], "-output") ) {
         if( ++ac >= argc ) {
            fprintf(stderr, "** missing argument for -output\n");
            return 2;
         }
         opts->fout = argv[ac];
      } else if( ! strcmp(argv[ac], "-verb") ) {
         if( ++ac >= argc ) {
            fprintf(stderr, "** missing argument for -verb\n");
            return 2;
         }
         opts->verb = atoi(argv[ac]);
      } else if( ! strcmp(argv[ac], "-verb_lib") ) {
         if( ++ac >= argc ) {
            fprintf(stderr, "** missing argument for -verb_lib\n");
            return 2;
         }
         opts->xverb = atoi(argv[ac]);
         axml_set_verb(opts->xverb);
      } else {
         fprintf(stderr,"** invalid option, '%s'\n", argv[ac]);
         return 1;
      }
   }

   return 0;
}

/* ----------------------------------------------------------------- */
int process(opts_t * opts)
{
   afni_xml_list xlist;

   if( !opts->fin ){ fprintf(stderr, "** missing option '-input'\n"); return 1;}

   /* read input dataset, including data */
   xlist = axml_read_file(opts->fin, 1);
   if( ! xlist.len || ! xlist.xlist ) {
      fprintf(stderr,"** failed to read XML data from '%s'\n", opts->fin);
      return 1;
   }

   /* maybe display the results */
   if( opts->disp_xlist )
      axml_disp_xlist(NULL, &xlist, opts->verb);
   else if ( opts->verb > 1 )
      printf("xlist read, nothing to do (but free it)\n");

   /* free everything when we are done */
   axml_free_xlist(&xlist);

   return 0;
}

int show_help( void )
{
   printf(
      "ct : short exmample of reading/writing XML files via afni_xml\n"
      "\n"
      "    basic usage: \n"
      "\n"
      "    options:\n"
      "\n"
      "       -help               : show this help\n"
      "       -input  INFILE      : specify input dataset\n"
      "       -verb LEVEL         : set the verbose level to LEVEL\n"
      "       -verb_lib LEVEL     : set the library verbose level to LEVEL\n"
      "\n");
   return 1;
}
