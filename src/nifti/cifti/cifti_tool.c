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

#define USE_NIFTI2
#include <nifti2_io.h>
#include "gifti_io.h"

/* ----------------------------------------------------------------- */
/* define and declare main option struct */
typedef struct {
   char * fin;
   char * fout;
   int    verb;
   int    as_cext;
   int    eval_cext;
   int    show_cext;
} opts_t;

opts_t gopt;


/* ----------------------------------------------------------------- */
/* protos */
int disp_cifti_extension (nifti_image * nim, char * fout);
int eval_cifti_buf       (char * buf, long long blen);
int eval_cext_file       (char * fin);
int eval_cifti_extension (nifti_image * nim);
int process_args         (int argc, char * argv[], opts_t * opts);
int process              (opts_t * opts);
int show_help            (void);
int write_cifti_extension(FILE * fp, nifti1_extension * ext, int maxlen);

/* ----------------------------------------------------------------- */
int main(int argc, char * argv[])
{
   int rv;

   memset(&gopt, 0, sizeof(gopt));
   gopt.verb = 1;

   rv = process_args(argc, argv, &gopt);
   if( rv < 0 ) return 1;  /* error */
   if( rv > 0 ) return 0;  /* non-error termination */

   /* rv == 0, continue... */
   return process(&gopt);

   return 0;
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
      }
      else if( ! strcmp(argv[ac], "-as_cext") ||
               ! strcmp(argv[ac], "-as_cifti_ext") ) {
         opts->as_cext = 1;
      }
      else if( ! strcmp(argv[ac], "-input") ) {
         if( ++ac >= argc ) {
            fprintf(stderr, "** missing argument for -input\n");
            return 1;
         }
         opts->fin = argv[ac];  /* no string copy, just pointer assignment */
      }
      else if( ! strcmp(argv[ac], "-output") ) {
         if( ++ac >= argc ) {
            fprintf(stderr, "** missing argument for -output\n");
            return 2;
         }
         opts->fout = argv[ac];
      }
      else if( ! strcmp(argv[ac], "-verb") ) {
         if( ++ac >= argc ) {
            fprintf(stderr, "** missing argument for -verb\n");
            return 2;
         }
         opts->verb = atoi(argv[ac]);
         nifti_set_debug_level(opts->verb);
         gifti_set_verb(opts->verb);
      }
      else {
         fprintf(stderr,"** invalid option, '%s'\n", argv[ac]);
         return 1;
      }
   }

   return 0;
}

/* ----------------------------------------------------------------- */
int process(opts_t * opts)
{
   nifti_image * nim;

   if( !opts->fin ){ fprintf(stderr, "** missing option '-input'\n"); return 1;}

   /* maybe the input is pure CIFTI */
   if( opts->as_cext ) return eval_cext_file(opts->fin);

   /* read input dataset, including data */
   nim = nifti_image_read(opts->fin, 1);
   if( !nim ) {
      fprintf(stderr,"** failed to read NIfTI image from '%s'\n", opts->fin);
      return 1;
   }

   if( opts->eval_cext ) eval_cifti_extension(nim);
   if( opts->show_cext ) disp_cifti_extension(nim, opts->fout);

   return 0;
}

int disp_cifti_extension(nifti_image * nim, char * fout)
{
   FILE * fp = NULL;
   int    to_file=0;
   int    ind, found=0;

   if      ( ! fout ) fp = stdout;
   else if ( ! strcmp(fout, "stdout") ) fp = stdout;
   else if ( ! strcmp(fout, "-")      ) fp = stdout;
   else if ( ! strcmp(fout, "stderr") ) fp = stderr;
   else {
      fp = fopen(fout, "w");
      if( !fp ) {
         fprintf(stderr,"** failed to open '%s' for writing\n", fout);
         return 1;
      }
      to_file = 1;
   }
  
   found = 0;
   for( ind = 0; ind < nim->num_ext; ind++ ) {
      if( nim->ext_list[ind].ecode != NIFTI_ECODE_CIFTI ) continue;

      found++;
      if( found > 1 ) fprintf(stderr,"** found CIFTI extension #%d\n", found);

      if( gopt.verb && to_file )
         fprintf(stderr,"++ writing CIFTI exten to %s\n", fout);

      write_cifti_extension(fp, nim->ext_list + ind, -1);
   }

   /* possibly close file */
   if( to_file ) fclose(fp);

   return 0;
}

int eval_cifti_extension(nifti_image * nim)
{
   nifti1_extension * ext;
   int                ind, found, rv;

   found = 0;
   ext = nim->ext_list;
   for( ind = 0; ind < nim->num_ext; ind++, ext++ ) {
      if( ext->ecode != NIFTI_ECODE_CIFTI ) continue;

      found++;
      if( found > 1 ) fprintf(stderr,"** found CIFTI extension #%d\n", found);

      rv = eval_cifti_buf(ext->edata, ext->esize-8);
      if( rv ) {
         fprintf(stderr,"** failure for file %s\n", nim->fname);
         return rv;
      }
   }

   return 0;
}

int eval_cext_file(char * fin)
{
   gifti_image * gim;

   gim = gifti_read_image(fin, 1);
   if( !gim ) {
      fprintf(stderr,"** failed to process CIFTI ext file %s\n", fin);
      return 1;
   }

   return 0;
}

int eval_cifti_buf(char * buf, long long blen)
{
   gifti_image * gim;

   gim = gifti_read_image_buf(buf, blen);
   if( !gim ) {
      fprintf(stderr,"** failed to process CIFTI buffer\n");
      return 1;
   }

   return 0;
}

int write_cifti_extension(FILE * fp, nifti1_extension * ext, int maxlen)
{
   int len;

   if( gopt.verb > 1 )
      fprintf(stderr,"ecode = %d, esize = %d\n", ext->ecode, ext->esize);

   if( ! ext->edata ) { fprintf(fp, "(NULL)\n"); return 0; }

   len = ext->esize-8;
   if( maxlen >= 0 && len > maxlen ) len = maxlen;

   fprintf(fp, "%.*s\n", len, (char *)ext->edata);

   return 0;
}


int show_help( void )
{
   printf(
      "ct : short exmample of reading/writing CIFTI-2 datasets\n"
      "\n"
      "    This program is to demonstrate how to read a CIFTI-2 dataset.\n"
      "\n"
      "    basic usage: \n"
      "\n"
      "    options:\n"
      "\n"
      "       -help               : show this help\n"
      "       -input  INFILE      : specify input dataset\n"
      "       -verb LEVEL         : set the verbose level to LEVEL\n"
      "\n");
   return 1;
}

