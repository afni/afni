/*----------------------------------------------------------------------
 * nifti_tool    - a tool for nifti file perusal and manipualation
 *
 * 
 * usage:
 *    nifti_tool [options] -infiles files...
 *
 * Via this tool, one should be able to:
 * 
 *     (ready?)
 *       no  - display the contents of a nifti_image
 *       no  - display the contents of a nifti1_header
 *       no  - display the contents of any field of a nifti_image
 *       no  - display the contents of any field of a nifti1_header
 *
 *       no  - do a diff on two nifti_image structs
 *       no  - do a diff on two nifti1_header structs
 *
 *       no  - modify any single field of a nifti_image
 *       no  - modify any single field of a nifti1_struct
 *             (would use a -prefix option)
 *       no  - add an AFNI extension
 *    
 *       no  - debug level defaults to 0 (since operations are given)
 *    
 * usage forms:
 *
 *   nt -help
 *   nt -hist
 *   nt -ver
 *   nt -nifti_help
 *   nt -nifti_ver
 *
 *   nt -disp_nhdr [-field fieldname] [...] -infiles f1 ...
 *   nt -disp_nim  [-field fieldname] [...] -infiles f1 ...

 *   nt -diff_nhdr [-field fieldname] [...] -infiles f1 f2
 *   nt -diff_nim  [-field fieldname] [...] -infiles f1 f2
 *  
 *   nt -mod_nhdr  [-mod_field fieldname new_val] [...] -infiles f1 ...
 *   nt -mod_nim   [-mod_field fieldname new_val] [...] -infiles f1 ...
 *  
 *   nt -add_AFNI_ext "extension in quotes" -infiles f1 ...
 *  
 *  
 * author: Rick Reynolds, NIH, 2004/5
 *----------------------------------------------------------------------*/

/* three globals: modification history, version string, and debug level */

static char g_history[] =
  "----------------------------------------------------------------------\n"
  "nifti_tool modification history:\n"
  "\n"
  "0.1  30 December 2004 [rickr]\n"
  "     (Rick Reynolds of the National Institutes of Health, SSCC/DIRP/NIMH)\n"
  "   - skeleton version: options read and printed\n"
  "----------------------------------------------------------------------\n";
static char g_version[] = "nifti_tool, version 0.1 (December 30, 2004)";
static int  g_debug = 1;

#define _NIFTI_TOOL_C_
#include "nifti1_io.h"
#include "nifti_tool.h"

int main( int argc, char * argv[] )
{
   nt_opts opts;
   int     rv;

   if( (rv = process_opts(argc, argv, &opts)) != 0)
      return rv;

   return 0;
}

/*----------------------------------------------------------------------
 * process user options, return 0 on success
 *----------------------------------------------------------------------*/
int process_opts( int argc, char * argv[], nt_opts * opts )
{
   int ac;

   memset(opts, 0, sizeof(*opts));

   if( argc < 2 ) return usage(argv[0], USE_SHORT);

   /* quick options are first, the rest are sorted */
   for( ac = 1; ac < argc; ac++ )
   {
      if( ! strncmp(argv[ac], "-help", 5) )
         return usage(argv[0], USE_FULL);
      else if( ! strncmp(argv[ac], "-hist", 5) )
         return usage(argv[0], USE_HIST);
      else if( ! strncmp(argv[ac], "-ver", 2) )
         return usage(argv[0], USE_VERSION);
      else if( ! strncmp(argv[ac], "-nifti_hist", 11) )
      {
         nifti_disp_lib_hist();
         return 1;
      }
      else if( ! strncmp(argv[ac], "-nifti_ver", 10) )
      {
         nifti_disp_lib_version();
         return 1;
      }

      /* begin normal execution options... */

      else if( ! strncmp(argv[ac], "-add_AFNI_ext", 6) )
      {
         ac++;
         CHECK_NEXT_OPT(ac, argc, "-add_AFNI_ext");
         if( add_string(&opts->elist, argv[ac]) ) return 1; /* add extension */
      }
      else if( ! strncmp(argv[ac], "-debug", 6) )
      {
         ac++;
         CHECK_NEXT_OPT(ac, argc, "-add_AFNI_ext");
         g_debug = opts->debug = atoi(argv[ac]);
      }
      else if( ! strncmp(argv[ac], "-diff_nhdr", 8) )
         opts->diff_nhdr = 1;
      else if( ! strncmp(argv[ac], "-diff_nim", 8) )
         opts->diff_nim = 1;
      else if( ! strncmp(argv[ac], "-disp_nhdr", 8) )
         opts->disp_nhdr = 1;
      else if( ! strncmp(argv[ac], "-disp_nim", 8) )
         opts->disp_nim = 1;
      else if( ! strncmp(argv[ac], "-field", 2) )
      {
         ac++;
         CHECK_NEXT_OPT(ac, argc, "-field");
         if( add_string(&opts->flist, argv[ac]) ) return 1; /* add field */
      }
      else if( ! strncmp(argv[ac], "-infiles", 3) )
      {
         int count;
         /* for -infiles, get all next arguments until a '-' or done */
         ac++;
         for( count = 0; (ac < argc) && (argv[ac][0] != '-'); ac++, count++ )
            if( add_string(&opts->infiles, argv[ac]) ) return 1; /* add field */
         if( count > 1 && ac < argc ) ac--;  /* more options to process */
         if( g_debug > 2 ) fprintf(stderr,"+d have %d file names\n", count);
      }
      else if( ! strncmp(argv[ac], "-mod_field", 6) )
      {
         ac++;
         CHECK_NEXT_OPT(ac, argc, "-mod_field");
         if( add_string(&opts->flist, argv[ac]) ) return 1; /* add field */
         ac++;
         CHECK_NEXT_OPT(ac, argc, "-mod_field (2)");
         if( add_string(&opts->vlist, argv[ac]) ) return 1; /* add value */
      }
      else if( ! strncmp(argv[ac], "-mod_nhdr", 7) )
         opts->mod_nhdr = 1;
      else if( ! strncmp(argv[ac], "-mod_nim", 7) )
         opts->mod_nim = 1;
      else
      {
         fprintf(stderr,"** unknown option: '%s'\n", argv[ac]);
         return 1;
      }
   }

   if( g_debug > 2 ) disp_nt_opts("options read: ", opts);

   return 0;
}

/*----------------------------------------------------------------------
 * - do not duplicate the string
 * - only bother to alloc one pointer at a time (don't need efficiency here)
 * - return 0 on success
 *----------------------------------------------------------------------*/
int add_string(str_list * slist, char * str)
{
   if( slist->len == 0 ) slist->list = NULL;  /* just to be safe */
   slist->len++;
   slist->list = (char **)realloc(slist->list,slist->len*sizeof(char *));
   if( ! slist->list ){
      fprintf(stderr,"** failed to alloc %d (char *) elements\n",slist->len);
      return -1;
   }

   slist->list[slist->len-1] = str;

   return 0;
}


/*----------------------------------------------------------------------
 * display information on using the program
 *----------------------------------------------------------------------*/
int usage(char * prog, int level)
{
   if( level == USE_SHORT )
   {
      fprintf(stderr,"usage %s [options] -infiles files...", prog);
      fprintf(stderr,"usage %s -help\n", prog);
   }
   else if( level == USE_FULL )
   {
      fprintf(stderr,"usage %s [options] -infiles files...", prog);
   }
   else if( level == USE_HIST )
      fputs(g_history, stderr);
   else if( level == USE_VERSION )
      fprintf(stderr, "%s\n", g_version);
   else
      fprintf(stderr,"** illegal level for usage(): %d\n", level);

   return 1;
}


/*----------------------------------------------------------------------
 * display the contents of the struct and all lists
 *----------------------------------------------------------------------*/
int disp_nt_opts(char * mesg, nt_opts * opts)
{
   int c;

   if( mesg ) fputs(mesg, stderr);
   if( ! opts )
   {
      fprintf(stderr,"** disp_nt_opts: missing opts\n");
      return -1;
   }

   fprintf(stderr,"nt_opts @ %p\n"
                  "   diff_nhdr, diff_nim  = %d, %d\n"
                  "   disp_nhdr, disp_nim  = %d, %d\n"
                  "   mod_nhdr,  mod_nim   = %d, %d\n"
                  "   debug                = %d\n",
            opts,
            opts->diff_nhdr, opts->diff_nim, opts->disp_nhdr, opts->disp_nim,
            opts->mod_nhdr,  opts->mod_nim, opts->debug);

   fprintf(stderr,"   elist (%d elements)   :\n", opts->elist.len);
   for( c = 0; c < opts->elist.len; c++ )
       fprintf(stderr,"      %d : %s\n", c, opts->elist.list[c]);

   fprintf(stderr,"   flist (%d elements)   :\n", opts->flist.len);
   for( c = 0; c < opts->flist.len; c++ )
       fprintf(stderr,"      %d : %s\n", c, opts->flist.list[c]);

   fprintf(stderr,"   vlist (%d elements)   :\n", opts->vlist.len);
   for( c = 0; c < opts->vlist.len; c++ )
       fprintf(stderr,"      %d : %s\n", c, opts->vlist.list[c]);

   fprintf(stderr,"   infiles (%d elements) :\n", opts->infiles.len);
   for( c = 0; c < opts->infiles.len; c++ )
       fprintf(stderr,"      %d : %s\n", c, opts->infiles.list[c]);

   return 0;
}

