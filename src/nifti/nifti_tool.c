/*----------------------------------------------------------------------
 * nifti_tool    - a tool for nifti file perusal and manipulation
 * 
 * usage: nifti_tool [options] -infiles files...
 *
 * Via this tool, one should be able to:
 * 
 *       - display the contents of a nifti_image (or various fields)
 *       - display the contents of a nifti1_header (or various fields)
 *       - display AFNI extensions (they are text)
 *
 *       - do a diff on two nifti_image structs (or various fields)
 *       - do a diff on two nifti1_header structs (or various fields)
 *
 *       - add an AFNI extension
 *       - remove any extension
 *
 *       - modify any field(s) of a nifti_image
 *       - modify any field(s) of a nifti1_struct
 *    
 * usage forms:
 *
 *   nifti_tool -help
 *   nifti_tool -help_hdr
 *   nifti_tool -help_nim
 *   nifti_tool -hist
 *   nifti_tool -ver
 *   nifti_tool -nifti_hist
 *   nifti_tool -nifti_ver
 *
 *   nifti_tool -disp_exts -infiles f1 ...
 *   nifti_tool -disp_hdr [-field fieldname] [...] -infiles f1 ...
 *   nifti_tool -disp_nim [-field fieldname] [...] -infiles f1 ...
 *
 *   nifti_tool -diff_hdr [-field fieldname] [...] -infiles f1 f2
 *   nifti_tool -diff_nim [-field fieldname] [...] -infiles f1 f2
 *  
 *   nifti_tool -add_afni_ext "extension in quotes" -infiles f1 ...
 *   nifti_tool -rm_ext ext_index -infiles f1 ...
 *  
 *   nifti_tool -mod_hdr  [-mod_field fieldname new_val] [...] -infiles f1 ...
 *   nifti_tool -mod_nim  [-mod_field fieldname new_val] [...] -infiles f1 ...
 *  
 * author: Rick Reynolds, NIH, January 2005
 *----------------------------------------------------------------------*/

static char g_history[] =
  "----------------------------------------------------------------------\n"
  "nifti_tool modification history:\n"
  "\n"
  "0.1  30 December 2004 [rickr]\n"
  "     (Rick Reynolds of the National Institutes of Health, SSCC/DIRP/NIMH)\n"
  "   - skeleton version: options read and printed\n"
  "\n"
  "1.0  07 January 2005 [rickr]\n"
  "   - initial release version\n"
  "\n"
  "1.1  14 January 2005 [rickr]\n"
  "   - changed all non-error/non-debug output from stderr to stdout\n"
  "       note: creates a mis-match between normal output and debug messages\n"
  "   - modified act_diff_hdrs and act_diff_nims to do the processing in\n"
  "       lower-level functions\n"
  "   - added functions diff_hdrs, diff_hdrs_list, diff_nims, diff_nims_list\n"
  "   - added function get_field, to return a struct pointer via a fieldname\n"
  "   - made 'quiet' output more quiet (no description on output)\n"
  "   - made hdr and nim_fields arrays global, so do not pass in main()\n"
  "   - return (from main()) after first act_diff() difference\n"
  "----------------------------------------------------------------------\n";
static char g_version[] = "version 1.1 (January 14, 2004)";
static int  g_debug = 1;

#define _NIFTI_TOOL_C_
#include "nifti1_io.h"
#include "nifti_tool.h"

/* these are effectively constant, and are built only for verification */
field_s g_hdr_fields[NT_HDR_NUM_FIELDS];    /* nifti_1_header fields */
field_s g_nim_fields [NT_NIM_NUM_FIELDS];   /* nifti_image fields    */

int main( int argc, char * argv[] )
{
   nt_opts opts;
   int     rv;

   if( (rv = process_opts(argc, argv, &opts)) != 0)
      return rv;

   if( (rv = fill_hdr_field_array(g_hdr_fields)) != 0 )
      return rv;

   if( (rv = fill_nim_field_array(g_nim_fields)) != 0 )
      return rv;

   if( (rv = verify_opts(&opts, argv[0])) != 0 )
      return rv;

   /* now perform the requested action(s) */

   /* perform modifications first, in case we allow multiple actions */
   if( opts.add_exts  && ((rv = act_add_exts (&opts)) != 0) ) return rv;
   if( opts.rm_exts   && ((rv = act_rm_ext   (&opts)) != 0) ) return rv;

   if( opts.mod_hdr   && ((rv = act_mod_hdrs (&opts)) != 0) ) return rv;
   if( opts.mod_nim   && ((rv = act_mod_nims (&opts)) != 0) ) return rv;

   /* if a diff, return wither a difference exists (like the UNIX command) */
   if( opts.diff_hdr  && ((rv = act_diff_hdrs(&opts)) != 0) ) return rv;
   if( opts.diff_nim  && ((rv = act_diff_nims(&opts)) != 0) ) return rv;

   /* last action type is display */
   if( opts.disp_exts && ((rv = act_disp_exts(&opts)) != 0) ) return rv;
   if( opts.disp_hdr  && ((rv = act_disp_hdrs(&opts)) != 0) ) return rv;
   if( opts.disp_nim  && ((rv = act_disp_nims(&opts)) != 0) ) return rv;

   return 0;
}

/*----------------------------------------------------------------------
 * process user options, return 0 on success
 *----------------------------------------------------------------------*/
int process_opts( int argc, char * argv[], nt_opts * opts )
{
   int ac;

   memset(opts, 0, sizeof(*opts));

   opts->prefix = NULL;
   opts->debug = 1;  /* init debug level to basic output */

   if( argc < 2 ) return usage(argv[0], USE_SHORT);

   /* terminal options are first, the rest are sorted */
   for( ac = 1; ac < argc; ac++ )
   {
      if( ! strncmp(argv[ac], "-help_hdr", 9) )
         return usage(argv[0], USE_FIELD_HDR);
      if( ! strncmp(argv[ac], "-help_nim", 9) )
         return usage(argv[0], USE_FIELD_NIM);
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
      else if( ! strncmp(argv[ac], "-add_afni_ext", 6) )
      {
         ac++;
         CHECK_NEXT_OPT(ac, argc, "-add_afni_ext");
         if( add_string(&opts->elist, argv[ac]) ) return 1; /* add extension */
         opts->add_exts = 1;
      }
      else if( ! strncmp(argv[ac], "-debug", 6) )
      {
         ac++;
         CHECK_NEXT_OPT(ac, argc, "-debug");
         opts->debug = atoi(argv[ac]);
      }
      else if( ! strncmp(argv[ac], "-diff_hdr", 8) )
         opts->diff_hdr = 1;
      else if( ! strncmp(argv[ac], "-diff_nim", 8) )
         opts->diff_nim = 1;
      else if( ! strncmp(argv[ac], "-disp_exts", 7) )
         opts->disp_exts = 1;
      else if( ! strncmp(argv[ac], "-disp_hdr", 8) )
         opts->disp_hdr = 1;
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
         if( count > 0 && ac < argc ) ac--;  /* more options to process */
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
      else if( ! strncmp(argv[ac], "-mod_hdr", 7) )
         opts->mod_hdr = 1;
      else if( ! strncmp(argv[ac], "-mod_nim", 7) )
         opts->mod_nim = 1;
      else if( ! strncmp(argv[ac], "-overwrite", 6) )
         opts->overwrite = 1;
      else if( ! strncmp(argv[ac], "-prefix", 4) )
      {
         ac++;
         CHECK_NEXT_OPT(ac, argc, "-prefix");
         opts->prefix = argv[ac];
      }
      else if( ! strncmp(argv[ac], "-quiet", 3) )
         opts->debug = 0;
      else if( ! strncmp(argv[ac], "-rm_ext", 7) )
      {
         int index;
         ac++;
         CHECK_NEXT_OPT(ac, argc, "-rm_ext");
         if( ((index = atoi(argv[ac])) < 0) || (index > 1000) ||
             !isdigit(*argv[ac]) ){
            fprintf(stderr,
                    "** '-rm_ext' requires an extension index (read '%s')\n",
                    argv[ac]);
            return 1;
         }
         if( add_string(&opts->elist, argv[ac]) ) return 1;
         opts->rm_exts = 1;
      }
      else
      {
         fprintf(stderr,"** unknown option: '%s'\n", argv[ac]);
         return 1;
      }
   }

   g_debug = opts->debug;
   nifti_set_debug_level(g_debug);

   if( g_debug > 2 ) disp_nt_opts("options read: ", opts);

   return 0;
}


/*----------------------------------------------------------------------
 * verify that the options make sense
 *----------------------------------------------------------------------*/
int verify_opts( nt_opts * opts, char * prog )
{
   int ac, errs = 0;   /* number of requested action types */

   /* check that only one of disp, diff, mod or add_afni_ext is used */
   ac =  (opts->diff_hdr || opts->diff_nim)  ? 1 : 0;
   ac += (opts->disp_hdr || opts->disp_nim 
                         || opts->disp_exts) ? 1 : 0;
   ac += (opts->mod_hdr  || opts->mod_nim)   ? 1 : 0;
   ac += (opts->add_exts || opts->rm_exts)   ? 1 : 0;

   if( ac < 1 )
   {
      fprintf(stderr,
              "** no action option, so nothing to do...\n"
              "   (try one of '-add...', '-diff...', '-disp...' or '-mod...')\n"
              "   (see '%s -help' for details)\n", prog);
      return 1;
   }
   else if( ac > 1 )
   {
      fprintf(stderr,
         "** only one action option is allowed, please use only one of:\n"
         "   '-add_...', '-diff_...', '-disp_...' or '-mod_...'\n"
         "   (see '%s -help' for details)\n", prog);
      return 1;
   }

   /* can modify nifti_1_header or nifti_image, but not both */
   if( opts->mod_hdr && opts->mod_nim )
   {
      fprintf(stderr,"** cannot use both '-mod_hdr' and '-mod_nim'\n");
      return 1;
   }

   /* can add or remove extensions, but not both */
   if( opts->add_exts && opts->rm_exts )
   {
      fprintf(stderr,"** cannot use both '-add_afni_ext' and '-rm_ext'\n");
      return 1;
   }
   if( (opts->add_exts || opts->rm_exts) && opts->elist.len <= 0 )
   {
      fprintf(stderr,"** missing extensions to add or remove\n");
      return 1;
   }
   if( opts->rm_exts && opts->elist.len > 1 )
   {
      fprintf(stderr,"** sorry, you may remove only 1 extension at a time\n");
      return 1;
   }

   /* if modify, then we need fields and corresponding values */
   if( opts->mod_hdr || opts->mod_nim )
   {
      if( opts->flist.len <= 0 )
      {
         fprintf(stderr,"** missing field to modify (need '-mod_field' opt)\n");
         return 1;
      }
      if( opts->flist.len != opts->vlist.len )
      {
         fprintf(stderr,"** error: modifying %d fields with %d values\n",
                 opts->flist.len, opts->vlist.len);
         return 1;
      }
   }

   /* verify the number of files given for each of 4 action types */

   /* -diff_... : require nfiles == 2 */
   if( opts->diff_hdr || opts->diff_nim )
   {
     if( opts->infiles.len != 2 )
     {
      fprintf(stderr,"** '-diff_XXX' options require exactly 2 inputs files\n");
      return 1;
     }
   }
   /* if we are making changes, but not overwriting... */
   else if( (opts->elist.len > 0 || opts->mod_hdr || opts->mod_nim) &&
            !opts->overwrite )
   {
      if( opts->infiles.len > 1 )
      {
         fprintf(stderr,"** without -overwrite, only one input file may be"
                          " modified at a time\n");
         errs++;
      }
      if( ! opts->prefix )
      {
         fprintf(stderr,"** missing -prefix for output file\n");
         errs++;
      }
   }

   if( opts->infiles.len <= 0 ) /* in any case */
   {
      fprintf(stderr,"** missing input files (see -infiles option)\n");
      errs++;
   }

   if ( opts->overwrite && opts->prefix )
   {
      fprintf(stderr, "** please specify only one of -prefix and -overwrite\n");
      errs++;
   }

   if( errs ) return 1;

   if( g_debug > 1 ) fprintf(stderr,"+d options seem valid\n");

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
      fprintf(stdout,"usage %s [options] -infiles files...\n", prog);
      fprintf(stdout,"usage %s -help\n", prog);
   }
   else if( level == USE_FULL )
      use_full("nifti_tool");  /* let's not allow paths in here */
   else if( level == USE_HIST )
      fputs(g_history, stdout);
   else if( level == USE_FIELD_HDR )
   {
      field_s nhdr_fields[NT_HDR_NUM_FIELDS];  /* just do it all here */

      fill_hdr_field_array(nhdr_fields);
      disp_field_s_list("nifti_1_header: ", nhdr_fields, NT_HDR_NUM_FIELDS);
   }
   else if( level == USE_FIELD_NIM )
   {
      field_s nim_fields[NT_NIM_NUM_FIELDS];

      fill_nim_field_array(nim_fields);
      disp_field_s_list("nifti_image: ", nim_fields, NT_NIM_NUM_FIELDS);
   }
   else if( level == USE_VERSION )
      fprintf(stdout, "%s, %s\n", prog, g_version);
   else
      fprintf(stdout,"** illegal level for usage(): %d\n", level);

   return 1;
}


/*----------------------------------------------------------------------
 * full usage
 *----------------------------------------------------------------------*/
int use_full(char * prog)
{
   printf(
   "%s - display, modify or compare nifti structures in datasets\n"
   "\n"
   "  This program can be used to display information from nifti datasets,\n"
   "  to modify information in nifti datasets, or to look for differences\n"
   "  between two nifti datasets (like the UNIX 'diff' command).\n"
   "\n"
   "  Only one action type is allowed, e.g. one cannot modify a dataset\n"
   "  and then take a 'diff'.\n"
   "\n"
   "  one can display - any or all fields in the nifti_1_header structure\n"
   "                  - any or all fields in the nifti_image structure\n"
   "                  - the extensions in the nifti_image structure\n"
   "\n"
   "  one can modify  - any or all fields in the nifti_1_header structure\n"
   "                  - any or all fields in the nifti_image structure\n"
   "          add/rm  - any or all extensions in the nifti_image structure\n"
   "\n"
   "  one can compare - any or all field pairs of nifti_1_header structures\n"
   "                  - any or all field pairs of nifti_image structures\n"
   "\n"
   "\n"
   "  Note: to learn about which fields exist in either of the structures,\n"
   "        or to learn a field's type, size of each element, or the number\n"
   "        of elements in the field, use either the '-help_hdr' option, or\n"
   "        the '-help_nim' option.  No further options are required.\n"
   "  ------------------------------\n"
   "\n"
   "  usage styles:\n"
   "\n"
   "    %s [options] -infiles files...\n"
   "\n"
   "    %s -help                 : show this help\n"
   "    %s -help_hdr             : show nifti_1_header field info\n"
   "    %s -help_nim             : show nifti_image field info\n"
   "\n"
   "    %s -ver                  : show the current version\n"
   "    %s -hist                 : show the modification history\n"
   "    %s -nifti_ver            : show the nifti library version\n"
   "    %s -nifti_hist           : show the nifti library history\n"
   "\n"
   "    %s -disp_hdr [-field FIELDNAME] [...] -infiles f1 ...\n"
   "    %s -disp_nim [-field FIELDNAME] [...] -infiles f1 ...\n"
   "    %s -disp_exts -infiles f1 ...\n"
   "\n"
   "    %s -mod_hdr  [-mod_field FIELDNAME NEW_VAL] [...] -infiles f1 ...\n"
   "    %s -mod_nim  [-mod_field FIELDNAME NEW_VAL] [...] -infiles f1 ...\n"
   "\n"
   "    %s -add_afni_ext 'extension in quotes' [...] -infiles f1 ...\n"
   "    %s -rm_ext INDEX [...] -infiles f1 ...\n"
   "\n"
   "    %s -diff_hdr [-field FIELDNAME] [...] -infiles f1 f2\n"
   "    %s -diff_nim [-field FIELDNAME] [...] -infiles f1 f2\n"
   "\n"
   "  ------------------------------\n",
   prog, prog, prog, prog, prog, prog,    /* 1 + 17, so far */
   prog, prog, prog, prog, prog, prog,
   prog, prog, prog, prog, prog, prog);

   printf(
   "\n"
   "  options for display actions:\n"
   "\n"
   "    -disp_hdr          : display nifti_1_header fields for datasets\n"
   "\n"
   "       This flag means the user wishes to see some of the nifti_1_header\n"
   "       fields in one or more nifti datasets. The user may want to specify\n"
   "       mutliple '-field' options along with this.  This option requires\n"
   "       one or more files input, via '-infiles'.\n"
   "\n"
   "       If no '-field' option is present, all fields will be displayed.\n"
   "\n"
   "       e.g. to display the contents of all fields:\n"
   "       %s -disp_hdr -infiles dset0.nii\n"
   "       %s -disp_hdr -infiles dset0.nii dset1.nii dset2.nii\n"
   "\n"
   "       e.g. to display the contents of select fields:\n"
   "       %s -disp_hdr -field dim -infiles dset0.nii\n"
   "       %s -disp_hdr -field dim -field descrip -infiles dset0.nii\n"
   "\n"
   "    -disp_nim          : display nifti_image fields for datasets\n"
   "\n"
   "       This flag option works the same way as the '-disp_hdr' option,\n"
   "       except that the fields in question are from the nifti_image\n"
   "       structure.\n"
   "\n"
   "    -disp_exts         : display all AFNI-type extensions\n"
   "\n"
   "       This flag option is used to display all nifti_1_extension data,\n"
   "       for only those extensions of type AFNI (code = 4).  The only\n"
   "       other option used will be '-infiles'.\n"
   "\n"
   "       e.g. to display the extensions in datasets:\n"
   "       %s -disp_exts -infiles dset0.nii\n"
   "       %s -disp_exts -infiles dset0.nii dset1.nii dset2.nii\n"
   "\n"
   "  ------------------------------\n",
   prog, prog, prog, prog, prog, prog );

   printf(
   "\n"
   "  options for modification actions:\n"
   "\n"
   "    -mod_hdr           : modify nifti_1_header fields for datasets\n"
   "\n"
   "       This flag is used to modify some of the nifti_1_header fields in\n"
   "       one or more datasets.  The user must specify a list of fields to\n"
   "       modify via one or more '-mod_field' options, which include field\n"
   "       names, along with the new (set of) values.\n"
   "\n"
   "       The user can modify a dataset in place, or use '-prefix' to\n"
   "       produce a new dataset, to which the changes have been applied.\n"
   "       It is recommended to normally use the '-prefix' option, so as not\n"
   "       to ruin a dataset.\n"
   "\n"
   "       Note that some fields have a length greater than 1, meaning that\n"
   "       the field is an array of numbers, or a string of characters.  In\n"
   "       order to modify an array of numbers, the user must provide the\n"
   "       correct number of values, and contain those values in quotes, so\n"
   "       that they are seen as a single option.\n"
   "\n"
   "       To modify a string field, put the string in quotes.\n"
   "\n"
   "       The '-mod_field' option takes a field_name and a list of values.\n"
   "\n"
   "\n"
   "       e.g. to modify the contents of various fields:\n"
   "       %s -mod_hdr -prefix dnew -infiles dset0.nii  \\\n"
   "                  -mod_field qoffset_x -17.325\n"
   "       %s -mod_hdr -prefix dnew -infiles dset0.nii  \\\n"
   "                  -mod_field dim '4 64 64 20 30 1 1 1 1'\n"
   "       %s -mod_hdr -prefix dnew -infiles dset0.nii  \\\n"
   "                  -mod_field descrip 'beer, brats and cheese, mmmmm...'\n"
   "\n"
   "       e.g. to modify the contents of multiple fields:\n"
   "       %s -mod_hdr -prefix dnew -infiles dset0.nii  \\\n"
   "                  -mod_field qoffset_x -17.325 -mod_field slice_start 1\n"
   "\n"
   "       e.g. to modify the contents of multiple files (must overwrite):\n"
   "       %s -mod_hdr -overwrite -mod_field qoffset_x -17.325   \\\n"
   "                  -infiles dset0.nii dset1.nii\n"
   "\n"
   "    -mod_nim          : modify nifti_image fields for datasets\n"
   "\n"
   "       This flag option is used the same way that '-mod_hdr' is used,\n"
   "       except that the fields in question are from the nifti_image\n"
   "       structure.\n"
   "\n"
   "    -add_afni_ext EXT : add an AFNI extension to the dataset\n"
   "\n"
   "       This option is used to add AFNI-type extensions to one or more\n"
   "       datasets.  This option may be used more than once to add more than\n"
   "       one extension.\n"
   "\n"
   "       The '-prefix' option is recommended, to create a new dataset.\n"
   "       In such a case, only a single file may be taken as input.  Using\n"
   "       '-overwrite' allows the user to overwrite the current file, or\n"
   "       to add the extension(s) to multiple files, overwriting them.\n"
   "\n"
   "       e.g. to modify the contents of various fields:\n"
   "       %s -add_afni_ext 'wow, my first extension :)' -prefix dnew \\\n"
   "                  -infiles dset0.nii\n"
   "\n"
   "       e.g. to modify the contents of various fields:\n"
   "       %s -add_afni_ext 'wow, my first extension :)'      \\\n"
   "                  -add_afni-ext 'look, my second...'              \\\n"
   "                  -prefix dnew -infiles dset0.nii\n"
   "\n"
   "       e.g. to modify the contents of various fields:\n"
   "       %s -add_afni_ext 'some AFNI extension' -overwrite \\\n"
   "                  -infiles dset0.nii dset1.nii \n"
   "\n"
   "    -rm_ext INDEX     : remove the extension given by INDEX\n"
   "\n"
   "       This option is used to remove any single extension from the\n"
   "       dataset.  Only one extension may be removed per program execution.\n"
   "\n"
   "       notes  - extension indices begin with 0 (zero)\n"
   "              - to view the current extensions, see '-disp_exts'\n"
   "\n"
   "       e.g. to remove the extension #0:\n"
   "       %s -rm_ext 0 -overwrite -infiles dset0.nii\n"
   "\n"
   "  ------------------------------\n",
   prog, prog, prog, prog, prog,
   prog, prog, prog, prog);

   printf(
   "\n"
   "  options for showing differences:\n"
   "\n"
   "    -diff_hdr         : display header field diffs between two datasets\n"
   "\n"
   "       This option is used to find differences between two datasets.\n"
   "       If any fields are different, the contents of those fields is\n"
   "       displayed (unless the '-quiet' option is used).\n"
   "\n"
   "       A list of fields can be specified by using multiple '-field'\n"
   "       options.  If no '-field' option is given, all fields will be\n"
   "       checked.\n"
   "\n"
   "       Exactly two dataset names must be provided via '-infiles'.\n"
   "\n"
   "       e.g. to display all nifti_1_header field differences:\n"
   "       %s -diff_hdr -infiles dset0.nii dset1.nii\n"
   "\n"
   "       e.g. to display selected nifti_1_header field differences:\n"
   "       %s -diff_hdr -field dim -field intent_code  \\\n"
   "                  -infiles dset0.nii dset1.nii \n"
   "\n"
   "    -diff_nim         : display nifti_image field diffs between datasets\n"
   "\n"
   "       This option works the same as '-diff_hdr', except that the fields\n"
   "       in question are from the nifti_image structure.\n"
   "\n"
   "  ------------------------------\n",
   prog, prog );

   printf(
   "\n"
   "  miscellaneous options:\n"
   "\n"
   "    -debug LEVEL      : set the debugging level\n"
   "\n"
   "       Level 0 will attempt to operate with no screen output, but errors.\n"
   "       Level 1 is the default.\n"
   "       Levels 2 and 3 give progressively more infomation.\n"
   "\n"
   "       e.g. -debug 2\n"
   "\n"
   "    -field FIELDNAME  : provide a field to work with\n"
   "\n"
   "       This option is used to provide a field to display, modify or\n"
   "       compare.  This option can be used along with one of the action\n"
   "       options presented above.\n"
   "\n"
   "       See '-disp_hdr', above, for complete examples.\n"
   "\n"
   "       e.g. %s -field descrip\n"
   "       e.g. %s -field descrip -field dim\n"
   "\n"
   "    -infiles file0... : provide a list of files to work with\n"
   "\n"
   "       This parameter is required for any of the actions, in order to\n"
   "       provide a list of files to process.  If input filenames do not\n"
   "       have an extension, the directory we be searched for any\n"
   "       appropriate files (such as .nii or .hdr).\n"
   "\n"
   "       See '-mod_hdr', above, for complete examples.\n"
   "\n"
   "       e.g. %s -infiles file0.nii\n"
   "       e.g. %s -infiles file1.nii file2 file3.hdr\n"
   "\n"
   "    -mod_field NAME 'VALUE_LIST' : provide new values for a field\n"
   "\n"
   "       This parameter is required for any the modification actions.\n"
   "       If the user wants to modify any fields of a dataset, this is\n"
   "       where the fields and values are specified.\n"
   "\n"
   "       NAME is a field name (in either the nifti_1_header structure or\n"
   "       the nifti_image structure).  If the action option is '-mod_hdr',\n"
   "       then NAME must be the name of a nifti_1_header field.  If the\n"
   "       action is '-mod_nim', NAME must be from a nifti_image structure.\n"
   "\n"
   "       VALUE_LIST must be one or more values, as many as are required\n"
   "       for the field, contained in quotes if more than one is provided.\n"
   "\n"
   "       Use '%s -help_hdr' to get a list of nifti_1_header fields\n"
   "       Use '%s -help_nim' to get a list of nifti_image fields\n"
   "\n"
   "       See '-mod_hdr', above, for complete examples.\n"
   "\n"
   "       e.g. modifying nifti_1_header fields:\n"
   "            -mod_field descrip 'toga, toga, toga'\n"
   "            -mod_field qoffset_x 19.4 -mod_field qoffset_z -11\n"
   "            -mod_field pixdim '1 0.9375 0.9375 1.2 1 1 1 1'\n"
   "\n"
   "    -overwrite        : any modifications will be made to input files\n"
   "\n"
   "       This option is used so that all field modifications, including\n"
   "       extension additions or deletions, will be made to the files that\n"
   "       are input.\n"
   "\n"
   "       In general, the user is recommended to use the '-prefix' option\n"
   "       to create new files.  But if overwriting the contents of the\n"
   "       input files is prefered, this is how to do it.\n"
   "\n"
   "       See '-mod_hdr' or '-add_afni_ext', above, for complete examples.\n"
   "\n"
   "       e.g. -overwrite\n"
   "\n"
   "    -prefix           : specify an output file to write change into\n"
   "\n"
   "       This option is used to specify an output file to write, after\n"
   "       modifications have been made.  If modifications are being made,\n"
   "       then either '-prefix' or '-overwrite' is required.\n"
   "\n"
   "       If no extension is given, the output extension will be '.nii'.\n"
   "\n"
   "       e.g. -prefix new_dset\n"
   "       e.g. -prefix new_dset.nii\n"
   "       e.g. -prefix new_dset.hdr\n"
   "\n"
   "    -quiet            : report only errors or requested information\n"
   "\n"
   "       This option is equivalent to '-debug 0'.\n"
   "\n"
   "  ------------------------------\n",
   prog, prog, prog, prog, prog, prog );

   printf(
   "\n"
   "  basic help options:\n"
   "\n"
   "    -help             : show this help\n"
   "\n"
   "       e.g.  %s -help\n"
   "\n"
   "    -help_hdr         : show nifti_1_header field info\n"
   "\n"
   "       e.g.  %s -help_hdr\n"
   "\n"
   "    -help_nim         : show nifti_image field info\n"
   "\n"
   "       e.g.  %s -help_nim\n"
   "\n"
   "    -ver              : show the program version number\n"
   "\n"
   "       e.g.  %s -ver\n"
   "\n"
   "    -hist             : show the program modification history\n"
   "\n"
   "       e.g.  %s -hist\n"
   "\n"
   "    -nifti_ver        : show the nifti library version number\n"
   "\n"
   "       e.g.  %s -nifti_ver\n"
   "\n"
   "    -nifti_hist       : show the nifti library modification history\n"
   "\n"
   "       e.g.  %s -nifti_hist\n"
   "\n"
   "  ------------------------------\n"
   "\n"
   "  R. Reynolds\n"
   "  compiled: %s\n"
   "  %s\n\n",
   prog, prog, prog, prog, prog, prog, prog, __DATE__, g_version );

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
                  "   diff_hdr, diff_nim  = %d, %d\n"
                  "   disp_hdr, disp_nim  = %d, %d\n"
                  "   disp_exts           = %d\n"
                  "   add_exts, rm_exts   = %d, %d\n"
                  "   mod_hdr,  mod_nim   = %d, %d\n"
                  "   debug, overwrite    = %d, %d\n"
                  "   prefix              = '%s'\n",
            opts,
            opts->diff_hdr, opts->diff_nim, opts->disp_hdr, opts->disp_nim,
            opts->disp_exts, opts->add_exts, opts->rm_exts,
            opts->mod_hdr,  opts->mod_nim, opts->debug, opts->overwrite,
            opts->prefix ? opts->prefix : "(NULL)" );

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


/*----------------------------------------------------------------------
 * For each file, add all extensions with type NIFTI_ECODE_AFNI.
 * Though it should not matter, copy the trailing null characters.
 *----------------------------------------------------------------------*/
int act_add_exts( nt_opts * opts )
{
   nifti1_extension   ext;
   nifti_image      * nim;
   int                fc, ec, slen;

   if( g_debug > 2 )
      fprintf(stderr,"+d adding %d extensions to %d files...\n",
              opts->elist.len, opts->infiles.len);

   if( opts->prefix && opts->infiles.len != 1 ){
      fprintf(stderr,"** error: we have a prefix but %d files\n",
              opts->infiles.len);
      return 1;
   }

   if( opts->elist.len <= 0 ) return 0;

   ext.ecode = NIFTI_ECODE_AFNI;

   for( fc = 0; fc < opts->infiles.len; fc++ )
   {
      nim = nifti_image_read( opts->infiles.list[fc], 1 );
      if( !nim ) return 1;  /* errors come from the library */

      for( ec = 0; ec < opts->elist.len; ec++ ){
         slen = strlen(opts->elist.list[ec]);  /* need multiple of 16 */
         if( slen & 0xf ){
            if( g_debug > 2 ) 
               fprintf(stderr,"+d add_ext #%d, changing size from %d to %d\n",
                       ec, slen, (slen + 0xf) & ~0xf);
            slen = (slen + 0xf) & ~0xf;
         }

         ext.esize = slen;
         ext.edata = (char *)calloc(slen, sizeof(char));
         if( ext.edata ) strncpy(ext.edata, opts->elist.list[ec], slen);
         if( ext.esize <= 0 || ! ext.edata ){
            fprintf(stderr,"** failed to dup %d byte extension, '%s'\n",
                    ext.esize, opts->elist.list[ec]);
         }
         nim->num_ext++;  /* and insert */
         if( nifti_add_exten_to_list(&ext, &nim->ext_list, nim->num_ext) ) {
            nifti_image_free(nim);
            return 1;     /* failure */
         }
      }

      if( opts->prefix &&
          nifti_set_filenames(nim,opts->prefix,!opts->overwrite) ){
         nifti_image_free(nim);
         return 1;
      }

      if( g_debug > 1 )
         fprintf(stderr,"+d writing %s with %d new extension(s)\n",
                 opts->infiles.list[fc], opts->elist.len);

      nifti_image_write(nim);
      nifti_image_free(nim);
   }

   if( g_debug > 0 )
      fprintf(stderr,"+d added %d extension(s) to %d files\n",
              opts->elist.len, opts->infiles.len);

   return 0;
}


/*----------------------------------------------------------------------
 * For each file, remove the given extension for the given indices.
 *----------------------------------------------------------------------*/
int act_rm_ext( nt_opts * opts )
{
   nifti_image      * nim;
   int                fc, ec, ext_ind;

   if( g_debug > 2 )
      fprintf(stderr,"+d removing %d extensions to %d files...\n",
              opts->elist.len, opts->infiles.len);

   if( opts->elist.len <= 0 ) return 0;

   if( opts->prefix && opts->infiles.len != 1 ){
      fprintf(stderr,"** error: we have a prefix but %d files\n",
              opts->infiles.len);
      return 1;
   }

   if( opts->elist.len != 1 ){
      fprintf(stderr,"** error: only 1 extension may be removed at a time\n");
      return 1;
   }

   ext_ind = atoi(opts->elist.list[0]);
   if( ext_ind < 0 ){
      fprintf(stderr,"** bad extension index to remove: %d\n", ext_ind);
      return 1;
   }

   if( g_debug > 1 ) fprintf(stderr,"+d removing extension index %d\n",ext_ind);

   for( fc = 0; fc < opts->infiles.len; fc++ )
   {
      nim = nifti_image_read( opts->infiles.list[fc], 1 );
      if( !nim ) return 1;  /* errors come from the library */

      if( ext_ind >= nim->num_ext ){
         fprintf(stderr,"** extension index %d out of range [0,%d] for %s\n",
                 ext_ind, nim->num_ext-1, opts->infiles.list[fc]);
         nifti_image_free(nim);
         return 1;
      }
      else if( g_debug > 1 )
         disp_nifti1_extension("+d removing ext: ", nim->ext_list + ext_ind);

      /* now squeeze all remaining extensions, and decrement num_ext */
      for( ec = ext_ind; ec < nim->num_ext - 1; ec++ )
         nim->ext_list[ec] = nim->ext_list[ec+1];
      nim->num_ext--;

      if( nim->num_ext == 0 ){  /* did we trash the only extension? */
         free(nim->ext_list);
         nim->ext_list = NULL;
      }

      if( opts->prefix &&
          nifti_set_filenames(nim,opts->prefix,!opts->overwrite) ){
         nifti_image_free(nim);
         return 1;
      }

      if( g_debug > 1 )
         fprintf(stderr,"+d writing %s with %d fewer extension(s)\n",
                 opts->infiles.list[fc], opts->elist.len);

      nifti_image_write(nim);
      nifti_image_free(nim);
   }

   if( g_debug > 0 )
      fprintf(stderr,"+d removed %d extension(s) from %d files\n",
              opts->elist.len, opts->infiles.len);

   return 0;
}


/*----------------------------------------------------------------------
 * check for diffs between all fields in opts->flist, or in the
 * entire nifti_1_header
 *
 * if quiet mode (debug == 0) return on first diff
 *
 * return: 1 if diffs exist, 0 otherwise
 *----------------------------------------------------------------------*/
int act_diff_hdrs( nt_opts * opts )
{
   nifti_1_header * nhdr0, * nhdr1;
   int              diffs = 0;

   if( opts->infiles.len != 2 ){
      fprintf(stderr,"** -diff_hdr requires 2 -infiles, have %d\n",
              opts->infiles.len);
      return 1;
   }

   if( g_debug > 2 )
      fprintf(stderr,"-d nifti_1_header diff between %s and %s...\n",
              opts->infiles.list[0], opts->infiles.list[1]);

   /* get the nifiti headers */

   nhdr0 = nifti_read_header(opts->infiles.list[0], NULL);
   if( ! nhdr0 ) return 1;  /* errors have been printed */

   nhdr1 = nifti_read_header(opts->infiles.list[1], NULL);
   if( ! nhdr1 ){ free(nhdr0); return 1; }

   if( g_debug > 1 )
      fprintf(stderr,"\n-d nifti_1_header diffs between '%s' and '%s'...\n",
              opts->infiles.list[0], opts->infiles.list[1]);

   if( opts->flist.len <= 0 )
      diffs = diff_hdrs(nhdr0, nhdr1, g_debug > 0);
   else
      diffs = diff_hdrs_list(nhdr0, nhdr1, &opts->flist, g_debug > 0);

   if( diffs == 0 && g_debug > 1 )
      fprintf(stderr,"+d no differences found\n");
   else if ( g_debug > 2 )
      fprintf(stderr,"+d %d differences found\n", diffs);

   free(nhdr0);
   free(nhdr1);

   return (diffs > 0);
}


/*----------------------------------------------------------------------
 * check for diffs between all fields in opts->flist, or in the
 * entire nifti_image
 *
 * if quiet mode (debug == 0) return on first diff
 *
 * return: 1 if diffs exist, 0 otherwise
 *----------------------------------------------------------------------*/
int act_diff_nims( nt_opts * opts )
{
   nifti_image * nim0, * nim1;
   int           diffs = 0;

   if( opts->infiles.len != 2 ){
      fprintf(stderr,"** -diff_nim requires 2 -infiles, have %d\n",
              opts->infiles.len);
      return 1;
   }

   if( g_debug > 2 )
      fprintf(stderr,"-d nifti_image diff between %s and %s...\n",
              opts->infiles.list[0], opts->infiles.list[1]);

   /* get the nifiti images */

   nim0 = nifti_image_read(opts->infiles.list[0], 0);
   if( ! nim0 ) return 1;  /* errors have been printed */

   nim1 = nifti_image_read(opts->infiles.list[1], 0);
   if( ! nim1 ){ free(nim0); return 1; }

   if( g_debug > 1 )
      fprintf(stderr,"\n-d nifti_image diffs between '%s' and '%s'...\n",
              opts->infiles.list[0], opts->infiles.list[1]);

   if( opts->flist.len <= 0 )
      diffs = diff_nims(nim0, nim1, g_debug > 0);
   else
      diffs = diff_nims_list(nim0, nim1, &opts->flist, g_debug > 0);

   if( diffs == 0 && g_debug > 1 )
      fprintf(stderr,"+d no differences found\n");
   else if ( g_debug > 2 )
      fprintf(stderr,"+d %d differences found\n", diffs);

   nifti_image_free(nim0);
   nifti_image_free(nim1);

   return (diffs > 0);
}


/*----------------------------------------------------------------------
 * display all extensions for each dataset
 *----------------------------------------------------------------------*/
int act_disp_exts( nt_opts * opts )
{
   nifti_image * nim;
   char          mesg[32];
   int           ec, fc;

   if( g_debug > 2 )
      fprintf(stderr,"-d displaying all extensions for %d files...\n",
              opts->infiles.len);

   for( fc = 0; fc < opts->infiles.len; fc++ )
   {
      nim = nifti_image_read(opts->infiles.list[fc], 0);
      if( !nim ) return 1;  /* errors are printed from library */

      if( g_debug > 0 )
         fprintf(stdout,"header file '%s', num_ext = %d\n",
                 nim->fname, nim->num_ext);
      for( ec = 0; ec < nim->num_ext; ec++ )
      {
         sprintf(mesg, "    ext #%d : ", ec);
         disp_nifti1_extension(mesg, nim->ext_list + ec);
      }

      nifti_image_free(nim);
   }

   return 0;
}


/*----------------------------------------------------------------------
 * for each file, read nifti1_header and display all fields
 *----------------------------------------------------------------------*/
int act_disp_hdrs( nt_opts * opts )
{
   nifti_1_header *  nhdr;
   field_s        *  fnhdr;
   char           ** sptr;
   int               nfields, filenum, fc;

   /* set the number of fields to display */
   nfields = opts->flist.len > 0 ? opts->flist.len : NT_HDR_NUM_FIELDS;

   if( g_debug > 2 )
      fprintf(stderr,"-d displaying %d fields for %d nifti datasets...\n",
              nfields, opts->infiles.len);

   for( filenum = 0; filenum < opts->infiles.len; filenum++ )
   {
      nhdr = nifti_read_header(opts->infiles.list[filenum], NULL);
      if( !nhdr ) return 1;  /* errors are printed from library */
                                                                                
      if( g_debug > 0 )
         fprintf(stdout,"\nheader file '%s', num_fields = %d, fields:\n\n",
                 opts->infiles.list[filenum], nfields);

      if( opts->flist.len <= 0 ) /* then display all fields */
         disp_field("all fields:\n", g_hdr_fields, nhdr, nfields, g_debug > 0);
      else  /* print only the requested fields... */
      {
         /* must locate each field before printing it */
         sptr = opts->flist.list;
         for( fc = 0; fc < opts->flist.len; fc++ )
         {
            fnhdr = get_hdr_field(*sptr, filenum == 0);
            if( fnhdr ) disp_field(NULL, fnhdr, nhdr, 1, g_debug>0 && fc == 0);
            sptr++;
         }
      }

      free(nhdr);
   }

   return 0;
}


/*----------------------------------------------------------------------
 * for each file, get nifti_image and display all fields
 *----------------------------------------------------------------------*/
int act_disp_nims( nt_opts * opts )
{
   nifti_image *  nim;
   field_s     *  fnim;
   char        ** sptr;
   int            nfields, filenum, fc;

   /* set the number of fields to display */
   nfields = opts->flist.len > 0 ? opts->flist.len : NT_NIM_NUM_FIELDS;

   if( g_debug > 2 )
      fprintf(stderr,"-d displaying %d fields for %d nifti datasets...\n",
              nfields, opts->infiles.len);

   for( filenum = 0; filenum < opts->infiles.len; filenum++ )
   {
      nim = nifti_image_read(opts->infiles.list[filenum], 0);
      if( !nim ) return 1;  /* errors are printed from library */
                                                                                
      if( g_debug > 0 )
         fprintf(stdout,"\nheader file '%s', num_fields = %d, fields:\n\n",
                 nim->fname, nfields);

      if( opts->flist.len <= 0 ) /* then display all fields */
         disp_field("all fields:\n", g_nim_fields, nim, nfields, g_debug > 0);
      else  /* print only the requested fields... */
      {
         /* must locate each field before printing it */
         sptr = opts->flist.list;
         for( fc = 0; fc < opts->flist.len; fc++ )
         {
            fnim = get_nim_field(*sptr, filenum == 0);
            if( fnim ) disp_field(NULL, fnim, nim, 1, g_debug > 0 && fc == 0);
            sptr++;
         }
      }

      nifti_image_free(nim);
   }

   return 0;
}


/*----------------------------------------------------------------------
 * - read header
 * - modify header
 * - if -prefix duplicate file
 * - else if swapped, swap back
 * - overwrite file header      (allows (danger-of) no evaluation of data)
 *----------------------------------------------------------------------*/
int act_mod_hdrs( nt_opts * opts )
{
   nifti_1_header * nhdr;
   nifti_image    * nim;         /* for reading/writing entire datasets */
   int              filec, swap;
   char           * fname, * dupname;
   char             func[] = { "act_mod_hdrs" };
 
   if( g_debug > 2 )
      fprintf(stderr,"-d modifying %d fields for %d nifti headers...\n",
              opts->flist.len, opts->infiles.len);
   if( opts->flist.len <= 0 || opts->infiles.len <= 0 ) return 0;

   for( filec = 0; filec < opts->infiles.len; filec++ )
   {
      nhdr = nifti_read_header(opts->infiles.list[filec], &swap);
      if( !nhdr ) return 1;

      if( g_debug > 1 )
         fprintf(stderr,"-d modifying %d fields of '%s' header\n",
                 opts->flist.len, opts->infiles.list[filec]);

      /* okay, let's actually trash the data fields */
      if( modify_all_fields(nhdr, opts, g_hdr_fields, NT_HDR_NUM_FIELDS) )
      {
         free(nhdr);
         return 1;
      }

      fname = opts->infiles.list[filec];  /* init for modification file */
      dupname = NULL;                     /* unless we duplicate file   */

      /* possibly duplicate the current dataset before writing new header */
      if( opts->prefix )
      {
         nim = nifti_image_read(opts->infiles.list[filec], 1); /* get data */
         if( !nim )
         {
            fprintf(stderr,"** failed to dup file '%s' before modifying\n",
                    opts->infiles.list[filec]);
            return 1;
         }
         if( nifti_set_filenames(nim,opts->prefix,1) )
         {
            LNI_FERR(func,"failed to set prefix for new file: ",opts->prefix);
            nifti_image_free(nim);
            return 1;
         }
         dupname = nifti_strdup(nim->fname);  /* so we know to free it */
         fname = dupname;
         nifti_image_write(nim);  /* create the duplicate file */
         nifti_image_free(nim);
      }
      else if ( swap )
         swap_nifti_header(nhdr, NIFTI_VERSION(*nhdr));

      /* if all is well, overwrite header in fname dataset */
      (void)write_hdr_to_file(nhdr, fname); /* errors printed in function */

      if( dupname ) free(dupname);
      free(nhdr);
   }

   return 0;
}


/*----------------------------------------------------------------------
 * - read image w/data, modify and write
 *----------------------------------------------------------------------*/
int act_mod_nims( nt_opts * opts )
{
   nifti_image    * nim;         /* for reading/writing entire datasets */
   int              filec;
   char             func[] = { "act_mod_nims" };
 
   if( g_debug > 2 )
      fprintf(stderr,"-d modifying %d fields for %d nifti images...\n",
              opts->flist.len, opts->infiles.len);
   if( opts->flist.len <= 0 || opts->infiles.len <= 0 ) return 0;

   for( filec = 0; filec < opts->infiles.len; filec++ )
   {
      nim = nifti_image_read(opts->infiles.list[filec], 1); /* with data */

      if( g_debug > 1 )
         fprintf(stderr,"-d modifying %d fields from '%s' image\n",
                 opts->flist.len, opts->infiles.list[filec]);

      /* okay, let's actually trash the data fields */
      if( modify_all_fields(nim, opts, g_nim_fields, NT_NIM_NUM_FIELDS) )
      {
         nifti_image_free(nim);
         return 1;
      }

      /* possibly duplicate the current dataset before writing new header */
      if( opts->prefix )
         if( nifti_set_filenames(nim,opts->prefix,1) )
         {
            LNI_FERR(func,"failed to set prefix for new file: ",opts->prefix);
            nifti_image_free(nim);
            return 1;
         }

      nifti_image_write(nim);  /* and write it out, piece of cake :) */
      nifti_image_free(nim);
   }

   return 0;
}


/*----------------------------------------------------------------------
 * overwrite nifti_1_header in the given file
 *----------------------------------------------------------------------*/
int write_hdr_to_file( nifti_1_header * nhdr, char * fname )
{
   znzFile fp;
   size_t  bytes;
   char    func[] = { "write_hdr_to_file" };
   int     rv = 0;

   fp = znzopen(fname,"r+b",nifti_is_gzfile(fname));
   if( znz_isnull(fp) ){
      LNI_FERR(func, "failed to re-open mod file", fname);
      return 1;
   }

   bytes = znzwrite(nhdr, 1, sizeof(nifti_1_header), fp);
   if( bytes != sizeof(nifti_1_header)){
      LNI_FERR(func, "failed to write nifti_1_header to file",fname);
      fprintf(stderr,"  - wrote %d of %d bytes\n",
              (int)bytes,(int)sizeof(nifti_1_header));
      rv = 1;
   }

   znzclose(fp);

   return rv;
}


/*----------------------------------------------------------------------
 * modify all fields in the list
 *----------------------------------------------------------------------*/
int modify_all_fields( void * basep, nt_opts * opts, field_s * fields, int flen)
{
   field_s * fp;
   int       fc, lc;  /* field and list counters */

   if( opts->flist.len <= 0 ) return 0;
   if( opts->flist.len != opts->vlist.len ){
      fprintf(stderr,"** have %d fields but %d new values\n",
              opts->flist.len, opts->vlist.len);
      return 1;
   }

   for( lc = 0; lc < opts->flist.len; lc++ )
   {
      /* is it in the list? */
      fp = fields;
      for( fc = 0; fc < flen; fc++, fp++ )
         if( strcmp(opts->flist.list[lc], fp->name) == 0 ) break;

      if( fc == flen )    /* do no modifications on failure */
      {
         fprintf(stderr,"** field '%s' not found in structure\n",
                 opts->flist.list[lc]);
         return 1;
      }

      if( modify_field( basep, fp, opts->vlist.list[lc]) )
         return 1;
   }

   return 0;
}


/*----------------------------------------------------------------------
 * modify a single field with the given value field
 *
 * pointer fields are not allowed here
 *----------------------------------------------------------------------*/
int modify_field(void * basep, field_s * field, char * data)
{
   float   fval;
   char  * posn = data;
   int     val, max, fc, nchars;

   if( g_debug > 1 )
      fprintf(stderr,"+d modifying field '%s' with '%s'\n", field->name, data);

   if( !data || strlen(data) == 0 )
   {
      fprintf(stderr,"** no data for '%s' field modification\n",field->name);
      return 1;
   }

   switch( field->type )
   {
         case DT_UNKNOWN:
         case NT_DT_POINTER:
         case NT_DT_CHAR_PTR:
         case NT_DT_EXT_PTR:
         default:
            fprintf(stderr,"** refusing to modify a pointer field, '%s'\n",
                    field->name);
            return 1;

         case DT_INT8:
         {
            max = 127;
            for( fc = 0; fc < field->len; fc++ )
            {
               if( sscanf(posn, " %d%n", &val, &nchars) != 1 )
               {
                  fprintf(stderr,"** found %d of %d modify values\n",
                          fc,field->len);
                  return 1;
               }
               if( val > max || val < -(max+1) )
               {
                  fprintf(stderr,
                    "** mod val #%d (= %d) outside byte range [-%d,%d]\n",
                    fc, val, max+1, max);
                  return 1;
               }
               /* otherwise, we're good */
               (((char *)basep + field->offset))[fc] = (char)val;
               if( g_debug > 1 )
                  fprintf(stderr,"+d setting posn %d of '%s' to %d\n",
                          fc, field->name, val);
               posn += nchars;
            }
         }
         break;

         case DT_INT16:
         {
            max = 32767;
            for( fc = 0; fc < field->len; fc++ )
            {
               if( sscanf(posn, " %d%n", &val, &nchars) != 1 )
               {
                  fprintf(stderr,"** found %d of %d modify values\n",
                          fc,field->len);
                  return 1;
               }
               if( val > max || val < -(max+1) )
               {
                  fprintf(stderr,
                    "** mod val #%d (= %d) outside byte range [-%d,%d]\n",
                    fc, val, max+1, max);
                  return 1;
               }
               /* otherwise, we're good */
               ((short *)((char *)basep + field->offset))[fc] = (short)val;
               if( g_debug > 1 )
                  fprintf(stderr,"+d setting posn %d of '%s' to %d\n",
                          fc, field->name, val);
               posn += nchars;
            }
         }
         break;

         case DT_INT32:
         {
            for( fc = 0; fc < field->len; fc++ )
            {
               if( sscanf(posn, " %d%n", &val, &nchars) != 1 )
               {
                  fprintf(stderr,"** found %d of %d modify values\n",
                          fc,field->len);
                  return 1;
               }
               ((int *)((char *)basep + field->offset))[fc] = val;
               if( g_debug > 1 )
                  fprintf(stderr,"+d setting posn %d of '%s' to %d\n",
                          fc, field->name, val);
               posn += nchars;
            }
         }
         break;

         case DT_FLOAT32:
         {
            for( fc = 0; fc < field->len; fc++ )
            {
               if( sscanf(posn, " %f%n", &fval, &nchars) != 1 )
               {
                  fprintf(stderr,"** found %d of %d modify values\n",
                          fc,field->len);
                  return 1;
               }
               /* otherwise, we're good */
               ((float *)((char *)basep + field->offset))[fc] = fval;
               if( g_debug > 1 )
                  fprintf(stderr,"+d setting posn %d of '%s' to %f\n",
                          fc, field->name, fval);
               posn += nchars;
            }
         }
         break;

         case NT_DT_STRING:
         {
            char * dest = (char *)basep + field->offset;
            nchars = strlen(data);
            strncpy(dest, data, field->len);
            if( nchars < field->len )  /* clear the rest */
               memset(dest+nchars, '\0', field->len-nchars);
         }
         break;
   }

   return 0;
}


/*----------------------------------------------------------------------
 * fill the nifti_1_header field list
 *----------------------------------------------------------------------*/
int fill_hdr_field_array( field_s * nh_fields )
{
   nifti_1_header   nhdr;
   field_s        * nhf = nh_fields;
   int              rv, errs;

   memset(nhf, 0, NT_HDR_NUM_FIELDS*sizeof(field_s));

   /* this macro takes (TYPE, NAME, NUM) and does:
         fill_field(nhdr, TYPE, NT_OFF(nhdr,NAME), NUM, "NAME");
         nhf++;
   */
   errs = 0;
   NT_SFILL(nhdr, nhf, DT_INT32,     sizeof_hdr,     1, rv);  errs += rv;
   NT_SFILL(nhdr, nhf, NT_DT_STRING, data_type,     10, rv);  errs += rv;
   NT_SFILL(nhdr, nhf, NT_DT_STRING, db_name,       18, rv);  errs += rv;
   NT_SFILL(nhdr, nhf, DT_INT32,     extents,        1, rv);  errs += rv;
   NT_SFILL(nhdr, nhf, DT_INT16,     session_error,  1, rv);  errs += rv;
   NT_SFILL(nhdr, nhf, NT_DT_STRING, regular,        1, rv);  errs += rv;
   NT_SFILL(nhdr, nhf, DT_INT8,      dim_info,       1, rv);  errs += rv;

   NT_SFILL(nhdr, nhf, DT_INT16,     dim,            8, rv);  errs += rv;
   NT_SFILL(nhdr, nhf, DT_FLOAT32,   intent_p1,      1, rv);  errs += rv;
   NT_SFILL(nhdr, nhf, DT_FLOAT32,   intent_p2,      1, rv);  errs += rv;
   NT_SFILL(nhdr, nhf, DT_FLOAT32,   intent_p3,      1, rv);  errs += rv;
   NT_SFILL(nhdr, nhf, DT_INT16,     intent_code,    1, rv);  errs += rv;

   NT_SFILL(nhdr, nhf, DT_INT16,     datatype,       1, rv);  errs += rv;
   NT_SFILL(nhdr, nhf, DT_INT16,     bitpix,         1, rv);  errs += rv;
   NT_SFILL(nhdr, nhf, DT_INT16,     slice_start,    1, rv);  errs += rv;
   NT_SFILL(nhdr, nhf, DT_FLOAT32,   pixdim,         8, rv);  errs += rv;
   NT_SFILL(nhdr, nhf, DT_FLOAT32,   vox_offset,     1, rv);  errs += rv;
   NT_SFILL(nhdr, nhf, DT_FLOAT32,   scl_slope,      1, rv);  errs += rv;
   NT_SFILL(nhdr, nhf, DT_FLOAT32,   scl_inter,      1, rv);  errs += rv;
   NT_SFILL(nhdr, nhf, DT_INT16,     slice_end,      1, rv);  errs += rv;

   NT_SFILL(nhdr, nhf, DT_INT8,      slice_code,     1, rv);  errs += rv;
   NT_SFILL(nhdr, nhf, DT_INT8,      xyzt_units,     1, rv);  errs += rv;
   NT_SFILL(nhdr, nhf, DT_FLOAT32,   cal_max,        1, rv);  errs += rv;
   NT_SFILL(nhdr, nhf, DT_FLOAT32,   cal_min,        1, rv);  errs += rv;
   NT_SFILL(nhdr, nhf, DT_FLOAT32,   slice_duration, 1, rv);  errs += rv;
   NT_SFILL(nhdr, nhf, DT_FLOAT32,   toffset,        1, rv);  errs += rv;
   NT_SFILL(nhdr, nhf, DT_INT32,     glmax,          1, rv);  errs += rv;
   NT_SFILL(nhdr, nhf, DT_INT32,     glmin,          1, rv);  errs += rv;

   NT_SFILL(nhdr, nhf, NT_DT_STRING, descrip,       80, rv);  errs += rv;
   NT_SFILL(nhdr, nhf, NT_DT_STRING, aux_file,      24, rv);  errs += rv;
   NT_SFILL(nhdr, nhf, DT_INT16,     qform_code,     1, rv);  errs += rv;
   NT_SFILL(nhdr, nhf, DT_INT16,     sform_code,     1, rv);  errs += rv;

   NT_SFILL(nhdr, nhf, DT_FLOAT32,   quatern_b,      1, rv);  errs += rv;
   NT_SFILL(nhdr, nhf, DT_FLOAT32,   quatern_c,      1, rv);  errs += rv;
   NT_SFILL(nhdr, nhf, DT_FLOAT32,   quatern_d,      1, rv);  errs += rv;
   NT_SFILL(nhdr, nhf, DT_FLOAT32,   qoffset_x,      1, rv);  errs += rv;
   NT_SFILL(nhdr, nhf, DT_FLOAT32,   qoffset_y,      1, rv);  errs += rv;
   NT_SFILL(nhdr, nhf, DT_FLOAT32,   qoffset_z,      1, rv);  errs += rv;

   NT_SFILL(nhdr, nhf, DT_FLOAT32,   srow_x,         4, rv);  errs += rv;
   NT_SFILL(nhdr, nhf, DT_FLOAT32,   srow_y,         4, rv);  errs += rv;
   NT_SFILL(nhdr, nhf, DT_FLOAT32,   srow_z,         4, rv);  errs += rv;
   NT_SFILL(nhdr, nhf, NT_DT_STRING, intent_name,   16, rv);  errs += rv;
   NT_SFILL(nhdr, nhf, NT_DT_STRING, magic,          4, rv);  errs += rv;

   if( errs > 0 ){
      fprintf(stderr, "** %d fill_fields errors!\n", errs);
      return 1;
   }

   /* failure here is a serious problem */
   if( check_total_size("nifti_1_header test: ", nh_fields,
                        NT_HDR_NUM_FIELDS, sizeof(nhdr)) )
      return 1;

   if( g_debug > 2 )
      disp_field_s_list("nh_fields: ", nh_fields, NT_HDR_NUM_FIELDS);

   return 0;
}


/*----------------------------------------------------------------------
 * fill the nifti_image field list
 *----------------------------------------------------------------------*/
int fill_nim_field_array( field_s * nim_fields )
{
   nifti_image   nim;
   field_s     * nif = nim_fields;
   int           rv, errs;

   memset(nif, 0, NT_NIM_NUM_FIELDS*sizeof(field_s));

   errs = 0;

   NT_SFILL(nim, nif, DT_INT32,             ndim,  1, rv);  errs += rv;
   NT_SFILL(nim, nif, DT_INT32,               nx,  1, rv);  errs += rv;
   NT_SFILL(nim, nif, DT_INT32,               ny,  1, rv);  errs += rv;
   NT_SFILL(nim, nif, DT_INT32,               nz,  1, rv);  errs += rv;
   NT_SFILL(nim, nif, DT_INT32,               nt,  1, rv);  errs += rv;
   NT_SFILL(nim, nif, DT_INT32,               nu,  1, rv);  errs += rv;
   NT_SFILL(nim, nif, DT_INT32,               nv,  1, rv);  errs += rv;
   NT_SFILL(nim, nif, DT_INT32,               nw,  1, rv);  errs += rv;
   NT_SFILL(nim, nif, DT_INT32,              dim,  8, rv);  errs += rv;
   NT_SFILL(nim, nif, DT_INT32,             nvox,  1, rv);  errs += rv;
   NT_SFILL(nim, nif, DT_INT32,           nbyper,  1, rv);  errs += rv;
   NT_SFILL(nim, nif, DT_INT32,         datatype,  1, rv);  errs += rv;
   NT_SFILL(nim, nif, DT_FLOAT32,             dx,  1, rv);  errs += rv;
   NT_SFILL(nim, nif, DT_FLOAT32,             dy,  1, rv);  errs += rv;
   NT_SFILL(nim, nif, DT_FLOAT32,             dz,  1, rv);  errs += rv;
   NT_SFILL(nim, nif, DT_FLOAT32,             dt,  1, rv);  errs += rv;
   NT_SFILL(nim, nif, DT_FLOAT32,             du,  1, rv);  errs += rv;
   NT_SFILL(nim, nif, DT_FLOAT32,             dv,  1, rv);  errs += rv;
   NT_SFILL(nim, nif, DT_FLOAT32,             dw,  1, rv);  errs += rv;
   NT_SFILL(nim, nif, DT_FLOAT32,         pixdim,  8, rv);  errs += rv;
   NT_SFILL(nim, nif, DT_FLOAT32,      scl_slope,  1, rv);  errs += rv;
   NT_SFILL(nim, nif, DT_FLOAT32,      scl_inter,  1, rv);  errs += rv;
   NT_SFILL(nim, nif, DT_FLOAT32,        cal_min,  1, rv);  errs += rv;
   NT_SFILL(nim, nif, DT_FLOAT32,        cal_max,  1, rv);  errs += rv;
   NT_SFILL(nim, nif, DT_INT32,       qform_code,  1, rv);  errs += rv;
   NT_SFILL(nim, nif, DT_INT32,       sform_code,  1, rv);  errs += rv;
   NT_SFILL(nim, nif, DT_INT32,         freq_dim,  1, rv);  errs += rv;
   NT_SFILL(nim, nif, DT_INT32,        phase_dim,  1, rv);  errs += rv;
   NT_SFILL(nim, nif, DT_INT32,        slice_dim,  1, rv);  errs += rv;
   NT_SFILL(nim, nif, DT_INT32,       slice_code,  1, rv);  errs += rv;
   NT_SFILL(nim, nif, DT_INT32,      slice_start,  1, rv);  errs += rv;
   NT_SFILL(nim, nif, DT_INT32,        slice_end,  1, rv);  errs += rv;
   NT_SFILL(nim, nif, DT_FLOAT32, slice_duration,  1, rv);  errs += rv;
   NT_SFILL(nim, nif, DT_FLOAT32,      quatern_b,  1, rv);  errs += rv;
   NT_SFILL(nim, nif, DT_FLOAT32,      quatern_c,  1, rv);  errs += rv;
   NT_SFILL(nim, nif, DT_FLOAT32,      quatern_d,  1, rv);  errs += rv;
   NT_SFILL(nim, nif, DT_FLOAT32,      qoffset_x,  1, rv);  errs += rv;
   NT_SFILL(nim, nif, DT_FLOAT32,      qoffset_y,  1, rv);  errs += rv;
   NT_SFILL(nim, nif, DT_FLOAT32,      qoffset_z,  1, rv);  errs += rv;
   NT_SFILL(nim, nif, DT_FLOAT32,           qfac,  1, rv);  errs += rv;
   NT_SFILL(nim, nif, DT_FLOAT32,        qto_xyz, 16, rv);  errs += rv;
   NT_SFILL(nim, nif, DT_FLOAT32,        qto_ijk, 16, rv);  errs += rv;
   NT_SFILL(nim, nif, DT_FLOAT32,        sto_xyz, 16, rv);  errs += rv;
   NT_SFILL(nim, nif, DT_FLOAT32,        sto_ijk, 16, rv);  errs += rv;
   NT_SFILL(nim, nif, DT_FLOAT32,        toffset,  1, rv);  errs += rv;
   NT_SFILL(nim, nif, DT_INT32,        xyz_units,  1, rv);  errs += rv;
   NT_SFILL(nim, nif, DT_INT32,       time_units,  1, rv);  errs += rv;
   NT_SFILL(nim, nif, DT_INT32,       nifti_type,  1, rv);  errs += rv;
   NT_SFILL(nim, nif, DT_INT32,      intent_code,  1, rv);  errs += rv;
   NT_SFILL(nim, nif, DT_FLOAT32,      intent_p1,  1, rv);  errs += rv;
   NT_SFILL(nim, nif, DT_FLOAT32,      intent_p2,  1, rv);  errs += rv;
   NT_SFILL(nim, nif, DT_FLOAT32,      intent_p3,  1, rv);  errs += rv;
   NT_SFILL(nim, nif, NT_DT_STRING,  intent_name, 16, rv);  errs += rv;
   NT_SFILL(nim, nif, NT_DT_STRING,      descrip, 80, rv);  errs += rv;
   NT_SFILL(nim, nif, NT_DT_STRING,     aux_file, 24, rv);  errs += rv;
   NT_SFILL(nim, nif, NT_DT_CHAR_PTR,      fname,  1, rv);  errs += rv;
   NT_SFILL(nim, nif, NT_DT_CHAR_PTR,      iname,  1, rv);  errs += rv;
   NT_SFILL(nim, nif, DT_INT32,     iname_offset,  1, rv);  errs += rv;
   NT_SFILL(nim, nif, DT_INT32,         swapsize,  1, rv);  errs += rv;
   NT_SFILL(nim, nif, DT_INT32,        byteorder,  1, rv);  errs += rv;
   NT_SFILL(nim, nif, NT_DT_POINTER,        data,  1, rv);  errs += rv;
   NT_SFILL(nim, nif, DT_INT32,          num_ext,  1, rv);  errs += rv;
   NT_SFILL(nim, nif, NT_DT_EXT_PTR,    ext_list,  1, rv);  errs += rv;

   if( errs > 0 ){
      fprintf(stderr, "** %d fill_fields errors "
                      "(note that pointers get aligned)\n", errs);
      return 1;
   }

   if( g_debug > 2 )  /* failure here is not an error condition */
       check_total_size("nifti_image test: ", nim_fields,
                        NT_NIM_NUM_FIELDS, sizeof(nim));

   if( g_debug > 2 )
      disp_field_s_list("nim_fields: ", nim_fields, NT_NIM_NUM_FIELDS);

   return 0;
}


/*----------------------------------------------------------------------
 * compare sizes to offset, including total
 *----------------------------------------------------------------------*/
int check_total_size( char * mesg, field_s * fields, int nfields, int tot_size )
{
   field_s * fp;
   int       c, total;
   int       bad_offs;

   total = 0;
   bad_offs = 0;
   for( c = 0, fp = fields; c < nfields; c++, fp++ ){
      if( fp->offset != total ){
         if( g_debug > 2 )
            fprintf(stderr,"** bad offset for field '%s'\n"
                           "   offset = %d, total = %d\n",
                           fp->name, fp->offset, total);
         bad_offs++;
      }

      total += fp->size * fp->len;
   }

   if( g_debug > 1  || (g_debug > 0 && bad_offs > 0) ){
      fputs(mesg, stderr);  c = 0;
      if( bad_offs > 0 ){
         fprintf(stderr,"** found %d bad offsets\n", bad_offs);  c++; }
      if( total != tot_size ){
         fprintf(stderr,"** computed total %d not equal to struct size %d\n",
                 total, tot_size);   c++; }
      if( c == 0 ) fputs("... okay\n", stderr);
   }

   if( bad_offs > 0 ) return 1;

   return 0;
}


/*----------------------------------------------------------------------
 * fill the field structure with the given data
 *----------------------------------------------------------------------*/
int fill_field( field_s * fp, int type, int offset, int num, char * name )
{
   fp->type   = type;
   fp->offset = offset;
   fp->size   = 1;     /* init before check */
   fp->len    = num;
   
   strncpy(fp->name, name, NT_FIELD_NAME_LEN-1);

   switch( type ){
      case DT_UNKNOWN:
      case DT_INT8:
      case NT_DT_STRING:
         fp->size = 1;
         break;

      case DT_INT16:
         fp->size = 2;
         break;

      case DT_INT32:
      case DT_FLOAT32:
         fp->size = 4;
         break;

      case NT_DT_POINTER:
      case NT_DT_CHAR_PTR:
      case NT_DT_EXT_PTR:
         fp->size = (int)sizeof(void *);
         break;

      default:
         fprintf(stderr,"** fill_field: invalid type %d\n", type );
         return 1;
   }

   return 0;
}


/*----------------------------------------------------------------------
 * display the contents of all of the field structures
 *----------------------------------------------------------------------*/
char * field_type_str( int type )
{
   if( type == DT_INT8 )        return "DT_INT8";
   if( type == DT_INT16 )       return "DT_INT16";
   if( type == DT_INT32 )       return "DT_INT32";
   if( type == DT_FLOAT32 )     return "DT_FLOAT32";
   if( type == NT_DT_STRING )   return "NT_DT_STRING";
   if( type == NT_DT_POINTER )  return "NT_DT_POINTER";
   if( type == NT_DT_CHAR_PTR ) return "NT_DT_CHAR_PTR"; /* longest: 14 */
   if( type == NT_DT_EXT_PTR )  return "NT_DT_EXT_PTR";

   return "DT_UNKNOWN";  /* for DT_UNKNOWN, or as an else */
}

#define NT_MAX_DT_STR_LEN 14

/*----------------------------------------------------------------------
 * display the contents of all of the field structures
 *----------------------------------------------------------------------*/
int disp_field_s_list( char * mesg, field_s * fp, int nfields )
{
   int c;

   if( mesg ) fputs(mesg, stdout);

   fprintf(stdout," %d fields:\n"
           "   name                  size   len   offset   type\n"
           "   -------------------   ----   ---   ------   --------------\n",
           nfields);

   for( c = 0; c < nfields; c++, fp++ )
      fprintf(stdout,"   %-*s  %4d    %3d   %4d     %-14s\n",
                     NT_FIELD_NAME_LEN-1, fp->name, fp->size, fp->len,
                     fp->offset, field_type_str(fp->type));

   return 0;
}


/*----------------------------------------------------------------------
 * display the contents of all of the field structures
 *----------------------------------------------------------------------*/
int disp_field(char *mesg, field_s *fieldp, void * str, int nfields, int header)
{
   field_s * fp;
   float   * floatp;      /* generic data type pointers */
   short   * shortp;
   char    * charp;
   int     * intp;
   int       c, ind;

   if( mesg ) fputs(mesg, stdout);

   if( header && g_debug > 0 ){
      fprintf(stdout, "  name                offset  nvals  values\n");
      fprintf(stdout, "  ------------------- ------  -----  ------\n");
   }

   fp = fieldp;
   for( c = 0; c < nfields; c++, fp++ )
   {
      /* start by displaying the field information */
      if( g_debug > 0 )
         fprintf(stdout, "  %-*.*s %4d    %3d    ",
                      NT_FIELD_NAME_LEN-1, NT_FIELD_NAME_LEN-1, fp->name,
                      fp->offset, fp->len);

      /* now, print the value(s), depending on the type */
      switch( fp->type ){
         case DT_UNKNOWN:
         default:
            fprintf(stdout,"(unknown data type)\n");
            break;

         case DT_INT8:
            charp = (char *)str + fp->offset;
            for (ind = 0; ind < fp->len; ind++, charp++ )
               fprintf(stdout,"%d ", *charp);
            fputc('\n', stdout);
            break;

         case DT_INT16:
            shortp = (short *)((char *)str + fp->offset);
            for (ind = 0; ind < fp->len; ind++, shortp++ )
               fprintf(stdout,"%d ", *shortp);
            fputc('\n', stdout);
            break;

         case DT_FLOAT32:
            floatp = (float *)((char *)str + fp->offset);
            for (ind = 0; ind < fp->len; ind++, floatp++ )
               fprintf(stdout,"%f ", *floatp);
            fputc('\n', stdout);
            break;

         case DT_INT32:
            intp = (int *)((char *)str + fp->offset);
            for (ind = 0; ind < fp->len; ind++, intp++ )
               fprintf(stdout,"%d ", *intp);
            fputc('\n', stdout);
            break;

         case NT_DT_POINTER:
            fprintf(stdout,"(raw data of unknown type)\n");
            break;

         case NT_DT_CHAR_PTR:  /* look for string of length <= 40 */
         {
            char * sp;
            int    len;

            /* start by sucking the pointer stored here */
            sp = *(char **)((char *)str + fp->offset);

            if( ! sp ){ fprintf(stdout,"(NULL)\n");  break; }  /* anything? */

            /* see if we have a printable string here */
            for(len = 0; len <= 40 && *sp && isprint(*sp); len++, sp++ )
               ;
            if( len > 40 )
               fprintf(stdout,"(apparent long string)\n");
            else if ( len == 0 )
               fprintf(stdout,"(empty string)\n");
            else if( !isprint(*sp) )
               fprintf(stdout,"(non-printable string)\n");
            else  /* woohoo!  a good string */
               fprintf(stdout,"'%.40s'\n", (char *)str + fp->offset);
            break;
         }

         case NT_DT_EXT_PTR:
         {
            nifti1_extension * extp;

            /* yank the address sitting there into extp */
            extp = *(nifti1_extension **)((char *)str + fp->offset);

            /* the user may use -disp_exts to display all of them */
            if( extp ) disp_nifti1_extension(NULL, extp);
            else fprintf(stdout,"(NULL)\n");
            break;
         }

         case NT_DT_STRING:
            charp = (char *)str + fp->offset;
            fprintf(stdout,"%.*s\n", fp->len, charp);
            break;
      }
   }

   return 0;
}


/*----------------------------------------------------------------------
 * no display, just return whether any fields differ
 *----------------------------------------------------------------------*/
int diff_field(field_s *fieldp, void * str0, void * str1, int nfields)
{
   field_s * fp;
   char    * cp0, * cp1;
   int       fnum, c, size;

   fp = fieldp;
   for( fnum = 0; fnum < nfields; fnum++, fp++ )
   {
      switch( fp->type ){
         case DT_UNKNOWN:     /* all basic types are easy */
         case DT_INT8:
         case DT_INT16:
         case DT_INT32:
         case DT_FLOAT32:
         case NT_DT_STRING:
            size = fp->size * fp->len;  /* total field size */
            cp0 = (char *)str0 + fp->offset;
            cp1 = (char *)str1 + fp->offset;
            for( c = 0; c < size; c++, cp0++, cp1++ )
               if( *cp0 != *cp1 ) break;

            if(c < size) return 1;  /* found a diff */
  
            break;

         case NT_DT_POINTER:     /* let's pass on these - no diff */
         case NT_DT_CHAR_PTR:
            
            break;

         case NT_DT_EXT_PTR:
         {
            nifti1_extension * ext0, * ext1;

            ext0 = *(nifti1_extension **)((char *)str0 + fp->offset);
            ext1 = *(nifti1_extension **)((char *)str1 + fp->offset);

            if( ! ext0 && ! ext1 ) break;     /* continue on */

            if( ext0 && ! ext1 )   return 1;  /* pointer diff is diff */
            if( ! ext0 && ext1 )   return 1;

            /* just check size and type for a single extension */
            if( ext0->esize != ext1->esize ) return 1;
            if( ext0->ecode != ext1->ecode ) return 1;

            break;
         }
      }
   }

   return 0;   /* no diffs found */
}


/*----------------------------------------------------------------------
 * display a single extension
 *----------------------------------------------------------------------*/
int disp_nifti1_extension(char *mesg, nifti1_extension * ext)
{
   if( mesg ) fputs(mesg, stdout);

   if( !ext )
   {
      fprintf(stderr,"** no extension to display\n");
      return 1;
   }

   fprintf(stdout,"ecode = %d, esize = %d, edata = ",
           ext->ecode, ext->esize);

   if( !ext->edata )
      fprintf(stdout,"(NULL)\n");
   else if ( ext->ecode == NIFTI_ECODE_AFNI )
      fprintf(stdout,"%.*s\n", ext->esize-8, (char *)ext->edata);
   else
      fprintf(stdout,"(unknown data type)\n");

   return 0;
}


/*----------------------------------------------------------------------
 * return the appropritate pointer into the g_hdr_fields struct
 *----------------------------------------------------------------------*/
field_s * get_hdr_field( char * fname, int show_fail )
{
   field_s * fp;
   int       c;

   if( ! fname || *fname == '\0' ) return NULL;

   fp = g_hdr_fields;
   for( c = 0; c < NT_HDR_NUM_FIELDS; c++, fp++ )
      if( strcmp(fname, fp->name) == 0 ) break;

   if( c == NT_HDR_NUM_FIELDS )
   {
      if( show_fail > 0 )
         fprintf(stderr,"** get_hdr_field: field not found in hdr: %s\n",fname);
      return NULL;
   }

   return fp;
}


/*----------------------------------------------------------------------
 * return the appropritate pointer into the g_hdr_fields struct
 *----------------------------------------------------------------------*/
field_s * get_nim_field( char * fname, int show_fail )
{
   field_s * fp;
   int       c;

   if( ! fname || *fname == '\0' ) return NULL;

   fp = g_nim_fields;
   for( c = 0; c < NT_NIM_NUM_FIELDS; c++, fp++ )
      if( strcmp(fname, fp->name) == 0 ) break;

   if( c == NT_NIM_NUM_FIELDS )
   {
      if( show_fail > 0 )
         fprintf(stderr,"** get_nim_field: field not found in hdr: %s\n",fname);
      return NULL;
   }

   return fp;
}


/*----------------------------------------------------------------------
 * return the number of fields that differ
 *----------------------------------------------------------------------*/
int diff_hdrs( nifti_1_header * s0, nifti_1_header * s1, int display )
{
   field_s * fp = g_hdr_fields;
   int       c, ndiff = 0;

   for( c = 0; c < NT_HDR_NUM_FIELDS; c++, fp++ )
      if( diff_field(fp, s0, s1, 1) )
      {
         if( display ) disp_field(NULL, fp, s0, 1, ndiff == 0);
         if( display ) disp_field(NULL, fp, s1, 1, 0);
         ndiff++;
      }

   return ndiff;
}


/*----------------------------------------------------------------------
 * return the number of fields that differ
 *----------------------------------------------------------------------*/
int diff_nims( nifti_image * s0, nifti_image * s1, int display )
{
   field_s * fp = g_nim_fields;
   int       c, ndiff = 0;

   for( c = 0; c < NT_NIM_NUM_FIELDS; c++, fp++ )
      if( diff_field(fp, s0, s1, 1) )
      {
         if( display ) disp_field(NULL, fp, s0, 1, ndiff == 0);
         if( display ) disp_field(NULL, fp, s1, 1, 0);
         ndiff++;
      }

   return ndiff;
}


/*----------------------------------------------------------------------
 * return the number of fields that differ
 *----------------------------------------------------------------------*/
int diff_hdrs_list( nifti_1_header * s0, nifti_1_header * s1, str_list * slist,
                    int display )
{
   field_s  * fp;
   char    ** sptr;
   int        c, ndiff = 0;

   sptr = slist->list;
   for( c = 0; c < slist->len; c++ )
   {
      fp = get_hdr_field(*sptr, 1);    /* "not found" displayed in func */
      if( fp && diff_field(fp, s0, s1, 1) )
      {
         if( display ) disp_field(NULL, fp, s0, 1, ndiff == 0);
         if( display ) disp_field(NULL, fp, s1, 1, 0);
         ndiff++;
      }
      sptr++;
   }

   return ndiff;
}


/*----------------------------------------------------------------------
 * return the number of fields that differ
 *----------------------------------------------------------------------*/
int diff_nims_list( nifti_image * s0, nifti_image * s1, str_list * slist,
                    int display )
{
   field_s  * fp;
   char    ** sptr;
   int        c, ndiff = 0;

   sptr = slist->list;
   for( c = 0; c < slist->len; c++ )
   {
      fp = get_nim_field(*sptr, 1);    /* "not found" displayed in func */
      if( fp && diff_field(fp, s0, s1, 1) )
      {
         if( display ) disp_field(NULL, fp, s0, 1, ndiff == 0);
         if( display ) disp_field(NULL, fp, s1, 1, 0);
         ndiff++;
      }
      sptr++;
   }

   return ndiff;
}


