
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "gifti_io.h"
#include "gifti_tool.h"

static char * g_history[] = 
{
  "----------------------------------------------------------------------\n"
  "history (of gifti_tool):\n"
  "\n",
  "0.0  28 Dec, 2007\n"
  "     (Rick Reynolds of the National Institutes of Health, SSCC/DIRP/NIMH)\n"
  "     - initial version\n"
  "0.1  03 Jan, 2008: changed structure of program\n",
  "     - can do one of display, write or test (more to come)\n"
  "     - added dset creation ability and options, via -new_dset or MAKE_IM\n"
  "         (options -new_*, for numDA, intent, dtype, ndim, dims, data)\n"
  "     - added AFNI-style DA selection, for input datasets\n"
};

static char g_version[] = "gifti_tool version 0.1, 03 January 2008";

/* globals: verbosity, for now */
typedef struct { int verb; } gt_globs;
gt_globs G = { 1 };

/* local prototypes */
static int add_to_int_list(gt_int_list * ilist, int val);
static int add_to_str_list(gt_str_list * slist, char * str);
static int disp_gt_opts(char * mesg, gt_opts * opts, FILE * stream);
static int free_gt_opts(gt_opts * opts);
static int init_opts(gt_opts * opts);
static int show_help(void);
static int show_hist(void);
static int process_opts(int argc, char *argv[], gt_opts * opts);
static int show_version(void);

/* the main event */
int main( int argc, char * argv[] )
{
    gt_opts       opts;
    int           rv;

    init_opts(&opts);
    rv = process_opts(argc, argv, &opts);
    if      ( rv < 0 ) return 1;        /* non-zero means terminate */
    else if ( rv > 0 ) return 0;

    /* choose top-level operation to perform */
    if     ( opts.gt_display ) rv = gt_display(&opts);
    else if( opts.gt_write )   rv = gt_write(&opts);
    else if( opts.gt_compare ) rv = 1; /* gt_compare(&opts); to do */
    else                       rv = gt_test(&opts);

    free_gt_opts(&opts);

    return rv;
}

/* process the user options
 *
 * return  1 : success, but exit program
 *         0 : success, continue
 *        -1 : failure, terminate
*/
static int process_opts(int argc, char *argv[], gt_opts * opts)
{
    int ac, c;

    if( argc <= 1 ) { show_help(); return 1; }

    for( ac = 1; ac < argc; ac++ )
    {
        /* terminal options, verbose, then alphabetical */
        if( !strcmp(argv[ac], "-help") ) {
            show_help();
            return 1;
        } else if( !strcmp(argv[ac], "-hist") ) {
            show_hist();
            return 1;
        } else if( !strcmp(argv[ac], "-ver") ) {
            show_version();
            return 1;
        } else if( !strcmp(argv[ac], "-gifti_hist") ) {
            gifti_disp_lib_hist();
            return 1;
        } else if( !strcmp(argv[ac], "-gifti_ver") ) {
            gifti_disp_lib_version();
            return 1;
        }

        /* do this early, in case it is wanted for other options */
        else if( !strcmp(argv[ac], "-verb") ) {
            ac++;
            CHECK_NEXT_OPT(ac, argc, "-verb");
            opts->verb = atoi(argv[ac]);
            G.verb = opts->verb;
        }

        /* now alphabetical */
        else if( !strcmp(argv[ac], "-b64_check") ) {
            ac++;
            CHECK_NEXT_OPT(ac, argc, "-b64_check");
            if     ( !strcmp(argv[ac], "NONE" ) )
                        opts->b64_check = GIFTI_B64_CHECK_NONE;
            else if( !strcmp(argv[ac], "DETECT") )
                        opts->b64_check = GIFTI_B64_CHECK_DETECT;
            else if( !strcmp(argv[ac], "COUNT") )
                        opts->b64_check = GIFTI_B64_CHECK_COUNT;
            else if( !strcmp(argv[ac], "SKIP" ) )
                        opts->b64_check = GIFTI_B64_CHECK_SKIP;
            else if( !strcmp(argv[ac], "SKIPnCOUNT" ) )
                        opts->b64_check = GIFTI_B64_CHECK_SKIPNCOUNT;
            else {
                fprintf(stderr,"** invalid parm to -b64_check: %s\n",argv[ac]);
                return -1;
            }
        } else if( !strcmp(argv[ac], "-buf_size") ) {
            ac++;
            CHECK_NEXT_OPT(ac, argc, "-buf_size");
            opts->buf_size = atoi(argv[ac]);
        } else if( !strcmp(argv[ac], "-da_list") ) {
            ac++;
            for( c = 0; (ac < argc) && (argv[ac][0] != '-'); ac++, c++ )
               if( add_to_int_list(&opts->DAlist, atoi(argv[ac])) ) return -1;
            if( G.verb > 1 )
                fprintf(stderr,"+d have %d DA indices names\n", c);
            if( opts->DAlist.len == 0 ) {
                fprintf(stderr,"** no DA indices with '-da_list'\n");
                return -1;
            }
            /* and back up if we've looked too far */
            if( ac < argc && argv[ac][0] == '-') ac--;
        } else if( !strcmp(argv[ac], "-encoding") ) {
            ac++;
            CHECK_NEXT_OPT(ac, argc, "-encoding");
            if     ( !strcmp(argv[ac], "ASCII" ) )
                opts->encoding = GIFTI_ENCODING_ASCII;
            else if( !strcmp(argv[ac], "BASE64") )
                opts->encoding = GIFTI_ENCODING_B64BIN;
            else if( !strcmp(argv[ac], "BASE64GZIP") )
                opts->encoding = GIFTI_ENCODING_B64GZ;
            else {
                fprintf(stderr,"** invalid parm to -encoding: %s\n",argv[ac]);
                return -1;
            }
        } else if( !strcmp(argv[ac], "-indent") ) {
            ac++;
            CHECK_NEXT_OPT(ac, argc, "-indent");
            opts->indent = atoi(argv[ac]);
        } else if( !strncmp(argv[ac], "-infile", 7) ) { /* maybe infiles... */
            ac++;
            for( c = 0; (ac < argc) && (argv[ac][0] != '-'); ac++, c++ )
               if( add_to_str_list(&opts->infiles, argv[ac]) ) return -1;
            if( G.verb > 1 )
                fprintf(stderr,"+d have %d infile names\n", c);
            if( opts->infiles.len == 0 ) {
                fprintf(stderr,"** no filenames with '-infiles'\n");
                return -1;
            }
            /* and back up if we've looked too far */
            if( ac < argc && argv[ac][0] == '-') ac--;
        } else if( !strcmp(argv[ac], "-new_dset") ) {
            if( add_to_str_list(&opts->infiles, "MAKE_IM") ) return -1;
        } else if( !strcmp(argv[ac], "-new_numDA") ) {
            ac++;
            CHECK_NEXT_OPT(ac, argc, "-new_numDA");
            opts->new_numDA = atol(argv[ac]);
        } else if( !strcmp(argv[ac], "-new_intent") ) {
            ac++;
            CHECK_NEXT_OPT(ac, argc, "-new_intent");
            opts->new_intent = gifti_intent_from_string(argv[ac]);
            if( !gifti_intent_is_valid(opts->new_intent) ) {
                fprintf(stderr,"** invalid intent '%s'\n",argv[ac]);
                return -1;
            }
        } else if( !strcmp(argv[ac], "-new_dtype") ) {
            ac++;
            CHECK_NEXT_OPT(ac, argc, "-new_dtype");
            opts->new_dtype = gifti_str2datatype(argv[ac]);
            if( opts->new_dtype == DT_UNKNOWN ) {
                fprintf(stderr,"** invalid datatype '%s'\n",argv[ac]);
                return -1;
            }
        } else if( !strcmp(argv[ac], "-new_ndim") ) {
            ac++;
            CHECK_NEXT_OPT(ac, argc, "-new_ndim");
            opts->new_ndim = atol(argv[ac]);
        } else if( !strcmp(argv[ac], "-new_dims") ) {
            ac++;
            for( c = 0; ac < argc && c < GIFTI_DARRAY_DIM_LEN
                                  && argv[ac][0] != '-'; ac++, c++ )
               opts->new_dims[c] = atol(argv[ac]);
            if( c < GIFTI_DARRAY_DIM_LEN ) {
                fprintf(stderr, "** -new_dims have only %d of %d dims\n",
                                c, GIFTI_DARRAY_DIM_LEN);
                return -1;
            }
            ac--;  /* You've gone too far, go to your room! */
        } else if( !strcmp(argv[ac], "-new_data") ) {
            opts->new_data = 1;
        } else if( !strcmp(argv[ac], "-no_data") ) {
            opts->dstore = 0;
        } else if( !strcmp(argv[ac], "-show") ) {
            opts->gt_display = 1;
            opts->show_gifti = 1;
        } else if( !strcmp(argv[ac], "-write_1D") ) {
            ac++;
            CHECK_NEXT_OPT(ac, argc, "-write_1D");
            opts->ofile_1D = argv[ac];
            opts->gt_write = 1;
        } else if( !strcmp(argv[ac], "-write_asc") ) {
            ac++;
            CHECK_NEXT_OPT(ac, argc, "-write_asc");
            opts->ofile_asc = argv[ac];
            opts->gt_write = 1;
        } else if( !strcmp(argv[ac], "-write_gifti") ) {
            ac++;
            CHECK_NEXT_OPT(ac, argc, "-write_gifti");
            opts->ofile_gifti = argv[ac];
            opts->gt_write = 1;
        } else if( !strcmp(argv[ac], "-zlevel") ) {
            ac++;
            CHECK_NEXT_OPT(ac, argc, "-zlevel");
            opts->zlevel = atoi(argv[ac]);
        } else {
            fprintf(stderr,"** unknown option: '%s'\n",argv[ac]);
            return 1;
        }
    }

    if( G.verb > 2 ) disp_gt_opts("options read: ", opts, stderr);

    /* be sure we have something to read */
    if( opts->infiles.len <= 0 ) {
        fprintf(stderr,"** missing option: -infiles\n");
        return 1;
    }

    /* apply any XML user options
     * (non-zero defaults: verb, zlevel -1)
     */
    if( opts->verb   !=  1 ) gifti_set_verb(opts->verb);
    if( opts->indent != -1 ) gifti_set_indent(opts->indent);
    if( opts->buf_size     ) gifti_set_xml_buf_size(opts->buf_size);
    if( opts->b64_check    ) gifti_set_b64_check(opts->b64_check);
    if( opts->zlevel != -1 ) gifti_set_zlevel(opts->zlevel);

    return 0;
}

static int free_gt_opts(gt_opts * opts)
{
    if( opts->DAlist.len  > 0 && opts->DAlist.list  ) free(opts->DAlist.list);
    if( opts->infiles.len > 0 && opts->infiles.list ) free(opts->infiles.list);

    opts->DAlist.len = 0;   opts->DAlist.list = NULL;
    opts->infiles.len = 0;  opts->infiles.list = NULL;

    return 0;
}


int gt_display(gt_opts * opts)
{
    gifti_image * gim;
    int           c, rv = 0;

    if( opts->infiles.len < 1 ) {
        fprintf(stderr,"** no datasets to display\n");
        return 1;
    }

    /* just display any dataset for now */
    opts->show_gifti = 1;

    for( c = 0; c < opts->infiles.len; c++ ) {
        gim = gt_read_dataset(opts, opts->infiles.list[c]);
        if( !gim ) {
            fprintf(stderr,"** gt_display: failed to read '%s'\n",
                           opts->infiles.list[c]);
            rv = 1;
        }
        else gifti_free_image(gim);
    }

    return rv;
}

int gt_test(gt_opts * opts)
{
    gifti_image * gim;
    int           c, rv = 0;

    if( opts->infiles.len < 1 ) {
        fprintf(stderr,"** no datasets to test\n");
        return 1;
    }

    /* add more test later, now we just try to read */
    for( c = 0; c < opts->infiles.len; c++ ) {
        gim = gt_read_dataset(opts, opts->infiles.list[c]);
        if( !gim ) {
            fprintf(stderr,"** gt_test: failed to read '%s'\n",
                           opts->infiles.list[c]);
            rv = 1;
        }
        else gifti_free_image(gim);
    }

    return rv;
}

/* output is desired, one of:
 *
 *   - GIFTI dataset
 *   - 1D file as text data
 *   - .asc file, as a FreeSurfer style geometry dataset
 *
 * input: there can be only one (immortal?  Sean Connery?)
 */
int gt_write(gt_opts * opts)
{
    gifti_image * gim;
    int           c;

    if( opts->infiles.len > 1 ) {
        fprintf(stderr,"** when writing, only one input dataset is allowed\n");
        return 1;
    }

    /* actually read the dataset */
    gim = gt_read_dataset(opts, opts->infiles.list[0]);
    if( !gim ){ fprintf(stderr,"** failed gifti_read_da_list()\n"); return 1; }

    /* possibly adjust encoding */
    if( opts->encoding > GIFTI_ENCODING_UNDEF &&
        opts->encoding <= GIFTI_ENCODING_MAX )
        for( c = 0; c < gim->numDA; c++ )
            if( gim->darray[c]->encoding )
                gim->darray[c]->encoding = opts->encoding;

    if(opts->ofile_gifti) gifti_write_image(gim,opts->ofile_gifti,opts->dstore);
    if(opts->ofile_1D)  write_1D_file(gim->darray,gim->numDA,opts->ofile_1D,1); 
    if(opts->ofile_asc) write_as_asc(gim, opts->ofile_asc);

    /* clean up */
    gifti_free_image(gim);  gim = NULL;

    return 0;
}

/* read one GIFTI dataset
 *
 * The default is to just call gifti_read_da_list(), but...
 * if name == MAKE_IM   : create a new dataset
 * else if we have name : read one dataset
 *
 * Note that name may have the form dset[int list], where the integer
 * list is used to create the DataArray list.
 * 
 * e.g.  dset.gii[5..12(3),0,$]
 *
 *       this would select DA elements 5,8,11,0,numDA-1
 *
 * Note that the DA list selection requires reading the dataset twice,
 * first to compute the number of DA elements.
 */
gifti_image * gt_read_dataset(gt_opts * opts, char * fname)
{
    gifti_image * gim;
    char        * fcopy = NULL, * iptr, * infile = fname;
    int         * dalist = NULL, numDA = -1;

    if( !fname || !*fname ) {
        fprintf(stderr,"** gt_read_dataset: no filename to read\n");
        return NULL;
    }

    /* first case, create a new image (fname == MAKE_IM) */
    if( !strcmp(fname, "MAKE_IM") ) {
        if( opts->verb > 1 ) fprintf(stderr,"++ creating new GIFTI dataset\n");

        gim = gifti_create_image(opts->new_numDA, opts->new_intent,
                                 opts->new_dtype, opts->new_ndim,
                                 opts->new_dims,  opts->new_data);

        if( opts->show_gifti ) gifti_disp_gifti_image("dset MAKE_IM :",gim,1);

        return gim;
    }

    /* otherwise, see if there is an int list, before using gifti_read */

    if( strchr(fname, '[') ) { /* then create an int list */
        fcopy = gifti_strdup(fname);
        infile = fcopy;         /* store for later */
        iptr = strchr(fcopy, '[');

        if(opts->verb>2) fprintf(stderr,"-- getting DA list from %s\n",iptr);
        *iptr = '\0';   /* don't need the char, but want terminated filename */

        /* read dataset just for numDA */
        numDA = gifti_read_dset_numDA(fcopy);
        if( numDA < 0 ) {
            fprintf(stderr, "** GT_RD: failed to get numDA from '%s'\n", fcopy);
            free(fcopy);
            return NULL;
        }

        dalist = nifti_get_intlist(numDA, iptr+1);
        if( !dalist ) {
            fprintf(stderr,"** GT_RD: bad int list from '%s'\n", iptr+1);
            free(fcopy);
            return NULL;
        }
        if( opts->verb > 2 ) {
            fprintf(stderr,"++ have DA list: ");
            gifti_disp_raw_data(dalist, NIFTI_TYPE_INT32, numDA, 1, stderr);
        }
    }

    /* pass either dalist or from opts */
    if( dalist && numDA > 0 )
        gim = gifti_read_da_list(infile, opts->dstore, dalist+1, dalist[0]);
    else
        gim = gifti_read_da_list(infile, opts->dstore,
                                 opts->DAlist.list, opts->DAlist.len);

    /* regardless of success, check to free data and return */
    if( dalist ) free(dalist);
    if( fcopy  ) free(fcopy);

    if( opts->show_gifti ) {
        fcopy = (char *)malloc((strlen(fname)+32) * sizeof(char));
        if( !fcopy ) return gim;  /* forget it */
        sprintf(fcopy, "dset '%s' :", fname);

        gifti_disp_gifti_image(fcopy, gim, 1 );
        free(fcopy);
    }

    return gim;
}

/* init any options that should not default to 0 (so 0 means something,
 * or the default is non-zero) */
static int init_opts(gt_opts * opts)
{
    memset(opts, 0, sizeof(gt_opts));

    /* gt_* should init to 0 */

    /* new flags can be set to something useful (1 DA, no data, ...) */
    opts->new_numDA = 1;
    opts->new_intent = NIFTI_INTENT_NONE;
    opts->new_dtype = NIFTI_TYPE_FLOAT32;
    opts->new_ndim = 0;
    /* opts->new_dims left with zeros */
    opts->new_data = 0;

    opts->verb = 1;
    opts->indent = -1;
    opts->dstore = 1;
    opts->zlevel = -1;

    return 0;
}

static int disp_gt_opts(char * mesg, gt_opts * opts, FILE * stream)
{
    FILE * fp = stream ? stream : stdout;
    int    c;

    if( mesg ) fputs(mesg, fp);

    fprintf(fp, "gt_opts struct:\n"
        "    gt_compare    : %d\n"
        "    gt_display    : %d\n"
        "    gt_test       : %d\n"
        "    gt_write      : %d\n"
        "    gt_modify     : %d\n"
        "\n"
        "    new_numDA     : %d\n"
        "    new_intent    : %d\n"
        "    new_dtype     : %d\n"
        "    new_ndim      : %d\n"
        "    new_dims [%d]  : ",
        opts->gt_compare, opts->gt_display, opts->gt_test,
        opts->gt_write, opts->gt_modify,
        opts->new_numDA, opts->new_intent, opts->new_dtype, opts->new_ndim,
        GIFTI_DARRAY_DIM_LEN
        );

    gifti_disp_raw_data(opts->new_dims, NIFTI_TYPE_INT32,
                        GIFTI_DARRAY_DIM_LEN, 1, fp);
    fprintf(fp,
        "    new_data      : %d\n"
        "\n"
        "    verb          : %d\n"
        "    indent        : %d\n"
        "    buf_size      : %d\n"
        "    b64_check     : %d\n"
        "    zlevel        : %d\n"
        "\n"
        "    dstore        : %d\n"
        "    encoding      : %d\n"
        "    show_gifti    : %d\n"
        "    ofile_1D      : %s\n"
        "    ofile_asc     : %s\n"
        "    ofile_gifti   : %s\n\n",
        opts->new_data,
        opts->verb, opts->indent, opts->buf_size, opts->b64_check, opts->zlevel,
        opts->dstore, opts->encoding, opts->show_gifti,
        G_CHECK_NULL_STR(opts->ofile_1D),
        G_CHECK_NULL_STR(opts->ofile_asc),
        G_CHECK_NULL_STR(opts->ofile_gifti));

    /* DataArray index list */
    fprintf(fp, "    DAlist[%d]     : ", opts->DAlist.len);
    if( opts->DAlist.len <= 0 || !opts->DAlist.list )
        fprintf(fp, "<empty>\n");
    else
        gifti_disp_raw_data(opts->DAlist.list, NIFTI_TYPE_INT32,
                            opts->DAlist.len, 1, fp);

    /* infile list */
    fprintf(fp, "    infiles[%d]    : ", opts->infiles.len);
    if( opts->infiles.len <= 0 || !opts->infiles.list )
        fprintf(fp, "<empty>\n");
    else {
        for( c = 0; c < opts->infiles.len; c++ )
            fprintf(fp, "%s ", opts->infiles.list[c]);
        fputc('\n', fp);
    }

    return 0;
}

static int show_hist(void)
{
    int c, len = sizeof(g_history)/sizeof(char *);
    for( c = 0; c < len; c++)
        printf(g_history[c]);
    return 0;
}

static int show_version(void)
{
    puts(g_version);
    return 0;
}

static int show_help()
{
    printf(
    "------------------------------------------------------------\n"
    "gifti_tool  - create, display, modify or compare GIFTI datasets\n"
    "\n"
    "  examples:\n"
    "\n"
    "    1. read in a GIFTI dataset (verbose?  show output?)\n"
    "\n"
    "        gifti_tool -infile dset.gii\n"
    "        gifti_tool -infile dset.gii -verb 3\n"
    "        gifti_tool -infile dset.gii -show\n"
    "\n"
    "    2. copy a GIFTI dataset (check differences?)\n"
    "\n"
    "        gifti_tool -infile dset.gii -write_gifti copy.gii\n"
    "        diff dset.gii copy.gii\n"
    "\n"
    "    3. copy a GIFTI data, but write out only 3 surf indices: 4, 0, 5\n"
    "\n"
    "        gifti_tool -infile time_series.gii -write_gifti ts3.gii  \\\n"
    "                   -da_list 4 0 5\n"
    "              OR\n"
    "\n"
    "        gifti_tool -infile time_series.gii'[4,0,5]' -write_gifti ts3.gii\n"
    "\n"
    "    4. create .asc FreeSurfer-style surface dataset (pial.asc)\n"
    "\n"
    "        gifti_tool -infile pial.gii -write_asc pial.asc\n"
    "\n"
    "    5. create .1D time series surface dataset (ts.1D)\n"
    "\n"
    "        gifti_tool -infile time_series.gii -write_1D ts.1D\n"
    "\n"
    "    6. create a new gifti dataset from nothing\n"
    "\n"
    "        gifti_tool -new_dset -write_gifti new.gii           \\\n"
    "                   -new_numDA 3 -new_dtype NIFTI_TYPE_INT16 \\\n"
    "                   -new_intent NIFTI_INTENT_TTEST           \\\n"
    "                   -new_ndim 2 -new_dims 5 2 0 0 0 0\n"
    "\n"
    "  options:\n"
    "     -help             : show this help\n"
    "\n"
    "     -b64_check   TYPE : set method for checking base64 errors\n"
    "                  TYPE = NONE       : no checks - assume all is well\n"
    "                  TYPE = DETECT     : report whether errors were found\n"
    "                  TYPE = COUNT      : count the number of bad chars\n"
    "                  TYPE = SKIP       : ignore any bad characters\n"
    "                  TYPE = SKIPnCOUNT : ignore but count bad characters\n"
    "     -buf_size         : set buffer size\n"
    "                         e.g. -buf_size 1024\n"
    "     -da_list s0 ...   : restrict DataArray list indices s0, s1, ...\n"
    "     -encoding    TYPE : set the data encoding for any output file\n"
    "                  TYPE = ASCII      : ASCII encoding\n"
    "                  TYPE = BASE64     : base64 binary\n"
    "                  TYPE = BASE64GZIP : base64 compressed binary\n"
    "     -gifti_hist       : show giftilib history\n"
    "     -gifti_ver        : show giftilib version\n"
    "     -infile     INPUT : write out dataset as gifti image\n"
    "\n"
    "     -new_dset         : create a new GIFTI dataset\n"
    "     -new_numDA  NUMDA : new dataset will have NUMDA DataArray elements\n"
    "                         e.g. -new_numDA 3\n"
    "     -new_intent INTENT: DA elements will have intent INTENT\n"
    "                         e.g. -new_intent NIFTI_INTENT_FTEST\n"
    "     -new_dtype   TYPE : set datatype to TYPE\n"
    "                         e.g. -new_dtype NIFTI_TYPE_FLOAT32\n"
    "     -new_ndim NUMDIMS : set Dimensionality to NUMDIMS (see -new_dims)\n"
    "     -new_dims D0...D5 : set dims[] to these 6 values\n"
    "                         e.g. -new_ndim 2 -new_dims 7 2 0 0 0 0\n"
    "     -new_data         : allocate space for data in created dataset\n"
    "\n"
    "     -no_data          : do not write out data\n"
    "     -show             : show final gifti image\n"
    "     -verb        VERB : set verbose level\n"
    "     -write_1D    DSET : write out data to AFNI style 1D file\n"
    "     -write_asc   DSET : write out geometry to FreeSurfer style ASC file\n"
    "     -write_gifti DSET : write out dataset as gifti image\n"
    "     -zlevel     LEVEL : set compression level (-1 or 0..9)\n"
    "------------------------------------------------------------\n"
    );
    return 0;
}

int write_as_asc(gifti_image * gim, char * prefix)
{
    giiDataArray  * dac; /* coords */
    giiDataArray  * dat; /* triangles */

    fprintf(stderr,"-- trying to write data with prefix '%s'\n", prefix);

    /* write surface file, *.1D */
    if( (dac = gifti_find_DA(gim, NIFTI_INTENT_POINTSET, 0)) &&
        (dat = gifti_find_DA(gim, NIFTI_INTENT_TRIANGLE, 0))    )
        (void) write_surf_file(dac, dat, prefix, 1);
    else {
        fprintf(stderr,"** failed to find coordinate/triangle structs\n");
        return 1;
    }

    return 0;
}


/* if dlist contains 1 element, write out as 2-D list,
   else each DA must have only 1 dimension */
int write_1D_file(giiDataArray ** dlist, int len, char * prefix, int add_suf)
{
    giiDataArray * da;
    FILE         * fp;
    char         * name = prefix;
    char         * nbuf = NULL;
    long long      rows, cols, c;

    if( add_suf && !strstr(prefix, ".1D")) {     /* create a new name */
        nbuf = (char *)malloc(strlen(prefix) + strlen(".1D") + 1);
        strcpy(nbuf, prefix);
        strcat(nbuf, ".1D");
        name = nbuf;
    }

    if( len == 1 ){     /* write out as 2D list */
        /* note the number of rows and columns */
        fprintf(stderr,"++ writing 1D '%s' from single DA\n", name);
        da = dlist[0];
        if( gifti_DA_rows_cols(da, &rows, &cols) ) {
            fprintf(stderr,"** bad return from rows_cols, failing...\n");
            if( nbuf ) free(nbuf);
            return 1;
        }

        if( !(fp = fopen(name, "w")) ) {
            fprintf(stderr,"** failed to open '%s' for 'w'\n",name);
            if( nbuf ) free(nbuf);
            return 1;
        }

        fprintf(stderr,"++ 1D write, RxC = %lld x %lld\n", rows, cols);
        if( da->ind_ord == GIFTI_IND_ORD_COL_MAJOR ) {
            fprintf(stderr,"-- writing data rows in reverse order\n");
            for(c = rows-1; c >= 0; c-- )
                ewrite_data_line(da->data, da->datatype, c, cols, 0, 0, fp);
        } else {
            fprintf(stderr,"-- writing data rows in normal order\n");
            for(c = 0; c < rows; c++ )
                ewrite_data_line(da->data, da->datatype, c, cols, 0, 0, fp);
        }
    } else {            /* write da->nvals lines of 'num values */
        void ** vlist = (void **)malloc(len * sizeof(void *));

        fprintf(stderr,"++ writing 1D '%s' from DA list (%d)\n", name, len);

        /* set data pointers */
        for( c = 0; c < len; c++ ) {
            vlist[c] = dlist[c]->data;
            if( dlist[c]->nvals != dlist[0]->nvals ) {
                fprintf(stderr,"** d[%lld] has %lld vals, but d[0] has %lld\n",
                        c, dlist[c]->nvals, dlist[0]->nvals);
                free(vlist);
                if( nbuf ) free(nbuf);
                return 1;
            } else if (dlist[c]->datatype != dlist[0]->datatype) {
                fprintf(stderr,"** d[%lld] has type %d, but d[0] has %d\n",
                        c, dlist[c]->datatype, dlist[0]->datatype);
                free(vlist);
                if( nbuf ) free(nbuf);
                return 1;
            }
        }

        /* good to go */
        if( !(fp = fopen(name, "w")) ) {
            fprintf(stderr,"** failed to open '%s' for 'w'\n",name);
            if( nbuf ) free(nbuf);
            return 1;
        }

        fprintf(stderr,"++ 1D write, RxC = %u x %d\n",
                (unsigned)dlist[0]->nvals, len);
        ewrite_many_lines(vlist, dlist[0]->datatype,len, dlist[0]->nvals, 0,fp);
                          
        free(vlist);
    }

    if( G.verb > 1 ) fprintf(stderr,"++ 1D write, apparent success\n");

    if( nbuf ) free(nbuf);
    fclose(fp);

    return 0;
}


int write_surf_file(giiDataArray * dc, giiDataArray * dt, char * prefix,
                    int add_suf)
{
    giiDataArray * da;
    FILE         * fp;
    char         * name = prefix;
    char         * nbuf = NULL;
    long long      crows, ccols, trows, tcols, rows, cols, c;

    if( add_suf && !strstr(prefix, ".asc") ) {   /* maybe create a new name */
        nbuf = (char *)malloc(strlen(prefix) + strlen(".asc") + 1);
        strcpy(nbuf, prefix);
        strcat(nbuf, ".asc");
        name = nbuf;
    }

    if( !(fp = fopen(name, "w")) ) {
        fprintf(stderr,"** failed to open '%s' for 'w'\n",name);
        if( nbuf ) free(nbuf);
        return 1;
    }

    /* note the number of rows and columns */
    if( gifti_DA_rows_cols(dc, &crows, &ccols) ) {
        fclose(fp);
        if( nbuf ) free(nbuf);
        return 1;
    } else if( gifti_DA_rows_cols(dt, &trows, &tcols) ) {
        fclose(fp);
        if( nbuf ) free(nbuf);
        return 1;
    }

    fprintf(fp, "#!ascii version of surface\n"
                "%lld %lld\n", crows, trows);

    /* write out the coordinates */

    da = dc;
    rows = crows;
    cols = ccols;

    if( da->ind_ord == GIFTI_IND_ORD_COL_MAJOR ) {
        fprintf(stderr,"-- writing coord rows in reverse order\n");
        for(c = rows-1; c >= 0; c-- )
            ewrite_data_line(da->data, da->datatype, c, cols, 0, 1, fp);
    } else {
        fprintf(stderr,"-- writing coord rows in normal order\n");
        for(c = 0; c < rows; c++ )
            ewrite_data_line(da->data, da->datatype, c, cols, 0, 1, fp);
    }

    /* write out the triangles */

    da = dt;
    rows = trows;
    cols = tcols;

    if( da->ind_ord == GIFTI_IND_ORD_COL_MAJOR ) {
        fprintf(stderr,"-- writing triangle rows in reverse order\n");
        for(c = rows-1; c >= 0; c-- )
            ewrite_data_line(da->data, da->datatype, c, cols, 0, 1, fp);
    } else {
        fprintf(stderr,"-- writing triangle rows in normal order\n");
        for(c = 0; c < rows; c++ )
            ewrite_data_line(da->data, da->datatype, c, cols, 0, 1, fp);
    }


    fclose(fp);

    return 0;
}


int ewrite_data_line(void * data, int type, int row, int cols, int spaces,
                     int trail0, FILE * fp)
{
    int c;
    if( !data || row < 0 || cols <= 0 || !fp ) {
        static int show = 1;
        if(show){
            fprintf(stderr,"** write data line: bad inputs (%p,%d,%d,%p)\n",
                   data, row, cols, (void *)fp);
            show=0;
        }
        return 1;
    }

    fprintf(fp, "%*s", spaces, "");
    switch( type ) {
        default :
            fprintf(stderr,"** write_data_line, unknown type %d\n",type);
            return -1;
        case NIFTI_TYPE_UINT8: {
            unsigned char * ptr = (unsigned char *)data + row * cols;
            for( c = 0; c < cols; c++ ) fprintf(fp, "%u ", ptr[c]);
            break;
        }
        case NIFTI_TYPE_INT16: {
            short * ptr = (short *)data + row * cols;
            for( c = 0; c < cols; c++ ) fprintf(fp, "%d ", ptr[c]);
            break;
        }
        case NIFTI_TYPE_INT32: { 
            int * ptr = (int *)data + row * cols;
            for( c = 0; c < cols; c++ ) fprintf(fp, "%d ", ptr[c]);
            break;
        }
        case NIFTI_TYPE_FLOAT32: {
            float * ptr = (float *)data + row * cols;
            for( c = 0; c < cols; c++ ) fprintf(fp, "%f ", ptr[c]);
            break;
        }
        case NIFTI_TYPE_COMPLEX64: {
            float * ptr = (float *)data + row * cols;
            for(c = 0; c < 2*cols; c+=2)fprintf(fp, "%f %f   ",ptr[c],ptr[c+1]);            break;
        }
        case NIFTI_TYPE_FLOAT64: {
            double * ptr = (double *)data + row * cols;
            for( c = 0; c < cols; c++ ) fprintf(fp, "%f ", ptr[c]);
            break;
        }
        case NIFTI_TYPE_RGB24: {
            unsigned char * ptr = (unsigned char *)data + row * cols;
            for( c = 0; c < 3*cols; c+=3 )
                fprintf(fp, "%u %u %u   ", ptr[c], ptr[c+1], ptr[c+2]);
            break;
        }
        case NIFTI_TYPE_INT8: {
            char * ptr = (char *)data + row * cols;
            for( c = 0; c < cols; c++ ) fprintf(fp, "%d ", ptr[c]);
            break;
        }
        case NIFTI_TYPE_UINT16: {
            unsigned short * ptr = (unsigned short *)data + row * cols;
            for( c = 0; c < cols; c++ ) fprintf(fp, "%u ", ptr[c]);
            break;
        }
        case NIFTI_TYPE_UINT32: {     /* NIFTI_TYPE_UINT32 */
            unsigned int * ptr = (unsigned int *)data + row * cols;
            for( c = 0; c < cols; c++ ) fprintf(fp, "%u ", ptr[c]);
            break;
        }
        case NIFTI_TYPE_INT64: {
            long long * ptr = (long long *)data + row * cols;
            for( c = 0; c < cols; c++ ) fprintf(fp, "%lld ", ptr[c]);
            break;
        }
        case NIFTI_TYPE_UINT64: {
            unsigned long long * ptr = (unsigned long long *)data + row * cols;
            for( c = 0; c < cols; c++ ) fprintf(fp, "%llu ", ptr[c]);
            break;
        }
        case NIFTI_TYPE_FLOAT128: {
            fprintf(stderr,"** ewrite_data_line, won't write %s\n",
                    gifti_datatype2str(type));
            break;
        }
        case NIFTI_TYPE_COMPLEX128: {
            double * ptr = (double *)data + row * cols;
            for(c = 0; c < 2*cols; c+=2)fprintf(fp, "%f %f   ",ptr[c],ptr[c+1]);            break;
        }
        case NIFTI_TYPE_COMPLEX256: {
            fprintf(stderr,"** ewrite_data_line, won't write %s\n",
                    gifti_datatype2str(type));
            break;
        }
    }

    if( trail0 ) fputs(" 0", fp);  /* maybe write trailing zero */

    fputc('\n', fp);

    return 0;
}


/* write out as cols by rows (else we'd use ewrite_data_line) */
int ewrite_many_lines(void ** data, int type, long long cols, long long rows,
                      int spaces, FILE * fp)
{
    long long r, c;

    if( !data || rows == 0 || cols == 0 || !fp ) return 1;

    fprintf(fp, "%*s", spaces, "");
    switch( type ) {
        default :
            fprintf(stderr,"** write_data_line, unknown type %d\n",type);
            return -1;
        case 2: {       /* NIFTI_TYPE_UINT8 */
            unsigned char ** ptr = (unsigned char **)data;
            for( r = 0; r < rows; r++ ) {
                for( c = 0; c < cols; c++ ) fprintf(fp, "%u ", ptr[c][r]);
                fputc('\n', fp);
            }
            break;
        }
        case 4: {       /* NIFTI_TYPE_INT16 */
            short ** ptr = (short **)data;
            for( r = 0; r < rows; r++ ) {
                for( c = 0; c < cols; c++ ) fprintf(fp, "%d ", ptr[c][r]);
                fputc('\n', fp);
            }
            break;
        }
        case 8: {       /* NIFTI_TYPE_INT32 */
            int ** ptr = (int **)data;
            for( r = 0; r < rows; r++ ) {
                for( c = 0; c < cols; c++ ) fprintf(fp, "%d ", ptr[c][r]);
                fputc('\n', fp);
            }
            break;
        }
        case 16: {      /* NIFTI_TYPE_FLOAT32 */
            float ** ptr = (float **)data;
            for( r = 0; r < rows; r++ ) {
                for( c = 0; c < cols; c++ ) fprintf(fp, "%f ", ptr[c][r]);
                fputc('\n', fp);
            }
            break;
        }
        case 32: {      /* NIFTI_TYPE_COMPLEX64 */
            break;
        }
        case 64: {      /* NIFTI_TYPE_FLOAT64 */
            double ** ptr = (double **)data;
            for( r = 0; r < rows; r++ ) {
                for( c = 0; c < cols; c++ ) fprintf(fp, "%f ", ptr[c][r]);
                fputc('\n', fp);
            }
            break;
        }
        case 128: {     /* NIFTI_TYPE_RGB24 */
            break;
        }
        case 256: {     /* NIFTI_TYPE_INT8 */
            char ** ptr = (char **)data;
            for( r = 0; r < rows; r++ ) {
                for( c = 0; c < cols; c++ ) fprintf(fp, "%d ", ptr[c][r]);
                fputc('\n', fp);
            }
            break;
        }
        case 512: {     /* NIFTI_TYPE_UINT16 */
            unsigned short ** ptr = (unsigned short **)data;
            for( r = 0; r < rows; r++ ) {
                for( c = 0; c < cols; c++ ) fprintf(fp, "%u ", ptr[c][r]);
                fputc('\n', fp);
            }
            break;
        }
        case 768: {     /* NIFTI_TYPE_UINT32 */
            unsigned int ** ptr = (unsigned int **)data;
            for( r = 0; r < rows; r++ ) {
                for( c = 0; c < cols; c++ ) fprintf(fp, "%u ", ptr[c][r]);
                fputc('\n', fp);
            }
            break;
        }
        case 1024: {    /* NIFTI_TYPE_INT64 */
            /* rcr - do we need to check #defines? */
            break;
        }
        case 1280: {    /* NIFTI_TYPE_UINT64 */
            /* rcr - do we need to check #defines? */
            break;
        }
        case 1536: {    /* NIFTI_TYPE_FLOAT128 */
            /* rcr - do we need to check #defines? */
            break;
        }
        case 1792: {    /* NIFTI_TYPE_COMPLEX128 */
            break;
        }
        case 2048: {    /* NIFTI_TYPE_COMPLEX256 */
            /* rcr - do we need to check #defines? */
            break;
        }
    }

    return 0;
}

/*----------------------------------------------------------------------
 * - only bother to alloc one pointer at a time (don't need efficiency here)
 * - return 0 on success
 *----------------------------------------------------------------------*/
static int add_to_int_list(gt_int_list * ilist, int val)
{
   if( ilist->len == 0 ) ilist->list = NULL;  /* just to be safe */
   ilist->len++;
   ilist->list = (int *)realloc(ilist->list,ilist->len*sizeof(int));
   if( ! ilist->list ){
      fprintf(stderr,"** A2IL: failed to alloc %d (int) elements\n",ilist->len);
      return -1;
   }

   ilist->list[ilist->len-1] = val;

   return 0;
}


/*----------------------------------------------------------------------
 * - do not duplicate the string
 * - only bother to alloc one pointer at a time (don't need efficiency here)
 * - return 0 on success
 *----------------------------------------------------------------------*/
static int add_to_str_list(gt_str_list * slist, char * str)
{
   if( slist->len == 0 ) slist->list = NULL;  /* just to be safe */
   slist->len++;
   slist->list = (char **)realloc(slist->list,slist->len*sizeof(char *));
   if( ! slist->list ){
      fprintf(stderr,"** A2SL: failed to alloc %d (char *) elements\n",
              slist->len);
      return -1;
   }

   slist->list[slist->len-1] = str;

   return 0;
}

