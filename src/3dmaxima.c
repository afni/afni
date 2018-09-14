/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/

static char * g_history[] =
{
    "History:",
    "",
    "  1.0  06 Oct 2005 [rickr]  - initial release of 3dmaxima",
    "    - for now, no improvements over plug_maxima",
    "  1.1  18 Aug 2006 [rickr]  - added -coords_only option",
    "  1.2  17 Jul 2007 [rickr]  - fixed -n_style_sort option use",
    "  1.3  16 Apr 2013 [rickr]  - apply modern dset coordinate signs",
    "                              reported by G Pagnoni",
    "  1.4  13 Sep 2018 [rickr]  - return 0 on terminal return"
};

#define MAXIMA_VERSION "1.4 (September 13, 2018)"

static char * g_help[] =
{
"3dmaxima - used to locate extrema in a functional dataset.",
"",
"   This program reads a functional dataset and locates any relative extrema",
"   (maximums or minimums, depending on the user option).  A _relative_",
"   maximum is a point that is greater than all neighbors (not necessarily",
"   greater than all other values in the sub-brick).  The output from this",
"   process can be text based (sent to the terminal window) and it can be a",
"   mask (integral) dataset, where the locations of the extrema are set.",
"",
"   When writing a dataset, it is often useful to set a sphere around each",
"   extrema, not to just set individual voxels.  This makes viewing those",
"   locations much more reasonable.  Also, if the 'Sphere Values' option is",
"   set to 'N to 1', the sphere around the most extreme voxel will get the",
"   value N, giving it the 'top' color in afni (and so on, down to 1).",
"",
"   Notes : The only required option is the input dataset.",
"           Input datasets must be of type short.",
"           All distances are in voxel units.",
"",
"----------------------------------------------------------------------",
"                        ***  Options  ***",
"",
"-----  Input Dset:  -----",
"",
"   -input DSET           : specify input dataset",
"",
"         e.g. -input func+orig'[7]'",
"",
"       Only one sub-brick may be specified.  So if a dataset has multiple",
"       sub-bricks, the [] selector must be used.",
"",
"-----  Output Dset:  -----",
"",
"   -prefix PREFIX        : prefix for an output mask dataset",
"",
"         e.g. -prefix maskNto1",
"",
"       This dataset may be viewed as a mask.  It will have a value set at",
"       the location of any selected extrema.  The -out_rad option can be",
"       used to change those points to 'spheres'.",
"",
"   -spheres_1            : [flag] set all output values to 1",
"",
"       This is the default, which sets all values in the output dataset",
"       to 1.  This is for the extreme points, and for the spheres centered",
"       around them.",
"",
"   -spheres_1toN         : [flag] output values will range from 1 to N",
"",
"       In this case, the most extreme voxel will be set with a value of 1.",
"       The next most extreme voxel will get 2, and so on.",
"",
"   -spheres_Nto1         : [flag] output values will range from N to 1",
"",
"       With this option, the highest extrema will be set to a value of N,",
"       where N equals the number of reported extrema.  The advantage of",
"       this is that the most extreme point will get the highest color in",
"       afni.",
"",
"-----  Threshold:  -----",
"",
"   -thresh CUTOFF        : provides a cutoff value for extrema",
"",
"         e.g. -thresh 17.4",
"",
"       Extrema not meeting this cutoff will not be considered.",
"       Note that if the '-neg_ext' option is applied, the user",
"       will generally want a negative threshold.",
"",
"-----  Separation:  -----",
"",
"   -min_dist VOXELS      : minimum acceptable distance between extrema",
"",
"         e.g. -min_dist 4",
"",
"       Less significant extrema which are close to more significant extrema",
"       will be discounted in some way, depending on the 'neighbor style'",
"       options.",
"",
"       See '-n_style_sort' and '-n_style_weight_ave' for more information.",
"",
"       Note that the distance is in voxels, not mm.",
"",
"-----  Output Size:  -----",
"",
"   -out_rad SIZE         : set the output radius around extrema voxels",
"",
"         e.g. -out_rad 9",
"",
"       If the user wants the output BRIK to consist of 'spheres' centered",
"       at extrema points, this option can be used to set the radius for",
"       those spheres.  Note again that this is in voxel units.",
"",
"-----  Neighbor:  -----",
"",
"   If extrema are not as far apart as is specified by the '-min_dist'",
"   option, the neighbor style options specify how to handle the points.",
"",
"   -n_style_sort         : [flag] use 'Sort-n-Remove' style (default)",
"",
"       The extrema are sorted by magnitude.  For each extrema (which has",
"       not previously removed), all less significant extrema neighbors",
"       within the separation radius (-min_dist) are removed.",
"",
"       See '-min_dist' for more information.",
"",
"   -n_style_weight_ave   : [flag] use 'Weighted-Average' style",
"",
"       Again, traverse the sorted list of extrema.  Replace the current",
"       extrema with the center of mass of all extrema within the Separation",
"       radius of the current point, removing all others within this radius.",
"",
"       This should not change the number of extrema, it should only shift",
"       the locations.",
"",
"-----  Params:  -----",
"",
"   -neg_ext              : [flag] search for negative extrema (minima)",
"",
"       This will search for the minima of the dataset.",
"       Note that a negative threshold may be desired.",
"",
"   -true_max             : [flag] extrema may not have equal neighbors",
"",
"       By default, points may be considered extrema even if they have a",
"       neighbor with the same value.  This flag option requires extrema",
"       to be strictly greater than any of their neighbors.",
"",
"       With this option, extrema locations that have neighbors at the same",
"       value are ignored.",
"",
"-----  Output Text:  -----",
"",
"   -debug LEVEL          : output extra information to the terminal",
"",
"       e.g. -debug 2",
"",
"   -no_text              : [flag] do not display the extrma points as text",
"",
"   -coords_only          : [flag] only output coordinates (no text or vals)",
"",
"-----  Output Coords:  -----",
"",
"   -dset_coords          : [flag] display output in the dataset orientation",
"",
"       By default, the xyz-coordinates are displayed in DICOM orientation",
"       (RAI), i.e. right, anterior and inferior coordinates are negative,",
"       and they are printed in that order (RL, then AP, then IS).",
"",
"       If this flag is set, the dataset orientation is used, whichever of",
"       the 48 it happens to be.",
"",
"       Note that in either case, the output orientation is printed above",
"       the results in the terminal window, to remind the user.",
"",
"-----  Other :  -----",
"",
"   -help                 : display this help",
"",
"   -hist                 : display module history",
"",
"   -ver                  : display version number",
"",
"Author: R Reynolds",
""
};


#include "afni.h"
#include "maxima.h"

/*------------------------- option macros -------------------------*/
#define CHECK_NEXT_OPT(n,m,str)                                       \
   do { if ( (n) >= (m) ) {                                           \
           fprintf(stderr,"** option '%s': missing parameter\n",str); \
           fprintf(stderr,"   consider: '3dmaxima -help'\n");         \
           RETURN(1);      }                                          \
      } while(0)

#define CHECK_NEXT_OPT_MSG(n,m,str,msg)                               \
   do { if ( (n) >= (m) ) {                                           \
           fprintf(stderr,"** option '%s': %s\n",str,msg);            \
           fprintf(stderr,"   consider: '3dmaxima -help'\n");         \
           RETURN(1);      }                                          \
      } while(0)


/*------------------------ local prototypes -----------------------*/
static int process_args(int argc, char * argv[], r_afni_s * A, maxima_s * M);


/*----------------------------------------------------------------------
**
**  Main routine for this plugin (will be called from AFNI).
**
**----------------------------------------------------------------------
*/
int main( int argc, char * argv[] )
{
    THD_3dim_dataset * dnew;
    r_afni_s           A;
    maxima_s           M;
    int                rv;

    mainENTRY("3dmaxima main"); machdep() ;
    if (( rv = process_args( argc, argv, &A, &M )) != 0 )
	return(rv < 0);

    if ( ! process_data( &M ) )
	return 1;

    if ( ! M.quiet ) display_coords( &A, &M );

    if ( *M.outfile )
    {
        if ( ( dnew = EDIT_empty_copy( M.dset ) ) == NULL )
        {
            rERROR( "Error: wr_00\n" "Failed to copy dataset." );
            return 1;
        }

        /* add history */
        tross_Copy_History( M.dset , dnew );
        tross_Make_History( "3dmaxima", argc, argv, dnew );

        if ( ! set_results( &A, &M, dnew ) )
            return 1;

        DSET_write(dnew);
    }

    free_memory( &A, &M );

    return 0;
}


/*----------------------------------------------------------------------
 *  process user arguments
 *
 *  return  > 0 : valid exit
 *            0 : success
 *          < 0 : failure
 *----------------------------------------------------------------------
*/
static int process_args( int argc, char * argv[], r_afni_s * A, maxima_s * M )
{
    THD_3dim_dataset * dset = NULL;
    char             * outfile = NULL;
    float              cutoff = 0.0, min_dist = 0.0, out_rad = 0.0;
    int                negatives = 0, quiet = 0, coords_only = 0;
    int                true_max = 0, opcnt = 0;
    int                debug = 0, style = MAX_SORT_N_REMOVE_STYLE;
    int                sval_style = 0, dicom_coords = 1;
    int                ac;

ENTRY("process_args");

    if( argc < 2 )
    {
        disp_str_list(g_help, sizeof(g_help)/sizeof(char*));
        RETURN(1);
    }

    if( ! init_afni_s( A ) ) RETURN(-1);

    for( ac = 1; ac < argc; ac++ )
    {
	if( ! strcmp( argv[ac], "-input" ) )
        {
            ac++;
            CHECK_NEXT_OPT(ac, argc, "-input");
            dset = THD_open_dataset(argv[ac]);
            if( !dset )
            {
                fprintf(stderr,"** failed to open dataset '%s'\n",argv[ac]);
                RETURN(-1);
            }
            /* hopefully we can remove this check sometime soon... */
            if( DSET_BRICK_TYPE(dset,0) != MRI_short )
            {
                fprintf(stderr,"** only short datasets are allowed, sorry!\n");
                RETURN(-1);
            }
            if( DSET_NVALS(dset) > 1)
            {
                fprintf(stderr,"** dset has %d sub-bricks, must indicate one\n",
                        DSET_NVALS(dset));
                RETURN(-1);
            }
        }
	else if( ! strncmp( argv[ac], "-coords_only", 7 ) )  /* 18 Aug 2006 */
	{
	    coords_only = 1;
        }
	else if( ! strcmp( argv[ac], "-debug" ) )
        {
            ac++;
            CHECK_NEXT_OPT(ac, argc, "-debug");
	    debug = atoi(argv[ac]);
	}
	else if( ! strcmp( argv[ac], "-dset_coords" ) )
	{
	    dicom_coords = 0;
	}
	else if( ! strcmp( argv[ac], "-help" ) )
	{
	    disp_str_list(g_help, sizeof(g_help)/sizeof(char*));
            RETURN(1);
	}
	else if( ! strcmp( argv[ac], "-hist" ) )
	{
	    disp_str_list(g_history, sizeof(g_history)/sizeof(char*));
            RETURN(1);
	}
	else if( ! strcmp( argv[ac], "-min_dist" ) )
	{
            ac++;
            CHECK_NEXT_OPT(ac, argc, "-min_dist");
	    min_dist = atof(argv[ac]);
	}
	else if( ! strcmp( argv[ac], "-out_rad" ) )
	{
            ac++;
            CHECK_NEXT_OPT(ac, argc, "-out_rad");
	    out_rad = atof(argv[ac]);
	}
	else if( ! strcmp( argv[ac], "-n_style_sort" ) )
	{
	    style = MAX_SORT_N_REMOVE_STYLE;
	}
	else if( ! strcmp( argv[ac], "-n_style_weight_ave" ) )
	{
	    style = MAX_WEIGHTED_AVE_STYLE;
	}
	else if( ! strcmp( argv[ac], "-neg_ext" ) )
	{
	    negatives = 1;
        }
	else if( ! strcmp( argv[ac], "-no_text" ) )  /* no text output */
	{
	    quiet = 1;
        }
	else if( ! strcmp(argv[ac], "-prefix") )
	{
            ac++;
            CHECK_NEXT_OPT(ac, argc, "-prefix");
	    outfile = argv[ac];
	}
	else if( ! strcmp( argv[ac], "-spheres_1" ) )
	{
	    sval_style = 0;
	}
	else if( ! strcmp( argv[ac], "-spheres_1toN" ) )
	{
	    sval_style = 1;
	}
	else if( ! strcmp( argv[ac], "-spheres_Nto1" ) )
	{
	    sval_style = 2;
	}
	else if( ! strcmp( argv[ac], "-thresh" ) )
	{
            ac++;
            CHECK_NEXT_OPT(ac, argc, "-thresh");
	    cutoff = atof(argv[ac]);
	}
	else if( ! strcmp( argv[ac], "-true_max" ) )
	{
	    true_max = 1;
	}
	else if( ! strcmp( argv[ac], "-ver" ) )
	{
	    puts(MAXIMA_VERSION);
            fflush(stdout);
            RETURN(1);
	}
	else	/* illegal option? */
	{
	    fprintf(stderr,"** unexpected option: '%s'\n",argv[ac]);
	    RETURN(-1);
	}

	opcnt++;
    }

    if( !dset )
    {
        fprintf(stderr,"** must specify input dataset via '-input'\n");
        RETURN(-1);
    }

    if( ( out_rad > 0 ) && ( outfile == NULL ) )
    {
        fprintf(stderr,"** must specify output file to use output radius\n");
        RETURN(-1);
    }

    DSET_load(dset);

    if( ! r_set_afni_s_from_dset( A, dset, debug ) )
        RETURN(-1);

    if( ! init_maxima_s( M, A, outfile ) )
        RETURN(-1);

    /* now fill any remaining parameters */
    M->sval_style   = sval_style;
    M->cutoff       = cutoff / A->factor[0];
    M->min_dist     = min_dist;
    M->out_rad      = out_rad;

    M->negatives    = negatives;
    M->ngbr_style   = style;
    M->quiet        = quiet;
    M->coords_only  = coords_only;
    M->true_max     = true_max;
    M->dicom_coords = dicom_coords;
    M->debug        = debug;

    gr_fac          = A->factor[0];

    if ( M->debug > 0 )
    {
	if ( M->debug > 1 ) show_maxima_s( "plugin values applied ", M );
	fprintf(stderr,"  using sub-brick %d, factor %f (1/%f)\n",
		A->sub_brick, A->factor[0], 1/A->factor[0]);
    }

    RETURN(0);
}
