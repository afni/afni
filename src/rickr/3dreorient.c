#include "mrilib.h"
#include "r_new_resam_dset.h"

#define MAIN

/*----------------------------------------------------------------------
 * 3dreorient - create a new dataset by reorienting and resampling
 *              an existing one 
 *
 * usage:  3dreorient  [options]  -prefix OUTPUT_DSET  -inset INPUT_DSET
 *
 *    options:
 *		-help             : detailed program info
 *              -dxyz DX DY DZ    : resample to a new grid
 *					(DX, DY, DZ are real numbers in mm)
 *		-orient OR_CODE	  : reorient to new orientation code
 *					(a three character string, each
 *					 from the set {A,P, I,S, L,R})
 *		-rmode RESAM      : one of {"NN", "Li", "Cu", "Bk"}
 *
 *    examples:
 *	3dreorient -orient "asl" -rmode NN -prefix asl.dset -inset inset+orig
 *	3dreorient -dxyz 1.0 1.0 0.9 -prefix 119.dset some.input+tlrc
 *
 *----------------------------------------------------------------------
*/

/*--- local stuff ------------------------------------------------------*/

#define USE_LONG	1
#define USE_SHORT	2

#define DELTA_MIN	 0.0
#define DELTA_MAX	99.9

typedef struct
{
    THD_3dim_dataset * dset;
    double             dx, dy, dz;
    char             * orient;
    char             * prefix;
    int                resam;
} options_t;

int initialize_options ( options_t * O, int argc, char * argv [] );
int resam_str2mode     ( char * mode );
int usage              ( char * prog, int level );

/*----------------------------------------------------------------------*/

int main( int argc , char * argv[] )
{
    THD_3dim_dataset * dout;
    options_t          opt;
    int                ret_val;

    mainENTRY("3dreorient"); machdep(); AFNI_logger("3dreorient",argc,argv);

    /* validate inputs and init options structure */
    if ( (ret_val = initialize_options(&opt, argc, argv)) != 0 )
	return ret_val;

    /* actually resample and/or reorient the dataset */
    if ( (dout = r_new_resam_dset( opt.dset, opt.dx, opt.dy, opt.dz,
				   opt.orient, opt.resam ) ) == NULL )
    {
	fprintf( stderr, "failure to resample dataset, exiting...\n" );
	return FAIL;
    }

    return write_results( dout, &opt, argc, argv );

    return 0;
}


/*----------------------------------------------------------------------
 * initialize_options - validate inputs, give help, init options struct
 *----------------------------------------------------------------------
*/
int initialize_options ( options_t * O, int argc, char * argv [] )
{
    int ac;

    /* clear out the options structure */
    memset( O, 0, sizeof(options_t) );

    for ( ac = 1; ac < argc; ac++ )
    {
	if ( ! strncmp(argv[ac], "-help", 2) )
	{
	    usage( argv[0], USE_LONG );
	    return FAIL;
	}
	else if ( ! strncmp(argv[ac], "-dxyz", 3) )	/* dxyz */
	{
	    if ( (ac+3) >= argc )
	    {
		fputs( "option usage: -dxyz DX DY DZ\n", stderr );
		usage( argv[0], USE_SHORT );
		return FAIL;
	    }

	    O->dx = atof(argv[++ac]);
	    O->dy = atof(argv[++ac]);
	    O->dz = atof(argv[++ac]);

	    if ( (O->dx <= DELTA_MIN || O->dx > DELTA_MAX) ||
	         (O->dy <= DELTA_MIN || O->dy > DELTA_MAX) ||
	         (O->dz <= DELTA_MIN || O->dz > DELTA_MAX) )
	    {
		fprintf( stderr, "dxyz must be in (%.1f,%.1f]\n",
			 DELTA_MIN, DELTA_MAX );
		return FAIL;
	    }
	}
	else if ( ! strncmp(argv[ac], "-or", 3) )	/* orientation */
	{
	    if ( (ac+1) >= argc )
	    {
		fputs( "option usage: -orient OR_STRING\n", stderr );
		usage( argv[0], USE_SHORT );
		return FAIL;
	    }

	    O->orient = argv[++ac];
	}
	else if ( ! strncmp(argv[ac], "-rmode", 6) )	/* resample mode */
	{
	    if ( (ac+1) >= argc )
	    {
		fputs( "option usage: -rmode RESAMPLE_MODE\n", stderr );
		usage( argv[0], USE_SHORT );
		return FAIL;
	    }

	    if ( ( (O->resam = resam_str2mode(argv[++ac]) ) < 0 ) ||
		 (  O->resam > LAST_RESAM_TYPE ) )
	    {
		fprintf( stderr, "invalid resample mode <%s>\n", argv[ac] );
		return FAIL;
	    }
	}
	else if ( ! strncmp(argv[ac], "-prefix", 4) )	/* new dset prefix */
	{
	    if ( (ac+1) >= argc )
	    {
		fputs( "option usage: -prefix OUTPUT_PREFIX\n", stderr );
		usage( argv[0], USE_SHORT );
		return FAIL;
	    }

	    O->prefix = argv[++ac];
	    if ( !THD_filename_ok(O->prefix) )
	    {
		fprintf( stderr, "invalid output prefix <%s>\n", O->prefix );
		return usage( argv[0], USE_SHORT );
	    }
	}
	else if ( ! strncmp(argv[ac], "-inset", 3 ) )     /* input dset */
	{
	    if ( (ac+1) >= argc )
	    {
		fputs( "option usage: -inset INPUT_DSET\n", stderr );
		usage( argv[0], USE_SHORT );
		return FAIL;
	    }

	    O->dset = THD_open_dataset( argv[++ac] );
	    if ( ! ISVALID_DSET(O->dset) )
	    {
		fprintf( stderr, "invalid input dataset <%s>\n", argv[ac] );
		return FAIL;
	    }
	}
	else	 /* invalid option */
	{
	    fprintf( stderr, "invalid option <%s>\n", argv[ac] );
	    usage( argv[0], USE_SHORT );
	    return FAIL;
	}
    }

    if ( (O->dset == NULL) || (O->prefix == NULL ) )
    {
	fprintf( stderr, "missing prefix or input dset, exiting...\n" );
	usage( argv[0], USE_SHORT );
	return FAIL;
    }

    return 0;
}

/*----------------------------------------------------------------------*/
int usage ( char * prog, int level )
{
    if ( level == USE_SHORT )
    {
	fprintf( stderr, "usage : %s [options] -prefix OUT_DSET "
		                              "-inset IN_DSET\n", prog );
	return 0;
    }
    else if ( level == USE_LONG )
    {
	printf( "\n"
		"%s - reorient and/or resample a dataset\n"
		"\n"
		"  usage: %s [options] -prefix OUT_DSET -inset IN_DSET\n"
		"\n"
		"  options: \n"
		"    -help            : show this help information\n"
		"\n"
		"    -dxyz DX DY DZ   : resample to new dx, dy and dz\n"
		"          e.g.  -dxyz 1.0 1.0 0.9\n"
		"\n"
		"          Each of DX,DY,DZ must be a positive real number,\n"
		"          and will be used for a voxel delta in the new\n"
		"          dataset (according to any new orientation).\n"
		"\n"
		"    -orient OR_CODE  : reorient to new axis order.\n"
		"          e.g.  -orient asl\n"
		"\n"
		"          The orientation code is a 3 character string,\n"
		"          where the characters come from the respective\n"
		"          sets {A,P}, {I,S}, {L,R}.\n"
		"\n"
		"    -rmode RESAM     : use this resampling method\n"
		"          e.g.  -rmode Linear\n"
		"\n"
		"          The resampling method string RESAM should come\n"
		"          from the set {'NN', 'Li', 'Cu', 'Bk'}.  These\n"
                "          are for 'Nearest Neighbor', 'Linear', 'Cubic'\n"
		"          and 'Blocky' interpolation, respectively.\n"
		"          See 'Anat resam mode' under the 'Define Markers'\n"
		"          window in afni.\n"
		"\n"
		"    -prefix OUT_DSET : required prefix for output dataset\n"
		"          e.g.  -prefix reori.asl.pickle\n"
		"\n"
		"    -inset IN_DSET   : required input dataset to reorient\n"
		"          e.g.  -inset old.dset+orig\n"
		"\n",
		prog, prog );

	return 0;
    }

    fprintf( stderr, "usage called with illegal level <%d>\n", level );

    return FAIL;
}

/*----------------------------------------------------------------------*/
int resam_str2mode ( char * modestr )
{
    int mode;

    for (mode = FIRST_RESAM_TYPE; mode <= LAST_RESAM_TYPE; mode++ )
    {
	if ( ! strncmp( modestr, RESAM_typestr[mode], 2 ) )
	    return mode;
    }

    return FAIL;
}


/*----------------------------------------------------------------------*/
int write_results ( THD_3dim_dataset * dout, options_t * O,
		    int argc, char * argv [] )
{
    /* set filename */
    EDIT_dset_items( dout, ADN_prefix, O->prefix, ADN_none );

    if ( THD_is_file(DSET_HEADNAME(dout)) )
    {
	fprintf( stderr, "error: cannot overwrite existing dataset <%s>\n",
		 DSET_HEADNAME(dout) );
	return FAIL;
    }

    /* set number of time-axis slices to 0 */
    if( DSET_NUM_TTOFF(dout) > 0 )
	EDIT_dset_items( dout, ADN_nsl, 0, ADN_none );

    /* add to old history */
    tross_Copy_History( O->dset , dout );
    tross_Make_History( "3dreorient", argc, argv, dout );


    /* write the output files */
    if ( DSET_write( dout ) != True )
    {
	fprintf( stderr, "failure to write dataset, exiting...\n" );
	return FAIL;
    }

    return 0;
}

