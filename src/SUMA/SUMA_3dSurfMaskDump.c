
#define VERSION "version 1.2 (February 11, 2003)"

/*----------------------------------------------------------------------
 * 3dSurfMaskDump - dump ascii dataset values corresponding to a surface
 *
 * This program is used to display AFNI dataset values that correspond to
 * a surface.  The surface points are mapped to xyz coordinates, according
 * to the SURF_VOL (surface volume) AFNI dataset.  These coordinates are
 * then matched to voxels in other AFNI datasets.  So given any other
 * AFNI dataset, this program can output all of the sub-brick values that
 * correspond to each of the suface locations.  The user also has options
 * to mask regions for output.
 *
 * usage:
 *    3dSurfMaskDump [options] -spec SPEC_FILE -sv SURF_VOL -inset AFNI_DSET
 *
 * options:
 *
 * 	-help
 * 	-version
 *
 * 	-spec       SPEC_FILE
 * 	-sv         SURF_VOL
 * 	-inset      AFNI_DSET
 * 	-cmask      MASK_COMMAND
 * 	-outfile    OUTPUT_FILE
 * 	-debug      LEVEL
 * 	-no_headers
 *
 * examples:
 *
 *    3dSurfMaskDump -spec  SubjA.spec -sv SubjA_anat+orig               \ 
 *                   -inset SubjA_EPI+orig -outfile SubjA.surf.out
 *
 *    3dSurfMaskDump -spec    SubjectA.spec                              \
 *                   -sv      SubjectA_spgr+orig                         \ 
 *                   -cmask   '-a SubjA.func+orig[2] -expr step(a-0.6)'  \
 *                   -inset   SubjA_EPI+orig                             \
 *                   -outfile SubjA.surf.out                        
 *
 *----------------------------------------------------------------------
*/

/*----------------------------------------------------------------------
 * history:
 *
 * 1.2  February 11, 2003
 *   - do not free structs at the end
 *
 * 1.1  February 11, 2003
 *   - handle no arguments as with -help
 *   - minor updates to -help
 *
 * 1.0  February 10, 2003
 *   - initial release
 *----------------------------------------------------------------------
*/

#include "mrilib.h"
#include "SUMA_suma.h"
#include "SUMA_3dSurfMaskDump.h"

/* SUMA globals */
SUMA_SurfaceViewer * SUMAg_SVv = NULL;	/* array of Surf View structs   */
int                  SUMAg_N_SVv = 0;	/* length of SVv array          */
SUMA_DO            * SUMAg_DOv = NULL;	/* array of Displayable Objects */
int                  SUMAg_N_DOv = 0;	/* length of DOv array          */
SUMA_CommonFields  * SUMAg_CF = NULL;	/* info common to all viewers   */


#define MAIN

/*----------------------------------------------------------------------*/

int main( int argc , char * argv[] )
{
    SUMA_SurfSpecFile  spec;
    param_t            params;
    opts_t             opts;
    int                ret_val;

    mainENTRY("3dSurfMaskDump main");
    machdep();
    AFNI_logger("3dSurfMaskDump",argc,argv);

    /* validate inputs and init options structure */
    if ( ( ret_val = init_options(&opts, &params, argc, argv) ) != 0 )
	return ret_val;

    /* read surface files */
    if ( ret_val == 0 )
       	read_surf_files(&opts, &params, &spec);

    /* display relevant values */
    if ( ret_val == 0 )
       	write_output(&opts, &params, &spec);

    /* free memory */
    final_clean_up(&opts, &params, &spec);

    return ret_val;
}


/*----------------------------------------------------------------------
 * write_output - display dataset values for relevant surface nodes
 *
 * For each Displayable Object, if it is not directly mappable, skip it.
 * Otherwise, write the Surface Object data.
 *----------------------------------------------------------------------
*/
int write_output ( opts_t * opts, param_t * p, SUMA_SurfSpecFile * spec )
{
    SUMA_SurfaceObject * so;
    int                  count, socount = 0;

    for ( count = 0; count < SUMAg_N_DOv; count++ )
    {
	if ( ! SUMA_isSO(SUMAg_DOv[count]) )
	    continue;

	so = (SUMA_SurfaceObject *)SUMAg_DOv[count].OP;

	if ( ! SUMA_isINHmappable( so ) )
	    continue;

	if ( opts->debug > 2 )
	{
	    fprintf( stderr, "\n---------- surface #%d -----------",
		     socount );
	    SUMA_Print_Surface_Object( so, stderr );
	}

	/* we have a mappable/displayable object - now show data */
	write_so_data( opts, p, so );

	socount++;
    }

    return 0;
}


/*----------------------------------------------------------------------
 * write_so_data -  write the output for a single surface object
 *
 * display a header for the surface object
 * display column headers
 * for each surface node
 *     get the dicomm coordinates from the surface object
 *     translate to mm, ijk indices and an overall index in the AFNI dset
 *     if this index is masked out, skip it
 *     output surface, volume and ijk indices
 *     extract and display the series from the AFNI dset
 *----------------------------------------------------------------------
*/
int write_so_data ( opts_t * opts, param_t * p, SUMA_SurfaceObject * so )
{
    THD_fvec3   f3mm, f3dicomm;
    THD_ivec3   i3ind;
    MRI_IMAGE * im;
    float     * fser;			/* float series from input dset */
    float     * fp;
    float       x, y, z;
    int         sub, sindex, vindex;
    int         nx, ny, nz, subs;
    
    /* get dimensions and sub-bricks from dset */
    nx   = DSET_NX   (p->dset);
    ny   = DSET_NY   (p->dset);
    nz   = DSET_NZ   (p->dset);
    subs = DSET_NVALS(p->dset);			/* num sub-bricks in dset */

    if ( so->N_Node <= 0 )
	return -1;

    /* display output header - unless the user specified otherwise */
    if ( ! opts->no_headers )
    {
	fprintf( p->outfp,
		 "# --------------------------------------------------\n" );
	fprintf( p->outfp, "# surface '%s' :\n", so->Label );
	fprintf( p->outfp, "#\n" );

	/* output column headers */
	fprintf( p->outfp, "#    node     1dindex    i    j    k     " );
	for ( sub = 0; sub < subs; sub++ )
	    fprintf( p->outfp, "    v%-2d     ", sub );
	fputc( '\n', p->outfp );

	/* underline the column headers */
	fprintf( p->outfp, "#   ------    -------   ---  ---  ---    " );
	for ( sub = 0; sub < subs; sub++ )
	    fprintf( p->outfp, " --------   " );
	fputc( '\n', p->outfp );
    }

    /* note, NodeList elements are in dicomm mm orientation */

    for ( sindex = 0, fp = so->NodeList;
	  sindex < so->N_Node;
	  sindex++, fp += 3 )
    {
	/* locate this voxel - in mm, ijk, and index positions */

	memcpy( f3dicomm.xyz, fp, 3*sizeof(float) );
	f3mm  = THD_dicomm_to_3dmm( p->dset, f3dicomm );
	i3ind = THD_3dmm_to_3dind ( p->dset, f3mm );
	vindex = i3ind.ijk[0] + nx * (i3ind.ijk[1] + ny * i3ind.ijk[2] );

	/* if we don't want this index, skip it */
	if ( p->cmask && !p->cmask[vindex] )
	    continue;

	/* rcr - may want to consider threshold on separate brick */

	/* output surface, volume, and ijk indices */
	fprintf( p->outfp, "  %8d   %8d   %3d  %3d  %3d ",
		 sindex, vindex, i3ind.ijk[0], i3ind.ijk[1], i3ind.ijk[2] );

	/* get series, as float array */
	im   = THD_extract_series( vindex, p->dset, 0 );
	fser = MRI_FLOAT_PTR( im );

	/* hey, these numbers are why I'm writing the program, woohoo! */
	for ( sub = 0; sub < im->nx; sub++ )
	    fprintf( p->outfp, "  %10s", MV_format_fval(fser[sub]) );
	fputc( '\n', p->outfp );
    }

    return 0;
}


/*----------------------------------------------------------------------
 * free memory, close output file
 *----------------------------------------------------------------------
*/
int final_clean_up ( opts_t * opts, param_t * p, SUMA_SurfSpecFile * spec )
{
#if 0
    if ( SUMA_Free_Displayable_Object_Vect(SUMAg_DOv, SUMAg_N_DOv) == 0 )
	fprintf( stderr, "** failed SUMA_Free_Displayable_Object_Vect()\n" );

    if ( SUMA_Free_SurfaceViewer_Struct_Vect(SUMAg_SVv, SUMAg_N_SVv) == 0 )
	fprintf( stderr, "** failed SUMA_Free_SurfaceViewer_Struct_Vect()\n" );

    if ( SUMA_Free_CommonFields(SUMAg_CF) == 0 )
	fprintf( stderr, "** failed SUMA_Free_CommonFields()\n" );
#endif

    fclose( p->outfp );

    return 0;
}


/*----------------------------------------------------------------------
 * read surfaces (much stolen from SUMA_suma.c - thanks Ziad!)
 *----------------------------------------------------------------------
*/
int read_surf_files ( opts_t * opts, param_t * p, SUMA_SurfSpecFile * spec )
{
    if ( opts->debug > 2 )
	fputs( "-- SUMA_Create_CommonFields()...\n", stderr );

    /* initialize common fields struct */
    SUMAg_CF = SUMA_Create_CommonFields();

    if ( SUMAg_CF == NULL )
    {
	fprintf( stderr, "** failed SUMA_Create_CommonFields(), exiting...\n" );
	return -1;
    }

    /* for SUMA type notifications */
    if ( opts->debug > 3 )
	SUMAg_CF->InOut_Notify = 1;

    if ( opts->debug > 2 )
	fputs( "-- SUMA_Alloc_DisplayObject_Struct()...\n", stderr );

    SUMAg_DOv = SUMA_Alloc_DisplayObject_Struct(SUMA_MAX_DISPLAYABLE_OBJECTS);

    if ( opts->debug > 2 )
	fputs( "-- SUMA_Read_SpecFile()...\n", stderr );

    if ( SUMA_Read_SpecFile( opts->spec_file, spec) == 0 )
    {
	fprintf( stderr, "** failed SUMA_Read_SpecFile(), exiting...\n" );
	return -1;
    }

    /* make sure only group was read from spec file */
    if ( spec->N_Groups != 1 )
    {
	fprintf( stderr,"** error: N_Groups <%d> must be 1 in spec file <%s>\n",
		 spec->N_Groups, opts->spec_file );
	return -1;
    }

    if ( opts->debug > 2 )
	fputs( "-- SUMA_LoadSpec()...\n", stderr );

    /* actually load the surface(s) from the spec file */
    if ( SUMA_LoadSpec(spec, SUMAg_DOv, &SUMAg_N_DOv, opts->sv_file) == 0 )
    {
	fprintf( stderr, "** error: failed SUMA_LoadSpec(), exiting...\n" );
	return -1;
    }

    if ( opts->debug > 2 )
	fputs( "++ surfaces loaded.\n", stderr );

    return 0;
}


/*----------------------------------------------------------------------
 * init_options - validate inputs, give help, init options struct
 *----------------------------------------------------------------------
*/
int init_options ( opts_t * opts, param_t * p, int argc, char * argv [] )
{
    int ac;

    if ( argc < 2 )
    {
	usage( PROG_NAME, SMD_USE_LONG );
	return -1;
    }

    /* clear out the options and parameter structures */
    memset( opts, 0, sizeof( opts_t) );
    memset( p,    0, sizeof(param_t) );

    for ( ac = 1; ac < argc; ac++ )
    {
	/* do help first, the rest alphabetically */
	if ( ! strncmp(argv[ac], "-help", 2) )
	{
	    usage( PROG_NAME, SMD_USE_LONG );
	    return -1;
	}
	else if ( ! strncmp(argv[ac], "-cmask", 6) )
	{
	    if ( (ac+1) >= argc )
	    {
		fputs( "option usage: -cmask COMMAND\n\n", stderr );
		usage( PROG_NAME, SMD_USE_SHORT );
		return -1;
	    }

	    opts->cmask_cmd = argv[++ac];
	}
	else if ( ! strncmp(argv[ac], "-debug", 6) )
	{
	    if ( (ac+1) >= argc )
	    {
		fputs( "option usage: -debug LEVEL\n\n", stderr );
		usage( PROG_NAME, SMD_USE_SHORT );
		return -1;
	    }

	    opts->debug = atoi(argv[++ac]);
	    if ( opts->debug < 0 || opts->debug > SMD_MAX_DEBUG )
	    {
		fprintf( stderr, "bad debug level <%d>, should be in [0,%d]\n",
			opts->debug, SMD_MAX_DEBUG );
		usage( PROG_NAME, SMD_USE_SHORT );
		return -1;
	    }
	}
	else if ( ! strncmp(argv[ac], "-inset", 3) )  /* -inset or -input */
	{
	    if ( (ac+1) >= argc )
	    {
		fputs( "option usage: -inset INPUT_DSET\n\n", stderr );
		usage( PROG_NAME, SMD_USE_SHORT );
		return -1;
	    }

	    opts->dset_file = argv[++ac];
	    p->dset         = THD_open_dataset( opts->dset_file );
	}
	else if ( ! strncmp(argv[ac], "-mrange", 5) )
	{
	    if ( (ac+2) >= argc )
	    {
		fputs( "option usage: -mrange BOTTOM TOP\n\n", stderr );
		usage( PROG_NAME, SMD_USE_SHORT );
		return -1;
	    }

	    opts->range_bot = atoi(argv[++ac]);
	    opts->range_top = atoi(argv[++ac]);
	    if ( ( opts->range_top < opts->range_bot ) ||
		 ( ( opts->range_top == opts->range_bot ) &&
		   ( opts->range_top == 0 ) ) )
	    {
		fprintf( stderr, "illegal arguments to -mrange : (%d,%d)\n",
			 opts->range_bot, opts->range_top );
		usage( PROG_NAME, SMD_USE_SHORT );
		return -1;
	    }
	}
	else if ( ! strncmp(argv[ac], "-no_headers", 5) )
	{
            opts->no_headers = 1;
	}
	else if ( ! strncmp(argv[ac], "-outfile", 2) )
	{
	    if ( (ac+1) >= argc )
            {
		fputs( "option usage: -outfile OUTFILE_NAME\n\n", stderr );
		usage( PROG_NAME, SMD_USE_SHORT );
		return -1;
	    }

            opts->out_file = argv[++ac];
	}
	else if ( ! strncmp(argv[ac], "-spec", 3) )
	{
	    if ( (ac+1) >= argc )
            {
		fputs( "option usage: -spec SPEC_FILE\n\n", stderr );
		usage( PROG_NAME, SMD_USE_SHORT );
		return -1;
	    }

            opts->spec_file = argv[++ac];
	}
	else if ( ! strncmp(argv[ac], "-sv", 3) )
	{
	    if ( (ac+1) >= argc )
            {
		fputs( "option usage: -sv SURFACE_VOLUME\n\n", stderr );
		usage( PROG_NAME, SMD_USE_SHORT );
		return -1;
	    }

            opts->sv_file = argv[++ac];
	}
	else if ( ! strncmp(argv[ac], "-version", 2) )
	{
	    usage( PROG_NAME, SMD_USE_VERSION );
	    return -1;
	}
	else	 /* invalid option */
	{
	    fprintf( stderr, "invalid option <%s>\n", argv[ac] );
	    usage( PROG_NAME, SMD_USE_SHORT );
	    return -1;
	}
    }

    if ( validate_options( opts, p ) != 0 )
	return -1;

    if ( opts->debug > 1 )
    {
	disp_opts_t ( "++ opts validated: ", opts );
	disp_param_t( "++ opts validated: ", p );
    }

    return 0;
}

/*----------------------------------------------------------------------
 * validate_options
 *
 *     - validate output file (it should not yet exist)
 *     - validate datasets
 *     - validate surface
 *----------------------------------------------------------------------
*/
int validate_options ( opts_t * opts, param_t * p )
{
    if ( opts->debug > 2 )
    {
	disp_opts_t ( "++ opts read: ", opts );
	disp_param_t( "++ opts read: ", p );
    }

    /* open any output file, or use stdout (if one was not specified) */
    if ( opts->out_file == NULL )
	p->outfp = stdout;
    else if ( strcmp( opts->out_file, "stderr" ) == 0 )
	p->outfp = stderr;  			/* special case, shhh... */
    else
    {
	if ( THD_is_file(opts->out_file) )
	{
	    fprintf( stderr, "** error: output file '%s' already exists\n",
		     opts->out_file );
	    return -1;
	}

	p->outfp = fopen( opts->out_file, "w" );
	if ( p->outfp == NULL )
	{
	    fprintf( stderr, "** failure: cannot open %s for writing\n",
		     opts->out_file );
	    return -1;
	}
    }

    if ( validate_datasets( opts, p ) != 0 )
    {
	fclose(p->outfp);
	return -1;
    }

    if ( validate_surface( opts, p ) != 0 )
    {
	fclose(p->outfp);
	return -1;
    }

    return 0;
}


/*----------------------------------------------------------------------
 * validate_surface
 *
 * Verify that the user entered options for both the spec file and
 * the surface volume (AFNI dataset).
 *----------------------------------------------------------------------
*/
int validate_surface ( opts_t * opts, param_t * p )
{
    int errs = 0;

    if ( opts->spec_file == NULL )
    {
	fprintf( stderr, "** missing '-spec_file SPEC_FILE' option\n" );
	errs++;
    }

    if ( opts->sv_file == NULL )
    {
	fprintf( stderr, "** missing '-sv SURF_VOL' option\n" );
	errs++;
    }

    if ( errs > 0 )
	return -1;

    return 0;
}

/*----------------------------------------------------------------------
 * validate_datasets
 *
 * Note that we do not validate the SURFACE_VOLUME AFNI dataset here.
 * That is done in SUMA_LoadSpec().
 *
 * Verify the AFNI dataset used for value output.
 * Check for a cmask dataset and command.
 * Verify that AFNI dataset and the mask have the same size.
 *----------------------------------------------------------------------
*/
int validate_datasets ( opts_t * opts, param_t * p )
{
    if ( !ISVALID_DSET(p->dset) )
    {
	if ( opts->dset_file == NULL )
	    fprintf( stderr, "** error: missing '-inset DSET' option\n" );
	else
	    fprintf( stderr, "** error: invalid input dataset '%s'\n",
		     opts->dset_file);
	return -1;
    }
    else if ( DSET_BRICK_TYPE(p->dset, 0) == MRI_complex )
    {
	fprintf(stderr,
		"** failure: cannot deal with complex-valued dataset, '%s'\n",
		opts->dset_file);
	return -1;
    }

    p->nvox = DSET_NVOX( p->dset );

    /* check for cmask - casually stolen from 3dmaskdump.c (thanks, Bob! :) */
    if ( opts->cmask_cmd != NULL )
    {
	int    clen = strlen( opts->cmask_cmd );
	char * cmd;

	/* save original cmask command */
	cmd = (char *)malloc((clen + 1) * sizeof(char));
	strcpy( cmd, opts->cmask_cmd );

	p->cmask = EDT_calcmask( opts->cmask_cmd, &p->ncmask );

	opts->cmask_cmd = cmd;		/* just forget the argv pointer */

	if ( p->cmask == NULL )
	{
	    fprintf( stderr, "** failure: cannot compute mask from option:\n"
		     "   -cmask '%s'\n", opts->cmask_cmd );
	    return -1;
	}
	if ( p->ncmask != p->nvox )
	{
	    fprintf( stderr, "** error: input and cmask datasets do not have "
		     "the same dimensions\n" );
	    return -1;
	}
	if ( ( p->ccount = THD_countmask( p->ncmask, p->cmask ) ) <= 0 )
	{
	    fprintf( stderr, "** no voxels in computed cmask, exiting...\n" );
	    return -1;
	}
    }

    if ( opts->debug > 0 )
	fprintf( stderr, "++ input dset has nvox = %d, nvals = %d\n",
		 p->nvox, DSET_NVALS(p->dset) );

    return 0;
}

/*----------------------------------------------------------------------
 * usage  -  output usage information
 *
 * SMD_USE_SHORT	- display brief output
 * SMD_USE_LONG		- display long output
 * SMD_USE_VERSION	- show the VERSION of the program
 *----------------------------------------------------------------------
*/
int usage ( char * prog, int level )
{
    if ( level == SMD_USE_SHORT )
    {
	fprintf( stderr,
		 "usage: %s [options] -spec SPEC_FILE -sv SURF_VOL "
		                    " -inset AFNI_DSET\n"
		 "usage: %s -help\n",
		 prog, prog );
	return 0;
    }
    else if ( level == SMD_USE_LONG )
    {
	printf(
	    "\n"
	    "%s - dump ascii dataset values corresponding to a surface\n"
	    "\n"
	    "    This program can be used to display, in ascii format, values\n"
	    "    from an AFNI volume corresponding to locations on a surface.\n"
	    "    AFNI volume masks can be applied as well.\n"
	    "\n"
	    "    Each suface node (that is in any applied mask) gets one row\n"
	    "    of output.  The information displayed for that node includes\n"
	    "\n"
	    "        - the node index\n"
	    "        - the AFNI BRIK index\n"
	    "        - the AFNI ijk indices (corresponding to the BRIK index)\n"
	    "        - each AFNI sub-brick value, corresponding to the node\n"
	    "\n"
	    "  usage: %s [options] -spec SPEC_FILE -sv SURF_VOL "
	                                          "-inset AFNI_DSET\n"
	    "\n"
	    "  examples:\n"
	    "\n"
	    "    %s                       \\\n"
	    "       -spec    fred.spec                \\\n"
	    "       -sv      fred_anat+orig           \\\n"
	    "       -inset   fred_epi+orig            \\\n"
	    "       -output  fred_surf_func.txt\n"
	    "\n"
	    "    %s                       \\\n"
	    "       -spec    fred.spec                \\\n"
	    "       -sv      fred_anat+orig           \\\n"
	    "       -inset  'fred_epi+orig[10..30]'   \\\n"
	    "       -output  fred_surf_func.txt\n"
	    "\n"
	    "    %s                                        \\\n"
	    "       -spec    fred.spec                                 \\\n"
	    "       -sv      fred_anat+orig                            \\\n"
	    "       -inset  'fred_func+orig[0..3]'                     \\\n"
	    "       -cmask  '-a fred_func+orig[2] -expr step(a-0.8)'   \\\n"
	    "       -output  fred_surf_func_thresh_8.txt               \\\n"
	    "       -no_headers\n"
	    "\n"
	    "  REQUIRED COMMAND ARGUMENTS:\n"
	    "\n"
	    "    -spec SPEC_FILE        : (REQUIRED) SUMA spec file\n"
	    "\n"
	    "        e.g. -spec fred.spec\n"
	    "\n"
	    "        The surface specification file contains the list of\n"
	    "        mappable surfaces that are used.\n"
	    "\n"
	    "        See @SUMA_Make_Spec_FS and @SUMA_Make_Spec_SF.\n"
	    "\n"
	    "    -sv SURFACE_VOLUME     : (REQUIRED) AFNI dataset\n"
	    "\n"
	    "        e.g. -sv fred_anat+orig\n"
	    "\n"
	    "        This is the AFNI dataset that the surface is mapped to.\n"
	    "        This dataset is used for the intial surface node to xyz\n"
	    "        coordinate mapping, in the Dicomm orientation.\n"
	    "\n"
	    "    -inset AFNI_DSET       : (REQUIRED) AFNI dataset\n"
	    "\n"
	    "        e.g. -inset fred_function+orig\n"
	    "\n"
	    "        The values in this AFNI dataset are what the user wants\n"
	    "        to output.  Given a (masked?) surface, each node will\n"
	    "        be output on a single line, with column values coming\n"
	    "        from this AFNI dataset.\n"
	    "\n"
	    "        The user may apply sub-brick selectors on the command\n"
	    "        line.  In that case, the dataset name should be enclosed\n"
	    "        in single quotes, so the shell does not try to interpret\n"
	    "        those special characters.\n"
	    "\n"
	    "        e.g. -inset 'fred_function+orig[0,3,9]'\n"
	    "\n"
	    "  options:\n"
	    "\n"
	    "    -cmask MASK_COMMAND    : (optional) command for dataset mask\n"
	    "\n"
	    "        e.g. -cmask '-a fred_func+orig[2] -expr step(a-0.8)'\n"
	    "\n"
	    "        This option will produce a mask to be applied to the\n"
	    "        dataset from the '-inset ANFI_DSET' option.  Note that\n"
	    "        this mask should form a single sub-brick, according to\n"
	    "        any supplied expression (with '-expr').\n"
	    "\n"
	    "        This option follows the style of 3dmaskdump (since the\n"
	    "        code for it was, uh, borrowed from there (thanks Bob!)).\n"
	    "\n"
	    "        See '3dmaskdump -help' for more information.\n"
	    "\n"
	    "    -debug LEVEL\n"
	    "\n"
	    "        e.g. -debug 2      :  (optional) verbose output\n"
	    "\n"
	    "        This option is used to print out status information \n"
	    "        during the execution of the program.  Current levels are\n"
	    "        from 0 to 4.\n"
	    "\n"
	    "    -help                  : show this help\n"
	    "\n"
	    "        If you can't get help here, please get help somewhere.\n"
	    "\n"
	    "    -no_headers            : do not write headers to output file\n"
	    "\n"
	    "        Do not display headers in the output file.  This option\n"
	    "        useful if the user wants to pipe the output into another\n"
	    "        program.\n"
	    "\n"
	    "        This option is also useful for keeping Mike B happy, and\n"
	    "        that's what really matters.\n"
	    "\n"
	    "    -outfile OUTPUT_FILE   : specify a file for the output\n"
	    "\n"
	    "        e.g. -outfile some_output_file\n"
	    "        e.g. -outfile mask_values_over_dataset.txt\n"
	    "        e.g. -outfile stderr\n"
	    "        default: write to stdout\n"
	    "\n"
	    "        This is where the user will specify which file they want\n"
	    "        the output to be written to.  Note that the output file\n"
	    "        should not yet exist.\n"
	    "\n"
	    "        Two special (valid) cases are stdout and stderr, either\n"
	    "        of which may be specified.\n"
	    "\n"
	    "    -version               : show version information\n"
	    "\n"
	    "        Show version and compile date.\n"
	    "\n"
	    "\n"
	    "  Author: R. Reynolds  - %s\n"
	    "\n"
	    "                (many thanks to Z. Saad and R.W. Cox)\n"
	    "\n",
	    prog, prog,
	    prog, prog, prog,
	    VERSION );

	return 0;
    }
    else if ( level == SMD_USE_VERSION )
    {
	printf( "%s : %s, compile date: %s\n", prog, VERSION, __DATE__ );
	return 0;
    }

    fprintf( stderr, "usage called with illegal level <%d>\n", level );

    return -1;
}


/*----------------------------------------------------------------------
 * disp_opts_t  -  display the contents of the opts_t struct
 *----------------------------------------------------------------------
*/
int disp_opts_t ( char * info, opts_t * opts )
{
    if ( info )
	fputs( info, stdout );

    if ( opts == NULL )
    {
	printf( "disp_opts_t: opts == NULL\n" );
	return -1;
    }

    printf( "options struct at %p :\n"
	    "    dset_file              = %s\n"
	    "    out_file               = %s\n"
	    "    spec_file              = %s\n"
	    "    sv_file                = %s\n"
	    "    cmask_cmd              = %s\n"
	    "    (range_bot, range_top) = (%d, %d)\n"
	    "    (debug, no_headers)    = (%d, %d)\n",
	    opts,
	    opts->dset_file, opts->out_file, opts->spec_file, opts->sv_file,
	    opts->cmask_cmd, opts->range_bot, opts->range_top,
	    opts->debug, opts->no_headers
	    );

    return 0;
}

/*----------------------------------------------------------------------
 * disp_param_t  -  display the contents of the param_t struct
 *----------------------------------------------------------------------
*/
int disp_param_t ( char * info, param_t * p )
{
    if ( info )
	fputs( info, stdout );

    if ( p == NULL )
    {
	printf( "disp_param_t: p == NULL\n" );
	return -1;
    }

    printf( "param_t struct at %p :\n"
	    "    dset  : vcheck   = %p : %s\n"
	    "    outfp : fileno   = %p : %d\n"
	    "    nvox             = %d\n"
	    "    cmask            = %p\n"
	    "    (ncmask, ccount) = (%d, %d)\n",
	    p,
	    p->dset, ISVALID_DSET(p->dset) ? "valid" : "invalid",
	    p->outfp, (p->outfp == NULL) ? -1 : fileno(p->outfp),
	    p->nvox, p->cmask, p->ncmask, p->ccount
	    );

    return 0;
}

