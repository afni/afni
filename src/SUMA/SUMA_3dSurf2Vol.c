
#define VERSION "version 1.1 (June 12, 2003)"

/*----------------------------------------------------------------------
 * 3dSurf2Vol - create an AFNI volume dataset from a surface
 *
 * This program is meant to take as input a surface and an AFNI dataset,
 * and to output a new dataset consisting of the surface mapped to the
 * dataset grid space.
 *
 * The surface points are mapped to xyz coordinates, according to the
 * SURF_VOL (surface volume) AFNI dataset.  These coordinates are then
 * matched to voxels in the grid parent AFNI dataset.  The output dataset
 * will have the same grid as the grid parent dataset, with values coming
 * from the surface, and subject to any mask (see -cmask).
 *
 * usage:
 *    3dSurf2Vol [options] -spec SPEC_FILE -sv SURF_VOL -grid_parent AFNI_DSET
 *
 * options:
 *
 * 	-help
 * 	-version
 *
 * 	-spec                SPEC_FILE
 * 	-sv                  SURF_VOL
 * 	-grid_parent         AFNI_DSET
 * 	-prefix              OUTPUT_DSET
 *      -map_func            MAP_FUNC
 *
 * 	-cmask               MASK_COMMAND
 * 	-datum               D_TYPE
 * 	-debug               LEVEL
 * 	-noscale
 *
 * 	-m2_steps            NUM_MASK2_STEPS
 *
 * examples:
 *
 *    3dSurf2Vol -spec        SubjectA.spec                              \
 *               -sv          SubjectA_spgr+orig                         \ 
 *               -grid_parent SubjA_EPI+orig                             \
 *               -map_func    mask2                                      \
 *               -prefix      SubjA_mask
 *
 *    3dSurf2Vol -spec        SubjectA.spec                              \
 *               -sv          SubjectA_spgr+orig                         \ 
 *               -cmask       '-a SubjA.func+orig[2] -expr step(a-0.6)'  \
 *               -grid_parent SubjA_EPI+orig                             \
 *               -map_func    mask2                                      \
 *               -debug       2                                          \
 *               -prefix      SubjA_mask
 *
 *----------------------------------------------------------------------
*/

/*----------------------------------------------------------------------
 * history:
 *
 * 1.1  June 11, 2003
 *   - small reorg of s2v_fill_mask2() (should have no effect)
 *   - improve description of -m2_steps option
 *
 * 1.0  May 29, 2003
 *   - initial release
 *----------------------------------------------------------------------
*/

#include "mrilib.h"
#include "SUMA_suma.h"
#include "SUMA_3dSurf2Vol.h"

/* globals */
SUMA_SurfaceViewer * SUMAg_SVv = NULL;	/* array of Surf View structs   */
int                  SUMAg_N_SVv = 0;	/* length of SVv array          */
SUMA_DO            * SUMAg_DOv = NULL;	/* array of Displayable Objects */
int                  SUMAg_N_DOv = 0;	/* length of DOv array          */
SUMA_CommonFields  * SUMAg_CF = NULL;	/* info common to all viewers   */

/* this must match s2v_map_num enum */
char * gs2v_map_names[] = { "none", "mask", "mask2", "ave", "count",
    			    "min", "max" };



/* AFNI prototype */
extern void machdep( void );

#define MAIN

/*----------------------------------------------------------------------*/

int main( int argc , char * argv[] )
{
    SUMA_SurfSpecFile  spec;
    node_list_t        node_list = {NULL, 0, 0};
    param_t            params;
    s2v_opts_t         sopt;
    opts_t             opts;
    int                ret_val;

    mainENTRY("3dSurf2Vol main");
    machdep();
    AFNI_logger("3dSurf2Vol",argc,argv);

    /* validate inputs and init options structure */
    if ( ( ret_val = init_options(&opts, argc, argv) ) != 0 )
	return ret_val;

    if ( ( ret_val = validate_options(&opts, &params) ) != 0 )
	return ret_val;

    if ( (ret_val = set_map_opts( &opts, &params, &sopt )) != 0 )
	return ret_val;

    /* read surface files */
    ret_val = read_surf_files(&opts, &params, &spec);

    /*  get node list from surfaces (multiple points per node)
     *  need merge function
     */
    if ( ret_val == 0 )
	ret_val = create_node_list( &sopt, &node_list );

    if ( ret_val == 0 )
	ret_val = write_output( &sopt, &opts, &params, &node_list, argc, argv );

    /* free memory */
    final_clean_up(&opts, &params, &spec, &node_list);

    return ret_val;
}


/*----------------------------------------------------------------------
 * write_output - create and write a new afni dataset
 *----------------------------------------------------------------------
*/
int write_output ( s2v_opts_t * sopt, opts_t * opts, param_t * p,
	           node_list_t * N, int argc, char * argv[] )
{
    if ( sopt == NULL || opts == NULL || p == NULL || N == NULL )
    {
	fprintf( stderr, "** s2v_wo - bad params (%p,%p,%p,%p)\n",
		 sopt, opts, p, N );
	return -1;
    }

    p->oset = s2v_nodes2volume( N, p->gpar, p->cmask, sopt );

    if ( p->oset == NULL )
	return -1;

    EDIT_dset_items( p->oset, ADN_prefix, opts->oset_file, ADN_none );
    if ( THD_is_file(DSET_HEADNAME(p->oset)) )
    {
	fprintf( stderr, "** cannot overwrite existing dataset '%s'\n",
		 DSET_HEADNAME(p->oset) );
	DSET_delete( p->oset );
	return -1;
    }

    tross_Copy_History( p->gpar, p->oset );
    tross_Make_History( PROG_NAME, argc, argv, p->oset );

    if ( DSET_write( p->oset ) != True )
    {
	fprintf( stderr, "** failed to write dataset '%s', exiting...\n",
		 opts->oset_file );
	return -1;
    }
    
    return 0;
}


/*----------------------------------------------------------------------
 * s2v_nodes2volume	- create an AFNI dataset from the node_list
 * 			  and map type (subject to any mask).
 *
 * inputs:
 * 		N	- xyz coordinate node list for surfaces
 * 		gpar    - AFNI dataset to base output upon
 * 		cmask   - a mask that may be applied to the output
 * 		map	- the surface to dataset mapping type
 *
 * return  AFNI dataset - on success
 *         NULL         - on failure
 *----------------------------------------------------------------------
*/
THD_3dim_dataset * s2v_nodes2volume( node_list_t * N, THD_3dim_dataset * gpar,
				     byte * cmask, s2v_opts_t * sopt )
{
    THD_3dim_dataset * dout;
    float            * fdata, fac;
    void             * vdata = NULL;
    int                nvox, dsize, valid;

    if ( N == NULL || ! ISVALID_DSET(gpar) )
    {
	fprintf( stderr, "** s2v_nodes2volume: bad params (%p,%p)\n", N, gpar);
	return NULL;
    }

    dout = EDIT_empty_copy( gpar );
    if ( ! ISVALID_3DIM_DATASET( dout ) )
    {
	fprintf( stderr, "** failed EDIT_empty_copy()\n" );
	return NULL;
    }

    /* output dataset will have one sub-brick (for now) */
    EDIT_dset_items( dout, ADN_nvals,     1,
	    		   ADN_datum_all, sopt->datum,
		     ADN_none );

    if ( sopt->debug > 1 )
	fprintf( stderr, "++ creating dataset '%s' of type '%s', nvals = %d\n",
		 DSET_HEADNAME(dout), MRI_TYPE_name[sopt->datum], 1 );

    nvox  = DSET_NVOX(gpar);

    /* allocate a computational sub-brick of floats */
    if ( (fdata = (float *)calloc(nvox, sizeof(float))) == NULL )
    {
	fprintf( stderr, "** failed to allocate %d floats for computation\n",
		 nvox );
	DSET_delete( dout );
	return NULL;
    }
    
    dsize = mri_datum_size(sopt->datum);
    if ( (vdata = malloc( nvox * dsize )) == NULL )
    {
	fprintf( stderr, "** failed to allocate %d bytes for vdata\n",
		 nvox * dsize );
	free( fdata );
	DSET_delete( dout );
    }

    valid = 0;

    switch (sopt->map)
    {
	case S2V_MAP_AVE:
	case S2V_MAP_COUNT:
	case S2V_MAP_MAX:
	case S2V_MAP_MIN:
	{
	    fprintf( stderr, "** function '%s' coming soon ...\n",
		     gs2v_map_names[sopt->map] );
	    break;
	}

	case S2V_MAP_MASK2:
	{
	    if ( s2v_fill_mask2( N, gpar, fdata, cmask, sopt ) == 0 )
		valid = 1;
	    break;
	}

	case S2V_MAP_MASK:
	{
	    if ( s2v_fill_mask( N, gpar, fdata, cmask, sopt ) == 0 )
		valid = 1;
	    break;
	}

	default:
	    fprintf( stderr, "** s2v_n2v: unknown map %d\n", sopt->map );
	    break;
    }

    if ( ! valid )	/* then clean up memory */
    {
	free( fdata );
	free( vdata );
	DSET_delete( dout );
	return NULL;
    }

    /* convert to output data type */
    fac = 1.0;

    /* for int output and scaling, fac is maxval/maxabs */
    if ( MRI_IS_INT_TYPE( sopt->datum ) && !sopt->noscale )
    {
	float amax = MCW_vol_amax( nvox, 1, 1, MRI_float, fdata );
	if ( amax != 0.0 )
	    fac = MRI_TYPE_maxval[sopt->datum]/amax;
    }

    EDIT_coerce_scale_type( nvox, fac, MRI_float, fdata, sopt->datum, vdata );

    /* attach the final vdata to the dataset */
    EDIT_substitute_brick( dout, 0, sopt->datum, vdata );
    DSET_BRICK_FACTOR( dout, 0 ) = 0.0;

    free(fdata);

    return dout;
}


/*----------------------------------------------------------------------
 * s2v_fill_mask2   - mask 2-surface region to dataset
 *
 * return     0 : success
 * 	   else : failure
 *----------------------------------------------------------------------
*/
int s2v_fill_mask2( node_list_t * N, THD_3dim_dataset * gpar,
		    float * fdata, byte * mask, s2v_opts_t * sopt )
{
    THD_fvec3 * fvp;
    THD_fvec3 * f3mm;
    THD_ivec3   i3ind;
    float     * fp, *f0, *fn, *fs;
    float       rat0, ratn;			 /* distance ratios      */
    int         steps   = sopt->m2_steps;
    int         s_min_1 = sopt->m2_steps - 1;    /* repeated computation */
    int         vindex, node, scount;
    int         nx, ny;
    int		mcount, fcount;

    if ( N == NULL || gpar == NULL || fdata == NULL )
    {
	fprintf( stderr, "** s2vfm - bad params (%p,%p,%p)\n",
		 N, gpar, fdata );
	return -1;
    }

    if ( N->depth != 2 )
    {
	fprintf( stderr, "** s2vf mask2: depth (%d) should be 2\n", N->depth );
	return -1;
    }

    if ( (f3mm = (THD_fvec3 *)malloc((steps) * sizeof(THD_fvec3))) == NULL )
    {
	fprintf( stderr, "** s2vfm2: failed to allocate %d fvec3's\n", steps);
	return -1;
    }

    nx = DSET_NX( gpar );
    ny = DSET_NY( gpar );

    mcount = fcount = 0;
    for ( node = 0; node < N->nnodes; node++ )
    {
	/* note the first and last locations */
	f3mm[0]       = THD_dicomm_to_3dmm( gpar, N->nodes[node] );
	f3mm[s_min_1] = THD_dicomm_to_3dmm( gpar, N->nodes[node+N->nnodes] );

	f0 = f3mm[0].xyz;
	fn = f3mm[s_min_1].xyz;

	if ( sopt->debug > 1 && node == S2V_DEBUG_TEST_NODE )
	    fprintf( stderr, "++ node %d : %f ", S2V_DEBUG_TEST_NODE, f0[0] );

	/* now fill all intermediate f3mm points */
	for ( scount = 1; scount < s_min_1; scount ++ )
	{
	    /* for ease of typing, note xyz address */
	    fs = f3mm[scount].xyz;

	    /* note portions of endpoints */
	    ratn = (float)scount / s_min_1;
	    rat0 = 1.0 - ratn;

	    fs[0] = rat0 * f0[0] + ratn * fn[0];
	    fs[1] = rat0 * f0[1] + ratn * fn[1];
	    fs[2] = rat0 * f0[2] + ratn * fn[2];

	    if ( sopt->debug > 1 && node == S2V_DEBUG_TEST_NODE )
		    fprintf( stderr, "%f ", fs[0] );
	}

	if ( sopt->debug > 1 && node == S2V_DEBUG_TEST_NODE )
		fprintf( stderr, "%f\n            ", fn[0] );

	/* for each point, get the index and fill */
	for ( scount = 0; scount < steps; scount++ )
	{
	    i3ind  = THD_3dmm_to_3dind ( gpar, f3mm[scount] );
	    vindex = i3ind.ijk[0] + nx * (i3ind.ijk[1] + ny * i3ind.ijk[2] );

	    if ( sopt->debug > 1 && node == S2V_DEBUG_TEST_NODE )
		    fprintf( stderr, "%d ", vindex );

	    if ( (mask != NULL) && (mask[vindex] == 0) ) /* do we skip this? */
		continue;

	    /* assign value - this is just a mask */
	    fdata[vindex] = 1.0;
	}

	if ( sopt->debug > 1 && node == S2V_DEBUG_TEST_NODE )
	    fputc( '\n', stderr );
    }

    free(f3mm);

    return 0;
}


/*----------------------------------------------------------------------
 * s2v_fill_mask   - simple masking of surface to dataset
 *
 * return     0 : success
 * 	   else : failure
 *----------------------------------------------------------------------
*/
int s2v_fill_mask( node_list_t * N, THD_3dim_dataset * gpar,
		   float * fdata, byte * mask, s2v_opts_t * sopt )
{
    THD_fvec3 * fvp, f3mm;
    THD_ivec3   i3ind;
    float     * fp;
    int         vindex, node;
    int         nx, ny;
    int		mcount, fcount;

    if ( N == NULL || gpar == NULL || fdata == NULL )
    {
	fprintf( stderr, "** s2vfm - bad params (%p,%p,%p)\n",
		 N, gpar, fdata );
	return -1;
    }

    nx = DSET_NX( gpar );
    ny = DSET_NY( gpar );

    mcount = fcount = 0;
    for ( node = 0; node < N->nnodes; node++ )
    {
	f3mm   = THD_dicomm_to_3dmm( gpar, N->nodes[node] );
	i3ind  = THD_3dmm_to_3dind ( gpar, f3mm );
	vindex = i3ind.ijk[0] + nx * (i3ind.ijk[1] + ny * i3ind.ijk[2] );

	if ( (mask != NULL) && (mask[vindex] == 0) ) /* do we skip this? */
	    continue;

	/* assign value - this is just a mask */
	fdata[vindex] = 1.0;
    }

    return 0;
}


/*----------------------------------------------------------------------
 * create_node_list	- from surfaces
 *
 *----------------------------------------------------------------------
*/
int create_node_list ( s2v_opts_t * sopt, node_list_t * N )
{
    if ( sopt == NULL || N == NULL )
    {
	fprintf( stderr, "** cnl - bad params (%p,%p)\n", sopt, N );
	return -1;
    }

    switch (sopt->map)
    {
	case S2V_MAP_AVE:
	case S2V_MAP_COUNT:
	case S2V_MAP_MAX:
	case S2V_MAP_MIN:
	    fprintf( stderr, "** function '%s' coming soon ...\n",
		     gs2v_map_names[sopt->map] );
	    break;

	case S2V_MAP_MASK:
	    if ( alloc_node_list( sopt, N, 1 ) )
		return -1;
	    break;

	case S2V_MAP_MASK2:
	    if ( alloc_node_list( sopt, N, 2 ) )
		return -1;
	    break;

	default:
	    fprintf( stderr, "** cnl: unknown map %d\n", sopt->map );
	    return -1;
    }

    return 0;
}


/*----------------------------------------------------------------------
 * alloc_node_list - create node list for mask mapping
 *
 * Allocate space for node list(s), and fill with xyz coordinates.
 *
 * return -1 : on error
 *         0 : on success
 *----------------------------------------------------------------------
*/
int alloc_node_list ( s2v_opts_t * sopt, node_list_t * N, int nsurf )
{
    SUMA_SurfaceObject ** so;
    THD_fvec3          *  fvp;
    float              *  fp;
    int                   rv, nindex, sindex;

    if ( sopt == NULL || N == NULL || nsurf < 0 )
    {
	fprintf( stderr, "** anl: bad params (%p,%p,%d)\n",
		 sopt, N, nsurf );
	return -1;
    }

    /* create a temporary list of surface pointers */
    so = (SUMA_SurfaceObject **)calloc(nsurf, sizeof(SUMA_SurfaceObject *));
    if ( so == NULL )
    {
	fprintf( stderr, "** anl: failed to alloc %d surf pointers\n", nsurf );
	return -1;
    }

    if ( (rv = get_mappable_surfs( so, nsurf, sopt->debug )) != nsurf )
    {
	fprintf( stderr, "** found %d mappable surfaces (but expected %d)\n",
		 rv, nsurf );
	free(so);
	return -1;
    }

    /* fill node list struct */
    N->depth  = nsurf;
    N->nnodes = so[0]->N_Node;
    N->nodes  = (THD_fvec3 *)malloc(N->depth * N->nnodes * sizeof(THD_fvec3));
    if ( N->nodes == NULL )
    {
	fprintf( stderr, "** cnlm: failed to allocate %d THD_fvec3 structs\n",
		 N->depth * N->nnodes );
	free(so);
	return -1;
    }

    /* copy the xyz coordinates for each node */

    fvp = N->nodes;	/* linear coverage of all nodes */
    for ( sindex = 0; sindex < nsurf; sindex++ )
    {
	if ( so[sindex]->N_Node != N->nnodes )
	{
	    fprintf( stderr, "** surface has %d nodes (but expected %d)\n",
		     so[sindex]->N_Node, N->nnodes );
	    free( N->nodes );  N->nodes = NULL;
	    free(so);
	    return -1;					/* bail */
	}

	for ( nindex = 0, fp = so[sindex]->NodeList;
	      nindex < N->nnodes;
	      nindex++, fp += 3 )
	{
	    memcpy( fvp->xyz, fp, 3*sizeof(float) );
	    fvp++;
	}
    }

    if ( sopt->debug > 1 )
	fprintf( stderr, "++ allocated %d x %d (x %d) node list\n",
		 N->depth, N->nnodes, sizeof(THD_fvec3) );

    free(so);
    return 0;
}


/*----------------------------------------------------------------------
 * get_mappable_surfs - return mappable surface objects
 *
 * return the number of surfaces found
 *----------------------------------------------------------------------
*/
int get_mappable_surfs( SUMA_SurfaceObject ** slist, int how_many, int debug )
{
    SUMA_SurfaceObject * so;
    int			 count, socount = 0;

    if ( slist == NULL )
    {
	fprintf( stderr, "** gms: missing slist!\n" );
	return -1;
    }

    for ( count = 0; count < SUMAg_N_DOv; count++ )
    {
	if ( ! SUMA_isSO(SUMAg_DOv[count]) )
	    continue;

	so = (SUMA_SurfaceObject *)SUMAg_DOv[count].OP;

	if ( ! SUMA_isINHmappable( so ) )
	    continue;

	if ( debug > 2 )
	{
	    fprintf( stderr, "\n---------- surface #%d -----------",
		     socount );
	    SUMA_Print_Surface_Object( so, stderr );
	}

	if ( socount < how_many )	/* store a good surface */
	    slist[socount] = so;

	socount++;
    }

    if ( debug > 1 )
	fprintf( stderr, "++ found %d mappable surfaces\n", socount );

    return socount;
}


/*----------------------------------------------------------------------
 * set_map_opts  - given options and mapping function, set flag bits
 *
 * return  0 : success
 *        -1 : error condition
 *----------------------------------------------------------------------
*/
int set_map_opts( opts_t * opts, param_t * p, s2v_opts_t * sopt )
{
    memset( sopt, 0, sizeof(*sopt) );

    if ( (sopt->map = check_map_func( opts->map_str )) == S2V_MAP_INVALID )
	return -1;

    sopt->datum = check_datum_type(opts->datum_str, DSET_BRICK_TYPE(p->gpar,0));
    if (sopt->datum < 0)
	return -1;

    if ( opts->noscale == 1 )
	sopt->noscale = 1;

    sopt->debug  = opts->debug;	/* for output in library functions */
    sopt->cmask = p->cmask;

    switch (sopt->map)
    {
	default:

	case S2V_MAP_AVE:
	case S2V_MAP_COUNT:
	case S2V_MAP_MAX:
	case S2V_MAP_MIN:
	    break;

	case S2V_MAP_MASK:
	    sopt->noscale = 1;
	    break;

	case S2V_MAP_MASK2:
	    sopt->noscale = 1;
	    if ( opts->m2_steps <= 2 )
		sopt->m2_steps = 2;
	    else
		sopt->m2_steps = opts->m2_steps;
	    break;
    }

    if ( opts->debug > 0 )
	disp_s2v_opts_t( "++ s2v_opts_set :", sopt );

    return 0;
}


/*----------------------------------------------------------------------
 * free memory, close output file
 *----------------------------------------------------------------------
*/
int final_clean_up ( opts_t * opts, param_t * p, SUMA_SurfSpecFile * spec,
		     node_list_t * N )
{
    if ( ( SUMAg_DOv != NULL ) &&
	 ( SUMA_Free_Displayable_Object_Vect(SUMAg_DOv, SUMAg_N_DOv) == 0 ) )
	fprintf(stderr, "** failed SUMA_Free_Displayable_Object_Vect()\n" );

    if ( ( SUMAg_SVv != NULL ) &&
	 ( SUMA_Free_SurfaceViewer_Struct_Vect(SUMAg_SVv, SUMAg_N_SVv) == 0 ) )
	fprintf( stderr, "** failed SUMA_Free_SurfaceViewer_Struct_Vect()\n" );

    if ( ( SUMAg_CF != NULL ) && ( SUMA_Free_CommonFields(SUMAg_CF) == 0 ) )
	fprintf( stderr, "** failed SUMA_Free_CommonFields()\n" );

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
    if ( opts->debug > 2 )
    {
	SUMAg_CF->MemTrace = 1;

	if ( opts->debug > 3 )
	    SUMAg_CF->InOut_Notify = 1;
    }

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

    if ( opts->debug > 1 )
	fputs( "++ surfaces loaded.\n", stderr );

    return 0;
}


/*----------------------------------------------------------------------
 * init_options - fill opts struct, display help
 *----------------------------------------------------------------------
*/
int init_options ( opts_t * opts, int argc, char * argv [] )
{
    int ac;

    if ( argc < 2 )
    {
	usage( PROG_NAME, S2V_USE_LONG );
	return -1;
    }

    /* clear out the options and parameter structures */
    memset( opts, 0, sizeof( opts_t) );

    for ( ac = 1; ac < argc; ac++ )
    {
	/* do help first, the rest alphabetically */
	if ( ! strncmp(argv[ac], "-help", 2) )
	{
	    usage( PROG_NAME, S2V_USE_LONG );
	    return -1;
	}
	else if ( ! strncmp(argv[ac], "-cmask", 6) )
	{
	    if ( (ac+1) >= argc )
	    {
		fputs( "option usage: -cmask COMMAND\n\n", stderr );
		usage( PROG_NAME, S2V_USE_SHORT );
		return -1;
	    }

	    opts->cmask_cmd = argv[++ac];
	}
	else if ( ! strncmp(argv[ac], "-datum", 6) )
	{
	    if ( (ac+1) >= argc )
	    {
		fputs( "option usage: -datum DTYPE\n\n", stderr );
		usage( PROG_NAME, S2V_USE_SHORT );
		return -1;
	    }

	    opts->datum_str = argv[++ac];
	}
	else if ( ! strncmp(argv[ac], "-debug", 6) )
	{
	    if ( (ac+1) >= argc )
	    {
		fputs( "option usage: -debug LEVEL\n\n", stderr );
		usage( PROG_NAME, S2V_USE_SHORT );
		return -1;
	    }

	    opts->debug = atoi(argv[++ac]);
	    if ( opts->debug < 0 || opts->debug > S2V_DEBUG_MAX_LEV )
	    {
		fprintf( stderr, "bad debug level <%d>, should be in [0,%d]\n",
			opts->debug, S2V_DEBUG_MAX_LEV );
		usage( PROG_NAME, S2V_USE_SHORT );
		return -1;
	    }
	}
	else if ( ! strncmp(argv[ac], "-grid_parent", 5) )
	{
	    if ( (ac+1) >= argc )
	    {
		fputs( "option usage: -grid_parent INPUT_DSET\n\n", stderr );
		usage( PROG_NAME, S2V_USE_SHORT );
		return -1;
	    }

	    opts->gpar_file = argv[++ac];
	}
	else if ( ! strncmp(argv[ac], "-m2_steps", 6) )
	{
	    if ( (ac+1) >= argc )
	    {
		fputs( "option usage: -m2_steps NUM_STEPS\n\n", stderr );
		usage( PROG_NAME, S2V_USE_SHORT );
		return -1;
	    }

	    opts->m2_steps = atoi(argv[++ac]);
	}
	else if ( ! strncmp(argv[ac], "-map_func", 4) )  /* mapping function */
	{
	    if ( (ac+1) >= argc )
	    {
		fputs( "option usage: -map_func FUNCTION\n\n", stderr );
		return -1;
	    }

	    opts->map_str = argv[++ac];	    /* store user string for now */
	}
	else if ( ! strncmp(argv[ac], "-noscale", 4) )
	{
	    opts->noscale = 1;
	}
	else if ( ! strncmp(argv[ac], "-prefix", 4) )
	{
	    if ( (ac+1) >= argc )
            {
		fputs( "option usage: -prefix OUTPUT_PREFIX\n\n", stderr );
		usage( PROG_NAME, S2V_USE_SHORT );
		return -1;
	    }

            opts->oset_file = argv[++ac];
	}
	else if ( ! strncmp(argv[ac], "-spec", 3) )
	{
	    if ( (ac+1) >= argc )
            {
		fputs( "option usage: -spec SPEC_FILE\n\n", stderr );
		usage( PROG_NAME, S2V_USE_SHORT );
		return -1;
	    }

            opts->spec_file = argv[++ac];
	}
	else if ( ! strncmp(argv[ac], "-sv", 3) )
	{
	    if ( (ac+1) >= argc )
            {
		fputs( "option usage: -sv SURFACE_VOLUME\n\n", stderr );
		usage( PROG_NAME, S2V_USE_SHORT );
		return -1;
	    }

            opts->sv_file = argv[++ac];
	}
	else if ( ! strncmp(argv[ac], "-version", 2) )
	{
	    usage( PROG_NAME, S2V_USE_VERSION );
	    return -1;
	}
	else	 /* invalid option */
	{
	    fprintf( stderr, "invalid option <%s>\n", argv[ac] );
	    usage( PROG_NAME, S2V_USE_SHORT );
	    return -1;
	}
    }

    if ( opts->debug > 0 )
	disp_opts_t ( "++ opts read: ", opts );

    return 0;
}

/*----------------------------------------------------------------------
 * validate_options - fill param struct from options
 *
 *     - validate datasets
 *     - validate surface
 *----------------------------------------------------------------------
*/
int validate_options ( opts_t * opts, param_t * p )
{
    memset( p, 0, sizeof(*p) );

    if ( check_map_func( opts->map_str ) == S2V_MAP_INVALID )
	return -1;

    if ( validate_datasets( opts, p ) != 0 )
	return -1;

    if ( validate_surface( opts, p ) != 0 )
	return -1;

    if ( opts->debug > 1 )
    {
	disp_opts_t ( "++ opts validated: ", opts );
	disp_param_t( "++ opts validated: ", p );
    }

    return 0;
}


/*----------------------------------------------------------------------
 * check_datum_type		- determine type for output dataset
 *
 * currently allowable types are MRI_byte, MRI_short, MRI_int, MRI_float
 *
 * return  datum type : on success
 *         -1         : on failure (no valid type can be determined)
 *----------------------------------------------------------------------
*/
int check_datum_type ( char * datum_str, int default_type )
{
    int c, dt = -1;
    
    if ( datum_str )
    {
	for ( c = 0; c <= MRI_rgba; c++ )
	    if ( ! strcmp( datum_str, MRI_TYPE_name[c] ) )
	    {
		dt = c;
		break;
	    }

	/* if we didn't find the requested type, inform the user and quit */
	if ( c > MRI_rgba )
	{
	    fprintf( stderr, "** invalid datum type name '%s'\n",
		     datum_str );
	    return -1;
	}
    }
    else
    {
	dt = default_type;
    }

    /* rcr - support more types */
    if ( ( dt != MRI_float ) &&
         ( dt != MRI_byte  ) && 
         ( dt != MRI_short )
       )
    {
	fprintf( stderr, "** data type '%s' is not supported\n",
		 MRI_TYPE_name[dt] );
	return -1;
    }

    return dt;
}


/*----------------------------------------------------------------------
 * check_map_func
 *
 *     - check for map_str
 *     - validate the map type
 *----------------------------------------------------------------------
*/
int check_map_func ( char * map_str )
{
    int map;

    if ( map_str == NULL )
    {
	fprintf( stderr, "** missing option: '-map_func FUNCTION'\n" );
	return S2V_MAP_INVALID;
    }

    map = s2v_map_type( map_str );

    if ( map == S2V_MAP_INVALID )
    {
	fprintf( stderr, "** invalid map string '%s'\n", map_str );
	return -1;
    }

    return map;
}


/*----------------------------------------------------------------------
 * s2v_map_type - return an S2V_MAP_XXX code
 *
 * on failure, return -1 (S2V_MAP_INVALID)
 * else        return >0 (a valid map code)
 *----------------------------------------------------------------------
*/
int s2v_map_type ( char * map_str )
{
    s2v_map_num map;

    if ( map_str == NULL )
    {
	fprintf( stderr, "** s2v_map_type: missing map_str parameter\n" );
	return (int)S2V_MAP_INVALID;
    }

    if ( sizeof(gs2v_map_names) / sizeof(char *) != (int)S2V_MAP_FINAL )
    {
	fprintf( stderr, "** error:  gs2v_map_names/s2v_map_num mis-match\n");
	return (int)S2V_MAP_INVALID;
    }

    for ( map = S2V_MAP_MASK; map < S2V_MAP_FINAL; map++ )
	if ( !strcmp( map_str, gs2v_map_names[map] ) )
	    return (int)map;

    return (int)S2V_MAP_INVALID;
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
int validate_datasets( opts_t * opts, param_t * p )
{
    p->gpar = THD_open_dataset( opts->gpar_file );

    if ( !ISVALID_DSET(p->gpar) )
    {
	if ( opts->gpar_file == NULL )
	    fprintf( stderr, "** error: missing '-grid_parent DSET' option\n" );
	else
	    fprintf( stderr, "** error: invalid input dataset '%s'\n",
		     opts->gpar_file);
	return -1;
    }
    else if ( DSET_BRICK_TYPE(p->gpar, 0) == MRI_complex )
    {
	fprintf(stderr,
		"** failure: cannot deal with complex-valued dataset, '%s'\n",
		opts->gpar_file);
	return -1;
    }

    p->nvox = DSET_NVOX( p->gpar );

    if ( ! THD_filename_ok( opts->oset_file ) )
    {
	fprintf( stderr, "** illegal output prefix: '%s'\n",
		 opts->oset_file ? opts->oset_file : "<none>" );
	return -1;
    }

    if ( DSET_NVALS(p->gpar) > 1 )	/* rcr - maybe update this */
    {
	fprintf( stderr, "** gpar has %d sub-bricks (only 1 is allowed)\n",
	         DSET_NVALS(p->gpar) );
	return -1;
    }

    /* -------------------------------------------------------------------- */
    /* check for cmask - casually stolen from 3dmaskdump.c (thanks, Bob! :) */

    if ( opts->cmask_cmd != NULL )
    {
	int    clen = strlen( opts->cmask_cmd );
	char * cmd;

	/* save original cmask command, as EDT_calcmask() is destructive */
	cmd = (char *)malloc((clen + 1) * sizeof(char));
	strcpy( cmd, opts->cmask_cmd );

	p->cmask = EDT_calcmask( cmd, &p->ncmask );

	free( cmd );			   /* free EDT_calcmask() string */

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
	    fprintf( stderr, "** Warning!  No voxels in computed cmask!\n" );
	    /* return -1;   continue, and let the user deal with it...  */
	}
    }

    if ( opts->debug > 0 )
    {
	fprintf( stderr, "++ input dset has nvox = %d, nvals = %d",
		 p->nvox, DSET_NVALS(p->gpar) );
	if ( p->cmask == NULL )
	    fputc( '\n', stderr );
	else
	    fprintf( stderr, " (%d voxels in mask)\n", p->ccount );
    }

    return 0;
}

/*----------------------------------------------------------------------
 * usage  -  output usage information
 *
 * S2V_USE_SHORT	- display brief output
 * S2V_USE_LONG		- display long output
 * S2V_USE_VERSION	- show the VERSION of the program
 *----------------------------------------------------------------------
*/
int usage ( char * prog, int level )
{
    if ( level == S2V_USE_SHORT )
    {
	fprintf( stderr,
		 "usage: %s [options] -spec SPEC_FILE -sv SURF_VOL "
		                    " -grid_parent AFNI_DSET\n"
		 "usage: %s -help\n",
		 prog, prog );
	return 0;
    }
    else if ( level == S2V_USE_LONG )
    {
	printf(
	    "\n"
	    "%s - create an AFNI volume dataset from a surface\n"
	    "\n"
	    "    This program is meant to take as input a surface and an AFNI\n"
	    "    dataset, and to output a new AFNI dataset consisting of the\n"
	    "    surface mapped to the dataset grid space, according to some\n"
	    "    user-specified function.\n"
	    "\n"
	    "    The surface points are mapped to xyz coordinates, according\n"
	    "    to the SURF_VOL (surface volume) AFNI dataset.  These xyz\n"
	    "    coordinates are then matched to voxels in the grid parent\n"
	    "    dataset.  The output dataset will have the same grid as the\n"
	    "    grid parent dataset, with values coming from the surface,\n"
	    "    and subject to any mask (see -cmask).\n"
	    "\n"
	    "  usage: %s [options] -spec SPEC_FILE -sv SURF_VOL \\\n"
	    "                    -grid_parent AFNI_DSET -map_func MAP_FUNC\n"
	    "\n"
	    "  examples:\n"
	    "\n"
	    "    %s                       \\\n"
	    "       -spec         fred.spec                \\\n"
	    "       -sv           fred_anat+orig           \\\n"
	    "       -grid_parent  fred_anat+orig           \\\n"
	    "       -map_func     mask                     \\\n"
	    "       -prefix       fred_surf\n"
	    "\n"
	    "    %s                       \\\n"
	    "       -spec         fred.spec                               \\\n"
	    "       -sv           fred_anat+orig                          \\\n"
	    "       -grid_parent 'fred_epi+orig[0]'                       \\\n"
	    "       -map_func     mask                                    \\\n"
	    "       -cmask       '-a fred_func+orig[2] -expr step(a-0.6)' \\\n"
	    "       -debug        2                                       \\\n"
	    "       -prefix       fred_surf_mask\n"
	    "\n"
	    "    %s                       \\\n"
	    "       -spec         fred.spec                               \\\n"
	    "       -sv           fred_anat+orig                          \\\n"
	    "       -grid_parent  fred_anat+orig                          \\\n"
	    "       -map_func     mask2                                   \\\n"
	    "       -m2_steps     20                                      \\\n"
	    "       -cmask       '-a fred_func+orig[2] -expr step(a-0.6)'\\\n"
	    "       -debug        2                                       \\\n"
	    "       -prefix       fred_surf_mask2\n"
	    "\n"
	    "\n"
	    "  REQUIRED COMMAND ARGUMENTS:\n"
	    "\n"
	    "    -spec SPEC_FILE        : SUMA spec file\n"
	    "\n"
	    "        e.g. -spec fred.spec\n"
	    "\n"
	    "        The surface specification file contains the list of\n"
	    "        mappable surfaces that are used.\n"
	    "\n"
	    "        See @SUMA_Make_Spec_FS and @SUMA_Make_Spec_SF.\n"
	    "\n"
	    "    -sv SURFACE_VOLUME     : AFNI dataset\n"
	    "\n"
	    "        e.g. -sv fred_anat+orig\n"
	    "\n"
	    "        This is the AFNI dataset that the surface is mapped to.\n"
	    "        This dataset is used for the intial surface node to xyz\n"
	    "        coordinate mapping, in the Dicomm orientation.\n"
	    "\n"
	    "    -grid_parent AFNI_DSET : AFNI dataset\n"
	    "\n"
	    "        e.g. -grid_parent fred_function+orig\n"
	    "\n"
	    "        This dataset is used as a grid and orientation master\n"
	    "        for the output dataset.\n"
	    "\n"
	    "    -map_func MAP_FUNC     : surface to dataset function\n"
	    "\n"
	    "        e.g. -map_func mask\n"
	    "        e.g. -map_func mask2 -m2_steps 20\n"
	    "\n"
	    "        Given one or more surfaces, there are many ways to\n"
	    "        select voxel locations, and to select corresponding\n"
	    "        values for the output dataset.  Some of the functions\n"
	    "        have separate options.\n"
	    "\n"
	    "        The current mapping functions are:\n"
	    "\n"
	    "          mask   : For each xyz location, set the corresponding\n"
	    "                   voxel to 1.\n"
	    "\n"
	    "          mask2  : Given 2 surfaces with the same geometry\n"
	    "                   (i.e. which are just different views of the\n"
	    "                   same surface), for each corresponding pair\n"
	    "                   of nodes, set the voxel that would contain\n"
	    "                   each node.\n"
	    "\n"
	    "                  -m2_steps NUM_STEPS\n"
	    "\n"
	    "                   e.g. -m2_steps 20\n"
	    "                   the default (and minimum) is 2\n"
	    "\n"
	    "                   This separate option may be used to specify\n"
	    "                   the number of nodes that should be included\n"
	    "                   along the segment containing each node pair\n"
	    "                   from the surfaces.\n"
	    "                   \n"
	    "                   So for each pair of nodes, include all points\n"
	    "                   at evenly spaced interval between them.  The\n"
	    "                   effect is like using NUM_STEPS surface layers\n"
	    "                   for the mapping, not just the two which were\n"
	    "                   input (in the spec file).\n"
	    "\n"
	    "    -prefix OUTPUT_PREFIX  : prefix for the output dataset\n"
	    "\n"
	    "        e.g. -prefix anat_surf_mask\n"
	    "\n"
	    "        This is used to specify the prefix of the resulting AFNI\n"
	    "        dataset.\n"
	    "\n"
	    "  options:\n"
	    "\n"
	    "    -cmask MASK_COMMAND    : (optional) command for dataset mask\n"
	    "\n"
	    "        e.g. -cmask '-a fred_func+orig[2] -expr step(a-0.8)'\n"
	    "\n"
	    "        This option will produce a mask to be applied to the\n"
	    "        output dataset.  Note that this mask should form a\n"
	    "        single sub-brick.\n"
	    "\n"
	    "        This option follows the style of 3dmaskdump (since the\n"
	    "        code for it was, uh, borrowed from there (thanks Bob!)).\n"
	    "\n"
	    "        See '3dmaskdump -help' for more information.\n"
	    "\n"
	    "    -debug LEVEL           :  (optional) verbose output\n"
	    "\n"
	    "        e.g. -debug 2\n"
	    "\n"
	    "        This option is used to print out status information \n"
	    "        during the execution of the program.  Current levels are\n"
	    "        from 0 to 4.\n"
	    "\n"
	    "    -help                  : show this help\n"
	    "\n"
	    "        If you can't get help here, please get help somewhere.\n"
	    "\n"
	    "    -noscale               : no scale factor in output dataset\n"
	    "\n"
	    "        If the output dataset is an integer type (byte, shorts\n"
	    "        or ints), then the output dataset may end up with a\n"
	    "        scale factor attached (see 3dcalc -help).  With this\n"
	    "        option, the output dataset will not be scaled.\n"
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
    else if ( level == S2V_USE_VERSION )
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
	    "    gpar_file          = %s\n"
	    "    oset_file          = %s\n"
	    "    spec_file          = %s\n"
	    "    sv_file            = %s\n"
	    "    cmask_cmd          = %s\n"
	    "    map_str            = %s\n"
	    "    datum_str          = %s\n"
	    "    debug, no_headers  = %d, %d\n"
	    "    m2_steps           = %d\n"
	    , opts,
	    opts->gpar_file, opts->oset_file, opts->spec_file, opts->sv_file,
	    opts->cmask_cmd, opts->map_str, opts->datum_str, opts->debug,
	    opts->no_headers, opts->m2_steps
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
	    "    gpar  : vcheck  = %p : %s\n"
	    "    oset  : vcheck  = %p : %s\n"
	    "    nvox, cmask     = %d, %p\n"
	    "    ncmask, ccount  = %d, %d\n"
	    , p,
	    p->gpar, ISVALID_DSET(p->gpar) ? "valid" : "invalid",
	    p->oset, ISVALID_DSET(p->oset) ? "valid" : "invalid",
	    p->nvox, p->cmask, p->ncmask, p->ccount
	    );

    return 0;
}


/*----------------------------------------------------------------------
 * disp_s2v_opts_t  -  display the contents of the s2v_opts_t struct
 *----------------------------------------------------------------------
*/
int disp_s2v_opts_t ( char * info, s2v_opts_t * sopt )
{
    if ( info )
	fputs( info, stdout );

    if ( sopt == NULL )
    {
	printf( "disp_s2v_opts_t: sopt == NULL\n" );
	return -1;
    }

    printf( "s2v_opts_t struct at %p :\n"
	    "    map, datum     = %d, %d\n"
	    "    noscale, debug = %d, %d\n"
	    "    cmask          = %p\n"
	    "    m2_steps       = %d\n"
	    , sopt,
	    sopt->map, sopt->datum, sopt->noscale, sopt->debug,
	    sopt->cmask, sopt->m2_steps
	    );

    return 0;
}
