
#define VERSION "version 2.1 (June 17, 2003)"

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
 *    3dSurfMaskDump [options] -spec SPEC_FILE -sv SURF_VOL               \
 *                              -grid_par AFNI_DSET -map_func MAPPING_FUNC
 *
 * options:
 *
 * 	-help
 * 	-version
 *
 * 	-grid_par   AFNI_DSET
 * 	-spec       SPEC_FILE
 * 	-sv         SURF_VOL
 *
 * 	-cmask      MASK_COMMAND
 * 	-debug      LEVEL
 * 	-outfile    OUTPUT_FILE
 * 	-no_headers
 *
 * examples:
 *
 *    3dSurfMaskDump -spec     SubjA.spec     -sv       SubjA_anat+orig   \ 
 *                   -grid_par SubjA_EPI+orig -map_func mask  >  output.txt
 *
 *    3dSurfMaskDump -spec     SubjectA.spec                              \
 *                   -sv       SubjectA_spgr+orig                         \ 
 *                   -grid_par SubjA_EPI+orig                             \
 *                   -cmask    '-a SubjA.func+orig[2] -expr step(a-0.6)'  \
 *                   -map_func midpoint					  \
 *                   -outfile  SubjA_surf_out.txt
 *
 *----------------------------------------------------------------------
*/

/*----------------------------------------------------------------------
 * history:
 *
 * 2.1  June 10, 2003
 *   - added ave map function (see dump_ave_map)
 *
 * 2.0  June 06, 2003
 *   - re-wrote program according to 3dSurf2Vol (which was written
 *     according to this :) - using map functions and node lists
 *   - added midpoint map function
 *
 * 1.3  February 14, 2003
 *   - optionally enable more SUMA debugging
 *
 * 1.2  February 13, 2003
 *   - init SUMAg array pointers, check before calling Free_()
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

/* globals */
SUMA_SurfaceViewer * SUMAg_SVv = NULL;	/* array of Surf View structs   */
int                  SUMAg_N_SVv = 0;	/* length of SVv array          */
SUMA_DO            * SUMAg_DOv = NULL;	/* array of Displayable Objects */
int                  SUMAg_N_DOv = 0;	/* length of DOv array          */
SUMA_CommonFields  * SUMAg_CF = NULL;	/* info common to all viewers   */

/* this must match smap_nums enum */
char * g_smap_names[] = { "none", "mask", "midpoint", "mask2", "ave",
                          "count", "min", "max" };


/* AFNI prototype */
extern void machdep( void );

#define MAIN

/*----------------------------------------------------------------------*/

int main( int argc , char * argv[] )
{
    SUMA_SurfSpecFile  spec;
    node_list_t        node_list = {NULL, 0, 0};
    param_t            params;
    smap_opts_t        sopt;
    opts_t             opts;
    int                ret_val;

    mainENTRY("3dSurfMaskDump main");
    machdep();
    AFNI_logger("3dSurfMaskDump",argc,argv);

    /* validate inputs and init options structure */
    if ( ( ret_val = init_options(&opts, argc, argv) ) != 0 )
	return ret_val;

    if ( ( ret_val = validate_options(&opts, &params) ) != 0 )
	return ret_val;

    if ( (ret_val = set_smap_opts( &opts, &params, &sopt )) != 0 )
	return ret_val;

    /* read surface files */
    if ( (ret_val = read_surf_files(&opts, &params, &spec)) != 0 )
	return ret_val;

    /*  get node list from surfaces (multiple points per node)
     *  need merge function
     */
    if ( (ret_val = create_node_list( &sopt, &node_list )) != 0 )
	return ret_val;

    if ( (ret_val = write_output( &sopt, &opts, &params, &node_list )) != 0 )
	return ret_val;

    /* free memory */
    final_clean_up(&opts, &params, &spec, &node_list);

    return ret_val;
}


/*----------------------------------------------------------------------
 * write_output
 *----------------------------------------------------------------------
*/
int write_output ( smap_opts_t * sopt, opts_t * opts, param_t * p,
	           node_list_t * N )
{
    if ( sopt == NULL || opts == NULL || p == NULL || N == NULL )
    {
	fprintf( stderr, "** smd_wo - bad params (%p,%p,%p,%p)\n",
		 sopt, opts, p, N );
	return -1;
    }

    switch (sopt->map)
    {
	case E_SMAP_AVE:
	{
	    dump_ave_map( sopt, p, N );
	    break;
	}

	case E_SMAP_COUNT:
	case E_SMAP_MAX:
	case E_SMAP_MIN:
	case E_SMAP_MASK2:
	{
	    fprintf( stderr, "** function '%s' coming soon ...\n",
		     g_smap_names[sopt->map] );
	    break;
	}

	case E_SMAP_MIDPT:
	{
	    dump_midpt_map( sopt, p, N );
	    break;
	}

	case E_SMAP_MASK:
	{
	    dump_single_map( sopt, p, N );
	    break;
	}

	default:
	    fprintf( stderr, "** smd_n2v: unknown map %d\n", sopt->map );
	    break;
    }

    return 0;
}


/*----------------------------------------------------------------------
 * dump_ave_map - for each node pair, dump the average voxel value
 *                along the connecting line segment
 *----------------------------------------------------------------------
*/
int dump_ave_map ( smap_opts_t * sopt, param_t * p, node_list_t * N )
{
    THD_fvec3   f3mm0, f3mmn, f3mm;
    THD_ivec3   i3ind;
    MRI_IMAGE * im;
    double    * bdata;		/* brick data, and for sums */
    double    * bptr, * bave;   /* temp pointers into bdata */
    float     * fser;
    float       rat0, ratn;
    int         node, sub, subs;
    int         vindex, sindex, prev_ind;
    int		scount, bcount;		    /* step and sub-brick counters */
    int         dcount;
    int		steps;
    int         nx, ny, nz;

    if ( sopt == NULL || p == NULL || N == NULL )
    {
	fprintf( stderr, "** smd_dsm : bad params (%p,%p,%p)\n", sopt, p, N );
	return -1;
    }

    nx   = DSET_NX(p->gpar);
    ny   = DSET_NY(p->gpar);
    nz   = DSET_NZ(p->gpar);
    subs = DSET_NVALS(p->gpar);

    /* one last precaution */
    if ( N->depth < 2 || N->nnodes <= 0 || sopt->m2_steps < 2 )
    {
	fprintf( stderr, "** bad setup for ave mapping (%d,%d,%d)\n",
		 N->depth, N->nnodes, sopt->m2_steps );
	return -1;
    }

    if ( sopt->debug > 1 )
	fprintf( stderr, "++ output (depth, nnodes, subs, nvox) = "
		                   "(%d, %d, %d, %d)\n",
		 	 N->depth, N->nnodes, subs, nx*ny*nz );

    /* store info for each step (in case of debug), plus one for sum */
    bdata = (double *)malloc(subs * (sopt->m2_steps+1) * sizeof(double));
    if ( bdata == NULL )
    {
	fprintf(stderr, "** can't allocate %d doubles\n", subs*sopt->m2_steps);
	return -1;
    }

    if ( ! sopt->no_head )
    {
	fprintf( p->outfp,
                 "# --------------------------------------------------\n" );
	fprintf( p->outfp, "# surface '%s', '%s' :\n",
		 N->labels[0], g_smap_names[sopt->map] );
	fprintf( p->outfp, "#\n" );

	/* output column headers */
	fprintf( p->outfp, "#    node     1dindex    i    j    k     "
		           "nvox" );
	for ( sub = 0; sub < subs; sub++ )
	    fprintf( p->outfp, "       v%-2d  ", sub );
	fputc( '\n', p->outfp );

	/* underline the column headers */
	fprintf( p->outfp, "#   ------    -------   ---  ---  ---    ----   " );
	for ( sub = 0; sub < subs; sub++ )
	    fprintf( p->outfp, " --------   " );
	fputc( '\n', p->outfp );
    }

    steps  = sopt->m2_steps;
    dcount = 2;

    /* note, NodeList elements are in dicomm mm orientation */

    for ( node = 0; node < N->nnodes; node++ )
    {
	/* note the endpoints */
	f3mm0 = THD_dicomm_to_3dmm(p->gpar, N->nodes[node]);
	f3mmn = THD_dicomm_to_3dmm(p->gpar, N->nodes[node+N->nnodes]);

	/* init values for average */
	scount   = 0;		/* for count of used voxels            */
	prev_ind = -1;		/* want only new indices along segment */
	vindex   = -1;		/* basically useless                   */

	/* compute the dataset average along this node pair segment */
	for ( sindex = 0; sindex < steps; sindex++ )
	{
	    /* set ratios for current point */
	    ratn = (float)sindex / (steps - 1);
	    rat0 = 1.0 - ratn;

	    f3mm.xyz[0] = rat0 * f3mm0.xyz[0] + ratn * f3mmn.xyz[0];
	    f3mm.xyz[1] = rat0 * f3mm0.xyz[1] + ratn * f3mmn.xyz[1];
	    f3mm.xyz[2] = rat0 * f3mm0.xyz[2] + ratn * f3mmn.xyz[2];

	    i3ind  = THD_3dmm_to_3dind ( p->gpar, f3mm );
	    vindex = i3ind.ijk[0] + nx * (i3ind.ijk[1] + ny * i3ind.ijk[2] );

	    /* if we don't want this index, skip it */
	    if ( p->cmask && !p->cmask[vindex] )
		continue;

	    /* okay, so is this a different voxel than last time? */
	    if ( vindex == prev_ind )
		continue;

	    /* woohoo! a new voxel to add to our bdata matrix */
	    
	    im   = THD_extract_series( vindex, p->gpar, 0 );
	    fser = MRI_FLOAT_PTR( im ); /* get series, as float array    */

	    /* store the information */
	    bptr = bdata + scount * subs;	/* don't repeat calc.    */
	    for ( bcount = 0; bcount < subs; bcount++ )
		bptr[bcount] = fser[bcount];

	    prev_ind = vindex;		/* so we don't repeat this one   */
	    scount++;			/* count the voxel               */

	    free(im);			/* free the image for this voxel */
	}

	if ( scount == 0 )		/* any good voxels in the bunch? */
	    continue;

	/* compute sums of and using bdata */
	bave = bdata + sopt->m2_steps * subs;	/* point to our extra array */
	for (bcount = 0; bcount < subs; bcount++ )	/* init to zero     */
	    bave[bcount] = 0.0;
	for ( sindex = 0; sindex < scount; sindex++ )
	{
	    bptr = bdata + sindex * subs;
	    for (bcount = 0; bcount < subs; bcount++ )	/* compute the sums */
		bave[bcount] += bptr[bcount];
	}
	for (bcount = 0; bcount < subs; bcount++ )	/* and take average */
	    bave[bcount] /= scount;

	/* debug display if first node with step index greater than 1 */
	if ( sopt->debug > 1 && scount > dcount )
	{
	    dcount++;
	    fprintf( stderr, "++ (node, vindex, scount) = "
		     "(%d, %d, %d)\n",
		     node, vindex, scount );
	    fprintf( stderr, "++ data :\n" );

	    /* data is in row major order, so just walk through it */
	    bptr = bdata;
	    for ( sindex = 0; sindex < scount; sindex++ )
	    {
		fprintf( stderr, "   step %3d : ", sindex );
		for (bcount = 0; bcount < subs; bcount++ )
		    fprintf( stderr, "%10s ", MV_format_fval(*bptr) );
		bptr++;
		fputc( '\n', stderr );
	    }
	    fprintf( stderr, "++ ave      : " );
	    for (bcount = 0; bcount < subs; bcount++ )
		fprintf( stderr, "%10s ", MV_format_fval(bave[bcount]) );
	    fputc( '\n', stderr );
	}

	/* output surface, volume, ijk indices and nvoxels*/
	fprintf( p->outfp, "  %8d   %8d   %3d  %3d  %3d     %3d",
	     node, vindex, i3ind.ijk[0], i3ind.ijk[1], i3ind.ijk[2], scount );

	/* hey, these numbers are why I'm writing the program, woohoo! */
	for ( sub = 0; sub < subs; sub++ )
	    fprintf( p->outfp, "  %10s", MV_format_fval(bave[sub]) );
	fputc( '\n', p->outfp );

    }

    free(bdata);

    return 0;
}


/*----------------------------------------------------------------------
 * dump_midpt_map - for each node pair, dump dataset values derived
 *                  from their midpoint
 *----------------------------------------------------------------------
*/
int dump_midpt_map ( smap_opts_t * sopt, param_t * p, node_list_t * N )
{
    THD_fvec3   f3mma, f3da;
    THD_fvec3 * f3p0, * f3p1;
    THD_ivec3   i3ind;
    MRI_IMAGE * im;
    float     * fser, * fp;
    int         node, sub, subs;
    int         vindex;
    int         nx, ny, nz;

    if ( sopt == NULL || p == NULL || N == NULL )
    {
	fprintf( stderr, "** smd_dsm : bad params (%p,%p,%p)\n", sopt, p, N );
	return -1;
    }

    nx   = DSET_NX(p->gpar);
    ny   = DSET_NY(p->gpar);
    nz   = DSET_NZ(p->gpar);
    subs = DSET_NVALS(p->gpar);

    /* one last precaution */
    if ( N->depth < 2 || N->nnodes <= 0 )
    {
	fprintf( stderr, "** bad setup for midpoint mapping!\n" );
	return -1;
    }

    if ( sopt->debug > 1 )
	fprintf( stderr, "++ output (depth, nnodes, subs, nvox) = "
		                   "(%d, %d, %d, %d)\n",
		 	 N->depth, N->nnodes, subs, nx*ny*nz );

    if ( ! sopt->no_head )
    {
	fprintf( p->outfp,
                 "# --------------------------------------------------\n" );
	fprintf( p->outfp, "# surface '%s', '%s' :\n",
		 N->labels[0], g_smap_names[sopt->map] );
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

    f3p0 = N->nodes;			/* set the 2 node pointers */
    f3p1 = N->nodes + N->nnodes;
    for ( node = 0; node < N->nnodes; node++, f3p0++, f3p1++ )
    {
	/* use the geometric midpoint */
	f3da.xyz[0] = (f3p0->xyz[0] + f3p1->xyz[0]) / 2.0;
	f3da.xyz[1] = (f3p0->xyz[1] + f3p1->xyz[1]) / 2.0;
	f3da.xyz[2] = (f3p0->xyz[2] + f3p1->xyz[2]) / 2.0;

	f3mma = THD_dicomm_to_3dmm( p->gpar, f3da );

	if ( (sopt->debug > 2) && (node % 15000 == 0) )
	{
	    fprintf( stderr, "++ node %6d: (%f,%f,%f), (%f,%f,%f)\n"
			     "             (%f,%f,%f), (%f,%f,%f)\n",
		     node,
		     f3p0->xyz[0], f3p0->xyz[1], f3p0->xyz[2], 
		     f3p1->xyz[0], f3p1->xyz[1], f3p1->xyz[2], 
		     f3da.xyz[0],  f3da.xyz[1],  f3da.xyz[2], 
		     f3mma.xyz[0], f3mma.xyz[1], f3mma.xyz[2] );
	}

	i3ind  = THD_3dmm_to_3dind ( p->gpar, f3mma );
	vindex = i3ind.ijk[0] + nx * (i3ind.ijk[1] + ny * i3ind.ijk[2] );

	/* if we don't want this index, skip it */
	if ( p->cmask && !p->cmask[vindex] )
	    continue;

	/* rcr - may want to consider threshold on separate brick */

	/* output surface, volume, and ijk indices */
	fprintf( p->outfp, "  %8d   %8d   %3d  %3d  %3d ",
		 node, vindex, i3ind.ijk[0], i3ind.ijk[1], i3ind.ijk[2] );

	/* get series, as float array */
	im   = THD_extract_series( vindex, p->gpar, 0 );
	fser = MRI_FLOAT_PTR( im );

	/* hey, these numbers are why I'm writing the program, woohoo! */
	for ( sub = 0; sub < im->nx; sub++ )
	    fprintf( p->outfp, "  %10s", MV_format_fval(fser[sub]) );
	fputc( '\n', p->outfp );

	free(im);
    }

    return 0;
}


/*----------------------------------------------------------------------
 * dump_single_map - for each node, dump dataset value for each sub-brick
 *----------------------------------------------------------------------
*/
int dump_single_map ( smap_opts_t * sopt, param_t * p, node_list_t * N )
{
    THD_fvec3   f3mm;
    THD_ivec3   i3ind;
    MRI_IMAGE * im;
    float     * fser, * fp;
    int         node, sub, subs;
    int         vindex;
    int         nx, ny, nz;

    if ( sopt == NULL || p == NULL || N == NULL )
    {
	fprintf( stderr, "** smd_dsm : bad params (%p,%p,%p)\n", sopt, p, N );
	return -1;
    }

    nx   = DSET_NX(p->gpar);
    ny   = DSET_NY(p->gpar);
    nz   = DSET_NZ(p->gpar);
    subs = DSET_NVALS(p->gpar);

    if ( N->nnodes <= 0 )
	return -1;

    if ( ! sopt->no_head )
    {
	fprintf( p->outfp,
                 "# --------------------------------------------------\n" );
	fprintf( p->outfp, "# surface '%s', '%s' :\n",
		 N->labels[0], g_smap_names[sopt->map] );
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

    for ( node = 0; node < N->nnodes; node++ )
    {
	f3mm   = THD_dicomm_to_3dmm( p->gpar, N->nodes[node] );
	i3ind  = THD_3dmm_to_3dind ( p->gpar, f3mm );
	vindex = i3ind.ijk[0] + nx * (i3ind.ijk[1] + ny * i3ind.ijk[2] );

	/* if we don't want this index, skip it */
	if ( p->cmask && !p->cmask[vindex] )
	    continue;

	/* rcr - may want to consider threshold on separate brick */

	/* output surface, volume, and ijk indices */
	fprintf( p->outfp, "  %8d   %8d   %3d  %3d  %3d ",
		 node, vindex, i3ind.ijk[0], i3ind.ijk[1], i3ind.ijk[2] );

	/* get series, as float array */
	im   = THD_extract_series( vindex, p->gpar, 0 );
	fser = MRI_FLOAT_PTR( im );

	/* hey, these numbers are why I'm writing the program, woohoo! */
	for ( sub = 0; sub < im->nx; sub++ )
	    fprintf( p->outfp, "  %10s", MV_format_fval(fser[sub]) );
	fputc( '\n', p->outfp );

	free(im);
    }

    return 0;
}


/*----------------------------------------------------------------------
 * create_node_list	- from surfaces
 *
 *----------------------------------------------------------------------
*/
int create_node_list ( smap_opts_t * sopt, node_list_t * N )
{
    if ( sopt == NULL || N == NULL )
    {
	fprintf( stderr, "** cnl - bad params (%p,%p)\n", sopt, N );
	return -1;
    }

    switch (sopt->map)
    {
	case E_SMAP_COUNT:
	case E_SMAP_MAX:
	case E_SMAP_MIN:
	case E_SMAP_MASK2:
	    fprintf( stderr, "** function '%s' coming soon ...\n",
		     g_smap_names[sopt->map] );
	    return -1;
	    break;

	case E_SMAP_MASK:
	    if ( alloc_node_list( sopt, N, 1 ) )
		return -1;
	    break;

	case E_SMAP_AVE:
	case E_SMAP_MIDPT:
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
int alloc_node_list ( smap_opts_t * sopt, node_list_t * N, int nsurf )
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

    N->labels = (char **)malloc(N->depth * sizeof(char *));
    if ( N->labels == NULL )
    {
	fprintf(stderr,"** cnlm: failed to allocate for %d labels\n",N->depth);
	free(so);
	free(N->nodes);
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

	N->labels[sindex] = so[sindex]->Label;

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
 * set_smap_opts  - given options and mapping function, set flag bits
 *
 * return  0 : success
 *        -1 : error condition
 *----------------------------------------------------------------------
*/
int set_smap_opts( opts_t * opts, param_t * p, smap_opts_t * sopt )
{
    memset( sopt, 0, sizeof(*sopt) );

    if ( (sopt->map = check_map_func( opts->map_str )) == E_SMAP_INVALID )
	return -1;

    sopt->debug   = opts->debug;	/* for output in library functions */
    sopt->no_head = opts->no_head;
    sopt->cmask   = p->cmask;

    switch (sopt->map)
    {
	default:

	case E_SMAP_COUNT:
	case E_SMAP_MAX:
	case E_SMAP_MIN:
	case E_SMAP_MASK:
	case E_SMAP_MIDPT:
	    break;

	case E_SMAP_AVE:
	    if ( opts->m2_steps <= 2 )
		sopt->m2_steps = 2;
	    else
		sopt->m2_steps = opts->m2_steps;
	    break;

	case E_SMAP_MASK2:
	    sopt->m2_steps = opts->m2_steps;
	    break;
    }

    if ( opts->debug > 0 )
	disp_smap_opts_t( "++ smap_opts_set :", sopt );

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
	else if ( ! strncmp(argv[ac], "-grid_parent", 5) ||
		  ! strncmp(argv[ac], "-inset", 6)       ||
		  ! strncmp(argv[ac], "-input", 6) )
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
	else if ( ! strncmp(argv[ac], "-no_headers", 5) )
	{
	    opts->no_head = 1;
	}
	else if ( ! strncmp(argv[ac], "-outfile", 4) )
	{
	    if ( (ac+1) >= argc )
            {
		fputs( "option usage: -outfile OUTPUT_FILE\n\n", stderr );
		usage( PROG_NAME, S2V_USE_SHORT );
		return -1;
	    }

            opts->out_file = argv[++ac];
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

    if ( opts->debug > 0 )
	disp_opts_t ( "++ opts read: ", opts );

    if ( check_map_func( opts->map_str ) == E_SMAP_INVALID )
	return -1;

    if ( set_outfile( opts, p ) != 0 )
	return -1;

    if ( opts->spec_file == NULL )
    {
	fprintf( stderr, "** missing '-spec_file SPEC_FILE' option\n" );
	return -1;
    }

    if ( opts->sv_file == NULL )
    {
	fprintf( stderr, "** missing '-sv SURF_VOL' option\n" );
	return -1;
    }

    if ( validate_datasets( opts, p ) != 0 )
	return -1;

    if ( opts->debug > 1 )
	disp_param_t( "++ opts validated: ", p );

    return 0;
}


/*----------------------------------------------------------------------
 * decide where the output goes
 *----------------------------------------------------------------------
*/
int set_outfile( opts_t * opts, param_t * p )
{
    if ( opts == NULL || p == NULL )
	return -1;

    if ( opts->out_file == NULL )
	p->outfp = stdout;
    else if ( strcmp( opts->out_file, "stdout" ) == 0 )
	p->outfp = stdout;
    else if ( strcmp( opts->out_file, "stderr" ) == 0 )
	p->outfp = stderr;
    else
    {
	if ( THD_is_file(opts->out_file) )
	{
	    fprintf( stderr, "** output file '%s' already exists\n",
		     opts->out_file );
	    return -1;
	}

	p->outfp = fopen( opts->out_file, "w" );
	if ( p->outfp == NULL )
	{
	    fprintf( stderr, "** failure to open '%s' for writing\n",
		     opts->out_file );
	    return -1;
	}
    }

    return 0;
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
	return E_SMAP_INVALID;
    }

    map = smd_map_type( map_str );

    switch ( map )
    {
	default:
	    map = E_SMAP_INVALID;
	    break;

	case E_SMAP_COUNT:
	case E_SMAP_MAX:
	case E_SMAP_MIN:
	case E_SMAP_MASK2:
	case E_SMAP_MIDPT:
	    fprintf( stderr, "** function '%s' coming soon ...\n",
		     g_smap_names[map] );
	    return E_SMAP_INVALID;
	    break;

	case E_SMAP_MASK:
	case E_SMAP_AVE:
	    break;
    }

    if ( map == E_SMAP_INVALID )
    {
	fprintf( stderr, "** invalid map string '%s'\n", map_str );
	return -1;
    }

    return map;
}


/*----------------------------------------------------------------------
 * smd_map_type - return an E_SMAP_XXX code
 *
 * on failure, return -1 (E_SMAP_INVALID)
 * else        return >0 (a valid map code)
 *----------------------------------------------------------------------
*/
int smd_map_type ( char * map_str )
{
    smap_nums map;

    if ( map_str == NULL )
    {
	fprintf( stderr, "** smd_map_type: missing map_str parameter\n" );
	return (int)E_SMAP_INVALID;
    }

    if ( sizeof(g_smap_names) / sizeof(char *) != (int)E_SMAP_FINAL )
    {
	fprintf( stderr, "** error:  g_smap_names/smd_map_num mis-match\n");
	return (int)E_SMAP_INVALID;
    }

    for ( map = E_SMAP_INVALID; map < E_SMAP_FINAL; map++ )
	if ( !strcmp( map_str, g_smap_names[map] ) )
	    return (int)map;

    return (int)E_SMAP_INVALID;
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
	    "%s - dump ascii dataset values corresponding to a surface\n"
	    "\n"
	    "This program is used to display AFNI dataset values that\n"
	    "correspond to a surface.  The surface points are mapped to xyz\n"
	    "coordinates, according to the SURF_VOL (surface volume) AFNI\n"
	    "dataset.  These coordinates are then matched to voxels in other\n"
	    "AFNI datasets.  So given any other AFNI dataset, this program\n"
	    "can output all of the sub-brick values that correspond to each\n"
	    "of the suface locations.  The user also has options to mask\n"
	    "regions for output.\n"
	    "\n"
	    "Different mappings are allowed from the surface(s) to the grid\n"
	    "parent dataset.  The mapping function is a required parameter to\n"
	    "the program.\n"
	    "\n"
	    "The current mapping functions are:\n"
	    "\n"
	    "    ave       : for each node pair (from 2 surfaces), output the\n"
	    "                average of all voxel values along that line\n"
	    "                segment\n"
	    "    mask      : each node in the surface is mapped to one voxel\n"
	    "    midpoint  : for each node pair (from 2 surfaces), output the\n"
	    "                dataset value at their midpoint (in xyz space)\n"
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
	    "\n"
	    "    %s                       \\\n"
	    "       -spec         fred.spec                               \\\n"
	    "       -sv           fred_anat+orig                          \\\n"
	    "       -grid_parent 'fred_epi+orig[0]'                       \\\n"
	    "       -map_func     mask                                    \\\n"
	    "       -cmask       '-a fred_func+orig[2] -expr step(a-0.6)' \\\n"
	    "       -debug        2                                       \\\n"
	    "       -output       fred_surf_vals.txt\n"
	    "\n"
	    "    %s                       \\\n"
	    "       -spec         fred.spec                               \\\n"
	    "       -sv           fred_anat+orig                          \\\n"
	    "       -grid_parent  fred_anat+orig                          \\\n"
	    "       -map_func     ave                                     \\\n"
	    "       -m2_steps     10                                      \\\n"
	    "       -cmask       '-a fred_func+orig[2] -expr step(a-0.6)' \\\n"
	    "       -output       fred_surf_ave.txt\n"
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
	    "        for the output.  Output coordinates are based upon\n"
	    "        this dataset.\n"
	    "\n"
	    "    -map_func MAP_FUNC     : surface to dataset function\n"
	    "\n"
	    "        e.g. -map_func ave\n"
	    "        e.g. -map_func mask\n"
	    "        e.g. -map_func midpoint\n"
	    "\n"
	    "        Given one or more surfaces, there are many ways to\n"
	    "        select voxel locations, and to select corresponding\n"
	    "        values for the output dataset.  Some of the functions\n"
	    "        will have separate options.\n"
	    "\n"
	    "        The current mapping functions are:\n"
	    "\n"
	    "          ave      : Given 2 related surfaces, for each node\n"
	    "                     pair, output the average of the dataset\n"
	    "                     values located along the segment joining\n"
	    "                     those nodes.\n"
	    "\n"
	    "                     The -m2_steps option may be added here, to\n"
	    "                     specify the number of points to use in the\n"
	    "                     average.  The default is 5, minimum is 2.\n"
	    "\n"
	    "                     e.g.  -map_func ave -m2_steps 10\n"
	    "\n"
	    "          mask     : For each surface xyz location, output the\n"
	    "                     dataset values of each sub-brick.\n"
	    "\n"
	    "          midpoint : Given 2 related surfaces, for each node\n"
	    "                     pair, output the dataset value with xyz\n"
	    "                     coordinates at the midpoint of the nodes.\n"
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
	    "    out_file           = %s\n"
	    "    spec_file          = %s\n"
	    "    sv_file            = %s\n"
	    "    cmask_cmd          = %s\n"
	    "    map_str            = %s\n"
	    "    debug, no_head     = %d, %d\n"
	    "    m2_steps           = %d\n"
	    , opts,
	    opts->gpar_file, opts->out_file, opts->spec_file, opts->sv_file,
	    opts->cmask_cmd, opts->map_str, opts->debug, opts->no_head,
	    opts->m2_steps
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
	    "    outfp, cmask    = %p : %p\n"
	    "    ncmask, ccount  = %d, %d\n"
	    "    nvox            = %d\n"
	    , p,
	    p->gpar, ISVALID_DSET(p->gpar) ? "valid" : "invalid",
	    p->outfp, p->cmask, p->ncmask, p->ccount, p->nvox
	    );

    return 0;
}


/*----------------------------------------------------------------------
 * disp_smap_opts_t  -  display the contents of the smap_opts_t struct
 *----------------------------------------------------------------------
*/
int disp_smap_opts_t ( char * info, smap_opts_t * sopt )
{
    if ( info )
	fputs( info, stdout );

    if ( sopt == NULL )
    {
	printf( "disp_smap_opts_t: sopt == NULL\n" );
	return -1;
    }

    printf( "smap_opts_t struct at %p :\n"
	    "    map, debug        = %d, %d\n"
	    "    no_head, m2_steps = %d, %d\n"
	    "    cmask             = %p\n"
	    , sopt,
	    sopt->map, sopt->debug, sopt->no_head, sopt->m2_steps, sopt->cmask
	    );

    return 0;
}
