/*----------------------------------------------------------------------
 * main functions for afni (and others):
 *
 *     v2s_results * afni_vol2surf      - create surface data
 *     int           free_v2s_results   - free surface data
 *
 * main interface routing:
 *
 *     v2s_results * vol2surf           - create surface data
 *					  (assumes valid parameters)
 *
 * display functions:
 *
 *     int disp_mri_imarr     ( char * info, MRI_IMARR * dp );
 *     int disp_v2s_opts_t    ( char * info, v2s_opts_t * sopt );
 *     int disp_v2s_param_t   ( char * info, v2s_param_t * p );
 *     int disp_v2s_results   ( char * mesg, v2s_results * d );
 *
 * Author: R Reynolds
 *----------------------------------------------------------------------
 */

#define _VOL2SURF_C_		/* so the header files know */

char gv2s_history[] =
    "----------------------------------------------------------------------\n"
    "vol2surf library history:\n"
    "\n"
    "September 01, 2004 [rickr]\n"
    "  - initial install into afni\n"
    "\n"
    "September 02, 2004 [rickr]\n"
    "  - moved gv2s_map_names here (from SUMA_3dVol2Surf.c)\n"
    "  - moved v2s_map_type (and test) here (from SUMA_3dVol2Surf.c)\n"
    "  - define _VOL2SURF_C_, to allow extern defines in vol2surf.h\n"
    "\n"
    "September 09, 2004 [rickr]\n"
    "  - in afni_vol2surf(), print v2s options when debug > 1\n"
    "  - allow (first_node > last_node) if (last == 0), then change to n-1\n"
    "\n"
    "September 16, 2004 [rickr]\n"
    "  - added support for -gp_index, computing over a single sub-brick\n"
    "    - altered subs in dump_surf_3dt(), max_index in set_surf_results(),\n"
    "      set brick_index in v2s_apply_filter(), mem in alloc_output_mem(),\n"
    "      and added an index check in validate_v2s_inputs()\n"
    "    - add gp_index as a parameter of afni_vol2surf()\n"
    "    - changed keep_norm_dir to norm_dir, allowing default/keep/reverse\n"
    "---------------------------------------------------------------------\n";

#include "mrilib.h"
#include "vol2surf.h"

/*----------------------------------------------------------------------*/
/* local typedefs							*/
typedef struct
{
    int     nused;
    int     nalloc;
    float * list;
} float_list;

typedef struct
{
    THD_3dim_dataset * dset;            /* for data and geometry     */
    THD_fvec3          p1;              /* segment endpoints         */
    THD_fvec3          pn;
    int                debug;           /* for local control         */
} range_3dmm;
                                                                                
typedef struct
{
    MRI_IMARR   ims;                    /* the image array struct     */
    int         masked;                 /* number of masked points    */
    int         ifirst;                 /* 1D index of first point    */
    THD_ivec3   i3first;                /* i3ind index of first point */
    THD_ivec3 * i3arr;                  /* i3ind index array          */
} range_3dmm_res;

/*----------------------------------------------------------------------*/
/* local prototypes							*/
static v2s_results * alloc_output_mem (v2s_opts_t *sopt, v2s_param_t *p);

static int    alloc_ints(int ** ptr, int length, char * dstr, int debug);
static int    alloc_vals_list(float *** ptr, int length, int width, int debug);
static int    check_SUMA_surface( SUMA_surface * s );
static float  directed_dist(float * pnew, float * pold, float *dir, float dist);
static float  dist_f3mm( THD_fvec3 * p1, THD_fvec3 * p2 );
static int    disp_range_3dmm( char * info, range_3dmm * dp );
static int    disp_range_3dmm_res( char * info, range_3dmm_res * dp );
static int    disp_surf_vals( char * mesg, v2s_results * sd, int node );
static int    dump_surf_3dt(v2s_opts_t *sopt, v2s_param_t *p, v2s_results *sd);
static int    f3mm_out_of_bounds(THD_fvec3 *cp, THD_fvec3 *min, THD_fvec3 *max);
static int    float_list_alloc(float_list *f, int **ilist, int size, int trunc);
static int    float_list_comp_mode(float_list *f, float *mode, int *nvals,
                                   int *index);
static int    float_list_slow_sort(float_list * f, int * ilist);
static int    init_seg_endpoints(v2s_opts_t * sopt, v2s_param_t * p,
                                 range_3dmm * R, int node );
static int    init_range_structs( range_3dmm * r3, range_3dmm_res * res3 );
static double magnitude_f( float * p, int length );
static int    set_3dmm_bounds(THD_3dim_dataset *dset, THD_fvec3 *min,
			      THD_fvec3 *max);
static int    set_all_surf_vals (v2s_results * sd, int node_ind, int vind,
                                 int i, int j, int k, float fval);
static int    set_surf_results(v2s_param_t *p, v2s_opts_t *sopt,v2s_results *sd,
                               range_3dmm_res * r3res, int node, int findex);
static int    segment_imarr(range_3dmm_res *res,range_3dmm *R,v2s_opts_t *sopt,
			    byte * cmask);
static int    v2s_adjust_endpts(v2s_opts_t *sopt, THD_fvec3 *p1, THD_fvec3 *pn);
static float  v2s_apply_filter(range_3dmm_res *rr, v2s_opts_t *sopt, int index,
                               int * findex);
static int    v2s_map_needs_sort(int map);
static int    validate_v2s_inputs(v2s_opts_t * sopt, v2s_param_t * p);

/*----------------------------------------------------------------------*/
/* globals to be accessed by plugin and in afni_suma.c                  */
v2s_plugin_opts gv2s_plug_opts = {0, -1, -1};
			    /* this must match v2s_map_nums enum */
char * gv2s_map_names[] = { "none", "mask", "midpoint", "mask2", "ave",
                            "count", "min", "max", "max_abs", "seg_vals",
                            "median", "mode" };


/*----------------------------------------------------------------------
 * afni_vol2surf     - create v2s_results from gv2s_* afni globals
 *
 *    input:   gpar     : AFNI dataset to be used as the grid parent
 *             gp_index : sub-brick selector
 *             sA       : surface A structure
 *             sB       : surface B structure
 *	       mask     : thresholding mask
 * 
 *    output:  sd    : allocated v2s_results struct, with requested data
 *
 * This function is used to map data from an AFNI volume to a surface.
 * These structures are expected to be complete.
 *----------------------------------------------------------------------
*/
v2s_results * afni_vol2surf ( THD_3dim_dataset * gpar, int gp_index,
			SUMA_surface * sA, SUMA_surface * sB, byte * mask )
{
    static v2s_param_t   P;
    v2s_plugin_opts    * popt;
    v2s_opts_t         * sopt;

ENTRY("afni_vol2surf");

    if ( !gv2s_plug_opts.ready ) RETURN(NULL);	/* run away! */
    if ( !gpar )                 RETURN(NULL);

    if (       check_SUMA_surface(sA) ) RETURN(NULL);
    if ( sB && check_SUMA_surface(sB) ) RETURN(NULL);

    popt = &gv2s_plug_opts;
    sopt = &gv2s_plug_opts.sopt;
    sopt->gp_index = gp_index;

    /* now fill the param struct based on the inputs */
    memset(&P, 0, sizeof(P));
    P.gpar       = gpar;
    P.cmask      = mask;
    P.nvox       = DSET_NVOX(gpar);
    P.over_steps = v2s_vals_over_steps(sopt->map);
    P.nsurf      = sB ? 2 : 1;
    P.surf[0]    = *sA;

    if ( sB ) P.surf[1] = *sB;

    if ( sopt->debug > 1 )
    {
	fprintf(stderr, "**afni_vol2surf: \n"
			"  ready, surf_A, surf_B = %d, %d, %d\n",
			popt->ready, popt->surfA, popt->surfB);
	disp_v2s_opts_t ("  options: ", &gv2s_plug_opts.sopt);
    }

    /* fire it up */

    RETURN(vol2surf(sopt, &P));
}


/*----------------------------------------------------------------------
 * vol2surf     - produce a v2s_results surface dataset
 *
 *    input:   sopt  : volume to surface options struct
 *             p     : volume to surface parameter struct
 * 
 *    output:  sd    : allocated v2s_results struct, with requested data
 *
 * This function is used to map data from an AFNI volume to a surface.
 * These structures are expected to be complete.
 *----------------------------------------------------------------------
*/
v2s_results * vol2surf ( v2s_opts_t * sopt, v2s_param_t * p )
{
    v2s_results * sd;
    int           rv;
ENTRY("vol2surf");

    if ( sopt == NULL || p == NULL )
    {
	fprintf( stderr, "** smd_wo - bad params (%p,%p)\n", sopt, p );
	RETURN(NULL);
    }

    if ( validate_v2s_inputs(sopt, p) )
	RETURN(NULL);

    if ( sopt->map == E_SMAP_INVALID )
    {
	fprintf(stderr,"** v2s wo: invalid map %d\n", sopt->map);
	RETURN(NULL);
    }

    sd = alloc_output_mem( sopt, p );
    if ( !sd ) RETURN(NULL);

    if ( sopt->debug > 1 ) disp_v2s_param_t( "-- post alloc_output_mem : ", p );

    rv = dump_surf_3dt( sopt, p, sd );

    if ( sopt->debug > 1 ) disp_v2s_results( "-- post surf creation : ", sd);

    RETURN(sd);
}


/*----------------------------------------------------------------------
 * dump_surf_3dt - for each node index, get an appropriate node sampling,
 *                 and compute and output results across sub-bricks
 *----------------------------------------------------------------------
*/
static int dump_surf_3dt( v2s_opts_t * sopt, v2s_param_t * p, v2s_results * sd )
{
    THD_fvec3      dset_min, dset_max;
    range_3dmm_res r3mm_res;
    range_3dmm     r3mm;
    float          dist, min_dist, max_dist;
    int            sub, subs, nindex, findex = 0;
    int            oobc, oomc, max_index;

ENTRY("dump_surf_3dt");

    if ( ! sopt || ! p || ! sd )
    {
	fprintf(stderr, "** ds3 : bad params (%p,%p,%p)\n", sopt, p, sd );
	RETURN(-1);
    }

    /* note the number of sub-bricks, unless the user has given just one */
    subs = sopt->gp_index >= 0 ? 1 : DSET_NVALS(p->gpar);
    set_3dmm_bounds( p->gpar, &dset_min, &dset_max );
    if ( sopt->debug > 1 )
	fprintf(stderr, "-- dset bounding box: (%f, %f, %f)\n"
			"                      (%f, %f, %f)\n",
		dset_min.xyz[0], dset_min.xyz[1], dset_min.xyz[2], 
		dset_max.xyz[0], dset_max.xyz[1], dset_max.xyz[2]);

    min_dist = 9999.9;						/* v2.3 */
    max_dist = -1.0;
    oobc     = 0; 			  /* init out-of-bounds counter */
    oomc     = 0; 			  /* init out-of-mask counter   */

    init_range_structs( &r3mm, &r3mm_res );		    /* to empty */
    r3mm.dset = p->gpar;

    /* note, NodeList elements are in dicomm mm orientation */

    for ( nindex = sopt->first_node; nindex <= sopt->last_node; nindex++ )
    {
	/* init default max for oob and oom cases */
	max_index = p->over_steps ? sopt->f_steps : subs;

	init_seg_endpoints(sopt, p, &r3mm, nindex);    /* segment endpoints */
	v2s_adjust_endpts( sopt, &r3mm.p1, &r3mm.pn );

  	if ( r3mm.debug )
	    r3mm.debug = 0;

	if ( (sopt->debug > 0) && ( nindex == sopt->dnode ) )
	    r3mm.debug = sopt->debug;

	/* if either point is outside our dataset, skip the pair   v2.3 */
	if ( f3mm_out_of_bounds( &r3mm.p1, &dset_min, &dset_max ) ||
	     f3mm_out_of_bounds( &r3mm.pn, &dset_min, &dset_max ) )
	{
	    oobc++;
	    if ( sopt->oob.show )
		if ( set_all_surf_vals( sd, nindex, sopt->oob.index,
					sopt->oob.index, sopt->oob.index,
					sopt->oob.index, sopt->oob.value) )
		    RETURN(1);
	    if ( (sopt->debug > 0) && ( nindex == sopt->dnode ) )
		disp_surf_vals("-- debug node, out-of-bounds : ", sd, -1);
	    continue;
	}

	dist = dist_f3mm( &r3mm.p1, &r3mm.pn );
	if ( dist < min_dist ) min_dist = dist;
	if ( dist > max_dist ) max_dist = dist;

	if ( segment_imarr( &r3mm_res, &r3mm, sopt, p->cmask ) != 0 )
	    continue;

	if ( r3mm_res.ims.num == 0 )	/* any good voxels in the bunch? */
	{
	    oomc++;
	    if ( sopt->oom.show )
		if ( set_all_surf_vals( sd, nindex, r3mm_res.ifirst,
			r3mm_res.i3first.ijk[0], r3mm_res.i3first.ijk[1],
			r3mm_res.i3first.ijk[2], sopt->oom.value ) )
		    RETURN(1);
	    if ( (sopt->debug > 0) && ( nindex == sopt->dnode ) )
		disp_surf_vals("-- debug node, out-of-mask : ", sd, -1);
	    continue;
	}

	/* get element 0, just for the findex */
	(void)v2s_apply_filter(&r3mm_res, sopt, 0, &findex);

	if ( set_surf_results(p, sopt, sd, &r3mm_res, nindex, findex) )
	    RETURN(-1);

	/* clean up the MRI_IMARR struct, but don't free imarr */
	for ( sub = 0; sub < r3mm_res.ims.num; sub++ )
	{
	    mri_free(r3mm_res.ims.imarr[sub]);
	    r3mm_res.ims.imarr[sub] = NULL;
	}
	r3mm_res.ims.num = 0;
    }

    if ( sopt->debug > 0 )					/* v2.3 */
    {
	fprintf( stderr, "-- node pair dist (min,max) = (%f,%f)\n",
		 min_dist, max_dist );
	fprintf( stderr, "-- out-of-bounds, o-o-mask counts : %d, %d (of %d)\n",
		 oobc, oomc, sopt->last_node - sopt->first_node + 1);
    }

    /* now we can free the imarr and voxel lists */
    if ( r3mm_res.ims.nall > 0 )
    {
	free(r3mm_res.ims.imarr);
	free(r3mm_res.i3arr);
	r3mm_res.ims.nall = 0;
    }

    RETURN(0);
}


/***********************************************************************
 * set_surf_results
 ***********************************************************************
*/
static int set_surf_results(v2s_param_t *p, v2s_opts_t * sopt, v2s_results * sd,
			    range_3dmm_res * r3res, int node, int findex)
{
    int           i, j, k, volind;
    int           c, max_index;
ENTRY("set_surf_results");

    if ( sd->nused >= sd->nalloc )
    {
	fprintf(stderr,"** ssr: nused (%d) >= nalloc (%d)!\n",
		sd->nused, sd->nalloc);
	RETURN(1);
    }

    i = r3res->i3arr[findex].ijk[0];
    j = r3res->i3arr[findex].ijk[1];
    k = r3res->i3arr[findex].ijk[2];

    /* now get 3D and 1D coordinates */
    volind = i + DSET_NX(p->gpar) * (j + DSET_NY(p->gpar) * k );

    /* set everything but the values */
    if (sd->nodes )  sd->nodes[sd->nused]  = node;
    if (sd->volind)  sd->volind[sd->nused] = volind;
    if (sd->i     )  sd->i[sd->nused]      = i;
    if (sd->j     )  sd->j[sd->nused]      = j;
    if (sd->k     )  sd->k[sd->nused]      = k;
    if (sd->nvals )  sd->nvals[sd->nused]  = r3res->ims.num;

    /* rcr : should I nuke the MRI images, and just copy what is needed? */
    if ( ! p->over_steps && sopt->gp_index >= 0 &&
	 (sopt->debug > 1) && (node == sopt->dnode) )
	    fprintf(stderr,"** dnode %d gets %f from gp_index %d\n",
		    node, sd->vals[0][sd->nused], sopt->gp_index);
    
    /* set max_index, and adjust in case max_vals has been restricted */
    max_index = p->over_steps ? r3res->ims.num : DSET_NVALS(p->gpar);
    if ( max_index > sd->max_vals ) max_index = sd->max_vals;

    for ( c = 0; c < max_index; c++ )
	sd->vals[c][sd->nused] = v2s_apply_filter(r3res, sopt, c, NULL);

    /* possibly fill line with default if by steps and short */
    if ( max_index < sd->max_vals )
	for ( c = max_index; c < sd->max_vals; c++ )
	    sd->vals[c][sd->nused] = 0.0;

    if ( (sopt->debug > 1) && (node == sopt->dnode) )
    {
	fprintf(stderr, "-- debug: node, findex, vol_index = %d, %d, %d\n",
		node, findex, volind );
	if ( sopt->use_norms )
	{
	    float * fp = p->surf[0].norm[node].xyz;
	    fprintf(stderr,"-- normal %f, %f, %f\n", fp[0], fp[1], fp[2]);
	}
	disp_mri_imarr( "++ raw data: ", &r3res->ims );
    }

    sd->nused++;

    RETURN(0);
}


/***********************************************************************
 * set_all_surf_vals
 * return 0 on success
 ***********************************************************************
*/
static int set_all_surf_vals( v2s_results * sd, int node_ind, int vind,
			      int i, int j, int k, float fval )
{
    int           c, nused;

ENTRY("set_all_surf_vals");

    nused = sd->nused;

    if ( nused >= sd->nalloc )
    {
	fprintf(stderr,"** sasv: nused=%d >= nalloc=%d!\n", nused, sd->nalloc);
	RETURN(1);
    }

    if ( sd->nodes  )  sd->nodes[nused]  = node_ind;
    if ( sd->volind )  sd->volind[nused] = vind;
    if ( sd->i      )  sd->i[nused]      = i;
    if ( sd->j      )  sd->j[nused]      = j;
    if ( sd->k      )  sd->k[nused]      = k;
    if ( sd->nvals  )  sd->nvals[nused]  = sd->max_vals;

    for ( c = 0; c < sd->max_vals; c++ )
	sd->vals[c][nused] = fval;

    sd->nused++;

    RETURN(0);
}


/***********************************************************************
 * segment_imarr	- get MRI_IMARR for steps over a segment
 *
 * The res->ims structure should be empty, except that it may
 * optionally contain space for pointers in imarr.  Note that nall
 * should be accurate.
 *
 * return 0 on success
 ***********************************************************************
*/
static int segment_imarr( range_3dmm_res *res, range_3dmm *R, v2s_opts_t *sopt,
			  byte * cmask )
{
    THD_fvec3 f3mm;
    THD_ivec3 i3ind;
    float     rat1, ratn;
    int       nx, ny;
    int       step, vindex, prev_ind;

ENTRY("segment_imarr");

    /* check params for validity */
    if ( !R || !sopt || !res || !R->dset )
    {
	fprintf(stderr, "** seg_imarr: invalid params (%p,%p,%p)\n",R,sopt,res);
	if ( R ) disp_range_3dmm("segment_imarr: bad inputs:", R );
	RETURN(-1);
    }

    if ( R->debug > 1 )
	disp_range_3dmm("segment_imarr: ", R );

    /* handle this as an acceptable, trivial case */
    if ( sopt->f_steps < 1 )
    {
	res->ims.num = 0;
	RETURN(0);
    }

    nx = DSET_NX(R->dset);
    ny = DSET_NY(R->dset);

    /* if we don't have enough memory for results, (re)allocate some */
    if ( res->ims.nall < sopt->f_steps )
    {
	if ( R->debug > 1 )
	    fprintf(stderr,"++ realloc of imarr (from %d to %d pointers)\n",
		    res->ims.nall,sopt->f_steps);

	res->ims.nall  = sopt->f_steps;
	res->ims.imarr = realloc(res->ims.imarr,
                                 sopt->f_steps*sizeof(MRI_IMAGE *));
	res->i3arr     = realloc(res->i3arr, sopt->f_steps*sizeof(THD_ivec3));
	if ( !res->ims.imarr || !res->i3arr )
	{
	    fprintf(stderr,"** seg_imarr: no memory for %d MRI_IMAGE ptrs\n",
		    sopt->f_steps);
	    res->ims.nall = res->ims.num = 0;
	    /* one might be good */
	    if ( res->ims.imarr ) free(res->ims.imarr);
	    if ( res->i3arr )     free(res->i3arr);
	    RETURN(-1);
	}
    }

    /* init return structure */
    res->ims.num = 0;
    res->masked  = 0;
    res->i3first = THD_3dmm_to_3dind( R->dset, R->p1 );
    res->ifirst  = res->i3first.ijk[0] +
	           nx * (res->i3first.ijk[1] + ny * res->i3first.ijk[2] );

    prev_ind = -1;			/* in case we want unique voxels */

    for ( step = 0; step < sopt->f_steps; step++ )
    {
	/* set our endpoint ratios */
	if ( sopt->f_steps < 2 )     /* if this is based on a single point */
	    ratn = 0.0;
	else
	    ratn = (float)step / (sopt->f_steps - 1);
	rat1 = 1.0 - ratn;

	f3mm.xyz[0] = rat1 * R->p1.xyz[0] + ratn * R->pn.xyz[0];
	f3mm.xyz[1] = rat1 * R->p1.xyz[1] + ratn * R->pn.xyz[1];
	f3mm.xyz[2] = rat1 * R->p1.xyz[2] + ratn * R->pn.xyz[2];

	i3ind = THD_3dmm_to_3dind( R->dset, f3mm );
	vindex = i3ind.ijk[0] + nx * (i3ind.ijk[1] + ny * i3ind.ijk[2] );

	/* is this voxel masked out? */
	if ( cmask && !cmask[vindex] )
	{
	    res->masked++;
	    continue;
	}

	/* is this voxel repeated, and if so, do we skip it? */
	if ( sopt->f_index == V2S_INDEX_VOXEL && vindex == prev_ind )
	    continue;

	/* Huston, we have a good voxel... */

	prev_ind = vindex;		/* note this for next time  */

	/* now for the big finish, get and insert the actual data */

	res->i3arr    [res->ims.num] = i3ind;	/* store the 3-D indices */
	res->ims.imarr[res->ims.num] = THD_extract_series( vindex, R->dset, 0 );
	res->ims.num++;

	if ( R->debug > 2 )
	    fprintf(stderr, "-- seg step %2d, vindex %d, coords %f %f %f\n",
		    step,vindex,f3mm.xyz[0],f3mm.xyz[1],f3mm.xyz[2]);
    }

    if ( R->debug > 0 )
	disp_range_3dmm_res( "++ i3mm_seg_imarr results: ", res );

    RETURN(0);
}


/*----------------------------------------------------------------------
 * f3mm_out_of_bounds	 - check wether cp is between min and max
 *
 * 			 - v2.3 [rickr]
 *----------------------------------------------------------------------
*/
static int f3mm_out_of_bounds( THD_fvec3 * cp, THD_fvec3 * min, THD_fvec3 * max)
{
    int c;

    if ( !cp || !min || !max )
	return(-1);

    for ( c = 0; c < 3; c++ )
    {
	if ( ( cp->xyz[c] < min->xyz[c] ) ||
	     ( cp->xyz[c] > max->xyz[c] ) )
	    return(-1);
    }

    return(0);
}


/*----------------------------------------------------------------------
 * v2s_adjust_endpoints		- adjust endpoints for map and options
 *
 * return   0 on success
 *        < 0 on error
 *----------------------------------------------------------------------
*/
static int v2s_adjust_endpts( v2s_opts_t * sopt, THD_fvec3 * p1, THD_fvec3 * pn)
{
    THD_fvec3 f3_diff;
    float     dist, factor;

ENTRY("v2s_adjust_endpts");

    if ( !sopt || !p1 || !pn )
    {
	fprintf(stderr,"** v2s_ae: invalid params (%p,%p,%p)\n", sopt, p1, pn);
	RETURN(-1);
    }

    /* first, get the difference, and distance */
    f3_diff.xyz[0] = pn->xyz[0] - p1->xyz[0];
    f3_diff.xyz[1] = pn->xyz[1] - p1->xyz[1];
    f3_diff.xyz[2] = pn->xyz[2] - p1->xyz[2];

    dist = dist_f3mm( p1, pn );

    if ( (sopt->f_p1_fr != 0.0) || (sopt->f_p1_mm != 0.0) )
    {
	if ( sopt->f_p1_fr != 0.0 )	/* what the heck, choose fr if both */
	    factor = sopt->f_p1_fr;
	else
	    factor = (dist == 0.0) ? 0.0 : sopt->f_p1_mm / dist;

	p1->xyz[0] += factor * f3_diff.xyz[0];
	p1->xyz[1] += factor * f3_diff.xyz[1];
	p1->xyz[2] += factor * f3_diff.xyz[2];
    }

    if ( (sopt->f_pn_fr != 0.0) || (sopt->f_pn_mm != 0.0) )
    {
	if ( sopt->f_pn_fr != 0.0 )
	    factor = sopt->f_pn_fr;
	else
	    factor = (dist == 0.0) ? 0.0 : sopt->f_pn_mm / dist;

	pn->xyz[0] += factor * f3_diff.xyz[0];
	pn->xyz[1] += factor * f3_diff.xyz[1];
	pn->xyz[2] += factor * f3_diff.xyz[2];
    }

    switch ( sopt->map )
    {
	default:
	    fprintf(stderr,"** v2s_ae: mapping %d not ready\n", sopt->map );
	    RETURN(-1);

	case E_SMAP_AVE:
	case E_SMAP_MAX:
	case E_SMAP_MAX_ABS:
	case E_SMAP_MIN:
	case E_SMAP_MASK:
	case E_SMAP_SEG_VALS:
	case E_SMAP_MEDIAN:
	case E_SMAP_MODE:
	    break;

	case E_SMAP_MIDPT:

	    /* set the first point to be the average of the two */
	    p1->xyz[0] = (p1->xyz[0] + pn->xyz[0]) / 2.0;
	    p1->xyz[1] = (p1->xyz[1] + pn->xyz[1]) / 2.0;
	    p1->xyz[2] = (p1->xyz[2] + pn->xyz[2]) / 2.0;

	    break;
    }

    RETURN(0);
}


/*---------------------------------------------------------------------------
 * v2s_apply_filter  - compute results for the given function and index
 *
 * As a side step, return any filter result index.
 *---------------------------------------------------------------------------
*/
static float v2s_apply_filter( range_3dmm_res * rr, v2s_opts_t * sopt,
			       int index, int * findex )
{
    static float_list flist = { 0, 0, NULL };    /* for sorting results */
    static int      * ind_list = NULL;		 /* track index sources */
    double            tmp, comp = 0.0;
    float             fval;
    int               count, source;
    int               brick_index = 0;

ENTRY("v2s_apply_filter");

    if ( !rr || !sopt || index < 0 )
    {
	fprintf(stderr,"** v2s_cm2: invalid params (%p,%p,%d)\n",
		rr, sopt, index);
	RETURN(0.0);
    }
    
    if ( rr->ims.num <= 0 )
	RETURN(0.0);

    /* if sorting is required for resutls, do it now */
    if ( v2s_map_needs_sort( sopt->map ) )
    {
	if ( float_list_alloc( &flist, &ind_list, rr->ims.num, 0 ) != 0 )
	    RETURN(0.0);
	for ( count = 0; count < rr->ims.num; count++ )
	{
	    flist.list[count] = MRI_FLOAT_PTR(rr->ims.imarr[count])[index];
	    ind_list  [count] = count;   /* init index sources */
	}
	flist.nused = rr->ims.num;
	float_list_slow_sort( &flist, ind_list );
    }

    switch ( sopt->map )
    {
	default:
	    if ( findex ) *findex = 0;
	    RETURN(0.0);

	case E_SMAP_AVE:
	    if ( findex ) *findex = 0;
	    for ( count = 0; count < rr->ims.num; count++ )
		comp += MRI_FLOAT_PTR(rr->ims.imarr[count])[index];

	    comp = comp / rr->ims.num;
	    break;

	case E_SMAP_MASK:
	case E_SMAP_MIDPT:
	    if ( findex ) *findex = 0;
	    /* we have only the one point */
	    comp = MRI_FLOAT_PTR(rr->ims.imarr[0])[index];
	    break;

	case E_SMAP_MAX:
	    comp = MRI_FLOAT_PTR(rr->ims.imarr[0])[index];
	    if ( findex ) *findex = 0;

	    for ( count = 1; count < rr->ims.num; count++ )
	    {
		tmp = MRI_FLOAT_PTR(rr->ims.imarr[count])[index];
		if ( tmp > comp )
		{
		    if ( findex ) *findex = count;
		    comp = tmp;
		}
	    }
	    break;

	case E_SMAP_MAX_ABS:
	    comp = MRI_FLOAT_PTR(rr->ims.imarr[0])[index];
	    if ( findex ) *findex = 0;

	    for ( count = 1; count < rr->ims.num; count++ )
	    {
		tmp = MRI_FLOAT_PTR(rr->ims.imarr[count])[index];
		if ( fabs(tmp) > fabs(comp) )
		{
		    if ( findex ) *findex = count;
		    comp = tmp;
		}
	    }
	    break;

	case E_SMAP_MIN:
	    comp = MRI_FLOAT_PTR(rr->ims.imarr[0])[index];
	    if ( findex ) *findex = 0;

	    for ( count = 1; count < rr->ims.num; count++ )
	    {
		tmp = MRI_FLOAT_PTR(rr->ims.imarr[count])[index];
		if ( tmp < comp )
		{
		    if ( findex ) *findex = count;
		    comp = tmp;
		}
	    }
	    break;

	case E_SMAP_SEG_VALS:
	    /* if the user has specified a brick index, use it */
	    if ( sopt->gp_index >= 0 ) brick_index = sopt->gp_index;
	    if ( findex ) *findex = 0;
	    comp = MRI_FLOAT_PTR(rr->ims.imarr[index])[brick_index];
	    break;

	case E_SMAP_MEDIAN:
	    count = flist.nused >> 1;
	    if ( (flist.nused & 1) || (count == 0) )
	    {
		comp = flist.list[count];
		if ( findex ) *findex = ind_list[count];
	    }
	    else
	    {
		comp = (flist.list[count-1] + flist.list[count]) / 2;
		if ( findex ) *findex = ind_list[count-1]; /* take first */
	    }
	    break;

	case E_SMAP_MODE:
	    float_list_comp_mode(&flist, &fval, &count, &source);
	    comp = fval;
	    if ( findex ) *findex = ind_list[source];
	    break;
    }

    RETURN((float)comp);
}


/*----------------------------------------------------------------------
 * v2s_map_needs_sort		- does this map function require sorting?
 *
 * return  1 on true
 *         0 on false
 *----------------------------------------------------------------------
*/
static int v2s_map_needs_sort( int map )
{
    if ( (map == E_SMAP_MEDIAN) || (map == E_SMAP_MODE) )
	return 1;

    return 0;
}


/*----------------------------------------------------------------------
 * float_list_comp_mode		- compute the mode of the list
 *
 * return  0 : on success
 *        -1 : on error
 *----------------------------------------------------------------------
*/
static int float_list_comp_mode( float_list *f, float *mode, int *nvals,
				 int *index )
{
    float fcur;
    int   ncur, c;

ENTRY("float_list_comp_mode");

    /* init default results */
    *nvals = ncur = 1;
    *mode  = fcur = f->list[0];
    *index = 0;

    for ( c = 1; c < f->nused; c++ )
    {
	if ( f->list[c] == fcur )
	    ncur ++;
	else			    /* found a new entry to count   */
	{
	    if ( ncur > *nvals )     /* keep track of any new winner */
	    {
		*mode  = fcur;
		*nvals = ncur;
		*index = c;
	    }

	    fcur = f->list[c];
	    ncur = 1;
	}
    }

    if ( ncur > *nvals )     /* keep track of any new winner */
    {
	*mode  = fcur;
	*nvals = ncur;
	*index = c;
    }

    RETURN(0);
}


/*----------------------------------------------------------------------
 * float_list_slow_sort		- sort (small) float list
 *
 * If ilist, track index sources.
 *
 * return   0 on success
 *        < 0 on error
 *----------------------------------------------------------------------
*/
static int float_list_slow_sort( float_list * f, int * ilist )
{
    float * list, save;
    int     c0, c1, sindex;

ENTRY("float_list_slow_sort");

    list = f->list;  /* for any little speed gain */

    for ( c0 = 0; c0 < f->nused-1; c0++ )
    {
	sindex = c0;
	save   = list[c0];

	/* find smallest remaining */
	for ( c1 = c0+1; c1 < f->nused; c1++ )
	    if ( list[c1] < save )
	    {
		sindex = c1;
		save   = list[sindex];
	    }

	/* swap if smaller was found */
	if ( sindex > c0 )
	{
	    list[sindex] = list[c0];
	    list[c0]     = save;

	    if ( ilist )        /* make same swap of indices */
	    {
		c1            = ilist[sindex];
		ilist[sindex] = ilist[c0];
		ilist[c0]     = c1;
	    }
	}
    }

    RETURN(0);
}


/*----------------------------------------------------------------------
 * float_list_alloc		- verify float list memory
 *
 * If trunc(ate), realloc down to necessary size.
 * If ilist, make space for accompanying int list.
 *
 * return   0 on success
 *        < 0 on error
 *----------------------------------------------------------------------
*/
static int float_list_alloc(float_list *f, int **ilist, int size, int trunc)
{
ENTRY("float_list_alloc");

    if ( (f->nalloc < size) ||
	 (trunc && (f->nalloc > size)) )
    {
	f->list = (float *)realloc(f->list, size * sizeof(float));
	if ( ! f->list )
	{
	    fprintf(stderr,"** float_list_alloc: failed for %d floats\n", size);
	    RETURN(-2);
	}
	f->nalloc = size;

	if ( ilist )	 /* then allocate accompanying ilist */
	{
	    *ilist = (int *)realloc(*ilist, size * sizeof(int));
	    if ( ! *ilist )
	    {
		fprintf(stderr,"** float_list_alloc: failed for %d ints\n",
			size);
		RETURN(-2);
	    }
	}

	if ( trunc && (f->nused > f->nalloc) )
	    f->nused = f->nalloc;
    }

    RETURN(0);
}


/*---------------------------------------------------------------------------
 * directed_dist  - travel from pold, along dir, a distance of dist
 *---------------------------------------------------------------------------
*/
static float directed_dist(float * pnew, float * pold, float * dir, float dist)
{
    double mag, ratio;
    int    c;

ENTRY("directed_dist");

    mag = magnitude_f(dir, 3);

    if ( mag < V2S_EPSILON )	/* can't be negative */
	ratio = 0.0;
    else
	ratio = dist/mag;

    for ( c = 0; c < 3; c++ )
	pnew[c] = pold[c] + dir[c]*ratio;

    RETURN(dist);
}


/*----------------------------------------------------------------------
 * init_seg_endpoints              - initialize segment endpoints
 *----------------------------------------------------------------------
*/
static int init_seg_endpoints ( v2s_opts_t * sopt, v2s_param_t * p,
				range_3dmm * R, int node )
{
    SUMA_surface * sp;
    THD_fvec3      p1, pn;
ENTRY("init_seg_endpoints");

    /* get node from first surface */
    sp = p->surf;
    p1.xyz[0] = sp->ixyz[node].x;
    p1.xyz[1] = sp->ixyz[node].y;
    p1.xyz[2] = sp->ixyz[node].z;

    /* note the endpoints */
    if ( sopt->use_norms )
    {
	/* first apply normals, then transform to current 3dmm */
	directed_dist(pn.xyz, p1.xyz, sp->norm[node].xyz, sopt->norm_len);

	R->p1 = THD_dicomm_to_3dmm(R->dset, p1);
	R->pn = THD_dicomm_to_3dmm(R->dset, pn);
    }
    else
    {
	R->p1 = THD_dicomm_to_3dmm(R->dset, p1);

	if ( p->nsurf > 1 )
	{
	    /* get node from second surface */
	    sp = p->surf + 1;
	    pn.xyz[0] = sp->ixyz[node].x;
	    pn.xyz[1] = sp->ixyz[node].y;
	    pn.xyz[2] = sp->ixyz[node].z;

	    R->pn = THD_dicomm_to_3dmm(R->dset, pn);
	}
	else
	    R->pn = R->p1;
    }

    RETURN(0);
}


/*----------------------------------------------------------------------
 * init_range_structs
 *----------------------------------------------------------------------
*/
static int init_range_structs( range_3dmm * r3, range_3dmm_res * res3 )
{
ENTRY("init_range_structs");

    r3->dset        = NULL;
    r3->debug       = 0;

    res3->ims.num   = 0;
    res3->ims.nall  = 0;
    res3->ims.imarr = NULL;
    res3->i3arr     = NULL;

    RETURN(0);
}


/*----------------------------------------------------------------------
 * set_3dmm_bounds       - note 3dmm bounding values
 *
 * This is an outer bounding box, like FOV, not SLAB.
 *----------------------------------------------------------------------
*/
int set_3dmm_bounds ( THD_3dim_dataset *dset, THD_fvec3 *min, THD_fvec3 *max)
{
    float tmp;
    int   c;
                                                                                
ENTRY("set_3dmm_bounds");
                                                                                
    if ( !dset || !min || !max )
    {
        fprintf(stderr, "** invalid params to set_3dmm_bounds: (%p,%p,%p)\n",
                dset, min, max );
        RETURN(-1);
    }
                                                                                
    /* get undirected bounds */
    min->xyz[0] = DSET_XORG(dset) - 0.5 * DSET_DX(dset);
    max->xyz[0] = min->xyz[0] + DSET_NX(dset) * DSET_DX(dset);
                                                                                
    min->xyz[1] = DSET_YORG(dset) - 0.5 * DSET_DY(dset);
    max->xyz[1] = min->xyz[1] + DSET_NY(dset) * DSET_DY(dset);
                                                                                
    min->xyz[2] = DSET_ZORG(dset) - 0.5 * DSET_DZ(dset);
    max->xyz[2] = min->xyz[2] + DSET_NZ(dset) * DSET_DZ(dset);
                                                                                
    for ( c = 0; c < 3; c++ )
        if ( min->xyz[c] > max->xyz[c] )
        {
            tmp = min->xyz[c];
            min->xyz[c] = max->xyz[c];
            max->xyz[c] = tmp;
        }
                                                                                
    RETURN(0);
}


/*----------------------------------------------------------------------
 * alloc_output_mem  - for output surface dataset
 *----------------------------------------------------------------------
*/
static v2s_results * alloc_output_mem(v2s_opts_t *sopt, v2s_param_t *p)
{
    v2s_results * sd;
    int           rv = 0, mem, nnodes;

ENTRY("allocate_output_mem");

    /* if last <= 0, it will be set to nodes-1 */
    if ( sopt->first_node > sopt->last_node && sopt->last_node > 0 )
    {
	fprintf(stderr,"** error : first_node (%d) > last_node (%d)\n",
		sopt->first_node, sopt->last_node);
	RETURN(NULL);
    }

    nnodes = p->surf[0].num_ixyz;		/* just for typing ease */

    sd = calloc(1, sizeof(v2s_results));
    if ( ! sd )
    {
	fprintf(stderr,"** aom: failed to allocate v2s_results struct\n");
	RETURN(NULL);
    }

    /* explicitly initialize all pointers with NULL */
    sd->nodes = sd->volind = sd->i = sd->j = sd->k = sd->nvals = NULL;
    sd->vals  = NULL;
 
    /* rcr - eventually, this may not apply */
    if ( sopt->first_node <  0      ) sopt->first_node = 0;
    if ( sopt->first_node >= nnodes ) sopt->first_node = nnodes-1;
    if ( sopt->last_node  <= 0      ) sopt->last_node  = nnodes-1;
    if ( sopt->last_node  >= nnodes ) sopt->last_node  = nnodes-1;

    sd->nalloc   = sopt->last_node - sopt->first_node + 1;
    sd->nused    = 0;

    /* decide the maximum number of entries per row */
    if ( p->over_steps )            sd->max_vals = sopt->f_steps;
    else if ( sopt->gp_index >= 0 ) sd->max_vals = 1;
    else                            sd->max_vals = DSET_NVALS(p->gpar);

    if ( sopt->skip_cols & V2S_SKIP_VALS ) sd->max_vals = 1;

    if ( p->over_steps && (sopt->skip_cols & V2S_SKIP_VALS) )
    {
	fprintf(stderr,"** if SKIP_VALS, cannot compute results over steps\n");
	free(sd);
	RETURN(NULL);
    }

    sd->memory   = 0;

    /* first, compute the memory needed for one row */
    mem = 0;
    if ( !(sopt->skip_cols & V2S_SKIP_NODES ) ) mem += sizeof(int);
    if ( !(sopt->skip_cols & V2S_SKIP_VOLIND) ) mem += sizeof(int);
    if ( !(sopt->skip_cols & V2S_SKIP_I     ) ) mem += sizeof(int);
    if ( !(sopt->skip_cols & V2S_SKIP_J     ) ) mem += sizeof(int);
    if ( !(sopt->skip_cols & V2S_SKIP_K     ) ) mem += sizeof(int);
    if ( !(sopt->skip_cols & V2S_SKIP_NVALS ) ) mem += sizeof(int);

    /* now note the actual output values */
    if ( !(sopt->skip_cols & V2S_SKIP_VALS  ) ) mem += sizeof(float);
    else                         mem += sd->max_vals * sizeof(float);

    mem *= sd->nalloc;  /* and multiply by the height of each column */
    sd->memory = mem;

    if ( sopt->debug > 0 )
	fprintf(stderr,"++ allocating memory for results: %d bytes\n", mem);

    /* okay, this time let's allocate something... */

    if ( ! (sopt->skip_cols & V2S_SKIP_NODES) )
        rv = alloc_ints(&sd->nodes, sd->nalloc, "nodes", sopt->debug);

    if ( ! rv && ! (sopt->skip_cols & V2S_SKIP_VOLIND) )
        rv = alloc_ints(&sd->volind, sd->nalloc, "volind", sopt->debug);

    if ( ! rv && ! (sopt->skip_cols & V2S_SKIP_I) )
        rv = alloc_ints(&sd->i, sd->nalloc, "i", sopt->debug);

    if ( ! rv && ! (sopt->skip_cols & V2S_SKIP_J) )
        rv = alloc_ints(&sd->j, sd->nalloc, "j", sopt->debug);

    if ( ! rv && ! (sopt->skip_cols & V2S_SKIP_K) )
        rv = alloc_ints(&sd->k, sd->nalloc, "k", sopt->debug);

    if ( ! rv && ! (sopt->skip_cols & V2S_SKIP_NVALS) )
        rv = alloc_ints(&sd->nvals, sd->nalloc, "nvals", sopt->debug);

    if ( ! rv && ! (sopt->skip_cols & V2S_SKIP_VALS) )
	rv = alloc_vals_list(&sd->vals, sd->nalloc, sd->max_vals, sopt->debug);
    else if ( ! rv )
	rv = alloc_vals_list(&sd->vals, sd->nalloc, 1, sopt->debug);

    if ( rv )
    {
	fprintf(stderr,"** failed v2s_results allocation\n");
	free_v2s_results(sd);
	sd = NULL;
    }

    RETURN(sd);
}


/*----------------------------------------------------------------------
 * alloc_vals_list  - allocate 2D array for surface data values
 *----------------------------------------------------------------------
*/
static int alloc_vals_list(float *** ptr, int length, int width, int debug)
{
    int c;

ENTRY("alloc_vals_list");

    *ptr = (float **)malloc(width * sizeof(float *));
    if ( !*ptr )
	fprintf(stderr,"** avl: failed to alloc %d floats pointers\n", width);

    for ( c = 0; c < width; c++ )
    {
	(*ptr)[c] = (float *)malloc(length * sizeof(float));
	if ( (*ptr)[c] == NULL )
	    fprintf(stderr,"** avl: failed to alloc %d floats (# %d of %d)\n",
		    length, c, width);
    }

    if ( debug > 1 )
	fprintf(stderr,"-- alloc'd %d x %d floats for surf data\n",
		width, length);

    RETURN(0);
}


/*----------------------------------------------------------------------
 * alloc_ints  - allocate 1D array of ints
 *----------------------------------------------------------------------
*/
static int alloc_ints( int ** ptr, int length, char * dstr, int debug )
{
ENTRY("alloc_ints");

    *ptr = (int *)malloc(length * sizeof(int));
    if ( ! *ptr )
    {
	fprintf(stderr,"** ai: failed to alloc %d ints for '%s'\n",length,dstr);
	RETURN(1);
    }
    if ( debug > 1 )
	fprintf(stderr,"-- ai: alloc'd %d ints for '%s'\n", length, dstr);

    RETURN(0);
}


/*----------------------------------------------------------------------
 * disp_v2s_param_t  -  display the contents of the v2s_param_t struct
 *----------------------------------------------------------------------
*/
int disp_v2s_param_t ( char * info, v2s_param_t * p )
{
ENTRY("disp_v2s_param_t");

    if ( info )
	fputs( info, stderr );

    if ( p == NULL )
    {
	fprintf(stderr, "disp_v2s_param_t: p == NULL\n" );
	RETURN(-1);
    }

    fprintf(stderr,
	    "v2s_param_t struct at  %p :\n"
	    "    gpar  : vcheck   = %p : %s\n"
	    "    cmask            = %p\n"
	    "    nvox, over_steps = %d, %d\n"
	    , p,
	    p->gpar, ISVALID_DSET(p->gpar) ? "valid" : "invalid",
	    p->cmask, p->nvox, p->over_steps
	    );

    RETURN(0);
}


/*----------------------------------------------------------------------
 * disp_v2s_opts_t  -  display the contents of the v2s_opts_t struct
 *----------------------------------------------------------------------
*/
int disp_v2s_opts_t ( char * info, v2s_opts_t * sopt )
{
ENTRY("disp_v2s_opts_t");

    if ( info )
	fputs( info, stderr );

    if ( sopt == NULL )
    {
	fprintf(stderr, "disp_v2s_opts_t: sopt == NULL\n");
	RETURN(-1);
    }

    fprintf(stderr,
	    "v2s_opts_t struct at %p  :\n"
	    "    map, gp_index         = %d, %d\n"
	    "    debug, dnode          = %d, %d\n"
	    "    no_head, skip_cols    = %d, %d\n"
	    "    first_node, last_node = %d, %d\n"
	    "    use_norms, norm_len   = %d, %f\n"
	    "    norm_dir              = %d\n"
	    "    f_index, f_steps      = %d, %d\n"
	    "    f_p1_fr, f_pn_fr      = %f, %f\n"
	    "    f_p1_mm, f_pn_mm      = %f, %f\n"
	    "    outfile_1D            = %s\n"
	    "    outfile_niml          = %s\n"
	    , sopt,
	    sopt->map, sopt->gp_index, sopt->debug, sopt->dnode,
	    sopt->no_head, sopt->skip_cols,
	    sopt->first_node, sopt->last_node,
	    sopt->use_norms, sopt->norm_len, sopt->norm_dir,
	    sopt->f_index, sopt->f_steps,
	    sopt->f_p1_fr, sopt->f_pn_fr, sopt->f_p1_mm, sopt->f_pn_mm,
	    CHECK_NULL_STR(sopt->outfile_1D),
	    CHECK_NULL_STR(sopt->outfile_niml)
	    );

    RETURN(0);
}


/*----------------------------------------------------------------------
 * magnitude_f		- return magnitude of float vector
 *----------------------------------------------------------------------
*/
static double magnitude_f( float * p, int length )
{
    int     c;
    double  sums = 0.0;

    for ( c = 0; c < length; c++, p++ )
	sums += (*p) * (*p);

    return(sqrt(sums));
}


/*----------------------------------------------------------------------
 * dist_f3mm		- return Euclidean distance between the points
 *----------------------------------------------------------------------
*/
static float dist_f3mm( THD_fvec3 * p1, THD_fvec3 * p2 )
{
    double d0, d1, d2;

    if ( p1 == NULL || p2 == NULL )
    {
	fprintf( stderr, "** dist_f3mm: invalid params (%p,%p)\n", p1, p2 );
	return(0.0);
    }

    d0 = p1->xyz[0] - p2->xyz[0];
    d1 = p1->xyz[1] - p2->xyz[1];
    d2 = p1->xyz[2] - p2->xyz[2];

    return(sqrt(d0*d0 + d1*d1 + d2*d2));
}


/*---------------------------------------------------------------------------
 * disp_range_3dmm  -  display the contents of the range_3dmm struct
 *---------------------------------------------------------------------------
*/
static int disp_range_3dmm ( char * info, range_3dmm * dp )
{
ENTRY("disp_range_3dmm");

    if ( info )
	fputs( info, stderr );

    if ( dp == NULL )
    {
	fprintf(stderr, "disp_range_3dmm: dp == NULL\n");
	RETURN(-1);
    }

    fprintf(stderr,
	    "range_3dmm struct at %p :\n"
	    "    dset    = %p : %s\n"
	    "    p1      = (%f, %f, %f)\n"
	    "    pn      = (%f, %f, %f)\n",
	    dp, dp->dset, ISVALID_DSET(dp->dset) ? "valid" : "invalid",
	    dp->p1.xyz[0], dp->p1.xyz[1], dp->p1.xyz[2],
	    dp->pn.xyz[0], dp->pn.xyz[1], dp->pn.xyz[2] );

    RETURN(0);
}


/*---------------------------------------------------------------------------
 * disp_range_3dmm_res  -  display the contents of the range_3dmm_res struct
 *---------------------------------------------------------------------------
*/
static int disp_range_3dmm_res ( char * info, range_3dmm_res * dp )
{
ENTRY("disp_range_3dmm_res");

    if ( info )
	fputs( info, stderr );

    if ( dp == NULL )
    {
	fprintf(stderr, "disp_range_3dmm: dp == NULL\n");
	RETURN(-1);
    }

    fprintf(stderr,
	    "range_3dmm_res struct at %p :\n"
	    "    ims.num, ims.nall  = %d, %d\n"
	    "    ims.imarr          = %p\n"
	    "    masked, ifirst     = %d, %d\n"
	    "    i3first[0,1,2]     = %d, %d, %d\n"
	    "    i3arr              = %p\n"
	    , dp,
	    dp->ims.num, dp->ims.nall, dp->ims.imarr, dp->masked, dp->ifirst,
	    dp->i3first.ijk[0], dp->i3first.ijk[1], dp->i3first.ijk[2],
	    dp->i3arr );

    if ( dp->i3arr )
	fprintf(stderr,
	    "    i3arr[0].ijk       = %d, %d, %d\n",
	    dp->i3arr[0].ijk[0], dp->i3arr[0].ijk[1], dp->i3arr[0].ijk[2] );

    RETURN(0);
}


/*---------------------------------------------------------------------------
 * disp_mri_imarr  -  display the contents of the MRI_IMARR struct
 *---------------------------------------------------------------------------
*/
int disp_mri_imarr ( char * info, MRI_IMARR * dp )
{
    float * fp;
    int     cr, cc;

ENTRY("disp_mri_imarr");

    if ( info )
	fputs( info, stderr );

    if ( dp == NULL )
    {
	fprintf(stderr, "disp_mri_imarr: dp == NULL\n");
	RETURN(-1);
    }

    fprintf(stderr,
	    "mri_imarr struct at %p :\n"
	    "    num, nall = %d, %d\n",
	    dp, dp->num, dp->nall );

    for ( cr = 0; cr < dp->num; cr++ )
    {
	fp = MRI_FLOAT_PTR(dp->imarr[cr]);
	fprintf(stderr, "    %3d: ", cr);
	for ( cc = 0; cc < dp->imarr[cr]->nx; cc++, fp++ )
	    fprintf(stderr, "%f  ", *fp );
	fputc( '\n', stderr );
    }

    RETURN(0);
}


/***********************************************************************
 * disp_surf_vals
 ***********************************************************************
*/
static int disp_surf_vals( char * mesg, v2s_results * sd, int node )
{
    int index, c;

ENTRY("disp_surf_vals");

    if ( mesg ) fputs( mesg, stderr );
    if ( sd->nused < 1 )
    {
	fprintf(stderr,"** no surf nodes defined\n");
	RETURN(-1);
    }

    index = (node >= 0) ? node : sd->nodes[sd->nused - 1];

    fprintf(stderr, "v2s_results vals for sd_index %d, node %d :\n"
		    "    volind, (i, j, k) = %d, (%d, %d, %d)\n"
		    "    nvals: values...  = %d:  ", index,
		sd->nodes  ? sd->nodes[index]  : 0,
		sd->volind ? sd->volind[index] : 0,
		sd->i      ? sd->i[index]      : 0,
		sd->j      ? sd->j[index]      : 0,
		sd->k      ? sd->k[index]      : 0,
		sd->nvals  ? sd->nvals[index]  : 0 );

    for ( c = 0; c < sd->max_vals; c++ )
	fprintf(stderr,"%s  ", MV_format_fval(sd->vals[c][index]));
    fputc('\n', stderr);

    RETURN(0);
}


/***********************************************************************
 * disp_v2s_results
 ***********************************************************************
*/
int disp_v2s_results( char * mesg, v2s_results * d )
{
ENTRY("disp_v2s_results");

    if ( mesg ) fputs( mesg, stderr );

    fprintf(stderr, "v2s_results @ %p\n"
		    "    nalloc, nused    = %d, %d\n"
		    "    max_vals, memory = %d, %d\n"
		    "    nodes, volind    = %p, %p\n"
		    "    i, j, k          = %p, %p, %p\n"
		    "    nvals, vals      = %p, %p\n",
	    	    d, d->nalloc, d->nused, d->max_vals, d->memory,
		    d->nodes, d->volind, d->i, d->j, d->k, d->nvals, d->vals);

    RETURN(0);
}


/*----------------------------------------------------------------------
 * free_v2s_results  - free contents of v2s_results struct
 *----------------------------------------------------------------------
*/
int free_v2s_results( v2s_results * sd )
{
    int c;
                                                                                
ENTRY("free_v2s_results");

    if( ! sd ) RETURN(0);
                                                                                
    if (sd->nodes)  { free(sd->nodes);   sd->nodes  = NULL; }
    if (sd->volind) { free(sd->volind);  sd->volind = NULL; }
    if (sd->i)      { free(sd->i);       sd->i      = NULL; }
    if (sd->j)      { free(sd->j);       sd->j      = NULL; }
    if (sd->k)      { free(sd->k);       sd->k      = NULL; }
    if (sd->nvals)  { free(sd->nvals);   sd->nvals  = NULL; }
                                                                                
    if (sd->vals)
    {
        for ( c = 0; c < sd->max_vals; c++ )
            if ( sd->vals[c] ) { free(sd->vals[c]);  sd->vals[c] = NULL; }
                                                                                
        free(sd->vals);
        sd->vals = NULL;
    }

    free(sd);
                                                                                
    RETURN(0);
}


/*----------------------------------------------------------------------
 * validate structure contents
 *----------------------------------------------------------------------
*/
static int validate_v2s_inputs ( v2s_opts_t * sopt, v2s_param_t * p )
{
    int c;

ENTRY("validate_v2s_inputs");

    if ( !sopt || !p )
    {
	fprintf(stderr,"** validate_v2s_inputs: bad params (%p,%p)\n", sopt, p);
	RETURN(-1);
    }

    /* validate surface options structure */
    if ( sopt->map <= E_SMAP_INVALID || sopt->map >= E_SMAP_FINAL )
    {
	fprintf(stderr,"** invalid mapping index %d\n", sopt->map);
	RETURN(1);
    }
    else if ( v2s_map_type(gv2s_map_names[sopt->map]) != sopt->map )
    {
	fprintf(stderr,"** mapping index failure for %d\n", sopt->map);
	RETURN(1);
    }

    if ( sopt->f_steps <= 0 || sopt->f_steps >= V2S_STEPS_TOOOOO_BIG )
    {
	fprintf(stderr,"** invalid f_steps = %d\n", sopt->f_steps);
	RETURN(1);
    }

    /* validate the contents of p, proper */
    if ( !p->gpar ) {fprintf(stderr,"** vv2si: no dset?\n"); RETURN(2);}

    if ( p->nvox != DSET_NVOX(p->gpar) )
    {
	fprintf(stderr,"** invalid voxel count (%d) for grid_parent\n",p->nvox);
	RETURN(2);
    }

    if ( sopt->gp_index >= DSET_NVALS(p->gpar) )
    {
	fprintf(stderr,"** gp_index (%d) > max grid_parent index (%d)\n",
		sopt->gp_index, DSET_NVALS(p->gpar) - 1); 
	RETURN(2);
    }

    if ( p->nsurf < 1 || p->nsurf > 2 )  /* see V2S_MAX_SURFS */
    {
	fprintf(stderr,"** invalid: nsurf = %d must be in [%d,%d]\n",
		p->nsurf, 1, 2);
	RETURN(2);
    }

    /* validate individual SUMA_surface structs */
    for ( c = 0; c < p->nsurf; c++ )
	if ( check_SUMA_surface(p->surf + c) )
	    RETURN(3);

    /* now look for consistency */
    if ( p->nsurf > 1 )
    {
	if ( p->surf[0].num_ixyz != p->surf[1].num_ixyz )
	{
	    fprintf(stderr,"** invalid surfaces, different # nodes (%d,%d)\n",
		    p->surf[0].num_ixyz, p->surf[1].num_ixyz);
	    RETURN(4);
	}
    }
    else if ( sopt->use_norms && !p->surf[0].norm )
    {
	fprintf(stderr,"** missing surface normals for surface '%s'\n",
		CHECK_EMPTY_STR(p->surf[0].label));
	RETURN(4);
    }

    RETURN(0);
}

/* return 0 if valid, > 0 otherwise */
static int check_SUMA_surface( SUMA_surface * s )
{
    int rv = 0;
ENTRY("is_valid_SUMA_surface");

    if ( !s ) { fprintf(stderr,"** ivSs: no surface\n");  RETURN(0); }

    if ( s->type != SUMA_SURFACE_TYPE )
    {
	fprintf(stderr,"** surf '%s': invalid type %d\n",
		CHECK_EMPTY_STR(s->label), s->type);
	rv++;
    }

    if ( s->num_ixyz < 0 || s->nall_ixyz < 0 || s->num_ixyz < s->nall_ixyz )
    {
	fprintf(stderr,"** surf '%s': invalid num_ixyz = %d, nall_ixyz = %d\n",
		CHECK_EMPTY_STR(s->label), s->num_ixyz, s->nall_ixyz);
	rv++;
    }

    if ( s->seq == 0 || s->sorted == 0 || s->seqbase != 0 )
    {
	fprintf(stderr,"** surf '%s': invalid seq, sort or base (%d, %d, %d)\n",
		CHECK_EMPTY_STR(s->label), s->seq, s->sorted, s->seqbase);
	rv++;
    }

    if ( !s->ixyz )
    {
	fprintf(stderr,"** surf '%s': invalid, missing nodes\n",
		CHECK_EMPTY_STR(s->label));
	rv++;
    }

    RETURN(rv);
}


/*---------------------------------------------------------------------------
 * v2s_vals_over_steps    - return whether a function is displayed over steps
 *
 * Most function results are output per sub-brick.  These functions will
 * have results displayed over the segment steps.
 *---------------------------------------------------------------------------
*/
int v2s_vals_over_steps( int map )
{
    if ( map == E_SMAP_SEG_VALS )
        return 1;

    return 0;
}

                                                                                
/*----------------------------------------------------------------------
 * v2s_map_type - return an E_SMAP_XXX code
 *
 * on failure, return -1 (E_SMAP_INVALID)
 * else        return >0 (a valid map code)
 *----------------------------------------------------------------------
*/
int v2s_map_type ( char * map_str )
{
    v2s_map_nums map;
                                                                                
ENTRY("v2s_map_type");
                                                                                
    if ( map_str == NULL )
    {
        fprintf( stderr, "** v2s_map_type: missing map_str parameter\n" );
        RETURN((int)E_SMAP_INVALID);
    }
                                                                                
    if ( sizeof(gv2s_map_names) / sizeof(char *) != (int)E_SMAP_FINAL )
    {
        fprintf( stderr, "** error:  gv2s_map_names/v2s_map_num mis-match\n");
        RETURN((int)E_SMAP_INVALID);
    }
                                                                                
    for ( map = E_SMAP_INVALID; map < E_SMAP_FINAL; map++ )
        if ( !strcmp( map_str, gv2s_map_names[map] ) )
            RETURN((int)map);
                                                                                
    RETURN((int)E_SMAP_INVALID);
}

