
#define VERSION "version 0.3 (November 14, 2003)"

/*----------------------------------------------------------------------
 * SurfMeasures - compute measures from the surface dataset(s)
 *
 * This program takes as input one or two surfaces (as part of a
 * spec file), and outputs a 1D format surface dataset, containing
 * requested measures from the input surfaces.
 *
 * Valid measures (to be supplied via the -func option) are:
 *
 *     ang_norms	: angle between the 2 surface normals
 *     ang_ns_A		: angle between the segment and first normal
 *     ang_ns_B		: angle between the segment and second normal
 *     coord_A		: first xyz coordinate
 *     coord_B		: second xyz coordinate
 *     n_area_A		: area for each node on the first surface
 *     n_area_B		: area for each node on the second surface
 *     nodes            : node index
 *     node_vol		: between surface volume per node
 *     thick            : thickness - length of node segment
 *
 * See "SurfMeasures -help" for more information.
 *
 * Author: R. Reynolds
 *----------------------------------------------------------------------
*/


/*----------------------------------------------------------------------
 * history
 *
 * 0.3 November 14, 2003
 *   - wokring on 'usage' for initial release
 *----------------------------------------------------------------------
*/


/*----------------------------------------------------------------------
 * todo:
 *
 * - full usage
 * - add '-node_mask' option?  -cmask?
 * - '-no_nodes' option?  remove "nodes" as default?
 *----------------------------------------------------------------------
*/

#include "mrilib.h"
#include "SUMA_suma.h"
#include "SUMA_SurfMeasures.h"

extern void machdep( void );


/* globals */
SUMA_SurfaceViewer * SUMAg_SVv = NULL;  /* array of Surf View structs   */
int                  SUMAg_N_SVv = 0;   /* length of SVv array          */
SUMA_DO            * SUMAg_DOv = NULL;  /* array of Displayable Objects */
int                  SUMAg_N_DOv = 0;   /* length of DOv array          */
SUMA_CommonFields  * SUMAg_CF = NULL;   /* info common to all viewers   */

/* this must match smeasure_codes_e enum */
char * g_sm_names[] = { "none", "ang_norms", "ang_ns_A", "ang_ns_B",
			"coord_A", "coord_B", "n_area_A", "n_area_B", "nodes",
			"node_vol", "thick" };

/*----------------------------------------------------------------------*/

int main ( int argc, char * argv[] )
{
    param_t p;
    opts_t  opts;
    int     rv;

    mainENTRY("SurfMeasures main");
    machdep();
    AFNI_logger(PROG_NAME,argc,argv);

    if ( (rv = init_options(&opts, argc, argv)) != 0 )
	return rv;

    if ( (rv = validate_options(&opts, &p)) != 0 )
	return rv;

    if ( (rv = get_surf_data(&opts, &p)) != 0 )
	return rv;

    if ( (rv = write_output(&opts, &p)) != 0 )
	return rv;

    final_cleanup(&opts, &p);

    return 0;
}


/*----------------------------------------------------------------------
 * write_output
 *
 *----------------------------------------------------------------------
*/
int write_output( opts_t * opts, param_t * p )
{
    THD_fvec3   p0, p1;
    double      vol0 = -1.0, vol1 = -1.0, a0 = -1.0, a1 = -1.0;
    double      tvol0, tvol1, tarea0, tarea1, r;
    double      base, dist, min_dist, max_dist, tdist = -1.0;
    double      atn = 0.0, atna = 0.0, atnb = 0.0;  /* angle totals */
    float       fvn, fva, fvb;
    float     * fp0, * fp1;
    float     * norms0, * norms1;
    int       * fcodes;
    int         c, vflag, fnum, node;

ENTRY("write_output");

    /* just a simple case for now */

    print_column_headers(opts, p);

    /* initialize some pointers */
    fp0    = p->S.slist[0]->NodeList;
    norms0 = p->S.slist[0]->NodeNormList;

    /* if we have only one surface, just init fp1 to that */
    if ( p->S.nsurf > 0 )
    {
	fp1    = p->S.slist[1]->NodeList;
	norms1 = p->S.slist[1]->NodeNormList;
    }
    else
    {
	fp1    = fp0;
	norms1 = norms0;
    }

    fcodes = p->F->codes;	/* for convenience */

    tvol0  = 0.0;		/* for total volume computation */
    tvol1  = 0.0;		/* for total volume computation */
    tarea0 = 0.0;		/* for total area computation */
    tarea1 = 0.0;		/* for total area computation */
    vflag  = (p->S.narea[0] != NULL) && (p->S.narea[1] != NULL);

    min_dist = 9999.0;
    max_dist = 0.0;
    for (node = 0; node < p->S.nnodes; node++)
    {
	p0.xyz[0] = fp0[0];  p0.xyz[1] = fp0[1];  p0.xyz[2] = fp0[2];
	p1.xyz[0] = fp1[0];  p1.xyz[1] = fp1[1];  p1.xyz[2] = fp1[2];

	dist = dist_f3mm(&p0, &p1);
	if ( dist < min_dist ) min_dist = dist;
	if ( dist > max_dist ) max_dist = dist;

	/* do we compute the volume? */
	if ( vflag )
	{
	    a0      = p->S.narea[0][node];
	    a1      = p->S.narea[1][node];
	    base    = a0 > a1 ? a0 : a1;
	    vol0    = 0.5 * (a0 + a1) * dist;

	    r = (a0 == 0 || a1 == 0) ? 0 : ((a0 < a1) ? a0/a1 : a1/a0);
	    r = sqrt(r);
	    vol1 = base * dist * (1 + r + r*r) / 3.0;

	    /* keep track of everything for comparison */
	    tvol0  += vol0;
	    tvol1  += vol1;
	    tarea0 += a0;
	    tarea1 += a1;
	    tdist  += dist;
	}

	fputc(' ', p->outfp);

	for (fnum = 0; fnum < p->F->nused; fnum++)
	{
	    switch(fcodes[fnum])
	    {
		default:
		    fprintf(stderr,"** bad output Info code %d\n",fcodes[fnum]);
		    break;

		case E_SM_ANG_NORMS:
		    fvn = vector_angle(norms0 + 3*node, norms1 + 3*node);
		    fprintf(p->outfp,"  %10s", MV_format_fval(fvn));
		    atn += fvn;
		    break;

		case E_SM_ANG_NS_A:
		    fva = norm2seg_angle(&p0, &p1, norms0 + 3*node);
		    fprintf(p->outfp,"  %10s", MV_format_fval(fva));
		    atna += fva;
		    break;

		case E_SM_ANG_NS_B:
		    fvb = norm2seg_angle(&p0, &p1, norms1 + 3*node);
		    fprintf(p->outfp,"  %10s", MV_format_fval(fvb));
		    atnb += fvb;
		    break;

		case E_SM_N_AREA_A:
		    fprintf(p->outfp,"  %10s",
			    MV_format_fval(p->S.narea[0][node]));
		    break;

		case E_SM_N_AREA_B:
		    fprintf(p->outfp,"  %10s",
			    MV_format_fval(p->S.narea[1][node]));
		    break;

		case E_SM_NODES:
		    fprintf(p->outfp,"  %10d", node);
		    break;

		case E_SM_NODE_VOL:
		    fprintf(p->outfp,"  %10s", MV_format_fval(vol0));
		    break;

		case E_SM_THICK:
		    fprintf(p->outfp,"  %10s", MV_format_fval(dist));
		    break;

		case E_SM_COORD_A:
		    for (c = 0; c < 3; c++)
			fprintf(p->outfp,"  %10s", MV_format_fval(fp0[c]));
		    break;

		case E_SM_COORD_B:
		    for (c = 0; c < 3; c++)
			fprintf(p->outfp,"  %10s", MV_format_fval(fp1[c]));
		    break;
	    }
	}

	fputc('\n', p->outfp);

	if ( node == opts->dnode && opts->debug > 0 )
	{
	    fprintf(stderr,"-- dnode %d, dist = %s\n",
		    node, MV_format_fval(dist_f3mm(&p0,&p1)) );
	    if ( vflag )
	    {
		fprintf(stderr,"-- vol0 = %s", MV_format_fval(vol0));
		fprintf(stderr,", vol1 = %s",  MV_format_fval(vol1));
		fprintf(stderr,", a0 = %s",    MV_format_fval(a0));
		fprintf(stderr,", a1 = %s\n",  MV_format_fval(a1));
	    }

	    disp_f3_point("-- p0    = ", fp0);
	    disp_f3_point("-- p1    = ", fp1);

	    if ( atn > 0.0 || atna > 0.0 || atnb > 0.0 )
	    {
		disp_f3_point("-- normA = ", norms0 + 3*node);
		disp_f3_point("-- normB = ", norms1 + 3*node);
	    }
	}

	fp0 += 3;
	fp1 += 3;
    }

    printf("------------------------------------------------------------\n");
    if ( vflag )
    {
	printf("-- total volume = %.1f\n", tvol0);
	if ( opts->total_vol )
	{
	printf("-- icone volume = %.1f, aarea*adist = %.1f\n",
		   tvol1, (tarea0+tarea1)*tdist/(2*p->S.nnodes));
	    printf("-- area0 = %.1f, area1 = %.1f, ave dist = %.3f\n",
		    tarea0, tarea1, tdist/p->S.nnodes);
	}
    }

    printf("-- min surf dist = %.5f, max surf dist = %.5f\n",min_dist,max_dist);

    if ( atn > 0.0 || atna > 0.0 || atnb > 0.0 )
	printf("-- ave: ang_norms = %.4f, ang_ns_A = %.4f, ang_ns_B = %.4f\n",
		atn /p->S.nnodes, atna/p->S.nnodes, atnb/p->S.nnodes);

    RETURN(0);
}


/*----------------------------------------------------------------------
 * norm2seg_angle		- return the angle between segment and norm
 *----------------------------------------------------------------------
*/
float norm2seg_angle( THD_fvec3 * p0, THD_fvec3 * p1, float * norm )
{
    float seg[3];
    int   c;

ENTRY("norm2seg_angle");

    for ( c = 0; c < 3; c++ )
	seg[c] = p1->xyz[c] - p0->xyz[c];

    RETURN(vector_angle(seg, norm));
}


/*----------------------------------------------------------------------
 * fvec_magnitude			- compute magnitude of float vector
 *----------------------------------------------------------------------
*/
float fvec_magnitude( float * v, int length )
{
    double sums;
    int    c;

ENTRY("fvec_magnitude");

    sums = 0.0;
    for ( c = 0; c < length; c++ )
	sums += v[c] * v[c];

    RETURN(sqrt(sums));
}


/*----------------------------------------------------------------------
 * vector_angle		- return the angle between the vectors
 *
 * if either magnitude is 0, return 0
 * else, use dot product definition
 *----------------------------------------------------------------------
*/
float vector_angle( float * v0, float * v1 )
{
    double mag0, mag1, dot, ratio, angle;

ENTRY("vector_angle");

    mag0 = fvec_magnitude(v0, 3);
    mag1 = fvec_magnitude(v1, 3);
    dot  = v0[0]*v1[0] + v0[1]*v1[1] + v0[2]*v1[2];

    if ( mag0 == 0.0 || mag1 == 0.0 )
	RETURN(0.0);

    ratio = dot / (mag0 * mag1);
    RANGE(-1.0, ratio, 1.0);

    angle = acos(ratio);

    /* keep angle in [0,PI/2] */
    if ( 2 * angle > ST_PI )
	angle = PI - angle;

    RETURN(angle);
}


/*----------------------------------------------------------------------
 * print_column_headers
 *----------------------------------------------------------------------
*/
int print_column_headers( opts_t * opts, param_t * p )
{
    int c, c2, num2print;

ENTRY("print_column_headers");

    fputc('#', p->outfp);
    for (c = 0; c < p->F->nused; c++ )
    {
	if ( (p->F->codes[c] == E_SM_COORD_A) ||
	     (p->F->codes[c] == E_SM_COORD_B) )
	    num2print = 3;
	else
	    num2print = 1;

	for (c2 = 0; c2 < num2print; c2++)
	    if ( num2print > 1 )
		fprintf(p->outfp, " %8s[%d]", p->F->names[c],c2);
	    else
		fprintf(p->outfp, "  %10s", p->F->names[c]);
    }
    fputc('\n', p->outfp);

    fputc('#', p->outfp);
    for (c = 0; c < p->F->nused; c++ )
    {
	if ( (p->F->codes[c] == E_SM_COORD_A) ||
	     (p->F->codes[c] == E_SM_COORD_B) )
	    num2print = 3;
	else
	    num2print = 1;

	for (c2 = 0; c2 < num2print; c2++)
	    fprintf(p->outfp, "  ----------");
    }
    fputc('\n', p->outfp);

    RETURN(0);
}


/*----------------------------------------------------------------------
 * verify_surf_t
 *
 *----------------------------------------------------------------------
*/
int verify_surf_t( opts_t * opts, param_t * p )
{
    int c, rv;

ENTRY("verify_surf_t");

    if ( p->S.nsurf <= 0 )
    {
	fprintf(stderr,"** no surfaces found\n");
	if ( opts->debug > 0 )
	    disp_surf_t("-- empty? ", &p->S);
	RETURN(-1);
    }

    if ( p->S.nsurf > 1 )
    {
	for ( c = 1; c < p->S.nsurf; c++ )
	    if ( p->S.slist[c]->N_Node != p->S.nnodes )
	    {
		fprintf(stderr,"** surface %d has %d nodes (should be %d)\n",
			c, p->S.slist[c]->N_Node, p->S.nnodes);
		RETURN(-1);
	    }
    }

    if ( opts->debug > 1 )
	disp_surf_t("-- surf params verified: ", &p->S);

    if ( validate_option_lists(opts, p) != 0 )
	RETURN(-1);

    RETURN(0);
}


/*----------------------------------------------------------------------
 * get_surf_data
 *
 *----------------------------------------------------------------------
*/
int get_surf_data( opts_t * opts, param_t * p )
{
    int rv;

ENTRY("get_surf_data");

    rv = spec2SUMA(&p->S.spec, opts->spec_file, opts->sv_file, opts->debug);
    if ( rv != 0 )
	RETURN(rv);

    if ( (rv = all_mappable_surfs(opts, p)) != 0 )
	RETURN(rv);

    if ( (rv = verify_surf_t(opts, p)) != 0)
	RETURN(rv);

    if ( (rv = get_surf_measures(opts, p)) != 0)
	RETURN(rv);

    RETURN(0);
}


/*----------------------------------------------------------------------
 * get_surf_measures
 *----------------------------------------------------------------------
*/
int get_surf_measures( opts_t * opts, param_t * p )
{
    int * fcodes;
    int   c, debug;
    int   geta, getb;

ENTRY("get_surf_measures");

    geta = getb = 0;
    debug = opts->debug > 2;

    if ( opts->total_vol )
    {
	geta = getb = 1;
    }
    else		/* maybe there are functions that need them */
    {
	fcodes = p->F->codes;
	for ( c = 0; c < p->F->nused; c++ )
	{
	    if ( fcodes[c] == E_SM_N_AREA_A )
		geta = 1;
	    else if ( fcodes[c] == E_SM_N_AREA_B )
		getb = 1;
	    else if ( fcodes[c] == E_SM_NODE_VOL )
	    {
		geta = getb = 1;
	    }
	}
    }

    if ( geta ) 
    {
	if ( !SUMA_SurfaceMetrics_eng(p->S.slist[0], "PolyArea", NULL, debug) )
	{
	    fprintf(stderr,"** gsf: surface metrics A failure\n");
	    RETURN(-1);
	}

	if ( compute_node_areas(opts, p, 0) != 0 )	/* index 0 */
	    RETURN(-1);
    }

    if ( getb )
    {
	if ( !SUMA_SurfaceMetrics_eng(p->S.slist[1], "PolyArea", NULL, debug) )
	{
	    fprintf(stderr,"** gsf: surface metrics A failure\n");
	    RETURN(-1);
	}

	if ( compute_node_areas(opts, p, 1) != 0 )  /* index 1 */
	    RETURN(-1);
    }

    RETURN(0);
}


/*----------------------------------------------------------------------
 * compute_node_areas			- surface area for each node
 *
 * The area of a node is defined as one third of the area of the
 * associated triangles.
 *----------------------------------------------------------------------
*/
int compute_node_areas( opts_t * opts, param_t * p, int sindex )
{
    SUMA_SurfaceObject    * so;
    double                  sum;
    float                 * alist;
    int                   * flist;
    int                     node, c;

ENTRY("compute_node_areas");

    if ( sindex < 0 || sindex >= p->S.nsurf || sindex >= 2 )
    {
	fprintf(stderr,"** cna: surf index <%d> is out of range\n",sindex);
	RETURN(-1);
    }

    so = p->S.slist[sindex];		/* just for ease of typing */

    if ( ! so->PolyArea )
    {
	fprintf(stderr,"** cna: no PolyArea to compute from\n");
	RETURN(-1);
    }

    alist = (float *)malloc(p->S.nnodes*sizeof(float));
    ALLOC_CHECK(alist, "float", p->S.nnodes);
    p->S.narea[sindex] = alist;

    for ( node = 0; node < p->S.nnodes; node++ )
    {
	flist = so->MF->NodeMemberOfFaceSet[node];
	sum = 0.0;
	for (c = 0; c < so->MF->N_Memb[node]; c++)
	{
	    if ( flist[c] < 0 || flist[c] >= so->N_FaceSet )
	    {
		fprintf(stderr,"** cna: FaceSet mis-match flist,max = %d,%d\n",
			flist[c], so->N_FaceSet);
		free(p->S.narea[sindex]);
		RETURN(-1);
	    }

	    sum += so->PolyArea[flist[c]];
	}

	alist[node] = sum/3.0;

	flist += so->MF->N_Memb_max;

	if ( node == opts->dnode )
	    fprintf(stderr, "-- dnode %d: area = %s (%d faces, surf %s)\n",
		    node, MV_format_fval(alist[node]),
		    so->MF->N_Memb[node], CHECK_NULL_STR(so->Label));
    }

    RETURN(0);
}


/*----------------------------------------------------------------------
 * all_mappable_surfs		- fill surf_t struct, S
 *
 *----------------------------------------------------------------------
*/
int all_mappable_surfs( opts_t * opts, param_t * p )
{
    SUMA_SurfaceObject * so;
    int                  count;

ENTRY("all_mappable_surfs");

    p->S.slist = (SUMA_SurfaceObject **)malloc(SUMAg_N_DOv *
	                                       sizeof(SUMA_SurfaceObject *));
    ALLOC_CHECK(p->S.slist,"SUMA_SurfaceObject *",SUMAg_N_DOv);

    p->S.salloc = SUMAg_N_DOv;		/* number allocated for              */
    p->S.nsurf  = 0;			/* number of mappable surfaces found */

    for ( count = 0; count < SUMAg_N_DOv; count++ )
    {
	if ( ! SUMA_isSO(SUMAg_DOv[count]) )
	    continue;

	so = (SUMA_SurfaceObject *)SUMAg_DOv[count].OP;

	if ( ! SUMA_isINHmappable(so) )
	{
	    if ( opts->debug )
		fprintf(stderr,"** warning: surface '%s' is not mappable, "
		        "skipping...\n", so->Label ? so->Label : "<unnamed>");
	    continue;
	}

	if ( opts->debug > 1 )
	{
	    fprintf(stderr,"-------- surface #%d (%s) --------\n",
		    p->S.nsurf, so->Label ? so->Label : "<unnamed>");
	    if ( opts->debug > 3 )
		SUMA_Print_Surface_Object(so, stderr);
	}

	p->S.slist[p->S.nsurf] = so;
	p->S.nsurf++;
    }

    if ( p->S.nsurf > 0 )
	p->S.nnodes = p->S.slist[0]->N_Node;

    if ( opts->debug )
	fprintf(stderr, "++ found %d mappable surfaces\n", p->S.nsurf);

    RETURN(0);
}


/*----------------------------------------------------------------------
 * spec2SUMA		- call the SUMA functions for reading surfaces
 *
 *----------------------------------------------------------------------
*/
int spec2SUMA( SUMA_SurfSpecFile * spec, char * spec_file,
	       char * sv_file, int debug )
{

ENTRY("spec2SUMA");

    /* initialize common fields struct */
    SUMAg_CF = SUMA_Create_CommonFields();

    if ( SUMAg_CF == NULL )
    {
        fprintf( stderr, "** failed SUMA_Create_CommonFields(), exiting...\n" );
	RETURN(-1);
    }

    /* for SUMA type notifications */
    if ( debug > 3 )
    {
	SUMAg_CF->MemTrace = 1;

	if ( debug > 4 )
	    SUMAg_CF->InOut_Notify = 1;
    }

    SUMAg_DOv = SUMA_Alloc_DisplayObject_Struct(SUMA_MAX_DISPLAYABLE_OBJECTS);

    if ( SUMA_Read_SpecFile( spec_file, spec) == 0 )
    {
	fprintf( stderr, "** failed SUMA_Read_SpecFile(), exiting...\n" );
	RETURN(-1);
    }

    /* make sure only group was read from spec file */
    if ( spec->N_Groups != 1 )
    {
	fprintf( stderr,"** error: N_Groups <%d> must be 1 in spec file <%s>\n",
		 spec->N_Groups, spec_file );
	RETURN(-1);
    }

    /* actually load the surface(s) from the spec file */
    if (SUMA_LoadSpec_eng(spec, SUMAg_DOv, &SUMAg_N_DOv, sv_file, debug>2) == 0)
    {
	fprintf( stderr, "** error: failed SUMA_LoadSpec(), exiting...\n" );
	RETURN(-1);
    }

    if ( debug > 0 )
	fputs( "++ surfaces loaded.\n", stderr );

    RETURN(0);
}


/*----------------------------------------------------------------------
 * add_to_flist			- add function name and code to list
 *----------------------------------------------------------------------
*/
int add_to_flist( func_t * F, char * fname )
{
ENTRY("add_to_flist");

    if ( F->nused >= F->nalloc )
    {
	F->nalloc += ST_DEFAULT_FALLOC;
	F->names   = (char **)realloc(F->names, F->nalloc*sizeof(char *));
	F->codes   = (int   *)realloc(F->codes, F->nalloc*sizeof(int   ));

	if ( !F->names || !F->codes )
	{
	    fprintf(stderr,"** failed to allocate for %d functs (%p,%p)\n",
		    F->nalloc, F->names, F->codes);
	    RETURN(-1);
	}
    }

    F->names[F->nused] = fname;
    F->codes[F->nused] = check_func_name(fname);

    if ( F->codes[F->nused] == E_SM_INVALID )
    {
	fprintf(stderr,"** function '%s' is not valid\n",
		CHECK_NULL_STR(fname));
	RETURN(-1);
    }

    F->nused++;			/* we've got another good one */

    RETURN(0);
}


/*----------------------------------------------------------------------
 * init_opts_t			- initialize the struct
 *----------------------------------------------------------------------
*/
int init_opts_t( opts_t * opts )
{
ENTRY("init_options");

    memset(opts, 0, sizeof(opts_t));

    /* init the function list func_t struct */
    opts->F.names  = NULL;
    opts->F.codes  = NULL;
    opts->F.nalloc = 0;
    opts->F.nused  = 0;

    /* just to try this out, init the info list */
    if ( add_to_flist(&opts->F, g_sm_names[E_SM_NODES]) != 0 )
    {
	fprintf(stderr,"** failed to init func_t list with '%s'\n",
		g_sm_names[E_SM_NODES]);
	RETURN(-1);
    }

    opts->spec_file   = NULL;
    opts->sv_file     = NULL;
    opts->out_1D_file = NULL;
    opts->dnode       = -1;		/* init to something invalid */

    RETURN(0);
}


/*----------------------------------------------------------------------*/
/* this macro is specifically for init_options(), below                 */

#define CHECK_ARG_COUNT(ac,str)     \
	do {                        \
            if ((ac+1) >= argc) {   \
		fputs(str,stderr);  \
		RETURN(-1);         \
	    }                       \
	} while (0)

/*----------------------------------------------------------------------
 * init_options
 *----------------------------------------------------------------------
*/
int init_options( opts_t * opts, int argc, char * argv[] )
{
    int ac;

ENTRY("init_options");

    if ( argc < 2 )
	RETURN( usage(PROG_NAME, ST_USE_LONG) );

    /* init the structure to empty */
    if ( init_opts_t(opts) != 0 )
	RETURN(-1);

    for ( ac = 1; ac < argc; ac++ )
    {
	/* check for help first, the rest alphabetically */
	if ( ! strncmp(argv[ac], "-help", 2) )
	    RETURN(usage(PROG_NAME, ST_USE_LONG));
	else if ( ! strncmp(argv[ac], "-debug", 6) )
	{
	    CHECK_ARG_COUNT(ac,"option usage: -debug LEVEL\n");

	    opts->debug = atoi(argv[++ac]);
	    if ( opts->debug < 0 || opts->debug > ST_DEBUG_MAX_LEVEL )
	    {
		fprintf(stderr,"** bad debug level %d, should be in [0,%d]\n",
			opts->debug, ST_DEBUG_MAX_LEVEL);
		RETURN(-1);
	    }
	}
	else if ( ! strncmp(argv[ac], "-dnode", 6) )
	{
	    CHECK_ARG_COUNT(ac,"option usage: -dnode NODE_NUM\n");
	    opts->dnode = atoi(argv[++ac]);
	}
	else if ( ! strncmp(argv[ac], "-func", 5) )
	{
	    CHECK_ARG_COUNT(ac,"option usage: -func FUNCTION\n");
	    ++ac;
	    if ( add_to_flist(&opts->F, argv[ac]) != 0 )
		RETURN(-1);
	}
	else if ( ! strncmp(argv[ac], "-out_1D", 7) )
	{
	    CHECK_ARG_COUNT(ac,"option usage: -out_1D OUTPUT_FILE\n");
	    opts->out_1D_file = argv[++ac];
	}
	else if ( ! strncmp(argv[ac], "-spec", 5) )
	{
	    CHECK_ARG_COUNT(ac,"option usage: -spec SPEC_FILE\n");
	    opts->spec_file = argv[++ac];
	}
	else if ( ! strncmp(argv[ac], "-sv", 3) )
	{
	    CHECK_ARG_COUNT(ac,"option usage: -sv SURF_VOLUME\n");
	    opts->sv_file = argv[++ac];
	}
	else if ( ! strncmp(argv[ac], "-total_vol",10) )
	{
	    opts->total_vol = 1;
	}
	else if ( ! strncmp(argv[ac], "-ver",4) )
	{
	    RETURN( usage(PROG_NAME, ST_USE_VERSION) );
	}
	else
	{
	    fprintf(stderr,"invalid option <%s>\n",argv[ac]);
	    RETURN( usage(PROG_NAME, ST_USE_SHORT) );
	}
    }

    RETURN(0);
}


/*----------------------------------------------------------------------
 * validate_options
 *----------------------------------------------------------------------
*/
int validate_options( opts_t * opts, param_t * p )
{
    int errs = 0;

ENTRY("validate_options");    

    if ( !opts || !p )
    {
	fprintf(stderr,"** vo: bad params (%p,%p)\n", opts, p);
	RETURN(-1);
    }

    if ( opts->F.nused <= 0 )
    {
	fprintf(stderr,"** must specify at least one '-func' option\n");
	errs++;
    }

    if ( ! opts->spec_file )
    {
	fprintf(stderr,"** missing argument: -spec\n");
	errs++;
    }

    /* we don't necessarily need an sv_file ... do not check */

    /* verify output file, and open for writing */
    if ( ! opts->out_1D_file )
    {
	fprintf(stderr,"** missing argument: -out_1D\n");
	errs++;
    }
    else if ( THD_is_file(opts->out_1D_file) )
    {
	fprintf(stderr,"** output file already exists: %s\n",opts->out_1D_file);
	errs++;
    }

    if ( errs > 0 )
	RETURN(-1);

    if ( opts->debug > 1 )
    {
	disp_opts_t( "-- opts okay: ", opts );
	disp_func_t( "-- opts okay: ", &opts->F );
    }


    /* options look good, now fill the param_t struct */
    memset(p, 0, sizeof(param_t));	/* clear params    */

    p->S.slist    = NULL;			/* to be safe...   */
    p->S.narea[0] = NULL;
    p->S.narea[1] = NULL;

    p->F          = &opts->F;			/* point to struct */

    if ( (p->outfp = fopen(opts->out_1D_file, "w")) == NULL )
    {
	fprintf(stderr,"** cannot open output file '%s'\n",opts->out_1D_file);
	RETURN(-1);
    }

    RETURN(0);
}


/*----------------------------------------------------------------------
 * validate_option_lists		- check function and info entries
 *----------------------------------------------------------------------
*/
int validate_option_lists( opts_t * opts, param_t * p )
{
    int * fcodes, c, errs;

ENTRY("validate_option_lists");

    errs = 0;

    if ( opts->total_vol && p->S.nsurf < 2 )
    {
	fprintf(stderr,"** -total_vol option requiers 2 surfaces\n");
	errs++;
    }

    /* verify all the functions in our list */
    fcodes = p->F->codes;	/* just for less typing */
    for ( c = 0; c < p->F->nused; c++ )
    {
	switch (fcodes[c])
	{
	    default:
		fprintf(stderr, "** vol: invalid function code %d\n",fcodes[c]);
		errs++;
		break;

	    case E_SM_COORD_A:
	    case E_SM_N_AREA_A:
	    case E_SM_NODES:
		break;

	    case E_SM_ANG_NORMS:
	    case E_SM_ANG_NS_A:
	    case E_SM_ANG_NS_B:
		if ( !p->S.slist[0]->NodeNormList ||
		     !p->S.slist[1]->NodeNormList )
		{
		    fprintf(stderr,"** missing node normals for func '%s'\n",
			    g_sm_names[fcodes[c]]);
		    errs++;
		}

		/* and continue for the nsurf check... */
		
	    case E_SM_COORD_B:
	    case E_SM_N_AREA_B:
	    case E_SM_NODE_VOL:
	    case E_SM_THICK:
		if ( p->S.nsurf < 2 )
		{
		    fprintf(stderr,"** func '%s' requires 2 surfaces\n",
			    g_sm_names[fcodes[c]]);
		    errs++;
		}
		break;
	}

	if ( opts->debug > 1 && errs == 0 )
	    fprintf(stderr,"++ have valid function name '%s'\n",
		    g_sm_names[fcodes[c]]);
    }

    if ( errs > 0 )
	RETURN(-1);

    if ( opts->debug > 0 )
	fprintf(stderr,"-- found %d output functions\n", p->F->nused);

    RETURN(0);
}



/*----------------------------------------------------------------------
 * usage              - provide info, depending on the type
 *
 * ST_USE_SHORT
 * ST_USE_LONG
 * ST_USE_VERSION
 *
 * return -1 to signal this as a terminal function
 *----------------------------------------------------------------------
*/
int usage( char * prog, int use_type )
{

ENTRY("usage");

    if ( use_type == ST_USE_SHORT )
    {
	fprintf(stderr,"usage: %s [options] -spec SPEC_FILE -func FUNC_NAME\\\n"
	               "                    -out_1D OUTFILE\n", prog);
    }
    else if ( use_type == ST_USE_LONG )
    {
    }
    else if ( use_type == ST_USE_VERSION )
    {
	printf("%s: %s, compile date: %s\n", prog, VERSION, __DATE__);
    }
    else
	fprintf(stderr,"** error: usage - invalid use_type %d\n", use_type); 

    RETURN(-1);
}


/*----------------------------------------------------------------------
 * check_func_name			- return function code for name
 *----------------------------------------------------------------------
*/
int check_func_name( char * func )
{
    int fnum;

ENTRY("check_func_name");

    if ( !func )
	RETURN(E_SM_INVALID);

    /* just to be safe, let's verify the names and enum */

    if ( sizeof(g_sm_names)/sizeof(char *) != (int)E_SM_FINAL )
    {
	fprintf(stderr,"** error: g_sm_names mis-match\n");
	RETURN(E_SM_INVALID);
    }

    for ( fnum = E_SM_INVALID; fnum < E_SM_FINAL; fnum++ )
	if ( !strcmp(func, g_sm_names[fnum]) )
	    RETURN(fnum);

    RETURN(E_SM_INVALID);
}


/*----------------------------------------------------------------------
 * final_cleanup			- free memory
 *----------------------------------------------------------------------
*/
int final_cleanup( opts_t * opts, param_t * p )
{

ENTRY("final_cleanup");

    if ( p->outfp != stdout )
	fclose(p->outfp);

    if ( p->F->nalloc > 0 )
    {
	free(p->F->names);
	free(p->F->codes);
    }

    if ( p->S.narea[0] )  free(p->S.narea[0]);
    if ( p->S.narea[1] )  free(p->S.narea[1]);

    /* rcr - check for the mallocs (like function stuff) */

    if ( opts->debug > 2 )
	fprintf(stderr,"-- freeing SUMA data...\n");

    if ( ( SUMAg_DOv != NULL ) &&
	 ( SUMA_Free_Displayable_Object_Vect(SUMAg_DOv, SUMAg_N_DOv) == 0 ) )
	fprintf(stderr, "** failed SUMA_Free_Displayable_Object_Vect()\n" );

    if ( ( SUMAg_SVv != NULL ) &&
	 ( SUMA_Free_SurfaceViewer_Struct_Vect(SUMAg_SVv, SUMAg_N_SVv) == 0 ) )
	fprintf( stderr, "** failed SUMA_Free_SurfaceViewer_Struct_Vect()\n" );

    if ( ( SUMAg_CF != NULL ) && ( SUMA_Free_CommonFields(SUMAg_CF) == 0 ) )
	fprintf( stderr, "** failed SUMA_Free_CommonFields()\n" );

    RETURN(0);
}


/*----------------------------------------------------------------------
 * dist_f3mm            - return Euclidean distance between the points
 *----------------------------------------------------------------------
*/
float dist_f3mm( THD_fvec3 * p1, THD_fvec3 * p2 )
{
    double d0, d1, d2;

    if ( p1 == NULL || p2 == NULL )
    {
	fprintf( stderr, "** dist_f3mm: invalid params (%p,%p)\n", p1, p2 );
	return 0.0;
    }

    d0 = p1->xyz[0] - p2->xyz[0];
    d1 = p1->xyz[1] - p2->xyz[1];
    d2 = p1->xyz[2] - p2->xyz[2];

    return sqrt(d0*d0 + d1*d1 + d2*d2);
}


/*----------------------------------------------------------------------
 * disp_surf_t
 *----------------------------------------------------------------------
*/
int disp_surf_t( char * info, surf_t * d )
{
    if ( info )
	fputs( info, stderr );

    if ( ! d )
    {
	fprintf(stderr,"** disp_surf_t: d == NULL\n");
	return -1;
    }

    fprintf(stderr,
	    "surf_t struct at %p:\n"
	    "    spec N_Surfs   = %d\n"
	    "    spec N_Groups  = %d\n"
	    "    slist, nsurf   = %p, %d\n"
	    "    salloc, nnodes = %d, %d\n",
	    d,
	    d->spec.N_Surfs, d->spec.N_Groups, d->slist,
	    d->nsurf, d->salloc, d->nnodes);

    return 0;
}


/*----------------------------------------------------------------------
 * disp_func_t
 *----------------------------------------------------------------------
*/
int disp_func_t( char * info, func_t * d )
{
    int c;

    if ( info )
	fputs( info, stderr );

    if ( ! d )
    {
	fprintf(stderr,"** disp_func_t: d == NULL\n");
	return -1;
    }

    fprintf(stderr,
	    "func_t struct at %p:\n"
	    "    names, codes   = %p, %p\n"
	    "    nalloc, nused  = %d, %d\n",
	    d,
	    d->names, d->codes, d->nalloc, d->nused);

    return 0;
}


/*----------------------------------------------------------------------
 * disp_opts_t
 *----------------------------------------------------------------------
*/
int disp_opts_t( char * info, opts_t * d )
{
    if ( info )
	fputs( info, stderr );

    if ( ! d )
    {
	fprintf(stderr,"** disp_opts_t: d == NULL\n");
	return -1;
    }

    fprintf(stderr,
	    "opts_t struct at %p:\n"
	    "    spec_file    = %s\n"
	    "    sv_file      = %s\n"
	    "    out_1D_file  = %s\n"
	    "    total_vol    = %d\n"
	    "    debug, dnode = %d, %d\n",
	    d,
	    CHECK_NULL_STR(d->spec_file),
	    CHECK_NULL_STR(d->sv_file),
	    CHECK_NULL_STR(d->out_1D_file),
	    d->total_vol, d->debug, d->dnode);

    return 0;
}


/*----------------------------------------------------------------------
 * disp_f3_point
 *----------------------------------------------------------------------
*/
int disp_f3_point( char * info, float * d )
{
    if ( info )
	fputs( info, stderr );

    if ( ! d )
    {
	fprintf(stderr,"** disp_f3_point: d == NULL\n");
	return -1;
    }

    fprintf(stderr,"(%6s, ", MV_format_fval(d[0]));
    fprintf(stderr,"%6s, ",  MV_format_fval(d[1]));
    fprintf(stderr,"%6s)\n", MV_format_fval(d[2]));

    return 0;
}

