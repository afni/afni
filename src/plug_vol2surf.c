/***********************************************************************
 * plug_vol2surf.c		- plugin interface to vol2surf computation
 *
 * Provide an interface to the global v2s_plugin_opts structure.
 ***********************************************************************
*/

#include "afni.h"
#include "vol2surf.h"

#ifndef ALLOW_PLUGINS
#  error "Plugins not properly set up -- see machdep.h"
#endif

static char * PV2S_main( PLUGIN_interface * );

static char g_help[] = 
    " The plugin provides an interface to the vol2surf options used by afni\n"
    " (for display of the overlay dataset values on the surfaces in suma).\n"
    " If the vol2surf options are not used, afni will default to the basic\n"
    " surface intersection technique.\n"
    " \n"
    " The minimum choices that should be made are those on the first two\n"
    " lines: 'function' and 'surfaces'.  To use this functionality, please\n"
    " start by answering 'use vol2surf?' with 'yes'.  :)\n"
    " \n"
    " ----------------------------- options --------------------------------\n"
    " function:\n"
    " \n"
    "   use vol2surf? : choose 'yes' to make the plugin options active\n"
    "   map func      : choose a function to apply to values along node pair\n"
    "                   segments, embedded in the AFNI volume\n"
    "   seg index     : apply all values along a each segment, or only those\n"
    "                   from unique voxels\n"
    "   num steps     : the number of evenly spaced points each segment will\n"
    "                   be divided into\n"
    " \n"
    " surfaces:\n"
    " \n"
    "   surf_A        : choose the afni surface index for surface A\n"
    "   use B?        : form segments by joining nodes from surface A with\n"
    "                   nodes on surface B (an alternate choice is to use\n"
    "                   only surface A, along with its normals)\n"
    "   surf_B        : choose the afni surface index for surface B\n"
    " \n"
    " normals:\n"
    " \n"
    "   use normals?  : segments are formed from each node from surface A\n"
    "                   along its normal, and of length 'norm len'\n"
    "   norm len      : this will be the length of each segment\n"
    "   norm dir?     : choose whether to keep the normal directions that are\n"
    "                   computed by SUMA, or to re-compute them, based on the\n"
    "                   surface limits and its center of mass\n"
    " \n"
    " offsets:\n"
    " \n"
    "   NOTE - segments are considered directed from the node on surface A\n"
    "          in the direction of surface B (or the normal), so if f_p1 is\n"
    "          positive each segment gets shorter (going toward surface B),\n"
    "          but if f_pn is positive each segment gets longer (still moving\n"
    "          in the direction from surface A to surface B)\n"
    " \n"
    "   f_p1_mm       : move each segment starting point this number of\n"
    "                   millimeters toward surface B\n"
    "   f_pn_mm       : move each segment ending point this number of\n"
    "                   millimeters farther from surface A\n"
    "   f_p1_fr       : move each segment starting point this fraction of the\n"
    "                   distance toward surface B\n"
    "   f_pn_fr       : move each segment ending point this fraction of the\n"
    "                   distance farther from surface A\n"
    " \n"
    " out of range:\n"
    " \n"
    "   oob nodes?    : whether to still output results for those nodes which\n"
    "                   are out-of-bounds (i.e. outside the bounding box of\n"
    "                   the AFNI overlay dataset)\n"
    "   oob value     : if out-of-bounds nodes are output, this value will be\n"
    "                   given to them\n"
    "   oom nodes?    : whether to still output results for those nodes which\n"
    "                   are out-of-mask (i.e. masked by the threshold brick)\n"
    "   oob value     : if out-of-mask nodes are output, this value will be\n"
    "                   given to them\n"
    " output:\n"
    " \n"
    "   first node    : starting index for nodes to output\n"
    "   last node     : ending index for nodes to output (0 means 'nodes-1')\n"
    "                   (sorry, that means you cannot output from 0 to 0)\n"
    "   outfile.1D    : also send the results to this file (in 1D format)\n"
    "   outfile.niml  : also send the results to this file (in niml format)\n"
    " \n"
    " debug level:\n"
    " \n"
    "   level         : provide this level of debug output (0-5)\n"
    "   node          : provide additional debug output for this node\n"
    " \n"
    " \n"
    " *  for more infomation, please try the command: '3dVol2Surf -help'   *\n"
    " ----------------------------------------------------------------------\n"
    " \n"
    " \n"
    " Author: R Reynolds\n"
    " \n"
    "   History:\n"
    " \n"
    "   1.0  9 September 2004 [rickr]\n"
    "     - initial version\n";

#define P_MAP_NAMES_NVALS      12	/* should match enum for global maps */
#define P_NY_NVALS              2
#define P_KEEP_NVALS            2
#define P_STEP_NVALS            2

static char * gp_ny_list[]   = { "no", "yes" };
static char * gp_keep_list[] = { "keep", "check" };
static char * gp_step_list[] = { "voxel", "node" };

typedef struct
{
    v2s_plugin_opts  * vpo;
    char             * hist;
    char            ** maps;
} pv2s_globals;

static pv2s_globals globs;

/* local functions */
static int check_surfaces(PLUGIN_interface * plint, int sa, int sb, char *mesg);
static int init_plugin_opts(pv2s_globals * g);
static int process_args(PLUGIN_interface * plint,pv2s_globals * g, char *mesg);

/* for ease of error reporting */
#define P_V2S_BAIL_VALUE(buf,str,val)                                   \
        do { sprintf((buf),  "-------------------------------------\n"  \
                             "%s\n"                                     \
                             "bad value = %d\n"                         \
                             "-------------------------------------",   \
                             (str), (val) ); } while (0)

DEFINE_PLUGIN_PROTOTYPE			/* for C++ compilation */

PLUGIN_interface * PLUGIN_init( int ncall )
{
    PLUGIN_interface * plint;
    void             * void_vpo;

ENTRY("vol2surf: PLUGIN_init");

    if ( ncall > 0 ) RETURN(NULL);	/* only one interface */

    /* might be temporary */
    if ( PLUTO_set_v2s_addrs(&void_vpo, &globs.maps, &globs.hist) )
    {
	fprintf(stderr,"** plug_v2s: failed to init globals\n");
	RETURN(NULL);
    }

    /* using a void pointer so we don't have to put vol2surf.h in afni.h */
    globs.vpo = (v2s_plugin_opts *)void_vpo;

    init_plugin_opts(&globs);

    plint = PLUTO_new_interface("Vol2Surf",
		"configure afni's volume to surface options",
		g_help, PLUGIN_CALL_VIA_MENU, PV2S_main);

    PLUTO_add_hint     (plint, "configure vol2surf options");
    PLUTO_set_sequence (plint, "A:afnicontrol:surf");
    PLUTO_set_runlabels(plint, "Set+Keep", "Set+Close");

    /* first input : do we use vol2surf? */
    PLUTO_add_option( plint, "function", "op_st", TRUE );
    PLUTO_add_hint  ( plint, "decide whether to use vol2surf" );
    PLUTO_add_string( plint, "use vol2surf? ", P_NY_NVALS, gp_ny_list, 0 );
    PLUTO_add_hint  ( plint, "use vol2surf (or basic intersection)" );
    PLUTO_add_string( plint, "map func",P_MAP_NAMES_NVALS,globs.maps,0);
    PLUTO_add_hint  ( plint, "choose a filter to apply to segment values" );
    PLUTO_add_string( plint, "seg index",P_STEP_NVALS,gp_step_list,0);
    PLUTO_add_hint  ( plint, "along segments, count only distinct voxels?");
    PLUTO_add_number( plint, "num steps", 0, 100, 0, 2, 1 );
    PLUTO_add_hint  ( plint, "number of steps to divide each segment into" );

    /* choose surface indices                                   */
    /*   - note that we do not yet have surfaces to choose from */
    PLUTO_add_option( plint, "surfaces", "surfaces", TRUE );
    PLUTO_add_hint  ( plint, "choose surface index(es)" );
    PLUTO_add_number( plint, "surf_A", 0, 50, 0, 0, 1 );
    PLUTO_add_hint  ( plint, "choose surface A index" );
    PLUTO_add_string( plint, "use B? ", P_NY_NVALS, gp_ny_list, 0 );
    PLUTO_add_hint  ( plint, "decide whether to use surface B" );
    PLUTO_add_number( plint, "surf_B", 0, 50, 0, 1, 1 );
    PLUTO_add_hint  ( plint, "choose surface B index" );

    /* menu for using normals */
    PLUTO_add_option( plint, "normals", "normals", FALSE );
    PLUTO_add_hint  ( plint, "control use of normals (instead of surf_B)" );
    PLUTO_add_string( plint, "use normals? ", P_NY_NVALS, gp_ny_list, 0 );
    PLUTO_add_hint  ( plint, "should normals be used to simulate surf_B?" );
    PLUTO_add_number( plint, "norm len", -100, 100, 1, 0, 1 );
    PLUTO_add_hint  ( plint, "what (signed) length should the normals be?" );
    PLUTO_add_string( plint, "norm dir?", P_KEEP_NVALS, gp_keep_list, 0 );
    PLUTO_add_hint  ( plint, "use SUMA normal direction (or re-compute)?" );

    /* segment offsets */
    PLUTO_add_option( plint, "offsets", "offsets", FALSE );
    PLUTO_add_hint  ( plint,"offset segment endpoints, directed from p1 to pn");
    PLUTO_add_number( plint, "f_p1_mm", -100, 100, 1, 0, 1 );
    PLUTO_add_hint  ( plint, "move p1 towards pn, in millimeters" );
    PLUTO_add_number( plint, "f_pn_mm", -100, 100, 1, 0, 1 );
    PLUTO_add_hint  ( plint, "move pn farther from p1, in millimeters" );
    PLUTO_add_number( plint, "f_p1_fr", -100, 100, 1, 0, 1 );
    PLUTO_add_hint  ( plint, "move p1 towards pn, by this segment fraction" );
    PLUTO_add_number( plint, "f_pn_fr", -100, 100, 1, 0, 1 );
    PLUTO_add_hint  ( plint, "move pn farther from p1, by this fraction" );

    /* out of bounds or mask */
    PLUTO_add_option( plint, "out of range", "oor", FALSE );
    PLUTO_add_hint  ( plint, "what to do when out of bounds or mask" );
    PLUTO_add_string( plint, "oob nodes?", P_NY_NVALS, gp_ny_list, 0 );
    PLUTO_add_hint  ( plint, "keep nodes that are outside the dataset?");
    PLUTO_add_number( plint, "oob value", -100, 100, 1, 0, 1 );
    PLUTO_add_hint  ( plint, "value to apply when out of dataset bounds" );
    PLUTO_add_string( plint, "oom nodes?", P_NY_NVALS, gp_ny_list, 0 );
    PLUTO_add_hint  ( plint, "keep nodes that are masked out?");
    PLUTO_add_number( plint, "oom value", -100, 100, 1, 0, 1 );
    PLUTO_add_hint  ( plint, "value for masked out nodes" );

    /* choose node processing range */
    PLUTO_add_option( plint, "output", "output", FALSE );
    PLUTO_add_hint  ( plint, "select node range and output files" );
    PLUTO_add_number( plint, "first node", 0, 0, 0, 0, 1 );
    PLUTO_add_hint  ( plint, "starting node to process (zero based)" );
    PLUTO_add_number( plint, "last node", 0, 0, 0, 0, 1 );
    PLUTO_add_hint  ( plint, "end node to process (zero based)" );
    PLUTO_add_string( plint, "outfile.1D", 0, NULL, 14 );
    PLUTO_add_hint  ( plint, "name for 1D output file" );
    PLUTO_add_string( plint, "outfile.niml", 0, NULL, 14 );
    PLUTO_add_hint  ( plint, "name for niml output file" );

    /* debugging level */
    PLUTO_add_option( plint, "debug level", "debug level", FALSE );
    PLUTO_add_hint  ( plint, "choose debug level (and debug node)" );
    PLUTO_add_number( plint, "level", 0, 5, 0, 0, 1 );
    PLUTO_add_hint  ( plint, "debug level - how much to print to terminal" );
    PLUTO_add_number( plint, "node", 0, 0, 0, -1, 1 );
    PLUTO_add_hint  ( plint, "particular node to print debug infomation for" );

    RETURN(plint);
}

char * PV2S_main ( PLUGIN_interface * plint )
{
    pv2s_globals * g;
    static char    message[2048];	/* use this to output failures */

ENTRY("PV2S_main");

    g = &globs;			/* to have only one global access */
    message[0] = '\0';		/* init to empty string */

    g->vpo->ready = 0;

    if ( process_args(plint, g, message) != 0 )
	RETURN(message);

    RETURN(NULL);
}

/* base defaults to local and duplicate to global */
static int init_plugin_opts(pv2s_globals * g)
{
ENTRY("init_plugin_opts");
    memset(g->vpo, 0, sizeof(*g->vpo));

    g->vpo->ready =  0;		/* flag as "not ready to go" */
    g->vpo->surfA = -1;
    g->vpo->surfB = -1;

    g->vpo->sopt.dnode         = -1;
    g->vpo->sopt.outfile_1D    = NULL;
    g->vpo->sopt.outfile_niml  = NULL;

    RETURN(0);
}



static int process_args(PLUGIN_interface * plint, pv2s_globals * g, char * mesg)
{
    THD_session     * ss;
    MCW_idcode      * idc;
    v2s_plugin_opts   lvpo;
    v2s_opts_t      * sopt;
    float             fval;
    char            * tag, * str;
    int               val, ready = 0;

ENTRY("process_args");

    /* do we have a valid 3D view and session? */
    if ( !IM3D_OPEN(plint->im3d) || !plint->im3d->ss_now )
    {
	sprintf(mesg, "----------------------------------------------\n"
		      "strange failure: invalid 3D view or session???\n"
		      "----------------------------------------------");
	RETURN(-1);
    }

    /* to a quick check to be sure we are talking with suma */
    ss = plint->im3d->ss_now;
    if (ss && ss->su_num < 1)
    {
	sprintf(mesg, "-----------------------------------------\n"
		      "no surfaces: is afni 'talking' with suma?\n"
		      "-----------------------------------------");
	RETURN(1);
    }

    /* copy current values, and make local changes while checking */
    lvpo = *g->vpo;
    sopt = &lvpo.sopt;		/* just for typing */

    while ( (tag = PLUTO_get_optiontag(plint)) != NULL )
    {
	if ( ! strcmp(tag, "op_st") )
	{
	    str = PLUTO_get_string(plint);
	    val = PLUTO_string_index(str, P_NY_NVALS, gp_ny_list);

	    if ( val == 0 )		/* then don't test options */
		break;

	    if ( (val < 0) || (val >= P_NY_NVALS) )
	    {
		P_V2S_BAIL_VALUE(mesg,"bad NY vals", val);
		RETURN(1);
	    }
	    ready = val;		/* this is the interface to "ready" */

	    /* now get map */
	    str = PLUTO_get_string(plint);
	    val = PLUTO_string_index(str, P_MAP_NAMES_NVALS, g->maps);
	    if ( val == E_SMAP_INVALID )
	    {
		sprintf( mesg,  "--------------------------------\n"
				"please choose a mapping function\n"
				"--------------------------------" );
		RETURN(1);
	    }
	    else if ( (val < E_SMAP_INVALID) || (val >= E_SMAP_FINAL) )
	    {
		P_V2S_BAIL_VALUE(mesg, "illegal 'map func'", val);
		RETURN(1);
	    }
	    sopt->map = val;

	    /* now get step index */
	    str = PLUTO_get_string(plint);
	    val = PLUTO_string_index(str, P_STEP_NVALS, gp_step_list);
	    sopt->f_index = val > 0 ? 1 : 0;	/* be sure */

	    val = (int)PLUTO_get_number(plint);	/* num steps */
	    if ( (val <= 0) || (val >= V2S_STEPS_TOOOOO_BIG) )
	    {
		P_V2S_BAIL_VALUE(mesg, "steps too big", val);
		RETURN(1);
	    }
	    sopt->f_steps = val;
	}
	else if ( ! strcmp(tag, "surfaces") )
	{
	    lvpo.surfA = (int)PLUTO_get_number(plint);	/* surf_A */
	    str = PLUTO_get_string(plint);
	    if ( PLUTO_string_index(str, P_NY_NVALS, gp_ny_list) == 0 )
		lvpo.surfB = -1;	/* then do not use surf_B */
	    else
		lvpo.surfB = (int)PLUTO_get_number(plint);

	    if ( check_surfaces(plint, lvpo.surfA, lvpo.surfB, mesg) )
		RETURN(1);
   	}
	else if ( ! strcmp(tag, "normals") )
	{
	    str = PLUTO_get_string(plint);
	    if ( PLUTO_string_index(str, P_NY_NVALS, gp_ny_list) != 0 )
	    {
		sopt->use_norms = 1;
		sopt->norm_len = PLUTO_get_number(plint);

		str = PLUTO_get_string(plint);
		if ( PLUTO_string_index(str, P_KEEP_NVALS, gp_keep_list) != 0 )
		    sopt->keep_norm_dir = 1;
	    }
	}
	else if ( ! strcmp(tag, "offsets") )
	{
	    int test = 0;

	    sopt->f_p1_fr = PLUTO_get_number(plint);
	    sopt->f_pn_fr = PLUTO_get_number(plint);
	    sopt->f_p1_mm = PLUTO_get_number(plint);
	    sopt->f_pn_mm = PLUTO_get_number(plint);

	    /* check for consistency */
	    if ( sopt->f_p1_fr > 0 || sopt->f_pn_fr > 0 ) test |= 1;
	    if ( sopt->f_p1_mm > 0 || sopt->f_pn_mm > 0 ) test |= 2;
	    if ( test > 2 )  /* i.e. == 3 */
   	    {
		sprintf( mesg,  "---------------------------------\n"
				"use only one pair of f*_mm, f*_fr\n"
				"to change normal lengths     \n"
				"---------------------------------" );
		RETURN(1);
	    }
	}
	else if ( ! strcmp(tag, "oor") )
	{
	    int use = 0;

	    /* out of bounds ... */
	    str = PLUTO_get_string(plint);
	    if ( PLUTO_string_index(str, P_NY_NVALS, gp_ny_list) != 0 )
		use = 1;
	    fval = PLUTO_get_number(plint);
	    if ( use )
	    {
		sopt->oob.show  = 1;
		sopt->oob.value = fval;
	    }

	    /* out of mask ... */
	    use = 0;
	    str = PLUTO_get_string(plint);
	    if ( PLUTO_string_index(str, P_NY_NVALS, gp_ny_list) != 0 )
		use = 1;
	    fval = PLUTO_get_number(plint);
	    if ( use )
	    {
		sopt->oom.show  = 1;
		sopt->oom.value = fval;
	    }
	}
	else if ( ! strcmp(tag, "output") )
	{
	    sopt->first_node = (int)PLUTO_get_number(plint);
	    sopt->last_node  = (int)PLUTO_get_number(plint);
	    if ( sopt->first_node > sopt->last_node )
	    {
		sprintf( mesg,  "-----------------------------\n"
				"illegal node range values:   \n"
				"first (%d) > last (%d)       \n"
				"-----------------------------",
				sopt->first_node, sopt->last_node );
		RETURN(1);
	    }

	    /* get output filenames */
	    if ( sopt->outfile_1D )   free(sopt->outfile_1D);
	    if ( sopt->outfile_niml ) free(sopt->outfile_niml);
	    sopt->outfile_1D = sopt->outfile_niml = NULL;

	    str = PLUTO_get_string(plint);
	    if ( strlen(str) > 0 )
	    {
		sopt->outfile_1D = (char *)calloc(strlen(str)+1,sizeof(char));
		strcpy(sopt->outfile_1D, str);
	    }

	    str = PLUTO_get_string(plint);
	    if ( strlen(str) > 0 )
	    {
		sopt->outfile_niml = (char *)calloc(strlen(str)+1,sizeof(char));
		strcpy(sopt->outfile_niml, str);
	    }
	}
	else if ( ! strcmp(tag, "debug level") )
	{
	    sopt->debug = (int)PLUTO_get_number(plint);
	    sopt->dnode = (int)PLUTO_get_number(plint);
	}
	else
	{
	    sprintf( mesg,  "---------------------------\n"
			    "Unknown option tag : %s\n"
			    "---------------------------", tag );
	    RETURN(1);
	}
    }

    if ( sopt->debug > 1 )
	disp_v2s_opts_t( "plug_vol2surf options done : ", sopt );

    if ( lvpo.surfB >= 0 && sopt->use_norms )
    {
	sprintf( mesg,  "----------------------------------------\n"
			"cannot use normals while using surface B\n"
			"----------------------------------------" );
	RETURN(1);
    }

    /* for now, only output nodes and a single data column */
    sopt->skip_cols = V2S_SKIP_ALL ^ V2S_SKIP_NODES;

    if ( ready )		/* then copy changes over old values */
	*g->vpo = lvpo;

    RETURN(0);
}

static int check_surfaces(PLUGIN_interface * plint, int sa, int sb, char * mesg)
{
    THD_session * ss;

ENTRY("check_surfaces");

    ss = plint->im3d->ss_now;

    if (ss->su_num < 1 || (sb >= 0 && ss->su_num == 1))
    {
	P_V2S_BAIL_VALUE(mesg, "Not enough surfaces in session.\n", ss->su_num);
	RETURN(1);
    }

    if (sa >= ss->su_num)
    {
	P_V2S_BAIL_VALUE(mesg, "surf_A beyond valid index", ss->su_num - 1);
	RETURN(1);
    }

    if (sb >= ss->su_num)
    {
	P_V2S_BAIL_VALUE(mesg, "surf_B beyond valid index", ss->su_num - 1);
	RETURN(1);
    }

    /* rcr - Once SUMA_surface has been updated to include local domain
     *       parent, verify that the two surfaces come from the same one.
     *
     *       If not, output a list of valid surface labels with indices.
     */

    RETURN(0);
}
