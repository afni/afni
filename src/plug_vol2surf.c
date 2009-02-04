/***********************************************************************
 * plug_vol2surf.c              - plugin interface to vol2surf computation
 *
 * Provide an interface to the global v2s_plugin_opts structure.
 *
 * - R. Reynolds
 ***********************************************************************
*/

#include "afni.h"
#include "vol2surf.h"

#ifndef ALLOW_PLUGINS
#  error "Plugins not properly set up -- see machdep.h"
#endif

static char * PV2S_main( PLUGIN_interface * );

static char g_help[] = 
    " This plugin provides an interface to the vol2surf options used by afni\n"
    " for the display of the Overlay dataset values on the surfaces in suma.\n"
    " \n"
    " For each Local Domain Parent that afni has received from suma, vol2surf\n"
    " computes a node color overlay and sends it to suma.  This is computed\n"
    " by mapping afni's Overlay data across the 1 or 2 surfaces of each given\n"
    " LDP (Local Domain Parent).\n"
    " \n"
    " By default, each LDP gets a color list based on:\n"
    " \n"
    "   o  if only 1 surface for this LDP     : intersection of the surface\n"
    "      and afni's Overlay sub-brick, subject to afni's color bar and\n"
    "      thresholding (afni voxels that do not survive the threshold take\n"
    "      no part in the mapping)\n"
    " \n"
    "   o  if 2 or more surfaces for this LDP : intersection of the midpoint\n"
    "      of the first 2 surfaces received from suma (for this LDP) and the\n"
    "      Overlay sub-brick in afni, again subject to afni's color bar and\n"
    "      thresholding\n"
    " \n"
    " These defaults are for surfaces that have _not_ been chosen via the\n"
    " plugin interface.  Surfaces chosen via the plugin are, of course,\n"
    " mapped as the user specifies.  Note that to recreate the defaults, use\n"
    " the 'mask' function for 1 surface, and the midpoint function for more\n"
    " than 1.  The 'num steps' and 'seg index' will not be applicable.\n"
    " \n"
    " ---\n"
    " \n"
    " The minimum choices that must be made are on the 'function' line:\n"
    " 'use vol2surf?' (should be 'yes' :), and 'map func' (should be valid).\n"
    " \n"
    " ** Note that as a plugin, surfaces will be unknown at initialization\n"
    "    time.  Therefore the interface is via afni's surface index list.\n"
    "    To see the surface index list, set the debug level to 2, and then\n"
    "    press 'Set+Keep'.  Part of the text output to the _terminal window_\n"
    "    will be '+d valid surface indices and labels: ...'.\n"
    " \n"
    " ** For detailed infomation, please try the command: '3dVol2Surf -help'.\n"
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
    " surf pair 0:\n"
    " \n"
    "   apply?        : if no, skip this surface (pair)\n"
    "   surf_A        : choose the afni surface index for surface A\n"
    "   use B?        : form segments by joining nodes from surface A with\n"
    "                   nodes on surface B (an alternate choice is to use\n"
    "                   only surface A, along with its normals)\n"
    "   surf_B        : choose the afni surface index for surface B\n"
    " \n"
    " surf pair 1:\n"
    " \n"
    "   apply?        : if no, skip this surface (pair)\n"
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
    "   norm dir      : choose what to do with the normal directions:\n"
    "                     check   : let vol2surf try to decide\n"
    "                     keep    : keep the default directions\n"
    "                     reverse : reverse the default directions\n"
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
    " Author: R Reynolds\n"
    " \n"
    " ----------------------------------------------------------------------\n"
    "   History:\n"
    " \n"
    "   1.0   9 September 2004 [rickr] - initial version\n"
    " \n"
    "   1.1  16 September 2004 [rickr]\n"
    "     - init gp_index to -1 (set it in afni)\n"
    "     - allow the user to keep or reverse normal directions\n"
    " \n"
    "   1.2  29 September 2004 [rickr]\n"
    "     - now set global ready if all is well\n"
    "     - clear norms if not in use\n"
    "     - name all local functions PV2S_*\n"
    "     - if debug > 0, display chosen surfaces in terminal\n"
    "     - if debug > 1, display all possible surfaces in terminal\n"
    " \n"
    "   1.3  08 October 2004 [rickr]\n"
    "     - Global changes for new LDP processing:\n"
    "     - added second surface pair to GUI\n"
    "     - made small help and hint changes\n"
    "     - fixed receive order of fr and mm offsets (mm was fr)\n"
    "     - verify that surface pairs have matching LDPs\n"
    "     - added PV2S_disp_afni_surfaces() to list all surfaces w/indices\n"
    " \n"
    "   1.4  25 October 2004 [rickr]\n"
    "     - make sure the surface pairs are actually different\n"
    "     - make sure surfaces have the same number of nodes\n"
    "     - process all parameters, but only complain if 'ready'\n"
    "     - always pass along debug/dnode\n"
    " \n"
    "   1.5  11 Dec 2004 [rickr] - describe default behavior here\n"
    "   1.5a 22 Mar 2005 [rickr] - removed all tabs\n"
    "   1.6  28 Mar 2006 [rickr] - fixed mode computation\n"
    "   1.7  09 Aug 2006 [rickr] - store surface labels for history note\n"
        ;

#define P_MAP_NAMES_NVALS      12       /* should match enum for global maps */
#define P_NY_NVALS              2
#define P_KEEP_NVALS            3
#define P_STEP_NVALS            2

static char * gp_ny_list[]   = { "no", "yes" };
static char * gp_keep_list[] = { "no check yet", "keep", "reverse" };
static char * gp_step_list[] = { "voxel", "node" };

typedef struct
{
    v2s_plugin_opts  * vpo;
    char             * hist;
    char            ** maps;
} pv2s_globals;

static pv2s_globals globs;      /* these are just pointers to afni globals */

/* local functions */
static int PV2S_check_surfaces(PLUGIN_interface * plint, int sa, int sb,
                               char *mesg, int debug);
static int PV2S_disp_afni_surfaces(PLUGIN_interface * plint);
static int PV2S_init_plugin_opts(pv2s_globals * g);
static int PV2S_process_args(PLUGIN_interface * plint,pv2s_globals * g,
                             char *mesg);

/* for ease of error reporting */
#define PV2S_BAIL_VALUE(buf,str,val)                                   \
        do { sprintf((buf),  "-------------------------------------\n"  \
                             "%s\n"                                     \
                             "bad value = %d\n"                         \
                             "-------------------------------------",   \
                             (str), (val) ); } while (0)

DEFINE_PLUGIN_PROTOTYPE                 /* for C++ compilation */

PLUGIN_interface * PLUGIN_init( int ncall )
{
    PLUGIN_interface * plint;
    void             * void_vpo;

ENTRY("vol2surf: PLUGIN_init");

    if ( ncall > 0 ) RETURN(NULL);      /* only one interface */

    /* might be temporary */
    if ( PLUTO_set_v2s_addrs(&void_vpo, &globs.maps, &globs.hist) )
    {
        fprintf(stderr,"** plug_v2s: failed to init globals\n");
        RETURN(NULL);
    }

    /* using a void pointer so we don't have to put vol2surf.h in afni.h */
    globs.vpo = (v2s_plugin_opts *)void_vpo;

    PV2S_init_plugin_opts(&globs);

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

    /* choose surface indices for surf pair 0                   */
    /*   - note that we do not yet have surfaces to choose from */
    PLUTO_add_option( plint, "surf pair 0", "surf pair 0", TRUE );
    PLUTO_add_hint  ( plint, "choose first surface index(es)" );
    PLUTO_add_string( plint, "apply? ", P_NY_NVALS, gp_ny_list, 1 );
    PLUTO_add_hint  ( plint, "decide whether to apply surface(s)" );
    PLUTO_add_number( plint, "surf_A", 0, 50, 0, 0, 1 );
    PLUTO_add_hint  ( plint, "choose surface A index" );
    PLUTO_add_string( plint, "use B? ", P_NY_NVALS, gp_ny_list, 0 );
    PLUTO_add_hint  ( plint, "decide whether to use surface B" );
    PLUTO_add_number( plint, "surf_B", 0, 50, 0, 1, 1 );
    PLUTO_add_hint  ( plint, "choose surface B index" );

    /* choose surface indices for surf pair 1                   */
    /*   - note that we do not yet have surfaces to choose from */
    PLUTO_add_option( plint, "surf pair 1", "surf pair 1", TRUE );
    PLUTO_add_hint  ( plint, "choose second surface index(es)" );
    PLUTO_add_string( plint, "apply? ", P_NY_NVALS, gp_ny_list, 0 );
    PLUTO_add_hint  ( plint, "decide whether to apply surface(s)" );
    PLUTO_add_number( plint, "surf_A", 0, 50, 0, 2, 1 );
    PLUTO_add_hint  ( plint, "choose surface A index" );
    PLUTO_add_string( plint, "use B? ", P_NY_NVALS, gp_ny_list, 0 );
    PLUTO_add_hint  ( plint, "decide whether to use surface B" );
    PLUTO_add_number( plint, "surf_B", 0, 50, 0, 3, 1 );
    PLUTO_add_hint  ( plint, "choose surface B index" );

    /* menu for using normals */
    PLUTO_add_option( plint, "normals", "normals", FALSE );
    PLUTO_add_hint  ( plint, "control use of normals (instead of surf_B)" );
    PLUTO_add_string( plint, "use normals? ", P_NY_NVALS, gp_ny_list, 0 );
    PLUTO_add_hint  ( plint, "should normals be used to simulate surf_B?" );
    PLUTO_add_number( plint, "norm len", -100, 100, 1, 10, 1 );
    PLUTO_add_hint  ( plint, "what (signed) length should the normals be?" );
    PLUTO_add_string( plint, "norm dir", P_KEEP_NVALS, gp_keep_list, 1 );
    PLUTO_add_hint  ( plint, "check normal direction, or keep or reverse it" );

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
    PLUTO_add_hint  ( plint, "keep out-of-bounds nodes? (outside the dataset)");
    PLUTO_add_number( plint, "oob value", 0, 0, 0, -2, 1 );
    PLUTO_add_hint  ( plint, "value to apply when out of dataset bounds" );
    PLUTO_add_string( plint, "oom nodes?", P_NY_NVALS, gp_ny_list, 0 );
    PLUTO_add_hint  ( plint, "keep out-of-mask nodes? (below threshold)");
    PLUTO_add_number( plint, "oom value", 0, 0, 0, -1, 1 );
    PLUTO_add_hint  ( plint, "value for masked out nodes" );

    /* choose node processing range */
    PLUTO_add_option( plint, "output", "output", FALSE );
    PLUTO_add_hint  ( plint, "select node range and output files" );
    PLUTO_add_number( plint, "first node", 0, 0, 0, 0, 1 );
    PLUTO_add_hint  ( plint, "starting node to process (zero based)" );
    PLUTO_add_number( plint, "last node", 0, 0, 0, 0, 1 );
    PLUTO_add_hint  ( plint, "end node to process (zero based)" );
    PLUTO_add_string( plint, "outfile.1D", 0, NULL, 14 );
    PLUTO_add_hint  ( plint, "name for 1D output file - will overwrite!" );
    PLUTO_add_string( plint, "outfile.niml", 0, NULL, 14 );
    PLUTO_add_hint  ( plint, "name for niml output file - will overwrite!" );

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
    static char    message[2048];       /* use this to output failures */

ENTRY("PV2S_main");

    g = &globs;                 /* to have only one global access */
    message[0] = '\0';          /* init to empty string */

    g->vpo->ready = 0;

    if ( (PV2S_process_args(plint, g, message) != 0) && message[0] )
        RETURN(message);

    RETURN(NULL);
}

/* base defaults to local and duplicate to global */
static int PV2S_init_plugin_opts(pv2s_globals * g)
{
ENTRY("PV2S_init_plugin_opts");
    memset(g->vpo, 0, sizeof(*g->vpo));

    g->vpo->ready    =  0;         /* flag as "not ready to go" */
    g->vpo->use0     =  0;
    g->vpo->use1     =  0;
    g->vpo->s0A      = -1;
    g->vpo->s0B      = -1;
    g->vpo->s1A      = -1;
    g->vpo->s1B      = -1;
    g->vpo->label[0] = gv2s_no_label;    /* init surface labels as undefined */
    g->vpo->label[0] = gv2s_no_label;    /*               7 Aug 2006 [rickr] */
    g->vpo->label[0] = gv2s_no_label;
    g->vpo->label[0] = gv2s_no_label;

    g->vpo->sopt.gp_index      = -1;
    g->vpo->sopt.dnode         = -1;
    g->vpo->sopt.outfile_1D    = NULL;
    g->vpo->sopt.outfile_niml  = NULL;
    g->vpo->sopt.segc_file     = NULL;   /* 30 Jun 2006 */

    RETURN(0);
}



static int PV2S_process_args(PLUGIN_interface * plint, pv2s_globals * g,
                             char * mesg)
{
    THD_session     * ss;
    v2s_plugin_opts   lvpo;
    v2s_opts_t      * sopt;
    float             fval;
    char            * tag, * str;
    int               val, ready = 0;

ENTRY("PV2S_process_args");

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
    sopt = &lvpo.sopt;          /* just for typing */

    while ( (tag = PLUTO_get_optiontag(plint)) != NULL )
    {
        if ( sopt->debug > 2 )
            fprintf(stderr,"+d received option tag: %s\n", tag);

        if ( ! strcmp(tag, "op_st") )
        {
            str = PLUTO_get_string(plint);
            val = PLUTO_string_index(str, P_NY_NVALS, gp_ny_list);

            if ( (val < 0) || (val >= P_NY_NVALS) )
            {
                PV2S_BAIL_VALUE(mesg,"bad NY vals", val);
                RETURN(1);
            }
            ready = val;                /* this is the interface to "ready" */

            /* now get map */
            str = PLUTO_get_string(plint);
            val = PLUTO_string_index(str, P_MAP_NAMES_NVALS, g->maps);
            if ( ready && val == E_SMAP_INVALID )
            {
                sprintf( mesg,  "--------------------------------\n"
                                "please choose a mapping function\n"
                                "--------------------------------" );
                RETURN(1);
            }
            else if (ready && ((val < E_SMAP_INVALID) || (val >= E_SMAP_FINAL)))
            {
                PV2S_BAIL_VALUE(mesg, "illegal 'map func'", val);
                RETURN(1);
            }
            sopt->map = val;

            /* now get step index */
            str = PLUTO_get_string(plint);
            val = PLUTO_string_index(str, P_STEP_NVALS, gp_step_list);
            sopt->f_index = val > 0 ? 1 : 0;    /* be sure */

            val = (int)PLUTO_get_number(plint); /* num steps */
            if (ready && ((val <= 0) || (val >= V2S_STEPS_TOOOOO_BIG)))
            {
                PV2S_BAIL_VALUE(mesg, "steps too big", val);
                RETURN(1);
            }
            sopt->f_steps = val;
        }
        else if ( ! strcmp(tag, "surf pair 0") )
        {
            lvpo.use0 = 0;
            str = PLUTO_get_string(plint);
            if ( PLUTO_string_index(str, P_NY_NVALS, gp_ny_list) != 0 )
            {
                lvpo.use0 = 1;
                lvpo.s0A = (int)PLUTO_get_number(plint);
                lvpo.s0B = -1;  /* first assume to not use surf_B */
                str = PLUTO_get_string(plint);
                if ( PLUTO_string_index(str, P_NY_NVALS, gp_ny_list) != 0 )
                    lvpo.s0B = (int)PLUTO_get_number(plint);
            }
        }
        else if ( ! strcmp(tag, "surf pair 1") )
        {
            lvpo.use1 = 0;
            str = PLUTO_get_string(plint);
            if ( PLUTO_string_index(str, P_NY_NVALS, gp_ny_list) != 0 )
            {
                lvpo.use1 = 1;
                lvpo.s1A = (int)PLUTO_get_number(plint);
                lvpo.s1B = -1;  /* first assume to not use surf_B */
                str = PLUTO_get_string(plint);
                if ( PLUTO_string_index(str, P_NY_NVALS, gp_ny_list) != 0 )
                    lvpo.s1B = (int)PLUTO_get_number(plint);
            }
        }
        else if ( ! strcmp(tag, "normals") )
        {
            str = PLUTO_get_string(plint);
            if ( PLUTO_string_index(str, P_NY_NVALS, gp_ny_list) != 0 )
            {
                sopt->use_norms = 1;
                sopt->norm_len = PLUTO_get_number(plint);

                str = PLUTO_get_string(plint);
                val = PLUTO_string_index(str, P_KEEP_NVALS, gp_keep_list);
                if      ( val == 1 ) sopt->norm_dir = V2S_NORM_KEEP;
                else if ( val == 2 ) sopt->norm_dir = V2S_NORM_REVERSE;
                else                 sopt->norm_dir = V2S_NORM_DEFAULT;
            }
            else
                sopt->use_norms = 0;
        }
        else if ( ! strcmp(tag, "offsets") )
        {
            int test = 0;

            sopt->f_p1_mm = PLUTO_get_number(plint);
            sopt->f_pn_mm = PLUTO_get_number(plint);
            sopt->f_p1_fr = PLUTO_get_number(plint);
            sopt->f_pn_fr = PLUTO_get_number(plint);

            /* check for consistency */
            if ( sopt->f_p1_fr != 0 || sopt->f_pn_fr != 0 ) test |= 1;
            if ( sopt->f_p1_mm != 0 || sopt->f_pn_mm != 0 ) test |= 2;
            if ( ready && test > 2 )  /* i.e. == 3 */
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
            /* out of bounds ... */
            str  = PLUTO_get_string(plint);
            val  = PLUTO_string_index(str, P_NY_NVALS, gp_ny_list);
            fval = PLUTO_get_number(plint);
            if ( val != 0 )
            {
                sopt->oob.show  = 1;
                sopt->oob.value = fval;
            }
            else
                sopt->oob.show  = 0;

            /* out of mask ... */
            str  = PLUTO_get_string(plint);
            val  = PLUTO_string_index(str, P_NY_NVALS, gp_ny_list);
            fval = PLUTO_get_number(plint);
            if ( val != 0 )
            {
                sopt->oom.show  = 1;
                sopt->oom.value = fval;
            }
            else
                sopt->oom.show  = 0;
        }
        else if ( ! strcmp(tag, "output") )
        {
            sopt->first_node = (int)PLUTO_get_number(plint);
            sopt->last_node  = (int)PLUTO_get_number(plint);
            if ( ready &&  sopt->first_node > sopt->last_node )
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

            /* make sure the string looks like *.niml.dset    4 Nov 2008 */
            val = strlen(str);
            if ( val > 0 && (val < 11 || strcmp(str+val-10,".niml.dset")) )
            {
                sprintf( mesg,  "-----------------------------------\n"
                                "NIML dataset must end in .niml.dset\n"
                                "have: %s\n"
                                "-----------------------------------", str );
                RETURN(1);
            }

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
    {
        disp_v2s_plugin_opts( "plug_vol2surf options done : ", &lvpo );
        disp_v2s_opts_t( "  surface options : ", sopt );
    }

    /* should we just run away?  always adjust debugging first... */
    g->vpo->sopt.debug = lvpo.sopt.debug;
    g->vpo->sopt.dnode = lvpo.sopt.dnode;

    if ( lvpo.use0 == 0 && lvpo.use1 == 0 ) ready = 0;

    if ( ! ready )
    {
        if ( sopt->debug > 0 )
            PV2S_disp_afni_surfaces(plint);
        RETURN(1);
    }

    if ( ! v2s_is_good_map(sopt->map, 1) )
    {
        sprintf( mesg,  "-------------------------------------------\n"
                        "mapping function is invalid in this context\n"
                        "index %d, name '%s'\n"
                        "-------------------------------------------",
                sopt->map,
                (sopt->map < E_SMAP_INVALID || sopt->map >= E_SMAP_FINAL) ?
                "out-of-range" : g->maps[sopt->map] );
        RETURN(1);
    }

    /* verify surface consistency */
    if ( lvpo.use0 )
    {
        if ( PV2S_check_surfaces(plint, lvpo.s0A, lvpo.s0B, mesg, sopt->debug) )
            RETURN(1);
        if ( lvpo.s0A == lvpo.s0B ) 
        {
            sprintf( mesg,  "--------------------------------\n"
                            "error: for pair 0, surfA = surfB\n"
                            "--------------------------------" );
            RETURN(1);
        }
        lvpo.label[0] = ss->su_surf[lvpo.s0A]->label_ldp;
        if(lvpo.s0B >= 0) lvpo.label[1] = ss->su_surf[lvpo.s0B]->label_ldp;
    }
    else
        lvpo.label[0] = lvpo.label[1] = gv2s_no_label;    /* no labels */

    if ( lvpo.use1 )
    {
        if ( PV2S_check_surfaces(plint, lvpo.s1A, lvpo.s1B, mesg, sopt->debug) )
            RETURN(1);
        if ( lvpo.s1A == lvpo.s1B ) 
        {
            sprintf( mesg,  "--------------------------------\n"
                            "error: for pair 1, surfA = surfB\n"
                            "--------------------------------" );
            RETURN(1);
        }
        lvpo.label[2] = ss->su_surf[lvpo.s1A]->label_ldp;
        if(lvpo.s1B >= 0) lvpo.label[3] = ss->su_surf[lvpo.s1B]->label_ldp;
    }
    else
        lvpo.label[2] = lvpo.label[3] = gv2s_no_label;    /* no labels */

    /* if the user wan't normals, they can only supply one surface per pair */
    if ( sopt->use_norms && ((lvpo.use0 && lvpo.s0B >= 0) ||
                             (lvpo.use1 && lvpo.s1B >= 0)) )
    {
        sprintf( mesg,  "----------------------------------------\n"
                        "cannot use normals while using surface B\n"
                        "----------------------------------------" );
        RETURN(1);
    }

    if ( sopt->debug > 0 )
        PV2S_disp_afni_surfaces(plint);

    /* for now, only output nodes and a single data column */
    sopt->skip_cols = V2S_SKIP_ALL ^ V2S_SKIP_NODES;

    if ( ready )                /* then copy changes over old values */
    {
        *g->vpo = lvpo;
        g->vpo->ready = 1;
    }

    RETURN(0);
}

static int PV2S_check_surfaces(PLUGIN_interface * plint, int sa, int sb,
                               char * mesg, int debug)
{
    THD_session * ss;

ENTRY("PV2S_check_surfaces");

    ss = plint->im3d->ss_now;

    if ( ss->su_num < 1 )
    {
        PV2S_BAIL_VALUE(mesg, "Not enough surfaces in session.\n", ss->su_num);
        RETURN(1);
    }

    /* verify that the surface indices are valid */
    if ( sa < 0 )
    {
        PV2S_BAIL_VALUE(mesg, "surf_A has invalid index", sa);
        RETURN(1);
    }

    if ( sa >= ss->su_num )
    {
        PV2S_BAIL_VALUE(mesg, "surf_A beyond valid index", ss->su_num - 1);
        RETURN(1);
    }

    if ( sb >= ss->su_num )
    {
        PV2S_BAIL_VALUE(mesg, "surf_B beyond valid index", ss->su_num - 1);
        RETURN(1);
    }

    if ( sb >= 0 )
    {
        /* then check that surf_A and surf_B share LDP */
        if (strncmp(ss->su_surf[sa]->idcode_ldp,ss->su_surf[sb]->idcode_ldp,32))
        {
            char * la = ss->su_surf[sa]->label_ldp;
            char * lb = ss->su_surf[sb]->label_ldp;
            if ( ! *la ) la = "undefined";
            if ( ! *lb ) lb = "undefined";
            sprintf(mesg, "---------------------------------------\n"
                          "Error: Surf_A and Surf_B have different\n"
                          "       local domain parents\n"
                          "LDP #%d = '%s', LDP #%d = '%s'\n"
                          "---------------------------------------",
                          sa, la, sb, lb);
            RETURN(1);
        }

        /* and that they have the same number of nodes */
        if ( ss->su_surf[sa]->num_ixyz != ss->su_surf[sb]->num_ixyz )
        {
            sprintf(mesg, "------------------------------------------------\n"
                          "Big problem: Surf_A and Surf_B have different\n"
                          "    numbers of nodes!  They cannot share an LDP.\n"
                          "    SurfA: '%s', %d nodes\n"
                          "    SurfB: '%s', %d nodes\n"
                          "------------------------------------------------",
                          ss->su_surf[sa]->label, ss->su_surf[sa]->num_ixyz,
                          ss->su_surf[sb]->label, ss->su_surf[sb]->num_ixyz);
            RETURN(1);
        }
    }

    if ( debug > 0 && ss->su_surf )
    {
        if ( ss->su_surf[sa] )      /* we have checked sa >= 0, above */
            fprintf(stderr,"+d surf_A label: '%s'\n",
                *ss->su_surf[sa]->label ? ss->su_surf[sa]->label : "not set");
        else
            fprintf(stderr,"** surf_A (#%d) pointer not set??\n", sa);

        if ( sb < 0 )
            fprintf(stderr,"-d surf_B not in use\n");
        else if ( ss->su_surf[sb] )
            fprintf(stderr,"+d surf_B label: '%s'\n",
                *ss->su_surf[sb]->label ? ss->su_surf[sb]->label : "not set");
        else
            fprintf(stderr,"** surf_B (#%d) pointer not set??\n", sb);
    }

    RETURN(0);
}

static int PV2S_disp_afni_surfaces(PLUGIN_interface * plint)
{
    THD_session * ss;
    char        * ldp, * label;
    int           c;

ENTRY("disp_afni_surfaces");

    ss = plint->im3d->ss_now;

    if ( ss->su_surf <= 0 )
        RETURN(0);

    fprintf(stderr,"-d --------------------------------------\n");
    fprintf(stderr,"   afni surface indices, labels and LDPs:\n");
    for ( c = 0; c < ss->su_num; c++ )
    {
        label = ss->su_surf[c]->label;
        ldp   = ss->su_surf[c]->label_ldp;
        if ( ! *label ) label = "undefined";
        if ( ! *ldp   ) ldp   = "undefined";

        fprintf(stderr,"   index %2d, label '%s', LDP '%s'\n",
                    c, label, ldp );
    }

    RETURN(0);
}
