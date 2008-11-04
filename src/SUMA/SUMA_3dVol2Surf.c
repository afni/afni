/*----------------------------------------------------------------------
 * 3dVol2Surf - dump ascii dataset values corresponding to a surface
 *
 * This program is used to display AFNI dataset values that correspond to
 * a surface.  The surface points are mapped to xyz coordinates, according
 * to the SURF_VOL (surface volume) AFNI dataset.  These coordinates are
 * then matched to voxels in other AFNI datasets.  So given any other
 * AFNI dataset, this program can output all of the sub-brick values that
 * correspond to each of the surface locations.  The user also has options
 * to mask regions for output.
 *
 * usage:
 *    3dVol2Surf [options] -spec SPEC_FILE -sv SURF_VOL               \
 *                         -grid_par AFNI_DSET -map_func MAPPING_FUNC
 *
 * options:
 *
 *      -help
 *      -hist
 *      -v2s_hist
 *      -version
 *
 *      -grid_par   AFNI_DSET
 *      -map_func   MAP_FUNCTION
 *      -spec       SPEC_FILE
 *      -sv         SURF_VOL
 *
 *      -cmask      MASK_COMMAND
 *      -debug      LEVEL
 *      -dnode      NODE_NUM
 *      -f_index    INDEX_TYPE
 *      -f_steps    NUM_STEPS
 *      -f_p1_mm    DISTANCE
 *      -f_pn_mm    DISTANCE
 *      -f_p1_fr    FRACTION
 *      -f_pn_fr    FRACTION
 *      -gp_index   INDEX
 *      -oob_index  INDEX
 *      -oob_value  VALUE
 *      -oom_value  VALUE
 *      -out_1D     OUTPUT_FILE
 *      -no_headers
 *
 * examples:
 *
 *    3dVol2Surf -spec     SubjA.spec     -sv       SubjA_anat+orig   \ 
 *               -grid_par SubjA_EPI+orig -map_func mask  >  output.txt
 *
 *    3dVol2Surf -spec       SubjectA.spec                              \
 *               -sv         SubjectA_spgr+orig                         \ 
 *               -grid_par   SubjA_EPI+orig                             \
 *               -cmask      '-a SubjA.func+orig[2] -expr step(a-0.6)'  \
 *               -map_func   midpoint                                   \
 *               -out_niml   SubjA_surf_out.niml.dset
 *
 *----------------------------------------------------------------------
*/

/* define program history for -hist option */

static char g_history[] = 

    "----------------------------------------------------------------------\n"
    "history:\n"
    "\n"
    "1.0  February 10, 2003  [rickr]\n"
    "  - initial release\n"
    "\n"
    "1.1  February 11, 2003  [rickr]\n"
    "  - handle no arguments as with -help\n"
    "  - minor updates to -help\n"
    "\n"
    "1.2  February 13, 2003  [rickr]\n"
    "  - init SUMAg array pointers, check before calling Free_()\n"
    "\n"
    "1.3  February 14, 2003  [rickr]\n"
    "  - optionally enable more SUMA debugging\n"
    "\n"
    "2.0  June 06, 2003  [rickr]\n"
    "  - re-wrote program according to 3dSurf2Vol (which was written\n"
    "    according to this :) - using map functions and node lists\n"
    "  - added midpoint map function\n"
    "\n"
    "2.1  June 10, 2003  [rickr]\n"
    "  - added ave map function (see dump_ave_map)\n"
    "\n"
    "2.2  June 19, 2003  [rickr]\n"
    "  - added -f_index INDEX_TYPE option (to index across nodes, too)\n"
    "  - set the default of -f_steps to 2\n"
    "  - use SMD prefix for macros\n"
    "\n"
    "2.3  July 21, 2003  [rickr]\n"
    "  - fixed problem with nodes outside grid_par dataset\n"
    "  - added min/max distance info\n"
    "\n"
    "3.0  August 05, 2003  [rickr]\n"
    "  - renamed SUMA_3dSurfMaskDump.[ch] to SUMA_3dVol2Surf.[ch]\n"
    "  - all output functions now go through dump_surf_3dt\n"
    "  - dump_surf_3dt() is a generalized function to get an MRI_IMARR for\n"
    "    one or a pair of nodes, by converting to a segment of points\n"
    "  - added v2s_adjust_endpts() to apply segment endpoint modifications\n"
    "  - added segment_imarr() to get the segment of points and fill the\n"
    "    MRI_IMARR list (along with other info)\n"
    "  - filter functions have been taken to v2s_apply_filter()\n"
    "  - added min, max and seg_vals map functions (filters)\n"
    "  - added options of the form -f_pX_XX to adjust segment endpoints\n"
    "  - added -dnode option for specific node debugging\n"
    "  - changed -output option to -out_1D\n"
    "  - added new debug info\n"
    "  - added checking of surface order (process from inner to outer)\n"
    "\n"
    "3.1  September 17, 2003  [rickr]\n"
    "  - fixed the help instructions for '-cmask'\n"
    "\n"
    "3.2  September 20, 2003  [rickr]\n"
    "  - added max_abs mapping function\n"
    "  - added options '-oob_index' and '-oob_value'\n"
    "  - added CHECK_NULL_STR macro\n"
    "\n"
    "3.3  September 23, 2003  [rickr]\n"
    "  - added help for -no_headers option\n"
    "\n"
    "3.4  October 01, 2003  [rickr]\n"
    "  - added -oom_value option\n"
    "  - added additional help example (for -oob and -oom options)\n"
    "\n"
    "3.5  October 20, 2003  [rickr]\n"
    "  - call the new engine function, SUMA_LoadSpec_eng()\n"
    "    (this will restrict the debug output from SUMA_LoadSpec())\n"
    "\n"
    "3.6  October 21, 2003  [rickr]\n"
    "  - finish upates for -f_keep_surf_order option\n"
    "    (help and sopt)\n"
    "\n"
    "3.7  November 04, 2003  [rickr]\n"
    "  - added ENTRY() stuff\n"
    "\n"
    "3.8  December 15, 2003  [rickr]\n"
    "  - added options '-surf_A' and '-surf_B'\n"
    "  - called SUMA_spec_select_surfs() and SUMA_spec_set_map_refs()\n"
    "    to pick requested surfaces from spec file\n"
    "  - removed option '-kso'\n"
    "  - added '-hist' option\n"
    "\n"
    "3.9  January 08, 2004  [rickr]\n"
    "  - added '-use_norms' option (segments come from norms)\n"
    "  - added '-norm_len' option to alter default normal lengths\n"
    "  - added '-keep_norm_dir' option to prevent direction check\n"
    "  - reversed order from '-hist' option (newer at bottom)\n"
    "  - added example with normals to help, along with option descriptions\n"
    "\n"
    "4.0  January 23, 2004  [rickr]\n"
    "  - SUMA_isINHmappable() is deprecated, check with AnatCorrect field\n"
    "  - version -> 4.0 to celebrate normals :)\n"
    "\n"
    "4.1  February 10, 2004  [rickr]\n"
    "  - output a little more debug info for !AnatCorrect case\n"
    "  - small updates to help examples\n"
    "\n"
    "4.2  February 18, 2004  [rickr]\n"
    "  - added functionality for mapping functions that require sorting\n"
    "  - added mapping functions: median and mode\n"
    "\n"
    "4.3  February 19, 2004  [rickr]\n"
    "  - track 1dindex sources for new sorting filters (median, mode)\n"
    "    i.e. idindex is accurate, not just defaulting to first node\n"
    "\n"
    "4.4  March 26, 2004  [ziad]\n"
    "  - DsetList added to SUMA_LoadSpec_eng() call\n"
    "\n"
    "4.5  April 07, 2004  [rickr]\n"
    "  - fixed inconsistency in check_norm_dirs(), default dirs reversed\n"
    "\n"
    "5.0  May 18, 2004  [rickr]\n"
    "  - added '-out_niml' option for niml output\n"
    "  - added '-first_node' and '-last_node' option for limited output\n"
    "  - made major additions for memory handling of output data\n"
    "    (went from 'print as you go' to 'store and output at end')\n"
    "\n"
    "5.1  May 19, 2004  [rickr]\n"
    "  - added help for '-first_node' and '-last_node' options (sorry, Ziad)\n"
    "\n"
    "6.0  September 1, 2004  [rickr]\n"
    "  - created vol2surf() library files vol2surf.[ch] from core functions\n"
    "  - this represents a significant re-write of many existing functions,\n"
    "    modifying locations of action, structure names/contents, etc.\n"
    "  - add library to libmri (as this will end up in afni proper)\n"
    "  - separate all vol2surf.[ch] functions from SUMA_3dVol2surf.[ch]\n"
    "  - keep allocation/free action of results struct within library\n"
    "  - now using SUMA_surface struct for surface info (replace node_list)\n"
    "  - added main vol2surf(), afni_vol2surf(), free_v2s_results(),\n"
    "    and disp...() functions as vol2surf library interface\n"
    "  - added options to control column output (-skip_col_NAME)\n"
    "  - added -v2s_hist option for library history access\n"
    "\n"
    "6.1  September 2, 2004  [rickr]\n"
    "  - library organizing: moved v2s_map_type() to vol2surf.c\n"
    "  - moved gv2s_map_names to vol2surf.c, and externs to vol2surf.h\n"
    "\n"
    "6.2  September 16, 2004  [rickr]\n"
    "  - added -gp_index option, mostly to use in plugin interface\n"
    "  - added -reverse_norm_dir option, for reversing the default direction\n"
    "\n"
    "6.3  October 8, 2004  [rickr]\n"
    "  - in suma2afni_surf(), dealt with LDP changes to SUMA_surface\n"
    "  - changed write_outfile functions to v2s_*() and moved them to library\n"
    "\n"
    "6.3a March 22, 2005 [rickr] - removed tabs\n"
    "6.4  June   2, 2005 [rickr] - added -skip_col_non_results option\n"
    "6.5  June  30, 2006 [rickr] - added -save_seg_coords option\n"
    "6.6  Aug 9, 2006 [rickr]\n"
    "  - store command-line arguments for history note\n"
    "  - added -skip_col_NSD_format option\n"
    "\n"
    "6.7  Aug 23, 2006 [rickr] - added/modified output column options\n"
    "  - changed -skip_col_results     to -outcols_1_result\n"
    "  - changed -skip_col_non_results to -outcols_results\n"
    "  - changed -skip_col_NSD_format  to -outcols_NSD_format\n"
    "  - added -outcols_afni_NSD option\n"
    "\n"
    "6.8  Dec 15, 2006 [rickr] - added example for EPI -> surface in help\n"
    "---------------------------------------------------------------------\n";

#define VERSION "version  6.7 (Aug 23, 2006)"

/*----------------------------------------------------------------------
 * todo:
 *----------------------------------------------------------------------
*/

#include "mrilib.h"
#include "SUMA_suma.h"
#include "vol2surf.h"
#include "SUMA_3dVol2Surf.h"

/* --------------------  globals  -------------------- */

/* for all SUMA programs... */
SUMA_SurfaceViewer * SUMAg_SVv = NULL;  /* array of Surf View structs   */
int                  SUMAg_N_SVv = 0;   /* length of SVv array          */
SUMA_DO            * SUMAg_DOv = NULL;  /* array of Displayable Objects */
int                  SUMAg_N_DOv = 0;   /* length of DOv array          */
SUMA_CommonFields  * SUMAg_CF = NULL;   /* info common to all viewers   */


/* --------------------  AFNI prototype(s)  -------------------- */
extern void machdep( void );

#define MAIN

/*----------------------------------------------------------------------*/

int main( int argc , char * argv[] )
{
    SUMA_SurfSpecFile  spec;
    v2s_param_t        params;
    v2s_opts_t         sopt;
    opts_t             opts;
    int                ret_val;

    mainENTRY("3dVol2Surf main");
    machdep();
    AFNI_logger("3dVol2Surf",argc,argv);

    /* validate inputs and init options structure */
    if ( ( ret_val = init_options(&opts, argc, argv) ) != 0 )
        return ret_val;

    if ( ( ret_val = validate_options(&opts, &params) ) != 0 )
        return ret_val;

    if ( (ret_val = set_smap_opts( &opts, &params, &sopt, argc, argv )) != 0 )
        return ret_val;

    /* initialize the spec ZSS Jan 9 06*/
    if (!SUMA_AllocSpecFields(&spec)) {
       fprintf( stderr, "** failed to initialize spec\n" );
       return(-1);
    }

    /* read surface files */
    if ( (ret_val = read_surf_files(&opts, &spec)) != 0 )
        return ret_val;

    /*  get node list from surfaces (multiple points per node)
     *  need merge function
     */
    if ( (ret_val = get_surf_data( &sopt, &params )) != 0 )
        return ret_val;

    if ( (ret_val = write_output( &sopt, &params )) != 0 )
        return ret_val;

    /* free memory */
    final_clean_up(&opts, &params, &spec);

    /* free the spec ZSS Jan 9 06*/
    if (!SUMA_FreeSpecFields(&spec)) {
       fprintf( stderr, "** failed to free spec\n" );
       return(-1);
    }

    return ret_val;
}


/*----------------------------------------------------------------------
 * write_output
 *----------------------------------------------------------------------
*/
int write_output ( v2s_opts_t * sopt, v2s_param_t * p )
{
    v2s_results * sd;
    int rv = 0;
ENTRY("write_output");

    if ( sopt == NULL || p == NULL )
    {
        fprintf( stderr, "** v2s_wo - bad params (%p,%p)\n", sopt, p );
        RETURN(-1);
    }

    if ( sopt->map == E_SMAP_INVALID )
    {
        fprintf(stderr,"** v2s wo: invalid map %d\n", sopt->map);
        RETURN(-1);
    }

    sd = vol2surf(sopt, p);
    if ( !sd )
        fprintf(stderr,"** vol2surf failed\n");

    if ( sd && sopt->debug > 1 ) disp_v2s_results("-- post vol2surf() : ",sd);

    if ( sd && sopt->outfile_1D )
        rv = v2s_write_outfile_1D(sopt, sd, p->surf[0].label);

    if ( sd && !rv && sopt->outfile_niml )
        rv = v2s_write_outfile_NSD(sd, sopt, p, 1); /* request to free data */

    free_v2s_results( sd );
    sd = NULL;

    RETURN(rv);
}


/*----------------------------------------------------------------------
 * get_surf_data        - from surfaces
 *
 *----------------------------------------------------------------------
*/
int get_surf_data ( v2s_opts_t * sopt, v2s_param_t * p )
{
    int nsurf = 2;

ENTRY("get_surf_data");

    if ( sopt == NULL || p == NULL )
    {
        fprintf( stderr, "** cnl - bad params (%p,%p)\n", sopt, p );
        RETURN(-1);
    }

    if ( (sopt->map == E_SMAP_MASK) || sopt->use_norms )
        nsurf = 1;

    if ( copy_surfaces( sopt, p, nsurf ) )
        RETURN(-1);

    if ( sopt->use_norms )
    {
        if ( ! p->surf[0].norm )
        {
            fprintf(stderr,"** failure: surface '%s' has no normal list\n",
                    CHECK_NULL_STR(p->surf[0].label));
            RETURN(-1);
        }

        if ((sopt->norm_dir == V2S_NORM_DEFAULT) && check_norm_dirs(sopt, p, 0))
            RETURN(-1);
        else if ( sopt->norm_dir == V2S_NORM_REVERSE)
        {
            /* okay, reverse the direction */
            sopt->norm_len *= -1;
            if ( sopt->debug > 0 )
                fprintf(stderr,"++ reversing normal direction\n");
        }
        /* else V2S_NORM_KEEP, i.e. do nothing */
    }

    RETURN(0);
}


/*----------------------------------------------------------------------
 * check_norm_dirs      - check to reverse normal directions
 *
 * Count how many of six bounding points point away from the center.
 *     6   : perfect, no change
 *     5   : provide warning, but apply no change
 *     2-4 : cannot determine direction
 *           --> this is a program terminating failure condition
 *     1   : provide warning, and reverse direction
 *     0   : no warning, just reverse direction
 *
 * return   0 : success (cases 0,1,5,6)
 *         -1 : failure (cases 2-4)
 *----------------------------------------------------------------------
*/
int check_norm_dirs ( v2s_opts_t * sopt, v2s_param_t * p, int surf )
{
    SUMA_ixyz * coords;
    THD_fvec3 * norms;
    float       fmin[3], fmax[3];
    int         min[3], max[3];
    int         match[6];               /* +/- 1, for direction match */
    int         node, c, ncount;

ENTRY("check_norm_dirs");

    norms  = p->surf[surf].norm;         /* point to norms for this surface  */
    coords = p->surf[surf].ixyz;         /* point to coords for this surface */

    /* init mins and max's from node 0 */
    min[0] = max[0] = min[1] = max[1] = min[2] = max[2] = 0;

    fmin[0] = fmax[0] = coords->x;
    fmin[1] = fmax[1] = coords->y;
    fmin[2] = fmax[2] = coords->z;

    /* now check the rest of them */
    for ( node = 1; node < p->surf[surf].num_ixyz; node++, coords++ )
    {
        if ( fmin[0] > coords[node].x )         /* x min */
        {
            min [0] = node;
            fmin[0] = coords[node].x;
        }
        if ( fmax[0] < coords[node].x )         /* x max */
        {
            max [0] = node;
            fmax[0] = coords[node].x;
        }

        if ( fmin[1] > coords[node].y )         /* y min */
        {
            min [1] = node;
            fmin[1] = coords[node].y;
        }
        if ( fmax[1] < coords[node].y )         /* y max */
        {
            max [1] = node;
            fmax[1] = coords[node].y;
        }

        if ( fmin[2] > coords[node].z )         /* z min */
        {
            min [2] = node;
            fmin[2] = coords[node].z;
        }
        if ( fmax[2] < coords[node].z )         /* z max */
        {
            max [2] = node;
            fmax[2] = coords[node].z;
        }
    }

    if ( sopt->debug > 1 )
        fprintf(stderr,"++ normals:\n"
                "      mins       : %d, %d, %d\n"
                "      min coords : %f, %f, %f\n"
                "      mins0      : %f, %f, %f\n"
                "      mins1      : %f, %f, %f\n"
                "      mins2      : %f, %f, %f\n"
                "\n"
                "      maxs       : %d, %d, %d\n"
                "      max coords : %f, %f, %f\n"
                "      maxs0      : %f, %f, %f\n"
                "      maxs1      : %f, %f, %f\n"
                "      maxs2      : %f, %f, %f\n",
                min[0], min[1], min[2], 
                fmin[0], fmin[1], fmin[2], 
                coords[min[0]].x, coords[min[0]].y, coords[min[0]].z, 
                coords[min[1]].x, coords[min[1]].y, coords[min[1]].z, 
                coords[min[2]].x, coords[min[2]].y, coords[min[2]].z, 
                max[0], max[1], max[2], 
                fmax[0], fmax[1], fmax[2], 
                coords[max[0]].x, coords[max[0]].y, coords[max[0]].z, 
                coords[max[1]].x, coords[max[1]].y, coords[max[1]].z, 
                coords[max[2]].x, coords[max[2]].y, coords[max[2]].z);

    /* now count the number of normals pointing "away from" the center */
    /* fixed directions - inconsistent usage       07 Apr 2004 [rickr] */
    match[0] = norms[min[0]].xyz[0] < 0;
    match[1] = norms[min[1]].xyz[1] < 0;
    match[2] = norms[min[2]].xyz[2] < 0;

    match[3] = norms[max[0]].xyz[0] > 0;
    match[4] = norms[max[1]].xyz[1] > 0;
    match[5] = norms[max[2]].xyz[2] > 0;

    if ( sopt->debug > 1 )
        fprintf(stderr,"-- matches[0..5] = (%d, %d, %d,  %d, %d, %d)\n",
                match[0], match[1], match[2], match[3], match[4], match[5] );

    ncount = 0;
    for ( c = 0; c < 6; c++ )
        if ( match[c] )
            ncount++;

    /* do we fail? */
    if ( (ncount >= 2) && (ncount <= 4) )
    {
        fprintf(stderr, "** cannot determine directions for normals:\n"
                "   To proceed, use one of -keep_norm_dir/-reverse_norm_dir.\n"
                "   \n"
                "   It is ~%d%% likely that you will want to negate the\n"
                "   normal length in '-norm_len'\n",
                (int)(100*ncount/6.0) );
        RETURN(-1);
    }

    /* or do we just warn the user? */
    if ( (ncount == 1) || (ncount == 5) )
        fprintf(stderr,"** warning: only 83%% sure of direction of normals\n");
    
    /* do we need to reverse the direction? */
    if ( ncount < 2 )
    {
        fprintf(stderr,"-- reversing direction of normals\n");
        sopt->norm_len *= -1.0;
    }

    RETURN(0);
}


/*----------------------------------------------------------------------
 * copy_surfaces - fill SUMA_surface structures
 *
 *
 *
 * return -1 : on error
 *         0 : on success
 *----------------------------------------------------------------------
*/
int copy_surfaces ( v2s_opts_t * sopt, v2s_param_t * p, int nsurf )
{
    SUMA_SurfaceObject ** so;
    float                 radius[V2S_MAX_SURFS];
    int                   rv, sindex;

ENTRY("copy_surfaces");

    if ( sopt == NULL || p == NULL || nsurf < 0 )
    {
        fprintf( stderr, "** anl: bad params (%p,%p,%d)\n", sopt, p, nsurf );
        RETURN(-1);
    }

    /* create a temporary list of surface pointers */
    so = (SUMA_SurfaceObject **)calloc(nsurf, sizeof(SUMA_SurfaceObject *));
    if ( so == NULL )
    {
        fprintf( stderr, "** anl: failed to alloc %d surf pointers\n", nsurf );
        RETURN(-1);
    }

    if ( (rv = get_mappable_surfs( so, nsurf, sopt->debug )) != nsurf )
    {
        fprintf( stderr, "** found %d mappable surfaces (but expected %d)\n",
                 rv, nsurf );
        free(so);
        RETURN(-1);
    }

    /* fill SUMA_surface structs */

    p->nsurf     = nsurf;
    for ( sindex = 0; sindex < nsurf; sindex++ )
    {
        if ( suma2afni_surf( sopt, p, so[sindex], sindex ) )
            RETURN(-1);
        surf_ave_radius(radius+sindex, so[sindex], sopt->debug);
    }

    if ( sopt->debug ) fprintf(stderr,"-- surfaces converted: suma to afni\n");

    free(so);

    RETURN(0);
}


/*----------------------------------------------------------------------
 * suma2afni_surf      - convert surface from SUMA to AFNI structures
 *----------------------------------------------------------------------
*/
int suma2afni_surf(v2s_opts_t * sopt, v2s_param_t * p, SUMA_SurfaceObject * so,
                   int sindex)
{
    SUMA_surface * sp;
    float        * fp;
    int            node;

ENTRY("suma2afni_surf");

    if ( !sopt || !p || !so || sindex < 0 || sindex >= V2S_MAX_SURFS )
    {
        fprintf(stderr,"** s2as: bad params (%p,%p,%p,%d)\n",sopt,p,so,sindex);
        RETURN(-1);
    }

    sp = p->surf + sindex;

    sp->type      = SUMA_SURFACE_TYPE;  /* always (for a SUMA_surface)       */
    sp->num_ixyz  = so->N_Node;         /* number of nodes                   */
    sp->nall_ixyz = so->N_Node;         /* allocate later in this function   */
    sp->num_ijk   = 0;                  /* no triangles                      */
    sp->nall_ijk  = 0;                  /* twice as many triangles as before */

    sp->seq       = 1;                  /* surfaces are sequential           */
    sp->seqbase   = 0;                  /*     - from 0                      */
    sp->sorted    = 1;                  /*     - and therefore, a sorted lot */

    sp->ixyz      = NULL;               /* just for the moment ...           */
    sp->norm      = NULL;

    if ( !so->NodeList )
    {
        fprintf(stderr,"** s2as: missing surface NodeList for '%s'\n",
                so->Label ? so->Label: "<no label>");
        RETURN(1);
    }

    if ( sopt->use_norms )
    {
        if ( so->NodeNormList )
        {
            sp->norm = (THD_fvec3 *)malloc(sp->num_ixyz * sizeof(THD_fvec3));
            if ( !sp->norm )
            {
                fprintf(stderr,"** s2as: cannot allocate %d THD_fvec3's\n",
                        sp->num_ixyz);
                RETURN(1);
            }

            if ( sopt->debug > 1 )
                fprintf(stderr,"++ filling in norms for surf # %d (%d bytes)\n",
                        sindex, (int)(sp->num_ixyz * sizeof(THD_fvec3)));

            fp = so->NodeNormList;
            for ( node = 0; node < sp->num_ixyz; node++ )
            {
                sp->norm[node].xyz[0] = *fp++;
                sp->norm[node].xyz[1] = *fp++;
                sp->norm[node].xyz[2] = *fp++;
            }
        }
        else
        {
            fprintf(stderr,"** missing normals for surface # %d, '%s'\n",
                    sindex, so->Label ? so->Label: "<no label>");
        }
    }

    sp->ixyz = (SUMA_ixyz *)malloc(sp->num_ixyz * sizeof(SUMA_ixyz));

    if ( !sp->ixyz )
    {
        fprintf(stderr,"** failed to allocate %d SUMA_ixyz\n",sp->num_ixyz);
        if (sp->norm) free(sp->norm);
        RETURN(1);
    }
    else if ( sopt->debug > 1 )
        fprintf(stderr,"++ s2as: allocated %d SUMA_ixyz nodes (%d bytes)\n",
                sp->num_ixyz, (int)(sp->num_ixyz*sizeof(SUMA_ixyz)));

    fp = so->NodeList;
    for ( node = 0; node < sp->num_ixyz; node++ )
    {
        sp->ixyz[node].id = node;
        sp->ixyz[node].x  = *fp++;
        sp->ixyz[node].y  = *fp++;
        sp->ixyz[node].z  = *fp++;
    }

    sp->xbot = so->MinDims[0];
    sp->ybot = so->MinDims[1];
    sp->zbot = so->MinDims[2];

    sp->xtop = so->MaxDims[0];
    sp->ytop = so->MaxDims[1];
    sp->ztop = so->MaxDims[2];

    sp->xcen = so->Center[0];
    sp->ycen = so->Center[1];
    sp->zcen = so->Center[2];

    if (so->idcode_str)
    {
        strncpy(sp->idcode, so->idcode_str, 31);
        sp->idcode[31] = '\0';
    }
    else
        UNIQ_idcode_fill(sp->idcode);

    sp->idcode_ldp[0]  = '\0';  /* maybe assign these ... */
    sp->idcode_dset[0] = '\0';

    strncpy(sp->label, so->Label, 63);  sp->label[63] = '\0';
    if (so->LocalDomainParent && *so->LocalDomainParent)
    {
        strncpy(sp->label_ldp, so->LocalDomainParent, 63);
        sp->label[63] = '\0';
    }
    else
        strcpy(sp->label_ldp, "no_LDP_label");

    sp->vv = NULL;      /* no mappings, for now */
    sp->vn = NULL;

    RETURN(0);
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
    int                  count, socount = 0;

ENTRY("get_mappable_surfs");

    if ( slist == NULL )
    {
        fprintf( stderr, "** gms: missing slist!\n" );
        RETURN(-1);
    }

    for ( count = 0; count < SUMAg_N_DOv; count++ )
    {
        if ( ! SUMA_isSO(SUMAg_DOv[count]) )
            continue;

        so = (SUMA_SurfaceObject *)SUMAg_DOv[count].OP;

        if ( ! so->AnatCorrect )
        {
            if ( debug )
                fprintf(stderr,"-- surf #%d '%s', anat not correct, skipping\n",
                        socount, CHECK_NULL_STR(so->Label));
            if ( debug > 1 )
                fprintf(stderr,"** consider adding the following to the "
                               "surface definition in the spec file:\n"
                               "       Anatomical = Y\n");
            continue;
        }

        if ( debug > 1 )
            fprintf( stderr, "\n---------- surface #%d '%s' -----------\n",
                     socount, CHECK_NULL_STR(so->Label) );
        if ( debug > 2 ) SUMA_Print_Surface_Object( so, stderr );

        if ( socount < how_many )       /* store a good surface */
            slist[socount] = so;

        socount++;
    }

    if ( debug > 1 )
        fprintf( stderr, "++ found %d mappable surfaces\n", socount );

    RETURN(socount);
}


/*----------------------------------------------------------------------
 * set_smap_opts  - fill v2s_opts_t struct
 *
 * return  0 : success
 *        -1 : error condition
 *----------------------------------------------------------------------
*/
int set_smap_opts( opts_t * opts, v2s_param_t * p, v2s_opts_t * sopt,
                   int argc, char * argv[] )
{
    int nsurf = 1;

ENTRY("set_smap_opts");

    memset( sopt, 0, sizeof(*sopt) );

    if ( (sopt->map = check_map_func( opts->map_str )) == E_SMAP_INVALID )
        RETURN(-1);

    /* set defaults before checking map type */

    sopt->gp_index      = opts->gp_index;
    sopt->debug         = opts->debug;
    sopt->dnode         = opts->dnode;
    sopt->no_head       = opts->no_head;
    sopt->skip_cols     = opts->skip_cols;

    sopt->first_node    = opts->first_node > 0 ? opts->first_node : 0;
    sopt->last_node     = opts->last_node  > 0 ? opts->last_node  : 0;
    if ( sopt->first_node > sopt->last_node )
    {
        fprintf(stderr, "** error: -first_node (%d) > -last_node (%d)\n",
                sopt->first_node, sopt->last_node);
        RETURN(1);
    }

    sopt->use_norms = opts->use_norms;
    sopt->norm_len  = opts->norm_len;
    sopt->norm_dir  = opts->norm_dir;
    sopt->f_index   = V2S_INDEX_VOXEL;       /* default is "voxel" */

    if ( (opts->f_index_str != NULL) &&
         (!strncmp(opts->f_index_str, "node", 4)) )
            sopt->f_index = V2S_INDEX_NODE;

    if ( opts->f_steps <= V2S_M2_STEPS_DEFAULT )        /* default is 2    */
        sopt->f_steps = V2S_M2_STEPS_DEFAULT;
    else
        sopt->f_steps = opts->f_steps;

    sopt->f_p1_fr      = opts->f_p1_fr;      /* copy fractions & distances */
    sopt->f_pn_fr      = opts->f_pn_fr;
    sopt->f_p1_mm      = opts->f_p1_mm;
    sopt->f_pn_mm      = opts->f_pn_mm;
    sopt->outfile_1D   = opts->outfile_1D;
    sopt->outfile_niml = opts->outfile_niml;
    sopt->segc_file    = opts->seg_coords_file;
    sopt->oob          = opts->oob;             /* out of bounds info */
    sopt->oom          = opts->oom;             /* out of bounds info */

    if ( sopt->oom.show && !p->cmask )
    {
        fprintf(stderr,"** '-cmask' option is required with '-oom_value'\n");
        RETURN(1);
    }

    /* great, now my switch is ineffective - save for later */
    switch (sopt->map)
    {
        default:
            break;

        case E_SMAP_AVE:
        case E_SMAP_COUNT:
        case E_SMAP_MAX:
        case E_SMAP_MAX_ABS:
        case E_SMAP_MIN:
        case E_SMAP_MASK2:
        case E_SMAP_SEG_VALS:
        case E_SMAP_MEDIAN:
        case E_SMAP_MODE:
            nsurf = 2;
            break;

        case E_SMAP_MIDPT:      /* continue to SMAP_MASK */
            nsurf = 2;

        case E_SMAP_MASK:
            if (sopt->f_steps != V2S_M2_STEPS_DEFAULT)
            {
                fprintf(stderr,"** -f_steps option not valid\n");
                RETURN(-1);
            }

            /* we will only use the first point in the computation */
            sopt->f_steps = 1;
            break;
    }

    if ( (nsurf == 2) && !opts->snames[1] && !opts->use_norms )
    {
        fprintf(stderr, "** function '%s' requires 2 surfaces\n",
                        gv2s_map_names[sopt->map]);
        RETURN(-1);
    }

    p->over_steps = v2s_vals_over_steps(sopt->map);

    /* include command options for history     7 Aug 2006 [rickr] */
    sopt->cmd.fake = 0;  sopt->cmd.argc = argc;  sopt->cmd.argv = argv;

    if ( opts->debug > 0 )
        disp_v2s_opts_t( "++ smap opts set :", sopt );

    RETURN(0);
}


/*----------------------------------------------------------------------
 * free memory, close output file
 *----------------------------------------------------------------------
*/
int final_clean_up ( opts_t * opts, v2s_param_t * p, SUMA_SurfSpecFile * spec )
{
    int surf;
ENTRY("final_clean_up");

    for ( surf = 0; surf < p->nsurf; surf++ )
    {
        if ( opts->debug > 2    ) fprintf(stderr,"-- freeing nodes[%d]\n",surf);
        if ( p->surf[surf].ixyz ) free(p->surf[surf].ixyz);
        if ( opts->debug > 2    ) fprintf(stderr,"-- freeing norms[%d]\n",surf);
        if ( p->surf[surf].norm ) free(p->surf[surf].norm);
    }

    if ( p->cmask ) free(p->cmask);

    if ( opts->debug > 2 ) fprintf(stderr,"-- freeing DOV\n");
    if ( ( SUMAg_DOv != NULL ) &&
         ( SUMA_Free_Displayable_Object_Vect(SUMAg_DOv, SUMAg_N_DOv) == 0 ) )
        fprintf(stderr, "** failed SUMA_Free_Displayable_Object_Vect()\n" );

    if ( opts->debug > 2 ) fprintf(stderr,"-- freeing SVSV\n");
    if ( ( SUMAg_SVv != NULL ) &&
         ( SUMA_Free_SurfaceViewer_Struct_Vect(SUMAg_SVv, SUMAg_N_SVv) == 0 ) )
        fprintf( stderr, "** failed SUMA_Free_SurfaceViewer_Struct_Vect()\n" );

    if ( opts->debug > 2 ) fprintf(stderr,"-- freeing CF\n");
    if ( ( SUMAg_CF != NULL ) && ( SUMA_Free_CommonFields(SUMAg_CF) == 0 ) )
        fprintf( stderr, "** failed SUMA_Free_CommonFields()\n" );

    if ( opts->debug > 1 ) fprintf(stderr,"-- freeing complete\n");

    RETURN(0);
}


/*----------------------------------------------------------------------
 * read surfaces (much stolen from SUMA_suma.c - thanks Ziad!)
 *----------------------------------------------------------------------
*/
int read_surf_files ( opts_t * opts, SUMA_SurfSpecFile * spec )
{
    int debug, rv;                                      /* v3.5 [rickr] */
    
ENTRY("read_surf_files");

    debug = (opts->debug > 2);

    if ( debug )
        fputs( "-- SUMA_Create_CommonFields()...\n", stderr );

    /* initialize common fields struct */
    SUMAg_CF = SUMA_Create_CommonFields();

    if ( SUMAg_CF == NULL )
    {
        fprintf( stderr, "** failed SUMA_Create_CommonFields(), exiting...\n" );
        RETURN(-1);
    }

    /* for SUMA type notifications */
    if ( opts->debug > 3 )
    {
        SUMAg_CF->MemTrace = 1;

        if ( opts->debug > 4 )
            SUMAg_CF->InOut_Notify = 1;
    }

    if ( debug )
        fputs( "-- SUMA_Alloc_DisplayObject_Struct()...\n", stderr );

    SUMAg_DOv = SUMA_Alloc_DisplayObject_Struct(SUMA_MAX_DISPLAYABLE_OBJECTS);

    if ( debug )        /* can't SUMA_ShowSpecStruct() yet    - v3.9 */
        fputs( "-- SUMA_Read_SpecFile()...\n", stderr );

    if ( SUMA_Read_SpecFile( opts->spec_file, spec) == 0 )
    {
        fprintf( stderr, "** failed SUMA_Read_SpecFile(), exiting...\n" );
        RETURN(-1);
    }

    rv = SUMA_spec_select_surfs(spec, opts->snames, V2S_MAX_SURFS, opts->debug);
    if ( rv < 1 )
    {
        if ( rv == 0 )
            fprintf(stderr,"** no named surfaces found in spec file\n");
        RETURN(-1);
    }

    if ( debug )
        SUMA_ShowSpecStruct(spec, stderr, opts->debug > 2 ? 3 : 1);

    if ( SUMA_spec_set_map_refs(spec, opts->debug) != 0 )
        RETURN(-1);

    /* make sure only group was read from spec file */
    if ( spec->N_Groups != 1 )
    {
        fprintf( stderr,"** error: N_Groups <%d> must be 1 in spec file <%s>\n",
                 spec->N_Groups, opts->spec_file );
        RETURN(-1);
    }

    if ( debug )
        fputs( "-- SUMA_LoadSpec_eng()...\n", stderr );

    /* actually load the surface(s) from the spec file */
    if (SUMA_LoadSpec_eng(spec,SUMAg_DOv,&SUMAg_N_DOv,opts->sv_file,debug,
             SUMAg_CF->DsetList) == 0)     /* DsetList   26 Mar 2004 [ziad] */
    {
        fprintf( stderr, "** error: failed SUMA_LoadSpec_eng(), exiting...\n" );
        RETURN(-1);
    }

    if ( opts->debug > 1 )
        fprintf(stderr, "++ %d surfaces loaded.\n", spec->N_Surfs );

    RETURN(0);
}


/*----------------------------------------------------------------------
 * init_options - fill opts struct, display help
 *----------------------------------------------------------------------
*/
int init_options ( opts_t * opts, int argc, char * argv [] )
{
    int ac, ind;

ENTRY("init_options");

    if ( argc < 2 )
    {
        usage( PROG_NAME, V2S_USE_LONG );
        RETURN(-1);
    }

    /* clear out the options structure */
    memset( opts, 0, sizeof( opts_t) );
    opts->gpar_file       = NULL;
    opts->outfile_1D      = NULL;
    opts->outfile_niml    = NULL;
    opts->seg_coords_file = NULL;
    opts->spec_file       = NULL;
    opts->sv_file         = NULL;
    opts->cmask_cmd       = NULL;
    opts->map_str         = NULL;
    opts->snames[0]       = NULL;
    opts->snames[1]       = NULL;
    opts->f_index_str     = NULL;

    opts->gp_index        = -1;            /* means none: use all       */
    opts->norm_len        = 1.0;           /* init to 1.0 millimeter    */
    opts->dnode           = -1;            /* init to something invalid */

    for ( ac = 1; ac < argc; ac++ )
    {
        /* alphabetical... */
        if ( ! strncmp(argv[ac], "-cmask", 6) )
        {
            if ( (ac+1) >= argc )
            {
                fputs( "option usage: -cmask COMMAND\n\n", stderr );
                usage( PROG_NAME, V2S_USE_SHORT );
                RETURN(-1);
            }

            opts->cmask_cmd = argv[++ac];
        }
        else if ( ! strncmp(argv[ac], "-debug", 6) )
        {
            if ( (ac+1) >= argc )
            {
                fputs( "option usage: -debug LEVEL\n\n", stderr );
                usage( PROG_NAME, V2S_USE_SHORT );
                RETURN(-1);
            }

            opts->debug = atoi(argv[++ac]);
            if ( opts->debug < 0 || opts->debug > V2S_DEBUG_MAX_LEV )
            {
                fprintf( stderr, "bad debug level <%d>, should be in [0,%d]\n",
                        opts->debug, V2S_DEBUG_MAX_LEV );
                usage( PROG_NAME, V2S_USE_SHORT );
                RETURN(-1);
            }
        }
        else if ( ! strncmp(argv[ac], "-dnode", 6) )
        {
            if ( (ac+1) >= argc )
            {
                fputs( "option usage: -dnode NODE_NUM\n\n", stderr );
                usage( PROG_NAME, V2S_USE_SHORT );
                RETURN(-1);
            }

            opts->dnode = atoi(argv[++ac]);
        }
        else if ( ! strncmp(argv[ac], "-f_index", 7) )
        {
            if ( (ac+1) >= argc )
            {
                fputs( "option usage: -f_index INDEX_TYPE\n\n", stderr );
                usage( PROG_NAME, V2S_USE_SHORT );
                RETURN(-1);
            }

            opts->f_index_str = argv[++ac];
        }
        else if ( ! strncmp(argv[ac], "-f_keep_surf_order", 9) )
        {
            /*  opts->f_kso = 1;                v3.8 */

            fprintf(stderr,"** the -f_keep_surf_order option is depreciated\n"
                    "   in favor of -surf_A and -surf_B (version 3.8)\n");
            RETURN(-1);
        }
        else if ( ! strncmp(argv[ac], "-f_p1_fr", 9) )
        {
            if ( (ac+1) >= argc )
            {
                fputs( "option usage: -f_p1_fr FRACTION\n\n", stderr );
                usage( PROG_NAME, V2S_USE_SHORT );
                RETURN(-1);
            }

            opts->f_p1_fr = atof(argv[++ac]);
        }
        else if ( ! strncmp(argv[ac], "-f_pn_fr", 9) )
        {
            if ( (ac+1) >= argc )
            {
                fputs( "option usage: -f_pn_fr FRACTION\n\n", stderr );
                usage( PROG_NAME, V2S_USE_SHORT );
                RETURN(-1);
            }

            opts->f_pn_fr = atof(argv[++ac]);
        }
        else if ( ! strncmp(argv[ac], "-f_p1_mm", 9) )
        {
            if ( (ac+1) >= argc )
            {
                fputs( "option usage: -f_p1_mm DISTANCE\n\n", stderr );
                usage( PROG_NAME, V2S_USE_SHORT );
                RETURN(-1);
            }

            opts->f_p1_mm = atof(argv[++ac]);
        }
        else if ( ! strncmp(argv[ac], "-f_pn_mm", 9) )
        {
            if ( (ac+1) >= argc )
            {
                fputs( "option usage: -f_pn_mm DISTANCE\n\n", stderr );
                usage( PROG_NAME, V2S_USE_SHORT );
                RETURN(-1);
            }

            opts->f_pn_mm = atof(argv[++ac]);
        }
        else if ( ! strncmp(argv[ac], "-f_steps", 7) )
        {
            if ( (ac+1) >= argc )
            {
                fputs( "option usage: -f_steps NUM_STEPS\n\n", stderr );
                usage( PROG_NAME, V2S_USE_SHORT );
                RETURN(-1);
            }

            opts->f_steps = atoi(argv[++ac]);
        }
        else if ( ! strncmp(argv[ac], "-first_node", 11) )
        {
            if ( (ac+1) >= argc )
            {
                fputs( "option usage: -first_node NODE_INDEX\n\n", stderr );
                usage( PROG_NAME, V2S_USE_SHORT );
                RETURN(-1);
            }

            opts->first_node = atoi(argv[++ac]);
        }
        else if ( ! strncmp(argv[ac], "-gp_index", 7) )
        {
            if ( (ac+1) >= argc )
            {
                fputs( "option usage: -gp_index BRICK_INDEX\n\n", stderr );
                usage( PROG_NAME, V2S_USE_SHORT );
                RETURN(-1);
            }

            opts->gp_index = atoi(argv[++ac]);
        }
        else if ( ! strncmp(argv[ac], "-help", 5) )
        {
            usage( PROG_NAME, V2S_USE_LONG );
            RETURN(-1);
        }
        else if ( ! strncmp(argv[ac], "-hist", 5) )
        {
            usage( PROG_NAME, V2S_USE_HIST );
            RETURN(-1);
        }
        else if ( ! strncmp(argv[ac], "-v2s_hist", 9) )
        {
            usage( PROG_NAME, V2S_USE_LIB_HIST );
            RETURN(-1);
        }
        else if ( ! strncmp(argv[ac], "-grid_parent", 5) ||
                  ! strncmp(argv[ac], "-inset", 6)       ||
                  ! strncmp(argv[ac], "-input", 6) )
        {
            if ( (ac+1) >= argc )
            {
                fputs( "option usage: -grid_parent INPUT_DSET\n\n", stderr );
                usage( PROG_NAME, V2S_USE_SHORT );
                RETURN(-1);
            }

            opts->gpar_file = argv[++ac];
        }
        else if ( ! strncmp(argv[ac], "-keep_norm_dir", 14) )
        {
            opts->norm_dir = V2S_NORM_KEEP;
        }
        else if ( ! strncmp(argv[ac], "-last_node", 11) )
        {
            if ( (ac+1) >= argc )
            {
                fputs( "option usage: -last_node NODE_INDEX\n\n", stderr );
                usage( PROG_NAME, V2S_USE_SHORT );
                RETURN(-1);
            }

            opts->last_node = atoi(argv[++ac]);
        }
        else if ( ! strncmp(argv[ac], "-map_func", 4) )  /* mapping function */
        {
            if ( (ac+1) >= argc )
            {
                fputs( "option usage: -map_func FUNCTION\n\n", stderr );
                RETURN(-1);
            }

            opts->map_str = argv[++ac];     /* store user string for now */
        }
        else if ( ! strncmp(argv[ac], "-no_headers", 5) )
        {
            opts->no_head = 1;
        }
        else if ( ! strncmp(argv[ac], "-norm_len", 9) )
        {
            if ( (ac+1) >= argc )
            {
                fputs( "option usage: -norm_len LENGTH\n\n", stderr );
                usage( PROG_NAME, V2S_USE_SHORT );
                RETURN(-1);
            }

            opts->norm_len = atof(argv[++ac]);
        }
        else if ( ! strncmp(argv[ac], "-oob_index", 8) )
        {
            if ( (ac+1) >= argc )
            {
                fputs( "option usage: -oob_index INDEX_VALUE\n\n", stderr );
                usage( PROG_NAME, V2S_USE_SHORT );
                RETURN(-1);
            }

            opts->oob.show  = 1;
            opts->oob.index = atoi(argv[++ac]);
        }
        else if ( ! strncmp(argv[ac], "-oob_value", 8) )
        {
            if ( (ac+1) >= argc )
            {
                fputs( "option usage: -oob_value VALUE\n\n", stderr );
                usage( PROG_NAME, V2S_USE_SHORT );
                RETURN(-1);
            }

            opts->oob.show  = 1;
            opts->oob.value = atof(argv[++ac]);
        }
        else if ( ! strncmp(argv[ac], "-oom_value", 8) )
        {
            if ( (ac+1) >= argc )
            {
                fputs( "option usage: -oob_value VALUE\n\n", stderr );
                usage( PROG_NAME, V2S_USE_SHORT );
                RETURN(-1);
            }

            opts->oom.show  = 1;
            opts->oom.value = atof(argv[++ac]);
        }
        else if ( ! strncmp(argv[ac], "-out_1D", 7) )
        {
            if ( (ac+1) >= argc )
            {
                fputs( "option usage: -out_1D OUTPUT_FILE\n\n", stderr );
                usage( PROG_NAME, V2S_USE_SHORT );
                RETURN(-1);
            }

            opts->outfile_1D = argv[++ac];
        }
        else if ( ! strncmp(argv[ac], "-out_niml", 7) )
        {
            if ( (ac+1) >= argc )
            {
                fputs( "option usage: -out_niml OUTPUT_FILE\n\n", stderr );
                usage( PROG_NAME, V2S_USE_SHORT );
                RETURN(-1);
            }

            opts->outfile_niml = argv[++ac];
            ind = strlen(opts->outfile_niml);
            /* make sure the filename ends with .niml.dset     4 Nov 2008 */
            if ( ind > 0 && (ind < 11 ||
                             strcmp(opts->outfile_niml+ind-10, ".niml.dset")) )
            {
                fputs( "** -out_niml name must end in .niml.dset\n\n",stderr);
                RETURN(-1);
            }
        }
        else if ( ! strncmp(argv[ac], "-reverse_norm_dir", 8) )
        {
            opts->norm_dir = V2S_NORM_REVERSE;
        }
        else if ( ! strncmp(argv[ac], "-save_seg_coords", 9) ) /* 30 Jun 2006 */
        {
            if ( (ac+1) >= argc )
            {
                fputs( "option usage: -save_seg_coords OUTPUT_FILE\n\n",stderr);
                usage( PROG_NAME, V2S_USE_SHORT );
                RETURN(-1);
            }

            opts->seg_coords_file = argv[++ac];
        }
        /* added -outcols_* options  23 Aug 2006 [rickr] */
        else if ( ! strncmp(argv[ac], "-outcols_afni_NSD", 13) )
            opts->skip_cols = V2S_SKIP_ALL ^ V2S_SKIP_NODES;
        else if ( ! strncmp(argv[ac], "-outcols_NSD_format", 12) )
            opts->skip_cols = V2S_SKIP_ALL ^ V2S_SKIP_NODES ^ V2S_SKIP_VALS;
        else if ( ! strncmp(argv[ac], "-skip_col_non_results", 15) ||
                  ! strncmp(argv[ac], "-outcols_results", 12) )
            opts->skip_cols |= (V2S_SKIP_ALL & ~V2S_SKIP_VALS);
        else if ( ! strncmp(argv[ac], "-skip_col_nodes", 13) )
            opts->skip_cols |= V2S_SKIP_NODES;
        else if ( ! strncmp(argv[ac], "-skip_col_1dindex", 12) )
            opts->skip_cols |= V2S_SKIP_VOLIND;
        else if ( ! strncmp(argv[ac], "-skip_col_i", 11) )
            opts->skip_cols |= V2S_SKIP_I;
        else if ( ! strncmp(argv[ac], "-skip_col_j", 11) )
            opts->skip_cols |= V2S_SKIP_J;
        else if ( ! strncmp(argv[ac], "-skip_col_k", 11) )
            opts->skip_cols |= V2S_SKIP_K;
        else if ( ! strncmp(argv[ac], "-skip_col_vals", 13) )
            opts->skip_cols |= V2S_SKIP_NVALS;
        else if ( ! strncmp(argv[ac], "-skip_col_results", 13) || 
                  ! strncmp(argv[ac], "-outcols_1_result", 14) )
            opts->skip_cols |= V2S_SKIP_VALS;
        else if ( ! strncmp(argv[ac], "-spec", 3) )
        {
            if ( (ac+1) >= argc )
            {
                fputs( "option usage: -spec SPEC_FILE\n\n", stderr );
                usage( PROG_NAME, V2S_USE_SHORT );
                RETURN(-1);
            }

            opts->spec_file = argv[++ac];
        }
        else if ( ! strncmp(argv[ac], "-surf_", 6) )
        {
            if ( (ac+1) >= argc )
            {
                fputs( "option usage: -surf_X SURF_NAME\n\n", stderr );
                usage( PROG_NAME, V2S_USE_SHORT );
                RETURN(-1);
            }
            ind = argv[ac][6] - 'A';
            if ( (ind < 0) || (ind >= V2S_MAX_SURFS) )
            {
                fprintf(stderr,"** -surf_X option: '%s' out of range,\n"
                        "   use one of '-surf_A' through '-surf_%c'\n",
                        argv[ac], 'A'+V2S_MAX_SURFS-1);
                RETURN(-1);
            }

            opts->snames[ind] = argv[++ac];
        }
        else if ( ! strncmp(argv[ac], "-sv", 3) )
        {
            if ( (ac+1) >= argc )
            {
                fputs( "option usage: -sv SURFACE_VOLUME\n\n", stderr );
                usage( PROG_NAME, V2S_USE_SHORT );
                RETURN(-1);
            }

            opts->sv_file = argv[++ac];
        }
        else if ( ! strncmp(argv[ac], "-use_norms", 5) )
        {
            opts->use_norms = 1;
        }
        else if ( ! strncmp(argv[ac], "-version", 2) )
        {
            usage( PROG_NAME, V2S_USE_VERSION );
            RETURN(-1);
        }
        else     /* invalid option */
        {
            fprintf( stderr, "invalid option <%s>\n", argv[ac] );
            usage( PROG_NAME, V2S_USE_SHORT );
            RETURN(-1);
        }
    }

    RETURN(0);
}

/*----------------------------------------------------------------------
 * validate_options - fill param struct from options
 *
 *     - validate datasets
 *     - validate surface
 *----------------------------------------------------------------------
*/
int validate_options ( opts_t * opts, v2s_param_t * p )
{
ENTRY("validate_options");

    memset( p, 0, sizeof(*p) );
    p->gpar  = NULL;
    p->cmask = NULL;

    if ( opts->debug > 0 )
    {
        usage( PROG_NAME, V2S_USE_VERSION );
        disp_opts_t ( "++ opts read: ", opts );
    }

    if ( check_map_func( opts->map_str ) == E_SMAP_INVALID )
        RETURN(-1);

    if ( !opts->outfile_1D && !opts->outfile_niml )
    {
        fprintf( stderr, "** missing '-out_1D OUTPUT_FILE' option\n" );
        RETURN(-1);
    }

    if ( opts->spec_file == NULL )
    {
        fprintf( stderr, "** missing '-spec_file SPEC_FILE' option\n" );
        RETURN(-1);
    }

    if ( opts->sv_file == NULL )
    {
        fprintf( stderr, "** missing '-sv SURF_VOL' option\n" );
        RETURN(-1);
    }

    if ( opts->snames[0] == NULL )
    {
        fprintf(stderr,"** missing '-surf_A SURF_NAME' option\n");
        RETURN(-1);
    }

    if ( opts->use_norms )
    {
        if ( opts->snames[1] )
        {
            fprintf(stderr,"** no '-use_norms' option with 2 surfaces\n");
            RETURN(-1);
        }
    }
    else if ( opts->norm_len != 1.0 || opts->norm_dir )
    {
        fprintf(stderr,"** options for normals requires '-use_norms'\n");
        RETURN(-1);
    }

    if ( check_outfile( opts, p ) != 0 )
        RETURN(-1);

    if ( validate_datasets( opts, p ) != 0 )
        RETURN(-1);

    if ( opts->debug > 1 )
        disp_v2s_param_t( "++ opts validated: ", p );

    RETURN(0);
}


/*----------------------------------------------------------------------
 * be sure the output file does not already exist
 *----------------------------------------------------------------------
*/
int check_outfile( opts_t * opts, v2s_param_t * p )
{
ENTRY("check_outfile");

    if ( opts == NULL || p == NULL )
        RETURN(-1);

    if ( !THD_ok_overwrite() && THD_is_file(opts->outfile_1D) )
    {
        fprintf(stderr, "** output file '%s' already exists\n",
                opts->outfile_1D);
        RETURN(-1);
    }

    if ( !THD_ok_overwrite() && THD_is_file(opts->outfile_niml) )
    {
        fprintf(stderr, "** output file '%s' already exists\n",
                opts->outfile_niml);
        RETURN(-1);
    }

    if ( !THD_ok_overwrite() && THD_is_file(opts->seg_coords_file) )
    {
        fprintf(stderr, "** segment coords output file '%s' already exists\n",
                opts->seg_coords_file);
        RETURN(-1);
    }

    RETURN(0);
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

ENTRY("check_map_func");

    if ( map_str == NULL )
    {
        fprintf( stderr, "** missing option: '-map_func FUNCTION'\n" );
        RETURN(E_SMAP_INVALID);
    }

    map = v2s_map_type( map_str );

    switch ( map )
    {
        default:
            map = E_SMAP_INVALID;
            break;

        case E_SMAP_COUNT:
        case E_SMAP_MASK2:
            fprintf( stderr, "** function '%s' coming soon ...\n",
                     gv2s_map_names[map] );
            RETURN(E_SMAP_INVALID);
            break;

        case E_SMAP_AVE:
        case E_SMAP_MASK:
        case E_SMAP_MAX:
        case E_SMAP_MAX_ABS:
        case E_SMAP_MIN:
        case E_SMAP_MIDPT:
        case E_SMAP_SEG_VALS:
        case E_SMAP_MEDIAN:
        case E_SMAP_MODE:
            break;
    }

    if ( map == E_SMAP_INVALID )
        fprintf( stderr, "** invalid map string '%s'\n", map_str );

    RETURN(map);
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
int validate_datasets( opts_t * opts, v2s_param_t * p )
{
    int ccount = 0;
ENTRY("validate_datasets");

    p->gpar = THD_open_dataset( opts->gpar_file );

    if ( !ISVALID_DSET(p->gpar) )
    {
        if ( opts->gpar_file == NULL )
            fprintf( stderr, "** error: missing '-grid_parent DSET' option\n" );
        else
            fprintf( stderr, "** error: invalid input dataset '%s'\n",
                     opts->gpar_file);
        RETURN(-1);
    }
    else if ( DSET_BRICK_TYPE(p->gpar, 0) == MRI_complex )
    {
        fprintf(stderr,
                "** failure: cannot deal with complex-valued dataset, '%s'\n",
                opts->gpar_file);
        RETURN(-1);
    }
    else if ( opts->gp_index >= DSET_NVALS(p->gpar) )
    {
        fprintf(stderr,"** error: gp_index (%d) > max grid parent index (%d)\n",
                opts->gp_index, DSET_NVALS(p->gpar)-1);
        RETURN(-1);
    }

    p->nvox = DSET_NVOX( p->gpar );

    /* -------------------------------------------------------------------- */
    /* check for cmask - casually stolen from 3dmaskdump.c (thanks, Bob! :) */

    if ( opts->cmask_cmd != NULL )
    {
        int    ncmask, clen = strlen( opts->cmask_cmd );
        char * cmd;

        /* save original cmask command, as EDT_calcmask() is destructive */
        cmd = (char *)malloc((clen + 1) * sizeof(char));
        strcpy( cmd, opts->cmask_cmd );

        p->cmask = EDT_calcmask( cmd, &ncmask, 0 );

        free( cmd );                       /* free EDT_calcmask() string */

        if ( p->cmask == NULL )
        {
            fprintf( stderr, "** failure: cannot compute mask from option:\n"
                     "   -cmask '%s'\n", opts->cmask_cmd );
            RETURN(-1);
        }
        if ( ncmask != p->nvox )
        {
            fprintf( stderr, "** error: input and cmask datasets do not have "
                     "the same dimensions\n" );
            RETURN(-1);
        }
        if ( ( ccount = THD_countmask( ncmask, p->cmask ) ) <= 0 )
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
            fprintf( stderr, " (%d voxels in mask)\n", ccount );
    }

    RETURN(0);
}

/*----------------------------------------------------------------------
 * usage  -  output usage information
 *
 * V2S_USE_SHORT        - display brief output
 * V2S_USE_LONG         - display long output
 * V2S_USE_VERSION      - show the VERSION of the program
 *----------------------------------------------------------------------
*/
int usage ( char * prog, int level )
{
ENTRY("usage");

    if ( level == V2S_USE_SHORT )
    {
        fprintf( stderr,
                 "usage: %s [options] -spec SPEC_FILE -sv SURF_VOL "
                                    " -grid_parent AFNI_DSET\n"
                 "usage: %s -help\n",
                 prog, prog );
    }
    else if ( level == V2S_USE_LONG )
    {
        printf(
            "\n"
            "%s - map data from a volume domain to a surface domain\n"
            "\n"
            "  usage: %s [options] -spec SPEC_FILE -sv SURF_VOL \\\n"
            "                    -grid_parent AFNI_DSET -map_func MAP_FUNC\n"
            "\n"
            "This program is used to map data values from an AFNI volume\n"
            "dataset to a surface dataset.  A filter may be applied to the\n"
            "volume data to produce the value(s) for each surface node.\n"
            "\n"
            "The surface and volume domains are spacially matched via the\n"
            "'surface volume' AFNI dataset.  This gives each surface node xyz\n"
            "coordinates, which are then matched to the input 'grid parent'\n"
            "dataset.  This grid parent is an AFNI dataset containing the\n"
            "data values destined for output.\n"
            "\n"
            "Typically, two corresponding surfaces will be input (via the\n"
            "spec file and the '-surf_A' and '-surf_B' options), along with\n"
            "a mapping function and relevant options.  The mapping function\n"
            "will act as a filter over the values in the AFNI volume.\n"
            "\n"
            "Note that an alternative to using a second surface with the\n"
            "'-surf_B' option is to define the second surface by using the\n"
            "normals from the first surface.  By default, the second surface\n"
            "would be defined at a distance of 1mm along the normals, but the\n"
            "user may modify the applied distance (and direction).  See the\n"
            "'-use_norms' and '-norm_len' options for more details.\n"
            "\n"
            "For each pair of corresponding surface nodes, let NA be the node\n"
            "on surface A (such as a white/grey boundary) and NB be the\n"
            "corresponding node on surface B (such as a pial surface).  The\n"
            "filter is applied to the volume data values along the segment\n"
            "from NA to NB (consider the average or maximum as examples of\n"
            "filters).\n"
            "\n"
            "Note: if either endpoint of a segment is outside the grid parent\n"
            "      volume, that node (pair) will be skipped.\n"
            "\n"
            "Note: surface A corresponds to the required '-surf_A' argument,\n"
            "      while surface B corresponds to '-surf_B'.\n"
            "\n",
            prog, prog);

        printf(
            "By default, this segment only consists of the endpoints, NA and\n"
            "NB (the actual nodes on the two surfaces).  However the number\n"
            "of evenly spaced points along the segment may be specified with\n"
            "the -f_steps option, and the actual locations of NA and NB may\n"
            "be altered with any of the -f_pX_XX options, covered below.\n"
            "\n"
            "As an example, for each node pair, one could output the average\n"
            "value from some functional dataset along a segment of 10 evenly\n"
            "spaced points, where the segment endpoints are defined by the\n"
            "xyz coordinates of the nodes.  This is example 3, below.\n"
            "\n"
            "The mapping function (i.e. filter) is a required parameter to\n"
            "the program.\n"
            "\n"
            "Brief descriptions of the current mapping functions are as\n"
            "follows.  These functions are defined over a segment of points.\n"
            "\n"
            "    ave       : output the average of all voxel values along the\n"
            "                segment\n"
            "    mask      : output the voxel value for the trivial case of a\n"
            "                segment - defined by a single surface point\n"
            "    median    : output the median value from the segment\n"
            "    midpoint  : output the dataset value at the segment midpoint\n"
            "    mode      : output the mode of the values along the segment\n"
            "    max       : output the maximum volume value over the segment\n"
            "    max_abs   : output the dataset value with max abs over seg\n"
            "    min       : output the minimum volume value over the segment\n"
            "    seg_vals  : output _all_ volume values over the segment (one\n"
            "                sub-brick only)\n"
            "\n"
            );

        printf(
            "  --------------------------------------------------\n"
            "\n"
            "  examples:\n"
            "\n"
            "    1. Apply a single surface mask to output volume values over\n"
            "       each surface node.  Output is one value per sub-brick\n"
            "       (per surface node).\n"
            "\n"
            "    %s                                \\\n"
            "       -spec         fred.spec                \\\n"
            "       -surf_A       smoothwm                 \\\n"
            "       -sv           fred_anat+orig           \\\n"
            "       -grid_parent  fred_anat+orig           \\\n"
            "       -map_func     mask                     \\\n"
            "       -out_1D       fred_anat_vals.1D\n"
            "\n"
            "    2. Apply a single surface mask to output volume values over\n"
            "       each surface node.  In this case restrict input to the\n"
            "       mask implied by the -cmask option.  Supply additional\n"
            "       debug output, and more for surface node 1874\n"
            "\n"
            "    %s                                                \\\n"
            "       -spec         fred.spec                                \\\n"
            "       -surf_A       smoothwm                                 \\\n"
            "       -sv           fred_anat+orig                           \\\n"
            "       -grid_parent 'fred_epi+orig[0]'                        \\\n"
            "       -cmask       '-a fred_func+orig[2] -expr step(a-0.6)'  \\\n"
            "       -map_func     mask                                     \\\n"
            "       -debug        2                                        \\\n"
            "       -dnode        1874                                     \\\n"
            "       -out_niml     fred_epi_vals.niml.dset\n"
            "\n"
            "    3. Given a pair of related surfaces, for each node pair,\n"
            "       break the connected line segment into 10 points, and\n"
            "       compute the average dataset value over those points.\n"
            "       Since the index is nodes, each of the 10 points will be\n"
            "       part of the average.  This could be changed so that only\n"
            "       values from distinct volume nodes are considered (by\n"
            "       changing the -f_index from nodes to voxels).  Restrict\n"
            "       input voxels to those implied by the -cmask option\n"
            "       Output is one average value per sub-brick (per surface\n"
            "       node).\n"
            "\n"
            "    %s                                                \\\n"
            "       -spec         fred.spec                                \\\n"
            "       -surf_A       smoothwm                                 \\\n"
            "       -surf_B       pial                                     \\\n"
            "       -sv           fred_anat+orig                           \\\n"
            "       -grid_parent  fred_func+orig                           \\\n"
            "       -cmask        '-a fred_func+orig[2] -expr step(a-0.6)' \\\n"
            "       -map_func     ave                                      \\\n"
            "       -f_steps      10                                       \\\n"
            "       -f_index      nodes                                    \\\n"
            "       -out_niml     fred_func_ave.niml.dset\n"
            "\n"
            "    4. Similar to example 3, but restrict the output columns to\n"
            "       only node indices and values (i.e. skip 1dindex, i, j, k\n"
            "       and vals).\n"
            "\n"
            "    %s                                                \\\n"
            "       -spec         fred.spec                                \\\n"
            "       -surf_A       smoothwm                                 \\\n"
            "       -surf_B       pial                                     \\\n"
            "       -sv           fred_anat+orig                           \\\n"
            "       -grid_parent  fred_func+orig                           \\\n"
            "       -cmask        '-a fred_func+orig[2] -expr step(a-0.6)' \\\n"
            "       -map_func     ave                                      \\\n"
            "       -f_steps      10                                       \\\n"
            "       -f_index      nodes                                    \\\n"
            "       -skip_col_1dindex                                      \\\n"
            "       -skip_col_i                                            \\\n"
            "       -skip_col_j                                            \\\n"
            "       -skip_col_k                                            \\\n"
            "       -skip_col_vals                                         \\\n"
            "       -out_niml     fred_func_ave_short.niml.dset\n"
            "\n"
            "    5. Similar to example 3, but each of the node pair segments\n"
            "       has grown by 10%% on the inside of the first surface,\n"
            "       and 20%% on the outside of the second.  This is a 30%%\n"
            "       increase in the length of each segment.  To shorten the\n"
            "       node pair segment, use a '+' sign for p1 and a '-' sign\n"
            "       for pn.\n"
            "       As an interesting side note, '-f_p1_fr 0.5 -f_pn_fr -0.5'\n"
            "       would give a zero length vector identical to that of the\n"
            "       'midpoint' filter.\n"
            "\n"
            "    %s                                                \\\n"
            "       -spec         fred.spec                                \\\n"
            "       -surf_A       smoothwm                                 \\\n"
            "       -surf_B       pial                                     \\\n"
            "       -sv           fred_anat+orig                           \\\n"
            "       -grid_parent  fred_func+orig                           \\\n"
            "       -cmask        '-a fred_func+orig[2] -expr step(a-0.6)' \\\n"
            "       -map_func     ave                                      \\\n"
            "       -f_steps      10                                       \\\n"
            "       -f_index      voxels                                   \\\n"
            "       -f_p1_fr      -0.1                                     \\\n"
            "       -f_pn_fr      0.2                                      \\\n"
            "       -out_niml     fred_func_ave2.niml.dset\n"
            "\n"
            "    6. Similar to example 3, instead of computing the average\n"
            "       across each segment (one average per sub-brick), output\n"
            "       the volume value at _every_ point across the segment.\n"
            "       The output here would be 'f_steps' values per node pair,\n"
            "       though the output could again be restricted to unique\n"
            "       voxels along each segment with '-f_index voxels'.\n"
            "       Note that only sub-brick 0 will be considered here.\n"
            "\n"
            "    %s                                                \\\n"
            "       -spec         fred.spec                                \\\n"
            "       -surf_A       smoothwm                                 \\\n"
            "       -surf_B       pial                                     \\\n"
            "       -sv           fred_anat+orig                           \\\n"
            "       -grid_parent  fred_func+orig                           \\\n"
            "       -cmask        '-a fred_func+orig[2] -expr step(a-0.6)' \\\n"
            "       -map_func     seg_vals                                 \\\n"
            "       -f_steps      10                                       \\\n"
            "       -f_index      nodes                                    \\\n"
            "       -out_niml     fred_func_segvals_10.niml.dset\n"
            "\n"
            "    7. Similar to example 6, but make sure there is output for\n"
            "       every node pair in the surfaces.  Since it is expected\n"
            "       that some nodes are out of bounds (meaning that they lie\n"
            "       outside the domain defined by the grid parent dataset),\n"
            "       the '-oob_value' option is added to include a default\n"
            "       value of 0.0 in such cases.  And since it is expected\n"
            "       that some node pairs are \"out of mask\" (meaning that\n"
            "       their resulting segment lies entirely outside the cmask),\n"
            "       the '-oom_value' was added to output the same default\n"
            "       value of 0.0.\n"
            "\n"
            "    %s                                                \\\n"
            "       -spec         fred.spec                                \\\n"
            "       -surf_A       smoothwm                                 \\\n"
            "       -surf_B       pial                                     \\\n"
            "       -sv           fred_anat+orig                           \\\n"
            "       -grid_parent  fred_func+orig                           \\\n"
            "       -cmask        '-a fred_func+orig[2] -expr step(a-0.6)' \\\n"
            "       -map_func     seg_vals                                 \\\n"
            "       -f_steps      10                                       \\\n"
            "       -f_index      nodes                                    \\\n"
            "       -oob_value    0.0                                      \\\n"
            "       -oom_value    0.0                                      \\\n"
            "       -out_niml     fred_func_segvals_10_all.niml.dset\n"
            "\n"
            "    8. This is a basic example of calculating the average along\n"
            "       each segment, but where the segment is produced by only\n"
            "       one surface, along with its set of surface normals.  The\n"
            "       segments will be 2.5 mm in length.\n"
            "\n"
            "    %s                                                \\\n"
            "       -spec         fred.spec                                \\\n"
            "       -surf_A       smoothwm                                 \\\n"
            "       -sv           fred_anat+orig                           \\\n"
            "       -grid_parent  fred_anat+orig                           \\\n"
            "       -use_norms                                             \\\n"
            "       -norm_len     2.5                                      \\\n"
            "       -map_func     ave                                      \\\n"
            "       -f_steps      10                                       \\\n"
            "       -f_index      nodes                                    \\\n"
            "       -out_niml     fred_anat_norm_ave.2.5.niml.dset\n"
            "\n"
            "    9. This is the same as example 8, but where the surface\n"
            "       nodes are restricted to the range 1000..1999 via the\n"
            "       options '-first_node' and '-last_node'.\n"
            "\n"
            "    %s                                                \\\n"
            "       -spec         fred.spec                                \\\n"
            "       -surf_A       smoothwm                                 \\\n"
            "       -sv           fred_anat+orig                           \\\n"
            "       -grid_parent  fred_anat+orig                           \\\n"
            "       -first_node   1000                                     \\\n"
            "       -last_node    1999                                     \\\n"
            "       -use_norms                                             \\\n"
            "       -norm_len     2.5                                      \\\n"
            "       -map_func     ave                                      \\\n"
            "       -f_steps      10                                       \\\n"
            "       -f_index      nodes                                    \\\n"
            "       -out_niml     fred_anat_norm_ave.2.5.niml.dset\n"
            "\n"
            "   10. Create an EPI time-series surface dataset, suitable for\n"
            "       performing single-subject processing on the surface.  So\n"
            "       map a time-series onto each surface node.\n"
            "\n"
            "       Note that any time shifting (3dTshift) or registration\n"
            "       of volumes (3dvolreg) should be done before this step.\n"
            "\n"
            "       After this step, the user can finish pre-processing with\n"
            "       blurring (SurfSmooth) and scaling (3dTstat, 3dcalc),\n"
            "       before performing the regression (3dDeconvolve).\n"
            "\n"
            "    %s                                                \\\n"
            "       -spec                fred.spec                         \\\n"
            "       -surf_A              smoothwm                          \\\n"
            "       -surf_B              pial                              \\\n"
            "       -sv                  SurfVolAlndExp+orig               \\\n"
            "       -grid_parent         EPI_all_runs+orig                 \\\n"
            "       -map_func            ave                               \\\n"
            "       -f_steps             15                                \\\n"
            "       -f_index             nodes                             \\\n"
            "       -outcols_NSD_format                                    \\\n"
            "       -out_niml            EPI_runs.niml.dset\n"
            "\n"
            "  --------------------------------------------------\n",
            prog, prog, prog, prog, prog, prog, prog, prog, prog, prog );

        printf(
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
            "    -surf_A SURF_NAME      : name of surface A (from spec file)\n"
            "    -surf_B SURF_NAME      : name of surface B (from spec file)\n"
            "\n"
            "        e.g. -surf_A smoothwm\n"
            "        e.g. -surf_A lh.smoothwm\n"
            "        e.g. -surf_B lh.pial\n"
            "\n"
            "        This is used to specify which surface(s) will be used by\n"
            "        the program.  The '-surf_A' parameter is required, as it\n"
            "        specifies the first surface, whereas since '-surf_B' is\n"
            "        used to specify an optional second surface, it is not\n"
            "        required.\n"
            "\n"
            "        Note that any need for '-surf_B' may be fulfilled using\n"
            "        the '-use_norms' option.\n"
            "\n"
            "        Note that any name provided must be in the spec file,\n"
            "        uniquely matching the name of a surface node file (such\n"
            "        as lh.smoothwm.asc, for example).  Note that if both\n"
            "        hemispheres are represented in the spec file, then there\n"
            "        may be both lh.pial.asc and rh.pial.asc, for instance.\n"
            "        In such a case, 'pial' would not uniquely determine a\n"
            "        a surface, but the name 'lh.pial' would.\n"
            "\n"
            "    -sv SURFACE_VOLUME     : AFNI volume dataset\n"
            "\n"
            "        e.g. -sv fred_anat+orig\n"
            "\n"
            "        This is the AFNI dataset that the surface is mapped to.\n"
            "        This dataset is used for the initial surface node to xyz\n"
            "        coordinate mapping, in the Dicom orientation.\n"
            "\n"
            "    -grid_parent AFNI_DSET : AFNI volume dataset\n"
            "\n"
            "        e.g. -grid_parent fred_function+orig\n"
            "\n"
            "        This dataset is used as a grid and orientation master\n"
            "        for the output (i.e. it defines the volume domain).\n"
            "        It is also the source of the output data values.\n"
            "\n"
            "    -map_func MAP_FUNC     : filter for values along the segment\n"
            "\n"
            "        e.g. -map_func ave\n"
            "        e.g. -map_func ave -f_steps 10\n"
            "        e.g. -map_func ave -f_steps 10 -f_index nodes\n"
            "\n"
            "        The current mapping function for 1 surface is:\n"
            "\n"
            "          mask     : For each surface xyz location, output the\n"
            "                     dataset values of each sub-brick.\n"
            "\n"
            "        Most mapping functions are defined for 2 related input\n"
            "        surfaces (such as white/grey boundary and pial).  For\n"
            "        each node pair, the function will be performed on the\n"
            "        values from the 'grid parent dataset', and along the\n"
            "        segment connecting the nodes.\n"
            "\n"
            "          ave      : Output the average of the dataset values\n"
            "                     along the segment.\n"
            "\n"
            "          max      : Output the maximum dataset value along the\n"
            "                     connecting segment.\n"
            "\n"
            "          max_abs  : Output the dataset value with the maximum\n"
            "                     absolute value along the segment.\n"
            "\n"
            "          median   : Output the median of the dataset values\n"
            "                     along the connecting segment.\n"
            "\n"
            "          midpoint : Output the dataset value with xyz\n"
            "                     coordinates at the midpoint of the nodes.\n"
            "\n"
            "          min      : Output the minimum dataset value along the\n"
            "                     connecting segment.\n"
            "\n"
            "          mode     : Output the mode of the dataset values along\n"
            "                     the connecting segment.\n"
            "\n"
            "          seg_vals : Output all of the dataset values along the\n"
            "                     connecting segment.  Here, only sub-brick\n"
            "                     number 0 will be considered.\n"
            "\n"
            "  ------------------------------\n"
            "\n"
            "  options specific to functions on 2 surfaces:\n"
            "\n"
            "          -f_steps NUM_STEPS :\n"
            "\n"
            "                     Use this option to specify the number of\n"
            "                     evenly spaced points along each segment.\n"
            "                     The default is 2 (i.e. just use the two\n"
            "                     surface nodes as endpoints).\n"
            "\n"
            "                     e.g.     -f_steps 10\n"
            "                     default: -f_steps 2\n"
            "\n"
            "          -f_index TYPE :\n"
            "\n"
            "                     This option specifies whether to use all\n"
            "                     segment point values in the filter (using\n"
            "                     the 'nodes' TYPE), or to use only those\n"
            "                     corresponding to unique volume voxels (by\n"
            "                     using the 'voxel' TYPE).\n"
            "\n"
            "                     For instance, when taking the average along\n"
            "                     one node pair segment using 10 node steps,\n"
            "                     perhaps 3 of those nodes may occupy one\n"
            "                     particular voxel.  In this case, does the\n"
            "                     user want the voxel counted only once, or 3\n"            "                     times?  Each way makes sense.\n"
            "                     \n"
            "                     Note that this will only make sense when\n"
            "                     used along with the '-f_steps' option.\n"
            "                     \n"
            "                     Possible values are \"nodes\", \"voxels\".\n"
            "                     The default value is voxels.  So each voxel\n"
            "                     along a segment will be counted only once.\n"
            "                     \n"
            "                     e.g.  -f_index nodes\n"
            "                     e.g.  -f_index voxels\n"
            "                     default: -f_index voxels\n"
            "\n"
            "          -f_keep_surf_order :\n"
            "\n"
            "                     Depreciated.\n"
            "\n"
            "                     See required arguments -surf_A and -surf_B,\n"
            "                     above.\n"
            "\n"
            "          Note: The following -f_pX_XX options are used to alter\n"
            "                the lengths and locations of the computational\n"
            "                segments.  Recall that by default, segments are\n"
            "                defined using the node pair coordinates as\n"
            "                endpoints.  And the direction from p1 to pn is\n"
            "                from the inner surface to the outer surface.\n"
            "\n"
            "          -f_p1_mm DISTANCE :\n"
            "\n"
            "                     This option is used to specify a distance\n"
            "                     in millimeters to add to the first point of\n"
            "                     each line segment (in the direction of the\n"
            "                     second point).  DISTANCE can be negative\n"
            "                     (which would set p1 to be farther from pn\n"
            "                     than before).\n"
            "\n"
            "                     For example, if a computation is over the\n"
            "                     grey matter (from the white matter surface\n"
            "                     to the pial), and it is wished to increase\n"
            "                     the range by 1mm, set this DISTANCE to -1.0\n"
            "                     and the DISTANCE in -f_pn_mm to 1.0.\n"
            "\n"
            "                     e.g.  -f_p1_mm -1.0\n"
            "                     e.g.  -f_p1_mm -1.0 -f_pn_mm 1.0\n"
            "\n"
            "          -f_pn_mm DISTANCE :\n"
            "\n"
            "                     Similar to -f_p1_mm, this option is used\n"
            "                     to specify a distance in millimeters to add\n"
            "                     to the second point of each line segment.\n"
            "                     Note that this is in the same direction as\n"
            "                     above, from point p1 to point pn.\n"
            "                     \n"
            "                     So a positive DISTANCE, for this option,\n"
            "                     would set pn to be farther from p1 than\n"
            "                     before, and a negative DISTANCE would set\n"
            "                     it to be closer.\n"
            "\n"
            "                     e.g.  -f_pn_mm 1.0\n"
            "                     e.g.  -f_p1_mm -1.0 -f_pn_mm 1.0\n"
            "\n"
            "          -f_p1_fr FRACTION :\n"
            "\n"
            "                     Like the -f_pX_mm options above, this\n"
            "                     is used to specify a change to point p1, in\n"
            "                     the direction of point pn, but the change\n"
            "                     is a fraction of the original distance,\n"
            "                     not a pure change in millimeters.\n"
            "                     \n"
            "                     For example, suppose one wishes to do a\n"
            "                     computation based on the segments spanning\n"
            "                     the grey matter, but to add 20%% to either\n"
            "                     side.  Then use -0.2 and 0.2:\n"
            "\n"
            "                     e.g.  -f_p1_fr -0.2\n"
            "                     e.g.  -f_p1_fr -0.2 -f_pn_fr 0.2\n"
            "\n"
            "          -f_pn_fr FRACTION :\n"
            "\n"
            "                     See -f_p1_fr above.  Note again that the\n"
            "                     FRACTION is in the direction from p1 to pn.\n"
            "                     So to extend the segment past pn, this\n"
            "                     FRACTION will be positive (and to reduce\n"
            "                     the segment back toward p1, this -f_pn_fr\n"
            "                     FRACTION will be negative).\n"
            "\n"
            "                     e.g.  -f_pn_fr 0.2\n"
            "                     e.g.  -f_p1_fr -0.2 -f_pn_fr 0.2\n"
            "\n"
            "                     Just for entertainment, one could reverse\n"
            "                     the order that the segment points are\n"
            "                     considered by adjusting p1 to be pn, and\n"
            "                     pn to be p1.  This could be done by adding\n"
            "                     a fraction of 1.0 to p1 and by subtracting\n"
            "                     a fraction of 1.0 from pn.\n"
            "\n"
            "                     e.g.  -f_p1_fr 1.0 -f_pn_fr -1.0\n"
            "\n"
            "  ------------------------------\n"
        );

        printf(
            "\n"
            "  options specific to use of normals:\n"
            "\n"
            "    Notes:\n"
            "\n"
            "      o Using a single surface with its normals for segment\n"
            "        creation can be done in lieu of using two surfaces.\n"
            "\n"
            "      o Normals at surface nodes are defined by the average of\n"
            "        the normals of the triangles including the given node.\n"
            "\n"
            "      o The default normals have a consistent direction, but it\n"
            "        may be opposite of what is should be.  For this reason,\n"
            "        the direction is verified by default, and may be negated\n"
            "        internally.  See the '-keep_norm_dir' option for more\n"
            "        information.\n"
            "\n"
            "    -use_norms             : use normals for second surface\n"
            "\n"
            "        Segments are usually defined by connecting corresponding\n"
            "        node pairs from two surfaces.  With this options the\n"
            "        user can use one surface, along with its normals, to\n"
            "        define the segments.\n"
            "\n"
            "        By default, each segment will be 1.0 millimeter long, in\n"
            "        the direction of the normal.  The '-norm_len' option\n"
            "        can be used to alter this default action.\n"
            "\n"
            "    -keep_norm_dir         : keep the direction of the normals\n"
            "\n"
            "        Normal directions are verified by checking that the\n"
            "        normals of the outermost 6 points point away from the\n"
            "        center of mass.  If they point inward instead, then\n"
            "        they are negated.\n"
            "\n"
            "        This option will override the directional check, and\n"
            "        use the normals as they come.\n"
            "\n"
            "        See also -reverse_norm_dir, below.\n"
            "\n"
            "    -norm_len LENGTH       : use LENGTH for node normals\n"
            "\n"
            "        e.g.     -norm_len  3.0\n"
            "        e.g.     -norm_len -3.0\n"
            "        default: -norm_len  1.0\n"
            "\n"
            "        For use with the '-use_norms' option, this allows the\n"
            "        user to specify a directed distance to use for segments\n"
            "        based on the normals.  So for each node on a surface,\n"
            "        the computation segment will be from the node, in the\n"
            "        direction of the normal, a signed distance of LENGTH.\n"
            "\n"
            "        A negative LENGTH means to use the opposite direction\n"
            "        from the normal.\n"
            "\n"
            "        The '-surf_B' option is not allowed with the use of\n"
            "        normals.\n"
            "\n"
            "    -reverse_norm_dir      : reverse the normal directions\n"
            "\n"
            "        Normal directions are verified by checking that the\n"
            "        normals of the outermost 6 points point away from the\n"
            "        center of mass.  If they point inward instead, then\n"
            "        they are negated.\n"
            "\n"
            "        This option will override the directional check, and\n"
            "        reverse the direction of the normals as they come.\n"
            "\n"
            "        See also -keep_norm_dir, above.\n"
            "\n"
            "  ------------------------------\n"
            "\n"
            "  output options:\n"
            "\n"
            "    -debug LEVEL           :  (optional) verbose output\n"
            "\n"
            "        e.g. -debug 2\n"
            "\n"
            "        This option is used to print out status information \n"
            "        during the execution of the program.  Current levels are\n"
            "        from 0 to 5.\n"
            "\n"
            "    -first_node NODE_NUM   : skip all previous nodes\n"
            "\n"
            "        e.g. -first_node 1000\n"
            "        e.g. -first_node 1000 -last_node 1999\n"
            "\n"
            "        Restrict surface node output to those with indices as\n"
            "        large as NODE_NUM.  In the first example, the first 1000\n"
            "        nodes are ignored (those with indices from 0 through\n"
            "        999).\n"
            "\n"
            "        See also, '-last_node'.\n"
            "\n"
            "    -dnode NODE_NUM        :  (optional) node for debug\n"
            "\n"
            "        e.g. -dnode 1874\n"
            "\n"
            "        This option is used to print out status information \n"
            "        for node NODE_NUM.\n"
            "\n"
            "    -out_1D OUTPUT_FILE    : specify a 1D file for the output\n"
            "\n"
            "        e.g. -out_1D mask_values_over_dataset.1D\n"
            "\n"
            "        This is where the user will specify which file they want\n"
            "        the output to be written to.  In this case, the output\n"
            "        will be in readable, column-formatted ASCII text.\n"
            "\n"
            "        Note : the output file should not yet exist.\n"
            "             : -out_1D or -out_niml must be used\n"
            "\n"
            "    -out_niml OUTPUT_FILE  : specify a niml file for the output\n"
            "\n"
            "        e.g. -out_niml mask_values_over_dataset.niml.dset\n"
            "\n"
            "        The user may use this option to get output in the form\n"
            "        of a niml element, with binary data.  The output will\n"
            "        contain (binary) columns of the form:\n"
            "\n"
            "            node_index  value_0  value_1  value_2  ...\n"
            "\n"
            "        A major difference between 1D output and niml output is\n"
            "        that the value_0 column number will be 6 in the 1D case,\n"
            "        but will be 2 in the niml case.  The index columns will\n"
            "        not be used for niml output.\n"
            "        It is possible to write niml datasets in both ASCII and \n"
            "        BINARY formats. BINARY format is recommended for large\n"
            "        datasets. The .afnirc environment variable:\n"
            "        AFNI_NIML_TEXT_DATA controls whether output is\n"
            "        ASCII (YES) or BINARY (NO).\n"
            "\n"
            "        Note : the output file should not yet exist.\n"
            "             : -out_1D or -out_niml must be used\n"
            "\n"
            "    -help                  : show this help\n"
            "\n"
            "        If you can't get help here, please get help somewhere.\n"
            "\n"
            "    -hist                  : show revision history\n"
            "\n"
            "        Display module history over time.\n"
            "\n"
            "        See also, -v2s_hist\n"
            "\n"
            "    -last_node NODE_NUM    : skip all following nodes\n"
            "\n"
            "        e.g. -last_node 1999\n"
            "        e.g. -first_node 1000 -last_node 1999\n"
            "\n"
            "        Restrict surface node output to those with indices no\n"
            "        larger than NODE_NUM.  In the first example, nodes above\n"
            "        1999 are ignored (those with indices from 2000 on up).\n"
            "\n"
            "        See also, '-first_node'.\n"
            "\n"
            "    -no_headers            : do not output column headers\n"
            "\n"
            "        Column header lines all begin with the '#' character.\n"
            "        With the '-no_headers' option, these lines will not be\n"
            "        output.\n"
            "\n"
            "    -oob_index INDEX_NUM   : specify default index for oob nodes\n"
            "\n"
            "        e.g.     -oob_index -1\n"
            "        default: -oob_index  0\n"
            "\n"
            "        By default, nodes which lie outside the box defined by\n"
            "        the -grid_parent dataset are considered out of bounds,\n"
            "        and are skipped.  If an out of bounds index is provided,\n"
            "        or an out of bounds value is provided, such nodes will\n"
            "        not be skipped, and will have indices and values output,\n"
            "        according to the -oob_index and -oob_value options.\n"
            "        \n"
            "        This INDEX_NUM will be used for the 1dindex field, along\n"
            "        with the i, j and k indices.\n"
            "        \n"
            "\n"
            "    -oob_value VALUE       : specify default value for oob nodes\n"
            "\n"
            "        e.g.     -oob_value -999.0\n"
            "        default: -oob_value    0.0\n"
            "\n"
            "        See -oob_index, above.\n"
            "        \n"
            "        VALUE will be output for nodes which are out of bounds.\n"
            "\n"
            "    -oom_value VALUE       : specify default value for oom nodes\n"
            "\n"
            "        e.g. -oom_value -999.0\n"
            "        e.g. -oom_value    0.0\n"
            "\n"
            "        By default, node pairs defining a segment which gets\n"
            "        completely obscured by a command-line mask (see -cmask)\n"
            "        are considered \"out of mask\", and are skipped.\n"
            "\n"
            "        If an out of mask value is provided, such nodes will not\n"
            "        be skipped.  The output indices will come from the first\n"
            "        segment point, mapped to the AFNI volume.  All output vN\n"
            "        values will be the VALUE provided with this option.\n"
            "\n"
            "        This option is meaningless without a '-cmask' option.\n"
            "\n"
            "    -outcols_afni_NSD      : output nodes and one result column\n"
            "    -outcols_1_result      : output only one result column\n"
            "    -outcols_results       : output only all result columns\n"
            "    -outcols_NSD_format    : output nodes and all results\n"
            "                             (NI_SURF_DSET foramt)\n"
            "\n"
            "        These options are used to restrict output.  They are\n"
            "        similar to the -skip_col_* options, but are used to\n"
            "        choose columns to output (they are for convenience, so\n"
            "        the user need not apply many -skip_col options).\n"
            "\n"
            "        see also: -skip_col_*\n"
            "\n"
            "    -save_seg_coords FILE  : save segment coordinates to FILE\n"
            "\n"
            "        e.g. -save_seg_coords seg.coords.1D\n"
            "\n"
            "        Each node that has output values computed along a valid\n"
            "        segment (i.e. not out-of-bounds or out-of-mask) has its\n"
            "        index written to this file, along with all applied\n"
            "        segment coordinates.\n"
            "\n"
            "    -skip_col_nodes        : do not output node column\n"
            "    -skip_col_1dindex      : do not output 1dindex column\n"
            "    -skip_col_i            : do not output i column\n"
            "    -skip_col_j            : do not output j column\n"
            "    -skip_col_k            : do not output k column\n"
            "    -skip_col_vals         : do not output vals column\n"
            "\n"
            "        These options are used to restrict output.  Each option\n"
            "        will prevent the program from writing that column of\n"
            "        output to the 1D file.\n"
            "\n"
            "        For now, the only effect that these options can have on\n"
            "        the niml output is by skipping nodes or results (all\n"
            "        other columns are skipped by default).\n"
            "\n"
            "        see also: -outcols_*\n"
            "\n"
            "    -v2s_hist              : show revision history for library\n"
            "\n"
            "        Display vol2surf library history over time.\n"
            "\n"
            "        See also, -hist\n"
            "\n"
            "    -version               : show version information\n"
            "\n"
            "        Show version and compile date.\n"
            "\n"
            "  ------------------------------\n"
            "\n"
            "  general options:\n"
            "\n"
            "    -cmask MASK_COMMAND    : (optional) command for dataset mask\n"
            "\n"
            "        e.g. -cmask '-a fred_func+orig[2] -expr step(a-0.8)'\n"
            "\n"
            "        This option will produce a mask to be applied to the\n"
            "        input AFNI dataset.  Note that this mask should form a\n"
            "        single sub-brick.\n"
            "\n"
            "        This option follows the style of 3dmaskdump (since the\n"
            "        code for it was, uh, borrowed from there (thanks Bob!)).\n"
            "\n"
            "        See '3dmaskdump -help' for more information.\n"
            "\n"
            "    -gp_index SUB_BRICK    : choose grid_parent sub-brick\n"
            "\n"
            "        e.g. -gp_index 3\n"
            "\n"
            "        This option allows the user to choose only a single\n"
            "        sub-brick from the grid_parent dataset for computation.\n"
            "        Note that this option is virtually useless when using\n"
            "        the command-line, as the user can more directly do this\n"
            "        via brick selectors, e.g. func+orig'[3]'.\n"
            "        \n"
            "        This option was written for the afni interface.\n"
            "\n"
            "  --------------------------------------------------\n"
            "\n"
            "Output from the program defaults to 1D format, in ascii text.\n"
            "For each node (pair) that results in output, there will be one\n"
            "line, consisting of:\n"
            "\n"
            "    node    : the index of the current node (or node pair)\n"
            "\n"
            "    1dindex : the global index of the AFNI voxel used for output\n"
            "\n"
            "              Note that for some filters (min, max, midpoint,\n"
            "              median and mode) there is a specific location (and\n"
            "              therefore voxel) that the result comes from.  It\n"
            "              will be accurate (though median may come from one\n"
            "              of two voxels that are averaged).\n"
            "\n"
            "              For filters without a well-defined source (such as\n"
            "              average or seg_vals), the 1dindex will come from\n"
            "              the first point on the corresponding segment.\n"
            "\n"
            "              Note: this will _not_ be output in the niml case.\n"
            "\n"
            "    i j k   : the i j k indices matching 1dindex\n"
            "\n"
            "              These indices are based on the orientation of the\n"
            "              grid parent dataset.\n"
            "\n"
            "              Note: these will _not_ be output in the niml case.\n"
            "\n"
            "    vals    : the number of segment values applied to the filter\n"
            "\n"
            "              Note that when -f_index is 'nodes', this will\n"
            "              always be the same as -f_steps, except when using\n"
            "              the -cmask option.  In that case, along a single \n"
            "              segment, some points may be in the mask, and some\n"
            "              may not.\n"
            "\n"
            "              When -f_index is 'voxels' and -f_steps is used,\n"
            "              vals will often be much smaller than -f_steps.\n"
            "              This is because many segment points may in a\n"
            "              single voxel.\n"
            "\n"
            "              Note: this will _not_ be output in the niml case.\n"
            "\n"
            "    v0, ... : the requested output values\n"
            "\n"
            "              These are the filtered values, usually one per\n"
            "              AFNI sub-brick.  For example, if the -map_func\n"
            "              is 'ave', then there will be one segment-based\n"
            "              average output per sub-brick of the grid parent.\n"
            "\n"
            "              In the case of the 'seg_vals' filter, however,\n"
            "              there will be one output value per segment point\n"
            "              (possibly further restricted to voxels).  Since\n"
            "              output is not designed for a matrix of values,\n"
            "              'seg_vals' is restricted to a single sub-brick.\n"
            "\n"
            "\n"
            "  Author: R. Reynolds  - %s\n"
            "\n"
            "                (many thanks to Z. Saad and R.W. Cox)\n"
            "\n",
            VERSION );
    }
    else if ( level == V2S_USE_HIST )
        fputs(g_history, stdout);
    else if ( level == V2S_USE_LIB_HIST )
        fputs(gv2s_history, stdout);
    else if ( level == V2S_USE_VERSION )
        fprintf(stderr,"%s : %s, compile date: %s\n", prog, VERSION, __DATE__);
    else 
        fprintf( stderr, "usage called with illegal level <%d>\n", level );

    RETURN(-1);
}


/*---------------------------------------------------------------------------
 * surf_ave_radius  -  compute the average distance from the Center
 *---------------------------------------------------------------------------
*/
int surf_ave_radius( float * radius, SUMA_SurfaceObject * so, int disp )
{
    double   ss, sum = 0.0;
    float  * fp;
    float    c0, c1, c2;
    int      node;

ENTRY("surf_ave_radius");

    if ( !so || !radius || so->N_Node <= 0 )
    {
        fprintf(stderr, "** disp_sar, so, radius == %p,%p\n", so, radius );
        RETURN(-1);
    }

    c0 = so->Center[0];                            /* for a little speed */
    c1 = so->Center[1];
    c2 = so->Center[2];

    fp = so->NodeList;
    for ( node = 0; node < so->N_Node; node++ )
    {
        ss  = (*fp - c0) * (*fp - c0);   fp++;
        ss += (*fp - c1) * (*fp - c1);   fp++;
        ss += (*fp - c2) * (*fp - c2);   fp++;

        sum += sqrt(ss);
    }

    *radius = sum/so->N_Node;

    if ( disp )
        fprintf(stderr,"-- surf %s has average dist %f\n"
                       "        to center (%f, %f, %f)\n",
                so->Label, *radius, c0, c1, c2 );

    RETURN(0);
}


/*----------------------------------------------------------------------
 * disp_opts_t  -  display the contents of the opts_t struct
 *----------------------------------------------------------------------
*/
int disp_opts_t ( char * info, opts_t * opts )
{
ENTRY("disp_opts_t");

    if ( info )
        fputs( info, stderr );

    if ( opts == NULL )
    {
        fprintf( stderr, "disp_opts_t: opts == NULL\n" );
        RETURN(-1);
    }

    fprintf(stderr,
            "options struct at %p :\n"
            "    gpar_file              = %s\n"
            "    outfile_1D             = %s\n"
            "    outfile_niml           = %s\n"
            "    seg_coords_file        = %s\n"
            "    spec_file              = %s\n"
            "    sv_file                = %s\n"
            "    cmask_cmd              = %s\n"
            "    map_str                = %s\n"
            "    snames[0,1]            = %s, %s\n"
            "    gp_index, no_head      = %d, %d\n"
            "    first_node, last_node  = %d, %d\n"
            "    use_norms, norm_len    = %d, %f\n"
            "    norm_dir, debug, dnode = %d, %d, %d\n"
            "    f_index_str            = %s\n"
            "    f_steps                = %d\n"
            "    f_p1_fr, f_pn_fr       = %f, %f\n"
            "    f_p1_mm, f_pn_mm       = %f, %f\n"
            , opts,
            CHECK_NULL_STR(opts->gpar_file), CHECK_NULL_STR(opts->outfile_1D),
            CHECK_NULL_STR(opts->outfile_niml),
            CHECK_NULL_STR(opts->seg_coords_file),
            CHECK_NULL_STR(opts->spec_file), CHECK_NULL_STR(opts->sv_file),
            CHECK_NULL_STR(opts->cmask_cmd), CHECK_NULL_STR(opts->map_str),
            CHECK_NULL_STR(opts->snames[0]), CHECK_NULL_STR(opts->snames[1]),
            opts->gp_index, opts->no_head, opts->first_node, opts->last_node,
            opts->use_norms, opts->norm_len, opts->norm_dir, opts->debug,
            opts->dnode, CHECK_NULL_STR(opts->f_index_str), opts->f_steps,
            opts->f_p1_fr, opts->f_pn_fr, opts->f_p1_mm, opts->f_pn_mm
            );

    RETURN(0);
}

