/*----------------------------------------------------------------------
 * main functions for afni (and others):
 *
 *     v2s_results * afni_vol2surf      - create surface data
 *     int           free_v2s_results   - free surface data
 *
 * main interface routing:
 *
 *     v2s_results * vol2surf           - create surface data
 *                                        (assumes valid parameters)
 *
 * display functions:
 *
 *     int disp_mri_imarr       ( char * info, MRI_IMARR       * dp   );
 *     int disp_v2s_opts_t      ( char * info, v2s_opts_t      * sopt );
 *     int disp_v2s_param_t     ( char * info, v2s_param_t     * p    );
 *     int disp_v2s_plugin_opts ( char * info, v2s_plugin_opts * d    );
 *     int disp_v2s_results     ( char * mesg, v2s_results     * d    );
 *
 * Author: R Reynolds
 *----------------------------------------------------------------------
 */

#define _VOL2SURF_C_            /* so the header files know */

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
    "\n"
    "September 29, 2004 [rickr]\n"
    "  - added thd_mask_from_brick() (to make byte mask from sub-brick)\n"
    "  - added compact_results(), in case nalloc > nused\n"
    "  - added realloc_ints() and realloc_vals_list() (for compact_results())\n"
    "  - in afni_vol2surf(), if 1 surf and no norms, set steps to 1\n"
    "  - in set_surf_results(), pass gp_index to v2s_apply_filter\n"
    "  - segment oob only if both nodes are\n"
    "  - move dset bounds to range_3dmm struct\n"
    "  - in segment_imarr()\n"
    "      - changed THD_3dmm_to_3dind() to new THD_3dmm_to_3dind_no_wod()\n"
    "      - verify success of THD_extract_series()\n"
    "      - keep track of repeated and oob nodes\n"
    "  - in init_seg_endpoints(), nuke p1, pn; save dicom_to_mm until end\n"
    "  - changed THD_3dmm_to_3dind() to new THD_3dmm_to_3dind_no_wod()\n"
    "  - added function v2s_is_good_map_index()\n"
    "\n"
    "October 08, 2004 [rickr]\n"
    "  - added disp_v2s_plugin_opts()\n"
    "  - dealt with default v2s mapping of surface pairs\n"
    "  - added v2s_fill_sopt_default()\n"
    "  - moved v2s_write_outfile_*() here, with print_header()\n"
    "  - in afni_vol2surf(), actually write output files\n"
    "\n"
    "October 25, 2004 [rickr]\n"
    "  - apply debug and dnode, even for defaults\n"
    "  - if the user sets dnode, then skip any (debug > 0) tests for it\n"
    "  - check for out of bounds, even if an endpoint is in (e.g. midpoint)\n"
    "  - in thd_mask_from_brick(), allow for absolute thresholding\n"
    "\n"
    "March 22, 2005 [rickr] - removed tabs\n"
    "March 28, 2006 [rickr] - fixed mode computation\n"
    "June 30, 2006 [rickr] - segc_file functionality (-save_seg_coords)\n"
    "\n"
    "August 9, 2006 [rickr]\n"
    "  - create argc, argv from options in v2s_make_command()\n"
    "  - added loc_add_2_list() and v2s_free_cmd() for v2s_make_command()\n"
    "  - added labels, thres index/value and surf vol dset to gv2s_plug_opts\n"
    "\n"
    "August 23, 2006 [rickr]\n"
    "  - in v2s_make_command(), change -skip_col_NSD to -outcols_afni_NSD\n"
    "  - in v2s_write_outfile_NSD(), only output node list if it exists\n"
    "  - do not let set_sparse_data_attribs() set nodes_from_dset attrib\n"
    "September 6, 2006 [rickr]\n"
    "  - use NI_free() with NI_search_group_shallow()\n"
    "November 10, 2006 [rickr]\n"
    "  - added thd_multi_mask_from_brick()\n"
    "---------------------------------------------------------------------\n";

#include "mrilib.h"
#include "vol2surf.h"
#include "suma_suma.h"

/*----------------------------------------------------------------------*/
/* local typedefs                                                       */
typedef struct
{
    int     nused;
    int     nalloc;
    float * list;
} float_list_t;

typedef struct
{
    THD_3dim_dataset * dset;            /* for data and geometry     */
    THD_fvec3          p1;              /* segment endpoints         */
    THD_fvec3          pn;
    THD_fvec3          dset_min;        /* bounds on the dataset     */
    THD_fvec3          dset_max;
    int                oob_check;       /* should we check for oob?   */
    int                debug;           /* for local control         */
} range_3dmm;
                                                                                
typedef struct
{
    MRI_IMARR   ims;                    /* the image array struct     */
    int         repeats;                /* number of repeated nodes   */
    int         masked;                 /* number of masked points    */
    int         oob;                    /* number of oob points       */
    int         ifirst;                 /* 1D index of first point    */
    THD_ivec3   i3first;                /* i3ind index of first point */
    THD_ivec3 * i3arr;                  /* i3ind index array          */
} range_3dmm_res;

/*----------------------------------------------------------------------*/
/* local prototypes                                                     */
static v2s_results * alloc_output_mem (v2s_opts_t *sopt, v2s_param_t *p);

static int    alloc_ints(int ** ptr, int length, char * dstr, int debug);
static int    alloc_vals_list(float *** ptr, int length, int width, int debug);
static int    check_SUMA_surface( SUMA_surface * s );
static int    compact_results(v2s_results * sd, int debug);
static float  directed_dist(float * pnew, float * pold, float *dir, float dist);
static float  dist_f3mm( THD_fvec3 * p1, THD_fvec3 * p2 );
static int    disp_range_3dmm( char * info, range_3dmm * dp );
static int    disp_range_3dmm_res( char * info, range_3dmm_res * dp );
static int    disp_surf_vals( char * mesg, v2s_results * sd, int node );
static int    dump_surf_3dt(v2s_opts_t *sopt, v2s_param_t *p, v2s_results *sd);
static int    f3mm_out_of_bounds(THD_fvec3 *cp, THD_fvec3 *min, THD_fvec3 *max);
static int    float_list_alloc(float_list_t *f,int **ilist,int size,int trunc);
static int    float_list_comp_mode(float_list_t *f, float *mode, int *nvals,
                                   int *index);
static int    float_list_slow_sort(float_list_t * f, int * ilist);
static int    init_seg_endpoints(v2s_opts_t * sopt, v2s_param_t * p,
                                 range_3dmm * R, int node );
static int    init_range_structs( range_3dmm * r3, range_3dmm_res * res3 );
static int    loc_add_2_list( char *** list, int * nall, int * len, char * str);
static double magnitude_f( float * p, int length );
static int    print_header(FILE *outfp, char *surf, char *map, v2s_results *sd);
static int    realloc_ints( int ** ptr, int length, char * dstr, int debug );
static int    realloc_vals_list(float ** ptr, int length, int width, int debug);
static int    set_3dmm_bounds(THD_3dim_dataset *dset, THD_fvec3 *min,
                              THD_fvec3 *max);
static int    set_all_surf_vals (v2s_results * sd, int node_ind, int vind,
                                 int i, int j, int k, float fval);
static int    set_output_labels(v2s_results * sd, v2s_param_t * p,
                                v2s_opts_t * sopt);
static int    set_surf_results(v2s_param_t *p, v2s_opts_t *sopt,v2s_results *sd,
                               range_3dmm_res * r3res, int node, int findex);
static int    segment_imarr(range_3dmm_res *res,range_3dmm *R,v2s_opts_t *sopt,
                            byte * cmask, FILE * cfp, int nindex);
static int    v2s_adjust_endpts(v2s_opts_t *sopt, THD_fvec3 *p1, THD_fvec3 *pn);
static float  v2s_apply_filter(range_3dmm_res *rr, v2s_opts_t *sopt, int index,
                               int * findex);
static int    v2s_free_cmd(v2s_opts_t * sopt);
static int    v2s_map_needs_sort(int map);
static int    validate_v2s_inputs(v2s_opts_t * sopt, v2s_param_t * p);

/*----------------------------------------------------------------------*/
/* globals to be accessed by plugin and in afni_suma.c                  */
v2s_plugin_opts gv2s_plug_opts = {
        0,0,0,                    /* ready, use0, use1     */
        -1,-1,-1,-1,              /* s0A, s0B, s1A, s1B    */
        -1,0.0,                   /* threshold index/value */
        {NULL, NULL, NULL, NULL}, /* surface labels [4]    */
        NULL                      /* surf vol dataset      */
};
                            /* this must match v2s_map_nums enum */
char * gv2s_map_names[] = { "none", "mask", "midpoint", "mask2", "ave",
                            "count", "min", "max", "max_abs", "seg_vals",
                            "median", "mode" };
char gv2s_no_label[] = "undefined";

/*----------------------------------------------------------------------
 * afni_vol2surf     - create v2s_results from gv2s_* afni globals
 *
 *    input:   gpar         : AFNI dataset to be used as the grid parent
 *             gp_index     : sub-brick selector
 *             sA           : surface A structure
 *             sB           : surface B structure
 *             mask         : thresholding mask
 *             use_defaults : use default sopt structure
 * 
 *    output:  sd    : allocated v2s_results struct, with requested data
 *
 * This function is used to map data from an AFNI volume to a surface.
 * These structures are expected to be complete.
 *
 * The function now relies on opt_vol2surf.          29 Apr 2009 [rickr]
 *----------------------------------------------------------------------
*/
v2s_results * afni_vol2surf ( THD_3dim_dataset * gpar, int gp_index,
                              SUMA_surface * sA, SUMA_surface * sB,
                              byte * mask, int use_defaults )
{
    v2s_opts_t * sopt, sopt_def;

    ENTRY("afni_vol2surf");

    if ( use_defaults )
    {
        sopt = &sopt_def;
        v2s_fill_sopt_default(sopt, sB ? 2 : 1);  /* 1 or 2 surfaces */

        /* but apply any debug options */
        sopt->debug = gv2s_plug_opts.sopt.debug;
        sopt->dnode = gv2s_plug_opts.sopt.dnode;
    }
    else 
        sopt = &gv2s_plug_opts.sopt;

    sopt->gp_index = gp_index;

    RETURN(opt_vol2surf(gpar, sopt, sA, sB, mask));
}


/*----------------------------------------------------------------------
 * opt_vol2surf - create v2s_results from v2s_plugin_opts struct
 *
 * Fill in v2s_param_t struct, call vol2surf, write any output files.
 *
 *    input:   gpar         : AFNI dataset to be used as the grid parent
 *             sopt         : vol2surf options struct
 *             sA           : surface A structure
 *             sB           : surface B structure
 *             mask         : volume mask
 * 
 *    output:  sd    : allocated v2s_results struct, with requested data
 *
 * This function is used to map data from an AFNI volume to a surface.
 * These structures are expected to be complete.
 *----------------------------------------------------------------------
*/
v2s_results * opt_vol2surf (THD_3dim_dataset * gpar, v2s_opts_t * sopt,
                            SUMA_surface * sA, SUMA_surface * sB, byte * mask)
{
    v2s_param_t   P;
    v2s_results * res;

ENTRY("opt_vol2surf");

    if ( !gpar ) RETURN(NULL);
    if (       check_SUMA_surface(sA) ) RETURN(NULL);
    if ( sB && check_SUMA_surface(sB) ) RETURN(NULL);

    /* now fill the param struct based on the inputs */
    memset(&P, 0, sizeof(P));
    P.gpar         = gpar;
    P.cmask        = mask;
    P.nvox         = DSET_NVOX(gpar);
    P.over_steps   = v2s_vals_over_steps(sopt->map);
    P.nsurf        = sB ? 2 : 1;
    P.surf[0]      = *sA;
    if ( sB ) P.surf[1] = *sB;

    /* verify steps, in case the user has not selected 2 surfaces */
    if ( P.nsurf == 1 && ! sopt->use_norms )
        sopt->f_steps = 1;

    if ( sopt->debug > 2 ) disp_v2s_opts_t("-- v2s options: ", sopt);

    /* fire it up */

    res = vol2surf(sopt, &P);

    v2s_make_command(sopt, &P);
    if( gv2s_plug_opts.sopt.debug > 2 ) disp_v2s_command(sopt);

    /* if the user wants output files, here they are (don't error check) */
    if( res && sopt->outfile_1D ) {
       if( THD_is_file(sopt->outfile_1D) )
          fprintf(stderr,"** over-writing 1D output file '%s'\n",
                  sopt->outfile_1D);
       v2s_write_outfile_1D(sopt, res, P.surf[0].label);
    }

    if( res && sopt->outfile_niml )
    {
        if ( THD_is_file(sopt->outfile_niml) )
            fprintf(stderr,"** over-writing niml output file '%s'\n",
                    sopt->outfile_niml);
        v2s_write_outfile_NSD(res, sopt, &P, 0);
    }

    if( sopt->cmd.fake ) v2s_free_cmd( sopt );

    RETURN(res);
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

    if ( sopt->debug > 1 ) disp_v2s_param_t( "-d post alloc_output_mem : ", p );

    rv = dump_surf_3dt( sopt, p, sd );

    if ( compact_results(sd, sopt->debug) )
    {
        free_v2s_results(sd);           /* free whatever didn't get burned */
        RETURN(NULL);
    }

    if ( sopt->debug > 1 ) disp_v2s_results( "-d post surf creation : ", sd);

    RETURN(sd);
}


/* compact_results    - if nused < nalloc, realloc all pointers */
static int compact_results(v2s_results * sd, int debug)
{
    int rv = 0, mem = 0;
ENTRY("compact_results");

    if ( sd->nused > sd->nalloc )  /* should not happen, of course */
    {
        fprintf(stderr,"** cr: nused (%d) > nalloc (%d) !!\n",
                sd->nused, sd->nalloc);
        RETURN(-1);
    }

    if ( sd->nused == sd->nalloc ) RETURN(0);   /* we're good */

    /* otherwise, realloc everything */

    sd->nalloc = sd->nused;

    if ( sd->nodes )
    {
        mem += sizeof(int);
        rv = realloc_ints(&sd->nodes, sd->nalloc, "nodes", debug);
    }

    if ( ! rv && sd->volind )
    {
        mem += sizeof(int);
        rv = realloc_ints(&sd->volind, sd->nalloc, "volind", debug);
    }

    if ( ! rv && sd->i )
    {
        mem += sizeof(int);
        rv = realloc_ints(&sd->i, sd->nalloc, "i", debug);
    }

    if ( ! rv && sd->j )
    {
        mem += sizeof(int);
        rv = realloc_ints(&sd->j, sd->nalloc, "j", debug);
    }

    if ( ! rv && sd->k )
    {
        mem += sizeof(int);
        rv = realloc_ints(&sd->k, sd->nalloc, "k", debug);
    }

    if ( ! rv && sd->nvals )
    {
        mem += sizeof(int);
        rv = realloc_ints(&sd->nvals, sd->nalloc, "nvals", debug);
    }

    if ( ! rv )
    {
        mem += (sizeof(float) * sd->max_vals);
        rv = realloc_vals_list(sd->vals, sd->nalloc, sd->max_vals, debug);
    }

    if ( rv ) RETURN(rv);       /* if there was a failure, just leave */

    mem *= sd->nalloc;          /* now multiply be the array length */

    if ( debug > 1 )
        fprintf(stderr,"+d compact results: reallocated %d bytes down to %d\n",
                sd->memory, mem);

    sd->memory = mem;

    RETURN(rv);
}


/*----------------------------------------------------------------------
 * dump_surf_3dt - for each node index, get an appropriate node sampling,
 *                 and compute and output results across sub-bricks
 *----------------------------------------------------------------------
*/
static int dump_surf_3dt( v2s_opts_t * sopt, v2s_param_t * p, v2s_results * sd )
{
    range_3dmm_res   r3mm_res;
    range_3dmm       r3mm;
    float            dist, min_dist, max_dist;
    FILE           * coord_fp = NULL;
    int              sub, nindex, findex = 0;
    int              oobc, oomc;
    int              oob1, oob2;

ENTRY("dump_surf_3dt");

    if ( ! sopt || ! p || ! sd )
    {
        fprintf(stderr, "** ds3 : bad params (%p,%p,%p)\n", sopt, p, sd );
        RETURN(-1);
    }

    /* possibly write to a coordinate output file   30 Jun 2006 [rickr] */
    if ( sopt->segc_file )
    {
        coord_fp = fopen(sopt->segc_file, "w");
        if ( !coord_fp ) /* complain, but continue */
            fprintf(stderr,"** failed to open coord file '%s', continuing...\n",                    sopt->segc_file);
    }

    /* note the number of sub-bricks, unless the user has given just one */
    init_range_structs( &r3mm, &r3mm_res );                 /* to empty */
    r3mm.dset = p->gpar;
    set_3dmm_bounds( p->gpar, &r3mm.dset_min, &r3mm.dset_max );

    if ( sopt->debug > 1 )
        fprintf(stderr, "-d dset bounding box: (%f, %f, %f)\n"
                        "                      (%f, %f, %f)\n",
                r3mm.dset_min.xyz[0],r3mm.dset_min.xyz[1],r3mm.dset_min.xyz[2], 
                r3mm.dset_max.xyz[0],r3mm.dset_max.xyz[1],r3mm.dset_max.xyz[2]);

    min_dist = 9999.9;                                          /* v2.3 */
    max_dist = -1.0;
    oobc     = 0;                         /* init out-of-bounds counter */
    oomc     = 0;                         /* init out-of-mask counter   */

    /* note, NodeList elements are in dicomm mm orientation */

    for ( nindex = sopt->first_node; nindex <= sopt->last_node; nindex++ )
    {
        init_seg_endpoints(sopt, p, &r3mm, nindex);    /* segment endpoints */
        v2s_adjust_endpts( sopt, &r3mm.p1, &r3mm.pn );

        if ( r3mm.debug )
            r3mm.debug = 0;

        if ( nindex == sopt->dnode )      /* if we have dnode, forget debug */
            r3mm.debug = sopt->debug > 0 ? sopt->debug : 1;

        /* if both points are outside our dataset, skip the pair   v2.3 */
        oob1 = f3mm_out_of_bounds( &r3mm.p1, &r3mm.dset_min, &r3mm.dset_max );
        oob2 = f3mm_out_of_bounds( &r3mm.pn, &r3mm.dset_min, &r3mm.dset_max );
        if ( oob1 && oob2 )
        {
            oobc++;
            if ( sopt->oob.show )
                if ( set_all_surf_vals( sd, nindex, sopt->oob.index,
                                        sopt->oob.index, sopt->oob.index,
                                        sopt->oob.index, sopt->oob.value) )
                    RETURN(1);
            if ( nindex == sopt->dnode )
            {
                disp_surf_vals("-d debug node, out-of-bounds : ", sd, -1);
                fprintf(stderr,"-d dnode coords: (%f, %f, %f)\n",
                        r3mm.p1.xyz[0], r3mm.p1.xyz[1], r3mm.p1.xyz[2]);
            }
            continue;
        }
        else
            r3mm.oob_check = oob1 || oob2;

        dist = dist_f3mm( &r3mm.p1, &r3mm.pn );
        if ( dist < min_dist ) min_dist = dist;
        if ( dist > max_dist ) max_dist = dist;

        if ( segment_imarr(&r3mm_res,&r3mm,sopt,p->cmask,coord_fp,nindex) != 0 )
            continue;

        if ( r3mm_res.ims.num == 0 )    /* any good voxels in the bunch? */
        {
            /* oob or oom? */
            if ( r3mm_res.oob == sopt->f_steps ) /* out of bounds */
            {
                oobc++;
                if ( sopt->oob.show )
                    if ( set_all_surf_vals( sd, nindex, sopt->oob.index,
                                            sopt->oob.index, sopt->oob.index,
                                            sopt->oob.index, sopt->oob.value) )
                        RETURN(1);
                if ( nindex == sopt->dnode )
                    disp_surf_vals("-d debug node, out-of-bounds : ", sd, -1);
            }
            else   /* then we consider it out of mask */
            {
                oomc++;
                if ( sopt->oom.show )
                    if ( set_all_surf_vals( sd, nindex, r3mm_res.ifirst,
                            r3mm_res.i3first.ijk[0], r3mm_res.i3first.ijk[1],
                            r3mm_res.i3first.ijk[2], sopt->oom.value ) )
                        RETURN(1);
                if ( nindex == sopt->dnode )
                    disp_surf_vals("-d debug node, out-of-mask : ", sd, -1);
            }

            if ( nindex == sopt->dnode )
                fprintf(stderr,"-d dnode coords: (%f, %f, %f)\n",
                        r3mm.p1.xyz[0], r3mm.p1.xyz[1], r3mm.p1.xyz[2]);

            continue;   /* in any case */
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

    if ( sopt->debug > 0 )                                      /* v2.3 */
    {
        fprintf( stderr, "-d node pair dist (min,max) = (%f,%f)\n",
                 min_dist, max_dist );
        fprintf( stderr, "-d out-of-bounds, o-o-mask counts : %d, %d (of %d)\n",
                 oobc, oomc, sopt->last_node - sopt->first_node + 1);
    }

    /* set up the labels before leaving */
    set_output_labels(sd, p, sopt);

    /* close any coord file */
    if ( coord_fp ) fclose( coord_fp );

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
 * copy labels to v2s_results struct
 ***********************************************************************
*/
static int set_output_labels(v2s_results * sd, v2s_param_t * p,
                             v2s_opts_t * sopt)
{
    int  c, nl;
    char str[32], label[32];

ENTRY("set_output_labels");

    if(!sd->labels){ fprintf(stderr,"** SOL: NULL labels!\n"); RETURN(1); }

    if( sopt->gp_index >= 0 || p->over_steps )  /* get one label */
    {
        if(sd->nlab != sd->max_vals)
            fprintf(stderr,"** SOL: nlabel mis-match: %d vs %d\n",
                    sd->nlab, sd->max_vals);

	/* set label (prefix) from sub-brick, if possible */
        nl = sopt->gp_index >= 0 ? sopt->gp_index : 0;
        if( p->gpar->dblk->brick_lab && p->gpar->dblk->brick_lab[nl] )
            MCW_strncpy(str, p->gpar->dblk->brick_lab[nl], 24);
        else
	    sprintf(str, "vol %d\n", nl);

	/* if one label use str, else index upto nlab */
	if( sd->nlab == 1 ) sd->labels[0] = strdup(str);
	else {
	    for( c = 0; c < sd->nlab; c++ ) {
		sprintf(label,"%s #%d", str, c);
		sd->labels[c] = strdup(label);
	    }
	}
    }
    else /* use all sub-brick labels */
    {
        nl = DSET_NVALS(p->gpar);
        if(sd->nlab != nl)
            fprintf(stderr,"** SOL: %d labels != %d\n", sd->nlab, nl);
        if(nl > sd->nlab) nl = sd->nlab;
        for( c = 0; c < nl; c++ )
        {
            if( !p->gpar->dblk->brick_lab || !p->gpar->dblk->brick_lab[c] )
            {
                sprintf(str,"volume #%d\n",c);
                sd->labels[c] = strdup(str);
            }
            else
                sd->labels[c] = strdup(p->gpar->dblk->brick_lab[c]);
        }
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

    /* set max_index, and adjust in case max_vals has been restricted */
    max_index = p->over_steps ? r3res->ims.num : DSET_NVALS(p->gpar);
    if ( max_index > sd->max_vals ) max_index = sd->max_vals;

    if ( sopt->gp_index >= 0 )
        sd->vals[0][sd->nused] =
            v2s_apply_filter(r3res, sopt, sopt->gp_index, NULL);
    else
        for ( c = 0; c < max_index; c++ )
            sd->vals[c][sd->nused] = v2s_apply_filter(r3res, sopt, c, NULL);

    /* possibly fill line with default if by steps and short */
    if ( max_index < sd->max_vals )
        for ( c = max_index; c < sd->max_vals; c++ )
            sd->vals[c][sd->nused] = 0.0;

    /* maybe the user wants us to be verbose about this node */
    if ( node == sopt->dnode )
    {
        fprintf(stderr,
                "--------------------------------------------------\n");
        if ( ! p->over_steps && sopt->gp_index >= 0 )
            fprintf(stderr,"+d dnode %d gets %f from gp_index %d\n",
                    node, sd->vals[0][sd->nused], sopt->gp_index);
        if ( sopt->debug > 1 )
            fprintf(stderr, "-d debug: node %d, findex %d, vol_index %d\n",
                    node, findex, volind );
        if ( sopt->use_norms )
        {
            float * fp = p->surf[0].norm[node].xyz;
            fprintf(stderr,"-d normal %f, %f, %f\n", fp[0], fp[1], fp[2]);
        }
        disp_mri_imarr( "+d raw data: ", &r3res->ims );
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
 * segment_imarr        - get MRI_IMARR for steps over a segment
 *
 * The res->ims structure should be empty, except that it may
 * optionally contain space for pointers in imarr.  Note that nall
 * should be accurate.
 *
 * return 0 on success
 ***********************************************************************
*/
static int segment_imarr( range_3dmm_res *res, range_3dmm *R, v2s_opts_t *sopt,
                          byte * cmask, FILE * cfp, int nindex )
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
        disp_range_3dmm("-d segment_imarr: ", R );

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
            fprintf(stderr,"+d realloc of imarr (from %d to %d pointers)\n",
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
    res->repeats = 0;
    res->masked  = 0;
    res->oob     = 0;
    res->i3first = THD_3dmm_to_3dind_no_wod( R->dset, R->p1 );
    res->ifirst  = res->i3first.ijk[0] +
                   nx * (res->i3first.ijk[1] + ny * res->i3first.ijk[2] );

    prev_ind = -1;                      /* in case we want unique voxels */

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

        /* accept part being oob                30 Sep 2004 [rickr] */
        if ( R->oob_check && 
             f3mm_out_of_bounds( &f3mm, &R->dset_min, &R->dset_max ) )
        {
            res->oob++;
            continue;
        }

        i3ind = THD_3dmm_to_3dind_no_wod( R->dset, f3mm );
        vindex = i3ind.ijk[0] + nx * (i3ind.ijk[1] + ny * i3ind.ijk[2] );

        /* is this voxel masked out? */
        if ( cmask && !cmask[vindex] )
        {
            res->masked++;
            continue;
        }

        /* is this voxel repeated, and if so, do we skip it? */
        if ( sopt->f_index == V2S_INDEX_VOXEL && vindex == prev_ind )
        {
            res->repeats++;
            continue;
        }

        /* Huston, we have a good voxel... */

        /* now for the big finish, get and insert the actual data */

        res->i3arr    [res->ims.num] = i3ind;   /* store the 3-D indices */
        res->ims.imarr[res->ims.num] = THD_extract_series( vindex, R->dset, 0 );
        res->ims.num++;

        if ( ! res->ims.imarr[res->ims.num-1] )
        {
            fprintf(stderr,"** failed THD_extract_series, vox %d\n", vindex);
            RETURN(-1);
        }

        if ( R->debug > 2 )
            fprintf(stderr, "-d seg step %2d, vindex %d, coords %f %f %f\n",
                    step,vindex,f3mm.xyz[0],f3mm.xyz[1],f3mm.xyz[2]);
        if ( cfp ) /* then write voxel coordinates to output file */
        {
            f3mm = THD_3dmm_to_dicomm(R->dset, f3mm);  /* output in DICOM */
            if ( prev_ind == -1 ) fprintf(cfp, "%8d", nindex);
            fprintf(cfp, "    %9.3f %9.3f %9.3f",
                    f3mm.xyz[0],f3mm.xyz[1],f3mm.xyz[2]);
        }

        prev_ind = vindex;              /* note this for next time  */

    }

    /* maybe write eol to coord file */
    if ( cfp && prev_ind != -1 ) fputc('\n', cfp);

    if ( R->debug > 1 )
        disp_range_3dmm_res( "+d i3mm_seg_imarr results: ", res );

    RETURN(0);
}


/*----------------------------------------------------------------------
 * f3mm_out_of_bounds    - check wether cp is between min and max
 *
 *                       - v2.3 [rickr]
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
 * v2s_adjust_endpoints         - adjust endpoints for map and options
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
        if ( sopt->f_p1_fr != 0.0 )     /* what the heck, choose fr if both */
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
    static float_list_t   flist = { 0, 0, NULL };  /* for sorting results */
    static int          * ind_list = NULL;         /* track index sources */
    double                tmp, comp = 0.0;
    float                 fval;
    int                   count, source;
    int                   brick_index = 0;

ENTRY("v2s_apply_filter");

    if ( !rr || !sopt || index < 0 )
    {
        fprintf(stderr,"** v2s_cm2: invalid params (%p,%p,%d)\n",
                rr, sopt, index);
        RETURN(0.0);
    }
    
    if ( rr->ims.num <= 0 )
        RETURN(0.0);

    /* if sorting is required for results, do it now */
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
 * v2s_map_needs_sort           - does this map function require sorting?
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
 * float_list_comp_mode         - compute the mode of the sorted list
 *
 * return  0 : on success
 *        -1 : on error
 *----------------------------------------------------------------------
*/
static int float_list_comp_mode( float_list_t *f, float *mode, int *nvals,
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
        else                        /* found a new entry to count   */
        {
            if ( ncur > *nvals )     /* keep track of any new winner */
            {
                *mode  = fcur;
                *nvals = ncur;
                *index = c - ncur;   /* note first occurance */
            }

            fcur = f->list[c];
            ncur = 1;
        }
    }

    if ( ncur > *nvals )     /* keep track of any new winner */
    {
        *mode  = fcur;
        *nvals = ncur;
        *index = c - ncur;   /* note first occurance */
    }

    RETURN(0);
}


/*----------------------------------------------------------------------
 * float_list_slow_sort         - sort (small) float list
 *
 * If ilist, track index sources.
 *
 * return   0 on success
 *        < 0 on error
 *----------------------------------------------------------------------
*/
static int float_list_slow_sort( float_list_t * f, int * ilist )
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
 * float_list_alloc             - verify float list memory
 *
 * If trunc(ate), realloc down to necessary size.
 * If ilist, make space for accompanying int list.
 *
 * return   0 on success
 *        < 0 on error
 *----------------------------------------------------------------------
*/
static int float_list_alloc(float_list_t *f, int **ilist, int size, int trunc)
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

        if ( ilist )     /* then allocate accompanying ilist */
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

    if ( mag < V2S_EPSILON )    /* can't be negative */
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
    float        * norm;

ENTRY("init_seg_endpoints");

    /* get node from first surface */
    sp = p->surf;
    R->p1.xyz[0] = sp->ixyz[node].x;
    R->p1.xyz[1] = sp->ixyz[node].y;
    R->p1.xyz[2] = sp->ixyz[node].z;

    /* set pn based on other parameters */
    if ( sopt->use_norms ) {
        /* actually reverse norm direction     4 Feb 2009 [rickr] */
        norm = sp->norm[node].xyz;
        if ( sopt->norm_dir == V2S_NORM_REVERSE )
        {
            if( node == sopt->dnode )
                fprintf(stderr,"-d reversing norm dir at dnode %d\n", node);
            directed_dist(R->pn.xyz, R->p1.xyz, norm, -sopt->norm_len);
        } else
            directed_dist(R->pn.xyz, R->p1.xyz, norm, sopt->norm_len);
    }
    else if ( p->nsurf > 1 )
    {
        /* get node from second surface */
        sp = p->surf + 1;
        R->pn.xyz[0] = sp->ixyz[node].x;
        R->pn.xyz[1] = sp->ixyz[node].y;
        R->pn.xyz[2] = sp->ixyz[node].z;
    }
    else
        R->pn = R->p1;

    R->p1 = THD_dicomm_to_3dmm(R->dset, R->p1);
    R->pn = THD_dicomm_to_3dmm(R->dset, R->pn);

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

    nnodes = p->surf[0].num_ixyz;               /* just for typing ease */

    sd = calloc(1, sizeof(v2s_results));
    if ( ! sd )
    {
        fprintf(stderr,"** aom: failed to allocate v2s_results struct\n");
        RETURN(NULL);
    }

    /* explicitly initialize all pointers with NULL */
    sd->nodes  = sd->volind = sd->i = sd->j = sd->k = sd->nvals = NULL;
    sd->vals   = NULL;
    sd->labels = NULL;
 
    /* verify first and last node indices */
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

    /* allocate labels array - always max_vals		13 Jan 2010 */
    sd->nlab = sd->max_vals;
    sd->labels = (char **)malloc(sd->nlab * sizeof(char *));
    if(!sd->labels){ fprintf(stderr,"** AOM malloc failure\n"); rv = 1; }

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

    if ( sopt->debug > 1 )
        fprintf(stderr,"+d alloc result mem: %d bytes, max_vals = %d\n",
                mem, sd->max_vals);

    /* okay, this time let's allocate something... */

    if ( ! rv && ! (sopt->skip_cols & V2S_SKIP_NODES) )
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
        fprintf(stderr,"-d alloc'd %d x %d floats for surf data\n",
                width, length);

    RETURN(0);
}


/*----------------------------------------------------------------------
 * realloc_vals_list  - reallocate 2D arrays for surface data values
 *----------------------------------------------------------------------
*/
static int realloc_vals_list(float ** ptr, int length, int width, int debug)
{
    int c, count;

ENTRY("realloc_vals_list");

    count = 0;
    for ( c = 0; c < width; c++ )
    {
        if ( ptr[c] )
        {
            ptr[c] = (float *)realloc(ptr[c], length * sizeof(float));
            if ( ptr[c] == NULL )
                fprintf(stderr,"** rvl: fail to realloc %d floats (%d of %d)\n",
                    length, c, width);
            count++;
        }
    }

    if ( debug > 1 )
        fprintf(stderr,"-d realloc'd %d x %d floats for surf data\n",
                count, length);

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
        fprintf(stderr,"-d ai: alloc'd %d ints for '%s'\n", length, dstr);

    RETURN(0);
}


/*----------------------------------------------------------------------
 * realloc_ints  - reallocate 1D array of ints (could replace alloc_ints)
 *----------------------------------------------------------------------
*/
static int realloc_ints( int ** ptr, int length, char * dstr, int debug )
{
ENTRY("realloc_ints");

    *ptr = (int *)realloc(*ptr, length * sizeof(int));
    if ( ! *ptr )
    {
        fprintf(stderr,"** ri: failed to alloc %d ints for '%s'\n",length,dstr);
        RETURN(1);
    }
    if ( debug > 1 )
        fprintf(stderr,"-d ri: alloc'd %d ints for '%s'\n", length, dstr);

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
            "v2s_param_t struct at     %p :\n"
            "    gpar  : vcheck      = %p : %s\n"
            "    cmask               = %p\n"
            "    nvox, over_steps    = %d, %d\n"
            "    nsurf               = %d\n"
            , p,
            p->gpar, ISVALID_DSET(p->gpar) ? "valid" : "invalid",
            p->cmask, p->nvox, p->over_steps, p->nsurf
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
            "    segc_file             = %s\n"
            "    fake, argc, argv      = %d, %d, %p\n"
            , sopt,
            sopt->map, sopt->gp_index, sopt->debug, sopt->dnode,
            sopt->no_head, sopt->skip_cols,
            sopt->first_node, sopt->last_node,
            sopt->use_norms, sopt->norm_len, sopt->norm_dir,
            sopt->f_index, sopt->f_steps,
            sopt->f_p1_fr, sopt->f_pn_fr, sopt->f_p1_mm, sopt->f_pn_mm,
            CHECK_NULL_STR(sopt->outfile_1D),
            CHECK_NULL_STR(sopt->outfile_niml),
            CHECK_NULL_STR(sopt->segc_file),
            sopt->cmd.fake, sopt->cmd.argc, sopt->cmd.argv
            );

    RETURN(0);
}


/*----------------------------------------------------------------------
 * magnitude_f          - return magnitude of float vector
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
 * dist_f3mm            - return Euclidean distance between the points
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
            "    dset             = %p : %s\n"
            "    p1               = (%f, %f, %f)\n"
            "    pn               = (%f, %f, %f)\n"
            "    dset_min         = (%f, %f, %f)\n"
            "    dset_max         = (%f, %f, %f)\n"
            "    oob_check, debug = (%d, %d)\n",
            dp, dp->dset, ISVALID_DSET(dp->dset) ? "valid" : "invalid",
            dp->p1.xyz[0], dp->p1.xyz[1], dp->p1.xyz[2],
            dp->pn.xyz[0], dp->pn.xyz[1], dp->pn.xyz[2],
            dp->dset_min.xyz[0], dp->dset_min.xyz[1], dp->dset_min.xyz[2],
            dp->dset_max.xyz[0], dp->dset_max.xyz[1], dp->dset_max.xyz[2],
            dp->oob_check, dp->debug
        );

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
            "    ims.imarr         = %p\n"
            "    ims.num, ims.nall = %d, %d\n"
            "    repeats, masked   = %d, %d\n"
            "    oob, ifirst       = %d, %d\n"
            "    i3first[0,1,2]    = %d, %d, %d\n"
            "    i3arr             = %p\n"
            , dp,
            dp->ims.imarr, dp->ims.num, dp->ims.nall,
            dp->repeats, dp->masked, dp->oob, dp->ifirst,
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

    fprintf(stderr, "-------------------------------------------------\n");
    if ( mesg ) fputs( mesg, stderr );
    if ( sd->nused < 1 )
    {
        fprintf(stderr,"** no surf nodes defined\n");
        RETURN(-1);
    }

    index = (node >= 0) ? node : sd->nused - 1;

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
                    "    nvals, vals      = %p, %p\n"
                    "    labels, nlab     = %p, %d\n",
                    d, d->nalloc, d->nused, d->max_vals, d->memory,
                    d->nodes, d->volind, d->i, d->j, d->k, d->nvals, d->vals,
                    d->labels, d->nlab);

    RETURN(0);
}


/***********************************************************************
 * disp_v2s_plugin_opts
 ***********************************************************************
*/
int disp_v2s_plugin_opts( char * mesg, v2s_plugin_opts * d )
{
ENTRY("disp_v2s_plugin_opts");

    if ( mesg ) fputs( mesg, stderr );

    fprintf(stderr, "v2s_plugin_opts @ %p\n"
                    "    ready      = %d\n"
                    "    use0, use1 = %d, %d\n"
                    "    s0A, s0B   = %d, %d\n"
                    "    s1A, s1B   = %d, %d\n"
                    "    gpt_index  = %d\n"
                    "    gpt_thresh = %f\n"
                    "    label[0,1] = %s, %s\n"
                    "    label[2,3] = %s, %s\n"
                    "    surf_vol   = %s\n",
                    d, d->ready, d->use0, d->use1,
                    d->s0A, d->s0B, d->s1A, d->s1B,
                    d->gpt_index, d->gpt_thresh,
                    CHECK_NULL_STR(d->label[0]),
                    CHECK_NULL_STR(d->label[1]),
                    CHECK_NULL_STR(d->label[2]),
                    CHECK_NULL_STR(d->label[3]),
                    d->sv_dset ? DSET_FILECODE(d->sv_dset) : "NULL"
           );
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

    if (sd->labels && sd->nlab > 0)
    {
        for ( c = 0; c < sd->nlab; c++ )
            if ( sd->labels[c] ) { free(sd->labels[c]); sd->labels[c] = NULL; }

        free(sd->labels);
        sd->labels = NULL;
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
        fprintf( stderr, "** error:  gv2s_map_names/v2s_map_num mismatch\n");
        RETURN((int)E_SMAP_INVALID);
    }
                                                                                
    /* not ready for E_SMAP_COUNT yet (until someone wants it) */
    if ( !strcmp( map_str, gv2s_map_names[E_SMAP_COUNT] ) )
        RETURN((int)E_SMAP_INVALID);
                                                                                
    for ( map = E_SMAP_INVALID; map < E_SMAP_FINAL; map++ )
        if ( !strcmp( map_str, gv2s_map_names[map] ) )
            RETURN((int)map);

    RETURN((int)E_SMAP_INVALID);
}


/*----------------------------------------------------------------------
 * check for a map index that we consider valid
 *
 * from anywhere, E_SMAP_MASK2 and E_SMAP_COUNT are not yet implemented
 * from afni, E_SMAP_SEG_VALS is not acceptable (only allow 1 output)
 *----------------------------------------------------------------------
*/
int v2s_is_good_map( int map, int from_afni )
{
ENTRY("v2s_good_map_index");

    if ( map <= E_SMAP_INVALID || map >= E_SMAP_FINAL )
        RETURN(0);

    /* these have not (yet? do we care?) been implemented */
    if ( map == E_SMAP_MASK2 || map == E_SMAP_COUNT )
        RETURN(0);

    if ( from_afni && map == E_SMAP_SEG_VALS )
        RETURN(0);

    RETURN(1);
}

/*----------------------------------------------------------------------
 * check for a map index that we consider valid
 *
 * from anywhere, E_SMAP_MASK2 and E_SMAP_COUNT are not yet implemented
 * from afni, E_SMAP_SEG_VALS is not acceptable (only allow 1 output)
 *----------------------------------------------------------------------
*/
int v2s_fill_sopt_default(v2s_opts_t * sopt, int nsurf )
{

ENTRY("v2s_fill_sopt_default");

    if ( !sopt || nsurf < 1 || nsurf > 2 )
    {
        fprintf(stderr,"** FSAD: bad params (%p,%d)\n",sopt,nsurf);
        RETURN(1);
    }

    /* first set any zeros */
    memset(sopt, 0, sizeof(*sopt));

    if ( nsurf == 2 )
        sopt->map = E_SMAP_MIDPT;
    else
        sopt->map = E_SMAP_MASK;

    sopt->gp_index     = -1;
    sopt->no_head      = 1;
    sopt->skip_cols    = V2S_SKIP_ALL ^ V2S_SKIP_NODES;  /* nodes and 1 val */
    sopt->f_steps      = 1;
    sopt->outfile_1D   = NULL;
    sopt->outfile_niml = NULL;
    sopt->segc_file    = NULL;

    sopt->cmd.fake     = 0;
    sopt->cmd.argc     = 0;
    sopt->cmd.argv     = NULL;

    RETURN(0);
}


/*----------------------------------------------------------------------
 * v2s_write_outfile_1D         - write results to 1D file
 *----------------------------------------------------------------------
*/
int v2s_write_outfile_1D ( v2s_opts_t * sopt, v2s_results * sd, char * label )
{
    FILE        * fp;
    int           c, c2;
ENTRY("v2s_write_outfile_1D");

    fp = fopen( sopt->outfile_1D, "w" );
    if ( fp == NULL )
    {
        fprintf( stderr, "** failure to open '%s' for writing\n",
                 sopt->outfile_1D );
        RETURN(-1);
    }

    if ( ! sopt->no_head )
        print_header(fp, label, gv2s_map_names[sopt->map], sd);

    for ( c = 0; c < sd->nused; c++ )
    {
        /* keep old spacing */
        fputc(' ', fp);
        if ( sd->nodes  ) fprintf(fp, " %8d", sd->nodes[c]);
        if ( sd->volind ) fprintf(fp, "   %8d ", sd->volind[c]);
        if ( sd->i      ) fprintf(fp, "  %3d", sd->i[c]);
        if ( sd->j      ) fprintf(fp, "  %3d", sd->j[c]);
        if ( sd->k      ) fprintf(fp, "  %3d", sd->k[c]);
        if ( sd->nvals  ) fprintf(fp, "     %3d", sd->nvals[c]);

        for ( c2 = 0; c2 < sd->max_vals; c2++ )
            fprintf(fp, "  %10s", MV_format_fval(sd->vals[c2][c]));
        fputc('\n', fp);
    }

    fclose(fp);

    RETURN(0);
}


/*----------------------------------------------------------------------
 * v2s_write_outfile_niml       - write results to niml file
 *                              - free data pointers as we go
 *----------------------------------------------------------------------
*/
int v2s_write_outfile_niml ( v2s_opts_t * sopt, v2s_results * sd, int free_vals)
{
    static char   v2s_name[] = "3dVol2Surf_dataset";
    NI_element  * nel = NULL;
    NI_stream     ns;
    char        * ni_name;
    int           c;

ENTRY("v2s_write_outfile_niml");

    if ( !sopt->outfile_niml ) RETURN(0);

    nel = NI_new_data_element( v2s_name, sd->nused );
    if ( !nel )
    {
        fprintf(stderr,"** file NI_new_data_element, n = '%s', len = %d\n",
                v2s_name, sd->nused);
        RETURN(1);
    }

    ni_name = (char *)calloc(strlen(sopt->outfile_niml)+6, sizeof(char));
    if ( !ni_name ) { fprintf(stderr,"** ni_name failed\n"); RETURN(1); }
    sprintf(ni_name, "file:%s", sopt->outfile_niml);

    ns = NI_stream_open(ni_name, "w");

    NI_add_column(nel,NI_INT,sd->nodes);

    for ( c = 0; c < sd->max_vals; c++ )
    {
        NI_add_column(nel, NI_FLOAT, sd->vals[c]);
        if ( free_vals ) { free(sd->vals[c]); sd->vals[c] = NULL; }
    }
    if ( free_vals ) { free(sd->vals); sd->vals = NULL; }

    if ( NI_write_element(ns, nel, NI_BINARY_MODE) < 0 )
    {
        fprintf(stderr,"** NI_write_element failed for: '%s'\n", ni_name);
        RETURN(1);
    }

    NI_free_element( nel );
    NI_stream_close( ns );
    free(ni_name);

    RETURN(0);
}


/*----------------------------------------------------------------------
 * v2s_write_outfile_NSD        - write results to NI_SURF_DSET file
 *----------------------------------------------------------------------
*/
int v2s_write_outfile_NSD(v2s_results *sd, v2s_opts_t * sopt, 
                          v2s_param_t * p, int free_vals)
{
    SUMA_DSET  * sdset;
    NI_element * nel;
    void      ** elist = NULL;
    char       * oname, * statsym;
    int          c, rv, ind;

ENTRY("v2s_write_outfile_NSD");

    if ( !sd || !p || !sopt || !sopt->outfile_niml ) RETURN(1);

    /* create an empty dataset without an idcode or domain string */
    sdset = SUMA_CreateDsetPointer(sopt->outfile_niml, SUMA_NODE_BUCKET,
                                   NULL, NULL, sd->nused);
    /* add node indices, if they exist */
    if( sd->nodes )
    {
        rv = SUMA_AddDsetNelCol(sdset, "Node Indices", SUMA_NODE_INDEX,
                                (void *)sd->nodes, NULL, 1);
        if( !rv ){ fprintf(stderr,"** WO_NSD add nodes failure\n"); RETURN(1); }

        if( free_vals ){ free(sd->nodes); sd->nodes = NULL; }
        if( sopt->debug>1 ) fprintf(stderr,"+d adding node indices to NSD\n");
    }

    for( c = 0; c < sd->max_vals; c++ )
    {
        rv = SUMA_AddDsetNelCol(sdset, sd->labels[c],
                                SUMA_NODE_FLOAT, sd->vals[c],NULL,1);
        if( !rv ){ fprintf(stderr,"** WO_NSD add col failure\n"); RETURN(1); }
        if( free_vals ){ free(sd->vals[c]); sd->vals[c] = NULL; }
    }

    /* add history (may need to fake it) */
    SUMA_AddNgrHist(sdset->ngr,
                    sopt->cmd.fake ? "Vol2Surf_plugin" : "3dVol2Surf",
                    sopt->cmd.argc, sopt->cmd.argv);

    set_ni_globs_from_env();   /* init niml globals from environment */
    set_gni_debug(sopt->debug);

    /*--- add COLMS_STATSYM (probably already there) ---*/
    if( sopt->gp_index >= 0 ) ind = sopt->gp_index;  /* pick one sub-brick  */
    else if( p->over_steps )  ind = 0;               /* must be sub-brick 0 */
    else                      ind = -1;              /* get all sub-bricks  */
    statsym = THD_make_statsym_string(p->gpar, ind);
    if( statsym ) {
        nel = SUMA_FindDsetAttributeElement(sdset, "COLMS_STATSYM");
        if( nel ) { /* if it exists, replace the old, else make one */
            SUMA_NEL_REPLACE_STRING(nel, 0, 0, statsym);
            if( sopt->debug > 2 )
                fprintf(stderr,"++ replaced COLMS_STATSYM with '%s'\n",statsym);
        } else {
            nel = NI_new_data_element("AFNI_atr", 1);
            NI_set_attribute(nel,"atr_name", "COLMS_STATSYM");
            NI_add_column(nel, NI_STRING, &statsym);
            NI_add_to_group(sdset->ngr, nel);
            if( sopt->debug > 2 )
                fprintf(stderr,"++ added COLMS_STATSYM as '%s'\n", statsym);
        }
        free(statsym);
    } else if ( sopt->debug > 1 )
        fprintf(stderr,"** failed to make statsym_string...\n");

    /* find the data element and set the output format */
    c = NI_search_group_shallow(sdset->ngr, "SPARSE_DATA", &elist);
    if( c == 1 && (nel = (NI_element *)elist[0]) )
        { NI_free(elist); set_sparse_data_attribs(nel, p->gpar, 0); }
    else
        fprintf(stderr, "** WO_NSD: missing SPARSE_DATA?\n");

    oname = SUMA_WriteDset_ns(sopt->outfile_niml, sdset, 
                              SUMA_NO_DSET_FORMAT, 1,1);
    if(sopt->debug && oname) fprintf(stderr,"+d wrote NI_SURF_DSET %s\n",oname);

    SUMA_FreeDset(sdset);

    RETURN(0);
}


/*----------------------------------------------------------------------
 * print_header    - dump standard header for node output         - v2.4
 *----------------------------------------------------------------------
*/
static int print_header(FILE * outfp, char * surf, char * map, v2s_results * sd)
{
    int val;

ENTRY("print_header");

    fprintf( outfp, "# --------------------------------------------------\n" );
    fprintf( outfp, "# surface '%s', '%s' :\n", surf, map );
    fprintf( outfp, "#\n" );

    /* keep old style, but don't presume all columns get used (v 6.0) :
     *     fprintf( outfp, "#    node     1dindex    i    j    k     vals" );
     *     fprintf( outfp, "#   ------    -------   ---  ---  ---    ----" );
     */

    /* output column headers */
    fputc( '#', outfp );        /* still comment line */

    if ( sd->nodes  ) fprintf(outfp, "    node ");
    if ( sd->volind ) fprintf(outfp, "    1dindex ");
    if ( sd->i      ) fprintf(outfp, "   i ");
    if ( sd->j      ) fprintf(outfp, "   j ");
    if ( sd->k      ) fprintf(outfp, "   k ");
    if ( sd->nvals  ) fprintf(outfp, "    vals");

    for ( val = 0; val < sd->max_vals; val++ )
        fprintf( outfp, "       v%-2d  ", val );
    fputc( '\n', outfp );

    fputc( '#', outfp );
    /* underline the column headers */
    if ( sd->nodes  ) fprintf(outfp, "   ------");
    if ( sd->volind ) fprintf(outfp, "    ------- ");
    if ( sd->i      ) fprintf(outfp, "  ---");
    if ( sd->j      ) fprintf(outfp, "  ---");
    if ( sd->k      ) fprintf(outfp, "  ---");
    if ( sd->nvals  ) fprintf(outfp, "    ----");

    fputs( "   ", outfp );
    for ( val = 0; val < sd->max_vals; val++ )
        fprintf( outfp, " --------   " );
    fputc( '\n', outfp );

    RETURN(0);
}

static int v2s_free_cmd(v2s_opts_t * sopt)
{
    int c;

ENTRY("v2s_free_cmd");

    if( ! sopt->cmd.fake ) RETURN(0);
    if( sopt->cmd.argc <= 0 || !sopt->cmd.argv ) RETURN(0);

    for( c = 0; c < sopt->cmd.argc; c++ )
        if( sopt->cmd.argv[c] ) free(sopt->cmd.argv[c]);
    free(sopt->cmd.argv);

    sopt->cmd.fake = 0;
    sopt->cmd.argc = 0;
    sopt->cmd.argv = NULL;

    RETURN(0);
}

/*----------------------------------------------------------------------
 * create a command string from the passed structures 9 Aug 2006 [rickr]
 *
 * allocate and fill a string that might work as a command
 *----------------------------------------------------------------------
*/
int v2s_make_command( v2s_opts_t * opt, v2s_param_t * p )
{
    char ** argv = NULL, str[512];
    char  * dset_file = NULL, * sv_file = NULL;
    int     argc = 0, acnall = 0;

ENTRY("v2s_make_command");

    /* set the sv and grid_parent filenames */
    if(gv2s_plug_opts.sv_dset) sv_file = DSET_FILECODE(gv2s_plug_opts.sv_dset);
    else                       sv_file = "UNKNOWN_SURF_VOL";
    dset_file = DSET_FILECODE(p->gpar);

    /* start setting options (3dVol2Surf may get replaced) */
    loc_add_2_list(&argv, &acnall, &argc, "3dVol2Surf");

    loc_add_2_list(&argv, &acnall, &argc, "-spec");
    if( p->surf[0].spec_file[0] )
        loc_add_2_list(&argv, &acnall, &argc, p->surf[0].spec_file);
    else
        loc_add_2_list(&argv, &acnall, &argc, "NO_SPEC_FILE");

    loc_add_2_list(&argv, &acnall, &argc, "-surf_A");
    loc_add_2_list(&argv, &acnall, &argc, p->surf[0].label);
    if( p->nsurf == 2 ){
        loc_add_2_list(&argv, &acnall, &argc, "-surf_B");
        loc_add_2_list(&argv, &acnall, &argc, p->surf[1].label);
    }

    loc_add_2_list(&argv, &acnall, &argc, "-sv");
    loc_add_2_list(&argv, &acnall, &argc, sv_file);

    loc_add_2_list(&argv, &acnall, &argc, "-grid_parent");
    loc_add_2_list(&argv, &acnall, &argc, dset_file);
    loc_add_2_list(&argv, &acnall, &argc, "-gp_index");
    sprintf(str,"%d",opt->gp_index);
    loc_add_2_list(&argv, &acnall, &argc, str);

    loc_add_2_list(&argv, &acnall, &argc, "-map_func");
    loc_add_2_list(&argv, &acnall, &argc, gv2s_map_names[opt->map]);

    sprintf(str, "%d", opt->f_steps);
    loc_add_2_list(&argv, &acnall, &argc, "-f_steps");
    loc_add_2_list(&argv, &acnall, &argc, str);

    loc_add_2_list(&argv, &acnall, &argc, "-f_index");
    if( opt->f_index == V2S_INDEX_VOXEL )
        loc_add_2_list(&argv, &acnall, &argc, "voxels");
    else
        loc_add_2_list(&argv, &acnall, &argc, "nodes");

    if( gv2s_plug_opts.gpt_index >= 0 ){
        loc_add_2_list(&argv, &acnall, &argc, "-cmask");
        sprintf(str,"-a %s[%d] -expr astep(a,%f)+equals(a,%f)",
                dset_file, gv2s_plug_opts.gpt_index, gv2s_plug_opts.gpt_thresh,
                gv2s_plug_opts.gpt_thresh);
        loc_add_2_list(&argv, &acnall, &argc, str);
    }

    if( opt->first_node > 0 ){
        loc_add_2_list(&argv, &acnall, &argc, "-first_node");
        sprintf(str,"%d",opt->first_node);
        loc_add_2_list(&argv, &acnall, &argc, str);
    }

    if( opt->last_node > 0 && opt->last_node < p->surf[0].num_ixyz-1 ){
        loc_add_2_list(&argv, &acnall, &argc, "-last_node");
        sprintf(str,"%d",opt->last_node);
        loc_add_2_list(&argv, &acnall, &argc, str);
    }

    if( opt->use_norms ){
        loc_add_2_list(&argv, &acnall, &argc, "-use_norms");
        if( opt->norm_len != 0.0 ){
            loc_add_2_list(&argv, &acnall, &argc, "-norm_len");
            loc_add_2_list(&argv, &acnall, &argc,MV_format_fval(opt->norm_len));
        }
    }

    if( opt->f_p1_fr != 0 ){
        loc_add_2_list(&argv, &acnall, &argc, "-f_p1_fr");
        loc_add_2_list(&argv, &acnall, &argc,MV_format_fval(opt->f_p1_fr));
    }

    if( opt->f_pn_fr != 0 ){
        loc_add_2_list(&argv, &acnall, &argc, "-f_pn_fr");
        loc_add_2_list(&argv, &acnall, &argc,MV_format_fval(opt->f_pn_fr));
    }

    if( opt->f_p1_mm != 0 ){
        loc_add_2_list(&argv, &acnall, &argc, "-f_p1_mm");
        loc_add_2_list(&argv, &acnall, &argc,MV_format_fval(opt->f_p1_mm));
    }

    if( opt->f_pn_mm != 0 ){
        loc_add_2_list(&argv, &acnall, &argc, "-f_pn_mm");
        loc_add_2_list(&argv, &acnall, &argc,MV_format_fval(opt->f_pn_mm));
    }

    if( opt->oob.show && opt->oob.index > 0 ){
        loc_add_2_list(&argv, &acnall, &argc, "-oob_index");
        sprintf(str,"%d",opt->oob.index);
        loc_add_2_list(&argv, &acnall, &argc, str);
    }

    if( opt->oob.show && opt->oob.value != 0.0 ){
        loc_add_2_list(&argv, &acnall, &argc, "-oob_value");
        loc_add_2_list(&argv, &acnall, &argc,MV_format_fval(opt->oob.value));
    }

    if( DSET_NVALS(p->gpar) > 1 )
        loc_add_2_list(&argv, &acnall, &argc, "-outcols_afni_NSD");

    if( opt->debug ){
        loc_add_2_list(&argv, &acnall, &argc, "-debug");
        sprintf(str,"%d",opt->debug);
        loc_add_2_list(&argv, &acnall, &argc, str);
    }

    if( opt->dnode ){
        loc_add_2_list(&argv, &acnall, &argc, "-dnode");
        sprintf(str,"%d",opt->dnode);
        loc_add_2_list(&argv, &acnall, &argc, str);
    }

    if( opt->segc_file ){
        loc_add_2_list(&argv, &acnall, &argc, "-save_seg_coords");
        loc_add_2_list(&argv, &acnall, &argc, opt->segc_file);
    }

    if( opt->outfile_1D ){
        loc_add_2_list(&argv, &acnall, &argc, "-out_1D");
        loc_add_2_list(&argv, &acnall, &argc, opt->outfile_1D);
    }

    if( opt->outfile_niml ){
        loc_add_2_list(&argv, &acnall, &argc, "-out_niml");
        loc_add_2_list(&argv, &acnall, &argc, opt->outfile_niml);
    }

    /* insert this into the cmd struct */
    opt->cmd.fake = 1;
    opt->cmd.argc = argc;
    opt->cmd.argv = argv;

    RETURN(0);
}

/*----------------------------------------------------------------------
 * display command
 *----------------------------------------------------------------------
*/
int disp_v2s_command( v2s_opts_t * sopt )
{
    char * arg;
    int    ac, quote;

ENTRY("disp_v2s_command");

    if( sopt->cmd.argc <= 1 || ! sopt->cmd.argv || ! sopt->cmd.argv[0] )
        return 1;

    printf("------------------------------------------------------\n"
           "+d applying vol2surf similar to the following command:\n");
    for( ac = 0; ac < sopt->cmd.argc; ac++ )
        if( sopt->cmd.argv[ac] )
        {
            arg = sopt->cmd.argv[ac];
            /* if there are special char, quote the option */
            if( strchr(arg, '(') || strchr(arg, '[') ) quote = 1;
            else quote = 0;

            if( quote ) putchar('\'');
            fputs(arg, stdout);
            if( quote ) putchar('\'');
            putchar(' ');
        }
    putchar('\n');
    fflush(stdout);

    RETURN(0);
}


/*----------------------------------------------------------------------
 * add 'str' to 'list' via strcpy
 *
 * if list is not long enough, realloc more pointers
 *----------------------------------------------------------------------
*/
static int loc_add_2_list( char *** list, int * nall, int * len, char * str)
{
ENTRY("loc_add_2_list");
    if( !list || !nall || !len || !str ) RETURN(-1);

    if( *nall <  0 ) *nall = 0;
    if( *nall == 0 ){ *list = NULL;  *nall = 0;  *len = 0; }  /* init */

    if( *nall <= *len ) /* then allocate more memory */
    {
        *nall += 32;
        *list = (char **)realloc(*list, *nall * sizeof(char *));
        if(!*list){ fprintf(stderr,"** LA2L: cannot alloc list\n"); RETURN(1); }
    }

    /* add the string to the list */
    (*list)[*len] = strdup(str);
    (*len)++;

    RETURN(0);
}

