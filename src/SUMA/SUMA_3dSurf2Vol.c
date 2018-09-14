/*----------------------------------------------------------------------
 * 3dSurf2Vol - create an AFNI volume dataset from a surface
 *
 * This program is meant to take as input a surface and an AFNI dataset,
 * and to output a new AFNI dataset consisting of the surface mapped to
 * the dataset grid space.
 *
 * The surface points are mapped to xyz coordinates, according to the
 * SURF_VOL (surface volume) AFNI dataset.  These coordinates are then
 * matched to voxels in the grid parent AFNI dataset.  The output dataset
 * will have the same grid as the grid parent dataset, with values coming
 * from the surface, and subject to any mask (see -cmask).
 *
 * usage:
 *    3dSurf2Vol [options] -spec SPEC_FILE -sv SURF_VOL -grid_parent AFNI_DSET
 *                         -map_func MAP_FUNC -prefix OUTPUT_DSET
 *
 * options:
 *
 *      -help
 *      -version
 *
 *      -spec                SPEC_FILE
 *      -surf_A              SURF_NAME_A
 *      -surf_B              SURF_NAME_B
 *      -sv                  SURF_VOL
 *      -grid_parent         AFNI_DSET
 *      -prefix              OUTPUT_DSET
 *      -map_func            MAP_FUNC
 *
 *      -surf_xyz_1D         SURF_XYZ.1D
 *      -sdata_1D            SURF_DATA.1D
 *
 *      -cmask               MASK_COMMAND
 *      -data_expr           EXPRESSION
 *      -datum               D_TYPE
 *      -debug               LEVEL
 *      -dnode               NODE_NUM
 *      -dvoxel              VOXEL_NUM
 *      -noscale
 *
 *      -f_steps             SEGMENT_STEPS
 *      -f_index             STEP_TYPE
 *      -f_p1_fr             FRACTION
 *      -f_pn_fr             FRACTION
 *      -f_p1_mm             DISTANCE
 *      -f_pn_mm             DISTANCE
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

static char g_history[] =
 "----------------------------------------------------------------------\n"
 "history:\n"
 "\n"
 "1.0  May 29, 2003 [rickr]\n"
 "  - initial release\n"
 "\n"
 "1.1  June 11, 2003 [rickr]\n"
 "  - small reorg of s2v_fill_mask2() (should have no effect)\n"
 "  - improve description of -f_steps option\n"
 "\n"
 "1.2  July 21, 2003 [rickr]\n"
 "  - make sure input points fit in output dataset\n"
 "  - add min/max distance output, along with out-of-bounds count\n"
 "\n"
 "2.0  October 2, 2003 [rickr]\n"
 "  - Major changes accepting surface data, surface coordinates, output data\n"
 "    type, debug options, multiple sub-brick output, and node pair segment\n"
 "    alterations.\n"
 "  - Added many options:  -surf_xyz_1D, -sdata_1D, -data_expr, -datum,\n"
 "                         -dnode, -dvoxel, -f_index, -f_p1_fr, -f_pn_fr,\n"
 "                         -f_p1_mm, -f_pn_mm\n"
 "\n"
 "2.1  October 20, 2003 [rickr]\n"
 "  - call the new engine function, SUMA_LoadSpec_eng()\n"
 "    (this will restrict the debug output from SUMA_LoadSpec())\n"
 "\n"
 "2.2  December 15, 2003 [rickr]\n"
 "  - added program arguments '-surf_A' and '-surf_B' (-surf_A is required)\n"
 "  - added option '-hist' (for program history)\n"
 "  - explicit pointer init to NULL (a.o.t. memset() to 0)\n"
 "\n"
 "3.0  December 18, 2003 [rickr]\n"
 "  - removed requirement of 2 surfaces for most functions\n"
 "    (so now all functions work with either 1 or 2 input surfaces)\n"
 "\n"
 "3.1  January 23, 2004 [rickr]\n"
 "  - SUMA_isINHmappable() is depreciated, check with AnatCorrect field\n"
 "  - reversed order of output for '-hist' option\n"
 "\n"
 "3.2  February 10, 2004 [rickr]\n"
 "  - output a little more debug info for !AnatCorrect case\n"
 "\n"
 "3.3  March 26, 2004  [ziad]\n"
 "  - DsetList added to SUMA_LoadSpec_eng() call\n"
 "\n"
 "3.4  June 21, 2004  [rickr]\n"
 "  - Fixed -surf_xyz_1D option (broken in v3.0 on nsurf test).\n"
 "\n"
 "3.5  July 22, 2004  [rickr]\n"
 "  - Fixed bug with test for valid sdata_1D file.\n"
 "\n"
 "3.6  July 28, 2004  [rickr]\n"
 "  - Fixed bug: old change caused the default f_steps to revert to 1,\n"
 "               now it is set back to 2 (thanks, Kuba).\n"
 "\n"
 "3.6a March 22, 2005  [rickr]\n"
 "  - removed tabs\n"
 "\n"
 "3.7  November 4, 2011  [rickr]\n"
 "  - 6.5 years since the last change?  really??\n"
 "  - added 'mode' mapping function\n"
 "  - for R Mruczek and Z Puckett\n"
 "\n"
 "3.8  April 28, 2018  [rickr]\n"
 "  - 7.5 years since the last change, again??  done until late 2024?\n"
 "  - added -stop_gap option\n"
 "  - added mapping functions: nzave, nzmode, median, nzmedian\n"
 "\n"
 "3.9  September 13, 2018  [rickr]\n"
 "  - return 0 on terminal options\n"
 "----------------------------------------------------------------------\n";

#define VERSION "version  3.9 (September 13, 2018)"


/*----------------------------------------------------------------------
 * todo:
 *   - handle niml input
 *----------------------------------------------------------------------
*/

#include "mrilib.h"
#include "parser.h"
#include "SUMA_suma.h"
#include "SUMA_3dSurf2Vol.h"

/* globals */
SUMA_SurfaceViewer * SUMAg_SVv = NULL;  /* array of Surf View structs   */
int                  SUMAg_N_SVv = 0;   /* length of SVv array          */
SUMA_DO            * SUMAg_DOv = NULL;  /* array of Displayable Objects */
int                  SUMAg_N_DOv = 0;   /* length of DOv array          */
SUMA_CommonFields  * SUMAg_CF = NULL;   /* info common to all viewers   */

/* this must match s2v_map_num enum */
char * gs2v_map_names[] = { "none", "mask", "mask2", "ave", "nzave", "count",
                            "min", "max", "max_abs", "mode", "nzmode",
                            "median", "nzmedian" };

/* AFNI prototype */
extern void machdep( void );

#define MAIN

/*----------------------------------------------------------------------*/

int main( int argc , char * argv[] )
{
    SUMA_SurfSpecFile  spec;
    node_list_t        node_list = { NULL, 0, 0, NULL, NULL, 0 };
    param_t            params;
    s2v_opts_t         sopt;
    opts_t             opts;
    int                ret_val;

    mainENTRY("3dSurf2Vol main");
    machdep();
    AFNI_logger("3dSurf2Vol",argc,argv);

    /* validate inputs and init options structure */
    if ( ( ret_val = init_options(&opts, argc, argv) ) != 0 ) {
        if( ret_val > 0 ) return 0;
        else              return 1;
    }

    if ( ( ret_val = validate_options(&opts, &params) ) != 0 )
        return ret_val;

    if ( (ret_val = set_smap_opts( &opts, &params, &sopt )) != 0 )
        return ret_val;

    /* initialize the spec ZSS Jan 9 06*/
    if (!SUMA_AllocSpecFields(&spec)) {
       fprintf( stderr, "** failed to initialize spec\n" );
       return(-1);
    }

    /* read surface files */
    ret_val = read_surf_files(&opts, &params, &spec, &sopt, &node_list);

    if ( ret_val == 0 )
        ret_val = write_output( &sopt, &opts, &params, &node_list, argc, argv );

    /* free memory */
    final_clean_up( &node_list );

    /* free the spec ZSS Jan 9 06*/
    if (!SUMA_FreeSpecFields(&spec)) {
       fprintf( stderr, "** failed to free spec\n" );
       return(-1);
    }

    return ret_val;
}


/*----------------------------------------------------------------------
 * write_output - create and write a new afni dataset
 *----------------------------------------------------------------------
*/
int write_output ( s2v_opts_t * sopt, opts_t * opts, param_t * p,
                   node_list_t * N, int argc, char * argv[] )
{
ENTRY("write_output");

    if ( sopt == NULL || opts == NULL || p == NULL || N == NULL )
    {
        fprintf( stderr, "** s2v_wo - bad params (%p,%p,%p,%p)\n",
                 sopt, opts, p, N );
        RETURN(-1);
    }

    p->oset = s2v_nodes2volume( N, p, sopt );

    if ( p->oset == NULL )
        RETURN(-1);

    EDIT_dset_items( p->oset, ADN_prefix, opts->oset_file, ADN_none );

    /* no longer check for output dataset, let it fail or succeed
     *                          for Mike B    9 Aug, 2009 [rickr] */

    tross_Copy_History( p->gpar, p->oset );
    tross_Make_History( PROG_NAME, argc, argv, p->oset );

    if ( DSET_write( p->oset ) != True )
    {
        fprintf( stderr, "** failed to write dataset '%s', exiting...\n",
                 opts->oset_file );
        RETURN(-1);
    }

    RETURN(0);
}


/*----------------------------------------------------------------------
 * s2v_nodes2volume     - create an AFNI dataset from the node_list
 *                        and map type (subject to any mask).
 *
 * inputs:
 *              N       - xyz coordinate node list for surfaces
 *              p       - param_t struct
 *              map     - the surface to dataset mapping type
 *
 * return  AFNI dataset - on success
 *         NULL         - on failure
 *----------------------------------------------------------------------
*/
THD_3dim_dataset * s2v_nodes2volume( node_list_t * N, param_t * p,
                                     s2v_opts_t * sopt )
{
    THD_3dim_dataset * dout;
    THD_fvec3        * pary;
    double           * ddata;
    void             * vdata = NULL;
    int              * cdata;
    aggr_list_t        aggr = {NULL};
    float              fac;
    int                nvox, dsize, valid;
    int                sub, vind;

ENTRY("s2v_nodes2volume");

    if ( N == NULL || ! ISVALID_DSET(p->gpar) )
    {
        fprintf(stderr,"** s2v_nodes2volume: bad params (%p,%p)\n", N, p->gpar);
        RETURN(NULL);
    }

    dout = EDIT_empty_copy( p->gpar );
    if ( ! ISVALID_3DIM_DATASET( dout ) )
    {
        fprintf( stderr, "** failed EDIT_empty_copy()\n" );
        RETURN(NULL);
    }

    /* output dataset will have nsubs sub-bricks */
    EDIT_dset_items( dout, ADN_nvals,     p->nsubs,
                           ADN_type,      HEAD_FUNC_TYPE,
                           ADN_func_type, FUNC_BUCK_TYPE,
                           ADN_datum_all, sopt->datum,
                           ADN_ntt,       0,
                     ADN_none );

    if ( sopt->debug > 1 )
        fprintf( stderr, "++ creating dataset '%s' of type '%s', nvals = %d\n",
                 DSET_HEADNAME(dout), MRI_TYPE_name[sopt->datum], p->nsubs );

    nvox  = DSET_NVOX(p->gpar);

    /* allocate a computational array of doubles */
    if ( (ddata = (double *)malloc(nvox * sizeof(double))) == NULL )
    {
        fprintf( stderr, "** n2v: failed to allocate %d doubles\n", nvox );
        DSET_delete( dout );
        RETURN(NULL);
    }

    /* allocate an array of ints for functional counting */
    if ( (cdata = (int *)malloc(nvox * sizeof(int))) == NULL )
    {
        fprintf( stderr, "** n2v: failed to allocate %d ints\n", nvox );
        DSET_delete( dout );  free( ddata );
        RETURN(NULL);
    }

    /* allocate space for the point list */
    if ( (pary = (THD_fvec3 *)malloc(sopt->f_steps*sizeof(THD_fvec3))) == NULL )
    {
        fprintf(stderr,"** n2v: cannot allocate %d THD_fvec3s\n",sopt->f_steps);
        DSET_delete( dout );  free( ddata );  free( cdata );
        RETURN(NULL);
    }

    /* if we need to aggregate values, create arrays */
    aggr.vlist = NULL;
    if( is_aggregate_type(sopt->map) ) {
        aggr.vlist = (float_list *)malloc(nvox*sizeof(float_list));
        if( ! aggr.vlist ) {
            fprintf(stderr,"** failed to alloc %d elem float_list\n", nvox);
            DSET_delete( dout );  free( ddata );  free( cdata );
            RETURN(NULL);
        }
        for( vind = 0; vind < nvox; vind++ )
            if( init_float_list(aggr.vlist+vind, 4) != 4 ) {
                fprintf(stderr,"** failed to init float_list %d\n", vind);
                DSET_delete( dout );  free( ddata );  free( cdata );
                RETURN(NULL);
            }
    }

    dsize = mri_datum_size(sopt->datum);
    /* create the sub-brick data for output */
    for ( sub = 0; sub < p->nsubs; sub++ )
    {
        valid = 0;

        /* need new data after each sub-brick is attached to the dset */
        if ( (vdata = malloc( nvox * dsize )) == NULL )
        {
            fprintf( stderr, "** failed to allocate %d bytes for vdata\n",
                     nvox * dsize );
            DSET_delete( dout );  free( ddata );  free( cdata );
            RETURN(NULL);
        }

        /* init data to zeros */
        memset(cdata, 0, nvox*sizeof(int));
        memset(ddata, 0, nvox*sizeof(double));

        /* set node_list data */
        if ( set_node_list_data( N, p, sopt, sub ) != 0 )
        {
            DSET_delete(dout);
            free(ddata);
            free(cdata);
            free(vdata);

            RETURN(NULL);
        }

        /* if we have aggregate list, clear (ignore) the contents */
        if( is_aggregate_type(sopt->map) && aggr.vlist )
            for( vind = 0; vind < nvox; vind++ ) aggr.vlist[vind].num = 0;

        if ( compute_results( p, N, sopt, ddata, cdata, pary, &aggr ) == 0 )
            valid = 1;

        if ( ! valid )  /* then clean up memory */
        {
            free( ddata );
            free( vdata );
            DSET_delete( dout );
            RETURN(NULL);
        }

        /* convert to output data type */
        fac = 0.0;

        if ( sopt->debug > 1 )
            fprintf(stderr,"++ sub-brick %d, integral_doubles = %d\n",
                    sub, integral_doubles( ddata, nvox ));

        /* for int output and scaling, fac is maxval/maxabs */
        if ( MRI_IS_INT_TYPE( sopt->datum ) && !sopt->noscale )
        {
            float amax = MCW_vol_amax( nvox, 1, 1, MRI_double, ddata );

            if ( amax > MRI_TYPE_maxval[sopt->datum] )      /* we must scale */
                fac = MRI_TYPE_maxval[sopt->datum]/amax;
            else if ( integral_doubles( ddata, nvox ) )  /* don't need scale */
                fac = 0.0;
            else if ( amax != 0.0 )
                fac = MRI_TYPE_maxval[sopt->datum]/amax;

            if ( sopt->debug > 1 )
                fprintf(stderr,"++ fac = %f, amax = %f \n", fac, amax);
        }

        EDIT_coerce_scale_type(nvox, fac, MRI_double,ddata, sopt->datum,vdata);

        /* attach the final vdata to the dataset */
        EDIT_substitute_brick( dout, sub, sopt->datum, vdata );
        DSET_BRICK_FACTOR( dout, sub ) = (fac != 0.0) ? 1.0/fac : 0.0;
    }

    if (sopt->debug > 0)
        fprintf(stderr,"++ %d sub-brick(s) computed\n", p->nsubs);

    /* if we have an aggregate list, free it */
    if( is_aggregate_type(sopt->map) && aggr.vlist ) {
        for( vind = 0; vind < nvox; vind++ ) free_float_list(aggr.vlist+vind);
        free(aggr.vlist);
    }

    free(ddata);
    free(cdata);
    free(pary);

    RETURN(dout);
}

/*----------------------------------------------------------------------
 * compute_results   - fill ddata with results, cdata with count
 *
 * return     0 : success
 *         else : failure
 *----------------------------------------------------------------------
*/
int compute_results( param_t * p, node_list_t * N, s2v_opts_t * sopt,
                         double * ddata, int * cdata, THD_fvec3 * pary,
                         aggr_list_t * aggr )
{
    THD_fvec3          p1, pn;
    float              dist, min_dist, max_dist;
    int                nindex, node;
    int                oobc;

ENTRY("compute_results");

    if ( !p || !N || !sopt || !ddata || !cdata )
    {
        fprintf(stderr,"** cr: bad params (%p,%p,%p,%p,%p)\n",
                p, N, sopt, ddata, cdata);
        RETURN(-1);
    }

    min_dist = 9999.9;
    max_dist = -1.0;
    oobc     = 0;               /* init out-of-bounds counter */

    for ( nindex = 0; nindex < N->ilen; nindex++ )
    {
        node = N->ilist[nindex];
        if ( node < 0 || node >= N->nnodes )
        {
            fprintf(stderr,"** node %d (%d) out-of-range [0,%d]\n",
                    nindex, node, N->nnodes);
            RETURN(-1);
        }

        /* initiate the endpoints */
        p1 = N->nodes[node];

        if ( N->depth > 1 )
            pn = N->nodes[node+N->nnodes];
        else
            pn = p1;

        if ( ! sopt->sxyz_ori_gpar ) /* if orient is not as gpar it is Dicom */
        {
            p1 = THD_dicomm_to_3dmm(p->gpar, p1);
            pn = THD_dicomm_to_3dmm(p->gpar, pn);
        }

        if ( sopt->debug > 0 && sopt->dnode == node )
        {
            fprintf(stderr,"-- debug node: %d\n", node );
            fprintf(stderr,"-- orig endpts: (%f, %f, %f)\n"
                           "                (%f, %f, %f)\n",
                    p1.xyz[0], p1.xyz[1], p1.xyz[2],
                    pn.xyz[0], pn.xyz[1], pn.xyz[2]);
        }

        adjust_endpts( sopt, &p1, &pn );        /* per user options */

        /* if both points are outside gpar box, skip the node */
        if ( f3mm_out_of_bounds( &p1, &p->f3mm_min, &p->f3mm_max ) &&
             f3mm_out_of_bounds( &pn, &p->f3mm_min, &p->f3mm_max ) )
        {
            oobc++;
            continue;
        }

        dist = dist_f3mm( &p1, &pn );
        if ( dist < min_dist ) min_dist = dist;
        if ( dist > max_dist ) max_dist = dist;

        make_point_list( pary, &p1, &pn, sopt );

        /* do all the work to insert data */
        if ( insert_list(N, p, sopt, pary, nindex, ddata, cdata, aggr) )
            RETURN(-1);
    }

    if ( sopt->debug > 0 )
        fprintf(stderr, "-- (min_dist, max_dist) = (%f, %f)\n"
                        "   %d of %d nodes were out of bounds\n",
                min_dist, max_dist, oobc, N->ilen);

    if ( final_computations(ddata, cdata, sopt, DSET_NVOX(p->gpar), aggr) )
        RETURN(-1);

    RETURN(0);
}

/* just compare 2 floats */
int fcomp(const void *f0, const void *f1)
{
    float * fp0 = (float *)f0;
    float * fp1 = (float *)f1;

    if( *fp0  < *fp1 ) return -1;
    if( *fp0 == *fp1 ) return 0;
    return 1;
}


/*----------------------------------------------------------------------
 * final_computations   - perform any last computation over the dataset
 *----------------------------------------------------------------------
*/
int final_computations(double *ddata, int *cdata, s2v_opts_t *sopt, int nvox,
                       aggr_list_t * aggr)
{
    float_list * flp = NULL;
    int index;

ENTRY("final_computations");

    if ( (sopt->debug > 1) && (sopt->dvox >= 0) && (sopt->dvox < nvox) )
        fprintf(stderr,"++ final: voxel %d, from values (%d, %f) ",
                sopt->dvox,cdata[sopt->dvox],ddata[sopt->dvox]);

    switch ( sopt->map )
    {
        default:
            fprintf(stderr,"** fc: mapping %d not ready\n", sopt->map );
            RETURN(-1);

        case E_SMAP_AVE:
        case E_SMAP_NZ_AVE:
            /* we have sum, divide for average */
            for ( index = 0; index < nvox; index++ )
                if ( cdata[index] > 0 )
                    ddata[index] /= cdata[index];
            break;

        case E_SMAP_NZ_MEDIAN:
        case E_SMAP_MEDIAN:
        {
            int mind;
            float mval;
            /* for each voxel, sort list and aggregate */
            for ( index = 0, flp = aggr->vlist; index < nvox; index++, flp++ ) {
                /* handle empty or single element list separately */
                if( flp->num == 0 ) {
                    ddata[index] = 0.0;
                    if( sopt->debug > 1 && sopt->dvox == index )
                        fprintf(stderr, "\n-- voxel %d, nothing to aggregate\n",
                                index);
                    continue;
                } else if ( flp->num == 1 ) {
                    ddata[index] = flp->list[0];
                    if( sopt->debug > 1 && sopt->dvox == index ) {
                        fprintf(stderr, "\n-- voxel %d, one val to aggregate\n",
                                index);
                        fprintf(stderr, "++ final median %g\n", ddata[index]);
                    }
                    continue;
                }

                /* at least 2, sort and assign */
                qsort(flp->list, flp->num, sizeof(float), fcomp);

                /* mid index = floor of length/2 */
                /* (correct (as floor) if odd, higher of pair if even) */
                mind = flp->num/2;

                /* assign median as value or average of middle two */
                if( flp->num % 2 )
                    ddata[index] = flp->list[mind];
                else
                    ddata[index] = (flp->list[mind-1] + flp->list[mind])/2.0;

                /* announce the glorious action to the world */
                if( sopt->debug > 1 && sopt->dvox == index ) {
                    fprintf(stderr, "\n-- aggregating voxel %d, %d vals "
                                    "from %g to %g\n",
                                    index, flp->num, flp->list[0],
                                    flp->list[flp->num-1]);
                    fprintf(stderr, "++ final median %g\n", ddata[index]);
                }
            }
            break;
        }

        case E_SMAP_NZ_MODE:
        case E_SMAP_MODE:
        {
            int mcount, ncount, find;
            float mval, nval;
            /* for each voxel, sort list and aggregate */
            for ( index = 0, flp = aggr->vlist; index < nvox; index++, flp++ ) {
                /* if nothing here, set to 0 and continue */
                if( flp->num == 0 ) {
                    ddata[index] = 0.0;
                    if( sopt->debug > 1 && sopt->dvox == index )
                        fprintf(stderr, "\n-- voxel %d, nothing to aggregate\n",
                                index);
                    continue;
                }
                qsort(flp->list, flp->num, sizeof(float), fcomp);

                /* get initial max count */
                find = 0; /* current index */
                mval = flp->list[find];
                while(find < flp->num && mval == flp->list[find]) find++;
                mcount = find; /* initial max is current index */

                /* now continue looking for more */
                while(find < flp->num) {
                    ncount = 1; nval = flp->list[find];
                    find++;
                    while(find < flp->num && nval == flp->list[find]) {
                        find++;
                        ncount++;
                    }
                    if( ncount > mcount ) {      /* we have a new max! */
                        mcount = ncount;  mval = nval;
                    }
                }
                ddata[index] = mval;    /* and finally, assign */

                if( sopt->debug > 1 && sopt->dvox == index ) {
                    fprintf(stderr, "\n-- aggregating voxel %d, %d vals "
                                    "from %g to %g\n",
                                    index, flp->num, flp->list[0],
                                    flp->list[flp->num-1]);
                    fprintf(stderr, "++ final mode %g (%d vals)\n",mval,mcount);
                }

            }
            break;
        }

        case E_SMAP_COUNT:
        case E_SMAP_MAX_ABS:
        case E_SMAP_MAX:
        case E_SMAP_MIN:
        case E_SMAP_MASK:
        case E_SMAP_MASK2:
            break;
    }

    if ( (sopt->debug > 1) && (sopt->dvox >= 0) && (sopt->dvox < nvox) )
        fprintf(stderr,"to values (%d, %f)\n",
                cdata[sopt->dvox],ddata[sopt->dvox]);

    RETURN(0);
}


/*----------------------------------------------------------------------
 * insert_list          - given xyz list, add to ddata/cdata
 *
 * for each point in list
 *   get voxel index
 *   if out of mask, skip
 *   if undesired voxel repeat, skip
 *   apply mapping function to insert data
 *
 * return   0 on success
 *        < 0 on error
 *----------------------------------------------------------------------
*/
int insert_list( node_list_t * N, param_t * p, s2v_opts_t * sopt,
                 THD_fvec3 * pary, int nindex, double * ddata, int * cdata,
                 aggr_list_t * aggr )
{
    THD_ivec3 i3;
    int       vindex, prev_vind;
    int       step, node;
    int       nx, ny, debug;
    int       pre_gap = 0;  /* for stop_gap, along segment, signal seeing */
                            /* non-zero mask */

ENTRY("insert_list");

    if ( !N || !p || !sopt || !pary || nindex < 0 || !ddata || !cdata )
    {
        fprintf(stderr,"** ID params (%p,%p,%p,%p,%d,%p,%p)\n",
                N, p, sopt, pary, nindex, ddata, cdata);
        RETURN(-1);
    }

    node = N->ilist[nindex];

    debug = sopt->debug && node == sopt->dnode;

    if ( debug )    /* show xyz coords? */
        fprintf(stderr,"++ point list for N[%d] = %d:\n", nindex, node);

    nx        = DSET_NX(p->gpar);
    ny        = DSET_NY(p->gpar);
    prev_vind = -1;
    for ( step = 0; step < sopt->f_steps; step++ )
    {
        if ( f3mm_out_of_bounds( pary+step, &p->f3mm_min, &p->f3mm_max ) )
            continue;

        i3 = THD_3dmm_to_3dind(p->gpar, pary[step]);
        vindex = i3.ijk[0] + nx * (i3.ijk[1] + ny * i3.ijk[2]);

        if ( debug )
            fprintf(stderr,"   %3d, vox %d, [%3d,%3d,%3d]: %f, %f, %f",
                    step, vindex, i3.ijk[0], i3.ijk[1], i3.ijk[2],
                    pary[step].xyz[0], pary[step].xyz[1], pary[step].xyz[2]);

        if ( sopt->cmask && !sopt->cmask[vindex] )
        {
            if ( debug ) fprintf(stderr, " : skip (mask)\n");
            /* if stop_gap, out of mask after in mask, so bail */
            if( sopt->stop_gap && pre_gap ) break;
            continue;
        }
        /* only applies to stop_gap */
        if( ! pre_gap ) pre_gap = 1;

        if ( (vindex == prev_vind) && (sopt->f_index == S2V_F_INDEX_VOXEL) )
        {
            if ( debug ) fprintf(stderr, " : skip (repeat voxel)\n");
            continue;
        }

        /* hey, we finally have a new voxel to add */

        prev_vind = vindex;

        if ( debug )
            fprintf( stderr, " : (old) %d %f", cdata[vindex],ddata[vindex]);

        if (insert_value(sopt, ddata, cdata, vindex, node, N->fdata[nindex],
                         aggr))
            RETURN(-1);

        if ( debug )
            fprintf( stderr, " : (new) %d %f\n", cdata[vindex],ddata[vindex]);
    }

    RETURN(0);
}

/* aggregate types are ones for which:                  3 Nov 2011 [rickr]
 *    for each voxel
 *        - accumulate all node values
 *        - aggregate into final voxel value (e.g. mode, median, rand)
 */
int is_aggregate_type(int map_func)
{
    switch( map_func ) {
       case E_SMAP_MODE:
       case E_SMAP_NZ_MODE:
       case E_SMAP_MEDIAN:
       case E_SMAP_NZ_MEDIAN:
          return 1;
    }

    return 0;
}


/*----------------------------------------------------------------------
 * insert_value         - insert value based on user function
 *
 * return   0 on success
 *        < 0 on error
 *----------------------------------------------------------------------
*/
int insert_value(s2v_opts_t * sopt, double *dv, int *cv, int vox, int node,
                 float value, aggr_list_t * aggr)
{
ENTRY("insert_value");

    if ( !sopt || !dv || !cv || vox < 0 )
    {
        fprintf(stderr, "** IV: bad params (%p,%p,%p,%d)\n",sopt,dv,cv,vox);
        RETURN(-1);
    }

    if ( (sopt->debug > 1) && (vox == sopt->dvox) )
        fprintf(stderr,"++ voxel %d, node %d, values (%d, %f) ",
                vox, node, cv[vox], dv[vox]);

    switch ( sopt->map )
    {
        default:
            fprintf(stderr,"** IV: mapping %d not ready\n", sopt->map );
            RETURN(-1);

        case E_SMAP_MODE:
            /* allocate memory in 4 value blocks */
            add_to_float_list(aggr->vlist+vox, value, 4);
            dv[vox] = value;      /* useless, but lets us track via debug */
            break;

        case E_SMAP_NZ_MODE:
            /* return, to avoid cv increment (pretend we were never here) */
            if( !value ) RETURN(0);

            add_to_float_list(aggr->vlist+vox, value, 4);
            dv[vox] = value;      /* useless, but lets us track via debug */
            break;

        case E_SMAP_AVE:
            if ( cv[vox] == 0 )
                dv[vox] = value;
            else
                dv[vox] += value;               /* divide by count later */
            break;

        case E_SMAP_NZ_AVE:
            /* return, to avoid cv increment (pretend we were never here) */
            if( !value ) RETURN(0);

            if ( cv[vox] == 0 )
                dv[vox] = value;
            else
                dv[vox] += value;               /* divide by count later */
            break;

        case E_SMAP_MEDIAN:
            add_to_float_list(aggr->vlist+vox, value, 4);
            dv[vox] = value;      /* useless, but lets us track via debug */
            break;

        case E_SMAP_NZ_MEDIAN:
            /* return, to avoid cv increment (pretend we were never here) */
            if( !value ) RETURN(0);

            add_to_float_list(aggr->vlist+vox, value, 4);
            dv[vox] = value;      /* useless, but lets us track via debug */
            break;

        case E_SMAP_COUNT:
            if ( cv[vox] == 0 )
                dv[vox] = 1.0;
            else
                dv[vox]++;
            break;

        case E_SMAP_MAX:
            if ( cv[vox] == 0 )
                dv[vox] = value;
            else if (value > dv[vox])
                dv[vox] = value;

            break;

        case E_SMAP_MAX_ABS:
            if ( cv[vox] == 0 )
                dv[vox] = value;
            else if (fabs(value) > fabs(dv[vox]))
                dv[vox] = value;

            break;

        case E_SMAP_MIN:
            if ( cv[vox] == 0 )
                dv[vox] = value;
            else if (value < dv[vox])
                dv[vox] = value;

            break;

        case E_SMAP_MASK:
        case E_SMAP_MASK2:
            dv[vox] = 1.0;
            break;
    }

    cv[vox]++;          /* in any case, increment the counter */

    if ( (sopt->debug > 1) && (vox == sopt->dvox) )
        fprintf(stderr,"to (%d, %f)\n",cv[vox],dv[vox]);

    RETURN(0);
}


/*----------------------------------------------------------------------
 * make_point_list  - from endpoints and steps
 *
 * return   0 on success
 *        < 0 on error
 *----------------------------------------------------------------------
*/
int make_point_list( THD_fvec3 * list, THD_fvec3 * p1, THD_fvec3 * pn,
                     s2v_opts_t * sopt )
{
    float rat1, ratn;
    int   step;

ENTRY("make_point_list");

    if ( !list || !p1 || !pn || !sopt )
    {
        fprintf(stderr,"** mpl: bad params (%p,%p,%p,%p)\n", list,p1,pn,sopt);
        RETURN(-1);
    }

    list[0] = *p1;

    for ( step = 1; step < sopt->f_steps; step++ )
    {
        ratn = step / (sopt->f_steps - 1.0);
        rat1 = 1.0 - ratn;

        list[step].xyz[0] = rat1 * p1->xyz[0] + ratn * pn->xyz[0];
        list[step].xyz[1] = rat1 * p1->xyz[1] + ratn * pn->xyz[1];
        list[step].xyz[2] = rat1 * p1->xyz[2] + ratn * pn->xyz[2];
    }

    RETURN(0);
}


/*----------------------------------------------------------------------
 * adjust_endpts                - adjust endpoints for map and options
 *
 * return   0 on success
 *        < 0 on error
 *----------------------------------------------------------------------
*/
int adjust_endpts( s2v_opts_t * sopt, THD_fvec3 * p1, THD_fvec3 * pn )
{
    THD_fvec3 f3_diff;
    float     dist, factor;

ENTRY("adjust_endpts");

    if ( !sopt || !p1 || !pn )
    {
        fprintf(stderr,"** ae: invalid params (%p,%p,%p)\n", sopt, p1, pn);
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

    /* let's be efficient and less AR and not do this here ... */
    #if 0
    switch ( sopt->map )
    {
        default:
            fprintf(stderr,"** ae: mapping %d not ready\n", sopt->map );
            RETURN(-1);

        case E_SMAP_AVE:
        case E_SMAP_NZ_AVE:
        case E_SMAP_COUNT:
        case E_SMAP_MAX_ABS:
        case E_SMAP_MAX:
        case E_SMAP_MIN:
        case E_SMAP_MASK:
        case E_SMAP_MASK2:
        case E_SMAP_MODE:
        case E_SMAP_NZ_MODE:
        case E_SMAP_MEDIAN:
        case E_SMAP_NZ_MEDIAN:
            break;
    }
    #endif

    RETURN(0);
}


/*----------------------------------------------------------------------
 * set_node_list_data   - fill fdata in node list
 *
 * For the given column, fill N->ilist with data from p->sdata_im.
 *----------------------------------------------------------------------
*/
int set_node_list_data ( node_list_t *N, param_t *p, s2v_opts_t *sopt, int col )
{
    float * fp, fval;
    int     c, lposn;

ENTRY("set_node_list_data");

    if ( !N || !p || !sopt || col < 0 )
    {
        fprintf(stderr,"** snld: bad params (%p,%p,%p,%d)\n", N, p, sopt, col);
        RETURN(-1);
    }

    if ( sopt->debug > 1 )
        fprintf(stderr, "-- setting fdata for column %d\n", col);

    /* if neither sdata_im or dset exists, fill as mask and return */
    if ( !p->sdata_im && !p->dset)
    {
        fval = 1.0;     /* init to mask value */

        /* if we have a PARSER_code (presumably without symbols), use it */
        if ( p->parser.pcode )
        {
            for ( c = 0; c < 26; c++ )
                p->parser.atoz[c] = 0.0;
            fval = PARSER_evaluate_one( p->parser.pcode, p->parser.atoz );
        }

        for ( c = 0; c < N->ilen; c++ )
            N->fdata[c] = fval;

        RETURN(0);
    } else if ( p->sdata_im ) {

       /* else sdata exists: check column, and copy data from sdata_im */
       if ( col > (p->sdata_im->nx - 2) && !p->parser.pcode )
       {
           fprintf(stderr,"** snld error: col > nx-2 (%d > %d)\n",
                   col, p->sdata_im->nx-2);
           RETURN(-1);
       }
       else if ( p->sdata_im->ny < N->ilen )
       {
           fprintf(stderr,"** snld error: ny < ilen (%d < %d)\n",
                   p->sdata_im->ny, N->ilen);
           RETURN(-1);
       }
       else if ( !N->fdata )
       {
           fprintf(stderr,"** snld error: missing idata\n");
           RETURN(-1);
       }
       else if ( p->parser.pcode && col != 0 )             /* let's be safe */
       {
           fprintf(stderr,"** snld error: cannot use parser with col = %d\n",
                          col);
           RETURN(-1);
       }

       /* hmmmm, we're still missing something...  oh yes, data! */

       fp = MRI_FLOAT_PTR( p->sdata_im ) + col+1;  /* offset by column number */

       for ( c = 0; c < N->ilen; c++ )
         {
        if ( p->parser.pcode )
        {
            /* fill atoz with surface node data */
            for ( lposn = 0; lposn < p->parser.max_sym; lposn++ )
                p->parser.atoz[lposn] = fp[lposn];

            N->fdata[c] = PARSER_evaluate_one(p->parser.pcode, p->parser.atoz);
        }
        else
            N->fdata[c] = *fp;

        fp += p->sdata_im->nx;
         }
   } else if ( p->dset ){
      if (!(fp = SUMA_DsetCol2Float (p->dset,  col, 1))) {
         fprintf(stderr,"** snld error: Failed to get col %d from dset\n",
                        col);
         RETURN(-1);
      }
       for ( c = 0; c < N->ilen; c++ )
       {
           if ( p->parser.pcode )
           {
               /* fill atoz with surface node data */
               for ( lposn = 0; lposn < p->parser.max_sym; lposn++ )
                   p->parser.atoz[lposn] = (float)
                        SUMA_GetDsetValInCol2(p->dset,
                                              lposn, c);
               N->fdata[c] = PARSER_evaluate_one(p->parser.pcode,
                                 p->parser.atoz);
           }
           else
               N->fdata[c] = fp[c];
       }
       SUMA_free(fp); fp=NULL;
   } else {
      fprintf(stderr,"** snld error: This NULLity should not be.\n");
      RETURN(-1);
   }


    RETURN(0);
}


/*----------------------------------------------------------------------
 * init_node_list       - from surfaces or from sxyz file
 *
 * Fill nodes with coordinate data.
 *----------------------------------------------------------------------
*/
int init_node_list (opts_t *opts, param_t *p, s2v_opts_t *sopt, node_list_t *N)
{
    int nsurf, rv;

ENTRY("init_node_list");

    if ( opts == NULL || p == NULL || sopt == NULL || N == NULL )
    {
        fprintf(stderr, "** inl - bad params (%p,%p,%p,%p)\n",opts,p,sopt,N);
        RETURN(-1);
    }

    memset(N,0,sizeof(*N));     /* first, clear it out - have care w/pointers */
    N->nodes = NULL;  N->fdata = NULL;  N->ilist = NULL;

    nsurf = SUMAg_N_DOv;        /* verify, as long as there is no sxyz_im */
    if ( !p->sxyz_im && ((nsurf < 1) || (nsurf > S2V_MAX_SURFS)) )
    {
        fprintf(stderr,"** inl: SUMAg_N_DOv has invalid value of %d\n", nsurf);
        RETURN(-1);
    }

#if 0           /* remove requrement of 2 surfaces for functions [v3.0] */

    if ( sopt->map == E_SMAP_MASK )
        nsurf = 1;
    else
    {
        /* do a quick check before slow reading action */
        if ( ! p->sxyz_im && (SUMAg_N_DOv < 2) )
        {
            fprintf(stderr,"** function '%s' requires 2 surfaces\n",
                    gs2v_map_names[sopt->map]);
            RETURN(-1);
        }

        nsurf = 2;
    }
#endif

    if ( p->sxyz_im )
        rv = sxyz_1D_to_nlist( opts, sopt, p, N, &nsurf );
    else
        rv = surf_to_node_list( sopt, N, nsurf );

    if ( sopt->debug > 1 && rv == 0 )         /* errors reported separately */
        fprintf(stderr,"++ node list initialized\n");

    RETURN(rv);
}


/*----------------------------------------------------------------------
 * sxyz_1D_to_nlist     - nodes from sxyz 1D to node_list
 *
 *----------------------------------------------------------------------
*/
int sxyz_1D_to_nlist(opts_t * opts, s2v_opts_t * sopt, param_t * p,
                     node_list_t * N, int * nsurf)
{
    THD_fvec3 * fvp;
    float     * fp;
    int         c, sc;

ENTRY("sxyz_1D_to_nlist");

    if ( !sopt || !p || !N )
    {
        fprintf(stderr,"** sxyz2nl: bad params (%p,%p,%p)\n",sopt,p,N);
        RETURN(-1);
    }

    if ( !p->sxyz_im )
    {
        fprintf(stderr,"** missing sxyz_im for sxyz surf '%s'\n",
                opts->surf_xyz_1D_file);
        RETURN(-1);
    }

    *nsurf = p->sxyz_im->nx / 3;

    if ( p->sxyz_im->nx != 3 * *nsurf )
    {
        fprintf(stderr,"** sxyz surf '%s' has %d columns (%d expected)\n",
                opts->surf_xyz_1D_file, p->sxyz_im->nx, 3**nsurf);
        RETURN(-1);
    }
    else if ( p->sxyz_im->ny <= 0 )
    {
        fprintf(stderr,"** sxyz surf '%s': bad sxyz dimensions (%d,%d)\n",
                opts->surf_xyz_1D_file, p->sxyz_im->nx, p->sxyz_im->ny);
        RETURN(-1);
    }

    N->depth = *nsurf;
    N->nnodes = p->sxyz_im->ny;

    N->nodes = (THD_fvec3 *)malloc(N->depth*N->nnodes*sizeof(THD_fvec3));
    if ( N->nodes == NULL )
    {
        fprintf(stderr,"** failed to allocate %dx%d THD_fvec3's for nodes\n",
                N->nnodes, N->depth);
        RETURN(-1);
    }

    fvp = N->nodes;
    for ( sc = 0; sc < *nsurf; sc++ )
    {
        fp = MRI_FLOAT_PTR( p->sxyz_im ) + sc * 3;      /* offset for surf */
        for ( c = 0; c < N->nnodes; c++ )
        {
            fvp->xyz[0] = fp[0];
            fvp->xyz[1] = fp[1];
            fvp->xyz[2] = fp[2];

            fp += p->sxyz_im->nx;
            fvp++;
        }
    }

    if ( sopt->debug > 0 )
    {
        fprintf(stderr, "++ sxyz_1D nodes from '%s': nxyz = %d, nsurf = %d\n",
                opts->surf_xyz_1D_file, N->nnodes, N->depth);
        if ( sopt->dnode >= 0 && sopt->dnode <= p->sxyz_im->ny )
        {
            fprintf(stderr,"   debug node (%d) loc", sopt->dnode);
            for ( sc = 0; sc < *nsurf; sc++ )
                fprintf(stderr," : (%f, %f, %f)",
                    N->nodes[sopt->dnode + sc*N->nnodes].xyz[0],
                    N->nodes[sopt->dnode + sc*N->nnodes].xyz[1],
                    N->nodes[sopt->dnode + sc*N->nnodes].xyz[2]);
            fputc('\n', stderr);
        }
    }

    RETURN(0);
}


/*----------------------------------------------------------------------
 * surf_to_node_list - create node list for mask mapping
 *
 * Allocate space for node list(s), and fill with xyz coordinates.
 *
 * return -1 : on error
 *         0 : on success
 *----------------------------------------------------------------------
*/
int surf_to_node_list ( s2v_opts_t * sopt, node_list_t * N, int nsurf )
{
    SUMA_SurfaceObject ** so;
    THD_fvec3          *  fvp;
    float              *  fp;
    int                   rv, nindex, sindex;

ENTRY("surf_to_node_list");

    if ( sopt == NULL || N == NULL || nsurf < 0 || nsurf > 2 )
    {
        fprintf( stderr, "** anl: bad params (%p,%p,%d)\n",
                 sopt, N, nsurf );
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
        fprintf( stderr, "** error: found %d (of %d) mappable surfaces\n",
                rv, nsurf );
        RETURN(-1);
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
        RETURN(-1);
    }

    /* copy the xyz coordinates for each node */

    fvp = N->nodes;     /* linear coverage of all nodes */
    for ( sindex = 0; sindex < N->depth; sindex++ )
    {
        if ( so[sindex]->N_Node != N->nnodes )
        {
            fprintf(stderr, "** surf #%d (%s) has %d nodes (but expected %d)\n",
                    sindex,
                    so[sindex]->Label ? so[sindex]->Label : "<unnamed>",
                    so[sindex]->N_Node, N->nnodes );
            free( N->nodes );  N->nodes = NULL;
            free(so);
            RETURN(-1);
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
                 N->depth, N->nnodes, (int)sizeof(THD_fvec3) );

    free(so);
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

ENTRY("get_mappable_surfts");

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
        {
            fprintf( stderr, "\n---------- surface #%d '%s' -----------\n",
                     socount, CHECK_NULL_STR(so->Label) );
            SUMA_Print_Surface_Object( so, stderr );
        }

        if ( socount < how_many )       /* store a good surface */
            slist[socount] = so;

        socount++;
    }

    if ( debug > 1 )
        fprintf( stderr, "++ found %d mappable surfaces\n", socount );

    RETURN(socount);
}


/*----------------------------------------------------------------------
 * set_smap_opts  - given options and mapping function, fill struct
 *
 * return  0 : success
 *        -1 : error condition
 *----------------------------------------------------------------------
*/
int set_smap_opts( opts_t * opts, param_t * p, s2v_opts_t * sopt )
{
    int nsurf;

ENTRY("set_smap_opts");

    memset( sopt, 0, sizeof(*sopt) );   /* clear the sopt struct */
    sopt->cmask = NULL;

    if ( (sopt->map = check_map_func( opts->map_str )) == E_SMAP_INVALID )
        RETURN(-1);

    sopt->datum = check_datum_type(opts->datum_str, DSET_BRICK_TYPE(p->gpar,0));
    if (sopt->datum < 0)
        RETURN(-1);

    if ( opts->noscale == 1 )
        sopt->noscale = 1;

    sopt->debug         = opts->debug;  /* for output in library functions */
    sopt->dnode         = opts->dnode;
    sopt->dvox          = opts->dvox;
    sopt->stop_gap      = opts->stop_gap;
    sopt->sxyz_ori_gpar = opts->sxyz_ori_gpar;
    sopt->cmask         = p->cmask;

    if ( opts->f_steps < S2V_F_STEPS_MIN )             /* minimum is 1    */
    {
        /* if f_steps was not given, init to the number of surfaces  - v3.6 */
        for ( nsurf = 0; nsurf < S2V_MAX_SURFS && opts->snames[nsurf]; nsurf++ )
            ;
        if ( nsurf <= 0 )
        {
            fprintf(stderr,"** error: sso: no input surfaces\n");
            RETURN(-1);
        }
        sopt->f_steps = nsurf;
    }
    else
        sopt->f_steps = opts->f_steps;

    sopt->f_index = S2V_F_INDEX_VOXEL;
    if ( (opts->f_index_str != NULL) &&
         ( !strncmp(opts->f_index_str, "point", 5) ||
           !strncmp(opts->f_index_str, "node",  4) ) ) /* preserve old use */
        sopt->f_index = S2V_F_INDEX_POINT;

    sopt->f_p1_fr = opts->f_p1_fr;         /* copy fractions & distances */
    sopt->f_pn_fr = opts->f_pn_fr;
    sopt->f_p1_mm = opts->f_p1_mm;
    sopt->f_pn_mm = opts->f_pn_mm;


    switch (sopt->map)
    {
        default:

        case E_SMAP_AVE:
        case E_SMAP_MAX:
        case E_SMAP_MIN:
        case E_SMAP_MAX_ABS:
            break;

        case E_SMAP_COUNT:
        case E_SMAP_MASK:
        case E_SMAP_MASK2:
            sopt->noscale = 1;
            break;
    }

    if ( opts->debug > 1 )
        disp_s2v_opts_t( "++ s2v_opts_set :", sopt );

    RETURN(0);
}


/*----------------------------------------------------------------------
 * free memory, close output file
 *----------------------------------------------------------------------
*/
int final_clean_up ( node_list_t * N )
{
ENTRY("final_clean_up");

    if ( ( SUMAg_DOv != NULL ) &&
         ( SUMA_Free_Displayable_Object_Vect(SUMAg_DOv, SUMAg_N_DOv) == 0 ) )
        fprintf(stderr, "** failed SUMA_Free_Displayable_Object_Vect()\n" );

    if ( ( SUMAg_SVv != NULL ) &&
         ( SUMA_Free_SurfaceViewer_Struct_Vect(SUMAg_SVv, SUMAg_N_SVv) == 0 ) )
        fprintf( stderr, "** failed SUMA_Free_SurfaceViewer_Struct_Vect()\n" );

    if ( ( SUMAg_CF != NULL ) && ( SUMA_Free_CommonFields(SUMAg_CF) == 0 ) )
        fprintf( stderr, "** failed SUMA_Free_CommonFields()\n" );

    if ( N )
    {
        if ( N->nodes )  free( N->nodes );
        if ( N->fdata )  free( N->fdata );
        if ( N->ilist )  free( N->ilist );
    }

    RETURN(0);
}


/*----------------------------------------------------------------------
 *
 *----------------------------------------------------------------------
*/
int read_surf_files ( opts_t * opts, param_t * p, SUMA_SurfSpecFile * spec,
                      s2v_opts_t * sopt, node_list_t * N )
{
    int rv;

ENTRY("read_surf_files");

    /* get surface coordinates */
    if ( opts->spec_file )
    {
        if ( (rv = fill_SUMA_structs(opts, spec)) != 0 )
            RETURN(rv);
    }
    else if ( opts->surf_xyz_1D_file )
    {
        if ( (rv = read_sxyz_1D( opts, p )) != 0 )
            RETURN(rv);
    }
    else
    {
        fprintf(stderr,"** missing spec or sdata file, exiting...\n");
        RETURN(-1);
    }

    /* assign node list nodes (xyz coords) */
    if ( (rv = init_node_list(opts, p, sopt, N)) != 0 )
        RETURN(rv);

    /* check status and allocate memory in node list */
    if ( (rv = fill_node_list(opts, p, N)) != 0 )
        RETURN(rv);

    if ( (rv = verify_parser_expr(opts, p)) != 0 )
        RETURN(rv);

    RETURN(0);
}


/*----------------------------------------------------------------------
 * read_sxyz_file       - read surf_xyz_1D_file
 *----------------------------------------------------------------------
*/
int read_sxyz_1D ( opts_t * opts, param_t * p )
{
    MRI_IMAGE * im;

ENTRY("read_sxyz_1D");

    if ( !opts || !p )
    {
        fprintf(stderr,"** rsxyz1D: bad params (%p,%p)\n", opts, p);
        RETURN(-1);
    }

    if ( (im = mri_read_1D(opts->surf_xyz_1D_file)) == NULL )
    {
        fprintf(stderr,"** failed to read sxyz 1D file '%s'\n",
                opts->surf_xyz_1D_file);
        RETURN(-1);
    }
    if ( im->nx < 1 || im->ny < 3 )     /* must have xyz coords, at least */
    {
        fprintf(stderr,"** bad sxyz file '%s'?\n", opts->surf_xyz_1D_file);
        RETURN(-1);
    }

    /* transpose list to node-major order, and lose temp image */
    p->sxyz_im = mri_transpose(im);
    mri_free(im);

    if ( opts->debug > 0 )
        fprintf(stderr,"++ read 1D xyz surface file '%s' (nx = %d, ny = %d)\n",
                opts->surf_xyz_1D_file, p->sxyz_im->nx, p->sxyz_im->ny );

    RETURN(0);
}


/*----------------------------------------------------------------------
 * fill the node_list data fields
 *
 *     ilist gets index list, if any
 *     fdata gets allocation for data
 *     ilen is the length of the two lists
 *----------------------------------------------------------------------
*/
int fill_node_list ( opts_t * opts, param_t * p, node_list_t * N )
{
    int rv;

ENTRY("fill_node_list");

    if ( !opts || !p || !N )
    {
        fprintf(stderr,"** fill_node_list: bad params (%p,%p,%p)\n",opts,p,N);
        RETURN(-1);
    }

    p->nsubs = 1;               /* unless we get additional surface data */

    if ( opts->sdata_file_1D )
    {
        if ( (rv = sdata_from_1D( opts, p, N )) != 0 )
            RETURN(rv);
        if ( ! p->parser.pcode )
            p->nsubs = p->sdata_im->nx - 1;
    }
    else if ( opts->sdata_file_niml )
    {
        if ( (rv = sdata_from_niml( opts, p, N )) != 0 )
            RETURN(rv);
        if ( ! p->parser.pcode )
            p->nsubs = SDSET_VECNUM(p->dset);
    }
    else
    {
        if ( (rv = sdata_from_default( N )) != 0 )
            RETURN(rv);
    }

    if ( (rv = verify_node_list( N )) != 0 )
        RETURN(rv);

    if ( opts->debug > 1 )
        disp_node_list_t( "++ node list filled: ", N );

    RETURN(0);
}


/*----------------------------------------------------------------------
 * verify that the parser expression has sufficient surface data values
 *----------------------------------------------------------------------
*/
int verify_parser_expr ( opts_t * opts, param_t * p )
{
    int max_used;

ENTRY("verify_parser_expr");

    if ( !opts || !p )
    {
        fprintf(stderr,"** vpe: invalid params (%p,%p)\n", opts, p);
        RETURN(-1);
    }

    /* if no parser code, there is nothing to do */
    if ( ! p->parser.pcode )
        RETURN(0);


    for ( max_used = 25; max_used >= 0; max_used-- )
        if ( p->parser.has_sym[max_used] )
            break;
    max_used++;         /* this is the number of surface values needed */
    p->parser.max_sym = max_used;

    /* if the expression is not constant, we need some data */
    if ( !p->sdata_im && !p->dset)
    {
         fprintf(stderr, "** parser expression requires surface data\n"
                         "   (see '-sdata_1D' or '-sdata')\n");
         RETURN(-1);
    }
    if (p->sdata_im)
    {
       if ( max_used > 0 )
       {
           if ( max_used > p->sdata_im->nx - 1 )
           {
               fprintf(stderr,
                       "** error: not enough surface values for expression\n"
                       "          svals = %d, exp_vals = %d, expr = '%s'\n",
                       p->sdata_im->nx - 1, max_used, opts->data_expr);
               RETURN(-1);
           }
       }

       if ( opts->debug > 1 )
           fprintf(stderr,"-- surf_vals = %d, expr_vals = %d\n",
                   p->sdata_im ? (p->sdata_im->nx - 1) : 0, max_used);
   } else if (p->dset)
   {
      if ( max_used > 0 )
       {
           if ( max_used > SDSET_VECNUM(p->dset) )
           {
               fprintf(stderr,
                       "** error: not enough surface values for expression\n"
                       "          svals = %d, exp_vals = %d, expr = '%s'\n",
                       SDSET_VECNUM(p->dset), max_used, opts->data_expr);
               RETURN(-1);
           }
       }

       if ( opts->debug > 1 )
           fprintf(stderr,"-- surf_vals = %d, expr_vals = %d\n",
                   p->dset ? SDSET_VECNUM(p->dset) : 0, max_used);
   }

   RETURN(0);
}


/*----------------------------------------------------------------------
 * verify that the node list makes sense
 *----------------------------------------------------------------------
*/
int verify_node_list ( node_list_t * N )
{
    int icount, errs = 0;

ENTRY("verify_node_list");

    if ( !N )
    {
        fprintf(stderr, "** vnl - no node list\n" );
        RETURN(-1);
    }

    if ( !N->nodes || !N->ilist )
    {
        fprintf(stderr,"** missing nodes or ilist\n" );
        errs++;
    }

    if ( N->depth < 1 || N->nnodes < 1 || N->ilen < 1 )
    {
        fprintf(stderr,"** invalid depth, nnodes or ilen" );
        errs++;
    }

    if ( errs )
    {
        disp_node_list_t("** invalid data : ", N );
        RETURN(-errs);
    }

    /* now check that the indices are within nnodes range */
    for ( icount = 0; icount < N->ilen; icount++ )
        if ( N->ilist[icount] < 0 || N->ilist[icount] >= N->nnodes )
        {
            fprintf(stderr,"** surf data index number %d is out of range:\n"
                           "   index = %d, range is [%d,%d]\n",
                           icount, N->ilist[icount], 0, N->nnodes-1);
            RETURN(-10);
        }

    /* node_list is okay, so make space for the actual data */
    if ( (N->fdata = (float *)malloc(N->ilen*sizeof(float))) == NULL )
    {
        fprintf(stderr,"** vnl: failed to allocate %d floats\n",N->ilen);
        free(N->ilist);
        RETURN(-20);
    }

    RETURN(0);
}


/*----------------------------------------------------------------------
 * fill ilist with default indices (i[k] = k)
 *----------------------------------------------------------------------
*/
int sdata_from_default ( node_list_t * N )
{
    int c;

ENTRY("sdata_from_default");

    if ( !N )
        RETURN(-1);

    if ( (N->ilist = (int *)malloc(N->nnodes * sizeof(int))) == NULL )
    {
        fprintf(stderr,"** failed to allocate %d ints for ilist\n",N->nnodes);
        RETURN(-1);
    }

    N->ilen   = N->nnodes;

    /* now fill ilist with default node indices */
    for ( c = 0; c < N->ilen; c++ )
        N->ilist[c] = c;

    RETURN(0);
}


/*----------------------------------------------------------------------
 * fill node_list_t struct from 1D surface file
 *----------------------------------------------------------------------
*/
int sdata_from_1D ( opts_t * opts, param_t * p, node_list_t * N )
{
    MRI_IMAGE * im;
    float     * fim;
    int         c;

ENTRY("sdata_from_1D");

    if ( !opts || !N || !p )
        RETURN(-1);

    if ( (im = mri_read_1D(opts->sdata_file_1D)) == NULL )
    {
        fprintf(stderr,"** failed to read 1D file '%s'\n", opts->sdata_file_1D);
        RETURN(-2);
    }
    /* transpose list to node-major order, and lose temp image */
    p->sdata_im = mri_transpose(im);
    mri_free(im);

    if ( p->sdata_im->nx < 2 || p->sdata_im->ny < 1 )
    {
        fprintf(stderr,"** bad (%d x %d) surf data 1D file '%s'\n",
                p->sdata_im->ny, p->sdata_im->nx, opts->sdata_file_1D);
        RETURN(-3);
    }

    N->ilen = p->sdata_im->ny;

    if ( opts->debug > 0 )
        fprintf(stderr,"++ read 1D surface file '%s' (nx = %d, ny = %d)\n",
                opts->sdata_file_1D, p->sdata_im->nx, p->sdata_im->ny );

    /* only allocate space ilist */
    if ( (N->ilist = (int *)malloc(N->ilen*sizeof(int))) == NULL )
    {
        fprintf(stderr,"** sf1D a: failed to allocate %d ints\n", N->ilen);
        RETURN(-1);
    }

    /* first set the node index values */
    fim = MRI_FLOAT_PTR( p->sdata_im );
    for ( c = 0; c < N->ilen; c++, fim += p->sdata_im->nx )
        N->ilist[c] = (int)*fim;                          /* set node index */

    RETURN(0);
}

/*----------------------------------------------------------------------
 * fill node_list_t struct from niml surface file
 *----------------------------------------------------------------------
*/
int sdata_from_niml ( opts_t * opts, param_t * p, node_list_t * N )
{
    int         c, *nind = NULL;
    SUMA_DSET_FORMAT form = SUMA_NO_DSET_FORMAT;
ENTRY("sdata_from_niml");

    if ( !opts || !N || !p )
        RETURN(-1);

    if ( !(p->dset = SUMA_LoadDset_s(opts->sdata_file_niml,
                                     &form, 0)) )     {
        fprintf(stderr,"** failed to read file '%s'\n", opts->sdata_file_niml);
        RETURN(-2);
    }
    if (!(SUMA_PopulateDsetNodeIndexNel(p->dset,1))) {
        fprintf(stderr,"** failed to populate node indices in '%s'\n",
                opts->sdata_file_niml);
        RETURN(-2);
    }
    if (!(nind = SDSET_NODE_INDEX_COL(p->dset))) {
         fprintf(stderr,"** No node index column in '%s'\n",
                  opts->sdata_file_niml);
        RETURN(-2);
    }
    N->ilen = SDSET_VECLEN(p->dset);

    if ( opts->debug > 0 )
        fprintf(stderr,"++ read surface dset file '%s' (nx = %d, ny = %d)\n",
                opts->sdata_file_niml, SDSET_VECNUM(p->dset), N->ilen );

    /* only allocate space ilist */
    if ( (N->ilist = (int *)malloc(N->ilen*sizeof(int))) == NULL )
    {
        fprintf(stderr,"** sf a: failed to allocate %d ints\n", N->ilen);
        RETURN(-1);
    }

    /* first set the node index values */
    for ( c = 0; c < N->ilen; c++ )
        N->ilist[c] = nind[c];                          /* set node index */

    RETURN(0);
}


/*----------------------------------------------------------------------
 * read surfaces (much stolen from SUMA_suma.c - thanks Ziad!)
 *
 * return 0 on success
 *----------------------------------------------------------------------
*/
int fill_SUMA_structs ( opts_t * opts, SUMA_SurfSpecFile * spec )
{
    int debug = 0, rv;
ENTRY("fill_SUMA_structs");

    if ( opts->debug > 2 )
        debug = 1;

    if ( debug )
        fputs( "-- SUMA_Create_CommonFields()...\n", stderr );

    if ( opts->spec_file == NULL )
        RETURN(-1);

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

    if ( debug )
        fputs( "-- SUMA_Read_SpecFile()...\n", stderr );

    if ( SUMA_Read_SpecFile( opts->spec_file, spec) == 0 )
    {
        fprintf( stderr, "** failed SUMA_Read_SpecFile(), exiting...\n" );
        RETURN(-1);
    }

    if ( debug )
        SUMA_ShowSpecStruct(spec, stderr, 1);

    rv = SUMA_spec_select_surfs(spec, opts->snames, S2V_MAX_SURFS, opts->debug);
    if ( rv < 1 )
    {
        if ( rv == 0 )
            fprintf(stderr,"** no named surfaces found in spec file\n");
        RETURN(-1);
    }

    if ( opts->debug > 0 )
        SUMA_ShowSpecStruct(spec, stderr, 1);

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
             SUMAg_CF->DsetList) == 0)     /* DsetList - 26 Mar 2004 [ziad] */
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
        usage( PROG_NAME, S2V_USE_LONG );
        RETURN(1);
    }

    /* clear out the options structure, pointers get explicit NULL */
    memset( opts, 0, sizeof( opts_t) );
    opts->gpar_file        = NULL;      opts->oset_file     = NULL;
    opts->spec_file        = NULL;      opts->sv_file       = NULL;
    opts->surf_xyz_1D_file = NULL;      opts->sdata_file_1D = NULL;
    opts->sdata_file_niml  = NULL;      opts->cmask_cmd     = NULL;
    opts->data_expr        = NULL;      opts->map_str       = NULL;
    opts->datum_str        = NULL;      opts->f_index_str   = NULL;
    opts->snames[0]        = NULL;      opts->snames[1]     = NULL;


    opts->dnode = -1;                   /* init to something invalid */
    opts->dvox  = -1;                   /* init to something invalid */

    for ( ac = 1; ac < argc; ac++ )
    {
        /* alphabetical... */
        if ( ! strncmp(argv[ac], "-cmask", 6) )
        {
            if ( (ac+1) >= argc )
            {
                fputs( "option usage: -cmask COMMAND\n\n", stderr );
                usage( PROG_NAME, S2V_USE_SHORT );
                RETURN(-1);
            }

            opts->cmask_cmd = argv[++ac];
        }
        else if ( ! strncmp(argv[ac], "-data_expr", 8) )
        {
            if ( (ac+1) >= argc )
            {
                fputs( "option usage: -data_expr EXPR\n\n", stderr );
                usage( PROG_NAME, S2V_USE_SHORT );
                RETURN(-1);
            }

            opts->data_expr = argv[++ac];
        }
        else if ( ! strncmp(argv[ac], "-datum", 6) )
        {
            if ( (ac+1) >= argc )
            {
                fputs( "option usage: -datum DTYPE\n\n", stderr );
                usage( PROG_NAME, S2V_USE_SHORT );
                RETURN(-1);
            }

            opts->datum_str = argv[++ac];
        }
        else if ( ! strncmp(argv[ac], "-debug", 6) )
        {
            if ( (ac+1) >= argc )
            {
                fputs( "option usage: -debug LEVEL\n\n", stderr );
                usage( PROG_NAME, S2V_USE_SHORT );
                RETURN(-1);
            }

            opts->debug = atoi(argv[++ac]);
            if ( opts->debug < 0 || opts->debug > S2V_DEBUG_MAX_LEV )
            {
                fprintf( stderr, "bad debug level <%d>, should be in [0,%d]\n",
                        opts->debug, S2V_DEBUG_MAX_LEV );
                usage( PROG_NAME, S2V_USE_SHORT );
                RETURN(-1);
            }
        }
        else if ( ! strncmp(argv[ac], "-dnode", 6) )
        {
            if ( (ac+1) >= argc )
            {
                fputs( "option usage: -dnode DEBUG_NODE\n\n", stderr );
                usage( PROG_NAME, S2V_USE_SHORT );
                RETURN(-1);
            }

            opts->dnode = atoi(argv[++ac]);
        }
        else if ( ! strncmp(argv[ac], "-dvoxel", 5) )
        {
            if ( (ac+1) >= argc )
            {
                fputs( "option usage: -dvoxel DEBUG_VOXEL\n\n", stderr );
                usage( PROG_NAME, S2V_USE_SHORT );
                RETURN(-1);
            }

            opts->dvox = atoi(argv[++ac]);
        }
        else if ( ! strncmp(argv[ac], "-f_index", 7) )
        {
            if ( (ac+1) >= argc )
            {
                fputs( "option usage: -f_index INDEX_TYPE\n\n", stderr );
                usage( PROG_NAME, S2V_USE_SHORT );
                RETURN(-1);
            }
            opts->f_index_str = argv[++ac];
        }
        else if ( ! strncmp(argv[ac], "-f_p1_fr", 9) )
        {
            if ( (ac+1) >= argc )
            {
                fputs( "option usage: -f_p1_fr FRACTION\n\n", stderr );
                usage( PROG_NAME, S2V_USE_SHORT );
                RETURN(-1);
            }

            opts->f_p1_fr = atof(argv[++ac]);
        }
        else if ( ! strncmp(argv[ac], "-f_pn_fr", 9) )
        {
            if ( (ac+1) >= argc )
            {
                fputs( "option usage: -f_pn_fr FRACTION\n\n", stderr );
                usage( PROG_NAME, S2V_USE_SHORT );
                RETURN(-1);
            }

            opts->f_pn_fr = atof(argv[++ac]);
        }
        else if ( ! strncmp(argv[ac], "-f_p1_mm", 9) )
        {
            if ( (ac+1) >= argc )
            {
                fputs( "option usage: -f_p1_mm DISTANCE\n\n", stderr );
                usage( PROG_NAME, S2V_USE_SHORT );
                RETURN(-1);
            }

            opts->f_p1_mm = atof(argv[++ac]);
        }
        else if ( ! strncmp(argv[ac], "-f_pn_mm", 9) )
        {
            if ( (ac+1) >= argc )
            {
                fputs( "option usage: -f_pn_mm DISTANCE\n\n", stderr );
                usage( PROG_NAME, S2V_USE_SHORT );
                RETURN(-1);
            }

            opts->f_pn_mm = atof(argv[++ac]);
        }
        else if ( ! strncmp(argv[ac], "-f_steps", 6) )
        {
            if ( (ac+1) >= argc )
            {
                fputs( "option usage: -f_steps NUM_STEPS\n\n", stderr );
                usage( PROG_NAME, S2V_USE_SHORT );
                RETURN(-1);
            }

            opts->f_steps = atoi(argv[++ac]);
        }
        else if ( ! strncmp(argv[ac], "-grid_parent", 5)||
                  ! strncmp(argv[ac], "-inset", 6)       ||
                  ! strncmp(argv[ac], "-input", 6) )

        {
            if ( (ac+1) >= argc )
            {
                fputs( "option usage: -grid_parent INPUT_DSET\n\n", stderr );
                usage( PROG_NAME, S2V_USE_SHORT );
                RETURN(-1);
            }

            opts->gpar_file = argv[++ac];
        }
        else if ( ! strncmp(argv[ac], "-help", 5) )
        {
            usage( PROG_NAME, S2V_USE_LONG );
            RETURN(1);
        }
        else if ( ! strncmp(argv[ac], "-hist", 5) )
        {
            usage( PROG_NAME, S2V_USE_HIST );
            RETURN(1);
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
                RETURN(-1);
            }

            opts->oset_file = argv[++ac];
        }
        else if ( ! strncmp(argv[ac], "-sdata_1D", 9) )
        {
            if ( (ac+1) >= argc )
            {
                fputs( "option usage: -sdata_1D SURF_DATA.1D\n\n", stderr );
                usage( PROG_NAME, S2V_USE_SHORT );
                RETURN(-1);
            }

            opts->sdata_file_1D = argv[++ac];
        }
        else if ( ! strcmp(argv[ac], "-sdata") )
        {
            if ( (ac+1) >= argc )
            {
                fputs( "option usage: -sdata SURF_DATA.niml\n\n", stderr );
                usage( PROG_NAME, S2V_USE_SHORT );
                RETURN(-1);
            }

            opts->sdata_file_niml = argv[++ac];
        }
        else if ( ! strncmp(argv[ac], "-spec", 5) )
        {
            if ( (ac+1) >= argc )
            {
                fputs( "option usage: -spec SPEC_FILE\n\n", stderr );
                usage( PROG_NAME, S2V_USE_SHORT );
                RETURN(-1);
            }

            opts->spec_file = argv[++ac];
        }
        else if ( ! strncmp(argv[ac], "-surf_xyz_1D", 9) )
        {
            if ( (ac+1) >= argc )
            {
                fputs( "option usage: -surf_xyz_1D NODE_FILE\n\n", stderr );
                usage( PROG_NAME, S2V_USE_SHORT );
                RETURN(-1);
            }

            opts->surf_xyz_1D_file = argv[++ac];
        }
        else if ( ! strncmp(argv[ac], "-surf_", 6) )
        {
            if ( (ac+1) >= argc )
            {
                fputs( "option usage: -surf_X SURF_NAME\n\n", stderr );
                usage( PROG_NAME, S2V_USE_SHORT );
                RETURN(-1);
            }
            ind = argv[ac][6] - 'A';
            if ( (ind < 0) || (ind >= S2V_MAX_SURFS) )
            {
                fprintf(stderr,"** -surf_X option: '%s' out of range,\n"
                        "   use one of '-surf_A' through '-surf_%c'\n",
                        argv[ac], 'A'+S2V_MAX_SURFS-1);
                RETURN(-1);
            }

            opts->snames[ind] = argv[++ac];
        }
        else if ( ! strncmp(argv[ac], "-sv", 3) )
        {
            if ( (ac+1) >= argc )
            {
                fputs( "option usage: -sv SURFACE_VOLUME\n\n", stderr );
                usage( PROG_NAME, S2V_USE_SHORT );
                RETURN(-1);
            }

            opts->sv_file = argv[++ac];
        }
        else if ( ! strcmp(argv[ac], "-stop_gap") )
        {
            opts->stop_gap = 1;
        }
        else if ( ! strncmp(argv[ac], "-sxyz_orient_as_gpar", 20) )
        {
            opts->sxyz_ori_gpar = 1;
        }
        else if ( ! strncmp(argv[ac], "-version", 2) )
        {
            usage( PROG_NAME, S2V_USE_VERSION );
            RETURN(1);
        }
        else     /* invalid option */
        {
            fprintf( stderr, "invalid option <%s>\n", argv[ac] );
            usage( PROG_NAME, S2V_USE_SHORT );
            suggest_best_prog_option(argv[0], argv[ac]);
            RETURN(-1);
        }
    }

    if ( opts->debug > 1 )
        disp_opts_t ( "++ opts read: ", opts );

    RETURN(0);
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
ENTRY("validate_options");

    /* clear param struct - pointer get explicit NULL */
    memset( p, 0, sizeof(*p) );
    p->gpar         = NULL;    p->oset     = NULL;
    p->sxyz_im      = NULL;    p->sdata_im = NULL;
    p->parser.pcode = NULL;    p->cmask    = NULL;
    p->dset         = NULL;

    if ( check_map_func( opts->map_str ) == E_SMAP_INVALID )
        RETURN(-1);

    if ( validate_datasets( opts, p ) != 0 )
        RETURN(-1);

    if ( validate_surface( opts, p ) != 0 )
        RETURN(-1);

    if ( opts->data_expr )
    {
        PARSER_set_printout(1);
        p->parser.pcode = PARSER_generate_code( opts->data_expr );
        if ( p->parser.pcode == NULL )
        {
            fprintf(stderr,"** failed to generate code from expression '%s'\n",
                    opts->data_expr);
            RETURN(-1);
        }
        PARSER_mark_symbols( p->parser.pcode, p->parser.has_sym );

        if ( opts->debug > 1 )
            disp_parser_t( "-- PARSER expr okay: ", &p->parser );
    }

    if ( opts->debug > 1 )
        disp_param_t( "++ params set: ", p );

    RETURN(0);
}


/*----------------------------------------------------------------------
 * check_datum_type             - determine type for output dataset
 *
 * currently allowable types are MRI_byte, MRI_short, MRI_float, MRI_double
 *
 * return  datum type : on success
 *         -1         : on failure (no valid type can be determined)
 *----------------------------------------------------------------------
*/
int check_datum_type ( char * datum_str, int default_type )
{
    int c, dt = -1;

ENTRY("check_datum_type");

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
            RETURN(-1);
        }
    }
    else
    {
        dt = default_type;
    }

    if ( ( dt != MRI_byte   ) &&
         ( dt != MRI_short  ) &&
         ( dt != MRI_float  ) &&
         ( dt != MRI_double ) )
    {
        fprintf( stderr, "** data type '%s' is not supported\n",
                 MRI_TYPE_name[dt] );
        RETURN(-1);
    }

    RETURN(dt);
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

    map = s2v_map_type( map_str );

    if ( map == E_SMAP_INVALID )
    {
        fprintf( stderr, "** invalid map string '%s'\n", map_str );
        RETURN(-1);
    }

    RETURN(map);
}


/*----------------------------------------------------------------------
 * s2v_map_type - return an E_SMAP_XXX code
 *
 * on failure, return -1 (E_SMAP_INVALID)
 * else        return >0 (a valid map code)
 *----------------------------------------------------------------------
*/
int s2v_map_type ( char * map_str )
{
    s2v_map_num map;

ENTRY("s2v_map_type");

    if ( map_str == NULL )
    {
        fprintf( stderr, "** s2v_map_type: missing map_str parameter\n" );
        RETURN((int)E_SMAP_INVALID);
    }

    if ( sizeof(gs2v_map_names) / sizeof(char *) != (int)E_SMAP_FINAL )
    {
        fprintf( stderr, "** error:  gs2v_map_names/s2v_map_num mismatch\n");
        RETURN((int)E_SMAP_INVALID);
    }

    for ( map = E_SMAP_MASK; map < E_SMAP_FINAL; map++ )
        if ( !strcmp( map_str, gs2v_map_names[map] ) )
            RETURN((int)map);

    RETURN((int)E_SMAP_INVALID);
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

ENTRY("validate_surface");

    if ( ! opts->surf_xyz_1D_file )  /* then check the surface input */
    {
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

        if ( opts->snames[0] == NULL )
        {
            fprintf( stderr, "** missing '-surf_A SURF_NAME' option\n" );
            errs++;
        }
    }
    else if ( opts->spec_file != NULL )
    {
        fprintf(stderr,
                "** cannot use both spec and xyz surface files, %s and %s\n",
                opts->spec_file, opts->surf_xyz_1D_file);
        errs++;
    }

    if ( opts->sdata_file_1D && opts->sdata_file_niml )
    {
        fprintf(stderr,"** cannot use both NIML and 1D surface files\n");
        errs++;
    }

    if ( errs > 0 )
        RETURN(-1);

    RETURN(0);
}

/*----------------------------------------------------------------------
 * validate_datasets
 *
 * Note that we do not validate the SURFACE_VOLUME AFNI dataset here.
 * That is done in SUMA_LoadSpec_eng().
 *
 * Verify the AFNI dataset used for value output.
 * Check for a cmask dataset and command.
 * Verify that AFNI dataset and the mask have the same size.
 *----------------------------------------------------------------------
*/
int validate_datasets( opts_t * opts, param_t * p )
{
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

    p->nvox = DSET_NVOX( p->gpar );
    set_3dmm_bounds( p->gpar, &p->f3mm_min, &p->f3mm_max );

    if ( ! THD_filename_ok( opts->oset_file ) )
    {
        fprintf( stderr, "** illegal output prefix: '%s'\n",
                 opts->oset_file ? opts->oset_file : "<none>" );
        RETURN(-1);
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

        p->cmask = EDT_calcmask( cmd, &p->ncmask, 0 );

        free( cmd );                       /* free EDT_calcmask() string */

        if ( p->cmask == NULL )
        {
            fprintf( stderr, "** failure: cannot compute mask from option:\n"
                     "   -cmask '%s'\n", opts->cmask_cmd );
            RETURN(-1);
        }
        if ( p->ncmask != p->nvox )
        {
            fprintf( stderr, "** error: input and cmask datasets do not have "
                     "the same dimensions\n" );
            RETURN(-1);
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

    RETURN(0);
}

/*----------------------------------------------------------------------
 * usage  -  output usage information
 *
 * S2V_USE_SHORT        - display brief output
 * S2V_USE_LONG         - display long output
 * S2V_USE_VERSION      - show the VERSION of the program
 *----------------------------------------------------------------------
*/
int usage ( char * prog, int level )
{
ENTRY("usage");

    if ( level == S2V_USE_SHORT )
        fprintf(stderr,
                "  usage: %s [options] -spec SPEC_FILE -surf_A SURF_NAME \\\n"
                "             -grid_parent AFNI_DSET -sv SURF_VOL \\\n"
                "             -map_func MAP_FUNC -prefix OUTPUT_DSET\n"
                "usage: %s -help\n",
                 prog, prog );
    else if ( level == S2V_USE_LONG )
    {
        printf(
            "\n"
            "%s - map data from a surface domain to an AFNI volume domain\n"
            "\n"
            "  usage: %s [options] -spec SPEC_FILE -surf_A SURF_NAME \\\n"
            "             -grid_parent AFNI_DSET -sv SURF_VOL \\\n"
            "             -map_func MAP_FUNC -prefix OUTPUT_DSET\n"
            "\n"
            "    This program is meant to take as input a pair of surfaces,\n"
            "    optionally including surface data, and an AFNI grid parent\n"
            "    dataset, and to output a new AFNI dataset consisting of the\n"
            "    surface data mapped to the dataset grid space.  The mapping\n"
            "    function determines how to map the surface values from many\n"
            "    nodes to a single voxel.\n"
            "\n"
            "    Surfaces (from the spec file) are specified using '-surf_A'\n"
            "    (and '-surf_B', if a second surface is input).  If two\n"
            "    surfaces are input, then the computed segments over node\n"
            "    pairs will be in the direction from surface A to surface B.\n"
            "\n"
            "    The basic form of the algorithm is:\n"
            "\n"
            "       o for each node pair (or single node)\n"
            "           o form a segment based on the xyz node coordinates,\n"
            "             adjusted by any '-f_pX_XX' options\n"
            "           o divide the segment up into N steps, according to \n"
            "             the '-f_steps' option\n"
            "           o for each segment point\n"
            "               o if the point is outside the space of the output\n"
            "                 dataset, skip it\n"
            "               o locate the voxel in the output dataset which\n"
            "                 corresponds to this segment point\n"
            "               o if the '-cmask' option was given, and the voxel\n"
            "                 is outside the implied mask, skip it\n"
            "               o if the '-f_index' option is by voxel, and this\n"
            "                 voxel has already been considered, skip it\n"
            "               o insert the surface node value, according to the\n"
            "                 user-specified '-map_func' option\n"
            "\n"
            "  Surface Coordinates:\n"
            "\n"
            "      Surface coordinates are assumed to be in the Dicom\n"
            "      orientation.  This information may come from the option\n"
            "      pair of '-spec' and '-sv', with which the user provides\n"
            "      the name of the SPEC FILE and the SURFACE VOLUME, along\n"
            "      with '-surf_A' and optionally '-surf_B', used to specify\n"
            "      actual surfaces by name.  Alternatively, the surface\n"
            "      coordinates may come from the '-surf_xyz_1D' option.\n"
            "      See these option descriptions below.\n"
            "\n"
            "      Note that the user must provide either the three options\n"
            "      '-spec', '-sv' and '-surf_A', or the single option,\n"
            "      '-surf_xyz_1D'.\n"
            "\n"
            "  Surface Data:\n"
            "\n"
            "      Surface domain data can be input via the '-sdata_1D'\n"
            "      or '-sdata' option.  In such a case, the data is with \n"
            "      respect to the input surface.  \n"
            "      Note: With -sdata_1D,  the first column of the file \n"
            "      should contain a node's index, and following columns are\n"
            "      that node's data. See the '-sdata_1D' option for more info.\n"
            "      Option -sdata takes NIML or GIFTI input which contain\n"
            "      node index information in their headers.\n"
            "\n"
            "      If the surfaces have V values per node (pair), then the\n"
            "      resulting AFNI dataset will have V sub-bricks (unless the\n"
            "      user applies the '-data_expr' option).\n"
            "\n"
            "  Mapping Functions:\n"
            "\n"
            "      Mapping functions exist because a single volume voxel may\n"
            "      be occupied by multiple surface nodes or segment points.\n"
            "      Depending on how dense the surface mesh is, the number of\n"
            "      steps provided by the '-f_steps' option, and the indexing\n"
            "      type from '-f_index', even a voxel which is only 1 cubic\n"
            "      mm in volume may have quite a few contributing points.\n"
            "\n"
            "      The mapping function defines how multiple surface values\n"
            "      are combined to get a single result in each voxel.  For\n"
            "      example, the 'max' function will take the maximum of all\n"
            "      surface values contributing to each given voxel.\n"
            "\n"
            "      Current mapping functions are listed under the '-map_func'\n"
            "      option, below.\n"
            "\n"
            "------------------------------------------------------------\n"
            "\n"
            "  examples:\n"
            "\n"
            "    1. Map a single surface to an anatomical volume domain,\n"
            "       creating a simple mask of the surface.  The output\n"
            "       dataset will be fred_surf+orig, and the orientation and\n"
            "       grid spacing will follow that of the grid parent.  The\n"
            "       output voxels will be 1 where the surface exists, and 0\n"
            "       elsewhere.\n"
            "\n"
            "    %s                       \\\n"
            "       -spec         fred.spec                \\\n"
            "       -surf_A       pial                     \\\n"
            "       -sv           fred_anat+orig           \\\n"
            "       -grid_parent  fred_anat+orig           \\\n"
            "       -map_func     mask                     \\\n"
            "       -prefix       fred_surf\n"
            "\n"
            "    2. Map the cortical grey ribbon (between the white matter\n"
            "       surface and the pial surface) to an AFNI volume, where\n"
            "       the resulting volume is restricted to the mask implied by\n"
            "       the -cmask option.\n"
            "\n"
            "       Surface data will come from the file sdata_10.1D, which\n"
            "       has 10 values per node, and lists only a portion of the\n"
            "       entire set of surface nodes.  Each node pair will be form\n"
            "       a segment of 15 equally spaced points, the values from\n"
            "       which will be applied to the output dataset according to\n"
            "       the 'ave' filter.  Since the index is over points, each\n"
            "       of the 15 points will have its value applied to the\n"
            "       appropriate voxel, even multiple times.  This weights the\n"
            "       resulting average by the fraction of each segment that\n"
            "       occupies a given voxel.\n"
            "\n"
            "       The output dataset will have 10 sub-bricks, according to\n"
            "       the 10 values per node index in sdata_10.1D.\n"
            "\n"
            "    %s                       \\\n"
            "       -spec         fred.spec                               \\\n"
            "       -surf_A       smoothwm                                \\\n"
            "       -surf_B       pial                                    \\\n"
            "       -sv           fred_anat+orig                          \\\n"
            "       -grid_parent 'fred_func+orig[0]'                      \\\n"
            "       -cmask       '-a fred_func+orig[2] -expr step(a-0.6)' \\\n"
            "       -sdata_1D     sdata_10.1D                             \\\n"
            "       -map_func     ave                                     \\\n"
            "       -f_steps      15                                      \\\n"
            "       -f_index      points                                  \\\n"
            "       -prefix       fred_surf_ave\n"
            "\n"
            "    3. The inputs in this example are identical to those in\n"
            "       example 2, including the surface dataset, sdata_10.1D.\n"
            "       Again, the output dataset will have 10 sub-bricks.\n"
            "\n"
            "       The surface values will be applied via the 'max_abs'\n"
            "       filter, with the intention of assigning to each voxel the\n"
            "       node value with the most significance.  Here, the index\n"
            "       method does not matter, so it is left as the default,\n"
            "       'voxel'.\n"
            "\n"
            "       In this example, each node pair segment will be extended\n"
            "       by 20%% into the white matter, and by 10%% outside of the\n"
            "       grey matter, generating a \"thicker\" result.\n"
            "\n"
            "    %s                       \\\n"
            "       -spec         fred.spec                               \\\n"
            "       -surf_A       smoothwm                                \\\n"
            "       -surf_B       pial                                    \\\n"
            "       -sv           fred_anat+orig                          \\\n"
            "       -grid_parent 'fred_func+orig[0]'                      \\\n"
            "       -cmask       '-a fred_func+orig[2] -expr step(a-0.6)' \\\n"
            "       -sdata_1D     sdata_10.1D                             \\\n"
            "       -map_func     max_abs                                 \\\n"
            "       -f_steps      15                                      \\\n"
            "       -f_p1_fr      -0.2                                    \\\n"
            "       -f_pn_fr       0.1                                    \\\n"
            "       -prefix       fred_surf_max_abs\n"
            "\n"
            "    4. This is similar to example 2.  Here, the surface nodes\n"
            "       (coordinates) come from 'surf_coords_2.1D'.  But these\n"
            "       coordinates do not happen to be in Dicom orientation,\n"
            "       they are in the same orientation as the grid parent, so\n"
            "       the '-sxyz_orient_as_gpar' option is applied.\n"
            "\n"
            "       Even though the data comes from 'sdata_10.1D', the output\n"
            "       AFNI dataset will only have 1 sub-brick.  That is because\n"
            "       of the '-data_expr' option.  Here, each applied surface\n"
            "       value will be the average of the sines of the first 3\n"
            "       data values (columns of sdata_10.1D).\n"
            "\n"
            "    %s                       \\\n"
            "       -surf_xyz_1D  surf_coords_2.1D                        \\\n"
            "       -sxyz_orient_as_gpar                                  \\\n"
            "       -grid_parent 'fred_func+orig[0]'                      \\\n"
            "       -sdata_1D     sdata_10.1D                             \\\n"
            "       -data_expr   '(sin(a)+sin(b)+sin(c))/3'               \\\n"
            "       -map_func     ave                                     \\\n"
            "       -f_steps      15                                      \\\n"
            "       -f_index      points                                  \\\n"
            "       -prefix       fred_surf_ave_sine\n"
            "\n"
            "    5. In this example, voxels will get the maximum value from\n"
            "       column 3 of sdata_10.1D (as usual, column 0 is used for\n"
            "       node indices).  The output dataset will have 1 sub-brick.\n"
            "\n"
            "       Here, the output dataset is forced to be of type 'short',\n"
            "       regardless of what the grid parent is.  Also, there will\n"
            "       be no scaling factor applied.\n"
            "\n"
            "       To track the numbers for surface node #1234, the '-dnode'\n"
            "       option has been used, along with '-debug'.  Additionally,\n"
            "       '-dvoxel' is used to track the results for voxel #6789.\n"
            "\n"
            "    %s                       \\\n"
            "       -spec         fred.spec                               \\\n"
            "       -surf_A       smoothwm                                \\\n"
            "       -surf_B       pial                                    \\\n"
            "       -sv           fred_anat+orig                          \\\n"
            "       -grid_parent 'fred_func+orig[0]'                      \\\n"
            "       -sdata_1D     sdata_10.1D'[0,3]'                      \\\n"
            "       -map_func     max                                     \\\n"
            "       -f_steps      15                                      \\\n"
            "       -datum        short                                   \\\n"
            "       -noscale                                              \\\n"
            "       -debug        2                                       \\\n"
            "       -dnode        1234                                    \\\n"
            "       -dvoxel       6789                                    \\\n"
            "       -prefix       fred_surf_max\n"
            "\n"
            "    6. Draw some surface ROIs, and map them to the volume.  Some\n"
            "       voxels may contain nodes from multiple ROIs, so take the\n"
            "       most common one (the mode), as suggested by R Mruczek.\n"
            "\n"
            "       ROIs are left in 1D format for the -sdata_1D option.\n"
            "\n"
            "\n"
            "    setenv AFNI_NIML_TEXT_DATA YES\n"
            "    ROI2dataset -prefix rois.1D.dset -input rois.niml.roi\n"
            "\n"
            "    %s                           \\\n"
            "       -spec         fred.spec           \\\n"
            "       -surf_A       smoothwm            \\\n"
            "       -surf_B       pial                \\\n"
            "       -sv           fred_anat+orig      \\\n"
            "       -grid_parent 'fred_func+orig[0]'  \\\n"
            "       -sdata_1D     rois.1D.dset        \\\n"
            "       -map_func     mode                \\\n"
            "       -f_steps      10                  \\\n"
            "       -prefix       rois.from.surf\n"
            "\n"
            "\n"
            "------------------------------------------------------------\n"
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
            "        Note: this option, along with '-sv', may be replaced\n"
            "              by the '-surf_xyz_1D' option.\n"
            "\n"
            "    -surf_A SURF_NAME      : specify surface A (from spec file)\n"
            "    -surf_B SURF_NAME      : specify surface B (from spec file)\n"
            "\n"
            "        e.g. -surf_A smoothwm\n"
            "        e.g. -surf_A lh.smoothwm\n"
            "        e.g. -surf_B lh.pial\n"
            "\n"
            "        This parameter is used to tell the program with surfaces\n"
            "        to use.  The '-surf_A' parameter is required, but the\n"
            "        '-surf_B' parameter is an option.\n"
            "\n"
            "        The surface names must uniquely match those in the spec\n"
            "        file, though a sub-string match is good enough.  The\n"
            "        surface names are compared with the names of the surface\n"
            "        node coordinate files.\n"
            "\n"
            "        For instance, given a spec file that has only the left\n"
            "        hemisphere in it, 'pial' should produce a unique match\n"
            "        with lh.pial.asc.  But if both hemispheres are included,\n"
            "        then 'pial' would not be unique (matching rh.pial.asc,\n"
            "        also).  In that case, 'lh.pial' would be better.\n"
            "\n"
            "    -sv SURFACE_VOLUME     : AFNI dataset\n"
            "\n"
            "        e.g. -sv fred_anat+orig\n"
            "\n"
            "        This is the AFNI dataset that the surface is mapped to.\n"
            "        This dataset is used for the initial surface node to xyz\n"
            "        coordinate mapping, in the Dicom orientation.\n"
            "\n"
            "        Note: this option, along with '-spec', may be replaced\n"
            "              by the '-surf_xyz_1D' option.\n"
            "\n"
            "    -surf_xyz_1D SXYZ_NODE_FILE : 1D coordinate file\n"
            "\n"
            "        e.g. -surf_xyz_1D my_surf_coords.1D\n"
            "\n"
            "        This ascii file contains a list of xyz coordinates to be\n"
            "        considered as a surface, or 2 sets of xyz coordinates to\n"
            "        considered as a surface pair.  As usual, these points\n"
            "        are assumed to be in Dicom orientation.  Another option\n"
            "        for coordinate orientation is to use that of the grid\n"
            "        parent dataset.  See '-sxyz_orient_as_gpar' for details.\n"
            "\n"
            "        This option is an alternative to the pair of options, \n"
            "        '-spec' and '-sv'.\n"
            "\n"
            "        The number of rows of the file should equal the number\n"
            "        of nodes on each surface.  The number of columns should\n"
            "        be either 3 for a single surface, or 6 for two surfaces.\n"
            "        \n"
            "        sample line of an input file (one surface):\n"
            "        \n"
            "        11.970287  2.850751  90.896111\n"
            "        \n"
            "        sample line of an input file (two surfaces):\n"
            "        \n"
            "        11.97  2.85  90.90    12.97  2.63  91.45\n"
            "        \n"
            "\n"
            "    -grid_parent AFNI_DSET : AFNI dataset\n"
            "\n"
            "        e.g. -grid_parent fred_function+orig\n"
            "\n"
            "        This dataset is used as a grid and orientation master\n"
            "        for the output AFNI dataset.\n"
            "\n"
            "    -map_func MAP_FUNC     : surface to dataset function\n"
            "\n"
            "        e.g. -map_func max\n"
            "        e.g. -map_func mask -f_steps 20\n"
            "\n"
            "        This function applies to the case where multiple data\n"
            "        points get mapped to a single voxel, which is expected\n"
            "        since surfaces tend to have a much higher resolution\n"
            "        than AFNI volumes.  In the general case data points come\n"
            "        from each point on each partitioned line segment, with\n"
            "        one segment per node pair.  Note that these segments may\n"
            "        have length zero, such as when only a single surface is\n"
            "        input.\n"
            "\n"
            "        See \"Mapping Functions\" above, for more information.\n"
            "\n"
            "        The current mapping function for one surface is:\n"
            "\n"
            "          mask   : For each xyz location, set the corresponding\n"
            "                   voxel to 1.\n"
            "\n"
            "        The current mapping functions for two surfaces are as\n"
            "        follows.  These descriptions are per output voxel, and\n"
            "        over the values of all points mapped to a given voxel.\n"
            "\n"
            "          mask2  : if any points are mapped to the voxel, set\n"
            "                   the voxel value to 1\n"
            "\n"
            "          ave    : average all values\n"
            "\n"
            "          nzave  : ave, but ignorning any zero values\n"
            "\n"
            "          count  : count the number of mapped data points\n"
            "\n"
            "          min    : find the minimum value from all mapped points\n"
            "\n"
            "          max    : find the maximum value from all mapped points\n"
            "\n"
            "          max_abs: find the number with maximum absolute value\n"
            "                   (the resulting value will retain its sign)\n"
            "\n"
            "          median : median of all mapped values\n"
            "\n"
            "          nzmedian: median, but ignoring any zero values\n"
            "\n"
            "          mode   : apply the most common value per voxel\n"
            "                   (appropriate where surf ROIs overlap)\n"
            "\n"
            "          nzmode : mode, but ignoring any zero values\n"
            "\n"
            "    -prefix OUTPUT_PREFIX  : prefix for the output dataset\n"
            "\n"
            "        e.g. -prefix anat_surf_mask\n"
            "\n"
            "        This is used to specify the prefix of the resulting AFNI\n"
            "        dataset.\n"
            "\n"
            "  ------------------------------\n"
            "  SUB-SURFACE DATA FILE OPTIONS:\n"
            "\n"
            "    -sdata_1D SURF_DATA.1D : 1D sub-surface file, with data\n"
            "\n"
            "        e.g. -sdata_1D roi3.1D\n"
            "\n"
            "        This is used to specify a 1D file, which contains\n"
            "        surface indices and data.  The indices refer to the\n"
            "        surface(s) read from the spec file.\n"
            "        \n"
            "        The format of this data file is a surface index and a\n"
            "        list of data values on each row.  To be a valid 1D file,\n"
            "        each row must have the same number of columns.\n"
            "\n"
            "    -sdata SURF_DATA_DSET: NIML, or GIFTI formatted dataset.\n"
            "\n"
            "  ------------------------------\n"
            "  OPTIONS SPECIFIC TO SEGMENT SELECTION:\n"
            "\n"
            "    (see \"The basic form of the algorithm\" for more details)\n"
            "\n"
            "    -f_steps NUM_STEPS     : partition segments\n"
            "\n"
            "        e.g. -f_steps 10\n"
            "        default: -f_steps 2   (or 1, the number of surfaces)\n"
            "\n"
            "        This option specifies the number of points to divide\n"
            "        each line segment into, before mapping the points to the\n"
            "        AFNI volume domain.  The default is the number of input\n"
            "        surfaces (usually, 2).  The default operation is to have\n"
            "        the segment endpoints be the actual surface nodes,\n"
            "        unless they are altered with the -f_pX_XX options.\n"
            "\n"
            "    -f_index TYPE          : index by points or voxels\n"
            "\n"
            "        e.g. -f_index points\n"
            "        e.g. -f_index voxels\n"
            "        default: -f_index voxels\n"
            "\n"
            "        Along a single segment, the default operation is to\n"
            "        apply only those points mapping to a new voxel.  The\n"
            "        effect of the default is that a given voxel will have\n"
            "        at most one value applied per voxel pair.\n"
            "\n"
            "        If the user applies this option with 'points' or 'nodes'\n"
            "        as the argument, then every point along the segment will\n"
            "        be applied.  This may be preferred if, for example, the\n"
            "        user wishes to have the average weighted by the number\n"
            "        of points occupying a voxel, not just the number of node\n"
            "        pair segments.\n"
            "\n"
            "    Note: the following -f_pX_XX options are used to alter the\n"
            "          locations of the segment endpoints, per node pair.\n"
            "          The segments are directed, from the node on the first\n"
            "          surface to the node on the second surface.  To modify\n"
            "          the first endpoint, use a -f_p1_XX option, and use\n"
            "          -f_pn_XX to modify the second.\n"
            "\n"
            "    -f_p1_fr FRACTION      : offset p1 by a length fraction\n"
            "\n"
            "        e.g. -f_p1_fr -0.2\n"
            "        e.g. -f_p1_fr -0.2  -f_pn_fr 0.2\n"
            "\n"
            "        This option moves the first endpoint, p1, by a distance\n"
            "        of the FRACTION times the original segment length.  If\n"
            "        the FRACTION is positive, it moves in the direction of\n"
            "        the second endpoint, pn.\n"
            "\n"
            "        In the example, p1 is moved by 20%% away from pn, which\n"
            "        will increase the length of each segment.\n"
            "\n"
            "    -f_pn_fr FRACTION      : offset pn by a length fraction\n"
            "\n"
            "        e.g. -f_pn_fr  0.2\n"
            "        e.g. -f_p1_fr -0.2  -f_pn_fr 0.2\n"
            "\n"
            "        This option moves pn by a distance of the FRACTION times\n"
            "        the original segment length, in the direction from p1 to\n"
            "        pn.  So a positive fraction extends the segment, and a\n"
            "        negative fraction reduces it.\n"
            "\n"
            "        In the example above, using 0.2 adds 20%% to the segment\n"
            "        length past the original pn.\n"
            "\n"
            "    -f_p1_mm DISTANCE      : offset p1 by a distance in mm.\n"
            "\n"
            "        e.g. -f_p1_mm -1.0\n"
            "        e.g. -f_p1_mm -1.0  -f_pn_fr 1.0\n"
            "\n"
            "        This option moves p1 by DISTANCE mm., in the direction\n"
            "        of pn.  If the DISTANCE is positive, the segment gets\n"
            "        shorter.  If DISTANCE is negative, the segment will get\n"
            "        longer.\n"
            "\n"
            "        In the example, p1 is moved away from pn, extending the\n"
            "        segment by 1 millimeter.\n"
            "\n"
            "    -f_pn_mm DISTANCE      : offset pn by a distance in mm.\n"
            "\n"
            "        e.g. -f_pn_mm  1.0\n"
            "        e.g. -f_p1_mm -1.0  -f_pn_fr 1.0\n"
            "\n"
            "        This option moves pn by DISTANCE mm., in the direction\n"
            "        from the first point to the second.  So if DISTANCE is\n"
            "        positive, the segment will get longer.  If DISTANCE is\n"
            "        negative, the segment will get shorter.\n"
            "\n"
            "        In the example, pn is moved 1 millimeter farther from\n"
            "        p1, extending the segment by that distance.\n"
            "\n"
            "    -stop_gap              : stop when a zero gap has been hit\n"
            "\n"
            "        This limits segment processing such that once a non-zero\n"
            "        mask value has been encountered, the segment will be\n"
            "        terminated on any subsequent zero mask value.\n"
            "        \n"
            "        The goal is to prevent mixing masked cortex regions.\n"
            "\n"
            "  ------------------------------\n"
            "  GENERAL OPTIONS:\n"
            "\n"
            "    -cmask MASK_COMMAND    : command for dataset mask\n"
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
            "    -data_expr EXPRESSION  : apply expression to surface input\n"
            "\n"
            "        e.g. -data_expr 17\n"
            "        e.g. -data_expr '(a+b+c+d)/4'\n"
            "        e.g. -data_expr '(sin(a)+sin(b))/2'\n"
            "\n"
            "        This expression is applied to the list of data values\n"
            "        from the surface data file input via '-sdata_1D'.  The\n"
            "        expression is applied for each node or node pair, to the\n"
            "        list of data values corresponding to that node.\n"
            "\n"
            "        The letters 'a' through 'z' may be used as input, and\n"
            "        refer to columns 1 through 26 of the data file (where\n"
            "        column 0 is a surface node index).  The data file must\n"
            "        have enough columns to support the expression.  It is\n"
            "        valid to have a constant expression without a data file.\n"
            "\n"
            "    -datum DTYPE           : set data type in output dataset\n"
            "\n"
            "        e.g. -datum short\n"
            "        default: same as that of grid parent\n"
            "\n"
            "        This option specifies the data type for the output AFNI\n"
            "        dataset.  Valid choices are byte, short and float, which\n"
            "        are 1, 2 and 4 bytes for each data point, respectively.\n"
            "\n"
            "    -debug LEVEL           : verbose output\n"
            "\n"
            "        e.g. -debug 2\n"
            "\n"
            "        This option is used to print out status information \n"
            "        during the execution of the program.  Current levels are\n"
            "        from 0 to 5.\n"
            "\n"
            "    -dnode DEBUG_NODE      : extra output for that node\n"
            "\n"
            "        e.g. -dnode 123456\n"
            "\n"
            "        This option requests additional debug output for the\n"
            "        given surface node.  This index is with respect to the\n"
            "        input surface (included in the spec file, or through the\n"
            "        '-surf_xyz_1D' option).\n"
            "\n"
            "        This will have no effect without the '-debug' option.\n"
            "\n"
            "    -dvoxel DEBUG_VOXEL    : extra output for that voxel\n"
            "\n"
            "        e.g. -dvoxel 234567\n"
            "\n"
            "        This option requests additional debug output for the\n"
            "        given volume voxel.  This 1-D index is with respect to\n"
            "        the output AFNI dataset.  One good way to find a voxel\n"
            "        index to supply is from output via the '-dnode' option.\n"
            "\n"
            "        This will have no effect without the '-debug' option.\n"
            "\n"
            "    -hist                  : show revision history\n"
            "\n"
            "        Display module history over time.\n"
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
            "    -sxyz_orient_as_gpar   : assume gpar orientation for sxyz\n"
            "\n"
            "        This option specifies that the surface coordinate points\n"
            "        in the '-surf_xyz_1D' option file have the orientation\n"
            "        of the grid parent dataset.\n"
            "\n"
            "        When the '-surf_xyz_1D' option is applied the surface\n"
            "        coordinates are assumed to be in Dicom orientation, by\n"
            "        default.  This '-sxyz_orient_as_gpar' option overrides\n"
            "        the Dicom default, specifying that the node coordinates\n"
            "        are in the same orientation as the grid parent dataset.\n"
            "\n"
            "        See the '-surf_xyz_1D' option for more information.\n"
            "\n"
            "    -version               : show version information\n"
            "\n"
            "        Show version and compile date.\n"
            "\n"
            "------------------------------------------------------------\n"
            "\n"
            "  Author: R. Reynolds  - %s\n"
            "\n"
            "                (many thanks to Z. Saad and R.W. Cox)\n"
            "\n",
            prog, prog,
            prog, prog, prog, prog, prog, prog,
            VERSION );
    }
    else if ( level == S2V_USE_HIST )
        fputs(g_history, stdout);
    else if ( level == S2V_USE_VERSION )
        printf( "%s : %s, compile date: %s\n", prog, VERSION, __DATE__ );
    else
        fprintf( stderr, "usage called with illegal level <%d>\n", level );

    RETURN(-1);
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
        fprintf(stderr, "disp_opts_t: opts == NULL\n" );
        RETURN(-1);
    }

    fprintf(stderr,
            "options struct at %p :\n"
            "    gpar_file          = %s\n"
            "    oset_file          = %s\n"
            "    spec_file          = %s\n"
            "    sv_file            = %s\n"
            "    surf_xyz_1D_file   = %s\n"
            "    sdata_file_1D      = %s\n"
            "    sdata_file_niml    = %s\n"
            "    cmask_cmd          = %s\n"
            "    data_expr          = %s\n"
            "    map_str            = %s\n"
            "    datum_str          = %s\n"
            "    f_index_str        = %s\n"
            "    stop_gap           = %d\n"
            "    sxyz_ori_gpar      = %d\n"
            "    debug, dnode, dvox = %d, %d, %d\n"
            "    noscale, f_steps   = %d, %d\n"
            "    f_p1_fr, f_pn_fr   = %f, %f\n"
            "    f_p1_mm, f_pn_mm   = %f, %f\n"
            , opts,
            CHECK_NULL_STR(opts->gpar_file), CHECK_NULL_STR(opts->oset_file),
            CHECK_NULL_STR(opts->spec_file), CHECK_NULL_STR(opts->sv_file),
            CHECK_NULL_STR(opts->surf_xyz_1D_file),
            CHECK_NULL_STR(opts->sdata_file_1D),
            CHECK_NULL_STR(opts->sdata_file_niml),
            CHECK_NULL_STR(opts->cmask_cmd), CHECK_NULL_STR(opts->data_expr),
            CHECK_NULL_STR(opts->map_str), CHECK_NULL_STR(opts->datum_str),
            CHECK_NULL_STR(opts->f_index_str),
            opts->sxyz_ori_gpar, opts->stop_gap,
            opts->debug, opts->dnode, opts->dvox, opts->noscale, opts->f_steps,
            opts->f_p1_fr, opts->f_pn_fr, opts->f_p1_mm, opts->f_pn_mm
            );

    RETURN(0);
}


/*----------------------------------------------------------------------
 * disp_param_t  -  display the contents of the param_t struct
 *----------------------------------------------------------------------
*/
int disp_param_t ( char * info, param_t * p )
{
ENTRY("disp_param_t");

    if ( info )
        fputs( info, stderr );

    if ( p == NULL )
    {
        fprintf(stderr,"disp_param_t: p == NULL\n");
        RETURN(-1);
    }

    fprintf(stderr,
            "param_t struct at %p :\n"
            "    gpar  : vcheck    = %p : %s\n"
            "    oset  : vcheck    = %p : %s\n"
            "    sxyz_im, sdata_im = %p, %p\n"
            "             dset     = %p\n"
            "    f3mm_min (xyz)    = (%f, %f, %f)\n"
            "    f3mm_max (xyz)    = (%f, %f, %f)\n"
            "    nvox, nsubs       = %d, %d\n"
            "    cmask             = %p\n"
            "    ncmask, ccount    = %d, %d\n"
            , p,
            p->gpar, ISVALID_DSET(p->gpar) ? "valid" : "invalid",
            p->oset, ISVALID_DSET(p->oset) ? "valid" : "invalid",
            p->sxyz_im, p->sdata_im,
            p->dset,
            p->f3mm_min.xyz[0], p->f3mm_min.xyz[1], p->f3mm_min.xyz[2],
            p->f3mm_max.xyz[0], p->f3mm_max.xyz[1], p->f3mm_max.xyz[2],
            p->nvox, p->nsubs, p->cmask, p->ncmask, p->ccount
            );

    RETURN(0);
}


/*----------------------------------------------------------------------
 * disp_node_list_t  -  display the contents of the node_list_t struct
 *----------------------------------------------------------------------
*/
int disp_node_list_t ( char * info, node_list_t * d )
{
ENTRY("disp_node_list_t");

    if ( info )
        fputs( info, stderr );

    if ( d == NULL )
    {
        fprintf(stderr,"disp_node_list_t: d == NULL\n");
        RETURN(-1);
    }

    fprintf(stderr,
            "node_list_t struct at %p :\n"
            "    nodes         = %p\n"
            "    depth, nnodes = %d, %d\n"
            "    fdata, ilist  = %p, %p\n"
            "    ilen          = %d\n"
            , d,
            d->nodes, d->depth, d->nnodes,
            d->fdata, d->ilist, d->ilen
            );

    RETURN(0);
}


/*----------------------------------------------------------------------
 * disp_s2v_opts_t  -  display the contents of the s2v_opts_t struct
 *----------------------------------------------------------------------
*/
int disp_s2v_opts_t ( char * info, s2v_opts_t * sopt )
{
ENTRY("disp_s2v_opts_t");

    if ( info )
        fputs( info, stderr );

    if ( sopt == NULL )
    {
        fprintf(stderr,"disp_s2v_opts_t: sopt == NULL\n" );
        RETURN(-1);
    }

    fprintf(stderr,
            "s2v_opts_t struct at %p :\n"
            "    map, datum, noscale = %d, %d, %d\n"
            "    debug, dnode, dvox  = %d, %d, %d\n"
            "    stop_gap            = %d\n"
            "    sxyz_ori_gpar       = %d\n"
            "    cmask               = %p\n"
            "    f_steps, f_index    = %d, %d\n"
            "    f_p1_fr, f_pn_fr    = %f, %f\n"
            "    f_p1_mm, f_pn_mm    = %f, %f\n"
            , sopt,
            sopt->map, sopt->datum, sopt->noscale,
            sopt->debug, sopt->dnode, sopt->dvox,
            sopt->stop_gap, sopt->sxyz_ori_gpar,
            sopt->cmask, sopt->f_steps, sopt->f_index,
            sopt->f_p1_fr, sopt->f_pn_fr, sopt->f_p1_mm, sopt->f_pn_mm
            );

    RETURN(0);
}


/*----------------------------------------------------------------------
 * disp_parser_t  -  display the contents of the parser_t struct
 *----------------------------------------------------------------------
*/
int disp_parser_t ( char * info, parser_t * d )
{
    int c;

ENTRY("disp_parser_t");

    if ( info )
        fputs( info, stderr );

    if ( d == NULL )
    {
        fprintf(stderr,"disp_parser_t: d == NULL\n" );
        RETURN(-1);
    }

    fprintf(stderr, "parser_t struct at %p :\n"
                    "    pcode    = %p\n"
                    "    max_sym  = %d\n",
                    d, d->pcode, d->max_sym );

    if ( d->pcode )
    {
        fprintf(stderr, "    num_code = %d\n"
                        "    c_code   = %s\n",
                d->pcode->num_code, d->pcode->c_code );

        fprintf(stderr, "    has_sym  =");
        for ( c = 0; c < 26; c++ )
            fprintf(stderr, " %d", d->has_sym[c]);
        fputc('\n', stderr);
    }

    RETURN(0);
}


/*----------------------------------------------------------------------
 * set_3dmm_bounds       - note 3dmm bounding values            - v1.2
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
 * dist_f3mm            - return Euclidean distance between the points
 *                      - v1.2
 *----------------------------------------------------------------------
 */
float dist_f3mm( THD_fvec3 * p1, THD_fvec3 * p2 )
{
    double d0, d1, d2;

ENTRY("dist_f3mm");

    if ( p1 == NULL || p2 == NULL )
    {
        fprintf( stderr, "** dist_f3mm: invalid params (%p,%p)\n", p1, p2 );
        RETURN(0.0);
    }

    d0 = p1->xyz[0] - p2->xyz[0];
    d1 = p1->xyz[1] - p2->xyz[1];
    d2 = p1->xyz[2] - p2->xyz[2];

    RETURN(sqrt(d0*d0 + d1*d1 + d2*d2));
}


/*----------------------------------------------------------------------
 * f3mm_out_of_bounds    - check wether cp is between min and max
 *                       - v1.2 [rickr]
 *----------------------------------------------------------------------
*/
int f3mm_out_of_bounds( THD_fvec3 * cp, THD_fvec3 * min, THD_fvec3 * max )
{
    int c;

ENTRY("f3mm_out_of_bounds");

    if ( !cp || !min || !max )
        RETURN(-1);

    for ( c = 0; c < 3; c++ )
    {
        if ( ( cp->xyz[c] < min->xyz[c] ) ||
             ( cp->xyz[c] > max->xyz[c] ) )
            RETURN(-1);
    }

    RETURN(0);
}



/*----------------------------------------------------------------------
 * integral_doubles      - are all double values integral?
 *----------------------------------------------------------------------
*/
int integral_doubles( double * dp, int nvals )
{
ENTRY("integral_doubles");

    while ( nvals > 0 )
    {
        if ( ((double)(int)*dp) != *dp )
            RETURN(0);

        dp++;
        nvals--;
    }

    RETURN(1);
}
