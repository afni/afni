#include "mrilib.h"
#include "r_idisp.h"

#define MAIN

/*----------------------------------------------------------------------
 * 3dresample - create a new dataset by reorienting and resampling
 *              an existing one 
 *
 * This program can be used to 
 *    - change the orientation of a dataset to one that is specified
 *    - change the dx, dy, dz spacing, to one that is specified
 *    - master a dataset, so that its orientation and spacing matches
 *
 * usage:  3dresample  [options]  -prefix OUTPUT_DSET  -input INPUT_DSET
 *
 *    options:
 *              -help             : detailed program info
 *              -debug LEVEL      : spit out info
 *              -version          : print version info
 *
 *              -dxyz DX DY DZ    : resample to a new grid
 *                                      (DX, DY, DZ are real numbers in mm)
 *              -orient OR_CODE   : reorient to new orientation code
 *                                      (a three character string, each
 *                                       from the set {A,P, I,S, L,R})
 *
 *              -master MAST_DSET : apply orient/dxyz from MAST_DSET
 *
 *              -rmode RESAM      : one of {"NN", "Li", "Cu", "Bk"}
 *
 *              -bound_type TYPE  : one of {"FOV", "SLAB"}
 *
 *    examples:
 *      3dresample -orient "asl" -rmode NN -prefix asl.dset -input inset+orig
 *      3dresample -dxyz 1.0 1.0 0.9 -prefix 119.dset -input some.input+tlrc
 *      3dresample -master master+orig -prefix new.copy -input old.copy+orig
 *----------------------------------------------------------------------
*/

static char g_history[] =
 "----------------------------------------------------------------------\n"
 " history:\n"
 "\n"
 " 1.0  May 23, 2002 - initial release\n"
 " 1.1  Jul 02, 2002 - modified to fully align new data set grid to master\n"
 " 1.2  Jul 29, 2002\n"
 "   - no change here, but updated r_new_resam_dset() for view type\n"
 " 1.3  January 14, 2003\n"
 "   - clear warp information before writing to disk (fix uncommon problem)\n"
 " 1.4  Jul 27, 2003 - wrap unknown printed strings in NULL check\n"
 " 1.5  Jan 07, 2004\n"
 "   - added suggestion of 3dfractionize to -help output\n"
 "   - added '-hist' option\n"
 " 1.6  Mar 04, 2004\n"
 "   - added check for RESAM_shortstr[] (to catch NN and Bk modes)\n"
 "   - reversed order of history: recent at the bottom\n"
 " 1.7  Jul 26, 2004 - passed NULL sublist to r_new_resam_dset()\n"
 " 1.7a Mar 22, 2005 - removed tabs\n"
 " 1.8  Aug 02, 2005 - allow dxyz to override those from master\n"
 " 1.9  Apr 27, 2009 - small help update (also, show help if no args)\n"
 " 1.10 Jun 26, 2014 - added -bound_type FOV/SLAB\n"
 "----------------------------------------------------------------------\n";

#define VERSION "Version 1.10 <June 26, 2014>"


/*--- local stuff ------------------------------------------------------*/

#define USE_LONG        1
#define USE_SHORT       2
#define USE_VERSION     3
#define USE_HISTORY     4

#define DELTA_MIN        0.0000001
#define DELTA_MAX       9999.9

#define RL_DEBUG_OFF    0
#define RL_DEBUG_LOW    1
#define RL_DEBUG_HIGH   2

typedef struct
{
    THD_3dim_dataset * dset;
    THD_3dim_dataset * mset;
    double             dx, dy, dz;
    char             * orient;
    char             * prefix;
    int                resam;
    int                bound_type;
    int                debug;
} options_t;

int disp_opts_data   ( char * info, options_t * opts );
int init_options     ( options_t * opts, int argc, char * argv [] );
int sync_master_opts ( options_t * opts );
int usage            ( char * prog, int level );
int write_results    ( THD_3dim_dataset * dout, options_t * opts,
                       int argc, char * argv [] );

/*----------------------------------------------------------------------*/

int main( int argc , char * argv[] )
{
    THD_3dim_dataset * dout;
    options_t          opts;
    int                ret_val;

    mainENTRY("3dresample"); machdep(); AFNI_logger("3dresample",argc,argv);

    /* validate inputs and init options structure */
    if ( (ret_val = init_options(&opts, argc, argv)) != 0 ) {
        if( ret_val < 0 ) return 1;
        else              return 0;
    }

    /* actually resample and/or reorient the dataset */
    dout = r_new_resam_dset_eng(opts.dset, opts.mset, opts.dx,opts.dy,opts.dz,
                                opts.orient, opts.resam, NULL, 1, 0,
                                opts.bound_type);
    if ( dout == NULL )
    {
        fprintf( stderr, "failure to resample dataset, exiting...\n" );
        return FAIL;
    }

    return write_results( dout, &opts, argc, argv );
}


/*----------------------------------------------------------------------
 * init_options - validate inputs, give help, init options struct
 *----------------------------------------------------------------------
*/
int init_options ( options_t * opts, int argc, char * argv [] )
{
    int ac;

    /* clear out the options structure, and explicitly set pointers */
    memset( opts, 0, sizeof(options_t) );
    opts->orient = opts->prefix = NULL; /* laziness with proper conversions */
    opts->dset   = opts->mset   = NULL;  

    /* show help if there are no arguments */
    if ( argc < 2 ) { usage( argv[0], USE_LONG ); return 1; }

    for ( ac = 1; ac < argc; ac++ )
    {
        if ( ! strncmp(argv[ac], "-help", 5) )
        {
            usage( argv[0], USE_LONG );
            return 1;
        }
        else if ( ! strncmp(argv[ac], "-hist", 5) )
        {
            usage( argv[0], USE_HISTORY );
            return 1;
        }
        else if ( ! strncmp(argv[ac], "-version", 2) )
        {
            usage( argv[0], USE_VERSION );
            return 1;
        }
        else if ( ! strncmp(argv[ac], "-bound_type", 6) ) /* 26 Jun 2014 */
        {
            if ( (ac+1) >= argc )
            {
                fputs( "option usage: -bound_type FOV/SLAB\n", stderr );
                usage( argv[0], USE_SHORT );
                return FAIL;
            }

            if ( (opts->bound_type = resam_str2bound(argv[++ac])) < 0 )
            {
                fprintf( stderr, "invalid -bound_type <%s>\n", argv[ac] );
                return FAIL;
            }
        }
        else if ( ! strncmp(argv[ac], "-debug", 6) )
        {
            if ( (ac+1) >= argc )
            {
                fputs( "option usage: -debug LEVEL\n", stderr );
                usage( argv[0], USE_SHORT );
                return FAIL;
            }

            opts->debug = atoi(argv[++ac]);
            if ( opts->debug < 0 || opts->debug > RL_DEBUG_HIGH )
            {
                fprintf( stderr, "bad debug level <%d>, should be in [%d,%d]\n",
                        opts->debug, RL_DEBUG_OFF, RL_DEBUG_HIGH );
                usage( argv[0], USE_SHORT );
                return FAIL;
            }
        }
        else if ( ! strncmp(argv[ac], "-dxyz", 3) )     /* dxyz */
        {
            if ( (ac+3) >= argc )
            {
                fputs( "option usage: -dxyz DX DY DZ\n", stderr );
                usage( argv[0], USE_SHORT );
                return FAIL;
            }

            opts->dx = atof(argv[++ac]);
            opts->dy = atof(argv[++ac]);
            opts->dz = atof(argv[++ac]);

            if ( (opts->dx < DELTA_MIN || opts->dx > DELTA_MAX) ||
                 (opts->dy < DELTA_MIN || opts->dy > DELTA_MAX) ||
                 (opts->dz < DELTA_MIN || opts->dz > DELTA_MAX) )
            {
                fprintf( stderr, "dxyz must be in [%.1f,%.1f]\n",
                         DELTA_MIN, DELTA_MAX );
                return FAIL;
            }
        }
        else if ( ! strncmp(argv[ac], "-or", 3) )       /* orientation */
        {
            if ( (ac+1) >= argc )
            {
                fputs( "option usage: -orient OR_STRING\n", stderr );
                usage( argv[0], USE_SHORT );
                return FAIL;
            }

            opts->orient = argv[++ac];
        }
        else if ( ! strncmp(argv[ac], "-master", 5) )   /* master */
        {
            if ( (ac+1) >= argc )
            {
                fputs( "option usage: -master MAST_DSET\n", stderr );
                usage( argv[0], USE_SHORT );
                return FAIL;
            }

            opts->mset = THD_open_dataset( argv[++ac] );
            if ( ! ISVALID_DSET(opts->mset) )
            {
                fprintf( stderr, "invalid master dataset <%s>\n", argv[ac] );
                return FAIL;
            }
        }
        else if ( ! strncmp(argv[ac], "-zeropad", 5) )  /* zeropad */
        {
            fputs("warning: '-zeropad' is no longer a valid option\n", stderr);
            /* but still move on... */
        }
        else if ( ! strncmp(argv[ac], "-rmode", 6) )    /* resample mode */
        {
            if ( (ac+1) >= argc )
            {
                fputs( "option usage: -rmode RESAMPLE_MODE\n", stderr );
                usage( argv[0], USE_SHORT );
                return FAIL;
            }

            if ( ( (opts->resam = resam_str2mode(argv[++ac]) ) < 0 ) ||
                 (  opts->resam > LAST_RESAM_TYPE ) )
            {
                fprintf( stderr, "invalid resample mode <%s>\n", argv[ac] );
                return FAIL;
            }
        }
        else if ( ! strncmp(argv[ac], "-prefix", 4) )   /* new dset prefix */
        {
            if ( (ac+1) >= argc )
            {
                fputs( "option usage: -prefix OUTPUT_PREFIX\n", stderr );
                usage( argv[0], USE_SHORT );
                return FAIL;
            }

            opts->prefix = argv[++ac];
            if ( !THD_filename_ok(opts->prefix) )
            {
                fprintf( stderr, "invalid output prefix <%s>\n", opts->prefix );
                return usage( argv[0], USE_SHORT );
            }
        }
        else if ( ! strncmp(argv[ac], "-inset", 3) ||
                  ! strncmp(argv[ac], "-input", 6) )    /* input dset */
        {
            if ( (ac+1) >= argc )
            {
                fputs( "option usage: -input INPUT_DSET\n", stderr );
                usage( argv[0], USE_SHORT );
                return FAIL;
            }

            opts->dset = THD_open_dataset( argv[++ac] );
            if ( ! ISVALID_DSET(opts->dset) )
            {
                fprintf( stderr, "invalid input dataset <%s>\n", argv[ac] );
                return FAIL;
            }
        }
        else     /* invalid option */
        {
            fprintf( stderr, "invalid option <%s>\n", argv[ac] );
            usage( argv[0], USE_SHORT );
            return FAIL;
        }
    }

    if ( !ISVALID_DSET(opts->dset) || (opts->prefix == NULL) )
    {
        fprintf( stderr, "missing prefix or input dset, exiting...\n" );
        usage( argv[0], USE_SHORT );
        return FAIL;
    }

    if ( opts->debug >= RL_DEBUG_LOW )
    {
        disp_opts_data( "++ options initialized: ", opts );

        if ( opts->debug >= RL_DEBUG_HIGH )     /* dset is valid by now */
        {
            r_idisp_thd_3dim_dataset( "inset : ", opts->dset );
            r_idisp_thd_dataxes     ( "inset : ", opts->dset->daxes );
            r_idisp_thd_datablock   ( "inset : ", opts->dset->dblk  );
            if ( opts->dset->dblk )
                r_idisp_thd_diskptr ( "inset : ", opts->dset->dblk->diskptr );
        }
    }

    if ( sync_master_opts( opts ) )
        return FAIL;

    return 0;
}

#if 0  /* lose new_zeropad_dset */
/*----------------------------------------------------------------------
 * new_zeropad_dset - create a new zeropadded dataset
 *
 * Replace dout with a new padded one.
 *
 * This function copies the master part of 3dZeropad.c.
 *----------------------------------------------------------------------
*/
int new_zeropad_dset ( options_t * opts, THD_3dim_dataset ** dout )
{
    THD_3dim_dataset * tmp_dset;
    THD_dataxes      * max = opts->mset->daxes, * iax = (*dout)->daxes;
    int                nerr = 0;
    float              mxbot,mybot,mzbot, mxtop,mytop,mztop, mdx,mdy,mdz;
    float              ixbot,iybot,izbot, ixtop,iytop,iztop, idx,idy,idz;
    int                mnx,mny,mnz, inx,iny,inz;
    int                add_xb,add_xt, add_yb,add_yt, add_zb,add_zt;
    int                add_I=0, add_S=0, add_A=0, add_P=0, add_L=0, add_R=0;

    /* check if datasets are oriented the same */
    if( max->xxorient != iax->xxorient ||
        max->yyorient != iax->yyorient ||
        max->zzorient != iax->zzorient )
    {
        fputs("error: orientation mismatch!\n", stderr );
        nerr++;
    }

    /* check if datasets have same voxel dimensions */
    mdx = max->xxdel;  mdy = max->yydel; mdz = max->zzdel;
    idx = iax->xxdel;  idy = iax->yydel; idz = iax->zzdel;
    mnx = max->nxx;    mny = max->nyy;   mnz = max->nzz;
    inx = iax->nxx;    iny = iax->nyy;   inz = iax->nzz;

    if( fabs(mdx-idx) > 0.01*fabs(mdx) ||
        fabs(mdy-idy) > 0.01*fabs(mdy) ||
        fabs(mdz-idz) > 0.01*fabs(mdz) )
    {
       fputs("error: voxel size mismatch!\n", stderr);
       nerr++;
    }

    if ( nerr > 0 )
        return FAIL;    /* we have already printed the failure cause(s) */

    /* the data looks okay */

    /* calculate coords at top and bottom of each dataset */
    mxbot = max->xxorg; mxtop = mxbot + mnx*mdx;
    mybot = max->yyorg; mytop = mybot + mny*mdy;
    mzbot = max->zzorg; mztop = mzbot + mnz*mdz;

    ixbot = iax->xxorg; ixtop = ixbot + inx*idx;
    iybot = iax->yyorg; iytop = iybot + iny*idy;
    izbot = iax->zzorg; iztop = izbot + inz*idz;

    /* calculate amount to add/trim at each face */
    add_xb = (int) rint((ixbot-mxbot)/idx);
    add_xt = (int) rint((mxtop-ixtop)/idx);
    add_yb = (int) rint((iybot-mybot)/idy);
    add_yt = (int) rint((mytop-iytop)/idy);
    add_zb = (int) rint((izbot-mzbot)/idz);
    add_zt = (int) rint((mztop-iztop)/idz);

    /* map trims from x,y,z to RL,AP,IS coords */

    switch( iax->xxorient ){
        case ORI_R2L_TYPE: add_R = add_xb; add_L = add_xt; break;
        case ORI_L2R_TYPE: add_L = add_xb; add_R = add_xt; break;
        case ORI_I2S_TYPE: add_I = add_xb; add_S = add_xt; break;
        case ORI_S2I_TYPE: add_S = add_xb; add_I = add_xt; break;
        case ORI_A2P_TYPE: add_A = add_xb; add_P = add_xt; break;
        case ORI_P2A_TYPE: add_P = add_xb; add_A = add_xt; break;
        default          : fputs("bad xxorient!\n", stderr); return FAIL;
    }

    switch( iax->yyorient ){
        case ORI_R2L_TYPE: add_R = add_yb; add_L = add_yt; break;
        case ORI_L2R_TYPE: add_L = add_yb; add_R = add_yt; break;
        case ORI_I2S_TYPE: add_I = add_yb; add_S = add_yt; break;
        case ORI_S2I_TYPE: add_S = add_yb; add_I = add_yt; break;
        case ORI_A2P_TYPE: add_A = add_yb; add_P = add_yt; break;
        case ORI_P2A_TYPE: add_P = add_yb; add_A = add_yt; break;
        default          : fputs("bad yyorient!\n", stderr); return FAIL;
    }

    switch( iax->zzorient ){
        case ORI_R2L_TYPE: add_R = add_zb; add_L = add_zt; break;
        case ORI_L2R_TYPE: add_L = add_zb; add_R = add_zt; break;
        case ORI_I2S_TYPE: add_I = add_zb; add_S = add_zt; break;
        case ORI_S2I_TYPE: add_S = add_zb; add_I = add_zt; break;
        case ORI_A2P_TYPE: add_A = add_zb; add_P = add_zt; break;
        case ORI_P2A_TYPE: add_P = add_zb; add_A = add_zt; break;
        default          : fputs("bad zzorient!\n", stderr); return FAIL;
    }

    if ( opts->debug >= RL_DEBUG_LOW )
    {
        printf( "++ zeropad: (I,S,A,P,L,R) = (%d,%d,%d,%d,%d,%d)\n",
                add_I, add_S, add_A, add_P, add_L, add_R );
    }

    /* pad if we need to */
    if ( add_I || add_S || add_A || add_P || add_L || add_R )
    {
        tmp_dset = THD_zeropad( *dout,
                                add_I, add_S, add_A, add_P, add_L, add_R,
                                opts->prefix, ZPAD_PURGE );

        if ( !ISVALID_DSET( tmp_dset ) )
        {
            fputs( "THD_zeropad failed!\n", stderr );
            return FAIL;
        }

        DSET_delete( *dout );
        *dout = tmp_dset;
    }

    return 0;
}
#endif   /* end chop of new_zeropad_dset() */

/*----------------------------------------------------------------------*/
int usage ( char * progg, int level )
{
    char *prog = THD_trailname(progg,0) ;  /* 25 Jul 2006 - RWCox */
    if ( level == USE_SHORT )
    {
        fprintf( stderr,
                 "usage: %s [options] -prefix OUT_DSET -input IN_DSET\n"
                 "usage: %s -help\n",
                 prog, prog );
        return 0;
    }
    else if ( level == USE_LONG )
    {
        printf(
            "\n"
            "%s - reorient and/or resample a dataset\n"
            "\n"
            "    This program can be used to change the orientation of a\n"
            "    dataset (via the -orient option), or the dx,dy,dz\n"
            "    grid spacing (via the -dxyz option), or change them\n"
            "    both to match that of a master dataset (via the -master\n"
            "    option).\n"
            "\n"
            "    Note: if both -master and -dxyz are used, the dxyz values\n"
            "          will override those from the master dataset.\n"
            "\n"
            " ** It is important to note that once a dataset of a certain\n"
            "    grid is created (i.e. orientation, dxyz, field of view),\n"
            "    if other datasets are going to be resampled to match that\n"
            "    first one, then using -master should be used, instead of\n"
            "    -dxyz.  That will guarantee that all grids match.\n"
            "\n"
            "    Otherwise, even using both -orient and -dxyz, one may not\n"
            "    be sure that the fields of view will identical, for example.\n"
            "\n"
            " ** Warning: this program is not meant to transform datasets\n"
            "             between view types (such as '+orig' and '+tlrc').\n"
            "\n"
            "             For that purpose, please see '3dfractionize -help'\n"
            "             or 'adwarp -help'.\n"
            "\n"
            "------------------------------------------------------------\n"
            "\n"
            "  usage: %s [options] -prefix OUT_DSET -input IN_DSET\n"
            "\n"
            "  examples:\n"
            "\n"
            "    %s -orient asl -rmode NN -prefix asl.dset -input in+orig\n"
            "    %s -dxyz 1.0 1.0 0.9 -prefix 119.dset -input in+tlrc\n"
            "    %s -master master+orig -prefix new.dset -input old+orig\n"
            "\n"
            "  note:\n"
            "\n"
            "    Information about a dataset's voxel size and orientation\n"
            "    can be found in the output of program 3dinfo\n"
            "\n"
            "------------------------------------------------------------\n"
            "\n"
            "  options: \n"
            "\n"
            "    -help            : show this help information\n"
            "\n"
            "    -hist            : output the history of program changes\n"
            "\n"
            "    -debug LEVEL     : print debug info along the way\n"
            "          e.g.  -debug 1\n"
            "          default level is 0, max is 2\n"
            "\n"
            "    -version         : show version information\n"
            "\n"
            "    -bound_type TYPE : specify which boundary is preserved\n"
            "          e.g.  -bound_type SLAB\n"
            "          default is FOV (field of view)\n"
            "\n"
            "          The default and original use preserves the field of\n"
            "          of view when resampling, allowing the extents (SLABs)\n"
            "          to grow or shrink by half of the difference in the\n"
            "          dimension size (big voxels to small will cause the\n"
            "          extents to expand, for example, while small to big\n"
            "          will cause them to shrink).\n"
            "\n"
            "          Using -bound_type SLAB will have the opposite effect.\n"
            "          The extents should be unchanged, while the FOV will\n"
            "          grow or shrink in the opposite way as above).\n"
            "\n"
            "          Note that when using SLAB, edge voxels should be\n"
            "          mostly unaffected by the interpolation.\n"
            "\n"
            "    -dxyz DX DY DZ   : resample to new dx, dy and dz\n"
            "          e.g.  -dxyz 1.0 1.0 0.9\n"
            "          default is to leave unchanged\n"
            "\n"
            "          Each of DX,DY,DZ must be a positive real number,\n"
            "          and will be used for a voxel delta in the new\n"
            "          dataset (according to any new orientation).\n"
            "\n"
            "    -orient OR_CODE  : reorient to new axis order.\n"
            "          e.g.  -orient asl\n"
            "          default is to leave unchanged\n"
            "\n"
            "          The orientation code is a 3 character string,\n"
            "          where the characters come from the respective\n"
            "          sets {A,P}, {I,S}, {L,R}.\n"
            "\n"
            "          For example OR_CODE = LPI is the standard\n"
            "          'neuroscience' orientation, where the x-axis is\n"
            "          Left-to-Right, the y-axis is Posterior-to-Anterior,\n"
            "          and the z-axis is Inferior-to-Superior.\n"
            "\n"
            "    -rmode RESAM     : use this resampling method\n"
            "          e.g.  -rmode Linear\n"
            "          default is NN (nearest neighbor)\n"
            "\n"
            "          The resampling method string RESAM should come\n"
            "          from the set {'NN', 'Li', 'Cu', 'Bk'}.  These\n"
            "          are for 'Nearest Neighbor', 'Linear', 'Cubic'\n"
            "          and 'Blocky' interpolation, respectively.\n"
            "\n"
            "          For details, go to the 'Define Datamode' panel\n"
            "          of the afni GUI, click BHelp and then the\n"
            "          'ULay resam mode' menu.\n"
            "\n"
            "    -master MAST_DSET: align dataset grid to that of MAST_DSET\n"
            "          e.g.  -master master.dset+orig\n"
            "\n"
            "          Get dxyz and orient from a master dataset.  The\n"
            "          resulting grid will match that of the master.  This\n"
            "          option can be used with -dxyz, but not with -orient.\n"
            "\n"
            "    -prefix OUT_DSET : required prefix for output dataset\n"
            "          e.g.  -prefix reori.asl.pickle\n"
            "\n"
            "    -input IN_DSET   : required input dataset to reorient\n"
            "          e.g.  -input old.dset+orig\n"
            "\n"
            "    -inset IN_DSET   : alternative to -input\n"
            "------------------------------------------------------------\n"
            "\n"
            "  Author: R. Reynolds - %s\n"
            "\n",
            prog, prog, prog, prog, prog, VERSION );

        return 0;
    }
    else if ( level == USE_HISTORY )
    {
        fputs( g_history, stdout );
        return 0;
    }
    else if ( level == USE_VERSION )
    {
        printf( "%s %s, compile date: %s\n", prog, VERSION, __DATE__ );
        return 0;
    }

    fprintf( stderr, "usage called with illegal level <%d>\n", level );

    return FAIL;
}

/*----------------------------------------------------------------------*/
int write_results ( THD_3dim_dataset * dout, options_t * opts,
                    int argc, char * argv [] )
{
    /* set filename */
    EDIT_dset_items( dout, ADN_prefix, opts->prefix, ADN_none );

    /* don't worry about overwriting, that's AFNI_DECONFLICT's job */

    /* set number of time-axis slices to 0 */
    if( DSET_NUM_TTOFF(dout) > 0 )
        EDIT_dset_items( dout, ADN_nsl, 0, ADN_none );

    /* since we are writing data to disk, clear warp info */
    ZERO_IDCODE( dout->warp_parent_idcode );
    dout->warp_parent_name[0] = '\0';
    dout->warp = NULL;

    /* add to old history */
    tross_Copy_History( opts->dset , dout );
    tross_Make_History( "3dresample", argc, argv, dout );

    /* write the output files */
    if ( DSET_write( dout ) != True )
    {
        fputs( "failure: cannot write dataset, exiting...\n", stderr );
        return 1;
    }

    if ( opts->debug >= RL_DEBUG_LOW )
    {
        printf( "dset <%s> has been written to disk\n", opts->prefix );

        if ( opts->debug >= RL_DEBUG_HIGH )
        {
            r_idisp_thd_3dim_dataset( "final dset  : ", dout );
            r_idisp_thd_dataxes     ( "final daxes : ", dout->daxes );
            r_idisp_thd_datablock   ( "final dblk  : ", dout->dblk  );
            if ( dout->dblk )
                r_idisp_thd_diskptr ( "final diskp : ", dout->dblk->diskptr );
        }
    }

    return 0;
}


/*----------------------------------------------------------------------*/
int sync_master_opts ( options_t * opts )
{
    THD_dataxes * dax;

    if ( !opts->mset )
        return 0;       /* OK */

    if ( ! ISVALID_DSET(opts->mset) ||
         ! ISVALID_DATAXES(opts->mset->daxes ) )
    {
        fputs( "error: master dset or daxes not valid, exiting...\n", stderr );
        return FAIL;                    /* non-NULL but invalid is bad */
    }

    /* allow dxyz override of master data           03 Aug 2005 [rickr] */
    if ( opts->orient != NULL )
    {
        fputs( "error: -orient is not valid with -master option, exiting...\n",
                stderr );
        return FAIL;
    }

    /* all is okay, so fill dxyz and orientation code from master */
    dax = opts->mset->daxes;

    if ( opts->debug >= RL_DEBUG_LOW )
    {
        if (opts->dx == 0.0) fprintf(stderr,"-d using dxyz from master\n");
        else                 fprintf(stderr,"-d overriding dxyz from master\n");
    }

    if ( opts->dx == 0.0 ) /* then get the values from the master */
    {
        opts->dx = fabs(dax->xxdel);
        opts->dy = fabs(dax->yydel);
        opts->dz = fabs(dax->zzdel);
    }

    if ( opts->debug >= RL_DEBUG_LOW )
    {
        if (!opts->orient) fprintf(stderr,"-d using orient from master\n");
        else               fprintf(stderr,"-d overriding orient from master\n");
    }

    if ( opts->orient == NULL ) /* then get values from the master */
    {
        /* make space for orient string */
        if ( (opts->orient = (char *)malloc(4 * sizeof(char)) ) == NULL )
        {
            fputs( "failure: malloc failure for orient, exiting...\n", stderr );
            return FAIL;
        }

        opts->orient[0] = ORIENT_typestr[dax->xxorient][0];
        opts->orient[1] = ORIENT_typestr[dax->yyorient][0];
        opts->orient[2] = ORIENT_typestr[dax->zzorient][0];
        opts->orient[3] = '\0';
    }

    if ( opts->debug >= RL_DEBUG_LOW )
    {
        disp_opts_data( "++ mastered options : ", opts );

        if ( opts->debug >= RL_DEBUG_HIGH )
        {
            r_idisp_thd_3dim_dataset("sync mset : ", opts->mset );
            r_idisp_thd_dataxes     ("sync mset : ", opts->mset->daxes );
            r_idisp_thd_datablock   ("sync mset : ", opts->mset->dblk  );
            if ( opts->mset->dblk )
                r_idisp_thd_diskptr ("sync mset : ", opts->mset->dblk->diskptr);
        }
    }

    return 0;
}

/*----------------------------------------------------------------------*/
int disp_opts_data ( char * info, options_t * opts )
{
    if ( info )
        fputs( info, stdout );

    if ( opts == NULL )
    {
        printf( "disp_opts_data: opts == NULL\n" );
        return FAIL;
    }

    printf( "options struct at %p :\n"
            "    dset        = %p (%s)\n"
            "    mset        = %p (%s)\n"
            "    (dx,dy,dz)  = (%6.3f, %6.3f, %6.3f)\n"
            "    orient      = %.6s\n"
            "    prefix      = %.60s\n"
            "    resam       = %d\n"
            "    debug       = %d\n",
            opts,
            opts->dset, ISVALID_DSET(opts->dset) ? "valid" : "invalid",
            opts->mset, ISVALID_DSET(opts->mset) ? "valid" : "invalid",
            opts->dx, opts->dy, opts->dz,
            CHECK_NULL_STR(opts->orient), CHECK_NULL_STR(opts->prefix),
            opts->resam, opts->debug );

    return 0;
}

