
static char g_history[] =
    "----------------------------------------------------------------------\n"
    " history:\n"
    "\n"
    " 1.0  Jul  5, 2005 [rickr] - initial release\n"
    " 1.1  Jul 13, 2005 [rickr] - process run of fewer than 3 slices\n"
    " 1.2  Jul 22, 2005 [rickr] - use IOCHAN_CLOSENOW() in realtime.c\n"
    " 1.3  Jul 25, 2005 [rickr] - force tcp close for multiple term signals\n"
    "----------------------------------------------------------------------\n";

#define DIMON_VERSION "version 1.3 (July 25, 2005)"

/*----------------------------------------------------------------------
 * todo:
 *
 *    - without -rt_cmd 'PREFIX ...', set from last dir in glob (data/time?)
 *    - if there is a change in valid volume size, restart
 *    - setup an infile sorting step that will organize the files
 *      before running
 * ok - re-write help
 * ok - add -infile_prefix for no wildcards (and no quotes)
 * ok - find memory leak in dicom reader
 * ok - make this work for a single volume (e.g. anatomical)
 * ok - process run of single-slice volumes
 * ok - process run of fewer than 3 slices
 * ok - remove tabs
 *----------------------------------------------------------------------
*/

/*----------------------------------------------------------------------
 * Dimon - monitor real-time aquisition of Dicom or I-files
 *
 *     This program is intended to be run during a scanning session
 *     on a GE scanner, to monitor the collection of 2D image files.
 *     The user will be notified of any missing slice or any slice that
 *     is aquired out of order.
 *
 *     It is recommended that the user runs 'Dimon' when scanning
 *     begins, and then watches for error messages during the
 *     scanning session.  The user should terminate the program
 *     whey they are done with all runs.
 *
 *     - When acquiring GEMS 5.x I-files, users should run Dimon
 *       once for the entire scanning session.
 *
 *     - When acquiring DICOM image files, users should run Dimon
 *       once per run.
 *
 *     At the present time, the user must use <ctrl-c> to terminate
 *     the program.
 *
 *   usage: Dimon [options] -infile_pattern input_file_format
 *
 *   examples:    Dimon -infile_pattern 's12345/i*'
 *                Dimon -help
 *                Dimon -version
 *                Dimon -infile_prefix 's12345/i' -rt -host pickle -quit
 *----------------------------------------------------------------------
*/

#include <stdio.h>
#include <ctype.h>
#include <dirent.h>
#include <errno.h>
#include <math.h>
#include <signal.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>

#define MAIN
#define IFM_PROG_NAME   "Dimon"

#include "Imon.h"
#include "l_mcw_glob.h"
#include "thd_iochan.h"
#include "realtime.h"
#include "mri_image.h"
#include "dbtrace.h"

extern char  DI_MRL_orients[8];
extern float DI_MRL_tr;

extern struct dimon_stuff_t { int study, series, image; } gr_dimon_stuff;

extern MRI_IMAGE * r_mri_read_dicom( char *fname, int debug, void ** data );
static int read_dicom_image( char * pathname, finfo_t * fp );

/*----------------------------------------------------------------------*/
/* static function declarations */

static int add_to_string_list  ( string_list * list, char * str );
static int alloc_x_im          ( im_store_t * is, int bytes );
static int check_error         ( int * retry, float tr, char * note );
static int check_im_byte_order ( int * order, vol_t * v, param_t * p );
static int check_im_store_space( im_store_t * is, int num_images );
static int check_stalled_run   ( int run, int seq_num, int naps, int nap_time );
static int complete_orients_str( vol_t * v, param_t * p );
static int create_gert_script  ( stats_t * s, opts_t * opts );
static int dir_expansion_form  ( char * sin, char ** sexp );
static int disp_ftype          ( char * info, int ftype );
static int empty_string_list   ( string_list * list, int free_mem );
static int find_first_volume   ( vol_t * v, param_t * p, ART_comm * ac );
static int find_fl_file_index  ( param_t * p );
static int find_more_volumes   ( vol_t * v, param_t * p, ART_comm * ac );
static int find_next_zoff      ( param_t * p, int start, float zoff );
static int init_extras         ( param_t * p, ART_comm * ac );
static int init_options        ( param_t * p, ART_comm * a, int argc,
                                 char * argv[] );
static int nap_time_from_tr    ( float tr );
static int path_to_dir_n_suffix( char * dir, char * suff, char * path );
static int read_ge_files       ( param_t * p, int start, int max );
static int read_ge_image       ( char * pathname, finfo_t * fp,
                                 int get_image, int need_memory );
static int scan_ge_files       ( param_t * p, int next, int nfiles );
static int set_nice_level      ( int level );
static int set_volume_stats    ( param_t * p, stats_t * s, vol_t * v );
static int show_run_stats      ( stats_t * s );
static int str_char_count      ( char * str, int len, char target );
static int swap_4              ( void * ptr );

static void hf_signal          ( int signum );

/* volume scanning */
int        check_one_volume    (param_t *p, int start, int *fl_start, int bound,
                                int state, int * r_first, int * r_last,
                                float * r_delta);
static int volume_match  ( vol_t * vin, vol_t * vout, param_t * p, int start );
static int volume_search ( vol_t * V, param_t * p, int start, int maxsl,
                           int * fl_start, int * state );

/* information functions */
static int idisp_opts_t         ( char * info, opts_t * opt );
static int idisp_param_t        ( char * info, param_t * p );
static int idisp_vol_t          ( char * info, vol_t * v );
static int idisp_ge_extras      ( char * info, ge_extras * E );
static int idisp_ge_header_info ( char * info, ge_header_info * I );
static int idisp_im_store_t     ( char * info, im_store_t * is );

static int usage                ( char * prog, int level );

/* local copy of AFNI function */
unsigned long l_THD_filesize( char * pathname );

/*----------------------------------------------------------------------*/

/***********************************************************************/
/* globals */

IFM_debug gD;           /* debug information         */
param_t   gP;           /* main parameter struct     */
stats_t   gS;           /* general run information   */
ART_comm  gAC;          /* afni communication struct */

/***********************************************************************/
int main( int argc, char * argv[] )
{
    ART_comm * ac = &gAC;               /* access AFNI comm as local   */
    param_t  * p  = &gP;                /* access the global as local  */
    vol_t      baseV;                   /* base volume - first scanned */
    int        ret_val;

    /* validate inputs and init options structure */
    if ( (ret_val = init_options( p, ac, argc, argv )) != 0 )
        return ret_val;

    if ( (ret_val = init_extras( p, ac )) != 0 )
        return ret_val;

    if ( (ret_val = find_first_volume( &baseV, p, ac )) != 0 )
        return ret_val;

    if ( (ret_val = find_more_volumes( &baseV, p, ac )) != 0 )
        return ret_val;

    return 0;
}
/***********************************************************************/

/*----------------------------------------------------------------------
 * find_first_volume:   scan p->flist for a complete volume
 *
 * This function runs until either a volume is located, or an
 * error occurs.
 * 
 * return:     0 : on success
 *          else : error
 *----------------------------------------------------------------------
*/
static int find_first_volume( vol_t * v, param_t * p, ART_comm * ac )
{
    int max_im_alloc = IFM_MAX_IM_ALLOC;
    int ret_val;
    int sleep_secs = -1; /* has not been set from data yet */
    int vs_state = 0;    /* state for volume search, can reset */
    int fl_start = 0;    /* starting offset into the current flist */

    if ( gD.level > 0 )                 /* status */
        fprintf( stderr, "-- scanning for first volume\n" );

    ret_val = 0;
    while ( ret_val == 0 )
    {
        ret_val = read_ge_files( p, fl_start, max_im_alloc );

        if ( ret_val > 0 )
        {
            ret_val = volume_search( v, p, 0, 0, &fl_start, &vs_state );

            /* try to recover from a data error */
            if ( ret_val == -1 ) ret_val = 0;

            /* If we don't have a volume yet, but have used "too much" of our
             * available memory, request more, making sure there is enough for
             * a volume, despite the previous max_im_alloc limitation.  */
            if ( (ret_val == 0) && (p->nused > (max_im_alloc / 2)) )
                max_im_alloc *= 2;
        }

        if ( ret_val == 0 )                     /* we are not done yet */
        {
            if ( gD.level > 0 ) fprintf( stderr, "." );   /* status    */

            /* try to update nap time */
            if( sleep_secs < 0 && p->nused > 0 )
                sleep_secs = nap_time_from_tr(p->flist->geh.tr);

            if( sleep_secs < 0 ) sleep( 4 );              /* nap time! */
            else                 sleep(sleep_secs);
        }
        else if ( ret_val > 0 )         /* success - we have a volume! */
        {
            if ( gD.level > 0 )
            {
                fprintf( stderr, "\n-- first volume found\n" );
                if ( gD.level > 1 )
                {
                    idisp_vol_t( "+d first volume : ", v );
                    idisp_param_t( "-d first vol - new params : ", p );
                    disp_ftype("-d ftype: ", p->ftype);
                }
            }

            /* make sure there is enough memory for bad volumes */
            if ( p->nalloc < (4 * v->nim) )
            {
                p->nalloc = 4 * v->nim;
                p->flist = (finfo_t *)realloc( p->flist,
                                               p->nalloc*sizeof(finfo_t) );
                if ( p->flist == NULL )
                {
                    fprintf( stderr, "** FFV: failure to allocate %d finfo_t "
                                     "structs!\n", p->nalloc );
                    return -1;
                }

                if ( gD.level > 1 )
                    idisp_param_t( "++ final realloc of flist : ", p );
            }

            /* use this volume to complete the geh.orients string */
            if ( complete_orients_str( v, p ) < 0 )
                return -1;

            /* use this volume to note the byte order of image data */
            if ( check_im_byte_order( &ac->byte_order, v, p ) < 0 )
                return -1;

            /* if wanted, verify afni link, send image info and first volume */
            if ( ac->state == ART_STATE_TO_OPEN )
                ART_open_afni_link( ac, 5, 0, gD.level );

            if ( ac->state == ART_STATE_TO_SEND_CTRL )
                ART_send_control_info( ac, v, gD.level );

            if ( ac->state == ART_STATE_IN_USE )
                ART_send_volume( ac, v, gD.level );

            if ( gD.level > 1 )
            {
                ART_idisp_ART_comm( "-- first vol ", ac );
                idisp_im_store_t( "-- first vol ", &p->im_store );
            }
        }
        else
            return ret_val;             /* fatal error condition */
    }

    if ( ret_val > 0 )
        return 0;
    else
        return ret_val;
}


/*----------------------------------------------------------------------
 * find_more_volumes:   given first volume, keep scanning for others
 *
 * This function runs until a fatal error occurs.
 * 
 * return:     0 : on success
 *          else : error
 *----------------------------------------------------------------------
*/
static int find_more_volumes( vol_t * v0, param_t * p, ART_comm * ac )
{
    vol_t vn;
    int   ret_val, done;
    int   run, seq_num, next_im;
    int   fl_index;                     /* current index into p->flist    */
    int   naps;                         /* keep track of consecutive naps */
    int   nap_time;                     /* sleep time, in seconds         */

    if ( v0 == NULL || p == NULL )
    {
        fprintf( stderr, "error: IFM:FMV() lacking parameters\n" );
        return -1;
    }

    done     = 0;
    naps     = 0;

    run      = v0->run;
    seq_num  = v0->seq_num = 1;         /* set first seq_num to 1 */
    fl_index = v0->fn_n + 1;            /* start looking past first volume */
    next_im  = v0->fn_n + 1;            /* for read_ge_files()             */

    nap_time = nap_time_from_tr( v0->geh.tr );

    if ( gD.level > 0 )                 /* status */
    {
        fprintf( stderr, "-- scanning for additional volumes...\n" );
        fprintf( stderr, "-- run %d: %d ", run, seq_num );
    }

    /* give stats when user quits */
    signal( SIGHUP,  hf_signal );
    signal( SIGINT,  hf_signal );
    signal( SIGQUIT,  hf_signal );
    signal( SIGTRAP,  hf_signal );
    signal( SIGABRT,  hf_signal );
    signal( SIGTERM, hf_signal );
    signal( SIGSEGV,  hf_signal );

    if ( set_volume_stats( p, &gS, v0 ) )
        return -1;

    while ( ! done )
    {
        /* check all of the volumes we've already scanned in */
        ret_val = 1;
        while ( (ret_val == 1) || (ret_val == -1) )
        {
            ret_val = volume_match( v0, &vn, p, fl_index );

            if ( ret_val < -1 )                 /* bail out on fatal error */
                return ret_val;

            if ( (ret_val == 1) || (ret_val == -1) )
            {
                if ( gD.level > 2 )
                    idisp_vol_t( "-- new volume: ", &vn );

                fl_index += vn.nim;             /* note the new position   */
                next_im   = vn.fn_n + 1;        /* for read_ge_files()     */

                if ( vn.run != run )            /* new run?                */
                {
                    /* pass run and seq_num before they are updated */
                    if ( ac->state == ART_STATE_IN_USE )
                        ART_send_end_of_run( ac, run, seq_num, gD.level );

                    run = vn.run;               /* reset                   */
                    seq_num = 1;

                    if ( gD.level > 0 )
                        fprintf( stderr, "\n-- run %d: %d ", run, seq_num );
                }
                else
                {
                    seq_num++;

                    if ( gD.level > 0 )
                        fprintf( stderr, "%d ", seq_num );
                }

                vn.seq_num = seq_num;

                if ( set_volume_stats( p, &gS, &vn ) )
                    return -1;

                if ( complete_orients_str( &vn, p ) < 0 )
                    return -1;

                if ( ac->state == ART_STATE_TO_SEND_CTRL )
                    ART_send_control_info( ac, &vn, gD.level );

                /* only send good volumes to afni   - 2003.03.10 */
                if ( (ac->state == ART_STATE_IN_USE) && (ret_val == 1) )
                    ART_send_volume( ac, &vn, gD.level );

                naps = 0;                       /* reset on existing volume */

                if( p->opts.pause > 0 ) iochan_sleep(p->opts.pause);
            }
        }

        /* now we need new data - skip past last file name index */

        ret_val = read_ge_files( p, next_im, p->nalloc );
        fl_index = 0;                   /* reset flist index          */

        while ( (ret_val >= 0 ) &&      /* no fatal error, and        */
                (ret_val < v0->nim) )   /* didn't see full volume yet */
        {
            if ( naps > 0 )
            {
                if ( p->opts.quit )     /* then we are outta here */
                {
                    if ( ac->state == ART_STATE_IN_USE )
                        ART_send_end_of_run( ac, run, seq_num, gD.level );

                    show_run_stats( &gS );
                    return 0;
                }

                /* continue, regardless */
                if ( check_stalled_run( run, seq_num, naps, nap_time ) > 0 )
                    if ( ac->state == ART_STATE_IN_USE )
                        ART_send_end_of_run( ac, run, seq_num, gD.level );

                if ( gD.level > 0 )     /* status */
                    fprintf( stderr, ". " );
            }

            sleep( nap_time );          /* wake after a couple of TRs */
            naps ++;

            ret_val = read_ge_files( p, next_im, p->nalloc );
        }

        if ( ret_val < 0 )              /* aaaaagh!  panic!  wet pants! */
        {
            fprintf( stderr, "\n** failure: IFM:RGF fatal error\n" );
            return -1;
        }
    }

    return 0;   /* success */
}


/*----------------------------------------------------------------------
 * volume_search:   scan p->flist for a complete volume
 *
 *     - start should be at the expected beginning of a volume!
 *     - *fl_start may be returned as a new starting index into fnames
 * 
 * state:     0 :  < 2 slices found (in the past)
 *            1 : >= 2 slices (new 'bound')
 *            2 : >= 2 slices (repeated 'bound', may be one volume run)
 * 
 * return:   -2 : on programming error
 *           -1 : on data error
 *            0 : on success - no volume yet
 *            1 : on success - volume read
 *----------------------------------------------------------------------
*/
static int volume_search(
        vol_t   * V,                    /* volume to fill                 */
        param_t * p,                    /* master structure               */
        int       start,                /* starting index into p->flist   */
        int       maxsl,                /* max number of slices to check  */
        int     * fl_start,             /* return new fnames search point */
        int     * state )               /* assumed state of search        */
{
    float      delta;
    int        bound;                   /* upper bound on image slice  */
    static int prev_bound = -1;         /* note previous 'bound' value */
    int        first = start;           /* first image (start or s+1)  */
    int        last;                    /* final image in volume       */
    int        rv;

    if ( V == NULL || p == NULL || p->flist == NULL || start < 0 )
    {
        fprintf( stderr, "failure: FNV: bad parameter data\n" );
        return -2;
    }

    /* note the bound on the slice index */
    if ( (maxsl <= 0) || ((maxsl + first) >= p->nused) )
        bound = p->nused;
    else
        bound = first + maxsl;

    if ( ( bound-first < 1) ||      /* from 3              8 Jul 2005 */
         ((bound-first < 4) && !p->opts.use_dicom) )
        return 0;                   /* not enough data to work with   */

    /* maintain the state */
    if ( *state == 1 && bound == prev_bound ) *state = 2;  /* try to finish */
    else                                      *state = 1;  /* continue mode */
    prev_bound = bound;

    rv = check_one_volume(p,start,fl_start,bound,*state, &first,&last,&delta);

    if ( rv == 1 )
    {
        /* One volume exists from slice 'first' to slice 'last'. */

        V->geh      = p->flist[first].geh;         /* copy GE structure  */
        V->gex      = p->flist[first].gex;         /* copy GE extras     */
        V->nim      = last - first + 1;
        V->fl_1     = first;
        V->fn_1     = p->flist[first].index;
        V->fn_n     = p->flist[last].index;
        strncpy( V->first_file, p->fnames[V->fn_1], IFM_MAX_FLEN );
        strncpy( V->last_file,  p->fnames[V->fn_n], IFM_MAX_FLEN );
        V->z_first  = p->flist[first].geh.zoff;
        V->z_last   = p->flist[last].geh.zoff;
        V->z_delta  = delta;
        V->seq_num  = -1;                               /* uninitialized */
        V->run      = V->geh.uv17;

        return 1;
    }
    else if ( rv == 0 )
        return 0;                           /* we did not finish a volume */
    else if ( rv == -1 )
    {
        /* We have gone in the wrong direction.  This means that the
         * starting slice was not the first in the volume.  Try restarting
         * from the current position.  */
        fprintf( stderr, "\n"
                "*************************************************\n"
                "Error: missing slice(s) in first volume!\n"
                "       attempting to re-start at file: %s\n"
                "*************************************************\n",
                p->fnames[p->flist[last+1].index] );
        *fl_start = p->flist[last+1].index;
    }
    else /* ( rv == -2 ) : right direction, but bad delta */
    {
        /* the next slice does not match the original - interleaving? */
        int testc;
        for ( testc = last; testc < bound; testc++ )
            if ( abs( p->flist[first].geh.zoff -
                      p->flist[testc].geh.zoff ) < IFM_EPSILON )
            {
                /* aaaaagh!  we are missing data from the first volume!   */
                /* print error, and try to skip this volume               */
                fprintf( stderr, "\n"
                        "*************************************************\n"
                        "Error: missing slice in first volume!\n"
                        "       detected    at file: %s\n"
                        "       re-starting at file: %s\n"
                        "*************************************************\n",
                        p->fnames[p->flist[last+1].index],
                        p->fnames[p->flist[testc].index] );

                /* try to skip this volume and recover */
                *fl_start = p->flist[testc].index;

                return -1;
            }

        /* we didn't find the original zoff, wait for more files */
        return 0;
    }

    return -1;  /* should not reach here */
}

/*----------------------------------------------------------------------
 * check_one_volume:   scan p->flist for an entire volume
 *
 * upon success, fill r_first, r_last and r_delta
 *
 * return:   -2 : general error
 *           -1 : found direction change
 *            0 : no error, no volume yet
 *            1 : found volume
 *----------------------------------------------------------------------
*/
int check_one_volume(param_t *p, int start, int *fl_start, int bound, int state,
                     int * r_first, int * r_last, float * r_delta)
{
    finfo_t * fp;
    float     delta, z_orig, prev_z, dz;
    int       run0, run1, first, next, last;

    if( bound <= start )
    {
        fprintf(stderr,"error: COV: bad bound, start (%d,%d)\n", bound, start);
        return -2;
    }

    /* if state is 2 and we have only 1 or 2 images, return quickly */
    if( state == 2 && (bound-start) < 3 )
    {
        if( gD.level > 1 )
            fprintf(stderr,"-d stall after only %d slices\n", bound-start);
        *r_first = start;
        if( (bound-start) == 1 ) /* then only one slice */
        {
            *r_last = start;
            *r_delta = 1.0;  /* doesn't really matter, but 0 may be bad */
        }
        else /* 2 slices, so 1 volume of 2 slices or 2 volumes of 1 slice */
        {
            delta = p->flist[start+1].geh.zoff - p->flist[start].geh.zoff;
            if ( fabs(delta) < IFM_EPSILON )  /* one slice per volume */
            {
                *r_last = start;
                *r_delta = 1.0;
            }
            else                              /* two slices per volume */
            {
                *r_last = start+1;
                *r_delta = delta;
            }
        }
        return 1;  /* done */
    }
    else if ( bound-start < 3 )
        return 0;

    first = start;
    delta = p->flist[first+1].geh.zoff - p->flist[first].geh.zoff;

    run0  = p->flist[first  ].geh.uv17;
    run1  = p->flist[first+1].geh.uv17;

    /* if apparent 1-slice volume, skip and start over */
    if ( (fabs(delta) < IFM_EPSILON) || (run1 != run0) )
    {
        /* consider this a single slice volume */
        if ( p->opts.use_dicom )
        {
            if( gD.level > 1 ) fprintf(stderr,"+d found single slice volume\n");
            *r_first = *r_last = first;
            *r_delta = 1.0;   /* make one up, zero may be bad */
            return 1;         /* success */
        }

        if ( gD.level > 1 )
            fprintf( stderr, "-- skipping single slice volume <%s>\n",
                     p->fnames[p->flist[first].index] );
        first++;
        delta = p->flist[first+1].geh.zoff - p->flist[first].geh.zoff;
        run0  = run1;

        if ( fabs(delta) < IFM_EPSILON )
        {
            fprintf( stderr, "Error: 3 slices with 0 delta, beginning with"
                     "file <%s>\n", p->fnames[p->flist[start].index] );
            *fl_start = p->flist[start+2].index;
            return -1;
        }
    }

    fp = p->flist + first;                      /* initialize flist posn  */
    z_orig = fp->geh.zoff;                      /* note original position */

    /* set current values at position (first+1) */
    fp++;
    prev_z = fp->geh.zoff;
    run1   = fp->geh.uv17;
    dz     = delta;

    /* scan for volume break */
    next = first + 2;                           /* next z to look at      */
    while ( (next < bound) && (fabs(dz - delta) < IFM_EPSILON) &&
            (run1 == run0) )
    {
        fp++;                             /* good index so get new values */

        dz     = fp->geh.zoff - prev_z;
        run1   = fp->geh.uv17;
        prev_z = fp->geh.zoff;

        next++;
    }

    /* note final image in current volume -                        */
    /* if we left the current volume, next is too far by 2, else 1 */
    if ( (fabs(dz - delta) > IFM_EPSILON) || (run1 != run0) ) last = next - 2;
    else                                                      last = next - 1;

    /* set return values */
    *r_first = first;
    *r_last  = last;
    *r_delta = delta;

    if( gD.level > 1 )
        fprintf(stderr,"+d cov: returning first, last, delta = %d, %d, %f\n",
                first, last, delta);

    /* If we have found the same slice location, we are done. */
    if ( fabs(fp->geh.zoff - p->flist[first].geh.zoff) < IFM_EPSILON )
    {
        if ( gD.level > 1 )
            fprintf(stderr,"+d found first slice of second volume\n");
        return 1;  /* success */
    }

    /* Also, if we are still waiting for the same location, but are in
       state 2, then we seem to have only a single volume to read. */
    if ( ( state == 2 && fabs(dz-delta)<IFM_EPSILON) && run1 == run0 )
    {
        if ( gD.level > 1 )
            fprintf(stderr,"+d no new data after finding sufficient slices\n"
                           "   --> assuming completed single volume\n");
        return 1;
    }
    /* otherwise, if we have not changed the delta or run, continue */
    if ( (fabs(dz - delta) < IFM_EPSILON) && (run1 == run0) ) /* not state 2 */
        return 0;  /* not done yet */
    if ( dz * delta < 0.0 ) return -1;   /* wrong direction */

    /* all other cases, until we hear of a new one to watch for */
    return -2;
}


/*----------------------------------------------------------------------
 * volume_match:   scan p->flist for a matching volume
 *
 *     - start should be at the expected beginning of a volume!
 * 
 * return:   -2 : fatal error
 *           -1 : recoverable data error
 *            0 : on success - no volume yet
 *            1 : on success - volume read
 *----------------------------------------------------------------------
*/
static int volume_match( vol_t * vin, vol_t * vout, param_t * p, int start )
{
    static int   retry = 1;                             /* v2.12 */
    finfo_t    * fp;
    finfo_t    * fp_test;
    float        z;
    int          count, next_start = -1;
    int          missing = 0;

    if ( vin == NULL || vout == NULL ||
         p == NULL || p->flist == NULL || start < 0 )
    {
        fprintf( stderr, "failure: FMV: bad parameter data\n" );
        return -2;
    }

    if ( (p->nused - start) < vin->nim )        /* enough files to scan? */
        return 0;

    /* now 'everything' should match */

    fp = p->flist+start;
    for ( count = 0; count < vin->nim - 1; count++ )   /* last is separate */
    {
        z = vin->z_first + count * vin->z_delta;        /* note expected z */

        if ( fabs( z - fp->geh.zoff ) > IFM_EPSILON )
        {
            /* slice is either missing or out of order */

            fp_test = fp + 1;                          /* check next image */
            if ( fabs( z + vin->z_delta - fp_test->geh.zoff ) < IFM_EPSILON )
            {
                /* report the error?                v2.12 */
                if ( !check_error(&retry, vin->geh.tr, "slice out of order") )
                    return 0;

                /* next slice as expected, so current is out of order */
                /* nothing to do but warn the user and continue */

                IFM_BIG_ERROR_MESG( "slice out of order!",
                        p->fnames[fp->index], z, fp->geh.zoff,
                        fp->geh.uv17, count + 1, vin->nim );
            }
            else if ( fabs(z + vin->z_delta - fp->geh.zoff) < IFM_EPSILON )
            {
                /* current slice matches next expected - slice missing */

                /* report the error? */
                if ( !check_error(&retry, vin->geh.tr, "slice missing") )
                    return 0;

                /* nothing to do but note error, warn user and continue */
                missing++;

                IFM_BIG_ERROR_MESG( "slice missing!",
                        p->fnames[fp->index], z, fp->geh.zoff,
                        fp->geh.uv17, count + 1, vin->nim );

                count++;    /* attempt to continue by skipping this slice */
            }
            else        /* unknown error - find start of next volume */
            {
                /* search for a next starting point */
                next_start = find_next_zoff( p, start+count, vin->z_first );

                if ( next_start < 0 )   /* come back and try again later */
                    return 0;
                else
                {
                    /* report error? */
                    if ( !check_error(&retry, vin->geh.tr, "vol toasted") )
                        return 0;

                    IFM_BIG_ERROR_MESG( "volume severely toasted!",
                            p->fnames[fp->index], z, fp->geh.zoff,
                            fp->geh.uv17, count + 1, vin->nim );

                    break;      /* terminate for loop and try to recover */
                }
            }
        }

        fp++;
    }

    z = vin->z_first + count * vin->z_delta;      /* note expected z   */

    if ( count >= vin->nim )    /* missed second to last slice */
        next_start = start + vin->nim - missing;
    else if ( (next_start < 0) && (fabs( z - fp->geh.zoff ) > IFM_EPSILON) )
    {
        /* check last slice - count and fp should be okay*/
        if ( (p->nused - start) <= vin->nim )   /* no more images to check */
            return 0;                           /* wait for more data      */
        
        fp_test = fp + 1;                              /* check next image */
        if ( fabs( vin->z_first - fp_test->geh.zoff ) < IFM_EPSILON )
        {
            /* next image starts next run, slice is probably out of order */

            /* report error? */
            if ( !check_error(&retry, vin->geh.tr, "last slice out of order") )
                return 0;

            IFM_BIG_ERROR_MESG( "last slice out of order!",
                    p->fnames[fp->index], z, fp->geh.zoff,
                    fp->geh.uv17, count + 1, vin->nim );
        }
        else if ( fabs(vin->z_first - fp->geh.zoff) < IFM_EPSILON )
        {
            /* this image starts next run, slice is missing */

            /* report error? */
            if ( !check_error(&retry, vin->geh.tr, "last slice missing") )
                return 0;

            missing++;

            IFM_BIG_ERROR_MESG( "last slice missing!",
                    p->fnames[fp->index], z, fp->geh.zoff,
                    fp->geh.uv17, count + 1, vin->nim );
        }
        else    /* unknown error - find start of next volume */
        {
            /* search for a next starting point */
            next_start = find_next_zoff( p, start+count+1, vin->z_first );

            if ( next_start < 0 )       /* come back and try again later */
                return 0;
            else
            {
                /* report error? */
                if ( !check_error(&retry, vin->geh.tr, "Vol toasted") )
                    return 0;

                IFM_BIG_ERROR_MESG( "Volume severely toasted!",
                        p->fnames[fp->index], z, fp->geh.zoff,
                        fp->geh.uv17, count + 1, vin->nim );
            }
        }
    }

    if ( next_start < 0)
        next_start = start + vin->nim - missing;

    if ( retry == 0 && gD.level > 0 )                   /* v2.12 */
        fprintf(stderr," (retry OK - no errors)\n");

    retry = 1;                          /* next error gets two tries - v2.12 */

    /* fill volume structure */

    vout->geh      = p->flist[start].geh;
    vout->gex      = p->flist[start].gex;
    vout->nim      = next_start - start;
    vout->fl_1     = start;
    vout->fn_1     = p->flist[start].index;
    vout->fn_n     = p->flist[start+vout->nim-1].index;
    strncpy( vout->first_file, p->fnames[vout->fn_1], IFM_MAX_FLEN );
    strncpy( vout->last_file,  p->fnames[vout->fn_n],  IFM_MAX_FLEN );
    vout->z_first  = vin->z_first;
    vout->z_last   = vin->z_last;
    vout->z_delta  = vin->z_delta;
    vout->seq_num  = -1;                                /* uninitialized */
    vout->run      = vout->geh.uv17;

    if ( vout->nim != vin->nim )
        return -1;
    else
        return 1;
}

/*----------------------------------------------------------------------
 * check_error:         only report error on second failure     v2.12
 *
 * return:  < 0  : on programming error
 *            0  : do not report error yet
 *            1  : report error
 *----------------------------------------------------------------------
*/
static int check_error( int * retry, float tr, char * note )
{
    if ( !retry )
        return -1;

    if ( *retry == 1 )
    {
        /* let user know we're checking */
        if ( gD.level > 0 )
            fprintf(stderr," (volume retry test for warning '%s'...)\n",
                    CHECK_NULL_STR(note));

        *retry = 0;
        sleep( nap_time_from_tr(tr) );
        return 0;
    }

    /* so calling function should print error */

    *retry = 2;

    return 1;
}


/*----------------------------------------------------------------------
 * read_ge_files:
 *
 *     - sanity checks
 *     - read_directory (free any old file expansion list)
 *     - decide number of files to scan (max == 0 implies the rest)
 *     - allocate necessary p->flist memory
 *     - read GE structures
 * 
 * return:       < 0  : on error
 *         files read : on success
 *----------------------------------------------------------------------
*/
static int read_ge_files(
        param_t * p,            /* parameter scruct                */
        int       start,        /* index of next file to scan from */
        int       max )         /* max number of files to scan     */
{
    int n2scan;                 /* number of files to actually scan */
    int next = start;           /* initialize next index to start   */

    if ( p == NULL )
    {
        fputs( "failure: RAF: no param_t struct\n", stderr  );
        return -1;
    }

    /* clear away old file list */
    if ( p->fnames != NULL )
    {
        if ( p->nfiles <= 0 )
        {
            fputs( "failure: RAF: fnames does not match nfiles\n", stderr );
            return -1;
        }

        MCW_free_expand( p->nfiles, p->fnames );
        p->fnames = NULL;
    }

    /* get files (check for dicom) */
    if ( p->opts.use_dicom && p->opts.dicom_glob )
        MCW_file_expand( 1, &p->opts.dicom_glob, &p->nfiles, &p->fnames );
    else
        MCW_file_expand( 1, &p->glob_dir, &p->nfiles, &p->fnames );

    /* if next is 0, search for any first_file */
    if ( (next == 0 ) && (p->opts.start_file || p->opts.start_dir) )
    {
        next = find_fl_file_index( p );

        if ( next < 0 )         /* if not found, try again later */
        {
            if ( gD.level > 0 ) /* inform the user */
            {
                static int attempts = 0;

                if ( attempts == 0 )
                    fprintf(stderr, "-- still searching for start_file, '%s'\n",
                            p->opts.start_file );

                attempts++;
            }
            return 0;
        }
    }
    
    if ( gD.level > 4 )
    {
        int fnum;
        for ( fnum = next; fnum < p->nfiles; fnum++ )
            printf( "file %4d: %s\n", fnum, p->fnames[fnum] );
    }

    if ( p->nfiles <= 0 )
        return 0;

    /* set the number of files to scan - if max is usable, use it */
    if ( (max > 0) && (max <= (p->nfiles - next)) )
        n2scan = max;                           /* scan next max files */
    else
        n2scan = p->nfiles - next;              /* scan rest of files  */

    /* do we need/want more memory? */
    if ( (n2scan > p->nalloc) || (max > p->nalloc) )
    {
        int nalloc;

        /* allow a request to allocate 'max' entries */
        nalloc = (n2scan >= max) ? n2scan : max;

        p->flist = (finfo_t *)realloc( p->flist, nalloc * sizeof(finfo_t) );

        if ( p->flist == NULL )
        {
            fprintf(stderr, "failure to allocate %d finfo_t structs\n", nalloc);
            return -1;
        }

        p->nalloc = nalloc;

        if ( gD.level > 1 )
        {
            idisp_param_t( "++ realloc of flist : ", p );
            fprintf( stderr,  "-- n2scan = %d, max = %d\n", n2scan, max );
        }
    }

    p->nused = scan_ge_files( p, next, n2scan );

    if ( gD.level > 2 )
        idisp_param_t( "end read_ge_files : ", p );

    /* may be negative for an error condition */
    return p->nused;
}

/*----------------------------------------------------------------------
 * scan_ge_files:
 *
 * Starting at index 'next' of 'fnames', scan 'nfiles' files,
 * filling the 'flist' array.
 *----------------------------------------------------------------------
*/
static int scan_ge_files (
        param_t  * p,                   /* general parameter structure */
        int        next,                /* index of next file to scan  */
        int        nfiles )             /* number of files to scan     */
{
    finfo_t    * fp;
    int          im_num, fnum;
    int          files_read, rv = 0;
    int          need_M;                /* do we need image memory?    */

    if ( nfiles <= 0 )
        return 0;

    if ( check_im_store_space( &p->im_store, nfiles ) < 0 )
        return -1;

    p->im_store.nused = 0;
    /* scan from 'next' to 'next + nfiles - 1' */
    for ( im_num = 0, fnum = next, fp = p->flist;
          im_num < nfiles;
          im_num++, fnum++, fp++ )
    {
        /* do we need image memory? */
        if ( im_num < p->im_store.nalloc )
        {
            fp->image = p->im_store.im_ary[im_num];
            need_M    = 0;
        }
        else                                    /* get it from read_ge_image */
        {
            fp->image = NULL;
            need_M    = 1;
        }

        if ( p->opts.use_dicom )
            rv = read_dicom_image( p->fnames[fnum], fp );
        else 
            rv = read_ge_image( p->fnames[fnum], fp, 1, need_M );

        /* don't lose any allocated memory, regardless of the return value */
        if ( (need_M == 1) && (fp->image != NULL) )
        {
            p->im_store.im_ary[im_num] = fp->image;
            p->im_store.nalloc++;

            /* note the size of the image and get memory for x_im */
            if ( p->im_store.im_size == 0 )
            {
                if ( alloc_x_im( &p->im_store, fp->bytes ) < 0 )
                    return -1;
            }

            if ( gD.level > 1 )
                fprintf( stderr, "++ allocated image %d at address %p\n",
                         im_num, p->im_store.im_ary[im_num] );
        }

        if ( (rv != 0) || (fp->geh.good != 1) )
        {
            static int read_failure = -1;  /* last read_ge_image failure    */
            static int fail_count   =  0;  /* get multiple tries to succeed */

            /* on first failure, note file and set fail_count to 1 */
            if ( read_failure != fnum )
            {
                read_failure = fnum;
                fail_count   = 1;
            }
            else
                fail_count++;

            /* after too many failure, we give up */
            if ( fail_count > IFM_MAX_GE_FAILURES )
            {
                fprintf( stderr, "\n** failure: cannot read GE header for "
                         "file <%s>\n", p->fnames[fnum] );
                return -1;
            }

            /* we failed, but will try again later - maybe inform user */
            if ( gD.level > 1 )
                fprintf( stderr, "\n-- (%d) failures to read GE header for "
                         "file <%s>, trying again...\n",
                         fail_count, p->fnames[fnum] );

            break;
        }
        else
        {
            p->im_store.nused++;        /* keep track of used images     */
            fp->index = fnum;           /* store index into fnames array */

            if ( gD.level > 2 )
            {
                idisp_ge_header_info( p->fnames[fp->index], &fp->geh );
                idisp_ge_extras( p->fnames[fp->index], &fp->gex );
            }
        }
    }

    /* even on failure, this non-negative integer is accurate */
    files_read = fnum - next;

    if ( rv == 0 && gD.level > 1 )
        printf( "-- scanned %d image files, from <%s> to <%s>\n",
                files_read, p->fnames[next], p->fnames[next+files_read-1] );

    return files_read;
}


/*----------------------------------------------------------------------
 * init_options:
 *
 *     1. check usage: Imon -start_dir DIR
 *     2. do initial allocation of data structures
 *----------------------------------------------------------------------
*/
static int init_options( param_t * p, ART_comm * A, int argc, char * argv[] )
{
    int ac, errors = 0;

    if ( p == NULL )
        return 2;

    if ( argc < 2 )
    {
        usage( IFM_PROG_NAME, IFM_USE_SHORT );
        return 1;
    }

    /* basic initialization of data structures */

    memset(  p,  0, sizeof(*p)  );      /* parameters       */
    memset( &gD, 0, sizeof(gD)  );      /* debug struct     */
    memset( &gS, 0, sizeof(gS)  );      /* stats struct     */
    memset(  A,  0, sizeof(gAC) );      /* afni comm struct */

    ART_init_AC_struct( A );            /* init for no real-time comm */
    A->param = p;                       /* store the param_t pointer  */
    p->opts.use_dicom = 1;              /* will delete this later...  */

    empty_string_list( &p->opts.drive_list, 0 );
    empty_string_list( &p->opts.rt_list, 0 );

    /* debug level 1 is now the default - by order of Wen-Ming :) */
    gD.level = 1;

    for ( ac = 1; ac < argc; ac++ )
    {
        if ( ! strncmp( argv[ac], "-debug", 4 ) )
        {
            if ( ++ac >= argc )
            {
                fputs( "option usage: -debug LEVEL\n", stderr );
                usage( IFM_PROG_NAME, IFM_USE_SHORT );
                return 1;
            }

            p->opts.debug = atoi(argv[ac]);
            gD.level      = p->opts.debug;
            if ( gD.level < 0 || gD.level > IFM_MAX_DEBUG )
            {
                fprintf( stderr, "error: debug level must be in [0,%d]\n",
                         IFM_MAX_DEBUG );
                errors++;
            }
        }
        else if ( ! strncmp( argv[ac], "-GERT_Reco2", 7 ) )
        {
            p->opts.gert_reco = 1;      /* output script at the end */
        }
        else if ( ! strncmp( argv[ac], "-help", 5 ) )
        {
            usage( IFM_PROG_NAME, IFM_USE_LONG );
            return 1;
        }
        else if ( ! strncmp( argv[ac], "-hist", 5 ) )
        {
            usage( IFM_PROG_NAME, IFM_USE_HIST );
            return 1;
        }
        else if ( ! strncmp( argv[ac], "-infile_pattern", 11 ) ||
                  ! strncmp( argv[ac], "-dicom_glob", 9 ) )
        {
            if ( ++ac >= argc )
            {
                fputs( "option usage: -infile_pattern FILE_PATTERN\n", stderr );
                usage( IFM_PROG_NAME, IFM_USE_SHORT );
                return 1;
            }

            p->opts.dicom_glob = argv[ac];
        }
        else if ( ! strncmp( argv[ac], "-infile_prefix", 11 ) )
        {
            if ( ++ac >= argc )
            {
                fputs( "option usage: -infile_prefix FILE_PREFIX\n", stderr );
                usage( IFM_PROG_NAME, IFM_USE_SHORT );
                return 1;
            }
            /* just append a '*' to the PREFIX */
            p->opts.dicom_glob = calloc(strlen(argv[ac])+2, sizeof(char));
            strcpy(p->opts.dicom_glob, argv[ac]);
            strcat(p->opts.dicom_glob, "*");
        }
        else if ( ! strncmp( argv[ac], "-nice", 4 ) )
        {
            if ( ++ac >= argc )
            {
                fputs( "option usage: -nice INCREMENT\n", stderr );
                usage( IFM_PROG_NAME, IFM_USE_SHORT );
                return 1;
            }

            p->opts.nice = atoi(argv[ac]);
            if ( (p->opts.nice < IFM_MIN_NICE_INC) ||
                 (p->opts.nice > IFM_MAX_NICE_INC) )
            {
                fprintf( stderr, "error: nice incrment must be in [%d,%d]\n",
                         IFM_MIN_NICE_INC, IFM_MAX_NICE_INC );
                errors++;
            }
        }
        else if ( ! strncmp( argv[ac], "-nt", 3 ) )
        {
            if ( ++ac >= argc )
            {
                fputs( "option usage: -nt VOLUMES_PER_RUN\n", stderr );
                usage( IFM_PROG_NAME, IFM_USE_SHORT );
                return 1;
            }

            p->opts.nt = atoi(argv[ac]);
            if ( p->opts.nt < 0 || p->opts.nt > IFM_MAX_NT )
            {
                fprintf( stderr,
                    "option usage: -nt VOLUMES_PER_RUN\n"
                    "       error: VOLUMES_PER_RUN must be in [%d,%d]\n",
                    0, IFM_MAX_NT );
                errors++;
            }
        }
        else if ( ! strncmp( argv[ac], "-od", 3 ) ||
                  ! strncmp( argv[ac], "-gert_outdir", 9 ) )
        {
            if ( ++ac >= argc )
            {
                fputs( "option usage: -gert_outdir OUTPUT_DIR\n", stderr );
                usage( IFM_PROG_NAME, IFM_USE_SHORT );
                return 1;
            }

            p->opts.gert_outdir = argv[ac];
        }
        else if ( ! strncmp( argv[ac], "-pause", 6 ) )
        {
            if ( ++ac >= argc )
            {
                fputs( "option usage: -pause milliseconds\n", stderr );
                usage( IFM_PROG_NAME, IFM_USE_SHORT );
                return 1;
            }

            p->opts.pause = atoi(argv[ac]);
            if ( p->opts.pause < 0 )
            {
                fprintf(stderr,"** illegal -pause time: %s\n",argv[ac]);
                errors++;
            }
        }
        else if ( ! strncmp( argv[ac], "-quiet", 6 ) )
        {
            /* only go quiet if '-debug' option has not changed it */
            if ( gD.level == IFM_DEBUG_DEFAULT )
                gD.level = 0;
        }
        else if ( ! strncmp( argv[ac], "-quit", 5 ) )
        {
            p->opts.quit = 1;
        }
        else if ( ! strncmp( argv[ac], "-sp", 3 ) )
        {
            if ( ++ac >= argc )
            {
                fputs( "option usage: -sp PATTERN\n", stderr );
                usage( IFM_PROG_NAME, IFM_USE_SHORT );
                return 1;
            }

            p->opts.sp = argv[ac];
        }
        else if ( ! strncmp( argv[ac], "-start_dir", 8 ) )
        {
            if ( ++ac >= argc )
            {
                fputs( "option usage: -start_dir DIRECTORY\n", stderr );
                usage( IFM_PROG_NAME, IFM_USE_SHORT );
                return 1;
            }

            p->opts.start_dir = argv[ac];
        }
        else if ( ! strncmp( argv[ac], "-start_file", 8 ) )
        {
            if ( ++ac >= argc )
            {
                fputs( "option usage: -start_file DIR/FIRST_FILE\n", stderr );
                usage( IFM_PROG_NAME, IFM_USE_SHORT );
                return 1;
            }

            p->opts.start_file = argv[ac];
        }
        else if ( ! strncmp( argv[ac], "-version", 2 ) )
        {
            usage( IFM_PROG_NAME, IFM_USE_VERSION );
            return 1;
        }
        /* real-time options */
        else if ( ! strncmp( argv[ac], "-drive_afni", 6 ) )
        {
            if ( ++ac >= argc )
            {
                fputs( "option usage: -drive_afni COMMAND\n", stderr );
                usage( IFM_PROG_NAME, IFM_USE_SHORT );
                return 1;
            }

            if ( add_to_string_list( &p->opts.drive_list, argv[ac] ) != 0 )
            {
                fprintf(stderr,"** failed add '%s' to drive_list\n",argv[ac]);
                return 1;
            }
        }
        else if ( ! strncmp( argv[ac], "-host", 4 ) )
        {
            if ( ++ac >= argc )
            {
                fputs( "option usage: -host HOSTNAME\n", stderr );
                usage( IFM_PROG_NAME, IFM_USE_SHORT );
                return 1;
            }

            p->opts.host = argv[ac];    /* note and store the user option   */
            strncpy( A->host, argv[ac], ART_NAME_LEN-1 );
            A->host[ART_NAME_LEN-1] = '\0';     /* just to be sure */
        }
        else if ( ! strncmp( argv[ac], "-rev_byte_order", 4 ) )
        {
            p->opts.rev_bo = 1;           /* note to send reverse byte order */
        }
        else if ( ! strncmp( argv[ac], "-rt_cmd", 6 ) )
        {
            if ( ++ac >= argc )
            {
                fputs( "option usage: -rt_cmd COMMAND\n", stderr );
                usage( IFM_PROG_NAME, IFM_USE_SHORT );
                return 1;
            }

            if ( add_to_string_list( &p->opts.rt_list, argv[ac] ) != 0 )
            {
                fprintf(stderr,"** failed add '%s' to rt_list\n",argv[ac]);
                return 1;
            }
        }
        else if ( ! strncmp( argv[ac], "-rt", 3 ) )
        {
            A->state = ART_STATE_TO_OPEN; /* real-time is open for business */
            p->opts.rt = 1;               /* just note the user option      */
        }
        else if ( ! strncmp( argv[ac], "-swap", 5 ) )
        {
            A->swap = 1;                /* do byte swapping before sending  */
            p->opts.swap = 1;           /* just note the user option        */
        }
        else if ( ! strncmp( argv[ac], "-use_imon", 7 ) )
        {
            /* still run as Imon */
            p->opts.use_dicom = 0;
        }
        else if ( ! strncmp( argv[ac], "-zorder", 6 ) )
        {
            if ( ++ac >= argc )
            {
                fputs( "option usage: -zorder ORDER\n", stderr );
                usage( IFM_PROG_NAME, IFM_USE_SHORT );
                return 1;
            }

            A->zorder = argv[ac];
        }
        else
        {
            fprintf( stderr, "error: invalid option <%s>\n\n", argv[ac] );
            usage( IFM_PROG_NAME, IFM_USE_SHORT );
            return 1;
        }
    }

    if ( errors > 0 )          /* check for all minor errors before exiting */
    {
        usage( IFM_PROG_NAME, IFM_USE_SHORT );
        return 1;
    }

    if ( p->opts.start_dir == NULL && !p->opts.use_dicom )
    {
        fputs( "error: missing '-start_dir DIR' option\n", stderr );
        usage( IFM_PROG_NAME, IFM_USE_SHORT );
        return 1;
    }

    if ( p->opts.rev_bo && p->opts.swap )
    {
        fprintf( stderr, "error: options '-rev_byte_order' and '-swap' "
                 "cannot both be used\n");
        usage( IFM_PROG_NAME, IFM_USE_SHORT );
        return 1;
    }

    if ( A->zorder )
    {
        if ( strcmp(A->zorder, "alt") && strcmp(A->zorder, "seq") )
        {
            fprintf(stderr,"** order '%s' is invalid for '-zorder' option,\n"
                    "   must be either 'alt' or 'seq'\n", A->zorder);
            return 1;
        }
    }

    if ( p->opts.use_dicom )
    {   
        if( p->opts.gert_reco )
        {
            fprintf(stderr,"** -GERT_Reco is not valid for Dicom files\n");
            return 1;
        }
        if( ! p->opts.dicom_glob )
        {
            fprintf(stderr,"** missing -infile_pattern option\n");
            return 1;
        }
    }

    /* done processing argument list */

    /* if dicom, start_dir is not used for globbing */
    if ( p->opts.use_dicom )
    {
        p->glob_dir = p->opts.dicom_glob;
        p->ftype    = IFM_IM_FTYPE_DICOM;
    }
    else
    {
        if ( dir_expansion_form(p->opts.start_dir, &p->glob_dir) ) return 2;
        p->ftype = IFM_IM_FTYPE_GEMS5;
    }

    /* save command arguments to add as a NOTE to any AFNI datasets */
    p->opts.argv = argv;
    p->opts.argc = argc;

    if ( gD.level > 1 )
    {
        idisp_opts_t ( "end init_options : ", &p->opts );
        idisp_param_t( "end init_options : ", p );
    }

    /* rcr - make this an option (large-to-small sort is -1) */
    if ( p->opts.use_dicom )
        rglob_set_sort_dir( 1 );   /* use a negative sort */

    if ( gD.level > 0 )
        fprintf( stderr, "\n%s running, use <ctrl-c> to quit...\n\n",
                 IFM_PROG_NAME );

    return 0;
}


/*----------------------------------------------------------------------
 * initialize:
 *     - nice level
 *     - afni communications
 *     - verify that any passed 'start_file' "matches" 'start_dir'
 *       (for now, just do this by comparing directory depth)
 *----------------------------------------------------------------------
*/
static int init_extras( param_t * p, ART_comm * ac )
{
    if ( p->opts.nice && set_nice_level(p->opts.nice) )
        return 1;

    if ( ac->state == ART_STATE_TO_OPEN )            /* open afni comm link */
    {
        atexit( ART_exit );
        ac->mode = AFNI_OPEN_CONTROL_MODE;
        ART_open_afni_link( ac, 2, 1, gD.level );
    }
    
    /* check directory depth of start_file against glob_dir */
    if ( p->opts.start_file != NULL )
    {
        char * sf     = p->opts.start_file;
        char * gd     = p->glob_dir;
        int    flevel = str_char_count( sf, strlen(sf), (char)'/' );

        /* check whether the number of slashes match */
        if ( flevel != str_char_count( gd, strlen(gd), (char)'/' ) )
        {
            fprintf( stderr,
                     "** warning : relative path to       : '-start_dir  %s'\n"
                     "             does not seem to match : '-start_file %s'\n"
                     "             (so 'start_file' may never be found)\n\n",
                     p->opts.start_file, p->opts.start_dir );
        }
        else if ( gD.level > 1 )
            fprintf( stderr, "-- '-start_file %s' and\n"
                             "   '-start_dir  %s' match at dir level %d\n",
                             p->opts.start_file, p->opts.start_dir, flevel );
    }

    return 0;
}


/*----------------------------------------------------------------------
 *  set_nice_level              - set to given "nice" value
 *
 *  returns:   0  : success
 *           else : failure
 *----------------------------------------------------------------------
*/
static int set_nice_level( int level )
{
    int rv;

    rv = nice( level );
    if ( rv != 0 )
    {
        if ( level < 0 )
            fprintf( stderr, "error: only root may decrement nice value\n"
                             "       (errno = %d, rv = %d)\n", errno, rv );
        else
            fprintf( stderr,
                     "error: failure to adjust nice by %d\n"
                     "       (errno = %d, rv = %d)\n", level, errno, rv );
    }
    else if ( gD.level > 1 )
        fprintf( stderr, "-- nice value incremented by %d\n", level );

    return rv;
}


/*------------------------------------------------------------
 *  dir_expansion_form
 *
 *  sin    : must be in the form "...0[01]n", where n is a digit
 *           (note that "...0[01]n/..." is okay)
 *  sexp   : memory is allocated for the output string of the
 *           form: "...[0-9][02468]n/I.*"
 *
 *  returns:    0 : on success
 *           else : error
 *------------------------------------------------------------
*/
int dir_expansion_form( char * sin, char ** sexp )
{
    char * out;
    char * cp;
    char   d0, d1, d2;                  /* the three relevant digits */
    int    len;

    if ( (sin == NULL) || (sexp == NULL) )
        return -1;

    *sexp = NULL;
    len = strlen(sin);

    out = (char *)malloc((len + IFM_PAD_LEN) * sizeof(char));
    if ( out == NULL )
    {
        fprintf( stderr, "failure: dir_expansion_form malloc\n" );
        return -1;
    }

    *sexp = out;                        /* save resulting malloc'd address */

    strcpy( out,sin );

    cp = out + len - 1;                         /* point to end */

    /* we'd better find 0[01]n - ignore the rest??? */
    while ( (cp > (out+2)) && !isdigit( *cp ) )
        cp--;

    if ( !isdigit(*cp) )                        /* didn't find even one */
    {
        fprintf( stderr, "error: dir <%s> is not of the form 00n (e.g. 003)\n",
                 sin );
        free(out);
        return -1;
    }

    cp -= 2;                                    /* should be first zero  */

    d0 = cp[0];                                 /* note the three digits */
    d1 = cp[1];
    d2 = cp[2];

    if ( (d0 != '0') ||                         /* first is not a zero   */
         ( (d1 != '0') && (d1 != '1')) )        /* second is not 0 or 1  */
    {
        fprintf( stderr, "error: dir <%s> is not of the form 0[01]n"
                         " (e.g. 003)\n", sin );
        free(out);
        return -1;
    }

    /* woohooo!  we're good to go! */
    /* set to "...[0-9][02468]n/I.*" (or with [13579]) */

    strcpy( cp, "[0-9]" );                      /* add and skip "[0-9]" */
    cp += strlen( "[0-9]" );

    if ( d1 == '0' )                            /* adding 2 to each     */
        strcpy( cp, "[02468]" );
    else
        strcpy( cp, "[13579]" );
    cp += strlen( "[02468]" );

    *cp++ = d2;                                 /* insert final digit */

    /* allow either I.* or i.*  - v2.11 */
    strcpy( cp, "/[Ii].*" );                    /* the big finish */

    return 0;
}


/*----------------------------------------------------------------------
 *  read_dicom_image
 *---------------------------------------------------------------------- */
static int read_dicom_image( char * pathname, finfo_t * fp )
{
    MRI_IMAGE * im;

    im = r_mri_read_dicom( pathname, gD.level, &fp->image );
    if ( !im )
    {
        fprintf(stderr,"** failed to read file '%s' as dicom\n", pathname);
        return 1;
    }

    if ( gD.level > 2 )
    {
        fprintf(stderr,"+d dinfo (%s): std, ser, im = (%d, %d, %3d)\n",
            pathname,
            gr_dimon_stuff.study, gr_dimon_stuff.series, gr_dimon_stuff.image );
        if ( gD.level > 2 )
        fprintf(stderr,"          im->xo,yo,zo =    (%6.1f,%6.1f,%6.1f)\n",
                im->xo, im->yo, im->zo);
    }

    /* fill the finfo_t struct */

    fp->geh.good = 1;
    fp->geh.nx   = im->nx;
    fp->geh.ny   = im->ny;
    fp->geh.uv17 = gr_dimon_stuff.series;
    fp->geh.dx   = im->dx;
    fp->geh.dy   = im->dy;
    fp->geh.dz   = im->dz;
    fp->geh.zoff = im->zo;

    /* get some stuff from mrilib */
    fp->geh.tr = DI_MRL_tr;
    fp->geh.te = 0; /* rcr - none to set? */
    memset(fp->geh.orients, 0, 8);
    strncpy(fp->geh.orients, DI_MRL_orients, 7);

    /* ge_extras */
    fp->gex.bpp    = im->pixel_size;
    fp->gex.cflag  = 0;
    fp->gex.hdroff = -1;
    fp->gex.skip   = -1;
    fp->gex.swap   = im->was_swapped;
    fp->gex.kk     = 0;
    fp->gex.xorg   = im->xo;
    fp->gex.yorg   = im->yo;

    /* skip xyz[9] */

    fp->bytes = im->nvox * im->pixel_size;

    free(im);  /* do not free data, of course */

    return 0;
}


/*----------------------------------------------------------------------
 *  read_ge_image  (basically from Ifile.c, but use file_tool.c version)
 *
 *  returns:   0  : success
 *           < 0  : failure
 *----------------------------------------------------------------------
*/
static int read_ge_image( char * pathname, finfo_t * fp,
                          int get_image, int need_memory )
{
   ge_header_info * hi  = &fp->geh;

   FILE *imfile ;
   int  length , skip , swap=0 ;
   char orients[8] , str[8] ;
   int nx , ny , bpp , cflag , hdroff ;
        float uv17 = -1.0;
        
   if( hi == NULL ) return -1;            /* bad */
   hi->good = 0 ;                       /* not good yet */
   if( pathname    == NULL ||
       pathname[0] == '\0'   ) return -1; /* bad */

   length = l_THD_filesize( pathname ) ;
   if( length < 1024 ) return -1;         /* bad */

   imfile = fopen( pathname , "r" ) ;
   if( imfile == NULL ) return -1;        /* bad */

   strcpy(str,"JUNK") ;     /* initialize string */
   fread(str,1,4,imfile) ;  /* check for "IMGF" at start of file */

   if( str[0]!='I' || str[1]!='M' || str[2]!='G' || str[3]!='F' ){ /* bad */
      fclose(imfile) ; return -2;
   }

   /*-- read next 5 ints (after the "IMGF" string) --*/

   fread( &skip , 4,1, imfile ) ; /* offset into file of image data */
   fread( &nx   , 4,1, imfile ) ; /* x-size */
   fread( &ny   , 4,1, imfile ) ; /* y-size */
   fread( &bpp  , 4,1, imfile ) ; /* bits per pixel (should be 16) */
   fread( &cflag, 4,1, imfile ) ; /* compression flag (1=uncompressed)*/

        /*-- check if nx is funny --*/

   if( nx < 0 || nx > 8192 ){      /* have to byte swap these 5 ints */
     swap = 1 ;                    /* flag to swap data, too */
     swap_4(&skip); swap_4(&nx); swap_4(&ny); swap_4(&bpp); swap_4(&cflag);
   } else {
     swap = 0 ;  /* data is ordered for this CPU */
   }
   if( nx < 0 || nx > 8192 || ny < 0 || ny > 8192 ){  /* bad */
      fclose(imfile) ; return -1;
   }

   hi->nx = nx ;
   hi->ny = ny ;

   if( skip+2*nx*ny >  length ||               /* file is too short */
       skip         <= 0      ||               /* bizarre  */
       cflag        != 1      ||               /* data is compressed */
       bpp          != 16        ){
      fclose(imfile); return -1;    /* data is not shorts */
   }

   /*-- try to read image header data as well --*/

   fseek( imfile , 148L , SEEK_SET ) ; /* magic GEMS offset */
   fread( &hdroff , 4,1 , imfile ) ;   /* location of image header */
   if( swap ) swap_4(&hdroff) ;

   if( hdroff > 0 && hdroff+256 < length ){   /* can read from image header */
       float dx,dy,dz, xyz[9], zz ; int itr, ii,jj,kk ;

       /*-- get voxel grid sizes --*/

       fseek( imfile , hdroff+26 , SEEK_SET ) ;    /* dz */
       fread( &dz , 4,1 , imfile ) ;

       fseek( imfile , hdroff+50 , SEEK_SET ) ;    /* dx and dy */
       fread( &dx , 4,1 , imfile ) ;
       fread( &dy , 4,1 , imfile ) ;

       if( swap ){ swap_4(&dx); swap_4(&dy); swap_4(&dz); }

       hi->dx = dx ; hi->dy = dy ; hi->dz = dz ;

       /* grid orientation: from 3 sets of LPI corner coordinates: */
       /*   xyz[0..2] = top left hand corner of image     (TLHC)   */
       /*   xyz[3..5] = top right hand corner of image    (TRHC)   */
       /*   xyz[6..8] = bottom right hand corner of image (BRHC)   */
       /* GEMS coordinate orientation here is LPI                  */

       fseek( imfile , hdroff+154 , SEEK_SET ) ;  /* another magic number */
       fread( xyz , 4,9 , imfile ) ;
       if( swap ){
          swap_4(xyz+0); swap_4(xyz+1); swap_4(xyz+2);
          swap_4(xyz+3); swap_4(xyz+4); swap_4(xyz+5);
          swap_4(xyz+6); swap_4(xyz+7); swap_4(xyz+8);
       }

       /* x-axis orientation */
       /* ii determines which spatial direction is x-axis  */
       /* and is the direction that has the biggest change */
       /* between the TLHC and TRHC                        */

       dx = fabs(xyz[3]-xyz[0]) ; ii = 1 ;
       dy = fabs(xyz[4]-xyz[1]) ; if( dy > dx ){ ii=2; dx=dy; }
       dz = fabs(xyz[5]-xyz[2]) ; if( dz > dx ){ ii=3;        }
       dx = xyz[ii+2]-xyz[ii-1] ; if( dx < 0. ){ ii = -ii;    }
       switch( ii ){
        case  1: orients[0]= 'L'; orients[1]= 'R'; break;
        case -1: orients[0]= 'R'; orients[1]= 'L'; break;
        case  2: orients[0]= 'P'; orients[1]= 'A'; break;
        case -2: orients[0]= 'A'; orients[1]= 'P'; break;
        case  3: orients[0]= 'I'; orients[1]= 'S'; break;
        case -3: orients[0]= 'S'; orients[1]= 'I'; break;
        default: orients[0]='\0'; orients[1]='\0'; break;
       }

       /* y-axis orientation */
       /* jj determines which spatial direction is y-axis  */
       /* and is the direction that has the biggest change */
       /* between the BRHC and TRHC                        */

       dx = fabs(xyz[6]-xyz[3]) ; jj = 1 ;
       dy = fabs(xyz[7]-xyz[4]) ; if( dy > dx ){ jj=2; dx=dy; }
       dz = fabs(xyz[8]-xyz[5]) ; if( dz > dx ){ jj=3;        }
       dx = xyz[jj+5]-xyz[jj+2] ; if( dx < 0. ){ jj = -jj;    }
       switch( jj ){
         case  1: orients[2] = 'L'; orients[3] = 'R'; break;
         case -1: orients[2] = 'R'; orients[3] = 'L'; break;
         case  2: orients[2] = 'P'; orients[3] = 'A'; break;
         case -2: orients[2] = 'A'; orients[3] = 'P'; break;
         case  3: orients[2] = 'I'; orients[3] = 'S'; break;
         case -3: orients[2] = 'S'; orients[3] = 'I'; break;
         default: orients[2] ='\0'; orients[3] ='\0'; break;
       }

       orients[4] = '\0' ;   /* terminate orientation string */

       kk = 6 - abs(ii)-abs(jj) ;   /* which spatial direction is z-axis   */
                                    /* where 1=LR, 2=PA, 3=IS               */
                                    /* (can't tell orientation from 1 slice) */

       zz = xyz[kk-1] ;             /* z-coordinate of this slice */

       hi->zoff = zz ;
       strcpy(hi->orients,orients) ;

       /* similarly (with zoff), store x and y origins in ge_extras */
       /*                                       2003 Jun 25 [rickr] */
       fp->gex.xorg = xyz[abs(ii)-1];
       fp->gex.yorg = xyz[abs(jj)-1];

       /*-- get TR in seconds --*/

       fseek( imfile , hdroff+194 , SEEK_SET ) ;
       fread( &itr , 4,1 , imfile ) ; /* note itr is an int */
       if( swap ) swap_4(&itr) ;
       hi->tr = 1.0e-6 * itr ;        /* itr is in microsec */

       /*-- get TE in milliseconds --*/

       fseek( imfile , hdroff+202 , SEEK_SET ) ;
       fread( &itr , 4,1 , imfile ) ; /* itr is an int, in microsec */
       if( swap ) swap_4(&itr) ;
       hi->te = 1.0e-6 * itr ;

       /* zmodify: get User Variable 17, a likely indicator of a new scan,
        * info by S. Marrett, location from S. Inati's matlab function
        * GE_readHeaderImage.m
        */

        /* printf ("\nuv17 = \n"); */
        fseek ( imfile , hdroff+272+202, SEEK_SET ) ;
        fread( &uv17 , 4, 1 , imfile ) ;
        if( swap ) swap_4(&uv17) ;
        /* printf ("%d ", (int)uv17);  */
        hi->uv17 = (int)uv17; 
        /* printf ("\n"); */

        /* store the ge_extra info */
        fp->gex.bpp    = bpp;
        fp->gex.cflag  = cflag;
        fp->gex.hdroff = hdroff;
        fp->gex.skip   = skip;
        fp->gex.swap   = swap;
        fp->gex.kk     = kk;

        memcpy( fp->gex.xyz, xyz, sizeof(xyz) );
        
        hi->good = 1 ;                  /* this is a good file */

    } /* end of actually reading image header */

    /* read image in as well */
    if ( get_image )
    {
        int elements = hi->nx * hi->ny;

        fp->bytes = elements * 2;                       /* bpp == 16 */

        if ( need_memory )
            fp->image = malloc( fp->bytes );

        if ( fp->image == NULL )
        {
            fprintf(stderr, "** RGI: no memory for %d byte image\n", fp->bytes);
            hi->good = 0;
            return -1;
        }

        fseek ( imfile, skip, SEEK_SET );
        if ( fread( fp->image , 2, elements, imfile ) != elements )
        {
            fprintf( stderr, "** RGI: failed to read %d shorts from %s\n",
                     elements, pathname );
            hi->good = 0;               /* signal file problem */
            return -1;
        }
    }

    fclose(imfile);
    return 0;
}


/*------------------------------------------------------------
 * Reverse the order of the 4 bytes at this address.
 *------------------------------------------------------------
*/
static int swap_4( void * ptr )            /* destructive */
{
    unsigned char * addr = ptr;

    addr[0] ^= addr[3]; addr[3] ^= addr[0]; addr[0] ^= addr[3];
    addr[1] ^= addr[2]; addr[2] ^= addr[1]; addr[1] ^= addr[2];

    return 0;
}


/*------------------------------------------------------------
 * print out the contents of the im_store_t struct
 *------------------------------------------------------------
*/
static int idisp_im_store_t( char * info, im_store_t * is )
{
    if ( info )
        fputs( info, stdout );

    if ( is == NULL )
    {
        printf( "idisp_im_store_t: is == NULL\n" );
        return -1;
    }

    printf( "im_store_t struct at %p :\n"
            "   (nalloc, nused)    = (%d, %d)\n"
            "   (ary_len, im_size) = (%d, %d)\n"
            "   (im_ary, x_im)     = (%p, %p)\n",
            is, is->nalloc, is->nused,
            is->ary_len, is->im_size, is->im_ary, is->x_im );

    return 0;
}


/*------------------------------------------------------------
 * print out the contents of the param_t struct
 *------------------------------------------------------------
*/
static int idisp_param_t( char * info, param_t * p )
{
    if ( info )
        fputs( info, stdout );

    if ( p == NULL )
    {
        printf( "idisp_param_t: p == NULL\n" );
        return -1;
    }

    printf( "param_t struct at %p :\n"
            "   ftype             = %d\n"
            "   (nused, nalloc)   = (%d, %d)\n"
            "   flist             = %p\n"
            "   glob_dir          = %s\n"
            "   nfiles            = %d\n"
            "   fnames            = %p\n",
            p, p->ftype, p->nused, p->nalloc, p->flist,
            CHECK_NULL_STR(p->glob_dir),
            p->nfiles, p->fnames );

    return 0;
}


/*------------------------------------------------------------
 * print out the contents of the opts_t struct
 *------------------------------------------------------------
*/
static int idisp_opts_t( char * info, opts_t * opt )
{
    if ( info )
        fputs( info, stdout );

    if ( opt == NULL )
    {
        printf( "idisp_opts_t: opt == NULL\n" );
        return -1;
    }

    printf( "opts_t struct at %p :\n"
            "   start_file         = %s\n"
            "   start_dir          = %s\n"
            "   dicom_glob         = %s\n"
            "   sp                 = %s\n"
            "   gert_outdir        = %s\n"
            "   (argv, argc)       = (%p, %d)\n"
            "   (nt, nice)         = (%d, %d)\n"
            "   pause              = %d\n"
            "   (debug, gert_reco) = (%d, %d)\n"
            "   use_dicom, quit    = %d, %d\n"
            "   (rt, swap, rev_bo) = (%d, %d, %d)\n"
            "   host               = %s\n"
            "   drive_list(u,a,p)  = %d, %d, %p\n"
            "   rt_list   (u,a,p)  = %d, %d, %p\n",
            opt,
            CHECK_NULL_STR(opt->start_file),
            CHECK_NULL_STR(opt->start_dir),
            CHECK_NULL_STR(opt->dicom_glob),
            CHECK_NULL_STR(opt->sp),
            CHECK_NULL_STR(opt->gert_outdir),
            opt->argv, opt->argc,
            opt->nt, opt->nice, opt->pause,
            opt->debug, opt->gert_reco, opt->use_dicom, opt->quit,
            opt->rt, opt->swap, opt->rev_bo,
            CHECK_NULL_STR(opt->host),
            opt->drive_list.nused, opt->drive_list.nalloc, opt->drive_list.str,
            opt->rt_list.nused, opt->rt_list.nalloc, opt->rt_list.str
            );

    return 0;
}


/*------------------------------------------------------------
 * print out a string corresponding to the file type
 *------------------------------------------------------------
*/
static int disp_ftype( char * info, int ftype )
{
    if ( info ) fputs(info, stdout);

    switch( ftype )
    {
        case IFM_IM_FTYPE_GEMS5:
            printf("GEMS 5.x\n");
            break;

        case IFM_IM_FTYPE_DICOM:
            printf("DICOM\n");
            break;

        default:
            printf("UNKNOWN (%d)\n", ftype);
            break;
    }

    fflush(stdout);

    return 0;
}

/*------------------------------------------------------------
 * print out the contents of the vol_t struct
 *------------------------------------------------------------
*/
static int idisp_vol_t( char * info, vol_t * v )
{
    if ( info )
        fputs( info, stdout );

    if ( v == NULL )
    {
        printf( "idisp_vol_t: v == NULL\n" );
        return -1;
    }

    printf( "vol_t struct at %p :\n"
            "   nim                 = %d\n"
            "   (fl_1, fn_1, fn_n)  = (%d, %d, %d)\n"
            "   first_file          = %s\n"
            "   last_file           = %s\n"
            "   (z_first, z_last)   = (%f, %f)\n"
            "   z_delta             = %f\n"
            "   (seq_num, run)      = (%d, %d)\n",
            v, v->nim, v->fl_1, v->fn_1, v->fn_n,
            v->first_file, v->last_file,
            v->z_first, v->z_last, v->z_delta,
            v->seq_num, v->run );

    idisp_ge_header_info( info, &v->geh );
    idisp_ge_extras( info, &v->gex );

    return 0;
}


/*------------------------------------------------------------
 *  Display the contents of the ge_extras struct.
 *  (copied from file_tool.c)
 *------------------------------------------------------------
*/
static int idisp_ge_extras( char * info, ge_extras * E )
{
    if ( info )
        fputs( info, stdout );

    if ( E == NULL )
    {
        printf( "idisp_ge_extras: E == NULL\n" );
        return -1;
    }

    printf( "ge_extras at %p :\n"
            "    bpp              = %d\n"
            "    cflag            = %d\n"
            "    hdroff           = %d\n"
            "    skip             = %d\n"
            "    swap             = %d\n"
            "    kk               = %d\n"
            "    xorg             = %f\n"
            "    yorg             = %f\n"
            "    (xyz0,xyz1,xyz2) = (%f,%f,%f)\n"
            "    (xyz3,xyz4,xyz5) = (%f,%f,%f)\n"
            "    (xyz6,xyz7,xyz8) = (%f,%f,%f)\n",
            E, E->bpp, E->cflag, E->hdroff, E->skip, E->swap, E->kk,
            E->xorg,   E->yorg,
            E->xyz[0], E->xyz[1], E->xyz[2],
            E->xyz[3], E->xyz[4], E->xyz[5],
            E->xyz[6], E->xyz[7], E->xyz[8]
          );
    return 0;
}

/*------------------------------------------------------------
 *  Display the contents of the ge_header_info struct.
 *  (copied from file_tool.c)
 *------------------------------------------------------------
*/
static int idisp_ge_header_info( char * info, ge_header_info * I )
{
    if ( info )
        fputs( info, stdout );

    if ( I == NULL )
    {
        printf( "idisp_ge_header_info: I == NULL\n" );
        return -1;
    }

    printf( "ge_header_info at %p :\n"
            "    good        = %d\n"
            "    (nx,ny)     = (%d,%d)\n"
            "    uv17        = %d\n"
            "    (dx,dy,dz)  = (%f,%f,%f)\n"
            "    zoff        = %f\n"
            "    (tr,te)     = (%f,%f)\n"
            "    orients     = %-8s\n",
            I, I->good, I->nx, I->ny, I->uv17,
            I->dx, I->dy, I->dz, I->zoff, I->tr, I->te,
            CHECK_NULL_STR(I->orients)
          );

    return 0;
}

/*----------------------------------------------------------------------
 * usage 
 *----------------------------------------------------------------------
*/
static int usage ( char * prog, int level )
{
    if ( level == IFM_USE_SHORT )
    {
        fprintf( stderr,
            "usage: %s [options] -start_dir DIR\n"
            "usage: %s -help\n",
            prog, prog );
        return 0;
    }
    else if ( level == IFM_USE_LONG )
    {
        printf(
          "\n"
          "%s - monitor real-time acquisition of DICOM image files\n"
          "    (or GEMS 5.x I-files, as 'Imon')\n"
          "\n"
          "    This program is intended to be run during a scanning session\n"
          "    on a scanner, to monitor the collection of image files.  The\n"
          "    user will be notified of any missing slice or any slice that\n"
          "    is aquired out of order.\n"
          "\n"
          "    When collecting DICOM files, it is recommended to run this\n"
          "    once per run, only because it is easier to specify the input\n"
          "    file pattern for a single run (it may be very difficult to\n"
          "    predict the form of input filenames runs that have not yet\n"
          "    occurred.\n"
          "\n"
          "    If no -quit option is provided, the user should terminate the\n"
          "    program when it is done collecting images according to the\n"
          "    input file pattern.\n"
          "\n"
          "    Dimon can be terminated using <ctrl-c>.\n"
          "\n"
          "  ---------------------------------------------------------------\n"
          "  realtime notes for running afni remotely:\n"
          "\n"
          "    - The afni program must be started with the '-rt' option to\n"
          "      invoke the realtime plugin functionality.\n"
          "\n"
          "    - If afni is run remotely, then AFNI_TRUSTHOST will need to be\n"
          "      set on the host running afni.  The value of that variable\n"
          "      should be set to the IP address of the host running %s.\n"
          "      This may set as an environment variable, or via the .afnirc\n"
          "      startup file.\n"
          "\n"
          "    - The typical default security on a Linux system will prevent\n"
          "      %s from communicating with afni on the host running afni.\n"
          "      The iptables firewall service on afni's host will need to be\n"
          "      configured to accept the communication from the host running\n"
          "      %s, or it (iptables) will need to be turned off.\n"
          "  ---------------------------------------------------------------\n"
          "  usage: %s [options] -infile_prefix PREFIX\n"
          "     OR: %s [options] -infile_pattern \"PATTERN\"\n"
          "\n"
          "  ---------------------------------------------------------------\n"
          "  examples (no real-time options):\n"
          "\n"
          "    %s -infile_pattern 's8912345/i*'\n"
          "    %s -infile_prefix   s8912345/i\n"
          "    %s -help\n"
          "    %s -infile_prefix   s8912345/i   -quit\n"
          "    %s -infile_prefix   s8912345/i   -nt 120 -quit\n"
          "    %s -infile_prefix   s8912345/i   -debug 2\n"
          "\n"
          "  examples (with real-time options):\n"
          "\n"
          "    %s -infile_prefix s8912345/i -rt \n"
          "\n"
          "    %s -infile_pattern 's*/i*' -rt \n"
          "    %s -infile_pattern 's*/i*' -rt -nt 120\n"
          "    %s -infile_pattern 's*/i*' -rt -quit\n"
          "\n"
          "  ** detailed real-time example:\n"
          "    %s                                    \\\n"
          "       -infile_pattern 's*/i*'               \\\n"
          "       -rt -nt 120                           \\\n"
          "       -host some.remote.computer            \\\n"
          "       -rt_cmd \"PREFIX 2005_0513_run3\"     \\\n"
          "       -quit                                 \n"
          "\n"
          "    This example scans data starting from directory 003, expects\n"
          "    160 repetitions (TRs), and invokes the real-time processing,\n"
          "    sending data to a computer called some.remote.computer.name\n"
          "    (where afni is running, and which considers THIS computer to\n"
          "    be trusted - see the AFNI_TRUSTHOST environment variable).\n"
          "\n"
          "  ---------------------------------------------------------------\n"
          "    Multiple DRIVE_AFNI commands are passed through '-drive_afni'\n"
          "    options, one requesting to open an axial image window, and\n"
          "    another requesting an axial graph, with 160 data points.\n"
          "\n"
          "    See README.driver for acceptable DRIVE_AFNI commands.\n"
          "\n"
          "    Also, multiple commands specific to the real-time plugin are\n"
          "    passed via '-rt_cmd' options.  The PREFIX command sets the\n"
          "    prefix for the datasets output by afni.  The GRAPH_XRANGE and\n"
          "    GRAPH_YRANGE commands set the graph dimensions for the 3D\n"
          "    motion correction graph (only).  And the GRAPH_EXPR command\n"
          "    is used to replace the 6 default motion correction graphs with\n"
          "    a single graph, according to the given expression, the square\n"
          "    root of the average squared entry of the 3 rotaion parameters,\n"
          "    roll, pitch and yaw, ignoring the 3 shift parameters, dx, dy\n"
          "    and dz.\n"
          "\n"
          "    See README.realtime for acceptable DRIVE_AFNI commands.\n"
          "\n"
          "    %s                                                   \\\n"
          "       -infile_pattern 's*/i*.dcm'                         \\\n"
          "       -nt 160                                             \\\n"
          "       -rt                                                 \\\n"
          "       -host some.remote.computer.name                     \\\n"
          "       -drive_afni 'OPEN_WINDOW axialimage'                \\\n"
          "       -drive_afni 'OPEN_WINDOW axialgraph pinnum=160'     \\\n"
          "       -rt_cmd 'PREFIX eat.more.cheese'                    \\\n"
          "       -rt_cmd 'GRAPH_XRANGE 160'                          \\\n"
          "       -rt_cmd 'GRAPH_YRANGE 1.02'                         \\\n"
          "       -rt_cmd 'GRAPH_EXPR sqrt((d*d+e*e+f*f)/3)'            \n"
          "\n"
          "  ---------------------------------------------------------------\n",
          prog, prog, prog, prog, prog, prog,
          prog, prog, prog, prog, prog, prog,
          prog, prog, prog, prog, prog, prog );
          
        printf(
          "  notes:\n"
          "\n"
          "    - Once started, unless the '-quit' option is used, this\n"
          "      program exits only when a fatal error occurs (single\n"
          "      missing or out of order slices are not considered fatal).\n"
          "      Otherwise, it keeps waiting for new data to arrive.\n"
          "\n"
          "      With the '-quit' option, the program will terminate once\n"
          "      there is a significant (~2 TR) pause in acquisition.\n"
          "\n"
          "    - To terminate this program, use <ctrl-c>.\n"
          "\n"
          "  ---------------------------------------------------------------\n"
          "  main options:\n"
          "\n"
          "    For DICOM images, either -infile_pattern or -infile_prefix\n"
          "    is required.\n"
          "\n"
          "    -infile_pattern PATTERN : specify pattern for input files\n"
          "\n"
          "        e.g. -infile_pattern 'run1/i*.dcm'\n"
          "\n"
          "        This option is used to specify a wildcard pattern matching\n"
          "        the names of the input DICOM files.  These files should be\n"
          "        sorted in the order that they are to be assembled, i.e.\n"
          "        when the files are sorted alphabetically, they should be\n"
          "        sequential slices in a volume, and the volumes should then\n"
          "        progress over time (as with the 'to3d' program).\n"
          "\n"
          "        The pattern for this option must be within quotes, because\n"
          "        it will be up to the program to search for new files (that\n"
          "        match the pattern), not the shell.\n"
          "\n"
          "    -infile_prefix PREFIX   : specify prefix matching input files\n"
          "\n"
          "        e.g. -infile_prefix run1/i\n"
          "\n"
          "        This option is similar to -infile_pattern.  By providing\n"
          "        only a prefix, the user need not use wildcard characters\n"
          "        with quotes.  Using PREFIX with -infile_prefix is\n"
          "        equivalent to using 'PREFIX*' with -infile_pattern (note\n"
          "        the needed quotes).\n"
          "\n"
          "        Note that it may not be a good idea to use, say 'run1/'\n"
          "        for the prefix, as there might be a readme file under\n"
          "        that directory.\n"
          "\n"
          "        Note also that it is necessary to provide a '/' at the\n"
          "        end, if the prefix is a directory (e.g. use run1/ instead\n"
          "        of simply run1).\n"
          "\n"
          "  ---------------------------------------------------------------\n"
          "  real-time options:\n"
          "\n"
          "    -rt                : specify to use the real-time facility\n"
          "\n"
          "        With this option, the user tells '%s' to use the real-time\n"
          "        facility, passing each volume of images to an existing\n"
          "        afni process on some machine (as specified by the '-host'\n"
          "        option).  Whenever a new volume is aquired, it will be\n"
          "        sent to the afni program for immediate update.\n"
          "\n"
          "        Note that afni must also be started with the '-rt' option\n"
          "        to make use of this.\n"
          "\n"
          "        Note also that the '-host HOSTNAME' option is not required\n"
          "        if afni is running on the same machine.\n"
          "\n"
          "    -drive_afni CMND   : send 'drive afni' command, CMND\n"
          "\n"
          "        e.g.  -drive_afni 'OPEN_WINDOW axialimage'\n"
          "\n"
          "        This option is used to pass a single DRIVE_AFNI command\n"
          "        to afni.  For example, 'OPEN_WINDOW axialimage' will open\n"
          "        such an axial view window on the afni controller.\n"
          "\n"
          "        Note: the command 'CMND' must be given in quotes, so that\n"
          "              the shell will send it as a single parameter.\n"
          "\n"
          "        Note: this option may be used multiple times.\n"
          "\n"
          "        See README.driver for more details.\n"
          "\n"
          "    -host HOSTNAME     : specify the host for afni communication\n"
          "\n"
          "        e.g.  -host mycomputer.dot.my.network\n"
          "        e.g.  -host 127.0.0.127\n"
          "        e.g.  -host mycomputer\n"
          "        the default host is 'localhost'\n"
          "\n"
          "        The specified HOSTNAME represents the machine that is\n"
          "        running afni.  Images will be sent to afni on this machine\n"
          "        during the execution of '%s'.\n"
          "\n"
          "        Note that the enviroment variable AFNI_TRUSTHOST must be\n"
          "        set on the machine running afni.  Set this equal to the\n"
          "        name of the machine running Imon (so that afni knows to\n"
          "        accept the data from the sending machine).\n"
          "\n"
          "    -pause TIME_IN_MS : pause after each new volume\n"
          "\n"
          "        e.g.  -pause 200\n"
          "\n"
          "        In some cases, the user may wish to slow down a real-time\n"
          "        process.  This option will cause a delay of TIME_IN_MS\n"
          "        milliseconds after each volume is found.\n"
          "\n"
          "    -rev_byte_order   : pass the reverse of the BYTEORDER to afni\n"
          "\n"
          "        Reverse the byte order that is given to afni.  In case the\n"
          "        detected byte order is not what is desired, this option\n"
          "        can be used to reverse it.\n"
          "\n"
          "        See the (obsolete) '-swap' option for more details.\n"
          "\n"
          "    -rt_cmd COMMAND   : send COMMAND(s) to realtime plugin\n"
          "\n"
          "        e.g.  -rt_cmd 'GRAPH_XRANGE 120'\n"
          "        e.g.  -rt_cmd 'GRAPH_XRANGE 120 \\n GRAPH_YRANGE 2.5'\n"
          "\n"
          "        This option is used to pass commands to the realtime\n"
          "        plugin.  For example, 'GRAPH_XRANGE 120' will set the\n"
          "        x-scale of the motion graph window to 120 (repetitions).\n"
          "\n"
          "        Note: the command 'COMMAND' must be given in quotes, so\n"
          "        that the shell will send it as a single parameter.\n"
          "\n"
          "        Note: this option may be used multiple times.\n"
          "\n"
          "        See README.realtime for more details.\n"
          "\n"
          "    -swap  (obsolete) : swap data bytes before sending to afni\n"
          "\n"
          "        Since afni may be running on a different machine, the byte\n"
          "        order may differ there.  This option will force the bytes\n"
          "        to be reversed, before sending the data to afni.\n"
          "\n"
          "        ** As of version 3.0, this option should not be necessary.\n"
          "           '%s' detects the byte order of the image data, and then\n"
          "           passes that information to afni.  The realtime plugin\n"
          "           will (now) decide whether to swap bytes in the viewer.\n"
          "\n"
          "           If for some reason the user wishes to reverse the order\n"
          "           from what is detected, '-rev_byte_order' can be used.\n"
          "\n"
          "    -zorder ORDER     : slice order over time\n"
          "\n"
          "        e.g. -zorder alt\n"
          "        e.g. -zorder seq\n"
          "        the default is 'alt'\n"
          "\n"
          "        This options allows the user to alter the slice\n"
          "        acquisition order in real-time mode, simliar to the slice\n"
          "        pattern of the '-sp' option.  The main differences are:\n"
          "            o  only two choices are presently available\n"
          "            o  the syntax is intentionally different (from that\n"
          "               of 'to3d' or the '-sp' option)\n"
          "\n"
          "        ORDER values:\n"
          "            alt   : alternating in the Z direction (over time)\n"
          "            seq   : sequential in the Z direction (over time)\n"
          "\n"
          "  ---------------------------------------------------------------\n"
          "  other options:\n"
          "\n"
          "    -debug LEVEL       : show debug information during execution\n"
          "\n"
          "        e.g.  -debug 2\n"
          "        the default level is 1, the domain is [0,3]\n"
          "        the '-quiet' option is equivalent to '-debug 0'\n"
          "\n"
          "    -help              : show this help information\n"
          "\n"
          "    -hist              : display a history of program changes\n"
          "\n"
          "    -nice INCREMENT    : adjust the nice value for the process\n"
          "\n"
          "        e.g.  -nice 10\n"
          "        the default is 0, and the maximum is 20\n"
          "        a superuser may use down to the minimum of -19\n"
          "\n"
          "        A positive INCREMENT to the nice value of a process will\n"
          "        lower its priority, allowing other processes more CPU\n"
          "        time.\n"
          "\n"
          "    -nt VOLUMES_PER_RUN : set the number of time points per run\n"
          "\n"
          "        e.g.  -nt 120\n"
          "\n"
          "        With this option, if a run stalls before the specified\n"
          "        VOLUMES_PER_RUN is reached (notably including the first\n"
          "        run), the user will be notified.\n"
          "\n"
          "        Without this option, %s will compute the expected number\n"
          "        of time points per run based on the first run (and will\n"
          "        allow the value to increase based on subsequent runs).\n"
          "        Therefore %s would not detect a stalled first run.\n"
          "\n"
          "    -quiet             : show only errors and final information\n"
          "\n"
          "    -quit              : quit when there is no new data\n"
          "\n"
          "        With this option, the program will terminate once a delay\n"
          "        in new data occurs.  This is most appropriate to use when\n"
          "        the image files have already been collected.\n"
          "\n"
          "    -start_file S_FILE : have %s process starting at S_FILE\n"
          "\n"
          "        e.g.  -start_file 043/I.901\n"
          "\n"
          "        With this option, any earlier I-files will be ignored\n"
          "        by %s.  This is a good way to start processing a later\n"
          "        run, if it desired not to look at the earlier data.\n"
          "\n"
          "        In this example, all files in directories 003 and 023\n"
          "        would be ignored, along with everything in 043 up through\n"
          "        I.900.  So 043/I.901 might be the first file in run 2.\n"
          "\n"
          "    -use_imon          : revert to Imon functionality\n"
          "\n"
          "    -version           : show the version information\n"
          "\n"
          "  ---------------------------------------------------------------\n"
          "  GERT_Reco2 options:\n"
          "\n"
          "    -GERT_Reco2        : output a GERT_Reco2 script\n"
          "\n"
          "        Create a script called 'GERT_Reco2', similar to the one\n"
          "        that Ifile creates.  This script may be run to create the\n"
          "        AFNI datasets corresponding to the I-files.\n"
          "\n"
          "    -gert_outdir OUTPUT_DIR  : set output directory in GERT_Reco2\n"
          "\n"
          "        e.g. -gert_outdir subject_A7\n"
          "        e.g. -od subject_A7\n"
          "        the default is '-gert_outdir afni'\n"
          "\n"
          "        This will add '-od OUTPUT_DIR' to the @RenamePanga command\n"
          "        in the GERT_Reco2 script, creating new datasets in the\n"
          "        OUTPUT_DIR directory, instead of the 'afni' directory.\n"
          "\n"
          "    -sp SLICE_PATTERN  : set output slice pattern in GERT_Reco2\n"
          "\n"
          "        e.g. -sp alt-z\n"
          "        the default is 'alt+z'\n"
          "\n"
          "        This options allows the user to alter the slice\n"
          "        acquisition pattern in the GERT_Reco2 script.\n"
          "\n"
          "        See 'to3d -help' for more information.\n"
          "\n"
          "  ---------------------------------------------------------------\n"
          "\n"
          "  Author: R. Reynolds - %s\n"
          "\n",
          prog, prog, prog, prog, prog, prog, prog,
          DIMON_VERSION
        );

        return 0;
    }
    else if ( level == IFM_USE_HIST )
    {
        fputs( g_history, stdout );
        return 0;
    }
    else if ( level == IFM_USE_VERSION )
    {
        printf( "%s: %s, compile date: %s\n",
                prog, DIMON_VERSION, __DATE__ );
        return 0;
    }

    fprintf( stderr, "error: usage() called with illegal level <%d>\n", level );

    return -1;
}


/* ----------------------------------------------------------------------
 * signal handler
 * ----------------------------------------------------------------------
*/
static void hf_signal( int signum )
{
    switch ( signum )
    {
        default :
            fprintf( stderr, "\nError: received unknown signal, %d\n",
                     signum );
            break;

        case SIGHUP  :
        case SIGINT  :
        case SIGTERM :
            show_run_stats( &gS );
            break;
    }

    if(gD.level > 1) fprintf(stderr, "\n+d received signal, %d\n", signum);

    if( gAC.state != ART_STATE_NO_USE )
    {
        if( gD.level > 1 )
            fprintf(stderr,"-d closing afni connection, state %d\n", gAC.state);
        ART_exit();
    }

    fflush(stderr);  /* in case it is redirected */
    fflush(stdout);

    exit(0);
}


/* ----------------------------------------------------------------------
 * set_volume_stats
 * ----------------------------------------------------------------------
*/
static int set_volume_stats( param_t * p, stats_t * s, vol_t * v )
{
    run_t * rp;           /* for a little speed, this will be called often */

    if ( v == NULL || v->seq_num < 0 || v->run < 0 )
    {
        fprintf( stderr, "failure: SVS - insufficient data\n\n" );
        idisp_vol_t ( "-- VOLUME FAILURE INFO : ", v );
    }

    /* initialize the stats structure */
    if ( s->nalloc == 0 )
    {
        s->runs = (run_t *)calloc( IFM_STAT_ALLOC, sizeof(run_t) );
        if ( s->runs == NULL )
        {
            fprintf( stderr, "failure: cannot allocate space for run info\n" );
            return -1;
        }

        /* first time caller - fill initial stats info */
        s->slices  = v->nim;
        s->z_first = v->z_first;
        s->z_last  = v->z_last;
        s->z_delta = v->z_delta;

        s->nalloc  = IFM_STAT_ALLOC;
        s->nused   = 0;
        s->nvols   = gP.opts.nt;        /* init with any user input value */

        if ( gD.level > 1 )
            fprintf( stderr, "\n-- svs: init alloc - vol %d, run %d, file %s\n",
                     v->seq_num, v->run, v->first_file );
    }

    if ( v->run >= s->nalloc )          /* run is 0-based */
    {
        /* make space for many more runs - we don't want to do this often */
        s->runs = (run_t *)realloc( s->runs, (v->run + IFM_STAT_ALLOC) *
                                             sizeof(run_t) );
        if ( s->runs == NULL )
        {
            fprintf( stderr, "failure: cannot realloc space for run info\n" );
            return -1;
        }

        s->nalloc = v->run + IFM_STAT_ALLOC;

        /* zero out any new memory */
        memset( s->runs + s->nused, 0, (s->nalloc - s->nused)*sizeof(run_t) );

        if ( gD.level > 1 )
            fprintf( stderr,
                "\n-- svs: realloc (%d entries) - vol %d, run %d, file %s\n",
                s->nalloc, v->seq_num, v->run, v->first_file );

    }

    /* we have memory - the current run number is an index into runs */

    rp = s->runs + v->run;

    if ( s->nused < v->run+1 )
        s->nused = v->run+1;

    if ( rp->volumes == 0 )
        strncpy( rp->f1name, v->first_file, IFM_MAX_FLEN );

    rp->volumes = v->seq_num;

    /* update nvols (if the user did not specify it and it is small) */
    if ( (p->opts.nt <= 0) && (s->nvols < v->seq_num) )
        s->nvols = v->seq_num;

    if ( gD.level > 2 )
        fprintf( stderr, "\n-- svs: run %d, seq_num %d\n", v->run, v->seq_num );

    return 0;
}


/* ----------------------------------------------------------------------
 * Create a gert_reco script.
 *
 * Note - stats struct parameters have been checked.
 * ----------------------------------------------------------------------
*/
static int create_gert_script( stats_t * s, opts_t * opts )
{
    FILE * fp;
    char * spat;                        /* slice acquisition pattern */
    char   cdir[4], csuff[IFM_SUFFIX_LEN];
    int    num_valid, c;

    /* if the user did not give a slice pattern string, use the default */
    spat = opts->sp ? opts->sp : IFM_SLICE_PAT;

    for ( c = 0, num_valid = 0; c < s->nused; c++ )
        if ( s->runs[c].volumes > 0 )
            num_valid++;

    if ( num_valid == 0 )
    {
        fprintf( stderr, "-- no runs to use for '%s'\n", IFM_GERT_SCRIPT );
        return 0;
    }

    if ( (fp = fopen( IFM_GERT_SCRIPT, "w" )) == NULL )
    {
        fprintf( stderr, "failure: cannot open '%s' for writing, "
                 "check permissions\n", IFM_GERT_SCRIPT );
        return -1;
    }

    /* output text casually, uh, borrowed from Ifile.c */
    fprintf( fp,
             "#!/bin/tcsh\n"
             "\n"
             "# This script was automatically generated by '%s'.\n"
             "# The script format was, uh, borrowed from Ziad's Ifile.c.\n"
             "#\n"
             "# Please modify the following options for your own evil uses.\n"
             "\n"
             "set OutlierCheck = '-oc'         # use '' to skip outlier check\n"
             "set OutPrefix    = 'OutBrick'    # prefix for datasets\n"
             "set OutputDir    = '-od %s'    # where to put output datasets\n"
             "\n"
             "\n",
             IFM_PROG_NAME,
             opts->gert_outdir ? opts->gert_outdir : "afni"
           );

    for ( c = 0; c < s->nused; c++ )
        if ( s->runs[c].volumes > 0 )
        {
            if ( path_to_dir_n_suffix(cdir, csuff, s->runs[c].f1name) < 0 )
            {
                fclose( fp );
                return -1;
            }

            fprintf( fp, "@RenamePanga %s %s %d %d $OutPrefix "
                         "-sp %s $OutlierCheck $OutputDir\n",
                     cdir, csuff, s->slices, s->runs[c].volumes, spat );
        }

    fputc( '\n', fp );
    fclose( fp );

    /* now make it an executable */
    system( "chmod u+x " IFM_GERT_SCRIPT );

    return 0;
}


/* ----------------------------------------------------------------------
 * - show statistics from the runs
 * - output any requested GERT_Reco2 file
 * ----------------------------------------------------------------------
*/
static int show_run_stats( stats_t * s )
{
    int c;

    if ( s == NULL )
    {
        fprintf( stderr, "failure, SRS - no stats struct!\n" );
        return -1;
    }

    if ( s->nalloc <= 0 || s->nused <= 0 )
        return 0;

    printf( "\n\n"
            "final run statistics:\n"
            "    volume info :\n"
            "        slices  : %d\n"
            "        z_first : %.4f\n"
            "        z_last  : %.4f\n"
            "        z_delta : %.4f\n"
            "\n",
            s->slices, s->z_first, s->z_last, s->z_delta );

    for ( c = 0; c < s->nused; c++ )
    {
        if ( s->runs[c].volumes > 0 )
            printf( "    run #%4d : volumes = %3d, first file = %s\n",
                    c, s->runs[c].volumes, s->runs[c].f1name );
    }

    putchar( '\n' );

    if ( gP.opts.gert_reco )
        (void)create_gert_script( s, &gP.opts );

    fflush( stdout );

    return 0;
}


/* ----------------------------------------------------------------------
 * given tr, compute a sleep time of approximate 2*TR
 * ----------------------------------------------------------------------
*/
static int nap_time_from_tr( float tr )
{
    float tr2 = 2 * tr;
    int   nap_time;

    if ( tr2 < 1 )
        return 1;

    if ( tr2 > 10 )
        return 10;                      /* ??? tres big */

    nap_time = (int)(tr2 + 0.9);

    if ( gD.level > 1 )
        fprintf(stderr,"-d computed nap_time is %d seconds (TR = %.2f)\n",
                nap_time, tr );

    return( nap_time );
}


/* ----------------------------------------------------------------------
 * find_next_zoff
 *
 * Given p->flist, search from index start for an image with
 * geh.zoff equal to zoff.
 *
 * return   index : upon succes         (start <= index <= p->nused)
 *             -1 : not found
 *             -2 : error
 * ----------------------------------------------------------------------
*/
static int find_next_zoff( param_t * p, int start, float zoff )
{
    int count;

    if ( (p == NULL) || (start < 0) )
        return -2;

    if ( start > p->nused )                     /* say not found */
        return -1;

    for ( count = start; count <= p->nused; count++ )
        if ( fabs( zoff - p->flist[count].geh.zoff ) < IFM_EPSILON )
            return count;                       /* found! */

    return -1;
}


/* ----------------------------------------------------------------------
 * Given:  run     > 0
 *         seq_num > 0
 *         naps
 *
 * If naps is too big, and the run is incomplete, print an obnoxious
 * warning message to the user.
 *
 * notes:   - print only 1 warning message per seq_num, per run
 *          - prev_run and prev_seq_num are for the previously found volume
 *
 * returns:
 *          2 : run is stalled - message printed
 *          1 : apparent end of a run
 *          0 : no stall, or if a message has already been printed
 *         -1 : function failure
 * ----------------------------------------------------------------------
*/
static int check_stalled_run ( int run, int seq_num, int naps, int nap_time )
{
    static int func_failure =  0;
    static int prev_run     = -1;
    static int prev_seq     = -1;

    if ( func_failure != 0 )
        return 0;

    if ( ( run < 1 ) || ( seq_num < 1 ) || ( naps <= IFM_MAX_RUN_NAPS ) )
        return 0;

    /* verify that we have already taken note of the previous volume */
    if ( (((gS.nused + 1) < run) || (gS.runs[run].volumes < seq_num)) &&
         ( func_failure == 0 ) )
    {
        fprintf( stderr, "** warning: CSR - stats inconsistancy!\n" );
        func_failure = 1;

        return -1;
    }

    if ( seq_num < gS.nvols )      /* are we done with the run yet? */
    {
        /* if we haven't printed before, this is the first stalled case */
        if ( (run != prev_run) || (seq_num != prev_seq) )
        {
            fprintf( stderr, "\007\n"
                     "****************************************************\n"
                     "Warning: run seems to be stalled\n"
                     "\n"
                     "    run                    : %d\n"
                     "    TRs completed          : %d (of %d)\n"
                     "    approximate idle time  : %d seconds\n"
                     "    first file of this run : %s\n"
                     "****************************************************\n",
                     run, seq_num, gS.nvols,
                     naps*nap_time, gS.runs[run].f1name );

            prev_run = run;
            prev_seq = seq_num;

            return 2;
        }
    }
    /* else (we are done) */
    else if ( (run != prev_run) || (seq_num != prev_seq) )
    {
        /* this is our first visit, note the fact */
        prev_run = run;
        prev_seq = seq_num;

        return 1;
    }

    return 0;
}

/*-------------------------------------------------------*/
/*! Return the file length (-1 if file not found).       */
/*  (local copy from thd_filestuff.c                     */

unsigned long l_THD_filesize( char * pathname )
{
    static struct stat buf ; int ii ;

    if( pathname == NULL ) return -1 ;
        ii = stat( pathname , &buf ) ; if( ii != 0 ) return -1 ;

    return buf.st_size ;
}

/* ----------------------------------------------------------------------
 * Make sure im_ary has enough memory for num_images pointers.
 *
 * return  -1 : on error
 *          0 : nothing needed
 *          1 : memory successfully created
 * ----------------------------------------------------------------------
*/
static int check_im_store_space( im_store_t * is, int num_images )
{
    if ( (is == NULL) || (num_images <= 0) )
    {
        fprintf( stderr, "** CISS: invalid parameters (%p,%d)\n",
                 is, num_images );
        return -1;
    }

    if ( is->ary_len >= num_images )
        return 0;

    /* so we need memory */

    if ( gD.level > 2 )
        fprintf( stderr, "++ allocating %d image pointers (was %d)\n",
                 num_images, is->ary_len );

    is->im_ary = realloc(is->im_ary, num_images * sizeof(void *));

    if ( is->im_ary == NULL )
    {
        fprintf( stderr, "** failure: cannot allocate %d image pointers\n",
                 num_images );
        return -1;
    }

    /* clear the new pointers */
    memset(is->im_ary+is->ary_len, 0, (num_images-is->ary_len)*sizeof(void*));

    is->ary_len = num_images;

    return 1;
}


/* ----------------------------------------------------------------------
 * Set the im_size and allocate memory for x_im.
 *
 * return  -1 : on error
 *          0 : success
 * ----------------------------------------------------------------------
*/
static int alloc_x_im( im_store_t * is, int bytes )
{
    if ( (is == NULL) || (bytes <= 0) )
    {
        fprintf( stderr, "** bad params to AXI (%p,%d)\n", is, bytes );
        return -1;
    }

    is->im_size = bytes;

    if ( (is->x_im = malloc( bytes )) == NULL )
    {
        fprintf( stderr, "** AXI: failed to malloc %d bytes for x_im\n",
                 bytes );
        return -1;
    }

    if ( gD.level > 1 )
        fprintf( stderr, "++ allocating %d bytes for is->x_im\n", bytes );

    return 0;
}


/* ----------------------------------------------------------------------
 * Determine the byte order of the image.
 *
 * Note our byte order (LSB_FIRST or MSB_FIRST).
 * If gex.swap is set, reverse it.  If user wants opposite, reverse it.
 *
 * return   0 : success
 *         -1 : on error
 * ----------------------------------------------------------------------
*/
static int check_im_byte_order( int * order, vol_t * v, param_t * p )
{
    int one = 1;

    if ( (order == NULL) || (v == NULL) || (p == NULL) )
    {
        fprintf( stderr, "** invalid paramters to CIBO (%p,%p,%p)\n",
                 order, v, p );
        return -1;
    }

    /* note the order for the current system */
    *order = (*(char *)&one == 1) ? LSB_FIRST : MSB_FIRST;

    if ( gD.level > 1 )
        fprintf( stderr, "-- system order is %s, ",
                 (*order == MSB_FIRST) ? "MSB_FIRST" : "LSB_FIRST" );

    /* are the images the opposite of this?  does the user want the opposite? */
    if ( p->flist[v->fl_1].gex.swap ^ p->opts.rev_bo )
        *order = LSB_FIRST + MSB_FIRST - *order;      /* for entertainment */

    if ( gD.level > 1 )
        fprintf( stderr, "image order is %s\n",
                 (*order == MSB_FIRST) ? "MSB_FIRST" : "LSB_FIRST" );

    return 0;
}


/* ----------------------------------------------------------------------
 * Use gex.kk to figure out the z orientation, and complete
 * the v->geh.orients string.
 *
 * orient(kk) = { LR, if kk = 1
 *              { PA, if kk = 2
 *              { IS, if kk = 3
 *
 * return   0 : success
 *         -1 : on error
 * ----------------------------------------------------------------------
*/
static int complete_orients_str( vol_t * v, param_t * p )
{
    int kk;

    if ( (v == NULL) || (p == NULL) )
    {
        fprintf( stderr, "** invalid paramters to COS (%p,%p)\n", v, p );
        return -1;
    }

    if ( gD.level > 2 )
        fprintf(stderr,"completing orients from '%s' to", v->geh.orients);

    if ( p->ftype == IFM_IM_FTYPE_DICOM )
        strncpy(v->geh.orients + 4, DI_MRL_orients + 4, 2 );
    else
    {
        kk = p->flist[v->fl_1].gex.kk;

        switch( kk )
        {
            case 1:                                     /* LR */
                if ( v->z_delta > 0 )
                {
                    v->geh.orients[4] = 'L';
                    v->geh.orients[5] = 'R';
                }
                else
                {
                    v->geh.orients[4] = 'R';
                    v->geh.orients[5] = 'L';
                }
                break;

            case 2:                                     /* PA */
                if ( v->z_delta > 0 )
                {
                    v->geh.orients[4] = 'P';
                    v->geh.orients[5] = 'A';
                }
                else
                {
                    v->geh.orients[4] = 'A';
                    v->geh.orients[5] = 'P';
                }
                break;
                
            case 3:                                     /* IS */
                if ( v->z_delta > 0 )
                {
                    v->geh.orients[4] = 'I';
                    v->geh.orients[5] = 'S';
                }
                else
                {
                    v->geh.orients[4] = 'S';
                    v->geh.orients[5] = 'I';
                }
                break;
                
            default:
            {
                fprintf(stderr, "** COS failure: kk (%d) not in [1,3]\n", kk);
                return -1;
            }
        }
    }

    v->geh.orients[6] = '\0';

    if ( gD.level > 2 ) fprintf(stderr,"'%s'\n", v->geh.orients);
                                
    return 0;
}


/* ----------------------------------------------------------------------
 * return the index of 'file' in the directory list
 *
 * We can have any combination of start_dir and start_file.
 *
 * return  -1   : failed to find it
 *         else : index
 * ----------------------------------------------------------------------
*/
static int find_fl_file_index( param_t * p )
{
    char ** nlp;
    char  * sd, * sf;
    int     index, found = 0;
    int     dlen;

    if ( ! p || (! p->opts.start_file && ! p->opts.start_dir) )
        return 0;

    sd = p->opts.start_dir;
    sf = p->opts.start_file;

    if ( gD.level > 2 )
        fprintf(stderr,"-d searching for initial dir, file: %s, %s\n",
                sd ? sd : "<no start_dir>", sf ? sf : "<no start_file>");

    index = 0;       /* init search params */
    nlp = p->fnames;

    if ( sd )
    {
        dlen = strlen(sd);
        for ( ; index < p->nfiles; index++, nlp++ )
            if ( ! strncmp(*nlp, sd, dlen) ) { found = 1; break; }
    }

    if ( sf )  /* then further continue the search until the file is found */
    {
        found = 0;
        for ( ; index < p->nfiles; index++, nlp++ )
            if ( ! strcmp(*nlp, sf) ) { found = 1; break; }
    }

    if ( gD.level > 2 )
    {
        if(found) fprintf(stderr,"-d found match at entry %d: %s\n",index,*nlp);
        else      fprintf(stderr,"-d no match found (yet)...\n");
    }

    if ( found ) return index;
    else         return -1;
}

/* ----------------------------------------------------------------------
 * return the number of occurances of 'target' in 'str' of length 'len'
 * ----------------------------------------------------------------------
*/
static int str_char_count( char * str, int len, char target )
{
    char * cp;
    char * last = str + len;
    int    num = 0;

    if ( (str == NULL) || (len <= 0) )
        return 0;

    for ( cp = str; cp < last; cp++ )
        if ( *cp == target )
            num++;

    return num;
}


/* ----------------------------------------------------------------------
 * Given path, find dir and suff.
 *
 * dir    - 3 character starting directory           (e.g. 003)
 * suff   - 10 char (max) trailing I-file suffix     (e.g. 017, for I.017)
 * path   - first file in run                        (e.g. 003/I.017)
 *
 * return  0 : success
 *        -1 : failure
 * ----------------------------------------------------------------------
*/
static int path_to_dir_n_suffix( char * dir, char * suff, char * path )
{
    char * cp, *cp2;

    if ( (dir == NULL) || (suff == NULL) || (path == NULL) )
    {
        fprintf( stderr, "failure: PTDNS - invalid params (%p,%p,%p)\n",
                 dir, suff, path );
        return -1;
    }

    /* find last '.' */
    for ( cp = path + strlen(path) - 1; (*cp != '.') && (cp > path); cp-- )
        ;

    if ( *cp != '.' )
    {
        fprintf( stderr, "failure: cannot find suffix in '%s'\n", path );
        return -1;
    }
    else if ( strlen( cp ) > IFM_SUFFIX_LEN )          /* '.' not included */
    {
        fprintf( stderr, "failure: suffix too long in '%s'\n", path );
        return -1;
    }

    strcpy( suff, cp+1 );               /* copy null-terminated suffix */

    /* make sure all characters are digits (treat as string, not int) */
    for ( cp2 = suff; (*cp2 != '\0') && isdigit(*cp2); cp2++ )
       ;

    if ( *cp2 != '\0' )
    {
        fprintf( stderr, "failure: suffix not integer in '%s'\n", path );
        return -1;
    }

    /* now get dir prefix - should be nnn/I.mmm */
    cp -= 5;
    if ( ( cp < path )          ||    /* we should have a directory here */
         ( ! isdigit( cp[0] ) ) ||    /* then 3 digits */
         ( ! isdigit( cp[1] ) ) ||
         ( ! isdigit( cp[2] ) ) ||
         (   cp[3] != '/'     ) ||
         (   cp[4] != 'I'     ) )
    {
        fprintf( stderr, "failure: PTDNS - ill-formed path '%s'\n", path );
        return -1;
    }

    /* we are set, just copy the data */
    strncpy( dir, cp, 3 );
    dir[3] = '\0';

    return 0;
}


/* ----------------------------------------------------------------------
 * Add given string (pointer) to list.                       v3.3 [rickr]
 *
 * return  0 : success
 *        -1 : failure (realloc)
 * ----------------------------------------------------------------------
*/
static int add_to_string_list( string_list * list, char * str )
{
    if ( !list || !str )
        return -1;

    /* if needed, just add 10 at a time to nalloc (they're only pointers) */
    if ( list->nalloc == 0 || (list->nalloc <= list->nused) )
    {
        list->nalloc += 10;
        list->str = (char **)realloc(list->str, list->nalloc*sizeof(char *));
        if ( !list->str )
        {
            fprintf(stderr,"** failed to allocate for %d (char *)s\n",
                    list->nalloc);
            return -1;
        }
        if( gD.level > 2 )
            fprintf(stderr,"+d realloc %d (char *)'s\n",list->nalloc);
    }

    list->str[list->nused] = str;
    list->nused++;

    return 0;
}


/* ----------------------------------------------------------------------
 * If free_mem, free memory.  Set contents as empty.         v3.3 [rickr]
 * ----------------------------------------------------------------------
*/
static int empty_string_list( string_list * list, int free_mem )
{
    if ( list->str && free_mem )
        free( list->str );

    list->str    = NULL;
    list->nalloc = 0;
    list->nused  = 0;

    return 0;
}


