
#define IFM_VERSION "version 2.0 (January, 2003)"

/*----------------------------------------------------------------------
 * history:
 *
 * 2.0  January 15, 2003
 *   - rtfeedme feature
 *       o added -rt   option: pass data to afni as collected
 *       o added -host option: specify afni host for real-time
 *       o added -swap option: byte swap pairs before sending to afni
 *       o created realtime.[ch] for all RT processing functions
 *       o (see gAC struct and ART_ functions)
 *   - actually read and store images (to be sent to afni)
 *   - moved function declarations to Imon.c (from Imon.h)
 *
 * 1.3  December 13, 2002
 *   - compile as standalone (include mcw_glob, but not mcw_malloc)
 *   - added l_THD_filesize (local copy of THD_filesize)
 *   - removed dependance on mrilib.h and r_idisp.h
 *
 * 1.2  November 27, 2002
 *   - after N idle mid-run TRs, print warning message
 *   - added '-nice INCR' option
 *   - added BEEP on error
 *   - replaced '-status' with '-quiet', so '-debug 1' is default
 *   - no fatal error during volume search, try to recover
 *   - display that the user should use <ctrl-c> to quit
 *   - adjust globbing to be "...[0-9][02468]?/I.*"  (or w/[13579])
 *
 * 1.1  November 27, 2002
 *   - renamed from Hfile to Imon (I-file monitor)
 *
 * 1.0  November 21, 2002
 *   - initial release
 *----------------------------------------------------------------------
*/

/*----------------------------------------------------------------------
 * todo:
 *
 * - add -host and -swap options
 * - check for missing first slice
 * - add '-first_file FILE' option, for a new starting point
 *----------------------------------------------------------------------
*/

/*----------------------------------------------------------------------
 * Imon - monitor real-time aquisition of I-files
 *
 *     This program is intended to be run during a scanning session
 *     on a GE scanner, to monitor the collection of I-files.  The
 *     user will be notified of any missing slice or any slice that
 *     is aquired out of order.
 *
 *     It is recommended that the user runs 'Imon' before scanning
 *     begins, and then watches for error messages during the
 *     scanning session.  The user should terminate the program
 *     whey they are done with all runs.
 *
 *     At the present time, the user must use <ctrl-c> to terminate
 *     the program.
 *
 *   usage: Imon [options] -start_dir DIR
 *
 *   examples:    Imon -start_dir 003
 *                Imon -help
 *                Imon -version
 *                Imon -debug 2 -start_dir 003
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

#include "Imon.h"
#include "l_mcw_glob.h"
#include "thd_iochan.h"
#include "realtime.h"

/*----------------------------------------------------------------------*/
/* static function declarations */

static int alloc_x_im          ( im_store_t * is, int bytes );
static int check_im_store_space( im_store_t * is, int num_images );
static int check_stalled_run   ( int run, int seq_num, int naps, int nap_time );
static int complete_orients_str( vol_t * v, param_t * p );
static int dir_expansion_form  ( char * sin, char ** sexp );
static int find_first_volume   ( vol_t * v, param_t * p, ART_comm * ac );
static int find_more_volumes   ( vol_t * v, param_t * p, ART_comm * ac );
static int find_next_zoff      ( param_t * p, int start, float zoff );
static int init_extras         ( param_t * p, ART_comm * ac );
static int init_options        ( param_t * p, ART_comm * a, int argc,
				 char * argv[] );
static int nap_time_from_tr    ( float tr );
static int read_ge_files       ( param_t * p, int next, int max );
static int read_ge_image       ( char * pathname, finfo_t * fp,
	                         int get_image, int need_memory );
static int scan_ge_files       ( param_t * p, int next, int nfiles );
static int set_nice_level      ( int level );
static int set_volume_stats    ( vol_t * v );
static int show_run_stats      ( stats_t * s );
static int swap_4              ( void * ptr );

static void hf_signal          ( int signum );

/* volume scanning */
static int volume_match  ( vol_t * vin, vol_t * vout, param_t * p, int start );
static int volume_search ( vol_t * V, param_t * p, int * start, int maxsl );

/* information functions */
static int idisp_hf_param_t     ( char * info, param_t * p );
static int idisp_hf_vol_t       ( char * info, vol_t * v );
static int idisp_ge_extras      ( char * info, ge_extras * E );
static int idisp_ge_header_info ( char * info, ge_header_info * I );
static int idisp_im_store_t     ( char * info, im_store_t * is );

static int usage                ( char * prog, int level );

/* local copy of AFNI function */
static unsigned long l_THD_filesize( char * pathname );

/*----------------------------------------------------------------------*/

#define MAIN

/***********************************************************************/
/* globals */

IFM_debug gD;		/* debug information         */
param_t   gP;		/* main parameter struct     */
stats_t   gS;		/* general run information   */
ART_comm  gAC;		/* afni communication struct */

/***********************************************************************/
int main( int argc, char * argv[] )
{
    ART_comm * ac = &gAC;		/* access AFNI comm as local   */
    param_t  * p  = &gP;		/* access the global as local  */
    vol_t      baseV;			/* base volume - first scanned */
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
    int start = 0;  /* initial starting location for the first volume */

    if ( gD.level > 0 ) 		/* status */
        fprintf( stderr, "-- scanning for first volume\n" );

    ret_val = 0;
    while ( ret_val == 0 )
    {
	ret_val = read_ge_files( p, 0, max_im_alloc );

	if ( ret_val > 0 )
	{
	    ret_val = volume_search( v, p, &start, 0 );

	    if ( ret_val == -1 )   /* try to recover from a data error */
		ret_val = 0;
	    else if ( (ret_val == 0) && (max_im_alloc < (2*p->nused)) )
	    {
		/* If we don't have a volume yet, but have used "too much"
		 * of our available memory, request more, making sure there
		 * is enough for a volume, despite the previous max_im_alloc
		 * limitation.
		 */
		max_im_alloc *= 2;
	    }
	}

	if ( ret_val == 0 )			/* we are not done yet */
	{
	    if ( gD.level > 0 ) 		             /* status */
		fprintf( stderr, "." );

	    sleep( 4 );					  /* nap time! */
	}
	else if ( ret_val > 0 )		/* success - we have a volume! */
	{
	    if ( gD.level > 0 )
	    {
		fprintf( stderr, "\n-- first volume found\n" );
		if ( gD.level > 1 )
		{
		    idisp_hf_vol_t( "first volume : ", v );
		    idisp_hf_param_t( "first vol - new params : ", p );
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
		    idisp_hf_param_t( "++ final realloc of flist : ", p );
	    }

	    /* use this volume to comple the geh.orients string */
	    if ( complete_orients_str( v, p ) < 0 )
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
	    return ret_val;		/* fatal error condition */
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
    int   fl_index;			/* current index into p->flist    */
    int   naps;				/* keep track of consecutive naps */
    int   nap_time;			/* sleep time, in seconds         */

    if ( v0 == NULL || p == NULL )
    {
	fprintf( stderr, "error: HF:FMV() lacking parameters\n" );
	return -1;
    }

    done     = 0;
    naps     = 0;

    run      = v0->run;
    seq_num  = v0->seq_num = 1;		/* set first seq_num to 1 */
    fl_index = v0->fn_n + 1;		/* start looking past first volume */
    next_im  = v0->fn_n + 1;		/* for read_ge_files()             */

    nap_time = nap_time_from_tr( v0->geh.tr );

    if ( gD.level > 0 ) 		/* status */
    {
        fprintf( stderr, "-- scanning for additional volumes...\n" );
        fprintf( stderr, "-- run %d: %d ", run, seq_num );
    }

    /* give stats when user quits */
    signal( SIGTERM, hf_signal );
    signal( SIGINT,  hf_signal );

    if ( set_volume_stats( v0 ) )
	return -1;

    while ( ! done )
    {
	/* check all of the volumes we've already scanned in */
	ret_val = 1;
	while ( (ret_val == 1) || (ret_val == -1) )
	{
	    ret_val = volume_match( v0, &vn, p, fl_index );

	    if ( ret_val < -1 )			/* bail out on fatal error */
		return ret_val;

	    if ( (ret_val == 1) || (ret_val == -1) )
	    {
		if ( gD.level > 2 )
		    idisp_hf_vol_t( "-- new volume: ", &vn );

		fl_index += vn.nim;		/* note the new position   */
		next_im   = vn.fn_n + 1;	/* for read_ge_files()     */

		if ( vn.run != run )		/* new run?                */
		{
		    /* pass run and seq_num before they are updated */
		    if ( ac->state == ART_STATE_IN_USE )
			ART_send_end_of_run( ac, run, seq_num, gD.level );

		    run = vn.run;		/* reset                   */
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

		if ( set_volume_stats( &vn ) )
		    return -1;

		if ( complete_orients_str( &vn, p ) < 0 )
		    return -1;

		if ( ac->state == ART_STATE_TO_SEND_CTRL )
		    ART_send_control_info( ac, &vn, gD.level );

		if ( ac->state == ART_STATE_IN_USE )
		    ART_send_volume( ac, &vn, gD.level );

		naps = 0;			/* reset on existing volume */
	    }
	}

	/* now we need new data - skip past last file name index */

	ret_val = read_ge_files( p, next_im, p->nalloc );
	fl_index = 0;			/* reset flist index          */

	while ( (ret_val >= 0 ) &&	/* no fatal error, and        */
	        (ret_val < v0->nim) )	/* didn't see full volume yet */
	{
	    if ( naps > 0 )
	    {
		/* continue, regardless */
		if ( check_stalled_run( run, seq_num, naps, nap_time ) > 0 )
		    if ( ac->state == ART_STATE_IN_USE )
			ART_send_end_of_run( ac, run, seq_num, gD.level );

		if ( gD.level > 0 ) 	/* status */
		    fprintf( stderr, ". " );
	    }

	    sleep( nap_time );		/* wake after a couple of TRs */
	    naps ++;

	    ret_val = read_ge_files( p, next_im, p->nalloc );
	}

	if ( ret_val < 0 )		/* aaaaagh!  panic!  wet pants! */
	{
	    fprintf( stderr, "\n** failure: HF:RGF fatal error\n" );
	    return -1;
	}
    }

    return 0;	/* success */
}


/*----------------------------------------------------------------------
 * volume_search:   scan p->flist for a complete volume
 *
 *     - *start should be at the expected beginning of a volume!
 *     - *start may be adjusted to begin with next volume
 * 
 * return:   -2 : on programming error
 *           -1 : on data error
 *            0 : on success - no volume yet
 *            1 : on success - volume read
 *----------------------------------------------------------------------
*/
static int volume_search(
	vol_t   * V,			/* volume to fill                */
	param_t * p,			/* master structure              */
	int     * start,		/* starting index into p->flist  */
        int       maxsl	)		/* max number of slices to check */
{
    finfo_t * fp;
    float     z_orig, delta, dz, prev_z;
    int       bound;			/* upper bound on image slice  */
    int       next;
    int       run0, run1;		/* run numbers, for comparison */
    int       first = *start;		/* first image (start or s+1)  */
    int       last;                     /* final image in volume       */

    if ( V == NULL || p == NULL || p->flist == NULL || *start < 0 )
    {
	fprintf( stderr, "failure: FNV: bad parameter data\n" );
	return -2;
    }

    /* note the bound on the slice index */
    if ( (maxsl <= 0) || ((maxsl + first) >= p->nused) )
	bound = p->nused;
    else
	bound = first + maxsl;

    if ( (bound - first) < 4 )		/* not enough data to work with   */
	return 0;

    delta = p->flist[first+1].geh.zoff - p->flist[first].geh.zoff;

    run0  = p->flist[first  ].geh.uv17;
    run1  = p->flist[first+1].geh.uv17;

    /* if apparent 1-slice volume, skip and start over */
    if ( (fabs(delta) < IFM_EPSILON) || (run1 != run0) )
    {
	if ( gD.level > 1 )
	    fprintf( stderr, "-- skipping single slice volume <%s>\n",
		     p->fnames[p->flist[first].index] );

	first++;
	delta = p->flist[first+1].geh.zoff - p->flist[first].geh.zoff;
	run0  = run1;

	if ( fabs(delta) < IFM_EPSILON )
	{
	    fprintf( stderr, "Error: 3 slices with 0 delta, beginning with"
		     "file <%s>\n", p->fnames[p->flist[*start].index] );

	    *start = *start + 2;	       /* try to skip and recover */

	    return -1;
	}
    }

    fp     = p->flist + first;			/* initialize flist posn  */
    z_orig = fp->geh.zoff;			/* note original position */

    /* set current values at position (first+1) */
    fp++;
    prev_z = fp->geh.zoff;
    run1   = fp->geh.uv17;
    dz     = delta;

    /* scan for volume break */
    next = first + 2;				/* next z to look at      */
    while ( (next < bound) && (fabs(dz - delta) < IFM_EPSILON) &&
	    (run1 == run0) )
    {
	fp++;				  /* good index so get new values */

	dz     = fp->geh.zoff - prev_z;
	run1   = fp->geh.uv17;
	prev_z = fp->geh.zoff;

	next++;
    }

    last = next - 2;			    /* note final image in volume */

    if ( (fabs(dz - delta) < IFM_EPSILON) && (run1 == run0) )
	return 0;			    /* we did not finish a volume */
    else if ( abs(p->flist[first].geh.zoff - fp->geh.zoff) > IFM_EPSILON )
    {
	/* the next slice does not match the original - interleaving? */
	int testc;

	for ( testc = next - 1; testc < bound; testc++ )
	    if ( abs( p->flist[first].geh.zoff -
		      p->flist[testc].geh.zoff ) < IFM_EPSILON )
	    {
		/* aaaaagh!  we are missing data from the first volume!   */
		/* print error, and fix start (try to skip this volume)   */
		fprintf( stderr, "\n"
			"*************************************************\n"
			"Error: missing slice in first volume!\n"
			"       detected    at file: %s\n"
			"       re-starting at file: %s\n"
			"*************************************************\n",
		       	p->fnames[last+1], p->fnames[testc] );

	        /* try to skip this volume and recover */
		*start = p->flist[testc].index;

		return -1;
	    }

	return 0;		       /* not done, volume is interleaved */
    }

    /* we have a volume! */

    /* So deltas are consistent from slice 'first' to slice 'last'. */

    V->geh      = p->flist[first].geh;		/* copy GE structure      */
    V->nim      = last - first + 1;
    V->fl_1     = first;
    V->fn_1     = p->flist[first].index;
    V->fn_n     = p->flist[last].index;
    strncpy( V->first_file, p->fnames[V->fn_1], IFM_MAX_FLEN );
    strncpy( V->last_file,  p->fnames[V->fn_n],  IFM_MAX_FLEN );
    V->z_first  = p->flist[first].geh.zoff;
    V->z_last   = p->flist[last].geh.zoff;
    V->z_delta  = delta;
    V->seq_num  = -1;				/* uninitialized */
    V->run      = V->geh.uv17;

    return 1;
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
    finfo_t * fp;
    finfo_t * fp_test;
    float     z;
    int       count, next_start = -1;
    int       missing = 0;

    if ( vin == NULL || vout == NULL ||
	 p == NULL || p->flist == NULL || start < 0 )
    {
	fprintf( stderr, "failure: FMV: bad parameter data\n" );
	return -2;
    }

    if ( (p->nused - start) < vin->nim )	/* enough files to scan? */
	return 0;

    /* now 'everything' should match */

    fp = p->flist+start;
    for ( count = 0; count < vin->nim - 1; count++ )   /* last is separate */
    {
	z = vin->z_first + count * vin->z_delta; 	/* note expected z */

	if ( fabs( z - fp->geh.zoff ) > IFM_EPSILON )
	{
	    /* slice is either missing or out of order */

	    fp_test = fp + 1;			       /* check next image */
	    if ( fabs( z + vin->z_delta - fp_test->geh.zoff ) < IFM_EPSILON )
	    {
		/* next slice as expected, so current is out of order */
		/* nothing to do but warn the user and continue */

		IFM_BIG_ERROR_MESG( "slice out of order!",
			p->fnames[fp->index], z, fp->geh.zoff,
			fp->geh.uv17, count + 1, vin->nim );
	    }
	    else if ( fabs(z + vin->z_delta - fp->geh.zoff) < IFM_EPSILON )
	    {
		/* current slice matches next expected - slice missing */

		/* nothing to do but note error, warn user and continue */
		missing++;

		IFM_BIG_ERROR_MESG( "slice missing!",
			p->fnames[fp->index], z, fp->geh.zoff,
			fp->geh.uv17, count + 1, vin->nim );

		count++;    /* attempt to continue by skipping this slice */
	    }
	    else	/* unknown error - find start of next volume */
	    {
		/* search for a next starting point */
		next_start = find_next_zoff( p, start+count+1, vin->z_first );

		if ( next_start < 0 )	/* come back and try again later */
		    return 0;
		else
		{
		    IFM_BIG_ERROR_MESG( "volume severely toasted!",
			    p->fnames[fp->index], z, fp->geh.zoff,
			    fp->geh.uv17, count + 1, vin->nim );

		    break;	/* terminate for loop and try to recover */
		}
	    }
	}

	fp++;
    }

    /* check last slice - count and fp should be okay*/
    z = vin->z_first + count * vin->z_delta;	      /* note expected z   */

    if ( (next_start < 0) && (fabs( z - fp->geh.zoff ) > IFM_EPSILON) )
    {
	if ( (p->nused - start) <= vin->nim )   /* no more images to check */
	    return 0;                           /* wait for more data      */
	
	fp_test = fp + 1;			       /* check next image */
	if ( fabs( vin->z_first - fp_test->geh.zoff ) < IFM_EPSILON )
	{
	    /* next image starts next run, slice is probably out of order */

	    IFM_BIG_ERROR_MESG( "slice out of order!",
		    p->fnames[fp->index], z, fp->geh.zoff,
		    fp->geh.uv17, count + 1, vin->nim );
	}
	else if ( fabs(vin->z_first - fp->geh.zoff) < IFM_EPSILON )
	{
	    /* this image starts next run, slice is missing */
	    missing++;

	    IFM_BIG_ERROR_MESG( "slice missing!",
		    p->fnames[fp->index], z, fp->geh.zoff,
		    fp->geh.uv17, count + 1, vin->nim );
	}
	else	/* unknown error - find start of next volume */
	{
	    /* search for a next starting point */
	    next_start = find_next_zoff( p, start+count+1, vin->z_first );

	    if ( next_start < 0 )	/* come back and try again later */
		return 0;
	    else
	    {
		IFM_BIG_ERROR_MESG( "Volume severely toasted!",
			p->fnames[fp->index], z, fp->geh.zoff,
			fp->geh.uv17, count + 1, vin->nim );
	    }
	}
    }
    else if ( next_start < 0)
        next_start = start + vin->nim - missing;

    /* fill volume structure */

    vout->geh      = p->flist[start].geh;
    vout->nim      = next_start - start;
    vout->fl_1     = start;
    vout->fn_1     = p->flist[start].index;
    vout->fn_n     = p->flist[start+vout->nim-1].index;
    strncpy( vout->first_file, p->fnames[vout->fn_1], IFM_MAX_FLEN );
    strncpy( vout->last_file,  p->fnames[vout->fn_n],  IFM_MAX_FLEN );
    vout->z_first  = vin->z_first;
    vout->z_last   = vin->z_last;
    vout->z_delta  = vin->z_delta;
    vout->seq_num  = -1;				/* uninitialized */
    vout->run      = vout->geh.uv17;

    if ( vout->nim != vin->nim )
	return -1;
    else
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
	param_t * p,		/* parameter scruct                */
	int       next,		/* index of next file to scan from */
	int       max )		/* max number of files to scan     */
{
    int n2scan;			/* number of files to actually scan */

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

    /* get files */
    MCW_file_expand( 1, &p->glob_dir, &p->nfiles, &p->fnames );
    
    if ( gD.level > 2 )
    {
	int fnum;
	for ( fnum = next; fnum < p->nfiles; fnum++ )
	    printf( "file %4d: %s\n", fnum, p->fnames[fnum] );
    }

    if ( p->nfiles <= 0 )
	return 0;

    /* set the number of files to scan - if max is usable, use it */
    if ( (max > 0) && (max <= (p->nfiles - next)) )
	n2scan = max;				/* scan next max files */
    else
	n2scan = p->nfiles - next;		/* scan rest of files  */

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
	    idisp_hf_param_t( "++ realloc of flist : ", p );
    }

    p->nused = scan_ge_files( p, next, n2scan );

    if ( gD.level > 2 )
	idisp_hf_param_t( "end read_ge_files : ", p );

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
	param_t  * p,			/* general parameter structure */
	int        next,		/* index of next file to scan  */
	int        nfiles )		/* number of files to scan     */
{
    static int   read_failure = -1;	/* last read_ge_image failure */
    finfo_t    * fp;
    int          im_num, fnum;
    int          files_read, rv;
    int          need_M;		/* do we need image memory?    */

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
	if ( im_num < p->im_store.nalloc )	/* then we have image memory */
	{
	    fp->image = p->im_store.im_ary[im_num];
	    need_M    = 0;
	}
	else					/* get it from read_ge_image */
	{
	    fp->image = NULL;
	    need_M    = 1;
	}

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
	    if ( read_failure != fnum )
	    {
		/* first time to fail with this file - wait and try again */
		if ( gD.level > 1 )
		    fprintf( stderr, "\n** failure to read GE header for "
			     "file <%s>\n", p->fnames[fnum] );
		read_failure = fnum;

		break;
	    }
	    else
	    {
		fprintf( stderr, "\nfailure: cannot read GE header for "
			 "file <%s>\n", p->fnames[fnum] );
		return -1;
	    }
	}
	else
	{
	    p->im_store.nused++;	/* keep track of used images     */
	    fp->index = fnum;		/* store index into fnames array */

	    if ( gD.level > 2 )
		idisp_ge_header_info( p->fnames[fp->index], &fp->geh );
	}
    }

    /* even on failure, this non-negative integer is accurate */
    files_read = fnum - next;

    if ( gD.level > 1 )
	printf( "-- scanned %d GE files, from <%s> to <%s>\n",
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

    memset(  p,  0, sizeof(*p)  );	/* parameters       */
    memset( &gD, 0, sizeof(gD)  );	/* debug struct     */
    memset( &gS, 0, sizeof(gS)  );	/* stats struct     */
    memset(  A,  0, sizeof(gAC) );	/* afni comm struct */

    ART_init_AC_struct( A );		/* init for no real-time comm */
    A->param = p;			/* store the param_t pointer  */

    /* debug level 1 is now the default - by order of Wen-Ming :) */
    gD.level = 1;

    for ( ac = 1; ac < argc; ac++ )
    {
	if ( ! strncmp( argv[ac], "-help", 5 ) )
	{
	    usage( IFM_PROG_NAME, IFM_USE_LONG );
	    return 1;
	}
	else if ( ! strncmp( argv[ac], "-version", 2 ) )
	{
	    usage( IFM_PROG_NAME, IFM_USE_VERSION );
	    return 1;
	}
	else if ( ! strncmp( argv[ac], "-start_dir", 6 ) )
	{
	    if ( ++ac >= argc )
	    {
		fputs( "option usage: -start_dir DIRECTORY\n", stderr );
		usage( IFM_PROG_NAME, IFM_USE_SHORT );
		return 1;
	    }

	    p->start_dir = argv[ac];
	}
	else if ( ! strncmp( argv[ac], "-debug", 4 ) )
	{
	    if ( ++ac >= argc )
	    {
		fputs( "option usage: -debug LEVEL\n", stderr );
		usage( IFM_PROG_NAME, IFM_USE_SHORT );
		return 1;
	    }

	    gD.level = atoi(argv[ac]);
	    if ( gD.level < 0 || gD.level > IFM_MAX_DEBUG )
	    {
		fprintf( stderr, "error: debug level must be in [0,%d]\n",
			 IFM_MAX_DEBUG );
		errors++;
	    }
	}
	else if ( ! strncmp( argv[ac], "-nice", 4 ) )
	{
	    if ( ++ac >= argc )
	    {
		fputs( "option usage: -nice INCREMENT\n", stderr );
		usage( IFM_PROG_NAME, IFM_USE_SHORT );
		return 1;
	    }

	    p->nice = atoi(argv[ac]);
	    if ( p->nice < IFM_MIN_NICE_INC || p->nice > IFM_MAX_NICE_INC )
	    {
		fprintf( stderr, "error: nice incrment must be in [%d,%d]\n",
			 IFM_MIN_NICE_INC, IFM_MAX_NICE_INC );
		errors++;
	    }
	}
	else if ( ! strncmp( argv[ac], "-quiet", 3 ) )
	{
	    /* only go quiet if '-debug' option has not changed it */
	    if ( gD.level == IFM_DEBUG_DEFAULT )
		gD.level = 0;
	}
	/* real-time options */
	else if ( ! strncmp( argv[ac], "-host", 4 ) )
	{
	    if ( ++ac >= argc )
	    {
		fputs( "option usage: -host HOSTNAME\n", stderr );
		usage( IFM_PROG_NAME, IFM_USE_SHORT );
		return 1;
	    }

	    strncpy( A->host, argv[ac], ART_NAME_LEN-1 );
	    A->host[ART_NAME_LEN-1] = '\0';	/* just to be sure */
	}
	else if ( ! strncmp( argv[ac], "-rt", 3 ) )
	{
	    A->state = ART_STATE_TO_OPEN; /* real-time is open for business */
	}
	else if ( ! strncmp( argv[ac], "-swap", 5 ) )
	{
	    A->swap = 1;		/* do byte swapping before sending */
	}
	else
	{
	    fprintf( stderr, "error: invalid option <%s>\n\n", argv[ac] );
	    usage( IFM_PROG_NAME, IFM_USE_SHORT );
	    return 1;
	}
    }

    if ( errors > 0 )	       /* check for all minor errors before exiting */
    {
	usage( IFM_PROG_NAME, IFM_USE_SHORT );
	return 1;
    }

    if ( p->start_dir == NULL )
    {
	fputs( "error: missing '-start_dir DIR' option\n", stderr );
	usage( IFM_PROG_NAME, IFM_USE_SHORT );
	return 1;
    }

    /* done processing argument list */

    if ( dir_expansion_form( p->start_dir, &p->glob_dir ) != 0 )
	return 2;

    if ( gD.level > 1 )
	idisp_hf_param_t( "end init_options : ", p );

    if ( gD.level > 0 )
	fprintf( stderr, "\n%s running, use <ctrl-c> to quit...\n\n",
		 IFM_PROG_NAME );

    return 0;
}


/*----------------------------------------------------------------------
 * initialize:
 *     - nice level
 *     - afni communications
 *----------------------------------------------------------------------
*/
static int init_extras( param_t * p, ART_comm * ac )
{
    if ( p->nice && set_nice_level(p->nice) )
        return 1;

    if ( ac->state == ART_STATE_TO_OPEN )            /* open afni comm link */
    {
	atexit( ART_exit );
	ac->mode = AFNI_OPEN_CONTROL_MODE;
	ART_open_afni_link( ac, 2, 1, gD.level );
    }

    return 0;
}


/*----------------------------------------------------------------------
 *  set_nice_level		- set to given "nice" value
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
    char   d0, d1, d2;			/* the three relevant digits */
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

    *sexp = out;			/* save resulting malloc'd address */

    strcpy( out,sin );

    cp = out + len - 1;				/* point to end */

    /* we'd better find 0[01]n - ignore the rest??? */
    while ( (cp > (out+2)) && !isdigit( *cp ) )
	cp--;

    if ( !isdigit(*cp) )			/* didn't find even one */
    {
	fprintf( stderr, "error: dir <%s> is not of the form 00n (e.g. 003)\n",
		 sin );
	free(out);
	return -1;
    }

    cp -= 2;					/* should be first zero  */

    d0 = cp[0];					/* note the three digits */
    d1 = cp[1];
    d2 = cp[2];

    if ( (d0 != '0') ||				/* first is not a zero   */
	 ( (d1 != '0') && (d1 != '1')) )	/* second is not 0 or 1  */
    {
	fprintf( stderr, "error: dir <%s> is not of the form 0[01]n"
	                 " (e.g. 003)\n", sin );
	free(out);
	return -1;
    }

    /* woohooo!  we're good to go! */
    /* set to "...[0-9][02468]n/I.*" (or with [13579]) */

    strcpy( cp, "[0-9]" );			/* add and skip "[0-9]" */
    cp += strlen( "[0-9]" );

    if ( d1 == '0' )				/* adding 2 to each     */
	strcpy( cp, "[02468]" );
    else
	strcpy( cp, "[13579]" );
    cp += strlen( "[02468]" );

    *cp++ = d2;					/* insert final digit */

    strcpy( cp, "/I.*" );			/* the big finish */

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

	fp->bytes = elements * 2;			/* bpp == 16 */

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
	    hi->good = 0;		/* signal file problem */
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
	    "   (im_ary, x_im)     = (0x%p,0x%p)\n",
	    is, is->nalloc, is->nused,
	    is->ary_len, is->im_size, is->im_ary, is->x_im );

    return 0;
}


static int idisp_hf_param_t( char * info, param_t * p )
{
    if ( info )
	fputs( info, stdout );

    if ( p == NULL )
    {
	printf( "idisp_hf_param_t: p == NULL\n" );
	return -1;
    }

    printf( "param_t struct at %p :\n"
            "   (nused, nalloc)   = (%d, %d)\n"
            "   flist             = %p\n"
	    "   start_dir         = %s\n"
	    "   glob_dir          = %s\n"
	    "   nfiles            = %d\n"
	    "   fnames            = %p\n"
	    "   nice              = %d\n",
	    p, p->nused, p->nalloc, p->flist, p->start_dir, p->glob_dir,
	    p->nfiles, p->fnames, p->nice );

    return 0;
}


static int idisp_hf_vol_t( char * info, vol_t * v )
{
    if ( info )
	fputs( info, stdout );

    if ( v == NULL )
    {
	printf( "idisp_hf_vol_t: v == NULL\n" );
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
	    "    (xyz0,xyz1,xyz2) = (%f,%f,%f)\n"
	    "    (xyz3,xyz4,xyz5) = (%f,%f,%f)\n"
	    "    (xyz6,xyz7,xyz8) = (%f,%f,%f)\n",
	    E, E->bpp, E->cflag, E->hdroff, E->skip, E->swap, E->kk,
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
	    I->dx, I->dy, I->dz, I->zoff, I->tr, I->te, I->orients
	  );

    return 0;
}

/* ----------------------------------------------------------------------
 * usage 
 * ----------------------------------------------------------------------
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
	  "%s - monitor real-time aquisition of I-files\n"
	  "\n"
	  "    This program is intended to be run during a scanning session\n"
	  "    on a GE scanner, to monitor the collection of I-files.  The\n"
	  "    user will be notified of any missing slice or any slice that\n"
	  "    is aquired out of order.\n"
	  "\n"
	  "    It is recommended that the user runs '%s' before scanning\n"
	  "    begins, and then watches for error messages during the\n"
	  "    scanning session.  The user should terminate the program\n"
	  "    whey they are done with all runs.\n"
	  "\n"
	  "    At the present time, the user must use <ctrl-c> to terminate\n"
	  "    the program.\n"
	  "\n"
	  "  usage: %s [options] -start_dir DIR\n"
	  "\n"
	  "  examples (no real-time options):\n"
	  "\n"
	  "    %s -start_dir 003\n"
	  "    %s -help\n"
	  "    %s -debug 2 -nice 10 -start_dir 003\n"
	  "\n"
	  "  examples (with real-time options):\n"
	  "\n"
	  "    %s -start_dir 003 -rt\n"
	  "    %s -start_dir 003 -rt -host pickle -swap \n"
	  "\n"
	  "  notes:\n"
	  "\n"
	  "    - Once started, this program exits only when a fatal error\n"
	  "      occurs (single missing or out of order slices are not\n"
	  "      considered fatal).\n"
	  "\n"
	  "    - To terminate this program, use <ctrl-c>.\n"
	  "\n"
	  "  main option:\n"
	  "\n"
	  "    -start_dir DIR     : (REQUIRED) specify starting directory\n"
	  "\n"
	  "        e.g. -start_dir 003\n"
	  "\n"
	  "        The starting directory, DIR, must be of the form 00n,\n"
	  "        where n is a digit.  The program then monitors all\n"
	  "        directories of the form ??n, created by the GE scanner.\n"
	  "\n"
	  "        For instance, with the option '-start_dir 003', this\n"
	  "        program watches for new directories 003, 023, 043, etc.\n"
	  "\n"
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
	  "    -swap             : swap data bytes before sending to afni\n"
	  "\n"
	  "        Since afni may be running on a different machine, the byte\n"
	  "        order may differ there.  This option will force the bytes\n"
	  "        to be reversed, before sending the data to afni.\n"
	  "\n"
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
	  "    -nice              : adjust the nice value for the process\n"
	  "\n"
	  "        e.g.  -nice 10\n"
	  "        the default is 0, and the maximum is 20\n"
	  "        a superuser may use through the minimum of -19\n"
	  "\n"
	  "        A positive increment to the nice value of a process will\n"
	  "        lower its priority, allowing other processes more CPU\n"
	  "        time.\n"
	  "\n"
	  "    -quiet             : show only errors and final information\n"
	  "\n"
	  "    -version           : show the version information\n"
	  "\n"
	  "  Author: R. Reynolds - %s\n"
	  "\n",
	  prog, prog, prog,
	  prog, prog, prog, prog, prog,
	  prog, prog,
	  IFM_VERSION
	);

	return 0;
    }
    else if ( level == IFM_USE_VERSION )
    {
	printf( "%s: %s, compile date: %s\n",
		prog, IFM_VERSION, __DATE__ );
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

	case SIGINT :
	case SIGTERM :
	    show_run_stats( &gS );
	    break;
    }

    exit(1);
}


/* ----------------------------------------------------------------------
 * set_volume_stats
 * ----------------------------------------------------------------------
*/
static int set_volume_stats( vol_t * v )
{
    run_t * rp;		  /* for a little speed, this will be called often */

    if ( v == NULL || v->seq_num < 0 || v->run < 0 )
    {
	fprintf( stderr, "failure: SVS - insufficient data\n\n" );
	idisp_hf_vol_t ( "-- VOLUME FAILURE INFO : ", v );
    }

    if ( gS.nalloc == 0 )
    {
	gS.runs = (run_t *)calloc( IFM_STAT_ALLOC, sizeof(run_t) );
	if ( gS.runs == NULL )
	{
	    fprintf( stderr, "failure: cannot allocate space for run info\n" );
	    return -1;
	}

	/* first time caller - fill initial stats info */
	gS.nalloc  = IFM_STAT_ALLOC;
	gS.nused   = 0;
	gS.slices  = v->nim;
	gS.z_first = v->z_first;
	gS.z_last  = v->z_last;
	gS.z_delta = v->z_delta;

	if ( gD.level > 1 )
	    fprintf( stderr, "\n-- svs: init alloc - vol %d, run %d, file %s\n",
		     v->seq_num, v->run, v->first_file );
    }

    if ( v->run >= gS.nalloc )		/* run is 0-based */
    {
	/* make space for many more runs - we don't want to do this often */
	gS.runs = (run_t *)realloc( gS.runs, (v->run + IFM_STAT_ALLOC) *
		                             sizeof(run_t) );
	if ( gS.runs == NULL )
	{
	    fprintf( stderr, "failure: cannot realloc space for run info\n" );
	    return -1;
	}

	gS.nalloc = v->run + IFM_STAT_ALLOC;

	/* zero out any new memory */
	memset( gS.runs + gS.nused, 0, (gS.nalloc - gS.nused)*sizeof(run_t) );

	if ( gD.level > 1 )
	    fprintf( stderr,
		"\n-- svs: realloc (%d entries) - vol %d, run %d, file %s\n",
		gS.nalloc, v->seq_num, v->run, v->first_file );

    }

    /* we have memory - the current run number is an index into runs */

    rp = gS.runs + v->run;

    if ( gS.nused < v->run+1 )
	gS.nused = v->run+1;

    if ( rp->volumes == 0 )
	strncpy( rp->f1name, v->first_file, IFM_MAX_FLEN );

    rp->volumes = v->seq_num;

    if ( gD.level > 2 )
	fprintf( stderr, "\n-- svs: run %d, seq_num %d\n", v->run, v->seq_num );

    return 0;
}

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
	    printf( "    run #%4d : volumes = %d, first file = %s\n",
		    c, s->runs[c].volumes, s->runs[c].f1name );
    }

    putchar( '\n' );

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

    if ( tr2 < 1 )
	return 1;

    if ( tr2 > 10 )
	return 10;			/* ??? tres big */

    return( (int)(tr + 0.9) );  	/* basically, use ceiling */
}


/* ----------------------------------------------------------------------
 * find_next_zoff
 *
 * Given p->flist, search from index start for an image with
 * geh.zoff equal to zoff.
 *
 * return   index : upon succes		(start <= index <= p->nused)
 *             -1 : not found
 *             -2 : error
 * ----------------------------------------------------------------------
*/
static int find_next_zoff( param_t * p, int start, float zoff )
{
    int count;

    if ( (p == NULL) || (start < 0) )
	return -2;

    if ( start > p->nused )			/* say not found */
	return -1;

    for ( count = start; count <= p->nused; count++ )
	if ( fabs( zoff - p->flist[count].geh.zoff ) < IFM_EPSILON )
	    return count;			/* found! */

    return -1;
}


/* ----------------------------------------------------------------------
 * Given:  run     > 0
 *         seq_num > 0
 *         naps
 *
 * If this is not run 1, and naps is too big, and the run is incomplete,
 * print a warning message to the user.
 *
 * notes:   - print only 1 warning message per seq_num, per run
 *          - run and seq_num are for the previously found volume
 *
 * returns:
 *          2 : run is stalled - message printed
 *          1 : first run appears stalled - end of run?
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

    if ( run == 1 )
    {
	if ( gD.level > 1 )
	    fprintf( stderr, "apparently done with run 1\n" );
	return 1;
    }

    /* verify that we have already taken note of the previous volume */
    if ( ((gS.nused + 1) < run) || (gS.runs[run].volumes < seq_num) )
    {
	fprintf( stderr, "** warning: CSR - stats inconsistancy!\n" );
	func_failure = 1;

	return -1;
    }

    if ( seq_num < gS.runs[1].volumes )	   /* are we done with the run yet? */
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
		     run, seq_num, gS.runs[1].volumes,
		     naps*nap_time, gS.runs[run].f1name );

	    prev_run = run;
	    prev_seq = seq_num;

	    return 2;
	}
    }
    else if ( seq_num >= gS.runs[1].volumes )
    {
	/* if this is our first visit, then we just finished a run */
	if ( (run != prev_run) || (seq_num != prev_seq) )
	{
	    prev_run = run;
	    prev_seq = seq_num;

	    return 1;
	}
	else
	    return 0;
    }

    return 0;
}

/*-------------------------------------------------------*/
/*! Return the file length (-1 if file not found).       */
/*  (local copy from thd_filestuff.c                     */

static unsigned long l_THD_filesize( char * pathname )
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

    kk = p->flist[v->fl_1].gex.kk;

    switch( kk )
    {
	case 1:					/* LR */
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

	case 2:					/* PA */
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
	    
	case 3:					/* IS */
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
	    fprintf(stderr, "** COS failure: kk (%d) out of [1,3] range\n", kk);
	    return -1;
	}
    }

    v->geh.orients[6] = '\0';

    return 0;
}

