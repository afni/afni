
#define IFM_VERSION "version 1.2 (November, 2002)"

/*----------------------------------------------------------------------
 * history:
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
#include <signal.h>
#include <string.h>
#include <unistd.h>

#include "mrilib.h"
#include "machdep.h"
#include "Imon.h"
#include "r_idisp.h"

#define MAIN

/***********************************************************************/
/* globals */

IFM_debug gD;		/* debug information       */
param_t   gP;		/* main parameter struct   */
stats_t   gS;		/* general run information */

/***********************************************************************/
int main( int argc, char * argv[] )
{
    param_t * p = &gP;			/* access the global as local  */
    vol_t     baseV;			/* base volume - first scanned */
    int       ret_val;

    /* validate inputs and init options structure */
    if ( (ret_val = init_options( p, argc, argv )) != 0 )
	return ret_val;

    if ( gD.level > 0 )
	fprintf( stderr, "\n%s running, use <ctrl-c> to quit...\n\n",
		 IFM_PROG_NAME );

    if ( (ret_val = find_first_volume( &baseV, p )) != 0 )
	return ret_val;

    if ( (ret_val = find_more_volumes( &baseV, p )) != 0 )
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
static int find_first_volume( vol_t * v, param_t * p )
{
    int ret_val;
    int start = 0;  /* initial starting location for the first volume */

    if ( gD.level > 0 ) 		/* status */
        fprintf( stderr, "-- scanning for first volume\n" );

    ret_val = 0;
    while ( ret_val == 0 )
    {
	ret_val = read_ge_files( p, 0, 0 );

	if ( ret_val > 0 )
	{
	    ret_val = volume_search( v, p, &start, 0 );

	    if ( ret_val == -1 )   /* try to recover from a data error */
		ret_val = 0;
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
		    idisp_hf_vol_t( "first volume : ", v );
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
static int find_more_volumes( vol_t * v0, param_t * p )
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
    fl_index = v0->last_im + 1;		/* start looking past first volume */
    next_im  = v0->last_im + 1;		/* for read_ge_files()             */

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
		if ( gD.level > 1 )
		    idisp_hf_vol_t( "-- new volume: ", &vn );

		fl_index += vn.nim;		/* note the new position   */
		next_im   = vn.last_im + 1;	/* for read_ge_files()     */

		if ( vn.run != run )		/* new run?                */
		{
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
		(void)check_stalled_run( run, seq_num, naps, nap_time );

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
    V->first_im = p->flist[first].index;
    V->last_im  = p->flist[last].index;
    strncpy( V->first_file, p->fnames[V->first_im], MAX_FLEN );
    strncpy( V->last_file,  p->fnames[V->last_im],  MAX_FLEN );
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
    vout->first_im = p->flist[start].index;
    vout->last_im  = p->flist[start+vout->nim-1].index;
    strncpy( vout->first_file, p->fnames[vout->first_im], MAX_FLEN );
    strncpy( vout->last_file,  p->fnames[vout->last_im],  MAX_FLEN );
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

    if ( n2scan > p->nalloc )
    {
	/* we need more memory */
	p->flist = (finfo_t *)realloc( p->flist, n2scan * sizeof(finfo_t) );

	if ( p->flist == NULL )
	{
	    fprintf(stderr, "failure to allocate %d finfo_t structs\n", n2scan);
	    return -1;
	}

	p->nalloc = n2scan;
    }

    p->nused = scan_ge_files( p->flist, p->fnames, next, n2scan );

    if ( gD.level > 1 )
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
	finfo_t  * flist,		/* location of file info array */
	char    ** fnames,		/* list of filenames to scan   */
	int        next,		/* index of next file to scan  */
	int        nfiles )		/* number of files to scan     */
{
    static int   read_failure = -1;	/* last read_ge_header failure */
    finfo_t    * fp;
    int          count, files_read, rv;

    if ( flist == NULL || fnames == NULL )
    {
	fprintf( stderr, "failure: SGF with (flist, fnames) = (%p, %p)\n",
		 flist, fnames );
	return -1;
    }

    if ( nfiles <= 0 )
	return 0;

    /* scan from 'next' to 'next + nfiles - 1' */
    for ( count = next, fp = flist; count < next + nfiles; count++, fp++ )
    {
	rv = read_ge_header( fnames[count], &fp->geh, &fp->gex );
	if ( rv != 0 )
	{
	    if ( read_failure != count )
	    {
		/* first time to fail with this file - wait and try again */
		if ( gD.level > 1 )
		    fprintf( stderr, "\n** failure to read GE header for "
			     "file <%s>\n", fnames[count] );
		read_failure = count;

		break;
	    }
	    else
	    {
		fprintf( stderr, "\nfailure: cannot read GE header for "
			 "file <%s>\n", fnames[count] );
		return -1;
	    }
	}
	else
	{
	    fp->index = count;		/* store index into fnames array */

	    if ( gD.level > 2 )
		idisp_ge_header_info( fnames[fp->index], &fp->geh );
	}
    }

    /* even on failure, this non-negative integer is accurate */
    files_read = count - next;

    if ( gD.level > 1 )
	printf( "-- scanned %d GE files, from <%s> to <%s>\n",
		files_read, fnames[next], fnames[next+files_read-1] );

    return files_read;
}


/*----------------------------------------------------------------------
 * init_options:
 *
 *     1. check usage: Imon -start_dir DIR
 *     2. do initial allocation of data structures
 *----------------------------------------------------------------------
*/
static int init_options( param_t * p, int argc, char * argv[] )
{
    int ac, rv;

    if ( p == NULL )
	return 2;

    memset( p,   0, sizeof(*p) );	/* parameters   */
    memset( &gD, 0, sizeof(gD) );	/* debug struct */
    memset( &gS, 0, sizeof(gS) );	/* stats struct */

    if ( argc < 2 )
    {
	usage( IFM_PROG_NAME, IFM_USE_SHORT );
	return 1;
    }

    /* debug level 1 is now the default - by order of Wen Ming :) */
    gD.level = 1;

    for ( ac = 1; ac < argc; ac++ )
    {
	if ( ! strncmp( argv[ac], "-help", 2 ) )
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
		usage( IFM_PROG_NAME, IFM_USE_SHORT );
		return 1;
	    }
	}
	else if ( ! strncmp( argv[ac], "-quiet", 3 ) )
	{
	    /* only go quiet if '-debug' option has not changed it */
	    if ( gD.level == IFM_DEBUG_DEFAULT )
		gD.level = 0;
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
		usage( IFM_PROG_NAME, IFM_USE_SHORT );
		return 1;
	    }
	}
	else
	{
	    fprintf( stderr, "error: invalid option <%s>\n\n", argv[ac] );
	    usage( IFM_PROG_NAME, IFM_USE_SHORT );
	    return 1;
	}
    }

    if ( p->start_dir == NULL )
    {
	fputs( "error: missing '-start_dir DIR' option\n", stderr );
	usage( IFM_PROG_NAME, IFM_USE_SHORT );
	return 1;
    }

    if ( dir_expansion_form( p->start_dir, &p->glob_dir ) != 0 )
	return 2;

    if ( p->glob_dir == NULL )			/* error message from DEF() */
	return 1;

    if ( p->nice != 0 )
    {
	rv = nice( p->nice );
	if ( rv == -1 )
	{
	    if ( p->nice < 0 )
		fprintf( stderr, "error: only root may decrement nice value\n"
			         "       (errno = %d, rv = %d)\n", errno, rv );
	    else
		fprintf( stderr,
			 "error: failure to adjust nice by %d\n"
			 "       (errno = %d, rv = %d)\n", p->nice, errno, rv );
	    return 1;
	}

	if ( gD.level > 1 )
	    fprintf( stderr, "-- nice value incremented by %d (rv = %d)\n",
		     p->nice, rv );
    }

    if ( gD.level > 1 )
	idisp_hf_param_t( "end init_options : ", p );

    return 0;
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

    /* we'd better find 00n - ignore the rest??? */
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
 *  read_ge_header  (basically from Ifile.c, but use file_tool.c version)
 *
 *  returns:   0  : success
 *           < 0  : failure
 *----------------------------------------------------------------------
*/
static int read_ge_header( char * pathname, ge_header_info * hi, ge_extras * E )
{
   FILE *imfile ;
   int  length , skip , swap=0 ;
   char orients[8] , str[8] ;
   int nx , ny , bpp , cflag , hdroff ;
	float uv17 = -1.0;
	
   if( hi == NULL ) return -1;            /* bad */
   hi->good = 0 ;                       /* not good yet */
   if( pathname    == NULL ||
       pathname[0] == '\0'   ) return -1; /* bad */

   length = THD_filesize( pathname ) ;
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
	
	hi->good = 1 ;                  /* this is a good file */

	/* fill any existing ge_extras structure */
	if ( E )
	{
	    E->bpp    = bpp;		/* store the ge_extra info */
	    E->cflag  = cflag;
	    E->hdroff = hdroff;
	    E->skip   = skip;
	    E->swap   = swap;

	    memcpy( E->xyz, xyz, sizeof(xyz) );
	}
    } /* end of actually reading image header */

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
	printf( "idisp_hf_param_t: v == NULL\n" );
	return -1;
    }

    printf( "vol_t struct at %p :\n"
	    "   nim                 = %d\n"
	    "   (first_im, last_im) = (%d, %d)\n"
	    "   first_file          = %s\n"
	    "   last_file           = %s\n"
	    "   (z_first, z_last)   = (%f, %f)\n"
	    "   z_delta             = %f\n"
	    "   (seq_num, run)      = (%d, %d)\n",
	    v, v->nim, v->first_im, v->last_im,
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
	    "    (xyz0,xyz1,xyz2) = (%f,%f,%f)\n"
	    "    (xyz3,xyz4,xyz5) = (%f,%f,%f)\n"
	    "    (xyz6,xyz7,xyz8) = (%f,%f,%f)\n",
	    E, E->bpp, E->cflag, E->hdroff, E->skip, E->swap,
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
	    "    orients     = %8s\n",
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
	  "    begins, and then watches for error messages during the.\n"
	  "    scanning session.  The user should terminate the program\n"
	  "    whey they are done with all runs.\n"
	  "\n"
	  "    At the present time, the user must use <ctrl-c> to terminate\n"
	  "    the program.\n"
	  "\n"
	  "  usage: %s [options] -start_dir DIR\n"
	  "\n"
	  "  examples:\n"
	  "\n"
	  "    %s -start_dir 003\n"
	  "    %s -quiet -start_dir 003\n"
	  "    %s -help\n"
	  "    %s -version\n"
	  "    %s -debug 2 -nice 10 -start_dir 003\n"
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
	  "    -start_dir DIR     : REQUIRED - specify starting directory\n"
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
	  prog, prog, prog, prog, prog, prog, prog, prog, IFM_VERSION
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
    int     seq_num, run;

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
	strncpy( rp->first_im, v->first_file, MAX_FLEN );

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
		    c, s->runs[c].volumes, s->runs[c].first_im );
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
 * check_stalled_run
 *
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
 *          1 : run is stalled - message printed
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

    if ( ( run <= 1 ) || ( seq_num < 1 ) || ( naps <= IFM_MAX_RUN_NAPS ) )
	return 0;

    /* verify that we have already taken note of the previous volume */
    if ( ((gS.nused + 1) < run) || (gS.runs[run].volumes < seq_num) )
    {
	fprintf( stderr, "** warning: CSR - stats inconsistancy!\n" );
	func_failure = 1;

	return -1;
    }

    if ( seq_num < gS.runs[1].volumes )		/* are we done with a run? */
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
		     naps*nap_time, gS.runs[run].first_im );

	    prev_run = run;
	    prev_seq = seq_num;

	    return 1;
	}
    }

    return 0;
}
