
/* ----------------------------------------------------------------------
 * This is an interface for processing GEMS 4.x image files
 * (see ge4_header.h for structure contents).
 * ----------------------------------------------------------------------
 * int ge4_read_header( char * filename, ge4_header * H )
 *
 *     The basic point is to pass an empty ge4_header structure in,
 *     along with a file name.  This function fills the structure.
 *
 *     One exception: if H->image is non-null, this function will
 *                    read the image into that location.
 *
 *     - returns 0 on success
 *
 * R. Reynolds  2003 April 29
 * ----------------------------------------------------------------------
*/

#include <stdio.h>
#include <string.h>

#include "ge4_header.h"


/* comes from either Imon.o or libmri.a */
extern unsigned long THD_filesize ( char * pathname );
extern int swap_2( void * ptr );
extern int swap_4( void * ptr );

/* ---------------------------------------------------------------------- */
/* series header value descriptions - for display */

static char * g_ge4_sl_im_modes[] =
			{ "2D single", "2D multiple", "3D volume", "cine",
			  "spectroscopy" };
static char * g_ge4_sl_pulse_seqs[] =
			{ "memp", "ir", "ps", "rm", "rmge", "gre", "vemp",
			  "mpgr", "mpgrv", "mpirs", "mpiri", "3d/gre",
			  "cine/gre", "spgr", "sspf", "cin/spgr", "3d/spgr",
			  "fse", "fve", "fspr", "fgr", "fmpspgr", "fmpgr",
			  "fmpir", "probe.s", "probe.p" };
static char * g_ge4_sl_orient[] = { "supine", "prone", "Lt", "Rt" };

/* ---------------------------------------------------------------------- */

/* ----------------------------------------------------------------------
 * Validate and read header data from a GEMS 4.x formatted file.
 * 
 * If H->image is non-NULL, fill with image data.
 *
 * return   0 : on success
 *        < 0 : on error
 * ----------------------------------------------------------------------
*/
int ge4_read_header( char * filename, ge4_header * H )
{
    ge4_image_t  * ih;
    ge4_series_t * sh;
    ge4_study_t  * st;
    FILE         * fp;
    int            file_len;
    int            rres = 0;		/* read result */

    if ( filename == NULL || H == NULL )
    {
	fprintf( stderr, "** rg4h : bad params: %p, %p\n", filename, H );
	return -1;
    }

    memset( H, 0, sizeof(ge4_header) );

    file_len = THD_filesize( filename );

    /* file size must be fixed at 145408 bytes (142 KB) */
    if ( file_len != (GE4_HEADER_LENGTH + GE4_IMAGE_SIZE) )
	return 1;

    if ( (fp = fopen( filename, "r" )) == NULL )
    {
	fprintf( stderr, "ge4_read_header: failed to open '%s' for reading\n",
		 filename);
	return -1;
    }

    /* quickly scan and validate titles */

    sh = &H->ser_h;	/* set helper pointer */
    ih = &H->im_h;	/* set helper pointer */
    st = &H->std_h;	/* set helper pointer */

    fseek( fp, GE4_OFF_STDY_TITLE, SEEK_SET );
    rres |= (1 - fread( st->title, GE4_L_STDY_TITLE, 1, fp ));

    fseek( fp, GE4_OFF_SER_TITLE, SEEK_SET );
    rres |= (1 - fread( sh->title, GE4_L_SER_TITLE, 1, fp ));

    fseek( fp, GE4_OFF_IMG_TITLE, SEEK_SET );
    rres |= (1 - fread( ih->title, GE4_L_IM_TITLE, 1, fp ));

    /* if read failure or bad title fields, we're outta' here */
    if ( rres							  ||
         strncmp( st->title, GE4_STUDY_TITLE,  GE4_L_STDY_TITLE ) ||
         strncmp( sh->title, GE4_SERIES_TITLE, GE4_L_SER_TITLE  ) ||
         strncmp( ih->title, GE4_IMAGE_TITLE,  GE4_L_IM_TITLE   )
       )
	return 1;


    /* study header fields */

    fseek( fp, GE4_OFF_STDY_NUM, SEEK_SET );
    rres |= (1 - fread( st->num, GE4_L_STDY_NUM, 1, fp ));

    fseek( fp, GE4_OFF_STDY_DATE, SEEK_SET );
    rres |= (1 - fread( st->date, GE4_L_STDY_DATE, 1, fp ));

    fseek( fp, GE4_OFF_STDY_TIME, SEEK_SET );
    rres |= (1 - fread( st->time, GE4_L_STDY_TIME, 1, fp ));

    fseek( fp, GE4_OFF_STDY_PAT_NAME, SEEK_SET );
    rres |= (1 - fread( st->pat_name, GE4_L_STDY_PAT_NAME, 1, fp ));

    fseek( fp, GE4_OFF_STDY_PAT_ID, SEEK_SET );
    rres |= (1 - fread( st->pat_id, GE4_L_STDY_PAT_ID, 1, fp ));

    fseek( fp, GE4_OFF_STDY_AGE, SEEK_SET );
    rres |= (1 - fread( st->age, GE4_L_STDY_AGE, 1, fp ));

    fseek( fp, GE4_OFF_STDY_SEX, SEEK_SET );
    rres |= (1 - fread( &st->sex, 1, 1, fp ));

    /* series header fields */

    fseek( fp, GE4_OFF_SER_SERIES_NUM, SEEK_SET );
    rres |= (1 - fread( sh->series_num, GE4_L_SER_SER_NUM, 1, fp ));

    fseek( fp, GE4_OFF_SER_PLANE_TYPE, SEEK_SET );
    rres |= (1 - fread( &sh->plane_type, sizeof(sh->plane_type), 1, fp ));

    fseek( fp, GE4_OFF_SER_PLANE_DESC, SEEK_SET );
    rres |= (1 - fread( sh->plane_desc, GE4_L_SER_PL_DESC, 1, fp ));

    fseek( fp, GE4_OFF_SER_IM_MODE, SEEK_SET );
    rres |= (1 - fread( &sh->im_mode, sizeof(sh->im_mode), 1, fp ));

    fseek( fp, GE4_OFF_SER_PULSE_SEQ, SEEK_SET );
    rres |= (1 - fread( &sh->pulse_seq, sizeof(sh->pulse_seq), 1, fp ));

    fseek( fp, GE4_OFF_SER_FOV, SEEK_SET );
    rres |= (1 - fread( &sh->fov, sizeof(sh->fov), 1, fp ));

    fseek( fp, GE4_OFF_SER_CENTER, SEEK_SET );
    rres |= (1 - fread( sh->center, sizeof(sh->center), 1, fp ));

    fseek( fp, GE4_OFF_SER_ORIENT, SEEK_SET );
    rres |= (1 - fread( &sh->orient, sizeof(sh->orient), 1, fp ));

    fseek( fp, GE4_OFF_SER_SCAN_MAT_X, SEEK_SET );
    rres |= (1 - fread( &sh->scan_mat_x, sizeof(sh->scan_mat_x), 1, fp ));

    fseek( fp, GE4_OFF_SER_SCAN_MAT_Y, SEEK_SET );
    rres |= (1 - fread( &sh->scan_mat_y, sizeof(sh->scan_mat_y), 1, fp ));

    fseek( fp, GE4_OFF_SER_IM_MAT, SEEK_SET );
    rres |= (1 - fread( &sh->im_mat, sizeof(sh->im_mat), 1, fp ));


    /* image header fields */

    fseek( fp, GE4_OFF_IMG_IM_NUM, SEEK_SET );
    rres |= (1 - fread( ih->im_num, GE4_L_IM_NUM, 1, fp ));

    fseek( fp, GE4_OFF_IMG_IM_LOCN, SEEK_SET );
    rres |= (1 - fread( &ih->im_loc, sizeof(ih->im_loc), 1, fp ));

    fseek( fp, GE4_OFF_IMG_TABLE_POSN, SEEK_SET );
    rres |= (1 - fread( &ih->table_posn, sizeof(ih->table_posn), 1, fp ));

    fseek( fp, GE4_OFF_IMG_IM_THICK, SEEK_SET );
    rres |= (1 - fread( &ih->im_thickness, sizeof(ih->im_thickness), 1, fp ));

    fseek( fp, GE4_OFF_IMG_IM_SPACING, SEEK_SET );
    rres |= (1 - fread( &ih->im_spacing, sizeof(ih->im_spacing), 1, fp ));

    fseek( fp, GE4_OFF_IMG_TR, SEEK_SET );
    rres |= (1 - fread( &ih->tr, sizeof(ih->tr), 1, fp ));

    fseek( fp, GE4_OFF_IMG_TE, SEEK_SET );
    rres |= (1 - fread( &ih->te, sizeof(ih->te), 1, fp ));

    fseek( fp, GE4_OFF_IMG_TI, SEEK_SET );
    rres |= (1 - fread( &ih->ti, sizeof(ih->ti), 1, fp ));

    fseek( fp, GE4_OFF_IMG_NUM_ECHOS, SEEK_SET );
    rres |= (1 - fread( &ih->num_echoes, sizeof(ih->num_echoes), 1, fp ));

    fseek( fp, GE4_OFF_IMG_ECHO_NUM, SEEK_SET );
    rres |= (1 - fread( &ih->echo_num, sizeof(ih->echo_num), 1, fp ));

    fseek( fp, GE4_OFF_IMG_NEX_INT, SEEK_SET );
    rres |= (1 - fread( &ih->iNEX, sizeof(ih->iNEX), 1, fp ));

    fseek( fp, GE4_OFF_IMG_NEX_REAL, SEEK_SET );
    rres |= (1 - fread( &ih->fNEX, sizeof(ih->fNEX), 1, fp ));

    fseek( fp, GE4_OFF_IMG_FLIP_ANGLE, SEEK_SET );
    rres |= (1 - fread( &ih->flip_angle, sizeof(ih->flip_angle), 1, fp ));

    if ( rres )
    {
	fprintf( stderr, "** failed to read ge4 header for '%s'\n", filename );
	return -1;
    }

    if ( ge4_validate_header( H ) )
	return 1;

    if ( H->image != NULL )	/* then read in the image */
    {
	fseek( fp, GE4_HEADER_LENGTH, SEEK_SET );
	rres = fread( H->image, GE4_IMAGE_SIZE, 1, fp );

	if ( rres != 1 )
	{
	    fprintf( stderr, "** failed to read ge4 image for file '%s'\n",
		     filename );
	    return -1;
	}
    }

    return 0;
}


/*------------------------------------------------------------
 *  Check for valid data in the header.
 *
 *  If values are out of range, try byte swapping.
 *
 *  series header:
 *	plane_type	: in {0..4}
 *	image_mode	: in {0..4}
 *	pulse_seq	: in {0..25}
 *
 *  return    0 : valid
 *         else : invalid
 *------------------------------------------------------------
*/
int ge4_validate_header( ge4_header * h )
{
    ge4_series_t * s;
    ge4_image_t  * im;

    if ( h == NULL )
	return -1;

    s  = &h->ser_h;
    im = &h->im_h;

    /* note that titles have already been validated */
    if ( (s->plane_type < 0) || (s->plane_type > 4) ||
	 (s->im_mode    < 0) || (s->im_mode    > 4) ||
         (s->pulse_seq  < 0) || (s->pulse_seq  > 25) )
    {
	ge4_swap_all_bytes( h );
    }

    /* if these are still off, we are hosed... */
    if ( (s->plane_type < 0) || (s->plane_type > 4) ||
	 (s->im_mode    < 0) || (s->im_mode    > 4) ||
         (s->pulse_seq  < 0) || (s->pulse_seq  > 25) )
    {
	return -1;
    }

    return 0;
}


/*------------------------------------------------------------
 *  Swap all numeric fields in ge4_header sub-structs.
 *------------------------------------------------------------
*/
int ge4_swap_all_bytes( ge4_header * h )
{
    if ( h == NULL )
    {
	fprintf( stderr, "** ge4_SAB : no header!\n" );
	return -1;
    }

    h->swap = 1;		/* note that we have swapped */

    /* series header */

    swap_2( &h->ser_h.plane_type );
    swap_2( &h->ser_h.im_mode );
    swap_2( &h->ser_h.pulse_seq );

    swap_4( &h->ser_h.fov );
    swap_4( &h->ser_h.center[0] );
    swap_4( &h->ser_h.center[1] );
    swap_4( &h->ser_h.center[2] );

    swap_2( &h->ser_h.orient );
    swap_2( &h->ser_h.scan_mat_x );
    swap_2( &h->ser_h.scan_mat_y );
    swap_2( &h->ser_h.im_mat );

    /* image header */

    swap_4( &h->im_h.im_loc );
    swap_4( &h->im_h.table_posn );
    swap_4( &h->im_h.im_thickness );
    swap_4( &h->im_h.im_spacing );

    swap_4( &h->im_h.tr );
    swap_4( &h->im_h.te );
    swap_4( &h->im_h.ti );

    swap_2( &h->im_h.num_echoes );
    swap_2( &h->im_h.echo_num );

    swap_2( &h->im_h.iNEX );
    swap_4( &h->im_h.fNEX );

    swap_2( &h->im_h.flip_angle );

    return 0;
}


/*------------------------------------------------------------
 *  Display the contents of the ge4_image_t struct.
 *------------------------------------------------------------
*/
int idisp_ge4_study_header( char * info, ge4_study_t * st )
{
    if ( info )
	fputs( info, stdout );

    if ( st == NULL )
    {
	printf( "r_idisp_ge4_study_t: st == NULL" );
	return -1;
    }

    printf( " ge4_study_t at %p :\n"
	    "    title                    = %s\n"
	    "    num                      = %s\n"
	    "    date                     = %s\n"
	    "    time                     = %s\n"
	    "    pat_name                 = %s\n"
	    "    pat_id                   = %s\n"
	    "    age                      = %s\n"
	    "    sex                      = %c\n",
	    st, st->title, st->num, st->date, st->time,
	    st->pat_name, st->pat_id, st->age, st->sex
	    );

    return 0;
}


/*------------------------------------------------------------
 *  Display the contents of the ge4_image_t struct.
 *------------------------------------------------------------
*/
int idisp_ge4_image_header( char * info, ge4_image_t * im )
{
    if ( info )
	fputs( info, stdout );

    if ( im == NULL )
    {
	printf( "r_idisp_ge4_image_t: im == NULL" );
	return -1;
    }

    printf( " ge4_image_t at %p :\n"
	    "    title                    = %s\n"
	    "    im_num                   = %s\n"
	    "    im_loc                   = %.3f\n"
	    "    table_posn               = %.3f\n"
	    "    im_thickness             = %.3f\n"
	    "    im_spacing               = %.3f\n"
	    "    tr (in ms)               = %.3f\n"
	    "    te (in ms)               = %.3f\n"
	    "    ti (in ms)               = %.3f\n"
	    "    num_echoes               = %d\n"
	    "    echo_num                 = %d\n"
	    "    iNEX                     = %d\n"
	    "    fNEX                     = %.3f\n"
	    "    flip_angle               = %d\n",
	    im, im->title, im->im_num, im->im_loc, im->table_posn,
	    im->im_thickness, im->im_spacing, im->tr, im->te, im->ti,
	    im->num_echoes, im->echo_num, im->iNEX, im->fNEX, im->flip_angle
	    );

    return 0;
}


/*------------------------------------------------------------
 *  Display the contents of the ge4_series_t struct.
 *------------------------------------------------------------
*/
int idisp_ge4_series_header( char * info, ge4_series_t * s )
{
    if ( info )
	fputs( info, stdout );

    if ( s == NULL )
    {
	printf( "r_idisp_ge4_series_t: s == NULL" );
	return -1;
    }

    printf( " ge4_series_t at %p :\n"
	    "    title                    = %s\n"
	    "    series_num               = %s\n"
	    "    plane_type, plane_desc   = %d, %s\n"
	    "    image_mode               = %d (%s)\n"
	    "    pulse_seq                = %d (%s)\n"
	    "    FOV (in mm)              = %.3f\n"
	    "    center[0], c[1], c[2]    = %.3f, %.3f, %.3f\n"
	    "    orient                   = %d (%s)\n"
	    "    scan_mat_x, scan_mat_y   = %d, %d\n"
	    "    im_mat                   = %d\n",
	    s, s->title, s->series_num, s->plane_type, s->plane_desc,
	    s->im_mode, GE4M_IND2STR(s->im_mode, g_ge4_sl_im_modes),
	    s->pulse_seq, GE4M_IND2STR(s->pulse_seq, g_ge4_sl_pulse_seqs),
	    s->fov, s->center[0], s->center[1], s->center[2],
	    s->orient, GE4M_IND2STR(s->orient,g_ge4_sl_orient),
	    s->scan_mat_x, s->scan_mat_y, s->im_mat
	    );

    return 0;
}


/*------------------------------------------------------------
 *  Swap multiple byte pairs.
 *------------------------------------------------------------
*/
int swap_2_multi( void * ptr, int num_shorts )
{
    unsigned char * cp0, * cp1;
    unsigned char   tmpc;
    int             index;

    if ( ptr == NULL ) return -1;

    cp0 = (unsigned char *)ptr;
    cp1 = cp0 + 1;

    for ( index = 0; index < num_shorts; index++ )
    {
	tmpc = *cp0;
	*cp0 = *cp1;
	*cp1 = tmpc;

	cp0 += 2;
	cp1 += 2;
    }

    return 0;
}

