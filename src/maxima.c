/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/

#include "afni.h"
#include "maxima.h"

char grMessage[ R_MESSAGE_L ];

/*----------------------------------------------------------------------
**
**  Find local maxima, storing the extrema as a mask.
**
**----------------------------------------------------------------------
*/
int process_data( maxima_s * M )
{
ENTRY("process_data");
    ( void )find_local_maxima( M );

    if ( ! create_point_list( M ) )
        RETURN(0);

    gr_orig_data = M->sdata;            /* global needed for sorting */
    if ( M->negatives )
        qsort( M->P.plist, M->P.used, sizeof( int ), point_comp_neg );
    else
        qsort( M->P.plist, M->P.used, sizeof( int ), point_comp_pos );

    if ( M->debug > 1 )
	show_point_list_s( "+d point list sorted: ", &M->P, M->debug );

    if ( ( M->min_dist > 1.0 ) && ! apply_min_dist( M ) )
        RETURN(0);

    if ( M->debug > 1 )
	show_point_list_s( "+d point list cleaned: ", &M->P, M->debug );

    if ( M->outfile )
        apply_fill_radius( M );

    RETURN(1);
}


/*----------------------------------------------------------------------
**  Display the contents of the point_list_s struct.
**----------------------------------------------------------------------
*/
void show_point_list_s( char * mesg, point_list_s * p, int debug )
{
    int c;

ENTRY("show_point_list_s");

    if ( mesg ) fputs( mesg, stderr );

    fprintf(stderr, "point_list_s @ %p, used = %d, M = %d\n",
	    (void *)p, p->used, p->M);

    if ( debug <= 0 ) EXRETURN;		/* we're done */

    fprintf(stderr,"  plist starting @ %p:", (void *)p->plist );

    for ( c = 0; c < p->used; c++ )
	fprintf(stderr,"  %d", p->plist[c] );
    fprintf(stderr,"\n");

    EXRETURN;
}


/*----------------------------------------------------------------------
**
**  Display the contents of the maxima structure.
**
**----------------------------------------------------------------------
*/
void show_maxima_s( char * mesg, maxima_s * M )
{
ENTRY("show_maxima_s");

    if ( mesg ) fputs( mesg, stderr );

    fprintf( stderr,
        "------------------------------\n"
        "dset   *      : %p\n"
        "sdata  *      : %p\n"
        "result *      : %p\n"
        "nx            : %d\n"
        "ny            : %d\n"
        "nz            : %d\n"
        "nxy           : %d\n"
        "nvox          : %d\n"

        "P.plist       : %p\n"
        "P.used        : %d\n"
        "P.M           : %d\n"

        "extrema count : %d\n"

        "data_type     : %d\n"
        "adn_type      : %d\n"
        "func_type     : %d\n"

        "outfile       : %s\n"
        "sval_style    : %d\n"

        "cutoff        : %f\n"
        "min_dist      : %f\n"
        "out_rad       : %f\n"

        "negatives     : %d\n"
        "ngbr_style    : %d\n"
        "overwrite     : %d\n"
        "quiet         : %d\n"
        "coords_only   : %d\n"
        "true_max      : %d\n"
        "dicom_coords  : %d\n"
        "debug         : %d\n"
        "------------------------------\n",

        (void *)M->dset, (void *)M->sdata, (void *)M->result,
        M->nx, M->ny, M->nz, M->nxy, M->nvox,
        (void *)M->P.plist, M->P.used, M->P.M,
        M->extrema_count,
        M->data_type, M->adn_type, M->func_type,
        M->outfile, M->sval_style,
        M->cutoff, M->min_dist, M->out_rad,
        M->negatives, M->ngbr_style, M->overwrite,
	M->quiet, M->coords_only, M->true_max, M->dicom_coords, M->debug
    );

    EXRETURN;
}


/*----------------------------------------------------------------------
**
**  Remove mask points around lower intensity extrema.
**
**----------------------------------------------------------------------
*/
static int
apply_min_dist( maxima_s * M )
{
    int          * iptr, count;
    point_list_s   newP = { NULL, 0, 0 };


    for ( count = 0, iptr = M->P.plist; count < M->P.used; count++, iptr++ )
	clear_around_point( *iptr, M, &newP );

    free( M->P.plist );		/* replace old point list with new */
    M->P = newP;

    return 1;
}


/*----------------------------------------------------------------------
**
**  Simply clear to a radius of min_dist around the given point.
**
**  Clear all memory in the given radius and reset the point.
**
**----------------------------------------------------------------------
*/
static int
clear_around_point( int p, maxima_s * M, point_list_s * newP )
{
    int X, Y, Z;
    int xmin, xmax, ymin, ymax, zmin, zmax;
    int yc, zc, xrad, yrad, yrad2;
    int xbase, ybase, zbase;

    short * optr;
    float   radius = M->min_dist;

    static point_list_s P = { NULL, 0, 0 };  /* for allocation speed */
    P.used = 0;


    X =  p % M->nx;
    Y = (p % M->nxy) / M->nx;
    Z =  p / M->nxy;

    if ( ! *(M->result + ( Z*M->ny + Y ) * M->nx + X ) )
	return 1;

    zmin = ( Z < radius ) ? Z : radius;
    zmax = ( Z + radius >= M->nz ) ? ( M->nz-Z-1 ) : radius;

    if ( M->debug > 1 )
        fprintf(stderr,"+d index %d, val %f\n", p, M->sdata[p]*gr_fac);

    for ( zc = -zmin; zc <= zmax; zc++ )
    {
        zbase = ( Z + zc ) * M->nx * M->ny;
        yrad2 = radius * radius - zc * zc;
        yrad  = (int)sqrt( yrad2 );

        ymin = ( Y < yrad ) ? Y : yrad;
        ymax = ( Y + yrad >= M->ny ) ? ( M->ny - Y - 1 ) : yrad;

        for ( yc = -ymin; yc <= ymax; yc++ )
        {
            ybase = ( Y + yc ) * M->nx;
            xrad  = (int)sqrt( yrad2 - yc * yc );

            xmin = ( X < xrad ) ? X : xrad;
            xmax = ( X + xrad >= M->nx ) ? ( M->nx - X - 1 ) : xrad;

            optr = M->result + ybase + zbase;

            for ( xbase = X-xmin; xbase <= X+xmax; xbase++ )
		if ( *( optr + xbase ) )
		{
		    *(optr + xbase) = 0;
						/* maybe a switch later */
		    if ( M->ngbr_style == MAX_WEIGHTED_AVE_STYLE )
                    {
			if ( ! add_point_to_list( &P, xbase+ybase+zbase ) )
			    return 0;
                        if ( M->debug > 2 )
                            fprintf(stderr,"  coords %d %d %d [%d], value %f\n",
                                xbase, Y+yc, Z+zc, xbase+ybase+zbase,
                                M->sdata[xbase+ybase+zbase]*gr_fac);
                    }
		}
        }
    }

    switch( M->ngbr_style )
    {
	case MAX_SORT_N_REMOVE_STYLE :
	    if ( ! add_point_to_list( newP, p ) )
		return 0;

	    break;

	case MAX_WEIGHTED_AVE_STYLE :
	    if ( ( p = weighted_index( &P, M ) ) < 0 )
		return 0;

	    if ( ! add_point_to_list( newP, p ) )
		return 0;

	    break;

	default :
	    sprintf( grMessage, "Error: cap_00\nInvalid ngbr_style %d.",
		     M->ngbr_style );
	    rERROR( grMessage );
	    return 0;

	    break;
    }

    return 1;
}


/*----------------------------------------------------------------------
**
**  Given a list of points and the data in M, calculate the index
**  of the weighted average of those points.  Values are from M->sdata.
**
**----------------------------------------------------------------------
*/
static int
weighted_index( point_list_s * P, maxima_s * M )
{
    double  total_x = 0.0, total_y = 0.0, total_z = 0.0;  /* weight*position */
    double  weight = 0.0;
    double  value;
    int     x, y, z;
    int     count, index;
    int   * iptr;

    if ( ( P->plist == NULL ) || ( P->used <= 0 ) )
    {
	rERROR( "Error wi_00\nEmpty point list." );
	return( -1 );
    }

    if ( P->used == 1 )		/* no weighting necessary */
	return( P->plist[0] );

    for ( count = 0, iptr = P->plist; count < P->used; count++, iptr++ )
    {
	index = *iptr;

	x =   index % M->nx;
	y = ( index % M->nxy ) / M->nx;
	z =   index / M->nxy;

	value = M->sdata[ index ];

	weight  += value;
	total_x += value * x;
	total_y += value * y;
	total_z += value * z;
    }

    if ( M->debug > 1 )
        fprintf(stderr, "-d nvals, weight, ave = %d, %f, %f\n",
                P->used, weight*gr_fac, weight*gr_fac/P->used);

    if ( weight <= 0.0 )
    {
	sprintf( grMessage, "Error: wi_10\nunexpected weight of %f", weight );
	rERROR( grMessage );
    }

    x = ( int )( total_x / weight + 0.4 );	/* ~rounded average */
    y = ( int )( total_y / weight + 0.4 );
    z = ( int )( total_z / weight + 0.4 );

    index = ( z * M->ny + y ) * M->nx + x;

    if ( M->debug > 1 )
        fprintf(stderr, "-d weighted i,j,k,  ind, val = %d, %d, %d,  %d, %f\n",
                x, y, z, index, M->sdata[index]*gr_fac);

    return index;
}


/*----------------------------------------------------------------------
**
**  Create a point list of all set extremas.
**
**  Walk through the result data, and for any set point, insert into list.
**
**----------------------------------------------------------------------
*/
static int
create_point_list( maxima_s * M )
{
    short        * mptr;
    int            count;
    point_list_s * P = &M->P;

ENTRY("create_pint_list");

    mptr = M->result;
    for ( count = 0; count < M->nvox; count++ )
	if ( *mptr++ )
	    if ( ! add_point_to_list( P, count ) )
		RETURN(0);

    if ( M->debug > 0 )
	show_point_list_s( "+d point list created: ", &M->P, M->debug );

    RETURN(1);
}


/*----------------------------------------------------------------------
**
**  Add a point to the list.
**
**----------------------------------------------------------------------
*/
static int
add_point_to_list( point_list_s * P, int offset )
{
ENTRY("add_point_to_list");
    if ( ! P->plist )
    {
	P->M = 100;
	P->used = 0;

	if ( ( P->plist = (int *)malloc( P->M * sizeof(int) ) ) == NULL )
	{
	    rERROR( "Error: aptl_10\n"
		    "Failed to allocate memory for initial point list.\n" );
	    RETURN(0);
	}
    }
    else if ( P->used == P->M )
    {
	P->M = P->M + 100;

	if ( ( P->plist = (int *)realloc( P->plist, P->M*sizeof(int))) == NULL )
	{
	    sprintf( grMessage, "Error: aptl_20\n"
		    "Failed to reallocate %d ints for point list", P->M );
	    RETURN(0);
	}
    }

    P->plist[P->used] = offset;
    P->used++;

    RETURN(1);
}


/*----------------------------------------------------------------------
**
**  Fill a sphere around any set point.
**
**----------------------------------------------------------------------
*/
static int
apply_fill_radius( maxima_s * M )
{
    int    count, outval;
    int    x, y, z;
    int  * iptr;

ENTRY("apply_fill_radius");

    for ( count = 0, iptr = M->P.plist; count < M->P.used; count++, iptr++ )
    {
        outval = (M->sval_style == 0) ? MAX_MASK_FILL_VAL :
                 (M->sval_style == 1) ? (count+1)         :
                 (M->P.used - count);

	if ( M->out_rad < 1.0 )
	{
	    M->result[ *iptr ] = outval;
	    continue;
	}

	x =   *iptr % M->nx;
	y = ( *iptr % M->nxy ) / M->nx;
	z =   *iptr / M->nxy;

	radial_fill( x, y, z, M, outval );
    }

    RETURN(1);
}


/*----------------------------------------------------------------------
**
**  Fill a radius around the current point.
**
**----------------------------------------------------------------------
*/
static int
radial_fill( int X, int Y, int Z, maxima_s * M, int val )
{
    int xmin, xmax, ymin, ymax, zmin, zmax;
    int yc, zc, xrad, yrad, yrad2;
    int xbase, ybase, zbase;

    short * sptr, * optr;
    float   radius = M->out_rad;

ENTRY("radial_fill");

    zmin = ( Z < radius ) ? Z : radius;
    zmax = ( Z + radius >= M->nz ) ? ( M->nz-Z-1 ) : radius;

    for ( zc = -zmin; zc <= zmax; zc++ )
    {
        zbase = ( Z + zc ) * M->nx * M->ny;
        yrad2 = radius * radius - zc * zc;
	yrad  = (int)sqrt( yrad2 );

        ymin = ( Y < yrad ) ? Y : yrad;
        ymax = ( Y + yrad >= M->ny ) ? ( M->ny - Y - 1 ) : yrad;

        for ( yc = -ymin; yc <= ymax; yc++ )
        {
            ybase = ( Y + yc ) * M->nx;
            xrad  = (int)sqrt( yrad2 - yc * yc );

            xmin = ( X < xrad ) ? X : xrad;
            xmax = ( X + xrad >= M->nx ) ? ( M->nx - X - 1 ) : xrad;

	    optr = M->result + ybase + zbase;

            for ( xbase = X-xmin; xbase <= X+xmax; xbase++ )
	    {
		sptr = optr + xbase;

                if ( ! *sptr )
		    *sptr = val;
	    }
        }
    }

    RETURN(1);
}


/*----------------------------------------------------------------------
**
**  Find local maxima in short brick.
**
**----------------------------------------------------------------------
*/
static int
find_local_maxima( maxima_s * M )
{
    short * sourcep, * destp;
    int     c, cx, cy, cz;
    int     maxx = M->nx - 1;
    int     maxy = M->ny - 1;
    int     nx = M->nx, nxy = M->nx * M->ny;
    int     offset[ 26 ];		/* for speed */

ENTRY("find_local_maxima");

    offset[ 0] = +1;
    offset[ 1] = -1;
    offset[ 2] =  nx;
    offset[ 3] =  nx+1;
    offset[ 4] =  nx-1;
    offset[ 5] = -nx;
    offset[ 6] = -nx+1;
    offset[ 7] = -nx-1;
    offset[ 8] = nxy;
    offset[ 9] = nxy+1;
    offset[10] = nxy-1;
    offset[11] = nxy+nx;
    offset[12] = nxy+nx+1;
    offset[13] = nxy+nx-1;
    offset[14] = nxy-nx;
    offset[15] = nxy-nx+1;
    offset[16] = nxy-nx-1;
    offset[17] = -nxy;
    offset[18] = -nxy+1;
    offset[19] = -nxy-1;
    offset[20] = -nxy+nx;
    offset[21] = -nxy+nx+1;
    offset[22] = -nxy+nx-1;
    offset[23] = -nxy-nx;
    offset[24] = -nxy-nx+1;
    offset[25] = -nxy-nx-1;

    sourcep = M->sdata  + nxy;		/* skip first plane */
    destp   = M->result + nxy;

    for ( cz = 0; cz < M->nz-2; cz++ )  /* and skip last plane (1->2) [rickr] */
    {
	for ( cy = 0; cy < M->ny; cy++ )
	{
	    if ( ( cy == 0 ) || ( cy == maxy ) )
	    {
		sourcep += nx;
		destp += nx;

		continue;
	    }

	    for ( cx = 0; cx < M->nx; cx++ )
	    {
		if ( ( cx == 0 ) || ( cx == maxx ) )
		{
		    sourcep++;
		    destp++;

		    continue;
		}

		if ( ! M->negatives && ( *sourcep < M->cutoff ) )
		{
		    sourcep++;
		    destp++;

		    continue;
		}

		if ( M->negatives && ( *sourcep > M->cutoff ) )
		{
		    sourcep++;
		    destp++;

		    continue;
		}

		*destp = MAX_MASK_FILL_VAL;
		M->extrema_count++;

		if ( M->true_max )
		{
		    if ( ! M->negatives )
		    {
			for ( c = 0; c < 26; c++ )
			    if ( *sourcep <= sourcep[offset[c]] )
			    {
				*destp = 0;
				M->extrema_count--;

				break;
			    }
		    }
		    else
		    {
			for ( c = 0; c < 26; c++ )
			    if ( *sourcep >= sourcep[offset[c]] )
			    {
				*destp = 0;
				M->extrema_count--;

				break;
			    }
		    }
		}
		else
		{
		    if ( ! M->negatives )
		    {
			for ( c = 0; c < 26; c++ )
			    if ( *sourcep < sourcep[offset[c]] )
			    {
				*destp = 0;
				M->extrema_count--;

				break;
			    }
		    }
		    else
		    {
			for ( c = 0; c < 26; c++ )
			    if ( *sourcep > sourcep[offset[c]] )
			    {
				*destp = 0;
				M->extrema_count--;

				break;
			    }
		    }
		}

		sourcep++;
		destp++;
	    }
	}
    }

    if ( M->debug > 3 ) show_maxima_s( "post find local maxima: ", M );

    RETURN(1);
}


/*----------------------------------------------------------------------
**
**  Display extrema coordinates and write output BRIK.
**
**  Put any new BRIK into memory.
**
**----------------------------------------------------------------------
*/
int set_results( r_afni_s * A, maxima_s * M, THD_3dim_dataset * dset )
{
ENTRY("write_results");

    if ( ! dset ) RETURN(1);

    EDIT_dset_items( dset,
	ADN_prefix,	M->outfile,
	ADN_label1,	M->outfile,
	ADN_nvals,	1,
	ADN_ntt,	0,
	ADN_type,     	HEAD_FUNC_TYPE,
	ADN_func_type,	FUNC_FIM_TYPE,
	ADN_none
	);

    EDIT_substitute_brick( dset, 0, M->data_type, M->result );
    EDIT_BRICK_FACTOR    ( dset, 0, 0.0 );

    RETURN(1);
}


/*----------------------------------------------------------------------
 * print a list of strings
**----------------------------------------------------------------------
*/
int disp_str_list( char * list[], int len )
{
    int c;
    for( c = 0; c < len; c++ ) puts(list[c]);
    fflush(stdout);
    return 0;
}


/*----------------------------------------------------------------------
**
**  For every point in list, display the talairach coordinates.
**
**----------------------------------------------------------------------
*/
int display_coords( r_afni_s * A, maxima_s * M )
{
    THD_fvec3 f3, t3;
    THD_ivec3 i3;
    float   prod, factor = A->factor[0];
    short * optr;
    short * mptr;
    int   * iptr;
    int     X, Y, Z, count;

    THD_coorder cord;           /* 16 Apr 2013 */
    char        orcode[4];

ENTRY("display_coords");

    /* For dset based coordinates (including signs), use THD_coorder struct
       and THD_dicom_to_coorder().  Previously (i.e. for past 15 years), the
       dataset coord order just permuted the axes, but did not apply signs.
       This issue was reported by G Pagnoni.            16 Apr 2013 [rickr]*/
    orcode[0] = ORIENT_first[M->dset->daxes->xxorient];
    orcode[1] = ORIENT_first[M->dset->daxes->yyorient];
    orcode[2] = ORIENT_first[M->dset->daxes->zzorient];
    orcode[3] = '\0';
    THD_coorder_fill(orcode, &cord);

    point_list_s * P = &M->P;

    if( !M->coords_only )  /* 18 Aug 2006 [rickr] */
    {
        printf( "---------------------------------------------\n" );
        if ( M->dicom_coords )
            printf( "RAI mm coordinates:\n\n" );
        else
            printf( "%3s mm coordinates:\n\n", orcode);
    }

    for ( count = 0, iptr = P->plist; count < P->used; count++, iptr++ )
    {
	X =  *iptr % M->nx;
	Y = (*iptr % M->nxy) / M->nx;
	Z =  *iptr / M->nxy;
	i3.ijk[0] = X;  i3.ijk[1] = Y;  i3.ijk[2] = Z;
	f3 = THD_3dind_to_dicomm_no_wod(M->dset, i3);
        if ( ! M->dicom_coords ) /* first dicom, then invert  16 Apr 2013 */
           THD_dicom_to_coorder(&cord, f3.xyz, f3.xyz+1, f3.xyz+2);

	optr = M->sdata  + *iptr;
	mptr = M->result + *iptr;

	if ( M->coords_only )
	    printf( "%7.2f  %7.2f  %7.2f\n", f3.xyz[0], f3.xyz[1], f3.xyz[2] );
	else
        {
            prod = *optr * factor;
	    printf( "(%7.2f  %7.2f  %7.2f) : val = %f\n",
		    f3.xyz[0], f3.xyz[1], f3.xyz[2], prod );
        }
    }

    if( !M->coords_only )
    {
        if ( P->used )
            printf( "\nnumber of extrema = %d\n", P->used );
        else
            printf( "No extrema found.\n" );
        printf( "---------------------------------------------\n" );
    }

    RETURN(1);
}


/*----------------------------------------------------------------------
**
**  Initialize the afni structure.
**
**  These are the fields that need to be set before reading a dataset.
**
**----------------------------------------------------------------------
*/
int init_afni_s( r_afni_s * A )
{
ENTRY("init_afni_s");

    memset( A, 0, sizeof( r_afni_s ) );

    A->must_be_short   = 1;
    A->want_floats     = 1;
    A->subs_must_equal = 1;
    A->max_subs        = 1;
    A->sub_brick       = 0;

    RETURN(1);
}


/*----------------------------------------------------------------------
**
**      Initialize the maxima structure.
**
**----------------------------------------------------------------------
*/
int init_maxima_s( maxima_s * M, r_afni_s * A, char * outprefix )
{
ENTRY("init_maxima_s");

    M->dset   = A->dset[0];

    M->sdata = A->simage[0];

    if ( ( M->result = (short *)calloc( A->nvox, sizeof( short ) ) ) == NULL )
    {
        sprintf( grMessage, "Error: ims_05\n"
		 "Failed to allocate M for %d shorts.", A->nvox );
	rERROR( grMessage );
	RETURN(0);
    }

    M->nx 	 = A->nx;
    M->ny	 = A->ny;
    M->nz	 = A->nz;
    M->nxy	 = A->nx * A->ny;
    M->nvox	 = A->nvox;

    M->P.plist   = NULL;
    M->P.used    = 0;
    M->P.M       = 0;

    M->extrema_count = 0;

    M->data_type = MRI_short;		/* output will be short */
    M->adn_type  = HEAD_FUNC_TYPE;
    M->func_type = FUNC_FIM_TYPE;

    if ( outprefix && strlen( outprefix ) > R_FILE_L )
    {
        sprintf( grMessage, "Error: ims_10\n"
		 "Outfile prefix exceeds %d characters.", R_FILE_L );
	rERROR( grMessage );
	RETURN(0);
    }

    if ( outprefix )
	strcpy( M->outfile, outprefix );
    else
	*M->outfile = 0;

    M->cutoff       = 0.0;
    M->min_dist     = 0.0;
    M->out_rad      = 0.0;

    M->negatives    = 0;
    M->ngbr_style   = MAX_SORT_N_REMOVE_STYLE;
    M->overwrite    = 0;
    M->quiet        = 0;
    M->coords_only  = 0;
    M->true_max     = 0;
    M->debug        = 0;

    RETURN(1);
}


/*----------------------------------------------------------------------
**
**  Comparasin function of positives for qsort.
**
**----------------------------------------------------------------------
*/
int
point_comp_pos( const void * p1, const void * p2 )
{
    short v1 = *( gr_orig_data + *(int *)p1 );
    short v2 = *( gr_orig_data + *(int *)p2 );

    if ( v1 < v2 )		/* reverse order to get large numbers first */
	return 1;
    else if ( v1 > v2 )
	return -1;

    return 0;
}


/*----------------------------------------------------------------------
**
**  Comparasin function of negatives for qsort.
**
**----------------------------------------------------------------------
*/
int
point_comp_neg( const void * p1, const void * p2 )
{
    short v1 = *( gr_orig_data + *(int *)p1 );
    short v2 = *( gr_orig_data + *(int *)p2 );

    /* more negative is greater AND reverse the order */
    if ( v1 > v2 )
	return 1;
    else if ( v1 < v2 )
	return -1;


    return 0;
}


/*----------------------------------------------------------------------
**
**  Fill the afni structure from an existing dataset.
**
**----------------------------------------------------------------------
*/
int
r_set_afni_s_from_dset( r_afni_s * A, THD_3dim_dataset * dset, int debug )
{
ENTRY("r_set_afni_s_from_dset");

    if( debug > 3 ) disp_r_afni_s( "-d initial struct", A);

    if ( A->num_dsets >= R_MAX_AFNI_DSETS )
    {
        sprintf( grMessage, "Error: rsasfd_00\n"
                 "We only have memory to hold %d datasets.    exiting...\n",
                 R_MAX_AFNI_DSETS );
        rERROR( grMessage );

        RETURN(0);
    }

    A->dset[ 0 ] = dset;                 /* rickr - use sub-brick */
    A->simage[ 0 ] = ( short * )DSET_ARRAY( dset, A->sub_brick );

    if ( !A->simage[0] )
    {
	sprintf(grMessage,
            "** data not available, is this in warp-on-demand mode?\n");
	rERROR(grMessage);
	RETURN(0);
    }

    if ((A->factor[0] = DSET_BRICK_FACTOR(dset, A->sub_brick)) == 0.0 )
        A->factor[0] = 1.0;

    A->subs  [ 0 ] = DSET_NVALS( dset );

    A->nx   = dset->daxes->nxx;
    A->ny   = dset->daxes->nyy;
    A->nz   = dset->daxes->nzz;
    A->nvox = A->nx * A->ny * A->nz;

    if ( A->want_floats )
    {
        int     count;
        short * sptr;
        float * fptr;
        float   factor = A->factor[ 0 ];   /* just for speed */

        if ( ( A->fimage[ 0 ] =
                ( float * )calloc( A->nvox, sizeof( float ) ) ) == NULL )
        {
            sprintf( grMessage, "Error: rsasfd_10\n"
                     "Failed to allocate memory for %d floats.\n", A->nvox );
            rERROR( grMessage );

            RETURN(0);
        }

        fptr = A->fimage[ 0 ];
        sptr = A->simage[ 0 ];
        for ( count = 0; count < A->nvox; count++ )
            *fptr++ = *sptr++ * factor;
    }

    A->max_u_short  = r_get_max_u_short( (u_short *)A->simage[0], A->nvox );

/*    A->num_dsets++;   not using more than one */

    if( debug > 1 ) disp_r_afni_s( "-d initial struct", A);

    RETURN(1);
}


/*----------------------------------------------------------------------
**
**  Compute the maximum unsigned short in an array.
**
**----------------------------------------------------------------------
*/
u_short
r_get_max_u_short( u_short * S, int size )
{
    u_short * usptr, max = *S;
    int       c = 0;

    for ( c = 0, usptr = S; c < size; c++, usptr++ )
    {
        if ( *usptr > max )
            max = *usptr;
    }

    return max;
}


/*----------------------------------------------------------------------
**
**  Free all local memory.
**
**----------------------------------------------------------------------
*/
void free_memory( r_afni_s * A, maxima_s * M )
{
ENTRY("free_memory");
    if ( A->want_floats && A->fimage[0] )
	free( A->fimage[0] );

    if ( M->result && !M->outfile[0] )
	free( M->result );

    if ( M->P.plist )
	free( M->P.plist );

    EXRETURN;
}


/*----------------------------------------------------------------------
 *  display contents of r_afni_s struct
 *----------------------------------------------------------------------
*/
int disp_r_afni_s( char * mesg, r_afni_s * A )
{
ENTRY("disp_r_afni_s");

    if( mesg ) puts(mesg);

    printf("-d r_afni_s @ %p\n"
           "     must_be_short, want_floats    = %d, %d\n"
           "     subs_must_equal, max_subs     = %d, %d\n"
           "     dset, simage                  = %p, %p\n"
           "     factor                        = %f\n"
           "     subs, sub_brick               = %d, %d\n"
           "     nx, ny, nz, nvox              = %d, %d, %d, %d\n"
           "     fimage                        = %p\n"
           "     max_u_short, num_dsets        = %d, %d\n",
           A,
           A->must_be_short, A->want_floats,
           A->subs_must_equal, A->max_subs,
           (void *)A->dset[0], (void *)A->simage[0],
           A->factor[0], A->subs[0], A->sub_brick,
           A->nx, A->ny, A->nz, A->nvox, (void *)A->fimage,
           A->max_u_short, A->num_dsets);

    return 0;
}

