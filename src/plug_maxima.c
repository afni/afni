/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/
   
/*----------------------------------------------------------------------
 *
 *  plug_maxima.c	- AFNI plugin to locate relative extrema
 *
 *----------------------------------------------------------------------
*/

/*----------------------------------------------------------------------
 * history:
 *
 * 2004 Feb 20 [rickr]
 *   - added ENTRY/RETURN calls to most functions
 *   - do not process last plane in find_local_maxima()
 *   - allow any anat or func dataset of type short
 *   - added a sub-brick selector
 *   - note that dist and radius are in voxels
 *   - output coordinates in RAI mm format
 *----------------------------------------------------------------------
*/

#include "afni.h"
#include "plug_maxima.h"

#ifndef ALLOW_PLUGINS
#  error "Plugins not properly set up -- see machdep.h"
#endif

/***********************************************************************
  plugin to find local extrema
************************************************************************/

char * MAXIMA_main( PLUGIN_interface * );

char               grMessage[ R_MESSAGE_L ];
static char      * grStyle[] = { "Sort-n-Remove", "Weighted-Average" };

static char        helpstring[] =
    "Maxima - used to locate extrema in a functional dataset.\n"
    "\n"
    "This plugin reads a functional dataset and locates any relative extrema\n"
    "(maximums or minimums, depending on the user option).  A _relative_\n"
    "maximum is a point that is greater than all neighbors.  The output from\n"
    "this proces can be text based (and sent to the command-line window) and\n"
    "it can be a unit BRIK, where the locations of the extrema are set.\n"
    "\n"
    "When writing a BRIK, it is often useful to set a sphere around each\n"
    "extrema, not just use single points.  This makes viewing those locations\n"
    "much more reasonable.\n"
    "\n"
    "Notes : The only required option is the input dataset.\n"
    "        All distances are in voxel units.\n"
    "\n"
    "                        ***  Options  ***\n"
    "\n"
    "Input        - functional dataset to locate extrema within\n"
    "\n"
    "Output       - prefix for an output functional dataset\n"
    "\n"
    "               This dataset may be viewed as a mask.  Its set points\n"
    "               will correspond to any selected extrema.  The \"Output\n"
    "               Size\" option will change those points to spheres.\n"
    "\n"
    "Threshold    - provides a cutoff value for extrema\n"
    "\n"
    "               Extrema not meeting this cutoff will not be considered.\n"
    "               Note that if the \"Neg Extrema\" option is applied, the\n"
    "               user will generally want a negative threshold.\n"
    "\n"
    "Separation   - minimum acceptable distance between extrema\n"
    "\n"
    "               Less significant extrema which are too close to more\n"
    "               significant extrema will be discounted in some way.\n"
    "               This option works with the \"Neighbor Style\" option.\n"
    "\n"
    "Output Size  - output mask radius around set points\n"
    "\n"
    "               If the user wants the output BRIK to consist of spheres\n"
    "               centered around extrema points, this option can be used\n"
    "               to set the radius for those spheres.\n"
    "\n"
    "Neg Extrema  - search for negative extrema (minima)\n"
    "\n"
    "               This will search for the minima of the dataset.\n"
    "               Note that a negative threshold may be desired.\n"
    "\n"
    "Neighbor     - technique for handling clustered extrema\n"
    "\n"
    "               If extrema are not as far apart as is specified by the\n"
    "               \"Separation\" option, the Neighbor (Style) option\n"
    "               specifies how to handle those points.\n"
    "\n"
    "               Sort-n-Remove : The extrema are sorted by magnitude.\n"
    "                  For each such extrema, if the extrema is still set\n"
    "                  (not previously removed), all (less significant)\n"
    "                  neighbors within the Separation radius are removed.\n"
    "\n"
    "               Weighted-Average : Again, traverse the sorted list of\n"
    "                  extrema.  Set the current extrema to the center of\n"
    "                  mass of all extrema within the Separation radius.\n"
    "\n"
    "True Max     - do not consider extrema with an equal valued neighbor\n"
    "\n"
    "               As a default points may be considered extrema, even if\n"
    "               they have a neighbor of the same value.  Setting this\n"
    "               option requires extrema to be stricktly greater than\n"
    "               any of their neighbors.\n"
    "               Note that with this set, extrema locations that have\n"
    "               more than one voxel at the maximal value are ignored.\n"
    "\n"
    "No Text Out  - do not display the extrma points as text\n"
    "\n"
    "Author: (the lamented) R Reynolds\n"
    "\n"
    "Updated: 2004 Feb 20 [rickr]\n"
   ;



/***********************************************************************
   Set up the interface to the user
************************************************************************/


DEFINE_PLUGIN_PROTOTYPE

PLUGIN_interface * PLUGIN_init( int ncall )
{
   PLUGIN_interface * plint ;

   if( ncall > 0 ) return NULL ;  /* only one interface */

   /* create the new interface */

   plint = PLUTO_new_interface( "Maxima", "find extrema in a dataset",
		helpstring, PLUGIN_CALL_VIA_MENU , MAXIMA_main );

   PLUTO_add_hint( plint, "find local maxima/minima" );

   PLUTO_set_sequence( plint , "z:Reynolds" ) ;

   /*-- first line of input: input dataset --*/

   PLUTO_add_option( plint, "Input" , "Input" , TRUE );
   PLUTO_add_hint( plint, "choose dataset for input" );
   PLUTO_add_dataset(plint, "Dataset" , ANAT_ALL_MASK , FUNC_ALL_MASK, 
                                         DIMEN_ALL_MASK | BRICK_SHORT_MASK );
   PLUTO_add_number( plint , "Sub-brick" , 0,9999,0 , 0,1 ) ; /* new [rickr] */


   /*-- second line of input: prefix for output dataset --*/

   PLUTO_add_option( plint, "Output" , "prefix" , FALSE );
   PLUTO_add_hint( plint, "option: choose dataset prefix for output" );
   PLUTO_add_string( plint, "Prefix", 0 , NULL, 19 );

   /*-- third line of input: cutoff option --*/

   PLUTO_add_option( plint, "Threshold" , "cutoff" , FALSE ) ;
   PLUTO_add_hint( plint, "option: choose a threshold for value at extrema" );
   PLUTO_add_number( plint, "Cutoff", -20000, 20000, 0, 1000, 1 );

   /*-- fourth line of input: min_dist option --*/

   PLUTO_add_option( plint, "Separation" , "min_dist" , FALSE ) ;
   PLUTO_add_hint( plint, "option: choose a minimum distance between extrema" );
   PLUTO_add_number( plint, "Distance(vox)", 0, 1000, 1, 80, 1 );

   /*-- fifth line of input: out_rad option --*/

   PLUTO_add_option( plint, "Output Size" , "out_rad" , FALSE ) ;
   PLUTO_add_hint( plint, "option: choose a spherical radius around extrema "
			  "points in mask" );
   PLUTO_add_number( plint, "Radius(vox)", 0, 1000, 1, 50, 1 );

   /*-- sixth line of input: negatives option --*/

   PLUTO_add_option( plint, "Neg Extrema" , "negatives" , FALSE ) ;
   PLUTO_add_hint( plint, "option: search for negative extrema" );

   /*-- seventh line of input: style option --*/

   PLUTO_add_option( plint, "Neighbor" , "style" , FALSE ) ;
   PLUTO_add_hint( plint, "option: technique for neighbor removal" );
   PLUTO_add_string( plint, "Style", 2, grStyle, 0 );

   /*-- eighth line of input: true_max option --*/

   PLUTO_add_option( plint, "True Max" , "true_max" , FALSE ) ;
   PLUTO_add_hint( plint, "option: do not include extrema with "
			  "equal neighbors" );

   /*-- ninth line of input: quiet option --*/

   PLUTO_add_option( plint, "No Text Out" , "quiet" , FALSE ) ;
   PLUTO_add_hint( plint, "option: do not output extrema as text" );


   return plint;
}


/*----------------------------------------------------------------------
**
**  Main routine for this plugin (will be called from AFNI).
**
**----------------------------------------------------------------------
*/
char * MAXIMA_main( PLUGIN_interface * plint )
{
    r_afni_s   A;
    maxima_s   M;
    char     * ret_string = NULL;


    if ( ( ret_string = process_args( &A, &M, plint ) ) != NULL )
	return ret_string;

    if ( ! process_data( &M ) )
	return  "************************************\n"
	        "MAXIMA_main: data processing failure\n"
		"************************************";

    if ( ! write_results( &A, &M, plint ) )
	return  "***********************************\n"
	        "MAXIMA_main: result writing failure\n"
		"***********************************";

    free_memory( &A, &M );

    return NULL;
}


/*----------------------------------------------------------------------
**
**  Process arguments.
**
**  Return a description string on failure.
**
**----------------------------------------------------------------------
*/
static char *
process_args( r_afni_s * A, maxima_s * M, PLUGIN_interface * plint )
{
    THD_3dim_dataset * dset;
    MCW_idcode       * idc ;
    char             * optag, * infile = NULL, * outfile = NULL, * style_str;
    float              cutoff = 0.0, min_dist = 0.0, out_rad = 0.0;
    int                negatives = 0, quiet = 0, true_max = 0, opcnt = 0;
    int                style = MAX_SORT_N_REMOVE_STYLE, sb;

ENTRY("process_args");
    /* get AFNI inputs */

    if( plint == NULL )
	RETURN("----------------------\n"
               "arguments : NULL input\n"
               "----------------------");

    if ( ! init_afni_s( A ) )
	RETURN( "------------------------\n"
		"arguments : init failure\n"
		"------------------------");

    PLUTO_next_option( plint );
    idc  = PLUTO_get_idcode( plint );
    dset = PLUTO_find_dset( idc );

    if( dset == NULL )
	RETURN("-----------------------------\n"
               "arguments : bad input dataset\n"
               "-----------------------------");

    sb = (int)PLUTO_get_number( plint );	/* 2004 Feb 20 [rickr] */
    if ( sb >= DSET_NVALS(dset) || sb < 0 )
	RETURN("--------------------------\n"
               "arguments : bad sub-brick \n"
               "--------------------------");
    A->sub_brick = sb;

    DSET_load( dset );

    for ( optag  = PLUTO_get_optiontag( plint );
	  optag != NULL;
	  optag  = PLUTO_get_optiontag( plint )
	)
    {
	if ( ! strcmp( optag, "prefix" ) )
	{
	    outfile = PLUTO_get_string( plint );
	    if ( ! PLUTO_prefix_ok( outfile ) )
		RETURN( "-------------------------\n"
		        "options : bad file prefix\n"
			"-------------------------");
	}
	else if ( ! strcmp( optag, "cutoff" ) )
	{
	    cutoff = PLUTO_get_number( plint );
	}
	else if ( ! strcmp( optag, "min_dist" ) )
	{
	    if ( ( min_dist = PLUTO_get_number( plint ) ) < 0 )
		RETURN( "-----------------------------------------\n"
			"options : Separation must be non-negative\n"
			"-----------------------------------------");
	}
	else if ( ! strcmp( optag, "out_rad" ) )
	{
	    if ( ( out_rad = PLUTO_get_number( plint ) ) < 0 )
		RETURN( "--------------------------------------------\n"
			"options : Output radius must be non-negative\n"
			"--------------------------------------------");
	}
	else if ( ! strcmp( optag, "negatives" ) )
	{
	    negatives = 1;
	}
	else if ( ! strcmp( optag, "style" ) )
	{
	    if ( ( style_str = PLUTO_get_string( plint ) ) == NULL )
		RETURN( "-------------------------------\n"
			"options : missing style string?\n"
			"-------------------------------");
	    if ((( style = PLUTO_string_index(style_str, 2, grStyle)) 
				< 0 ) || ( style > MAX_MAX_STYLE ) )
	    {
		sprintf( grMessage,
		    "---------------------------\n"
		    "options : bad style is %d\n"
		    "---------------------------", style );
		RETURN(grMessage);
	    }
	}
	else if ( ! strcmp( optag, "true_max" ) )
	{
	    true_max = 1;
	}
	else if ( ! strcmp( optag, "quiet" ) )
	{
	    quiet = 1;
	}
	else	/* illegal option? */
	{
	    sprintf( grMessage, "Error: pa_00\n"
		     "Unexpected option #%d: '%s'", opcnt, optag );
	    RETURN(grMessage);
	}

	opcnt++;
    }

    if ( ( out_rad > 0 ) && ( outfile == NULL ) )
	RETURN( "------------------------------------------------\n"
                "arguments : specify outfile to use output radius\n"
                "------------------------------------------------");

    if ( ! r_set_afni_s_from_dset( A, dset ) )
	RETURN( "-------------------------------\n"
                "arguments : afni_s init failure\n"
                "-------------------------------");

    if ( ! init_maxima_s( M, A, outfile ) )
	RETURN("----------------------------------\n"
               "MAXIMA_main: maxima_s init failure\n"
               "----------------------------------");

    /* now fill any remaining parameters */
    M->cutoff     = cutoff/A->factor[0];
    M->min_dist   = min_dist;
    M->out_rad    = out_rad;

    M->negatives  = negatives;
    M->ngbr_style = style;
    M->quiet      = quiet;
    M->true_max   = true_max;

    RETURN(NULL);
}


/*----------------------------------------------------------------------
**
**  Find local maxima, storing the extrema as a mask.
**
**----------------------------------------------------------------------
*/
static int
process_data( maxima_s * M )
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

    if ( ( M->min_dist > 1.0 ) && ! apply_min_dist( M ) )
        RETURN(0);

    if ( M->outfile )
        apply_fill_radius( M );

    RETURN(1);
}


/*----------------------------------------------------------------------
**
**  Display the contents of the maxima structure.
**
**----------------------------------------------------------------------
*/
static void
show_maxima_s( maxima_s * M )
{
    fprintf( stderr,
        "------------------------------\n"
        "dset   *      : %x\n"
        "sdata  *      : %x\n"
        "result *      : %x\n"
        "nx            : %d\n"
        "ny            : %d\n"
        "nz            : %d\n"
        "nxy           : %d\n"
        "nvox          : %d\n"

        "P.plist       : %x\n"
        "P.used        : %d\n"
        "P.M           : %d\n"

        "extrema count : %d\n"

        "data_type     : %d\n"
        "adn_type      : %d\n"
        "func_type     : %d\n"

        "outfile       : %s\n"

        "cutoff        : %f\n"
        "min_dist      : %f\n"
        "out_rad       : %f\n"

        "negatives     : %d\n"
        "ngbr_style    : %d\n"
        "overwrite     : %d\n"
        "quiet         : %d\n"
        "true_max      : %d\n"
        "------------------------------\n",

        (int)M->dset, (int)M->sdata, (int)M->result,
        M->nx, M->ny, M->nz, M->nxy, M->nvox,
        (int)M->P.plist, M->P.used, M->P.M,
        M->extrema_count,
        M->data_type, M->adn_type, M->func_type,
        M->outfile,
        M->cutoff, M->min_dist, M->out_rad,
        M->negatives, M->ngbr_style, M->overwrite, M->quiet, M->true_max
    );
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

    short * sptr, * optr, * mptr;
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
			if ( ! add_point_to_list( &P, xbase+ybase+zbase ) )
			    return 0;
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

    if ( weight <= 0.0 )
    {
	sprintf( grMessage, "Error: wi_10\nunexpected weight of %f", weight );
	rERROR( grMessage );
    }

    x = ( int )( total_x / weight + 0.4 );	/* ~rounded average */
    y = ( int )( total_y / weight + 0.4 );
    z = ( int )( total_z / weight + 0.4 );

    index = ( z * M->ny + y ) * M->nx + x;

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
    int    count;
    int    x, y, z;
    int  * iptr;

ENTRY("apply_fill_radius");

    for ( count = 0, iptr = M->P.plist; count < M->P.used; count++, iptr++ )
    {
	if ( M->out_rad < 1.0 )
	{
	    M->result[ *iptr ] = MAX_MASK_FILL_VAL;
	    continue;
	}

	x =   *iptr % M->nx;
	y = ( *iptr % M->nxy ) / M->nx;
	z =   *iptr / M->nxy;

	radial_fill( x, y, z, M );
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
radial_fill( int X, int Y, int Z, maxima_s * M )
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
		    *sptr = MAX_MASK_FILL_VAL;
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
		else
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

		sourcep++;
		destp++;
	    }
	}
    }

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
static int
write_results( r_afni_s * A, maxima_s * M, PLUGIN_interface * plint )
{
    THD_3dim_dataset * newdset;

ENTRY("write_results");

    if ( ! M->quiet )
	display_coords( A, M );

    if ( ! *M->outfile )
	RETURN(1);


    /* actually write a new dataset */

    if ( ( newdset = EDIT_empty_copy( M->dset ) ) == NULL )
    {
	rERROR( "Error: wr_00\n" "Failed to copy dataset." );
	RETURN(0);
    }

    { char * his = PLUTO_commandstring(plint) ;
      tross_Copy_History( M->dset , newdset ) ;
      tross_Append_History( newdset , his ) ; free(his) ;
    }

    EDIT_dset_items( newdset,
	ADN_prefix,	M->outfile,
	ADN_label1,	M->outfile,
	ADN_nvals,	1,
	ADN_ntt,	0,
	ADN_type,     	HEAD_FUNC_TYPE,
	ADN_func_type,	FUNC_FIM_TYPE,
	ADN_none
	);

    EDIT_substitute_brick( newdset, 0, M->data_type, M->result );
    EDIT_BRICK_FACTOR    ( newdset, 0, 0.0 );

    if ( PLUTO_add_dset( plint, newdset, DSET_ACTION_MAKE_CURRENT ) )
    {
	rERROR( "Error: wr_10\n" "Failed to make current dataset." );
	RETURN(0);
    }
    else
	DSET_unload( M->dset );

    RETURN(1);
}


/*----------------------------------------------------------------------
**
**  For ever point in list, display the talairach coordinates.
**
**----------------------------------------------------------------------
*/
static int
display_coords( r_afni_s * A, maxima_s * M )
{
    THD_fvec3 f3;
    THD_ivec3 i3;
    float   xmin = M->dset->daxes->xxmin;
    float   xlen = M->dset->daxes->xxmax - xmin;
    float   ymin = M->dset->daxes->yymin;
    float   ylen = M->dset->daxes->yymax - ymin;
    float   zmin = M->dset->daxes->zzmin;
    float   zlen = M->dset->daxes->zzmax - zmin;
    float   prod, factor = A->factor[0];
    short * optr;
    short * mptr;
    int   * iptr;
    int     X, Y, Z, offset, count;
    int     xm1 = M->nx - 1, ym1 = M->ny - 1, zm1 = M->nz - 1;

    point_list_s * P = &M->P;

ENTRY("display_coords");

    printf( "---------------------------------------------\n" );
    printf( "RAI mm coordinates:\n\n" );

    for ( count = 0, iptr = P->plist; count < P->used; count++, iptr++ )
    {
	X =  *iptr % M->nx;
	Y = (*iptr % M->nxy) / M->nx;
	Z =  *iptr / M->nxy;
	i3.ijk[0] = X;  i3.ijk[1] = Y;  i3.ijk[2] = Z;
	f3 = THD_3dind_to_3dmm(M->dset, i3);
	f3 = THD_3dmm_to_dicomm(M->dset, f3);

	optr   = M->sdata  + *iptr;
	mptr   = M->result + *iptr;
    
	if ( factor == 1 )
	{
	    /* do dicom coordinates from ijk, instead */
	    printf( "(%.2f  %.2f  %.2f) : val = %d\n",
		    f3.xyz[0], f3.xyz[1], f3.xyz[2], *optr );

/*     printf( "(%4d,%4d,%4d) : val = %d\n",
		(int)(xmin)+(int)(xlen*X/xm1),
		(int)(ymin)+(int)(ylen*Y/ym1),
		(int)(zmin)+(int)(zlen*Z/zm1),
		*optr );
*/
	}
	else
	{
	    prod = *optr * factor;

	    printf( "(%.2f  %.2f  %.2f) : val = %f\n",
		    f3.xyz[0], f3.xyz[1], f3.xyz[2], prod );

/*	    printf( "(%4d,%4d,%4d) : val = %f\n",
		(int)(xmin)+(int)(xlen*X/xm1),
		(int)(ymin)+(int)(ylen*Y/ym1),
		(int)(zmin)+(int)(zlen*Z/zm1),
		prod );
*/
	}
    }

    if ( P->used )
	printf( "\nnumber of extrema = %d\n", P->used );
    else
	printf( "No extrema found.\n" );
    printf( "---------------------------------------------\n" );


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
static int
init_afni_s( r_afni_s * A )
{
ENTRY("init_afni_s");

    memset( A, 0, sizeof( r_afni_s ) );

    A->must_be_short   = 1;
    A->want_floats     = 1;
    A->subs_must_equal = 1;
    A->max_subs        = 1;

    RETURN(1);
}


/*----------------------------------------------------------------------
**
**      Initialize the maxima structure.
**
**----------------------------------------------------------------------
*/
static int
init_maxima_s( maxima_s * M, r_afni_s * A, char * outprefix )
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
    M->true_max     = 0;

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
r_set_afni_s_from_dset( r_afni_s * A, THD_3dim_dataset * dset )
{
ENTRY("r_set_afni_s_from_dset");

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
static void
free_memory( r_afni_s * A, maxima_s * M )
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


