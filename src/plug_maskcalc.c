/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/
   
/***********************************************************************
 *
 * plug_maskcalc.c		- plugin to do mask-based computations
 *
 * $Log$
 * Revision 1.6  2004/01/07 19:50:37  rwcox
 * Cput
 *
 * Revision 1.5  2003/07/15 13:28:30  rwcox
 * Cput
 *
 * Revision 1.4  2003/06/25 20:45:07  rwcox
 * Cput
 *
 * Revision 1.3  2000/12/21 16:10:54  cox
 * AFNI
 *
 * Revision 1.1  1999/08/06 19:10:48  cox
 * AFNI
 *
 * Revision 1.1  1998/11/12 18:11:06  rickr
 * Initial revision
 *
 ***********************************************************************
*/
#include <stdio.h>
#include <math.h>
#include <stdlib.h>
#if !(defined(DARWIN) || defined(FreeBSD))
#  include <malloc.h>
#endif
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>

#include "mrilib.h"
#include "afni.h"
#include "plug_maskcalc.h"

#ifndef ALLOW_PLUGINS
#  error "Plugins not properly set up -- see machdep.h"
#endif

char * MASKCALC_main( PLUGIN_interface * );

static char   grMessage[ R_MESSAGE_L ];

static char * gr_help_message = 
       "maskcalc plugin - rickr";


DEFINE_PLUGIN_PROTOTYPE

PLUGIN_interface * PLUGIN_init( int ncall )
{
    PLUGIN_interface * plint;

    if ( ncall > 0 )
	return NULL;		/* only one interface */

    plint = PLUTO_new_interface( "maskcalc", "masked computations on datasets",
	    gr_help_message, PLUGIN_CALL_VIA_MENU, MASKCALC_main );

    PLUTO_add_hint( plint, "Wouldn't some cookies be right tasty?" );

    PLUTO_set_sequence( plint , "z:Reynolds" ) ;

    /* first input : the operation */

    PLUTO_add_option( plint, "Function", "op_st", TRUE );
    PLUTO_add_hint  ( plint, "function to perform on the data" );
    PLUTO_add_string( plint, "Operation", gr_num_ops, gr_op_strings, 0 );
    PLUTO_add_hint  ( plint, "function to perform on the data" );

    /* second input : the mask */

    PLUTO_add_option ( plint, "Dataset", "mask_st", TRUE );
    PLUTO_add_hint   ( plint, "dataset to be used as mask" );
    PLUTO_add_dataset( plint, "Mask", 0, FUNC_FIM_MASK,
			    DIMEN_3D_MASK | DIMEN_4D_MASK | BRICK_SHORT_MASK );
    PLUTO_add_hint   ( plint, "dataset to be used as mask" );

    /* third input : the computational dataset */

    PLUTO_add_option ( plint, "Dataset", "dset_st", TRUE );
    PLUTO_add_hint   ( plint, "computational dataset" );
    PLUTO_add_dataset( plint, "Dset", ANAT_ALL_MASK, FUNC_FIM_MASK,
				DIMEN_ALL_MASK | BRICK_SHORT_MASK );
    PLUTO_add_hint   ( plint, "dataset to be used for computation" );

    /* fourth input : optional output file */

    PLUTO_add_option( plint, "Output", "ofile_st", FALSE );
    PLUTO_add_string( plint, "Outfile", 0, NULL, 0 );
    PLUTO_add_hint  ( plint, "file for statistical output" );
    PLUTO_add_string( plint, "Overwrite", gr_num_yn_strings, gr_yn_strings, 1 );
    PLUTO_add_hint  ( plint, "option to overwrite output file" );

    /* fifth input : minimum cutoff */
    PLUTO_add_option( plint, "Cutoff", "min_st", FALSE );
    PLUTO_add_number( plint, "Min", -10000, 10000, 1, 0, 1 );
    PLUTO_add_hint  ( plint, "exclude values below this cutoff" );

    /* sixth input : maximum cutoff */
    PLUTO_add_option( plint, "Cutoff", "max_st", FALSE );
    PLUTO_add_number( plint, "Max", -10000, 10000, 1, 0, 1 );
    PLUTO_add_hint  ( plint, "exclude values above this cutoff" );

    /* seventh input : tails */
    PLUTO_add_option( plint, "Tails", "tails_st", FALSE );
    PLUTO_add_hint  ( plint, "apply min and max as tail cutoffs" );

    /* eighth input : number of bins for histogram */
    PLUTO_add_option( plint, "Histogram", "bins_st", FALSE );
    PLUTO_add_number( plint, "Bins", 1, 1000, 0, 20, 1 );
    PLUTO_add_hint  ( plint, "number of bins in histogram" );

    return plint;
}

char * 
MASKCALC_main ( PLUGIN_interface * plint )
{
    r_afni_s     A;
    mask_opt_s   M;
    char       * ret_string;

    memset( &A, 0, sizeof( A ) );
    memset( &M, 0, sizeof( M ) );

    if ( ( ret_string = process_args( &A, &M, plint ) ) != NULL )
	return( ret_string );

    return( process( &A, &M ) );
}



/*--------------------------------------------------------------------
   Read the arguments, load the afni and mask structures.
  --------------------------------------------------------------------
*/
static char *
process_args(
	r_afni_s         * A,
	mask_opt_s       * M,
	PLUGIN_interface * plint
	)
{
    MCW_idcode  * idc;
    char        * tag, * str;
    int           op_val;

    A->must_be_short   = 1;
    A->want_floats     = 0;
    A->subs_must_equal = 0;
    A->max_subs        = 0;

    if ( plint == NULL )
	return  "----------------------\n"
		"arguments : NULL input\n"
		"----------------------\n";

    while ( ( tag = PLUTO_get_optiontag( plint ) ) != NULL )
    {
	if ( ! strcmp( tag, "op_st" ) )
	{
	    str = PLUTO_get_string( plint );
	    op_val = 1 + PLUTO_string_index( str, gr_num_ops, gr_op_strings );
	    if ( ( op_val <= (int)no_op ) || ( op_val >= (int)last_op ) )
	    {
		sprintf( grMessage, "-------------------\n"
				    "Illegal operation : '%s'\n"
				    "Value is          : %d\n"
				    "-------------------\n",
				    str, M->operation );
		return grMessage;
	    }
	    M->operation = (op_enum)op_val;
	    continue;
	}
	else if ( ! strcmp( tag, "mask_st" ) )
	{
	    idc = PLUTO_get_idcode( plint );
	    A->dset[0] = PLUTO_find_dset( idc );
	    if ( A->dset[0] == NULL )
		return  "----------------------\n"
			"arg : bad mask dataset\n"
			"----------------------";
	    DSET_load( A->dset[0] );
	    A->num_dsets++;
	    continue;
	}
	else if ( ! strcmp( tag, "dset_st" ) )
	{
	    idc = PLUTO_get_idcode( plint );
	    A->dset[1] = PLUTO_find_dset( idc );
	    if ( A->dset[1] == NULL )
		return  "-----------------------\n"
			"arg : bad inupt dataset\n"
			"-----------------------";
	    DSET_load( A->dset[1] );
	    A->num_dsets++;
	    continue;
	}
	else if ( ! strcmp( tag, "ofile_st" ) )
	{
	    M->outfile  = PLUTO_get_string( plint );
	    str 	= PLUTO_get_string( plint );
	    if ( ( *str != 'y' ) && file_exists( M->outfile, "" ) )
	    {
		sprintf( grMessage,
			 "-------------------------------\n"
			 "output file '%s' already exists\n"
			 "consider the 'overwrite' option\n"
			 "-------------------------------", M->outfile );
		return( grMessage );
	    }
	    continue;
	}
	else if ( ! strcmp( tag, "min_st" ) )
	{
	    M->min = PLUTO_get_number( plint );
	    M->use_min = 1;
	    continue;
	}

	if ( ! strcmp( tag, "max_st" ) )
	{
	    M->max = PLUTO_get_number( plint );
	    M->use_max = 1;
	    continue;
	}

	if ( ! strcmp( tag, "tails_st" ) )
	{
	    M->use_tails = 1;		/* need to check with min/max */
	    continue;
	}

	if ( ! strcmp( tag, "bins_st" ) )
	{
	    M->num_bins = (int)PLUTO_get_number( plint );
	    if ( ( M->num_bins <= 0 ) || ( M->num_bins > R_MAX_BINS ) )
	    {
		sprintf( grMessage, "-----------------------------\n"
				    "Illegal number of bins : %d\n"
				    "(must be in range [1,%d])\n"
				    "-----------------------------",
				    M->num_bins, R_MAX_BINS );
		return grMessage;
	    }

	    continue;
	}

	/* we should not get to this point */

	sprintf( grMessage, "-----------------------\n"
			    "Unknown optiontag : %s\n"
			    "-----------------------", tag );
	return grMessage;
    }

    if ( M->use_tails && ( ! M->use_min || ! M->use_max ) )
    {
	sprintf( grMessage, "------------------------------------------\n"
			    "'tails' option requires min and max values\n"
			    "------------------------------------------" );
	return grMessage;
    }

    if ( M->num_bins && ( M->operation != hist_op ) )
    {
	return  "----------------------------------------------------\n"
		"choosing # bins applies only to the 'hist' operation\n"
		"----------------------------------------------------";
    }
    else if ( ! M->num_bins )
	M->num_bins = 20;

    if ( ( str = fill_afni_struct( A ) ) != NULL )
	return str;

    if ( M->outfile && *M->outfile )
    {
	if ( ( M->outfp = open_file( M->outfile, "w" ) ) == NULL )
	{
	    sprintf( grMessage, "--------------------------------\n"
				"Failed to open '%s' for writing.\n"
				"--------------------------------",
				M->outfile );
	    return grMessage;
	}
    }
    else 
	M->outfp = stdout;

    return NULL;
}


/***********************************************************************
**
**	Perform computation, storing the result back in image1 (space).
**
**	return  1 - successful completion
**		0 - failure
**
************************************************************************
*/
static char *
process( r_afni_s * A, mask_opt_s * M )
{
    char * ret_string;

    switch ( M->operation )
    {
	case hist_op:

		ret_string = calc_hist( A, M );

	    	break;

	case stats_op:

		ret_string = calc_stats( A, M );

	    	break;

	default:

		ret_string = "--------------------\n"
			     "Error: maskcalc_p_00\n"
			     "Invalid operation.\n"
			     "--------------------";
    }   /* end switch */

    PURGE_DSET( A->dset[0] );
    PURGE_DSET( A->dset[1] );

    if ( M->outfp != stdout )
	fclose( M->outfp );

    return ret_string;
}


/***********************************************************************
**
**	Mann-Whitney U-test for comparison of two arbitrary datasets.
**
************************************************************************
*/
static long
get_mask_size( 
	r_afni_s * A,
	int        index,	/* index into simage array */
	int        subbrick
	)
{
    long    count, size;
    short * ptr;

    for ( count = 0, size = 0, ptr = A->simage[ index ][ subbrick ];
	  count < A->nvox;
	  count++, ptr++ )
	if ( *ptr )
	    size++;

    return size;
}



/***********************************************************************
**
**	Comparison function for two shorts, for use in qsort.
**
************************************************************************
*/
int short_test(
	const void * p1,
	const void * p2
	)
{
    short * s1 = ( short * )p1;
    short * s2 = ( short * )p2;

    if ( *s1 < *s2 )
	return -1;

    return ( *s1 > *s2 );	/* if >, return 1, else ==, so 0 */
}




/***********************************************************************
**
**	Output histogram from masked image.
**
************************************************************************
*/
static char *
calc_hist( r_afni_s * A, mask_opt_s * M )
{
    float * data;
    float * ptr;
    float   bin_size, cum, junk;

    long    size, new_size = 0;
    long    count;
    int   * bins;

    int     places;		/* decimal places in output */


    if (( data = (float *)malloc(A->subs[1] * A->nvox * sizeof(float))) == NULL)
    {
	sprintf( grMessage, "Error: maskcalc_ch_00\n"
		 "Failed to allocate memory for %d floats.\n",
		 A->nvox * A->subs[1] );
	return grMessage;
    }

    if ( ( size = mask_all_shorts_to_float( A, 1, 0, data ) ) == 0 )
    {
	sprintf( grMessage, "Error: 5090\n"
		 "Masking shorts results in empty array.\n" );
	return grMessage;
    }

    if ( !M->use_min && !M->use_max )
	assign_min_max( data, size, &M->min, &M->max );
    else if ( !M->use_max )
    {
	assign_min_max( data, size, &junk, &M->max );

	if ( M->min > M->max )
	{
	    sprintf( grMessage, "Error: maskcalc_ch_10\n"
			     "Min of %f is greater than max of %f\n",
			     M->min, M->max );
	    return grMessage;
	}
    }

    junk = ( fabsf( M->max ) > fabsf( M->min ) ) ? 
		fabsf( M->max ) : fabsf( M->min );

    if ( junk == 0 )
	places = 2;
    else
    {
	places = 4 - ( int )floor( log10( junk ) );

	if ( places > 7 )
	    places = 7;
	else if ( places < 0 )
	    places = 0;
    }

    if ( ( bins = (int *)calloc( M->num_bins, sizeof( int ) ) ) == NULL )
    {
	sprintf( grMessage, "Error: maskcalc_ch_30\n"
			 "Failed to allocate for %d longs.\n", M->num_bins );
	return grMessage;
    }

    bin_size = ( M->max - M->min ) / M->num_bins;
    bin_size += 0.000001 * bin_size;
    if ( bin_size == 0.0 )
	bin_size = 1.0e-34;

    for ( count = 0, ptr = data; count < size; count++, ptr++ )
	if ( ( *ptr <= M->max ) && ( *ptr >= M->min ) )
	{
	    bins[ ( int )( ( *ptr - M->min ) / bin_size ) ]++;
	    new_size++;
	}

    if ( new_size == 0 )
	new_size = 1;


    fprintf( M->outfp, "\n        range       \t  #vox  \t  %%   \t  cum %%\n");
    fprintf( M->outfp,   "------------------- \t--------\t------\t-------\n");

    cum = 0.0;
    for ( count = 0; count < M->num_bins; count++ )
    {
	cum += 100.0 * bins[ count ] / new_size;

	fprintf( M->outfp, "[%8.*f,%8.*f) \t%8d\t%6.3f\t%7.3f\n", 
		places, M->min + count * bin_size, 
		places, M->min + (count+1) * bin_size, 
		bins[ count ],
		100.0 * bins[ count ] / new_size,
		cum );
    }
    fputc( '\n', M->outfp );

    return NULL;
}


/***********************************************************************
**
**	Assign min and max values to global variables.
**
************************************************************************
*/
static void
assign_min_max(
	float * data,
	long    size,
	float * min,
	float * max
	)
{
    float * ptr = data;
    long    count;


    *min = *data;
    *max = *data;

    for ( count = 1; count < size; count++, ptr++ )
    {
	if ( *ptr < *min )
	    *min = *ptr;

	if ( *ptr > *max )
	    *max = *ptr;
    }
}


/***********************************************************************
**
**	Output statistics from a masked image.
**
************************************************************************
*/
static char *
calc_stats( r_afni_s * A, mask_opt_s * M )
{
    float * data;
    float   min, max, savemin, savemax;

    long    size;
    int     sub;


    if ( ( data = ( float * )malloc( A->nvox * sizeof(float) ) )
	       == NULL )
    {
	sprintf( grMessage, "Error: 5130\n"
		 "Failed to allocate memory for %d floats.\n",
		 A->nvox );
	return grMessage;
    }

    print_stats_header( M->outfp );

    for ( sub = 0; sub < A->subs[1]; sub++ )
    {
	if ( ( size = mask_shorts_to_float( A, data, sub, 0, 1 ) ) == 0 )
	{
	    sprintf( grMessage, "Error: 5140\n"
		     "Masking shorts results in empty array.\n" );
	    return grMessage;
	}

	assign_min_max( data, size, &savemin, &savemax );

	if ( ! M->use_min && ! M->use_max )
	{
	    min = savemin;		/* use actual min/max for data */
	    max = savemax;
	    
	    do_stats( A, data, size, min, max, sub, M->outfp, NULL, NULL, NULL);
	}
	else if ( ! M->use_max )	/* so use_min is set */
	{
	    min = M->min;		/* use user input cutoff */
	    max = savemax;
	    
	    if ( min <= max )
		do_stats( A, data, size, min, max, sub, M->outfp,
							NULL, NULL, NULL);
	    else
		print_empty_stats( M->outfp );
	}
	else 	/*  use_min AND use_max are set */
	{
		/* NOTE : we are using the tails here */

	    min = savemin;
	    max = M->min;

	    if ( min <= max )
		do_stats( A, data, size, min, max, sub, M->outfp,
							NULL, NULL, NULL);
	    else
		print_empty_stats( M->outfp );

	    min = M->max;
	    max = savemax;

	    if ( min <= max )
		do_stats( A, data, size, min, max, sub, M->outfp,
							NULL, NULL, NULL);
	    else
		print_empty_stats( M->outfp );
	}
    }

    return NULL;
}


/***********************************************************************
**
** 	Print stats header.
**
************************************************************************
*/
static void
print_stats_header ( FILE * fp )
{
    fprintf( fp, " Sub\t  vol  \t%% BRIK\t min  \t max  "
		 "\t mean \t SEM  \t STD  \t    95%%      "
		 "\t thresh # \t thresh %%\n" );
    fprintf( fp, "-----\t-------\t------\t------\t------"
		 "\t------\t------\t------\t-------------"
		 "\t----------\t---------\n" );
}


/***********************************************************************
**
** 	Print empty stats.
**
************************************************************************
*/
static void
print_empty_stats ( FILE * fp )
{
    fprintf( fp, "   0  \t   0   \t   0  \t   0  \t   0  "
		 "\t   0  \t   0  \t   0  \t   ( 0, 0 )  "
		 "\t    0     \t    0    \n" );
}


/***********************************************************************
**
**	Output statistics for each masked subbrick.  We are given
** 	a min and max value to further restrict our brick.
**
************************************************************************
*/
static void
do_stats(
	r_afni_s   * A,
	float      * data,	/* IN  */
	long         size,	/* IN  */
	float        min,	/* IN  */
	float        max,	/* IN  */
	int          sub,	/* IN  */
	FILE       * fp,	/* IN  */
	long       * ret_size,	/* OUT */
	float      * average,	/* OUT */
	float      * ret_var	/* OUT */
	)
{
    double  sum_diff2 = 0.0;
    double  dtmp;

    float   mean, SEM, STD;
    float * ptr = data;
    float   tmp; 
    float   local_min = max, local_max = min;

    long    count;
    long    new_size;


    /* start by getting the mean, min and max (across mask and in range) */

    dtmp = 0.0;
    new_size = 0;
    for ( count = 0, ptr = data; count < size; count++, ptr++ )
	if ( ( *ptr >= min ) && ( *ptr <= max ) )
	{
	    new_size++;

	    dtmp += *ptr;

            if ( *ptr < local_min )
                local_min = *ptr;

            if ( *ptr > local_max )
                local_max = *ptr;
	}

    if ( new_size > 0 )
	mean = dtmp / new_size;
    else
	mean = 0;

    /* now get variance */
    sum_diff2 = 0.0;
    for ( count = 0, ptr = data; count < size; count++, ptr++ )
	if ( ( *ptr >= min ) && ( *ptr <= max ) )
	{
	    tmp        = *ptr - mean;
	    sum_diff2 += tmp * tmp;
	}

    if ( new_size < 2 )
    {
	STD = 0;
	SEM = 0;
    }
    else
    {
	STD = sqrt( sum_diff2 / ( new_size - 1 ) );
	SEM = STD / sqrt( new_size );
    }

    fprintf( fp, 
    "%5d\t%7ld\t %5.*f\t%6.*f\t%6.*f\t%6.*f\t%6.*f\t%6.*f\t(%-5.*f, %5.*f)"
		"\t %8ld \t  %6.*f\n", 
	sub, size, 
	num_places( 100.0*size/A->nvox, 5 ), 100.0*size/A->nvox,  
	num_places( local_min, 6 ), local_min, 
	num_places( local_max, 6 ), local_max,
	num_places( mean, 6 ), mean, 
	num_places( SEM, 6 ), SEM, 
	num_places( STD, 6 ), STD, 
	num_places( mean-1.96*SEM, 5 ), mean-1.96*SEM, 
	num_places( mean+1.96*SEM, 5 ), mean+1.96*SEM,
	new_size,
	num_places( 100.0*new_size/size, 6 ), 100.0*new_size/size );

    if ( ret_size != NULL )
	*ret_size = new_size;

    if ( average != NULL )
	*average = mean;

    if ( ret_var != NULL )
	*ret_var = STD*STD;
}


/***********************************************************************
**
**	Calculate the number of decimal places needed for a float
**	given the magnitude and the total space.
**
************************************************************************
*/
static int 
num_places(
	float num,
	int   size
	)
{
    float junk;
    int   places;

    junk = fabsf( num );

    junk = ( junk == 0 ) ? 1.0 : junk;

    /*
    ** Allow for at least one place to the left of the decimal, 
    ** and the decimal itself.
    */

    places = size - 2 - ( int )floor( log10( junk ) );

    if ( places < 0 )
	places = 0;
    else if ( places >= size )
	places = size - 1;

    return places;
}
	

/***********************************************************************
**
**	Copy masked and scaled shorts into float array.
**
**	Return the number of floats.
**
************************************************************************
*/
static long
mask_shorts_to_float(
	r_afni_s * A,
	float    * data,	/* output data location     */
	int	   sub,		/* current subbrick         */
	int	   mask_index,	/* index of mask 	    */
	int	   data_index	/* index of data 	    */
	)
{
    float * fptr;		/* floats 	 */
    short * mptr;		/* mask   	 */
    short * sptr;		/* shorts 	 */

    float   factor;
    long    count;		/* voxel counter */


    fptr = data;				/* floats */

    if ( A->subs[ mask_index ] > 1 )
	mptr = A->simage[ mask_index ][ sub ];	/* mask   */
    else
	mptr = A->simage[ mask_index ][ 0 ];	/* mask   */

    sptr = A->simage[ data_index ][ sub ];	/* shorts */

    /*
    ** Note that mptr and sptr get incremented continuously, where
    ** fptr gets incremented only when we have a good mask value;
    */

    if ( ( factor = A->factor[ data_index ][ sub ] ) == 1 )
    {
	for( count = 0; count < A->nvox; count++, mptr++, sptr++ )
	    if ( *mptr )
		*fptr++ = *sptr;
    }
    else
    {
	for( count = 0; count < A->nvox; count++, mptr++, sptr++ )
	    if ( *mptr )
		*fptr++ = *sptr * factor;
    }


    return( ( long )( fptr - data ) );		/* return # of floats */
}


/***********************************************************************
**
**	Copy masked shorts into separate array.
**
**	Return the number copied.
**
************************************************************************
*/
static long
mask_shorts_to_short(
	r_afni_s * A,
	short    * data,	/* output data location     */
	int	   sub,		/* current subbrick         */
	int	   mask_index,	/* index of mask */
	int	   data_index	/* index of data */
	)
{
    short * dptr;		/* destination 	 */
    short * mptr;		/* mask   	 */
    short * sptr;		/* shorts 	 */

    long    count;		/* voxel counter */


    dptr = data;			   /* destination pointer */

    if ( A->subs[ mask_index ] > 1 )
	mptr = A->simage[ mask_index ][ sub ];		/* mask   */
    else
	mptr = A->simage[ mask_index ][ 0 ];		/* mask   */

    sptr = A->simage[ data_index ][ sub ];		/* shorts */

    /*
    ** Note that mptr and sptr get incremented continuously, where
    ** dptr gets incremented only when we have a good mask value;
    */

    for( count = 0; count < A->nvox; count++, mptr++, sptr++ )
	if ( *mptr )
	    *dptr++ = *sptr;

    return( ( long )( dptr - data ) );
}


/***********************************************************************
**
**      Copy masked and scaled shorts into float array.
**
**      Return the number of floats.
**
************************************************************************
*/
static long
mask_all_shorts_to_float(
	r_afni_s   * A,
	int	     data_index,
	int	     mask_index,
        float      * data
        )
{
    float * fptr;               /* floats        */
    short * mptr;               /* mask          */
    short * sptr;               /* shorts        */

    float   factor;
    long    count;              /* voxel counter */
    int     sub;                /* sub-brick     */


    fptr = data;                                /* floats */

    for ( sub = 0; sub < A->subs[data_index]; sub ++ )
    {
	if ( A->subs[mask_index] == A->subs[data_index] )
	    mptr = A->simage[ mask_index ][ sub ];       /* mask   */
	else
	    mptr = A->simage[ mask_index ][ 0 ];

        sptr = A->simage[ data_index ][ sub ];           /* shorts */

        /*
        ** Note that mptr and sptr get incremented continuously, where
        ** fptr gets incremented only when we have a good mask value;
        */

        if ( ( factor = A->factor[ data_index ][ sub ] ) == 1 )
        {
            for( count = 0; count < A->nvox; count++, mptr++, sptr++ )
                if ( *mptr )
                    *fptr++ = *sptr;
        }
        else
        {
            for( count = 0; count < A->nvox; count++, mptr++, sptr++ )
                if ( *mptr )
                    *fptr++ = *sptr * factor;
        }
    }

    return( ( long )( fptr - data ) );          /* return # of floats */
}


/*----------------------------------------------------------------------
**
**	Open a file in the given mode (just to shorten code).
**
**----------------------------------------------------------------------
*/
static FILE *
open_file(
	char * file,
	char * mode
	)
{
    return fopen( file, mode );
}


/***********************************************************************
**
**      Check for existence of output file.
**
**      If suffix is NULL, only consider filename.
**      If filename already ends in the suffix, only consider filname.
**      If the filename ends in a period, only add the suffix.
**      Otherwise add a period and the suffix before checking its existence.
**
**      return  1 - exists
**              0 - does not exist
**
************************************************************************
*/
static int
file_exists(
        char * filename,
        char * suffix
        )
{
    struct stat buf;
    char        file[ R_FILE_L + 6 ] = "";
    char      * filep = file;

    if ( suffix == NULL )   /* we don't need to worry about memory */
        filep = filename;
    else if ( ! strcmp( suffix, filename+strlen(filename)-strlen(suffix) ) )
        strcpy( file, filename );
    else if ( filename[ strlen( filename ) - 1 ] == '.' )
        sprintf( file, "%s%s", filename, suffix );
    else
        sprintf( file, "%s.%s", filename, suffix );

        /* stat returns 0 on existence */
    return ( stat( filep, &buf ) == 0 );
}


/***********************************************************************
**
**  Use the dset variable to fill the rest of the structure.
**
************************************************************************
*/
static char *
fill_afni_struct( r_afni_s * A )
{
    u_short mus;
    int     sub, brick;

    for ( brick = 0; brick < A->num_dsets; brick++ )
    {
	A->subs[ brick ] = DSET_NVALS( A->dset[ brick ] );

	if ( A->max_subs && ( A->subs[brick] > A->max_subs ) )
	{
	    sprintf( grMessage, "------------------------------------\n"
				"Error: maskcalc_fas_00\n"
				"Brick #%d contains %d sub-bricks.\n"
				"The limit is %d.\n"
				"------------------------------------",
				brick, A->subs[brick], A->max_subs );
	    return grMessage;
	}

	if ( A->subs_must_equal && ( A->subs[brick] != A->subs[0] ) )
	{
	    sprintf( grMessage, "------------------------------------\n"
				"Error: maskcalc_fas_02\n"
				"Brick #%d contains %d sub-bricks.\n"
				"Brick #%d contains %d sub-bricks.\n"
				"We are requiring them to be equal.\n"
				"------------------------------------",
				0, A->subs[0],
				brick, A->subs[brick] );
	    return grMessage;
	}

	if ( ( A->simage[brick] = 
		(short **)malloc( A->subs[brick]*sizeof(short *)) ) == NULL )
	{
	    return "-------------------------\n"
		   "Error: maskcalc_fas_05\n"
		   "memory allocation failure\n"
		   "-------------------------";
	}
	if ( ( A->factor[brick] = 
		(float *)malloc( A->subs[brick]*sizeof(float)) ) == NULL)
	{
	    return "-------------------------\n"
		   "Error: maskcalc_fas_10\n"
		   "memory allocation failure\n"
		   "-------------------------";
	}

	for ( sub = 0; sub < A->subs[brick]; sub++ )
	{
	    A->simage[brick][sub] = (short *)DSET_ARRAY(A->dset[brick],sub);
	    A->factor[brick][sub] = DSET_BRICK_FACTOR(A->dset[brick],sub);
	    if ( A->factor[brick][sub] == 0.0 )
		A->factor[brick][sub] = 1.0;
	}

	if ( brick == 0 )
	{
	    A->nx   = A->dset[brick]->daxes->nxx;
	    A->ny   = A->dset[brick]->daxes->nyy;
	    A->nz   = A->dset[brick]->daxes->nzz;
	    A->nvox = A->nx * A->ny * A->nz;
	}
	else if ( ( A->nx != A->dset[brick]->daxes->nxx ) ||
		  ( A->ny != A->dset[brick]->daxes->nyy ) ||
		  ( A->nz != A->dset[brick]->daxes->nzz ) )
	{
	    sprintf( grMessage,
		     "--------------------------------\n"
		     "Error : maskcalc_fas_20\n"
		     "Unaligned dimensions.\n"
		     "(%d,%d,%d) != (%d,%d,%d)\n"
		     "--------------------------------",
		     A->dset[brick]->daxes->nxx, A->dset[brick]->daxes->nyy,
		     A->dset[brick]->daxes->nzz, A->nx, A->ny, A->nz );
	    return grMessage;
	}
    }

    if ( A->want_floats && ! assign_afni_floats( A ) )
	return  "-----------------------------\n"
		"Error: maskcalc_fas_30\n"
		"Failed to create afni fimage.\n"
		"-----------------------------";

    mus = 0;
    A->max_u_short = 0;
    for ( brick = 1; brick < A->num_dsets; brick++ )
	for ( sub = 0; sub < A->subs[brick]; sub++ )
	{
	    mus = r_get_max_u_short( (u_short *)A->simage[brick][sub], A->nvox);
    	    if ( mus > A->max_u_short )
		A->max_u_short = mus;
	}

    return NULL;
}


/***********************************************************************
**
**  Create a float brick corresponding to the first subbrick of 
**  every non-mask brick.
**
************************************************************************
*/
static int
assign_afni_floats( r_afni_s * A )
{
    short * sptr;
    float * fptr;
    float   factor = A->factor[1][0];
    int     count;
 
    /* at this point, only brick 1 is a non-mask brick */
    if ( ( A->fimage[1] = (float *)malloc( A->nvox * sizeof(float))) == NULL )
	return 0;

    for ( count = 0, fptr = A->fimage[1], sptr = A->simage[1][0];
	  count < A->nvox;
	  count++, fptr++, sptr++ )
	*fptr = factor * *sptr;

    return 1;
}


/***********************************************************************
**
**  Calculate the maximum unsigned short in the array.
**
************************************************************************
*/
static u_short
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


