/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/

/*----------------------------------------------------------------------
 *
 *  plug_hemisub.c      - AFNI plugin to subtract hemispheres
 *
 *  $Log$
 *  Revision 1.4  2000/12/21 16:10:54  cox
 *  AFNI
 *
 *  Revision 1.2  2000/06/15 22:02:40  cox
 *  AFNI
 *
 *  Revision 1.1  1999/08/06 19:10:48  cox
 *  AFNI
 *
 * Revision 1.1  1998/09/04  19:59:54  rickr
 * Initial revision
 *
 *----------------------------------------------------------------------
 */

#include "afni.h"

typedef struct
{
    int thresh_type;    /* none, pos_only, neg_only */
} hemi_s;

static char * process_data     ( THD_3dim_dataset *, hemi_s * );
static char * process_as_floats( THD_3dim_dataset *, hemi_s * );

#ifndef ALLOW_PLUGINS
#  error "Plugins not properly set up -- see machdep.h"
#endif

/***********************************************************************
  plugin to find local extrema
************************************************************************/

char * HEMISUB_main( PLUGIN_interface * );

#define            NUM_T_OPTS   3

static char      * thresh_opts[ NUM_T_OPTS ] =
			{ "any", "positives only", "negatives only" };
static char        helpstring[] =
    "Hemisubtract - used to subtract one hemisphere from the other\n"
    ;



/***********************************************************************
   Set up the interface to the user
************************************************************************/

PLUGIN_interface * PLUGIN_init( int ncall )
{
    PLUGIN_interface * plint ;

    if( ncall > 0 ) return NULL ;  /* only one interface */

    /* create the new interface */

    plint = PLUTO_new_interface( "Hemi-subtract", "hemisphere subtraction",
		helpstring, PLUGIN_CALL_VIA_MENU , HEMISUB_main );

    PLUTO_add_hint( plint,
	"from each voxel's value, subtract that of the reflected voxel" );

    PLUTO_set_sequence( plint , "z:Reynolds" ) ;

    /*-- first line of input: input dataset --*/

    PLUTO_add_option( plint, "Input" , "Input" , TRUE );
    PLUTO_add_hint( plint, "choose dataset for input" );
    PLUTO_add_dataset(plint, "Dataset" , ANAT_ALL_MASK , FUNC_FIM_MASK,
					 DIMEN_3D_MASK | BRICK_SHORT_MASK );

    /*-- second line of input: prefix for output dataset --*/

    PLUTO_add_option( plint, "Output" , "prefix" , TRUE );
    PLUTO_add_hint( plint, "option: choose dataset prefix for output" );
    PLUTO_add_string( plint, "Prefix", 0, NULL, 19 );

    /*-- third line of input: threshold type option --*/

    PLUTO_add_option( plint, "Thresh Type", "Thresh Type", FALSE );
    PLUTO_add_string( plint, "Type", NUM_T_OPTS, thresh_opts, 0 );

    return plint;
}


/*----------------------------------------------------------------------
**
**  Main routine for this plugin (will be called from AFNI).
**
**----------------------------------------------------------------------
*/
char * HEMISUB_main( PLUGIN_interface * plint )
{
    THD_3dim_dataset * dset, * new_dset;
    MCW_idcode       * idc;
    hemi_s             hs = { 0 };
    char             * new_prefix;
    char             * ret_string = NULL;
    char             * tag;


    if ( plint == NULL )
	return  "------------------------\n"
		"HEMISUB_main: NULL input\n"
		"------------------------\n";

    PLUTO_next_option( plint );
    idc  = PLUTO_get_idcode( plint );
    dset = PLUTO_find_dset( idc );

    if( dset == NULL )
	return "-------------------------------\n"
	       "HEMISUB_main: bad input dataset\n"
	       "-------------------------------";

    DSET_load( dset );

    PLUTO_next_option( plint );
    new_prefix = PLUTO_get_string( plint );
    if ( ! PLUTO_prefix_ok( new_prefix ) )
	return  "------------------------\n"
		"HEMISUB_main: bad prefix\n"
		"------------------------\n";

    if ( ( new_dset = PLUTO_copy_dset( dset, new_prefix ) ) == NULL )
	return  "------------------------------------------\n"
		"HEMISUB_main: failed to copy input dataset\n"
		"------------------------------------------\n";

    tag = PLUTO_get_optiontag( plint );
    if ( tag && ! strcmp( tag, "Thresh Type" ) )
    {
	tag = PLUTO_get_string( plint );
	if ( tag != NULL )
	    hs.thresh_type = PLUTO_string_index( tag, NUM_T_OPTS, thresh_opts );
    }

    if ( ret_string = process_data( new_dset, &hs ) )
	return  ret_string;

    if ( PLUTO_add_dset( plint, new_dset, DSET_ACTION_MAKE_CURRENT ) )
    {
	THD_delete_3dim_dataset( new_dset, False );

	return  "---------------------------------------\n"
		"HEMISUB_main: failed to add new dataset\n"
		"---------------------------------------\n";
    }

    return NULL;
}


/*----------------------------------------------------------------------
**
**  Subtract hemispheres.
**
**  Check if we need to create or change the factor.
**
**----------------------------------------------------------------------
*/
static char *
process_data( THD_3dim_dataset * dset, hemi_s * hs )
{
    int     count, nx, ny, nz, cx, cx2;
    int     type, diff, floats = ( DSET_BRICK_FACTOR( dset, 0 ) != 0.0 );
    short * data, * sp, * sp2;

    nx = dset->daxes->nxx;
    ny = dset->daxes->nyy;
    nz = dset->daxes->nzz;

    type = hs->thresh_type;

    data = (short *)DSET_ARRAY( dset, 0 );
    for ( count = 0; ! floats && count < ny*nz; count++ )
    {
	sp  = data;
	sp2 = data + nx - 1;

	for ( cx = 0; cx < (nx+1)/2; cx++ )
	{
	    if ( type == 1 )            /* positives only */
	    {
		if ( *sp < 0 )
		    *sp = 0;
		if ( *sp2 < 0 )
		    *sp2 = 0;
	    }
	    else if ( type == 2 )       /* negatives only */
	    {
		if ( *sp > 0 )
		    *sp = 0;
		if ( *sp2 > 0 )
		    *sp2 = 0;
	    }

	    diff = *sp - *sp2;
						  /* if out of short range */
	    if ( ( diff > 32767 ) || ( diff < -32768 ) )
		floats = 1;
	    else
	    {
		*sp  = diff;
		*sp2 = -diff;
	    }

	    sp++;
	    sp2--;
	}

	data += nx;
    }

    if ( floats )
	return process_as_floats( dset, hs );

    return NULL;        /* success */
}


/*----------------------------------------------------------------------
**
**  Subtract hemispheres assuming we need floats.
**
**----------------------------------------------------------------------
*/
static char *
process_as_floats( THD_3dim_dataset * dset, hemi_s * hs )
{
    int     count, cx, type = hs->thresh_type;
    int     nx, ny, nz, nvox;
    short * sp, * sdata;
    float * fdata, * fp, * fp2;
    float   factor, maxabs;

    nx   = dset->daxes->nxx;
    ny   = dset->daxes->nyy;
    nz   = dset->daxes->nzz;
    nvox = nx * ny * nz;

    sdata = (short *)DSET_ARRAY( dset, 0 );

    factor = DSET_BRICK_FACTOR( dset, 0 );
    factor = factor == 0.0 ? 1.0 : factor;

    /* first get the data into a float array */

    if ( ( fdata = (float *)malloc( nvox * sizeof( float ) ) ) == NULL )
	return  "------------------------------\n"
		"paf: failed allocation of floats"
		"------------------------------\n";

    fp = fdata;
    sp = sdata;
    for ( count = 0; count < nvox; count++ )
    {
	*fp = *sdata * factor;

	if ( ( type == 1 ) && ( *fp < 0 ) )
	    *fp = 0;
	else if ( ( type == 2 ) && ( *fp > 0 ) )
	    *fp = 0;

	fp++;
	sp++;
    }

    /* now make the subtraction as floats */

    for ( count = 0; count < ny*nz; count++ )
    {
	fp  = fdata + count * nx;
	fp2 = fp + nx - 1;

	for ( cx = 0; cx < (nx+1)/2; cx++ )
	{
	    *fp  = *fp - *fp2;
	    *fp2 = -*fp;

	    fp++;
	    fp2--;
	}
    }

    /* now make a new factor */

    maxabs = MCW_vol_amax( nvox, 1, 1, MRI_float, fdata );

    /* result is all zero, let the user worry */
    if ( maxabs != 0.0 )
    {
	factor = MRI_TYPE_maxval[MRI_short] /maxabs;        /* 32767? / maxabs */
    
	EDIT_coerce_scale_type( nvox, factor, MRI_float, fdata, MRI_short, sdata );
    
	DSET_BRICK_FACTOR( dset, 0 ) = factor == 0.0 ? 0.0 : 1.0 / factor;
    
	THD_load_statistics( dset );
    }
    free(fdata);
    return NULL;        /* success */
}

