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
 * history:  the history is maintained in 3dmaxima
 *----------------------------------------------------------------------
*/

#include "afni.h"
#include "maxima.h"

#ifndef ALLOW_PLUGINS
#  error "Plugins not properly set up -- see machdep.h"
#endif

/***********************************************************************
  plugin to find local extrema
************************************************************************/

char * MAXIMA_main( PLUGIN_interface * );
char * process_args( r_afni_s * A, maxima_s * M, PLUGIN_interface * plint );

static char      * grStyle[] = { "Sort-n-Remove", "Weighted-Average" };
static char      * grSvals[] = { "1 (default)", "1 to N", "N to 1" };
static char      * grNY[]    = { "No", "Yes" };

static char        helpstring[] =
    "Maxima - used to locate extrema in a functional dataset.\n"
    "\n"
    "This plugin reads a functional dataset and locates any relative extrema\n"
    "(maximums or minimums, depending on the user option).  A _relative_\n"
    "maximum is a point that is greater than all neighbors (not necessarily\n"
    "greater than all other values in the sub-brick).  The output from this\n"
    "process can be text based (sent to the terminal window) and it can be a\n"
    "mask (integral) dataset, where the locations of the extrema are set.\n"
    "\n"
    "When writing a dataset, it is often useful to set a sphere around each\n"
    "extrema, not to just set individual voxels.  This makes viewing those\n"
    "locations much more reasonable.  Also, if the 'Sphere Values' option is\n"
    "set to 'N to 1', the sphere around the most extreme voxel will get the\n"
    "value N, giving it the 'top' color in afni (and so on, down to 1).\n"
    "\n"
    "Notes : The only required option is the input dataset.\n"
    "        Input datasets must be of type short.\n"
    "        All distances are in voxel units.\n"
    "\n"
    "                        ***  Options  ***\n"
    "\n"
    "-----  Input:  -----\n"
    "\n"
    "   Dataset   - dataset to locate extrema within\n"
    "\n"
    "   Sub-brick - sub-brick index of volume to locate extrema within\n"
    "\n"
    "-----  Output Dset:  -----\n"
    "\n"
    "   Prefix    - prefix for an output functional dataset\n"
    "\n"
    "               This dataset may be viewed as a mask.  Its set points\n"
    "               will correspond to any selected extrema.  The \"Output\n"
    "               Size\" option will change those points to spheres.\n"
    "\n"
    "   Values    - values to fill output spheres with\n"
    "\n"
    "               1 (default) - fill all spheres with the value 1\n"
    "               1 to N      - fill sphere of most extreme point with 1,\n"
    "                             the next with 2, and so on\n"
    "               N to 1      - fill sphere of most extreme point with N,\n"
    "                             the next with N-1, etc., perhaps matching\n"
    "                             Overlay coloring better\n"
    "\n"
    "-----  Threshold:  -----\n"
    "\n"
    "   Cutoff    - provides a cutoff value for extrema\n"
    "\n"
    "               Extrema not meeting this cutoff will not be considered.\n"
    "               Note that if the \"Neg Extrema\" option is applied, the\n"
    "               user will generally want a negative threshold.\n"
    "\n"
    "-----  Separation:  -----\n"
    "\n"
    "   Distance  - minimum acceptable distance between extrema\n"
    "\n"
    "               Less significant extrema which are too close to more\n"
    "               significant extrema will be discounted in some way.\n"
    "               This option works with the \"Neighbor Style\" option.\n"
    "\n"
    "               Note that the distance is in voxels, not mm.\n"
    "\n"
    "-----  Output Size:  -----\n"
    "\n"
    "   Radius    - output mask radius around set points\n"
    "\n"
    "               If the user wants the output BRIK to consist of spheres\n"
    "               centered around extrema points, this option can be used\n"
    "               to set the radius for those spheres.\n"
    "\n"
    "-----  Neighbor:  -----\n"
    "\n"
    "   Style     - technique for handling clustered extrema\n"
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
    "-----  Params:  -----\n"
    "\n"
    "   Neg Extr  - search for negative extrema (minima)\n"
    "\n"
    "               This will search for the minima of the dataset.\n"
    "               Note that a negative threshold may be desired.\n"
    "\n"
    "   True Max  - do not consider extrema with an equal valued neighbor\n"
    "\n"
    "               As a default points may be considered extrema, even if\n"
    "               they have a neighbor of the same value.  Setting this\n"
    "               option requires extrema to be strictly greater than\n"
    "               any of their neighbors.\n"
    "               Note that with this set, extrema locations that have\n"
    "               more than one voxel at the maximal value are ignored.\n"
    "\n"
    "-----  Output Text:  -----\n"
    "\n"
    "   No Text  - do not display the extrma points as text\n"
    "\n"
    "   Debug    - how much extra information to output to the terminal\n"
    "\n"
    "-----  Output Coords:  -----\n"
    "\n"
    "   Dicom    - Yes/No: whether to display output in Dicom orientation\n"
    "\n"
    "              In Dicom orientation, the output is RAI, meaning right,\n"
    "              anterior and inferior are negative coordinates, and they\n"
    "              are printed in that order (RL, then AP, then IS).\n"
    "\n"
    "              If this option is set to NO, the dataset orientation is\n"
    "              used, whichever of the 48 it happens to be.\n"
    "\n"
    "              Note that in either case, the output orientation is\n"
    "              printed above the results, in the terminal window.\n"
    "\n"
    "Author: (the hopefully-no-longer-lamented) R Reynolds\n"
    "\n"
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

   PLUTO_add_option( plint, "Output Dset" , "prefix" , FALSE );
   PLUTO_add_hint( plint, "options for the creation of an output dataset");
   PLUTO_add_string( plint, "Prefix", 0 , NULL, 19 );
   PLUTO_add_hint( plint, "option: choose dataset prefix for output" );
   PLUTO_add_string( plint, "Sphere Values", 3 , grSvals, 0 );
   PLUTO_add_hint( plint, "option: choose value style for output spheres" );

   /*-- third line of input: cutoff option --*/

   PLUTO_add_option( plint, "Threshold" , "cutoff" , FALSE ) ;
   PLUTO_add_hint( plint, "option: choose a threshold for value at extrema" );
   PLUTO_add_number( plint, "Cutoff", 0, 0, 0, 1000, 1 );

   /*-- fourth line of input: min_dist option --*/

   PLUTO_add_option( plint, "Separation" , "min_dist" , FALSE ) ;
   PLUTO_add_hint( plint, "option: choose a minimum distance between extrema" );
   PLUTO_add_number( plint, "Distance(vox)", 0, 1000, 1, 40, 1 );

   /*-- fifth line of input: out_rad option --*/

   PLUTO_add_option( plint, "Output Size" , "out_rad" , FALSE ) ;
   PLUTO_add_hint( plint, "option: choose a spherical radius around extrema "
			  "points in mask" );
   PLUTO_add_number( plint, "Radius(vox)", 0, 1000, 1, 50, 1 );

   /*-- sixth line of input: style option --*/

   PLUTO_add_option( plint, "Neighbor" , "style" , FALSE ) ;
   PLUTO_add_hint( plint, "option: technique for neighbor removal" );
   PLUTO_add_string( plint, "Style", 2, grStyle, 0 );

   /*-- seventh line of input: negatives and true max options --*/

   PLUTO_add_option( plint, "Params" , "params" , FALSE ) ;
   PLUTO_add_hint( plint, "options: negative extrema and true max" );
   PLUTO_add_string( plint, "Neg Extrema", 2, grNY, 0 );
   PLUTO_add_hint( plint, "search for negative extrema, not positive" );
   PLUTO_add_string( plint, "True Max", 2, grNY, 0 );
   PLUTO_add_hint( plint, "exclude extrema with equal neighbors" );

   /*-- eighth line of input: true_max option --*/

   PLUTO_add_option( plint, "Output Text" , "output" , FALSE ) ;
   PLUTO_add_hint( plint, "options: no output text, debug level" );
   PLUTO_add_string( plint, "No Text Out", 2, grNY, 0 );
   PLUTO_add_hint( plint, "do not output extrema as text (to terminal)" );
   PLUTO_add_number( plint, "Debug Level", 0, 4, 0, 0, 0 );
   PLUTO_add_hint( plint, "search for negative extrema, not positive" );

   /*-- ninth line of input: dicom_coords option --*/

   PLUTO_add_option( plint, "Output Coords" , "dicom_coords" , FALSE ) ;
   PLUTO_add_hint( plint, "option: output coordinates in Dicom format" );
   PLUTO_add_string( plint, "Dicom Coords", 2, grNY, 1 );

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
    r_afni_s           A;
    maxima_s           M;
    char             * ret_string = NULL;
    THD_3dim_dataset * dset;


    if ( ( ret_string = process_args( &A, &M, plint ) ) != NULL )
	return ret_string;

    if ( ! process_data( &M ) )
	return  "************************************\n"
	        "MAXIMA_main: data processing failure\n"
		"************************************";

    if ( ! M.quiet ) display_coords( &A, &M );

    if ( *M.outfile )
    {
        if ( ( dset = EDIT_empty_copy( M.dset ) ) == NULL )
        {
            rERROR( "Error: wr_00\n" "Failed to copy dataset." );
            return(0);
        }

        { /* add dataset history */
          char * his = PLUTO_commandstring(plint) ;
          tross_Copy_History( M.dset , dset ) ;
          tross_Append_History( dset , his ) ; free(his) ;
        }

        if ( ! set_results( &A, &M, dset ) )
            return  "***********************************\n"
                    "MAXIMA_main: result writing failure\n"
                    "***********************************";

        if ( PLUTO_add_dset( plint, dset, DSET_ACTION_MAKE_CURRENT ) )
        {
            rERROR( "Error: wr_10\n" "Failed to make current dataset." );
            return(0);
        }
        else
            DSET_unload( M.dset );
    }


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
char * process_args( r_afni_s * A, maxima_s * M, PLUGIN_interface * plint )
{
    THD_3dim_dataset * dset;
    MCW_idcode       * idc ;
    char             * optag, * outfile = NULL, * str;
    float              cutoff = 0.0, min_dist = 0.0, out_rad = 0.0;
    int                negatives = 0, quiet = 0, true_max = 0, opcnt = 0;
    int                val, debug = 0, style = MAX_SORT_N_REMOVE_STYLE, sb;
    int                sval_style = 0, dicom_coords = 1;

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
            sval_style = PLUTO_string_index(PLUTO_get_string(plint),3,grSvals);
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
	else if ( ! strcmp( optag, "params" ) )
	{
	    str = PLUTO_get_string( plint );		/* Neg Extrema */
	    val = PLUTO_string_index(str, 2, grNY);
	    if ( val > 0 ) negatives = 1;
	    str = PLUTO_get_string( plint );		/* True Max    */
	    val = PLUTO_string_index(str, 2, grNY);
	    if ( val > 0 ) true_max = 1;
	}
	else if ( ! strcmp( optag, "style" ) )
	{
	    if ( ( str = PLUTO_get_string( plint ) ) == NULL )
		RETURN( "-------------------------------\n"
			"options : missing style string?\n"
			"-------------------------------");
	    if ((( style = PLUTO_string_index(str, 2, grStyle)) 
				< 0 ) || ( style >= MAX_MAX_STYLE ) )
	    {
		sprintf( grMessage,
		    "---------------------------\n"
		    "options : bad style is %d\n"
		    "---------------------------", style );
		RETURN(grMessage);
	    }
	    style++;
	}
	else if ( ! strcmp( optag, "output" ) )
	{
	    str = PLUTO_get_string( plint );		/* No Text Out */
	    val = PLUTO_string_index(str, 2, grNY);
	    if ( val > 0 ) quiet = 1;
	    debug = PLUTO_get_number( plint );          /* Debug Level */
	    
	}
	else if ( ! strcmp( optag, "dicom_coords" ) )
	{
	    str = PLUTO_get_string( plint );		/* Neg Extrema */
	    val = PLUTO_string_index(str, 2, grNY);
	    if ( val == 0 ) dicom_coords = 0;
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

    if ( ! r_set_afni_s_from_dset( A, dset, debug ) )
	RETURN( "-------------------------------\n"
                "arguments : afni_s init failure\n"
                "-------------------------------");

    if ( ! init_maxima_s( M, A, outfile ) )
	RETURN("----------------------------------\n"
               "MAXIMA_main: maxima_s init failure\n"
               "----------------------------------");

    /* now fill any remaining parameters */
    M->sval_style   = sval_style;
    M->cutoff       = cutoff / A->factor[0];
    M->min_dist     = min_dist;
    M->out_rad      = out_rad;

    M->negatives    = negatives;
    M->ngbr_style   = style;
    M->quiet        = quiet;
    M->true_max     = true_max;
    M->dicom_coords = dicom_coords;
    M->debug        = debug;

    gr_fac          = A->factor[0];

    if ( M->debug > 0 )
    {
	if ( M->debug > 1 ) show_maxima_s( "plugin values applied ", M );
	fprintf(stderr,"  using sub-brick %d, factor %f (1/%f)\n",
		A->sub_brick, A->factor[0], 1/A->factor[0]);
    }

    RETURN(NULL);
}

