/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/
   
#include "afni.h"

#ifndef ALLOW_PLUGINS
#  error "Plugins not properly set up -- see machdep.h"
#endif

/***********************************************************************
 ************************************************************************/

char * Fourier_Main( PLUGIN_interface * ) ;

static char helpstring[] =
   " Purpose: Do some common FFT manipulations of 3d+time data\n"
   " Inputs:\n"
   " Dataset = A 3d+time dataset in memory.\n"
   " Outputs:\n"
   " Prefix = Filename prefix to be usedfor the output dataset.\n"
   " Parameters:\n"
   " Preprocess: Data manipulations before/after filtering\n"
   "  Ignore: ignore the first n datapoints.  After filtering\n"
   "   these points are set to the value of the n+1th point\n"
   "   such that the timeseries is the same length\n"
   "  A linear trend is removed before any filtering is performed.\n"
   "   Setting retrend to 'Yes' restores this trend after filtering\n" 
   "   This is always yes for the 1D transform version\n"
   " Lowpass: Perform a low pass filtering of the data.\n"
   "  Fc is the frequency cutoff (in Hz).  Note that this is\n"
   "   typically a very small number.\n"
   " Highpass: Perform a high pass filtering of the data.\n"
   "  Fc is the frequency cutoff (in Hz).  Note that this is\n"
   "   typically a very small number.\n\n"
   " With appropriate combination of lowpass and highpass, a\n"
   "  bandpass (f_high < f_low) or notch (f_high > f_low)\n"
   "  filter can be made.\n"
/*   " Autocorrelate: Perform an autocorrelation (DUH) of the time-\n"
   "  series.  Essentially the correlation between the data and\n"
   "  and all possible phase shifts of it.\n\n"
*/   " Note that if multiple operations are chosen, they are performed\n"
   "  in the order listed.\n\n"
   " IMPORTANT: This plugin also sets the parameters for the 1D \n"
   "  transform named Fourier.  Thus, it is not necessary to have\n"
   "  an input and output.\n\n"
   " BY- T. Ross and K. Heimerl 8/99\n"
;

static float low_fc, high_fc;    /* Global variables so we can be a plugin or 1D transformation filter */
static char *output_prefix;
static int autocorr, retrend, ignore;
static MCW_idcode *idc;


/***********************************************************************
  include the filter driver and filter routines
 ************************************************************************/
#define IN_FOURIER_PLUGIN
#include "fourier_filter.c"

/***********************************************************************
   1D transformation function
 ************************************************************************/

void fourier_1D(int num, double to, double dt, float *vec) {
	filter(vec, low_fc, high_fc, num, (float)dt, ignore, TRUE, TRUE);
}


/***********************************************************************
   Set up the interface to the user
************************************************************************/


DEFINE_PLUGIN_PROTOTYPE

PLUGIN_interface * PLUGIN_init( int ncall )
{
   PLUGIN_interface * plint ;
   static char *yn[2] = {"No", "Yes"};

   if( ncall > 0 ) return NULL ;  /* only one interface */

   /*-- set titles and call point --*/

   /* Initialize variables so we can be a 1D transformer*/
   low_fc=0; high_fc = 0; ignore=1; autocorr=FALSE; retrend=FALSE; output_prefix=NULL; idc=NULL;
   PLUTO_register_1D_function( "Fourier", fourier_1D );


   plint = PLUTO_new_interface( "Fourier Stuff" , "Filtering, autocorrelation and other stuff done with FFTs" , helpstring ,
                                 PLUGIN_CALL_VIA_MENU , Fourier_Main  ) ;

   PLUTO_add_hint( plint , "Filtering, autocorrelation and other stuff done with FFTs" ) ;

   PLUTO_set_sequence( plint , "z:Ross" ) ;
   /*-- first line of input: Dataset --*/

   PLUTO_add_option( plint , "Input" , "Input" , FALSE ) ;
   PLUTO_add_dataset(plint , "Dataset" ,
                                    ANAT_EPI_MASK , 0 ,
                                    DIMEN_4D_MASK | BRICK_ALLREAL_MASK ) ;

   /*-- second line of input: Prefix for output dataset --*/

   PLUTO_add_option( plint , "Output" , "Output" , FALSE ) ;
   PLUTO_add_string( plint , "Prefix" , 0, NULL , 19 ) ;

   /*-- third line of input: Preprocessing --*/

   PLUTO_add_option( plint , "Preprocess" , "Preprocess" , TRUE ) ;
   PLUTO_add_number( plint , "Ignore" ,  0, 10,  0, 1 , FALSE) ;
   PLUTO_add_string( plint, "Re-trend", 2, yn, 0); 

   /*-- fourth line of input: Lowpass option --*/

   PLUTO_add_option( plint , "Lowpass" , "Lowpass" , FALSE ) ;
   PLUTO_add_number( plint , "Fc" ,  0, 5000,  3, 0 , TRUE) ;

   /*-- fifth line of input: Highass option --*/

   PLUTO_add_option( plint , "Highpass" , "Highpass" , FALSE ) ;
   PLUTO_add_number( plint , "Fc" ,  0, 5000,  3, 0 , TRUE) ;

   /*-- sixt line of input: Autocorrelation option --*/
/*
   PLUTO_add_option( plint , "Autocorrelate" , "Autocorrelate" , FALSE ) ;
 */
   return plint ;
}

/***************************************************************************
  Main routine for this plugin (will be called from AFNI).
****************************************************************************/

char * Fourier_Main( PLUGIN_interface * plint )
{
   THD_3dim_dataset *input ;
   char *tag;


   /* New call, so reinitialize variables */
   low_fc=0; high_fc = 0; autocorr=FALSE; retrend=FALSE; output_prefix=NULL, idc=NULL;

   /*--------------------------------------------------------------------*/
   /*----- Check inputs from AFNI to see if they are reasonable-ish -----*/

   if( plint == NULL )
      return "*************************\n"
             "Fourier_Main:  NULL input\n"
             "*************************"  ;


   tag = PLUTO_get_optiontag(plint) ;
   while( tag != NULL ){

	if (strcmp(tag, "Input") == 0 ) {
	   idc  = PLUTO_get_idcode(plint) ;
	   input = PLUTO_find_dset(idc) ;
	   if( input == NULL )
	      return "********************************\n"
        	     "Fourier_Main:  bad input dataset\n"
	             "********************************"  ;
	}

	else if (strcmp(tag, "Preprocess") == 0) {
	        ignore = PLUTO_get_number(plint);
 	        retrend = strcmp(PLUTO_get_string(plint), "No");
	}

      else if( strcmp(tag,"Output") == 0 ){
         output_prefix = PLUTO_get_string(plint) ;
         if( ! PLUTO_prefix_ok(output_prefix) )
            return "*************************\n"
                   "Fourier_Main:  bad prefix\n"
                   "*************************"  ;
      }

      else if( strcmp(tag,"Lowpass") == 0 ){
         low_fc = PLUTO_get_number(plint);
      }

      else if( strcmp(tag,"Highpass") == 0 ){
         high_fc = PLUTO_get_number(plint);
      }
 
      else if( strcmp(tag,"Autocorrelate") == 0 ){
         autocorr=TRUE;
      }

      tag = PLUTO_get_optiontag(plint) ;
   }

   /*------------------------------------------------------*/
   /*---------- At this point, the inputs are OK ----------*/

   if ((output_prefix == NULL) || (idc == NULL)) /* must be setting up for 1D transform */
      return NULL ;
   else 
      return Fourier_Filter_Driver(plint, input, low_fc, high_fc, ignore, autocorr, retrend, output_prefix);  
}
