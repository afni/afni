/***********************************************************************
  plug_retroicor.c: AFNI plugin to perform Retrospective Image
  Correction for physiological motion effects, using a slightly
  modified version of the RETROICOR algorithm described in:

    Glover, G. H., Li, T., & Ress, D. (2000). Image-based method for
  retrospective correction of physiological motion effects in fMRI:
  RETROICOR. Magnetic Resonance in Medicine, 44, 162-167.

  Fred Tam
  Sunnybrook & Women's College Health Sciences Centre
  April 15, 2002

  Copyright (C) 2002 Sunnybrook & Women's College Health Sciences Centre,
  Toronto, ON, Canada. As part of the AFNI software package, this software
  is distributed under the GNU General Public License, Version 2.
  See the file README.copyright for details.
************************************************************************/

/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/

#include "afni.h"

#ifndef ALLOW_PLUGINS
#  error "Plugins not properly set up -- see machdep.h"
#endif

#include "retroicor.h"

static char * PRIC_main( PLUGIN_interface * ) ;

static char helpstring[] =
  " Purpose: Perform Retrospective Image Correction for physiological\n"
  "   motion effects, using a slightly modified version of the RETROICOR\n"
  "   algorithm described in:\n"
  " \n"
  "     Glover, G. H., Li, T., & Ress, D. (2000). Image-based method for\n"
  "   retrospective correction of physiological motion effects in fMRI:\n"
  "   RETROICOR. Magnetic Resonance in Medicine, 44, 162-167.\n"
  " \n"
  " Inputs:\n"
  " \n"
  "   Datasets: Input  = 3D+time dataset to process\n"
  "             Output = Prefix for new, corrected dataset\n"
  "             Ignore = Number of initial timepoints to ignore in input\n"
  "                      (These points will be passed through uncorrected)\n"
  " \n"
  "   Cardiac: Input     = 1D cardiac waveform data for correction\n"
  "            Output    = Filename for 1D cardiac phase output\n"
  "                        (Leave blank if you don't want this output)\n"
  "            Threshold = Threshold for detection of R-wave peaks in input\n"
  "                        (Make sure it's above the background noise level;\n"
  "                        Try 3/4 or 4/5 times range plus minimum)\n"
  " \n"
  "   Resp: Input  = 1D respiratory waveform data for correction\n"
  "         Output = Filename for 1D resp phase output\n"
  "                  (Leave blank if you don't want this output)\n"
/*-- removed winsize ui
  "         Window = Window size for input point estimate of slope\n"
  "                  (Try 1/3 or 1/2 times respiratory sampling rate in Hz)\n"
removed winsize ui --*/
  " \n"
  "   Params: Order = The order of the correction\n"
  "           (2 is typical; higher-order terms yield little improvement\n"
  "            according to Glover et al.)\n"
  " \n"
  "   ** The input and output datasets and at least one input for correction\n"
  "      (cardiac and/or resp) are required.\n"
  " \n"
  " NOTES\n"
  " -----\n"
  " \n"
  " The durations of the physiological inputs are assumed to equal the\n"
  " duration of the dataset. Any constant sampling rate may be used, but\n"
  " 40 Hz seems to be acceptable. This plugin's cardiac peak detection\n"
  " algorithm is rather simplistic, so you might try using the scanner's\n"
  " cardiac gating output (transform it to a spike wave if necessary).\n"
  " \n"
  " This plugin uses slice timing information embedded in the dataset to\n"
  " estimate the proper cardiac/respiratory phase for each slice. It makes\n"
  " sense to run this plugin before any program that may destroy the slice\n"
  " timings (e.g. 3dvolreg for motion correction).\n"
  " \n"
  " Author -- Fred Tam, April 2002"
;

#define PRIC_C_DEF_THRESHOLD 10
#define PRIC_R_DEF_WINSIZE 20
#define PRIC_M_DEF_ORDER 2
#define PRIC_I_DEF_IGNORE 0

/***********************************************************************
   Set up the interface to the user
************************************************************************/

/* from Greg Balls   7 Aug 2006 [rickr] */
DEFINE_PLUGIN_PROTOTYPE

PLUGIN_interface * PLUGIN_init( int ncall )
{
   PLUGIN_interface * plint ;

   if( ncall > 0 ) return NULL ;  /* only one interface */

   CHECK_IF_ALLOWED("RETROICOR","RETROICOR") ;  /* 30 Sep 2016 */

   /*-- set titles and call point --*/

   plint = PLUTO_new_interface( "RETROICOR" ,
				"Physio Correction of a 3D+time Dataset" ,
				helpstring ,
				PLUGIN_CALL_VIA_MENU , PRIC_main  ) ;

   PLUTO_add_hint( plint , "Physio Correction of a 3D+time Dataset" ) ;

   PLUTO_set_sequence( plint , "A:newdset:retroicor" ) ;

   /*-- first line of input: Datasets --*/

   PLUTO_add_option( plint , "Datasets" , "Datasets" , TRUE ) ;

   PLUTO_add_dataset( plint , "Input" ,
                                    ANAT_ALL_MASK , FUNC_FIM_MASK ,
                                    DIMEN_4D_MASK | BRICK_ALLREAL_MASK ) ;
   PLUTO_add_hint( plint , "Choose 3D+time input" ) ;

   PLUTO_add_string( plint , "Output" , 0, NULL , 19 ) ;
   PLUTO_add_hint( plint , "Prefix for corrected 3D+time output" ) ;

   PLUTO_add_number( plint , "Ignore" , 0, 100, 0, PRIC_I_DEF_IGNORE, TRUE ) ;
   PLUTO_add_hint( plint , "Number of initial input timepoints to ignore" ) ;

   /*-- second line of input: Cardiac --*/

   PLUTO_add_option( plint , "Cardiac", "Cardiac" , FALSE ) ;

   PLUTO_add_timeseries( plint , "Input" );
   PLUTO_add_hint( plint , "Choose 1D cardiac waveform input");

   PLUTO_add_string( plint , "Output" , 0, NULL , 19 ) ;
   PLUTO_add_hint( plint , "Filename for 1D cardiac phase output (optional)" );

   PLUTO_add_number( plint , "Threshold" , -1280, 1270, 1,
		     PRIC_C_DEF_THRESHOLD, TRUE ) ;
   PLUTO_add_hint( plint , "Threshold for input R-wave peak detection" ) ;

   /*-- third line of input: Resp --*/

   PLUTO_add_option( plint , "Resp", "Resp" , FALSE ) ;

   PLUTO_add_timeseries( plint , "Input" );
   PLUTO_add_hint( plint , "Choose 1D resp waveform input");

   PLUTO_add_string( plint , "Output" , 0, NULL , 19 ) ;
   PLUTO_add_hint( plint , "Filename for 1D resp phase output (optional)" ) ;
   /*-- removed winsize ui
   PLUTO_add_number( plint , "Window" , 2, 100, 0, PRIC_R_DEF_WINSIZE, TRUE ) ;
   PLUTO_add_hint( plint , "Window size for input point estimate of slope" ) ;
   removed winsize ui --*/
   /*-- fourth line of input: Params --*/

   PLUTO_add_option( plint , "Params", "Params" , FALSE ) ;

   PLUTO_add_number( plint , "Order" , 1, 5, 0, PRIC_M_DEF_ORDER, FALSE ) ;
   PLUTO_add_hint( plint , "Order of correction" ) ;

   return plint ;
}

/***************************************************************************
  Main routine for this plugin (will be called from AFNI).
****************************************************************************/

static char * PRIC_main( PLUGIN_interface * plint )
{
   char * tag , * new_prefix , * cphase1d = NULL, * rphase1d = NULL;
   MCW_idcode * idc ;
   THD_3dim_dataset * dset , * new_dset;
   double * avg = NULL;
   double * ca , * cb, * ra, * rb;
   MRI_IMAGE * card = NULL, * resp = NULL;
   MRI_IMAGE * cardphase = NULL, * respphase = NULL;
   float threshold=0.0, ignore_input, M_input;
   /*-- removed winsize ui
   , winsize_input;
   removed winsize ui --*/
   int ignore;
   int M = PRIC_M_DEF_ORDER;
   int winsize;
   float tr;
   int ival, nvals;
   FILE * fp;
   float * cpdata, * rpdata;
   char * histstring;

   /*--------------------------------------------------------------------*/
   /*----- Check inputs from AFNI to see if they are reasonable-ish -----*/

   if( plint == NULL )
      return "*********************\n"
             "PRIC_main: NULL input\n"
             "*********************"    ;

   /*-- first line of input: Datasets (required) --*/

   tag = PLUTO_get_optiontag(plint) ;
   if( tag==NULL || strcmp(tag,"Datasets") != 0 )
      return "*******************************\n"
             "PRIC_main: bad Input option tag\n"
             "*******************************"    ;

   idc  = PLUTO_get_idcode(plint) ;
   dset = PLUTO_find_dset(idc) ;
   if( dset == NULL )
      return "****************************\n"
             "PRIC_main: bad input dataset\n"
             "****************************"   ;

   new_prefix = PLUTO_get_string(plint) ;
   if( ! PLUTO_prefix_ok(new_prefix) )
       return "*********************\n"
	      "PRIC_main: bad prefix\n"
              "*********************"   ;

   ignore_input = PLUTO_get_number(plint) ;
   ignore = rint(ignore_input);
   if( ignore_input == BAD_NUMBER || ignore < 0 )
      return "***************************\n"
             "PRIC_main: bad ignore input\n"
             "***************************"   ;

   /*-- second line of input: Cardiac --*/

   tag = PLUTO_get_optiontag(plint) ;

   if( tag != NULL && strcmp(tag, "Cardiac") == 0 ) {

       card = PLUTO_get_timeseries(plint) ;
       if( card == NULL )
	   return "****************************\n"
	          "PRIC_main: bad cardiac input\n"
	          "****************************"   ;

       /* It's okay if this is zero length--it means no output */
       cphase1d = PLUTO_get_string(plint) ;
       if( cphase1d == NULL ||
	   (strlen(cphase1d) > 0 && ! THD_filename_ok(cphase1d)) )
	   return "*****************************\n"
	          "PRIC_main: bad CPhase 1D name\n"
	          "*****************************"   ;

       threshold = PLUTO_get_number(plint) ;
       if( threshold == BAD_NUMBER )
	   return "******************************\n"
	          "PRIC_main: bad threshold input\n"
	          "******************************"   ;

       tag = PLUTO_get_optiontag(plint) ;
   }

   /*-- third line of input: Resp --*/

   /* By this point we already got the next option tag */

   if( tag != NULL && strcmp(tag, "Resp") == 0 ) {

       resp = PLUTO_get_timeseries(plint) ;
       if( resp == NULL )
	   return "**************************\n"
                  "PRIC_main: bad resp input\n"
                  "*************************"   ;

       /* It's okay if this is zero length--it means no output */
       rphase1d = PLUTO_get_string(plint) ;
       if( rphase1d == NULL ||
	   (strlen(rphase1d) > 0 && ! THD_filename_ok(rphase1d)) )
	   return "*****************************\n"
	          "PRIC_main: bad RPhase 1D name\n"
                  "*****************************"   ;

       /*-- removed winsize ui
       winsize_input = PLUTO_get_number(plint) ;
       winsize = rint(winsize_input);
       if( winsize_input == BAD_NUMBER || winsize < 2 )
	   return "********************************\n"
	          "PRIC_main: bad window size input\n"
	          "********************************"   ;
       removed winsize ui --*/

       tag = PLUTO_get_optiontag(plint) ;
   }

   /* Check that at least one of Cardiac and Resp were selected */
   if (card == NULL && resp == NULL)
       return "****************************************************\n"
	      "PRIC_main: at least one of Cardiac and Resp required\n"
	      "****************************************************"   ;

   /*-- fourth line of input: Params --*/

   /* By this point we already got the next option tag */

   if( tag != NULL && strcmp(tag, "Params") == 0 ) {
       M_input = PLUTO_get_number(plint) ;
       M = rint(M_input);
       if( M_input == BAD_NUMBER || M < 1)
	   return "**************************\n"
	          "PRIC_main: bad Order input\n"
	          "**************************"   ;
   }

   /*------------------------------------------------------*/
   /*---------- At this point, the inputs are OK ----------*/

   PLUTO_popup_meter(plint);

   /*-- copy the image data for editing in place --*/

   new_dset = PLUTO_copy_dset( dset , new_prefix );
   if( new_dset == NULL )
       return "********************************\n"
	      "PRIC_main: error copying dataset\n"
              "********************************"   ;
   tross_Copy_History(dset, new_dset); /* Copy and add to new_dset history */
   histstring = PLUTO_commandstring(plint);
   tross_Append_History(new_dset, histstring);
   free(histstring);
   DSET_unload( dset ) ;  /* We won't need the old dataset anymore */

   PLUTO_set_meter(plint, 10);

   /*-- calculate cardiac correction coefficients if requested --*/

   if (card != NULL) {
       /*-- convert cardiac waveform to phase --*/
       cardphase = RIC_ToCardiacPhase(card, threshold) ;
       if (cardphase == NULL) {
	   THD_delete_3dim_dataset( new_dset , False ) ;
	   return "******************************************\n"
	          "PRIC_main: error transforming cardiac data\n"
                  "******************************************"   ;
       }
       PLUTO_set_meter(plint, 20);

       /*-- calculate dataset voxel means --*/
       avg = RIC_CalcVoxelMeans(new_dset, ignore);
       if (avg == NULL) {
	   THD_delete_3dim_dataset( new_dset , False ) ;
	   mri_free(cardphase);
	   return "************************************************\n"
	          "PRIC_main: error calculating dataset voxel means\n"
                  "************************************************"   ;
       }
       PLUTO_set_meter(plint, 33);

       /*-- calculate coefficients for each voxel --*/
       if (RIC_CalcCoeffAB(new_dset, cardphase, avg, &ca, &cb, M, ignore)
	   != 0) {

	   THD_delete_3dim_dataset( new_dset , False ) ;
	   mri_free(cardphase);
	   return "******************************************************\n"
	          "PRIC_main: error calculating cardiac a, b coefficients\n"
                  "******************************************************"   ;
       }
   }
   PLUTO_set_meter(plint, 30);

   /*-- calculate respiratory correction coefficients if requested --*/

   if (resp != NULL) {
       /*-- Set winsize to 1/2 sampling rate of resp in Hz --*/
       tr = new_dset->taxis->ttdel;
       switch (new_dset->taxis->units_type) {
       case UNITS_MSEC_TYPE: tr /= 1000; break;
       case UNITS_SEC_TYPE:  break;
       case UNITS_HZ_TYPE:   tr = 1 / tr; break;
       default:
	   THD_delete_3dim_dataset( new_dset , False ) ;
	   return "*****************************************\n"
	          "PRIC_main: bad time units type in dataset\n"
                  "*****************************************"   ;
       }
       winsize = ceil(resp->nx / (tr * DSET_NVALS(new_dset)) / 2.0);

       /*-- convert respiratory waveform to phase --*/
       respphase = RIC_ToRespPhase(resp, winsize) ;
       if (respphase == NULL) {
	   THD_delete_3dim_dataset( new_dset , False ) ;
	   return "***************************************\n"
	          "PRIC_main: error transforming resp data\n"
                  "***************************************"   ;
       }
       PLUTO_set_meter(plint, 40);

       /*-- calculate dataset voxel means if not already done --*/
       if (avg == NULL) {
	   avg = RIC_CalcVoxelMeans(new_dset, ignore);
	   if (avg == NULL) {
	       THD_delete_3dim_dataset( new_dset , False ) ;
	       mri_free(respphase);
	       return "**************************************************\n"
		      "PRIC_main: error calculating dataset voxel means 2\n"
		      "**************************************************"   ;
	   }
       }
       PLUTO_set_meter(plint, 43);

       /*-- calculate coefficients for each voxel --*/
       if (RIC_CalcCoeffAB(new_dset, respphase, avg, &ra, &rb, M, ignore)
	   != 0) {

	   THD_delete_3dim_dataset( new_dset , False ) ;
	   mri_free(respphase);
	   return "***************************************************\n"
	          "PRIC_main: error calculating resp a, b coefficients\n"
                  "***************************************************"   ;
       }
   }
   PLUTO_set_meter(plint, 50);

   /*-- do cardiac correction if requested --*/

   if (card != NULL) {
       /*-- correct the image data --*/
       if (RIC_CorrectDataset(new_dset, cardphase, ca, cb, M, ignore) != 0) {
	   THD_delete_3dim_dataset( new_dset , False ) ;
	   mri_free(cardphase);
	   free(ca); free(cb);
	   return "********************************************\n"
	          "PRIC_main: error applying cardiac correction\n"
                  "********************************************"   ;
       }
       PLUTO_set_meter(plint, 60);

       /*-- if requested, write phase data to file and pass to AFNI --*/
       if ( THD_filename_ok(cphase1d) ) {
	   /* Write the file */
	   fp = fopen(cphase1d, "w");
	   nvals = cardphase->nx;
	   cpdata = MRI_FLOAT_PTR(cardphase);
	   for (ival = 0; ival < nvals; ival += 1) {
	       fprintf(fp, "%f\n", cpdata[ival]);
	   }
	   fclose(fp);

	   /* Pass to afni */
	   PLUTO_register_timeseries( cphase1d , cardphase ) ;
       }

       mri_free(cardphase);
       free(ca); free(cb); free(avg); avg = NULL;
   }
   PLUTO_set_meter(plint, 70);

   /*-- do resp correction if requested --*/

   if (resp != NULL) {
       /*-- correct the image data --*/
       if (RIC_CorrectDataset(new_dset, respphase, ra, rb, M, ignore) != 0) {
	   THD_delete_3dim_dataset( new_dset , False ) ;
	   mri_free(respphase);
	   free(ra); free(rb);
	   return "*****************************************\n"
	          "PRIC_main: error applying resp correction\n"
	          "*****************************************"   ;
       }
       PLUTO_set_meter(plint, 80);

       /*-- if requested, write phase data to file and pass to AFNI --*/
       if ( THD_filename_ok(rphase1d) ) {
	   /* Write the file */
	   fp = fopen(rphase1d, "w");
	   nvals = respphase->nx;
	   rpdata = MRI_FLOAT_PTR(respphase);
	   for (ival = 0; ival < nvals; ival += 1) {
	       fprintf(fp, "%f\n", rpdata[ival]);
	   }
	   fclose(fp);

	   /* Pass to afni */
	   PLUTO_register_timeseries( rphase1d , respphase ) ;
       }

       mri_free(respphase);
       free(ra); free(rb); if (avg != NULL) free(avg);
   }
   PLUTO_set_meter(plint, 90);

   /*-- give new dataset to AFNI --*/

   if ( PLUTO_add_dset( plint , new_dset , DSET_ACTION_MAKE_CURRENT ) ) {
       THD_delete_3dim_dataset( new_dset , False ) ;
       return "*********************************************\n"
	      "PRIC_main: failure to add new dataset to AFNI\n"
              "*********************************************" ;
   }
   PLUTO_set_meter(plint, 100);

   /*-- done successfully!!! --*/

   return NULL ;
}
