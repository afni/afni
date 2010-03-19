/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/

/*
   Plugin to edit an AFNI dataset.  This plugin is an interactive version 
   of batch program 3dmerge.

   File:     plug_edit.c
   Author:   B. Douglas Ward
   Date:     15 May 1997


   Mod:      Added Erode/Dilate option to sever narrow connecting path 
             between clusters, by first eroding the outer layer of voxels, 
	     then restoring voxels near the main body of the cluster.
   Author:   B. Douglas Ward
   Date:     18 June 1998
*/


#include "afni.h"

#ifndef ALLOW_PLUGINS
#  error "Plugins not properly set up -- see machdep.h"
#endif

#define MEGA  1048576  /* 2^20 */

char * EDIT_main( PLUGIN_interface * ) ;


static char helpstring[] = 
  "Purpose: AFNI plugin to edit data and return new dataset.\n"
  "Inputs: \n"
  " Dataset       Input and Output datasets\n"
  "   Input       Input dataset that must already be in memory \n"
  "   Prefix        Prefix for output file name \n"
  "   Session       Write output into specified directory (default=./) \n"
  " Clip          Clip intensities in range (lower,upper) to zero \n"
  "   Unscaled      Do not apply any automatic scaling factor \n"
  " Threshold     Use threshold sub-brick to censor the intensities \n"
  " Blur          Gaussian blur using specified function width \n"
  " Zero Vol UL   Zero out entries inside the 3D volume defined by: \n"
  " Zero Vol LL     xLL <= x <=xUL,  yLL <= y <=yUL,  zLL <= z <= zUL \n"
  " Cluster       Form clusters and clip off data not in clusters \n"
  "   Type          Options for setting voxel intensities within a cluster \n"
  "   Radius        Max. distance for 2 voxels to be connected in a cluster \n"
  "   MinVol        Min. volume for a cluster to survive \n"
  " Erode/Dilate  Sever narrow connecting paths between clusters \n"
  "   % Voxels      Min. % of active 'neighbors' for a voxel to survive \n"
  "   Dilate        Restore voxels near main body of cluster \n"
  " Filter        Filter voxel intensities \n"
  "   Type          Defines filter action \n"
  "   Radius        Voxel intensity is effected by voxels within this radius\n"
  " Multiply      Multiply intensities by the given factor\n"
  " Datum         Coerce output data to be stored as the given type \n"
  " Keep Thr      Copy the threshold sub-brick into the output dataset \n"
  " Thr Blur      Apply Gaussian blur function to threshold data sub-brick \n"
  " Thr Filter    Apply specified filter to threshold data sub-brick \n"
  " Thr Datum     Coerce threshold data sub-brick to be stored as given type\n"
  "Author -- BD Ward"
;


/*---------------------------------------------------------------------------*/
/*
  Set up the interface to the user
*/


DEFINE_PLUGIN_PROTOTYPE

PLUGIN_interface * PLUGIN_init( int ncall )
{
  /*----- plugin option labels -----*/
  char * boolean_types[2] = {"False", "True"};
  char * blur_types[3] = {"Sigma", "RMS", "FWHM"};
  char * cluster_types[7] = {"Keep", "Mean", "Max", "AMax", "SMax", "Size",
                             "Order"};
  char * filter_types[6] = {"Mean", "NZMean", "Max", "AMax", "SMax", "Aver" };
  char * brick_types[2] = {"Intensity", "Threshold"};
  char * datum_types[3] = {"Byte", "Short", "Float"};

  PLUGIN_interface * plint ;


  if( ncall > 0 ) return NULL ;  /* only one interface */
  
  /*-- set titles and call point --*/
  plint = PLUTO_new_interface( "3D Edit" , "Dataset Editing" , helpstring ,
			       PLUGIN_CALL_VIA_MENU , EDIT_main  ) ;

  PLUTO_add_hint( plint , "Edit Dataset Contents" ) ;

  PLUTO_set_sequence( plint , "A:newdset:edit" ) ;

  /*---- line 1 of input: Dataset -----*/
  PLUTO_add_option (plint, "Dataset", "Dataset", TRUE);
  PLUTO_add_hint( plint , "Choose input and output" ) ;

  PLUTO_add_dataset (plint, "Input",
		     ANAT_ALL_MASK, FUNC_ALL_MASK,
		     DIMEN_3D_MASK | BRICK_ALLREAL_MASK);
  PLUTO_add_hint( plint , "Choose input dataset" ) ;

  PLUTO_add_string (plint, "Prefix", 0, NULL, 19);
  PLUTO_add_hint( plint , "Name output dataset" ) ;

  PLUTO_add_string (plint, "Session", 0, NULL, 19);
  PLUTO_add_hint( plint , "Name output directory" ) ;

  /*----- line 2 of input: Options -----*/
  PLUTO_add_option (plint, "Options", "Options", FALSE);
  PLUTO_add_hint( plint , "Preprocessing steps" ) ;

  PLUTO_add_string (plint, "Thr->Int",  2, boolean_types, 0);
  PLUTO_add_hint( plint , "Copy threshold over intensity brick?" ) ;

  PLUTO_add_string (plint, "No Neg",    2, boolean_types, 0);
  PLUTO_add_hint( plint , "Zero out negative values?" ) ;

  PLUTO_add_string (plint, "Abs Value", 2, boolean_types, 0);
  PLUTO_add_hint( plint , "Take absolute value?" ) ;
  
  /*----- line 3 of input: Clipping -----*/
  PLUTO_add_option (plint, "Clip", "Clip", FALSE);
  PLUTO_add_hint( plint , "Zero out values in some range" ) ;

  PLUTO_add_number (plint, "Lower", -99999, 99999, 0, 0, TRUE);
  PLUTO_add_hint( plint , "Values above this => zero" ) ;

  PLUTO_add_number (plint, "Upper", -99999, 99999, 0, 0, TRUE);
  PLUTO_add_hint( plint , "Values below this => zero" ) ;

  PLUTO_add_string (plint, "Unscaled?", 2, boolean_types, 0);
  PLUTO_add_hint( plint , "Don't apply scaling factors?" ) ;

  /*----- line 4 of input: Threshold -----*/
  PLUTO_add_option (plint, "Threshold", "Threshold" , FALSE);
  PLUTO_add_hint( plint , "Zero out if threshold brick too small" ) ;

  PLUTO_add_number (plint, "Cutoff"   , 0, 10000, 2, 50, TRUE);
  PLUTO_add_hint( plint , "Threshold values < this => 0" ) ;

  /*----- line 5 of input: Blurring -----*/
  PLUTO_add_option (plint, "Blur", "Blur", FALSE);
  PLUTO_add_hint( plint , "Gaussian convolution" ) ;

  PLUTO_add_string (plint, "Format", 3, blur_types, 0);
  PLUTO_add_hint( plint , "How blur width is specified" ) ;

  PLUTO_add_number (plint, "Width(mm)", 0, 500, 1, 20, TRUE);
  PLUTO_add_hint( plint , "Range of blurring function" ) ;

  /*----- line 6 of input: Zero Volume -----*/
  PLUTO_add_option (plint, "Zero Vol UL", "Zero Vol UL", FALSE);
  PLUTO_add_number (plint, "x Upper", -999, 999, 0, 0, TRUE);
  PLUTO_add_number (plint, "y Upper", -999, 999, 0, 0, TRUE);
  PLUTO_add_number (plint, "z Upper", -999, 999, 0, 0, TRUE);   

  /*----- line 7 of input: Zero Volume -----*/
  PLUTO_add_option (plint, "Zero Vol LL", "Zero Vol LL", FALSE);
  PLUTO_add_number (plint, "x Lower", -999, 999, 0, 0, TRUE);
  PLUTO_add_number (plint, "y Lower", -999, 999, 0, 0, TRUE);
  PLUTO_add_number (plint, "z Lower", -999, 999, 0, 0, TRUE);

  /*----- line 8 of input: Cluster Parameters -----*/
  PLUTO_add_option (plint, "Cluster", "Cluster", FALSE);
  PLUTO_add_hint( plint , "Find and reject small clusters" ) ;

  PLUTO_add_string (plint, "Type", 7, cluster_types, 0);
  PLUTO_add_hint( plint , "How to process data inside clusters" ) ;

  PLUTO_add_number (plint, "Radius(mm)", 0, 100, 1, 20, TRUE);
  PLUTO_add_hint( plint , "Max distance between 'neighbors'" ) ;

  PLUTO_add_number (plint, "MinVol(ul)", 0, 1000, -1, 100, TRUE);
  PLUTO_add_hint( plint , "Min size for cluster to survive" ) ;

  /*----- line 8a of input: Erosion/Dilation option -----*/ /* 18 June 1998 */
  PLUTO_add_option (plint, "Erode/Dilate", "Erode/Dilate", FALSE);
  PLUTO_add_hint (plint , "Sever narrow connecting paths between clusters");

  PLUTO_add_number (plint, "% Voxels", 0, 100, 0, 50, TRUE);
  PLUTO_add_hint (plint,  
		  "Min % of active 'neighbors' for a voxel to survive");

  PLUTO_add_string (plint, "Dilate?",  2, boolean_types, 0);
  PLUTO_add_hint (plint , "Restore voxels near main body of cluster");

  /*----- line 9 of input: Filtering -----*/
  PLUTO_add_option (plint, "Filter", "Filter", FALSE);
  PLUTO_add_string (plint, "Type", 6, filter_types, 0);
  PLUTO_add_number (plint, "Radius(mm)", 0, 100, 1, 20, TRUE);

  /*----- line 10 of input: Multiply -----*/
  PLUTO_add_option (plint, "Multiply", "Multiply", FALSE);
  PLUTO_add_number (plint, "Factor", -99999, 99999, 0, 1, TRUE);

  /*----- line 11 of input: Datum -----*/
  PLUTO_add_option (plint, "Datum", "Datum", FALSE);
  PLUTO_add_string (plint, "Type", 3, datum_types, 1);

  /*----- line 12 of input: Keep Threshold -----*/
  PLUTO_add_option (plint, "Keep Thr", "Keep Thr", FALSE);
  PLUTO_add_string (plint, "Keep?",  2, boolean_types, 0);
  
  /*----- line 13 of input: Threshold Blur -----*/
  PLUTO_add_option (plint, "Thr Blur", "Thr Blur", FALSE);
  PLUTO_add_string (plint, "Format", 3, blur_types, 0);
  PLUTO_add_number (plint, "Width(mm)", 0, 100, 1, 20, TRUE);

  /*----- line 14 of input: Threshold Filter -----*/
  PLUTO_add_option (plint, "Thr Filter", "Thr Filter", FALSE);
  PLUTO_add_string (plint, "Type", 6, filter_types, 0);
  PLUTO_add_number (plint, "Radius(mm)", 0, 100, 1, 20, TRUE);

  /*----- line 15 of input: Threshold Datum -----*/
  PLUTO_add_option (plint, "Thr Datum", "Thr Datum", FALSE);
  PLUTO_add_string (plint, "Type", 3, datum_types, 1);

  return plint ;
}


/*---------------------------------------------------------------------------*/
/*
  Routine to read the editing options.
*/

char * EDIT_opts
( 
  PLUGIN_interface * plint,       /* plugin interface */
  THD_3dim_dataset ** dset,       /* original dataset */
  EDIT_options * edopt,           /* the editing options */
  char ** new_prefix,             /* output file name for edited dataset  */
  char ** new_session,            /* output directory name */
  int * datum,                    /* output intensity sub-brick data type */
  int * keepthr,                  /* boolean for keep threshold sub-brick */
  int * thrdatum                  /* output threshold sub-brick data type */
)

{
  char * tag;                     /* plugin option tag */
  char * str;                     /* input string */
  float rmm;                      /* cluster or filter radius (mm) */
  float vmul;                     /* cluster minimum volume (ul) */
  float thresh;                   /* threshold level */
  MCW_idcode * idc ;              /* dataset id code */
  int ival;                       /* integer value input */
  float bot, top;                 /* clip option limits */
  float blur;                     /* Gaussian blur function width */
  float fval;                     /* input floating point value */
  float dx, dy, dz, dxyz;         /* voxel dimensions */
  float x1, x2, y1, y2, z1, z2;   /* zero volume limits */
  float pv;                       /* pv % voxels within rmm must be active */  
  

  /*----- Check inputs from AFNI to see if they are reasonable-ish -----*/
  if( plint == NULL )
    return 
      "*********************\n"
      "EDIT_opts: NULL input\n"
      "*********************";
  

  /*--------- go to first input line ---------*/
  tag = PLUTO_get_optiontag(plint) ;
  if( (tag==NULL) || (strcmp(tag,"Dataset") != 0) )
    return 
      "*********************************\n"
      "EDIT_opts: Bad dataset option tag\n"
      "*********************************";
  
  idc  = PLUTO_get_idcode(plint) ;
  (*dset) = PLUTO_find_dset(idc) ;
  if( (*dset) == NULL )
    return 
      "****************************\n"
      "EDIT_opts: Bad input dataset\n"
      "****************************";
  
  if (DSET_NUM_TIMES((*dset)) > 1)
    return
      "*************************************************\n"
      "EDIT_opts: Unable to edit time-dependent datasets\n"
      "*************************************************";
  
  /*----- get the dimensions -----*/
  dx = fabs((*dset)->daxes->xxdel);
  dy = fabs((*dset)->daxes->yydel);
  dz = fabs((*dset)->daxes->zzdel);
  dxyz  = dx*dy*dz ;
  
  
  str = PLUTO_get_string(plint);
  if (str != NULL) 
    {
      if( ! PLUTO_prefix_ok(str) )
	return 
	  "*********************\n"
	  "EDIT_opts: bad prefix\n"
	  "*********************";
      else
	*new_prefix = str;
    } 
  
  str = PLUTO_get_string(plint);
  if (str != NULL) 
    {
      *new_session = str;
    } 
  

  /*------ loop over remaining options, check their tags, process them -----*/
  do 
    {
      tag = PLUTO_get_optiontag(plint) ; 
      if( tag == NULL ) break ;
      
      
      /*----- Miscellaneous Options -----*/
      if (strcmp (tag, "Options") == 0)
	{
	  str = PLUTO_get_string(plint);
	  if (strcmp (str, "True") == 0)
	    {
	      if (DSET_THRESH_INDEX(*dset) < 0)
		return 
		  "*********************************************\n"
		  "EDIT_opts: Dataset has no threshold sub-brick\n"
		  "*********************************************";
	      else
		edopt->thtoin = 1;
	    }
	  else
	    edopt->thtoin = 0;
	  
	  str = PLUTO_get_string(plint);
	  if (strcmp (str, "True") == 0)
	    edopt->noneg = 1;
	  else
	    edopt->noneg = 0;
	  
	  str = PLUTO_get_string(plint);
	  if (strcmp (str, "True") == 0)
	    edopt->abss = 1;
	  else
	    edopt->abss = 0;
	  
	  continue;
	}
      
      
      /*----- Clip Option -----*/
      if (strcmp(tag,"Clip") == 0)
	{
	  bot = PLUTO_get_number(plint);
	  top = PLUTO_get_number(plint);
	  str = PLUTO_get_string(plint);
	  
	  if (bot >= top)
	    return 
	      "**********************************************************\n"
	      "EDIT_opts: First clip value must be less than second value\n"
	      "**********************************************************";
	  
	  edopt->clip_bot = bot;
	  edopt->clip_top = top;

	  if (strcmp (str, "True") == 0)
	    edopt->clip_unscaled = 1;
	  else
	    edopt->clip_unscaled = 0;
	  
	  continue;
	}


      /*----- Threshold Option -----*/
      if (strcmp(tag,"Threshold") == 0)
	{
	  thresh = PLUTO_get_number(plint) ;
	  
	  if (thresh < 0.0)
	    return 
	      "******************************\n"
	      "EDIT_opts: Bad Threshold input\n"
	      "******************************";
	  
	  if( thresh > 0.0 && DSET_THRESH_INDEX(*dset) < 0 )
	    return 
	      "*********************************************\n"
	      "EDIT_opts: Dataset has no threshold sub-brick\n"
	      "*********************************************";
	  
	  edopt->thresh =  thresh;
	  edopt->thbot  = -thresh;  /* 26 Dec 2007 */
	  
	  continue;
	} 


      /*----- Blur Option -----*/
      if (strcmp(tag,"Blur") == 0)
	{
	  str = PLUTO_get_string(plint);
	  blur = PLUTO_get_number(plint);
	  
	  if (blur <= 0.0 )
	    return 
	      "*****************************\n"
	      "EDIT_opts: Illegal Blur input\n"
	      "*****************************";
	  
	  if (strcmp(str,"Sigma") == 0)
	    edopt->blur  = blur;
	  else
	    if (strcmp(str,"RMS") == 0)
	      edopt->blur = RMS_TO_SIGMA(blur);
	    else
	      if (strcmp(str,"FWHM") == 0)
		edopt->blur = FWHM_TO_SIGMA(blur);
	      else
		return 
		  "******************************\n"
		  "EDIT_opts: Illegal Blur option\n"
		  "******************************";
	  
	  continue;
	} 
      
      
      /*----- Zero Volume Option -----*/
      if (strcmp(tag, "Zero Vol UL") == 0)
	{
	  x2 = PLUTO_get_number(plint);
	  y2 = PLUTO_get_number(plint);
	  z2 = PLUTO_get_number(plint);
	  
	  tag = PLUTO_get_optiontag(plint);
	  if (strcmp(tag, "Zero Vol LL") == 0)
	    {
	      x1 = PLUTO_get_number(plint);
	      y1 = PLUTO_get_number(plint);
	      z1 = PLUTO_get_number(plint);
	    }
	  else
	    return 
	      "***************************\n"
	      "EDIT_opts: Need Zero Vol LL\n"
	      "***************************";

	  edopt->zv_x1 = x1;  edopt->zv_x2 = x2;
	  edopt->zv_y1 = y1;  edopt->zv_y2 = y2;
	  edopt->zv_z1 = z1;  edopt->zv_z2 = z2;
	  edopt->do_zvol = 1;
	  continue;
	}


      if (strcmp(tag, "Zero Vol LL") == 0)
	{
	  return 
	    "***************************\n"
	    "EDIT_opts: Need Zero Vol UL\n"
	    "***************************";
	}
      
      
      /*----- Cluster Option -----*/
      if (strcmp(tag,"Cluster") == 0)
	{
	  str = PLUTO_get_string(plint);
	  rmm  = PLUTO_get_number(plint) ;
	  vmul = PLUTO_get_number(plint) ;
	  
	  if ( (rmm < dx) && (rmm < dy) && (rmm < dz) )
	    return 
	      "***********************************\n"
	      "EDIT_opts: Cluster rmm is too small\n"
	      "***********************************";

	  if (vmul <= dxyz)
	    return 
	      "************************************\n"
	      "EDIT_opts: Cluster vmul is too small\n"
	      "************************************";
	
	  edopt->clust_rmm  = rmm;
	  edopt->clust_vmul = vmul;
	  
	  if (strcmp(str,"Keep") == 0)
	    edopt->edit_clust = ECFLAG_SAME;
	  else
	    if (strcmp(str,"Mean") == 0)
	      edopt->edit_clust = ECFLAG_MEAN;
	    else
	      if (strcmp(str,"Max") == 0)
		edopt->edit_clust = ECFLAG_MAX;
	      else
		if (strcmp(str,"AMax") == 0)
		  edopt->edit_clust = ECFLAG_AMAX;
		else
		  if (strcmp(str,"SMax") == 0)
		    edopt->edit_clust = ECFLAG_SMAX;
		  else
		    if (strcmp(str,"Size") == 0)
		      edopt->edit_clust = ECFLAG_SIZE;
		    else
		      if (strcmp(str,"Order") == 0)
			edopt->edit_clust = ECFLAG_ORDER;
		      else
		         if (strcmp(str,"Depth") == 0)
			   edopt->edit_clust = ECFLAG_DEPTH;
               else
			return 
			  "*********************************\n"
			  "EDIT_opts: Illegal Cluster option\n"
			  "*********************************";
	  
	  continue;
	}
      
      
      /*----- Erosion/Dilation Option -----*/
      if (strcmp(tag,"Erode/Dilate") == 0)
	{
	  pv  = PLUTO_get_number(plint);
	  if ((pv > 0.0) && (edopt->clust_rmm <= 0.0))
	    return 
	      "******************************************************\n"
	      "EDIT_opts: Erode/Dilate requires use of Cluster option\n"
	      "******************************************************";
	  else
	    edopt->erode_pv  = pv / 100.0;
	  
	  str = PLUTO_get_string(plint);
	  if (strcmp (str, "True") == 0)
	    {
	      if (pv <= 0.0)
		return 
		  "**********************************************\n"
		  "EDIT_opts: Dilate requires use of Erode option\n"
		  "**********************************************";
	      else
		edopt->dilate = 1;
	    }
	  else
	    edopt->dilate = 0;
	  
	  continue;
	}
      
      
      /*----- Filter Option -----*/
      if (strcmp(tag,"Filter") == 0)
	{
	  str = PLUTO_get_string(plint);
	  rmm  = PLUTO_get_number(plint) ;
	  
	  if ( (rmm < dx) && (rmm < dy) && (rmm < dz) )
	    return 
	      "**********************************\n"
	      "EDIT_opts: Filter rmm is too small\n"
	      "**********************************";

	  edopt->filter_rmm  = rmm;
	  
	  if (strcmp(str,"Mean") == 0)
	    edopt->filter_opt = FCFLAG_MEAN;
	  else
	    if (strcmp(str,"NZMean") == 0)
	      edopt->filter_opt = FCFLAG_NZMEAN;
	    else
	      if (strcmp(str,"Max") == 0)
		edopt->filter_opt = FCFLAG_MAX;
	      else
		if (strcmp(str,"AMax") == 0)
		  edopt->filter_opt = FCFLAG_AMAX;
		else
		  if (strcmp(str,"SMax") == 0)
		    edopt->filter_opt = FCFLAG_SMAX;
                  else
		    if (strcmp(str,"Aver") == 0)       /* 07 Jan 1998 */
		      edopt->filter_opt = FCFLAG_AVER;
		    else
		      return 
		        "********************************\n"
		        "EDIT_opts: Illegal Filter option\n"
		        "********************************";
	  
	  continue;
	}
      

      /*----- Multiply Option -----*/
      if (strcmp(tag,"Multiply") == 0)
	{
	  fval  = PLUTO_get_number(plint);
	  
	  if (fval == 0.0)
	    return 
	      "*****************************\n"
	      "EDIT_opts: Bad Multiply input\n"
	      "*****************************";
	  
	  edopt->mult = fval;
	  
	  continue;
	}
      
      
      /*----- Datum Type Option -----*/
      if (strcmp(tag, "Datum") == 0)
	{
	  str = PLUTO_get_string(plint);
	  if (strcmp(str,"Byte") == 0)
	    *datum = MRI_byte;
	  else
	    if (strcmp(str,"Short") == 0)
	      *datum = MRI_short;
	    else
	      if (strcmp(str,"Float") == 0)
		*datum = MRI_float;
	      else 
		{
		  return 
		    "*****************************\n"
		    "EDIT_opts: Illegal Datum type\n"
		    "*****************************";
		}
	  
	  continue;
	}
      
      
      /*----- Keep Threshold Option -----*/
      if (strcmp(tag,"Keep Thr") == 0)
	{
	  str = PLUTO_get_string(plint);
	  if (strcmp (str, "True") == 0)
	    {
	      if (DSET_THRESH_INDEX(*dset) < 0)
		return 
		  "*********************************************\n"
		  "EDIT_opts: Dataset has no threshold sub-brick\n"
		  "*********************************************";
	      else
		*keepthr = 1;
	    }
	  else
	    *keepthr = 0;
	  
	  continue;
	}
      
      
      /*----- Threshold Blur Option -----*/
      if (strcmp(tag,"Thr Blur") == 0)
	{
	  if (DSET_THRESH_INDEX(*dset) < 0)
	    return 
	      "*********************************************\n"
	      "EDIT_opts: Dataset has no threshold sub-brick\n"
	      "*********************************************";

	  str = PLUTO_get_string(plint);
	  blur = PLUTO_get_number(plint) ;
	  
	  if (blur <= 0.0 )
	    return 
	      "***************************************\n"
	      "EDIT_opts: Illegal Threshold Blur input\n"
	      "***************************************";
	  
	  if (strcmp(str,"Sigma") == 0)
	    edopt->thrblur  = blur;
	  else
	    if (strcmp(str,"RMS") == 0)
	      edopt->thrblur = RMS_TO_SIGMA(blur);
	    else
	      if (strcmp(str,"FWHM") == 0)
		edopt->thrblur = FWHM_TO_SIGMA(blur);
	      else
		return 
		  "******************************\n"
		  "EDIT_opts: Illegal Blur option\n"
		  "******************************";
	  
	  *keepthr = 1;
	  
	  continue;
	} 
      
      
      /*----- Threshold Filter Option -----*/
      if (strcmp(tag,"Thr Filter") == 0)
	{
	  if (DSET_THRESH_INDEX(*dset) < 0)
	    return 
	      "*********************************************\n"
	      "EDIT_opts: Dataset has no threshold sub-brick\n"
	      "*********************************************";

	  str = PLUTO_get_string(plint);
	  rmm  = PLUTO_get_number(plint) ;
	  
	  if ( (rmm < dx) && (rmm < dy) && (rmm < dz) )
	    return 
	      "**************************************\n"
	      "EDIT_opts: Thr Filter rmm is too small\n"
	      "**************************************";

	  edopt->thrfilter_rmm  = rmm;
	  
	  if (strcmp(str,"Mean") == 0)
	    edopt->thrfilter_opt = FCFLAG_MEAN;
	  else
	    if (strcmp(str,"NZMean") == 0)
	      edopt->thrfilter_opt = FCFLAG_NZMEAN;
	    else
	      if (strcmp(str,"Max") == 0)
		edopt->thrfilter_opt = FCFLAG_MAX;
	      else
		if (strcmp(str,"AMax") == 0)
		  edopt->thrfilter_opt = FCFLAG_AMAX;
		else
		  if (strcmp(str,"SMax") == 0)
		    edopt->thrfilter_opt = FCFLAG_SMAX;
                  else
		    if (strcmp(str,"Aver") == 0)       /* 07 Jan 1998 */
		      edopt->thrfilter_opt = FCFLAG_AVER;
		    else
		      return 
		        "************************************\n"
		        "EDIT_opts: Illegal Thr Filter option\n"
		        "************************************";

	  *keepthr = 1;
	  
	  continue;
	}
      

      /*----- Threshold Datum Type Option -----*/
      if (strcmp(tag, "Thr Datum") == 0)
	{
	  if (DSET_THRESH_INDEX(*dset) < 0)
	    return 
	      "*********************************************\n"
	      "EDIT_opts: Dataset has no threshold sub-brick\n"
	      "*********************************************";

	  str = PLUTO_get_string(plint);
	  if (strcmp(str,"Byte") == 0)
	    *thrdatum = MRI_byte;
	  else
	    if (strcmp(str,"Short") == 0)
	      *thrdatum = MRI_short;
	    else
	      if (strcmp(str,"Float") == 0)
		*thrdatum = MRI_float;
	      else 
		{
		  return 
		    "***************************************\n"
		    "EDIT_opts: Illegal Threshold Datum type\n"
		    "***************************************";
		}
	  
	  *keepthr = 1;
	  
	  continue;
	}
      
      
      /*----- Illegal Option -----*/
      else 
	{
	  return 
	    "***********************************\n"
	    "EDIT_opts: Illegal optiontag found!\n"
	    "***********************************";
	}
      
    } while(1) ;
  
  
  /*---------- End of input options ----------*/
  
  return NULL;
}


/*---------------------------------------------------------------------------*/
/*
  Main routine for this plugin (will be called from AFNI).
*/

char * EDIT_main( PLUGIN_interface * plint )
{
  EDIT_options PE_edopt;
  int PE_keepthr = 0;
  int PE_datum = ILLEGAL_TYPE;
  int PE_thdatum = ILLEGAL_TYPE;
  int PE_be_quiet = 0;
  char * PE_output_session = NULL;
  char * PE_output_prefix = NULL;
  

  int nx,ny,nz , nxyz , ii ,  ival;
  THD_3dim_dataset * old_dset = NULL, * dset=NULL , * new_dset=NULL ;
  int     datum ;
  float fimfac , fimfacinv , first_fimfac , thrfac ;
  int   output_datum , output_thdatum ;
  int   input_datum  , input_thdatum , first_datum ;
  
  float thr_stataux[MAX_STAT_AUX] ;
  char * str;

  
  
  /*-- set up for dataset editing --*/
  INIT_EDOPT( &PE_edopt ) ;


  /*----- read input options -----*/
  str = EDIT_opts (plint, &old_dset, &PE_edopt,  
		   &PE_output_prefix, &PE_output_session, &PE_datum,
		   &PE_keepthr, &PE_thdatum);
  if (str != NULL)  return (str);

  /*----- make a copy of the original dataset -----*/
  dset = PLUTO_copy_dset (old_dset, NULL);
  DSET_unload (old_dset);

  /*----- get the dimensions -----*/
  nx = dset->daxes->nxx ;
  ny = dset->daxes->nyy ;
  nz = dset->daxes->nzz ; nxyz = nx*ny*nz ;


  ival        = DSET_PRINCIPAL_VALUE(dset) ;
  input_datum = DSET_BRICK_TYPE(dset,ival) ;
  if (PE_datum >= 0) output_datum = PE_datum ;
  else               output_datum = input_datum ;
  
  new_dset = EDIT_empty_copy( dset ) ;
  
  EDIT_dset_items( new_dset ,
		   ADN_prefix , PE_output_prefix ,
		   ADN_label1 , PE_output_prefix ,
		   ADN_directory_name , PE_output_session ,
		   ADN_none ) ;
  strcat( new_dset->self_name , "(PE)" ) ;

  { char * his = PLUTO_commandstring(plint) ;
    tross_Copy_History( dset , new_dset ) ;
    tross_Append_History( new_dset, his ) ; free(his) ;
  }
  
  if( ! PE_keepthr && new_dset->dblk->nvals > 1 )
    EDIT_dset_items( new_dset ,
		     ADN_nvals , 1 ,
		     ADN_func_type , FUNC_FIM_TYPE ,
		     ADN_none ) ;
  
  if ( PE_keepthr && ISFUNC(new_dset) && FUNC_HAVE_THR(new_dset->func_type) )
    {
      ii            = FUNC_ival_thr[dset->func_type] ;
      input_thdatum = DSET_BRICK_TYPE(dset,ii) ;
      if (PE_thdatum >= 0) output_thdatum = PE_thdatum ;
      else                 output_thdatum = input_thdatum ;
    } 
  else 
    {
      output_thdatum = input_thdatum = ILLEGAL_TYPE ;
    }
  
  if ( THD_is_file(new_dset->dblk->diskptr->header_name) )
    {
      fprintf(stderr,
	      "*** Output file %s already exists -- cannot continue!\n",
	      new_dset->dblk->diskptr->header_name ) ;
      return("*** Output file already exists -- cannot continue!");
      /*EXIT(1) ;*/
    }
  
  if (! PE_be_quiet)
    {
      printf("-- editing input dataset in memory (%.1f MB)",
	          ((double)dset->dblk->total_bytes) / MEGA ) ;
      fflush(stdout) ;
    } 


  EDIT_one_dataset( dset , &PE_edopt ) ;  /* all the real work */

  
  if (! PE_be_quiet)  { printf(".\n") ; fflush(stdout) ; }
  

  /** Coerce the output data type into a new brick, if needed **/
  ival = DSET_PRINCIPAL_VALUE(dset) ;
  ii   = DSET_PRINCIPAL_VALUE(new_dset) ;
  
  if( input_datum == output_datum )
    {
      /*
	Attach the brick of the input dataset to the brick of the output.
	This isn't exactly kosher, but we are exiting almost immediately. 
      */ 
      if (! PE_be_quiet) 
	printf ("connecting edited input to be output \n");
      mri_fix_data_pointer (DSET_ARRAY(dset,ival), DSET_BRICK(new_dset,ii));
      DSET_BRICK_FACTOR(new_dset,ii) = DSET_BRICK_FACTOR(dset,ival);
    }
  else 
    {
      /** Must create a new brick and do the conversion **/ 
      void * dfim , * efim ;
      float etop ;
      
      if(! PE_be_quiet)
	{
	  printf("-- coercing output datum to be %s\n",
		 MRI_TYPE_name[output_datum]);
	} 
      
      efim = DSET_ARRAY(dset,ival) ;
      dfim = (void *) XtMalloc( mri_datum_size(output_datum) * nxyz ) ;
      
      fimfac = EDIT_coerce_autoscale( nxyz , input_datum  , efim ,
				      output_datum , dfim  ) ;
      
      DSET_BRICK_FACTOR(new_dset,ii) = (fimfac != 0.0 && fimfac != 1.0)
	? 1.0/fimfac : 0.0 ;

      EDIT_substitute_brick( new_dset , ii , output_datum , dfim ) ;
      mri_free( DSET_BRICK(dset,ival) ) ;
    }

  /** Now do the threshold data **/
  
  if( output_thdatum >= 0 )
    {      
      ival = FUNC_ival_thr[    dset->func_type] ;
      ii   = FUNC_ival_thr[new_dset->func_type] ;
      
      if( input_thdatum == output_thdatum )
	{
	  if (! PE_be_quiet)
	    printf ("connecting input and output thresholds \n") ;
	  mri_fix_data_pointer (DSET_ARRAY(dset,ival),DSET_BRICK(new_dset,ii));
	  DSET_BRICK_FACTOR(new_dset,ii) = DSET_BRICK_FACTOR(dset,ival) ; 
	} 
      else 
	{
	  void * dfim , * efim ;
	  
	  if( ! PE_be_quiet )
	    {
	      printf("-- coercing threshold datum to be %s\n",
		     MRI_TYPE_name[output_thdatum]);
	    } 
	  
	  efim = DSET_ARRAY(dset,ival) ;
	  dfim = (void *) XtMalloc( mri_datum_size(output_thdatum) * nxyz ) ;
	  
	  switch( output_thdatum )
	    {
	    default: fprintf(stderr,"** illegal output_thdatum = %d\n",
			     output_thdatum);
	      return("** illegal output_thdatum");
	      /* EXIT(1) ;*/
	    
	    case MRI_float:
	      fimfacinv = 0.0 ;
	      fimfac    = DSET_BRICK_FACTOR(dset,ival) ;
	      if( fimfac == 0.0 )
		{
		  fimfac = (input_thdatum == MRI_short)
		    ? 1.0/FUNC_scale_short[dset->func_type]
		    : (input_thdatum == MRI_byte)
		    ? 1.0/FUNC_scale_byte[dset->func_type] : 0.0 ;
		}
	      break ;

	    case MRI_short:
	      if( input_datum == MRI_float )
		{
		  fimfac    = FUNC_scale_short[new_dset->func_type] ;
		  fimfacinv = 1.0 / fimfac ;
		} 
	      else 
		if( input_datum == MRI_byte )
		  {
		    fimfac    = ((float)FUNC_scale_short[new_dset->func_type])
		      / FUNC_scale_byte[new_dset->func_type] ;
		    fimfacinv = 1.0 / FUNC_scale_short[new_dset->func_type] ;
                  } 
		else 
		  {
		    fprintf(stderr,
			    "** illegal input_thdatum = %d\n",input_thdatum);
                    return("** illegal input_thdatum");
		    /* EXIT(1) ;*/
                  }
	      break ;
	      
	    case MRI_byte:
	      if( input_datum == MRI_float )
		{
		  fimfac    = FUNC_scale_byte[new_dset->func_type] ;
		  fimfacinv = 1.0 / fimfac ;
		}
	      else 
		if( input_datum == MRI_short )
		  {
		    fimfac    = ((float)FUNC_scale_byte[new_dset->func_type])
		      / FUNC_scale_short[new_dset->func_type] ;
		    fimfacinv = 1.0 / FUNC_scale_byte[new_dset->func_type] ;
                  } 
		else 
		  {
		    fprintf(stderr,"** illegal input_thdatum = %d\n",
			    input_thdatum);
	            return("** illegal input_thdatum");
	      /* EXIT(1) ;*/
                  }
	      break;
            }
	  
	  EDIT_coerce_scale_type( nxyz , fimfac ,
				  DSET_BRICK_TYPE(dset,ival),efim ,
				  output_thdatum,dfim );
	  
	  DSET_BRICK_FACTOR(new_dset,ii) = fimfacinv;
	  EDIT_substitute_brick( new_dset , ii , output_thdatum , dfim );
	  mri_free( DSET_BRICK(dset,ival) );
	}
    }
  
  if (! PE_be_quiet)
    printf("-- Writing edited dataset:%s\n" , DSET_BRIKNAME(new_dset) ) ;
  
  ival = PLUTO_add_dset( plint , new_dset , DSET_ACTION_MAKE_CURRENT ) ;

  if (ival)
    {
      THD_delete_3dim_dataset( new_dset , False ) ;
      return 
	"*********************************************\n"
	"EDIT_main: failure to add new dataset to AFNI\n"
	"*********************************************" ;
    }
  else
    return (NULL) ;
  
}





