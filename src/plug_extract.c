/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/
   
#include "afni.h"

#ifndef ALLOW_PLUGINS
#  error "Plugins not properly set up -- see machdep.h"
#endif

/* -----------------------START---------------------------------*/
/* definition and decleration part to suport the main algorithm */

#include <stdlib.h>
#include <stdio.h>
#include <math.h>

/*	Definitions of prototypes and declaration of support functions 
	this is taken from the list of include files that I use in the original code*/ 

/*-------------------------------------------------------------------*/
/* COMPLEX STRUCTURE */

typedef struct {
    float x, y, z;
} fXYZ;


/***********************************************************************
  Plugin to extract 3D+time time courses whos index or xyz corrdinates 
  match a certain criterion
************************************************************************/
typedef struct extract_data
	{
		  int nxx;			/* number of voxels in the x direction */
		  int nyy;			/* number of voxels in the y direction */
		  int nzz;			/* number of voxels in the z direction */
		  char *dsetname; /* prefix of data set analyzed */
		  int ignore;			/* number ofpoints to ignore from time courses */
		  int ln;			/* length of FMRI vector */
		  int dtrnd;		/* remove linear trend or just the mean */
		  int errcode;		/* error code number returned from function */
		  int out;			/* flag for writing to a file */
		  int outts;		/* flag for writing time series to a file */
		  int format;
		  int iloc;
		  int xloc;
		  int yloc;
		  int zloc;
		  float pass;  /* value of voxels that are extracted */
		  float fail;	/* value of voxels that are not extacted */
		  int ncols;
		  int nrows;
		  float * indvect;	/* vector that will hold the list of indices */
		  fXYZ * xyzvect;			/* vecor that will hold the list of xyz coordinates */
		  char * new_prefix; /* new prefix for data set */
		  char * strout;
		  char * strin;
		  FILE * outwritets;
		  FILE * outlogfile;
		  char outname[PLUGIN_MAX_STRING_RANGE]; /* output data file name */
	};

/*--------------------- string to 'help' the user --------------------*/

static char helpstring[] =
  "                3D+t Extract Plugin \n"
  "This Plugin is similar to the 4D Dump Plugin. The only difference is that\n"
  "in addition to the ascii output, a Mask AFNI brick is also created.\n"
  "The plugin takes an ascii mask file as an input specifying the AFNI indices\n"
  "or AFNI X Y Z coordinates of the voxel time series that should be extracted\n"
  "to an ascii file. A brick is also created which has a value of 'Pass Value'\n"
  "for voxels specified in the ascii mask file and 0 for the remaining voxels.\n\n"
    "Plugin Inputs\n"
  "   1- Data :\n"
  "      3D+time    -> Chooser for 3D+time data set.\n"
  "      Ignore     -> Specify the number of points to ignore from the beginning \n"
  "                    of each voxel time series.\n"
  "      Dtrnd      -> (Y/N) Apply detrending to time series. \n"
  "                    If the dtrnd option is off, time series do not have zero mean.\n\n"
  "   2- Mask file :\n"
  "      Mask File  -> The index or X Y Z coordinates that specify wich voxel time series\n"
  "                    should be output are specified in an ascii file.\n"
  "                    Each line in the ascii file holds the index or X Y Z coordinates \n"
  "                    of the voxel time series to be extracted. Each line of the ascii \n"
  "                    file can hold many values, the user chooses which ones correspond \n"
  "                    to i or the X Y Z triplet. The X Y Z coordinates used for masking\n"
  "                    correspond to those shown in the AFNI window and NOT the graph window. \n"
  "                    You can see these coordinates in the upper left side of the AFNI window.\n"
  "                    To do so, you must switch the voxel coordinate units from mm to slice coordinates.\n"
  "                      Define Datamode -> Misc -> Voxel Coords ?\n"
  "                    Masking files could be the ascii output files of other plugins such as \n"
  "                   '3Ddump' and 'Hilber Delay98'.\n"
  "      N Columns  -> Total number of values on each line in the ascii mask file.\n"
  "      Pass Value -> Value given to voxels in the output brick if they are present\n"
  "                    in the ascii mask file. Otherwise the value is 0\n"
  "   3- Index Mask : (optional)\n"
  "      i col.     -> Column number where the AFNI indices for the time series to be extracted \n"
  "                    are stored in the ascii mask file.\n"
  "   4- XYZ Mask : (optional)\n"
  "      x,y,z col. -> Specify the column numbers where X Y Z triplets are stored\n\n"
  "   5- Output :\n"
  "      Afni Prfx  -> 3D Mask Brick generated by giving voxels specified in the ascii\n"
  "                    mask file a value of 'Pass Value' and 0 otherwise.\n"
  "                    If no output prefix is specified, the default is the prefix\n"
  "                    of the 3D+time input brick with the extenstion '.XTRCT' appended to it.\n"
  "      Filename   -> Name of the ascii file where the time series are written.\n"
  "                    If no output filename is specified, the ouput is the prefix\n"
  "                    of the 3D+time input brick with the extenstion '.4Ddump' appended to it.\n"
  "                    A LOG file, 'Filename.log' is also written to disk.\n"
  "                    The log file contains all the parameters settings used\n"
  "                    for generating 'Filename'.\n"
  "      Format     -> ts[1] ts[2] .... -> one time series per line.\n"
  "                    i x y z ts[1] ts[2] .... -> index, X Y Z coordinates and time series per line.\n\n"
  "   PS: If all the voxels specified in the mask file exist in the data file then each line\n"
  "       in the output file correspond to the line in the mask file. Otherwise you should use\n"
  "       the option i x y z ts[1] ts[2] .... to know which time series came from which voxel\n\n"
  "If you have/find questions/comments/bugs about the plugin, \n"
  "send me an E-mail: ziad@image.bien.mu.edu\n\n"
  "                          Ziad Saad June 29 97, lastest update Feb 23 98.\n\n"
;

/*--------------------- strings for output format --------------------*/

static char * yn_strings[] = { "n" , "y" }; 
static char * format_strings[] = { "i x y z ts[1] ..." , "ts[1] ts[2] ..." };

#define NUM_YN_STRINGS (sizeof(yn_strings)/sizeof(char *))
#define NUM_FORMAT_STRINGS (sizeof(yn_strings)/sizeof(char *))


#define YUP  1
#define NOPE 0

#define ERROR_NOSUCHVOXEL 	1				/* Nothing to do in function */
#define ERROR_FILEREAD		2
#define ERROR_FILEWRITE		2
#define ERROR_OPTIONS		17
#define ERROR_OUTCONFLICT 	19

/*----------------- prototypes for internal routines -----------------*/

static char * EXTRACT_main( PLUGIN_interface * ) ;  /* the entry point */

static void EXTRACT_tsfunc( double T0 , double TR ,
                   int npts , float ts[] , double ts_mean , double ts_slope , 
                   void * udp , float * dumb) ;

static void show_ud (struct extract_data* ud);

static void write_ud (struct extract_data* ud);

static void indexTOxyz (struct extract_data* ud, int ncall, int *xpos , int *ypos , int *zpos);

static void error_report (struct extract_data* ud, int ncall );

static void writets (struct extract_data* ud,float * ts, int ncall);

float * extract_index (char *fname, int ind_col_loc, int ncols, int *nrows, int *Err);

static fXYZ * extract_xyz (char *fname, int x_col_loc, int y_col_loc, int z_col_loc, int ncols, int *nrows, int *Err);

static void disp_vect (float *v,int l);

static int filexists (char *f_name);

static int f_file_size (char *f_name);

/*---------------------------- global data ---------------------------*/

static PLUGIN_interface * global_plint = NULL ;

/***********************************************************************
   Set up the interface to the user:
    1) Create a new interface using "PLUTO_new_interface";

    2) For each line of inputs, create the line with "PLUTO_add_option"
         (this line of inputs can be optional or mandatory);

    3) For each item on the line, create the item with
        "PLUTO_add_dataset" for a dataset chooser,
        "PLUTO_add_string"  for a string chooser,
        "PLUTO_add_number"  for a number chooser.
************************************************************************/


DEFINE_PLUGIN_PROTOTYPE

PLUGIN_interface * PLUGIN_init( int ncall )
{
   PLUGIN_interface * plint ;     /* will be the output of this routine */

   if( ncall > 0 ) return NULL ;  /* only one interface */

   /*---------------- set titles and call point ----------------*/

   plint = PLUTO_new_interface( "3D+t Extract" ,
                                "Extract voxel time courses given their index or XYZ coordinates" ,
                                helpstring ,
                                PLUGIN_CALL_VIA_MENU , EXTRACT_main  ) ;

   global_plint = plint ;  /* make global copy */

   /*--------- 1st line: Input dataset and mask files ---------*/

   PLUTO_add_option( plint ,
                     "Data" ,  /* label at left of input line */
                     "Data" ,  /* tag to return to plugin */
                     TRUE       /* is this mandatory? */
                   ) ;

   PLUTO_add_dataset(  plint ,
                       "3D+time" ,        /* label next to button   */
                       ANAT_EPI_MASK ,    /* take only EPI datasets */
                       0,  				   /*  No fim funcs   */
                       DIMEN_4D_MASK |    /* need 3D+time datasets  */
                       BRICK_ALLREAL_MASK /* need real-valued datasets */
                    ) ;
  
   PLUTO_add_number( plint ,
                    "Ignore" ,  /* label next to chooser */
                    0 ,         /* smallest possible value */
                    50 ,        /* largest possible value (inactivated for now)*/
                    0 ,         /* decimal shift (none in this case) */
                    0 ,         /* default value */
                    FALSE       /* allow user to edit value? */
                  ) ;
	PLUTO_add_string( plint ,
                     "Dtrnd" ,  /* label next to textfield */
                     2,yn_strings,    /* strings to choose among */
                     1         /* Default option */
                   ) ;
	
	/*---------- 2nd line: Mask file info  ----------*/
   PLUTO_add_option( plint ,
                     "Mask file" ,  /* label at left of input line */
                     "Mask" ,  /* tag to return to plugin */
                     TRUE       /* is this mandatory? */
                   ) ;
   
   PLUTO_add_string( plint , "Mask File" , 0 , NULL , 19 ) ;
   
   PLUTO_add_number( plint ,
                    "N Columns" ,  /* label next to chooser */
                    1 ,         /* smallest possible value */
                    1000 ,        /* largest possible value (inactivated for now)*/
                    0 ,         /* decimal shift (none in this case) */
                    3 ,         /* default value */
                    TRUE       /* allow user to edit value? */
                  ) ;
						
	PLUTO_add_number( plint ,
                    "Pass Value" ,  /* label next to chooser */
                    -10000 ,         /* smallest possible value */
                    10000 ,        /* largest possible value (inactivated for now)*/
                    0 ,         /* decimal shift (none in this case) */
                    1 ,         /* default value */
                    TRUE       /* allow user to edit value? */
                  ) ;
	
   

   /*---------- 3rd line: index mask location ----------*/
   
   PLUTO_add_option( plint ,
                     "Index Mask ?" ,  /* label at left of input line */
                     "Index" ,  /* tag to return to plugin */
                     FALSE       /* is this mandatory? */
                   ) ;
   
   PLUTO_add_number( plint ,
                    "i col." ,  /* label next to chooser */
                    1 ,         /* smallest possible value */
                    1000 ,        /* largest possible value (inactivated for now)*/
                    0 ,         /* decimal shift (none in this case) */
                    1 ,         /* default value */
                    TRUE       /* allow user to edit value? */
                  ) ;
	
                   
   /*---------- 4th line: xyz mask location ----------*/

   PLUTO_add_option( plint ,
                     "XYZ Mask ?" ,  /* label at left of input line */
                     "XYZ" ,  /* tag to return to plugin */
                     FALSE       /* is this mandatory? */
                   ) ;

   PLUTO_add_number( plint ,
                    "x col." ,  /* label next to chooser */
                    1 ,         /* smallest possible value */
                    1000 ,        /* largest possible value */
                    0 ,         /* decimal shift (none in this case) */
                    2 ,         /* default value */
                    TRUE       /* allow user to edit value? */
                  ) ;
	
	PLUTO_add_number( plint ,
                    "y col." ,  /* label next to chooser */
                    1 ,         /* smallest possible value */
                    1000 ,        /* largest possible value */
                    0 ,         /* decimal shift (none in this case) */
                    3 ,         /* default value */
                    TRUE       /* allow user to edit value? */
                  ) ;
                  

	PLUTO_add_number( plint ,
                    "z col." ,  /* label next to chooser */
                    1 ,         /* smallest possible value */
                    1000 ,      /* largest possible value */
                    0 ,         /* decimal shift (none in this case) */
                    4 ,         /* default value */
                    TRUE       /* allow user to edit value? */
                  ) ;
                  
   /*---------- 5th line: output stuff ----------*/

   PLUTO_add_option( plint ,
                     "Output" ,  /* label at left of input line */
                     "Output" ,  /* tag to return to plugin */
                     TRUE        /* is this mandatory? */
                   ) ;

   PLUTO_add_string( plint ,
                     "Afni Prfx" ,  /* label next to textfield */
                     0,NULL ,    /* no fixed strings to choose among */
                     19          /* 19 spaces for typing in value */
                   ) ;
   
   
   PLUTO_add_string( plint , "Filename" , 0 , NULL , 19 ) ;
              
   PLUTO_add_string( plint ,
                     "Format" ,  /* label next to textfield */
                     2,format_strings,    /* strings to choose among */
                     0          /* Default option */
                   ) ;
   
   return plint ;
}

/***************************************************************************
  Main routine for this plugin (will be called from AFNI).
  If the return string is not NULL, some error transpired, and
  AFNI will popup the return string in a message box.
****************************************************************************/

static char * EXTRACT_main( PLUGIN_interface * plint )
{
   struct extract_data uda,*ud;
   MRI_IMAGE * tsim;
   MCW_idcode * idc ;                          /* input dataset idcode */
   THD_3dim_dataset * old_dset , * new_dset ;  /* input and output datasets */
   char *tmpstr , * str , *nprfxstr;                 
   int   ntime, nvec ,nprfx, Err , itmp;
	float * vec , fs , T ;
	char * tag;                     /* plugin option tag */	
	
	/* Allocate as much character space as Bob specifies in afni.h + a bit more */
	
	tmpstr = (char *) calloc (PLUGIN_MAX_STRING_RANGE+10,sizeof(char));
	nprfxstr = (char *) calloc (PLUGIN_MAX_STRING_RANGE+10,sizeof(char));
	
	if (tmpstr == NULL || nprfxstr == NULL) 
									  return "********************\n"
												"Could not Allocate\n"
												"a teeni weeni bit of\n"
												"Memory ! Go complain\n"
												"to yer Mamma ! \n"
												"********************\n";
												
	ud = &uda;		/* ud now points to an allocated space */
	ud->errcode = 0;	/*reset error flag */
	
   /*--------------------------------------------------------------------*/
   /*----- Check inputs from AFNI to see if they are reasonable-ish -----*/

   /*--------- go to first input line ---------*/
		
   tag = PLUTO_get_optiontag(plint) ;
   
   if (tag == NULL)
   	{
   		return "************************\n"
             "Bad 1st line option \n"
             "************************"  ;
   	}	

   idc      = PLUTO_get_idcode(plint) ;   /* get dataset item */
   old_dset = PLUTO_find_dset(idc) ;      /* get ptr to dataset */
   if( old_dset == NULL )
      return "*************************\n"
             "Cannot find Input Dataset\n"
             "*************************"  ;
   
   ud->dsetname = DSET_FILECODE (old_dset);
	
	ud->ignore = PLUTO_get_number(plint) ;    /* get number item */
	
	str = PLUTO_get_string(plint) ; 				
	ud->dtrnd = (int)PLUTO_string_index( str , NUM_YN_STRINGS , yn_strings );
	
	
	
	/*--------- loop over ramining options ---------*/
	
		
	ud->iloc = -1;
	ud->xloc = -1;
	ud->yloc = -1;
	ud->zloc = -1;
	do
		{
			tag = PLUTO_get_optiontag(plint) ;
			if (tag == NULL) break;
			if (strcmp (tag, "Mask") == 0)
				{
					ud->strin = PLUTO_get_string(plint) ; 
					ud->ncols = PLUTO_get_number(plint) ;
					ud->pass = PLUTO_get_number(plint) ;
					ud->fail = 0;  /* Set voxels that don't make it to 0 */
					continue;
				}
			
			if (strcmp (tag, "Index") == 0)
				{
					ud->iloc = PLUTO_get_number(plint) ;    /* get number item */
					continue;
				}
   		
			if (strcmp (tag, "XYZ") == 0)
   				{
  	 					ud->xloc = PLUTO_get_number(plint) ;    /* get number item */
  	 					ud->yloc = PLUTO_get_number(plint) ;    /* get number item */
  	 					ud->zloc = PLUTO_get_number(plint) ;    /* get number item */
  	 					continue;
   				}

			if (strcmp (tag, "Output") == 0)
					{
						ud->new_prefix = PLUTO_get_string(plint) ;   /* get string item (the output prefix) */
							/* check to see if the field is empty */
							if (ud->new_prefix == NULL)
									nprfx = 0;
								else
									nprfx = 1;
							/* check if the size is larger than 0. I did not want to check for this unless it's allocated */
							if (nprfx == 1 && (int)strlen (ud->new_prefix) == 0)
								nprfx = 0;

							if (nprfx == 0)		/* now create the new name and make new_prefix point to it */
								{
									sprintf (nprfxstr,"%s.XTRCT",DSET_PREFIX (old_dset));
									ud->new_prefix = nprfxstr;
									/*printf ("New prefix is set to be : %s\n\a",ud->new_prefix);*/
								}

   					
						if( ! PLUTO_prefix_ok(ud->new_prefix) )      /* check if it is OK */
      					return "************************\n"
            					 "Output Prefix is illegal\n"
            					 "************************"  ;

   					ud->strout = PLUTO_get_string(plint) ; 

   					str = PLUTO_get_string(plint) ; 				
						ud->format = (int)PLUTO_string_index( str , NUM_FORMAT_STRINGS , format_strings );
						continue;
					}
			
 		} while (1);
 	/* ------------------ Check for some errorsor inconsistencies ------------- */
 	 	
 	if (ud->iloc == -1 && ud->xloc == -1)
 		{
 			return "**************************\n"
 					 "At least iloc or x/y/zloc\n"
 					 "must be specified\n"
 					 "**************************\n"
 					 ;
 		}
 	
 	if (ud->iloc != -1 && ud->xloc != -1)
 		{
 			return "***************************\n"
 					 "iloc AND x/y/zloc can not\n"
 					 "be simultaneously specified\n"
 					 "***************************\n"
 					 ;
 		}
 	
 	
 	/* ------------------Done with user parameters ---------------------------- */
	
	/* Now loadup that index list or the xyz list */
	if (ud->iloc != -1)
		{	
			itmp = 0;  /* might want to give option of setting it to number of rows if*/ 
                    /* the users know it, otherwise, it is automatically determined*/    
			ud->indvect = extract_index (ud->strin, ud->iloc, ud->ncols, &itmp, &Err);
		}
	else 		/* assuming the only other case is that x y z are specified */
		{
			itmp = 0; 
			ud->xyzvect = extract_xyz (ud->strin , ud->xloc , ud->yloc , ud->zloc , ud->ncols, &itmp, &Err);
		}
	
		
	ud->nrows = itmp;
	
	switch (Err)
	{
		case (0):
			break;
		case (1):
			return "****************************************\n"
			       "index location should be > 0 and < ncols\n"
			       "****************************************\n";
		case (2):
			return "********************************************\n"
                "file size and number of columns do not match\n"
			       "********************************************\n";
		case (3):
			return "**********************\n"
                "Can't find matrix file\n"
			       "**********************\n";
		case (4):
			return "*****************\n"
                "ncols must be > 0\n"
			       "*****************\n";
		case (5):
			return "****************************************\n"
                "x/y/z column numbers can NOT be the same\n"
			       "****************************************\n";
		default:
			return "****************************************\n"
                "Should not have gotten here .....\n"
			       "****************************************\n";
		
	}
	
	
	if (strcmp (ud->strout,"") == 0)   /* no output file is specified */ 
 		{
 			sprintf ( tmpstr , "%s" , ud->new_prefix);
 			ud->strout = tmpstr;
 		}
 	
 	if (filexists(ud->strout) == 1)
 		{
 			return "*******************************\n"
					 "Outfile exists, won't overwrite\n"
					 "*******************************\n";	
 		}
 	ud->outwritets = fopen (ud->strout ,"w"); 	
 	
 	sprintf ( tmpstr , "%s.log" , ud->strout);
 	if (filexists(tmpstr) == 1)
 		{
 			return "*******************************\n"
					 "Outfile.log exists, won't overwrite\n"
					 "*******************************\n";	
 		}
 	
 	ud->outlogfile = fopen (tmpstr ,"w"); 
 	 	
 	if ((ud->outwritets == NULL) || (ud->outlogfile == NULL) )
						{
							ud->errcode = ERROR_FILEWRITE; 
							
							return "***********************\n"
									 "Could Not Write Outfile\n"
									 "***********************\n";
						}				
 	
	
	ud->nxx = (int)old_dset->daxes->nxx;				/* get data set dimensions */
	ud->nyy = (int)old_dset->daxes->nyy;
	ud->nzz = (int)old_dset->daxes->nzz;
	
   /* ready to dump the log file */
   write_ud (ud);
   
   /*------------- ready to compute new dataset -----------*/
  
   new_dset = PLUTO_4D_to_typed_fim( old_dset ,             /* input dataset */
                               ud->new_prefix ,           /* output prefix */
                               -1,							/* negative value indicating data type is like original brick */
                               ud->ignore ,               /* ignore count */
                               ud->dtrnd ,                    /* detrend */
                               EXTRACT_tsfunc ,         /* timeseries processor */
                               (void *)ud          /* data for tsfunc */
                             ) ;
   
   PLUTO_add_dset( plint , new_dset , DSET_ACTION_MAKE_CURRENT ) ;

	fclose (ud->outlogfile);
	fclose (ud->outwritets);
	
	free (tmpstr);		
	free (nprfxstr);
	
   return NULL ;  /* null string returned means all was OK */
}

/**********************************************************************
   Function that does the real work
***********************************************************************/

static void EXTRACT_tsfunc( double T0 , double TR ,
                   int npts , float ts[] , double ts_mean , double ts_slope ,
                   void * udp , float * dumb)
{
   static int nvox , ncall ;
	struct extract_data uda,*ud;
	float xcor=0.0 ,  tmp=0.0 , tmp2 = 0.0 ,  dtx = 0.0 ,\
			 slp = 0.0 , vts = 0.0 , vrvec = 0.0 , rxcorCoef = 0.0;
	int i , is_ts_null , status , opt , actv , zpos , ypos , xpos , hit;
	
	ud = &uda;
	ud = (struct extract_data *) udp;
	
	
   /** is this a "notification"? **/

	
   if( dumb == NULL ){
		
      if( npts > 0 ){  /* the "start notification" */

         PLUTO_popup_meter( global_plint ) ;  /* progress meter  */
         nvox  = npts ;                       /* keep track of   */
         ncall = 0 ;                          /* number of calls */
			
      } else {  /* the "end notification" */
			
         PLUTO_set_meter( global_plint , 100 ) ; /* set meter to 100% */

      }
      return ;
   }

	
	hit = 0;
	ud->ln = npts;
	
	
	if (ud->iloc != -1)			/* for each ncall, find out if this index is wanted*/
		{
			*dumb = ud->fail;
			for (i=0;i<ud->nrows;++i)
				{
					if (ud->indvect[i] == (float)ncall) 
						{
							hit = 1;
						   *dumb = ud->pass;
							writets (ud,ts,ncall); 
							i = ud->nrows;
						}
				}
		}
	else
		{
			*dumb = ud->fail;
			indexTOxyz (ud, ncall, &xpos , &ypos , &zpos);
			for (i=0;i<ud->nrows;++i)
				{
					if (ud->xyzvect[i].x == (float)xpos)
						{
							if (ud->xyzvect[i].y == (float)ypos)
								{
									if (ud->xyzvect[i].z == (float)zpos)
										{
											hit = 1;
											*dumb = ud->pass;
											writets (ud,ts,ncall); 
											i = ud->nrows;
										}
								}
						}		
				}
			
		}
	
	
	/* the output brick generated here is practically useless, it has 1 at the voxels 
	whos time courses were used and 0 where nothing was extracted */

	
   
   if (ud->errcode == 0) 				/* if there are no errors, proceed */	
		{/* 						*/
   	}/* ud->errcode == 0 outer loop */
   
   /** set the progress meter to the % of completion **/
   ncall++ ;
   
   PLUTO_set_meter( global_plint , (100*ncall)/nvox ) ;
   
   ud->errcode = 0;	/* Rest error to nothing */
   
   return ;
}

/* ************************************************************ */
/* function to display user data input (debugging stuff)        */
/* ************************************************************ */

static void show_ud (struct extract_data* ud)
	{
		printf ("\n\nUser Data Values at location :\n");
		printf ("ud->dsetname= %s\n",ud->dsetname);
		printf ("ud->strin= %s\n",ud->strin);
		printf ("ud->strout= %s\n",ud->strout);
		printf ("ud->nxx= %d\n",ud->nxx);
		printf ("ud->nyy= %d\n",ud->nyy);
		printf ("ud->nzz= %d\n",ud->nzz);
		printf ("ud->iloc= %d\n",ud->iloc);
		printf ("ud->xloc= %d\n",ud->xloc);
		printf ("ud->yloc= %d\n",ud->yloc);
		printf ("ud->zloc= %d\n",ud->zloc);
		printf ("ud->ncols= %d\n",ud->ncols);
		printf ("ud->nrows= %d\n",ud->nrows);
		printf ("ud->ignore= %d\n",ud->ignore);
		printf ("ud->dtrnd= %d\n",ud->dtrnd);
		printf ("ud->ln= %d\n",ud->ln);
		printf ("ud->new_prefix= %s\n",ud->new_prefix);
		printf ("ud->format= %d\n",ud->format);
		printf ("Hit enter to continue\a\n\n");
		getchar ();
		return;
	}

/* ************************************************************ */
/* function to write user data input to log file        */
/* ************************************************************ */

static void write_ud (struct extract_data* ud)
	{
		fprintf (ud->outlogfile,"\n\nUser Data Values \n");
		fprintf (ud->outlogfile,"ud->dsetname= %s\n",ud->dsetname);
		fprintf (ud->outlogfile,"ud->strin= %s\n",ud->strin);
		fprintf (ud->outlogfile,"ud->strout= %s\n",ud->strout);
		fprintf (ud->outlogfile,"ud->nxx= %d\n",ud->nxx);
		fprintf (ud->outlogfile,"ud->nyy= %d\n",ud->nyy);
		fprintf (ud->outlogfile,"ud->nzz= %d\n",ud->nzz);
		fprintf (ud->outlogfile,"ud->iloc= %d\n",ud->iloc);
		fprintf (ud->outlogfile,"ud->xloc= %d\n",ud->xloc);
		fprintf (ud->outlogfile,"ud->yloc= %d\n",ud->yloc);
		fprintf (ud->outlogfile,"ud->zloc= %d\n",ud->zloc);
		fprintf (ud->outlogfile,"ud->ncols= %d\n",ud->ncols);
		fprintf (ud->outlogfile,"ud->nrows= %d\n",ud->nrows);
		fprintf (ud->outlogfile,"ud->ignore= %d\n",ud->ignore);
		fprintf (ud->outlogfile,"ud->dtrnd= %d\n",ud->dtrnd);
		fprintf (ud->outlogfile,"ud->pass= %f\n",ud->pass);
		fprintf (ud->outlogfile,"ud->fail= %f\n",ud->fail);
		fprintf (ud->outlogfile,"ud->new_prefix= %s\n",ud->new_prefix);
		fprintf (ud->outlogfile,"ud->format= %d\n",ud->format);
		fprintf (ud->outlogfile,"\nThe format for the output file is the following:\n");
		switch (ud->format)
			{
				case (0):
					fprintf (ud->outlogfile,"ncall\txpos\typos\tzpos\tts[0]\tts[1]\t...\n");
					break;
				case (1):
					fprintf (ud->outlogfile,"ts[0]\tts[1]\t...\n");
					break;
					
			}
		
		return;
	}

/* ************************************************************ */ 
/* function to compute x, y, z coordinates from the index       */
/* ************************************************************ */ 

static void indexTOxyz (struct extract_data* ud, int ncall, int *xpos , int *ypos , int *zpos)  	
	{
		*zpos = (int)ncall / (int)(ud->nxx*ud->nyy);
		*ypos = (int)(ncall - *zpos * ud->nxx * ud->nyy) / (int)ud->nxx;
		*xpos = ncall - ( *ypos * ud->nxx ) - ( *zpos * ud->nxx * ud->nyy ) ;
		return;
	}
	
/* ************************************************************ */
/* function to report errors encountered to the logfile         */
/* Only errors that happen during runtime  are handeled here,   */
/* the others are handeled instantaneously, and need not be 	*/
/* logged 																		 */
/* ************************************************************ */

static void error_report (struct extract_data* ud, int ncall ) 
	{
		int xpos,ypos,zpos;
		
		indexTOxyz (ud, ncall, &xpos , &ypos , &zpos); 
		
		switch (ud->errcode)
			{
				
				default:
					fprintf (ud->outlogfile,"De Fault, De Fault (%d), the two sweetest words in the english langage ! ",ud->errcode);
					break;
			}	
		fprintf (ud->outlogfile,"%d\t%d\t%d\t%d\t%\n", ncall , xpos , ypos , zpos  );
		return;
	}
	
/* *************************************************************** */
/* function to write the time course into a line in the given file */
/* *************************************************************** */

static void writets (struct extract_data * ud,float * ts, int ncall)

	{	
		int i,xpos,ypos,zpos;
		
		switch (ud->format)
			{
				case (0):
					indexTOxyz (ud,ncall,&xpos , &ypos , &zpos); 
					fprintf (ud->outwritets, "%d\t%d\t%d\t%d\t",ncall,xpos, ypos,zpos);
					break;
				case (1):
					break;
				default:
					break;
			}
		
		for (i=0;i<ud->ln;++i)
			{
				fprintf (ud->outwritets, "%f\t",ts[i]);
			}
		fprintf (ud->outwritets,"\n");
	}

/* *************************************************************** */
/* function to extract x y z colums form a matrix format  file */
/* *************************************************************** */
static fXYZ * extract_xyz (char *fname, int x_col_loc, int y_col_loc, int z_col_loc, int ncols, int *nrows, int *Err)
{/*extract_xyz*/
	
	float tmp, *tmpX;
	int sz,i,indx,tst;
	div_t divstuff,tempx,tempy,tempz;
	FILE * INFILE;
	fXYZ * xyzvect=NULL;
	
	/* ncols must be > 0 */
	if (ncols <= 0)
		
		{
			*Err = 4;
			return (xyzvect); /* ncols <= 0 !*/
		}
		
		
	/* ind_col_loc must be > 0  (1st column is 1, and it must be less than ncols) */
	if (x_col_loc <= 0 || x_col_loc > ncols || y_col_loc <= 0 || y_col_loc > ncols || z_col_loc <= 0 || z_col_loc > ncols )
		{
			*Err = 1;
			return (xyzvect); /* ?_col_loc should be >0 and <ncols*/
		}
	
	/* if the number of rows is given, compute required size */
	if (*nrows > 0)
		{
			sz = *nrows * ncols;
		}
	else
		{ /* dtermine the size and compute the number of rows, check if it's an integer*/
			sz = f_file_size (fname);
			if (sz == -1)
           {
                   *Err = 3;
                   return (xyzvect);
            }
			divstuff = div (sz,ncols);
			if (divstuff.rem != 0)
				{
					*Err = 2;
					return (xyzvect); /* size of file and number of columns don't match */
				}
			else 
				{
					*nrows = divstuff.quot;
				}
		}
	
	tst = (x_col_loc - y_col_loc) * (x_col_loc - z_col_loc) * (y_col_loc - z_col_loc);
	if (tst == 0)
		{
			*Err = 5;
			return (xyzvect); /* columns specified are the same */
		} 
	
	/* Allocate and check for necessary space */
	xyzvect = (fXYZ *) calloc (sz+2,sizeof(fXYZ));
	
	 if (xyzvect == NULL)
				{
					printf ("\nFatal Error : Failed to Allocate memory\a\n");
					printf ("Abandon Lab Immediately !\n\n");
					return NULL;
				};
	
	INFILE = fopen (fname,"r");
	if (INFILE == NULL)
		{
			*Err = 3; /* can't find file */
			return (xyzvect);
		}

	tempx = div (x_col_loc,ncols);
	tempy = div (y_col_loc,ncols);
	tempz = div (z_col_loc,ncols);
	
	for (i=0;i<sz;++i)
		{
			fscanf (INFILE,"%f",&tmp);
			divstuff = div ((i+1),ncols);
			if (divstuff.rem != 0)
				indx = divstuff.quot;
			else 
				indx = divstuff.quot - 1;
			
			if (divstuff.rem == tempx.rem)
					xyzvect[indx].x = tmp;
				
				else if (divstuff.rem == tempy.rem)
						xyzvect[indx].y = tmp;
					
					else if (divstuff.rem == tempz.rem)
							xyzvect[indx].z = tmp;
		}

	
	*Err = 0;
	return (xyzvect);
	
}/*extract_xyz*/


/* *************************************************************** */
/* function to extract an indices columnform a matrix format   file */
/* *************************************************************** */


static float * extract_index (char *fname, int ind_col_loc, int ncols, int *nrows, int *Err)
{/*extract_index*/
	
	float tmp, *indxvect=NULL;
	int sz,i;
	div_t divstuff,temp;
	FILE * INFILE;
	
	/* ncols must be > 0 */
	if (ncols <= 0)
		
		{
			*Err = 4;
			return (indxvect); /* ncols <= 0 !*/
		}
		
	
	/* ind_col_loc must be > 0  (1st column is 1, and it must be less than ncols) */
	if (ind_col_loc <= 0 || ind_col_loc > ncols)
		{
			*Err = 1;
			return (indxvect); /* ind_col_loc should be >0 and <ncols*/
		}
	
	/* if the number of rows is given, compute required size */
	if (*nrows > 0)
		{
			sz = *nrows * ncols;
		}
	else
		{ /* dtermine the size and compute the number of rows, check if it's an integer*/
			sz = f_file_size (fname);
			if (sz == -1)
            {
               *Err = 3;
              return (indxvect);      
            }
			divstuff = div (sz,ncols);
			if (divstuff.rem != 0)
				{
					*Err = 2;
					return (indxvect); /* size of file and number of columns don't match */
				}
			else 
				{
					*nrows = divstuff.quot;
				}
		}
	
	/* Allocate and check for necessary space */
	indxvect = (float *) calloc (sz+2,sizeof(float));
	
	 if (indxvect == NULL)
				{
					printf ("\nFatal Error : Failed to Allocate memory\a\n");
					printf ("Abandon Lab Immediately !\n\n");
					return NULL ;
				};
	
	INFILE = fopen (fname,"r");
	if (INFILE == NULL)
		{
			*Err = 3; /* can't find file */
			return (indxvect);
		}

	temp = div (ind_col_loc,ncols);
	
	for (i=0;i<sz;++i)
		{
			fscanf (INFILE,"%f",&tmp);
			divstuff = div ((i+1),ncols);
			if (divstuff.rem == temp.rem)
				{
					if (divstuff.rem != 0) indxvect[divstuff.quot] = tmp;
						else indxvect[divstuff.quot-1] = tmp;
				}
		}

	
	*Err = 0;
	return (indxvect);
	
}/*extract_index*/

/* *************************************************************** */
/* function to return the size of a float numbers   file */
/* *************************************************************** */

static int f_file_size (char *f_name)
   
    { 
      

     int cnt=0,ex;
     float buf;
     
     FILE*internal_file;
     
     internal_file = fopen (f_name,"r");
     if (internal_file == NULL) {
     								return (-1);
     						   	}
     ex = fscanf (internal_file,"%f",&buf);					   	
     while (ex != EOF)
      {
        ++cnt;
        ex = fscanf (internal_file,"%f",&buf);
      }
      
      
      fclose (internal_file);
      return (cnt);  							     
   }
 
/* *************************************************************** */
/* function to test if a file exists 									    */
/* *************************************************************** */

static int filexists (char *f_name)
{/*filexists*/
        FILE *outfile;
        
        outfile = fopen (f_name,"r");
        if (outfile == NULL)
                return (0);
        else 
                fclose (outfile);
                return (1);
                
}/*filexists*/


/* *************************************************************** */
/* function to display a vector (debugging) 								 */
/* *************************************************************** */

static void disp_vect (float *v,int l)
        {
                int i;

                printf ("\n");
                if ((l-1) == 0)
                        {
                                printf ("V = %f\n",*v);
                        }
                else 
                {
                        for (i=0;i<l;++i)
                        {
                                printf ("V[%d] = %f\t",i,v[i]);
                        }
                        printf ("\n");
                }
                return;

        }
