/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/
   
#include "afni.h"
#include "afni_plugin.h"

#ifndef ALLOW_PLUGINS
#  error "Plugins not properly set up -- see machdep.h"
#endif

/* -----------------------START---------------------------------*/
/* definition and decleration part to suport the main algorithm */

#include <stdlib.h>
#include <stdio.h>
#include <math.h>


/*------------------ macros to return workspace at exit -------------------*/

#define ZFREEUP(x) do{if((x) != NULL){free((x)); (x)=NULL;}}while(0)

#undef  ZFREE_WORKSPACE
#define ZFREE_WORKSPACE                              \
  do{ ZFREEUP(bptr) ; ZFREEUP(sptr) ; ZFREEUP(fptr) ;  \
      ZFREEUP(cptr) ; ZFREEUP(fxar) ; ZFREEUP(fac)  ;  \
      ZFREEUP(dtr)  ;   \
    } while(0)
/*-------------------------------------------------------------------------*/


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
		  int ncols;
		  int nrows;
		  float * indvect;	/* vector that will hold the list of indices */
		  fXYZ * xyzvect;			/* vecor that will hold the list of xyz coordinates */
		  char * strout;
		  char * strin;
		  FILE * outwritets;
		  FILE * outlogfile;
		  char outname[PLUGIN_MAX_STRING_RANGE]; /* output data file name */
	};

/*--------------------- string to 'help' the user --------------------*/

static char helpstring[] =
  "                               4D Dump Plugin\n\n"
  "This plugin allows the extraction of selected FMRI time series into an ascii file. \n"
  "The time series to be extracted are specified either by their AFNI index or \n"
  "by their AFNI XYZ corrdinates.\n\n"
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
  "                    Masking files could be the ascii output files of other plugins such as\n" 
  "                   '3Ddump' and 'Hilber Delay98'.\n"
  "      N Columns  -> Total number of values on each line in the ascii mask file.\n\n"
  "   3- Index Mask : (optional)\n"
  "      i col.     -> Column number where the AFNI indices for the time series to be extracted \n"
  "                    are stored in the ascii mask file.\n"
  "   4- XYZ Mask : (optional)\n"
  "      x,y,z col. -> Specify the column numbers where X Y Z triplets are stored\n\n"
  "   5- Output :\n"
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
  "                          Ziad Saad Jan 6 97, lastest update Feb 23 98.\n\n"
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

static void EXTRACT_tsfunc() ;                      /* the timeseries routine */

static void show_ud (struct extract_data* ud);

static void write_ud (struct extract_data* ud);

static void indexTOxyz (struct extract_data* ud, int ncall, int *xpos , int *ypos , int *zpos);

static void error_report (struct extract_data* ud, int ncall );

static void writets (struct extract_data* ud,float * ts, int ncall);

static float * extract_index (char *fname, int ind_col_loc, int ncols, int *nrows, int *Err);

static fXYZ * extract_xyz (char *fname, int x_col_loc, int y_col_loc, int z_col_loc, int ncols, int *nrows, int *Err);

static void disp_vect (float *v,int l);

static int filexists (char *f_name);

static int f_file_size (char *f_name);

static int * PLUTO_4D_to_nothing (THD_3dim_dataset * old_dset , int ignore , int detrend ,
                         generic_func * user_func , void * user_data );

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

PLUGIN_interface * PLUGIN_init( int ncall )
{
   PLUGIN_interface * plint ;     /* will be the output of this routine */

   if( ncall > 0 ) return NULL ;  /* only one interface */

   /*---------------- set titles and call point ----------------*/

   plint = PLUTO_new_interface( "4D Dump" ,
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
   char *tmpstr , * str ;                 
   int   ntime, nvec , Err , itmp, nprfx;
	float * vec , fs , T ;
	char * tag;                     /* plugin option tag */	
	
	/* Allocate as much character space as Bob specifies in afni.h + a bit more */
	
	tmpstr = (char *) calloc (PLUGIN_MAX_STRING_RANGE+10,sizeof(char));
	
	if (tmpstr == NULL) 
									  return "********************\n"
												"Could not Allocate\n"
												"a teeni weeni bit of\n"
												"Memory ! \n"
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
   
   ud->dsetname = DSET_PREFIX (old_dset);
	
	ud->ignore = PLUTO_get_number(plint) ;    /* get number item */
	
	str = PLUTO_get_string(plint) ; 				
	ud->dtrnd = (int)PLUTO_string_index( str , NUM_YN_STRINGS , yn_strings );
	
	
	
	/*--------- loop over remaining options ---------*/
	
		
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
	
	/* check to see if the field is empty */
	if (ud->strout == NULL) nprfx = 0;
		else nprfx = 1;
	
	/* check if the size is larger than 0. I did not want to check for this unless it's allocated */
	if (nprfx == 1 && (int)strlen (ud->strout) == 0) nprfx = 0;
		
		
	if (nprfx == 0)   /* no output file is specified */ 
 		{
 			sprintf ( tmpstr , "%s.4Ddump" , DSET_PREFIX (old_dset));
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

	PLUTO_4D_to_nothing (old_dset , ud->ignore , ud->dtrnd ,
                        EXTRACT_tsfunc , (void *)ud );

	fclose (ud->outlogfile);
	fclose (ud->outwritets);
	
	if (tmpstr) free (tmpstr);		
   return NULL ;  /* null string returned means all was OK */
}

/**********************************************************************
   Function that does the real work
***********************************************************************/

static void EXTRACT_tsfunc( double T0 , double TR ,
                   int npts , float ts[] , double ts_mean , double ts_slope ,
                   void * udp)
{
   static int nvox = -1, ncall = -1;
	struct extract_data uda,*ud;
	float xcor=0.0 ,  tmp=0.0 , tmp2 = 0.0 ,  dtx = 0.0 ,\
			 slp = 0.0 , vts = 0.0 , vrvec = 0.0 , rxcorCoef = 0.0;
	int i , is_ts_null , status , opt , actv , zpos , ypos , xpos , hit;
	
	ud = &uda;
	ud = (struct extract_data *) udp;
	
	
   /** is this a "notification"? **/

	
   if( ncall == -1 || ncall == nvox)
   	{
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
			for (i=0;i<ud->nrows;++i)
				{
					if (ud->indvect[i] == (float)ncall) 
						{
							hit = 1;
							writets (ud,ts,ncall); 
							i = ud->nrows;
						}
				}
		}
	else
		{
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
											writets (ud,ts,ncall); 
											i = ud->nrows;
										}
								}
						}		
				}
			
		}
	
	
   if (ud->errcode == 0) 				/* if there are no errors, proceed */	
		{/* 						*/
   	}/* ud->errcode == 0 outer loop */
   
   /** set the progress meter to the % of completion **/
   ncall++ ;
   
   PLUTO_set_meter( global_plint , (100*ncall)/nvox ) ;
   
   ud->errcode = 0;	/* Reset error to nothing */
   
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

void error_report (struct extract_data* ud, int ncall ) 
	{
		int xpos,ypos,zpos;
		
		indexTOxyz (ud, ncall, &xpos , &ypos , &zpos); 
		
		switch (ud->errcode)
			{
				
				default:
					fprintf (ud->outlogfile,"De Fault, De Fault (%d), the two sweetest words in the english langage ! ",ud->errcode);
					break;
			}	
		fprintf (ud->outlogfile,"%d\t%d\t%d\t%d\t\n", ncall , xpos , ypos , zpos  );
		return;
	}
	
/* *************************************************************** */
/* function to write the time course into a line in the given file */
/* *************************************************************** */

void writets (struct extract_data * ud,float * ts, int ncall)

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
	fXYZ * xyzvect=NULL ;
	
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
					return NULL ;
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

/* *************************************************************** */
/* function to retrieve 4D data from AFNI                          */
/*    (adapted from BDW's function PLUTO_4D_to_typed_fith 			 */
/* *************************************************************** */

static int * PLUTO_4D_to_nothing (THD_3dim_dataset * old_dset , int ignore , int detrend ,
                         generic_func * user_func, void * user_data )
{

   byte    ** bptr = NULL ;  /* one of these will be the array of */
   short   ** sptr = NULL ;  /* pointers to input dataset sub-bricks */
   float   ** fptr = NULL ;  /* (depending on input datum type) */
   complex ** cptr = NULL ;

   float * fxar = NULL ;  /* array loaded from input dataset */
   float * fac  = NULL ;  /* array of brick scaling factors */
   float * dtr  = NULL ;  /* will be array of detrending coeff */

   float val , d0fac , d1fac , x0,x1;
   double tzero , tdelta , ts_mean , ts_slope ;
   int   ii , old_datum , nuse , use_fac , iz,izold, nxy,nvox ;
   static int retval;
	register int kk ;

   /*----------------------------------------------------------*/
   /*----- Check inputs to see if they are reasonable-ish -----*/

   if( ! ISVALID_3DIM_DATASET(old_dset) ) return NULL ;

   if( user_func == NULL ) return NULL ;

   if( ignore < 0 ) ignore = 0 ;

   /*--------- set up pointers to each sub-brick in the input dataset ---------*/

   old_datum = DSET_BRICK_TYPE( old_dset , 0 ) ;   /* get old dataset datum */
   nuse      = DSET_NUM_TIMES(old_dset) - ignore ; /* # of points on time axis */
   if( nuse < 2 ) return NULL ;

   DSET_load( old_dset ) ;  /* must be in memory before we get pointers to it */

   kk = THD_count_databricks( old_dset->dblk ) ;  /* check if it was */
   if( kk < DSET_NVALS(old_dset) ){               /* loaded correctly */
      DSET_unload( old_dset ) ;
      return NULL ;
   }

   switch( old_datum ){  /* pointer type depends on input datum type */

      default:                      /** don't know what to do **/
         DSET_unload( old_dset ) ;
         return NULL ;

      /** create array of pointers into old dataset sub-bricks **/

      /*--------- input is bytes ----------*/
      /* voxel #i at time #k is bptr[k][i] */
      /* for i=0..nvox-1 and k=0..nuse-1.  */

      case MRI_byte:
         bptr = (byte **) malloc( sizeof(byte *) * nuse ) ;
         if( bptr == NULL ) return NULL ;
         for( kk=0 ; kk < nuse ; kk++ )
            bptr[kk] = (byte *) DSET_ARRAY(old_dset,kk+ignore) ;
      break ;

      /*--------- input is shorts ---------*/
      /* voxel #i at time #k is sptr[k][i] */
      /* for i=0..nvox-1 and k=0..nuse-1.  */

      case MRI_short:
         sptr = (short **) malloc( sizeof(short *) * nuse ) ;
         if( sptr == NULL ) return NULL ;
         for( kk=0 ; kk < nuse ; kk++ )
            sptr[kk] = (short *) DSET_ARRAY(old_dset,kk+ignore) ;
      break ;

      /*--------- input is floats ---------*/
      /* voxel #i at time #k is fptr[k][i] */
      /* for i=0..nvox-1 and k=0..nuse-1.  */

      case MRI_float:
         fptr = (float **) malloc( sizeof(float *) * nuse ) ;
         if( fptr == NULL ) return NULL ;
         for( kk=0 ; kk < nuse ; kk++ )
            fptr[kk] = (float *) DSET_ARRAY(old_dset,kk+ignore) ;
      break ;

      /*--------- input is complex ---------*/
      /* voxel #i at time #k is cptr[k][i]  */
      /* for i=0..nvox-1 and k=0..nuse-1.   */

      case MRI_complex:
         cptr = (complex **) malloc( sizeof(complex *) * nuse ) ;
         if( cptr == NULL ) return NULL ;
         for( kk=0 ; kk < nuse ; kk++ )
            cptr[kk] = (complex *) DSET_ARRAY(old_dset,kk+ignore) ;
      break ;

   } /* end of switch on input type */

	nvox = old_dset->daxes->nxx * old_dset->daxes->nyy * old_dset->daxes->nzz ;

   
   /*---- allocate space for 1 voxel timeseries ----*/

   fxar = (float *) malloc( sizeof(float) * nuse ) ;   /* voxel timeseries */
   if( fxar == NULL ){ ZFREE_WORKSPACE ; return NULL ; }

   /*--- get scaling factors for sub-bricks ---*/

   fac = (float *) malloc( sizeof(float) * nuse ) ;   /* factors */
   if( fac == NULL ){ ZFREE_WORKSPACE ; return NULL ; }

   use_fac = 0 ;
   for( kk=0 ; kk < nuse ; kk++ ){
      fac[kk] = DSET_BRICK_FACTOR(old_dset,kk+ignore) ;
      if( fac[kk] != 0.0 ) use_fac++ ;
      else                 fac[kk] = 1.0 ;
   }
   if( !use_fac ) ZFREEUP(fac) ;

   /*--- setup for detrending ---*/

   dtr = (float *) malloc( sizeof(float) * nuse ) ;
   if( dtr == NULL ){ ZFREE_WORKSPACE ; return NULL ; }

   d0fac = 1.0 / nuse ;
   d1fac = 12.0 / nuse / (nuse*nuse - 1.0) ;
   for( kk=0 ; kk < nuse ; kk++ )
      dtr[kk] = kk - 0.5 * (nuse-1) ;  /* linear trend, orthogonal to 1 */


   /*----- set up to find time at each voxel -----*/

   tdelta = old_dset->taxis->ttdel ;
   if( DSET_TIMEUNITS(old_dset) == UNITS_MSEC_TYPE ) tdelta *= 0.001 ;
   if( tdelta == 0.0 ) tdelta = 1.0 ;

   izold  = -666 ;
   nxy    = old_dset->daxes->nxx * old_dset->daxes->nyy ;

   /*----------------------------------------------------*/
   /*----- Setup has ended.  Now do some real work. -----*/

   /* start notification */
   user_func(  0.0 , 0.0 , nvox , NULL,0.0,0.0 , user_data ) ;

   /***** loop over voxels *****/   
   for( ii=0 ; ii < nvox ; ii++  ){  /* 1 time series at a time */
		
      /*** load data from input dataset, depending on type ***/

      switch( old_datum ){

         /*** input = bytes ***/

         case MRI_byte:
            for( kk=0 ; kk < nuse ; kk++ ) fxar[kk] = bptr[kk][ii] ;
         break ;

         /*** input = shorts ***/

         case MRI_short:
            for( kk=0 ; kk < nuse ; kk++ ) fxar[kk] = sptr[kk][ii] ;
         break ;

         /*** input = floats ***/

         case MRI_float:
            for( kk=0 ; kk < nuse ; kk++ ) fxar[kk] = fptr[kk][ii] ;
         break ;

         /*** input = complex (note we use absolute value) ***/

         case MRI_complex:
            for( kk=0 ; kk < nuse ; kk++ ) fxar[kk] = CABS(cptr[kk][ii]) ;
         break ;

      } /* end of switch over input type */

      /*** scale? ***/
     if( use_fac )
         for( kk=0 ; kk < nuse ; kk++ ) fxar[kk] *= fac[kk] ;

      /** compute mean and slope **/

      x0 = x1 = 0.0 ;
      for( kk=0 ; kk < nuse ; kk++ ){
         x0 += fxar[kk] ; x1 += fxar[kk] * dtr[kk] ;
      }

      x0 *= d0fac ; x1 *= d1fac ;  /* factors to remove mean and trend */

      ts_mean  = x0 ;
      ts_slope = x1 / tdelta ;
 
      /** detrend? **/

      if( detrend )
         for( kk=0 ; kk < nuse ; kk++ ) fxar[kk] -= (x0 + x1 * dtr[kk]) ;

      /** compute start time of this timeseries **/
		/* The info computed here is not being used in this version*/
      iz = ii / nxy ;    /* which slice am I in? */

      if( iz != izold ){          /* in a new slice? */
         tzero = THD_timeof( ignore ,
                             old_dset->daxes->zzorg
                           + iz*old_dset->daxes->zzdel , old_dset->taxis ) ;
         izold = iz ;

         if( DSET_TIMEUNITS(old_dset) == UNITS_MSEC_TYPE ) tzero *= 0.001 ;
      }

      /*** Send data to user function ***/
      user_func( tzero,tdelta , nuse,fxar,ts_mean,ts_slope , user_data) ;

      

   } /* end of outer loop over 1 voxels at a time */

   DSET_unload( old_dset ) ;  

   /* end notification */
   user_func( 0.0 , 0.0 , 0 , NULL,0.0,0.0 , user_data ) ;

   
   /*-------------- Cleanup and go home ----------------*/
   
   ZFREE_WORKSPACE ;
	retval = 0;
	return &retval; /* this value is not used for now .... */

}

