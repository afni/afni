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

/***********************************************************************
  Plugin to extract 3D brick data 
************************************************************************/
typedef struct extract_data
	{
		  int nxx;			/* number of voxels in the x direction */
		  int nyy;			/* number of voxels in the y direction */
		  int nzz;			/* number of voxels in the z direction */
		  char *dsetname; /* prefix of data set analyzed */
		  int errcode;		/* error code number returned from function */
		  int out;			/* flag for writing to a file */
		  int format;
		  int iloc;
		  int xloc;
		  int yloc;
		  int zloc;
		  int fimonly;		/* set to 1 if no threshold is available */
		  int DoInt;
		  int DoThres;
		  int DoInd;
		  int intind; 
		  int	thrind;
		  int Nsub;
		  int isanat;
		  int isfunc;
		  float mini, maxi, minth, maxth;
		  char * strout;
		  FILE * outfile;
		  FILE * outlogfile;
		  char outname[PLUGIN_MAX_STRING_RANGE]; /* output data file name */
	};


static char helpstring[] =
  "                   3Ddump98 Plugin\n"
  "This plugin is used to write to an ascii file the data present in AFNI bricks.\n"
  "You can apply intenstity or threshold masks to the voxel data that is being extracted.\n\n" 
  "Plugin Inputs:\n\n"
  "   1- Dataset :\n"
  "      3D brick  -> 3D AFNI brick of the type :\n"
  "                    fim, fith, fico, fbuc, etc...\n"
  "                    spgr, epan.\n\n"
  "   2- SubBrick info : (Optional) \n"
  "      Intensity -> Index of the subbrick to be used\n"
  "                   as an intensity subbrick.\n"
  "      Threshold -> Index of the subbrick to be used \n"
  "                   as a threshold subbrick.\n" 
  "   While the subbrick indices are obvious when dealing with most bricks\n"
  "   You might need to specified them for bricks of the type bucket\n\n"
  "   3- Intensity Mask : (optional) \n"
  "      Minimum   -> Minimum boundary for intensity value (inclusive)\n"
  "      Maximum   -> Maximum boundary for intensity value (inclusive)\n"
  "   Data from voxels with intensity between Minimum and Maximum \n"
  "   are written to 'Filename'.\n\n"
  "   4- Threshold Mask : (optional) \n"
  "      Minimum   -> Minimum boundary for threshold value (inclusive)\n"
  "      Maximum   -> Maximum boundary for threshold value (inclusive)\n"
  "   Data from voxels with threshold value between Minimum and Maximum \n"
  "   are written to 'Filename'.\n\n"
  "   5- Output : \n"
  "      Filename  -> Name of ascii output file. \n"
  "                   If no name is specified, the default is\n"
  "                   the prefix of the inbut brick with the \n"
  "                   extension '.3Ddump' appended at the end.\n"
  "                   A LOG file, 'Filename.log' is also written to disk.\n"
  "                   The log file contains all the parameters settings used\n"
  "                   for generating 'Filename'.\n"
  "                   The format of 'Filename' is as follows :\n"
  "                   1- Voxel Index (VI) : Each voxel in an AFNI brick has a unique index.\n"
  "                                         Indices map directly to XYZ coordinates.\n"
  "                                         See AFNI plugin documentations for more info.\n"
  "                   2..4- Voxel coordinates (X Y Z) : Those are the voxel slice coordinates.\n"
  "                                                     You can see these coordinates in the upper left side\n"
  "                                                     of the AFNI window. To do so, you must first switch the\n"
  "                                                     voxel coordinate units from mm to slice coordinates.\n"
  "                                                     Define Datamode -> Misc -> Voxel Coords ?\n"
  "                                                     PS: The coords that show up in the graph window\n"
  "                                                     could be different from those in the upper left side \n"
  "                                                     of AFNI's main window.\n"
  "                   5..n- Subbrick values (Sb1 Sb2 ... Sbn) : Voxel values at each subbrick.\n\n"
  "If you have/find questions/comments/bugs about the plugin, \n"
  "send me an E-mail: ziad@image.bien.mu.edu\n\n"
  "                    Ziad Saad   Nov. 9 97, latest update Aug. 26 99.\n\n"
;

/* Significant update Aug. 26 99 */ 
/* The indexing into Storear has been swapped (columns became rows and rows columns.
That's because each subbrick was not stored in a vector on N elements, rather in N vectors
of 1 element each. That made the allocation process very slow, especially now that Bob has
malloc and calloc going through his own macros. */

/*-------- strings for output format  and some definitions -----------*/

static char * yn_strings[] = { "n" , "y" }; 

#define NUM_YN_STRINGS (sizeof(yn_strings)/sizeof(char *))

#define YUP  1
#define NOPE 0

#define ERROR_FILEWRITE		2
#define ERROR_OPTIONS		3

/*---------- prototypes for internal routines ----------*/
static int filexists (char *);

static char * DUMP_main( PLUGIN_interface * ) ;

static int Dumpit( struct extract_data* , THD_3dim_dataset * ) ;

static void write_ud (struct extract_data*);

static char **allocate2D (int rows,int cols,int element_size);

static void free2D(char **a,int rows);

static int equal_strings (char *s1,char *s2);


/***********************************************************************
   Set up the interface to the user
************************************************************************/

DEFINE_PLUGIN_PROTOTYPE

PLUGIN_interface * PLUGIN_init( int ncall )
{
   PLUGIN_interface * plint ;

   if( ncall > 0 ) return NULL ;  /* only one interface */

   /*-- set titles and call point --*/

   plint = PLUTO_new_interface( "3D Dump98" , "Ascii dump of 3D Dataset" , helpstring ,
                                 PLUGIN_CALL_VIA_MENU , DUMP_main  ) ;

   PLUTO_set_runlabels( plint , "Dump+Keep" , "Dump+Close" ) ;  /* 04 Nov 2003 */

   /*-- first line of input: Dataset --*/

   PLUTO_add_option( plint , "Dataset" , "Dataset" , TRUE ) ;
   PLUTO_add_dataset(plint , "3D brick" ,
                                    ANAT_ALL_MASK  ,  FUNC_ALL_MASK ,
                                    SESSION_ALL_MASK |
                                    DIMEN_3D_MASK    | BRICK_ALLREAL_MASK ) ;
   
	/*-- second line of input: intensity and threshold brick indices --*/
   
   
	PLUTO_add_option( plint ,
                     "SubBrik info" ,  /* label at left of input line */
                     "Index" ,  /* tag to return to plugin */
                     FALSE       /* is this mandatory? */
                   ) ;
	PLUTO_add_number( plint ,
                    "Intensity" ,  /* label next to chooser */
                    1 ,         /* smallest possible value */
                    10000 ,        /* largest possible value (inactivated for now)*/
                    0 ,         /* decimal shift (none in this case) */
                    0 ,         /* default value */
                    FALSE       /* allow user to edit value? */
                  ) ;
				
	PLUTO_add_number( plint ,
                    "Threshold" ,  /* label next to chooser */
                    1 ,         /* smallest possible value */
                    10000 ,        /* largest possible value (inactivated for now)*/
                    0 ,         /* decimal shift (none in this case) */
                    2 ,         /* default value */
                    FALSE       /* allow user to edit value? */
                  ) ;	
	/*-- Third line of input: intensity mask --*/
   
   PLUTO_add_option( plint ,
                     "Intensity Mask" ,  /* label at left of input line */
                     "Intensity" ,  /* tag to return to plugin */
                     FALSE       /* is this mandatory? */
                   ) ;
   
   PLUTO_add_number( plint ,
                    "Minimum" ,  /* label next to chooser */
                    -100000 ,         /* smallest possible value */
                    100000 ,        /* largest possible value (inactivated for now)*/
                    0 ,         /* decimal shift (none in this case) */
                    0 ,         /* default value */
                    TRUE       /* allow user to edit value? */
                  ) ;
                  
	PLUTO_add_number( plint ,
                    "Maximum" ,  /* label next to chooser */
                    -10000 ,         /* smallest possible value */
                    10000 ,        /* largest possible value (inactivated for now)*/
                    0 ,         /* decimal shift (none in this case) */
                    0 ,         /* default value */
                    TRUE       /* allow user to edit value? */
                  ) ;
   
   /*-- Fourth line of input: threshold mask --*/
   
   PLUTO_add_option( plint ,
                     "Threshold Mask" ,  /* label at left of input line */
                     "Threshold" ,  /* tag to return to plugin */
                     FALSE       /* is this mandatory? */
                   ) ;
   
   PLUTO_add_number( plint ,
                    "Minimum" ,  /* label next to chooser */
                    -10000 ,         /* smallest possible value */
                    10000 ,        /* largest possible value (inactivated for now)*/
                    0 ,         /* decimal shift (none in this case) */
                    0.5 ,         /* default value */
                    TRUE       /* allow user to edit value? */
                  ) ;
                  
	PLUTO_add_number( plint ,
                    "Maximum" ,  /* label next to chooser */
                    -10000 ,         /* smallest possible value */
                    10000 ,        /* largest possible value (inactivated for now)*/
                    0 ,         /* decimal shift (none in this case) */
                    1 ,         /* default value */
                    TRUE       /* allow user to edit value? */
                  ) ;
   
   /*---------- 5th line: output stuff ----------*/

   PLUTO_add_option( plint ,
                     "Output" ,  /* label at left of input line */
                     "Output" ,  /* tag to return to plugin */
                     TRUE        /* is this mandatory? */
                   ) ;

   PLUTO_add_string( plint ,
                     "Filename" ,  /* label next to textfield */
                     0,NULL ,    /* no fixed strings to choose among */
                     19          /* 19 spaces for typing in value */
                   ) ;
                  
   return plint ;
}

/***************************************************************************
  Main routine for this plugin (will be called from AFNI).
****************************************************************************/

static char * DUMP_main( PLUGIN_interface * plint )
{
   struct extract_data uda,*ud;
   MCW_idcode * idc ;
   THD_3dim_dataset * xset , * yset ;
   char * tag ;
   int demean ,ndmp,nprf;
   char *str, *nprfxstr, *mssg;
   float minx , maxx , minthr , maxthr ;
	
	str = (char *) calloc (PLUGIN_MAX_STRING_RANGE+10,sizeof(char));
	nprfxstr	 = (char *) calloc (PLUGIN_MAX_STRING_RANGE+20,sizeof(char));
	/* Do not allocate more space for mssg, because AFNI would choke on it*/
	mssg = (char *) calloc (PLUGIN_MAX_STRING_RANGE,sizeof(char));
	
	if (str == NULL || nprfxstr == NULL || mssg == NULL ) 
									  return "********************\n"
												"Could not Allocate\n"
												"a teeni weeni bit of\n"
												"Memory ! \n"
												"********************\n";

	ud = &uda;		/* ud now points to an allocated space */
   
   /*--------------------------------------------------------------------*/
   /*----- Check inputs from AFNI to see if they are reasonable-ish -----*/
	tag = PLUTO_get_optiontag(plint) ;
   
   if (tag == NULL)
   	{
   		return "************************\n"
             "Bad 1st line option \n"
             "************************"  ;
   	}	
   	
   
   idc  = PLUTO_get_idcode(plint) ; 	/* get 1st dataset item */
   xset = PLUTO_find_dset(idc) ;                   /* get ptr to dataset */
   if( xset == NULL )
      return "**********************\n"
             "Cannot find Dataset #1\n"
             "**********************"  ;
	
	ud->dsetname = DSET_FILECODE (xset);
	ud->Nsub = DSET_NVALS (xset);
	
	/*--------- loop over ramining options ---------*/
	ud->DoInt = NOPE;
	ud->DoThres = NOPE;
	ud->DoInd = NOPE;
	
	ud->intind = 1; /* The first subbrick is numbered 1 here, it makes more sense to the user.*/
	ud->thrind = 2; 
	
	do
		{
			tag = PLUTO_get_optiontag(plint) ;
			if (tag == NULL) break;
			
			if (equal_strings (tag, "Index") == 1)
				{
					ud->DoInd = YUP;
					ud->intind = PLUTO_get_number(plint) ; 
					ud->thrind = PLUTO_get_number(plint) ;
					continue; 
				}
			
			if (equal_strings (tag, "Intensity") == 1)
				{
					ud->DoInt = YUP;
					ud->mini = PLUTO_get_number(plint) ; 
					ud->maxi = PLUTO_get_number(plint) ;
					continue;
				}
			
			if (equal_strings (tag, "Threshold") == 1)
				{
					ud->DoThres = YUP;
					ud->minth = PLUTO_get_number(plint) ; 
					ud->maxth = PLUTO_get_number(plint) ;
					continue;
				}
			
			if (equal_strings (tag, "Output") == 1)
					{
						ud->strout = PLUTO_get_string(plint) ; 
						continue;
					}
			
			
		} while (1);
		
	if ( ISFUNC(xset) ) ud->isfunc = YUP;
		else ud->isfunc = NOPE;
	
	if ( ISANAT(xset) ) ud->isanat = YUP;
		else ud->isanat = NOPE;
	
	if (xset->func_type == FUNC_FIM_TYPE)
			ud->fimonly = 1;
		else
			ud->fimonly = 0;
	
	if (ud->isanat && (ud->DoThres== YUP || ud->DoInd == YUP))
		{
			return "*************************************\n"
                "Can't use threshold or index options \n" 
                "for fim or ANAT type bricks !\n"
                "*************************************"  ;
		}
	

	if (ud->DoInd == YUP && (ud->fimonly == YUP && ud->isfunc == YUP))
		{
			return "*******************************\n"
                "Can't specify Indices for fim\n" 
                "type bricks, they only have one !\n"
                "*******************************"  ;
		}
			

	/* Check for plausibility of input parameters */
	if ((ud->DoInt && (ud->maxi < ud->mini)) || (ud->DoThres && (ud->maxth < ud->minth)))
		{
		return "**********************\n"
             "Something's wrong with\n" 
             "min and max  values.\n"
             "**********************"  ;
		}
	if (ud->DoInd && (ud->intind > ud->Nsub || ud->thrind > ud->Nsub))
		{
		
		return "**********************\n"
             "One or both of the indices\n" 
             "is larger than the maximum\n"
				 "number of sub-bricks\n"
             "**********************"  ;
		}

	/*------------------------------------------------------*/
   /*----- Open the output file for writing operation -----*/
   
	/*if strout is null or of length 0, use the a default name */
	if (ud->strout == NULL)
   	nprf = 0;
   else
   	nprf = 1;
   	
	
   if (nprf == 1 && (int)strlen(ud->strout) == 0)
   	nprf = 0;
		
	
   if (nprf == 0)
   	{
			sprintf (nprfxstr,"%s.3Ddump",DSET_PREFIX(xset));
			ud->strout = nprfxstr;
   	}

   
	sprintf (str,"%s.log",ud->strout);	
		
   if ((filexists(ud->strout) == 1) || (filexists(str) == 1))
   	{
   		return "**************************************\n"
   		       "Output file(s) exists, can't overwrite\n"
   		       "**************************************\n";
   	}
   	
	ud->outfile = fopen (ud->strout,"w");
	ud->outlogfile = fopen (str,"w");
	
	if ((ud->outfile == NULL) || (ud->outlogfile == NULL))
		{
			return "*****************************************\n"
   		       "Could not open Output file(s) for writing\n"
   		       "Check permissions.\n"
   		       "*****************************************\n";
		}
	
   /*------------------------------------------------------*/
   /*---------- At this point, the inputs are OK ----------*/

   /*-- do the actual work --*/
   ndmp = Dumpit( ud ,  xset ) ;

   if( ndmp < -1.0 )
      {
			switch (ndmp)
				{
					case -2:
						return 	"********************************************\n"
             					"Fatal Error: Could not allocate memory\n"
									"(this is a message brought to you by Dumpit)\n"
             					"********************************************\n"  ;
						break;
					default:
						return 	"*********************************\n"
             					"Error while dumping data\n"
             					"*********************************"  ;

						break;
			
				}
		}

   /*-- put the output to the screen --*/

   /* That output message was too long. AFNI was crashing, must ask Bob to allow more verbose messages */ 
	 
   /*sprintf(mssg , "            Dataset %s was dumped.\n"
                 "%d voxels (%5f %% of total) met the boundary conditions.\n"
                  , DSET_FILECODE(xset) , ndmp , (float)ndmp/(float)(ud->nxx * ud->nyy * ud-> nzz)*100.0) ;*/
						
   /* That's shorter */
	sprintf(mssg , "%d voxels (%5f %% of total) were dumped.\n" 
	                 , ndmp , (float)ndmp/(float)(ud->nxx * ud->nyy * ud-> nzz)*100.0) ;

	PLUTO_popup_message( plint , mssg ) ;
	
	
	
	fclose (ud->outfile);
	fclose (ud->outlogfile);
	free (nprfxstr);	
	free (str);
	free (mssg); 
	 
   return NULL ;  /* null string returned means all was OK */
}


static int Dumpit( struct extract_data* ud, THD_3dim_dataset * xset)
{
   void  *  xar  ;
   void  * thar ;
   float * fxar  ;
	float ** Storear;
   int ii , jj, nxyz , fxar_new = 0  ,xpos,ypos,zpos , ndmp, pass;

	ndmp = -1;
	
   /*-- get datasets sizes --*/

	ud->nxx = xset->daxes->nxx;
	ud->nyy = xset->daxes->nyy;
	ud->nzz = xset->daxes->nzz;
	
   nxyz = xset->daxes->nxx * xset->daxes->nyy * xset->daxes->nzz ;

   /* Allocate space for data storage */
	fxar = (float *) malloc( sizeof(float) * nxyz ) ; fxar_new = 1 ;
   Storear = (float **) allocate2D (ud->Nsub ,nxyz ,sizeof(float)); /* changed from :  allocate2D (nxyz  ,ud->Nsub,sizeof(float)) */
	
	if (fxar == NULL || Storear == NULL)
		{
			return -2;
		}
	
	/*-- load the first dataset into memory --*/

		DSET_load( xset ) ;
		
	/* Store Bricks in 2D array */	
   for (ii = 0;ii < ud->Nsub; ++ii)
		{
   		xar   = DSET_ARRAY(xset,ii) ;        /* get the array */
   		EDIT_coerce_scale_type (nxyz,DSET_BRICK_FACTOR(xset,ii),
   							DSET_BRICK_TYPE(xset,ii), xar,	MRI_float,fxar ) ;   
			
			/* Store the iith sub-brick in Storear array */
			for (jj = 0; jj < nxyz; ++jj)
					Storear[ii][jj] = fxar[jj]; /* changed from : Storear[jj][ii] */
		}
		
   DSET_unload( xset ) ;  /* don't need this in memory anymore */
   	
	/* Dump the input info data to the log file */
	write_ud (ud);
	

   /* Now dump the data that meets the threshold */

   if( 1 ){
      for( ii=0 ; ii < nxyz ; ii++ ){
      	pass = YUP;
      	if (pass && ud->DoInt)
      		{
      			if (Storear[ud->intind-1][ii] < ud->mini || Storear[ud->intind-1][ii] > ud->maxi) /* changed both from : Storear[ii][ud->intind-1]*/
      				pass = NOPE;
      		}
      	
      	if (pass && ud->DoThres)   
      		{
      			if (Storear[ud->thrind-1][ii] < ud->minth || Storear[ud->thrind-1][ii] > ud->maxth)/* changed both from : Storear[ii][ud->intind-1]*/
      				pass = NOPE;
      		}
      	
      	if (pass)
      	{
      		zpos = (int)ii / (int)(xset->daxes->nxx * xset->daxes->nyy);
				ypos = (int)(ii - zpos * xset->daxes->nxx * xset->daxes->nyy) / xset->daxes->nxx;
				xpos = ii - ( ypos * xset->daxes->nxx ) - ( zpos * xset->daxes->nxx * xset->daxes->nyy ) ;
         	
         	fprintf (ud->outfile,"%d\t%d\t%d\t%d\t",ii,xpos,ypos,zpos);
         		
				for (jj = 0; jj < ud->Nsub; ++jj)
					fprintf (ud->outfile," %f\t",Storear[jj][ii]); /* changed from: Storear[ii][jj] */
					
				fprintf (ud->outfile,"\n");
					    	
				++ndmp;
         }
        }
		
		/* increment by one because I want it to start at 0 */
     	++ndmp;
   }
	
	
	fprintf (ud->outlogfile,"\n%d voxel points met the threshold conditions\n",ndmp);
	
   /*-- free up arrays --*/

	
   if( fxar_new ) 
		{
		free(fxar) ;
		}
	 else 
	 	{
			DSET_unload(xset) ;
		}
	
	
	free2D ((char **)Storear,ud->Nsub);	/* changed from free2D ((char **)Storear,nxyz); */

	
   return ndmp ;
}


/* ************************************************************ */ 
/* function to check for file existence       */
/* ************************************************************ */ 
	
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

/* ************************************************************ */ 
/* function to create log file       */
/* ************************************************************ */ 

void write_ud (struct extract_data* ud)
	{
		fprintf (ud->outlogfile,"\n\nUser Data Values \n");
		fprintf (ud->outlogfile,"Input data set file name= %s\n",ud->dsetname);
		fprintf (ud->outlogfile,"Is the file fim only type ? = %d\n",ud->fimonly);
		fprintf (ud->outlogfile,"Number of Subbricks : %d\n",ud->Nsub);
		fprintf (ud->outlogfile,"output file name = %s\n",ud->strout);
		fprintf (ud->outlogfile,"Number of voxels in X direction = %d\n",ud->nxx);
		fprintf (ud->outlogfile,"Number of voxels in Y direction = %d\n",ud->nyy);
		fprintf (ud->outlogfile,"Number of voxels in Z direction = %d\n",ud->nzz);
		fprintf (ud->outlogfile,"Do intensity mask ? = %d\n",ud->DoInt);
		fprintf (ud->outlogfile,"Minimum intensity = %f\n",ud->mini);
		fprintf (ud->outlogfile,"Maximum intensity = %f\n",ud->maxi);
		fprintf (ud->outlogfile,"Do threshold mask ? = %d\n",ud->DoThres);
		fprintf (ud->outlogfile,"Minimum threshold = %f\n",ud->minth);
		fprintf (ud->outlogfile,"Maximum threshold = %f\n",ud->maxth);
		fprintf (ud->outlogfile,"Select Intensity and Threshold indices = %d\n",ud->DoInd);
		fprintf (ud->outlogfile,"Intensity index = %d\n",ud->intind);
		fprintf (ud->outlogfile,"Threshold index = %d\n",ud->thrind);
		fprintf (ud->outlogfile,"\nThe format for the output file is the following:\n");
	   fprintf (ud->outlogfile,"VI\tX\tY\tZ\tSb1\tSb2\t... Sbn\n\n");
		
		
		return;
	}

/* ************************************************************ */ 
/* function to allocate 2D arrays       */
/* ************************************************************ */ 

static char **allocate2D (int rows,int cols,int element_size)

{
    int i;
    char **A;

/* try to allocate the request */
    switch(element_size) {
        case sizeof(short): {    /* integer matrix */
            short **int_matrix;
            int_matrix = (short **)calloc(rows,sizeof(short *));
            if(!int_matrix) {
                printf("\nError making pointers in %dx%d int matrix\n"
                            ,rows,cols);
                exit(1);
            }
            for(i = 0 ; i < rows ; i++) {
                int_matrix[i] = (short *)calloc(cols,sizeof(short));
                if(!int_matrix[i]) {
                    printf("\nError making row %d in %dx%d int matrix\n"
                            ,i,rows,cols);
                    exit(1);
                }
            }
            A = (char **)int_matrix;
            break;
        }
        case sizeof(float): {    /* float matrix */
            float **float_matrix;
            float_matrix = (float **)calloc(rows,sizeof(float *));
            if(!float_matrix) {
                printf("\nError making pointers in %dx%d float matrix\n"
                            ,rows,cols);
                exit(1);
            }
            for(i = 0 ; i < rows ; i++) {
                float_matrix[i] = (float *)calloc(cols,sizeof(float));
                if(!float_matrix[i]) {
                    printf("\nError making row %d in %dx%d float matrix\n"
                            ,i,rows,cols);
                    exit(1);
                }
            }
            A = (char **)float_matrix;
            break;
        }
        case sizeof(double): {   /* double matrix */
            double **double_matrix;
            double_matrix = (double **)calloc(rows,sizeof(double *));
            if(!double_matrix) {
                printf("\nError making pointers in %dx%d double matrix\n"
                            ,rows,cols);
                exit(1);
            }
            for(i = 0 ; i < rows ; i++) {
                double_matrix[i] = (double *)calloc(cols,sizeof(double));
                if(!double_matrix[i]) {
                    printf("\nError making row %d in %dx%d double matrix\n"
                            ,i,rows,cols);
                    exit(1);
                }
            }
            A = (char **)double_matrix;
            break;
        }
        default:
            printf("\nERROR in matrix_allocate: unsupported type\n");
            exit(1);
    }
    return(A);
}

/* ************************************************************ */ 
/* function to free 2D arrays       */
/* ************************************************************ */ 
static void free2D(char **a,int rows)
    
{
    int i;
    
/* free each row of data */
    for(i = 0 ; i < rows ; i++) free(a[i]);

/* free each row pointer */
    free((char *)a);
    a = NULL;           /* set to null for error */
    
	return;
}

/* ************************************************************ */ 
/* function to Check if strings are equal       */
/* ************************************************************ */ 


int equal_strings (char *s1,char *s2)

 {
   int i=0;
   
   if (s1 == NULL && s2 == NULL) return (-2);
   
   if ((s1 == NULL && s2 != NULL) || (s1 != NULL && s2 == NULL)) return (-1);
   
   while (s1[i] == s2[i] 
   			&& s1[i] != '\0' && s2[i] != '\0') ++i;
   			
   	if (s1[i] == '\0' && s2[i] == '\0') return (1);
   	 else return (0);
 
 }
