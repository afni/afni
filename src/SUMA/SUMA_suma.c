#define DEBUG_1
#ifdef DEBUG_1
	#define DEBUG_2
	#define DEBUG_3
#endif
   
/* Header FILES */
   
#include "SUMA_suma.h"

/* CODE */


SUMA_SurfaceViewer *SUMAg_cSV; /*!< Global pointer to current Surface Viewer structure*/
SUMA_SurfaceViewer *SUMAg_SVv; /*!< Global pointer to the vector containing the various Surface Viewer Structures */
int SUMAg_N_SVv = 0; /*!< Number of SVs stored in SVv */
SUMA_DO *SUMAg_DOv;	/*!< Global pointer to Displayable Object structure vector*/
int SUMAg_N_DOv = 0; /*!< Number of DOs stored in DOv */
SUMA_CommonFields *SUMAg_CF; /*!< Global pointer to structure containing info common to all viewers */

void SUMA_usage ()
   
  {/*Usage*/
          printf ("\n\33[1mUsage: \33[0m SUMA \n\t-spec <Spec file> [-sv <SurfVol>] [-ah AfniHost]\n\n");
			 printf ("\n\t-spec <Spec file>: File containing surface specification. This file is typically \n");
			 printf ("\n\t                   generated by @SUMA_Make_Spec_FS (for FreeSurfer surfaces) or \n");
			 printf ("\n\t                   @SUMA_Make_Spec_SF (for SureFit surfaces). The Spec file should \n");
			 printf ("\n\t                   be located in the directory containing the surfaces.\n");
			 printf ("\n\t[-sv <SurfVol>]: Anatomical volume used in creating the surface and registerd to \n");
			 printf ("\n\t                 the current experiment's anatomical volume (using @SUMA_AlignToExperiment).\n");
			 printf ("\n\t                 This parameter is optional but linking to AFNI is impossible without it.\n");
			 printf ("\n\t[-ah <AfniHost>]: Name (or IP address) of the computer running AFNI. This parameter is \n");
			 printf ("\n\t                  optional, the default is localhost. \n");
          printf ("\n\t                  When both AFNI and SUMA are on the same computer, you can turn shared \n");
          printf ("\n\t                  memory communication off by explicitly setting AfniHost to 127.0.0.1\n");
   		 /*
			 printf ("\n\t[-dev]: This option will give access to options that are not well polished for consuption.\n");
			 printf ("\n\t        \n");
			 printf ("\n\t[-iodbg] This option will trun on the In/Out debug info from the getgo.\n");
			 printf ("\n\t[-memdbg] This option will trun on the memory tracing from the getgo.\n");
          printf ("\n\t[-visuals] Shows the available glxvisuals and exits.\n");
          */
			 /*SUMA_VolSurf_help(NULL); OBSOLETE */
			 printf ("\n\n\tFor help on interacting with SUMA, press 'ctrl+h' with the mouse pointer inside SUMA's window.\n");
			 printf ("\n\n\tFor more help: http://afni.nimh.nih.gov/ssc/ziad/SUMA/SUMA_doc.htm\n");
			 printf ("\n\n\tIf you can't get help here, please get help somewhere.\n");
			 SUMA_Version(NULL);
			 
          printf ("\n\t Ziad S. Saad SSCC/NIMH/NIH ziad@nih.gov \n\n");
          exit (0);
  }/*Usage*/
     




/*!\**
File : SUMA.c
\author : Ziad Saad
Date : Thu Dec 27 16:21:01 EST 2001
   
Purpose : 
   
   
   
Input paramters : 
\param   
\param   
   
Usage : 
		SUMA ( )
   
   
Returns : 
\return   
\return   
   
Support : 
\sa   OpenGL prog. Guide 3rd edition
\sa   varray.c from book's sample code
   
Side effects : 
   
   
   
***/
int main (int argc,char *argv[])
{/* Main */
   static char FuncName[]={"SUMA"}; 
	int kar;
	SUMA_SFname *SF_name;
	SUMA_Boolean brk, SurfIn;
	char *VolParName, *NameParam, *specfilename = NULL, *AfniHostName;
   char *homeenv=NULL, *sumarc=NULL;
	SUMA_SurfSpecFile Spec;   
	SUMA_Axis *EyeAxis; 	
   SUMA_EngineData *ED= NULL;
   DList *list = NULL;
   DListElmt *Element= NULL;
   int iv15[15], N_iv15;
   struct stat stbuf;
   SUMA_Boolean LocalHead = NOPE;
   
    
   if (argc < 2)
       {
          SUMA_usage ();
          exit (1);
       }
		
	/* allocate space for CommonFields structure */
	if (LocalHead) fprintf (SUMA_STDERR,"%s: Calling SUMA_Create_CommonFields ...\n", FuncName);
   
   /* load the environment variables */
   homeenv = getenv("HOME");
   if (!homeenv) sumarc = SUMA_copy_string(".sumarc");
   else sumarc = SUMA_append_string (homeenv, "/.sumarc");
   
   if (stat(sumarc, &stbuf) != -1) {
      if (LocalHead) fprintf (SUMA_STDERR,"%s: Loading %s ...\n", FuncName, sumarc);
      AFNI_process_environ(sumarc); 
   } else {
      if (LocalHead) fprintf (SUMA_STDERR,"%s: No rc files found.\n", FuncName);
   }
   if (sumarc) free(sumarc);
   
   SUMAg_CF = SUMA_Create_CommonFields ();
	if (SUMAg_CF == NULL) {
		fprintf(SUMA_STDERR,"Error %s: Failed in SUMA_Create_CommonFields\n", FuncName);
		exit(1);
	}
   if (LocalHead) fprintf (SUMA_STDERR,"%s: SUMA_Create_CommonFields Done.\n", FuncName);
	
   /* initialize Volume Parent and AfniHostName to nothing */
	VolParName = NULL;
	AfniHostName = NULL; 
	
      
	/* Allocate space for DO structure */
	SUMAg_DOv = SUMA_Alloc_DisplayObject_Struct (SUMA_MAX_DISPLAYABLE_OBJECTS);
	   
	/* Work the options */
	kar = 1;
	brk = NOPE;
	SurfIn = NOPE;
	while (kar < argc) { /* loop accross command ine options */
		/*fprintf(stdout, "%s verbose: Parsing command line...\n", FuncName);*/
		
      if (strcmp(argv[kar], "-h") == 0 || strcmp(argv[kar], "-help") == 0) {
			SUMA_usage ();
          exit (1);
		}
		
      if (strcmp(argv[kar], "-visuals") == 0) {
			 SUMA_ShowAllVisuals ();
          exit (0);
		}
      
		if (!brk && (strcmp(argv[kar], "-iodbg") == 0)) {
			fprintf(SUMA_STDOUT,"Warning %s: SUMA running in in/out debug mode.\n", FuncName);
			SUMAg_CF->InOut_Notify = YUP;
			brk = YUP;
		}
      
		#if SUMA_MEMTRACE_FLAG
         if (!brk && (strcmp(argv[kar], "-memdbg") == 0)) {
			   fprintf(SUMA_STDOUT,"Warning %s: SUMA running in memory trace mode.\n", FuncName);
			   SUMAg_CF->MemTrace = YUP;
			   brk = YUP;
		   }
      #endif
      
      if (!brk && (strcmp(argv[kar], "-dev") == 0)) {
			fprintf(SUMA_STDOUT,"Warning %s: SUMA running in developer mode, some options may malfunction.\n", FuncName);
			SUMAg_CF->Dev = YUP;
			brk = YUP;
		}
		
		if (!brk && (strcmp(argv[kar], "-vp") == 0 || strcmp(argv[kar], "-sa") == 0 || strcmp(argv[kar], "-sv") == 0))
		{
			kar ++;
			if (kar >= argc)  {
		  		fprintf (SUMA_STDERR, "need argument after -vp|-sa|-sv ");
				exit (1);
			}
			VolParName = argv[kar];
			/*fprintf(SUMA_STDOUT, "Found: %s\n", VolParName);*/

			brk = YUP;
		}		
		
		if (!brk && strcmp(argv[kar], "-ah") == 0)
		{
			kar ++;
			if (kar >= argc)  {
		  		fprintf (SUMA_STDERR, "need argument after -ah ");
				exit (1);
			}
			AfniHostName = argv[kar];
			/*fprintf(SUMA_STDOUT, "Found: %s\n", AfniHostName);*/

			brk = YUP;
		}	
		if (!brk && strcmp(argv[kar], "-spec") == 0)
		{ 
			kar ++;
		  if (kar >= argc)  {
		  		fprintf (SUMA_STDERR, "need argument after -spec ");
				exit (1);
			}
			
			specfilename = argv[kar];
			/*fprintf(SUMA_STDOUT, "Found: %s\n", specfilename);*/
			brk = YUP;
		} 
		

		if (!brk) {
			fprintf (SUMA_STDERR,"Error %s: Option %s not understood. Try -help for usage\n", FuncName, argv[kar]);
			exit (1);
		} else {	
			brk = NOPE;
			kar ++;
		}
		
	}/* loop accross command ine options */

	if (specfilename == NULL) {
		fprintf (SUMA_STDERR,"Error %s: No spec filename specified.\n", FuncName);
		exit(1);
	}

	if(!SUMA_Assign_AfniHostName (SUMAg_CF, AfniHostName)) {
		fprintf (SUMA_STDERR, "Error %s: Failed in SUMA_Assign_AfniHostName", FuncName);
		exit (1);
	}
	
	/* create an Eye Axis DO */
	EyeAxis = SUMA_Alloc_Axis ("Eye Axis");
	if (EyeAxis == NULL) {
		SUMA_error_message (FuncName,"Error Creating Eye Axis",1);
		exit(1);
	}

	/* Store it into SUMAg_DOv */
	if (!SUMA_AddDO(SUMAg_DOv, &SUMAg_N_DOv, (void *)EyeAxis,  AO_type, SUMA_SCREEN)) {
		SUMA_error_message (FuncName,"Error Adding DO", 1);
		exit(1);
	}
	/*fprintf (SUMA_STDERR, "SUMAg_N_DOv = %d created\n", SUMAg_N_DOv);*/


	/* Allocate space (and initialize) Surface Viewer Structure */
	SUMAg_SVv = SUMA_Alloc_SurfaceViewer_Struct (SUMA_MAX_SURF_VIEWERS);
   
	/* Check on initialization */
	/*SUMA_Show_SurfaceViewer_Struct (SUMAg_cSV, stdout);*/

	/* Create the Surface Viewer Window */
	if (!SUMA_X_SurfaceViewer_Create ()) {
		fprintf(stderr,"Error in SUMA_X_SurfaceViewer_Create. Exiting\n");
		return 1;
	}
   
	#if 1
   if (!list) list = SUMA_CreateList();
   ED = SUMA_InitializeEngineListData (SE_Load_Group);
   if (!( Element = SUMA_RegisterEngineListCommand (  list, ED, 
                                          SEF_cp, (void *)specfilename, 
                                          SES_Suma, NULL, NOPE, 
                                          SEI_Head, NULL ))) {
      fprintf(SUMA_STDERR,"Error %s: Failed to register command\n", FuncName);
      exit (1);
   }
   if (!( Element = SUMA_RegisterEngineListCommand (  list, ED, 
                                          SEF_vp, (void *)VolParName, 
                                          SES_Suma, NULL, NOPE, 
                                          SEI_In, Element ))) {
      fprintf(SUMA_STDERR,"Error %s: Failed to register command\n", FuncName);
      exit (1);
   }
   
   N_iv15 = SUMA_MAX_SURF_VIEWERS;
   if (N_iv15 > 15) {
      fprintf(SUMA_STDERR,"Error %s: trying to register more than 15 viewers!\n", FuncName);
      exit(1);
   }
   for (kar=0; kar<N_iv15; ++kar) iv15[kar] = kar;
   if (!( Element = SUMA_RegisterEngineListCommand (  list, ED, 
                                          SEF_iv15, (void *)iv15, 
                                          SES_Suma, NULL, NOPE, 
                                          SEI_In, Element ))) {
      fprintf(SUMA_STDERR,"Error %s: Failed to register command\n", FuncName);
      exit (1);
   }
   
   if (!( Element = SUMA_RegisterEngineListCommand (  list, ED, 
                                          SEF_i, (void *)&N_iv15, 
                                          SES_Suma, NULL, NOPE, 
                                          SEI_In, Element ))) {
      fprintf(SUMA_STDERR,"Error %s: Failed to register command\n", FuncName);
      exit (1);
   }
   
   
   if (!SUMA_Engine (&list)) {
      fprintf(SUMA_STDERR,"Error %s: Failed in SUMA_Engine\n", FuncName);
      exit (1);
   }
   #else
   /* load the specs file and the specified surfaces*/
		/* Load The spec file */
		if (!SUMA_Read_SpecFile (specfilename, &Spec)) {
			fprintf(SUMA_STDERR,"Error %s: Error in SUMA_Read_SpecFile\n", FuncName);
			exit(1);
		}	

		/* make sure only one group was read in */
		if (Spec.N_Groups != 1) {
			fprintf(SUMA_STDERR,"Error %s: One and only one group of surfaces is allowed at the moment (%d found).\n", FuncName, Spec.N_Groups);
			exit(1);
		}
		
		/* load the surfaces specified in the specs file, one by one*/			
		if (!SUMA_LoadSpec (&Spec, SUMAg_DOv, &SUMAg_N_DOv, VolParName)) {
			fprintf(SUMA_STDERR,"Error %s: Failed in SUMA_LoadSpec.\n", FuncName);
			exit(1);
		}
	
	/* Register the surfaces in Spec file with the surface viewer and perform setups */
	for (kar = 0; kar < SUMA_MAX_SURF_VIEWERS; ++kar) {
		if (!SUMA_SetupSVforDOs (Spec, SUMAg_DOv, SUMAg_N_DOv, &SUMAg_SVv[kar])) {
			fprintf (SUMA_STDERR, "Error %s: Failed in SUMA_SetupSVforDOs function.\n", FuncName);
			exit (1);
		}
	}
	
   if (!list) list = SUMA_CreateList();
   ED = SUMA_InitializeEngineListData (SE_Home);
   if (!SUMA_RegisterEngineListCommand (  list, ED, 
                                          SEF_Empty, NULL, 
                                          SES_Afni, (void*)&SUMAg_SVv[0], NOPE, 
                                          SEI_Tail, NULL )) {
      fprintf(SUMA_STDERR,"Error %s: Failed to register command\n", FuncName);
      exit (1);
   }
   ED = SUMA_InitializeEngineListData (SE_Redisplay);
   if (!SUMA_RegisterEngineListCommand (  list, ED, 
                                          SEF_Empty, NULL, 
                                          SES_Afni, (void*)&SUMAg_SVv[0], NOPE, 
                                          SEI_Tail, NULL )) {
      fprintf(SUMA_STDERR,"Error %s: Failed to register command\n", FuncName);
      exit (1);
   }
   
   if (!SUMA_Engine (&list)) {
      fprintf(SUMA_STDERR,"Error %s: Failed in SUMA_Engine\n", FuncName);
      exit (1);
   }
   
   #endif

	/*Main loop */
	XtAppMainLoop(SUMAg_CF->X->App);

	
	/* Done, clean up time */
	  
	if (!SUMA_Free_Displayable_Object_Vect (SUMAg_DOv, SUMAg_N_DOv)) SUMA_error_message(FuncName,"DO Cleanup Failed!",1);
	if (!SUMA_Free_SurfaceViewer_Struct_Vect (SUMAg_SVv, SUMAg_N_SVv)) SUMA_error_message(FuncName,"SUMAg_SVv Cleanup Failed!",1);
	if (!SUMA_Free_CommonFields(SUMAg_CF)) SUMA_error_message(FuncName,"SUMAg_CF Cleanup Failed!",1);
  return 0;             /* ANSI C requires main to return int. */
}/* Main */ 


