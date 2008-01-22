#include "SUMA_suma.h"

SUMA_SurfaceViewer *SUMAg_cSV = NULL; /*!< Global pointer to current Surface Viewer structure*/
SUMA_SurfaceViewer *SUMAg_SVv = NULL; /*!< Global pointer to the vector containing the various Surface Viewer Structures 
                                    SUMAg_SVv contains SUMA_MAX_SURF_VIEWERS structures */
int SUMAg_N_SVv = 0; /*!< Number of SVs realized by X */
SUMA_DO *SUMAg_DOv = NULL;   /*!< Global pointer to Displayable Object structure vector*/
int SUMAg_N_DOv = 0; /*!< Number of DOs stored in DOv */
SUMA_CommonFields *SUMAg_CF = NULL; /*!< Global pointer to structure containing info common to all viewers */


void usage_SUMA_quickspec()
{
   static char FuncName[]={"usage_SUMA_quickspec"};
   char * s = NULL;
   printf ( 
"\nUsage:  quickspec \n"
"        <-tn TYPE NAME> ...\n"
"        <-tsn TYPE STATE NAME> ...\n"
"        [<-spec specfile>] [-h/-help]\n"
"  Use this spec file for quick and dirty way of \n"
"  loading a surface into SUMA or the command line programs.\n"
"\n"
"Options:\n"
"   -tn TYPE NAME: specify surface type and name.\n"
"                  See below for help on the parameters.\n"
"   -tsn TYPE STATE NAME: specify surface type state and name.\n"
"        TYPE: Choose from the following (case sensitive):\n"
"           1D: 1D format\n"
"           FS: FreeSurfer ascii format\n"
"           PLY: ply format\n"
"           SF: Caret/SureFit format\n"
"           BV: BrainVoyager format\n"
"        STATE: State of the surface.\n"
"           Default is S1, S2.... for each surface.\n"
"        NAME: Name of surface file. \n"
"           For SF and 1D formats, NAME is composed of two names\n"
"           the coord file followed by the topo file\n"
"   -spec specfile: Name of spec file output.\n"
"                   Default is quick.spec\n"
"                   The program will only overwrite \n"
"                   quick.spec (the default) spec file.\n"
"   -h or -help: This message here.\n" 
"\n"
"  You can use any combinaton of -tn and -tsn options.\n"
"  Fields in the spec file that are (or cannot) be specified\n"
"  by this program are set to default values.\n"
"\n   This program was written to ward off righteous whiners and is\n"
"  not meant to replace the venerable @SUMA_Make_Spec_XX scripts.\n"
"\n"
      );
     s = SUMA_New_Additions(0, 1); printf("%s\n", s);SUMA_free(s); s = NULL;
     printf("      Ziad S. Saad SSCC/NIMH/NIH saadz@mail.nih.gov \n\t\t Tue Dec 30\n"
            "\n");
    return;   
}

int main (int argc,char *argv[])
{/* Main */    
   static char FuncName[]={"quickspec"};
   int detail, kar, i, N_surf, N_name, idefstate;
   FILE *fid = NULL;
   char *spec_name, stmp[500], *Unique_st;
   SUMA_SO_File_Type TypeC[SUMA_MAX_N_SURFACE_SPEC];
   char  *State[SUMA_MAX_N_SURFACE_SPEC],
         *Name_coord[SUMA_MAX_N_SURFACE_SPEC],
         *Name_topo[SUMA_MAX_N_SURFACE_SPEC];
   SUMA_Boolean brk;
   
   SUMA_mainENTRY;
   
	/* allocate space for CommonFields structure */
	SUMAg_CF = SUMA_Create_CommonFields ();
	if (SUMAg_CF == NULL) {
		fprintf(SUMA_STDERR,"Error %s: Failed in SUMA_Create_CommonFields\n", FuncName);
		exit(1);
	}
   
   if (argc < 3)
       {
          usage_SUMA_quickspec ();
          exit (1);
       }
   
   kar = 1;
	brk = NOPE;
   detail = 1;
   N_surf = 0;
   N_name = 0;
   spec_name = NULL;
	while (kar < argc) { /* loop accross command ine options */
		/*fprintf(stdout, "%s verbose: Parsing command line...\n", FuncName);*/
		if (strcmp(argv[kar], "-h") == 0 || strcmp(argv[kar], "-help") == 0) {
			 usage_SUMA_quickspec();
          exit (1);
		}
		if (!brk && (strcmp(argv[kar], "-spec") == 0)) {
         kar ++;
			if (kar >= argc)  {
		  		fprintf (SUMA_STDERR, "need argument after -spec \n");
				exit (1);
			}
         spec_name = argv[kar];
			if (!THD_ok_overwrite() && SUMA_filexists(spec_name)) {
            fprintf (SUMA_STDERR, 
               "File %s exists, choose another one.\n", spec_name);
            exit(1);
         }
			brk = YUP;
		}
      if (!brk && (strcmp(argv[kar], "-tn") == 0)) {
         if (N_surf >= SUMA_MAX_N_SURFACE_SPEC) {
            SUMA_SL_Err("Exceeding maximum number of allowed surfaces...");
            exit(1);   
         }
         /* get the type */
         kar ++;
			if (kar >= argc)  {
		  		fprintf (SUMA_STDERR, "Type argument must follow -tn \n");
				exit (1);
			}
         TypeC[N_surf] = SUMA_SurfaceTypeCode(argv[kar]);
         if (TypeC[N_surf] == SUMA_FT_ERROR || TypeC[N_surf] == SUMA_FT_NOT_SPECIFIED) {
            fprintf (SUMA_STDERR, "%s is a bad file type.\n", argv[kar]);
            exit(1);
         }
         /* get the name */
         if (TypeC[N_surf] == SUMA_SUREFIT || TypeC[N_surf] == SUMA_VEC) N_name = 2;
         else N_name = 1;
         if (kar+N_name >= argc)  {
		  		fprintf (SUMA_STDERR, "need %d elements for NAME \n", N_name);
				exit (1);
			}
         kar ++; Name_coord[N_surf] = argv[kar];
         if (N_name == 2) {
            kar ++; Name_topo[N_surf] = argv[kar];
         } else { 
            Name_topo[N_surf] = NULL;
         }
         State[N_surf] = NULL;
         
         ++N_surf; 
			brk = YUP;
		}
      if (!brk && (strcmp(argv[kar], "-tsn") == 0)) {
         if (N_surf >= SUMA_MAX_N_SURFACE_SPEC) {
            SUMA_SL_Err("Exceeding maximum number of allowed surfaces...");
            exit(1);   
         }
         /* get the type */
         kar ++;
			if (kar >= argc)  {
		  		fprintf (SUMA_STDERR, "TYPE argument must follow -tsn \n");
				exit (1);
			}
         TypeC[N_surf] = SUMA_SurfaceTypeCode(argv[kar]);
         if (TypeC[N_surf] == SUMA_FT_ERROR || TypeC[N_surf] == SUMA_FT_NOT_SPECIFIED) {
            fprintf (SUMA_STDERR, "%s is a bad file TYPE.\n", argv[kar]);
            exit(1);
         }
         /* get the state */
         kar ++;
			if (kar >= argc)  {
		  		fprintf (SUMA_STDERR, "STATE argument must follow TYPE with -tsn \n");
				exit (1);
			}
         State[N_surf] = argv[kar];
         
         /* get the name */
         if (TypeC[N_surf] == SUMA_SUREFIT || TypeC[N_surf] == SUMA_VEC) N_name = 2;
         else N_name = 1;
         if (kar+N_name >= argc)  {
		  		fprintf (SUMA_STDERR, "need %d elements for NAME \n", N_name);
				exit (1);
			}
         kar ++; Name_coord[N_surf] = argv[kar];
         if (N_name == 2) {
            kar ++; Name_topo[N_surf] = argv[kar];
         } else { 
            Name_topo[N_surf] = NULL;
         }
         
         ++N_surf; 
			brk = YUP;
		}
      
      if (!brk) {
			fprintf (SUMA_STDERR,"Error %s: Option %s not understood. Try -help for usage\n", FuncName, argv[kar]);
			exit (1);
		} else {	
			brk = NOPE;
			kar ++;
		}
   }
   
   /* write out the comments */
   if (!spec_name) {
      fid = fopen("quick.spec", "w");
   } else {
      fid = fopen(spec_name,"w");
   }
   if (!fid){
      SUMA_SL_Err("Failed to open file for output");
      exit(1);
   }
   fprintf(fid,"# define the group\n");
   fprintf(fid,"\tGroup = QuickSpec\n");
   
   
   /* now create a list of unique states */
   idefstate = 0;
   if (!State[0]) {
      Unique_st = SUMA_copy_string ("\tStateDef = S_1\n");
      idefstate = 1;
   } else {
      sprintf(stmp, "\tStateDef = %s\n", State[0]);
      Unique_st = SUMA_copy_string (stmp);
   }
   for (i=1; i < N_surf; ++i) {
      if (!State[i]) { 
         ++idefstate;
         sprintf(stmp,"\tStateDef = S_%d\n", idefstate);
         Unique_st = SUMA_append_replace_string (Unique_st, stmp, "", 1);
      } else { 
         if (SUMA_iswordin(Unique_st, State[i]) != 1) {
            sprintf(stmp, "\tStateDef = %s\n", State[i]);
            Unique_st = SUMA_append_replace_string(Unique_st, stmp, "", 1);
         }
      }
   }
   fprintf (fid, "# define the various States\n");
   fprintf (fid, "%s\n", Unique_st);
   
   /* now loop accross surfaces and write out the results */
   idefstate = 0;
   for (i=0; i < N_surf; ++i) {
      fprintf(fid, "\nNewSurface\n");
      fprintf(fid, "\tSurfaceType = %s\n", SUMA_SurfaceTypeString(TypeC[i]));
      if (!State[i]) { 
         ++idefstate;
         fprintf(fid, "\tSurfaceState = S_%d\n", idefstate);
      } else fprintf(fid, "\tSurfaceState = %s\n", State[i]);
      if (Name_topo[i]) {
         fprintf(fid, "\tCoordFile = %s\n", Name_coord[i]);
         fprintf(fid, "\tTopoFile = %s\n", Name_topo[i]);
      } else {
         fprintf(fid, "\tSurfaceName = %s\n", Name_coord[i]); 
      }
      /* add LocalDomainParent */
      fprintf(fid, "\tLocalDomainParent = SAME\n");
      /* add Anatomical */
      fprintf(fid, "\tAnatomical = Y\n");
      /* binary ? */
      switch (TypeC[i]) {
         case SUMA_FREE_SURFER:
            if (!SUMA_isExtension(Name_coord[i], ".asc")) {
               fprintf(fid, "\tSurfaceFormat = BINARY\n");
            }
            break;
         default:
            break;
      }
   }
   
   fclose(fid); fid = NULL;
   
   if (Unique_st) SUMA_free(Unique_st); Unique_st = NULL;
   
   if (!SUMA_Free_CommonFields(SUMAg_CF)) {
      fprintf(SUMA_STDERR,"Error %s: SUMAg_CF Cleanup Failed!\n", FuncName);
      exit(1);
   }
   
   SUMA_RETURN(0);
   
}/* main quickspec */
