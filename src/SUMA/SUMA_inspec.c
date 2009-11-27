#include "SUMA_suma.h"

SUMA_SurfaceViewer *SUMAg_cSV = NULL; /*!< Global pointer to current Surface Viewer structure*/
SUMA_SurfaceViewer *SUMAg_SVv = NULL; /*!< Global pointer to the vector containing the various Surface Viewer Structures 
                                    SUMAg_SVv contains SUMA_MAX_SURF_VIEWERS structures */
int SUMAg_N_SVv = 0; /*!< Number of SVs realized by X */
SUMA_DO *SUMAg_DOv = NULL;   /*!< Global pointer to Displayable Object structure vector*/
int SUMAg_N_DOv = 0; /*!< Number of DOs stored in DOv */
SUMA_CommonFields *SUMAg_CF = NULL; /*!< Global pointer to structure containing info common to all viewers */


void usage_SUMA_inspec()
{
   static char FuncName[]={"usage_SUMA_inspec"};
   char * s = NULL;
      
   printf ( "\n"
            "Usage: inspec <-spec specfile> \n"
            "              [-detail d] [-prefix newspecname] \n"
            "              [-LRmerge leftspec rightspec]\n"
            "              [-h/-help]\n"
            "Outputs information found from specfile.\n" 
            "    -spec specfile: specfile to be read\n"
            "    -prefix newspecname: rewrite spec file.\n"
            "    -detail d: level of output detail default is 1 in general,\n"
            "               0 with -LRmerge.  \n"
            "               Available levels are 0, 1, 2 and 3.\n"
            "    -LRmerge LeftSpec RightSpec:\n"
            "             Merge two spec files in a way that makes\n"
            "             sense for viewing in SUMA\n"
            "    -h or -help: This message here.\n" );
   s = SUMA_New_Additions(0, 1); printf("%s\n", s);SUMA_free(s); s = NULL;
   printf ( "      Ziad S. Saad SSCC/NIMH/NIH saadz@mail.nih.gov \n"
            "     Dec 2 03\n"
            "\n");   
   return;
}
int main (int argc,char *argv[])
{/* Main */
   static char FuncName[]={"inspec"};
   int detail, kar;
   char *spec_name=NULL, *spec_name_right=NULL,*outname=NULL;
   SUMA_SurfSpecFile Spec;   
   SUMA_Boolean brk;
   
   SUMA_mainENTRY;
   
	/* allocate space for CommonFields structure */
	SUMAg_CF = SUMA_Create_CommonFields ();
	if (SUMAg_CF == NULL) {
		fprintf(SUMA_STDERR,
         "Error %s: Failed in SUMA_Create_CommonFields\n", FuncName);
		exit(1);
	}
   
   if (argc < 3)
       {
          usage_SUMA_inspec ();
          exit (1);
       }
   
   kar = 1;
	brk = NOPE;
   detail = -1;
   spec_name = NULL;
   spec_name_right = NULL;
	while (kar < argc) { /* loop accross command ine options */
		/*fprintf(stdout, "%s verbose: Parsing command line...\n", FuncName);*/
		if (strcmp(argv[kar], "-h") == 0 || strcmp(argv[kar], "-help") == 0) {
			 usage_SUMA_inspec();
          exit (1);
		}
		if (!brk && (strcmp(argv[kar], "-prefix") == 0)) {
         kar ++;
			if (kar >= argc)  {
		  		fprintf (SUMA_STDERR, "need argument after -prefix ");
				exit (1);
			}
         outname = SUMA_Extension(argv[kar], ".spec", NOPE);
			brk = YUP;
		}
      if (!brk && (strcmp(argv[kar], "-spec") == 0)) {
         kar ++;
			if (kar >= argc)  {
		  		fprintf (SUMA_STDERR, "need argument after -spec ");
				exit (1);
			}
         spec_name = argv[kar];
			if (!SUMA_filexists(spec_name)) {
            fprintf (SUMA_STDERR, 
                     "File %s not found or not readable.\n", spec_name);
            exit(1);
         }
			brk = YUP;
		}
      if (!brk && (strcmp(argv[kar], "-LRmerge") == 0)) {
         kar ++;
			if (kar+1 >= argc)  {
		  		fprintf (SUMA_STDERR, "need 2 arguments after -LRmerge ");
				exit (1);
			}
         spec_name = argv[kar]; kar ++;
         spec_name_right = argv[kar];
			if (!SUMA_filexists(spec_name)) {
            fprintf (SUMA_STDERR, 
                     "File %s not found or not readable.\n", spec_name);
            exit(1);
         }
         if (!SUMA_filexists(spec_name_right)) {
            fprintf (SUMA_STDERR, 
                     "File %s not found or not readable.\n", spec_name_right);
            exit(1);
         }
			brk = YUP;
		}

      if (!brk && (strcmp(argv[kar], "-detail") == 0)) {
         kar ++;
			if (kar >= argc)  {
		  		fprintf (SUMA_STDERR, "need argument after -detail ");
				exit (1);
			}
			detail = atoi(argv[kar]);
         if (detail < 0 || detail > 3) {
            SUMA_SL_Err("detail is < 0 or > 3");
            exit (1);
         }
			brk = YUP;
		}
      
      if (!brk) {
			fprintf (SUMA_STDERR,
                  "Error %s: Option %s not understood. Try -help for usage\n", 
                  FuncName, argv[kar]);
			exit (1);
		} else {	
			brk = NOPE;
			kar ++;
		}
   }
   
   if (spec_name_right && detail < 0) detail = 0;
   
   if (detail < 0) detail = 1;
   
   if (!outname && !detail) {
      SUMA_SL_Err("No detail, or output file requested.\n"
                  "Nothing to do here.");
      exit(1);
   }
   
   if (!spec_name) {
      SUMA_SL_Err("-spec option must be specified.\n");
      exit(1);
   }
   /* load spec file */
   if (!SUMA_AllocSpecFields(&Spec)) { SUMA_S_Err("Error initing"); exit(1); }
   if (!SUMA_Read_SpecFile (spec_name, &Spec)) {
      SUMA_SL_Err("Error in SUMA_Read_SpecFile\n");
      exit(1);
   }
   if (spec_name_right) {
      SUMA_SurfSpecFile SpecR, SpecM;
      if (!SUMA_AllocSpecFields(&SpecR)) { SUMA_S_Err("Error initing"); exit(1);}
      if (!SUMA_Read_SpecFile (spec_name_right, &SpecR)) {
         SUMA_SL_Err("Error in SUMA_Read_SpecFile\n");
         exit(1);
      }
      if (!(SUMA_Merge_SpecFiles( &Spec,
                                  &SpecR,
                                  &SpecM,
                                  outname ? outname:"both.spec"))) {
         SUMA_S_Err("Failed to merge spec files");
         exit(1);
      }
      /* switch */
      SUMA_FreeSpecFields(&Spec); Spec = SpecM;
      SUMA_FreeSpecFields(&SpecR);
   }
   
   /* showme the contents */
   if (detail && !SUMA_ShowSpecStruct (&Spec, NULL, detail)) {
      SUMA_SL_Err("Failed in SUMA_ShowSpecStruct\n");
      exit(1);
   }
   
   if (outname) {
      SUMA_Write_SpecFile(&Spec, outname, NULL, NULL);
      SUMA_free(outname); outname = NULL;
   }
   if (!SUMA_FreeSpecFields(&Spec)) { SUMA_S_Err("Error freeing"); exit(1); }
   
   if (!SUMA_Free_CommonFields(SUMAg_CF)) {
      fprintf(SUMA_STDERR,"Error %s: SUMAg_CF Cleanup Failed!\n", FuncName);
      exit(1);
   }
   
   SUMA_RETURN(0);
}/* main inspec */
