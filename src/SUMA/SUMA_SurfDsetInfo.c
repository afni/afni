#include "SUMA_suma.h"

SUMA_SurfaceViewer *SUMAg_cSV = NULL; /*!< Global pointer to current Surface Viewer structure*/
SUMA_SurfaceViewer *SUMAg_SVv = NULL; /*!< Global pointer to the vector containing the various Surface Viewer Structures 
                                    SUMAg_SVv contains SUMA_MAX_SURF_VIEWERS structures */
int SUMAg_N_SVv = 0; /*!< Number of SVs realized by X */
SUMA_DO *SUMAg_DOv = NULL;   /*!< Global pointer to Displayable Object structure vector*/
int SUMAg_N_DOv = 0; /*!< Number of DOs stored in DOv */
SUMA_CommonFields *SUMAg_CF = NULL; /*!< Global pointer to structure containing info common to all viewers */

void usage_SurfDsetInfo (SUMA_GENERIC_ARGV_PARSE *ps)
{
      static char FuncName[]={"usage_SurfDsetInfo"};
      char * s = NULL, *sio=NULL, *st = NULL, *sts = NULL;
      int i;
      
      SUMA_ENTRY;
      
      s = SUMA_help_basics();
      sio  = SUMA_help_IO_Args(ps);
      printf ( "\n"
               "Usage: SurfDsetInfo [options] -input DSET1 -input DSET2 \n"
               "   or: SurfDsetInfo [options] DSET1 DSET2 ... \n"
               "   Optional Params:\n"
               "      -debug DBG: if DBG = 2, show dset->ngr in its entirety in NIML form.\n"
               "%s"
               "%s"
               "\n", sio,  s);
      SUMA_free(s); s = NULL; SUMA_free(st); st = NULL; SUMA_free(sio); sio = NULL;       
      s = SUMA_New_Additions(0, 1); printf("%s\n", s);SUMA_free(s); s = NULL;
      printf("       Ziad S. Saad SSCC/NIMH/NIH ziad@nih.gov     \n");
      exit(0);
}

SUMA_GENERIC_PROG_OPTIONS_STRUCT *SUMA_SurfDsetInfo_ParseInput(char *argv[], int argc, SUMA_GENERIC_ARGV_PARSE *ps)
{
   static char FuncName[]={"SUMA_SurfDsetInfo_ParseInput"}; 
   SUMA_GENERIC_PROG_OPTIONS_STRUCT *Opt=NULL;
   int kar;
   SUMA_Boolean brk;
   SUMA_Boolean LocalHead = NOPE;

   SUMA_ENTRY;
   
   Opt = SUMA_Alloc_Generic_Prog_Options_Struct();
   kar = 1;
   brk = NOPE;
	while (kar < argc) { /* loop accross command ine options */
		/*fprintf(stdout, "%s verbose: Parsing command line...\n", FuncName);*/
		if (strcmp(argv[kar], "-h") == 0 || strcmp(argv[kar], "-help") == 0) {
			 usage_SurfDsetInfo(ps);
          exit (0);
		}
		
		SUMA_SKIP_COMMON_OPTIONS(brk, kar);
      
      if (!brk && (strcmp(argv[kar], "-debug") == 0))
      {
         if (kar+1 >= argc)
         {
            fprintf (SUMA_STDERR, "need a number after -debug \n");
            exit (1);
         }
         
         Opt->debug = atoi(argv[++kar]);
         brk = YUP;
      }
      
      if (!brk && (strcmp(argv[kar], "-input") == 0))
      {
         if (kar+1 >= argc)
         {
            fprintf (SUMA_STDERR, "need surface dset after -input \n");
            exit (1);
         }
         if (Opt->n_in_namev < SUMA_GENERIC_PROG_MAX_IN_NAME) {
            Opt->in_namev[Opt->n_in_namev] = argv[++kar];
            ++Opt->n_in_namev; 
         } else {
               SUMA_S_Err("Too many input dsets on command line");
         }
         brk = YUP;
      }
      
      if (!brk && !ps->arg_checked[kar]) {
			/* Assume the rest is input data */
			while (kar < argc) {
            if (Opt->n_in_namev < SUMA_GENERIC_PROG_MAX_IN_NAME) {
               Opt->in_namev[Opt->n_in_namev] = argv[kar];
               ++Opt->n_in_namev; ++kar;
            } else {
               SUMA_S_Err("Too many input dsets on command line");
            }
         }
		} else {	
			brk = NOPE;
			kar ++;
		}
   }
   
   SUMA_RETURN(Opt);
}

int main (int argc,char *argv[])
{/* Main */    
   static char FuncName[]={"SurfDsetInfo"}; 
   SUMA_GENERIC_PROG_OPTIONS_STRUCT *Opt;  
   SUMA_GENERIC_ARGV_PARSE *ps=NULL;
   SUMA_DSET *dset=NULL;
   SUMA_PARSED_NAME *NewName = NULL;
   SUMA_DSET_FORMAT iform = SUMA_NO_DSET_FORMAT;
   int i;
   SUMA_Boolean LocalHead = NOPE;

   SUMA_STANDALONE_INIT;
	SUMA_mainENTRY;

   /* Allocate space for DO structure */
	SUMAg_DOv = SUMA_Alloc_DisplayObject_Struct (SUMA_MAX_DISPLAYABLE_OBJECTS);
   ps = SUMA_Parse_IO_Args(argc, argv, "");
   
   if (argc < 2) {
      usage_SurfDsetInfo(ps);
      exit (1);
   }
   
   Opt = SUMA_SurfDsetInfo_ParseInput (argv, argc, ps);

   if (Opt->debug > 2) LocalHead = YUP;
   
   for (i=0; i<Opt->n_in_namev; ++i) {
      fprintf(SUMA_STDOUT, "\n"
                           "Info for dset: %s\n"
                           "--------------\n",
                            Opt->in_namev[i]);
      
      if (!(NewName = SUMA_ParseFname(Opt->in_namev[i], NULL))) {
         SUMA_S_Err("Name parsing error");
         exit(1);
      }


      if (!(dset = SUMA_LoadDset_s (NewName->FileName, &iform, 0))) {
         SUMA_S_Err("Failed reading dset");
         exit(1);
      }

      if (Opt->debug < 2) SUMA_ShowDset(dset, 0, SUMA_STDOUT);
      else {
         SUMA_ShowNel((void*)dset->ngr);
      }
      if (dset) SUMA_FreeDset(dset); dset = NULL;
      if (NewName) SUMA_Free_Parsed_Name(NewName); NewName = NULL;
   }
   if (ps) SUMA_FreeGenericArgParse(ps); ps = NULL;
   if (Opt) Opt = SUMA_Free_Generic_Prog_Options_Struct(Opt);
   if (!SUMA_Free_CommonFields(SUMAg_CF)) SUMA_error_message(FuncName,"SUMAg_CF Cleanup Failed!",1);
   exit(0);
   
} 
