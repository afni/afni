#include "SUMA_suma.h"

SUMA_SurfaceViewer *SUMAg_cSV = NULL; /*!< Global pointer to current Surface Viewer structure*/
SUMA_SurfaceViewer *SUMAg_SVv = NULL; /*!< Global pointer to the vector containing the various Surface Viewer Structures 
                                    SUMAg_SVv contains SUMA_MAX_SURF_VIEWERS structures */
int SUMAg_N_SVv = 0; /*!< Number of SVs realized by X */
SUMA_DO *SUMAg_DOv = NULL;   /*!< Global pointer to Displayable Object structure vector*/
int SUMAg_N_DOv = 0; /*!< Number of DOs stored in DOv */
SUMA_CommonFields *SUMAg_CF = NULL; /*!< Global pointer to structure containing info common to all viewers */

void usage_SurfInfo (SUMA_GENERIC_ARGV_PARSE *ps)
{
      static char FuncName[]={"usage_SurfInfo"};
      char * s = NULL, *sio=NULL, *st = NULL, *sts = NULL;
      int i;
      
      SUMA_ENTRY;
      
      s = SUMA_help_basics();
      sio  = SUMA_help_IO_Args(ps);
      printf ( "\n"
               "Usage: SurfInfo [options] <surface> \n"
               "   surface: A surface specified in any of the methods \n"
               "            shown below.\n"
               "   Optional Params:\n"
               "     -detail DETAIL: 1 = calculate surface metrics.\n"
               "     -debug DEBUG: Debugging level (2 turns LocalHead ON)\n"
               "   Specific Info: Using any of these options outputs values\n"
               "                  only for the specified parameters.\n"  
               "     -N_Node: Number of nodes\n"
               "     -N_FaceSet or -N_Tri: Number of triangles.\n"
               "     \n"
               "     -quiet: Do not include name of parameter in output.\n"
               "     -sep SEP: Use string SEP to separate parameter values.\n"
               "               Default is ' ; '\n"
               "%s"
               "%s"
               "\n", sio,  s);
      SUMA_free(s); s = NULL; SUMA_free(st); st = NULL; 
      SUMA_free(sio); sio = NULL;       
      s = SUMA_New_Additions(0, 1); printf("%s\n", s);SUMA_free(s); s = NULL;
      printf("       Ziad S. Saad SSCC/NIMH/NIH saadz@mail.nih.gov     \n");
      exit(0);
}

SUMA_GENERIC_PROG_OPTIONS_STRUCT *SUMA_SurfInfo_ParseInput(char *argv[], int argc, SUMA_GENERIC_ARGV_PARSE *ps)
{
   static char FuncName[]={"SUMA_SurfInfo_ParseInput"}; 
   SUMA_GENERIC_PROG_OPTIONS_STRUCT *Opt=NULL;
   int kar;
   SUMA_Boolean brk;
   SUMA_Boolean LocalHead = NOPE;

   SUMA_ENTRY;
   
   Opt = SUMA_Alloc_Generic_Prog_Options_Struct();
   Opt->b2=0;
   Opt->in_1D=" ; ";
   kar = 1;
   brk = NOPE;
	while (kar < argc) { /* loop accross command ine options */
		/*fprintf(stdout, "%s verbose: Parsing command line...\n", FuncName);*/
		if (strcmp(argv[kar], "-h") == 0 || strcmp(argv[kar], "-help") == 0) {
			 usage_SurfInfo(ps);
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
      
      if (!brk && (strcmp(argv[kar], "-detail") == 0))
      {
         if (kar+1 >= argc)
         {
            fprintf (SUMA_STDERR, "need a number after -detail \n");
            exit (1);
         }
         
         Opt->b1 = (byte)atoi(argv[++kar]);
         brk = YUP;
      }

      if (!brk && (strcmp(argv[kar], "-sep") == 0))
      {
         if (kar+1 >= argc)
         {
            fprintf (SUMA_STDERR, "need a string after -sep \n");
            exit (1);
         }
         
         Opt->in_1D = argv[++kar];
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
      
      if (!brk && (strcmp(argv[kar], "-N_Node") == 0))
      {
         Opt->s = SUMA_append_replace_string(Opt->s,"N_Node","|",1);
         brk = YUP;
      }
      
      if (!brk && (strcmp(argv[kar], "-N_FaceSet") == 0
                  || strcmp(argv[kar], "-N_Tri") == 0))
      {
         Opt->s = SUMA_append_replace_string(Opt->s,(argv[kar]+1),"|",1);
         brk = YUP;
      }
      
      if (!brk && (strcmp(argv[kar], "-quiet") == 0))
      {
         Opt->b2 = 1;
         brk = YUP;
      }
      
      if (!brk && !ps->arg_checked[kar]) {
			/* Assume the rest is input data */
			while (kar < argc) {
            if (Opt->n_in_namev < SUMA_GENERIC_PROG_MAX_IN_NAME) {
               Opt->in_namev[Opt->n_in_namev] = argv[kar];
               ++Opt->n_in_namev; ++kar;
            } else {
               SUMA_S_Err("Too many input surfaces on command line");
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
   static char FuncName[]={"SurfInfo"}; 
   SUMA_GENERIC_PROG_OPTIONS_STRUCT *Opt;  
   SUMA_GENERIC_ARGV_PARSE *ps=NULL;
   SUMA_SurfSpecFile *Spec = NULL;
   int i, N_Spec;
   char *s = NULL;
   SUMA_SurfaceObject *SO=NULL;
   SUMA_Boolean LocalHead = NOPE;

   SUMA_STANDALONE_INIT;
	SUMA_mainENTRY;

   /* Allocate space for DO structure */
	SUMAg_DOv = SUMA_Alloc_DisplayObject_Struct (SUMA_MAX_DISPLAYABLE_OBJECTS);
   ps = SUMA_Parse_IO_Args(argc, argv, "-i;-t;-spec;-sv;");
   
   if (argc < 2) {
      usage_SurfInfo(ps);
      exit (1);
   }
   
   Opt = SUMA_SurfInfo_ParseInput (argv, argc, ps);

   if (Opt->debug > 2) LocalHead = YUP;
   
   if (ps->s_N_surfnames + ps->i_N_surfnames + ps->t_N_surfnames != 1) {
      SUMA_S_Err("Multiple surface specifications used. "
                 "Only one surface allowed.");
      exit(1);
   }

   Spec = SUMA_IO_args_2_spec(ps, &N_Spec);
   if (N_Spec == 0) {
      SUMA_S_Err("No surfaces found.");
      exit(1);
   }
   if (N_Spec != 1) {
      SUMA_S_Err("Multiple spec at input.");
      exit(1);
   }

   SUMA_LH("Loading surface...");
   SO = SUMA_Load_Spec_Surf(Spec, 0, ps->sv[0], Opt->debug);
   if (!SO) {
         fprintf (SUMA_STDERR,"Error %s:\n"
                              "Failed to find surface\n"
                              "in spec file. \n",
                              FuncName );
         exit(1);
      
   }   
   if (Opt->b1) {
      SUMA_LH("Calculating all metrics, be patient...");
      /* calc trimmings */
      if (!SUMA_SurfaceMetrics_eng(SO, "Convexity|EdgeList|PolyArea|Curvature|"
                                       "EdgeList|MemberFace|CheckWind", 
                                       NULL, Opt->debug, SUMAg_CF->DsetList)) {
         fprintf (SUMA_STDERR,
                  "Error %s: Failed in SUMA_SurfaceMetrics.\n", FuncName);
      }
   }
   if (!Opt->s) { /* the whole thing */
      SUMA_Print_Surface_Object(SO, stdout);
   } else { /* just the specifics */
      char *s=NULL;
      i = 0;
      while ( (s=SUMA_NI_get_ith_string(Opt->s,"|",i) ) ) {
         if (!strcmp(s,"N_Node")) {   
            if (Opt->b2) {
               if (i) fprintf(SUMA_STDOUT, "%s%d", Opt->in_1D, SO->N_Node);
               else fprintf(SUMA_STDOUT, "%d", SO->N_Node);
            } else {
               if (i) fprintf(SUMA_STDOUT, "%s%s=%d", Opt->in_1D, s, SO->N_Node);
               else fprintf(SUMA_STDOUT, "%s=%d", s, SO->N_Node);
            }         
         } else if (!strcmp(s,"N_Tri") || !strcmp(s,"N_FaceSet")) {   
            if (Opt->b2) {
               if (i) fprintf(SUMA_STDOUT, "%s%d", Opt->in_1D, SO->N_FaceSet);
               else fprintf(SUMA_STDOUT, "%d", SO->N_FaceSet);
            } else {
               if (i) fprintf(SUMA_STDOUT, "%s%s=%d", 
                                          Opt->in_1D, s, SO->N_FaceSet);
               else fprintf(SUMA_STDOUT, "%s=%d", s, SO->N_FaceSet);
            }
         } else {
            SUMA_S_Errv("Don't know about parameter >>%s<<\n", s);
            exit(1);
         }
         SUMA_free(s);
         ++i;
      }
      fprintf(SUMA_STDOUT,"\n");
   }
   if (SO) SUMA_Free_Surface_Object(SO); SO = NULL;
   if (ps) SUMA_FreeGenericArgParse(ps); ps = NULL;
   if (Opt) Opt = SUMA_Free_Generic_Prog_Options_Struct(Opt);
   if (!SUMA_Free_CommonFields(SUMAg_CF)) 
      SUMA_error_message(FuncName,"SUMAg_CF Cleanup Failed!",1);
   exit(0);
   
} 
