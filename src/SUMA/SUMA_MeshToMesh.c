/*USE This sample to start writing standalone programs.
Change MeshToMesh to the program name of your choosing.
*/
#include "SUMA_suma.h"

SUMA_SurfaceViewer *SUMAg_cSV = NULL; /*!< Global pointer to current Surface Viewer structure*/
SUMA_SurfaceViewer *SUMAg_SVv = NULL; /*!< Global pointer to the vector containing the various Surface Viewer Structures 
                                    SUMAg_SVv contains SUMA_MAX_SURF_VIEWERS structures */
int SUMAg_N_SVv = 0; /*!< Number of SVs realized by X */
SUMA_DO *SUMAg_DOv = NULL;   /*!< Global pointer to Displayable Object structure vector*/
int SUMAg_N_DOv = 0; /*!< Number of DOs stored in DOv */
SUMA_CommonFields *SUMAg_CF = NULL; /*!< Global pointer to structure containing info common to all viewers */

void usage_MeshToMesh (SUMA_GENERIC_ARGV_PARSE *ps)
{
      static char FuncName[]={"usage_MeshToMesh"};
      char * s = NULL, *sio=NULL, *st = NULL, *sts = NULL;
      int i;
      s = SUMA_help_basics();
      sio  = SUMA_help_IO_Args(ps);
      printf ( "\n"
               "Usage: MeshToMesh <-i_TYPE S1> [<-sv SV1>]\n"
               "                  <-i_TYPE S2> [<-sv SV1>]\n"
               " \n"
               "%s"
               "%s"
               "\n", sio,  s);
      SUMA_free(s); s = NULL; SUMA_free(st); st = NULL; SUMA_free(sio); sio = NULL;       
      s = SUMA_New_Additions(0, 1); printf("%s\n", s);SUMA_free(s); s = NULL;
      printf("       Ziad S. Saad SSCC/NIMH/NIH ziad@nih.gov     \n");
      exit(0);
}

SUMA_GENERIC_PROG_OPTIONS_STRUCT *SUMA_MeshToMesh_ParseInput(char *argv[], int argc, SUMA_GENERIC_ARGV_PARSE *ps)
{
   static char FuncName[]={"SUMA_BrainWrap_ParseInput"}; 
   SUMA_GENERIC_PROG_OPTIONS_STRUCT *Opt=NULL;
   int kar;
   SUMA_Boolean brk;
   SUMA_Boolean LocalHead = NOPE;

   SUMA_ENTRY;
   
   Opt = SUMA_Alloc_Generic_Prog_Options_Struct();
   kar = 1;
   brk = NOPE;
   Opt->NodeDbg = -1;
   Opt->debug = 0;
   while (kar < argc) { /* loop accross command ine options */
		/*fprintf(stdout, "%s verbose: Parsing command line...\n", FuncName);*/
		if (strcmp(argv[kar], "-h") == 0 || strcmp(argv[kar], "-help") == 0) {
			 usage_MeshToMesh(ps);
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
      
      if (!brk && (strcmp(argv[kar], "-node_debug") == 0))
      {
         if (kar+1 >= argc)
         {
            fprintf (SUMA_STDERR, "need a number after -node_debug \n");
            exit (1);
         }
         
         Opt->NodeDbg = atoi(argv[++kar]);
         brk = YUP;
      }
      
      if (!brk && !ps->arg_checked[kar]) {
			fprintf (SUMA_STDERR,"Error %s:\nOption %s not understood. Try -help for usage\n", FuncName, argv[kar]);
			exit (1);
		} else {	
			brk = NOPE;
			kar ++;
		}
   }
   
   SUMA_RETURN(Opt);
}

int main (int argc,char *argv[])
{/* Main */    
   static char FuncName[]={"MeshToMesh"}; 
   SUMA_GENERIC_PROG_OPTIONS_STRUCT *Opt;  
   SUMA_GENERIC_ARGV_PARSE *ps=NULL;
   SUMA_SurfaceObject *SO1=NULL, *SO2 = NULL;
   SUMA_SurfSpecFile *Spec = NULL;
   SUMA_M2M_STRUCT *M2M = NULL;
   int N_Spec=0;
   
   SUMA_Boolean LocalHead = NOPE;

	SUMA_mainENTRY;
   SUMA_STANDALONE_INIT;

   /* Allocate space for DO structure */
	SUMAg_DOv = SUMA_Alloc_DisplayObject_Struct (SUMA_MAX_DISPLAYABLE_OBJECTS);
   ps = SUMA_Parse_IO_Args(argc, argv, "-i;-t;-spec;-s;-sv;");
   
   if (argc < 2) {
      usage_MeshToMesh(ps);
      exit (1);
   }
   
   Opt = SUMA_MeshToMesh_ParseInput (argv, argc, ps);

   if (Opt->debug > 2) LocalHead = YUP;

   /* Load the surfaces from command line*/
   Spec = SUMA_IO_args_2_spec(ps, &N_Spec);
   if (N_Spec != 1) {
      SUMA_S_Err( "Multiple spec at input.\n"
                  "Do not mix surface input types together\n");
      exit(1);
   }

      if (Spec->N_Surfs != 2) {
      SUMA_S_Err("2 surfaces expected.");
      exit(1);
   }
   
   SO1 = SUMA_Load_Spec_Surf(Spec, 0, ps->sv[0], 0);
   if (!SO1) {
         fprintf (SUMA_STDERR,"Error %s:\n"
                              "Failed to find surface\n"
                              "in spec file. \n",
                              FuncName );
         exit(1);
   }
   if (!SUMA_SurfaceMetrics(SO1, "EdgeList|MemberFace", NULL)) { SUMA_SL_Err("Failed to create edge list for SO1"); exit(1);  }
   if (!SO1->NodeNormList || !SO1->FaceNormList) { SUMA_LH("Node Normals"); SUMA_RECOMPUTE_NORMALS(SO1); }
   if (Opt->NodeDbg >= SO1->N_Node) {
      SUMA_SL_Warn("node_debug index is larger than number of nodes in surface, ignoring -node_debug.");
      Opt->NodeDbg = -1;
   }
      
   SO2 = SUMA_Load_Spec_Surf(Spec, 1, ps->sv[1], 0);
   if (!SO2) {
         fprintf (SUMA_STDERR,"Error %s:\n"
                              "Failed to find surface\n"
                              "in spec file. \n",
                              FuncName );
         exit(1);
   }  
   if (!SUMA_SurfaceMetrics(SO2, "EdgeList|MemberFace", NULL)) { SUMA_SL_Err("Failed to create edge list for SO2"); exit(1);  }
   if (!SO2->NodeNormList || !SO2->FaceNormList) { SUMA_LH("Node Normals"); SUMA_RECOMPUTE_NORMALS(SO2); }
   
   if (LocalHead) { 
      SUMA_LH("Surf1");
      SUMA_Print_Surface_Object(SO1, NULL);
      SUMA_LH("Surf2");
      SUMA_Print_Surface_Object(SO2, NULL);
   }
   
   SUMA_LH("Going for the mapping of SO1 --> SO2");
   M2M = SUMA_GetM2M_NN( SO1, SO2, NULL, 0, NULL, 0);
   
   /* Now show the mapping results for a debug node ? */
   if (Opt->NodeDbg >= 0) {
      char *s = NULL;
      s = SUMA_M2M_node_Info(M2M, Opt->NodeDbg);
      fprintf(SUMA_STDERR,"%s: Debug for node %d ([%f, %f, %f])of SO1:\n%s\n\n", 
                           FuncName, Opt->NodeDbg, 
                           SO1->NodeList[3*Opt->NodeDbg], SO1->NodeList[3*Opt->NodeDbg+1], SO1->NodeList[3*Opt->NodeDbg+2],
                           s); 
      SUMA_free(s); s = NULL;
   }
   
   if (M2M) M2M = SUMA_FreeM2M(M2M);
   if (SO1) SUMA_Free_Surface_Object(SO1); SO1 = NULL;
   if (SO2) SUMA_Free_Surface_Object(SO2); SO2 = NULL;
   if (Spec) SUMA_free(Spec); Spec = NULL;
   if (ps) SUMA_FreeGenericArgParse(ps); ps = NULL;
   if (Opt) Opt = SUMA_Free_Generic_Prog_Options_Struct(Opt);
   if (!SUMA_Free_CommonFields(SUMAg_CF)) SUMA_error_message(FuncName,"SUMAg_CF Cleanup Failed!",1);
   exit(0);
   
} 
