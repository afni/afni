/*USE This sample to start writing standalone programs.
Change SurfDist to the program name of your choosing.
*/
#include "SUMA_suma.h"

SUMA_SurfaceViewer *SUMAg_cSV = NULL; /*!< Global pointer to current Surface Viewer structure*/
SUMA_SurfaceViewer *SUMAg_SVv = NULL; /*!< Global pointer to the vector containing the various Surface Viewer Structures 
                                    SUMAg_SVv contains SUMA_MAX_SURF_VIEWERS structures */
int SUMAg_N_SVv = 0; /*!< Number of SVs realized by X */
SUMA_DO *SUMAg_DOv = NULL;   /*!< Global pointer to Displayable Object structure vector*/
int SUMAg_N_DOv = 0; /*!< Number of DOs stored in DOv */
SUMA_CommonFields *SUMAg_CF = NULL; /*!< Global pointer to structure containing info common to all viewers */

void usage_SurfDist (SUMA_GENERIC_ARGV_PARSE *ps)
{
      static char FuncName[]={"usage_SurfDist"};
      char * s = NULL, *sio=NULL;
      int i;
      s = SUMA_help_basics();
      sio  = SUMA_help_IO_Args(ps);
      printf ( "\n"
               "Usage: SurfDist <SURFACE> <NODEPAIRS>\n"
               "       Output shortest distance between NODEPAIRS along\n"
               "       the nesh of SURFACE.\n"
               "  <SURFACE> : Surface on which distances are computed.\n"
               "              (For option's syntax, see \n"
               "              'Specifying input surfaces' section below).\n"
               "  <NODEPAIRS>: A dataset of two columns where each row\n"
               "               specifies a node pair.\n"
               "               (For option's syntax, see \n"
               "              'SUMA dataset input options' section below).\n"
               "  example:\n"
               "     echo make a toy surface\n"
               "     CreateIcosahedron\n"
               "     echo Create some nodepairs\n"
               "     echo 13 344 > nodelist.1D\n"
               "     echo 123 32414 >> nodelist.1D\n"
               "     echo Get distances and write out results in a 1D file\n"
               "     SurfDist -i CreateIco_surf.asc \\\n"
               "              -input nodelist.1D > example.1D\n"
               "     less example.1D\n"
               "\n"
               "%s"
               "%s"
               "\n", 
               ps->hverb ? sio:"Use -help for more detail.\n", 
               ps->hverb ? s:"");
      if (s) SUMA_free(s); s = NULL; 
      if (sio) SUMA_free(sio); sio = NULL;       
      s = SUMA_New_Additions(0, 1); printf("%s\n", s);SUMA_free(s); s = NULL;
      printf("       Ziad S. Saad SSCC/NIMH/NIH saadz@mail.nih.gov     \n");
      exit(0);
}

SUMA_GENERIC_PROG_OPTIONS_STRUCT *SUMA_SurfDist_ParseInput(
   char *argv[], int argc, SUMA_GENERIC_ARGV_PARSE *ps)
{
   static char FuncName[]={"SUMA_SurfDist_ParseInput"}; 
   SUMA_GENERIC_PROG_OPTIONS_STRUCT *Opt=NULL;
   int kar;
   SUMA_Boolean brk;
   SUMA_Boolean LocalHead = NOPE;

   SUMA_ENTRY;
   
   Opt = SUMA_Alloc_Generic_Prog_Options_Struct(); 
   Opt->ps = ps;  /* just hold it there for convenience */
   Opt->ps = ps; /* for convenience */
   kar = 1;
   brk = NOPE;
	while (kar < argc) { /* loop accross command ine options */
		/*fprintf(stdout, "%s verbose: Parsing command line...\n", FuncName);*/
		if (strcmp(argv[kar], "-h") == 0 || strcmp(argv[kar], "-help") == 0) {
			 ps->hverb = 1;
          usage_SurfDist(ps);
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
      
      if (!brk && !ps->arg_checked[kar]) {
			SUMA_S_Errv("Option %s not understood.\n"
                     "Try -help for usage\n", 
                     argv[kar]);
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
   static char FuncName[]={"SurfDist"}; 
   SUMA_GENERIC_PROG_OPTIONS_STRUCT *Opt;  
   SUMA_GENERIC_ARGV_PARSE *ps=NULL;
   SUMA_DSET_FORMAT iform = SUMA_NO_DSET_FORMAT;
   SUMA_DSET *din=NULL, *dout=NULL;
   SUMA_SurfSpecFile *Spec = NULL;
   int ii, N_Spec, N_inmask = -1;
   int *nPath = NULL;
   int Nfrom=-1, Nto=-1, N_n=-1;
   float nDistance;
   SUMA_SurfaceObject *SO=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_STANDALONE_INIT;
	SUMA_mainENTRY;

   /* Allocate space for DO structure */
	SUMAg_DOv = SUMA_Alloc_DisplayObject_Struct (SUMA_MAX_DISPLAYABLE_OBJECTS);
   ps = SUMA_Parse_IO_Args(argc, argv, "-i;-t;-s;-sv;-spec;-dset;"); 
                                       /* -mask could be used if needed, see 
                                          comment on masks below*/
   
   if (argc < 2) {
      usage_SurfDist(ps);
      exit (1);
   }
   
   Opt = SUMA_SurfDist_ParseInput (argv, argc, ps);

   if (Opt->debug > 2) LocalHead = YUP;
   if (Opt->ps->N_dsetname != 1) {
      SUMA_S_Errv("Need one and only one dset please.\n"
                  "Have %d on command line.\n", 
                  Opt->ps->N_dsetname);
      exit(1);
   }
   if (!(din = SUMA_LoadDset_s (Opt->ps->dsetname[0], &iform, 0))) {
      SUMA_S_Errv("Failed to load dset named %s\n", Opt->ps->dsetname[0]);
      exit(1);
   }
   
   Spec = SUMA_IO_args_2_spec(ps, &N_Spec);
   if (N_Spec == 0) {
      SUMA_S_Err("No surfaces found.");
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
   /* need some metrics */
   if (!SUMA_SurfaceMetrics_eng( SO, "EdgeList", NULL, 
                                 SUMA_MAX_PAIR(0, Opt->debug -1), 
                                 SUMAg_CF->DsetList)){
      SUMA_S_Err("Failed to metricate");
      exit(1);
   }
   if (!(Opt->nmask = SUMA_load_all_command_masks(
                        Opt->ps->bmaskname, Opt->ps->nmaskname, Opt->ps->cmask,
                        SO->N_Node, &N_inmask)) 
         && N_inmask < 0) {
         SUMA_S_Err("Failed loading mask");
         exit(1);
   }
   
   /* dset has to have two columns */
   if (SDSET_VECNUM(din)!= 2) {
      SUMA_S_Errv("Expected two columns in input data from %s, have %d!\n",
                  Opt->ps->dsetname[0], SDSET_VECNUM(din)); 
      exit(1);
   }
   /* work  the node pairs */
   fprintf(SUMA_STDOUT, "#Internodal distance along graph of surface %s\n"
                        "#A distance of -1 indicates an error of sorts.\n", 
                        SO->Label);
   fprintf(SUMA_STDOUT, "#%-6s %-6s %-6s\n",
                        "From" , "to", "Dist." );
   /* no making is being used at the moment. You
   can't just pass Opt->nmask because Dijkstra destroys its contents.
   You'll need to keep an original copy of it or patch it each time 
   it gets modified. */
   for (ii=0; ii<SDSET_VECLEN(din); ++ii) {
      Nfrom = (int)SUMA_GetDsetValInCol2(din, 0, ii);
      Nto = (int)SUMA_GetDsetValInCol2(din, 1, ii);
      if (!(nPath = SUMA_Dijkstra ( SO, Nfrom, Nto, 
                              NULL, NULL, 1, 
                              &nDistance, &N_n))) {
         nDistance = -1.0;
      } else {
         SUMA_free(nPath); nPath = NULL;
      }
      
      fprintf(SUMA_STDOUT, " %-6d %-6d %-4.2f\n", 
                           Nfrom, Nto, nDistance
                           );
            
   }
   
   if (!SUMA_FreeSpecFields(Spec)) {
      SUMA_S_Err("Failed to free Spec fields");
   } SUMA_free(Spec); Spec = NULL;
   if (ps) SUMA_FreeGenericArgParse(ps); ps = NULL;
   if (Opt) Opt = SUMA_Free_Generic_Prog_Options_Struct(Opt);
   if (!SUMA_Free_CommonFields(SUMAg_CF)) 
      SUMA_error_message(FuncName,"SUMAg_CF Cleanup Failed!",1);
   
   exit(0);
   
} 
