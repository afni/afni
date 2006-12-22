/*USE This sample to start writing standalone programs.
Change SurfFWHM to the program name of your choosing.
*/
#include "SUMA_suma.h"

SUMA_SurfaceViewer *SUMAg_cSV = NULL; /*!< Global pointer to current Surface Viewer structure*/
SUMA_SurfaceViewer *SUMAg_SVv = NULL; /*!< Global pointer to the vector containing the various Surface Viewer Structures 
                                    SUMAg_SVv contains SUMA_MAX_SURF_VIEWERS structures */
int SUMAg_N_SVv = 0; /*!< Number of SVs realized by X */
SUMA_DO *SUMAg_DOv = NULL;   /*!< Global pointer to Displayable Object structure vector*/
int SUMAg_N_DOv = 0; /*!< Number of DOs stored in DOv */
SUMA_CommonFields *SUMAg_CF = NULL; /*!< Global pointer to structure containing info common to all viewers */

void usage_SurfFWHM (SUMA_GENERIC_ARGV_PARSE *ps)
{
      static char FuncName[]={"usage_SurfFWHM"};
      char * s = NULL, *sio=NULL, *st = NULL, *sts = NULL;
      int i;
      s = SUMA_help_basics();
      sio  = SUMA_help_IO_Args(ps);
      printf ( "\n"
               "Usage: A program for calculating local and global FWHM.\n"
      " -hood R     = Using this option indicates that you want local\n"
      " -nbhd_rad R = as well as global measures of FWHM. Local measurements\n"
      "               at node n are obtained using a neighborhood that \n"
      "               consists of nodes within R distance from n \n"
      "               as measured by the shortest \n"
      "               distance along the mesh.\n"
      " -prefix PREFIX = Prefix of output data set. Only need when doing \n"
      "                  local FWHM calculations.\n"
               " \n"
               "%s"
               "%s"
               "\n", sio,  s);
      SUMA_free(s); s = NULL; SUMA_free(st); st = NULL; SUMA_free(sio); sio = NULL;       
      s = SUMA_New_Additions(0, 1); printf("%s\n", s);SUMA_free(s); s = NULL;
      printf("       Ziad S. Saad SSCC/NIMH/NIH ziad@nih.gov     \n");
      exit(0);
}
static int ncode=-1 , code[MAX_NCODE];

SUMA_GENERIC_PROG_OPTIONS_STRUCT *SUMA_SurfFWHM_ParseInput(char *argv[], int argc, SUMA_GENERIC_ARGV_PARSE *ps)
{
   static char FuncName[]={"SUMA_SurfFWHM_ParseInput"}; 
   SUMA_GENERIC_PROG_OPTIONS_STRUCT *Opt=NULL;
   int kar;
   SUMA_Boolean brk;
   SUMA_Boolean LocalHead = NOPE;

   SUMA_ENTRY;
   
      
   Opt = SUMA_Alloc_Generic_Prog_Options_Struct();
   Opt->ps = ps; /* for convenience */
   Opt->NodeDbg = -1;
   Opt->out_prefix = NULL;
   Opt->r = -1.0;
   ncode = 0;
   kar = 1;
   brk = NOPE;
	while (kar < argc) { /* loop accross command ine options */
		/*fprintf(stdout, "%s verbose: Parsing command line...\n", FuncName);*/
		if (strcmp(argv[kar], "-h") == 0 || strcmp(argv[kar], "-help") == 0) {
			 usage_SurfFWHM(ps);
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
            fprintf (SUMA_STDERR, "need a node index after -node_debug \n");
            exit (1);
         }
         
         Opt->NodeDbg = atoi(argv[++kar]);
         brk = YUP;
      }
      
      if (!brk && (strcmp(argv[kar], "-prefix") == 0))
      {
         if (kar+1 >= argc)
         {
            fprintf (SUMA_STDERR, "need a dset prefix after -prefix \n");
            exit (1);
         }
         
         Opt->out_prefix = SUMA_copy_string(argv[++kar]);
         brk = YUP;
      }
      
      if (!brk && (strcmp(argv[kar], "-hood") == 0 || strcmp(argv[kar], "-nbhd_rad") == 0))
      {
         if (kar+1 >= argc)
         {
            fprintf (SUMA_STDERR, "need a value after -nbhd_rad \n");
            exit (1);
         }
         
         Opt->r = atof(argv[++kar]);
         if (Opt->r <= 0.0) {
            fprintf (SUMA_STDERR,"Error %s:\nneighborhood radius is not valid (have %f from %s).\n", FuncName, Opt->r, argv[kar]);
		      exit (1);
         }
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

   if (!Opt->out_prefix) {
      Opt->out_prefix = SUMA_copy_string("SurfLocalstat");
   }

   
   SUMA_RETURN(Opt);
}

int main (int argc,char *argv[])
{/* Main */    
   static char FuncName[]={"SurfFWHM"}; 
   SUMA_GENERIC_PROG_OPTIONS_STRUCT *Opt;  
   SUMA_GENERIC_ARGV_PARSE *ps=NULL;
   SUMA_SurfaceObject *SO=NULL;
   int *icols=NULL, N_icols = -1;
   SUMA_DSET_FORMAT iform = SUMA_NO_DSET_FORMAT;
   SUMA_DSET *din=NULL, *dout=NULL;
   SUMA_SurfSpecFile *Spec = NULL;
   int i, N_Spec, N_inmask = -1;
   float *fwhmv=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_STANDALONE_INIT;
	SUMA_mainENTRY;

   /* Allocate space for DO structure */
	SUMAg_DOv = SUMA_Alloc_DisplayObject_Struct (SUMA_MAX_DISPLAYABLE_OBJECTS);
   ps = SUMA_Parse_IO_Args(argc, argv, "-i;-t;-spec;-m;-dset;-talk;");
   
   if (argc < 2) {
      usage_SurfFWHM(ps);
      exit (1);
   }
   
   Opt = SUMA_SurfFWHM_ParseInput (argv, argc, ps);

   if (Opt->debug > 2) LocalHead = YUP;
   if (Opt->ps->N_dsetname != 1) {
      SUMA_S_Errv("Need one and only one dset please. Have %d on command line.\n", Opt->ps->N_dsetname);
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
   
   if (!(Opt->nmask = SUMA_load_all_command_masks(Opt->ps->bmaskname, Opt->ps->nmaskname, Opt->ps->cmask, SO->N_Node, &N_inmask)) && N_inmask < 0) {
         SUMA_S_Err("Failed loading mask");
         exit(1);
   }

   if (Opt->r > 0.0) { /* wants to do localized FWHM */
      SUMA_S_Note("Doing local FWHM...");
      code[0] = NSTAT_FWHMx;
      ncode = 1;
      if (!(dout = SUMA_CalculateLocalStats(SO, din, 
                                       Opt->nmask, 1,
                                       Opt->r, NULL,
                                       ncode, code, 
                                       NULL, Opt->NodeDbg))) {
         SUMA_S_Err("Failed in SUMA_CalculateLocalStats");
         exit(1);
      }
   
      /* write it out */
      SUMA_WriteDset_s(Opt->out_prefix, dout, iform, 0, 0);
      if (dout) SUMA_FreeDset(dout); dout = NULL;
   }
   
   SUMA_S_Note("Doing Global FWHM...");
   /* what columns can we process ?*/
   icols = SUMA_FindNumericDataDsetCols(din, &N_icols);
         
   if (N_icols <= 0) {
      SUMA_SL_Err("No approriate data columns in dset");
      exit(1);   
   }

   if (!(fwhmv = SUMA_estimate_dset_FWHM_1dif(  SO, din, 
                                          icols, N_icols, Opt->nmask, 
                                          1, NULL))) {
      SUMA_S_Err("Rien ne va plus");
      exit(1);                                         
   }
  
   for (i=0; i<N_icols; ++i) {
      fprintf(stdout,"FWHM[%4d]=%.4f\n", icols[i], fwhmv[i]);
   }
   
   if (din) SUMA_FreeDset(din); din = NULL;
   if (dout) SUMA_FreeDset(dout); dout = NULL;
   if (fwhmv) SUMA_free(fwhmv); fwhmv=NULL;
   if (ps) SUMA_FreeGenericArgParse(ps); ps = NULL;
   if (Opt) Opt = SUMA_Free_Generic_Prog_Options_Struct(Opt);
   if (!SUMA_Free_CommonFields(SUMAg_CF)) SUMA_error_message(FuncName,"SUMAg_CF Cleanup Failed!",1);
   
   exit(0);
   
} 
