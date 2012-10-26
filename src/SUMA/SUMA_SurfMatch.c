#include "SUMA_suma.h"
#include "thd_segtools_fNM.h"
#include "SUMA_SegOpts.h"
#include "SUMA_SegFunc.h"
#include "SUMA_CoordMatch.h"

void usage_SurfMatch (SUMA_GENERIC_ARGV_PARSE *ps)
{
      static char FuncName[]={"usage_SurfMatch"};
      char * s = NULL, *sio=NULL, *st = NULL, *sts = NULL;
      int i;
      s = SUMA_help_basics();
      sio  = SUMA_help_IO_Args(ps);
      printf ( 
   "\n"
   "Usage: SurfMatch <-i_TYPE BASE> <-i_TYPE INPUT> <-prefix PREFIX> \n"
   "                 [-sv SURF_VOL] \n"
   " \n"
   "Example: SurfMatch -i std.6rh.pial.asc -i std.6lh.pial.asc -warp sro\n"
   "\n"              
   "\n");
      SUMA_free(s); s = NULL; SUMA_free(st); st = NULL; SUMA_free(sio); sio = NULL;       
      s = SUMA_New_Additions(0, 1); printf("%s\n", s);SUMA_free(s); s = NULL;
      printf("       Ziad S. Saad SSCC/NIMH/NIH saadz@mail.nih.gov     \n");
      exit(0);
}

SUMA_GENERIC_PROG_OPTIONS_STRUCT *
   SUMA_SurfMatch_ParseInput(char *argv[], int argc, SUMA_GENERIC_ARGV_PARSE *ps)
{
   static char FuncName[]={"SUMA_SurfMatch_ParseInput"}; 
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
			 usage_SurfMatch(ps);
          exit (0);
		}

		SUMA_SKIP_COMMON_OPTIONS(brk, kar);

      
      if (!brk && (strcmp(argv[kar], "-warp") == 0))
      {
         if (kar+1 >= argc)
         {
            fprintf (SUMA_STDERR, "need a string after -warp \n");
            exit (1);
         }
         ++kar; 
                if (!strcmp(argv[kar],"shift_only") ||
                    !strcmp(argv[kar],"sho")) { 
            Opt->s = SUMA_copy_string("shft");
         } else if (!strcmp(argv[kar],"shift_rotate") ||
                    !strcmp(argv[kar],"sro")) { 
            Opt->s = SUMA_copy_string("shft+rot");
         } else if (!strcmp(argv[kar],"shift_rotate_scale") ||
                    !strcmp(argv[kar],"srs")) { 
            Opt->s = SUMA_copy_string("shft+rot+scl");
         } else if (!strcmp(argv[kar],"affine_general") ||
                    !strcmp(argv[kar],"aff")) { 
            Opt->s = SUMA_copy_string("shft+rot+scl+shr");
         } else {
            SUMA_S_Errv("Bad -warp parameter of %s\n"
                        "Choose from sho, sro, srs, or aff\n",
                        argv[kar]);
            exit(1);
         }  
         brk = YUP;
      }
      
      if (!brk && (strcmp(argv[kar], "-prefix") == 0))
      {
         if (kar+1 >= argc)
         {
            fprintf (SUMA_STDERR, "need a number after -prefix \n");
            exit (1);
         }
         Opt->out_prefix = SUMA_copy_string(argv[++kar]);
         
         brk = YUP;
      }
            
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
			fprintf (SUMA_STDERR,
                  "Error SurfMatch: Option %s not understood\n", argv[kar]);
         suggest_best_prog_option(argv[0], argv[kar]);
			exit (1);
		} else {	
			brk = NOPE;
			kar ++;
		}
   }
   
   if (!Opt->out_prefix) {
      Opt->out_prefix = SUMA_copy_string("SurfMatch.gii");
      THD_force_ok_overwrite(1) ;
   }
   if (!Opt->s) {
      Opt->s = SUMA_copy_string("shft");
   }
   SUMA_RETURN(Opt);
}

int main (int argc,char *argv[])
{/* Main */    
   static char FuncName[]={"SurfMatch"}; 
   SUMA_GENERIC_PROG_OPTIONS_STRUCT *Opt;  
   SUMA_GENERIC_ARGV_PARSE *ps=NULL;
   SUMA_SurfSpecFile *Spec = NULL;
   int N_Spec=0;
   SUMA_SurfaceObject *SO = NULL, *SOr=NULL;
   SUMA_Boolean LocalHead = NOPE;

   SUMA_STANDALONE_INIT;
	SUMA_mainENTRY;

   /* Allocate space for DO structure */
	SUMAg_DOv = SUMA_Alloc_DisplayObject_Struct (SUMA_MAX_DISPLAYABLE_OBJECTS);
   ps = SUMA_Parse_IO_Args(argc, argv, "-i;-t;-spec;-s;-sv;");
   
   if (argc < 2) {
      usage_SurfMatch(ps);
      exit (1);
   }
   
   Opt = SUMA_SurfMatch_ParseInput (argv, argc, ps);

   if (Opt->debug > 2) LocalHead = YUP;
   
   /* check on inputs */
   if (ps->s_N_surfnames + ps->i_N_surfnames + ps->t_N_surfnames != 2) {
      SUMA_S_Err("Must have two surfaces");
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

   SOr = SUMA_Load_Spec_Surf_with_Metrics(Spec, 0, ps->sv[0], 1);
   if (!SOr) {
         fprintf (SUMA_STDERR,"Error %s:\n"
                              "Failed to find surface\n"
                              "in spec file. \n",
                              FuncName );
         exit(1);
      
   }   
   SO = SUMA_Load_Spec_Surf_with_Metrics(Spec, 1, ps->sv[0], 1);
   
   {
      float *xyzp=NULL;
      int ii, iimax, iimin;
      /* PCA of coords, project along plane perp to eig. vector closest to Y axis,
      then rotate projection plane to match plane Y Z*/
      xyzp = SUMA_Project_Coords_PCA (SO->NodeList, SO->N_Node, 2, 
                                      4, 2); 
      iimax = 0; iimin = 0;
      for (ii=1; ii<SO->N_Node; ++ii) {
         if (xyzp[3*ii+2] > xyzp[3*iimax+2]) {
            iimax = ii;
         }
         if (xyzp[3*ii+2] < xyzp[3*iimin+2]) {
            iimin = ii;
         }
      }
      SUMA_S_Notev("Highest node %d, lowest node %d\n",
         iimax, iimin);
      SUMA_free(xyzp); xyzp = NULL;
   }
   SUMA_S_Notev("Have Reference %s %d nodes, and input %s, %d nodes\n",
                  SOr->Label, SOr->N_Node, SO->Label, SO->N_Node);
   SUMA_AlignCoords(SO->NodeList, SO->N_Node, NULL, 1, 
                    SOr, Opt->s);
   
   /* write surface */
   if (!SUMA_Save_Surface_Object_Wrap (Opt->out_prefix, NULL, SO, 
                                  SUMA_FT_NOT_SPECIFIED, SUMA_FF_NOT_SPECIFIED, 
                                  NULL)) {
      SUMA_S_Err("Failed to write surface of whole head");
      exit (1);
   }
   if (SOr) SUMA_Free_Surface_Object(SOr); SOr = NULL;
   if (SO) SUMA_Free_Surface_Object(SO); SO = NULL;
   
                    
   if (ps) SUMA_FreeGenericArgParse(ps); ps = NULL;
   if (N_Spec) {
      int k=0; 
      for (k=0; k<N_Spec; ++k) {
         if (!SUMA_FreeSpecFields(&(Spec[k]))) { 
            SUMA_S_Err("Failed to free spec fields"); 
         } 
      }
      SUMA_free(Spec); Spec = NULL; N_Spec = 0;
   }
   if (Opt) Opt = SUMA_Free_Generic_Prog_Options_Struct(Opt);
   if (!SUMA_Free_CommonFields(SUMAg_CF)) 
      SUMA_error_message(FuncName,"SUMAg_CF Cleanup Failed!",1);
   exit(0);
   
} 
