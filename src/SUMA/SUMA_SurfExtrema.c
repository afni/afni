#include "SUMA_suma.h"

void usage_SurfExtrema (SUMA_GENERIC_ARGV_PARSE *ps)
{
      static char FuncName[]={"usage_SurfExtrema"};
      char * s = NULL, *sio=NULL, *st = NULL, *sts = NULL;
      int i;
      s = SUMA_help_basics();
      sio  = SUMA_help_IO_Args(ps);
      printf ( "\n"
      "Usage: A program finding the local extrema in a dataset.\n"
      "   The program finds the nodes with the highest value within an Rmm\n"
      "   radius, and at which the gradient of the signal meets a preset\n"
      "   threshold.\n"
      "   By default, the program searches for maxima.\n"
      "\n"
      " -input DSET = Dset in which Extrema are to be identified.\n"
      "               If you do not specify one, the program use the surface's\n"
      "               convexity dataset.\n"
      " -hood R     = Neighborhood of node n consists of nodes within R \n"
      " -nbhd_rad R = distance from n as measured by the shortest \n"
      "               distance along the mesh.\n"
      "               Default is 8 mm\n"
      " -thresh TH  = Do not consider nodes with value less than TH\n"
      "               Default is 0\n"
      " -gthresh GTH = Do not consider nodes with gradient less than GTH.\n"
      "                Default is 0.01\n"
      " -gscale SCL = What scaling to apply to gradient computation.\n"
      "               Choose from:\n"
      "        NONE: g[n] = sum(v[n]-v[k])/Nk with k the neighbs. of n\n"
      "        LMEAN : Divide g[n] by mean of n and its neighbors * 100\n"
      "        GMEAN : Divide g[n] by mean of all nodes in mask * 100\n"
      "             Default is LMEAN\n"
      " -extype TYP = Find maxima, minima, or extrema.\n"
      "               TYP is one of: MAX (default)\n"
      "                              MIN \n"
      "                              ABS \n"
      " -prefix PREFIX = Prefix of two output data sets.\n"
      "                  First dset is called PREFIX.grd and contains the \n"
      "                  scaled average gradient values.\n"
      "                  Second dset is called PREFIX.ext and contains the \n"
      "                  nodes with maximum values. The value of a non-zero\n"
      "                  node is its rank.\n"
      " -table TABLE = Name of file in which to store a record of the extrema\n"
      "                found. The header part of TABLE contains examples\n"
      "                for easily extracting certain values from it.\n"
      "\n"
      " Examples:\n"
      " ---------\n"
      " 1-  SurfExtrema -i SUMA/std141.rh.smoothwm.asc \\\n"
      "                 -input pb05.rh.niml.dset'[1]' \\\n"
      "                 -gscale LMEAN \\\n"
      "                 -prefix ex1.rh \\\n"
      "                 -table ex1.log \n"
               " \n"
               "%s"
               "%s"
               "\n", sio,  s);
      SUMA_free(s); s = NULL; SUMA_free(st); st = NULL;
      SUMA_free(sio); sio = NULL;
      printf("       Ziad S. Saad SSCC/NIMH/NIH saadz@mail.nih.gov     \n");
      exit(0);
}



SUMA_GENERIC_PROG_OPTIONS_STRUCT *SUMA_SurfExtrema_ParseInput(
                     char *argv[], int argc, SUMA_GENERIC_ARGV_PARSE *ps)
{
   static char FuncName[]={"SUMA_SurfExtrema_ParseInput"};
   SUMA_GENERIC_PROG_OPTIONS_STRUCT *Opt=NULL;
   int kar;
   SUMA_Boolean brk;
   SUMA_Boolean LocalHead = NOPE;

   SUMA_ENTRY;

   Opt = SUMA_Alloc_Generic_Prog_Options_Struct();
   Opt->ps = ps; /* for convenience */
   Opt->NodeDbg = -1;
   Opt->out_prefix = NULL;
   Opt->r = 8.0;
   Opt->t = 0.0;
   Opt->t2 = 0.01;
   Opt->b1 = SUMA_MEAN_GRAD_SCALE;
   Opt->b2 = SUMA_MAXIMUS;
   Opt->s = NULL;
   kar = 1;
   brk = NOPE;
	while (kar < argc) { /* loop accross command ine options */
		/*fprintf(stdout, "%s verbose: Parsing command line...\n", FuncName);*/
		if (strcmp(argv[kar], "-h") == 0 || strcmp(argv[kar], "-help") == 0) {
			 usage_SurfExtrema(ps);
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

      if (!brk && (strcmp(argv[kar], "-table") == 0))
      {
         if (kar+1 >= argc)
         {
            fprintf (SUMA_STDERR, "need a string after -table \n");
            exit (1);
         }

         Opt->s = SUMA_copy_string(argv[++kar]);
         brk = YUP;
      }

      if (!brk && (strcmp(argv[kar], "-prefix") == 0))
      {
         if (kar+1 >= argc)
         {
            fprintf (SUMA_STDERR, "need a dset prefix after -prefix \n");
            exit (1);
         }

         Opt->out_prefix = SUMA_RemoveDsetExtension_s(argv[++kar],
                                                SUMA_NO_DSET_FORMAT);
         brk = YUP;
      }

      if (!brk && (strcmp(argv[kar], "-thresh") == 0))
      {
         if (kar+1 >= argc)
         {
            fprintf (SUMA_STDERR, "need a value after -thresh \n");
            exit (1);
         }

         Opt->t = (float)strtod(argv[++kar],NULL);
         brk = YUP;
      }

      if (!brk && (strcmp(argv[kar], "-gthresh") == 0))
      {
         if (kar+1 >= argc)
         {
            fprintf (SUMA_STDERR, "need a value after -gthresh \n");
            exit (1);
         }

         Opt->t2 = (float)strtod(argv[++kar],NULL);
         brk = YUP;
      }

      if (!brk && (strcmp(argv[kar], "-gscale") == 0))
      {
         if (kar+1 >= argc)
         {
            fprintf (SUMA_STDERR, "need a value after -gscale \n");
            exit (1);
         }
         ++kar;
         if (!strcmp(argv[kar],"NONE"))        Opt->b1=SUMA_NO_GRAD_SCALE;
         else if (!strcmp(argv[kar],"LMEAN")) Opt->b1=SUMA_MEAN_GRAD_SCALE;
         else if (!strcmp(argv[kar],"GMEAN"))
                                                 Opt->b1=SUMA_GMEAN_GRAD_SCALE;
         else {
            SUMA_S_Errv("Bad value (%s) for option -gscale\n", argv[kar]);
            exit (1);
         }
         brk = YUP;
      }

      if (!brk && (strcmp(argv[kar], "-extype") == 0))
      {
         if (kar+1 >= argc)
         {
            fprintf (SUMA_STDERR, "need a string after -extype \n");
            exit (1);
         }
         ++kar;
         if (!strcmp(argv[kar],"MIN"))        Opt->b2=SUMA_MINIMUS;
         else if (!strcmp(argv[kar],"MAX")) Opt->b2=SUMA_MAXIMUS;
         else if (!strcmp(argv[kar],"ABS")) Opt->b2=SUMA_EXTREMUS;
         else {
            SUMA_S_Errv("Bad value (%s) for option -gscale\n", argv[kar]);
            exit (1);
         }
         brk = YUP;
      }



      if (!brk && (strcmp(argv[kar], "-hood") == 0 ||
                   strcmp(argv[kar], "-nbhd_rad") == 0))
      {
         if (kar+1 >= argc)
         {
            fprintf (SUMA_STDERR, "need a value after -nbhd_rad \n");
            exit (1);
         }

         Opt->r = atof(argv[++kar]);
         if (Opt->r <= 0.0) {
            SUMA_S_Errv("neighborhood radius is not valid (have %f from %s).\n",
                        Opt->r, argv[kar]);
		      exit (1);
         }
         brk = YUP;
      }

      if (!brk && !ps->arg_checked[kar]) {
			SUMA_S_Errv("Option %s not understood. Try -help for usage\n",
                     argv[kar]);
			suggest_best_prog_option(argv[0], argv[kar]);
         exit(1);
		} else {
			brk = NOPE;
			kar ++;
		}
   }

   if (!Opt->out_prefix) {
      Opt->out_prefix = SUMA_copy_string("SurfExtrema");
   }
   if (Opt->r <= 0.0) {
      SUMA_S_Errv("neighborhood radius is not set (have %f).\n", Opt->r);
		exit (1);
   }

   SUMA_RETURN(Opt);
}


int main (int argc,char *argv[])
{/* Main */
   static char FuncName[]={"SurfExtrema"};
   SUMA_GENERIC_PROG_OPTIONS_STRUCT *Opt;
   SUMA_GENERIC_ARGV_PARSE *ps=NULL;
   SUMA_DSET_FORMAT iform = SUMA_NO_DSET_FORMAT, oform = SUMA_NO_DSET_FORMAT;
   SUMA_DSET *din=NULL, *dout=NULL, *doute=NULL;
   SUMA_SurfSpecFile *Spec = NULL;
   int i, N_Spec, N_inmask = -1, exists=0;
   SUMA_SurfaceObject *SO=NULL, *SOf=NULL;
   char *ooo=NULL, *s=NULL;
   SUMA_Boolean LocalHead = NOPE;

   SUMA_STANDALONE_INIT;
	SUMA_mainENTRY;

   /* Allocate space for DO structure */
	SUMAg_DOv = SUMA_Alloc_DisplayObject_Struct (SUMA_MAX_DISPLAYABLE_OBJECTS);
   ps = SUMA_Parse_IO_Args(argc, argv, "-i;-t;-spec;-sv;-m;-dset;-talk;");

   if (argc < 2) {
      usage_SurfExtrema(ps);
      exit (1);
   }

   Opt = SUMA_SurfExtrema_ParseInput (argv, argc, ps);

   SUMA_CHECK_OUTPUT_SDSET_STATUS(Opt->out_prefix,
                                  Opt->ps->dsetname[0], oform,
                                  NULL, ".grd", exists);
   SUMA_CHECK_OUTPUT_SDSET_STATUS(Opt->out_prefix,
                                  Opt->ps->dsetname[0], oform,
                                  NULL, ".ext", exists);

   if (Opt->debug > 2) LocalHead = YUP;
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
   if (Spec->N_Surfs == 2) {
      SOf = SUMA_Load_Spec_Surf(Spec, 1, ps->sv[0], Opt->debug);
      if (!SOf) {
         fprintf (SUMA_STDERR,"Error %s:\n"
                              "Failed to find surface\n"
                              "in spec file. \n",
                              FuncName );
         exit(1);
      }
   } else { SOf = NULL; }

   if (!(Opt->nmask = SUMA_load_all_command_masks(Opt->ps->bmaskname,
                              Opt->ps->nmaskname, Opt->ps->cmask, SO->N_Node,
                              &N_inmask)) && N_inmask < 0) {
         SUMA_S_Err("Failed loading mask");
         exit(1);
   }

   if (Opt->ps->N_dsetname != 1 && Opt->ps->N_dsetname != 0) {
      SUMA_S_Errv("Need one and only one dset please."
                  "Have %d on command line.\n", Opt->ps->N_dsetname);
      exit(1);
   }

   if (Opt->ps->N_dsetname) {
      if (!(din = SUMA_LoadDset_s (Opt->ps->dsetname[0], &iform, 0))) {
         SUMA_S_Errv("Failed to load dset named %s\n", Opt->ps->dsetname[0]);
         exit(1);
      }
   } else {
      SUMA_S_Note("No input datasets, will use surface convexity as input.");
      /* Use convexity as input dataset*/
      if (!SUMA_SurfaceMetrics(SO, "Convexity", NULL)) {
         SUMA_S_Errv("Failed to compute convexity for %s\n",
                     SO->Label);
         exit(1);
      }
      if (!(din = (SUMA_DSET *)SUMA_GetCx(SO->idcode_str,
                                          SUMAg_CF->DsetList, 1))) {
         SUMA_S_Errv("Could not get convexity dset for surface %s\n",
                     SO->Label);
         exit(1);
      }
   }

   if (!(dout = SUMA_DsetAvgGradient(SO, NULL, din, Opt->nmask,
                                     0, Opt->b1))) {
      SUMA_S_Err("Failed in SUMA_DsetAvgGradient");
      exit(1);
   }
   /* write it out */
   s = SUMA_OutputDsetFileStatus(Opt->out_prefix, NULL, &oform,
                                    NULL, ".grd", &exists);
   SUMA_AddNgrHist(dout->ngr, FuncName, argc, argv);
   ooo = SUMA_WriteDset_s(s, dout, oform,
                           THD_ok_overwrite(), 0);
   SUMA_free(ooo); ooo=NULL; SUMA_free(s); s = NULL;

   if (!(doute = SUMA_DsetExtrema(SO, NULL, din, dout,
                                  Opt->r, Opt->t, Opt->t2,
                                  Opt->nmask, 0, Opt->b2, Opt->s))) {
      SUMA_S_Err("Failed in SUMA_DsetAvgGradient");
      exit(1);
   }
   /* write it out */
   s = SUMA_OutputDsetFileStatus(Opt->out_prefix, NULL, &oform,
                                    NULL, ".ext", &exists);
   SUMA_AddNgrHist(doute->ngr, FuncName, argc, argv);
   ooo = SUMA_WriteDset_s(s, doute, oform,
                           THD_ok_overwrite(), 0);
   SUMA_free(ooo); ooo=NULL; SUMA_free(s); s = NULL;


   if (ps) SUMA_FreeGenericArgParse(ps); ps = NULL;
   if (Opt) Opt = SUMA_Free_Generic_Prog_Options_Struct(Opt);
   if (!SUMA_Free_CommonFields(SUMAg_CF))
      SUMA_error_message(FuncName,"SUMAg_CF Cleanup Failed!",1);

   exit(0);

}
