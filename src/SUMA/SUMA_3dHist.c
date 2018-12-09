#define MAIN

#include "SUMA_suma.h"
#include "thd_segtools_fNM.h"
#include "SUMA_SegOpts.h"
#include "SUMA_SegFunc.h"
#include "matrix.h"


static int vn=0 ;



static char shelp_Hist[] = {
"3dHist computes histograms using functions for generating priors.\n"
"If you are not sure you need this particular program, use 3dhistog instead.\n"
"\n"
"Example:\n"
"3dHist      -input sigs+orig \\n"
"               \n"
"\n"
"Options:\n"
"   -input DSET: Dset providing values for histogram. Exact 0s are not counted\n"
"   -dind SB: Use sub-brick SB from the input rather than 0\n"
"   -mask MSET: Provide mask dataset to select subset of input.\n"
"   -mask_range BOT TOP: Specify the range of values to consider from MSET.\n"
"                        Default is anything non-zero\n"
"   -cmask CMASK: Provide cmask expression. Voxels where expression is 0\n"
"                 are excluded from computations. For example:\n"
"            -cmask '-a T1.div.r+orig -b T1.uni.r+orig -expr step(a/b-10)'\n"
"   -thishist HIST.niml.hist: Read this previously created histogram instead\n"
"                             of forming one from DSET.\n"
"                             Obviously, DSET, or -mask options are not needed\n"
"   -prefix PREF: Write histogram to niml file called PREF.niml.hist \n"
"   -equalized PREF: Write a histogram equalized version of the input dataset\n"
"   Histogram Creation Parameters:\n"
"     By default, the program will select bin number, bin width, \n"
"     and range automatically. You can also set the parameters manually with \n"
"     the following options.\n"
"   -nbin K: Use K bins.\n"
"   -min MIN: Minimum intensity.\n"
"   -max MAX: Maximum intensity.\n"
"   -binwidth BW: Bin width\n"
"   -ignore_out: Do not count samples outside the user specified range.\n"
"   -rhist RHIST.niml.hist: Use previously created histogram to set range\n"
"                           and binwidth parameters.\n"
"\n"
"   -showhist: Display histogram to stdout\n"
"              You can also graph it with: 1dRplot HistOut.niml.hist\n"
"\n"
"   Histogram Queries:\n"
"   -at VAL: Set the value at which you want histogram values\n"
"   -get 'PAR1,PAR2,PAR3..': Return the following PAR* properties at VAL\n"
"                            Choose from:\n"
"                            freq: Frequency (normalized count)\n"
"                            count: Count\n"
"                            bin: Continuous bin location estimate\n"
"                            cdf: Cumulative count\n"
"                            rcdf: Reverse cumulative count (from the top)\n"
"                            ncdf: The normalized version of cdf\n"
"                            nrcdf: The reverse version of ncdf\n"
"                            outl: 1.0-(2*smallest tail area)\n"
"                               0 means VAL splits area in the middle\n"
"                               1 means VAL is at either end of the histogram\n"
"                            ALL: All the above.\n"
"                   You can select multiple ones with something like:\n"
"                        -get 'freq, count, bin' \n"
"\n"
"                   You can also set one of the PAR* to 'upvol' to get \n"
"                   the volume (liters) of voxels with values exceeding VAL\n"
"                   The use of upvol usually requires option -voxvol too.\n"
"  -voxvol VOL_MM3: A voxel's volume in mm^3. To be used with upvol if\n"
"                   no dataset is available or if you want to override\n"
"                   it.\n"
"  -val_at PAR PARVAL: Return the value (magnitude) where histogram property\n"
"                      PAR is equal to PARVAL\n"
"                      PAR can only be one of: cdf, rcdf, ncdf, nrcdf, upvol\n"
"                      For upvol, PARVAL is in Liters\n"
"  -quiet: Return a concise output to simplify parsing. For the moment, this\n"
"          option only affects output of option -val_at\n"
"\n"
"  Examples:\n"
"       #A histogram a la 3dhistog:\n"
"       3dHist -input T1+orig.\n"
"       #Getting parameters from previously created histogram:\n"
"       3dHist -thishist HistOut.niml.hist -at 144.142700 \n"
"       #Or the reverse query:\n"
"       3dHist -thishist HistOut.niml.hist -val_at ncdf 0.132564\n"
"       #Compute histogram and find dataset threshold (approximate)\n"
"       #such that 1.5 liters of voxels remain above it.\n"
"       3dHist -prefix toy -input flair_axial.nii.gz -val_at upvol 1.5 \n"
"\n"
/* Untested here
"   -mrange M0 M1: Consider MASK only for values between M0 and M1, inclusive\n"
"   -debug DBG: Set debug level\n"
*/
"\n"
};


void Hist_usage(int detail)
{
   int i = 0;

   ENTRY("Hist_usage");


   printf( "%s", shelp_Hist );
   PRINT_COMPILE_DATE ;
   exit(0);
}

SEG_OPTS *Hist_Default(char *argv[], int argc)
{
   SEG_OPTS *Opt=NULL;

   ENTRY("Hist_Default");

   Opt = SegOpt_Struct();

   Opt->helpfunc = &Hist_usage;
   Opt->ps = SUMA_Parse_IO_Args(argc, argv, "-talk;");
   Opt->mset_name = NULL;
   Opt->sig_name = NULL;
   Opt->ndist_name = NULL;
   Opt->uid[0] = '\0';
   Opt->prefix = NULL;
   Opt->crefix = NULL;
   Opt->aset = NULL;
   Opt->mset = NULL;
   Opt->gset = NULL;
   Opt->sig = NULL;
   Opt->FDV = NULL;
   Opt->pset = NULL;
   Opt->cset = NULL;
   Opt->debug = 0;
   Opt->verbose = 1;
   Opt->idbg = Opt->kdbg = Opt->jdbg = -1;
   Opt->feats=Opt->clss=NULL;
   Opt->feat_exp=NULL; Opt->featexpmeth=0; Opt->featsfam=NULL;
   Opt->keys = NULL;
   Opt->mixfrac=NULL;
   Opt->logp = 1;
   Opt->VoxDbg = -1;
   Opt->VoxDbgOut = NULL;
   Opt->rescale_p = 1;
   Opt->openmp = 0;
   Opt->labeltable_name = NULL;
   Opt->pweight = 1;
   Opt->cmask = NULL;
   Opt->dimcmask = 0;
   Opt->cmask_count=0;
   Opt->mask_bot = 1.0;
   Opt->mask_top = -1.0;
   Opt->DO_p = TRUE;
   Opt->DO_c = TRUE;
   Opt->Writepcg_G_au = FALSE;
   Opt->DO_r = FALSE;
   Opt->group_classes = NULL;
   Opt->group_keys = NULL;
   Opt->fitmeth = SEG_LSQFIT;
   Opt->proot = NULL;
   Opt->cs = NULL;
   Opt->Gcs = NULL;
   Opt->fast = 1;
   Opt->ShowThisDist = NULL;
   Opt->wA = -123456999.0;
   Opt->wL = -123456999.0;
   Opt->this_xset_name=NULL;
   Opt->smode = 0;
   Opt->N_biasgroups = 0;
   Opt->binwidth = 0;
   Opt->UseTmp = 0;
   Opt->range[0] = -9876543.0;
   Opt->range[1] = -9876543.0;
   Opt->bias_meth = NULL;
   SUMA_RETURN(Opt);
}

int Hist_CheckOpts(SEG_OPTS *Opt)
{
   static char FuncName[]={"Hist_CheckOpts"};

   SUMA_ENTRY;

   if (  !Opt->sig_name   && !Opt->proot) {
      SUMA_S_Err("Missing input");
      SUMA_RETURN(0);
   }

   SUMA_RETURN(1);
}
char ALL_GETS[]={"bin, count, freq, cdf, ncdf, rcdf, nrcdf, outl"};

SEG_OPTS *Hist_ParseInput (SEG_OPTS *Opt, char *argv[], int argc)
{
   static char FuncName[]={"Hist_ParseInput"};
   int kar, i, ind, exists;
   char *outname, cview[10];
   int brk = 0;
   SUMA_GENERIC_ARGV_PARSE *ps=NULL;

   SUMA_ENTRY;

   brk = 0;
   Opt->f1 = -1.0;
   kar = 1;
	while (kar < argc) { /* loop accross command ine options */
		/*fprintf(stdout, "%s verbose: Parsing command line...\n", FuncName);*/
		if (strcmp(argv[kar], "-h") == 0 || strcmp(argv[kar], "-help") == 0) {
			 Opt->helpfunc(0);
          exit (0);
		}

 		SUMA_SKIP_COMMON_OPTIONS(brk, kar);

      #ifdef USE_TRACING
            if( strncmp(argv[kar],"-trace",5) == 0 ){
               DBG_trace = 1 ;
               brk = 1 ;
            }
            if( strncmp(argv[kar],"-TRACE",5) == 0 ){
               DBG_trace = 2 ;
               brk = 1 ;
            }
      #endif

      if (!brk && (strcmp(argv[kar], "-debug") == 0)) {
         kar ++;
			if (kar >= argc)  {
		  		fprintf (stderr, "need argument after -debug \n");
				exit (1);
			}
			Opt->debug = atoi(argv[kar]);
         brk = 1;
		}

      if (!brk && (strcmp(argv[kar], "-quiet") == 0)) {
			Opt->verbose = 0;
         brk = 1;
		}


      if (!brk && (strcmp(argv[kar], "-cmask") == 0)) {
         kar ++;
			if (kar >= argc)  {
		  		ERROR_exit("-cmask option requires a following argument!\n");
			}
			Opt->cmask = EDT_calcmask( argv[kar] , &(Opt->dimcmask), 0 ) ;
         if( Opt->cmask == NULL ) ERROR_exit("Can't compute -cmask!\n");
         brk = 1;
		}

      if (!brk && (strcmp(argv[kar], "-mask") == 0)) {
         kar ++;
			if (kar >= argc)  {
		  		fprintf (stderr, "need argument after -mask \n");
				exit (1);
			}
			Opt->mset_name = argv[kar];
         brk = 1;
      }

      if( !brk && (strncmp(argv[kar],"-mrange",5) == 0) ){
         if( kar+2 >= argc )
           ERROR_exit("-mrange option requires 2 following arguments!\n");
         Opt->mask_bot = strtod( argv[++kar] , NULL ) ;
         Opt->mask_top = strtod( argv[++kar] , NULL ) ;
         if( Opt->mask_top < Opt->mask_bot )
           ERROR_exit("-mrange inputs are illegal!\n") ;
         brk = 1;
      }

      if (!brk && (strcmp(argv[kar], "-input") == 0)) {
         kar ++;
			if (kar >= argc)  {
		  		fprintf (stderr, "need argument after -input \n");
				exit (1);
			}
			while (kar < argc && argv[kar][0] != '-') {
            Opt->sig_name =
               SUMA_append_replace_string(Opt->sig_name, argv[kar], " ", 1);
            ++kar;
         }
         if (kar < argc && argv[kar][0] == '-') --kar; /* unwind */
         brk = 1;
		}

      if (!brk && (strcmp(argv[kar], "-rhist") == 0)) {
         kar ++;
			if (kar >= argc)  {
		  		fprintf (stderr, "need argument after -rhist \n");
				exit (1);
			}
			Opt->ndist_name = argv[kar];
         brk = 1;
		}

      if (!brk && (strcmp(argv[kar], "-nbin") == 0)) {
         kar ++;
			if (kar >= argc)  {
		  		fprintf (stderr, "need argument after -nbin \n");
				exit (1);
			}
			Opt->UseTmp = atoi(argv[kar]);
         brk = 1;
		}

      if (!brk && (strcmp(argv[kar], "-binwidth") == 0)) {
         kar ++;
			if (kar >= argc)  {
		  		fprintf (stderr, "need value after -binwidth \n");
				exit (1);
			}
			Opt->binwidth = atof(argv[kar]);
         brk = 1;
		}

      if (!brk && (strcmp(argv[kar], "-min") == 0)) {
         kar ++;
			if (kar >= argc)  {
		  		fprintf (stderr, "need value after -min \n");
				exit (1);
			}
			Opt->range[0] = atof(argv[kar]);
         brk = 1;
		}

      if (!brk && (strcmp(argv[kar], "-voxvol") == 0)) {
         kar ++;
			if (kar >= argc)  {
		  		fprintf (stderr, "need value after -voxvol in mm^3\n");
				exit (1);
			}
			Opt->f1 = atof(argv[kar]);
         brk = 1;
		}

      if (!brk && (strcmp(argv[kar], "-max") == 0)) {
         kar ++;
			if (kar >= argc)  {
		  		fprintf (stderr, "need value after -max \n");
				exit (1);
			}
			Opt->range[1] = atof(argv[kar]);
         brk = 1;
		}

      if( !brk && (strncmp(argv[kar],"-range",5) == 0) ){
         if( kar+2 >= argc )
           ERROR_exit("-range option requires 2 following arguments!\n");
         Opt->range[0] = strtod( argv[++kar] , NULL ) ;
         Opt->range[1] = strtod( argv[++kar] , NULL ) ;
         if( Opt->range[0] < Opt->range[1] )
           ERROR_exit("-range inputs are illegal!\n") ;
         brk = 1;
      }

      if (!brk && (strcmp(argv[kar], "-at") == 0)) {
         kar ++;
			if (kar >= argc)  {
		  		fprintf (stderr, "need value after -at \n");
				exit (1);
			}
			Opt->wA = atof(argv[kar]);
         brk = 1;
		}

      if (!brk && (strcmp(argv[kar], "-dind") == 0)) {
         kar ++;
			if (kar >= argc)  {
		  		fprintf (stderr, "need integer after -dind \n");
				exit (1);
			}
			Opt->N_biasgroups = atoi(argv[kar]);
         brk = 1;
		}

      if (!brk && (strcmp(argv[kar], "-get") == 0)) {
         kar ++;
			if (kar >= argc)  {
		  		fprintf (stderr, "need string after -get \n");
				exit (1);
			}
         if (strstr(argv[kar],"ALL")) {
            Opt->feats = NI_strict_decode_string_list(ALL_GETS, ";, ");
         } else {
            if (!(Opt->feats = NI_strict_decode_string_list(argv[kar] ,";, "))){
               SUMA_S_Errv("Bad option %s after -get", argv[kar]);
               exit(1);
            }
         }
         brk = 1;
      }

      if (!brk && (strcmp(argv[kar], "-val_at") == 0)) {
         kar ++;
			if (kar+1 >= argc)  {
		  		fprintf (stderr, "need string and value after -val_at \n");
				exit (1);
			}
         Opt->this_xset_name = argv[kar];
         if (strcmp(argv[kar],"cdf") &&
             strcmp(argv[kar],"rcdf") &&
             strcmp(argv[kar],"ncdf") &&
             strcmp(argv[kar],"nrcdf") &&
             strcmp(argv[kar],"upvol")   ) {
            SUMA_S_Errv("String %s not allowed after -val_at\n",
                        argv[kar]);
            exit(1);
         }
         kar ++;
         Opt->wL = atof(argv[kar]);
         brk = 1;
      }

      if (!brk && (strcmp(argv[kar], "-showhist") == 0)) {
			Opt->DO_r = TRUE;
         brk = 1;
		}
      if (!brk && (strcmp(argv[kar], "-ignore_out") == 0)) {
			Opt->smode = 1;
         brk = 1;
		}

      if (!brk && (strcmp(argv[kar], "-thishist") == 0)) {
         kar ++;
			if (kar >= argc)  {
		  		fprintf (stderr, "need argument after -thishist \n");
				exit (1);
			}
			Opt->proot = argv[kar];
         brk = 1;
		}

      if (!brk && (strcmp(argv[kar], "-prefix") == 0)) {
         kar ++;
			if (kar >= argc)  {
		  		fprintf (stderr, "need argument after -prefix \n");
				exit (1);
			}
         Opt->prefix = (char*)calloc(strlen(argv[kar])+20, sizeof(char));
         strcpy(Opt->prefix, argv[kar]); Opt->prefix[strlen(argv[kar])]='\0';
         brk = 1;
		}

      if (!brk && (strcmp(argv[kar], "-equalized") == 0)) {
         kar ++;
			if (kar >= argc)  {
		  		fprintf (stderr, "need argument after -equalized \n");
				exit (1);
			}
         Opt->crefix = (char*)calloc(strlen(argv[kar])+20, sizeof(char));
         strcpy(Opt->crefix, argv[kar]); Opt->crefix[strlen(argv[kar])]='\0';
         brk = 1;
		}


      if (!brk) {
			fprintf (stderr,"Option %s not understood. \n"
                         "Try -help for usage\n", argv[kar]);
			suggest_best_prog_option(argv[0], argv[kar]);
         exit (1);
		} else {
			brk = 0;
			kar ++;
		}

   }

   if (!Opt->prefix) Opt->prefix = strdup("./HistOut");
   if (Opt->uid[0]=='\0') UNIQ_idcode_fill(Opt->uid);
   if (Opt->VoxDbg > -1 && !Opt->VoxDbgOut) {
      char stmp[256];
      sprintf(stmp,"%d.GP.dbg", Opt->VoxDbg);
      Opt->VoxDbgOut = fopen(stmp,"w");
   }


   SUMA_RETURN(Opt);
}


int main(int argc, char **argv)
{
   static char FuncName[]={"3dHist"};
   SEG_OPTS *Opt=NULL;
   int ii=0;
   SUMA_HIST *hh=NULL, *rhh=NULL;
   SUMA_Boolean LocalHead = NOPE;

   SUMA_STANDALONE_INIT;
	SUMA_mainENTRY;

   SUMAg_DOv = SUMA_Alloc_DisplayObject_Struct (SUMA_MAX_DISPLAYABLE_OBJECTS);
   Opt = Hist_Default(argv,  argc);
   Opt = Hist_ParseInput (Opt, argv,  argc);

   if (!Hist_CheckOpts(Opt)) {
      ERROR_exit("Failed on option check");
   }


   if (Opt->proot) { /* get this hist */
      hh = SUMA_read_hist(Opt->proot);
   } else {
      if (Opt->ndist_name) { /* reference hist? */
         rhh = SUMA_read_hist(Opt->ndist_name);
         Opt->bias_meth = "NONE";
      }
      /* load the input data */
      if (Opt->sig_name && !(Opt->sig = Seg_load_dset( Opt->sig_name ))) {
         SUMA_S_Errv("Failed to load input %s\n", Opt->sig_name);
         exit(1);
      }

      if (Opt->mset_name) {
         if (!(Opt->mset = Seg_load_dset( Opt->mset_name ))) {
            exit(1);
         }
      }

      if (Opt->sig) {
         Opt->cmask = MaskSetup(Opt, Opt->sig, 1,
                   &(Opt->mset), &(Opt->cmask), Opt->dimcmask,
                   Opt->mask_bot, Opt->mask_top, &(Opt->cmask_count));
      }

      if (   Opt->binwidth != 0.0 || Opt->UseTmp != 0 ||
             (int)Opt->range[0] != -9876543 || (int)Opt->range[1] != -9876543 ) {
         Opt->bias_meth = "NONE";
         SUMA_S_Note("Manual mode");
         /* manual mode */
         if (rhh) {
            SUMA_S_Note("Overriding some reference histogram parameters");
            if (Opt->UseTmp > 0) rhh->K = Opt->UseTmp;
            if (Opt->binwidth != 0.0) rhh->W = Opt->binwidth;
            if ((int)Opt->range[0] != -9876543) rhh->min = Opt->range[0];
            if ((int)Opt->range[1] != -9876543) rhh->max = Opt->range[1];
         }  else {
            rhh = (SUMA_HIST *)SUMA_calloc(1, sizeof(SUMA_HIST));
            rhh->K = Opt->UseTmp;
            rhh->W = Opt->binwidth;
            if ((int)Opt->range[0] != -9876543) rhh->min = Opt->range[0];
               else rhh->min = 0.0;
            if ((int)Opt->range[1] != -9876543) rhh->max = Opt->range[1];
               else rhh->max = 0.0;
         }
         if (Opt->range[0] > Opt->range[1]) {
            SUMA_S_Err ("Bad manual range values,"
                        "both min and max must be specified");
            exit(1);
         }
      }
      /* Create the hist */
      if (!(hh = SUMA_dset_hist(Opt->sig, Opt->N_biasgroups,
                           Opt->cmask, DSET_PREFIX(Opt->sig), rhh,
                           Opt->smode, 0.0, Opt->bias_meth))) {
         SUMA_S_Errv("Failed to create histogram from %s\n",
                     DSET_PREFIX(Opt->sig));
         exit(1);
      }
      if (Opt->prefix) {
         if (!SUMA_write_hist(hh, Opt->prefix)) {
            SUMA_S_Errv("Failed to write hist to %s\n",
                        Opt->prefix);
            exit(1);
         }

      }
   }

   if (!hh) {
      SUMA_S_Err("No Hist, Can't Travel.\n");
      exit(1);
   }

   if (Opt->crefix) {
      THD_3dim_dataset *dout=NULL;
      SUMA_LH("Equalizing");
      if (!(dout = SUMA_dset_hist_equalize(Opt->sig, 0, Opt->cmask, hh))) {
         SUMA_S_Err("Failed to equalize");
         exit(1);
      }
      EDIT_dset_items( dout , ADN_prefix,  Opt->crefix,  ADN_none ) ;
      tross_Copy_History( Opt->sig , dout ) ;
      tross_Make_History( "3dHist" , argc,argv , dout ) ;

      DSET_write(dout);
      DSET_delete(dout); dout=NULL;
   }


   if (Opt->DO_r) {
      SUMA_Show_hist(hh, 0, NULL);
   }


   if (Opt->wA != -123456999.0) {
      if (!Opt->feats) {
         Opt->feats = NI_strict_decode_string_list(ALL_GETS, ";, ");
      }
      for (ii=0; ii<Opt->feats->num; ++ii) {
         if (!strcmp(Opt->feats->str[ii], "upvol")) {
            if (Opt->f1 < 0) {
               if (Opt->sig) {
                  Opt->f1 = SUMA_ABS(DSET_DX(Opt->sig)*
                                    DSET_DY(Opt->sig)*DSET_DZ(Opt->sig));
               } else {
                  SUMA_S_Warn( "Have no -voxvol in mm^3 and no dataset "
                              "from which to compute it, assuming 1mm^3");
                  Opt->f1 == 1.0;
               }

            }
            if (Opt->f1 == 0.0f) Opt->f1 = 1.0;
         }
      }
      fprintf(stdout,"At value %f:\n", Opt->wA);
      for (ii=0; ii<Opt->feats->num; ++ii) {
         if (!strcmp(Opt->feats->str[ii], "upvol")) {
            float vv;
            if (Opt->f1 < 0) {
               SUMA_S_Err( "Bad news seeting voxvol");
               exit(1);
            }
            vv = SUMA_hist_value(hh, Opt->wA, "rcdf") / 1.0e6 * Opt->f1;
            fprintf(stdout,"  %8s: %f\n",
               Opt->feats->str[ii], vv);
         } else {
            fprintf(stdout,"  %8s: %f\n",
               Opt->feats->str[ii],
               SUMA_hist_value(hh, Opt->wA, Opt->feats->str[ii]));
         }
      }
   }

   if (Opt->wL != -123456999.0) {
      if (!Opt->this_xset_name || !strcmp(Opt->this_xset_name, "rcdf")) {
         if (Opt->verbose)
            fprintf(stdout,"Val: %f at %s: %f\n",
                     SUMA_val_at_count(hh, Opt->wL, 0, 1),
                     Opt->this_xset_name, Opt->wL);
         else
            fprintf(stdout,"%f\n", SUMA_val_at_count(hh, Opt->wL, 0, 1));
      } else if (!strcmp(Opt->this_xset_name, "cdf")) {
         if (Opt->verbose)
            fprintf(stdout,"Val: %f at %s: %f\n",
                     SUMA_val_at_count(hh, Opt->wL, 0, 0),
                     Opt->this_xset_name, Opt->wL);
         else
            fprintf(stdout,"%f\n", SUMA_val_at_count(hh, Opt->wL, 0, 0));
      } else if (!strcmp(Opt->this_xset_name, "ncdf")) {
         if (Opt->verbose)
            fprintf(stdout,"Val: %f at %s: %f\n",
                     SUMA_val_at_count(hh, Opt->wL, 1, 0),
                     Opt->this_xset_name, Opt->wL);
         else
            fprintf(stdout,"%f\n", SUMA_val_at_count(hh, Opt->wL, 1, 0));
      } else if (!strcmp(Opt->this_xset_name, "nrcdf")) {
         if (Opt->verbose)
            fprintf(stdout,"Val: %f at %s: %f\n",
                     SUMA_val_at_count(hh, Opt->wL, 1, 1),
                     Opt->this_xset_name, Opt->wL);
         else
            fprintf(stdout,"%f\n", SUMA_val_at_count(hh, Opt->wL, 1, 1));
      } else if (!strcmp(Opt->this_xset_name, "upvol")) {
         float voxthr;
         if (Opt->f1 < 0) {
            if (Opt->sig) {
               Opt->f1 = SUMA_ABS(DSET_DX(Opt->sig)*
                                 DSET_DY(Opt->sig)*DSET_DZ(Opt->sig));
            } else {
               SUMA_S_Err( "Have no -voxvol in mm^3 and no dataset "
                           "from which to compute it");
               exit(1);
            }
         }
         if (Opt->f1 == 0.0f) Opt->f1 = 1.0;
         if (Opt->f1 < 0) {
            SUMA_S_Err( "Bad news seeting voxvol");
            exit(1);
         }
         voxthr = SUMA_val_at_count(hh, Opt->wL*1.0e6/Opt->f1, 0, 1);
         if (Opt->verbose)
            fprintf(stdout,"Val: %f at %s: %f\n",
                     voxthr,
                     Opt->this_xset_name, Opt->wL);
         else
            fprintf(stdout,"%f\n", voxthr);
      }
   }

   /* all done, free */
   Opt = free_SegOpts(Opt);
   if (rhh) SUMA_Free_hist(rhh);
   if (hh) SUMA_Free_hist(hh);

   exit(0);
}
