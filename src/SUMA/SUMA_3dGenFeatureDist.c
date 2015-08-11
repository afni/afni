#define MAIN

#include "SUMA_suma.h"
#include "thd_segtools_fNM.h"
#include "SUMA_SegOpts.h"
#include "SUMA_SegFunc.h"
#include "matrix.h"

static int vn=0 ;

static void vstep_print(void)
{
   static char xx[10] = "0123456789" ;
   fprintf(stderr , "%c" , xx[vn%10] ) ;
   if( vn%10 == 9) fprintf(stderr,".") ;
   vn++ ;
}

/* If a label ends with '*', return 
the length of the string -1. That would be the 
number of characters to match. 
Otherwise, return -1 */
int SUMA_is_wild_hspec_label(char *lab) 
{
   static char FuncName[]={"SUMA_is_wild_hspec_label"};
   int nlab = 0;

   SUMA_ENTRY;

   if (!lab) SUMA_RETURN(-1);
   nlab = strlen(lab);
   if (lab[nlab-1] == '*') SUMA_RETURN(nlab-1);

   SUMA_RETURN(-1);
}

static HELP_OPT GenFeatureDistOptList[] = {
   {  
"-classes", 
"-classes 'CLASS_STRING': CLASS_STRING is a semicolon delimited\n"
"                         string of class labels. For example\n"
"                         -classes 'CSF; WM; GM'\n", 
NULL
      },
   {  
"-OTHER", 
"-OTHER: Add histograms for an 'OTHER' class that has a uniform pdf.\n", 
NULL
      },

   {  
"-no_OTHER", 
"-no_OTHER: Opposite of -OTHER.\n", 
NULL
      },

   {  
"-features", 
"-features 'FEATURES_STRING': FEATURES_STRING is a semicolon delimited\n"
"                         string of features. For example\n"
"                         -features 'MEAN.00_mm; median.19_mm; ...'\n", 
NULL
      },
   {  
"-sig", 
"-sig 'FEATURE_VOL1 FEATURE_VOL2 ...': Specify volumes that define\n"
"                         the features. Each sub-brick is a feature\n"
"                         and the sub-brick's name is used to name the \n"
"                         feature. Multiple volumes get catenated.\n"
"                         Each occurence of -sig option must be paired with\n"
"                         a -samp option. Think of each pair of '-sig, -samp'\n"
"                         options as describing data on the same voxel grid; \n"
"                         Think from the same subject. When specifying \n"
"                         training data from K subjects, you will end up using\n"
"                         K pairs of '-sig, -samp'.\n"
"                         All volumes from the kth -sig instance should have \n"
"                         the same voxel grid as each other and as that of\n"
"                         the kth -samp datasets.\n",
NULL
      },
   {  
"-samp", 
"-samp 'SAMPLE_VOX1 SAMPLE_VOX2 ...': Specify which voxels belong to\n"
"                         each class of interest. Each of the volumes\n"
"                         should contain voxel values (keys) that are\n"
"                         defined in -labeltable. You can specify multiple\n"
"                         volumes, they all get catenated. Any volume can\n"
"                         contain voxels from 1 or more classes.\n"
"                         Each occurence of -samp option must be paired with\n"
"                         a -sig option. Think of each pair of '-sig, -samp'\n"
"                         options as describing data on the same voxel grid; \n"
"                         Think from the same subject. When specifying \n"
"                         training data from K subjects, you will end up using\n"
"                         K pairs of '-sig, -samp'.\n"
"                         All volumes from the kth -samp instance should have \n"
"                         the same voxel grid as each other and as that of\n"
"                         the kth -sig datasets.\n",
NULL
      },

   {  
"-hspec",
"-hspec FEATURE MIN MAX NBINS: Set histogram parameters for feature FEATURE\n"
"                              FEATURE: String label of feature\n"
"                              MIN, MAX: Range of histogram\n"
"                              NBINS: Number of bins\n"
"        Use this option to set the histogram parameters for the features for\n"
"        the automatic parameter selection was lousy. You can specify \n"
"        for multiple features by using multiple -hspec instances. The only\n"
"        condition is that all feature labels (FEATURE) must be part of the \n"
"        set named in -features.\n" , 
NULL 
      },

   
   {  
"-prefix",
"-prefix PREF: PREF is the prefix for all output volume that are not \n"
"              debugging related.\n", 
"GenFeatDist" 
      },
   
   {  
"-ShowTheseHists",
"-ShowTheseHists HISTNAMES: Show histograms specified by HISTNAMES and quit.\n"
"              HISTNAMES can specify just one .niml.hist file or a bunch of \n"
"              them using a space, or comma separated list. \n"
"              List multiple names between quotes.\n",
NULL 
      },
   {  
"-overwrite",
"-overwrite: An option common to almost all AFNI programs. It is \n"
"            automatically turned on if you provide no PREF.\n",
NULL
      },
   {  
"-debug",
"-debug: Debugging level\n",
"1"
      },

   {  
"-labeltable",
"-labeltable LT: Specify the label table\n",
"1"
      },
   {  NULL, NULL, NULL  }
};


static char shelp_GenFeatureDist[] = {
   "3dGenFeatureDist produces hives.\n"
};

void GenFeatureDist_usage(int detail) 
{
   char *s=NULL;
      
   ENTRY("GenFeatureDist_usage");
   
   
   printf( "%s", shelp_GenFeatureDist );
   s = SUMA_OptList_string(GenFeatureDistOptList);
   printf( "%s", s );
   SUMA_free(s);
   
   EXRETURN;
}


int GenFeatureDist(SEG_OPTS *Opt) 
{
   
   ENTRY("GenFeatureDist");
   
   
   /* get the probability maps */
   if (!Opt->pset && Opt->DO_p) {
      if (!(Opt->pset = p_C_GIV_A(Opt))) {
         ERROR_message("Failed miserably");
         RETURN(0);
      }
   }
      
   /* Get the classes */
   if (!Opt->cset && Opt->crefix && Opt->DO_c) {
      if (!(SUMA_assign_classes_eng(Opt->pset, 
                           Opt->clss->str, Opt->clss->num, Opt->keys, 
                           Opt->cmask, &Opt->cset))) {
         ERROR_message("Failed aimlessly");
         RETURN(0);
      }
      EDIT_dset_items(Opt->cset, ADN_prefix, Opt->crefix, ADN_none);
      if( !THD_ok_overwrite() && THD_is_file( DSET_HEADNAME(Opt->cset) ) ){
      ERROR_exit("Output file %s already exists -- cannot continue!\n",
                  DSET_HEADNAME(Opt->cset) ) ;
      }
   }  
   
   /* group classes ? */
   if (Opt->group_classes) {
      THD_3dim_dataset *gcset=NULL;
      THD_3dim_dataset *gpset=NULL;
      if (!SUMA_Regroup_classes (Opt, 
                     Opt->clss->str, Opt->clss->num, Opt->keys,
                     Opt->group_classes->str,
                     Opt->group_classes->num,
                     Opt->group_keys, Opt->cmask,
                     Opt->pset, 
                     Opt->cset,
                     &gpset, 
                     &gcset) ) {
         ERROR_message("Failed to regroup");
         RETURN(0);
      }
      DSET_write(gpset);
      DSET_write(gcset);
   }
          
   RETURN(1);
}

SEG_OPTS *GenFeatureDist_Default(char *argv[], int argc) 
{
   SEG_OPTS *Opt=NULL;
   
   ENTRY("GenFeatureDist_Default");
   
   Opt = SegOpt_Struct();
   
   Opt->helpfunc = &GenFeatureDist_usage;
   Opt->ps = SUMA_Parse_IO_Args(argc, argv, "-talk;");
   Opt->aset_name = NULL;
   Opt->mset_name = NULL;
   Opt->sig_name = NULL;
   Opt->samp_name = NULL;
   Opt->this_pset_name = NULL;
   Opt->this_cset_name = NULL;
   Opt->ndist_name = NULL;
   Opt->uid[0] = '\0';
   Opt->prefix = NULL;
   Opt->aset = NULL;
   Opt->mset = NULL;
   Opt->gset = NULL;
   Opt->sig = NULL;
   Opt->samp = NULL;
   Opt->FDV = NULL;
   Opt->pset = NULL;
   Opt->cset = NULL;
   Opt->debug =(int)strtod(SUMA_OptList_get(
                     GenFeatureDistOptList, "-debug", "val"), NULL);
   Opt->idbg = Opt->kdbg = Opt->jdbg = -1;
   Opt->binwidth = 0.01; /* the R function area.gam was used to pick a decent 
                            binwidth. I picked a large one where discrepancy
                            between Reference and Approximation was good. 
                            0.1 is too coarse, 0.001 is overkill*/ 
   Opt->feats=Opt->clss=NULL;
   Opt->keys = NULL;
   Opt->mixfrac=NULL;
   Opt->UseTmp = 1; 
   Opt->logp = 1;
   Opt->VoxDbg = -1;
   Opt->VoxDbgOut = NULL;
   Opt->rescale_p = 1;
   Opt->openmp = 0;
   Opt->labeltable_name = NULL;
   Opt->smode = STORAGE_BY_BRICK;
   Opt->pweight = 1;
   Opt->cmask = NULL;
   Opt->dimcmask = 0;
   Opt->cmask_count=0;
   Opt->mask_bot = 1.0;
   Opt->mask_top = -1.0;
   Opt->DO_p = TRUE;
   Opt->DO_c = TRUE;
   Opt->DO_r = FALSE;
   Opt->Writepcg_G_au = FALSE;
   Opt->group_classes = NULL;
   Opt->group_keys = NULL;
   Opt->fitmeth = SEG_LSQFIT;
   Opt->proot = SUMA_OptList_get(GenFeatureDistOptList, "-prefix","val");
   Opt->cs = NULL;
   Opt->Gcs = NULL;
   Opt->Other = 1;
   
   RETURN(Opt);
}

int GenFeatureDist_CheckOpts(SEG_OPTS *Opt) 
{
   static char FuncName[]={"GenFeatureDist_CheckOpts"};
   int i=0, kk=0, nmatch;
   
   SUMA_ENTRY;
   
   if (!Opt->clss) {
      SUMA_S_Err("Need -classes option");
      SUMA_RETURN(1);
   } 
   if (!Opt->sig_names) {
      SUMA_S_Err("Need -sig option");
      SUMA_RETURN(1);
   }
   if (!Opt->samp_names) {
      SUMA_S_Err("Need -samp option");
      SUMA_RETURN(1);
   }
   if (Opt->samp_names->num != Opt->sig_names->num) {
      SUMA_S_Errv("Need as many -samp options (%d) as -sig options (%d)\n",
            Opt->samp_names->num, Opt->sig_names->num);
      SUMA_RETURN(1);
   }
   
   /* labeltable? */
   if (Opt->labeltable_name) {
      Dtable *vl_dtable=NULL;
      char *labeltable_str=NULL;
      
      /* read the table */
      if (!(labeltable_str = AFNI_suck_file( Opt->labeltable_name))) {
         ERROR_exit("Failed to read %s", Opt->labeltable_name);
      }
      if (!(vl_dtable = Dtable_from_nimlstring(labeltable_str))) {
         ERROR_exit("Could not parse labeltable");
      }
      /* make sure all classes are in the labeltable */
      for (i=0; i<Opt->clss->num; ++i) {
         if ((kk = SUMA_KeyofLabel_Dtable(vl_dtable, Opt->clss->str[i]))<0){
               ERROR_exit("Key not found in %s for %s ", 
                        Opt->labeltable_name, Opt->clss->str[i]);
         }
         if (Opt->keys) {
            if (Opt->keys[i]!=kk) {
               ERROR_exit("Key mismatch %d %d", Opt->keys[i], kk);
            }
         }   
      }   
      if (!Opt->keys) { /* get them from table */
         Opt->keys = (int *)calloc(Opt->clss->num, sizeof(int));
         for (i=0; i<Opt->clss->num; ++i) {
            if ((kk = SUMA_KeyofLabel_Dtable(vl_dtable, Opt->clss->str[i]))<0){
                  ERROR_exit("(should noy happen) Key not found in %s for %s ", 
                           Opt->labeltable_name, Opt->clss->str[i]);
            }
            Opt->keys[i] = kk;
         }
      }
      destroy_Dtable(vl_dtable); vl_dtable=NULL;
   } 
   
   
   /* Make sure any histogram specs are for a class in use */
   for (kk=0; kk<Opt->N_hspec; ++kk) {
      if ( (nmatch = SUMA_is_wild_hspec_label(Opt->hspec[kk]->label)) >= 0) {
         if (!nmatch) break;
         for (i=0; i<Opt->feats->num; ++i) {
            if (!strncmp(Opt->feats->str[i], 
                         Opt->hspec[kk]->label, nmatch)) break;
         }
      } else {
         for (i=0; i<Opt->feats->num; ++i) {
            if (!strcmp(Opt->feats->str[i], Opt->hspec[kk]->label)) break;
         }
      }
      if (i==Opt->feats->num) {
         SUMA_S_Warn("Feature %s in -hspec not found under -features and will have no impact on the output\n",
                    Opt->hspec[kk]->label);
      }
   }
   
   if (!Opt->keys) {
      /* add default keys */
      if (Opt->debug) SUMA_S_Note("Keys not available, assuming defaults");
      Opt->keys = (int *)calloc(Opt->clss->num, sizeof(int));
      for (i=0; i<Opt->clss->num; ++i) {
         Opt->keys[i] = i+1;
      }
   }
   
   /* Show the match between keys and classes */
   if (Opt->debug > 1) {
      SUMA_S_Note("Class-->key map");
      SUMA_ShowClssKeys(Opt->clss->str, Opt->clss->num, Opt->keys);
   }
   
   if( ! THD_is_directory(Opt->proot) ){
      if( mkdir( Opt->proot , THD_MKDIR_MODE ) != 0 ){
         SUMA_S_Errv("Failed to create %s\n", Opt->proot);
         SUMA_RETURN(0);
      }
   }
   

   SUMA_RETURN(1);
}

SEG_OPTS *GenFeatureDist_ParseInput (SEG_OPTS *Opt, char *argv[], int argc)
{
   static char FuncName[]={"GenFeatureDist_ParseInput"}; 
   int kar, i, ind, exists;
   char *outname, cview[10], *sbuf=NULL;
   int brk = 0;
   SUMA_GENERIC_ARGV_PARSE *ps=NULL;

   SUMA_ENTRY;
   
   brk = 0;
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
      
      if (!brk && (strcmp(argv[kar], "-talk_afni") == 0)) {
         Opt->ps->cs->talk_suma = 1;
         brk = 1;
		}      
      
      if (!brk && (strcmp(argv[kar], "-openmp") == 0)) {
			Opt->openmp = 1;
         brk = 1;
		}   
         
      if (!brk && (strcmp(argv[kar], "-no_openmp") == 0)) {
			Opt->openmp = 0;
         brk = 1;
		}      

      if (!brk && (strcmp(argv[kar], "-OTHER") == 0)) {
			Opt->Other = 1;
         brk = 1;
		}   
         
      if (!brk && (strcmp(argv[kar], "-no_OTHER") == 0)) {
			Opt->Other = 0;
         brk = 1;
		}      


      if (!brk && (strcmp(argv[kar], "-vox_debug") == 0)) {
         kar ++;
			if (kar >= argc)  {
		  		fprintf (stderr, "need 1D vox index after -vox_debug \n");
				exit (1);
			}
         if (kar+2<argc) { /* see if we have ijk */
            int iii, jjj, kkk;
            if (argv[kar][0]!='-' && argv[kar][1]!='-' && argv[kar][2]!='-' &&
                (iii = atoi(argv[kar  ])) >= 0 &&
                (jjj = atoi(argv[kar+1])) >= 0 && 
                (kkk = atoi(argv[kar+2])) >= 0 ) {
               Opt->VoxDbg3[0]=iii;
               Opt->VoxDbg3[1]=jjj;
               Opt->VoxDbg3[2]=kkk;    
               ++kar; ++kar;
            } 
         }
			if (Opt->VoxDbg3[0] < 0) {
            Opt->VoxDbg = atoi(argv[kar]);
         }
         brk = 1;
		}      

      if (!brk && (strcmp(argv[kar], "-vox_debug_file") == 0)) {
         kar ++;
			if (kar >= argc)  {
		  		fprintf (stderr, "need filename after -vox_debug_file \n");
				exit (1);
			}
			if (!strcmp(argv[kar],"-")) {
            Opt->VoxDbgOut = stdout;
         } else if (!strcmp(argv[kar],"+")) {
            Opt->VoxDbgOut = stderr;
         } else {
            Opt->VoxDbgOut = fopen(argv[kar],"w");
         }
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
      
      if (!brk && (strcmp(argv[kar], "-anat") == 0)) {
         kar ++;
			if (kar >= argc)  {
		  		fprintf (stderr, "need argument after -anat \n");
				exit (1);
			}
			Opt->aset_name = argv[kar];
         brk = 1;
		}
            
      if (!brk && (strcmp(argv[kar], "-sig") == 0)) {
         kar ++;
			if (kar >= argc)  {
		  		fprintf (stderr, "need argument after -sig \n");
				exit (1);
			}
			while (kar < argc && argv[kar][0] != '-') { 
            sbuf = 
               SUMA_append_replace_string(sbuf, argv[kar], " ", 1);
            ++kar;
         }
         if (kar < argc && argv[kar][0] == '-') --kar; /* unwind */
         Opt->sig_names = SUMA_NI_str_array(Opt->sig_names, sbuf, "add");
         SUMA_free(sbuf); sbuf = NULL;
         brk = 1;
		}
      
      if (!brk && (strcmp(argv[kar], "-samp") == 0)) {
         kar ++;
			if (kar >= argc)  {
		  		fprintf (stderr, "need argument after -samp \n");
				exit (1);
			}
			while (kar < argc && argv[kar][0] != '-') { 
            sbuf = 
               SUMA_append_replace_string(sbuf, argv[kar], " ", 1);
            ++kar;
         }
         if (kar < argc && argv[kar][0] == '-') --kar; /* unwind */
         Opt->samp_names = SUMA_NI_str_array(Opt->samp_names, sbuf, "add");
         SUMA_free(sbuf); sbuf = NULL;
         brk = 1;
		}
      
      if (!brk && (strcmp(argv[kar], "-hspec") == 0)) {
         kar ++;
			if (kar+3 >= argc)  {
		  		fprintf (stderr, "need 4 arguments after -hspec \n");
				exit (1);
			}
			Opt->hspec = (SUMA_HIST **)
                  SUMA_realloc(Opt->hspec, (Opt->N_hspec+1)*sizeof(SUMA_HIST *));
         Opt->hspec[Opt->N_hspec] = (SUMA_HIST*)SUMA_calloc(1,sizeof(SUMA_HIST));
         Opt->hspec[Opt->N_hspec]->label = SUMA_copy_string(argv[kar++]);
         Opt->hspec[Opt->N_hspec]->min = (float)strtod(argv[kar++],NULL);
         Opt->hspec[Opt->N_hspec]->max = (float)strtod(argv[kar++],NULL);
         Opt->hspec[Opt->N_hspec]->K   = (float)strtod(argv[kar],NULL);
         ++Opt->N_hspec;
         brk = 1;
		}
      
      
                  
      if (!brk && (strcmp(argv[kar], "-prefix") == 0)) {
         kar ++;
			if (kar >= argc)  {
		  		fprintf (stderr, "need argument after -prefix \n");
				exit (1);
			}
			Opt->smode = storage_mode_from_filename(argv[kar]);
         Opt->proot = argv[kar];
         Opt->prefix = (char*)calloc(strlen(argv[kar])+20, sizeof(char));
         Opt->crefix = (char*)calloc(strlen(argv[kar])+20, sizeof(char));
         Opt->pgrefix = (char*)calloc(strlen(argv[kar])+20, sizeof(char));
         Opt->cgrefix = (char*)calloc(strlen(argv[kar])+20, sizeof(char));
         Opt->frefix = (char*)calloc(strlen(argv[kar])+20, sizeof(char));
         Opt->xrefix = (char*)calloc(strlen(argv[kar])+20, sizeof(char));
         sprintf(Opt->prefix,"%s.p", argv[kar]);
         sprintf(Opt->pgrefix,"%s.pg", argv[kar]);
         sprintf(Opt->crefix,"%s.c", argv[kar]);
         sprintf(Opt->cgrefix,"%s.cg", argv[kar]);
         sprintf(Opt->frefix,"%s.f", argv[kar]);
         sprintf(Opt->xrefix,"%s.x", argv[kar]);
         brk = 1;
		}
      
      
      if (!brk && (strcmp(argv[kar], "-classes") == 0)) {
         kar ++;
			if (kar >= argc)  {
		  		fprintf (stderr, "need argument after -classes \n");
				exit (1);
			}
			Opt->clss = NI_strict_decode_string_list(argv[kar] ,";, ");
         brk = 1;
		}
      
      if (!brk && (strcmp(argv[kar], "-features") == 0)) {
         kar ++;
			if (kar >= argc)  {
		  		fprintf (stderr, "need argument after -features \n");
				exit (1);
			}
			Opt->feats = NI_strict_decode_string_list(argv[kar] ,";, ");
         brk = 1;
		}
      
      if (!brk && (strcmp(argv[kar], "-keys") == 0)) {
         NI_str_array *nstr=NULL; int ii;
         kar ++;
			if (kar >= argc)  {
		  		fprintf (stderr, "need argument after -keys \n");
				exit (1);
			}
			if (!(nstr = NI_strict_decode_string_list(argv[kar] ,";, "))){
            ERROR_exit("Bad option %s after -keys", argv[kar]);
         }
         Opt->keys = (int *)calloc(nstr->num, sizeof(int));
         for (ii=0;ii<nstr->num; ++ii) 
            Opt->keys[ii] = strtol(nstr->str[ii],NULL,10);
         NI_delete_str_array(nstr);nstr=NULL;
         brk = 1;
		}
      
      
      if (!brk && (strcmp(argv[kar], "-labeltable") == 0)) {
         kar ++;
			if (kar >= argc)  {
		  		fprintf (stderr, "need argument after -labeltable \n");
				exit (1);
			}
			Opt->labeltable_name = argv[kar];
         brk = 1;
		}

      if (!brk && (strcmp(argv[kar], "-uid") == 0)) {
         kar ++;
			if (kar >= argc)  {
		  		fprintf (stderr, "need argument after -uid \n");
				exit (1);
			}
			snprintf(Opt->uid,128,"%s",argv[kar]);
         brk = 1;
		}
      
      if (!brk && (strcmp(argv[kar], "-ShowTheseHists") == 0)) {
         kar ++;
			if (kar >= argc)  {
		  		fprintf (stderr, "need argument after -ShowTheseHists \n");
				exit (1);
			}
			{ 
            NI_str_array *hisnames=NULL;
            SUMA_HIST *hh=NULL;
            int ii=0;
            
            hisnames = NI_strict_decode_string_list(argv[kar],";, ");         
            for (ii=0; ii<hisnames->num; ++ii) {
               if ((hh = SUMA_read_hist(hisnames->str[ii]))) {
                  SUMA_Show_hist(hh, 1, NULL);
                  hh = SUMA_Free_hist(hh);
               } else {
                  SUMA_S_Errv("Hist %s not found.\n", hisnames->str[ii]);
                  exit(1); 
               }
            }
            hisnames = SUMA_free_NI_str_array(hisnames);
         }
         exit(0);
         brk = 1;
		}  
          
      if (!brk) {
			fprintf (stderr,"Option #%d %s not understood. \n"
                         "Try -help for usage\n", kar, argv[kar]);
			suggest_best_prog_option(argv[0], argv[kar]);
         exit (1);
		} else {	
			brk = 0;
			kar ++;
		}

   }
   
   if (!Opt->prefix) Opt->prefix = strdup("./GenFeatureDistOut.p");
   if (!Opt->frefix) Opt->frefix = strdup("./GenFeatureDistOut.f");
   if (!Opt->xrefix) Opt->xrefix = strdup("./GenFeatureDistOut.x");
   if (!Opt->crefix) Opt->crefix = strdup("./GenFeatureDistOut.c");
   if (Opt->uid[0]=='\0') UNIQ_idcode_fill(Opt->uid);
   if (Opt->VoxDbg > -1 && !Opt->VoxDbgOut) {
      char stmp[256];
      sprintf(stmp,"%d.GFD.dbg", Opt->VoxDbg);
      Opt->VoxDbgOut = fopen(stmp,"w");
   }
   SUMA_RETURN(Opt);
}

int main(int argc, char **argv)
{
   static char FuncName[]={"3dGenFeatureDist"};
   SEG_OPTS *Opt=NULL;
   char *atr=NULL, sbuf[512], *methods=NULL;
   int  cc, /* class counter */
        kk, /* key counter */
        aa, /* feature counter */
        nn, /* sub-brick index */
        vv, /* voxel index */
        ss, /* subjects counter */
        iii, /* dummy counter */
        nbins, /* number of bins */
        *ifeat=NULL, key, 
        **N_alloc_FCset=NULL, /* Number of values allocated for each 
                                 vector in FCset */
        **N_FCset=NULL, /* Number of filled values for each vector in FCset */ 
        N_ffalloc=0, N_ff, isneg=0,
        missfeatwarn=-1 /* Missing feature warning for some subject */;
   float fsf=0.0, fsb=0.0, 
         ***FCset=NULL, /* Table holding samples for each feature/class combo */ 
         hrange[2]={-3.0, 3.0}, bwidth1=0.05, bwidth=0.0,
         *ff=NULL;
   short *sf=NULL, *sb=NULL;
   byte **masks=NULL;
   SUMA_HIST ***hh=NULL, **hf=NULL;
   double ff_m, ff_s;   
   SUMA_Boolean LocalHead = NOPE;

   SUMA_STANDALONE_INIT;
	SUMA_mainENTRY;
   
   SUMAg_DOv = SUMA_Alloc_DisplayObject_Struct (SUMA_MAX_DISPLAYABLE_OBJECTS);
   Opt = GenFeatureDist_Default(argv,  argc);
   Opt = GenFeatureDist_ParseInput (Opt, argv,  argc);
   Opt->hist = tross_commandline( FuncName , argc , argv ) ;
   
   if (!GenFeatureDist_CheckOpts(Opt)) {
      ERROR_exit("Failed on option check");
   }
   
   /* labeltable? */
   if (Opt->labeltable_name) {
      Dtable *vl_dtable=NULL;
      char *labeltable_str=NULL;
      
      /* read the table */
      if (!(labeltable_str = AFNI_suck_file( Opt->labeltable_name))) {
         ERROR_exit("Failed to read %s", Opt->labeltable_name);
      }
      if (!(vl_dtable = Dtable_from_nimlstring(labeltable_str))) {
         ERROR_exit("Could not parse labeltable");
      }
      /* make sure all classes are in the labeltable */
      for (cc=0; cc<Opt->clss->num; ++cc) {
         if ((key = SUMA_KeyofLabel_Dtable(vl_dtable, Opt->clss->str[cc]))<0){
            ERROR_exit("Key not found in %s for %s ", 
                        Opt->labeltable_name, Opt->clss->str[cc]);
         }
         if (Opt->keys) {
            if (Opt->keys[cc]!=key) {
               ERROR_exit("Key mismatch %d %d", Opt->keys[cc], key);
            }
         }   
      }   
      if (!Opt->keys) { /* get them from table */
         Opt->keys = (int *)calloc(Opt->clss->num, sizeof(int));
         for (cc=0; cc<Opt->clss->num; ++cc) {
            if ((key = SUMA_KeyofLabel_Dtable(vl_dtable, Opt->clss->str[cc]))<0){
               ERROR_exit("(should noy happen) Key not found in %s for %s ", 
                           Opt->labeltable_name, Opt->clss->str[cc]);
            }
            Opt->keys[cc] = key;
         }
      }
      destroy_Dtable(vl_dtable); vl_dtable=NULL;
   } 
   
   if (!Opt->keys) {
      /* add default keys */
      SUMA_S_Note("Keys not available, assuming defaults");
      Opt->keys = (int *)calloc(Opt->clss->num, sizeof(int));
      for (cc=0; cc<Opt->clss->num; ++cc) {
         Opt->keys[cc] = cc+1;
      }
   }
   
   /* Show the match between keys and classes */
   SUMA_ShowClssKeys(Opt->clss->str, Opt->clss->num, Opt->keys);
   /* For each feature, each class, collect the values */
   SUMA_S_Notev("Collecting data from %d subjects\n", Opt->sig_names->num);
   
   missfeatwarn = -1;
   for (ss=0; ss<Opt->sig_names->num; ++ss) { /* for each subject */
      /* load the input data */   
      if (!(Opt->sig = Seg_load_dset( Opt->sig_names->str[ss] ))) {      
         exit(1);
      }
      if (Opt->debug > 1) {
         SUMA_S_Notev("Have %d sub-bricks in signatures of dude %d\n",
                   DSET_NVALS(Opt->sig), ss);
      }
      
      if (ss == 0) { /* some setup based on initial grid */
         if (!Opt->feats) { /* create features from signature */
            char *allfeats=NULL;
            for (nn=0; nn<DSET_NVALS(Opt->sig); ++nn) {
               allfeats = 
                  SUMA_append_replace_string(allfeats,
                                          DSET_BRICK_LABEL(Opt->sig,nn),";", 1);
            }
            Opt->feats = NI_strict_decode_string_list(allfeats,";, ");
            SUMA_free(allfeats); allfeats=NULL;
         } 

         SUMA_S_Notev("Have to work with %d classes, %d features\n",
                      Opt->clss->num, Opt->feats->num);

         SUMA_S_Note("Initializing storage");
         /* Receptacles for all observations for each feature 
            and class combination */
         FCset = (float ***)SUMA_calloc(Opt->feats->num, sizeof(float **));
         N_FCset = (int **)SUMA_calloc(Opt->feats->num, sizeof(int *));
         N_alloc_FCset = (int **)SUMA_calloc(Opt->feats->num, sizeof(int *));
         ifeat = (int *)SUMA_calloc(Opt->feats->num, sizeof(int));
         for (aa=0; aa<Opt->feats->num; ++aa) {
            FCset[aa] = (float **)calloc(Opt->clss->num, sizeof(float *));
            N_FCset[aa] = (int *)SUMA_calloc(Opt->clss->num, sizeof(int));
            N_alloc_FCset[aa] = (int *)SUMA_calloc(Opt->clss->num, sizeof(int));
         }
         masks = (byte **)SUMA_calloc(Opt->sig_names->num, sizeof (byte *));

         /* Fix VoxDbg */
         if (Opt->VoxDbg >= 0) {
            Vox1D2Vox3D(Opt->VoxDbg, 
                        DSET_NX(Opt->sig), DSET_NX(Opt->sig)*DSET_NY(Opt->sig),
                        Opt->VoxDbg3);
         } else if (Opt->VoxDbg3[0]>=0) {
            Opt->VoxDbg =  Opt->VoxDbg3[0] + 
                           Opt->VoxDbg3[1]*DSET_NX(Opt->sig) +
                           Opt->VoxDbg3[2]*DSET_NX(Opt->sig)*DSET_NY(Opt->sig);
         }
      }
      
      /* allocate for mask which will be non-zero whenever a voxel is in at least         1 mask. It will have the 1st assignment */
      masks[ss] = (byte *)SUMA_calloc(DSET_NVOX(Opt->sig), sizeof(byte));
      
      /* create mapping between feature names and sub-briks */
      for (aa=0; aa<Opt->feats->num; ++aa) {
         ifeat[aa] = 0;
         while (ifeat[aa] < DSET_NVALS(Opt->sig) &&
            strcmp(DSET_BRICK_LABEL(Opt->sig,ifeat[aa]),
                   Opt->feats->str[aa])) ++ifeat[aa];
         if (ifeat[aa] >= DSET_NVALS(Opt->sig)) ifeat[aa]=-1;
         if (Opt->debug > 1) {
            SUMA_S_Notev("Have feature %s in sub-brick %d\n",
                      Opt->feats->str[aa], ifeat[aa]);
         }
      }
      
      SUMA_S_Notev("Loading sample classes for subject #%d\n", ss);
      if (!(Opt->samp = Seg_load_dset( Opt->samp_names->str[ss] ))) {      
         exit(1);
      }
      
      if (THD_dataset_mismatch(Opt->samp, Opt->sig)) {
         SUMA_S_Err(
            "Grid mismatch between -samp [%dx%dx%d] and \n"
            "                      -sig  [%dx%dx%d] volumes for pair #%d\n", 
            DSET_NX(Opt->samp), DSET_NY(Opt->samp), DSET_NZ(Opt->samp),
            DSET_NX(Opt->sig), DSET_NY(Opt->sig), DSET_NZ(Opt->sig), ss);
         exit(1);
      }
      
      if (Opt->debug > 1) {
         SUMA_S_Notev("Have %d sub-bricks in samples of dude %d\n", 
                     DSET_NVALS(Opt->samp), ss);
      }
      
      /* Now collect features for each class */
      SUMA_S_Note("Collecting features for each class");
      for (cc=0; cc<Opt->clss->num; ++cc) {
         if (Opt->debug > 1) {
            SUMA_S_Notev("Working class %s\n", Opt->clss->str[cc]);
         }
         key = Opt->keys[cc];
         for (nn=0; nn<DSET_NVALS(Opt->samp); ++nn) {
            if (Opt->debug > 2) {
               SUMA_S_Notev("Looking for key %d for class %s in sb %d\n",
                  key, Opt->clss->str[cc], nn);
            }
            sb = (short *)DSET_ARRAY(Opt->samp,nn);
            fsb = DSET_BRICK_FACTOR(Opt->samp,nn);
            if (fsb == 0.0) fsb = 1.0;
            if (fsb != 1.0) {
               SUMA_S_Err("Non-integral dset, possibly.");
               exit(1);
            }
            for (vv=0; vv<DSET_NVOX(Opt->samp); ++vv) {
               if (sb[vv] == key) {
                  for (aa=0; aa<Opt->feats->num; ++aa) {
                     if (ifeat[aa]>-1) {
                        if (N_alloc_FCset[aa][cc] <= N_FCset[aa][cc]) {
                           N_alloc_FCset[aa][cc] += 10000;
                           FCset[aa][cc] = 
                              (float*)SUMA_realloc(FCset[aa][cc],
                                           N_alloc_FCset[aa][cc]*sizeof(float));
                        }
                        sf = (short *)DSET_ARRAY(Opt->sig, ifeat[aa]);
                        fsf = DSET_BRICK_FACTOR(Opt->sig,ifeat[aa]);
                        if (fsf == 0.0) fsf = 1.0;
                        FCset[aa][cc][N_FCset[aa][cc]] = sf[vv]*fsf; 
                        ++N_FCset[aa][cc];
                        if (!masks[ss][vv]) {
                           masks[ss][vv] = (short)key; /* fcfs */
                                       /* in case we exceed short range */
                           if (masks[ss][vv]) masks[ss][vv] = 1; 
                        }
                     } else {
                        if (missfeatwarn != ss) {
                           SUMA_S_Warnv("Feature %s not found in subject %d\n",
                                 Opt->feats->str[aa], ss);
                           missfeatwarn = ss;
                        }
                     }  
                  }
               }  
            }
         }
      }
      DSET_delete(Opt->sig); Opt->sig=NULL;
      DSET_delete(Opt->samp); Opt->samp=NULL;
   } /* loop across all subjects */
   
   /* compute histograms of features across all classes and save them */
   hf = (SUMA_HIST **)SUMA_calloc(Opt->feats->num, sizeof(SUMA_HIST *));
   SUMA_S_Note("Computing histograms of features across all classes");
   ff = NULL; N_ffalloc = 0;
   for (aa=0; aa<Opt->feats->num; ++aa) {
      N_ff=0;
      for (cc=0; cc<Opt->clss->num; ++cc) {
         N_ff += N_FCset[aa][cc]; /* more than I need because same voxel 
                                     can belong to multiple classes, but just 
                                     to be safe */
      }
      if (N_ffalloc < N_ff) {
         N_ffalloc = N_ff;
         if (ff) SUMA_free(ff); ff=NULL;
         if (!(ff = (float*)SUMA_calloc(N_ff, sizeof(float)))) {
            SUMA_S_Crit("Failed to allocate");
            exit(1);
         }
      } 
      N_ff=0; isneg = 0; ff_m=0.0;
      for (ss=0; ss<Opt->sig_names->num; ++ss) { /* Once again, unfortunately  */
         /* load the input data */   
         if (!(Opt->sig = Seg_load_dset( Opt->sig_names->str[ss] ))) {      
            exit(1);
         }
         if (Opt->debug > 1) {
            SUMA_S_Notev("Have %d sub-bricks in signatures of dude %d\n",
                      DSET_NVALS(Opt->sig), ss);
         }
         if (ifeat[aa]>-1) {
            sb = (short *)DSET_ARRAY(Opt->sig,ifeat[aa]);
            fsb = DSET_BRICK_FACTOR(Opt->sig,ifeat[aa]);
            if (fsb == 0.0) fsb = 1.0;
            for (vv=0; vv<DSET_NVOX(Opt->sig); ++vv) {
               if (masks[ss][vv]) {
                  ff[N_ff] = sb[vv]*fsb;
                  if (ff[N_ff] < 0) ++isneg;
                  ff_m += ff[N_ff];
                  ++N_ff;
               }
            }
         }
         DSET_delete(Opt->sig); Opt->sig=NULL;
      }
      ff_m /= N_ff; ff_s=0.0;
      for (iii=0; iii<N_ff; ++iii) {
          ff_s += SUMA_POW2(ff[iii]-ff_m);
      }
      ff_s = sqrt(ff_s/N_ff);
      sprintf(sbuf, "h(%s)",Opt->feats->str[aa]);
      
      /* Check if you have user specified binning specs */
      bwidth = -1; nbins = -1;
      for (iii=0; iii<Opt->N_hspec; ++iii) {
         if (!strcmp(Opt->feats->str[aa], Opt->hspec[iii]->label)) {
            hrange[0] =  Opt->hspec[iii]->min;
            hrange[1] =  Opt->hspec[iii]->max;
            nbins     =  Opt->hspec[iii]->K;
            bwidth    = 0;
            methods   =  "hands off sir";
            break;
         }
      }
      

         
      if (bwidth < 0) {
         int nmatch = -1;
         /* Check the wildcard option */
         for (iii=0; iii<Opt->N_hspec; ++iii) {
            if ((nmatch = SUMA_is_wild_hspec_label(Opt->hspec[iii]->label))>=0) {
               if (!nmatch || !strncmp(Opt->feats->str[aa], 
                                       Opt->hspec[iii]->label, nmatch)) {
                  SUMA_S_Note("Feature %s matched with hspec %s\n",
                              Opt->feats->str[aa], Opt->hspec[iii]->label);
                  hrange[0] =  Opt->hspec[iii]->min;
                  hrange[1] =  Opt->hspec[iii]->max;
                  nbins     =  Opt->hspec[iii]->K;
                  bwidth    = 0;
                  methods   =  "hands woff sir";
                  break;
               }
            }
         }
      }
      
      if (bwidth < 0) { /* no user specs found */
         nbins = 0;
         methods = "Range|OsciBinWidth";
         if ((float)isneg/(float)N_ff*100.0 > 1.0) {
            hrange[0] =  ff_m-3*ff_s;
            hrange[1] =  ff_m+3*ff_s;
            bwidth = bwidth1*ff_s;
         } else if (ff_m-3*ff_s > 0) {
            hrange[0] =  ff_m-3*ff_s;
            hrange[1] =  ff_m+3*ff_s;
            bwidth = bwidth1*ff_s;
         } else {
            hrange[0] =  0;
            hrange[1] =  6.0*ff_s/2.0;
            bwidth = bwidth1*ff_s/2.0;
         }
      }
      SUMA_S_Notev("Feature %s: mean %f, std %f\n"
                   "Hist params: [%f %f], binwidth %f\n", 
                        Opt->feats->str[aa], ff_m, ff_s,
                        hrange[0], hrange[1], bwidth);
      if (!(hf[aa] = SUMA_hist_opt(ff, N_ff, nbins, bwidth, hrange, sbuf, 1, 
                                    0.1, methods))) {
         SUMA_S_Errv("Failed to generate histogram for %s. \n"
                     "This will cause trouble at classification.\n",
                     Opt->feats->str[aa]);
      } else {
         if ((float)hf[aa]->N_ignored/(float)hf[aa]->n > 0.05) {
            SUMA_S_Warnv("For histogram %s, %.2f%% of the samples were\n"
                         "ignored for being outside the range [%f %f]\n",
                   Opt->feats->str[aa],
                   100*(float)hf[aa]->N_ignored/(float)hf[aa]->n, 
                   hf[aa]->min, hf[aa]->max);
         }
         if (Opt->debug > 1) SUMA_Show_hist(hf[aa], 1, NULL);
         /* save the histogram */
         if (!SUMA_write_hist(hf[aa],
                  SUMA_hist_fname(Opt->proot, 
                                  Opt->feats->str[aa], NULL, 0))) {
            SUMA_S_Errv("Failed to write histog to %s\n", sbuf);
         } 
      }
   }  
   if (ff) SUMA_free(ff); ff = NULL;
   
   
   /* Compute histograms of features per class && save them*/
   hh = (SUMA_HIST ***)SUMA_calloc(Opt->feats->num, sizeof(SUMA_HIST **));
   for (aa=0; aa<Opt->feats->num; ++aa) {
      /* allocate for one more to accommodate OTHER */
      hh[aa] = (SUMA_HIST **)SUMA_calloc(Opt->clss->num+1, sizeof(SUMA_HIST *));
   }

   SUMA_S_Note("Computing histograms of features per class");
   for (cc=0; cc<Opt->clss->num; ++cc) {
      if (N_FCset[0][cc] < 10) {
         SUMA_S_Errv("Requested class %s (%d) has just %d samples.\n"
                     "Not enough to grease your pan.\n",
                     Opt->clss->str[cc], Opt->keys[cc], N_FCset[0][cc]);
         exit(1);
      }
      for (aa=0; aa<Opt->feats->num; ++aa) {
         sprintf(sbuf, "h(%s|%s)",Opt->feats->str[aa], Opt->clss->str[cc]);
         hrange[0] = hf[aa]->min; hrange[1] = hf[aa]->max; 
         /* Do not optimize hist range and binwidth anymore, 
            but allow smoothing. This is needed when a particular 
            class has very few samples */
         if (!(hh[aa][cc] = SUMA_hist_opt(FCset[aa][cc], N_FCset[aa][cc], 
                                    hf[aa]->K, hf[aa]->W, 
                                    hrange, sbuf, 1,
                                    0.1, "OsciSmooth"))) {
            SUMA_S_Errv("Failed to generate histogram for %s|%s. \n"
                        "This will cause trouble at classification.\n",
                        Opt->feats->str[aa], Opt->clss->str[cc])
         } else {
            if (Opt->debug > 1) SUMA_Show_hist(hh[aa][cc], 1, NULL);
            /* save the histogram */
            if (!SUMA_write_hist(hh[aa][cc],
                     SUMA_hist_fname(Opt->proot, 
                              Opt->feats->str[aa], Opt->clss->str[cc], 0))) {
               SUMA_S_Errv("Failed to write histog to %s\n", sbuf);
            } 
         }
      }
   }
   
   if (Opt->Other) {   
      cc = Opt->clss->num;
      SUMA_S_Note("Generating OTHER class with uniform PDF");
      for (aa=0; aa<Opt->feats->num; ++aa) {
         sprintf(sbuf, "h(%s|%s)",Opt->feats->str[aa], "OTHER");
         hrange[0] = hf[aa]->min; hrange[1] = hf[aa]->max; 
         if (!(hh[aa][cc] = SUMA_hist_opt(NULL, 0, 
                                    hf[aa]->K, hf[aa]->W, 
                                    hrange, sbuf, 1,
                                    0, "handsoff"))) {
            SUMA_S_Errv("Failed to generate histogram for %s|%s. \n"
                        "This will cause trouble at classification.\n",
                        Opt->feats->str[aa], "OTHER")
         } else {
            if (Opt->debug > 1) SUMA_Show_hist(hh[aa][cc], 1, NULL);
            /* save the histogram */
            if (!SUMA_write_hist(hh[aa][cc],
                     SUMA_hist_fname(Opt->proot, 
                              Opt->feats->str[aa], "OTHER", 0))) {
               SUMA_S_Errv("Failed to write histog to %s\n", sbuf);
            } 
         }
      }
   }
    
   SUMA_S_Note("Computing Correlation matrices");
   /* L2 normalize all of FCset */
   for (cc=0; cc<Opt->clss->num; ++cc) {
      for (aa=0; aa<Opt->feats->num; ++aa) {
         THD_normalize(N_FCset[aa][cc], FCset[aa][cc]);
      }
   }
   
   {
      NI_element **CC=NULL;
      float *fm=NULL, *fn=NULL;
      NI_element *nel = NULL;
      int suc;
      
   /* Compute the correlation matrices for each class */
   CC = (NI_element **) SUMA_calloc(Opt->clss->num, sizeof(NI_element *));
   
   for(cc=0; cc<Opt->clss->num; ++cc) {
      sprintf(sbuf, "CorrMat(%s)", Opt->clss->str[cc]);
      CC[cc] = NI_new_data_element(sbuf, Opt->feats->num);
      NI_set_attribute(CC[cc],"Measure","correlation");
      atr = SUMA_NI_str_ar_2_comp_str(Opt->feats, " ; ");
      NI_set_attribute(CC[cc],"ColumnLabels", atr);SUMA_free(atr); atr = NULL;
      atr = SUMA_HistString (FuncName, argc, argv, NULL);
      NI_set_attribute(CC[cc],"CommandLine", atr);SUMA_free(atr); atr = NULL;
      for (aa=0; aa<Opt->feats->num; ++aa) {
         NI_add_column_stride ( CC[cc], NI_FLOAT, NULL, 1 );
      }
      for (aa=0; aa<Opt->feats->num; ++aa) {
         fm = (float*)CC[cc]->vec[aa];
         for (iii=0; iii<aa; ++iii) fm[iii] = 0.0; /* will fill later */
         fm[aa]=1.0;
         for (iii=aa+1; iii<Opt->feats->num; ++iii) {
            if (N_FCset[aa][cc]!=N_FCset[iii][cc]) {
               SUMA_S_Errv("Sanity check failed, %d != %d\n",
                              N_FCset[aa][cc], N_FCset[iii][cc]);
            }
            SUMA_DOTP_VEC(FCset[aa][cc], FCset[iii][cc], 
                          fm[iii], N_FCset[aa][cc], 
                          float, float);
         }
      }
      /* Now fill the remainder */
      for (aa=0; aa<Opt->feats->num; ++aa) {
         fm = (float*)CC[cc]->vec[aa];
         for (iii=0; iii<aa; ++iii) {
            fn = (float*)CC[cc]->vec[iii];
            fm[iii] = fn[aa];
         }
      }
      snprintf(sbuf, 510, "file:%s.niml.cormat", 
               SUMA_corrmat_fname(Opt->proot, Opt->clss->str[cc], 0));
      NEL_WRITE_TXH(CC[cc], sbuf, suc);
   }
   
   }
   /* free everything */
   if (FCset) {
      for (aa=0; aa<Opt->feats->num; ++aa) {
         for (cc=0; cc<Opt->clss->num; ++cc) {
            if (FCset[aa][cc]) SUMA_free(FCset[aa][cc]);
         }
         SUMA_free(FCset[aa]); 
      }
      SUMA_free(FCset); FCset=NULL;
   }
   if (N_FCset) {
      for (aa=0; aa<Opt->feats->num; ++aa) {
         SUMA_free(N_FCset[aa]);
      }
      SUMA_free(N_FCset); N_FCset=NULL;
   }
   if (N_alloc_FCset) {
      for (aa=0; aa<Opt->feats->num; ++aa) {
         SUMA_free(N_alloc_FCset[aa]);
      }
      SUMA_free(N_alloc_FCset); N_alloc_FCset=NULL;
   }
   if (ifeat) SUMA_free(ifeat); ifeat=NULL;
   
   if (hh) {
      for (aa=0; aa<Opt->feats->num; ++aa) {
         for (cc=0; cc<Opt->clss->num; ++cc) {
            if (hh[aa][cc]) hh[aa][cc] = SUMA_Free_hist(hh[aa][cc]);
         }
         SUMA_free(hh[aa]);
      }
      SUMA_free(hh); hh=NULL;
   }
   
   if (hf) {
      for (aa=0; aa<Opt->feats->num; ++aa) {
         if (hf[aa]) hf[aa] = SUMA_Free_hist(hf[aa]);
      }
      SUMA_free(hf); hf=NULL;
   }
   
   if (masks) {
      for (ss=0; ss<Opt->sig_names->num; ++ss) {
         if (masks[ss]) SUMA_free(masks[ss]);
      }
      masks[ss]=NULL;
   }
   
   SUMA_S_Notev("\n"
                "Consider running this script to examine the distributions:\n"
                "   @ExamineGenFeatDists -fdir %s -odir %s\n",
                Opt->proot, Opt->proot);
   
   /* all done, free */
   Opt = free_SegOpts(Opt);
   
   PRINT_COMPILE_DATE ; exit(0);
}
