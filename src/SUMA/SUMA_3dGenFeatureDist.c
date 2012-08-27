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

static HELP_OPT GenFeatureDistOptList[] = {
   {  
"-classes", 
"-classes 'CLASS_STRING': CLASS_STRING is a semicolon delimited\n"
"                         string of class labels. For example\n"
"                         -classes 'CSF; WM; GM'", 
NULL
      },
   {  
"-features", 
"-features 'FEATURES_STRING': FEATURES_STRING is a semicolon delimited\n"
"                         string of features. For example\n"
"                         -features 'MEAN.00_mm; median.19_mm; ...'", 
NULL
      },
   {  
"-sig", 
"-sig 'FEATURE_VOL1 FEATURE_VOL2 ...': Specify volumes that define\n"
"                         the features. Each sub-brick is a feature\n"
"                         and the sub-brick's name is used to name the \n"
"                         feature. Multiple volumes get catenated", 
NULL
      },
   {  
"-samp", 
"-samp 'SAMPLE_VOX1 SAMPLE_VOX2 ...': Specify which voxels belong to\n"
"                         each class of interest. Each of the volumes\n"
"                         should contain voxel values (keys) that are\n"
"                         defined in -labeltable. You can specify multiple\n"
"                         volumes, they all get catenated. Any volume can\n"
"                         contain voxels from 1 or more classes.", 
NULL
      },
   
   {  
"-prefix",
"-prefix PREF: PREF is the prefix for all output volume that are not \n"
"              debugging related.", 
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
"            automatically turned on if you provide no PREF.",
NULL
      },
   {  
"-debug",
"-debug: Debugging level",
"1"
      },

   {  
"-labeltable",
"-labeltable LT: Specify the label table",
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
   Opt->VoxDbgOut = stdout;
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
   
   RETURN(Opt);
}

int GenFeatureDist_CheckOpts(SEG_OPTS *Opt) 
{
   static char FuncName[]={"GenFeatureDist_CheckOpts"};
   int i=0;
   
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
      int kk=0;
      
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
			Opt->VoxDbgOut = fopen(argv[kar],"w");
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
			fprintf (stderr,"Option %s not understood. \n"
                         "Try -help for usage\n", argv[kar]);
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

   SUMA_RETURN(Opt);
}

int main(int argc, char **argv)
{
   static char FuncName[]={"3dGenFeatureDist"};
   SEG_OPTS *Opt=NULL;
   char *atr=NULL, sbuf[512];
   int i, j, k, n, *ifeat=NULL, key, **N_alloc=NULL, **N_ffv=NULL;
   float fsf=0.0, fsb=0.0, ***ffv=NULL, hrange[2]={-3.0, 3.0}, bwidth=0.05;
   short *sf=NULL, *sb=NULL;
   SUMA_HIST ***hh=NULL;
   
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
      int kk=0;
      
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
   
   if (!Opt->keys) {
      /* add default keys */
      SUMA_S_Note("Keys not available, assuming defaults");
      Opt->keys = (int *)calloc(Opt->clss->num, sizeof(int));
      for (i=0; i<Opt->clss->num; ++i) {
         Opt->keys[i] = i+1;
      }
   }
   
   /* Show the match between keys and classes */
   SUMA_ShowClssKeys(Opt->clss->str, Opt->clss->num, Opt->keys);
   /* For each feature, each class, collect the values */
   SUMA_S_Notev("Collecting data from %d subjects\n", Opt->sig_names->num);
   for (j=0; j<Opt->sig_names->num; ++j) { /* for each subject */
      /* load the input data */   
      if (!(Opt->sig = Seg_load_dset( Opt->sig_names->str[j] ))) {      
         exit(1);
      }
      if (Opt->debug > 1) {
         SUMA_S_Notev("Have %d sub-bricks in signatures of dude %d\n",
                   DSET_NVALS(Opt->sig), j);
      }
      
      if (!(Opt->samp = Seg_load_dset( Opt->samp_names->str[j] ))) {      
         exit(1);
      }
      if (Opt->debug > 1) {
         SUMA_S_Notev("Have %d sub-bricks in samples of dude %d\n", 
                     DSET_NVALS(Opt->samp), j);
      }
      
      if (j == 0) { /* some setup based on initial grid */
         if (!Opt->feats) { /* create features from signature */
            char *allfeats=NULL;
            for (i=0; i<DSET_NVALS(Opt->sig); ++i) {
               allfeats = 
                  SUMA_append_replace_string(allfeats,
                                          DSET_BRICK_LABEL(Opt->sig,i),";", 1);
            }
            Opt->feats = NI_strict_decode_string_list(allfeats,";, ");
            SUMA_free(allfeats); allfeats=NULL;
         } 

         SUMA_S_Notev("Have to work with %d classes, %d features\n",
                      Opt->clss->num, Opt->feats->num);

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
      
      /* collect all observations for each feature and class combination */
      ffv = (float ***)SUMA_calloc(Opt->feats->num, sizeof(float **));
      N_ffv = (int **)SUMA_calloc(Opt->feats->num, sizeof(int *));
      N_alloc = (int **)SUMA_calloc(Opt->feats->num, sizeof(int *));
      ifeat = (int *)SUMA_calloc(Opt->feats->num, sizeof(int));
      for (i=0; i<Opt->feats->num; ++i) {
         ffv[i] = (float **)calloc(Opt->clss->num, sizeof(float *));
         N_ffv[i] = (int *)SUMA_calloc(Opt->clss->num, sizeof(int));
         N_alloc[i] = (int *)SUMA_calloc(Opt->clss->num, sizeof(int));
      }
      
      /* create mapping between feature names and sub-briks */
      for (i=0; i<Opt->feats->num; ++i) {
         ifeat[i] = 0;
         while (ifeat[i] < DSET_NVALS(Opt->sig) &&
            strcmp(DSET_BRICK_LABEL(Opt->sig,ifeat[i]),
                   Opt->feats->str[i])) ++ifeat[i];
         if (ifeat[i] >= DSET_NVALS(Opt->sig)) ifeat[i]=-1;
         if (Opt->debug > 1) {
            SUMA_S_Notev("Have feature %s in sub-brick %d\n",
                      Opt->feats->str[i], ifeat[i]);
         }
      }
      
      
      /* Now collect features for each class */
      for (j=0; j<Opt->clss->num; ++j) {
         if (Opt->debug > 1) {
            SUMA_S_Notev("Working class %s\n", Opt->clss->str[j]);
         }
         key = Opt->keys[j];
         for (k=0; k<DSET_NVALS(Opt->samp); ++k) {
            if (Opt->debug > 2) {
               SUMA_S_Notev("Looking for key %d for class %s in sb %d\n",
                  key, Opt->clss->str[j], k);
            }
            sb = (short *)DSET_ARRAY(Opt->samp,k);
            fsb = DSET_BRICK_FACTOR(Opt->samp,k);
            if (fsb == 0.0) fsb = 1.0;
            if (fsb != 1.0) {
               SUMA_S_Err("Non-integral dset, possibly.");
               exit(1);
            }
            for (n=0; n<DSET_NVOX(Opt->samp); ++n) {
               if (sb[n] == key) {
                  for (i=0; i<Opt->feats->num; ++i) {
                     if (ifeat[i]>-1) {
                        if (N_alloc[i][j] <= N_ffv[i][j]) {
                           N_alloc[i][j] += 10000;
                           ffv[i][j] = (float*)SUMA_realloc(ffv[i][j],
                                                 N_alloc[i][j]*sizeof(float));
                        }
                        sf = (short *)DSET_ARRAY(Opt->sig, ifeat[i]);
                        fsf = DSET_BRICK_FACTOR(Opt->sig,ifeat[i]);
                        if (fsf == 0.0) fsf = 1.0;
                        ffv[i][j][N_ffv[i][j]] = sf[n]*fsf; ++N_ffv[i][j];
                     }
                  }
               }  
            }
         }
      }
      DSET_delete(Opt->sig); Opt->sig=NULL;
      DSET_delete(Opt->samp); Opt->samp=NULL;
   } /* loop across all subjects */
   
   hh = (SUMA_HIST ***)SUMA_calloc(Opt->feats->num, sizeof(SUMA_HIST **));
   for (i=0; i<Opt->feats->num; ++i) {
      hh[i] = (SUMA_HIST **)SUMA_calloc(Opt->clss->num, sizeof(SUMA_HIST *));
   }
   
   /* Compute histograms && save them*/
   SUMA_S_Note("Computing histograms");
   for (j=0; j<Opt->clss->num; ++j) {
      if (N_ffv[0][j] < 10) {
         SUMA_S_Errv("Requested class %s (%d) has just %d samples.\n"
                     "Not enough to grease your pan.\n",
                     Opt->clss->str[j], Opt->keys[j], N_ffv[0][j]);
         exit(1);
      }
      for (i=0; i<Opt->feats->num; ++i) {
         sprintf(sbuf, "h(%s|%s)",Opt->feats->str[i], Opt->clss->str[j]);
         if (!(hh[i][j] = SUMA_hist(ffv[i][j], N_ffv[i][j], 0, bwidth, 
                                    hrange, sbuf, 1))) {
            SUMA_S_Errv("Failed to generate histogram for %s|%s. \n"
                        "This will cause trouble at classification.\n",
                        Opt->feats->str[i], Opt->clss->str[j])
         } else {
            if (Opt->debug > 1) SUMA_Show_hist(hh[i][j], 1, NULL);
            /* save it */
            if (!SUMA_write_hist(hh[i][j],
                     SUMA_hist_fname(Opt->proot, 
                                    Opt->feats->str[i], Opt->clss->str[j]))) {
               SUMA_S_Errv("Failed to write histog to %s\n", sbuf);
            } 
         }
      }
   }
   
   /* save the histograms */
   
   /* free everything */
   if (ffv) {
      for (i=0; i<Opt->feats->num; ++i) {
         for (j=0; j<Opt->clss->num; ++j) {
            if (ffv[i][j]) SUMA_free(ffv[i][j]);
         }
         SUMA_free(ffv[i]); 
      }
      SUMA_free(ffv); ffv=NULL;
   }
   if (N_ffv) {
      for (i=0; i<Opt->feats->num; ++i) {
         SUMA_free(N_ffv[i]);
      }
      SUMA_free(N_ffv); N_ffv=NULL;
   }
   if (N_alloc) {
      for (i=0; i<Opt->feats->num; ++i) {
         SUMA_free(N_alloc[i]);
      }
      SUMA_free(N_alloc); N_alloc=NULL;
   }
   if (ifeat) SUMA_free(ifeat); ifeat=NULL;
   
   if (hh) {
      for (i=0; i<Opt->feats->num; ++i) {
         for (j=0; j<Opt->clss->num; ++j) {
            if (hh[i][j]) hh[i][j] = SUMA_Free_hist(hh[i][j]);
         }
         SUMA_free(hh[i]);
      }
      SUMA_free(hh); hh=NULL;
   }
   
   SUMA_S_Crit("Die here\n"); exit(1);

   /*
         sprintf(sbuf,"g(%s|%s)",Opt->feats->str[i], Opt->clss->str[j]);
   */


   if (Opt->group_classes) { /* just a check */
      if (!SUMA_Regroup_classes (Opt, 
                     Opt->clss->str, Opt->clss->num, Opt->keys,
                     Opt->group_classes->str, 
                     Opt->group_classes->num, NULL,
                     NULL, NULL, NULL, 
                     NULL, NULL)) {
         ERROR_exit("Failed to determine mapping");
      }
   }
   
   
   if (Opt->sig) {
      Opt->cmask = MaskSetup(Opt, Opt->sig, 
                &(Opt->mset), &(Opt->cmask), Opt->dimcmask, 
                Opt->mask_bot, Opt->mask_top, &(Opt->cmask_count));
   }
   
   
   if (Opt->VoxDbg >= 0) {
      fprintf(Opt->VoxDbgOut, "Command:");
      for (i=0; i<argc; ++i) {
         fprintf(Opt->VoxDbgOut, "%s ", argv[i]);
      }
      fprintf(Opt->VoxDbgOut, "\nDebug info for voxel %d\n", Opt->VoxDbg);
   }
   
   /* An inportant sanity check */
   if (Opt->pset && Opt->clss->num != DSET_NVALS(Opt->pset)) {
      ERROR_exit( "Number of classes %d does not "
                  "match number of pset subricks %d\n",
                  Opt->clss->num , DSET_NVALS(Opt->pset));
   }
   
   if (!GenFeatureDist(Opt)) {
      ERROR_exit("Failed in GenFeatureDist");
   }
   
   /* write output */
   if (Opt->pset && !Opt->this_pset_name) {
      DSET_write(Opt->pset);
   }
   if (Opt->cset && !Opt->this_cset_name) {
      DSET_write(Opt->cset);
   }
   
   /* all done, free */
   Opt = free_SegOpts(Opt);
   
   PRINT_COMPILE_DATE ; exit(0);
}
