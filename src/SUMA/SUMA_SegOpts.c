#include "SUMA_suma.h"
#include "SUMA_SegOpts.h"
#include "SUMA_SegFunc.h"



SEG_OPTS *SegOpt_Struct()
{
   SEG_OPTS *Opt=NULL;
   
   ENTRY("SegOpt_Struct");
   Opt = (SEG_OPTS *)calloc(1, sizeof(SEG_OPTS));
   
   Opt->ps = NULL;
   Opt->helpfunc = NULL;
   Opt->aset_name = NULL;
   Opt->mset_name = NULL;
   Opt->sig_name = NULL;
   Opt->samp_name = NULL;
   Opt->gold_name = NULL;
   Opt->gold_bias_name = NULL;
   Opt->this_pset_name = NULL;
   Opt->this_cset_name = NULL;
   Opt->this_fset_name = NULL;
   Opt->this_xset_name = NULL;
   Opt->ndist_name = NULL;
   Opt->uid[0] = '\0';
   Opt->proot = NULL;
   Opt->prefix = NULL;
   Opt->gold = NULL;
   Opt->gold_bias = NULL;
   Opt->aset = NULL;
   Opt->mset = NULL;
   Opt->gset = NULL;
   Opt->sig = NULL;
   Opt->FDV = NULL;
   Opt->pset = NULL;
   Opt->cset = NULL;
   Opt->fset = NULL;
   Opt->xset = NULL;
   Opt->debug = 0;
   Opt->idbg = Opt->kdbg = Opt->jdbg = -1;
   Opt->binwidth = 0; 
   Opt->feats=NULL;
   Opt->clss=NULL;
   Opt->Other=0;
   Opt->keys=NULL;
   Opt->mixopt = NULL;
   Opt->mixfrac=NULL;
   Opt->UseTmp = 0; 
   Opt->logp = 0;
   Opt->VoxDbg = -1;
   Opt->VoxDbg3[0] = Opt->VoxDbg3[1] = Opt->VoxDbg3[2] = -1;
   Opt->VoxDbgOut = stdout;
   Opt->rescale_p = 0;
   Opt->openmp = 0;
   Opt->labeltable_name = NULL;
   Opt->smode = STORAGE_BY_BRICK;
   Opt->bias_classes = NULL;
   Opt->pweight = 0;
   Opt->N_biasgroups=0;
   Opt->bias_param = 25;
   Opt->bias_meth = "Wells";
   Opt->cmask = NULL;
   Opt->dimcmask = 0;
   Opt->cmask_count=0;
   Opt->mask_bot = 1.0;
   Opt->mask_top = -1.0;
   Opt->DO_p = FALSE;
   Opt->DO_f = FALSE;
   Opt->DO_c = FALSE;
   Opt->DO_x = FALSE;
   Opt->Writepcg_G_au = FALSE;
   
   Opt->group_classes = NULL;
   Opt->group_keys = NULL;
   
   Opt->fitmeth = 0;
   Opt->N_enhance_cset_init = 0;
   Opt->N_main = 0;
   Opt->mix_frac_floor = 0.0001;
   Opt->clust_cset_init = 0;
   
   Opt->cs = NULL;
   Opt->Gcs = NULL;
   
   Opt->B = 1.0;
   Opt->T = 1.0;
   
   Opt->na = 8.0;
   
   Opt->edge = 0.0;
   
   Opt->hist = NULL;
   
   Opt->priCgA = NULL; /* Prob. class given features */
   Opt->wA=-1.0;
   Opt->priCgAname = NULL;
   
   Opt->priCgL = NULL; /* Prob. class given location */
   Opt->wL=-1.0;
   Opt->priCgLname = NULL;
   
   Opt->priCgALL = NULL; /* Prob. class */
   Opt->priCgALLname = NULL;
   
   Opt->Bset = NULL;
   Opt->pstCgALL = NULL;
   Opt->pCgN = NULL;
   Opt->pstCgALLname = NULL;
   Opt->Bsetname = NULL;
   Opt->Split = NULL;
   
   Opt->blur_meth = SEG_BFT;
   
   Opt->ShowThisDist = NULL;
   Opt->fast = 0;
   
   Opt->sig_names = NULL;
   Opt->samp_names = NULL;
   
   Opt->N_hspec = 0;
   Opt->hspec = NULL;
   RETURN(Opt);
}

SEG_OPTS *free_SegOpts(SEG_OPTS *Opt) {
   static char FuncName[]={"free_SegOpts"};
   
   SUMA_ENTRY;
   
   if (!Opt) SUMA_RETURN(NULL);
   if (Opt->gold) DSET_delete(Opt->gold); Opt->gold = NULL;
   if (Opt->gold_bias) DSET_delete(Opt->gold_bias); Opt->gold_bias = NULL;
   if (Opt->aset) DSET_delete(Opt->aset); Opt->aset = NULL;
   if (Opt->mset) DSET_delete(Opt->mset); Opt->mset = NULL;
   if (Opt->pset) DSET_delete(Opt->pset); Opt->pset = NULL;
   if (Opt->cset) DSET_delete(Opt->cset); Opt->cset = NULL;
   if (Opt->fset) DSET_delete(Opt->fset); Opt->fset = NULL;
   if (Opt->xset) DSET_delete(Opt->xset); Opt->xset = NULL;
   if (Opt->gset) DSET_delete(Opt->gset); Opt->gset = NULL;
   if (Opt->outl) DSET_delete(Opt->outl); Opt->outl = NULL;
   if (Opt->sig)  DSET_delete(Opt->sig); Opt->sig = NULL;
   if (Opt->priCgA)  DSET_delete(Opt->priCgA); Opt->priCgA = NULL;
   if (Opt->priCgL)  DSET_delete(Opt->priCgL); Opt->priCgL = NULL;
   if (Opt->priCgALL)  DSET_delete(Opt->priCgALL); Opt->priCgALL = NULL;
   if (Opt->feats) NI_delete_str_array(Opt->feats);Opt->feats = NULL;
   if (Opt->featsfam) NI_delete_str_array(Opt->featsfam);Opt->featsfam = NULL;
   if (Opt->feat_exp) {
      if (!Opt->clss) {
         SUMA_S_Err("This should not happen!");
      } else {
         SUMA_free2D((char**)Opt->feat_exp, Opt->clss->num);
      }
      Opt->feat_exp = NULL;
   }
   if (Opt->clss)  NI_delete_str_array(Opt->clss );Opt->clss = NULL;
   if (Opt->keys) free(Opt->keys); Opt->keys = NULL;
   if (Opt->mixfrac) free(Opt->mixfrac);Opt->mixfrac = NULL;
   if (Opt->VoxDbgOut && Opt->VoxDbgOut != stdout) fclose(Opt->VoxDbgOut);
      Opt->VoxDbgOut  = NULL;
   if (Opt->prefix) free(Opt->prefix);Opt->prefix = NULL;
   if (Opt->frefix) free(Opt->frefix);Opt->frefix = NULL;
   if (Opt->crefix) free(Opt->crefix);Opt->crefix = NULL;
   if (Opt->xrefix) free(Opt->xrefix);Opt->xrefix  = NULL;
   if (Opt->cgrefix) free(Opt->cgrefix); Opt->cgrefix = NULL;
   if (Opt->pgrefix) free(Opt->pgrefix); Opt->pgrefix = NULL;

   if (Opt->group_classes) NI_delete_str_array(Opt->group_classes) ;
      Opt->group_classes = NULL;
   if (Opt->group_keys) free(Opt->group_keys); Opt->group_keys = NULL;
   if (Opt->cs) Opt->cs = SUMA_Free_Class_Stat(Opt->cs);  
   if (Opt->Gcs) Opt->cs = SUMA_Free_Class_Stat(Opt->Gcs);  
   if (Opt->hist) free(Opt->hist); Opt->hist=NULL;
   if (Opt->Split) free(Opt->Split); Opt->Split=NULL;
   if (Opt->samp_names) 
      Opt->samp_names = SUMA_free_NI_str_array(Opt->samp_names);
   if (Opt->sig_names) 
      Opt->sig_names = SUMA_free_NI_str_array(Opt->sig_names);
   if (Opt->sig_name) SUMA_free(Opt->sig_name);
   if (Opt->N_hspec && Opt->hspec) {
      int ii;
      for (ii=0; ii<Opt->N_hspec; ++ii) {
         SUMA_Free_hist(Opt->hspec[ii]);
      }
   }
   if (Opt->hspec) SUMA_free(Opt->hspec); Opt->hspec = NULL;
   Opt->N_hspec = 0;
   
   Opt->FDV = SUMA_free_dists(Opt->FDV);
   
   free(Opt); Opt = NULL;
   SUMA_RETURN(NULL);
}


SEG_OPTS *Seg_ParseInput (SEG_OPTS *Opt, char *argv[], int argc)
{
   static char FuncName[]={"Seg_ParseInput"}; 
   int kar, i, ind, exists;
   char *outname, cview[10];
   int brk = 0;

   ENTRY("Seg_ParseInput");
   
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
      
      if (!brk && (strcmp(argv[kar], "-do") == 0)) {
         kar ++;
			if (kar >= argc)  {
		  		fprintf (stderr, "need argument after -do \n");
				exit (1);
			}
			if (strchr(argv[kar], 'c')) Opt->DO_c = 1;
			if (strchr(argv[kar], 'f')) Opt->DO_f = 1;
			if (strchr(argv[kar], 'x')) Opt->DO_x = 1;
			if (strchr(argv[kar], 'p')) Opt->DO_p = 1;
         
         brk = 1;
		}      

      if (!brk && (strcmp(argv[kar], "-L2") == 0)) {
			Opt->fitmeth = SEG_LSQFIT;
         brk = 1;
		}      

      if (!brk && (strcmp(argv[kar], "-L1") == 0)) {
			Opt->fitmeth = SEG_L1FIT;
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

      if (!brk && (strcmp(argv[kar], "-pweight") == 0)) {
			Opt->pweight = 1;
         brk = 1;
		}      

      if (!brk && (strcmp(argv[kar], "-no_pweight") == 0)) {
			Opt->pweight = 0;
         brk = 1;
		}      

      if (!brk && (strcmp(argv[kar], "-no_edge") == 0)) {
			Opt->edge = 0;
         brk = 1;
		}      

      if (!brk && (strcmp(argv[kar], "-edge") == 0)) {
			Opt->edge = 1;
         brk = 1;
		}
      
      if (!brk && (strcmp(argv[kar], "-edge1") == 0)) {
			Opt->edge = 1;
         brk = 1;
		}      

      if (!brk && (strcmp(argv[kar], "-edge2") == 0)) {
			Opt->edge = 2;
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
            if (  argv[kar  ][0]!='-' && 
                  argv[kar+1][0]!='-' && 
                  argv[kar+2][0]!='-' &&
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
      
      if (strcmp(argv[kar],"-logp") == 0 ) {
         Opt->logp = 1;
         brk = 1;
      }
      
      if (strcmp(argv[kar],"-p") == 0 ) {
         Opt->logp = 0;
         brk = 1;
      }
      
      if( strcmp(argv[kar],"-use_tmp") == 0 ){
         Opt->UseTmp = 1 ;
         brk = 1;
      }

      if( strcmp(argv[kar],"-no_tmp") == 0 ){
         Opt->UseTmp = 0 ;
         brk = 1;
      }
      
      if (!brk && (strcmp(argv[kar], "-vox_debug") == 0)) {
         kar ++;
			if (kar+2 >= argc)  {
		  		fprintf (stderr, "need 3 arguments after -vox_debug \n");
				exit (1);
			}
			Opt->idbg = atoi(argv[kar]); ++kar;
         Opt->jdbg = atoi(argv[kar]); ++kar;
         Opt->kdbg = atoi(argv[kar]);
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
			Opt->sig_name = SUMA_copy_string(argv[kar]);
         brk = 1;
		}
      
      if (!brk && (strcmp(argv[kar], "-pset") == 0)) {
         kar ++;
			if (kar >= argc)  {
		  		fprintf (stderr, "need argument after -pset \n");
				exit (1);
			}
			Opt->this_pset_name = argv[kar];
         brk = 1;
		}
      
      if (!brk && (strcmp(argv[kar], "-gold") == 0)) {
         kar ++;
			if (kar >= argc)  {
		  		fprintf (stderr, "need argument after -gold \n");
				exit (1);
			}
			Opt->gold_name = argv[kar];
         brk = 1;
		}

      if (!brk && (strcmp(argv[kar], "-gold_bias") == 0)) {
         kar ++;
			if (kar >= argc)  {
		  		fprintf (stderr, "need argument after -gold_bias \n");
				exit (1);
			}
			Opt->gold_bias_name = argv[kar];
         brk = 1;
		}

      if (!brk && (strcmp(argv[kar], "-pstCgALL") == 0)) {
         kar ++;
			if (kar >= argc)  {
		  		fprintf (stderr, "need argument after -pstCgALL \n");
				exit (1);
			}
			Opt->pstCgALLname = argv[kar];
         brk = 1;
		}
      
      if (!brk && (strcmp(argv[kar], "-priCgL") == 0)) {
         kar ++;
			if (kar >= argc)  {
		  		fprintf (stderr, "need argument after -priCgL \n");
				exit (1);
			}
			Opt->priCgLname = argv[kar];
         brk = 1;
		}
      
      if (!brk && (strcmp(argv[kar], "-priCgALL") == 0)) {
         kar ++;
			if (kar >= argc)  {
		  		fprintf (stderr, "need argument after -priCgALL \n");
				exit (1);
			}
			Opt->priCgALLname = argv[kar];
         brk = 1;
		}
      
      if (!brk && (strcmp(argv[kar], "-wL") == 0)) {
         kar ++;
			if (kar >= argc)  {
		  		fprintf (stderr, "need argument after -wL \n");
				exit (1);
			}
			Opt->wL = atof(argv[kar]);
         if (Opt->wL < 0.0 || Opt->wL > 1.0) {
            SUMA_S_Errv("-wL must be between 0 and 1.0, have %s", argv[kar]);
            exit(1);
         }
         brk = 1;
		}
      
      if (!brk && (strcmp(argv[kar], "-priCgA") == 0)) {
         kar ++;
			if (kar >= argc)  {
		  		fprintf (stderr, "need argument after -priCgA \n");
				exit (1);
			}
			Opt->priCgAname = argv[kar];
         brk = 1;
		}
      
      if (!brk && (strcmp(argv[kar], "-wA") == 0)) {
         kar ++;
			if (kar >= argc)  {
		  		fprintf (stderr, "need argument after -wA \n");
				exit (1);
			}
			Opt->wA = atof(argv[kar]);
         if (Opt->wA < 0.0 || Opt->wA > 1.0) {
            SUMA_S_Errv("-wA must be between 0 and 1.0, have %s", argv[kar]);
            exit(1);
         }
         brk = 1;
		}
      
      if (!brk && (strcmp(argv[kar], "-cset") == 0)) {
         kar ++;
			if (kar >= argc)  {
		  		fprintf (stderr, "need argument after -cset \n");
				exit (1);
			}
			Opt->this_cset_name = argv[kar];
         brk = 1;
		}

      if (!brk && (strcmp(argv[kar], "-fset") == 0)) {
         kar ++;
			if (kar >= argc)  {
		  		fprintf (stderr, "need argument after -fset \n");
				exit (1);
			}
			Opt->this_fset_name = argv[kar];
         brk = 1;
		}
      
      if (!brk && (strcmp(argv[kar], "-xset") == 0)) {
         kar ++;
			if (kar >= argc)  {
		  		fprintf (stderr, "need argument after -xset \n");
				exit (1);
			}
			Opt->this_xset_name = argv[kar];
         brk = 1;
		}
      
      if (!brk && (strcmp(argv[kar], "-tdist") == 0)) {
         kar ++;
			if (kar >= argc)  {
		  		fprintf (stderr, "need argument after -tdist \n");
				exit (1);
			}
			Opt->ndist_name = argv[kar];
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

      if (!brk && (strcmp(argv[kar], "-sphere_hood") == 0)) {
         kar ++;
			if (kar >= argc)  {
		  		fprintf (stderr, "need argument after -sphere_hood \n");
				exit (1);
			}
			Opt->na = atof(argv[kar]);
         brk = 1;
		} 
      
      if (!brk && (strcmp(argv[kar], "-blur_meth") == 0)) {
         kar ++;
			if (kar >= argc)  {
		  		fprintf (stderr, "need argument after -blur_meth \n");
				exit (1);
			}
			if (!strcmp(argv[kar],"BIM")) Opt->blur_meth = SEG_BIM;
         else if (!strncmp(argv[kar],"LS",2)) Opt->blur_meth = SEG_LSB;
         else if (!strcmp(argv[kar],"BNN")) Opt->blur_meth = SEG_BNN;
         else if (!strcmp(argv[kar],"BFT")) Opt->blur_meth = SEG_BFT;
         else {
            SUMA_S_Errv("-blur_meth %s not valid\n", argv[kar]);
            exit(1);
         }
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
      
      if (!brk && (strcmp(argv[kar], "-pprefix") == 0)) {
         kar ++;
			if (kar >= argc)  {
		  		fprintf (stderr, "need argument after -pprefix \n");
				exit (1);
			}
			Opt->smode = storage_mode_from_filename(argv[kar]);
         Opt->prefix = (char*)calloc(strlen(argv[kar])+20, sizeof(char));
         sprintf(Opt->prefix,"%s", argv[kar]);
         brk = 1;
		}
      
      if (!brk && (strcmp(argv[kar], "-fprefix") == 0)) {
         kar ++;
			if (kar >= argc)  {
		  		fprintf (stderr, "need argument after -fprefix \n");
				exit (1);
			}
			Opt->smode = storage_mode_from_filename(argv[kar]);
         Opt->frefix = (char*)calloc(strlen(argv[kar])+20, sizeof(char));
         sprintf(Opt->frefix,"%s", argv[kar]);
         brk = 1;
		}
      
      if (!brk && (strcmp(argv[kar], "-cprefix") == 0)) {
         kar ++;
			if (kar >= argc)  {
		  		fprintf (stderr, "need argument after -cprefix \n");
				exit (1);
			}
			Opt->smode = storage_mode_from_filename(argv[kar]);
         Opt->crefix = (char*)calloc(strlen(argv[kar])+20, sizeof(char));
         sprintf(Opt->crefix,"%s", argv[kar]);
         brk = 1;
		}
      
      if (!brk && (strcmp(argv[kar], "-cgprefix") == 0)) {
         kar ++;
			if (kar >= argc)  {
		  		fprintf (stderr, "need argument after -cgprefix \n");
				exit (1);
			}
			Opt->smode = storage_mode_from_filename(argv[kar]);
         Opt->cgrefix = (char*)calloc(strlen(argv[kar])+20, sizeof(char));
         sprintf(Opt->cgrefix,"%s", argv[kar]);
         brk = 1;
		}
      
      if (!brk && (strcmp(argv[kar], "-pgprefix") == 0)) {
         kar ++;
			if (kar >= argc)  {
		  		fprintf (stderr, "need argument after -pgprefix \n");
				exit (1);
			}
			Opt->smode = storage_mode_from_filename(argv[kar]);
         Opt->pgrefix = (char*)calloc(strlen(argv[kar])+20, sizeof(char));
         sprintf(Opt->pgrefix,"%s", argv[kar]);
         brk = 1;
		}
      
      if (!brk && (strcmp(argv[kar], "-xprefix") == 0)) {
         kar ++;
			if (kar >= argc)  {
		  		fprintf (stderr, "need argument after -xprefix \n");
				exit (1);
			}
			Opt->smode = storage_mode_from_filename(argv[kar]);
         Opt->xrefix = (char*)calloc(strlen(argv[kar])+20, sizeof(char));
         sprintf(Opt->xrefix,"%s", argv[kar]);
         brk = 1;
		}
      
      if (!brk && (strcmp(argv[kar], "-bias_classes") == 0)) {
         kar ++;
			if (kar >= argc)  {
		  		fprintf (stderr, "need argument after -bias_classes \n");
				exit (1);
			}
			Opt->bias_classes = argv[kar];
         brk = 1;
		}
      
      if (!brk && (strcmp(argv[kar], "-group_classes") == 0)) {
         kar ++;
			if (kar >= argc)  {
		  		fprintf (stderr, "need argument after -group_classes \n");
				exit (1);
			}
			Opt->group_classes = NI_strict_decode_string_list(argv[kar] ,";");
         brk = 1;
		}

      if (!brk && (strcmp(argv[kar], "-group_keys") == 0)) {
         NI_str_array *nstr=NULL; int ii;
         kar ++;
			if (kar >= argc)  {
		  		fprintf (stderr, "need argument after -group_keys \n");
				exit (1);
			}
			if (!(nstr = NI_strict_decode_string_list(argv[kar] ,";, "))){
            ERROR_exit("Bad option %s after -group_keys", argv[kar]);
         }
         Opt->group_keys = (int *)calloc(nstr->num, sizeof(int));
         for (ii=0;ii<nstr->num; ++ii) 
            Opt->group_keys[ii] = strtol(nstr->str[ii],NULL,10);
         NI_delete_str_array(nstr);nstr=NULL;
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
      
      if (!brk && (strcmp(argv[kar], "-split_classes") == 0)) {
         NI_str_array *nstr=NULL; int ii;
         kar ++;
			if (kar >= argc)  {
		  		fprintf (stderr, "need argument after -split_classes \n");
				exit (1);
			}
			nstr = NI_strict_decode_string_list(argv[kar] ,";, ");
         Opt->Split = (int *)calloc(nstr->num+1, sizeof(int));
         for (ii=0;ii<nstr->num; ++ii) {
            Opt->Split[ii] = strtol(nstr->str[ii],NULL,10);
            if (Opt->Split[ii]<1 || Opt->Split[ii]>9) {
               SUMA_S_Errv("Bad split value of %d in %s\n", 
                           Opt->Split[ii], argv[kar]);
               exit(1);
            }
         }
         Opt->Split[nstr->num]=-1; /* plug */
         
         brk = 1;
		}

      if (!brk && (strcmp(argv[kar], "-other") == 0)) {
         Opt->Other = 1;
         brk = 1;
      }
      
      if (!brk && (strcmp(argv[kar], "-no_other") == 0)) {
         Opt->Other = 0;
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
      
      if (!brk && (strcmp(argv[kar], "-bias_order") == 0)) {
         kar ++;
			if (kar >= argc)  {
		  		fprintf (stderr, "need integer after -bias_order \n");
				exit (1);
			}
			Opt->bias_param = atof(argv[kar]);
         Opt->bias_meth = "Poly";
         brk = 1;
		}
      
      if (!brk && (strcmp(argv[kar], "-bias_fwhm") == 0)) {
         kar ++;
			if (kar >= argc)  {
		  		fprintf (stderr, "need integer after -bias_fwhm \n");
				exit (1);
			}
			Opt->bias_param = atof(argv[kar]);
         Opt->bias_meth = "Wells";
         brk = 1;
		}
      
      if (!brk && (strcmp(argv[kar], "-enhance_cset_init") == 0)) {
         kar ++;
			if (kar >= argc)  {
		  		fprintf (stderr, "need integer after -enhance_cset_init \n");
				exit (1);
			}
			Opt->N_enhance_cset_init = atoi(argv[kar]);
         SUMA_S_Err("Option not in use at the moment");
         brk = 1;
		}

      if (!brk && (strcmp(argv[kar], "-main_N") == 0)) {
         kar ++;
			if (kar >= argc)  {
		  		fprintf (stderr, "need integer after -main_N \n");
				exit (1);
			}
			Opt->N_main = atoi(argv[kar]);
         brk = 1;
		}
      
      if (!brk && (strcmp(argv[kar], "-mixfloor") == 0)) {
         kar ++;
			if (kar >= argc)  {
		  		fprintf (stderr, "need value between 0 and 1/(number of classes)"
                             " after -mixfloor \n");
				exit (1);
			}
			Opt->mix_frac_floor = atof(argv[kar]);
         brk = 1;
		}
      if (!brk && (strcmp(argv[kar], "-clust_cset_init") == 0)) {
			Opt->clust_cset_init = 1;
         brk = 1;
      }
      
      if (!brk && (strcmp(argv[kar], "-no_clust_cset_init") == 0)) {
			Opt->clust_cset_init = 0;
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
      
      if (!brk && (strcmp(argv[kar], "-mixfrac") == 0)) {
         kar ++;
			if (kar >= argc)  {
		  		fprintf (stderr, "need argument after -mixfrac \n");
				exit (1);
			}
			Opt->mixopt = argv[kar];
         brk = 1;
		}

      if (!brk && (strcmp(argv[kar], "-Bmrf") == 0)) {
         kar ++;
			if (kar >= argc)  {
		  		fprintf (stderr, "need value after -Bmrf \n");
				exit (1);
			}
			Opt->B = atof(argv[kar]);
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
   
   if (!Opt->prefix) Opt->prefix = strdup("./GenPriorsOut.p");
   if (!Opt->frefix) Opt->frefix = strdup("./GenPriorsOut.f");
   if (!Opt->xrefix) Opt->xrefix = strdup("./GenPriorsOut.x");
   if (!Opt->crefix) Opt->crefix = strdup("./GenPriorsOut.c");
   if (Opt->uid[0]=='\0') UNIQ_idcode_fill(Opt->uid);
   if (Opt->VoxDbg > -1 && !Opt->VoxDbgOut) {
      char stmp[256];
      sprintf(stmp,"%d.dbg", Opt->VoxDbg);
      Opt->VoxDbgOut = fopen(stmp,"w");
   }

   RETURN(Opt);
}

byte *MaskSetup(SEG_OPTS *Opt, THD_3dim_dataset *aset, int mask_zero_aset,
                THD_3dim_dataset **msetp, byte **cmaskp, int dimcmask, 
                float mask_bot, float mask_top, int *mcount) 
{ 
   static char FuncName[]={"MaskSetup"};
   byte *mmm=NULL;
   int ii=0, kk=0, Fixit=0;
   byte *cmask = NULL;
   THD_3dim_dataset *mset = NULL;
   float *fa=NULL;
   MRI_IMAGE *imin=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   /* ------------- Mask business -----------------*/
   if (!aset) {
      SUMA_S_Err("Null aset?");
      SUMA_RETURN(NULL);
   }
   if (cmaskp) cmask = *cmaskp;
   if (msetp) mset = *msetp;
   
   if (Opt->mset_name && !strcmp(Opt->mset_name,"VOX_DEBUG")) {
      /* Just the debugging voxel man! */
      if (Opt->VoxDbg >= 0) {
         if (Opt->VoxDbg < DSET_NVOX(aset)) {
            SUMA_S_Note("Mask consists of voxel %d only", Opt->VoxDbg);
            mmm = (byte *)calloc( DSET_NVOX(aset), sizeof(byte));
            mmm[Opt->VoxDbg]=1;
            *mcount = 1;
         } else {
            SUMA_S_Err("VoxDbg (%d) > #voxels (%d) in dset", 
                       Opt->VoxDbg, DSET_NVOX(aset));
            ERROR_exit("No reason to proceed");
         }
      } else {
         SUMA_S_Err("Used VOX_DEBUG for -mask but no -vox_debug entry.\n"
                     "Ignoring masking, all %d voxels in the dataset will\n"
                     "be used.", DSET_NVOX(aset)) ;
         ERROR_exit("Goodbye cruel world");
      }
   } else {
      if( mset == NULL ){
         mmm = NULL ;
         if( Opt->debug ) 
            INFO_message("%d voxels in the entire dataset (no mask)\n",
                        DSET_NVOX(aset)) ;
         *mcount = DSET_NVOX(aset);
      } else {
         if( DSET_NVOX(mset) != DSET_NVOX(aset) )
           ERROR_exit("Input and mask datasets are not same dimensions!\n");
         mmm = THD_makemask( mset , 0 , mask_bot, mask_top ) ;
         *mcount = THD_countmask( DSET_NVOX(mset) , mmm ) ;
         if( *mcount <= 0 ) {
            ERROR_message("No voxels in the mask!\n") ;
            SUMA_RETURN(NULL);
         }
         if( Opt->debug ) INFO_message("%d voxels in the mask\n",*mcount) ;
         DSET_delete(mset); *msetp=NULL;
      }
   }

   if( cmask != NULL ){
      SUMA_LH("Merging with cmask");
      if( dimcmask != DSET_NVOX(aset) )
        ERROR_exit("Input and cmask datasets are not same dimensions!\n");
      if( mmm != NULL ){
         for( ii=0 ; ii < DSET_NVOX(aset) ; ii++ ) 
            mmm[ii] = (mmm[ii] && cmask[ii]) ;
         free(cmask) ; *cmaskp=NULL;
         *mcount = THD_countmask( DSET_NVOX(aset) , mmm ) ;
         if( *mcount <= 0 ) {
            ERROR_message("No voxels in the mask+cmask!\n") ;
            SUMA_RETURN(NULL);
         }
         if( Opt->debug ) 
            INFO_message("%d voxels in the mask+cmask\n",*mcount) ;
      } else {
         mmm = cmask ;
         *mcount = THD_countmask( DSET_NVOX(aset) , mmm ) ;
         if( *mcount <= 0 ) {
            ERROR_message("No voxels in the cmask!\n") ;
            SUMA_RETURN(NULL);
         }
         if( Opt->debug ) INFO_message("%d voxels in the cmask\n",*mcount) ;
      }
   }
   
   /* Make sure that aset has no exact 0s that are in the mask 
      Unless had VOX_DEBUG for mask */
   if (mask_zero_aset) {
      if (Opt->mset_name && strcmp(Opt->mset_name,"VOX_DEBUG")) {
         imin = THD_extract_float_brick(0,aset) ;
         fa = MRI_FLOAT_PTR(imin);
         Fixit = 0;
         for( ii=0 ; ii < DSET_NVOX(aset) && !Fixit; ii++ ) {
            if (IN_MASK(mmm, ii) && fa[ii] == 0.0) {
               Fixit = 1; 
            }
         }
      }
      if (Fixit) {
         SUMA_LH("Have to merge mask with anat");
         if (!mmm) {
            mmm = (byte *)malloc(DSET_NVOX(aset)*sizeof(byte));
            memset(mmm, 1, sizeof(byte)*DSET_NVOX(aset));
            *mcount = DSET_NVOX(aset);
         }
         for( ii=0 ; ii < DSET_NVOX(aset); ii++ ) {
            if (IN_MASK(mmm, ii) && fa[ii] == 0.0) {
               mmm[ii] = 0; *mcount = *mcount - 1;
            }
         }
      }
      if (imin) mri_free(imin); imin = NULL; fa = NULL;
   }
   
   SUMA_RETURN(mmm);         
}

void *Seg_NI_read_file(char *fname) {
   static char FuncName[]={"Seg_NI_read_file"};
   char *niname = NULL;
   NI_stream ns = NULL;
   void *nel=NULL;
   
   SUMA_ENTRY;
   
   niname = (char *)SUMA_malloc(sizeof(char)*(strlen(fname)+10));
   
   sprintf(niname,"file:%s",fname);
   
   if (!(ns = NI_stream_open(niname, "r"))) {
      SUMA_S_Errv("Failed to open steam %s\n", niname);
      SUMA_free(niname); 
      SUMA_RETURN(nel);
   }

   nel = NI_read_element(ns,1);
   
   NI_stream_close( ns ) ; ns = NULL;
   SUMA_free(niname);
   
   SUMA_RETURN(nel);
}

int SUMA_ShortizeDset(THD_3dim_dataset **dsetp, float thisfac) {
   static char FuncName[]={"SUMA_ShortizeDset"};
   char sprefix[THD_MAX_PREFIX+10];
   int i, j;
   byte *bb=NULL;
   short *sb=NULL;
   float bbf=0.0;
   
   THD_3dim_dataset *cpset=NULL, *dset=*dsetp;
   
   SUMA_ENTRY;
   
   if (!dset) {
      SUMA_S_Err("NULL *dsetp at input!");
      SUMA_RETURN(0);
   }
   
   sprintf(sprefix, "%s.s", dset->dblk->diskptr->prefix);
   NEW_SHORTY(dset, DSET_NVALS(dset), "ss.cp", cpset);      
   for (i=0; i<DSET_NVALS(dset); ++i) {
      if (DSET_BRICK_TYPE(dset,i) == MRI_byte) {
         bb = (byte *)DSET_ARRAY(dset,i);
         sb = (short *)DSET_ARRAY(cpset,i);
         if (thisfac <= 0.0) {
            for (j=0; j<DSET_NVOX(dset); ++j) {
               sb[j] = (short)bb[j];
            }
            thisfac = DSET_BRICK_FACTOR(dset,i);
         } else {
            bbf = DSET_BRICK_FACTOR(dset,i); if (bbf == 0.0f) bbf = 1.0;
            bbf = bbf/thisfac;
            for (j=0; j<DSET_NVOX(dset); ++j) {
               sb[j] = SHORTIZE((((float)bb[j])*bbf));
            }
         }
         EDIT_BRICK_FACTOR( cpset,i,thisfac ) ;
      } else {
         EDIT_substscale_brick(cpset, i, DSET_BRICK_TYPE(dset,i), 
                            DSET_ARRAY(dset,i), MRI_short, thisfac);
         if (DSET_BRICK_TYPE(dset,i) != MRI_short) {
            DSET_FREE_ARRAY(dset, i);
         } else {
            DSET_NULL_ARRAY(dset, i);
         }
      }
   }
   /* preserve tables, if any */
   THD_copy_labeltable_atr( cpset->dblk,  dset->dblk); 
   DSET_delete(dset); dset = NULL; 
   *dsetp=cpset;

   SUMA_RETURN(1);
}
   
THD_3dim_dataset *Seg_load_dset( char *set_name  ) {
   return(Seg_load_dset_eng(set_name, NULL));
}

THD_3dim_dataset *Seg_load_dset_eng( char *set_name, char *view ) 
{
   static char FuncName[]={"Seg_load_dset_eng"};
   THD_3dim_dataset *dset=NULL, *sdset=NULL;
   int i=0;
   byte make_cp=0;
   int verb=0;
   char sprefix[THD_MAX_PREFIX+10], *stmp=NULL;
   
   SUMA_ENTRY;
   
   dset = THD_open_dataset( set_name );
   if( !ISVALID_DSET(dset) ){
     fprintf(stderr,"**ERROR: can't open dataset %s\n",set_name) ;
     SUMA_RETURN(NULL);
   }
   
   DSET_mallocize(dset)   ; DSET_load(dset);
   
   for (i=0; i<DSET_NVALS(dset); ++i) {
      if (DSET_BRICK_TYPE(dset,i) != MRI_short) {
         if (verb) INFO_message("Sub-brick %d in %s not of type short.\n"
                       "Creating new short copy of dset ", 
                       i, DSET_PREFIX(dset));
         make_cp=1; break;
      }
   }
   
   if (make_cp) {
      if (!SUMA_ShortizeDset(&dset, -1.0)) {
         SUMA_S_Err("**ERROR: Failed to shortize");
         SUMA_RETURN(NULL);
      }
   }
   
   if (DSET_IS_MASTERED(dset)) {
      if (verb) INFO_message("Dset is mastered, making copy...");
      stmp = SUMA_ModifyName(set_name, "append", ".cp", NULL);
      sdset = dset;
      dset = EDIT_full_copy(sdset, stmp);
      free(stmp); DSET_delete(sdset); sdset = NULL;  
   }
      
   
   if (view) {
      if (view) {
               if (!strstr(view,"orig")) 
            EDIT_dset_items(dset,ADN_view_type, VIEW_ORIGINAL_TYPE, ADN_none); 
         else  if (!strstr(view,"acpc")) 
            EDIT_dset_items(dset,ADN_view_type, VIEW_ACPCALIGNED_TYPE, ADN_none);
         else  if (!strstr(view,"tlrc")) 
            EDIT_dset_items(dset ,ADN_view_type, VIEW_TALAIRACH_TYPE, ADN_none);
         else SUMA_S_Errv("View of %s is rubbish", view);
      }
   }
   
   SUMA_RETURN(dset);
}

int Seg_ClssAndKeys_from_dset(THD_3dim_dataset *dset, 
                              NI_str_array **nstrp, int **keysp) 
{
   ATR_string *atr=NULL;
   NI_str_array *nstr=NULL; 
   int *keys=NULL;
   NI_stream ns ;
   NI_element *nel ;
   int nn , ii ;
   Dtable *dt ;
   char **la , **lb ;
   
   if (!(atr = THD_find_string_atr( dset->dblk , "VALUE_LABEL_DTABLE" )))
      return(0);
   
   if (!(nel = (NI_element *)NI_read_element_fromstring(atr->ch))) {
      return (0);
   }

   /* see if element is OK for this purpose */

   if( NI_element_type(nel) != NI_ELEMENT_TYPE ){
     NI_free_element(nel) ; return (0) ;
   }

   if( nel->vec_len    <  1         ||  /* empty element?             */
       nel->vec_filled <  1         ||  /* no data was filled in?     */
       nel->vec_num    <  2         ||  /* less than 4 columns?       */
       nel->vec_typ[0] != NI_STRING ||  /* must be String, String     */
       nel->vec_typ[1] != NI_STRING   ){

     NI_free_element(nel) ; return (0) ;
   }

   la = (char **) nel->vec[0] ;  /* first column of String */
   lb = (char **) nel->vec[1] ;  /* second column of String */

   nn = nel->vec_filled ;
   ii = rint(sqrt(2*nn+1.0l)) ;
   if( ii < 7 ) ii = 7 ; else if( ii%2 == 0 ) ii++ ;

   /* make array, insert strings */
   nstr = SUMA_NI_string_vec_to_str_array(lb, nn);
   
   /* get keys */
   keys = (int *)calloc(nn, sizeof(int));
   for( ii=0 ; ii < nn ; ii++ )
     keys[ii] = strtol(la[ii], NULL, 10);

   NI_free_element(nel) ; 
   
   if (nstrp) {*nstrp = nstr; nstr=NULL;}
   else { nstr = SUMA_free_NI_str_array(nstr); }
   
   if (keysp) { *keysp = keys; keys=NULL; }
   else { free(keys); keys=NULL; }
   
   return (1) ;               
}
