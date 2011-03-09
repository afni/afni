#define MAIN

#ifdef USE_OMP
#include <omp.h>
#endif

#include "SUMA_suma.h"
#include "thd_segtools_fNM.h"
#include "SUMA_SegOpts.h"
#include "SUMA_SegFunc.h"
#include "matrix.h"

#ifdef USE_OMP
#include "mri_blur3d_variable.c"
#include "SUMA_SegFunc.c"
#endif


static int vn=0 ;



int SUMA_SegEngine(SEG_OPTS *Opt) 
{
   
   static char FuncName[]={"SUMA_SegEngine"};
   THD_3dim_dataset *pygcbo=NULL;
   int iter=0, kk, UseK[500];
   char sinf[256];
   SUMA_Boolean LocalHead = YUP;
   
   SUMA_ENTRY;
   
#ifdef USE_OMP
#pragma omp parallel
 {
  if( LocalHead && omp_get_thread_num() == 0 )
    INFO_message("OpenMP thread count = %d",omp_get_num_threads()) ;
 }
#endif

   if (!Opt->priCgALL) {
      if ((Opt->priCgA || Opt->priCgL)) {
         if (!SUMA_MergeCpriors( Opt->cs, Opt->cmask, Opt->aset, 
                                 Opt->priCgA, Opt->wA, 
                                 Opt->priCgL, Opt->wL,
                                 &Opt->priCgALL, Opt)) {
            SUMA_S_Err("NULL Opt->priCgALL");
            SUMA_RETURN(0);
         }
      } else if ((Opt->priCgLname && !strcmp(Opt->priCgLname, "INIT_MIXFRAC")) ||
                 (Opt->priCgAname && !strcmp(Opt->priCgAname, "INIT_MIXFRAC")) ){
         SUMA_S_Note("Forcing spatial priors at initial mixing fraction");
         if (!SUMA_MergeCpriors( Opt->cs, Opt->cmask, Opt->aset, 
                                 NULL, 0.0, 
                                 NULL, 0.0,
                                 &Opt->priCgALL, Opt)) {
            SUMA_S_Err("NULL Opt->priCgALL");
            SUMA_RETURN(0);
         }
      }
      if (Opt->priCgALL && Opt->debug > 1) {
         SUMA_SEG_WRITE_DSET("priCgALLmerged", Opt->priCgALL, -1, Opt->hist);
      }
   }
      
   if (Opt->cset) {/* Hide Classes not for analysis */
      int mm;
      short *sc=NULL;
      sc= (short *)DSET_ARRAY (Opt->cset, 0);
      for (kk=0 ; kk<DSET_NVOX(Opt->cset); ++kk) {
         for (mm=0; mm<Opt->cs->N_label; ++mm) {
            if (sc[kk] == Opt->cs->keys[mm]) break;
         }
         if (mm >= Opt->cs->N_label) sc[kk] = 0;
      }
   } else {
      SUMA_S_Err("Need cset");
      SUMA_RETURN(0);
   }
   
   /* get the initial parameters pstCgALL is still null here normally 
      and priCgALL will not be used when that is the case.
      So these estimates are from cset alone */
   if (!SUMA_Class_stats( Opt->aset, Opt->cset, Opt->cmask, Opt->cmask_count,
                          Opt->pstCgALL, Opt->priCgALL, Opt->gold, 
                          Opt->cs)) {
      SUMA_S_Err("Failed in class stats");
      SUMA_RETURN(0);
   }
   if (Opt->debug) SUMA_show_Class_Stat(Opt->cs, "Class Stat At Input:\n");
   
   if (!Opt->pstCgALL) { /* Compute initial posterior distribution */
      if (!(SUMA_pst_C_giv_ALL(Opt->aset, 
                               Opt->cmask, Opt->cmask_count,
                               Opt->cs,  
                               Opt->priCgALL, Opt->pCgN, &Opt->pstCgALL, Opt))) {
         SUMA_S_Err("Failed in SUMA_pst_C_giv_ALL");
         SUMA_RETURN(0);
      }
   }
     
   if (!SUMA_Class_stats( Opt->aset, Opt->cset, Opt->cmask, Opt->cmask_count,
                          Opt->pstCgALL, Opt->priCgALL, Opt->gold, 
                          Opt->cs)) {
      SUMA_S_Err("Failed in class stats");
      SUMA_RETURN(0);
   }
   if (Opt->debug) 
      SUMA_show_Class_Stat(Opt->cs, 
                           "Posterior Weighted Class Stat At Input:\n");
   
   /* To begin iterations, we should have class stats and pstCgALL. 
      Also, need an initial cset if B > 0.0  */
   for (iter=0; iter<Opt->N_main; ++iter) {
      if (Opt->bias_param > 0) {
         if (Opt->debug > 1) 
            SUMA_S_Notev("Wells Bias field correction, FWHM %f, iteration %d\n", 
                           Opt->bias_param, iter);
         if (!strcmp(Opt->bias_meth,"Wells")) { 
            if (!(SUMA_estimate_bias_field_Wells(Opt, Opt->cmask, Opt->cs,
                                             Opt->bias_param, Opt->bias_classes,
                                             Opt->aset, Opt->pstCgALL, 
                                             &Opt->Bset ))) {
               SUMA_S_Err("Failed to estimate bias");
               SUMA_RETURN(0);
            }
         } else {
            SUMA_S_Errv("Only Wells is allowed for now, have %s\n", 
                     Opt->bias_meth);
            SUMA_RETURN(0);
         }
         
         if (!(SUMA_apply_bias_field(Opt, Opt->aset, Opt->Bset,
                                &Opt->xset))) {
            SUMA_S_Err("Failed to apply field");
            SUMA_RETURN(0);
         }
      } else {
         if (iter == 0) {
            if (Opt->debug > 1) 
               SUMA_S_Note("Skipping bias field correction");
            if (!Opt->xset) Opt->xset = EDIT_full_copy(Opt->aset, Opt->xrefix);
            if (!Opt->Bset) {
               float vv=1.0;
               NEW_SHORTY(Opt->aset,1, "ConstantField", Opt->Bset);
               if (!SUMA_InitDset(Opt->Bset, &vv, 1, Opt->cmask, 1)) {
                  SUMA_S_Err("Failed to initialize Bset");
                  SUMA_RETURN(0);
               } 
            }
         }
      }
      
      /* improve parameters based on edge energy */
      if (Opt->edge) {
         double en;
         short *cmap=NULL;
         cmap = (short *)SUMA_calloc(DSET_NVOX(Opt->aset), sizeof(short));
         en = SUMA_DsetEdgeEnergy(Opt->aset, (short *)DSET_ARRAY(Opt->cset, 0), 
                                  Opt->cmask, 
                                  Opt->fset, Opt->cs); 
         SUMA_S_Notev("Edge Enenergy, Pre MAP : %f\n", en);
         
         if (!SUMA_MAP_EdgeEnergy(  Opt->aset, Opt->cmask, Opt->fset, 
                                    Opt->cs, Opt->cset, Opt, cmap)) {
            SUMA_S_Err("Failed in MAP_EdgeEnergy");
            exit(1);
         }  
         
         en = SUMA_DsetEdgeEnergy(Opt->aset, (short *)cmap, 
                                  Opt->cmask, 
                                  Opt->fset, Opt->cs); 
         SUMA_S_Notev("Edge Enenergy, Post MAP : %f\n", en);
         SUMA_ifree(cmap);
      }
      
      
      if (Opt->B > 0) {
         if (Opt->debug > 1 && iter==0) {
            SUMA_SEG_WRITE_DSET("MAPlabel.-1", Opt->cset, -1, Opt->hist);
         }
         if (!(SUMA_MAP_labels(Opt->xset, Opt->cmask, 
                               Opt->cs, 6, &Opt->cset, 
                               &Opt->pCgN, Opt))) {
            SUMA_S_Err("Failed in SUMA_MAP_labels");
            SUMA_RETURN(0);
         }
         if (Opt->debug > 1) {
            SUMA_SEG_WRITE_DSET("MAPlabel", Opt->cset, iter, Opt->hist);
            SUMA_SEG_WRITE_DSET("pCgN", Opt->pCgN, iter, Opt->hist);
         }
         AFNI_FEED(Opt->ps->cs, "MAPlabel", iter, Opt->cset);
      }
                                  
      if (!(SUMA_pst_C_giv_ALL(Opt->xset, 
                               Opt->cmask, Opt->cmask_count,
                               Opt->cs,  
                               Opt->priCgALL, Opt->pCgN,
                               &Opt->pstCgALL, Opt))) {
         SUMA_S_Err("Failed in SUMA_pst_C_giv_ALL");
         SUMA_RETURN(0);
      }
      if (Opt->debug > 1) {
         SUMA_SEG_WRITE_DSET("pstCgALL", Opt->pstCgALL, iter, Opt->hist);
      }
      
      if (Opt->B <= 0.0f) { /* no need if B > 0 because cset is 
                              set in SUMA_MAP_labels*/
         /* update class based on max(Opt->pstCgALL) */
         if (!(SUMA_assign_classes( Opt->pstCgALL, Opt->cs, 
                                    Opt->cmask, &Opt->cset))) { 
            SUMA_S_Err("Failed in assign_classes");
            SUMA_RETURN(0);
         }
      }
      
      /* Now update class stats  */
      if (!SUMA_Class_stats(  Opt->xset, Opt->cset, 
                              Opt->cmask, Opt->cmask_count,
                              Opt->pstCgALL, Opt->priCgALL, Opt->gold, 
                              Opt->cs)) {
         SUMA_S_Err("Failed in class stats");
         SUMA_RETURN(0);
      }
   

      
      if (Opt->debug || Opt->gold || Opt->gold_bias) {
         double bad_bias_thresh, bias_bad_count;
         sprintf(sinf, "Class Stat iter %d:\n", iter);
         if (iter == Opt->N_main-1 || Opt->debug) {
            SUMA_show_Class_Stat(Opt->cs, sinf);
         }
         /* Report on bias correction */
         bad_bias_thresh = 0.06;
         if ((Opt->gold_bias && Opt->Bset) && 
             (iter == Opt->N_main-1 || Opt->debug)) {
            bias_bad_count = SUMA_CompareBiasDsets(Opt->gold_bias, Opt->Bset, 
                                 Opt->cmask, Opt->cmask_count, 
                                 bad_bias_thresh, NULL);
            SUMA_S_Notev("bad_count at thresh %f = %f%% of mask.\n",
                  bad_bias_thresh, bias_bad_count);
         }
      }
      
   }
   
   SUMA_RETURN(1);
}

SEG_OPTS *Seg_Default(char *argv[], int argc) 
{
   SEG_OPTS *Opt=NULL;
   
   ENTRY("Seg_Default");
   
   Opt = SegOpt_Struct();
   Opt->helpfunc = &Seg_usage;
   Opt->ps = SUMA_Parse_IO_Args(argc, argv, "-talk;");
   Opt->aset_name = NULL;
   Opt->mset_name = NULL;
   Opt->sig_name = NULL;
   Opt->gold_name=NULL;
   Opt->gold_bias_name=NULL;
   Opt->this_pset_name = NULL;
   Opt->this_cset_name = NULL;
   Opt->ndist_name = NULL;
   Opt->uid[0] = '\0';
   Opt->prefix = NULL;
   Opt->aset = NULL;
   Opt->mset = NULL;
   Opt->gset = NULL;
   Opt->sig = NULL;
   Opt->ndist = NULL;
   Opt->pset = NULL;
   Opt->cset = NULL;
   Opt->gold = NULL;
   Opt->gold_bias = NULL;
   Opt->bias_meth = "Wells";
   Opt->bias_param = 25;
   Opt->debug = 0;
   Opt->idbg = Opt->kdbg = Opt->jdbg = -1;
   Opt->binwidth = 0.01; /* the R function area.gam was used to pick a decent 
                            binwidth. I picked a large one where discrepancy
                            between Reference and Approximation was good. 
                            0.1 is too coarse, 0.001 is overkill*/ 
   Opt->feats=NULL;
   Opt->clss=NULL;
   Opt->Other = 0;
   Opt->keys = NULL;
   Opt->UseTmp = 1; 
   Opt->logp = 1;
   Opt->VoxDbg = -1;
   Opt->VoxDbg3[0] = Opt->VoxDbg3[1] = Opt->VoxDbg3[2] = -1;
   Opt->VoxDbgOut = stderr;
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
   Opt->group_classes = NULL;
   Opt->group_keys = NULL;
   Opt->fitmeth = SEG_LSQFIT;
   Opt->N_enhance_cset_init = 0;
   Opt->N_main = 4;
   Opt->clust_cset_init=1;
   
   Opt->B = 1.0;
   Opt->T = 1.0;
   
   Opt->edge = 0.0;
   Opt->na = 8.0;
   
   Opt->priCgA=NULL;
   Opt->wA = -1.0;
   Opt->priCgL=NULL;
   Opt->wL = -1.0;
   Opt->priCgAname=NULL;
   Opt->priCgLname=NULL;
   Opt->priCgALL=NULL;
   Opt->priCgALLname=NULL;
   
   Opt->pstCgALL=NULL;
   Opt->Bset=NULL;
   Opt->pstCgALLname = NULL;
   Opt->Bsetname = NULL;
   
   SUMA_RETURN(Opt);
}

int Seg_CheckOpts(SEG_OPTS *Opt) 
{
   static char FuncName[]={"Seg_CheckOpts"};
   
   SUMA_ENTRY;
   
  SUMA_RETURN(1);
}

int main(int argc, char **argv)
{
   static char FuncName[]={"3dSeg"};
   SEG_OPTS *Opt=NULL;
   char *atr=NULL;
   float *mixfrac= NULL;
   int i=0;
   double ff;
   SUMA_SEND_2AFNI SS2A;
   SUMA_Boolean LocalHead = NOPE;

   SUMA_STANDALONE_INIT;
	SUMA_mainENTRY;
   
   SUMAg_DOv = SUMA_Alloc_DisplayObject_Struct (SUMA_MAX_DISPLAYABLE_OBJECTS);
   
   Opt = Seg_Default(argv, argc);
   Opt = Seg_ParseInput (Opt,argv,  argc);
   Opt->hist = tross_commandline( FuncName , argc , argv ) ;
   
   if (!Seg_CheckOpts(Opt)) {
      SUMA_S_Err("Failed on option check");
      SUMA_RETURN(1);
   }
   
   /* load the input data */
   if (!(Opt->aset = Seg_load_dset( Opt->aset_name ))) {      
      SUMA_RETURN(1);
   }
   
   /* Fix VoxDbg */
   if (Opt->VoxDbg >= 0) {
      Vox1D2Vox3D(Opt->VoxDbg, 
                  DSET_NX(Opt->aset), DSET_NX(Opt->aset)*DSET_NY(Opt->aset),
                  Opt->VoxDbg3);
   } else if (Opt->VoxDbg3[0]>=0) {
      Opt->VoxDbg = Opt->VoxDbg3[0] + Opt->VoxDbg3[1]*DSET_NX(Opt->aset) +
                        Opt->VoxDbg3[2]*DSET_NX(Opt->aset)*DSET_NY(Opt->aset);
   }
   
   /* Load mask dataset */
   if (Opt->mset_name) {
      if (!(Opt->mset = Seg_load_dset( Opt->mset_name ))) {      
         SUMA_RETURN(1);
      }
   }

   /* reference classes */
   if (Opt->gold_name) {
      if (!(Opt->gold = Seg_load_dset( Opt->gold_name ))) {      
         SUMA_RETURN(1);
      }   
   }
   
   if (Opt->gold_bias_name) {
      if (!(Opt->gold_bias = Seg_load_dset( Opt->gold_bias_name ))) {      
         SUMA_RETURN(1);
      }   
   }
   
   if (!Opt->clss) {
      SUMA_S_Err("Need -classes option");
      SUMA_RETURN(1);
   } 
      
   /* Talk ? */
   if (Opt->ps->cs->talk_suma) {
      Opt->ps->cs->istream = SUMA_BRAINWRAP_LINE;
      Opt->ps->cs->afni_istream = SUMA_AFNI_STREAM_INDEX2;
      if (!SUMA_SendToAfni (Opt->ps->cs, NULL,  0)) {
         SUMA_SL_Err("Failed to initialize SUMA_SendToAfni");
         Opt->ps->cs->afni_Send = NOPE;
         Opt->ps->cs->Send = NOPE;
      } else {
         /* send in_vol to afni */
            SUMA_SL_Note("Sending anat volume to AFNI");
            SS2A.dset = Opt->aset; SS2A.at_sb=-1;
            if (!SUMA_SendToAfni(Opt->ps->cs, &SS2A, 1)) {
               SUMA_SL_Err("Failed to send volume to AFNI");
               Opt->ps->cs->afni_Send = NOPE;
            }
      }
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
   if (Opt->debug > 1) {
      SUMA_S_Note("Class-->key map");
      SUMA_ShowClssKeys(Opt->clss->str, Opt->clss->num, Opt->keys);
   }
   
   /* Mask setup */
   if (Opt->debug > 1) {
      SUMA_S_Note("MaskSetup");
   }
   Opt->cmask = MaskSetup(Opt, Opt->aset, 
                &(Opt->mset), &(Opt->cmask), Opt->dimcmask, 
                Opt->mask_bot, Opt->mask_top, &(Opt->cmask_count));
   
   if (Opt->VoxDbg >= 0) {
   SUMA_S_Note("DBG setup");
      fprintf(Opt->VoxDbgOut, "Command:");
      for (i=0; i<argc; ++i) {
         fprintf(Opt->VoxDbgOut, "%s ", argv[i]);
      }
      fprintf(Opt->VoxDbgOut, "\nDebug info for voxel %d\n", Opt->VoxDbg);
   }
   
   Opt->cs = SUMA_New_Class_Stat(Opt->clss, Opt->keys, 3, NULL);

     
   /* Load prob. of class given features */
   if (Opt->priCgAname && strcmp(Opt->priCgAname, "INIT_MIXFRAC")) {
      if (!(Opt->priCgA = Seg_load_dset(Opt->priCgAname))) {
         SUMA_S_Errv("Failed to read priCgA %s\n", Opt->priCgAname);
         SUMA_RETURN(1);
      }
      if (GRID_MISMATCH(Opt->priCgA, Opt->aset)) {
         SUMA_S_Err("All input data must have same grid (-priCgA != -aset)"); 
         SUMA_RETURN(1);   
      }
      /* Make sure dset is properly formatted */
      if (!SUMA_ShortizeProbDset(&Opt->priCgA, 
                        Opt->cs, 
                        Opt->cmask, Opt->cmask_count, 
                        Opt, &Opt->priCgA)) {
         SUMA_S_Errv("Failed to shortize priCgA %s\n", Opt->priCgAname);
         SUMA_RETURN(1);
      }
   } else {
      /* uniform probability */
   }
   
   
   /* Load prob. of class given location */
   if (Opt->priCgLname && strcmp(Opt->priCgLname, "INIT_MIXFRAC")) {
      if (!(Opt->priCgL = Seg_load_dset(Opt->priCgLname))) {
         SUMA_S_Errv("Failed to read priCgL %s\n", Opt->priCgLname);
         SUMA_RETURN(1);
      }
      if (GRID_MISMATCH(Opt->priCgL, Opt->aset)) {
         SUMA_S_Err("All input data must have same grid (-priCgL != -aset)"); 
         SUMA_RETURN(1);   
      }
      /* Make sure dset is properly formatted */
      if (!SUMA_ShortizeProbDset(&Opt->priCgL, 
                        Opt->cs, 
                        Opt->cmask, Opt->cmask_count, 
                        Opt, &Opt->priCgL)) {
         SUMA_S_Errv("Failed to shortize priCgL %s\n", Opt->priCgLname);
         SUMA_RETURN(1);
      }
   } else {
      /* uniform probability */
   }

   /* check on weights of priors */
   if (Opt->wA >= 0.0 && Opt->wL < 0.0) {
      Opt->wL = 1.0 - Opt->wA;
   } else if (Opt->wL >= 0.0 && Opt->wA < 0.0) {
      Opt->wA = 1.0 - Opt->wL;
   } else if (Opt->wA < 0.0 && Opt->wL < 0.0) { /* defaults */
      Opt->wA = 0.5; Opt->wL = 0.5;
   }
   ff = Opt->wA+Opt->wL;
   Opt->wA = Opt->wA/ff; Opt->wL = Opt->wL/ff;
   
   /* classified set ? */
   if (Opt->this_cset_name) { /* user supplied initializer */
      if (!(Opt->cset = Seg_load_dset( Opt->this_cset_name ))) {      
         SUMA_RETURN(1);
      }
   }
   
   if (!Opt->cset) {
      if (!SUMA_SegInitCset(Opt->aset, 
                            &Opt->cset, 
                            Opt->cmask, Opt->cmask_count,
                            Opt->mixopt, 
                            Opt->cs,
                            Opt)) {
         SUMA_S_Err("Failed to get initializer");
         SUMA_RETURN(1);
      }
      if (Opt->debug > 1) {
         SUMA_SEG_WRITE_DSET("classes_init", Opt->cset, -1, Opt->hist);
      }
   }
   
   /* Now add the 'OTHER' class if needed */
   if (Opt->Other) {
      if (!SUMA_AddOther( Opt->clss, &Opt->keys, 
                     Opt->cmask, Opt->cmask_count,
                     Opt->cset, Opt->pstCgALL,
                     Opt->priCgA, Opt->priCgL,
                     Opt->cs)) {
         SUMA_S_Err("Failed to add other");
         SUMA_RETURN(0);              
      }
   }

   

   /* store mixfrac in class stats */
   for (i=0;i<Opt->cs->N_label; ++i) {
      if ((ff = SUMA_mixopt_2_mixfrac(Opt->mixopt, Opt->cs->label[i], 
                                   Opt->cs->keys[i], Opt->cs->N_label,
                                   Opt->cmask, Opt->cset))<0.0) {
         SUMA_S_Err("Can't get mixfrac");
         SUMA_RETURN(1);
      }
      SUMA_set_Stat(Opt->cs, Opt->cs->label[i], "init.mix", ff); 
      SUMA_set_Stat(Opt->cs, Opt->cs->label[i], "mix", ff);
   }

      
   /* Now call the workhorse */
   if (!SUMA_SegEngine(Opt)) {
      SUMA_S_Err("Failed in SUMA_SegEngine");
      exit(1);
   }
   
   /* write output */
   if (Opt->fset && !Opt->this_fset_name) {
      tross_Append_History(Opt->fset, Opt->hist);
      DSET_write(Opt->fset);
   }
   if (Opt->xset && !Opt->this_xset_name) {
      AFNI_FEED(Opt->ps->cs, "BiasCorrect", -1, Opt->xset);
      tross_Append_History(Opt->xset, Opt->hist);
      DSET_write(Opt->xset);
   }
   if (Opt->cset) {
      SUMA_SEG_WRITE_DSET(Opt->crefix, Opt->cset, -1, Opt->hist);
      AFNI_FEED(Opt->ps->cs, "FinalClasses", -1, Opt->cset);
   }
   if (Opt->pstCgALL) {
      SUMA_SEG_WRITE_DSET(Opt->prefix, Opt->pstCgALL, -1, Opt->hist);
      AFNI_FEED(Opt->ps->cs, "pstCgALL-final", -1, Opt->pstCgALL);
   }
   
   /* all done, free */
   Opt = free_SegOpts(Opt);
  
   PRINT_COMPILE_DATE ; 
   SUMA_RETURN(0);
}
