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

static HELP_OPT SegOptList[] = {
   {  
"-anat", 
"-anat ANAT: ANAT is the volume to segment", 
NULL 
      },
   {  
"-mask", 
"-mask MASK: MASK only non-zero voxels in MASK are analyzed.\n"
"        MASK is useful when no voxelwise priors are available.\n"
"        MASK can either be a dataset or the string 'AUTO'\n"
"        which would use AFNI's automask function to create the mask.\n", 
NULL 
      },
   {  
"-blur_meth",
"-blur_meth BMETH: Set the blurring method for bias field estimation.\n"
"     -blur_meth takes one of: BFT, BIM, \n"
"             BFT: Use Fourier smoothing, masks be damned.\n"
"             BIM: Blur in mask, slower, more accurate, not necessarily \n"
"                  better bias field estimates.\n"
"             BNN: A crude blurring in mask. Faster than BIM but it does\n"
"                  not result in accurate FWHM. This option is for \n"
"                  impatient testing. Do not use it.\n"
"             LSB: Localstat moving average smoothing. Debugging only. \n"
"                  Do not use.",
"BFT" 
      },
   {  
"-bias_fwhm",
"-bias_fwhm BIAS_FWHM: The amount of blurring used when estimating the\n"
"                      field bias with the Wells method.\n"
"                      [Wells et. al. IEEE TMI 15, 4, 1997].\n"
"                      Use 0.0 to turn off bias field estimation.\n",
"25.0" 
      },
   {  
"-classes", 
"-classes 'CLASS_STRING': CLASS_STRING is a semicolon delimited\n"
"                         string of class labels. At the moment\n"
"                         CLASS_STRING can only be 'CSF; GM; WM'", 
"CSF; GM; WM"
      },
   {  
"-Bmrf",
"-Bmrf BMRF: Weighting factor controlling spatial homogeneity of the \n"
"            classifications. The larger BMRF, the more homogenious the\n"
"            classifications will be.\n"
"            See Berthod et al. Image and Vision Computing 14 (1996),\n"
"            MRFs are also used in FSL's FAST program.\n"
"            BMRF = 0.0 means no MRF, 1.0 is a start. \n"
"            Use this option if you have noisy data and no good \n"
"            voxelwise priors.",
"0.0" 
      },           
   {  
"-bias_classes",
"-bias_classes 'BIAS_CLASS_STRING': A semcolon demlimited string of \n"
"                                   classes that contribute to the \n"
"                                   estimation of the bias field.",
"'GM; WM'" 
      },
   {  
"-prefix",
"-prefix PREF: PREF is the prefix for all output volume that are not \n"
"              debugging related.", 
"Segsy" 
      },
   {  
"-overwrite",
"-overwrite: An option common to almost all AFNI programs. It is \n"
"            automatically turned on if you provide no PREF.",
NULL
      },
   {  
"-debug",
"-debug LEVEL: Set debug level to 0(default), 1, or 2 ",
NULL
      },
   {  
"-mixfrac",
"-mixfrac 'MIXFRAC': MIXFRAC sets up the volume-wide (within mask)\n"
"                    tissue fractions while initializing the \n"
"                    segmentation (see IGNORE for exception).\n"
"                    You can specify the mixing fractions\n"
"                    directly such as with '0.1 0.45 0.45', or with\n"
"                    the following special flags:\n"
"              'UNI': Equal mixing fractions \n"
"              'AVG152_BRAIN_MASK': Mixing fractions reflecting AVG152\n"
"                                   template.\n"
"              'IGNORE': Ignore mixing fraction while computing posterior\n"
"                        probabilities for all the iterations, not just at the\n"
"                        initialization as for the preceding variants",
"UNI" 
      }, 
   {  
"-mixfloor",
"-mixfloor 'FLOOR': Set the minimum value for any class's mixing fraction.\n"
"                   The value should be between 0 and 1 and not to exceed\n"
"                   1/(number of classes). This parameter should be kept to\n"
"                   a small value.",
"0.0001" 
      }, 
   {  
"-gold",
"-gold GOLD: A goldstandard segmentation volume should you wish to\n"
"               compare 3dSeg's results to it.",
NULL 
      },
   {  
"-gold_bias",
"-gold_bias GOLD: A goldstandard bias volume should you wish to\n"
"               compare 3dSeg's bias estimate to it.\n",
NULL 
      },
   {  
"-main_N",
"-main_N Niter: Number of iterations to perform.",
"5" 
      },
   {  
"-cset",
"-cset CSET: Initial classfication. If CSET is not given,\n"
"            initialization is carried out with 3dkmean's engine.\n",
NULL 
      },
   {  
"-labeltable",
"-labeltable LT: Label table containing integer keys and corresponding labels.",
NULL 
      },
   {  
"-vox_debug",
"-vox_debug 1D_DBG_INDEX: 1D index of voxel to debug.\n"
"       OR\n"
"   -vox_debug I J K: where I, J, K are the 3D voxel indices \n"
"                     (not RAI coordinates in mm).",
NULL 
      },
   {  
"-vox_debug_file",
"-vox_debug_file DBG_OUTPUT_FILE: File in which debug information is output\n"
"                                    use '-' for stdout, '+' for stderr.",
NULL 
      },
   
 {  NULL, NULL, NULL  }
};

static char shelp_Seg[] = {
"3dSeg segments brain volumes into tissue classes. The program allows\n"
"for adding a variety of global and voxelwise priors. However for the moment,\n"
"only mixing fractions and MRF are documented.\n"
"\n"
"I do not recommend you use this program for quantitative segmentation,\n"
"at least not yet. I have a lot of emotional baggage to overcome on that\n"
"front.\n" 
"\n"
"Example 1: Segmenting a skull-stripped T1 volume with:\n"
"              Brain mask, No prior volumes, Uniform mixing fraction\n"
"           3dSeg    -anat anat.nii    -mask AUTO \\\n"
"                    -classes 'CSF ; GM ; WM' -bias_classes 'GM ; WM' \\\n"
"                    -bias_fwhm 25 -mixfrac UNI -main_N 5 \\\n"
"                    -blur_meth BFT\n"   
"Options:\n"
"\n"
#if 0
"Examples: (All examples can do without the -gold* options)\n"
"  Case A: Segmenting a T1 volume with a brain mask available\n"
"  A.1:  Brain mask and MRF only.\n"
"  3dSeg    -anat banat+orig     \\\n"
"           -mask anat.ns+orig   \\\n"
"           -gold goldseg+orig   -gold_bias goldbias+orig   \\\n"
"           -classes 'CSF ; GM ; WM' \\\n"
"           -Bmrf 1.0            \\\n"
"           -bias_classes 'GM ; WM' -bias_fwhm 25 \\\n"
"           -prefix case.A.1  -overwrite    \\\n"
"\n"
"  A.2:  Adding average mixing fraction constraint derived from\n"
"        population based spatial priors, and preserving the weighting\n"
"        throughout the segmentation.\n"
"  3dSeg    -anat banat+orig     \\\n"
"           -mask anat.ns+orig   \\\n"
"           -gold goldseg+orig   -gold_bias goldbias+orig   \\\n"
"           -classes 'CSF ; GM ; WM' \\\n"
"           -bias_classes 'GM ; WM' -bias_fwhm 25 \\\n"
"           -prefix case.A.2  -overwrite    \\\n"
"           -mixfrac WHOLE_BRAIN \\\n"
"           -Bmrf 1.0 -main_N 4           \\\n"
#endif
"\n"
"\n"
};

void Seg_usage(int detail) 
{
   int i = 0;
   char *s=NULL;
   
   ENTRY("Seg_usage");
   
   printf( "%s", shelp_Seg );
   s = SUMA_OptList_string(SegOptList);
   printf( "%s", s );
   SUMA_free(s);
   
   EXRETURN;
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
   Opt->FDV = NULL;
   Opt->pset = NULL;
   Opt->cset = NULL;
   Opt->outl = NULL;
   Opt->gold = NULL;
   Opt->gold_bias = NULL;
   Opt->bias_meth = "Wells";
   Opt->bias_param = (float)strtod(
                    SUMA_OptList_get(SegOptList, "-bias_fwhm", "val"), NULL);
   Opt->debug = 0;
   Opt->idbg = Opt->kdbg = Opt->jdbg = -1;
   Opt->binwidth = 0.01; /* the R function area.gam was used to pick a decent 
                            binwidth. I picked a large one where discrepancy
                            between Reference and Approximation was good. 
                            0.1 is too coarse, 0.001 is overkill*/ 
   Opt->feats=NULL;
   Opt->clss=NI_strict_decode_string_list(
            SUMA_OptList_get(SegOptList, "-classes", "value"),";, ");
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
   Opt->DO_o = FALSE;
   Opt->DO_r = FALSE;
   Opt->Writepcg_G_au = FALSE;
   Opt->group_classes = NULL;
   Opt->group_keys = NULL;
   Opt->fitmeth = SEG_LSQFIT;
   Opt->N_enhance_cset_init = 0;
   Opt->N_main = (int)strtod(
                  SUMA_OptList_get(SegOptList, "-main_N", "val"), NULL); 
                              /* defaulted to 4 before May 7 2012 */
   Opt->clust_cset_init=1;
   
   Opt->B = 0.0;  /* defaulted to 1.0 before March 7 2012 */
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
   
   Opt->proot = SUMA_OptList_get(SegOptList, "-prefix","val");
   SUMA_RETURN(Opt);
}

int Seg_CheckOpts(SEG_OPTS *Opt) 
{
   static char FuncName[]={"Seg_CheckOpts"};
   
   SUMA_ENTRY;
   
   if( ! THD_is_directory(Opt->proot) ){
      if( mkdir( Opt->proot , THD_MKDIR_MODE ) != 0 ){
         SUMA_S_Errv("Failed to create %s\n", Opt->proot);
         exit(1);
      }
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
   
   SUMA_set_SegFunc_debug( Opt->debug, Opt->VoxDbg, Opt->VoxDbg3, 
                           Opt->VoxDbgOut);
   
   SUMA_RETURN(1);
}


int SUMA_SegEngine(SEG_OPTS *Opt) 
{
   
   static char FuncName[]={"SUMA_SegEngine"};
   THD_3dim_dataset *pygcbo=NULL;
   int iter=0, kk, UseK[500];
   char sinf[256];
   char sreport[512]={"unset_report_name.txt"};
   SUMA_Boolean LocalHead = YUP;
   
   SUMA_ENTRY;
   
#ifdef USE_OMP
#pragma omp parallel
 {
  if( LocalHead && omp_get_thread_num() == 0 )
    INFO_message("OpenMP thread count = %d",omp_get_num_threads()) ;
}
#endif
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
   
   if (!Opt->priCgALL) {
      if ((Opt->priCgA || Opt->priCgL)) {
         if (!SUMA_MergeCpriors( Opt->cs, Opt->cmask, Opt->aset, 
                                 Opt->priCgA, Opt->wA, 
                                 Opt->priCgL, Opt->wL,
                                 &Opt->priCgALL, Opt)) {
            SUMA_S_Err("NULL Opt->priCgALL");
            SUMA_RETURN(0);
         }
      } else if ((Opt->priCgLname && !strcmp(Opt->priCgLname,"INIT_MIXFRAC")) ||
                 (Opt->priCgAname && !strcmp(Opt->priCgAname,"INIT_MIXFRAC")) ){
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
         SUMA_Seg_Write_Dset(Opt->proot,"priCgALLmerged", 
                             Opt->priCgALL, -1, Opt->hist);
      }
   }
   
   /* split the classes */
   if (Opt->Split) { 
      THD_3dim_dataset *Scset=NULL;
      int N_split=0;
      SUMA_CLASS_STAT *Scs=NULL;
      SUMA_S_Warn("Splitting classes");
      while (Opt->Split[N_split] > 0) ++N_split;
      if (N_split != Opt->cs->N_label) {
         SUMA_S_Errv("Split vector malformed.\n"
                     "Have %d values in Split, but %d classes\n",
                     N_split, Opt->cs->N_label);
         SUMA_RETURN(0);
      }
      if (!SUMA_Split_Classes(Opt->cs->label, Opt->cs->N_label, Opt->cs->keys,
                              Opt->Split, Opt->aset, Opt->cset, Opt->cmask,
                              &Scset, &Scs, Opt)) {
         SUMA_S_Err("Failed to split classes");
         SUMA_RETURN(0);
      }
      /* Save old class stats and replace by split classes */
      Opt->Gcs = Opt->cs; Opt->cs = Scs; Scs=NULL;
      DSET_delete(Opt->cset); Opt->cset = Scset; Scset=NULL;
   }
      
   /* get the initial parameters pstCgALL is still null here normally 
      and priCgALL will not be used when that is the case.
      So these estimates are from cset alone */
   if (!SUMA_Class_stats( Opt->aset, Opt->cset, Opt->cmask, Opt->cmask_count,
                          Opt->pstCgALL, Opt->priCgALL, Opt->gold, 
                          Opt->cs, Opt->mix_frac_floor)) {
      SUMA_S_Err("Failed in class stats");
      SUMA_RETURN(0);
   }
   if (Opt->debug) SUMA_show_Class_Stat(Opt->cs, "Class Stat At Input:\n", NULL);
   
   /* Make sure there are good estimates for all classes */
   if (SUMA_ZeroSamp_from_ClassStat(Opt->cs)) {
      if (!Opt->debug) 
         SUMA_show_Class_Stat(Opt->cs, "Class Stat At Input:\n", NULL);
      SUMA_S_Err("Have empty classes at initialization. Not cool\n");
      SUMA_RETURN(0);
   }

   if (!Opt->pstCgALL) { /* Compute initial posterior distribution */
      if (!(SUMA_pst_C_giv_ALL(Opt->aset, 
                   Opt->cmask, Opt->cmask_count,
                   Opt->cs, Opt->priCgALL, Opt->pCgN, 
                   Opt->B, Opt->T, 
                   (!Opt->mixopt || strcmp(Opt->mixopt,"IGNORE")) ? 1:0,
                               &Opt->pstCgALL))) {
         SUMA_S_Err("Failed in SUMA_pst_C_giv_ALL");
         SUMA_RETURN(0);
      }
   }
     
   if (!SUMA_Class_stats( Opt->aset, Opt->cset, Opt->cmask, Opt->cmask_count,
                          Opt->pstCgALL, Opt->priCgALL, Opt->gold, 
                          Opt->cs, Opt->mix_frac_floor)) {
      SUMA_S_Err("Failed in class stats");
      SUMA_RETURN(0);
   }
   if (Opt->debug) 
      SUMA_show_Class_Stat(Opt->cs, 
                           "Posterior Weighted Class Stat At Input:\n", NULL);
   
   /* To begin iterations, we should have class stats and pstCgALL. 
      Also, need an initial cset if B > 0.0  */
   for (iter=0; iter<Opt->N_main; ++iter) {
      if (Opt->debug) {
         INFO_message("Iteration %d memory check:\n",iter);MCHECK;
      }
      /* improve parameters based on edge energy */
      if (Opt->edge) {
         double en;
         float vv=1.0;
         int *UseK, N_kok;
         THD_3dim_dataset *skelset=NULL, *l_Bset=NULL, *l_aset=NULL;
         NEW_SHORTY(Opt->aset, Opt->cs->N_label*(Opt->cs->N_label-1)/2, 
                     "skelly", skelset);
         UseK = (int *)SUMA_calloc(Opt->cs->N_label, sizeof(int));
         if ((N_kok = SUMA_Class_k_Selector(Opt->cs, "classes_string", 
                                          "CSF; GM; WM", UseK))<0) {
            SUMA_S_Err("Failed to find classes");
            SUMA_RETURN(0);
         }
         if (1) {
            /* It should be the case that edge energy should not be affected
            by the presence of bias field (METH2), for now, I will
            pass a constant field here for testing */
            NEW_SHORTY(Opt->aset, 1, "l_Bset", l_Bset);
            if (!SUMA_InitDset(l_Bset, &vv, 1, Opt->cmask, 1)) {
                     SUMA_S_Err("Failed to initialize l_Bset");
                     SUMA_RETURN(0);
            }
            if (iter == 0) l_aset = Opt->aset;
            else l_aset = Opt->xset; 
         } else { /* old approach */
            l_aset = Opt->aset; l_Bset = Opt->Bset;
         }  
         en = SUMA_DsetEdgeEnergy(l_aset, Opt->cset, 
                                  Opt->cmask, l_Bset, 
                                  skelset, Opt->cs, Opt->edge,
                                  UseK, N_kok); 
         SUMA_Seg_Write_Dset(Opt->proot, "PreSkel", skelset, iter, Opt->hist);
         SUMA_S_Notev("Edge Enenergy, Pre MAP : %f\n", en);
         
         #if 1
         if (!SUMA_MAP_EdgeEnergy(  l_aset, Opt->cmask, Opt->cmask_count,
                                    l_Bset, Opt->cs, 
                                    Opt->cset, Opt->edge, 
                                    Opt->priCgALL, Opt->pCgN,
                                    Opt->B, Opt->T, 0.4, 0.4,
                                    Opt)) {
            SUMA_S_Err("Failed in MAP_EdgeEnergy");
            exit(1);
         }  
         
         en = SUMA_DsetEdgeEnergy(l_aset, Opt->cset, 
                                  Opt->cmask, 
                                  l_Bset, skelset, Opt->cs, Opt->edge,
                                  UseK, N_kok);
         SUMA_Seg_Write_Dset(Opt->proot, "PstSkel", skelset, iter, Opt->hist); 
         SUMA_S_Notev("Edge Enenergy, Post MAP : %f\n", en);
         #endif

         DSET_delete(skelset); skelset=NULL;
         if (l_Bset && l_Bset != Opt->Bset) DSET_delete(l_Bset); l_Bset=NULL;
         SUMA_ifree(UseK);
      }

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

      
      
      if (Opt->B > 0) {
         if (Opt->debug > 1 && iter==0) {
            SUMA_Seg_Write_Dset(Opt->proot, "MAPlabel.-1", Opt->cset, 
                                 -1, Opt->hist);
         }
         if (!(SUMA_MAP_labels(Opt->xset, Opt->cmask, 
                               Opt->cs, 6, Opt->priCgALL, &Opt->cset, 
                               &Opt->pCgN, Opt))) {
            SUMA_S_Err("Failed in SUMA_MAP_labels");
            SUMA_RETURN(0);
         }
         if (Opt->debug > 1) {
            SUMA_Seg_Write_Dset(Opt->proot, "MAPlabel", Opt->cset, 
                                iter, Opt->hist);
            SUMA_Seg_Write_Dset(Opt->proot, "pCgN", Opt->pCgN, iter, 
                                 Opt->hist);
         }
         AFNI_FEED(Opt->ps->cs, "MAPlabel", iter, Opt->cset);
      }
                                  
      if (!(SUMA_pst_C_giv_ALL(Opt->xset, 
                Opt->cmask, Opt->cmask_count,
                Opt->cs,  
                Opt->priCgALL, Opt->pCgN,
                Opt->B, Opt->T, 
                (!Opt->mixopt || strcmp(Opt->mixopt,"IGNORE")) ? 1:0,
                               &Opt->pstCgALL))) {
         SUMA_S_Err("Failed in SUMA_pst_C_giv_ALL");
         SUMA_RETURN(0);
      }
      if (Opt->debug > 1) {
         SUMA_Seg_Write_Dset(Opt->proot, "pstCgALL", Opt->pstCgALL, 
                            iter, Opt->hist);
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
                              Opt->cs, Opt->mix_frac_floor)) {
         SUMA_S_Err("Failed in class stats");
         SUMA_RETURN(0);
      }
   

      
      if (Opt->debug || Opt->gold || Opt->gold_bias) {
         double bad_bias_thresh, bias_bad_count;
         char *sbig=NULL;
         sprintf(sinf, "Class Stat iter %d:\n", iter+1);
         if (iter == Opt->N_main-1 || Opt->debug) {
            SUMA_show_Class_Stat(Opt->cs, sinf, NULL);
            if (Opt->proot) sprintf(sreport, 
                     "%s/ClassStat.i%02d%s.txt", Opt->proot, 
                                  iter+1, (iter==Opt->N_main-1) ? ".FINAL":"");
            else snprintf(sreport, 500, 
                     "%s.ClassStat.i%02d%s.txt",  
                     Opt->prefix, iter+1, (iter==Opt->N_main-1) ? ".FINAL":"");
            sbig = SUMA_append_replace_string(Opt->hist, sinf,"\n",0);
            SUMA_show_Class_Stat(Opt->cs, sbig, sreport);
            SUMA_ifree(sbig);
         }
         
         /* Report on bias correction */
         bad_bias_thresh = 0.06;
         if ((Opt->gold_bias && Opt->Bset) && 
             (iter == Opt->N_main-1 || Opt->debug)) {
            FILE *fout = fopen(sreport,"a");
            bias_bad_count = SUMA_CompareBiasDsets(Opt->gold_bias, Opt->Bset, 
                                 Opt->cmask, Opt->cmask_count, 
                                 bad_bias_thresh, NULL);
            SUMA_S_Notev("bad_count at thresh %f = %f%% of mask.\n",
                  bad_bias_thresh, bias_bad_count);
            if (fout) {
               fprintf(fout, "bad_count at thresh %f = %f%% of mask.\n",
                  bad_bias_thresh, bias_bad_count);
               fclose(fout); fout = NULL;
            }
         }
      }
      
   }
   
   if (Opt->Split) {
      THD_3dim_dataset *Gcset=NULL;
      THD_3dim_dataset *GpstCgALL=NULL;
      /* need to put things back */
      if (!SUMA_Regroup_classes(Opt, 
                        Opt->cs->label, Opt->cs->N_label, Opt->cs->keys,
                        Opt->Gcs->label, Opt->Gcs->N_label, Opt->Gcs->keys,
                        Opt->cmask, Opt->pstCgALL, Opt->cset,
                        &GpstCgALL, &Gcset)) {
      }
      /* switch dsets */
      DSET_delete(Opt->pstCgALL); Opt->pstCgALL = GpstCgALL; GpstCgALL = NULL;
      DSET_delete(Opt->cset); Opt->cset = Gcset; Gcset = NULL;
   }
   
   SUMA_RETURN(1);
}

#define CLASS_KEYS_FROM_LT(vl_dtable) {\
   int i, kk;\
   /* make sure all classes are in the labeltable */\
   for (i=0; i<Opt->clss->num; ++i) {\
      if ((kk = SUMA_KeyofLabel_Dtable(vl_dtable, Opt->clss->str[i]))<0){\
            ERROR_exit("Key not found in %s for %s ", \
                     Opt->labeltable_name, Opt->clss->str[i]);\
      }\
      if (Opt->keys) {\
         if (Opt->keys[i]!=kk) {\
            ERROR_exit("Key mismatch %d %d", Opt->keys[i], kk);\
         }\
      }   \
   }   \
   if (!Opt->keys) { /* get them from table */\
      Opt->keys = (int *)calloc(Opt->clss->num, sizeof(int));\
      for (i=0; i<Opt->clss->num; ++i) {\
         if ((kk = SUMA_KeyofLabel_Dtable(vl_dtable,\
                                          Opt->clss->str[i]))<0){\
            ERROR_exit("(should noy happen) Key not found in %s for %s ", \
                        Opt->labeltable_name, Opt->clss->str[i]);\
         }\
         Opt->keys[i] = kk;\
      }\
   }\
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
   
   /* load the input data */
   if (!(Opt->aset = Seg_load_dset( Opt->aset_name ))) {      
      SUMA_RETURN(1);
   }
   
   if (!Seg_CheckOpts(Opt)) {
      SUMA_S_Err("Failed on option check");
      SUMA_RETURN(1);
   }
   
   /* Load mask dataset */
   if (Opt->mset_name) {
      if (!strncasecmp(Opt->mset_name,"auto", 4)) {
         byte *mm=NULL;
         int j;
         short *sb=NULL;
         if (!(mm = THD_automask(Opt->aset))) {
            SUMA_RETURN(1);
         }
         NEW_SHORTY(Opt->aset, DSET_NVALS(Opt->aset), 
                              "automask.cp", Opt->mset);
         sb = (short *)DSET_ARRAY(Opt->mset,0);
         for (j=0; j<DSET_NVOX(Opt->mset); ++j) {
               sb[j] = (short)mm[j];
         }
         free(mm); mm=NULL;
      } else if (!(Opt->mset = Seg_load_dset( Opt->mset_name ))) {      
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

   /* classified set ? */
   if (Opt->this_cset_name) { /* user supplied initializer */
      if (!(Opt->cset = Seg_load_dset( Opt->this_cset_name ))) {      
         SUMA_RETURN(1);
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
      CLASS_KEYS_FROM_LT(vl_dtable);

      destroy_Dtable(vl_dtable); vl_dtable=NULL;
   } 
   
   if (!Opt->keys) {
      Dtable *vl_dtable=NULL;
      if (Opt->cset && (vl_dtable = DSET_Label_Dtable(Opt->cset))) { 
         if (Opt->debug) SUMA_S_Note("Getting keys from -cset dataset");
         /* try getting keys from cset */
         CLASS_KEYS_FROM_LT(vl_dtable);
         /* Do not delete vl_dtable, it is the same pointer in Opt->cset */
      } else {
         /* add default keys */
         if (Opt->debug) SUMA_S_Note("Keys not available, assuming defaults");
         Opt->keys = (int *)calloc(Opt->clss->num, sizeof(int));
         for (i=0; i<Opt->clss->num; ++i) {
            Opt->keys[i] = i+1;
         }
      }
   }
   
   /* Make sure you have no negative values and requesting bias field correction.
      The implementation uses log() for this so the negative values would be
      ill advised */
   {
      float amin, amax;
      THD_subbrick_minmax(Opt->aset, 0, 1,&amin, &amax);
      if (amin < 0 && Opt->bias_param > 0) {
         SUMA_S_Err("Cannot use field bias correction on volumes with negative\n"
           "values. Either turn off bias field estimation with -bias_fwhm 0.0\n"
           "or shift the values of the input by something like:\n"
           "   3dcalc -a %s -expr 'a+bool(a)*%d' -prefix SHIFTED\n"
           "and rerun the segmentation on SHIFTED. Note the suggested shift\n"
           "leaves zero values unchanged.",
              DSET_HEADNAME(Opt->aset), (int)ceil(-amin+1.0));
         exit(1);
      }
   }
   /* Show the match between keys and classes */
   if (Opt->debug > 1) {
      SUMA_S_Note("Class-->key map");
      SUMA_ShowClssKeys(Opt->clss->str, Opt->clss->num, Opt->keys);
   }
   if (Opt->clss->num < 2) {
      if (Opt->debug <= 1) {
         SUMA_S_Note("Class-->key map");
         SUMA_ShowClssKeys(Opt->clss->str, Opt->clss->num, Opt->keys);
      }
      SUMA_S_Err("Less than 2 classes? I am out of here");
      SUMA_RETURN(0);
   }
   
   /* Mask setup */
   if (Opt->debug > 1) {
      SUMA_S_Note("MaskSetup");
   }
   Opt->cmask = MaskSetup(Opt, Opt->aset, 1,
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
   
   Opt->cs = SUMA_New_Class_Stat(Opt->clss->str, Opt->clss->num, 
                                 Opt->keys, 3, NULL);
   
     
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
      /* set the floor of the input dset */
      if (0) {
         SUMA_S_Note("Setting probability floor, USEFULNESS NOT TESTED...");
         if (!set_p_floor(Opt->priCgA, 0.1, Opt->cmask)) {
            SUMA_S_Errv("Failed to set p floor for priCgA %s\n", 
                        Opt->priCgAname);
            SUMA_RETURN(1);
         }
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
   }
   if (Opt->debug > 1) {
      SUMA_Seg_Write_Dset(Opt->proot, "classes_init", Opt->cset, 
                            -1, Opt->hist);
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
         SUMA_S_Errv("Can't get mixfrac for %s\n", Opt->mixopt);
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
   if (Opt->Bset && !Opt->this_fset_name) {
      tross_Append_History(Opt->Bset, Opt->hist);
      SUMA_Seg_Write_Dset(Opt->proot, "BiasField", /* DSET_PREFIX(Opt->Bset) */
                              Opt->Bset, -1, Opt->hist);
   }
   if (Opt->xset && !Opt->this_xset_name) {
      AFNI_FEED(Opt->ps->cs, "BiasCorrect", -1, Opt->xset);
      SUMA_Seg_Write_Dset(Opt->proot, "AnatUB", /* DSET_PREFIX(Opt->xset)*/
                              Opt->xset, -1, Opt->hist);
   }
   if (Opt->cset) {
      SUMA_Seg_Write_Dset(Opt->proot, "Classes", /* Opt->crefix */ 
                          Opt->cset, -1, Opt->hist);
      AFNI_FEED(Opt->ps->cs, "FinalClasses", -1, Opt->cset);
   }
   if (Opt->pstCgALL) {
      SUMA_Seg_Write_Dset(Opt->proot, "Posterior",  /* Opt->prefix */
                          Opt->pstCgALL, -1, Opt->hist);
      AFNI_FEED(Opt->ps->cs, "pstCgALL-final", -1, Opt->pstCgALL);
   }
   if (Opt->aset) {
      SUMA_Seg_Write_Dset(Opt->proot, "Anat",  
                          Opt->aset, -1, Opt->hist);
   }
   if (Opt->debug) SUMA_S_Note("Writing Unmodulated output");
   if (!(SUMA_pst_C_giv_ALL(Opt->xset, 
                               Opt->cmask, Opt->cmask_count,
                               Opt->cs,  
                               NULL, NULL,
                               Opt->B, Opt->T, 0,
                               &Opt->pstCgALL))) {
         SUMA_S_Err("Failed in SUMA_pst_C_giv_ALL unmodulated");
         SUMA_RETURN(1);
   }
   SUMA_Seg_Write_Dset(Opt->proot, "Unmodulated.p", 
                        Opt->pstCgALL, -1, Opt->hist);
   
   if (!(SUMA_assign_classes( Opt->pstCgALL, Opt->cs, 
                              Opt->cmask, &Opt->cset))) { 
      SUMA_S_Err("Failed in assign_classes");
      SUMA_RETURN(1);
   }
   SUMA_Seg_Write_Dset(Opt->proot, "Unmodulated.c", 
                       Opt->cset, -1, Opt->hist);
                       
   /* all done, free */
   Opt = free_SegOpts(Opt);
  
   PRINT_COMPILE_DATE ; 
   SUMA_RETURN(0);
}
