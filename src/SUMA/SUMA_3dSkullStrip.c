#include "SUMA_suma.h"
#include "../thd_brainormalize.h"

#undef STAND_ALONE

#if defined SUMA_BrainWrap_STANDALONE
#define STAND_ALONE 
#endif

#ifdef STAND_ALONE
/* these global variables must be declared even if they will not be used by this main */
SUMA_SurfaceViewer *SUMAg_cSV = NULL; /*!< Global pointer to current Surface Viewer structure*/
SUMA_SurfaceViewer *SUMAg_SVv = NULL; /*!< Global pointer to the vector containing the various Surface Viewer Structures 
                                    SUMAg_SVv contains SUMA_MAX_SURF_VIEWERS structures */
int SUMAg_N_SVv = 0; /*!< Number of SVs realized by X */
SUMA_DO *SUMAg_DOv = NULL;   /*!< Global pointer to Displayable Object structure vector*/
int SUMAg_N_DOv = 0; /*!< Number of DOs stored in DOv */
SUMA_CommonFields *SUMAg_CF = NULL; /*!< Global pointer to structure containing info common to all viewers */
#else
extern SUMA_CommonFields *SUMAg_CF;
extern SUMA_DO *SUMAg_DOv;
extern SUMA_SurfaceViewer *SUMAg_SVv;
extern int SUMAg_N_SVv; 
extern int SUMAg_N_DOv;  
#endif

#ifdef SUMA_BrainWrap_STANDALONE
void usage_SUMA_BrainWrap (SUMA_GENERIC_ARGV_PARSE *ps)
   {
      static char FuncName[]={"usage_SUMA_BrainWrap"};
      char * s = NULL, *sio=NULL, *st = NULL, *sts = NULL;
      int i;
      s = SUMA_help_basics();
      sio  = SUMA_help_IO_Args(ps);
      sts = SUMA_help_talk(ps);
      printf ( "\n"
               "Usage: A program to extract the brain from surrounding.\n"
               "  tissue from MRI T1-weighted images. The largely automated\n"
               "  process consists of three steps:\n"
               "  1- Preprocessing of volume to remove gross spatial image \n"
               "  non-uniformity artifacts and reposition the brain in\n"
               "  a reasonable manner for convenience.\n"
               "  2- Expand a spherical surface iteratively until it envelopes\n"
               "  the brain. This is a modified version of the BET algorithm:\n"
               "     Fast robust automated brain extraction, \n"
               "      by Stephen M. Smith, HBM 2002 v 17:3 pp 143-155\n"
               "    Modifications include the use of:\n"
               "     . outer brain surface\n"
               "     . expansion driven by data inside and outside the surface\n"
               "     . avoidance of eyes and ventricles\n"
               "     . a set of operations to avoid the clipping of certain brain\n"
               "       areas and reduce leakage into the skull in heavily shaded\n"
               "       data\n"
               "  3- The creation of various masks and surfaces modeling brain\n"
               "     and portions of the skull\n"  
               "\n"
               "  3dSkullStrip  < -input VOL >\n"
               "             [< -o_TYPE PREFIX >] [< -prefix Vol_Prefix >] \n"
               "             [< -spatnorm >] [< -no_spatnorm >] [< -write_spatnorm >]\n"
               "             [< -niter N_ITER >] [< -ld LD >] \n"
               "             [< -shrink_fac SF >] [< -var_shrink_fac >] \n"
               "             [< -no_var_shrink_fac >] [< shrink_fac_bot_lim SFBL >]\n"
               "             [< -pushout >] [< -no_pushout >] [< -exp_frac FRAC]\n"
               "             [< -touchup >] [< -no_touchup >]\n"
               "             [< -fill_hole R >] [< -NN_smooth NN_SM >]\n"
               "             [< -smooth_final SM >] [< -avoid_vent >] [< -no_avoid_vent >]\n"
               "             [< -use_skull >] [< -no_use_skull >] \n"
               "             [< -avoid_eyes >] [< -no_avoid_eyes >] \n"
               "             [< -perc_int PERC_INT >]\n"
               "             [< -max_inter_iter MII >] \n"
               "             [< -debug DBG >] [< -node_dbg NODE_DBG >]\n"
               "             [< -demo_pause >]\n"  
               "\n"
               "  Mandatory parameters:\n"
               "     -input VOL: Input AFNI (or AFNI readable) volume.\n"
               "                 \n"
               "\n"
               "  Optional Parameters:\n"
               "     -o_TYPE PREFIX: prefix of output surface.\n"
               "        where TYPE specifies the format of the surface\n"
               "        and PREFIX is, well, the prefix.\n"
               "        TYPE is one of: fs, 1d (or vec), sf, ply.\n"
               "        Default is: -o_ply brainwrap_out\n"
               "        More on that below.\n"
               "     -prefix VOL_PREFIX: prefix of output volumes.\n"
               "        If not specified, the prefix is the same\n"
               "        as the one used with -o_TYPE.\n"
               "     -spat_norm: (Default) Perform spatial normalization first.\n"
               "                 This is a necessary step unless the volume has\n"
               "                 been 'spatnormed' already.\n"
               "     -no_spatnorm: Do not perform spatial normalization.\n"
               "                   Use this option only when the volume \n"
               "                   has been run through the 'spatnorm' process\n"
               "     -write_spatnorm: Write the 'spatnormed' volume to disk.\n"
               "     -niter N_ITER: Number of iterations. Default is 250\n"
               "        For denser meshes, you need more iterations\n"
               "        N_ITER of 750 works for LD of 50.\n"
               "     -ld LD: Parameter to control the density of the surface.\n"
               "             Default is 20. See CreateIcosahedron -help\n"
               "             for details on this option.\n"
               "     -shrink_fac SF: Parameter controlling the brain vs non-brain\n"
               "             intensity threshold (tb). Default is 0.6.\n"
               "              tb = (Imax - t2) SF + t2 \n"
               "             where t2 is the 2 percentile value and Imax is the local\n"
               "             maximum, limited to the median intensity value.\n"
               "             For more information on tb, t2, etc. read the BET paper\n"
               "             mentioned above. Note that in 3dSkullStrip, SF can vary across \n"
               "             iterations and might be automatically clipped in certain areas.\n"
               "             SF can vary between 0 and 1.\n"
               "             0: Intensities < median inensity are considered non-brain\n"
               "             1: Intensities < t2 are considered non-brain\n" 
               "     -var_shrink_fac: Vary the shrink factor with the number of\n"
               "             iterations. This reduces the likelihood of a surface\n"
               "             getting stuck on large pools of CSF before reaching\n"
               "             the outer surface of the brain. (Default)\n"
               "     -no_var_shrink_fac: Do not use var_shrink_fac.\n"
               "     -shrink_fac_bot_lim SFBL: Do not allow the varying SF to go\n"
               "             below SFBL . Default 0.65. \n"
               "             This option helps reduce potential for leakage below \n"
               "             the cerebellum.\n"
               "     -pushout: Consider values above each node in addition to values\n"
               "               below the node when deciding on expansion. (Default)\n"
               "     -no_pushout: Do not use -pushout.\n"
               "     -exp_frac FRAC: Speed of expansion (see BET paper). Default is 0.1.\n"
               "     -touchup: Perform touchup operations at end to include\n"
               "               areas not covered by surface expansion. (Default)\n"
               "     -no_touchup: Do not use -touchup\n"
               "     -fill_hole R: Fill small holes that can result from small surface\n"
               "                   intersections caused by the touchup operation.\n"
               "                   R is the maximum number of pixels on the side of a hole\n"
               "                   that can be filled. Big holes are not filled.\n"
               "                   If you use -touchup, the default R is 5. Otherwise \n"
               "                   the default is 0.\n"
               "                   This is a less than elegant solution to the small\n"
               "                   intersections and I hope to make do without it in \n"
               "                   the near future. \n"
               "     -NN_smooth NN_SM: Perform Nearest Neighbor coordinate interpolation\n"
               "                       every few iterations. Default is 72\n"
               "     -smooth_final SM: Perform final surface smoothing after all iterations.\n"
               "                       Default is 20 smoothing iterations.\n"
               "                       Smoothing is done using Taubin's method, \n"
               "                       see SurfSmooth -help for detail.\n"
               "     -avoid_vent: avoid ventricles. Default.\n"
               "     -no_avoid_vent: Do not use -avoid_vent.\n"
               "     -avoid_eyes: avoid eyes. Default\n"
               "     -no_avoid_eyes: Do not use -avoid_eyes.\n"
               "     -use_skull: Use outer skull to limit expansion of surface into\n"
               "                 the skull due to very strong shading artifacts.\n"
               "                 Default.\n"
               "                 Turn this option off (-no_use_skull) if you do not\n"
               "                 have skull imaged at the top or the sides of the brain.\n"
               "     -no_use_skull: Do not use -use_skull\n"
               "     -send_no_skull: Do not send the skull surface to SUMA if you are\n"
               "                     using  -talk_suma\n" 
               "     -perc_int PERC_INT: Percentage of segments allowed to intersect\n"
               "                         surface. Ideally this should be 0 (Default). \n"
               "                         However, few surfaces might have small stubborn\n"
               "                         intersections that produce a few holes.\n"
               "                         PERC_INT should be a small number, typically\n"
               "                         between 0 and 0.1\n"
               "     -max_inter_iter N_II: Number of iteration to remove intersection\n"
               "                           problems. With each iteration, the program\n"
               "                           automatically increases the amount of smoothing\n"
               "                           to get rid of intersections. Default is 4\n"
               "     -demo_pause: Pause at various step in the process to facilitate\n"
               "                  interactive demo while 3dSkullStrip is communicating\n"
               "                  with AFNI and SUMA. See 'Eye Candy' mode below and\n"
               "                  -talk_suma option. \n"
               "\n"
               "%s"
               "\n"
               "%s"
               "\n"
               "     -debug DBG: debug levels of 0 (default), 1, 2, 3.\n"
               "        This is no Rick Reynolds debug, which is oft nicer\n"
               "        than the results, but it will do.\n"
               "     -node_dbg NODE_DBG: Output lots of parameters for node\n"
               "                         NODE_DBG for each iteration.\n"
               "\n"
               " Eye Candy Mode: *** Only possible when input volume has been 'spatnormed'\n"
               "                     Make sure you use -no_spatnorm option also.\n"
               "                     In the future, this restriction will be lifted.\n"
               " You can run BrainWarp and have it send successive iterations\n"
               " to SUMA and AFNI. This is very helpful in following the\n"
               " progression of the algorithm and determining the source\n"
               " of trouble, if any.\n"
               " Example:\n"
               "     afni -niml -yesplugouts &\n"
               "     suma -niml &\n"
               "     3dSkullStrip -input Anat+orig -o_ply anat_brain -talk_suma -feed_afni -send_kth 5\n"
               "\n"
               " Tips:\n"
               " I ran the program with the default parameters on 200+ datasets.\n"
               " The results were quite good in all but a couple of instances:\n"
               "\n"  
               /*
               "     -sm_fac SMFAC: Smoothing factor (Default is 1)\n"
               "     -d1 D1: Distance to search inward (Default 20 mm)\n"
               "     -t2 T2: Force t2 to be T2 (Default automated)\n"
               "     -t T: Force t to be T (Default automated)\n"
               "     -tm TM: Force tm to be TM (Default automated)\n"
               "     -t98 T98: Force t98 to be T98 (Default automated)\n"
               */
               "%s"
               "\n", sio, sts, s);
       SUMA_free(s); s = NULL; SUMA_free(st); st = NULL; SUMA_free(sio); sio = NULL; SUMA_free(sts); sts = NULL;         
       s = SUMA_New_Additions(0, 1); printf("%s\n", s);SUMA_free(s); s = NULL;
       printf("       Ziad S. Saad SSCC/NIMH/NIH ziad@nih.gov     \n");
       exit (0);
   }
/*!
   \brief parse the arguments for SurfSmooth program
   
   \param argv (char *)
   \param argc (int)
   \return Opt (SUMA_ISOSURFACE_OPTIONS *) options structure.
               To free it, use 
               SUMA_free(Opt->out_name); 
               SUMA_free(Opt);
*/
SUMA_ISOSURFACE_OPTIONS *SUMA_BrainWrap_ParseInput (char *argv[], int argc, SUMA_GENERIC_ARGV_PARSE *ps)
{
   static char FuncName[]={"SUMA_BrainWrap_ParseInput"}; 
   SUMA_ISOSURFACE_OPTIONS *Opt=NULL;
   int kar, i, ind;
   char *outname;
   SUMA_Boolean brk = NOPE;
   SUMA_Boolean LocalHead = NOPE;

   SUMA_ENTRY;
   
   Opt = (SUMA_ISOSURFACE_OPTIONS *)SUMA_malloc(sizeof(SUMA_ISOSURFACE_OPTIONS));
   
   kar = 1;
   Opt->spec_file = NULL;
   Opt->out_vol_prefix = NULL;
   Opt->out_prefix = NULL;
   Opt->sv_name = NULL;
   Opt->N_surf = -1;
   Opt->in_name = NULL;
   Opt->cmask = NULL;
   Opt->MaskMode = SUMA_ISO_UNDEFINED;
   for (i=0; i<ISOSURFACE_MAX_SURF; ++i) { Opt->surf_names[i] = NULL; }
   outname = NULL;
   Opt->in_vol = NULL;
   Opt->nvox = -1;
   Opt->ninmask = -1;
   Opt->mcdatav = NULL;
   Opt->debug = 0;
   Opt->v0 = 0.0;
   Opt->v1 = -1.0;
   Opt->dvec = NULL;
   Opt->SurfFileType = SUMA_PLY;
   Opt->SurfFileFormat = SUMA_ASCII;
   Opt->xform = SUMA_ISO_XFORM_MASK;
   Opt->obj_type = -1;
   Opt->obj_type_res = -1;
   Opt->XYZ = NULL;
   Opt->in_1D = NULL;
   Opt->N_XYZ = 0;
   Opt->Zt = 0.6;
   Opt->ExpFrac = 0.1;
   Opt->N_it = 250;
   Opt->Icold = 20;
   Opt->NodeDbg = -1;
   Opt->t2 = Opt->t98 = Opt->t = Opt->tm = -1;
   Opt->r = 0;
   Opt->d1 = 20;
   Opt->su1 = 1;
   Opt->UseNew = 1.0;
   Opt->d4 = 15;
   Opt->ztv = NULL;
   Opt->Kill98 = 0;
   Opt->NoEyes = 1;
   Opt->NNsmooth = 72;
   Opt->smootheach = 50;
   Opt->avoid_vent = 1;
   Opt->smooth_end = 20;
   Opt->k98mask = NULL;
   Opt->k98maskcnt = 0;
   Opt->dbg_eyenodes = NULL;
   Opt->travstp = 1.0;
   Opt->Stop = NULL;
   Opt->MaxIntIter = 4;
   Opt->UseExpansion = 1;
   Opt->PercInt = 0;
   Opt->UseSkull = 1;
   Opt->send_hull = 1;
   Opt->bot_lztclip = 0.65; /* 0.5 is OK but causes too much leakage below cerebellum in most dsets, 0.65 seems better. 0 if you do not want to use it*/
	Opt->var_lzt = 1.0; /* a flag at the moment, set it to 1 to cause shirnk fac to vary during iterations. Helps escape certain large 
                           chunks of CSF just below the brain */
   Opt->DemoPause = 0;
   Opt->DoSpatNorm = 1;
   Opt->WriteSpatNorm = 0;
   Opt->fillhole = -1;
   Opt->iset = NULL;
   Opt->SpatShift[0] = Opt->SpatShift[1] = Opt->SpatShift[2] = 0.0;
   brk = NOPE;
	while (kar < argc) { /* loop accross command ine options */
		/*fprintf(stdout, "%s verbose: Parsing command line...\n", FuncName);*/
		if (strcmp(argv[kar], "-h") == 0 || strcmp(argv[kar], "-help") == 0) {
			 usage_SUMA_BrainWrap(ps);
          exit (0);
		}
		
		SUMA_SKIP_COMMON_OPTIONS(brk, kar);
      
      if (!brk && (strcmp(argv[kar], "-pushout") == 0)) {
         Opt->UseExpansion = 1;
         brk = YUP;
      }
      
      if (!brk && (strcmp(argv[kar], "-no_pushout") == 0)) {
         Opt->UseExpansion = 0;
         brk = YUP;
      }
      
      if (!brk && (strcmp(argv[kar], "-spatnorm") == 0)) {
         Opt->DoSpatNorm = 1;
         brk = YUP;
      }
      
      if (!brk && (strcmp(argv[kar], "-no_spatnorm") == 0)) {
         Opt->DoSpatNorm = 0;
         brk = YUP;
      }
      
      if (!brk && (strcmp(argv[kar], "-write_spatnorm") == 0)) {
         Opt->WriteSpatNorm = 1;
         brk = YUP;
      }
      
      if (!brk && (strcmp(argv[kar], "-use_skull") == 0)) {
         Opt->UseSkull = 1;
         brk = YUP;
      }

      if (!brk && (strcmp(argv[kar], "-no_use_skull") == 0)) {
         Opt->UseSkull = 0;
         brk = YUP;
      }

      if (!brk && (strcmp(argv[kar], "-send_no_skull") == 0)) {
         Opt->send_hull = 0;
         brk = YUP;
      }
      
      
      if (!brk && (strcmp(argv[kar], "-var_shrink_fac") == 0)) {
         Opt->var_lzt = 1;
         brk = YUP;
      }
      
      if (!brk && (strcmp(argv[kar], "-no_var_shrink_fac") == 0)) {
         Opt->var_lzt = 0;
         brk = YUP;
      }
      
      
      if (!brk && (strcmp(argv[kar], "-fill_hole") == 0)) {
         kar ++;
			if (kar >= argc)  {
		  		fprintf (SUMA_STDERR, "need argument after -fill_hole \n");
				exit (1);
			}
         Opt->fillhole = atoi(argv[kar]);
         if ( (Opt->fillhole < 0 || Opt->fillhole > 50) ) {
            fprintf (SUMA_STDERR, "parameter after -fill_hole (%d) should be >= 0 and <= 50 \n", Opt->fillhole);
				exit (1);
         }
         brk = YUP;
      }
            
      if (!brk && (strcmp(argv[kar], "-perc_int") == 0)) {
         kar ++;
			if (kar >= argc)  {
		  		fprintf (SUMA_STDERR, "need argument after -perc_int \n");
				exit (1);
			}
			Opt->PercInt = atof(argv[kar]);
         if ( (Opt->PercInt < 0 && Opt->PercInt != -1) || Opt->PercInt > 10) {
            fprintf (SUMA_STDERR, "parameter after -perc_int should be -1 or between 0 and 10 (have %f) \n", Opt->PercInt);
				exit (1);
         }
         brk = YUP;
		}
      
      
      if (!brk && (strcmp(argv[kar], "-input_1D") == 0)) {
         kar ++;
			if (kar >= argc)  {
		  		fprintf (SUMA_STDERR, "need argument after -input_1D \n");
				exit (1);
			}
			Opt->in_1D = argv[kar];
         brk = YUP;
		}
      
      if (!brk && (strcmp(argv[kar], "-avoid_vent") == 0)) {
			Opt->avoid_vent = 1;
         brk = YUP;
		}
      
      if (!brk && (strcmp(argv[kar], "-demo_pause") == 0)) {
			Opt->DemoPause = 1;
         brk = YUP;
		}
      
      if (!brk && (strcmp(argv[kar], "-d1") == 0)) {
         kar ++;
			if (kar >= argc)  {
		  		fprintf (SUMA_STDERR, "need argument after -d1 \n");
				exit (1);
			}
			Opt->d1 = atof(argv[kar]);
         brk = YUP;
		}
      
      if (!brk && (strcmp(argv[kar], "-max_inter_iter") == 0)) {
         kar ++;
			if (kar >= argc)  {
		  		fprintf (SUMA_STDERR, "need argument after -max_inter_iter \n");
				exit (1);
			}
			Opt->MaxIntIter = atoi(argv[kar]);
         if (Opt->MaxIntIter < 0 || Opt->MaxIntIter > 10) {
            fprintf (SUMA_STDERR, "-max_inter_iter parameter should be between 0 and 10 \n");
				exit (1);
         }  
         brk = YUP;
		}
      if (!brk && (strcmp(argv[kar], "-smooth_final") == 0)) {
         kar ++;
			if (kar >= argc)  {
		  		fprintf (SUMA_STDERR, "need argument after -smooth_final \n");
				exit (1);
			}
			Opt->smooth_end = atoi(argv[kar]);
         if (Opt->smooth_end < 0 || Opt->smooth_end > 100 || Opt->smooth_end % 2) {
            fprintf (SUMA_STDERR, "irrational or exhuberant values for smooth_final\n Must use even number between 0 to 100 (%d entered)\n", Opt->smooth_end);
				exit (1);
         }
         brk = YUP;
		}
      if (!brk && (strcmp(argv[kar], "-NNsmooth") == 0)) {
         kar ++;
			if (kar >= argc)  {
		  		fprintf (SUMA_STDERR, "need argument after -NNsmooth \n");
				exit (1);
			}
			Opt->NNsmooth = atoi(argv[kar]);
         if (Opt->NNsmooth % 2) {
            fprintf (SUMA_STDERR, "Number of smoothing steps must be an even positive number (not %d) \n", Opt->NNsmooth);
				exit (1);
         }
         brk = YUP;
		}
      
      if (!brk && ((strcmp(argv[kar], "-shrink_fac") == 0) || (strcmp(argv[kar], "-bf") == 0))) {
         kar ++;
			if (kar >= argc)  {
		  		fprintf (SUMA_STDERR, "need argument after -shrink_fac || -bf \n");
				exit (1);
			}
			Opt->Zt = atof(argv[kar]);
         brk = YUP;
		}
      
      if (!brk && (strcmp(argv[kar], "-shrink_fac_bot_lim") == 0)) {
         kar ++;
			if (kar >= argc)  {
		  		fprintf (SUMA_STDERR, "need argument after -shrink_fac_bot_lim \n");
				exit (1);
			}
			Opt->bot_lztclip = atof(argv[kar]);
         brk = YUP;
		}
      
      if (!brk && (strcmp(argv[kar], "-t2") == 0)) {
         kar ++;
			if (kar >= argc)  {
		  		fprintf (SUMA_STDERR, "need argument after -t2 \n");
				exit (1);
			}
			Opt->t2 = atof(argv[kar]);
         brk = YUP;
		}
      if (!brk && (strcmp(argv[kar], "-t98") == 0)) {
         kar ++;
			if (kar >= argc)  {
		  		fprintf (SUMA_STDERR, "need argument after -t98 \n");
				exit (1);
			}
			Opt->t98 = atof(argv[kar]);
         brk = YUP;
		}
      if (!brk && (strcmp(argv[kar], "-t") == 0)) {
         kar ++;
			if (kar >= argc)  {
		  		fprintf (SUMA_STDERR, "need argument after -t \n");
				exit (1);
			}
			Opt->t = atof(argv[kar]);
         brk = YUP;
		}
      if (!brk && (strcmp(argv[kar], "-tm") == 0)) {
         kar ++;
			if (kar >= argc)  {
		  		fprintf (SUMA_STDERR, "need argument after -tm \n");
				exit (1);
			}
			Opt->tm = atof(argv[kar]);
         brk = YUP;
		}
      
      if (!brk && (strcmp(argv[kar], "-niter") == 0)) {
         kar ++;
			if (kar >= argc)  {
		  		fprintf (SUMA_STDERR, "need argument after -niter \n");
				exit (1);
			}
			Opt->N_it = atoi(argv[kar]);
         if (Opt->N_it < 0 || Opt->N_it > 100000) {
            fprintf (SUMA_STDERR, "%d is a bad iteration number.\n", Opt->N_it);
				exit (1);
         }
         brk = YUP;
		}
      
      if (!brk && (strcmp(argv[kar], "-exp_frac") == 0)) {
         kar ++;
			if (kar >= argc)  {
		  		fprintf (SUMA_STDERR, "need argument after -exp_frac \n");
				exit (1);
			}
			Opt->ExpFrac = atof(argv[kar]);
         if (Opt->ExpFrac < -1 || Opt->ExpFrac > 1) { /* accroding to algo, should be between 0 and 1 */
            fprintf (SUMA_STDERR, "%f is a bad expantion fraction.\n", Opt->ExpFrac);
				exit (1);
         }
         brk = YUP;
		}
      if (!brk && (strcmp(argv[kar], "-node_dbg") == 0)) {
         kar ++;
			if (kar >= argc)  {
		  		fprintf (SUMA_STDERR, "need argument after -node_dbg \n");
				exit (1);
			}
			Opt->NodeDbg = atoi(argv[kar]);
         if (Opt->NodeDbg < 0) {
            fprintf (SUMA_STDERR, "%d is a bad node number.\n", Opt->NodeDbg);
				exit (1);
         }
         brk = YUP;
		}
      
      if (!brk && (strcmp(argv[kar], "-sm_fac") == 0)) {
         kar ++;
			if (kar >= argc)  {
		  		fprintf (SUMA_STDERR, "need argument after -sm_fac \n");
				exit (1);
			}
			Opt->su1 = atof(argv[kar]);
         if (Opt->su1 < 0 || Opt->su1 > 1.5) {
            fprintf (SUMA_STDERR, "%f is a bad smoothness factor (acceptable range is 0 to 1\nbut values up to 1.5 are allowed for amusement).\n", Opt->su1);
				exit (1);
         }
         brk = YUP;
		}
      
      if (!brk && (strcmp(argv[kar], "-ld") == 0)) {
         kar ++;
			if (kar >= argc)  {
		  		fprintf (SUMA_STDERR, "need argument after -ld \n");
				exit (1);
			}
			Opt->Icold = atoi(argv[kar]);
         if (Opt->Icold < 0 || Opt->Icold > 300) {
            fprintf (SUMA_STDERR, "%d is a bad iteration number.\n", Opt->Icold);
				exit (1);
         }
         brk = YUP;
		}
      
      if (!brk && (strcmp(argv[kar], "-debug") == 0)) {
         kar ++;
			if (kar >= argc)  {
		  		fprintf (SUMA_STDERR, "need argument after -debug \n");
				exit (1);
			}
			Opt->debug = atoi(argv[kar]);
         if (Opt->debug > 2) { LocalHead = YUP; }
         brk = YUP;
		}      
      
     
      if (!brk && (strcmp(argv[kar], "-input") == 0)) {
         kar ++;
			if (kar >= argc)  {
		  		fprintf (SUMA_STDERR, "need argument after -input \n");
				exit (1);
			}
			Opt->in_name = SUMA_copy_string(argv[kar]);
         brk = YUP;
		}
      
      if (!brk && (strcmp(argv[kar], "-prefix") == 0)) {
         kar ++;
			if (kar >= argc)  {
		  		fprintf (SUMA_STDERR, "need argument after -prefix \n");
				exit (1);
			}
			Opt->out_vol_prefix = SUMA_copy_string(argv[kar]);
         brk = YUP;
		}
      
      
      if (!brk && ( (strcmp(argv[kar], "-touchup") == 0) ) ) {
         Opt->UseNew = 1.0;
         brk = YUP;
      }

      if (!brk && ( (strcmp(argv[kar], "-no_touchup") == 0) ) ) {
         Opt->UseNew = 0.0;
         brk = YUP;
      }
      
      if (!brk && (strcmp(argv[kar], "-k98") == 0)) {
         SUMA_SL_Warn("Bad option, causes trouble (big clipping) in other parts of brain.");
         Opt->Kill98 = 1;
         brk = YUP;
      }
      
      if (!brk && ( (strcmp(argv[kar], "-noeyes") == 0) || (strcmp(argv[kar], "-no_eyes") == 0) || (strcmp(argv[kar], "-avoid_eyes") == 0)) ) {
         Opt->NoEyes = 1;
         brk = YUP;
      }
      
      if (!brk && ( (strcmp(argv[kar], "-no_noeyes") == 0) || (strcmp(argv[kar], "-no_no_eyes") == 0) || (strcmp(argv[kar], "-no_avoid_eyes") == 0)) ) {
         Opt->NoEyes = 0;
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
   
   if (Opt->fillhole < 0) {
      if (Opt->UseExpansion) {
         if (Opt->debug) {
            SUMA_SL_Note("Setting fill_hole to 5");
         }
         Opt->fillhole = 5;
      } else  Opt->fillhole = 0;
   }
   
   /* transfer some options to Opt from ps. Clunky because this is retrofitting */
   if (ps->o_N_surfnames) {
      Opt->out_prefix = SUMA_copy_string(ps->o_surfnames[0]);
      Opt->SurfFileType = ps->o_FT[0];
      Opt->SurfFileFormat = ps->o_FF[0];
   }
   
   if (!Opt->in_name) {
      fprintf (SUMA_STDERR,"Error %s:\n-input  must be used.\n", FuncName);
      exit(1);
   }
   

   if (!Opt->out_prefix) Opt->out_prefix = SUMA_copy_string("brainwrap_out");
   if (!Opt->out_vol_prefix) {
      if (!Opt->out_prefix) Opt->out_vol_prefix = SUMA_copy_string("brainwrap_out");
      else Opt->out_vol_prefix = SUMA_copy_string(Opt->out_prefix);
   }
   
   if (Opt->t2 >= 0 || Opt->t98 >= 0 || Opt->tm >= 0  || Opt->t >= 0 ) {
      if (!(Opt->t2 >= 0 && Opt->t98 > 0 && Opt->tm > 0 && Opt->t > 0)){
         fprintf (SUMA_STDERR,"Error %s: \nAll or none of the t parameters are to be specified on command line:\nt2 %f t %f tm %f t98 %f\n", 
            FuncName, Opt->t2, Opt->t, Opt->tm, Opt->t98);
         exit(1);
      }
   }
   SUMA_RETURN(Opt);
}




int main (int argc,char *argv[])
{/* Main */    
   static char FuncName[]={"3dSkullStrip"}; 
	int i, N_in = 0, i3, kth_buf, hull_ld;
   int ii,jj,kk,ll,ijk , nx,ny,nz , nn, nint = 0 , nseg;
   void *SO_name=NULL, *SO_name_hull=NULL;
   float vol, *isin_float=NULL, pint, *dsmooth = NULL, XYZrai_shift[3];
   SUMA_SurfaceObject *SO = NULL, *SOhull=NULL;
   SUMA_ISOSURFACE_OPTIONS *Opt;  
   char  stmp[200], stmphull[200], *hullprefix=NULL, *prefix=NULL, *spatprefix=NULL;
   SUMA_Boolean exists = NOPE;
   SUMA_GENERIC_ARGV_PARSE *ps=NULL;
   short *isin = NULL;
   SUMA_FORM_AFNI_DSET_STRUCT *OptDs = NULL;
   THD_3dim_dataset *dset = NULL;
   THD_ivec3 orixyz , nxyz ;
   THD_fvec3 dxyz , orgxyz ;
   THD_3dim_dataset *oset = NULL;
   MRI_IMAGE *imin=NULL, *imout=NULL ;
   
   SUMA_Boolean LocalHead = NOPE;

	SUMA_mainENTRY;
   SUMA_STANDALONE_INIT;
   

   /* Allocate space for DO structure */
	SUMAg_DOv = SUMA_Alloc_DisplayObject_Struct (SUMA_MAX_DISPLAYABLE_OBJECTS);
   ps = SUMA_Parse_IO_Args(argc, argv, "-o;-talk;");
   
   if (argc < 2) {
      usage_SUMA_BrainWrap(ps);
      exit (1);
   }
   
   Opt = SUMA_BrainWrap_ParseInput (argv, argc, ps);

   if (Opt->debug > 2) LocalHead = YUP;

   SO_name = SUMA_Prefix2SurfaceName(Opt->out_prefix, NULL, NULL, Opt->SurfFileType, &exists);
   if (exists) {
      fprintf(SUMA_STDERR,"Error %s:\nOutput file(s) %s* on disk.\nWill not overwrite.\n", FuncName, Opt->out_prefix);
      exit(1);
   }
   hullprefix = SUMA_append_string(Opt->out_prefix,"_hull");
   SO_name_hull = SUMA_Prefix2SurfaceName(hullprefix, NULL, NULL, Opt->SurfFileType, &exists);
   if (exists) {
      fprintf(SUMA_STDERR,"Error %s:\nOutput file(s) %s* on disk.\nWill not overwrite.\n", FuncName, hullprefix);
      exit(1);
   }   
   kth_buf = ps->cs->kth; /* keep track of required value */
   
   /* turn on debugging for inodes */
   #if 0
   SUMA_SL_Note("Debugging for eye nodes");
   Opt->dbg_eyenodes = fopen("eyenodes.1D.dset", "w");
   #endif
   
   /* Load the AFNI volume and prep it*/
   if (Opt->DoSpatNorm) { /* chunk taken from 3dSpatNorm.c */
      if( Opt->debug ) SUMA_SL_Note("Loading dset, performing Spatial Normalization");
      /* load the dset */
      Opt->iset = THD_open_dataset( Opt->in_name );
      if( !ISVALID_DSET(Opt->iset) ){
        fprintf(stderr,"**ERROR: can't open dataset %s\n",Opt->in_name) ;
        exit(1);
      }
      /*--- get median brick --*/
      imin = THD_median_brick( Opt->iset ) ;
      if( imin == NULL ){
        fprintf(stderr,"**ERROR: can't load dataset %s\n",Opt->in_name) ;
        exit(1);
      }
      imin->dx = fabs(Opt->iset->daxes->xxdel) ;
      imin->dy = fabs(Opt->iset->daxes->yydel) ;
      imin->dz = fabs(Opt->iset->daxes->zzdel) ;

      DSET_unload( Opt->iset ) ;  /* don't need this data no more */

      /*-- convert image to shorts, if appropriate --*/
      if( DSET_BRICK_TYPE(Opt->iset,0) == MRI_short ||
          DSET_BRICK_TYPE(Opt->iset,0) == MRI_byte    ){

        imout = mri_to_short(1.0,imin) ;
        mri_free(imin) ; imin = imout ;
      }

      /*--- normalize image spatially ---*/
      mri_brainormalize_verbose( Opt->debug ) ;
      imout = mri_brainormalize( imin , Opt->iset->daxes->xxorient,
                                        Opt->iset->daxes->yyorient,
                                        Opt->iset->daxes->zzorient) ;
      mri_free( imin ) ;

      if( imout == NULL ){
        fprintf(stderr,"**ERROR: normalization fails!?\n"); exit(1);
      }

      oset = EDIT_empty_copy( NULL ) ;
      tross_Copy_History( Opt->iset , oset ) ;
      tross_Make_History( "3dSpatNorm" , argc,argv , oset ) ;
      
      LOAD_IVEC3( nxyz   , imout->nx    , imout->ny    , imout->nz    ) ;
      LOAD_FVEC3( dxyz   , imout->dx    , imout->dy    , imout->dz    ) ;
      LOAD_FVEC3( orgxyz , imout->xo    , imout->yo    , imout->zo    ) ;
      LOAD_IVEC3( orixyz , ORI_R2L_TYPE , ORI_A2P_TYPE , ORI_I2S_TYPE ) ;
      
      prefix = SUMA_AfniPrefix(Opt->in_name, NULL); 
      if (!prefix) { SUMA_SL_Err("Bad prefix!!!"); exit(1); }
      spatprefix = SUMA_append_string(prefix, "_SpatNorm");
      EDIT_dset_items( oset ,
                         ADN_prefix      , spatprefix ,
                         ADN_datum_all   , imout->kind ,
                         ADN_nxyz        , nxyz ,
                         ADN_xyzdel      , dxyz ,
                         ADN_xyzorg      , orgxyz ,
                         ADN_xyzorient   , orixyz ,
                         ADN_malloc_type , DATABLOCK_MEM_MALLOC ,
                         ADN_view_type   , VIEW_ORIGINAL_TYPE ,
                         ADN_type        , HEAD_ANAT_TYPE ,
                         ADN_func_type   , ANAT_BUCK_TYPE ,
                       ADN_none ) ;

      EDIT_substitute_brick( oset , 0 , imout->kind , mri_data_pointer(imout) ) ;      
      if (Opt->WriteSpatNorm) {
         SUMA_LH("Writing SpatNormed dset");
         DSET_write(oset) ;
      }
      Opt->in_vol = oset; oset = NULL;
      
      if (prefix) SUMA_free(prefix); prefix = NULL;
      if (spatprefix) SUMA_free(spatprefix); spatprefix = NULL;
      fprintf(SUMA_STDERR,"%s: -spatnorm: Expecting %d voxels in in_vol dset (%d %d %d)\n", 
         FuncName, DSET_NVOX( Opt->in_vol ), DSET_NX( Opt->in_vol ), DSET_NY( Opt->in_vol ), DSET_NZ( Opt->in_vol )); 
      
   } else {
      /* volume already SpatNormed, or user does not want SpatNorm */
      if( Opt->debug ) SUMA_SL_Note("Loading dset, performing no Spatial Normalization");
      Opt->in_vol = THD_open_dataset( Opt->in_name );
      fprintf(SUMA_STDERR,"%s: -no_spatnorm: Expecting %d voxels in in_vol dset (%d %d %d)\n", 
         FuncName, DSET_NVOX( Opt->in_vol ), DSET_NX( Opt->in_vol ), DSET_NY( Opt->in_vol ), DSET_NZ( Opt->in_vol )); 
      /* load the dset */
      DSET_load(Opt->in_vol);
   }
   
   
   if (!ISVALID_DSET(Opt->in_vol)) {
      if (!Opt->in_name) {
         SUMA_SL_Err("NULL input volume.");
         exit(1);
      } else {
         SUMA_SL_Err("invalid volume.");
         exit(1);
      }
   } else if ( DSET_BRICK_TYPE(Opt->in_vol, 0) == MRI_complex) {
      SUMA_SL_Err("Can't do complex data.");
      exit(1);
   }

   
   if (Opt->UseSkull) { SOhull = SUMA_Alloc_SurfObject_Struct(1); }
   else SOhull = NULL;
   sprintf(stmphull,"OuterHull");  

   vol = SUMA_LoadPrepInVol (Opt, &SOhull);
   if ( vol <= 0 ) {
      SUMA_S_Err("Failed to load/prep volume");
      exit(1);
   } else {
      if (LocalHead) fprintf (SUMA_STDERR,"%s: Got me a volume of %f mm3\n", FuncName, vol);
   }
   
   
   
  do {   
      /* Now create that little sphere that is about to expand */
      sprintf(stmp,"icobaby_ld%d", Opt->Icold);  
      SO = SUMA_CreateIcosahedron (Opt->r/2.0, Opt->Icold, Opt->cog, "n", 1);
      if (!SO) {
         SUMA_S_Err("Failed to create Icosahedron");
         exit(1);
      }
      if (Opt->NoEyes) {
         if (Opt->Stop) SUMA_free(Opt->Stop); Opt->Stop = NULL;
         if (!Opt->Stop) {
            Opt->Stop = (float *)SUMA_calloc(SO->N_Node, sizeof(float));
            if (!Opt->Stop) {
               SUMA_SL_Crit("Failed to allocate");
               exit(1);
            }
         }
      }
      if (Opt->avoid_vent) {
         float U[3], Un, *a, P2[2][3];
         for (i=0; i<SO->N_Node; ++i) {
            /* stretch the top coordinates by d1 and the back too*/
            a = &(SO->NodeList[3*i]); 
            if (a[2] - SO->Center[2] > Opt->r/3 || a[1] - SO->Center[1] > Opt->r/2) {
               SUMA_UNIT_VEC(SO->Center, a, U, Un);
               SUMA_POINT_AT_DISTANCE_NORM(U, SO->Center, (Un+1.1*Opt->d1), P2);
               SO->NodeList[3*i] = P2[0][0]; SO->NodeList[3*i+1] = P2[0][1]; SO->NodeList[3*i+2] = P2[0][2];
            }
         }   
      }
      /* allocate and fix zt */
      Opt->ztv = (float *)SUMA_malloc(sizeof(float)*SO->N_Node);
      if (!Opt->ztv) {
         SUMA_SL_Crit("Failed to allocate");
         exit(1);
      } 
      for (i=0; i<SO->N_Node; ++i) Opt->ztv[i] = Opt->Zt;

      /* need sv for communication to AFNI */
      SO->VolPar = SUMA_VolParFromDset (Opt->in_vol);
      SO->SUMA_VolPar_Aligned = YUP; /* Surface is in alignment with volume, should not call SUMA_Align_to_VolPar ... */
   
      if (!SO->State) {SO->State = SUMA_copy_string("3dSkullStrip"); }
      if (!SO->Group) {SO->Group = SUMA_copy_string("3dSkullStrip"); }
      if (!SO->Label) {SO->Label = SUMA_copy_string(stmp); }
      /* make the idcode_str depend on the Label, it is convenient to
      send the same surface all the time to SUMA */
      if (SO->Label) { if (SO->idcode_str) SUMA_free(SO->idcode_str); SO->idcode_str = UNIQ_hashcode(SO->Label); }
      
      if (!nint && SOhull) {
         SOhull->VolPar = SUMA_VolParFromDset (Opt->in_vol);
         SOhull->SUMA_VolPar_Aligned = YUP; /* Surface is in alignment with volume, should not call SUMA_Align_to_VolPar ... */
   
         if (!SOhull->State) {SOhull->State = SUMA_copy_string("3dSkullStrip"); }
         if (!SOhull->Group) {SOhull->Group = SUMA_copy_string("3dSkullStrip"); }
         if (!SOhull->Label) {SOhull->Label = SUMA_copy_string(stmphull); }
         if (SOhull->Label) { if (SOhull->idcode_str) SUMA_free(SOhull->idcode_str); SOhull->idcode_str = UNIQ_hashcode(SOhull->Label); }
      }

      /* see if SUMA talk is turned on */
      if (ps->cs->talk_suma) {
         ps->cs->istream = SUMA_BRAINWRAP_LINE;
         ps->cs->kth = 1; /* make sure all surfaces get sent */
         if (!SUMA_SendToSuma (NULL, ps->cs, NULL, SUMA_NO_DSET_TYPE, 0)) {
            SUMA_SL_Err("Failed to initialize SUMA_SendToSuma");
            ps->cs->Send = NOPE;
            ps->cs->talk_suma = NOPE;
         } else {
            if (!nint && SOhull) {
               if (Opt->send_hull) {
                  SUMA_LH("Sending Hull");
                  if (Opt->DemoPause) { SUMA_PAUSE_PROMPT("Sending HULL next"); }
                  SUMA_SendSumaNewSurface(SOhull, ps->cs);
               }
            }
            SUMA_LH("Sending Ico");
            if (Opt->DemoPause) { SUMA_PAUSE_PROMPT("Sending Ico next"); }
            SUMA_SendSumaNewSurface(SO, ps->cs);

         }
         ps->cs->kth = kth_buf;
      }

      if (!nint && Opt->UseSkull) {
         /* get a crude mask of inner skull */
         if (Opt->DemoPause) { SUMA_PAUSE_PROMPT("Shrinking skull hull next"); }
         SUMA_SkullMask (SOhull, Opt, ps->cs);
         /* Now take mask and turn it into a volume */
         fprintf (SUMA_STDERR,"%s: Locating voxels on skull boundary  ...\n", FuncName);
         isin = SUMA_FindVoxelsInSurface (SOhull, SO->VolPar, &N_in, 0);
         isin_float = (float *)SUMA_malloc(sizeof(float) * SO->VolPar->nx*SO->VolPar->ny*SO->VolPar->nz);
         if (!isin_float) {
            SUMA_SL_Crit("Failed to allocate");
            exit(1);
         }
         for (i=0; i<SO->VolPar->nx*SO->VolPar->ny*SO->VolPar->nz; ++i) { if (isin[i] <= SUMA_ON_NODE) Opt->dvec[i] = 0; }
         #if 0
            SUMA_SL_Note("Writing hull mask");
            {
               FILE *fout=fopen("hullmask.1D","w");
               int ii, jj, kk; 
               for (i=0; i<SO->VolPar->nx*SO->VolPar->ny*SO->VolPar->nz; ++i) { 
                  SUMA_1D_2_3D_index(i, ii, jj, kk, SO->VolPar->nx, SO->VolPar->nx*SO->VolPar->ny); 
                  fprintf(fout,"%d %d %d %d\n",ii, jj, kk, isin[i]); 
               }
               fclose(fout);
            }
         #endif
         if (isin) SUMA_free(isin); isin = NULL;
      }
            
      /* This is it baby, start walking */
      if (Opt->DemoPause) { SUMA_PAUSE_PROMPT("Brain expansion next"); }
      SUMA_StretchToFitLeCerveau (SO, Opt, ps->cs);

      /* check for intersections */
      if (Opt->PercInt >= 0) {
         fprintf(SUMA_STDERR,"%s: Checking for self intersection...\n", FuncName);
         nseg = 30 * Opt->Icold * Opt->Icold; /* number of segments in Ico */
         nint = SUMA_isSelfIntersect(SO, (int)(Opt->PercInt * nseg / 100.0));
         if (nint < 0) {
            SUMA_SL_Err("Error in SUMA_isSelfIntersect. Ignoring self intersection test.");
            nint = 0;
         }
         /* percentage of guilty segments */
         pint = (float)nint / (float)nseg * 100;
         if (LocalHead) fprintf (SUMA_STDERR,"%s: at least %d (%f%%) of segments intersect the surface.\nStopping criterion is %f%%\n", FuncName, nint, pint, Opt->PercInt);
         if (pint > Opt->PercInt) {
            static int SelfIntRep = 0;
            int smstep;
            if (SelfIntRep < Opt->MaxIntIter) {
               /* SUMA_PAUSE_PROMPT("SelfIntersection Baby"); */
               if (!Opt->avoid_vent && !SelfIntRep) {
                  Opt->avoid_vent = 1;
               } else {
                  smstep = 12 * ( Opt->Icold / 25 ) * ( Opt->Icold / 25 ); /* 12 is OK for ld = 25 */ 
                  if ( smstep < 12) smstep = 12;
                  else if ( smstep % 2 ) ++smstep;
                  #if 0
                  if (!(SelfIntRep % 2)) { 
                     Opt->avoid_vent = 0;
                     Opt->NNsmooth += smstep; 
                  } else {
                     Opt->avoid_vent = 1;
                  }
                  #else
                     /* don't mess with avoid_vent option, it works well all the time */
                     Opt->NNsmooth += smstep; 
                  #endif
                  ++SelfIntRep;
               }
               fprintf(SUMA_STDERR,"Warning %s:****************\n Surface self intersecting! trying again:\n smoothing of %d, avoid_vent %d\n", FuncName, Opt->NNsmooth, (int)Opt->avoid_vent);
               SUMA_Free_Surface_Object(SO); SO = NULL;
            } else {
               fprintf(SUMA_STDERR,"Warning %s: Stubborn intersection remaining at smoothing of %d. Ignoring it.", FuncName, Opt->NNsmooth);
               nint = 0;
            }
         } else  {
            if (nint) fprintf(SUMA_STDERR,"%s: Number of intersection below criterion.\n", FuncName);
            else fprintf(SUMA_STDERR,"%s: No intersections found.\n", FuncName);
            nint = 0;
         }
      } else {
         fprintf(SUMA_STDERR,"%s: Self intersection check turned off.\n", FuncName);
         nint = 0;   
      }
   } while (nint != 0);
   fprintf(SUMA_STDERR,"%s: Final smoothing of %d\n", FuncName, Opt->NNsmooth);
   
   /* touch up, these might cause some surface intersection, but their effects should be small */
   if (Opt->UseNew) {
         double mval = 255;
         fprintf (SUMA_STDERR,"%s: Touchup correction, pass 1 ...\n", FuncName);
         if (Opt->DemoPause) { SUMA_PAUSE_PROMPT("touchup correction next"); }
         /* recover the eye balls please */
         if (mval < Opt->t98) {
            SUMA_SL_Warn("Looks like some values in dset might be larger than 255 !");
            mval = Opt->t98+10;
         }
         if (Opt->k98maskcnt && Opt->k98mask) { for (ii=0; ii<Opt->k98maskcnt; ++ii) Opt->dvec[Opt->k98mask[ii]] = mval; }
         /* SUMA_REPOSITION_TOUCHUP(6);*/
         SUMA_Reposition_Touchup(SO, Opt, 6, ps->cs);
         #if 0
         /* smooth the surface a bit */
         ps->cs->kth = 1;
         dsmooth = SUMA_Taubin_Smooth( SO, NULL,
                                    0.6307, -.6732, SO->NodeList,
                                    8, 3, SUMA_ROW_MAJOR, dsmooth, ps->cs, NULL);    
         memcpy((void*)SO->NodeList, (void *)dsmooth, SO->N_Node * 3 * sizeof(float));
         SUMA_RECOMPUTE_NORMALS(SO);
         if (ps->cs->Send) {
            if (!SUMA_SendToSuma (SO, ps->cs, (void *)SO->NodeList, SUMA_NODE_XYZ, 1)) {
               SUMA_SL_Warn("Failed in SUMA_SendToSuma\nCommunication halted.");
            }
         }
         ps->cs->kth = kth_buf;
         /* SUMA_REPOSITION_TOUCHUP(3);  */
         SUMA_Reposition_Touchup(SO, Opt, 3, ps->cs);

         #endif
         if (LocalHead) fprintf (SUMA_STDERR,"%s: Touchup correction  Done.\n", FuncName);
   }
   
   /* smooth the surface a bit */
   if (Opt->smooth_end) {
      ps->cs->kth = 1;  /*make sure all gets sent at this stage */
      if (Opt->DemoPause) { SUMA_PAUSE_PROMPT("beauty treatment smoothing next"); }
      fprintf (SUMA_STDERR,"%s: The beauty treatment smoothing.\n", FuncName);
      dsmooth = SUMA_Taubin_Smooth( SO, NULL,
                                    0.6307, -.6732, SO->NodeList,
                                    Opt->smooth_end, 3, SUMA_ROW_MAJOR, dsmooth, ps->cs, NULL);    
      memcpy((void*)SO->NodeList, (void *)dsmooth, SO->N_Node * 3 * sizeof(float));
      SUMA_RECOMPUTE_NORMALS(SO);
      if (ps->cs->Send) {
         if (!SUMA_SendToSuma (SO, ps->cs, (void *)SO->NodeList, SUMA_NODE_XYZ, 1)) {
            SUMA_SL_Warn("Failed in SUMA_SendToSuma\nCommunication halted.");
         }
      }
      ps->cs->kth = kth_buf; 
   }
   
   /* one more correction pass */
   if (Opt->UseNew) {
      ps->cs->kth = 1; /*make sure all gets sent at this stage */
      if (Opt->DemoPause) { SUMA_PAUSE_PROMPT("touchup correction 2 next"); }
      fprintf (SUMA_STDERR,"%s: Final touchup correction ...\n", FuncName);
      /* SUMA_REPOSITION_TOUCHUP(2); */
      SUMA_Reposition_Touchup(SO, Opt, 2, ps->cs);

      if (LocalHead) fprintf (SUMA_STDERR,"%s: Final touchup correction  Done.\n", FuncName);
      ps->cs->kth = kth_buf; 
   }
   
   /* send the last surface */
   ps->cs->kth = 1;
   if (ps->cs->Send) {
      if (!SUMA_SendToSuma (SO, ps->cs, (void *)SO->NodeList, SUMA_NODE_XYZ, 1)) {
         SUMA_SL_Warn("Failed in SUMA_SendToSuma\nCommunication halted.");
      }
   }
   ps->cs->kth = kth_buf; 
   
   if (Opt->DoSpatNorm) {
      float ispat, jspat, kspat, iorig, jorig, korig , xrai_orig, yrai_orig, zrai_orig;
      
      SUMA_LH("Changing coords of output surface");
 
      /* what does the origin point (THD_BN_XORG THD_BN_YORG THD_BN_ZORG, ijk 0 0 0 )in the spat normed brain correspond to ? */
      ispat = 0; jspat = 0; kspat = 0;
      brainnormalize_coord( ispat , jspat, kspat,
                           &iorig, &jorig, &korig , Opt->iset,
                           &xrai_orig, &yrai_orig, &zrai_orig);
      Opt->SpatShift[0] = xrai_orig - THD_BN_XORG; Opt->SpatShift[1] = yrai_orig - THD_BN_YORG; Opt->SpatShift[2] = zrai_orig - THD_BN_ZORG;

      /* shift the surface */
      for (i=0; i<SO->N_Node; ++i) {
         i3 = 3*i;
         SO->NodeList[i3 + 0] += Opt->SpatShift[0];
         SO->NodeList[i3 + 1] += Opt->SpatShift[1];
         SO->NodeList[i3 + 2] += Opt->SpatShift[2];
      }
      /* do the deed for the hull thing */
      if (SOhull) {
         for (i=0; i<SOhull->N_Node; ++i) {
            i3 = 3*i;
            SOhull->NodeList[i3 + 0] += Opt->SpatShift[0];
            SOhull->NodeList[i3 + 1] += Opt->SpatShift[1];
            SOhull->NodeList[i3 + 2] += Opt->SpatShift[2];
         }
      }
      /* Change the number of voxels in VolPar to reflect the number of voxels in the non-spatnormed dset */
      /* SUMA_VolDims(Opt->iset, &SO->VolPar->nx, &SO->VolPar->ny, &SO->VolPar->nz);  *//* remember, the dset that SO->VolPar represents is in RAI still */
      if (LocalHead) fprintf(SUMA_STDERR,"%s: \nPre: %d %d %d\n", FuncName, SO->VolPar->nx, SO->VolPar->ny, SO->VolPar->nz); 
      SUMA_Free_VolPar(SO->VolPar); SO->VolPar = NULL;
      SO->VolPar = SUMA_VolParFromDset(Opt->iset);
      if (LocalHead) fprintf(SUMA_STDERR,"%s: \nPost: %d %d %d\n", FuncName, SO->VolPar->nx, SO->VolPar->ny, SO->VolPar->nz); 
      if (SOhull) {
         SUMA_Free_VolPar(SOhull->VolPar); SOhull->VolPar = NULL;
         SOhull->VolPar = SUMA_VolParFromDset (Opt->iset);
      }
   }
   
   /* write the surfaces to disk */
   fprintf (SUMA_STDERR,"%s: Writing surface  ...\n", FuncName);
   if (!SUMA_Save_Surface_Object (SO_name, SO, Opt->SurfFileType, Opt->SurfFileFormat, NULL)) {
      fprintf (SUMA_STDERR,"Error %s: Failed to write surface object.\n", FuncName);
      exit (1);
   }
   if (Opt->UseSkull && SOhull) {
      fprintf (SUMA_STDERR,"%s: Writing skull surface  ...\n", FuncName);
      if (!SUMA_Save_Surface_Object (SO_name_hull, SOhull, Opt->SurfFileType, Opt->SurfFileFormat, NULL)) {
         fprintf (SUMA_STDERR,"Error %s: Failed to write surface object.\n", FuncName);
         exit (1);
      }
   }
   
   /* what voxels are inside the surface ? */
   fprintf (SUMA_STDERR,"%s: Locating voxels inside surface  ...\n", FuncName);
   isin = SUMA_FindVoxelsInSurface (SO, SO->VolPar, &N_in, Opt->fillhole);
   isin_float = (float *)SUMA_malloc(sizeof(float) * SO->VolPar->nx*SO->VolPar->ny*SO->VolPar->nz);
   if (!isin_float) {
      SUMA_SL_Crit("Failed to allocate");
      exit(1);
   }
   for (i=0; i<SO->VolPar->nx*SO->VolPar->ny*SO->VolPar->nz; ++i) isin_float[i] = (float)isin[i];
   if (isin) SUMA_free(isin); isin = NULL;
      
   fprintf (SUMA_STDERR,"%s: Writing mask volume  ...\n", FuncName);
   OptDs = SUMA_New_FormAfniDset_Opt();
   if (Opt->out_vol_prefix) {
      SUMA_FileName NewName = SUMA_StripPath(Opt->out_vol_prefix);
      OptDs->prefix = NewName.FileName; NewName.FileName = NULL;
      OptDs->prefix_path = NewName.Path; NewName.Path = NULL;
   }  else {
      OptDs->prefix = SUMA_copy_string("3dSkullStrip");
      OptDs->prefix_path = SUMA_copy_string("./");
   }
   if (Opt->DoSpatNorm) {
      OptDs->mset = Opt->iset; 
   } else {
      OptDs->mset = Opt->in_vol;
   }
   OptDs->full_list = 1;
   OptDs->dval = 1;
   dset = SUMA_FormAfnidset (NULL, isin_float, SO->VolPar->nx*SO->VolPar->ny*SO->VolPar->nz, OptDs);
   if (!dset) {
      SUMA_SL_Err("Failed to create output dataset!");
   } else {
      tross_Make_History( FuncName , argc,argv , dset ) ;
      DSET_write(dset) ;
   }
   
   /* you don't want to exit rapidly because the SUMA might not be done processing the last elements*/
   if (ps->cs->Send && !ps->cs->GoneBad) {
      /* cleanup and close connections */
      if (!SUMA_SendToSuma (SO, ps->cs, NULL, SUMA_NODE_XYZ, 2)) {
         SUMA_SL_Warn("Failed in SUMA_SendToSuma\nCleanup failed");
      }
   }   
   
   if (dsmooth) SUMA_free(dsmooth); dsmooth = NULL;
   if (OptDs) { OptDs->mset = NULL; OptDs = SUMA_Free_FormAfniDset_Opt(OptDs);  }
   if (dset) { DSET_delete(dset); dset = NULL; }
   if (isin) { SUMA_free(isin); isin = NULL; }
   if (isin_float) { SUMA_free(isin_float); isin_float = NULL; }
   if (ps) SUMA_FreeGenericArgParse(ps); ps = NULL;
   if (Opt->dbg_eyenodes) fclose(Opt->dbg_eyenodes); Opt->dbg_eyenodes = NULL;
   if (Opt->k98mask) SUMA_free(Opt->k98mask); Opt->k98mask = NULL;
   if (Opt->Stop) SUMA_free(Opt->Stop); Opt->Stop = NULL;
   if (Opt->dvec) SUMA_free(Opt->dvec); Opt->dvec = NULL;
   if (Opt->mcdatav) {SUMA_free(Opt->mcdatav); Opt->mcdatav = NULL;} 
   if (Opt->in_vol) { DSET_delete( Opt->in_vol); Opt->in_vol = NULL;} 
   if (Opt->out_prefix) SUMA_free(Opt->out_prefix); Opt->out_prefix = NULL;
   if (Opt->out_vol_prefix) SUMA_free(Opt->out_vol_prefix); Opt->out_vol_prefix = NULL;
   if (Opt->XYZ) SUMA_free(Opt->XYZ); Opt->XYZ = NULL;
   if (Opt->ztv) SUMA_free(Opt->ztv); Opt->ztv = NULL;
   if (Opt) SUMA_free(Opt);
   if (hullprefix) SUMA_free(hullprefix); hullprefix = NULL;
   if (SO_name_hull) SUMA_free(SO_name_hull); SO_name_hull = NULL;
   if (SO_name) SUMA_free(SO_name); SO_name = NULL;
   if (SO) SUMA_Free_Surface_Object(SO); SO = NULL;
   if (SOhull) SUMA_Free_Surface_Object(SOhull); SOhull = NULL;
   if (!SUMA_Free_Displayable_Object_Vect (SUMAg_DOv, SUMAg_N_DOv)) {
      SUMA_SL_Err("DO Cleanup Failed!");
   }
   if (!SUMA_Free_CommonFields(SUMAg_CF)) SUMA_error_message(FuncName,"SUMAg_CF Cleanup Failed!",1);
   exit(0);
}   
#endif
