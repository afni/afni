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
      printf ( 
"\n"
"Usage: A program to extract the brain from surrounding.\n"
"  tissue from MRI T1-weighted images. The fully automated\n"
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
"     . two additional processing stages to ensure convergence and\n"
"       reduction of clipped areas.\n"
"     . use of 3d edge detection, see Deriche and Monga references\n"
"       in 3dedge3 -help.\n"
"  3- The creation of various masks and surfaces modeling brain\n"
"     and portions of the skull\n"
"\n" 
"  Common examples of usage:\n"
"  -------------------------\n"
"  o 3dSkullStrip -input VOL -prefix VOL_PREFIX\n"
"     Vanilla mode, should work for most datasets.\n"
"  o 3dSkullStrip -input VOL -prefix VOL_PREFIX -push_to_edge\n"
"     Adds an agressive push to brain edges. Use this option\n"
"     when the chunks of gray matter are not included. This option\n"
"     might cause the mask to leak into non-brain areas.\n"
"  o 3dSkullStrip -input VOL -surface_coil -prefix VOL_PREFIX -monkey\n"
"     Vanilla mode, for use with monkey data.\n"
"  o 3dSkullStrip -input VOL -prefix VOL_PREFIX -ld 30\n"
"     Use a denser mesh, in the cases where you have lots of \n"
"     csf between gyri. Also helps when some of the brain is clipped\n"
"     close to regions of high curvature.\n"
"\n"
"  Tips:\n"
"  -----\n"
"     I ran the program with the default parameters on 200+ datasets.\n"
"     The results were quite good in all but a couple of instances, here\n"
"     are some tips on fixing trouble spots:\n"  
"\n"
"     Clipping in frontal areas, close to the eye balls:\n"
"        + Try -push_to_edge option first.\n"
"          Can also try -no_avoid_eyes option.\n"
"     Clipping in general:\n"
"        + Try -push_to_edge option first.\n"
"          Can also use lower -shrink_fac, start with 0.5 then 0.4\n"
"     Problems down below:\n"
"        + Piece of cerbellum missing, reduce -shrink_fac_bot_lim \n"
"          from default value.\n"
"        + Leakage in lower areas, increase -shrink_fac_bot_lim \n"
"          from default value.\n"
"     Some lobules are not included:\n"
"        + Use a denser mesh. Start with -ld 30. If that still fails,\n"
"        try even higher density (like -ld 50) and increase iterations \n"
"        (say to -niter 750). \n"
"        Expect the program to take much longer in that case.\n"
"        + Instead of using denser meshes, you could try blurring the data \n"
"        before skull stripping. Something like -blur_fwhm 2 did\n"
"        wonders for some of my data with the default options of 3dSkullStrip\n"
"        Blurring is a lot faster than increasing mesh density.\n"
"        + Use also a smaller -shrink_fac is you have lots of CSF between\n"
"        gyri.\n"
"     Massive chunks missing:\n"
"        + If brain has very large ventricles and lots of CSF between gyri,\n"
"        the ventricles will keep attracting the surface inwards. \n"
"        This often happens with older brains. In such \n"
"        cases, use the -visual option to see what is happening.\n"
"        For example, the options below did the trick in various\n"
"        instances. \n"
"            -blur_fwhm 2 -use_skull  \n"
"        or for more stubborn cases increase csf avoidance with this cocktail\n"
"            -blur_fwhm 2 -use_skull -avoid_vent -avoid_vent -init_radius 75 \n"
"\n"
"     Large regions outside brain included:\n"
"       + Usually because noise level is high. Try @NoisySkullStrip.\n"
"\n" 
"  Make sure that brain orientation is correct. This means the image in \n"
"  AFNI's axial slice viewer should be close to the brain's axial plane.\n"
"  The same goes for the other planes. Otherwise, the program might do a lousy\n"
"  job removing the skull.\n"
"\n" 
"  Eye Candy Mode: \n"
"  ---------------\n"
"  You can run BrainWarp and have it send successive iterations\n"
" to SUMA and AFNI. This is very helpful in following the\n"
" progression of the algorithm and determining the source\n"
" of trouble, if any.\n"
"  Example:\n"
"     afni -niml -yesplugouts &\n"
"     suma -niml &\n"
"     3dSkullStrip -input Anat+orig -o_ply anat_brain -visual\n"
"\n"
"  Help section for the intrepid:\n"
"  ------------------------------\n" 
"  3dSkullStrip  < -input VOL >\n"
"             [< -o_TYPE PREFIX >] [< -prefix VOL_PREFIX >] \n"
"             [< -spatnorm >] [< -no_spatnorm >] [< -write_spatnorm >]\n"
"             [< -niter N_ITER >] [< -ld LD >] \n"
"             [< -shrink_fac SF >] [< -var_shrink_fac >] \n"
"             [< -no_var_shrink_fac >] [< -shrink_fac_bot_lim SFBL >]\n"
"             [< -pushout >] [< -no_pushout >] [< -exp_frac FRAC]\n"
"             [< -touchup >] [< -no_touchup >]\n"
"             [< -fill_hole R >] [< -NN_smooth NN_SM >]\n"
"             [< -smooth_final SM >] [< -avoid_vent >] [< -no_avoid_vent >]\n"
"             [< -use_skull >] [< -no_use_skull >] \n"
"             [< -avoid_eyes >] [< -no_avoid_eyes >] \n"
"             [< -use_edge >] [< -no_use_edge >] \n"
"             [< -push_to_edge >] [<-no_push_to_edge>]\n"
"             [< -perc_int PERC_INT >] \n"
"             [< -max_inter_iter MII >] [-mask_vol | -orig_vol | -norm_vol]\n"
"             [< -debug DBG >] [< -node_debug NODE_DBG >]\n"
"             [< -demo_pause >]\n"
"             [< -monkey >] [< -marmoset >] [<-rat>]\n"  
"\n"
"  NOTE: Please report bugs and strange failures\n"
"        to saadz@mail.nih.gov\n"
"\n"
"  Mandatory parameters:\n"
"     -input VOL: Input AFNI (or AFNI readable) volume.\n"
"                 \n"
"\n"
"  Optional Parameters:\n"
"     -monkey: the brain of a monkey.\n"
"     -marmoset: the brain of a marmoset. \n"
"                this one was tested on one dataset\n"
"                and may not work with non default\n"
"                options. Check your results!\n"
"     -rat: the brain of a rat.\n"
"           By default, no_touchup is used with the rat.\n"
"     -surface_coil: Data acquired with a surface coil.\n"
"     -o_TYPE PREFIX: prefix of output surface.\n"
"        where TYPE specifies the format of the surface\n"
"        and PREFIX is, well, the prefix.\n"
"        TYPE is one of: fs, 1d (or vec), sf, ply.\n"
"        More on that below.\n"
"     -skulls: Output surface models of the skull.\n"
"     -4Tom:   The output surfaces are named based\n"
"             on PREFIX following -o_TYPE option below.\n"
"     -prefix VOL_PREFIX: prefix of output volume.\n"
"        If not specified, the prefix is the same\n"
"        as the one used with -o_TYPE.\n"
"        The output volume is skull stripped version\n"
"        of the input volume. In the earlier version\n"
"        of the program, a mask volume was written out.\n" 
"        You can still get that mask volume instead of the\n"
"        skull-stripped volume with the option -mask_vol . \n"
"        NOTE: In the default setting, the output volume does not \n"
"              have values identical to those in the input. \n"
"              In particular, the range might be larger \n"
"              and some low-intensity values are set to 0.\n"
"              If you insist on having the same range of values as in\n"
"              the input, then either use option -orig_vol, or run:\n"
"         3dcalc -nscale -a VOL+VIEW -b VOL_PREFIX+VIEW \\\n"
"                -expr 'a*step(b)' -prefix VOL_SAME_RANGE\n"
"              With the command above, you can preserve the range\n"
"              of values of the input but some low-intensity voxels would\n"
"              still be masked. If you want to preserve them, then use\n"
"              -mask_vol in the 3dSkullStrip command that would produce \n"
"              VOL_MASK_PREFIX+VIEW. Then run 3dcalc masking with voxels\n"
"              inside the brain surface envelope:\n"
"         3dcalc -nscale -a VOL+VIEW -b VOL_MASK_PREFIX+VIEW \\\n"
"                -expr 'a*step(b-3.01)' -prefix VOL_SAME_RANGE_KEEP_LOW\n"
"     -norm_vol: Output a masked and somewhat intensity normalized and \n"
"                thresholded version of the input. This is the default,\n"
"                and you can use -orig_vol to override it.\n"
"     -orig_vol: Output a masked version of the input AND do not modify\n"
"                the values inside the brain as -norm_vol would.\n"
"     -mask_vol: Output a mask volume instead of a skull-stripped\n"
"                volume.\n"
"                The mask volume containes:\n"
"                 0: Voxel outside surface\n"
"                 1: Voxel just outside the surface. This means the voxel\n"
"                    center is outside the surface but inside the \n"
"                    bounding box of a triangle in the mesh. \n"
"                 2: Voxel intersects the surface (a triangle), but center\n"
"                    lies outside.\n"
"                 3: Voxel contains a surface node.\n"
"                 4: Voxel intersects the surface (a triangle), center lies\n"
"                    inside surface. \n"
"                 5: Voxel just inside the surface. This means the voxel\n"
"                    center is inside the surface and inside the \n"
"                    bounding box of a triangle in the mesh. \n"
"                 6: Voxel inside the surface. \n"
"     -spat_norm: (Default) Perform spatial normalization first.\n"
"                 This is a necessary step unless the volume has\n"
"                 been 'spatnormed' already.\n"
"     -no_spatnorm: Do not perform spatial normalization.\n"
"                   Use this option only when the volume \n"
"                   has been run through the 'spatnorm' process\n"
"     -spatnorm_dxyz DXYZ: Use DXY for the spatial resolution of the\n"
"                          spatially normalized volume. The default \n"
"                          is the lowest of all three dimensions.\n"
"                          For human brains, use DXYZ of 1.0, for\n"
"                          primate brain, use the default setting.\n"
"     -write_spatnorm: Write the 'spatnormed' volume to disk.\n"
"     -niter N_ITER: Number of iterations. Default is 250\n"
"        For denser meshes, you need more iterations\n"
"        N_ITER of 750 works for LD of 50.\n"
"     -ld LD: Parameter to control the density of the surface.\n"
"             Default is 20 if -no_use_edge is used,\n"
"             30 with -use_edge. See CreateIcosahedron -help\n"
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
"             below SFBL . Default 0.65, 0.4 when edge detection is used. \n"
"             This option helps reduce potential for leakage below \n"
"             the cerebellum.\n"
"             In certain cases where you have severe non-uniformity resulting\n"
"             in low signal towards the bottom of the brain, you will need to\n"
"             reduce this parameter.\n"
/*               "     -shrink_fac_bias SHRINK_BIAS_FILE: A file containing bias \n"
"                      factors to apply to the shrink_fac at certain nodes.\n"
"                      This option is experimental at the moment.\n"
"                      Column 0 has node indices, Col. 1 has the bias factors\n" */
"     -pushout: Consider values above each node in addition to values\n"
"               below the node when deciding on expansion. (Default)\n"
"     -no_pushout: Do not use -pushout.\n"
"     -exp_frac FRAC: Speed of expansion (see BET paper). Default is 0.1.\n"
"     -touchup: Perform touchup operations at end to include\n"
"               areas not covered by surface expansion. \n"
"               Use -touchup -touchup for aggressive makeup.\n"
"               (Default is -touchup)\n"
"     -no_touchup: Do not use -touchup\n"
"     -fill_hole R: Fill small holes that can result from small surface\n"
"                   intersections caused by the touchup operation.\n"
"                   R is the maximum number of pixels on the side of a hole\n"
"                   that can be filled. Big holes are not filled.\n"
"                   If you use -touchup, the default R is 10. Otherwise \n"
"                   the default is 0.\n"
"                   This is a less than elegant solution to the small\n"
"                   intersections which are usually eliminated\n"
"                   automatically. \n"
"     -NN_smooth NN_SM: Perform Nearest Neighbor coordinate interpolation\n"
"                       every few iterations. Default is 72\n"
"     -smooth_final SM: Perform final surface smoothing after all iterations.\n"
"                       Default is 20 smoothing iterations.\n"
"                       Smoothing is done using Taubin's method, \n"
"                       see SurfSmooth -help for detail.\n"
"     -avoid_vent: avoid ventricles. Default.\n"
"                  Use this option twice to make the avoidance more\n"
"                  agressive. That is at times needed with old brains.\n"
"     -no_avoid_vent: Do not use -avoid_vent.\n"
"     -init_radius RAD: Use RAD for the initial sphere radius.\n"
"                       For the automatic setting, there is an\n"
"                       upper limit of 100mm for humans.\n"
"                       For older brains with lots of CSF, you\n"
"                       might benefit from forcing the radius \n"
"                       to something like 75mm\n"
"     -avoid_eyes: avoid eyes. Default\n"
"     -no_avoid_eyes: Do not use -avoid_eyes.\n"
"     -use_edge: Use edge detection to reduce leakage into meninges and eyes.\n"
"                Default.\n"
"     -no_use_edge: Do no use edges.\n"
"     -push_to_edge: Perform aggressive push to edge at the end.\n"
"                    This option might cause leakage.\n"
"     -no_push_to_edge: (Default).\n"
"     -use_skull: Use outer skull to limit expansion of surface into\n"
"                 the skull due to very strong shading artifacts.\n"
"                 This option is buggy at the moment, use it only \n"
"                 if you have leakage into skull.\n"
"     -no_use_skull: Do not use -use_skull (Default).\n"
"     -send_no_skull: Do not send the skull surface to SUMA if you are\n"
"                     using  -talk_suma\n" 
"     -perc_int PERC_INT: Percentage of segments allowed to intersect\n"
"                         surface. Ideally this should be 0 (Default). \n"
"                         However, few surfaces might have small stubborn\n"
"                         intersections that produce a few holes.\n"
"                         PERC_INT should be a small number, typically\n"
"                         between 0 and 0.1\n. A -1 means do not do\n"
"                         any testing for intersection.\n"
"     -max_inter_iter N_II: Number of iteration to remove intersection\n"
"                           problems. With each iteration, the program\n"
"                           automatically increases the amount of smoothing\n"
"                           to get rid of intersections. Default is 4\n"
"     -blur_fwhm FWHM: Blur dset after spatial normalization.\n"
"                      Recommended when you have lots of CSF in brain\n"
"                      and when you have protruding gyri (finger like)\n"
"                      Recommended value is 2..4. \n"
"     -interactive: Make the program stop at various stages in the \n"
"                   segmentation process for a prompt from the user\n"
"                   to continue or skip that stage of processing.\n"
"                   This option is best used in conjunction with options\n"
"                   -talk_suma and -feed_afni\n"
"     -demo_pause: Pause at various step in the process to facilitate\n"
"                  interactive demo while 3dSkullStrip is communicating\n"
"                  with AFNI and SUMA. See 'Eye Candy' mode below and\n"
"                  -talk_suma option. \n"
"\n"
"%s"
"     -visual: Equivalent to using -talk_suma -feed_afni -send_kth 5\n"
"\n"
"     -debug DBG: debug levels of 0 (default), 1, 2, 3.\n"
"        This is no Rick Reynolds debug, which is oft nicer\n"
"        than the results, but it will do.\n"
"     -node_debug NODE_DBG: Output lots of parameters for node\n"
"                         NODE_DBG for each iteration.\n"
"     The next 3 options are for specifying surface coordinates\n"
"     to keep the program from having to recompute them.\n"
"     The options are only useful for saving time during debugging.\n"
"     -brain_contour_xyz_file BRAIN_CONTOUR_XYZ.1D\n"
"     -brain_hull_xyz_file BRAIN_HULL_XYZ.1D\n"
"     -skull_outer_xyz_file SKULL_OUTER_XYZ.1D\n"
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
"\n", sio,  s);
       SUMA_free(s); s = NULL; SUMA_free(st); st = NULL; SUMA_free(sio); sio = NULL; SUMA_free(sts); sts = NULL;         
       s = SUMA_New_Additions(0, 1); printf("%s\n", s);SUMA_free(s); s = NULL;
       printf("       Ziad S. Saad SSCC/NIMH/NIH saadz@mail.nih.gov     \n");
       exit (0);
   }
/*!
   \brief parse the arguments for SurfSmooth program
   
   \param argv (char *)
   \param argc (int)
   \return Opt (SUMA_GENERIC_PROG_OPTIONS_STRUCT *) options structure.
               To free it, use 
               SUMA_free(Opt->out_name); 
               SUMA_free(Opt);
*/
SUMA_GENERIC_PROG_OPTIONS_STRUCT *SUMA_BrainWrap_ParseInput (
   char *argv[], int argc, SUMA_GENERIC_ARGV_PARSE *ps)
{
   static char FuncName[]={"SUMA_BrainWrap_ParseInput"}; 
   SUMA_GENERIC_PROG_OPTIONS_STRUCT *Opt=NULL;
   int kar, i, ind, exists;
   char *outname, cview[10];
   SUMA_Boolean brk = NOPE;
   SUMA_Boolean LocalHead = NOPE;

   SUMA_ENTRY;
   
   Opt = SUMA_Alloc_Generic_Prog_Options_Struct();
   
   kar = 1;
   Opt->SpatNormDxyz = 0.0;
   Opt->spec_file = NULL;
   Opt->out_vol_prefix = NULL;
   Opt->out_prefix = NULL;
   Opt->sv_name = NULL;
   Opt->N_surf = -1;
   Opt->in_name = NULL;
   Opt->cmask = NULL;
   Opt->MaskMode = 0;
   for (i=0; i<SUMA_GENERIC_PROG_MAX_SURF; ++i) { Opt->surf_names[i] = NULL; }
   outname = NULL;
   Opt->in_vol = NULL;
   Opt->nvox = -1;
   Opt->ninmask = -1;
   Opt->mcdatav = NULL;
   Opt->debug = 0;
   Opt->v0 = 0.0;
   Opt->v1 = -1.0;
   Opt->fvec = NULL;
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
   Opt->Icold = -1;
   Opt->NodeDbg = -1;
   Opt->t2 = Opt->t98 = Opt->t = Opt->tm = -1;
   Opt->r = -1.0;
   Opt->d1 = -1;
   Opt->su1 = 1;
   Opt->UseNew = -1.0;
   Opt->d4 = -1;
   Opt->ztv = NULL;
   Opt->Kill98 = 0;
   Opt->NoEyes = 1;
   Opt->NNsmooth = 72;
   Opt->smootheach = 50;
   Opt->avoid_vent = -1;
   Opt->smooth_end = 20;
   Opt->k98mask = NULL;
   Opt->k98maskcnt = 0;
   Opt->dbg_eyenodes = NULL;
   Opt->travstp = 1.0;
   Opt->Stop = NULL;
   Opt->MaxIntIter = 4;
   Opt->UseExpansion = 1;
   Opt->PercInt = 0;
   Opt->UseSkull = 0;
   Opt->send_hull = 1;
   Opt->bot_lztclip = -1; /* 0.5 is OK but causes too much leakage below cerebellum in most dsets, 0.65 seems better. 0 if you do not want to use it*/
	Opt->var_lzt = 1.0; /* a flag at the moment, set it to 1 to cause shirnk fac to vary during iterations. Helps escape certain large 
                           chunks of CSF just below the brain */
   Opt->DemoPause = SUMA_3dSS_NO_PAUSE;
   Opt->DoSpatNorm = 1;
   Opt->WriteSpatNorm = 0;
   Opt->fillhole = -1;
   Opt->iset = NULL;
   Opt->SpatShift[0] = Opt->SpatShift[1] = Opt->SpatShift[2] = 0.0;
   Opt->OrigSpatNormedSet = NULL;
   Opt->in_edvol = NULL;
   Opt->blur_fwhm = 0.0;
   Opt->shrink_bias_name = NULL;
   Opt->shrink_bias = NULL;
   Opt->specie = HUMAN;
   Opt->Use_emask = 1;
   Opt->emask  = NULL;
   Opt->PushToEdge = -1;
   Opt->DoSkulls = 0;
   Opt->UseThisBrain = NULL;
   Opt->UseThisBrainHull = NULL;
   Opt->UseThisSkullOuter = NULL;
   Opt->efrac = -1.0;
   Opt->match_area = 0;  
   Opt->xyz_scale = 1.0; 
   Opt->cog[0]=-9000.0f;
   Opt->cog[1]=-9000.0f;
   Opt->cog[2]=-9000.0f;
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
      
      if (!brk && ( (strcmp(argv[kar], "-4Tom") == 0) 
         || (strcmp(argv[kar], "-skulls") == 0) ) ) {
         Opt->DoSkulls = 1;
         brk = YUP;
      }
      
      if (!brk && (strcmp(argv[kar], "-monkey") == 0)) {
         Opt->specie = MONKEY;
         brk = YUP;
      }
      if (!brk && (strcmp(argv[kar], "-marmoset") == 0)) {
         Opt->specie = MARMOSET;
         brk = YUP;
      }
      if (!brk && (strcmp(argv[kar], "-rat") == 0)) {
         Opt->specie = RAT;
         brk = YUP;
      }
      if (!brk && (strcmp(argv[kar], "-visual") == 0)) {
         ps->cs->talk_suma = 1;
         ps->cs->kth = 5;
         ps->cs->Feed2Afni = 1;
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
      
      if (!brk && (strcmp(argv[kar], "-cog") == 0)) {
         kar ++;
			if (kar+2 >= argc)  {
		  		fprintf (SUMA_STDERR, "need argument after -cog \n");
				exit (1);
			}
			Opt->cog[0] = atof(argv[kar]); ++kar;
         Opt->cog[1] = atof(argv[kar]); ++kar;
         Opt->cog[2] = atof(argv[kar]); 
         brk = YUP;
		}
      
      if (!brk && (strcmp(argv[kar], "-spatnorm_dxyz") == 0)) {
         kar ++;
			if (kar >= argc)  {
		  		fprintf (SUMA_STDERR, "need argument after -spatnorm_dxyz \n");
				exit (1);
			}
			Opt->SpatNormDxyz = atof(argv[kar]);
         if ( Opt->SpatNormDxyz < 0  || Opt->SpatNormDxyz > 10) {
            fprintf (SUMA_STDERR, "parameter after -spatnorm_dxyz should be between 0 and 10 (have %f) \n", 
                     Opt->SpatNormDxyz);
				exit (1);
         }
         brk = YUP;
		}
      
      if (!brk && (strcmp(argv[kar], "-blur_fwhm") == 0)) {
         kar ++;
			if (kar >= argc)  {
		  		fprintf (SUMA_STDERR, "need argument after -blur_fwhm \n");
				exit (1);
			}
			Opt->blur_fwhm = atof(argv[kar]);
         if ( Opt->blur_fwhm < 0 || Opt->blur_fwhm > 50) {
            fprintf (SUMA_STDERR, "parameter after -blur_fwhm should be between 0 and 50 (have %f) \n", Opt->blur_fwhm);
				exit (1);
         }
         brk = YUP;
		}
      
      if (!brk && (strcmp(argv[kar], "-init_radius") == 0)) {
         kar ++;
			if (kar >= argc)  {
		  		fprintf (SUMA_STDERR, "need argument after -init_radius \n");
				exit (1);
			}
			Opt->r = atof(argv[kar]);
         if ( Opt->r <= 0 || Opt->r > 100) {
            fprintf (SUMA_STDERR, "parameter after -init_radius should be between 0 and 100 (have %f) \n", Opt->r);
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
      
      if (!brk && (strcmp(argv[kar], "-brain_contour_xyz_file") == 0)) {
         kar ++;
			if (kar >= argc)  {
		  		fprintf (SUMA_STDERR, "need argument after -brain_contour_xyz_file \n");
				exit (1);
			}
			Opt->UseThisBrain = argv[kar];
         brk = YUP;
		}      
      
      if (!brk && (strcmp(argv[kar], "-brain_hull_xyz_file") == 0)) {
         kar ++;
			if (kar >= argc)  {
		  		fprintf (SUMA_STDERR, "need argument after -brain_hull_xyz_file \n");
				exit (1);
			}
			Opt->UseThisBrainHull = argv[kar];
         brk = YUP;
		}
      
      if (!brk && (strcmp(argv[kar], "-skull_outer_xyz_file") == 0)) {
         kar ++;
			if (kar >= argc)  {
		  		fprintf (SUMA_STDERR, "need argument after -skull_outer_xyz_file \n");
				exit (1);
			}
			Opt->UseThisSkullOuter = argv[kar];
         brk = YUP;
		}
      
      if (!brk && (strcmp(argv[kar], "-avoid_vent") == 0)) {
			if (Opt->avoid_vent < 0) Opt->avoid_vent = 1;
         else ++Opt->avoid_vent;
         brk = YUP;
		}
      
      if (!brk && (strcmp(argv[kar], "-no_avoid_vent") == 0)) {
			Opt->avoid_vent = 0;
         brk = YUP;
		}
      
      if (!brk && (strcmp(argv[kar], "-demo_pause") == 0)) {
			Opt->DemoPause = SUMA_3dSS_DEMO_PAUSE;
         brk = YUP;
		} 
      
      if (!brk && (strcmp(argv[kar], "-interactive") == 0)) {
			Opt->DemoPause = SUMA_3dSS_INTERACTIVE;
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
      if (!brk && (strcmp(argv[kar], "-NNsmooth") == 0 || strcmp(argv[kar], "-NN_smooth") == 0)) {
         kar ++;
			if (kar >= argc)  {
		  		fprintf (SUMA_STDERR, "need argument after -NN_smooth \n");
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
      if (!brk && ( (strcmp(argv[kar], "-node_dbg") == 0) || (strcmp(argv[kar], "-node_debug") == 0)) ) {
         kar ++;
			if (kar >= argc)  {
		  		fprintf (SUMA_STDERR, "need argument after -node_debug \n");
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
      
      if (!brk && (strcmp(argv[kar], "-edge_thr") == 0)) {
         kar ++;
			if (kar >= argc)  {
		  		fprintf (SUMA_STDERR, "need argument after -edge_thr \n");
				exit (1);
			}
			Opt->efrac = atof(argv[kar]);
         if (Opt->efrac < 0.0f || Opt->efrac > 1.0f) { 
            fprintf (SUMA_STDERR, " -edge_thr must be between 0.0 and 1.0. Have %f\n", Opt->efrac);
				exit (1); 
         }
         brk = YUP;
		}
         
      if (!brk && (strcmp(argv[kar], "-use_edge") == 0)) {
			Opt->Use_emask = 1;
         brk = YUP;
		}
      
      if (!brk && (strcmp(argv[kar], "-no_use_edge") == 0)) {
			Opt->Use_emask = 0;
         brk = YUP;
		}
      
      if (!brk && (strcmp(argv[kar], "-push_to_edge") == 0)) {
			Opt->PushToEdge = 1;
         brk = YUP;
		}
      
      if (!brk && (strcmp(argv[kar], "-no_push_to_edge") == 0)) {
			Opt->PushToEdge = 0;
         brk = YUP;
		}
      
      if (!brk && (strcmp(argv[kar], "-shrink_fac_bias") == 0)) {
         kar ++;
			if (kar >= argc)  {
		  		fprintf (SUMA_STDERR, "need argument after -shrink_fac_bias \n");
				exit (1);
			}
			Opt->shrink_bias_name = SUMA_copy_string(argv[kar]);
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
      
      if (!brk && ( (strcmp(argv[kar], "-mask_vol") == 0) ) ) {
         Opt->MaskMode = 1;
         brk = YUP;
      }
      if (!brk && ( (strcmp(argv[kar], "-orig_vol") == 0) ) ) {
         Opt->MaskMode = 2;
         brk = YUP;
      }
      if (!brk && ( (strcmp(argv[kar], "-norm_vol") == 0) ) ) {
         Opt->MaskMode = 0;
         brk = YUP;
      }
      if (!brk && ( (strcmp(argv[kar], "-touchup") == 0) ) ) {
         if (Opt->UseNew < 0) Opt->UseNew = 1.0;
         else ++ Opt->UseNew;
         brk = YUP;
      }
      
      if (!brk && ( (strcmp(argv[kar], "-no_touchup") == 0) ) ) {
         Opt->UseNew = 0.0;
         brk = YUP;
      }
      
      if (!brk && ( (strcmp(argv[kar], "-surface_coil") == 0) ) ) {
         Opt->SurfaceCoil = 1;
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
   
   if (Opt->UseNew < 0) {
      if (Opt->specie != RAT) {
         Opt->UseNew = Opt->UseNew * -1.0;
      } else {
         Opt->UseNew = 0; /* not for ze ghat monsieur */
      }
   }
   
   if (Opt->PushToEdge < 0) Opt->PushToEdge = 0;
   
   if (Opt->PushToEdge > 0 && Opt->Use_emask <= 0) {
      fprintf(SUMA_STDERR,"Error %s:\nCannot use -push_to_edge without -use_edge\n", FuncName);
      exit (1);      
   }
   
   if (Opt->bot_lztclip < 0) {
      if (!Opt->Use_emask) Opt->bot_lztclip = 0.65;
      else Opt->bot_lztclip = 0.4;
   }
   
   if (Opt->Icold < 0) {
      if (!Opt->Use_emask) Opt->Icold = 20;
      else Opt->Icold = 30;
   }
   
   if (Opt->fillhole < 0) {
      if (Opt->UseExpansion) {
         if (Opt->debug) {
            SUMA_SL_Note("Setting fill_hole to 10");
         }
         Opt->fillhole = 10;
      } else  Opt->fillhole = 0;
   }
   
   /* transfer some options to Opt from ps. Clunky because this is retrofitting */
   if (ps->o_N_surfnames) {
      Opt->out_prefix = SUMA_copy_string(ps->o_surfnames[0]);
      Opt->SurfFileType = ps->o_FT[0];
      Opt->SurfFileFormat = ps->o_FF[0];
   }
   if (ps->o_N_surfnames && Opt->specie == MARMOSET) {
      SUMA_S_Err("Cannot write out surfaces with MARMOSET brain.");
      exit(1);
   }
   if (ps->cs->talk_suma&& Opt->specie == MARMOSET) {
      SUMA_S_Err("Cannot talk to suma with MARMOSET brain.");
      exit(1);
   }
   if (Opt->DoSkulls && !ps->o_N_surfnames) {
      fprintf (SUMA_STDERR,"Error %s:\n-skulls must be used in conjunction with -o_TYPE option\n", FuncName);
      exit(1);
   }
   
   if (!Opt->in_name) {
      fprintf (SUMA_STDERR,"Error %s:\n-input  must be used.\n", FuncName);
      exit(1);
   }
   /* what is the view of the input ?*/
   if (!SUMA_AfniView (Opt->in_name, cview)) {
      fprintf (SUMA_STDERR,"Error %s:\nCould not guess view from input dset %s\n", FuncName, Opt->in_name);
      exit(1);
   }

   if (!Opt->out_prefix) Opt->out_prefix = SUMA_copy_string("skull_strip_out");
   if (!Opt->out_vol_prefix) {
      if (!Opt->out_prefix) Opt->out_vol_prefix = SUMA_AfniPrefix("skull_strip_out", NULL, NULL, &exists);
      else Opt->out_vol_prefix = SUMA_AfniPrefix(Opt->out_prefix, NULL, NULL, &exists);
   } else {
      char *stmp = Opt->out_vol_prefix;
      Opt->out_vol_prefix = SUMA_AfniPrefix(stmp, NULL, NULL, &exists); 
      SUMA_free(stmp); stmp = NULL;
   }
   if (!THD_ok_overwrite() && SUMA_AfniExistsView(exists, cview)) {
      fprintf (SUMA_STDERR,"Error %s:\nOutput dset %s%s exists, will not overwrite\n", FuncName, Opt->out_vol_prefix, cview);
      exit(1);
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
   void *SO_name=NULL, *SO_name_hull=NULL, *SO_name_bhull = NULL, *SO_name_iskull = NULL, *SO_name_oskull = NULL;
   float vol, *isin_float=NULL, pint, *dsmooth = NULL, XYZrai_shift[3];
   SUMA_SurfaceObject *SO = NULL, *SOhull=NULL;
   SUMA_GENERIC_PROG_OPTIONS_STRUCT *Opt;  
   char  stmp[200], stmphull[200], *hullprefix=NULL, *prefix=NULL, *spatprefix=NULL, cbuf,
         *bhullprefix=NULL, *oskullprefix=NULL, *iskullprefix=NULL;
   SUMA_Boolean exists = NOPE;
   SUMA_GENERIC_ARGV_PARSE *ps=NULL;
   short *isin = NULL;
   SUMA_FORM_AFNI_DSET_STRUCT *OptDs = NULL;
   THD_3dim_dataset *dset = NULL;
   THD_ivec3 orixyz , nxyz ;
   THD_fvec3 dxyz , orgxyz , fv2, originRAIfv;
   THD_3dim_dataset *oset = NULL;
   MRI_IMAGE *imin=NULL, *imout=NULL, *imout_orig=NULL  ;
   
   SUMA_Boolean LocalHead = NOPE;

   SUMA_STANDALONE_INIT;
	SUMA_mainENTRY;
   

   /* Allocate space for DO structure */
	SUMAg_DOv = SUMA_Alloc_DisplayObject_Struct (SUMA_MAX_DISPLAYABLE_OBJECTS);
   ps = SUMA_Parse_IO_Args(argc, argv, "-o;-talk;");
   
   if (argc < 2) {
      usage_SUMA_BrainWrap(ps);
      exit (1);
   }
   
   Opt = SUMA_BrainWrap_ParseInput (argv, argc, ps);

   if (Opt->debug > 2) LocalHead = YUP;

   SO_name = SUMA_Prefix2SurfaceName(
               Opt->out_prefix, NULL, NULL, 
               Opt->SurfFileType, &exists);
   if (  !THD_ok_overwrite() &&
         exists && strcmp(Opt->out_prefix,"skull_strip_out")) { 
      /* do not worry about the default name for the surface */
      fprintf( SUMA_STDERR,
               "Error %s:\nOutput file(s) %s* on disk.\nWill not overwrite.\n",
               FuncName, Opt->out_prefix);
      exit(1);
   }
   bhullprefix = SUMA_append_string(Opt->out_prefix,"_brainhull");
   oskullprefix = SUMA_append_string(Opt->out_prefix,"_outerskull");
   iskullprefix = SUMA_append_string(Opt->out_prefix,"_innerskull");
   SO_name_bhull = SUMA_Prefix2SurfaceName(bhullprefix, NULL, NULL,
                                           Opt->SurfFileType, &exists);
   if (  !THD_ok_overwrite() &&
         exists && strcmp(Opt->out_prefix,"skull_strip_out")) {
      fprintf( SUMA_STDERR,
               "Error %s:\nOutput file(s) %s* on disk.\nWill not overwrite.\n",
               FuncName, Opt->out_prefix);
      exit(1);
   }   
   SO_name_iskull = SUMA_Prefix2SurfaceName( iskullprefix, NULL, NULL,
                                             Opt->SurfFileType, &exists);
   if (  !THD_ok_overwrite() &&
         exists && strcmp(Opt->out_prefix,"skull_strip_out")) {
      fprintf( SUMA_STDERR,
               "Error %s:\nOutput file(s) %s* on disk.\nWill not overwrite.\n",
               FuncName, Opt->out_prefix);
      exit(1);
   }  
   SO_name_oskull = SUMA_Prefix2SurfaceName( oskullprefix, NULL, NULL,
                                             Opt->SurfFileType, &exists);
   if (  !THD_ok_overwrite() &&
         exists && strcmp(Opt->out_prefix,"skull_strip_out")) {
      fprintf( SUMA_STDERR,
               "Error %s:\nOutput file(s) %s* on disk.\nWill not overwrite.\n",
                FuncName, Opt->out_prefix);
      exit(1);
   }   
   
   hullprefix = SUMA_append_string(Opt->out_prefix,"_hull");
   SO_name_hull = SUMA_Prefix2SurfaceName(hullprefix, NULL, NULL,
                                          Opt->SurfFileType, &exists);
   if (  !THD_ok_overwrite() &&
         exists && strcmp(Opt->out_prefix,"skull_strip_out")) {
      fprintf(SUMA_STDERR,
               "Error %s:\nOutput file(s) %s* on disk.\nWill not overwrite.\n",
                FuncName, hullprefix);
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
      if (Opt->debug) 
         SUMA_SL_Note("Loading dset, performing Spatial Normalization");
      /* load the dset */
      Opt->iset = THD_open_dataset( Opt->in_name );
      if( !ISVALID_DSET(Opt->iset) ){
        fprintf(stderr,"**ERROR: can't open dataset %s\n",Opt->in_name) ;
        exit(1);
      }
      Opt->iset_hand = SUMA_THD_handedness( Opt->iset );
      if (LocalHead) 
         fprintf(SUMA_STDERR,
                  "%s: Handedness of orig dset %d\n", FuncName, Opt->iset_hand);
      
      /* if have marmoset, scale coords by 2.5 and pretend tiz a monkey */
      if (Opt->specie == MARMOSET) {
         Opt->xyz_scale = 2.5;
         THD_volDXYZscale(Opt->iset->daxes, Opt->xyz_scale, 0);
         Opt->specie = MONKEY;   /* now pretend it is a monkey! */
      }
      /*--- get median brick --*/
      imin = THD_median_brick( Opt->iset ) ;
      if( imin == NULL ){
        fprintf(stderr,"**ERROR: can't load dataset %s\n",Opt->in_name) ;
        exit(1);
      }
      
      mri_speciebusiness(Opt->specie);
      if (Opt->SpatNormDxyz) {
         if (Opt->debug) SUMA_S_Note("Overriding default resampling");
         mri_brainormalize_initialize(
            Opt->SpatNormDxyz, 
            Opt->SpatNormDxyz, 
            Opt->SpatNormDxyz);
      } else {
         float xxdel, yydel, zzdel, minres;
         if (Opt->specie == MONKEY) minres = 0.5;
         else if (Opt->specie == MARMOSET) minres = 0.2;
         else if (Opt->specie == RAT) minres = 0.1;
         else minres = 0.5;
         /* don't allow for too low a resolution, please */
         if (SUMA_ABS(Opt->iset->daxes->xxdel) < minres) xxdel = minres;
         else xxdel = SUMA_ABS(Opt->iset->daxes->xxdel);
         if (SUMA_ABS(Opt->iset->daxes->yydel) < minres) yydel = minres;
         else yydel = SUMA_ABS(Opt->iset->daxes->yydel);
         if (SUMA_ABS(Opt->iset->daxes->zzdel) < minres) zzdel = minres;
         else zzdel = SUMA_ABS(Opt->iset->daxes->zzdel);
         if (Opt->debug) {
            fprintf(SUMA_STDERR,
               "%s:\n Original resolution %f, %f, %f\n "
               "SpatNorm resolution %f, %f, %f\n",
               FuncName, 
               Opt->iset->daxes->xxdel, 
               Opt->iset->daxes->yydel, 
               Opt->iset->daxes->zzdel, 
               xxdel, yydel, zzdel);
         }   
         mri_brainormalize_initialize(xxdel, yydel, zzdel);
      }
      
      if (Opt->d1 < 0) {
         Opt->d1 = 20.0/THD_BN_rat(); /* account for size difference */
      }
      if (Opt->d4 < 0) {
         Opt->d4 = 15.0/THD_BN_rat(); /* account for size difference */
      }
      
      if (Opt->debug > 1) 
         fprintf(SUMA_STDERR,"%s: Size Ratio = %f\n", FuncName, THD_BN_rat());
      
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
      if (Opt->fillhole) {
         imout = mri_brainormalize( imin , Opt->iset->daxes->xxorient,
                                        Opt->iset->daxes->yyorient,
                                        Opt->iset->daxes->zzorient, &imout_orig, NULL) ;
      } else {
         imout = mri_brainormalize( imin , Opt->iset->daxes->xxorient,
                                        Opt->iset->daxes->yyorient,
                                        Opt->iset->daxes->zzorient, &imout_orig, NULL) ;
      }
      mri_free( imin ) ;

      if( imout == NULL ){
        fprintf(stderr,"**ERROR: normalization fails!?\n"); exit(1);
      }
      
      if (imout_orig) {
         if (Opt->debug > 1) SUMA_S_Note("Creating an output dataset in original grid...");
         /* me needs the origin of this dset in RAI world */
         LOAD_FVEC3(originRAIfv , Opt->iset->daxes->xxorg , Opt->iset->daxes->yyorg , Opt->iset->daxes->zzorg) ;
         originRAIfv = THD_3dmm_to_dicomm( Opt->iset , originRAIfv ) ;

         LOAD_FVEC3(fv2 , Opt->iset->daxes->xxorg + (Opt->iset->daxes->nxx-1)*Opt->iset->daxes->xxdel ,
                    Opt->iset->daxes->yyorg + (Opt->iset->daxes->nyy-1)*Opt->iset->daxes->yydel ,
                    Opt->iset->daxes->zzorg + (Opt->iset->daxes->nzz-1)*Opt->iset->daxes->zzdel  ) ;
         fv2 = THD_3dmm_to_dicomm( Opt->iset , fv2 ) ;

         if( originRAIfv.xyz[0] > fv2.xyz[0] ) { float tf; tf = originRAIfv.xyz[0]; originRAIfv.xyz[0] = fv2.xyz[0]; fv2.xyz[0] = tf; } 
         if( originRAIfv.xyz[1] > fv2.xyz[1] ) { float tf; tf = originRAIfv.xyz[1]; originRAIfv.xyz[1] = fv2.xyz[1]; fv2.xyz[1] = tf; }
         if( originRAIfv.xyz[2] > fv2.xyz[2] ) { float tf; tf = originRAIfv.xyz[2]; originRAIfv.xyz[2] = fv2.xyz[2]; fv2.xyz[2] = tf; }

         if (LocalHead) {
            fprintf(stderr,"++3dSpatNorm (ZSS): RAI origin info: %f %f %f\n", originRAIfv.xyz[0], originRAIfv.xyz[1], originRAIfv.xyz[2]);
         }

         Opt->OrigSpatNormedSet = EDIT_empty_copy( NULL ) ;
         tross_Copy_History( Opt->iset , Opt->OrigSpatNormedSet ) ;
         tross_Make_History( "3dSkullStrip" , argc,argv , Opt->OrigSpatNormedSet ) ;

         LOAD_IVEC3( nxyz   , imout_orig->nx    , imout_orig->ny    , imout_orig->nz    ) ;
         LOAD_FVEC3( dxyz   , imout_orig->dx    , imout_orig->dy    , imout_orig->dz    ) ;
         LOAD_FVEC3( orgxyz , originRAIfv.xyz[0]    , originRAIfv.xyz[1]    , originRAIfv.xyz[2]    ) ;
         LOAD_IVEC3( orixyz , ORI_R2L_TYPE , ORI_A2P_TYPE , ORI_I2S_TYPE ) ;

         prefix = SUMA_AfniPrefix(Opt->in_name, NULL, NULL, NULL); 
         if (!prefix) { SUMA_SL_Err("Bad prefix!!!"); exit(1); }
         spatprefix = SUMA_append_string(prefix, "_SpatNorm_OrigSpace");
         EDIT_dset_items( Opt->OrigSpatNormedSet ,
                            ADN_prefix      , spatprefix ,
                            ADN_datum_all   , imout_orig->kind ,
                            ADN_nxyz        , nxyz ,
                            ADN_xyzdel      , dxyz ,
                            ADN_xyzorg      , orgxyz ,
                            ADN_xyzorient   , orixyz ,
                            ADN_malloc_type , DATABLOCK_MEM_MALLOC ,
                            ADN_view_type   , VIEW_ORIGINAL_TYPE ,
                            ADN_type        , HEAD_ANAT_TYPE ,
                            ADN_func_type   , ANAT_BUCK_TYPE ,
                          ADN_none ) ;

         EDIT_substitute_brick( Opt->OrigSpatNormedSet , 0 , imout_orig->kind , mri_data_pointer(imout_orig) ) ;      
         oset = r_new_resam_dset ( Opt->OrigSpatNormedSet, Opt->iset,	0,	0,	0,	NULL, MRI_LINEAR, NULL, 1);
         if (!oset) {
            fprintf(stderr,"**ERROR: Failed to reslice!?\n"); exit(1);
         }
         EDIT_dset_items(  oset ,
                            ADN_prefix      , spatprefix,
                            ADN_none ) ;
         tross_Copy_History( Opt->OrigSpatNormedSet , oset ) ;
         DSET_delete(Opt->OrigSpatNormedSet); 
         Opt->OrigSpatNormedSet = oset; oset = NULL;

         if (Opt->WriteSpatNorm) {
            if (Opt->debug) 
               SUMA_S_Note("Writing SpatNormed dset in original space");
            if (Opt->xyz_scale != 1.0f) 
                  THD_volDXYZscale(Opt->OrigSpatNormedSet->daxes,
                                  1.0/Opt->xyz_scale, 0);

            DSET_write(Opt->OrigSpatNormedSet) ;
            /* now rescale back up in case it is to be reused */
            if (Opt->xyz_scale != 1.0f) 
                  THD_volDXYZscale(Opt->OrigSpatNormedSet->daxes,
                                   Opt->xyz_scale, 0);
         }
         if (prefix) SUMA_free(prefix); prefix = NULL;
         if (spatprefix) SUMA_free(spatprefix); spatprefix = NULL; 
      }
            
      oset = EDIT_empty_copy( NULL ) ;
      /* reset the idcode using a hash of a string formed 
      by idcode or orig dset and _Spatnorm */
      {  char idstr[500], *nid=NULL; 
         sprintf(idstr,"%s_Spatnorm", Opt->iset->idcode.str); 
         SUMA_NEW_ID(nid, idstr); 
         strncpy(oset->idcode.str, nid, IDCODE_LEN); 
         SUMA_free(nid); nid = NULL;}
      tross_Copy_History( Opt->iset , oset ) ;
      tross_Make_History( "3dSkullStrip" , argc,argv , oset ) ;
      
      LOAD_IVEC3( nxyz   , imout->nx    , imout->ny    , imout->nz    ) ;
      LOAD_FVEC3( dxyz   , imout->dx    , imout->dy    , imout->dz    ) ;
      LOAD_FVEC3( orgxyz , imout->xo    , imout->yo    , imout->zo    ) ;
      LOAD_IVEC3( orixyz , ORI_R2L_TYPE , ORI_A2P_TYPE , ORI_I2S_TYPE ) ;
      
      prefix = SUMA_AfniPrefix(Opt->in_name, NULL, NULL, NULL); 
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

      EDIT_substitute_brick(oset , 0 , 
                            imout->kind , mri_data_pointer(imout) ) ;      
      if (Opt->WriteSpatNorm) {
         SUMA_LH("Writing SpatNormed dset");
         if (Opt->xyz_scale != 1.0f) 
            THD_volDXYZscale(oset->daxes, 1.0/Opt->xyz_scale, 0);
         DSET_write(oset) ;
         /* and rescale back up for further usage */
         if (Opt->xyz_scale != 1.0f) 
            THD_volDXYZscale(oset->daxes, Opt->xyz_scale, 0);
      }
      Opt->in_vol = oset; oset = NULL;
      
      if (prefix) SUMA_free(prefix); prefix = NULL;
      if (spatprefix) SUMA_free(spatprefix); spatprefix = NULL;
      if( Opt->debug ) 
            fprintf(SUMA_STDERR,
                     "%s: -spatnorm: Expecting %d voxels"
                     " in in_vol dset (%d %d %d)\n", 
                     FuncName, 
                     DSET_NVOX( Opt->in_vol ), DSET_NX( Opt->in_vol ), 
                     DSET_NY( Opt->in_vol ), DSET_NZ( Opt->in_vol )); 
   } else {
      /* volume already SpatNormed, or user does not want SpatNorm */
      SUMA_SL_Note("Loading dset, performing no Spatial Normalization");
      Opt->in_vol = THD_open_dataset( Opt->in_name );
      if( !ISVALID_DSET(Opt->in_vol) ){
        fprintf(stderr,"**ERROR: can't open dataset %s\n",Opt->in_name) ;
        exit(1);
      }
      if( Opt->debug ) 
         fprintf(SUMA_STDERR,
            "%s: -no_spatnorm: Expecting %d voxels in in_vol dset (%d %d %d)\n",
            FuncName, 
            DSET_NVOX( Opt->in_vol ), 
            DSET_NX( Opt->in_vol ), 
            DSET_NY( Opt->in_vol ), 
            DSET_NZ( Opt->in_vol )); 
      /* load the dset */
      DSET_load(Opt->in_vol);
      
      /* if have marmoset, scale coords by 2.5 and pretend tiz a monkey */
      if (Opt->specie == MARMOSET) {
         if (Opt->debug)
            SUMA_S_Warn("Performing temporary scaling of axes. \n"
                     "-visual mode and other mm related parameters\n"
                     "may be off...\n");
         Opt->xyz_scale = 2.5;
         THD_volDXYZscale(Opt->in_vol->daxes, Opt->xyz_scale, 0);
         Opt->specie = MONKEY;   /* now pretend it is a monkey! */
      }

      if (Opt->fillhole) 
         Opt->OrigSpatNormedSet = Opt->in_vol; /* original is same as in_vol */
      /* initialize, just to make sure numbers are ok for if statement below */
      mri_speciebusiness(Opt->specie);
      mri_brainormalize_initialize(
         Opt->in_vol->daxes->xxdel, 
         Opt->in_vol->daxes->yydel, 
         Opt->in_vol->daxes->zzdel);
      if (  DSET_NX( Opt->in_vol) !=  THD_BN_nx() || 
            DSET_NY( Opt->in_vol) !=  THD_BN_ny() || 
            DSET_NZ( Opt->in_vol) !=  THD_BN_nz() ) {
         fprintf(SUMA_STDERR,
            "Error %s:\n SpatNormed Dset must be %d x %d x %d\n",
             FuncName, THD_BN_nx(), THD_BN_ny(), THD_BN_nz() );
         exit(1);
      }
      if (Opt->d1 < 0) {
         Opt->d1 = 20.0/THD_BN_rat(); /* half as big */
      }
      if (Opt->d4 < 0) {
         Opt->d4 = 15.0/THD_BN_rat(); /* half as big */
      }
      if (LocalHead) 
         fprintf(SUMA_STDERR,"%s: Size factor = %f\n", FuncName, THD_BN_rat());

      Opt->iset_hand = SUMA_THD_handedness( Opt->in_vol );
      if (LocalHead) fprintf(SUMA_STDERR,"%s: Handedness of orig dset %d\n", FuncName, Opt->iset_hand);

   }
   
   /* set the travel step based on the resolution of the voxels */
   Opt->travstp = SUMA_MIN_PAIR(SUMA_ABS(Opt->in_vol->daxes->xxdel), SUMA_ABS(Opt->in_vol->daxes->yydel));
   Opt->travstp = SUMA_MIN_PAIR(Opt->travstp, SUMA_ABS(Opt->in_vol->daxes->zzdel));
    
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

   /* calculate an edge mask ? */
   if (Opt->Use_emask) {
      float *emask_sort = NULL, PercRange[2]= { 10, 90 }, PercRangeVal[2] = { 0.0, 0.0 }, PercTh=-1.0;
      
      Opt->emask = (float *) SUMA_malloc( sizeof(float)*DSET_NVOX(Opt->in_vol));
      if (!Opt->emask) {
         fprintf(SUMA_STDERR,"Error %s:\n Failed to allocate for edge mask\n", FuncName);
         exit(1);
      }
      {   
         if (0) { /* to get a fattened mask too */
            THD_3dim_dataset *outset=NULL;
            THD_3dim_dataset *fatoutset=NULL;
            if (!SUMA_3dedge3(Opt->in_vol, Opt->emask, &outset)) {
               fprintf(SUMA_STDERR , "ERROR: gradient extraction failed.\n" );
               exit( 1 );
            }
            /* create a fat mask please */
            {  /* need a block for compiling     18 Apr 2006 [rickr] */
               int code[3], ncode;
               MCW_cluster *nbhd=NULL ;

               if (Opt->debug) {
                  SUMA_S_Note("Fattening the edge mask");
               }
               code[0] = NSTAT_MEAN; 
               ncode = 1;
               nbhd = MCW_rectmask ( 1.0f, 1.0f, 1.0f , 1.0f, 1.0f, 1.0f  ) ;
               fatoutset = THD_localstat( outset , NULL , nbhd , 1 , code, NULL ) ;
               Opt->fatemask = (float *) SUMA_malloc( sizeof(float)*DSET_NVOX(fatoutset));
               if (!Opt->fatemask) {
                  fprintf(SUMA_STDERR,"Error %s:\n Failed to allocate for fat edge mask\n", FuncName);
                  exit(1);
               }
               EDIT_coerce_scale_type( 
                     DSET_NVOX(fatoutset) ,
                     DSET_BRICK_FACTOR(fatoutset,0) ,
                     DSET_BRICK_TYPE(fatoutset,0), 
                     DSET_ARRAY(fatoutset, 0) ,   /* input  */
                     MRI_float               , Opt->fatemask  ) ;/* output */

               if (DSET_NVOX(fatoutset) != DSET_NVOX(outset) || DSET_NVOX(fatoutset) != DSET_NVOX(Opt->in_vol)) {
                  SUMA_S_Err("Bad news in tennis shoes!");
                  exit(1);
               }
               if (Opt->debug > 2) {
                  THD_write_3dim_dataset( NULL,NULL , fatoutset , True ) ;
                  fprintf(stderr,"  ++ Wrote output: %s\n",DSET_BRIKNAME(fatoutset)) ;   
               }
            }

            if (outset) DSET_delete(outset) ; outset = NULL;
            if (fatoutset) DSET_delete(fatoutset) ; fatoutset = NULL;
         } else {
            /* no fat junk */
            if (!SUMA_3dedge3(Opt->in_vol, Opt->emask, NULL)) {
               fprintf(SUMA_STDERR , "ERROR: gradient extraction failed.\n" );
               exit( 1 );
            }
         }
      }
      /* get the mask threshold */
      PercRange[0] = 92; PercRange[1] = 99.999;
      emask_sort = SUMA_PercRange (Opt->emask, NULL, DSET_NVOX(Opt->in_vol), PercRange, PercRangeVal, NULL);
      if (!emask_sort) {
         fprintf( stderr, "ERROR: mask sorting failed.\n" );
         exit( 1 );
      } else {
         SUMA_free(emask_sort); emask_sort = NULL;
      }
      /* The minimum acceptable edge is at least one tenth of the 99.999 percentile edge */
      if (PercRangeVal[0] < PercRangeVal[1]/10.0) PercRangeVal[0] = PercRangeVal[1]/10.0;
      
      if (Opt->efrac >= 0.0) {
         PercTh = PercRangeVal[0]+(PercRangeVal[1]-PercRangeVal[0])*Opt->efrac;
         if (Opt->debug) fprintf (SUMA_STDERR,  "%s: Edge threshold set to %f. (minimum acceptable was %f)\n"
                                             "      (%f percentile =%f, %f percentile = %f)\n", 
                        FuncName, PercTh, PercRangeVal[1]/10.0, PercRange[0], PercRangeVal[0], PercRange[1], PercRangeVal[1]);
      } else {
         PercTh = PercRangeVal[0];
         if (Opt->debug) fprintf (SUMA_STDERR,  "%s: Edge threshold set to %f. (minimum acceptable was %f)\n"
                                             "      (%f percentile =%f, %f percentile = %f)\n", 
                        FuncName, PercTh, PercRangeVal[1]/10.0, PercRange[0], PercRangeVal[0], PercRange[1], PercRangeVal[1]);
      }
      /* Now that we have an edge vector, select appropriate values */
      for (ii=0; ii<DSET_NVOX(Opt->in_vol); ++ii) {
         if (Opt->emask[ii] < PercTh) Opt->emask[ii] = 0.0;
      }
      if (Opt->fatemask) {
         /* same for the fat mask */
         PercRange[0] = 90; PercRange[1] = 95;
         emask_sort = SUMA_PercRange (Opt->fatemask, NULL, DSET_NVOX(Opt->in_vol), PercRange, PercRangeVal, NULL);
         if (!emask_sort) {
            fprintf( stderr, "ERROR: mask sorting failed.\n" );
            exit( 1 );
         } else {
            SUMA_free(emask_sort); emask_sort = NULL;
         }

         if (Opt->debug) fprintf (SUMA_STDERR,"%s: Fat Edge threshold set to %f (from %f percentile) , (%f percentile = %f)\n", 
                           FuncName, PercRangeVal[0], PercRange[0],  PercRange[1], PercRangeVal[1]);
         /* Now that we have an edge vector, select appropriate values */
         for (ii=0; ii<DSET_NVOX(Opt->in_vol); ++ii) {
            if (Opt->fatemask[ii] < PercRangeVal[0]) Opt->fatemask[ii] = 0.0;
         }
      }
   } 
  
   if (Opt->blur_fwhm) {
     if (Opt->debug) fprintf (SUMA_STDERR,"%s: Blurring...\n", FuncName);
     EDIT_blur_volume(  DSET_NX(Opt->in_vol), DSET_NY(Opt->in_vol), DSET_NZ(Opt->in_vol),
                        SUMA_ABS((DSET_DX(Opt->in_vol))), SUMA_ABS((DSET_DY(Opt->in_vol))),  SUMA_ABS((DSET_DZ(Opt->in_vol))),  
                        DSET_BRICK_TYPE(Opt->in_vol,0), DSET_ARRAY(Opt->in_vol,0), 0.42466090*Opt->blur_fwhm) ;
   }
   
   if (Opt->UseSkull) { SOhull = SUMA_Alloc_SurfObject_Struct(1); }
   else SOhull = NULL;
   sprintf(stmphull,"OuterHull");  

   if (Opt->debug) fprintf (SUMA_STDERR,"%s: Prepping volume...\n", FuncName);
   vol = SUMA_LoadPrepInVol (Opt, &SOhull);
   if ( vol <= 0 ) {
      SUMA_S_Err("Failed to load/prep volume");
      exit(1);
   } else {
      if (LocalHead) fprintf (SUMA_STDERR,"%s: Got me a volume of %f mm3\n", FuncName, vol);
   }
   
   /* create the ico, might need coords for bias correction below*/
   SO = SUMA_CreateIcosahedron (Opt->r/2.0, Opt->Icold, Opt->cog, "n", 1);
   if (!SO) {
      SUMA_S_Err("Failed to create Icosahedron");
      exit(1);
   }         
   /* Need Brain_Contour holder at the very least*/
   if (!Opt->Brain_Contour) Opt->Brain_Contour = (float *)SUMA_calloc(SO->N_Node * 3, sizeof(float));


   /* allocate and initialize shrink bias vector */
   {
      int nico = (2 + 10 * Opt->Icold * Opt->Icold );
      Opt->shrink_bias = (float *)SUMA_calloc(nico, sizeof(float));
      if (!Opt->shrink_bias) { SUMA_SL_Crit("Failed to allocate"); exit(1); }
      for (i=0; i<nico; ++i) Opt->shrink_bias[i] = 1.0;

      if (Opt->shrink_bias_name) {
         MRI_IMAGE *im = NULL;
         float *far = NULL;

         /* load the shrink_bias_file */
         im = mri_read_1D(Opt->shrink_bias_name);
         if (!im) { 
            SUMA_SL_Err("Failed to read 1D file of shrink factor bias file.");
            exit(1);
         }
         far = MRI_FLOAT_PTR(im);
         if (im->ny != 2) { 
            SUMA_SL_Err("Need 2 columns in shrink factor bias file."); 
            exit(1); 
         }
         if (im->nx > nico) {
            fprintf(SUMA_STDERR, 
                        "Error %s: File too big. \n"
                        "Maximum number of lines (%d) should not exceed %d,\n"
                        "which is the number of nodes forming the surface.\n",
                        FuncName, im->nx, nico);
            exit(1);
         }

         /* change to row major major and make it match nodeind */
         for (i=0; i<im->nx; ++i) {
            if (far[i] < nico) {
               Opt->shrink_bias[(int)far[i]] = far[i+  im->nx];
            } else {
               fprintf( SUMA_STDERR, 
                        "Error %s: Node index of (%d) \n"
                        "in shrink factor bias file\n"
                        "exceeds maximum allowed node \n"
                        "index (%d) for surface.\n", 
                        FuncName, (int)far[i], nico-1);
               exit(1);
            }
         }
         mri_free(im); im = NULL;   /* done with that baby */

      } else if (Opt->specie == RAT) {
      #if 0
         float p1[3], Y[3], Z[3], U[3], Un, dotz, doty;
         Y[0] = 0.0; Y[1] = 1.0; Y[2] = 0.0; /* The Y direction */
         Z[0] = 0.0; Z[1] = 0.0; Z[2] = 1.0; /* The Z direction */
         /* less on top, and sides, more in front */
         for (i=0; i<SO->N_Node; ++i) {
            /* vector from center to node */
            p1[0] = SO->NodeList[3*i  ]; p1[1] = SO->NodeList[3*i+1]; p1[2] = SO->NodeList[3*i+2];
            SUMA_UNIT_VEC(Opt->cog, p1, U, Un);  
            SUMA_DOTP_VEC(U,Y, doty , 3,float,float);
            SUMA_DOTP_VEC(U,Z, dotz , 3,float,float);
            Opt->shrink_bias[i] = ((1.5 - (SUMA_ABS(doty)*(1-SUMA_ABS(dotz))))) ;
         }
         if (Opt->debug) {
            snprintf(stmp, 198, "%s_shrink_bias.1D.dset", Opt->out_vol_prefix);
            SUMA_WRITE_ARRAY_1D(Opt->shrink_bias,SO->N_Node,1, stmp);        
         }
      #endif
      } else if (Opt->specie == MONKEY) {
      #if 0
         float p1[3], Y[3], Z[3], U[3], Un, dotz, doty;
         Y[0] = 0.0; Y[1] = 1.0; Y[2] = 0.0; /* The Y direction */
         Z[0] = 0.0; Z[1] = 0.0; Z[2] = 1.0; /* The Z direction */
         /* less on top, and sides, more in front */
         for (i=0; i<SO->N_Node; ++i) {
            /* vector from center to node */
            p1[0] = SO->NodeList[3*i  ]; p1[1] = SO->NodeList[3*i+1]; p1[2] = SO->NodeList[3*i+2];
            SUMA_UNIT_VEC(Opt->cog, p1, U, Un);  
            SUMA_DOTP_VEC(U,Y, doty , 3,float,float);
            SUMA_DOTP_VEC(U,Z, dotz , 3,float,float);
            if (doty < 0 && dotz > 0) { /* loosen front upper of brain */
               Opt->shrink_bias[i] = ((1.1 - (SUMA_ABS(doty)*(1-(dotz))))) ;
            } 
         }
         if (Opt->debug) {
            snprintf(stmp, 198, "%s_shrink_bias.1D.dset", Opt->out_vol_prefix);
            SUMA_WRITE_ARRAY_1D(Opt->shrink_bias,SO->N_Node,1, stmp);        
         }
      #endif
      } else if (Opt->specie == MARMOSET) { }

   } 
   if (Opt->debug) fprintf (SUMA_STDERR,"%s: Beginning brain extraction...\n", FuncName);
   do {   
      /* Now create that little sphere that is about to expand */
      sprintf(stmp,"icobaby_ld%d", Opt->Icold);  
      if (SO) {
         if (Opt->debug) fprintf (SUMA_STDERR,"%s: Have surface, OK for 1st entry.\n", FuncName);
      }  else {   
         if (Opt->debug) fprintf (SUMA_STDERR,"%s: Creating Ico.\n", FuncName);
         SO = SUMA_CreateIcosahedron (Opt->r/2.0, Opt->Icold, Opt->cog, "n", 1);
         if (!SO) {
            SUMA_S_Err("Failed to create Icosahedron");
            exit(1);
         }
      }

      if (Opt->Stop) SUMA_free(Opt->Stop); Opt->Stop = NULL;
      if (!Opt->Stop) {
         Opt->Stop = (float *)SUMA_calloc(SO->N_Node, sizeof(float));
         if (!Opt->Stop) {
            SUMA_SL_Crit("Failed to allocate");
            exit(1);
         }
      }

      if (Opt->avoid_vent && Opt->specie != RAT) {  /* No need for this
                                                      avoidance in rodents*/
         float U[3], Un, *a, P2[2][3], den=3;
         if (Opt->avoid_vent == 2) den = 10.0;
         else den = 3.0;
         if (Opt->debug) 
            fprintf (SUMA_STDERR,   
                     "%s: Stretching to avoid ventricles.\n"
                     "SO->Center is [%f %f %f]\n", 
                     FuncName, SO->Center[0], SO->Center[1], SO->Center[2]);
         for (i=0; i<SO->N_Node; ++i) {
            /* stretch the top coordinates by d1 and the back too*/
            a = &(SO->NodeList[3*i]); 
            if (  a[2] - SO->Center[2] > Opt->r/den || 
                  a[1] - SO->Center[1] > Opt->r/2) {
               SUMA_UNIT_VEC(SO->Center, a, U, Un);
               SUMA_POINT_AT_DISTANCE_NORM(U, SO->Center, (Un+1.1*Opt->d1), P2);
               SO->NodeList[3*i] = P2[0][0]; 
               SO->NodeList[3*i+1] = P2[0][1]; 
               SO->NodeList[3*i+2] = P2[0][2];
            }
         }
      }
      /* allocate and fix zt */
      Opt->ztv = (float *)SUMA_malloc(sizeof(float)*SO->N_Node);
      if (!Opt->ztv) {
         SUMA_SL_Crit("Failed to allocate");
         exit(1);
      } 
      for (i=0; i<SO->N_Node; ++i) 
         Opt->ztv[i] = SUMA_MIN_PAIR(1.0, (Opt->Zt*Opt->shrink_bias[i]));

      /* need sv for communication to AFNI */
      SO->VolPar = SUMA_VolParFromDset (Opt->in_vol);
      SO->SUMA_VolPar_Aligned = YUP; 
            /* Surface is in alignment with volume, 
               should not call SUMA_Align_to_VolPar ... */
      if (!SO->State) {SO->State = SUMA_copy_string("3dSkullStrip"); }
      if (!SO->Group) {SO->Group = SUMA_copy_string("3dSkullStrip"); }
      if (!SO->Label) {SO->Label = SUMA_copy_string(stmp); }
      /* make the idcode_str depend on the Label, it is convenient to
      send the same surface all the time to SUMA */
      if (SO->Label) { 
         if (SO->idcode_str) SUMA_free(SO->idcode_str); 
         SO->idcode_str = NULL; 
         SUMA_NEW_ID(SO->idcode_str, SO->Label); 
      }
      
      if (!nint && SOhull) {
         SOhull->VolPar = SUMA_VolParFromDset (Opt->in_vol);
         SOhull->SUMA_VolPar_Aligned = YUP; 
               /* Surface is in alignment with volume, 
               should not call SUMA_Align_to_VolPar ... */
   
         if (!SOhull->State) {SOhull->State = SUMA_copy_string("3dSkullStrip"); }
         if (!SOhull->Group) {SOhull->Group = SUMA_copy_string("3dSkullStrip"); }
         if (!SOhull->Label) {SOhull->Label = SUMA_copy_string(stmphull); }
         if (SOhull->Label) { 
            if (SOhull->idcode_str) SUMA_free(SOhull->idcode_str); 
            SOhull->idcode_str = NULL;       
            SUMA_NEW_ID(SOhull->idcode_str,SOhull->Label); 
         }
      }

      /* see if SUMA talk is turned on */
      if (ps->cs->talk_suma) {
         ps->cs->istream = SUMA_BRAINWRAP_LINE;
         ps->cs->afni_istream = SUMA_AFNI_STREAM_INDEX2;
         ps->cs->kth = 1; /* make sure all surfaces get sent */
         if (!SUMA_SendToSuma (NULL, ps->cs, NULL, SUMA_NO_DSET_TYPE, 0)) {
            SUMA_SL_Err("Failed to initialize SUMA_SendToSuma");
            ps->cs->Send = NOPE;
            ps->cs->afni_Send = NOPE;
            ps->cs->talk_suma = NOPE;
         } else if (!SUMA_SendToAfni (ps->cs, NULL,  0)) {
            SUMA_SL_Err("Failed to initialize SUMA_SendToAfni");
            ps->cs->afni_Send = NOPE;
            ps->cs->Send = NOPE;
         } else {
            /* send in_vol to afni */
            if (Opt->DoSpatNorm && ps->cs->afni_Send) {
               SUMA_SL_Note("Sending spatnormed volume to AFNI");
               if (!SUMA_SendToAfni(ps->cs, Opt->in_vol, 1)) {
                  SUMA_SL_Err("Failed to send volume to AFNI");
                  ps->cs->afni_Send = NOPE;
               }
            }

            if (!nint && SOhull) {
               if (Opt->send_hull) {
                  SUMA_LH("Sending Hull");
                  if (Opt->DemoPause == SUMA_3dSS_DEMO_PAUSE) { 
                     SUMA_PAUSE_PROMPT("Sending HULL next"); 
                  }
                  SUMA_SendSumaNewSurface(SOhull, ps->cs);
               }
            }
            SUMA_LH("Sending Ico");
            if (Opt->DemoPause  == SUMA_3dSS_DEMO_PAUSE) { 
               SUMA_PAUSE_PROMPT("Sending Ico next"); 
            }
            SUMA_SendSumaNewSurface(SO, ps->cs);

         }
         ps->cs->kth = kth_buf;
      }

      if (!nint && Opt->UseSkull) {
         /* get a crude mask of outer skull */
         if (Opt->DemoPause == SUMA_3dSS_DEMO_PAUSE) {
            SUMA_PAUSE_PROMPT("Shrinking skull hull next"); 
         }
         SUMA_SkullMask (SOhull, Opt, ps->cs);
         /* Now take mask and turn it into a volume */
         fprintf (SUMA_STDERR,
                  "%s: Locating voxels on skull boundary  ...\n", FuncName);
         isin = SUMA_FindVoxelsInSurface (SOhull, SO->VolPar, &N_in, 0, NULL);
         for (i=0; i<SO->VolPar->nx*SO->VolPar->ny*SO->VolPar->nz; ++i) { 
            if (isin[i] <= SUMA_ON_NODE) Opt->fvec[i] = 0; 
         }
         #if 0
            SUMA_SL_Note("Writing hull mask");
            {
               FILE *fout=fopen("hullmask.1D","w");
               int ii, jj, kk; 
               for (i=0; i<SO->VolPar->nx*SO->VolPar->ny*SO->VolPar->nz; ++i) { 
                  SUMA_1D_2_3D_index(  i, ii, jj, kk, 
                                       SO->VolPar->nx, 
                                       SO->VolPar->nx*SO->VolPar->ny); 
                  fprintf(fout,"%d %d %d %d\n",ii, jj, kk, isin[i]); 
               }
               fclose(fout);
            }
         #endif
         if (isin) SUMA_free(isin); isin = NULL;
      }
            
            
      /* This is it baby, start walking */
      if (Opt->DemoPause == SUMA_3dSS_DEMO_PAUSE) { 
         SUMA_PAUSE_PROMPT("Brain expansion next"); }
      if ( !Opt->UseThisBrain ) {
         SUMA_StretchToFitLeCerveau (SO, Opt, ps->cs);
      } else {
         /* DEVELOPMENT ONLY! */
         int ncol, nrow;
         float *far=SUMA_Load1D_s(Opt->UseThisBrain, &ncol, &nrow, 1, 0);
         if (!far || nrow != SO->N_Node || ncol != 3) {
            fprintf(SUMA_STDERR,"Error %s: SO has %d nodes, your coord file has %d cols, %d rows.\n", FuncName, SO->N_Node, ncol, nrow);
            exit(1);
         } else {
            fprintf(SUMA_STDERR,"Warning %s: adopting coordinates from %s\n", FuncName, Opt->UseThisBrain);
         }
         memcpy((void*)SO->NodeList,(void*)far, sizeof(float)*ncol*nrow);
         free(far); far = NULL;
         Opt->PercInt = -1; /* cancel intersection checking */ 
      }
      /* Make a copy to brain contours, in case user jumps ahead */
      memcpy((void*)Opt->Brain_Contour, (void *)SO->NodeList, SO->N_Node * 3 * sizeof(float));
      
      /* check for intersections */
      if (Opt->PercInt >= 0) {
         if (Opt->debug) fprintf(SUMA_STDERR,"%s: Checking for self intersection...\n", FuncName);
         nseg = 30 * Opt->Icold * Opt->Icold; /* number of segments in Ico */
         nint = SUMA_isSelfIntersect(SO, (int)(Opt->PercInt * nseg / 100.0), NULL);
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
            if (Opt->debug) {
               if (nint) fprintf(SUMA_STDERR,"%s: Number of intersection below criterion.\n", FuncName);
               else fprintf(SUMA_STDERR,"%s: No intersections found.\n", FuncName);
            }
            nint = 0;
         }
      } else {
         fprintf(SUMA_STDERR,"%s: Self intersection check turned off.\n", FuncName);
         nint = 0;   
      }
   } while (nint != 0);
   
   if ( !Opt->UseThisBrain ) {
      if (Opt->debug) fprintf(SUMA_STDERR,"%s: Final smoothing of %d\n", FuncName, Opt->NNsmooth);
      if (SUMA_DidUserQuit()) {
         if (Opt->debug) fprintf(SUMA_STDERR,"%s: straight to end per user demand (%d)...\n", FuncName, SUMA_DidUserQuit());
         goto FINISH_UP;
      }
      /* touch up, these might cause some surface intersection, but their effects should be small */
      /* TOUCHUP_1: */
      if (Opt->UseNew) {
            double mval = 255;
            if (Opt->debug) fprintf (SUMA_STDERR,"%s: Touchup correction, pass 1 ...\n", FuncName);
            if (Opt->DemoPause  == SUMA_3dSS_DEMO_PAUSE) { SUMA_PAUSE_PROMPT("touchup correction next"); }
            if (Opt->DemoPause == SUMA_3dSS_INTERACTIVE) {
               /* Make a copy to brain contours, in case user jumps ahead */
               memcpy((void*)Opt->Brain_Contour, (void *)SO->NodeList, SO->N_Node * 3 * sizeof(float));
               fprintf (SUMA_STDERR,"3dSkullStrip Interactive: \n"
                                    "Touchup, pass 1.\n"
                                    "Do you want to (C)ontinue, (P)ass or (S)ave this? ");
               cbuf = SUMA_ReadCharStdin ('c', 0,"csp");
               switch (cbuf) {
                  case 's':
                     fprintf (SUMA_STDERR,"Saving mask as is.\n");
                     goto FINISH_UP;
                     break;
                  case 'p':
                     fprintf (SUMA_STDERR,"Passing this stage \n");
                     goto PUSH_TO_EDGE;
                     break;
                  case 'c':
                     fprintf (SUMA_STDERR,"Continuing with stage.\n");
                     break;
               }                 
            }
            /* recover the eye balls please */
            if (mval < Opt->t98) {
               SUMA_SL_Warn("Looks like some values in dset might be larger than 255 !");
               mval = Opt->t98+10;
            }
            if (Opt->k98maskcnt && Opt->k98mask) { for (ii=0; ii<Opt->k98maskcnt; ++ii) Opt->fvec[Opt->k98mask[ii]] = mval; }
            /* SUMA_REPOSITION_TOUCHUP(6);*/
            SUMA_Reposition_Touchup(SO, Opt, 6, ps->cs);
            if (LocalHead) fprintf (SUMA_STDERR,"%s: Touchup correction  Done.\n", FuncName);
      }

      PUSH_TO_EDGE:
      if (Opt->PushToEdge) {
         fprintf (SUMA_STDERR,"%s: Pushing to Edge ...\n", FuncName);
         ps->cs->kth = 1; /*make sure all gets sent at this stage */
         if (Opt->DemoPause  == SUMA_3dSS_DEMO_PAUSE) { SUMA_PAUSE_PROMPT("Push To Edge Correction"); }
         if (Opt->debug) fprintf (SUMA_STDERR,"%s: Push to edge correction ...\n", FuncName);
         if (Opt->DemoPause == SUMA_3dSS_INTERACTIVE) {
               /* Make a copy to brain contours, in case user jumps ahead */
               memcpy((void*)Opt->Brain_Contour, (void *)SO->NodeList, SO->N_Node * 3 * sizeof(float));
               fprintf (SUMA_STDERR,"3dSkullStrip Interactive: \n"
                                    "Push To Edge.\n"
                                    "Do you want to (C)ontinue, (P)ass or (S)ave this?  ");
               cbuf = SUMA_ReadCharStdin ('c', 0,"csp");
               fprintf (SUMA_STDERR,"%c\n", cbuf);
               switch (cbuf) {
                  case 's':
                     fprintf (SUMA_STDERR,"Saving mask as is.\n");
                     goto FINISH_UP;
                     break;
                  case 'p':
                     fprintf (SUMA_STDERR,"Passing this stage \n");
                     goto BEAUTY;
                     break;
                  case 'c':
                     fprintf (SUMA_STDERR,"Continuing with stage.\n");
                     break;
               }                 
         }
         {  
            int il = 0;
            float dtroub = 1.0;
            int N_troub = 1, past_N_troub = 0;
            while (il < 5 && dtroub > 0.1 && N_troub) {
               N_troub = SUMA_PushToEdge(SO, Opt, 4, ps->cs, 0); SUMA_RECOMPUTE_NORMALS(SO);
               if (ps->cs->Send) {
                  if (!SUMA_SendToSuma (SO, ps->cs, (void *)SO->NodeList, SUMA_NODE_XYZ, 1)) {
                     SUMA_SL_Warn("Failed in SUMA_SendToSuma\nCommunication halted.");
                  }
               }
               if (!past_N_troub) { 
                  past_N_troub = N_troub; 
                  if (LocalHead) fprintf (SUMA_STDERR,"%s: \n PushToEdge, pass %d %d troubled nodes, going for more...\n", FuncName, il, N_troub);
               } else {
                  dtroub = (float)(past_N_troub - N_troub)/(float)past_N_troub;
                  if (LocalHead) fprintf (SUMA_STDERR,"%s: \n PushToEdge, pass %d : %f change in troubled nodes.\n", FuncName, il, dtroub);
                  past_N_troub = N_troub;
               }
             ++il;
           }
         }
         if (LocalHead) fprintf (SUMA_STDERR,"%s: Edge push correction  Done.\n", FuncName);
         ps->cs->kth = kth_buf; 
      }


      BEAUTY:
      /* smooth the surface a bit */
      if (Opt->smooth_end) {
         ps->cs->kth = 1;  /*make sure all gets sent at this stage */
         if (Opt->DemoPause  == SUMA_3dSS_DEMO_PAUSE) { SUMA_PAUSE_PROMPT("beauty treatment smoothing next"); }
         if (Opt->debug) fprintf (SUMA_STDERR,"%s: The beauty treatment smoothing.\n", FuncName);
         if (Opt->DemoPause == SUMA_3dSS_INTERACTIVE) {
               /* Make a copy to brain contours, in case user jumps ahead */
               memcpy((void*)Opt->Brain_Contour, (void *)SO->NodeList, SO->N_Node * 3 * sizeof(float));
               fprintf (SUMA_STDERR,"3dSkullStrip Interactive: \n"
                                    "Beauty treatment smoothing.\n"
                                    "Do you want to (C)ontinue, (P)ass or (S)ave this?  ");
               cbuf = SUMA_ReadCharStdin ('c', 0,"csp");
               switch (cbuf) {
                  case 's':
                     fprintf (SUMA_STDERR,"Saving mask as is.\n");
                     goto FINISH_UP;
                     break;
                  case 'p':
                     fprintf (SUMA_STDERR,"Passing this stage \n");
                     goto TOUCHUP_2;
                     break;
                  case 'c':
                     fprintf (SUMA_STDERR,"Continuing with stage.\n");
                     break;
               }                 
            }
         dsmooth = SUMA_Taubin_Smooth( SO, NULL,
                                       0.6307, -.6732, SO->NodeList,
                                       Opt->smooth_end, 3, SUMA_ROW_MAJOR, dsmooth, ps->cs, NULL, 0);    
         memcpy((void*)SO->NodeList, (void *)dsmooth, SO->N_Node * 3 * sizeof(float));
         SUMA_RECOMPUTE_NORMALS(SO);
         if (ps->cs->Send) {
            if (!SUMA_SendToSuma (SO, ps->cs, (void *)SO->NodeList, SUMA_NODE_XYZ, 1)) {
               SUMA_SL_Warn("Failed in SUMA_SendToSuma\nCommunication halted.");
            }
         }
         ps->cs->kth = kth_buf; 
      }

      TOUCHUP_2:
      /* one more correction pass */
      if (Opt->UseNew > 1.0) {
         ps->cs->kth = 1; /*make sure all gets sent at this stage */
         if (Opt->DemoPause  == SUMA_3dSS_DEMO_PAUSE) { SUMA_PAUSE_PROMPT("touchup correction 2 next"); }
         if (Opt->debug) fprintf (SUMA_STDERR,"%s: Final touchup correction ...\n", FuncName);
         if (Opt->DemoPause == SUMA_3dSS_INTERACTIVE) {
               /* Make a copy to brain contours, in case user jumps ahead */
               memcpy((void*)Opt->Brain_Contour, (void *)SO->NodeList, SO->N_Node * 3 * sizeof(float));
               fprintf (SUMA_STDERR,"3dSkullStrip Interactive: \n"
                                    "Touchup, pass 2.\n"
                                    "Do you want to (C)ontinue, (P)ass or (S)ave this?  ");
               cbuf = SUMA_ReadCharStdin ('c', 0,"csp");
               fprintf (SUMA_STDERR,"%c\n", cbuf);
               switch (cbuf) {
                  case 's':
                     fprintf (SUMA_STDERR,"Saving mask as is.\n");
                     goto FINISH_UP;
                     break;
                  case 'p':
                     fprintf (SUMA_STDERR,"Passing this stage \n");
                     goto PUSH_TO_OUTER_SKULL;
                     break;
                  case 'c':
                     fprintf (SUMA_STDERR,"Continuing with stage.\n");
                     break;
               }                 
         }
         /* SUMA_REPOSITION_TOUCHUP(2); */
         SUMA_Reposition_Touchup(SO, Opt, 2, ps->cs);

         if (LocalHead) fprintf (SUMA_STDERR,"%s: Final touchup correction  Done.\n", FuncName);
         ps->cs->kth = kth_buf; 
      }
   } else {
      SUMA_S_Note("Using brain read from file");
   }
   

   /* Put brain contours in Brain_Contour*/
   memcpy((void*)Opt->Brain_Contour, (void *)SO->NodeList, SO->N_Node * 3 * sizeof(float));
   
   PUSH_TO_OUTER_SKULL:
   if (Opt->DoSkulls) {
      if (ps->cs->Send) ps->cs->kth = 1; /* make sure all surfaces get sent */
      if (Opt->DemoPause  == SUMA_3dSS_DEMO_PAUSE) { SUMA_PAUSE_PROMPT("Skull detection next"); }
      if (Opt->debug) fprintf (SUMA_STDERR,"%s: Skull detection next ...\n", FuncName);
      if (Opt->DemoPause == SUMA_3dSS_INTERACTIVE) {
            fprintf (SUMA_STDERR,"3dSkullStrip Interactive: \n"
                                 "Skull detection.\n"
                                 "Do you want to (C)ontinue, (P)ass or (S)ave this?  ");
            cbuf = SUMA_ReadCharStdin ('c', 0,"csp");
            fprintf (SUMA_STDERR,"%c\n", cbuf);
            switch (cbuf) {
               case 's':
                  fprintf (SUMA_STDERR,"Saving mask as is.\n");
                  goto FINISH_UP;
                  break;
               case 'p':
                  fprintf (SUMA_STDERR,"Passing this stage \n");
                  goto FINISH_UP;
                  break;
               case 'c':
                  fprintf (SUMA_STDERR,"Continuing with stage.\n");
                  break;
            }                 
      }
      if (!Opt->UseThisBrainHull) {
         /* first get a convex hull volume of the brain surface */
         SUMA_Push_Nodes_To_Hull(SO, Opt, ps->cs, 15);
      } else {
         /* DEVELOPMENT ONLY! */
         int ncol, nrow;
         float *far=SUMA_Load1D_s(Opt->UseThisBrainHull, &ncol, &nrow, 1, 0);
         if (!far || nrow != SO->N_Node || ncol != 3) {
            fprintf(SUMA_STDERR,"Error %s: SO has %d nodes, your coord file has %d cols, %d rows.\n", FuncName, SO->N_Node, ncol, nrow);
            exit(1);
         } else {
            fprintf(SUMA_STDERR,"Warning %s: adopting brain hull coordinates from %s\n", FuncName, Opt->UseThisBrainHull);
         }
         memcpy((void*)SO->NodeList,(void*)far, sizeof(float)*ncol*nrow);
         free(far); far = NULL;
      }
      
      Opt->Brain_Hull = (float *)SUMA_calloc(SO->N_Node * 3, sizeof(float));
      memcpy((void*)Opt->Brain_Hull, (void *)SO->NodeList, SO->N_Node * 3 * sizeof(float));      
      
      if (ps->cs->Send) {
         if (!SUMA_SendToSuma (SO, ps->cs, (void *)SO->NodeList, SUMA_NODE_XYZ, 1)) {
            SUMA_SL_Warn("Failed in SUMA_SendToSuma\nCommunication halted.");
         }
      }
      
      if (!Opt->UseThisSkullOuter) {
         if (Opt->debug) fprintf (SUMA_STDERR,"%s: Push to Outer Skull ...                                               \n", FuncName);
         if (     LocalHead || Opt->debug > 1 
               || Opt->DemoPause  == SUMA_3dSS_DEMO_PAUSE
               || Opt->DemoPause  == SUMA_3dSS_INTERACTIVE
               ) { 
               SUMA_PAUSE_PROMPT("Brain hull done, Pausing before proceeding to outer skull "); 
         }
         {  
            int il = 0;
            float dtroub = 1.0;
            int N_exp, N_troub = 1, past_N_troub = 0;

            /* reset stop flags */
            Opt->nmask = (byte*)SUMA_malloc(sizeof(byte)*SO->N_Node);
            for (il=0; il<SO->N_Node; ++il) Opt->nmask[il] = 1;

            N_exp = 20;
            il = 0;
            while (il < N_exp || (N_troub < 0.01 * SO->N_Node && dtroub < 0.010)) {
               N_troub = SUMA_PushToOuterSkull(SO, Opt, 10+(25.0*(N_exp-il)/(float)N_exp), ps->cs, N_exp); SUMA_RECOMPUTE_NORMALS(SO);
               if (ps->cs->Send) {
                  if (!SUMA_SendToSuma (SO, ps->cs, (void *)SO->NodeList, SUMA_NODE_XYZ, 1)) {
                     SUMA_SL_Warn("Failed in SUMA_SendToSuma\nCommunication halted.");
                  }
               }
               if (LocalHead) fprintf (SUMA_STDERR,"%s: %d trouble nodes reported by SUMA_PushToOuterSkull\n", FuncName, N_troub);
               if (!past_N_troub) { 
                  past_N_troub = N_troub; 
                  if (LocalHead) fprintf (SUMA_STDERR,"%s: \nSUMA_PushToOuterSkull , pass %d %d troubled nodes, going for more...\n", FuncName, il, N_troub);
               } else {
                  dtroub = (float)(past_N_troub - N_troub)/(float)past_N_troub;
                  if (LocalHead) fprintf (SUMA_STDERR,"%s: \nSUMA_PushToOuterSkull, pass %d : %f change in troubled nodes.\n", FuncName, il, dtroub);
                  past_N_troub = N_troub;
               }
             ++il;
           }
         }
         if (LocalHead) fprintf (SUMA_STDERR,"%s: Outer Skull push correction  Done.\n", FuncName);
         ps->cs->kth = kth_buf; 
         dsmooth = SUMA_Taubin_Smooth( SO, NULL,
                                       0.6307, -.6732, SO->NodeList,
                                       Opt->smooth_end, 3, SUMA_ROW_MAJOR, dsmooth, ps->cs, NULL, 0);    
         memcpy((void*)SO->NodeList, (void *)dsmooth, SO->N_Node * 3 * sizeof(float));
         SUMA_RECOMPUTE_NORMALS(SO);
         if (ps->cs->Send) {
            if (!SUMA_SendToSuma (SO, ps->cs, (void *)SO->NodeList, SUMA_NODE_XYZ, 1)) {
               SUMA_SL_Warn("Failed in SUMA_SendToSuma\nCommunication halted.");
            }
         }
      } else {
         /* DEVELOPMENT ONLY! */
         int ncol, nrow;
         float *far=SUMA_Load1D_s(Opt->UseThisSkullOuter, &ncol, &nrow, 1, 0);
         if (!far || nrow != SO->N_Node || ncol != 3) {
            fprintf(SUMA_STDERR,"Error %s: SO has %d nodes, your coord file has %d cols, %d rows.\n", FuncName, SO->N_Node, ncol, nrow);
            exit(1);
         } else {
            fprintf(SUMA_STDERR,"Warning %s: adopting outer skull coordinates from %s\n", FuncName, Opt->UseThisSkullOuter);
         }
         memcpy((void*)SO->NodeList,(void*)far, sizeof(float)*ncol*nrow);
         free(far); far = NULL;
      }
      
      Opt->Skull_Outer = (float *)SUMA_calloc(SO->N_Node * 3, sizeof(float));
      memcpy((void*)Opt->Skull_Outer, (void *)SO->NodeList, SO->N_Node * 3 * sizeof(float));      

      if (     LocalHead || Opt->debug > 1 
               || Opt->DemoPause  == SUMA_3dSS_DEMO_PAUSE
               || Opt->DemoPause  == SUMA_3dSS_INTERACTIVE
               ) { 
         SUMA_PAUSE_PROMPT("Outer Skull done, Pausing before proceeding to inner skull "); 
      }
      /* now for the inner skull */
      {  
         int il = 0;
         /* reset stop flags */
         Opt->nmask = (byte*)SUMA_malloc(sizeof(byte)*SO->N_Node);
         for (il=0; il<SO->N_Node; ++il) Opt->nmask[il] = 1;
         
         ps->cs->kth = 1;
         SUMA_PushToInnerSkull(SO, Opt, 35.0, ps->cs); 
         SUMA_RECOMPUTE_NORMALS(SO);
      }
      if (ps->cs->Send) {
         if (!SUMA_SendToSuma (SO, ps->cs, (void *)SO->NodeList, SUMA_NODE_XYZ, 1)) {
            SUMA_SL_Warn("Failed in SUMA_SendToSuma\nCommunication halted.");
         }
      }
      
      Opt->Skull_Inner = (float *)SUMA_calloc(SO->N_Node * 3, sizeof(float));
      memcpy((void*)Opt->Skull_Inner, (void *)SO->NodeList, SO->N_Node * 3 * sizeof(float));      

      ps->cs->kth = kth_buf; 
   }
   

   

   FINISH_UP:
   /* reset coords to brain contours */
   memcpy((void *)SO->NodeList, (void*)Opt->Brain_Contour, SO->N_Node * 3 * sizeof(float));    
   
   /* send the last surface (kind a stupid since you have som many surfaces) ...*/
   ps->cs->kth = 1;
   if (ps->cs->Send) {
      SUMA_LH("SendSuma");
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
      Opt->SpatShift[0] = xrai_orig - THD_BN_xorg(); Opt->SpatShift[1] = yrai_orig - THD_BN_yorg(); Opt->SpatShift[2] = zrai_orig - THD_BN_zorg();

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
      /* ditto for other layers */
      if (Opt->Brain_Contour) {  
         for (i=0; i<SO->N_Node; ++i) {
            i3 = 3*i;
            Opt->Brain_Contour[i3 + 0] += Opt->SpatShift[0];
            Opt->Brain_Contour[i3 + 1] += Opt->SpatShift[1];
            Opt->Brain_Contour[i3 + 2] += Opt->SpatShift[2];
         }
      }
      
      if (Opt->Brain_Hull) {
         for (i=0; i<SO->N_Node; ++i) {
            i3 = 3*i;
            Opt->Brain_Hull[i3 + 0] += Opt->SpatShift[0];
            Opt->Brain_Hull[i3 + 1] += Opt->SpatShift[1];
            Opt->Brain_Hull[i3 + 2] += Opt->SpatShift[2];
         }
      }
      if (Opt->Skull_Outer) {
         for (i=0; i<SO->N_Node; ++i) {
            i3 = 3*i;
            Opt->Skull_Outer[i3 + 0] += Opt->SpatShift[0];
            Opt->Skull_Outer[i3 + 1] += Opt->SpatShift[1];
            Opt->Skull_Outer[i3 + 2] += Opt->SpatShift[2];
         }
      }
      if (Opt->Skull_Inner) {
         for (i=0; i<SO->N_Node; ++i) {
            i3 = 3*i;
            Opt->Skull_Inner[i3 + 0] += Opt->SpatShift[0];
            Opt->Skull_Inner[i3 + 1] += Opt->SpatShift[1];
            Opt->Skull_Inner[i3 + 2] += Opt->SpatShift[2];
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
   if (strcmp(Opt->out_prefix,"skull_strip_out")) {
      SUMA_LH("Output surfaces");
      if (Opt->Brain_Hull) {
         memcpy((void *)SO->NodeList, (void*)Opt->Brain_Hull, SO->N_Node * 3 * sizeof(float));  
         if (Opt->debug) fprintf (SUMA_STDERR,"%s: Writing Brain Hull surface  ...\n", FuncName);
         if (!SUMA_Save_Surface_Object (SO_name_bhull, SO, Opt->SurfFileType, Opt->SurfFileFormat, NULL)) {
            fprintf (SUMA_STDERR,"Error %s: Failed to write surface object.\n", FuncName);
            exit (1);
         }
      }
      if (Opt->Skull_Outer) {
         memcpy((void *)SO->NodeList, (void*)Opt->Skull_Outer, SO->N_Node * 3 * sizeof(float));  
         if (Opt->debug) fprintf (SUMA_STDERR,"%s: Writing Skull Outer surface  ...\n", FuncName);
         if (!SUMA_Save_Surface_Object (SO_name_oskull, SO, Opt->SurfFileType, Opt->SurfFileFormat, NULL)) {
            fprintf (SUMA_STDERR,
                     "Error %s: Failed to write surface object.\n", FuncName);
            exit (1);
         }
      }
      if (Opt->Skull_Inner) {
         memcpy((void *)SO->NodeList, (void*)Opt->Skull_Inner, 
                SO->N_Node * 3 * sizeof(float));  
         if (Opt->debug) 
            fprintf (SUMA_STDERR,
                     "%s: Writing Skull Inner surface  ...\n", FuncName);
         if (!SUMA_Save_Surface_Object (SO_name_iskull, SO, 
                                        Opt->SurfFileType, Opt->SurfFileFormat, 
                                        NULL)) {
            fprintf (SUMA_STDERR,
                     "Error %s: Failed to write surface object.\n", FuncName);
            exit (1);
         }
      }
      
      /* leave this one at the end since the work afterwards 
         will assume we are working with Brain_Contour in SO->NodeList */ 
      if (Opt->Brain_Contour) {
         memcpy((void *)SO->NodeList, (void*)Opt->Brain_Contour, 
                  SO->N_Node * 3 * sizeof(float));  
         if (Opt->debug) 
            fprintf (SUMA_STDERR,
                     "%s: Writing Brain Contour surface  ...\n", FuncName);
         if (!SUMA_Save_Surface_Object (SO_name, SO, Opt->SurfFileType, 
                                        Opt->SurfFileFormat, NULL)) {
            fprintf (SUMA_STDERR,
                     "Error %s: Failed to write surface object.\n", FuncName);
            exit (1);
         }
      }
   }
   
   if (Opt->debug) {
      float rad, vol, *p;
      int kkk;
      SUMA_LH("Rad Ratting");

      vol = SUMA_Mesh_Volume(SO, NULL, -1, 1, NULL);
      rad = pow(3.0/4.0/SUMA_PI*vol, 1.0/3.0);
      SUMA_MIN_MAX_SUM_VECMAT_COL(SO->NodeList, SO->N_Node, SO->NodeDim, 
                                  SO->MinDims, SO->MaxDims, SO->Center);
      SO->Center[0] /= (float)SO->N_Node; 
      SO->Center[1] /= (float)SO->N_Node; 
      SO->Center[2] /= (float)SO->N_Node; 
      /* what is the deviation from a sphere? */
      for (kkk=0; kkk<SO->N_Node; ++kkk) {
         p=&(SO->NodeList[3*kkk]);
         SUMA_SEG_LENGTH(SO->Center, p , Opt->shrink_bias[kkk]);  
         Opt->shrink_bias[kkk] /= rad; 
      }
      snprintf(stmp, 198, "%s_radrat.1D.dset", Opt->out_vol_prefix);
      SUMA_WRITE_ARRAY_1D(Opt->shrink_bias, SO->N_Node, 1, stmp);
   }
   
   if (Opt->UseSkull && SOhull) {
      fprintf (SUMA_STDERR,"%s: Writing skull surface  ...\n", FuncName);
      if (!SUMA_Save_Surface_Object (SO_name_hull, SOhull, 
                                     Opt->SurfFileType, Opt->SurfFileFormat, 
                                     NULL)) {
         fprintf (SUMA_STDERR,
                  "Error %s: Failed to write surface object.\n", FuncName);
         exit (1);
      }
   }
   
   /* prepare to write masked volume */
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
   
   /* what voxels are inside the surface ? */
   if (Opt->debug) 
      fprintf (SUMA_STDERR,
         "%s: Locating voxels inside surface  ...\n", FuncName);
   isin = SUMA_FindVoxelsInSurface (
                  SO, SO->VolPar, &N_in, 
                  Opt->fillhole, Opt->OrigSpatNormedSet);
   isin_float = (float *)SUMA_malloc(sizeof(float) *
                                    SO->VolPar->nx *
                                    SO->VolPar->ny *
                                    SO->VolPar->nz);
   if (!isin_float) {
      SUMA_SL_Crit("Failed to allocate");
      exit(1);
   }
   
   /* change Opt->fvec to reflect original data (not spat normed baby)*/
   {
      THD_3dim_dataset *dout=NULL;
      
      if (Opt->MaskMode == 0 || Opt->MaskMode == 1) { 
         /* use spatnormed baby, OK for mask too*/
         dout = Opt->OrigSpatNormedSet;
      } else if (Opt->MaskMode == 2) { /* use original dset */
         if (Opt->debug) 
            fprintf (SUMA_STDERR,
               "%s: Setting output to orig_vol (iset = %p) (osnd = %p)...\n", 
                  FuncName, Opt->iset, Opt->OrigSpatNormedSet);
         if (Opt->iset) {
            DSET_load(Opt->iset);
            dout = Opt->iset;
         } else { /* They may have used -no_spatnorm and -orig_vol options */
            DSET_load(Opt->OrigSpatNormedSet);
            dout = Opt->OrigSpatNormedSet;
         }
      } else {
         SUMA_S_Errv("Bad MaskMode value of %d!\n", Opt->MaskMode);
         SUMA_RETURN(NOPE);
      }
      
      if (Opt->fvec) { SUMA_free(Opt->fvec); Opt->fvec = NULL; }
      Opt->nvox = DSET_NVOX( dout );
      Opt->fvec = (float *)SUMA_malloc(sizeof(float) * Opt->nvox);
      if (!Opt->fvec) {
         SUMA_SL_Crit("Failed to allocate for fvec.\nOh misery.");
         SUMA_RETURN(NOPE);
      }

      if (Opt->debug) 
         fprintf (SUMA_STDERR,
            "%s: Coercing...\n", FuncName);
      EDIT_coerce_scale_type( Opt->nvox , DSET_BRICK_FACTOR(dout,0) ,
                              DSET_BRICK_TYPE(dout,0), 
                                 DSET_ARRAY(dout, 0) ,      /* input  */
                              MRI_float, Opt->fvec  ) ;   /* output */
   }
   if (Opt->MaskMode == 0 || Opt->MaskMode == 2) {
      SUMA_LH("Creating skull-stripped volume");
      for (i=0; i<SO->VolPar->nx*SO->VolPar->ny*SO->VolPar->nz; ++i) {
         /* apply the mask automatically */
         if (isin[i] >= SUMA_ON_NODE) isin_float[i] = (float)Opt->fvec[i];
         else isin_float[i] = 0.0;
      }
   } else if (Opt->MaskMode == 1) {
      SUMA_LH("Creating mask volume");
      for (i=0; i<SO->VolPar->nx*SO->VolPar->ny*SO->VolPar->nz; ++i) {
         isin_float[i] = (float)isin[i];
      }
   } else {
      SUMA_S_Errv("Bad MaskMode of %d!\n", Opt->MaskMode);
      SUMA_RETURN(NOPE);
   }
   if (isin) SUMA_free(isin); isin = NULL;
      
   if (Opt->debug) 
      fprintf (SUMA_STDERR,"%s: Writing masked volume  ...\n", FuncName);
   OptDs->full_list = 1;
   OptDs->dval = 1;
   dset = SUMA_FormAfnidset (NULL, isin_float,
                             SO->VolPar->nx*SO->VolPar->ny*SO->VolPar->nz,
                             OptDs);
   
   /* erode and dilate, a bit of a cleanup? */
   if (1){
      EDIT_options *edopt = SUMA_BlankAfniEditOptions();
      if (Opt->debug) 
         fprintf (SUMA_STDERR,
                  "%s: Applying a bit of erosion and dilatation \n", 
                  FuncName);
      edopt->edit_clust = ECFLAG_SAME;
      edopt->clust_rmm = SUMA_MAX_PAIR(SUMA_ABS((DSET_DX(OptDs->mset))),
                                       SUMA_ABS((DSET_DY(OptDs->mset))));
      edopt->clust_rmm = SUMA_MAX_PAIR(SUMA_ABS((DSET_DZ(OptDs->mset))), edopt->clust_rmm)*1.01;
	   edopt->clust_vmul = 1000*SUMA_ABS((DSET_DX(OptDs->mset)))*SUMA_ABS((DSET_DY(OptDs->mset)))*SUMA_ABS((DSET_DZ(OptDs->mset)));
      edopt->erode_pv  = 75.0 / 100.0;
      edopt->dilate = 1;
      EDIT_one_dataset( dset , edopt);
      SUMA_free(edopt);
   }
   
   
   if (!dset) {
      SUMA_SL_Err("Failed to create output dataset!");
   } else {
      tross_Make_History( FuncName , argc,argv , dset ) ;
      #if 1
      if (Opt->xyz_scale != 1.0f) 
         THD_volDXYZscale(dset->daxes, 1.0/Opt->xyz_scale, 0);
      #endif
      DSET_write(dset) ;
      if (Opt->MaskMode != 2) {
         if (Opt->MaskMode == 0) {
            fprintf(SUMA_STDERR,
                  "The intensity in the output dataset is a modified version\n"
                  "of the intensity in the input volume.\n" );
         } else {
            fprintf(SUMA_STDERR,
                  "The output dataset is a mask reflecting where voxels in the\n"
                  "input dataset lie in the brain.\n");
         }
         fprintf(SUMA_STDERR,
               "To obtain a masked version of the input with identical values inside\n"
               "the brain, you can either use 3dSkullStrip's -orig_vol option\n"
               "or run the following command:\n"
               "  3dcalc -a %s -b %s+orig -expr 'a*step(b)' \\\n"
               "         -prefix %s_orig_vol\n"
               "to generate a new masked version of the input.\n",
               Opt->in_name, Opt->out_vol_prefix, Opt->out_vol_prefix);
               
      }
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
   if (Opt) Opt = SUMA_Free_Generic_Prog_Options_Struct(Opt);
   if (hullprefix) SUMA_free(hullprefix); hullprefix = NULL;
   if (bhullprefix) SUMA_free(bhullprefix); bhullprefix = NULL;
   if (oskullprefix) SUMA_free(oskullprefix); oskullprefix = NULL;
   if (iskullprefix) SUMA_free(iskullprefix); iskullprefix = NULL;
   
   if (SO_name_hull) SUMA_free(SO_name_hull); SO_name_hull = NULL;
   if (SO_name_bhull) SUMA_free(SO_name_bhull); SO_name_bhull = NULL;
   if (SO_name_iskull) SUMA_free(SO_name_iskull); SO_name_iskull = NULL;
   if (SO_name_oskull) SUMA_free(SO_name_oskull); SO_name_oskull = NULL;
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
