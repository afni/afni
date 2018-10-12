#include "mrilib.h"
#include "thd_conformist.c"

/*---------------------------------------------------------------------------*/
/* Features to ruminate about:
    - NN interpolation of data, for matching label datasets      [not hard]

    - allow user-input weight volume                             [DONE]

    - plusminus mapping                                          [DONE]
      with Src(x-w(x)) = Bas(x+w(x)) instead of Src(x+w(x))=B(x)

    - initial 3dAllineate phase (-allineate or -resample)        [DONE]

    - symmetric mapping,                                         [very slow]
      with Src(W(x)) = Bas(INV(W(x))) instead of Src(W(x))=B(x)
      -- plusminus is nearly the same, and much simpler!

    - for patches with more than (say) 41^3=68921 'good' voxels, [not hard]
      extract a pseudorandom subset of them for the correlation
      calculation when penfac=0; also, keep penfac=0 to lev=2

    - vector-valued images                                       [medium]

    - GPU acceleration                                           [ugh squared]
*//*-------------------------------------------------------------------------*/

#ifdef USE_OMP       /* OpenMP = a must! */
# include <omp.h>
#endif

/** included here to be compiled with OpenMP, unlike the versions in libmri **/

#include "mri_genalign.c"
#include "mri_genalign_util.c"

/** include the warping functions, and enable the warp-optimizing functions **/

#define ALLOW_QWARP                /* this must be defined, or nothing works! */
#define ALLOW_QMODE                /* allows -Qfinal and -Qonly options */
#define ALLOW_PLUSMINUS            /* allows -plusminus option */
#undef  USE_PLUSMINUS_INITIALWARP  /* don't do this! doesn't work well! */
#define USE_NEW_HSAVE              /* 13 Mar 2018 */

#undef  ALLOW_DUPLO                /* 26 Jul 2018 */
#ifdef ALLOW_DUPLO
#  define DUPLO_STRING "-duplo"
#else
#  define DUPLO_STRING /*nada*/
#endif

/*----------------------------------------------------------------------*/
#include "mri_nwarp.c"             /* all the real work is done in here */

/** include the Powell NEWUOA functions for nonlinear optimization **/

#include "powell_int.c"
#define max MAX
#define min MIN
#include "powell_newuoa.c"
#undef max
#undef min
/*----------------------------------------------------------------------*/

/** constants for the mri_weightize() function (liberated from 3dAllineate) **/

static int auto_weight    = 1 ;      /* 1=weighted 2=binary 3=binary+box */
static float auto_wclip   = 0.0f ;   /* user-defined clip level */
static float auto_wpow    = 1.0f ;   /* raise weight to this power */
static int auto_dilation  = 5 ;      /* dilation of binarized mask */
static float wt_medsmooth = 2.25f ;  /* median radius for weight smooth */
static float wt_gausmooth = 4.50f ;  /* Gaussian radius for weight smooth */

static float wball_x = 0.0f ;  /* for -wball option [May 2016] */
static float wball_y = 0.0f ;
static float wball_z = 0.0f ;
static float wball_r = 0.0f ;  /* FWHM, actually */
static float wball_f = 0.0f ;

static THD_3dim_dataset *wmask_set = NULL ;  /* 21 Dec 2016 */
static float             wmask_f   = 0.0f ;

static int do_plusminus = 0 ;  /* doing plusminus warping? */

/*---------------------------------------------------------------------------*/
/*! Turn an input image into a weighting factor.
      If acod == 2, then make a binary mask at the end.
      If acod == 3, then make a boxed binary mask at the end.
    This function is taken from 3dAllineate.c.
-----------------------------------------------------------------------------*/

MRI_IMAGE * mri_weightize( MRI_IMAGE *im, int acod, int ndil, float aclip, float apow )
{
   float *wf,clip,clip2 ;
   int xfade,yfade,zfade , nx,ny,nz,nxy,nxyz , ii,jj,kk,ff ;
   byte *mmm ;
   MRI_IMAGE *qim , *wim ;

   if( Hverb ) INFO_message("Weightizing the base image") ;

   /*-- copy input image so we can mangle it --*/

   qim = mri_to_float(im) ; wf = MRI_FLOAT_PTR(qim) ;
   nx = qim->nx; ny = qim->ny; nz = qim->nz; nxy = nx*ny; nxyz = nxy*nz;
   for( ii=0 ; ii < nxyz ; ii++ ) wf[ii] = fabsf(wf[ii]) ;

   /*-- zero out along the edges so the edges of the volume get no weight --*/

#undef  WW
#define WW(i,j,k) wf[(i)+(j)*nx+(k)*nxy]

   xfade = (int)(0.05*qim->nx+3.0) ;                 /* number of points */
   yfade = (int)(0.05*qim->ny+3.0) ;                 /* along each face */
   zfade = (int)(0.05*qim->nz+3.0) ;                 /* to set to zero */
   if( 5*xfade >= qim->nx ) xfade = (qim->nx-1)/5 ;
   if( 5*yfade >= qim->ny ) yfade = (qim->ny-1)/5 ;
   if( 5*zfade >= qim->nz ) zfade = (qim->nz-1)/5 ;
   if( Hverb > 1 )
     ININFO_message("  xfade=%d yfade=%d zfade=%d",xfade,yfade,zfade);
   for( jj=0 ; jj < ny ; jj++ )
    for( ii=0 ; ii < nx ; ii++ )
     for( ff=0 ; ff < zfade ; ff++ ) WW(ii,jj,ff) = WW(ii,jj,nz-1-ff) = 0.0f;
   for( kk=0 ; kk < nz ; kk++ )
    for( jj=0 ; jj < ny ; jj++ )
     for( ff=0 ; ff < xfade ; ff++ ) WW(ff,jj,kk) = WW(nx-1-ff,jj,kk) = 0.0f;
   for( kk=0 ; kk < nz ; kk++ )
    for( ii=0 ; ii < nx ; ii++ )
     for( ff=0 ; ff < yfade ; ff++ ) WW(ii,ff,kk) = WW(ii,ny-1-ff,kk) = 0.0f;

   if( aclip > 0.0f ){  /* this is zero in 3dQwarp [31 Jul 2007] */
     int nleft , nclip ;
     for( nclip=nleft=ii=0 ; ii < nxyz ; ii++ ){
       if( wf[ii] > 0.0f ){
         if( wf[ii] < aclip ){ nclip++; wf[ii] = 0.0f; } else nleft++ ;
       }
     }
     if( Hverb > 1 )
       ININFO_message("  user clip=%g #clipped=%d #left=%d",
                      aclip,nclip,nleft) ;
   }

   /*-- squash super-large values down to reasonability --*/

   clip = 3.0f * THD_cliplevel(qim,0.5f) ;  /* defines 'super-large' */
   if( Hverb > 1 ) ININFO_message("  (unblurred) top clip=%g",clip) ;
   for( ii=0 ; ii < nxyz ; ii++ ) if( wf[ii] > clip ) wf[ii] = clip ;

   /*-- blur a little: median then Gaussian;
          the idea is that the median filter smashes localized spikes,
          then the Gaussian filter does a litte extra general smoothing. --*/

   mmm = (byte *)malloc( sizeof(byte)*nxyz ) ;         /* make a mask */
   for( ii=0 ; ii < nxyz ; ii++ ) mmm[ii] = (wf[ii] > 0.0f) ;
   if( wt_medsmooth > 0.0f ){
     wim = mri_medianfilter( qim , wt_medsmooth , mmm , 0 ) ;
     mri_free(qim) ;  /* no longer needed */
   } else {
     wim = qim ;      /* no median filter */
   }
   wf = MRI_FLOAT_PTR(wim) ;
   if( wt_gausmooth > 0.0f )                     /* Gaussian filter */
     FIR_blur_volume_3d( wim->nx , wim->ny , wim->nz ,
                         1.0f , 1.0f , 1.0f ,  wf ,
                         wt_gausmooth , wt_gausmooth , wt_gausmooth ) ;

   /*-- clip off small values, and
        keep only the largest cluster of supra threshold voxels --*/

   clip  = 0.05f * mri_max(wim) ;             /* define 'msall' */
   clip2 = 0.33f * THD_cliplevel(wim,0.33f) ; /* values here */
   clip  = MAX(clip,clip2) ;
   if( Hverb > 1 ) ININFO_message("  (blurred) bot clip=%g",clip) ;
   for( ii=0 ; ii < nxyz ; ii++ ) mmm[ii] = (wf[ii] >= clip) ;
   /* get biggest cluster, erode it, re-cluster that */
   THD_mask_clust( nx,ny,nz, mmm ) ;
   THD_mask_erode( nx,ny,nz, mmm, 1 ) ;  /* cf. thd_automask.c */
   THD_mask_clust( nx,ny,nz, mmm ) ;
   /* kill anyone not in the surviving cluster */
   for( ii=0 ; ii < nxyz ; ii++ ) if( !mmm[ii] ) wf[ii] = 0.0f ;
   free((void *)mmm) ;  /* free the mask */

   /*-- convert weight to 0..1 range [10 Sep 2007] --*/

   clip = 0.0f ;
   for( ii=0 ; ii < nxyz ; ii++ ) if( wf[ii] > clip ) clip = wf[ii] ;
   if( clip == 0.0f )
     ERROR_exit("Can't compute autoweight: max value seen as 0") ;
   clip = 1.0f / clip ;
   for( ii=0 ; ii < nxyz ; ii++ ) wf[ii] *= clip ;

   /*-- take a power? --*/

   if( apow > 0.0f && apow != 1.0f ){
     if( Hverb > 1 ) ININFO_message("  raising to %g power",apow) ;
     for( ii=0 ; ii < nxyz ; ii++ )
       if( wf[ii] > 0.0f ) wf[ii] = powf( wf[ii] , apow ) ;
   }

   /*-- binarize (acod==2)?  boxize (acod==3)? --*/

#undef  BPAD
#define BPAD 4
   if( acod == 2 || acod == 3 ){  /* binary weight: mask=2 or maskbox=3 */
     if( Hverb > 1 ) ININFO_message("  binarizing") ;
     for( ii=0 ; ii < nxyz ; ii++ ) if( wf[ii] != 0.0f ) wf[ii] = 1.0f ;
     if( ndil > 0 ){  /* 01 Mar 2007: dilation */
       byte *mmm = (byte *)malloc(sizeof(byte)*nxyz) ;
       if( Hverb > 1 ) ININFO_message("  dilating") ;
       for( ii=0 ; ii < nxyz ; ii++ ) mmm[ii] = (wf[ii] != 0.0f) ;
       for( ii=0 ; ii < ndil ; ii++ ){
         THD_mask_dilate     ( nx,ny,nz , mmm , 3 ) ;
         THD_mask_fillin_once( nx,ny,nz , mmm , 2 ) ;
       }
       for( ii=0 ; ii < nxyz ; ii++ ) wf[ii] = (float)mmm[ii] ;
       free(mmm) ;
     }
     if( acod == 3 ){  /* boxize */
       int xm,xp , ym,yp , zm,zp ;
       MRI_autobbox_clust(0) ;
       MRI_autobbox( wim , &xm,&xp , &ym,&yp , &zm,&zp ) ;
       xm -= BPAD ; if( xm < 1    ) xm = 1 ;
       ym -= BPAD ; if( ym < 1    ) ym = 1 ;
       zm -= BPAD ; if( zm < 1    ) zm = 1 ;
       xp += BPAD ; if( xp > nx-2 ) xp = nx-2 ;
       yp += BPAD ; if( yp > ny-2 ) yp = ny-2 ;
       zp += BPAD ; if( zp > nz-2 ) zp = nz-2 ;
       if( Hverb > 1 )
         ININFO_message("  box=%d..%d X %d..%d X %d..%d = %d voxels",
                        xm,xp , ym,yp , zm,zp , (xp-xm+1)*(yp-ym+1)*(zp-zm+1) ) ;
       for( kk=zm ; kk <= zp ; kk++ )
        for( jj=ym ; jj <= yp ; jj++ )
         for( ii=xm ; ii <= xp ; ii++ ) WW(ii,jj,kk) = 1.0f ;
     }
   }

   return wim ;
}

/*---------------------------------------------------------------------------*/

static THD_3dim_dataset *qset = NULL ;

/*---------------------------------------------------------------------------*/
/* Help me if you can, I'm feeling down down down */
/*---------------------------------------------------------------------------*/

void Qhelp(void)
{
  printf("\n") ;
  printf("Usage: 3dQwarp [OPTIONS]\n") ;
  printf(
    "\n"
    "* Computes a nonlinearly warped version of source_dataset to match base_dataset.\n"
    " ++ The detail allowed in the warping is set by the '-minpatch' option.\n"
    " ++ The discrete warp computed herein is a representation of an underlying\n"
    "    piecewise polynomial C1 diffeomorphism.\n"
    " ++ See the OUTLINE OF WARP OPTIMIZATION METHOD section, far below, for details.\n"
    "\n"
    "* The simplest way to use this program is via the @SSwarper script, for\n"
    "  warping a T1-weighted dataset to the (human brain) MNI 2009 template\n"
    "  dataset supplied with AFNI binaries.\n"
    "* You can use 3dQwarp directly if you want to control (or play with) the\n"
    "  various options for setting up the warping process.\n"
    "\n"
    "* Input datasets must be on the same 3D grid (unlike program 3dAllineate)!\n"
    " ++ Or you will get a fatal error when the program checks the datasets!\n"
    " ++ However, you can use the '-allineate' option in 3dQwarp to do\n"
    "    affine alignment before the nonlinear alignment, which will also\n"
    "    resample the aligned source image to the base dataset grid.\n"
    " ++ OR, you can use the '-resample' option in 3dQwarp to resample the\n"
    "    source dataset to the base grid before doing the nonlinear stuff,\n"
    "    without doing any preliminary affine alignment.\n"
    "\n"
    "* 3dQwarp CAN be used on 2D images -- that is, datasets with a single\n"
    "  slice. How well it works on such datasets has not been investigated\n"
    "  much, but it DOES work (and quickly, since the amount of data is small).\n"
    " ++ You CAN input .jpg or .png files as the source and base images.\n"
    " ++ 3dQwarp will convert RGB images to grayscale and attempt to align those.\n"
    "    The output will still be in dataset format (not image format) and\n"
    "    will be in grayscale floating point (not color). To get the warped\n"
    "    image output in .jpg or .png format, you can open the output dataset\n"
    "    in the AFNI GUI and save the image -- after turning off crosshairs!\n"
    "  + To get an RGB copy of a warped image, you have to apply the warp to\n"
    "    each channel (R, G, B) separately and then fuse the results.\n"
    " ++ Applying this program to 2D images is mostly for fun; the actual\n"
    "    utility of it in brain imaging is not clear to Emperor Zhark.\n"
    "\n"
    "* Input datasets should be reasonably well aligned already\n"
    "  (e.g., as from an affine warping via 3dAllineate).\n"
    " ++ The standard result from 3dAllineate will resample the affinely\n"
    "    aligned dataset to the same 3D grid as the -base dataset, so this\n"
    "    new dataset will be ready to run in 3dQwarp against the same base.\n"
    " ++ Again, the '-allineate' option can now do this for you, inside 3dQwarp.\n"
    "\n"
    "* Input datasets should be 'alike'.\n"
    " ++ For example, if the '-base' dataset is skull stripped, then the '-source'\n"
    "    dataset should be skull stripped also -- e.g., via 3dSkullStrip.\n"
    " ++ If the datasets have markedly different contrasts (e.g., T1 and T2), then\n"
    "    using a non-standard matching function such as '-nmi' or '-hel' or '-lpa'\n"
    "    might work better than the default Pearson correlation matching function.\n"
    "\n"
    "******************************************************************************\n"
    "* If the input datasets do NOT overlap reasonably well (please look at them  *\n"
    "* them in AFNI), or when the source is in scanner space and the base is in a *\n"
    "* template space (e.g., MNI), then you need to use '-allineate', or you will *\n"
    "* probably get                                                               *\n"
    "*  (a) a very bad result (or a program crash)                                *\n"
    "*  (b) that takes a long time and a lot of memory to compute.                *\n"
    "* 'Overlap well' means that the datasets match well in coordinate space.     *\n"
    "* In some cases, datasets may match well voxel-wise, but the xyz coordinates *\n"
    "* defined in the dataset headers do not match -- in such a case, 3dQwarp     *\n"
    "* will fail. This is why Zhark urges you to LOOK at the overlap in AFNI,     *\n"
    "* which uses coordinates for display matching, not voxel indexes. Or use     *\n"
    "* the '-allineate' option to get 3dAllineate to line up the dataset by       *\n"
    "* brute force, just to be safe (at the cost of a little extra CPU time).     *\n"
    "******************************************************************************\n"
    "\n"
    "* Outputs of 3dQwarp are the warped dataset and the warp that did it.\n"
    " ++ These datasets are stored in float format, no matter what the\n"
    "    data type of the source dataset.\n"
    " ++ Various other optional outputs are described later.\n"
    "\n"
    "* Simple example:\n"
    "    3dQwarp -allineate -blur 0 5              \\\n"
    "            -base ~/abin/MNI152_1mm+tlrc.HEAD \\\n"
    "            -source sub637_T1.nii             \\\n"
    "            -prefix sub637_T1qw.nii\n"
    "  which will produce a dataset warped to match the MNI152 T1 template\n"
    "  at a 1 mm resolution. Since the MNI152 template is already pretty\n"
    "  blurry, the amount of blurring applied to it is set to zero, while\n"
    "  the source dataset (presumably not blurry) will be Gaussian blurred\n"
    "  with a FWHM of 5 mm.\n"
    "\n"
    "* Matching uses the 'clipped Pearson' method by default, and\n"
    "  can be changed to 'pure Pearson' with the '-pear' option.\n"
    " ++ The purpose of 'clipping' is to reduce the impact of outlier values\n"
    "    (small or large) on the correlation.\n"
    " ++ For the adventurous, you can also try these matching functions:\n"
    "      '-hel' for Hellinger distance\n"
    "      '-mi'  for Mutual Information\n"
    "      '-nmi' for Normalized Mutual Information\n"
    "    These options have NOT been extensively tested for usefulness,\n"
    "    and should be considered experimental at this infundibulum.\n"
    " ++ The 'local' correlation options are also now available:\n"
    "      '-lpc' for Local Pearson minimization (i.e., EPI-T1 registration)\n"
    "      '-lpa' for Local Pearson maximization\n"
    "    These options also have not been extensively tested.\n"
    " ** If you use '-lpc', then '-maxlev 0' is automatically set. If you want\n"
    "    to go to more refined levels, you can set '-maxlev' AFTER '-lpc' on the\n"
    "    command line. Using maxlev > 1 is not recommended for EPI-T1 alignment.\n"
    " ** For aligning EPI to T1, the '-lpc' option can be used; my advice\n"
    "    would be to do something like the following:\n"
    "      3dSkullStrip -input SUBJ_anat+orig -prefix SUBJ_anatSS\n"
    "      3dbucket -prefix SUBJ_epiz SUBJ_epi+orig'[0]'\n"
    "      align_epi_anat.py -anat SUBJ_anat+orig                            \\\n"
    "                        -epi SUBJ_epiz+orig -epi_base 0 -partial_axial  \\\n"
    "                        -epi2anat -master_epi SUBJ_anat+orig            \\\n"
    "                        -big_move\n"
    "      3dQwarp -source SUBJ_anatSS+orig.HEAD   \\\n"
    "              -base   SUBJ_epiz_al+orig       \\\n"
    "              -prefix SUBJ_anatSSQ            \\\n"
    "              -lpc -verb -iwarp -blur 0 3\n"
    "      3dNwarpApply -nwarp  SUBJ_anatSSQ_WARPINV+orig  \\\n"
    "                   -source SUBJ_epiz_al+orig          \\\n"
    "                   -prefix SUBJ_epiz_alQ\n"
    "    * Zeroth, the T1 is prepared by skull stripping and the EPI is prepared\n"
    "      by extracting just the 0th sub-brick for registration purposes.\n"
    "    * First, the EPI is aligned to the T1 using the affine 3dAllineate, and\n"
    "      at the same time resampled to the T1 grid (via align_epi_anat.py).\n"
    "    * Second, it is nonlinearly aligned ONLY using the global warping -- it is\n"
    "      futile to try to align such dissimilar image types precisely.\n"
    "    * The EPI is used as the base in 3dQwarp so that it provides the weighting,\n"
    "      and so partial brain coverage (as long as it covers MOST of the brain)\n"
    "      should not cause a problem (we hope).\n"
    "    * Third, 3dNwarpApply is used to take the inverse warp from 3dQwarp to\n"
    "      transform the EPI to the T1 space, since 3dQwarp transformed the T1 to\n"
    "      EPI space. This inverse warp was output by 3dQwarp using '-iwarp'.\n"
    "    * Someday, this procedure may be incorporated into align_epi_anat.py :-)\n"
    " ** It is vitally important to visually look at the results of this process! **\n"
#if defined(USE_OMP) && defined(__GNU_C__)
    " ++ Note that these 'adventurous' matching options may cause trouble with\n"
    "    OpenMP compiled with GNU gcc, due to a bug in gcc's OpenMP library\n"
    "    -- and this binary is compiled that way!\n"
    "    -- this issue is one reason these options are labeled 'adventurous'.\n"
    "    -- It sometimes happens that gcc's OpenMP will 'freeze' during a long\n"
    "       run, apparently due to a 'race condition' bug in the GNU OpenMP\n"
    "       library. If this happens to you, one workaround is to\n"
    "       run the program to a larger minimum patch size, then\n"
    "       re-start the warping process from the last saved warp,\n"
    "       using '-inilev' and '-iniwarp'.\n"
#ifdef USE_NEW_HSAVE
    "       Using the '-allsave' option would also let you re-start the warping\n"
    "       from the end of the last completed level, since that option saves\n"
    "       the computed warp as each level finishes before starting the next.\n"
#endif
    "    -- Also, the use of '-verb' will cause 3dQwarp to print a\n"
    "       report for every patch, which will usually make it obvious\n"
    "       when the program freezes up.\n"
    "    -- Unfortunately, Emperor Zhark doesn't know how to get around\n"
    "       this problem with gcc (except to use the Intel icc compiler).\n"
#endif
    "\n"
    "* For aligning T1-weighted anatomical volumes, Zhark recommends that\n"
    "  you use the 3dUnifize program to (approximately) spatially uniformize\n"
    "  and normalize their intensities -- this helps in the matching process,\n"
    "  especially when using datasets from different scanners.\n"
    " ++ Skull stripping a la 3dSkullStrip is also a good idea (prior to 3dUnifize),\n"
    "    even if you are registering datasets from the same subject; see the\n"
    "    SAMPLE USAGE section below for an example.\n"
    " ++ If you ultimately want a non-3dUnifize-d transformed dataset, you can use\n"
    "    the output WARP dataset and 3dNwarpApply to transform the un-3dUnifize-d\n"
    "    source dataset; again, see the SAMPLE USAGE section below.\n"
    " ++ Some people prefer to nonlinearly align datasets with the 'skull' left on.\n"
    "    You are free to try this, of course, but we have not tested this method.\n"
    "    We give you tools; you build things with them.\n"
    "\n"
    "* If for some deranged reason you have datasets with very non-cubical voxels,\n"
    "  they should be resampled to a cubical grid before trying 3dQwarp. For example,\n"
    "  if you have acquired 1x1x4 mm T1-weighted structural volumes (why?), then\n"
    "  resample them to 1x1x1 mm before doing any other registration processing.\n"
    "  For example:\n"
    "    3dAllineate -input anatT1_crude+orig -newgrid 1.0 \\\n"
    "                -prefix anatT1_fine -final wsinc5     \\\n"
    "                -1Dparam_apply '1D: 12@0'\\'\n"
    "  This operation can also now be done using the '-allineate' or '-resample'\n"
    "  options to 3dQwarp.\n"
    "\n"
    "** Please note that this program is very CPU intensive, and is what computer\n"
    "   scientists call a 'pig' (i.e., run time from 10s of minutes to hours).\n"
#ifndef USE_OMP
    " ++ It would be best to run 3dQwarp on a multi-CPU computer, using a binary\n"
    "    compiled with the OpenMP library. Unfortunately, this particular version is\n"
    "    NOT built with OpenMP, and you will probably find it to be unbearably slow :-(\n"
#endif
    "\n"
    "------------\n"
    "SAMPLE USAGE\n"
    "------------\n"
    "* For registering a T1-weighted anat to a mildly blurry template at about\n"
    "  a 1x1x1 mm resolution (note that the 3dAllineate step, to give the\n"
    "  preliminary alignment, will also produce a dataset on the same 3D grid\n"
    "  as the TEMPLATE+tlrc dataset, which 3dQwarp requires):\n"
    "\n"
    "    3dUnifize -prefix anatT1_U -input anatT1+orig\n"
    "    3dSkullStrip -input anatT1_U+orig -prefix anatT1_US -niter 400 -ld 40\n"
    "    3dAllineate -prefix anatT1_USA -base TEMPLATE+tlrc    \\\n"
    "                -source anatT1_US+orig -twopass -cost lpa \\\n"
    "                -1Dmatrix_save anatT1_USA.aff12.1D        \\\n"
    "                -autoweight -fineblur 3 -cmass\n"
    "    3dQwarp -prefix anatT1_USAQ -blur 0 3 \\\n"
    "            -base TEMPLATE+tlrc -source anatT1_USA+tlrc\n"
    "\n"
    "  You can then use the anatT1_USAQ_WARP+tlrc dataset to transform other\n"
    "  datasets (that were aligned with the input anatT1+orig) in the same way\n"
    "  using program 3dNwarpApply, as in\n"
    "\n"
    "    3dNwarpApply -nwarp 'anatT1_USAQ_WARPtlrc anatT1_USA.aff12.1D' \\\n"
    "                 -source NEWSOURCE+orig -prefix NEWSOURCE_warped\n"
    "\n"
    "  For example, if you want a warped copy of the original anatT1+orig dataset\n"
    "  (without the 3dUnifize and 3dSkullStrip modifications), put 'anatT1' in\n"
    "  place of 'NEWSOURCE' in the above command.\n"
    "\n"
    "  Note that the '-nwarp' option to 3dNwarpApply has TWO filenames inside\n"
    "  single quotes. This feature tells that program to compose (catenate) those\n"
    "  2 spatial transformations before applying the resulting warp. See the -help\n"
    "  output of 3dNwarpApply for more sneaky/cunning ways to make the program warp\n"
    "  datasets (and also see the example just below).\n"
    "\n"
    "   ** PLEASE NOTE that if you use the '-allineate' option in 3dQwarp, to   **\n"
    "   ** do the 3dAllineate step inside 3dQwarp, then you do NOT catenate     **\n"
    "   ** the affine and nonlinear warps as in the 3dNwarpApply example above, **\n"
    "   ** since the output nonlinear warp will ALREADY have be catenated with  **\n"
    "   ** the affine warp -- this output warp is the transformation directly   **\n"
    "   ** between the '-source' and '-base' datasets (as is reasonable IZHO).  **\n"
    "\n"
    "  If the NEWSOURCE+orig dataset is integer-valued (e.g., anatomical labels),\n"
    "  then you would use the '-ainterp NN' with 3dNwarpApply, to keep the program\n"
    "  from interpolating the voxel values.\n"
    "\n"
    "* If you use align_epi_anat.py to affinely transform several EPI datasets to\n"
    "  match a T1 anat, and then want to nonlinearly warp the EPIs to the template,\n"
    "  following the warp generated above, the procedure is something like this:\n"
    "\n"
    "    align_epi_anat.py -anat anatT1+orig -epi epi_r1+orig \\\n"
    "                      -epi_base 3 -epi2anat -big_move    \\\n"
    "                      -child_epi epi_r2+orig epi_r3+orig\n"
    "\n"
    "    3dNwarpApply -source epi_r1+orig                                \\\n"
    "                 -nwarp 'anatT1_USAQ_WARP+tlrc anatT1_USA.aff12.1D' \\\n"
    "                 -affter epi_r1_al_reg_mat.aff12.1D                 \\\n"
    "                 -master WARP -newgrid 2.0                          \\\n"
    "                 -prefix epi_r1_AQ\n"
    "\n"
    "    (mutatis mutandis for 'child' datasets epi_r2, epi_r3, etc.).\n"
    "\n"
    "  The above procedure transforms the data directly from the un-registered\n"
    "  original epi_r1+orig dataset, catenating the EPI volume registration\n"
    "  transformations (epi_r1_al_reg_mat.aff12.1D) with the affine anat to\n"
    "  template transformation (anatT1_USA.aff12.1D) and with the nonlinear\n"
    "  anat to template transformation (anatT1_USAQ_WARP+tlrc).  3dNwarpApply\n"
    "  will use the default 'wsinc5' interpolation method, which does not blur\n"
    "  the results much -- an important issue for statistical analysis of the\n"
    "  EPI time series.\n"
    "\n"
    "  Various functions, such as volume change fraction (Jacobian determinant)\n"
    "  can be calculated from the warp dataset via program 3dNwarpFuncs.\n"
    "\n"
    "--------------------\n"
    "COMMAND LINE OPTIONS (too many of them)\n"
    "--------------------\n"
    "\n"
    "++++++++++ Input and Outputs +++++++++++++\n"
    "\n"
    " -base   base_dataset   = Alternative way to specify the base dataset.\n"
    "\n"
    " -source source_dataset = Alternative way to specify the source dataset.\n"
    "                         * You can either use both '-base' and '-source',\n"
    "                           OR you can put the base and source dataset\n"
    "                           names last on the command line.\n"
    "                         * But you cannot use just one of '-base' or '-source'\n"
    "                           and then put the other input dataset name at the\n"
    "                           end of the command line!\n"
    "                       *** Please note that if you are using 3dUnifize on one\n"
    "                           dataset (or the template was made with 3dUnifize-d\n"
    "                           datasets), then the other dataset should also be\n"
    "                           processed the same way for better results. This\n"
    "                           dictum applies in general: the source and base\n"
    "                           datasets should be pre-processed the same way,\n"
    "                           as far as practicable.\n"
    "\n"
    " -prefix ppp  = Sets the prefix for the output datasets.\n"
    "               * The source dataset is warped to match the base\n"
    "                 and gets prefix 'ppp'. (Except if '-plusminus' is used.)\n"
    "               * The final interpolation to this output dataset is\n"
    "                 done using the 'wsinc5' method. See the output of\n"
    "                   3dAllineate -HELP\n"
    "                 (in the \"Modifying '-final wsinc5'\" section) for\n"
    "                 the lengthy technical details.\n"
    "               * The 3D warp used is saved in a dataset with\n"
    "                 prefix '{prefix}_WARP' -- this dataset can be used\n"
    "                 with 3dNwarpApply and 3dNwarpCat, for example.\n"
    "                 * To be clear, this is the warp from source dataset\n"
    "                   coordinates to base dataset coordinates, where the\n"
    "                   values at each base grid point are the xyz displacments\n"
    "                   needed to move that grid point's xyz values to the\n"
    "                   corresponding xyz values in the source dataset:\n"
    "                     base( (x,y,z) + WARP(x,y,z) ) matches source(x,y,z)\n"
    "                   Another way to think of this warp is that it 'pulls'\n"
    "                   values back from source space to base space.\n"
    "               * 3dNwarpApply would use '{prefix}_WARP' to transform datasets\n"
    "                 aligned with the source dataset to be aligned with the\n"
    "                 base dataset.\n"
    "              ** If you do NOT want this warp saved, use the option '-nowarp'.\n"
    "            -->> (But: This warp is usually the most valuable possible output!)\n"
    "               * If you want to calculate and save the inverse 3D warp,\n"
    "                 use the option '-iwarp'. This inverse warp will then be\n"
    "                 saved in a dataset with prefix '{prefix}_WARPINV'.\n"
    "               * This inverse warp could be used to transform data from base\n"
    "                 space to source space, if you need to do such an operation.\n"
    "               * You can easily compute the inverse later, say by a command like\n"
    "                  3dNwarpCat -prefix Z_WARPINV 'INV(Z_WARP+tlrc)'\n"
    "                 or the inverse can be computed as needed in 3dNwarpApply, like\n"
    "                  3dNwarpApply -nwarp 'INV(Z_WARP+tlrc)' -source Dataset.nii ...\n"
    "\n"
    " -nowarp      = Do not save the _WARP file.\n"
    "               * By default, the {prefix}_WARP dataset will be saved.\n"
    "\n"
    " -iwarp       = Do compute and save the _WARPINV file.\n"
    "               * By default, the {prefix}_WARPINV file is NOT saved.\n"
    "\n"
    " -nodset      = Do not save the warped source dataset (i.e., if you only\n"
    "                need the _WARP).\n"
    "               * By default, the warped source dataset {prefix} is saved.\n"
    "\n"
    "\n"
    "++++++++++ Preliminary affine (linear transformation) alignment ++++++++++\n"
    "\n"
    " -allineate   = This option will make 3dQwarp run 3dAllineate first, to align\n"
    "   *OR*         the source dataset to the base with an affine transformation.\n"
    " -allin         It will then use that alignment as a starting point for the\n"
    "   *OR*         nonlinear warping.\n"
    " -allinfast    * With '-allineate', the source dataset does NOT have to be on\n"
    "                 the same 3D grid as the base, since the intermediate output\n"
    "                 of 3dAllineate (the substitute source) will be on the grid\n"
    "                 as the base.\n"
    "               * If the datasets overlap reasonably already, you can use the\n"
    "                 option '-allinfast' (instead of '-allineate') to add the\n"
    "                 option '-onepass' to the 3dAllineate command line, to make\n"
    "                 it run faster (by avoiding the time-consuming coarse pass\n"
    "                 step of trying lots of shifts and rotations to find an idea\n"
    "                 of how to start). But you should KNOW that the datasets do\n"
    "                 overlap well before using '-allinfast'.\n"
    "          -->>** The final output warp dataset is the warp directly between\n"
    "                 the original source dataset and the base (i.e., the catenation\n"
    "                 of the affine matrix from 3dAllineate and the nonlinear warp\n"
    "                 from the 'warpomatic' procedure in 3dQwarp).\n"
    "          -->>** The above point means that you should NOT NOT NOT use the\n"
    "                 affine warp output by the '-allineate' option in combination\n"
    "                 with the nonlinear warp output by 3dQwarp (say, when using\n"
    "                 3dNwarpApply), since the affine warp would then be applied\n"
    "                 twice -- which would be WRONG WRONG WRONG.\n"
    "          -->>** The final output warped dataset is warped directly from the\n"
    "                 original source dataset, NOT from the substitute source.\n"
    "               * The intermediate files from 3dAllineate (the substitute source\n"
    "                 dataset and the affine matrix) are saved, using 'prefix_Allin'\n"
    "                 in the filenames. If you wish to have them deleted, use the\n"
    "                 option '-allinkill' in addition to '-allineate'.\n"
    "             *** The following 3dQwarp options CANNOT be used with -allineate:\n"
    "                   -plusminus  -inilev  -iniwarp\n"
#ifdef ALLOW_DUPLO
    "             *** However, you CAN use -duplo with -allineate.\n"
#endif
    "               * The '-awarp' option will output the computed warp from the\n"
    "                 intermediea 3dAllineate-d dataset to the base dataset,\n"
    "                 in case you want that for some reason. This option will\n"
    "                 only have meaning if '-allineate' or '-allinfast' is used.\n"
    "                 The prefix of the '-awarp' output will have the string\n"
    "                 '_AWARP' appended to the {prefix} for the output dataset.\n"
    "\n"
    " -allineate_opts '-opt ...'\n"
    "   *OR*        * This option lets you add extra options to the 3dAllineate\n"
    " -allopt         command to be run by 3dQwarp. Normally, you won't need\n"
    "                 to do this.\n"
    "               * All the extra options for the 3dAllineate command line\n"
    "                 should be enclosed inside a pair of quote marks; e.g.,\n"
    "                    -allopt '-cost lpa -verb'\n"
    "               * If '-emask' is used in 3dQwarp, the same option will be\n"
    "                 passed to 3dAllineate automatically, so you don't have to\n"
    "                 do that yourself.\n"
    "             *** Do NOT attempt to use the (obsolescent) '-nwarp' option in\n"
    "                 3dAllineate from inside 3dQwarp -- bad things will probably\n"
    "                 happen, and you won't EVER get any Christmas presents again!\n"
    "\n"
    " -resample    = This option simply resamples the source dataset to match the\n"
    "                base dataset grid. You can use this if the two datasets\n"
    "                overlap well (as seen in the AFNI GUI), but are not on the\n"
    "                same 3D grid.\n"
    "               * If they don't overlap very well, use '-allineate' instead.\n"
    "               * As with -allineate, the final output dataset is warped\n"
    "                 directly from the source dataset, not from the resampled\n"
    "                 source dataset.\n"
    "               * The reampling here (and with -allineate) is done with the\n"
    "                 'wsinc5' method, which has very little blurring artifact.\n"
    "               * If the base and source datasets ARE on the same 3D grid,\n"
    "                 then the -resample option will be ignored.\n"
    "               * You CAN use -resample with these 3dQwarp options:\n"
    "                   -plusminus  -inilev  -iniwarp  " DUPLO_STRING "\n"
    "                 In particular, '-iniwarp' and '-resample' will work\n"
    "                 together if you need to re-start a warp job from the\n"
    "                 output of '-allsave'.\n"
    "               * Unless you are in a hurry, '-allineate' is better.\n"
    "\n"
    " -awarp       = If '-allineate' is used, output the nonlinear warp that\n"
    "                transforms from the 3dAllineate-d affine alignment of\n"
    "                source-to-base to the base. This warp (output {prefix}_AWARP)\n"
    "                combined with the affine transformation {prefix}.aff12.1D is\n"
    "                the same as the final {prefix}_WARP nonlinear transformation\n"
    "                directly from source-to-base.\n"
    "               * The '-awarp' output is mostly useful when you need to have\n"
    "                 this incremental nonlinear warp for various purposes; for\n"
    "                 example, it is used in the @SSwarper script.\n"
    "               * '-awarp' will not do anything unless '-allineate' is also\n"
    "                 used, because it doesn't have anything to do!\n"
    "               * By default, this {prefix}_AWARP file is NOT saved.\n"
    "\n"
    "++++++++++ Computing the 'cost' functional = how datasets are matched ++++++++++\n"
    "\n"
    " -pear        = Use strict Pearson correlation for matching.\n"
    "               * Not usually recommended, since the 'clipped Pearson' method\n"
    "                 used by default will reduce the impact of outlier values.\n"
    "               * No partridges or trees are implied by this option.\n"
    "\n"
    " -hel         = Hellinger metric\n"
    " -mi          = Mutual information\n"
    " -nmi         = Normalized mutual information\n"
    " -lpc         = Local Pearson correlation (signed)\n"
    " -lpa         = Local Pearson correlation (absolute value)\n"
    "               * These options mirror those in 3dAllineate, and have not\n"
    "                 been tested much. They may not be very useful.\n"
    "               * In particular, nonlinear warping of low resolution EPI\n"
    "                 data to T1 data is a difficult task, and can introduce\n"
    "                 more distortions to the result than it fixes.\n"
    "\n"
    " -noneg       = Replace negative values in either input volume with 0.\n"
    "               * If there ARE negative input values, and you do NOT use -noneg,\n"
    "                 then strict Pearson correlation will be used, since the\n"
    "                 'clipped' method only is implemented for non-negative volumes.\n"
    "               * '-noneg' is not the default, since there might be situations\n"
    "                 where you want to align datasets with positive and negative\n"
    "                 values mixed.\n"
    "               * But, in many cases, the negative values in a dataset are just\n"
    "                 the result of interpolation artifacts (or other peculiarities),\n"
    "                 and so they should be ignored. That is what '-noneg' is for.\n"
    "               * Therefore, '-noneg' is recommended for most applications.\n"
    "\n"
    " -nopenalty   = Don't use a penalty on the cost functional; the goal\n"
    "                of the penalty is to reduce grid distortions.\n"
    "               * If there penalty is turned off AND you warp down to\n"
    "                 a fine scale (e.g., '-minpatch 11'), you will probably\n"
    "                 get strange-looking results.\n"
    "\n"
    " -penfac ff   = Use the number 'ff' to weight the penalty.\n"
    "                The default value is 1. Larger values of 'ff' mean the\n"
    "                penalty counts more, reducing grid distortions,\n"
    "                insha'Allah; '-nopenalty' is the same as '-penfac 0'.\n"
#if 0
    "           -->>* [23 Sep 2013] -- Zhark increased the default value of\n"
    "                 the penalty by a factor of 5, and also made it get\n"
    "                 progressively larger with each level of refinement.\n"
    "                 Thus, warping results will vary from earlier instances\n"
    "                 of 3dQwarp.\n"
    "               * The progressive increase in the penalty at higher levels\n"
    "                 means that the 'cost functional' can actually look like the\n"
    "                 alignment is getting worse when the levels change.\n"
    "               * IF you wish to turn off this progression, for whatever\n"
    "                 reason (e.g., to keep compatibility with older results),\n"
    "                 use the option '-penold'. To be completely compatible with\n"
    "                 the older 3dQwarp, you'll also have to use '-penfac 0.2'.\n"
#endif
    "\n"
    " -useweight   = With '-useweight', each voxel in the base automask is weighted\n"
    "                by the intensity of the (blurred) base image. This makes\n"
    "                white matter count more in T1-weighted volumes, for example.\n"
    "           -->>* [24 Mar 2014] This option is is now the default.\n"
    "\n"
    " -noweight    = If you want a binary weight (the old default), use this option.\n"
    "                That is, each voxel in the base volume automask will be\n"
    "                weighted the same in the computation of the cost functional.\n"
    "\n"
    " -weight www  = Instead of computing the weight from the base dataset,\n"
    "                directly input the weight volume from dataset 'www'.\n"
    "               * Useful if you know what over parts of the base image you\n"
    "                 want to emphasize or de-emphasize the matching functional.\n"
    "\n"
    " -wball x y z r f =\n"
    "                Enhance automatic weight from '-useweight' by a factor\n"
    "                of 1+f*Gaussian(FWHM=r) centered in the base image at\n"
    "                DICOM coordinates (x,y,z) and with radius 'r'. The\n"
    "                goal of this option is to try and make the alignment\n"
    "                better in a specific part of the brain.\n"
    "               * Example:  -wball 0 14 6 30 40\n"
    "                 to emphasize the thalamic area (in MNI/Talairach space).\n"
    "               * The 'r' parameter must be positive!\n"
    "               * The 'f' parameter must be between 1 and 100 (inclusive).\n"
    "               * '-wball' does nothing if you input your own weight\n"
    "                 with the '-weight' option.\n"
    "               * '-wball' does change the binary weight created by\n"
    "                 the '-noweight' option.\n"
    "               * You can only use '-wball' once in a run of 3dQwarp.\n"
    "             *** The effect of '-wball' is not dramatic. The example\n"
    "                 above makes the average brain image across a collection\n"
    "                 of subjects a little sharper in the thalamic area, which\n"
    "                 might have some small value. If you care enough about\n"
    "                 alignment to use '-wball', then you should examine the\n"
    "                 results from 3dQwarp for each subject, to see if the\n"
    "                 alignments are good enough for your purposes.\n"
    "\n"
    " -wmask ws f  = Similar to '-wball', but here, you provide a dataset 'ws'\n"
    "                that indicates where to increase the weight.\n"
    "               * The 'ws' dataset must be on the same 3D grid as the base\n"
    "                  dataset.\n"
    "               * 'ws' is treated as a mask -- it only matters where it\n"
    "                 is nonzero -- otherwise, the values inside are not used.\n"
    "               * After 'ws' comes the factor 'f' by which to increase the\n"
    "                 automatically computed weight. Where 'ws' is nonzero,\n"
    "                 the weighting will be multiplied by (1+f).\n"
    "               * As with '-wball', the factor 'f' should be between 1 and 100.\n"
    "               * You cannot use '-wball' and '-wmask' together!\n"
    "\n"
    " -wtprefix p  = Saves auto-computed weight volume to a dataset with prefix 'p'.\n"
    "                If you are sufficiently dedicated, you could manually edit\n"
    "                this volume, in the AFNI GUI, in 3dcalc, et cetera. And then\n"
    "                use it, instead of the auto-computed default weight, via the\n"
    "                '-weight' option.\n"
    "               * If you use the '-emask' option, the effects of the exclusion\n"
    "                 mask are NOT shown in this output dataset!\n"
    "\n"
    " -emask ee    = Here, 'ee' is a dataset to specify a mask of voxels\n"
    "                to EXCLUDE from the analysis -- all voxels in 'ee'\n"
    "                that are NONZERO will not be used in the alignment.\n"
    "               * The base image always automasked -- the emask is\n"
    "                 extra, to indicate voxels you definitely DON'T want\n"
    "                 included in the matching process, even if they are\n"
    "                 inside the brain.\n"
    "           -->>* Note that 3dAllineate has the same option. Since you\n"
    "                 usually have to use 3dAllineate before 3dQwarp, you\n"
    "                 will probably want to use -emask in both programs.\n"
    "                [ Unless, of course, you are using '-allineate',  which  ]\n"
    "                [ will automatically include '-emask' in the 3dAllineate ]\n"
    "                [ phase if '-emask' is used here in 3dQwarp.             ]\n"
    "               * Applications: exclude a tumor or resected region\n"
    "                 (e.g., draw a mask in the AFNI Drawing plugin).\n"
    "           -->>* Note that the emask applies to the base dataset,\n"
    "                 so if you are registering a pre- and post-surgery\n"
    "                 volume, you would probably use the post-surgery\n"
    "                 dataset as the base. If you eventually want the\n"
    "                 result back in the pre-surgery space, then you\n"
    "                 would use the inverse warp afterwards (in 3dNwarpApply).\n"
    "\n"
    " -inedge      = Enhance interior edges in the base and source volumes, to\n"
    "                make the cost functional give more weight to these edges.\n"
    "               * This option MIGHT produce slightly better alignments, but\n"
    "                 its effect is small.\n"
    "               * The output transformed source dataset will NOT have these\n"
    "                 enhanced edges; the enhancement is done internally on the\n"
    "                 volume image copies that are being matched.\n"
    "\n"
    "++++++++++ Blurring the inputs (avoid trying to match TOO much detail) +++++++++\n"
    "\n"
    " -blur bb     = Gaussian blur the input images by 'bb' (FWHM) voxels before\n"
    "                doing the alignment (the output dataset will not be blurred).\n"
    "                The default is 2.345 (for no good reason).\n"
    "               * Optionally, you can provide 2 values for 'bb', and then\n"
    "                 the first one is applied to the base volume, the second\n"
    "                 to the source volume.\n"
    "           -->>* e.g., '-blur 0 3' to skip blurring the base image\n"
    "                 (if the base is a blurry template, for example).\n"
    "               * A negative blur radius means to use 3D median filtering,\n"
    "                 rather than Gaussian blurring. This type of filtering will\n"
    "                 better preserve edges, which might be important in alignment.\n"
    "               * If the base is a template volume that is already blurry,\n"
    "                 you probably don't want to blur it again, but blurring\n"
    "                 the source volume a little is probably a good idea, to\n"
    "                 help the program avoid trying to match tiny features.\n"
#ifdef ALLOW_DUPLO
    "               * Note that -duplo will blur the volumes some extra\n"
    "                 amount for the initial small-scale warping, to make\n"
    "                 that phase of the program converge more rapidly.\n"
#endif
    "\n"
    " -pblur       = Use progressive blurring; that is, for larger patch sizes,\n"
    "                the amount of blurring is larger. The general idea is to\n"
    "                avoid trying to match finer details when the patch size\n"
    "                and incremental warps are coarse. When '-blur' is used\n"
    "                as well, it sets a minimum amount of blurring that will\n"
    "                be used. [06 Aug 2014 -- '-pblur' may be the default someday].\n"
    "               * You can optionally give the fraction of the patch size that\n"
    "                 is used for the progressive blur by providing a value between\n"
    "                 0 and 0.25 after '-pblur'. If you provide TWO values, the\n"
    "                 the first fraction is used for progressively blurring the\n"
    "                 base image and the second for the source image. The default\n"
    "                 parameters when just '-pblur' is given is the same as giving\n"
    "                 the options as '-pblur 0.09 0.09'.\n"
    "               * '-pblur' is useful when trying to match 2 volumes with high\n"
    "                 amounts of detail; e.g, warping one subject's brain image to\n"
    "                 match another's, or trying to match a detailed template.\n"
    "               * Note that using negative values with '-blur' means that the\n"
    "                 progressive blurring will be done with median filters, rather\n"
    "                 than Gaussian linear blurring.\n"
    "         -->>*** The combination of the -allineate and -pblur options will make\n"
    "                 the results of using 3dQwarp to align to a template somewhat\n"
    "                 less sensitive to initial head position and scaling.\n"
    "\n"
    " -nopblur     = Don't use '-pblur'; equivalent to '-pblur 0 0'.\n"
    "\n"
    "++++++++++ Restricting the warp directions ++++++++++\n"
    "\n"
    " -noXdis      = These options let you specify that the warp should not\n"
    " -noYdis      = displace in the given direction. For example, combining\n"
    " -noZdis      = -noXdis and -noZdis would mean only warping along the\n"
    "                y-direction would be allowed.\n"
    "               * Here, 'x' refers to the first coordinate in the dataset,\n"
    "                 which is usually the Right-to-Left direction. Et cetera.\n"
    "               * Note that the output WARP dataset(s) will have sub-bricks\n"
    "                 for the displacements which are all zero; every WARP dataset\n"
    "                 has 3 sub-bricks.\n"
#if 0
    "               * xyz coordinates herein refer to the DICOM order, where\n"
    "                   -x = Right  -y = Anterior   -z = Inferior\n"
    "                   +x = Left   +y = Posterior  +z = Superior\n"
#endif
    "\n"
    "++++++++++ Controlling the warp calculation process in detail ++++++++++\n"
    "\n"
    " -iniwarp ww  = 'ww' is a dataset with an initial nonlinear warp to use.\n"
    "               * If this option is not used, the initial warp is the identity.\n"
    "               * You can specify a catenation of warps (in quotes) here, as in\n"
    "                 program 3dNwarpApply.\n"
    "               * You can scale a 3D warp's displacments by prefixing the dataset\n"
    "                 name by 'FAC:a,b,c:Warpdatasetname' where a b c are numbers\n"
    "                 by which to scale the x- y- z-displacments.\n"
    "               * As a special case, if you just input an affine matrix in a .1D\n"
    "                 file, that also works -- it is treated as giving the initial\n"
    "                 warp via the string \"IDENT(base_dataset) matrix_file.aff12.1D\".\n"
#ifdef ALLOW_DUPLO
    "               * You CANNOT use this option with -duplo !!\n"
#endif
    "               * -iniwarp is usually used with -inilev to re-start 3dQwarp from\n"
    "                 a previous stopping point, or from the output of '-allsave'.\n"
    "               * In particular, '-iniwarp' and '-resample' will work\n"
    "                 together if you need to re-start a warp job from the\n"
    "                 output of '-allsave'.\n"
#if 0
    "               * Special cases allow the creation of an initial affine 'warp'\n"
    "                 from a list of 12 numbers:\n"
    "                 * 'MATRIX(a11,a12,a13,a14,a21,a22,a23,a24,a31,a32,a33,a44)'\n"
    "                   provides the coordinate transformation matrix directly\n"
    "                   as might come from 3dAllineate's '-1Dmatrix_save' option.\n"
    "                 * 'PARAM(dx,dy,dz,za,xa,ya,sx,sy,sz,xs,ys,zs)'\n"
    "                   provides the 3 shift, 3 angle, 3 scale, and 3 shear\n"
    "                   parameters, as would come from 3dAllineate's\n"
    "                   '-1Dparam_save' option.\n"
    "                 * The numeric parameters can be separated by commas\n"
    "                   or blanks. The closing ')' isn't really required, but\n"
    "                   the opening '(' after 'MATRIX' or 'PARAM' is needed.\n"
    "                   For this reason, you will probably need to put this\n"
    "                   argument inside 'single' or \"double\" quotes, to protect\n"
    "                   it from interpretation by the Unix shell.\n"
#endif
    "\n"
    " -inilev lv   = 'lv' is the initial refinement 'level' at which to start.\n"
#ifdef ALLOW_DUPLO
    "               * Usually used with -iniwarp; CANNOT be used with -duplo.\n"
#endif
    "               * The combination of -inilev and -iniwarp lets you take the\n"
    "                 results of a previous 3dQwarp run and refine them further:\n"
    "                   3dQwarp -prefix Q25 -source SS+tlrc -base TEMPLATE+tlrc \\\n"
    "                           -minpatch 25 -blur 0 3\n"
    "                   3dQwarp -prefix Q11 -source SS+tlrc -base TEMPLATE+tlrc \\\n"
    "                           -inilev 7 -iniwarp Q25_WARP+tlrc -blur 0 2\n"
    "                 Note that the source dataset in the second run is the SAME as\n"
    "                 in the first run. If you don't see why this is necessary,\n"
    "                 then you probably need to seek help from an AFNI guru.\n"
    "          -->>** Also see the script @toMNI_Qwarpar for the use of this option\n"
    "                 in creating a template dataset from a collection of scans from\n"
    "                 different subjects.\n"
    "\n"
    " -minpatch mm = Set the minimum patch size for warp searching to 'mm' voxels.\n"
    "   *OR*        * The value of mm should be an odd integer.\n"
    " -patchmin mm  * The default value of mm is 25.\n"
    "               * For more accurate results than mm=25, try 19 or 13.\n"
    "               * The smallest allowed patch size is " NGMINS ".\n"
#ifdef USE_OMP
    "               * OpenMP parallelization becomes inefficient for patch sizes\n"
    "                 smaller than about 15x15x15 -- which is why running 3dQwarp\n"
    "                 down to the minimum patch level of " NGMINS " can be very slow.\n"
#endif
#if (NGMIN < 7) && defined(ALLOW_QMODE)
    "               * You may want stop at a larger patch size (say 7 or 9) and use\n"
    "                 the -Qfinal option to run that final level with quintic warps,\n"
    "                 which might run faster and provide the same degree of warp detail.\n"
#endif
    "               * Trying to make two different brain volumes match in fine detail\n"
    "                 is usually a waste of time, especially in humans. There is too\n"
    "                 much variability in anatomy to match gyrus to gyrus accurately,\n"
    "                 especially in the small foldings in the outer cerebral cortex.\n"
    "                 For this reason, the default minimum patch size is 25 voxels.\n"
    "                 Using a smaller '-minpatch' might try to force the warp to\n"
    "                 match features that do not match, and the result can be useless\n"
    "                 image distortions -- another reason to LOOK AT THE RESULTS.\n"
    "                                                        -------------------\n"
    "\n"
    " -maxlev lv   = Here, 'lv' is the maximum refinement 'level' to use. This\n"
    "                is an alternate way to specify when the program should stop.\n"
    "               * To only do global polynomial warping, use '-maxlev 0'.\n"
    "               * If you use both '-minpatch' and '-maxlev', then you are\n"
    "                 walking on the knife edge of danger.\n"
    "               * Of course, I know that you LIVE for such thrills.\n"
    "\n"
    " -gridlist gl = This option provides an alternate way to specify the patch\n"
    "                grid sizes used in the warp optimization process. 'gl' is\n"
    "                a 1D file with a list of patches to use -- in most cases,\n"
    "                you will want to use it in the following form:\n"
    "                  -gridlist '1D: 0 151 101 75 51'\n"
    "               * Here, a 0 patch size means the global domain. Patch sizes\n"
    "                 otherwise should be odd integers >= " NGMINS ".\n"
    "               * If you use the '0' patch size again after the first position,\n"
    "                 you will actually get an iteration at the size of the\n"
    "                 default patch level 1, where the patch sizes are 75%% of\n"
    "                 the volume dimension. There is no way to force the program\n"
    "                 to literally repeat the sui generis step of lev=0.\n"
    "               * You cannot use -gridlist with: -plusminus  " DUPLO_STRING ":(\n"
    "\n"
    " -allsave     = This option lets you save the output warps from each level\n"
    "   *OR*         of the refinement process. Mostly used for experimenting.\n"
    " -saveall      * Cannot be used with: -nopadWARP  " DUPLO_STRING ":(\n"
    "               * You could use the saved warps to create different versions\n"
    "                 of the warped source datasets (using 3dNwarpApply), to help\n"
    "                 you visualize how the warping process makes progress.\n"
#ifndef USE_NEW_HSAVE
    "               * Will only save all the outputs if the program terminates\n"
    "                 normally -- if it crashes, or freezes, then all these\n"
    "                 warps are lost.\n"
#else
    "               * The saved warps are written out at the end of each level,\n"
    "                 before the next level starts computation. Thus, they could\n"
    "                 be used to re-start the computation if the program crashed\n"
    "                 (by using options '-inilev' and '-iniwarp').\n"
    "               * If '-allsave' is used with '-plusminus', the intermediate\n"
    "                 saved warps are the \"PLUS\" half-warps (which are what the\n"
    "                 program is optimizing).\n"
#endif

#if defined(USE_OMP) && defined(__GNU_C__) /* I forget why this was here - getting old :( */
#endif

#ifdef ALLOW_DUPLO
    "\n"
    " -duplo       = Start off with 1/2 scale versions of the volumes,\n"
    "                for getting a speedy coarse first alignment.\n"
    "               * Then scales back up to register the full volumes.\n"
    "                 The goal is greater speed, and it seems to help this\n"
    "                 positively piggish program to be more expeditious.\n"
    "               * However, accuracy is somewhat lower with '-duplo',\n"
    "                 for reasons that currenly elude Zhark; for this reason,\n"
    "                 the Emperor does not usually use '-duplo'.\n"
#else
    "\n"
    " -duplo       = *** THIS OPTION IS NO LONGER AVAILABLE ***\n"
#endif
    "\n"
    " -workhard    = Iterate more times, which can help when the volumes are\n"
    "                hard to align at all, or when you hope to get a more precise\n"
    "                alignment.\n"
    "               * Slows the program down (possibly a lot), of course.\n"
#ifdef ALLOW_DUPLO
    "               * When you combine '-workhard'  with '-duplo', only the\n"
    "                 full size volumes get the extra iterations.\n"
#endif
    "               * For finer control over which refinement levels work hard,\n"
    "                 you can use this option in the form (for example)\n"
    "                     -workhard:4:7\n"
    "                 which implies the extra iterations will be done at levels\n"
    "                 4, 5, 6, and 7, but not otherwise.\n"
    "               * You can also use '-superhard' to iterate even more, but\n"
    "                 this extra option will REALLY slow things down.\n"
    "           -->>* Under most circumstances, you should not need to use either\n"
    "                 -workhard or -superhard.\n"
#ifdef ALLOW_DUPLO
    "           -->>* The fastest way to register to a template image is via the\n"
    "                 -duplo option, and without the -workhard or -superhard options.\n"
    "               * But accuracy may suffer :(\n"
#endif
#ifdef ALLOW_QMODE
    "           -->>* If you use this option in the form '-Workhard' (first letter\n"
    "                 in upper case), then the second iteration at each level is\n"
    "                 done with quintic polynomial warps.\n"
#endif

#ifdef ALLOW_QMODE
    "\n"
    " -Qfinal      = At the finest patch size (the final level), use Hermite\n"
    "                quintic polynomials for the warp instead of cubic polynomials.\n"
    "               * In a 3D 'patch', there are 2x2x2x3=24 cubic polynomial basis\n"
    "                 function parameters over which to optimize (2 polynomials\n"
    "                 dependent on each of the x,y,z directions, and 3 different\n"
    "                 directions of displacement).\n"
    "               * There are 3x3x3x3=81 quintic polynomial parameters per patch.\n"
    "               * With -Qfinal, the final level will have more detail in\n"
    "                 the allowed warps, at the cost of yet more CPU time.\n"
#if NGMIN < 7
    "               * However, no patch below 7x7x7 in size will be done with quintic\n"
    "                 polynomials.\n"
    "               * This option is also not usually needed, and is experimental.\n"
#endif
#endif /* ALLOW_QMODE */

#if 0                /* Don't let the user know about this option! [Apr 2016] */
#ifdef ALLOW_BASIS5
    "\n"
    " -5final      = At the finest patch size (the last level), use 'cubic+3'\n"
    "                polynomials -- 5 parameters in each direction.\n"
    "               * This option only works for final patch size between 13 and 23\n"
    "                 (inclusive), and should be considered experimental for now.\n"
    "               * The idea is to allow finer scale optimization at larger final\n"
    "                 patch sizes, in the hopes that this choice will give results\n"
    "                 similar to '-minpatch 7' but with more efficient use of\n"
    "                 OpenMP threads.\n"
    "               * For now [Apr 2016], this option is still being benchmarked\n"
    "                 and otherwise tested. Be careful out there!\n"
#endif
#endif
    "\n"
    " -Qonly       = Use Hermite quintic polynomials at all levels.\n"
    "               * Very slow (about 4 times longer). Also experimental.\n"
    "               * Will produce a (discrete representation of a) C2 warp.\n"
    "\n"
    " -nopad      = Do NOT use zero-padding on the 3D base and source images.\n"
    "               [Default == zero-pad as needed]\n"
    "              * The underlying model for deformations goes to zero at the\n"
    "                edge of the volume being warped. However, if there is\n"
    "                significant data near an edge of the volume, then it won't\n"
    "                get displaced much, and so the results might not be good.\n"
    "              * Zero padding is designed as a way to work around this potential\n"
    "                problem. You should NOT need the '-nopad' option for any\n"
    "                reason that Zhark can think of, but it is here to be symmetrical\n"
    "                with 3dAllineate.\n"
    "              * Note that the output (warped from source) dataset will be on the\n"
    "                base dataset grid whether or not zero-padding is allowed.,\n"
    "                However, unless you use the following option, allowing zero-\n"
    "                padding (i.e., the default operation) will make the output WARP\n"
    "                dataset(s) be on a larger grid (also see '-expad' below).\n"
    "\n"
    " -nopadWARP   = If you do NOT use '-nopad' (that is, you DO allow zero-padding\n"
    "                during the warp computations), then the computed warp will often\n"
    "                be bigger than the base volume. This situation is normally not\n"
    "                an issue, but if for some reason you require the warp volume to\n"
    "                match the base volume, then use '-nopadWARP' to have the output\n"
    "                WARP dataset(s) truncated.\n"
    "               * Note that 3dNwarpApply and 3dNwarpAdjust will deal with warps\n"
    "                 that are defined over grids that are larger than the datasets\n"
    "                 to which they are applied; this is why Zhark says above that\n"
    "                 a padded warp 'is normally not an issue'.\n"
    "\n"
    " -expad EE    = This option instructs the program to pad the warp by an extra\n"
    "                'EE' voxels (and then 3dQwarp starts optimizing it).\n"
    "               * This option is seldom needed, but can be useful if you\n"
    "                 might later catenate the nonlinear warp -- via 3dNwarpCat --\n"
    "                 with an affine transformation that contains a large shift.\n"
    "                 Under that circumstance, the nonlinear warp might be shifted\n"
    "                 partially outside its original grid, so expanding that grid\n"
    "                 can avoid this problem.\n"
    "               * Note that this option perforce turns off '-nopadWARP'.\n"
    "\n"
    " -ballopt     = Normally, the incremental warp parameters are optimized inside\n"
    "                a rectangular 'box' (24 dimensional for cubic patches, 81 for\n"
    "                quintic patches), whose limits define the amount of distortion\n"
    "                allowed at each step. Using '-ballopt' switches these limits\n"
    "                to be applied to a 'ball' (interior of a hypersphere), which\n"
    "                can allow for larger incremental displacements. Use this\n"
    "                option if you think things need to be able to move farther.\n"
    "\n"
    " -boxopt      = Use the 'box' optimization limits instead of the 'ball'\n"
    "                [this is the default at present].\n"
    "               * Note that if '-workhard' is used, then ball and box\n"
    "                 optimization are alternated in the different iterations at\n"
    "                 each level, so these two options have no effect in that case.\n"

#ifdef ALLOW_PLUSMINUS
    "\n"
    "++++++++++ Meet-in-the-middle warping - Also know as '-plusminus' +++++++++\n"
    "\n"
    " -plusminus   = Normally, the warp displacements dis(x) are defined to match\n"
    "                base(x) to source(x+dis(x)). With this option, the match\n"
    "                is between base(x-dis(x)) and source(x+dis(x)) -- the two\n"
    "                images 'meet in the middle'.\n"
    "               * One goal is to mimic the warping done to MRI EPI data by\n"
    "                 field inhomogeneities, when registering between a 'blip up'\n"
    "                 and a 'blip down' down volume, which will have opposite\n"
    "                 distortions.\n"
    "               * Define Wp(x) = x+dis(x) and Wm(x) = x-dis(x). Then since\n"
    "                 base(Wm(x)) matches source(Wp(x)), by substituting INV(Wm(x))\n"
    "                 wherever we see x, we have base(x) matches source(Wp(INV(Wm(x))));\n"
    "                 that is, the warp V(x) that one would get from the 'usual' way\n"
    "                 of running 3dQwarp is V(x) = Wp(INV(Wm(x))).\n"
    "               * Conversely, we can calculate Wp(x) in terms of V(x) as follows:\n"
    "                   If V(x) = x + dv(x), define Vh(x) = x + dv(x)/2;\n"
    "                   then Wp(x) = V(INV(Vh(x)))\n"
    "               * With the above formulas, it is possible to compute Wp(x) from\n"
    "                 V(x) and vice-versa, using program 3dNwarpCalc. The requisite\n"
    "                 commands are left as exercises for aspiring AFNI Jedi Masters.\n"
    "               *** Also see the '-pmBASE' option described below.\n"
    "           -->>* Alas: -plusminus does not work with: -allineate  " DUPLO_STRING ":-(\n"
    "                    ++ If a prior linear alignment is needed, it will have\n"
    "                       to be done \"manually\" using 3dAllineate, and then use\n"
    "                       the output of that program as the '-source' dataset for\n"
    "                       3dQwarp.\n"
    "                    ++ Generally speaking, -plusminus works well if the base and\n"
    "                       source datasets are reasonably well aligned to start with.\n"
    "               * However, you can use -iniwarp with -plusminus :-)\n"
#ifdef USE_PLUSMINUS_INITIALWARP
    "               * If -plusminus is used, the -plusminus warp is initialized by\n"
    "                 a coarse warping of the source to the base, then these warp\n"
    "                 displacements are scaled by 0.5, and then the actual\n"
    "                 'meet in the middle' warp optimization begins from that point.\n"
#endif
    "           -->>* The outputs have _PLUS (from the source dataset) and _MINUS\n"
    "                 (from the base dataset) in their filenames, in addition to\n"
    "                 the {prefix}. The -iwarp option, if present, will be ignored.\n"
    "               * If you use '-iniwarp' with '-plusminus', the warp dataset to\n"
    "                 provide with '-iniwarp' is the '_PLUS' warp. That is, you can't\n"
    "                 use a \"full base-to-source warp\" for the initial warp\n"
    "                 (one reason '-allineate' doesn't work with '-plusminus').\n"
    "\n"
    " -pmNAMES p m = This option lets you change the PLUS and MINUS prefix appendages\n"
    "                alluded to directly above to something else that might be more\n"
    "                easy for you to grok. For example, if you are warping EPI\n"
    "                volumes with phase-encoding in the LR-direction with volumes\n"
    "                that had phase-encoding in the RL-direction, you might do\n"
    "                something like\n"
    "   -base EPI_LR+orig -source EPI_RL+orig -plusminus -pmNAMES RL LR -prefix EPIuw\n"
    "                recalling that the PLUS name goes with the source (RL) and the\n"
    "                MINUS name goes with the base (RL). Then you'd end up with\n"
    "                datasets\n"
    "                  EPIuw_LR+orig and EPIuw_LR_WARP+orig from the base\n"
    "                  EPIuw_RL+orig and EPIuw_RL_WARP+orig from the source\n"
    "                The EPIuw_LR_WARP+orig file could then be used to unwarp (e.g.,\n"
    "                using 3dNwarpApply) other LR-encoded EPI datasets from the same\n"
    "                scanning session.\n"
#endif
    "\n"
    " -pmBASE      = With '-plusminus', computes the V(x) warp (source to base)\n"
    "                from the plusminus half-warp, and writes it to disk.\n"
    "                Also writes out the source dataset warped to base space,\n"
    "                in addition to the Wp(x) '_PLUS' and Wm(x) '_MINUS' results\n"
    "               * Sneaky aside: if you want the potential for larger displacements\n"
    "                 than 'normal' 3dQwarp, use '-plusminus', since the meet-in-the-\n"
    "                 middle approach will allow the full-size displacements in EACH\n"
    "                 of the half-warps, so that the overall displacment between\n"
    "                 base and source can be larger. The use of '-pmBASE' will let\n"
    "                 you get the source-transformed-to-base result at the end.\n"
    "                 If you don't want the plusminus 'in-the-middle' outputs,\n"
    "                 just delete them later.\n"
    "\n"
    "++++++++++ How 'loud' do you want this program to be? ++++++++++\n"
    "\n"
    " -verb        = Print out very verbose progress messages (to stderr) :-)\n"
    " -quiet       = Cut out most of the fun fun fun progress messages :-(\n"
    "\n"
    "-----------------------------------\n"
    "INTERRUPTING the program gracefully\n"
    "-----------------------------------\n"
    "If you want to stop the program AND have it write out the results up to\n"
    "the current point, you can do so with a command like\n"
    "  kill -s QUIT processID\n"
    "where 'processID' is the process identifier number (pid) for the 3dQwarp\n"
    "program you want to terminate. A command like\n"
    "  ps aux | grep 3dQwarp\n"
    "will give you a list of all your processes with the string '3dQwarp' in\n"
    "the command line. For example, at the moment I wrote this text, I would\n"
    "get the response\n"
    " rwcox 62873 693.8 2.3 3274496 755284 p2 RN+ 12:36PM 380:25.26 3dQwarp -prefix ...\n"
    " rwcox  6421   0.0 0.0 2423356    184 p0 R+   1:33PM   0:00.00 grep 3dQwarp\n"
    " rwcox  6418   0.0 0.0 2563664   7344 p4 S+   1:31PM   0:00.15 vi 3dQwarp.c\n"
    "so the processID for the actual run of 3dQwarp was 62873.\n"
    "(Also, you can see that Zhark is a 'vi' acolyte, not an 'emacs' heretic.)\n"
    "\n"
    "The program will 'notice' the QUIT signal at the end of the optimization\n"
    "of the next patch, so it may be a moment or two before it actually saves\n"
    "the output dataset(s) and exits.\n"
    "\n"
    "Of course, if you just want to kill the process in a brute force way, with\n"
    "nothing left behind to examine, then 'kill processID' will work.\n"
#ifdef USE_NEW_HSAVE
    "\n"
    "Using 'kill -s QUIT' combined with '-allsave' might be useful in some\n"
    "circumstances. At least to get some idea of what happened before you\n"
    "were forced to stop 3dQwarp.\n"
#endif
    "\n"
    "----------------------------------------------------------------\n"
    "CLARIFICATION about the confusing forward and inverse warp issue\n"
    "----------------------------------------------------------------\n"
    "An AFNI nonlinear warp dataset stores the displacements (in DICOM mm) from\n"
    "the base dataset grid to the source dataset grid. For computing the source\n"
    "dataset warped to the base dataset grid, these displacements are needed,\n"
    "so that for each grid point in the output (warped) dataset, the corresponding\n"
    "location in the source dataset can be found, and then the value of the source\n"
    "at that point can be computed (interpolated).\n"
    "\n"
    "That is, this forward warp is good for finding where a given point in the\n"
    "base dataset maps to in the source dataset. However, for finding where a\n"
    "given point in the source dataset maps to in the base dataset, the inverse\n"
    "warp is needed. Or, if you wish to warp the base dataset to 'look like' the\n"
    "source dataset, then you use 3dNwarpApply with the input warp being the\n"
    "inverse warp from 3dQwarp.\n"
    "\n"
    "-----------------------------------\n"
    "OUTLINE of warp optimization method\n"
    "-----------------------------------\n"
    "Repeated composition of incremental warps defined by Hermite cubic basis\n"
    "functions, first over the entire volume, then over steadily shrinking and\n"
    "overlapping patches increasing 'levels': the patches shrink by a factor of\n"
    "0.75 at each level).\n"
    "\n"
    "At 'level 0' (over the entire volume), Hermite quintic basis functions are also\n"
    "employed, but these are not used at the more refined levels. All basis functions\n"
    "herein are (at least) continuously differentiable, so the discrete warp computed\n"
    "will be a representation of an underlying C1 diffeomorphism. The basis functions\n"
    "go to zero at the edge of each patch, so the overall warp will decay to the\n"
    "identity warp (displacements=0) at the edge of the base volume. (However, use\n"
    "of '-allineate' can make the final output warp be nonzero at the edges; the\n"
    "programs that apply warps to datasets linearly extrapolate warp displacements\n"
    "outside the 3D box over which the warp is defined.)\n"
    "\n"
    "For this procedure to work, the source and base datasets need to be reasonably\n"
    "well aligned already (e.g., via 3dAllineate, if necessary). Multiple warps can\n"
    "later be composed and applied via programs 3dNwarpApply and/or 3dNwarpCalc.\n"
    "\n"
    "Note that it is not correct to say that the resulting warp is a piecewise cubic\n"
    "(or quintic) polynomial. The first warp created (at level 0) is such a warp;\n"
    "call that W0(x). Then the incremental warp W1(x) applied at the next iteration\n"
    "is also a cubic polynomial warp (say), and the result is W0(W1(x)), which is\n"
    "more complicated than a cubic polynomial -- and so on. The incremental warps\n"
    "aren't added, but composed, so that the mathematical form of the final warp\n"
    "would be very unwieldy to express in polynomial form. Of course, the program\n"
    "just keeps track of the displacements, not the polynomial coefficients, so it\n"
    "doesn't 'care' about the underlying polynomials at all.\n"
    "\n"
    "One reason for incremental improvement by composition, rather than by addition,\n"
    "is the simple fact that if W0(x) is invertible and W1(x) is invertible, then\n"
    "W0(W1(x)) is also invertible -- but W0(x)+W1(x) might not be. The incremental\n"
    "polynomial warps are kept invertible by simple constraints on the magnitudes\n"
    "of their coefficients.\n"
    "\n"
    "The penalty is a Neo-Hookean elastic energy function, based on a combination of\n"
    "bulk and shear distortions: cf. http://en.wikipedia.org/wiki/Neo-Hookean_solid\n"
    "The goal is to keep the warps from becoming too 'weird' (doesn't always work).\n"
    "\n"
    "By perusing the many options above, you can see that the user can control the\n"
    "warp optimization in various ways. All these options make using 3dQwarp seem\n"
    "pretty complicated. The reason there are so many options is that many different\n"
    "cases arise, and we are trying to make the program flexible enough to deal with\n"
    "them all. The SAMPLE USAGE section above is a good place to start for guidance.\n"
    "Or you can use the @SSwarper or auto_warp.py scripts.\n"
    "\n"
    "-------------------------------------------------------------------------------\n"
    "***** This program is experimental and subject to sudden horrific change! *****\n"
    "-------------------------------------------------------------------------------\n"
    "\n"
    "----- AUTHOR = Zhark the Grotesquely Warped -- Fall/Winter/Spring 2012-13 -----\n"
    "-----          (but still strangely handsome)                             -----\n"
  ) ;

  PRINT_AFNI_OMP_USAGE(
   "3dQwarp",
   "* Tests show that using more 10-12 CPUs with 3dQwarp doesn't help much.\n"
   "  If you have more CPUs on one system, it's faster to run two or three\n"
   "  separate registration jobs in parallel than to use all the CPUs on\n"
   "  one 3dQwarp task at a time.\n"
  ) ;
  exit(0) ;
}

/*---------------------------------------------------------------------------*/
/* Run 3dAllineate; result is stored in Qunstr.nii and Qunstr.aff12.1D */
/*---------------------------------------------------------------------------*/

static char *Qunstr=NULL ;  /* will be generated as a random unique string */

void Qallineate( char *basname , char *srcname , char *emkname , char *allopt )
{
   char *cmd ; int ss ;

   /* make space for the 3dAllineate command string */

   ss  = strlen(basname)+strlen(srcname)+2048 ;
   if( allopt  != NULL ) ss += strlen(allopt) ;
   if( emkname != NULL ) ss += strlen(emkname) ;
   cmd = (char *)malloc(ss) ;

   Qunstr = UNIQ_idcode() ;  /* create unique prefix for output filenames */

   /* setup the basic command to do some hard work */

   sprintf( cmd , "3dAllineate"
                  " -base %s"
                  " -source %s"
                  " -prefix %s.nii"
                  " -1Dmatrix_save %s"
                  " -cmass -final wsinc5 -float -master BASE -twobest 7" ,
            basname , srcname , Qunstr , Qunstr ) ;

   /* add options to the command string */

   switch( Hverb ){                           /* match 3dQwarp's verbosity */
     case 0: strcat(cmd," -quiet") ; break ;
     case 3:
     case 2: strcat(cmd," -verb" ) ; break ;
   }

   if( emkname != NULL )                      /* match 3dQwarp's -emask */
     sprintf( cmd+strlen(cmd) , " -emask %s" , emkname) ;

   if( allopt != NULL && *allopt != '\0' )    /* user-supplied options */
     sprintf( cmd+strlen(cmd) , " %s"        , allopt) ;

   if( allopt == NULL || strstr(allopt,"-fineblur") == NULL )
     strcat(cmd," -fineblur 4.44") ;

#if 0
   if( allopt == NULL || strstr(allopt,"-onepass") == NULL )
     strcat(cmd," -norefinal") ;
#endif

   /* and do the (external) work */

   INFO_message("Starting 3dAllineate (affine register) command:\n  %s\n ",cmd);
   INFO_message("###########################################################") ;
   ss = system(cmd) ;
   if( ss != 0 ) ERROR_exit("3dQwarp: 3dAllineate command failed :-(") ;
   free(cmd) ;
   return ;
}

/*---------------------------------------------------------------------------*/
/* Just use 3dAllineate for resampling (no registration) */
/*---------------------------------------------------------------------------*/

void Qallin_resample( char *basname , char *srcname )  /* 17 Jul 2013 */
{
   char *cmd ; int ss ;

   ss  = strlen(basname)+strlen(srcname)+2048 ;
   cmd = (char *)malloc(ss) ;

   Qunstr = UNIQ_idcode() ;

   sprintf( cmd , "3dAllineate"
                  " -master %s"
                  " -source %s"
                  " -prefix %s.nii"
                  " -final wsinc5 -float -quiet -1Dparam_apply '1D: 12@0'\\'" ,
            basname , srcname , Qunstr ) ;

   INFO_message("Starting 3dAllineate (resample only) command:\n  %s\n ",cmd) ;
   INFO_message("###########################################################") ;
   ss = system(cmd) ;
   if( ss != 0 ) ERROR_exit("3dQwarp: 3dAllineate command failed :-(") ;
   free(cmd) ;
   return ;
}

/*---------------------------------------------------------------------------*/
/* Function for writing out intermediate saved warps during optimization */

static int do_allin=0 ; char *allopt=NULL ;
static mat44 allin_matrix, allin_adjust_matrix ;
static THD_3dim_dataset *adset=NULL , *bset=NULL ;
static char *prefix = "Qwarp" ;
static int saved_argc=0 ; static char **saved_argv=NULL ;

#ifdef USE_NEW_HSAVE
void save_intermediate_warp( IndexWarp3D *hwarp , char *nam )
{
   IndexWarp3D *tarp ; char suffix[64], *qprefix ; THD_3dim_dataset *qset ;

   if( hwarp == NULL || nam == NULL ) return ;

ENTRY("save_intermediate_warp") ;

   if( do_allin ){
STATUS("compose with allin") ;
     tarp = IW3D_compose_w1m2(hwarp,allin_adjust_matrix,MRI_WSINC5) ;
   } else {
     tarp = hwarp ;
   }

STATUS("adoption") ;
   IW3D_adopt_dataset(tarp,adset) ;
   if( do_plusminus )
     sprintf(suffix,"_%s_PLUS_WARPsave",nam) ;
   else
     sprintf(suffix,"_%s_WARPsave",nam) ;
   qprefix = modify_afni_prefix(prefix,NULL,suffix) ;
STATUS("convert to dset") ;
   qset = IW3D_to_dataset( tarp , qprefix ) ;
   if( bset != NULL ){
STATUS("copy history") ;
     tross_Copy_History( bset , qset ) ;
     if( saved_argc > 0 ){
STATUS("make new history") ;
       tross_Make_History( "3dQwarp" , saved_argc,saved_argv , qset ) ;
     }
STATUS("copy atlas_space") ;
     MCW_strncpy( qset->atlas_space , bset->atlas_space , THD_MAX_NAME ) ;
  }
  if( do_allin ){  /* save copy of 3dAllineate matrix into header */
    float qar[12] ;
    UNLOAD_MAT44(allin_matrix,qar[0],qar[1],qar[ 2],qar[ 3],
                              qar[4],qar[5],qar[ 6],qar[ 7],
                              qar[8],qar[9],qar[10],qar[11] ) ;
    THD_set_float_atr( qset->dblk , "QWARP_ALLIN_MATRIX" , 12 , qar ) ;
  }
STATUS("write warp") ;
   DSET_write(qset); fprintf(stderr,"[%s]",DSET_BRIKNAME(qset)); DSET_delete(qset);
   if( tarp != hwarp ) IW3D_destroy(tarp) ;
   if( Hverb > 1 ) fprintf(stderr,"\n") ;
   EXRETURN ;
}
#endif

/*****************************************************************************/
/*---------------------------------------------------------------------------*/
/*                                                                           */
/***                Main program for 3dQwarp!!!                            ***/
/*                                                                           */
/*---------------------------------------------------------------------------*/
/*****************************************************************************/

int main( int argc , char *argv[] )
{
   THD_3dim_dataset *sset=NULL , *oset , *iwset=NULL , *sstrue=NULL ;
   char *bsname=NULL , *iwname=NULL , *ssname=NULL , *esname=NULL ;
   MRI_IMAGE *bim=NULL , *wbim=NULL , *sim=NULL , *oim=NULL ; float bmin,smin ;
   IndexWarp3D *oww=NULL , *owwi=NULL ; Image_plus_Warp *oiw=NULL ;
   char *prefix_clean=NULL ; int nopt , nevox=0 ;
   char *wtprefix=NULL  , *wtprefix_clean=NULL ;
   int meth=GA_MATCH_PEARCLP_SCALAR ; int meth_is_lpc=0 ;
   int ilev=0 , nowarp=0 , nowarpi=1 , mlev=666 , nodset=0 , nnz ;
   int do_awarp=0 ; /* 21 Dec 2016 */
   int duplo=0 , qsave=0 , minpatch=0 , nx,ny,nz , ct , nnn , noneg=0 ;
   float dx,dy,dz ;
   float dxal=0.0f,dyal=0.0f,dzal=0.0f ; int have_dxyzal=0 ;
   int do_resam=0 ; int keep_allin=1 ;
   int flags=0 , nbad=0 ;
   double cput=0.0 ;
   Image_plus_Warp **sbww=NULL, *qiw=NULL; /* 14 May 2013 */
   char *plusname="PLUS" , *minusname="MINUS" ;
   char appendage[THD_MAX_NAME] ;
   int zeropad=1, pad_xm=0,pad_xp=0, pad_ym=0,pad_yp=0, pad_zm=0,pad_zp=0 ; /* 13 Sep 2013 */
   int nxold=0,nyold=0,nzold=0 ;
   int zeropad_warp=1 ;
   int expad=0 , minpad=0 ;
   int iwpad_xm=0,iwpad_xp=0, iwpad_ym=0,iwpad_yp=0, iwpad_zm=0,iwpad_zp=0 ;
   int do_pmbase=0 ;
   IndexWarp3D *pmbase_warp=NULL ; MRI_IMAGE *pmbase_imag=NULL ;
   int xyzm_num=0 ; MRI_IMAGE *xyzm_bas=NULL , *xyzm_src=NULL ;

   /*---------- enlighten the supplicant ----------*/

   AFNI_SETUP_OMP(0) ;  /* 24 Jun 2013 */

   if( argc == 1 ) { Qhelp(); exit(0); }

   /*---------- startup bureaucracy --------*/

#if defined(USING_MCW_MALLOC) && !defined(USE_OMP)
   enable_mcw_malloc() ;
#endif

   /* for OpenMP, need some workspaces for each thread */
   /* if no OpenMP, these workspaces are hard-coded in mri_nwarp.c */

#ifdef USE_OMP
   omp_set_nested(0) ;
   nthmax = omp_get_max_threads() ;              /* number of OpenMP threads */
   dhaar  = (double *)malloc(sizeof(double)*nthmax*2) ; /* make 6 workspaces */
   dhbbr  = (double *)malloc(sizeof(double)*nthmax*2) ;   /* for each thread */
   dhccr  = (double *)malloc(sizeof(double)*nthmax*2) ;
   dhddr  = (double *)malloc(sizeof(double)*nthmax*2) ;
   dheer  = (double *)malloc(sizeof(double)*nthmax*2) ;
   dhffr  = (double *)malloc(sizeof(double)*nthmax*2) ;
   INFO_message("OpenMP thread count = %d",nthmax) ;
#else
   INFO_message  ("This edition not compiled with OpenMP.") ;
   ININFO_message("It will be very slooooowwwwww .... :-(") ;
#endif

   /* standard AFNI bureaucracy */

   mainENTRY("3dQwarp") ; machdep() ;
   AFNI_logger("3dQwarp",argc,argv);
   PRINT_VERSION("3dQwarp"); AUTHOR("Zhark the (Hermite) Cubically Warped");
   (void)COX_clock_time() ;  /* initialize the clock timer */
   putenv("AFNI_WSINC5_SILENT=YES") ;

   LOAD_IDENT_MAT44(allin_matrix);        /* Just to quiet initialization warning */
   LOAD_IDENT_MAT44(allin_adjust_matrix); /* Just to quiet initialization warning */

   saved_argc = argc ; saved_argv = argv ; /* 13 Mar 2018 */

   /*------------- scan for and parse options -------------*/

   nopt = 1 ;
   Hblur_b = Hblur_s = 2.345f ;  /* arbitrary initializations */
   while( nopt < argc && argv[nopt][0] == '-' ){   /* loop over cmd line args */

     /*---------------*/

     if( strcasecmp(argv[nopt],"-help") == 0 ||
         strcmp    (argv[nopt],"-h"   ) == 0   ){
       Qhelp(); exit(0);
     }

     /*---------------*/

     if( strcasecmp(argv[nopt],"-verb") == 0 ){
       Hverb++ ; nopt++ ; continue ;
     }

     /*---------------*/

     if( strcasecmp(argv[nopt],"-quiet") == 0 ){
       Hverb = 0 ; nopt++ ; continue ;
     }

     /*---------------*/

     if( strcasecmp(argv[nopt],"-nowarp") == 0 ){
       nowarp =  1 ; nopt++ ; continue ;
     }

     /*---------------*/

     if( strcasecmp(argv[nopt],"-iwarp") == 0 ){
       nowarpi = 0 ; nopt++ ; continue ;
     }

     /*---------------*/

     if( strcasecmp(argv[nopt],"-awarp") == 0 ){ /* 21 Dec 2016 */
       do_awarp = 1 ; nopt++ ; continue ;
     }

     /*---------------*/

     if( strcasecmp(argv[nopt],"-nodset") == 0 ){
       nodset =  1 ; nopt++ ; continue ;
     }

     /*---------------*/

     if( strcasecmp(argv[nopt],"-inedge") == 0 ){  /* Jul 2018 */
#ifdef ALLOW_INEDGE
       Hinedge_doit = 1 ;
#else
       ERROR_message("Option %s is disabled now -- ignoring it",argv[nopt]) ;
#endif
       nopt++ ; continue ;
     }

     /*---------------*/

     if( strcasecmp(argv[nopt],"-warpscale") == 0 ){ /* 08 Aug 2018 [hidden] */
       float fff ;
       if( ++nopt >= argc ) ERROR_exit("need arg after %s",argv[nopt-1]) ;
       fff = (float)strtod(argv[nopt],NULL) ;
       if( fff <= 0.5f ) fff = 0.5f ; else if( fff > 2.0f ) fff = 2.0f ;
       Hfactor_q = fff ;
       nopt++ ; continue ;
     }

     /*---------------*/

     if( strcasecmp(argv[nopt],"-noneg") == 0 ){  /* 24 May 2013 */
       noneg =  1 ; nopt++ ; continue ;
     }

     /*---------------*/

     if( strcasecmp(argv[nopt],"-nopad") == 0 ){  /* 13 Sep 2013 */
       zeropad = 0 ; nopt++ ; continue ;
     }

     /*---------------*/

     if( strcasecmp(argv[nopt],"-nopadWARP") == 0 ){ /* 19 Mar 2014 */
       zeropad_warp = 0 ; nopt++ ; continue ;
     }

     /*---------------*/

     if( strcasecmp(argv[nopt],"-expad") == 0 ){  /* no longer SECRET */
       if( ++nopt >= argc ) ERROR_exit("need arg after %s",argv[nopt-1]) ;
       expad = (int)strtod(argv[nopt],NULL) ;
       if( expad < 0 ) expad = 0 ;
       nopt++ ; continue ;
     }

     /*---------------*/

     if( strcasecmp(argv[nopt],"-minpad") == 0 ){  /* SECRET OPTION */
       if( ++nopt >= argc ) ERROR_exit("need arg after %s",argv[nopt-1]) ;
       minpad = (int)strtod(argv[nopt],NULL) ;
       if( minpad < 0 ) minpad = 0 ;
       nopt++ ; continue ;
     }

     /*---------------*/

     if( strcasecmp(argv[nopt],"-allineate") == 0 ||       /* 15 Jul 2013 */
         strcasecmp(argv[nopt],"-allin"    ) == 0   ){
       do_allin =  1 ; nopt++ ; continue ;
     }

     /*---------------*/

     if( strcasecmp(argv[nopt],"-NOallineate") == 0 ||     /* 17 Mar 2015 */
         strcasecmp(argv[nopt],"-NOallin"    ) == 0   ){
       do_allin =  0 ; nopt++ ; continue ;
     }

     /*---------------*/

     if( strcasecmp(argv[nopt],"-allinkeep") == 0 ){       /* 27 Aug 2013 */
       keep_allin = 1 ; nopt++ ; continue ;
     }

     /*---------------*/

     if( strcasecmp(argv[nopt],"-allinkill") == 0 ){       /* 27 Aug 2013 */
       keep_allin = 0 ; nopt++ ; continue ;
     }

     /*---------------*/

     if( strcasecmp(argv[nopt],"-allinfast") == 0 ||       /* 19 Jul 2013 */
         strcasecmp(argv[nopt],"-allfast"  ) == 0   ){
       do_allin = 2 ; nopt++ ; continue ;
     }

     /*---------------*/

     if( strcasecmp(argv[nopt],"-allineate_opts") == 0 ||  /* 16 Jul 2013 */
         strcasecmp(argv[nopt],"-allopt")         == 0   ){
       if( ++nopt >= argc ) ERROR_exit("need arg after %s",argv[nopt-1]) ;
       allopt = strdup(argv[nopt]) ;
       nopt++ ; continue ;
     }

     /*---------------*/

     if( strcasecmp(argv[nopt],"-resample") == 0 ||
         strcasecmp(argv[nopt],"-resam"   ) == 0   ){      /* 17 Jul 2013 */
       do_resam = 1 ; nopt++ ; continue ;
     }

     /*---------------*/

     if( strcasecmp(argv[nopt],"-plusminus") == 0 || strcmp(argv[nopt],"+-") == 0 ){
#ifdef ALLOW_PLUSMINUS
       do_plusminus++ ; nopt++ ; continue ;
#else
       ERROR_exit("Option '%s' is not currently available :-(",argv[nopt]) ;
#endif
     }

     /*---------------*/

     if( strcasecmp(argv[nopt],"-pmNAMES") == 0 ){
       if( ++nopt >= argc-1 ) ERROR_exit("need 2 args after %s",argv[nopt-1]) ;
       plusname = argv[nopt++] ; minusname = argv[nopt++] ;
       if( strcasecmp(plusname,minusname) == 0 )
         ERROR_exit("You can't use same suffix '%s' twice after '-pmNAMES'",plusname) ;
       if( !THD_filename_pure(plusname) || !THD_filename_pure(minusname) )
         ERROR_exit("Illegal name after '-pmNAMES'") ;
       continue ;
     }

     /*---------------*/

     if( strcasecmp(argv[nopt],"-pmBASE") == 0 ){  /* 12 Aug 2014 */
       do_pmbase = 1 ; nopt++ ; continue ;
     }

     /*---------------*/

     if( strcasecmp(argv[nopt],"-XYZmatch") == 0 ){  /* 15 Aug 2014 */
       double fac ;
       if( xyzm_bas != NULL || xyzm_src != NULL )
         ERROR_exit("You can't use option %s more than once!",argv[nopt]) ;
       if( nopt+3 >= argc )
         ERROR_exit("Need 3 arguments after option %s",argv[nopt]) ;
       fac = strtod(argv[++nopt],NULL) ;
       if( fac < 0.0f ){
         Hxyzmatch_pow = 1 ; Hxyzmatch_fac = -0.1 / fac ;
       } else if( fac > 0.0f ){
         Hxyzmatch_pow = 2 ; Hxyzmatch_fac =  0.1 / fac ;
       } else {
         ERROR_exit("You can't give the scale factor (for option %s) as 0",argv[nopt-1]) ;
       }
       xyzm_bas = mri_read_1D( argv[++nopt] ) ;
       if( xyzm_bas == NULL )
         ERROR_exit("Can't read 1D file '%s' after option %s",argv[nopt],argv[nopt-2]) ;
       if( xyzm_bas->ny != 3 )
         ERROR_exit("1D file '%s' after option '%s' does not have exactly 3 columns",argv[nopt],argv[nopt-2]) ;
       xyzm_src = mri_read_1D( argv[++nopt] ) ;
       if( xyzm_src == NULL )
         ERROR_exit("Can't read 1D file '%s' after option %s",argv[nopt],argv[nopt-3]) ;
       if( xyzm_src->ny != 3 )
         ERROR_exit("1D file '%s' after option '%s' does not have exactly 3 columns",argv[nopt],argv[nopt-3]) ;
       if( xyzm_src->nx != xyzm_bas->nx )
         ERROR_exit("1D files '%s' and '%s' after option '%s' do not have the same number of rows",argv[nopt],argv[nopt-1],argv[nopt-3]) ;
     }

     /*---------------*/

     if( strcasecmp(argv[nopt],"-nowarps") == 0 ){  /* these 2 options */
       WARNING_message("-nowarps option is now deprecated: see the -help output") ;
       nowarpi = nowarp = 1 ; nopt++ ; continue ;   /* are just for backward */
     }                                              /* compatibility */

     /*---------------*/

     if( strcasecmp(argv[nopt],"-nowarpi") == 0 ){
       WARNING_message("-nowarpi option is now deprecated: see the -help output") ;
       nowarpi = 1 ; nopt++ ; continue ;
     }

     /*---------------*/

     if( strcasecmp(argv[nopt],"-patchmin") == 0 || strcasecmp(argv[nopt],"-minpatch") == 0 ){
       if( ++nopt >= argc ) ERROR_exit("need arg after %s",argv[nopt-1]) ;
       minpatch = (int)strtod(argv[nopt],NULL) ;
       if( minpatch < NGMIN ) minpatch = NGMIN ; else if( minpatch%2 == 0 ) minpatch-- ;
       nopt++ ; continue ;
     }

     /*---------------*/

     if( strcasecmp(argv[nopt],"-inilev") == 0 ){
       if( duplo          ) ERROR_exit("Cannot use -inilev with -duplo :-(") ;
       if( ++nopt >= argc ) ERROR_exit("need arg after %s",argv[nopt-1]) ;
       ilev = (int)strtod(argv[nopt],NULL) ;
       if( ilev < 0 ) ilev = 0 ; else if( ilev > 19 ) ilev = 19 ;
       nopt++ ; continue ;
     }

     /*---------------*/

     if( strcasecmp(argv[nopt],"-maxlev") == 0 ){
       if( duplo          ) ERROR_exit("Cannot use -maxlev with -duplo :-(") ;
       if( ++nopt >= argc ) ERROR_exit("need arg after %s",argv[nopt-1]) ;
       mlev = (int)strtod(argv[nopt],NULL) ;
       if( mlev < 0 ) mlev = 0 ; else if( mlev > 99 ) mlev = 99 ;
       nopt++ ; continue ;
     }

     /*---------------*/

     if( strcasecmp(argv[nopt],"-iniwarp") == 0 ){
       char *apt=NULL ;
       if( duplo )          ERROR_exit("Cannot use -iniwarp with -duplo :-(") ;
       if( iwname != NULL ) ERROR_exit("Cannot use -iniwarp twice :-(") ;
       if( ++nopt >= argc ) ERROR_exit("need arg after %s",argv[nopt-1]) ;
       iwname = strdup(argv[nopt]) ; nopt++ ; continue ;
     }

     /*---------------*/

     if( strcasecmp(argv[nopt],"-weight") == 0 ){  /* 17 Oct 2013 - Open Up Day */
       if( wbim != NULL )   ERROR_exit("Cannot use -weight twice :-(") ;
       if( ++nopt >= argc ) ERROR_exit("need arg after %s",argv[nopt-1]) ;
       qset = THD_open_dataset(argv[nopt]) ;
       if( qset == NULL )   ERROR_exit("Cannot open -weight dataset :-(") ;
       DSET_load(qset) ; CHECK_LOAD_ERROR(qset) ;
       wbim = THD_extract_float_brick(0,qset) ; DSET_delete(qset) ; qset=NULL ;
       nopt++ ; continue ;
     }

     /*---------------*/

     if( strcasecmp(argv[nopt],"-duplo") == 0 ){
#ifdef ALLOW_DUPLO
       if( iwname != NULL )
         ERROR_exit("Cannot use -duplo with -iniwarp :-(") ;
       if( ilev != 0 || mlev < 99 )
         ERROR_exit("Cannot use -duplo with -inilev or -maxlev :-(") ;
       duplo = 1 ;
#else
       WARNING_message("-duplo is no longer supported, and is being ignored") ;
#endif
       nopt++ ; continue ;
     }

     /*---------------*/

     if( strncasecmp(argv[nopt],"-workhard",9) == 0 ){
       char *wpt = argv[nopt]+9 , *cpt=NULL ; int addone=0 ;
       Hworkhard1 = 0 ; Hworkhard2 = 66 ;
       if( *wpt == ':' && isdigit(*(wpt+1)) ){
         Hworkhard2 = (int)strtod(++wpt,NULL) ;
         cpt = strchr(wpt,':') ;
         if( cpt != NULL && isdigit(*(cpt+1)) ){
           Hworkhard1 = Hworkhard2 ;
           Hworkhard2 = (int)strtod(cpt+1,NULL) ;
         }
       } else if( nopt+1 < argc                          &&
                  isdigit(argv[nopt+1][0])               &&
                  (cpt=strchr(argv[nopt+1],':')) != NULL &&
                  isdigit(*(cpt+1))                         ){
         Hworkhard1 = (int)strtod(argv[nopt+1],NULL) ;
         Hworkhard2 = (int)strtod(cpt+1,NULL) ;
         addone = 1 ;
       }
#ifdef ALLOW_QMODE
       if( argv[nopt][1] == 'W' ) Hqhard = 1 ;
#endif
       nopt++ ; if( addone ) nopt++ ;

       continue ;
     }

     /*---------------*/
     /* take it easy at the 0 level? */

     if( strcasecmp(argv[nopt],"-zeasy") == 0 ){     /* 26 Jun 2014 */
       Hzeasy = 1 ; nopt++ ; continue ;              /* not in -help */
     }

     /*---------------*/
     /* don't do quintic at the 0 level? */

     if( strcasecmp(argv[nopt],"-noQ") == 0 ){       /* 01 Jul 2014 */
       Hznoq = 1 ; nopt++ ; continue ;               /* not in -help */
     }

     /*---------------*/

     if( strncasecmp(argv[nopt],"-superhard",10) == 0 ){  /* 30 Apr 2013 */
       char *wpt = argv[nopt]+10 , *cpt=NULL ; int addone=0 ;
       Hsuperhard1 = 0 ; Hsuperhard2 = 66 ;
       if( *wpt == ':' && isdigit(*(wpt+1)) ){
         Hsuperhard2 = (int)strtod(++wpt,NULL) ;
         cpt = strchr(wpt,':') ;
         if( cpt != NULL && isdigit(*(cpt+1)) ){
           Hsuperhard1 = Hsuperhard2 ;
           Hsuperhard2 = (int)strtod(++cpt,NULL) ;
         }
       } else if( nopt+1 < argc                          &&
                  isdigit(argv[nopt+1][0])               &&
                  (cpt=strchr(argv[nopt+1],':')) != NULL &&
                  isdigit(*(cpt+1))                         ){
         Hsuperhard1 = (int)strtod(argv[nopt+1],NULL) ;
         Hsuperhard2 = (int)strtod(cpt+1,NULL) ;
         addone = 1 ;
       }
#ifdef ALLOW_QMODE
       if( argv[nopt][1] == 'S' ) Hqhard = 1 ;
#endif
       if( addone ) nopt++ ;
       nopt++ ; continue ;
     }

     /*---------------*/

#ifdef ALLOW_QMODE
     if( strcasecmp(argv[nopt],"-Qfinal") == 0 ){     /* 07 May 2013 */
       Hqfinal = 1 ; H5final = 0 ; nopt++ ; continue ;
     }
     if( strcasecmp(argv[nopt],"-Qonly") == 0 ){      /* 27 Jun 2013 */
       Hqonly = 1 ; H5final = 0 ; nopt++ ; continue ;
     }
#endif

#ifdef ALLOW_BASIS5
     if( strcasecmp(argv[nopt],"-5final") == 0 ){     /* 06 Nov 2015 [SECRET] */
       H5final = 3 ; Hqfinal = 0 ; nopt++ ; continue ;
     }
     if( strcasecmp(argv[nopt],"-4final") == 0 ){     /* 06 Nov 2015 [SECRET] */
       H5final = 2 ; Hqfinal = 0 ; nopt++ ; continue ;
     }
     if( strcasecmp(argv[nopt],"-3final") == 0 ){     /* 06 Nov 2015 [SECRET] */
       H5final = 1 ; Hqfinal = 0 ; nopt++ ; continue ;
     }
     if( strcasecmp(argv[nopt],"-4zero") == 0 ){      /* 12 Apr 2016 [SECRET] */
       H4zero = 1 ; nopt++ ; continue ;
     }
#endif

     /*---------------*/

     if( strcasecmp(argv[nopt],"-qsave") == 0 ){
       WARNING_message("-qsave option no longer works in 3dQwarp :-(") ;
       nopt++ ; continue ;
     }

     /*---------------*/

     if( strcasecmp(argv[nopt],"-noXdis") == 0 ){
       flags |= NWARP_NOXDIS_FLAG ; nopt++ ; continue ;
     }
     if( strcasecmp(argv[nopt],"-noYdis") == 0 ){
       flags |= NWARP_NOYDIS_FLAG ; nopt++ ; continue ;
     }
     if( strcasecmp(argv[nopt],"-noZdis") == 0 ){
       flags |= NWARP_NOZDIS_FLAG ; nopt++ ; continue ;
     }

     /*---------------*/

     if( strcasecmp(argv[nopt],"-base") == 0 ){
       if( bset   != NULL ) ERROR_exit("Can't use -base twice!") ;
       if( ++nopt >= argc ) ERROR_exit("need arg after -base") ;
       bset = THD_open_dataset(argv[nopt]) ;
       if( bset == NULL ) ERROR_exit("Can't open -base '%s'",argv[nopt]) ;
       bsname = strdup(argv[nopt]) ; DSET_COPYOVER_REAL(bset) ;
       nopt++ ; continue ;
     }

     /*---------------*/

     if( strcasecmp(argv[nopt],"-source") == 0 ||
         strcasecmp(argv[nopt],"-src")    == 0   ){
       if( sset   != NULL ) ERROR_exit("Can't use %s twice!",argv[nopt]) ;
       if( ++nopt >= argc ) ERROR_exit("need arg after %s"  ,argv[nopt-1]) ;
       sset = THD_open_dataset(argv[nopt]) ;
       if( sset == NULL ) ERROR_exit("Can't open %s '%s'",argv[nopt-1],argv[nopt]) ;
       ssname = strdup(argv[nopt]) ; sstrue = sset ; DSET_COPYOVER_REAL(sset) ;
       nopt++ ; continue ;
     }

     /*---------------*/

     if( strcasecmp(argv[nopt],"-emask") == 0 ){
       THD_3dim_dataset *eset ;
       if( Hemask != NULL ) ERROR_exit("Can't use -emask twice!") ;
       if( ++nopt >= argc ) ERROR_exit("need arg after -emask") ;
       eset = THD_open_dataset(argv[nopt]) ;
       if( eset == NULL ) ERROR_exit("Can't open -emask '%s'",argv[nopt]) ;
       DSET_load(eset) ; CHECK_LOAD_ERROR(eset) ;
       Hemask = THD_makemask( eset , 0 , 1.0f , -1.0f ) ;
       if( Hemask == NULL ) ERROR_exit("Can't make -emask for some reason :-(") ;
       nevox = DSET_NVOX(eset) ;
       DSET_delete(eset) ; esname = strdup(argv[nopt]) ;
       nopt++ ; continue ;
     }

     /*---------------*/

     if( strcasecmp(argv[nopt],"-blur") == 0 ){
       float val1,val2 ;
       if( ++nopt >= argc ) ERROR_exit("need arg after -blur") ;
       if( !isnumeric(argv[nopt][0]) )
         ERROR_exit("value after '-blur' must be a number: '%s'",argv[nopt]) ;
       val2 = val1 = (float)strtod(argv[nopt],NULL) ;
       if( nopt+1 < argc && isnumeric(argv[nopt+1][0]) && !isalpha(argv[nopt+1][1]) )
         val2 = (float)strtod(argv[++nopt],NULL) ;
       Hblur_b = val1 ; Hblur_s = val2 ;
       nopt++ ; continue ;
     }

     /*---------------*/

#define DEF_PBLUR 0.09f
#define MAX_PBLUR 0.25f
     if( strcasecmp(argv[nopt],"-pblur") == 0 ){
       float val1,val2 ;
       nopt++ ;
       if( nopt >= argc || isalpha(argv[nopt][0]) ||
           (argv[nopt][0] == '-' && isalpha(argv[nopt][1])) ){  /* defaults */
         Hpblur_b = Hpblur_s = DEF_PBLUR ; continue ;
       }
       if( !isnumeric(argv[nopt][0]) )
         ERROR_exit("value after '-pblur' must be a number: '%s'",argv[nopt]) ;
       val2 = val1 = (float)strtod(argv[nopt],NULL) ;
       if( nopt+1 < argc && isnumeric(argv[nopt+1][0]) && !isalpha(argv[nopt+1][1]) )
         val2 = (float)strtod(argv[++nopt],NULL) ;
       Hpblur_b = val1 ; Hpblur_s = val2 ;
       if( val1 < 0.0f ){
         WARNING_message("base -pblur %f cannot be negative: setting it to zero",val1) ;
         Hpblur_b = 0.0f ;
       } else if( val1 > MAX_PBLUR ){
         Hpblur_b = MAX_PBLUR ;
         WARNING_message("base -pblur %f too large: altering to %f",val1,Hpblur_b) ;
       }
       if( val2 < 0.0f ){
         WARNING_message("source -pblur %f cannot be negative: setting it to zero",val2) ;
         Hpblur_s = 0.0f ;
       } else if( val2 > MAX_PBLUR ){
         Hpblur_s = MAX_PBLUR ;
         WARNING_message("source -pblur %f too large: altering to %f",val2,Hpblur_s) ;
       }
       nopt++ ; continue ;
     }

     if( strcasecmp(argv[nopt],"-nopblur") == 0 ){
       Hpblur_b = Hpblur_s = 0.0f ; nopt++ ; continue ;
     }

     /*---------------*/

     if( strcasecmp(argv[nopt],"-nopenalty") == 0 ){
       Hpen_fac = 0.0 ; nopt++ ; continue ;
     }

     /*---------------*/

     if( strcasecmp(argv[nopt],"-wball") == 0 ){
       if( ++nopt >= argc-4 )
         ERROR_exit("need 5 args after -wball") ;
       if( wmask_set != NULL )
         ERROR_exit("You can't use -wball and -wmask together") ;
       if( wball_r > 0.0f && wball_f > 0.0f )
         WARNING_message("repeated use of -wball erases earlier use") ;
       wball_x = (float)strtod(argv[nopt++],NULL) ;  /* center */
       wball_y = (float)strtod(argv[nopt++],NULL) ;
       wball_z = (float)strtod(argv[nopt++],NULL) ;
       wball_r = (float)strtod(argv[nopt++],NULL) ;  /* FWHM */
       wball_f = (float)strtod(argv[nopt++],NULL) ;  /* factor */
       if( wball_r <= 0.0f ){
         WARNING_message("-wball r=%g is illegal ==> ignoring this option") ;
         wball_r = wball_f = 0.0f ;
       } else if( wball_f < 1.0f || wball_f > 100.0f ){
         WARNING_message("-wball f=%g is illegal ==> ignoring this option") ;
         wball_r = wball_f = 0.0f ;
       }
       if( Hverb > 1 )
         INFO_message("-wball option: x=%g y=%g z=%g r=%g f=%g",
                      wball_x,wball_y,wball_z,wball_r,wball_f) ;
       continue ;
     }

     /*---------------*/

     if( strcasecmp(argv[nopt],"-wmask") == 0 ){  /* 21 Dec 2016 */
       if( ++nopt >= argc-1 )
         ERROR_exit("need 2 args after -wmask") ;
       if( wmask_set != NULL )
         ERROR_exit("You can't use -wmask twice") ;
       if( wball_r > 0.0f && wball_f > 0.0f )
         ERROR_exit("You can't use -wmask and -wball together") ;
       wmask_set = THD_open_dataset(argv[nopt]) ;    /* dataset */
       if( wmask_set == NULL )
         ERROR_exit("Can't open -wmask dataset '%s'",argv[nopt]) ;
       DSET_load(wmask_set) ; CHECK_LOAD_ERROR(wmask_set) ;
       wmask_f = (float)strtod(argv[++nopt],NULL) ;  /* factor */
       if( wmask_f < 1.0f || wmask_f > 100.0f ){
         WARNING_message("-wmask f=%g is illegal ==> ignoring this option") ;
         DSET_delete(wmask_set) ; wmask_set = NULL ; wmask_f = 0.0f ;
       }
       nopt++ ; continue ;
     }

     /*---------------*/

     if( strncasecmp(argv[nopt],"-useweight",10) == 0 ||
         strcasecmp (argv[nopt],"-use_weight"  ) == 0   ){
       auto_weight = 1 ;
       if( argv[nopt][10] == '*' && argv[nopt][11] == '*' && isnumeric(argv[nopt][12]) )
         auto_wpow = (float)strtod(argv[nopt]+12,NULL) ;
       if( Hverb && auto_wpow != 1.0f ) INFO_message("-useweight is now the default") ;
       nopt++ ; continue ;
     }

     /*---------------*/

     if( strcasecmp(argv[nopt],"-noweight") == 0 ){
       auto_weight = 2 ; nopt++ ; continue ;
     }

     /*---------------*/

     if( strcasecmp(argv[nopt],"-penfac") == 0 ){
       double val ;
       if( ++nopt >= argc ) ERROR_exit("need arg after -penfac") ;
       val = strtod(argv[nopt],NULL) ;
       if( val <= 0.0 ){
         INFO_message("-penfac turns the penalty off") ;  Hpen_fac = 0.0 ;
       } else {
         Hpen_fac = Hpen_fbase * val ;
       }
       nopt++ ; continue ;
     }

     /*---------------*/

     if( strcasecmp(argv[nopt],"-pencut") == 0 ){  /* 21 Mar 2014 [SECRET OPTION] */
       double val ;
       if( ++nopt >= argc ) ERROR_exit("need arg after -pencut") ;
       val = strtod(argv[nopt],NULL) ;
       if( val < 0.0 || val > 10.0 ){
         INFO_message("-pencut %g is illegal -- replaced with 1.0",val) ;  Hpen_cut = 1.0f ;
       } else if( val > 1.0f ){
         Hpen_cut = (float)(0.1*val) ;
       } else {
         Hpen_cut = (float)val ;
       }
       nopt++ ; continue ;
     }

     /*---------------*/

     if( strcasecmp(argv[nopt],"-penold") == 0 ){
       Hpen_old = 1 ; nopt++ ; continue ;
     }

     /*---------------*/

     if( strcasecmp(argv[nopt],"-ballopt") == 0 ){
       powell_newuoa_set_con_ball() ;
       Hopt_ball = 1 ;
       nopt++ ; continue ;
     }
     if( strcasecmp(argv[nopt],"-boxopt") == 0 ){
       powell_newuoa_set_con_box() ;
       Hopt_ball = 1 ;
       nopt++ ; continue ;
     }

     /*---------------*/

     if( strcasecmp(argv[nopt],"-prefix") == 0 ){
       if( ++nopt >= argc ) ERROR_exit("need arg after -prefix") ;
       prefix = strdup(argv[nopt]) ;
       if( !THD_filename_ok(prefix) )
         ERROR_exit("Illegal string after '-prefix'") ;
       nopt++ ; continue ;
     }

     /*---------------*/

     if( strcasecmp(argv[nopt],"-wtprefix") == 0 ){   /* 03 Jun 2016 */
       if( ++nopt >= argc ) ERROR_exit("need arg after -wtprefix") ;
       wtprefix = strdup(argv[nopt]) ;
       if( !THD_filename_ok(wtprefix) )
         ERROR_exit("Illegal string after '-wtprefix'") ;
       nopt++ ; continue ;
     }


     /*---------------*/

     if( strcasecmp(argv[nopt],"-hel") == 0 ){
       meth = GA_MATCH_HELLINGER_SCALAR ; nopt++ ; continue ;
     }

     /*---------------*/

     if( strcasecmp(argv[nopt],"-mi") == 0 ){
       meth = GA_MATCH_KULLBACK_SCALAR ; nopt++ ; continue ;
     }

     /*---------------*/

     if( strcasecmp(argv[nopt],"-nmi") == 0 ){
       meth = GA_MATCH_NORMUTIN_SCALAR ; nopt++ ; continue ;
     }

     /*---------------*/

     if( strcasecmp(argv[nopt],"-pcl") == 0 ){
       meth = GA_MATCH_PEARCLP_SCALAR ; nopt++ ; continue ;
     }

     /*---------------*/

     if( strcasecmp(argv[nopt],"-pear") == 0 ){
       meth = GA_MATCH_PEARSON_SCALAR ; nopt++ ; continue ;
     }

     /*---------------*/

     if( strcasecmp(argv[nopt],"-lpc") == 0 ){
       meth = GA_MATCH_PEARSON_LOCALS ;
       Hzeasy = meth_is_lpc = 1 ; mlev = 0 ; nopt++ ; continue ;
     }

     /*---------------*/

     if( strcasecmp(argv[nopt],"-lpa") == 0 ){
       meth = GA_MATCH_PEARSON_LOCALA ; nopt++ ; continue ;
     }

     /*---------------*/

     if( strcasecmp(argv[nopt],"-allsave") == 0 ||
         strcasecmp(argv[nopt],"-saveall") == 0       ){   /* 02 Jan 2015 */
       Hsave_allwarps = 1 ;
#ifdef USE_NEW_HSAVE
       Hsave_callback_func = save_intermediate_warp ;      /* 13 Mar 2018 */
#endif
       nopt++ ; continue ;
     }

     /*---------------*/

     if( strcasecmp(argv[nopt],"-gridlist") == 0 ){  /* 31 Dec 2014 */
       MRI_IMAGE *gim ; float *gar ; int gg , nbad ;
       if( HAVE_HGRID )     ERROR_exit("You can't use -gridlist twice :-(") ;
       if( ++nopt >= argc ) ERROR_exit("Need arg after -gridlist !!") ;
       gim = mri_read_1D(argv[nopt]) ;
       if( gim == NULL )    ERROR_exit("Can't read -gridlist file '%s'",argv[nopt]) ;
       gar = MRI_FLOAT_PTR(gim) ;
       for( nbad=gg=0 ; gg < gim->nx ; gg++ ){
         if( gar[gg] < NGMIN && gar[gg] != 0 ) nbad++ ;
       }
       if( nbad > 0 ){
         ERROR_exit("Can't use -gridlist file '%s' -- found %d error%s",
                    argv[nopt] , nbad , (nbad==1)?" ":"s" ) ;
       }
       Hgridlist_num = gim->nx ;
       Hgridlist     = (int *)malloc(sizeof(int)*gim->nx) ;
       for( nbad=gg=0 ; gg < gim->nx ; gg++ ){
         Hgridlist[gg] = (int)gar[gg] ;
         if( Hgridlist[gg] > 0 && Hgridlist[gg]%2 == 0 ){ Hgridlist[gg]++ ; nbad++ ; }
       }
       if( nbad > 0 ){
         WARNING_message("-gridlist file '%s' -- %d value%s even and %s incremented to make them odd" ,
                         argv[nopt] , nbad , (nbad==1) ? " is" : "s are" ,
                                             (nbad==1) ? "was" : "were"    ) ;
       }
       nopt++ ;
       continue ;
     }

     /*---------------*/

     /*---------- maybe we should just tell them to use SPM? ----------*/

     ERROR_message("Totally bogus option '%s'",argv[nopt]) ;
     suggest_best_prog_option(argv[0],argv[nopt]) ;
     exit(1) ;

   } /*--------------- end of loop over command line args --------------------*/

   if( argc < 3 )
     ERROR_exit("Too few options, use -help for details");

   /*----- make a 'clean' prefix (in case we got a dirty one?) -----*/

   { char *ns ;
     prefix_clean = strdup(prefix) ;
     ns = strstr(prefix_clean,".nii" ) ; if( ns != NULL ) *ns = '\0' ;
     ns = strstr(prefix_clean,"+orig") ; if( ns != NULL ) *ns = '\0' ;
     ns = strstr(prefix_clean,"+acpc") ; if( ns != NULL ) *ns = '\0' ;
     ns = strstr(prefix_clean,"+tlrc") ; if( ns != NULL ) *ns = '\0' ;
   }

   if(wtprefix!=NULL)
   { char *ns ;
     wtprefix_clean = strdup(wtprefix) ;
     ns = strstr(wtprefix_clean,".nii" ) ; if( ns != NULL ) *ns = '\0' ;
     ns = strstr(wtprefix_clean,"+orig") ; if( ns != NULL ) *ns = '\0' ;
     ns = strstr(wtprefix_clean,"+acpc") ; if( ns != NULL ) *ns = '\0' ;
     ns = strstr(wtprefix_clean,"+tlrc") ; if( ns != NULL ) *ns = '\0' ;
   }

   /*----- check for errorororors --------------------------------------------*/

STATUS("check for errors") ;

   nbad = 0 ;                 /* count of errors */
   ct   = NI_clock_time() ;   /* Oh, and start the clock timer */

   if( flags == NWARP_NODISP_FLAG ){
     ERROR_message("too many -no?dis flags ==> nothing to warp!") ; nbad++ ;
   }

   if( bset == NULL && sset == NULL && nopt+1 >= argc ){
     ERROR_message("need 2 args after all options, for base and source dataset names") ; nbad++ ;
   }

   if( (do_allin || do_resam) && xyzm_bas != NULL ){
     ERROR_message("You cannot use '-allineate' or '-resample' with '-XYZmatch'") ; nbad++ ;
   }
   if( do_allin && do_resam ){
     do_resam = 0 ;
     INFO_message("%s turns off -resample",(do_allin==1)?"-allineate":"-allinfast");
   }
   if( do_allin && do_plusminus ){
     ERROR_message("You cannot use -allineate and -plusminus together :-(") ; nbad++ ;
   }
   if( do_allin && iwname != NULL ){
     ERROR_message("You cannot use -allineate and -iniwarp together :-(") ; nbad++ ;
   }
   if( do_allin && ilev > 0 ){
     ERROR_message("You cannot use -allineate and -inilev together :-(") ; nbad++ ;
   }

#ifdef ALLOW_DUPLO
   if( iwname != NULL && duplo ){
     ERROR_message("You cannot combine -iniwarp and -duplo !! :-((") ; nbad++ ;
   }
   if( (ilev != 0 || mlev < 99 ) && duplo ){
     ERROR_message("You cannot combine -inilev or -maxlev and -duplo !! :-((") ; nbad++ ;
   }
#endif

   if( bset != NULL && sset == NULL ){
     ERROR_message("You can't use -base without -source!") ; nbad++ ;
   } else if( bset == NULL && sset != NULL ) {
     ERROR_message("You can't use -source without -base!") ; nbad++ ;
   }

#ifdef ALLOW_DUPLO
   if( HAVE_HGRID && duplo ){  /* 02 Jan 2015 */
     ERROR_message("You can't combine -gridlist with -duplo :-(") ; nbad++ ;
   }
#endif
   if( HAVE_HGRID && do_plusminus ){
     ERROR_message("You can't combine -gridlist with -plusminus :-(") ; nbad++ ;
   }
   if( Hsave_allwarps && !zeropad_warp ){
     ERROR_message("You can't combine -allsave with -nopadWARP :-(") ; nbad++ ;
   }

   /*--- any fatal errors flagged above, then it's time to go fatal ---*/

   if( nbad > 0 )
     ERROR_exit("Cannot continue after above error%s" , (nbad==1) ? "\0" : "s" ) ;

   /*--- other checks that aren't fatal, just to let the user beware ---*/

   if( !do_allin && do_awarp ){ /* 21 Dec 2016 */
     WARNING_message("-awarp given without -allineate -- turning -awarp off") ;
     do_awarp = 0 ;
   }

   if( meth_is_lpc && mlev > 0 )
     WARNING_message("Use of '-maxlev 0' is recommended with '-lpc'") ;

#ifdef ALLOW_DUPLO
   if( do_plusminus && duplo ){
     duplo = 0 ;
     WARNING_message("Alas, -plusminus does not work with -duplo -- turning -duplo off") ;
   }
#endif

   if( !do_plusminus && do_pmbase ){  /* 12 Aug 2014 */
     WARNING_message("-pmBASE without -plusminus: are you daft, mate?") ;
     do_pmbase = 0 ;
   }

   if( Hznoq && Hqonly ){
     Hznoq = 0 ;
     WARNING_message("-znoQ and -Qonly cannot be combined: turning off -znoQ") ;
   } else if( Hznoq && Hqfinal ){
     Hznoq = 0 ;
     WARNING_message("-znoQ and -Qfinal cannot be combined: turning off -znoQ") ;
   }

   if( wbim != NULL && wball_r > 0.0f && wball_f > 0.0f ){  /* May 2016 */
     WARNING_message("-weight option means -wball option is ignored :-(") ;
     wball_r = wball_f = 0.0f ;
   }
   if( wbim != NULL && wmask_set !=NULL && wmask_f > 0.0f ){ /* 21 Dec 2016 */
     WARNING_message("-weight option means -wmask option is ignored :-(") ;
     DSET_delete(wmask_set) ; wmask_set = NULL ; wmask_f = 0.0f ;
   }

   if( wbim != NULL && wtprefix != NULL ){                  /* 03 Jun 2016 */
     WARNING_message("-weight option means -wtprefix option is ignored :-(") ;
     wtprefix = wtprefix_clean = NULL ;
   }

#ifdef ALLOW_BASIS5
   if( Hqfinal && H5final ){
     WARNING_message("-Qfinal and -5final conflict: using -5final") ;
     Hqfinal = 0 ;
   }
   if( H5final==3 && minpatch < NGMIN_PLUS_3 ){
     WARNING_message("-5final resets -minpatch to %d",NGMIN_PLUS_3) ;
     minpatch = NGMIN_PLUS_3 ; mlev = 99 ;
   }
   if( H5final==2 && minpatch < NGMIN_PLUS_2 ){
     WARNING_message("-4final resets -minpatch to %d",NGMIN_PLUS_2) ;
     minpatch = NGMIN_PLUS_2 ; mlev = 99 ;
   }
   if( H5final==1 && minpatch < NGMIN_PLUS_1 ){
     WARNING_message("-3final resets -minpatch to %d",NGMIN_PLUS_1) ;
     minpatch = NGMIN_PLUS_1 ; mlev = 99 ;
   }
   if( H5final==3 && minpatch > NGMAX_PLUS_3 ){
     WARNING_message("-minpatch %d is too big for -5final [max=%d]: using -Qfinal instead",
                     minpatch, NGMAX_PLUS_3 ) ;
     H5final = 0 ; Hqfinal = 1 ;
   }
   if( H5final==2 && minpatch > NGMAX_PLUS_2 ){
     WARNING_message("-minpatch %d is too big for -4final [max=%d]: using -Qfinal instead",
                     minpatch, NGMAX_PLUS_2 ) ;
     H5final = 0 ; Hqfinal = 1 ;
   }
   if( H5final==1 && minpatch > NGMAX_PLUS_1 ){
     WARNING_message("-minpatch %d is too big for -3final [max=%d]: using -Qfinal instead",
                     minpatch, NGMAX_PLUS_1 ) ;
     H5final = 0 ; Hqfinal = 1 ;
   }
#endif

   /*----------- get the input datasets, check them for errors -----------*/

STATUS("read inputs") ;

   /* base dataset (if not already read) */

   if( bset == NULL ){
     bset = THD_open_dataset(argv[nopt++]) ; DSET_COPYOVER_REAL(bset) ;
     if( bset == NULL ) ERROR_exit("Can't open base dataset '%s'",argv[nopt-1]) ;
     bsname = strdup(argv[nopt-1]) ;
STATUS("base dataset opened") ;
   }
   if( DSET_NVALS(bset) > 1 )
     INFO_message("base dataset has more than 1 sub-brick: ignoring all but the first") ;

   /* source dataset (if not already read) */

   if( sset == NULL ){
     sset = THD_open_dataset(argv[nopt++]) ; DSET_COPYOVER_REAL(sset) ;
     if( sset == NULL ) ERROR_exit("Can't open source dataset '%s'",argv[nopt-1]) ;
     ssname = strdup(argv[nopt-1]) ; sstrue = sset ;
STATUS("source dataset opened") ;
   }
   if( DSET_NVALS(sset) > 1 )
     INFO_message("source dataset has more than 1 sub-brick: ignoring all but the first") ;

   if( do_resam && EQUIV_GRIDXYZ(bset,sset) ){
     INFO_message("-resample is not needed (datasets on same 3D grid) -- turning it off") ;
     do_resam = 0 ;
   }

   if( wmask_set != NULL && !EQUIV_GRIDS_NXYZ(wmask_set,bset) ) /* 21 Dec 2016 */
     ERROR_exit("-wmask dataset and -base dataset grids don't match :(") ;

   /*---- Set up -XYZmatch stuff, if present [15 Aug 2014] ----*/

   if( xyzm_bas != NULL ){
     int nx , ngood ; float *pbas , *psrc ;
     nx = xyzm_bas->nx ;
     pbas = MRI_FLOAT_PTR(xyzm_bas) ; psrc = MRI_FLOAT_PTR(xyzm_src) ;
     ngood = IW3D_xyzmatch_internalize( bset , nx ,
                                        pbas , pbas+nx , pbas+2*nx ,
                                        psrc , psrc+nx , psrc+2*nx  ) ;
     if( ngood < 1 )
       ERROR_exit("No good points from option '-XYZmatch' ???") ;
     else if( ngood < nx )
       WARNING_message("Ignoring %d (out of %d) points in '-XYZmatch', for being outside",
                       nx-ngood , nx ) ;
   }

   /*---- Run 3dAllineate first, replace source dataset [15 Jul 2013] --------*/

   if( !do_allin && allopt != NULL ){
     WARNING_message("-allineate_opts is ignored: no -allineate option was given!") ;
     free(allopt) ; allopt = NULL ;
   }

   if( do_allin || do_resam ){   /* here is where 3dAllineate is invoked */
     char *qs , *ns , *rs ; /* temp stuff */

STATUS("3dAllineate coming up next") ;

     if( do_allin && noneg ){
       if( allopt != NULL ) allopt = (char *)realloc(allopt,strlen(allopt)+32);
       else                 allopt = (char *)calloc(32,1) ;
       strcat(allopt," -zclip") ;
     }
     if( do_allin == 2 ){  /* the 'fast' way */
       if( allopt != NULL ) allopt = (char *)realloc(allopt,strlen(allopt)+64);
       else                 allopt = (char *)calloc(64,1) ;
       strcat(allopt," -onepass -conv 0.2") ;
     }

     DSET_unload(sstrue) ;                /* de-allocate orig source dataset */

     /*--- run 3dAllineate now now now ---*/

     if( do_allin )
       Qallineate( bsname , ssname , esname , allopt ) ;  /* run 3dAllineate */
     else /* do_resam */
       Qallin_resample( bsname , ssname ) ;           /* just for resampling */

     fprintf(stderr,"\n") ;

     /*-- try to load the results: dataset first (in NIfTI format) --*/

     qs = (char *)malloc(strlen(Qunstr)+strlen(prefix)+64) ;
     ns = (char *)malloc(strlen(Qunstr)+strlen(prefix)+64) ;
     sprintf(qs,"%s.nii",Qunstr) ;                        /* dataset filename */
     if( !THD_is_file(qs) ){     /* check for compressed output [07 Feb 2014] */
       strcpy(ns,qs) ; strcat(ns,".gz") ;
       if( !THD_is_file(ns) )
         ERROR_message("Can't find 3dAllineate output '%s' or '%s' :-(",qs,ns) ;
       else
         strcpy(qs,ns) ;
     }
     if( keep_allin ){    /* if keeping 3dAllineate output, make a nicer name */
       sprintf(ns,"%s_Allin.nii",prefix_clean) ;
       if( STRING_HAS_SUFFIX(qs,".gz") ) strcat(ns,".gz") ;    /* 07 Feb 2014 */
       rename(qs,ns) ; rs = ns ;
     } else {
       rs = qs ;
     }
     INFO_message("3dQwarp: replacing source dataset with 3dAllineate result %s",rs) ;
     sset = THD_open_dataset(rs) ;                 /* get its output dataset */
     if( sset == NULL ) ERROR_exit("Can't open replacement source dataset %s :-(",rs) ;
     DSET_load(sset); CHECK_LOAD_ERROR(sset); DSET_lock(sset); DSET_COPYOVER_REAL(sset);
     if( !keep_allin ) remove(qs) ;  /* erase the 3dAllineate dataset from disk */

     /*-- load alignment matrix, to be used after warping --*/

     if( do_allin ){
       MRI_IMAGE *zim ; float *qar ;
       sprintf(qs,"%s.aff12.1D",Qunstr) ;
       zim = mri_read_1D(qs) ;           /* get the 3dAllineate output matrix */
       if( zim == NULL )
         ERROR_exit("Can't open 3dAllineate's matrix file '%s'",qs) ;
       if( zim->nvox < 12 )
         ERROR_exit("3dAllineate's .aff12.1D file has incorrect format??") ;
       qar = MRI_FLOAT_PTR(zim) ;
       LOAD_MAT44(allin_matrix,qar[0],qar[1],qar[ 2],qar[ 3],  /* load matrix */
                               qar[4],qar[5],qar[ 6],qar[ 7],
                               qar[8],qar[9],qar[10],qar[11] ) ;
         /* save the magnitude of the shifts (needed for zero pad guesstimate */
       dxal = fabsf(qar[3]); dyal = fabsf(qar[7]); dzal = fabsf(qar[11]); have_dxyzal = 1;
       if( !keep_allin ){
         remove(qs) ;           /* erase the 3dAllineate matrix file from disk */
         if( Hverb ) ININFO_message("3dAllineate output files have been deleted");
       } else {
         sprintf(ns,"%s_Allin.aff12.1D",prefix_clean) ; rename(qs,ns) ;
         if( Hverb ) ININFO_message("3dAllineate output files have been renamed") ;
       }
       if( Hverb && do_allin ) DUMP_MAT44("3dAllineate matrix",allin_matrix) ;
       mri_free(zim) ;
     }

     free(qs) ; free(ns) ;

   } /*--- end of 3dAllineate prolegomenon -----------------------------------*/

STATUS("check dataset for stupid errors") ; /*--------------------------------*/

   if( !EQUIV_GRIDXYZ(bset,sset) )
     ERROR_exit("base-source dataset grid mismatch :-( : try the -resample option") ;

   if(  EQUIV_DSETS  (bset,sset) )
     ERROR_exit("base & source datasets are identical :-( : are you trying something sneaky?");

   /*--------------- construct the initial warp dataset, if any
                     [altered somewhat: 15 Jul 2013]            --------------*/

   if( iwname != NULL ){
     int ijkpad[12] , qq ; THD_3dim_dataset *qsar[2] ;

STATUS("construct initial warp") ;

     /* take care of case when just a .1D file is input here */

     if( strstr(iwname,".1D") != NULL && strchr(iwname,' ') == NULL ){
       char *qstr = (char *)malloc(strlen(iwname)+strlen(bsname)+64) ;
       sprintf(qstr,"IDENT(%s) %s",bsname,iwname) ;
       free(iwname) ; iwname = qstr ;
     }

     iwset = IW3D_read_catenated_warp(iwname) ;    /* fancy way to read warps */
     if( iwset == NULL )
       ERROR_exit("Cannot open -iniwarp %s",iwname) ;
     if( DSET_NVALS(iwset) < 3 || DSET_BRICK_TYPE(iwset,0) != MRI_float )
       ERROR_exit("-iniwarp %s is not in the right format :-(",argv[nopt]) ;

     /* check for previous -allin matrix [16 Mar 2018] */

     { ATR_float *afl = THD_find_float_atr( iwset->dblk , "QWARP_ALLIN_MATRIX" ) ;
       if( afl != NULL && afl->nfl > 11 ){
         float *qar = afl->fl ;
         dxal = fabsf(qar[3]); dyal = fabsf(qar[7]); dzal = fabsf(qar[11]); have_dxyzal = 1;
       }
     }

     /* compute how much bset must be padded to fit the iniwarp [11 Apr 2014] */

     qsar[0] = iwset ; qsar[1] = bset ;
     qq = THD_conformist( 2 , qsar , CONFORM_NOREFIT , ijkpad ) ;
     if( qq < 0 )
       ERROR_exit("-iniwarp grid does not conform with base dataset grid") ;
     if( ijkpad[0] > 0 || ijkpad[1] > 0 || ijkpad[2] > 0 ||
         ijkpad[3] > 0 || ijkpad[4] > 0 || ijkpad[5] > 0   )
       ERROR_exit("-iniwarp grid conforms to but does not contain base dataset grid") ;

     iwpad_xm=ijkpad[6]; iwpad_xp=ijkpad[ 7]; iwpad_ym=ijkpad[ 8];
     iwpad_yp=ijkpad[9]; iwpad_zm=ijkpad[10]; iwpad_zp=ijkpad[11];
     if( !zeropad &&
         (iwpad_xm > 0 || iwpad_xp > 0 || iwpad_ym > 0 ||
          iwpad_yp > 0 || iwpad_zm > 0 || iwpad_zp > 0   ) )
       ERROR_exit("-iniwarp grid is bigger than base dataset grid AND you used -nopad") ;

     if( Hverb > 1 &&
         (iwpad_xm > 0 || iwpad_xp > 0 || iwpad_ym > 0 ||
          iwpad_yp > 0 || iwpad_zm > 0 || iwpad_zp > 0   ) )
       INFO_message("-iniwarp requires dataset to be padded at least %d %d  %d %d  %d %d voxels",
                    iwpad_xm, iwpad_xp, iwpad_ym, iwpad_yp, iwpad_zm, iwpad_zp ) ;

   } /* at this point, iwset = initial warp dataset */

STATUS("load datasets") ; /*--------------------------------------------------*/

   DSET_load(bset) ; CHECK_LOAD_ERROR(bset) ;
   bim = THD_extract_float_brick(0,bset) ; DSET_unload(bset) ;
   if( DSET_NVALS(bset) > 1 )
     INFO_message("base dataset has more than 1 sub-brick: ignoring all but the first") ;

   nnz = mri_nonzero_count(bim) ;  /* 13 Mar 2017 */
   if( nnz < 100 )
     ERROR_exit("3dQwarp fails :: base image has %d nonzero voxel%s (< 100)",
                nnz , (nnz==1) ? "\0" : "s" ) ;

   DSET_load(sset) ; CHECK_LOAD_ERROR(sset) ;
   sim = THD_extract_float_brick(0,sset) ;
   DSET_unlock(sset) ; DSET_unload(sset) ;

   nnz = mri_nonzero_count(sim) ;  /* 13 Mar 2017 */
   if( nnz < 100 )
     ERROR_exit("3dQwarp fails :: source image has %d nonzero voxel%s (< 100)",
                nnz , (nnz==1) ? "\0" : "s" ) ;

   if( nevox > 0 && nevox != DSET_NVOX(bset) )
     ERROR_exit("-emask doesn't match base dataset grid :-(") ;

   /*---------------- deal with negative values [24 May 2013] ----------------*/

   bmin = mri_min(bim) ;
   if( bmin < 0.0f && noneg ){
     float *bar = MRI_FLOAT_PTR(bim) ; int ii , nneg=0 ;
     for( ii=0 ; ii < bim->nvox ; ii++ ){ if( bar[ii] < 0.0f ){ bar[ii] = 0.0; nneg++; } }
     INFO_message("-noneg converted %d base voxels to 0",nneg) ; bmin = 0.0f ;
   }

   smin = mri_min(sim) ;
   if( smin < 0.0f && noneg ){
     float *sar = MRI_FLOAT_PTR(sim) ; int ii , nneg=0 ;
     for( ii=0 ; ii < sim->nvox ; ii++ ){ if( sar[ii] < 0.0f ){ sar[ii] = 0.0; nneg++; } }
     INFO_message("-noneg converted %d source voxels to 0",nneg) ; smin = 0.0f ;
   }

   if( (bmin < 0.0f || smin < 0.0f) && meth == GA_MATCH_PEARCLP_SCALAR ){
     meth = GA_MATCH_PEARSON_SCALAR ;
     INFO_message("negative values in %s ==> using strict Pearson correlation",
                     (bmin < 0.0f && smin < 0.0f) ? "base and source"
                   : (bmin < 0.0f)                ? "base"            : "source" ) ;
   }

   /*----------- dimensions of the universe (as it is now, anyhoo) -----------*/

   nxold = nx = DSET_NX(bset); nyold = ny = DSET_NY(bset); nzold = nz = DSET_NZ(bset);
   dx = fabsf(DSET_DX(bset)) ; dy = fabsf(DSET_DY(bset)) ; dz = fabsf(DSET_DZ(bset)) ;

   /*------ Do we need to zeropad datasets? [Friday the 13th, Sep 2013] ------*/
   /*(((((( That is, do we expand the universe with dark matter/energy? ))))))*/

   if( expad > 0 || minpad > 0 ) zeropad = 1 ;

   if( zeropad ){                /* adapted/stolen/liberated from 3dAllineate */
     float cv , *qar  ; MRI_IMAGE *qim ; int mpad_min=9 ;
     int bpad_xm,bpad_xp, bpad_ym,bpad_yp, bpad_zm,bpad_zp ;
     int spad_xm,spad_xp, spad_ym,spad_yp, spad_zm,spad_zp ;
     int mpad_x , mpad_y , mpad_z , ii ;

     cv = 0.33f * THD_cliplevel(bim,0.22f) ;        /* set threshold on base, */
     qim = mri_copy(bim); qar = MRI_FLOAT_PTR(qim); /* then pad on bounding box */
     for( ii=0 ; ii < qim->nvox ; ii++ ) if( qar[ii] < cv ) qar[ii] = 0.0f ;
     MRI_autobbox( qim, &bpad_xm,&bpad_xp, &bpad_ym,&bpad_yp, &bpad_zm,&bpad_zp ) ;
     mri_free(qim) ;
     if( Hverb > 1 )
       INFO_message("Zero-pad: base dataset autobox = %d..%d  %d..%d  %d..%d",
                    bpad_xm,bpad_xp , bpad_ym,bpad_yp , bpad_zm,bpad_zp ) ;

     /* the code to use the source to define the zero pad is removed,
        so that the zero padding is the same whenever the same base is used */

#if 0  /* use the source to define zero pad amount */
     cv = 0.33f * THD_cliplevel(sim,0.22f) ;       /* set threshold on source */
     qim = mri_copy(sim); qar = MRI_FLOAT_PTR(qim);
     for( ii=0 ; ii < qim->nvox ; ii++ ) if( qar[ii] < cv ) qar[ii] = 0.0f ;
     MRI_autobbox( qim, &spad_xm,&spad_xp, &spad_ym,&spad_yp, &spad_zm,&spad_zp ) ;
     mri_free(qim) ;
     if( Hverb > 1 )
       ININFO_message("Zero-pad: source dataset autobox = %d..%d  %d..%d  %d..%d",
                      bpad_xm,bpad_xp , bpad_ym,bpad_yp , bpad_zm,bpad_zp ) ;
#else  /* do NOT use the source to define zero pad amount */
     spad_xm = bpad_xm ; spad_ym = bpad_ym ; spad_zm = bpad_zm ;
     spad_xp = bpad_xp ; spad_yp = bpad_yp ; spad_zp = bpad_zp ;
#endif

     /* these variables are the box in which the data is contained */

     pad_xm = MIN(bpad_xm,spad_xm) ; pad_xp = MAX(bpad_xp,spad_xp) ;
     pad_ym = MIN(bpad_ym,spad_ym) ; pad_yp = MAX(bpad_yp,spad_yp) ;
     pad_zm = MIN(bpad_zm,spad_zm) ; pad_zp = MAX(bpad_zp,spad_zp) ;

     if( have_dxyzal ){                           /* extend pad size for */
       float dm = MIN(dx,dy) ; dm = MIN(dm,dz) ;  /* 3dAllineate shifts? */
       dxal /= dm ; dyal /= dm ; dzal /= dm ;
       dm = MAX(dxal,dyal); dm = MAX(dm,dzal); mpad_min += (int)rintf(1.0111f*dm);
       /* INFO_message("dxyzal => mpad_min = %d",mpad_min) ; */
     }

     /* define minimum padding for each direction */

     mpad_x = (int)rintf(0.1111f*bim->nx) ; mpad_x = MAX(mpad_x,mpad_min) ;
     mpad_y = (int)rintf(0.1111f*bim->ny) ; mpad_y = MAX(mpad_y,mpad_min) ;
     mpad_z = (int)rintf(0.1111f*bim->nz) ; mpad_z = MAX(mpad_z,mpad_min) ;

     /* compute padding so at least mpad_Q all-zero slices on each Q-face
        will be present after the padding is done, for Q = x or y or z   */

     pad_xm = mpad_x - pad_xm               ; if( pad_xm < 0 ) pad_xm = 0 ;
     pad_ym = mpad_y - pad_ym               ; if( pad_ym < 0 ) pad_ym = 0 ;
     pad_zm = mpad_z - pad_zm               ; if( pad_zm < 0 ) pad_zm = 0 ;
     pad_xp = mpad_x - (bim->nx-1 - pad_xp) ; if( pad_xp < 0 ) pad_xp = 0 ;
     pad_yp = mpad_y - (bim->ny-1 - pad_yp) ; if( pad_yp < 0 ) pad_yp = 0 ;
     pad_zp = mpad_z - (bim->nz-1 - pad_zp) ; if( pad_zp < 0 ) pad_zp = 0 ;

     if( Hverb > 1 &&
         (pad_xm > 0 || pad_xp > 0 || pad_ym > 0 ||
          pad_yp > 0 || pad_zm > 0 || pad_zp > 0   ) )
       ININFO_message("dataset padding needs at least %d %d  %d %d  %d %d voxels",
                      pad_xm, pad_xp, pad_ym, pad_yp, pad_zm, pad_zp ) ;

     if( pad_xm < iwpad_xm ) pad_xm = iwpad_xm ;  /* make sure padding */
     if( pad_xp < iwpad_xp ) pad_xp = iwpad_xp ;  /* is at least what */
     if( pad_ym < iwpad_ym ) pad_ym = iwpad_ym ;  /* is needed to fit */
     if( pad_yp < iwpad_yp ) pad_yp = iwpad_yp ;  /* the initial warp */
     if( pad_zm < iwpad_zm ) pad_zm = iwpad_zm ;
     if( pad_zp < iwpad_zp ) pad_zp = iwpad_zp ;

     if( pad_xm < minpad   ) pad_xm = minpad ;  /* minimum padding allowed? */
     if( pad_xp < minpad   ) pad_xp = minpad ;  /* (SECRET OPTION) */
     if( pad_ym < minpad   ) pad_ym = minpad ;
     if( pad_yp < minpad   ) pad_yp = minpad ;
     if( pad_zm < minpad   ) pad_zm = minpad ;
     if( pad_zp < minpad   ) pad_zp = minpad ;

     if( expad > 0 ){                             /* extra padding   */
       pad_xm += expad ; pad_xp += expad ;        /* ordered by the  */
       pad_ym += expad ; pad_yp += expad ;        /* cautious user   */
       pad_zm += expad ; pad_zp += expad ;
     }

     if( bim->nz == 1 ){     /* but no z-padding for 2D image! */
       pad_zm = pad_zp = 0 ;
       if( iwpad_zm > 0 || iwpad_zp > 0 )
         ERROR_exit("-iniwarp required padding in 3D but base dataset is 2D ?!?");
     }

     /* flag to mark if any zero padding is done */

     zeropad = (pad_xm > 0 || pad_xp > 0 ||
                pad_ym > 0 || pad_yp > 0 || pad_zm > 0 || pad_zp > 0) ;

     if( zeropad ){  /*----- print a report and actually do it -----*/

       int do_warn , wx,wy,wz ;  /* excess padding? [11 Jan 2018] */
       wx = bim->nx/2; wy = bim->ny/2; wz = bim->nz/2;
       do_warn = (pad_xm > wx) || (pad_xp > wx) ||
                 (pad_ym > wy) || (pad_yp > wy) ||
                 ( wz > 1 && ((pad_zm >= wz) || (pad_zp >= wz)) ) ;

       if( Hverb || do_warn )
         INFO_message("Dataset zero-pad:"
                      " xbot=%d xtop=%d  ybot=%d ytop=%d  zbot=%d ztop=%d voxels",
                       pad_xm,pad_xp , pad_ym,pad_yp , pad_zm,pad_zp ) ;
       if( do_warn ){
         WARNING_message(
           "At least one padding is more than 50%% of dataset grid size!" ) ;
         WARNING_message(
           "  Computation time might be tremendously long :(") ;
         WARNING_message(
           "  Preliminary alignment of dataset centers might help a LOT.") ;
       }

       /*-- replace base image --*/

       nxold=bim->nx ; nyold=bim->ny ; nzold=bim->nz ;  /* orginal dimensions */

       qim = mri_zeropad_3D( pad_xm,pad_xp , pad_ym,pad_yp ,
                                             pad_zm,pad_zp , bim ) ;
       mri_free(bim) ; bim = qim ;

       /*-- replace source image --*/

       qim = mri_zeropad_3D( pad_xm,pad_xp , pad_ym,pad_yp ,
                                             pad_zm,pad_zp , sim ) ;
       mri_free(sim) ; sim = qim ;

       /*-- also zeropad emask --*/

       if( Hemask != NULL ){
         byte *ezp = (byte *)EDIT_volpad( pad_xm,pad_xp ,
                                          pad_ym,pad_yp ,
                                          pad_zm,pad_zp ,
                                          nxold,nyold,nzold ,
                                          MRI_byte , Hemask ) ;
         if( ezp == NULL ) ERROR_exit("zeropad of emask fails !?!") ;
         free(Hemask) ; Hemask = ezp ; nevox = bim->nvox ;
       }
     }

     nx = bim->nx; ny = bim->ny; nz = bim->nz; /* altered dimensions of universe */

   } /*--------- end of zeropad for base, source, and emask inputs ---------*/

#if 0
   if( !zeropad ) zeropad_warp = 0 ;
#endif

   /*------- setup initial warp, if any (e.g., make it match bset) -------*/
   /*((((((( Global variable S2BIM_iwarp contains the initial warp )))))))*/

   if( iwset != NULL ){

     int nxiw,nyiw,nziw , mm ;
     DSET_load(iwset) ; CHECK_LOAD_ERROR(iwset) ;

     /* Compute how much iwset must be padded to fit bset [11 Apr 2014] */
     /* Remember, at this point, bset might be zeropadded to be bigger. */
     /* By construction above, iwpad_?? <= pad_?? for all 6 ?? values,  */
     /* so we need to pad the initial warp by pad_?? - iwpad_??         */

     iwpad_xm = pad_xm - iwpad_xm ; iwpad_xp = pad_xp - iwpad_xp ;
     iwpad_ym = pad_ym - iwpad_ym ; iwpad_yp = pad_yp - iwpad_yp ;
     iwpad_zm = pad_zm - iwpad_zm ; iwpad_zp = pad_zp - iwpad_zp ;

     /* convert input dataset to index warp */

     S2BIM_iwarp = IW3D_from_dataset(iwset,0,0) ;
     if( S2BIM_iwarp == NULL )
       ERROR_exit("Cannot create 3D warp from -iniwarp dataset :-(") ;
     DSET_delete(iwset) ; iwset = NULL ;

     /* pad (actually, extend in size by linear extrapolation), if needed */

     if( iwpad_xm > 0 || iwpad_xp > 0 || iwpad_ym > 0 ||
         iwpad_yp > 0 || iwpad_zm > 0 || iwpad_zp > 0   ){

       IndexWarp3D *QQ = IW3D_extend(S2BIM_iwarp, iwpad_xm,iwpad_xp,
                                                  iwpad_ym,iwpad_yp, iwpad_zm,iwpad_zp , 0 ) ;
       IW3D_destroy(S2BIM_iwarp) ; S2BIM_iwarp = QQ ;
       if( Hverb )
         ININFO_message("Extended/padded iniwarp to match base volume: %d %d  %d %d  %d %d voxels",
                        iwpad_xm,iwpad_xp, iwpad_ym,iwpad_yp, iwpad_zm,iwpad_zp ) ;

     }
#if 0
     if( Hverb > 1 )
       INFO_message("-iniwarp energy = %g",IW3D_load_energy(S2BIM_iwarp)) ;
#endif

   } else {  /*------- no initial warp (that was easy) -----------------------*/

     S2BIM_iwarp = NULL ;

   }

   /*----------------------- other initial setup stuff -----------------------*/

   S2BIM_ilev = ilev ;            /* initial level */
   S2BIM_mlev = MAX(mlev,ilev) ;  /* maximum level */

   nnn = 0 ;
   if( nx >= NGMIN             ) nnn = nx ;  /* find largest dimension */
   if( ny >= NGMIN && ny > nnn ) nnn = ny ;  /* bigger than min grid  */
   if( nz >= NGMIN && nz > nnn ) nnn = nz ;  /* allowed for warping  */
   if( nnn == 0 )
     ERROR_exit("dataset grid size %d x %d x %d is too small for warping",nx,ny,nz) ;

#if 0
   if( minpatch > 0 && minpatch > nnn/2 && minpatch > NGMIN ){
     WARNING_message("-minpatch %d is too large for dataset grid %d x %d x %d",
                     minpatch , nx,ny,nz ) ;
     minpatch = nnn/2 ;
     if( minpatch%2 == 0     ) minpatch-- ;
     if( minpatch   <  NGMIN ) minpatch = NGMIN ;
     WARNING_message("minpatch is reduced to %d",minpatch) ;
   }
#endif

   if( minpatch > 0 ) Hngmin = minpatch ;  /* minimum patch size */

#ifdef ALLOW_DUPLO
   if( duplo && (nx < 3*Hngmin || ny < 3*Hngmin || nz < 3*Hngmin) ){
     duplo = 0 ;
       INFO_message("-duplo disabled since dataset is small: %d x %d x %d",nx,ny,nz) ;
     ININFO_message(" smallest size allowed for -duplo is    %d x %d x %d",3*Hngmin,3*Hngmin,3*Hngmin) ;
     ININFO_message(" ['small' is relative to the minimum patch size you set = %d]",Hngmin) ;
   }
#endif

   /*-------------------- create weight volume -------------------------------*/

STATUS("construct weight/mask volume") ;

   if( wbim == NULL ){  /* construct from base image */

     wbim = mri_weightize(bim,auto_weight,auto_dilation,auto_wclip,auto_wpow) ;

     if( wball_r > 0.0f && wball_f > 0.0f ){  /* wball stuff [May 2016] */
       THD_3dim_dataset *qset ;
       float xx,yy,zz, xd,yd,zd, sig,rqq,fff, *wbar=MRI_FLOAT_PTR(wbim) ;
       int ixx,iyy,izz , rxx,ryy,rzz , ii,jj,kk , nwb=0 ;
       if( zeropad )
         qset = THD_zeropad( bset ,
                            pad_xm,pad_xp , pad_ym,pad_yp , pad_zm,pad_zp ,
                            "BSET_zeropadded" , ZPAD_IJK | ZPAD_EMPTY ) ;
       else
         qset = bset ;

       MAT44_VEC(qset->daxes->dicom_to_ijk,
                 wball_x,wball_y,wball_z , xx,yy,zz ) ;
       ixx = (int)rintf(xx) ; rxx = (int)rintf(2.0f*wball_r/dx) ;
       iyy = (int)rintf(yy) ; ryy = (int)rintf(2.0f*wball_r/dy) ;
       izz = (int)rintf(zz) ; rzz = (int)rintf(2.0f*wball_r/dz) ;
       if( nz == 1 ){ izz = 0 ; rzz = 0.0f ; }

       sig = FWHM_TO_SIGMA(wball_r) ; sig = 0.5/(sig*sig) ;
       rqq = wball_r * wball_r ;

       if( Hverb > 1 )
         INFO_message("-wball: center i,j,k=%d,%d,%d  ri,rj,rk=%d,%d,%d",
                      ixx,iyy,izz , rxx,ryy,rzz ) ;

       for( kk=izz-rzz ; kk <= izz+rzz ; kk++ ){
        if( kk < 0 || kk >= nz ) continue ;
        zd = dz*(kk-izz) ; zd *= zd ;
        for( jj=iyy-ryy ; jj <= iyy+ryy ; jj++ ){
         if( jj < 0 || jj >= ny ) continue ;
         yd = dy*(jj-iyy) ; yd *= yd ;
         for( ii=ixx-rxx ; ii <= ixx+rxx ; ii++ ){
          if( ii < 0 || ii >= nx ) continue ;
          xd = dx*(ii-ixx) ; xd = xd*xd + yd +zd ;
          if( xd <= rqq ){
            fff = wball_f * expf( -xd*sig ) + 1.0f ;
            wbar[ii+jj*nx+kk*nx*ny] *= fff ;
            if( fff >= 1.333f ) nwb++ ;
          }
       }}}

       /* a wball message for the user? */

       if( nwb == 0 )
         WARNING_message(
           "-wball did not change any weights significantly. Check parameters.");
       else if( nwb < 100 )
         WARNING_message(
           "-wball significantly affected the weight in only %d voxel%s",
           nwb , (nwb>1) ? "s" : "\0" ) ;
       else if( Hverb > 0 )
         INFO_message("-wball significantly affected the weight in %d voxels",nwb);

       if( qset != bset ){ DSET_delete(qset); qset=NULL; }  /* trash */

     } /* end of wball-ification */

     else if( wmask_set != NULL && wmask_f > 0.0f ){  /* 21 Dec 2016 */

       THD_3dim_dataset *qset ; MRI_IMAGE *qim ;
       float fff, *wbar=MRI_FLOAT_PTR(wbim) , *qar ;
       int ii , nxyz=nx*ny*nz , nwb=0 ;
       if( zeropad ){
         qset = THD_zeropad( wmask_set ,
                             pad_xm,pad_xp , pad_ym,pad_yp , pad_zm,pad_zp ,
                            "wmask_zeropadded" , ZPAD_IJK ) ;
         DSET_delete(wmask_set) ;
       } else {
         qset = wmask_set ;
       }

       qim = THD_extract_float_brick(0,qset) ; qar = MRI_FLOAT_PTR(qim) ;
       DSET_delete(qset) ; qset=NULL ;

       fff = 1.0f + wmask_f ;
       for( ii=0 ; ii < nxyz ; ii++ ){
         if( qar[ii] != 0.0f && wbar[ii] > 0.0f ){ wbar[ii] *= fff ; nwb++ ; }
       }
       mri_free(qim) ;

       if( nwb == 0 )
         WARNING_message(
           "-wmask did not change any weights!");
       else if( nwb < 100 )
         WARNING_message(
           "-wmask affected the weight in only %d voxel%s",
           nwb , (nwb>1) ? "s" : "\0" ) ;
       else if( Hverb > 0 )
         INFO_message("-wmask affected the weight in %d voxels",nwb);

     } /* end of wmask-ification */

   } else {             /* just use -weight input image */

     if( zeropad ){
       MRI_IMAGE *qim ;
       qim = mri_zeropad_3D( pad_xm,pad_xp , pad_ym,pad_yp ,
                                             pad_zm,pad_zp , wbim ) ;
       mri_free(wbim) ; wbim = qim ;
     }
     if( wbim->nx != nx || wbim->ny != ny || wbim->nz != nz )
       ERROR_exit("-weight image doesn't match -base image grid") ;

   }

   /*--- write weight image to dataset? [03 Jun 2016] ---*/

   if( wtprefix != NULL ){
     MRI_IMAGE *qim ; THD_3dim_dataset *qset ;
     qim = mri_zeropad_3D( -pad_xm,-pad_xp,
                           -pad_ym,-pad_yp, -pad_zm,-pad_zp, wbim ) ;
     qset = EDIT_empty_copy(bset) ;
     tross_Copy_History( bset , qset ) ;
     tross_Make_History( "3dQwarp" , argc,argv , qset ) ;
     EDIT_dset_items( qset ,
                        ADN_prefix    , wtprefix ,
                        ADN_nvals     , 1 ,
                        ADN_ntt       , 0 ,
                        ADN_datum_all , MRI_float ,
                      ADN_none ) ;
     EDIT_BRICK_FACTOR(qset,0,0.0) ;
     EDIT_substitute_brick( qset, 0, MRI_float, MRI_FLOAT_PTR(qim) ) ;
     DSET_write(qset) ; WROTE_DSET(qset) ; DSET_delete(qset) ; qset=NULL ;
   }

   /*--- scale weight volume so max value is 1 (and is all non-negative) ---*/

   { float fac , *wt ; int ii ;
     fac = (float)mri_max(wbim) ;
     if( fac <= 0.0f ) ERROR_exit("weight volume is not positive?!") ;
     fac = 1.0f / fac ; wt = MRI_FLOAT_PTR(wbim) ;
     for( ii=0 ; ii < wbim->nvox ; ii++ )
       wt[ii] = ( wt[ii] <= 0.0f ) ? 0.0f : fac * wt[ii] ;
   }

   nnz = mri_nonzero_count(wbim) ;  /* 13 Mar 2017 */
   if( nnz < 100 )
     ERROR_exit("3dQwarp fails :: weight image has %d nonzero voxel%s (< 100)",
                nnz , (nnz==1) ? "\0" : "s" ) ;

   /*----- blurring of base is now done in warpomatic, along with source -----*/
#if 0
   /*----- blur base here if so ordered (source is blurred in warpomatic) ----*/

   if( Hblur_b >= 0.5f && !do_plusminus ){
     MRI_IMAGE *qim ;
     if( Hverb > 1 ) ININFO_message("   blurring base image %.3g voxels FWHM",Hblur_b) ;
     qim = mri_float_blur3D( FWHM_TO_SIGMA(Hblur_b) , bim ) ;
     mri_free(bim) ; bim = qim ;
   } else if( Hblur_b <= -1.0f && !do_plusminus ){
     MRI_IMAGE *qim ;
     if( Hverb > 1 ) ININFO_message("   median-izing base image %.3g voxels",-Hblur_b) ;
     qim = mri_medianfilter( bim , -Hblur_b , NULL , 0 ) ;
     mri_free(bim) ; bim = qim ;
   }
#endif

   /* create a dataset to be the warp's "adoption parent" */

   if( !zeropad_warp )
     adset = bset ;
   else
     adset = THD_zeropad( bset ,
                          pad_xm,pad_xp , pad_ym,pad_yp , pad_zm,pad_zp ,
                          "BSET_zeropadded" , ZPAD_IJK | ZPAD_EMPTY ) ;

   if( do_allin ){ /* make warp adjustment matrix [13 Mar 2018] */
     mat44 tmat,smat,qmat , cmat,imat ;
     cmat = adset->daxes->ijk_to_dicom ;
     imat = MAT44_INV(cmat) ;
     qmat = allin_matrix ;                 /* convert matrix to */
     tmat = MAT44_MUL(qmat,cmat) ;         /* index space from  */
     smat = MAT44_MUL(imat,tmat) ;         /* coordinate space  */
     allin_adjust_matrix = smat ;
   }

   /*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*/
   /*------------------------- do some actual work! --------------------------*/

   IW3D_setup_signal_quit() ; /* QUIT signal => graceful death [25 Sep 2014] */

   if( Hverb )
     INFO_message("+++++++++++ Begin warp optimization:  base=%s  source=%s" ,
                  DSET_HEADNAME(bset) , DSET_HEADNAME(sset) ) ;

   if( do_plusminus ){   /*--- special case of plusminus warp ----------------*/

#ifndef ALLOW_PLUSMINUS
     ERROR_exit("-plusminus: This message should never appear!") ;
#else
     sbww = IW3D_warp_s2bim_plusminus( bim,wbim,sim, MRI_WSINC5, meth, flags ) ;
     oiw  = sbww[0] ;  /* plus warp and image */
     qiw  = sbww[1] ;  /* minus warp and image */

     if( do_pmbase ){  /* 12 Aug 2014: warp source all the way back to base */
       IndexWarp3D *qwinv ;
       if( Hverb ) fprintf(stderr,"Computing -pmBASE outputs ") ;
       qwinv = IW3D_invert( qiw->warp , NULL , MRI_WSINC5 ) ;
       if( Hverb ) fprintf(stderr,"W") ;
       pmbase_warp = IW3D_compose( oiw->warp , qwinv , MRI_WSINC5 ) ;
       if( Hverb ) fprintf(stderr,"I") ;
       pmbase_imag = IW3D_warp_floatim( pmbase_warp, sim, MRI_WSINC5, 1.0f ) ;
       IW3D_destroy(qwinv) ;
       if( Hverb ) fprintf(stderr,"\n") ;
     }
#endif

   } else {              /*--- the standard case -----------------------------*/

     qiw = NULL ;
#ifdef ALLOW_DUPLO
     if( duplo )
       oiw = IW3D_warp_s2bim_duplo( bim,wbim,sim, MRI_WSINC5, meth, flags ) ;
     else
#endif
       oiw = IW3D_warp_s2bim( bim,wbim,sim, MRI_WSINC5, meth, flags ) ;

   }

   /*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*/
   /**----------------------- check for errorosities ------------------------**/

   if( oiw == NULL ) ERROR_exit("s2bim fails") ;

   INFO_message("========== total number of parameters 'optimized' = %d",Hnpar_sum) ;

   mri_free(wbim) ; wbim = NULL ;  /* not needed after here [17 Oct 2013] */
   mri_free(bim)  ; bim  = NULL ;

   /*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*/
   /*--------- Post-processing to get the datasets to write to disk ----------*/

   /*---------- un-zeropad the output stuff, if needed and desired -----------*/

   if( zeropad ){
     MRI_IMAGE *qim ; IndexWarp3D *QQ ;

     /* crop the output image (maybe) */
     if( oiw->im->nx > nxold || oiw->im->ny > nyold || oiw->im->nz > nzold ){
       if( Hverb > 1 )
         INFO_message("un-zero-padding output volume back to original base grid");
       qim = mri_zeropad_3D( -pad_xm,-pad_xp,
                             -pad_ym,-pad_yp, -pad_zm,-pad_zp, oiw->im ) ;
       mri_free(oiw->im) ; oiw->im = qim ;
     }

     /* crop the output warp (maybe) */
     if( !zeropad_warp ){
       if( Hverb > 1 )
         ININFO_message("un-zero-padding warp back to original base grid") ;
       QQ = IW3D_extend( oiw->warp, -pad_xm,-pad_xp,
                                    -pad_ym,-pad_yp, -pad_zm,-pad_zp , 0 ) ;
       IW3D_destroy(oiw->warp) ; oiw->warp = QQ ;
     }

     /* same stuff for the plusminus warp results as well */
     if( qiw != NULL ){
       if( qiw->im->nx > nxold || qiw->im->ny > nyold || qiw->im->nz > nzold ){
         qim = mri_zeropad_3D( -pad_xm,-pad_xp,
                               -pad_ym,-pad_yp, -pad_zm,-pad_zp, qiw->im ) ;
         mri_free(qiw->im) ; qiw->im = qim ;
       }
       if( !zeropad_warp ){
         QQ = IW3D_extend( qiw->warp, -pad_xm,-pad_xp,
                                      -pad_ym,-pad_yp, -pad_zm,-pad_zp , 0 ) ;
         IW3D_destroy(qiw->warp) ; qiw->warp = QQ ;
       }
     }

     if( pmbase_imag != NULL ){  /* 12 Aug 2014 */
       if( pmbase_imag->nx > nxold || pmbase_imag->ny > nyold || pmbase_imag->nz > nzold ){
         qim = mri_zeropad_3D( -pad_xm,-pad_xp,
                               -pad_ym,-pad_yp, -pad_zm,-pad_zp, pmbase_imag ) ;
         mri_free(pmbase_imag) ; pmbase_imag = qim ;
       }
       if( pmbase_warp != NULL && !zeropad_warp ){
         QQ = IW3D_extend( pmbase_warp, -pad_xm,-pad_xp,
                                        -pad_ym,-pad_yp, -pad_zm,-pad_zp , 0 ) ;
         IW3D_destroy(pmbase_warp) ; pmbase_warp = QQ ;
       }
     }

   }  /*---------- end of patching up for zeropad ----------*/

   /*--- make the warps adopt a dataset to specify their extrinsic geometry --*/

   oim = oiw->im ; oww = oiw->warp ;

                             IW3D_adopt_dataset( oww        , adset ) ;
   if( qiw         != NULL ) IW3D_adopt_dataset( qiw->warp  , adset ) ;
   if( pmbase_warp != NULL ) IW3D_adopt_dataset( pmbase_warp, adset ) ;

   /*-------------------------------------------------------------------------*/
   /*-- Special case of pre-3dAllineate: adjust output warp and image (oiw) --*/

   if( do_awarp ){  /* but first save a copy of the 'pure' warp [21 Dec 2016] */
     char *qprefix ; IndexWarp3D *awarp ;
     awarp = IW3D_copy (oww  ,1.0f ) ;
     IW3D_adopt_dataset(awarp,adset) ;
STATUS("output awarp") ;
     qprefix = modify_afni_prefix(prefix,NULL,"_AWARP") ;
     qset = IW3D_to_dataset( awarp , qprefix ) ;
     tross_Copy_History( bset , qset ) ;
     tross_Make_History( "3dQwarp" , argc,argv , qset ) ;
     MCW_strncpy( qset->atlas_space , bset->atlas_space , THD_MAX_NAME ) ;
     DSET_write(qset) ; WROTE_DSET(qset) ; DSET_delete(qset) ; qset=NULL ;
   }

   if( do_allin || do_resam ){

     /* adjust warp for 3dAllineate matrix that came earlier */
     /* (don't have to adjust plusminus warp since can't be run with -allin) */

     if( do_allin ){
       IndexWarp3D *tarp ; int ii ;
STATUS("adjust for 3dAllineate matrix") ;
       tarp = IW3D_compose_w1m2(oiw->warp,allin_adjust_matrix,MRI_WSINC5) ;  /* adjust warp */
       IW3D_destroy(oiw->warp) ; oww = oiw->warp = tarp ;
       IW3D_adopt_dataset(oww,adset) ;

       /** also adjust all intermediate saved warps [02 Jan 2015] **/
       for( ii=0 ; ii < Hsave_num ; ii++ ){
         tarp = IW3D_compose_w1m2(Hsave_iwarp[ii],allin_adjust_matrix,MRI_WSINC5) ;
         IW3D_destroy(Hsave_iwarp[ii]) ; Hsave_iwarp[ii] = tarp ;
       }
     }

     /** directly warp from original source dataset to output image **/
     /** (then replace existing oiw->im with this re-warped image)  **/

     if( !nodset ){
       THD_3dim_dataset *wset, *iset ; MRI_IMAGE *iim ;
       wset = IW3D_to_dataset( oiw->warp , "ZharkTheGlorious" ) ;
       iset = THD_nwarp_dataset( wset , sstrue , bset , "WhoTheHellCares" ,
                                 MRI_WSINC5,MRI_WSINC5, 0.0f,1.0f, 1, NULL ) ;
       if( iset == NULL )  /* should be impossible */
         ERROR_exit("Can't warp from original dataset for some reason :-(") ;
       iim = THD_extract_float_brick(0,iset) ;
       mri_free(oiw->im) ; oim = oiw->im = iim ; DSET_delete(iset) ; DSET_delete(wset) ;

       if( noneg && mri_min(oiw->im) < 0.0f ){   /* clipping */
         float *oar = MRI_FLOAT_PTR(oiw->im) ; int ii ;
         for( ii=0 ; ii < oiw->im->nvox ; ii++ ){
           if( oar[ii] < 0.0f ) oar[ii] = 0.0 ;
         }
       }
     }

   } /*--------- end of patchup for input from 3dAllineate ---------*/

#if 0
   if( Hverb > 1 )
     INFO_message("output warp energy = %g",IW3D_load_energy(oww)) ;
#endif

   /*-------------------------------------------------------------------------*/
   /*------------ finally, output some results to pacify the user ------------*/

   if( !nodset ){                    /*----- output warped dataset -----*/
     char *qprefix = prefix ;
STATUS("output warped dataset") ;

     if( do_plusminus ){
       sprintf(appendage,"_%s",plusname) ;
       qprefix = modify_afni_prefix(prefix,NULL,appendage) ;
     }
     oset = EDIT_empty_copy(bset) ;
     tross_Copy_History( bset , oset ) ;
     tross_Make_History( "3dQwarp" , argc,argv , oset ) ;
     EDIT_dset_items( oset ,
                        ADN_prefix    , qprefix ,
                        ADN_nvals     , 1 ,
                        ADN_ntt       , 0 ,
                        ADN_datum_all , MRI_float ,
                      ADN_none ) ;
     EDIT_BRICK_FACTOR(oset,0,0.0) ;
     EDIT_substitute_brick( oset, 0, MRI_float, MRI_FLOAT_PTR(oim) ) ;
     DSET_write(oset) ; WROTE_DSET(oset) ; DSET_delete(oset) ;

     if( do_plusminus && qiw != NULL ){
       sprintf(appendage,"_%s",minusname) ;
       qprefix = modify_afni_prefix(prefix,NULL,appendage) ;
       oset = EDIT_empty_copy(bset) ;
       tross_Copy_History( bset , oset ) ;
       tross_Make_History( "3dQwarp" , argc,argv , oset ) ;
       EDIT_dset_items( oset ,
                          ADN_prefix    , qprefix ,
                          ADN_nvals     , 1 ,
                          ADN_ntt       , 0 ,
                          ADN_datum_all , MRI_float ,
                        ADN_none ) ;
       EDIT_BRICK_FACTOR(oset,0,0.0) ;
       EDIT_substitute_brick( oset, 0, MRI_float, MRI_FLOAT_PTR(qiw->im) ) ;
       DSET_write(oset) ; WROTE_DSET(oset) ; DSET_delete(oset) ;
     }

     if( pmbase_imag != NULL ){        /* 12 Aug 2014 */
       oset = EDIT_empty_copy(bset) ;
       tross_Copy_History( bset , oset ) ;
       tross_Make_History( "3dQwarp" , argc,argv , oset ) ;
       EDIT_dset_items( oset ,
                          ADN_prefix    , prefix ,
                          ADN_nvals     , 1 ,
                          ADN_ntt       , 0 ,
                          ADN_datum_all , MRI_float ,
                        ADN_none ) ;
       EDIT_BRICK_FACTOR(oset,0,0.0) ;
       EDIT_substitute_brick( oset, 0, MRI_float, MRI_FLOAT_PTR(pmbase_imag) ) ;
       DSET_write(oset) ; WROTE_DSET(oset) ; DSET_delete(oset) ;
     }

   } /* end of writing warped datasets */

   if( !nowarp ){                 /*----- output the warp itself -----*/
     char *qprefix ;
STATUS("output warp") ;
     if( do_plusminus){
       sprintf(appendage,"_%s_WARP",plusname) ;
       qprefix = modify_afni_prefix(prefix,NULL,appendage) ;
     } else {
       qprefix = modify_afni_prefix(prefix,NULL,"_WARP") ;
     }
     qset = IW3D_to_dataset( oww , qprefix ) ;
#if 0
INFO_message("warp dataset origin: %g %g %g",DSET_XORG(qset),DSET_YORG(qset),DSET_ZORG(qset)) ;
#endif
     tross_Copy_History( bset , qset ) ;
     tross_Make_History( "3dQwarp" , argc,argv , qset ) ;
     MCW_strncpy( qset->atlas_space , bset->atlas_space , THD_MAX_NAME ) ;
     if( do_allin ){  /* save copy of 3dAllineate matrix into header */
       float qar[12] ;
       UNLOAD_MAT44(allin_matrix,qar[0],qar[1],qar[ 2],qar[ 3],
                                 qar[4],qar[5],qar[ 6],qar[ 7],
                                 qar[8],qar[9],qar[10],qar[11] ) ;
       THD_set_float_atr( qset->dblk , "QWARP_ALLIN_MATRIX" , 12 , qar ) ;
     }
     DSET_write(qset) ; WROTE_DSET(qset) ; DSET_delete(qset) ; qset=NULL ;

     if( do_plusminus && qiw != NULL ){
       sprintf(appendage,"_%s_WARP",minusname) ;
       qprefix = modify_afni_prefix(prefix,NULL,appendage) ;
       qset = IW3D_to_dataset( qiw->warp , qprefix ) ;
       tross_Copy_History( bset , qset ) ;
       tross_Make_History( "3dQwarp" , argc,argv , qset ) ;
       MCW_strncpy( qset->atlas_space , bset->atlas_space , THD_MAX_NAME ) ;
       DSET_write(qset) ; WROTE_DSET(qset) ; DSET_delete(qset) ; qset=NULL ;
     }

     if( pmbase_warp != NULL ){   /* 12 Aug 2014 */
       qprefix = modify_afni_prefix(prefix,NULL,"_WARP") ;
       qset = IW3D_to_dataset( pmbase_warp , qprefix ) ;
       tross_Copy_History( bset , qset ) ;
       tross_Make_History( "3dQwarp" , argc,argv , qset ) ;
       MCW_strncpy( qset->atlas_space , bset->atlas_space , THD_MAX_NAME ) ;
       DSET_write(qset) ; WROTE_DSET(qset) ; DSET_delete(qset) ; qset=NULL ;
     }

   } /* end of output of warp dataset */

   if( !nowarpi && !do_plusminus ){      /*----- output the inverse warp -----*/
     if( Hverb ) fprintf(stderr,"++ Inverting warp ") ;
     owwi = IW3D_invert( oiw->warp , NULL , MRI_WSINC5 ) ;
     if( Hverb ) fprintf(stderr,"\n") ;
     IW3D_adopt_dataset( owwi , adset ) ;
     qset = IW3D_to_dataset( owwi, modify_afni_prefix(prefix,NULL,"_WARPINV")) ;
     tross_Copy_History( bset , qset ) ;
     tross_Make_History( "3dQwarp" , argc,argv , qset ) ;
     MCW_strncpy( qset->atlas_space , bset->atlas_space , THD_MAX_NAME ) ;
     DSET_write(qset) ; WROTE_DSET(qset) ; DSET_delete(qset) ; qset=NULL ;
     IW3D_destroy(owwi) ; owwi = NULL ;
   }

   /*-- output intermediate saved warps [02 Jan 2015] --*/

   if( Hsave_allwarps && Hsave_num > 0 ){
     char suffix[64] , *qprefix ; int ii ;
     for( ii=0 ; ii < Hsave_num ; ii++ ){
       IW3D_adopt_dataset(Hsave_iwarp[ii],adset) ;
       sprintf(suffix,"_%s_WARP",Hsave_iname[ii]) ;
       qprefix = modify_afni_prefix(prefix,NULL,suffix) ;
       qset = IW3D_to_dataset( Hsave_iwarp[ii] , qprefix ) ;
       tross_Copy_History( bset , qset ) ;
       tross_Make_History( "3dQwarp" , argc,argv , qset ) ;
       MCW_strncpy( qset->atlas_space , bset->atlas_space , THD_MAX_NAME ) ;
       DSET_write(qset) ; WROTE_DSET(qset) ; DSET_delete(qset) ; qset=NULL ;
     }
     HSAVE_DESTROY ;
   }

   /*------------- go back to watching Matlock reruns ------------------------*/

STATUS("watching Matlock reruns") ;

   cput = COX_cpu_time() ;
   if( cput > 0.05 )
     INFO_message("===== CPU time = %.1f sec  clock time =%s",
                  cput , nice_time_string(NI_clock_time()-ct) ) ;
   else
     INFO_message("===== clock time =%s" , nice_time_string(NI_clock_time()-ct) ) ;

   exit(0) ;
}
