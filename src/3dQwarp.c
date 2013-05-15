#include "mrilib.h"

/*---------------------------------------------------------------------------*/
/* Features to ruminate about:
    - NN interpolation of data, for matching label datasets      [not hard]
    - allow user-input weight volume                             [not hard]
    - symmetric mapping,                                         [hard]
      with Src(W(x)) = Bas(INV(W(x))) instead of Src(W(x))=B(x)
    - plusminus mapping                                          [done]
      with Src(x-w(x)) = Bas(x+w(x)) instead of Src(x+w(x))=B(x)
*//*-------------------------------------------------------------------------*/

#ifdef USE_OMP       /* OpenMP */
# include <omp.h>
#endif

/** included here to be compiled with OpenMP, unlike the versions in libmri **/
#include "mri_genalign.c"
#include "mri_genalign_util.c"

/** include the warping functions, and enable the warp-optimizing functions **/

#define ALLOW_QWARP
#define ALLOW_PLUSMINUS
#undef  USE_PLUSMINUS_INITIALWARP   /* don't do this */
#include "mri_nwarp.c"

/** constants for the mri_weightize() function (liberated from 3dAllineate) **/

static int auto_weight    = 2 ;
static float auto_wclip   = 0.0f ;
static float auto_wpow    = 1.0f ;
static int auto_dilation  = 5 ;
static float wt_medsmooth = 2.25f ;
static float wt_gausmooth = 4.50f ;

/*---------------------------------------------------------------------------*/
/*! Turn an input image into a weighting factor (for -autoweight).
    If acod == 2, then make a binary mask at the end.
    If acod == 3, then make a boxed binary mask at the end.
-----------------------------------------------------------------------------*/

MRI_IMAGE * mri_weightize( MRI_IMAGE *im, int acod, int ndil, float aclip, float apow )
{
   float *wf,clip,clip2 ;
   int xfade,yfade,zfade , nx,ny,nz,nxy,nxyz , ii,jj,kk,ff ;
   byte *mmm ;
   MRI_IMAGE *qim , *wim ;

   /*-- copy input image --*/

   qim = mri_to_float(im) ; wf = MRI_FLOAT_PTR(qim) ;
   nx = qim->nx; ny = qim->ny; nz = qim->nz; nxy = nx*ny; nxyz = nxy*nz;
   for( ii=0 ; ii < nxyz ; ii++ ) wf[ii] = fabsf(wf[ii]) ;

   /*-- zero out along the edges --*/
#undef  WW
#define WW(i,j,k) wf[(i)+(j)*nx+(k)*nxy]

   xfade = (int)(0.05*qim->nx+3.0) ;                 /* number of points */
   yfade = (int)(0.05*qim->ny+3.0) ;                 /* along each face */
   zfade = (int)(0.05*qim->nz+3.0) ;                 /* to set to zero */
   if( 5*xfade >= qim->nx ) xfade = (qim->nx-1)/5 ;
   if( 5*yfade >= qim->ny ) yfade = (qim->ny-1)/5 ;
   if( 5*zfade >= qim->nz ) zfade = (qim->nz-1)/5 ;
   if( Hverb > 1 )
     ININFO_message("Weightize: xfade=%d yfade=%d zfade=%d",xfade,yfade,zfade);
   for( jj=0 ; jj < ny ; jj++ )
    for( ii=0 ; ii < nx ; ii++ )
     for( ff=0 ; ff < zfade ; ff++ ) WW(ii,jj,ff) = WW(ii,jj,nz-1-ff) = 0.0f;
   for( kk=0 ; kk < nz ; kk++ )
    for( jj=0 ; jj < ny ; jj++ )
     for( ff=0 ; ff < xfade ; ff++ ) WW(ff,jj,kk) = WW(nx-1-ff,jj,kk) = 0.0f;
   for( kk=0 ; kk < nz ; kk++ )
    for( ii=0 ; ii < nx ; ii++ )
     for( ff=0 ; ff < yfade ; ff++ ) WW(ii,ff,kk) = WW(ii,ny-1-ff,kk) = 0.0f;

   if( aclip > 0.0f ){  /* 31 Jul 2007 */
     int nleft , nclip ;
     for( nclip=nleft=ii=0 ; ii < nxyz ; ii++ ){
       if( wf[ii] > 0.0f ){
         if( wf[ii] < aclip ){ nclip++; wf[ii] = 0.0f; } else nleft++ ;
       }
     }
     if( Hverb > 1 ) ININFO_message("Weightize: user clip=%g #clipped=%d #left=%d",
                                   aclip,nclip,nleft) ;
   }

   /*-- squash super-large values down to reasonability --*/

   clip = 3.0f * THD_cliplevel(qim,0.5f) ;
   if( Hverb > 1 ) ININFO_message("Weightize: (unblurred) top clip=%g",clip) ;
   for( ii=0 ; ii < nxyz ; ii++ ) if( wf[ii] > clip ) wf[ii] = clip ;

   /*-- blur a little: median then Gaussian;
          the idea is that the median filter smashes localized spikes,
          then the Gaussian filter does a litte extra general smoothing. --*/

   mmm = (byte *)malloc( sizeof(byte)*nxyz ) ;
   for( ii=0 ; ii < nxyz ; ii++ ) mmm[ii] = (wf[ii] > 0.0f) ; /* mask */
   if( wt_medsmooth > 0.0f ){
     wim = mri_medianfilter( qim , wt_medsmooth , mmm , 0 ) ; mri_free(qim) ;
   } else {
     wim = qim ;
   }
   wf = MRI_FLOAT_PTR(wim) ;
   if( wt_gausmooth > 0.0f )
     FIR_blur_volume_3d( wim->nx , wim->ny , wim->nz ,
                         1.0f , 1.0f , 1.0f ,  wf ,
                         wt_gausmooth , wt_gausmooth , wt_gausmooth ) ;

   /*-- clip off small values, and
        keep only the largest cluster of supra threshold voxels --*/

   clip  = 0.05f * mri_max(wim) ;
   clip2 = 0.33f * THD_cliplevel(wim,0.33f) ;
   clip  = MAX(clip,clip2) ;
   if( Hverb > 1 ) ININFO_message("Weightize: (blurred) bot clip=%g",clip) ;
   for( ii=0 ; ii < nxyz ; ii++ ) mmm[ii] = (wf[ii] >= clip) ;
   THD_mask_clust( nx,ny,nz, mmm ) ;
   THD_mask_erode( nx,ny,nz, mmm, 1 ) ;  /* cf. thd_automask.c */
   THD_mask_clust( nx,ny,nz, mmm ) ;
   for( ii=0 ; ii < nxyz ; ii++ ) if( !mmm[ii] ) wf[ii] = 0.0f ;
   free((void *)mmm) ;

   /*-- convert to 0..1 range [10 Sep 2007] --*/

   clip = 0.0f ;
   for( ii=0 ; ii < nxyz ; ii++ ) if( wf[ii] > clip ) clip = wf[ii] ;
   if( clip == 0.0f )
     ERROR_exit("Can't compute autoweight: max value seen as 0") ;
   clip = 1.0f / clip ;
   for( ii=0 ; ii < nxyz ; ii++ ) wf[ii] *= clip ;

   /*-- power? --*/

   if( apow > 0.0f && apow != 1.0f ){
     if( Hverb > 1 ) ININFO_message("Weightize: raising to %g power",apow) ;
     for( ii=0 ; ii < nxyz ; ii++ )
       if( wf[ii] > 0.0f ) wf[ii] = powf( wf[ii] , apow ) ;
   }

   /*-- binarize (acod==2)?  boxize (acod==3)? --*/

#undef  BPAD
#define BPAD 4
   if( acod == 2 || acod == 3 ){  /* binary weight: mask=2 or maskbox=3 */
     if( Hverb > 1 ) ININFO_message("Weightize: binarizing") ;
     for( ii=0 ; ii < nxyz ; ii++ ) if( wf[ii] != 0.0f ) wf[ii] = 1.0f ;
     if( ndil > 0 ){  /* 01 Mar 2007: dilation */
       byte *mmm = (byte *)malloc(sizeof(byte)*nxyz) ;
       if( Hverb > 1 ) ININFO_message("Weightize: dilating") ;
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
         ININFO_message("Weightize: box=%d..%d X %d..%d X %d..%d = %d voxels",
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

#undef  USE_SAVER
#ifdef  USE_SAVER
void Qsaver(char *lab, MRI_IMAGE *im)
{
   static int first=1 ;

   if( im == NULL || qset == NULL ) return ;

   if( first ){
     EDIT_substitute_brick( qset, 0, MRI_float,  MRI_FLOAT_PTR(im) ) ; first = 0 ;
   } else {
     EDIT_add_brick( qset, MRI_float, 0.0f, MRI_FLOAT_PTR(im) ) ;
   }
   if( lab != NULL && *lab != '\0' )
     EDIT_BRICK_LABEL(qset,DSET_NVALS(qset)-1,lab) ;

   mri_clear_data_pointer(im) ; return ;
}
#endif

/*---------------------------------------------------------------------------*/

void Qhelp(void)
{
  printf("\n") ;
  printf("Usage: 3dQwarp [OPTIONS] base_dataset source_dataset\n") ;
  printf(
    "\n"
    "* Computes a nonlinearly warped version of source_dataset to match base_dataset.\n"
    " ++ The detail allowed in the warping is set by the '-minpatch' option.\n"
    " ++ The discrete warp computed herein is a representation of an underlying\n"
    "    piecewise polynomial C1 diffeomorphism.\n"
    "\n"
    "* Input datasets must be on the same 3D grid!\n"
    " ++ If necessary, you can use 3dAllineate or 3dresample to make a\n"
    "    resampled version of one dataset to match the other's 3D grid.\n"
    "\n"
    "* Input datasets should be reasonably well aligned already\n"
    "  (e.g., as from an affine warping via 3dAllineate).\n"
    " ++ The standard result from 3dAllineate will resample the affinely\n"
    "    aligned dataset to the same 3D grid as the -base dataset, so this\n"
    "    new dataset will be ready to run in 3dQwarp against the same base.\n"
    "\n"
    "* Outputs of 3dQwarp are the warped dataset and the warp that did it.\n"
    " ++ These datasets are stored in float format, no matter what the\n"
    "    data type of the source dataset.\n"
    "\n"
    "* Matching by default is the 'clipped Pearson' method, and\n"
    "  can be changed to 'pure Pearson' with the '-pear' option.\n"
    " ++ The purpose of 'clipping' is to reduce the impact of outlier values\n"
    "    (small or large) on the correlation.\n"
    " ++ For the adventurous, you can also try these matching functions:\n"
    "      '-hel' for Hellinger distance\n"
    "      '-mi'  for Mutual Information\n"
    "      '-nmi' for Normalized Mutual Information\n"
    "    These options have not been extensively tested for usefulness.\n"
#if defined(USE_OMP) && defined(__GNU_C__)
    " ++ Note that these 3 'adventurous' options may cause trouble with\n"
    "    OpenMP compiled with GNU gcc, due to a bug in gcc's OpenMP library\n"
    "    -- and this binary is compiled that way!\n"
    "    -- this issue is one reason these options are labeled 'adventurous'.\n"
#endif
    " ++ At this time, the 'local Pearson' statistics have not been\n"
    "    implemented in this program :-(\n"
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
    "\n"
    "* If for some deranged reason you have datasets with very non-cubical voxels,\n"
    "  they should be resampled to a cubical grid before trying 3dQwarp.  For example,\n"
    "  if you have acquired 1x1x4 mm T1-weighted structural volumes (why?), then\n"
    "  resample them to 1x1x1 mm before doing any other registration processing.\n"
    "  For example:\n"
    "    3dAllineate -input anatT1_crude+orig -newgrid 1.0 \\\n"
    "                -prefix anatT1_fine -final wsinc5     \\\n"
    "                -1Dparam_apply '1D: 12@0'\\'\n"
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
    "    3dQwarp -prefix anatT1_USAQ -duplo -useweight -blur 0 3 \\\n"
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
    "-------\n"
    "OPTIONS\n"
    "-------\n"
    " -base   base_dataset   = Alternative way to specify the base dataset.\n"
    " -source source_dataset = Alternative way to specify the source dataset.\n"
    "                         * You can either use both '-base' and '-source',\n"
    "                           OR you can put the base and source dataset\n"
    "                           names last on the command line.\n"
    "\n"
    " -prefix ppp  = Sets the prefix for the output datasets.\n"
    "               * The source dataset is warped to match the base\n"
    "                 and gets prefix 'ppp'.\n"
    "               * The 3D warp used is saved in a dataset with\n"
    "                 prefix 'ppp_WARP' -- this dataset can be used\n"
    "                 with 3dNwarpApply and 3dNwarpCat, for example.\n"
    "                 * To be clear, this is the warp from source dataset\n"
    "                   coordinates to base dataset coordinates, where the\n"
    "                   values at each base grid point are the xyz displacments\n"
    "                   needed to move that grid point's xyz values to the\n"
    "                   corresponding xyz values in the source dataset:\n"
    "                     base( (x,y,z) + WARP(x,y,z) ) matches source(x,y,z)\n"
    "                   Another way to think of this warp is that it 'pulls'\n"
    "                   values back from source space to base space.\n"
    "               * 3dNwarpApply would use 'ppp_WARP' to transform datasets\n"
    "                 aligned with the source dataset to be aligned with the\n"
    "                 base dataset.\n"
    "              ** If you do NOT want this warp saved, use the option '-nowarp'.\n"
    "            -->> (However, this warp is the most valuable possible output!)\n"
    "               * If you want to calculate and save the inverse 3D warp,\n"
    "                 use the option '-iwarp'.  This inverse warp will then be\n"
    "                 saved in a dataset with prefix 'ppp_WARPINV'.\n"
    "               * This inverse warp could be used to transform data from base\n"
    "                 space to source space, if you need to do such an operation.\n"
    "               * You can easily compute the inverse later, say by a command like\n"
    "                   3dNwarpCat -prefix Z_WARPINV 'INV(Z_WARP+tlrc)'\n"
    "\n"
    " -nowarp      = Do not save the _WARP file.\n"
    " -iwarp       = Do compute and save the _WARPINV file.\n"
    " -nodset      = Do not save the warped source dataset (i.e., if you only need the _WARP).\n"
    "\n"
    " -pear        = Use strict Pearson correlation for matching.\n"
    "               * Not usually recommended, since the 'clipped Pearson' method\n"
    "                 used by default will reduce the impact of outlier values.\n"
    "\n"
    " -nopenalty   = Don't use a penalty on the cost function; the goal\n"
    "                of the penalty is to reduce grid distortions.\n"
    " -penfac ff   = Use the number 'ff' to weight the penalty.\n"
    "                The default value is 1.  Larger values of 'ff' mean the\n"
    "                penalty counts more, reducing grid distortions,\n"
    "                insha'Allah. '-nopenalty' is the same as '-penfac 0'.\n"
    " -useweight   = Normally, each voxel in the automask of the base dataset\n"
    "                counts the same.  With '-useweight', each voxel is weighted\n"
    "                by the intensity of the (blurred) base image.  This makes\n"
    "                white matter count more in T1-weighted volumes, for example.\n"
    "           -->>* This option is generally recommended.\n"
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
    "                 rather than Gaussian blurring.  This type of filtering will\n"
    "                 better preserve edges, which can be important in alignment.\n"
    "               * If the base is a template volume that is already blurry,\n"
    "                 you probably don't want to blur it again, but blurring\n"
    "                 the source volume a little is probably a good idea, to\n"
    "                 help the program avoid trying to match tiny features.\n"
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
    "               * Applications: exclude a tumor or resected region\n"
    "                 (e.g., draw a mask in the AFNI Drawing plugin).\n"
    "               * Note that the emask applies to the base dataset,\n"
    "                 so if you are registering a pre- and post-surgery\n"
    "                 volume, you would probably use the post-surgery\n"
    "                 dataset as the base.  If you eventually want the\n"
    "                 result back in the pre-surgery space, then you\n"
    "                 would use the inverse warp afterwards.\n"
    "\n"
    " -noXdis      = These options let you specify that the warp should not\n"
    " -noYdis      = displace in the given direction.  For example, combining\n"
    " -noZdis      = -noXdis and -noZdis would mean only warping along the\n"
    "                y-direction would be allowed.\n"
    "               * xyz coordinates herein refer to the DICOM order, where\n"
    "                   -x = Right  -y = Anterior   -z = Inferior\n"
    "                   +x = Left   +y = Posterior  +z = Superior\n"
    "\n"
    " -iniwarp ww  = 'ww' is a dataset with an initial nonlinear warp to use.\n"
    "               * If this option is not used, the initial warp is the identity.\n"
    "               * You CANNOT use this option with -duplo !!\n"
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
    "                   or blanks.  The closing ')' isn't really required, but\n"
    "                   the opening '(' after 'MATRIX' or 'PARAM' is needed.\n"
    "                   For this reason, you will probably need to put this\n"
    "                   argument inside 'single' or \"double\" quotes, to protect\n"
    "                   it from interpretation by the Unix shell.\n"
#endif
    "\n"
    " -inilev lv   = 'lv' is the initial refinement 'level' at which to start.\n"
    "               * Usually used with -iniwarp; CANNOT be used with -duplo.\n"
    "               * The combination of -inilev and -iniwarp lets you take the\n"
    "                 results of a previous 3dQwarp run and refine them further:\n"
    "                   3dQwarp -prefix Q25 -source SS+tlrc -base TEMPLATE+tlrc \\\n"
    "                           -duplo -minpatch 25 -useweight -blur 0 3\n"
    "                   3dQwarp -prefix Q11 -source SS+tlrc -base TEMPLATE+tlrc \\\n"
    "                           -inilev 7 -iniwarp Q25_WARP+tlrc -useweight -blur 0 2\n"
    "                 Note that the source dataset in the second run is the SAME as\n"
    "                 in the first run.  If you don't see why this is necessary,\n"
    "                 then you probably need to seek help from an AFNI guru.\n"
    "               * Also see the script @toMNI_Qwarpar for the use of this option\n"
    "                 in creating a template dataset from a collection of scans from\n"
    "                 different subjects.\n"
    "\n"
    " -minpatch mm = Set the minimum patch size for warp searching to 'mm' voxels.\n"
    "   *OR*        * The value of mm should be an odd integer.\n"
    " -patchmin mm  * The default value of mm is 25.\n"
    "               * For more accurate results than mm=25, try 19 or 13.\n"
    "               * The smallest allowed value is 9 (which will be VERY slow).\n"
#ifdef USE_SAVER
    "               * If you want to see the warped results at various levels\n"
    "                 of patch size, use the '-qsave' option.\n"
#endif
    "\n"
    " -maxlev lv   = Here, 'lv' is the maximum refinement 'level' to use.  This\n"
    "                is an alternate way to specify when the program should stop.\n"
    "               * To only do global polynomial warping, use '-maxlev 0'.\n"
    "               * If you use both '-minpatch' and '-maxlev', then you are\n"
    "                 living on the edge of danger.\n"
    "\n"
    " -duplo       = Start off with 1/2 scale versions of the volumes,\n"
    "                for getting a speedy coarse first alignment.\n"
    "               * Then scales back up to register the full volumes.\n"
    "                 The goal is greater speed, and it seems to help this\n"
    "                 somewhat piggish program to be more expeditious.\n"
    "\n"
    " -workhard    = Iterate more times, which can help when the volumes are\n"
    "                hard to align at all, or when you hope to get a more precise\n"
    "                alignment.\n"
    "               * Slows the program down (possibly a lot), of course.\n"
    "               * When you combine '-workhard'  with '-duplo', only the\n"
    "                 full size volumes get the extra iterations.\n"
    "               * For finer control over which refinement levels work hard,\n"
    "                 you can use this option in the form (for example)\n"
    "                     -workhard:4:7\n"
    "                 which implies the extra iterations will be done at levels\n"
    "                 4, 5, 6, and 7, but not otherwise.\n"
    "               * You can also use '-superhard' to iterate even more, but\n"
    "                 this extra option will REALLY slow things down.\n"
    "           -->>* Under most circumstances, you should not need to use either\n"
    "                 -workhard or -superhard.\n"
    "           -->>* The fastest way to register to a template image is via the\n"
    "                 -duplo option, and without the -workhard or -superhard options.\n"
#ifdef ALLOW_QFINAL
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
    "               * This option is also not usually needed, and is experimental.\n"
#endif
#ifdef USE_SAVER
    "\n"
    " -qsave       = Save intermediate warped results as well, in a dataset\n"
    "                with '_SAVE' appended to the '-prefix' value.\n"
    "               * This allows you to see the amount of improvement at\n"
    "                 each patch refinement level, and may help you decide\n"
    "                 the size for '-minpatch' for future work.\n"
    "               * Otherwise, this option is mostly for debugging.\n"
#endif
#ifdef ALLOW_PLUSMINUS
    "\n"
    " -plusminus   = Normally, the warp displacements dis(x) are defined to match\n"
    "                base(x) to source(x+dis(x)).  With this option, the match\n"
    "                is between base(x-dis(x)) and source(x+dis(x)) -- the two\n"
    "                images 'meet in the middle'.\n"
    "               * One goal is to mimic the warping done to MRI EPI data by\n"
    "                 field inhomogeneities, when registering between a 'blip up'\n"
    "                 and a 'blip down' down volume, which will have opposite\n"
    "                 distortions.\n"
    "               * -plusminus does not work with -duplo :-(\n"
#ifdef USE_PLUSMINUS_INITIALWARP
    "               * If -plusminus is used, the -plusminus warp is initialized by\n"
    "                 a coarse warping of the source to the base, then these warp\n"
    "                 displacements are scaled by 0.5, and then the actual\n"
    "                 'meet in the middle' warp optimization begins from that point.\n"
#endif
    "               * The outputs have _PLUS (from the source dataset) and _MINUS\n"
    "                 (from the base dataset) in their filenames, in addition to\n"
    "                 the prefix.  The -iwarp option, if present, will be ignored.\n"
    "\n"
    " -pmNAMES p m = This option lets you change the PLUS and MINUS prefix appendages\n"
    "                alluded to directly above to something else that might be more\n"
    "                easy for you to grok.  For example, if you are warping EPI volumes\n"
    "                with phase-encoding in the LR-direction with volumes that had\n"
    "                phase-encoding in the RL-direction, you might do something like\n"
    "        -base EPI_LR+orig -source EPI_RL+orig -plusminus -pmNAMES RL LR -prefix EPIuw\n"
    "                recalling the the PLUS name goes with the source (RL) and the\n"
    "                MINUS name goes with the base (RL).  Then you'd end up with datasets\n"
    "                  EPIuw_LR+orig and EPIuw_LR_WARP+orig from the base\n"
    "                  EPIuw_RL+orig and EPIuw_RL_WARP+orig from the source\n"
    "                The EPIuw_LR_WARP+orig file could then be used to unwarp (e.g.,\n"
    "                using 3dNwarpApply) other LR-encoded EPI datasets from the same\n"
    "                scanning session.\n"
#endif
    "\n"
    " -verb        = Print out very very verbose progress messages (to stderr) :-)\n"
    " -quiet       = Cut out most of the fun fun fun progress messages :-(\n"
    "\n"
    "-----------------\n"
    "OUTLINE OF METHOD\n"
    "-----------------\n"
    "Composition of incremental warps defined by Hermite cubic basis functions, first\n"
    "over the entire volume, then over steadily shrinking and overlapping patches\n"
    "(increasing 'levels': the patches shrink by a factor of 0.75 at each level).\n"
    "At 'level 0' (over the entire volume), Hermite quintic basis functions are also\n"
    "employed, but these are not used at the more refined levels.  All basis functions\n"
    "herein are (at least) continuously differentiable, so the discrete warp computed\n"
    "will be a representation of an underlying C1 diffeomorphism.  The basis functions\n"
    "go to zero at the edge of each patch, so the overall warp will decay to the identity\n"
    "warp (displacements=0) at the edge of the base volume.\n"
    "\n"
    "For this procedure to work, the source and base datasets need to be reasonably\n"
    "well aligned already (e.g., via 3dAllineate, if necessary). Multiple warps can\n"
    "later be composed and applied via programs 3dNwarpApply and/or 3dNwarpCalc.\n"
    "\n"
    "Note that it is not correct to say that the resulting warp is a piecewise cubic\n"
    "(or quintic) polynomial.  The first warp created (at level 0) is such a warp;\n"
    "call that W0(x).  Then the incremental warp W1(x) applied at the next iteration\n"
    "is also a cubic polynomial warp (say), and the result is W0(W1(x)), which is\n"
    "more complicated than a cubic polynomial -- and so on.  The incremental warps\n"
    "aren't added, but composed, so that the mathematical form of the final warp\n"
    "would be very unwieldy to express in polynomial form.  Of course, the program\n"
    "just keeps track of the displacements, not the polynomial coefficients, so it\n"
    "doesn't 'care' about the underlying polynomials at all.\n"
    "\n"
    "One reason for incremental improvement by composition, rather than by addition,\n"
    "is the simple fact that if W0(x) is invertible and W1(x) is invertible, then\n"
    "W0(W1(x)) is also invertible -- but W0(x)+W1(x) might not be.  The incremental\n"
    "polynomial warps are kept invertible by constraints on their coefficients.\n"
    "\n"
    "The penalty is a Neo-Hookean elastic energy function, based on a combination\n"
    "of bulk and shear distortions; cf. http://en.wikipedia.org/wiki/Neo-Hookean_solid\n"
    "The goal is to keep the warps from becoming too 'weird'.\n"
    "\n"
    "***** This program is experimental and subject to sudden horrific change! *****\n"
    "\n"
    "----- AUTHOR = Zhark the Grotesquely Warped -- Fall/Winter/Spring 2012-13 -----\n"
  ) ;

  PRINT_AFNI_OMP_USAGE("3dQwarp",
                       "* Tests show that using more 10-12 CPUs with 3dQwarp doesn't help.\n"
                       "  If you have more CPUs on one system, it's faster to run two or three\n"
                       "  separate registration jobs in parallel than to use all the CPUs on\n"
                       "  one 3dQwarp task.\n" ) ;
  exit(0) ;
}

/*---------------------------------------------------------------------------*/

int main( int argc , char *argv[] )
{
   THD_3dim_dataset *bset=NULL , *sset=NULL , *oset , *iwset=NULL ;
   MRI_IMAGE *bim , *wbim , *sim , *oim ;
   NI_float_array *iwvec=NULL ;
   IndexWarp3D *oww , *owwi ; Image_plus_Warp *oiw=NULL ;
   char *prefix = "Qwarp" ; int nopt , nevox=0 ;
   int meth = GA_MATCH_PEARCLP_SCALAR ;
   int ilev = 0 , nowarp = 0 , nowarpi = 1 , mlev = 666 , nodset = 0 ;
   int duplo=0 , qsave=0 , minpatch=0 , nx,ny,nz , ct , nnn ;
   int flags = 0 ;
   double cput ;
   int do_plusminus=0; Image_plus_Warp **sbww=NULL, *qiw=NULL; /* 14 May 2013 */
   char *plusname = "PLUS" , *minusname = "MINUS" ;
   char appendage[THD_MAX_NAME] ;

   /*---------- enlighten the supplicant ----------*/

   if( argc < 3 || strcasecmp(argv[1],"-help") == 0 ){ Qhelp(); exit(0); }

   /*---------- startup bureaucracy --------*/

#if defined(USING_MCW_MALLOC) && !defined(USE_OMP)
   enable_mcw_malloc() ;
#endif

#ifdef USE_OMP
   omp_set_nested(0) ;
   nthmax = omp_get_max_threads() ;
   dhaar  = (double *)malloc(sizeof(double)*nthmax*2) ;  /* 6 workspaces */
   dhbbr  = (double *)malloc(sizeof(double)*nthmax*2) ;  /* for each thread */
   dhccr  = (double *)malloc(sizeof(double)*nthmax*2) ;
   dhddr  = (double *)malloc(sizeof(double)*nthmax*2) ;
   dheer  = (double *)malloc(sizeof(double)*nthmax*2) ;
   dhffr  = (double *)malloc(sizeof(double)*nthmax*2) ;
   INFO_message("OpenMP thread count = %d",nthmax) ;
#else
   INFO_message  ("This edition not compiled with OpenMP.") ;
   ININFO_message("It will be very slooooowwwwww .... :-(") ;
#endif

   mainENTRY("3dQwarp") ; machdep() ;
   AFNI_logger("3dQwarp",argc,argv);
   PRINT_VERSION("3dQwarp"); AUTHOR("Zhark the (Hermite) Cubically Warped");
   (void)COX_clock_time() ;  /* initialize the clock timer */
   putenv("AFNI_WSINC5_SILENT=YES") ;

   /*--- options ---*/

   nopt = 1 ;
   Hblur_b = Hblur_s = 2.345f ;
   while( nopt < argc && argv[nopt][0] == '-' ){

     if( strcasecmp(argv[nopt],"-verb") == 0 ){
       Hverb++ ; nopt++ ; continue ;
     }
     if( strcasecmp(argv[nopt],"-quiet") == 0 ){
       Hverb = 0 ; nopt++ ; continue ;
     }
     if( strcasecmp(argv[nopt],"-nowarp") == 0 ){
       nowarp =  1 ; nopt++ ; continue ;
     }
     if( strcasecmp(argv[nopt],"-iwarp") == 0 ){
       nowarpi = 0 ; nopt++ ; continue ;
     }
     if( strcasecmp(argv[nopt],"-nodset") == 0 ){
       nodset =  1 ; nopt++ ; continue ;
     }

     if( strcasecmp(argv[nopt],"-plusminus") == 0 || strcmp(argv[nopt],"+-") == 0 ){
#ifdef ALLOW_PLUSMINUS
       do_plusminus++ ; nopt++ ; continue ;
#else
       ERROR_exit("Option '%s' is not currently available :-(",argv[nopt]) ;
#endif
     }

     if( strcasecmp(argv[nopt],"-pmNAMES") == 0 ){
       if( ++nopt >= argc-1 ) ERROR_exit("need 2 args after %s",argv[nopt-1]) ;
       plusname = argv[nopt++] ; minusname = argv[nopt++] ; continue ;
     }

     if( strcasecmp(argv[nopt],"-nowarps") == 0 ){  /* these 2 options */
       WARNING_message("-nowarps option is now deprecated: see the -help output") ;
       nowarpi = nowarp = 1 ; nopt++ ; continue ;   /* are just for backward */
     }                                              /* compatibility */
     if( strcasecmp(argv[nopt],"-nowarpi") == 0 ){
       WARNING_message("-nowarpi option is now deprecated: see the -help output") ;
       nowarpi = 1 ; nopt++ ; continue ;
     }

     if( strcasecmp(argv[nopt],"-patchmin") == 0 || strcasecmp(argv[nopt],"-minpatch") == 0 ){
       if( ++nopt >= argc ) ERROR_exit("need arg after %s",argv[nopt-1]) ;
       minpatch = (int)strtod(argv[nopt],NULL) ;
       if( minpatch < NGMIN ) minpatch = NGMIN ; else if( minpatch%2 == 0 ) minpatch-- ;
       nopt++ ; continue ;
     }

     if( strcasecmp(argv[nopt],"-inilev") == 0 ){
       if( duplo          ) ERROR_exit("Cannot use -inilev with -duplo :-(") ;
       if( ++nopt >= argc ) ERROR_exit("need arg after %s",argv[nopt-1]) ;
       ilev = (int)strtod(argv[nopt],NULL) ;
       if( ilev < 0 ) ilev = 0 ; else if( ilev > 9 ) ilev = 9 ;
       nopt++ ; continue ;
     }

     if( strcasecmp(argv[nopt],"-maxlev") == 0 ){
       if( duplo          ) ERROR_exit("Cannot use -maxlev with -duplo :-(") ;
       if( ++nopt >= argc ) ERROR_exit("need arg after %s",argv[nopt-1]) ;
       mlev = (int)strtod(argv[nopt],NULL) ;
       if( mlev < 0 ) mlev = 0 ; else if( mlev > 99 ) mlev = 99 ;
       nopt++ ; continue ;
     }

     if( strcasecmp(argv[nopt],"-iniwarp") == 0 ){
       char *apt=NULL ;
       if( duplo )
         ERROR_exit("Cannot use -iniwarp with -duplo :-(") ;
       if( iwset != NULL || iwvec != NULL )
         ERROR_exit("Cannot use -iniwarp twice :-(") ;
       if( ++nopt >= argc )
         ERROR_exit("need arg after %s",argv[nopt-1]) ;

       /* see if the name specifies an affine transform */

       if( strncasecmp(argv[nopt],"MATRIX(",7) == 0 ){
         apt = argv[nopt]+7 ; affmode = AFF_MATRIX ;
       } else if( strncasecmp(argv[nopt],"PARAM(",6) == 0 ){
         apt = argv[nopt]+6 ; affmode = AFF_PARAM ;
       }

       if( apt != NULL ){  /* read list of 12 parameters for affine transform */

         iwvec = NI_decode_float_list(apt,",;:)") ;
         if( iwvec == NULL )
           ERROR_exit("Can't decode -iniwarp parameter list :-(") ;
         if( iwvec->num < 12 )
           ERROR_exit("-iniwarp parameter list has fewer than 12 values :-(") ;
         if( iwvec->num > 12 )
           WARNING_message("-iniwarp parameter list has more than 12 values ?!?") ;

       } else {            /* read dataset for initial warp */

         iwset = IW3D_read_catenated_warp(argv[nopt]) ;  /* the fancy way to read */
         if( iwset == NULL )
           ERROR_exit("Cannot open -iniwarp %s",argv[nopt]) ;
         if( DSET_NVALS(iwset) < 3 || DSET_BRICK_TYPE(iwset,0) != MRI_float )
           ERROR_exit("-iniwarp %s is not in the right format :-(",argv[nopt]) ;

       }

       nopt++ ; continue ;
     }

     if( strcasecmp(argv[nopt],"-duplo") == 0 ){
       if( iwset != NULL || iwvec != NULL )
         ERROR_exit("Cannot use -duplo with -iniwarp :-(") ;
       if( ilev != 0 || mlev < 99 )
         ERROR_exit("Cannot use -duplo with -inilev or -maxlev :-(") ;
       duplo = 1 ; nopt++ ; continue ;
     }

     if( strncasecmp(argv[nopt],"-workhard",9) == 0 ){
       char *wpt = argv[nopt]+9 ;
       Hworkhard1 = 0 ; Hworkhard2 = 66 ;
       if( *wpt == ':' && isdigit(*(wpt+1)) ){
         char *cpt ;
         Hworkhard2 = (int)strtod(++wpt,NULL) ;
         cpt = strchr(wpt,':') ;
         if( cpt != NULL && isdigit(*(cpt+1)) ){
           Hworkhard1 = Hworkhard2 ;
           Hworkhard2 = (int)strtod(++cpt,NULL) ;
         }
       }
       nopt++ ; continue ;
     }

     if( strcasecmp(argv[nopt],"-superhard") == 0 ){  /* 30 Apr 2013 */
       char *wpt = argv[nopt]+9 ;
       Hsuperhard1 = 0 ; Hsuperhard2 = 66 ;
       if( *wpt == ':' && isdigit(*(wpt+1)) ){
         char *cpt ;
         Hsuperhard2 = (int)strtod(++wpt,NULL) ;
         cpt = strchr(wpt,':') ;
         if( cpt != NULL && isdigit(*(cpt+1)) ){
           Hsuperhard1 = Hsuperhard2 ;
           Hsuperhard2 = (int)strtod(++cpt,NULL) ;
         }
       }
       nopt++ ; continue ;
     }

#ifdef ALLOW_QFINAL
     if( strcasecmp(argv[nopt],"-Qfinal") == 0 ){     /* 07 May 2013 */
       Hqfinal = 1 ; nopt++ ; continue ;
     }
#endif

#ifdef USE_SAVER
     if( strcasecmp(argv[nopt],"-qsave") == 0 ){
       qsave = 1 ; nopt++ ; continue ;
     }
#endif

     if( strcasecmp(argv[nopt],"-noXdis") == 0 ){
       flags |= NWARP_NOXDIS_FLAG ; nopt++ ; continue ;
     }
     if( strcasecmp(argv[nopt],"-noYdis") == 0 ){
       flags |= NWARP_NOYDIS_FLAG ; nopt++ ; continue ;
     }
     if( strcasecmp(argv[nopt],"-noZdis") == 0 ){
       flags |= NWARP_NOZDIS_FLAG ; nopt++ ; continue ;
     }

     if( strcasecmp(argv[nopt],"-base") == 0 ){
       if( bset   != NULL ) ERROR_exit("Can't use -base twice!") ;
       if( ++nopt >= argc ) ERROR_exit("need arg after -base") ;
       bset = THD_open_dataset(argv[nopt]) ; if( bset == NULL ) ERROR_exit("Can't open -base") ;
       nopt++ ; continue ;
     }

     if( strcasecmp(argv[nopt],"-source") == 0 ){
       if( sset   != NULL ) ERROR_exit("Can't use -source twice!") ;
       if( ++nopt >= argc ) ERROR_exit("need arg after -source") ;
       sset = THD_open_dataset(argv[nopt]) ; if( sset == NULL ) ERROR_exit("Can't open -source") ;
       nopt++ ; continue ;
     }

     if( strcasecmp(argv[nopt],"-emask") == 0 ){
       THD_3dim_dataset *eset ;
       if( Hemask != NULL ) ERROR_exit("Can't use -emask twice!") ;
       if( ++nopt >= argc ) ERROR_exit("need arg after -emask") ;
       eset = THD_open_dataset(argv[nopt]) ; if( eset == NULL ) ERROR_exit("Can't open -emask") ;
       DSET_load(eset) ; CHECK_LOAD_ERROR(eset) ;
       Hemask = THD_makemask( eset , 0 , 1.0f , -1.0f ) ;
       if( Hemask == NULL ) ERROR_exit("Can't make -emask for some reason :-(") ;
       nevox = DSET_NVOX(eset) ;
       DSET_delete(eset) ;
       nopt++ ; continue ;
     }

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

     if( strcasecmp(argv[nopt],"-nopenalty") == 0 ){
       Hpen_fac = 0.0 ; nopt++ ; continue ;
     }

     if( strncasecmp(argv[nopt],"-useweight",10) == 0 ){
       auto_weight = 1 ;
       if( argv[nopt][10] == '*' && argv[nopt][11] == '*' && isnumeric(argv[nopt][12]) )
         auto_wpow = (float)strtod(argv[nopt]+12,NULL) ;
       nopt++ ; continue ;
     }

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

     if( strcasecmp(argv[nopt],"-prefix") == 0 ){
       if( ++nopt >= argc ) ERROR_exit("need arg after -prefix") ;
       prefix = strdup(argv[nopt]) ; nopt++ ; continue ;
     }

     if( strcasecmp(argv[nopt],"-hel") == 0 ){
       meth = GA_MATCH_HELLINGER_SCALAR ; nopt++ ; continue ;
     }

     if( strcasecmp(argv[nopt],"-mi") == 0 ){
       meth = GA_MATCH_KULLBACK_SCALAR ; nopt++ ; continue ;
     }

     if( strcasecmp(argv[nopt],"-nmi") == 0 ){
       meth = GA_MATCH_NORMUTIN_SCALAR ; nopt++ ; continue ;
     }

     if( strcasecmp(argv[nopt],"-pcl") == 0 ){
       meth = GA_MATCH_PEARCLP_SCALAR ; nopt++ ; continue ;
     }

     if( strcasecmp(argv[nopt],"-pear") == 0 ){
       meth = GA_MATCH_PEARSON_SCALAR ; nopt++ ; continue ;
     }

     ERROR_exit("Bogus option '%s'",argv[nopt]) ;
   }

   /*----- check for errorororors -----*/

   if( flags == NWARP_NODISP_FLAG )
     ERROR_exit("too many -no?dis flags ==> nothing to warp!") ;

   ct = NI_clock_time() ;

   if( bset == NULL && sset == NULL && nopt+1 >= argc )
     ERROR_exit("need 2 args for base and source") ;

   if( do_plusminus && duplo ){
     WARNING_message("Alas, -plusminus does not work with -duplo -- turning -duplo off") ;
     duplo = 0 ;
   }

   if( (iwset != NULL || iwvec != NULL) && duplo )
     ERROR_exit("You cannot combine -iniwarp and -duplo !! :-((") ;
   if( (ilev != 0 || mlev < 99 ) && duplo )
     ERROR_exit("You cannot combine -inilev or -maxlev and -duplo !! :-((") ;

   /*--- get the input datasts, check for errors ---*/

   if( bset != NULL && sset == NULL )
     ERROR_exit("You can't use -base without -source!") ;
   else if( bset == NULL && sset != NULL )
     ERROR_exit("You can't use -source without -base!") ;

   if( bset == NULL ){
     bset = THD_open_dataset(argv[nopt++]) ;
     if( bset == NULL ) ERROR_exit("Can't open base dataset") ;
   }
   if( sset == NULL ){
     sset = THD_open_dataset(argv[nopt++]) ;
     if( sset == NULL ) ERROR_exit("Can't open source dataset") ;
   }

   if( !EQUIV_GRIDXYZ(bset,sset) ) ERROR_exit("base-source dataset grid mismatch :-(") ;
   if(  EQUIV_DSETS  (bset,sset) ) ERROR_exit("base & source datasets are identical :-(");

   if( iwset != NULL && !EQUIV_GRIDXYZ(bset,iwset) )
     ERROR_exit("-iniwarp dataset grid mismatch with base dataset :-(") ;

   DSET_load(bset) ; CHECK_LOAD_ERROR(bset) ;
   bim = THD_extract_float_brick(0,bset) ; DSET_unload(bset) ;
   if( DSET_NVALS(bset) > 1 )
     WARNING_message("base dataset has more than 1 sub-brick: ignoring all but the first") ;

   DSET_load(sset) ; CHECK_LOAD_ERROR(sset) ;
   sim = THD_extract_float_brick(0,sset) ; DSET_unload(sset) ;
   if( DSET_NVALS(sset) > 1 )
     WARNING_message("source dataset has more than 1 sub-brick: ignoring all but the first") ;

   if( nevox > 0 && nevox != DSET_NVOX(bset) )
     ERROR_exit("-emask doesn't match base dataset grid :-(") ;

   nx = DSET_NX(bset) ; ny = DSET_NY(bset) ; nz = DSET_NZ(bset) ;

   /*--- initial warp? ---*/

   if( iwset != NULL ){
     DSET_load(iwset) ; CHECK_LOAD_ERROR(iwset) ;
     S2BIM_iwarp = IW3D_from_dataset(iwset,0,0) ;
     if( S2BIM_iwarp == NULL )
       ERROR_exit("Cannot create 3D warp from -iniwarp dataset :-(") ;
     DSET_delete(iwset) ; iwset = NULL ;
   } else if( iwvec != NULL ){
     IndexWarp3D *WW = IW3D_from_dataset(bset,1,0) ;
     if( WW == NULL )                                 /* should never transpire */
       ERROR_exit("Cannot create template 3D warp for -iniwarp -- this is a bug!") ;
     S2BIM_iwarp = IW3D_from_poly(12,iwvec->ar,WW) ;
     if( S2BIM_iwarp == NULL )
       ERROR_exit("Cannot create 3D warp from -iniwarp parameters :-(") ;
     IW3D_destroy(WW) ; NI_delete_float_array(iwvec) ; iwvec = NULL ;
   } else {
     S2BIM_iwarp = NULL ;
   }

   S2BIM_ilev = ilev ;
   S2BIM_mlev = MAX(mlev,ilev) ;

   nnn = 0 ;
   if( nx >= NGMIN             ) nnn = nx ;
   if( ny >= NGMIN && ny > nnn ) nnn = ny ;
   if( nz >= NGMIN && nz > nnn ) nnn = nz ;
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
   if( minpatch > 0 ) Hngmin = minpatch ;

   if( duplo && (nx < 3*Hngmin || ny < 3*Hngmin || nz < 3*Hngmin) ){
     duplo = 0 ;
     INFO_message("-duplo disabled since dataset is so small: %d x %d x %d",nx,ny,nz) ;
   }

#ifdef USE_SAVER
   if( qsave ){
     qset = EDIT_empty_copy(bset) ;
     EDIT_dset_items( qset ,
                        ADN_prefix    , modify_afni_prefix(prefix,NULL,"_SAVE") ,
                        ADN_nvals     , 1 ,
                        ADN_ntt       , 0 ,
                        ADN_datum_all , MRI_float ,
                      ADN_none ) ;
     EDIT_BRICK_FACTOR(qset,0,0.0) ;
     iterfun = Qsaver ;
     tross_Copy_History( bset , qset ) ;
     tross_Make_History( "3dQwarp" , argc,argv , qset ) ;
   }
#endif

   wbim = mri_weightize(bim,auto_weight,auto_dilation,auto_wclip,auto_wpow) ;

   /* blur base here if so ordered */

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

   /*--- do some actual work! ---*/

   if( Hverb )
     INFO_message("Begin warp optimization:  base=%s  source=%s" ,
                  DSET_HEADNAME(bset) , DSET_HEADNAME(sset)  ) ;

   if( do_plusminus ){   /*--- special case of plusminus warp ---*/
#ifndef ALLOW_PLUSMINUS
     ERROR_exit("-plusminus: This message should never appear!") ;
#else
     sbww = IW3D_warp_s2bim_plusminus( bim,wbim,sim, MRI_WSINC5, meth, flags ) ;
     oiw  = sbww[0] ;  /* plus warp and image */
     qiw  = sbww[1] ;  /* minus warp and image */
#endif
   } else {              /*--- the standard case ---*/
     qiw = NULL ;
     if( duplo )
       oiw = IW3D_warp_s2bim_duplo( bim,wbim,sim, MRI_WSINC5, meth, flags ) ;
     else
       oiw = IW3D_warp_s2bim( bim,wbim,sim, MRI_WSINC5, meth, flags ) ;
   }

   /** check for errorosities **/

   if( oiw == NULL ) ERROR_exit("s2bim fails") ;

   INFO_message("===== total number of parameters optimized = %d",Hnpar_sum) ;

   oim = oiw->im ; oww = oiw->warp ;

   /*----- output some results to pacify the user -----*/

   if( !nodset ){
     char *qprefix = prefix ;
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
   } /* end of writing warped datasets */

#ifdef  USE_SAVER
   if( qset != NULL && DSET_NVALS(qset) > 1 ){
     EDIT_dset_items( qset , ADN_ntt , DSET_NVALS(qset) , ADN_none ) ;
     DSET_write(qset) ; WROTE_DSET(qset) ; DSET_delete(qset) ;
   }
#endif

   if( !nowarp ){
     char *qprefix ;
     if( do_plusminus){
       sprintf(appendage,"_%s_WARP",plusname) ;
       qprefix = modify_afni_prefix(prefix,NULL,appendage) ;
     } else {
       qprefix = modify_afni_prefix(prefix,NULL,"_WARP") ;
     }
     IW3D_adopt_dataset( oww , bset ) ;
     qset = IW3D_to_dataset( oww , qprefix ) ;
     tross_Copy_History( bset , qset ) ;
     tross_Make_History( "3dQwarp" , argc,argv , qset ) ;
     MCW_strncpy( qset->atlas_space , bset->atlas_space , THD_MAX_NAME ) ;
     DSET_write(qset) ; WROTE_DSET(qset) ; DSET_delete(qset) ;

     if( do_plusminus && qiw != NULL ){
       sprintf(appendage,"_%s_WARP",minusname) ;
       qprefix = modify_afni_prefix(prefix,NULL,appendage) ;
       IW3D_adopt_dataset( qiw->warp , bset ) ;
       qset = IW3D_to_dataset( qiw->warp , qprefix ) ;
       tross_Copy_History( bset , qset ) ;
       tross_Make_History( "3dQwarp" , argc,argv , qset ) ;
       MCW_strncpy( qset->atlas_space , bset->atlas_space , THD_MAX_NAME ) ;
       DSET_write(qset) ; WROTE_DSET(qset) ; DSET_delete(qset) ;
     }
   } /* end of output of warp dataset */

   if( !nowarpi && !do_plusminus ){
     if( Hverb ) ININFO_message("Inverting warp for output") ;

     owwi = IW3D_invert( oww , NULL , MRI_WSINC5 ) ;
     IW3D_adopt_dataset( owwi , bset ) ;
     qset = IW3D_to_dataset( owwi , modify_afni_prefix(prefix,NULL,"_WARPINV")) ;
     tross_Copy_History( bset , qset ) ;
     tross_Make_History( "3dQwarp" , argc,argv , qset ) ;
     MCW_strncpy( qset->atlas_space , bset->atlas_space , THD_MAX_NAME ) ;
     DSET_write(qset) ; WROTE_DSET(qset) ; DSET_delete(qset) ;
   }

   /*--- go back to watching Matlock reruns ---*/

   cput = COX_cpu_time() ;
   if( cput > 0.05 )
     INFO_message("===== CPU time = %.1f sec  clock time =%s",
                  cput , nice_time_string(NI_clock_time()-ct) ) ;
   else
     INFO_message("===== clock time =%s" , nice_time_string(NI_clock_time()-ct) ) ;

   exit(0) ;
}
