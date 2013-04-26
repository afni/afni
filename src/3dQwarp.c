#include "mrilib.h"

/*---------------------------------------------------------------------------*/
/* Features to ruminate about:
    - NN interpolation of data, for matching label datasets      [not hard]
    - allow user-input weight volume                             [not hard]
    - symmetric mapping,                                         [hard]
      with Src(W(x)) = Bas(INV(W(x))) instead of Src(W(x))=B(x)
*//*-------------------------------------------------------------------------*/

#ifdef USE_OMP
# include <omp.h>
#endif

#include "mri_genalign.c"
#include "mri_genalign_util.c"
#include "mri_nwarp.c"

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

#define USE_SAVER
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

int main( int argc , char *argv[] )
{
   THD_3dim_dataset *bset , *sset , *oset , *iwset=NULL ;
   MRI_IMAGE *bim , *wbim , *sim , *oim ;
   NI_float_array *iwvec=NULL ;
   IndexWarp3D *oww , *owwi ; Image_plus_Warp *oiw ;
   char *prefix = "Qwarp" ; int nopt , nevox=0 ;
   int meth = GA_MATCH_PEARCLP_SCALAR ;
   int ilev = 0 , nowarp = 0 , nowarpi = 1 , mlev = 666 , nodset = 0 ;
   int duplo=0 , qsave=0 , minpatch=0 , nx,ny,nz , ct , nnn ;
   int flags = 0 ;
   double cput ;

   /*---------- enlighten the supplicant ----------*/

   if( argc < 3 || strcasecmp(argv[1],"-help") == 0 ){
     printf("Usage: 3dQwarp [OPTIONS] base_dataset source_dataset\n") ;
     printf(
       "\n"
       "* Computes a warped version of source_dataset to match base_dataset.\n"
       "\n"
       "* Input datasets must be on the same 3D grid!\n"
       "\n"
       "* Inputs should be reasonably well aligned already\n"
       "  (e.g., as from an affine warping via 3dAllineate).\n"
       "\n"
       "* Outputs are the warped dataset and the warp that did it.\n"
       "\n"
       "* Matching by default is the 'clipped Pearson' method, and\n"
       "  can be changed to 'pure Pearson' with the '-pear' option.\n"
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
       " ++ Skull stripping a la 3dSkullStrip is also a good idea (prior to 3dUnifize).\n"
       " ++ If you ultimately want a non-3dUnifize-d transformed dataset, you can use\n"
       "    the output WARP dataset and 3dNwarpApply to transform the un-3dUnifize-d\n"
       "    source dataset.\n"
       "\n"
       "** Please note that this program is both CPU and memory intensive, and is\n"
       "   what computer scientists call a 'pig' or a 'hog'.\n"
#ifndef USE_OMP
       " ++ It would be best to run 3dQwarp on a multi-CPU computer, using a binary\n"
       "    compiled with the OpenMP library. Unfortunately, this particular version is\n"
       "    NOT built with OpenMP, and you will probably find it to be unbearably slow :-(\n"
#endif
       "\n"
       "-------\n"
       "OPTIONS\n"
       "-------\n"
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
       "                 (However, this warp is the most valuable possible output!)\n"
       "               * If you want to calculate and save the inverse 3D warp,\n"
       "                 use the option '-iwarp'.  This inverse warp will then be\n"
       "                 saved in a dataset with prefix 'ppp_WARPINV'.\n"
       "               * This inverse warp could be used to transform data from base\n"
       "                 space to source space, if you need to do such an operation.\n"
       "               * You can easily compute the inverse later, say by a command like\n"
       "                   3dNwarpCat -prefix Z_WARPINV 'INV(Z_WARP+tlrc)'\n"
       "\n"
       " -nowarp      = Don't save the WARP file.\n"
       " -iwarp       = Do save the WARPINV file.\n"
       " -nodset      = Don't save the warped dataset file.\n"
       "\n"
       " -pear        = Use strict Pearson correlation for matching.\n"
       "               * Not usually recommended, since the 'clipped Pearson' method\n"
       "                 used by default will reduce the impact of outliers.\n"
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
       "               * This option is generally recommended.\n"
       "\n"
       " -blur bb     = Gaussian blur the input images by 'bb' (FWHM) voxels before\n"
       "                doing the alignment (the output dataset will not be blurred).\n"
       "                The default is 3.456 (for no good reason).\n"
       "               * Optionally, you can provide 2 values for 'bb', and then\n"
       "                 the first one is applied to the base volume, the second\n"
       "                 to the source volume.\n"
       "               * e.g., '-blur 0 3' to skip blurring the base image\n"
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
       "                 included in the matching process.\n"
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
       "               * You cannot use this option with -duplo !!\n"
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
       "               * Usually used with -iniwarp; cannot be used with -duplo.\n"
       "\n"
       " -minpatch mm = Set the minimum patch size for warp searching to 'mm' voxels.\n"
       "   *OR*        * The value of mm should be an odd integer.\n"
       " -patchmin mm  * The default value of mm is 25.\n"
       "               * For more accurate results than mm=25, try 19 or 13.\n"
       "               * The smallest allowed value is 9 (which will be VERY slow).\n"
       "               * If you want to see the warped results at various levels\n"
       "                 of patch size, use the '-qsave' option.\n"
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
       "                 somewhat piggish program be more expeditious.\n"
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
       "\n"
       " -qsave       = Save intermediate warped results as well, in a dataset\n"
       "                with '_SAVE' appended to the '-prefix' value.\n"
       "               * This allows you to see the amount of improvement at\n"
       "                 each patch refinement level, and may help you decide\n"
       "                 the size for '-minpatch' for future work.\n"
       "               * Otherwise, this optio is mostly for debugging.\n"
       "\n"
       " -verb        = Print very verbose progress messages.\n"
       " -quiet       = Cut out most progress messages.\n"
       "\n"
       "METHOD\n"
       "------\n"
       "Composition of increment warps defined by cubic basic functions, first over\n"
       "the entire volume, then over steadily shrinking patches (increasing 'levels':\n"
       "the patches shrink by a factor of 0.75 at each level).\n"
       "\n"
       "For this procedure to work, the source and base datasets need to be pretty\n"
       "well aligned already (e.g., 3dAllineate).\n"
       "\n"
       "***** This program is experimental and subject to sudden horrific change! *****\n"
       "\n"
       "--- AUTHOR = RWCox -- Fall/Winter/Spring 2012-13 ---\n"
     ) ;
     PRINT_AFNI_OMP_USAGE("3dQwarp",
                          "* Tests show that using more 10-12 CPUs with 3dQwarp doesn't help.\n"
                          "  If you have more CPUs on one system, it's faster to run two or three\n"
                          "  separate registration jobs in parallel than to use all the CPUs on\n"
                          "  one 3dQwarp task.\n" ) ;
     exit(0) ;
   }

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
   ININFO_message("It will be slooooowwwwww ....  :-(") ;
#endif

   mainENTRY("3dQwarp") ; machdep() ;
   AFNI_logger("3dQwarp",argc,argv);
   PRINT_VERSION("3dQwarp"); AUTHOR("Zhark the Hermite Cubically Warped");
   (void)COX_clock_time() ;  /* initialize the clock timer */
   putenv("AFNI_WSINC5_SILENT=YES") ;

   /*--- options ---*/

   nopt = 1 ;
   Hblur_b = Hblur_s = 3.456f ;
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
       } else if( strchr(argv[nopt],'(') != NULL ){
         ERROR_exit("don't understand -iniwarp '%s'",argv[nopt]) ;
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

         iwset = IW3D_read_catenated_warp(argv[nopt]) ;
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

     if( strcasecmp(argv[nopt],"-qsave") == 0 ){
       qsave = 1 ; nopt++ ; continue ;
     }

     if( strcasecmp(argv[nopt],"-noXdis") == 0 ){
       flags |= NWARP_NOXDIS_FLAG ; nopt++ ; continue ;
     }
     if( strcasecmp(argv[nopt],"-noYdis") == 0 ){
       flags |= NWARP_NOYDIS_FLAG ; nopt++ ; continue ;
     }
     if( strcasecmp(argv[nopt],"-noZdis") == 0 ){
       flags |= NWARP_NOZDIS_FLAG ; nopt++ ; continue ;
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
       if( nopt+1 < argc && isnumeric(argv[nopt+1][0]) )
         val2 = (float)strtod(argv[++nopt],NULL) ;
       Hblur_b = val1 ; Hblur_s = val2 ;
       nopt++ ; continue ;
     }

     if( strcasecmp(argv[nopt],"-nopenalty") == 0 ){
       Hpen_fac = 0.0 ; nopt++ ; continue ;
     }

     if( strcasecmp(argv[nopt],"-useweight") == 0 ){
       auto_weight = 1 ; nopt++ ; continue ;
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

   if( nopt+1 >= argc ) ERROR_exit("need 2 args for base and source") ;

   if( (iwset != NULL || iwvec != NULL) && duplo )
     ERROR_exit("You cannot combine -iniwarp and -duplo !! :-((") ;
   if( (ilev != 0 || mlev < 99 ) && duplo )
     ERROR_exit("You cannot combine -inilev or -maxlev and -duplo !! :-((") ;

   /*--- get the input datasts  ---*/

   bset = THD_open_dataset(argv[nopt++]) ; if( bset == NULL ) ERROR_exit("Can't open bset") ;
   sset = THD_open_dataset(argv[nopt++]) ; if( sset == NULL ) ERROR_exit("Can't open sset") ;
   if( !EQUIV_GRIDXYZ(bset,sset) ) ERROR_exit("base-source dataset grid mismatch :-(") ;

   if( iwset != NULL && !EQUIV_GRIDXYZ(bset,iwset) )
     ERROR_exit("-iniwarp dataset grid mismatch with base dataset :-(") ;

   DSET_load(bset) ; CHECK_LOAD_ERROR(bset) ;
   bim = THD_extract_float_brick(0,bset) ; DSET_unload(bset) ;

   DSET_load(sset) ; CHECK_LOAD_ERROR(sset) ;
   sim = THD_extract_float_brick(0,sset) ; DSET_unload(sset) ;

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

   if( Hblur_b >= 0.5f ){
     MRI_IMAGE *qim ;
     if( Hverb > 1 ) ININFO_message("   blurring base image %.3g voxels FWHM",Hblur_b) ;
     qim = mri_float_blur3D( FWHM_TO_SIGMA(Hblur_b) , bim ) ;
     mri_free(bim) ; bim = qim ;
   } else if( Hblur_b <= -1.0f ){
     MRI_IMAGE *qim ;
     if( Hverb > 1 ) ININFO_message("   median-izing base image %.3g voxels",-Hblur_b) ;
     qim = mri_medianfilter( bim , -Hblur_b , NULL , 0 ) ;
     mri_free(bim) ; bim = qim ;
   }

   /*--- do some actual work! ---*/

   if( Hverb )
     INFO_message("Begin warp optimization:  base=%s  source=%s" ,
                  DSET_HEADNAME(bset) , DSET_HEADNAME(sset)  ) ;

   if( duplo )
     oiw = IW3D_warp_s2bim_duplo( bim,wbim , sim , MRI_WSINC5 , meth , flags ) ;
   else
     oiw = IW3D_warp_s2bim( bim,wbim , sim , MRI_WSINC5 , meth , flags ) ;

   if( oiw == NULL ) ERROR_exit("s2bim fails") ;

   INFO_message("===== total number of parameters optimized = %d",Hnpar_sum) ;

   oim = oiw->im ; oww = oiw->warp ;

   /*----- output some results to pacify the user -----*/

   if( !nodset ){
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
     EDIT_substitute_brick( oset, 0, MRI_float, MRI_FLOAT_PTR(oim) ) ;
     DSET_write(oset) ; WROTE_DSET(oset) ; DSET_delete(oset) ;
   }

   if( qset != NULL && DSET_NVALS(qset) > 1 ){
     EDIT_dset_items( qset , ADN_ntt , DSET_NVALS(qset) , ADN_none ) ;
     DSET_write(qset) ; WROTE_DSET(qset) ; DSET_delete(qset) ;
   }

   if( !nowarp ){
     IW3D_adopt_dataset( oww , bset ) ;
     qset = IW3D_to_dataset( oww , modify_afni_prefix(prefix,NULL,"_WARP") ) ;
     tross_Copy_History( bset , qset ) ;
     tross_Make_History( "3dQwarp" , argc,argv , qset ) ;
     MCW_strncpy( qset->atlas_space , bset->atlas_space , THD_MAX_NAME ) ;
     DSET_write(qset) ; WROTE_DSET(qset) ; DSET_delete(qset) ;
   }
   if( !nowarpi ){
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
