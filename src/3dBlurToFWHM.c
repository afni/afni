#include "mrilib.h"

#define NTYPE_SPHERE 1
#define NTYPE_RECT   2
#define NTYPE_NULL   666

#define BAILOUT      3
#define BLURMAX      0.05f
#define DFGOAL       0.2f
#define DFGOALC      (1.0f-DFGOAL)

static int verb = 1 ;

void estimate_blur_map( MRI_IMARR *bmar , byte *mask  , MCW_cluster *nbhd ,
                        float *fxar     , float *fyar , float *fzar        ) ;

/*----------------------------------------------------------------------------*/

int main( int argc , char *argv[] )
{
   int iarg=1 , ii,nvox ;
   THD_3dim_dataset *bmset=NULL, *inset=NULL, *outset=NULL , *mset=NULL ;
   char *prefix="./blurto" ;
   float fwhm_goal=0.0f , fwhm_subgoal ; int fwhm_2D=0 ;
   MCW_cluster *nbhd=NULL ;
   byte *mask=NULL ; int mask_nx=0,mask_ny=0,mask_nz=0 , automask=0 , nmask ;
   int ntype=0 ; float na=0.0f,nb=0.0f,nc=0.0f ;
   MRI_IMARR *bmar ; MRI_IMAGE *bmed , *bmim ; int ibm ;
   MRI_IMARR *dsar ; MRI_IMAGE *dsim ;         int ids ;
   MRI_IMAGE *fxim=NULL , *fyim=NULL , *fzim=NULL ;
   float     *fxar=NULL , *fyar=NULL , *fzar=NULL ;
   float dx,dy,dz=0.0f , hx,hy,hz=0.0f , qx,qy,qz=0.0f ;
   float gx,gy,gz=0.0f , val , maxfxyz , maxfx,maxfy,maxfz ;
   int   nite , bmeqin=0 , maxite=0 , numfxyz , nd,nblur , xdone,ydone,zdone ;
   int   xstall , ystall , zstall ;
   float bx,by,bz ;
   float last_fwx=0.0f , last_fwy=0.0f , last_fwz=0.0f ;
   float delt_fwx      , delt_fwy      , delt_fwz      ;
   char *bsave_prefix=NULL ;
   THD_fvec3 fw ;
   int nbail=0 , xalmost,yalmost,zalmost , xstopped=0,ystopped=0,zstopped=0 ;
   float blurfac , blurmax=BLURMAX ;
   float xrat,yrat,zrat , dmin ;
   int temperize=0 , temper_fx=0, temper_fy=0, temper_fz=0 ;
   float fx_tbot=0.0f,fx_ttop=0.0f,fy_tbot=0.0f,fy_ttop=0.0f,fz_tbot=0.0f,fz_ttop=0.0f ;
   float fx_trat=0.0f,fy_trat=0.0f,fz_trat=0.0f ;
   int bmall=0,do_unif=0 ;           /* 11 Dec 2006 */
   MRI_IMARR *imar ; MRI_IMAGE *imed, *imad ; float *imedar=NULL, *imadar=NULL, *bar ;

   int corder_bm=-1 , corder_in=0 ;   /* 04 Jun 2007: detrending */
   MRI_IMARR *corder_inar=NULL ; MRI_IMAGE *corder_invv ;
   float **corder_inref=NULL ;
   int corder_inrefnum=0 ;

   /*-- help the pitifully ignorant luser? --*/

   if( argc < 2 || strcmp(argv[1],"-help") == 0 ){
     printf(
      "Usage: 3dBlurToFWHM [options]\n"
      "Blurs a 'master' dataset until it reaches a specified FWHM\n"
      "smoothness (approximately).  The same blurring schedule is\n"
      "applied to the input dataset to produce the output.  The goal\n"
      "is to make the output dataset have the given smoothness, no\n"
      "matter what smoothness it had on input (however, the program\n"
      "cannot 'unsmooth' a dataset!).  See below for the method used.\n"
      "\n"
      "OPTIONS\n"
      "-------\n"
      " -input      ddd = This required 'option' specifies the dataset\n"
      "                   that will be smoothed and output.\n"
      " -blurmaster bbb = This option specifies the dataset whose\n"
      "                   whose smoothness controls the process.\n"
      "                  **N.B.: If not given, the input dataset is used.\n"
      "                  **N.B.: This should be one continuous run.\n"
      "                          Do not input catenated runs!\n"
      " -prefix     ppp = Prefix for output dataset will be 'ppp'.\n"
      "                  **N.B.: Output dataset is always in float format.\n"
      " -mask       mmm = Mask dataset, if desired.  Blurring will\n"
      "                   occur only within the mask.  Voxels NOT in\n"
      "                   the mask will be set to zero in the output.\n"
      " -automask       = Create an automask from the input dataset.\n"
      "                  **N.B.: Not useful if the input dataset has\n"
      "                          been detrended before input!\n"
      " -FWHM       f   = Blur until the 3D FWHM is 'f'.\n"
      " -FWHMxy     f   = Blur until the 2D (x,y)-plane FWHM is 'f'.\n"
      "                   No blurring is done along the z-axis.\n"
      "                  **N.B.: Note that you can't REDUCE the smoothness\n"
      "                          of a dataset.\n"
      "                  **N.B.: Here, 'x', 'y', and 'z' refer to the\n"
      "                          grid/slice order as stored in the dataset,\n"
      "                          not DICOM ordered coordinates!\n"
      "                  **N.B.: With -FWHMxy, smoothing is done only in the\n"
      "                          dataset xy-plane.  With -FWHM, smoothing\n"
      "                          is done in 3D.\n"
      "                  **N.B.: The actual goal is reached when\n"
      "                            -FHWM  :  cbrt(FWHMx*FWHMy*FWHMz) >= f\n"
      "                            -FWHMxy:  sqrt(FWHMx*FWHMy)       >= f\n"
      "                          That is, when the area or volume of a\n"
      "                          'resolution element' goes past a threshold.\n"
      " -quiet            Shut up the verbose progress reports.\n"
      "                  **N.B.: This should be the first option, to stifle\n"
      "                          any verbosity from the option processing code.\n"
      "\n"
      "FILE RECOMMENDATIONS for -blurmaster:\n"
      "For FMRI statistical purposes, you DO NOT want the FWHM to reflect\n"
      "  the spatial structure of the underlying anatomy.  Rather, you want\n"
      "  the FWHM to reflect the spatial structure of the noise.  This means\n"
      "  that the -blurmaster dataset should not have anatomical structure.  One\n"
      "  good form of input is the output of '3dDeconvolve -errts', which is\n"
      "  the residuals left over after the GLM fitted signal model is subtracted\n"
      "  out from each voxel's time series.\n"
      "You CAN give a multi-brick EPI dataset as the -blurmaster dataset; the\n"
      "  dataset will be detrended in time (like the -detrend option in 3dFWHMx)\n"
      "  which will tend to remove the spatial structure.  This makes it\n"
      "  practicable to make the input and blurmaster datasets be the same,\n"
      "  without having to create a detrended or residual dataset beforehand.\n"
      "  Considering the accuracy of blurring estimates, this is probably good\n"
      "  enough for government work [that is an insider's joke]. \n"
      "  N.B.: Do not use catenated runs as blurmasters. There \n"
      "  should be no discontinuities in the time axis of blurmaster.\n"
      "\n"
      "ALSO SEE:\n"
      " * 3dFWHMx, which estimates smoothness globally\n"
      " * 3dLocalstat -stat FHWM, which estimates smoothness locally\n"
      " * This paper, which discusses the need for a fixed level of smoothness\n"
      "   when combining FMRI datasets from different scanner platforms:\n"
      "     Friedman L, Glover GH, Krenz D, Magnotta V; The FIRST BIRN. \n"
      "     Reducing inter-scanner variability of activation in a multicenter\n"
      "     fMRI study: role of smoothness equalization.\n"
      "     Neuroimage. 2006 Oct 1;32(4):1656-68.\n"
      "\n"
      "METHOD:\n"
      "The blurring is done by a conservative finite difference approximation\n"
      "to the diffusion equation:\n"
      "  du/dt = d/dx[ D_x(x,y,z) du/dx ] + d/dy[ D_y(x,y,z) du/dy ]\n"
      "                                   + d/dz[ D_z(x,y,z) du/dz ]\n"
      "        = div[ D(x,y,z) grad[u(x,y,z)] ]\n"
      "where diffusion tensor D() is diagonal, Euler time-stepping is used, and\n"
      "with Neumann (reflecting) boundary conditions at the edges of the mask\n"
      "(which ensures that voxel data inside and outside the mask don't mix).\n"
      "* At each pseudo-time step, the FWHM is estimated globally (like '3dFWHMx')\n"
      "  and locally (like '3dLocalstat -stat FWHM'). Voxels where the local FWHM\n"
      "  goes past the goal will not be smoothed any more (D gets set to zero).\n"
      "* When the global smoothness estimate gets close to the goal, the blurring\n"
      "  rate (pseudo-time step) will be reduced, to avoid over-smoothing.\n"
      "* When an individual direction's smoothness (e.g., FWHMz) goes past the goal,\n"
      "  all smoothing in that direction stops, but the other directions continue\n"
      "  to be smoothed until the overall resolution element goal is achieved.\n"
      "* When the global FWHM estimate reaches the goal, the program is done.\n"
      "  It will also stop if progress stalls for some reason, or if the maximum\n"
      "  iteration count is reached (infinite loops being unpopular).\n"
      "* The output dataset will NOT have exactly the smoothness you ask for, but\n"
      "  it will be close (fondly we do hope).  In our Imperial experiments, the\n"
      "  results (measured via 3dFWHMx) are within 10%% of the goal (usually better).\n"
      "* 2D blurring via -FWHMxy may increase the smoothness in the z-direction\n"
      "  reported by 3dFHWMx, even though there is no inter-slice processing.\n"
      "  At this moment, I'm not sure why.  It may be an estimation artifact due\n"
      "  to increased correlation in the xy-plane that biases the variance estimates\n"
      "  used to calculate FWHMz.\n"
      "\n"
      "ADVANCED OPTIONS:\n"
      " -maxite  ccc = Set maximum number of iterations to 'ccc' [Default=variable].\n"
      " -rate    rrr = The value of 'rrr' should be a number between\n"
      "                0.05 and 3.5, inclusive.  It is a factor to change\n"
      "                the overall blurring rate (slower for rrr < 1) and thus\n"
      "                require more or less blurring steps.  This option should only\n"
      "                be needed to slow down the program if the it over-smooths\n"
      "                significantly (e.g., it overshoots the desired FWHM in\n"
      "                Iteration #1 or #2).  You can increase the speed by using\n"
      "                rrr > 1, but be careful and examine the output.\n"
      " -nbhd    nnn = As in 3dLocalstat, specifies the neighborhood\n"
      "                used to compute local smoothness.\n"
      "                [Default = 'SPHERE(-4)' in 3D, 'SPHERE(-6)' in 2D]\n"
      "               ** N.B.: For the 2D -FWHMxy, a 'SPHERE()' nbhd\n"
      "                        is really a circle in the xy-plane.\n"
      "               ** N.B.: If you do NOT want to estimate local\n"
      "                        smoothness, use '-nbhd NULL'.\n"
      " -bsave   bbb = Save the local smoothness estimates at each iteration\n"
      "                with dataset prefix 'bbb' [for debugging purposes].\n"
      " -bmall       = Use all blurmaster sub-bricks.\n"
      "                [Default: a subset will be chosen, for speed]\n"
      " -unif        = Uniformize the voxel-wise MAD in the blurmaster AND\n"
      "                input datasets prior to blurring.  Will be restored\n"
      "                in the output dataset.\n"
      " -detrend     = Detrend blurmaster dataset to order NT/30 before starting.\n"
      " -nodetrend   = Turn off detrending of blurmaster.\n"
      "               ** N.B.: '-detrend' is the new default [05 Jun 2007]!\n"
      " -detin       = Also detrend input before blurring it, then retrend\n"
      "                it afterwards. [Off by default]\n"
      " -temper      = Try harder to make the smoothness spatially uniform.\n"
      "\n"
      "-- Author: The Dreaded Emperor Zhark - Nov 2006\n"
     ) ;
     PRINT_COMPILE_DATE ; exit(0) ;
   }

   /*---- official startup ---*/

   PRINT_VERSION("3dBlurToFWHM"); mainENTRY("3dBlurToFWHM main"); machdep();
   AFNI_logger("3dBlurToFWHM",argc,argv);

   /*---- loop over options ----*/

   while( iarg < argc && argv[iarg][0] == '-' ){

     if( strcmp(argv[iarg],"-nodetrend") == 0 ){        /* 05 Jun 2007 */
       corder_bm = corder_in = 0 ;
       iarg++ ; continue ;
     }

     if( strcmp(argv[iarg],"-detin") == 0 ){            /* 05 Jun 2007 */
       corder_bm = corder_in = -1 ; do_unif = 0 ;
       iarg++ ; continue ;
     }

     if( strcmp(argv[iarg],"-detrend") == 0 ){          /* 04 Jun 2007 */
       corder_bm = -1 ; do_unif = 0 ;
       iarg++ ; continue ;
     }

     if( strcmp(argv[iarg],"-rate") == 0 ){
       if( ++iarg >= argc ) ERROR_exit("Need argument after '-rate'") ;
       val = (float)strtod(argv[iarg],NULL) ;
       if( val >= 0.05f && val <= 3.555f ) blurmax = BLURMAX * val ;
       else ERROR_exit("Illegal value after '-rate': '%s'",argv[iarg]) ;
       iarg++ ; continue ;
     }

     if( strncmp(argv[iarg],"-q",2) == 0 ){
       verb = 0 ; iarg++ ; continue ;
     }
     if( strncmp(argv[iarg],"-verb",4) == 0 ){
       verb = 1 ; iarg++ ; continue ;
     }

     if( strncmp(argv[iarg],"-bmal",5) == 0 ){
       bmall = 1 ; iarg++ ; continue ;
     }

     if( strncmp(argv[iarg],"-unif",4) == 0 ){
       do_unif = 1 ; iarg++ ; continue ;
     }
     if( strncmp(argv[iarg],"-nounif",6) == 0 ){
       do_unif = 0 ; iarg++ ; continue ;
     }

     if( strncmp(argv[iarg],"-temper",6) == 0 ){
       temperize = 1 ; iarg++ ; continue ;
     }

     if( strcmp(argv[iarg],"-maxite") == 0 ){
       if( ++iarg >= argc ) ERROR_exit("Need argument after '-maxite'") ;
       maxite = (int)strtod(argv[iarg],NULL) ;
       if( maxite <= 0 ) maxite = 666 ;
       iarg++ ; continue ;
     }

     if( strcmp(argv[iarg],"-input") == 0 || strcmp(argv[iarg],"-dset") == 0 ){
       if( inset != NULL  ) ERROR_exit("Can't have two -input options") ;
       if( ++iarg >= argc ) ERROR_exit("Need argument after '-input'") ;
       inset = THD_open_dataset( argv[iarg] );
       CHECK_OPEN_ERROR(inset,argv[iarg]) ;
       iarg++ ; continue ;
     }

     if( strncmp(argv[iarg],"-blurm",6) == 0 ){
       if( bmset != NULL  ) ERROR_exit("Can't have two -blurmaster options") ;
       if( ++iarg >= argc ) ERROR_exit("Need argument after '-blurmaster'") ;
       bmset = THD_open_dataset( argv[iarg] ) ;
       CHECK_OPEN_ERROR(bmset,argv[iarg]) ;
       iarg++ ; continue ;
     }

     if( strcmp(argv[iarg],"-prefix") == 0 ){
       if( ++iarg >= argc ) ERROR_exit("Need argument after '-prefix'") ;
       prefix = strdup(argv[iarg]) ;
       if( !THD_filename_ok(prefix) ) ERROR_exit("Bad name after '-prefix'") ;
       iarg++ ; continue ;
     }

     if( strcmp(argv[iarg],"-bsave") == 0 ){
       if( ++iarg >= argc ) ERROR_exit("Need argument after '-bsave'") ;
       bsave_prefix = strdup(argv[iarg]) ;
       if( !THD_filename_ok(bsave_prefix) ) ERROR_exit("Bad name after '-bsave'") ;
       iarg++ ; continue ;
     }

     if( strcmp(argv[iarg],"-mask") == 0 ){
       if( ++iarg >= argc ) ERROR_exit("Need argument after '-mask'") ;
       if( mask != NULL || automask ) ERROR_exit("Can't have two mask inputs") ;
       mset = THD_open_dataset( argv[iarg] ) ;
       CHECK_OPEN_ERROR(mset,argv[iarg]) ;
       DSET_load(mset) ; CHECK_LOAD_ERROR(mset) ;
       mask_nx = DSET_NX(mset); mask_ny = DSET_NY(mset); mask_nz = DSET_NZ(mset);
       mask = THD_makemask( mset , 0 , 0.5f, 0.0f ) ; DSET_unload(mset) ;
       if( mask == NULL ) ERROR_exit("Can't make mask from dataset '%s'",argv[iarg]) ;
       nmask = THD_countmask( mask_nx*mask_ny*mask_nz , mask ) ;
       if( verb ) INFO_message("Number of voxels in mask = %d",nmask) ;
       if( nmask < 333 ) ERROR_exit("Mask is too small to process") ;
       iarg++ ; continue ;
     }

     if( strcmp(argv[iarg],"-automask") == 0 ){
       if( mask != NULL ) ERROR_exit("Can't have -automask and -mask") ;
       automask = 1 ; iarg++ ; continue ;
     }

     if( strcmp(argv[iarg],"-nbhd") == 0 ){
       char *cpt ;
       if( ntype  >  0    ) ERROR_exit("Can't have 2 '-nbhd' options") ;
       if( ++iarg >= argc ) ERROR_exit("Need argument after '-nbhd'") ;

       cpt = argv[iarg] ;
       if( strncasecmp(cpt,"SPHERE",6) == 0 ){
         sscanf( cpt+7 , "%f" , &na ) ;
         if( na == 0.0f ) ERROR_exit("Can't have a SPHERE of radius 0") ;
         ntype = NTYPE_SPHERE ;
       } else if( strncasecmp(cpt,"RECT",4) == 0 ){
         sscanf( cpt+5 , "%f,%f,%f" , &na,&nb,&nc ) ;
         if( na == 0.0f && nb == 0.0f && nc == 0.0f )
           ERROR_exit("'RECT(0,0,0)' is not a legal neighborhood") ;
         ntype = NTYPE_RECT ;
       } else if( strcasecmp(cpt,"NULL") == 0 || *cpt == '0' ){
         ntype = NTYPE_NULL ;
       } else {
          ERROR_exit("Unknown -nbhd shape: '%s'",cpt) ;
       }
       iarg++ ; continue ;
     }

     if( strcmp(argv[iarg],"-FWHM") == 0 || strcmp(argv[iarg],"-FHWM") == 0 ){
       if( ++iarg >= argc ) ERROR_exit("Need argument after '%s'",argv[iarg-1]);
       val = (float)strtod(argv[iarg],NULL) ;
       if( val <= 0.0f ) ERROR_exit("Illegal value after '%s': '%s'",
                                    argv[iarg-1],argv[iarg]) ;
       fwhm_goal = val ; fwhm_2D = 0 ;
       iarg++ ; continue ;
     }

     if( strcmp(argv[iarg],"-FWHMxy") == 0 || strcmp(argv[iarg],"-FHWMxy") == 0 ){
       if( ++iarg >= argc ) ERROR_exit("Need argument after '%s'",argv[iarg-1]);
       val = (float)strtod(argv[iarg],NULL) ;
       if( val <= 0.0f ) ERROR_exit("Illegal value after '%s': '%s'",
                                    argv[iarg-1],argv[iarg]) ;
       fwhm_goal = val ; fwhm_2D = 1 ;
       iarg++ ; continue ;
     }

     ERROR_exit("Uknown option '%s'",argv[iarg]) ;

   } /*--- end of loop over options ---*/

   /*----- check for stupid inputs, load datasets, et cetera -----*/

   if( fwhm_goal == 0.0f )
     ERROR_exit("No -FWHM option given! What do you want?") ;

   fwhm_subgoal = 0.95f*fwhm_goal ;  /* stop one axis if it gets 95% of the way */
                                     /* while the other axes need to catch up   */
   blurfac      = blurmax ;

   if( inset == NULL ){
     if( iarg >= argc ) ERROR_exit("No input dataset on command line?") ;
     inset = THD_open_dataset( argv[iarg] ) ;
     CHECK_OPEN_ERROR(inset,argv[iarg]) ;
   }
   nvox = DSET_NVOX(inset)     ;
   dx   = fabs(DSET_DX(inset)) ; if( dx == 0.0f ) dx = 1.0f ;
   dy   = fabs(DSET_DY(inset)) ; if( dy == 0.0f ) dy = 1.0f ;
   dz   = fabs(DSET_DZ(inset)) ; if( dz == 0.0f ) dz = 1.0f ;
   dmin = MIN(dx,dy) ; if( !fwhm_2D ) dmin = MIN(dmin,dz) ;
   xrat = dmin/dx ; xrat = xrat*xrat ;
   yrat = dmin/dy ; yrat = yrat*yrat ;
   zrat = dmin/dz ; zrat = zrat*zrat ;

   if( bmset == NULL ){
     bmset = inset ; bmeqin = 1 ;
     if( verb ) INFO_message("Using input dataset as blurmaster") ;
   }

   if( DSET_NX(inset) != DSET_NX(bmset) ||
       DSET_NY(inset) != DSET_NY(bmset) ||
       DSET_NZ(inset) != DSET_NZ(bmset)   )
     ERROR_exit("-blurmaster dataset grid doesn't match input dataset") ;

   if( DSET_NZ(inset) == 1 && !fwhm_2D ){
     WARNING_message("Dataset is 2D ==> switching from -FWHM to -FWHMxy") ;
     fwhm_2D = 1 ;
   }

   if( maxite <= 0 ){
     if( fwhm_2D ) maxite = (int)( 66.6 * fwhm_goal / sqrt(dx*dy) ) ;
     else          maxite = (int)( 66.6 * fwhm_goal / cbrt(dx*dy*dz) ) ;
     if( maxite < 66 ) maxite = 66 ;
     INFO_message("Max number iterations set to %d",maxite) ;
   }

   /*--- deal with mask or automask ---*/

   if( mask != NULL ){
     if( mask_nx != DSET_NX(inset) ||
         mask_ny != DSET_NY(inset) ||
         mask_nz != DSET_NZ(inset)   )

       ERROR_exit("-mask dataset grid doesn't match input dataset") ;

   } else if( automask ){
     mask = THD_automask( inset ) ;
     if( mask == NULL )
       ERROR_message("Can't create -automask from input dataset?") ;
     nmask = THD_countmask( DSET_NVOX(inset) , mask ) ;
     if( verb ) INFO_message("Number of voxels in automask = %d",nmask);
     if( nmask < 333 ) ERROR_exit("Automask is too small to process") ;

   } else {
     mask = (byte *)malloc(sizeof(byte)*nvox) ; nmask = nvox ;
     memset(mask,1,sizeof(byte)*nvox) ;
     if( verb ) INFO_message("No mask ==> processing all %d voxels",nvox);
   }

   /*---- create neighborhood -----*/

   if( ntype == 0 ){
     ntype = NTYPE_SPHERE ; na = (fwhm_2D) ? -6.0f : -4.0f ;
     if( verb ) INFO_message("Using default neighborhood 'SPHERE(%.1f)'",na);
   }

   switch( ntype ){
     default:
       ERROR_exit("WTF?  ntype=%d",ntype) ;   /* should not happen */

     case NTYPE_NULL: nbhd = NULL ; break ;

     case NTYPE_SPHERE:{
       float ddx , ddy , ddz ;
       if( na < 0.0f ){ ddx = ddy = ddz = 1.0f ; na = -na ; }
       else           { ddx = dx ; ddy = dy ; ddz = dz ;    }
       if( fwhm_2D ) ddz = 1.e+10 ;   /* 2D only */
       nbhd = MCW_spheremask( ddx,ddy,ddz , na ) ;
     }
     break ;

     case NTYPE_RECT:{
       float ddx , ddy , ddz ;
       if( na < 0.0f ){ ddx = 1.0f; na = -na; } else ddx = dx ;
       if( nb < 0.0f ){ ddy = 1.0f; nb = -nb; } else ddy = dy ;
       if( nc < 0.0f ){ ddz = 1.0f; nc = -nc; } else ddz = dz ;
       nbhd = MCW_rectmask( ddx,ddy,ddz , na,nb,nc ) ;
     }
     break ;
   }

   if( nbhd != NULL ){
     if( verb || nbhd->num_pt < 19)
       INFO_message("Neighborhood comprises %d voxels",nbhd->num_pt) ;
     if( nbhd->num_pt < 19 )
       ERROR_exit("FWHM estimation requires neighborhood of at least 19 voxels!") ;
   } else {
     if( bsave_prefix != NULL ){
       WARNING_message("Option '-bsave' means nothing with '-nbhd NULL'") ;
       bsave_prefix = NULL ;
     }
     if( temperize ){
       WARNING_message("Option '-temper' means nothing with '-nbhd NULL'") ;
       temperize = 0 ;
     }
   }

   /*--- process blurmaster dataset to produce blur master image array ---*/

   DSET_load(bmset) ; CHECK_LOAD_ERROR(bmset) ;
   INIT_IMARR(bmar) ;

   if( corder_bm ) do_unif = 0 ;
   if( corder_bm < 0 ) corder_bm = DSET_NVALS(bmset) / 30 ;
#if 0
   if( corder_bm > 0 && 2*corder_bm+3 >= DSET_NVALS(bmset) )
     ERROR_exit("-corder %d is too big for blurmaster dataset",corder_bm) ;
#endif

   if( DSET_NVALS(bmset) == 1 ){

     bmed = THD_median_brick(bmset) ;   /* scaled to floats, if need be */
     if( mri_allzero(bmed) )
       ERROR_exit("blurmaster dataset has 1 sub-brick, which is all zero!") ;
     ADDTO_IMARR(bmar,bmed) ;
     if( do_unif )
       WARNING_message("Can't apply -unif: only 1 sub-brick in blurmaster dataset") ;
     else if( corder_bm > 0 ){
       WARNING_message("Can't apply -detrend: only 1 sub-brick in blurmaster dataset") ;
       corder_bm = 0 ;
     }

   } else {

     MRI_IMAGE *smed ;
     float *mar , *sar ;
     int ntouse , nvb=DSET_NVALS(bmset) , ibot , idel , nzs ;
     int *az=(int *)malloc(sizeof(int)*nvb) , naz=0 , jbm , jj ;

     for( jj=0 ; jj < nvb ; jj++ ){
       az[jj] = mri_allzero(DSET_BRICK(bmset,jj)); if( az[jj] ) naz++;
     }
     if( corder_bm > 0 && naz > 0 ){                       /* 04 Nov 2008 */
       corder_bm = 0 ;
       WARNING_message("blurmaster detrend disabled: %d all zero sub-bricks found",naz) ;
     }

     if( corder_bm > 0 ){                                  /*--- 04 Jun 2007 ---*/
       int nref=2*corder_bm+3, iv,kk ;
       float **ref , tm,fac,fq ;
       THD_3dim_dataset *newset ;
       INFO_message("detrending blurmaster: %d ref funcs, %d time points",nref,nvb) ;

       ref = THD_build_trigref( corder_bm , nvb ) ;
       if( ref == NULL ) ERROR_exit("THD_build_trigref failed!") ;

       newset = THD_detrend_dataset( bmset, nref,ref , 2,1 , mask , NULL ) ;
       if( newset == NULL ) ERROR_exit("detrending failed!?") ;

       if( !bmeqin ) DSET_delete(bmset) ;
       bmset = newset ;
       ININFO_message("detrending of blurmaster complete") ;

       for( jj=0 ; jj < nref ; jj++ ) free(ref[jj]) ;
       free(ref) ;
     }

     imar = THD_medmad_bricks(bmset) ;
     bmed = IMARR_SUBIM(imar,0) ;        /* 11 Dec 2006: -do_unif: */
     smed = IMARR_SUBIM(imar,1) ;        /* normalize each voxel   */
     mar  = MRI_FLOAT_PTR(bmed) ;        /* by 1/MAD, to allow for */
     sar  = MRI_FLOAT_PTR(smed) ;        /* spatial variability    */

     for( nzs=ii=0 ; ii < nvox ; ii++ )                 /* 10 May 2007 */
       if( mask[ii] && sar[ii] == 0.0f ){ mask[ii] = 0; nzs++ ; }
     if( nzs > 0 )
       INFO_message("Removed %d voxels with 0 MAD in blurmaster from mask",nzs) ;

     if( do_unif )
       for( ii=0 ; ii < nvox ; ii++ ) if( sar[ii] != 0.0f ) sar[ii] = 1.0/sar[ii] ;

     if( nbhd != NULL ){
       ntouse = (int)ceil(9999.9999/nbhd->num_pt) ;
       ntouse = MAX(2,ntouse) ;
       ntouse = MIN(nvb,ntouse) ;
     } else {
       ntouse = MIN(nvb,32) ;
     }
     if( bmall ) ntouse = nvb ;
     idel = nvb / ntouse ;
     ibot = (nvb-1 - idel*(ntouse-1)) / 2 ;
     if( verb ) INFO_message("Using blurmaster sub-bricks [%d..%d(%d)]",
                             ibot,ibot+(ntouse-1)*idel,idel);
     for( ibm=ibot ; ibm < nvb && IMARR_COUNT(bmar) < ntouse  ; ibm+=idel ){
       jbm = ibm ;
       if( az[jbm] ){
         if( idel == 1 ){
           WARNING_message("skip blurmaster sub-brick #%d because it is all zero",ibm) ;
           continue ;
         }
         for( jbm=ibm-1 ; jbm >= 0 && jbm > ibm-idel && az[jbm] ; jbm-- ) ; /*nada*/
         if( jbm < 0 || jbm == ibm-idel || az[jbm] ){
           WARNING_message("skip blurmaster sub-brick #%d because it is all zero",ibm) ;
           continue ;
         }
         if( verb )
           INFO_message("replace blurmaster all zero sub-brick #%d with #%d",ibm,jbm) ;
       }
       bmim = mri_scale_to_float(DSET_BRICK_FACTOR(bmset,jbm),DSET_BRICK(bmset,jbm));
       bar  = MRI_FLOAT_PTR(bmim) ;
       if( do_unif )
         for( ii=0 ; ii < nvox ; ii++ ) bar[ii] = (bar[ii]-mar[ii])*sar[ii] ;
       else if( corder_bm == 0 )
         for( ii=0 ; ii < nvox ; ii++ ) bar[ii] = bar[ii]-mar[ii] ;
       ADDTO_IMARR(bmar,bmim) ;
       if( !bmeqin ) DSET_unload_one(bmset,jbm) ;
     }
     DESTROY_IMARR(imar) ;  /* median and MAD bricks */
     free(az) ;
     if( IMARR_COUNT(bmar) == 0 )
       ERROR_exit("Can't create blurmaster subset collection? Help me, Mr. Wizard!") ;
   }

   if( !bmeqin ) DSET_unload(bmset) ;
   for( ibm=0 ; ibm < IMARR_COUNT(bmar) ; ibm++ ){
     IMARR_SUBIM(bmar,ibm)->dx = dx ;
     IMARR_SUBIM(bmar,ibm)->dy = dy ;
     IMARR_SUBIM(bmar,ibm)->dz = dz ;
   }

   /*--- pre-process input dataset ---*/

#if 1
   if( corder_in < 0 ) corder_in = DSET_NVALS(inset) / 30 ;
#else
   corder_in = 0 ;
#endif

   DSET_load(inset) ; CHECK_LOAD_ERROR(inset) ;

   outset = EDIT_empty_copy( inset ) ;   /* moved here 04 Jun 2007 */
   EDIT_dset_items( outset , ADN_prefix , prefix , ADN_none ) ;
   EDIT_dset_items( outset , ADN_brick_fac , NULL , ADN_none ) ;  /* 11 Sep 2007 */
   tross_Copy_History( inset , outset ) ;
   tross_Make_History( "3dBlurToFWHM" , argc,argv , outset ) ;

   if( corder_in > 0 ){  /* 04 Jun 2007 -- detrend input (but don't scale) */

     int jj,iv,kk , nvi = DSET_NVALS(inset) , nzs ;
     float tm,fac,fq , *vv,*ww , vmed ;
     THD_3dim_dataset *newset ;

     corder_inrefnum = 2*corder_in+3 ;
     INFO_message("detrending input: %d ref funcs, %d time points",corder_inrefnum,nvi) ;

     corder_inref = THD_build_trigref( corder_in , nvi ) ;
     if( corder_inref == NULL ) ERROR_exit("THD_build_trigref failed!") ;

     newset = THD_detrend_dataset( inset, corder_inrefnum,corder_inref ,
                                   2,0 , mask , &corder_inar ) ;
     if( newset == NULL ) ERROR_exit("detrending failed!?") ;
     corder_invv = IMARR_SUBIM(corder_inar,corder_inrefnum) ;
     vv = MRI_FLOAT_PTR(corder_invv) ;
     ww = (float *)malloc(sizeof(float)*nvox) ;
     for( ii=jj=0 ; ii < nvox ; ii++ ) if( mask[ii] ) ww[jj++] = vv[ii] ;
     vmed = 0.05 * qmed_float(jj,ww) ; free(ww) ;
     for( nzs=ii=0 ; ii < nvox ; ii++ )
       if( mask[ii] && vv[ii] < vmed ){ mask[ii] = 0; nzs++ ; }
     if( nzs > 0 )
       INFO_message("Removed %d voxels with small input stdev from mask",nzs) ;

     DSET_delete(inset) ; inset = newset ;
     ININFO_message("detrending of input complete") ;

   } else if( do_unif ){

     int nvi = DSET_NVALS(inset) ;
     if( nvi < 3 ){
       WARNING_message(
         "Can't apply -unif: only %d sub-brick%s in input dataset" ,
         nvi , (nvi==1) ? "\0" : "s" ) ;
       do_unif = 0 ;
     } else {
       imar = THD_medmad_bricks(inset) ;
       imed = IMARR_SUBIM(imar,0) ; imedar = MRI_FLOAT_PTR(imed) ;
       imad = IMARR_SUBIM(imar,1) ; imadar = MRI_FLOAT_PTR(imad) ;
       for( ii=0 ; ii < nvox ; ii++ )
         if( imadar[ii] != 0.0f ) imadar[ii] = 1.0f/imadar[ii] ;
     }
   }
   INIT_IMARR(dsar) ;
   for( ids=0 ; ids < DSET_NVALS(inset) ; ids++ ){
     dsim = mri_scale_to_float(DSET_BRICK_FACTOR(inset,ids),DSET_BRICK(inset,ids));
     if( do_unif ){
       bar = MRI_FLOAT_PTR(dsim) ;
       for( ii=0 ; ii < nvox ; ii++ ) bar[ii] = (bar[ii]-imedar[ii])*imadar[ii] ;
     }
     ADDTO_IMARR(dsar,dsim) ; DSET_unload_one(inset,ids) ;
   }
   if( do_unif ){
     for( ii=0 ; ii < nvox ; ii++ )
       if( imadar[ii] != 0.0f ) imadar[ii] = 1.0f/imadar[ii] ;
   }
   DSET_unload(inset) ;
   for( ids=0 ; ids < IMARR_COUNT(dsar) ; ids++ ){
     IMARR_SUBIM(dsar,ids)->dx = dx ;
     IMARR_SUBIM(dsar,ids)->dy = dy ;
     IMARR_SUBIM(dsar,ids)->dz = dz ;
   }

   /*--- make arrays for FWHM estimates and then diffusion parameters ---*/

   numfxyz = 0 ;
   fxim = mri_new_conforming( IMARR_SUBIM(bmar,0) , MRI_float ) ;
   fxar = MRI_FLOAT_PTR(fxim) ;
   gx   = fwhm_goal - 0.011f*dx ; numfxyz++ ;
   hx   = 0.8f*gx ; qx = 1.0f/(gx-hx) ;

   fyim = mri_new_conforming( IMARR_SUBIM(bmar,0) , MRI_float ) ;
   fyar = MRI_FLOAT_PTR(fyim) ;
   gy   = fwhm_goal - 0.011f*dy ; numfxyz++ ;
   hy   = 0.8f*gy ; qy = 1.0f/(gy-hy) ;

   if( !fwhm_2D ){
     fzim = mri_new_conforming( IMARR_SUBIM(bmar,0) , MRI_float ) ;
     fzar = MRI_FLOAT_PTR(fzim) ;
     gz   = fwhm_goal - 0.011f*dz ; numfxyz++ ;
     hz   = 0.8f*gz ; qz = 1.0f/(gz-hz) ;
   }
   maxfxyz    = blurfac / numfxyz ;  /* maximum lambda */
#if 0
   fwhm_goal *= 0.995f ;            /* fudge factor */
#endif

   /*----------- Do the work:
                   estimate smoothness of blur master
                     - exit loop when all done
                   set up incremental blurring parameters
                   blur the master and the input
                   loop back to top                       --------------*/

   for( nite=1 ; nite <= maxite ; nite++ ){

     /*--- global smoothness estimation into bx,by,bz ---*/

     fw = mriarr_estimate_FWHM_1dif( bmar , mask , do_unif ) ;

     UNLOAD_FVEC3(fw,bx,by,bz) ;
     if( bx <= 0.0f ) bx = dx ;  /* should not happen */
     if( by <= 0.0f ) by = dy ;
     if( bz <= 0.0f ) bz = dz ;

     /*--- test for progress towards our goal ---*/

     if( fwhm_2D ){
       val = (float)sqrt(bx*by) ;
       if( verb )
         INFO_message("-- Iteration #%d: 2D FWHMx=%.4f FWHMy=%.4f sqrt()=%.4f",
                      nite,bx,by,val) ;
       if( val >= fwhm_goal ){
         if( verb ) INFO_message("** Passes 2D threshold sqrt(FWHMx*FWHMy) ==> done!");
         break;
       }
       if( nite > 3 && bx < last_fwx && by < last_fwy ){  /* negative progress? */
         nbail++ ;
         if( nbail == BAILOUT ){
           if( verb ) INFO_message("** Bailing out for sluggish progress!") ;
           break ;
         } else {
           if( verb ) INFO_message("** Progress is slow for some reason?!") ;
         }
       } else nbail = 0 ;
       xdone   = (bx >= fwhm_goal) ;  /* is x done? */
       ydone   = (by >= fwhm_goal) ;  /* is y done? */
       zdone   = 1 ;
       xalmost = (!xdone && bx >= DFGOALC*fwhm_goal) ;  /* is x 80% done? */
       yalmost = (!ydone && by >= DFGOALC*fwhm_goal) ;  /* is y 80% done? */
       zalmost = 0 ;
       xstall  = (!xdone && (bx < last_fwx && nbail==BAILOUT)) ;  /* is x going backwards? */
       ystall  = (!ydone && (by < last_fwy && nbail==BAILOUT)) ;  /* is y going backwards? */
       zstall  = 1 ;
     } else {
       val = (float)cbrt(bx*by*bz) ;
       if( verb )
         INFO_message("-- Iteration #%d: 3D FWHMx=%.4f FWHMy=%.4f FWHMz=%.4f cbrt()=%.4f",
                      nite,bx,by,bz,val) ;
       if( val >= fwhm_goal ){
         if( verb )
           INFO_message("** Passes 3D threshold cbrt(FWHMx*FWHMy*FWHMz) ==> done!");
         break;
       }
       if( nite > 3 && bx < last_fwx && by < last_fwy && bz < last_fwz ){  /* negative progress? */
         nbail++ ;
         if( nbail == BAILOUT ){
           if( verb ) INFO_message("** Bailing out for sluggish progress!") ;
           break ;
         } else {
           if( verb ) INFO_message("** Progress is slow for some reason?!") ;
         }
       } else nbail = 0 ;
       xdone   = (bx >= fwhm_goal) ;                     /* is x done? */
       ydone   = (by >= fwhm_goal) ;                     /* is y done? */
       zdone   = (bz >= fwhm_goal) ;                     /* is z done? */
       xalmost = (!xdone && bx >= DFGOALC*fwhm_goal) ;   /* is x 80% done? */
       yalmost = (!ydone && by >= DFGOALC*fwhm_goal) ;   /* is y 80% done? */
       zalmost = (!zdone && bz >= DFGOALC*fwhm_goal) ;   /* is z 80% done? */
       xstall  = (!xdone && (bx < last_fwx && nbail==BAILOUT)) ; /* is x going backwards? */
       ystall  = (!ydone && (by < last_fwy && nbail==BAILOUT)) ; /* is y going backwards? */
       zstall  = (!zdone && (bz < last_fwz && nbail==BAILOUT)) ; /* is z going backwards? */
     }

     /* these cases should not happen, but just in case ... */
     if( xdone && ydone && zdone ){
       if( verb ) INFO_message("** All axes done!") ;
       break ;
     }
     if( xstall && ystall && zstall ){
       if( verb ) INFO_message("** All axes stalled ==> quitting in disgust") ;
       break ;
     }

     /** if one axis is very close but others still need work, stop that one now **/

     if( fwhm_2D && !xdone && !ydone ){
            if( by > 1.01f*bx && by >= fwhm_subgoal ) ydone = 1;  /* stop y */
       else if( bx > 1.01f*by && bx >= fwhm_subgoal ) xdone = 1;  /* stop y */
     } else if( !fwhm_2D && !xdone && !ydone && !zdone ){
            if( bz > 1.01f*by & bz > 1.01f*bx && bz >= fwhm_subgoal ) zdone = 1; /* stop z */
       else if( by > 1.01f*bx & by > 1.01f*bz && by >= fwhm_subgoal ) ydone = 1; /* stop y */
       else if( bx > 1.01f*by & bx > 1.01f*bz && bx >= fwhm_subgoal ) xdone = 1; /* stop x */
     }

     /** if global blurring is going too fast
         (more than 20% progress per iteration), slow it down **/

     if( nite > 1 ){
       float bfac=1.0f , dfg=DFGOAL*fwhm_goal ;
       delt_fwx = bx-last_fwx; delt_fwy = by-last_fwy; delt_fwz = bz-last_fwz;
       if( !xdone && !xstall && delt_fwx > 0.0f && (val=dfg/delt_fwx) < 1.0f )
         bfac = MIN( bfac , val ) ;
       if( !ydone && !ystall && delt_fwy > 0.0f && (val=dfg/delt_fwy) < 1.0f )
         bfac = MIN( bfac , val ) ;
       if( !zdone && !zstall && delt_fwz > 0.0f && (val=dfg/delt_fwz) < 1.0f )
         bfac = MIN( bfac , val ) ;
       bfac = MAX(0.123f,bfac) ; blurfac *= bfac ;
       maxfxyz = blurfac / numfxyz ;
       if( bfac < 0.999f )
         if( verb )
           ININFO_message(" Slowing overall blur rate by factor of %.3f",bfac) ;
     }

     /** if near the goal in any direction, slow that direction's blurring down **/

     maxfx = maxfxyz * xrat ;
     if( xdone || xstall ){
       maxfx = 0.0f; xstopped = 1;
       if( verb ) ININFO_message(" x-blur is stopped");
     } else if( xalmost ){
       val = (nite==1) ? 0.123f : sqrt((fwhm_goal-bx)/(DFGOAL*fwhm_goal)) ;
       if( val >= 0.0f && val < 0.999f ){
         val = MAX(0.09f,val) ;
         if( verb ) ININFO_message(" Slowing x-blur rate by factor of %.3f",val) ;
         maxfx *= val ;
       }
     }

     maxfy = maxfxyz * yrat ;
     if( ydone || ystall ){
       maxfy = 0.0f; ystopped = 1;
       if( verb ) ININFO_message(" y-blur is stopped");
     } else if( yalmost ){
       val = (nite==1) ? 0.123f : sqrt((fwhm_goal-by)/(DFGOAL*fwhm_goal)) ;
       if( val >= 0.0f && val < 0.999f ){
         val = MAX(0.09f,val) ;
         if( verb ) ININFO_message(" Slowing y-blur rate by factor of %.3f",val) ;
         maxfy *= val ;
       }
     }

     maxfz = maxfxyz * zrat ;
     if( zdone || zstall ){
       maxfz = 0.0f ; zstopped = 1;
       if( !fwhm_2D && verb ) ININFO_message(" z-blur is stopped");
     } else if( zalmost ){
       val = (nite==1) ? 0.123f : sqrt((fwhm_goal-bz)/(DFGOAL*fwhm_goal)) ;
       if( val >= 0.0f && val < 0.999f ){
         val = MAX(0.09f,val) ;
         if( verb ) ININFO_message(" Slowing z-blur rate by factor of %.3f",val) ;
         maxfz *= val ;
       }
     }

     last_fwx = bx; last_fwy = by; last_fwz = bz;

     /*--- blur map estimation (within each nbhd) ---*/

     if( nbhd != NULL ){
       if( verb ) ININFO_message(" Estimate smoothness in each voxel's nbhd") ;
       estimate_blur_map( bmar , mask,nbhd , fxar,fyar,fzar ) ;

       /*--- Save blur maps for debuggers? ---*/

       if( bsave_prefix != NULL ){
         char bp[1024]; THD_3dim_dataset *bset; float *qqq;
         { float *bbb=MRI_FLOAT_PTR( IMARR_SUBIM(bmar,0) ) ;
           qqq = malloc(sizeof(float)*nvox); memcpy(qqq,bbb,sizeof(float)*nvox);
           bset = EDIT_empty_copy(inset); sprintf(bp,"%s_%04d_bm",bsave_prefix,nite);
           EDIT_dset_items( bset, ADN_prefix,bp, ADN_nvals,1, ADN_ntt,0, ADN_none );
           EDIT_substitute_brick( bset , 0 , MRI_float , qqq ) ;
           EDIT_BRICK_LABEL( bset , 0 , "Bmar0" ) ;
           DSET_write(bset) ; WROTE_DSET(bset) ; DSET_delete(bset) ;
         }
         if( fxar != NULL ){
           qqq = malloc(sizeof(float)*nvox); memcpy(qqq,fxar,sizeof(float)*nvox);
           bset = EDIT_empty_copy(inset); sprintf(bp,"%s_%04dx",bsave_prefix,nite);
           EDIT_dset_items( bset, ADN_prefix,bp, ADN_nvals,1, ADN_ntt,0, ADN_none );
           EDIT_substitute_brick( bset , 0 , MRI_float , qqq ) ;
           EDIT_BRICK_LABEL( bset , 0 , "FWHMx" ) ;
           DSET_write(bset) ; WROTE_DSET(bset) ; DSET_delete(bset) ;
         }
         if( fyar != NULL ){
           qqq = malloc(sizeof(float)*nvox); memcpy(qqq,fyar,sizeof(float)*nvox);
           bset = EDIT_empty_copy(inset); sprintf(bp,"%s_%04dy",bsave_prefix,nite);
           EDIT_dset_items( bset, ADN_prefix,bp, ADN_nvals,1, ADN_ntt,0, ADN_none );
           EDIT_substitute_brick( bset , 0 , MRI_float , qqq ) ;
           EDIT_BRICK_LABEL( bset , 0 , "FWHMy" ) ;
           DSET_write(bset) ; WROTE_DSET(bset) ; DSET_delete(bset) ;
         }
         if( fzar != NULL ){
           qqq = malloc(sizeof(float)*nvox); memcpy(qqq,fzar,sizeof(float)*nvox);
           bset = EDIT_empty_copy(inset); sprintf(bp,"%s_%04dz",bsave_prefix,nite);
           EDIT_dset_items( bset, ADN_prefix,bp, ADN_nvals,1, ADN_ntt,0, ADN_none );
           EDIT_substitute_brick( bset , 0 , MRI_float , qqq ) ;
           EDIT_BRICK_LABEL( bset , 0 , "FWHMz" ) ;
           DSET_write(bset) ; WROTE_DSET(bset) ; DSET_delete(bset) ;
         }
       }

       /*--- compute local blurring parameters where needed ---*/

       /* temper the blurring rate depending on the local smoothness? */

#undef  TB
#undef  TT
#undef  TR
#define TB 0.8f
#define TT 0.2f
#define TR 0.05f
       if( temperize ){
         float_pair qp; MRI_IMAGE *qim; float *qar; int ng;
         temper_fx = temper_fy = temper_fz = 0 ;
         if( fxim != NULL && !xstopped ){
           for( ii=ng=0 ; ii < nvox ; ii++ )
             if( mask[ii] == 1 && fxar[ii] > 0.0f && fxar[ii] < gx ) ng++ ;
           if( ng >= 666 ){
             qim = mri_new_vol( ng,1,1 , MRI_float ); qar = MRI_FLOAT_PTR(qim);
             for( ii=ng=0 ; ii < nvox ; ii++ )
               if( mask[ii] == 1 && fxar[ii] > 0.0f && fxar[ii] < gx )
                 qar[ng++] = fxar[ii] ;
             qp = mri_twoquantiles( qim , 0.15f , 0.85f ) ;
             fx_tbot = qp.a ; fx_ttop = qp.b ; mri_free(qim) ;
             fx_trat = fx_ttop - fx_tbot ;
             temper_fx = (fx_trat > 0.0f) && (fx_trat > TR*gx) ;
             if( temper_fx ) fx_trat = TB / fx_trat ;
             if( verb ) ININFO_message(" temper x: %.4f .. %.4f %s",
                                       fx_tbot,fx_ttop,temper_fx?"ON":"OFF") ;
           }
         }
         if( fyim != NULL && !ystopped ){
           for( ii=ng=0 ; ii < nvox ; ii++ )
             if( mask[ii] == 1 && fyar[ii] > 0.0f && fyar[ii] < gy ) ng++ ;
           if( ng >= 666 ){
             qim = mri_new_vol( ng,1,1 , MRI_float ); qar = MRI_FLOAT_PTR(qim);
             for( ii=ng=0 ; ii < nvox ; ii++ )
               if( mask[ii] == 1 && fyar[ii] > 0.0f && fyar[ii] < gy )
                 qar[ng++] = fyar[ii] ;
             qp = mri_twoquantiles( qim , 0.15f , 0.85f ) ;
             fy_tbot = qp.a ; fy_ttop = qp.b ; mri_free(qim) ;
             fy_trat = fy_ttop - fy_tbot ;
             temper_fy = (fy_trat > 0.0f) && (fy_trat > TR*gy) ;
             if( verb ) ININFO_message(" temper y: %.4f .. %.4f %s",
                                       fy_tbot,fy_ttop,temper_fy?"ON":"OFF") ;
           }
         }
         if( fzim != NULL && !zstopped ){
           for( ii=ng=0 ; ii < nvox ; ii++ )
             if( mask[ii] == 1 && fzar[ii] > 0.0f && fzar[ii] < gz ) ng++ ;
           if( ng >= 666 ){
             qim = mri_new_vol( ng,1,1 , MRI_float ); qar = MRI_FLOAT_PTR(qim);
             for( ii=ng=0 ; ii < nvox ; ii++ )
               if( mask[ii] == 1 && fzar[ii] > 0.0f && fzar[ii] < gz )
                 qar[ng++] = fzar[ii] ;
             qp = mri_twoquantiles( qim , 0.15f , 0.85f ) ;
             fz_tbot = qp.a ; fz_ttop = qp.b ; mri_free(qim) ;
             fz_trat = fz_ttop - fz_tbot ;
             temper_fz = (fz_trat > 0.0f) && (fz_trat > TR*gz) ;
             if( verb ) ININFO_message(" temper z: %.4f .. %.4f %s",
                                       fz_tbot,fz_ttop,temper_fz?"ON":"OFF") ;
           }
         }
       }

       nblur = 0 ;
       for( ii=0 ; ii < nvox ; ii++ ){
         if( mask[ii] == 1 ){
           nd = 0 ;
           if( fxar != NULL ){
             if( fxar[ii] <= 0.0f || fxar[ii] >= gx )
               fxar[ii] = 0.0f ;
             else if( !temper_fx )
               fxar[ii] = (fxar[ii] <= hx) ? maxfx : maxfx*qx*(gx-fxar[ii]) ;
             else if( fxar[ii] <= fx_tbot )
               fxar[ii] = maxfx ;
             else if( fxar[ii] >= fx_ttop )
               fxar[ii] = TT*maxfx ;
             else
               fxar[ii] = (TT+fx_trat*(fx_ttop-fxar[ii]))*maxfx ;

             if( fxar[ii] > 0.0f ) nd++ ;
           }
           if( fyar != NULL ){
             if( fyar[ii] <= 0.0f || fyar[ii] >= gy )
               fyar[ii] = 0.0f ;
             else if( !temper_fy )
               fyar[ii] = (fyar[ii] <= hy) ? maxfy : maxfy*qy*(gy-fyar[ii]) ;
             else if( fyar[ii] <= fy_tbot )
               fyar[ii] = maxfy ;
             else if( fyar[ii] >= fy_ttop )
               fyar[ii] = TT*maxfy ;
             else
               fyar[ii] = (TT+fy_trat*(fy_ttop-fyar[ii]))*maxfy ;

             if( fyar[ii] > 0.0f ) nd++ ;
           }
           if( fzar != NULL ){
             if( fzar[ii] <= 0.0f || fzar[ii] >= gz )
               fzar[ii] = 0.0f ;
             else if( !temper_fz )
               fzar[ii] = (fzar[ii] <= hz) ? maxfz : maxfz*qz*(gz-fzar[ii]) ;
             else if( fzar[ii] <= fz_tbot )
               fzar[ii] = maxfz ;
             else if( fzar[ii] >= fz_ttop )
               fzar[ii] = TT*maxfz ;
             else
               fzar[ii] = (TT+fz_trat*(fz_ttop-fzar[ii]))*maxfz ;

             if( fzar[ii] > 0.0f ) nd++ ;
           }
           if( nd == 0 ) mask[ii] = 2 ;   /* turn off future diffusion here */
           else          nblur++ ;
         }
       }
       if( nblur == 0 ) break ;   /* no blurring ==> done?! */

     } else {  /* no nbhd ==> blur all non-mask points equally */
       nblur = 0 ;
       for( ii=0 ; ii < nvox ; ii++ ){
         if( fxar != NULL ) fxar[ii] = (mask[ii]) ? maxfx : 0.0f ;
         if( fyar != NULL ) fyar[ii] = (mask[ii]) ? maxfy : 0.0f ;
         if( fzar != NULL ) fzar[ii] = (mask[ii]) ? maxfz : 0.0f ;
         if( mask[ii] ) nblur++ ;
       }
     }

     /*--- blur the master and the input ---*/

     if( verb ) ININFO_message(" Blurring %d voxels in master",nblur) ;
     for( ii=0 ; ii < IMARR_COUNT(bmar) ; ii++ )
       mri_blur3D_variable( IMARR_SUBIM(bmar,ii) , mask , fxim,fyim,fzim ) ;

     if( verb ) ININFO_message(" Blurring input") ;
     for( ii=0 ; ii < IMARR_COUNT(dsar) ; ii++ )
       mri_blur3D_variable( IMARR_SUBIM(dsar,ii) , mask , fxim,fyim,fzim ) ;

#if 0
     (void)mriarr_estimate_FWHM_1dif( dsar , mask , do_unif ) ;
#endif

   } /*-- loop back to estimate smoothness, etc --*/

   /*----- toss some trash ---*/

   DESTROY_IMARR(bmar) ;

   if( fxim != NULL ) mri_free(fxim) ;
   if( fyim != NULL ) mri_free(fyim) ;
   if( fzim != NULL ) mri_free(fzim) ;

   /*--- put blurred data into the output dataset ---*/

   for( ids=0 ; ids < DSET_NVALS(outset) ; ids++ ){
     bar = MRI_FLOAT_PTR( IMARR_SUBIM(dsar,ids) ) ;
     if( do_unif ){
       for( ii=0 ; ii < nvox ; ii++ )
         if( mask[ii] ) bar[ii] = bar[ii]*imadar[ii] + imedar[ii] ;
     }
     EDIT_substitute_brick( outset , ids , MRI_float , bar ) ;
   }
#if 1
   if( corder_inrefnum > 0 ){
     INFO_message("re-trending output dataset") ;
     THD_retrend_dataset( outset , corder_inrefnum,corder_inref ,
                          0 , mask , corder_inar ) ;
   }
#endif
   DSET_write(outset) ;
   WROTE_DSET(outset) ;

   /*--- 3dFWHMx ---*/

   if( verb ){
     char *buf , *pg ;
     pg  = THD_find_executable("3dFWHMx") ;
     buf = malloc(     ((pg != NULL) ? strlen(pg)+999 : 999      ) ) ;
     sprintf(buf,"%s", ((pg != NULL) ? pg             : "3dFWHMx") ) ;
     sprintf(buf+strlen(buf)," -arith") ;
     if( do_unif )
       sprintf(buf+strlen(buf)," -unif") ;
     else if( corder_bm > 0 )
       sprintf(buf+strlen(buf)," -detrend") ;
     if( automask )
       sprintf(buf+strlen(buf)," -automask") ;
     else if( mset != NULL )
       sprintf(buf+strlen(buf)," -mask %s",DSET_BRIKNAME(mset)) ;
     sprintf(buf+strlen(buf)," %s",DSET_BRIKNAME(outset)) ;
     if( pg != NULL ){
         INFO_message("Checking results by running command below:") ;
       ININFO_message(" %s",buf) ;
       (void)system(buf) ;
     } else {
         INFO_message("To check results, run the command below:") ;
       ININFO_message(" %s",buf) ;
     }
     free(buf) ;
   }
   exit(0) ;
}

/*----------------------------------------------------------------------------*/

#undef  ASSIF
#define ASSIF(f,i,v) if(f != NULL)f[i]=v

void estimate_blur_map( MRI_IMARR *bmar , byte *mask  , MCW_cluster *nbhd ,
                        float *fxar     , float *fyar , float *fzar        )
{
   int ibm , ii , nvox , nar , nx,nxy , xx,yy,zz , pp ;
   THD_fvec3 fw ;
   float  fxx ,  fyy , fzz ;
   int   nfxx , nfyy , nfzz;
   float  vxx ,  vyy ,  vzz;

ENTRY("estimate_blur_map") ;

   nvox = IMARR_SUBIM(bmar,0)->nvox ;
   nar  = IMARR_COUNT(bmar) ;
   nx   = IMARR_SUBIM(bmar,0)->nx ;
   nxy  = nx * IMARR_SUBIM(bmar,0)->ny ;

   for( ii=0 ; ii < nvox ; ii++ ){
     vxx = vyy = vzz = 0.0f ;
     if( mask[ii] == 1 ){
       nfxx = nfyy = nfzz = 0 ;
        fxx =  fyy =  fzz = 0.0f ;
       IJK_TO_THREE(ii,xx,yy,zz,nx,nxy) ;
       for( ibm=0 ; ibm < nar ; ibm++ ){
         fw = mri_nstat_fwhmxyz( xx,yy,zz , IMARR_SUBIM(bmar,ibm),mask,nbhd );
         if( fxar != NULL && fw.xyz[0] > 0.0f ){ nfxx++; fxx += fw.xyz[0]; }
         if( fyar != NULL && fw.xyz[1] > 0.0f ){ nfyy++; fyy += fw.xyz[1]; }
         if( fzar != NULL && fw.xyz[2] > 0.0f ){ nfzz++; fzz += fw.xyz[2]; }
       }
       if( nfxx > 0 ) vxx = fxx / nfxx ;
       if( nfyy > 0 ) vyy = fyy / nfyy ;
       if( nfzz > 0 ) vzz = fzz / nfzz ;
     }
     ASSIF(fxar,ii,vxx); ASSIF(fyar,ii,vyy); ASSIF(fzar,ii,vzz);
   }

   EXRETURN;
}
