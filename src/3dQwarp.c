#include "mrilib.h"

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
   if( Hverb )
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
     if( Hverb ) ININFO_message("Weightize: user clip=%g #clipped=%d #left=%d",
                                   aclip,nclip,nleft) ;
   }

   /*-- squash super-large values down to reasonability --*/

   clip = 3.0f * THD_cliplevel(qim,0.5f) ;
   if( Hverb ) ININFO_message("Weightize: (unblurred) top clip=%g",clip) ;
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
   if( Hverb ) ININFO_message("Weightize: (blurred) bot clip=%g",clip) ;
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
     if( Hverb ) ININFO_message("Weightize: raising to %g power",apow) ;
     for( ii=0 ; ii < nxyz ; ii++ )
       if( wf[ii] > 0.0f ) wf[ii] = powf( wf[ii] , apow ) ;
   }

   /*-- binarize (acod==2)?  boxize (acod==3)? --*/

#undef  BPAD
#define BPAD 4
   if( acod == 2 || acod == 3 ){  /* binary weight: mask=2 or maskbox=3 */
     if( Hverb ) ININFO_message("Weightize: binarizing") ;
     for( ii=0 ; ii < nxyz ; ii++ ) if( wf[ii] != 0.0f ) wf[ii] = 1.0f ;
     if( ndil > 0 ){  /* 01 Mar 2007: dilation */
       byte *mmm = (byte *)malloc(sizeof(byte)*nxyz) ;
       if( Hverb ) ININFO_message("Weightize: dilating") ;
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
       if( Hverb )
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
   THD_3dim_dataset *bset , *sset , *oset ;
   MRI_IMAGE *bim , *wbim , *sim , *oim ;
   IndexWarp3D *oww ; Image_plus_Warp *oiw ;
   char *prefix = "Qwarp" , ppp[256] ; int nopt , nevox=0 ;
   int meth = GA_MATCH_PEARCLP_SCALAR ;
   int duplo=0 , qsave=0 , minpatch=0 , nx,ny,nz , ct , nnn ;

   if( argc < 3 || strcasecmp(argv[1],"-help") == 0 ){
     printf("Usage: 3dQwarp [OPTIONS] base_dataset source_dataset\n") ;
     printf(
       "\n"
       "* Computes a warped version of source_dataset to match base_dataset.\n"
       "\n"
       "* Input datasets must be on the same grid!\n"
       "\n"
       "* Inputs should be reasonably well aligned already\n"
       "  (e.g., as from an affine warping via 3dAllineate).\n"
       "\n"
       "* Outputs are the warped dataset and the warp that did it.\n"
       "\n"
       "* Matching by default is the 'clipped Pearson' method, and\n"
       "  can be changed to 'pure Pearson' with the '-pear' option.\n"
       "\n"
       "OPTIONS\n"
       "-------\n"
       " -prefix ppp  = Sets the prefix for the output datasets.\n"
       "               * The source dataset is warped to match the base\n"
       "                 and gets prefix 'ppp'.\n"
       "               * The 3D warp used is saved in a dataset with\n"
       "                 prefix 'ppp_WARP' -- this dataset can be used\n"
       "                 with 3dNwarpApply and 3dNwarpCalc.\n"
       "\n"
       " -pear        = Use Pearson correlation for matching.\n"
       "\n"
       " -nopenalty   = Don't use a penalty on the cost function; the goal\n"
       "                of the penalty is to reduce grid distortions.\n"
       " -penfac ff   = Use the number 'ff' to weight the penalty.\n"
       "                The default is 1.  Larger values of 'ff' mean the\n"
       "                penalty counts more, reducing grid distortions,\n"
       "                insha'Allah. '-nopenalty' is the same as '-penfac 0'.\n"
       "\n"
       " -blur bb     = Blur the input images by 'bb' voxels before doing the\n"
       "                alignment (the output dataset will not be blurred).\n"
       "                The default is 3.456.  Optionally, you can provide\n"
       "                2 values for 'bb', and then the first one is applied\n"
       "                to the base volume, the second to the source volume.\n"
       "               * e.g., '-blur 0 3' to skip blurring the base image\n"
       "                 (if the base is a blurry template, for example).\n"
       "\n"
       " -emask ee    = Here, 'ee' is a dataset to specify a mask of voxels\n"
       "                to EXCLUDE from the analysis -- all voxels in 'ee'\n"
       "                that are nonzero will not be used in the alignment.\n"
       "               * The base image is also automasked -- the emask is\n"
       "                 extra, to indicate voxels you definitely DON'T want\n"
       "                 included in the matching process.\n"
       "               * Applications: exclude a tumor or resected region.\n"
       "               * Note that the emask applies to the base dataset,\n"
       "                 so if you are registering a pre- and post-surgery\n"
       "                 volume, you would probably use the post-surgery\n"
       "                 dataset as the base.  If you eventually want the\n"
       "                 result back in the pre-surgery space, then you\n"
       "                 would use the inverse warp afterwards, via program\n"
       "                 3dNwarpCalc.\n"
       "\n"
       " -minpatch mm  = Set the minimum patch size for warp searching to 'mm' voxels.\n"
       "   *OR*         * The value of mm should be an odd integer.\n"
       " -patchmin mm   * The default value of mm is 25.\n"
       "                * For more accurate results than mm=25, try 19.\n"
       "                * The smallest allowed value is 9 (which will be very slow).\n"
       "                * If you want to see the warped results at various levels\n"
       "                  of patch size, use the '-qsave' option.\n"
       "\n"
       " -duplo        = Start off with 1/2 scale versions of the volumes,\n"
       "                 for getting a speedy first alignment.\n"
       " -workhard     = Iterate more times at the coarser grid levels,\n"
       "                 which can help when the volumes are hard to align.\n"
       "                * Slows the program down, of course.\n"
       "                * Although -workhard will work OK with -duplo, it is better\n"
       "                  applied without the -duplo option.\n"
       "                * You can also try '-workharder' and '-workhardest'.\n"
       " -qsave        = Save intermediate warped results as well, in a dataset\n"
       "                 with '_SAVE' appended to the '-prefix' value.\n"
       "\n"
       "METHOD\n"
       "------\n"
       "Incremental warping with cubic basic functions, first over the entire volume,\n"
       "then over steadily shrinking patches.  For this to work,  the source and base\n"
       "need to be pretty well aligned already (e.g., 3dAllineate-d).\n"
       "\n"
       "*** This program is experimental and subject to sudden drastic change! ***\n"
       "*** At some point, it will be incorporated into 3dAllineate (I hope)!! ***\n"
       "\n"
       "--- AUTHOR = RWCox -- Fall/Winter 2012-13 ---\n"
     ) ;
     PRINT_AFNI_OMP_USAGE("3dQwarp",NULL) ;
     exit(0) ;
   }

   enable_mcw_malloc() ;

#ifdef USE_OMP
   omp_set_nested(0) ;
   nthmax = omp_get_max_threads() ;
   dhaar  = (double *)malloc(sizeof(double)*nthmax) ;
   dhbbr  = (double *)malloc(sizeof(double)*nthmax) ;
   dhccr  = (double *)malloc(sizeof(double)*nthmax) ;
   dhddr  = (double *)malloc(sizeof(double)*nthmax) ;
   dheer  = (double *)malloc(sizeof(double)*nthmax) ;
   dhffr  = (double *)malloc(sizeof(double)*nthmax) ;
   INFO_message("OpenMP thread count = %d",nthmax) ;
#else
   INFO_message("this edition not compiled with OpenMP :-(") ;
#endif

   mainENTRY("3dQwarp") ;

   nopt = 1 ;
   Hblur_b = Hblur_s = 3.456f ;
   while( nopt < argc && argv[nopt][0] == '-' ){

     if( strcasecmp(argv[nopt],"-patchmin") == 0 || strcasecmp(argv[nopt],"-minpatch") == 0 ){
       if( ++nopt >= argc ) ERROR_exit("need arg after %s",argv[nopt-1]) ;
       minpatch = (int)strtod(argv[nopt],NULL) ;
       if( minpatch < NGMIN ) minpatch = NGMIN ; else if( minpatch%2 == 0 ) minpatch-- ;
       nopt++ ; continue ;
     }

     if( strcasecmp(argv[nopt],"-duplo") == 0 ){
       duplo = 1 ; nopt++ ; continue ;
     }

     if( strcasecmp(argv[nopt],"-workhard") == 0 ){
       if( Hworkhard < 4 ) Hworkhard = 4 ;
       else                Hworkhard++ ;
       nopt++ ; continue ;
     }

     if( strcasecmp(argv[nopt],"-workharder") == 0 ){
       if( Hworkhard < 7 ) Hworkhard  = 7 ;
       else                Hworkhard += 2 ; ;
       nopt++ ; continue ;
     }

     if( strcasecmp(argv[nopt],"-workhardest") == 0 ){
       if( Hworkhard < 10 ) Hworkhard  = 10 ;
       else                 Hworkhard +=  2 ;
       nopt++ ; continue ;
     }

     if( strcasecmp(argv[nopt],"-qsave") == 0 ){
       qsave = 1 ; nopt++ ; continue ;
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
       if( !isdigit(argv[nopt][0]) && argv[nopt][0] != '-' )
         ERROR_exit("value after '-blur' must start with a digit or '-'") ;
       val2 = val1 = (float)strtod(argv[nopt],NULL) ;
       if( nopt+1 < argc &&
           ( isdigit(argv[nopt+1][0]) ||
             (argv[nopt+1][0] == '-' && isdigit(argv[nopt+1][1])) ) ){
           val2 = (float)strtod(argv[++nopt],NULL) ;
       }
       Hblur_b = val1 ; Hblur_s = val2 ;
       nopt++ ; continue ;
     }

     if( strcasecmp(argv[nopt],"-nopenalty") == 0 ){
       Hpen_fac = 0.0 ; nopt++ ; continue ;
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

#if 1
     if( strcasecmp(argv[nopt],"-hel") == 0 ){
       meth = GA_MATCH_HELLINGER_SCALAR ; nopt++ ; continue ;
     }

     if( strcasecmp(argv[nopt],"-mi") == 0 ){
       meth = GA_MATCH_KULLBACK_SCALAR ; nopt++ ; continue ;
     }

     if( strcasecmp(argv[nopt],"-nmi") == 0 ){
       meth = GA_MATCH_NORMUTIN_SCALAR ; nopt++ ; continue ;
     }
#endif

     if( strcasecmp(argv[nopt],"-pcl") == 0 ){
       meth = GA_MATCH_PEARCLP_SCALAR ; nopt++ ; continue ;
     }

     if( strcasecmp(argv[nopt],"-pear") == 0 ){
       meth = GA_MATCH_PEARSON_SCALAR ; nopt++ ; continue ;
     }

     ERROR_exit("Bogus option '%s'",argv[nopt]) ;
   }

   ct = NI_clock_time() ;

   if( nopt+1 >= argc ) ERROR_exit("need 2 args for base and source") ;

   bset = THD_open_dataset(argv[nopt++]) ; if( bset == NULL ) ERROR_exit("Can't open bset") ;
   sset = THD_open_dataset(argv[nopt++]) ; if( sset == NULL ) ERROR_exit("Can't open sset") ;
   if( !EQUIV_GRIDXYZ(bset,sset) ) ERROR_exit("Dataset grid mismatch") ;

   DSET_load(bset) ; CHECK_LOAD_ERROR(bset) ;
   bim = THD_extract_float_brick(0,bset) ; DSET_unload(bset) ;

   DSET_load(sset) ; CHECK_LOAD_ERROR(sset) ;
   sim = THD_extract_float_brick(0,sset) ; DSET_unload(sset) ;

   if( nevox > 0 && nevox != DSET_NVOX(bset) )
     ERROR_exit("-emask doesn't match base dataset grid :-(") ;

   nx = DSET_NX(bset) ; ny = DSET_NY(bset) ; nz = DSET_NZ(bset) ;

   nnn = 0 ;
   if( nx >= NGMIN             ) nnn = nx ;
   if( ny >= NGMIN && ny > nnn ) nnn = ny ;
   if( nz >= NGMIN && nz > nnn ) nnn = nz ;
   if( nnn == 0 )
     ERROR_exit("dataset grid size %d x %d x %d is to small for warping",nx,ny,nz) ;
   if( minpatch > 0 && minpatch > nnn/2 ){
     WARNING_message("-minpatch %d is too large for dataset grid %d x %d x %d",
                     minpatch , nx,ny,nz ) ;
     minpatch = nnn/2 ; if( minpatch%2 == 0 ) minpatch-- ;
   }
   if( minpatch > 0 ) Hngmin = minpatch ;

   if( duplo && (nx < 3*Hngmin || ny < 3*Hngmin || nz < 3*Hngmin) ){
     duplo = 0 ;
     INFO_message("-duplo disabled since dataset is so small: %d x %d x %d",nx,ny,nz) ;
   }

#ifdef USE_SAVER
   if( qsave ){
     sprintf(ppp,"%s_SAVE",prefix) ;
     qset = EDIT_empty_copy(bset) ;
     EDIT_dset_items( qset ,
                        ADN_prefix    , ppp ,
                        ADN_nvals     , 1 ,
                        ADN_ntt       , 0 ,
                        ADN_datum_all , MRI_float ,
                      ADN_none ) ;
     EDIT_BRICK_FACTOR(qset,0,0.0) ;
     iterfun = Qsaver ;
   }
#endif

   wbim = mri_weightize(bim,auto_weight,auto_dilation,auto_wclip,auto_wpow) ;

   if( Hblur_b >= 0.5f ){
     MRI_IMAGE *qim ;
     if( Hverb ) ININFO_message("   blurring base image %.3g voxels FWHM",Hblur_b) ;
     qim = mri_float_blur3D( FWHM_TO_SIGMA(Hblur_b) , bim ) ;
     mri_free(bim) ; bim = qim ;
   } else if( Hblur_b <= -1.0f ){
     MRI_IMAGE *qim ;
     if( Hverb ) ININFO_message("   median-izing base image %.3g voxels",-Hblur_b) ;
     qim = mri_medianfilter( bim , -Hblur_b , NULL , 0 ) ;
     mri_free(bim) ; bim = qim ;
   }

   if( duplo )
     oiw = IW3D_warp_s2bim_duplo( bim,wbim , sim , MRI_WSINC5 , meth , 0 ) ;
   else
     oiw = IW3D_warp_s2bim( bim,wbim , sim , MRI_WSINC5 , meth , 0 ) ;

   if( oiw == NULL ) ERROR_exit("s2bim fails") ;

   INFO_message("===== total number of parameters = %d",Hnpar_sum) ;

   oim = oiw->im ; oww = oiw->warp ;

   oset = EDIT_empty_copy(bset) ;
   EDIT_dset_items( oset ,
                      ADN_prefix    , prefix ,
                      ADN_nvals     , 1 ,
                      ADN_ntt       , 0 ,
                      ADN_datum_all , MRI_float ,
                    ADN_none ) ;
   EDIT_BRICK_FACTOR(oset,0,0.0) ;
   EDIT_substitute_brick( oset, 0, MRI_float, MRI_FLOAT_PTR(oim) ) ;
   DSET_write(oset) ; WROTE_DSET(oset) ; DSET_delete(oset) ;

   if( qset != NULL && DSET_NVALS(qset) > 1 ){
     EDIT_dset_items( qset , ADN_ntt , DSET_NVALS(qset) , ADN_none ) ;
     DSET_write(qset) ; WROTE_DSET(qset) ; DSET_delete(qset) ;
   }

   IW3D_adopt_dataset( oww , bset ) ;
   sprintf(ppp,"%s_WARP",prefix) ;
   qset = IW3D_to_dataset( oww , ppp ) ;
   DSET_write(qset) ; WROTE_DSET(qset) ; DSET_delete(qset) ;

   INFO_message("===== clock time =%s",nice_time_string(NI_clock_time()-ct)) ;
   exit(0) ;
}
