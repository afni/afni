/**** TO DO (someday, maybe):
        -matini
****/

/****** N.B.: What used to be 'target' is now 'source' to users,
              but remains as 'target' in the code and comments.  ******/

/****** N.B.: See the note about coordinates at the end of this file! ******/

/*----------------------------------------------------------------------------*/
#include "mrilib.h"
#include "r_new_resam_dset.h"
#include <time.h>
#include <sys/types.h>
#include <unistd.h>

#ifdef USE_OMP
#include <omp.h>
#endif

#define MAXPAR   199
#define PARC_FIX 1
#define PARC_INI 2
#define PARC_RAN 3
typedef struct { int np,code; float vb,vt ; } param_opt ;

#define WARP_SHIFT    1
#define WARP_ROTATE   2
#define WARP_SCALE    3
#define WARP_AFFINE   4

#define APPLY_PARAM   1   /* 23 Jul 2007 */
#define APPLY_AFF12   2
#define APPLY_BILIN   3
#define NPBIL        39   /* plus 4 */

#define WARP_BILINEAR 666

static float wt_medsmooth = 2.25f ;   /* for mri_weightize() */
static float wt_gausmooth = 4.50f ;

static int verb = 1 ; /* somewhat on by default */

MRI_IMAGE * mri_weightize( MRI_IMAGE *, int, int, float,float ); /* prototype */

float param_dist( GA_setup *stp , float *aa , float *bb ) ;      /* prototype */

void AL_setup_warp_coords( int,int,int,int ,
                           int *, float *, mat44,
                           int *, float *, mat44 ) ;             /* prototype */

#undef MEMORY_CHECK
#ifdef USING_MCW_MALLOC
# define MEMORY_CHECK(mm)                                                    \
   do{ if( verb > 5 ) mcw_malloc_dump() ;                                    \
       if( verb > 1 ){                                                       \
         long long nb = mcw_malloc_total() ;                                 \
         if( nb > 0 ) INFO_message("Memory usage now = %lld (%s): %s" ,      \
                      nb , approximate_number_string((double)nb) , (mm) ) ;  \
       }                                                                     \
   } while(0)
#else
# define MEMORY_CHECK(mm) /*nada*/
#endif

#define ALLOW_METH_CHECK   /* for the -check option: 03 Apr 2008 */

/*----------------------------------------------------------------------------*/
#undef  NMETH
#define NMETH GA_MATCH_METHNUM_SCALAR  /* cf. mrilib.h */

static int meth_visible[NMETH] =       /* 1 = show in -help; 0 = don't show */
  { 1 , 0 , 1 , 1 , 1 , 0 , 1 , 1 , 1 , 0 , 0 , 0 , 0 } ;
/* ls  sp  mi  crM nmi je  hel crA crU lss lpc lpa ncd */

static int meth_noweight[NMETH] =      /* 1 = don't allow weights, just masks */
  { 0 , 1 , 1 , 0 , 1 , 1 , 1 , 0 , 0 , 0 , 0 , 0 , 1 } ;
/* ls  sp  mi  crM nmi je  hel crA crU lss lpc lpa ncd */

static int visible_noweights ;

static char *meth_shortname[NMETH] =   /* short names for terse cryptic users */
  { "ls" , "sp" , "mi" , "crM", "nmi", "je", "hel",
    "crA", "crU", "lss", "lpc", "lpa", "ncd"       } ;

static char *meth_longname[NMETH] =    /* long names for prolix users */
  { "leastsq"         , "spearman"     ,
    "mutualinfo"      , "corratio_mul" ,
    "norm_mutualinfo" , "jointentropy" ,
    "hellinger"       ,
    "corratio_add"    , "corratio_uns" , "signedPcor" ,
    "localPcorSigned" , "localPcorAbs" , "NormCompDist" } ;

static char *meth_username[NMETH] =    /* descriptive names */
  { "Least Squares [Pearson Correlation]"   ,
    "Spearman [rank] Correlation"           ,  /* hidden */
    "Mutual Information [H(b)+H(s)-H(b,s)]" ,
    "Correlation Ratio (Symmetrized*)"      ,
    "Normalized MI [H(b,s)/(H(b)+H(s))]"    ,
    "Joint Entropy [H(b,s)]"                ,  /* hidden */
    "Hellinger metric"                      ,
    "Correlation Ratio (Symmetrized+)"      ,
    "Correlation Ratio (Unsym)"             ,
    "Signed Pearson Correlation"            ,  /* hidden */
    "Local Pearson Correlation Signed"      ,  /* hidden */
    "Local Pearson Correlation Abs"         ,  /* hidden */
    "Normalized Compression Distance"     } ;  /* hidden */

static char *meth_costfunctional[NMETH] =  /* describe cost functional */
  { "1 - abs(Pearson correlation coefficient)"                 ,
    "1 - abs(Spearman correlation coefficient)"                ,
    "- Mutual Information = H(base,source)-H(base)-H(source)"  ,
    "1 - abs[ CR(base,source) * CR(source,base) ]"             ,
    "1/Normalized MI = H(base,source)/[H(base)+H(source)]"     ,
    "H(base,source) = joint entropy of image pair"             ,
    "- Hellinger distance(base,source)"                        ,
    "1 - abs[ CR(base,source) + CR(source,base) ]"             ,
    "CR(source,base) = Var(source|base) / Var(source)"         ,
    "Pearson correlation coefficient between image pair"       ,
    "nonlinear average of Pearson cc over local neighborhoods" ,
    "1 - abs(lpc)"                                             ,
    "mutual compressibility (via zlib) -- doesn't work yet"
  } ;
/*---------------------------------------------------------------------------*/

#define SETUP_BILINEAR_PARAMS                                                \
 do{ char str[16] ;                                                          \
     stup.wfunc_numpar = NPBIL+4 ;                                           \
     stup.wfunc        = mri_genalign_bilinear ;                             \
     stup.wfunc_param  = (GA_param *)realloc( (void *)stup.wfunc_param,      \
                                              (NPBIL+4)*sizeof(GA_param) ) ; \
     for( jj=12 ; jj < NPBIL ; jj++ ){                                       \
       sprintf(str,"blin%02d",jj+1) ;                                        \
       DEFPAR( jj,str, -0.1999f,0.1999f , 0.0f,0.0f,0.0f ) ;                 \
       stup.wfunc_param[jj].fixed = 1 ;                                      \
     }                                                                       \
     DEFPAR(NPBIL  ,"xcen" ,-1.0e9,1.0e9 , 0.0f,0.0f,0.0f ) ;                \
     DEFPAR(NPBIL+1,"ycen" ,-1.0e9,1.0e9 , 0.0f,0.0f,0.0f ) ;                \
     DEFPAR(NPBIL+2,"zcen" ,-1.0e9,1.0e9 , 0.0f,0.0f,0.0f ) ;                \
     DEFPAR(NPBIL+3,"ddfac", 0.0f ,1.0e9 , 1.0f,0.0f,0.0f ) ;                \
     stup.wfunc_param[NPBIL  ].fixed = 2 ;                                   \
     stup.wfunc_param[NPBIL+1].fixed = 2 ;                                   \
     stup.wfunc_param[NPBIL+2].fixed = 2 ;                                   \
     stup.wfunc_param[NPBIL+3].fixed = 2 ;                                   \
 } while(0)

/*---------------------------------------------------------------------------*/

int main( int argc , char *argv[] )
{
   THD_3dim_dataset *dset_out=NULL ;
   MRI_IMAGE *im_base, *im_targ, *im_weig=NULL, *im_mask=NULL, *qim ;
   MRI_IMAGE *im_bset, *im_wset, *im_tmask=NULL ;
   GA_setup stup ;
   int iarg , ii,jj,kk , nmask=0 , nfunc , rr , ntask , ntmask=0 ;
   int   nx_base,ny_base,nz_base , nx_targ,ny_targ,nz_targ , nxy_base ;
   float dx_base,dy_base,dz_base , dx_targ,dy_targ,dz_targ ;
   int   nxyz_base[3] , nxyz_targ[3] , nxyz_dout[3] ;
   float dxyz_base[3] , dxyz_targ[3] , dxyz_dout[3] ;
   int nvox_base ;
   float v1,v2 , xxx_p,yyy_p,zzz_p,siz , xxx_m,yyy_m,zzz_m , xxx,yyy,zzz , xc,yc,zc ;
   int pad_xm=0,pad_xp=0 , pad_ym=0,pad_yp=0 , pad_zm=0,pad_zp=0 ;
   int tfdone=0;  /* stuff for -twofirst */
   float tfparm[PARAM_MAXTRIAL+2][MAXPAR];  /* +2 for some extra cases */
   float ffparm[PARAM_MAXTRIAL+2][MAXPAR];  /* not really used yet */
   float tfcost[PARAM_MAXTRIAL+2] ;
   int   tfindx[PARAM_MAXTRIAL+2] ;
   int skip_first=0 , didtwo , targ_kind , skipped=0 , nptwo=6 ;
   double ctim=0.0,dtim , rad , conv_rad ;
   float **parsave=NULL ;
   mat44 *matsave=NULL ;
   mat44 targ_cmat,base_cmat,base_cmat_inv,targ_cmat_inv,mast_cmat,mast_cmat_inv,
         qmat,wmat ;
   MRI_IMAGE *apply_im = NULL ;
   float *apply_far    = NULL ;
   int apply_nx=0, apply_ny=0, apply_mode=0, nparam_free , diffblur=1 ;
   float cost, cost_ini ;
   mat44 cmat_bout , cmat_tout , aff12_xyz ;
   int   nxout=0,nyout=0,nzout=0 ;
   float dxout,dyout,dzout ;
   floatvec *allcost ;   /* 19 Sep 2007 */
   float     allpar[MAXPAR] ;
   float xsize , ysize , zsize ;  /* 06 May 2008: box size */
   float bfac ;                   /* 14 Oct 2008: brick factor */

   /*----- input parameters, to be filled in from the options -----*/

   THD_3dim_dataset *dset_base = NULL ;
   THD_3dim_dataset *dset_targ = NULL ;
   THD_3dim_dataset *dset_mast = NULL ;
   THD_3dim_dataset *dset_weig = NULL ;
   int tb_mast                 = 0 ;            /* for -master SOURCE/BASE */
   int auto_weight             = 3 ;            /* -autobbox == default */
   float auto_wclip            = 0.0f ;         /* 31 Jul 2007 */
   float auto_wpow             = 1.0f ;         /* 10 Sep 2007 */
   char *auto_string           = "-autobox" ;
   int auto_dilation           = 0 ;            /* for -automask+N */
   int wtspecified             = 0 ;            /* 10 Sep 2007 */
   double dxyz_mast            = 0.0f ;         /* implemented 24 Jul 2007 */
   int meth_code               = GA_MATCH_HELLINGER_SCALAR ;
   int sm_code                 = GA_SMOOTH_GAUSSIAN ;
   float sm_rad                = 0.0f ;
   float fine_rad              = 0.0f ;
   int floatize                = 0 ;            /* off by default */
   int twopass                 = 1 ;            /* on by default */
   int twofirst                = 1 ;            /* on by default */
   int zeropad                 = 1 ;            /* on by default */
   char *prefix                = NULL ;         /* off by default */
   char *wtprefix              = NULL ;         /* off by default */
   char *param_save_1D         = NULL ;         /* off by default */
   char *matrix_save_1D        = NULL ;         /* 23 Jul 2007 */
   char *apply_1D              = NULL ;         /* off by default */
   int interp_code             = MRI_LINEAR ;
   int npt_match               = -47 ;          /* 47%, that is */
   int final_interp            = MRI_CUBIC ;
   int warp_code               = WARP_AFFINE ;
   int warp_freeze             = 0 ;            /* off by default */
   int nparopt                 = 0 ;
   MRI_IMAGE *matini           = NULL ;
   int tbest                   = 4 ;            /* default=try best 4 */
   int num_rtb                 = 99 ;           /* 28 Aug 2008 */
   int nocast                  = 0 ;            /* 29 Aug 2008 */
   param_opt paropt[MAXPAR] ;
   float powell_mm             = 0.0f ;
   float powell_aa             = 0.0f ;
   float conv_mm               = 0.05 ;         /* millimeters */
   float nmask_frac            = -1.0;          /* use default for voxel fraction */
   int matorder                = MATORDER_SDU ; /* matrix mult order */
   int smat                    = SMAT_LOWER ;   /* shear matrix triangle */
   int dcode                   = DELTA_AFTER ;  /* shift after */
   int meth_check_count        = 0 ;            /* don't do it */
   int meth_check[NMETH+1] ;
   int meth_median_replace     = 0 ;            /* don't do it */
   char *save_hist             = NULL ;         /* don't save it */
   long seed                   = 7654321 ;      /* random? */
   int XYZ_warp                = 0 ;            /* off by default */
   double hist_pow             = 0.0 ;
   int hist_nbin               = 0 ;
   int epi_fe                  = -1 ;           /* off by default */
   int epi_pe                  = -1 ;
   int epi_se                  = -1 ;
   int epi_targ                = -1 ;
   int replace_base            = 0 ;            /* off by default */
   int replace_meth            = 0 ;            /* off by default */
   int usetemp                 = 0 ;            /* off by default */
   int nmatch_setup            = 98765 ;
   int ignout                  = 0 ;            /* 28 Feb 2007 */
   int    hist_mode            = 0 ;            /* 08 May 2007 */
   float  hist_param           = 0.0f ;
   int    hist_setbyuser       = 0 ;
   int    do_cmass             = 0 ;            /* 30 Jul 2007 */
   int    do_refinal           = 1 ;            /* 14 Nov 2007 */

   int auto_tdilation          = 0 ;            /* for -source_automask+N */
   int auto_tmask              = 0 ;
   char *auto_tstring          = NULL ;

   int bloktype                = GA_BLOK_RHDD ; /* 20 Aug 2007 */
   float blokrad               = 6.54321f ;
   int blokmin                 = 0 ;

   int do_allcost              = 0 ;            /* 19 Sep 2007 */

   MRI_IMAGE *allcostX1D       = NULL ;         /* 02 Sep 2008 */
   char *allcostX1D_outname    = NULL ;

   int   nwarp_pass            = 0 ;
   int   nwarp_type            = WARP_BILINEAR ;
   float nwarp_order           = 2.9f ;

   /**----------------------------------------------------------------------*/
   /**----------------- Help the pitifully ignorant user? -----------------**/

   if( argc < 2 || strcmp(argv[1],"-help")==0 ||
                   strcmp(argv[1],"-HELP")==0 || strcmp(argv[1],"-POMOC")==0 ){

     visible_noweights = 0 ;
     for( ii=0 ; ii < NMETH ; ii++ )
       if( meth_visible[ii] && meth_noweight[ii] ) visible_noweights++ ;

     printf(
"Usage: 3dAllineate [options] sourcedataset\n"
"\n"
"Program to align one dataset (the 'source') to a base dataset.\n"
"Options are available to control:\n"
" ++ How the matching between the source and the base is computed\n"
"    (i.e., the 'cost functional' measuring image mismatch).\n"
" ++ How the resliced source is interpolated to the base space.\n"
" ++ The complexity of the spatial transformation ('warp') used.\n"
" ++ And many technical options to control the process in detail,\n"
"    if you know what you are doing (or just like to play around).\n"
"\n"
"====\n"
"NOTE: If you want to align EPI volumes to a T1-weighted structural\n"
"====  volume, the script align_epi_anat.py is recommended.  It will\n"
"      use 3dAllineate in the recommended way for this type of problem.\n"
" -->> This script can also be used for other alignment purposes, such\n"
"      as T1-weighted alignment between field strengths using the\n"
"      '-lpa' cost functional.  Investigate align_epi_anat.py\n"
"      to see if it will do what you need -- you might make your life\n"
"      a little easier and nicer.\n"
"\n"
"OPTIONS:\n"
"=======\n"
" -base bbb   = Set the base dataset to be the #0 sub-brick of 'bbb'.\n"
"               If no -base option is given, then the base volume is\n"
"               taken to be the #0 sub-brick of the source dataset.\n"
"               (Base must be stored as floats, shorts, or bytes.)\n"
"\n"
" -source ttt = Read the source dataset from 'ttt'.  If no -source\n"
"   *OR*        (or -input) option is given, then the source dataset\n"
" -input ttt    is the last argument on the command line.\n"
"               (Source must be stored as floats, shorts, or bytes.)\n"
"\n"
"  * NOTA BENE: The base and source dataset do NOT have to be defined *\n"
"  *            on the same 3D grids; the alignment process uses the  *\n"
"  *            coordinate systems defined in the dataset headers to  *\n"
"  *            make the match between spatial locations.             *\n"
"  *       -->> However, this coordinate-based matching requires that *\n"
"  *            image volumes be defined on roughly the same patch of *\n"
"  *            of (x,y,z) space, in order to find a decent starting  *\n"
"  *            point for the transformation.  You might need to use  *\n"
"  *            the script @Align_Centers to do this, if the 3D       *\n"
"  *            spaces occupied by the images do not overlap much.    *\n"
"  *       -->> Or the '-cmass' option to this program might be       *\n"
"  *            sufficient to solve this problem, maybe.              *\n"
"\n"
" -prefix ppp = Output the resulting dataset to file 'ppp'.  If this\n"
"   *OR*        option is NOT given, no dataset will be output!  The\n"
" -out ppp      transformation matrix to align the source to the base will\n"
"               be estimated, but not applied.  You can save the matrix\n"
"               for later use using the '-1Dmatrix_save' option.\n"
"        *N.B.: By default, the new dataset is computed on the grid of the\n"
"                base dataset; see the '-master' and/or the '-mast_dxyz'\n"
"                options to change this grid.\n"
"        *N.B.: If 'ppp' is 'NULL', then no output dataset will be produced.\n"
"                This option is for compatibility with 3dvolreg.\n"
"\n"
" -floatize   = Write result dataset as floats.  Internal calculations\n"
" -float        are all done on float copies of the input datasets.\n"
"               [Default=convert output dataset to data format of  ]\n"
"               [        source dataset; if the source dataset was ]\n"
"               [        shorts with a scale factor, then the new  ]\n"
"               [        dataset will get a scale factor as well;  ]\n"
"               [        if the source dataset was shorts with no  ]\n"
"               [        scale factor, the result will be unscaled.]\n"
"\n"
" -1Dparam_save ff   = Save the warp parameters in ASCII (.1D) format into\n"
"                      file 'ff' (1 row per sub-brick in source).\n"
"               *N.B.: A historical synonym for this option is '-1Dfile'.\n"
"\n"
" -1Dparam_apply aa  = Read warp parameters from file 'aa', apply them to \n"
"                      the source dataset, and produce a new dataset.\n"
"                      (Must also use the '-prefix' option for this to work!  )\n"
"                      (In this mode of operation, there is no optimization of)\n"
"                      (the cost functional by changing the warp parameters;  )\n"
"                      (previously computed parameters are applied directly.  )\n"
"               *N.B.: A historical synonym for this is '-1Dapply'.\n"
"               *N.B.: If you use -1Dparam_apply, you may also want to use\n"
"                       -master to control the grid on which the new\n"
"                       dataset is written -- the base dataset from the\n"
"                       original 3dAllineate run would be a good possibility.\n"
"                       Otherwise, the new dataset will be written out on the\n"
"                       3D grid coverage of the source dataset, and this\n"
"                       might result in clipping off part of the image.\n"
"               *N.B.: Each row in the 'aa' file contains the parameters for\n"
"                       transforming one sub-brick in the source dataset.\n"
"                       If there are more sub-bricks in the source dataset\n"
"                       than there are rows in the 'aa' file, then the last\n"
"                       row is used repeatedly.\n"
"               *N.B.: A trick to use 3dAllineate to resample a dataset to\n"
"                       a finer grid spacing:\n"
"                         3dAllineate -input dataset+orig         \\\n"
"                                     -master template+orig       \\\n"
"                                     -prefix newdataset          \\\n"
"                                     -final quintic              \\\n"
"                                     -1Dparam_apply '1D: 12@0'\\'  \n"
"                       Here, the identity transformation is specified\n"
"                       by giving all 12 affine parameters as 0 (note\n"
"                       the extra \\' at the end of the '1D: 12@0' input!).\n"
"\n"
" -1Dmatrix_save ff  = Save the transformation matrix for each sub-brick into\n"
"                      file 'ff' (1 row per sub-brick in the source dataset).\n"
"                      If 'ff' does NOT end in '.1D', then the program will\n"
"                      append '.aff12.1D' to 'ff' to make the output filename.\n"
"               *N.B.: This matrix is the coordinate transformation from base\n"
"                       to source DICOM coordinates. In other terms:\n"
"                          Xin = Xsource = M Xout = M Xbase\n"
"                                   or\n"
"                          Xout = Xbase = inv(M) Xin = inv(M) Xsource\n"
"                       where Xin or Xsource is the 4x1 coordinates of a\n"
"                       location in the input volume. Xout is the \n"
"                       coordinate of that same location in the output volume.\n"
"                       Xbase is the coordinate of the corresponding location\n"
"                       in the base dataset. M is ff augmented by a 4th row of\n"
"                       [0 0 0 1], X. is an augmented column vector [x,y,z,1]'\n"
"                       To get the inverse matrix inv(M)\n"
"                       (source to base), use the cat_matvec program, as in\n"
"                         cat_matvec fred.aff12.1D -I\n"
"\n"
" -1Dmatrix_apply aa = Use the matrices in file 'aa' to define the spatial\n"
"                      transformations to be applied.  Also see program\n"
"                      cat_matvec for ways to manipulate these matrix files.\n"
"               *N.B.: You probably want to use either -base or -master\n"
"                      with either *_apply option, so that the coordinate\n"
"                      system that the matrix refers to is correctly loaded.\n"
"\n"
"  * The -1Dmatrix_* options can be used to save and re-use the transformation *\n"
"  * matrices.  In combination with the program cat_matvec, which can multiply *\n"
"  * saved transformation matrices, you can also adjust these matrices to      *\n"
"  * other alignments.                                                         *\n"
"\n"
"  * The script 'align_epi_anat.py' uses 3dAllineate and 3dvolreg to align EPI *\n"
"  * datasets to T1-weighted anatomical datasets, using saved matrices between *\n"
"  * the two programs.  This script is our currently recommended method for    *\n"
"  * doing such intra-subject alignments.                                      *\n"
"\n"
" -cost ccc   = Defines the 'cost' function that defines the matching\n"
"               between the source and the base; 'ccc' is one of\n"
      ) ;

      for( ii=0 ; ii < NMETH ; ii++ )
        if( meth_visible[ii] )
          printf( "                %-4s *OR*  %-16s= %s\n" ,
                  meth_shortname[ii] , meth_longname[ii] , meth_username[ii] ) ;

      printf(
"               You can also specify the cost functional using an option\n"
"               of the form '-mi' rather than '-cost mi', if you like\n"
"               to keep things terse and cryptic (as I do).\n"
"               [Default == '-hel' (for no good reason).]\n"
"\n"
" -interp iii = Defines interpolation method to use during matching\n"
"               process, where 'iii' is one of\n"
"                 NN      *OR* nearestneighbour *OR nearestneighbor\n"
"                 linear  *OR* trilinear\n"
"                 cubic   *OR* tricubic\n"
"                 quintic *OR* triquintic\n"
"               Using '-NN' instead of '-interp NN' is allowed (e.g.).\n"
"               Note that using cubic or quintic interpolation during\n"
"               the matching process will slow the program down a lot.\n"
"               Use '-final' to affect the interpolation method used\n"
"               to produce the output dataset, once the final registration\n"
"               parameters are determined.  [Default method == 'linear'.]\n"
"            ** N.B.: Linear interpolation is used during the coarse\n"
"                     alignment pass; the selection here only affects\n"
"                     the interpolation method used during the second\n"
"                     (fine) alignment pass.\n"
"\n"
" -final iii  = Defines the interpolation mode used to create the\n"
"               output dataset.  [Default == 'cubic']\n"
"            ** N.B.: For '-final' ONLY, you can use 'wsinc5' to specify\n"
"                       that the final interpolation be done using a\n"
"                       weighted sinc interpolation method.  This method\n"
"                       is so SLOW that you aren't allowed to use it for\n"
"                       the registration itself.\n"
"                  ++ wsinc5 interpolation is highly accurate and should\n"
"                       reduce the smoothing artifacts from lower\n"
"                       order interpolation methods (which are most\n"
"                       visible if you interpolate an EPI time series\n"
"                       to high resolution and then make an image of\n"
"                       the voxel-wise variance).\n"
"                  ++ On my Intel-based Mac, it takes about 2.5 s to do\n"
"                       wsinc5 interpolation, per 1 million voxels output.\n"
"                       For comparison, quintic interpolation takes about\n"
"                       0.3 s per 1 million voxels: 8 times faster than wsinc5.\n"
"                  ++ The '5' refers to the width of the sinc interpolation\n"
"                       weights: plus/minus 5 grid points in each direction\n"
"                       (this is a tensor product interpolation, for speed).\n"
"\n"
"TECHNICAL OPTIONS (used for fine control of the program):\n"
"=================\n"
" -nmatch nnn = Use at most 'nnn' scattered points to match the\n"
"               datasets.  The smaller nnn is, the faster the matching\n"
"               algorithm will run; however, accuracy may be bad if\n"
"               nnn is too small.  If you end the 'nnn' value with the\n"
"               '%%' character, then that percentage of the base's\n"
"               voxels will be used.\n"
"               [Default == 47%% of voxels in the weight mask]\n"
"\n"
" -nopad      = Do not use zero-padding on the base image.\n"
"               [Default == zero-pad, if needed; -verb shows how much]\n"
"\n"
" -conv mmm   = Convergence test is set to 'mmm' millimeters.\n"
"               This doesn't mean that the results will be accurate\n"
"               to 'mmm' millimeters!  It just means that the program\n"
"               stops trying to improve the alignment when the optimizer\n"
"               (NEWUOA) reports it has narrowed the search radius\n"
"               down to this level.  [Default == 0.05 mm]\n"
"\n"
" -verb       = Print out verbose progress reports.\n"
"               [Using '-VERB' will give even more prolix reports.]\n"
" -quiet      = Don't print out verbose stuff.\n"
" -usetemp    = Write intermediate stuff to disk, to economize on RAM.\n"
"               Using this will slow the program down, but may make it\n"
"               possible to register datasets that need lots of space.\n"
"       **N.B.: Temporary files are written to the directory given\n"
"               in environment variable TMPDIR, or in /tmp, or in ./\n"
"               (preference in that order).  If the program crashes,\n"
"               these files are named TIM_somethingrandom, and you\n"
"               may have to delete them manually. (TIM=Temporary IMage)\n"
"       **N.B.: If the program fails with a 'malloc failure' type of\n"
"               message, then try '-usetemp' (malloc=memory allocator).\n"
#ifdef USING_MCW_MALLOC
"       **N.B.: If you use '-verb', then memory usage is printed out\n"
"               at various points along the way.\n"
#endif
" -nousetemp  = Don't use temporary workspace on disk [the default].\n"
"\n"
#ifdef ALLOW_METH_CHECK
" -check kkk  = After cost functional optimization is done, start at the\n"
"               final parameters and RE-optimize using the new cost\n"
"               function 'kkk'.  If the results are too different, a\n"
"               warning message will be printed.  However, the final\n"
"               parameters from the original optimization will be\n"
"               used to create the output dataset. Using '-check'\n"
"               increases the CPU time, but can help you feel sure\n"
"               that the alignment process did not go wild and crazy.\n"
"               [Default == no check == don't worry, be happy!]\n"
"       **N.B.: You can put more than one function after '-check', as in\n"
"                 -nmi -check mi hel crU crM\n"
"               to register with Normalized Mutual Information, and\n"
"               then check the results against 4 other cost functionals.\n"
"       **N.B.: On the other hand, some cost functionals give better\n"
"               results than others for specific problems, and so\n"
"               a warning that 'mi' was significantly different than\n"
"               'hel' might not actually mean anything (e.g.).\n"
#if 0
"       **N.B.: If you use '-CHECK' instead of '-check', AND there are\n"
"               at least two extra check functions specified (in addition\n"
"               to the primary cost functional), then the output parameter\n"
"               set will be the median of all the final parameter sets\n"
"               generated at this stage (including the primary set).\n"
"                 **** '-CHECK' is experimental and CPU intensive ****\n"
#endif
#endif
"\n"
" ** PARAMETERS THAT AFFECT THE COST OPTIMIZATION STRATEGY **\n"
" -onepass    = Use only the refining pass -- do not try a coarse\n"
"               resolution pass first.  Useful if you know that only\n"
"               small amounts of image alignment are needed.\n"
"               [The default is to use both passes.]\n"
" -twopass    = Use a two pass alignment strategy, first searching for\n"
"               a large rotation+shift and then refining the alignment.\n"
"               [Two passes are used by default for the first sub-brick]\n"
"               [in the source dataset, and then one pass for the others.]\n"
"               ['-twopass' will do two passes for ALL source sub-bricks.]\n"
" -twoblur rr = Set the blurring radius for the first pass to 'rr'\n"
"               millimeters.  [Default == 11 mm]\n"
"       **N.B.: You may want to change this from the default if\n"
"               your voxels are unusually small or unusually large\n"
"               (e.g., outside the range 1-4 mm on each axis).\n"
" -twofirst   = Use -twopass on the first image to be registered, and\n"
"               then on all subsequent images from the source dataset,\n"
"               use results from the first image's coarse pass to start\n"
"               the fine pass.\n"
"               (Useful when there may be large motions between the   )\n"
"               (source and the base, but only small motions within   )\n"
"               (the source dataset itself; since the coarse pass can )\n"
"               (be slow, doing it only once makes sense in this case.)\n"
"       **N.B.: [-twofirst is on by default; '-twopass' turns it off.]\n"
" -twobest bb = In the coarse pass, use the best 'bb' set of initial\n"
"               points to search for the starting point for the fine\n"
"               pass.  If bb==0, then no search is made for the best\n"
"               starting point, and the identity transformation is\n"
"               used as the starting point.  [Default=4; min=0 max=7]\n"
"       **N.B.: Setting bb=0 will make things run faster, but less reliably.\n"
" -fineblur x = Set the blurring radius to use in the fine resolution\n"
"               pass to 'x' mm.  A small amount (1-2 mm?) of blurring at\n"
"               the fine step may help with convergence, if there is\n"
"               some problem.  [Default == 0 mm]\n"
"   **NOTES ON\n"
"   **STRATEGY: * If you expect only small-ish (< 2 voxels?) image movement,\n"
"                 then using '-onepass' or '-twobest 0' makes sense.\n"
"               * If you expect large-ish image movements, then do not\n"
"                 use '-onepass' or '-twobest 0'; the purpose of the\n"
"                 '-twobest' parameter is to search for large initial\n"
"                 rotations/shifts with which to start the coarse\n"
"                 optimization round.\n"
"               * If you have multiple sub-bricks in the source dataset,\n"
"                 then the default '-twofirst' makes sense if you don't expect\n"
"                 large movements WITHIN the source, but expect large motions\n"
"                 between the source and base.\n"
      ) ;

      printf(
"\n"
" -cmass        = Use the center-of-mass calculation to bracket the shifts.\n"
"                   [This option is OFF by default]\n"
"                 If given in the form '-cmass+xy' (for example), means to\n"
"                 do the CoM calculation in the x- and y-directions, but\n"
"                 not the z-direction.\n"
" -nocmass      = Don't use the center-of-mass calculation. [The default]\n"
"                  (You would not want to use the C-o-M calculation if the  )\n"
"                  (source sub-bricks have very different spatial locations,)\n"
"                  (since the source C-o-M is calculated from all sub-bricks)\n"
" **EXAMPLE: You have a limited coverage set of axial EPI slices you want to\n"
"            register into a larger head volume (after 3dSkullStrip, of course).\n"
"            In this case, '-cmass+xy' makes sense, allowing CoM adjustment\n"
"            along the x = R-L and y = A-P directions, but not along the\n"
"            z = I-S direction, since the EPI doesn't cover the whole brain\n"
"            along that axis.\n"
      ) ;

      printf(
"\n"
" -autoweight = Compute a weight function using the 3dAutomask\n"
"               algorithm plus some blurring of the base image.\n"
"       **N.B.: '-autoweight+100' means to zero out all voxels\n"
"                 with values below 100 before computing the weight.\n"
"               '-autoweight**1.5' means to compute the autoweight\n"
"                 and then raise it to the 1.5-th power (e.g., to\n"
"                 increase the weight of high-intensity regions).\n"
"               These two processing steps can be combined, as in\n"
"                 '-autoweight+100**1.5'\n"
      ) ;
      if( visible_noweights ){
         printf(
"       **N.B.: Some cost functionals do not allow -autoweight, and\n"
"               will use -automask instead.  A warning message\n"
"               will be printed if you run into this situation.\n"
"               If a clip level '+xxx' is appended to '-autoweight',\n"
"               then the conversion into '-automask' will NOT happen.\n"
"               Thus, using a small positive '+xxx' can be used trick\n"
"               -autoweight into working on any cost functional.\n"
         ) ;
      }
      printf(
" -automask   = Compute a mask function, which is like -autoweight,\n"
"               but the weight for a voxel is set to either 0 or 1.\n"
"       **N.B.: '-automask+3' means to compute the mask function, and\n"
"               then dilate it outwards by 3 voxels (e.g.).\n"
" -autobox    = Expand the -automask function to enclose a rectangular\n"
"               box that holds the irregular mask.\n"
"       **N.B.: This is the default mode of operation!\n"
"               For intra-modality registration, '-autoweight' may be better!\n"
"             * If the cost functional is 'ls', then '-autoweight' will be\n"
"               the default, instead of '-autobox'.\n"
" -nomask     = Don't compute the autoweight/mask; if -weight is not\n"
"               also used, then every voxel will be counted equally.\n"
" -weight www = Set the weighting for each voxel in the base dataset;\n"
"               larger weights mean that voxel counts more in the cost\n"
"               function.\n"
"       **N.B.: The weight dataset must be defined on the same grid as\n"
"               the base dataset.\n"
"       **N.B.: Even if a method does not allow -autoweight, you CAN\n"
"               use a weight dataset that is not 0/1 valued.  The\n"
"               risk is yours, of course (!*! as always in AFNI !*!).\n"
" -wtprefix p = Write the weight volume to disk as a dataset with\n"
"               prefix name 'p'.  Used with '-autoweight/mask', this option\n"
"               lets you see what voxels were important in the algorithm.\n"
      ) ;

      if( visible_noweights > 0 ){
        printf("\n"
               "    Method  Allows -autoweight\n"
               "    ------  ------------------\n") ;
        for( ii=0 ; ii < NMETH ; ii++ )
          if( meth_visible[ii] )
            printf("     %-4s   %s\n", meth_shortname[ii] ,
                                       meth_noweight[ii] ? "NO" : "YES" ) ;
      }

      printf(
       "\n"
       " -source_mask sss = Mask the source (input) dataset, using 'sss'.\n"
       " -source_automask = Automatically mask the source dataset.\n"
       "                      [By default, all voxels in the source]\n"
       "                      [dataset are used in the matching.   ]\n"
       "            **N.B.: You can also use '-source_automask+3' to dilate\n"
       "                    the default source automask outward by 3 voxels.\n"
      ) ;

      printf(
       "\n"
       " -warp xxx   = Set the warp type to 'xxx', which is one of\n"
       "                 shift_only         *OR* sho =  3 parameters\n"
       "                 shift_rotate       *OR* shr =  6 parameters\n"
       "                 shift_rotate_scale *OR* srs =  9 parameters\n"
       "                 affine_general     *OR* aff = 12 parameters\n"
       "               [Default = affine_general, which includes image]\n"
       "               [      shifts, rotations, scaling, and shearing]\n"
       "\n"
       " -warpfreeze = Freeze the non-rigid body parameters (those past #6)\n"
       "               after doing the first sub-brick.  Subsequent volumes\n"
       "               will have the same spatial distortions as sub-brick #0,\n"
       "               plus rigid body motions only.\n"
       "\n"
       " -replacebase   = If the source has more than one sub-brick, and this\n"
       "                  option is turned on, then after the #0 sub-brick is\n"
       "                  aligned to the base, the aligned #0 sub-brick is used\n"
       "                  as the base image for subsequent source sub-bricks.\n"
       "\n"
       " -replacemeth m = After sub-brick #0 is aligned, switch to method 'm'\n"
       "                  for later sub-bricks.  For use with '-replacebase'.\n"
       "\n"
       " -EPI        = Treat the source dataset as being composed of warped\n"
       "               EPI slices, and the base as comprising anatomically\n"
       "               'true' images.  Only phase-encoding direction image\n"
       "               shearing and scaling will be allowed with this option.\n"
       "       **N.B.: For most people, the base dataset will be a 3dSkullStrip-ed\n"
       "               T1-weighted anatomy (MPRAGE or SPGR).  If you don't remove\n"
       "               the skull first, the EPI images (which have little skull\n"
       "               visible due to fat-suppression) might expand to fit EPI\n"
       "               brain over T1-weighted skull.\n"
       "       **N.B.: Usually, EPI datasets don't have as complete slice coverage\n"
       "               of the brain as do T1-weighted datasets.  If you don't use\n"
       "               some option (like '-EPI') to suppress scaling in the slice-\n"
       "               direction, the EPI dataset is likely to stretch the slice\n"
       "               thicknesss to better 'match' the T1-weighted brain coverage.\n"
#if 0
       "       **N.B.: '-EPI' turns on '-warpfreeze -replacebase -replacemeth ls'.\n"
       "               To disable '-replacemeth ls', use '-replacemeth 0' after '-EPI'.\n"
#else
       "       **N.B.: '-EPI' turns on '-warpfreeze -replacebase'.\n"
#endif
       "               You can use '-nowarpfreeze' and/or '-noreplacebase' AFTER the\n"
       "               '-EPI' on the command line if you do not want these options used.\n"
       "\n"
       " -parfix n v   = Fix parameter #n to be exactly at value 'v'.\n"
       " -parang n b t = Allow parameter #n to range only between 'b' and 't'.\n"
       "                 If not given, default ranges are used.\n"
       " -parini n v   = Initialize parameter #n to value 'v', but then\n"
       "                 allow the algorithm to adjust it.\n"
       "         **N.B.: Multiple '-par...' options can be used, to constrain\n"
       "                 multiple parameters.\n"
       "         **N.B.: -parini has no effect if -twopass is used, since\n"
       "                 the -twopass algorithm carries out its own search\n"
       "                 for initial parameters.\n"
       "\n"
       " -maxrot dd    = Allow maximum rotation of 'dd' degrees.  Equivalent\n"
       "                 to '-parang 4 -dd dd -parang 5 -dd dd -parang 6 -dd dd'\n"
       "                 [Default=30 degrees]\n"
       " -maxshf dd    = Allow maximum shift of 'dd' millimeters.  Equivalent\n"
       "                 to '-parang 1 -dd dd -parang 2 -dd dd -parang 3 -dd dd'\n"
       "                 [Default=33%% of the size of the base image]\n"
       "         **N.B.: This max shift setting is relative to the center-of-mass\n"
       "                 shift, if the '-cmass' option is used.\n"
       " -maxscl dd    = Allow maximum scaling factor to be 'dd'.  Equivalent\n"
       "                 to '-parang 7 1/dd dd -parang 8 1/dd dd -paran2 9 1/dd dd'\n"
       "                 [Default=1.2=image can go up or down 20%% in size]\n"
#if 0
       "\n"
       " -matini mmm   = Initialize 3x4 affine transformation matrix to 'mmm',\n"
       "                 which is either a .1D file or an expression in the\n"
       "                 syntax of program 1dmatcalc.  Using this option is\n"
       "                 like using '-parini' on all affine matrix parameters.\n"
#endif
       "\n"
       " -master mmm = Write the output dataset on the same grid as dataset\n"
       "               'mmm'.  If this option is NOT given, the base dataset\n"
       "               is the master.\n"
       "       **N.B.: 3dAllineate transforms the source dataset to be 'similar'\n"
       "               to the base image.  Therefore, the coordinate system\n"
       "               of the master dataset is interpreted as being in the\n"
       "               reference system of the base image.  It is thus vital\n"
       "               that these finite 3D volumes overlap, or you will lose data!\n"
       "       **N.B.: If 'mmm' is the string 'SOURCE', then the source dataset\n"
       "               is used as the master for the output dataset grid.\n"
       "               You can also use 'BASE', which is of course the default.\n"
       "\n"
       " -mast_dxyz del = Write the output dataset using grid spacings of\n"
       "  *OR*            'del' mm.  If this option is NOT given, then the\n"
       " -newgrid del     grid spacings in the master dataset will be used.\n"
       "                  This option is useful when registering low resolution\n"
       "                  data (e.g., EPI time series) to high resolution\n"
       "                  datasets (e.g., MPRAGE) where you don't want to\n"
       "                  consume vast amounts of disk space interpolating\n"
       "                  the low resolution data to some artificially fine\n"
       "                  (and meaningless) spatial grid.\n"
     ) ;

     printf(
      "\n"
      "----------------------------------------------\n"
      "DEFINITION OF AFFINE TRANSFORMATION PARAMETERS\n"
      "----------------------------------------------\n"
      "The 3x3 spatial transformation matrix is calculated as [S][D][U],\n"
      "where [S] is the shear matrix,\n"
      "      [D] is the scaling matrix, and\n"
      "      [U] is the rotation (proper orthogonal) matrix.\n"
      "Thes matrices are specified in DICOM-ordered (x=-R+L,y=-A+P,z=-I+S)\n"
      "coordinates as:\n"
      "\n"
      "  [U] = [Rotate_y(param#6)] [Rotate_x(param#5)] [Rotate_z(param #4)]\n"
      "        (angles are in degrees)\n"
      "\n"
      "  [D] = diag( param#7 , param#8 , param#9 )\n"
      "\n"
      "        [    1        0     0 ]        [ 1 param#10 param#11 ]\n"
      "  [S] = [ param#10    1     0 ]   OR   [ 0    1     param#12 ]\n"
      "        [ param#11 param#12 1 ]        [ 0    0        1     ]\n"
      "\n"
      "The shift vector comprises parameters #1, #2, and #3.\n"
      "\n"
      "The goal of the program is to find the warp parameters such that\n"
      "   I([x]_warped) 'is similar to' J([x]_in)\n"
      "as closely as possible in some sense of 'similar', where J(x) is the\n"
      "base image, and I(x) is the source image.\n"
      "\n"
      "Using '-parfix', you can specify that some of these parameters\n"
      "are fixed.  For example, '-shift_rotate_scale' is equivalent\n"
      "'-affine_general -parfix 10 0 -parfix 11 0 -parfix 12 0'.\n"
      "Don't even think of using the '-parfix' option unless you grok\n"
      "this example!\n"
      "\n"
      "----------- Special Note for the '-EPI' Option's Coordinates -----------\n"
      "In this case, the parameters above are with reference to coordinates\n"
      "  x = frequency encoding direction (by default, first axis of dataset)\n"
      "  y = phase encoding direction     (by default, second axis of dataset)\n"
      "  z = slice encoding direction     (by default, third axis of dataset)\n"
      "This option lets you freeze some of the warping parameters in ways that\n"
      "make physical sense, considering how echo-planar images are acquired.\n"
      "The x- and z-scaling parameters are disabled, and shears will only affect\n"
      "the y-axis.  Thus, there will be only 9 free parameters when '-EPI' is\n"
      "used.  If desired, you can use a '-parang' option to allow the scaling\n"
      "fixed parameters to vary (put these after the '-EPI' option):\n"
      "  -parang 7 0.833 1.20     to allow x-scaling\n"
      "  -parang 9 0.833 1.20     to allow z-scaling\n"
      "You could also fix some of the other parameters, if that makes sense\n"
      "in your situation; for example, to disable out of slice rotations:\n"
      "  -parfix 5 0  -parfix 6 0\n"
      "and to disable out of slice translation:\n"
      "  -parfix 3 0\n"
      "NOTE WELL: If you use '-EPI', then the output warp parameters (e.g., in\n"
      "           '-1Dparam_save') apply to the (freq,phase,slice) xyz coordinates,\n"
      "           NOT to the DICOM xyz coordinates, so equivalent transformations\n"
      "           will be expressed with different sets of parameters entirely\n"
      "           than if you don't use '-EPI'!  This comment does NOT apply\n"
      "           to the output of '-1Dmatrix_save', since that matrix is\n"
      "           defined relative to the RAI (DICOM) spatial coordinates.\n"
     ) ;

     printf(
      "\n"
      "*********** CHANGING THE ORDER OF MATRIX APPLICATION ***********\n"
      "\n"
      "  -SDU or -SUD }= Set the order of the matrix multiplication\n"
      "  -DSU or -DUS }= for the affine transformations:\n"
      "  -USD or -UDS }=   S = triangular shear (params #10-12)\n"
      "                    D = diagonal scaling matrix (params #7-9)\n"
      "                    U = rotation matrix (params #4-6)\n"
      "                  Default order is '-SDU', which means that\n"
      "                  the U matrix is applied first, then the\n"
      "                  D matrix, then the S matrix.\n"
      "\n"
      "  -Supper      }= Set the S matrix to be upper or lower\n"
      "  -Slower      }= triangular [Default=lower triangular]\n"
      "\n"
      "  -ashift OR   }= Apply the shift parameters (#1-3) after OR\n"
      "  -bshift      }= before the matrix transformation. [Default=after]\n"
     ) ;

     printf(
      "\n"
      "            ==================================================\n"
      "        ===== RWCox - September 2006 - Live Long and Prosper =====\n"
      "            ==================================================\n"
      "\n"
      "         ********************************************************\n"
      "        *** From Webster's Dictionary: Allineate == 'to align' ***\n"
      "         ********************************************************\n"
     ) ;

     /*......................................................................*/

     if( argc > 1 &&
        ( strcmp(argv[1],"-HELP") ==0 ||
          strcmp(argv[1],"-POMOC")==0 || AFNI_yesenv("AFNI_POMOC") ) ){
       printf(
        "\n"
        "===========================================================================\n"
        "                TOP SECRET HIDDEN OPTIONS (-HELP or -POMOC)\n"
        "---------------------------------------------------------------------------\n"
        "                ** N.B.: Most of these are experimental! **\n"
        "===========================================================================\n"
        "\n"
        " -num_rtb n  = At the beginning of the fine pass, the best set of results\n"
        "               from the coarse pass are 'refined' a little by further\n"
        "               optimization, before the single best one is chosen for\n"
        "               for the final fine optimization.\n"
        "              * This option sets the maximum number of cost functional\n"
        "                evaluations to be used (for each set of parameters)\n"
        "                in this step.\n"
        "              * The default is 99; a larger value will take more CPU\n"
        "                time but may give more robust results.\n"
        "              * If you want to skip this step entirely, use '-num_rtb 0'.\n"
        "                then, the best of the coarse pass results is taken\n"
        "                straight to the final optimization passes.\n"
        "       **N.B.: If you use '-VERB', you will see that one extra case\n"
        "               is involved in this initial fine refinement step; that\n"
        "               case is starting with the identity transformation, which\n"
        "               helps insure against the chance that the coarse pass\n"
        "               optimizations ran totally amok.\n"
        " -nocast     = By default, parameter vectors that are too close to the\n"
        "               best one are cast out at the end of the coarse pass\n"
        "               refinement process. Use this option if you want to keep\n"
        "               them all for the fine resolution pass.\n"
        " -norefinal  = Do NOT re-start the fine iteration step after it\n"
        "               has converged.  The default is to re-start it, which\n"
        "               usually results in a small improvement to the result\n"
        "               (at the cost of CPU time).  This re-start step is an\n"
        "               an attempt to avoid a local minimum trap.  It is usually\n"
        "               not necessary, but sometimes helps.\n"
        "\n"
        " -savehist sss = Save start and final 2D histograms as PGM\n"
        "                 files, with prefix 'sss' (cost: cr mi nmi hel).\n"
        "                * if filename contains 'FF', floats is written\n"
        "                * these are the weighted histograms!\n"
        "                * -savehist will also save histogram files when\n"
        "                  the -allcost evaluations takes place\n"
        "                * this option is mostly useless unless '-histbin' is\n"
        "                  also used\n"
#if 0
        " -seed iii     = Set random number seed (for coarse startup search)\n"
        "                 to 'iii'.\n"
        "                 [Default==7654321; if iii==0, a unique value is used]\n"
#endif
        " -median       = Smooth with median filter instead of Gaussian blur.\n"
        "                 (Somewhat slower, and not obviously useful.)\n"
        " -powell m a   = Set the Powell NEWUOA dimensional parameters to\n"
        "                 'm' and 'a' (cf. source code in powell_int.c).\n"
        "                 The number of points used for approximating the\n"
        "                 cost functional is m*N+a, where N is the number\n"
        "                 of parameters being optimized.  The default values\n"
        "                 are m=2 and a=3.  Larger values will probably slow\n"
        "                 the program down for no good reason.\n"
        " -target ttt   = Same as '-source ttt'.  In the earliest versions,\n"
        "                 what I now call the 'source' dataset was called the\n"
        "                 'target' dataset:\n"
        "                    Try to remember the kind of September (2006)\n"
        "                    When life was slow and oh so mellow\n"
        "                    Try to remember the kind of September\n"
        "                    When grass was green and source was target.\n"
#if 0
        " -targijk      = Align source xyz axes with ijk indexes, rather than\n"
        "                 using coordinates in target header.\n"
#endif
        " -Xwarp       =} Change the warp/matrix setup so that only the x-, y-, or z-\n"
        " -Ywarp       =} axis is stretched & sheared.  Useful for EPI, where 'X',\n"
        " -Zwarp       =} 'Y', or 'Z' corresponds to the phase encoding direction.\n"
        " -FPS fps      = Generalizes -EPI to arbitrary permutation of directions.\n"
        " -histpow pp   = By default, the number of bins in the histogram used\n"
        "                 for calculating the Hellinger, Mutual Information, and\n"
        "                 Correlation Ratio statistics is n^(1/3), where n is\n"
        "                 the number of data points.  You can change that exponent\n"
        "                 to 'pp' with this option.\n"
        " -histbin nn   = Or you can just set the number of bins directly to 'nn'.\n"
        " -eqbin   nn   = Use equalized marginal histograms with 'nn' bins.\n"
        " -clbin   nn   = Use 'nn' equal-spaced bins except for the bot and top,\n"
        "                 which will be clipped (thus the 'cl').  If nn is 0, the\n"
        "                 program will pick the number of bins for you.\n"
        "                 **N.B.: '-clbin 0' is now the default [25 Jul 2007];\n"
        "                         if you want the old all-equal-spaced bins, use\n"
        "                         '-histbin 0'.\n"
        "                 **N.B.: '-clbin' only works when the datasets are\n"
        "                         non-negative; any negative voxels in either\n"
        "                         the input or source volumes will force a switch\n"
        "                         to all equal-spaced bins.\n"
        " -wtmrad  mm   = Set autoweight/mask median filter radius to 'mm' voxels.\n"
        " -wtgrad  gg   = Set autoweight/mask Gaussian filter radius to 'gg' voxels.\n"
        " -nmsetup nn   = Use 'nn' points for the setup matching [default=98756]\n"
        " -ignout       = Ignore voxels outside the warped source dataset.\n"
        "\n"
        " -blok bbb     = Blok definition for the 'lp?' (Local Pearson) cost\n"
        "                 functions: 'bbb' is one of\n"
        "                   'BALL(r)' or 'CUBE(r)' or 'RHDD(r)' or 'TOHD(r)'\n"
        "                 corresponding to\n"
        "                   spheres or cubes or rhombic dodecahedra or\n"
        "                   truncated octahedra\n"
        "                 where 'r' is the size parameter in mm.\n"
        "                 [Default is 'RHDD(6.54321)' (rhombic dodecahedron)]\n"
        "\n"
        " -allcost        = Compute ALL available cost functionals and print them\n"
        "                   at various points.\n"
        " -allcostX       = Compute and print ALL available cost functionals for the\n"
        "                   un-warped inputs, and then quit.\n"
        " -allcostX1D p q = Compute ALL available cost functionals for the set of\n"
        "                   parameters given in the 1D file 'p' (12 values per row),\n"
        "                   write them to the 1D file 'q', then exit. (For you, Zman)\n"
        "                  * N.B.: If -fineblur is used, that amount of smoothing\n"
        "                          will be applied prior to the -allcostX evaluations.\n"
       ) ;

       printf("\n"
        "===========================================================================\n" );
       printf("\n"
              " Hidden experimental cost functionals:\n") ;
       for( ii=0 ; ii < NMETH ; ii++ )
        if( !meth_visible[ii] )
          printf( "   %-4s *OR*  %-16s= %s\n" ,
                  meth_shortname[ii] , meth_longname[ii] , meth_username[ii] );

       printf("\n"
              " Cost functional descriptions (for use with -allcost output):\n") ;
       for( ii=0 ; ii < NMETH ; ii++ )
         printf("   %-4s:: %s\n",
                meth_shortname[ii] , meth_costfunctional[ii] ) ;

       printf("\n") ;
       printf(" * N.B.: Some cost functional values (as printed out herein)\n"
              "   are negated (e.g., 'hel', 'mi'), so that the best image\n"
              "   alignment will be found when the cost is minimized.  See\n"
              "   the descriptions above and the references below for more\n"
              "   details for each functional.\n");
       printf("\n") ;
       printf(" * For more information about the 'lpc' functional, see\n"
              "     ZS Saad, DR Glen, G Chen, MS Beauchamp, R Desai, RW Cox.\n"
              "       A new method for improving functional-to-structural\n"
              "       MRI alignment using local Pearson correlation.\n"
              "       NeuroImage 44: 839-848, 2009.\n"
              "     http://dx.doi.org/10.1016/j.neuroimage.2008.09.037\n"
              "     http://afni.nimh.nih.gov/sscc/rwcox/papers/LocalPearson2009.pdf\n"
              "   The '-blok' option can be used to control the regions\n"
              "   (size and shape) used to compute the local correlations.\n");
       printf("\n") ;
       printf(" * For more information about the 'cr' functionals, see\n"
              "     http://en.wikipedia.org/wiki/Correlation_ratio\n"
              "   Note that CR(x,y) is not the same as CR(y,x), which\n"
              "   is why there are symmetrized versions of it available.\n") ;
       printf("\n") ;
       printf(" * For more information about the 'mi', 'nmi', and 'je'\n"
              "   cost functionals, see\n"
              "     http://en.wikipedia.org/wiki/Mutual_information\n"
              "     http://en.wikipedia.org/wiki/Joint_entropy\n"
              "     http://www.cs.jhu.edu/~cis/cista/746/papers/mutual_info_survey.pdf\n");
       printf("\n") ;
       printf(" * For more information about the 'hel' functional, see\n"
              "     http://en.wikipedia.org/wiki/Hellinger_distance\n"     ) ;
       printf("\n") ;
       printf(" * Some cost functionals (e.g., 'mi', 'cr', 'hel') are\n"
              "   computed by creating a 2D joint histogram of the\n"
              "   base and source image pair.  Various options above\n"
              "   (e.g., '-histbin', etc.) can be used to control the\n"
              "   number of bins used in the histogram on each axis.\n"
              "   (If you care to control the program in such detail!)\n"  ) ;
       printf("\n") ;
       printf(" * Minimization of the chosen cost functional is done via\n"
              "   the NEWUOA software, described in detail in\n"
              "     MJD Powell. 'The NEWUOA software for unconstrained\n"
              "       optimization without derivatives.' In: GD Pillo,\n"
              "       M Roma (Eds), Large-Scale Nonlinear Optimization.\n"
              "       Springer, 2006.\n"
              "     http://www.damtp.cam.ac.uk/user/na/NA_papers/NA2004_08.pdf\n");

       printf("\n"
        "===========================================================================\n"
        "\n"
#if 0
        " -nwarp [type [order]] = Experimental nonlinear warp\n"
        "                         'type' = 'bilinear'  or\n"
        "                                  'trig'      or\n"
        "                                  'legendre'  or\n"
        "                                  'gegenbauer'\n"
        "                         'order'= value in range 2..5 (inclusive)\n"
#else
        " -nwarp type = Experimental nonlinear warp:\n"
        "              * At present, the only 'type' is 'bilinear',\n"
        "                as in 3dWarpDrive, with 39 parameters.\n"
        "              * I plan to implement more complicated nonlinear\n"
        "                warps in the future, someday ....\n"
        "              * -nwarp can only be applied to a source dataset\n"
        "                that has a single sub-brick!\n"
        "              * -1Dparam_save and -1Dparam_apply work with\n"
        "                bilinear warps; see the Notes for more information.\n"
        "-nwarp NOTES:\n"
        "-------------\n"
        "* -nwarp is slow!\n"
        "* Check the results to make sure the optimizer didn't run amok!\n"
        "   (You should always do this with any registration software.)\n"
        "* If you use -1Dparam_save, then you can apply the bilinear\n"
        "   warp to another dataset using -1Dparam_apply in a later\n"
        "   3dAllineate run. To do so, use '-nwarp bilinear' in both\n"
        "   runs, so that the program knows what the extra parameters\n"
        "   in the file are to be used for.\n"
        "  ++ 43 values are saved in 1 row of the param file.\n"
        "  ++ The first 12 are the affine parameters\n"
        "  ++ The next 27 are the D1,D2,D3 matrix parameters.\n"
        "  ++ The final 'extra' 4 values are used to specify\n"
        "      the center of coordinates (vector Xc below), and a\n"
        "      pre-computed scaling factor applied to parameters #13..39.\n"
        "* Bilinear warp formula:\n"
        "   Xout = inv[ I + {D1 (Xin-Xc) | D2 (Xin-Xc) | D3 (Xin-Xc)} ] [ A Xin ]\n"
        "  where Xin  = input vector  (base dataset coordinates)\n"
        "        Xout = output vector (source dataset coordinates)\n"
        "        Xc   = center of coordinates used for nonlinearity\n"
        "               (will be the center of the base dataset volume)\n"
        "        A    = matrix representing affine transformation (12 params)\n"
        "        I    = 3x3 identity matrix\n"
        "    D1,D2,D3 = three 3x3 matrices (the 27 'new' parameters)\n"
        "               * when all 27 parameters == 0, warp is purely affine\n"
        "     {P|Q|R} = 3x3 matrix formed by adjoining the 3-vectors P,Q,R\n"
        "    inv[...] = inverse 3x3 matrix of stuff inside '[...]'\n"
        "* The inverse of a bilinear transformation is another bilinear\n"
        "   transformation.  Someday, I may write a program that will let\n"
        "   you compute that inverse transformation, so you can use it for\n"
        "   some cunning and devious purpose.\n"
        "* If you expand the inv[...] part of the above formula in a 1st\n"
        "   order Taylor series, you'll see that a bilinear warp is basically\n"
        "   a quadratic warp, with the additional feature that its inverse\n"
        "   is directly computable (unlike a pure quadratic warp).\n"
        "* Is '-nwarp bilinear' useful?  Try it and tell me!\n"
#endif
        "\n"
        "===========================================================================\n"
       ) ;

     } else {

       printf(
        "\n"
        "[[[ To see a plethora of advanced/experimental options, use '-HELP'. ]]]\n");

     }

#ifdef USE_OMP
     printf(
       "\n"
       " *************************************************************************\n"
       "* This version of 3dAllineate is compiled using OpenMP, a semi-automatic\n"
       "   parallelizer software toolkit.  The number of CPU threads used will\n"
       "   default to the maximum number on your system.  You can control this\n"
       "   value by setting environment variable OMP_NUM_THREADS to some smaller\n"
       "   value (including 1).  Setting OMP_NUM_THREADS to 0 resets OpenMP back\n"
       "   to its default state of using all CPUs available.\n"
       "* OpenMP may or may not speed up the program significantly.  Limited\n"
       "   tests show that it provides some benefit, particularly when using\n"
       "   the more complicated interpolation methods (e.g., '-cubic' and/or\n"
       "   '-final wsinc5'), for up to 3-4 CPU threads.\n"
       "* But the speedup is definitely not linear in the number of threads, alas.\n"
       "   Probably because my parallelization efforts were pretty limited.\n"
       "* The number of CPUs on this particular computer system is %d.\n"
       " *************************************************************************\n"
       , omp_get_num_procs()
     ) ;
#endif

     PRINT_COMPILE_DATE ; exit(0);
   }

   /**--- bookkeeping and marketing ---**/

#ifdef USING_MCW_MALLOC
   enable_mcw_malloc() ;
#endif

   mainENTRY("3dAllineate"); machdep();
   AFNI_logger("3dAllineate",argc,argv);
   PRINT_VERSION("3dAllineate"); AUTHOR("Emperor Zhark");
   THD_check_AFNI_version("3dAllineate");
   (void)COX_clock_time() ;

   /**--- process command line options ---**/

   iarg = 1 ;
   while( iarg < argc && argv[iarg][0] == '-' ){

     /*-----*/

     if( strcmp(argv[iarg],"-nwarp") == 0 ){     /* 03 Apr 2008 = SECRET */
       nwarp_pass = 1 ; iarg++ ;

       if( iarg < argc && isalpha(argv[iarg][0]) ){
         if( strncasecmp(argv[iarg],"tri",3) == 0 ){
           nwarp_type = WARPFIELD_TRIG_TYPE ;
         } else if( strncasecmp(argv[iarg],"leg",3) == 0 ){
           nwarp_type = WARPFIELD_LEGEN_TYPE ;
         } else if( strncasecmp(argv[iarg],"geg",3) == 0 ){
           nwarp_type = WARPFIELD_GEGEN_TYPE ;
         } else if( strncasecmp(argv[iarg],"bil",3) == 0 ){
           nwarp_type = WARP_BILINEAR ;
         } else {
           WARNING_message("unknown -nwarp type '%s'",argv[iarg]) ;
         }
         warp_code = WARP_AFFINE ; iarg++ ;
       }

       if( iarg < argc && isdigit(argv[iarg][0]) ){
         nwarp_order = (float)strtod(argv[iarg],NULL) ;
         if( nwarp_order < 2.0f || nwarp_order > 5.0f ){
           WARNING_message("illegal -nwarp order '%s'",argv[iarg]) ;
           nwarp_order = 2.9f ;
         }
         if( nwarp_type == WARP_BILINEAR )
           WARNING_message("'order' is meaningless for bilinear warp") ;
         iarg++ ;
       }

       /* change some other parameters from their defaults */

       do_refinal = 0 ;
       continue ;
     }

     /*-----*/

     if( strcmp(argv[iarg],"-norefinal") == 0 ){ /* 14 Nov 2007 */
       do_refinal = 0 ; iarg++ ; continue ;      /* SECRET OPTION */
     }

     /*-----*/

     if( strcmp(argv[iarg],"-allcost") == 0 ){   /* 19 Sep 2007 */
       do_allcost = 1 ; iarg++ ; continue ;      /* SECRET OPTIONS */
     }
     if( strcmp(argv[iarg],"-allcostX") == 0 ){
       do_allcost = -1 ; iarg++ ; continue ;
     }
     if( strcmp(argv[iarg],"-allcostX1D") == 0 ){ /* 02 Sep 2008 */
       MRI_IMAGE *qim ;
       do_allcost = -2 ;
       qim = mri_read_1D( argv[++iarg] ) ;
       if( qim == NULL )
         ERROR_exit("Can't read -allcostX1D '%s'",argv[iarg]) ;
       allcostX1D = mri_transpose(qim) ; mri_free(qim) ;
       if( allcostX1D->nx < 12 )
         ERROR_exit("-allcostX1D '%s' has only %d values per row!" ,
                    argv[iarg] , allcostX1D->nx ) ;
       allcostX1D_outname = strdup(argv[++iarg]) ;
       ++iarg ; continue ;
     }

     /*-----*/

     if( strncmp(argv[iarg],"-cmass",6) == 0 ){
       if( argv[iarg][6] == '+' ){
         if (strchr(argv[iarg]+6,'a')) {
            do_cmass = -1; /* ZSS */
         } else {
            do_cmass =    (strchr(argv[iarg]+6,'x') != NULL)
                      + 2*(strchr(argv[iarg]+6,'y') != NULL)
                      + 4*(strchr(argv[iarg]+6,'z') != NULL) ;
         }
         if( do_cmass == 0 )
           ERROR_exit("Don't understand coordinates in '%s",argv[iarg]) ;
       } else {
         do_cmass = 7 ;  /* all coords */
       }
       iarg++ ; continue ;
     }
     if( strcmp(argv[iarg],"-nocmass") == 0 ){
       do_cmass = 0 ; iarg++ ; continue ;
     }

     /*-----*/

     if( strcmp(argv[iarg],"-ignout") == 0 ){               /* SECRET OPTION */
       GA_set_outval(1.e+33); ignout = 1; iarg++; continue; /* 28 Feb 2007  */
     }

     /*-----*/

     if( strcmp(argv[iarg],"-matini") == 0 ){
       if( matini != NULL ) ERROR_exit("Can't have multiple %s options!",argv[iarg]) ;
       if( ++iarg >= argc ) ERROR_exit("no argument after '%s'!",argv[iarg-1]) ;
       if( strncmp(argv[iarg],"1D:",3) == 0 ||
           (strchr(argv[iarg],' ') == NULL && argv[iarg][0] != '&') ){
         matini = mri_read_1D( argv[iarg] ) ;
         if( matini == NULL ) ERROR_exit("Can't read -matini file '%s'",argv[iarg]);
       } else {
         matini = mri_matrix_evalrpn( argv[iarg] ) ;
         if( matini == NULL ) ERROR_exit("Can't evaluate -matini expression");
       }
       if( matini->nx < 3 || matini->ny < 3 )
         ERROR_exit("-matini matrix has nx=%d and ny=%d (should be at least 3)",
                    matini->nx,matini->ny) ;
       else if( matini->nx > 3 || matini->ny > 4 )
         WARNING_message("-matini matrix has nx=%d and ny=%d (should be 3x4)",
                    matini->nx,matini->ny) ;

       WARNING_message("-matini is not yet implemented!") ;
       iarg++ ; continue ;
     }

     /*-----*/

     if( strcmp(argv[iarg],"-mast_dxyz") == 0 ||
         strcmp(argv[iarg],"-dxyz_mast") == 0 ||
         strcmp(argv[iarg],"-newgrid"  ) == 0   ){

       if( ++iarg >= argc ) ERROR_exit("no argument after '%s'!",argv[iarg-1]) ;
       dxyz_mast = strtod(argv[iarg],NULL) ;
       if( dxyz_mast <= 0.0 )
         ERROR_exit("Illegal value '%s' after -mast_dxyz",argv[iarg]) ;
       if( dxyz_mast <= 0.5 )
         WARNING_message("Small value %g after -mast_dxyz",dxyz_mast) ;
       iarg++ ; continue ;
     }

     /*-----*/

     if( strcmp(argv[iarg],"-nmsetup") == 0 ){
       if( ++iarg >= argc ) ERROR_exit("no argument after '%s'!",argv[iarg-1]) ;
       nmatch_setup = (int)strtod(argv[iarg],NULL) ;
       if( nmatch_setup < 9999 ) nmatch_setup = 23456 ;
       iarg++ ; continue ;
     }

     /*-----*/

     if( strncmp(argv[iarg],"-master",6) == 0 ){
       if( dset_mast != NULL || tb_mast )
         ERROR_exit("Can't have multiple %s options!",argv[iarg]) ;
       if( ++iarg >= argc ) ERROR_exit("no argument after '%s'!",argv[iarg-1]) ;
       if( strcmp(argv[iarg],"SOURCE") == 0 ){  /* 19 Jul 2007 */
         tb_mast = 1 ;
       } else if( strcmp(argv[iarg],"BASE") == 0 ){
         tb_mast = 2 ;
       } else {
         dset_mast = THD_open_dataset( argv[iarg] ) ;
         if( dset_mast == NULL )
           ERROR_exit("can't open -master dataset '%s'",argv[iarg]);
       }
       iarg++ ; continue ;
     }

     /*-----*/

     if( strcmp(argv[iarg],"-seed") == 0 ){   /* SECRET OPTION */
       if( ++iarg >= argc ) ERROR_exit("no argument after '%s'!",argv[iarg-1]) ;
       seed = (long)strtod(argv[iarg],NULL) ; iarg++ ; continue ;
     }

     /*-----*/

     if( strcmp(argv[iarg],"-powell") == 0 ){  /* SECRET OPTION */
       if( ++iarg >= argc-1 ) ERROR_exit("no arguments after '%s'!",argv[iarg-1]) ;
       powell_mm = (float)strtod(argv[iarg++],NULL) ;
       powell_aa = (float)strtod(argv[iarg++],NULL) ;
       continue ;
     }

     /*-----*/

     if( strncmp(argv[iarg],"-weight_frac",11) == 0 ){
       if( ++iarg >= argc ) ERROR_exit("no argument after '%s'!",argv[iarg-1]) ;
       nmask_frac = atof( argv[iarg] ) ;
       if( nmask_frac < 0.0f || nmask_frac > 1.0f )
         ERROR_exit("-weight_frac must be between 0.0 and 1.0 (have '%s')",argv[iarg]);
       wtspecified = 1 ; iarg++ ; continue ;
     }

     /*-----*/

     if( strncmp(argv[iarg],"-weight",6) == 0 ){
       auto_weight = 0 ;
       if( dset_weig != NULL ) ERROR_exit("Can't have multiple %s options!",argv[iarg]) ;
       if( ++iarg >= argc ) ERROR_exit("no argument after '%s'!",argv[iarg-1]) ;
       dset_weig = THD_open_dataset( argv[iarg] ) ;
       if( dset_weig == NULL ) ERROR_exit("can't open -weight dataset '%s'",argv[iarg]);
       wtspecified = 1 ; iarg++ ; continue ;
     }

     /*-----*/

     if( strncmp(argv[iarg],"-autoweight",11) == 0 ){
       char *cpt ;
       if( dset_weig != NULL ) ERROR_exit("Can't use -autoweight AND -weight!") ;
       auto_weight = 1 ; auto_string = argv[iarg] ;
       cpt = strstr(auto_string,"+") ;
       if( cpt != NULL && *(cpt+1) != '\0' )      /* 31 Jul 2007 */
         auto_wclip = (float)strtod(cpt+1,NULL) ;
       cpt = strstr(auto_string,"**") ;
       if( cpt != NULL && *(cpt+2) != '\0' )      /* 10 Sep 2007 */
         auto_wpow = (float)strtod(cpt+2,NULL) ;
       wtspecified = 1 ; iarg++ ; continue ;
     }

     if( strncmp(argv[iarg],"-automask",9) == 0 ){
       if( dset_weig != NULL ) ERROR_exit("Can't use -automask AND -weight!") ;
       auto_weight = 2 ; auto_string = argv[iarg] ;
       if( auto_string[9] == '+' && auto_string[10] != '\0' )
         auto_dilation = (int)strtod(auto_string+10,NULL) ;
       wtspecified = 1 ; iarg++ ; continue ;
     }

     if( strncmp(argv[iarg],"-noauto",6) == 0 ||
         strncmp(argv[iarg],"-nomask",6) == 0   ){
       wtspecified = 1 ; auto_weight = 0 ; iarg++ ; continue ;
     }

     if( strcmp(argv[iarg],"-autobox") == 0 ){
       wtspecified = 1 ; auto_weight = 3 ; auto_string = "-autobox" ;
       iarg++ ; continue ;
     }

     /*-----*/

     if( strcmp(argv[iarg],"-source_mask") == 0 ){  /* 07 Aug 2007 */
       byte *mmm ; THD_3dim_dataset *dset_tmask ;
       if( im_tmask != NULL )
         ERROR_exit("Can't use -source_mask twice!") ;
       if( auto_tmask )
         ERROR_exit("Can't use -source_mask AND -source_automask!") ;
       if( ++iarg >= argc ) ERROR_exit("no argument after '%s'!",argv[iarg-1]) ;
       dset_tmask = THD_open_dataset( argv[iarg] ) ;
       if( dset_tmask == NULL )
         ERROR_exit("can't open -source_mask dataset '%s'",argv[iarg]);
       mmm = THD_makemask( dset_tmask , 0 , 1.0f,-1.0f ) ;
       if( mmm == NULL )
         ERROR_exit("Can't use -source_mask '%s' for some reason",argv[iarg]) ;
       im_tmask = mri_new_vol_empty(
                   DSET_NX(dset_tmask),DSET_NY(dset_tmask),DSET_NZ(dset_tmask) ,
                   MRI_byte ) ;
       DSET_delete(dset_tmask) ; /* ZSS: Moved here cause that's
                                    right and proper*/
       mri_fix_data_pointer( mmm , im_tmask ) ;
       ntmask = THD_countmask( im_tmask->nvox , mmm ) ;
       if( ntmask < 666 )
         ERROR_exit("Too few (%d) voxels in -source_mask!",ntmask) ;
       if( verb ) INFO_message("%d voxels in -source_mask",ntmask) ;
       iarg++ ; continue ;
     }

     if( strncmp(argv[iarg],"-source_automask",16) == 0 ){  /* 07 Aug 2007 */
       if( im_tmask != NULL )
         ERROR_exit("Can't use -source_automask AND -source_mask!") ;
       auto_tmask = 1 ; auto_tstring = argv[iarg] ;
       if( auto_tstring[16] == '+' && auto_string[17] != '\0' )
         auto_tdilation = (int)strtod(auto_tstring+17,NULL) ;
       iarg++ ; continue ;
     }

     /*-----*/

     if( strncmp(argv[iarg],"-wtprefix",6) == 0 ){
       if( ++iarg >= argc ) ERROR_exit("no argument after '%s'!",argv[iarg-1]) ;
       if( !THD_filename_ok(argv[iarg]) )
         ERROR_exit("badly formed filename: '%s' '%s'",argv[iarg-1],argv[iarg]) ;
       wtprefix = argv[iarg] ; iarg++ ; continue ;
     }

     /*-----*/

     if( strcmp(argv[iarg],"-savehist") == 0 ){  /* SECRET OPTION */
       if( ++iarg >= argc ) ERROR_exit("no argument after '%s'!",argv[iarg-1]) ;
       if( !THD_filename_ok(argv[iarg]) )
         ERROR_exit("badly formed filename: '%s' '%s'",argv[iarg-1],argv[iarg]) ;
       save_hist = argv[iarg] ; iarg++ ; continue ;
     }

     /*-----*/

     if( strcmp(argv[iarg],"-histpow") == 0 ){   /* SECRET OPTION */
       if( ++iarg >= argc ) ERROR_exit("no argument after '%s'!",argv[iarg-1]) ;
       hist_pow = strtod(argv[iarg],NULL) ;
       set_2Dhist_hpower(hist_pow) ;
       iarg++ ; continue ;
     }

     if( strcmp(argv[iarg],"-histbin") == 0 ){   /* SECRET OPTION */
       if( ++iarg >= argc ) ERROR_exit("no argument after '%s'!",argv[iarg-1]) ;
       hist_nbin = (int)strtod(argv[iarg],NULL) ;
       hist_mode = 0 ; hist_param = 0.0f ; hist_setbyuser = 1 ;
       set_2Dhist_hbin( hist_nbin ) ;
       iarg++ ; continue ;
     }

     if( strcmp(argv[iarg],"-clbin") == 0 ){   /* SECRET OPTION - 08 May 2007 */
       if( ++iarg >= argc ) ERROR_exit("no argument after '%s'!",argv[iarg-1]) ;
       hist_mode  = GA_HIST_CLEQWD ;
       hist_param = (float)strtod(argv[iarg],NULL) ; hist_setbyuser = 1 ;
       iarg++ ; continue ;
     }

     if( strcmp(argv[iarg],"-izz") == 0 ){    /* EXPERIMENTAL!! */
       THD_correlate_ignore_zerozero(1) ; iarg++ ; continue ;
     }

     if( strcmp(argv[iarg],"-eqbin") == 0 ){   /* SECRET OPTION - 08 May 2007 */
       if( ++iarg >= argc ) ERROR_exit("no argument after '%s'!",argv[iarg-1]) ;
       hist_mode  = GA_HIST_EQHIGH ;
       hist_param = (float)strtod(argv[iarg],NULL) ; hist_setbyuser = 1 ;
       if( hist_param < 3.0f || hist_param > 255.0f ){
         WARNING_message("'-eqbin %f' is illegal -- ignoring",hist_param) ;
         hist_mode = 0 ; hist_param = 0.0f ; hist_setbyuser = 0 ;
       }
       iarg++ ; continue ;
     }

     if( strcmp(argv[iarg],"-wtmrad") == 0 ){   /* SECRET OPTION */
       if( ++iarg >= argc ) ERROR_exit("no argument after '%s'!",argv[iarg-1]) ;
       wt_medsmooth = (float)strtod(argv[iarg],NULL) ;
       iarg++ ; continue ;
     }

     if( strcmp(argv[iarg],"-wtgrad") == 0 ){   /* SECRET OPTION */
       if( ++iarg >= argc ) ERROR_exit("no argument after '%s'!",argv[iarg-1]) ;
       wt_gausmooth = (float)strtod(argv[iarg],NULL) ;
       iarg++ ; continue ;
     }

     /*-----*/

     if( strcmp(argv[iarg],"-verb") == 0 ){
       verb++ ; iarg++ ; continue ;
     }
     if( strcmp(argv[iarg],"-VERB") == 0 ){
       verb+=2 ; iarg++ ; continue ;
     }
     if( strcmp(argv[iarg],"-quiet") == 0 ){  /* 10 Oct 2006 */
       verb=0 ; iarg++ ; continue ;
     }

     if( strcmp(argv[iarg],"-usetemp") == 0 ){  /* 20 Dec 2006 */
       usetemp = 1 ; iarg++ ; continue ;
     }
     if( strcmp(argv[iarg],"-nousetemp") == 0 ){
       usetemp = 0 ; iarg++ ; continue ;
     }

     /*-----*/

     if( strncmp(argv[iarg],"-floatize",6) == 0 ){
       floatize++ ; iarg++ ; continue ;
     }

     /*-----*/

     if( strncmp(argv[iarg],"-nopad",5) == 0 ){
       zeropad = 0 ; iarg++ ; continue ;
     }

     /*----- Check the various cost options -----*/

     /** -shortname **/

     for( jj=ii=0 ; ii < NMETH ; ii++ ){
       if( strcasecmp(argv[iarg]+1,meth_shortname[ii]) == 0 ){
         meth_code = jj = ii+1 ; break ;
       }
     }
     if( jj > 0 ){ iarg++ ; continue ; }  /* there was a match */

     /** -longname **/

     for( jj=ii=0 ; ii < NMETH ; ii++ ){
       if( strncasecmp(argv[iarg]+1,meth_longname[ii],7) == 0 ){
         meth_code = jj = ii+1 ; break ;
       }
     }
     if( jj > 0 ){ iarg++ ; continue ; }  /* there was a match */

     /** -cost shortname  *OR*  -cost longname **/

     if( strcmp(argv[iarg],"-cost") == 0 || strcmp(argv[iarg],"-meth") == 0 ){
       if( ++iarg >= argc ) ERROR_exit("no argument after '-cost'!") ;

       for( jj=ii=0 ; ii < NMETH ; ii++ ){
         if( strcasecmp(argv[iarg],meth_shortname[ii]) == 0 ){
           meth_code = jj = ii+1 ; break ;
         }
       }
       if( jj > 0 ){ iarg++ ; continue ; } /* there was a match */

       for( jj=ii=0 ; ii < NMETH ; ii++ ){
         if( strncasecmp(argv[iarg],meth_longname[ii],7) == 0 ){
           meth_code = jj = ii+1 ; break ;
         }
       }
       if( jj >=0 ){ iarg++ ; continue ; } /* there was a match */

       ERROR_exit("Unknown code '%s' after -cost!",argv[iarg]) ;
     }

#ifdef ALLOW_METH_CHECK
     /*----- -check costname -----*/

     if( strncasecmp(argv[iarg],"-check",5) == 0 ){
       /** if( strncmp(argv[iarg],"-CHECK",5) == 0 ) meth_median_replace = 1 ; **/
       if( ++iarg >= argc ) ERROR_exit("no argument after '%s'!",argv[iarg-1]) ;

       for( ; iarg < argc && argv[iarg][0] != '-' ; iarg++ ){
         if( meth_check_count == NMETH ) continue ; /* malicious user? */
         for( jj=ii=0 ; ii < NMETH ; ii++ ){
           if( strcasecmp(argv[iarg],meth_shortname[ii]) == 0 ){
             jj = ii+1 ; break ;
           }
         }
         if( jj > 0 ){ meth_check[ meth_check_count++ ] = jj; continue ;}

         for( jj=ii=0 ; ii < NMETH ; ii++ ){
           if( strncasecmp(argv[iarg],meth_longname[ii],7) == 0 ){
             jj = ii+1 ; break ;
           }
         }
         if( jj >=0 ){ meth_check[ meth_check_count++ ] = jj; continue ;}

         WARNING_message("Unknown code '%s' after -check!",argv[iarg]) ;
       }
       continue ;
     }
#else
     if( strncasecmp(argv[iarg],"-check",5) == 0 ){
       WARNING_message("option '%s' is no longer available",argv[iarg]) ;
       for( ; iarg < argc && argv[iarg][0] != '-' ; iarg++ ) ; /*nada*/
       continue ;
     }
#endif

     /*-----*/

     if( strcmp(argv[iarg],"-base") == 0 ){
       if( dset_base != NULL ) ERROR_exit("Can't have multiple %s options!",argv[iarg]) ;
       if( ++iarg >= argc ) ERROR_exit("no argument after '-base'!") ;
       dset_base = THD_open_dataset( argv[iarg] ) ;
       if( dset_base == NULL ) ERROR_exit("can't open -base dataset '%s'",argv[iarg]);
       ii = (int)DSET_BRICK_TYPE(dset_base,0) ;
       if( ii != MRI_float && ii != MRI_short && ii != MRI_byte )
         ERROR_exit("base dataset %s has non-scalar data type '%s'",
                    DSET_BRIKNAME(dset_base) , MRI_TYPE_name[ii] ) ;
       iarg++ ; continue ;
     }

     /*-----*/

     if( strncmp(argv[iarg],"-source",6) == 0 ||
         strncmp(argv[iarg],"-input" ,5) == 0 ||
         strncmp(argv[iarg],"-target",7) == 0   ){
       if( dset_targ != NULL ) ERROR_exit("Can't have multiple %s options!",argv[iarg]) ;
       if( ++iarg >= argc ) ERROR_exit("no argument after '%s'!",argv[iarg-1]) ;
       dset_targ = THD_open_dataset( argv[iarg] ) ;
       if( dset_targ == NULL )
         ERROR_exit("can't open -%s dataset '%s'",argv[iarg-1],argv[iarg]);
       iarg++ ; continue ;
     }

     /*-----*/

     if( strncmp(argv[iarg],"-median",5) == 0 ){        /* SECRET OPTION */
       sm_code = GA_SMOOTH_MEDIAN ; iarg++ ; continue ;
     }

     /*-----*/

     if( strncmp(argv[iarg],"-twoblur",7) == 0 ){
       if( ++iarg >= argc ) ERROR_exit("no argument after '%s'!",argv[iarg-1]) ;
       sm_rad = (float)strtod(argv[iarg],NULL) ; twopass = 1 ;
       if( sm_rad < 0.0f ) sm_rad = 0.0f ;
       iarg++ ; continue ;
     }

     if( strncmp(argv[iarg],"-fineblur",8) == 0 ){
       if( ++iarg >= argc ) ERROR_exit("no argument after '%s'!",argv[iarg-1]) ;
       fine_rad = (float)strtod(argv[iarg],NULL) ; iarg++ ; continue ;
     }

     /*-----*/

     if( strncmp(argv[iarg],"-twobest",7) == 0 ){
       if( ++iarg >= argc ) ERROR_exit("no argument after '%s'!",argv[iarg-1]) ;
       tbest = (int)strtod(argv[iarg],NULL) ; twopass = 1 ;
       if( tbest < 0 ){
         WARNING_message("-twobest %d is illegal: replacing with 0",tbest) ;
         tbest = 0 ;
       } else if( tbest > PARAM_MAXTRIAL ){
         WARNING_message("-twobest %d is illegal: replacing with %d",tbest,PARAM_MAXTRIAL) ;
         tbest = PARAM_MAXTRIAL ;
       }
       iarg++ ; continue ;
     }

     if( strncmp(argv[iarg],"-num_rtb",7) == 0 ){
       if( ++iarg >= argc ) ERROR_exit("no argument after '%s'!",argv[iarg-1]) ;
       num_rtb = (int)strtod(argv[iarg],NULL) ; twopass = 1 ;
            if( num_rtb <= 0   ) num_rtb = 0 ;
       else if( num_rtb <  66  ) num_rtb = 66 ;
       else if( num_rtb >  666 ) num_rtb = 666 ;
       iarg++ ; continue ;
     }

     if( strncmp(argv[iarg],"-nocast",6) == 0 ){
       nocast = 1 ; iarg++ ; continue ;
     }

     /*-----*/

     if( strncmp(argv[iarg],"-onepass",6) == 0 ){
       twopass = twofirst = 0 ; iarg++ ; continue ;
     }
     if( strncmp(argv[iarg],"-twopass",6) == 0 ){
       twopass = 1 ; twofirst = 0 ; iarg++ ; continue ;
     }
     if( strncmp(argv[iarg],"-twofirst",6) == 0 ){
       twofirst = twopass = 1 ; iarg++ ; continue ;
     }

     /*-----*/

     if( strncmp(argv[iarg],"-output",5) == 0 || strncmp(argv[iarg],"-prefix",5) == 0 ){
       if( prefix != NULL ) ERROR_exit("Can't have multiple %s options!",argv[iarg]) ;
       if( ++iarg >= argc ) ERROR_exit("no argument after '%s'!",argv[iarg-1]) ;
       if( !THD_filename_ok(argv[iarg]) )
         ERROR_exit("badly formed filename: '%s' '%s'",argv[iarg-1],argv[iarg]) ;
       if( strcmp(argv[iarg],"NULL") == 0 ) prefix = NULL ;
       else                                 prefix = argv[iarg] ;
       iarg++ ; continue ;
     }

     /*-----*/

     if( strncmp(argv[iarg],"-1Dfile",5) == 0 || strncmp(argv[iarg],"-1Dparam_save",12) == 0 ){
       if( param_save_1D != NULL ) ERROR_exit("Can't have multiple %s options!",argv[iarg]);
       if( ++iarg >= argc ) ERROR_exit("no argument after '%s'!",argv[iarg-1]) ;
       if( !THD_filename_ok(argv[iarg]) )
         ERROR_exit("badly formed filename: %s '%s'",argv[iarg-1],argv[iarg]) ;
       if( STRING_HAS_SUFFIX(argv[iarg],".1D") ){
         param_save_1D = argv[iarg] ;
       } else {
         param_save_1D = calloc(sizeof(char*),strlen(argv[iarg])+16) ;
         strcpy(param_save_1D,argv[iarg]) ; strcat(param_save_1D,".param.1D") ;
       }
       iarg++ ; continue ;
     }

     /*-----*/

     if( strncmp(argv[iarg],"-1Dmatrix_save",13) == 0 ){
       if( matrix_save_1D != NULL ) ERROR_exit("Can't have multiple %s options!",argv[iarg]);
       if( ++iarg >= argc ) ERROR_exit("no argument after '%s'!",argv[iarg-1]) ;
       if( !THD_filename_ok(argv[iarg]) )
         ERROR_exit("badly formed filename: %s '%s'",argv[iarg-1],argv[iarg]) ;
       if( STRING_HAS_SUFFIX(argv[iarg],".1D") ){
         matrix_save_1D = argv[iarg] ;
       } else {
         matrix_save_1D = calloc(sizeof(char*),strlen(argv[iarg])+16) ;
         strcpy(matrix_save_1D,argv[iarg]) ; strcat(matrix_save_1D,".aff12.1D") ;
       }
       iarg++ ; continue ;
     }

     /*-----*/

#undef  APL
#define APL(i,j) apply_far[(i)+(j)*apply_nx] /* i=param index, j=row index */

     if( strncmp(argv[iarg],"-1Dapply",5)        == 0 ||
         strncmp(argv[iarg],"-1Dparam_apply",13) == 0   ){

       if( apply_1D != NULL || apply_mode != 0 )
         ERROR_exit("Can't have multiple 'apply' options!") ;
       if( ++iarg >= argc ) ERROR_exit("no argument after '%s'!",argv[iarg-1]) ;
 /*      if( strncmp(argv[iarg],"1D:",3) != 0 && !THD_filename_ok(argv[iarg]) )
         ERROR_exit("badly formed filename: %s '%s'",argv[iarg-1],argv[iarg]) ;
*/
       apply_1D = argv[iarg] ; qim = mri_read_1D(apply_1D) ;
       if( qim == NULL ) ERROR_exit("Can't read %s '%s'",argv[iarg-1],apply_1D) ;
       apply_im  = mri_transpose(qim); mri_free(qim);
       apply_far = MRI_FLOAT_PTR(apply_im) ;
       apply_nx  = apply_im->nx ;  /* # of values per row */
       apply_ny  = apply_im->ny ;  /* number of rows */
       apply_mode = APPLY_PARAM ;
       iarg++ ; continue ;
     }

     /*-----*/

     if( strncmp(argv[iarg],"-1Dmatrix_apply",13) == 0 ){
       if( apply_1D != NULL || apply_mode != 0 )
         ERROR_exit("Can't have multiple 'apply' options!") ;
       if( ++iarg >= argc ) ERROR_exit("no argument after '%s'!",argv[iarg-1]) ;
       /*if( !THD_filename_ok(argv[iarg]) )
         ERROR_exit("badly formed filename: %s '%s'",argv[iarg-1],argv[iarg]) ;*/
       apply_1D = argv[iarg] ; qim = mri_read_1D(apply_1D) ;
       if( qim == NULL ) ERROR_exit("Can't read -1Dmatrix_apply '%s'",apply_1D) ;
       apply_im  = mri_transpose(qim); mri_free(qim);
       apply_far = MRI_FLOAT_PTR(apply_im) ;
       apply_nx  = apply_im->nx ;  /* # of values per row */
       apply_ny  = apply_im->ny ;  /* number of rows */
       apply_mode = APPLY_AFF12 ;
       if( apply_nx < 12 )
         ERROR_exit("Less than 12 numbers per row in -1Dmatrix_apply '%s'",apply_1D) ;
       else if( apply_nx > 12 )
         WARNING_message("More than 12 numbers per row in -1Dmatrix_apply '%s'",apply_1D) ;
       iarg++ ; continue ;
     }

     /*-----*/

     if( strcmp(argv[iarg],"-NN") == 0 || strncmp(argv[iarg],"-nearest",6) == 0 ){
       interp_code = MRI_NN ; iarg++ ; continue ;
     }
     if( strncmp(argv[iarg],"-linear",4)==0 || strncmp(argv[iarg],"-trilinear",6)==0 ){
       interp_code = MRI_LINEAR ; iarg++ ; continue ;
     }
     if( strncmp(argv[iarg],"-cubic",4)==0 || strncmp(argv[iarg],"-tricubic",6)==0 ){
       interp_code = MRI_CUBIC ; iarg++ ; continue ;
     }
     if( strncmp(argv[iarg],"-quintic",4)==0 || strncmp(argv[iarg],"-triquintic",6)==0 ){
       interp_code = MRI_QUINTIC ; iarg++ ; continue ;
     }
#if 0
     if( strcasecmp(argv[iarg],"-VARP1") == 0 ){
       interp_code = MRI_VARP1 ; iarg++ ; continue ;
     }
     if( strncasecmp(argv[iarg],"-WSINC") == 0 ){
       interp_code = MRI_WSINC5 ; iarg++ ; continue ;
     }
#endif
     if( strncmp(argv[iarg],"-interp",5)==0 ){
       if( ++iarg >= argc ) ERROR_exit("no argument after '%s'!",argv[iarg-1]) ;
       if( strcmp(argv[iarg],"NN")==0 || strncmp(argv[iarg],"nearest",5)==0 )
         interp_code = MRI_NN ;
       else
       if( strncmp(argv[iarg],"linear",3)==0 || strncmp(argv[iarg],"trilinear",5)==0 )
         interp_code = MRI_LINEAR ;
       else
       if( strncmp(argv[iarg],"cubic",3)==0 || strncmp(argv[iarg],"tricubic",5)==0 )
         interp_code = MRI_CUBIC ;
       else
       if( strncmp(argv[iarg],"quintic",3)==0 || strncmp(argv[iarg],"triquintic",5)==0 )
         interp_code = MRI_QUINTIC ;
#if 0
       else
       if( strcasecmp(argv[iarg],"VARP1")==0 )
         interp_code = MRI_VARP1 ;
       else
       if( strncasecmp(argv[iarg],"WSINC",5)==0 )
         interp_code = MRI_WSINC5 ;
#endif
       else
         ERROR_exit("Unknown code '%s' after '%s'!",argv[iarg],argv[iarg-1]) ;
       iarg++ ; continue ;
     }
     if( strncmp(argv[iarg],"-final",5) == 0 ){
       if( ++iarg >= argc ) ERROR_exit("no argument after '%s'!",argv[iarg-1]) ;
       if( strcmp(argv[iarg],"NN") == 0 || strncmp(argv[iarg],"nearest",5) == 0 )
         final_interp = MRI_NN ;
       else
       if( strncmp(argv[iarg],"linear",3) == 0 || strncmp(argv[iarg],"trilinear",5) == 0 )
         final_interp = MRI_LINEAR ;
       else
       if( strncmp(argv[iarg],"cubic",3) == 0 || strncmp(argv[iarg],"tricubic",5) == 0 )
         final_interp = MRI_CUBIC ;
       else
       if( strncmp(argv[iarg],"quintic",3)==0 || strncmp(argv[iarg],"triquintic",5)==0 )
         final_interp = MRI_QUINTIC ;
#if 0
       else
       if( strcasecmp(argv[iarg],"VARP1")==0 )
         final_interp = MRI_VARP1 ;
#endif
       else
       if( strncasecmp(argv[iarg],"WSINC",5)==0 )
         final_interp = MRI_WSINC5 ;
       else
         ERROR_exit("Unknown code '%s' after '%s'!",argv[iarg],argv[iarg-1]) ;
       iarg++ ; continue ;
     }

     /*-----*/

     if( strncmp(argv[iarg],"-converge",5) == 0 ){
       float vv ;
       if( ++iarg >= argc ) ERROR_exit("no argument after '%s'!",argv[iarg-1]) ;
       vv = (float)strtod(argv[iarg],NULL) ;
       if( vv <= 0.001f || vv > 6.66f ){
         vv = 0.05f ;
         WARNING_message("-conv '%s' is out of range",argv[iarg]) ;
       }
       conv_mm = vv ; iarg++ ; continue ;
     }

     /*-----*/

     if( strncmp(argv[iarg],"-nmatch",5) == 0 ){
       char *cpt ;
       if( ++iarg >= argc ) ERROR_exit("no argument after '%s'!",argv[iarg-1]) ;
       npt_match = (int)strtod(argv[iarg],&cpt) ;
       if( npt_match <= 0 )
         ERROR_exit("Illegal value '%s' after '%s'!",argv[iarg],argv[iarg-1]) ;
       if( *cpt == '%' || npt_match <= 100 )
         npt_match = -npt_match ;  /* signal for % */
       iarg++ ; continue ;
     }

     /*-----*/

     if( strcmp(argv[iarg],"-warp") == 0 ){
      if( ++iarg >= argc ) ERROR_exit("no argument after '%s'!",argv[iarg-1]) ;
      if( strcmp(argv[iarg],"sho")     ==0 || strcmp(argv[iarg],"shift_only")        ==0 )
        warp_code = WARP_SHIFT ;
      else if( strcmp(argv[iarg],"shr")==0 || strcmp(argv[iarg],"shift_rotate")      ==0 )
        warp_code = WARP_ROTATE ;
      else if( strcmp(argv[iarg],"srs")==0 || strcmp(argv[iarg],"shift_rotate_scale")==0 )
        warp_code = WARP_SCALE ;
      else if( strcmp(argv[iarg],"aff")==0 || strcmp(argv[iarg],"affine_general")    ==0 )
        warp_code = WARP_AFFINE ;
      else
        ERROR_exit("Unknown code '%s' after '%s'!",argv[iarg],argv[iarg-1]) ;
      iarg++ ; continue ;
     }
     if( strcmp(argv[iarg],"-dof") == 0 ){
       if( ++iarg >= argc ) ERROR_exit("no argument after '%s'!",argv[iarg-1]) ;
       ii = (int)strtod(argv[iarg],NULL) ;
       switch(ii){
         case  3: warp_code = WARP_SHIFT  ; break ;
         case  6: warp_code = WARP_ROTATE ; break ;
         case  9: warp_code = WARP_SCALE  ; break ;
         case 12: warp_code = WARP_AFFINE ; break ;
         default:
           ERROR_exit("Unknown value '%s' after '%s'!",argv[iarg],argv[iarg-1]) ;
       }
       iarg++ ; continue ;
     }

     /*-----*/

     if( strcmp(argv[iarg],"-parfix") == 0 ){
       if( ++iarg >= argc-1 ) ERROR_exit("need 2 arguments after '%s'!",argv[iarg-1]) ;
       if( nparopt >= MAXPAR ) ERROR_exit("too many -par... options!") ;
       ii = (int)strtod(argv[iarg],NULL) ;
       if( ii <= 0 ) ERROR_exit("-parfix '%s' is illegal!",argv[iarg]) ;
       v1 = (float)strtod(argv[++iarg],NULL) ;
       paropt[nparopt].np   = ii-1 ;
       paropt[nparopt].code = PARC_FIX ;
       paropt[nparopt].vb   = v1 ;
       nparopt++ ; iarg++ ; continue ;
     }

     /*-----*/

     if( strcmp(argv[iarg],"-parang") == 0 ){
       if( ++iarg >= argc-2 ) ERROR_exit("need 3 arguments after '%s'!",argv[iarg-1]) ;
       if( nparopt >= MAXPAR ) ERROR_exit("too many -par... options!") ;
       ii = (int)strtod(argv[iarg],NULL) ;
       if( ii <= 0 ) ERROR_exit("-parang '%s' is illegal!",argv[iarg]) ;
       v1 = (float)strtod(argv[++iarg],NULL) ;
       v2 = (float)strtod(argv[++iarg],NULL) ;
       if( v1 > v2 ) ERROR_exit("-parang %d '%s' '%s' is illegal!",
                     ii,argv[iarg-1],argv[iarg] ) ;
       paropt[nparopt].np   = ii-1 ;
       paropt[nparopt].code = PARC_RAN ;
       paropt[nparopt].vb   = v1 ;
       paropt[nparopt].vt   = v2 ;
       nparopt++ ; iarg++ ; continue ;
     }

     /*-----*/

     if( strcmp(argv[iarg],"-maxrot") == 0 ){
       float vv ;
       if( ++iarg >= argc ) ERROR_exit("no argument after '%s'!",argv[iarg-1]) ;
       if( nparopt+2 >= MAXPAR ) ERROR_exit("too many -par... options!") ;
       vv = (float)strtod(argv[iarg],NULL) ;
       if( vv <= 0.0f || vv > 90.0f ) ERROR_exit("-maxrot %f is illegal!",vv) ;
       paropt[nparopt].np   = 3 ;
       paropt[nparopt].code = PARC_RAN ;
       paropt[nparopt].vb   = -vv ;
       paropt[nparopt].vt   =  vv ; nparopt++ ;
       paropt[nparopt].np   = 4 ;
       paropt[nparopt].code = PARC_RAN ;
       paropt[nparopt].vb   = -vv ;
       paropt[nparopt].vt   =  vv ; nparopt++ ;
       paropt[nparopt].np   = 5 ;
       paropt[nparopt].code = PARC_RAN ;
       paropt[nparopt].vb   = -vv ;
       paropt[nparopt].vt   =  vv ; nparopt++ ; iarg++ ; continue ;
     }

     /*-----*/

     if( strcmp(argv[iarg],"-maxscl") == 0 ){
       float vv , vvi ; char *cpt ;
       if( ++iarg >= argc ) ERROR_exit("no argument after '%s'!",argv[iarg-1]) ;
       if( nparopt+2 >= MAXPAR ) ERROR_exit("too many -par... options!") ;
       vv = (float)strtod(argv[iarg],&cpt) ;
       if( *cpt == '%' ) vv = 1.0f + 0.01*vv ;
       if( vv == 1.0f || vv > 2.0f || vv < 0.5f )
         ERROR_exit("-maxscl %f is illegal!",vv) ;
       if( vv > 1.0f ){ vvi = 1.0f/vv; }
       else           { vvi = vv ; vv = 1.0f/vvi ; }
       paropt[nparopt].np   = 6 ;
       paropt[nparopt].code = PARC_RAN ;
       paropt[nparopt].vb   = vvi ;
       paropt[nparopt].vt   =  vv ; nparopt++ ;
       paropt[nparopt].np   = 7 ;
       paropt[nparopt].code = PARC_RAN ;
       paropt[nparopt].vb   = vvi ;
       paropt[nparopt].vt   =  vv ; nparopt++ ;
       paropt[nparopt].np   = 8 ;
       paropt[nparopt].code = PARC_RAN ;
       paropt[nparopt].vb   = vvi ;
       paropt[nparopt].vt   =  vv ; nparopt++ ; iarg++ ; continue ;
     }

     /*-----*/

     if( strcmp(argv[iarg],"-maxshf") == 0 ){
       float vv ;
       if( ++iarg >= argc ) ERROR_exit("no argument after '%s'!",argv[iarg-1]) ;
       if( nparopt+2 >= MAXPAR ) ERROR_exit("too many -par... options!") ;
       vv = (float)strtod(argv[iarg],NULL) ;
       if( vv <= 0.0f ) ERROR_exit("-maxshf %f is illegal!",vv) ;
       paropt[nparopt].np   = 0 ;
       paropt[nparopt].code = PARC_RAN ;
       paropt[nparopt].vb   = -vv ;
       paropt[nparopt].vt   =  vv ; nparopt++ ;
       paropt[nparopt].np   = 1 ;
       paropt[nparopt].code = PARC_RAN ;
       paropt[nparopt].vb   = -vv ;
       paropt[nparopt].vt   =  vv ; nparopt++ ;
       paropt[nparopt].np   = 2 ;
       paropt[nparopt].code = PARC_RAN ;
       paropt[nparopt].vb   = -vv ;
       paropt[nparopt].vt   =  vv ; nparopt++ ; iarg++ ; continue ;
     }

     /*-----*/

     if( strcmp(argv[iarg],"-parini") == 0 ){
       if( ++iarg >= argc-1 ) ERROR_exit("need 2 arguments after '%s'!",argv[iarg-1]) ;
       if( nparopt >= MAXPAR ) ERROR_exit("too many -par... options!") ;
       ii = (int)strtod(argv[iarg],NULL) ;
       if( ii <= 0 ) ERROR_exit("-parini '%s' is illegal!",argv[iarg]) ;
       v1 = (float)strtod(argv[++iarg],NULL) ;
       paropt[nparopt].np   = ii-1 ;
       paropt[nparopt].code = PARC_INI ;
       paropt[nparopt].vb   = v1 ;
       nparopt++ ; iarg++ ; continue ;
     }

     /*-----*/

     if( strncmp(argv[iarg],"-FPS",4)==0 || strncmp(argv[iarg],"-EPI",4)==0 ){
       int fe=-1 , pe=-1 , se=-1 ; char *fps , *aaa=argv[iarg] ;

       if( epi_targ >= 0 )
         ERROR_exit("Can't have multiple '%4.4s' options!",aaa) ;

       /* is the EPI dataset the target (default) or base? */

       epi_targ = (aaa[4] != '\0' && toupper(aaa[4]) == 'B') ? 0 : 1 ;

       if( aaa[1] == 'F' ){   /* -FPS code */
         if( ++iarg >= argc ) ERROR_exit("no argument after '%s'!",argv[iarg-1]);
         fps = argv[iarg] ;
         if( strlen(fps) < 3 ) ERROR_exit("Too short %4.4s codes '%s'",aaa,fps);
       } else {
         fps = "123" ;        /* -EPI */
       }

       /* decode the FPS directions, so that
            epi_fe = freq encode direction = 0 or 1 or 2
            epi_pe = phase encode direction
            epi_se = slice encode direction */

       switch( fps[0] ){
         default: ERROR_exit("Illegal %4.4s f code '%c'" , aaa,fps[0] );
         case 'i': case 'I': case 'x': case 'X': case '1':  fe = 1; break;
         case 'j': case 'J': case 'y': case 'Y': case '2':  fe = 2; break;
         case 'k': case 'K': case 'z': case 'Z': case '3':  fe = 3; break;
       }
       switch( fps[1] ){
         default: ERROR_exit("Illegal %4.4s p code '%c'" , aaa,fps[1] );
         case 'i': case 'I': case 'x': case 'X': case '1':  pe = 1; break;
         case 'j': case 'J': case 'y': case 'Y': case '2':  pe = 2; break;
         case 'k': case 'K': case 'z': case 'Z': case '3':  pe = 3; break;
       }
       switch( fps[2] ){
         default: ERROR_exit("Illegal %4.4s s code '%c'" , aaa,fps[2] );
         case 'i': case 'I': case 'x': case 'X': case '1':  se = 1; break;
         case 'j': case 'J': case 'y': case 'Y': case '2':  se = 2; break;
         case 'k': case 'K': case 'z': case 'Z': case '3':  se = 3; break;
       }
       if( fe+pe+se != 6 ) ERROR_exit("Illegal %4.4s combination '%s'",aaa,fps);

       epi_fe = fe-1 ; epi_pe = pe-1 ; epi_se = se-1 ;  /* process later */

       if( verb > 1 )
         INFO_message("EPI parameters: targ=%d  fe=%d pe=%d se=%d",
                      epi_targ,epi_fe,epi_pe,epi_se ) ;

       /* restrict some transformation parameters */

       smat = SMAT_YYY ;               /* shear only in y (PE) direction */
       warp_freeze = 1 ;               /* 10 Oct 2006 */

       /* matrix order depends on if we are restricting transformation
          parameters in the base image or in the target image coordinates */

       matorder = (epi_targ) ? MATORDER_SDU : MATORDER_USD ;

       paropt[nparopt].np   = 6 ;      /* fix x-scale to 1 */
       paropt[nparopt].code = PARC_FIX ;
       paropt[nparopt].vb   = 1.0 ; nparopt++ ;

       paropt[nparopt].np   = 8 ;      /* fix z-scale to 1 */
       paropt[nparopt].code = PARC_FIX ;
       paropt[nparopt].vb   = 1.0 ; nparopt++ ;

       paropt[nparopt].np   = 11 ;      /* fix last shear to 0 */
       paropt[nparopt].code = PARC_FIX ;
       paropt[nparopt].vb   = 0.0 ; nparopt++ ;

       twofirst = 1; replace_base = 1;
#if 0
       replace_meth = GA_MATCH_PEARSON_SCALAR;
#endif
       iarg++ ; continue ;
     }

     /*-----*/

     if( strcmp(argv[iarg],"-replacebase") == 0 ){  /* 18 Oct 2006 */
       twofirst = replace_base = 1 ; iarg++ ; continue ;
     }
     if( strcmp(argv[iarg],"-warpfreeze") == 0 ){  /* 18 Oct 2006 */
       warp_freeze = 1 ; iarg++ ; continue ;
     }

     if( strcmp(argv[iarg],"-nowarpfreeze") == 0 ){  /* 01 Feb 2007 */
       warp_freeze = 0 ; iarg++ ; continue ;
     }
     if( strcmp(argv[iarg],"-noreplacebase") == 0 ){  /* 01 Feb 2007 */
       replace_base = 0 ; iarg++ ; continue ;
     }

     /*-----*/

     if( strcmp(argv[iarg],"-replacemeth") == 0 ){  /* 18 Oct 2006 */
       if( ++iarg >= argc ) ERROR_exit("no argument after '-replacemeth'!") ;

       if( strcmp(argv[iarg],"0") == 0 ){
         replace_meth = 0 ; iarg++ ; continue ;  /* special case */
       }

       for( jj=ii=0 ; ii < NMETH ; ii++ ){
         if( strcasecmp(argv[iarg],meth_shortname[ii]) == 0 ){
           replace_meth = jj = ii+1 ; break ;
         }
       }
       if( jj > 0 ){ iarg++ ; continue ; }

       for( jj=ii=0 ; ii < NMETH ; ii++ ){
         if( strncasecmp(argv[iarg],meth_longname[ii],7) == 0 ){
           replace_meth = jj = ii+1 ; break ;
         }
       }
       if( jj >=0 ){ iarg++ ; continue ; }

       ERROR_exit("Unknown code '%s' after -replacemeth!",argv[iarg]) ;
       iarg++ ; continue ;
     }

     /*-----*/

     if( strcmp(argv[iarg],"-Xwarp") == 0 ){  /* 02 Oct 2006 */
       if( XYZ_warp > 0 ) ERROR_exit("only one use of -[XYZ]warp is allowed");
       matorder = MATORDER_USD ;       /* rotation after shear and scale */

       paropt[nparopt].np   = 7 ;      /* fix y-scale to 1 */
       paropt[nparopt].code = PARC_FIX ;
       paropt[nparopt].vb   = 1.0 ; nparopt++ ;

       paropt[nparopt].np   = 8 ;      /* fix z-scale to 1 */
       paropt[nparopt].code = PARC_FIX ;
       paropt[nparopt].vb   = 1.0 ; nparopt++ ;

       paropt[nparopt].np   = 11 ;      /* fix last shear to 0 */
       paropt[nparopt].code = PARC_FIX ;
       paropt[nparopt].vb   = 0.0 ; nparopt++ ;

       smat = SMAT_XXX ;                /* fix shear matrix to x-only */
       XYZ_warp = 1 ;iarg++ ; continue ;
     }

     if( strcmp(argv[iarg],"-Ywarp") == 0 ){  /* 02 Oct 2006 */
       if( XYZ_warp > 0 ) ERROR_exit("only one use of -[XYZ]warp is allowed");
       matorder = MATORDER_USD ;       /* rotation after shear and scale */

       paropt[nparopt].np   = 6 ;      /* fix x-scale to 1 */
       paropt[nparopt].code = PARC_FIX ;
       paropt[nparopt].vb   = 1.0 ; nparopt++ ;

       paropt[nparopt].np   = 8 ;      /* fix z-scale to 1 */
       paropt[nparopt].code = PARC_FIX ;
       paropt[nparopt].vb   = 1.0 ; nparopt++ ;

       paropt[nparopt].np   = 11 ;      /* fix last shear to 0 */
       paropt[nparopt].code = PARC_FIX ;
       paropt[nparopt].vb   = 0.0 ; nparopt++ ;

       smat = SMAT_YYY ;                /* fix shear matrix to y-only */
       XYZ_warp = 2 ; iarg++ ; continue ;
     }

     if( strcmp(argv[iarg],"-Zwarp") == 0 ){  /* 02 Oct 2006 */
       if( XYZ_warp > 0 ) ERROR_exit("only one use of -[XYZ]warp is allowed");
       matorder = MATORDER_USD ;       /* rotation after shear and scale */

       paropt[nparopt].np   = 6 ;      /* fix x-scale to 1 */
       paropt[nparopt].code = PARC_FIX ;
       paropt[nparopt].vb   = 1.0 ; nparopt++ ;

       paropt[nparopt].np   = 7 ;      /* fix y-scale to 1 */
       paropt[nparopt].code = PARC_FIX ;
       paropt[nparopt].vb   = 1.0 ; nparopt++ ;

       paropt[nparopt].np   = 11 ;      /* fix last shear to 0 */
       paropt[nparopt].code = PARC_FIX ;
       paropt[nparopt].vb   = 0.0 ; nparopt++ ;

       smat = SMAT_ZZZ ;                /* fix shear matrix to x-only */
       XYZ_warp = 3 ; iarg++ ; continue ;
     }

     /*-----*/

     if( strcmp(argv[iarg],"-SDU") == 0 ){
       matorder = MATORDER_SDU ; iarg++ ; continue ;
     }
     if( strcmp(argv[iarg],"-SUD") == 0 ){
       matorder = MATORDER_SUD ; iarg++ ; continue ;
     }
     if( strcmp(argv[iarg],"-DSU") == 0 ){
       matorder = MATORDER_DSU ; iarg++ ; continue ;
     }
     if( strcmp(argv[iarg],"-DUS") == 0 ){
       matorder = MATORDER_DUS ; iarg++ ; continue ;
     }
     if( strcmp(argv[iarg],"-USD") == 0 ){
       matorder = MATORDER_USD ; iarg++ ; continue ;
     }
     if( strcmp(argv[iarg],"-UDS") == 0 ){
       matorder = MATORDER_UDS ; iarg++ ; continue ;
     }
     if( strcmp(argv[iarg],"-ashift") == 0 ){
       dcode = DELTA_AFTER     ; iarg++ ; continue ;
     }
     if( strcmp(argv[iarg],"-bshift") == 0 ){
       dcode = DELTA_BEFORE    ; iarg++ ; continue ;
     }
     if( strcmp(argv[iarg],"-Slower") == 0 ){
       smat  = SMAT_LOWER      ; iarg++ ; continue ;
     }
     if( strcmp(argv[iarg],"-Supper") == 0 ){
       smat  = SMAT_UPPER      ; iarg++ ; continue ;
     }

     /*-----*/

     if( strcmp(argv[iarg],"-blok") == 0 ){   /* hidden */
       int ia=0 ;
       if( ++iarg >= argc ) ERROR_exit("Need argument after -blok") ;
       if( strncmp(argv[iarg],"SPHERE(",7) == 0 ){
         ia = 7 ; bloktype = GA_BLOK_BALL ;
       } else if( strncmp(argv[iarg],"BALL(",5) == 0 ){
         ia = 5 ; bloktype = GA_BLOK_BALL ;
       } else if( strncmp(argv[iarg],"RECT(",5) == 0 ){
         ia = 5 ; bloktype = GA_BLOK_CUBE ;
       } else if( strncmp(argv[iarg],"CUBE(",5) == 0 ){
         ia = 5 ; bloktype = GA_BLOK_CUBE ;
       } else if( strncmp(argv[iarg],"RHDD(",5) == 0 ){
         ia = 5 ; bloktype = GA_BLOK_RHDD ;
       } else if( strncmp(argv[iarg],"TOHD(",5) == 0 ){
         ia = 5 ; bloktype = GA_BLOK_TOHD ;
       } else {
         ERROR_exit("Illegal argument after -blok") ;
       }
       blokrad = (float)strtod(argv[iarg]+ia,NULL) ;
       iarg++ ; continue ;
     }

     /*-----*/

     ERROR_exit("Unknown and Illegal option '%s'",argv[iarg]) ;
   }

   if( iarg < argc )
     WARNING_message("Processing command line options stopped at '%s'",argv[iarg]);

   /*---------------------------------------------------------------*/
   /*--- check inputs for validity, consistency, and moral fibre ---*/

   if( seed == 0 ) seed = (long)time(NULL)+(long)getpid() ;
   srand48(seed) ;

   if( meth_code == GA_MATCH_PEARSON_SCALAR && !wtspecified ){ /* 10 Sep 2007 */
     auto_weight = 1 ;  /* for '-ls', use '-autoweight' */
     if( verb ) INFO_message("Cost 'ls' ==> using '-autoweight' default") ;
   }

   if( !hist_setbyuser ){   /* 25 Jul 2007 */
     switch( meth_code ){
       case GA_MATCH_PEARSON_LOCALS:
       case GA_MATCH_PEARSON_LOCALA:
       case GA_MATCH_SPEARMAN_SCALAR:
       case GA_MATCH_PEARSON_SCALAR:  hist_mode = 0 ; break ;

       default: hist_mode  = GA_HIST_CLEQWD ; break ;
     }
   }

   if( do_allcost < 0 && prefix != NULL ){  /* 19 Sep 2007 */
     prefix = NULL ;
     WARNING_message("-allcostX means -prefix is ignored!") ;
   }
   if( do_allcost < 0 && param_save_1D != NULL ){
     param_save_1D = NULL ;
     WARNING_message("-allcostX means -1Dparam_save is ignored!") ;
   }
   if( do_allcost < 0 && matrix_save_1D != NULL ){
     matrix_save_1D = NULL ;
     WARNING_message("-allcostX means -1Dmatrix_save is ignored!") ;
   }

   if( warp_freeze ) twofirst = 1 ;  /* 10 Oct 2006 */

   if( apply_mode > 0 ){
     if( nwarp_pass && nwarp_type == WARP_BILINEAR ){
       if( apply_nx >= NPBIL+4 ){
         apply_mode = APPLY_BILIN ;
         INFO_message(
          "found %d param/row in param file '%s'; applying bilinear warp",
          apply_nx , apply_1D) ;
       } else {
         INFO_message(
          "found %d param/row in param file '%s'; not enough for bilinear warp",
          apply_nx , apply_1D) ;
       }
     } else if( nwarp_pass ){
       ERROR_exit("Can't apply -nwarp except bilinear, at this time.") ;
     }
   }


   /* open target from last argument, if not already open */

   if( dset_targ == NULL ){
     if( iarg >= argc )
       ERROR_exit("no source datset on command line!?") ;
     dset_targ = THD_open_dataset( argv[iarg] ) ;
     if( dset_targ == NULL )
       ERROR_exit("Can't open source dataset '%s'",argv[iarg]) ;
   }

   if( nwarp_pass && DSET_NVALS(dset_targ) > 1 )
     ERROR_exit("Can't use -nwarp on more than 1 sub-brick!") ;

   switch( tb_mast ){                        /* 19 Jul 2007 */
     case 1: dset_mast = dset_targ ; break ;
     case 2: dset_mast = dset_base ; break ;
   }

   if( replace_base && DSET_NVALS(dset_targ) == 1 ) replace_base = 0 ;

   /* check target data type */

   targ_kind = (int)DSET_BRICK_TYPE(dset_targ,0) ;
   if( targ_kind != MRI_float && targ_kind != MRI_short && targ_kind != MRI_byte )
     ERROR_exit("source dataset %s has non-scalar data type '%s'",
                DSET_BRIKNAME(dset_targ) , MRI_TYPE_name[targ_kind] ) ;
   if( !DSET_datum_constant(dset_targ) )
     WARNING_message("source dataset %s does not have constant data type!",
                     DSET_BRIKNAME(dset_targ)) ;

   /*-- if applying a set of parameters, some options are turned off --*/

   if( apply_1D != NULL ){
     if( prefix == NULL ) ERROR_exit("-1D*_apply also needs -prefix!") ;
     if( param_save_1D  != NULL ) WARNING_message("-1D*_apply: Can't do -1Dparam_save") ;
     if( matrix_save_1D != NULL ) WARNING_message("-1D*_apply: Can't do -1Dmatrix_save") ;
     wtprefix = param_save_1D = matrix_save_1D = NULL ;
     zeropad = 0 ; auto_weight = auto_tmask = 0 ;
     if( dset_weig != NULL ){
       WARNING_message("-1D*_apply: Ignoring weight dataset") ;
       DSET_delete(dset_weig) ; dset_weig=NULL ;
     }
     if( im_tmask != NULL ){
       WARNING_message("-1D*_apply: Ignoring -source_mask") ;
       mri_free(im_tmask) ; im_tmask = NULL ;
     }
     if( dset_mast == NULL && dxyz_mast == 0.0 )
       INFO_message("You might want to use '-master' when using '-1D*_apply'") ;
     if( do_allcost ){  /* 19 Sep 2007 */
       do_allcost = 0 ;
       WARNING_message("-allcost option illegal with -1D*_apply") ;
     }
   }

   /* if no base input, target should have more than 1 sub-brick */

   if( dset_base == NULL && apply_1D == NULL ){
     if( DSET_NVALS(dset_targ) == 1 )
       ERROR_exit("No base dataset AND source dataset has only 1 sub-brick") ;

     WARNING_message("No -base dataset: using sub-brick #0 of source") ;
     skip_first = 1 ;  /* don't register sub-brick #0 of targ to itself! */
   }

   if( final_interp < 0 ) final_interp = interp_code ;  /* default */

   /*--- load input datasets ---*/

   if( verb ) INFO_message("Loading datasets") ;

   /* target MUST be present */

   DSET_load(dset_targ) ; CHECK_LOAD_ERROR(dset_targ) ;
   nx_targ = DSET_NX(dset_targ) ; dx_targ = fabsf(DSET_DX(dset_targ)) ;
   ny_targ = DSET_NY(dset_targ) ; dy_targ = fabsf(DSET_DY(dset_targ)) ;
   nz_targ = DSET_NZ(dset_targ) ; dz_targ = fabsf(DSET_DZ(dset_targ)) ;

   nxyz_targ[0] = nx_targ; nxyz_targ[1] = ny_targ; nxyz_targ[2] = nz_targ;
   dxyz_targ[0] = dx_targ; dxyz_targ[1] = dy_targ; dxyz_targ[2] = dz_targ;

   if( nx_targ < 2 || ny_targ < 2 )
     ERROR_exit("Source dataset has nx=%d ny=%d ???",nx_targ,ny_targ) ;

   /*-- 07 Aug 2007: make target automask? --*/

   if( auto_tmask ){

     byte *mmm ; int ndil=auto_tdilation ;
     mmm = THD_automask( dset_targ ) ;
     if( mmm == NULL )
       ERROR_exit("Can't make -source_automask for some reason!") ;
     im_tmask = mri_new_vol_empty( nx_targ,ny_targ,nz_targ , MRI_byte ) ;
     mri_fix_data_pointer( mmm , im_tmask ) ;
     if( ndil > 0 ){
       for( ii=0 ; ii < ndil ; ii++ ){
         THD_mask_dilate     ( nx_targ,ny_targ,nz_targ , mmm , 3 ) ;
         THD_mask_fillin_once( nx_targ,ny_targ,nz_targ , mmm , 2 ) ;
       }
     }
     ntmask = THD_countmask( im_tmask->nvox , mmm ) ;
     if( ntmask < 666 )
       ERROR_exit("Too few (%d) voxels in %s!",ntmask,auto_tstring) ;
     if( verb )
       INFO_message("%d voxels in %s",ntmask,auto_tstring) ;

   } else if( im_tmask != NULL ){  /*-- check -source_mask vs. target --*/

     if( im_tmask->nx != nx_targ ||
         im_tmask->ny != ny_targ || im_tmask->nz != nz_targ )
       ERROR_exit("-source_mask and -source datasets "
                  "have different dimensions!\n"
                  "Have: %d %d %d versus %d %d %d\n",
                  im_tmask->nx, im_tmask->ny , im_tmask->nz,
                  nx_targ, ny_targ, nz_targ) ;
   }

   /*-- load base dataset if defined --*/

   if( dset_base != NULL ){
     DSET_load(dset_base) ; CHECK_LOAD_ERROR(dset_base) ;
     im_base = mri_scale_to_float( DSET_BRICK_FACTOR(dset_base,0) ,
                                   DSET_BRICK(dset_base,0)         ) ;
     DSET_unload(dset_base) ;
     dx_base = fabsf(DSET_DX(dset_base)) ;
     dy_base = fabsf(DSET_DY(dset_base)) ;
     dz_base = fabsf(DSET_DZ(dset_base)) ;
     if( im_base->nx < 2 || im_base->ny < 2 )
       ERROR_exit("Base dataset has nx=%d ny=%d ???",im_base->nx,im_base->ny) ;
   } else {
     if( apply_mode == 0 )
       INFO_message("no -base option ==> base is #0 sub-brick of source") ;
     im_base = mri_scale_to_float( DSET_BRICK_FACTOR(dset_targ,0) ,
                                   DSET_BRICK(dset_targ,0)         ) ;
     dx_base = dx_targ; dy_base = dy_targ; dz_base = dz_targ;
     if( do_cmass && apply_mode == 0 ){   /* 30 Jul 2007 */
       WARNING_message("no base dataset ==> -cmass is disabled"); do_cmass = 0;
     }
   }
   nx_base = im_base->nx ;
   ny_base = im_base->ny ; nxy_base  = nx_base *ny_base ;
   nz_base = im_base->nz ; nvox_base = nxy_base*nz_base ;

   /* find the autobbox, and setup zero-padding */

#undef  MPAD
#define MPAD 4     /* max #slices to zeropad */
   if( zeropad ){
     float cv , *qar  ;
     cv = 0.33f * THD_cliplevel(im_base,0.33f) ;       /* set threshold */
     qim = mri_copy(im_base); qar = MRI_FLOAT_PTR(qim);
     for( ii=0 ; ii < qim->nvox ; ii++ ) if( qar[ii] < cv ) qar[ii] = 0.0f ;

     /* find edges of box that contain supra-threshold contents */

     MRI_autobbox( qim, &pad_xm,&pad_xp, &pad_ym,&pad_yp, &pad_zm,&pad_zp ) ;
     mri_free(qim) ;
#if 0
     if( verb ){
       INFO_message("bbox: xbot=%3d xtop=%3d nx=%3d",pad_xm,pad_xp,nx_base);
       INFO_message("    : ybot=%3d ytop=%3d ny=%3d",pad_ym,pad_yp,ny_base);
      if( nz_base > 1 )
       INFO_message("    : zbot=%3d ztop=%3d nz=%3d",pad_zm,pad_zp,nz_base);
     }
#endif

     /* compute padding so that at least MPAD all-zero slices on each face */

     pad_xm = MPAD - pad_xm               ; if( pad_xm < 0 ) pad_xm = 0 ;
     pad_ym = MPAD - pad_ym               ; if( pad_ym < 0 ) pad_ym = 0 ;
     pad_zm = MPAD - pad_zm               ; if( pad_zm < 0 ) pad_zm = 0 ;
     pad_xp = MPAD - (nx_base-1 - pad_xp) ; if( pad_xp < 0 ) pad_xp = 0 ;
     pad_yp = MPAD - (ny_base-1 - pad_yp) ; if( pad_yp < 0 ) pad_yp = 0 ;
     pad_zp = MPAD - (nz_base-1 - pad_zp) ; if( pad_zp < 0 ) pad_zp = 0 ;
     if( nz_base == 1 ){ pad_zm = pad_zp = 0 ; }  /* don't z-pad 2D image! */

     zeropad = (pad_xm > 0 || pad_xp > 0 ||
                pad_ym > 0 || pad_yp > 0 || pad_zm > 0 || pad_zp > 0) ;

     if( verb && apply_mode == 0 ){
       if( zeropad ){
         if( pad_xm > 0 || pad_xp > 0 )
           INFO_message("Zero-pad: xbot=%d xtop=%d",pad_xm,pad_xp) ;
         if( pad_ym > 0 || pad_yp > 0 )
           INFO_message("zero-pad: ybot=%d ytop=%d",pad_ym,pad_yp) ;
         if( pad_zm > 0 || pad_zp > 0 )
           INFO_message("zero-pad: zbot=%d ztop=%d",pad_zm,pad_zp) ;
       } else {
         INFO_message("Zero-pad: not needed") ;
       }
     }

     /* zeropad the base image at this point in spacetime? */

     if( zeropad ){
       qim = mri_zeropad_3D( pad_xm,pad_xp , pad_ym,pad_yp ,
                                             pad_zm,pad_zp , im_base ) ;
       mri_free(im_base) ; im_base = qim ;
       nx_base = im_base->nx ;
       ny_base = im_base->ny ; nxy_base  = nx_base *ny_base ;
       nz_base = im_base->nz ; nvox_base = nxy_base*nz_base ;
     }
   }

   nxyz_base[0] = nx_base; nxyz_base[1] = ny_base; nxyz_base[2] = nz_base;
   dxyz_base[0] = dx_base; dxyz_base[1] = dy_base; dxyz_base[2] = dz_base;

   /* check for base:target dimensionality mismatch */

   if( nz_base >  1 && nz_targ == 1 )
     ERROR_exit("Can't register 2D source into 3D base!") ;
   if( nz_base == 1 && nz_targ >  1 )
     ERROR_exit("Can't register 3D source onto 2D base!") ;
   if( nz_base == 1 && nwarp_pass )
     ERROR_exit("Can't use -nwarp on 2D images!") ;  /* 03 Apr 2008 */

   /* load weight dataset if defined */

   if( dset_weig != NULL ){
     DSET_load(dset_weig) ; CHECK_LOAD_ERROR(dset_weig) ;
     im_weig = mri_scale_to_float( DSET_BRICK_FACTOR(dset_weig,0) ,
                                   DSET_BRICK(dset_weig,0)         ) ;
     DSET_unload(dset_weig) ;

     /* zeropad weight to match base? */

     if( zeropad ){
       qim = mri_zeropad_3D( pad_xm,pad_xp , pad_ym,pad_yp ,
                                             pad_zm,pad_zp , im_weig ) ;
       mri_free(im_weig) ; im_weig = qim ;
     }
     if( im_weig->nx != nx_base ||
         im_weig->ny != ny_base || im_weig->nz != nz_base )
       ERROR_exit("-weight and base volumes don't match grid dimensions!") ;

   } else if( auto_weight ){  /* manufacture weight from the base */
     if( meth_noweight[meth_code-1] && auto_weight == 1 && auto_wclip == 0.0f ){
       WARNING_message("Cost function '%s' ('%s') uses -automask NOT -autoweight",
                       meth_longname[meth_code-1] , meth_shortname[meth_code-1] ) ;
       auto_weight = 2 ;
     } else if( verb >= 1 ){
       INFO_message("Computing %s",auto_string) ;
     }
     if( verb > 1 ) ctim = COX_cpu_time() ;
     im_weig = mri_weightize(im_base,auto_weight,auto_dilation,auto_wclip,auto_wpow) ;
     if( verb > 1 ) INFO_message("%s CPU time = %.1f s" ,
                                 auto_string , COX_cpu_time()-ctim ) ;
   }

   /* also, make a mask from the weight (not used much, yet) */

   if( im_weig != NULL ){
     float *wf = MRI_FLOAT_PTR(im_weig) ;
     byte  *mf ;
     im_mask = mri_new_conforming(im_weig,MRI_byte) ;
     mf = MRI_BYTE_PTR(im_mask) ;
     for( ii=0 ; ii < im_mask->nvox ; ii++ ) mf[ii] = (wf[ii] > 0.0f) ;
     nmask = THD_countmask(im_mask->nvox,mf) ;
     if( verb ) INFO_message("%d voxels [%.1f%%] in weight mask",
                             nmask, 100.0*nmask/(float)im_mask->nvox ) ;
   } else {
     nmask = nvox_base ;  /* the universal 'mask' */
   }
   if( usetemp ) mri_purge(im_mask) ;

   /* save weight? */

   if( wtprefix != NULL && im_weig != NULL ){
     THD_3dim_dataset *wset ;
     wset = EDIT_empty_copy( (dset_base!=NULL) ? dset_base : dset_targ ) ;
     EDIT_dset_items( wset ,
                        ADN_prefix    , wtprefix ,
                        ADN_nvals     , 1 ,
                        ADN_ntt       , 0 ,
                        ADN_datum_all , MRI_float ,
                      ADN_none ) ;
     EDIT_BRICK_FACTOR(wset,0,0.0);
     if( zeropad ) qim = mri_zeropad_3D( -pad_xm,-pad_xp , -pad_ym,-pad_yp ,
                                         -pad_zm,-pad_zp , im_weig ) ;
     else          qim = mri_copy(im_weig) ;
     EDIT_substitute_brick( wset, 00, MRI_float, MRI_FLOAT_PTR(qim) );
     mri_clear_data_pointer(qim) ; mri_free(qim) ;
     DSET_write(wset); if( verb ) WROTE_DSET(wset);
     DSET_delete(wset) ;
   }

   /* initialize ntask, regardless     26 Aug 2008 [rickr] */
   ntask = DSET_NVOX(dset_targ) ;
   ntask = (ntask < nmask) ? (int)sqrt(ntask*(double)nmask) : nmask ;
   /* number of points to use for matching */
   if( nmask_frac < 0 ){
      if( npt_match < 0     ) npt_match = (int)(-0.01f*npt_match*ntask) ;
      if( npt_match < 9999  ) npt_match = 9999 ;
      if( npt_match > ntask ) npt_match = ntask ;
   } else {
      npt_match = (int)(nmask_frac*(double)nmask);
   }
   if( verb && apply_mode == 0 )
     INFO_message("Number of points for matching = %d",npt_match) ;

   /*------ setup alignment structure parameters ------*/

   memset(&stup,0,sizeof(GA_setup)) ;  /* NULL out */

   stup.match_code = meth_code ;
   stup.usetemp    = usetemp ;     /* 20 Dec 2006 */

   stup.hist_mode  = hist_mode ;   /* 08 May 2007 */
   stup.hist_param = hist_param ;

   /* spatial coordinates: 'cmat' transforms from ijk to xyz */

   if( !ISVALID_MAT44(dset_targ->daxes->ijk_to_dicom) )
     THD_daxes_to_mat44(dset_targ->daxes) ;
   stup.targ_cmat = dset_targ->daxes->ijk_to_dicom ;

   /* base coordinates are drawn from it's header, or are same as target */

   if( dset_base != NULL ){
     if( !ISVALID_MAT44(dset_base->daxes->ijk_to_dicom) )
       THD_daxes_to_mat44(dset_base->daxes) ;
     stup.base_cmat = dset_base->daxes->ijk_to_dicom ;

     if( MAT44_DET(stup.base_cmat) * MAT44_DET(stup.targ_cmat) < 0.0f ){
       WARNING_message("base and source datasets have different handedness!") ;
       WARNING_message("Alignment will proceed, but examine results carefully!");
     }
   } else {
     stup.base_cmat = stup.targ_cmat ;
   }

   stup.blokset = NULL ;
   if( meth_code == GA_MATCH_PEARSON_LOCALS ||
       meth_code == GA_MATCH_PEARSON_LOCALA || do_allcost  ){
     float mr = 1.23f * ( MAT44_COLNORM(stup.base_cmat,0)
                         +MAT44_COLNORM(stup.base_cmat,1)
                         +MAT44_COLNORM(stup.base_cmat,2) ) ;
     if( blokrad < mr ) blokrad = mr ;
     stup.bloktype = bloktype ; stup.blokrad = blokrad ; stup.blokmin = 0 ;
     if( verb ) INFO_message("Local correlation: blok type = '%s(%g)'",
                             GA_BLOK_STRING(bloktype) , blokrad        ) ;
   }

   /* modify base_cmat to allow for zeropad? */

   if( pad_xm > 0 || pad_ym > 0 || pad_zm > 0 )
     MAT44_EXTEND_IJK( stup.base_cmat , pad_xm,pad_ym,pad_zm ) ;

   targ_cmat = stup.targ_cmat; targ_cmat_inv = MAT44_INV(targ_cmat); /* 23 Jul 2007 */
   base_cmat = stup.base_cmat; base_cmat_inv = MAT44_INV(base_cmat);

   /*---------- define warp 'before' and 'after' matrices ----------*/

   AL_setup_warp_coords( epi_targ,epi_fe,epi_pe,epi_se,
                         nxyz_base, dxyz_base, stup.base_cmat,
                         nxyz_targ, dxyz_targ, stup.targ_cmat ) ;

   /*---------- define warp parameters and function ----------*/

   mri_genalign_affine_setup( matorder , dcode , smat ) ;

   stup.wfunc       = mri_genalign_affine ;  /* warping function */
   stup.wfunc_param = (GA_param *)calloc(12,sizeof(GA_param)) ;

   if( nwarp_pass && warp_code != WARP_AFFINE ){
     WARNING_message("Use of -nwarp ==> must use all 12 affine parameters") ;
     warp_code = WARP_AFFINE ;
   }

   switch( warp_code ){
     case WARP_SHIFT:   stup.wfunc_numpar =  3 ; break ;
     case WARP_ROTATE:  stup.wfunc_numpar =  6 ; break ;
     case WARP_SCALE:   stup.wfunc_numpar =  9 ; break ;
     case WARP_AFFINE:  stup.wfunc_numpar = 12 ; break ;
   }

   /*-- check if -1Dapply_param is giving us enough parameters for this warp --*/

   if( apply_1D != NULL ){
     if( apply_mode == APPLY_PARAM && apply_nx < stup.wfunc_numpar )
       ERROR_exit(
         "-1Dparam_apply '%s': %d isn't enough parameters per row for desired warp",
         apply_1D,apply_nx);

     if( apply_ny < DSET_NVALS(dset_targ) )
       WARNING_message(
        "-1D*_apply '%s': %d isn't enough rows for source dataset -- last row will repeat",
        apply_1D,apply_ny);
   }

   /*-- macro to set up control values for a given parameter --*/

#define DEFPAR(p,nm,bb,tt,id,dd,ll)               \
 do{ stup.wfunc_param[p].min      = (bb) ;        \
     stup.wfunc_param[p].max      = (tt) ;        \
     stup.wfunc_param[p].delta    = (dd) ;        \
     stup.wfunc_param[p].toler    = (ll) ;        \
     stup.wfunc_param[p].ident    = (id) ;        \
     stup.wfunc_param[p].val_init = (id) ;        \
     stup.wfunc_param[p].val_pinit= (id) ;        \
     stup.wfunc_param[p].val_fixed= (id) ;        \
     stup.wfunc_param[p].val_out  = (id) ;        \
     strcpy( stup.wfunc_param[p].name , (nm) ) ;  \
     stup.wfunc_param[p].fixed  = 0 ;             \
 } while(0)

   /*-- compute range of shifts allowed --*/

   xxx = 0.321 * (nx_base-1) ;
   yyy = 0.321 * (ny_base-1) ;
   zzz = 0.321 * (nz_base-1) ; xxx_m = yyy_m = zzz_m = 0.0f ;
   for( ii=-1 ; ii <= 1 ; ii+=2 ){
    for( jj=-1 ; jj <= 1 ; jj+=2 ){
      for( kk=-1 ; kk <= 1 ; kk+=2 ){
        MAT33_VEC( base_cmat , (ii*xxx),(jj*yyy),(kk*zzz) ,
                   xxx_p,yyy_p,zzz_p ) ;
        xxx_p = fabsf(xxx_p); yyy_p = fabsf(yyy_p); zzz_p = fabsf(zzz_p);
        xxx_m = MAX(xxx_m,xxx_p);
        yyy_m = MAX(yyy_m,yyy_p); zzz_m = MAX(zzz_m,zzz_p);
   }}}
   xxx = xxx_m ; yyy = yyy_m ; zzz = zzz_m ;

   /*-- 30 Jul 2007: center-of-mass sets range of shifts --*/

   if( do_cmass ){
     float xtarg,ytarg,ztarg , xbase,ybase,zbase ;

     mri_get_cmass_3D( im_base , &xc,&yc,&zc ) ;
     MAT44_VEC( base_cmat , xc,yc,zc , xbase,ybase,zbase ) ;
     if( verb > 2 )
       INFO_message("base center of mass = %.3f %.3f %.3f (index)",xc,yc,zc) ;
     im_targ = THD_median_brick( dset_targ ) ;
     mri_get_cmass_3D( im_targ , &xc,&yc,&zc ) ; mri_free(im_targ) ;
     if( verb > 2 )
       INFO_message("source center of mass = %.3f %.3f %.3f (index)",xc,yc,zc) ;
     MAT44_VEC( targ_cmat , xc,yc,zc , xtarg,ytarg,ztarg ) ;
     xc = xtarg-xbase ; yc = ytarg-ybase ; zc = ztarg-zbase ;
     if( verb > 2 )
       INFO_message("source-target CM = %.3f %.3f %.3f (xyz)",xc,yc,zc) ;
     if (do_cmass < 0) {
         /* try to figure what is OK, for partial coverage */
         if (fabs(xc) >= fabs(yc) && fabs(xc) >= fabs(zc)) {
            if (     fabs(xc) > 4.0          /* more than 4 voxels */
                  && fabs(xc) > 2.0*fabs(yc) /* more than twice the 2nd */
                  && fabs(xc) > 2.0*fabs(zc) /* more than twice the 3rd */) {
               xc = 0.0f;
            }
         } else if (fabs(yc) >= fabs(xc) && fabs(yc) >= fabs(zc)) {
            if (     fabs(yc) > 4.0          /* more than 4 voxels */
                  && fabs(yc) > 2.0*fabs(xc) /* more than twice the 2nd */
                  && fabs(yc) > 2.0*fabs(zc) /* more than twice the 3rd */) {
               yc = 0.0f;
            }
         } else if (fabs(zc) >= fabs(xc) && fabs(zc) >= fabs(yc)) {
            if (     fabs(zc) > 4.0          /* more than 4 voxels */
                  && fabs(zc) > 2.0*fabs(xc) /* more than twice the 2nd */
                  && fabs(zc) > 2.0*fabs(yc) /* more than twice the 3rd */) {
               zc = 0.0f;
            }
         }
     } else {
        if( (do_cmass & 1) == 0 ) xc = 0.0f ;
        if( (do_cmass & 2) == 0 ) yc = 0.0f ;
        if( (do_cmass & 4) == 0 ) zc = 0.0f ;
     }
     if( verb > 2 && apply_mode == 0 ){
       INFO_message("center of mass shifts = %.3f %.3f %.3f",xc,yc,zc) ;
     }
   } else {
     xc = yc = zc = 0.0f ;
   }
   xxx_p = xc + xxx ; xxx_m = xc - xxx ;
   yyy_p = yc + yyy ; yyy_m = yc - yyy ;
   zzz_p = zc + zzz ; zzz_m = zc - zzz ;

   if( verb > 2 && apply_mode == 0 )
     INFO_message("shift param auto-range: %.1f..%.1f %.1f..%.1f %.1f..%.1f",
                  xxx_m,xxx_p , yyy_m,yyy_p , zzz_m,zzz_p ) ;

   /*-- we now define all 12 affine parameters, though not all may be used --*/

   DEFPAR( 0, "x-shift" , xxx_m , xxx_p , 0.0 , 0.0 , 0.0 ) ;    /* mm */
   DEFPAR( 1, "y-shift" , yyy_m , yyy_p , 0.0 , 0.0 , 0.0 ) ;
   DEFPAR( 2, "z-shift" , zzz_m , zzz_p , 0.0 , 0.0 , 0.0 ) ;
   if( do_cmass ){                                            /* 31 Jul 2007 */
     if( nx_base > 1 ) stup.wfunc_param[0].val_pinit = xc ;
     if( ny_base > 1 ) stup.wfunc_param[1].val_pinit = yc ;
     if( nz_base > 1 ) stup.wfunc_param[2].val_pinit = zc ;
   }

   DEFPAR( 3, "z-angle" , -30.0 , 30.0 , 0.0 , 0.0 , 0.0 ) ;  /* degrees */
   DEFPAR( 4, "x-angle" , -30.0 , 30.0 , 0.0 , 0.0 , 0.0 ) ;
   DEFPAR( 5, "y-angle" , -30.0 , 30.0 , 0.0 , 0.0 , 0.0 ) ;

   DEFPAR( 6, "x-scale" , 0.833 , 1.20 , 1.0 , 0.0 , 0.0 ) ;  /* identity */
   DEFPAR( 7, "y-scale" , 0.833 , 1.20 , 1.0 , 0.0 , 0.0 ) ;  /*  == 1.0 */
   DEFPAR( 8, "z-scale" , 0.833 , 1.20 , 1.0 , 0.0 , 0.0 ) ;

   DEFPAR(  9, "y/x-shear" , -0.1111 , 0.1111 , 0.0 , 0.0 , 0.0 ) ;
   DEFPAR( 10, "z/x-shear" , -0.1111 , 0.1111 , 0.0 , 0.0 , 0.0 ) ;
   DEFPAR( 11, "z/y-shear" , -0.1111 , 0.1111 , 0.0 , 0.0 , 0.0 ) ;

   if( nz_base == 1 ){                 /* 2D images */
     stup.wfunc_param[ 2].fixed = 2 ;  /* fixed==2 means cannot be un-fixed */
     stup.wfunc_param[ 4].fixed = 2 ;  /* fixed==1 is 'temporarily fixed'   */
     stup.wfunc_param[ 5].fixed = 2 ;
     stup.wfunc_param[ 8].fixed = 2 ;
     stup.wfunc_param[10].fixed = 2 ;
     stup.wfunc_param[11].fixed = 2 ;
     if( verb && apply_mode == 0 )
       INFO_message("base dataset is 2D ==> froze z-parameters") ;
   }

   /*-- apply any parameter-altering user commands --*/

   for( ii=0 ; ii < nparopt ; ii++ ){
     jj = paropt[ii].np ;
     if( jj < stup.wfunc_numpar ){
       if( stup.wfunc_param[jj].fixed )
         WARNING_message("Altering fixed param#%d [%s]" ,
                          jj+1 , stup.wfunc_param[jj].name ) ;

       switch( paropt[ii].code ){
         case PARC_FIX: stup.wfunc_param[jj].fixed     = 2 ; /* permanent fix */
                        stup.wfunc_param[jj].val_fixed = paropt[ii].vb;
         if( verb > 1 )
           ININFO_message("Fix param#%d [%s] = %f",
                          jj+1 , stup.wfunc_param[jj].name ,
                                 stup.wfunc_param[jj].val_fixed ) ;
         break;

         case PARC_INI: stup.wfunc_param[jj].fixed     = 0 ;
                        stup.wfunc_param[jj].val_fixed =
                        stup.wfunc_param[jj].val_init  =
                        stup.wfunc_param[jj].val_pinit = paropt[ii].vb;
         if( verb > 1 )
           ININFO_message("Init param#%d [%s] = %f",
                          jj+1 , stup.wfunc_param[jj].name ,
                                 stup.wfunc_param[jj].val_pinit ) ;
         break;

         case PARC_RAN:{
           float vb = paropt[ii].vb , vt = paropt[ii].vt ;
           if( do_cmass ){  /* 06 Aug 2007 */
             switch(jj){
               case 0: vb += xc ; vt += xc ; break ;
               case 1: vb += yc ; vt += yc ; break ;
               case 2: vb += zc ; vt += zc ; break ;
             }
           }
           stup.wfunc_param[jj].fixed = 0 ;
           stup.wfunc_param[jj].min   = vb;
           stup.wfunc_param[jj].max   = vt;
           if( verb > 1 )
             ININFO_message("Range param#%d [%s] = %f .. %f",
                            jj+1 , stup.wfunc_param[jj].name ,
                                   stup.wfunc_param[jj].min  ,
                                   stup.wfunc_param[jj].max   ) ;
         }
         break;
       }
     } else {
       WARNING_message("Can't alter parameter #%d: out of range!",jj+1) ;
     }
   }

   /* check to see if we have free parameters so we can actually do something */

   for( ii=jj=0 ; jj < stup.wfunc_numpar ; jj++ )  /* count free params */
     if( !stup.wfunc_param[jj].fixed ) ii++ ;
   if( ii == 0 ) ERROR_exit("No free parameters for aligning datasets?!!") ;
   nparam_free = ii ;
   if( verb > 1 && apply_mode == 0 ) ININFO_message("%d free parameters",ii) ;

   /*-- should have some free parameters in the first 6 if using twopass --*/

   if( twopass ){
     for( ii=jj=0 ; jj < stup.wfunc_numpar && jj < 6 ; jj++ )
       if( !stup.wfunc_param[jj].fixed ) ii++ ;
     if( ii == 0 ){
       WARNING_message("Disabling twopass because no free parameters in first 6!?");
       twopass = 0 ;
     }
   }

   /*-- set convergence radius for parameter search --*/

   if( im_weig == NULL ){
     xsize = xxx = 0.5f * (nx_base-1) * dx_base ;
     ysize = yyy = 0.5f * (ny_base-1) * dy_base ;
     zsize = zzz = 0.5f * (nz_base-1) * dz_base ;
   } else {
     int xm,xp , ym,yp , zm,zp ;
     MRI_autobbox_clust(0) ;
     MRI_autobbox( im_weig , &xm,&xp , &ym,&yp , &zm,&zp ) ;
     MRI_autobbox_clust(1) ;
#if 0
     fprintf(stderr,"xm,xp,nx=%d,%d,%d\n",xm,xp,nx_base) ;
     fprintf(stderr,"ym,yp,ny=%d,%d,%d\n",ym,yp,ny_base) ;
     fprintf(stderr,"zm,zp,nz=%d,%d,%d\n",zm,zp,nz_base) ;
#endif
     xsize = xxx = 0.5f * (xp-xm) * dx_base ;
     ysize = yyy = 0.5f * (yp-ym) * dy_base ;
     zsize = zzz = 0.5f * (zp-zm) * dz_base ;
   }
   xxx = (nz_base > 1) ? cbrt(xxx*yyy*zzz) : sqrt(xxx*yyy) ;
   zzz = 0.01f ;
   for( jj=0 ; jj < 9 && jj < stup.wfunc_numpar ; jj++ ){
     if( stup.wfunc_param[jj].fixed ) continue ;
     siz = stup.wfunc_param[jj].max - stup.wfunc_param[jj].min ;
     if( siz <= 0.0f ) continue ;
          if( jj < 3 ) yyy = conv_mm / siz ;               /* shift */
     else if( jj < 6 ) yyy = 57.3f * conv_mm / (xxx*siz) ; /* angle */
     else              yyy = conv_mm / (xxx*siz) ;         /* scale */
     zzz = MIN(zzz,yyy) ;
   }
   conv_rad = MIN(zzz,0.001f) ; conv_rad = MAX(conv_rad,0.00001f) ;
   if( verb > 1 && apply_mode == 0 )
     INFO_message("Normalized convergence radius = %.6f",conv_rad) ;

   /*-- special case: 04 Apr 2008 --*/

   if( apply_mode == APPLY_BILIN ){
     SETUP_BILINEAR_PARAMS ;
   }

   /*****------ create shell of output dataset ------*****/

   if( prefix == NULL ){
     WARNING_message("No output dataset will be calculated") ;
     if( dxyz_mast > 0.0 )
       WARNING_message("-mast_dxyz %g option was meaningless!",dxyz_mast) ;
   } else {
     if( dset_mast == NULL ){
       if( dset_base != NULL ){
         if( verb ) INFO_message("master dataset for output = base") ;
         dset_mast = dset_base ;
       } else {
         if( verb ) INFO_message("master dataset for output = source") ;
         dset_mast = dset_targ ;
       }
     }
     if( dxyz_mast > 0.0 ){   /* 24 Jul 2007 */
       THD_3dim_dataset *qset ;
       qset = r_new_resam_dset( dset_mast , NULL ,
                                dxyz_mast,dxyz_mast,dxyz_mast ,
                                NULL , RESAM_NN_TYPE , NULL , 0 ) ;
       if( qset != NULL ){
         dset_mast = qset ;
         THD_daxes_to_mat44(dset_mast->daxes) ;
         if( verb )
           INFO_message("changing output grid spacing to %.3f mm",dxyz_mast) ;
       }
     }
     if( !ISVALID_MAT44(dset_mast->daxes->ijk_to_dicom) )
       THD_daxes_to_mat44(dset_mast->daxes) ;

     mast_cmat     = dset_mast->daxes->ijk_to_dicom ;  /* 24 Jul 2007 */
     mast_cmat_inv = MAT44_INV(mast_cmat) ;

     dset_out = EDIT_empty_copy( dset_mast ) ;
     EDIT_dset_items( dset_out ,
                        ADN_prefix    , prefix ,
                        ADN_nvals     , DSET_NVALS(dset_targ) ,
                        ADN_datum_all , MRI_float ,
                      ADN_none ) ;
     if( DSET_NUM_TIMES(dset_targ) > 1 )
       EDIT_dset_items( dset_out ,
                          ADN_ntt   , DSET_NVALS(dset_targ) ,
                          ADN_ttdel , DSET_TR(dset_targ) ,
                          ADN_tunits, UNITS_SEC_TYPE ,
                          ADN_nsl   , 0 ,
                        ADN_none ) ;
     else
       EDIT_dset_items( dset_out ,
                          ADN_func_type , ISANAT(dset_out) ? ANAT_BUCK_TYPE
                                                           : FUNC_BUCK_TYPE ,
                        ADN_none ) ;

     /* copy brick info into output */

     THD_copy_datablock_auxdata( dset_targ->dblk , dset_out->dblk ) ; /* 20 Nov 2007 */
     for( kk=0 ; kk < DSET_NVALS(dset_out) ; kk++ )
       EDIT_BRICK_FACTOR(dset_out,kk,0.0);

     tross_Copy_History( dset_targ , dset_out ) ;
     tross_Make_History( "3dAllineate" , argc,argv , dset_out ) ;

     THD_daxes_to_mat44(dset_out->daxes) ;
     cmat_tout = dset_targ->daxes->ijk_to_dicom ;
     cmat_bout = dset_out ->daxes->ijk_to_dicom ;
     nxout = DSET_NX(dset_out) ; dxout = fabsf(DSET_DX(dset_out)) ;
     nyout = DSET_NY(dset_out) ; dyout = fabsf(DSET_DY(dset_out)) ;
     nzout = DSET_NZ(dset_out) ; dzout = fabsf(DSET_DZ(dset_out)) ;
     nxyz_dout[0] = nxout; nxyz_dout[1] = nyout; nxyz_dout[2] = nzout;
     dxyz_dout[0] = dxout; dxyz_dout[1] = dyout; dxyz_dout[2] = dzout;
   }

   /***---------------------- start alignment process ----------------------***/

#ifdef USE_OMP
#pragma omp parallel
 {
  if( omp_get_thread_num() == 0 )
    INFO_message("OpenMP thread count = %d",omp_get_num_threads()) ;
 }
#endif

/* macros for verbosity */

#undef  PARDUMP
#define PARDUMP(ss,xxx)                                     \
  do{ fprintf(stderr," + %s Parameters =",ss) ;             \
      for( jj=0 ; jj < stup.wfunc_numpar ; jj++ ){          \
        if( jj == 12 ) fprintf(stderr," |") ;               \
        fprintf(stderr," %.4f",stup.wfunc_param[jj].xxx) ;  \
      }                                                     \
      fprintf(stderr,"\n") ;                                \
  } while(0)
#undef  PAROUT
#define PAROUT(ss) PARDUMP(ss,val_out)
#undef  PARINI
#define PARINI(ss) PARDUMP(ss,val_init)
#undef  PARVEC
#define PARVEC(ss,vv)                              \
  do{ fprintf(stderr," + %s Parameters =",ss) ;    \
      for( jj=0 ; jj < stup.wfunc_numpar ; jj++ )  \
        fprintf(stderr," %.4f",vv[jj]) ;           \
      fprintf(stderr,"\n") ;                       \
  } while(0)

#undef  PAR_CPY
#define PAR_CPY(xxx)                              \
  do{ for( jj=0 ; jj < stup.wfunc_numpar ; jj++ ) \
        allpar[jj] = stup.wfunc_param[jj].xxx ;   \
  } while(0)

   /*-- the annunciation --*/

   if( do_allcost >= 0 && verb ){
     if( apply_1D == NULL )
       INFO_message("======= Allineation of %d sub-bricks using %s =======",
                    DSET_NVALS(dset_targ) , meth_username[meth_code-1] ) ;
     else
       INFO_message("========== Applying transformation to %d sub-bricks ==========",
                    DSET_NVALS(dset_targ) ) ;
   }

   if( verb > 1 ) mri_genalign_verbose(verb-1) ;  /* inside mri_genalign.c */

   /*-- array in which to save parameters for later waterboarding --*/

   if( param_save_1D != NULL || apply_mode != APPLY_AFF12 )
     parsave = (float **)calloc(sizeof(float *),DSET_NVALS(dset_targ)) ;

   if( apply_mode != APPLY_BILIN ){                                     /* 04 Apr 2008 */
    if( matrix_save_1D != NULL || apply_mode != APPLY_AFF12  )
      matsave = (mat44 * )calloc(sizeof(mat44),DSET_NVALS(dset_targ)) ; /* 23 Jul 2007 */
   }

#undef  SAVEHIST
#define SAVEHIST(nnn,docc)                                                 \
 do{ int nbin ; float *xyc ;                                               \
     if( docc ) (void)mri_genalign_scalar_cost( &stup , NULL ) ;           \
     nbin = retrieve_2Dhist( &xyc ) ;                                      \
     if( nbin > 0 && xyc != NULL ){                                        \
       char fname[256] ; MRI_IMAGE *fim ; double ftop ;                    \
       fim = mri_new(nbin,nbin,MRI_float); mri_fix_data_pointer(xyc,fim);  \
       if( strstr(save_hist,"FF") == NULL ){                               \
         ftop = mri_max(fim) ; qim = mri_to_byte_scl(255.4/ftop,0.0,fim) ; \
         mri_clear_data_pointer(fim); mri_free(fim);                       \
         fim = mri_flippo(MRI_ROT_90,0,qim); mri_free(qim);                \
         sprintf(fname,"%s_%s_%04d.pgm",save_hist,nnn,kk) ;                \
         mri_write_pnm(fname,fim); mri_free(fim);                          \
       } else {                                                            \
         qim = mri_flippo(MRI_ROT_90,0,fim);                               \
         mri_clear_data_pointer(fim); mri_free(fim);                       \
         sprintf(fname,"%s_%s_%04d.mri",save_hist,nnn,kk) ;                \
         mri_write(fname,qim); mri_free(qim);                              \
       }                                                                   \
       if( verb ) ININFO_message("- Saved histogram to %s",fname) ;        \
     }                                                                     \
 } while(0)

   /***-------------------- loop over target sub-bricks --------------------***/

   im_bset = im_base ;  /* base image for first loop */
   im_wset = im_weig ;

   if( im_tmask != NULL ){
     mri_genalign_set_targmask( im_tmask , &stup ) ;  /* 07 Aug 2007 */
     mri_free(im_tmask) ; im_tmask = NULL ;           /* is copied inside */
   }

   MEMORY_CHECK("about to start alignment loop") ;

   if( sm_rad == 0.0f &&
       ( meth_code == GA_MATCH_PEARSON_LOCALS ||
         meth_code == GA_MATCH_PEARSON_LOCALA   ) ) sm_rad = 2.222f ;

   for( kk=0 ; kk < DSET_NVALS(dset_targ) ; kk++ ){

     stup.match_code = meth_code ;

     ZERO_MAT44(aff12_xyz) ; /* 23 Jul 2007: invalidate */

     bfac = DSET_BRICK_FACTOR(dset_targ,kk) ;  /* 14 Oct 2008 */

     skipped = 0 ;
     if( kk == 0 && skip_first ){  /* skip first image since it == im_base */
       if( verb )
         INFO_message("========= Skipping sub-brick #0: it's also base image =========");
       DSET_unload_one(dset_targ,0) ;

       /* load parameters with identity transform */

       for( jj=0 ; jj < stup.wfunc_numpar ; jj++ )   /* for -1Dfile output */
         stup.wfunc_param[jj].val_out = stup.wfunc_param[jj].ident ;

       /* load aff12_xyz matrix with identity transform [23 Jul 2007] */

       LOAD_DIAG_MAT44(aff12_xyz,1.0f,1.0f,1.0f) ;
       skipped = 1 ; goto WRAP_IT_UP_BABY ;
     }

     /* make copy of target brick, and deal with that */

     if( verb )
       INFO_message("========== sub-brick #%d ========== [total CPU=%.1f s]",
                    kk , COX_cpu_time() ) ;

     im_targ = mri_scale_to_float( bfac , DSET_BRICK(dset_targ,kk) ) ;
     DSET_unload_one(dset_targ,kk) ;

     /*** if we are just applying input parameters, set up for that now ***/

     if( apply_1D != NULL ){
       int rr=kk ;
       if( rr >= apply_ny ){  /* 19 Jul 2007 */
         rr = apply_ny-1 ;
         WARNING_message("Re-using final row of -1D*_apply '%s' for sub-brick #%d",
                         apply_1D , kk ) ;
       }
       stup.interp_code = final_interp ;
       stup.smooth_code = 0 ;
       stup.npt_match   = 11 ;
       mri_genalign_scalar_setup( im_bset , NULL , im_targ , &stup ) ;
       im_bset = NULL ;  /* after setting base, don't need to set it again */
       mri_free(im_targ) ; im_targ = NULL ;

       switch( apply_mode ){   /* 23 Jul 2007 */
         default:
         case APPLY_BILIN:
         case APPLY_PARAM:     /* load parameters from file into structure */
           if( verb > 1 ) INFO_message("using -1Dparam_apply") ;
           for( jj=0 ; jj < stup.wfunc_numpar ; jj++ )
             stup.wfunc_param[jj].val_out = APL(jj,rr) ;
         break ;

         case APPLY_AFF12:     /* load matrix from file into aff12_xyz */
           if( verb > 1 ) INFO_message("using -1Dmatrix_apply") ;
           LOAD_MAT44_AR( aff12_xyz , &APL(0,rr) ) ;    /* DICOM coord matrix */
         break ;
       }
       goto WRAP_IT_UP_BABY ;
     }

     /* initialize parameters (for the -onepass case) */

     for( jj=0 ; jj < stup.wfunc_numpar ; jj++ )
       stup.wfunc_param[jj].val_init = stup.wfunc_param[jj].val_pinit ;

     /* initialize coordinate systems */

     AL_setup_warp_coords( epi_targ,epi_fe,epi_pe,epi_se,
                           nxyz_base, dxyz_base, stup.base_cmat,
                           nxyz_targ, dxyz_targ, stup.targ_cmat ) ;

     if( do_allcost != 0 ){  /*-- print all cost functionals, for fun? --*/

       stup.interp_code = MRI_LINEAR ;
       stup.npt_match   = npt_match ;
       if( do_allcost < 0 && fine_rad > 0.0f ){
         stup.smooth_code        = sm_code ;
         stup.smooth_radius_base = stup.smooth_radius_targ = fine_rad ;
       }

       mri_genalign_scalar_setup( im_bset , im_wset , im_targ , &stup ) ;

       if( allcostX1D == NULL ){ /* just do init parameters == the old way */

         PAR_CPY(val_init) ;   /* copy init parameters into the allpar arrary */
         allcost = mri_genalign_scalar_allcosts( &stup , allpar ) ;
         PARINI("initial") ;
         INFO_message("allcost output: init #%d",kk) ;
         for( jj=0 ; jj < GA_MATCH_METHNUM_SCALAR ; jj++ )
           fprintf(stderr,"   %-3s = %g\n",meth_shortname[jj],allcost->ar[jj]) ;
         KILL_floatvec(allcost) ;

         if( save_hist != NULL ) SAVEHIST("allcost_init",0) ;
         if( do_allcost == -1 ) continue ;  /* skip to next sub-brick */

       } else {  /* 02 Sep 2008: do a bunch of parameter vectors */

         float *av=MRI_FLOAT_PTR(allcostX1D); int nxp=allcostX1D->nx; FILE *fp;

         if( strcmp(allcostX1D_outname,"-")      == 0 ||
             strcmp(allcostX1D_outname,"stdout") == 0   ){
           fp = stdout ;
         } else {
           fp = fopen( allcostX1D_outname , "w" ) ;
           if( fp == NULL )
             ERROR_exit("Can't open file '%s' for -allcostX1D output!" ,
                        allcostX1D_outname ) ;
         }
         INFO_message("Writing -allcostX1D results to '%s'",allcostX1D_outname) ;
         fprintf( fp , "# 3dAllineate -allcostX1D results:\n" ) ;
         fprintf( fp , "#" ) ;
         for( jj=0 ; jj < GA_MATCH_METHNUM_SCALAR ; jj++ )
           fprintf( fp , "  ___ %-3s ___",meth_shortname[jj]) ;
         fprintf( fp , "\n") ;
         for( ii=0 ; ii < allcostX1D->ny ; ii++ ){
           allcost = mri_genalign_scalar_allcosts( &stup , av + ii*nxp ) ;
           fprintf( fp , " " ) ;
           for( jj=0 ; jj < GA_MATCH_METHNUM_SCALAR ; jj++ )
             fprintf( fp , " %12.6f" , allcost->ar[jj] ) ;
           fprintf( fp , "\n") ;
           KILL_floatvec(allcost) ;
           if( save_hist != NULL ){
             char fn[32] ; sprintf(fn,"allcost%06d",ii) ; SAVEHIST(fn,0) ;
           }
         }
         if( fp != stdout ) fclose(fp) ;
         INFO_message("-allcostX1D finished") ; exit(0) ;
       }
     }

     /*-------- do coarse resolution pass? --------*/

     didtwo = 0 ;
     if( twopass && (!twofirst || !tfdone) ){
       int tb , ib , ccode ;
       if( verb ) INFO_message("Start coarse pass") ;
       ccode            = (interp_code == MRI_NN) ? MRI_NN : MRI_LINEAR ;
       stup.interp_code = ccode ;
       stup.npt_match   = ntask / 15 ;
       if( stup.npt_match < nmatch_setup ) stup.npt_match = nmatch_setup;

       stup.smooth_code        = sm_code ;
       stup.smooth_radius_base =
        stup.smooth_radius_targ = (sm_rad == 0.0f) ? 7.777f : sm_rad ;

       mri_genalign_scalar_setup( im_bset , im_wset , im_targ , &stup ) ;
       im_bset = NULL; im_wset = NULL;  /* after being set, needn't set again */
       if( usetemp ){
         mri_purge(im_targ); mri_purge(im_base); mri_purge(im_weig);
       }

       if( save_hist != NULL ) SAVEHIST("start",1) ;

       /*- search for coarse start parameters, then optimize them? -*/

       if( tbest > 0 ){  /* default tbest==4 */
         int nrefine ;

         if( verb ) ININFO_message("- Search for coarse starting parameters") ;

         /* startup search only allows up to 6 parameters, so freeze excess */

         nptwo = (int)AFNI_numenv("AFNI_TWOPASS_NUM") ;
         if( nptwo < 1 || nptwo > 6 ) nptwo = 6 ;
         if( nparam_free > nptwo ){
           for( ii=jj=0 ; jj < stup.wfunc_numpar ; jj++ ){
             if( !stup.wfunc_param[jj].fixed ){
               ii++ ;  /* number free so far */
               if( ii > nptwo ) stup.wfunc_param[jj].fixed = 1 ;  /* temp freeze */
             }
           }
         }

         /* do the startup parameter search:
              saves best param set in val_init (and val_out),
              plus a few more good sets in val_trial for refinement */

         if( verb > 1 ) ctim = COX_cpu_time() ;

         mri_genalign_scalar_ransetup( &stup , 31 ) ;  /* the initial search! */

         if( verb > 1 ) ININFO_message("- Search CPU time = %.1f s",COX_cpu_time()-ctim);

         /* unfreeze those that were temporarily frozen above */

         for( jj=0 ; jj < stup.wfunc_numpar ; jj++ )
           if( stup.wfunc_param[jj].fixed == 1 ) stup.wfunc_param[jj].fixed = 0 ;

         /*-- now refine the tbest values saved already (from val_trial) --*/

         tb = MIN(tbest,stup.wfunc_ntrial) ; nfunc=0 ;
         if( verb > 1 ) ctim = COX_cpu_time() ;

         for( ib=0 ; ib < tb ; ib++ )
           for( jj=0 ; jj < stup.wfunc_numpar ; jj++ )
             tfparm[ib][jj] = stup.wfunc_param[jj].val_trial[ib] ;

         /* add identity transform to set, for comparisons */

         for( jj=0 ; jj < stup.wfunc_numpar ; jj++ )
           tfparm[tb][jj] = stup.wfunc_param[jj].val_pinit ;

         tfdone = tb+1 ;  /* number of parameter sets now saved in tfparm */

         nrefine = (int)AFNI_numenv("AFNI_TWOPASS_REFINE") ;
         if( nrefine <= 0 || nrefine >= 3 ) nrefine = 3 ;
         rad = 0.0444 ;  /* initial search radius in parameter space */

         for( rr=0 ; rr < nrefine ; rr++ , rad*=0.6789 ){ /* refine with less smoothing */

           if( verb > 1 )
             INFO_message("Start refinement #%d on %d coarse parameter sets",rr+1,tfdone);

           stup.smooth_radius_base *= 0.7071 ;  /* less smoothing */
           stup.smooth_radius_targ *= 0.7071 ;
           stup.npt_match          *= 1.5 ;     /* more points for matching */
           mri_genalign_scalar_setup( NULL,NULL,NULL , &stup ) ;

           for( ib=0 ; ib < tfdone ; ib++ ){              /* loop over param sets */
             for( jj=0 ; jj < stup.wfunc_numpar ; jj++ )  /* load parameters */
               stup.wfunc_param[jj].val_init = tfparm[ib][jj] ;

             nfunc += mri_genalign_scalar_optim( &stup, rad, 0.0666*rad, 99 ) ;

             for( jj=0 ; jj < stup.wfunc_numpar ; jj++ )  /* save optimized params */
               tfparm[ib][jj] = stup.wfunc_param[jj].val_out ;

             tfcost[ib] = stup.vbest ; tfindx[ib] = ib ;  /* save cost */
             if( verb > 1 )
               ININFO_message("- param set #%d has cost=%f",ib+1,stup.vbest) ;
             if( verb > 2 ) PAROUT("--") ;
           }

           /* 29 Aug 2008: sort tfparm by cost, then cast out the close ones */

           if( !nocast && tfdone > 2 ){
             int jb,ncast=0 ; float pdist ;

             if( verb > 1 ) ININFO_message("- sorting parameter sets by cost") ;
             for( ib=0 ; ib < tfdone ; ib++ )       /* copy tfparm into ffparm */
               memcpy( ffparm[ib], tfparm[ib], sizeof(float)*stup.wfunc_numpar );
             qsort_floatint( tfdone , tfcost , tfindx ) ;      /* sort by cost */
             for( ib=0 ; ib < tfdone ; ib++ ){        /* copy back into tfparm */
               jb = tfindx[ib] ;      /* jb = index in unsorted copy in ffparm */
               memcpy( tfparm[ib], ffparm[jb], sizeof(float)*stup.wfunc_numpar );
             }

             /* now cast out parameter sets that are very close to the best one */

#undef  CTHRESH
#define CTHRESH 0.02f
             if( verb > 1 ) ININFO_message("-- scanning for distances from #1") ;
             for( ib=1 ; ib < tfdone ; ib++ ){
               pdist = param_dist( &stup , tfparm[0] , tfparm[ib] ) ;
               if( verb > 2 ) ININFO_message("--- dist(#%d,#1) = %.3g %s" ,
                                             ib+1, pdist, (pdist<CTHRESH)?"XXX":"" ) ;
               if( tfdone > 2 && pdist < CTHRESH ){
                 for( jb=ib+1 ; jb < tfdone ; jb++ )  /* copy those above down */
                   memcpy( tfparm[jb-1], tfparm[jb], sizeof(float)*stup.wfunc_numpar );
                 ncast++ ; tfdone-- ;
               }
             }
             if( ncast > 0 && verb > 1 )
               ININFO_message(
                 "- cast out %d parameter set%s for being too close to best set" ,
                 ncast , (ncast==1)?"":"s" ) ;
           }

         } /* end of refinement loop (rr) */

         if( verb > 1 )
           ININFO_message("- Total coarse refinement CPU time = %.1f s; %d funcs",
                          COX_cpu_time()-ctim,nfunc ) ;

         /* end of '-twobest x' for x > 0 */

       } else {  /*- if stoopid user did '-twobest 0' -*/
                 /*- just optimize coarse setup from default parameters -*/

         if( verb     ) ININFO_message("- Start coarse optimization with -twobest 0") ;
         if( verb > 1 ) ctim = COX_cpu_time() ;
         nfunc = mri_genalign_scalar_optim( &stup , 0.05 , 0.005 , 666 ) ;
         if( verb > 2 ) PAROUT("--(a)") ;
         stup.npt_match = ntask / 7 ;
         if( stup.npt_match < nmatch_setup  ) stup.npt_match = nmatch_setup ;
         stup.smooth_radius_base *= 0.456 ;
         stup.smooth_radius_targ *= 0.456 ;
         mri_genalign_scalar_setup( NULL,NULL,NULL , &stup ) ;
         nfunc += mri_genalign_scalar_optim( &stup , 0.0333 , 0.00333 , 666 ) ;
         if( verb > 2 ) PAROUT("--(b)") ;
         stup.smooth_radius_base *= 0.456 ;
         stup.smooth_radius_targ *= 0.456 ;
         mri_genalign_scalar_setup( NULL,NULL,NULL , &stup ) ;
         nfunc += mri_genalign_scalar_optim( &stup , 0.0166 , 0.00166 , 666 ) ;
         if( verb > 2 ) PAROUT("--(c)") ;
         if( verb > 1 ) ININFO_message("- Coarse CPU time = %.1f s; %d funcs",
                                       COX_cpu_time()-ctim,nfunc) ;
         if( verb     ) ININFO_message("- Coarse optimization:  best cost=%f",
                                       stup.vbest) ;

         for( jj=0 ; jj < stup.wfunc_numpar ; jj++ )  /* save best params */
           tfparm[0][jj] = stup.wfunc_param[jj].val_out ;
         tfdone = 1 ;  /* number of parameter sets saved in tfparm */

       } /* end of '-twobest 0' */

       /*- 22 Sep 2006: add default init params to the tfparm list -*/
       /*-              (so there will be at least 2 sets there)   -*/

       for( jj=0 ; jj < stup.wfunc_numpar ; jj++ )
         tfparm[tfdone][jj] = stup.wfunc_param[jj].val_pinit ;
       tfdone++ ;

       didtwo = 1 ;   /* mark that we did the first pass */

     } /*------------- end of twopass-ization -------------*/

     /*-----------------------------------------------------------------------*/
     /*----------------------- do final resolution pass ----------------------*/

     if( verb ) INFO_message("*** Fine pass begins ***") ;
     ctim = COX_cpu_time() ;

     stup.interp_code = interp_code ;  /* set interpolation   */
     stup.smooth_code = sm_code ;      /* and smoothing codes */

     /*-- setup smoothing --*/

     if( fine_rad > 0.0f ){  /* if ordered by user */
       stup.smooth_radius_base = stup.smooth_radius_targ = fine_rad ;
     } else if( diffblur ){  /* if base finer resolution than target */
       float br,tr ;
       if( nz_base > 1 ){
         br = cbrt(dx_base*dy_base*dz_base) ;  /* base voxel size */
         tr = cbrt(dx_targ*dy_targ*dz_targ) ;  /* targ voxel size */
       } else {
         br = sqrt(dx_base*dy_base) ;
         tr = sqrt(dx_targ*dy_targ) ;
       }
       stup.smooth_radius_targ = 0.0f ;
       stup.smooth_radius_base = (tr <= 1.1f*br) ? 0.0f
                                                 : sqrt(tr*tr-br*br) ;
     }

     stup.npt_match = npt_match ;
     if( didtwo )                                  /* did first pass already: */
       mri_genalign_scalar_setup( NULL,NULL,NULL, &stup ); /* simple re-setup */
     else {
       mri_genalign_scalar_setup( im_bset , im_wset , im_targ , &stup ) ;
       im_bset = NULL; im_wset = NULL;  /* after being set, needn't set again */
       if( usetemp ) mri_purge( im_targ ) ;
     }

     switch( tfdone ){                  /* initial param radius for optimizer */
        case 0: rad = 0.0345 ; break ;
        case 1:
        case 2: rad = 0.0266 ; break ;
       default: rad = 0.0166 ; break ;
     }
     if( rad < 22.2f*conv_rad ) rad = 22.2f*conv_rad ;

     /*-- choose initial parameters, based on interp_code cost functional --*/

     if( tfdone ){                           /* find best in tfparm array */
       int kb=0 , ib ; float cbest=1.e+33 ;

       if( verb > 1 )
         INFO_message("Picking best parameter set out of %d cases",tfdone) ;
       for( ib=0 ; ib < tfdone ; ib++ ){
         cost = mri_genalign_scalar_cost( &stup , tfparm[ib] ) ;
         if( verb > 1 ) ININFO_message("- cost(#%d)=%f %c",
                                       ib+1,cost,(cost<cbest)?'*':' ');
         if( verb > 2 ) PARVEC("--",tfparm[ib]) ;
         if( cost < cbest ){ cbest=cost ; kb=ib ; }  /* save best case */
       }

       if( num_rtb == 0 ){  /* 27 Aug 2008: this was the old way,  */
                            /* to just pick the best at this point */
         if( verb > 1 )
           ININFO_message("-num_rtb 0 ==> pick best of the %d cases (#%d)",tfdone,kb+1);
         for( jj=0 ; jj < stup.wfunc_numpar ; jj++ )        /* copy best set */
           stup.wfunc_param[jj].val_init = tfparm[kb][jj] ; /* for fine work */

         for( ib=0 ; ib < tfdone ; ib++ )  /* save all cases into ffparm */
           memcpy( ffparm[ib], tfparm[ib], sizeof(float)*stup.wfunc_numpar ) ;

       } else {             /* now: try to make these a little better instead */

         if( verb > 1 )
           ININFO_message("-num_rtb %d ==> refine all %d cases",num_rtb,tfdone);
         cbest = 1.e+33 ;
         for( ib=0 ; ib < tfdone ; ib++ ){
           for( jj=0 ; jj < stup.wfunc_numpar ; jj++ )
             stup.wfunc_param[jj].val_init = tfparm[ib][jj] ;
           nfunc = mri_genalign_scalar_optim( &stup, rad, 0.0777*rad,
                                              (ib==tfdone-1) ? 2*num_rtb : num_rtb );
           for( jj=0 ; jj < stup.wfunc_numpar ; jj++ )       /* save refined */
             ffparm[ib][jj] = stup.wfunc_param[jj].val_out ; /* parameters */
           cost = stup.vbest ;
           if( verb > 1 ) ININFO_message("- cost(#%d)=%f %c",
                                         ib+1,cost,(cost<cbest)?'*':' ' );
           if( verb > 2 ) PAROUT("--") ;
           if( cost < cbest ){ cbest=cost ; kb=ib ; }  /* save best case */
         }
         if( verb > 1 ) ININFO_message("- case #%d is now the best",kb+1) ;
         for( jj=0 ; jj < stup.wfunc_numpar ; jj++ )
           stup.wfunc_param[jj].val_init = ffparm[kb][jj] ;
       }
       cost_ini = cbest ;

     } else {  /*-- did not do first pass, so we start at default params --*/

       cost_ini = mri_genalign_scalar_cost( &stup , NULL ) ;

     }

     if( do_allcost != 0 ){  /*-- print out all cost functionals, for fun --*/
       PAR_CPY(val_init) ;   /* copy init parameters into allpar[] */
       allcost = mri_genalign_scalar_allcosts( &stup , allpar ) ;
       INFO_message("allcost output: start fine #%d",kk) ;
       for( jj=0 ; jj < GA_MATCH_METHNUM_SCALAR ; jj++ )
         fprintf(stderr,"   %-3s = %g\n",meth_shortname[jj],allcost->ar[jj]) ;
       KILL_floatvec(allcost) ;
       if( save_hist != NULL ) SAVEHIST("allcost_finestart",0) ;
     }

     if( verb > 1 ){
       ININFO_message("- Initial  cost = %f",cost_ini) ;
       PARINI("- Initial fine") ;
     }

     if( powell_mm > 0.0f ) powell_set_mfac( powell_mm , powell_aa ) ;
     nfunc = 0 ;

     /*-- start with some optimization with linear interp, for speed? --*/

     if( num_rtb == 0 &&
         (MRI_HIGHORDER(interp_code) || npt_match > 999999) ){
       float pini[MAXPAR] ;
       stup.interp_code = MRI_LINEAR ;
       stup.npt_match   = MIN(499999,npt_match) ;
       mri_genalign_scalar_setup( NULL,NULL,NULL, &stup ) ;
       if( verb > 1 ) ININFO_message("- start Intermediate optimization") ;
       /*** if( verb > 2 ) GA_do_params(1) ; ***/

       nfunc = mri_genalign_scalar_optim( &stup, rad, 0.0666*rad, 333 );

       for( jj=0 ; jj < stup.wfunc_numpar ; jj++ ){
         pini[jj] = stup.wfunc_param[jj].val_init ;
         stup.wfunc_param[jj].val_init = stup.wfunc_param[jj].val_out ;
       }

       stup.interp_code = interp_code ;  /* check cost of result with  */
       stup.npt_match   = npt_match ;    /* actual final interp method */
       mri_genalign_scalar_setup( NULL,NULL,NULL, &stup ) ;
       cost = mri_genalign_scalar_cost( &stup , NULL ) ; /* interp_code, not LINEAR */
       if( cost > cost_ini ){   /* should not happen, but it could since  */
         if( verb > 1 )         /* LINEAR cost optimized above isn't same */
           ININFO_message("- Intrmed  cost = %f > Initial cost = %f :-(",cost,cost_ini);
         for( jj=0 ; jj < stup.wfunc_numpar ; jj++ )
           stup.wfunc_param[jj].val_init = pini[jj] ;
       } else {
         if( verb > 1 ){
           PARINI("- Intrmed fine") ;
           ININFO_message("- Intrmed  cost = %f ; %d funcs",cost,nfunc) ;
         }
         if( nfunc < 333 ){
           rad *= 0.246f ; if( rad < 9.99f*conv_rad ) rad = 9.99f*conv_rad ;
         }
       }

       if( do_allcost != 0 ){  /*-- all cost functionals for fun again --*/
         PAR_CPY(val_init) ;   /* copy init parameters into allpar[] */
         allcost = mri_genalign_scalar_allcosts( &stup , allpar ) ;
         INFO_message("allcost output: intermed fine #%d",kk) ;
         for( jj=0 ; jj < GA_MATCH_METHNUM_SCALAR ; jj++ )
           fprintf(stderr,"   %-3s = %g\n",meth_shortname[jj],allcost->ar[jj]) ;
         KILL_floatvec(allcost) ;
         if( save_hist != NULL ) SAVEHIST("allcost_fineintermed",0) ;
       }
     }

     /*-- now do the final final optimization, with the correct interp mode --*/

     /*** if( verb > 2 ) GA_do_params(1) ; ***/

     nfunc = mri_genalign_scalar_optim( &stup , rad, conv_rad,6666 );

     if( do_refinal ){  /*-- 14 Nov 2007: a final final optimization? --*/
       if( verb > 1 )
         ININFO_message("- Finalish cost = %f ; %d funcs",stup.vbest,nfunc) ;
       for( jj=0 ; jj < stup.wfunc_numpar ; jj++ )
         stup.wfunc_param[jj].val_init = stup.wfunc_param[jj].val_out;
       stup.need_hist_setup = 1 ;
       nfunc = mri_genalign_scalar_optim( &stup , 0.444f*rad, conv_rad,6666 );
     }

     /*** if( powell_mm > 0.0f ) powell_set_mfac( 0.0f , 0.0f ) ; ***/
     /*** if( verb > 2 ) GA_do_params(0) ; ***/

     if( verb ) ININFO_message("- Final    cost = %f ; %d funcs",stup.vbest,nfunc) ;
     if( verb > 1 && meth_check_count < 1 ) PAROUT("Final fine fit") ;
     if( verb > 1 ) ININFO_message("- Fine CPU time = %.1f s",COX_cpu_time()-ctim) ;

     if( save_hist != NULL ) SAVEHIST("final",1) ;

     if( do_allcost != 0 ){  /*-- all costs at final affine solution? --*/
       PAR_CPY(val_out) ;    /* copy output parameters into allpar[] */
       allcost = mri_genalign_scalar_allcosts( &stup , allpar ) ;
       INFO_message("allcost output: final fine #%d",kk) ;
       for( jj=0 ; jj < GA_MATCH_METHNUM_SCALAR ; jj++ )
         fprintf(stderr,"   %-3s = %g\n",meth_shortname[jj],allcost->ar[jj]) ;
       KILL_floatvec(allcost) ;
       if( save_hist != NULL ) SAVEHIST("allcost_finefinal",0) ;
     }

#if 0
     for( jj=0 ; jj < stup.wfunc_numpar ; jj++ )
       stup.wfunc_param[jj].val_init = stup.wfunc_param[jj].val_out ;
     mri_genalign_verbose(9) ;
     cost = mri_genalign_scalar_cost( &stup , NULL ) ;
     INFO_message("Recomputed final cost = %g",cost) ;
     if( verb > 1 ) mri_genalign_verbose(verb-1) ;
#endif

     /*----------------------------------------------------------------------*/
     /*--------------- Nonlinear warp improvement? --------------------------*/

     if( nwarp_pass ){

       if( nwarp_type == WARP_BILINEAR ){  /*------ special case ------------*/

         float rr , xcen,ycen,zcen , brad ; int nbf ;

         rr = MAX(xsize,ysize) ; rr = MAX(zsize,rr) ; rr = 1.2f / rr ;

         SETUP_BILINEAR_PARAMS ;  /* nonlinear params */

         /* nonlinear transformation is centered at middle of base volume
            indexes (xcen,ycen,zcen) and is scaled by reciprocal of size (rr) */

         MAT44_VEC( stup.base_cmat,
                    0.5f*nx_base, 0.5f*ny_base, 0.5f*nz_base,
                    xcen        , ycen        , zcen         ) ;
         stup.wfunc_param[NPBIL  ].val_fixed = stup.wfunc_param[NPBIL  ].val_init = xcen;
         stup.wfunc_param[NPBIL+1].val_fixed = stup.wfunc_param[NPBIL+1].val_init = ycen;
         stup.wfunc_param[NPBIL+2].val_fixed = stup.wfunc_param[NPBIL+2].val_init = zcen;
         stup.wfunc_param[NPBIL+3].val_fixed = stup.wfunc_param[NPBIL+3].val_init = rr  ;

         /* affine part is copied from results of work thus far */

         for( jj=0 ; jj < 12 ; jj++ )
           stup.wfunc_param[jj].val_init = stup.wfunc_param[jj].val_out;

         stup.need_hist_setup = 1 ;
         mri_genalign_scalar_setup( NULL,NULL,NULL, &stup );

         /* do the first pass of the bilinear optimization */

         if( verb > 0 ) INFO_message("Start bilinear warping") ;
         if( verb > 1 ) PARINI("- Bilinear initial") ;
         for( jj=12 ; jj <= 14 ; jj++ ){
           stup.wfunc_param[jj   ].fixed = 0 ;  /* just free up diagonal */
           stup.wfunc_param[jj+12].fixed = 0 ;  /* elements of B tensor */
           stup.wfunc_param[jj+24].fixed = 0 ;
         }
         if( verb ) ctim = COX_cpu_time() ;
         brad = MAX(conv_rad,0.001f) ;
              if( rad > 33.3f*brad ) rad = 33.3f*brad ;
         else if( rad < 22.2f*brad ) rad = 22.2f*brad ;
         nbf = mri_genalign_scalar_optim( &stup , rad, 11.1f*brad, 555 );
         if( verb ){
           dtim = COX_cpu_time() ;
           ININFO_message("- Bilinear#1 cost = %f ; %d funcs ; CPU = %.1f s",
                          stup.vbest,nbf,dtim-ctim) ;
           ctim = dtim ;
         }

         /* do the second pass, with more parameters varying */

         for( jj=12 ; jj < NPBIL ; jj++ )   /* now free up all B elements */
           stup.wfunc_param[jj].fixed = 0 ;
         for( jj=0  ; jj < NPBIL ; jj++ )
           stup.wfunc_param[jj].val_init = stup.wfunc_param[jj].val_out;
         nbf = mri_genalign_scalar_optim( &stup, 22.2f*brad, 3.33f*brad,2222 );
         if( verb ){
           dtim = COX_cpu_time() ;
           ININFO_message("- Bilinear#2 cost = %f ; %d funcs ; CPU = %.1f s",
                          stup.vbest,nbf,dtim-ctim) ;
           ctim = dtim ;
         }

         /* run it again to see if it improves any more */

         for( jj=0  ; jj < NPBIL ; jj++ )
           stup.wfunc_param[jj].val_init = stup.wfunc_param[jj].val_out;
         nbf = mri_genalign_scalar_optim( &stup, 4.44f*brad, brad, 222 );
         if( verb ){
           dtim = COX_cpu_time() ;
           ININFO_message("- Bilinear#3 cost = %f ; %d funcs ; CPU = %.1f s",
                          stup.vbest,nbf,dtim-ctim) ;
           ctim = dtim ;
         }
         if( verb > 1 ) PAROUT("- Bilinear final") ;

       } else {   /*----------------------- general nonlinear expansion -----*/

#define GSIZ 3
#define NHH  2

         char str[16] , xyz[4]="xyz" ;
         Warpfield *wf ;   /* cf. mri_warpfield.[ch] */
         int wf_nparam , nbf , ngrp , gg , hh ;
         float xbot,ybot,zbot, xtop,ytop,ztop, xcen,ycen,zcen , brad , rr,vv ;

         MAT44_VEC( stup.base_cmat,
                    0.5f*nx_base, 0.5f*ny_base, 0.5f*nz_base,
                    xcen        , ycen        , zcen         ) ;

         MAT44_VEC( stup.base_cmat,
                    0.5f*nx_base-0.5f*xsize/dx_base-0.99f ,
                    0.5f*ny_base-0.5f*ysize/dy_base-0.99f ,
                    0.5f*nz_base-0.5f*zsize/dz_base-0.99f ,
                    xbot , ybot , zbot ) ;

         MAT44_VEC( stup.base_cmat,
                    0.5f*nx_base+0.5f*xsize/dx_base+0.99f ,
                    0.5f*ny_base+0.5f*ysize/dy_base+0.99f ,
                    0.5f*nz_base+0.5f*zsize/dz_base+0.99f ,
                    xtop , ytop , ztop ) ;

         if( xbot > xtop ){ brad=xbot ; xbot=xtop ; xtop=brad; }
         if( ybot > ytop ){ brad=ybot ; ybot=ytop ; ytop=brad; }
         if( zbot > ztop ){ brad=zbot ; zbot=ztop ; ztop=brad; }

         wf = Warpfield_init( nwarp_type , nwarp_order , 0 , NULL ) ;
         if( wf == NULL )
           ERROR_exit("Can't setup nonlinear Warpfield!?") ;
         mri_genalign_warpfield_set(wf) ;

         ngrp              = (wf->nfun + GSIZ-1) / GSIZ ;
         stup.wfunc_numpar = wf_nparam = 12 + 3*wf->nfun ;
         stup.wfunc        = mri_genalign_warpfield ;
         stup.wfunc_param  = (GA_param *)realloc( (void *)stup.wfunc_param ,
                                                  wf_nparam*sizeof(GA_param) ) ;
         for( jj=12 ; jj < wf_nparam ; jj++ ){
           sprintf(str,"%c#%03d",xyz[jj%3],(jj-9)/3) ;
           DEFPAR( jj,str, -0.05f,0.05f , 0.0f,0.0f,0.0f ) ;
         }

         /* affine part is fixed at results of work thus far */

         for( jj=0 ; jj < 12 ; jj++ ){
           stup.wfunc_param[jj].val_init =
            stup.wfunc_param[jj].val_fixed = stup.wfunc_param[jj].val_out;
           stup.wfunc_param[jj].fixed = 1 ;
         }

         stup.need_hist_setup = 1 ;
         mri_genalign_scalar_setup( NULL,NULL,NULL, &stup );

         if( verb > 0 ){
           INFO_message("----------- Start Warpfield optimization -----------");
           if( verb > 1 )
             ININFO_message(" %d warp parameters per dimension",wf->nfun) ;
         }

         mri_genalign_set_boxsize( xbot,xtop , ybot,ytop , zbot,ztop ) ;

         if( verb > 1 ) PARINI("- Warpfield initial") ;
         rr   = MAX(xsize,ysize)     ; rr = MAX(zsize,rr) ;
         vv   = MAX(dx_base,dy_base) ; vv = MAX(vv,dz_base) ;
         brad = 2.0f * vv / rr ;
         if( brad < 0.003f ) brad = 0.005f ; else if( brad > 0.03f ) brad = 0.03f ;
         rad  = 12.345f * brad ;
         if( verb > 1 ) ININFO_message(" - convergence radius = %.4f",brad) ;

         for( hh=0 ; hh < NHH ; hh++ ){
           for( gg=0 ; gg < ngrp ; gg++ ){

             for( jj=12 ; jj < wf_nparam ; jj++ ){
               if( (jj-12)/(3*GSIZ) == gg ){   /* activate */
                 stup.wfunc_param[jj].fixed = 0 ;
                 stup.wfunc_param[jj].val_init = stup.wfunc_param[jj].val_out ;
               } else {                        /* deactivate */
                 stup.wfunc_param[jj].fixed = 1 ;
                 stup.wfunc_param[jj].val_fixed = stup.wfunc_param[jj].val_out ;
               }
             }

             ctim = COX_cpu_time() ;
             nbf  = mri_genalign_scalar_optim( &stup , rad, 2.345f*brad, 33*GSIZ );
             dtim = COX_cpu_time() ;
             if( verb ){
               ININFO_message("- Warpfield#%d/%d cost = %f ; %d funcs ; CPU = %.1f s",
                              hh*ngrp+gg+1,NHH*ngrp,stup.vbest,nbf,dtim-ctim) ;
               if( verb > 1 ) PAROUT("- Warpfield") ;
             }
           }
           rad *= 0.777f ;
         }

         for( jj=12 ; jj < wf_nparam ; jj++ ){
           stup.wfunc_param[jj].fixed = 0 ;
           stup.wfunc_param[jj].val_init = stup.wfunc_param[jj].val_out ;
         }
         if( verb ) ININFO_message("Start global Warpfield optimization") ;
         ctim = COX_cpu_time() ;
         nbf  = mri_genalign_scalar_optim( &stup , 6.66f*brad, brad, 11*wf->nfun );
         dtim = COX_cpu_time() ;
         if( verb ){
           ININFO_message("- Warpfield final cost = %f ; %d funcs ; CPU = %.1f s",
                          stup.vbest,nbf,dtim-ctim) ;
           if( verb > 1 ) PAROUT("- Warpfield final") ;
         }

       } /* end of Warpfield */

     } /* end of nonlinear warp */

     /*-------- FINALLY HAVE FINISHED -----------------------------*/

     mri_free(im_targ) ; im_targ = NULL ;

#ifdef ALLOW_METH_CHECK
     /*--- 27 Sep 2006: check if results are stable when
                        we optimize a different cost functional ---*/

     if( meth_check_count > 0 ){
       float pval[MAXPAR] , pdist , dmax ; int jmax,jtop ;
       float **aval = NULL ;
       int mm , mc ;

       if( meth_check_count > 1 ){   /* save for median-izing at end */
         aval = (float **)malloc(sizeof(float *)*stup.wfunc_numpar) ;
         for( jj=0 ; jj < stup.wfunc_numpar ; jj++ ){
           aval[jj] = (float *)malloc(sizeof(float)*(meth_check_count+1)) ;
           aval[jj][0] = stup.wfunc_param[jj].val_out ;
         }
       }

       PAROUT("Final fit") ;
       INFO_message("Checking %s (%s) vs other costs",
                    meth_longname[meth_code-1] , meth_shortname[meth_code-1] ) ;
       for( mm=0 ; mm < meth_check_count ; mm++ ){
         mc = meth_check[mm] ; if( mc <= 0 ) continue ;
         if( verb > 1 ){
           ININFO_message("- checking vs cost %s (%s)",
                          meth_longname[mc-1],meth_shortname[mc-1]) ;
           ctim = COX_cpu_time() ;
         }
         for( jj=0 ; jj < stup.wfunc_numpar ; jj++ ) /* save output params */
           stup.wfunc_param[jj].val_init = pval[jj] = stup.wfunc_param[jj].val_out;

         stup.match_code = mc ;
         nfunc = mri_genalign_scalar_optim( &stup, 33.3*conv_rad, conv_rad,666 );
         stup.match_code = meth_code ;

         if( aval != NULL ){
           for( jj=0 ; jj < stup.wfunc_numpar ; jj++ )
             aval[jj][mm+1] = stup.wfunc_param[jj].val_out ;
         }

         /* compute distance between 2 output parameter sets */

         jtop = MIN( 9 , stup.wfunc_numpar ) ; jmax = 0 ;
         for( dmax=0.0f,jj=0 ; jj < jtop ; jj++ ){
           if( !stup.wfunc_param[jj].fixed ){
             pdist = fabsf( stup.wfunc_param[jj].val_out - pval[jj] )
                    /(stup.wfunc_param[jj].max-stup.wfunc_param[jj].min) ;
             if( pdist > dmax ){ dmax = pdist ; jmax = jj ; }
           }
         }

         if( dmax > 20.0*conv_rad )
           WARNING_message(
             "Check vs %s (%s): max parameter discrepancy=%.4f%%! tolerance=%.4f%%",
             meth_longname[mc-1] , meth_shortname[mc-1] , 100.0*dmax , 2000.0*conv_rad ) ;
         else
           ININFO_message(
             "INFO:   Check vs %s (%s): max parameter discrepancy=%.4f%% tolerance=%.4f%%",
             meth_longname[mc-1] , meth_shortname[mc-1] , 100.0*dmax , 2000.0*conv_rad ) ;
         PAROUT("Check fit") ;
         if( verb > 1 )
           ININFO_message("- Check CPU time=%.1f s; funcs=%d; dmax=%f jmax=%d",
                          COX_cpu_time()-ctim , nfunc , dmax , jmax ) ;
         if( do_allcost != 0 ){
           PAR_CPY(val_out) ;  /* copy output parameters into allpar */
           allcost = mri_genalign_scalar_allcosts( &stup , allpar ) ;
           ININFO_message("allcost output: check %s",meth_shortname[mc-1]) ;
           for( jj=0 ; jj < GA_MATCH_METHNUM_SCALAR ; jj++ )
             fprintf(stderr,"   %-3s = %g\n",meth_shortname[jj],allcost->ar[jj]) ;
           KILL_floatvec(allcost) ;
           if( save_hist != NULL ){
             char fn[64] ; sprintf(fn,"allcost_check_%s",meth_shortname[mc-1]);
             SAVEHIST(fn,0);
           }
         }

         for( jj=0 ; jj < stup.wfunc_numpar ; jj++ )
           stup.wfunc_param[jj].val_out = pval[jj] ;  /* restore previous param */
       } /* end of loop over check methods */

       if( aval != NULL ){  /* median-ize the parameter sets */
         for( jj=0 ; jj < stup.wfunc_numpar ; jj++ ){
           pval[jj] = qmed_float( meth_check_count+1 , aval[jj] ) ;
           free((void *)aval[jj]) ;
         }
         free((void *)aval) ;
         fprintf(stderr," + Median of Parameters =") ;
         for( jj=0 ; jj < stup.wfunc_numpar ; jj++ ) fprintf(stderr," %.4f",pval[jj]) ;
         fprintf(stderr,"\n") ;
         if( meth_median_replace ){  /* replace final results with median! */
           ININFO_message("Replacing Final parameters with Median") ;
           for( jj=0 ; jj < stup.wfunc_numpar ; jj++ )
             stup.wfunc_param[jj].val_out = pval[jj] ;
         }
       }

     } /* end of checking */
#endif

     /*- freeze warp-ing parameters (those after #0..5) for later rounds */

     if( warp_freeze && DSET_NVALS(dset_targ) > 1 ){  /* 10 Oct 2006 */
       for( jj=6 ; jj < stup.wfunc_numpar ; jj++ ){
         if( !stup.wfunc_param[jj].fixed ){
           if( verb > 1 ) INFO_message("Freezing parameter #%d [%s] = %.5f",
                                       jj+1 , stup.wfunc_param[jj].name ,
                                              stup.wfunc_param[jj].val_out ) ;
           stup.wfunc_param[jj].fixed = 2 ;
           stup.wfunc_param[jj].val_fixed = stup.wfunc_param[jj].val_out ;
         }
       }
     }

     /*--- do we replace the base image with warped first target image? ---*/

     if( replace_base ){
       float pp[MAXPAR] ; MRI_IMAGE *aim ;
       for( jj=0 ; jj < stup.wfunc_numpar ; jj++ )
         pp[jj] = stup.wfunc_param[jj].val_out ;
       mri_free(im_base) ;
       if( verb > 1 ) INFO_message("Computing replacement base image") ;
       aim = (stup.ajimor != NULL) ? stup.ajimor : stup.ajim ;
       im_base =
        im_bset = mri_genalign_scalar_warpone(
                             stup.wfunc_numpar , pp , stup.wfunc ,
                             aim, nx_base,ny_base,nz_base, final_interp );
#if 0
       im_wset = im_weig ;  /* not needed, since stup 'remembers' the weight */
#endif
       replace_base = 0 ; diffblur = 0 ;
     }
     if( replace_meth ){
       if( verb > 1 ) INFO_message("Replacing meth='%s' with '%s'",
                                   meth_shortname[meth_code] ,
                                   meth_shortname[replace_meth] ) ;
       meth_code = replace_meth; replace_meth = 0;
     }

     /*-- get final DICOM coord transformation matrix [23 Jul 2007] --*/

     { float par[MAXPAR] ;
       for( jj=0 ; jj < stup.wfunc_numpar ; jj++ )
         par[jj] = stup.wfunc_param[jj].val_out ;
#if 0
mri_genalign_set_pgmat(1) ;
#endif
       mri_genalign_affine( stup.wfunc_numpar , par , 0,NULL,NULL,NULL , NULL,NULL,NULL ) ;
       mri_genalign_affine_get_gammaijk( &qmat ) ;
       wmat = MAT44_MUL(targ_cmat,qmat) ;
       aff12_xyz = MAT44_MUL(wmat,base_cmat_inv) ;  /* DICOM coord matrix */
     }

#if 0
DUMP_MAT44("targ_cmat",targ_cmat) ;
DUMP_MAT44("targ_cmat_inv",targ_cmat_inv) ;
DUMP_MAT44("base_cmat",base_cmat) ;
DUMP_MAT44("base_cmat_inv",base_cmat_inv) ;
DUMP_MAT44("aff12_xyz",aff12_xyz) ;
DUMP_MAT44("aff12_ijk",qmat) ;
#endif

     /*--- at this point, val_out contains alignment parameters ---*/

   WRAP_IT_UP_BABY: /***** goto target !!!!! *****/

     /* save parameters for the historical record */

     if( parsave != NULL ){
       parsave[kk] = (float *)malloc(sizeof(float)*stup.wfunc_numpar) ;
       for( jj=0 ; jj < stup.wfunc_numpar ; jj++ )
         parsave[kk][jj] = stup.wfunc_param[jj].val_out ;
     }

     /* save matrix for the hysterical record [23 Jul 2007] */

     if( matsave != NULL ){
       if( ISVALID_MAT44(aff12_xyz) )
         matsave[kk] = aff12_xyz ;
       else
         LOAD_DIAG_MAT44(matsave[kk],1.0f,1.0f,1.0f) ;
     }

     /** store warped volume into the output dataset **/

     if( dset_out != NULL ){
       MRI_IMAGE *aim = (stup.ajimor != NULL) ? stup.ajimor : stup.ajim ;
       if( verb > 1 ) INFO_message("Computing output image") ;
#if 0
mri_genalign_set_pgmat(1) ;
#endif

       switch( apply_mode ){
         default:
         case APPLY_BILIN:
         case APPLY_PARAM:
           AL_setup_warp_coords( epi_targ,epi_fe,epi_pe,epi_se,
                                 nxyz_dout, dxyz_dout, cmat_bout,
                                 nxyz_targ, dxyz_targ, cmat_tout ) ;

           im_targ = mri_genalign_scalar_warpone(
                                 stup.wfunc_numpar , parsave[kk] , stup.wfunc ,
                                 aim , nxout,nyout,nzout, final_interp ) ;
         break ;

         case APPLY_AFF12:{
           float ap[12] ;
#if 0
DUMP_MAT44("aff12_xyz",aff12_xyz) ;
#endif
           wmat = MAT44_MUL(aff12_xyz,mast_cmat) ;
           qmat = MAT44_MUL(targ_cmat_inv,wmat) ;  /* index transform matrix */
           UNLOAD_MAT44_AR(qmat,ap) ;
#if 0
DUMP_MAT44("aff12_ijk",qmat) ;
#endif
           im_targ = mri_genalign_scalar_warpone(
                                 12 , ap , mri_genalign_mat44 ,
                                 aim , nxout,nyout,nzout, final_interp ) ;
         }
         break ;
       }

       /* 04 Apr 2007: save matrix into dataset header */

       { static mat44 gam , gami ; char anam[64] ; float matar[12] ;

         if( matsave != NULL )
           gam = matsave[kk] ;
         else if( ISVALID_MAT44(aff12_xyz) )
           gam = aff12_xyz ;
         else
           mri_genalign_affine_get_gammaxyz( &gam ) ;  /* should not happen */

         if( ISVALID_MAT44(gam) ){
           sprintf(anam,"ALLINEATE_MATVEC_B2S_%06d",kk) ;
           UNLOAD_MAT44_AR(gam,matar) ;
           THD_set_float_atr( dset_out->dblk , anam , 12 , matar ) ;
           gami = MAT44_INV(gam) ;
           sprintf(anam,"ALLINEATE_MATVEC_S2B_%06d",kk) ;
           UNLOAD_MAT44_AR(gami,matar) ;
           THD_set_float_atr( dset_out->dblk , anam , 12 , matar ) ;
         }
       }

       /* save sub-brick without scaling factor */

       if( floatize || targ_kind == MRI_float ){
         EDIT_substitute_brick( dset_out,kk,MRI_float, MRI_FLOAT_PTR(im_targ) );
         mri_clear_data_pointer(im_targ) ;  /* data in im_targ saved directly */
       } else {
         EDIT_substscale_brick( dset_out,kk,MRI_float, MRI_FLOAT_PTR(im_targ),
                                targ_kind ,
                                (bfac == 0.0f) ? 1.0f : 0.0f ) ;
       }
       mri_free(im_targ) ; im_targ = NULL ;

       if( usetemp && DSET_NVALS(dset_out) > 1 )   /* 31 Jan 2007 */
         mri_purge( DSET_BRICK(dset_out,kk) ) ;
     }

     MEMORY_CHECK("end of sub-brick alignment") ;

   } /***------------- end of loop over target sub-bricks ------------------***/

   /*--- unload stuff we no longer need ---*/

   DSET_unload(dset_targ) ;
   mri_free(im_base) ; mri_free(im_weig) ; mri_free(im_mask) ;

   MRI_FREE(stup.bsim); MRI_FREE(stup.bsims);
   MRI_FREE(stup.ajim); MRI_FREE(stup.ajims); MRI_FREE(stup.bwght);
   MRI_FREE(stup.ajimor);

   /***--- write output dataset to disk? ---***/

   MEMORY_CHECK("end of sub-brick loop (after cleanup)") ;

   if( dset_out != NULL ){
     DSET_write(dset_out); WROTE_DSET(dset_out); DSET_unload(dset_out);
     MEMORY_CHECK("after writing output dataset") ;
   }

   /*--- save parameters to a file, if desired ---*/

   if( param_save_1D != NULL && parsave != NULL ){
     FILE *fp ;
     fp = (strcmp(param_save_1D,"-") == 0) ? stdout
                                           : fopen(param_save_1D,"w") ;
     if( fp == NULL ) ERROR_exit("Can't open -1Dparam_save %s for output!?",param_save_1D);
     fprintf(fp,"# 3dAllineate parameters:\n") ;
     fprintf(fp,"#") ;
     for( jj=0 ; jj < stup.wfunc_numpar ; jj++ )
       fprintf(fp," %s",stup.wfunc_param[jj].name) ;
     fprintf(fp,"\n") ;
     for( kk=0 ; kk < DSET_NVALS(dset_targ) ; kk++ ){
       for( jj=0 ; jj < stup.wfunc_numpar ; jj++ )
         fprintf(fp," %.5f",parsave[kk][jj]) ;
       fprintf(fp,"\n") ;                           /* oops */
     }
     if( fp != stdout ){
       fclose(fp) ; if( verb ) INFO_message("Wrote -1Dparam_save %s",param_save_1D) ;
     }
   }

   /*--- save matrices to disk, if so ordered by the omniscient user ---*/

   if( matrix_save_1D != NULL && matsave != NULL ){
     FILE *fp ;
     float a11,a12,a13,a14,a21,a22,a23,a24,a31,a32,a33,a34 ;
     fp = (strcmp(matrix_save_1D,"-") == 0) ? stdout
                                            : fopen(matrix_save_1D,"w") ;
     if( fp == NULL ) ERROR_exit("Can't open -1Dmatrix_save %s for output!?",matrix_save_1D);
     fprintf(fp,"# 3dAllineate matrices (DICOM-to-DICOM, row-by-row):\n") ;
     for( kk=0 ; kk < DSET_NVALS(dset_targ) ; kk++ ){
       UNLOAD_MAT44(matsave[kk],a11,a12,a13,a14,a21,a22,a23,a24,a31,a32,a33,a34) ;
       fprintf(fp,
               " %13.6g %13.6g %13.6g %13.6g %13.6g %13.6g %13.6g %13.6g %13.6g %13.6g %13.6g %13.6g\n",
               a11,a12,a13,a14,a21,a22,a23,a24,a31,a32,a33,a34 ) ;
     }
     if( fp != stdout ){
       fclose(fp) ; if( verb ) INFO_message("Wrote -1Dmatrix_save %s",matrix_save_1D) ;
     }
   }

   /*---------- FREE AT LAST, FREE AT LAST ----------*/

   FREE_GA_setup(&stup) ;
   if( parsave != NULL ){
     for( kk=0 ; kk < DSET_NVALS(dset_targ) ; kk++ )
       if( parsave[kk] != NULL ) free((void *)parsave[kk]) ;
     free((void *)parsave) ;
   }
   if( matsave != NULL ) free((void *)matsave) ;

   if( verb )
     INFO_message("total CPU time = %.1f sec  Elapsed = %.1f\n",
                  COX_cpu_time() , COX_clock_time() ) ;
   MEMORY_CHECK("end of program (after final cleanup)") ;
   if( verb )
    INFO_message("###########################################################");

   exit(0) ;
}

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
   if( verb > 1 )
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
     if( verb > 1 ) ININFO_message("Weightize: user clip=%g #clipped=%d #left=%d",
                                   aclip,nclip,nleft) ;
   }

   /*-- squash super-large values down to reasonability --*/

   clip = 3.0f * THD_cliplevel(qim,0.5f) ;
   if( verb > 1 ) ININFO_message("Weightize: (unblurred) top clip=%g",clip) ;
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
   if( verb > 1 ) ININFO_message("Weightize: (blurred) bot clip=%g",clip) ;
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
     if( verb > 1 ) ININFO_message("Weightize: raising to %g power",apow) ;
     for( ii=0 ; ii < nxyz ; ii++ )
       if( wf[ii] > 0.0f ) wf[ii] = powf( wf[ii] , apow ) ;
   }

   /*-- binarize (acod==2)?  boxize (acod==3)? --*/

#undef  BPAD
#define BPAD 4
   if( acod == 2 || acod == 3 ){  /* binary weight: mask=2 or maskbox=3 */
     if( verb > 1 ) ININFO_message("Weightize: binarizing") ;
     for( ii=0 ; ii < nxyz ; ii++ ) if( wf[ii] != 0.0f ) wf[ii] = 1.0f ;
     if( ndil > 0 ){  /* 01 Mar 2007: dilation */
       byte *mmm = (byte *)malloc(sizeof(byte)*nxyz) ;
       if( verb > 1 ) ININFO_message("Weightize: dilating") ;
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
       if( verb > 1 ) ININFO_message("Weightize: box=%d..%d X %d..%d X %d..%d",
                                     xm,xp , ym,yp , zm,zp ) ;
       for( kk=zm ; kk <= zp ; kk++ )
        for( jj=ym ; jj <= yp ; jj++ )
         for( ii=xm ; ii <= xp ; ii++ ) WW(ii,jj,kk) = 1.0f ;
     }
   }

   return wim ;
}

/*---------------------------------------------------------------------------*/
/*! Return the L_infinity distance between two parameter vectors. */

float param_dist( GA_setup *stp , float *aa , float *bb )
{
   int jj ; float ap, bp, pdist, dmax ;

   if( stp == NULL || aa == NULL || bb == NULL ) return 1.0f ;

   dmax = 0.0f ;
   for( jj=0 ; jj < stp->wfunc_numpar ; jj++ ){
     if( !stp->wfunc_param[jj].fixed ){
       ap = aa[jj] ; bp = bb[jj] ;
       pdist = fabsf(ap-bp)
              / (stp->wfunc_param[jj].max - stp->wfunc_param[jj].min) ;
       if( pdist > dmax ) dmax = pdist ;
     }
   }
   return dmax ;
}

/*---------------------------------------------------------------------------*/
/*! Setup before and after index-to-coordinate matrices in the warp func.
    See the notes at the end of this file for the gruesome details.
-----------------------------------------------------------------------------*/

void AL_setup_warp_coords( int epi_targ , int epi_fe, int epi_pe, int epi_se,
                           int *nxyz_base, float *dxyz_base, mat44 base_cmat,
                           int *nxyz_targ, float *dxyz_targ, mat44 targ_cmat )
{
   mat44 cmat_before , imat_after , gmat,tmat,qmat ;
   float *dijk ; int *nijk ;

   if( epi_targ < 0 ){            /*---------- no EPI info given ----------*/

     /* [it] = inv[Ct] [S] [D] [U] [Cb]     [ib]
               ------- ----------- --------
               [after] [transform] [before]      */

     imat_after  = MAT44_INV(targ_cmat) ;  /* xyz -> ijk for target */
     cmat_before = base_cmat ;             /* ijk -> xyz for base */

   } else if( epi_targ == 1 ){  /*---------- target is EPI --------------*/

     dijk = dxyz_targ ; nijk = nxyz_targ ;

     /* -FPS kij should have           [ 0  0  dk -mk ]
                              [gmat] = [ di 0  0  -mi ]
                                       [ 0  dj 0  -mj ]
                                       [ 0  0  0   1  ]
        In this example, epi_fe=2, epi_pe=0, epi_se=1   */

     ZERO_MAT44(gmat) ;
     gmat.m[0][epi_fe] = dijk[epi_fe] ;
     gmat.m[1][epi_pe] = dijk[epi_pe] ;
     gmat.m[2][epi_se] = dijk[epi_se] ;
     gmat.m[0][3]      = -0.5f * dijk[epi_fe] * (nijk[epi_fe]-1) ;
     gmat.m[1][3]      = -0.5f * dijk[epi_pe] * (nijk[epi_pe]-1) ;
     gmat.m[2][3]      = -0.5f * dijk[epi_se] * (nijk[epi_se]-1) ;

     /* [it] = inv[Gt] [S] [D] [U] inv[Rt] [Cb] [ib]
               ------- ----------- ------------
               [after] [transform] [before]        where [Ct] = [Rt] [Gt] */

     imat_after  = MAT44_INV(gmat) ;
     tmat        = MAT44_INV(targ_cmat) ;       /* inv[Ct] */
     qmat        = MAT44_MUL(tmat,base_cmat) ;  /* inv[Ct] [Cb] */
     cmat_before = MAT44_MUL(gmat,qmat) ;       /* [G] inv[Ct] [Cb] */

   } else {                     /*---------- base is EPI ----------------*/

     dijk = dxyz_base ; nijk = nxyz_base ;

     ZERO_MAT44(gmat) ;
     gmat.m[0][epi_fe] = dijk[epi_fe] ;
     gmat.m[1][epi_pe] = dijk[epi_pe] ;
     gmat.m[2][epi_se] = dijk[epi_se] ;
     gmat.m[0][3]      = -0.5f * dijk[epi_fe] * (nijk[epi_fe]-1) ;
     gmat.m[1][3]      = -0.5f * dijk[epi_pe] * (nijk[epi_pe]-1) ;
     gmat.m[2][3]      = -0.5f * dijk[epi_se] * (nijk[epi_se]-1) ;

     /*  [it] = inv[Ct] [Rb] [U] [S] [D] [Gb]     [ib]
                ------------ ----------- --------
                [after]      [transform] [before]  where [Cb] = [Rb] [Gb] */

     cmat_before = gmat ;                       /* [Gb] */
     qmat        = MAT44_INV(gmat) ;            /* inv[Gb] */
     qmat        = MAT44_MUL(base_cmat,qmat) ;  /* [Cb] inv[Gb] = [Rb] */
     tmat        = MAT44_INV(targ_cmat) ;       /* inv[Ct] */
     imat_after  = MAT44_MUL(tmat,qmat) ;       /* inv[Ct] [Rb] */
   }

   /*-- actually let the warping function 'know' about these matrices --*/

   mri_genalign_affine_set_befafter( &cmat_before , &imat_after ) ;

   return ;
}

/*----------------------------------------------------------------------------*/
#if 0
#undef  MMM
#define MMM(i,j,k) mmm[(i)+(j)*nx+(k)*nxy]

int * mri_edgesize( MRI_IMAGE *im )  /* 13 Aug 2007 */
{
   byte *mmm ;
   int ii,jj,kk , nx,ny,nz , nxy ;
   static int eijk[6] ;

ENTRY("mri_edgesize") ;

   if( im == NULL ) RETURN( NULL );

   nx = im->nx ; ny = im->ny ; nz = im->nz ; nxy = nx*ny ;

   STATUS("automask-ing on the cheap") ;

   THD_automask_set_cheapo(1) ;
   mmm = mri_automask_image( im ) ;
   if( mmm == NULL ) RETURN( NULL );

   /* check i-direction */

   STATUS("check i-direction") ;

   for( ii=0 ; ii < nx ; ii++ ){
     for( kk=0 ; kk < nz ; kk++ ){
       for( jj=0 ; jj < ny ; jj++ ) if( MMM(ii,jj,kk) ) goto I1 ;
   }}
 I1: eijk[0] = ii ;
   for( ii=nx-1 ; ii >= 0 ; ii-- ){
     for( kk=0 ; kk < nz ; kk++ ){
       for( jj=0 ; jj < ny ; jj++ ) if( MMM(ii,jj,kk) ) goto I2 ;
   }}
 I2: eijk[1] = nx-1-ii ;

   /* check j-direction */

   STATUS("check j-direction") ;

   for( jj=0 ; jj < ny ; jj++ ){
     for( kk=0 ; kk < nz ; kk++ ){
       for( ii=0 ; ii < nx ; ii++ ) if( MMM(ii,jj,kk) ) goto J1 ;
   }}
 J1: eijk[2] = jj ;
     for( jj=ny-1 ; jj >= 0 ; jj-- ){
       for( kk=0 ; kk < nz ; kk++ ){
         for( ii=0 ; ii < nx ; ii++ ) if( MMM(ii,jj,kk) ) goto J2 ;
     }}
 J2: eijk[3] = ny-1-jj ;

   /* check k-direction */

   STATUS("check k-direction") ;

   for( kk=0 ; kk < nz ; kk++ ){
     for( jj=0 ; jj < ny ; jj++ ){
       for( ii=0 ; ii < nx ; ii++ ) if( MMM(ii,jj,kk) ) goto K1 ;
   }}
 K1: eijk[4] = kk ;
   for( kk=nz-1 ; kk >= 0 ; kk-- ){
     for( jj=0 ; jj < ny ; jj++ ){
       for( ii=0 ; ii < nx ; ii++ ) if( MMM(ii,jj,kk) ) goto K2 ;
   }}
 K2: eijk[5] = nz-1-kk ;

   free(mmm) ; RETURN( eijk );
}
#endif

/******************************************************************************
*******************************************************************************

       ==============================================================
     ===== Notes on Coordinates and Indexes - RWCox - 05 Oct 2006 =====
       ==============================================================

The base and target datasets each have their own coordinate systems and
indexes.  We use 4x4 matrices to represent affine transformations, and
4-vectors to represent coordinates and indexes.  (The last row of a 4x4
matrix is [0 0 0 1] and the last element of a 4-vector is always 1.)
The index-to-coordinate transformations for base and target are given by

  [xb] = [Cb] [ib]
  [xt] = [Ct] [it]

where [Cb] and [Ct] are the dset->daxes->ijk_to_dicom matrices in the
datasets' header.

The 4x4 affine transformation matrix is not directly parametrized by its 12
non-trivial elements.  To give control over and meaning to the parameters,
the matrix is instead modeled as

  [T] = [S] [D] [U]

where [S] is a shear matrix, [D] is a diagonal scaling matrix, and [U] is
a proper orthogonal matrix.  If we wish to restrict the transformation [T]
to rigid body movement, for example, then we can fix the [S] and [D]
matrices to be the identity.

N.B.: The shift matrix [H] can be inserted before or after the [S][D][U]
product, as desired, so we'd really have [H][S][D][U] for dcode==DELTA_AFTER
and [S][D][U][H] for dcode==DELTA_BEFORE.  [H] must be a matrix of the form
   [ 1 0 0 a ]
   [ 0 1 0 b ]
   [ 0 0 1 c ]
   [ 0 0 0 1 ]
where {a,b,c} are the shifts.  I will ignore [H] in what follows.  Also,
the order [S][D][U] can be altered by the user, which I'll pretty much
ignore below, as well.

For EPI data, we may want to restrict the transformation parameters so as
to treat the phase-encoding direction differently than the frequency- and
slice-encoding directions.  However, the matrices as described above mean
that the [T] matrix components apply to DICOM coordinates, which may not
be aligned with the FPS directions in the image.  In such a case, putting
restrictions on the [T] parameters will not translate in a simple way into
FPS coordinates.

The solution is to break the transformation from indexes to spatial
coordinates into two pieces.  Let [C] = [R] [G], where [C] is an index-to
DICOM space matrix, [G] is a matrix that transforms indexes to FPS coordinates,
and [R] is "what's left" (should be a rotation matrix, possibly with
det[R]=-1).  A sample [G] is

        [ 0  0  dk -mk ]
  [G] = [ di 0  0  -mi ]
        [ 0  dj 0  -mj ]
        [ 0  0  0   1  ]

where the dataset is stored with '-FPS kij':
  i=P (phase-encoding), j=S (slice-encoding), and k=F (frequency-encoding);
  d{i,j,k} is the grid spacing along the respective dimensions; and
  m{i,j,k} is the coordinate at the volume center: e.g., mi=0.5*di*(ni-1).
(Here, 'i' refers to the first index in the dataset, etc.)

If we break up [Ct] this way, then the transformation is

  [xt] = [S] [D] [U] [xb], or
  [xb] = inv[U] inv[D] inv[S] [xt]
       = inv[U] inv[D] inv[S] [Rt] [Gt] [it]

and inv[T] is applied to the DICOM ordered coordinates in [Rt] [Gt] [it].
If we want to apply inv[T] to the FPS ordered coordinates, then we change
the last equation above to

  [xb] = [Rt] inv[U] inv[D] inv[S] [Gt] [it]

where inv[T] is now applied to coordinates where xyz=FPS.  Now, restricting
the parameters in [T] applies directly to FPS coordinates (e.g., fix the
scaling factor in [D] along the z-axis to 1, and then there will be no
stretching/shrinking of the data along the slice-encoding direction).  The
final multiplication by [Rt] rotates the inv[T]-transformed FPS coordinates
to DICOM order.

So, if the target dataset is the EPI dataset, then the transformation from
[ib] to [it] is expressed at

  [it] = inv[Gt] [S] [D] [U] inv[Rt] [Cb] [ib]
         ------- ----------- ------------
         [after] [transform] [before]

where the [transform] matrix is what the parameter searching is all about,
and the [before] and [after] matrices are fixed.

If the base dataset is the EPI dataset, on the other hand, then the index-
to-index transformation (what is really needed, after all) is

  [it] = inv[Ct] [Rb] [U] [D] [S] [Gb]     [ib]
         ------------ ----------- --------
         [after]      [transform] [before]

(N.B.: The SDU order has been inverted to UDS, on the presumption that
we will control the scaling and shear in the FPS coordinate system given
by [Gb][ib].) In the 'normal' case, where either (a) we are going to allow
full transform generality, or (b) no particular distortions of the image are
to be specially allowed for, then we simply have

  [it] = inv[Ct] [S] [D] [U] [Cb]     [ib]
         ------- ----------- --------
         [after] [transform] [before]

All of these cases are possible.  They will be especially important if/when
nonlinear warping is allowed in place of the simple [S][D][U] transformation,
the user needs to restrict the warping to occur only in the P-direction, and
the data slices are oblique.

*******************************************************************************
*******************************************************************************/
