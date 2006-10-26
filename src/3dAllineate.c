/**** TO DO (someday, maybe):
        -matini
         -dxyz
         center of mass: use to set center of range?
****/

/****** N.B.: What used to be 'target' is now 'source' to users,
              but remains as 'target' in the code and comments.  ******/

/****** N.B.: See the note about coordinates at the end of this file! ******/

/*----------------------------------------------------------------------------*/
#include "mrilib.h"
#include <time.h>
#include <sys/types.h>
#include <unistd.h>

#define MAXPAR   199
#define PARC_FIX 1
#define PARC_INI 2
#define PARC_RAN 3
typedef struct { int np,code; float vb,vt ; } param_opt ;

#define WARP_SHIFT    1
#define WARP_ROTATE   2
#define WARP_SCALE    3
#define WARP_AFFINE   4

static float wt_medsmooth = 2.25f ;   /* for mri_weightize() */
static float wt_gausmooth = 4.50f ;

static int verb           = 1 ;       /* somewhat on by default */

MRI_IMAGE * mri_weightize( MRI_IMAGE *, int ) ;  /* prototype */

void AL_setup_warp_coords( int,int,int,int ,
                           int *, float *, mat44,
                           int *, float *, mat44 ) ; /* prototype */

/*----------------------------------------------------------------------------*/
#undef  NMETH
#define NMETH GA_MATCH_METHNUM_SCALAR  /* cf. mrilib.h */

static int meth_visible[NMETH] =       /* 1 = show in -help; 0 = don't show */
  { 1 , 0 , 1 , 1 , 1 , 0 , 1 , 1 , 1 } ;

static int meth_noweight[NMETH] =      /* 1 = don't allow weights, just masks */
  { 0 , 1 , 0 , 0 , 1 , 1 , 1 , 0 , 0 } ;

static char *meth_shortname[NMETH] =   /* short names for terse cryptic users */
  { "ls" , "sp" , "mi" , "crM" , "nmi" , "je" , "hel" , "crA" , "crU" } ;

static char *meth_longname[NMETH] =    /* long names for prolix users */
  { "leastsq"         , "spearman"     ,
    "mutualinfo"      , "corratio_mul" ,
    "norm_mutualinfo" , "jointentropy" ,
    "hellinger"       ,
    "corratio_add"    , "corratio_uns"  } ;

static char *meth_username[NMETH] =    /* descriptive names */
  { "Least Squares [Pearson Correlation]"   ,
    "Spearman [rank] Correlation"           ,
    "Mutual Information [H(b)+H(t)-H(b,t)]" ,
    "Correlation Ratio (Sym *)"             ,
    "Normalized MI [H(b,t)/(H(b)+H(t))]"    ,
    "Joint Entropy [H(b,t)]"                ,
    "Hellinger metric"                      ,
    "Correlation Ratio (Sym +)"             ,
    "Correlation Ratio (Unsym)"              } ;
/*---------------------------------------------------------------------------*/

int main( int argc , char *argv[] )
{
   THD_3dim_dataset *dset_out=NULL ;
   MRI_IMAGE *im_base, *im_targ, *im_weig=NULL, *im_mask=NULL, *qim ;
   MRI_IMAGE *im_bset, *im_wset ;
   GA_setup stup ;
   int iarg , ii,jj,kk , nmask=0 , nfunc , rr ;
   int   nx_base,ny_base,nz_base , nx_targ,ny_targ,nz_targ , nxy_base ;
   float dx_base,dy_base,dz_base , dx_targ,dy_targ,dz_targ ;
   int   nxyz_base[3] , nxyz_targ[3] , nxyz_dout[3] ;
   float dxyz_base[3] , dxyz_targ[3] , dxyz_dout[3] ;
   int nvox_base ;
   float v1,v2 , xxx,yyy,zzz,siz ;
   int pad_xm=0,pad_xp=0 , pad_ym=0,pad_yp=0 , pad_zm=0,pad_zp=0 ;
   int tfdone=0;  /* stuff for -twofirst */
   float tfparm[PARAM_MAXTRIAL+1][MAXPAR];
   int skip_first=0 , didtwo , targ_kind ;
   double ctim , rad , conv_rad ;
   float **parsave=NULL ;
   MRI_IMAGE *apply_im = NULL ;
   float *apply_far    = NULL ;
   int apply_nx, apply_ny , nparam_free , diffblur=1 ;
   float cost, cost_ini ;
   mat44 cmat_bout , cmat_tout ;
   int   nxout,nyout,nzout ;
   float dxout,dyout,dzout ;

   /*----- input parameters, to be filled in from the options -----*/

   THD_3dim_dataset *dset_base = NULL ;
   THD_3dim_dataset *dset_targ = NULL ;
   THD_3dim_dataset *dset_mast = NULL ;
   THD_3dim_dataset *dset_weig = NULL ;
   int auto_weight             = 3 ;            /* -autobbox == default */
   char *auto_string           = "-autobox" ;
   float dxyz_mast             = 0.0f ;         /* not implemented */
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
   char *fname_1D              = NULL ;         /* off by default */
   char *apply_1D              = NULL ;         /* off by default */
   int interp_code             = MRI_LINEAR ;
   int npt_match               = -47 ;          /* 47%, that is */
   int final_interp            = MRI_CUBIC ;
   int warp_code               = WARP_AFFINE ;
   int warp_freeze             = 0 ;            /* off by default */
   int nparopt                 = 0 ;
   MRI_IMAGE *matini           = NULL ;
   int tbest                   = 4 ;            /* default=try best 4 */
   param_opt paropt[MAXPAR] ;
   float powell_mm             = 0.0f ;
   float powell_aa             = 0.0f ;
   float conv_mm               = 0.05 ;         /* millimeters */
   int matorder                = MATORDER_SDU ; /* matrix mult order */
   int smat                    = SMAT_LOWER ;   /* shear matrix triangle */
   int dcode                   = DELTA_AFTER ;  /* shift after */
   int meth_check              = -1 ;           /* don't do it */
   char *save_hist             = NULL ;         /* don't save it */
   long seed                   = 7654321 ;      /* random? */
   int XYZ_warp                = 0 ;            /* off by default */
   double hist_pow             = 0.0 ;
   int hist_nbin               = 0 ;
   int epi_fe                  = -1 ;            /* off by default */
   int epi_pe                  = -1 ;
   int epi_se                  = -1 ;
   int epi_targ                = -1 ;
   int replace_base            = 0 ;             /* off by default */
   int replace_meth            = 0 ;             /* off by default */

   /**----------------------------------------------------------------------*/
   /**----------------- Help the pitifully ignorant user? -----------------**/

   if( argc < 2 || strcmp(argv[1],"-help")==0 ||
                   strcmp(argv[1],"-HELP")==0 || strcmp(argv[1],"-POMOC")==0 ){
     printf(
       "Usage: 3dAllineate [options] sourcedataset\n"
       "\n"
       "Program to align one dataset (the 'source') to a base dataset.\n"
       "Options are available to control:\n"
       " + How the matching between the source and the base is computed.\n"
       " + How the resliced source is interpolated to the base space.\n"
       " + The complexity of the spatial transformation ('warp') used.\n"
       " + And many technical options to control the process in detail,\n"
       "    if you know what you are doing.\n"
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
       "               (Target must be stored as floats, shorts, or bytes.)\n"
       "\n"
       " * NOTA BENE: The base and source dataset do NOT have to be defined *\n"
       " *            on the same grids; the alignment process uses the     *\n"
       " *            coordinate systems defined in the dataset headers to  *\n"
       " *            make the match between spatial locations.             *\n"
       "\n"
       " -prefix ppp = Output the resulting dataset to file 'ppp'.  If this\n"
       "   *OR*        option is NOT given, no dataset will be output!  The\n"
       " -out ppp      transformation to align the source to the base will\n"
       "               be estimated, but not applied.  (By default, the new\n"
       "               dataset is computed on the grid of the base dataset,\n"
       "               and it will be stored in float format.)\n"
       "\n"
       " -floatize   = Write result dataset as floats.  Internal calculations\n"
       "               are all done on float copies of the input datasets.\n"
       "               [Default=convert output dataset to data format of]\n"
       "               [        source dataset, without any scale factor]\n"
       "\n"
       " -1Dfile fff = Save the warp parameters in ASCII (.1D) format into\n"
       "               file 'fff' (1 row per sub-brick in source).\n"
       "\n"
       " -1Dapply aa = Read warp parameters from file 'aa', apply them to the\n"
       "               source dataset, and produce a new dataset.\n"
       "               (Must also use the '-prefix' option for this to work! )\n"
       "               (In this mode of operation, there is no optimization  )\n"
       "               (of the cost function by changing the warp parameters;)\n"
       "               (previously computed parameters are applied directly. )\n"
       "\n"
       " -cost ccc   = Defines the 'cost' function that defines the matching\n"
       "               between the source and the base; 'ccc' is one of\n"
      ) ;

      for( ii=0 ; ii < NMETH ; ii++ )
        if( meth_visible[ii] )
          printf( "                %-4s *OR*  %-16s= %s\n" ,
                  meth_shortname[ii] , meth_longname[ii] , meth_username[ii] ) ;

      printf(
       "               You can also specify the cost function using an option\n"
       "               of the form '-mi' rather than '-cost mi', if you like\n"
       "               to keep things terse and cryptic (as I do).\n"
       "               [Default == '-hel'.]\n"
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
       "               to produce the output dataset.  [Default == 'linear'.]\n"
       "\n"
       " -final iii  = Defines the interpolation mode used to create the\n"
       "               output dataset.  [Default == 'cubic']\n"
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
       "               [Default == 0.05 mm]\n"
       "\n"
       " -verb       = Print out verbose progress reports.\n"
       "               [Using '-VERB' will give even more prolix reports.]\n"
       " -quiet      = Don't print out verbose stuff.\n"
       "\n"
       " -check kkk  = After cost function optimization is done, start at the\n"
       "               final parameters and RE-optimize using the new cost\n"
       "               function 'kkk'.  If the results are too different, a\n"
       "               warning message will be printed.  In any case, the\n"
       "               final parameters from the original optimization will be\n"
       "               used to create the output dataset. Using -check\n"
       "               increases the CPU time, but can help you feel sure\n"
       "               that the alignment process did not go wild and crazy.\n"
       "               [Default == no check == don't worry, be happy!]\n"
       "\n"
       " ** PARAMETERS THAT AFFECT THE COST OPTIMIZATION STRATEGY **\n"
       " -onepass    = Use only the refining pass -- do not try a coarse\n"
       "               resolution pass first.  Useful if you know that only\n"
       "               small amounts of image alignment are needed.\n"
       "               [The default is to use both passes.]\n"
       " -twopass    = Use a two pass alignment strategy, first searching\n"
       "               for a large rotation+shift and then refining the\n"
       "               alignment.  [Two passes are used by default.]\n"
       " -twoblur rr = Set the blurring radius for the first pass to 'rr'\n"
       "               millimeters.  [Default == 11 mm]\n"
       " -twofirst   = Use -twopass on the first image to be registered, and\n"
       "               then on all subsequent images, use the results from\n"
       "               the first image's coarse pass to start the fine pass.\n"
       "               (Useful when there may be large motions between the   )\n"
       "               (source and the base, but only small motions within   )\n"
       "               (the source dataset itself; since the coarse pass can )\n"
       "               (be slow, doing it only once makes sense in this case.)\n"
       "               [-twofirst is on by default; '-twopass' turns it off.]\n"
       " -twobest bb = In the coarse pass, use the best 'bb' set of initial\n"
       "               points to search for the starting point for the fine\n"
       "               pass.  If bb==0, then no search is made for the best\n"
       "               starting point, and the identity transformation is\n"
       "               used as the starting point.  [Default=4; min=0 max=7]\n"
       " -fineblur x = Set the blurring radius to use in the fine resolution\n"
       "               pass to 'x' mm.  [Default == 0 mm]\n"
       "   **NOTES ON\n"
       "   **STRATEGY: * If you expect only small-ish (< 5 mm?) image movements,\n"
       "                 then using '-onepass' or '-twobest 0' makes sense.\n"
       "               * If you expect large-ish image movements, then do not\n"
       "                 use '-onepass' or '-twobest 0'; the purpose of the\n"
       "                 '-twobest' parameter is to search for large initial\n"
       "                 rotations/shifts with which to start the coarse\n"
       "                 optimization round.\n"
       "               * If you have multiple sub-bricks in the source dataset,\n"
       "                 then '-twofirst' makes sense if you don't expect large\n"
       "                 movements WITHIN the source, but expect large movements\n"
       "                 between the source and base.\n"
       "               * '-fineblur' is experimental, and if you use it, the\n"
       "                 value should probably be small (1 mm?).\n"
       "\n"
       " -autoweight = Compute a weight function using the 3dAutomask\n"
       "               algorithm plus some blurring of the base image.\n"
       "       **N.B.: Some cost functions do not allow -autoweight, and\n"
       "               will use -automask instead.  A warning message\n"
       "               will be printed if you run into this situation.\n"
       " -automask   = Compute a mask function, which is like -autoweight,\n"
       "               but the weight for a voxel is either 0 or 1.\n"
       " -autobox    = Expand the -automask function to enclose a rectangular\n"
       "               box that holds the irregular mask.\n"
       "               [This is the default mode of operation.]\n"
       " -nomask     = Don't compute the autoweight/mask; if -weight is not\n"
       "               used, then every voxel will be counted equally.\n"
       " -weight www = Set the weighting for each voxel in the base dataset;\n"
       "               larger weights mean that voxel counts more in the cost\n"
       "               function.\n"
       "       **N.B.: The weight dataset must be defined on the same grid as\n"
       "               the base dataset.\n"
       " -wtprefix p = Write the weight volume to disk as a dataset with\n"
       "               prefix name 'p'.  Used with '-autoweight/mask', this option\n"
       "               lets you see what voxels were important in the allineation.\n"
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
       "                 to '-parfix 4 -dd dd -parang 5 -dd dd -parang 6 -dd dd'\n"
       "                 [Default=30 degrees]\n"
       " -maxshf dd    = Allow maximum shift of 'dd' millimeters.  Equivalent\n"
       "                 to '-parang 1 -dd dd -parang 2 -dd dd -paran2 6 -dd dd'\n"
       "                 [Default=33%% of the size of the base image]\n"
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
       "               reference system of the base image.\n"
#if 0
       " -dxyz del   = Write the output dataset using grid spacings of\n"
       "               'del' mm.  If this option is NOT given, then the\n"
       "               grid spacings in the master dataset will be used.\n"
#endif
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
      "NOTE WELL: If you use '-EPI', then the output warp parameters apply\n"
      "           to the (freq,phase,slice) xyz coordinates, NOT to the DICOM\n"
      "           xyz coordinates, so equivalent transformations will be\n"
      "           expressed with different sets of parameters entirely\n"
      "           than if you don't use '-EPI'!\n"
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
      "================================================\n"
      "  RWCox - September 2006 - Live Long and Prosper\n"
      "================================================\n"
      "** From Webster's Dictionary: Allineate == 'to align' **\n"
     ) ;

     if( argc > 1 &&
        (strcmp(argv[1],"-HELP")==0 || strcmp(argv[1],"-POMOC")==0) ){
       printf(
        "\n"
        "===========================================\n"
        "TOP SECRET HIDDEN OPTIONS (-HELP or -POMOC)\n"
        "===========================================\n"
        " -savehist sss = Save start and final 2D histograms as PGM\n"
        "                 files, with prefix 'sss' (cost: cr mi nmi hel).\n"
        " -seed iii     = Set random number seed (for coarse startup search)\n"
        "                 to 'iii'.\n"
        "                 [Default==7654321; if iii==0, a unique value is used]\n"
        " -median       = Smooth with median filter instead of Gaussian blur.\n"
        " -powell m a   = Set the NEWUOA dimensional parameters to\n"
        "                 'm' and 'a' (cf. powell_int.c).\n"
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
        " -wtmrad  mm   = Set autoweight/mask median filter radius to 'mm' voxels.\n"
        " -wtgrad  gg   = Set autoweight/mask Gaussian filter radius to 'gg' voxels.\n"
       ) ;
     } else {
       printf("\n"
              "[[[[[ To see a few super-advanced options, use '-HELP'. ]]]]]\n") ;
     }

     printf("\n"); exit(0);
   }

   /**--- bookkeeping and marketing ---**/

#ifdef USING_MCW_MALLOC
   enable_mcw_malloc() ;
#endif

   mainENTRY("3dAllineate"); machdep();
   AFNI_logger("3dAllineate",argc,argv);
   PRINT_VERSION("3dAllineate"); AUTHOR("Emperor Zhark");
   THD_check_AFNI_version("3dAllineate");

   /**--- process command line options ---**/

   iarg = 1 ;
   while( iarg < argc && argv[iarg][0] == '-' ){

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

     if( strcmp(argv[iarg],"-dxyz") == 0 ){
       if( ++iarg >= argc ) ERROR_exit("no argument after '%s'!",argv[iarg-1]) ;
       dxyz_mast = (float)strtod(argv[iarg],NULL) ;
       if( dxyz_mast <= 0.0f )
         ERROR_exit("Illegal value '%s' after -dxyz",argv[iarg]) ;
       iarg++ ; continue ;
     }

     /*-----*/

     if( strncmp(argv[iarg],"-master",6) == 0 ){
       if( dset_mast != NULL ) ERROR_exit("Can't have multiple %s options!",argv[iarg]) ;
       if( ++iarg >= argc ) ERROR_exit("no argument after '%s'!",argv[iarg-1]) ;
       dset_mast = THD_open_dataset( argv[iarg] ) ;
       if( dset_mast == NULL ) ERROR_exit("can't open -master dataset '%s'",argv[iarg]);
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

     if( strncmp(argv[iarg],"-weight",6) == 0 ){
       auto_weight = 0 ;
       if( dset_weig != NULL ) ERROR_exit("Can't have multiple %s options!",argv[iarg]) ;
       if( ++iarg >= argc ) ERROR_exit("no argument after '%s'!",argv[iarg-1]) ;
       dset_weig = THD_open_dataset( argv[iarg] ) ;
       if( dset_weig == NULL ) ERROR_exit("can't open -weight dataset '%s'",argv[iarg]);
       iarg++ ; continue ;
     }

     /*-----*/

     if( strncmp(argv[iarg],"-autoweight",8) == 0 ){
       if( dset_weig != NULL ) ERROR_exit("Can't use -autoweight AND -weight!") ;
       auto_weight = 1 ; auto_string = "-autoweight" ; iarg++ ; continue ;
     }

     if( strncmp(argv[iarg],"-automask",8) == 0 ){
       if( dset_weig != NULL ) ERROR_exit("Can't use -automask AND -weight!") ;
       auto_weight = 2 ; auto_string = "-automask" ; iarg++ ; continue ;
     }

     if( strncmp(argv[iarg],"-noauto",6) == 0 ||
         strncmp(argv[iarg],"-nomask",6) == 0   ){
       auto_weight = 0 ; iarg++ ; continue ;
     }

     if( strcmp(argv[iarg],"-autobox") == 0 ){
       auto_weight = 3 ; auto_string = "-autobox" ; iarg++ ; continue ;
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
       set_2Dhist_hbin( hist_nbin ) ;
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
       if( strcmp(argv[iarg]+1,meth_shortname[ii]) == 0 ){
         meth_code = jj = ii+1 ; break ;
       }
     }
     if( jj > 0 ){ iarg++ ; continue ; }

     /** -longname **/

     for( jj=ii=0 ; ii < NMETH ; ii++ ){
       if( strncmp(argv[iarg]+1,meth_longname[ii],7) == 0 ){
         meth_code = jj = ii+1 ; break ;
       }
     }
     if( jj > 0 ){ iarg++ ; continue ; }

     /** -cost shortname  *OR*  -cost longname **/

     if( strcmp(argv[iarg],"-cost") == 0 || strcmp(argv[iarg],"-meth") == 0 ){
       if( ++iarg >= argc ) ERROR_exit("no argument after '-cost'!") ;

       for( jj=ii=0 ; ii < NMETH ; ii++ ){
         if( strcmp(argv[iarg],meth_shortname[ii]) == 0 ){
           meth_code = jj = ii+1 ; break ;
         }
       }
       if( jj > 0 ){ iarg++ ; continue ; }

       for( jj=ii=0 ; ii < NMETH ; ii++ ){
         if( strncmp(argv[iarg],meth_longname[ii],7) == 0 ){
           meth_code = jj = ii+1 ; break ;
         }
       }
       if( jj >=0 ){ iarg++ ; continue ; }

       ERROR_exit("Unknown code '%s' after -cost!",argv[iarg]) ;
     }

     /*----- -check costname -----*/

     if( strncmp(argv[iarg],"-check",5) == 0 ){
       if( ++iarg >= argc ) ERROR_exit("no argument after '-check'!") ;

       for( jj=ii=0 ; ii < NMETH ; ii++ ){
         if( strcmp(argv[iarg],meth_shortname[ii]) == 0 ){
           meth_check = jj = ii+1 ; break ;
         }
       }
       if( jj > 0 ){ iarg++ ; continue ; }

       for( jj=ii=0 ; ii < NMETH ; ii++ ){
         if( strncmp(argv[iarg],meth_longname[ii],7) == 0 ){
           meth_check = jj = ii+1 ; break ;
         }
       }
       if( jj >=0 ){ iarg++ ; continue ; }

       WARNING_message("Unknown code '%s' after -check!",argv[iarg]) ;
       meth_check = -1 ; iarg++ ; continue ;
     }

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
       sm_rad = (float)strtod(argv[iarg],NULL) ; twopass = 1 ; iarg++ ; continue ;
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
       prefix = argv[iarg] ; iarg++ ; continue ;
     }

     /*-----*/

     if( strncmp(argv[iarg],"-1Dfile",5) == 0 ){
       if( fname_1D != NULL ) ERROR_exit("Can't have multiple %s options!",argv[iarg]);
       if( ++iarg >= argc ) ERROR_exit("no argument after '%s'!",argv[iarg-1]) ;
       if( !THD_filename_ok(argv[iarg]) )
         ERROR_exit("badly formed filename: %s '%s'",argv[iarg-1],argv[iarg]) ;
       fname_1D = argv[iarg] ; iarg++ ; continue ;
     }

     /*-----*/

     if( strncmp(argv[iarg],"-1Dapply",5) == 0 ){
       if( apply_1D != NULL ) ERROR_exit("Can't have multiple %s options!",argv[iarg]);
       if( ++iarg >= argc ) ERROR_exit("no argument after '%s'!",argv[iarg-1]) ;
       if( !THD_filename_ok(argv[iarg]) )
         ERROR_exit("badly formed filename: %s '%s'",argv[iarg-1],argv[iarg]) ;
       apply_1D = argv[iarg] ; qim = mri_read_1D(apply_1D) ;
       if( qim == NULL ) ERROR_exit("Can't read -1Dapply '%s'",apply_1D) ;
       apply_im  = mri_transpose(qim); mri_free(qim);
       apply_far = MRI_FLOAT_PTR(apply_im) ;
       apply_nx  = apply_im->nx ;  /* # of values per row */
       apply_ny  = apply_im->ny ;  /* number of rows */
       iarg++ ; continue ;
     }
#undef  APL
#define APL(i,j) apply_far[(i)+(j)*apply_nx] /* i=param index, j=row index */

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
       float vv , vvi ;
       if( ++iarg >= argc ) ERROR_exit("no argument after '%s'!",argv[iarg-1]) ;
       if( nparopt+2 >= MAXPAR ) ERROR_exit("too many -par... options!") ;
       vv = (float)strtod(argv[iarg],NULL) ;
       if( vv == 1.0f || vv > 2.0f || vv < 0.5f )
         ERROR_exit("-maxscl %f is illegal!",vv) ;
       if( vv > 1.0f ){ vvi = 1.0f/vvi; }
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

     /*-----*/

     if( strcmp(argv[iarg],"-replacemeth") == 0 ){  /* 18 Oct 2006 */
       if( ++iarg >= argc ) ERROR_exit("no argument after '-replacemeth'!") ;

       if( strcmp(argv[iarg],"0") == 0 ){
         replace_meth = 0 ; iarg++ ; continue ;  /* special case */
       }

       for( jj=ii=0 ; ii < NMETH ; ii++ ){
         if( strcmp(argv[iarg],meth_shortname[ii]) == 0 ){
           replace_meth = jj = ii+1 ; break ;
         }
       }
       if( jj > 0 ){ iarg++ ; continue ; }

       for( jj=ii=0 ; ii < NMETH ; ii++ ){
         if( strncmp(argv[iarg],meth_longname[ii],7) == 0 ){
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

     ERROR_exit("Unknown and Illegal option '%s'",argv[iarg]) ;
   }

   /*---------------------------------------------------------------*/
   /*--- check inputs for validity, consistency, and moral fibre ---*/

   if( seed == 0 ) seed = (long)time(NULL)+(long)getpid() ;
   srand48(seed) ;

   if( meth_check == meth_code ){
     WARNING_message("-check and -cost are the same?!") ;
     meth_check = -1 ;
   }

   if( warp_freeze ) twofirst = 1 ;  /* 10 Oct 2006 */

   /* open target from last argument, if not already open */

   if( dset_targ == NULL ){
     if( iarg >= argc )
       ERROR_exit("no source datset on command line!?") ;
     dset_targ = THD_open_dataset( argv[iarg] ) ;
     if( dset_targ == NULL )
       ERROR_exit("Can't open source dataset '%s'",argv[iarg]) ;
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
     if( prefix == NULL ) ERROR_exit("-1Dapply also needs -prefix!") ;
     wtprefix = fname_1D = NULL ; zeropad = 0 ; auto_weight = 0 ;
     if( dset_weig != NULL ){ DSET_delete(dset_weig); dset_weig=NULL; }
   }

   /* if no base input, target should have more than 1 sub-brick */

   if( dset_base == NULL && apply_1D == NULL ){
     if( DSET_NVALS(dset_targ) == 1 )
       ERROR_exit("No base dataset AND source dataset has only 1 sub-brick") ;

     WARNING_message("No -base dataset: using sub-brick #0 of source") ;
     skip_first = 1 ;  /* don't register sub-brick #0 of targ */
   }

   if( final_interp < 0 ) final_interp = interp_code ;  /* default */

   /*--- load input datasets ---*/

   if( verb ) INFO_message("Loading datasets") ;

   /* target MUST be present */

   DSET_load(dset_targ) ;
   if( !DSET_LOADED(dset_targ) ) ERROR_exit("Can't load source dataset") ;
   nx_targ = DSET_NX(dset_targ) ; dx_targ = fabsf(DSET_DX(dset_targ)) ;
   ny_targ = DSET_NY(dset_targ) ; dy_targ = fabsf(DSET_DY(dset_targ)) ;
   nz_targ = DSET_NZ(dset_targ) ; dz_targ = fabsf(DSET_DZ(dset_targ)) ;

   nxyz_targ[0] = nx_targ; nxyz_targ[1] = ny_targ; nxyz_targ[2] = nz_targ;
   dxyz_targ[0] = dx_targ; dxyz_targ[1] = dy_targ; dxyz_targ[2] = dz_targ;

   if( nx_targ < 2 || ny_targ < 2 )
     ERROR_exit("Target dataset has nx=%d ny=%d ???",nx_targ,ny_targ) ;

   /* load base if defined */

   if( dset_base != NULL ){
     DSET_load(dset_base) ;
     if( !DSET_LOADED(dset_base) ) ERROR_exit("Can't load base dataset") ;
     im_base = mri_scale_to_float( DSET_BRICK_FACTOR(dset_base,0) ,
                                   DSET_BRICK(dset_base,0)         ) ;
     DSET_unload(dset_base) ;
     dx_base = fabsf(DSET_DX(dset_base)) ;
     dy_base = fabsf(DSET_DY(dset_base)) ;
     dz_base = fabsf(DSET_DZ(dset_base)) ;
     if( im_base->nx < 2 || im_base->ny < 2 )
       ERROR_exit("Base dataset has nx=%d ny=%d ???",nx_base,ny_base) ;
   } else {
     im_base = mri_scale_to_float( DSET_BRICK_FACTOR(dset_targ,0) ,
                                   DSET_BRICK(dset_targ,0)         ) ;
     dx_base = dx_targ; dy_base = dy_targ; dz_base = dz_targ;
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

     if( verb ){
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

   /* load weight dataset if defined */

   if( dset_weig != NULL ){
     DSET_load(dset_weig) ;
     if( !DSET_LOADED(dset_weig) ) ERROR_exit("Can't load weight dataset") ;
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
       ERROR_exit("-weight and base volumes don't match!") ;

   } else if( auto_weight ){  /* manufacture weight from the base */
     if( meth_noweight[meth_code-1] && auto_weight == 1 ){
       WARNING_message("Cost function '%s' uses -automask NOT -autoweight",
                       meth_longname[meth_code-1] ) ;
       auto_weight = 2 ;
     } else if( verb >= 1 ){
       INFO_message("Computing %s",auto_string) ;
     }
     if( verb > 1 ) ctim = COX_cpu_time() ;
     im_weig = mri_weightize( im_base , auto_weight ) ;
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

   /* number of points to use for matching */

   if( npt_match < -100 ){                              /* default */
     npt_match = (int)(0.20*nmask) ;
     if( npt_match > 66666 ) npt_match = 66666 ;
   } else if( npt_match < 0 ){                          /* percentage */
     npt_match = (int)(-0.01*npt_match*nmask) ;
   }
   if( npt_match < 666 ) npt_match = 666 ;
   if( verb ) INFO_message("Number of points for matching = %d",npt_match) ;

   /*------ setup alignment structure parameters ------*/

   memset(&stup,0,sizeof(GA_setup)) ;

   stup.match_code = meth_code ;

   /* spatial coordinates: 'cmat' transforms from ijk to xyz */

   if( !ISVALID_MAT44(dset_targ->daxes->ijk_to_dicom) )
     THD_daxes_to_mat44(dset_targ->daxes) ;
   stup.targ_cmat = dset_targ->daxes->ijk_to_dicom ;

   /* base coordinates are drawn from it's header, or are same as target */

   if( dset_base != NULL ){
     if( !ISVALID_MAT44(dset_base->daxes->ijk_to_dicom) )
       THD_daxes_to_mat44(dset_base->daxes) ;
     stup.base_cmat = dset_base->daxes->ijk_to_dicom ;

     if( MAT44_DET(stup.base_cmat) * MAT44_DET(stup.targ_cmat) < 0.0f )
       WARNING_message("base and source datasets have different handedness!") ;
   } else {
     stup.base_cmat = stup.targ_cmat ;
   }

   /* modify base_cmat to allow for zeropad? */

   if( pad_xm > 0 || pad_ym > 0 || pad_zm > 0 )
     MAT44_EXTEND_IJK( stup.base_cmat , pad_xm,pad_ym,pad_zm ) ;

   /*---------- define warp 'before' and 'after' matrices ----------*/

   AL_setup_warp_coords( epi_targ,epi_fe,epi_pe,epi_se,
                         nxyz_base, dxyz_base, stup.base_cmat,
                         nxyz_targ, dxyz_targ, stup.targ_cmat ) ;

   /*---------- define warp parameters and function ----------*/

   mri_genalign_affine_setup( matorder , dcode , smat ) ;

   stup.wfunc       = mri_genalign_affine ;  /* warping function */
   stup.wfunc_param = (GA_param *)calloc(12,sizeof(GA_param)) ;

   switch( warp_code ){
     case WARP_SHIFT:   stup.wfunc_numpar =  3 ; break ;
     case WARP_ROTATE:  stup.wfunc_numpar =  6 ; break ;
     case WARP_SCALE:   stup.wfunc_numpar =  9 ; break ;
     case WARP_AFFINE:  stup.wfunc_numpar = 12 ; break ;
   }

   if( apply_1D != NULL && apply_nx < stup.wfunc_numpar )
     ERROR_exit("-1Dapply '%s': %d is not enough parameters per row",
                apply_1D,apply_nx);
   if( apply_1D != NULL && apply_ny < DSET_NVALS(dset_targ) )
     ERROR_exit("-1Dapply '%s': %d is not enough columns",
                apply_1D,apply_ny);

#define DEFPAR(p,nm,bb,tt,id,dd,ll)               \
 do{ stup.wfunc_param[p].min      = (bb) ;        \
     stup.wfunc_param[p].max      = (tt) ;        \
     stup.wfunc_param[p].delta    = (dd) ;        \
     stup.wfunc_param[p].toler    = (ll) ;        \
     stup.wfunc_param[p].ident    = (id) ;        \
     stup.wfunc_param[p].val_init = (id) ;        \
     stup.wfunc_param[p].val_pinit= (id) ;        \
     stup.wfunc_param[p].val_fixed= (id) ;        \
     strcpy( stup.wfunc_param[p].name , (nm) ) ;  \
     stup.wfunc_param[p].fixed  = 0 ;             \
 } while(0)

   xxx = 0.321 * (nx_base-1) * dx_base ;  /* range of shifts allowed */
   yyy = 0.321 * (ny_base-1) * dy_base ;
   zzz = 0.321 * (nz_base-1) * dz_base ;

   /* we define all 12 affine parameters, though not all may be used */

   DEFPAR( 0, "x-shift" , -xxx , xxx , 0.0 , 0.0 , 0.0 ) ;
   DEFPAR( 1, "y-shift" , -yyy , yyy , 0.0 , 0.0 , 0.0 ) ;
   DEFPAR( 2, "z-shift" , -zzz , zzz , 0.0 , 0.0 , 0.0 ) ;

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
     if( verb ) INFO_message("2D input ==> froze z-parameters") ;
   }

   /* apply any parameter-altering user commands */

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

         case PARC_RAN: stup.wfunc_param[jj].fixed     = 0 ;
                        stup.wfunc_param[jj].min       = paropt[ii].vb;
                        stup.wfunc_param[jj].max       = paropt[ii].vt;
         if( verb > 1 )
           ININFO_message("Range param#%d [%s] = %f .. %f",
                          jj+1 , stup.wfunc_param[jj].name ,
                                 stup.wfunc_param[jj].min  ,
                                 stup.wfunc_param[jj].max   ) ;
         break;
       }
     } else {
       WARNING_message("Can't alter parameter #%d: out of range!",jj+1) ;
     }
   }

   /* check to see if we have free parameters so we can actually do something */

   for( ii=jj=0 ; jj < stup.wfunc_numpar ; jj++ )
     if( !stup.wfunc_param[jj].fixed ) ii++ ;
   if( ii == 0 ) ERROR_exit("No free parameters for aligning datasets?!!") ;
   nparam_free = ii ;
   if( verb > 1 ) ININFO_message("%d free parameters",ii) ;

   /* should have some free parameters in the first 6 if using twopass */

   if( twopass ){
     for( ii=jj=0 ; jj < stup.wfunc_numpar && jj < 6 ; jj++ )
       if( !stup.wfunc_param[jj].fixed ) ii++ ;
     if( ii == 0 ){
       WARNING_message("Disabling twopass because no free parameters!?");
       twopass = 0 ;
     }
   }

   /** set convergence radius for parameter search **/

   if( im_weig == NULL ){
     xxx = 0.5f * (nx_base-1) * dx_base ;
     yyy = 0.5f * (ny_base-1) * dy_base ;
     zzz = 0.5f * (nz_base-1) * dz_base ;
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
     xxx = 0.5f * (xp-xm) * dx_base ;
     yyy = 0.5f * (yp-ym) * dy_base ;
     zzz = 0.5f * (zp-zm) * dz_base ;
   }
   xxx = (nz_base > 1) ? cbrt(xxx*yyy*zzz) : sqrt(xxx*yyy) ;
   zzz = 1.e+33 ;
   for( jj=0 ; jj < 6 && jj < stup.wfunc_numpar ; jj++ ){
     if( stup.wfunc_param[jj].fixed ) continue ;
     siz = stup.wfunc_param[jj].max - stup.wfunc_param[jj].min ;
     if( siz <= 0.0f ) continue ;
     if( jj < 3 ) yyy = conv_mm / siz ;               /* shift */
     else         yyy = 57.3f * conv_mm / (xxx*siz) ; /* angle */
     zzz = MIN(zzz,yyy) ;
   }
   conv_rad = MIN(zzz,0.001) ; conv_rad = MAX(conv_rad,0.00001) ;
   if( verb > 1 ) INFO_message("Normalized convergence radius = %.6f",conv_rad) ;

   /*** create shell of output dataset ***/

   if( prefix == NULL ){
     WARNING_message("No output dataset will be calculated") ;
   } else {
     if( dset_mast == NULL ){
       if( dset_base != NULL ) dset_mast = dset_base ;
       else                    dset_mast = dset_targ ;
     }
     if( !ISVALID_MAT44(dset_mast->daxes->ijk_to_dicom) )
       THD_daxes_to_mat44(dset_mast->daxes) ;

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

#undef  PARDUMP
#define PARDUMP(ss,xxx)                                     \
  do{ fprintf(stderr," + %s Parameters =",ss) ;             \
      for( jj=0 ; jj < stup.wfunc_numpar ; jj++ )           \
        fprintf(stderr," %.4f",stup.wfunc_param[jj].xxx) ;  \
      fprintf(stderr,"\n") ;                                \
  } while(0)
#undef  PAROUT
#define PAROUT(ss) PARDUMP(ss,val_out)
#undef  PARINI
#define PARINI(ss) PARDUMP(ss,val_init)

   if( verb && apply_1D == NULL )
     INFO_message("======== Allineation: %s =======",
                  meth_username[meth_code-1] ) ;
   else
     INFO_message("======== Processing %d sub-bricks ========",
                  DSET_NVALS(dset_targ) ) ;

   if( verb > 1 ) mri_genalign_verbose(verb-1) ;

   /*-- array in which to save parameters for later waterboarding --*/

   parsave = (float **)malloc(sizeof(float *)*DSET_NVALS(dset_targ)) ;

   /***-------------------- loop over target sub-bricks --------------------***/

   im_bset = im_base ;  /* base image for first loop */
   im_wset = im_weig ;

   for( kk=0 ; kk < DSET_NVALS(dset_targ) ; kk++ ){

     stup.match_code = meth_code ;

     if( kk == 0 && skip_first ){  /* skip first image since it == im_base */
       if( verb )
         INFO_message("===== Skip sub-brick #0: it's also base image =====") ;
       DSET_unload_one(dset_targ,0) ;
       for( jj=0 ; jj < stup.wfunc_numpar ; jj++ )   /* for -1Dfile output */
         stup.wfunc_param[jj].val_out = stup.wfunc_param[jj].ident ;
       goto WRAP_IT_UP_BABY ;
     }

     /* make copy of target brick, and deal with that */

     if( verb ) INFO_message("===== sub-brick #%d =====",kk) ;

     im_targ = mri_scale_to_float( DSET_BRICK_FACTOR(dset_targ,kk) ,
                                   DSET_BRICK(dset_targ,kk)         ) ;
     DSET_unload_one(dset_targ,kk) ;

     /* if we are just applying input parameters, set up for that now */

     if( apply_1D != NULL ){
       if( verb > 1 ) INFO_message("using -1Dapply parameters") ;
       for( jj=0 ; jj < stup.wfunc_numpar ; jj++ )
         stup.wfunc_param[jj].val_out = APL(jj,kk) ;
       stup.interp_code   = final_interp ;
       stup.smooth_code   = 0 ;
       stup.npt_match     = 11 ;
       mri_genalign_scalar_setup( im_bset , NULL , im_targ , &stup ) ;
       im_bset = NULL ;  /* after setting base, don't need to set it again */
       mri_free(im_targ) ; im_targ = NULL ;
       goto WRAP_IT_UP_BABY ;
     }

     /* initialize parameters (for the -onepass case) */

     for( jj=0 ; jj < stup.wfunc_numpar ; jj++ )
       stup.wfunc_param[jj].val_init = stup.wfunc_param[jj].val_pinit ;

     /* initialize coordinate systems */

     AL_setup_warp_coords( epi_targ,epi_fe,epi_pe,epi_se,
                           nxyz_base, dxyz_base, stup.base_cmat,
                           nxyz_targ, dxyz_targ, stup.targ_cmat ) ;

     /*--- do coarse resolution pass? ---*/

     didtwo = 0 ;
     if( twopass && (!twofirst || !tfdone) ){
       int tb , ib , ccode ;
       if( verb ) INFO_message("Start coarse pass") ;
       ccode            = (interp_code == MRI_NN) ? MRI_NN : MRI_LINEAR ;
       stup.interp_code = ccode ;
       stup.npt_match   = nmask / 20 ;
            if( stup.npt_match <   666 ) stup.npt_match =   666 ;
       else if( stup.npt_match > 22222 ) stup.npt_match = 23456 ;

       stup.smooth_code        = sm_code ;
       stup.smooth_radius_base =
        stup.smooth_radius_targ = (sm_rad == 0.0f) ? 11.111f : sm_rad ;

       mri_genalign_scalar_setup( im_bset , im_wset , im_targ , &stup ) ;
       im_bset = NULL; im_wset = NULL;  /* after being set, needn't set again */

       if( save_hist != NULL ){  /* Save start 2D histogram: 28 Sep 2006 */
         int nbin ; float *xyc ;
         (void)mri_genalign_scalar_cost( &stup ) ;
         nbin = retrieve_2Dhist( &xyc ) ;
         if( nbin > 0 && xyc != NULL ){
           char fname[256] ; MRI_IMAGE *fim ; double ftop ;
           fim = mri_new(nbin,nbin,MRI_float); mri_fix_data_pointer(xyc,fim);
           ftop = mri_max(fim) ; qim = mri_to_byte_scl(255.4/ftop,0.0,fim) ;
           mri_clear_data_pointer(fim); mri_free(fim);
           fim = mri_flippo(MRI_ROT_90,0,qim); mri_free(qim);
           sprintf(fname,"%s_start_%04d.pgm",save_hist,kk) ;
           mri_write_pnm(fname,fim); mri_free(fim);
           if( verb > 1 ) ININFO_message("- Saved histogram to %s",fname) ;
         }
       }

       /*- search for coarse start parameters, then optimize them? -*/

       if( tbest > 0 ){  /* default tbest==4 */

         if( verb ) ININFO_message("- Search for coarse starting parameters") ;

         /* startup search only allows up to 6 parameters, so freeze excess */

         if( nparam_free > 6 ){
           for( ii=jj=0 ; jj < stup.wfunc_numpar ; jj++ ){
             if( !stup.wfunc_param[jj].fixed ){
               ii++ ;  /* number free so far */
               if( ii > 6 ) stup.wfunc_param[jj].fixed = 1 ;  /* temp freeze */
             }
           }
         }

         /* do the startup parameter search;
            saves best params in val_init (and val_out), plus a few more in val_trial */

         if( verb > 1 ) ctim = COX_cpu_time() ;
         mri_genalign_scalar_ransetup( &stup , 31 ) ;
         if( verb > 1 ) ININFO_message("- Search CPU time = %.1f s",COX_cpu_time()-ctim);

         /* unfreeze those that were temporarily frozen above */

         for( jj=0 ; jj < stup.wfunc_numpar ; jj++ )
           if( stup.wfunc_param[jj].fixed == 1 ) stup.wfunc_param[jj].fixed = 0 ;

         stup.npt_match = nmask / 7 ;
              if( stup.npt_match < 666   ) stup.npt_match = 666 ;
         else if( stup.npt_match > 99999 ) stup.npt_match = 99999 ;

         /* now optimize the tbest values saved already */

         tb = MIN(tbest,stup.wfunc_ntrial) ; nfunc=0 ;
         if( verb ) ININFO_message("- Start %d coarse optimizations",tb) ;
         if( verb > 1 ) ctim = COX_cpu_time() ;

         for( ib=0 ; ib < tb ; ib++ )
           for( jj=0 ; jj < stup.wfunc_numpar ; jj++ )
             tfparm[ib][jj] = stup.wfunc_param[jj].val_trial[ib] ;

         rad = 0.0345 ;
         for( rr=0 ; rr < 2 ; rr++ , rad*=0.234 ){ /* refine w/ less smooth */
           stup.smooth_radius_base *= 0.567 ;
           stup.smooth_radius_targ *= 0.567 ;
           mri_genalign_scalar_setup( NULL,NULL,NULL , &stup ) ;

           for( ib=0 ; ib < tb ; ib++ ){                  /* loop over trials */
             for( jj=0 ; jj < stup.wfunc_numpar ; jj++ )  /* load parameters */
               stup.wfunc_param[jj].val_init = tfparm[ib][jj] ;
             nfunc += mri_genalign_scalar_optim( &stup , rad , 0.1*rad , 6666 ) ;
             for( jj=0 ; jj < stup.wfunc_numpar ; jj++ )  /* save best params */
               tfparm[ib][jj] = stup.wfunc_param[jj].val_out ;
             if( verb > 1 ) ININFO_message("- #%d has cost=%f",ib+1,stup.vbest) ;
           }
         }
         if( verb > 1 ) ININFO_message("- Coarse CPU time = %.1f s; funcs = %d",
                                       COX_cpu_time()-ctim,nfunc ) ;

         tfdone = tb ;  /* number we've save in tfparm */

       /*- just optimize at coarse setup from default initial parameters -*/

       } else {  /* if user did '-twobest 0' */

         if( verb     ) ININFO_message("- Start coarse optimization") ;
         if( verb > 1 ) ctim = COX_cpu_time() ;
         nfunc = mri_genalign_scalar_optim( &stup , 0.05 , 0.005 , 6666 ) ;
         stup.npt_match = nmask / 10 ;
              if( stup.npt_match < 666   ) stup.npt_match = 666 ;
         else if( stup.npt_match > 55555 ) stup.npt_match = 55555 ;
         stup.smooth_radius_base *= 0.666 ;
         stup.smooth_radius_targ *= 0.666 ;
         mri_genalign_scalar_setup( NULL,NULL,NULL , &stup ) ;
         nfunc += mri_genalign_scalar_optim( &stup , 0.02 , 0.002 , 6666 ) ;
         if( verb > 1 ) ININFO_message("- Coarse CPU time = %.1f s; funcs = %d",
                                       COX_cpu_time()-ctim,nfunc) ;
         if( verb     ) ININFO_message("- Coarse optimization:  best cost=%f",
                                       stup.vbest) ;

         for( jj=0 ; jj < stup.wfunc_numpar ; jj++ )  /* save best params */
           tfparm[0][jj] = stup.wfunc_param[jj].val_out ;
         tfdone = 1 ;
       }

       /*- 22 Sep 2006: include default init params in the tfparm list -*/

       for( jj=0 ; jj < stup.wfunc_numpar ; jj++ )
         tfparm[tfdone][jj] = stup.wfunc_param[jj].val_pinit ;
       tfdone++ ;

       didtwo = 1 ;   /* mark that we did the first pass */

     } /*------------- end of twopass-ization -------------*/

     /*----------------------- do final resolution pass -----------------------*/

     if( verb ) INFO_message("Fine pass begins") ;
     stup.interp_code = interp_code ;
     stup.smooth_code = sm_code ;
     if( fine_rad > 0.0f ){
       stup.smooth_radius_base = stup.smooth_radius_targ = fine_rad ;
     } else if( diffblur ){
       float br=cbrt(dx_base*dy_base*dz_base) ;  /* base voxel size */
       float tr=cbrt(dx_targ*dy_targ*dz_targ) ;  /* targ voxel size */
       stup.smooth_radius_targ = 0.0f ;
       stup.smooth_radius_base = (tr <= 1.1f*br) ? 0.0f
                                                 : sqrt(tr*tr-br*br) ;
     }
     stup.npt_match = npt_match ;
     if( didtwo )
       mri_genalign_scalar_setup( NULL,NULL,NULL, &stup ) ;  /* simple re-setup */
     else {
       mri_genalign_scalar_setup( im_bset , im_wset , im_targ , &stup ) ;
       im_bset = NULL; im_wset = NULL;  /* after being set, needn't set again */
     }

     /* choose initial parameters, based on interp_code cost function */

     if( tfdone ){                           /* find best in tfparm array */
       int kb=0 , ib ; float cbest=1.e+33 ;
       for( ib=0 ; ib < tfdone ; ib++ ){
         for( jj=0 ; jj < stup.wfunc_numpar ; jj++ )
           stup.wfunc_param[jj].val_init = tfparm[ib][jj] ;
         cost = mri_genalign_scalar_cost( &stup ) ;
         if( verb > 1 ) ININFO_message("- cost(#%d)=%f %c",
                                       ib+1,cost,(cost<cbest)?'*':' ');
         if( cost < cbest ){ cbest=cost ; kb=ib ; }
       }
       for( jj=0 ; jj < stup.wfunc_numpar ; jj++ )
         stup.wfunc_param[jj].val_init = tfparm[kb][jj] ;
       cost_ini = cbest ;
     } else {
       cost_ini = mri_genalign_scalar_cost( &stup ) ;
     }

     if( verb > 1 ){
       PARINI("- Initial fine") ;
       ININFO_message("- Initial cost = %f",cost_ini) ;
       ctim = COX_cpu_time() ;
     }

     if( powell_mm > 0.0f ) powell_set_mfac( powell_mm , powell_aa ) ;
     nfunc = 0 ;
     switch( tfdone ){
        case 0: rad = 0.0345 ; break ;
        case 1:
        case 2: rad = 0.0123 ; break ;
       default: rad = 0.0066 ; break ;
     }

     /* start with some optimization with linear interp, for speed? */

     if( MRI_HIGHORDER(interp_code) || npt_match > 999999 ){
       float pini[MAXPAR] ;
       stup.interp_code = MRI_LINEAR ;
       stup.npt_match   = MIN(499999,npt_match) ;
       mri_genalign_scalar_setup( NULL,NULL,NULL, &stup ) ;
       if( verb > 1 ) ININFO_message("- start Intrmed optimization") ;
       nfunc = mri_genalign_scalar_optim( &stup, rad, 0.0666*rad, 6666 );
       for( jj=0 ; jj < stup.wfunc_numpar ; jj++ ){
         pini[jj] = stup.wfunc_param[jj].val_init ;
         stup.wfunc_param[jj].val_init = stup.wfunc_param[jj].val_out ;
       }
       stup.interp_code = interp_code ;
       stup.npt_match   = npt_match ;
       mri_genalign_scalar_setup( NULL,NULL,NULL, &stup ) ;
       cost = mri_genalign_scalar_cost( &stup ) ; /* interp_code, not LINEAR */
       if( cost > cost_ini ){   /* should not happen, but it could since  */
         if( verb > 1 )         /* LINEAR cost optimized above isn't same */
           ININFO_message("- Intrmed cost = %f > Initial cost = %f :-(",cost,cost_ini);
         for( jj=0 ; jj < stup.wfunc_numpar ; jj++ )
           stup.wfunc_param[jj].val_init = pini[jj] ;
       } else {
         if( verb > 1 ){
           PARINI("- Intrmed fine") ;
           ININFO_message("- Intrmed cost = %f  funcs = %d",cost,nfunc) ;
         }
         if( nfunc < 6666 ) rad *= 0.246 ;
       }
     }

     /* now do the final final optimization, with the correct interp mode */

     if( verb > 2 ) GA_do_cost(1);

     nfunc += mri_genalign_scalar_optim( &stup , rad, conv_rad,6666 );

     if( powell_mm > 0.0f ) powell_set_mfac( 0.0f , 0.0f ) ;
     if( verb > 2 ) GA_do_cost(0);
     if( verb > 1 ) ININFO_message("- Fine CPU time = %.1f s",
                                   COX_cpu_time()-ctim) ;
     if( verb ) ININFO_message("- Fine Optimization took %d trials; final cost=%f",
                               nfunc,stup.vbest) ;

     if( verb > 1 ) PAROUT("Final fine fit") ;

#if 0
     for( jj=0 ; jj < stup.wfunc_numpar ; jj++ )
       stup.wfunc_param[jj].val_init = stup.wfunc_param[jj].val_out ;
     mri_genalign_verbose(9) ;
     cost = mri_genalign_scalar_cost( &stup ) ;
     INFO_message("Recomputed final cost = %g",cost) ;
     if( verb > 1 ) mri_genalign_verbose(verb-1) ;
#endif

     if( save_hist != NULL ){  /* Save final 2D histogram: 28 Sep 2006 */
       int nbin ; float *xyc ;
       for( jj=0 ; jj < stup.wfunc_numpar ; jj++ )
         stup.wfunc_param[jj].val_init = stup.wfunc_param[jj].val_out;
       (void)mri_genalign_scalar_cost( &stup ) ;
       nbin = retrieve_2Dhist( &xyc ) ;
       if( nbin > 0 && xyc != NULL ){
         char fname[256] ; MRI_IMAGE *fim ; double ftop ;
         fim = mri_new(nbin,nbin,MRI_float); mri_fix_data_pointer(xyc,fim);
         ftop = mri_max(fim) ; qim = mri_to_byte_scl(255.4/ftop,0.0,fim) ;
         mri_clear_data_pointer(fim); mri_free(fim);
         fim = mri_flippo(MRI_ROT_90,0,qim); mri_free(qim);
         sprintf(fname,"%s_final_%04d.pgm",save_hist,kk) ;
         mri_write_pnm(fname,fim); mri_free(fim);
         if( verb > 1 ) ININFO_message("- Saved histogram to %s",fname) ;
       }
     }

     mri_free(im_targ) ; im_targ = NULL ;

     /*--- 27 Sep 2006: check if results are stable when
                        we optimize a different cost function ---*/

     if( meth_check > 0 ){
       float pval[MAXPAR] , pdist , dmax ; int jmax,jtop ;
       if( verb > 1 ){
         ININFO_message("- Starting check vs %s",meth_longname[meth_check-1]) ;
         ctim = COX_cpu_time() ;
       }
       for( jj=0 ; jj < stup.wfunc_numpar ; jj++ ) /* save output params */
         stup.wfunc_param[jj].val_init = pval[jj] = stup.wfunc_param[jj].val_out;

       stup.match_code = meth_check ;
       nfunc = mri_genalign_scalar_optim( &stup, 22.2*conv_rad, conv_rad,666 );
       stup.match_code = meth_code ;

       /* compute distance between 2 output parameter sets */

       jtop = MIN( 9 , stup.wfunc_numpar ) ;
       for( dmax=0.0f,jj=0 ; jj < jtop ; jj++ ){
         if( !stup.wfunc_param[jj].fixed ){
           pdist = fabsf( stup.wfunc_param[jj].val_out - pval[jj] )
                  /(stup.wfunc_param[jj].max-stup.wfunc_param[jj].min) ;
           if( pdist > dmax ){ dmax = pdist ; jmax = jj ; }
         }
       }

       if( verb > 1 ){
         ININFO_message("- Check CPU time=%.1f s; trials=%d; dmax=%f jmax=%d",
                        COX_cpu_time()-ctim , nfunc , dmax , jmax ) ;
         PAROUT("Check fit") ;
       }
       if( dmax > 20.0*conv_rad )
         WARNING_message(
           "Check vs %s: max parameter discrepancy=%.4f%%! tolerance=%.4f%%",
           meth_longname[meth_check-1] , 100.0*dmax , 2000.0*conv_rad ) ;

       for( jj=0 ; jj < stup.wfunc_numpar ; jj++ )
         stup.wfunc_param[jj].val_out = pval[jj] ;  /* restore previous param */
     }

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
       float pp[MAXPAR] ;
       for( jj=0 ; jj < stup.wfunc_numpar ; jj++ )
         pp[jj] = stup.wfunc_param[jj].val_out ;
       mri_free(im_base) ;
       if( verb > 1 ) INFO_message("Computing replacement base image") ;
       im_base =
        im_bset = mri_genalign_scalar_warpone(
                             stup.wfunc_numpar , pp , stup.wfunc ,
                             stup.ajim, nx_base,ny_base,nz_base, final_interp );
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

     /*--- at this point, val_out contains alignment parameters ---*/

   WRAP_IT_UP_BABY: /***** goto target !!!!! *****/

     /* save parameters for the historical record */

     parsave[kk] = (float *)malloc(sizeof(float)*stup.wfunc_numpar) ;
     for( jj=0 ; jj < stup.wfunc_numpar ; jj++ )
       parsave[kk][jj] = stup.wfunc_param[jj].val_out ;

     /** store warped volume into the output dataset **/

     if( dset_out != NULL ){

       AL_setup_warp_coords( epi_targ,epi_fe,epi_pe,epi_se,
                             nxyz_dout, dxyz_dout, cmat_bout,
                             nxyz_targ, dxyz_targ, cmat_tout ) ;

       if( verb > 1 ) INFO_message("Computing output image") ;
       im_targ = mri_genalign_scalar_warpone(
                             stup.wfunc_numpar , parsave[kk] , stup.wfunc ,
                             stup.ajim ,
                             nxout , nyout , nzout , final_interp ) ;

       /* save without scaling factor */

       if( floatize || targ_kind == MRI_float ){
         EDIT_substitute_brick( dset_out,kk,MRI_float, MRI_FLOAT_PTR(im_targ) );
         mri_clear_data_pointer(im_targ) ;  /* data in im_targ saved directly */
       } else {
         EDIT_substscale_brick( dset_out,kk,MRI_float, MRI_FLOAT_PTR(im_targ) ,
                                            targ_kind, 1.0f ) ;
       }
       mri_free(im_targ) ; im_targ = NULL ;
     }

   } /***------------- end of loop over target sub-bricks ------------------***/

   DSET_unload(dset_targ) ;
   mri_free(im_base) ; mri_free(im_weig) ; mri_free(im_mask) ;

   /***--- write output dataset to disk? ---***/

   if( dset_out != NULL ){ DSET_write(dset_out); WROTE_DSET(dset_out); }

   /*--- save parameters to a file, if desired ---*/

   if( fname_1D != NULL ){
     FILE *fp ;
     fp = (strcmp(fname_1D,"-") == 0) ?  stdout
                                      : fopen(fname_1D,"w") ;
     if( fp == NULL ) ERROR_exit("Can't open -1Dfile %s for output!?",fname_1D);
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
       fclose(fp) ; if( verb ) INFO_message("Wrote -1Dfile %s",fname_1D) ;
     }
   }

   /*---------- FREE AT LAST, FREE AT LAST ----------*/

   FREE_GA_setup(&stup) ;
   for( kk=0 ; kk < DSET_NVALS(dset_targ) ; kk++ ) free((void *)parsave[kk]) ;
   free((void *)parsave) ;

   if( verb ) INFO_message("total CPU time = %.1f sec\n",COX_cpu_time()) ;
   exit(0) ;
}

/*---------------------------------------------------------------------------*/
/*! Turn an input image into a weighting factor (for -autoweight).
    If acod == 2, then make a binary mask at the end.
-----------------------------------------------------------------------------*/

MRI_IMAGE * mri_weightize( MRI_IMAGE *im , int acod )
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

   /*-- squash super-large values down to reasonability --*/

   clip = 3.0f * THD_cliplevel(qim,0.5f) ;
   if( verb > 1 ) ININFO_message("Weightize: top clip=%g",clip) ;
   for( ii=0 ; ii < nxyz ; ii++ ) if( wf[ii] > clip ) wf[ii] = clip ;

   /*-- blur a little: median then Gaussian;
          the idea is that the median filter smashes localized spikes,
          then the Gaussian filter does some general smoothing.  --*/

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
   if( verb > 1 ) ININFO_message("Weightize: bot clip=%g",clip) ;
   for( ii=0 ; ii < nxyz ; ii++ ) mmm[ii] = (wf[ii] >= clip) ;
   THD_mask_clust( nx,ny,nz, mmm ) ;
   THD_mask_erode( nx,ny,nz, mmm, 1 ) ;  /* cf. thd_automask.c */
   THD_mask_clust( nx,ny,nz, mmm ) ;
   for( ii=0 ; ii < nxyz ; ii++ ) if( !mmm[ii] ) wf[ii] = 0.0f ;
   free((void *)mmm) ;

   /*-- binarize (acod==2)?  boxize (acod==3)? --*/

#undef  BPAD
#define BPAD 4
   if( acod == 2 || acod == 3 ){
     if( verb > 1 ) ININFO_message("Weightize: binarizing") ;
     for( ii=0 ; ii < nxyz ; ii++ ) if( wf[ii] != 0.0f ) wf[ii] = 1.0f ;
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
