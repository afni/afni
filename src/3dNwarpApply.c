#include "mrilib.h"

#ifdef USE_OMP
#include <omp.h>
#endif

/* bodily include the relevant interpolation and warp functions, for speed */
#include "mri_genalign_util.c"
#include "mri_genalign.c"
#include "mri_nwarp.c"

/*----------------------------------------------------------------------------*/

void NWA_help(void)
{
     printf("\n"
      "Usage: 3dNwarpApply [options]\n"
      "\n"
      "Program to apply a nonlinear 3D warp saved from 3dQwarp (or 3dNwarpCat, etc.)\n"
      "to a 3D dataset, to produce a warped version of the source dataset.\n"
      "\n"
      "The '-nwarp' and '-source' options are MANDATORY.  For both of these options,\n"
      "as well as '-prefix', the input arguments after the option name are applied up\n"
      "until an argument starts with the '-' character, or until the arguments run out.\n"
      "\n"
      "This program has been heavily modified [01 Dec 2014], including the following\n"
      "major improvements:\n"
      "(1) Allow catenation of warps with different grid spacings -- the functions\n"
      "    that deal with the '-nwarp' option will automatically deal with the grids.\n"
      "(2) Allow input of affine warps with multiple time points, so that 3D+time\n"
      "    datasets can be warped with a time dependent '-nwarp' list.\n"
      "(3) Allow input of multiple source datasets, so that several datasets can be\n"
      "    warped the same way at once.  This operation is more efficient than running\n"
      "    3dNwarpApply several times, since the auto-regridding and auto-catenation\n"
      "    in '-nwarp' will only have to be done once.\n"
      "  *  Specification of the output dataset names can be done via multiple\n"
      "     arguments to the '-prefix' option, or via the new '-suffix' option.\n"
      "\n"
      "New Feature [28 Mar 2018]:\n"
      "(4) If a source dataset contains complex numbers, then 3dNwarpApply will warp\n"
      "    the real and imaginary parts separately, combine them, and produce a\n"
      "    complex-valued dataset as output.\n"
      "  *  Previously, the program would have warped the magnitude of the input\n"
      "     dataset and written out a float-valued dataset.\n"
      "  *  No special option is needed to warp complex-valued datasets.\n"
      "  *  If you WANT to warp the magnitude of a complex-valued dataset, you will\n"
      "     have to convert the dataset to a float dataset via 3dcalc, then use\n"
      "     3dNwarpApply on THAT dataset instead.\n"
      "  *  You cannot use option '-short' with complex-valued source datasets!\n"
      "     More precisely, you can try to use this option, but it will be ignored.\n"
      "  *  This ability is added for those of you who deal with complex-valued\n"
      "     EPI datasets (I'm looking at YOU, O International Man of Mystery).\n"
      "\n"
      "OPTIONS:\n"
      "--------\n"
      " -nwarp  www  = 'www' is the name of the 3D warp dataset\n"
      "                (this is a mandatory option!)\n"
      "               ++ Multiple warps can be catenated here.\n"
      "             -->> Please see the lengthier discussion below on this feature!\n"
      "    ++ NOTE WELL: The interpretation of this option has changed,\n"
      "                  as of 01 Dec 2014.  In particular, this option is\n"
      "                  generalized from the version in other programs, including\n"
      "                  3dNwarpCat, 3dNwarpFuncs, and 3dNwarpXYZ.  The major\n"
      "                  change is that multi-line matrix files are allowed to\n"
      "                  be included in the 'www' mixture, so that the nonlinear\n"
      "                  warp being calculated can be time-dependent.\n"
      "                  In addition, the warps supplied need not all be on the\n"
      "                  same 3D grid -- this ability lets you catenate a warp\n"
      "                  defined on the EPI data grid with a warp defined on the\n"
      "                  structural data grid (e.g.).\n"
      "\n"
      " -iwarp       = After the warp specified in '-nwarp' is computed,\n"
      "                invert it.  If the input warp would take a dataset\n"
      "                from space A to B, then the inverted warp will do\n"
      "                the reverse.\n"
      "                ++ The combination \"-iwarp -nwarp 'A B C'\" is equivalent\n"
      "                   to \"-nwarp 'INV(C) INV(B) INV(A)'\" -- that is, inverting\n"
      "                   each warp/matrix in the list *and* reversing their order.\n"
      "                ++ The '-iwarp' option is provided for convenience, and\n"
      "                   may prove to be very slow for time-dependent '-nwarp' inputs.\n"
      "\n"
      " -affter aaa  = *** THIS OPTION IS NO LONGER AVAILABLE ***\n"
      "                  See the discussion of the new '-nwarp' option above to see\n"
      "                  how to do include time-dependent matrix transformations\n"
      "                  in this program.\n"
#if 0
      "\n"
      " -wfac   fff  = Scale the warp by factor 'fff' [default=1.0]\n"
      "                ++ This option doesn't really have much use, except\n"
      "                   in making fun movies of how a brain volume deforms.\n"
      "                ++ Also note that '-wfac' applies to the warp AFTER\n"
      "                   it is inverted, if '-iwarp' is used.\n"
#endif
      "\n"
      " -source sss  = 'sss' is the name of the source dataset.\n"
      "                ++ That is, the dataset to be warped.\n"
      "                ++ Multiple datasets can be supplied here; they MUST\n"
      "                   all be defined over the same 3D grid.\n"
      "            -->>** You can no longer simply supply the source\n"
      "                   dataset as the last argument on the command line.\n"
      "\n"
      " -master mmm  = 'mmm  is the name of the master dataset.\n"
      "                ++ Which defines the output grid.\n"
      "                ++ If '-master' is not used, then output\n"
      "                   grid is the same as the source dataset grid.\n"
      "                ++ It is often the case that it makes more sense to\n"
      "                   use the '-nwarp' dataset as the master, since\n"
      "                   that is the grid on which the transformation is\n"
      "                   defined, and is (usually) the grid to which the\n"
      "                   transformation 'pulls' the source data.\n"
      "                ++ You can use '-master WARP' or '-master NWARP'\n"
      "                   for this purpose -- but ONLY if all the warps listed\n"
      "                   in the '-nwarp' option have the same 3D grid structure.\n"
      "                ++ In particular, if the transformation includes a\n"
      "                   long-distance translation, then the source dataset\n"
      "                   grid may not have a lot of overlap with the source\n"
      "                   dataset after it is transformed!\n"
      "\n"
      " -newgrid dd  = 'dd' is the new grid spacing (cubical voxels, in mm)\n"
      "   *OR        = ++ This lets you resize the master dataset grid spacing.\n"
      " -dxyz dd     =    for example, to bring EPI data to a 1 mm template, but at\n"
      "                   a coarser resolution, use '-dxyz 2'.\n"
      "\n"
      " -interp iii  = 'iii' is the interpolation mode\n"
      "                ++ Default interpolation mode is 'wsinc5' (slowest, bestest)\n"
      "                ++ Available modes are the same as in 3dAllineate:\n"
      "                     NN  linear  cubic  quintic  wsinc5\n"
      "                ++ The same interpolation mode is used for the warp\n"
      "                   itself (if needed) and then for the data being warped.\n"
      "                ++ The warp will be interpolated if the output dataset is\n"
      "                   not on the same 3D grid as the warp itself, or if a warp\n"
      "                   expression is used in the '-nwarp' option.  Otherwise,\n"
      "                   it won't need to be interpolated.\n"
      "\n"
      " -ainterp jjj = This option lets you specify a different interpolation mode\n"
      "                for the data than might be used for the warp.  In particular,\n"
      "                '-ainterp NN' would be most logical for atlas datasets, where\n"
      "                the data values being mapped are labels.\n"
#if 0
      "\n"
      " -expad EE    = Add 'EE' voxels to the warp on input, to help avoid any problems\n"
      "                that might arise if you are catenating multiple warps / matrices\n"
      "                where the nonlinear transform might be shifted far off its\n"
      "                original grid definition.  Normally not needed, since the program\n"
      "                internally estimates how much padding is needed.\n"
      "                ++ This option does not affect the use of '-master WARP', which\n"
      "                    still refers to the original grid on which the nonlinear\n"
      "                    warp is defined.\n"
#endif
      "\n"
      " -prefix ppp  = 'ppp' is the name of the new output dataset\n"
      "                ++ If more than 1 source dataset is supplied, then you\n"
      "                   should supply more than one prefix.  Otherwise, the\n"
      "                   program will invent prefixes for each output, by\n"
      "                   attaching the suffix '_Nwarp' to each source\n"
      "                   dataset's prefix.\n"
      "\n"
      " -suffix sss  = If the program generates prefixes, you can change the\n"
      "                default '_Nwarp' suffix to whatever you want (within\n"
      "                reason) by this option.\n"
      "                ++ His Holiness Emperor Zhark defines 'within reason', of course.\n"
      "                ++ By using '-suffix' and NOT using '-prefix', the program\n"
      "                   will generate prefix names for all output datasets in\n"
      "                   a systematic way -- this might be useful for some people.\n"
      "                ++ Note that only ONE suffix can be supplied even if many source\n"
      "                   datasets are input -- unlike the case with '-prefix'.\n"
      "\n"
      " -short       = Write output dataset using 16-bit short integers, rather than\n"
      "                the usual 32-bit floats.\n"
      "                ++ Intermediate values are rounded to the nearest integer.\n"
      "                   No scaling is performed.\n"
      "                ++ This option is intended for use with '-ainterp' and for\n"
      "                   source datasets that contain integral values.\n"
      "                ++ If the source dataset is complex-valued, this option will\n"
      "                   be ignored.\n"
      "\n"
      " -quiet       = Don't be verbose :-(\n"
      " -verb        = Be extra verbose :-)\n"
      "\n"
      "SPECIFYING THE NONLINEAR WARP IN '-nwarp'\n"
      "[If you are catenating warps, read this carefully!]\n"
      "---------------------------------------------------\n"
      "A single nonlinear warp (usually created by 3dQwarp) is an AFNI or NIfTI-1\n"
      "dataset with 3 sub-bricks, holding the 3D displacements of each voxel.\n"
      "(All coordinates and displacements are expressed in DICOM order.)\n"
      "\n"
      "The '-nwarp' option is used to specify the nonlinear transformation used\n"
      "to create the output dataset from the source dataset.  For many purposes,\n"
      "the only input needed here is the name of a single dataset holding the\n"
      "warp to be used.\n"
      "\n"
      "However, the '-nwarp' option also allows the catenation of a sequence of\n"
      "spatial transformations (in short, 'warps') that will be combined before\n"
      "being applied to the source dataset.  Each warp is either a nonlinear\n"
      "warp dataset or a matrix warp (a linear transformation of space).\n"
      "\n"
      "A single affine (or linear) warp is a set of 12 numbers, defining a 3x4 matrix\n"
      "   a11 a12 a13 a14\n"
      "   a21 a22 a23 a24\n"
      "   a31 a32 a33 a34\n"
      "A matrix is stored on a single line, in a file with the extension\n"
      "'.1D' or '.txt', in this order\n"
      "   a11 a12 a13 a14 a21 a22 a23 a24 a31 a32 a33 a34\n"
      "For example, the identity matrix is given by\n"
      "   1 0 0 0 0 1 0 0 0 0 1 0\n"
      "This format is output by the '-1Dmatrix_save' options in 3dvolreg and\n"
      "3dAllineate, for example.\n"
      "\n"
      "If the argument 'www' following '-nwarp' is made up of more than one warp\n"
      "filename, separated by blanks, then the nonlinear warp to be used is\n"
      "composed on the fly as needed to transform the source dataset.  For\n"
      "example,\n"
      "   -nwarp 'AA_WARP.nii BB.aff12.1D CC_WARP.nii'\n"
      "specifies 3 spatial transformations, call them A(x), B(x), and C(x) --\n"
      "where B(x) is just the 3-vector x multipled into the matrix in the\n"
      "BB.aff12.1D file.  The resulting nonlinear warp function N(x) is\n"
      "obtained by applying these transformations in the order given, A(x) first:\n"
      "   N(x) = C( B( A(x) ) )\n"
      "That is, the first warp A is applied to the output grid coordinate x,\n"
      "then the second warp B to that results, then the third warp C.  The output\n"
      "coordinate y = C(B(A(x))) is the coordinate in the source dataset at which\n"
      "the output value will be interpolated (for the voxel at coordinate x).\n"
      "\n"
      "The Proper Order of Catenated Warps:\n"
      "....................................\n"
      "To determine the correct order in which to input the warps, it is necessary\n"
      "to understand what a warp of the source dataset actually computes.  Call the\n"
      "source image S(x) = (scalar) value of source image at voxel location x.\n"
      "For each x in the output grid, the warped result is S(N(x)) -- that is,\n"
      "N(x) tells where each output location x must be warped to in order to\n"
      "find the corresponding value of the source S.\n"
      "\n"
      "N(x) does *NOT* tell to where an x in the source image must be moved to in\n"
      "the output space -- which is what you might think if you mentally prioritize\n"
      "the idea of 'warping the source image'.  It is better to think of N(x) as\n"
      "reaching out from x in the output space to a location in the source space,\n"
      "and then the program will interpolate from the discrete source space grid\n"
      "at that location -- which is unlikely to be exactly on a grid node.\n"
      "\n"
      "Now suppose the sequence of operations on an EPI dataset is\n"
      " (1) Nonlinearly unwarp the dataset via warp AA_WARP.nii (perhaps\n"
      "     from 3dQwarp -plusminus).\n"
      " (2) Perform linear volume registration on the result from (1) (with\n"
      "     program 3dvolreg) to get affine matrix file BB.aff12.1D -- which\n"
      "     will have 1 line per time point in the EPI dataset.\n"
      " (3) Linearly register the structural volume to the EPI dataset\n"
      "     (via script align_epi_anat.py).  Note that this step transforms\n"
      "     the structural volume to match the EPI, not the EPI to match the\n"
      "     structural volume, so this step does not affect the chain of\n"
      "     transformations being applied to the EPI dataset.\n"
      " (4) Nonlinearly warp the structural image from (3) to MNI space via\n"
      "     warp CC_WARP.nii (generated by 3dQwarp).\n"
      "Finally, the goal is to take the original EPI time series dataset, and\n"
      "warp it directly to MNI space, including the time series registration for\n"
      "each sub-brick in the dataset, with only one interplation being used --\n"
      "rather than the 3 interpolations that would come by serially implementing\n"
      "steps (1), (2), and (4).  This one-big-step transformation can be done\n"
      "with 3dNwarpApply using the '-nwarp' option:\n"
      "   -nwarp 'CC_WARP.nii BB.aff12.1D AA_WARP.nii'\n"
      "that is, N(x) = A( B( C(x) ) ) -- the opposite order to the sample above,\n"
      "and with the transformations occuring in the opposite order to the sequence\n"
      "in which they were calculated.  The reason for this apparent backwardness\n"
      "is that the 'x' being transformed is on the output grid -- in this case, in\n"
      "MNI-template space.  So the warp C(x) transforms such an output grid 'x' to\n"
      "the EPI-aligned structural space.  The warp B(x) then transforms THAT\n"
      "coordinate from aligned spaced back to the rotated head position of the subject.\n"
      "And the warp A(x) transforms THAT coordinate back to the original grid that had\n"
      "to be unwarped (e.g., from susceptibility and/or eddy current artifacts).\n"
      "\n"
      "Also note that in step (2), the matrix file BB.aff12.1D has one line for\n"
      "each time point.  When transforming a source dataset, the i-th time point\n"
      "will be transformed by the warp computed using the i-th line from any\n"
      "multi-line matrix file in the '-nwarp' specification.  (If there are more\n"
      "dataset time points than matrix lines, then the last line will be re-used.)\n"
      "\n"
      "In this way, 3dNwarpApply can be used to carry out time-dependent warping\n"
      "of time-dependent datasets, provided that the time-dependence in the warp\n"
      "only occurs in the affine (matrix) parts of the transformation.\n"
      "\n"
      "Note that the now-obsolete option '-affter' is subsumed into the new way\n"
      "that '-nwarp' works.  Formerly, the only time-dependent matrix had to\n"
      "be specified as being at the end of the warp chain, and was given via\n"
      "the '-affter' option.  Now, a time-dependent matrix (or more than one)\n"
      "can appear anywhere in the warp chain, so there is no need for a special\n"
      "option.  If you DID use '-affter', you will have to alter your script\n"
      "simply by putting the final matrix filename at the end of the '-nwarp'\n"
      "chain.  (If this seems too hard, please consider another line of work.)\n"
      "\n"
      "The other 3dNwarp* programs that take the '-nwarp' option operate similarly,\n"
      "but do NOT allow time-dependent matrix files.  Those programs are built to\n"
      "operate with one nonlinear warp, so allowing a time-dependent warp doesn't\n"
      "make sense for them.\n"
      "\n"
      "NOTE: If a matrix is NOT time-dependent (just a single set of 12 numbers),\n"
      "      it can be input in the .Xat.1D format of 3 rows, each with 4 values:\n"
      "         a11 a12 a13 a14  }                        1 0 0 0\n"
      "         a21 a22 a23 a24  } e.g, identity matrix = 0 1 0 0\n"
      "         a31 a32 a33 a34  }                        0 0 1 0\n"
      "      This option is just for convenience.  Remember that the coordinates\n"
      "      are DICOM order, and if your matrix comes from Some other PrograM\n"
      "      or from a Fine Software Library, you probably have to change some\n"
      "      signs in the matrix to get things to work correctly.\n"
      "\n"
      "RANDOM NOTES:\n"
      "-------------\n"
      "* At present, this program doesn't work with 2D warps, only with 3D.\n"
      "  (That is, each warp dataset must have 3 sub-bricks.)\n"
      "\n"
      "* At present, the output dataset is stored in float format, no matter what\n"
      "  absurd data format the input dataset uses (but cf. the '-short' option).\n"
      "\n"
      "* As described above, 3dNwarpApply allows you to catenate warps directly on\n"
      "  the command line, as if you used 3dNwarpCat before running 3dNwarpApply.\n"
      "  For example:\n"
      "\n"
      "  ++ You have aligned dataset Fred+orig to MNI-affine space using @auto_tlrc,\n"
      "     giving matrix file Fred.Xaff12.1D\n"
      "\n"
      "  ++ Then you further aligned from MNI-affine to MNI-qwarp via 3dQwarp,\n"
      "     giving warp dataset Fred_WARP+tlrc\n"
      "\n"
      "  ++ You can combine the transformations and interpolate Fred+orig directly\n"
      "     to MNI-qwarp space using a command like\n"
      "        3dNwarpApply -prefix Fred_final    \\\n"
      "                     -source Fred+orig     \\\n"
      "                     -master NWARP         \\\n"
      "                     -nwarp 'Fred_WARP+tlrc Fred.Xaff12.1D'\n"
      "     Note the warps to be catenated are enclosed in quotes to make a single\n"
      "     input argument passed to the program.  The processing used for this\n"
      "     purpose is the same as in 3dNwarpCat -- see the help output for that\n"
      "     program for a little more information.\n"
      "\n"
      "  ++ When you specify a nonlinear warp dataset, you can use the 'SQRT()' and\n"
      "     'INV()' and 'INVSQRT()' operators, as well as the various 1D-to-3D\n"
      "     displacement prefixes ('AP:' 'RL:' 'IS:' 'VEC:', as well as 'FAC:') -- \n"
      "     for example, the following is a legal (and even useful) definition of a\n"
      "     warp herein:\n"
      "        'SQRT(AP:epi_BU_yWARP+orig)'\n"
      "     where the 'AP:' transforms the y-displacements in epi_BU_ywarp+orig to a\n"
      "     full 3D warp (with x- and z-displacments set to zero), then calculates the\n"
      "     square root of that warp, then applies the result to some input dataset.\n"
      "    + This is a real example, where the y-displacement-only warp is computed between\n"
      "      blip-up and blip-down EPI datasets, and then the SQRT warp is applied to\n"
      "      warp them into the 'intermediate location' which should be better aligned\n"
      "      with the subject's anatomical datasets.\n"
      " -->+ However: see also the '-plusminus' option for 3dQwarp for another way to\n"
      "      reach the same goal.\n"
      "    + See the output of 3dNwarpCat -help for a little more information on the\n"
      "      1D-to-3D warp prefixes ('AP:' 'RL:' 'IS:' 'VEC:').\n"
      "\n"
      "  ++ You can scale the displacements in a 3D warp file via the 'FAC:' prefix, as in\n"
      "       FAC:0.6,0.4,-0.2:fred_WARP.nii\n"
      "     which will scale the x-displacements by 0.6, the y-displacements by 0.4, and\n"
      "     the z-displacments by -0.2.\n"
      "   + So if you need to reverse the sign of x- and y-displacments, since in AFNI\n"
      "     +x=Left and +y=Posterior while another package uses +x=Right and +y=Anterior,\n"
      "     you could use 'FAC:-1,-1,1:Warpdatasetname' to apply a warp from that\n"
      "     other software package.\n"
      "\n"
      "  ++ You can also use 'IDENT(dataset)' to define a \"nonlinear\" 3D warp whose\n"
      "     grid is defined by the dataset header -- nothing else from the dataset will\n"
      "     be used.  This warp will be filled with all zero displacements, which represents\n"
      "     the identity warp.  The purpose of such an object is to let you apply a pure\n"
      "     affine warp -- since this program requires a '-nwarp' option, you can use\n"
      "     -nwarp 'IDENT(dataset)' to define the 3D grid for the 'nonlinear' 3D warp and\n"
      "     then catenate the affine warp.\n"
      "\n"
      "* PLEASE note that if you use the '-allineate' option in 3dQwarp, then the affine\n"
      "  warp is already included in the output nonlinear warp from 3dQwarp, and so it\n"
      "  does NOT need to be applied again in 3dNwarpApply!  This mistake has been made\n"
      "  in the past, and the results were not good.\n"
     ) ;

     PRINT_AFNI_OMP_USAGE("3dNwarpApply",NULL) ; PRINT_COMPILE_DATE ;
     exit(0) ;
}

/*----------------------------------------------------------------------------*/
/* This program is basically a wrapper for function THD_nwarp_dataset() */
/*----------------------------------------------------------------------------*/

int main( int argc , char *argv[] )
{
   THD_3dim_dataset_array *dset_srcar=NULL , *dset_outar=NULL ;
   THD_3dim_dataset *dset_mast=NULL , *dset_out=NULL , *dset_sss,*dset_ooo ;
   Nwarp_catlist *nwc=NULL ; char *nwc_string=NULL ;
   MRI_IMARR *imar_nwarp=NULL , *im_src ;
   char **prefix    = NULL ; int nprefix=0 ; char *suffix = "_Nwarp" ;
   float dxyz_mast  = 0.0f ;
   int interp_code  = MRI_WSINC5 ;
   int ainter_code  = -666 ;
   int iarg , kk , verb=1 , iv , do_wmast=0 , do_iwarp=0 ;
   mat44 src_cmat, nwarp_cmat, mast_cmat ;
   MRI_IMAGE *fim , *wim ; float *ip,*jp,*kp ;
   int nx,ny,nz,nxyz , toshort=0 ;
   float wfac=1.0f ;
#if 0
   MRI_IMAGE *awim=NULL ; THD_3dim_dataset *dset_nwarp=NULL ;
#endif
   int expad=0 ;  /* 26 Aug 2014 */

   AFNI_SETUP_OMP(0) ;  /* 24 Jun 2013 */

   /**----------------------------------------------------------------------*/
   /**----------------- Help the pitifully ignorant user? -----------------**/

   if( argc < 2 || strcasecmp(argv[1],"-help") == 0 ) NWA_help() ;

   /**-------- bookkeeping and marketing --------**/

   mainENTRY("3dNwarpApply"); machdep();
   AFNI_logger("3dNwarpApply",argc,argv);
   PRINT_VERSION("3dNwarpApply"); AUTHOR("Zhark the Warped");
   (void)COX_clock_time() ;
   putenv("AFNI_WSINC5_SILENT=YES") ;

   /**--- process command line options ---**/

   iarg = 1 ;
   while( iarg < argc && argv[iarg][0] == '-' ){

     /*---------------*/

     if( strcasecmp(argv[iarg],"-quiet") == 0 ){
       verb = 0 ; iarg++ ; continue ;
     }
     if( strcasecmp(argv[iarg],"-verb") == 0 ){
       verb++ ; iarg++ ; continue ;
     }
     if( strcasecmp(argv[iarg],"-short") == 0 ){
       toshort = 1 ; iarg++ ; continue ;
     }
     if( strcmp(argv[iarg],"-") == 0 ){  /* just skip */
       iarg++ ; continue ;
     }

     /*---------------*/

     if( strcasecmp(argv[iarg],"-expad") == 0 ){  /* 26 Aug 2014 */
       if( ++iarg >= argc ) ERROR_exit("need arg after %s",argv[iarg-1]) ;
       expad = (int)strtod(argv[iarg],NULL) ;
       if( expad < 0 ) expad = 0 ;
       iarg++ ; continue ;
     }

     /*---------------*/

#if 0
     if( strcasecmp(argv[iarg],"-affter") == 0 || strcasecmp(argv[iarg],"-after") == 0 ){
       MRI_IMAGE *qim ;
       if( awim != NULL )   ERROR_exit("Can't have multiple %s options :-("        ,argv[iarg]  );
       if( ++iarg >= argc ) ERROR_exit("No argument after '%s' :-("                ,argv[iarg-1]);
       awim = mri_read_1D(argv[iarg]) ;
       if( awim == NULL )   ERROR_exit("Can't read affine warp data from file '%s'",argv[iarg]  );
       if( awim->nx == 3 && awim->ny == 4 ){                         /* in 3x4 'Xat.1D' format? */
         MRI_IMAGE *qim = mri_rowmajorize_1D(awim); mri_free(awim); awim = qim; /* make it 1x12 */
         awim->nx = 12 ; awim->ny = 1 ;
       } else {                               /* should be in N x 12 format */
              if( awim->ny < 12 )
                ERROR_exit("Affine warp file '%s': fewer than 12 values per row"     ,argv[iarg]  );
         else if( awim->ny > 12 )
                WARNING_message("Affine warp file '%s': more than 12 values per row" ,argv[iarg]  );
         qim = mri_transpose(awim) ; mri_free(awim) ; awim = qim ;  /* flip to column major order */
       }
       iarg++ ; continue ;
     }
#endif

     /*---------------*/

     if( strcasecmp(argv[iarg],"-nwarp") == 0 ||
         strcasecmp(argv[iarg],"-warp" ) == 0 ||
         strcasecmp(argv[iarg],"-qwarp") == 0   ){

       int nwclen=0 ;  /* length of string to create */

       if( nwc_string != NULL )
         ERROR_exit("Can't have multiple %s options :-(",argv[iarg]) ;
       if( ++iarg >= argc )
         ERROR_exit("No argument after '%s' :-(",argv[iarg-1]) ;

       for( kk=iarg ; kk < argc && argv[kk][0] != '-' ; kk++ ){ /* to '-' or end */
         nwclen += strlen(argv[kk])+4 ; /* iarg->kk   27 Jul, 2017 [rickr] */
       }
       if( nwclen <= 0 )
         ERROR_exit("Invalid argument after '%s'",argv[iarg-1]) ;
       nwc_string = (char *)malloc(sizeof(char)*nwclen) ; nwc_string[0] = '\0' ;
       for( kk=iarg ; kk < argc && argv[kk][0] != '-' ; kk++,iarg++ ){
         strcat(nwc_string,argv[kk]) ; strcat(nwc_string," ") ;
       }
       continue ; /* don't do iarg++ since iarg already at next option (or end) */
     }

     /*---------------*/

     if( strcasecmp(argv[iarg],"-source") == 0 ){
       int nbad=0 ; char *gs=NULL , *hs=NULL ;
       if( dset_srcar != NULL )
         ERROR_exit("Can't have multiple %s options :-(",argv[iarg]) ;
       if( ++iarg >= argc )
         ERROR_exit("No argument after '%s' :-(",argv[iarg-1]) ;
       INIT_3DARR(dset_srcar) ;
       for( ; iarg < argc && argv[iarg][0] != '-' ; iarg++ ){  /* to '-' or end */
         dset_sss = THD_open_dataset( argv[iarg] ) ;
         if( dset_sss == NULL ){
           ERROR_message("Can't open -source dataset '%s' :-(",argv[iarg]) ;
           nbad++ ; continue ;
         }
         if( verb ) INFO_message("opened source dataset '%s'",argv[iarg]) ;
         DSET_COPYOVER_REAL(dset_sss) ;
         ADDTO_3DARR(dset_srcar,dset_sss) ;
         if( dset_srcar->num == 1 ){ /* check geometries of datasets match 1st */
           gs = EDIT_get_geometry_string(dset_sss) ;
         } else {
           hs = EDIT_get_geometry_string(dset_sss) ;
           if( EDIT_geometry_string_diff(gs,hs) > 0.01f ){
             ERROR_message(
              "Geometry mismatch for -source dataset '%s' (compared to first)",
              argv[iarg]);
             nbad++ ;
           }
           free(hs) ; hs = NULL ;
         }
       }
       if( dset_srcar->num <= 0 )
         ERROR_exit("No good source datasets found!") ;
       else if( nbad       >  0 )
         ERROR_exit("Can't continue after above error%s",(nbad > 0)?"s":"\0") ;
       if( gs != NULL ) free(gs) ;
       continue ;  /* don't do iarg++ ; iarg already at next option (or end) */
     }

     /*---------------*/

     if( strncasecmp(argv[iarg],"-prefix",5) == 0 ){
       int nbad=0 , pp,qq ;
       if( prefix != NULL )
         ERROR_exit("Can't have multiple %s options :-(",argv[iarg]) ;
       if( ++iarg >= argc )
         ERROR_exit("No argument after '%s' :-(",argv[iarg-1]) ;

       for( kk=iarg ; kk < argc && argv[kk][0] != '-' ; kk++ ){ /* to '-' or end */
         if( !THD_filename_ok(argv[kk]) || strcasecmp(argv[kk],"NULL")==0 ){
           ERROR_message("Badly formed prefix '%s' :-(",argv[kk]);
           nbad++ ;
         }
         nprefix++ ;
       }
       if( nprefix   <= 0 )
         ERROR_exit("No prefix name given after '%s' ???",argv[iarg-1]) ;
       else if( nbad >  0 )
         ERROR_exit("Can't continue after above error%s",(nbad > 0)?"s":"\0") ;

       prefix = (char **)malloc(sizeof(char *)*nprefix) ;
       for( pp=0,kk=iarg ; kk < argc && argv[kk][0] != '-' ; kk++,pp++ ){
         prefix[pp] = strdup(argv[kk]) ;
         for( qq=0 ; qq < pp ; qq++ ){
           if( strcmp(prefix[pp],prefix[qq]) == 0 ){
             ERROR_message("Prefix name '%s' is duplicated -- this is FORBIDDEN!",
                           prefix[pp] ) ;
             nbad++ ;
           }
         }
       }
       if( nbad >  0 )
         ERROR_exit("Can't continue after above error%s",(nbad > 0)?"s":"\0") ;
       iarg = kk ; continue ;
     }

     /*---------------*/

     if( strncasecmp(argv[iarg],"-suffix",5) == 0 ){
       if( ++iarg >= argc )
         ERROR_exit("No argument after '%s' :-(",argv[iarg-1]) ;
       if( !THD_filename_pure(argv[iarg]) )
         ERROR_exit("Illegal name '%s' after option '%s'",
                    argv[iarg],argv[iarg-1]) ;
       if( strlen(argv[iarg]) > 128 )
         ERROR_exit("Too long name '%s' after option '%s'",
                    argv[iarg],argv[iarg-1]) ;
       suffix = strdup(argv[iarg]) ;
       iarg++ ; continue ;
     }

     /*---------------*/

     if( strcasecmp(argv[iarg],"-master") == 0 ){
       if( dset_mast != NULL || do_wmast )
         ERROR_exit("Can't have multiple %s options :-(",argv[iarg]) ;
       if( ++iarg >= argc )
         ERROR_exit("No argument after '%s' :-(",argv[iarg-1]) ;
       if( strcmp(argv[iarg],"WARP") == 0 || strcmp(argv[iarg],"NWARP") == 0 ){
         do_wmast = 1 ;
         if( verb ) INFO_message("-master dataset is the input warp") ;
       } else {
         dset_mast = THD_open_dataset( argv[iarg] ) ;
         if( dset_mast == NULL )
           ERROR_exit("can't open -master dataset '%s' :-(",argv[iarg]);
         DSET_COPYOVER_REAL(dset_mast) ;
         if( verb ) INFO_message("-master dataset is '%s'",argv[iarg]) ;
       }
       iarg++ ; continue ;
     }

     /*---------------*/

     if( strcasecmp(argv[iarg],"-mast_dxyz") == 0 ||
         strcasecmp(argv[iarg],"-dxyz_mast") == 0 ||
         strcasecmp(argv[iarg],"-dxyz")      == 0 ||
         strcasecmp(argv[iarg],"-newgrid"  ) == 0   ){

       if( ++iarg >= argc )
         ERROR_exit("No argument after '%s' :-(",argv[iarg-1]) ;
       dxyz_mast = (float)strtod(argv[iarg],NULL) ;
       if( dxyz_mast <= 0.01f )
         ERROR_exit("Illegal value '%s' after -mast_dxyz :-(",argv[iarg]) ;
       if( dxyz_mast <= 0.5f )
         WARNING_message("Small value %g after -mast_dxyz :-?",dxyz_mast) ;
       INFO_message("output grid size = %.3g mm",dxyz_mast) ;
       iarg++ ; continue ;
     }

     /*---------------*/

     if( strcasecmp(argv[iarg],"-NN")         == 0 ||
         strncasecmp(argv[iarg],"-nearest",6) == 0   ){
       CW_interp = interp_code = MRI_NN ; iarg++ ; continue ;
     }
     if( strncasecmp(argv[iarg],"-linear",4)   ==0 ||
         strncasecmp(argv[iarg],"-trilinear",6)==0   ){
       CW_interp = interp_code = MRI_LINEAR ; iarg++ ; continue ;
     }
     if( strncasecmp(argv[iarg],"-cubic",4)==0    ||
         strncasecmp(argv[iarg],"-tricubic",6)==0   ){
       CW_interp = interp_code = MRI_CUBIC ; iarg++ ; continue ;
     }
     if( strncasecmp(argv[iarg],"-quintic",4)==0    ||
         strncasecmp(argv[iarg],"-triquintic",6)==0   ){
       CW_interp = interp_code = MRI_QUINTIC ; iarg++ ; continue ;
     }
     if( strncasecmp(argv[iarg],"-wsinc",5) == 0 ){
       CW_interp = interp_code = MRI_WSINC5 ; iarg++ ; continue ;
     }
     if( strncasecmp(argv[iarg],"-interp",5)==0 ){
       char *inam ;
       if( ++iarg >= argc ) ERROR_exit("No argument after '%s' :-(",argv[iarg-1]) ;
       inam = argv[iarg] ; if( *inam == '-' ) inam++ ;
       if( strcasecmp(inam,"NN")==0 || strncasecmp(inam,"nearest",5)==0 )
         CW_interp = interp_code = MRI_NN ;
       else
       if( strncasecmp(inam,"linear",3)==0    ||
           strncasecmp(inam,"trilinear",5)==0   )
         CW_interp = interp_code = MRI_LINEAR ;
       else
       if( strncasecmp(inam,"cubic",3)==0    ||
           strncasecmp(inam,"tricubic",5)==0   )
         CW_interp = interp_code = MRI_CUBIC ;
       else
       if( strncasecmp(inam,"quintic",3)==0    ||
           strncasecmp(inam,"triquintic",5)==0   )
         CW_interp = interp_code = MRI_QUINTIC ;
       else
       if( strncasecmp(inam,"WSINC",5)==0 )
         CW_interp = interp_code = MRI_WSINC5 ;
       else
         ERROR_exit("Unknown code '%s' after '%s' :-(",argv[iarg],argv[iarg-1]) ;
       iarg++ ; continue ;
     }
     if( strncasecmp(argv[iarg],"-ainterp",5)==0 ){
       char *inam ;
       if( ++iarg >= argc ) ERROR_exit("No argument after '%s' :-(",argv[iarg-1]) ;
       inam = argv[iarg] ; if( *inam == '-' ) inam++ ;
       if( strcasecmp(inam,"NN")==0 || strncasecmp(inam,"nearest",5)==0 )
         ainter_code = MRI_NN ;
       else
       if( strncasecmp(inam,"linear",3)==0    ||
           strncasecmp(inam,"trilinear",5)==0   )
         ainter_code = MRI_LINEAR ;
       else
       if( strncasecmp(inam,"cubic",3)==0    ||
           strncasecmp(inam,"tricubic",5)==0   )
         ainter_code = MRI_CUBIC ;
       else
       if( strncasecmp(inam,"quintic",3)==0    ||
           strncasecmp(inam,"triquintic",5)==0   )
         ainter_code = MRI_QUINTIC ;
       else
       if( strncasecmp(inam,"WSINC",5)==0 )
         ainter_code = MRI_WSINC5 ;
       else
         ERROR_exit("Unknown code '%s' after '%s' :-(",argv[iarg],argv[iarg-1]) ;
       iarg++ ; continue ;
     }

     /*---------------*/

     if( strcasecmp(argv[iarg],"-iwarp") == 0 ){
       do_iwarp = 1 ; iarg++ ; continue ;
     }

     /*---------------*/

     ERROR_message("Mysteriously Unknown option '%s' :-( :-( :-(",argv[iarg]) ;
     suggest_best_prog_option(argv[0],argv[iarg]) ;
     exit(1) ;

   }

   /*-------- check inputs to see if the user is completely demented ---------*/

   if( nwc_string == NULL )
     ERROR_exit("No -nwarp option?  How do you want to warp? :-(") ;

   if( dset_srcar == NULL )
     ERROR_exit("No -source option?  What do you want to do? :-(") ;

   /*-- deal with extra or insufficient prefixes --*/

   if( nprefix > dset_srcar->num ){  /* extra */

     INFO_message(
       "-prefix had %d names, but only %d source datasets ==> ignoring any extra prefixes",
       nprefix , dset_srcar->num ) ;

   } else if( nprefix < dset_srcar->num ){  /* insufficient */
     char *cpt ;

     INFO_message("Number of -prefix names (%d) is fewer than number of source datasets (%d)",
                  nprefix , dset_srcar->num ) ;
     INFO_message("Making up the missing prefix names from Everest-thin air:") ;

     prefix = (char **)realloc(prefix,sizeof(char *)*dset_srcar->num) ;
     for( kk=nprefix ; kk < dset_srcar->num ; kk++ ){
       prefix[kk] = (char *)malloc(sizeof(char)*THD_MAX_NAME) ;
       dset_sss = DSET_IN_3DARR(dset_srcar,kk) ;
       strcpy( prefix[kk] , DSET_PREFIX(dset_sss) ) ;
       cpt = strcasestr(prefix[kk],".nii") ; if( cpt != NULL ) *cpt = '\0' ;
       strcat(prefix[kk],suffix) ; if( cpt != NULL ) strcat(prefix[kk],".nii") ;
       ININFO_message("Source %s ==> Prefix %s",DSET_PREFIX(dset_sss),prefix[kk]) ;
     }

   }

   /*-- default interp codes --*/

   if( ainter_code < 0 ) ainter_code = interp_code ;

   /* verb_nww = global variable used in warping functions in mri_nwarp.c */

   verb_nww = AFNI_yesenv("AFNI_DEBUG_WARP") ? 2 : verb ;

   /*--- read and setup the list of nonlinear warps ---*/

   if( verb || verb_nww ) fprintf(stderr,"++ Processing -nwarp ") ;

   /* read the list */

   nwc = IW3D_read_nwarp_catlist( nwc_string ) ;
   if( verb && verb_nww < 2 ) fprintf(stderr,"\n") ;

   if( nwc == NULL )
     ERROR_exit("Cannot process warp string '%s'",nwc_string) ;

   if( do_iwarp ) nwc->flags |= NWC_INVERT_MASK ;  /* set inversion flag */

   /* set up the geometry of the constituent warps */

   CW_extra_pad = expad ; /* and ask for some addition padding */

   /* create the master dataset */

   if( do_wmast ){
     if(  nwc->actual_geomstring == NULL )
       ERROR_exit("Cannot use '-master NWARP' when '-nwarp' contains datasets with mis-matched grids!") ;
     dset_mast = EDIT_geometry_constructor( nwc->actual_geomstring , "Zhark_Likes_Elephants" ) ;
   } else if( dset_mast == NULL ){
     dset_mast = DSET_IN_3DARR(dset_srcar,0) ;
   }

   if( dxyz_mast > 0.0f ){
     THD_3dim_dataset *qset ; double dxyz = (double)dxyz_mast ;
     qset = r_new_resam_dset( dset_mast , NULL ,
                              dxyz,dxyz,dxyz ,
                              NULL , RESAM_NN_TYPE , NULL , 0 , 0) ;
     if( qset != NULL ){
       dset_mast = qset ;
       THD_daxes_to_mat44(dset_mast->daxes) ;
     }
   }

   /* here is where geometry of nonlinear warps is harmonized (if needed) */

   if( nwc->actual_geomstring == NULL )
     IW3D_set_geometry_nwarp_catlist( nwc , EDIT_get_geometry_string(dset_mast) ) ;

   /* combine warps (matrices and datasets) to the extent possible */

   IW3D_reduce_nwarp_catlist( nwc ) ;  /* may already have been done */
   NI_sleep(1) ;

   /*--------- the actual work of warping ---------*/

   if( verb > 1 || verb_nww > 1 )
     INFO_message(".......... Starting dataset warping ..........") ;

   dset_outar = THD_nwarp_dataset_array( nwc , dset_srcar , dset_mast , prefix ,
                                         interp_code,ainter_code , 0.0f , wfac , 0 ) ;

   if( dset_outar == NULL ) ERROR_exit("Warping failed for some reason :-(((") ;

   /*-- conversion to shorts? --*/

   if( toshort ){
     int iv,nxyz ; short *sar ; float *far ;
     for( kk=0 ; kk < dset_outar->num ; kk++ ){
       dset_ooo = DSET_IN_3DARR(dset_outar,kk) ;
       nxyz = DSET_NVOX(dset_ooo) ;
       for( iv=0 ; iv < DSET_NVALS(dset_ooo) ; iv++ ){
         if( DSET_BRICK_TYPE(dset_ooo,iv) == MRI_float ){
           far = (float *)DSET_ARRAY(dset_ooo,iv) ;
           sar = (short *)malloc(sizeof(short)*nxyz) ;
           EDIT_coerce_type( nxyz , MRI_float,far , MRI_short,sar ) ;
           EDIT_substitute_brick( dset_ooo , iv , MRI_short,sar ) ;
           if( iv == 0 )
             INFO_message("Converting %s to shorts",DSET_HEADNAME(dset_ooo)) ;
         } else {
           if( iv == 0 )
             INFO_message("Cannot convert non-float %s to shorts",DSET_HEADNAME(dset_ooo)) ;
         }
       }
     }
   }

   /*-- atlas space? --*/

   if( dset_mast != NULL && dset_mast->atlas_space != NULL ){
     for( kk=0 ; kk < dset_outar->num ; kk++ ){
       dset_ooo = DSET_IN_3DARR(dset_outar,kk) ;
       MCW_strncpy( dset_ooo->atlas_space , dset_mast->atlas_space , THD_MAX_NAME ) ;
     }
   }

   /*-- output! --*/

   for( kk=0 ; kk < dset_outar->num ; kk++ ){
     dset_sss = DSET_IN_3DARR(dset_srcar,kk) ;
     dset_ooo = DSET_IN_3DARR(dset_outar,kk) ;
     tross_Copy_History( dset_sss , dset_ooo ) ;        /* hysterical records */
     tross_Make_History( "3dNwarpApply" , argc,argv , dset_ooo ) ;
     DSET_write(dset_ooo) ; if( verb ) WROTE_DSET(dset_ooo) ;
     DSET_delete(dset_ooo) ; DSET_delete(dset_sss) ;
   }

   /*-- exeunt omnes, pursued by a bear --*/

   if( verb ){
     double cput=COX_cpu_time() , clkt=COX_clock_time() ;
     if( cput >= 0.1 && clkt >= 0.1 )
       INFO_message("total CPU time = %.1f sec  Elapsed = %.1f\n",cput,clkt) ;
     else if( clkt >= 0.1 )
       INFO_message("total Elapsed time = %.1f sec\n",clkt) ;
   }

   exit(0) ;
}
