#include "mrilib.h"
#include <time.h>
#include <sys/types.h>
#include <unistd.h>

#define MAXPAR   99
#define PARC_FIX 1
#define PARC_INI 2
#define PARC_RAN 3
typedef struct { int np,code; float vb,vt ; } param_opt ;

#define WARP_SHIFT    1
#define WARP_ROTATE   2
#define WARP_SCALE    3
#define WARP_AFFINE   4

MRI_IMAGE * mri_weightize( MRI_IMAGE *im ) ;  /* prototype: function at end */

/*---------------------------------------------------------------------------*/

int main( int argc , char *argv[] )
{
   THD_3dim_dataset *dset_out ;
   MRI_IMAGE *im_base , *im_targ , *im_out , *im_weig=NULL , *im_mask=NULL ;
   GA_setup stup ;
   int iarg , ii,jj,kk , nmask=0 ;
   int   nx_base,ny_base,nz_base , nx_targ,ny_targ,nz_targ ;
   float dx_base,dy_base,dz_base , dx_targ,dy_targ,dz_targ ;
   int nvox_base ;
   float v1,v2 , xxx,yyy,zzz ;

   /*----- input parameters, to be filled in from the options -----*/

   THD_3dim_dataset *dset_base = NULL ;
   THD_3dim_dataset *dset_targ = NULL ;
   THD_3dim_dataset *dset_mast = NULL ;
   THD_3dim_dataset *dset_weig = NULL ;
   int auto_weight             = 0 ;
   float dxyz_mast             = 0.0f ;
   int meth_code               = GA_MATCH_CORRATIO_SCALAR ;
   int sm_code                 = GA_SMOOTH_GAUSSIAN ;
   float sm_rad                = 0.0f ;
   int twopass                 = 0 ;
   int verb                    = 0 ;
   char *prefix                = NULL ;
   char *fname_1D              = NULL ;
   int interp_code             = MRI_LINEAR ;
   int npt_match               = -666 ;
   int final_interp            = -1 ;
   int warp_code               = WARP_AFFINE ;
   int nparopt                 = 0 ;
   MRI_IMAGE *matini           = NULL ;
   param_opt paropt[MAXPAR] ;

   /**----- Help the pitifully ignorant user? -----**/

   if( argc < 2 || strcmp(argv[1],"-help") == 0 ){
     printf(
       "Usage: 3dAllineate [options] targetdataset\n"
       "\n"
       "Program to align one dataset (the 'target') to a base dataset.\n"
       "Options are available to control:\n"
       " + How the matching between the target and the base is computed.\n"
       " + How the resliced target is interpolated to the base space.\n"
       " + The complexity of the spatial transformation ('warp') used.\n"
       " + And many technical options to control the process.\n"
       "\n"
       "OPTIONS:\n"
       "=======\n"
       " -base bbb   = Set the base dataset to be the #0 sub-brick of 'bbb'.\n"
       "               If no -base option is given, then the base volume is\n"
       "               taken to be the #0 sub-brick of the target dataset.\n"
       "\n"
       " -target ttt = Read the target dataset from 'ttt'.  If no -target\n"
       "   *OR*        (or -input) option is given, then the target dataset\n"
       " -input ttt    is the last argument on the command line.\n"
       "\n"
       "  ** NOTA BENE: The base and target dataset do NOT have to be defined\n"
       "  **            on the same grids; the alignment process uses the\n"
       "  **            coordinate systems defined in the dataset headers to\n"
       "  **            make the match between spatial locations.\n"
       "\n"
       " -prefix ppp = Output the resulting dataset to file 'ppp'.  If this\n"
       "   *OR*        option is NOT given, no dataset will be output!  The\n"
       " -out ppp      transformation to align the target to the base will\n"
       "               be estimated, but not applied.\n"
       "\n"
       " -1Dfile fff = Save the warp parameters in ASCII (.1D) format into\n"
       "               file 'fff'.\n"
       "\n"
       " -cost ccc   = Defines the 'cost' function that defines the matching\n"
       "               between the target and the base; 'ccc' is one of\n"
       "                 mi *OR* mutualinfo  = Mutual Information\n"
       "                 cr *OR* corratio    = Correlation Ratio\n"
       "                 ls *OR* leastsq     = Least Squares\n"
       "                 sp *OR* spearman    = Spearman (rank) Correlation\n"
       "               You can also specify the cost function using an option\n"
       "               of the form '-mi' rather than '-cost mi', if you like\n"
       "               to keep things terse and cryptic (as I do).\n"
       "               [Default == 'corratio'.]\n"
       "\n"
       " -interp iii = Defines interpolation method to use during matching\n"
       "               process, where 'iii' is one of\n"
       "                 NN      *OR* nearestneighbour\n"
       "                 linear  *OR* trilinear\n"
       "                 cubic   *OR* tricubic\n"
       "                 quintic *OR* triquintic\n"
       "               Using '-NN' instead of '-interp NN' is allowed (e.g.).\n"
       "               [Default == 'linear'.]\n"
       "\n"
       "TECHNICAL OPTIONS (used for fine control of the program):\n"
       "=================\n"
       " -nmatch nnn = Use at most 'nnn' scattered points to match the\n"
       "               datasets.  The smaller nnn is, the faster the matching\n"
       "               algorithm will run; however, accuracy may be bad if\n"
       "               nnn is too small.  If you end the 'nnn' value with the\n"
       "               '%%' character, then that percentage of the base's\n"
       "               voxels will be used.\n"
       "               [Default == smaller of 20%% of voxels and 66,666.]\n"
       "\n"
       " -final iii  = Defines the interpolation mode used to create the\n"
       "               output dataset.  [Default == whatever '-interp' used.]\n"
       "\n"
       " -verb       = Print out verbose progress reports.\n"
       "\n"
       " -twopass    = Use a two pass alignment strategy, first searching\n"
       "               for a large rotation+shift and then refining the\n"
       "               alignment.  [Default == use only the refining pass.]\n"
       " -twoblur rr = Set the blurring radius for the first pass to 'rr'\n"
       "               millimeters.  [Default == 5%% of dataset size.]\n"
#if 0
       "\n"
       " -weight www = Set the weighting for each voxel in the base dataset;\n"
       "               larger weights mean that voxel counts more in the cost\n"
       "               function.  [Default == all voxels weighted equally.]\n"
       " -autoweight = Compute a weight function using the 3dAutomask algorithm\n"
       "               plus some blurring.\n"
#endif
       "\n"
       " -warp xxx   = Set the warp type to 'xxx', which is one of\n"
       "                 shift_only         *OR* sho =  3 parameters\n"
       "                 shift_rotate       *OR* shr =  6 parameters\n"
       "                 shift_rotate_scale *OR* srs =  9 parameters\n"
       "                 affine_general     *OR* aff = 12 parameters\n"
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
       " -matini mmm   = Initialize 3x4 affine transformation matrix to 'mmm',\n"
       "                 which is either a .1D file or an expression in the\n"
       "                 syntax of program 1dmatcalc.  Using this option is\n"
       "                 like using '-parini' on all affine matrix parameters.\n"
       "\n"
       " -master mmm = Write the output dataset on the same grid as dataset\n"
       "               'mmm'.  If this option is NOT given, the base dataset\n"
       "               is the master.\n"
       " -dxyz del   = Write the output dataset using grid spacings of\n"
       "               'del' mm.  If this option is NOT given, then the\n"
       "               grid spacings in the master dataset will be used.\n"
     ) ;

     printf(
      "\n"
      "---------------------------------\n"
      "AFFINE TRANSFORMATION PARAMETERS:\n"
      "---------------------------------\n"
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
      " The shift vector comprises parameters #1, #2, and #3.\n"
      "\n"
      " The goal of the program is to find the warp parameters such that\n"
      "   I([x]_warped) 'is similar to' J([x]_in)\n"
      " as closely as possible in some sense, where J(x) is the base\n"
      " image, and I(x) is the target image.\n"
      "\n"
      " Using '-parfix', you can specify that some of these parameters\n"
      " are fixed.  For example, '-shift_rotate_scale' is equivalent\n"
      " '-affine_general -parfix 10 0 -parfix 11 0 -parfix 12 0'.\n"
      " Don't even think of using the '-parfix' option unless you grok\n"
      " this example!\n"
     ) ;

     printf(
      "\n"
      "--------------------------\n"
      "  RWCox - September 2006\n"
      "--------------------------\n"
      "-- From Webster's Dictionary: Allineate == 'to align'\n\n"
     ) ;

     exit(0);
   }

   /**--- bookkeeping and marketing ---**/

#ifdef USING_MCW_MALLOC
   enable_mcw_malloc() ;
#endif

   mainENTRY("3dAllineate"); machdep();
   AFNI_logger("3dAllineate",argc,argv);
   PRINT_VERSION("3dAllineate"); AUTHOR("RW Cox");
   THD_check_AFNI_version("3dAllineate");
   srand48((long)time(NULL)+(long)getpid()) ;

   /**--- process command line options ---**/

   iarg = 1 ;
   while( iarg < argc && argv[iarg][0] == '-' ){

INFO_message("Processing argument #%d '%s'",iarg,argv[iarg]) ;

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

     if( strncmp(argv[iarg],"-master",5) == 0 ){
       if( dset_mast != NULL ) ERROR_exit("Can't have multiple %s options!",argv[iarg]) ;
       if( ++iarg >= argc ) ERROR_exit("no argument after '%s'!",argv[iarg-1]) ;
       dset_mast = THD_open_dataset( argv[iarg] ) ;
       if( dset_mast == NULL ) ERROR_exit("can't open -master dataset '%s'",argv[iarg]);
       iarg++ ; continue ;
     }

     /*-----*/

#if 0
     if( strncmp(argv[iarg],"-weight",5) == 0 ){
       if( auto_weight ) ERROR_exit("Can't use -autoweight AND -weight!") ;
       if( dset_weig != NULL ) ERROR_exit("Can't have multiple %s options!",argv[iarg]) ;
       if( ++iarg >= argc ) ERROR_exit("no argument after '%s'!",argv[iarg-1]) ;
       dset_weig = THD_open_dataset( argv[iarg] ) ;
       if( dset_weig == NULL ) ERROR_exit("can't open -weight dataset '%s'",argv[iarg]);
       iarg++ ; continue ;
     }

     /*-----*/

     if( strncmp(argv[iarg],"-autoweight",6) == 0 ){
       if( dset_weig != NULL ) ERROR_exit("Can't use -autoweight AND -weight!") ;
       auto_weight = 1 ; iarg++ ; continue ;
     }
#endif

     /*-----*/

     if( strncmp(argv[iarg],"-verb",5) == 0 ){
       verb++ ; iarg++ ; continue ;
     }

     /*-----*/

     if( strncmp(argv[iarg],"-sp",3) == 0 ){
       meth_code = GA_MATCH_SPEARMAN_SCALAR ; iarg++ ; continue ;
     }
     if( strcmp(argv[iarg],"-cr") == 0 || strncmp(argv[iarg],"-corratio",5) == 0 ){
       meth_code = GA_MATCH_CORRATIO_SCALAR ; iarg++ ; continue ;
     }
     if( strcmp(argv[iarg],"-mi") == 0 || strncmp(argv[iarg],"-mutualinfo",5) == 0 ){
       meth_code = GA_MATCH_KULLBACK_SCALAR ; iarg++ ; continue ;
     }
     if( strcmp(argv[iarg],"-ls") == 0 || strncmp(argv[iarg],"-leastsq",5) == 0 ){
       meth_code = GA_MATCH_PEARSON_SCALAR ; iarg++ ; continue ;
     }
     if( strncmp(argv[iarg],"-cost",4) == 0 ){
       if( ++iarg >= argc ) ERROR_exit("no argument after '-cost'!") ;
       if( strncmp(argv[iarg],"sp",2) == 0 )
         meth_code = GA_MATCH_SPEARMAN_SCALAR ;
       else if( strcmp(argv[iarg],"cr") == 0 || strncmp(argv[iarg],"corratio",4) == 0 )
         meth_code = GA_MATCH_CORRATIO_SCALAR ;
       else if( strcmp(argv[iarg],"mi") == 0 || strncmp(argv[iarg],"mutualinfo",4) == 0 )
         meth_code = GA_MATCH_KULLBACK_SCALAR ;
       else if( strcmp(argv[iarg],"ls") == 0 || strncmp(argv[iarg],"leastsq",4) == 0 )
         meth_code = GA_MATCH_PEARSON_SCALAR ;
       else
         ERROR_exit("Unknown code '%s' after -cost!",argv[iarg]) ;
       iarg++ ; continue ;
     }

     /*-----*/

     if( strcmp(argv[iarg],"-base") == 0 ){
       if( dset_base != NULL ) ERROR_exit("Can't have multiple %s options!",argv[iarg]) ;
       if( ++iarg >= argc ) ERROR_exit("no argument after '-base'!") ;
       dset_base = THD_open_dataset( argv[iarg] ) ;
       if( dset_base == NULL ) ERROR_exit("can't open -base dataset '%s'",argv[iarg]);
INFO_message("Opened -base %s",argv[iarg]) ;
       iarg++ ; continue ;
     }

     /*-----*/

     if( strncmp(argv[iarg],"-target",5) == 0 || strncmp(argv[iarg],"-input",3) == 0 ){
       if( dset_targ != NULL ) ERROR_exit("Can't have multiple %s options!",argv[iarg]) ;
       if( ++iarg >= argc ) ERROR_exit("no argument after '%s'!",argv[iarg-1]) ;
       dset_targ = THD_open_dataset( argv[iarg] ) ;
       if( dset_targ == NULL )
         ERROR_exit("can't open -%s dataset '%s'",argv[iarg-1],argv[iarg]);
INFO_message("Opened -targ %s",argv[iarg]) ;
       iarg++ ; continue ;
     }

     /*-----*/

     if( strncmp(argv[iarg],"-median",4) == 0 ){        /* not documented */
       sm_code = GA_SMOOTH_MEDIAN ; iarg++ ; continue ;
     }

     /*-----*/

     if( strncmp(argv[iarg],"-twoblur",5) == 0 ){
       if( ++iarg >= argc ) ERROR_exit("no argument after '%s'!",argv[iarg-1]) ;
       sm_rad = (float)strtod(argv[iarg],NULL) ; twopass = 1 ; iarg++ ; continue ;
     }

     /*-----*/

     if( strncmp(argv[iarg],"-twopass",5) == 0 ){
       twopass = 1 ; iarg++ ; continue ;
     }

     /*-----*/

     if( strncmp(argv[iarg],"-output",4) == 0 || strncmp(argv[iarg],"-prefix",5) == 0 ){
       if( prefix != NULL ) ERROR_exit("Can't have multiple %s options!",argv[iarg]) ;
       if( ++iarg >= argc ) ERROR_exit("no argument after '%s'!",argv[iarg-1]) ;
       if( !THD_filename_ok(argv[iarg]) )
         ERROR_exit("badly formed filename: '%s' '%s'",argv[iarg-1],argv[iarg]) ;
       prefix = argv[iarg] ; iarg++ ; continue ;
     }

     /*-----*/

     if( strncmp(argv[iarg],"-1Dfile",4) == 0 ){
       if( fname_1D != NULL ) ERROR_exit("Can't have multiple %s options!",argv[iarg]) ;
       if( ++iarg >= argc ) ERROR_exit("no argument after '%s'!",argv[iarg-1]) ;
       if( !THD_filename_ok(argv[iarg]) )
         ERROR_exit("badly formed filename: '%s' '%s'",argv[iarg-1],argv[iarg]) ;
       fname_1D = argv[iarg] ; iarg++ ; continue ;
     }

     /*-----*/

     if( strcmp(argv[iarg],"-NN") == 0 || strncmp(argv[iarg],"-nearest",5) == 0 ){
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
       if( strcmp(argv[iarg],"NN")==0 || strncmp(argv[iarg],"nearest",4)==0 )
         interp_code = MRI_NN ;
       if( strncmp(argv[iarg],"linear",3)==0 || strncmp(argv[iarg],"trilinear",5)==0 )
         interp_code = MRI_LINEAR ;
       if( strncmp(argv[iarg],"cubic",3)==0 || strncmp(argv[iarg],"tricubic",5)==0 )
         interp_code = MRI_CUBIC ;
       if( strncmp(argv[iarg],"quintic",3)==0 || strncmp(argv[iarg],"triquintic",5)==0 )
         interp_code = MRI_QUINTIC ;
       else
         ERROR_exit("Unknown code '%s' after '%s'!",argv[iarg],argv[iarg-1]) ;
       iarg++ ; continue ;
     }
     if( strncmp(argv[iarg],"-final",5) == 0 ){
       if( ++iarg >= argc ) ERROR_exit("no argument after '%s'!",argv[iarg-1]) ;
       if( strcmp(argv[iarg],"NN") == 0 || strncmp(argv[iarg],"nearest",4) == 0 )
         final_interp = MRI_NN ;
       if( strncmp(argv[iarg],"linear",3) == 0 || strncmp(argv[iarg],"trilinear",5) == 0 )
         final_interp = MRI_LINEAR ;
       if( strncmp(argv[iarg],"cubic",3) == 0 || strncmp(argv[iarg],"tricubic",5) == 0 )
         final_interp = MRI_CUBIC ;
       if( strncmp(argv[iarg],"quintic",3)==0 || strncmp(argv[iarg],"triquintic",5)==0 )
         final_interp = MRI_QUINTIC ;
       else
         ERROR_exit("Unknown code '%s' after '%s'!",argv[iarg],argv[iarg-1]) ;
       iarg++ ; continue ;
     }

     /*-----*/

     if( strncmp(argv[iarg],"-nmatch",5) == 0 ){
       char *cpt ;
       if( ++iarg >= argc ) ERROR_exit("no argument after '%s'!",argv[iarg-1]) ;
       npt_match = (int)strtod(argv[iarg],&cpt) ;
       if( npt_match <= 0 )
         ERROR_exit("Illegal value '%s' after '%s'!",argv[iarg],argv[iarg-1]) ;
       if( *cpt == '%' ) npt_match = -npt_match ;  /* signal for % */
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

     ERROR_exit("Unknown and Illegal option '%s'",argv[iarg]) ;
   }

   /*--- check inputs for validity, consistency, and moral fibre ---*/

   if( dset_targ == NULL ){
     if( iarg >= argc )
       ERROR_exit("no target datset on command line!?") ;
     dset_targ = THD_open_dataset( argv[iarg] ) ;
     if( dset_targ == NULL )
       ERROR_exit("Can't open target dataset '%s'",argv[iarg]) ;
   }

   if( dset_base == NULL ){
     if( DSET_NVALS(dset_targ) == 1 )
       ERROR_exit("No -base dataset and target has only 1 sub-brick") ;
     else
       WARNING_message("No -base dataset: using sub-brick #0 of target") ;
   }

   if( prefix == NULL )
     WARNING_message("No output dataset will be calculated!") ;

   if( final_interp < 0 ) final_interp = interp_code ;

   /*--- load input datasets ---*/

   if( verb ) INFO_message("Loading datasets") ;

   /* target must be present */

   DSET_load(dset_targ) ;
   if( !DSET_LOADED(dset_targ) ) ERROR_exit("Can't load target dataset") ;
   nx_targ = DSET_NX(dset_targ) ; dx_targ = fabs(DSET_DX(dset_targ)) ;
   ny_targ = DSET_NY(dset_targ) ; dy_targ = fabs(DSET_DY(dset_targ)) ;
   nz_targ = DSET_NZ(dset_targ) ; dz_targ = fabs(DSET_DZ(dset_targ)) ;

   /* load base if defined */

   if( dset_base != NULL ){
     DSET_load(dset_base) ;
     if( !DSET_LOADED(dset_base) ) ERROR_exit("Can't load base dataset") ;
     im_base = mri_scale_to_float( DSET_BRICK_FACTOR(dset_base,0) ,
                                   DSET_BRICK(dset_base,0)         ) ;
     DSET_unload(dset_base) ;
     dx_base = fabs(DSET_DX(dset_base)) ;
     dy_base = fabs(DSET_DY(dset_base)) ;
     dz_base = fabs(DSET_DZ(dset_base)) ;
   } else {
     im_base = mri_scale_to_float( DSET_BRICK_FACTOR(dset_targ,0) ,
                                   DSET_BRICK(dset_targ,0)         ) ;
     dx_base = dx_targ; dy_base = dy_targ; dz_base = dz_targ;
   }
   nx_base = im_base->nx ;
   ny_base = im_base->ny ;
   nz_base = im_base->nz ; nvox_base = nx_base*ny_base*nz_base ;

   /* check for base:target dimensionality mismatch */

   if( nz_base >  1 && nz_targ == 1 )
     ERROR_exit("Can't register 2D target into 3D base!") ;
   if( nz_base == 1 && nz_targ >  1 )
     ERROR_exit("Can't register 3D target into 2D base!") ;

   /* load weight dataset if defined */

   if( dset_weig != NULL ){
     DSET_load(dset_weig) ;
     if( !DSET_LOADED(dset_weig) ) ERROR_exit("Can't load weight dataset") ;
     im_weig = mri_scale_to_float( DSET_BRICK_FACTOR(dset_weig,0) ,
                                   DSET_BRICK(dset_weig,0)         ) ;
     DSET_unload(dset_weig) ;

   } else if( auto_weight ){  /* manufacture weight from the base */
     if( verb ) INFO_message("Computing -autoweight") ;
     im_weig = mri_weightize( im_base ) ;
   }

   if( im_weig != NULL ){
     float *wf = MRI_FLOAT_PTR(im_weig) ;
     byte  *mf ;
     im_mask = mri_new_conforming(im_weig,MRI_byte) ;
     mf = MRI_BYTE_PTR(im_mask) ;
     for( ii=0 ; ii < im_mask->nvox ; ii++ ) mf[ii] = (wf[ii] > 0.0f) ;
     nmask = THD_countmask(im_mask->nvox,mf) ;
     if( verb ) INFO_message("%d voxels in weight mask",nmask) ;
   } else {
     nmask = nvox_base ;
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

   /*--- setup alignment structure parameters ---*/

   memset(&stup,0,sizeof(GA_setup)) ;

   stup.match_code = meth_code ;

   /* spatial coordinates */

   stup.use_cmat  = 1 ;
   if( !ISVALID_MAT44(dset_targ->daxes->ijk_to_dicom) )
     THD_daxes_to_mat44(dset_targ->daxes) ;
   stup.targ_cmat = dset_targ->daxes->ijk_to_dicom ;

   if( dset_base != NULL ){
     if( !ISVALID_MAT44(dset_base->daxes->ijk_to_dicom) )
     THD_daxes_to_mat44(dset_base->daxes) ;
     stup.base_cmat = dset_base->daxes->ijk_to_dicom ;
   } else {
     stup.base_cmat = stup.targ_cmat ;
   }

   /* actual warp parameters */

   stup.wfunc       = mri_genalign_affine ;
   stup.wfunc_param = (GA_param *)calloc(12,sizeof(GA_param)) ;

   switch( warp_code ){
     case WARP_SHIFT:   stup.wfunc_numpar =  3 ;
     case WARP_ROTATE:  stup.wfunc_numpar =  6 ;
     case WARP_SCALE:   stup.wfunc_numpar =  9 ;
     case WARP_AFFINE:  stup.wfunc_numpar = 12 ;
   }

#define DEFPAR(p,nm,bb,tt,id,dd,ll)               \
 do{ stup.wfunc_param[p].min      = (bb) ;        \
     stup.wfunc_param[p].max      = (tt) ;        \
     stup.wfunc_param[p].delta    = (dd) ;        \
     stup.wfunc_param[p].toler    = (ll) ;        \
     stup.wfunc_param[p].ident    = (id) ;        \
     stup.wfunc_param[p].val_init = (id) ;        \
     strcpy( stup.wfunc_param[p].name , (nm) ) ;  \
     stup.wfunc_param[p].fixed = 0 ;              \
 } while(0)

   xxx = 0.333 * nx_base * dx_base ;
   yyy = 0.444 * ny_base * dy_base ;
   zzz = 0.444 * nz_base * dz_base ;

   DEFPAR( 0, "x-shift" , -xxx , xxx , 0.0 , 0.0 , 0.0 ) ;
   DEFPAR( 1, "y-shift" , -yyy , yyy , 0.0 , 0.0 , 0.0 ) ;
   DEFPAR( 2, "z-shift" , -zzz , zzz , 0.0 , 0.0 , 0.0 ) ;

   DEFPAR( 3, "z-angle" , -30.0 , 30.0 , 0.0 , 0.0 , 0.0 ) ;  /* degrees */
   DEFPAR( 4, "x-angle" , -30.0 , 30.0 , 0.0 , 0.0 , 0.0 ) ;
   DEFPAR( 5, "y-angle" , -30.0 , 30.0 , 0.0 , 0.0 , 0.0 ) ;

   DEFPAR( 6, "x-scale" , 0.618  , 1.618 , 1.0 , 0.0 , 0.0 ) ;  /* identity */
   DEFPAR( 7, "y-scale" , 0.618  , 1.618 , 1.0 , 0.0 , 0.0 ) ;  /*  == 1.0 */
   DEFPAR( 8, "z-scale" , 0.618  , 1.618 , 1.0 , 0.0 , 0.0 ) ;

   DEFPAR(  9, "y/x-shear" , -0.1666 , 0.1666 , 0.0 , 0.0 , 0.0 ) ;
   DEFPAR( 10, "z/x-shear" , -0.1666 , 0.1666 , 0.0 , 0.0 , 0.0 ) ;
   DEFPAR( 11, "z/y-shear" , -0.1666 , 0.1666 , 0.0 , 0.0 , 0.0 ) ;

   if( im_base->nz == 1 ){             /* 2D images */
     stup.wfunc_param[ 2].fixed = 2 ;  /* fixed==2 means cannot be un-fixed */
     stup.wfunc_param[ 4].fixed = 2 ;  /* fixed==1 is 'temporarily fixed'   */
     stup.wfunc_param[ 5].fixed = 2 ;
     stup.wfunc_param[ 8].fixed = 2 ;
     stup.wfunc_param[10].fixed = 2 ;
     stup.wfunc_param[11].fixed = 2 ;
   }

   for( ii=0 ; ii < nparopt ; ii++ ){
     jj = paropt[ii].np ;
     if( jj < stup.wfunc_numpar ){
       if( stup.wfunc_param[jj].fixed ){
         WARNING_message("Can't alter parameter #%d: it is fixed!",jj+1) ;
       } else {
         switch( paropt[ii].code ){
           case PARC_FIX: stup.wfunc_param[jj].fixed     = 2 ;
                          stup.wfunc_param[jj].val_fixed = paropt[ii].vb; break;
           case PARC_INI: stup.wfunc_param[jj].fixed     = 0 ;
                          stup.wfunc_param[jj].val_init  = paropt[ii].vb; break;
           case PARC_RAN: stup.wfunc_param[jj].fixed     = 0 ;
                          stup.wfunc_param[jj].min       = paropt[ii].vb;
                          stup.wfunc_param[jj].max       = paropt[ii].vt; break;
         }
       }
     } else {
       WARNING_message("Can't alter parameter #%d: out of range!",jj+1) ;
     }
   }

   /*** start alignment process ***/

#undef  PARDUMP
#define PARDUMP(ss) do{ fprintf(stderr," + %s Parameters =",ss) ;                 \
                        for( jj=0 ; jj < stup.wfunc_numpar ; jj++ )               \
                          fprintf(stderr," %.2f",stup.wfunc_param[jj].val_out) ;  \
                        fprintf(stderr,"\n") ;                                    \
                    } while(0)

   for( kk=0 ; kk < DSET_NVALS(dset_targ) ; kk++ ){

     im_targ = mri_scale_to_float( DSET_BRICK_FACTOR(dset_targ,kk) ,
                                   DSET_BRICK(dset_targ,kk)         ) ;
     DSET_unload_one(dset_targ,kk) ;

     if( verb ) INFO_message("===== Start on sub-brick #%d =====",kk) ;
     if( twopass ){
       if( verb ) INFO_message("Start coarse pass") ;
       stup.interp_code   = MRI_LINEAR ;
       stup.smooth_code   = sm_code ;
       stup.smooth_radius = (sm_rad == 0.0f) ? 11.111f : sm_rad ;
       stup.npt_match     = nmask / 20 ;
            if( stup.npt_match < 666       ) stup.npt_match = 666 ;
       else if( stup.npt_match > npt_match ) stup.npt_match = npt_match ;

       mri_genalign_scalar_setup( im_base , im_mask , im_targ , &stup ) ;

       if( verb ) ININFO_message("- Look for coarse starting parameters") ;

       /* startup search only allows rotations and shifts, so freeze all others */

       for( jj=7 ; jj < stup.wfunc_numpar ; jj++ )
         if( !stup.wfunc_param[ii].fixed ) stup.wfunc_param[ii].fixed = 1 ;

       mri_genalign_scalar_ransetup( &stup , 77 ) ;

       if( verb > 1 ) PARDUMP("Starting") ;

       /* unfreeze those that were temporarily frozen above */

       for( jj=7 ; jj < stup.wfunc_numpar ; jj++ )
         if( stup.wfunc_param[ii].fixed == 1 ) stup.wfunc_param[ii].fixed = 0 ;

       if( verb ) ININFO_message("- Start coarse optimization") ;

       stup.npt_match = nmask / 10 ;
            if( stup.npt_match < 666       ) stup.npt_match = 666 ;
       else if( stup.npt_match > npt_match ) stup.npt_match = npt_match ;
       mri_genalign_scalar_setup( NULL,NULL,NULL , &stup ) ;
       ii = mri_genalign_scalar_optim( &stup , 0.04 , 0.002 , 6666 ) ;

       if( verb ) ININFO_message("- Coarse optimization took %d trials",ii) ;
       if( verb > 1 ) PARDUMP("Final Coarse") ;

       for( jj=0 ; jj < stup.wfunc_numpar ; jj++ )
         stup.wfunc_param[jj].val_init = stup.wfunc_param[jj].val_out ;
     }

     if( verb ) ININFO_message("Start fine pass") ;
     stup.smooth_code   = 0  ;
     stup.smooth_radius = 0.0f ;
     stup.interp_code   = interp_code ;
     stup.npt_match     = npt_match ;
     mri_genalign_scalar_setup( NULL,NULL,NULL, &stup ) ;

     ii = mri_genalign_scalar_optim( &stup , 0.01 , 0.0001 , 6666 ) ;

     if( verb ) ININFO_message("- Fine Optimization took %d trials",ii) ;

     if( verb > 1 ) PARDUMP("Final Fine") ;

     mri_free(im_targ) ;
   }

   exit(0) ;
}

/*---------------------------------------------------------------------------*/
/*! Turn an input image into a weighting factor (for -autoweight).
-----------------------------------------------------------------------------*/

MRI_IMAGE * mri_weightize( MRI_IMAGE *im )
{
   float *wf,clip,clip2 ;
   int xfade,yfade,zfade , nx,ny,nz,nxy,nxyz , ii,jj,kk,ff ;
   byte *mmm ;
   MRI_IMAGE *qim , *wim ;

   /* copy input image */

   qim = mri_to_float(im) ; wf = MRI_FLOAT_PTR(qim) ;
   nx = qim->nx; ny = qim->ny; nz = qim->nz; nxy = nx*ny; nxyz = nxy*nz;
   for( ii=0 ; ii < nxyz ; ii++ ) wf[ii] = fabs(wf[ii]) ;

   /*-- zero out along the edges --*/
#undef  WW
#define WW(i,j,k) wf[(i)+(j)*nx+(k)*nxy]

   xfade = (int)(0.05*qim->nx+3.0) ;                 /* number of points */
   yfade = (int)(0.05*qim->ny+3.0) ;                 /* along each face */
   zfade = (int)(0.05*qim->nz+3.0) ;                 /* to set to zero */
   if( 4*xfade >= qim->nx ) xfade = (qim->nx-1)/4 ;
   if( 4*yfade >= qim->ny ) yfade = (qim->ny-1)/4 ;
   if( 3*zfade >= qim->nz ) zfade = (qim->nz-1)/4 ;
   for( jj=0 ; jj < ny ; jj++ )
    for( ii=0 ; ii < nx ; ii++ )
     for( ff=0 ; ff < zfade ; ff++ ) WW(ii,jj,ff) = WW(ii,jj,nz-1-ff) = 0.0f;
   for( kk=0 ; kk < nz ; kk++ )
    for( jj=0 ; jj < ny ; jj++ )
     for( ff=0 ; ff < xfade ; ff++ ) WW(ff,jj,kk) = WW(nx-1-ff,jj,kk) = 0.0f;
   for( kk=0 ; kk < nz ; kk++ )
    for( ii=0 ; ii < nx ; ii++ )
     for( ff=0 ; ff < yfade ; ff++ ) WW(ii,ff,kk) = WW(ii,ny-1-ff,kk) = 0.0f;

   /*-- blur a little: median then Gaussian;
          the idea is that the median filter smashes big spikes,
          then the Gaussian filter does some general smoothing.  --*/

   mmm = (byte *)malloc( sizeof(byte)*nxyz ) ;
   for( ii=0 ; ii < nxyz ; ii++ ) mmm[ii] = (wf[ii] > 0.0f) ; /* mask */
   wim = mri_medianfilter( qim , 1.415 , mmm , 0 ) ;
   mri_free(qim) ; wf = MRI_FLOAT_PTR(wim) ;

   FIR_blur_volume_3d( wim->nx , wim->ny , wim->nz ,
                       1.0f , 1.0f , 1.0f ,  wf ,
                       3.0f , 3.0f , 3.0f ) ;

   /*-- clip off small values, and
        keep only the largest cluster of supra threshold voxels --*/

   clip  = 0.05f * mri_max(wim) ;
   clip2 = 0.33f * THD_cliplevel(wim,0.33f) ;
   clip  = MAX(clip,clip2) ;
   for( ii=0 ; ii < nxyz ; ii++ ) mmm[ii] = (wf[ii] >= clip) ;
   THD_mask_clust( nx,ny,nz, mmm ) ;
   THD_mask_erode( nx,ny,nz, mmm, 1 ) ;  /* cf. thd_automask.c */
   THD_mask_clust( nx,ny,nz, mmm ) ;
   for( ii=0 ; ii < nxyz ; ii++ ) if( !mmm[ii] ) wf[ii] = 0.0f ;
   free((void *)mmm) ;

   return wim ;
}
