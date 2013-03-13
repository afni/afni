#include "mrilib.h"

#ifdef USE_OMP
#include <omp.h>
#include "mri_genalign_util.c"
#include "mri_genalign.c"
#include "mri_nwarp.c"
#endif

/*----------------------------------------------------------------------------*/

int main( int argc , char *argv[] )
{
   THD_3dim_dataset *dset_nwarp=NULL , *dset_src=NULL , *dset_mast=NULL ;
   MRI_IMARR *imar_nwarp=NULL , *im_src ;
   char *prefix     = NULL ;
   float dxyz_mast  = 0.0f ;
   int interp_code  = MRI_WSINC5 ;
   int ainter_code  = -666 ;
   int iarg , kk , verb=1 , iv , do_wmast=0 ;
   mat44 src_cmat, nwarp_cmat, mast_cmat ;
   THD_3dim_dataset *dset_out ;
   MRI_IMAGE *fim , *wim ; float *ip,*jp,*kp ;
   int nx,ny,nz,nxyz , toshort=0 ;
   float wfac=1.0f ;
   MRI_IMAGE *awim=NULL ;

   /**----------------------------------------------------------------------*/
   /**----------------- Help the pitifully ignorant user? -----------------**/

   if( argc < 2 || strcasecmp(argv[1],"-help") == 0 ){
     printf(
      "Usage: 3dNwarpApply [options] sourcedataset\n"
      "\n"
      "Program to apply a nonlinear 3D warp saved from 3dQwarp (or 3dNwarpCat, etc.)\n"
      "to a 3D dataset, to produce a warped version of the source dataset.\n"
      "\n"
      "OPTIONS:\n"
      "--------\n"
      " -nwarp  www  = 'www' is the name of the 3D warp dataset\n"
      "                (this is a mandatory option!)\n"
      "\n"
      " -affter aaa  = 'aaa' is the name of an optional file containing\n"
      "                an affine matrix to apply to the nonlinear warp,\n"
      "                as in A(N(x)), where N(x) is defined by '-nwarp'\n"
      "                and the 12 numbers that define A(x) come from\n"
      "                file 'aaa'.\n"
      "                ++ If the '-source' dataset has multiple sub-bricks,\n"
      "                   then the k-th sub-brick is transformed by the\n"
      "                   warp Ak(N(x)), where Ak is defined by the 12\n"
      "                   numbers on the k-th line of file 'aaa'.\n"
      "                ++ Note that 'aaa' must be formatted with 12 numbers in\n"
      "                   ONE line of the file giving a matrix -- that is, in\n"
      "                   the 'aff12.1D' format, NOT in the 'Xat.1D' 3x4 format!\n"
      "                ++ This ability can be used when warping EPI time\n"
      "                   series datasets, where the affine part would be\n"
      "                   different for each sub-brick (because of motion),\n"
      "                   and the nonlinear warp part (to a template) would\n"
      "                   be the same.\n"
      "                ++ Typically, 'aaa' would be a '-1Dmatrix_save'\n"
      "                   result from 3dAllineate or 3dWarpDrive or ....\n"
      "\n"
      " -wfac   fff  = Scale the warp by factor 'fff' [default=1.0]\n"
      "                ++ This option doesn't really have much use, except\n"
      "                   in making movies of how a brain volume deforms.\n"
      "\n"
      " -source sss  = 'sss' is the name of the source dataset\n"
      "                ++ That is, the dataset to be warped\n"
      "                ++ Or you can provide the source dataset name as the\n"
      "                   last argument on the command line.\n"
      "\n"
      " -master mmm  = 'mmm  is the name of the master dataset\n"
      "                ++ Which defines the output grid.\n"
      "                ++ If '-master' is not used, then output\n"
      "                   grid is the same as the source dataset grid.\n"
      "                ++ It is often the case that it makes more sense to\n"
      "                   use the '-nwarp' dataset as the master, since\n"
      "                   that is the grid on which the transformation is\n"
      "                   defined.  You can use '-master WARP' or '-master NWARP'\n"
      "                   for this purpose.\n"
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
      "                   not on the same 3D grid as the warp itself.  Otherwise,\n"
      "                   it won't need to be interpolated.\n"
      "\n"
      " -ainterp jjj = This option lets you specify a different interpolation mode\n"
      "                for the data than might be used for the warp.  In particular,\n"
      "                '-ainterp NN' would be most logical for atlas datasets, where\n"
      "                the data values being mapped are labels.\n"
      "\n"
      " -prefix ppp  = 'ppp' is the name of the new output dataset\n"
#if 0
      " -short       = Write output dataset using 16-bit short integers, rather than\n"
      "                the usual 32-bit floats.\n"
#endif
      "\n"
      " -quiet       = Don't be verbose :-(\n"
      " -verb        = Be extra verbose :-)\n"
      "\n"
      "NOTES:\n"
      "------\n"
      "* At present, this program doesn't work with 2D warps, only with 3D.\n"
      "\n"
      "* At present, the output dataset is stored in float format, no matter what\n"
      "  absurd data format the input dataset uses.\n"
      "\n"
      "* To make life both simpler and more complex, 3dNwarpApply allows you to\n"
      "  catenate warps directly on the command line, as if you used 3dNwarpCat\n"
      "  before running 3dNwarpApply.  For example:\n"
      "  ++ You have aligned dataset Fred+orig to MNI-affine space using @auto_tlrc,\n"
      "     giving matrix file Fred.Xaff12.1D\n"
      "  ++ Then you further aligned from MNI-affine to MNI-qwarp via 3dQwarp,\n"
      "     giving warp dataset Fred_WARP+tlrc\n"
      "  ++ You can combine the transformations and interpolate Fred+orig directly\n"
      "     to MNI-qwarp space using a command like\n"
      "        3dNwarpApply -prefix Fred_final    \\\n"
      "                     -source Fred+orig     \\\n"
      "                     -master NWARP         \\\n"
      "                     -nwarp 'Fred_WARP+tlrc Fred.Xaff12.1D'\n"
      "     Note the warps to be catenated are enclosed in quotes to make a single\n"
      "     input argument passed to the program.  The processing used for this\n"
      "     purpose is the same as in 3dNwarpCat -- see the help output for that\n"
      "     program for a little more information.  (You can use 'INV' here as well.)\n"
      "  ++ This simple example could also be accomplished with a command like\n"
      "        3dNwarpApply -prefix Fred_final    \\\n"
      "                     -source Fred+orig     \\\n"
      "                     -master NWARP         \\\n"
      "                     -nwarp  Fred_WARP+tlrc -affter Fred.Xaff12.1D\n"
     ) ;

     PRINT_AFNI_OMP_USAGE("3dNwarpApply",NULL) ; PRINT_COMPILE_DATE ;
     exit(0) ;
   }

   /**--- bookkeeping and marketing ---**/

   mainENTRY("3dNwarpApply"); machdep();
   AFNI_logger("3dNwarpApply",argc,argv);
   PRINT_VERSION("3dNwarpApply"); AUTHOR("Zhark the Warped");
   (void)COX_clock_time() ;

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

     /*---------------*/

     if( strcasecmp(argv[iarg],"-wfac") == 0 ){
       if( ++iarg >= argc ) ERROR_exit("No argument after '%s' :-(",argv[iarg-1]) ;
       wfac = (float)strtod(argv[iarg],NULL) ;
       if( wfac == 0.0f ) wfac = 1.0f ;
       iarg++ ; continue ;
     }

     /*---------------*/

     if( strcasecmp(argv[iarg],"-affter") == 0 || strcasecmp(argv[iarg],"-after") == 0 ){
       MRI_IMAGE *qim ;
       if( awim != NULL )   ERROR_exit("Can't have multiple %s options :-("        ,argv[iarg]  );
       if( ++iarg >= argc ) ERROR_exit("No argument after '%s' :-("                ,argv[iarg-1]);
       awim = mri_read_1D(argv[iarg]) ;
       if( awim == NULL )   ERROR_exit("Can't read affine warp data from file '%s'",argv[iarg]  );
       if( awim->nx == 3 && awim->ny == 4 ){  /* in 3x4 'Xat.1D' format? */
         qim = mri_transpose(awim) ; mri_free(awim) ; awim = qim ;
         awim->nx = 12 ; awim->ny = 1 ;       /* make it look like a 1x12 array */
       } else {                               /* should be in N x 12 format */
              if( awim->ny < 12 )
                ERROR_exit("Affine warp file '%s': fewer than 12 values per row"     ,argv[iarg]  );
         else if( awim->ny > 12 )
                WARNING_message("Affine warp file '%s': more than 12 values per row" ,argv[iarg]  );
         qim = mri_transpose(awim) ; mri_free(awim) ; awim = qim ;  /* flip to column major order */
       }
       iarg++ ; continue ;
     }

     /*---------------*/

     if( strncasecmp(argv[iarg],"-prefix",5) == 0 ){
       if( prefix != NULL ) ERROR_exit("Can't have multiple %s options :-(",argv[iarg]) ;
       if( ++iarg >= argc ) ERROR_exit("No argument after '%s' :-(",argv[iarg-1]) ;
       if( !THD_filename_ok(argv[iarg]) )
         ERROR_exit("badly formed filename: '%s' '%s' :-(",argv[iarg-1],argv[iarg]) ;
       if( strcasecmp(argv[iarg],"NULL") == 0 )
         ERROR_exit("can't use filename: '%s' '%s' :-(",argv[iarg-1],argv[iarg]) ;
       else
         prefix = strdup(argv[iarg]) ;
       iarg++ ; continue ;
     }

     /*---------------*/

     if( strcasecmp(argv[iarg],"-nwarp") == 0 || strcasecmp(argv[iarg],"-warp") == 0 ){
       if( dset_nwarp != NULL ) ERROR_exit("Can't have multiple %s options :-(",argv[iarg]) ;
       if( ++iarg >= argc ) ERROR_exit("No argument after '%s' :-(",argv[iarg-1]) ;
#if 0
       dset_nwarp = THD_open_dataset( argv[iarg] ) ;
#else
       dset_nwarp = IW3D_read_catenated_warp( argv[iarg] ) ;
#endif
       if( dset_nwarp == NULL ) ERROR_exit("can't open warp dataset '%s' :-(",argv[iarg]);
       if( DSET_NVALS(dset_nwarp) < 3 ) ERROR_exit("dataset '%s' isn't a 3D warp",argv[iarg]);
       iarg++ ; continue ;
     }

     /*---------------*/

     if( strcasecmp(argv[iarg],"-source") == 0 ){
       if( dset_src != NULL ) ERROR_exit("Can't have multiple %s options :-(",argv[iarg]) ;
       if( ++iarg >= argc ) ERROR_exit("No argument after '%s' :-(",argv[iarg-1]) ;
       dset_src = THD_open_dataset( argv[iarg] ) ;
       if( dset_src == NULL ) ERROR_exit("can't open -source dataset '%s' :-(",argv[iarg]);
       iarg++ ; continue ;
     }

     /*---------------*/

     if( strcasecmp(argv[iarg],"-master") == 0 ){
       if( dset_mast != NULL ) ERROR_exit("Can't have multiple %s options :-(",argv[iarg]) ;
       if( ++iarg >= argc ) ERROR_exit("No argument after '%s' :-(",argv[iarg-1]) ;
       if( strcmp(argv[iarg],"WARP") == 0 || strcmp(argv[iarg],"NWARP") == 0 ){
         do_wmast = 1 ;
       } else {
         dset_mast = THD_open_dataset( argv[iarg] ) ;
         if( dset_mast == NULL ) ERROR_exit("can't open -master dataset '%s' :-(",argv[iarg]);
       }
       iarg++ ; continue ;
     }

     /*---------------*/

     if( strcasecmp(argv[iarg],"-mast_dxyz") == 0 ||
         strcasecmp(argv[iarg],"-dxyz_mast") == 0 ||
         strcasecmp(argv[iarg],"-dxyz")      == 0 ||
         strcasecmp(argv[iarg],"-newgrid"  ) == 0   ){

       if( ++iarg >= argc ) ERROR_exit("No argument after '%s' :-(",argv[iarg-1]) ;
       dxyz_mast = (float)strtod(argv[iarg],NULL) ;
       if( dxyz_mast <= 0.0f )
         ERROR_exit("Illegal value '%s' after -mast_dxyz :-(",argv[iarg]) ;
       if( dxyz_mast <= 0.5f )
         WARNING_message("Small value %g after -mast_dxyz :-(",dxyz_mast) ;
       iarg++ ; continue ;
     }

     /*---------------*/

     if( strcasecmp(argv[iarg],"-NN") == 0 || strncasecmp(argv[iarg],"-nearest",6) == 0 ){
       interp_code = MRI_NN ; iarg++ ; continue ;
     }
     if( strncasecmp(argv[iarg],"-linear",4)==0 || strncasecmp(argv[iarg],"-trilinear",6)==0 ){
       interp_code = MRI_LINEAR ; iarg++ ; continue ;
     }
     if( strncasecmp(argv[iarg],"-cubic",4)==0 || strncasecmp(argv[iarg],"-tricubic",6)==0 ){
       interp_code = MRI_CUBIC ; iarg++ ; continue ;
     }
     if( strncasecmp(argv[iarg],"-quintic",4)==0 || strncasecmp(argv[iarg],"-triquintic",6)==0 ){
       interp_code = MRI_QUINTIC ; iarg++ ; continue ;
     }
     if( strncasecmp(argv[iarg],"-wsinc",5) == 0 ){
       interp_code = MRI_WSINC5 ; iarg++ ; continue ;
     }
     if( strncasecmp(argv[iarg],"-interp",5)==0 ){
       char *inam ;
       if( ++iarg >= argc ) ERROR_exit("No argument after '%s' :-(",argv[iarg-1]) ;
       inam = argv[iarg] ; if( *inam == '-' ) inam++ ;
       if( strcasecmp(inam,"NN")==0 || strncasecmp(inam,"nearest",5)==0 )
         interp_code = MRI_NN ;
       else
       if( strncasecmp(inam,"linear",3)==0 || strncasecmp(inam,"trilinear",5)==0 )
         interp_code = MRI_LINEAR ;
       else
       if( strncasecmp(inam,"cubic",3)==0 || strncasecmp(inam,"tricubic",5)==0 )
         interp_code = MRI_CUBIC ;
       else
       if( strncasecmp(inam,"quintic",3)==0 || strncasecmp(inam,"triquintic",5)==0 )
         interp_code = MRI_QUINTIC ;
       else
       if( strncasecmp(inam,"WSINC",5)==0 )
         interp_code = MRI_WSINC5 ;
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
       if( strncasecmp(inam,"linear",3)==0 || strncasecmp(inam,"trilinear",5)==0 )
         ainter_code = MRI_LINEAR ;
       else
       if( strncasecmp(inam,"cubic",3)==0 || strncasecmp(inam,"tricubic",5)==0 )
         ainter_code = MRI_CUBIC ;
       else
       if( strncasecmp(inam,"quintic",3)==0 || strncasecmp(inam,"triquintic",5)==0 )
         ainter_code = MRI_QUINTIC ;
       else
       if( strncasecmp(inam,"WSINC",5)==0 )
         ainter_code = MRI_WSINC5 ;
       else
         ERROR_exit("Unknown code '%s' after '%s' :-(",argv[iarg],argv[iarg-1]) ;
       iarg++ ; continue ;
     }

     /*---------------*/

     ERROR_exit("Unknown, Illegal, and Fattening option '%s' :-( :-( :-(",argv[iarg]) ;
   }

   /*-------- check inputs to see if the user is completely demented ---------*/

   if( dset_nwarp == NULL )
     ERROR_exit("No -nwarp option?  How do you want to warp? :-(") ;

   if( dset_src == NULL ){  /* check last argument if no -source option */
     if( ++iarg < argc ){
       dset_src = THD_open_dataset( argv[iarg] ) ;
       if( dset_src == NULL )
         ERROR_exit("Can't open source dataset '%s' :-(",argv[iarg]);
     } else {
       ERROR_exit("No source dataset?  What do you want to warp? :-(") ;
     }
   }

   if( ainter_code < 0 ) ainter_code = interp_code ;  /* default interp codes */

   /*--- the actual work (bow your head in reverence) ---*/

   if( do_wmast && dset_mast == NULL ) dset_mast = dset_nwarp ;

   verb_nww = verb ;

   dset_out = THD_nwarp_dataset( dset_nwarp , dset_src , dset_mast , prefix ,
                                 interp_code,ainter_code , dxyz_mast , wfac , 0 , awim ) ;

   if( dset_mast != NULL )
     MCW_strncpy( dset_out->atlas_space , dset_mast->atlas_space , THD_MAX_NAME ) ;
   else
     MCW_strncpy( dset_out->atlas_space , dset_nwarp->atlas_space , THD_MAX_NAME ) ;

   tross_Copy_History( dset_src , dset_out ) ;        /* hysterical records */
   tross_Make_History( "3dNwarpApply" , argc,argv , dset_out ) ;

   DSET_write(dset_out) ; if( verb ) WROTE_DSET(dset_out) ;

   if( verb ) INFO_message("total CPU time = %.1f sec  Elapsed = %.1f\n",
                           COX_cpu_time() , COX_clock_time() ) ;
   exit(0) ;
}
