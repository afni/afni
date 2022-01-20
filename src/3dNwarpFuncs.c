#include "mrilib.h"

#ifdef USE_OMP
#include <omp.h>
#endif

#include "mri_genalign_util.c"
#include "mri_genalign.c"
#include "mri_nwarp.c"

/*----------------------------------------------------------------------------*/
/* This program is mostly a wrapper for function IW3D_load_bsv() */

int main( int argc , char *argv[] )
{
   THD_3dim_dataset *dset_nwarp=NULL , *dset_out=NULL ;
   char *prefix = "NFunc" ;
   int iarg , verb=1 , do_bulk=0, do_shear=0, do_vort=0 , nxyz ;
   float *bim=NULL , *sim=NULL , *vim=NULL ;
   IndexWarp3D *AA ;

   AFNI_SETUP_OMP(0) ;  /* 24 Jun 2013 */

   /**----------------------------------------------------------------------*/
   /**----------------- Help the pitifully ignorant user? -----------------**/

   if( argc < 2 || strcasecmp(argv[1],"-help") == 0 ){
     printf(
      "Usage: 3dNwarpFuncs [options]\n"
      "\n"
      "This program reads in a nonlinear 3D warp (from 3dQwarp, etc.) and\n"
      "computes some functions of the displacements.  See the OPTIONS below\n"
      "for information on what can be computed. The NOTES sections describes\n"
      "the formulae of the functions that are available.\n"
      "\n"
      "--------\n"
      "OPTIONS:\n"
      "--------\n"
      " -nwarp  www  = 'www' is the name of the 3D warp dataset\n"
      "                (this is a mandatory option!)\n"
      "               ++ This can be computed on the fly, as in 3dNwarpApply.\n"
      "\n"
      " -prefix ppp  = 'ppp' is the name of the new output dataset\n"
      "\n"
      " -bulk        = Compute the (fractional) bulk volume change.\n"
      "               ++ e.g., Jacobian determinant minus 1.\n"
      "               ++ see 'MORE...' (below) for interpreting the sign of '-bulk'.\n"
      " -shear       = Compute the shear energy.\n"
      " -vorticity   = Compute the vorticity enerty.\n"
      " -all         = Compute all 3 of these fun fun functions.\n"
      "\n"
      "If none of '-bulk', '-shear', or '-vorticity' are given, then '-bulk'\n"
      "will be assumed.\n"
      "\n"
      "------\n"
      "NOTES:\n"
      "------\n"
      "Denote the displacement vector field (warp) by\n"
      "   [ p(x,y,z) , q(x,y,z) , r(x,y,z) ]\n"
      "Define the Jacobian matrix by\n"
      "\n"
      "      [ 1+dp/dx   dp/dy    dp/dz  ]   [ Jxx Jxy Jxz ]\n"
      "  J = [  dq/dx   1+dq/dy   dq/dz  ] = [ Jyx Jyy Jyz ]\n"
      "      [  dr/dx    dr/dy   1+dr/dz ]   [ Jzx Jzy Jzz ]\n"
      "\n"
      "* The '-bulk' output is the determinant of this matrix (det[J]), minus 1.\n"
      "* It measures the fractional amount of volume distortion.\n"
      "* Negative means the warped coordinates are shrunken (closer together)\n"
      "  than the input coordinates. Also see the 'MORE...' section below.\n"
      "\n"
      "* The '-shear' output is the sum of squares of the J matrix elements --\n"
      "  which equals the sum of squares of its eigenvalues -- divided by\n"
      "  det[J]^(2/3), then minus 3.\n"
      "* It measures the amount of shearing distortion (normalized by the amount\n"
      "  of volume distortion).\n"
      "\n"
      "* The '-vorticity' output is the sum of squares of the skew part of\n"
      "  the J matrix = [ Jxy-Jyx , Jxz-Jzx , Jyz-Jzy ], divided by det[J]^(2/3).\n"
      "* It measures the amount of twisting distortion (also normalized).\n"
      "\n"
      "* All 3 of these functions are dimensionless.\n"
      "\n"
      "* The penalty used in 3dQwarp is a combination of the bulk, shear,\n"
      "  and vorticity functions.\n"
      "\n"
      "------------------------------\n"
      "MORE about interpreting -bulk:\n"
      "------------------------------\n"
      "If the warp N(x,y,z) is the '_WARP' output from 3dQwarp, then N(x,y,z)\n"
      "maps the base dataset (x,y,z) coordinates to the source dataset (x,y,z)\n"
      "coordinates. If the source dataset has to expand in size to match\n"
      "the base dataset, then going from base coordinates to source must\n"
      "be a shrinkage. Thus, negative '-bulk' in this '_WARP' dataset\n"
      "corresponds to expansion going from source to base. Conversely,\n"
      "in this situation, positive '-bulk' will show up in the '_WARPINV'\n"
      "dataset from 3dQwarp as that is the map from source (x,y,z) to\n"
      "base (x,y,z).\n"
      "\n"
      "The situation above happens a lot when using one of the MNI152 human\n"
      "brain templates as the base dataset. This family of datasets is larger\n"
      "than the average human brain, due to the averaging process used to\n"
      "define the first MNI152 template back in the 1990s.\n"
      "\n"
      "I have no easy interpretation handy for the '-shear' and '-vorticity'\n"
      "outputs, alas. They are computed as part of the penalty function used\n"
      "to control weirdness in the 3dQwarp optimization process.\n"
      "\n"
      "---------------------------\n"
      "AUTHOR -- RWCox == @AFNIman\n"
      "---------------------------\n"
     ) ;

     PRINT_COMPILE_DATE ;
     exit(0) ;
   }

   /**--- bookkeeping and marketing ---**/

   mainENTRY("3dNwarpFuncs"); machdep();
   AFNI_logger("3dNwarpFuncs",argc,argv);
   PRINT_VERSION("3dNwarpFuncs"); AUTHOR("Zhark the Warped");
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

     /*---------------*/

     if( strncasecmp(argv[iarg],"-prefix",5) == 0 ){
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
       dset_nwarp = IW3D_read_catenated_warp( argv[iarg] ) ;
       if( dset_nwarp == NULL ) ERROR_exit("can't open warp dataset '%s' :-(",argv[iarg]);
       if( DSET_NVALS(dset_nwarp) < 3 ) ERROR_exit("dataset '%s' isn't a 3D warp",argv[iarg]);
       iarg++ ; continue ;
     }

     /*---------------*/

     if( strncasecmp(argv[iarg],"-bulk",4) == 0 ){
       do_bulk = 1 ; iarg++ ; continue ;
     }
     if( strncasecmp(argv[iarg],"-shear",4) == 0 ){
       do_shear = 1 ; iarg++ ; continue ;
     }
     if( strncasecmp(argv[iarg],"-vort",4) == 0 ){
       do_vort = 1 ; iarg++ ; continue ;
     }
     if( strcasecmp(argv[iarg],"-all")     == 0 ) {
       do_bulk = do_shear = do_vort = 1 ; iarg++ ; continue ;
     }

     /*---------------*/

     ERROR_message("Bogusly Unknown option '%s' :-( :-( :-(",argv[iarg]) ;
     suggest_best_prog_option(argv[0],argv[iarg]) ;
     exit(1) ;

   }

   /*-------- check inputs to see if the user is completely demented ---------*/

   if( dset_nwarp == NULL && iarg >= argc )
     ERROR_exit("No -nwarp option?  How do you want to warp? :-(") ;

   if( dset_nwarp == NULL ){
     dset_nwarp = IW3D_read_catenated_warp( argv[iarg] ) ;
     if( dset_nwarp == NULL ) ERROR_exit("can't open warp dataset '%s' :-(",argv[iarg]);
     if( DSET_NVALS(dset_nwarp) < 3 ) ERROR_exit("dataset '%s' isn't a 3D warp",argv[iarg]);
   }

   if( do_bulk == 0 && do_shear == 0 && do_vort == 0 ){
     do_bulk = 1 ;
     INFO_message("No outputs ordered ==> '-bulk' is what you'll get") ;
   }

   nxyz = DSET_NVOX(dset_nwarp) ;

   if( do_bulk  ) bim = (float *)calloc(sizeof(float),nxyz) ;
   if( do_shear ) sim = (float *)calloc(sizeof(float),nxyz) ;
   if( do_vort  ) vim = (float *)calloc(sizeof(float),nxyz) ;

   /*--- the actual work (bow your head in reverence) ---*/

   verb_nww = 0 ;

   AA = IW3D_from_dataset( dset_nwarp , 0,0 ) ;

   IW3D_load_bsv( AA , fabsf(DSET_DX(dset_nwarp)) ,
                       fabsf(DSET_DY(dset_nwarp)) ,
                       fabsf(DSET_DZ(dset_nwarp)) , bim,sim,vim ) ;

   DSET_unload(dset_nwarp) ; IW3D_destroy(AA) ;

   dset_out = EDIT_empty_copy(dset_nwarp) ;
   EDIT_dset_items( dset_out ,
                      ADN_prefix    , prefix ,
                      ADN_nvals     , do_bulk+do_shear+do_vort ,
                      ADN_ntt       , 0 ,
                      ADN_datum_all , MRI_float ,
                    ADN_none ) ;
   tross_Copy_History( dset_nwarp , dset_out ) ;        /* hysterical records */
   tross_Make_History( "3dNwarpFuncs" , argc,argv , dset_out ) ;

   iarg = 0 ;
   if( do_bulk ){
     EDIT_substitute_brick( dset_out , iarg , MRI_float , bim ) ;
     EDIT_BRICK_LABEL( dset_out , iarg , "bulk" ) ;
     iarg++ ;
   }
   if( do_shear ){
     EDIT_substitute_brick( dset_out , iarg , MRI_float , sim ) ;
     EDIT_BRICK_LABEL( dset_out , iarg , "shear" ) ;
     iarg++ ;
   }
   if( do_vort ){
     EDIT_substitute_brick( dset_out , iarg , MRI_float , vim ) ;
     EDIT_BRICK_LABEL( dset_out , iarg , "vorticity" ) ;
     iarg++ ;
   }

   DSET_write(dset_out) ; if( verb ) WROTE_DSET(dset_out) ;

   exit(0) ;
}
