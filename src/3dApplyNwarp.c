#include "mrilib.h"
#include "r_new_resam_dset.h"

#ifdef USE_OMP
#include <omp.h>
#include "mri_genalign_util.c"
#endif

int main( int argc , char *argv[] )
{
   THD_3dim_dataset *dset_nwarp=NULL , *dset_src=NULL , *dset_mast=NULL ;
   MRI_IMARR *imar_nwarp=NULL , *im_src ;
   char *prefix     = NULL ;
   double dxyz_mast = 0.0 ;
   int interp_code  = MRI_QUINTIC ;
   int iarg ;

   /**----------------------------------------------------------------------*/
   /**----------------- Help the pitifully ignorant user? -----------------**/

   if( argc < 2 || strcasecmp(argv[1],"-help") == 0 ){
     printf(
      "Usage: 3dApplyNwarp [options] sourcedataset\n"
      "\n"
      "Program to apply a nonlinear warp saved from 3dAllineate -nwarp_save\n"
      "to a dataset, to produce a warped version of the source dataset.\n"
      "\n"
      "OPTIONS:\n"
      "--------\n"
      " -nwarp  www  = 'www' is the name of the warp dataset\n"
      " -source sss  = 'sss' is the name of the source dataset\n"
      " -master mmm  = 'mmm  is the name of the master dataset\n"
      " -newgrid dd  = 'dd' is the new grid spacing (in mm)\n"
      " -interp iii  = 'iii' is the interpolation method\n"
      " -prefix ppp  = 'ppp' is the name of the new output dataset\n"
     ) ;
     exit(0) ;
   }

   /**--- bookkeeping and marketing ---**/

   mainENTRY("3dApplyNwarp"); machdep();
   AFNI_logger("3dApplyNwarp",argc,argv);
   PRINT_VERSION("3dApplyNwarp"); AUTHOR("Zhark the Warped");
   THD_check_AFNI_version("3dApplyNwarp");
   (void)COX_clock_time() ;

   /**--- process command line options ---**/

   iarg = 1 ;
   while( iarg < argc && argv[iarg][0] == '-' ){

     /*---------------*/

     if( strncasecmp(argv[iarg],"-prefix",5) == 0 ){
       if( prefix != NULL ) ERROR_exit("Can't have multiple %s options :-(",argv[iarg]) ;
       if( !THD_filename_ok(argv[iarg]) )
         ERROR_exit("badly formed filename: '%s' '%s' :-(",argv[iarg-1],argv[iarg]) ;
       if( strcasecmp(argv[iarg],"NULL") == 0 )
         ERROR_exit("can't use filename: '%s' '%s' :-(",argv[iarg-1],argv[iarg]) ;
       else
         prefix = argv[iarg] ;
       iarg++ ; continue ;
     }

     /*---------------*/

     if( strcasecmp(argv[iarg],"-nwarp") == 0 ){
       if( dset_nwarp != NULL ) ERROR_exit("Can't have multiple %s options :-(",argv[iarg]) ;
       if( ++iarg >= argc ) ERROR_exit("no argument after '%s' :-(",argv[iarg-1]) ;
       dset_nwarp = THD_open_dataset( argv[iarg] ) ;
       if( dset_nwarp == NULL ) ERROR_exit("can't open -nwarp dataset '%s' :-(",argv[iarg]);
       iarg++ ; continue ;
     }

     /*---------------*/

     if( strcasecmp(argv[iarg],"-source") == 0 ){
       if( dset_src != NULL ) ERROR_exit("Can't have multiple %s options :-(",argv[iarg]) ;
       if( ++iarg >= argc ) ERROR_exit("no argument after '%s' :-(",argv[iarg-1]) ;
       dset_src = THD_open_dataset( argv[iarg] ) ;
       if( dset_src == NULL ) ERROR_exit("can't open -source dataset '%s' :-(",argv[iarg]);
       iarg++ ; continue ;
     }

     /*---------------*/

     if( strcasecmp(argv[iarg],"-master") == 0 ){
       if( dset_mast != NULL ) ERROR_exit("Can't have multiple %s options :-(",argv[iarg]) ;
       if( ++iarg >= argc ) ERROR_exit("no argument after '%s' :-(",argv[iarg-1]) ;
       dset_mast = THD_open_dataset( argv[iarg] ) ;
       if( dset_mast == NULL ) ERROR_exit("can't open -master dataset '%s' :-(",argv[iarg]);
       iarg++ ; continue ;
     }

     /*---------------*/

     if( strcasecmp(argv[iarg],"-mast_dxyz") == 0 ||
         strcasecmp(argv[iarg],"-dxyz_mast") == 0 ||
         strcasecmp(argv[iarg],"-newgrid"  ) == 0   ){

       if( ++iarg >= argc ) ERROR_exit("no argument after '%s' :-(",argv[iarg-1]) ;
       dxyz_mast = strtod(argv[iarg],NULL) ;
       if( dxyz_mast <= 0.0 )
         ERROR_exit("Illegal value '%s' after -mast_dxyz :-(",argv[iarg]) ;
       if( dxyz_mast <= 0.5 )
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
       if( ++iarg >= argc ) ERROR_exit("no argument after '%s' :-(",argv[iarg-1]) ;
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

     /*---------------*/

     ERROR_exit("Unknown and Illegal option '%s' :-( :-( :-(",argv[iarg]) ;
   }

   /*---------- check inputs to see if the user is demented ----------*/

}
