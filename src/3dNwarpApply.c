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
   int iarg , kk , verb=1 , iv ;
   mat44 src_cmat, nwarp_cmat, mast_cmat ;
   THD_3dim_dataset *dset_out ;
   MRI_IMAGE *fim , *wim ; float *ip,*jp,*kp ;
   int nx,ny,nz,nxyz , nvals ;
   float wfac=1.0f ;
#if 0
   mat44 awarp ; int use_awarp=0 ;
#endif

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
      " -wfac   fff  = Scale the warp by factor 'fff' [default=1.0]\n"
      " -source sss  = 'sss' is the name of the source dataset\n"
      "                ++ That is, the dataset to be warped\n"
      " -master mmm  = 'mmm  is the name of the master dataset\n"
      "                ++ Defines the output grid.\n"
      "                ++ If '-master' is not used, then output\n"
      "                   grid is the same as the source dataset grid.\n"
      " -newgrid dd  = 'dd' is the new grid spacing (in mm)\n"
      "                ++ This lets you resize the grid without\n"
      "                   needing to use '-master'.\n"
      " -interp iii  = 'iii' is the interpolation mode\n"
      "                ++ Default interpolation mode is 'wsinc5' (slowest, bestest)\n"
      "                ++ Available modes are the same as in 3dAllineate:\n"
      "                     NN  linear  cubic  quintic  wsinc5\n"
      "                ++ The same interpolation mode is used for the warp\n"
      "                   itself (if needed) and then for the data being warped.\n"
      " -prefix ppp  = 'ppp' is the name of the new output dataset\n"
      " -quiet       = Don't be verbose :-(\n"
      " -verb        = Be extra verbose :-)\n"
      "\n"
      "NOTES:\n"
      "------\n"
      "* At present, this program doesn't work with 2D warps, only with 3D.\n"
      "\n"
      "* At present, the output is always in float format, no matter what\n"
      "   absurd data type is in the input.\n"
      "\n"
      "* Program 3dNwarpCalc could be used to operate on 3D warps:\n"
      "  ++ e.g.: catenate (compose) them; invert or square root them.\n"
      "  ++ 3dNwarpCalc also has built-in a slightly simplified version\n"
      "     of 3dNwarpApply's functionality, so that you can calculate a\n"
      "     warp and apply it to a dataset in one program.\n"
     ) ;

     printf(
      "\n"
      "AUTHOR -- RWCox -- Anno Domini Two Thousand Eleven\n"
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

     /*---------------*/

     if( strcasecmp(argv[iarg],"-wfac") == 0 ){
       if( ++iarg >= argc ) ERROR_exit("no argument after '%s' :-(",argv[iarg-1]) ;
       wfac = (float)strtod(argv[iarg],NULL) ;
       if( wfac == 0.0f ) wfac = 1.0f ;
       iarg++ ; continue ;
     }

     /*---------------*/

     if( strncasecmp(argv[iarg],"-prefix",5) == 0 ){
       if( prefix != NULL ) ERROR_exit("Can't have multiple %s options :-(",argv[iarg]) ;
       if( ++iarg >= argc ) ERROR_exit("no argument after '%s' :-(",argv[iarg-1]) ;
       if( !THD_filename_ok(argv[iarg]) )
         ERROR_exit("badly formed filename: '%s' '%s' :-(",argv[iarg-1],argv[iarg]) ;
       if( strcasecmp(argv[iarg],"NULL") == 0 )
         ERROR_exit("can't use filename: '%s' '%s' :-(",argv[iarg-1],argv[iarg]) ;
       else
         prefix = argv[iarg] ;
       iarg++ ; continue ;
     }

     /*---------------*/

#if 0
     if( strcasecmp(arg[iarg],"-awarp") == 0 || strcasecmp(argv[iarg],"-matrix") == 0 ){
       mat44 mmm ; MRI_IMAGE *qim ; float *qar ;
       if( use_awarp ) ERROR_exit("Can't have multiple %s options :-(",argv[iarg]) ;
       if( ++iarg >= argc ) ERROR_exit("no argument after '%s' :-(",argv[iarg-1]) ;
       qim = mri_read_1D(argv[iarg]) ;
       if( qim == NULL || qim->nvox < 9 )
         ERROR_exit("cannot read matrix from file '%s'",argv[iarg]);
       if( qim->ny > 1 ){
         MRI_IMAGE *tim = mri_transpose(qim) ; mri_free(qim) ; qim = tim ;
       }
       qar = MRI_FLOAT_PTR(qim) ;
       if( qim->nvox < 12 )                           /* presumably a rotation */
         LOAD_MAT44(mmm,qar[0],qar[1],qar[2],0,
                        qar[3],qar[4],qar[5],0,
                        qar[6],qar[7],qar[8],0) ;
       else                                           /* a full matrix */
         LOAD_MAT44(mmm,qar[0],qar[1],qar[2],qar[3],
                        qar[4],qar[5],qar[6],qar[7],
                        qar[8],qar[9],qar[10],qar[11]) ;
       mri_free(qim) ;
       awarp = mmm ; use_awarp = 1 ; iarg++ ; continue ;
     }
#endif

     /*---------------*/

     if( strcasecmp(argv[iarg],"-nwarp") == 0 || strcasecmp(argv[iarg],"-warp") == 0 ){
       if( dset_nwarp != NULL ) ERROR_exit("Can't have multiple %s options :-(",argv[iarg]) ;
       if( ++iarg >= argc ) ERROR_exit("no argument after '%s' :-(",argv[iarg-1]) ;
       dset_nwarp = THD_open_dataset( argv[iarg] ) ;
       if( dset_nwarp == NULL ) ERROR_exit("can't open warp dataset '%s' :-(",argv[iarg]);
       if( DSET_NVALS(dset_nwarp) < 3 ) ERROR_exit("dataset '%s' isn't a 3D warp",argv[iarg]);
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

   /*--- the actual work (bow your head in reverence) ---*/

   dset_out = THD_nwarp_dataset( dset_nwarp , dset_src , dset_mast ,
                                 prefix , interp_code , dxyz_mast , wfac , 0 ) ;

   tross_Copy_History( dset_src , dset_out ) ;        /* hysterical records */
   tross_Make_History( "3dNwarpApply" , argc,argv , dset_out ) ;

   DSET_write(dset_out) ; WROTE_DSET(dset_out) ;
   exit(0) ;
}
