#include "mrilib.h"

int main( int argc , char *argv[] )
{
   THD_3dim_dataset *dset ;
   int iarg=1 ;
   int nt=1 , nx=64,ny=64,nz=64 ;
   char *prefix="Empty" ;
   THD_ivec3 ixyz ;

   if( argc < 2 || strcmp(argv[1],"-help") == 0 ){
     printf(
      "Usage: 3dEmpty [options]\n"
      "Makes an 'empty' dataset .HEAD file.\n"
      "\n"
      "Options:\n"
      "=======\n"
      " -prefix p   = Prefix name for output file\n"
      " -nxyz x y z = Set number of voxels to be 'x', 'y', and 'z'\n"
      "                 along the 3 axes [defaults=64]\n"
      " -nt         = Number of time points [default=1]\n"
      "\n"
      "* Other dataset parameters can be changed with 3drefit.\n"
      "* The purpose of this program (combined with 3drefit) is to\n"
      "  allow you to make up an AFNI header for an existing file.\n"
      "\n"
     ) ;
     PRINT_COMPILE_DATE ; exit(0) ;
   }

   mainENTRY("3dEmpty") ; machdep() ;

   while( iarg < argc ){

     if( strcmp(argv[iarg],"-prefix") == 0 ){
       prefix = argv[++iarg] ;
       if( !THD_filename_ok(prefix) ) ERROR_exit("bad -prefix value") ;
       iarg++ ; continue ;
     }

     if( strcmp(argv[iarg],"-nxyz") == 0 ){
       nx = (int)strtod(argv[++iarg],NULL) ;
       ny = (int)strtod(argv[++iarg],NULL) ;
       nz = (int)strtod(argv[++iarg],NULL) ;
       if( nx < 1 || ny < 1 || nz < 1 || nx*ny*nz == 1 )
         ERROR_exit("bad -nxyz values") ;
       iarg++ ; continue ;
     }

     if( strcmp(argv[iarg],"-nt") == 0 ){
       nt = (int)strtod(argv[++iarg],NULL) ;
       if( nt < 1 ) ERROR_exit("-bad nt value") ;
       iarg++ ; continue ;
     }

     ERROR_exit("Unknown option '%s'",argv[iarg]) ;
   }

   dset = EDIT_empty_copy( NULL ) ;

   LOAD_IVEC3(ixyz,nx,ny,nz) ;
   EDIT_dset_items( dset ,
                      ADN_nxyz   , ixyz ,
                      ADN_prefix , prefix ,
                    ADN_none ) ;
   if( nt > 1 )
     EDIT_dset_items( dset ,
                        ADN_nvals  , nt ,
                        ADN_ntt    , nt ,
                        ADN_ttdel  , 1.0 ,
                        ADN_ttorg  , 0.0 ,
                        ADN_nsl    , 0 ,
                        ADN_tunits , UNITS_SEC_TYPE ,
                      ADN_none ) ;

   DSET_write_header( dset ) ;
   exit(0) ;
}
