#include "mrilib.h"

int main( int argc , char *argv[] )
{
   THD_3dim_dataset *dset ;
   int iarg=1 ;
   int nt=1 , nx=64,ny=64,nz=64 , do_fill=0 ;
   char *prefix="Empty" ;
   THD_ivec3 ixyz ;
   char *gstr=NULL ;

   if( argc < 2 || strcmp(argv[1],"-help") == 0 ){
     printf(
      "Usage: 3dEmpty [options]\n"
      "Makes an 'empty' dataset .HEAD file.\n"
      "\n"
      "Options:\n"
      "=======\n"
      " -prefix p   = Prefix name for output file (default = 'Empty')\n"
      "\n"
      " -nxyz x y z = Set number of voxels to be 'x', 'y', and 'z'\n"
      "                 along the 3 axes [defaults=64]\n"
      "   *OR*\n"
      "\n"
      " -geometry m = Set the 3D geometry of the grid using a\n"
      "               string 'm' of the form\n"
      "          'MATRIX(a11,a12,a13,a14,a21,a22,a23,a24,a31,a32,a33,a34):nx,ny,nz'\n"
      "               which defines the number of grid points, as well as\n"
      "               relationship between grid indexes (voxel centers)\n"
      "               and the 3D xyz coordinates.\n"
      "             * Sample 'MATRIX()' entries can be found by using\n"
      "               program 3dinfo on an existing datasets.\n"
      "             * Each .niml file used by 3dGroupInCorr has a\n"
      "               'geometry=\"MATRIX(...)\" entry.\n"
      "\n"
      " -nt         = Number of time points [default=1]\n"
      "\n"
      "* Other dataset parameters can be changed with 3drefit.\n"
      "* The purpose of this program (combined with 3drefit) is to\n"
      "  allow you to make up an AFNI header for an existing data file.\n"
      "* This program does NOT create data to fill up the dataset.\n"
      "* If you want to create a dataset of a given size with random\n"
      "  values attached, a command like\n"
      "    3dcalc -a jRandomDataset:32,32,16,10 -expr a -prefix Something\n"
      "  would work. In this example, nx=ny=32 nz=16 nt=10.\n"
      "  (Changing '-expr a' to '-expr 0' would fill the dataset with zeros.)\n"
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

     if( strncmp(argv[iarg],"-geom",5) == 0 ){
       gstr = strdup(argv[++iarg]) ;
       if( ! ISVALID_GEOMETRY_STRING(gstr) )
         ERROR_exit("'%s' is not a legal geometry string",gstr) ;
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


   if( gstr == NULL ){
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
   } else {
     dset = EDIT_geometry_constructor(gstr,prefix) ;
   }

   if( dset == NULL )
     ERROR_exit("Failed to construct empty dataset :(") ;

   DSET_write_header( dset ) ;
   INFO_message("output %s",DSET_HEADNAME(dset)) ;
   exit(0) ;
}
