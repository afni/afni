#include "mrilib.h"

int main( int argc , char *argv[] )
{
   MRI_IMAGE *outim , *rim ;
   MRI_IMARR *imar ;
   int dcode=0 , ii=0,jj=0,kk=0 , ival,nval , nopt=1 , nrow ;
   THD_3dim_dataset *dset=NULL ;
   char *fname="-" ;

   if( argc < 2 || strcmp(argv[1],"-help") == 0 ){
     printf(
      "Program to extract 1 row from a dataset and write it as a .1D file\n"
      "Usage: 3dGetrow [options] dataset\n"
      "\n"
      "OPTIONS:\n"
      "-------\n"
      "Exactly ONE of the following three options is required:\n"
      " -xrow j k  = extract row along the x-direction at fixed y-index of j\n"
      "              and fixed z-index of k.\n"
      " -yrow i k  = similar for a row along the y-direction\n"
      " -zrow i j  = similar for a row along the z-direction\n"

      " -input ddd = read input from dataset 'ddd'\n"
      "              (instead of putting dataset name at end of command line)\n"
      " -output ff = filename for output .1D ASCII file will be 'ff'\n"
      "              (if 'ff' is '-', then output is to stdout, the default)\n"
      "\n"
      "N.B.: if the input dataset has more than one sub-brick, each\n"
      "      sub-brick will appear as a separate column in the output file.\n"
     ) ;
     PRINT_COMPILE_DATE ; exit(0) ;
   }

   /*---- scan command line args ----*/

   mainENTRY("3dGetrow main") ; machdep() ; PRINT_VERSION("3dGetrow") ;

   while( nopt < argc && argv[nopt][0] == '-' ){

     if( strcmp(argv[nopt],"-input") == 0 ){
       if( dset != NULL )
         ERROR_exit("Can't have 2 -input options") ;
       if( ++nopt >= argc )
         ERROR_exit("Need argument after '-input'") ;
       dset = THD_open_dataset(argv[nopt]) ; CHECK_OPEN_ERROR(dset,argv[nopt]) ;
       DSET_load(dset) ; CHECK_LOAD_ERROR(dset) ;
       nopt++ ; continue ;
     }

     if( strcmp(argv[nopt],"-output") == 0 ){
       if( ++nopt >= argc )
         ERROR_exit("Need argument after '-output'") ;
       fname = argv[nopt] ;
       if( !THD_filename_ok(fname) )
         ERROR_exit("Illegal output filename '%s'",fname) ;
       nopt++ ; continue ;
     }

     if( strcmp(argv[nopt],"-xrow") == 0 || strcmp(argv[nopt],"--xrow") == 0 ){
       if( nopt+2 >= argc )
         ERROR_exit("Need 2 arguments after '-xrow'") ;
       dcode = (argv[nopt][1] == '-') ? -1 : 1 ;
       jj = (int)strtod(argv[++nopt],NULL) ;
       kk = (int)strtod(argv[++nopt],NULL) ;
       nopt++ ; continue ;
     }

     if( strcmp(argv[nopt],"-yrow") == 0 || strcmp(argv[nopt],"--yrow") == 0 ){
       if( nopt+2 >= argc )
         ERROR_exit("Need 2 arguments after '-yrow'") ;
       dcode = (argv[nopt][1] == '-') ? -2 : 2 ;
       ii = (int)strtod(argv[++nopt],NULL) ;
       kk = (int)strtod(argv[++nopt],NULL) ;
       nopt++ ; continue ;
     }

     if( strcmp(argv[nopt],"-zrow") == 0 || strcmp(argv[nopt],"--zrow") == 0 ){
       if( nopt+2 >= argc )
         ERROR_exit("Need 2 arguments after '-zrow'") ;
       dcode = (argv[nopt][1] == '-') ? -3 : 3 ;
       ii = (int)strtod(argv[++nopt],NULL) ;
       jj = (int)strtod(argv[++nopt],NULL) ;
       nopt++ ; continue ;
     }

     ERROR_exit("Unknown option '%s'",argv[nopt]) ;
   }

   /* must have had a -?row option */

   if( dcode == 0 ) ERROR_exit("Need one of -xrow, -yrow, or -zrow!") ;

   /* dataset needed still? */

   if( dset == NULL ){
     if( nopt >= argc )
       ERROR_exit("No dataset on command line!?") ;
     dset = THD_open_dataset(argv[nopt]) ; CHECK_OPEN_ERROR(dset,argv[nopt]) ;
     DSET_load(dset) ; CHECK_LOAD_ERROR(dset) ;
   }

   /* get first sub-brick's row */

   nval = DSET_NVALS(dset) ;
   rim  = mri_get_dset_row( dset , 0 , dcode , ii,jj,kk ) ;

   if( rim == NULL )
     ERROR_exit("Can't extract row from (ii,jj,kk)=(%d,%d,%d) !?",ii,jj,kk) ;

   /* if only 1 sub-brick, am done! */

   if( nval == 1 ){
     mri_write_1D(fname,rim) ; exit(0) ;
   }

   /* multiple sub-bricks ==> get more rows, paste together */

   INIT_IMARR(imar) ;
   ADDTO_IMARR(imar,rim) ;

   for( ival=1 ; ival < nval ; ival++ ){
     rim = mri_get_dset_row( dset , ival , dcode , ii,jj,kk ) ;
     if( rim == NULL )
       ERROR_exit("Can't extract row from (ii,jj,kk,vv)=(%d,%d,%d,%d) !?",
                  ii,jj,kk,ival) ;
     ADDTO_IMARR(imar,rim) ;
   }

   DSET_unload(dset) ;

   /* paste images together into one big happy family */

   outim = mri_cat2D(1,nval,0,NULL,imar) ; DESTROY_IMARR(imar) ;
   if( outim == NULL )
     ERROR_exit("Can't glue multiple output rows together?!") ;

   mri_write_1D(fname,outim) ; exit(0) ;
}
