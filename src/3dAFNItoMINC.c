#include "mrilib.h"

int main( int argc , char *argv[] )
{
   THD_3dim_dataset *dset ;
   char *prefix=NULL , *fname ;
   int narg=1 , flags=0 , ii , swap = 0;

#ifndef DONT_ALLOW_MINC

WARNING_message("This program (3dAFNItoMINC) is old, not maintained, and probably useless!") ;

   if( argc < 2 || strcmp(argv[1],"-help") == 0 ){
      printf("Usage: 3dAFNItoMINC [options] dataset\n"
             "Reads in an AFNI dataset, and writes it out as a MINC file.\n"
             "\n"
             "OPTIONS:\n"
             " -prefix ppp  = Write result into file ppp.mnc;\n"
             "                  default prefix is same as AFNI dataset's.\n"
             " -floatize    = Write MINC file in float format.\n"
             " -swap        = Swap bytes when passing data to rawtominc\n"
             "\n"
             "NOTES:\n"
             "* Multi-brick datasets are written as 4D (x,y,z,t) MINC\n"
             "   files.\n"
             "* If the dataset has complex-valued sub-bricks, then this\n"
             "   program won't write the MINC file.\n"
             "* If any of the sub-bricks have floating point scale\n"
             "   factors attached, then the output will be in float\n"
             "   format (regardless of the presence of -floatize).\n"
             "* This program uses the MNI program 'rawtominc' to create\n"
             "   the MINC file; rawtominc must be in your path.  If you\n"
             "   don't have rawtominc, you must install the MINC tools\n"
             "   software package from MNI.  (But if you don't have the\n"
             "   MINC tools already, why do you want to convert to MINC\n"
             "   format anyway?)\n"
             "* At this time, you can find the MINC tools at\n"
             "     ftp://ftp.bic.mni.mcgill.ca/pub/minc/\n"
             "   You need the latest version of minc-*.tar.gz and also\n"
             "   of netcdf-*.tar.gz.\n"
             "\n"
             "-- RWCox - April 2002\n"
            ) ;
      PRINT_COMPILE_DATE; exit(0) ;
   }

   mainENTRY("3dAFNItoMINC main"); machdep(); PRINT_VERSION("3dAFNItoMINC");

   while( narg < argc && argv[narg][0] == '-' ){

     if( strcmp(argv[narg],"-prefix") == 0 ){
        prefix = argv[++narg] ;
        if( !THD_filename_ok(prefix) || prefix[0] == '-' ){
          fprintf(stderr,"** Prefix string is illegal: %s\n",prefix) ;
          exit(1) ;
        }
        narg++ ; continue ;
     }

     if( strcmp(argv[narg],"-floatize") == 0 ){
        flags |= MINC_FLOATIZE_MASK ;
        narg++ ; continue ;
     }

      
     if( strcmp(argv[narg],"-swap") == 0 ){
        flags |= MINC_SWAPIZE_MASK ;
        narg++ ; continue ;
     }
     
     fprintf(stderr,"** ERROR: unknown option: %s\n",argv[narg]); exit(1);
   }

   if( narg >= argc ){
     fprintf(stderr,"** ERROR: no dataset on command line?\n"); exit(1);
   }

   dset = THD_open_dataset(argv[narg]); CHECK_OPEN_ERROR(dset,argv[narg]) ;

   if( prefix == NULL ) prefix = DSET_PREFIX(dset) ;

   fname = malloc( strlen(prefix)+16 ) ;
   strcpy(fname,prefix) ;
   if( strstr(fname,".mnc") == NULL ) strcat(fname,".mnc") ;

   ii = THD_write_minc( fname , dset , flags ) ;
   exit(0) ;

#else

   ERROR_exit("program 3dAFNItoMINC is no longer compiled") ;

#endif
}
