#include "mrilib.h"

int main( int argc , char *argv[] )
{
   THD_3dim_dataset *dset ;
   char *prefix=NULL , *fname ;
   int narg=1 , flags=0 , ii ;

   if( argc < 2 || strcmp(argv[1],"-help") == 0 ){
      printf("Usage: 3dAFNItoMINC [options] dataset\n"
             "Reads in an AFNI dataset, and writes it out as a MINC file.\n"
             "\n"
             "OPTIONS:\n"
             " -prefix ppp  = Write result into file ppp.mnc;\n"
             "                  default prefix is same as AFNI dataset's.\n"
             " -floatize    = Write MINC file in float format.\n"
             "\n"
             "NOTES:\n"
             "* If the dataset has complex-valued sub-bricks, then this\n"
             "   program won't work.\n"
             "* If any of the sub-bricks have floating point scale\n"
             "   factors attached, then the output will be in float\n"
             "   format (regardless of the presence of -floatize).\n"
             "\n"
             "-- RWCox - April 2002\n"
            ) ;
      exit(0) ;
   }

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

     fprintf(stderr,"** ERROR: unknown option: %s\n",argv[narg]); exit(1);
   }

   if( narg >= argc ){
     fprintf(stderr,"** ERROR: no dataset on command line?\n"); exit(1);
   }

   dset = THD_open_dataset( argv[narg] ) ;
   if( !ISVALID_DSET(dset) ){
     fprintf(stderr,"** ERROR: can't open dataset %s\n",argv[narg]); exit(1);
   }

   if( prefix == NULL ) prefix = DSET_PREFIX(dset) ;

   fname = malloc( strlen(prefix)+16 ) ;
   strcpy(fname,prefix) ;
   if( strstr(fname,".mnc") == NULL ) strcat(fname,".mnc") ;

   ii = THD_write_minc( fname , dset , flags ) ;
   exit(0) ;
}
