#include "mrilib.h"

int main( int argc , char *argv[] )
{
   THD_3dim_dataset *dset ;
   char *prefix=NULL ;
   int narg=1 , ii ;

   if( argc < 2 || strcmp(argv[1],"-help") == 0 ){
      printf("Usage: 3dAFNIto3D [options] dataset\n"
             "Reads in an AFNI dataset, and writes it out as a 3D file.\n"
             "\n"
             "OPTIONS:\n"
             " -prefix ppp  = Write result into file ppp.3D;\n"
             "                  default prefix is same as AFNI dataset's.\n"
             "\n"
             "NOTES:\n"
             "* At present, all bricks are written out in float format.\n"
            ) ;
      exit(0) ;
   }

   mainENTRY("3dAFNIto3D main"); machdep() ;

   while( narg < argc && argv[narg][0] == '-' ){

     if( strcmp(argv[narg],"-prefix") == 0 ){
        prefix = argv[++narg] ;
        if( !THD_filename_ok(prefix) || prefix[0] == '-' ){
          fprintf(stderr,"** Prefix string is illegal: %s\n",prefix) ;
          exit(1) ;
        }
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
   DSET_load(dset) ;
   if( !DSET_LOADED(dset) ){
     fprintf(stderr,"** ERROR: can't load dataset %s\n",argv[narg]); exit(1);
   }

   THD_use_3D_format( 1 ) ;
   THD_write_3dim_dataset( NULL , prefix , dset , True ) ;
   THD_use_3D_format( 0 ) ;
   exit(0) ;
}
