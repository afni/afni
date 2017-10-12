#include "mrilib.h"

int main( int argc , char *argv[] )
{
   THD_3dim_dataset *dset ;
   char *prefix=NULL ;
   int narg=1 , ii ;

WARNING_message("This program (3dAFNIto3D) is old, not maintained, and probably useless!") ;

   if( argc < 2 || strcmp(argv[1],"-help") == 0 ){
      printf("Usage: 3dAFNIto3D [options] dataset\n"
             "Reads in an AFNI dataset, and writes it out as a 3D file.\n"
             "\n"
             "OPTIONS:\n"
             " -prefix ppp  = Write result into file ppp.3D;\n"
             "                  default prefix is same as AFNI dataset's.\n"
             " -bin         = Write data in binary format, not text.\n"
             " -txt         = Write data in text format, not binary.\n"
             "\n"
             "NOTES:\n"
             "* At present, all bricks are written out in float format.\n"
            ) ;
      PRINT_COMPILE_DATE; exit(0) ;
   }

   mainENTRY("3dAFNIto3D main"); machdep(); PRINT_VERSION("3dAFNIto3D");

   while( narg < argc && argv[narg][0] == '-' ){

     if( strncmp(argv[narg],"-bin",4) == 0 ){         /* 03 Jun 2005 */
       (void) AFNI_setenv( "AFNI_3D_BINARY  YES" ) ;
       narg++ ; continue ;
     }

     if( strcmp(argv[narg],"-txt") == 0 || strcmp(argv[narg],"-text") == 0 ){
       (void) AFNI_setenv( "AFNI_3D_BINARY  NO" ) ;
       narg++ ; continue ;
     }

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

   dset = THD_open_dataset( argv[narg] ) ; CHECK_OPEN_ERROR(dset,argv[narg]) ;
   DSET_load(dset) ;                       CHECK_LOAD_ERROR(dset) ;
   if( prefix == NULL ){                       /* 03 Jun 2005 */
     prefix = strdup( DSET_PREFIX(dset) ) ;
     if( STRING_HAS_SUFFIX(prefix,".1D") ){    /* don't overwrite .1D files! */
       char *cpt = strstr(prefix,".1D") ;
       strcpy(cpt,".3D") ;
     }
   }

   THD_use_3D_format( 1 ) ;
   THD_write_3dim_dataset( NULL , prefix , dset , True ) ;
   THD_use_3D_format( 0 ) ;
   exit(0) ;
}
