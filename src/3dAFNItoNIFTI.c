#include "mrilib.h"
#include "thd_niftiwrite.h"

int main( int argc , char *argv[] )
{
   THD_3dim_dataset *dset ;
   char *prefix=NULL , *fname ;
   int narg=1 , flags=0 , ii ;
   niftiwr_opts_t options ;

   if( argc < 2 || strcmp(argv[1],"-help") == 0 ){
      printf("Usage: 3dAFNItoNIFTI [options] dataset\n"
             "Reads in an AFNI dataset, and writes it out as a NIFTI file.\n"
             "\n"
             "OPTIONS:\n"
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
   if( strstr(fname,".nii") == NULL ) strcat(fname,".nii") ;

   options.infile_name = nifti_strdup(fname) ;
   options.debug_level = 3 ;

   ii = THD_write_nifti( dset , options ) ;
   exit(0) ;
}
