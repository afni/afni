#include "mrilib.h"
#include "thd_niftiwrite.h"

int main( int argc , char *argv[] )
{
   THD_3dim_dataset *dset ;
   char *prefix=NULL , *fname ;
   int narg=1 , flags=0 , ii , verb=0 ;
   niftiwr_opts_t options ;

   if( argc < 2 || strcmp(argv[1],"-help") == 0 ){
      printf("Usage: 3dAFNItoNIFTI [options] dataset\n"
             "Reads in an AFNI dataset, and writes it out as a NIfTI-1 file.\n"
             "\n"
             "OPTIONS:\n"
             "  -prefix ppp = Write the NIfTI-1 file as 'ppp.nii'.\n"
             "                  Default: the dataset's prefix is used.\n"
             "  -verb       = Be verbose = print progress messages.\n"
             "                  Repeating this increases the verbosity\n"
             "                  (maximum setting is 3 '-verb' options).\n"
            ) ;
      exit(0) ;
   }

   /*--- check options ---*/

   while( narg < argc && argv[narg][0] == '-' ){

     if( strcmp(argv[narg],"-prefix") == 0 ){
        prefix = argv[++narg] ;
        if( !THD_filename_ok(prefix) || prefix[0] == '-' ){
          fprintf(stderr,"** ERROR: -prefix is illegal: %s\n",prefix) ;
          exit(1) ;
        }
        narg++ ; continue ;
     }

     if( strcmp(argv[narg],"-verb") == 0 ){
       verb++ ; narg++ ; continue ;
     }

     fprintf(stderr,"** ERROR: unknown option: %s\n",argv[narg]); exit(1);
   }

   /*--- get the dataset ---*/

   if( narg >= argc ){
     fprintf(stderr,"** ERROR: no dataset on command line?\n"); exit(1);
   }

   dset = THD_open_dataset( argv[narg] ) ;
   if( !ISVALID_DSET(dset) ){
     fprintf(stderr,"** ERROR: can't open dataset %s\n",argv[narg]); exit(1);
   }

   /*--- deal with the filename ---*/

   if( prefix == NULL ) prefix = DSET_PREFIX(dset) ;

   fname = malloc( strlen(prefix)+16 ) ;
   strcpy(fname,prefix) ;
   if( strstr(fname,".nii") == NULL ) strcat(fname,".nii") ;

   options.infile_name = nifti_strdup(fname) ;
   options.debug_level = verb ;

   /*--- Go Baby, Go! ---*/

   ii = THD_write_nifti( dset , options ) ;
   exit(0) ;
}
