#include "mrilib.h"

int main( int argc , char *argv[] )
{
   THD_3dim_dataset *dset ;
   char *prefix ;
   int narg=1 ;

#ifndef DONT_ALLOW_MINC

WARNING_message("This program (3dMINCtoAFNI) is old, obsolete, and not maintained!") ;

   if( argc < 2 || strcmp(argv[1],"-help") == 0 ){
      printf("Usage: 3dMINCtoAFNI [-prefix ppp] dataset.mnc\n"
             "Reads in a MINC formatted file and writes it out as an\n"
             "AFNI dataset file pair with the given prefix.  If the\n"
             "prefix option isn't used, the input filename will be\n"
             "used, after the '.mnc' is chopped off.\n"
             "\n"
             "NOTES:\n"
             "* Setting environment variable AFNI_MINC_FLOATIZE to Yes\n"
             "   will cause MINC datasets to be converted to floats on\n"
             "   input.  Otherwise, they will be kept in their 'native'\n"
             "   data type if possible, which may cause problems with\n"
             "   scaling on occasion.\n"
             "* The TR recorded in MINC files is often incorrect.  You may\n"
             "   need to fix this (or other parameters) using 3drefit.\n"
            ) ;
      PRINT_COMPILE_DATE ; exit(0) ;
   }

   if( strcmp(argv[narg],"-prefix") == 0 ){
      prefix = argv[++narg] ;
      if( !THD_filename_ok(prefix) ){
         fprintf(stderr,"** Prefix string is illegal: %s\n",prefix) ;
         exit(1) ;
      }
      narg++ ;
   } else {
      int ii ;
      prefix = strdup(argv[narg]) ;
      ii = strlen(prefix) ;
      if( ii > 5 ) prefix[ii-4] = '\0' ;
      if( !THD_filename_ok(prefix) ){
         fprintf(stderr,"** Prefix string is illegal: %s\n",prefix) ;
         exit(1) ;
      }
   }

   dset = THD_open_minc( argv[narg] ) ;
   if( dset == NULL ){
      fprintf(stderr,"** Can't open dataset %s\n",argv[narg]) ;
      exit(1) ;
   }
   if( !DSET_IS_MINC(dset) ){
      fprintf(stderr,"** Not a MINC dataset: %s\n",argv[narg]) ;
      exit(1) ;
   }
   DSET_load(dset) ; CHECK_LOAD_ERROR(dset) ;

   EDIT_dset_items( dset , ADN_prefix,prefix , ADN_none ) ;
   dset->idcode = MCW_new_idcode() ;

   dset->dblk->diskptr->storage_mode = STORAGE_BY_BRICK ;

   tross_Make_History( "3dMINCtoAFNI" , argc,argv , dset ) ;

   DSET_write(dset) ;
   fprintf(stderr,"++ Wrote dataset %s\n",DSET_BRIKNAME(dset)) ;
   exit(0) ;

#else
   ERROR_exit("3dMINCtoAFNI is no longer compiled") ;
#endif
}
