#include "mrilib.h"

int main( int argc , char *argv[] )
{
   THD_3dim_dataset *dset ;
   char *prefix ;
   int narg=1 ;

   if( argc < 2 || strcmp(argv[1],"-help") == 0 ){
      printf("Usage: 3dMINCtoAFNI [-prefix ppp] dataset.mnc\n"
             "Reads in a MINC formatted file and writes it out as an\n"
             "AFNI dataset file pair with the given prefix.  If the\n"
             "prefix option isn't used, the input filename will be\n"
             "used, after the '.mnc' is chopped off.\n"
             "\n"
             "Setting environment variable AFNI_MINC_FLOATIZE to Yes\n"
             "will cause MINC datasets to be converted to floats on\n"
             "input.  Otherwise, they will be kept in their 'native'\n"
             "data type if possible, which may cause problems with\n"
             "scaling on occasion.\n"
            ) ;
      exit(0) ;
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
   DSET_load(dset) ;
   if( !DSET_LOADED(dset) ){
      fprintf(stderr,"** Can't read MINC dataset %s\n",argv[narg]) ;
      exit(1) ;
   }

   EDIT_dset_items( dset , ADN_prefix,prefix , ADN_none ) ;
   dset->idcode = MCW_new_idcode() ;

   dset->dblk->diskptr->storage_mode = STORAGE_BY_BRICK ;

   tross_Make_History( "3dMINCtoAFNI" , argc,argv , dset ) ;

   fprintf(stderr,"++ Writing dataset %s\n",DSET_HEADNAME(dset)) ;
   DSET_write(dset) ;
   exit(0) ;
}
