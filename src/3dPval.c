#include "mrilib.h"

int main( int argc , char *argv[] )
{
   THD_3dim_dataset *iset , *oset ;
   int iarg=1 , kk ;
   char *prefix="Pval" ;
   MRI_IMAGE *iim , *oim ;

   if( argc < 2 || strcmp(argv[1],"-help") == 0 ){
     printf(
      "Usage: 3dPval [options] dataset\n\n"
      "* Converts a dataset statistical sub-bricks to p-values.\n"
      "* Sub-bricks that are not marked as statistical volumes are unchanged.\n"
      "* However, all output volumes will be converted to float format!\n"
      "* A quick hack for Isaac, by Zhark the Probabilistic [26 Jun 2015].\n"
      "\n"
      "Options:\n"
      "=======\n"
      " -prefix p   = Prefix name for output file\n"
      "\n"
     ) ;
     PRINT_COMPILE_DATE ; exit(0) ;
   }

   mainENTRY("3dPval") ; machdep() ;

   while( iarg < argc && argv[iarg][0] == '-' ){

     if( strcmp(argv[iarg],"-prefix") == 0 ){
       prefix = argv[++iarg] ;
       if( !THD_filename_ok(prefix) ) ERROR_exit("bad -prefix value") ;
       iarg++ ; continue ;
     }

   }

   iset = THD_open_dataset( argv[iarg] ) ; CHECK_OPEN_ERROR(iset,argv[iarg]) ;
   DSET_load(iset) ;                     ; CHECK_LOAD_ERROR(iset) ;

   oset = EDIT_empty_copy(iset) ;

   EDIT_dset_items( oset ,
                      ADN_prefix    , prefix    ,
                      ADN_datum_all , MRI_float ,
                      ADN_brick_fac , NULL      ,
                    ADN_none ) ;

   fprintf(stderr,"++ Processing:") ;
   for( kk=0 ; kk < DSET_NVALS(iset) ; kk++ ){
     iim = THD_extract_float_brick(kk,iset) ;
     if( iim == NULL )
       ERROR_exit("Can't get sub-brick %d of input dataset :-(",kk) ;
     DSET_unload_one(iset,kk) ;

     oim = mri_to_pval( iim , DSET_BRICK_STATCODE(iset,kk) ,
                              DSET_BRICK_STATAUX (iset,kk)  ) ;

     if( oim == NULL ){
       oim = iim     ; fprintf(stderr,"-") ;
     } else {
       char *olab , nlab[32] ;
       mri_free(iim) ; fprintf(stderr,"+") ;
       olab = DSET_BRICK_LABEL(iset,kk) ;
       sprintf(nlab,"%.16s_pval",olab) ;
       EDIT_BRICK_LABEL(oset,kk,nlab) ;
       EDIT_BRICK_TO_NOSTAT(oset,kk) ;
     }
     EDIT_substitute_brick( oset , kk , MRI_float , MRI_FLOAT_PTR(oim) ) ;
     mri_clear_and_free(oim) ;
   }
   fprintf(stderr,"\n") ;

   DSET_write(oset) ; WROTE_DSET(oset) ;
   exit(0) ;
}
