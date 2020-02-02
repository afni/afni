#include "mrilib.h"

int main( int argc , char *argv[] )
{
   THD_3dim_dataset *iset , *oset ;
   int iarg=1 , kk ;
   int doz=0 ;            /* 20 Aug 2019 */
   int doq=0 , nqbad=0 ;  /* 01 Feb 2020 */
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
      " -zscore     = Convert to a z-score instead\n"
      " -qval       = Convert to a q-value (FDR) instead\n"
      "                This option only works with datasets that have\n"
      "                FDR curves inserted in their headers, which most\n"
      "                AFNI statistics programs will do. The program\n"
      "                3drefit can also do this, with the -addFDR option.\n"
      " -prefix p   = Prefix name for output file\n"
      "\n"
     ) ;
     PRINT_COMPILE_DATE ; exit(0) ;
   }

   mainENTRY("3dPval") ; machdep() ;

   while( iarg < argc && argv[iarg][0] == '-' ){

     if( strcasecmp(argv[iarg],"-zscore") == 0 || strcasecmp(argv[iarg],"-zstat") == 0 ){
       doq = 0 ; doz++ ; iarg++ ; continue ;  /* 20 Aug 2019 */
     }

     if( strcasecmp(argv[iarg],"-qval") == 0 ){
       doq++ ; doz = 0 ; iarg++ ; continue ;  /* 01 Feb 2020 */
     }

     if( strcmp(argv[iarg],"-prefix") == 0 ){
       prefix = argv[++iarg] ;
       if( !THD_filename_ok(prefix) ) ERROR_exit("bad -prefix value") ;
       iarg++ ; continue ;
     }

     WARNING_message("Skipping unknown option %s",argv[iarg]) ;
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

     if( doz ){
       oim = mri_to_zscore( iim , DSET_BRICK_STATCODE(iset,kk) ,
                                  DSET_BRICK_STATAUX (iset,kk)  ) ;
     } else if( doq ){
       floatvec *fv = DSET_BRICK_FDRCURVE(iset,kk) ;
       oim = mri_to_qval( iim , fv ) ;
       if( fv == NULL && FUNC_IS_STAT(DSET_BRICK_STATCODE(iset,kk)) ){
         WARNING_message(
           "volume %d is marked as a stat, but does not have an FDR curve :(",kk) ;
         nqbad++ ;
       }
     } else {
       oim = mri_to_pval( iim , DSET_BRICK_STATCODE(iset,kk) ,
                                DSET_BRICK_STATAUX (iset,kk)  ) ;
     }

     if( oim == NULL ){
       oim = iim     ; fprintf(stderr,"-") ;
     } else {
       char *olab , nlab[128] ;
       mri_free(iim) ; fprintf(stderr,"+") ;
       olab = DSET_BRICK_LABEL(iset,kk) ;
            if( doz ) sprintf(nlab,"%.120s_zstat",olab) ;
       else if( doq ) sprintf(nlab,"%.120s_qval" ,olab) ;
       else           sprintf(nlab,"%.120s_pval" ,olab) ;
       EDIT_BRICK_LABEL(oset,kk,nlab) ;
       if( doz ) EDIT_BRICK_TO_FIZT(oset,kk) ;
       else      EDIT_BRICK_TO_NOSTAT(oset,kk) ;
     }
     EDIT_substitute_brick( oset , kk , MRI_float , MRI_FLOAT_PTR(oim) ) ;
     mri_clear_and_free(oim) ;
   }
   fprintf(stderr,"\n") ;

   if( nqbad > 0 ){
     WARNING_message(
      "lack of FDR curves can be supplied by using 3drefit -addFDR") ;
   }

   DSET_write(oset) ; WROTE_DSET(oset) ;
   exit(0) ;
}
