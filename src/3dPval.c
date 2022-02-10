#include "mrilib.h"

int main( int argc , char *argv[] )
{
   THD_3dim_dataset *iset , *oset ;
   int iarg=1 , kk ;
   int doz=0 ;            /* 20 Aug 2019 */
   int doq=0 , nqbad=0 ;  /* 01 Feb 2020 */
   int dolog2=0 ;         /* 29 Jun 2021 */
   int dolog10=0 ;        /* ditto */
   char *prefix="Pval" ;
   MRI_IMAGE *iim , *oim ;

   if( argc < 2 || strcmp(argv[1],"-help") == 0 ){
     printf(
      "\n"
      "Usage: 3dPval [options] dataset\n"
      "\n"
      "* Converts a dataset's statistical sub-bricks to p-values.\n"
      "\n"
      "* Sub-bricks not internally marked as statistical volumes are unchanged.\n"
      "\n"
      "* However, all output volumes will be converted to float format!\n"
      "\n"
      "* If you wish to convert only sub-brick #3 (say) of a dataset, then\n"
      "   something like this command should do the job:\n"
      "     3dPval -prefix Zork.nii InputDataset.nii'[3]'\n"
      "\n"
      "* Note that sub-bricks being marked as statistical volumes, and\n"
      "   having value-to-FDR conversion curves attached, are AFNI-only\n"
      "   ideas, and are not part of any standard, NIfTI or otherwise!\n"
      "   In other words, this program will be useless for a random dataset\n"
      "   which you download from some random non-AFNI-centric site :(\n"
      "\n"
      "* Also note that SMALLER p- and q-values are more 'significant', but\n"
      "   that the AFNI GUI provides interactive thresholding for values\n"
      "   ABOVE a user-chosen level, so using the GUI to threshold on a\n"
      "   p-value or q-value volume will have the opposite result to what\n"
      "   you might wish for.\n"
      "\n"
      "* Although the program now allows conversion of statistic values\n"
      "   to z-scores or FDR q-values, instead of p-values, you can only\n"
      "   do one type of conversion per run of 3dPval. If you want p-values\n"
      "   AND q-values, you'll have to run this program twice.\n"
      "\n"
      "* Finally, 'sub-brick' is AFNI jargon for a single 3D volume inside\n"
      "   a multi-volume dataset.\n"
      "\n"
      "Options:\n"
      "=======\n"
      " -zscore   = Convert statistic to a z-score instead, an N(0,1) deviate\n"
      "               that represents the same p-value.\n"
      "\n"
      " -log2     = Convert statistic to -log2(p)\n"
      " -log10    = Convert statistic to -log10(p)\n"
      "\n"
      " -qval     = Convert statistic to a q-value (FDR) instead:\n"
      "             + This option only works with datasets that have\n"
      "               FDR curves inserted in their headers, which most\n"
      "               AFNI statistics programs will do. The program\n"
      "               3drefit can also do this, with the -addFDR option.\n"
      "\n"
      " -prefix p = Prefix name for output file (default name is 'Pval')\n"
      "\n"
      "\n"
      "AUTHOR: The Man With The Golden p < 0.000001\n"
     ) ;
     PRINT_COMPILE_DATE ; exit(0) ;
   }

   mainENTRY("3dPval") ; machdep() ;

   while( iarg < argc && argv[iarg][0] == '-' ){

     if( strcasecmp(argv[iarg],"-zscore") == 0 || strcasecmp(argv[iarg],"-zstat") == 0 ){
       dolog10 = dolog2 = doq = 0 ; doz++ ; iarg++ ; continue ;  /* 20 Aug 2019 */
     }

     if( strcasecmp(argv[iarg],"-qval") == 0 ){
       doq++ ; dolog10 = dolog2 = doz = 0 ; iarg++ ; continue ;  /* 01 Feb 2020 */
     }

     if( strcasecmp(argv[iarg],"-log2") == 0 ){
       dolog2++ ; dolog10 = doz = doq = 0 ; iarg++ ; continue ;  /* 29 Jun 2021 */
     }

     if( strcasecmp(argv[iarg],"-log10") == 0 ){
       dolog10++ ; dolog2 = doz = doq = 0 ; iarg++ ; continue ;  /* 29 Jun 2021 */
     }

     if( strcmp(argv[iarg],"-prefix") == 0 ){
       prefix = argv[++iarg] ;
       if( !THD_filename_ok(prefix) ) ERROR_exit("bad -prefix value") ;
       iarg++ ; continue ;
     }

     ERROR_exit("unknown option %s",argv[iarg]) ;
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

       if( oim != NULL && (dolog2 || dolog10) ){    /* 29 Jun 2021 */
         int ii , nvox=oim->nvox ; float val , *oar=MRI_FLOAT_PTR(oim) ;
         for( ii=0 ; ii < nvox ; ii++ ){
           val = oar[ii] ;
           if( val > 0.0f ){
             oar[ii] = (dolog2) ? -log2f (val)
                                : -log10f(val) ;
           }
         }
       }
     }

     if( oim == NULL ){
       oim = iim     ; fprintf(stderr,"-") ;  /* new data = old data */
     } else {
       char *olab , nlab[128] ;
       mri_free(iim) ; fprintf(stderr,"+") ;
       olab = DSET_BRICK_LABEL(iset,kk) ;
            if( doz     ) sprintf(nlab,"%.116s_zstat" ,olab) ;
       else if( doq     ) sprintf(nlab,"%.116s_qval"  ,olab) ;
       else if( dolog2  ) sprintf(nlab,"%.116s_log2p" ,olab) ;
       else if( dolog10 ) sprintf(nlab,"%.116s_log10p",olab) ;
       else               sprintf(nlab,"%.116s_pval"  ,olab) ; /* default */
       EDIT_BRICK_LABEL(oset,kk,nlab) ;
       if( doz ) EDIT_BRICK_TO_FIZT(oset,kk) ;    /* change stat code */
       else      EDIT_BRICK_TO_NOSTAT(oset,kk) ;  /* erase stat code */
     }
     /* shove new data into dataset */
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
