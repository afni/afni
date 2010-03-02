/* 3DSVM */ 
/* Integrate the functionality of SMV-Light learning/classification with AFNI data representation. */


#include "3dsvm_common.h"
#include "svm_common.h"
#include "debugtrace.h"

int main( int argc, char *argv[] )
{
  
  /*---------- DECLARATIONS ----------*/
  enum modes mode = NOTHING;
  int svm_type = CLASSIFICATION; /* JL May 2009: There are three svm_types: 
                                    CLASSIFICATION, REGRESSION and RANKING 
                                    which are defined in svm_common.h */ 
  ASLoptions options;
  KERNEL_PARM kernel_parm;     
  LEARN_PARM learn_parm;         
  long kernel_cache_size;                 /* for parameters dealing with svm-light */
  MODEL   model;
  AFNI_MODEL afniModel;                   /* read ahead of time if testing */
  THD_3dim_dataset* dsetTrain     = NULL; /* pointer for training dataset */
  THD_3dim_dataset* dsetMask      = NULL; /* pointer for mask dataset */
  MaskType*         dsetMaskArray = NULL; /* array to hold mask dataset values */

  THD_3dim_dataset *dsetTest      = NULL; /* pointer for testing dataset */
  THD_3dim_dataset *dsetModel     = NULL;
  char errorString[LONG_STRING];          /* needed for plugin */ 
  
  enable_mcw_malloc(); /* ZSS helps locate some memory problems*/
  mainENTRY("3dsvm");  /* ZSS, see -trace option in input_parse*/

  /*----- COMMAND LINE PARSE -> OPTIONS --------*/
 
  if ( input_parse(argc, argv, &verbosity, &kernel_cache_size, &learn_parm, 
      &kernel_parm, &options, &mode, &svm_type, &errorString) ) { 
    
    ERROR_exit(errorString);
  }

  /*----- TRAIN FUNCTIONS ---------------*/
  if( mode == TRAIN || mode == TRAIN_AND_TEST ) {
    if ( svm_type == CLASSIFICATION ) {
      /* SL & JL Feb. 2009: Passing kernel_cache_size to support non-linear
       * kernels. */
      train_routine(&model, &learn_parm, &kernel_parm, &kernel_cache_size, 
        &options, dsetTrain, dsetMask, dsetMaskArray, argc, argv);
    }
    
    else if ( svm_type == REGRESSION ) { 
      train_regression(&model, &learn_parm, &kernel_parm, &kernel_cache_size, 
        &options, dsetTrain, dsetMask, dsetMaskArray, argc, argv);
    }
    
    else ERROR_exit("type not supported!");
  }


  /*----- TEST FUNCTIONS ---------------*/
  if( mode == TEST || mode == TRAIN_AND_TEST ) {

    /* JL May 2009: Changed the flow for testing to support sv-regression.
     * Reading model and determining svm_type before testing. */

    get_afni_model(&options, &afniModel, dsetModel, &mode, &svm_type);

    if( svm_type == CLASSIFICATION ) {
      test_routine(&options, &model, &afniModel, dsetTest, dsetMask, 
          dsetModel, argc, argv);
    }
    
    else if( svm_type == REGRESSION ) {
      test_regression(&options, &model, &afniModel, dsetTest, dsetMask, 
          dsetModel, argc, argv);
    }

    else ERROR_exit("type not supported!");
  }
  RETURN(0);
}
