/* 3DSVM */ 
/* Integrate the functionality of SMV-Light learning/classification with AFNI data representation. */


#include "3dsvm_common.h"
#include "svm_common.h"
#include "debugtrace.h"

int main( int argc, char *argv[] )
{

  /*---------- DECLARATIONS ----------*/
  enum modes mode = NOTHING;

  ASLoptions options;
  KERNEL_PARM kernel_parm;       /* */
  LEARN_PARM learn_parm;         /* */
  long kernel_cache_size;        /* for parameters dealing with svm-light */
  MODEL   model;
  THD_3dim_dataset* dsetTrain = NULL;           /* pointer for training dataset */
  THD_3dim_dataset* dsetMask = NULL;           /* pointer for mask dataset */
  MaskType*         dsetMaskArray = NULL;      /* array to hold mask dataset values */

  THD_3dim_dataset *dsetTest = NULL;           /* pointer for testing dataset */
  THD_3dim_dataset *dsetModel = NULL;
  char errorString[LONG_STRING];

   enable_mcw_malloc(); /* ZSS helps locate some memory problems*/

   mainENTRY("3dsvm"); /* ZSS, see -trace option in input_parse*/

  /*----- COMMAND LINE PARSE -> OPTIONS --------*/
  strcpy(errorString,"OK");
  input_parse(argc,argv,&verbosity,&kernel_cache_size,&learn_parm,&kernel_parm,&options,&mode,&errorString[0]);
  if( !strcmp(errorString,"OK") == 0 ) {
    fputs(errorString,stderr);
    exit(0);
  }


  /*----- TRAIN ROUTINE ---------------*/
  if( mode == TRAIN || mode == TRAIN_AND_TEST ) {

    train_routine(&model, &learn_parm, &kernel_parm, &options, dsetTrain, dsetMask, dsetMaskArray, argc, argv);

/*
    if( mode == TRAIN )
      free(dsetMaskArray);
      free(dsetModel);
*/

  }


  if( mode == TEST || mode == TRAIN_AND_TEST ) {
    
    test_routine(&options, &model, dsetTest, dsetMask, dsetModel, argc, argv);

  }


  RETURN(0);
}


