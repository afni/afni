/* 3DSVM */ 
/* Integrate the functionality of SMV-Light learning/classification with AFNI data representation. */


#include "3dsvm_common.h"

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

  /*----- COMMAND LINE PARSE -> OPTIONS --------*/
  strcpy(errorString,"OK");
  input_parse(argc,argv,&verbosity,&kernel_cache_size,&learn_parm,&kernel_parm,&options,&mode,&errorString[0]);
  if( !strcmp(errorString,"OK") == 0 ) {
    fprintf(stderr,errorString);
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


  return 0;
}


