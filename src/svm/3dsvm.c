/*****************************************************************************/
/*                                                                           */
/* 3dsvm.c                                                                   */
/*                                                                           */
/* Integrate the functionality of SMV-Light (http://svmlight.joachims.org/)  */
/* with AFNI (https://afni.nimh.nih.gov)                                      */
/*                                                                           */
/* Copyright 2007 Stephen LaConte                                            */
/*                                                                           */
/* This file is part of 3dsvm                                                */
/*                                                                           */
/* 3dsvm is free software: you can redistribute it and/or modify             */
/* it under the terms of the GNU General Public License as published by      */
/* the Free Software Foundation, either version 3 of the License, or         */
/* (at your option) any later version.                                       */
/*                                                                           */
/* 3dsvm is distributed in the hope that it will be useful,                  */
/* but WITHOUT ANY WARRANTY; without even the implied warranty of            */
/* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the             */
/* GNU General Public License for more details.                              */
/*                                                                           */
/* You should have received a copy of the GNU General Public License         */
/* along with 3dsvm.  If not, see <http://www.gnu.org/licenses/>.            */
/*                                                                           */
/*                                                                           */
/* The SVM-light software is copyrighted by Thorsten Joachims                */
/* and is redistributed by permission.                                       */
/*                                                                           */
/* The SVM-light software is free only for non-commercial use. It must not   */
/* be distributed without prior permission of the author. The author is not  */
/* responsible for implications from the use of this software.               */
/*                                                                           */
/*                                                                           */
/* For AFNI's copyright please refer to ../README.copyright.                 */
/*                                                                           */
/*****************************************************************************/


#include "3dsvm_common.h"
#include "debugtrace.h"

int main( int argc, char *argv[] )
{
  
  /*---------- DECLARATIONS ----------*/
  enum modes mode = NOTHING;
  int svm_type = CLASSIFICATION; /* JL May 2009: There are three  svm_types:
                                    CLASSIFICATION,  REGRESSION and RANKING
                                    which are defined in svm_common.h */ 
  ASLoptions options;
  KERNEL_PARM kernel_parm;      /* svm-light structure */
  LEARN_PARM learn_parm;        /* svm-light structure */
  long kernel_cache_size;       /* svm-light variable  */
  MODEL   model;
  AFNI_MODEL afniModel;         /* read ahead of time if testing */
  THD_3dim_dataset*
    dsetTrain     = NULL;       /* pointer for training dataset */
  THD_3dim_dataset*
    dsetMask      = NULL;       /* pointer for mask dataset */
  MaskType*
    dsetMaskArray = NULL;       /* array to hold mask dataset values */
  THD_3dim_dataset*
    dsetTest      = NULL;       /* pointer for testing dataset */
  THD_3dim_dataset*
    dsetModel     = NULL;       /* pointer for model dataset */
  DatasetType**
    dsetModelArray = NULL;      /* pointer for model dataset arrray */

  long nt_model    = 0;         /* actual number of timepoints in model
    determined by DSET_NUM_TIMES. If the mask was written as a sub-brick into
    the model (that's the new default way), 2 is subtracted (since the mask is
    stored in the last two sub-bricks). We also have timepoints in AFNI_MODEL,
    which is stored in the model header (written during training) */

  long nvox_model  = 0;         /* actual number of voxels in model determined
                                   by DSET_NVOX */
  int i            = 0;
  int myargc       = 0;
  char *myargv[LONG_STRING];
  char *errorString = NULL;    /* needed for error-handling of plugin */
  enable_mcw_malloc();         /* ZSS helps locate some memory problems*/



  mainENTRY("3dsvm");           /* ZSS, see -trace option in input_parse*/
  

  /* JL June 2011: Modified error handling. Passing errorString as argument
   * to most functions and calling ERROR_exit() only from main. This gives us the 
   * flexibility to employ the same functions for plugin and command-line usage.  
   * For plugin calls, the error message is diplayed to the user 
   * as a pop-up, memory is freed properly and the afni controller stays operational!. 
   */


  /* -- allocate errorString --- */
  if( (errorString = (char *) malloc( LONG_STRING*sizeof(char)) ) == NULL ) {
    ERROR_exit("3dsvm: Memory allocation for errorString failed!");
  }
  snprintf(errorString, LONG_STRING, "What happened?! Undefined error message!");

  /*----- READ COMMAND-LINE OPTIONS FROM .afnirc -----*/
  /* JL Sep. 2010: Added capability to read in command line-options from
   * .afnirc using the -getenv flag only */
  if( (argc == 2) && (!strcmp(argv[1],"-getenv")) ) {
    INFO_message("Reading 3dsvm environment variables...");
    argvAppend(myargv, &myargc, PROGRAM_NAME,"");
    getEnvArgv(myargv, &myargc, "3DSVM_ALL_OPTIONS");
    printArgv(myargv, &myargc);
  }
  else {
    myargc = 0;
    for( i=0; i<argc; ++i ) {
      if( (myargv[i] = (char *)malloc( LONG_STRING * sizeof(char))) != NULL ) {
        strncpy(myargv[i], argv[i], LONG_STRING);
        myargc++;
      }
      else ERROR_exit("3dsvm: Memory allocation for myargv failed!");
    }
  }

  /*----- COMMAND LINE PARSE -> OPTIONS --------*/
  if( input_parse(myargc, myargv, &verbosity, &kernel_cache_size, &learn_parm,
      &kernel_parm, &options, &mode, &svm_type, errorString) ) {

    freeArgv( myargv, myargc );
    ERROR_exit(errorString);
  }

  /*---- REAL-TIME TRAIN AND TEST FUNCTIONS */
  /* JL Sep. 2010: Only available through the plugin and plugout_dirve */
  if( (mode == RT_TRAIN) || (mode == RT_TEST) ) {
    ERROR_exit("Training in real-time is only supported through the "
        "3dsvm plugin GUI or plugout_drive!");
  }

  /*----- TRAIN FUNCTIONS ---------------*/
  if( mode == TRAIN || mode == TRAIN_AND_TEST ) {
    if( svm_type == CLASSIFICATION ) {
      if( train_classification(&model, &learn_parm, &kernel_parm, &kernel_cache_size, 
            &options, dsetTrain, dsetMask, dsetMaskArray, myargc, myargv, errorString) ) {
          
        freeArgv( myargv, myargc );
        ERROR_exit(errorString);
      }
    }
    else if( svm_type == REGRESSION ) {
      if( train_regression(&model, &learn_parm, &kernel_parm, &kernel_cache_size, 
        &options, dsetTrain, dsetMask, dsetMaskArray, myargc, myargv, errorString) ) {
        

        freeArgv( myargv, myargc );
        ERROR_exit(errorString);
      }
    }
    /* we should never get here! */
    else {
      freeArgv( myargv, myargc );
      ERROR_exit("What happened?! Training type unknown!");
    }
  }
  
  /*----- TEST FUNCTIONS ---------------*/
  if( mode == TEST || mode == TRAIN_AND_TEST ) {

    /* JL May 2009: Changed the flow for testing to enable sv-regression.
     * Reading model and determining svm_type before testing. */

    if( readAllocateAfniModelAndArrays(&options, &afniModel, dsetModel, &dsetModelArray,
        &dsetMaskArray, &nt_model, &nvox_model, mode, &svm_type, errorString) ) {

      ERROR_exit(errorString);
    }
    /* printAfniModel(&afniModel) */

    if( svm_type == CLASSIFICATION ) {
      if( test_classification(&options, &model, &afniModel, dsetTest, dsetModelArray,
          dsetMaskArray, nt_model, nvox_model, myargc, myargv, errorString) ) {

        freeAfniModelAndArrays(&afniModel, dsetModelArray, dsetMaskArray, nt_model);
        freeArgv( myargv, myargc );
        ERROR_exit(errorString);
      }
    }
    else if( svm_type == REGRESSION ) {
      if( test_regression(&options, &model, &afniModel, dsetTest, dsetModelArray,
          dsetMaskArray, nt_model, nvox_model, myargc, myargv, errorString) ) {

        freeAfniModelAndArrays(&afniModel, dsetModelArray, dsetMaskArray, nt_model);
        freeArgv( myargv, myargc );
        ERROR_exit(errorString);
      }
    }
    else {
      freeAfniModelAndArrays(&afniModel, dsetModelArray, dsetMaskArray, nt_model);
      freeArgv( myargv, myargc );
      ERROR_exit("What happened?! Testing type unknown!");
    }


    /*  -- free memory -- */
    freeAfniModelAndArrays(&afniModel, dsetModelArray, dsetMaskArray, nt_model);
    freeArgv( myargv, myargc );
  }

  RETURN(0);
}
