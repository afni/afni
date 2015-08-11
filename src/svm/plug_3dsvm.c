/*****************************************************************************/
/*                                                                           */
/* plug_3dsvm.c                                                              */
/*                                                                           */
/* 3dsvm GUI and plugin for AFNI                                             */
/*                                                                           */
/* Copyright (C) 2007 Stephen LaConte                                        */
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


/* PLUG_AFNI_SVM_LIGHT */
#include "3dsvm_common.h"

#ifndef ALLOW_PLUGINS
  #error "Plugins not properly set up -- see machdep.h"
#endif

#define NKERNEL  4  /* Number of kernels */
#define NSVMTYPE 2  /* Number of svm learn types */


/* --- globals --- */
extern        RT_SVM_VARS GLOBAL_svm_vars;
static char * ASL_main( PLUGIN_interface * );  /* the entry point prototype */
static int    DBG_flag = 0;  /* set to 1 to print debugging statements */
static        PLUGIN_interface * plint = NULL ; /* AFNI plugin structure, global
                                         allows error popups for rt functions */

/* function prototypes */
void          svm_rt_callback( void * );
int           init_3dsvm_rt( char **, int , ASLoptions *, enum modes, char * ); 
void          free_rt_svm_vars( RT_SVM_VARS * );
int           test_rt( DatasetType **, long, double *, char * );
static int    drive_3dsvm_plugin ( char * );


/* ---- PLUGIN INTERFACE LAYOUT ----*/
DEFINE_PLUGIN_PROTOTYPE

PLUGIN_interface* PLUGIN_init( int ncall )
{
  /* --- declarations ---*/
  char *kernel_strlist[NKERNEL] = {"linear", "polynomial", "rbf", "sigmoid"};
  char *svmType_strlist[NSVMTYPE] = {"classification", "regression"};
  char *help_contribution_string;
  int str_length = strlen(plugin_helpstring) + strlen(contribution_string) + 2;

  /* --- register driver for plugin (See README.driver) --- */
  if (!ncall) {/* ZSS Nov 2011: Call just once at first or get yelled at */
    AFNI_driver_register( "3DSVM", drive_3dsvm_plugin ) ;
  }
  
  /* --- help and contribution string --- */
  if( (help_contribution_string = (char *) malloc(str_length*sizeof(char))) == NULL ) {
    fprintf(stderr, "**: ERROR: plug_3dsvm_rt: Can not allocate "
        "help_contribution_string\n"); return NULL;
  }

  if( ALLOW_realtime ) {
    snprintf( help_contribution_string, str_length, "%s \n %s",
        plugin_helpstring_rt, contribution_string );
  }
  else {
    snprintf( help_contribution_string, str_length, "%s \n %s",
            plugin_helpstring, contribution_string );
  }

  /*--- create new interface ---*/
  if( ncall > 0 ) return NULL;	/* only one interface */

  if( ALLOW_realtime ) {
    plint = PLUTO_new_interface("RT 3dsvm",
        "Set Real-Time Options for 3dsvm - An AFNI SVM-Light Plugin",
        help_contribution_string, PLUGIN_CALL_VIA_MENU, ASL_main);
    PLUTO_add_hint( plint , "Perform real-time SVM analysis" );
  }
  else {
    plint = PLUTO_new_interface("3dsvm", "3dsvm - An AFNI SVM Light Plugin",
    help_contribution_string, PLUGIN_CALL_VIA_MENU, ASL_main);
    PLUTO_add_hint( plint , "Perform SVM analysis");
  }

  /* --- top row options ---- */
  PLUTO_set_sequence( plint , "A:afnicontrol:dset" );
  PLUTO_set_runlabels( plint , "Run+Keep" , "Run+Close" );
  if (ALLOW_realtime) PLUTO_set_butcolor( plint , "hot" );

  /*--- Offline / Real-time ---*/
  /* the plugin crashes if none of the options are selected and the
   * plugin is closed. */
  if( ALLOW_realtime ) PLUTO_add_option( plint,"Real-time", "Real-time", TRUE);
  else PLUTO_add_option( plint, "Offline", "Offline", TRUE);

  /*---- Training ----*/
  PLUTO_add_option( plint,"Training","Training", FALSE );
  /* svm-learn type */
  PLUTO_add_string( plint, "Type", NSVMTYPE, svmType_strlist, FALSE );

  /* --- Train Data ---*/
  PLUTO_add_option( plint,"Train Data","Train Data",FALSE );

  /* train dataset chooser: */
  if (! ALLOW_realtime) {
    PLUTO_add_dataset( plint,"Dataset", ANAT_ALL_MASK, FUNC_ALL_MASK,
        DIMEN_ALL_MASK | BRICK_ALLTYPE_MASK );
  }
  /* training classification labels: */
  PLUTO_add_timeseries( plint,"Labels" );
  /* training censors: */
  PLUTO_add_timeseries( plint,"Censors" );

  /*--- Train Params ---*/
  PLUTO_add_option( plint,"Train Params","Train Params",FALSE );
  /* mask dataset chooser: */
  PLUTO_add_dataset( plint,"Mask", ANAT_ALL_MASK, FUNC_ALL_MASK,
  DIMEN_ALL_MASK | BRICK_ALLTYPE_MASK );
  /* svm light's "c" factor: */
  PLUTO_add_number( plint, "C", 0, 100000, 0, 1000, TRUE );
  /* svm light's "epsilon" parameter: */
  PLUTO_add_number( plint, "Epsilon", 0, 1000, 3, 1, TRUE);


  /*--- Kernel ---*/
  PLUTO_add_option( plint,"Kernel Params","Kernel Params", FALSE );
  /* kernel chooser: */
  PLUTO_add_string( plint, "Kernel Type", NKERNEL, kernel_strlist, FALSE );
  /* kernel function: */
  PLUTO_add_number( plint, "poly order (d)", 0, 1000, 0, 3, TRUE );
  PLUTO_add_number( plint, "rbf gamma (g)", 0, 1000, 0, 1, TRUE );
  /* PLUTO_add_number( plint, "sig/poly scale (s)", 0, 1000, 0, 1, TRUE ); */
  /* PLUTO_add_number( plint, "sig/poly offset (r)", 0, 1000, 0, 1, TRUE ); */

  /*--- Model Output ---*/
  PLUTO_add_option( plint, "Model Output", "Model Output", FALSE );
  /* name for model file output: */
  PLUTO_add_string( plint, "Prefix", 0, NULL, 19 );

  /*--- Model Inspection ---*/
  PLUTO_add_option( plint, "Model Inspection", "Model Inspection", FALSE );
  /* name for model weight vector map output: */
  PLUTO_add_string( plint, "FIM Prefix", 0, NULL, 19 );
  /* name for alpha file output */
  PLUTO_add_string( plint, "Alpha Prix (.1D)", 0, NULL, 19 );

  /*---- Testing ----*/
  PLUTO_add_option( plint,"Testing","Testing",FALSE );

  PLUTO_add_option( plint,"Test Data","Test Data",FALSE );
  if (! ALLOW_realtime) {
    /* test dataset chooser: */
    PLUTO_add_dataset( plint,"Dataset", ANAT_ALL_MASK,FUNC_ALL_MASK,
        DIMEN_ALL_MASK | BRICK_ALLTYPE_MASK );
  }
  /* model dataset chooser: */
  PLUTO_add_dataset( plint, "Model", ANAT_ALL_MASK,FUNC_ALL_MASK,
      DIMEN_ALL_MASK | BRICK_ALLTYPE_MASK );


  /*--- Predictions ---*/
  PLUTO_add_option( plint,"Predictions","Predictions",FALSE );
  /* name for prediction output:*/
  PLUTO_add_string( plint, "Prefix (.1D)", 0, NULL, 19 );

  if( !ALLOW_realtime ) {
    /*--- True Labels ---*/
    PLUTO_add_option( plint,"'True' Labels","'True' Labels",FALSE );
    /* name for true labels: */
    PLUTO_add_timeseries( plint, "File" );
  }

  /*--- Stimulus ---*/
  if( ALLOW_realtime ) {
    PLUTO_add_option( plint,"Stimulus", "Stimulus", FALSE );
    PLUTO_add_string( plint, "IP", 0, NULL, 19 );
    PLUTO_add_string( plint, "PORT", 0, NULL, 19 );
  }

  return plint;
}


static char* ASL_main( PLUGIN_interface* plint ) 
{
  /*----- declarations -----*/
  enum modes mode = NOTHING;
  ASLoptions options;

  KERNEL_PARM kernel_parm;  /* svm-light kernel parameters */
  LEARN_PARM learn_parm;    /* svm-light learn parameters */
  long kernel_cache_size;   /* svm-light kernel parameter */
  MODEL   model;            /* svm-light model */

  THD_3dim_dataset *
    dsetTrain     = NULL;  /* pointer for training dataset */
  THD_3dim_dataset *
    dsetMask      = NULL;  /* pointer for mask dataset */
  MaskType *
    dsetMaskArray = NULL;  /* array to hold mask dataset values */
  THD_3dim_dataset *
    dsetTest      = NULL;  /* pointer for testing dataset */
  THD_3dim_dataset *
    dsetModel     = NULL;  /* pointer for training model dataset */
  DatasetType **
    dsetModelArray = NULL; /* pointer to model dataset */
  THD_3dim_dataset *
    dsetModelTest = NULL;  /* pointer for input testing model dataset */

  AFNI_MODEL afniModel;    /* needed to determine learn (svm) type
                              before testing */
  MRI_IMAGE *
    tsim_train    = NULL;  /* pointer for image struct returned from
                              PLUTO_get_timeseries */
  MRI_IMAGE *
    tsim_censor   = NULL;  /* pointer for image struct returned from
                              PLUTO_get_timeseries */
  MRI_IMAGE *
    tsim_test     = NULL;  /* pointer to target classes for SVM
                              classification */
  MCW_idcode *
    idcode        = NULL;  /* idcode for option line dataset chooser */


  int svmType_index;

  char * svmType_string;    /* current learn (svm) type selection */
  char * svmType_strlist[NSVMTYPE] =    /* different learn (svm) types */
    { "classification",
      "regression"};

  char * kernel_string;     /* current kernel type selection */
  int kernel_index;
  char * kernel_strlist[NKERNEL] =     /* different kernel types */
        { "linear",
          "polynomial",
          "rbf",
          "sigmoid" };

  char * errorString = NULL;
  int svm_type = CLASSIFICATION;
  char * option_tag = NULL;
  char * myargv[LONG_STRING];
  int myargc = 0;
  char mystring[LONG_STRING];
  char * mystringPtr = NULL;
  char * err    = NULL;
  int trnFlag  = 0;
  int tstFlag  = 0;
  int maskFlag = 0;
  
  PLUGIN_option* opt;
  int iopt = 0;
  char *ept = NULL;
  long nt_model, nvox_model = 0;

  /* --- allocate an initialize --- */
  if( (err = (char *)malloc(LONG_STRING*sizeof(char))) == NULL ) {
    return("Memory allocation for err failed!");
  }
  if( (errorString = (char *)malloc(LONG_STRING*sizeof(char))) == NULL ) {
    return("Memory allocation for errorString failed!");
  }
  snprintf(errorString, LONG_STRING, "What happened?! Undefined error message!");

  argvAppend(myargv,&myargc, PROGRAM_NAME,"");
  if (DBG_flag) printArgv(myargv,&myargc);

  /* --- determine if training and testing is selected  
         combined training and testing is not allowed in real-time mode --- */
  if( ALLOW_realtime ) {
    for( iopt=0; iopt < plint->option_count; iopt++ ) {
      opt = plint->option[iopt] ;   /* option to deal with */
      if( opt == NULL ) continue ;  /* bad? */
      if( !strncmp(opt->tag, "Training", PLUGIN_STRING_SIZE) && (opt->chosen) ) {
        trnFlag = 1;
        argvAppend(myargv,&myargc,"-rt_train","");
      }
      if( !strncmp(opt->tag, "Testing", PLUGIN_STRING_SIZE) && (opt->chosen) ) {
        tstFlag = 1;
        argvAppend(myargv,&myargc,"-rt_test", "");
      }
    }
    /* -- try environment if not selected using plugout interface --  */
    if( (trnFlag == 0) && (tstFlag == 0) ) {
      ept = my_getenv("AFNI_3DSVM_RT_TRAIN");
      if( ept != NULL )  {
        if( !strncmp(ept, "YES", LONG_STRING) ) {
          trnFlag=1;
          argvAppend(myargv,&myargc,"-rt_train", "");
        }
      }
      ept = my_getenv("AFNI_3DSVM_RT_TEST");
      if( ept != NULL )  {
        if( !strncmp(ept,"YES", LONG_STRING) ) {
          tstFlag=1;
          argvAppend(myargv,&myargc,"-rt_test", "");
        }
      }
    }
    if( (trnFlag) && (tstFlag) ) {
      
      free(err);
      free(errorString);
      freeArgv( myargv, myargc );
      
      return
         "**********************************************\n"
         " 3dsvm plugin in real-time mode:              \n"
         " 'Training' and 'Testing' is not allowed      \n"
         " at the same time                             \n"
         "**********************************************";
    }
  }


  /*---- get inputs from plugin interface ----*/
  while(1) {

    option_tag = PLUTO_get_optiontag( plint ); /* step to next option line */

    if( option_tag == NULL ) {
      if( DBG_flag ) printArgv(myargv,&myargc);
      if( myargc == 1 ) {
        snprintf(err, LONG_STRING, 
            "****************************************************************\n"
            "Don't you want to do something?! Press 'Help' for more details! \n"
            "****************************************************************");
       
        freeArgv( myargv, myargc );
        free(errorString);
        return (err);
      }
      else {
        if ( input_parse(myargc, myargv, &verbosity, &kernel_cache_size,
              &learn_parm, &kernel_parm, &options, &mode, &svm_type,
              errorString) ) {
            snprintf(err, LONG_STRING,
              "****************************************************************\n"
              "%s\n"
              "****************************************************************",
              errorString);

          freeArgv( myargv, myargc );
          free(errorString);
          return (err);
        }
      }
      break;
    }

   /*---- Training -----*/
   if( strcmp(option_tag,"Training") == 0 ) {
     trnFlag = 1;

     /* get svm learn type */
     svmType_string = NULL;
     svmType_string = PLUTO_get_string( plint );
     if( svmType_string != NULL ) {
       svmType_index = PLUTO_string_index( svmType_string, NSVMTYPE, svmType_strlist );
       if (svmType_index == -1) svmType_index = 0;
       strncpy(mystring, svmType_strlist[svmType_index], LONG_STRING);
       argvAppend(myargv,&myargc,"-type", mystring);
       if (DBG_flag) printArgv(myargv,&myargc);
     }
   }
   else { /* get options form environment (e.g. .afnirc) */
     if( !argvCheck(myargv, &myargc, "-type", "" ) ) {
       getEnvArgv(myargv, &myargc, "AFNI_3DSVM_TRAIN_TYPE");
     }
   }

   /* --- Train Data --- */
   if( (strcmp(option_tag,"Train Data") == 0 ) ) {
     
     /* get training data */
     if ( !ALLOW_realtime ) {
       idcode = PLUTO_get_idcode( plint );
       dsetTrain = PLUTO_find_dset( idcode );
       if( dsetTrain != NULL ) {
         argvAppend(myargv,&myargc,"-trainvol", DSET_FILECODE(dsetTrain));
         if (DBG_flag) printArgv(myargv,&myargc);
       }
       else {
          free(err);
          free(errorString);
          freeArgv( myargv, myargc );
          return
             "*******************************************\n"
             "'Train Data' selected:                     \n"
             " But no Dataset chosen!                    \n"
             "*******************************************";
       }
     }
     
     /* get training labels */
     tsim_train = PLUTO_get_timeseries( plint );
     if( tsim_train != NULL ) {
       argvAppend(myargv,&myargc,"-trainlabels", THD_trailname(tsim_train->name,0));
       if (DBG_flag) printArgv(myargv,&myargc);
     }
     else {
       free(err);
       free(errorString);
       freeArgv( myargv, myargc );
       return
           "*******************************************\n"
           "'Train Data' selected:                     \n"
           " But Labels not chosen!                    \n"
           "*******************************************";
     }
     /* get training censors */
     if ( (tsim_censor = PLUTO_get_timeseries( plint )) != NULL ) {
       argvAppend(myargv,&myargc,"-censor", THD_trailname(tsim_censor->name,0));
       if (DBG_flag) printArgv(myargv,&myargc);
     }
   }
   else { /* get options from environment (e.g. .afnirc) */
     if( ALLOW_realtime ) {
       if( !argvCheck(myargv, &myargc, "-trainlabels", "" ) ) {
        getEnvArgv(myargv, &myargc, "AFNI_3DSVM_TRAIN_LBLS");
       }
       if( !argvCheck(myargv, &myargc, "-censor", "" ) ) {
        getEnvArgv(myargv, &myargc, "AFNI_3DSVM_CENSOR_FILE");
       }
     }
     else {
       if( !argvCheck(myargv, &myargc, "-trainvol", "" ) ) {
        getEnvArgv(myargv, &myargc, "AFNI_3DSVM_TRAIN_DSET");
       }
       if( !argvCheck(myargv, &myargc, "-trainlabels", "" ) ) {
        getEnvArgv(myargv, &myargc, "AFNI_3DSVM_TRAIN_LBLS");
       }
       if( !argvCheck(myargv, &myargc, "-censor", "" ) ) {
        getEnvArgv(myargv, &myargc, "AFNI_3DSVM_CENSOR_FILE");
       }

       trnFlag  = argvCheck(myargv, &myargc, "-trainvol", "");
       if (DBG_flag) printArgv(myargv,&myargc);
     }
   }

   /*--- Train Params ---*/
   if( trnFlag ) {
     if( (strcmp(option_tag,"Train Params") == 0) ) {

       /* get mask dataset */
       idcode = PLUTO_get_idcode( plint );
       dsetMask = PLUTO_find_dset( idcode );
       if( dsetMask ) {
         maskFlag = 1;
         argvAppend(myargv,&myargc,"-mask",DSET_FILECODE(dsetMask));
         if (DBG_flag) printArgv(myargv,&myargc);
       }
       else {
         free(err);
         free(errorString);
         freeArgv( myargv, myargc );

          return
             "*******************************************\n"
             "'Train Parms' selected:                    \n"
             " But no Mask chosen!                       \n"
             "*******************************************";
       }

       /* get "C" parameter */
       strncpy(mystring, "\0", LONG_STRING);
       sprintf(mystring,"%lf", (double)PLUTO_get_number( plint ));
       if( strncmp(mystring, "\0", LONG_STRING) ) {
         argvAppend(myargv,&myargc,"-c",mystring);
         if (DBG_flag) printArgv(myargv,&myargc);
       }

       /* get "Epsilon" parameter */
       strncpy(mystring, "\0", LONG_STRING);
       sprintf(mystring,"%lf", (double)PLUTO_get_number( plint ));
       if( strncmp(mystring, "\0", LONG_STRING) ) {
         argvAppend(myargv,&myargc,"-e",mystring);
         if (DBG_flag) printArgv(myargv,&myargc);
       }
     }
     else {
       if( !argvCheck(myargv, &myargc, "-mask", "" ) ) {
        getEnvArgv(myargv, &myargc, "AFNI_3DSVM_MASK_DSET");
       }
       if( !argvCheck(myargv, &myargc, "-mask", "" ) ) {
         getEnvArgv(myargv, &myargc, "AFNI_3DSVM_NOMASK");
       }
       if( !argvCheck(myargv, &myargc, "-c", "" ) ) {
        getEnvArgv(myargv, &myargc, "AFNI_3DSVM_C"); 
       }
       if( !argvCheck(myargv, &myargc, "-e", "" ) ) {
        getEnvArgv(myargv, &myargc, "AFNI_3DSVM_EPSILON"); 
       }

       maskFlag  = argvCheck(myargv, &myargc, "-mask", "");
     }
   }

   /* --- Kernel Params --- */
   if ( trnFlag ) {
     if( (strcmp(option_tag,"Kernel Params") == 0) ) {

       /* get kernel type */
       strncpy(mystring, "\0", LONG_STRING);
       kernel_string = PLUTO_get_string( plint );
       if( strncmp(mystring, "\0", LONG_STRING) ) {
         kernel_index = PLUTO_string_index( kernel_string, NKERNEL, kernel_strlist );
         strncpy(mystring, kernel_strlist[kernel_index], LONG_STRING);
         argvAppend(myargv,&myargc,"-kernel", mystring);
         if (DBG_flag) printArgv(myargv,&myargc);
       }

       /* get kernel parameter "d" */
       strncpy(mystring, "\0", LONG_STRING);
       sprintf(mystring,"%d", (int)PLUTO_get_number( plint ));
       if( strncmp(mystring, "\0", LONG_STRING) ) {
         argvAppend(myargv,&myargc,"-d", mystring);
         if( DBG_flag ) printArgv(myargv,&myargc);
       }

       /* get kernel parameter "g" */
       strncpy(mystring, "\0", LONG_STRING);
       sprintf(mystring,"%lf", (double)PLUTO_get_number( plint ));
       if( strncmp(mystring, "\0", LONG_STRING) ) {
         argvAppend(myargv,&myargc,"-g", mystring);
         if( DBG_flag ) printArgv(myargv,&myargc);
       }
     }
     else {
       if( !argvCheck(myargv, &myargc, "-kernel", "" ) ) {
        getEnvArgv(myargv, &myargc, "AFNI_3DSVM_KERNEL_TYPE"); 
       }
       if( !argvCheck(myargv, &myargc, "-d", "" ) ) {
        getEnvArgv(myargv, &myargc, "AFNI_3DSVM_KERNEL_PARM_D"); 
       }
       if( !argvCheck(myargv, &myargc, "-g", "" ) ) {
        getEnvArgv(myargv, &myargc, "AFNI_3DSVM_KERNEL_PARM_G"); 
       }
       if( !argvCheck(myargv, &myargc, "-s", "" ) ) {
        getEnvArgv(myargv, &myargc, "AFNI_3DSVM_KERNEL_PARM_S"); 
       }
       if( !argvCheck(myargv, &myargc, "-r", "" ) ) {
        getEnvArgv(myargv, &myargc, "AFNI_3DSVM_KERNEL_PARM_R"); 
       }
     }
   }

   /* --- Model Output --- */
   if( trnFlag ) {
     if( (strcmp(option_tag, "Model Output") == 0) ) {

       /* get output file for model */
       mystringPtr = NULL;
       mystringPtr = PLUTO_get_string( plint );
       if( mystringPtr != NULL ) {
         argvAppend(myargv,&myargc,"-model",mystringPtr);
         if (DBG_flag) printArgv(myargv,&myargc);

         if( !PLUTO_prefix_ok(mystringPtr) ) {

           free(err);
           free(errorString);
           freeArgv( myargv, myargc );
          
           return
             "*******************************************\n"
             "'Model Output' selected:                   \n"
             " Bad Training Model file name given,       \n"
             " file already exists or illegal name used  \n"
             "*******************************************";
         }
       }
     }
     else {
       if( !argvCheck(myargv, &myargc, "-model", "" ) ) {
         getEnvArgv(myargv, &myargc, "AFNI_3DSVM_MODEL_DSET");
       }
     }
   }

   /* --- Model Inspection ---*/
   if( trnFlag ) {
     if( (strcmp(option_tag,"Model Inspection") == 0) ) {
       /* get output file for model */
       mystringPtr = PLUTO_get_string( plint );

       if( PLUTO_prefix_ok(mystringPtr) ) {
         argvAppend(myargv,&myargc,"-bucket",mystringPtr);
         if (DBG_flag) printArgv(myargv,&myargc);
       }
       else {
         free(err);
         free(errorString);
         freeArgv( myargv, myargc );
         
         return
            "**********************************************\n"
            " 'Model Inspection' selected:                 \n"
            " FIM output file already exists  ,            \n"
            " illegal name used, or no file name entered.  \n"
            "**********************************************";
       }

       /* get output file for alphas */
       mystringPtr = PLUTO_get_string( plint );
       if( PLUTO_prefix_ok(mystringPtr) ) {
         argvAppend(myargv,&myargc,"-alpha",mystringPtr);
         if (DBG_flag) printArgv(myargv,&myargc);
       }
       else {
         free(err);
         free(errorString);
         freeArgv( myargv, myargc );
         
         return
           "**********************************************\n"
           " 'Model Inspection' selected:                 \n"
           " Alpha output file already exists,            \n"
           " illegal name used, or no file name entered.  \n"
           "**********************************************";
       }
     }
     else {
       if( !argvCheck(myargv, &myargc, "-bucket", "" ) ) {
        getEnvArgv(myargv, &myargc, "AFNI_3DSVM_BUCKET_DSET"); 
       }
       if( !argvCheck(myargv, &myargc, "-alpha", "" ) ) {
        getEnvArgv(myargv, &myargc, "AFNI_3DSVM_ALPHA_FILE");
       }
     }
   }


   /* ---- Testing ----- */
   if( strcmp(option_tag,"Testing") == 0 ) {
     tstFlag = 1;
   }
   else { 
     if( !argvCheck(myargv, &myargc, "-testvol", "") ) {
       getEnvArgv(myargv, &myargc, "AFNI_3DSVM_TEST_DSET");
     }
     if( !tstFlag ) {
       tstFlag = argvCheck(myargv, &myargc, "-testvol","");
     }
   }

   /* --- Test Data --- */
   if( tstFlag ) {
     if( (strcmp(option_tag,"Test Data") == 0) ) {
        if( !ALLOW_realtime ) {
          /* get test dataset */
          idcode = PLUTO_get_idcode( plint );
          dsetTest =  PLUTO_find_dset( idcode );
          if( dsetTest != NULL ) {
            argvAppend(myargv,&myargc,"-testvol", DSET_FILECODE(dsetTest));
            if (DBG_flag) printArgv(myargv,&myargc);
          }
        }

        /* get model dataset */
        if ( !trnFlag ) {
          idcode = PLUTO_get_idcode( plint );
          dsetModelTest = PLUTO_find_dset( idcode );

          if( dsetModelTest != NULL ) {
            argvAppend(myargv,&myargc,"-model",DSET_FILECODE(dsetModelTest));
            if (DBG_flag) printArgv(myargv,&myargc);
          }
        }
      }
      else {
        if( !argvCheck(myargv, &myargc, "-model", "" ) ) {
          getEnvArgv(myargv, &myargc, "AFNI_3DSVM_MODEL_DSET");
        }
      }
    }

    /* --- Stimulus --- */
    if( tstFlag ) {
      if( (strcmp(option_tag,"Stimulus") == 0) ) {
        /* get IP address */
        strncpy(mystring, "\0", LONG_STRING);
        strncpy( mystring, PLUTO_get_string(plint), LONG_STRING);
        if( strncmp(mystring, "\0", LONG_STRING) ) {
          argvAppend(myargv,&myargc,"-stim_ip", mystring);
          if (DBG_flag) printArgv(myargv,&myargc);
        }
        else {
          free(err);
          free(errorString);
          freeArgv( myargv, myargc );
          
          return
            "**********************************************\n"
            " 'Stimulus' selected, but no IP entered!        \n"
            "**********************************************";
        }

        /* get Port */
        strncpy(mystring, "\0", LONG_STRING);
        strncpy( mystring, PLUTO_get_string(plint), LONG_STRING);
        if( strncmp(mystring, "\0", LONG_STRING) ) {
          argvAppend(myargv,&myargc,"-stim_port", mystring);
          if (DBG_flag) printArgv(myargv,&myargc);
        }
        else {
          free(err);
          free(errorString);
          freeArgv( myargv, myargc );
          
          return
            "**********************************************\n"
            " 'Stimulus' selected, but no PORT entered!      \n"
            "**********************************************";
        }
      }
      else {
        if( ALLOW_realtime ) {
          if( !argvCheck(myargv, &myargc, "-stim_ip", "" ) ) {
            getEnvArgv(myargv, &myargc, "AFNI_3DSVM_RT_IP");
          }
          if( !argvCheck(myargv, &myargc, "-stim_port", "" ) ) {
            getEnvArgv(myargv, &myargc, "AFNI_3DSVM_RT_PORT");
          }
        }
      }
    }

    /* --- Predictions --- */
    if( tstFlag ) {
      if( (strcmp(option_tag,"Predictions") == 0) ) {

        /* get prediction file */
        mystringPtr = PLUTO_get_string( plint );
        if( PLUTO_prefix_ok(mystringPtr) ) {
          argvAppend(myargv,&myargc,"-predictions",mystringPtr);
          if (DBG_flag) printArgv(myargv,&myargc);
        }
        else {
          free(err);
          free(errorString);
          freeArgv( myargv, myargc );
          
          return
            "**********************************************\n"
  	    " 'Predictions' selected                       \n"
  	    " Prediction output file already exists,       \n"
  	    " illegal name used, or no file name entered.  \n"
  	    "**********************************************\n";
        }
      }
      else {
        if( !argvCheck(myargv, &myargc, "-predictions", "" ) ) {
          getEnvArgv(myargv, &myargc, "AFNI_3DSVM_PRED_FILE");
        }
      }
    }

    /* --- True Labels --- */
    if( tstFlag ) {
      if( (strcmp(option_tag,"'True' Labels") == 0) ) {

        /* get test label file */
        tsim_test = PLUTO_get_timeseries( plint );   /* training labels */
        if( tsim_test != NULL ) {
          argvAppend(myargv,&myargc,"-testlabels",THD_trailname(tsim_test->name,0));
          if (DBG_flag) printArgv(myargv,&myargc);
        }
      }
      else {
        if( !argvCheck(myargv, &myargc, "-testlabels", "" ) ) {
          getEnvArgv(myargv, &myargc, "AFNI_3DSVM_TEST_LBLS");
        }
      }
    }

  } /* end while(1)  */


  if (DBG_flag) printArgv(myargv,&myargc);
  /* --- some error checking ---*/
  if( (trnFlag) && (!maskFlag) ) {
    if( !argvCheck(myargv, &myargc, "-nomodelmask", "") ) {

      free(err);
      free(errorString);
      freeArgv( myargv, myargc );

      return
          "**********************************************\n"
          " 'Training' selected:                         \n"
          " You must specify a mask file under           \n"
          " 'Train Params'                               \n"
          "**********************************************\n";
#if 0     
          "                                              \n"
          " You can also set environment variable:       \n"
          " AFNI_3DSVM_NOMASK=YES                        \n"
          " and restart AFNI                             \n"
          "**********************************************"; 
#endif

    }
  }

  if( ALLOW_realtime ) {
    if( (mode != RT_TRAIN) && (mode != RT_TEST) ) {

      free(err);
      free(errorString);
      freeArgv( myargv, myargc );

      return
        "**********************************************\n"
        " 3dsvm plugin in real-time mode:              \n"
        " Must select 'Training' or 'Testing'!         \n"
        "**********************************************";
    }
  }

  /*----- REAL-TIME CALLBACK FOR TRAINING AND TESTING ---------------*/
  if( mode == RT_TRAIN || mode == RT_TEST) {
    if( ALLOW_realtime ) {

      if( init_3dsvm_rt(myargv, myargc, &options, mode, errorString) ) {
        snprintf(err, LONG_STRING,
            "****************************************************************\n"
            "%s\n"
            "****************************************************************",
            errorString);

        /* reset global afni callback function */
        GLOBAL_library.realtime_callback = NULL; 

        freeArgv( myargv, myargc );
        free(errorString);
        return (err);
      }
    }
    else {
      freeArgv( myargv, myargc );
      free(err);

      return
        "**********************************************\n"
        "Real-time not enabled!                        \n"
        "**********************************************";
    }
  }

  /* --- OFFLINE TRAIN FUNCTIONS ------------- */
  if( mode == TRAIN || mode == TRAIN_AND_TEST ) {
    if( svm_type == CLASSIFICATION ) {
      
      if( train_classification(&model, &learn_parm, &kernel_parm, &kernel_cache_size,
             &options, dsetTrain, dsetMask, dsetMaskArray, myargc, myargv, errorString) ) {
        
        snprintf(err, LONG_STRING,
            "****************************************************************\n"
            "%s\n"
            "****************************************************************",
            errorString);
        
        freeArgv( myargv, myargc );
        return (err);
      }
    }
    else if( svm_type == REGRESSION ) {
      if( train_regression(&model, &learn_parm, &kernel_parm, &kernel_cache_size,
              &options, dsetTrain, dsetMask, dsetMaskArray, myargc, myargv, errorString) ) {
        
        snprintf(err, LONG_STRING,
            "****************************************************************\n"
            "%s\n"
            "****************************************************************",
            errorString);
        
        freeArgv( myargv, myargc );
        return(err);
      }
    }
    else {
      free(err);
      freeArgv( myargv, myargc );

      return
         "**********************************************\n"
         " svm learn type not supported!                \n"
         "**********************************************";
    }
    if( mode != TRAIN_AND_TEST ) {
    
      /*---- print svm light copyright ----*/
      copyright_notice();

      /* --- print command-line ---*/
      printf("\n\n  ");
      printArgv( myargv,&myargc );
      printf("\n");

      free(err);
      freeArgv( myargv, myargc );
    
      printf("  ---3dsvm plugin run done---\n");
    }
  }

  /*----- OFFLINE TEST FUNCTIONS ---------------*/
  if( mode == TEST || mode == TRAIN_AND_TEST ) {

    /* JL May 2009: Changed the flow for testing to enable sv-regression.
     * Reading model and determining svm_type before testing. */

    if( readAllocateAfniModelAndArrays(&options, &afniModel, dsetModel, &dsetModelArray,
       &dsetMaskArray, &nt_model, &nvox_model, mode, &svm_type, errorString) ) {
        
        snprintf(err, LONG_STRING,
            "****************************************************************\n"
            "%s\n"
            "****************************************************************",
            errorString);
        
        freeArgv( myargv, myargc );
        return(err);
    }
        
    if( svm_type == CLASSIFICATION ) {
      if( test_classification(&options, &model, &afniModel, dsetTest, dsetModelArray,
          dsetMaskArray, nt_model, nvox_model, myargc, myargv, errorString) ) {

        snprintf(err, LONG_STRING,
            "****************************************************************\n"
            "%s\n"
            "****************************************************************",
            errorString);
        
        freeAfniModelAndArrays( &afniModel, dsetModelArray, dsetMaskArray, nt_model );
        freeArgv( myargv, myargc );
        return(err);
      }
    }
    else if( svm_type == REGRESSION ) {
      if( test_regression(&options, &model, &afniModel, dsetTest, dsetModelArray,
          dsetMaskArray, nt_model, nvox_model, myargc, myargv, errorString) ) {
        
        snprintf(err, LONG_STRING,
            "****************************************************************\n"
            "%s\n"
            "****************************************************************",
            errorString);
        
        freeAfniModelAndArrays( &afniModel, dsetModelArray, dsetMaskArray, nt_model );
        freeArgv( myargv, myargc );
        return(err);
      }
    }
    else {
      freeAfniModelAndArrays( &afniModel, dsetModelArray, dsetMaskArray, nt_model );
      freeArgv( myargv, myargc );
      free(err);
      
      return
        "**********************************************\n"
        " svm learn type not supported!                \n"
        "**********************************************";
    }
    
    /*  --- free model variables  --- */
    freeAfniModelAndArrays( &afniModel, dsetModelArray, dsetMaskArray, nt_model );
    
    /*---- print svm light copyright ----*/
    copyright_notice();

    /* --- print command-line ---*/
    printf("\n\n  ");
    printArgv( myargv,&myargc );
    printf("\n");

    free(err);
    freeArgv( myargv, myargc );
    
    printf("  ---3dsvm plugin run done---\n");
  
  }

  return NULL;
}

void svm_rt_callback(void *junk)
{

  RT_status *rts               = GLOBAL_library.realtime_status;

  MODEL rt_model;

  DatasetType **
    rt_testArray               = NULL;  /* holds last sub-brik that has been
    sent to afni. Declared this as 2D array out of convenience, since we already
    have functions that convert 2D arrays to svm-light data structures */

  THD_3dim_dataset *
    rt_dsetMask                = NULL;

  double rt_dist               = 0;
  long rt_nvox                 = 0;
  int rt_datum                 = 0;
  int rt_nt                    = 0;   /* number of briks (timepoints) that
                                         have been sent to afni (increases every TR) */

  long v                       = 0;
  int  ii                      = 0;
  char SVM_buf[LONG_STRING];
  char *rt_errorString        = NULL;
  char *err                   = NULL;

  /* -- allocate rt_errorString --- */
  if( (rt_errorString = (char *) malloc(LONG_STRING*sizeof(char))) == NULL ) {
    fprintf(stderr, "CB: 3dsvm: ERROR: Memory allocation for rt_errorString failed!\n"); 
    return;
  }
  if( (err = (char *) malloc(LONG_STRING*sizeof(char))) == NULL ) {
    fprintf(stderr, "CB: 3dsvm: ERROR: Memory allocation for err failed!\n"); 
    return;
  }

  /* ----- STARTUP ----- */
  if( rts->status == RT_STARTUP ) {
    /* --- initialize --- */

    /* -- initialization for real-time training -- */
    if( GLOBAL_svm_vars.mode == RT_TRAIN ) {
      fprintf(stderr,"CB: 3dsvm: training will start after entire "
          "dataset is acquired!\n");
    }
    /* -- initialization for real-time testing -- */
    else if( GLOBAL_svm_vars.mode == RT_TEST ) {
      fprintf(stderr, "CB: 3dsvm: testing in real-time!\n");

    }
    else {
      snprintf(rt_errorString, LONG_STRING, "What happened?! Real-time mode unknown!");
      fprintf(stderr, "CB: 3dsvm: ERROR: %s\n", rt_errorString);
      snprintf(err, LONG_STRING, "3dsvm plugin:\n ERROR: %s\n", rt_errorString);
      PLUTO_popup_transient( plint , err);
    
      /* free and return */
      free(rt_errorString);
      free(err);
      
      return;
    }
  }

  /* ----- CONTINUE ----- */
  else if( rts->status == RT_CONTINUE ) {

    /* --- real-time testing while data is being acquired and send to afni --- */
    if( GLOBAL_svm_vars.mode == RT_TEST ) {
      /* -- get dataset specs -- */
      rt_nt    = DSET_NUM_TIMES(rts->dset[rts->numdset-1]);
      rt_nvox  = DSET_NVOX(rts->dset[rts->numdset-1]);
      rt_datum = DSET_BRICK_TYPE(rts->dset[rts->numdset-1], rt_nt-1);

      /* -- allocate rt_testArray -- */
      /* 2D out of convenience */
      if ( (rt_testArray = (DatasetType **) Allocate2DT(1, rt_nvox)) == NULL ) {
        snprintf(rt_errorString, LONG_STRING, "Allocating rt_testArray failed!");
        fprintf(stderr, "CB: 3dsvm: ERROR: %s\n", rt_errorString);
        snprintf(err, LONG_STRING, "3dsvm plugin:\n ERROR: %s\n", rt_errorString);
        PLUTO_popup_transient( plint , err);
        
        /* free and return */
        free(rt_errorString);
        free(err);
        
        return;
      }

      /* -- extract last sub-brik that was sent to afni -- */
      if( rt_datum == MRI_short ) {
        short *tmp_dsetArray = (short *) DSET_ARRAY(rts->dset[rts->numdset-1], rt_nt-1);
        for( v=0; v<rt_nvox; ++v ) rt_testArray[0][v] = (DatasetType) tmp_dsetArray[v];
      }
      else if( rt_datum == MRI_float ) {
        float *tmp_dsetArray = (float *) DSET_ARRAY(rts->dset[rts->numdset-1], rt_nt-1);
        for( v=0; v<rt_nvox; ++v ) rt_testArray[0][v] = (DatasetType) tmp_dsetArray[v];
      }
      else {
        snprintf(rt_errorString, LONG_STRING, "Sorry, only datum type "
            "short and float is supported");
        fprintf(stderr, "CB: 3dsvm: ERROR: %s\n", rt_errorString);
        snprintf(err, LONG_STRING, "3dsvm plugin:\n ERROR: %s\n", rt_errorString);
        PLUTO_popup_transient( plint , err);
        
        /* free and return */
        free2DT(rt_testArray, 1);
        free(rt_errorString);
        free(err);
        
        return;
      }

      /* -- perform testing -- */
      if( test_rt(rt_testArray, rt_nvox, &rt_dist, rt_errorString) ) {
        fprintf(stderr, "CB: 3dsvm: ERROR: %s\n", rt_errorString);
        snprintf(err, LONG_STRING, "3dsvm plugin:\n ERROR: %s\n", rt_errorString);
        PLUTO_popup_transient( plint , err);
        
        /* free and return */
        free2DT(rt_testArray, 1);
        free(rt_errorString);
        free(err);
        
        return;
      }

      /* -- write distance to prediction file -- */
      fprintf(stderr,"CB: 3dsvm: Distance to hyper-plane = %6.3lf\n", rt_dist);
      if( GLOBAL_svm_vars.options->predFile[0] ) {
        fprintf(GLOBAL_svm_vars.fp_pred, "%.8g\n", rt_dist);
      }
      
      /* -- send distance to SVM host -- */
      if( GLOBAL_svm_vars.SVM_HOST_OK ) {
        snprintf(SVM_buf, LONG_STRING, "%6.4lf", rt_dist );
        /*snprintf(SVM_buf, LONG_STRING,"tcp_dist %6.4lf", rt_dist ); */
        ii = iochan_sendall( GLOBAL_svm_vars.SVM_ioc, SVM_buf, strlen(SVM_buf)+1 );

        if( ii < 0 ) {
          fprintf(stderr, "CB: 3dsvm: WARNING: Sending data to SVM host failed!\n");
        }
      }

      /* --- free rt_testArray --- */
      free2DT(rt_testArray, 1);
    }
  }

  /* ----- FINISHED ----- */
  else if( rts->status == RT_FINISHED ) {

    /* reset global afni callback function */
    GLOBAL_library.realtime_callback = NULL; 

    /* --- training after data acquisition is finished --- */
    if( GLOBAL_svm_vars.mode == RT_TRAIN ) {
      fprintf(stderr, "CB: 3dsvm: Calling training function...\n");
      if( GLOBAL_svm_vars.svm_type == CLASSIFICATION ) {
        if( train_classification(&rt_model, 
               GLOBAL_svm_vars.learn_parm, GLOBAL_svm_vars.kernel_parm, 
              &GLOBAL_svm_vars.kernel_cache_size,
               GLOBAL_svm_vars.options, rts->dset[rts->numdset-1], rt_dsetMask,
               GLOBAL_svm_vars.dsetMaskArray, GLOBAL_svm_vars.myargc,
               GLOBAL_svm_vars.myargv, rt_errorString) ) {
        
          fprintf(stderr, "CB: 3dsvm: ERROR: %s\n", rt_errorString);
          snprintf(err, LONG_STRING, "3dsvm plugin:\n ERROR: %s\n", rt_errorString);
          PLUTO_popup_transient( plint , err);
        
          /* free and return */
          free_rt_svm_vars(&GLOBAL_svm_vars);
          free(rt_errorString);
          free(err);

          return;
        }
      }
      else if( GLOBAL_svm_vars.svm_type == REGRESSION ) {
        if( train_regression(&rt_model, 
               GLOBAL_svm_vars.learn_parm, GLOBAL_svm_vars.kernel_parm, 
               &GLOBAL_svm_vars.kernel_cache_size,
               GLOBAL_svm_vars.options, rts->dset[rts->numdset-1], rt_dsetMask,
               GLOBAL_svm_vars.dsetMaskArray, GLOBAL_svm_vars.myargc,
               GLOBAL_svm_vars.myargv, rt_errorString) ) {
        
          fprintf(stderr, "CB: 3dsvm: ERROR: %s\n", rt_errorString);
          snprintf(err, LONG_STRING, "3dsvm plugin:\n ERROR: %s\n", rt_errorString);
          PLUTO_popup_transient( plint , err);
        }

        /* free and return */
        free_rt_svm_vars(&GLOBAL_svm_vars);
        free(rt_errorString);
        free(err);

        return; 
      }
      else {
        snprintf(rt_errorString, LONG_STRING, 
            "What happend?! Real-time train type is unknown!");
        fprintf(stderr, "CB: 3dsvm: ERROR: %s\n", rt_errorString);
        snprintf(err, LONG_STRING, "3dsvm plugin:\n ERROR: %s\n", rt_errorString);
        PLUTO_popup_transient( plint , err);
        
        /* free and return */
        free_rt_svm_vars(&GLOBAL_svm_vars);
        free(rt_errorString);
        free(err);

        return; 
      }
    }
    else if( GLOBAL_svm_vars.mode == RT_TEST ) {
      if( GLOBAL_svm_vars.SVM_HOST_OK == 1 ) {
        IOCHAN_CLOSE( GLOBAL_svm_vars.SVM_ioc );
      }
      if( GLOBAL_svm_vars.options->predFile[0] ) {
        fclose(GLOBAL_svm_vars.fp_pred);
      }

      free_rt_svm_vars(&GLOBAL_svm_vars);
    }
  }

  else {
     snprintf(rt_errorString, LONG_STRING,
         "What happened?! Real-time status unknown!");
     fprintf(stderr, "CB: 3dsvm: ERROR: %s\n", rt_errorString);
     snprintf(err, LONG_STRING, "3dsvm plugin:\n ERROR: %s\n", rt_errorString);
     PLUTO_popup_transient( plint , err);

     /* free and return */
     free(rt_errorString);
     free(err);
        
     return; 
  }
}

int init_3dsvm_rt( char **myargv, int myargc, ASLoptions *options, enum modes mode, char *errorString ) 
{

  THD_3dim_dataset *dsetModel     = NULL;
  int i, ii, ctry                 = 0;
  char predictionsFile[LONG_STRING];

  /* -- some error checking --*/
  if( !ALLOW_realtime ) {
    snprintf(errorString, LONG_STRING, 
        "Initializing 3dsvm real-time plugin failed!\n "
        "    AFNI is not in real-time mode\n\n"
        "    Lunch afni with option: -rt (afni -rt)!");

    return 1;
  }

  if( (mode != RT_TRAIN) && (mode != RT_TEST) ) {
    snprintf(errorString, LONG_STRING, 
        "Initializing 3dsvm real-time plugin failed!\n" 
        "    3dsvm is not in real-time mode!\n"
        "    Drive the plugin with:\n"
        "      option: -rt_train *OR*\n"
        "      option: -rt_test\n");
#if 0
        "    Alternatively you can set environment variable:\n"
        "      AFNI_3DSVM_RT_TRAIN=YES *OR*\n"
        "      AFNI_3DSVM_RT_TEST=YES!");
#endif 

    return 1;
  }


  /* --- setting global afni callback function to be invoked as
  * svm_rt_callback(void *junk) - junk will be NULL --- */
  GLOBAL_library.realtime_callback = svm_rt_callback;

  /* --- allocate and initialize myargv, myargc in GLOBAL_svm_vars  */
  /* afni's real-time callback functionality is not set up to pass
   * arguments to the callback function, so we have to go with globals
   */

  GLOBAL_svm_vars.options = (ASLoptions *)malloc(sizeof(ASLoptions));
  GLOBAL_svm_vars.myargc  = myargc;

  if( (GLOBAL_svm_vars.myargv = Allocate2c(myargc, LONG_STRING)) == NULL ) {
    snprintf(errorString, LONG_STRING, "init_3dsvm_rt: "
        "Memory allocation for GLOBAL_svm_vars.myargv failed!");

    return 1;
  }
  for( i=0; i<myargc; ++i ) strncpy(GLOBAL_svm_vars.myargv[i], myargv[i], LONG_STRING);
  
  /* --- allocate and initialize svm-light parameters in GLOBAL_svm_vars */ 
  GLOBAL_svm_vars.learn_parm  = (LEARN_PARM  *)malloc(sizeof(LEARN_PARM));
  GLOBAL_svm_vars.kernel_parm = (KERNEL_PARM *)malloc(sizeof(KERNEL_PARM));

  if( input_parse(GLOBAL_svm_vars.myargc,  GLOBAL_svm_vars.myargv, &verbosity, 
       &GLOBAL_svm_vars.kernel_cache_size, GLOBAL_svm_vars.learn_parm, 
        GLOBAL_svm_vars.kernel_parm,       GLOBAL_svm_vars.options, 
       &GLOBAL_svm_vars.mode,             &GLOBAL_svm_vars.svm_type, errorString) ) {
    
    /* we should never get here, input_parse is used to initialize and has
       been already called without any conflicts */
    free2c(GLOBAL_svm_vars.myargv, GLOBAL_svm_vars.myargc);
    return 1;
  }

  /* -- initialize GLOBAL_svm_vars for communication with SVM_host -- */
  strcpy(GLOBAL_svm_vars.SVM_iochan, "\0");
  GLOBAL_svm_vars.SVM_ioc = NULL;
  GLOBAL_svm_vars.SVM_HOST_OK = 0;

  /* --- real-time training --- */
  if( mode == RT_TRAIN ) {
     fprintf(stderr, "++  3dsvm: Ready for real-time training!\n");
  }

  /* --- real-time testing --- */
  if( mode == RT_TEST ) {
    
    /* -- read afni model -- */
    fprintf(stderr, "++  3dsvm: Reading model: %s...\n", options->modelFile);
    if( THD_open_one_dataset(options->modelFile) == NULL ) {

      /* TODO: Need some sort of search for a model here!
       *       Don't want to use system calls though...
       *       Maybe a default name will suffice 
       */
      snprintf(errorString, LONG_STRING, "Could not locate model dataset: %s!",
          options->modelFile);

      free2c(GLOBAL_svm_vars.myargv, GLOBAL_svm_vars.myargc);
      return 1;
    }

    GLOBAL_svm_vars.afniModel = (AFNI_MODEL *)malloc(sizeof(AFNI_MODEL));
    if( readAllocateAfniModelAndArrays(options,
         GLOBAL_svm_vars.afniModel, dsetModel,
         &GLOBAL_svm_vars.dsetModelArray, &GLOBAL_svm_vars.dsetMaskArray,
         &GLOBAL_svm_vars.nt_model, &GLOBAL_svm_vars.nvox_model, GLOBAL_svm_vars.mode,
         &GLOBAL_svm_vars.svm_type, errorString) ) {
      
      free2c(GLOBAL_svm_vars.myargv, GLOBAL_svm_vars.myargc);
      return 1;
    }

    /* -- open prediction file */
    if( options->predFile[0] ) {
      snprintf(predictionsFile, LONG_STRING, "%s.1D", options->predFile);
      
      if( (GLOBAL_svm_vars.fp_pred = fopen(predictionsFile, "w")) == NULL ) {
        snprintf(errorString, LONG_STRING, "Could not open prediction file: %s!", 
            predictionsFile);

          free2c(GLOBAL_svm_vars.myargv, GLOBAL_svm_vars.myargc);
          return 1;
      }
    }

    /* -- open connection to SVM host  -- */
    /*
     * *** adapted from rtfeedme ***
     */

    if( options->rtIP[0] ) {
      fprintf(stderr, "++  3dsvm: Initializing I/O to SVM host (IP: %s, PORT: %d)\n",
          options->rtIP, options->rtPort);

      snprintf(GLOBAL_svm_vars.SVM_iochan, 128, "tcp:%s:%d", options->rtIP,
          options->rtPort);

      /* - create socket - */
      GLOBAL_svm_vars.SVM_ioc = iochan_init( GLOBAL_svm_vars.SVM_iochan, "create" );

      if( GLOBAL_svm_vars.SVM_ioc == NULL ) {
        fprintf(stderr, "+*  3dsvm: WARNING: Can not open connection to %s\n",
        GLOBAL_svm_vars.SVM_iochan); GLOBAL_svm_vars.SVM_HOST_OK = 0;
      }

      /* - check if socket is connected to SVM_host - */
      else {
        for( ctry=0; ctry<SVM_HOST_NCTRY; ++ctry ) {
          /* check; wait at most 1ms */
          ii = iochan_writecheck( GLOBAL_svm_vars.SVM_ioc, 1 );

          if( ii <= 0 ) continue;   /* connection failed or pending  */
          else if( ii > 0 ) {       /* connection established */
            fprintf(stderr, "++  3dsvm: Connection to SVM host established!\n");
            GLOBAL_svm_vars.SVM_HOST_OK = 1;
            break;
          }
        }
      }
      if( !GLOBAL_svm_vars.SVM_HOST_OK ) {
        IOCHAN_CLOSE( GLOBAL_svm_vars.SVM_ioc );
        fprintf(stderr, "*+  3dsvm: WARNING: Could not establish connection "
            "to SVM host!\n");
      }
    }

    fprintf(stderr, "++  3dsvm: Ready for real-time testing!\n");
  }

  return 0;
}

void free_rt_svm_vars( RT_SVM_VARS *svm_vars ) {

  if( svm_vars->mode == RT_TRAIN ) {
    freeArgv(svm_vars->myargv, svm_vars->myargc);
  }
  else if( svm_vars->mode == RT_TEST ) {
    freeAfniModelAndArrays( svm_vars->afniModel, svm_vars->dsetModelArray, 
        svm_vars->dsetMaskArray,svm_vars->nt_model );
    freeArgv(svm_vars->myargv, svm_vars->myargc);
  }

  return;
}

int test_rt( DatasetType **currTestArray, long nvox_dset, double *dist, char *errorString )
{

  DOC   *currTestDoc         = NULL;
  MODEL *svmlModel           = NULL;
  int kernel_type            = (GLOBAL_svm_vars.afniModel)->kernel_type[0];
  int nvox_masked            = (GLOBAL_svm_vars.afniModel)->total_masked_features[0];


  /* ----- SOME ERROR CHECKING ---- */
  if( (currTestArray == NULL) ||
      (GLOBAL_svm_vars.afniModel == NULL) ||
      (GLOBAL_svm_vars.dsetModelArray == NULL) ||
      (GLOBAL_svm_vars.dsetMaskArray == NULL) ||
      (GLOBAL_svm_vars.options == NULL) ) {
    
    snprintf(errorString, LONG_STRING, "What happened? Memory gone bad?!");

     *dist = 0.0;
     return 1;
  }

  if( GLOBAL_svm_vars.svm_type == CLASSIFICATION ) {

    if( (GLOBAL_svm_vars.afniModel)->combinations > 1 ) {
      fprintf(stderr, "WARNING: Model contains: %d classifiers!\n",
          (GLOBAL_svm_vars.afniModel)->combinations);
      fprintf(stderr, "WARNING: Sorry, multi-class is currently not supported\n");
      fprintf(stderr, "WARNING: Using classifier: %s\n",
          (GLOBAL_svm_vars.afniModel)->combName[0] );
    }
  }
  if( (GLOBAL_svm_vars.svm_type == CLASSIFICATION) || 
      (GLOBAL_svm_vars.svm_type == REGRESSION) )   {
  
    /* ----- FILL SVM-LIGHT STRUCTURES ----- */
    /* --- allocate --- */
    currTestDoc = allocateDOCs(1, nvox_masked);
    svmlModel = (MODEL *)malloc(sizeof(MODEL));
    if( allocateModel( svmlModel, GLOBAL_svm_vars.afniModel, errorString) ){
      freeDOCs(currTestDoc, 1);
      return 1;
    }
  
    /* --- fill SVM-LIGHT DOC structure --- */
    afni_dset_to_svm_doc( currTestDoc, currTestArray,GLOBAL_svm_vars.dsetMaskArray,
        1, GLOBAL_svm_vars.nvox_model, nvox_masked );
  
    /* --- fill SVM-LIGHT MODEL structure --- */
    if( get_svm_model(svmlModel, GLOBAL_svm_vars.dsetModelArray,
        GLOBAL_svm_vars.dsetMaskArray, GLOBAL_svm_vars.afniModel,
        GLOBAL_svm_vars.nvox_model, (GLOBAL_svm_vars.options)->outModelNoMask,
        errorString) ) {

      freeDOCs(currTestDoc, 1);
      freeModel(svmlModel, GLOBAL_svm_vars.afniModel, TEST);
      return 1;
    }
  
    updateModel( svmlModel, GLOBAL_svm_vars.afniModel, 0 );
  
    /* ---- CLASSIFY ----- */
    /* --- linear kernel --- */
    if( kernel_type == LINEAR ) {
      *dist = classify_example_linear( svmlModel, &currTestDoc[0] );
    }
    /* --- non-linear kernel --- */
    else {
      *dist = classify_example( svmlModel, &currTestDoc[0] );
    }

    /* --- convert output predictions --- */
    if( (!GLOBAL_svm_vars.options->noPredScale) && 
         (GLOBAL_svm_vars.svm_type == CLASSIFICATION) ) { 
      *dist = 0.5*( *dist + 1 );
    }
  
    /* ----- FREE MEMORY ----- */
    freeDOCs(currTestDoc, 1);
    freeModel(svmlModel, GLOBAL_svm_vars.afniModel, TEST);
  }
  else {
    snprintf(errorString, LONG_STRING, "What happened?! Real-time testing type unknown!");
  }

  return 0;
}


static int drive_3dsvm_plugin ( char *cmdl )
{

  KERNEL_PARM   kernel_parm;                      /* svm-light kernel parameters */
  LEARN_PARM    learn_parm;                       /* svm-light learn parameters */
  long          kernel_cache_size;                /* svm-light kernel parameter */
  
  
  ASLoptions    options;
  enum modes    mode                   = NOTHING;
  int           svm_type               = CLASSIFICATION;
  
  int           myargc                 = 0;   
  char *        myargv[LONG_STRING];
  int           cmdlArgc               = 0;
  char **       cmdlArgv               = NULL;
  char *        errorString            = NULL;
  char *        err                    = NULL;

  int i = 0;


  fprintf(stderr, "++  3dsvm: Driving plugin...\n") ; 

  /* -- allocate errorString --- */
  if( (errorString = (char *) malloc(LONG_STRING*sizeof(char))) == NULL ) {
    fprintf(stderr, "**  3dsvm: ERROR: drive_3dsvm_plugin: Allocating errorString "
        "failed!\n"); 
    
    return 1;
  }

  if( (err = (char *) malloc(LONG_STRING*sizeof(char))) == NULL ) {
    fprintf(stderr, "**  3dsvm: ERROR: drive_3dsvm_plugin: Allocating err "
        "failed!\n"); 
    
    return 1;
  }

  /* -- get environment or command-line options */
  /* TODO: It would be great to use the 3dsvm environment variables and 
     update them with the command-line options given as an argument (cmdl) */
  
  if( strlen(cmdl) == 0 ) {
    fprintf(stderr, "++  3dsvm: No command-line given! Reading environment "
      "variables...\n");

    /* -- read environment variables --*/
    argvAppend( myargv, &myargc, PROGRAM_NAME,"" );
    getEnvArgv( myargv, &myargc, "3DSVM_ALL_OPTIONS" );
  }
  else {
    
    /* -- read command line -- */
    fprintf(stderr, "++  3dsvm: Parsing command-line. Ignoring 3dsvm environment variables!\n");
    getAllocateCmdlArgv( cmdl, PROGRAM_NAME, &cmdlArgc, &cmdlArgv );
    
    for( i=0; i<cmdlArgc; i++ )  argvAppend( myargv, &myargc, cmdlArgv[i], "" );
    freeArgv( cmdlArgv, cmdlArgc );
  }
  if( DBG_flag ) printArgv( myargv,  &myargc );

  /* -- parse argv, argc --*/
  if( myargc > 1 ) {
    if( input_parse(myargc, myargv, &verbosity, &kernel_cache_size,
          &learn_parm, &kernel_parm, &options, &mode, &svm_type,
          errorString) ) {
    
      fprintf(stderr, "**  3dsvm: ERROR: %s\n", errorString);
      snprintf(err, LONG_STRING, "3dsvm plugin:\n ERROR: %s\n", errorString);
      PLUTO_popup_transient( plint , err);
      free( errorString );
      free( err );
      freeArgv( myargv, myargc );
      return 1;
    }
  }
  
  /* -- initialize 3dsvm for real-time action -- */
  if( init_3dsvm_rt(myargv, myargc, &options, mode, errorString) ) {
    fprintf(stderr, "**  3dsvm: ERROR: %s\n", errorString); 
    
    /* reset global afni callback function */
    GLOBAL_library.realtime_callback = NULL; 

    snprintf(err, LONG_STRING, "3dsvm plugin:\n ERROR: %s\n", errorString);
    PLUTO_popup_transient( plint , err);
    free( errorString );
    free( err );
    freeArgv( myargv, myargc );

    return 1;
  }

  /* -- free -- */
  free( errorString );
  free( err );
  freeArgv( myargv, myargc );

  return 0;
}
