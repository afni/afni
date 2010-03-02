/* PLUG_AFNI_SVM_LIGHT */

#define _3DSVM_GUI

#define FAKE_COMMAND "3dsvm"
#define NKERNEL  4  /* Number of kernels */
#define NSVMTYPE 2  /* Number of learn (svm) types */

#include "3dsvm_common.h"
#include "svm_common.h"

#ifndef ALLOW_PLUGINS
	#error "Plugins not properly set up -- see machdep.h"
#endif


static char * ASL_main( PLUGIN_interface * );       /* the entry point prototype */

void argvAppend(char **myargv,int *myargc,char *option, char *value)
{
  /** append option **/
  if(   (myargv[*myargc] = (char *)malloc( (strlen(option)+1) * sizeof(char) ))   ) {
    strcpy(myargv[*myargc],option);
    (*myargc)++;
  }
  else {
    printf("argvAppend error: could not allocate string 1\n");
    exit(0);
  }

  /** append value **/
  if( value[0] ) { 
    if(   (myargv[*myargc] = (char *)malloc( (strlen(value)+1) * sizeof(char) ))   ) {
      strcpy(myargv[*myargc],value);
      (*myargc)++;
    }
    else {
      printf("argvAppend error: could not allocate string 2\n");
      exit(0);
    }
  }
}

void printArgv(char **myargv,int *myargc)
{
  int i;

  for(i=0; i < *myargc; ++i)  printf("%s ",myargv[i]);

  printf("\n");
}


/* ---- plugin interface ----*/
DEFINE_PLUGIN_PROTOTYPE

PLUGIN_interface* PLUGIN_init( int ncall )
{
    /*---- declarations ----*/
    PLUGIN_interface* plint;
	char* kernel_strlist[NKERNEL] = {"linear", "polynomial", "rbf", "sigmoid"};
    char* svmType_strlist[NSVMTYPE] = {"classification", "regression"};
    char* help_contribution_string;
	int str_length = strlen(plugin_helpstring) + strlen(contribution_string) + 2;
    
    /* --- heelp and contribution string --- */
    if ( (help_contribution_string = (char *) malloc(str_length*sizeof(char))) == NULL ) {
      ERROR_exit("Can not allocate memory");
    }
    snprintf(help_contribution_string, str_length, "%s \n %s", plugin_helpstring, 
        contribution_string);

    /*--- create new interface ---*/
	if (ncall > 0) return NULL;	/* only one interface */
    plint = PLUTO_new_interface("3dsvm", "3dsvm - An AFNI SVM Light Plugin",
        help_contribution_string, PLUGIN_CALL_VIA_MENU, ASL_main);
    PLUTO_add_hint( plint , "Perform SVM Light learning/classification with"
        "AFNI datasets.");
	/* top row options: */
    PLUTO_set_sequence( plint , "A:afnicontrol:dset" );
	PLUTO_set_runlabels( plint , "Run+Keep" , "Run+Close" );
	
    
    /*--- Dummy ---*/
    PLUTO_add_option( plint,"Offline","Offline", TRUE);
    
    /*---- Training ----*/
    PLUTO_add_option( plint,"Training","Training", FALSE );
    PLUTO_add_string( plint, "Type", NSVMTYPE, svmType_strlist, FALSE );

    /*--- Train Data ---*/
    PLUTO_add_option( plint,"Train Data","Train Data",FALSE );
    /* dataset chooser for fMRI data: */
    PLUTO_add_dataset( plint,"Dataset", ANAT_ALL_MASK,FUNC_ALL_MASK,
        DIMEN_ALL_MASK | BRICK_ALLTYPE_MASK );
    /* training classification labels: */
    PLUTO_add_timeseries( plint,"Labels" ); 	
    /* training censors: */
    PLUTO_add_timeseries( plint,"Censors" );/* training censor labels */

    /*--- Train Params ---*/
    PLUTO_add_option( plint,"Train Params","Train Params",FALSE );             
	/* mask dataset chooser: */
    PLUTO_add_dataset( plint,"Mask", ANAT_ALL_MASK,FUNC_ALL_MASK,
        DIMEN_ALL_MASK | BRICK_BYTE_MASK );
    /* svm light's "c" factor: */
    PLUTO_add_number( plint, "C", 0, 1000, 0, 100, 1 ); 
    /* svm light's "epsilon" parameter: */
    PLUTO_add_number( plint, "Epsilon", 0, 1000000, 1, 1, TRUE);
    
    /*--- Kernel ---*/
     PLUTO_add_option( plint,"Kernel","Kernel",FALSE );
    /* kernel chooser: */
	PLUTO_add_string( plint, "Kernel", NKERNEL, kernel_strlist, FALSE );
    /* kernel parms: */
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
	PLUTO_add_string( plint, "Alpha Prefix (.1D)", 0, NULL, 19 );

    /*---- Testing ----*/
	PLUTO_add_option( plint,"Testing","Testing",FALSE );

    /*--- Test Data ---*/ 
	PLUTO_add_option( plint,"Test Data","Test Data",FALSE );
	/* dataset chooser for fMRI data: */
    PLUTO_add_dataset( plint,"Dataset", ANAT_ALL_MASK,FUNC_ALL_MASK,
        DIMEN_ALL_MASK | BRICK_ALLTYPE_MASK );
    /* model dataset chooser: */
	PLUTO_add_dataset( plint, "Model", ANAT_ALL_MASK,FUNC_ALL_MASK,
        DIMEN_ALL_MASK | BRICK_ALLTYPE_MASK );
    
    /*--- Predictions ---*/
	PLUTO_add_option( plint,"Predictions","Predictions",FALSE );
	/* name for prediction output:*/
    PLUTO_add_string( plint, "Prefix (.1D)", 0, NULL, 19 );
    
    /*--- True Labels ---*/
    PLUTO_add_option( plint,"'True' Labels","'True' Labels",FALSE );
	/* name for ture labels: */
    PLUTO_add_timeseries( plint, "File" );

    /*--- Command Line ---*/
    PLUTO_add_option( plint,"Command Line","Command Line",FALSE );
    PLUTO_add_string( plint, "", 0, NULL, 19 );

			   
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
  
  THD_3dim_dataset* dsetTrain     = NULL;  /* pointer for training dataset */
  THD_3dim_dataset* dsetMask      = NULL;  /* pointer for mask dataset */
  MaskType*         dsetMaskArray = NULL;  /* array to hold mask dataset values */
  THD_3dim_dataset* dsetTest      = NULL;  /* pointer for testing dataset */
  THD_3dim_dataset* dsetModel     = NULL;  /* pointer for training model dataset */
  THD_3dim_dataset* dsetModelTest = NULL;  /* pointer for input testing model dataset */

  AFNI_MODEL afniModel;           /* needed to determine learn (svm) type
                                     before testing */
  MRI_IMAGE* tsim_train  = NULL;  /* pointer for image struct returned from 
                                     PLUTO_get_timeseries */
  MRI_IMAGE* tsim_censor = NULL;  /* pointer for image struct returned from 
                                     PLUTO_get_timeseries */
  MRI_IMAGE* tsim_test   = NULL;  /* pointer to target classes for SVM 
                                     classification */
  
  MCW_idcode* idcode;             /* idcode for option line dataset chooser */


  int svmType_index;
  char* svmType_string;           /* current learn (svm) type selection */
  char* svmType_strlist[NSVMTYPE] =       /* different learn (svm) types */ 
        { "classification", 
          "regression"};

  char* kernel_string;            /* current kernel type selection */
  int kernel_index;
  char* kernel_strlist[NKERNEL] =        /* different kernel types */ 
        { "linear", 
          "polynomial", 
          "rbf", 
          "sigmoid" };
 
  char* option_tag;
  char* myargv[LONG_STRING];		
  int myargc = 0;
  char mystring[LONG_STRING];
  char* mystringPtr;
  char* myfilename;
  char trnFlag = 0, 
       tstFlag = 0, 
       optionOKflag;
  char errorString[LONG_STRING];
  int svm_type = CLASSIFICATION;               
  char* err;
  int DBG_flag=0;
  
  if( (err = (char*) malloc(LONG_STRING*sizeof(char))) == NULL ) { 
    return("Could not allocate memory");
  }

  snprintf(errorString, LONG_STRING, "nothing");

  argvAppend(myargv,&myargc,FAKE_COMMAND,"");  
  if (DBG_flag) printArgv(myargv,&myargc);

  /*---- get inputs from plugin interface ----*/
  while(1) {
    option_tag = PLUTO_get_optiontag( plint ); /* step to next option line */
    
    if( option_tag == NULL ) {
      if (DBG_flag) printArgv(myargv,&myargc);
     
      if (myargc == 1 ) {
       return"*******************************************\n"
            "Must specify training or testing dataset!\n"
            "*******************************************";
      }
      else if ( input_parse(myargc, myargv, &verbosity, &kernel_cache_size, 
            &learn_parm, &kernel_parm, &options, &mode, &svm_type, 
            &errorString) ) {
        snprintf(err, LONG_STRING, 
            "***********************************************\n"
            "%s\n"
            "**********************************************", errorString);
        return (err);
      }
      break;
    }
      
    /*---- training -----*/  
    if( strcmp(option_tag,"Training") == 0 ) {
      trnFlag = 1; 
     
      /* get learning (svm) type */
      svmType_string = PLUTO_get_string( plint );
      svmType_index = PLUTO_string_index( svmType_string, NSVMTYPE, svmType_strlist );
      if (svmType_index == -1) svmType_index = 0;
      sprintf(mystring, svmType_strlist[svmType_index]);
      argvAppend(myargv,&myargc,"-type", mystring);  
      if (DBG_flag) printArgv(myargv,&myargc);
    }

    if( trnFlag ) {
      /*--- Train Data ---*/
      if( strcmp(option_tag,"Train Data") == 0 ) {
        
        /* get training data */
        idcode = PLUTO_get_idcode( plint ); 
        dsetTrain = PLUTO_find_dset( idcode );
        if( dsetTrain == NULL )
          return 
            "**********************************************\n"
            "Must choose a dataset for training\n"
            "**********************************************";
        argvAppend(myargv,&myargc,"-trainvol",
            DSET_FILECODE(dsetTrain));  /* is there a way to get the filename 
                                           without the find_dset */
        if (DBG_flag) printArgv(myargv,&myargc);
	 
        /* get training labels */ 
        tsim_train = PLUTO_get_timeseries( plint );
        if( tsim_train == NULL )
          return 
            "**************************************************\n"
            "Must choose labels for training.\n"
            "**************************************************";
        argvAppend(myargv,&myargc,"-trainlabels",
            THD_trailname(tsim_train->name,0));  
        if (DBG_flag) printArgv(myargv,&myargc);
       
        /* get training censors */ 
        if ( tsim_censor = PLUTO_get_timeseries( plint ) ) {  
          argvAppend(myargv,&myargc,"-censor",
              THD_trailname(tsim_censor->name,0));  
          if (DBG_flag) printArgv(myargv,&myargc);
        }
      }
      
       /*--- Train Params ---*/
      if( strcmp(option_tag,"Train Params") == 0 ) {
        
        /* get mask dataset */
        idcode = PLUTO_get_idcode( plint );
        dsetMask = PLUTO_find_dset( idcode );
        if( dsetMask )
        argvAppend(myargv,&myargc,"-mask",DSET_FILECODE(dsetMask));  
        if (DBG_flag) printArgv(myargv,&myargc);

        /* get "C" parameter */
        sprintf(mystring,"%lf", (double)PLUTO_get_number( plint ));  
        argvAppend(myargv,&myargc,"-c",mystring);  
        if (DBG_flag) printArgv(myargv,&myargc);
  
        /* get "Epsilon" parameter */
        sprintf(mystring,"%lf", (double)PLUTO_get_number( plint ));  
        argvAppend(myargv,&myargc,"-e",mystring);  
        if (DBG_flag) printArgv(myargv,&myargc);
      }
      
      /* --- Kernel --- */
      if( strcmp(option_tag,"Kernel") == 0 ) {
        /* get kernel type */
        kernel_string = PLUTO_get_string( plint );
        kernel_index = PLUTO_string_index( kernel_string, NKERNEL, kernel_strlist );
        sprintf(mystring, kernel_strlist[kernel_index]);
        argvAppend(myargv,&myargc,"-kernel", mystring);  
        if (DBG_flag) printArgv(myargv,&myargc);
     
        /* get kernel parameter "d" */
        sprintf(mystring,"%d", (int)PLUTO_get_number( plint ));  
        argvAppend(myargv,&myargc,"-d", mystring); 
        
        /* get kernel parameter "g" */
        sprintf(mystring,"%lf", (double)PLUTO_get_number( plint ));  
        argvAppend(myargv,&myargc,"-g", mystring);
        
        /* get kernel parameter "s" */
        /* sprintf(mystring,"%lf", (double)PLUTO_get_number( plint ));
        argvAppend(myargv,&myargc,"-s", mystring); */
        
        /* get kernel parameter "r" */
        /* sprintf(mystring,"%lf", (double)PLUTO_get_number( plint ));  
        argvAppend(myargv,&myargc,"-r", mystring); 
        if (DBG_flag) printArgv(myargv,&myargc); */
      }

      /* --- Model Output ---*/
      if( strcmp(option_tag,"Model Output") == 0 ) {
        
        /* get output file for model */
        mystringPtr = PLUTO_get_string( plint );  
        argvAppend(myargv,&myargc,"-model",mystringPtr);  
        if (DBG_flag) printArgv(myargv,&myargc);
        
        if( !PLUTO_prefix_ok(mystringPtr) ) {
	  return 
        "*******************************************\n"
	    "'Model Output' selected:\n"
        " Bad Training Model file name given. -\n"
	    " file already exists or illegal name used.\n"
	    "*******************************************\n";
        }
      }

      /* --- Model Inspection ---*/
      optionOKflag = 0;
      if( strcmp(option_tag,"Model Inspection") == 0 ) {
        
        /* get output file for model */
        mystringPtr = PLUTO_get_string( plint );  
        if( PLUTO_prefix_ok(mystringPtr) ) {
          optionOKflag = 1;
          argvAppend(myargv,&myargc,"-bucket",mystringPtr);  
          if (DBG_flag) printArgv(myargv,&myargc);
        }
        
        /* get output file for alphas */
        mystringPtr = PLUTO_get_string( plint );  
        if( PLUTO_prefix_ok(mystringPtr) ) {
          optionOKflag = 1;
          argvAppend(myargv,&myargc,"-alpha",mystringPtr);  
          if (DBG_flag) printArgv(myargv,&myargc);
        }
        if( !optionOKflag ) {
	      return 
            "**********************************************\n"
            " Model Inspection' selected:                    \n"
            " File already exists, illegal name used, or no  \n"
            " File selected for fim or alpha                 \n"
            "**********************************************\n";
        }
      }
    }
    
    /* ---- testing ----- */ 
    if( strcmp(option_tag,"Testing") == 0 ) tstFlag = 1;
      
    if( tstFlag ) {

      /* --- Test Data --- */
      if( strcmp(option_tag,"Test Data") == 0 ) {

        /* get test dataset */     
        /* is there a way to get the filename without the find_dset */
        idcode = PLUTO_get_idcode( plint );           
        dsetTest =  PLUTO_find_dset( idcode );
        if( dsetTest == NULL ) {
	      return 
            "****************************\n"
            "bad testing dataset\n"
            "****************************\n";
        }
        argvAppend(myargv,&myargc,"-testvol",DSET_FILECODE(dsetTest));  
        if (DBG_flag) printArgv(myargv,&myargc);

        
        /* get model dataset */
        if ( !trnFlag ) {
          /* is there a way to get the filename without the find_dset */
          idcode = PLUTO_get_idcode( plint );
          dsetModelTest = PLUTO_find_dset( idcode );
        
          if( dsetModelTest == NULL ) {
            return 
              "**********************************\n"
              "bad model dataset for testing \n"
              "**********************************\n";
          }
          argvAppend(myargv,&myargc,"-model",DSET_FILECODE(dsetModelTest));
          if (DBG_flag) printArgv(myargv,&myargc);
        }
      }

      /* --- Predicitons --- */ 
      if( strcmp(option_tag,"Predictions") == 0 ) {
          
          /* get prediction file */
          mystringPtr = PLUTO_get_string( plint );  
	      argvAppend(myargv,&myargc,"-predictions",mystringPtr);
          if (DBG_flag) printArgv(myargv,&myargc);
          if( !PLUTO_prefix_ok(mystringPtr) ) {  
	    return 
          "************************************************\n"
	      "Predictions file already exists, illegal\n"
	      "name used, or no file name entered \n"
	      "************************************************\n";}
      }

      /* --- True Labels --- */
      if( strcmp(option_tag,"'True' Labels") == 0 ) {
        
        /* get test label file */
        tsim_test = PLUTO_get_timeseries( plint );   /* training labels */
        if( tsim_test == NULL ) {
          return 
            "********************************************************\n"
            "'True' Labels was selected, but valid file not chosen.\n"
            "********************************************************";
        }
	    argvAppend(myargv,&myargc,"-testlabels",THD_trailname(tsim_test->name,0));
        if (DBG_flag) printArgv(myargv,&myargc);
      }
    }
  }

  /*---- print svm light copyright ----*/
  copyright_notice();
  
  /*----- TRAIN FUNCTIONS ---------------*/
  if( mode == TRAIN || mode == TRAIN_AND_TEST ) {
    if ( svm_type == CLASSIFICATION ) {
      /* SL & JL Feb. 2009: Passing kernel_cache_size to support non-linear
       * kernels. */
      train_routine(&model, &learn_parm, &kernel_parm, &kernel_cache_size, 
        &options, dsetTrain, dsetMask, dsetMaskArray, myargc, myargv);
    }
    
    else if ( svm_type == REGRESSION ) { 
      train_regression(&model, &learn_parm, &kernel_parm, &kernel_cache_size, 
        &options, dsetTrain, dsetMask, dsetMaskArray, myargc, myargv);
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
          dsetModel, myargc, myargv);
    }
    
    else if( svm_type == REGRESSION ) {
      test_regression(&options, &model, &afniModel, dsetTest, dsetMask, 
          dsetModel, myargc, myargv);
    }

    else ERROR_exit("type not supported!");
  }
  
  printf("\n\n  ");
  printArgv(myargv,&myargc);
  printf("\n");
  
  printf("  ---3dsvm plugin run done---\n");
  
  RETURN(0);
}

