/* PLUG_AFNI_SVM_LIGHT */

#define _3DSVM_GUI

#define FAKE_COMMAND "3dsvm"

#include "3dsvm_common.h"

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

DEFINE_PLUGIN_PROTOTYPE

PLUGIN_interface* PLUGIN_init( int ncall )
{
	PLUGIN_interface* plint;
	char* strlist[1] = {"Linear"};
	if (ncall > 0) return NULL;	//only one interface

	//create new interface
	plint = PLUTO_new_interface("3dsvm",
						      "3dsvm - An AFNI SVM Light Plugin",
						      plugin_helpstring, PLUGIN_CALL_VIA_MENU, ASL_main);
	PLUTO_add_hint( plint , "Perform SVM Light learning/classification with AFNI datasets.");
	PLUTO_set_sequence( plint , "A:afnicontrol:dset" );
	PLUTO_set_runlabels( plint , "Run+Keep" , "Run+Close" );

	PLUTO_add_option( plint,"Training","Training",FALSE );

	PLUTO_add_option( plint,"Train Data","Train Data",FALSE );
	PLUTO_add_dataset( plint,"Dataset",                         /* dataset chooser for fMRI data */
			   ANAT_ALL_MASK,FUNC_ALL_MASK,
			   DIMEN_ALL_MASK | BRICK_ALLTYPE_MASK );
	PLUTO_add_timeseries( plint,"Labels" );               /* training classification labels */
	PLUTO_add_timeseries( plint,"Censors" );               /* training censor labels */
	
	PLUTO_add_option( plint,"Train Params","Train Params",FALSE );             
	PLUTO_add_dataset( plint,"Mask",           /* MASK DATASET CHOOSER */
			   ANAT_ALL_MASK,FUNC_ALL_MASK,
			   DIMEN_ALL_MASK | BRICK_BYTE_MASK );
	PLUTO_add_string( plint, "Kernel", 1, strlist, 0 );         /* kernel chooser */
	PLUTO_add_number( plint, "C", 0, 1000, 0, 100, 1 );           /* svm light's "c" factor */

	PLUTO_add_option( plint, "Model Output", "Model Output", FALSE );   /* training outputs */
	PLUTO_add_string( plint, "Prefix", 0, NULL, 19 );               /* user enters file to write model params to */

	PLUTO_add_option( plint, "Model Inspection", "Model Inspection", FALSE );   /* training outputs */
	PLUTO_add_string( plint, "FIM Prefix", 0, NULL, 19 );
	PLUTO_add_string( plint, "Alpha Prefix (.1D)", 0, NULL, 19 );


	PLUTO_add_option( plint,"Testing","Testing",FALSE );

	PLUTO_add_option( plint,"Test Data","Test Data",FALSE );
	PLUTO_add_dataset( plint,"Dataset",
			   ANAT_ALL_MASK,FUNC_ALL_MASK,
			   DIMEN_ALL_MASK | BRICK_ALLTYPE_MASK );
	PLUTO_add_dataset( plint, "Model",
			   ANAT_ALL_MASK,FUNC_ALL_MASK,
                           DIMEN_ALL_MASK | BRICK_ALLTYPE_MASK );

	PLUTO_add_option( plint,"Predictions","Predictions",FALSE );
	PLUTO_add_string( plint, "Prefix (.1D)", 0, NULL, 19 );

	PLUTO_add_option( plint,"'True' Labels","'True' Labels",FALSE );
	PLUTO_add_timeseries( plint, "File" );
			   
	return plint;
}


static char* ASL_main( PLUGIN_interface* plint )
{
  /*---------- DECLARATIONS ----------*/
  enum modes mode = NOTHING;
  long verbosity = 0;

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

  /*---------- DECLARATIONS ----------*/
  char* option_tag;
  MCW_idcode* idcode;            /* idcode for option line dataset chooser */
  MRI_IMAGE* tsim_train = NULL;  /* pointer for image struct returned from PLUTO_get_timeseries */
  MRI_IMAGE* tsim_censor = NULL;  /* pointer for image struct returned from PLUTO_get_timeseries */
  MRI_IMAGE* tsim_test = NULL;   /* pointer to target classes for SVM classification */
  char* kernel_string;
  int kernel_index;
  char* strlist[1] = {"Linear"};
  THD_3dim_dataset* dsetModelTest = NULL;     /* pointer for input testing model dataset */

  char *myargv[LONG_STRING];		
  int myargc = 0;
  char mystring[LONG_STRING];
  char *mystringPtr;
  char *myfilename;
  char trnFlag=0, tstFlag=0, optionOKflag;
  char errorString[LONG_STRING];
  char *err;

  /* PRINT OUT SVM LIGHT COPYRIGHT */
  copyright_notice();

  strcpy(errorString,"OK");
  argvAppend(myargv,&myargc,FAKE_COMMAND,"");  /* is there a way to get the filename without the find_dset */
  //printArgv(myargv,&myargc);

  /*----- GET INPUTS FROM PLUGIN INTERFACE ------------------*/
  while(1) {

    option_tag = PLUTO_get_optiontag( plint ); /* step to next option line */
    if( option_tag == NULL ) 
    {
      printArgv(myargv,&myargc);
      input_parse(myargc,myargv,&verbosity,&kernel_cache_size,&learn_parm,&kernel_parm,&options,&mode,&errorString[0]);
      if( !strcmp(errorString,"OK") == 0 ) {
	err = errorString;
        return(err);
	strcpy(errorString,"OK");
      }
      break;
    }

    printf( "Option tag: %s\n",option_tag );

    if( strcmp(option_tag,"Training") == 0 ) trnFlag = 1; 

    if( trnFlag ) {
      if( strcmp(option_tag,"Train Data") == 0 ) {
        idcode = PLUTO_get_idcode( plint );    /* training data */
        dsetTrain = PLUTO_find_dset( idcode );
        if( dsetTrain == NULL )
	  return "*******************************************\n"
	    "Must choose a dataset for training\n"
	    "**********************************************"  ;
        argvAppend(myargv,&myargc,"-trainvol",DSET_FILECODE(dsetTrain));  /* is there a way to get the filename without the find_dset */
        //printArgv(myargv,&myargc);
	  
        tsim_train = PLUTO_get_timeseries( plint );   /* training labels */
        if( tsim_train == NULL )
	  return "***********************************************\n"
	    "Must choose a timeseries for training.\n"
	    "**************************************************";
        argvAppend(myargv,&myargc,"-trainlabels",THD_trailname(tsim_train->name,0));  /* filename without the get_timeseries? */
        //printArgv(myargv,&myargc);
        if ( tsim_censor = PLUTO_get_timeseries( plint ) ) {  /* training labels */
          argvAppend(myargv,&myargc,"-censor",THD_trailname(tsim_censor->name,0));  
          //printArgv(myargv,&myargc);
        }
      }

      if( strcmp(option_tag,"Train Params") == 0 ) {
        idcode = PLUTO_get_idcode( plint );       /* id for mask dataset */
        dsetMask = PLUTO_find_dset( idcode );
        if( dsetMask )
        argvAppend(myargv,&myargc,"-mask",DSET_FILECODE(dsetMask));  
        //printArgv(myargv,&myargc);

        kernel_string = PLUTO_get_string( plint );
        kernel_index = PLUTO_string_index( kernel_string, 1, strlist );
        if( kernel_index == -1 )
	  return "*******************************\n"
	    "bad kernel input\n"
	    "*******************************";
        sprintf(mystring,"%d", kernel_index);
        argvAppend(myargv,&myargc,"-t",mystring);  
        //printArgv(myargv,&myargc);
        sprintf(mystring,"%lf", (double)PLUTO_get_number( plint ));  /* get learning factor from number chooser */
        argvAppend(myargv,&myargc,"-c",mystring);  
        //printArgv(myargv,&myargc);
      }

      if( strcmp(option_tag,"Model Output") == 0 ) {
        mystringPtr = PLUTO_get_string( plint );  /* output file for model */
        argvAppend(myargv,&myargc,"-model",mystringPtr);  /* is there a way to get the filename without the find_dset */
        //printArgv(myargv,&myargc);
        if( !PLUTO_prefix_ok(mystringPtr) ) {
	  return "*******************************************\n"
	    "'Model Output' selected:                       \n"
            " Bad Training Model file name given. -\n"
	    " file already exists or illegal name used.\n"
	    "*******************************************\n";
        }
      }

      optionOKflag = 0;
      if( strcmp(option_tag,"Model Inspection") == 0 ) {
        mystringPtr = PLUTO_get_string( plint );  
        if( PLUTO_prefix_ok(mystringPtr) ) {
          optionOKflag = 1;
          argvAppend(myargv,&myargc,"-bucket",mystringPtr);  
          //printArgv(myargv,&myargc);
        }

        mystringPtr = PLUTO_get_string( plint );  
        if( PLUTO_prefix_ok(mystringPtr) ) {
          optionOKflag = 1;
          argvAppend(myargv,&myargc,"-alpha",mystringPtr);  
          //printArgv(myargv,&myargc);
        }
        if( !optionOKflag ) {
	  return "*******************************************\n"
	    "'Model Inspection' selected:                    \n"
	    " File already exists, illegal name used, or no  \n"
	    " File selected for fim or alpha                 \n"
	    "*******************************************\n";
        }
      }
    }

    if( strcmp(option_tag,"Testing") == 0 ) tstFlag = 1;
          
    if( tstFlag ) {
      if( strcmp(option_tag,"Test Data") == 0 ) {     /* test data option line */
        idcode = PLUTO_get_idcode( plint );           /* test dataset */
        dsetTest =  PLUTO_find_dset( idcode );
        if( dsetTest == NULL ) {
	  return "****************************\n"
	    "bad testing dataset\n"
	    "****************************\n";}
        argvAppend(myargv,&myargc,"-testvol",DSET_FILECODE(dsetTest));  /* is there a way to get the filename without the find_dset */
        //printArgv(myargv,&myargc);

        idcode = PLUTO_get_idcode( plint );
        dsetModelTest = PLUTO_find_dset( idcode );
        if( dsetModelTest == NULL ) {
	  return "****************************\n"
	    "bad model dataset\n"
	    "****************************\n";}
        argvAppend(myargv,&myargc,"-model",DSET_FILECODE(dsetModelTest));  /* is there a way to get the filename without the find_dset */
        //printArgv(myargv,&myargc);

      }
 
      if( strcmp(option_tag,"Predictions") == 0 ) {
          mystringPtr = PLUTO_get_string( plint );  
	  argvAppend(myargv,&myargc,"-predictions",mystringPtr);
          //printArgv(myargv,&myargc);
          //mystringPtr = PLUTO_get_string( plint );  
          if( !PLUTO_prefix_ok(mystringPtr) ) {  
	    return "****************************\n"
	      "Predictions file already exists, illegal\n"
	      "name used, or no file name entered \n"
	      "****************************\n";}
      }

      if( strcmp(option_tag,"'True' Labels") == 0 ) {
        tsim_test = PLUTO_get_timeseries( plint );   /* training labels */
        if( tsim_test == NULL )
	  return "***********************************************\n"
            "'True' Labels was selected, but valid file not chosen.\n"
            "**************************************************";
	  argvAppend(myargv,&myargc,"-testlabels",THD_trailname(tsim_test->name,0));
          //printArgv(myargv,&myargc);

      }

    }
  }

  verbosity = 1;

  /*----- TRAIN ROUTINE ---------------*/
  if( mode == TRAIN || mode == TRAIN_AND_TEST ) {

    train_routine(&model, &learn_parm, &kernel_parm, &options, dsetTrain, dsetMask, dsetModel, dsetMaskArray, myargc, myargv);

/*
    if( mode == TRAIN )
      free(dsetMaskArray);
      free(dsetModel);
*/

  }
  if( mode == TEST || mode == TRAIN_AND_TEST ) {
    
    test_routine(&options, &model, dsetTest, dsetMask, dsetModel, myargc, myargv);

  }

  printf("---3dsvm plugin run done---\n");
  return 0;
}

