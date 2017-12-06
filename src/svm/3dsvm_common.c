/*****************************************************************************/
/*                                                                           */
/* 3dsvm_common.c                                                            */
/*                                                                           */
/* Definitions and functions used by 3dsvm                                   */
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


#include "3dsvm_common.h"
#include "debugtrace.h"

/* JL Sep. 2010:  Global variables used for real-time training/testing.
 * If not declared here, unknown */
RT_SVM_VARS GLOBAL_svm_vars = {0};

/* from svm_classify.c - copied directly (print_help) for now since this file
 *                       also has main in it */
void print_help_classify(void)
{
  printf("\nSVM-light %s: Support Vector Machine, classification module     %s\n",
      VERSION_SVMLIGHT, VERSION_DATE_SVMLIGHT);
  copyright_notice();
  printf("   usage: svm_classify [options] example_file model_file output_file\n\n");
  printf("options: -h         -> this help\n");
  printf("         -v [0..3]  -> verbosity level (default 2)\n");
  printf("         -f [0,1]   -> 0: old output format of V1.0\n");
  printf("                    -> 1: output the value of decision function (default)\n\n");
}

/* from svm_learn_main.c - copied directly (print_help -
 * omitting the wait_any_key()) for now since this file also has main in it */
void print_help_learn()
{
  printf("\nSVM-light %s: Support Vector Machine, learning module     %sstim\n",
      VERSION_SVMLIGHT, VERSION_DATE_SVMLIGHT);
  copyright_notice();
  printf("   usage: svm_learn [options] example_file model_file\n\n");
  printf("Arguments:\n");
  printf("         example_file-> file with training data\n");
  printf("         model_file  -> file to store learned decision rule in\n");

  printf("General options:\n");
  printf("         -?          -> this help\n");
  printf("         -v [0..3]   -> level (default 1)\n");
  printf("Learning options:\n");
  printf("         -z {c,r,p}  -> select between classification (c), regression (r),\n");
  printf("                        and preference ranking (p) (default classification)\n");
  printf("         -c float    -> C: trade-off between training error\n");
  printf("                        and margin (default [avg. x*x]^-1)\n");
  printf("         -w [0..]    -> epsilon width of tube for regression\n");
  printf("                        (default 0.1)\n");
  printf("         -j float    -> Cost: cost-factor, by which training errors on\n");
  printf("                        positive examples outweight errors on negative\n");
  printf("                        examples (default 1) (see [4])\n");
  printf("         -b [0,1]    -> use biased hyperplane (i.e. x*w+b>0) instead\n");
  printf("                        of unbiased hyperplane (i.e. x*w>0) (default 1)\n");
  printf("         -i [0,1]    -> remove inconsistent training examples\n");
  printf("                        and retrain (default 0)\n");
  printf("Performance estimation options:\n");
  printf("         -x [0,1]    -> compute leave-one-out estimates (default 0)\n");
  printf("                        (see [5])\n");
  printf("         -o ]0..2]   -> value of rho for XiAlpha-estimator and for pruning\n");
  printf("                        leave-one-out computation (default 1.0) (see [2])\n");
  printf("         -k [0..100] -> search depth for extended XiAlpha-estimator \n");
  printf("                        (default 0)\n");
  printf("Transduction options (see [3]):\n");
  printf("         -p [0..1]   -> fraction of unlabeled examples to be classified\n");
  printf("                        into the positive class (default is the ratio of\n");
  printf("                        positive and negative examples in the training data)\n");
  printf("Kernel options:\n");
  printf("         -t int      -> type of kernel function:\n");
  printf("                        0: linear (default)\n");
  printf("                        1: polynomial (s a*b+c)^d\n");
  printf("                        2: radial basis function exp(-gamma ||a-b||^2)\n");
  printf("                        3: sigmoid tanh(s a*b + c)\n");
  printf("                        4: user defined kernel from kernel.h\n");
  printf("         -d int      -> parameter d in polynomial kernel\n");
  printf("         -g float    -> parameter gamma in rbf kernel\n");
  printf("         -s float    -> parameter s in sigmoid/poly kernel\n");
  printf("         -r float    -> parameter c in sigmoid/poly kernel\n");
  printf("         -u string   -> parameter of user defined kernel\n");
  printf("Optimization options (see [1]):\n");
  printf("         -q [2..]    -> maximum size of QP-subproblems (default 10)\n");
  printf("         -n [2..q]   -> number of new variables entering the working set\n");
  printf("                        in each iteration (default n = q). Set n<q to prevent\n");
  printf("                        zig-zagging.\n");
  printf("         -m [5..]    -> size of cache for kernel evaluations in MB (default 40)\n");
  printf("                        The larger the faster...\n");
  printf("         -e float    -> eps: Allow that error for termination criterion\n");
  printf("                        [y [w*x+b] - 1] >= eps (default 0.001)\n");
  printf("         -h [5..]    -> number of iterations a variable needs to be\n");
  printf("                        optimal before considered for shrinking (default 100)\n");
  printf("         -f [0,1]    -> do final optimality check for variables removed\n");
  printf("                        by shrinking. Although this test is usually \n");
  printf("                        positive, there is no guarantee that the optimum\n");
  printf("                        was found if the test is omitted. (default 1)\n");
  printf("Output options:\n");
  printf("         -l string   -> file to write predicted labels of unlabeled\n");
  printf("                        examples into after transductive learning\n");
  printf("         -a string   -> write all alphas to this file after learning\n");
  printf("                        (in the same order as in the training set)\n");
  printf("\nMore details in:\n");
  printf("[1] T. Joachims, Making Large-Scale SVM Learning Practical. Advances in\n");
  printf("    Kernel Methods - Support Vector Learning, B. Schoelkopf and C. Burges and\n");
  printf("    A. Smola (ed.), MIT Press, 1999.\n");
  printf("[2] T. Joachims, Estimating the Generalization performance of an SVM\n");
  printf("    Efficiently. International Conference on Machine Learning (ICML), 2000.\n");
  printf("[3] T. Joachims, Transductive Inference for Text Classification using Support\n");
  printf("    Vector Machines. International Conference on Machine Learning (ICML),\n");
  printf("    1999.\n");
  printf("[4] K. Morik, P. Brockhausen, and T. Joachims, Combining statistical learning\n");
  printf("    with a knowledge-based approach - A case study in intensive care  \n");
  printf("    monitoring. International Conference on Machine Learning (ICML), 1999.\n");
  printf("[5] T. Joachims, Learning to Classify Text Using Support Vector\n");
  printf("    Machines: Methods, Theory, and Algorithms. Dissertation, Kluwer,\n");
  printf("    2002.\n\n");
}

void print_version()
{
  printf("\n");
  printf("*************************************************\n");
  printf("*** 3dsvm: %s (%s), SVM-light: %s ***\n",
      VERSION_3DSVM, VERSION_DATE_3DSVM, VERSION_SVMLIGHT);
  printf("*************************************************\n");
  printf("%s", change_string);
}

int detrend_linear_cnsrs(float *data, LABELS *labels, char *errorString)
{

  /* This function performs detrending without censored time-points, so that 
   * removing censored volumes (e.g. using 3dTcat) is equivalent to using
   * 3dsvm with a censorfile and/or 9999 in the label file. 
   * The classifier output is typically written to the prediction file 
   * for all time-points, thus censored time-points are detrended based on 
   * on all data 
   *
   * TODO: Having a flag to detrend based on all data, even if
   * data points are censored, might be good
   *
   * JL Aug. 2013: Bugfix: Checking of censored data points did not include
   * censorfile causing buffer overflow */


  int t, tc, nt, ntc = 0;
  float *data_cnsrs  = NULL;

  
  ENTRY("detrend_linear_cnsrs");

  nt = labels->n;
  ntc = nt - labels->n_cnsrs;

  if( (data_cnsrs = (float *)malloc(sizeof(float)*(ntc))) == NULL ) {
    snprintf(errorString, LONG_STRING, "detrend_linear_cnsrs: "
        "Memory allocation for dist_cnsrs failed!");
    RETURN(1);
  }
    
  /* get data for uncensored time-points */
  for( t=0, tc=0; t<nt; t++) {
    if( labels->cnsrs[t] == 1 ) { /* not censored */
      data_cnsrs[tc] = data[t];
      tc++;
    }
  }

  DETREND_linear(nt,  data); /* detrend all  */
  DETREND_linear(ntc, data_cnsrs); /* detrend without censored time-points */
 
  /* replace values for uncensored data points */
  for( t=0, tc=0; t<nt; t++ ) {
    if( labels->cnsrs[t] == 1 ) {
      data[t] = data_cnsrs[tc];
      tc++;
    }
  }

  free(data_cnsrs);

  RETURN(0);
}

/* JL June 2009: This function writes the svm-light DOC structure into 
 * a svm-light readable textfile
 *
 * JL Apr. 2010: Writing 1e-6 for voxels (features) equal to 0, otherwise
 * svm-light gets the feature index wrong!
 *
 */
void write_svmLight_doc(DOC *docs, long nt, long nvox, 
    LabelType *target, char *fileName, char *svmLight_ver)
{    
  long t    = 0;
  long v    = 0;
  FILE *fp  = NULL;


  ENTRY("write_svmLight_doc");
  INFO_message("Writing svm-light textfile...");

  if ( (fp=fopen(fileName, "w")) == NULL ) {
    WARNING_message("Can not open: %s to write svm-light (doc) textfile!", 
        fileName);
    EXRETURN;
  }
  
  if ( !strcmp(svmLight_ver, "V5.00") ) {
    for ( t=0; t<nt; ++t ) {
      fprintf(fp, "%lf ", target[t]);
      for (v=0; v<nvox; ++v) {
        if ( docs[t].words[v].wnum == 0 ) {
          WARNING_message("Writing svm-light textfile: "
              "Number of words shorter than expected\n");
          continue;
        }
        if ( docs[t].words[v].weight != 0 ) {
          fprintf(fp, "%ld:%lf ", v+1, docs[t].words[v].weight );
        }
        else {
          WARNING_message("Timpepoint %4ld: voxel:%6ld is 0. Adding 1e-6 to fix "
              "a problem with svm-light", t, v);
          fprintf(fp, "%ld:%lf ", v+1, 1e-6);
        }
      }
      fprintf(fp, " # written by 3dsvm\n");
    }
  }
  else {
    WARNING_message("Can not write svm-light (doc) textfile"
        " svm-light version %s unknown", svmLight_ver);
    EXRETURN;
  }
  
  fclose(fp);
  
  EXRETURN;
}

/* JL Sep. 2010 */
/*    Mai  2011 */
void printASLoptions(ASLoptions* options)
{

  ENTRY("printASLoptions");

  INFO_message("ASLoptipns: labelFile       = %s\n", options->labelFile);
  INFO_message("ASLoptions: censorFile      = %s\n", options->censorFile);
  INFO_message("ASLoptions: trainFile       = %s\n", options->trainFile);
  INFO_message("ASLoptions: maskFile        = %s\n", options->maskFile);
  INFO_message("ASLoptions: modelFile       = %s\n", options->modelFile);
  INFO_message("ASLoptions: docFile         = %s\n", options->docFile);
  INFO_message("ASLoptions: docFileOnly     = %s\n", options->docFileOnly);
  INFO_message("ASLoptions: kernelName      = %s\n", options->kernelName);
  INFO_message("ASLoptions: svmType         = %s\n", options->svmType);
  INFO_message("ASLoptions: outModelNoMask  = %d\n", options->outModelNoMask);
  INFO_message("ASLoptions: noPredDetrend   = %d\n", options->noPredDetrend);
  INFO_message("ASLoptions: noPredCensor    = %d\n", options->noPredCensor);
  INFO_message("ASLoptions: noPredScale     = %d\n", options->noPredScale);
  INFO_message("ASLoptions: rtTrain         = %d\n", options->rtTrain);
  INFO_message("ASLoptions: rtTest          = %d\n", options->rtTest);
  INFO_message("ASLoptions: rtIP            = %s\n", options->rtIP);
  INFO_message("ASLoptions: rtPort          = %d\n", options->rtPort);
  INFO_message("ASLoptions: classout        = %d\n", options->classout);
  INFO_message("ASLoptions: testFile        = %d\n", options->testFile);
  INFO_message("ASLoptions: multiclass      = %s\n", options->multiclass);
  INFO_message("ASLoptions: predFile        = %s\n", options->predFile);
  INFO_message("ASLoptions: testLabelFile   = %s\n", options->testLabelFile);
  INFO_message("ASLoptions: modelAlphaFile  = %s\n", options->modelAlphaFile);
  INFO_message("ASLoptions: modelWeightFile = %s\n", options->modelWeightFile);

  EXRETURN;
}

/* JL Nov 2010 */
void printAfniModel( AFNI_MODEL *afniModel )
{

  int i, t = 0;


  ENTRY("printAfniModel");

  if( afniModel->version < 1.00 ) {
    ERROR_message("Can not print afniModel! Version number: '%f' outdated!",
        afniModel->version);
    EXRETURN;
    }

  INFO_message("afniModel: version                      = %f\n", afniModel->version);
  INFO_message("afniModel: svm_type                     = %s\n", afniModel->svm_type);
  INFO_message("afniModel: mask_used                    = %d\n", afniModel->mask_used);
  INFO_message("afniModel: class_count                  = %d\n", afniModel->class_count);
  INFO_message("afniModel: combinations                 = %d\n", afniModel->combinations);
  INFO_message("afniModel: timepoints                   = %d\n", afniModel->timepoints);

  for( i=0; i<afniModel->combinations; ++i ) {
    INFO_message("afniModel: combName[%04d]               = %s\n", i, afniModel->combName[i]);
    INFO_message("afniModel: kernel_custom[%04d]          = %s\n", i, afniModel->kernel_custom[i]);
    INFO_message("afniModel: kernel_type[%04d]            = %d\n", i, afniModel->kernel_type[i]);
    INFO_message("afniModel: polynomial_degree[%04d]      = %f\n", i, afniModel->polynomial_degree[i]);
    INFO_message("afniModel: rbf_gamma[%04d]              = %f\n", i, afniModel->rbf_gamma[i]);
    INFO_message("afniModel: linear_coefficient[%04d]     = %f\n", i, afniModel->linear_coefficient[i]);
    INFO_message("afniModel: total_masked_features[%04d]  = %d\n", i, afniModel->total_masked_features[i]);
    INFO_message("afniModel: total_support_vectors[%04d]  = %d\n", i, afniModel->total_support_vectors[i]);
    INFO_message("afniModel: b[%04d]                      = %f\n", i, afniModel->b[i]);
    INFO_message("afniModel: eps[%04d]                    = %f\n", i, afniModel->eps[i]);
    INFO_message("afniModel: svm_c[%04d]                  = %f\n", i, afniModel->svm_c[i]);
    INFO_message("afniModel: biased_hyperplane[%04d]      = %d\n", i, afniModel->biased_hyperplane[i]);
    INFO_message("afniModel: skip_final_opt_check[%04d]   = %d\n", i, afniModel->skip_final_opt_check[i]);
    INFO_message("afniModel: svm_maxqpsize[%04d]          = %d\n", i, afniModel->svm_maxqpsize[i]);
    INFO_message("afniModel: svm_newvarsinqp[%04d]        = %d\n", i, afniModel->svm_newvarsinqp[i]);
    INFO_message("afniModel: svm_iter_to_shrink[%04d]     = %d\n", i, afniModel->svm_iter_to_shrink[i]);
    INFO_message("afniModel: transduction_posratio[%04d]  = %f\n", i, afniModel->transduction_posratio[i]);
    INFO_message("afniModel: svm_costratio[%04d]          = %f\n", i, afniModel->svm_costratio[i]);
    INFO_message("afniModel: svm_costratio_unlab[%04d]    = %f\n", i, afniModel->svm_costratio_unlab[i]);
    INFO_message("afniModel: svm_unlabbound[%04d]         = %f\n", i, afniModel->svm_unlabbound[i]);
    INFO_message("afniModel: epsilon_a[%04d]              = %f\n", i, afniModel->epsilon_a[i]);
    INFO_message("afniModel: epsilon_crit[%04d]           = %f\n", i, afniModel->epsilon_crit[i]);
    INFO_message("afniModel: compute_loo[%04d]            = %d\n", i, afniModel->compute_loo[i]);
    INFO_message("afniModel: rho[%04d]                    = %f\n", i, afniModel->rho[i]);
    INFO_message("afniModel: xa_depth[%04d]               = %d\n", i, afniModel->xa_depth[i]);
  }

  EXRETURN;

}

/* JL Sep. 2010: This function was originally defined in plug_3dsvm.c. and
 * only used by the plugin. Now it is also used by 3dsvm directly */
void printArgv(char **myargv, int *myargc)
{
  int i = 0;

  ENTRY("printArgv");

  INFO_message("%s \\\n", myargv[0]);
  for( i=1; i<*myargc; ++i) printf("\t%s \\\n", myargv[i]);
  INFO_message("\n");

  EXRETURN;
}

/* JL Sep. 2010: This function was originally defined in plug_3dsvm.c.
 * and only used by the plugin. Now also used by 3dsvm directly to
 * read command-line options from environment (e.g. .afnirc) */
void argvAppend(char **myargv, int *myargc, char *option, char *value)
{

  ENTRY("argvAppend");

  /* --- append option --- */
  if(   (myargv[*myargc] = (char *)malloc( LONG_STRING * sizeof(char) ))   ) {
    strncpy(myargv[*myargc], option, LONG_STRING);
    (*myargc)++;
  }
  else ERROR_exit("Could not allocate option string!");

  /* --- append value --- */
  if( value[0] ) {
    if(   (myargv[*myargc] = (char *)malloc( LONG_STRING * sizeof(char) ))   ) {
      strncpy(myargv[*myargc],value, LONG_STRING);
      (*myargc)++;
    }
    else ERROR_exit("Could not allocate argument string!");
  }

  EXRETURN;
}

void freeArgv( char **myargv, int myargc )
{
  int i = 0;

  ENTRY("freeArgv");

  for( i=0; i<myargc; i++ ) {
    myargv[i]='\0';
    free(myargv[i]);
  }

  EXRETURN;
}

/* JL Sep. 2010: This function goes through argv and returns 1 
 * if option (or option && value) is present */ 
int argvCheck(char **myargv, int *myargc, char *option, char *value)
{
  int i = 0;

  ENTRY("argvCheck");

  for( i=0; i<*myargc; ++i ) {
    if( !strncmp(myargv[i], option, LONG_STRING) ) {
      if( (value[0])  && (i<*myargc-2) ) {
        if( !strncmp(myargv[i+1], value, LONG_STRING) ){
          RETURN(1);
        }
      }
      if( !value[0] ) RETURN(1);
    }
  }

  RETURN(0);
}

/* JL Mai 2011: My poor mans way to parse the command-line into argc, argv */
void getAllocateCmdlArgv( char *cmdl, char *progname, int *myargc, char ***myargv) 
{
  
  long      nargs        =  0;
  char **   args         = NULL;
  char *    cmdl_copy    = NULL;
  char *    option       = NULL;


  ENTRY("getAllocateCmdlArgv");

  
  /* -- initialize for strtok -- */
  cmdl_copy = strdup(cmdl);
  option    = strtok(cmdl_copy, " ");
  
  while( option != NULL ) {
    nargs++;
    option = strtok(NULL, " ");
  }
  nargs++; /* +1 for program name */

  /* -- allocate args -- */
  args = Allocate2c(nargs, LONG_STRING);

  /* -- go through command line and assign to args --*/
  strncpy(args[0], progname, LONG_STRING); /* copy program name */
  nargs = 1;
    
  cmdl_copy = strdup(cmdl);
  option    = strtok(cmdl_copy, " ");

  while( option != NULL ) {

    if( strlen(option) > LONG_STRING ) {
      ERROR_exit("Command line option %s\n"
          "               Exceeds maximum length: %d\n", option, LONG_STRING);
    }
      
    strncpy(args[nargs], option, LONG_STRING);
    option = strtok(NULL, " ");

    nargs++;
  }

  /* -- return pointers --*/
  *myargv=args;
  *myargc=nargs;

  
  EXRETURN;
}


/* JL Sep. 2010: This function reads the command-line options from
 * from the environment (e.g. .afnirc) 
 * (some of the options only can be used by the 3dsvm plugin
 *  in real-time mode) */
void getEnvArgv(char **myargv, int *myargc, char *name)
{

  char *ept = NULL;

  ENTRY("getEnvArgv");

  /* --- real-time flags ---*/

  /* These options only makes sense for the 3dsvm plugin in real-time mode.
   * and will cause error messages if 3dsvm is evoked with the "-getenv" option.
   */
  if( (!strncmp(name, "AFNI_3DSVM_RT_TRAIN", LONG_STRING)) ||
      (!strncmp(name, "3DSVM_ALL_OPTIONS", LONG_STRING)) ) {
    ept = my_getenv("AFNI_3DSVM_RT_TRAIN");
    if( ept != NULL )  {
      if( !strncmp(ept,"YES", LONG_STRING) ) {
        argvAppend(myargv,myargc,"-rt_train","");
      }
    }
  }

  if( (!strncmp(name, "AFNI_3DSVM_RT_TEST", LONG_STRING)) ||
      (!strncmp(name, "3DSVM_ALL_OPTIONS", LONG_STRING)) ) {
    ept = my_getenv("AFNI_3DSVM_RT_TEST");
    if( ept != NULL )  {
      if( !strncmp(ept,"YES", LONG_STRING) ) {
        argvAppend(myargv,myargc,"-rt_test","");
      }
    }
  }

  if( (!strncmp(name, "AFNI_3DSVM_RT_IP", LONG_STRING)) ||
      (!strncmp(name, "3DSVM_ALL_OPTIONS", LONG_STRING)) ) {
    ept = my_getenv("AFNI_3DSVM_RT_IP");
    if( ept != NULL ) argvAppend(myargv, myargc,"-stim_ip", ept);
  }

  if( (!strncmp(name, "AFNI_3DSVM_RT_PORT", LONG_STRING)) ||
      (!strncmp(name, "3DSVM_ALL_OPTIONS", LONG_STRING)) ) {
    ept = my_getenv("AFNI_3DSVM_RT_PORT");
    if( ept != NULL ) argvAppend(myargv, myargc,"-stim_port", ept);
  }

  /* --- testing flags ----*/
  if( (!strncmp(name, "AFNI_3DSVM_NOMASK", LONG_STRING)) ||
      (!strncmp(name, "3DSVM_ALL_OPTIONS", LONG_STRING)) ) {
    ept = my_getenv("AFNI_3DSVM_NOMASK");
    if( ept != NULL )  {
      if( !strncmp(ept,"YES", LONG_STRING) ) {
        argvAppend(myargv,myargc,"-nomodelmask","");
      }
    }
  }

  if( (!strncmp(name, "AFNI_3DSVM_NODETREND", LONG_STRING)) ||
      (!strncmp(name, "3DSVM_ALL_OPTIONS", LONG_STRING)) ) {
    ept = my_getenv("AFNI_3DSVM_NODETREND");
    if( ept != NULL )  {
      if( !strncmp(ept,"YES", LONG_STRING) ) {
        argvAppend(myargv,myargc,"-nodetrend","");
      }
    }
  }

  /* --- training options --- */
  if( (!strncmp(name, "AFNI_3DSVM_TRAIN_TYPE", LONG_STRING)) ||
      (!strncmp(name, "3DSVM_ALL_OPTIONS", LONG_STRING)) ) {
    ept = my_getenv("AFNI_3DSVM_TRAIN_TYPE");
    if( ept != NULL ) argvAppend(myargv, myargc,"-type", ept);
  }

  if( (!strncmp(name, "AFNI_3DSVM_TRAIN_DSET", LONG_STRING)) ||
      (!strncmp(name, "3DSVM_ALL_OPTIONS", LONG_STRING)) ) {
    ept = my_getenv("AFNI_3DSVM_TRAIN_DSET");
    if( ept != NULL ) argvAppend(myargv, myargc,"-trainvol", ept);
  }

  if( (!strncmp(name, "AFNI_3DSVM_TRAIN_LBLS", LONG_STRING)) ||
      (!strncmp(name, "3DSVM_ALL_OPTIONS", LONG_STRING)) ) {
    ept = my_getenv("AFNI_3DSVM_TRAIN_LBLS");
    if( ept != NULL ) argvAppend(myargv,myargc,"-trainlabels", ept);
  }

  if( (!strncmp(name, "AFNI_3DSVM_MASK_DSET", LONG_STRING)) ||
      (!strncmp(name, "3DSVM_ALL_OPTIONS", LONG_STRING)) ) {
    ept = my_getenv("AFNI_3DSVM_MASK_DSET");
    if( ept != NULL )  {
      argvAppend(myargv,myargc,"-mask", ept);
    }
  }

  if( (!strncmp(name, "AFNI_3DSVM_MODEL_DSET", LONG_STRING)) ||
      (!strncmp(name, "3DSVM_ALL_OPTIONS", LONG_STRING)) ) {
    ept = my_getenv("AFNI_3DSVM_MODEL_DSET");
    if( ept != NULL ) argvAppend(myargv,myargc,"-model", ept);
  }

  if( (!strncmp(name, "AFNI_3DSVM_BUCKET_DSET", LONG_STRING)) ||
      (!strncmp(name, "3DSVM_ALL_OPTIONS", LONG_STRING)) ) {
    ept = my_getenv("AFNI_3DSVM_BUCKET_DSET");
    if( ept != NULL ) argvAppend(myargv,myargc,"-bucket", ept);
  }

  if( (!strncmp(name, "AFNI_3DSVM_ALPHA_FILE", LONG_STRING)) ||
      (!strncmp(name, "3DSVM_ALL_OPTIONS", LONG_STRING)) ) {
    ept = my_getenv("AFNI_3DSVM_ALPHA_FILE");
    if( ept != NULL ) argvAppend(myargv,myargc,"-alpha", ept);
  }

  if( (!strncmp(name, "AFNI_3DSVM_PARM_C", LONG_STRING)) ||
      (!strncmp(name, "3DSVM_ALL_OPTIONS", LONG_STRING)) ) {
    ept = my_getenv("AFNI_3DSVM_PARM_C");
    if( ept != NULL ) argvAppend(myargv,myargc,"-c", ept);
  }

  if( (!strncmp(name, "AFNI_3DSVM_PARM_EPS", LONG_STRING)) ||
      (!strncmp(name, "3DSVM_ALL_OPTIONS", LONG_STRING)) ) {
    ept = my_getenv("AFNI_3DSVM_PARM_EPS");
    if( ept != NULL ) argvAppend(myargv,myargc,"-e", ept);
  }

  if( (!strncmp(name, "AFNI_3DSVM_KERNEL_TYPE", LONG_STRING)) ||
      (!strncmp(name, "3DSVM_ALL_OPTIONS", LONG_STRING)) ) {
    ept = my_getenv("AFNI_3DSVM_KERNEL_TYPE");
    if( ept != NULL ) argvAppend(myargv,myargc,"-kernel", ept);
  }

  if( (!strncmp(name, "AFNI_3DSVM_KERNEL_PARM_D", LONG_STRING)) ||
      (!strncmp(name, "3DSVM_ALL_OPTIONS", LONG_STRING)) ) {
    ept = my_getenv("AFNI_3DSVM_KERNEL_PARM_D");
    if( ept != NULL ) argvAppend(myargv,myargc,"-d", ept);
  }

  if( (!strncmp(name, "AFNI_3DSVM_KERNEL_PARM_G", LONG_STRING)) ||
      (!strncmp(name, "3DSVM_ALL_OPTIONS", LONG_STRING)) ) {
    ept = my_getenv("AFNI_3DSVM_KERNEL_PARM_G");
    if( ept != NULL )  argvAppend(myargv,myargc,"-g", ept);
  }

  if( (!strncmp(name, "AFNI_3DSVM_KERNEL_PARM_S", LONG_STRING)) ||
      (!strncmp(name, "3DSVM_ALL_OPTIONS", LONG_STRING)) ) {
    ept = my_getenv("AFNI_3DSVM_KERNEL_PARM_S");
    if( ept != NULL )  argvAppend(myargv,myargc,"-s", ept);
  }

  if( (!strncmp(name, "AFNI_3DSVM_KERNEL_PARM_R", LONG_STRING)) ||
      (!strncmp(name, "3DSVM_ALL_OPTIONS", LONG_STRING)) ) {
    ept = my_getenv("AFNI_3DSVM_KERNEL_PARM_R");
    if( ept != NULL )  argvAppend(myargv,myargc,"-r", ept);
  }


  /* --- testing options --- */
  if( (!strncmp(name, "AFNI_3DSVM_TEST_DSET", LONG_STRING)) ||
      (!strncmp(name, "3DSVM_ALL_OPTIONS", LONG_STRING)) ) {
    ept = my_getenv("AFNI_3DSVM_TEST_DSET");
    if( ept != NULL ) argvAppend(myargv,myargc,"-testvol", ept);
  }

  if( (!strncmp(name, "AFNI_3DSVM_TEST_LBLS", LONG_STRING)) ||
      (!strncmp(name, "3DSVM_ALL_OPTIONS", LONG_STRING)) ) {
    ept = my_getenv("AFNI_3DSVM_TEST_LBLS");
    if( ept != NULL )  argvAppend(myargv,myargc,"-testlabels", ept);
  }

  if( (!strncmp(name, "AFNI_3DSVM_PRED_FILE", LONG_STRING)) ||
      (!strncmp(name, "3DSVM_ALL_OPTIONS", LONG_STRING)) ) {
    ept = my_getenv("AFNI_3DSVM_PRED_FILE");
    if( ept != NULL ) argvAppend(myargv,myargc,"-predictions", ept);
  }

  if( (!strncmp(name, "AFNI_3DSVM_MCLASS_TYPE", LONG_STRING)) ||
      (!strncmp(name, "3DSVM_ALL_OPTIONS", LONG_STRING)) ) {
    ept = my_getenv("AFNI_3DSVM_MCLASS_TYPE");
    if( ept != NULL )  argvAppend(myargv,myargc,"-multiclass", ept);
  }

  /* --- training or testing option --- */
  if( (!strncmp(name, "AFNI_3DSVM_CENSOR_FILE", LONG_STRING)) ||
        (!strncmp(name, "3DSVM_ALL_OPTIONS", LONG_STRING)) ) {
      ept = my_getenv("AFNI_3DSVM_CENSOR_FILE");
      if( ept != NULL ) argvAppend(myargv,myargc,"-censor", ept);
    }


  EXRETURN;
}


/* JL Feb. 2009: This function calculates the squared Euclidean length of
 * a complex vector. */
double cpxtwonorm_sq(WORD *a) {

  long size_i, i;
  double Re_a, Im_a, sum;

  size_i=i=0;
  Re_a=Im_a=sum=0.0;


  ENTRY("cpxtwonorm_sq");


  while (a[i].wnum) {i++;}

  if (i%2 != 0) {
    ERROR_exit("something is wrong with the complex-valued data"
        "representation in the WORD structure.");
  }
  size_i=i/2;

  i=0;
  while (i < size_i) {
    Re_a=a[i].weight;
    Im_a=a[i+size_i].weight;
    sum+=Re_a*Re_a+Im_a*Im_a;

    i++;
  }

  RETURN(sum);
}

/* JL Sep 2010: Remove white space, tabs and \n from string */
char *trimString(char *string)
{
  char new_string[strlen(string)+1];
  int i,j = 0;

  ENTRY("trimString");

  if( string == NULL ) RETURN(NULL);

  j = 0;
  for( i=0; string[i] !='\0'; i++ ) {
    if( (string[i] != ' ') && (string[i] != '\t') && (string[i] != '\n') )
     new_string[j++] = string[i];
  }
  new_string[j]='\0';

  for( i=0; new_string[i] !='\0'; i++ ) {
    string[i] = new_string[i];
  }
  string[i]='\0';

  RETURN(string);
}

long getFileSize( char *fileName )
{
    FILE *fp;
    unsigned long int lineCount=0;
    char str[400];

    ENTRY("getFileSize");

    if( (fp = fopen(fileName, "r")) == NULL ) {
        ERROR_exit("Can not open file in getFileSize");
    }

    while( !feof(fp) ) {
        fgets(str,390,fp);
        lineCount ++;
    }
    lineCount --;

    fclose(fp);

    RETURN(lineCount);
}

int compare_ints( const int *a, const int *b ) {
  int tmp = *a - *b;
  if( tmp > 0 )
    return 1;
  else if( tmp < 0 )
    return -1;
  else
    return 0;
}


/* JL Mar. 2009 */
double **Allocate2d(long index1, long index2)
{
  long i        = 0;
  double **darr = NULL;

  ENTRY("Allocate2d");

  /* JL June 2011: Modified error handling. Return NULL if memory 
     can not be allocated. Don't exit! */

  if(   ( darr = (double **)malloc(index1*sizeof(double *)) )   ) {
    for(i = 0; i < index1; i++) {
      if(   ( darr[i] = (double *)malloc(index2*sizeof(double)) )   );
      else RETURN(NULL);
    }
  }
  else RETURN(NULL);
  
  
  RETURN(darr);
}

/* JL Mar. 2009 */
void free2d(double **x, long index1)
{
  long i;

  ENTRY("free2d");
  
  for(i = 0; i < index1; i++) {
    free(x[i]);
  }
  free(x);

  EXRETURN;
}

/* JL Mar. 2009 */
void Clear2d(double **x, long index1, long index2)
{
  long i,j;

  ENTRY("Clear2d");

  for ( i=0; i<index1; ++i ) {
    for ( j=0; j<index2; ++j ) {
      x[i][j] = (double) 0.0;
    }
  }

  EXRETURN;
}

/****************************************************************
 * Allocate2f()                                                 *
 * farr[index1][index2]				                *
 ****************************************************************/
float **Allocate2f(long index1, long index2)
{
  long      i     = 0;
  float **  farr  = NULL;

  ENTRY("Allocate2f");

  /* JL June 2011: Modified error handling. Return NULL if memory 
     can not be allocated. Don't exit! */

  if(   ( farr = (float **)malloc(index1*sizeof(float *)) )   ) {
    for(i = 0; i < index1; i++) {
      if(   ( farr[i] = (float *)malloc(index2*sizeof(float)) )   );
      else RETURN(NULL);
    }
  }
  else RETURN(NULL);


  RETURN(farr);
}

void free2f(float **x, long index1)
{
  long i;

  ENTRY("free2f");
  
  for(i = 0; i < index1; i++) {
    free(x[i]);
  }
  free(x);

  EXRETURN;
}

/* JL Mar. 2009 */
void Clear2f(float **x, long index1, long index2)
{
  long i,j;

  ENTRY("Clear2f");

  for ( i=0; i<index1; ++i ) {
    for ( j=0; j<index2; ++j ) {
      x[i][j] = (float) 0.0;
    }
  }

  EXRETURN;
}

/****************************************************************
 * Allocate2DT()  -- Datasetype                                 *
 * arr[index1][index2]                                          *
 ****************************************************************/
DatasetType **Allocate2DT(long index1, long index2)
{
  long i             = 0;
  DatasetType **arr  = NULL;

  ENTRY("Allocate2DT");

  /* JL June 2011: Modified error handling. Return NULL if memory 
     can not be allocated. Don't exit! */

  if(   ( arr = (DatasetType **)malloc(index1*sizeof(DatasetType *)) )   ) {
    for(i = 0; i < index1; i++) {
      if(   ( arr[i] = (DatasetType *)malloc(index2*sizeof(DatasetType)) )   );
      else RETURN(NULL);
    }
  }
  else RETURN(NULL);

  
  RETURN(arr);
}

void free2DT(DatasetType **x, long index1)
{
  long i;

  ENTRY("free2DT");

  for(i = 0; i < index1; i++) {
    free(x[i]);
  }
  free(x);
  
  EXRETURN;
}

/* JL Mar. 2009 */
void Clear2DT(DatasetType **x, long index1, long index2)
{
  long i,j;

  ENTRY("Clear2DT");

  for(i = 0; i < index1; i++) {
    for(j = 0; j< index2; j++) {
      x[i][j] = (DatasetType)0;
    }
  }
  
  EXRETURN;
}

/* JL Mar. 2009 */
char **Allocate2c(long index1, long index2)
{
  long i        = 0;
  char **carr   = NULL;

  ENTRY("Allocate2c");

  /* JL June 2011: Modified error handling. Return NULL if memory 
     can not be allocated. Don't exit! */

  if( (carr = (char **)malloc(sizeof(char *) * index1)) == NULL ) RETURN(NULL);
  for(i=0; i<index1; i++) {
    if( (carr[i] = (char *)malloc(sizeof(char) * index2)) == NULL ) RETURN(NULL);
  }
  
  RETURN(carr);
} 


/* JL Mar. 2009 */
void Clear2c(char **x, long index1)
{
  long i;

  ENTRY("Clear2c");

  for(i=0; i<index1; i++) {
      strcpy(x[i], "\0"); 
  }

  EXRETURN;
}

/* JL Mar. 2009 */
void free2c(char **x, long index1)
{
  long i;

  ENTRY("free2c");

  for(i=0; i<index1; i++) free(x[i]);
  free(x);

  EXRETURN;
}


DOC * allocateDOCs(long ndocsTime, long nvoxelWords)
{
  long i = 0;
  DOC * docs = NULL;

  /* JL: July 2011: Modified function to allocate the DOCs and
     the WORDs for the DOCs together. Return memory or NULL if 
     memory can not be allocated */

  ENTRY("allocateDOCs");

  /* -- allocate DOCs (timepoints) -- */
  if( (docs = (DOC*)malloc(sizeof(DOC)*ndocsTime)) == NULL ) {
    RETURN(NULL);
  }

  /* allocate WORDs (voxels) for each DOC (timepoints) */
  for( i=0; i < ndocsTime; ++i ) {
    if( (docs[i].words = (WORD*)malloc(sizeof(WORD)*(nvoxelWords+1))) == NULL ) {
      RETURN(NULL);
    }
  }

  RETURN(docs);
}

void freeDOCs(DOC *docs, long ndocsTime)
{

  long i = 0;

  ENTRY("freeDOCs");

  /* Note from svm-light:
     Warning: The model contains references to the original data 'docs'.
     If you want to free the original data, and only keep the model, you 
     have to make a deep copy of 'model'. */
  /* deep_copy_of_model=copy_model(model); */

  for( i=0; i < ndocsTime; ++i ) free(docs[i].words);

  free(docs);

  EXRETURN;
}

int allocateMultiClassArrays( float ***multiclass_dist, float **classCorrect, 
    float **classIncorrect, int **classVote, int **classList, long n_classMax, 
    long n_classComb, long nt, char *errorString )
{
  float ** tmp_mcdist           = NULL;
  float *  tmp_classCorrect     = NULL;
  float *  tmp_classIncorrect   = NULL;
  int   *  tmp_classVote        = NULL;
  int   *  tmp_classList        = NULL;

  ENTRY("allocateMultiClassArrays");

  /* JL July 2011: Added this function to simplify the flow in 
     test_classification. All arrays necessary for multiclass
     (current method DAG and vote) are allocated here. 
    
     TODO: We are bit inefficient, since either 
     DAG or vote is used, and we are allocating for both.   
  */


  if( (tmp_mcdist = Allocate2f(n_classComb, nt)) == NULL ) {
    snprintf(errorString, LONG_STRING, "allocateMultiClassArrays: " 
        "Memory allocation for tmp_mcdist failed!");
    RETURN(1);
  }

  if( (tmp_classCorrect = (float *)malloc(sizeof(float)*n_classMax)) == NULL ) {
   snprintf(errorString, LONG_STRING, "allocateMultiClassArrays: " 
       "Memory allocation for tmp_classCorrect failed!");
   
   /* free and return */
   free2f(tmp_mcdist, n_classComb);
   RETURN(1);
  }

  if( (tmp_classIncorrect = (float *)malloc(sizeof(float)*n_classMax)) == NULL ) {
    snprintf(errorString, LONG_STRING, "allocateMultiClassArrays: " 
        "Memory allocation for tmp_classIncorrect failed!");
    
    /* free and return */
    free2f(tmp_mcdist, n_classComb);
    free(tmp_classCorrect);
    RETURN(1);
  }

  if( (tmp_classVote = (int *)malloc(sizeof(int)*n_classMax)) == NULL ) {
    snprintf(errorString, LONG_STRING, "allocateMultiClassArrays: " 
        "Memory allocation for tmp_classVote failed!");
    
    /* free and return */
    free2f(tmp_mcdist, n_classComb);
    free(tmp_classCorrect);
    free(tmp_classIncorrect);
    RETURN(1);
  }

  if( (tmp_classList = (int *)malloc(sizeof(int)*n_classMax)) == NULL ) {
    snprintf(errorString, LONG_STRING, "allocateMultiClassArrays: " 
        "Memory allocation for tmp_classList failed!");
    
    /* free and return */
    free2f(tmp_mcdist, n_classComb);
    free(tmp_classCorrect);
    free(tmp_classIncorrect);
    free(tmp_classVote);
    RETURN(1);
  }


  /* -- return pointers to allocated memory -- */
  *multiclass_dist = tmp_mcdist;
  *classCorrect    = tmp_classCorrect;
  *classIncorrect  = tmp_classIncorrect;
  *classVote       = tmp_classVote;
  *classList       = tmp_classList;

  RETURN(0);
}

void freeMultiClassArrays( float **multiclass_dist, float *classCorrect, 
     float *classIncorrect, int *classVote, int *classList, long n_classComb )
{

  ENTRY("freeMultiClassArryas");

  free2f(multiclass_dist, n_classComb);
  free(classCorrect);
  free(classIncorrect);
  free(classVote);
  free(classList);

  EXRETURN;
}

/* JL Mar 2014: Added this function for handeling mask datasets of various
 * data types (byte only before that) */
MaskType* getAllocateMaskArray( THD_3dim_dataset *dset, char *errorString )
{
  long  v         = 0;    /* index over nvox */
  long  nvox      = 0;    /* number of voxels */
  int   datum     = 0;    /* datum type */

  MaskType* maskArray = NULL;

  ENTRY("getAllocateMaskArray");

  /* --- just making sure we have a dset to work with --- */
  /*      we should never get here                        */
  if( dset == NULL ) {
    snprintf(errorString, LONG_STRING, "getAllocateMaskArray: "
        "What happened?! Pointer to dataset is NULL!");

    RETURN(NULL);

  }
  if( !DSET_LOADED(dset) ) {
    snprintf(errorString, LONG_STRING, "getAllocateMaskArray: "
        "What happened?! Dataset is not in memory!");

    RETURN(NULL);
  }

  if ( DSET_NUM_TIMES(dset) > 1 ) {
    /* 3D+t as a mask dataset? */
    snprintf(errorString, LONG_STRING, "getAllocateMaskArray: "
        "Time dimension not supported!");
    RETURN(NULL);
  }

  /* --- initialize and allocate ---*/
  nvox = DSET_NVOX( dset );

  if( (maskArray = (MaskType *) malloc(sizeof(MaskType)*nvox)) == NULL) {
    snprintf(errorString, LONG_STRING, "getAllocateMaskArray: "
        "Memory allocation for dsetMask failed!");

    RETURN(NULL);
  }

  /* --- convert to internal mask representation (MaskType) --- */
  datum = DSET_BRICK_TYPE(dset,0);

  switch (datum) {
    case MRI_float: {

      float* tmp_dsetArray = (float *) DSET_ARRAY(dset,0);

      /* fill mask array */
      for( v=0; v<nvox; ++v ) {
        if( abs(tmp_dsetArray[v]) > 0.0000001f ) maskArray[v] = (MaskType) 1;
        else maskArray[v] = (MaskType) 0;
      }
    }
    break;

    case MRI_short: {

      short* tmp_dsetArray = (short *) DSET_ARRAY(dset,0);

      /* fill mask array */
      for( v=0; v<nvox; ++v ) {
        if( abs(tmp_dsetArray[v]) > 0 ) maskArray[v] = (MaskType) 1;
        else maskArray[v] = (MaskType) 0;
      }
    }
    break;

    case MRI_byte: {
      /* That's the datum type we want, but might define it differently
       * in the future, so doing cast regardless. */

      byte* tmp_dsetArray = (byte *) DSET_ARRAY(dset,0);

      /* fill mask array */
      for( v=0; v<nvox; ++v ) {
        if( tmp_dsetArray[v] > 0 ) maskArray[v] = (MaskType) 1;
        else maskArray[v] = (MaskType) 0;
      }
    }
    break;

    case MRI_rgb:
      snprintf(errorString, LONG_STRING,
          "Sorry, datum-type MRI_rgb (%d) is not supported!", datum);

      /* free end return */
      free(maskArray);
      RETURN(NULL);
      break;

    case MRI_complex:
      snprintf(errorString, LONG_STRING,
          "Sorry, datum-type MRI_complex (%d) is not supported!", datum);

      /* free end return */
      free(maskArray);
      RETURN(NULL);
      break;

    default:
      snprintf(errorString, LONG_STRING,
          "Unknown datum-type (%d)", datum);

      /* free end return */
      free(maskArray);
      RETURN(NULL);
      break;
  }

  RETURN(maskArray);
}


DatasetType** getAllocateDsetArray( THD_3dim_dataset *dset, char *errorString )
{
  long  v         = 0;    /* index over nvox */
  long  t         = 0;    /* index over nt */
  long  nt        = 0;    /* number of observations (time-points) total */
  long  nvox      = 0;    /* number of voxels */
  int   datum     = 0;    /* datum type */

  DatasetType **
    dsetArray     = NULL;

  ENTRY("getAllocateDsetArray");

  /* JL June 2011: Modified error handling: Passing error string as argument
   * to the calling function, allocated memory is freed, RETURN(1) 
   * instead of ERROR_exit.
  */
  
  /* --- just making sure we have a dset to work with --- */
  /*      we should never get here                        */
  if( dset == NULL) {
    snprintf(errorString, LONG_STRING, "getAllocateDsetArray: "
        "What happened?! Pointer to dataset is NULL!");

    RETURN(NULL);

  }
  if( !DSET_LOADED(dset) ) {
    snprintf(errorString, LONG_STRING, "getAllocateDsetArray: "
        "What happened?! Dataset is not in memory!");

    RETURN(NULL);
  }

  /* --- initialize and allocate ---*/
  nvox = DSET_NVOX( dset );
  nt = DSET_NUM_TIMES( dset );
  if( (dsetArray = Allocate2DT(nt, nvox)) == NULL ) {
    snprintf(errorString, LONG_STRING, "getAllocateDsetArray: "
        "Memory allocation for dsetArray failed!");

    RETURN(NULL);
  }


  /* ---  make sure all bricks have same datum --- */
  if ( !DSET_datum_constant(dset) ) {
    snprintf(errorString, LONG_STRING, "Creating dataset array failed! Sub-briks "
        "have different datum types!");
  
    /* -- free end return -- */
    free2DT( dsetArray, nt );

    RETURN(NULL);

  }

  /* --- converting data to internal representation (DatasetType) --- */
  datum = DSET_BRICK_TYPE(dset,0);

  switch (datum) {
    case MRI_float:
      for( t=0; t<nt; ++t ) {
        /* -- create 1D array to hold one volume -- */
        float* tmp_dsetArray = (float *) DSET_ARRAY(dset,t);

        /* -- create 2D array to hold [time][volume] -- */
        for( v=0; v<nvox; ++v ){
            dsetArray[t][v] = (DatasetType) tmp_dsetArray[v];
        }
      }
      break;
    case MRI_short:
      for( t=0; t<nt; ++t ) {

        /* -- create 1D array to hold one volume -- */
        short* tmp_dsetArray = (short *) DSET_ARRAY(dset,t);

        /* -- create 2D array to hold [time][volume] -- */
        for( v=0; v<nvox; ++v ){
            dsetArray[t][v] = (DatasetType) tmp_dsetArray[v];
        }
      }
      break;
    case MRI_byte:
      snprintf(errorString, LONG_STRING, 
          "Sorry, datum-type MRI_byte (%d) is not supported!", datum);
      
      /* free end return */
      free2DT( dsetArray, nt );
      RETURN(NULL);
      break;

    case MRI_rgb:
      snprintf(errorString, LONG_STRING, 
          "Sorry, datum-type MRI_rgb (%d) is not supported!", datum);
      
      /* free end return */
      free2DT( dsetArray, nt );
      RETURN(NULL);
      break;  

    case MRI_complex:
      snprintf(errorString, LONG_STRING,
          "Sorry, datum-type MRI_complex (%d) is not supported!", datum);

      /* free end return */
      free2DT( dsetArray, nt );
      RETURN(NULL);
 
      break;

    default:
      snprintf(errorString, LONG_STRING, 
          "Unknown datum-type (%d)", datum);

      /* free end return */
      free2DT( dsetArray, nt );
      RETURN(NULL);
 
      break;
  }

  RETURN(dsetArray);
}

/* TODO: Check if dset needs to be in memory in order to access
   DSET_NUM_TIMES() */
void freeDsetArray(THD_3dim_dataset *dset, DatasetType** dsetArray)
{
  long  nt        = 0;    /* number of observations (time-points) total */

  ENTRY("freeDsetArray");

  if( dset == NULL) EXRETURN;
  if( !DSET_LOADED(dset) ) EXRETURN; /*TODO: this might not be required ! */
                                    
  nt = DSET_NUM_TIMES( dset );
  free2DT( dsetArray, nt );
  
  EXRETURN;
}


int allocateModel( MODEL *model, AFNI_MODEL *afni_model, char *errorString ) 
{
  long nsv          = 0;   /* number of support vectors */
  long sv           = 0;   /* index over nsv */
  long nvox_masked  = 0;


  ENTRY("allocateModel");

  /* our approach to multiclass is to keep all training timepoints 
   * with non-support vectors as alpha = 0
   * thus the model "documents" and number of support vectors is
   * always the number of timepoints in in the training data
   *
   * JL July 2011: Modified error handling: Passing error message 
   * as argument (errorString) to the calling function, allocated memory
   * is freed, RETURN(1) instead of ERROR_exit()
   *               
   * JL July 2011: Replaced svm-light's my_malloc by malloc (or mcw_malloc,
   * I should say) this allows us to find memory problems and check
   * for out of memory in 3dsvm rather than in svm-light.
   * Doing that, it turns out that model->lin_weitghts causes corruption 
   * problems.
   *
   * TODO: need to fix this soon. It would be good to patch svm-light and
   * replace all my_malloc by malloc to see if there are leeks/corruption
   * problems.
   */

  if( afni_model == NULL ) {
    /* we should never get here */
    snprintf(errorString, LONG_STRING, "allocateModel: "
        "What happened? Can't access afni model!");

    RETURN(1);
  }

  /* -- initialize -- */
  if( !strcmp(afni_model->svm_type, "regression") ) {
    nsv = afni_model->total_support_vectors[0];
  }
  else if( !strcmp(afni_model->svm_type, "classification") ) {
    nsv = afni_model->timepoints + 1; 
     /* (timpoints  + 1) is svmlights number of support vectors */
  }

  nvox_masked = afni_model->total_masked_features[0];
    /* [0] assumes  that all models use the same mask */
  

  /* -- allocate -- */
  if( (model->supvec = (DOC **)malloc(sizeof(DOC *)*(nsv))) == NULL ) { 
    snprintf(errorString, LONG_STRING, "allocateModel: " 
        "Memory allocation for model->supvec failed!"); 

    RETURN(1);
  }

  for( sv=1; sv<nsv; ++sv ) {
    if( (model->supvec[sv] = (DOC *)calloc(sizeof(DOC), 1)) == NULL ) {
      snprintf(errorString, LONG_STRING, "allocateModel: " 
          "Memory allocation for model for model->supvec[%ld] failed!", sv);

      RETURN(1);
    }
    if( ((model->supvec[sv])->words = 
          (WORD *)calloc(sizeof(WORD), nvox_masked + 1)) == NULL ) {
            /* + 1 for end of list value */
      
      snprintf(errorString, LONG_STRING, "allocateModel: "
          "Memory allocation for model->supvec[%ld])->words failed!", sv);

      RETURN(1);
    }
  }

  if( (model->alpha = (double *)malloc(sizeof(double)*(nsv))) == NULL ) { 
    snprintf(errorString, LONG_STRING, "allocateModel: "
          "Memory allocation for model->alpha failed!");

    /* free and return */
    free(model->supvec);
    for( sv=1; sv<nsv; ++sv ) { 
      free( (model->supvec[sv])->words );
      free(model->supvec[sv]);
    }
    free(model->supvec);
    
    RETURN(1);
  }

  if( afni_model->kernel_type[0] == LINEAR ) {
    if( (model->lin_weights=(double *)my_malloc(sizeof(double)*nvox_masked + 1)) == NULL ) {

      snprintf(errorString, LONG_STRING, "allocateModel: "
          "Memory allocation for model->lin_weights failed!");

      /* free and return */
      free(model->supvec);
      for( sv=1; sv<nsv; ++sv ) { 
        free( (model->supvec[sv])->words );
        free(model->supvec[sv]);
      }
      free(model->supvec);
    
      RETURN(1);
    }
  }

  RETURN(0);
}

void freeModel( MODEL *model, AFNI_MODEL *afni_model, enum modes mode )
{

  long nsv = 0; 
  long sv  = 0;

  ENTRY("freeModel");

  
  /* Note from svm_learn_main.c:
     Warning: The model contains references to the original data 'docs'.
     If you want to free the original data, and only keep the model, you 
     have to make a deep copy of 'model'. */
  /* deep_copy_of_model=copy_model(model); */

  
  if( model == NULL ) {
    ERROR_message("Can't free svm-light model!");
    EXRETURN;
  }

  if( !strcmp(afni_model->svm_type, "regression") ) {
    nsv = afni_model->total_support_vectors[0];
  }
  else if( !strcmp(afni_model->svm_type, "classification") ) {
    nsv = afni_model->timepoints + 1;
                 /* (timepoints + 1) is svmlights number of support vectors */
  }

  if( mode == TEST ) {
    /* free the model throughly, we don't have other references to the
       original training data */
    for( sv=1; sv<nsv; ++sv) {
      free( (model->supvec[sv])->words );
      free(model->supvec[sv]);    
    }
    free(model->supvec);
    free(model->alpha);
  }
  else if( mode == TRAIN) {
    /* model->supvec are freed by freeing the DOCs in freeDOCs */

    free(model->supvec);
    free(model->alpha);
    free(model->index);
  }

  /* if(model->kernel_parm.kernel_type == LINEAR ) free(model->lin_weights); */

  EXRETURN;
}

void updateModel(MODEL *model, AFNI_MODEL *afni_model, int comb) 
{
  long i  = 0;
  long sv = 0;

  
  ENTRY("updateModel");

  model->kernel_parm.kernel_type = afni_model->kernel_type[comb];
  model->kernel_parm.poly_degree = afni_model->polynomial_degree[comb];
  model->kernel_parm.rbf_gamma = afni_model->rbf_gamma[comb];
  model->kernel_parm.coef_lin = (double) afni_model->linear_coefficient[comb];
  model->kernel_parm.coef_const = (double) afni_model->constant_coefficient[comb];
  model->totwords = (long) afni_model->total_masked_features[comb];
  strncpy(model->kernel_parm.custom, afni_model->kernel_custom[i], 50);
  model->b = (double) afni_model->b[comb];

  
  /* regression */
  if( !strcmp(afni_model->svm_type,"regression") )  {
    /* comb = 0 for regression */
    /* number of docs is doubled for regression */
    model->totdoc = (long) afni_model->timepoints*2;
    model->sv_num = (long) afni_model->total_support_vectors[0];

    sv=1;
    for( i=0; i<model->totdoc; ++i) {
      if ( fabs(afni_model->alphas[0][i]) > 0 ) {
        model->alpha[sv] = (double) afni_model->alphas[0][i];
      
        ++sv;
      }
    }
  }
  /* classification */
  else { 
  
  /* our approach to multiclass is to keep all training timepoints 
   * with non-support vectors as alpha = 0
   * thus the model "documents" and number of support vectors is
   * always the number of timepoints in in the training data
   *
   */
    model->totdoc = (long) afni_model->timepoints;
    model->sv_num = (long) afni_model->timepoints + 1;
    for( i=0 ; i< model->sv_num - 1 ; ++i ) {
      model->alpha[i+1] = (double)afni_model->alphas[comb][i];    
    }
  }

  if( model->kernel_parm.kernel_type == LINEAR ) {         
    /* essentially replacing call to add_weight_vector_to_linear_model(model)*/
    /* that function mallocs, which we don't want since we are re-using */
    
    /* JL July 2011: This part causes memory corruption problems. Read the comment
     * in allocateModel for more details. TODO: we should fix this soon. */
       
    clear_vector_n(model->lin_weights,model->totwords);
    for(i=1;i<model->sv_num;i++) {
      add_vector_ns(model->lin_weights,(model->supvec[i])->words, model->alpha[i]);
    }
  }

  if(verbosity >= 2) {
    INFO_message( "updateModel:");
    INFO_message( "sv_num = %ld", model->sv_num );
    INFO_message( "kernel_type = %ld", model->kernel_parm.kernel_type ); 
    INFO_message( "poly_degree = %ld", model->kernel_parm.poly_degree ); 
    INFO_message( "rbf_gamma = %lf", model->kernel_parm.rbf_gamma ); 
    INFO_message( "coef_lin = %lf", model->kernel_parm.coef_lin ); 
    INFO_message( "coef_const = %lf", model->kernel_parm.coef_const ); 
    INFO_message( "totwords = %ld", model->totwords ); 
    INFO_message( "totdoc = %ld", model->totdoc );
    INFO_message( "b = %lf", model->b );

    for( i=0 ; i< model->sv_num - 1 ; ++i ) {
      INFO_message(" model->alpha[%ld+1] = %e", i, model->alpha[i+1]);
    }

  }

  EXRETURN;
}

void freeModelArrays( DatasetType** dsetModelArray,
    MaskType* dsetMaskArray, long nt_model, int mask_used )
{

  ENTRY("freeModelArrays");

  if( mask_used == MASK_YES ) free(dsetMaskArray);
  free2DT(dsetModelArray, nt_model );


  EXRETURN;

}
/* JL May 2010: This functions produces and allocates the arrays holding the
 * model data and the model-mask data */
int getAllocateModelArrays(THD_3dim_dataset *dsetModel,
     DatasetType ***dsetModelArray, MaskType **dsetMaskArray,
     long *nt_model, long *nvox_model, int *mask_used,
     int noMaskFlag, char *errorString )
{
  long  v               = 0;  /* index over nvox_model */
  long  t               = 0;  /* index over nt_model */

  THD_3dim_dataset *
    dsetMask            = NULL;

  DatasetType **
    tmp_dsetArray       = NULL;

  MaskType *
    tmp_maskArrayPtr    = NULL;

  DatasetType **
    tmp_dsetModelArray  = NULL;

  MaskType *
    tmp_dsetMaskArray   = NULL;


  char *inModelFile          = NULL;
  char  inModelFileMask[LONG_STRING];
  char *inModelFileMaskExt   = MODEL_MSK_EXT;


  ENTRY("getAllocateModelArrays");


  /* JL Oct. 2010: Allocating model and mask array in this function.
   * If no mask was used for training, then dsetMaskArray == NULL. 
   * 
   * JL July 2011: Modified error handling: Passing error message 
   * as argument (errorString) to the calling function, 
   * allocated memory is freed, RETURN(1) instead of ERROR_exit().
   */ 


  /* --- initial error checking --- */
  if( !DSET_LOADED(dsetModel) ) {
    snprintf(errorString, LONG_STRING, "getAllocateModelArrays: "
        "What happened?! Model file not in memory!");

    RETURN(1);
  }

  if( (*mask_used != MASK_UNKNOWN) && (*mask_used != MASK_YES) &&
      (*mask_used != MASK_NO) ) { 
    snprintf(errorString, LONG_STRING, "getAllocateModelArrays: "
        "What happened?! Mask status unknown!");

    RETURN(1);
  }


  /* --- new way: mask is not unknown mask is stored in last 2 bricks
   *     (even if no mask was used!) --- */
  if (*mask_used != MASK_UNKNOWN) {

    /* -- initialize and allocate model array --*/
    *nvox_model = DSET_NVOX( dsetModel );
    *nt_model   = DSET_NUM_TIMES( dsetModel )-2; /* mask is stored in last 2 bricks */

    if( (tmp_dsetArray  = getAllocateDsetArray(dsetModel, errorString)) == NULL ) {

      RETURN(1);
    }

    if( (tmp_dsetModelArray = Allocate2DT( *nt_model, *nvox_model)) == NULL ) {
      snprintf(errorString, LONG_STRING, "getAllocateModelArrays: "
          "Memory allocation for tmp_dsetModelArray failed!");

      /* free and return */
      freeDsetArray(dsetModel, tmp_dsetArray);
      RETURN(1);
    }

    /* -- create model array --*/
    for (t=0; t<*nt_model; ++t) {
      for (v=0; v<*nvox_model; ++v) {
        tmp_dsetModelArray[t][v] = tmp_dsetArray[t][v];
      }
    }

    /* -- create mask array -- */
    if (*mask_used == MASK_YES) {

      /* - allocate mask array - */
      if( (tmp_dsetMaskArray = (MaskType *)malloc(*nvox_model*sizeof(MaskType))) == NULL ) {
        snprintf(errorString, LONG_STRING, "getAllocateModelArrays: "
            "Memory allocation for tmp_dsetMaskArray failed!");
      
        /* free and return */
        freeDsetArray(dsetModel, tmp_dsetArray);
        free2DT(tmp_dsetModelArray, *nt_model);
        RETURN(1);
      }

      for (v=0; v<*nvox_model; ++v) {
        tmp_dsetMaskArray[v] = (MaskType) tmp_dsetArray[*nt_model+1][v];
      }
    }
  }

  /* --- old way: mask is unknown, trying to locate mask dataset on disc --- */
  else {

    /* -- initialize and allocate -- */
    *nvox_model = DSET_NVOX( dsetModel );
    *nt_model = DSET_NUM_TIMES( dsetModel );
    inModelFile = DSET_PREFIX( dsetModel );
    
    if( (tmp_dsetArray  = getAllocateDsetArray(dsetModel, errorString)) == NULL ) { 

      RETURN(1);
    }

    if( (tmp_dsetModelArray = Allocate2DT( *nt_model, *nvox_model )) == NULL ) {
      snprintf(errorString, LONG_STRING, "getAllocateModelArrays: "
          "Memory allocation for tmp_dsetModelArray failed!");
      
      /* free and return */
      freeDsetArray(dsetModel, tmp_dsetArray);
      RETURN(1);
    }

    /* -- create model array --*/
    for (t=0; t<*nt_model; ++t) {
      for (v=0; v<*nvox_model; ++v) {
        tmp_dsetModelArray[t][v] = tmp_dsetArray[t][v];
      }
    }

    if ( !noMaskFlag ) {
    /* -- create mask array --*/
    /* - determine view type - */
      strncpy(inModelFileMask, inModelFile, LONG_STRING);
      strncat(inModelFileMask, inModelFileMaskExt, LONG_STRING);

      if (dsetModel->view_type == VIEW_ORIGINAL_TYPE) {
        strncat(inModelFileMask,"+orig", LONG_STRING);
      }
      else if (dsetModel->view_type == VIEW_TALAIRACH_TYPE) {
        strncat(inModelFileMask,"+tlrc", LONG_STRING);
      }
      else if (dsetModel->view_type == VIEW_ACPCALIGNED_TYPE)  {
        strncat(inModelFileMask,"+acpc", LONG_STRING);
      }
      else {
        snprintf(errorString, LONG_STRING,
            "Viewtype of model: %s unknown!", inModelFile);

        /* free and return */
        freeDsetArray(dsetModel, tmp_dsetArray);
        free2DT(tmp_dsetModelArray, *nt_model);
        RETURN(1);
      }

      /* - open mask dataset - */
      if( (dsetMask = THD_open_one_dataset( inModelFileMask )) != NULL ) { 
        
        /* mask dataset found */
        *mask_used = MASK_YES;
        DSET_load( dsetMask );

        /* get pointer to mask array */
        tmp_maskArrayPtr = (MaskType *)DSET_ARRAY(dsetMask,0);

        if( (tmp_dsetMaskArray = (MaskType *)malloc(*nvox_model*sizeof(MaskType))) == NULL ) {
          snprintf(errorString, LONG_STRING, "getAllocateModelArrays: "
              "Memory allocation for tmp_dsetMaskArray failed!");
        
          /* free and return */
          freeDsetArray(dsetModel, tmp_dsetArray);
          free2DT(tmp_dsetModelArray, *nt_model);
          DSET_unload(dsetMask);
          RETURN(1);
        }

        for (v=0; v<*nvox_model; ++v) tmp_dsetMaskArray[v] = tmp_maskArrayPtr[v];

        /* - free memory mask dataset */
        DSET_unload(dsetMask);
      }
      else { /* mask dataset not found */
        snprintf(errorString, LONG_STRING,
            "Failed to open mask dataset: %s! \n\n" 
            "   You are using an outdated model file!\n"
            "   Make sure the mask file is in your current working directory!\n"
            "   If no mask file was used for training use "
            "   option -nomodelmask!", inModelFileMask );

        /* free and return */
        freeDsetArray(dsetModel, tmp_dsetArray);
        free2DT(tmp_dsetModelArray, *nt_model);
        RETURN(1);
      }
    }
    else { /* (option -nomodelmask) */
      *mask_used = MASK_NO;
    }
  }

  /* -- free temporary memory */
  free(tmp_dsetArray);

  *dsetMaskArray =  tmp_dsetMaskArray;
  *dsetModelArray = tmp_dsetModelArray;

  RETURN(0);
}


/* just fills in the model data set (assumed constant accross class combinations) */
/* Need to also use updateModel for class */
/* The idea is to only call this once and then updateModel for combination specific aspects */
int get_svm_model(MODEL *model, DatasetType **dsetModelArray, 
    MaskType *dsetMaskArray, AFNI_MODEL *afni_model, long model_vox, 
    int noMaskFlag, char *errorString)
{
  long i = 0;
  long j = 0;
  long k = 0;

  long nt        = 0; /* number of timepoints */
  long t         = 0; /* index of nt */
  long v         = 0; /* index over model_vox */
  long nvox_msk  = 0; /* number of masked voxels */
  long vmsk      = 0; /* index over nvox_msk */
  long sv        = 0; /* sv index */
  
  ENTRY("get_svm_model");

  /* JL June 2014: Changed how alphas are retrieved for regression */
 
  if( !strcmp(afni_model->svm_type,"regression") ) { 
    nt = afni_model->timepoints;
    nvox_msk = (long) afni_model->total_masked_features[0];

    /* For regression, the array storing the alphas is twice as 
     * long as for classification. Since the number of timepoints
     * in the model (number of observations in training data) is 
     * not twice as long, alphas of index nt+t belong to model data 
     * of index t. */
    
    sv=1;
    for( k=0; k<2; k++) { 
      for( t=0; t<nt; ++t ) {
        vmsk=0;
        if( fabs(afni_model->alphas[0][k*nt+t]) > 0.0 ) {

          if( sv >= afni_model->total_support_vectors[0] ) {
            /* should never get here */
            snprintf(errorString, LONG_STRING, "Reading model failed. More SVs than expected!");
            RETURN(1);
          }

          for( v=0; v<model_vox; ++v ) {
            if( vmsk<nvox_msk ) {
              if( noMaskFlag ) { /* no mask */
                (model->supvec[sv])->words[vmsk].wnum = vmsk + 1;
                (model->supvec[sv])->words[vmsk].weight = 
                  (float)dsetModelArray[t][v];
              
                ++vmsk;
              }
              else {
                if( dsetMaskArray[v] ) { /* mask */
                  (model->supvec[sv])->words[vmsk].wnum = vmsk + 1;
                  (model->supvec[sv])->words[vmsk].weight = 
                    (float)dsetModelArray[t][v];

                  ++vmsk;
                }
              } 
            }
          }
          (model->supvec[sv])->words[vmsk].wnum=0; /* end of list */
          (model->supvec[sv])->twonorm_sq = sprod_ss((model->supvec[sv])->words, 
              (model->supvec[sv])->words);
          (model->supvec[sv])->docnum = -1;

          ++sv;
        }
      }
    }
  }
  else { /* before sv-regression: */

    for(i = 1; i < afni_model->timepoints + 1; i++) {  
      /* number of support vectors is (afni_model->timepoints + 1) */
      /* this simplifies multi-class life by allowing us to essentially */
      /* store and read the training data once, one brick, etc. */
      /* the real number is the number of non-zero alphas */
    
      k = 0;
      for( j=0 ; j< model_vox; ++j) {
        // if( dsetMaskArray[j] && (k < (long) afni_model->total_masked_features[0]) ) 
        // --- before dealing with noMaskFlag, used this
        if( k < (long) afni_model->total_masked_features[0] ) { 
          /* [0] assumes mask is the same for all class combinations */
          if( noMaskFlag ) { 
            (model->supvec[i])->words[k].wnum = k + 1; /* value should start at 1 */
            (model->supvec[i])->words[k].weight = (float)dsetModelArray[i-1][j];
            // printf("%ld: %f ", (model->supvec[i])->words[k].wnum, (model->supvec[i])->words[k].weight );
            ++k;
          }
          else if( dsetMaskArray[j] ) { 
            (model->supvec[i])->words[k].wnum = k + 1; /* value should start at 1 */
            (model->supvec[i])->words[k].weight = (float)dsetModelArray[i-1][j];
            // printf("%ld: %f ", (model->supvec[i])->words[k].wnum, (model->supvec[i])->words[k].weight );
            ++k;
          } 
        }
      }
      // printf("\n");
      ((model->supvec[i])->words[k]).wnum=0; /* end of list */
      (model->supvec[i])->twonorm_sq = sprod_ss((model->supvec[i])->words,
						 (model->supvec[i])->words);
      (model->supvec[i])->docnum = -1;
    }
  } 

  RETURN(0);
}

int readAllocateAfniModel( THD_3dim_dataset *dsetModel, AFNI_MODEL *afniModel, char *errorString )
{
  ATR_float *  atr_float   = NULL;
  ATR_int *    atr_int     = NULL;
  ATR_string * atr_string  = NULL;
  long i,j                 = 0;
  long nalphas             = 0;



  /* used for strtok magic */
  long p_string_size     = 0;    /* string size p, dependent on the
                                  number of class combinations */
  char *p                = NULL;
  char *q                = NULL;
  long max_comb          = CLASS_MAX*(CLASS_MAX-1)/2;


  char headernames[LONG_STRING];



  ENTRY("readAllocateAfniModel");


  /* JL Oct 2009: The naming and number of model parameters in the
   * model header has changed. We added "3DSVM" in front of each parameter name
   * to avoid collisions with header entries from other afni programs.
   *
   * JL Apr. 2010: Allocating p string (for strtok) dynamically.
   * Replaced all string functions by its equivalent that also takes the
   * string size as an argument
   *
   * JL Apr. 2010: Allocating combNames and kernel_custome dynamically
   * based on CLASS_MAX and CSV_STRING
   *
   * JL May  2010: Added 3DSVM_MASK_USED to determine if mask was used for
   * training or not. (Now, the mask is a sub-brick of the model dataset)
   *
   * JL May  2010: Trying to be backwards compatible based on version number
   *
   * JL July 2011: Modified error handling: Passing error message 
   * as argument (errorString) to the calling function. Checking allocation 
   * for every model parameter, if malloc fails, free memory and 
   * RETURN(1) instead of ERROR_exit().
   *
   */

   
  /* --- determine version number for backward compatiblity ---*/
  atr_string = THD_find_string_atr( dsetModel->dblk, "3DSVM_VERSION" );

  /* version number in model header (introduced Apr. 2010) */
  if( atr_string != NULL ) {
    afniModel->version = atof((char *)(atr_string->ch+1));
  }
  else { /* no version number in header, assign version number based on
   header entries */

    /* check if 3DSVM_TYPE is present (introduced Oct. 2009)
     * assign version number 0.90 */
    atr_string = THD_find_string_atr( dsetModel->dblk, "3DSVM_SVM_TYPE" );
    if (atr_string != NULL) afniModel->version = 0.90;
    else { /* maybe even older */

      /* check if COMBO_NAMES is present
       * assign version number 0.80
       */
      atr_string = THD_find_string_atr( dsetModel->dblk, "COMBO_NAMES" );
      if (atr_string != NULL) afniModel->version = 0.80;
      else { /* out of luck */
        snprintf(errorString, LONG_STRING, "Can not read model!");
        RETURN(1);
      }
    }
  }
  /* ----------------------------------------------------------------------  */
  /* --- read header information based on 3DSVM VERSION ---*/
  /* ----------------------------------------------------------------------  */
  if (afniModel->version >= 0.90) { 
    /* -- initialize -- */
    atr_int = THD_find_int_atr( dsetModel->dblk, "3DSVM_CLASS_COMBINATIONS" );
    afniModel->combinations = *atr_int->in;

    /* - allocate CSV strings - */
    p_string_size = afniModel->combinations*CSV_STRING;

    if( (p = (char *)malloc(p_string_size*sizeof(char))) == NULL ) {
      snprintf(errorString, LONG_STRING, "readAllocateAfniModel: "
          "Memory allocation for csv-string failed!");
      RETURN(1);
     }
    
    /* - allocate 2D char arrays - */
    if( (afniModel->combName = Allocate2c(max_comb, (long)CSV_STRING)) == NULL ) {
      snprintf(errorString, LONG_STRING, "readAllocateAfniModel: "
          "Memory allocation for combName failed!");

      /* free and return */
      free(p);
      RETURN(1);
    }
    Clear2c(afniModel->combName, max_comb);

    if( (afniModel->kernel_custom = Allocate2c(max_comb, (long)CSV_STRING)) == NULL ) {
      snprintf(errorString, LONG_STRING, "readAllocateAfniModel: "
          "Memory allocation for kernel_custom failed!");

      /* free and return */
      free(p);
      free2c(afniModel->combName, max_comb);
      RETURN(1);
    }
    Clear2c(afniModel->kernel_custom, max_comb);

    /* -- allocate and read header entries -- */
    /* JL Oct 2009 */
    atr_string = THD_find_string_atr( dsetModel->dblk, "3DSVM_SVM_TYPE" );
    strncpy(afniModel->svm_type, atr_string->ch, LONG_STRING);

    /* JL May 2010 */
    if (afniModel->version >= 1.10) {
      atr_int = THD_find_int_atr(dsetModel->dblk, "3DSVM_MASK_USED");
      afniModel->mask_used = *atr_int->in;
    }
    else afniModel->mask_used = MASK_UNKNOWN;

    atr_int = THD_find_int_atr( dsetModel->dblk, "3DSVM_CLASS_COUNT" );
    afniModel->class_count = *atr_int->in;

    atr_int = THD_find_int_atr( dsetModel->dblk, "3DSVM_TIMEPOINTS" );
    afniModel->timepoints = *atr_int->in;

    atr_string = THD_find_string_atr( dsetModel->dblk, "3DSVM_COMBO_NAMES" );
    strncpy(p, atr_string->ch, p_string_size);
    q = strtok(p,",");
    if (q != NULL) strncpy(afniModel->combName[0], q, CSV_STRING);
    else {
      snprintf(errorString, LONG_STRING, "Reading model combinations in header "
          "file failed");

      /* free and return */
      free(p);
      free2c(afniModel->combName, max_comb);
      free2c(afniModel->kernel_custom, max_comb);
      RETURN(1);
    }

    for(i = 1; i < afniModel->combinations; ++i) {
      q=strtok(NULL,",");
      if (q != NULL) strncpy(afniModel->combName[i], q, CSV_STRING);
      else {
        snprintf(errorString, LONG_STRING, 
            "Reading model combinations in header file failed! "
            "Number does not match expected: '%d'", afniModel->combinations);
        
        /* free and return */
        free(p);
        free2c(afniModel->combName, max_comb);
        free2c(afniModel->kernel_custom, max_comb);
        RETURN(1);
      }
    }

    atr_int = THD_find_int_atr( dsetModel->dblk, "3DSVM_KERNEL_TYPE" );
    if( (afniModel->kernel_type = (int *)malloc( atr_int->nin * sizeof(int) )) == NULL) {
      snprintf(errorString, LONG_STRING, "readAllocateAfniModel: "
          "Memory allocation for kernel_type failed!"); 

      /* free and return */
      free(p);
      free2c(afniModel->combName, max_comb);
      free2c(afniModel->kernel_custom, max_comb);
      RETURN(1);
    }
    for( i=0 ; i<atr_int->nin ; ++i ) {
      afniModel->kernel_type[i] = atr_int->in[i];
    }
  
    atr_string = THD_find_string_atr( dsetModel->dblk, "3DSVM_KERNEL_CUSTOM" );
    strncpy(p, atr_string->ch, p_string_size);
    q = strtok(p,",");
    if (q != NULL) strncpy(afniModel->kernel_custom[0],q, CSV_STRING);
    else {
      snprintf(errorString, LONG_STRING, 
          "Can't find 3DSVM_KERNEL_CUSTOM in model header file");


      /* free and return */
      free(p);
      free2c(afniModel->combName, max_comb);
      free2c(afniModel->kernel_custom, max_comb);
      free(afniModel->kernel_type);
      RETURN(1);
    }
    for( i=1; i<afniModel->combinations; ++i ) {
      q=strtok(NULL,",");
      if (q != NULL) strncpy(afniModel->kernel_custom[i], q, CSV_STRING);
      else {
        snprintf(errorString, LONG_STRING, 
            "Reading 3DSVM_KERNEL_CUSTOM in model header file number of class"
            "combinations does not match expected:'%d'", afniModel->combinations);

        /* free and return */
        free(p);
        free2c(afniModel->combName, max_comb);
        free2c(afniModel->kernel_custom, max_comb);
        free(afniModel->kernel_type);
        RETURN(1);
      }
    }
  
    atr_float = THD_find_float_atr( dsetModel->dblk, "3DSVM_RBF_GAMMA" );
    if( (afniModel->rbf_gamma = (float *)malloc( atr_float->nfl * sizeof(float))) == NULL ) {
      snprintf(errorString, LONG_STRING, "readAllocateAfniModel: "
          "Memory allocation for rbf_gamma failed!"); 
      
      /* free and return */
      free(p);
      free2c(afniModel->combName, max_comb);
      free2c(afniModel->kernel_custom, max_comb);
      free(afniModel->kernel_type);
      RETURN(1);
    }
    for( i=0 ; i<atr_float->nfl ; ++i ) {
      afniModel->rbf_gamma[i] = atr_float->fl[i];
    }
  
    atr_float = THD_find_float_atr( dsetModel->dblk, "3DSVM_LINEAR_COEFFICIENT" );
    if( (afniModel->linear_coefficient = (float *)malloc( atr_float->nfl * sizeof(float))) == NULL ) {
      snprintf(errorString, LONG_STRING, "readAllocateAfniModel: "
          "Memory allocation for linear_coefficient failed!");
      
      /* free and return */
      free(p);
      free2c(afniModel->combName, max_comb);
      free2c(afniModel->kernel_custom, max_comb);
      free(afniModel->kernel_type);
      free(afniModel->rbf_gamma);
      RETURN(1);
    }
    for( i=0 ; i<atr_float->nfl ; ++i ) {
      afniModel->linear_coefficient[i] = atr_float->fl[i];
    }
  
    atr_float = THD_find_float_atr( dsetModel->dblk, "3DSVM_CONSTANT_COEFFICIENT" );
    if( (afniModel->constant_coefficient = (float *)malloc( atr_float->nfl * sizeof(float))) == NULL ) {
      snprintf(errorString, LONG_STRING, "readAllocateAfniModel: "
          "Memory allocation for constant_coefficient failed!");
      
      /* free and return */
      free(p);
      free2c(afniModel->combName, max_comb);
      free2c(afniModel->kernel_custom, max_comb);
      free(afniModel->kernel_type);
      free(afniModel->rbf_gamma);
      free(afniModel->linear_coefficient);
      RETURN(1);
    }
    for( i=0 ; i<atr_float->nfl ; ++i ) {
      afniModel->constant_coefficient[i] = atr_float->fl[i];
    }
  
    atr_int = THD_find_int_atr( dsetModel->dblk, "3DSVM_TOTAL_MASKED_FEATURES" );
    if( (afniModel->total_masked_features = (int *)malloc( atr_int->nin * sizeof(int))) == NULL ) {
      snprintf(errorString, LONG_STRING, "readAllocateAfniModel: "
          "Memory allocation for total_masked_features failed!");

      /* free and return */
      free(p);
      free2c(afniModel->combName, max_comb);
      free2c(afniModel->kernel_custom, max_comb);
      free(afniModel->kernel_type);
      free(afniModel->rbf_gamma);
      free(afniModel->linear_coefficient);
      free(afniModel->constant_coefficient);
      RETURN(1);
    }
    for( i=0 ; i<atr_int->nin ; ++i ) {
      afniModel->total_masked_features[i] = atr_int->in[i];
    }
  
    atr_int = THD_find_int_atr( dsetModel->dblk, "3DSVM_TOTAL_SAMPLES" );
    if( (afniModel->total_samples = (int *)malloc( atr_int->nin * sizeof(int))) == NULL ) {
      snprintf(errorString, LONG_STRING, "readAllocateAfniModel: "
          "Memory allocation for total_samples failed!");
      
      /* free and return */
      free(p);
      free2c(afniModel->combName, max_comb);
      free2c(afniModel->kernel_custom, max_comb);
      free(afniModel->kernel_type);
      free(afniModel->rbf_gamma);
      free(afniModel->linear_coefficient);
      free(afniModel->constant_coefficient);
      free(afniModel->total_masked_features);
      RETURN(1);
    }
    for( i=0 ; i<atr_int->nin ; ++i ) {
      afniModel->total_samples[i] = atr_int->in[i];
    }
  
    atr_int = THD_find_int_atr( dsetModel->dblk, "3DSVM_TOTAL_SUPPORT_VECTORS" );
    if( (afniModel->total_support_vectors = (int *)malloc( atr_int->nin * sizeof(int))) == NULL ) {
      snprintf(errorString, LONG_STRING, "readAllocateAfniModel: "
          "Memory allocation for total_support_vectors failed!");
      
      /* free and return */
      free(p);
      free2c(afniModel->combName, max_comb);
      free2c(afniModel->kernel_custom, max_comb);
      free(afniModel->kernel_type);
      free(afniModel->rbf_gamma);
      free(afniModel->linear_coefficient);
      free(afniModel->constant_coefficient);
      free(afniModel->total_masked_features);
      free(afniModel->total_samples); 
      RETURN(1);
    }
    for( i=0 ; i<atr_int->nin ; ++i ) {
      afniModel->total_support_vectors[i] = atr_int->in[i];
    }
  
    atr_float = THD_find_float_atr( dsetModel->dblk, "3DSVM_B" );
    if( ( afniModel->b = (float *)malloc( atr_float->nfl * sizeof(float))) == NULL ) {
      snprintf(errorString, LONG_STRING, "readAllocateAfniModel: "
          "Memory allocation for b failed!");

      /* free and return */
      free(p);
      free2c(afniModel->combName, max_comb);
      free2c(afniModel->kernel_custom, max_comb);
      free(afniModel->kernel_type);
      free(afniModel->rbf_gamma);
      free(afniModel->linear_coefficient);
      free(afniModel->constant_coefficient);
      free(afniModel->total_masked_features);
      free(afniModel->total_samples); 
      free(afniModel->total_support_vectors); 
      RETURN(1);
    }
    for( i=0 ; i<atr_float->nfl ; ++i ) {
      afniModel->b[i] = atr_float->fl[i];
    }
  
    atr_int = THD_find_int_atr( dsetModel->dblk, "3DSVM_POLYNOMIAL_DEGREE" );
    if( (afniModel->polynomial_degree = (int *)malloc( atr_int->nin * sizeof(int))) == NULL ) {
      snprintf(errorString, LONG_STRING, "readAllocateAfniModel: "
          "Memory allocation for polynomial_degree failed!");
      /* free and return */
      free(p);
      free2c(afniModel->combName, max_comb);
      free2c(afniModel->kernel_custom, max_comb);
      free(afniModel->kernel_type);
      free(afniModel->rbf_gamma);
      free(afniModel->linear_coefficient);
      free(afniModel->constant_coefficient);
      free(afniModel->total_masked_features);
      free(afniModel->total_samples); 
      free(afniModel->total_support_vectors); 
      free(afniModel->b);
      RETURN(1);
    }
    for( i=0 ; i<atr_int->nin ; ++i ) {
      afniModel->polynomial_degree[i] = atr_int->in[i];
    }

    /* For regression, the array holding the alphas needs to be twice as long as for 
     * classification */
    if( !strcmp(afniModel->svm_type, "regression") ) {
      nalphas=afniModel->timepoints*2;
    }
    else if( !strcmp(afniModel->svm_type, "classification") ) {
      nalphas=afniModel->timepoints;
    }
    /* we should never get here */
    else {
      snprintf(errorString, LONG_STRING, "Can not read model! SVM type unknown!");
      RETURN(1);
    }
  
    if( (afniModel->alphas = Allocate2f((long) afniModel->combinations, 
            nalphas)) == NULL ) {
      snprintf(errorString, LONG_STRING, "readAllocateAfniModel: "
          "Memory allocation for alphas failed!");

      /* free and return */
      free(p);
      free2c(afniModel->combName, max_comb);
      free2c(afniModel->kernel_custom, max_comb);
      free(afniModel->kernel_type);
      free(afniModel->rbf_gamma);
      free(afniModel->linear_coefficient);
      free(afniModel->constant_coefficient);
      free(afniModel->total_masked_features);
      free(afniModel->total_samples); 
      free(afniModel->total_support_vectors); 
      free(afniModel->b);
      free(afniModel->polynomial_degree);
      RETURN(1);
    }

    for(i = 0; i < afniModel->combinations; ++i ) {
      snprintf(headernames, LONG_STRING, "3DSVM_ALPHAS_%s", afniModel->combName[i]);
      atr_float = THD_find_float_atr( dsetModel->dblk, headernames); 
      for(j = 0; j < nalphas; ++j ) {
        afniModel->alphas[i][j] = (double)atr_float->fl[j];
      }
    }


    /* JL Nov 2009: new parameters: */
    atr_float = THD_find_float_atr( dsetModel->dblk, "3DSVM_SVM_C" );
    if( (afniModel->svm_c = (float *)malloc( atr_float->nfl * sizeof(float))) == NULL ) {
      snprintf(errorString, LONG_STRING, "readAllocateAfniModel: "
          "Memory allocation for svm_c failed!");

      /* free and return */
      free(p);
      free2c(afniModel->combName, max_comb);
      free2c(afniModel->kernel_custom, max_comb);
      free(afniModel->kernel_type);
      free(afniModel->rbf_gamma);
      free(afniModel->linear_coefficient);
      free(afniModel->constant_coefficient);
      free(afniModel->total_masked_features);
      free(afniModel->total_samples); 
      free(afniModel->total_support_vectors); 
      free(afniModel->b);
      free(afniModel->polynomial_degree);
      free2f(afniModel->alphas, afniModel->combinations);
      RETURN(1);
    }
    for (i=0; i < afniModel->combinations; ++i ) {
      afniModel->svm_c[i] = atr_float->fl[i];
    }

    atr_float = THD_find_float_atr( dsetModel->dblk, "3DSVM_EPS" );
    if( (afniModel->eps = (float *)malloc( atr_float->nfl * sizeof(float))) == NULL ) {
      snprintf(errorString, LONG_STRING, "readAllocateAfniModel: "
          "Memory allocation for eps failed!");

      /* free and return */
      free(p);
      free2c(afniModel->combName, max_comb);
      free2c(afniModel->kernel_custom, max_comb);
      free(afniModel->kernel_type);
      free(afniModel->rbf_gamma);
      free(afniModel->linear_coefficient);
      free(afniModel->constant_coefficient);
      free(afniModel->total_masked_features);
      free(afniModel->total_samples); 
      free(afniModel->total_support_vectors); 
      free(afniModel->b);
      free(afniModel->polynomial_degree);
      free2f(afniModel->alphas, afniModel->combinations);
      free(afniModel->svm_c);
      RETURN(1);
    }
    for (i=0; i < afniModel->combinations; ++i ) {
      afniModel->eps[i] = atr_float->fl[i];
    }

    atr_int = THD_find_int_atr( dsetModel->dblk, "3DSVM_BIASED_HYPERPLANE" );
    if( (afniModel->biased_hyperplane = (int *)malloc( atr_int->nin * sizeof(int))) == NULL ) {
      snprintf(errorString, LONG_STRING, "readAllocateAfniModel: "
          "Memory allocation for biased_hyperplane failed!");

      /* free and return */
      free(p);
      free2c(afniModel->combName, max_comb);
      free2c(afniModel->kernel_custom, max_comb);
      free(afniModel->kernel_type);
      free(afniModel->rbf_gamma);
      free(afniModel->linear_coefficient);
      free(afniModel->constant_coefficient);
      free(afniModel->total_masked_features);
      free(afniModel->total_samples); 
      free(afniModel->total_support_vectors); 
      free(afniModel->b);
      free(afniModel->polynomial_degree);
      free2f(afniModel->alphas, afniModel->combinations);
      free(afniModel->svm_c);
      free(afniModel->eps);
      RETURN(1);
    }
    for( i=0; i<afniModel->combinations; ++i ) {
      afniModel->biased_hyperplane[i] = atr_int->in[i];
     }

    atr_int = THD_find_int_atr( dsetModel->dblk, "3DSVM_SKIP_FINAL_OPT_CHECK" );
    if( (afniModel->skip_final_opt_check = (int *)malloc( atr_int->nin * sizeof(int))) == NULL ) {
      snprintf(errorString, LONG_STRING, "readAllocateAfniModel: "
          "Memory allocation for skip_final_opt_check failed!");

      /* free and return */
      free(p);
      free2c(afniModel->combName, max_comb);
      free2c(afniModel->kernel_custom, max_comb);
      free(afniModel->kernel_type);
      free(afniModel->rbf_gamma);
      free(afniModel->linear_coefficient);
      free(afniModel->constant_coefficient);
      free(afniModel->total_masked_features);
      free(afniModel->total_samples); 
      free(afniModel->total_support_vectors); 
      free(afniModel->b);
      free(afniModel->polynomial_degree);
      free2f(afniModel->alphas, afniModel->combinations);
      free(afniModel->svm_c);
      free(afniModel->eps);
      free(afniModel->biased_hyperplane);
      RETURN(1);
    }
    for( i=0; i<afniModel->combinations; ++i ) {
      afniModel->skip_final_opt_check[i] = atr_int->in[i];
    }

    atr_int = THD_find_int_atr( dsetModel->dblk, "3DSVM_SVM_MAXQPSIZE" );
    if( (afniModel->svm_maxqpsize = (int *)malloc( atr_int->nin * sizeof(int))) == NULL ) {
      snprintf(errorString, LONG_STRING, "readAllocateAfniModel: "
          "Memory allocation for svm_maxqpsize failed!");

      /* free and return */
      free(p);
      free2c(afniModel->combName, max_comb);
      free2c(afniModel->kernel_custom, max_comb);
      free(afniModel->kernel_type);
      free(afniModel->rbf_gamma);
      free(afniModel->linear_coefficient);
      free(afniModel->constant_coefficient);
      free(afniModel->total_masked_features);
      free(afniModel->total_samples); 
      free(afniModel->total_support_vectors); 
      free(afniModel->b);
      free(afniModel->polynomial_degree);
      free2f(afniModel->alphas, afniModel->combinations);
      free(afniModel->svm_c);
      free(afniModel->eps);
      free(afniModel->biased_hyperplane);
      free(afniModel->skip_final_opt_check);
      RETURN(1);
    }
    for( i=0; i<afniModel->combinations; ++i ) {
      afniModel->svm_maxqpsize[i] = atr_int->in[i];
    }

    atr_int = THD_find_int_atr( dsetModel->dblk, "3DSVM_SVM_NEWVARSINQP" );
    if( (afniModel->svm_newvarsinqp = (int *)malloc( atr_int->nin * sizeof(int))) == NULL ) {
       snprintf(errorString, LONG_STRING, "readAllocateAfniModel: "
          "Memory allocation for svm_newvarsinqp failed!");

      /* free and return */
      free(p);
      free2c(afniModel->combName, max_comb);
      free2c(afniModel->kernel_custom, max_comb);
      free(afniModel->kernel_type);
      free(afniModel->rbf_gamma);
      free(afniModel->linear_coefficient);
      free(afniModel->constant_coefficient);
      free(afniModel->total_masked_features);
      free(afniModel->total_samples); 
      free(afniModel->total_support_vectors); 
      free(afniModel->b);
      free(afniModel->polynomial_degree);
      free2f(afniModel->alphas, afniModel->combinations);
      free(afniModel->svm_c);
      free(afniModel->eps);
      free(afniModel->biased_hyperplane);
      free(afniModel->skip_final_opt_check);
      free(afniModel->svm_maxqpsize);
      RETURN(1);
    }
    for( i=0; i<afniModel->combinations; ++i ) {
      afniModel->svm_newvarsinqp[i] = atr_int->in[i];
    }

    atr_int = THD_find_int_atr( dsetModel->dblk, "3DSVM_SVM_ITER_TO_SHRINK");
    if( (afniModel->svm_iter_to_shrink = (int *)malloc( atr_int->nin * sizeof(int))) == NULL ) {
      snprintf(errorString, LONG_STRING, "readAllocateAfniModel: "
          "Memory allocation for svm_iter_to_shrink failed!");


      /* free and return */
      free(p);
      free2c(afniModel->combName, max_comb);
      free2c(afniModel->kernel_custom, max_comb);
      free(afniModel->kernel_type);
      free(afniModel->rbf_gamma);
      free(afniModel->linear_coefficient);
      free(afniModel->constant_coefficient);
      free(afniModel->total_masked_features);
      free(afniModel->total_samples); 
      free(afniModel->total_support_vectors); 
      free(afniModel->b);
      free(afniModel->polynomial_degree);
      free2f(afniModel->alphas, afniModel->combinations);
      free(afniModel->svm_c);
      free(afniModel->eps);
      free(afniModel->biased_hyperplane);
      free(afniModel->skip_final_opt_check);
      free(afniModel->svm_maxqpsize);
      free(afniModel->svm_newvarsinqp);
      RETURN(1);
    }
    for( i=0; i<afniModel->combinations; ++i ) {
      afniModel->svm_iter_to_shrink[i] = atr_int->in[i];
    }

    atr_float = THD_find_float_atr( dsetModel->dblk, "3DSVM_TRANSDUCTION_POSRATIO" );
    if( (afniModel->transduction_posratio = (float *)malloc( atr_float->nfl * sizeof(float))) == NULL ) {
      snprintf(errorString, LONG_STRING, "readAllocateAfniModel: "
          "Memory allocation for transduction_posratio failed!");

      /* free and return */
      free(p);
      free2c(afniModel->combName, max_comb);
      free2c(afniModel->kernel_custom, max_comb);
      free(afniModel->kernel_type);
      free(afniModel->rbf_gamma);
      free(afniModel->linear_coefficient);
      free(afniModel->constant_coefficient);
      free(afniModel->total_masked_features);
      free(afniModel->total_samples); 
      free(afniModel->total_support_vectors); 
      free(afniModel->b);
      free(afniModel->polynomial_degree);
      free2f(afniModel->alphas, afniModel->combinations);
      free(afniModel->svm_c);
      free(afniModel->eps);
      free(afniModel->biased_hyperplane);
      free(afniModel->skip_final_opt_check);
      free(afniModel->svm_maxqpsize);
      free(afniModel->svm_newvarsinqp);
      free(afniModel->svm_iter_to_shrink);
      RETURN(1);
      
    }
    for (i=0; i < afniModel->combinations; ++i ) {
      afniModel->transduction_posratio[i] = atr_float->fl[i];
    }

    atr_float = THD_find_float_atr( dsetModel->dblk, "3DSVM_SVM_COSTRATIO" );
    if( (afniModel->svm_costratio = (float *)malloc( atr_float->nfl * sizeof(float))) == NULL ) {
      snprintf(errorString, LONG_STRING, "readAllocateAfniModel: "
          "Memory allocation for svm_costratio failed!");

      /* free and return */
      free(p);
      free2c(afniModel->combName, max_comb);
      free2c(afniModel->kernel_custom, max_comb);
      free(afniModel->kernel_type);
      free(afniModel->rbf_gamma);
      free(afniModel->linear_coefficient);
      free(afniModel->constant_coefficient);
      free(afniModel->total_masked_features);
      free(afniModel->total_samples); 
      free(afniModel->total_support_vectors); 
      free(afniModel->b);
      free(afniModel->polynomial_degree);
      free2f(afniModel->alphas, afniModel->combinations);
      free(afniModel->svm_c);
      free(afniModel->eps);
      free(afniModel->biased_hyperplane);
      free(afniModel->skip_final_opt_check);
      free(afniModel->svm_maxqpsize);
      free(afniModel->svm_newvarsinqp);
      free(afniModel->svm_iter_to_shrink);
      free(afniModel->transduction_posratio);
      RETURN(1);
    }
    for (i=0; i < afniModel->combinations; ++i ) {
      afniModel->svm_costratio[i] = atr_float->fl[i];
    }

    atr_float = THD_find_float_atr( dsetModel->dblk, "3DSVM_SVM_COSTRATIO_UNLAB" );
    if( (afniModel->svm_costratio_unlab = (float *)malloc( atr_float->nfl * sizeof(float))) == NULL ) {
      snprintf(errorString, LONG_STRING, "readAllocateAfniModel: "
          "Memory allocation for svm_costratio_unlab failed!");

      /* free and return */
      free(p);
      free2c(afniModel->combName, max_comb);
      free2c(afniModel->kernel_custom, max_comb);
      free(afniModel->kernel_type);
      free(afniModel->rbf_gamma);
      free(afniModel->linear_coefficient);
      free(afniModel->constant_coefficient);
      free(afniModel->total_masked_features);
      free(afniModel->total_samples); 
      free(afniModel->total_support_vectors); 
      free(afniModel->b);
      free(afniModel->polynomial_degree);
      free2f(afniModel->alphas, afniModel->combinations);
      free(afniModel->svm_c);
      free(afniModel->eps);
      free(afniModel->biased_hyperplane);
      free(afniModel->skip_final_opt_check);
      free(afniModel->svm_maxqpsize);
      free(afniModel->svm_newvarsinqp);
      free(afniModel->svm_iter_to_shrink);
      free(afniModel->transduction_posratio);
      free(afniModel->svm_costratio);
      RETURN(1);
    }
    for (i=0; i < afniModel->combinations; ++i ) {
      afniModel->svm_costratio_unlab[i] = atr_float->fl[i];
    }

    atr_float = THD_find_float_atr( dsetModel->dblk, "3DSVM_SVM_UNLABBOUND" );
    if( (afniModel->svm_unlabbound = (float *)malloc( atr_float->nfl * sizeof(float))) == NULL ) {
      snprintf(errorString, LONG_STRING, "readAllocateAfniModel: "
          "Memory allocation for svm_unlabbound failed!");

      /* free and return */
      free(p);
      free2c(afniModel->combName, max_comb);
      free2c(afniModel->kernel_custom, max_comb);
      free(afniModel->kernel_type);
      free(afniModel->rbf_gamma);
      free(afniModel->linear_coefficient);
      free(afniModel->constant_coefficient);
      free(afniModel->total_masked_features);
      free(afniModel->total_samples); 
      free(afniModel->total_support_vectors); 
      free(afniModel->b);
      free(afniModel->polynomial_degree);
      free2f(afniModel->alphas, afniModel->combinations);
      free(afniModel->svm_c);
      free(afniModel->eps);
      free(afniModel->biased_hyperplane);
      free(afniModel->skip_final_opt_check);
      free(afniModel->svm_maxqpsize);
      free(afniModel->svm_newvarsinqp);
      free(afniModel->svm_iter_to_shrink);
      free(afniModel->transduction_posratio);
      free(afniModel->svm_costratio);
      free(afniModel->svm_costratio_unlab);
      RETURN(1);
    }
    for (i=0; i < afniModel->combinations; ++i ) {
      afniModel->svm_unlabbound[i] = atr_float->fl[i];
    }

    atr_float = THD_find_float_atr( dsetModel->dblk, "3DSVM_EPSILON_A" );
    if( (afniModel->epsilon_a = (float *)malloc( atr_float->nfl * sizeof(float))) == NULL ) {
      snprintf(errorString, LONG_STRING, "readAllocateAfniModel: "
          "Memory allocation for epsilon_a failed!");

      /* free and return */
      free(p);
      free2c(afniModel->combName, max_comb);
      free2c(afniModel->kernel_custom, max_comb);
      free(afniModel->kernel_type);
      free(afniModel->rbf_gamma);
      free(afniModel->linear_coefficient);
      free(afniModel->constant_coefficient);
      free(afniModel->total_masked_features);
      free(afniModel->total_samples); 
      free(afniModel->total_support_vectors); 
      free(afniModel->b);
      free(afniModel->polynomial_degree);
      free2f(afniModel->alphas, afniModel->combinations);
      free(afniModel->svm_c);
      free(afniModel->eps);
      free(afniModel->biased_hyperplane);
      free(afniModel->skip_final_opt_check);
      free(afniModel->svm_maxqpsize);
      free(afniModel->svm_newvarsinqp);
      free(afniModel->svm_iter_to_shrink);
      free(afniModel->transduction_posratio);
      free(afniModel->svm_costratio);
      free(afniModel->svm_costratio_unlab);
      RETURN(1);
    }
    for (i=0; i < afniModel->combinations; ++i ) {
      afniModel->epsilon_a[i] = atr_float->fl[i];
    }

    atr_float = THD_find_float_atr( dsetModel->dblk, "3DSVM_EPSILON_CRIT" );
    if( (afniModel->epsilon_crit = (float *)malloc( atr_float->nfl * sizeof(float))) == NULL ) {
      snprintf(errorString, LONG_STRING, "readAllocateAfniModel: "
          "Memory allocation for epsilon_crit failed!");

      /* free and return */
      free(p);
      free2c(afniModel->combName, max_comb);
      free2c(afniModel->kernel_custom, max_comb);
      free(afniModel->kernel_type);
      free(afniModel->rbf_gamma);
      free(afniModel->linear_coefficient);
      free(afniModel->constant_coefficient);
      free(afniModel->total_masked_features);
      free(afniModel->total_samples); 
      free(afniModel->total_support_vectors); 
      free(afniModel->b);
      free(afniModel->polynomial_degree);
      free2f(afniModel->alphas, afniModel->combinations);
      free(afniModel->svm_c);
      free(afniModel->eps);
      free(afniModel->biased_hyperplane);
      free(afniModel->skip_final_opt_check);
      free(afniModel->svm_maxqpsize);
      free(afniModel->svm_newvarsinqp);
      free(afniModel->svm_iter_to_shrink);
      free(afniModel->transduction_posratio);
      free(afniModel->svm_costratio);
      free(afniModel->svm_costratio_unlab);
      free(afniModel->epsilon_a);
      RETURN(1);
    }
    for (i=0; i < afniModel->combinations; ++i ) {
      afniModel->epsilon_crit[i] = atr_float->fl[i];
    }

    atr_int = THD_find_int_atr( dsetModel->dblk, "3DSVM_COMPUTE_LOO" );
    if( (afniModel->compute_loo = (int *)malloc( atr_int->nin * sizeof(int))) == NULL ) {
      snprintf(errorString, LONG_STRING, "readAllocateAfniModel: "
          "Memory allocation for compute_loo failed!");

      /* free and return */
      free(p);
      free2c(afniModel->combName, max_comb);
      free2c(afniModel->kernel_custom, max_comb);
      free(afniModel->kernel_type);
      free(afniModel->rbf_gamma);
      free(afniModel->linear_coefficient);
      free(afniModel->constant_coefficient);
      free(afniModel->total_masked_features);
      free(afniModel->total_samples); 
      free(afniModel->total_support_vectors); 
      free(afniModel->b);
      free(afniModel->polynomial_degree);
      free2f(afniModel->alphas, afniModel->combinations);
      free(afniModel->svm_c);
      free(afniModel->eps);
      free(afniModel->biased_hyperplane);
      free(afniModel->skip_final_opt_check);
      free(afniModel->svm_maxqpsize);
      free(afniModel->svm_newvarsinqp);
      free(afniModel->svm_iter_to_shrink);
      free(afniModel->transduction_posratio);
      free(afniModel->svm_costratio);
      free(afniModel->svm_costratio_unlab);
      free(afniModel->epsilon_a);
      free(afniModel->epsilon_crit);
      RETURN(1);
    }
    for( i=0; i<afniModel->combinations; ++i ) {
      afniModel->compute_loo[i] = atr_int->in[i];
    }

    atr_float = THD_find_float_atr( dsetModel->dblk, "3DSVM_RHO" );
    if( (afniModel->rho = (float *)malloc( atr_float->nfl * sizeof(float))) == NULL ) {
      snprintf(errorString, LONG_STRING, "readAllocateAfniModel: "
          "Memory allocation for rho!");

      /* free and return */
      free(p);
      free2c(afniModel->combName, max_comb);
      free2c(afniModel->kernel_custom, max_comb);
      free(afniModel->kernel_type);
      free(afniModel->rbf_gamma);
      free(afniModel->linear_coefficient);
      free(afniModel->constant_coefficient);
      free(afniModel->total_masked_features);
      free(afniModel->total_samples); 
      free(afniModel->total_support_vectors); 
      free(afniModel->b);
      free(afniModel->polynomial_degree);
      free2f(afniModel->alphas, afniModel->combinations);
      free(afniModel->svm_c);
      free(afniModel->eps);
      free(afniModel->biased_hyperplane);
      free(afniModel->skip_final_opt_check);
      free(afniModel->svm_maxqpsize);
      free(afniModel->svm_newvarsinqp);
      free(afniModel->svm_iter_to_shrink);
      free(afniModel->transduction_posratio);
      free(afniModel->svm_costratio);
      free(afniModel->svm_costratio_unlab);
      free(afniModel->epsilon_a);
      free(afniModel->epsilon_crit);
      free(afniModel->compute_loo);
      RETURN(1);
    }
    for (i=0; i < afniModel->combinations; ++i ) {
      afniModel->rho[i] = atr_float->fl[i];
    }

    atr_int = THD_find_int_atr( dsetModel->dblk, "3DSVM_XA_DEPTH" );
    if( (afniModel->xa_depth = (int *)malloc( atr_int->nin * sizeof(int))) == NULL ) {
      snprintf(errorString, LONG_STRING, "readAllocateAfniModel: "
          "Memory allocation for xa_depth failed!");

      /* free and return */
      free(p);
      free2c(afniModel->combName, max_comb);
      free2c(afniModel->kernel_custom, max_comb);
      free(afniModel->kernel_type);
      free(afniModel->rbf_gamma);
      free(afniModel->linear_coefficient);
      free(afniModel->constant_coefficient);
      free(afniModel->total_masked_features);
      free(afniModel->total_samples); 
      free(afniModel->total_support_vectors); 
      free(afniModel->b);
      free(afniModel->polynomial_degree);
      free2f(afniModel->alphas, afniModel->combinations);
      free(afniModel->svm_c);
      free(afniModel->eps);
      free(afniModel->biased_hyperplane);
      free(afniModel->skip_final_opt_check);
      free(afniModel->svm_maxqpsize);
      free(afniModel->svm_newvarsinqp);
      free(afniModel->svm_iter_to_shrink);
      free(afniModel->transduction_posratio);
      free(afniModel->svm_costratio);
      free(afniModel->svm_costratio_unlab);
      free(afniModel->epsilon_a);
      free(afniModel->epsilon_crit);
      free(afniModel->compute_loo);
      free(afniModel->rho);
      RETURN(1);
    }
    for( i=0; i<afniModel->combinations; ++i ) {
      afniModel->xa_depth[i] = atr_int->in[i];
    }
  }

  /* -----------------------------------------------------*/
  /* ---- naming for model parameters before Oct. 2009 ---*/
  /* -----------------------------------------------------*/
  else if (afniModel->version >= 0.80) {

    atr_int = THD_find_int_atr( dsetModel->dblk, "CLASS_COMBINATIONS" );
    afniModel->combinations = *atr_int->in;

    /* --- allocate CSV strings --- */
    p_string_size = afniModel->combinations*CSV_STRING;
    if ( (p = (char *)malloc(p_string_size*sizeof(char))) == NULL ) {
      snprintf(errorString, LONG_STRING, "readAllocateAfniModel: "
          "Memory allocation for cvs-string failed!");
      RETURN(1);
    }

    /* - allocate 2D char arrays - */
    if( (afniModel->combName = Allocate2c(max_comb, (long)CSV_STRING)) == NULL ) {
      snprintf(errorString, LONG_STRING, "readAllocateAfniModel: "
          "Memory allocation for combName failed!");

      /* free and return */
      free(p);
      RETURN(1);
    }
    Clear2c(afniModel->combName, max_comb);

    if( (afniModel->kernel_custom = Allocate2c(max_comb, (long)CSV_STRING)) == NULL ) {
      snprintf(errorString, LONG_STRING, "readAllocateAfniModel: "
          "Memory allocation for kernel_custom failed!");

      /* free and return */
      free(p);
      free2c(afniModel->combName, max_comb);
      RETURN(1);
    }
    Clear2c(afniModel->kernel_custom, max_comb);

    /* -- write default values for non-existing header entries */
    strncpy(afniModel->svm_type, "classification", LONG_STRING);
    afniModel->mask_used = MASK_UNKNOWN;

    /* -- allocate and read header entries -- */
    atr_string = THD_find_string_atr( dsetModel->dblk, "COMBO_NAMES" );
    strncpy(p, atr_string->ch, p_string_size);
    q = strtok(p,",");
    if (q != NULL) strncpy(afniModel->combName[0], q, CSV_STRING);
    else {
      snprintf(errorString, LONG_STRING, 
          "Reading model combinations in header file failed");

      /*  free and return  */
      free(p);
      free2c(afniModel->combName, max_comb);
      free2c(afniModel->kernel_custom, max_comb);
      RETURN(1);
    }
    for(i = 1; i < afniModel->combinations; ++i) {
      q=strtok(NULL, ",");
      if (q != NULL) strncpy(afniModel->combName[i], q, CSV_STRING);
      else {
        snprintf(errorString, LONG_STRING, 
            "Reading model combinations in header file failed "
            "Number does not match expected(%d)", afniModel->combinations);
     
        /*  free and return  */
        free(p);
        free2c(afniModel->combName, max_comb);
        free2c(afniModel->kernel_custom, max_comb);
        RETURN(1);
      }
    }

    atr_int = THD_find_int_atr( dsetModel->dblk, "CLASS_COUNT" );
    afniModel->class_count = *atr_int->in;

    atr_int = THD_find_int_atr( dsetModel->dblk, "TIMEPOINTS" );
    afniModel->timepoints = *atr_int->in;

    atr_int = THD_find_int_atr( dsetModel->dblk, "KERNEL_TYPE" );
    if( (afniModel->kernel_type = (int *)malloc( atr_int->nin * sizeof(int))) == NULL ) { 
      snprintf(errorString, LONG_STRING, "readAllocateAfniModel: "
          "Memory allocation for kernel_type failed!"); 

        /*  free and return  */
        free(p);
        free2c(afniModel->combName, max_comb);
        free2c(afniModel->kernel_custom, max_comb);
        RETURN(1);
    }

    for( i=0 ; i<atr_int->nin ; ++i ) {
      afniModel->kernel_type[i] = atr_int->in[i];
    }
  
    /* JL Feb. 2009: Added this part to support custom kernels.
     * To be backward compatible, read KERNEL_CUSTOM only if training was
     * performed with a custom kernel. */

    if (afniModel->kernel_type[0] == CUSTOM) {
        atr_string = THD_find_string_atr( dsetModel->dblk, "KERNEL_CUSTOM" );
        strncpy(p,atr_string->ch, p_string_size);
        q = strtok(p,",");
        if (q != NULL) strncpy(afniModel->kernel_custom[0], q, CSV_STRING);
        else {
          snprintf(errorString, LONG_STRING, 
              "Reading model file failed. Can't find KERNEL_CUSTOM");

          /* free and return */
          free(p);
          free2c(afniModel->combName, max_comb);
          free2c(afniModel->kernel_custom, max_comb); 
          free(afniModel->kernel_type);
          RETURN(1);
        }

        for ( i=1; i<afniModel->combinations; ++i) {
          q=strtok(NULL,",");
          if (q != NULL) strncpy(afniModel->kernel_custom[i], q, p_string_size);
          else {
            snprintf(errorString, LONG_STRING, "Reading KERNEL_CUSTOM in model "
                "header. Number of class-combinations does not match expected(%d)\n",
                afniModel->combinations);

            /* free and return */
            free(p);
            free2c(afniModel->combName, max_comb);
            free2c(afniModel->kernel_custom, max_comb); 
            free(afniModel->kernel_type);
            RETURN(1);
          }
        }
      }
      else {
        for ( i=1; i<afniModel->combinations; ++i) {
          strncpy(afniModel->kernel_custom[i], "empty", CSV_STRING);
        }
    }

    atr_float = THD_find_float_atr( dsetModel->dblk, "RBF_GAMMA" );
    if( (afniModel->rbf_gamma = (float *)malloc( atr_float->nfl * sizeof(float) )) == NULL ) { 
      snprintf(errorString, LONG_STRING, "readAllocateAfniModel: "
          "Memory allocation for rbf_gamma failed!");
      
      /* free and return */
      free(p);
      free2c(afniModel->combName, max_comb);
      free2c(afniModel->kernel_custom, max_comb);
      free(afniModel->kernel_type);
      RETURN(1);
    }
    for( i=0 ; i<atr_float->nfl ; ++i ) {
      afniModel->rbf_gamma[i] = atr_float->fl[i];
    }
  
    atr_float = THD_find_float_atr( dsetModel->dblk, "LINEAR_COEFFICIENT" );
    if( (afniModel->linear_coefficient = (float *)malloc( atr_float->nfl * sizeof(float) )) == NULL ) { 
      
      snprintf(errorString, LONG_STRING, "readAllocateAfniModel: "
          "Memory allocation for linear_coefficient failed!"); 
      
      /* free and return */
      free(p);
      free2c(afniModel->combName, max_comb);
      free2c(afniModel->kernel_custom, max_comb);
      free(afniModel->kernel_type);
      free(afniModel->rbf_gamma);
      RETURN(1);
    }
    for( i=0 ; i<atr_float->nfl ; ++i ) {
      afniModel->linear_coefficient[i] = atr_float->fl[i];
    }
  
    atr_float = THD_find_float_atr( dsetModel->dblk, "CONSTANT_COEFFICIENT" );
    if( (afniModel->constant_coefficient = (float *)malloc( atr_float->nfl * sizeof(float) )) == NULL ) {
      snprintf(errorString, LONG_STRING, "readAllocateAfniModel: "
          "Memory allocation for constant_coefficient failed!");

      /* free and return */
      free(p);
      free2c(afniModel->combName, max_comb);
      free2c(afniModel->kernel_custom, max_comb);
      free(afniModel->kernel_type);
      free(afniModel->rbf_gamma);
      free(afniModel->linear_coefficient);
      RETURN(1);
    }
    for( i=0 ; i<atr_float->nfl ; ++i ) {
      afniModel->constant_coefficient[i] = atr_float->fl[i];
    }
  
    atr_int = THD_find_int_atr( dsetModel->dblk, "TOTAL_MASKED_FEATURES" );
    if( (afniModel->total_masked_features = (int *)malloc( atr_int->nin * sizeof(int) )) == NULL ) {
      snprintf(errorString, LONG_STRING, "readAllocateAfniModel: "
          "Memory allocation for total_masked_features failed!");

      /* -- free and return -- */
      free(p);
      free2c(afniModel->combName, max_comb);
      free2c(afniModel->kernel_custom, max_comb);
      free(afniModel->kernel_type);
      free(afniModel->rbf_gamma);
      free(afniModel->linear_coefficient);
      free(afniModel->constant_coefficient);
      RETURN(1);
    }
    for( i=0 ; i<atr_int->nin ; ++i ) {
      afniModel->total_masked_features[i] = atr_int->in[i];
    }
  
    atr_int = THD_find_int_atr( dsetModel->dblk, "TOTAL_SAMPLES" );
    if( (afniModel->total_samples = (int *)malloc( atr_int->nin * sizeof(int) )) == NULL ) {
      snprintf(errorString, LONG_STRING, "readAllocateAfniModel: "
          "Memory allocation for total_samples failed!");

      /* -- free and return -- */
      free(p);
      free2c(afniModel->combName, max_comb);
      free2c(afniModel->kernel_custom, max_comb);
      free(afniModel->kernel_type);
      free(afniModel->rbf_gamma);
      free(afniModel->linear_coefficient);
      free(afniModel->constant_coefficient);
      free(afniModel->total_masked_features);
      RETURN(1);
    }
    for( i=0 ; i<atr_int->nin ; ++i ) {
      afniModel->total_samples[i] = atr_int->in[i];
    }
  
    atr_int = THD_find_int_atr( dsetModel->dblk, "TOTAL_SUPPORT_VECTORS" );
    if( (afniModel->total_support_vectors = (int *)malloc( atr_int->nin * sizeof(int) )) == NULL ) {
      snprintf(errorString, LONG_STRING, "readAllocateAfniModel: "
          "Memory allocation for total_support_vectors failed!");

      /* -- free and return -- */
      free(p);
      free2c(afniModel->combName, max_comb);
      free2c(afniModel->kernel_custom, max_comb);
      free(afniModel->kernel_type);
      free(afniModel->rbf_gamma);
      free(afniModel->linear_coefficient);
      free(afniModel->constant_coefficient);
      free(afniModel->total_masked_features);
      free(afniModel->total_samples);
      RETURN(1);
    }
    for( i=0 ; i<atr_int->nin ; ++i ) {
      afniModel->total_support_vectors[i] = atr_int->in[i];
    }
  
    atr_float = THD_find_float_atr( dsetModel->dblk, "B" );
    if( (afniModel->b = (float *)malloc( atr_float->nfl * sizeof(float) )) == NULL ) {
      snprintf(errorString, LONG_STRING, "readAllocateAfniModel: "
          "Memory allocation for b failed!");

      /* -- free and return -- */
      free(p);
      free2c(afniModel->combName, max_comb);
      free2c(afniModel->kernel_custom, max_comb);
      free(afniModel->kernel_type);
      free(afniModel->rbf_gamma);
      free(afniModel->linear_coefficient);
      free(afniModel->constant_coefficient);
      free(afniModel->total_masked_features);
      free(afniModel->total_samples);
      free(afniModel->total_support_vectors);
      RETURN(1);
    }
    for( i=0 ; i<atr_float->nfl ; ++i ) {
      afniModel->b[i] = atr_float->fl[i];
    }
  
    atr_int = THD_find_int_atr( dsetModel->dblk, "POLYNOMIAL_DEGREE" );
    if( (afniModel->polynomial_degree = (int *)malloc( atr_int->nin * sizeof(int) )) == NULL ) {
      snprintf(errorString, LONG_STRING, "readAllocateAfniModel: "
          "Memory allocation for polynomial_degree failed!");

      /* -- free and return -- */
      free(p);
      free2c(afniModel->combName, max_comb);
      free2c(afniModel->kernel_custom, max_comb);
      free(afniModel->kernel_type);
      free(afniModel->rbf_gamma);
      free(afniModel->linear_coefficient);
      free(afniModel->constant_coefficient);
      free(afniModel->total_masked_features);
      free(afniModel->total_samples);
      free(afniModel->total_support_vectors);
      free(afniModel->b);
      RETURN(1);
    }
    for( i=0 ; i<atr_int->nin ; ++i ) {
      afniModel->polynomial_degree[i] = atr_int->in[i];
    }

    if( (afniModel->alphas = Allocate2f((long) afniModel->combinations, 
            nalphas)) == NULL ) {

      snprintf(errorString, LONG_STRING, "readAllocateAfniModel: "
          "Memory allocation for alphas failed!");

      /* -- free and return -- */
      free(p);
      free2c(afniModel->combName, max_comb);
      free2c(afniModel->kernel_custom, max_comb);
      free(afniModel->kernel_type);
      free(afniModel->rbf_gamma);
      free(afniModel->linear_coefficient);
      free(afniModel->constant_coefficient);
      free(afniModel->total_masked_features);
      free(afniModel->total_samples);
      free(afniModel->total_support_vectors);
      free(afniModel->b);
      free(afniModel->polynomial_degree);
      RETURN(1);
    }
     
    for(i = 0; i < afniModel->combinations; ++i ) {
      snprintf(headernames, LONG_STRING, "ALPHAS_%s", afniModel->combName[i]);
      atr_float = THD_find_float_atr( dsetModel->dblk, headernames); 
      for(j = 0; j < nalphas; ++j ) {
        afniModel->alphas[i][j] = (double)atr_float->fl[j];
      }
    }
  }
  else {
    snprintf(errorString, LONG_STRING, 
          "Could not read model header. Version V%3.2f unknown!", afniModel->version);
    /* -- free and return -- */
    free(p);
    free2c(afniModel->combName, max_comb);
    free2c(afniModel->kernel_custom, max_comb);
    free(afniModel->kernel_type);
    free(afniModel->rbf_gamma);
    free(afniModel->linear_coefficient);
    free(afniModel->constant_coefficient);
    free(afniModel->total_masked_features);
    free(afniModel->total_samples);
    free(afniModel->total_support_vectors);
    free(afniModel->b);
    free(afniModel->polynomial_degree);
    free2f(afniModel->alphas, afniModel->combinations);
    RETURN(1);
  }
    
  /* --- free p string used for strtok ---*/
  free(p);

  RETURN(0);
}


int allocateModelMaps(MODEL_MAPS *maps, long n_classes, long n_vox, char *kernelName)
{
  long class_comb = 0;

  ENTRY("allocateModelMaps");

  /* --- initialize --- */
  class_comb = n_classes*(n_classes-1)/2;
  maps->index=0;

  if ( (!strcmp(kernelName, "complex1")) ) {
    
    /* Remember the real- and imaginary part are concatenated for kernel-
     * option complex1. I'm probably redundant here... */
    if (n_vox%2 != 0) {
      WARNING_message("Can not create model maps. " 
          "Something is wrong with the complex-valued data representation");
     
      /* even if maps can not be generated, allocated some small chuck of memory.
         This makes freeing memory a bit easier and doesn't cost a lot of 
         resources. */
      maps->nmaps = 1;
      maps->nvox = 1;
    }
    else {
      maps->nvox = n_vox/2;

      /* Generating six weight-vector maps for linear-complex kernels:
       * RE, IM, MAG1, MAG2, PHA1, PHA2 */
       maps->nmaps = class_comb*6;
    }
  }
  else if ( (!strcmp(kernelName, "linear")) ) {
    maps->nmaps = class_comb;
    maps->nvox = n_vox;
  }
  else {
    WARNING_message("Sorry, Can not create model maps for kernel option: %s", kernelName);
    /* even if maps can not be generated, allocate some small chuck of memory.
       This makes freeing memory a bit easier and doesn't cost a lot of 
       resources. */
    maps->nmaps = 1;
    maps->nvox = 1;
  }

  /* --- allocate memory ---*/
  if( (maps->names = Allocate2c(maps->nmaps, (long)LONG_STRING)) == NULL ) {
    RETURN(1);
  }
  if( (maps->data = Allocate2d(maps->nmaps, maps->nvox)) == NULL ) {
    free2c(maps->names, maps->nmaps);
    RETURN(1);
  }

  /* --- null allocated memory --- */
  Clear2c(maps->names, maps->nmaps);
  Clear2d(maps->data, maps->nmaps, maps->nvox);

  RETURN(0);
}

void freeModelMaps(MODEL_MAPS *maps)
{
  ENTRY("freeModelMaps");

  free2d(maps->data, maps->nmaps);
  free2c(maps->names, maps->nmaps);


  EXRETURN;
}

void addToModelMap_bucket ( MODEL_MAPS *maps, AFNI_MODEL *afni_model,
  DatasetType **dsetTrainArray, MaskType *dsetMaskArray, char *maskFile,
  long cc)
{
  long v      = 0;
  long iMap   = 0;
  long nvoxh  = 0;
  long t      = 0; 
  long k      = 0;
  long nk     = 0;
  long nt     = 0;

  ENTRY("addToModelMap_bucket");

  /* Note: This function adds one ore more maps to the MODEL_MAPS structure for
   * each class-combination (cc). In order to keep track of how many maps were 
   * written, *map_index is updated for each function call. For now, only 
   * weight-vector maps for linear and linear-complex kernels are calculated.
   * The kernel has to be the same for each class-combination!
   *
   * JL Aug 2009: Added regression maps
   * JL Apr 2010: Incorporated map_index into the MODEL_MAPS structure
   * JL Jul 2015: Check for non-zero alphas more rigorously 
   * JL JUL 2015: Got rid of cAlphas (censored alphas). Unnecessary. 
   */

  /* --- initialization ---*/
  iMap=maps->index; /* TODO: prone for errors, should do something better than that */


  if( !strcmp(afni_model->svm_type, "regression") ) {
    /* For regression, the array storing the alphas is twice as long 
     * as for classification. */
    nk = 2;
  }
  else nk = 1;

  nt = afni_model->timepoints;

  /*  -- linear kernel -- */
  if(afni_model->kernel_type[cc] == LINEAR) {    
    for (k=0; k<nk; ++k) {
      for (t=0; t<nt; ++t) {
        if ( fabs(afni_model->alphas[cc][k*nt+t]) > 0.0 ) {
          for (v=0; v<maps->nvox; ++v) {
            if ( maskFile[0] ) { /* mask */
              if ( dsetMaskArray[v] ) {
                maps->data[iMap][v] += afni_model->alphas[cc][k*nt+t] * 
                                          dsetTrainArray[t ][v];
              }
              else { 
                maps->data[iMap][v] = 0;
              } 
            }
            else { /* no mask */
              maps->data[iMap][v] += afni_model->alphas[cc][k*nt+t] * 
                                          dsetTrainArray[t ][v];
            }
          }
        }
      }
    }
    snprintf(maps->names[iMap], LONG_STRING, "w_%s", afni_model->combName[cc]);
    ++iMap;
  } 
  /* -- complex-linear kernel -- */
  /* JL: Experimental stuff for Scott Peltier */
  else if( (afni_model->kernel_type[cc] == CUSTOM) && 
      (!strcmp(afni_model->kernel_custom[cc],"complex1")) ) {

    nvoxh = maps->nvox;
  
    for (k=0; k<nk; ++k) {
      for (t=0; t<nt; ++t) {
        if ( fabs(afni_model->alphas[cc][k*nt+t]) > 0.0 ) {
          for (v=0; v<maps->nvox; ++v) {
            if ( maskFile[0] ) { /* mask */
              if ( dsetMaskArray[v] ) {

                /*   - RE -   */
                maps->data[iMap  ][v] += afni_model->alphas[cc][k*nt+t] *
                                              dsetTrainArray[t ][v     ];
                /*   - IM -   */
                maps->data[iMap+1][v] += afni_model->alphas[cc][k*nt+t] *
                                              dsetTrainArray[t ][v+nvoxh];
                /*  - MAG1 - */
                maps->data[iMap+2][v] += afni_model->alphas[cc][k*nt+t] *
                  sqrt( dsetTrainArray[t ][v      ] * dsetTrainArray[t][v      ] +
                        dsetTrainArray[t ][v+nvoxh] * dsetTrainArray[t][v+nvoxh]);
             
                /*  - PHA1 - */
                maps->data[iMap+3][v] += 10e5 *afni_model->alphas[cc][k*nt+t ] *
                  atan2(dsetTrainArray[t ][v+nvoxh], dsetTrainArray[t ][v     ]);
              
              }
              else { 
                maps->data[iMap  ][v] = 0;
                maps->data[iMap+1][v] = 0;
                maps->data[iMap+2][v] = 0;
                maps->data[iMap+3][v] = 0;

              } 
            }
            else { /* no mask */
            
              /*  - RE - */
              maps->data[iMap  ][v] += afni_model->alphas[cc][k*nt+t] *
                                            dsetTrainArray[t ][v   ];
              /*  - IM - */
              maps->data[iMap+1][v] += afni_model->alphas[cc][k*nt+t] *
                                            dsetTrainArray[t ][v+nvoxh];
              /*  - MAG1 - */
              maps->data[iMap+2][v] += afni_model->alphas[cc][k*nt+t] *
                  sqrt( dsetTrainArray[t ][v      ] * dsetTrainArray[t ][v      ] +
                        dsetTrainArray[t ][v+nvoxh] * dsetTrainArray[t ][v+nvoxh]);
              /*  - PHA1 - */
              maps->data[iMap+3][v] += afni_model->alphas[cc][k*nt+t] *
                  atan2(dsetTrainArray[t ][v+nvoxh], dsetTrainArray[t ][v     ]);

            } 
          }
        }
      }
    }
    snprintf(maps->names[iMap  ], LONG_STRING, "CpxWvMapReal_%s", afni_model->combName[cc]);
    snprintf(maps->names[iMap+1], LONG_STRING, "CpxWvMapImag_%s", afni_model->combName[cc]);
    snprintf(maps->names[iMap+2], LONG_STRING, "CpxWvMapMag1_%s", afni_model->combName[cc]);
    snprintf(maps->names[iMap+3], LONG_STRING, "CpxWvMapPha1_%s", afni_model->combName[cc]);

    for (v=0; v<maps->nvox; ++v) {
      if ( maskFile[0] ) { /* mask */
        if ( dsetMaskArray[v] ) {
          /*   - MAG2 - */
          maps->data[iMap+4][v] = sqrt( maps->data[iMap  ][v] * 
                                        maps->data[iMap  ][v] +
                                        maps->data[iMap+1][v] * 
                                        maps->data[iMap+1][v] );  
          /*  - PHA2 - */
          maps->data[iMap+5][v] = atan2( maps->data[iMap+1][v] ,
                                       maps->data[iMap  ][v] );
        }
        else { 
          maps->data[iMap+4][v] = 0;
          maps->data[iMap+5][v] = 0;
        } 
      }
      else { /* no mask */
        /*   - MAG2 - */
        maps->data[iMap+4][v] = sqrt( maps->data[iMap  ][v] * 
                                      maps->data[iMap  ][v] +
                                      maps->data[iMap+1][v] * 
                                      maps->data[iMap+1][v] );  
        /*  - PHA2 - */
        maps->data[iMap+5][v] = atan2( maps->data[iMap+1][v] ,
                                     maps->data[iMap  ][v] );
      }
    }
    snprintf(maps->names[iMap+4], LONG_STRING, "CpxWvMapMag2_%s", afni_model->combName[cc]);
    snprintf(maps->names[iMap+5], LONG_STRING, "CpxWvMapPha2_%s", afni_model->combName[cc]);
    iMap=iMap+6;
  }
  
  maps->index=iMap;

  EXRETURN;
}

int writeModelMap_bucket( MODEL_MAPS *maps, MaskType *dsetMaskArray, 
    THD_3dim_dataset *dsetTrain,  char *maskFile, char *fileName, 
    float *b, long combinations, ASLoptions* options, int argc, char **argv,
    char *errorString )
{
  long v     = 0;
  long iMap  = 0;
  long nx    = 0;
  long ny    = 0;
  long nz    = 0;
  long nx_ny = 0;

  THD_ivec3 iv_nxyz; 
  int ierror  = 0;

  THD_3dim_dataset*  dsetModelMapBucket = NULL;
  float*             scaled_map         = NULL;

  char* commandline  = NULL;     /* for history */

  ENTRY("writeModelMap_bucket");

  /* Changes:
   * JL Apr. 2010: Writing VERSION_3DSVM and VERSION_3DSVM into the header
   * JL Apr. 2010: Writing B value into the header
   * JL Jul. 2010: Writing entire command line history into the header
   * JL July 2011: Modified error handling: Replaced ERROR_exit() by RETURN(1).
   *               Passing error message as argument (errorString) to the 
   *               calling function.
   *
   */


  /* --- initialize --- */
  dsetModelMapBucket = EDIT_empty_copy( dsetTrain );

  nx = DSET_NX( dsetTrain );
  ny = DSET_NY( dsetTrain );
  nx_ny = nx*ny;

  if ( !strcmp(options->kernelName, "complex1") ) {     
    /* JL: For complex kernel RE and IM is concatenated in the z-direction.
     * However, we are not concatenating data for the bucket (going back to 
     * nz/2) */

    if ( maps->nvox%nx_ny != 0 ) {
      snprintf(errorString, LONG_STRING, "Writing bucket with model maps failed! "
          "Something is wrong with the complex-valued data representation.");
      RETURN(1);
      }
    nz = maps->nvox/(nx*ny);
  }
  else {
    nz = DSET_NZ( dsetTrain );
  }

  LOAD_IVEC3( iv_nxyz, nx ,ny ,nz);
  ierror = EDIT_dset_items ( dsetModelMapBucket,
                            ADN_prefix,          fileName,
                            ADN_type,            HEAD_FUNC_TYPE,
                            ADN_func_type,       FUNC_BUCK_TYPE,
                            ADN_datum_all,       MRI_float,
                            ADN_ntt,             0,   /* no time axis */
                            ADN_nvals,           maps->nmaps,
                            ADN_nxyz,            iv_nxyz, 
                            ADN_malloc_type,     DATABLOCK_MEM_MALLOC ,
                            ADN_none ) ;

  if( ierror > 0 ) {
    snprintf(errorString, LONG_STRING, "writeModelMap_bucket: " 
        "%d errors in attempting to create bucket dataset!", ierror );
    RETURN(1);
  }

  /* -- record history -- */
  commandline = tross_commandline(PROGRAM_NAME, argc, argv);
  if (commandline == NULL) {
    WARNING_message("Can not copy command-line into bucket header!");
  }
  else tross_Append_History (dsetModelMapBucket, commandline);
  free(commandline);

 /* --- scale and write maps into bucket --- */
  for (iMap=0; iMap<maps->nmaps; ++iMap) {
    
    /* -- allocate scaled_map  -- */
    if( (scaled_map = (float *) malloc(sizeof(float)*maps->nvox)) == NULL ) {
      snprintf(errorString, LONG_STRING, "writeModelMap_bucket: "
          "Memory allocation failed!"); 
      
      /* free and return */
      DSET_unload(dsetModelMapBucket); 
      RETURN(1);
    }
    
    /*  -- scaling PHA--  */
    if ( !strncmp(maps->names[iMap], "CpxWvMapPha", LONG_STRING) ) {
      for (v=0; v<maps->nvox; ++v) { 
        if ( maskFile[0] ) {                   
          if ( dsetMaskArray[v] ) {
            scaled_map[v] = (float) (180.0/M_PI*maps->data[iMap][v]);
          }
          else {
            scaled_map[v] = 0;
          }
        }
        else { 
          scaled_map[v] = (float) (180.0/M_PI*maps->data[iMap][v]);
        }   
      }
    }
    else {
      /*  -- scaling RE, IM, MAG-- */
      for (v=0; v<maps->nvox; ++v) {
        if ( maskFile[0] ) {                   
          if ( dsetMaskArray[v] ) {
            scaled_map[v] = (float) (SCALE*maps->data[iMap][v]);
          }
          else {
            scaled_map[v] = 0;
          }
        }
        else { 
          scaled_map[v] = (float) (SCALE*maps->data[iMap][v]);
        }   
      }
    }

    /* -- add current map to bucket -- */      
    EDIT_substitute_brick( dsetModelMapBucket, iMap, MRI_float, scaled_map );
    EDIT_BRICK_LABEL( dsetModelMapBucket, iMap, maps->names[iMap] );
  }

  /* --- add information to the header --- */
  THD_set_string_atr( dsetModelMapBucket->dblk, "3DSVM_VERSION", VERSION_3DSVM );
  THD_set_string_atr( dsetModelMapBucket->dblk, "3DSVM_VERSION_DATE", VERSION_DATE_3DSVM );
  THD_set_float_atr( dsetModelMapBucket->dblk, "3DSVM_B", combinations, b );

  /* --- write entire bucket data set to disc --- */
  fflush(stdout);
  INFO_message("Writing bucket dataset: %s with %ld brick(s)...", fileName, maps->nmaps);
  THD_write_3dim_dataset( "./", fileName, dsetModelMapBucket, True );
 
  /* --- deallocate memory --- */
  free(scaled_map);
  
  RETURN(0);
}

/*-----------------------------------------------------------*/
void writeModelMask( THD_3dim_dataset *dsetMask, MaskType* dsetMaskArray, char *fileName) 
{
  char maskCopyName[LONG_STRING];
  THD_3dim_dataset* dsetMaskCopy;
  int ityp;

  ENTRY("writeModelMask");
  
  /* Write out model mask (actually, just a simple copy of mask used) */
  snprintf( maskCopyName, LONG_STRING, "%s%s", fileName, MODEL_MSK_EXT );
  dsetMaskCopy = EDIT_empty_copy(dsetMask);
  EDIT_dset_items( dsetMaskCopy,
        ADN_prefix, maskCopyName,
        ADN_label1, maskCopyName,
        ADN_type, 1,                    /* functional dataset */
        ADN_func_type, 0,               /* fim functional type */
        ADN_nvals, 1,
        ADN_ntt, 0,                     /* number of time points (?) */
        ADN_none );

  ityp = DSET_BRICK_TYPE( dsetMask, 0 );     /* ityp: 0=byte, 1=short, 2=float, 3=complex */

  EDIT_substitute_brick( dsetMaskCopy, 0, ityp, dsetMaskArray );

  tross_Copy_History( dsetMask, dsetMaskCopy );
  tross_Append_History( dsetMaskCopy, "a 3dsvm copy") ;

  fflush(stdout);
  INFO_message("Writing model dataset mask: %s...", fileName);
  THD_write_3dim_dataset( "./", maskCopyName, dsetMaskCopy, True );

  EXRETURN;
}

/*-----------------------------------------------------------*/
int writeModelBrik(AFNI_MODEL *afniModel, THD_3dim_dataset* dsetTrain,
    DatasetType** dsetTrainArray,MaskType* dsetMaskArray,
    ASLoptions* options, char* fileName, int argc, char **argv, char *errorString)
{

  THD_3dim_dataset *dsetModel = NULL;
  char* csv_combName          = NULL;  /* comma separated  "names" of class
                                        * category combinations */
  char* csv_kernelCustom      = NULL;

  char headernames[LONG_STRING];       /* comma separated "names" for each alpha
                                        *  set */
  long csv_string_size        = 0;     /* size of csv strings, dependent on number
                                          of class-combinations */
  char* commandline           = NULL;  /* for history */
  long i                      = 0;
  long nt                     = 0;
  long t                      = 0;
  long nvox                   = 0;
  long v                      = 0;
  long nalphas                = 0;
  int datum                   = 0;
  int maskUsed                = 0;
  int ierror                  = 0;
  short* tmp_dsetArrayShort   = NULL;
  float* tmp_dsetArrayFloat   = NULL;

  ENTRY("writeModelBrik");

  /* JL Oct. 2009: The naming and the number of parameters written into the
   * model header has changed. Now, we are writing all svm parameters
   * (that can be specified using the command-line) into the header. 
   * We also added  "3DSVM" in front of each parameter name to avoid 
   * collisions with header entries from other afni programs.
   * Trying to be backwards compatible.
   *
   * JL Apr. 2010: Changed allocation of strings holding comma separated values
   * to be dynamic. Replaced all string functions by its equivalent that also
   * takes the string size as an argument.
   *
   * JL May. 2010: Writing mask as a sub brick of the model. To overcome a
   * problem with EDIT_substitute_brick (or a header entry), the mask is written
   * in brick n+1 and n+2.
   *
   * JL Oct. 2010: Bugfix: .BRIK was not written for datum type float (casted
   * incorrectly)
   * 
   * JL July 2011: Modified error handling: Replaced ERROR_exit() by RETURN(1).
   * Passing error message as argument (errorString) to the calling function.
   *
   */

  dsetModel = EDIT_empty_copy (dsetTrain);

  nt = DSET_NUM_TIMES( dsetModel );
  nvox = DSET_NVOX( dsetModel );
  datum = DSET_BRICK_TYPE(dsetModel,0);

  ierror = EDIT_dset_items(dsetModel,
      ADN_prefix,       fileName,
      ADN_ntt,          nt+2,  /* two more timepoint to store mask */
      ADN_nvals,        nt+2,
      ADN_none);

  if( ierror > 0 ) {
    snprintf(errorString, LONG_STRING, "writeModelBrik: "
        "%d errors in attempting to create model dataset!", ierror );
    RETURN(1);
  }

  if( !strcmp(options->svmType, "regression") ) {
    nalphas = (long)afniModel->timepoints*2;
  }
  else if( !strcmp(options->svmType, "classification") ) {
    nalphas = (long)afniModel->timepoints;
  }
  else {
    /* should never get here */
    snprintf(errorString, LONG_STRING, "allocateAfniModel: SVM type unknown!");
    RETURN(1);
  }

  /* --- write mask and training data into model dataset --- */
  switch (datum) {
    case MRI_float:
      /* -- write train data into model dataset -- */
      for (t=0; t<nt; ++t) {
        if( (tmp_dsetArrayFloat = (float *)malloc(nvox * sizeof(float))) == NULL ) {
          snprintf(errorString, LONG_STRING, "writeModelBrik: "
          "Memory allocation failed!"); 

          /* free and return */
          DSET_unload(dsetModel); 
          RETURN(1);
        }
        for (v=0; v<nvox; ++v) tmp_dsetArrayFloat[v] = (float) dsetTrainArray[t][v];
        EDIT_substitute_brick(dsetModel, t, MRI_float, tmp_dsetArrayFloat);
      }

      /* -- write last (nt-1) brick again -- */
      /* TODO: I don't know if EDIT_substitute_brick has a bug or if I'm doing
       * something wrong, but writing the mask into brick n also writes the
       * mask in brick n-1. So I am writing brick n in n+1 and the mask in n+2,
       * to not overwrite brick n with the mask.
       */

      if( (tmp_dsetArrayFloat = (float *)malloc(nvox * sizeof(float))) == NULL ) {
        snprintf(errorString, LONG_STRING, "writeModelBrik: "
        "Memory allocation failed!"); 
        
        /* free and return */
        DSET_unload(dsetModel); 
        RETURN(1);
      }
      for (v=0; v<nvox; ++v) tmp_dsetArrayFloat[v]= (float) dsetTrainArray[nt-1][v];
      EDIT_substitute_brick( dsetModel, nt, MRI_float, tmp_dsetArrayFloat);

      /* -- write mask data into last brick of model dataset */
      if( (tmp_dsetArrayFloat = (float *)malloc(nvox * sizeof(float))) == NULL ) {
        snprintf(errorString, LONG_STRING, "writeModelBrik: "
          "Memory allocation failed!"); 
        
        /* free and return */
        DSET_unload(dsetModel); 
        RETURN(1);
      }
      if (options->maskFile[0]) { 
        for (v=0; v<nvox; ++v) tmp_dsetArrayFloat[v] = (float) dsetMaskArray[v];
      }
      else {
        for (v=0; v<nvox; ++v) tmp_dsetArrayFloat[v] = 1.0;
      }
      EDIT_substitute_brick( dsetModel, nt+1, MRI_float, tmp_dsetArrayFloat );

      break;

    case MRI_short:
      /* -- write train data into model dataset --*/
      for (t=0; t<nt; ++t) {
        if( (tmp_dsetArrayShort = (short *)malloc(nvox * sizeof(short))) == NULL ) {
          snprintf(errorString, LONG_STRING, "writeModelBrik: "
          "Memory allocation failed!"); 
          
          /* free and return */
          DSET_unload(dsetModel); 
          RETURN(1);
        }
        for (v=0; v<nvox; ++v) tmp_dsetArrayShort[v]= (short) dsetTrainArray[t][v];
        EDIT_substitute_brick( dsetModel, t, MRI_short, tmp_dsetArrayShort);
      }

      /* -- write last brick (nt-1) again -- */
      /* TODO: I don't know if EDIT_substitute_brick has a bug or if I'm doing
       * something wrong, but writing the mask into brick n also writes the
       * mask in brick n-1. So I am writing brick n in n+1 and the mask in n+2,
       * to not overwrite brick n with the mask.
       */

      if( (tmp_dsetArrayShort = (short *)malloc(nvox * sizeof(short))) == NULL ) {
        snprintf(errorString, LONG_STRING, "writeModelBrik: "
          "Memory allocation failed!"); 
        
        /* free and return */
        DSET_unload(dsetModel); 
        RETURN(1);
      }
      for (v=0; v<nvox; ++v) tmp_dsetArrayShort[v]= (short) dsetTrainArray[nt-1][v];
      EDIT_substitute_brick( dsetModel, nt, MRI_short, tmp_dsetArrayShort);


      /* -- write mask into the last brick of model dataset  -- */
      if (options->maskFile[0]) {
        for (v=0; v<nvox; ++v) tmp_dsetArrayShort[v] = (short) dsetMaskArray[v];
      }
      else {
        for (v=0; v<nvox; ++v) tmp_dsetArrayShort[v] = 1;
      }
      EDIT_substitute_brick(dsetModel, nt+1, MRI_short, tmp_dsetArrayShort);
      
      break;

    default:
      snprintf(errorString, LONG_STRING, "writeModelBrik: " 
          "Writing model failed! Unknown datum-type (%d)", datum);

      RETURN(1);
      break;
  }

  /* --- write header of model dataset --- */
  /* -- allocating csv strings -- */
  csv_string_size = afniModel->combinations*CSV_STRING;
  if( (csv_combName = (char *) malloc(csv_string_size * sizeof(char))) == NULL ) {
    snprintf(errorString, LONG_STRING, "writeModelBrik: "
          "Memory allocation for csv_combName failed!"); 

    DSET_unload(dsetModel); 
    RETURN(1);
  }
  if( (csv_kernelCustom = (char *) malloc(csv_string_size * sizeof(char))) == NULL ) {
    snprintf(errorString, LONG_STRING, "writeModelBrik: "
          "Memory allocation for csv_kernelCustom failed!"); 
    
    DSET_unload(dsetModel); 
    free(csv_combName);
    RETURN(1);
  }

  /* -- record history -- */
  commandline = tross_commandline(PROGRAM_NAME, argc, argv);
  if (commandline == NULL) {
    WARNING_message("Can not copy command-line into model header!");
  }
  else tross_Append_History (dsetModel, commandline);
  free(commandline);

  /* -- write model header -- */
  strncpy(csv_combName, afniModel->combName[0], csv_string_size);
  strncpy(csv_kernelCustom, afniModel->kernel_custom[0], csv_string_size);

  for(i = 1; i < afniModel->combinations; ++i) {
    strncat(csv_combName, ",", csv_string_size);
    strncat(csv_combName, afniModel->combName[i], csv_string_size);
    strncat(csv_kernelCustom, ",", csv_string_size);
    strncat(csv_kernelCustom, afniModel->kernel_custom[i], csv_string_size);
  }

  /* JL July 2011: */
  {
  int max_iterations = (int)afniModel->max_iterations; /* would be ideally long */
  THD_set_int_atr( dsetModel->dblk, "3DSVM_MAX_ITERATIONS", 1, &max_iterations);
  }
  
  /* JL May 2010: Write if mask was used into the header */
  if (options->modelFile[0]) maskUsed = 1; else maskUsed=0;
  THD_set_int_atr( dsetModel->dblk, "3DSVM_MASK_USED", 1, &maskUsed);

  THD_set_string_atr( dsetModel->dblk, "3DSVM_VERSION",       /* JL Apr. 2010 */
      VERSION_3DSVM);
  THD_set_string_atr( dsetModel->dblk, "3DSVM_VERSION_DATE",  /* JL Apr. 2010 */
        VERSION_DATE_3DSVM);
  THD_set_int_atr( dsetModel->dblk, "3DSVM_CLASS_COUNT", 1,
      &afniModel->class_count);
  THD_set_int_atr( dsetModel->dblk, "3DSVM_CLASS_COMBINATIONS", 1, 
      &afniModel->combinations);
  THD_set_int_atr( dsetModel->dblk, "3DSVM_TIMEPOINTS", 1,
      &afniModel->timepoints);
  THD_set_string_atr( dsetModel->dblk, "3DSVM_COMBO_NAMES",
      csv_combName);
  THD_set_string_atr( dsetModel->dblk, "3DSVM_SVM_TYPE",
      afniModel->svm_type );                                   /* JL May 2009 */ 
  THD_set_string_atr( dsetModel->dblk, "3DSVM_KERNEL_CUSTOM",
      csv_kernelCustom);                                       /* JL Feb 2009 */
  THD_set_int_atr( dsetModel->dblk, "3DSVM_KERNEL_TYPE",
      afniModel->combinations, afniModel->kernel_type);        /* JL May 2009 */
  THD_set_int_atr( dsetModel->dblk, "3DSVM_POLYNOMIAL_DEGREE",
      afniModel->combinations, afniModel->polynomial_degree);
  THD_set_float_atr( dsetModel->dblk, "3DSVM_RBF_GAMMA",
      afniModel->combinations, afniModel->rbf_gamma);
  THD_set_float_atr( dsetModel->dblk, "3DSVM_LINEAR_COEFFICIENT",
      afniModel->combinations, afniModel->linear_coefficient);
  THD_set_float_atr( dsetModel->dblk, "3DSVM_CONSTANT_COEFFICIENT",
      afniModel->combinations, afniModel->constant_coefficient);
  THD_set_int_atr( dsetModel->dblk, "3DSVM_TOTAL_MASKED_FEATURES",
      afniModel->combinations, afniModel->total_masked_features);
  THD_set_int_atr( dsetModel->dblk, "3DSVM_TOTAL_SAMPLES",
      afniModel->combinations, afniModel->total_samples);
  THD_set_int_atr( dsetModel->dblk, "3DSVM_TOTAL_SUPPORT_VECTORS",
      afniModel->combinations, afniModel->total_support_vectors);
  THD_set_float_atr( dsetModel->dblk, "3DSVM_B",
      afniModel->combinations, afniModel->b );
  /* JL Oct 2009: */
  THD_set_float_atr( dsetModel->dblk, "3DSVM_EPS",
      afniModel->combinations, afniModel->eps ); 
  THD_set_float_atr( dsetModel->dblk, "3DSVM_SVM_C",
      afniModel->combinations, afniModel->svm_c );
  THD_set_int_atr( dsetModel->dblk, "3DSVM_BIASED_HYPERPLANE",
      afniModel->combinations, afniModel->biased_hyperplane ); 
  THD_set_int_atr( dsetModel->dblk, "3DSVM_SKIP_FINAL_OPT_CHECK",
      afniModel->combinations, afniModel->skip_final_opt_check);
  THD_set_int_atr( dsetModel->dblk, "3DSVM_SVM_MAXQPSIZE",
      afniModel->combinations, afniModel->svm_maxqpsize );
  THD_set_int_atr( dsetModel->dblk, "3DSVM_SVM_NEWVARSINQP",
      afniModel->combinations, afniModel->svm_newvarsinqp );
  THD_set_int_atr( dsetModel->dblk, "3DSVM_SVM_ITER_TO_SHRINK",
      afniModel->combinations, afniModel->svm_iter_to_shrink );
  THD_set_float_atr( dsetModel->dblk, "3DSVM_TRANSDUCTION_POSRATIO",
      afniModel->combinations, afniModel->transduction_posratio );
  THD_set_float_atr( dsetModel->dblk, "3DSVM_SVM_COSTRATIO",
      afniModel->combinations, afniModel->svm_costratio );
  THD_set_float_atr( dsetModel->dblk, "3DSVM_SVM_COSTRATIO_UNLAB",
      afniModel->combinations, afniModel->svm_costratio_unlab );
  THD_set_float_atr( dsetModel->dblk, "3DSVM_SVM_UNLABBOUND",
      afniModel->combinations, afniModel->svm_unlabbound );
  THD_set_float_atr( dsetModel->dblk, "3DSVM_EPSILON_A",
      afniModel->combinations, afniModel->epsilon_a );
  THD_set_float_atr( dsetModel->dblk, "3DSVM_EPSILON_CRIT",
      afniModel->combinations, afniModel->epsilon_crit );
  THD_set_int_atr( dsetModel->dblk, "3DSVM_COMPUTE_LOO",
      afniModel->combinations, afniModel->compute_loo );
  THD_set_float_atr( dsetModel->dblk, "3DSVM_RHO",
      afniModel->combinations, afniModel->rho );
  THD_set_int_atr( dsetModel->dblk, "3DSVM_XA_DEPTH",
      afniModel->combinations, afniModel->xa_depth );
  for(i = 0; i < afniModel->combinations; ++i) {
    snprintf(headernames, LONG_STRING, "3DSVM_ALPHAS_%s",afniModel->combName[i]);
    THD_set_float_atr( dsetModel->dblk, headernames, nalphas, afniModel->alphas[i] );
  }
  
  /* --- write brick --- */
  fflush(stdout);
  INFO_message( "Writing model dataset: %s...", fileName );
  THD_write_3dim_dataset( "./", fileName, dsetModel, True );

  /* --- free memory ---*/
  free(csv_combName);
  free(csv_kernelCustom);

  RETURN(0);
}


 /* JL May  2009: Added 'ASLoptions *options' to support sv-regression */
void addToAfniModel(AFNI_MODEL *afniModel, MODEL *model, LEARN_PARM *learn_parm,
    LabelType *tmp_labels, ASLoptions *options, long classCount,
    long sampleCount, int comb0, int comb1)
{
  long nsv    = 0; /* number of support vectors */
  long sv     = 0; /* index over nsv */
  long nt     = 0; /* number of timepoints */
  long t      = 0; /* index over timepoints */
  long qid    = 0; /* incrementing queryid */
  long nqid   = 0;

  FILE *fp    = NULL; /* alpha file output for sv-regression*/
  char alphaFile[LONG_STRING]; /* naming of alphafile output */

  ENTRY("addToAfniModel");


  /* JL July 2009: Changed this function to retrieve the alphas directly from
   * the svm-light modelfile. Now, we are assigning a queryid containing the
   * time information to each doc (timepoint), which allows us to retrieve the
   * alphas in time order.
   *
   * JL Aug. 2009: Added alpha file output for sv-regression to this function,
   * since, for sv-regression, svm-light is not writing the alphas in time-
   * order
   *
   * JL Jul. 2014: Changed how alphas are stored for regression and how they
   * are written to file.
   *
   * JL Jul. 2015: Bugfix. Initialized alphas properly. Otherwise, some alphas
   * that should be zero might not be zero. This leads to problems 
   * (too many SVs) when the model is read in. 
   * Got rid of cAlphas (censored alphas). Unnecessary. 
   * Write both sets of alphas for regression even if they are zero. 
   */

  /* --- initialization ---*/
  nsv = model->sv_num;
  nt = afniModel->timepoints;
 
  /* - open file for writing alphas */ 
  if( options->modelAlphaFile[0] ) {
    if (afniModel->class_count > 2) {
      snprintf( alphaFile, LONG_STRING, "%s_%d_%d.1D", options->modelAlphaFile,
          comb0, comb1);
    }
    else {
      snprintf( alphaFile, LONG_STRING, "%s.1D", options->modelAlphaFile);
    }

    if ( (fp=fopen(alphaFile, "w")) == NULL ) {
      ERROR_message("Can not open alphafile: %s for writing", alphaFile);
    }
    else {
      fflush(stdout);
      if ( verbosity >= 1 ) INFO_message("Writing alphafile: %s...", alphaFile);
    }
  }

  /* recover time-order of alphas using quid and write them into 
   * afniModel->alphas (index over all time points)
   */

  /* - initialize alphas to zero - */
  if( !strcmp(options->svmType, "regression") ) { 
    /* For regression, the arrays holding the alphas is twice as long */
    for( t=0; t<2*nt; ++t ) afniModel->alphas[classCount][t] = 0.0;
  }
  else {
    for( t=0; t<nt; ++t ) afniModel->alphas[classCount][t] = 0.0;
  }

  qid=0;
  for( t=0; t<nt; ++t ) {
    /* only look at non-censored time-points, quid index only runs 
     * over non-censored time-points */
    if ( abs((int)rint(tmp_labels[t])) == 1) {

      /* - searching for alpha with (queryid == qid) - */ 
      for( sv=1; sv<nsv; ++sv) {
        if ( (model->supvec[sv])->queryid == qid) {
          afniModel->alphas[classCount][t] = (float)model->alpha[sv];

          /* - alpha with quid found. Exit loop over sv - */
          break;
        }
      }

      /* - write alpha to file - */
      if( options->modelAlphaFile[0] ) {
        fprintf(fp,"%.4g", afniModel->alphas[classCount][t]);
      }

      /* For regression, the number of alphas might double, so the 
       * array size for storing the alphas is twice as long  (nt*2) as for 
       * classification. Continue looping through the svmLight model and
       * keep searching for alphas with given qid. */
      if( !strcmp(options->svmType, "regression") ) { 
        ++sv;
        for( ; sv<nsv; ++sv) {
          if( (model->supvec[sv])->queryid == qid ) {
            afniModel->alphas[classCount][nt+t] = (float)model->alpha[sv];
              (float)model->alpha[sv];

            /* - second alpha with quid found. Exit loop over sv - */
            break;        
          }
        }
        /* - write second alpha to file - */
        if( options->modelAlphaFile[0] ) {
          fprintf(fp,"\t %.4g", afniModel->alphas[classCount][nt+t]);
        }
      }
      
      /* - done with writing alpha(s) for current timepoint - */
      if( options->modelAlphaFile[0] ) fprintf(fp,"\n");
      
      /* increment qid (quid only runs over non-censored time-points!) */
      ++qid;
    }
    else {
      /* - censored timepoints alpha=0 - -*/
      if( options->modelAlphaFile[0] ) {
        if( !strcmp(options->svmType, "regression") ) { 
          fprintf(fp,"%.4g\t %.4g\n", 0.0, 0.0);
        }
        else {
          fprintf(fp,"%.4g\n", 0.0);
        }
      }
    }
  }
  if( options->modelAlphaFile[0] ) fclose(fp);

  /* JL Feb. 2009: Added kernel_custom and kernel_type
   *    May. 2009: Added svm_type to support sv-regression
   *    Oct. 2009: Added remaining model parameters that can be specified 
   *               via command-line 
   *    July 2011: Added max_iterations.
   *
   *    TODO: Some parameters such as max_iterations don't change across
   *    class combinations, but get assigned here. Need to put then 
   *    in allocateAfniModel instead. 
   */


  snprintf( afniModel->svm_type, LONG_STRING, "%s", options->svmType);
  snprintf( afniModel->combName[classCount], CSV_STRING, "%d_%d", comb0, comb1 );
  snprintf( afniModel->kernel_custom[classCount], CSV_STRING,  "%s", model->kernel_parm.custom);
  afniModel->kernel_type[classCount] = model->kernel_parm.kernel_type; 
  afniModel->polynomial_degree[classCount] = model->kernel_parm.poly_degree;  
  afniModel->rbf_gamma[classCount] = model->kernel_parm.rbf_gamma; 
  afniModel->linear_coefficient[classCount] = model->kernel_parm.coef_lin; 
  afniModel->constant_coefficient[classCount] = model->kernel_parm.coef_const; 
  afniModel->total_masked_features[classCount] = (int) model->totwords;
  afniModel->total_samples[classCount] = (int) model->totdoc;
  afniModel->total_support_vectors[classCount] = (int) model->sv_num;
  afniModel->b[classCount] = model->b;
  
  /* July 2011: */
  afniModel->max_iterations=learn_parm->max_iterations; 
  
  /* Oct 2009: */
  afniModel->eps[classCount] = learn_parm->eps; 
  afniModel->svm_c[classCount] = learn_parm->svm_c;
  afniModel->biased_hyperplane[classCount] = learn_parm->biased_hyperplane;                                 
  afniModel->skip_final_opt_check[classCount] = learn_parm->skip_final_opt_check;
  afniModel->svm_maxqpsize[classCount] = learn_parm->svm_maxqpsize;
  afniModel->svm_newvarsinqp[classCount] = learn_parm->svm_newvarsinqp;
  afniModel->svm_iter_to_shrink[classCount] = learn_parm->svm_iter_to_shrink;
  afniModel->transduction_posratio[classCount] = learn_parm->transduction_posratio;
  afniModel->svm_costratio[classCount] = learn_parm->svm_costratio;
  afniModel->svm_costratio_unlab[classCount] = learn_parm->svm_costratio_unlab;
  afniModel->svm_unlabbound[classCount] = learn_parm->svm_unlabbound;
  afniModel->epsilon_a[classCount] = learn_parm->epsilon_a;
  afniModel->epsilon_crit[classCount] = learn_parm->epsilon_crit;
  afniModel->compute_loo[classCount] = learn_parm->compute_loo;
  afniModel->rho[classCount] = learn_parm->rho;
  afniModel->xa_depth[classCount] = learn_parm->xa_depth;

  EXRETURN;
}

/* JL Apr 2010: This function takes a dataset array and returns a dataset array
 * without censored time-points.
 *
 * For regression a few things can be simplified:
 * - we are only supporting censoring with a separate censor file (not 9999s)
 * - we don't have to worry about multi-class.
 *
 */
DatasetType** getAllocateCensoredRegressionArray(DatasetType **dsetArray,
    LABELS *labels, long nvox)
{
  long  v         = 0; /* index over voxels */
  long  t         = 0; /* index over timepoints */
  long  tnc       = 0; /* index over timepoints non censored */
  long  nt        = 0; /* number of timepoints */
  long  ntc       = 0; /* number of timepoints censored */

  DatasetType **dsetArrayCensored  = NULL;


  ENTRY("getAllocateCensoredRegressionArray");

  
  /* JL July 2011: Return NULL if memory can not be allocated */

  nt = labels->n;
  ntc = labels->n_cnsrs;

  /* allocate */
  if( (dsetArrayCensored = Allocate2DT(nt-ntc, nvox)) == NULL ) {
    RETURN(NULL);
  }

  /* extract non-censored time-points */
  tnc=0;
  for( t=0; t<nt; ++t ) {
    if( labels->cnsrs[t] != 0 ) {
      for( v=0 ; v<nvox ; ++v ) {
        dsetArrayCensored[tnc][v] = dsetArray[t][v];
      } ++tnc;
    }
  }

  RETURN(dsetArrayCensored);
}

/* JL July 2011: Added the corresponding free to 
 *  getAllocateCensoredRegressionArray 
 */
void freeCensoredRegressionArray(DatasetType **dsetArray, LABELS *labels) {

  ENTRY("freeCensoredRegressionArray");

  free2DT(dsetArray, labels->n - labels->n_cnsrs);

  EXRETURN;
}
  

/* JL Apr. 2010: This function retrieves the training array and the targets
 * for the current class-combination based on censoredTarget
 *
 * Note: We use 9999 as an input from the user to ignore time-points, but we
 * also use 9999 for internal purposes to exclude time-points, that do not belong
 * to the current class-combination
 */
void getClassTrainArrayAndTarget(DatasetType **dsetTrainArray,
    LabelType *censoredTarget, DatasetType **dsetClassTrainArray,
    LabelType *classTarget, long nt, long nvox)
{

  long  v      = 0;           /* index over nvox */
  long  t      = 0;           /* index over nt */
  long  k      = 0;           /* index over non censored time-points */


  ENTRY("getClassTrainArrayAndTarget");

   for( t=0; t<nt; ++t ) {
     if( censoredTarget[t] != 9999 ) {
       /* sample is not supposed to be ignored (is an observation of
        * current class combination and not censored by user) */

       /* -- set target value for svm-light  (+1,-1, or 0) -- */
       classTarget[k] = censoredTarget[t];

       for( v=0; v<nvox; ++v ){
          dsetClassTrainArray[k][v] = dsetTrainArray[t][v];
       }
       ++k;
     }
   }
  
   EXRETURN;
}

/* JL Feb. 2009: Added 'ASLoptions *options' as an argument to
 * support handling of complex-valued data */
void afni_dset_to_svm_doc( DOC *docs, DatasetType **dsetArray,
    MaskType* maskArray, long tpts, long nvoxels, long nmasked )
{
  long i, j, k;    /* loop indices */

  /* JL and SL July 2009: Writing the time order into the queryid of the
   * DOC structure (we hijacked this for our own evil purposes),
   * which allows us to retrieve the time order of the support
   * vectors and alphas after training. 
   * Queryid only is used in svm-light's ranking, so we can use this entry
   * for classification and regression. */

  ENTRY("afni_dset_to_svm_doc");

  for( i=0; i < tpts; ++i ) {
    docs[i].docnum = i;
    /* docs[i].queryid = 0; */
    docs[i].queryid = i; /* we hijacked here*/
    docs[i].costfactor = 1;
    docs[i].words[nmasked].wnum = 0; /* svmLight stop signal */

    if( maskArray ) {
      k = 0;
      for( j=0 ; j<nvoxels ; ++j) {
	    if( maskArray[j] ) {
	      docs[i].words[k].wnum = k+1;
	      docs[i].words[k].weight = (FVAL) dsetArray[i][j]; /*FVAL is svmLight defined*/
	      ++k;
	    }
      }
    }
    else {
      for( j=0 ; j<nvoxels ; ++j) {
	  docs[i].words[j].wnum = j+1;
	  docs[i].words[j].weight = (FVAL) dsetArray[i][j];
      }
    }
  docs[i].twonorm_sq = sprod_ss(&docs[i].words[0],&docs[i].words[0]);
  }

  EXRETURN; 
}

/*-----------------------------------------------------------*/
/* JL Apr. 2010: This function was previously named getTmpLabels. Changed name
 * to reflect that time-points which do no belong to the current class-combination
 * are censored (labeled with 9999).
 *
 */
int getCensoredClassTarget(LabelType *censoredTarget, long *sampleCount,
    LABELS *labels, long classIndex0, long classIndex1, enum modes mode,
    char *errorString)
{
  long i = 0;
  short labelWarningFlag = 0;  /* warn users if unknown class label
                                  - probably from multi-class */
  int class0 = 0;
  int class1 = 0;
  *sampleCount = 0;

  
  /* 
   * JL June 2011: Modified error handling: 
   * Replaced ERROR_exit by RETURN(1), passing error message as errorString
   * to the calling function;
   *
   */

  ENTRY("getCensoredClassTarget");

  if (mode == TRAIN) {
    class0 = labels->class_list[classIndex0];
    class1 = labels->class_list[classIndex1];
  }
  else if (mode == TEST) {
    class0 = classIndex0;
    class1 = classIndex1;
  }
  else { /* We should never get here */
    snprintf(errorString, LONG_STRING,
        "What happened?! getCensoredClassTarget: unknown mode!");
    RETURN(1);
  }

  if(verbosity >= 2) printf("++ ");

  for( i=0 ; i<labels->n ; ++i) {   /* convert timeseries input to one that
                                       can be used with svm light. */
    if( (int)(labels->cnsrs[i]) ) {
      if(labels->lbls[i] == class0) { /*class a */
        censoredTarget[i] = -1.0;
        (*sampleCount)++;
      }
      else if(labels->lbls[i] == class1) { /* class b */
        censoredTarget[i] = 1.0;
        (*sampleCount)++;
      }
      else if( labels->lbls[i] == -9999 ) { /* transductive sample */
        censoredTarget[i] = 0.0;
        (*sampleCount)++;
      }
      else if( labels->lbls[i] == 9999 ) { /* ignore sample */
        censoredTarget[i] = 9999.0;
      }
      else {
        censoredTarget[i] = 9999.0; /* invalid value - ignore */
        labelWarningFlag = 1;
        if(verbosity >= 2) printf("%ld,", i); /* ignored time point */
      }
    }
    else {
      censoredTarget[i] = 9999.0; /* censored sample - ignore */
    }
  }
  if( labelWarningFlag && (verbosity >= 1) ) {
    INFO_message("Time points ignored. If not using multi-class, check for bad labels.");
  }


  RETURN(0);
}

void freeAfniModel(AFNI_MODEL *afniModel)
{
  
  long max_comb = CLASS_MAX*(CLASS_MAX-1)/2;

  ENTRY("freeAfniModel");
  
  free( afniModel->kernel_type );
  free( afniModel->polynomial_degree );
  free( afniModel->rbf_gamma );
  free( afniModel->linear_coefficient );
  free( afniModel->constant_coefficient );
  free( afniModel->total_masked_features );
  free( afniModel->total_samples );
  free( afniModel->total_support_vectors );
  free( afniModel->b );
  free2f(afniModel->alphas,  (long) afniModel->combinations);
  free2c(afniModel->combName, max_comb);

  /* Oct. 2008: */
  if( afniModel->version >= 0.80 ) {
    free2c(afniModel->kernel_custom, max_comb);
  }
  
  /* JL Nov 2009: */
  if( afniModel->version >= 1.10 ) {
    free( afniModel->eps );
    free( afniModel->svm_c );
    free( afniModel->biased_hyperplane );
    free( afniModel->skip_final_opt_check );
    free( afniModel->svm_maxqpsize );
    free( afniModel->svm_newvarsinqp );
    free( afniModel->svm_iter_to_shrink );
    free( afniModel->transduction_posratio );
    free( afniModel->svm_costratio );
    free( afniModel->svm_costratio_unlab );
    free( afniModel->svm_unlabbound );
    free( afniModel->epsilon_a );
    free( afniModel->epsilon_crit );
    free( afniModel->compute_loo );
    free( afniModel->rho );
    free( afniModel->xa_depth );
  }

  EXRETURN;
}

int allocateAfniModel(AFNI_MODEL *afniModel, LABELS *labels, 
    ASLoptions *options, char *errorString)
{ 
  long max_comb = CLASS_MAX*(CLASS_MAX-1)/2;
  long nalphas = 0;

  ENTRY("allocateAfniModel");


  /* JL June 2009: Enabled sv-regression.
   *
   * JL June 2011: Modified error handling: Passing error string as argument
   * to the calling function, allocated memory is freed, RETURN(1) 
   * instead of ERROR_exit. Checking each malloc individually. 
   *
   * JL July 2014: Changed allocation of array holding alphas for regression 
   *
   */

  /* alpha and alpha* might be stored separately for sv-regression,
   * so the array being allocated to store them needs to be twice as long */
  if( !strcmp(options->svmType, "regression") ) {
    nalphas = (int) labels->n*2; 
  }
  else if( !strcmp(options->svmType, "classification") ) {
    nalphas = (int) labels->n;
  }
  else {
    /* should never get here */
    snprintf(errorString, LONG_STRING, "allocateAfniModel: SVM type unknown!");
    RETURN(1);
  }
  
  afniModel->timepoints = (int) labels->n; 
    /* would like to be long, but no equivalent to THD_set_int_atr */
  afniModel->class_count = (int) labels->n_classes;	
    /* would like to be long, but no equivalent to THD_set_int_atr */
  afniModel->combinations = (long) ( (labels->n_classes * (labels->n_classes - 1)) / 2 );
  if( (afniModel->kernel_type = (int *)malloc( afniModel->combinations * sizeof(int))) == NULL ) {
    snprintf(errorString, LONG_STRING, "allocateAfniModel: Memory allocation for kernel_type failed!");

    /* return */
    RETURN(1);
  }
  if( (afniModel->polynomial_degree = (int *)malloc( afniModel->combinations * sizeof(int))) == NULL ) { 
    snprintf(errorString, LONG_STRING, "allocateAfniModel: Memory allocation for polynomial_degree failed!");
 
    /* free and return */
    free(afniModel->kernel_type);
    RETURN(1);
  }

  if( (afniModel->rbf_gamma = (float *)malloc( afniModel->combinations * sizeof(float))) == NULL ) { 
    snprintf(errorString, LONG_STRING, "allocateAfniModel: Memory allocation for rbf_gamma failed!");
    
    /* free and return */
    free(afniModel->kernel_type);
    free(afniModel->polynomial_degree);
    RETURN(1);
  }

  if( (afniModel->linear_coefficient = (float *)malloc( afniModel->combinations * sizeof(float))) == NULL ) { 
    snprintf(errorString, LONG_STRING, "allocateAfniModel: Memory allocation for linear_coefficient failed!");
    
    /* free and return */
    free(afniModel->kernel_type);
    free(afniModel->polynomial_degree);
    free(afniModel->rbf_gamma);
    RETURN(1);
  }


  if( (afniModel->constant_coefficient = (float *)malloc( afniModel->combinations * sizeof(float))) == NULL ) { 
    snprintf(errorString, LONG_STRING, "allocateAfniModel: Memory allocation for constant_coefficient failed!");

    /* free and return */
    free(afniModel->kernel_type);
    free(afniModel->polynomial_degree);
    free(afniModel->rbf_gamma);
    free(afniModel->linear_coefficient);
    RETURN(1);
  }

  if( (afniModel->total_masked_features = (int *)malloc( afniModel->combinations * sizeof(int))) == NULL ) { 
    snprintf(errorString, LONG_STRING, "allocateAfniModel: Memory allocation for total_masked_features failed!");

    /* free and return */
    free(afniModel->kernel_type);
    free(afniModel->polynomial_degree);
    free(afniModel->rbf_gamma);
    free(afniModel->linear_coefficient);
    free(afniModel->constant_coefficient);
    RETURN(1);
  }

  if( (afniModel->total_samples = (int *)malloc( afniModel->combinations * sizeof(int))) == NULL ) { 
    snprintf(errorString, LONG_STRING, "allocateAfniModel: Memory allocation for total_samples failed!");

    /* free and return */
    free(afniModel->kernel_type);
    free(afniModel->polynomial_degree);
    free(afniModel->rbf_gamma);
    free(afniModel->linear_coefficient);
    free(afniModel->constant_coefficient);
    free(afniModel->total_masked_features);
    RETURN(1);
  }

  if( (afniModel->total_support_vectors = (int *)malloc( afniModel->combinations * sizeof(int))) == NULL ) { 
    snprintf(errorString, LONG_STRING, "allocateAfniModel: Memory allocation for total_support_vectors failed!");

    /* free and return */
    free(afniModel->kernel_type);
    free(afniModel->polynomial_degree);
    free(afniModel->rbf_gamma);
    free(afniModel->linear_coefficient);
    free(afniModel->constant_coefficient);
    free(afniModel->total_masked_features);
    free(afniModel->total_samples);
    RETURN(1);
  }

  if( (afniModel->b = (float *)malloc( afniModel->combinations * sizeof(float))) == NULL ) { 
    snprintf(errorString, LONG_STRING, "allocateAfniModel: Memory allocation for b failed!");

    /* free and return */
    free(afniModel->kernel_type);
    free(afniModel->polynomial_degree);
    free(afniModel->rbf_gamma);
    free(afniModel->linear_coefficient);
    free(afniModel->constant_coefficient);
    free(afniModel->total_masked_features);
    free(afniModel->total_samples);
    free(afniModel->total_support_vectors);
    RETURN(1);
  }

  if( (afniModel->alphas = Allocate2f((long) afniModel->combinations, nalphas)) == NULL ) {
    snprintf(errorString, LONG_STRING, "allocateAfniModel: Memory allocation for alphas failed!");

    /* free and return */
    free(afniModel->kernel_type);
    free(afniModel->polynomial_degree);
    free(afniModel->rbf_gamma);
    free(afniModel->linear_coefficient);
    free(afniModel->constant_coefficient);
    free(afniModel->total_masked_features);
    free(afniModel->total_samples);
    free(afniModel->total_support_vectors);
    free(afniModel->b);
    RETURN(1);
  }


  /* JL Nov 2009: Added model parameters */
  if( (afniModel->eps = (float *)malloc( afniModel->combinations * sizeof(float) )) == NULL ) {
    snprintf(errorString, LONG_STRING, "allocateAfniModel: Memory allocation for eps failed!");

    /* free and return */
    free(afniModel->kernel_type);
    free(afniModel->polynomial_degree);
    free(afniModel->rbf_gamma);
    free(afniModel->linear_coefficient);
    free(afniModel->constant_coefficient);
    free(afniModel->total_masked_features);
    free(afniModel->total_samples);
    free(afniModel->total_support_vectors);
    free(afniModel->b);
    free2f(afniModel->alphas, (long) afniModel->combinations);
    RETURN(1);
  }

  if( (afniModel->svm_c = (float *)malloc( afniModel->combinations * sizeof(float) )) == NULL ) {
    snprintf(errorString, LONG_STRING, "allocateAfniModel: Memory allocation for svm_c!");

    /* free and return */
    free(afniModel->kernel_type);
    free(afniModel->polynomial_degree);
    free(afniModel->rbf_gamma);
    free(afniModel->linear_coefficient);
    free(afniModel->constant_coefficient);
    free(afniModel->total_masked_features);
    free(afniModel->total_samples);
    free(afniModel->total_support_vectors);
    free(afniModel->b);
    free2f(afniModel->alphas, (long) afniModel->combinations);
    free(afniModel->eps);
    RETURN(1);
  }

  if( (afniModel->biased_hyperplane = (int *)malloc( afniModel->combinations * sizeof(int) )) == NULL ) {
    snprintf(errorString, LONG_STRING, "allocateAfniModel: Memory allocation for biased_hyperplane failed!");
    
    /* free and return */
    free(afniModel->kernel_type);
    free(afniModel->polynomial_degree);
    free(afniModel->rbf_gamma);
    free(afniModel->linear_coefficient);
    free(afniModel->constant_coefficient);
    free(afniModel->total_masked_features);
    free(afniModel->total_samples);
    free(afniModel->total_support_vectors);
    free(afniModel->b);
    free2f(afniModel->alphas, (long) afniModel->combinations);
    free(afniModel->eps);
    free(afniModel->svm_c);
    RETURN(1);
  }

  if( (afniModel->skip_final_opt_check = (int *)malloc( afniModel->combinations * sizeof(int) )) == NULL ) {
    snprintf(errorString, LONG_STRING, "allocateAfniModel: Memory allocation for skip_final_opt_check failed!");

    /* free and return */
    free(afniModel->kernel_type);
    free(afniModel->polynomial_degree);
    free(afniModel->rbf_gamma);
    free(afniModel->linear_coefficient);
    free(afniModel->constant_coefficient);
    free(afniModel->total_masked_features);
    free(afniModel->total_samples);
    free(afniModel->total_support_vectors);
    free(afniModel->b);
    free2f(afniModel->alphas, (long) afniModel->combinations);
    free(afniModel->eps);
    free(afniModel->svm_c);
    free(afniModel->biased_hyperplane);
    RETURN(1);
  }

  if( (afniModel->svm_maxqpsize = (int *)malloc( afniModel->combinations * sizeof(int))) == NULL ) { 
    snprintf(errorString, LONG_STRING, "allocateAfniModel: Memory allocation for svm_maxqpsize failed!");
    
    /* free and return */
    free(afniModel->kernel_type);
    free(afniModel->polynomial_degree);
    free(afniModel->rbf_gamma);
    free(afniModel->linear_coefficient);
    free(afniModel->constant_coefficient);
    free(afniModel->total_masked_features);
    free(afniModel->total_samples);
    free(afniModel->total_support_vectors);
    free(afniModel->b);
    free2f(afniModel->alphas, (long) afniModel->combinations);
    free(afniModel->eps);
    free(afniModel->svm_c);
    free(afniModel->biased_hyperplane);
    free(afniModel->skip_final_opt_check);
    RETURN(1);
  }

  if( (afniModel->svm_newvarsinqp = (int *)malloc( afniModel->combinations * sizeof(int))) == NULL ) { 
    snprintf(errorString, LONG_STRING, "allocateAfniModel: Memory allocation for svm_newvarsinqp failed!");
    
    /* free and return */
    free(afniModel->kernel_type);
    free(afniModel->polynomial_degree);
    free(afniModel->rbf_gamma);
    free(afniModel->linear_coefficient);
    free(afniModel->constant_coefficient);
    free(afniModel->total_masked_features);
    free(afniModel->total_samples);
    free(afniModel->total_support_vectors);
    free(afniModel->b);
    free2f(afniModel->alphas, (long) afniModel->combinations);
    free(afniModel->eps);
    free(afniModel->svm_c);
    free(afniModel->biased_hyperplane);
    free(afniModel->skip_final_opt_check);
    free(afniModel->svm_maxqpsize);
    RETURN(1);
  }

  if( (afniModel->svm_iter_to_shrink = (int *)malloc( afniModel->combinations * sizeof(int))) == NULL ) { 
    snprintf(errorString, LONG_STRING, "allocateAfniModel: Memory allocation for svm_iter_to_shrink failed!");
    
    /* free and return */
    free(afniModel->kernel_type);
    free(afniModel->polynomial_degree);
    free(afniModel->rbf_gamma);
    free(afniModel->linear_coefficient);
    free(afniModel->constant_coefficient);
    free(afniModel->total_masked_features);
    free(afniModel->total_samples);
    free(afniModel->total_support_vectors);
    free(afniModel->b);
    free2f(afniModel->alphas, (long) afniModel->combinations);
    free(afniModel->eps);
    free(afniModel->svm_c);
    free(afniModel->biased_hyperplane);
    free(afniModel->skip_final_opt_check);
    free(afniModel->svm_maxqpsize);
    free(afniModel->svm_newvarsinqp);
    RETURN(1);
  }

  if( (afniModel->transduction_posratio = (float *)malloc( afniModel->combinations * sizeof(float))) == NULL ) { 
    snprintf(errorString, LONG_STRING, "allocateAfniModel: Memory allocation for transduction_posratio failed!");

    /* free and return */
    free(afniModel->kernel_type);
    free(afniModel->polynomial_degree);
    free(afniModel->rbf_gamma);
    free(afniModel->linear_coefficient);
    free(afniModel->constant_coefficient);
    free(afniModel->total_masked_features);
    free(afniModel->total_samples);
    free(afniModel->total_support_vectors);
    free(afniModel->b);
    free2f(afniModel->alphas, (long) afniModel->combinations);
    free(afniModel->eps);
    free(afniModel->svm_c);
    free(afniModel->biased_hyperplane);
    free(afniModel->skip_final_opt_check);
    free(afniModel->svm_maxqpsize);
    free(afniModel->svm_newvarsinqp);
    free(afniModel->svm_iter_to_shrink);
    RETURN(1);
  }

  if( (afniModel->svm_costratio = (float *)malloc( afniModel->combinations * sizeof(float))) == NULL ) { 
    snprintf(errorString, LONG_STRING, "allocateAfniModel: Memory allocation for svm_costratio failed!");
    
    /* free and return */
    free(afniModel->kernel_type);
    free(afniModel->polynomial_degree);
    free(afniModel->rbf_gamma);
    free(afniModel->linear_coefficient);
    free(afniModel->constant_coefficient);
    free(afniModel->total_masked_features);
    free(afniModel->total_samples);
    free(afniModel->total_support_vectors);
    free(afniModel->b);
    free2f(afniModel->alphas, (long) afniModel->combinations);
    free(afniModel->eps);
    free(afniModel->svm_c);
    free(afniModel->biased_hyperplane);
    free(afniModel->skip_final_opt_check);
    free(afniModel->svm_maxqpsize);
    free(afniModel->svm_newvarsinqp);
    free(afniModel->svm_iter_to_shrink);
    free(afniModel->transduction_posratio);
    RETURN(1);
  }

  if( (afniModel->svm_costratio_unlab = (float *)malloc( afniModel->combinations * sizeof(float))) == NULL ) { 
    snprintf(errorString, LONG_STRING, "allocateAfniModel: Memory allocation for svm_costratio_unlab failed!");
    
    /* free and return */
    free(afniModel->kernel_type);
    free(afniModel->polynomial_degree);
    free(afniModel->rbf_gamma);
    free(afniModel->linear_coefficient);
    free(afniModel->constant_coefficient);
    free(afniModel->total_masked_features);
    free(afniModel->total_samples);
    free(afniModel->total_support_vectors);
    free(afniModel->b);
    free2f(afniModel->alphas, (long) afniModel->combinations);
    free(afniModel->eps);
    free(afniModel->svm_c);
    free(afniModel->biased_hyperplane);
    free(afniModel->skip_final_opt_check);
    free(afniModel->svm_maxqpsize);
    free(afniModel->svm_newvarsinqp);
    free(afniModel->svm_iter_to_shrink);
    free(afniModel->transduction_posratio);
    free(afniModel->svm_costratio);
    RETURN(1);
  }

  if( (afniModel->svm_unlabbound = (float *)malloc( afniModel->combinations * sizeof(float))) == NULL ) { 
    snprintf(errorString, LONG_STRING, "allocateAfniModel: Memory allocation for svm_unlabbound failed!");
    
    /* free and return */
    free(afniModel->kernel_type);
    free(afniModel->polynomial_degree);
    free(afniModel->rbf_gamma);
    free(afniModel->linear_coefficient);
    free(afniModel->constant_coefficient);
    free(afniModel->total_masked_features);
    free(afniModel->total_samples);
    free(afniModel->total_support_vectors);
    free(afniModel->b);
    free2f(afniModel->alphas, (long) afniModel->combinations);
    free(afniModel->eps);
    free(afniModel->svm_c);
    free(afniModel->biased_hyperplane);
    free(afniModel->skip_final_opt_check);
    free(afniModel->svm_maxqpsize);
    free(afniModel->svm_newvarsinqp);
    free(afniModel->svm_iter_to_shrink);
    free(afniModel->transduction_posratio);
    free(afniModel->svm_costratio);
    free(afniModel->svm_costratio_unlab);
    RETURN(1);
  }

  if( (afniModel->epsilon_a = (float *)malloc( afniModel->combinations * sizeof(float))) == NULL ) { 
    snprintf(errorString, LONG_STRING, "allocateAfniModel: Memory allocation for epsilon_a failed!");
    
    /* free and return */
    free(afniModel->kernel_type);
    free(afniModel->polynomial_degree);
    free(afniModel->rbf_gamma);
    free(afniModel->linear_coefficient);
    free(afniModel->constant_coefficient);
    free(afniModel->total_masked_features);
    free(afniModel->total_samples);
    free(afniModel->total_support_vectors);
    free(afniModel->b);
    free2f(afniModel->alphas, (long) afniModel->combinations);
    free(afniModel->eps);
    free(afniModel->svm_c);
    free(afniModel->biased_hyperplane);
    free(afniModel->skip_final_opt_check);
    free(afniModel->svm_maxqpsize);
    free(afniModel->svm_newvarsinqp);
    free(afniModel->svm_iter_to_shrink);
    free(afniModel->transduction_posratio);
    free(afniModel->svm_costratio);
    free(afniModel->svm_costratio_unlab);
    free(afniModel->svm_unlabbound);
    RETURN(1);
  }

  if( (afniModel->epsilon_crit = (float *)malloc( afniModel->combinations * sizeof(float))) == NULL ) { 
    snprintf(errorString, LONG_STRING, "allocateAfniModel: Memory allocation for epsilon_crit failed!");
    
    /* free and return */
    free(afniModel->kernel_type);
    free(afniModel->polynomial_degree);
    free(afniModel->rbf_gamma);
    free(afniModel->linear_coefficient);
    free(afniModel->constant_coefficient);
    free(afniModel->total_masked_features);
    free(afniModel->total_samples);
    free(afniModel->total_support_vectors);
    free(afniModel->b);
    free2f(afniModel->alphas, (long) afniModel->combinations);
    free(afniModel->eps);
    free(afniModel->svm_c);
    free(afniModel->biased_hyperplane);
    free(afniModel->skip_final_opt_check);
    free(afniModel->svm_maxqpsize);
    free(afniModel->svm_newvarsinqp);
    free(afniModel->svm_iter_to_shrink);
    free(afniModel->transduction_posratio);
    free(afniModel->svm_costratio);
    free(afniModel->svm_costratio_unlab);
    free(afniModel->svm_unlabbound);
    free(afniModel->epsilon_a);
    RETURN(1);
  }

  if( (afniModel->compute_loo = (int *)malloc( afniModel->combinations * sizeof(int))) == NULL ) { 
    snprintf(errorString, LONG_STRING, "allocateAfniModel: Memory allocation for compute_loo failed!");
    
    /* free and return */
    free(afniModel->kernel_type);
    free(afniModel->polynomial_degree);
    free(afniModel->rbf_gamma);
    free(afniModel->linear_coefficient);
    free(afniModel->constant_coefficient);
    free(afniModel->total_masked_features);
    free(afniModel->total_samples);
    free(afniModel->total_support_vectors);
    free(afniModel->b);
    free2f(afniModel->alphas, (long) afniModel->combinations);
    free(afniModel->eps);
    free(afniModel->svm_c);
    free(afniModel->biased_hyperplane);
    free(afniModel->skip_final_opt_check);
    free(afniModel->svm_maxqpsize);
    free(afniModel->svm_newvarsinqp);
    free(afniModel->svm_iter_to_shrink);
    free(afniModel->transduction_posratio);
    free(afniModel->svm_costratio);
    free(afniModel->svm_costratio_unlab);
    free(afniModel->svm_unlabbound);
    free(afniModel->epsilon_a);
    free(afniModel->epsilon_crit);
    RETURN(1);
  }

  if( (afniModel->rho = (float *)malloc( afniModel->combinations * sizeof(float))) == NULL ) { 
    snprintf(errorString, LONG_STRING, "allocateAfniModel: Memory allocation for rho failed!");
    
    /* free and return */
    free(afniModel->kernel_type);
    free(afniModel->polynomial_degree);
    free(afniModel->rbf_gamma);
    free(afniModel->linear_coefficient);
    free(afniModel->constant_coefficient);
    free(afniModel->total_masked_features);
    free(afniModel->total_samples);
    free(afniModel->total_support_vectors);
    free(afniModel->b);
    free2f(afniModel->alphas, (long) afniModel->combinations);
    free(afniModel->eps);
    free(afniModel->svm_c);
    free(afniModel->biased_hyperplane);
    free(afniModel->skip_final_opt_check);
    free(afniModel->svm_maxqpsize);
    free(afniModel->svm_newvarsinqp);
    free(afniModel->svm_iter_to_shrink);
    free(afniModel->transduction_posratio);
    free(afniModel->svm_costratio);
    free(afniModel->svm_costratio_unlab);
    free(afniModel->svm_unlabbound);
    free(afniModel->epsilon_a);
    free(afniModel->epsilon_crit);
    free(afniModel->compute_loo);
    RETURN(1);
  }

  if( (afniModel->xa_depth = (int *)malloc( afniModel->combinations * sizeof(int))) == NULL ) { 
    snprintf(errorString, LONG_STRING, "allocateAfniModel: Memory allocation for xa_depth failed!");
    
    /* free and return */
    free(afniModel->kernel_type);
    free(afniModel->polynomial_degree);
    free(afniModel->rbf_gamma);
    free(afniModel->linear_coefficient);
    free(afniModel->constant_coefficient);
    free(afniModel->total_masked_features);
    free(afniModel->total_samples);
    free(afniModel->total_support_vectors);
    free(afniModel->b);
    free2f(afniModel->alphas, (long) afniModel->combinations);
    free(afniModel->eps);
    free(afniModel->svm_c);
    free(afniModel->biased_hyperplane);
    free(afniModel->skip_final_opt_check);
    free(afniModel->svm_maxqpsize);
    free(afniModel->svm_newvarsinqp);
    free(afniModel->svm_iter_to_shrink);
    free(afniModel->transduction_posratio);
    free(afniModel->svm_costratio);
    free(afniModel->svm_costratio_unlab);
    free(afniModel->svm_unlabbound);
    free(afniModel->epsilon_a);
    free(afniModel->epsilon_crit);
    free(afniModel->compute_loo);
    free(afniModel->rho);
    RETURN(1);
  }

  /* JL Apr 2010: Changed to dynamic allocation  */
  if( (afniModel->combName = Allocate2c(max_comb, (long)CSV_STRING)) == NULL ) {
    snprintf(errorString, LONG_STRING, "allocateAfniModel: Memory allocation for combName failed!");
    
    /* free and return */
    free(afniModel->kernel_type);
    free(afniModel->polynomial_degree);
    free(afniModel->rbf_gamma);
    free(afniModel->linear_coefficient);
    free(afniModel->constant_coefficient);
    free(afniModel->total_masked_features);
    free(afniModel->total_samples);
    free(afniModel->total_support_vectors);
    free(afniModel->b);
    free2f(afniModel->alphas, (long) afniModel->combinations);
    free(afniModel->eps);
    free(afniModel->svm_c);
    free(afniModel->biased_hyperplane);
    free(afniModel->skip_final_opt_check);
    free(afniModel->svm_maxqpsize);
    free(afniModel->svm_newvarsinqp);
    free(afniModel->svm_iter_to_shrink);
    free(afniModel->transduction_posratio);
    free(afniModel->svm_costratio);
    free(afniModel->svm_costratio_unlab);
    free(afniModel->svm_unlabbound);
    free(afniModel->epsilon_a);
    free(afniModel->epsilon_crit);
    free(afniModel->compute_loo);
    free(afniModel->rho);
    free(afniModel->xa_depth);
    RETURN(1);
  }
  Clear2c(afniModel->combName, max_comb);

  if( (afniModel->kernel_custom = Allocate2c(max_comb, (long)CSV_STRING)) == NULL ) {
    snprintf(errorString, LONG_STRING, "allocateAfniModel: Memory allocation for kernel_custom failed!");
    
    /* free and return */
    free(afniModel->kernel_type);
    free(afniModel->polynomial_degree);
    free(afniModel->rbf_gamma);
    free(afniModel->linear_coefficient);
    free(afniModel->constant_coefficient);
    free(afniModel->total_masked_features);
    free(afniModel->total_samples);
    free(afniModel->total_support_vectors);
    free(afniModel->b);
    free2f(afniModel->alphas, (long) afniModel->combinations);
    free(afniModel->eps);
    free(afniModel->svm_c);
    free(afniModel->biased_hyperplane);
    free(afniModel->skip_final_opt_check);
    free(afniModel->svm_maxqpsize);
    free(afniModel->svm_newvarsinqp);
    free(afniModel->svm_iter_to_shrink);
    free(afniModel->transduction_posratio);
    free(afniModel->svm_costratio);
    free(afniModel->svm_costratio_unlab);
    free(afniModel->svm_unlabbound);
    free(afniModel->epsilon_a);
    free(afniModel->epsilon_crit);
    free(afniModel->compute_loo);
    free(afniModel->rho);
    free(afniModel->xa_depth);
    free2c(afniModel->combName, max_comb);
    RETURN(1);
  }
  Clear2c(afniModel->kernel_custom, max_comb);

  RETURN(0);
}

void freeAfniModelAndArrays(AFNI_MODEL *afniModel,
    DatasetType **dsetModelArray, MaskType *dsetMaskArray,
    long nt_model)
{
  ENTRY("freeAfniModelAndArrays");

  freeModelArrays(dsetModelArray, dsetMaskArray, nt_model, afniModel->mask_used);
  freeAfniModel(afniModel);

  EXRETURN;
}

/* JL Oct 2010: This functions determintes the svm learn type (svm_type),
 * reads and allocates afniModel, dsetModelArray and dsetMaskArray.
 */
int readAllocateAfniModelAndArrays( ASLoptions *options, AFNI_MODEL *afniModel,
    THD_3dim_dataset *dsetModel, DatasetType ***dsetModelArray,
    MaskType **dsetMaskArray, long *nt_model, long *nvox_model,
    enum modes mode, int *svm_type, char *errorString )
{

  DatasetType ** tmp_dsetModelArray = NULL;
  MaskType *     tmp_dsetMaskArray  = NULL;


  ENTRY("readAllocateAfniModelAndArrays");


  /* JL July 2011: Modified error handling: Passing error message 
   * as argument (errorString) to the calling function, 
   * allocated memory is freed, RETURN(1) instead of ERROR_exit().
   *
   * TODO: Should pass arrays directly to the testing functions, 
   * (not read them from disc twice!), if training and testing 
   * is performed at once. 
   */


  /*----- VIEWTYPE POSTFIX FOR MODEL (quick fix !!!) -----*/
  /* It would be better to not read in the model from disc */
  if( mode == TRAIN_AND_TEST ) {
    if( strstr(options->trainFile,"+orig") != NULL ) {
      if( strstr(options->testFile,"+orig") == NULL ) {
        snprintf(errorString, LONG_STRING, 
            "Viewtype of train dataset: %s does not match\n"
            "   Viewtype of test dataset: %s!", options->trainFile,
            options->testFile);

        RETURN(1);
      }
      else strncat(options->modelFile,"+orig", LONG_STRING);
    }
    else if( strstr(options->trainFile,"+tlrc") != NULL ) {
      if( strstr(options->testFile,"+tlrc") == NULL ) {
        snprintf(errorString, LONG_STRING, 
            "Viewtype of train dataset: %s does not match\n"
            "   Viewtype of test dataset: %s!", options->trainFile,
            options->testFile);

        RETURN(1);
      }
      else strncat(options->modelFile,"+tlrc", LONG_STRING);
    }
    else if( strstr(options->trainFile,"+acpc") != NULL ) {
      if( strstr(options->testFile,"+acpc") == NULL ) {
        snprintf(errorString, LONG_STRING, 
            "Viewtype of train dataset: %s does not match\n"
            "   Viewtype of test dataset: %s!", options->trainFile,
            options->testFile);

        RETURN(1);
      }
      else strncat(options->modelFile,"+acpc", LONG_STRING);
    }
    else {  
      snprintf(errorString, LONG_STRING, "Model viewtype unknown!");
      RETURN(1);
    }
  }

  /*----- LOAD AFNI MODEL -----*/
  if( (dsetModel = THD_open_one_dataset( options->modelFile )) == NULL ) {
    snprintf(errorString, LONG_STRING,
        "Failed to open model dataset: %s", options->modelFile);

    RETURN(1);
  }
  DSET_load( dsetModel );

  /*----- READ AFNI MODEL -----*/
  if( readAllocateAfniModel(dsetModel, afniModel, errorString) ) {
    
    /* free and return */
    DSET_unload(dsetModel);
    RETURN(1);
  }
 
  /*---- GET MODEL AND MASK ARRAY ------------------*/
  if( getAllocateModelArrays(dsetModel, &tmp_dsetModelArray, &tmp_dsetMaskArray,
     nt_model, nvox_model, &afniModel->mask_used, options->outModelNoMask,
     errorString) ) {

    /* free and return */
    DSET_unload(dsetModel);
    freeAfniModel(afniModel);
    RETURN(1);
  }

  /* --- SET svm learn type ---*/
  if( !strcmp(afniModel->svm_type, "regression") ) *svm_type = REGRESSION;
  else  *svm_type = CLASSIFICATION;

  /*----- FREE MEMORY ------*/
  DSET_unload( dsetModel );

  *dsetMaskArray = tmp_dsetMaskArray;
  *dsetModelArray = tmp_dsetModelArray;

  RETURN(0);
}

void freeClassificationLabels(LABELS *labels) {

  ENTRY("freeClasssificationLabels");

  free(labels->lbls);
  free(labels->cnsrs);
  free(labels->lbls_cont);
  free(labels->class_list);
  free(labels->lbls_count);
  
  EXRETURN;
}
   
int getAllocateClassificationLabels( LABELS *labels, char *labelFile, 
    char *censorFile, char *errorString )
{
  FILE *fp = NULL;
  int class_exists_flag = 0;
  long i,j,k = 0;
  char labelString[LONG_STRING];
  int strLength = 0;

  ENTRY("getAllocateClassificationLabels");

  /* Changes:
   * JL April 2010: Added checking for empty lines in label- and censor file
   * JL Aug.  2010: Fixed a bug in determining number of classes: Number of
   *                of classes was calculated incorrectly if an entire class was
   *                censored completely using the censorfile.
   * JL Aug. 2010:  Added lbls_cont. which holds the user-given labels
   *                converted  to continues label values (i.e. 0,1,2,...,n)
   *                (needed for calculation of multiclass prediction accuracies).
   * JL Sep. 2010:  Improved error checking for censor file
   *
   * JL June 2011:  Modified error handling: Passing error string as argument
   *                to the calling function, allocated memory is freed, RETURN(1)
   *                instead of ERROR_exit.
   *
   * JL Mar. 2014:  Determine occurrence of each label and store in lbls_count
   *                (Needed for calculating of multi-class accuracies)
   *             
   */ 

  /*----- RETRIEVE LABEL FILE AND CENSOR FILE--------------*/
  if( (fp = fopen(labelFile, "r")) == NULL ) {
    snprintf(errorString, LONG_STRING, "Could not open .1D label file: %s", 
        labelFile);

    RETURN(1);
  }
  
  /* --- get length --- */
  labels->n = getFileSize(labelFile);

  /* --- allocate labels --- */
  if( (labels->lbls = (LabelType*)malloc(sizeof(LabelType)*labels->n)) == NULL ) {
    snprintf(errorString, LONG_STRING, "getAllocateClassifcationLabels: "
        "Could not allocate lbls!");

    /* free and return */
    fclose(fp);
    RETURN(1);
  }

  if( (labels->lbls_cont = (LabelType*)malloc(sizeof(LabelType)*labels->n)) == NULL ) {
    snprintf(errorString, LONG_STRING, "getAllocateClassificationLabels: "
        "Could not allocate lbls_cont!");

    /* free and return */
    fclose(fp);
    free(labels->lbls);
    RETURN(1);
  }

  if( (labels->class_list = (int *)malloc(sizeof(int)*CLASS_MAX)) == NULL ) {
    snprintf(errorString, LONG_STRING, "getAllocateClassificationLabels: "
        "Could not allocate labels class list!");

    /* free and return */
    fclose(fp);
    free(labels->lbls);
    free(labels->lbls_cont);
    RETURN(1);
  }
  
  /* JL Mar. 2014 */
  if( (labels->lbls_count = (int *)malloc(sizeof(int)*CLASS_MAX)) == NULL ) {
    snprintf(errorString, LONG_STRING, "getAllocateClassificationLabels: "
        "Could not allocate lbls_count!");

    /* free and return */
    fclose(fp);
    free(labels->lbls);
    free(labels->lbls_cont);
    free(labels->class_list);
    RETURN(1);
  }


  /* --- read labels from file and do some error checking --- */
  for( i=0; i<labels->n; i++ ) {
    fgets(labelString, LONG_STRING, fp);

    /* -- check for empty lines -- */
    if ( (strLength = strlen(labelString)) == 1 ) {
      snprintf(errorString, LONG_STRING, "Labelfile: '%s' contains empty "
          "entry in line %ld!", labelFile, i+1);
      
      /* free and return */
      fclose(fp);
      free(labels->lbls);
      free(labels->lbls_cont);
      free(labels->class_list);
      free(labels->lbls_count);
      RETURN(1);
    }
    else labels->lbls[i] = (LabelType) atof(labelString);

    /* -- check for negative entires other than -9999 */
    if ( (labels->lbls[i] < 0.0) && ((int)labels->lbls[i] != -9999) ) {
      snprintf(errorString, LONG_STRING, "Labelfile: '%s' contains a negative "
          "entry in line %ld! ", labelFile, i+1);
      /* free and return */
      fclose(fp);
      free(labels->lbls);
      free(labels->lbls_cont);
      free(labels->class_list);
      free(labels->lbls_count);
      RETURN(1);
    }
  }
  fclose(fp);

  /* --- allocate censors --- */
  if( (labels->cnsrs = (LabelType *)malloc(sizeof(LabelType)*labels->n)) == NULL ) {
    snprintf(errorString, LONG_STRING, "getAllocateClassificationLabels: "
        "Could not allocate censors!");

    /* free and return */
    free(labels->lbls);
    free(labels->lbls_cont);
    free(labels->class_list);
    free(labels->lbls_count);

    RETURN(1);
  }

  /* --- initialize censors ---*/
  for( i=0; i<labels->n; ++i ) labels->cnsrs[i] = 1.0;
  labels->n_cnsrs = 0;

  /* --- read censors from file and do some error checking --- */
  if( censorFile[0] ) {
    if( (fp = fopen(censorFile,"r")) == NULL ) {
      snprintf(errorString, LONG_STRING, "Could not open .1D censor file: %s",
          censorFile);
     
      /* free and return */
      free(labels->lbls);
      free(labels->lbls_cont);
      free(labels->class_list);
      free(labels->lbls_count);
      free(labels->cnsrs);
      RETURN(1);
    }
    /* -- check if size of labelfile matches size of censorfile -- */
    if( labels->n != getFileSize(censorFile) ) {
      snprintf(errorString, LONG_STRING, "Lenght of labelfile: '%s' (%ld) "
          "does not match length of censorfile: '%s' (%ld)!", 
          labelFile, labels->n, censorFile, getFileSize(censorFile));

      /* free and return */
      free(labels->lbls);
      free(labels->lbls_cont);
      free(labels->class_list);
      free(labels->lbls_count);
      free(labels->cnsrs);
      fclose(fp);
      RETURN(1);
    }
    
    /* -- read censors and do some more error checking -- */
    for(i=0; i<labels->n; ++i) {
      fgets(labelString, LONG_STRING, fp);
      
      /* - check for empty lines - */
      if ( (strLength = strlen(labelString)) == 1 ) {
        snprintf(errorString, LONG_STRING, "Censorfile: '%s' line: '%ld' is "
            "empty!", censorFile, i+1);

        /* free and return */
        free(labels->lbls);
        free(labels->lbls_cont);
        free(labels->class_list);
        free(labels->lbls_count);
        free(labels->cnsrs);
        fclose(fp);
        RETURN(1);
      }
      else labels->cnsrs[i] = (LabelType) atof(labelString);

      /* - check for values other than 1 and 0 - */
      if ( (strcmp(trimString(labelString), "0")) &&
           (strcmp(trimString(labelString), "1")) ) { 
        
        snprintf(errorString, LONG_STRING, "Consorfile: '%s' line: '%ld' "
            "contains invalid entry: '%s'. Only 0 or 1 is allowed!", 
            censorFile, i+1, labelString);
        
        /* free and return */
        free(labels->lbls);
        free(labels->lbls_cont);
        free(labels->class_list);
        free(labels->lbls_count);
        free(labels->cnsrs);
        fclose(fp);
        RETURN(1);
      }
    }
    fclose(fp);
  }

  /*----- DETERMINE NUMBER OF CLASSES --------------*/
  /* --- initializ class list --- */
  for(j=0; j<CLASS_MAX; ++j) {
    labels->class_list[j] = 9999;
    labels->lbls_count[j] = 0;
  }

  /* i indexes all time points
  ** j indexes over total allowed classes (CLASS_MAX)
  ** k increments as each new class label is found
  */
  labels->n_classes = 0;
  k = 0;
  for( i=0; i < labels->n; ++i ) {
    if( ((int)rint(labels->lbls[i]) != 9999)  &&  /* not censored in labelfile */
        ((int)rint(labels->lbls[i]) != -9999) &&  /* not trunsductive ) */
        ((int)rint(labels->cnsrs[i])) )  {   /* not censored in censorfile*/

      for( j=0; j < CLASS_MAX; ++j ) {
        if( (int)rint(labels->lbls[i]) == labels->class_list[j] ) {
          class_exists_flag = 1;
          break;
        }
      }
      if( !class_exists_flag ) {
        labels->class_list[k] = (int)rint(labels->lbls[i]);
        ++labels->n_classes;
        ++k;
      }
      else {
        class_exists_flag = 0;
      }
    }
    else { 
      labels->n_cnsrs++;
      if ((int)rint(labels->lbls[i]) != -9999) labels->cnsrs[i] = (LabelType)0;
    }
  }
  
  /* -- sort label list -- */
  qsort( labels->class_list, CLASS_MAX, sizeof(int), (void *)compare_ints );

  /* -- convert user-given labels to continuous label values */
  for (j=0; j<labels->n_classes; ++j) {
    for(i=0; i<labels->n; ++i ) {
      if( ((int)rint(labels->lbls[i]) != 9999)  && /* not censored in labelfile */
          ((int)rint(labels->lbls[i]) != -9999) && /* not trunsductive ) */
          ((int)rint(labels->cnsrs[i])) )       {  /* not censored in censorfile*/

        if ((int)rint(labels->lbls[i]) == labels->class_list[j]) {
          labels->lbls_cont[i] = (LabelType)j;
          /* JL Mar 2014: Count occurrence of each label */
          labels->lbls_count[j]++;
        }
      }
      else {
        labels->lbls_cont[i] = (LabelType)9999;
      }
    }
  }

  if(verbosity >= 1) {
    INFO_message( "Number of classes = %d\n", labels->n_classes );
    printf("++ "); 
    for( i = 0; i < labels->n_classes; ++i ) {
      printf( "class[%ld] = %d, ", i, labels->class_list[i] );
    }
    printf("\n");
  }

  if (labels->n_classes >= CLASS_MAX) {
    snprintf(errorString, LONG_STRING, "Max numer of classes hard coded to %d! "
          "Complain to the authors if you need more.", CLASS_MAX-1);

    /* free and return */
    free(labels->lbls);
    free(labels->lbls_cont);
    free(labels->class_list);
    free(labels->lbls_count);
    free(labels->cnsrs);
    RETURN(1);
   }

  RETURN(0);
}

/* JL May 2009: This function may duplicate getAllocateClassificationLabels 
 * a bit, but for regression a few things can be simplified:
 *
 *  - we are only supporting censoring with a separate censor file (not 9999s)
 *  - we don't have to worry about multi-class
 *  - since the target for svm-light does not need to be updated (no multi-class)
 *    is generated here as well
 */
int getAllocateRegressionLabelsAndTarget(LABELS *labels, LabelType **target, 
    char *labelFile, char *censorFile, char *errorString)
{
  FILE *fp             = NULL;
  long i, j            = 0;
  long n9999           = 0;
  LabelType *tmpTarget = NULL;
  int strLength        = 0;
  char labelString[LONG_STRING];

  
  ENTRY("getAllocateRegressionLabelsAndTarget");

  /* JL Sep. 2010: Improved error checking for censor file and fixed a bug
   *               (length of censorfile was not determined correctly).
   * JL July 2011: Modified error handling: Passing error string as argument
   *               to the calling function, allocated memory is freed, 
   *               RETURN(1) instead of ERROR_exit().
   */ 

  /*--- open labelfile ---*/
  if( (fp = fopen(labelFile, "r") ) == NULL ) {
    snprintf(errorString, LONG_STRING, 
        "Could not open .1D label file: %s !", labelFile);

    RETURN(1);
  }

  /*--- initialize ---*/
  labels->n = getFileSize(labelFile);
  labels->n_cnsrs = 0;

  /* -- allocate lbls -- */
  if( (labels->lbls = (LabelType*)malloc(sizeof(LabelType)*labels->n)) == NULL ) { 
    snprintf(errorString, LONG_STRING, "getAllocateRegressionLabelsAndTarget: "
          "Memory allocation for labels failed!");
    
    /* free and return */
    fclose(fp);
    RETURN(1);
  }

 /* --------------------------------------------------------------------------*/
 /* TODO: This is not great and needs to be cleaned up!
  * Wasting some memory to be able to recycle existing functions
  * written for classification. 
  */
  if( (labels->class_list = (int *)malloc(sizeof(int)*CLASS_MAX)) == NULL ) {
    snprintf(errorString, LONG_STRING, "getAllocateRegressionLabelsAndTarget: " 
        "Memory allocation for class_list failed!");

    /* free and return */
    fclose(fp);
    free(labels->lbls);
    RETURN(1);

  }
  for( j=0 ; j<CLASS_MAX ; ++j ) {
    labels->class_list[j] = 9999;
  }
  labels->n_classes = 2;
  /* -------------------------------------------------------------------------*/

  /*--- read labelfile ---*/
  for(i=0; i<labels->n; i++) {
    fgets(labelString, LONG_STRING, fp);
    if ( (strLength = strlen(labelString)) == 1 ) {
    snprintf(errorString, LONG_STRING, 
        "Labelfile: '%s' contains empty entry in line %ld!", labelFile, i+1);

      /* free and return */
      fclose(fp);
      free(labels->lbls);
      free(labels->class_list);
      RETURN(1);
    }
    else labels->lbls[i] = (LabelType) atof(labelString);
  }
  fclose(fp);

  /*--- allocate censors ---*/
  if( (labels->cnsrs = (LabelType*)malloc(sizeof(LabelType)*labels->n)) == NULL ) {
    snprintf(errorString, LONG_STRING, "getAllocateRegressionLabelsAndTarget: "
        "Memory allocation for labels->cnsrs failed!");

    /* free and return */
    free(labels->lbls);
    free(labels->class_list);
    RETURN(1);
  }

  /* --- initialize censors --- */
  for(i=0; i<labels->n; ++i) labels->cnsrs[i] = 1.0;

  /*--- open censorfile ---*/
  if( censorFile[0] ) {
    if( (fp = fopen(censorFile, "r")) == NULL ) {
      snprintf(errorString, LONG_STRING, 
          "Could not open .1D censor file: %s", censorFile);

      /* free and return */
      free(labels->lbls);
      free(labels->class_list);
      free(labels->cnsrs);
      RETURN(1);
    }

    if( labels->n != getFileSize(censorFile) ) {
      snprintf(errorString, LONG_STRING, 
          "Lenght of labelfile: '%s' (%ld) does not match length of\n"
          "censorfile: '%s' (%ld)!", labelFile, labels->n, censorFile,
           getFileSize(censorFile));

      /* free and return */
      free(labels->lbls);
      free(labels->class_list);
      free(labels->cnsrs);
      fclose(fp);
      RETURN(1);
    }

    /*--- read censorfile and count censors ---*/
    labels->n_cnsrs = 0;

    for(i=0; i<labels->n; ++i) {
      fgets(labelString, LONG_STRING, fp);

      if ( (strLength = strlen(labelString)) == 1 ) {
        ERROR_exit("Censorfile: '%s' line: '%ld' is empty!", censorFile, i+1);
      }
      else labels->cnsrs[i] = (LabelType) atof(labelString);

      /* -- check for values other than 0 and 1 and count censors-- */
      if ( (strcmp(trimString(labelString), "0")) &&
           (strcmp(trimString(labelString), "1")) ) {
        snprintf(errorString, LONG_STRING,
            "Consorfile: '%s' line: '%ld' contains invalid entry: '%s'. "
            "Only 0 or 1 is allowed!", censorFile, i+1, labelString); 
        
        /* free and return */ 
        free(labels->lbls);
        free(labels->class_list);
        free(labels->cnsrs);
        fclose(fp);
        RETURN(1);
      }
      if( (int)labels->cnsrs[i] == 0 ) labels->n_cnsrs++;
    }
    fclose(fp);
  }

  /*--- allocate target ---*/
  if( (tmpTarget = (LabelType *)malloc((labels->n-labels->n_cnsrs)*sizeof(LabelType))) == NULL ) {
    snprintf(errorString, LONG_STRING, "getAllocateRegressionLabelsAndTarget: "
        "Memory allocation for target failed!");

    /* free and return */ 
    free(labels->lbls);
    free(labels->class_list);
    free(labels->cnsrs);
    RETURN(1);
  }

  /*--- check labels and create target ---*/
  j=0;
  for( i=0; i<labels->n; ++i ) {
    if( (int)labels->cnsrs[i] ) {

      /* -- check for 9999 in labels -- */
      if( (int)labels->lbls[i] == 9999 ) ++n9999;

      tmpTarget[j] = labels->lbls[i];
      ++j;
    }
  }

  if ( n9999 != 0 ) {
    WARNING_message("Labelfile '%s' contains 9999 '%ld' times.\n"
          "   For classification, '9999' can be used to ignore timepoints.\n"
          "   However, in regression (-type regression, you are running it right now)\n"
          "   '9999' can not be used to ignore timepoints\n"
          "   Please use a censorfile (option: -censor)", labelFile, n9999 ); 
  }

  
  *target = tmpTarget;
  
  RETURN(0);
}

void freeRegressionLabelsAndTarget(LABELS *labels, LabelType *target) 
{

  ENTRY("freeRegressionLabelsAndTarget");

  free(labels->lbls);
  free(labels->cnsrs);
  free(labels->class_list);
  free(target);
  
  EXRETURN;
}

int test_classification (ASLoptions *options, MODEL *model, AFNI_MODEL *afniModel, 
    THD_3dim_dataset *dsetTest, DatasetType **dsetModelArray,
    MaskType *dsetMaskArray, long nt_mod, long nvox_mod, 
    int argc, char **argv, char *errorString)
{

  DOC* docsTest           = NULL; /* svm-light data structure used for testing */

  DatasetType**
    dsetTestArray         = NULL; /* array to hold test dataset values */

  long nt                 = 0;    /* number of time points in TEST dataset */
  long nvox               = 0;    /* number of voxels per time point in TEST dataset */
  long nvox_masked        = 0;    /* number of voxels in mask */

  float dist_tmp          = 0;
  float *dist             = NULL;  /* holds the distance for all timepoints */
  float **multiclass_dist = NULL;  
   /* doing all of the pairwise tests and storing them in principle, don't have
    * to do this with directed, acyclic graph (DAG) but each test does not take
    * that long, and we may build in more options in the future.
    * This was originally a 1D array, and used "truth table" type of approach
    * that could have been slightly more robust but relied on the assumption that
    * most distances would be inside of their range ([-1,1] or transformed to [0,1]),
    * for now, I think it is better to stick with the DAG */

  long cc, dd               = 0;
  long sampleCount          = 0;   /* number of samples used in training */

  float correct             = 0.0;
  float incorrect           = 0.0;

  int DAG                   = 0;   /* abbreviation for Directed Acyclic Graph:
                                    * index variable for traversing multiclass_dist */
  short edgeFlag            = 0;   /* DAG related */
  short classExistFlag      = 0;   /* multi-class related */
  int classAssignment       = 0;   /* multi-class related */
  float *classCorrect       = NULL;
  float *classIncorrect     = NULL;

  int *classVote            = NULL; /* mulit-class vote */
  int currentComb           = 0;
  int class0, class1        = 0;
  int winningCount          = 0;    /* mulit-class vote */
  int *classList            = NULL; /* needed for mapping non-continuous 
                                     * to continuous class labels in multiclass */
                                                  

  enum mctypes { MCTYPE_DAG, MCTYPE_VOTE }; /* types for multiclass */
  enum mctypes mctype       = MCTYPE_DAG;   /* default value */

  /* labels: */
  LABELS testLabels;
  LabelType *censoredTargets= NULL; /* contains labels in svm-light readable
    * format. Here it is only used to calculate prediction accuracies and is
    * updated for each class combination (named tmp_Labels previously)*/

  /* used for strtok magic and csv strings: */
  char* p                   = NULL;
  char* q                   = NULL;
  long p_string_size        = 0;    /* size of p string, dependent on number of
                                     * number of class-combinations */
  /* etc: */
  FILE *fp                   = NULL;
  long i,j,c,cl              = 0;
  char predictionsFile[LONG_STRING];


  ENTRY("test_classification");


  /* JL Apr. 2010: Initialized all variables
   * JL Apr. 2010: Allocating p string (for strtok) dynamically.
   *               Replaced all string functions by its equivalent that takes
   *               also the string size as an argument
   * JL May. 2010: Mask is stored in brick (n and n+1) of model file
   * JL Aug. 2010: Modified multiclass (DAG and vote) to enable calculation of
   *               prediction accuracies for arbitrary (non-continuous) class labels
   * JL July 2011: Modified error handling: Passing error message 
   *               as argument (errorString) to the calling function, 
   *               allocated memory is freed, RETURN(1) instead of ERROR_exit().
   * JL Mar. 2014: Fixed -classout and multi-class accuracies for non-continuous
   *               class labels
   */

  if (verbosity >= 1) INFO_message("\n++ CLASSIFICATION (testing):\n++");

  /*----- INITIAL ERROR CHECKING ------*/
  if( afniModel == NULL || dsetModelArray == NULL ) {
    /* we should never get here */
    snprintf(errorString, LONG_STRING, "test_classification: "
        "What happened? Model could not be loaded!");
    
    RETURN(1);
  }

  /*----- LOAD TEST DATA --------*/
  if( (dsetTest = THD_open_one_dataset(options->testFile)) == NULL ) {
    snprintf(errorString, LONG_STRING, 
        "Failed to open test dataset: %s", options->testFile );

    RETURN(1);
  }
  DSET_load( dsetTest );
  nt = DSET_NUM_TIMES( dsetTest );
  nvox = DSET_NVOX( dsetTest );
  nvox_masked = afniModel->total_masked_features[0]; 
    /* assuming same mask for all class combinations */

  /*----- GET TEST LABELS ------- */
  if( options->testLabelFile[0] ) {
    if( getAllocateClassificationLabels(&testLabels, options->testLabelFile, 
        options->censorFile, errorString) ) {

      /* free and return */
      DSET_unload(dsetTest);
      RETURN(1);
    }

    if( testLabels.n != nt ) {
      snprintf(errorString, LONG_STRING,
            "Number of labels do not match the length of the test dataset:\n"
	    "   labelfile '%s' contains %ld labels, but the \n"
	    "   testvolume '%s' contains %ld brick(s). ",
            options->testLabelFile, testLabels.n, options->testFile, nt);

      /* free and return */
      DSET_unload(dsetTest);
      freeClassificationLabels(&testLabels);
      RETURN(1);
    }

    /*----- ALLOCATE censoredTargets ---- */
    if( (censoredTargets = (LabelType*)malloc(sizeof(LabelType)*testLabels.n)) == NULL ) {
      
      snprintf(errorString, LONG_STRING, "test_classification: "
          "Memory allocation for censoredTargets failed!");
      
      /* free and return */
      DSET_unload(dsetTest);
      freeClassificationLabels(&testLabels);
      RETURN(1);
    }
  }

  /*----- PRODUCE TEST DATA ARRAY -------*/
  if( (dsetTestArray = getAllocateDsetArray(dsetTest, errorString)) == NULL ) {

    /* free and return */
    DSET_unload(dsetTest);
    if( options->testLabelFile[0] ) freeClassificationLabels(&testLabels);
    if( options->testLabelFile[0] ) free(censoredTargets);
    RETURN(1);
  }
 
  /* JL May 2010: Make sure number of voxels/t in model matches
   * number of voxels/t in test dataset */
  if( nvox != nvox_mod ) {
    snprintf(errorString, LONG_STRING, 
        "Number of voxels in model: %s does not match\n"
        "   number of voxels in test dataset: %s",
        options->modelFile, options->testFile);
    
    /* free and return */
    freeDsetArray(dsetTest, dsetTestArray);
    DSET_unload(dsetTest);
    if( options->testLabelFile[0] ) freeClassificationLabels(&testLabels);
    if( options->testLabelFile[0] ) free(censoredTargets);
    RETURN(1);
  }

  /*---- SET MULTICLASS METHOD -----------------*/
  if( (options->multiclass[0]) && (afniModel->class_count > 2) ) { 
      if( !strcmp(options->multiclass, "DAG") )       mctype = MCTYPE_DAG;
      else if( !strcmp(options->multiclass, "vote") ) mctype = MCTYPE_VOTE;
      else { 
        WARNING_message("Unknown method for multiclass: %s\n" 
            "   Setting mctype = DAG [default].", options->multiclass); 

        mctype = MCTYPE_DAG;
     }
  }

  /*----- ALLOCATE AND FILL SVM-LIGHT STRUCTURES -----*/
  /* -- allocate DOCs for test dataset  -- */
  if( (docsTest = allocateDOCs(nt, nvox_masked)) == NULL ) {
    snprintf(errorString, LONG_STRING, "test_classification: "
        "Memory allocation for docsTest failed!");

    /* free and return */
    freeDsetArray(dsetTest, dsetTestArray);
    DSET_unload(dsetTest);
    if( options->testLabelFile[0] ) freeClassificationLabels(&testLabels);
    if( options->testLabelFile[0] ) free(censoredTargets);
    RETURN(1);
  }
  
  /* -- fill DOCs with test dataset -- */
  afni_dset_to_svm_doc( docsTest, dsetTestArray, dsetMaskArray, nt,
      nvox, nvox_masked);

  /* -- allocate MODEL -- */
  //model = (MODEL *)malloc(sizeof(MODEL));
  if( allocateModel(model, afniModel, errorString) ) {

    /* free and return */
    freeDsetArray(dsetTest, dsetTestArray);
    DSET_unload(dsetTest);
    if( options->testLabelFile[0] ) freeClassificationLabels(&testLabels);
    if( options->testLabelFile[0] ) free(censoredTargets);
    freeDOCs(docsTest, nt);
    RETURN(1);
  }

  /* -- fill MODEL with data from AFNI_MODEL */
  if( get_svm_model( model, dsetModelArray, dsetMaskArray, afniModel, nvox_mod,
      options->outModelNoMask, errorString) ) {

    /* free and return */
    freeDsetArray(dsetTest, dsetTestArray);
    DSET_unload(dsetTest);
    if( options->testLabelFile[0] ) freeClassificationLabels(&testLabels);
    if( options->testLabelFile[0] ) free(censoredTargets);
    freeDOCs(docsTest, nt);
    freeModel(model, afniModel, TEST);
    RETURN(1);
  }


  /*----- ALLOCATE TEST PREDICTION ARRAYS -------*/
  if( (dist = (float *)malloc(sizeof(float)*nt)) == NULL ) {
      snprintf(errorString, LONG_STRING, "test_classification: "
          "Memory allocation for dist failed!");

    /* free and return */
    freeDsetArray(dsetTest, dsetTestArray);
    DSET_unload(dsetTest);
    if( options->testLabelFile[0] ) freeClassificationLabels(&testLabels);
    if( options->testLabelFile[0] ) free(censoredTargets);
    freeDOCs(docsTest, nt);
    freeModel(model, afniModel, TEST);
    RETURN(1);
  }

  /* ------ ALLOCATE MULTICLASS ARRAYS ----- */
  
  /* JL Apr. 2010: Allocate p string for strtok */
  p_string_size = afniModel->combinations*CSV_STRING;
  if ( (p = (char *) malloc(p_string_size * sizeof (char))) == NULL ) {
    snprintf(errorString, LONG_STRING, "test_classification: "
        "Could not allocate csv string!");
      
    /* free and return */
    freeDsetArray(dsetTest, dsetTestArray);
    DSET_unload(dsetTest);
    if( options->testLabelFile[0] ) freeClassificationLabels(&testLabels);
    if( options->testLabelFile[0] ) free(censoredTargets);
    freeDOCs(docsTest, nt);
    freeModel(model, afniModel, TEST);
    free(dist);
    if( afniModel->class_count > 2 ) {
      freeMultiClassArrays(multiclass_dist, classCorrect, 
          classIncorrect,  classVote, classList,
          (long) afniModel->combinations);
    }
    RETURN(1);
  }

  /* JL July 2011: Only allocate multiclass arrays when needed */
  if( afniModel->class_count > 2 ) { /* multiclass ! */
    if( allocateMultiClassArrays(&multiclass_dist, &classCorrect, 
          &classIncorrect, &classVote,
          &classList, afniModel->class_count, (long) afniModel->combinations, 
          nt, errorString) ) {
      
      /* free and return */
      freeDsetArray(dsetTest, dsetTestArray);
      DSET_unload(dsetTest);
      if( options->testLabelFile[0] ) freeClassificationLabels(&testLabels);
      if( options->testLabelFile[0] ) free(censoredTargets);
      freeDOCs(docsTest, nt);
      freeModel(model, afniModel, TEST);
      free(dist);
      RETURN(1);
    }

    /* recover class labels in model from class combinations */
    for( c = 0; c < afniModel->class_count-1; ++c ) {
      strncpy(p, afniModel->combName[c], p_string_size);
      q = strtok(p,"_");
      cc = atol(q);
      q = strtok(NULL,"_");
      dd = atol(q);

      if( c == 0 ) classList[c]=cc;
      classList[c+1]=dd;
    }
  }
 
  /* -- binary classification -- */
  for(i = 0; i < afniModel->combinations; ++i ) {
    if(verbosity >= 1) {
      INFO_message(" ");
      INFO_message("--------------------------------------------------------------"
          "------------------");
      INFO_message("Category combination = %ld  (%s)", i, afniModel->combName[i]);
    }

    /* recover current class combination integers */
    strncpy(p, afniModel->combName[i], p_string_size);
    q = strtok(p,"_");
    cc = atol(q);
    q = strtok(NULL,"_");
    dd = atol(q);

    if( options->testLabelFile[0] ) {
      if( getCensoredClassTarget(censoredTargets, &sampleCount, &testLabels,
            cc, dd, TEST, errorString) ) {

        /* free and return */
        freeDsetArray(dsetTest, dsetTestArray);
        DSET_unload(dsetTest);
        freeClassificationLabels(&testLabels);
        free(censoredTargets);
        freeDOCs(docsTest, nt);
        freeModel(model, afniModel, TEST);
        free(dist);
        if( afniModel->class_count > 2 ) {
          freeMultiClassArrays(multiclass_dist, classCorrect, 
            classIncorrect, classVote, classList,
            (long) afniModel->combinations);
        }
        free(p);
        RETURN(1);
      }
     
      correct=0.0; 
      incorrect=0.0;
    }
  
    /*----- GET SVM-LIGHT MODEL STRUCTURE -----*/
    updateModel(model, afniModel, (int) i);

    if(afniModel->class_count == 2) {
      snprintf(predictionsFile, LONG_STRING, "%s.1D", options->predFile);
    }
    else {
      snprintf(predictionsFile, LONG_STRING, "%s_%s.1D", options->predFile,
          afniModel->combName[i]);
    }
    if( (fp = fopen( predictionsFile, "w" )) == NULL ) {
      snprintf(errorString, LONG_STRING,
          "Could not open file for writing predictions: %s", predictionsFile );

      /* free and return */
      freeDsetArray(dsetTest, dsetTestArray);
      DSET_unload(dsetTest);
      if( options->testLabelFile[0] ) freeClassificationLabels(&testLabels);
      if( options->testLabelFile[0] ) free(censoredTargets);
      freeDOCs(docsTest, nt);
      freeModel(model, afniModel, TEST);
      free(dist);
      if( afniModel->class_count > 2 ) {
        freeMultiClassArrays(multiclass_dist, classCorrect, 
          classIncorrect,  classVote, classList,
          (long) afniModel->combinations);
      }
      free(p);
      RETURN(1);
    }

    /* JL Feb. 2009: Changed this part to support non-linear kernels */
    if (afniModel->kernel_type[i] == LINEAR) { /* linear kernel */
      for(j = 0; j < nt; ++j) {
        dist_tmp=classify_example_linear(model,&docsTest[j]);
        /* should do something smarter than re-casting double to float */
        dist[j]= (float) dist_tmp;
        }
    }
    else { /* non-linear kernel */
      for(j = 0; j < nt; ++j) {
        dist_tmp=classify_example(model,&docsTest[j]);
        dist[j]= (float) dist_tmp;
      }
    }
    
    /* JL Nov. 2008: Changed detrending for censored timepoints */
    /* JL Aug. 2013: Bugfix: detrend_linear_cnsrs */ 
    if( (options->testLabelFile[0]) && (testLabels.n_cnsrs != 0) &&
        (!options->noPredDetrend) ) {
      if( detrend_linear_cnsrs(dist, &testLabels, errorString) ) {
        snprintf(errorString, LONG_STRING,
          "Could not open file for writing predictions: %s", predictionsFile );

        /* free and return */
        freeDsetArray(dsetTest, dsetTestArray);
        DSET_unload(dsetTest);
        if( options->testLabelFile[0] ) freeClassificationLabels(&testLabels);
        if( options->testLabelFile[0] ) free(censoredTargets);
        freeDOCs(docsTest, nt);
        freeModel(model, afniModel, TEST);
        free(dist);
        if( afniModel->class_count > 2 ) {
          freeMultiClassArrays(multiclass_dist, classCorrect, 
            classIncorrect, classVote, classList,
            (long) afniModel->combinations);
        }
        free(p);
        RETURN(1);
      }
    }
    else {
      /* WC and SL Aug. 08 : moved this up so that detrending is done before
       * accuracies are calculated */
      /* detrend in place - assuming no intercept (bias towards one class), or slope */
      if(!options->noPredDetrend) {
        DETREND_linear( (int) nt, dist );
      }
    }

    /* WC and SL Aug. 08 : now calculate the percent accuracy with the detrended data*/
    if( options->testLabelFile[0] ) {
      for(j = 0; j < nt; ++j){
        if( abs((int)rint(censoredTargets[j])) != 9999) {
          if(dist[j]>0) {
            if(censoredTargets[j]>0) correct++; else incorrect++;
          }
          else {
            if(censoredTargets[j]<0) correct++; else incorrect++;
          }
        }
      }
    }

    if(options->testLabelFile[0] && (verbosity>=1)) {
      INFO_message(" ");

      if (sampleCount == 0) {
        INFO_message("Accuracy on test set: 0.00%% "
            "(0 correct, 0 incorrect, 0 total)");
        INFO_message(" ");
      }
      else {
        INFO_message("Accuracy on test set: %.2f%% (%d correct, %d incorrect, %ld total)",
              (float)(correct)*100.0/sampleCount,(int)rint(correct),
              (int)rint(incorrect),sampleCount);
        INFO_message(" ");
      }
    }

    /* JL Apr. 2010: Added:
     * option -noPredCensor: Only write predictions for current class-combination
     * and without censored timepoints
     * option -noPredScale: Do not scale predictions to {0,1}
     */
    for(j = 0; j < nt; ++j) {
      if( afniModel->class_count > 2 ) multiclass_dist[i][j] += dist[j];

      /* convert output prediction to {0,1} class scale */
      if (!options->noPredScale) dist[j] = 0.5*( dist[j] + 1 );

      /* output integer class memberships */
      if( (options->classout) && (!options->noPredScale) ){
        /* dist is centered around 0.5 */
        /* JL Mar 2014: Return correct class membership for non-
         * continuous class labels (integers) */
 	if(dist[j] > 0.5) dist[j] = dd;
        else dist[j] = cc;
      }

      /* only write non-censored predictions */
      if ( options->testLabelFile[0] && options->noPredCensor ) {
        if( abs((int)rint(censoredTargets[j])) != 9999) fprintf(fp,"%.4g\n",dist[j]);
      }
      else fprintf(fp,"%.4g\n",dist[j]);
    }

    fclose(fp);
    if(verbosity >= 1)  INFO_message("Predictions written to %s\n",predictionsFile);
  }

  /* --- MULTICLASS --- */
  /* JL Aug. 2010: Modified (DAG and vote) to enable calculation of
   *               prediction accuracies for (non-continuous) class labels
   */
  if(afniModel->class_count > 2) { 

    if( options->testLabelFile[0] ) {
      correct=0.0; 
      incorrect=0.0;
      for(c = 0; c < afniModel->class_count; ++c) {
        classCorrect[c] = 0.0;
    	classIncorrect[c] = 0.0;
      }
    }

    /* --- multiclass: voting method --- */
    if (mctype == MCTYPE_VOTE) {
      snprintf(predictionsFile, LONG_STRING, "%s_overall_vote.1D", options->predFile);
      if( (fp = fopen( predictionsFile, "w" )) == NULL ) {
        ERROR_message("could not open file for writing predictions: %s", 
            predictionsFile );
      }
       
      if(verbosity >= 1)
        INFO_message(" "); 
        INFO_message("---------------------------------- vote " 
          "----------------------------------------\n");
      for(j = 0; j < nt; ++j) {
      /* code largely duplicated in DAG ................. */
        if(verbosity >=2) {
          for(i = 0; i < afniModel->combinations; ++i) { 
            INFO_message("model number:%ld time point:%ld classifier output=%f",
                i,j,multiclass_dist[i][j]);
          }
        }
        for(c = 0; c < afniModel->class_count; ++c) {
          classVote[c] = 0;
	}
        classAssignment = 0;
        winningCount = 0;
        currentComb = 0; 
        for(class0 = 0; class0 < afniModel->class_count-1; ++class0) { 
          for(class1 = class0+1; class1 < afniModel->class_count; ++class1) { 
            if(multiclass_dist[currentComb][j] < 0) {
              classVote[class0]++; 
             
              if(classVote[class0] > winningCount) {
                winningCount = classVote[class0];
                classAssignment = class0;
              }
            }
            else {
              classVote[class1]++;
              if(classVote[class1] > winningCount) { 
                winningCount = classVote[class1];
                classAssignment = class1;
              }
            }    
            currentComb++;
          }
        }
           
        if(verbosity >=2) printf("++ point number %ld:    ",j);
          for(i = 0; i < afniModel->class_count; ++i) { 
            if(verbosity >=2) printf("+ class: %d, classVote[%ld] = %d;   ",
                classList[i], i, classVote[i]);
        }
        if(verbosity >=2) printf("\n");
           
        if(verbosity >=2) INFO_message("Voting result: observation number=%ld, "
            "classAssignment = %d\n", j, classList[classAssignment]);

        /* write class assignment to prediction file */
        if ( options->testLabelFile[0] && options->noPredCensor ) {
          if ( (int)rint(testLabels.cnsrs[j]) )  {
            fprintf(fp,"%d\n", classList[classAssignment]);  
          }
        }
        else {
          fprintf(fp,"%d\n", classList[classAssignment]);  
        }


        /* compare result with label file */
        if( (options->testLabelFile[0]) && ((int)rint(testLabels.cnsrs[j])) ) {

          if (classList[classAssignment] == (int)rint(testLabels.lbls[j]))  {
            correct++; 
            classCorrect[classAssignment]++;
          }
          else {
            incorrect++;
            classIncorrect[classAssignment]++;
          }

          if(verbosity >= 2) {
            INFO_message("Overall:  test labels=%d, current number "
                "correct = %d incorrect = %d", (int)(testLabels.lbls[j]), 
                (int)rint(correct), (int)rint(incorrect));

          }
        }
      }
      fclose(fp);
    }
    /* --- multiclass:  Directed acyclic graph (DAG) ---*/
    else { /* if (mctype == MCTYPE_DAG) */

   /*  Directed acyclic graph of pairwise classifiers *************************
    *
    *  Example: N = 5
    *
    * array index(DAG) 0  1  2  3      4  5  6      7  8      9
    *  class pair   01 02 03 04  |  12 13 14  |  23 24  |  34
    *
    * set start index = N-2
    *
    *
    *                                           0 vs 4 (DAG=3)
    *L=1                                     -1/           \+N-L-1
    *                               0 vs 3 (DAG=2)            1 vs 4 (DAG=6)
    *L=2                         -1/          \+N-L      -1/           \+N-L-1
    *                  0 vs 2 (DAG=1)            1 vs 3 (DAG=5)             2 vs 4 (DAG=8)
    *L=3            -1/           \+N-L     -1/           \+N-L      -1/           \+N-L-1
    *     0 vs 1 (DAG=0)             1 vs 2 (DAG=4)            2 vs 3 (DAG=7)             3 vs 4 (DAG=9)
    *     ------------             ------------            ------------             ------------
    *     0                 1                        2                      3                  4
    *
    *
    * Right hand edge is sequence N-2  +N-2 +N-3 + N-4 ...
    * And! if you leave that edge, you can't get back
    *
    * everytime you go left, take one away from classAssignment N-1 
    ***************************************************************************/

      snprintf(predictionsFile, LONG_STRING, "%s_overall_DAG.1D", options->predFile);
      if( (fp = fopen( predictionsFile, "w" )) == NULL ) {
        snprintf(errorString, LONG_STRING,
          "Could not open file for writing predictions: %s", predictionsFile );
        
        /* free and return */
        freeDsetArray(dsetTest, dsetTestArray);
        DSET_unload(dsetTest);
        if( options->testLabelFile[0] ) freeClassificationLabels(&testLabels);
        if( options->testLabelFile[0] ) free(censoredTargets);
        freeDOCs(docsTest, nt);
        freeModel(model, afniModel, TEST);
        free(dist);
        freeMultiClassArrays(multiclass_dist, classCorrect, 
          classIncorrect,  classVote, classList,
          (long) afniModel->combinations);
        free(p);
        RETURN(1);
      }

      if(verbosity >= 1)
        INFO_message(" ");
        INFO_message("---------------------------------- DAG "
            "-----------------------------------------");
    
      if(verbosity >=2) INFO_message("Verbosity >= 2: multiclass details (note" 
        "decision threshold =0):");
      for(j = 0; j < nt; ++j) {
	if(verbosity >=2) {
	  for(i = 0; i < afniModel->combinations; ++i) {
	    INFO_message("model number:%ld time point:%ld classifier output=%f",
	        i, j, multiclass_dist[i][j]);
	  }
	}
	DAG = afniModel->class_count - 2;
	if(verbosity >= 2) printf("++ model number=%d:  ", DAG);
	classAssignment = afniModel->class_count - 1;
        /* assuming class values [0,...,N-1] */
	edgeFlag = 1;
	for(i = 1; i < afniModel->class_count; ++i) {
	  /* note: starting index at 1, and going through class_count-1 times*/
	  if(verbosity >= 2) printf("++ classifier output = %f  ", multiclass_dist[DAG][j]);
	  if(multiclass_dist[DAG][j]>0) {
	    if(edgeFlag) {
	      DAG += afniModel->class_count - i - 1;
	      if(verbosity >=2) INFO_message("next model number=%d, current max "
	          "possible classAssignment = %d", DAG, classAssignment);
	    }   
	    else {
	      DAG += afniModel->class_count - i;
	      if(verbosity >=2) INFO_message("next model number=%d, current max "
	          "possible classAssignment = %d", DAG, classAssignment);
	    }
	  }
	  else {
	    edgeFlag = 0;
	    DAG--;
	    classAssignment--;
	    if(verbosity >=2) INFO_message("next model number=%d, current max "
	        "possible classAssignment = %d", DAG, classAssignment);
	  }
	}
	if(verbosity >=2) INFO_message("DAG result: observation number=%ld model "
            "number=%d, classAssignment = %d (%d)", j, DAG, classAssignment, 
            classList[classAssignment]);

        /* write result to prediction file */
        if ( options->testLabelFile[0] && options->noPredCensor ) {
          if ( (int)rint(testLabels.cnsrs[j]) )  {
            fprintf(fp,"%d\n", classList[classAssignment]);  
          }
        }
        else {
          fprintf(fp,"%d\n", classList[classAssignment]);  
        }

        /* compare result with label file */
        if((options->testLabelFile[0]) && ((int)(testLabels.lbls_cont[j] != 9999))) {

          if (classList[classAssignment] == (int)testLabels.lbls[j])  {
            correct++; 
            classCorrect[classAssignment]++;
          }
          else {
            incorrect++;
            classIncorrect[classAssignment]++;
          }

          if(verbosity >= 2) {
            INFO_message("Overall:  test labels=%d, current number "
                "correct = %d incorrect = %d", (int)(testLabels.lbls[j]), 
                (int)rint(correct), (int)rint(incorrect));

          }
        }
      }
    fclose(fp);
    }


    /* report accuracies */
    if(options->testLabelFile[0] && (verbosity>=1)) {
      INFO_message("Overall accuracy on multiclass test set: %.2f%% "
          "(%d correct, %d incorrect, %d total)", 
          (float)correct*100.0/((int)rint(correct)+(int)rint(incorrect)),
          (int)rint(correct),(int)rint(incorrect),
          (int)rint(correct)+(int)rint(incorrect) );


      INFO_message("Individual Breakdown:");
      for( cl = 0; cl < testLabels.n_classes; ++cl ) {
        classExistFlag = 0;
        for( c = 0; c < afniModel->class_count; ++c ) {
          if( classList[c] == testLabels.class_list[cl] ) {
            classExistFlag=1;

            INFO_message("                       "
              "classLabel = %3d: %.2f%% (%d correct, %d total)\n",
              classList[c], 
              classCorrect[c]*100.0/testLabels.lbls_count[cl],
              (int)rint(classCorrect[c]), 
              testLabels.lbls_count[cl]);

            break;
          }
        }
        if(!classExistFlag) {
            WARNING_message("              "
            "classLabel = %3d: not present in model file!\n",
                testLabels.class_list[cl]);
        }
      }
    }

    /* free arrays allocated for multiclass */
    freeMultiClassArrays(multiclass_dist, classCorrect, classIncorrect, 
        classVote, classList, (long) afniModel->combinations);

  } /* multiclass done */

  if(verbosity >= 1)  {
    INFO_message("\n");
    INFO_message("Predictions for all categories written to %s", 
        predictionsFile);
  }

  /* free */
  if( options->testLabelFile[0] ) freeClassificationLabels(&testLabels);
  if( options->testLabelFile[0] ) free(censoredTargets);
  freeDOCs(docsTest, nt);
  freeModel(model, afniModel, TEST);
  free(dist);
   
  free(p);
  freeDsetArray(dsetTest, dsetTestArray);
  DSET_unload(dsetTest);


  RETURN(0);
}

/* JL May 2009: Added this function to support sv-regression in 3dsvm.
 * It is very similar to test_routine() (a lot of code has been recycled). 
 * However, major differences are:
 *
 *    - No need for mulitclass
 *    - No detrending
 *    - New function to read in the labelfile ( getAllocateRegressionLabels() )
 *    - Using rms error as a benchmark
 */ 
int test_regression (ASLoptions *options, MODEL *model, AFNI_MODEL *afniModel, 
    THD_3dim_dataset *dsetTest, DatasetType **dsetModelArray,
    MaskType *dsetMaskArray, long nt_mod, long nvox_mod, int argc, char **argv,
    char *errorString)
{
  long nt             = 0;        /* number of time points in test dataset */
  long nvox           = 0;        /* number of voxels per time point in test dataset */
  long nvox_masked    = 0;        /* number of voxels in mask */
  
  DOC*                            /* array to hold test dataset in svm-light data */
    docsTest          = NULL;     /* structure */
  DatasetType**
    dsetTestArray     = NULL;	  /* array to hold test dataset values */

  double dist_tmp     = 0;        /* temporary variable */
  double *dist        = NULL;     /* array holding the classification results for
                                     each tinepoint */

  LabelType *target   = NULL;     /* labels without censored timepoints. Assuming 
                                     the 'truth' is known and we want to determine
                                     the error. */ 
  LABELS testLabels;  
  long j              = 0;
  FILE* fp            = NULL;

  char predictionsFile[LONG_STRING];
  
  double rms          = 0;         /* used to calculate rms error */

  
  ENTRY("test_regression"); 

  /* JL July 2011: Modified error handling: Passing error message 
  *                as argument (errorString) to the calling function, 
  *                allocated memory is freed, RETURN(1) instead of ERROR_exit().
  */


  if (verbosity >= 1) INFO_message("\n++ REGRESSION (testing):\n++");

  /*----- INITIAL ERROR CHECKING ------*/
  if( afniModel == NULL || dsetModelArray == NULL ) {
    /* we should never get here */
    snprintf(errorString, LONG_STRING, "test_regression: "
        "What happened? Model could not be loaded!");
    
    RETURN(1);
  }

  /*----- LOAD TEST DATA --------*/
  if( (dsetTest = THD_open_one_dataset(options->testFile)) == NULL ) {
    snprintf(errorString, LONG_STRING, 
        "Failed to open test dataset: %s", options->testFile );

    RETURN(1);
  }
  DSET_load( dsetTest );
  nt = DSET_NUM_TIMES( dsetTest );
  nvox = DSET_NVOX( dsetTest );
  nvox_masked = afniModel->total_masked_features[0];
    /* assuming same mask for all class combinations */

  /*----- GET TEST LABELS -------*/
  if( options->testLabelFile[0] ) {
    if( getAllocateRegressionLabelsAndTarget(&testLabels, &target,
        options->testLabelFile, options->censorFile, errorString) ) {

      /* free and return */
      DSET_unload(dsetTest);
      RETURN(1);
    }

    if( testLabels.n != nt ) {
      snprintf(errorString, LONG_STRING,
            "Number of labels do not match the length of the test dataset:\n"
	    "   labelfile '%s' contains %ld labels, but the \n"
	    "   testvolume '%s' contains %ld brick(s). ",
            options->testLabelFile, testLabels.n, options->testFile, nt);

      /* free and return */
      DSET_unload(dsetTest);
      freeRegressionLabelsAndTarget(&testLabels, target);
      RETURN(1);
    }
  }

  /*----- PRODUCE TEST DATA ARRAY -------*/
  if( (dsetTestArray = getAllocateDsetArray(dsetTest, errorString) ) == NULL ) {

    /* free and return */
    DSET_unload(dsetTest);
    if( options->testLabelFile[0] ) 
      freeRegressionLabelsAndTarget(&testLabels, target);
    RETURN(1);
  }

  /*----- ALLOCATE AND FILL DOC STRUCTURE -----*/
  if( (docsTest = allocateDOCs(nt, nvox_masked)) == NULL ) {
    snprintf(errorString, LONG_STRING, "test_regression: "
        "Memory allocation for docsTest failed!");

    /* free and return */
    DSET_unload(dsetTest);
    if( options->testLabelFile[0] ) 
      freeRegressionLabelsAndTarget(&testLabels, target);
    freeDsetArray(dsetTest, dsetTestArray);
    RETURN(1);
  }

  afni_dset_to_svm_doc( docsTest, dsetTestArray, dsetMaskArray,
      nt, nvox, nvox_masked); 

  /*----- ALLOCATE AND FILL SVM MODEL -----*/
  model=(MODEL *)malloc(sizeof(MODEL));
  if( allocateModel(model, afniModel, errorString ) ) {

    /* free and return */
    DSET_unload(dsetTest);
    if( options->testLabelFile[0] ) 
      freeRegressionLabelsAndTarget(&testLabels, target);
    freeDsetArray(dsetTest, dsetTestArray);
    freeDOCs(docsTest, nt);
    RETURN(1);
  }
 
  if( get_svm_model(model, dsetModelArray, dsetMaskArray, afniModel, nvox_mod,
      options->outModelNoMask, errorString) ) {

    /* free and return */
    DSET_unload(dsetTest);
    if( options->testLabelFile[0] ) 
      freeRegressionLabelsAndTarget(&testLabels, target);
    freeDsetArray(dsetTest, dsetTestArray);
    freeDOCs(docsTest, nt);
    freeModel(model, afniModel, TEST);
    RETURN(1);
  }

  updateModel(model, afniModel,  0);

  /*----- ALLOCATE PREDICTION ARRAY --------*/
  if( (dist = (double *)malloc(sizeof(double)*nt)) == NULL ) {
    snprintf(errorString, LONG_STRING, "test_regression: "
        "Memory allocation for dist failed!");

    /* free and return */
    DSET_unload(dsetTest);
    if( options->testLabelFile[0] ) 
      freeRegressionLabelsAndTarget(&testLabels, target);
    freeDsetArray(dsetTest, dsetTestArray);
    freeDOCs(docsTest, nt);
    freeModel(model, afniModel, TEST);
    RETURN(1);
  }

  /*----- PREDICTION OUTPUT FILE -----*/
  snprintf(predictionsFile, LONG_STRING, "%s.1D", options->predFile);
  if( (fp = fopen(predictionsFile, "w" )) == NULL ) {
    snprintf(errorString, LONG_STRING,
        "Could not open file for writing predictions: %s", predictionsFile);
    
    /* free and return */
    DSET_unload(dsetTest);
    if( options->testLabelFile[0] ) 
      freeRegressionLabelsAndTarget(&testLabels, target);
    freeDsetArray(dsetTest, dsetTestArray);
    freeDOCs(docsTest, nt);
    freeModel(model, afniModel, TEST);
    free(dist);
    RETURN(1);
  }

  /*----- PERFORM TESTING -----*/
  /* JL May. 2010: Added testing for non-linear kernels */
  if (afniModel->kernel_type[0] == LINEAR) {
    for(j=0; j<nt; ++j) {
      dist_tmp = classify_example_linear(model,&docsTest[j]);
      dist[j] = (float)dist_tmp;
    }
  }
  else { /* non-linear kernel */
    for(j=0; j<nt; ++j) {
      dist_tmp = classify_example(model,&docsTest[j]);
      dist[j] = (float)dist_tmp;
    }
  }

  /*----- WRITE PREDICTIONS TO FILE -----*/
  for(j=0; j<nt; ++j) {
    if ( options->testLabelFile[0] ) {
      if (options->noPredCensor) {
        if( testLabels.cnsrs[j] == 1 ) fprintf(fp,"%.4g\n",dist[j]);
      }
      else fprintf(fp,"%.4g\n",dist[j]);
    }
    else fprintf(fp,"%.4g\n",dist[j]);
  }

  /*----- DETERMINE RMS ERROR -----*/
  if( (options->testLabelFile[0]) && (verbosity >= 1) ){
    rms=0;
    for(j=0; j<nt; ++j) {
      if ( testLabels.cnsrs[j] == 1 ) {
          rms+=(testLabels.lbls[j]-dist[j])*(testLabels.lbls[j]-dist[j]);
      }
    }
    
    rms=sqrt(rms/(testLabels.n-testLabels.n_cnsrs));

    INFO_message("--------------------------------------------------------------"
          "----------------\n++");

    INFO_message("RMS error: %.2f (%d censored, %ld total)\n++",
        rms, testLabels.n_cnsrs, testLabels.n);

    INFO_message("--------------------------------------------------------------"
          "----------------\n++");
  }
  if(verbosity >= 1) INFO_message("Predictions written to %s\n", predictionsFile);
  fclose(fp);

  /* --- FREE MEMORY --- */
  DSET_unload(dsetTest);
  if( options->testLabelFile[0] ) 
    freeRegressionLabelsAndTarget(&testLabels, target);
  freeDsetArray(dsetTest, dsetTestArray);
  freeDOCs(docsTest, nt);
  freeModel(model, afniModel, TEST);
  free(dist);

  RETURN(0);
}


/* SL & JL Feb. 2009: Included 'long *kernel_cache_size' as an argument to
 * support non-linear kernels. */
int train_classification( MODEL *model, LEARN_PARM *learn_parm, KERNEL_PARM *kernel_parm,
    long *kernel_cache_size, ASLoptions *options, THD_3dim_dataset *dsetTrain,
    THD_3dim_dataset *dsetMask, MaskType *dsetMaskArrayPtr, int argc, char **argv, 
    char * errorString )
{
  LABELS labels;                 /* structure holding labels (class-categories)
                                  * (input from user) */
  AFNI_MODEL afniModel;          /* holds everything required to write out
                                  * model.Head */
  MODEL_MAPS maps;               /* holds the maps (e.g., weight-vector maps)
                                  * for the bucket */

  LabelType*
    censoredTarget      = NULL;  /* array to hold svm-light readable labels
                                  * for current class-combination and 9999
                                  * otherwise (named tmp_labels previously)*/
  LabelType*
    classTarget         = NULL;  /* array to hold svm-light readable labels
                                  * for current class-combination
                                  * (named target previously)*/
  DatasetType**
    dsetTrainArray      = NULL;  /* array to hold training dataset values
                                  * for all time-points (JL: formerly holding
                                  * what is now called dsetClassTrainArray) */
  DatasetType**
    dsetClassTrainArray = NULL;  /* JL: array to hold training dataset values
                                  * for specific class-combination
                                  * (named dsetTrainArray previously) */
  DOC*   docsClassTrain = NULL;   /* svm-light data structure used for training
                                  * (JL: named docsTrain previously) */
  KERNEL_CACHE
    kernel_cache;                /* svm-light data structure holding kernel
                                  * parameters */

  long nt                = 0;    /* number of time points in TRAIN dataset */
  long nvox              = 0;    /* number of voxels per time point in TRAIN
                                  * dataset */
  long nvox_masked       = 0;    /* number of masked voxels */

  long classCount        = 0;    /* in training loop, keeps track of
                                  * current pairwise comb */
  long sampleCount       = 0;	 /* number of samples used in training */



  long i, cc, dd         =  0;
  char docFileName[LONG_STRING];  /* naming of svm-light textfile output */
   

  ENTRY("train_classification");

  /* JL June 2011: Modified error handling: Passing error string as argument
   * to the calling function, allocated memory is freed, RETURN(1) 
   * instead of ERROR_exit.
   *
   * JL March 2014: Removed byte data type restriction for mask dataset.
   *
   */
  
  if (verbosity >= 1) INFO_message("\n++ CLASSIFICATION (training):\n++");

  
  /*----- LOAD TRAINING DATA --------*/
  if( dsetTrain == NULL ) {
    if( (dsetTrain = THD_open_one_dataset(options->trainFile)) == NULL ) {
      snprintf(errorString, LONG_STRING, "Failed to open training dataset: %s",
          options->trainFile ); RETURN(1);
    }
    DSET_load( dsetTrain );
  }
  else {
    if( !DSET_LOADED(dsetTrain) ) {
      if( (dsetTrain = THD_open_one_dataset(options->trainFile)) == NULL ) {
      snprintf(errorString, LONG_STRING, "Failed to open training dataset: %s",
          options->trainFile ); RETURN(1);
    }
    DSET_load( dsetTrain );
    }
  }

  nt = DSET_NUM_TIMES( dsetTrain );
  nvox = DSET_NVOX( dsetTrain );
  nvox_masked = nvox; /* this will be modified later if mask used */

  /* JL Sep. 2010: Some trivial error checking */
  if( nt < 2 ) { snprintf(errorString, LONG_STRING, "Need at least two "
      "briks in training dataset %s!", options->trainFile); 
  
    /* free and return */
    DSET_unload(dsetTrain);
    RETURN(1);
  }
  if( nvox < 2 ) { snprintf(errorString, LONG_STRING, "Need at least two "
      "voxels in training dataset %s!", options->trainFile); RETURN(1);
    
    /* free and return  */
    DSET_unload(dsetTrain);
    RETURN(1);
  }

  if(verbosity >= 1)  
    INFO_message( "Number of time samples is %ld, and voxels %ld in training "
        "dataset.", nt, nvox );

  
  /*---- CRATE TRAINING DATASET ARRAY ----*/
  if( (dsetTrainArray = getAllocateDsetArray(dsetTrain, errorString)) == NULL ) {

    /*  free and return  */
    DSET_unload(dsetTrain);
    RETURN(1);
  }

  /*----- GET MASK ARRAY, IF SELECTED AND DETECT nvox_masked --------*/
  if( options->maskFile[0] ) {
    afniModel.mask_used = MASK_YES; /* JL */
    nvox_masked = 0;

    /* -- open dataset -- */
    if( (dsetMask = THD_open_one_dataset(options->maskFile)) == NULL ) {
      snprintf(errorString, LONG_STRING, "Failed to open mask file: '%s'",
          options->maskFile ); 
      
      /*  free and return - */
      freeDsetArray(dsetTrain, dsetTrainArray);
      DSET_unload(dsetTrain);
      RETURN(1);
    }
    DSET_load(dsetMask);

    /* JL May 2010: Make sure mask and training dataset have the same number of
     * voxels */
    if( DSET_NVOX( dsetMask ) != nvox) {
      snprintf(errorString, LONG_STRING, "Number of voxels in mask file: '%s' "
          "and training dataset: '%s' do not match", options->maskFile, 
          options->trainFile); 

      /* free and return */
      freeDsetArray(dsetTrain, dsetTrainArray);
      DSET_unload(dsetTrain);
      DSET_unload(dsetMask);
      RETURN(1);
    }

   /* JL Mar 2014: Make sure we have only one brick */
    if ( DSET_NUM_TIMES(dsetMask) > 1 ) {
      snprintf(errorString, LONG_STRING, "Mask file: '%s' can only contain "
          "a single brick!", options->maskFile);
      /* free and return */
      freeDsetArray(dsetTrain, dsetTrainArray);
      DSET_unload(dsetTrain);
      DSET_unload(dsetMask);
      RETURN(1);
    }

    /* -- get pointer to mask array -- */
    if( (dsetMaskArrayPtr = getAllocateMaskArray(dsetMask, errorString)) == NULL ) {

      /* free and return */
      freeDsetArray(dsetTrain, dsetTrainArray);
      DSET_unload(dsetTrain);
      RETURN(1);
    }
    DSET_unload(dsetMask);

    /* -- count number of voxels in mask -- */
    for( i=0 ; i<nvox ; ++i ) {
      if( dsetMaskArrayPtr[i] ) nvox_masked++;
    }
    if(verbosity >= 1) {
      INFO_message( "Number of non-zero mask voxels is: %ld\n", nvox_masked );
    }

    /* JL Sep. 2010: Some trivial error checking */
    if( nvox_masked < 2 ) {
      snprintf(errorString, LONG_STRING, "Need at least two voxels in mask "
          "dataset '%s'!", options->maskFile);
      
      /* free and return */
      freeDsetArray(dsetTrain, dsetTrainArray);
      DSET_unload(dsetTrain);
      free(dsetMaskArrayPtr);
      RETURN(1); 
    }
  }
  else if( !(options->outModelNoMask) ) {
    snprintf(errorString, LONG_STRING, "No mask file specified (use -mask "
        "file). If not using a mask file must use option -nomodelmask!"); 

    /* free and return */
    freeDsetArray(dsetTrain, dsetTrainArray);
    DSET_unload(dsetTrain);
    RETURN(1); 
  }
  else afniModel.mask_used = MASK_NO;


  /*----- RETRIEVE AND CHECK LABELS --------------*/
  labels.n = nt;

  /* --- retrieve training labels --- */
  if( getAllocateClassificationLabels(&labels,options->labelFile, 
        options->censorFile, errorString) ) {

    /* free and return */
    freeDsetArray(dsetTrain, dsetTrainArray);
    DSET_unload(dsetTrain);
    if( options->maskFile[0] ) free(dsetMaskArrayPtr);
    RETURN(1);
  }

  /* -- JL Apr. 2010: Added some trivial error checking -- */
  if (labels.n_classes < 2) {
     snprintf(errorString, LONG_STRING, "There is only one class in labelfile: "
         "'%s'. Need at least two!", options->labelFile);
    
    /* free and return */
    freeDsetArray(dsetTrain, dsetTrainArray);
    DSET_unload(dsetTrain);
    if( options->maskFile[0] ) free(dsetMaskArrayPtr);
    freeClassificationLabels(&labels);
    RETURN(1);
  }

  if( labels.n != nt ) {
    snprintf(errorString, LONG_STRING, "Number of labels do not match the "
          "length of the train dataset:\n"
          "   labelfile: '%s' contains %ld labels, but the \n"
          "   trainvol:  '%s' contains %ld bricks.", options->labelFile,
          labels.n, options->trainFile, nt); 
    
    /* free and return */
    freeDsetArray(dsetTrain, dsetTrainArray);
    DSET_unload(dsetTrain);
    if( options->maskFile[0] ) free(dsetMaskArrayPtr);
    freeClassificationLabels(&labels);
    RETURN(1);
  }

  /*----- ALLOCATE afniModel --------------*/
  if( allocateAfniModel(&afniModel, &labels, options, errorString) ) {

    /* free and return */
    freeDsetArray(dsetTrain, dsetTrainArray);
    DSET_unload(dsetTrain);
    if( options->maskFile[0] ) free(dsetMaskArrayPtr);
    freeClassificationLabels(&labels);
    RETURN(1);
  }

  /*----- ALLOCATE censoredTarget --------------*/
  if( (censoredTarget = (LabelType*)malloc(sizeof(LabelType)*labels.n)) == NULL ) {
    snprintf(errorString, LONG_STRING, "train_classification: "
        "Memory allocation for censoredTarget failed");

    /* free and return */
    freeDsetArray(dsetTrain, dsetTrainArray);
    DSET_unload(dsetTrain);
    if( options->maskFile[0] ) free(dsetMaskArrayPtr);
    freeClassificationLabels(&labels);
    freeAfniModel(&afniModel);
    RETURN(1);
  }

  /*----- ALLOCATE maps -----*/
  if( options->modelWeightFile[0] ) {
    if( allocateModelMaps(&maps, (long)labels.n_classes, nvox, options->kernelName) ) {
      snprintf(errorString, LONG_STRING, "train_classification: "
          "Memory allocation for model maps failed!");
    
    /* free and return */
    freeDsetArray(dsetTrain, dsetTrainArray);
    DSET_unload(dsetTrain);
    if( options->maskFile[0] ) free(dsetMaskArrayPtr);
    freeClassificationLabels(&labels);
    freeAfniModel(&afniModel);
    free(censoredTarget);
    RETURN(1);
    }
  }
  
  /*----- SVMLEARN FOR ALL COMBINATIONS OF PARWISE TRAINING --------*/
  /* cc indexes -1 class; dd indexes +1 class - over multiple classes */
  classCount = 0; /* could figure it out from cc and dd, but easier just to keep track */

  for( cc=0 ; cc<labels.n_classes-1; ++cc ) {
    for( dd=cc+1 ; dd<labels.n_classes; ++dd ) {
      if(verbosity >= 1)  { 
         INFO_message("\n++ Preparing classes %d and %d:", 
             labels.class_list[cc], labels.class_list[dd]);

         if (verbosity > 1) MCHECK ; fflush(stdout) ; /* ZSS */
      }   

      if( getCensoredClassTarget(censoredTarget, &sampleCount, &labels,
          cc, dd, TRAIN, errorString) ) { 
      
        /* free and return */
        freeDsetArray(dsetTrain, dsetTrainArray);
        DSET_unload(dsetTrain);
        if( options->maskFile[0] ) free(dsetMaskArrayPtr);
        freeClassificationLabels(&labels);
        freeAfniModel(&afniModel);
        free(censoredTarget);
        if( options->modelWeightFile[0] ) freeModelMaps(&maps);
        RETURN(1);
     }

     if(verbosity >= 1) INFO_message( "SampleCount = %ld\n", sampleCount );

      /*----- ALLOCATE MEMORY FOR SVM-LIGHT ARRAYS -----------*/
      if( (docsClassTrain = allocateDOCs(sampleCount, nvox_masked)) == NULL ) {
        snprintf(errorString, LONG_STRING, "train_classification: "
          "Memory allocation for docsClassTrain failed!");
 
        /* free and return */
        freeDsetArray(dsetTrain, dsetTrainArray); 
        DSET_unload(dsetTrain); 
        if( options->maskFile[0] ) free(dsetMaskArrayPtr);
        freeClassificationLabels(&labels); 
        freeAfniModel(&afniModel); 
        free(censoredTarget);
        if( options->modelWeightFile[0] ) freeModelMaps(&maps);
        RETURN(1);
      }

      if( (classTarget = (LabelType*)malloc(sizeof(LabelType)*sampleCount)) == NULL ) {
        snprintf(errorString, LONG_STRING, "train_classification: "
          "Memory allocation for classTarget failed!");

        /* free and return */
        freeDsetArray(dsetTrain, dsetTrainArray); 
        DSET_unload(dsetTrain); 
        if( options->maskFile[0] ) free(dsetMaskArrayPtr);
        freeClassificationLabels(&labels); 
        freeAfniModel(&afniModel); 
        free(censoredTarget);
        if( options->modelWeightFile[0] ) freeModelMaps(&maps);
        freeDOCs(docsClassTrain, sampleCount);
        RETURN(1);
      }
      
      if( (model=(MODEL *)malloc(sizeof(MODEL))) == NULL ) {
        snprintf(errorString, LONG_STRING, "train_classification: "
          "Memory allocation for model structure failed!");

        /* free and return */
        freeDsetArray(dsetTrain, dsetTrainArray); 
        DSET_unload(dsetTrain); 
        if( options->maskFile[0] ) free(dsetMaskArrayPtr);
        freeClassificationLabels(&labels); 
        freeAfniModel(&afniModel); 
        free(censoredTarget);
        if( options->modelWeightFile[0] ) freeModelMaps(&maps);
        freeDOCs(docsClassTrain, sampleCount);
        free(classTarget);
        RETURN(1);
      }

      /*----- GET TRAINING ARRAY AND CLASSTARGET FOR CURRENT CLASS COMBINATION -----*/
      if( (dsetClassTrainArray = Allocate2DT( sampleCount, nvox)) == NULL ) {
        snprintf(errorString, LONG_STRING, "train_classification: "
          "Memory allocation for dsetClassTrainArray failed!");

        /* free and return */
        freeDsetArray(dsetTrain, dsetTrainArray); 
        DSET_unload(dsetTrain); 
        if( options->maskFile[0] ) free(dsetMaskArrayPtr);
        freeClassificationLabels(&labels); 
        freeAfniModel(&afniModel); 
        free(censoredTarget);
        if( options->modelWeightFile[0] ) freeModelMaps(&maps);
        freeDOCs(docsClassTrain, sampleCount);
        free(classTarget);
        RETURN(1);
      }
 
      getClassTrainArrayAndTarget( dsetTrainArray, censoredTarget,
          dsetClassTrainArray, classTarget, nt, nvox );
        
      /*----- ALPHA FILE OUTPUT -----*/
      /* JL Aug. 2010: Changed how the alphas are written to file.
       * Please read the comments in addToAfniModel for more info  */

      /* ---- MASK DATA AND CONVERT TO SVM-LIGHT DOC STRUCTURE */
      afni_dset_to_svm_doc( docsClassTrain, dsetClassTrainArray, dsetMaskArrayPtr,
          sampleCount, nvox, nvox_masked);

      /* JL Apr. 2010: No training if we want to write out the svm-light
       * formated textfile only */
      if ( !options->docFileOnly[0] ) {

        /* ---- INITIALIZE KERNEL PARAMETERS & TRAIN ----*/
        /* SL & JL Feb. 2009: Added this part to initialize the kernel parameters
         * in case of non-linear kernels. */
        if(kernel_parm->kernel_type == LINEAR) {
          /* don't need the cache if linear*/

          svm_learn_classification( docsClassTrain, classTarget, sampleCount,
              nvox_masked, learn_parm, kernel_parm, NULL, model );
        }
        else {
          /* Always get a new kernel cache. It is not possible to use the
           * same cache for two different training runs */
          kernel_cache_init(&kernel_cache,sampleCount,*kernel_cache_size);
        
          svm_learn_classification( docsClassTrain, classTarget, sampleCount,
              nvox_masked, learn_parm, kernel_parm, &kernel_cache, model );

          /* Free the memory used for the cache. */
          kernel_cache_cleanup(&kernel_cache);
      
        }
        fflush(stdout);

        /* ---- SAVE RESULTS FOR CURRENT CLASS COMBINATION ---*/
        addToAfniModel(&afniModel, model, learn_parm, censoredTarget, options,
            classCount, sampleCount, labels.class_list[cc], labels.class_list[dd]);

        if( options->modelWeightFile[0] ) {
          addToModelMap_bucket(&maps, &afniModel, dsetTrainArray,
              dsetMaskArrayPtr, options->maskFile, classCount);
        }
      }

      /* ---- WRITE DATASET IN SVM-LIGHT FORMATED TEXTFILE  ----*/
      if (options->docFile[0]) {
        if (labels.n_classes == 2) {
          snprintf( docFileName, LONG_STRING, "%s.svml", options->docFile);
        }
        else {
          snprintf( docFileName, LONG_STRING, "%s_%d_%d.svml", options->docFile,
              labels.class_list[cc], labels.class_list[dd] );
        }
          write_svmLight_doc(docsClassTrain, sampleCount, nvox_masked, classTarget,
          docFileName, VERSION_SVMLIGHT);
      }

      ++classCount;

      if( !options->docFileOnly[0] ) freeModel(model, &afniModel, TRAIN);
      freeDOCs(docsClassTrain, sampleCount);
      free2DT(dsetClassTrainArray, sampleCount);
      free(classTarget);
    }
  }

  /* ----- WRITE MODEL AND BUCKET TO DISC ----- */
  if( !options->docFileOnly[0] && !options->noModelOut ) {
    /* JL May 2010: Modified writeModelBrick to write the model and the mask into
     * a single dataset */
    if( writeModelBrik(&afniModel, dsetTrain, dsetTrainArray, dsetMaskArrayPtr, options,
        options->modelFile, argc, argv, errorString) ) {

      /* free and return */
      freeDsetArray(dsetTrain, dsetTrainArray); 
      DSET_unload(dsetTrain); 
      if( options->maskFile[0] ) free(dsetMaskArrayPtr);
      freeClassificationLabels(&labels); 
      freeAfniModel(&afniModel); 
      free(censoredTarget);
      if( options->modelWeightFile[0] ) freeModelMaps(&maps);
      RETURN(1);
    }
  }

  if( (options->modelWeightFile[0]) && (!options->docFileOnly[0]) ) {
    if( writeModelMap_bucket(&maps, dsetMaskArrayPtr, dsetTrain, options->maskFile,
        options->modelWeightFile, afniModel.b, afniModel.combinations,
        options, argc, argv, errorString) ) { 
      
      /* free and return */
      freeDsetArray(dsetTrain, dsetTrainArray); 
      DSET_unload(dsetTrain); 
      if( options->maskFile[0] ) free(dsetMaskArrayPtr);
      freeClassificationLabels(&labels);
      freeAfniModel(&afniModel); 
      free(censoredTarget);
      freeModelMaps(&maps);
      RETURN(1);
    }
  }
  
  /* free memory */
  freeDsetArray(dsetTrain, dsetTrainArray); 
  DSET_unload(dsetTrain); 
  if( options->maskFile[0] ) free(dsetMaskArrayPtr);
  freeClassificationLabels(&labels); 
  freeAfniModel(&afniModel); 
  free(censoredTarget);
  if( options->modelWeightFile[0] ) freeModelMaps(&maps);

  RETURN(0);
}

/* JL May 2009: Added this function for sv-regression.
 * It is very similar to train_classification() (a lot of code has been recycled). 
 * However, major differences are:
 *
 *    - No need for multi-class
 *    - New function to read in the labelfile (getAllocateRegressionLabels())
 *    - New function to get the array with training data
 *      (getAllocateCensoredRegressionArray())
 *    - Using svm-light's function call: svm_learn_regression() instead of
 *      svm_learn_classification()
 */ 
int train_regression(MODEL *model, LEARN_PARM *learn_parm, 
    KERNEL_PARM *kernel_parm, long *kernel_cache_size, ASLoptions *options,
    THD_3dim_dataset *dsetTrain, THD_3dim_dataset *dsetMask,
    MaskType *dsetMaskArrayPtr, int argc, char **argv, char *errorString)
{
  
  LABELS labels;
  AFNI_MODEL afniModel;	         /* holds everything required to write out
                                    model.Head */
  MODEL_MAPS maps;               /* holds the maps (e.g., weight-vector maps)
                                    for the bucket */

  LabelType *target       = NULL; /* array to hold labels for svm-light */

  DatasetType**
    dsetTrainArray        = NULL; /* array to hold training dataset values */

  DatasetType**
   dsetTrainArrayCensored = NULL; /* array to hold training dataset values */

  DOC    *docsTrain       = NULL; /* svm-light data structure used for training */
  KERNEL_CACHE kernel_cache;      /* svm-light data structure holding kernel
                                   * paramters */


  long nt                 = 0;   /* number of time points in TRAIN dataset */
  long nvox               = 0;   /* number of voxels per time point in TRAIN
                                    dataset */
  long nvox_masked        = 0;    /* number of voxels in mask dataset */

  long sampleCount        = 0;    /* number of samples used in training */

  long i                  = 0;
  char docFileName[LONG_STRING];  /* nameing of svm-light textfile  output */


  ENTRY("train_regression");

  /* JL June 2011: Modified error handling: Passing error string as argument
   * to the calling function, allocated memory is freed, RETURN(1) 
   * instead of ERROR_exit.
   *
   * JL March 2014: Removed byte data type restriction for mask dataset.
   *
   */

  if (verbosity >= 1) INFO_message("\n++ REGRESSION (training):\n++");


  /*----- LOAD TRAINING DATA ---*/
  if( dsetTrain == NULL ) {
    if( (dsetTrain = THD_open_one_dataset(options->trainFile)) == NULL ) {
      snprintf(errorString, LONG_STRING, "Failed to open training dataset: %s",
          options->trainFile ); RETURN(1);
    }
    DSET_load( dsetTrain );
  }
  else {
    if( !DSET_LOADED(dsetTrain) ) {
      if( (dsetTrain = THD_open_one_dataset(options->trainFile)) == NULL ) {
      snprintf(errorString, LONG_STRING, "Failed to open training dataset: %s",
          options->trainFile ); RETURN(1);
    }
    DSET_load( dsetTrain );
    }
  }

  nt = DSET_NUM_TIMES( dsetTrain );
  nvox = DSET_NVOX( dsetTrain );
  nvox_masked = nvox; /* this will be modified later if mask used */

  if(verbosity >= 1) {
    INFO_message( "Number of time samples is %ld, and voxels %ld in training "
        "dataset.", nt, nvox );
  }

  /*------ GET MASK ARRAY, IF SELECTED AND DETECT nvox_masked ---*/
  if( options->maskFile[0] ) {
    afniModel.mask_used = MASK_YES; /* JL */
    nvox_masked = 0;
    
    if( (dsetMask = THD_open_one_dataset(options->maskFile)) == NULL ) {
      snprintf(errorString, LONG_STRING,
          "Failed to open mask file: %s", options->maskFile );

      /* free and return */
      DSET_unload(dsetTrain);
      RETURN(1);
    }
    DSET_load(dsetMask);

    /* JL April 2014: Make sure mask and training dataset have the same number of
     * voxels */
    if( DSET_NVOX( dsetMask ) != nvox) {
      snprintf(errorString, LONG_STRING, "Number of voxels in mask file: '%s' "
          "and training dataset: '%s' do not match", options->maskFile,
          options->trainFile);

      /* free and return */
      DSET_unload(dsetTrain);
      DSET_unload(dsetMask);
      RETURN(1);
    }

    /* JL Mar 2014: Make sure we have only one brick */
    if ( DSET_NUM_TIMES(dsetMask) > 1 ) {
      snprintf(errorString, LONG_STRING, "Mask file: '%s' can only contain "
          "a single brick!", options->maskFile);
      /* free and return */
      DSET_unload(dsetTrain);
      DSET_unload(dsetMask);
      RETURN(1);
    }

    if( (dsetMaskArrayPtr = getAllocateMaskArray(dsetMask, errorString)) == NULL ) {

      /* free and return */
      DSET_unload(dsetTrain);
      RETURN(1);
    }
    DSET_unload(dsetMask);


    for( i=0 ; i<nvox ; ++i ) {
      if( dsetMaskArrayPtr[i] ) nvox_masked++;
    }
    if(verbosity >= 1) 
      INFO_message( "Number of non-zero mask voxels is: %ld\n", nvox_masked );
  }
  else if( !(options->outModelNoMask) ) {
    snprintf(errorString, LONG_STRING, 
        "No mask file specified (use -mask file). "
        "If not using a mask file must use option -nomodelmask");
    
    /* free and return */
    DSET_unload(dsetTrain);
    RETURN(1);
  }
  else afniModel.mask_used = MASK_NO;

  /*----- RETRIEVE LABELS AND GET SVM-LIGHT TARGET ----*/
  if( (getAllocateRegressionLabelsAndTarget(&labels, &target, 
      options->labelFile, options->censorFile, errorString)) ) {

    /* free and return */
    DSET_unload(dsetTrain);
    if( options->maskFile[0] ) free(dsetMaskArrayPtr);
    RETURN(1);
  }
    
  if( labels.n != nt ) {
    snprintf(errorString, LONG_STRING, 
        "Number of labels %ld in labelfile: %s does not match\n"
        "   number of timepoints %ld in train dataset: %s!", 
        labels.n, options->labelFile, nt, options->trainFile);
    
    /* free and return */
    DSET_unload(dsetTrain);
    if( options->maskFile[0] ) free(dsetMaskArrayPtr);
    freeRegressionLabelsAndTarget(&labels, target);
    RETURN(1);
  }
  
  sampleCount=labels.n-labels.n_cnsrs; /* number of uncensored timepoints */

  /*----  GET TRAINING ARRAY -----*/
  if( (dsetTrainArray = getAllocateDsetArray(dsetTrain, errorString)) == NULL ) {
    
    /* free and return */
    DSET_unload(dsetTrain);
    if( options->maskFile[0] ) free(dsetMaskArrayPtr);
    freeRegressionLabelsAndTarget(&labels, target);
    RETURN(1);
  }

  /*----- GET TRAINING ARRAY WITHOUT CENSORED TIMEPOINTS -----*/
  if( (dsetTrainArrayCensored = getAllocateCensoredRegressionArray(dsetTrainArray,
      &labels, nvox)) == NULL ) {

    snprintf(errorString, LONG_STRING, "train_regression"
        "Memory allocation for dsetTrainArrayCensored failed!");
    
    /* free and return */
    freeDsetArray(dsetTrain, dsetTrainArray);
    DSET_unload(dsetTrain);
    if( options->maskFile[0] ) free(dsetMaskArrayPtr);
    freeRegressionLabelsAndTarget(&labels, target);
    RETURN(1);
  }

  /*----- ALLOCATE afniModel -----*/
  if( allocateAfniModel(&afniModel, &labels, options, errorString) ) {
    
    /* free and return */
    freeDsetArray(dsetTrain, dsetTrainArray);
    DSET_unload(dsetTrain);
    if( options->maskFile[0] ) free(dsetMaskArrayPtr);
    freeCensoredRegressionArray(dsetTrainArrayCensored, &labels);
    freeRegressionLabelsAndTarget(&labels, target);
    RETURN(1);
  }

  /*----- ALLOCATE maps ------*/
  if( options->modelWeightFile[0] ) {
    if( allocateModelMaps(&maps, (long)labels.n_classes, nvox, options->kernelName) ) {
      snprintf(errorString, LONG_STRING, "train_regression: "
        "Memory allocation for model maps failed!");

      /* free and return */
      freeDsetArray(dsetTrain, dsetTrainArray);
      DSET_unload(dsetTrain);
      if( options->maskFile[0] ) free(dsetMaskArrayPtr); 
      freeCensoredRegressionArray(dsetTrainArrayCensored, &labels);
      freeRegressionLabelsAndTarget(&labels, target);
      freeAfniModel(&afniModel);
      RETURN(1);
    }
  }

  /*----- ALLOCATE SVM-LIGHT DOCs ------*/
  if( (docsTrain = allocateDOCs(sampleCount, nvox_masked))  == NULL ) {
    snprintf(errorString, LONG_STRING, "train_classification: "
          "Memory allocation for docsTrain failed!");
 
    /* free and return */
    freeDsetArray(dsetTrain, dsetTrainArray);
    DSET_unload(dsetTrain);
    if( options->maskFile[0] ) free(dsetMaskArrayPtr);
    freeCensoredRegressionArray(dsetTrainArrayCensored, &labels);
    freeRegressionLabelsAndTarget(&labels, target);
    freeAfniModel(&afniModel);
    if( options->modelWeightFile[0] ) freeModelMaps(&maps);
    RETURN(1);
  }

  /*----- CONVERT TRAINING ARRAY TO SVM-LIGHT DOC STRUCTURE ------*/
  afni_dset_to_svm_doc( docsTrain, dsetTrainArrayCensored, dsetMaskArrayPtr,
      sampleCount, nvox, nvox_masked );

  /* JL Apr. 2010: No training if we want to write out the svm-light
   * formated textfile only */
  if ( !options->docFileOnly[0] ) {

    /*----- PERFORM THE SV-REGRESSION -----*/
    if ( !strcmp(options->kernelName, "linear") ) {

      svm_learn_regression ( docsTrain, target, sampleCount, nvox_masked,
          learn_parm, kernel_parm, NULL, model);

    }
    else { /* non-linear kernel */
      kernel_cache_init(&kernel_cache, 2*sampleCount, *kernel_cache_size);

      svm_learn_regression ( docsTrain, target, sampleCount, nvox_masked,
          learn_parm, kernel_parm, &kernel_cache, model);

      /* Free the memory used for the cache. */
      kernel_cache_cleanup(&kernel_cache);
    }
  }

  /*----- UPDATE AFNI-MODEL -----*/
  if ( !options->docFileOnly[0] ) {
    addToAfniModel(&afniModel, model, learn_parm,  &(labels.cnsrs[0]), options,
      0, sampleCount, 0, 0);
  }

  /*---- UPDATE MODEL-MAPS -----*/
  if( (options->modelWeightFile[0]) && (!options->docFileOnly[0]) ) {
    addToModelMap_bucket(&maps, &afniModel, dsetTrainArray, dsetMaskArrayPtr,
        options->maskFile, 0);
  }

  /*---- WRITE OUTPUT FILES TO DISC ----*/
  /* might not be necessary if testing and training are performed all at once */

  /* --- write afni model --- */
  if ( !options->docFileOnly[0] && !options->noModelOut ) {
    /* JL May 2010: Modified writeModelBrick to write the model and the mask into
     * a single dataset */
    if( writeModelBrik(&afniModel, dsetTrain, dsetTrainArray, dsetMaskArrayPtr, options,
            options->modelFile, argc, argv, errorString) ) {
    
      /* free and return */
      freeDsetArray(dsetTrain, dsetTrainArray);
      DSET_unload(dsetTrain);
      if( options->maskFile[0] ) free(dsetMaskArrayPtr);
      freeCensoredRegressionArray(dsetTrainArrayCensored, &labels);
      freeRegressionLabelsAndTarget(&labels, target);
      freeModel(model, &afniModel, TRAIN);
      freeAfniModel(&afniModel);
      if( options->modelWeightFile[0] ) freeModelMaps(&maps);
      freeDOCs(docsTrain, sampleCount);
      RETURN(1);
    }
  }

  /* --- write model maps --- */
  if( (options->modelWeightFile[0]) && (!options->docFileOnly[0]) ) {
    if( writeModelMap_bucket(&maps, dsetMaskArrayPtr, dsetTrain, options->maskFile, 
        options->modelWeightFile, afniModel.b, (long)afniModel.combinations,
        options, argc, argv, errorString) ) {
    
      /* free and return */
      freeDsetArray(dsetTrain, dsetTrainArray);
      DSET_unload(dsetTrain);
      if( options->maskFile[0] ) free(dsetMaskArrayPtr);
      freeCensoredRegressionArray(dsetTrainArrayCensored, &labels);
      freeRegressionLabelsAndTarget(&labels, target);
      freeModel(model, &afniModel, TRAIN);
      freeAfniModel(&afniModel);
      freeModelMaps(&maps);
      freeDOCs(docsTrain, sampleCount);
      RETURN(1);
    }
  }

  /* --- svm-light textfile ---*/
  if (options->docFile[0]) {
    snprintf( docFileName, LONG_STRING, "%s.svml", options->docFile);
    write_svmLight_doc(docsTrain, sampleCount, nvox_masked, target, 
        docFileName, VERSION_SVMLIGHT);
  }
  
  /*----- FREE MEMORY -----*/
  freeDsetArray(dsetTrain, dsetTrainArray);
  DSET_unload(dsetTrain);
  if( options->maskFile[0] ) free(dsetMaskArrayPtr);
  freeCensoredRegressionArray(dsetTrainArrayCensored, &labels);
  freeRegressionLabelsAndTarget(&labels, target);
  if (!options->docFileOnly[0]) freeModel(model, &afniModel, TRAIN);
  freeAfniModel(&afniModel);
  if( options->modelWeightFile[0] ) freeModelMaps(&maps);
  freeDOCs(docsTrain, sampleCount);


  RETURN(0);
}

/* JL Sep. 2009: Error checking for options with argument. 
 * Avoid out of bound error if last option and no argument
 * ppi = ++i */
int ppi (int argc, int i, char *optionString)
{
  
  ENTRY("ppi");

  if ( optionString[strlen(optionString)+1] == '-' ) {
    ERROR_exit("Argument for %s must not start with '-'!\n", optionString);
  }
  else if ( i<argc-1 ) RETURN(++i);
  else ERROR_exit("No argument after %s!", optionString);

  RETURN(++i);
}



int input_parse(int argc, char *argv[], long *main_verbosity,
    long *kernel_cache_size, LEARN_PARM *learn_parm, KERNEL_PARM *kernel_parm,
    ASLoptions* optionsData, enum modes *mode, int *svm_type, char *errorString)
{
  long i;
  char type[200];       /* svm-light type (REGRESSION, CLASSIFICATION or RANKING) */
  int parseFlag = 0;    /*ZSS: init to 0*/
  int aFlag = 0; 
  int alphaFlag = 0;
  int tFlag = 0;       /* JL: flag for svm-light option -t (kernel_type) */
  int zFlag = 0;       /* JL: flag for svm-light option -z (type) */
  int typeFlag = 0;    /* JL: flag for 3dsvm options -type */
  *mode = NOTHING;


  ENTRY("input_parse");
  

  /* TODO: The definitions bellow should be a header entry ...*/
  /* svm-light defaults */
  strncpy (learn_parm->predfile, "trans_predictions", 200);
  strncpy (learn_parm->alphafile, "", 200);
  (*main_verbosity)=1;
  verbosity=1; /*svm_light verbosity which is a little tricky as a static
    global and now the primary variable for functions in this file. */
  learn_parm->biased_hyperplane=1;
  learn_parm->remove_inconsistent=0;
  learn_parm->skip_final_opt_check=0;
  learn_parm->svm_maxqpsize=10;
  learn_parm->svm_newvarsinqp=0;
  learn_parm->svm_iter_to_shrink=-9999;
  (*kernel_cache_size)=40;
  learn_parm->svm_c=100.0;
  learn_parm->eps=0.1;
  learn_parm->transduction_posratio=-1.0;
  learn_parm->svm_costratio=1.0;
  learn_parm->svm_costratio_unlab=1.0;
  learn_parm->svm_unlabbound=1E-5;
  learn_parm->epsilon_crit=0.001;
  learn_parm->epsilon_a=1E-15;
  learn_parm->compute_loo=0;
  learn_parm->rho=1.0;
  learn_parm->xa_depth=0;
  /* JL July 2011: Added maximum number of iterations */
  learn_parm->max_iterations=1000000;
  kernel_parm->kernel_type=LINEAR;
  kernel_parm->poly_degree=3; 
  kernel_parm->rbf_gamma=1.0; 
  kernel_parm->coef_lin=1.0;
  kernel_parm->coef_const=1.0;
  strncpy(kernel_parm->custom,"empty", 50);
  strncpy(type,"c", 200);

  /* 3dsvm specific */
  strncpy(optionsData->labelFile, "",       LONG_STRING);
  strncpy(optionsData->censorFile, "",      LONG_STRING);
  strncpy(optionsData->trainFile, "",       LONG_STRING);
  strncpy(optionsData->maskFile, "",        LONG_STRING);
  strncpy(optionsData->modelFile, "",       LONG_STRING);
  strncpy(optionsData->docFile, "",         LONG_STRING);
  strncpy(optionsData->docFileOnly, "",     LONG_STRING);
  strncpy(optionsData->multiclass, "",      LONG_STRING);
  strncpy(optionsData->kernelName, "",      LONG_STRING);
  strncpy(optionsData->modelAlphaFile, "",  LONG_STRING);
  strncpy(optionsData->modelWeightFile, "", LONG_STRING);
  strncpy(optionsData->testFile, "",        LONG_STRING);
  strncpy(optionsData->testLabelFile, "",   LONG_STRING);
  strncpy(optionsData->predFile, "pred",    LONG_STRING);
  strncpy(optionsData->svmType, "",         LONG_STRING);
  strncpy(optionsData->rtIP, "",            LONG_STRING);
  optionsData->outModelNoMask = 0;
  optionsData->noModelOut     = 0;
  optionsData->noPredDetrend  = 0;
  optionsData->classout       = 0;
  optionsData->noPredCensor   = 0;
  optionsData->noPredScale    = 0;
  optionsData->linearWmap     = 0;
  optionsData->rtTrain        = 0;
  optionsData->rtTest         = 0;
  optionsData->rtPort         = 0;

  for( i=1; i<argc; ++i ) { 
    parseFlag = 0;

    if ( argv[i][0] != '-' ) { snprintf(errorString, LONG_STRING,
        "Option %s must start with '-'!", argv[i]); RETURN(1); }

    /* svm-light options: */
    if( !strcmp(argv[i],"-z") ) { parseFlag=1; i=ppi(argc,i,argv[i]); 
                                  strncpy(type,argv[i], 200); zFlag=1; }
    if( !strcmp(argv[i],"-v") ) { parseFlag=1; i=ppi(argc,i,argv[i]); 
                                  (*main_verbosity)=atol(argv[i]); verbosity = *main_verbosity; }
    if( !strcmp(argv[i],"-b") ) { parseFlag=1; i=ppi(argc,i,argv[i]); 
                                  learn_parm->biased_hyperplane=atol(argv[i]); }
    if( !strcmp(argv[i],"-i") ) { parseFlag=1; i=ppi(argc,i,argv[i]); 
                                  learn_parm->remove_inconsistent=atol(argv[i]); }
    if( !strcmp(argv[i],"-f") ) { parseFlag=1; i=ppi(argc,i,argv[i]); 
                                  learn_parm->skip_final_opt_check=!atol(argv[i]); }
    if( !strcmp(argv[i],"-q") ) { parseFlag=1; i=ppi(argc,i,argv[i]); 
                                  learn_parm->svm_maxqpsize=atol(argv[i]); }
    if( !strcmp(argv[i],"-n") ) { parseFlag=1; i=ppi(argc,i,argv[i]); 
                                  learn_parm->svm_newvarsinqp=atol(argv[i]); }
    if( !strcmp(argv[i],"-h") ) { parseFlag=1; i=ppi(argc,i,argv[i]); 
                                  learn_parm->svm_iter_to_shrink=atol(argv[i]); }
    if( !strcmp(argv[i],"-m") ) { parseFlag=1; i=ppi(argc,i,argv[i]); 
                                  (*kernel_cache_size)=atol(argv[i]); }
    if( !strcmp(argv[i],"-c") ) { parseFlag=1; i=ppi(argc,i,argv[i]); 
                                  learn_parm->svm_c=atof(argv[i]); }
    if( !strcmp(argv[i],"-w") ) { parseFlag=1; i=ppi(argc,i,argv[i]); 
                                  learn_parm->eps=atof(argv[i]); }
    if( !strcmp(argv[i],"-p") ) { parseFlag=1; i=ppi(argc,i,argv[i]); 
                                  learn_parm->transduction_posratio=atof(argv[i]); }
    if( !strcmp(argv[i],"-j") ) { parseFlag=1; i=ppi(argc,i,argv[i]); 
                                  learn_parm->svm_costratio=atof(argv[i]); }
    if( !strcmp(argv[i],"-e") ) { parseFlag=1; i=ppi(argc,i,argv[i]); 
                                  learn_parm->epsilon_crit=atof(argv[i]); }
    if( !strcmp(argv[i],"-o") ) { parseFlag=1; i=ppi(argc,i,argv[i]); 
                                  learn_parm->rho=atof(argv[i]); }
    if( !strcmp(argv[i],"-k") ) { parseFlag=1; i=ppi(argc,i,argv[i]); 
                                  learn_parm->xa_depth=atol(argv[i]); }
    if( !strcmp(argv[i],"-x") ) { parseFlag=1; i=ppi(argc,i,argv[i]); 
                                  learn_parm->compute_loo=atol(argv[i]); }
    if( !strcmp(argv[i],"-t") ) { parseFlag=1; i=ppi(argc,i,argv[i]); 
                                  kernel_parm->kernel_type=atol(argv[i]); tFlag=1; }
    if( !strcmp(argv[i],"-d") ) { parseFlag=1; i=ppi(argc,i,argv[i]); 
                                  kernel_parm->poly_degree=atol(argv[i]); }
    if( !strcmp(argv[i],"-g") ) { parseFlag=1; i=ppi(argc,i,argv[i]); 
                                  kernel_parm->rbf_gamma=atof(argv[i]); }
    if( !strcmp(argv[i],"-s") ) { parseFlag=1; i=ppi(argc,i,argv[i]); 
                                  kernel_parm->coef_lin=atof(argv[i]); }
    if( !strcmp(argv[i],"-r") ) { parseFlag=1; i=ppi(argc,i,argv[i]); 
                                  kernel_parm->coef_const=atof(argv[i]); }
    if( !strcmp(argv[i],"-u") ) { parseFlag=1; i=ppi(argc,i,argv[i]); 
                                  strncpy(kernel_parm->custom,argv[i], CSV_STRING); }
    if( !strcmp(argv[i],"-l") ) { parseFlag=1; i=ppi(argc,i,argv[i]); 
                                  strncpy(learn_parm->predfile,argv[i], 200); }
    
    /* JL July 2011: Added maximum number of iterations. Thanks CC */
    if( !strcmp(argv[i],"-max_iterations") ) { parseFlag=1; i=ppi(argc,i,argv[i]); 
                                  learn_parm->max_iterations=atol(argv[i]); }
    /* if( !strcmp(argv[i],"-a") ) { parseFlag=1; i=ppi(argc,i,argv[i]); 
     *                               strcpy(learn_parm->alphafile,argv[i]); }
     *
     * as an easy solution, we are fixing the svmLight's output file name and 
     * letting 3dsvm write out the desired file */
         
    /* 3dsvm options with arguments: */
    if( !strcmp(argv[i],"-type") )          { parseFlag=1; i=ppi(argc,i,argv[i]); 
                                              strncpy(optionsData->svmType,
                                              argv[i],  LONG_STRING); typeFlag=1; }
    if( !strcmp(argv[i],"-a") )             { parseFlag=1; i=ppi(argc,i,argv[i]); 
                                              strncpy(optionsData->modelAlphaFile,
                                              argv[i], LONG_STRING); aFlag=1;}
    if( !strcmp(argv[i],"-alpha") )         { parseFlag=1; i=ppi(argc,i,argv[i]); 
                                              strncpy(optionsData->modelAlphaFile,
                                              argv[i], LONG_STRING); alphaFlag=1;}
    if( !strcmp(argv[i],"-trainvol") )      { parseFlag=1; i=ppi(argc,i,argv[i]); 
                                              strncpy(optionsData->trainFile,
                                              argv[i], LONG_STRING); }
    if( !strcmp(argv[i],"-testvol") )       { parseFlag=1; i=ppi(argc,i,argv[i]); 
                                              strncpy(optionsData->testFile,
                                              argv[i], LONG_STRING); }
    if( !strcmp(argv[i],"-multiclass") )    { parseFlag=1; i=ppi(argc,i,argv[i]); 
                                              strncpy(optionsData->multiclass,
                                              argv[i], LONG_STRING); }
    if( !strcmp(argv[i],"-trainlabels") )   { parseFlag=1; i=ppi(argc,i,argv[i]); 
                                              strncpy(optionsData->labelFile,
                                              argv[i], LONG_STRING); }
    if( !strcmp(argv[i],"-censor") )        { parseFlag=1; i=ppi(argc,i,argv[i]); 
                                              strncpy(optionsData->censorFile,
                                              argv[i], LONG_STRING); }
    if( !strcmp(argv[i],"-mask") )          { parseFlag=1; i=ppi(argc,i,argv[i]); 
                                              strncpy(optionsData->maskFile,
                                              argv[i], LONG_STRING); }
    if( !strcmp(argv[i],"-model") )         { parseFlag=1; i=ppi(argc,i,argv[i]); 
                                              strncpy(optionsData->modelFile,
                                              argv[i], LONG_STRING); }
    if( !strcmp(argv[i],"-bucket") )        { parseFlag=1; i=ppi(argc,i,argv[i]); 
                                              strncpy(optionsData->modelWeightFile,
                                              argv[i], LONG_STRING); }
    if( !strcmp(argv[i],"-testlabels") )    { parseFlag=1; i=ppi(argc,i,argv[i]); 
                                              strncpy(optionsData->testLabelFile,
                                              argv[i], LONG_STRING); }
    if( !strcmp(argv[i],"-predictions") )   { parseFlag=1; i=ppi(argc,i,argv[i]); 
                                              strncpy(optionsData->predFile,
                                              argv[i], LONG_STRING); }
    if( !strcmp(argv[i],"-pred") )          { parseFlag=1; i=ppi(argc,i,argv[i]);
                                              strncpy(optionsData->predFile,
                                              argv[i], LONG_STRING); }
    if( !strcmp(argv[i],"-docout") )        { parseFlag=1; i=ppi(argc,i,argv[i]); 
                                              strncpy(optionsData->docFile,
                                              argv[i], LONG_STRING); }
    if( !strcmp(argv[i],"-doconly") )       { parseFlag=1; i=ppi(argc,i,argv[i]);
                                               strncpy(optionsData->docFileOnly,
                                               argv[i], LONG_STRING); }
    /* for kernel below, using svm-light options for kernel parameters */
    if( !strcmp(argv[i],"-kernel") )        { parseFlag=1; i=ppi(argc,i,argv[i]); 
                                              strncpy(optionsData->kernelName,
                                              argv[i], LONG_STRING); }

    if( !strcmp(argv[i],"-stim_ip") )        { parseFlag=1; i=ppi(argc,i,argv[i]);
                                              strncpy(optionsData->rtIP,
                                              argv[i], LONG_STRING); }

    if( !strcmp(argv[i],"-stim_port") )      { parseFlag=1; i=ppi(argc,i,argv[i]);
                                             optionsData->rtPort=atoi(argv[i]);}

    /* AFNI, 3dsvm options without arguments: */
    if( !strcmp(argv[i],"-trace")) { parseFlag = 1; 
      #ifdef USE_TRACING 
        DBG_trace = 1; 
      #endif 
    }
    if( !strcmp(argv[i],"-no_memcheck") )   { pause_mcw_malloc(); /* ZSS */ }
    if( !strcmp(argv[i],"-nomodelmask") )   { parseFlag=1; optionsData->outModelNoMask = 1; }
    if( !strcmp(argv[i],"-nomodelfile") )   { parseFlag=1; optionsData->noModelOut = 1; } /* JL Oct. 2017 */
    if( !strcmp(argv[i],"-nodetrend") )     { parseFlag=1; optionsData->noPredDetrend = 1; }
    if( !strcmp(argv[i],"-classout") )      { parseFlag=1; optionsData->classout = 1; } 
    if( !strcmp(argv[i],"-nopredcensored") ){ parseFlag=1; optionsData->noPredCensor = 1; }
    if( !strcmp(argv[i],"-nopredscale") )   { parseFlag=1; optionsData->noPredScale = 1; }
    if( !strcmp(argv[i],"-wout") )          { parseFlag=1; optionsData->linearWmap = 1; }

    if( !strcmp(argv[i],"-change_summary")) { print_version(); RETURN(0); }
    if( !strcmp(argv[i],"-version"))        { print_version(); RETURN(0); }
    if( !strcmp(argv[i],"-HELP") )          { printf("%s", advanced_helpstring); RETURN(0); }
    if( !strcmp(argv[i],"-rt_train") )      { parseFlag = 1; optionsData->rtTrain = 1; }
    if( !strcmp(argv[i],"-rt_test") )       { parseFlag = 1; optionsData->rtTest = 1;  }
    if( !strcmp(argv[i],"-help") )
    {  
      printf("%s", cl_helpstring); 
      printf("\n\n-------------------- SVM-light learn help -----------------------------\n");
      print_help_learn();
      printf("\n\n-------------------- SVM-light classify help -----------------------------\n");
      print_help_classify();
      printf("\n\n--------------------------------------------------------------------------\n");
      printf("%s", contribution_string); 

      RETURN(0); 
    }
  
    if( !parseFlag ) {
      snprintf(errorString, LONG_STRING, "Illegal option: %s !", argv[i]);
      RETURN(1);
    }
  }

  if( argc == 1 ) {
    printf("%s", cl_helpstring);
    printf("\n\n-------------------- SVM-light learn help -----------------------------\n");
    print_help_learn();
    printf("\n\n-------------------- SVM-light classify help -----------------------------\n");
    print_help_classify();
    printf("\n\n--------------------------------------------------------------------------\n");
    printf("%s", contribution_string);
    RETURN(0);
  }

  /* JL May 2009: Some error checking and initialization for svm learn type */
  if ( (zFlag) && (optionsData->svmType[0]) ) {
   WARNING_message("Both svm-light option: -z and 3dsvm option: -type "
       "were used.\n   Using -type %s\n", optionsData->svmType);
  }
  
  if( optionsData->svmType[0] ) {
    if( !strcmp(optionsData->svmType, "classification") ) {
        learn_parm->type=CLASSIFICATION;
        *svm_type=CLASSIFICATION;
        strncpy(type,"c", 200);
    }
    else if ( !strcmp(optionsData->svmType, "regression") ) {
      learn_parm->type=REGRESSION;
      *svm_type=REGRESSION; 
      strncpy(type,"r", 200);
    }
    else {
      snprintf(errorString, LONG_STRING, "Unknown option -type %s!\n",
       optionsData->svmType); RETURN(1);
    }
  }
  else 
    strncpy(optionsData->svmType,"classification", LONG_STRING);
  /* (matches default for learn_parm->type) */
    
  /* the following corresponds to -t option in SVM-Light's original logic */
  if(strcmp(type,"c")==0) {
    learn_parm->type=CLASSIFICATION;
    *svm_type=CLASSIFICATION;
    strncpy(optionsData->svmType, "classification", LONG_STRING);
  }
  else if(strcmp(type,"r")==0) {
    learn_parm->type=REGRESSION;
    *svm_type=REGRESSION;
    strncpy(optionsData->svmType, "regression", LONG_STRING);
  }
  else if(strcmp(type,"p")==0) {
    snprintf(errorString, LONG_STRING, "Svm-light option -z p (preference ranking) "
        "is not supported yet!"); RETURN(1);
    /* svm_type=RANKING; */
    /* learn_parm->type=RANKING; */
  }
  else {
      snprintf(errorString, LONG_STRING, "Unknown type '%s': Valid types are 'c' "
          "(classification), 'r' (regession), and 'p' (preference ranking).",
          type); RETURN(1);
  }

  /* JL Feb. 2009: Some error checking and initialization for kernel options */
  if ( tFlag && optionsData->kernelName[0] ) {
   WARNING_message("Both svm-light option: -t and 3dsvm option: -kernel "
       "were used.\n   Using -kernel %s\n", optionsData->kernelName);
  }

  if ( optionsData->kernelName[0] ) {
    if ( !strcmp(optionsData->kernelName, "complex1") ) {
      kernel_parm->kernel_type = CUSTOM;
      strncpy(kernel_parm->custom, "complex1", 50);
    }
    else if ( !strcmp(optionsData->kernelName, "linear") ) {
     kernel_parm->kernel_type = LINEAR;
    }
    else if ( !strcmp(optionsData->kernelName, "polynomial") ) {
      kernel_parm->kernel_type = POLY;
    }
    else if ( !strcmp(optionsData->kernelName, "rbf") ) {
      kernel_parm->kernel_type = RBF;
    }
    else if ( !strcmp(optionsData->kernelName, "sigmoid") ) {
      kernel_parm->kernel_type = SIGMOID;
    }
    else {
      snprintf(errorString, LONG_STRING, "Unknown kernel option -kernel %s\n",
          optionsData->kernelName); RETURN(1);
    }
  }
  else
    strncpy(optionsData->kernelName, "linear", LONG_STRING);
    /* (matches default for kernel_type) */


  /* JL Sep. 2010: Error checking an initialization of real-time options */
  if( (optionsData->rtTrain) && (optionsData->rtTest) ) {
    snprintf(errorString, LONG_STRING,
        "Combined training and testing is not supported in real time!");
    RETURN(1);
  }
  /* real-time regression is not implemented yet */
  /*if( (optionsData->rtTrain) || (optionsData->rtTest) ) {
    if( strncmp(optionsData->svmType, "classification", LONG_STRING) ) {
      snprintf(errorString, LONG_STRING,
          "Sorry, Only classification is supported in real time!");
      RETURN(1);
    }
  } */

  if( optionsData->rtTrain ) {
    *mode = RT_TRAIN;
    strncpy(optionsData->trainFile, "rt_data", LONG_STRING);

    if( !optionsData->labelFile[0] ) {
      snprintf(errorString, LONG_STRING,
          "Must specify a timeseries labelfile for training in real time!");
      RETURN(1);
    }
    if( (!optionsData->maskFile[0])  && (!optionsData->outModelNoMask) ) {
      snprintf(errorString, LONG_STRING,
          "Must specify a mask file for training in real time!\n"
          "For training without a mask :\n" 
          "  set environment variable: AFNI_3DSVM_NOMASK=YES\n");
      RETURN(1);
    }
    if( !optionsData->modelFile[0] && !optionsData->noModelOut ) {
      snprintf(errorString, LONG_STRING, "Must specify a model output file for "
          "training in real time or use option: -nomodelfile"); 
      RETURN (1);
    }

    RETURN(0);
  }
  else if( optionsData->rtTest ) {
    *mode = RT_TEST;
    if( !optionsData->modelFile[0] ) {
      snprintf(errorString, LONG_STRING,
          "Must specify a  model file for testing in real-time!");
      RETURN(1);
    }

    RETURN(0);
  }

  /* JL Apr. 2010: Added the ability to write out svm-light textfile without
   * having to go through training or to testing.
   *
   * Still going through train function for classification or regression
   * ("-type ..." mandatory!) but ONLY the doc textfile is written.
   *
   */

  if (optionsData->docFileOnly[0]) {
    /* some error checking for docout only */
    if ( (!typeFlag) && (!zFlag) ) {
      snprintf(errorString, LONG_STRING, "Must specify -type for -doconly!");
      RETURN(1);
    }

    if ( (optionsData->trainFile[0]) && (optionsData->testFile[0]) ) {
      snprintf(errorString, LONG_STRING, "Please specify either -trainvol or "
          "-testvol for -doconly!"); RETURN(1);
    }

    if ( (optionsData->labelFile[0]) && (optionsData->testLabelFile[0]) ){
      snprintf(errorString, LONG_STRING, "Please specify either -tainlabels or"
          " -testlabels for -doconly!"); RETURN(1);
     }

    /* make sure this works for -testvol as well */
    if ( optionsData->testFile[0] ) {
      strncpy(optionsData->trainFile, optionsData->testFile, LONG_STRING);
    }

    if ( optionsData->testLabelFile[0] ) {
          strncpy(optionsData->labelFile, optionsData->testLabelFile, LONG_STRING);
    }

    /* set mode */
    *mode=TRAIN;

    /* check for mask */
    if ( !optionsData->maskFile[0] ) {
      optionsData->outModelNoMask = 1;
    }

    strncpy(optionsData->docFile, optionsData->docFileOnly, LONG_STRING);

    RETURN(0);
  }

  if( (optionsData->docFile[0]) && (optionsData->testFile[0]) ) {
    snprintf(errorString, LONG_STRING, "Sorry, option  -testvol together with "
        "-docout is not supported. Please use option -doconly instead!");
    RETURN(1);
  }

  if( (optionsData->modelFile[0]) && (optionsData->noModelOut) ) { 
    WARNING_message("Option -model and -nomodelfile was specified. "
        "Option: -nomodelfile is ignored!");
    optionsData->noModelOut = 0;
  }

  /* Set mode and do some error checking */
  /* JL Aug. 2009: Changed error checking for testlabels. */
  if( optionsData->trainFile[0] ) {
    if( !(optionsData->labelFile[0]) ) {
        snprintf(errorString, LONG_STRING, "Must specify timeseries labelfile for " 
            "training!"); RETURN(1);
    }
    if( (optionsData->testFile[0]) ) *mode = TRAIN_AND_TEST;
    else *mode = TRAIN;
  }
  else if( (optionsData->testFile[0]) ) *mode = TEST;
  else { /* JL Oct 2009: Must specify trainvolume or testvolume */
    snprintf(errorString, LONG_STRING, "Must specify training or testing dataset!"); 
    RETURN(1);
  }

  if( !(optionsData->modelFile[0]) ) {
  if( ( (*mode == TRAIN) && !optionsData->noModelOut) || (*mode == TRAIN_AND_TEST) ) {
      snprintf(errorString, LONG_STRING, "Must specify a model output file for "
          "training or use option: -nomodelfile"); RETURN (1);
    /* In the future it would be great to keep them model in memory for
     * TRAIN_AND_TEST and not write it to disc and read it back in */
    }
    else if ( *mode == TEST) {
      snprintf(errorString, LONG_STRING, "Must specify a model input file for "
          "testing!"); RETURN (1);
    }
  }
  /* at some point may want to check for TRAIN/TEST specific mode options */
  /* e.g. nodetrend only applies in test mode                             */

  /* check for other errors */
  if( aFlag  && alphaFlag ) {
    /* if both -a and -alpha are specified, both files need to match */
    WARNING_message("Both -a and -alpha were specified. "
        "Using filename  %s", optionsData->modelAlphaFile);
    }
  
  /* JL Mar. 2009: Enabled -bucket option for only linear and complex-linear 
   * kernels */
  if( optionsData->modelWeightFile[0] ) {
    if ( !(kernel_parm->kernel_type == LINEAR) ) {
      if (  !( (kernel_parm->kernel_type == CUSTOM) && 
               (!strcmp(kernel_parm->custom, "complex1")) )  ) {
        WARNING_message("At this time, only can generate maps "
            "(-bucket option) for linear and linear-complex kernels!");
        
        strncpy(optionsData->modelWeightFile, "", LONG_STRING);
        }
      }
  }
  if( !(optionsData->trainFile[0]) && (optionsData->modelWeightFile[0])) { /* JL */ 
      WARNING_message("Maps (-bucket option) only can be generated "
          "during training.");
  }

  /* JL May 2011: */
  if( (optionsData->linearWmap) && !(optionsData->modelWeightFile[0])) {
    WARNING_message("Ignoring -wout. Please specify a bucket prefix (-bucket bprefix)");
  }
  
  /* Check mask usage */
  if( !(optionsData->modelFile[0]) && !(optionsData->outModelNoMask) 
                                   && !(optionsData->maskFile[0]) ){
      snprintf(errorString, LONG_STRING, "No mask file specified (use -mask file). " 
          "If not using a mask file must use option -nomodelmask"); RETURN(1);
  }
  if( (optionsData->maskFile[0]) && (optionsData->outModelNoMask) ) { /* JL */
    WARNING_message("Option -mask and -nomodelmask was specified. "
        "Option -nomodelmask is used!");
    strncpy(optionsData->maskFile, "", LONG_STRING);
  }
  
  /* This follows the original error checking of SVM-Light */
  if(learn_parm->svm_iter_to_shrink == -9999) {
    if(kernel_parm->kernel_type == LINEAR) 
      learn_parm->svm_iter_to_shrink=2;
    else
      learn_parm->svm_iter_to_shrink=100;
  }

  if((learn_parm->skip_final_opt_check) 
      && (kernel_parm->kernel_type == LINEAR)) {
    INFO_message("It does not make sense to skip the final optimality check "
        "for linear kernels.");
    learn_parm->skip_final_opt_check=0;
  }    
  if((learn_parm->skip_final_opt_check) 
      && (learn_parm->remove_inconsistent)) {
    snprintf(errorString, LONG_STRING, "It is necessary to do the final optimality check when "
        "removing inconsistent examples."); RETURN(1);
  }    
  if((learn_parm->svm_maxqpsize<2)) {
    snprintf(errorString, LONG_STRING, "Maximum size of QP-subproblems not in valid range: %ld [2..]",
        learn_parm->svm_maxqpsize); RETURN(1);
  }
  if((learn_parm->svm_maxqpsize<learn_parm->svm_newvarsinqp)) {
    snprintf(errorString, LONG_STRING, "Maximum size of QP-subproblems [%ld] must be larger than\n" 
        "  the number of new variables [%ld] entering the working set in each " 
        "iteration.",learn_parm->svm_maxqpsize, learn_parm->svm_newvarsinqp); RETURN(1); 
  }
  if(learn_parm->svm_iter_to_shrink<1) {
    snprintf(errorString, LONG_STRING, "Maximum number of iterations for shrinking not in valid "
       "range: %ld [1,..]",learn_parm->svm_iter_to_shrink); RETURN(1);
  }
  if(learn_parm->svm_c<0) {
    snprintf(errorString, LONG_STRING, "The C parameter must be greater than zero!"); RETURN(1);
  }
  if(learn_parm->transduction_posratio>1) {
    snprintf(errorString, LONG_STRING, "The fraction of unlabeled examples to classify as positives\n"
       "   must be less than 1.0 !!!"); RETURN(1);
  }
  if(learn_parm->svm_costratio<=0) {
    snprintf(errorString, LONG_STRING, "The COSTRATIO parameter must be greater than zero!"); RETURN(1);
  }
  if(learn_parm->epsilon_crit<=0) {
    snprintf(errorString, LONG_STRING, "The epsilon parameter must be greater than zero!"); RETURN(1);
  }
  if(learn_parm->rho<0) {
    snprintf(errorString, LONG_STRING, "The parameter rho for xi/alpha-estimates and leave-one-out\n"
        "   pruning mustbe greater than zero (typically 1.0 or 2.0, see\n"
        "   T. Joachims, Estimating the Generalization Performance of an\n"
        "   SVM Efficiently, ICML, 2000.)!"); RETURN(1);
  }
  if((learn_parm->xa_depth<0) || (learn_parm->xa_depth>100)) {
    snprintf(errorString, LONG_STRING, "The parameter rho for xi/alpha-estimates and leave-one-out\n"
        "   pruning mustbe greater than zero (typically 1.0 or 2.0, see\n"
        "   T. Joachims, Estimating the Generalization Performance of an\n"
        "   SVM Efficiently, ICML, 2000.)!"); RETURN(1);
  }

  if( (*main_verbosity) >=2 ) printASLoptions(optionsData);

  RETURN(0);
}
