#ifndef _3DSVM_COMMON_H
  #define _3DSVM_COMMON_H
#endif


#include "afni.h"
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <math.h>

/* both afni and svmlight use VERSION */
#ifdef VERSION  
  #undef VERSION
#endif
//#include "svm_common.h"
//#include "svm_learn.h"
#include "mrilib.h"

#define CLASS_MAX 300
#define SCALE 4000000
#define MAX_FILE_NAME_LENGTH 500
#define LONG_STRING 500
#define DatasetType short
#define MaskType    byte
#define LabelType   double

#define MODEL_MSK_EXT "_mask"

# define CUSTOM 4 /* JL */
/* LINEAR 0 
 * POLY 1 
 * RBF 2 
 * SIGMOID 3 
 * are defined in svm_common.h */

enum calling_fcn { ASL_PLUGIN, ASL_COMMAND_LINE };
enum modes { NOTHING, TRAIN, TEST, TRAIN_AND_TEST }; /* modes */

typedef struct ASLoptions{
  /* initialize at instantiation */
  char labelFile[MAX_FILE_NAME_LENGTH]; /* training class (label) file */
  char censorFile[MAX_FILE_NAME_LENGTH];/* training censor (ignore) file */
  char trainFile[MAX_FILE_NAME_LENGTH]; /* training dataset file */
  char maskFile[MAX_FILE_NAME_LENGTH];  /* mask dataset */
  char modelFile[MAX_FILE_NAME_LENGTH]; /* training output - model file */
  char docFile[MAX_FILE_NAME_LENGTH];   /* JL June 2009: write brick data into 
                                           svm-light formated textfile */
  char kernelName[LONG_STRING];         /* JL Feb. 2009: tring specifying 
                                           kernel functions  */
  char svmType[LONG_STRING];            /* JL May 2009: classification, 
                                           regression or ranking */
  int  outModelNoMask;	                /* flag signifying that no mask 
                                           should be applied to output model */
  int  noPredDetrend;	                /* flag signifying that no detrending 
          should be applied to output predictions in test mode */
  int  classout;	                    /* flag signifying thresholded class 
          predictions should be written to prediction files (rather than 
          continuous valued "distances") */
  char modelAlphaFile[MAX_FILE_NAME_LENGTH];
  char modelWeightFile[MAX_FILE_NAME_LENGTH];
  char testFile[MAX_FILE_NAME_LENGTH];      /* testing dataset file */
  char testLabelFile[MAX_FILE_NAME_LENGTH];	/* testing target classes 
                                               for test samples */
  char multiclass[LONG_STRING];	            /* type of classifyer 
                                               for a mulitclass dataset */
  char predFile[MAX_FILE_NAME_LENGTH];      /* predictions file */

}ASLoptions;

typedef struct labels {
  LabelType *lbls;			    /* the class labels indicating the stimulus
                                   categories for fMRI data */
  LabelType *cnsrs;			    /* indicates which labels to ignore 
                                   (value == 0) or use (value == 1) */
  int   class_list[CLASS_MAX];	/* hold class numbers appearing in classfile */
  int   n_classes;              /* number of different classes allowed 
                                   (for multiclass) */
  long  n;				        /* the number of labels and censors */
  int   n_cnsrs;                /* JL: number of censord time points 
                                   (i.e., label 9999) */
} LABELS;

typedef struct afniSvmModelHead {
  
    /* Gneral comment: The following int and float would be ideally 
     * long and double, but their ulitmate destination  is in the model 
     * header file (.HEAD). Unfortunately, there is no double or long 
     * functionality in THD_set_atr... */

    int   class_count;		   /* number of classes (stimulus categories) */
    int   combinations;		   /* all possible pair-wise combinations of 
                                  class_count  - would like to be long*/
    int   timepoints;		   /* total number (even counting censored data) */
    char  combName[(CLASS_MAX * (CLASS_MAX-1))/2][10];		
         /* short string describing the class combinations (e.g. '0_1','0_3') 
            ZSS: Changed from [10][...] to [...][10] 
            See how combName was used in function train_routine
            where classCount is the variable used to index into combName's 
            first dimension. classCount goes to (CLASS_MAX * (CLASS_MAX-1))/2
            and the previous declaration was causing bad corruption with other
            pointers. */
    char  kernel_custom[(CLASS_MAX * (CLASS_MAX-1))/2][50];
          /* JL: string describing user-defined kernel.
           * String-size 50 hard-coded as in svm-light. */
    char  svm_type[LONG_STRING];  /* JL: string describing svm (learn) type */
    int   *kernel_type;
    int   *polynomial_degree; 
    float *rbf_gamma;
    float *linear_coefficient;
    float *constant_coefficient;
    int   *total_masked_features;  
    int   *total_samples;         /* number of time points per class */
    int   *total_support_vectors; 
    float **cAlphas;            
           /* JL: censored alphas, cAlphas[class][total_samples[cc]]. Contains
            * alphas without censored timepoints (i.e., skipping labels: 9999),
            * to match index of data and index of alphas for each class-combintation */
    float **alphas;               /* alphas[class][timepoints] */
    float *b;

    
    /* JL Oct. 2009: Added remaining svm-light parameters that can be specified
     * from the command-line. */
    float *eps;                   /* epsilon for regression  */
    float *svm_c;                 /* upper bound C on alphas */
    int   *biased_hyperplane;     /* if nonzero, use hyperplane w*x+b=0 
                                     otherwise w*x=0 */
    int   *skip_final_opt_check;  /* do not check KT-Conditions at the end of
            optimization for examples removed by shrinking. WARNING: This might 
            lead to sub-optimal solutions! */ 
    int   *svm_maxqpsize;         /* size q of working set */ 
    int   *svm_newvarsinqp;       /* new variables to enter the working set 
                                     in each iteration */
    int   *svm_iter_to_shrink;    /* iterations h after which an example can
                                     be removed by shrinking */
    float *transduction_posratio; /* fraction of unlabeled examples to be 
                                     classified as positives */ 
    float *svm_costratio;         /* factor to multiply C for positive examples */
    float *svm_costratio_unlab;   /* for svm-ligth internal use */
    float *svm_unlabbound;        /* for svm-light internal use */
    float *epsilon_a;             /* for svm-light internal use */
    float *epsilon_crit;          /* tolerable error for distances used in 
                                     stopping criterion */
    int   *compute_loo;           /* if nonzero, computes leave-one-out
                                     estimates */
    float *rho;                   /* parameter in xi/alpha-estimates and for
                                     pruning leave-one-out range [1..2] */
    int   *xa_depth;              /* parameter in xi/alpha-estimates upper
            bounding the number of SV the current alpha_t is distributed over */
} AFNI_MODEL;


/* JL: Holds maps to be written into a functional bucket dataset  */
typedef struct ModelMaps {
  long nmaps;           /* number of maps */
  long nvox;            /* number of voxels in each map */
  char **names;         /* name for each map: names[nmaps][LONG_STRING]
                           shows up in the AFNI GUI (define overlay) */
  double **data;        /* data for each map:  data[nmaps][nvox] */
} MODEL_MAPS;


/* --- command line helpstring --- */
static char cl_helpstring[] = "\n"
"Program: 3dsvm\n"
"\n"
"+++++++++++ 3dsvm: support vector machine analysis of brain data  +++++++++++\n\n"
"3dsvm - temporally predictive modeling with the support vector machine\n"
"\n"
"   This program provides the ability to perform support vector machine\n"
"   (SVM) learning on AFNI datasets using the SVM-light package (version 5)\n"
"   developed by Thorsten Joachims (http://svmlight.joachims.org/).\n"
"\n"
"-----------------------------------------------------------------------------\n"
"Usage:\n"
"------\n"
"\t 3dsvm [options] \n"
"\n"
"Examples:\n"
"---------\n"
"1. Training: basic options require a training run, category (class) labels \n" 
"   for each timepoint, and an output model. In general, it usually makes \n"
"   sense to include a mask file to exclude at least non-brain voxels\n"
"\n"
"\t 3dsvm -trainvol run1+orig \\ \n"
"\t       -trainlabels run1_categories.1D \\ \n"
"\t       -mask mask+orig \\ \n"
"\t       -model model_run1\n"
"\n"
"2. Training: obtain model alphas (a_run1.1D) and \n"
"   model weights (fim: run1_fim+orig)\n"
"\n"
"\t 3dsvm -alpha a_run1 \\\n"
"\t       -trainvol run1+orig \\ \n"
"\t       -trainlabels run1_categories.1D \\ \n"
"\t       -mask mask+orig \\ \n"
"\t       -model model_run1\n"
"\t       -bucket run1_fim\n"
"\n"
"3. Training: exclude some time points using a censor file \n"
"\n"
"\t 3dsvm -alpha a_run1 \\\n"
"\t       -trainvol run1+orig \\ \n"
"\t       -trainlabels run1_categories.1D \\ \n"
"\t       -censor censor.1D \\ \n"
"\t       -mask mask+orig \\ \n"
"\t       -model model_run1\n"
"\t       -bucket run1_fim\n"
"\n"
"4. Training: control svm model complexity (C value)\n"
"\n"
"\t 3dsvm -c 100.0 \\\n"
"\t       -alpha a_run1 \\\n"
"\t       -trainvol run1+orig \\ \n"
"\t       -trainlabels run1_categories.1D \\ \n"
"\t       -censor censor.1D \\ \n"
"\t       -mask mask+orig \\ \n"
"\t       -model model_run1\n"
"\t       -bucket run1_fim\n"
"\n"
"5. Training: using a kernel \n"
"\n"
"\t 3dsvm -c 100.0 \\\n"
"\t       -kernel polynomial -d 2 \\\n"
"\t       -alpha a_run1 \\\n"
"\t       -trainvol run1+orig \\ \n"
"\t       -trainlabels run1_categories.1D \\ \n"
"\t       -censor censor.1D \\ \n"
"\t       -mask mask+orig \\ \n"
"\t       -model model_run1\n"
"\n"
"6. Training: using regression \n"
"\n"
"\t 3dsvm -type regression \\\n"
"\t       -c 100.0 \\\n"
"\t       -e 0.001 \\\n"
"\t       -alpha a_run1 \\\n"
"\t       -trainvol run1+orig \\ \n"
"\t       -trainlabels run1_categories.1D \\ \n"
"\t       -censor censor.1D \\ \n"
"\t       -mask mask+orig \\ \n"
"\t       -model model_run1\n"
"\n"
"7. Testing: basic options require a testing run, a model, and an output\n"
"   predictions file\n"
"\n"
"\t 3dsvm -testvol run2+orig \\\n"
"\t       -model model_run1+orig \\\n"
"\t       -predictions pred2_model1\n"
"\n"
"8. Testing: compare predictions with 'truth' \n"
"\n"
"\t 3dsvm -testvol run2+orig \\\n"
"\t       -model model_run1+orig \\\n"
"\t       -testlabels run2_categories.1D \\\n"
"\t       -predictions pred2_model1\n"
"\n"
"9. Testing: use -classout to output integer thresholded class predictions\n"
"   (rather than continuous valued output)\n"
"\n"
"\t 3dsvm -classout \\\n"
"\t       -testvol run2+orig \\\n"
"\t       -model model_run1+orig \\\n"
"\t       -testlabels run2_categories.1D \\\n"
"\t       -predictions pred2_model1\n"
"\n"
"\n"
"options:\n"
"--------\n"
"\n"
"------------------- TRAINING OPTIONS -------------------------------------------\n"
"-type tname            Specify tname:\n"
"\n"                          
"                             classification [default]\n"
"                             regression\n"
"\n"
"                       to select between classification or regression.\n"
"\n"
"-trainvol trnname      A 3D+t AFNI brik dataset to be used for training. \n"
"\n"
"-mask mname            mname must be is a byte-format brik file used to\n"
"                       mask voxels in the analysis. For example, a mask\n"
"                       of the whole brain can be generated by using \n"
"                       3dAutomask, or more specific ROIs could be generated\n"
"                       with the Draw Dataset plugin or converted from a \n"
"                       thresholded functional dataset. The mask is specified\n"
"                       during training but is also considered part of the \n"
"                       model output and is automatically applied to test \n"
"                       data. \n"
"\n"
"-nomodelmask           Flag to enable the ommission of a mask file. If this\n"
"                       option is used for training, it must also be used \n"
"                       for testing. \n"
"\n"
"-trainlabels lname     lname = filename of class category .1D labels \n"
"                       corresponding to the stimulus paradigm for the \n" 
"                       training data set. The number of labels in the \n" 
"                       selected file must be equal to the number of \n"
"                       time points in the training dataset. The labels\n"
"                       must be arranged in a single column, and they can\n"
"                       be any of the following values: \n"
"\n"
"                              0    - class 0\n"
"                              1    - class 1\n"
"                              n    - class n (where n is a positive integer)\n"
"                              9999 - censor this point \n"
"\n"
"                       It is recommended to use a continuous set of class\n"
"                       labels, starting at 0. See also -censor.\n"
"\n"
"-censor cname          Specify a .1D censor file that allows the user\n"
"                       to ignore certain samples in the training data.\n"
"                       To ignore a specific sample, put a 0 in the\n"
"                       row corresponding to the time sample - i.e., to\n"
"                       ignore sample t, place a 0 in row t of the file.\n"
"                       All samples that are to be included for training\n"
"                       must have a 1 in the corresponding row. If no\n"
"                       censor file is specified, all samples will be used \n"
"                       for training. Note the lname file specified by\n"
"                       trainlabels can also be used to censor time points\n" 
"                       (see -trainlabels).\n"
"\n"
"-kernel kfunc          kfunc = string specifying type of kernel function:\n"
"\n"
"                             linear     : <u,v>  [default] \n"
"                             polynomial : (s<u,v> + r)^d \n"
"                             rbf        : radial basis function\n" 
"                                          exp(-gamma ||u-v||^2) \n"
"                             sigmoid    : tanh(s <u,v> + r)) \n"
"\n"
"                       note: kernel parameters use SVM-light syntax:\n"
"\n"
"                             -d int     : d parameter in polyniomial kernel\n"
"                                            3 [default]\n"
"                             -g float   : gamma parameter in rbf kernel\n"
"                                            1.0 [default]\n"
"                             -s float   : s parameter in sigmoid/poly kernel\n"
"                                            1.0 [default]\n"
"                             -r float   : r parameter in sigmoid/poly kernel\n"
"                                            1.0 [default]\n"
"\n"
"-alpha aname           Write the alpha file generated by SVM-light to\n"
"                       aname.1D \n"
"\n"
"-wout                  Flag to output sum of weighted linear support \n"
"                       vectors to the bucket file. This is one means of\n"
"                       generating an \"activation map\" from linear kernel\n"
"                       SVMs see (LaConte et al., 2005). NOTE: this is \n"
"                       currently not required since it is the only output\n"
"                       option.\n"
"\n"
"-bucket bprefix        Currently only outputs the sum of weighted linear \n"
"                       support vectors written out to a functional (fim) \n"
"                       brik file. This is one means of generating an \n"
"                       \"activation map\" from linear kernel SVMS \n"
"                       (see LaConte et al, 2005). \n"
"\n"
"-docout docname        Write training data to a SVM-light (version 5.00)\n"
"                       formated textfile. For debugging purposes only.\n"
"\n"
"------------------- TRAINING AND TESTING MUST SPECIFY MODNAME ------------------\n"
"-model modname         modname = basename for the output model brik and any\n"
"                       axillary files during training. For testing, modname\n"
"                       is used to specify the model brik. As in the\n"
"                       examples above: \n"
"\n"
"                           3dsvm -trainvol run1+orig \\ \n"
"                                 -trainlabels run1_categories.1D \\ \n"
"                                 -mask mask+orig \\ \n"
"                                 -model model_run1\n"
"\n"
"                           3dsvm -testvol run2+orig \\ \n"
"                                 -model model_run1+orig  \\ \n"
"                                 -predictions pred2_model1\n"
"\n"
"------------------- TESTING OPTIONS --------------------------------------------\n"
"-testvol tstname       A 3D or 3D+t AFNI brik dataset to be used for testing. \n"
"                       A major assumption is that the training and testing  \n"
"                       volumes are aligned, and that voxels are of same number, \n"
"                       volume, etc. \n"
"\n"
"-predictions pname     pname = basename for .1D files output for a test\n"
"                       dataset. These files consist of single columns of\n"
"                       value results for each training data timepoint. A\n"
"                       seperate file is generated for each possible pair of\n"
"                       training classes. If more than two class categories\n"
"                       were specified, an \"overall\" file is also generated.\n"
"                       By default, the prediction values take on a continuous\n"
"                       range; to output inter-valued class decision values, \n"
"                       use the -classout flag. \n"
"\n"
"-classout              Flag to specify that pname files should be integer-\n"
"                       valued, corresponding to class category decisions.\n"
"\n"
"-nodetrend             Flag to specify that pname files should not be \n"
"                       linearly de-trended (detrend is the current default).\n"
"\n"
"-testlabels tlname     tlname = filename of 'true' class category .1D labels \n" 
"                       for the test dataset. It is used to calculate the \n"
"                       prediction accuracy performance of SVM classification. \n"
"                       If this option is not specified, then performance \n"
"                       calculations are not made. Format is the same as \n"
"                       lname specified for -trainlabels. \n"
"\n"
"-multiclass mctype     mctype specifies the multiclass algorithm for \n" 
"                       classification. Current implementations use 1-vs-1\n"
"                       two-class SVM models.\n"
"\n"
"                       mctype must be one of the following: \n"
"\n"
"                             DAG   :  Directed Acyclic Graph [default] \n"
"                             vote  :  Max Wins from votes of all 1-vs-1 models \n"
"\n"
"                       see http:\\\\cpu.bcm.edu\\laconte\\3dsvm for details and\n"
"                       references.\n"
"\n"
"------------------- INFORMATION OPTIONS ---------------------------------------\n"
"-help                  this help\n"
"\n"
"-change_summary        describes chages of note and rough dates of their\n" 
"                       implementation\n"
"\n"
"\n\n";

/* --- plugin helpstring --- */
static char plugin_helpstring[] = "\n"
"+++++++++++++++ 3dsvm: support vector machine analysis of brain data  +++++++++++++++\n\n"

"This plugin provides the ability to perform support vector machine \n"
"(SVM) analyses (training and testing) using SVM-Light (version 5),\n"
"developed by Thorsten Joachims, (http://svmlight.joachims.org/).\n"
"\n"
"General notes:\n"
"--------------\n"
" - This GUI plugin has a corresponding command-line version, 3dsvm \n"
"which offers extra functionality for learning and classification \n"
"with SVM-Light.  For example, the full set of SVM-Light command \n"
"line options is available in 3dsvm. \n"
"\n"
"Using the Plugin:\n"
"-----------------\n"
" - The user can choose to perform SVM training and testing either \n"
"alone or together.  This is done by selecting the \"Training\" and \n"
"or \"Testing\" options accordingly.  The interface is organized by \n"
"option lines: \n"
"\n"
"  1)  Training - Select this option to perform SVM training. \n"
"\n"
"    a.  Training Type - Choose classification or regression\n"
"       classification: labels represent stimulus/behavioral categories\n"
"       regression: labels for parametric tasks\n"
"       Note: See ""Labels"" below. For classifiaction, should have\n"
"             Integers (starting at 0). For regression, can have\n"
"             continuous valued labels.\n"
"\n"
"  2)  Train Data - Perform SVM learning using the data specified on \n"
"      this line. \n"
"\n"
"    a.  Dataset - Choose a 3D+t training dataset from the current session.\n"
"\n"
"    b.  Labels - Choose a .1D file indicating the class label for each \n"
"    time point in the dataset. The number of entries in the selected file \n"
"    must be equal to the number of time points in the chosen training \n"
"    dataset.  The labels must be arranged in a single column. \n"
"    See also the next section on censors.\n"
"\n"
"    For classification, labels can take on any of the following values: \n"
"       0    - class 0\n"
"       1    - class 1\n"
"       n    - class n\n"
"       9999 - censor this point\n"
"       It is recommended (currently required) that the class labels be\n"
"       continuous integers, starting at 0.\n"
"\n"
"    For regression, labels can any real number value\n"
"\n"
"    c.  Censors - Choosing a .1D file for this option allows \n"
"    the user to ignore certain samples in the training data. \n"
"    To ignore a specific sample, put a 0 in the row corresponding \n"
"    to the time sample - i.e., to ignore sample t, place a 0 in row t \n"
"    of the file.  All samples that are to be included in training \n"
"    must have a 1 in the corresponding row.  If no censor file is \n"
"    specified, all samples will be used for training. \n"
"\n"
"  3)  Train Params - parameters to control training. \n"
"\n"
"    a.  Mask - Allows the user to choose a mask (ROI) to apply to \n"
"    the training data.  For example, a mask of the whole brain can \n"
"    be generated by using 3dAutomask, or more specific ROIs could be \n"
"    generated with the Draw Dataset plugin or converted from a thresholded \n"
"    functional dataset. The mask dataset must be a byte-format brik \n"
"    file and must be in the current session to appear in the chooser. \n"
"    The mask is specified during training but is also considered part of\n"
"    the model output and is automatically applied to test data. A mask\n"
"    is currently required by the plugin.\n"
"\n"
"    b.  C - An SVM parameter that represents the trade off between \n"
"    the training error and the margin.  Default value is 100. \n"
"\n"
"    c.  Epsilon - For regression, the SVM loss funtion is insensitive \n" 
"    to training errors whos absolute value are smaller than epsilon. \n" 
"    Default value is 0.1. \n"
"\n"
"  4)  Kernel \n"
"\n"
"    a.  Kernel - Choose a kernel function \n"
"       linear     : <u,v>  [Default] \n"
"       polynomial : (s<u,v> + r)^d \n"
"       rbf        : radial basis function exp(-gamma ||u-v||^2) \n"
"       sigmoid    : tanh(s <u,v> + r)) \n"
"\n"
"  5)  Model Output \n"
"\n"
"    a.  Prefix - enter a prefix for the basename of the output model brik \n"
"    and any auxillary files generated during training. \n"
"\n"
"  6)  Model Inspection - The entries in this option line allow the \n"
"  user to specify output files that can be used to examine the SVM\n"
"  training model. \n"
"\n"
"    a.  FIM prefix - The user can choose to write out the sum of weighted \n"
"    linear support vectors to a functional (fim) brik file. This is \n"
"    one means of generating an \"activation map\" from linear kernel \n"
"    SVMs (see LaConte et al, 2005).\n" 
"\n"
"    b.  Alpha Prefix (.1D) - save the alpha file generated by SVM-Light.\n"
"\n"
"  7)  Testing - Classify a set of volumes using a training model.\n"
"  If both training and testing options are specified to be run by\n"
"  the plugin, the model produced by training is applied directly to\n"
"  the test data.  If testing is done alone, the SVM model file must\n"
"  be specified.\n" 
"\n"
"  8)  Test Data - Perform SVM testing using the data specified on \n"
"  this line.\n"
"\n"
"    a.  Dataset - The 3D or 3D+t dataset to test. A major assumption\n"
"    is that the training and testing volumes are aligned, and that\n"
"    voxels are of same number, volume, etc.\n"
"\n"
"    b.  Model - The AFNI BRIK/HEAD file is generated by SVM training. \n"
"    Currently, if training and testing are done at once then a model\n"
"    should not be selected here.\n"
"\n"
"  9)  Label Output - Ouput a .1D file containing a single \n"
"  column holding the values of the predictions resulting from the \n"
"  application of the training model to the test data.  One value \n"
"  is present for each tested volume. These values take on a continuous \n"
"  range; negative values correspond to class 0, positive values to \n"
"  class 1. \n"
"\n"
"    a.  Prefix (.1D) - The prefix to use for the naming of the output \n"
"    file. \n"
"\n"
" 10)  'True' Labels - A .1D label file that contains the true class values \n"
"  of the samples in the test dataset.  It is used to calculate the \n"
"  performance (successes, failures) of SVM classification.  If this option \n"
"  is not chosen, then performance calculations (like prediction accuracy)\n"
"   are not made. \n"
"\n"
"    a.  File - Choose the file to use for testing accuracy calculations. \n"
"\n"

"Summary:\n"
"-----------------\n"
" Train \n"
"      Required: Train Data (Dataset, Labels); Train Params (Mask); \n"
"                Model Output (Prefix).\n"
"      Optional: Train Data (Censors); Train Params (Kernel, C); \n"
"                Model Inspection (FIM Prefix, Alpha Prefix (.1D)). \n"
" Test \n"
"      Required: Test Data (Dataset, Model); Label Output (.1D).\n"
"      Optional: 'True' Labels (.1D File). \n"
"\n\n\n";

/* --- realtime plugin helpstring --- */
static char plugin_helpstring_rt[] = "\n"
" \n\n Ups, nothing yet... \n"
"\n\n\n";


/* ---- string with contributions ---- */
static char contribution_string [] =
"Significant programming contributions by: \n"
"\n"
"  Jeff W. Prescott     \n" 
"  William A. Curtis    \n" 
"  Ziad Saad            \n" 
"  Jonathan M. Lisinski \n" 
"  Stephen M. LaConte   \n"
"\n"
"Original version written by JP and SL, August 2006 \n"
"Released to general public, July 2007 \n"
"\n"
"Questions/Comments/Bugs - email slaconte@cpu.bcm.edu \n"
"\n"
"Reference:\n"
"LaConte, S., Strother, S., Cherkassky, V. and Hu, X. 2005. Support vector\n" 
"    machines for temporal classification of block design fMRI data. \n"
"    NeuroImage, 26, 317-329.\n"
"\n";

/*----- String that briefly describes changes -------------------*/
static char change_string[] = "\n"
"Changes of note:\n"
"\n"
"Circa Nov/Dec 2008\n"
"Note that 3dsvm's -predictions files have always been correct, however changes\n"
"1 and 3 (below) are important for those who only rely on prediction accuracy \n"
"summaries.\n"
"1) Fixed a bug in calculating prediction accuracies.\n"
"2) Changed multiclass for testvols - old method may have had problems in\n"
"   special cases. Now using DAG and Max Wins voting for or one vs. one\n" 
"   multiclass.\n"
"3) Improved handling of prediction accuracy calculations for censored test" 
"   data labels\n"
"4) This change_summary flag added!\n"
"\n"
"Circa Nov 2009\n"
"1) fixed a memory allocation bug that caused crashes in linux\n" 
"2) fixed a bug in bucket file that caused a one pixel shift in weight\n" 
"   vector map \n"
"3) enabled SVM-light-provided kernels\n"
"4) enabled SVM-light regression\n"
"5) added –docout flag to allow conversion of {trainvol, mask, trainlabels}\n"
"   to SVM-light format (for debugging)\n"
"6) combined multiclass –bucket output to actually be a bucket, rather than\n"
"   individual ‘fim’ briks\n"
"\n"
"-enjoy\n"
"\n\n";
