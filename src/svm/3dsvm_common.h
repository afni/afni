#ifndef _3DSVM_COMMON_H
  #define _3DSVM_COMMON_H
#endif

#test CVS

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

enum calling_fcn { ASL_PLUGIN, ASL_COMMAND_LINE };

enum modes { NOTHING, TRAIN, TEST, TRAIN_AND_TEST }; /* modes */

typedef struct ASLoptions{
  /* initialize at instantiation */
  char labelFile[MAX_FILE_NAME_LENGTH];         /* training input - class (label) file */
  char censorFile[MAX_FILE_NAME_LENGTH];        /* training input - censor (ignore) file */
  char trainFile[MAX_FILE_NAME_LENGTH];         /* training input - dataset file */
  char maskFile[MAX_FILE_NAME_LENGTH];          /* mask dataset */
  char modelFile[MAX_FILE_NAME_LENGTH];         /* training output - model file */
  int  outModelNoMask;	   /* flag signifying that no mask should be applied to output model */
  int  noPredDetrend;	   /* flag signifying that no detrending should be applied to output predictions in test mode */
  int  classout;	   /* flag signifying thresholded class predictions should be written to prediction files (rather than continuous valued "distances") */
  char multiclass[LONG_STRING];	                /* testing input - type of classifyer for a mulitclass dataset */
  char modelAlphaFile[MAX_FILE_NAME_LENGTH];
  char modelWeightFile[MAX_FILE_NAME_LENGTH];
  char testFile[MAX_FILE_NAME_LENGTH];          /* testing input - dataset file */
  char testLabelFile[MAX_FILE_NAME_LENGTH];	/* testing input - target classes for test samples */
  char predFile[MAX_FILE_NAME_LENGTH];          /* testing output - predictions file */

}ASLoptions;

typedef struct labels {
  LabelType *lbls;			/* the class labels indicating the stimulus categories for fMRI data */
  LabelType *cnsrs;			/* indicates which labels to ignore (value == 0) or use (value == 1) */
  int       class_list[CLASS_MAX];	/* hold class numbers appearing in classfile */
  int       n_classes;		/* number of different classes allowed (for multiclass) */
  long      n;				/* the number of labels and censors */
  int       n_cnsrs;         /* the number of censors (JL) */
}LABELS;

typedef struct afniSvmModelHead {
    int class_count;		/* number of classes (stimulus categories) */
    int combinations;		/* all possible pair-wise combinations of class_count  - would like to be long*/
    int timepoints;		/* total number (even counting censored data  - would like to be long*/
    char combName[(CLASS_MAX * (CLASS_MAX-1))/2][10];		
         /* short string describing the class combinations (e.g. '0_1','0_3') 
            ZSS: Changed from [10][...] to [...][10] 
            See how combName was used in function train_routine
            where classCount is the variable used to index into combName's 
            first dimension. classCount goes to (CLASS_MAX * (CLASS_MAX-1))/2
            and the previous declaration was causing bad corruption with other
            pointers. */
    int *kernel_type; 
    int *polynomial_degree; 
    float *rbf_gamma;
    float *linear_coefficient;
    float *constant_coefficient;

    /* the following ints would ideally be long integers, but
     * their ultimate destiny is in an AFNI .HEAD
     * unfortunately there does not seem to be a long int equivalent 
     * to THD_set_int_atr */
    int *total_masked_features;  
    int *total_samples;  /* number of time points per class */
    int *total_support_vectors;  

    float **alphas;       /* alphas[class][timepoints] */
    float *b; 
} AFNI_MODEL;

/* function prototypes  - really outdated*/
/*enum modes input_parse( int, char **,long *, long *, LEARN_PARM *, KERNEL_PARM *, ASLoptions *);*/
//void train_routine(MODEL *, LEARN_PARM *, KERNEL_PARM *, ASLoptions *, THD_3dim_dataset *, THD_3dim_dataset *, THD_3dim_dataset *, 
//                   MaskType *, int, char **);
//void test_routine(ASLoptions *, MODEL *, THD_3dim_dataset *, THD_3dim_dataset *, THD_3dim_dataset *, int, char **);

/***********************/
//int compare_ints( const int *, const int * );
//int compare_strings( const char *, const char * );
//void afni_dset_to_svm_doc( short *, byte *, DOC *, int, int, long);
//enum modes input_parse( int, char **,long *, long *, LEARN_PARM *, KERNEL_PARM *, ASLoptions *);
//void write_brik_alphas( char *, double *, long *, long );
//void write_brik_model( char*, MODEL* );
//void scale_factor( double*, long, double* );
//void read_brik_model( char*, MODEL* );
//void read_brik_model( THD_3dim_dataset*, MODEL* );
//void wait_any_key(void);
//void print_help(void);


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
"  2)  Train Data - Perform SVM learning using the data specified on \n"
"      this line. \n"
"\n"
"    a.  Dataset - Choose a 3D+t training dataset from the current session.\n"
"\n"
"    b.  Labels - Choose a .1D file indicating the class label for each \n"
"    time point in the dataset. The number of entries in the selected file \n"
"    must be equal to the number of time points in the chosen training \n"
"    dataset.  The labels must be arranged in a single column, and they \n"
"    can take on any of the following values: \n"
"\n"
"       0    - class 0\n"
"       1    - class 1\n"
"       n    - class n\n"
"       9999 - censor this point\n"
"\n"
"       It is recommended to use a continuous set of class labels, starting\n"
"       at 0. See next section on censors.\n"
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
"    b.  Kernel - The kernel type that is used for learning. Currently\n"
"    the user only has the choice of \"Linear\". \n"
"\n"
"    c.  C - an SVM parameter that represents the trade off between \n"
"    the training error and the margin.  Default value is 100. \n"
"\n"

"  4)  Model Output \n"
"\n"
"    a.  Prefix - enter a prefix for the basename of the output model brik \n"
"    and any auxillary files generated during training. \n"
"\n"
"  5)  Model Inspection - The entries in this option line allow the \n"
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
"  6)  Testing - Classify a set of volumes using a training model.\n"
"  If both training and testing options are specified to be run by\n"
"  the plugin, the model produced by training is applied directly to\n"
"  the test data.  If testing is done alone, the SVM model file must\n"
"  be specified.\n" 
"\n"

"  7)  Test Data - Perform SVM testing using the data specified on \n"
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
"  8)  Label Output - Ouput a .1D file containing a single \n"
"  column holding the values of the predictions resulting from the \n"
"  application of the training model to the test data.  One value \n"
"  is present for each tested volume. These values take on a continuous \n"
"  range; negative values correspond to class 0, positive values to \n"
"  class 1. \n"
"\n"
"    a.  Prefix (.1D) - The prefix to use for the naming of the output \n"
"    file. \n"
"\n"

"  9)  'True' Labels - A .1D label file that contains the true class values \n"
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
"\n\n\n"

"Jeff W. Prescott and Stephen M. LaConte \n"
"Questions/Comments/Bugs - email slaconte@bme.emory.edu \n"
"\n"
"Reference:\n"
"----------\n"
"LaConte, S., Strother, S., Cherkassky, V. and Hu, X. 2005. Support vector\n"
"    machines for temporal classification of block design fMRI data.\n"
"    NeuroImage, 26, 317-329.\n";



/*----- COMMAND LINE HELPSTRING -------------------*/
static char cl_helpstring[] = "\n"
"Program: 3dsvm\n"
"Authors: Jeffery Prescott and Stephen LaConte\n"
"\n"
"+++++++++++++++ 3dsvm: support vector machine analysis of brain data  +++++++++++++++\n\n"
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
"5. Testing: basic options require a testing run, a model, and an output\n"
"   predictions file\n"
"\n"
"\t 3dsvm -testvol run2+orig \\\n"
"\t       -model model_run1+orig \\\n"
"\t       -predictions pred2_model1\n"
"\n"
"6. Testing: compare predictions with 'truth' \n"
"\n"
"\t 3dsvm -testvol run2+orig \\\n"
"\t       -model model_run1+orig \\\n"
"\t       -testlabels run2_categories.1D \\\n"
"\t       -predictions pred2_model1\n"
"\n"
"7. Testing: use -classout to output integer thresholded class predictions\n"
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
"-trainvol trnname      A 3D+t AFNI brik dataset to be used for training. \n"
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
"-a aname               Write the alpha file generated by SVM-Light to\n"
"                       aname.1D \n"
"-alpha aname           Same as -a option above. \n"
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
"-multiclass mctype     mctype specifies the multiclass algorithm for classification\n"
"                       current implementations use 1-vs-1 two-class SVM models\n"
"\n"
"                       mctype must be one of the following: \n"
"\n"
"                             DAG     [Default]:  Directed Acyclic Graph\n"
"                             vote             :  Max Wins from votes of all 1-vs-1 models\n"
"\n"
"                       see http:\\\\cpu.bcm.edu\\laconte\\3dsvm for details and references.\n"
"\n"
"------------------- INFORMATION OPTIONS --------------------------------------------\n"
"-help                  this help\n"
"\n"
"-change_summary        describes chages of note and rough dates of their implementation\n"
"\n"
"\n\n";

/*----- String that briefly describes changes -------------------*/
static char change_string[] = "\n"
"Changes of note:\n"
"\n"
"Circa Nov/Dec 2008\n"
"Note that 3dsvm's -predictions files have always been correct, however changes 1 and 3 (below)\n"
"are important for those who only rely on prediction accuracy summaries.\n"
"1) Fixed a bug in calculating prediction accuracies.\n"
"2) Changed multiclass for testvols - old method may have had problems in special cases.\n"
"Now using DAG and Max Wins voting.\n"
"3) improved handling of prediction accuracy calculations for censored test data labels\n"
"4) this change_summary flag added!\n"
"\n"
"\n"
"-enjoy\n"
"\n\n";
