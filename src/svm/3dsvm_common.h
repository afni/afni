/*****************************************************************************/
/*                                                                           */
/* 3dsvm_common.h                                                            */
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


#ifndef _3DSVM_COMMON_H
  #define _3DSVM_COMMON_H
#endif

#include "afni.h"
#include "svm_common.h"
#include "svm_learn.h"
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <math.h>
#include "mrilib.h"

/* JL Apr 2010: Added version number and version date for 3dsvm
 * and changed VERSION in svm-light source to avoid conflicts with afni.
 * readAllocateAfniModel is using the version number to read in model
 * parameters correctly */

#define VERSION_3DSVM "V1.30"
#define VERSION_DATE_3DSVM "11/21/17"
#define CLASS_MAX 300
#define SCALE 4000000
#define MAX_FILE_NAME_LENGTH 500
#define LONG_STRING 500

#define PROGRAM_NAME   "3dsvm"   /* name of this program -used to include
                                    command-line history in model etc. */

#define CSV_STRING  20          /* JL: CSV_STRING: size of comma separated value
        string. Strings of this size are used to store class-combination and
        custom-kernel, which is plenty for comb_names, but for some reason
        size 50 is causing memory corruption problems. */

#define DatasetType float       /* JL: Internal data representation of dataset
                                   arrays. This used to be short */
#define MaskType    byte
#define LabelType   double

#define MODEL_MSK_EXT "_mask"   /* JL: Used for mask file naming before mask was
                                   written as a sub-brick into the model */
#define MASK_YES 1              /* mask was used for training */
#define MASK_NO  0              /* mask was not used for training */
#define MASK_UNKNOWN -1         /* unknown if mask was used for training. Only the
                                   case if version < V1.10 was used for training */
#define SVM_HOST_NCTRY  3       /* max number of attempts to connect to SVM host
                                   (relevant for real-time action */

/* JL: Used to define the kernel type. CUSTOM stands for custom kernel.
 * LINEAR 0
 * POLY 1 
 * RBF 2 
 * SIGMOID 3 
 * are defined in svm_common.h */
#define CUSTOM 4
enum calling_fcn { ASL_PLUGIN, ASL_COMMAND_LINE };
enum modes { NOTHING, TRAIN, TEST, TRAIN_AND_TEST,
              RT_TRAIN, RT_TEST   /* JL. Sep 2010 */ };
typedef struct ASLoptions{
  /* initialize at instantiation */
  char labelFile[LONG_STRING];  /* training class (label) file */
  char censorFile[LONG_STRING]; /* training censor (ignore) file */
  char trainFile[LONG_STRING];  /* training dataset file */
  char maskFile[LONG_STRING];   /* mask dataset */
  char modelFile[LONG_STRING];  /* training output - model file */
  char docFile[LONG_STRING];    /* JL June 2009: write brick data into
                                   svm-light formated textfile */
  char docFileOnly[LONG_STRING];/* JL June 2009: write brick data into
                                    svm-light formated textfile */
  char kernelName[LONG_STRING]; /* JL Feb. 2009: tring specifying
                                   kernel functions  */
  char svmType[LONG_STRING];    /* JL May 2009: classification, regression
                                   or ranking */
  int  outModelNoMask;	        /* flag signifying that no mask
                                   should be applied to output model */
  int  noModelOut;   	        /* JL Oct. 2017: flag: don't write model file */
  int  noPredDetrend;	        /* flag signifying that no detrending
                                   should be applied to output predictions
                                   in test mode */
  int  noPredCensor;            /* flag signifying that predictions for
                                   censored timepoints are not written
                                   into prediction file */
  int noPredScale;              /* flag signifying that predcitions are
                                   not scaled to {0,1} */
  int  classout;	        /* flag signifying thresholded class
                                   predictions should be written to
                                   prediction files (rather than
                                   continuous valued "distances") */
  int  rtTrain;                 /* flag indicating real-time training */
  int  rtTest;                  /* flag indicating real-time testing */
  char rtIP[LONG_STRING];       /* IP address to which values are being send
                                   during real-time testing (for feedback etc.) */
  int  rtPort;                  /* port to which values are being send
                                   during real-time testing (for feedback etc.) */
  char testFile[LONG_STRING];   /* testing dataset file */
  char multiclass[LONG_STRING];	/* type of classifyer for a mulitclass dataset */
  char predFile[LONG_STRING];   /* predictions file */
  char testLabelFile[LONG_STRING]; /* testing target classes for test samples */
  char modelAlphaFile[LONG_STRING];
  char modelWeightFile[LONG_STRING];
  int linearWmap;               /* JL May 2011: flag signifying that user has 
                                   specified -wout flag: Write sum of weighted 
                                   linear sv into bucket file. This is currently 
                                   the only activation map that is implemented. */
}ASLoptions;

typedef struct labels {
  LabelType* lbls; 		/* the class labels indicating the stimulus
                                   categories for fMRI data */
  LabelType* cnsrs;	        /* indicates which labels to ignore
                                   (value == 0) or use (value == 1) */
  LabelType* lbls_cont;         /* JL: labels converted to continuous values */
  int *class_list;              /* hold class numbers appearing in classfile
                                   JL: changed allocation due to corruption
                                   problems */
  int n_classes;                /* number of different classes allowed
                                   (for multiclass) */
  int *lbls_count;              /* occurrence of each class label 
                                   (continuous index) */
  long n;			/* the number of labels and censors */
  int n_cnsrs;                  /* JL: number of censored time points
                                   (i.e., label 9999) */
} LABELS;

typedef struct afniSvmModelHead {
  
  /* General comment: The following int and float would be ideally
   * long and double, but their ultimate destination  is in the model
   * header file (.HEAD). Unfortunately, there is no double or long
   * functionality in THD_set_atr... */

  float version;                /* JL Oct. 2010 */
  int   mask_used;              /* JL: flag indicating if mask was used
                                 * MASK_YES, MASK_NO, MASK_UNKNOWN */
  int   class_count;		/* number of classes (stimulus categories) */
  int   combinations;		/* all possible pair-wise combinations of
                                 * class_count  - would like to be long */
  int   timepoints;		/* total number (even counting censored data) */

  long  max_iterations;         /* JL July 2011: Maximum number of iterations */
  char  **combName;
       /* short string describing the class combinations (e.g. '0_1','0_3')
          ZSS: Changed from [10][...] to [...][10]
          See how combName was used in function train_classification
          where classCount is the variable used to index into combName's
          first dimension. classCount goes to (CLASS_MAX * (CLASS_MAX-1))/2
          and the previous declaration was causing bad corruption with other
          pointers.
          JL Apr. 2010: Now allocated at run time in allocateAfniModel
       */
  char  **kernel_custom;        /* JL: string describing user-defined kernel */
                                /* JL: now allocated at run time in allocateAfniModel */
  char  svm_type[LONG_STRING];  /* JL: string describing svm (learn) type */
  int   *kernel_type;
  int   *polynomial_degree;
  float *rbf_gamma;
  float *linear_coefficient;
  float *constant_coefficient;
  int   *total_masked_features;
  int   *total_samples;         /* number of time points per class */
  int   *total_support_vectors;
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
  float *svm_unlabbound;       /* for svm-light internal use */
  float *epsilon_a;             /* for svm-light internal use */
  float *epsilon_crit;          /* tolerable error for distances used in
                                   stopping criterion */
  int   *compute_loo;           /* if nonzero, computes leave-one-out
                                   estimates */
  float *rho;                   /* parameter in xi/alpha-estimates and for
                                   pruning leave-one-out range [1..2] */
  int   *xa_depth;              /* parameter in xi/alpha-estimates upper
                                   bounding the number of SV the current
                                   alpha_t is distributed over */
} AFNI_MODEL;

/* JL: Holds maps to be written into a functional bucket dataset  */
typedef struct ModelMaps {
  long      nmaps;             /* number of maps */
  long      nvox;              /* number of voxels in each map */
  long      index;             /* index over nmaps (keeps track of how many
                                  maps were written into this structure) */
  char   ** names;             /* name for each map: names[nmaps][LONG_STRING]
                                  shows up in the AFNI GUI (define overlay) */
  double ** data;              /* data for each map:  data[nmaps][nvox] */
} MODEL_MAPS;

/* JL: Holds variables and structures for real-time action */
typedef struct rt_svm_vars {
  ASLoptions *   options;        /* ptr to ASLoptions structure */
  enum modes     mode;           /* mode (RT_TRAIN, RT_TEST, ...) */
  DatasetType ** dsetModelArray; /* contains the model data for every voxel at
                                   every time-point */
  MaskType *     dsetMaskArray;   /* mask specifying a subset of voxels */
  AFNI_MODEL *   afniModel;       /* ptr to AFNI_MODEL structure */
  int            svm_type;        /* svm learn type (CLASSIFICATION, REGRESSION) */
  long           nvox_model;      /* number of voxels (featurs) in model */
  long           nt_model;        /* number of time-points (observations) in model */
  int            myargc;          /* number of "command-line arguments" */
  char **        myargv;          /* "command line arguments" */
  char           SVM_iochan[128]; /* I/O channel name SVM_host */
  IOCHAN *       SVM_ioc;         /* ptr to I/O channel */
  int            SVM_HOST_OK;     /* flag indicating if I/O with SVM_host (host that
                                   supposedly wants to receive svm data) is ok */
  FILE *         fp_pred;         /* file pointer to prediction file */
  KERNEL_PARM *  kernel_parm;     /* svm-light kernel parameters */
  LEARN_PARM  *  learn_parm;      /* svm-light learn parameters */ 
  long           kernel_cache_size; /* svm-light kernel parameter */

 } RT_SVM_VARS; 

/* --- advanced user command line help-string --- */
static char advanced_helpstring[] = "\n"
"3dsvm Advanced Usage:\n"
"---------------------\n\n"
"Usage:\n"
"  3dsvm [options] \n"
"\n\n"
"Options: \n"
"  -docout docname        Write training data to a SVM-light formated text-file.\n"
"                         (in addition to training/testing)\n"
"\n"
"  -doconly docname       Write training data to a SVM-light formated text-file.\n"
"                         (without training/testing)\n"
"\n"
"                         e.g. 3dsvm  -type classification \\ \n"
"                                     -trainvol run1+orig  \\ \n"
"                                     -trainlabels run1_categories.1D \\ \n"
"                                     -mask mask+orig \\ \n"
"                                     -doconly doc_run1 \n"
"\n"
"  -nopredscale          Do not scale predictions. Using this flag \n"
"                        with -nodetrend and -nopredcensored\n"
"                        will give results that are most closely comparable with \n"
"                        SVM-light predictions \n"
"\n"
"  -getenv               Will specify ALL command line options using the current environment\n"
"                        e.g. 3dsvm -getenv\n"
"\n"
"                        The use of environment variables was primarily motivated for plugin development.\n"
"                        Several environment variables are available - \n"
"                        here is a hypothetical example of a .afnirc file:\n"
"\n"
"                        AFNI_3DSVM_RT_TRAIN        = YES\n"
"                        AFNI_3DSVM_RT_TEST         = NO\n"
"                        AFNI_3DSVM_RT_IP           = 172.21.4.33\n"
"                        AFNI_3DSVM_RT_PORT         = 5000\n"
"                        AFNI_3DSVM_TRAIN_TYPE      = classification   // classification or regression\n"
"                        AFNI_3DSVM_TRAIN_DSET      = ~/data/trn_small+orig\n"
"                        AFNI_3DSVM_TRAIN_LBLS      = ~/data/lbls_trn_small.1D\n"
"                        AFNI_3DSVM_MASK_DSET       = ~/msk_small+orig\n"
"                        AFNI_3DSVM_MODEL_DSET      = ./xxx_model_small+orig // used for training and testing\n"
"                        AFNI_3DSVM_BUCKET_DSET     = ./xxx_bucket_small\n"
"                        AFNI_3DSVM_ALPHA_FILE      = ./xxx_alpha_small\n"
"                        AFNI_3DSVM_PARM_C          = 1000\n"
"                        AFNI_3DSVM_PARM_EPS        = 0.001\n"
"                        AFNI_3DSVM_KERNEL_TYPE     = linear   // linear, polynomial, rbf, sigmoid\n"
"                        AFNI_3DSVM_KERNEL_PARM_D   = 3\n"
"                        AFNI_3DSVM_KERNEL_PARM_G   = 1.0\n"
"                        AFNI_3DSVM_KERNEL_PARM_S   = 1.0\n"
"                        AFNI_3DSVM_KERNEL_PARM_R   = 1.0\n"
"                        AFNI_3DSVM_TEST_DSET       = ~/data/tst_small+orig\n"
"                        AFNI_3DSVM_TEST_LBLS       = ~/data/lbls_tst_small.1D\n"
"                        AFNI_3DSVM_PRED_FILE       = ./xxx_pred_small\n"
"                        AFNI_3DSVM_MCLASS_TYPE     = DAG // DAG or vote\n"
"                        AFNI_3DSVM_NOMASK          = YES\n"
"                        AFNI_3DSVM_NODETREND       = NO\n"
"                        AFNI_3DSVM_CENSOR_FILE     =~/data/censors.1D\n"
"\n"
"\n\n";

/* --- command line help-string --- */
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
"-mask mname            Specify a mask dataset to only perform the analysis\n" 
"                       on non-zero mask voxels.\n"
"                       ++ If '-mask' is not used '-nomodelmask must be\n"
"                          specified. \n"
"                       For example, a mask of the whole brain can be \n"
"                       generated by using 3dAutomask, or more specific ROIs\n" 
"                       could be generated with the Draw Dataset plugin or\n" 
"                       converted from a thresholded functional dataset. \n"
"                       The mask is specified during training but is also \n"
"                       considered part of the model output and is \n"
"                       automatically applied to test data. \n"
"\n"
"-nomodelmask           Flag to enable the omission of a mask file. This is \n"
"                       required if '-mask' is not used.\n"
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
"                       See also -censor.\n"
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
"-max_iterations int    Specify the maximum number of iterations for the\n" 
"                       optimization. 1 million [default].\n"
"\n"
"-alpha aname           Write the alphas to aname.1D \n"
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
"------------------- TRAINING AND TESTING MUST SPECIFY MODNAME ------------------\n"
"-model modname         modname = basename for the model brik.\n"
"\n"                       
"                       Training: modname is the basename for the output\n"
"                       brik containing the SVM model\n"
"\n"
"                           3dsvm -trainvol run1+orig \\ \n"
"                                 -trainlabels run1_categories.1D \\ \n"
"                                 -mask mask+orig \\ \n"
"                                 -model model_run1\n"
"\n"                      
"                       Testing: modname is the name for the input brik\n"
"                       containing the SVM model.\n"
"\n"
"                           3dsvm -testvol run2+orig \\ \n"
"                                 -model model_run1+orig  \\ \n"
"                                 -predictions pred2_model1\n"
"\n"
"-nomodelfile           Flag to enable the omission of a model file. This is \n"
"                       required if '-model' is not used during training. \n"
"                       ** Be careful, you might not be able to perform testing!\n"
"\n"
"------------------- TESTING OPTIONS --------------------------------------------\n"
"-testvol tstname       A 3D or 3D+t AFNI brik dataset to be used for testing. \n"
"                       A major assumption is that the training and testing  \n"
"                       volumes are aligned, and that voxels are of same number, \n"
"                       volume, etc. \n"
"\n"
"-predictions pname     pname = basename for .1D prediction file(s). \n"
"                       Prediction files contain a single column, where each line \n"
"                       holds the predicted value for the corresponding volume in\n"
"                       the test dataset. By default, the predicted values take \n"
"                       on a continuous range; to output integer-valued class\n"
"                       decision values use the -classout flag.\n"
"                       For classification: Values bellow 0.5 correspond to \n"
"                       (class A) and values above 0.5 to (class B), where A < B. \n"
"                       For more than two classes a separate prediction file for \n"
"                       each possible pair of training classes and one additional \n"
"                       \"overall\" file containing the predicted (integer-valued)\n"
"                       class membership is generated.\n" 
"                       For regression: Each value is the predicted parametric rate \n"
"                       for the corresponding volume in the test dataset. \n"
"\n"
"-classout              Flag to specify that pname files should be integer-\n"
"                       valued, corresponding to class category decisions.\n"
"\n"
"-nopredcensored        Do not write predicted values for censored time-points\n"
"                       to predictions file.\n"
"\n"
"-nodetrend             Flag to specify that pname files should NOT be \n"
"                       linearly detrended (detrending is performed by default).\n"
"                       ** Set this options if you are using GLM beta maps as\n"
"                          input for example. Temporal detrending only \n"
"                          makes sense if you are using time-dependent\n"
"                          data (chronological order!) as input.\n"
"\n"
"-nopredscale           Do not scale predictions. If used, values below 0.0 \n"
"                       correspond to (class A) and values above 0.0 to\n"
"                       (class B).\n"
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
"                       see http:\\\\lacontelab.org\\3dsvm.html for details and\n"
"                       references.\n"
"\n"
"------------------- INFORMATION OPTIONS ---------------------------------------\n"
"-help                  this help\n"
"\n"
"-version               print version history including rough description\n"
"                       of changes\n"
"\n\n";


/* --- plugin helpstring --- */
/* this string is currently at its maximum length */
static char plugin_helpstring[] = "\n"
"+++++++++++++++ 3dsvm: support vector machine analysis of brain data  +++++++++++++++\n\n"

"This plugin provides the ability to perform support vector machine \n"
"(SVM) analyses (training and testing) using SVM-Light (version 5),\n"
"developed by Thorsten Joachims, (http://svmlight.joachims.org/).\n"
"\n"
"General notes:\n"
"--------------\n"
"  This plugin gui provides basic functionality and the most common \n"
"  control options. More control options are available with the \n"
"  command-line version, 3dsvm. For example, the full set of \n"
"  SVM-Light command line options are available in 3dsvm. \n"
"\n"
"Using the Plugin:\n"
"-----------------\n"
" - The user can choose to perform SVM training and testing either \n"
"alone or together.  This is done by selecting the \"Training\" and \n"
"or \"Testing\" options.  The interface is organized by rows:\n"
"\n"
"  1)  Training - Select this option to perform SVM training. \n"
"\n"
"    a.  Type - Choose classification or regression.\n"
"               classification: labels for stimulus/behavioral categories\n"
"               regression: labels for parametric tasks\n"
"\n"
"  2)  Train Data - Perform SVM learning using the data specified.\n"
"\n"
"    a.  Dataset - Choose a 3D+t training dataset from the current session.\n"
"\n"
"    b.  Labels - Choose a .1D file indicating the class labels or regression\n"
"                 values for each TR in the dataset. (one value per line).\n"
"\n"
"    For classification, labels can take on any of the following values: \n"
"       0    - class 0\n"
"       1    - class 1\n"
"       n    - class n\n"
"       9999 - censor this point\n"
"\n"
"    For regression, labels can take any value. Censoring is only possible \n"
"    through a censor file.\n"
"\n"
"    c.  Censors - Choose a .1D file to ignore training samples. To ignore\n"
"                  a specific sample, put a 0 in the line corresponding to\n"
"                  that TR (i.e., to censor the first TR place a 0 in the \n"
"                  first line). All samples to be included must have a 1 \n"
"                  in the corresponding line.\n"
"\n"
"  3)  Train Params - parameters to control training. \n"
"\n"
"    a.  Mask - The plugin requires a mask to specify the voxels included\n"
"               in the SVM analysis. For example, a mask of the whole brain\n"
"               can be generated by using 3dAutomask. Or ROIs could be \n"
"               specified with the Draw Dataset plugin or converted from\n"
"               a thresholded functional dataset. The mask is specified \n"
"               during training but is also part of the model and is \n"
"               automatically applied to the test data. \n"
"\n"  
"    b.  C - An SVM parameter that represents the trade off between \n"
"    the training error and the margin.  Default value is 100. \n"
"\n"
"    c.  Epsilon - For regression, the SVM loss function is insensitive \n" 
"    to training errors whos absolute value are smaller than epsilon. \n" 
"    Default value is 0.1. \n"
"\n"
"  4)  Kernel Params - Kernel parameters to control training\n"
"\n"
"    a. Kernel Type - Choose a kernel function \n"
"         linear      : <u,v>  [Default] \n"
"         polynomial  : (s<u,v> + r)^d \n"
"         rbf         : radial basis function exp(-g ||u-v||^2) \n"
"         sigmoid     : tanh(s <u,v> + r)) \n"
"\n"
"    b. 'poly order (d)' - For the polynomial kernel select parameter 'd'\n"
"\n"
"    c. 'rbf gamma (g)' - For the rbf kernel select parameter 'g'\n"
"\n"
"  5)  Model Output \n"
"\n"
"    a.  Prefix - enter a prefix for the basename of the output model \n"
"\n"
"  6)  Model Inspection - specify output files to examine the SVM model.\n"
"\n"
"    a.  FIM prefix - The user can choose to write out the sum of weighted \n"
"    linear support vectors to a functional (fim) brik file. This is \n"
"    one way to generating a map from linear kernel SVMs (see \n"
"    LaConte et al, 2005).\n" 
"\n"
"    b.  Alpha Prefix - save the alphas (Lagrange Multipliers) to a .1D file.\n"
"\n"
"  7)  Testing - Test a set of volumes using a training model.\n"
"  If both training and testing options are specified to be run by\n"
"  the plugin, the model produced by training is applied directly to\n"
"  the test data.  If testing is done alone, the SVM model file must\n"
"  be specified.\n" 
"\n"
"  8)  Test Data - Perform SVM testing using the data specified on \n"
"  this line.\n"
"\n"
"    a.  Dataset - The 3D or 3D+t dataset to test. \n"
"\n"
"    b.  Model - The AFNI BRIK/HEAD file is generated by SVM training. \n"
"    Currently, if training and testing are done at once then a model\n"
"    should not be selected here.\n"
"\n"
"  9)  Predictions - Output .1D prediction file(s)\n"
"  Prediction files contain a single column. Each line holds the\n"
"  predicted  value for the corresponding volume in the test dataset\n"
"  These values are continuous. For classification: 0.5 is the default\n"
"  threshold. For more than two classes a separate prediction file is\n"
"  written for each pair of training classes and one additional \n"
"  \"overall\" file containing the predicted (integer-valued) \n"
"  class membership is generated.\n"
"\n"
"    a.  Prefix - The prefix for the .1D prediction file(s).\n"
"\n"
"  10)  'True' Labels - A .1D label file that contains the true class values \n"
"  of the samples in the test dataset.  It is used to calculate the \n"
"  performance of the SVM test. If this option is not chosen, then \n"
"  performance calculations (like prediction accuracy) are not made. \n"
"  Note: Prediction accuracy and RMS errors are sent to STDOUT\n"
"\n"
"    a.  File - Choose the .1D file with the 'True' labels. \n"
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
"+++++++++ 3dsvm: real-time SVM  +++++++++\n\n"
"\n"
"This plugin is integrated into AFNI's real-time framework and \n"
"provides the ability to perform real-time support vector machine (SVM) \n"
"analysis using SVM-Light (V5.00) developed by Thorsten Joachims,\n"
"(See http://svmlight.joachims.org/).\n"
"\n"
"General notes:\n"
"--------------\n"
"  A connection between the scanner and AFNI is required for real-time\n"
"  experiments and AFNI must be configured before data acquisition.\n"
"  See README.realtime for more details.\n"
"\n"
"  This plugin gui provides basic functionality and the most common \n"
"  control options. More control options are available for real-time \n"
"  analysis via the plugout_drive command. See README.3dsvm.realtime \n"
"\n"
"Using the Plugin: \n"
"------------------\n"
"\n"
"  The user can choose to either perform SVM training or SVM testing. \n"
"  This is done by selecting the 'Training' or the 'Testing' option.\n"
"  The interface is organized by rows: \n"
"\n"
"    1) Training - Select this option to perform SVM training. \n"
"\n"
"       a. Type - Choose classification or regression for training.\n"
"             classification: labels represent stimulus/behavioral categories\n"
"                             NOTE: only 2-class classification - for now. \n"
"             regression:     labels for parametric tasks\n"
"\n"
"\n"
"    2) Train Data - Perform SVM training using the data specified.   \n"
"\n"
"       a. Labels - Choose a .1D file indicating the class labels \n"
"                     or regression values for each TR that will be \n"
"                     acquired (one value per line).           \n"
"\n"
"                     For classification, labels can take on the following \n"
"                     values: \n"
"                     0    - class 0\n"
"                     1    - class 1\n"
"                     9999 - censor this time point\n"
"                     See also the next section on censors.\n"
"\n"
"                     For regression, labels can take any value. \n" 
"                     Censoring is only possible through a censor file.\n"
"\n"
"       b. Censors - Choose a .1D file to ignore training samples. \n"
"                    To ignore a specific sample, put a 0 in the line \n"
"                    corresponding to that TR (i.e., to censor the first TR\n"
"                    place a 0 in the first line). All samples to be \n"
"                    included must have a 1 in the corresponding line.\n" 
"\n"
"    3) Train Params - Parameters to control training.\n"
"\n"
"       a.  Mask  - The plugin requires a mask to specify the voxels\n"
"                     included in the SVM analysis. For example, a mask \n"
"                     of the whole brain can be generated by using \n"
"                     3dAutomask. Or ROIs could be specified with the \n"
"                     Draw Dataset plugin or converted from a thresholded \n"
"                     functional dataset. The mask is specified during \n"
"                     training but is also part of the model and is \n"
"                     automatically applied to the test data. \n"
"\n"
"       b.  C     - An SVM parameter that represents the trade off \n"
"                     between the training error and the margin. \n"
"                     Default value is 100.\n"
"\n"
"       c.  Epsilon - For regression, the SVM loss function is \n"
"                       insensitive to training errors whos absolute \n"
"                       value are smaller than epsilon. Default value is 0.1.\n"
"\n"
"   4) Kernel Params - Kernel parameters to control training \n"
"\n"
"       a.  Kernel Type - Choose a kernel function\n"
"              linear     : <u,v>  [Default]\n"
"              polynomial : (s<u,v> + r)^d\n"
"              rbf        : radial basis function exp(-gamma ||u-v||^2)\n"
"              sigmoid    : tanh(s <u,v> + r))\n"
"\n"
"       b. poly order (d) - For the polynomial kernel select parameter 'd'\n"
"\n"
"       c. rbf gamma (g) - For the rbf kernel select parameter 'g'\n"
"\n"
"   5)  Model Output \n"
"\n"
"       a.  Prefix - enter a prefix for the basename of the output model \n"
"\n"
"   6)  Model Inspection - specify output files to examine the SVM model\n"
"\n"
"       a.  FIM prefix - The user can write out the sum of weighted\n"
"                          linear support vectors to a functional (fim) \n"
"                          brik file. This is one way to generate a map \n" 
"                          from linear kernel SVMs (see LaConte et al, 2005).\n"
"\n"
"       b.  Alpha Prefix - save the alphas (Lagrange Multipliers) to a \n"
"                            .1D file.\n"
"\n"
"   7)  Testing - Select this option to perform SVM testing. \n"
"      \n"
"   8)  Test Data - Perform SVM testing using the model specified on \n"
"       this line. The actual test data are arriving in real time!\n"
"\n"
"       a. Model - Select a model file for testing. This model \n"
"                    is generated from independent training data. \n"
"\n"
"  9)  Predictions - Output a .1D file with the values of the \n"
"                      classification or regression results.\n"
"                      The values are continuous - even for classification.\n"
"                      By default 0.5 is the threshold for class 0 and \n"
"                      class 1. \n"
" \n"
"       a.  Prefix   - The prefix for the .1D predictions output file.\n"
"\n"
" 10)  Stimulus - Send the prediction results to a stimulus computer via\n"
"                   TCP/IP.\n"
"\n"
"       a.  IP - Specify the IP of the stimulus computer receiving the \n"
"           prediction results.\n"
"\n"
"       b.  PORT - Specify the port of the stimulus computer receiving the\n"
"           prediction results.\n"
"\n\n\n";


/* ---- string with contributions ---- */
static char contribution_string [] =
"Significant programming contributions by: \n"
"\n"
"  Jeff W. Prescott, William A. Curtis, Ziad Saad, Rick Reynolds, \n"
"  R. Cameron Craddock, Jonathan M. Lisinski, and  Stephen M. LaConte \n"
"\n"

"Original version written by JP and SL, August 2006 \n"
"Released to general public, July 2007 \n"
"\n"
"Questions/Comments/Bugs - email slaconte@vtc.vt.edu \n"
"\n\n"
"Reference:\n"
"LaConte, S., Strother, S., Cherkassky, V. and Hu, X. 2005. Support vector\n" 
"    machines for temporal classification of block design fMRI data. \n"
"    NeuroImage, 26, 317-329.\n"
"\n"
"Specific to real-time fMRI:\n"
"S. M. LaConte. (2011). Decoding fMRI brain states in real-time. \n"
"    NeuroImage, 56:440-54.\n"
"S. M. LaConte, S. J. Peltier, and X. P. Hu. (2007). Real-time fMRI using \n"
"brain-state classification. Hum Brain Mapp, 208:1033–1044. \n"
"\n"
"Please also consider to reference:\n"
"T. Joachims, Making Large-Scale SVM Learning Practical.\n"
"     Advances in Kernel Methods - Support Vector Learning,\n"
"     B. Schoelkopf and C. Burges and A. Smola (ed.), MIT Press, 1999.\n"
"\n"
"RW Cox. AFNI: Software for analysis and visualization of\n"
"    functional magnetic resonance neuroimages.\n"
"    Computers and Biomedical Research, 29:162-173, 1996.\n"
"\n";

/*----- String that briefly describes changes -------------------*/
static char change_string[] = "\n"
"V1.30 (11/21/17)\n"
"  1) Write prediction and alpha output files using four significant digits,\n"
"     since Lagrange multipliers (alphas) are currently written/read using\n"
"     single precision.\n"
"\n"
"V1.29 (10/31/17)\n"
"  1) Added flag: -nomodelfile.\n"
"\n"
"V1.28 (10/25/15)\n"
"  1) Bugfix: Flag -nopredcensored was not working for multi-class.\n"
"\n"
"V1.27 (07/21/15)\n"
"  1) Bugfix: Forgot to update plug_3dsvm in V1.26. Thanks Rick!\n"
"\n"
"V1.26 (07/07/15)\n"
"  1) Bugfix: Model was not initialized properly during regression training\n"
"     when excluding timepoints (-censor). This might have caused a\n"
"     crash (depending on platform) during testing\n"
"  2) Changed alpha file output (-alpha) for regression. Always write both\n"
"     sets of alphas even if they are zero\n" 
"\n"
"V1.25 (08/25/14)\n"
"  1) Bugfix: Non-linear kernels were not working for regression and caused\n"
"     3dsvm to crash. Classification was not affected by this.\n"
"\n"
"V1.24 (04/03/14)\n"
"  1) Allow data type short and float for the mask (previously only byte).\n"
"  2) Fixed option -classout and mulit-class accuracy report for non-\n"
"     continuous class labels (e.g., {4, 1, 9} instead of {0, 1, 2})\n"
"\n"
"V1.23 (08/16/13)\n"
"  1) Bugfix: Fixed -censor during testing.\n"
"\n"
"V1.22 (01/04/12)\n"
"  1) Bugfix: Fixed -max_iterations flag.\n"
"\n"
"V1.21 (01/04/12)\n"
"  1) Bugfix: 3dsvm real-time plugin caused AFNI crash for subsequent runs.\n"
"\n"
"V1.20 (10/18/11)\n"
"  1) Integrated the 3dsvm plugin into AFNI's real-time framework, which\n" 
"     enables SVM-based experiments in real time.\n"
"  2) Improved stability and error handling of the plugin.\n"
"\n"
"V1.13 (07/26/11)\n"
"  1) Added option -max_iterations. Allows user to specify maximum\n"
"     number of iterations for optimization. Default: 1 million iterations.\n"
"\n"
"V1.12 (10/26/10)\n"
"  1) Bugfix: Model was written incorrectly for datum type float\n"
"     (only .HEAD no .BRIK)\n"
"\n"
"V1.11 (09/31/10)\n"
"  1) Improved error checking for censor file and fixed a bug in deter- \n"
"     mining the length of the censor file.\n"
"\n"
"V1.10 (09/08/10)\n"
"  1) Removed restriction that class labels had to start from 0 and be \n"
"     continuous integers.  Now it is possible to have, say, a labels.1D that\n"
"     has {3,10,14}. Previously the user would have had to rename these\n"
"     as {0,1,2}.\n"
"  2) Mask is now the last (two - don't ask why) brik(s) in the model dataset.\n"
"     Previously the mask was a separate file. If you have old models laying\n"
"     around. We have kept backwards compatibility to handle this.\n"
"  3) Writing command-line history to bucket header now (still also writing\n"
"     to model header).\n"
"  4) Added option -version: Print 3dsvm's and SVM-light's version and brief\n"
"     description of changes\n"
"  5) Added option -HELP: Advanced user command-line options (mainly for\n"
"     debugging).\n"
"\n"
"V1.10 (09/08/10)\n"
"  1) Added support for non-continuous (arbitrary positive integers) class labels\n"
"  2) Merged model dataset and model-mask dataset to a single model dataset\n"
"  3) Writing command-line history to bucket header now\n"
"  4) Added option -version: Print 3dsvm's and SVM-light's version\n"
"     and rough description of changes\n"
"  5) Added option -HELP: Advanced user command-line options\n"
"\n"
"V1.00 (05/14/08) \n"
"  1) Added support for datum type float\n"
"  2) Changed internal representation for datasets to float\n"
"  3) Fixed a memory allocation bug for multi-class strings, that caused\n"
"     crashes for more than ~ 6 classes\n"
"  3) Writing weight-vector-maps (into the bucket) as float now\n"
"  4) Enabled SVM-light-provided kernels in regression\n"
"  5) Writing the b-value(s) of the model into bucket header now\n"
"  6) Added version number and version date\n"
"\n"
"Circa Nov 2009\n"
"  1) fixed a memory allocation bug that caused crashes in linux\n"
"  2) fixed a bug in bucket file that caused a one pixel shift in weight\n"
"     vector map \n"
"  3) enabled SVM-light-provided kernels\n"
"  4) enabled SVM-light regression\n"
"  5) combined multiclass bucket output to actually be a bucket, rather than\n"
"     individual briks\n"
"\n"
"Circa Nov/Dec 2008\n"
"  Note that 3dsvm's -predictions files have always been correct, however changes\n"
"  1 and 3 (below) are important for those who only rely on prediction accuracy \n"
"  summaries.\n\n"
"  1) Fixed a bug in calculating prediction accuracies.\n"
"  2) Changed multiclass for testvols - old method may have had problems in\n"
"     special cases. Now using DAG and Max Wins voting for or one vs. one\n"
"     multiclass.\n"
"  3) Improved handling of prediction accuracy calculations for censored test\n"
"     data labels\n"
"\n\n";

/* --- JL Jul 2011: Added funciton prototypes --- */
void           print_version( void );
int            detrend_linear_cnsrs( float *, LABELS *, char * );
void           write_svmLight_doc( DOC *, long , long , LabelType *, char *, char * ); 
void           printASLoptions( ASLoptions* );
void           printAfniModel( AFNI_MODEL * );
void           printArgv( char **, int *);
void           argvAppend( char **, int *, char *, char * );
void           freeArgv( char **, int );
void           getEnvArgv( char **, int *, char *);
int            argvCheck( char **, int *, char *, char * );
void           getAllocateCmdlArgv( char *, char *, int *, char *** ); 
double         cpxtwonorm_sq( WORD *a );
char *         trimString( char * );
long           getFileSize( char * );
double **      Allocate2d( long, long );
void           free2d( double **, long );
void           Clear2d( double **, long, long );
float **       Allocate2f( long , long );
void           free2f( float **, long );
void           Clear2f( float **, long, long );
DatasetType ** Allocate2DT( long, long );
void           free2DT( DatasetType **, long );
void           Clear2DT( DatasetType **, long, long );
char **        Allocate2c( long, long );
void           Clear2c( char **, long );
void           free2c( char **, long );
int            compare_ints( const int *, const int * );
DOC *          allocateDOCs( long , long ); 
void           freeDOCs( DOC *, long ); 
int            allocateMultiClassArrays( float ***, float **, float **, int **, 
                  int **, long, long, long, char * );
void           freeMultiClassArrays( float **, float *, float *, int*, int *, long );
DatasetType**  getAllocateDsetArray(THD_3dim_dataset *, char *);
void           freeDsetArray( THD_3dim_dataset *, DatasetType ** );
int            allocateModel( MODEL *, AFNI_MODEL *, char * ); 
void           freeModel( MODEL *, AFNI_MODEL *, enum modes );
void           updateModel( MODEL *, AFNI_MODEL *, int );
void           freeModelArrays( DatasetType**, MaskType*, long, int );
int            getAllocateModelArrays( THD_3dim_dataset*, DatasetType***, 
                   MaskType**, long *, long *, int *, int, char * );
int            get_svm_model( MODEL *, DatasetType **, MaskType *, AFNI_MODEL *,
                   long, int, char * );
int            readAllocateAfniModel( THD_3dim_dataset *,  AFNI_MODEL *, char * );
int            allocateModelMaps( MODEL_MAPS *, long, long, char * );
void           freeModelMaps( MODEL_MAPS * );
void           addToModelMap_bucket( MODEL_MAPS *, AFNI_MODEL *, DatasetType **, 
                   MaskType *, char *, long );
int            writeModelMap_bucket ( MODEL_MAPS *, MaskType *, THD_3dim_dataset *,
                   char *, char *, float *, long, ASLoptions*, int, char ** , char *);
void           writeModelMask( THD_3dim_dataset *, MaskType*, char * ); 
int            writeModelBrik( AFNI_MODEL *, THD_3dim_dataset *, DatasetType **, 
                   MaskType *, ASLoptions*, char *, int , char **, char *);
void           addToAfniModel( AFNI_MODEL *, MODEL *, LEARN_PARM *, LabelType *,
                   ASLoptions *, long, long, int, int );
DatasetType** getAllocateCensoredRegressionArray( DatasetType **, LABELS *, 
                  long );
void          freeCensoredRegressionArray(DatasetType **, LABELS *);
void          getClassTrainArrayAndTarget( DatasetType **, LabelType *, 
              DatasetType **, LabelType *, long, long );
void          afni_dset_to_svm_doc( DOC *, DatasetType **, MaskType*, long, 
                  long, long );
int           getCensoredClassTarget( LabelType *, long *, LABELS *, long, 
                  long, enum modes, char *);
void          getTmpLabels( LabelType *, long *, LABELS *, long, long );
void          freeAfniModel( AFNI_MODEL *);
int           allocateAfniModel( AFNI_MODEL *, LABELS *, ASLoptions *, char *);
void          freeAfniModelAndArrays( AFNI_MODEL *, DatasetType **, MaskType *,
                  long nt_model );
int           readAllocateAfniModelAndArrays( ASLoptions *, AFNI_MODEL *, 
              THD_3dim_dataset *, DatasetType ***, MaskType **dsetMaskArray, 
                  long *, long *, enum modes, int *, char * );
void          freeClassificationLabels( LABELS * );
int           getAllocateClassificationLabels (LABELS *, char *, char *, char * );
int           getAllocateRegressionLabelsAndTarget( LABELS *, LabelType **, char *, 
              char *, char * );
void          freeRegressionLabelsAndTarget( LABELS *, LabelType * );
int           test_classification( ASLoptions *, MODEL *, AFNI_MODEL *, 
              THD_3dim_dataset *, DatasetType **, MaskType *, long, long, 
                  int, char **, char * );
int           test_rt( DatasetType **, long, double *, char *errorString );

int           test_regression( ASLoptions *, MODEL *, AFNI_MODEL *, 
              THD_3dim_dataset *, DatasetType **, MaskType *, long, long, int,
                  char **, char * );
int           train_classification( MODEL *, LEARN_PARM *, KERNEL_PARM *, long *,
              ASLoptions *,THD_3dim_dataset *, THD_3dim_dataset *, MaskType *, 
                  int, char **, char *);
int           train_regression( MODEL *, LEARN_PARM *, KERNEL_PARM *, long *, 
              ASLoptions *, THD_3dim_dataset *, THD_3dim_dataset *, MaskType *,
                  int, char **, char * );
int           ppi (int , int , char *);
int           input_parse(int , char **, long *, long *, LEARN_PARM *, 
              KERNEL_PARM *, ASLoptions*, enum modes *, int *, char *);
