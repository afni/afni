#include "3dsvm_common.h"
#include "svm_learn.c"
#include "debugtrace.h"

# define PROGRAM_NAME   "3dsvm"    /* name of this program - used to include commandline history in model */

/* from svm_classify.c - copied directly (print_help) for now since this file also has main in it */
void print_help_classify(void)
{
  printf("\nSVM-light %s: Support Vector Machine, classification module     %s\n",VERSION,VERSION_DATE);
  copyright_notice();
  printf("   usage: svm_classify [options] example_file model_file output_file\n\n");
  printf("options: -h         -> this help\n");
  printf("         -v [0..3]  -> verbosity level (default 2)\n");
  printf("         -f [0,1]   -> 0: old output format of V1.0\n");
  printf("                    -> 1: output the value of decision function (default)\n\n");
}

/* from svm_learn_main.c - copied directly (print_help - omitting the wait_any_key()) for now since this file also has main in it */
void print_help_learn()
{
  printf("\nSVM-light %s: Support Vector Machine, learning module     %s\n",VERSION,VERSION_DATE);
  copyright_notice();
  printf("   usage: svm_learn [options] example_file model_file\n\n");
  printf("Arguments:\n");
  printf("         example_file-> file with training data\n");
  printf("         model_file  -> file to store learned decision rule in\n");

  printf("General options:\n");
  printf("         -?          -> this help\n");
  printf("         -v [0..3]   -> verbosity level (default 1)\n");
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
  printf("    Kernel Methods - Support Vector Learning, B. Schölkopf and C. Burges and\n");
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


void detrend_linear_cnsrs(float *data, float *data_cnsrs, LABELS *labels)
{
  int nt, ntc, n_t, n_tcnsrs;

  n_t = labels->n;
  n_tcnsrs = n_t - labels->n_cnsrs;

  for(nt = 0, ntc = 0; nt < n_t, ntc < n_tcnsrs; nt++) {
    if((labels->lbls[nt] != 9999) && (labels->lbls[nt] != -9999)) {
      data_cnsrs[ntc] = data[nt];
      ntc++;
      }
  }

  DETREND_linear(n_t, data);
  DETREND_linear(n_tcnsrs, data_cnsrs);
 
  for(nt = 0, ntc = 0; nt < n_t, ntc < n_tcnsrs; nt++) {
    if((labels->lbls[nt] != 9999) && (labels->lbls[nt] != -9999)) {
      data[nt] = data_cnsrs[ntc];
      ntc++;
      }
  }
  return; 
}

unsigned long int getFileSize(char *fileName)
{
    FILE *fp;
    unsigned long int lineCount=0;
    char str[400];

    if( (fp = fopen(fileName, "r")) == NULL ) {
        fprintf(stderr,"getFileSize: error - can not open file\n");
        exit(0);
    }

    while( !feof(fp) ) {
        fgets(str,390,fp);
        lineCount ++;
    }
    lineCount --;

    fclose(fp);

    return(lineCount);
}

/****************************************************************
 * Allocate2f()                                                 *
 * farr[index1][index2]				                *
 ****************************************************************/
float **Allocate2f(long index1, long index2)
{
  long i;
  float **farr;

  if(   ( farr = (float **)malloc(index1*sizeof(float *)) )   ) {
    for(i = 0; i < index1; i++) {
      if(   ( farr[i] = (float *)malloc(index2*sizeof(float)) )   );
      else {
        fprintf(stderr,"Allocate2f: error\n");
        fprintf(stderr,"Attemted to allocate [%ld][%ld]\n", index1,index2);
        fprintf(stderr,"Successfully allocated [%ld][%ld]", i-1,index2);
        exit(0);
      }
    }
  }
  else {
    printf("Allocate2f: error\n");
    exit(0);
  }
  return(farr);
}

/*************************************************************/
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

/****************************************************************
 * Allocate2DT()  -- Datasetype                                 *
 * arr[index1][index2]                                          *
 ****************************************************************/
DatasetType **Allocate2DT(long index1, long index2)
{
  long i;
  DatasetType **arr;

  if(   ( arr = (DatasetType **)malloc(index1*sizeof(DatasetType *)) )   ) {
    for(i = 0; i < index1; i++) {
      if(   ( arr[i] = (DatasetType *)malloc(index2*sizeof(DatasetType)) )   );
      else {
        fprintf(stderr,"Allocate2DT: error\n");
        fprintf(stderr,"Attemted to allocate [%ld][%ld]\n", index1,index2);
        fprintf(stderr,"Successfully allocated [%ld][%ld]", i-1,index2);
        exit(0);
      }
    }
  }
  else {
    printf("Allocate2DT: error\n");
    exit(0);
  }
  return(arr);
}

/*************************************************************/
void free2DT(DatasetType **x, long index1)
{
  long i;

  for(i = 0; i < index1; i++) {
    free(x[i]);
  }
  free(x);
  
  return;
} 


/*-----------------------------------------------------------*/
int compare_ints( const int *a, const int *b ) {
  int tmp = *a - *b;
  if( tmp > 0 )
    return 1;
  else if( tmp < 0 )
    return -1;
  else
    return 0;
}
/*-----------------------------------------------------------*/


void AllocateDOCwords(DOC *docs, long ndocsTime, long nvoxelWords)
{
  long i;
  for( i=0; i < ndocsTime; ++i ) {
    docs[i].words = (WORD*)malloc(sizeof(WORD)*(nvoxelWords+1));
  }
}

/*-----------------------------------------------------------*/

void freeDOCwords(DOC *docs, long ndocsTime)
{
  long i;
  for( i=0; i < ndocsTime; ++i ) {
    free(docs[i].words);
  }
}
/*-----------------------------------------------------------*/

void allocateModel( MODEL *model, AFNI_MODEL *afni_model)
{
  long i;

  /* our approach to multiclass is to keep all training timepoints 
   * with non-support vectors as alpha = 0
   * thus the model "documents" and number of support vectors is
   * always the number of timepoints in in the training data
   */

  model->supvec = (DOC **)my_malloc(sizeof(DOC *)*(afni_model->timepoints+1));
  model->alpha = (double *)my_malloc(sizeof(double)*(afni_model->timepoints+1));
  for(i = 1; i < afni_model->timepoints + 1; i++) {  /* (timpoints +1) is svmlights number of support vectors */
    model->supvec[i] = (DOC *)calloc(sizeof(DOC),1);
    (model->supvec[i])->words = (WORD *)calloc(sizeof(WORD),afni_model->total_masked_features[0] + 1); /* +1 for end of list value */
												/* [0] assumes  that all models use the same mask */
  }

  if(afni_model->kernel_type[0] == 0 ) {
    model->lin_weights=(double *)my_malloc(sizeof(double)*(afni_model->total_masked_features[0] + 1));
  }
}

/*-----------------------------------------------------------*/
void freeModel( MODEL *model, AFNI_MODEL *afni_model)
{
  long i;
  free(model->alpha);
  for(i = 1; i < afni_model->timepoints + 1; i++) {  /* (timpoints +1) is svmlights number of support vectors */
    free( (model->supvec[i])->words );
    free(model->supvec[i]);
  }
  free(model->supvec);

  if(model->kernel_parm.kernel_type == 0 ) {
    free(model->lin_weights);
  }
}
/*-----------------------------------------------------------*/
void updateModel(MODEL *model, AFNI_MODEL *afni_model, int comb) /* fill in all values for the first (index 0) class combination */
{
  long i;

  model->kernel_parm.kernel_type = afni_model->kernel_type[comb];
  model->kernel_parm.poly_degree = afni_model->polynomial_degree[comb];
  model->kernel_parm.rbf_gamma = afni_model->rbf_gamma[comb];
  model->kernel_parm.coef_lin = (double) afni_model->linear_coefficient[comb];
  model->kernel_parm.coef_const = (double) afni_model->constant_coefficient[comb];
  model->totwords = (long) afni_model->total_masked_features[comb];
  model->b = (double) afni_model->b[comb];

  /* our approach to multiclass is to keep all training timepoints 
   * with non-support vectors as alpha = 0
   * thus the model "documents" and number of support vectors is
   * always the number of timepoints in in the training data
   */
  model->totdoc = (long) afni_model->timepoints;
  model->sv_num = (long) afni_model->timepoints + 1;

  /*----- ALPHAS ------------------*/
  for( i=0 ; i< model->sv_num - 1 ; ++i ) {
    model->alpha[i+1] = (double)afni_model->alphas[comb][i];
  }

  if( model->kernel_parm.kernel_type == 0 ) {         
    /* essentially replacing call to add_weight_vector_to_linear_model(model)*/
    /* that function mallocs, which we don't want since we are re-using */
    clear_vector_n(model->lin_weights,model->totwords);
    for(i=1;i<model->sv_num;i++) {
      add_vector_ns(model->lin_weights,(model->supvec[i])->words, model->alpha[i]);
    }
  }

  if(verbosity >= 2) {
    printf( "sv_num = %ld\n", model->sv_num );
    printf( "kernel_type = %ld\n", model->kernel_parm.kernel_type ); 
    printf( "poly_degree = %ld\n", model->kernel_parm.poly_degree ); 
    printf( "rbf_gamma = %lf\n", model->kernel_parm.rbf_gamma ); 
    printf( "coef_lin = %lf\n", model->kernel_parm.coef_lin ); 
    printf( "coef_const = %lf\n", model->kernel_parm.coef_const ); 
    printf( "totwords = %ld\n", model->totwords ); 
    printf( "totdoc = %ld\n", model->totdoc );
    printf( "b = %lf\n", model->b );
  }
}

/*-----------------------------------------------------------*/
/* just fills in the model data set (assumed constant accross class combinations) */
/* Need to also use updateModel for class */
/* The idea is to only call this once and then updateModel for combination specific aspects */
void get_svm_model(MODEL *model, DatasetType **dsetModelArray, MaskType *dsetMaskArray, AFNI_MODEL *afni_model, long model_vox, int noMaskFlag)
{
  long i,j,k;

  for(i = 1; i < afni_model->timepoints + 1; i++) {  /* number of support vectors is (afni_model->timepoints + 1) */
						     /*	this simplifies multi-class life by allowing us to essentially */
						     /* store and read the training data once, one brik, etc. */
						     /* the real number is the number of non-zero alphas */
    k = 0;
    for( j=0 ; j< model_vox; ++j) {
      // if( dsetMaskArray[j] && (k < (long) afni_model->total_masked_features[0]) ) --- before dealing with noMaskFlag, used this
      if( k < (long) afni_model->total_masked_features[0] ) { /* [0] assumes mask is the same for all class combinations */
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
/*-----------------------------------------------------------*/


void readAllocateAfniModel( THD_3dim_dataset *dsetModel,  AFNI_MODEL *afniModel)
{
  ATR_float *atr_float;
  ATR_int *atr_int;
  ATR_string *atr_string;
  long i,j;
  char p[100],*q; /* used for strtok magic */
  char headernames[100];

   ENTRY("readAllocateAfniModel");
   
  atr_int = THD_find_int_atr( dsetModel->dblk, "CLASS_COUNT" );
  afniModel->class_count = *atr_int->in;

  atr_int = THD_find_int_atr( dsetModel->dblk, "CLASS_COMBINATIONS" );
  afniModel->combinations = *atr_int->in;

  atr_int = THD_find_int_atr( dsetModel->dblk, "TIMEPOINTS" );
  afniModel->timepoints = *atr_int->in;

  atr_string = THD_find_string_atr( dsetModel->dblk, "COMBO_NAMES" );
  strcpy(p,atr_string->ch);
  q = strtok(p,",");
  if (q != NULL) strcpy(afniModel->combName[0],q);
  else {
    printf("error reading model combinations in header file\n");
    exit(0);
  }
  for(i = 1; i < afniModel->combinations; ++i) {
    q=strtok(NULL,",");
    if (q != NULL) strcpy(afniModel->combName[i],q);
    else {
      printf("error reading model combinations in header file - number does not match expected(%d)\n",afniModel->combinations);
      exit(0);
    }
  }

  atr_int = THD_find_int_atr( dsetModel->dblk, "KERNEL_TYPE" );
  afniModel->kernel_type = (int *)malloc( atr_int->nin * sizeof(int) );
  for( i=0 ; i<atr_int->nin ; ++i ) {
    afniModel->kernel_type[i] = atr_int->in[i];
  }

  atr_int = THD_find_int_atr( dsetModel->dblk, "POLYNOMIAL_DEGREE" );
  afniModel->polynomial_degree = (int *)malloc( atr_int->nin * sizeof(int) );
  for( i=0 ; i<atr_int->nin ; ++i ) {
    afniModel->polynomial_degree[i] = atr_int->in[i];
  }

  atr_float = THD_find_float_atr( dsetModel->dblk, "RBF_GAMMA" );
  afniModel->rbf_gamma = (float *)malloc( atr_float->nfl * sizeof(float) );
  for( i=0 ; i<atr_float->nfl ; ++i ) {
    afniModel->rbf_gamma[i] = atr_float->fl[i];
  }

  atr_float = THD_find_float_atr( dsetModel->dblk, "LINEAR_COEFFICIENT" );
  afniModel->linear_coefficient = (float *)malloc( atr_float->nfl * sizeof(float) );
  for( i=0 ; i<atr_float->nfl ; ++i ) {
    afniModel->linear_coefficient[i] = atr_float->fl[i];
  }

  atr_float = THD_find_float_atr( dsetModel->dblk, "CONSTANT_COEFFICIENT" );
  afniModel->constant_coefficient = (float *)malloc( atr_float->nfl * sizeof(float) );
  for( i=0 ; i<atr_float->nfl ; ++i ) {
    afniModel->constant_coefficient[i] = atr_float->fl[i];
  }

  atr_int = THD_find_int_atr( dsetModel->dblk, "TOTAL_MASKED_FEATURES" );
  afniModel->total_masked_features = (int *)malloc( atr_int->nin * sizeof(int) );
  for( i=0 ; i<atr_int->nin ; ++i ) {
    afniModel->total_masked_features[i] = atr_int->in[i];
  }

  atr_int = THD_find_int_atr( dsetModel->dblk, "TOTAL_SAMPLES" );
  afniModel->total_samples = (int *)malloc( atr_int->nin * sizeof(int) );
  for( i=0 ; i<atr_int->nin ; ++i ) {
    afniModel->total_samples[i] = atr_int->in[i];
  }

  atr_int = THD_find_int_atr( dsetModel->dblk, "TOTAL_SUPPORT_VECTORS" );
  afniModel->total_support_vectors = (int *)malloc( atr_int->nin * sizeof(int) );
  for( i=0 ; i<atr_int->nin ; ++i ) {
    afniModel->total_support_vectors[i] = atr_int->in[i];
  }

  atr_float = THD_find_float_atr( dsetModel->dblk, "B" );
  afniModel->b = (float *)malloc( atr_float->nfl * sizeof(float) );
  for( i=0 ; i<atr_float->nfl ; ++i ) {
    afniModel->b[i] = atr_float->fl[i];
  }

  afniModel->alphas = Allocate2f((long) afniModel->combinations, (long) afniModel->timepoints);
  for(i = 0; i < afniModel->combinations; ++i ) {
    sprintf(headernames,"ALPHAS_%s",afniModel->combName[i]);
    atr_float = THD_find_float_atr( dsetModel->dblk, headernames); 
    for(j = 0; j < afniModel->timepoints; ++j ) {
      afniModel->alphas[i][j] = (double)atr_float->fl[j];
    }
  }
   

   EXRETURN;

}

void addToModelMap_brick(AFNI_MODEL *afni_model, MODEL *model, THD_3dim_dataset *dsetTrain, THD_3dim_dataset *dsetMask, MaskType *dsetMaskArray, char *modelFile, double nvox, int comb0, int comb1)
{
  DatasetType* scaled_weights;
  double *lin_weights;
  THD_3dim_dataset* dsetWeights;
  char dsetModelWeightFile[MAX_FILE_NAME_LENGTH];
  int i,k;
  int ityp;

  /* may someday be doing more than linear kernels */
  if( model->kernel_parm.kernel_type == 0 ) {
    /* as in the test_routine function, updateModel, */
    /* but probably not quite as clean since not pre-allocating and re-using */
    lin_weights=(double *)my_malloc(sizeof(double)*(afni_model->total_masked_features[0] + 1));
    clear_vector_n(lin_weights,model->totwords);
    for(i = 1; i<model->sv_num; i++) {
      add_vector_ns(lin_weights,(model->supvec[i])->words, model->alpha[i]);
    }

    scaled_weights = (DatasetType*)calloc(nvox,sizeof(DatasetType));
    k = 0;                                  /* count for feature number if using mask */
    for( i=0 ; i<nvox ; ++i ) {   /* fill in model */
      if( dsetMask ) {                       /* if mask is being used */
        if( dsetMaskArray[i] ) {
          scaled_weights[i] += (short)(SCALE*lin_weights[k]);
          ++k;
        }
      } 
      else                                   /* if no mask is being used */
        scaled_weights[i] = (short)(SCALE*lin_weights[i]);
    }

    dsetWeights = EDIT_empty_copy( dsetTrain ); 

    sprintf( dsetModelWeightFile, "%s_%d_%d", modelFile, comb0, comb1);
    

    EDIT_dset_items( dsetWeights,
        ADN_prefix, dsetModelWeightFile,
        ADN_label1, dsetModelWeightFile,
        ADN_type, 1,                    /* functional dataset */
        ADN_func_type, 0,               /* fim functional type */
        ADN_nvals, 1,
        ADN_ntt, 0,                       /* number of time points (?) */
        ADN_none );
    ityp = DSET_BRICK_TYPE( dsetWeights, 0 );     /* ityp: 0=byte, 1=short, 2=float, 3=complex */

    EDIT_substitute_brick( dsetWeights, 0, ityp, scaled_weights );

    tross_Copy_History( dsetTrain, dsetWeights ) ;

    printf( "Writing weight dataset..." );
    THD_write_3dim_dataset( "./", dsetModelWeightFile, dsetWeights, True );
    printf( "done\n" );

    free( scaled_weights );
    free( lin_weights );
  }
}


/*-----------------------------------------------------------*/
void writeModelMask( THD_3dim_dataset *dsetMask, MaskType* dsetMaskArray, char *fileName) 
{
  char maskCopyName[MAX_FILE_NAME_LENGTH];
  THD_3dim_dataset* dsetMaskCopy;
  int ityp;
   ENTRY("writeModelMask");
  /* Write out model mask (actually, just a simple copy of mask used) */
  sprintf( maskCopyName, "%s%s", fileName, MODEL_MSK_EXT );
  dsetMaskCopy = EDIT_empty_copy(dsetMask);
  EDIT_dset_items( dsetMaskCopy,
        ADN_prefix, maskCopyName,
        ADN_label1, maskCopyName,
        ADN_type, 1,                    /* functional dataset */
        ADN_func_type, 0,               /* fim functional type */
        ADN_nvals, 1,
        ADN_ntt, 0,                       /* number of time points (?) */
        ADN_none );
  ityp = DSET_BRICK_TYPE( dsetMask, 0 );     /* ityp: 0=byte, 1=short, 2=float, 3=complex */

  EDIT_substitute_brick( dsetMaskCopy, 0, ityp, dsetMaskArray );

  tross_Copy_History( dsetMask, dsetMaskCopy );
  tross_Append_History( dsetMaskCopy, "a 3dsvm copy") ;

  printf("Writing model dataset mask...\n");
  THD_write_3dim_dataset( "./", maskCopyName, dsetMaskCopy, True );
  EXRETURN;
}

/*-----------------------------------------------------------*/
void writeModelBrik(AFNI_MODEL *afniModel, THD_3dim_dataset *dsetTrain, char *fileName, int argc, char **argv)
{
  THD_3dim_dataset *dsetModel;
  char csv_combName[LONG_STRING];	/* to put in header file - comma seperated "names" of class category combinations */
  char headernames[LONG_STRING];	/* holds name for each alpha section in the .HEAD */
  long i;

  dsetModel = EDIT_full_copy( dsetTrain, fileName );

  strcpy(csv_combName, afniModel->combName[0]);
  for(i = 1; i < afniModel->combinations; ++i) {
    strcat(csv_combName, ",");
    strcat(csv_combName, afniModel->combName[i]);
  }

  /***  modified from 3dDeconvolve.c ***/
  /*----- Record history of dataset -----*/
  tross_Copy_History( dsetTrain, dsetModel);

  { char * commandline = tross_commandline( PROGRAM_NAME , argc , argv ) ;
    tross_Append_History ( dsetModel, commandline);
    free(commandline) ;
  }

  THD_set_int_atr( dsetModel->dblk, "CLASS_COUNT", 1, &afniModel->class_count);
  THD_set_int_atr( dsetModel->dblk, "CLASS_COMBINATIONS", 1, &afniModel->combinations);
  THD_set_int_atr( dsetModel->dblk, "TIMEPOINTS", 1, &afniModel->timepoints);
  THD_set_string_atr( dsetModel->dblk, "COMBO_NAMES", csv_combName); 
  THD_set_int_atr( dsetModel->dblk, "KERNEL_TYPE", afniModel->combinations, afniModel->kernel_type);
  THD_set_int_atr( dsetModel->dblk, "POLYNOMIAL_DEGREE", afniModel->combinations, afniModel->polynomial_degree);
  THD_set_float_atr( dsetModel->dblk, "RBF_GAMMA", afniModel->combinations, afniModel->rbf_gamma);
  THD_set_float_atr( dsetModel->dblk, "LINEAR_COEFFICIENT", afniModel->combinations, afniModel->linear_coefficient);
  THD_set_float_atr( dsetModel->dblk, "CONSTANT_COEFFICIENT", afniModel->combinations, afniModel->constant_coefficient);
  THD_set_int_atr( dsetModel->dblk, "TOTAL_MASKED_FEATURES", afniModel->combinations, afniModel->total_masked_features);
  THD_set_int_atr( dsetModel->dblk, "TOTAL_SAMPLES", afniModel->combinations, afniModel->total_samples);
  THD_set_int_atr( dsetModel->dblk, "TOTAL_SUPPORT_VECTORS", afniModel->combinations, afniModel->total_support_vectors);
  THD_set_float_atr( dsetModel->dblk, "B", afniModel->combinations, afniModel->b );
  for(i = 0; i < afniModel->combinations; ++i) {
    sprintf(headernames,"ALPHAS_%s",afniModel->combName[i]);
    THD_set_float_atr( dsetModel->dblk, headernames, afniModel->timepoints, afniModel->alphas[i] );
  }

  printf( "Writing model dataset..." );
  THD_write_3dim_dataset( "./", fileName, dsetModel, True );
  printf( "\n" );
}

void addToAfniModel(AFNI_MODEL *afniModel, char *alphaFile, MODEL *model, LabelType *tmp_labels, long classCount, long sampleCount, int comb0, int comb1)
{
  FILE *fp;
  long alphaCount;
  long i;
  double *tmp_alphas;

  /* it is difficult to access the alpha values without making modifications to the svmlight
   * source. So, for now, we are just reading in the file output by svmlight
   *******************************************************************************************/
  tmp_alphas = (double *)malloc(sizeof(double)*sampleCount);
  if( tmp_alphas  == NULL ) {
    fprintf(stderr, "memory allocation error! could not allocate tmp_alphas.\n");
    exit(0);
  }

  if( (fp = fopen(alphaFile,"r")) == NULL ) {
    fprintf( stderr,"Could not open .1D alpha file: %s\n", alphaFile );
    exit(0);
  }
  i = 0; 
  while( !feof(fp) ) {
    fscanf(fp,"%lf\n",&tmp_alphas[i]);
    ++i;
  }
  fclose(fp);

  alphaCount = 0;
  for(i = 0; i < afniModel->timepoints; ++i) {
    if( abs(tmp_labels[i]) == 1) {
      afniModel->alphas[classCount][i] = tmp_alphas[alphaCount]; 
      ++alphaCount;
    }
    else {
      afniModel->alphas[classCount][i] = 0.0;
    }
  }

  sprintf( afniModel->combName[classCount], "%d_%d", comb0, comb1 );

  afniModel->kernel_type[classCount] = model->kernel_parm.kernel_type; 
  afniModel->polynomial_degree[classCount] = model->kernel_parm.poly_degree; 
  afniModel->rbf_gamma[classCount] = model->kernel_parm.rbf_gamma; 
  afniModel->linear_coefficient[classCount] = model->kernel_parm.coef_lin; 
  afniModel->constant_coefficient[classCount] = model->kernel_parm.coef_const; 
  afniModel->total_masked_features[classCount] = (int) model->totwords;
  afniModel->total_samples[classCount] = (int) model->totdoc;
  afniModel->total_support_vectors[classCount] = (int) model->sv_num;
  afniModel->b[classCount] = model->b; 


  free(tmp_alphas);
}

void afni_dset_to_svm_doc( DOC *docs, DatasetType **dsetArray, MaskType* maskArray, long tpts, long nvoxels, long nmasked )
{
  long i, j, k;    /* loop indices */
  for( i=0; i < tpts; ++i ) {
    docs[i].docnum = i;
    docs[i].queryid = 0;
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
    docs[i].twonorm_sq=sprod_ss(&docs[i].words[0],&docs[i].words[0]);
  }
}

/*-----------------------------------------------------------*/
void getTmpLabels(LabelType *tmp_labels,long *sampleCount, LABELS *labels, long ind0, long ind1)
{
  long i;
  short labelWarningFlag = 0;  /* warn users if unknown class label - probably from multi-class */
  int class0 = labels->class_list[ind0];
  int class1 = labels->class_list[ind1];

  *sampleCount = 0;

  for( i=0 ; i<labels->n ; ++i) {   /* convert timeseries input to one that can be used with svm light. */
    if( (int)labels->cnsrs[i] ) {
      if( labels->lbls[i] == class0 ) {      
        tmp_labels[i] = -1;
        (*sampleCount)++;
      }
      else if( labels->lbls[i] == class1 ) { /* class b */
        tmp_labels[i] = 1;
        (*sampleCount)++;
      }
      else if( labels->lbls[i] == -9999 ) {          /* transductive sample */
        tmp_labels[i] = 0;
        (*sampleCount)++;
      }
      else if( labels->lbls[i] == 9999 ) {           /* ignore sample */
        tmp_labels[i] = 9999;
      }
      else {
        tmp_labels[i] = 9999;                  /* invalid value - ignore */
        labelWarningFlag = 1;
        if(verbosity >= 2) printf("%ld,", i);                     /* ignored time point */
      }
    }
    else {
      tmp_labels[i] = 9999;                    /* censored sample - ignore */
    }
  }
 if( labelWarningFlag && (verbosity >= 1) ) {
   printf("--time points ignored. If not using multi-class, check for bad labels.\n");
 }

}

void freeAfniModel(AFNI_MODEL *afniModel)
{
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

  free2f(afniModel->alphas,afniModel->combinations);
  EXRETURN;
}

void allocateAfniModel(AFNI_MODEL *afniModel,LABELS *labels)
{
  afniModel->timepoints = (int) labels->n;  /* would like to be long, but no equivalent to THD_set_int_atr */
  afniModel->class_count = (int) labels->n_classes;	/* would like to be long, but no equivalent to THD_set_int_atr */
  afniModel->combinations = (long) ( (labels->n_classes * (labels->n_classes - 1)) / 2 );
  afniModel->kernel_type = (int *)malloc( afniModel->combinations * sizeof(int) );
  afniModel->polynomial_degree = (int *)malloc( afniModel->combinations * sizeof(int) );
  afniModel->rbf_gamma = (float *)malloc( afniModel->combinations * sizeof(float) );
  afniModel->linear_coefficient = (float *)malloc( afniModel->combinations * sizeof(float) );
  afniModel->constant_coefficient = (float *)malloc( afniModel->combinations * sizeof(float) );
  afniModel->total_masked_features = (int *)malloc( afniModel->combinations * sizeof(int) );
  afniModel->total_samples = (int *)malloc( afniModel->combinations * sizeof(int) );
  afniModel->total_support_vectors = (int *)malloc( afniModel->combinations * sizeof(int) );
  afniModel->alphas = Allocate2f((long) afniModel->combinations, (long) afniModel->timepoints);
  afniModel->b = (float *)malloc( afniModel->combinations * sizeof(float) );
  /* allocation for alphas is checked in Allocate2f */
  if( afniModel->kernel_type == NULL || afniModel->polynomial_degree == NULL || afniModel->rbf_gamma == NULL || 
   afniModel->linear_coefficient == NULL || afniModel->constant_coefficient == NULL || 
   afniModel->total_masked_features == NULL || afniModel->total_samples == NULL || 
   afniModel->total_support_vectors == NULL || afniModel->b == NULL ) 
  {
    fprintf(stderr, "memory allocation error! could not allocate afniModel members.\n");
    exit(0);
  }

}

void freeLabels(LABELS *labels)
{
  ENTRY("freeLabels");
  // free labels.labels
  free(labels->lbls);

  // free labels.censors
  free(labels->cnsrs);
   EXRETURN;
}

//void getLabels(LABELS *labels,ASLoptions *options)
void getLabels(LABELS *labels, char *labelFile, char *censorFile)
{
  FILE *fp;
  int class_exists_flag = 0;
  int nine_exists_flag = 0;
  long i,j,k;

   ENTRY("getLabels");

  labels->n = getFileSize(labelFile);
     
  /*----- RETRIEVE LABEL FILE --------------*/
  labels->lbls = (LabelType*)malloc(sizeof(LabelType)*labels->n);
  if( labels->lbls == NULL ) {
    fprintf(stderr, "memory allocation error! could not allocate labels.\n");
    exit(0);
  }
    
  if( (fp = fopen(labelFile,"r")) == NULL ) {
    fprintf(stderr,"Could not open .1D label file: %s\n",labelFile);
    exit(0);
  }

  for(i = 0; i < labels->n; i++) {
      fscanf( fp,"%lf\n",&(labels->lbls[i]) );
  }
  fclose(fp);

  /*----- INITIALIZATION --------------*/
  for( j=0 ; j<CLASS_MAX ; ++j ) {
    labels->class_list[j] = 9999;     
  }

  /*----- Determine number of classes in label file --------------*/
  /* i indexes all time points
  ** j indexes over total allowed classes (CLASS_MAX)
  ** k increments as each new class label is found
  */
  labels->n_classes = 0;
  labels->n_cnsrs = 0;
  k = 0;
  for( i=0; i < labels->n; ++i ) {
    if (labels->lbls[i] < 0.0 && labels->lbls[i] != -9999) {
      fprintf(stderr, "\n"
                      "ERROR: labelfile contains a negative entry in line %ld. \n" 
                      "       Check labelfile '%s' \n\n", i+1, labelFile);
      exit(-1);
    }
    if( (labels->lbls[i] != 9999) && (labels->lbls[i] != -9999) ) {
      for( j=0; j < CLASS_MAX; ++j ) {
        if( labels->lbls[i] == labels->class_list[j] ) {
          ++j;
          class_exists_flag = 1;
        }
      }
      if( !class_exists_flag ) {
        labels->class_list[k] = labels->lbls[i];
        ++labels->n_classes;
        ++k;
      }
      else {
        class_exists_flag = 0;
      }
    }
    else { 
      labels->n_cnsrs++;
      nine_exists_flag = 1;
    }
  }

  qsort( labels->class_list, CLASS_MAX, sizeof(int), (void *)compare_ints );

  if(verbosity >= 1) {
    printf( "number of classes = %d\n", labels->n_classes );
    for( i = 0; i < labels->n_classes; ++i ) {
      printf( "class[%ld] = %d, ", i, labels->class_list[i] );
    }
    printf("\n");
  }
   if (labels->n_classes >= CLASS_MAX) {
      fprintf(stderr,"Error:\n"
                     "Max numer of classes hard coded to %d\n"
                     "Complain to the authors if you need more.\n",
                     CLASS_MAX-1);
      exit(1);
   }
  /*----- RETRIEVE CENSOR FILE --------------*/
  labels->cnsrs = (LabelType*)malloc(sizeof(LabelType)*labels->n);
  if( labels->cnsrs == NULL ) {
    fprintf(stderr, "memory allocation error! could not allocate censors.\n");
    exit(0);
  }

  if( censorFile[0] ) {
    if( (fp = fopen(censorFile,"r")) == NULL ) {
      fprintf(stderr,"Could not open .1D censor file: %s\n",censorFile);
      exit(0);
    }
    if (nine_exists_flag == 1) { // Hey SL, You may want do this only if verbosity >= 2
      // -SL- I just might. Let's wait and see. Hey JL, how was your weekend?
      fprintf(stderr, "\n"
                      "WARNING: labelfile '%s' contains censor information and \n"
                      "         censorfile '%s' was specified as well. \n"
                      "         Censoring data specified in both files. \n",labelFile, censorFile);
    }
    i = 0;
    while( !feof(fp) ) {
      if( i < labels->n ) {
        fscanf( fp,"%lf\n",&(labels->cnsrs[i]) );
      }
      else  {
        fprintf(stderr,"warning: censorFile (%s) is longer than expected length( %ld ).",censorFile, labels->n); 
        fprintf(stderr,"Make sure there are no extra lines at the end of this file.\n");
        exit(0);
      }
      ++i;
    }
    fclose(fp);
  }
  else {
    for(i = 0; i < labels->n; ++i) {
       labels->cnsrs[i] = 1;
    }
  }

}

void test_routine (ASLoptions *options, MODEL *model, THD_3dim_dataset *dsetTest, THD_3dim_dataset *dsetMask, THD_3dim_dataset *dsetModel, int argc, char **argv)
{
  AFNI_MODEL afniModel;			   
  char* inModelFile;
  char inModelFileMask[MAX_FILE_NAME_LENGTH];
  char* inModelFileMaskExt = MODEL_MSK_EXT;
  long nt;                      /* number of time points in TEST dataset */
  long nvox;                    /* number of voxels per time point in TEST dataset */
  long nt_mod, nvox_mod;	/* for MODEL dataset */
  DOC *docsTest;
  DatasetType **dsetTestArray;	/* array to hold test dataset values */
  DatasetType **dsetModelArray;	/* array to hold model dataset values */
  DatasetType *tmp_dsetArray;	/* temporary array to hold dataset values */
  MaskType *dsetMaskArray=NULL; /*ZSS: init to NULL*/
  float dist_tmp;
  float *dist;  /* really want this to be double, but am detrending - should do something smarter soon!*/
  float *dist_cnsrs;
  /* double *multiclass_dist;
   * double multiclassTmp;
   *
   * original variables for multiclass. For now only supporting directed acyclic graph. Future will
   * provide more options for users
   * 
   */
float **multiclass_dist;	/* doing all of the pairwise tests and storing them */
				/* in principle, don't have to do this with directed, acyclic graph (DAG)*/
				/* but each test does not take that long, and we may build in more options in the future. */
				/* this was originally a 1D array, and used "truth table" type of approach that could have */
				/* been slightly more robust but relied on the assumption that most distances would be */
				/* inside of their range ([-1,1] or transformed to [0,1]), for now, I think it is better */
				/* to stick with the DAG */


  FILE *fp=NULL;  /*ZSS: init to NULL*/

  char predictionsFile[MAX_FILE_NAME_LENGTH];
  long i,j,c;
  LABELS testLabels;
  LabelType *tmp_labels=NULL; /*ZSS: init to NULL*/
  char p[100],*q; /* used for strtok magic */
  long cc, dd;
  long sampleCount = 0;			/* number of samples used in training */
  float correct=0.0,incorrect=0.0,no_accuracy=0.0; /*ZSS: init to 0.0*/
  long res_a,res_b,res_c,res_d;
  int DAG=0; /* abbreviation for Directed Acyclic Graph: 
               index variable for traversing multiclass_dist   ZSS: init to 0 */
  short edgeFlag; /* DAG related */
  int classAssignment; /* multi-class related */
  float *classCorrect, *classIncorrect;
  long *nClass;
  int *classVote;    /* mulit-class vote */
  int currentComb, class0, class1, winningCount;    /* mulit-class vote */
  enum mctypes { MCTYPE_DAG, MCTYPE_VOTE };  /* classifyer types for multiclass */
  enum mctypes mctype = MCTYPE_DAG;          /* default value */

  /*----- LOAD TEST DATA --------*/
  dsetTest = THD_open_one_dataset( options->testFile );
  if ( dsetTest == NULL ) {
    fprintf( stderr, "Failed to open test dataset: %s\n", options->testFile );
    exit(0);
  }
  DSET_load( dsetTest );
  nt = DSET_NUM_TIMES( dsetTest );
  nvox = DSET_NVOX( dsetTest ); 

  /* GET TEST LABELS */
  if( options->testLabelFile[0] ) {
    getLabels(&testLabels,options->testLabelFile, options->censorFile); // included censorfile for testing (JL) 
    if(testLabels.n != nt) {
	fprintf(stderr,"\n"
            "ERROR: number of labels do not match the length of the test dataset: \n"
	    "       labelfile '%s' contains %ld labels, but the \n"
	    "       testfile '%s' contains %ld entries. \n\n",options->testLabelFile, testLabels.n, 
           options->testFile, nt);
           exit(0);
    }

    /*----- ALLOCATE tmp_labels --------------*/
    tmp_labels = (LabelType*)malloc(sizeof(LabelType)*testLabels.n);             /* for timeseries data */
    if( tmp_labels == NULL ) {
      fprintf(stderr, "test_routine: memory allocation error! could not allocate temporary labels.\n");
      exit(0);
    }
  }

  /*----- PRODUCE TEST DATA ARRAY -------*/
  dsetTestArray = Allocate2DT( nt, nvox);
  for( i=0; i < nt; ++i ) {
    tmp_dsetArray = (DatasetType*)DSET_ARRAY(dsetTest,i); /* AFNI functionality */
    for( j=0 ; j<nvox ; ++j ) {
      dsetTestArray[i][j] = tmp_dsetArray[j];   
    }
  }

  /*----- LOAD AFNI-SVM MODEL --------*/
  dsetModel = THD_open_one_dataset( options->modelFile );
  if ( dsetModel == NULL ) {
    fprintf( stderr, "Failed to open model dataset: %s\n", options->modelFile );
    exit(0);
  }
  DSET_load( dsetModel );
  nt_mod = DSET_NUM_TIMES( dsetModel );
  nvox_mod = DSET_NVOX( dsetModel );

  /*----- PRODUCE MODEL ARRAY ------------------*/
  dsetModelArray = Allocate2DT( nt_mod, nvox_mod);
  for(i = 0; i < nt_mod; ++i) {
    tmp_dsetArray = (DatasetType*)DSET_ARRAY(dsetModel,i);
    for (j = 0; j < nvox_mod; ++j) {
      dsetModelArray[i][j] = tmp_dsetArray[j];
    }
  }

  /*----- LOAD MODEL MASK --------*/
  if( !(options->outModelNoMask) ){
    inModelFile = DSET_PREFIX( dsetModel );
    strcpy(inModelFileMask,inModelFile);
    strcat(inModelFileMask,inModelFileMaskExt);
    strcat(inModelFileMask,"+orig"); // probably I will not like that I did this someday
    dsetMask = THD_open_one_dataset( inModelFileMask );
    if ( dsetMask == NULL ) {
      fprintf( stderr, "Failed to open mask dataset: %s. If not using a mask file, you must use option -nomodelmask\n", inModelFileMask );
      exit(0);
    }
    DSET_load(dsetMask);
    dsetMaskArray = (MaskType*)DSET_ARRAY(dsetMask,0);
  }

  readAllocateAfniModel(dsetModel, &afniModel);

 
  /*----- SETTING MCTYPE FOR MULTICLASS -----*/
  if ((options->multiclass[0]) && (afniModel.class_count > 2)) { 
      if ( !strcmp(options->multiclass,"DAG") ) mctype = MCTYPE_DAG;
      else if ( !strcmp(options->multiclass,"vote") ) mctype = MCTYPE_VOTE;
      else { 
          fprintf(stderr,"\n" 
		       "WARNING: -multiclass was specified with an unknown option: mctype = '%s'. \n"
               "         Setting mctype = DAG [Default]. \n", options->multiclass);       
        mctype = MCTYPE_DAG;
     }
  }
  else {
    if(verbosity >= 2) printf("\n Setting multiclass type to DAG [Default].\n");
  }

  /*----- ERROR CHECKING FOR MULTICLASS ----- */
   if ( options->testLabelFile[0] && verbosity >= 2 ) {
       if ( testLabels.n_classes != afniModel.class_count ) {     
            printf("\n"
		  "NOTE: number of class categories do not match:\n"
                  "         labelfile: '%s' contains %d categories, but the \n"
                  "         modelfile: '%s' contains %d categories. \n", options->testLabelFile, testLabels.n_classes, 
           options->modelFile, afniModel.class_count); 
       } 
   }        

   if ( (options->multiclass[0]) && (options->testLabelFile[0]) ) {
      if ( testLabels.n_classes < 3 ) { 
         printf("\n"
		"NOTE: -multiclass was specified, but labelfile '%s' contains only %d categories. \n", 
                 options->testLabelFile,testLabels.n_classes);
      }
   }
 


  /*----- FILL DOC STRUCTURE FROM TEST DATASET -----*/
  docsTest = (DOC*)malloc(sizeof(DOC)*nt);                           /* svm-light data structure */
  AllocateDOCwords(docsTest, nt, afniModel.total_masked_features[0]);

  /* assuming total_masked_features are all the same and same mask for training and testing */
  afni_dset_to_svm_doc( docsTest, dsetTestArray, dsetMaskArray, nt, nvox, afniModel.total_masked_features[0]); 

  allocateModel( model, &afniModel);
  get_svm_model(model, dsetModelArray, dsetMaskArray, &afniModel, nvox_mod, options->outModelNoMask);

  /*----- Allocate test predictions arrays --------*/
  dist = (float *)malloc(sizeof(float)*nt);
  dist_cnsrs = (float *)malloc(sizeof(float)*(nt-testLabels.n_cnsrs));
  /* Note: if not multiclass these may not get used - moreover, if only one multiclass approach
   *       still not everything will get used. So perhaps being a little inneficient here */
  /* multiclass_dist = (double *)calloc(sizeof(double),nt); -- SL Aug. 08*/
  multiclass_dist = Allocate2f((long) afniModel.combinations, (long) nt);
  classCorrect = (float *)malloc(sizeof(float)*afniModel.class_count);
  classIncorrect = (float *)malloc(sizeof(float)*afniModel.class_count);
  nClass = (long *)malloc(sizeof(long)*afniModel.class_count);
  classVote = (int *)malloc(sizeof(long)*afniModel.class_count);

  for(i = 0; i < afniModel.combinations; ++i ) {
    if(verbosity >= 1) {
      printf("\n--------------------------------------------------------------------------------\n");
      printf("category combination = %ld  (%s)\n",i,afniModel.combName[i]);
    }
 
    if( options->testLabelFile[0] ) {
      /* use strtok to recover combination name integers so that we can use the test label data */
      strcpy(p,afniModel.combName[i]);
      q = strtok(p,"_");
      cc = atol(q);
      q = strtok(NULL,"_");
      dd = atol(q);
    
      getTmpLabels(tmp_labels, &sampleCount, &testLabels, cc, dd);
      correct=0.0; 
      incorrect=0.0;
      no_accuracy=0.0;
      res_a=0.0;
      res_b=0.0;
      res_c=0.0;
      res_d=0.0;
    }

    /*----- GET SVM-LIGHT MODLE STRUCTURE -----*/
    updateModel(model, &afniModel, (int) i); 

    if(afniModel.class_count == 2) {
      sprintf(predictionsFile, "%s.1D", options->predFile);
    }
    else {
      sprintf(predictionsFile, "%s_%s.1D", options->predFile, afniModel.combName[i]);
    }
    if( (fp = fopen( predictionsFile, "w" )) == NULL ) {
      fprintf( stderr, "Could not open file for writing predictions: %s\n", predictionsFile );
      exit(0);
    }
    for(j = 0; j < nt; ++j){
      dist_tmp=classify_example_linear(model,&docsTest[j]);
      /* should do something smarter than re-casting double to float */
      dist[j]= (float) dist_tmp;
    }
   
    /* Nov. 08 : Changed detrending for censored timepoints */
    if( (testLabels.n_cnsrs != 0) && (!options->noPredDetrend)) {
      detrend_linear_cnsrs(dist, dist_cnsrs, &testLabels);
    }
    else {
      /* WC and SL Aug. 08 : moved this up so that detrending is done before accuracies are calculated */
      /* detrend in place - assuming no intercept (bias towards one class), or slope */
      if(!options->noPredDetrend) {
        DETREND_linear( (int) nt, dist );
      }
    }

    /* WC and SL Aug. 08 : now calculate the percent accuracy with the detrended data*/
    for(j = 0; j < nt; ++j){
      if( options->testLabelFile[0] && abs(tmp_labels[j]) != 9999) {
        if(dist[j]>0) {
          if(tmp_labels[j]>0) correct++; else incorrect++;
          if(tmp_labels[j]>0) res_a++; else res_b++;
        }
        else {
          if(tmp_labels[j]<0) correct++; else incorrect++;
          if(tmp_labels[j]>0) res_c++; else res_d++;
        }
      }
    }

    if(options->testLabelFile[0] && (verbosity>=1)) {
      printf("Accuracy on test set: %.2f%% (%d correct, %d incorrect, %ld total)\n",
              (float)(correct)*100.0/sampleCount,(int)rint(correct),(int)rint(incorrect),sampleCount);
      //can put this in after it is included for overall multiclass
      //printf("Precision/recall on test set: %.2f%%/%.2f%%\n",(float)(res_a)*100.0/(res_a+res_b),(float)(res_a)*100.0/(res_a+res_c));
    }


    for(j = 0; j < nt; ++j){
      /* multiclass_dist[j] += dist[j]; -- SL Aug. 08*/
      multiclass_dist[i][j] += dist[j];
      dist[j] = 0.5*( dist[j] + 1 ); /* convert output prediction to {0,1} class scale */
        if(options->classout) {  /* output integer class memberships */
	      dist[j] = rint(dist[j]); /* round (no rintf) */
	      if(dist[j] > 1) dist[j] = 1.0;
 	      if(dist[j] < 0) dist[j] = 0.0;
       }
      fprintf(fp,"%.8g\n",dist[j]);  
    }

    fclose(fp);
    if(verbosity >= 1)  printf("predictions written to %s\n",predictionsFile);
  }

  if(afniModel.class_count > 2) {

    if(mctype == MCTYPE_VOTE) {
        sprintf(predictionsFile, "%s_overall_vote.1D", options->predFile);
        if( (fp = fopen( predictionsFile, "w" )) == NULL ) {
            fprintf( stderr, "Could not open file for writing predictions: %s\n", predictionsFile );
            exit(0);
        }
    }
    else { // if (mctype == MCTYPE_DAG)
        sprintf(predictionsFile, "%s_overall_DAG.1D", options->predFile);
        if( (fp = fopen( predictionsFile, "w" )) == NULL ) {
            fprintf( stderr, "Could not open file for writing predictions: %s\n", predictionsFile );
            exit(0);
        }  
    }

    if( options->testLabelFile[0] ) {
      correct=0.0; 
      incorrect=0.0;
      no_accuracy=0.0;
      res_a=0.0;
      res_b=0.0;
      res_c=0.0;
      res_d=0.0;
      for(c = 0; c < afniModel.class_count; ++c) {
	classCorrect[c] = 0.0;
	classIncorrect[c] = 0.0;
	nClass[c] = 0L;
      }
    }

    /* Multiclass: voting method */
    if (mctype == MCTYPE_VOTE) {
    if(verbosity >= 1) printf("\n---------------------------------- vote ----------------------------------------\n");
        for(j = 0; j < nt; ++j) {
        /* code largely duplicated in DAG ................. */
	    if(verbosity >=2) {
                for(i = 0; i < afniModel.combinations; ++i) { 
                    printf("model number:%ld time point:%ld classifier output=%f\n",i,j,multiclass_dist[i][j]);
	    	    }
            }
            for(c = 0; c < afniModel.class_count; ++c) {
                classVote[c] = 0;
	        }
            classAssignment = 0;
            winningCount = 0;
            currentComb = 0; 
            for(class0 = 0; class0 < afniModel.class_count-1; ++class0) { 
                for(class1 = class0+1; class1 < afniModel.class_count; ++class1) { 
                
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
           
            if(verbosity >=2) printf("point number %ld:    ",j);
            for(i = 0; i < afniModel.class_count; ++i) { 
                if(verbosity >=2) printf("classVote[%ld] = %d;   ",i, classVote[i]);
            }
            if(verbosity >=2) printf("\n");
           
            /* code is largely duplicated in DAG ................. */
            if(verbosity >=2) printf("voting result: observation number=%ld model number=%d, classAssignment = %d\n",j, DAG, classAssignment);
            fprintf(fp,"%d\n", classAssignment);  
           
            if( (options->testLabelFile[0]) && ((int)testLabels.lbls[j] != 9999 )) {
                nClass[(int)testLabels.lbls[j]]++;
                if (classAssignment == testLabels.lbls[j] ) {
                    correct++; 
                    classCorrect[(int)testLabels.lbls[j]]++; 
                }
                else {
                    incorrect++;
                    classIncorrect[(int)testLabels.lbls[j]]++;
                }
                if(verbosity >=2) {
                    printf("Overall:  test labels=%d, current number correct = %d   incorrect = %d\n", (int) rint(testLabels.lbls[j]),
                            (int) rint(correct), (int) rint(incorrect));
                    for(c = 0; c < afniModel.class_count; ++c) {
		        printf( "Class Specific:  classLabel=%ld, current number correct = %d   incorrect = %d\n", c,
                                (int) rint(classCorrect[c]), (int) rint(classIncorrect[c]) );
                    }
                }
            }
        }
    }
   
    else { // if (mctype == MCTYPE_DAG)		
      if(verbosity >= 1) printf("\n---------------------------------- DAG -----------------------------------------\n");
      /* Multiclass:  Directed acyclic graph (DAG) */
        /*  Directed acyclic graph of pairwise classifiers
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
    
        if(verbosity >=2) printf("Verbosity >= 2: multiclass details (note decision threshold =0):\n");
        for(j = 0; j < nt; ++j) {
	        if(verbosity >=2) {
	            for(i = 0; i < afniModel.combinations; ++i) { 
		            printf("model number:%ld time point:%ld classifier output=%f\n",i,j,multiclass_dist[i][j]);
	            }
	        }  
	        DAG = afniModel.class_count - 2;
	        if(verbosity >= 2) printf("model number=%d:  ", DAG);
	        classAssignment = afniModel.class_count - 1; /* assuming class values [0,...,N-1] */
	        edgeFlag = 1;
	        for(i = 1; i < afniModel.class_count; ++i) { /* note: starting index at 1, and going through class_count-1 times*/
	            if(verbosity >= 2) printf("classifier output = %f  ",multiclass_dist[DAG][j]);
	            if(multiclass_dist[DAG][j]>0) {
		            if(edgeFlag) {
		                DAG += afniModel.class_count - i - 1;
		                if(verbosity >=2) printf("next model number=%d, current max possible classAssignment = %d\n", DAG, classAssignment);
		            }
		            else {
		                DAG += afniModel.class_count - i;
		                if(verbosity >=2) printf("next model number=%d, current max possible classAssignment = %d\n", DAG, classAssignment);
		           }
	            }
	            else {
		            edgeFlag = 0;
		            DAG--;
		            classAssignment--;
		            if(verbosity >=2) printf("next model number=%d, current max possible classAssignment = %d\n", DAG, classAssignment);
	           }
	        }

	        if(verbosity >=2) printf("DAG result: observation number=%ld model number=%d, classAssignment = %d\n",j, DAG, classAssignment);
            fprintf(fp,"%d\n", classAssignment);  

            if((options->testLabelFile[0]) && ((int)(testLabels.lbls[j] != 9999))) {
	            nClass[(int)testLabels.lbls[j]]++;
	            if (classAssignment == testLabels.lbls[j])  {
                    correct++; 
		            classCorrect[(int)testLabels.lbls[j]]++; 
	            }
	            else {
		            incorrect++;
		            classIncorrect[(int)testLabels.lbls[j]]++;
	            }
	            if(verbosity >= 2) {
		            printf("Overall:  test labels=%d, current number correct = %d   incorrect = %d\n", (int) rint(testLabels.lbls[j]),
                            (int) rint(correct), (int) rint(incorrect));
		            for(c = 0; c < afniModel.class_count; ++c) {
		                printf("Class Specific:  classLabel=%ld, current number correct = %d   incorrect = %d\n", c, 
                               (int) rint(classCorrect[c]),(int) rint(classIncorrect[c]) );
		            }
	            }
            }
        }
     }
  }

  fclose(fp);
  if(verbosity >= 1)  printf("predictions for all categories written to %s\n",predictionsFile);


/* this was original multiclass. For now only supporting 2-class DAG and voting - will add different methods in future. */
#if 0 
  if(afniModel.class_count > 2) {
    if(verbosity >= 1) printf("--------------------------------------------------------------------------------\n");
    sprintf(predictionsFile, "%s_overall.1D", options->predFile);
    if( (fp = fopen( predictionsFile, "w" )) == NULL ) {
      fprintf( stderr, "Could not open file for writing predictions: %s\n", predictionsFile );
      exit(0);
    }

    if( options->testLabelFile[0] ) {
      correct=0.0; 
      incorrect=0.0;
      no_accuracy=0.0;
      res_a=0.0;
      res_b=0.0;
      res_c=0.0;
      res_d=0.0;
    }
    for(j = 0; j < nt; ++j){
      multiclass_dist[j] = 0.5*( multiclass_dist[j]+afniModel.class_count-1 ); /* convert output prediction to {0,combinations-1} class scale */
      multiclassTmp = rint(multiclass_dist[j]);  /*round*/
      if(multiclassTmp > afniModel.class_count-1) multiclassTmp = (double) afniModel.class_count - 1.0;
      if(multiclassTmp < 0) multiclassTmp = 0.0;

      if( options->testLabelFile[0] ) {
	/*** SL - Aug. 08 - probably a mistake right here. index i should probably be j
 	 *** 			moot for now since not currently using this approach ***/
	if (multiclassTmp == testLabels.lbls[i] ) correct++; else incorrect++;
      }

      if(options->classout) {  /* output integer class memberships */
	multiclass_dist[j] = multiclassTmp;
      }
      fprintf(fp,"%.8g\n", multiclass_dist[j]);  
    }

    fclose(fp);
    if(verbosity >= 1)  printf("predictions for all categories written to %s\n",predictionsFile);
  }
#endif

  if(options->testLabelFile[0] && afniModel.class_count > 2 && (verbosity>=1)) {
      printf("Overall accuracy on multiclass test set: %.2f%% (%d correct, %d incorrect, %d total)\n",
             (float)(correct)*100.0/((int)rint(correct)+(int)rint(incorrect)),(int)rint(correct),(int)rint(incorrect),(int)rint(correct)+(int)rint(incorrect));
      printf( "Individual Breakdown:\n" );
      for(c = 0; c < afniModel.class_count; ++c) {
	  /* possible nan output if no examples of the current class, c (in otherwords divide by nClass = 0)*/
	      printf( "                       classLabel=%ld: %.2f%% (%d correct, %d incorrect, %ld total)\n", 
                  c, (float)(classCorrect[c])*100.0/nClass[c], (int)rint(classCorrect[c]), (int)rint(classIncorrect[c]), nClass[c] );
      }
  }

  free(dist);
  free(dist_cnsrs);
  free(classCorrect);
  free(classIncorrect);
  free(nClass);
  free(classVote);
  freeModel( model, &afniModel);
  freeAfniModel(&afniModel);
  freeDOCwords(docsTest, nt);
  free(docsTest);
  free2DT( dsetTestArray, nt );
  free2DT( dsetModelArray, nt_mod );
  DSET_unload( dsetModel );
  if( !(options->outModelNoMask) ){
    DSET_unload(dsetMask);
  }
  DSET_unload( dsetTest );
}

void train_routine(MODEL *model, LEARN_PARM *learn_parm, KERNEL_PARM *kernel_parm, ASLoptions *options, 
                   THD_3dim_dataset *dsetTrain, THD_3dim_dataset *dsetMask, 
                   MaskType *dsetMaskArray, int argc, char **argv)
{
  LABELS labels;
  AFNI_MODEL afniModel;			/* holds everything required to write out model.Head */
  long classCount;			/* in training loop, keeps track of current pairwise comb */
  long sampleCount = 0;			/* number of samples used in training */
  LabelType *tmp_labels, *target;  
  DatasetType **dsetTrainArray;		/* array to hold training dataset values */
  DatasetType *tmp_dsetTrainArray;	/* temporary array to hold training dataset values */
  DOC*    docsTrain;               /* SVM-Light data structure used for training */
  KERNEL_CACHE kernel_cache;
  long nt;                       /* number of time points in TRAIN dataset */
  long nvox;                     /* number of voxels per time point in TRAIN dataset */
  long nvox_masked = 0;
  long i,j,k, cc, dd;

   ENTRY("train_routine");
   
  /*----- LOAD TRAINING DATA --------*/
  dsetTrain = THD_open_one_dataset( options->trainFile );
  if ( dsetTrain == NULL ) {
    fprintf( stderr, "Failed to open training dataset: %s\n", options->trainFile );
    exit(0);
  }
  DSET_load( dsetTrain );

  nt = DSET_NUM_TIMES( dsetTrain );
  nvox = DSET_NVOX( dsetTrain );
  nvox_masked = nvox;                /* this will be modified later if mask used */
  if(verbosity >= 1)  printf( "Number of time samples is %ld, and voxels %ld in training dataset.\n",nt,nvox );

  /*----- GET MASK ARRAY, IF SELECTED AND DETECT nvox_masked --------*/
  if( options->maskFile[0] ) {
    nvox_masked = 0;
    dsetMask = THD_open_one_dataset( options->maskFile );
    if ( dsetMask == NULL ) {
      fprintf(stderr, "Failed to open mask file: %s\n", options->maskFile );
      exit(0);
    }
    DSET_load(dsetMask);
    if( DSET_BRICK_TYPE(dsetMask,0) != MRI_byte ) {
      fprintf(stderr, "mask file: %s is not a byte-format brik.\n", options->maskFile );
      exit(0);
    }
    dsetMaskArray = (MaskType*)DSET_ARRAY(dsetMask,0);
    for( i=0 ; i<nvox ; ++i ) {
      if( dsetMaskArray[i] )
        nvox_masked++;
    }
    if(verbosity >= 1) printf( "The number of non-zero elements in mask is: %ld\n", nvox_masked );
  }
  else if( !(options->outModelNoMask) ){
    fprintf(stderr, "No mask file specified (use -mask file). If not using a mask file must use option -nomodelmask\n");
    exit(0);
  }

  /*----- RETRIEVE LABELS --------------*/
  labels.n = nt;
  getLabels(&labels,options->labelFile, options->censorFile);

  /*----- ALLOCATE afniModel --------------*/
  allocateAfniModel(&afniModel,&labels);

  /*----- ALLOCATE tmp_labels --------------*/
  tmp_labels = (LabelType*)malloc(sizeof(LabelType)*labels.n);             /* for timeseries data */
  if( tmp_labels == NULL ) {
    fprintf(stderr, "train_routine: memory allocation error! could not allocate temporary labels.\n");
    exit(0);
  }

  /*----- svmlearn for all combinations of pairwise training --------*/
  /* cc indexes -1 class; dd indexes +1 class - over multiple classes */
  classCount = 0; /* could figure it out from cc and dd, but easier just to keep track */
  for( cc=0 ; cc < labels.n_classes-1; ++cc ) {
    for( dd=cc+1 ; dd < labels.n_classes; ++dd ) {
      
      if(verbosity >= 1)  { 
         printf(  "\nPreparing classes %d and %d: \n", 
                  labels.class_list[cc], labels.class_list[dd]);
         if (verbosity > 1) MCHECK ; fflush(stdout) ; /* ZSS */
      }   

      getTmpLabels(tmp_labels, &sampleCount, &labels, cc, dd);
      if(verbosity >= 1) printf( "sampleCount = %ld\n", sampleCount );

      /*----- ALLOCATE MEMORY for svmlight arrays -----------*/
      docsTrain = (DOC*)malloc(sizeof(DOC)*sampleCount);
      target = (LabelType*)malloc(sizeof(LabelType)*sampleCount);
      if( docsTrain == NULL || target == NULL ) {
        fprintf(stderr, "train_routine: memory allocation error! could not allocate docsTrain and/or target.\n");
        exit(0);
      }

      dsetTrainArray = Allocate2DT( sampleCount, nvox);

      /*----- PRODUCE TRAINING ARRAY -------*/
      /* Extract volume sample at given time point, place in array  */
      k = 0;		/* index for each training sample not ignored */
      for( i=0; i < labels.n; ++i ) {
        if( tmp_labels[i] != 9999 ) {      /* if sample is not supposed to be ignored */
          target[k] = tmp_labels[i];      /* set target value (+1,-1, or 0) */
          tmp_dsetTrainArray = (DatasetType*)DSET_ARRAY(dsetTrain,i); /* AFNI functionality */
          for( j=0 ; j<nvox ; ++j ) {
            dsetTrainArray[k][j] = tmp_dsetTrainArray[j];   /* populate training array with sample */
          }
          ++k;
        }
      }

      /* model file output may not be required if training and testing */
      /* performed at same time. */
      if(options->modelFile[0]) {
        char alphaFile[MAX_FILE_NAME_LENGTH];		
        if( options->modelAlphaFile[0] ) { 	/* user defined alpha file name */
          sprintf( alphaFile, "%s_%d_%d.1D", options->modelAlphaFile, labels.class_list[cc], labels.class_list[dd] );
          strcpy( learn_parm->alphafile, alphaFile);
        }
        else {				/* default alpha file naming */
        /**** should find a better solution ****/
        //  sprintf( alphaFile, "%s_%d_%d_alphas.1D", options->modelFile, labels.class_list[cc], labels.class_list[dd] );
          strcpy( learn_parm->alphafile, ".tmp_3dsvm_alphas.1D");
        }
      }

      AllocateDOCwords(docsTrain, sampleCount, nvox_masked);

      afni_dset_to_svm_doc( docsTrain, dsetTrainArray, dsetMaskArray, sampleCount, nvox, nvox_masked );

      svm_learn_classification( docsTrain, target, sampleCount, nvox_masked, learn_parm, kernel_parm, &kernel_cache, model );

      addToAfniModel(&afniModel, learn_parm->alphafile, model, tmp_labels, classCount, sampleCount, labels.class_list[cc],labels.class_list[dd]);

      if( options->modelWeightFile[0] ) {
	addToModelMap_brick(&afniModel, model, dsetTrain, dsetMask, dsetMaskArray, options->modelWeightFile, nvox, labels.class_list[cc],labels.class_list[dd]);
      }

      ++classCount;

      free(model->supvec);
      free(model->alpha);
      free(model->index);
      freeDOCwords(docsTrain, sampleCount);
      free2DT( dsetTrainArray, sampleCount );
      free(docsTrain);
      free(target);
    }
  }

  writeModelBrik(&afniModel, dsetTrain, options->modelFile, argc, argv);

  /* in the future, may want to add some flexibility, such as making the mask a subbrik in the model (but that *
   * would mix two different data types. Another possibility is to add the name of the mask as one of the      *
   * model variables                                                                                           */
  if( options->maskFile[0] ) {
    writeModelMask(dsetMask, dsetMaskArray, options->modelFile);
  }

  freeLabels(&labels);
  freeAfniModel(&afniModel);
  free(tmp_labels);
  
  EXRETURN;
}

void input_parse(int argc,char *argv[],long *main_verbosity,
    long *kernel_cache_size,LEARN_PARM *learn_parm,
    KERNEL_PARM *kernel_parm, ASLoptions* optionsData, enum modes *mode, char *errorString )
{
  long i;
  char type[200];
  int parseFlag = 0;    /*ZSS: init to 0*/
  int aFlag = 0; 
  int alphaFlag = 0;
  *mode = NOTHING;

  /* svmlight defaults */
  strcpy (learn_parm->predfile, "trans_predictions");
  strcpy (learn_parm->alphafile, "");
  (*main_verbosity)=1;
  verbosity=1; //svm_light verbosity which is a little tricky as a static global and now the primary
               // variable for functions in this file.
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
  kernel_parm->kernel_type=0;
  kernel_parm->poly_degree=3;
  kernel_parm->rbf_gamma=1.0;
  kernel_parm->coef_lin=1;
  kernel_parm->coef_const=1;
  strcpy(kernel_parm->custom,"empty");
  strcpy(type,"c");

  /* 3dsvm specific */
  strcpy(optionsData->labelFile, "");
  strcpy(optionsData->censorFile, "");
  strcpy(optionsData->trainFile, "");
  strcpy(optionsData->maskFile, "");
  strcpy(optionsData->modelFile, "");
  optionsData->outModelNoMask = 0;
  optionsData->noPredDetrend = 0;
  optionsData->classout = 0;
  strcpy(optionsData->multiclass, "");
  strcpy(optionsData->modelAlphaFile, "");
  strcpy(optionsData->modelWeightFile, "");
  strcpy(optionsData->testFile, "");
  strcpy(optionsData->testLabelFile, "");
  strcpy(optionsData->predFile, "");

  for( i=1 ; i<argc ; ++i ) {
    parseFlag = 0;
    if( !strcmp(argv[i],"-trace")) {
      parseFlag = 1;
#ifdef USE_TRACING
      DBG_trace = 1; 
#endif
            /* It is a good idea to use ENTRY(""), RETURN(); and EXRETURN;
            macros in order to enable the tracing utility.
            I have used them in a few places. ZSS */
    }
    if( !strcmp(argv[i],"-no_memcheck")) { pause_mcw_malloc(); /* ZSS */ }
    if( !strcmp(argv[i],"-z") ) { parseFlag = 1; ++i; strcpy(type,argv[i]); }
    if( !strcmp(argv[i],"-v") ) { parseFlag = 1; ++i; (*main_verbosity)=atol(argv[i]); verbosity = *main_verbosity; }
    if( !strcmp(argv[i],"-b") ) { parseFlag = 1; ++i; learn_parm->biased_hyperplane=atol(argv[i]); }
    if( !strcmp(argv[i],"-i") ) { parseFlag = 1; ++i; learn_parm->remove_inconsistent=atol(argv[i]); }
    if( !strcmp(argv[i],"-f") ) { parseFlag = 1; ++i; learn_parm->skip_final_opt_check=!atol(argv[i]); }
    if( !strcmp(argv[i],"-q") ) { parseFlag = 1; ++i; learn_parm->svm_maxqpsize=atol(argv[i]); }
    if( !strcmp(argv[i],"-n") ) { parseFlag = 1; ++i; learn_parm->svm_newvarsinqp=atol(argv[i]); }
    if( !strcmp(argv[i],"-h") ) { parseFlag = 1; ++i; learn_parm->svm_iter_to_shrink=atol(argv[i]); }
    if( !strcmp(argv[i],"-m") ) { parseFlag = 1; ++i; (*kernel_cache_size)=atol(argv[i]); }
    if( !strcmp(argv[i],"-c") ) { parseFlag = 1; ++i; learn_parm->svm_c=atof(argv[i]); }
    if( !strcmp(argv[i],"-w") ) { parseFlag = 1; ++i; learn_parm->eps=atof(argv[i]); }
    if( !strcmp(argv[i],"-p") ) { parseFlag = 1; ++i; learn_parm->transduction_posratio=atof(argv[i]); }
    if( !strcmp(argv[i],"-j") ) { parseFlag = 1; ++i; learn_parm->svm_costratio=atof(argv[i]); }
    if( !strcmp(argv[i],"-e") ) { parseFlag = 1; ++i; learn_parm->epsilon_crit=atof(argv[i]); }
    if( !strcmp(argv[i],"-o") ) { parseFlag = 1; ++i; learn_parm->rho=atof(argv[i]); }
    if( !strcmp(argv[i],"-k") ) { parseFlag = 1; ++i; learn_parm->xa_depth=atol(argv[i]); }
    if( !strcmp(argv[i],"-x") ) { parseFlag = 1; ++i; learn_parm->compute_loo=atol(argv[i]); }
    if( !strcmp(argv[i],"-t") ) { parseFlag = 1; ++i; kernel_parm->kernel_type=atol(argv[i]); }
    if( !strcmp(argv[i],"-d") ) { parseFlag = 1; ++i; kernel_parm->poly_degree=atol(argv[i]); }
    if( !strcmp(argv[i],"-g") ) { parseFlag = 1; ++i; kernel_parm->rbf_gamma=atof(argv[i]); }
    if( !strcmp(argv[i],"-s") ) { parseFlag = 1; ++i; kernel_parm->coef_lin=atof(argv[i]); }
    if( !strcmp(argv[i],"-r") ) { parseFlag = 1; ++i; kernel_parm->coef_const=atof(argv[i]); }
    if( !strcmp(argv[i],"-u") ) { parseFlag = 1; ++i; strcpy(kernel_parm->custom,argv[i]); }
    if( !strcmp(argv[i],"-l") ) { parseFlag = 1; ++i; strcpy(learn_parm->predfile,argv[i]); }
    //if( !strcmp(argv[i],"-a") ) { parseFlag = 1; ++i; strcpy(learn_parm->alphafile,argv[i]); }
    // as an easy solution, we are fixing the svmLight's output file name and letting 3dsvm write out the desired file

    if( !strcmp(argv[i],"-a") )             { parseFlag = 1; ++i; strcpy(optionsData->modelAlphaFile, argv[i]); aFlag = 1;}
    if( !strcmp(argv[i],"-alpha") )         { parseFlag = 1; ++i; strcpy(optionsData->modelAlphaFile, argv[i]); alphaFlag = 1;}
    if( !strcmp(argv[i],"-trainvol") )      { parseFlag = 1; ++i; strcpy(optionsData->trainFile, argv[i]); }
    if( !strcmp(argv[i],"-testvol") )       { parseFlag = 1; ++i; strcpy(optionsData->testFile, argv[i]); }
    if( !strcmp(argv[i],"-multiclass") )    { parseFlag = 1; ++i; strcpy(optionsData->multiclass, argv[i]); }
    if( !strcmp(argv[i],"-trainlabels") )   { parseFlag = 1; ++i; strcpy(optionsData->labelFile, argv[i]); }
    if( !strcmp(argv[i],"-censor") )        { parseFlag = 1; ++i; strcpy(optionsData->censorFile, argv[i]); }
    if( !strcmp(argv[i],"-mask") )          { parseFlag = 1; ++i; strcpy(optionsData->maskFile, argv[i]); }
    if( !strcmp(argv[i],"-model") )         { parseFlag = 1; ++i; strcpy(optionsData->modelFile, argv[i]); }
    if( !strcmp(argv[i],"-nomodelmask") )   { parseFlag = 1; optionsData->outModelNoMask = 1; }
    if( !strcmp(argv[i],"-nodetrend") )     { parseFlag = 1; optionsData->noPredDetrend = 1; }
    if( !strcmp(argv[i],"-classout") )      { parseFlag = 1; optionsData->classout = 1; }
    if( !strcmp(argv[i],"-bucket") )        { parseFlag = 1; ++i; strcpy(optionsData->modelWeightFile, argv[i]); }
    if( !strcmp(argv[i],"-testlabels") )    { parseFlag = 1; ++i; strcpy(optionsData->testLabelFile, argv[i]); }
    if( !strcmp(argv[i],"-predictions") )   { parseFlag = 1; ++i; strcpy(optionsData->predFile, argv[i]); }
    if( !strcmp(argv[i],"-help") ) { 
      printf( cl_helpstring ); 
      printf("\n\n-------------------- SVM-light learn help -----------------------------\n");
      print_help_learn();
      printf("\n\n-------------------- SVM-light classify help -----------------------------\n");
      print_help_classify();
      printf("\n\n--------------------------------------------------------------------------\n");
      printf("Jeff W. Prescott and Stephen M. LaConte \n");
      printf("\n");
      printf("Original version written by JP and SL, August 2006 \n");
      printf("Released to general public, July 2007 \n");
      printf("\n");
      printf("Questions/Comments/Bugs - email slaconte@cpu.bcm.edu \n");
      printf("\n");
      printf("Reference:\n");
      printf("LaConte, S., Strother, S., Cherkassky, V. and Hu, X. 2005. Support vector\n"); 
      printf("    machines for temporal classification of block design fMRI data. \n");
      printf("    NeuroImage, 26, 317-329.\n");
      
      exit(0); 
    }
    if( !strcmp(argv[i],"-change_summary") ) {
      printf( change_string ); 
      
      exit(0); 
    }

    if( !parseFlag ) { printf("++ Program %s:\n** ILLEGAL OPTION: %s\n\n\n** cannot continue\n\n try '%s -help'\n", argv[0], argv[i], argv[0]); exit(0); }

    }

    /* Set mode and do some error checking */
    if( optionsData->trainFile[0] ) {
      if( !(optionsData->labelFile[0]) ) {
        //fprintf( stderr, "Must specify timeseries file if training!\n" );
        //exit(0);
        strcpy(errorString,"Must specify timeseries file if training!\n");
      }
      if( !(optionsData->testFile[0]) )
        *mode = TRAIN;
      else
        *mode = TRAIN_AND_TEST;
    }
    else {
      if( !(optionsData->testFile[0]) ) {
        //fprintf( stderr, "Must specify training or test file!\n" );
        //exit(0);
        strcpy(errorString,"Must specify training or test file!\n");
      }
      else
        *mode = TEST;
    }
    if( !(*mode == TRAIN_AND_TEST) && !(optionsData->modelFile[0]) ) { 
      //fprintf( stderr, "Must specify a model file!\n" );

    if( !parseFlag ) { printf("++ Program %s:\n** ILLEGAL OPTION: %s\n\n\n** cannot continue\n\n try '%s -help'\n", argv[0], argv[i], argv[0]); exit(0); }

    }

    /* Set mode and do some error checking */
    if( optionsData->trainFile[0] ) {
      if( !(optionsData->labelFile[0]) ) {
        //fprintf( stderr, "Must specify timeseries file if training!\n" );
        //exit(0);
        strcpy(errorString,"Must specify timeseries file if training!\n");
      }
      if( !(optionsData->testFile[0]) )
        *mode = TRAIN;
      else
        *mode = TRAIN_AND_TEST;
    }
    else {
      if( !(optionsData->testFile[0]) ) {
        //fprintf( stderr, "Must specify training or test file!\n" );
        //exit(0);
        strcpy(errorString,"Must specify training or test file!\n");
      }
      else
        *mode = TEST;
    }
    if( !(*mode == TRAIN_AND_TEST) && !(optionsData->modelFile[0]) ) { 
      //fprintf( stderr, "Must specify a model file!\n" );
      //exit(0);
      strcpy(errorString,"Must specify a model file!\n");
    }
    /* at some point may want to check for TRAIN/TEST specific mode options */
    /* e.g. nodetrend only applies in test mode                             */

    /* check for multiclass errors 
     * multiclass only implemented for 'testing', more error checking in function: 'train_routine' */
    if( (optionsData->multiclass[0]) && (optionsData->trainFile[0]) )
        fprintf(stderr,"WARNING: -multiclass only implemented for testing purposes\n"); 
    	
    /* check for other errors */
    if( aFlag  && alphaFlag ) {/* if both -a and -alpha are specified, both files need to match */
        fprintf(stderr,"WARNING: both -a and -alpha were specified. Using filename  %s \n", optionsData->modelAlphaFile);
    }
    if( (optionsData->modelWeightFile[0]) && !(kernel_parm->kernel_type == 0) ) {
      //fprintf( stderr, "At this time, can only generate maps (-bucket option) for linear kernels (-t 0 [default]) \n" );
      //exit(0);
      strcpy(errorString,"At this time, can only generate maps (-bucket option) for linear kernels (-t 0 [default]) \n");
    }
    if( !(optionsData->modelFile[0]) && !(optionsData->outModelNoMask) ){
      //fprintf(stderr, "No mask file specified (use -mask file). If not using a mask file must use option -nomodelmask\n");
      //exit(0);
      strcpy(errorString, "No mask file specified (use -mask file). If not using a mask file must use option -nomodelmask\n");
    }

  if(learn_parm->svm_iter_to_shrink == -9999) {
    if(kernel_parm->kernel_type == LINEAR) 
      learn_parm->svm_iter_to_shrink=2;
    else
      learn_parm->svm_iter_to_shrink=100;
  }


  if((learn_parm->skip_final_opt_check) 
      && (kernel_parm->kernel_type == LINEAR)) {
    printf("\nIt does not make sense to skip the final optimality check for linear kernels.\n\n");
    learn_parm->skip_final_opt_check=0;
  }    
  if((learn_parm->skip_final_opt_check) 
      && (learn_parm->remove_inconsistent)) {
    printf("\nIt is necessary to do the final optimality check when removing inconsistent \nexamples.\n");
    exit(0);
  }    
  if((learn_parm->svm_maxqpsize<2)) {
    printf("\nMaximum size of QP-subproblems not in valid range: %ld [2..]\n",learn_parm->svm_maxqpsize); 
    exit(0);
  }
  if((learn_parm->svm_maxqpsize<learn_parm->svm_newvarsinqp)) {
    printf("\nMaximum size of QP-subproblems [%ld] must be larger than the number of\n",learn_parm->svm_maxqpsize); 
    printf("new variables [%ld] entering the working set in each iteration.\n",learn_parm->svm_newvarsinqp); 
    exit(0);
  }
  if(learn_parm->svm_iter_to_shrink<1) {
    printf("\nMaximum number of iterations for shrinking not in valid range: %ld [1,..]\n",learn_parm->svm_iter_to_shrink);
    exit(0);
  }
  if(learn_parm->svm_c<0) {
    printf("\nThe C parameter must be greater than zero!\n\n");
    exit(0);
  }
  if(learn_parm->transduction_posratio>1) {
    printf("\nThe fraction of unlabeled examples to classify as positives must\n");
    printf("be less than 1.0 !!!\n\n");
    exit(0);
  }
  if(learn_parm->svm_costratio<=0) {
    printf("\nThe COSTRATIO parameter must be greater than zero!\n\n");
    exit(0);
  }
  if(learn_parm->epsilon_crit<=0) {
    printf("\nThe epsilon parameter must be greater than zero!\n\n");
    exit(0);
  }
  if(learn_parm->rho<0) {
    printf("\nThe parameter rho for xi/alpha-estimates and leave-one-out pruning must\n");
    printf("be greater than zero (typically 1.0 or 2.0, see T. Joachims, Estimating the\n");
    printf("Generalization Performance of an SVM Efficiently, ICML, 2000.)!\n\n");
    exit(0);
  }
  if((learn_parm->xa_depth<0) || (learn_parm->xa_depth>100)) {
    printf("\nThe parameter depth for ext. xi/alpha-estimates must be in [0..100] (zero\n");
    printf("for switching to the conventional xa/estimates described in T. Joachims,\n");
    printf("Estimating the Generalization Performance of an SVM Efficiently, ICML, 2000.)\n");
    exit(0);
  }

  if( (*main_verbosity) >=2) {
    if(optionsData->labelFile[0]) printf("The label file is %s\n",optionsData->labelFile);
    if(optionsData->censorFile[0]) printf("The censor file is %s\n",optionsData->censorFile);
    if(optionsData->trainFile[0]) printf("The training dataset is %s\n",optionsData->trainFile);
    if(optionsData->maskFile[0]) printf("The mask dataset is %s\n",optionsData->maskFile);
    if(optionsData->modelFile[0]) printf("The model file is %s\n",optionsData->modelFile);
    if(optionsData->outModelNoMask) printf("The output model file without mask flag is set\n");
    if(optionsData->modelAlphaFile[0]) printf("The alpha file is %s\n",optionsData->modelAlphaFile);
    if(optionsData->modelWeightFile[0]) printf("The weight file is %s\n",optionsData->modelWeightFile);
    if(optionsData->testFile[0]) printf("The testing dataset is %s\n",optionsData->testFile);
    if(optionsData->testLabelFile[0]) printf("The test label file is %s\n",optionsData->testLabelFile);
    if(optionsData->predFile[0]) printf("The predictions file is %s\n",optionsData->predFile);
    printf("mode = %d\n",*mode);
  }


  return;
}

