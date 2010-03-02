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
  
  ENTRY("detrend_linear_cnsrc");

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
 
  for(nt=0, ntc=0; nt<n_t, ntc<n_tcnsrs; nt++) {
    if((labels->lbls[nt] != 9999) && (labels->lbls[nt] != -9999)) {
      data[nt] = data_cnsrs[ntc];
      ntc++;
      }
  }
  
  EXRETURN; 
}

/* JL June 2009: This function writes the svm-light DOC structure into 
 * a svm-light formated textfile */
void write_svmLight_doc(DOC *docs, long nt, long nvox, 
    LabelType *target, char *fileName, char *svmLight_ver)
{    
  long t    = 0;
  long v    = 0;
  FILE *fp  = NULL;

  ENTRY("write_svmLight_doc");
  INFO_message("Writing svm-light (doc) textfile..."); 
  
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
          WARNING_message("Writing svm-light (doc) textfile failed! "
              "Number of words shorter than expected");
          EXRETURN;
        }
        if ( docs[t].words[v].weight != 0 ) {
          fprintf(fp, "%ld:%lf ", v+1, docs[t].words[v].weight );
        }
      }
      fprintf(fp, " # written by 3dsvm\n");
    }
  }
  else {
    WARNING_message("Can not write svm-light (doc) textfile"
        " Svm-light version %s unknown", svmLight_ver);
    EXRETURN;
  }
  
  fclose(fp);
  
  EXRETURN;
}


unsigned long int getFileSize(char *fileName)
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

/* JL Mar. 2009 */
double **Allocate2d(long index1, long index2)
{
  long i;
  double **darr;

  ENTRY("Allocate2d");

  if(   ( darr = (double **)malloc(index1*sizeof(double *)) )   ) {
    for(i = 0; i < index1; i++) {
      if(   ( darr[i] = (double *)malloc(index2*sizeof(double)) )   );
      else {
        ERROR_exit("Memory allocation in Allocate2d failed!\n"
            "   Attemted to allocate [%ld][%ld]\n", 
            "   Successfully allocated [%ld][%ld]", index1, index2, i-1,index2);
      }
    }
  }
  else {
    ERROR_exit("Memory allocation in Allocate2d failed!");
  }
  
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
  long i;
  float **farr;

  ENTRY("Allocate2f");

  if(   ( farr = (float **)malloc(index1*sizeof(float *)) )   ) {
    for(i = 0; i < index1; i++) {
      if(   ( farr[i] = (float *)malloc(index2*sizeof(float)) )   );
      else {
        ERROR_exit("Memory allocation in Allocate2f failed!\n"
            "   Attemted to allocate [%ld][%ld]\n", 
            "   Successfully allocated [%ld][%ld]", index1, index2, i-1,index2);
      }
    }
  }
  else {
    ERROR_exit("Memory allocation in Allocate2f failed!");
  }

  RETURN(farr);
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
  long i;
  DatasetType **arr;

  ENTRY("Allocate2DT");

  if(   ( arr = (DatasetType **)malloc(index1*sizeof(DatasetType *)) )   ) {
    for(i = 0; i < index1; i++) {
      if(   ( arr[i] = (DatasetType *)malloc(index2*sizeof(DatasetType)) )   );
      else {
        ERROR_exit("Memory allocation in Allocate2DT failed!\n"
            "   Attemted to allocate [%ld][%ld]\n", 
            "   Successfully allocated [%ld][%ld]", index1, index2, i-1,index2);
      }
    }
  }
  else {
    ERROR_exit("Memory allocation in Allocate2DT failed!");
  }

  RETURN(arr);
}

/*************************************************************/
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
  long i;
  char **carr;

  ENTRY("Allocate2c");

  carr = (char **)malloc(sizeof(char *) * index1);
  if (carr == NULL) {
    ERROR_exit("Memory allocation in Allocate2c failed");
  }
  for(i=0; i<index1; i++) {
    carr[i] == NULL;
    carr[i] = malloc(sizeof(char) * index2);
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

  ENTRY("AllocateDOCwords");

  for( i=0; i < ndocsTime; ++i ) {
    docs[i].words = (WORD*)malloc(sizeof(WORD)*(nvoxelWords+1));
  }

  EXRETURN;
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
  long nsv = 0;   /* number of support vectors */
  long sv = 0;    /* index over nsv */
  long nt = 0;    /* number of timepoints */
  long t = 0;     /* index over nt */

  /* our approach to multiclass is to keep all training timepoints 
   * with non-support vectors as alpha = 0
   * thus the model "documents" and number of support vectors is
   * always the number of timepoints in in the training data
   */

  ENTRY("allocateModel");

  nt=afni_model->timepoints;

  /* JL July 2009: Added this part to support sv-regression */
  if ( !strcmp(afni_model->svm_type, "regression") ) {
   
    nsv=afni_model->total_support_vectors[0];

    model->supvec = (DOC **)my_malloc(sizeof(DOC *)*(nsv+1));
    model->alpha = (double *)my_malloc(sizeof(double)*(nsv+1));

    for ( sv=1; sv<nsv; ++sv ) {
      model->supvec[sv] = (DOC *)calloc(sizeof(DOC),1);
      (model->supvec[sv])->words = (WORD *)calloc(sizeof(WORD),
          afni_model->total_masked_features[0] + 1); 
    }
  }
  else {
    model->supvec = (DOC **)my_malloc(sizeof(DOC *)*(nt+1));
    model->alpha = (double *)my_malloc(sizeof(double)*(nt+1));
    
    for( t=1; t<nt+1; ++t ) {  
      /* (timpoints +1) is svmlights number of support vectors */
      
      model->supvec[t] = (DOC *)calloc(sizeof(DOC),1);
      (model->supvec[t])->words = (WORD *)calloc(sizeof(WORD),
          afni_model->total_masked_features[0] + 1); 
      /* +1 for end of list value */
	  /* [0] assumes  that all models use the same mask */
    }
  }

  if(afni_model->kernel_type[0] == LINEAR ) {
    model->lin_weights=(double *)my_malloc(sizeof(double)*
        (afni_model->total_masked_features[0] + 1));
  }

  EXRETURN;
}

/*-----------------------------------------------------------*/
void freeModel( MODEL *model, AFNI_MODEL *afni_model)
{
  long nsv = 0;   /* number of support vectors */
  long sv = 0;    /* index over nsv */
  long nt = 0;    /* number of timepoints */
  long t = 0;     /* index over nt */

  ENTRY("freeModel");
  
  nt=afni_model->timepoints;

  /* JL July 2009: Added this part to support sv-regression */
  if ( !strcmp(afni_model->svm_type, "regression") ) {
    nsv=afni_model->total_support_vectors[0];

    for( sv=1; sv<nsv; ++sv) {
      free( (model->supvec[sv])->words );
      free(model->supvec[sv]);    
    }
  }
  else {
    for( t=1; t<nt+1; t++) {  
      /* (timpoints +1) is svmlights number of support vectors */
    free( (model->supvec[t])->words );
    free(model->supvec[t]);
    }
  }

  if(model->kernel_parm.kernel_type == LINEAR ) {
    free(model->lin_weights);
  }
  
  free(model->supvec);
  free(model->alpha);

  EXRETURN;
}

/*-----------------------------------------------------------*/
void updateModel(MODEL *model, AFNI_MODEL *afni_model, ASLoptions *options, int comb) 
 
  /* fill in all values for the first (index 0) class combination */
{
  long i  = 0;
  long nt = 0;
  long t  = 0;
  long sv = 0;

  
  ENTRY("updateModel");

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
   *
   * JL July 2009: For sv-regression (and testing only!) the number of support 
   * vectors is the number of non-zeoro alphas and only  non-zero alphas are 
   * written into the svm-light model.
   *
   */

  model->totdoc = (long) afni_model->timepoints;
  nt = (long) afni_model->timepoints;

  if ( (!strcmp(afni_model->svm_type,"regression")) && (options->testFile[0]) ) {
    model->sv_num = (long) afni_model->total_support_vectors[comb];
    
    sv=1;
    for( t=0; t<nt; ++t) {
      if ( afni_model->alphas[comb][t] != 0 ) {
        model->alpha[sv] = (double) afni_model->alphas[comb][t];
      
        ++sv;
      }
    }
  }
  else { // before sv-regression
    model->sv_num = (long) afni_model->timepoints + 1;
    for( i=0 ; i< model->sv_num - 1 ; ++i ) {
      model->alpha[i+1] = (double)afni_model->alphas[comb][i];    
    }
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
    INFO_message( "sv_num = %ld", model->sv_num );
    INFO_message( "kernel_type = %ld", model->kernel_parm.kernel_type ); 
    INFO_message( "poly_degree = %ld", model->kernel_parm.poly_degree ); 
    INFO_message( "rbf_gamma = %lf", model->kernel_parm.rbf_gamma ); 
    INFO_message( "coef_lin = %lf", model->kernel_parm.coef_lin ); 
    INFO_message( "coef_const = %lf", model->kernel_parm.coef_const ); 
    INFO_message( "totwords = %ld", model->totwords ); 
    INFO_message( "totdoc = %ld", model->totdoc );
    INFO_message( "b = %lf", model->b );
  }

  EXRETURN;
}

/*-----------------------------------------------------------*/
/* just fills in the model data set (assumed constant accross class combinations) */
/* Need to also use updateModel for class */
/* The idea is to only call this once and then updateModel for combination specific aspects */
void get_svm_model(MODEL *model, DatasetType **dsetModelArray, 
    MaskType *dsetMaskArray, AFNI_MODEL *afni_model, long model_vox, 
    int noMaskFlag)
{
  long i = 0;
  long j = 0;
  long k = 0;

  long nt        = 0; /* number of timepoints */
  long t         = 0; /* index of nt */
  long nth       = 0; /* number of timepoints half */
  long th        = 0; /* index over nth */
  long v         = 0; /* index over model_vox */
  long nvox_msk  = 0; /* number of masked voxels */
  long vmsk      = 0; /* index over nvox_msk */
  long sv        = 0; /* sv index */
  
  ENTRY("get_svm_model");
 
  /* JL Feb. 2009: For sv-regression, only support vectors (alpha != 0) are wirtten into 
   * svm-light's modelfile. Afni_model->alphas contains the alphas and batas and has 
   * twice as many timepoints as the model brick. To retrieve the alpha and beta support-
   * vectors we need to loop through the model brick twice. */
  
  if( !strcmp(afni_model->svm_type,"regression") ) { 
    nt = afni_model->timepoints;
    nth = afni_model->timepoints/2;
    nvox_msk = (long) afni_model->total_masked_features[0];
    
    sv=1;
    for( t=0, th=0; t<nt; ++t, ++th ) {
      if ( th == nth ) th=0;
      vmsk=0;
      if ( afni_model->alphas[0][t] != 0 ) {
        for( v=0; v<model_vox; ++v ) {
          if( vmsk<nvox_msk ) {
            if( noMaskFlag ) { // no mask
              (model->supvec[sv])->words[vmsk].wnum = vmsk + 1;
              (model->supvec[sv])->words[vmsk].weight = 
                (float)dsetModelArray[th][v];
            
              ++vmsk;
            }
            else if( dsetMaskArray[v] ) { // mask
              (model->supvec[sv])->words[vmsk].wnum = vmsk + 1;
              (model->supvec[sv])->words[vmsk].weight = 
                (float)dsetModelArray[th][v];

              ++vmsk;
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
  else { // before sv-regression:
    for(i = 1; i < afni_model->timepoints + 1; i++) {  
      /* number of support vectors is (afni_model->timepoints + 1) */
	  /*	this simplifies multi-class life by allowing us to essentially */
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

  EXRETURN;

}
/*-----------------------------------------------------------*/


void readAllocateAfniModel( THD_3dim_dataset *dsetModel,  AFNI_MODEL *afniModel)
{
  ATR_float *atr_float;
  ATR_int *atr_int;
  ATR_string *atr_string;
  long i,j,c;
  char p[100],*q; /* used for strtok magic */
  char headernames[100];

  ENTRY("readAllocateAfniModel");


  /* JL Oct 2009: The naming and number of model parameters in the 
   * model header has changed. We added "3DSVM" in front of each parameter name 
   * to avoid collisions with header entries from other afni programs.
   *
   * Trying to be backwards compatible. */

  /* Assuming old (before Oct. 2009) naming for model parameters 
   * if "3DSVM_SVM_TYPE" is not present in the header. */
  
  atr_string = THD_find_string_atr( dsetModel->dblk, "3DSVM_SVM_TYPE" );
  
  
  /* ---- naming for model parameters before Oct. 2009 ---*/
  if( atr_string == NULL ) {
    WARNING_message("Can not find 3DSVM_SVM_TYPE in model header! "
        "You must have used an older version for training!");
    strcpy(afniModel->svm_type,"classification");

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
      ERROR_exit("Reading model combinations in header file failed");
    }
    for(i = 1; i < afniModel->combinations; ++i) {
      q=strtok(NULL,",");
      if (q != NULL) strcpy(afniModel->combName[i],q);
      else {
        ERROR_exit("Reading model combinations in header file failed\n" 
            "   Number does not match expected(%d)", afniModel->combinations);
      }
    }
  
    atr_int = THD_find_int_atr( dsetModel->dblk, "KERNEL_TYPE" );
    afniModel->kernel_type = (int *)malloc( atr_int->nin * sizeof(int) );
    for( i=0 ; i<atr_int->nin ; ++i ) {
      afniModel->kernel_type[i] = atr_int->in[i];
    }
  
    
    /* JL Feb. 2009: Added this part to support custom kernels. 
     * To be backward compatible, read KERNEL_CUSTOM only if training was 
     * performed with a custom kernel. */
    if (afniModel->kernel_type[0] == CUSTOM) {
        atr_string = THD_find_string_atr( dsetModel->dblk, "KERNEL_CUSTOM" );
        strcpy(p,atr_string->ch);
        q = strtok(p,",");
        if (q != NULL) strcpy(afniModel->kernel_custom[0],q);
        else ERROR_exit("Can't find KERNEL_CUSTOM in header file");
        
        for ( i=1; i<afniModel->combinations; ++i) {
          q=strtok(NULL,",");
          if (q != NULL) strcpy(afniModel->kernel_custom[i],q);
          else {
            ERROR_exit("Reading KERNEL_CUSTOM in header file number of class\n"
                "combinations does not match expected(%d)\n", 
                afniModel->combinations);
          }
        }
      }
      else {
        for ( i=1; i<afniModel->combinations; ++i) {
          strcpy(afniModel->kernel_custom[i],"empty");
        }
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
  
    atr_int = THD_find_int_atr( dsetModel->dblk, "POLYNOMIAL_DEGREE" );
    afniModel->polynomial_degree = (int *)malloc( atr_int->nin * sizeof(int) );
    for( i=0 ; i<atr_int->nin ; ++i ) {
      afniModel->polynomial_degree[i] = atr_int->in[i];
    }
  
      
    afniModel->alphas = Allocate2f((long) afniModel->combinations, (long) afniModel->timepoints);
    afniModel->cAlphas = Allocate2f((long) afniModel->combinations, (long) afniModel->timepoints);
    for(i = 0; i < afniModel->combinations; ++i ) {
      sprintf(headernames,"ALPHAS_%s",afniModel->combName[i]);
      atr_float = THD_find_float_atr( dsetModel->dblk, headernames); 
      for(j = 0; j < afniModel->timepoints; ++j ) {
        afniModel->alphas[i][j] = (double)atr_float->fl[j];
      }
    }
  }

  /* --- naming for model parameters (Oct. 2009) --- */
  else {
    strcpy(afniModel->svm_type,atr_string->ch);
  
    atr_int = THD_find_int_atr( dsetModel->dblk, "3DSVM_CLASS_COUNT" );
    afniModel->class_count = *atr_int->in;

    atr_int = THD_find_int_atr( dsetModel->dblk, "3DSVM_CLASS_COMBINATIONS" );
    afniModel->combinations = *atr_int->in;

    atr_int = THD_find_int_atr( dsetModel->dblk, "3DSVM_TIMEPOINTS" );
    afniModel->timepoints = *atr_int->in;

    atr_string = THD_find_string_atr( dsetModel->dblk, "3DSVM_COMBO_NAMES" );
    strcpy(p,atr_string->ch);
    q = strtok(p,",");
    if (q != NULL) strcpy(afniModel->combName[0],q);
    else {
      ERROR_exit("Reading model combinations in header file failed");
    }
    for(i = 1; i < afniModel->combinations; ++i) {
      q=strtok(NULL,",");
      if (q != NULL) strcpy(afniModel->combName[i],q);
      else {
        ERROR_exit("Reading model combinations in header file failed\n" 
            "   Number does not match expected(%d)", afniModel->combinations);
      }
    }
  
    atr_int = THD_find_int_atr( dsetModel->dblk, "3DSVM_KERNEL_TYPE" );
    afniModel->kernel_type = (int *)malloc( atr_int->nin * sizeof(int) );
    for( i=0 ; i<atr_int->nin ; ++i ) {
      afniModel->kernel_type[i] = atr_int->in[i];
    }
  
    atr_string = THD_find_string_atr( dsetModel->dblk, "3DSVM_KERNEL_CUSTOM" );
    strcpy(p,atr_string->ch);
    q = strtok(p,",");
    if (q != NULL) strcpy(afniModel->kernel_custom[0],q);
    else ERROR_exit("Can't find KERNEL_CUSTOM in header file");
    
    for ( i=1; i<afniModel->combinations; ++i) {
      q=strtok(NULL,",");
      if (q != NULL) strcpy(afniModel->kernel_custom[i],q);
      else {
        ERROR_exit("Reading KERNEL_CUSTOM in header file number of class\n"
            "combinations does not match expected(%d)\n", 
              afniModel->combinations);
      }
    }
  
    atr_float = THD_find_float_atr( dsetModel->dblk, "3DSVM_RBF_GAMMA" );
    afniModel->rbf_gamma = (float *)malloc( atr_float->nfl * sizeof(float) );
    for( i=0 ; i<atr_float->nfl ; ++i ) {
      afniModel->rbf_gamma[i] = atr_float->fl[i];
    }
  
    atr_float = THD_find_float_atr( dsetModel->dblk, "3DSVM_LINEAR_COEFFICIENT" );
    afniModel->linear_coefficient = (float *)malloc( atr_float->nfl * sizeof(float) );
    for( i=0 ; i<atr_float->nfl ; ++i ) {
      afniModel->linear_coefficient[i] = atr_float->fl[i];
    }
  
    atr_float = THD_find_float_atr( dsetModel->dblk, "3DSVM_CONSTANT_COEFFICIENT" );
    afniModel->constant_coefficient = (float *)malloc( atr_float->nfl * sizeof(float) );
    for( i=0 ; i<atr_float->nfl ; ++i ) {
      afniModel->constant_coefficient[i] = atr_float->fl[i];
    }
  
    atr_int = THD_find_int_atr( dsetModel->dblk, "3DSVM_TOTAL_MASKED_FEATURES" );
    afniModel->total_masked_features = (int *)malloc( atr_int->nin * sizeof(int) );
    for( i=0 ; i<atr_int->nin ; ++i ) {
      afniModel->total_masked_features[i] = atr_int->in[i];
    }
  
    atr_int = THD_find_int_atr( dsetModel->dblk, "3DSVM_TOTAL_SAMPLES" );
    afniModel->total_samples = (int *)malloc( atr_int->nin * sizeof(int) );
    for( i=0 ; i<atr_int->nin ; ++i ) {
      afniModel->total_samples[i] = atr_int->in[i];
    }
  
    atr_int = THD_find_int_atr( dsetModel->dblk, "3DSVM_TOTAL_SUPPORT_VECTORS" );
    afniModel->total_support_vectors = (int *)malloc( atr_int->nin * sizeof(int) );
    for( i=0 ; i<atr_int->nin ; ++i ) {
      afniModel->total_support_vectors[i] = atr_int->in[i];
    }
  
    atr_float = THD_find_float_atr( dsetModel->dblk, "3DSVM_B" );
    afniModel->b = (float *)malloc( atr_float->nfl * sizeof(float) );
    for( i=0 ; i<atr_float->nfl ; ++i ) {
      afniModel->b[i] = atr_float->fl[i];
    }
  
    atr_int = THD_find_int_atr( dsetModel->dblk, "3DSVM_POLYNOMIAL_DEGREE" );
    afniModel->polynomial_degree = (int *)malloc( atr_int->nin * sizeof(int) );
    for( i=0 ; i<atr_int->nin ; ++i ) {
      afniModel->polynomial_degree[i] = atr_int->in[i];
    }
  
    afniModel->alphas = Allocate2f((long) afniModel->combinations, (long) afniModel->timepoints);
    afniModel->cAlphas = Allocate2f((long) afniModel->combinations, (long) afniModel->timepoints);
    for(i = 0; i < afniModel->combinations; ++i ) {
      sprintf(headernames,"3DSVM_ALPHAS_%s",afniModel->combName[i]);
      atr_float = THD_find_float_atr( dsetModel->dblk, headernames); 
      for(j = 0; j < afniModel->timepoints; ++j ) {
        afniModel->alphas[i][j] = (double)atr_float->fl[j];
      }
    }

    /* JL Nov 2009: new parameters: */
    THD_find_float_atr( dsetModel->dblk, "3DSVM_EPS" );
    afniModel->eps = (float *)malloc( atr_float->nfl * sizeof(float) );
    for (i=0; i < afniModel->combinations; ++i ) {
      afniModel->eps[i] = atr_float->fl[i];
    }
    
    THD_find_float_atr( dsetModel->dblk, "3DSVM_SVM_C" );
    afniModel->svm_c = (float *)malloc( atr_float->nfl * sizeof(float) );
    for (i=0; i < afniModel->combinations; ++i ) {
      afniModel->svm_c[i] = atr_float->fl[i];
    }
  
    THD_find_int_atr( dsetModel->dblk, "3DSVM_BIASED_HYPERPLANE" );
    afniModel->biased_hyperplane = (int *)malloc( atr_int->nin * sizeof(int) );
    for( i=0; i<afniModel->combinations; ++i ) {
      afniModel->biased_hyperplane[i] = atr_int->in[i];
     }
    
    THD_find_int_atr( dsetModel->dblk, "3DSVM_SKIP_FINAL_OPT_CHECK" );
    afniModel->skip_final_opt_check = (int *)malloc( atr_int->nin * sizeof(int) );
    for( i=0; i<afniModel->combinations; ++i ) {
      afniModel->skip_final_opt_check[i] = atr_int->in[i];
    }

    THD_find_int_atr( dsetModel->dblk, "3DSVM_SVM_MAXQPSIZE" );
    afniModel->svm_maxqpsize = (int *)malloc( atr_int->nin * sizeof(int) );
    for( i=0; i<afniModel->combinations; ++i ) {
      afniModel->svm_maxqpsize[i] = atr_int->in[i];
    }
    
    THD_find_int_atr( dsetModel->dblk, "3DSVM_SVM_NEWVARSINQP" );
    afniModel->svm_newvarsinqp = (int *)malloc( atr_int->nin * sizeof(int) );
    for( i=0; i<afniModel->combinations; ++i ) {
      afniModel->svm_newvarsinqp[i] = atr_int->in[i];
    }
    
    THD_find_int_atr( dsetModel->dblk, "3DSVM_SVM_ITER_TO_SHRINK" );
    afniModel->svm_iter_to_shrink = (int *)malloc( atr_int->nin * sizeof(int) );
    for( i=0; i<afniModel->combinations; ++i ) {
      afniModel->svm_iter_to_shrink[i] = atr_int->in[i];
    }
  
    THD_find_float_atr( dsetModel->dblk, "3DSVM_TRANSDUCTION_POSRATIO" );
    afniModel->transduction_posratio = (float *)malloc( atr_float->nfl * sizeof(float) );
    for (i=0; i < afniModel->combinations; ++i ) {
      afniModel->transduction_posratio[i] = atr_float->fl[i];
    }

    THD_find_float_atr( dsetModel->dblk, "3DSVM_SVM_COSTRATIO" );
    afniModel->svm_costratio = (float *)malloc( atr_float->nfl * sizeof(float) );
    for (i=0; i < afniModel->combinations; ++i ) {
      afniModel->svm_costratio[i] = atr_float->fl[i];
    }
  
    THD_find_float_atr( dsetModel->dblk, "3DSVM_SVM_COSTRATIO_UNLAB" );
    afniModel->svm_costratio_unlab = (float *)malloc( atr_float->nfl * sizeof(float) );
    for (i=0; i < afniModel->combinations; ++i ) {
      afniModel->svm_costratio_unlab[i] = atr_float->fl[i];
    }

    THD_find_float_atr( dsetModel->dblk, "3DSVM_SVM_UNLABBOUND" );
    afniModel->svm_unlabbound = (float *)malloc( atr_float->nfl * sizeof(float) );
    for (i=0; i < afniModel->combinations; ++i ) {
      afniModel->svm_unlabbound[i] = atr_float->fl[i];
    }

    THD_find_float_atr( dsetModel->dblk, "3DSVM_EPSILON_A" );
    afniModel->epsilon_a = (float *)malloc( atr_float->nfl * sizeof(float) );
    for (i=0; i < afniModel->combinations; ++i ) {
      afniModel->epsilon_a[i] = atr_float->fl[i];
    }
    
    THD_find_float_atr( dsetModel->dblk, "3DSVM_EPSILON_CRIT" );
    afniModel->epsilon_crit = (float *)malloc( atr_float->nfl * sizeof(float) );
    for (i=0; i < afniModel->combinations; ++i ) {
      afniModel->epsilon_crit[i] = atr_float->fl[i];
    }
    
    THD_find_int_atr( dsetModel->dblk, "3DSVM_COMPUTE_LOO" );
    afniModel->compute_loo = (int *)malloc( atr_int->nin * sizeof(int) );
    for( i=0; i<afniModel->combinations; ++i ) {
      afniModel->compute_loo[i] = atr_int->in[i];
    }
    
    THD_find_float_atr( dsetModel->dblk, "3DSVM_RHO" );
    afniModel->rho = (float *)malloc( atr_float->nfl * sizeof(float) );
    for (i=0; i < afniModel->combinations; ++i ) {
      afniModel->rho[i] = atr_float->fl[i];
    }

    THD_find_int_atr( dsetModel->dblk, "3DSVM_XA_DEPTH" );
    afniModel->xa_depth = (int *)malloc( atr_int->nin * sizeof(int) );
    for( i=0; i<afniModel->combinations; ++i ) {
      afniModel->xa_depth[i] = atr_int->in[i];
    }
  }
  
   EXRETURN;
}

/* JL Mar. 2009: Changed nvox to be a long 
 * Before addToModelMap_bucket, we were using this function to write each map
 * as a separate brick. */
void addToModelMap_brick(AFNI_MODEL *afni_model, MODEL *model,
    THD_3dim_dataset *dsetTrain, THD_3dim_dataset *dsetMask, 
    MaskType *dsetMaskArray, char *modelFile, long nvox, int comb0, int comb1)
{
  DatasetType* scaled_weights;
  double *lin_weights;
  THD_3dim_dataset* dsetWeights;
  char dsetModelWeightFile[MAX_FILE_NAME_LENGTH];
  long i,k,nmskf;
  int ityp;

  ENTRY("addToModelMap_brick");
  
  nmskf=afni_model->total_masked_features[0] + 1;

  /* may someday be doing more than linear kernels */
  if ( model->kernel_parm.kernel_type == LINEAR ) { 
    
    /* as in the test_routine function, updateModel, */
    /* but probably not quite as clean since not pre-allocating and re-using */
    lin_weights=(double *)my_malloc(sizeof(double)*nmskf);
    clear_vector_n(lin_weights,model->totwords);
    for(i = 1; i<model->sv_num; i++) {
      add_vector_ns(lin_weights,(model->supvec[i])->words, model->alpha[i]);
    }
    
    scaled_weights = (DatasetType*)calloc(nvox,sizeof(DatasetType));
    k = 0;  /* count for feature number if using mask */
    for( i=0 ; i<nvox ; ++i ) {   /* fill in model */
      if( dsetMask ) {  /* if mask is being used */
        if( dsetMaskArray[i] ) {
          scaled_weights[i] += (DatasetType)(SCALE*lin_weights[k]);
          ++k;
        }
      } 
      else  /* if no mask is being used */
        scaled_weights[i] = (DatasetType)(SCALE*lin_weights[i]);
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

    INFO_message( "Writing weight dataset..." );
    THD_write_3dim_dataset( "./", dsetModelWeightFile, dsetWeights, True );
    
    free( scaled_weights );
    free( lin_weights );

    EXRETURN;
  }
}


allocateModelMaps(MODEL_MAPS *maps, long n_classes, long n_vox, char *kernelName)
{
  long i = 0;
  long class_comb = 0;
    
  ENTRY("allocateModelMaps");

  /* --- initialize --- */
  class_comb = n_classes*(n_classes-1)/2;

  if ( (!strcmp(kernelName, "complex1")) ) {  
    /* Remember the real- and imaginary part are concatenated for kernel-
     * option complex1. I'm probably redundant here... */
    if (n_vox%2!=0) {
      ERROR_exit("Trying to allocate model maps: Something is wrong with"
          "the complex-valued data representation");
    }
    maps->nvox = n_vox/2;

    /* Genearting six weight-vector maps for linear-complex kernels: 
     * RE, IM, MAG1, MAG2, PHA1, PHA2 */
    maps->nmaps = class_comb*6;
  }
  else if ( (!strcmp(kernelName, "linear")) ) {
    maps->nmaps = class_comb;
    maps->nvox = n_vox;
  }
  else {
    if (verbosity >= 2) {
      INFO_message("NOTE: Can't create model maps for kernel option: %s", 
          kernelName);
    }

    EXRETURN;
  }

  /* --- allocate memory ---*/
  maps->names = Allocate2c(maps->nmaps, (long)LONG_STRING);
  maps->data = Allocate2d(maps->nmaps, maps->nvox);

  /* --- null allocated memory --- */
  Clear2c(maps->names, maps->nmaps);
  Clear2d(maps->data, maps->nmaps, maps->nvox);

  EXRETURN;
}

freeModelMaps(MODEL_MAPS *maps) 
{
  ENTRY("freeModelMaps");

  free(maps->names);
  free2d(maps->data, maps->nmaps);
  
  EXRETURN;
}

void addToModelMap_bucket ( MODEL_MAPS *maps, AFNI_MODEL *afni_model,
  DatasetType **dsetTrainArray, MaskType *dsetMaskArray, char *maskFile, 
  long cc, long *map_index)
{
  long v      = 0;  
  long iMap   = 0;
  long nvoxh  = 0;
  long t      = 0; 
  long nt     = 0;
  long th     = 0;
  long nth    = 0;

  ENTRY("addToModelMap_bucket");

  /* Note: This function adds one ore more maps to the MODEL_MAPS structure for
   * each class-combination (cc). In order to keep track of how many maps were 
   * written, *map_index is updated for each function call. For now, only 
   * weight-vector maps for linear and linear-complex kernels are calculated.
   * The kernel has to be the same for each class-combination!
   *
   * JL August 2009: Added regression maps */

  /* --- initialization ---*/
  iMap=*map_index; /* prone for errors, should do something better than that */ 
  
  /* --- calculate weight-vector map for regression --- */
  if( !strcmp(afni_model->svm_type, "regression")) {  
    nt = afni_model->total_samples[cc];

    if ( nt%2 != 0 ) { /* I'm beeing redundant... */
      ERROR_exit("Adding to model map failed for sv-regression"
          "The number of timepoints (samples) is not a mulitple of 2!");
    }
    nth=nt/2;

    for (t=0, th=0; t<nt; ++t, ++th) {
      if (th == nth ) th=0;
      if ( afni_model->cAlphas[cc][t] ) {
        for (v=0; v<maps->nvox; ++v) {
          if ( maskFile[0] ) { /* mask */
            if ( dsetMaskArray[v] ) {
              maps->data[iMap][v] += afni_model->cAlphas[cc][t] * dsetTrainArray[th][v];
            }
            else { 
              maps->data[iMap][v] = 0;
            }
          }
          else { /* no mask */
            maps->data[iMap][v] += afni_model->cAlphas[cc][t] * dsetTrainArray[th][v];          
          }
        }
      }
    }
    sprintf(maps->names[iMap], "RegresMapRea_%s", afni_model->combName[cc]);
    ++iMap;
  }
  
  /* --- calculate weight-vector maps --- */ 
  else {
    nt = afni_model->total_samples[cc];
    
    /*  -- (real)-linear kernel -- */
    if(afni_model->kernel_type[cc] == LINEAR) {    
      for (t=0; t<nt; ++t) {
        if ( afni_model->cAlphas[cc][t] ) {
          for (v=0; v<maps->nvox; ++v) {
            if ( maskFile[0] ) { /* mask */
              if ( dsetMaskArray[v] ) {
                maps->data[iMap][v] += afni_model->cAlphas[cc][t] * 
                                          dsetTrainArray[t ][v];
              }
              else { 
                maps->data[iMap][v] = 0;
              } 
            }
            else { /* no mask */
              maps->data[iMap][v] += afni_model->cAlphas[cc][t] * 
                                          dsetTrainArray[t ][v];
            }
          }
        }
      }
      sprintf(maps->names[iMap], "RealWvMapRea_%s", afni_model->combName[cc]);
      ++iMap;
      } 

  
    /* -- complex-linear kernel -- */
    else if( (afni_model->kernel_type[cc] == CUSTOM) && 
        (!strcmp(afni_model->kernel_custom[cc],"complex1")) ) {

      nvoxh = maps->nvox;
    
      for (t=0; t<nt; ++t) {
        if ( afni_model->cAlphas[cc][t] ) {
          for (v=0; v<maps->nvox; ++v) {
            if ( maskFile[0] ) { /* mask */
              if ( dsetMaskArray[v] ) {

                /*   - RE -   */
                maps->data[iMap  ][v] += afni_model->cAlphas[cc][t     ] *
                                              dsetTrainArray[t ][v     ];
                /*   - IM -   */
                maps->data[iMap+1][v] += afni_model->cAlphas[cc][t      ] *
                                              dsetTrainArray[t ][v+nvoxh];
                /*  - MAG1 - */
                maps->data[iMap+2][v] += afni_model->cAlphas[cc][t      ] *
                  sqrt( dsetTrainArray[t ][v      ] * dsetTrainArray[t ][v      ] +
                        dsetTrainArray[t ][v+nvoxh] * dsetTrainArray[t ][v+nvoxh]);
             
                /*  - PHA1 - */
                maps->data[iMap+3][v] += 10e5 *afni_model->cAlphas[cc][t      ] *
                  atan2(dsetTrainArray[t ][v+nvoxh], dsetTrainArray[t ][v     ]);
              
                //printf("*** DBG: t = %ld, RE: %d, IM: %d, ALPHA: %e, PHA: %f, ALPHA*PHA:%f, wPha1[%ld] = %f\n",
                //t, dsetTrainArray[t ][v], dsetTrainArray[t ][v+nvoxh],
                //10e5*afni_model->cAlphas[cc][t],
                //atan2(dsetTrainArray[t ][v+nvoxh], dsetTrainArray[t ][v     ]),
                //10e5*afni_model->cAlphas[cc][t      ] *
                //atan2(dsetTrainArray[t ][v+nvoxh], dsetTrainArray[t ][v     ]),
                //v, maps->data[iMap+3][v] );

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
              maps->data[iMap  ][v] += afni_model->cAlphas[cc][t     ] *
                                            dsetTrainArray[t ][v   ];
              /*  - IM - */
              maps->data[iMap+1][v] += afni_model->cAlphas[cc][t      ] *
                                            dsetTrainArray[t ][v+nvoxh];
              /*  - MAG1 - */
              maps->data[iMap+2][v] += afni_model->cAlphas[cc][t      ] *
                  sqrt( dsetTrainArray[t ][v      ] * dsetTrainArray[t ][v      ] +
                        dsetTrainArray[t ][v+nvoxh] * dsetTrainArray[t ][v+nvoxh]);
              /*  - PHA1 - */
              maps->data[iMap+3][v] += afni_model->cAlphas[cc][t      ] *
                  atan2(dsetTrainArray[t ][v+nvoxh], dsetTrainArray[t ][v     ]);

            } 
          }
        }
      }
      sprintf(maps->names[iMap  ], "CpxWvMapReal_%s", afni_model->combName[cc]);
      sprintf(maps->names[iMap+1], "CpxWvMapImag_%s", afni_model->combName[cc]);
      sprintf(maps->names[iMap+2], "CpxWvMapMag1_%s", afni_model->combName[cc]);
      sprintf(maps->names[iMap+3], "CpxWvMapPha1_%s", afni_model->combName[cc]);

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
            //printf("*** DBG: RE: %f, IM: %f, MAG: %f, PHA: %f\n", maps->data[iMap][v],
            //maps->data[iMap+1][v],maps->data[iMap+2][v],maps->data[iMap+3][v]);
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
      sprintf(maps->names[iMap+4], "CpxWvMapMag2_%s", afni_model->combName[cc]);
      sprintf(maps->names[iMap+5], "CpxWvMapPha2_%s", afni_model->combName[cc]);
      iMap=iMap+6;
    }
  }
  
  *map_index=iMap;

  EXRETURN;
}

void writeModelMap_bucket ( MODEL_MAPS *maps, MaskType *dsetMaskArray, 
    THD_3dim_dataset *dsetTrain,  char *maskFile, char *modelFile, 
    ASLoptions* options, int argc, char **argv) 
{
  long v    = 0;
  long t    = 0;
  long iMap = 0;
  long nx    = 0;
  long ny    = 0;
  long nz    = 0;
  long nx_ny = 0;

  THD_ivec3 iv_nxyz; 
  int ierror            = 0;
  int ityp              = 0;
  THD_3dim_dataset 
    *dsetModelMapBucket = NULL;
  DatasetType 
    *scaled_map         = NULL;
    

  ENTRY("writeModelMap_bucket");

  /* --- initialize --- */
  dsetModelMapBucket = EDIT_empty_copy( dsetTrain );
  tross_Copy_History( dsetTrain, dsetModelMapBucket );

  nx = DSET_NX( dsetTrain );
  ny = DSET_NY( dsetTrain );
  nx_ny = nx*ny;

  if ( !strcmp(options->kernelName, "complex1") ) {     
    /* JL: For complex kernel RE and IM is concatenated in the z-direction.
     * However, we are not concatenating data for the bucket (going back to 
     * nz/2) */

    if ( maps->nvox%nx_ny != 0 ) {
      ERROR_message("Writing bucket file failed. Something is wrong with "
          "the complex-valued data representation.");
      EXRETURN;
      }
    nz = maps->nvox/(nx*ny);
  }
  else {
    nz = DSET_NZ( dsetTrain );
  }

  LOAD_IVEC3( iv_nxyz, nx ,ny ,nz);
  ierror = EDIT_dset_items ( dsetModelMapBucket,
                            ADN_prefix,          modelFile,
                            ADN_type,            HEAD_FUNC_TYPE,
                            ADN_func_type,       FUNC_BUCK_TYPE,
                            ADN_datum_all,       MRI_short,
                            ADN_ntt,             0,   /* no time axis */
                            ADN_nvals,           maps->nmaps,
                            ADN_nxyz,            iv_nxyz, 
                            ADN_malloc_type,     DATABLOCK_MEM_MALLOC ,
                            ADN_none ) ;

  if( ierror > 0 ) {
    ERROR_exit("%d errors in attempting to create bucket dataset!", ierror );
  }

  /* -- record and append history of dataset -- */
  tross_Copy_History( dsetTrain, dsetModelMapBucket);
  char * commandline = tross_commandline( PROGRAM_NAME , argc , argv ) ;
  tross_Append_History ( dsetModelMapBucket, commandline);
  free(commandline);

  
 /* --- scale and write maps into bucket --- */
  for (iMap=0; iMap<maps->nmaps; ++iMap) {
    
    /* -- allocate scaled_map  -- */
    scaled_map = (DatasetType *) malloc(sizeof(DatasetType)*maps->nvox);
    if (scaled_map == NULL) {
      ERROR_exit("Memory allocation in writeModelMap failed!");
    }
    
    /*  -- scaling PHA--  */
    if ( !strncmp(maps->names[iMap], "CpxWvMapPha", 11) ) {
      for (v=0; v<maps->nvox; ++v) { 
        if ( maskFile[0] ) {                   
          if ( dsetMaskArray[v] ) {
            scaled_map[v] = (DatasetType) (180.0/M_PI*maps->data[iMap][v]);
          }
          else {
            scaled_map[v] = 0;
          }
        }
        else { 
          scaled_map[v] = (DatasetType) (180.0/M_PI*maps->data[iMap][v]);
        }   
      }
    }
    else {
      /*  -- scaling RE, IM, MAG-- */
      for (v=0; v<maps->nvox; ++v) {
        if ( maskFile[0] ) {                   
          if ( dsetMaskArray[v] ) {
            scaled_map[v] = (DatasetType) (SCALE*maps->data[iMap][v]);
          }
          else {
            scaled_map[v] = 0;
          }
        }
        else { 
          scaled_map[v] = (DatasetType) (SCALE*maps->data[iMap][v]);
        }   
      }
    }

    /* -- add current map to bucket -- */      
    EDIT_substitute_brick( dsetModelMapBucket, iMap, MRI_short, scaled_map);
    EDIT_BRICK_LABEL( dsetModelMapBucket, iMap, maps->names[iMap]);
    
    /* free(scaled_map); */
  }

  /* --- write entire bucket data set to disc --- */
  fflush(stdout);
  INFO_message("Writing bucket dataset with %ld brick(s)...", maps->nmaps);
  THD_write_3dim_dataset( "./", modelFile, dsetModelMapBucket, True );
 
  /* --- deallocate memory --- */
  free (scaled_map);
  
  EXRETURN;
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
        ADN_ntt, 0,                     /* number of time points (?) */
        ADN_none );

  ityp = DSET_BRICK_TYPE( dsetMask, 0 );     /* ityp: 0=byte, 1=short, 2=float, 3=complex */

  EDIT_substitute_brick( dsetMaskCopy, 0, ityp, dsetMaskArray );

  tross_Copy_History( dsetMask, dsetMaskCopy );
  tross_Append_History( dsetMaskCopy, "a 3dsvm copy") ;

  fflush(stdout);
  INFO_message("Writing model dataset mask...");
  THD_write_3dim_dataset( "./", maskCopyName, dsetMaskCopy, True );

  EXRETURN;
}

/*-----------------------------------------------------------*/
void writeModelBrik(AFNI_MODEL *afniModel, ASLoptions *options, char *fileName,
		int argc, char **argv)
{
  THD_3dim_dataset *dsetModel;
  char csv_combName[LONG_STRING];	/* to put in header file - comma seperated "names" 
                                       of class category combinations */
  char csv_kernelCustom[50];        /* JL Feb. 2009: For custom kernels
                                       50 hard-coded as in svm-light */
  char headernames[LONG_STRING];	/* holds name for each alpha section in the .HEAD */
  long i;

  ENTRY("writeModelBrik");
 

  /* JL Oct 2009: The naming and the number of parameters written into the
   * model header has changed. Now, we are writting all svm parameters 
   * (that can be specified using the command-line) into the header. 
   * We also added  "3DSVM" in front of each parameter name to avoid 
   * collisions with header entries from other afni programs.
   * Trying to be backwards compatible. */


  /* JL July 2009: EDIT_full_copy was copying brick 0 into brick n. 
   * Replaced it by THD_open_one_dataset...*/
  dsetModel = THD_open_one_dataset( options->trainFile );
  if ( dsetModel == NULL ) {
    ERROR_exit("Failed to open training dataset for copying: %s", 
        options->trainFile );
    }
  DSET_load( dsetModel );
 

  /* JL Sep 2009: Assigning new idcode to avioid problems with 
   * dublicated idcodes */
  dsetModel->idcode=MCW_new_idcode();

  strcpy(csv_combName, afniModel->combName[0]);
  strcpy(csv_kernelCustom, afniModel->kernel_custom[0]);

  for(i = 1; i < afniModel->combinations; ++i) {
    strcat(csv_combName, ",");
    strcat(csv_combName, afniModel->combName[i]);
    strcat(csv_kernelCustom, ",");
    strcat(csv_kernelCustom, afniModel->kernel_custom[i]);
  }

  /*----- Record history of dataset -----*/

  char * commandline = tross_commandline( PROGRAM_NAME , argc , argv ) ;
  tross_Append_History ( dsetModel, commandline);
  free(commandline) ;
  
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
    sprintf(headernames,"3DSVM_ALPHAS_%s",afniModel->combName[i]);
    THD_set_float_atr( dsetModel->dblk, headernames, afniModel->timepoints, afniModel->alphas[i] );
  }
  
  fflush(stdout);
  INFO_message( "Writing model dataset..." );
  THD_write_3dim_dataset( "./", fileName, dsetModel, True );

  EXRETURN;
}

/* JL Feb. 2009: This function calculates the squared Euclidean length of
 * a complex vector. */
double cpxtwonorm_sq(WORD *a)
  {  
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


 /* JL May  2009: Added 'ASLoptions *options' to support sv-regression */
void addToAfniModel(AFNI_MODEL *afniModel, MODEL *model, LEARN_PARM *learn_parm,
    LabelType *tmp_labels, ASLoptions *options, long classCount, 
    long sampleCount, int comb0, int comb1)
{
  long nsv    = 0; /* number of support vectors */
  long sv     = 0; /* index over nsv */
  long nt     = 0; /* number ot timepoints */
  long t      = 0; /* index over timepoints */
  long nth    = 0; /* number ot timepoints half (needed for sv-regression)*/
  long th     = 0; /* index of nth */
  long ac     = 0; /* incrementing non-censored alphas count */
  long qid    = 0; /* incrementing queryid */

  FILE *fp    = NULL; /* alpha file output for sv-regression*/
  char alphaFile[LONG_STRING]; /* naming of alphafile output */

  ENTRY("addToAfniModel");


  /* JL July 2009: Changed this function to retrieve the alphas (and betas
   * for sv-regression) directly from the svm-light modelfile. Now, we are 
   * assigning a queryid containing the time information to each doc (time-
   * point), which allows us to retrieve the alphas in time order.
   *
   * For sv-regression, afniModel->alphas contains the alphas and betas.
   */ 

  /* JL Aug. 2009: Added alpha file output for sv-regression to this function, 
   * since, for sv-regression, svm-light is not writing the alphas in time-
   * order */ 
   

  /* --- initilization ---*/
  nsv = model->sv_num;
  nt = afniModel->timepoints;
    
  if ( !strcmp(options->svmType, "regression") ) {
    // Should always be a multiple of two for 
    // sv-regression. Just a redundant check here.
    if ( nt%2 != 0 ) {
      ERROR_exit("Adding to afni model failed for sv-regression"
          "The number of timepoints (samples) is not a mulitple of 2!");
    }
     nth=nt/2;
  } 
  
  /* --- determine timorder of alphas --- */ 
  /* -- for sv-regression -- */
   if ( !strcmp(options->svmType, "regression") ) {
    ac=0; 
    qid=0;

    for( t=0, th=0; t<nt; ++t, ++th ) {
      /* - null, to make alphas for non-support vectors zero - */
      afniModel->alphas[classCount][t] =  0.0;
    
      if ( th == nth ) th=0;
      if ( abs(tmp_labels[th]) == 1) {
        /* - serching for alpha or beta with (queryid == qid) - */ 
        for ( sv=1; sv<nsv; ++sv ) {
          if ( (model->supvec[sv])->queryid == qid) {
            afniModel->alphas[classCount][t] = (float)model->alpha[sv];
    
            /* - reset queryid for second loop through - */
            (model->supvec[sv])->queryid = -1.0;

            /* - alpha or beta found. Exit for-loop over sv -*/
            break;        
          }
        }
        ++qid;
        if (qid == nth) qid = 0;

        /* - write cAlphas - */ 
        afniModel->cAlphas[classCount][ac] = afniModel->alphas[classCount][t];
        ++ac;
      }
    }
  }

  /*  -- for classification --  */
  else {
    ac=0;
    qid=0;
    for( t=0; t<nt; ++t ) {
      /* - null, to make non-support vectors zero - */
      afniModel->alphas[classCount][t] =  0.0;

      if ( abs(tmp_labels[t]) == 1) {
        /* - serching for alpha with (queryid == qid) - */ 
        for ( sv=1; sv<nsv; ++sv ) {
          if ( (model->supvec[sv])->queryid == qid) {
            afniModel->alphas[classCount][t] = (float)model->alpha[sv];
    
            /* - alpha found. Exit for-loop over sv - */
            break;        
          }
        }
        ++qid;

        /* - write cAlphas (alpha index matches data index) - */ 
        afniModel->cAlphas[classCount][ac] = afniModel->alphas[classCount][t];
        ++ac;
      }
    }
  }

  /* JL Aug 2009: Alpha file output for sv-regression */
  if( options->modelAlphaFile[0] ) {
    if ( !strcmp(options->svmType, "regression") ) {
      snprintf( alphaFile, LONG_STRING, "%s.1D", options->modelAlphaFile);
      if ( (fp=fopen(alphaFile, "w")) == NULL ) {
        ERROR_message("Can not open alphafile: %s for writing", alphaFile);
      }
      else {
        fflush(stdout);
        if ( verbosity >= 1 ) INFO_message("Writing alphafile...");
        for ( t=0; t<nt; ++t ) {
          fprintf(fp,"%.8g\n", afniModel->alphas[classCount][t]);
        }
      }
      fclose(fp);
    }
  }

  /* JL Feb. 2009: Added kernel_custom and kernel_type
   *    May. 2009: Added svm_type to support sv-regression
   *    Oct. 2009: Added remaining model parameters that can be specified 
   *               via command-line 
   */

  sprintf( afniModel->svm_type, "%s", options->svmType);
  sprintf( afniModel->combName[classCount], "%d_%d", comb0, comb1 );
  sprintf( afniModel->kernel_custom[classCount], "%s", model->kernel_parm.custom); 
  afniModel->kernel_type[classCount] = model->kernel_parm.kernel_type; 
  afniModel->polynomial_degree[classCount] = model->kernel_parm.poly_degree;  
  afniModel->rbf_gamma[classCount] = model->kernel_parm.rbf_gamma; 
  afniModel->linear_coefficient[classCount] = model->kernel_parm.coef_lin; 
  afniModel->constant_coefficient[classCount] = model->kernel_parm.coef_const; 
  afniModel->total_masked_features[classCount] = (int) model->totwords;
  afniModel->total_samples[classCount] = (int) model->totdoc;
  afniModel->total_support_vectors[classCount] = (int) model->sv_num;
  afniModel->b[classCount] = model->b;
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

/* JL Feb. 2009: Added 'ASLoptions *options' as an argument to 
 * support handling of complex-valued data */
void afni_dset_to_svm_doc( DOC *docs, DatasetType **dsetArray, 
    MaskType* maskArray, ASLoptions *options, long tpts, long nvoxels, 
    long nmasked )
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
void getTmpLabels(LabelType *tmp_labels,long *sampleCount, LABELS *labels, long ind0, long ind1)
{
  long i;
  short labelWarningFlag = 0;  /* warn users if unknown class label - probably from multi-class */
  int class0 = labels->class_list[ind0];
  int class1 = labels->class_list[ind1];

  *sampleCount = 0;

  if(verbosity >= 2) printf("++ ");
  
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
      else if( labels->lbls[i] == -9999 ) { /* transductive sample */
        tmp_labels[i] = 0;
        (*sampleCount)++;
      }
      else if( labels->lbls[i] == 9999 ) { /* ignore sample */
        tmp_labels[i] = 9999;
      }
      else {
        tmp_labels[i] = 9999; /* invalid value - ignore */
        labelWarningFlag = 1;
        if(verbosity >= 2) printf("%ld,", i); /* ignored time point */
      }
    }
    else {
      tmp_labels[i] = 9999; /* censored sample - ignore */
    }
  }
 if( labelWarningFlag && (verbosity >= 1) ) {
   INFO_message("Time points ignored. If not using multi-class, check for bad labels.");
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
  free2f(afniModel->cAlphas,afniModel->combinations);
  
  
  /* JL Nov 2009: */
  if ( afniModel->eps != NULL ) { /* to be backwards compatible and allow
       testing with older 3dsvm versions */
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

void allocateAfniModel(AFNI_MODEL *afniModel, LABELS *labels, ASLoptions *options)
{ 
  
  ENTRY("allocateAfniModel");

  /* JL June 2009: Added this part to support sv-regression */
  /* We need to double the number of timepoints to make sv-regression work
   * with svm-light! */
  if( !strcmp(options->svmType, "regression") ) { 
    afniModel->timepoints = (int) (labels->n)*2;
  }
  else {
    afniModel->timepoints = (int) labels->n; 
    /* would like to be long, but no equivalent to THD_set_int_atr */
  }
  
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
  afniModel->b = (float *)malloc( afniModel->combinations * sizeof(float) );
  afniModel->alphas = Allocate2f((long) afniModel->combinations, (long) afniModel->timepoints);
  afniModel->cAlphas = Allocate2f((long) afniModel->combinations, (long) afniModel->timepoints);

  /* JL Nov 2009: */
  afniModel->eps = (float *)malloc( afniModel->combinations * sizeof(float) ); 
  afniModel->svm_c = (float *)malloc( afniModel->combinations * sizeof(float) );
  afniModel->biased_hyperplane = (int *)malloc( afniModel->combinations * sizeof(int) ); 
  afniModel->skip_final_opt_check = (int *)malloc( afniModel->combinations * sizeof(int) ); 
  afniModel->svm_maxqpsize = (int *)malloc( afniModel->combinations * sizeof(int) );
  afniModel->svm_newvarsinqp = (int *)malloc( afniModel->combinations * sizeof(int) );
  afniModel->svm_iter_to_shrink = (int *)malloc( afniModel->combinations * sizeof(int) );
  afniModel->transduction_posratio = (float *)malloc( afniModel->combinations * sizeof(float) );
  afniModel->svm_costratio = (float *)malloc( afniModel->combinations * sizeof(float) );
  afniModel->svm_costratio_unlab = (float *)malloc( afniModel->combinations * sizeof(float) );
  afniModel->svm_unlabbound = (float *)malloc( afniModel->combinations * sizeof(float) );
  afniModel->epsilon_a = (float *)malloc( afniModel->combinations * sizeof(float) );
  afniModel->epsilon_crit = (float *)malloc( afniModel->combinations * sizeof(float) );
  afniModel->compute_loo = (int *)malloc( afniModel->combinations * sizeof(int) );
  afniModel->rho = (float *)malloc( afniModel->combinations * sizeof(float) );
  afniModel->xa_depth = (int *)malloc( afniModel->combinations * sizeof(int) );
  
  
  /* allocation for alphas is checked in Allocate2f */
  if( afniModel->kernel_type == NULL || 
      afniModel->polynomial_degree == NULL || 
      afniModel->rbf_gamma == NULL || 
      afniModel->linear_coefficient == NULL || 
      afniModel->constant_coefficient == NULL || 
      afniModel->total_masked_features == NULL || 
      afniModel->total_samples == NULL || 
      afniModel->total_support_vectors == NULL || 
      afniModel->b == NULL || 
      
      /* JL Nov 2009: */
      afniModel->eps == NULL ||
      afniModel->svm_c == NULL ||
      afniModel->biased_hyperplane == NULL ||
      afniModel->svm_maxqpsize == NULL ||
      afniModel->svm_newvarsinqp == NULL ||
      afniModel->svm_iter_to_shrink == NULL ||
      afniModel->transduction_posratio == NULL ||
      afniModel->svm_costratio == NULL ||
      afniModel->svm_costratio_unlab == NULL ||
      afniModel->svm_unlabbound == NULL ||
      afniModel->epsilon_a == NULL ||
      afniModel->epsilon_crit == NULL ||
      afniModel->compute_loo == NULL ||
      afniModel->rho == NULL ||
      afniModel->xa_depth == NULL ) {

    ERROR_exit("Memory allocation in allocateAfniModel failed! "
        "Could not allocate afniModel members.\n");
  }
 
  EXRETURN;

}

  
void freeLabels(LABELS *labels) {
  ENTRY("freeLabels");

  free(labels->lbls);
  free(labels->cnsrs);
  
  EXRETURN;
}
   
void getLabels(LABELS *labels, char *labelFile, char *censorFile)
{
  FILE *fp;
  int class_exists_flag = 0;
  int nine_exists_flag = 0;
  long i,j,k;

   ENTRY("getLabels");

  /*----- RETRIEVE LABEL FILE --------------*/
  if( (fp = fopen(labelFile,"r")) == NULL ) {
    ERROR_exit("Could not open .1D label file: %s",labelFile);
  }
  
  labels->n = getFileSize(labelFile);

  labels->lbls = (LabelType*)malloc(sizeof(LabelType)*labels->n);
  if( labels->lbls == NULL ) {
    ERROR_exit("Memory allocation in getLabels failed! Could not allocate labels.");
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
      ERROR_exit("labelfile contains a negative entry in line %ld. " 
          "Check labelfile '%s'", i+1, labelFile);
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
    INFO_message( "Number of classes = %d\n", labels->n_classes );
    printf("++ "); 
    for( i = 0; i < labels->n_classes; ++i ) {
      printf( "class[%ld] = %d, ", i, labels->class_list[i] );
    }
    printf("\n");
  }
   if (labels->n_classes >= CLASS_MAX) {
   ERROR_exit("Max numer of classes hard coded to %d\n"
          "   Complain to the authors if you need more.", CLASS_MAX-1);
   }

  /*----- RETRIEVE CENSOR FILE --------------*/
  labels->cnsrs = (LabelType*)malloc(sizeof(LabelType)*labels->n);
  if( labels->cnsrs == NULL ) {
    ERROR_exit("Memory allocation error! could not allocate censors.");
  }

  if( censorFile[0] ) {
    if( (fp = fopen(censorFile,"r")) == NULL ) {
      ERROR_exit("Could not open .1D censor file: %s",censorFile);
    }
    if (nine_exists_flag == 1) { // Hey SL, You may want do this only if verbosity >= 2
      // -SL- I just might. Let's wait and see. Hey JL, how was your weekend?
      WARNING_message("Labelfile '%s' contains censor information and\n"
        "   censorfile '%s' was specified as well. "
        "Censoring data specified in both files.\n", labelFile, censorFile);
    }
    i = 0;
    while( !feof(fp) ) {
      if( i < labels->n ) {
        fscanf( fp,"%lf\n",&(labels->cnsrs[i]) );
      }
      else  {
        ERROR_exit("CensorFile (%s) is longer than expected length( %ld ).\n" 
            "   Make sure there are no extra lines at the end of this file.", 
            censorFile, labels->n);;
      }
      ++i;
    }
    fclose(fp);
  }
  else {
    for(i = 0; i < labels->n; ++i) {
       labels->cnsrs[i] = 1.0;
    }
  }

}

/* JL May 2009: This function may duplicate getLabels a bit, but for regression
 * a few things can be simplified:
 *  - we are only supporting censoring with a seperate censor file (not 9999s)
 *  - we don't have to worry about multi-class...
 */
LabelType* getRegressionLabels(LABELS *labels, char *labelFile, char *censorFile)
{
  FILE *fp          = NULL;
  long i, j         = 0;
  LabelType *target = NULL;
  long n9999        = 0;
  
  ENTRY("getRegressionLabels");
 
  /*--- open labelfile ---*/
  if( (fp = fopen(labelFile, "r") ) == NULL ) {
    ERROR_exit("Could not open .1D label file: %s",labelFile);
  }

  /*--- initialize ---*/
  labels->n = getFileSize(labelFile);
  labels->n_cnsrs = 0;

  /* to be able to use existing auxiliary functions: */
  labels->n_classes = 2;   
  for( j=0 ; j<CLASS_MAX ; ++j ) {
    labels->class_list[j] = 9999;     
  }

  /* -- allocate lbls -- */
  labels->lbls = (LabelType*)malloc(sizeof(LabelType)*labels->n);
  if( labels->lbls == NULL ) {
    ERROR_exit("Memory allocation in getRegressionLabels failed! Could not allocate labels.");
  }

  /*--- read labelfile ---*/
  for(i=0; i<labels->n; i++) {
      fscanf( fp,"%lf\n",&(labels->lbls[i]) );
  }
  fclose(fp);

  /*--- allocate cnsrs ---*/
  labels->cnsrs = (LabelType*)malloc(sizeof(LabelType)*labels->n);
  if( labels->cnsrs == NULL ) {
    ERROR_exit("Memory allocation in getRegressionLabels failed! Could not allocate censors.");
  }

  /*--- open censorfile ---*/
  if( censorFile[0] ) {
    if( (fp = fopen(censorFile,"r")) == NULL ) {
      ERROR_exit("Could not open .1D censor file: %s",censorFile);
    }

    /*--- read censorfile and count censors ---*/
    i=0;
    j=0;
    while( !feof(fp) ) {
      if( i<labels->n ) {
        fscanf( fp, "%lf\n", &(labels->cnsrs[i]) );
        if( (labels->cnsrs[i] != 0.0) && (labels->cnsrs[i] != 1.0) ) {
           ERROR_exit("Only 0 and 1 are allowed in censorfile!\n"  
             "   Check line %d of censorfile '%s'!", i+1, censorFile);
        }
        else if (labels->cnsrs[i] == 0.0) labels->n_cnsrs++;
        else ++j;
      }
      else {
        ERROR_exit("Censorfile '%s' is longer than expected length  %ld.\n" 
            "   Make sure there are no extra lines at the end of this file.", 
            censorFile, labels->n);
      } ++i;
    }
    if (j+labels->n_cnsrs != labels->n)
      ERROR_exit("Number of lines %ld in labelfile '%s' does not match\n"
          "   number of lines %ld in censorfile '%s'!", labels->n, labelFile,
         j+labels->n_cnsrs, censorFile); 
    fclose(fp);
  }
  else {
    for(i=0; i<labels->n; ++i) {
       labels->cnsrs[i] = 1;
    }
  }

  /*--- allocate target ---*/
  target = (LabelType *) malloc( (labels->n-labels->n_cnsrs)*sizeof(LabelType) );
  if( labels->cnsrs == NULL ) {
    ERROR_exit("Memory allocation in getRegressionLabels failed! Could not allocate target.");
  }

  /*--- check labels and create target ---*/
  j=0;
  for( i=0; i<labels->n; ++i ) {
    /* what should we do with label -9999 SL? Is there a transductive mode in
     * sv-regression? */
    if( labels->lbls[i] == 9999 ) {
      ++n9999;
    }
    if( labels->cnsrs[i] != 0.0 ) {
      target[j] = labels->lbls[i];
      ++j;
    }
  }

  if ( n9999 != 0 ) {
    WARNING_message("Labelfile '%s' contains '9999' '%ld' times.\n"
          "   For classification, '9999' can be used to ignore timepoints.\n"
          "   However, in regression (-type regression, you are running it right now)\n"
          "   '9999' can not be used to ignore timepoints\n"
          "   Please use a censorfile (option: -censor)", labelFile, n9999 ); 
    }
  
  RETURN(target);
}


/* JL May 2009: This function reads in an afni dataset and returns
 * a 2D array [time][voxel] without censored time-points given the labels
 * For regression a few things can be simplified:
 * - we are only supporting censoring with a separate censor file (not 9999s)
 * - we don't have to worry about multi-class..
 */
DatasetType** getRegressionArray(THD_3dim_dataset *dset, LABELS *labels)
{
  long  v         = 0; 
  long  t         = 0; 
  long  nt        = 0; 
  long  ta        = 0; 
  long  nt_all    = 0;
  long  nt_ncnsrd = 0; 
  long  nvox      = 0;
  DatasetType **dsetArray    = NULL;
  DatasetType *tmp_dsetArray = NULL;

  ENTRY("getRegressionArray");

  nvox = DSET_NVOX( dset );
  nt_all = labels->n;
  nt_ncnsrd = labels->n_cnsrs;
  nt = nt_all-nt_ncnsrd;

  dsetArray = Allocate2DT( nt, nvox);
  
  /* extract non-censored time-points */
  t=0;
  for( ta=0; ta<nt_all; ++ta ) {
    if( labels->cnsrs[ta] != 0 ) { 
      tmp_dsetArray = (DatasetType*)DSET_ARRAY(dset,ta);
      for( v=0 ; v<nvox ; ++v ) {
        dsetArray[t][v] = tmp_dsetArray[v];          
      } ++t;
    }
  }

  RETURN(dsetArray);
}

void test_routine (ASLoptions *options, MODEL *model, AFNI_MODEL *afniModel, 
    THD_3dim_dataset *dsetTest, THD_3dim_dataset *dsetMask, 
    THD_3dim_dataset *dsetModel, int argc, char **argv)
{
  char* inModelFile;
  char inModelFileMask[MAX_FILE_NAME_LENGTH];
  char* inModelFileMaskExt = MODEL_MSK_EXT;
  long nt;                      /* number of time points in TEST dataset */
  long nvox;                    /* number of voxels per time point in TEST dataset */
  long nt_mod, nvox_mod;	    /* for MODEL dataset */
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

  ENTRY("test_routine"); 
    
  if (verbosity >= 1) INFO_message("\n++ CLASSIFICATION (testing):\n++");

  /*----- INITIAL ERROR CHECKING ------*/
  /* afniModel is loaded in main() (get_afni_model) */

  if( afniModel == NULL ) { 
      ERROR_exit("Model could not be loaded!");
  }

  /*----- LOAD TEST DATA --------*/
  dsetTest = THD_open_one_dataset( options->testFile );
  if ( dsetTest == NULL ) {
    ERROR_exit("Failed to open test dataset: %s", options->testFile );
  }
  DSET_load( dsetTest );
  nt = DSET_NUM_TIMES( dsetTest );
  nvox = DSET_NVOX( dsetTest ); 

  /* JL Jan 2009: Quick bugfix -thanks Cameron: Datum type short is the only
   * datum type, that is supported in 3dsvm for now. We need to change how we
   * cast input data in the future.
  */
  if( DSET_BRICK_TYPE(dsetTest,0) != MRI_short ) {
  	   ERROR_exit("Sorry, datum type short is the only datum type that is supported for now!");
   }

  /*----- GET TEST LABELS ------- */
  if( options->testLabelFile[0] ) {

    /* JL: included censorfile for testing in getLabels */
    getLabels(&testLabels, options->testLabelFile, options->censorFile);     
    if(testLabels.n != nt) {
        ERROR_exit("Number of labels do not match the length of the test dataset:\n"
	    "   labelfile '%s' contains %ld labels, but the \n"
	    "   testfile '%s' contains %ld entries. ",options->testLabelFile, 
        testLabels.n, options->testFile, nt);
    }

   
    /* ---- allocate tmp_labels ---- */
    tmp_labels = (LabelType*)malloc(sizeof(LabelType)*testLabels.n); /* for timeseries data */
    if( tmp_labels == NULL ) {
      ERROR_exit("Memory allocation in test_routine! Could not allocate temporary labels.");
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
  /* would be great to change the mask datatype to short and include it
   * as a sub-brick of the model */
  
  if( !(options->outModelNoMask) ) {
    /* ---- determine file name for model-mask (JL) ---- */ 
    inModelFile = DSET_PREFIX( dsetModel );
    strcpy(inModelFileMask,inModelFile);
    strcat(inModelFileMask,inModelFileMaskExt);
    
    if (dsetModel->view_type == VIEW_ORIGINAL_TYPE) {
      strcat(inModelFileMask,"+orig");
    }
    else if (dsetModel->view_type == VIEW_TALAIRACH_TYPE) {
      strcat(inModelFileMask,"+tlrc");
    }
    else if (dsetModel->view_type == VIEW_ACPCALIGNED_TYPE)  {
      strcat(inModelFileMask,"+acpc");
    }
    else {
      ERROR_exit("Viewtype of model: %s unknown!", options->modelFile);
    }
    
    /* ---- open mask dataset ---- */
    dsetMask = THD_open_one_dataset( inModelFileMask );
    if ( dsetMask == NULL ) {
      ERROR_exit("Failed to open mask dataset: %s. If not using a mask file, " 
          "you must use option -nomodelmask\n", inModelFileMask );
    }
    DSET_load( dsetMask );
    dsetMaskArray = (MaskType*)DSET_ARRAY(dsetMask,0);
  }
  
  /*----- SETTING MCTYPE FOR MULTICLASS -----*/
  if ((options->multiclass[0]) && (afniModel->class_count > 2)) { 
      if ( !strcmp(options->multiclass,"DAG") ) mctype = MCTYPE_DAG;
      else if ( !strcmp(options->multiclass,"vote") ) mctype = MCTYPE_VOTE;
      else { 
	    WARNING_message("-multiclass was specified with an unknown option: %s\n"
            "   Setting mctype = DAG [default].", options->multiclass); 
        mctype = MCTYPE_DAG;
     }
  }
  else {
    if(verbosity >= 2) INFO_message("Setting multiclass type to DAG [default].");
  }

  /*----- ERROR CHECKING FOR MULTICLASS ----- */
   if ( options->testLabelFile[0] && verbosity >= 2 ) {
       if ( testLabels.n_classes != afniModel->class_count ) {     
        INFO_message("NOTE: number of class categories do not match:\n"
            "   labelfile: '%s' contains %d categories, but the\n"
            "   moelfile: '%s' contains %d categories.", 
              options->testLabelFile, testLabels.n_classes, 
              options->modelFile, afniModel->class_count); 
       } 
   }        
   if ( (options->multiclass[0]) && (options->testLabelFile[0]) ) {
      if ( testLabels.n_classes < 3 ) { 
		  INFO_message("NOTE: -multiclass was specified, but\n"
              "   labelfile '%s' contains only %d categories.", 
              options->testLabelFile,testLabels.n_classes);
      }
   }
 
  /*----- FILL DOC STRUCTURE FROM TEST DATASET -----*/
  docsTest = (DOC*)malloc(sizeof(DOC)*nt);                           /* svm-light data structure */
  AllocateDOCwords(docsTest, nt, afniModel->total_masked_features[0]);

  /* assuming total_masked_features are all the same and same mask for training and testing */
  afni_dset_to_svm_doc( docsTest, dsetTestArray, dsetMaskArray, options, nt, nvox, afniModel->total_masked_features[0]); 

  allocateModel( model, afniModel);
  get_svm_model(model, dsetModelArray, dsetMaskArray, afniModel, nvox_mod, options->outModelNoMask);

  /*----- Allocate test predictions arrays --------*/
  dist = (float *)malloc(sizeof(float)*nt);
  if( (options->testLabelFile[0]) && (testLabels.n_cnsrs != 0) ) {
  dist_cnsrs = (float *)malloc(sizeof(float)*(nt-testLabels.n_cnsrs));
  }

  /* Note: if not multiclass these may not get used - moreover, if only one multiclass approach
   *       still not everything will get used. So perhaps being a little inneficient here */
  /* multiclass_dist = (double *)calloc(sizeof(double),nt); -- SL Aug. 08*/
  multiclass_dist = Allocate2f((long) afniModel->combinations, (long) nt);
  classCorrect = (float *)malloc(sizeof(float)*afniModel->class_count);
  classIncorrect = (float *)malloc(sizeof(float)*afniModel->class_count);
  nClass = (long *)malloc(sizeof(long)*afniModel->class_count);
  classVote = (int *)malloc(sizeof(long)*afniModel->class_count);

  
  for(i = 0; i < afniModel->combinations; ++i ) {
    if(verbosity >= 1) {
      INFO_message(" ");
      INFO_message("--------------------------------------------------------------"
          "------------------");
      INFO_message("Category combination = %ld  (%s)",i,afniModel->combName[i]);
    }
 
    if( options->testLabelFile[0] ) {
      /* use strtok to recover combination name integers so that we can use the test label data */
      strcpy(p,afniModel->combName[i]);
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
    updateModel(model, afniModel, options, (int) i); 

    if(afniModel->class_count == 2) {
      sprintf(predictionsFile, "%s.1D", options->predFile);
    }
    else {
      sprintf(predictionsFile, "%s_%s.1D", options->predFile, afniModel->combName[i]);
    }
    if( (fp = fopen( predictionsFile, "w" )) == NULL ) {
      ERROR_exit("Could not open file for writing predictions: %s", predictionsFile );
    }
    
    /* JL Feb. 2009: Changed this part to support non-linear kernels */
    if (afniModel->kernel_type[i] == LINEAR) { // linear kernel 
      for(j = 0; j < nt; ++j) {
        dist_tmp=classify_example_linear(model,&docsTest[j]);
        /* should do something smarter than re-casting double to float */
        dist[j]= (float) dist_tmp;
        }
    }
    else { /* non-linear kernel */
      model->kernel_parm.poly_degree = afniModel->polynomial_degree[i];
      model->kernel_parm.coef_lin = afniModel->linear_coefficient[i];
      model->kernel_parm.coef_const = afniModel->constant_coefficient[i];
      model->kernel_parm.rbf_gamma = afniModel->rbf_gamma[i];
      strcpy(model->kernel_parm.custom, afniModel->kernel_custom[i]);

      for(j = 0; j < nt; ++j) {
        dist_tmp=classify_example(model,&docsTest[j]);
        dist[j]= (float) dist_tmp;
      }
    }
    
    /* JL Nov. 2008 : Changed detrending for censored timepoints */
    if( (options->testLabelFile[0]) && (testLabels.n_cnsrs != 0) && (!options->noPredDetrend)) {
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
    if( options->testLabelFile[0] ) {
      for(j = 0; j < nt; ++j){
        if( abs(tmp_labels[j]) != 9999) {
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
    }

    if(options->testLabelFile[0] && (verbosity>=1)) {
      INFO_message(" ");
      INFO_message("Aaccuracy on test set: %.2f%% (%d correct, %d incorrect, %ld total)",
              (float)(correct)*100.0/sampleCount,(int)rint(correct),
              (int)rint(incorrect),sampleCount);
      INFO_message(" ");
      
      //can put this in after it is included for overall multiclass
      //printf("Precision/recall on test set: %.2f%%/%.2f%%\n",(float)(res_a)*100.0/
      //(res_a+res_b),(float)(res_a)*100.0/(res_a+res_c));
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
    if(verbosity >= 1)  INFO_message("Predictions written to %s\n",predictionsFile);
  }

  if(afniModel->class_count > 2) {

    if( options->testLabelFile[0] ) {
      correct=0.0; 
      incorrect=0.0;
      no_accuracy=0.0;
      res_a=0.0;
      res_b=0.0;
      res_c=0.0;
      res_d=0.0;
      for(c = 0; c < afniModel->class_count; ++c) {
	classCorrect[c] = 0.0;
	classIncorrect[c] = 0.0;
	nClass[c] = 0L;
      }
    }

    /* Multiclass: voting method */
    if (mctype == MCTYPE_VOTE) {
      sprintf(predictionsFile, "%s_overall_vote.1D", options->predFile);
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
            INFO_message("model number:%ld time point:%ld classifier output=%f"
                ,i,j,multiclass_dist[i][j]);
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
            if(verbosity >=2) printf("+ classVote[%ld] = %d;   ",i, classVote[i]);
        }
        if(verbosity >=2) printf("\n");
           
        /* code is largely duplicated in DAG ................. */
        if(verbosity >=2) INFO_message("Voting result: observation number=%ld"
            "model number=%d, classAssignment = %d\n",j, DAG, classAssignment);
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
            INFO_message("Overall:  test labels=%d, current number correct = %d" 
                "incorrect = %d", (int) rint(testLabels.lbls[j]), 
                (int) rint(correct), (int) rint(incorrect));
            for(c = 0; c < afniModel->class_count; ++c) {
              INFO_message("Class Specific:  classLabel=%ld, current number" 
                "correct = %d incorrect = %d", c, (int) rint(classCorrect[c]),
                (int) rint(classIncorrect[c]) );
            }
          }
        }
      }
    fclose(fp);
    }  
   
    else { // if (mctype == MCTYPE_DAG)
      sprintf(predictionsFile, "%s_overall_DAG.1D", options->predFile);
      if( (fp = fopen( predictionsFile, "w" )) == NULL ) {
        ERROR_exit("Could not open file for writing predictions: %s", 
            predictionsFile );
      }

      if(verbosity >= 1) 
        INFO_message(" ");
        INFO_message("---------------------------------- DAG" 
            "-----------------------------------------");
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
	        if(verbosity >= 2) printf("++ classifier output = %f  ",multiclass_dist[DAG][j]);
	        if(multiclass_dist[DAG][j]>0) {
		      if(edgeFlag) {
		        DAG += afniModel->class_count - i - 1;
		        if(verbosity >=2) INFO_message("next model number=%d, current max" 
                    "possible classAssignment = %d", DAG, classAssignment);
		        }   
		        else {
		          DAG += afniModel->class_count - i;
		          if(verbosity >=2) INFO_message("next model number=%d, current max"
                      "possible classAssignment = %d", DAG, classAssignment);
		        }
	        }
	        else {
		      edgeFlag = 0;
		      DAG--;
		      classAssignment--;
		      if(verbosity >=2) INFO_message("next model number=%d, current max"
                  "possible classAssignment = %d", DAG, classAssignment);
	        }
	      }

	    if(verbosity >=2) INFO_message("DAG result: observation number=%ld model"
            "number=%d, classAssignment = %d",j, DAG, classAssignment);
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
          INFO_message("Overall:  test labels=%d, current number correct = %d"
            "incorrect = %d", (int) rint(testLabels.lbls[j]),
            (int) rint(correct), (int) rint(incorrect));
		    for(c = 0; c < afniModel->class_count; ++c) {
            INFO_message("Class Specific:  classLabel=%ld, current number" 
              "correct = %d   incorrect = %d",
              c, (int) rint(classCorrect[c]),(int) rint(classIncorrect[c]) );
            }
          }
        }
      }
    }
  fclose(fp);
  }
  if(verbosity >= 1)  INFO_message("Predictions for all categories written to %s",
    predictionsFile);


/* this was original multiclass. For now only supporting 2-class DAG and voting - will add different methods in future. */
#if 0 
  if(afniModel->class_count > 2) {
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
      multiclass_dist[j] = 0.5*( multiclass_dist[j]+afniModel->class_count-1 ); /* convert output prediction to {0,combinations-1} class scale */
      multiclassTmp = rint(multiclass_dist[j]);  /*round*/
      if(multiclassTmp > afniModel->class_count-1) multiclassTmp = (double) afniModel.class_count - 1.0;
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

  if(options->testLabelFile[0] && afniModel->class_count > 2 && (verbosity>=1)) {  
    INFO_message("Overall accuracy on multiclass test set: %.2f%% (%d correct,"
        "%d incorrect, %d total)", (float)(correct)*100.0/
        ( (int)rint(correct)+(int)rint(incorrect)),(int)rint(correct),
        (int)rint(incorrect),(int)rint(correct)+(int)rint(incorrect) );

    INFO_message("Individual Breakdown:");
    for(c = 0; c < afniModel->class_count; ++c) {
	/* possible nan output if no examples of the current class, 
    * c (in otherwords divide by nClass = 0) */
	  INFO_message("                       classLabel=%ld: %.2f%% (%d correct,"
        "%d incorrect, %ld total)\n", c, (float)(classCorrect[c])*100.0/nClass[c],
        (int)rint(classCorrect[c]), (int)rint(classIncorrect[c]), nClass[c] );
    }
  }

  free(dist);
  
  free(classCorrect);
  free(classIncorrect);
  free(nClass);
  free(classVote);
  freeModel( model, afniModel);
  freeAfniModel(afniModel);
  freeDOCwords(docsTest, nt);
  free(docsTest);
  free2DT( dsetTestArray, nt );
  free2DT( dsetModelArray, nt_mod );
  DSET_unload( dsetModel );
  free2f(multiclass_dist, afniModel->combinations);
  if( !(options->outModelNoMask) ){
    DSET_unload(dsetMask);
  }
  if( (options->testLabelFile[0]) && (testLabels.n_cnsrs != 0) ) {
    free(dist_cnsrs);
  }
  DSET_unload( dsetTest );
  
  EXRETURN;
}

void get_afni_model(ASLoptions *options, AFNI_MODEL *afniModel, 
    THD_3dim_dataset *dsetModel, enum modes *mode, int *svm_type)
{
  
  /* JL May 2009: This functions reads in the afni model to determine model
   * parameters before testing is performed. This may not be necessary, 
   * if training and testing are preformed all at once.
   *
   * JL Oct. 2009: Added viewtype postfix for modelfile if training and testing
   * are performed together. 
   *  
   *      *************************************************
   *      *       Don't forget to free afniModel,         *
   *      *       when using this function!               *
   *      *************************************************
   *
   */
  
  ENTRY("get_afni_model");

  /*----- VIEWTYPE POSTFIX FOR MODEL (quick fix !!!) -----*/
  /* It would be better to not read in the model from disc */ 
  if ( *mode == TRAIN_AND_TEST ) {
    if ( strstr(options->trainFile,"+orig") != NULL ) {
      if ( strstr(options->testFile,"+orig") == NULL ) {
        ERROR_exit("Viewtype of train dataset: %s does not match\n"
            "   Viewtype of test dataset: %s!", options->trainFile, 
            options->testFile);
      }
      else strcat(options->modelFile,"+orig");
    }

    else if ( strstr(options->trainFile,"+tlrc") != NULL ) {
      if ( strstr(options->testFile,"+tlrc") == NULL ) {
        ERROR_exit("Viewtype of train dataset: %s does not match\n"
            "   Viewtype of test dataset: %s!", options->trainFile, 
            options->testFile);
      }
      else strcat(options->modelFile,"+tlrc");
    }

    else if ( strstr(options->trainFile,"+acpc") != NULL ) {
      if ( strstr(options->testFile,"+acpc") == NULL ) {
        ERROR_exit("Viewtype of train dataset: %s does not match\n"
            "   Viewtype of test dataset: %s!", options->trainFile, 
            options->testFile);
      }
      else strcat(options->modelFile,"+acpc");
    }

    else ERROR_exit("Viewtype unknown!");
  }

  /*----- LOAD AFNI MODEL -----*/
  dsetModel = THD_open_one_dataset( options->modelFile );
  if ( dsetModel == NULL ) {
    ERROR_exit("Failed to open model dataset: %s", options->modelFile );
  }
  DSET_load( dsetModel );

  /*----- READ AFNI MODEL -----*/ 
  readAllocateAfniModel(dsetModel, afniModel);

  /* --- set svm_type ---*/
  if( !strcmp(afniModel->svm_type, "regression") )  *svm_type = REGRESSION;
  else  *svm_type = CLASSIFICATION; 

  /*----- FREE MEMORY ------*/
  DSET_unload( dsetModel );

  /* would be great to pass dsetModel to the test function and avoid 
   * reading it in twice */

  EXRETURN;
}


/* JL May 2009: Implemented this funciton to support sv-regression in 3dsvm. 
 * It is very similar to test_routine() (a lot of code has been recycled). 
 * However, major differences are:
 *
 *    - No need for mulitclass
 *    - No detrending
 *    - New function to read in the labelfile ( getRegressionLabels() )
 *    - Using rms error as a benchmark
 */ 

void test_regression (ASLoptions *options, MODEL *model, AFNI_MODEL *afniModel, 
    THD_3dim_dataset *dsetTest, THD_3dim_dataset *dsetMask, THD_3dim_dataset *dsetModel, 
    int argc, char **argv)
{
  long nt             = 0;        /* number of time points in test dataset */
  long nvox           = 0;        /* number of voxels per time point in test dataset */
  long nt_mod         = 0;        /* nt for model dataset */
  long nvox_mod       = 0;	      /* nvox for model dataset */
  
  DOC                             /* array to hold test dataset in svm-light data */
    *docsTest         = NULL;     /* structure */
  DatasetType 
    **dsetTestArray   = NULL;	  /* array to hold test dataset values */
  DatasetType 
    **dsetModelArray  = NULL;	  /* array to hold model dataset values */
  DatasetType
    *tmp_dsetArray    = NULL;	  /* temporary array to hold dataset values */

  MaskType 
    *dsetMaskArray    = NULL;
 
  double dist_tmp     = 0;        /* temporary variabel */
  double *dist        = NULL;     /* array holding the classification results for
                                     each tinepoint */

  LabelType *target   = NULL;     /* labels without censored timepoints. Assuming 
                                     the 'truth' is known and we want to determine
                                     the error. */ 
  LABELS testLabels;  

  long i,j,c          = 0;
  
  FILE *fp            = NULL;

  char *inModelFile   = NULL;
  char inModelFileMask[MAX_FILE_NAME_LENGTH];
  char *inModelFileMaskExt = MODEL_MSK_EXT;
  char predictionsFile[MAX_FILE_NAME_LENGTH];
  double rms          = 0;         /* to calculate rms error */
  
  ENTRY("test_regression"); 
 
  if (verbosity >= 1) INFO_message("\n++ REGRESSION (testing):\n++");

  /*----- INITIAL ERROR CHECKING ------*/
  /* afniModel is loaded in main() (get_afni_model) */
   
  if( afniModel == NULL ) { 
      ERROR_exit("Model could not be loaded!");
  }
  if (afniModel->kernel_type[0] != LINEAR) {
    ERROR_exit("At this point SV-regression is only supported for linear"
        "kernels");
  }

  /*----- LOAD TEST DATA --------*/
  dsetTest = THD_open_one_dataset( options->testFile );
  if ( dsetTest == NULL ) {
    ERROR_exit("Failed to open test dataset: %s", options->testFile );
  }
  DSET_load( dsetTest );
  nt = DSET_NUM_TIMES( dsetTest );
  nvox = DSET_NVOX( dsetTest ); 

  /* JL Jan 2009: Quick bugfix -thanks Cameron: Datum type short is the only
   * datum type, that is supported in 3dsvm for now. We need to change how we
   * cast input data in the future.
   */
  if( DSET_BRICK_TYPE(dsetTest,0) != MRI_short ){
	  ERROR_exit("Sorry, datum type short is the only datum type that is supported for now!");
  }

  /*----- GET TEST LABELS -------*/
  if( options->testLabelFile[0] ) {
    target = getRegressionLabels(&testLabels,options->testLabelFile, 
      options->censorFile);

    if( testLabels.n != nt ) {
      ERROR_exit("Number of labels do not match the length of the test dataset:\n" 
          "   labelfile: '%s' contains %ld labels, but the \n"
          "   test dataset: '%s' contains %ld entries. ", 
          options->testLabelFile, testLabels.n, options->testFile, nt);
    }
  }

  /*----- PRODUCE TEST DATA ARRAY -------*/
  dsetTestArray = Allocate2DT( nt, nvox ) ;
  for( i=0; i<nt; ++i ) {
    tmp_dsetArray = (DatasetType*)DSET_ARRAY( dsetTest,i );
    for( j=0 ; j<nvox ; ++j ) {
      dsetTestArray[i][j] = tmp_dsetArray[j];   
    }
  }

  /*----- LOAD AND PRODUCE MODEL ARRAY -----*/
  dsetModel = THD_open_one_dataset( options->modelFile );
  DSET_load ( dsetModel );
  nt_mod = DSET_NUM_TIMES( dsetModel );
  nvox_mod = DSET_NVOX( dsetModel );
  
  dsetModelArray = Allocate2DT( nt_mod, nvox_mod);
  for( i=0; i<nt_mod; ++i) {
    tmp_dsetArray = (DatasetType*)DSET_ARRAY(dsetModel,i);
    for (j=0; j<nvox_mod; ++j) {
      dsetModelArray[i][j] = tmp_dsetArray[j];
    }
  }

  /*----- LOAD MODEL MASK -----*/
  /* would be great to change the mask datatype to short and include it
   * as a sub-brick of the model */
  
  if( !(options->outModelNoMask) ) {

    /* ---- determine file name for model-mask ---- */ 
    inModelFile = DSET_PREFIX( dsetModel );
    strcpy(inModelFileMask,inModelFile);
    strcat(inModelFileMask,inModelFileMaskExt);
    
    if (dsetModel->view_type == VIEW_ORIGINAL_TYPE) {
      strcat(inModelFileMask,"+orig");
    }
    else if (dsetModel->view_type == VIEW_TALAIRACH_TYPE) {
      strcat(inModelFileMask,"+tlrc");
    }
    else if (dsetModel->view_type == VIEW_ACPCALIGNED_TYPE)  {
      strcat(inModelFileMask,"+acpc");
    }
    else {
      ERROR_exit("Viewtype of model: %s unknown!", options->modelFile);
    }
    
    /* ---- open mask dataset ---- */
    dsetMask = THD_open_one_dataset( inModelFileMask );
    if ( dsetMask == NULL ) {
      ERROR_exit("Failed to open mask dataset: %s. If not using a mask file, " 
          "you must use option -nomodelmask\n", inModelFileMask );
    }
    DSET_load( dsetMask );
    dsetMaskArray = (MaskType*)DSET_ARRAY(dsetMask,0);
  }

  /*----- ALLOCATE AND FILL DOC STRUCTURE -----*/
  docsTest = (DOC*)malloc(sizeof(DOC)*nt);
  AllocateDOCwords(docsTest, nt, afniModel->total_masked_features[0]);
  afni_dset_to_svm_doc( docsTest, dsetTestArray, dsetMaskArray, options, 
      nt, nvox, afniModel->total_masked_features[0]); 

  /*----- ALLOCATE AND FILL SVM MODEL -----*/
  allocateModel( model, afniModel);
  get_svm_model(model, dsetModelArray, dsetMaskArray, afniModel, nvox_mod,
      options->outModelNoMask);
  updateModel(model, afniModel, options, 0);

  /*----- ALLOCATE PREDICTION ARRAY --------*/
  dist = (double *)malloc(sizeof(double)*nt);

  /*----- PREDICTION OUTPUT FILE -----*/
  sprintf(predictionsFile, "%s.1D", options->predFile);
  if( (fp = fopen(predictionsFile, "w" )) == NULL ) {
      ERROR_exit("Could not open file for writing predictions: %s", 
          predictionsFile );
    }

  /*----- PERFORM TESTING & WRITE PREDICTIONS -----*/
  for(j=0; j<nt; ++j) {
    dist_tmp = classify_example_linear(model,&docsTest[j]);
    dist[j] = (float)dist_tmp;
  }

  /*----- WRITE PREDICTIONS TO FILE -----*/
  for(j=0; j<nt; ++j) {
    if ( options->testLabelFile[0] ) {
      if( testLabels.cnsrs[j] == 1 ) {
        fprintf(fp, "%lf\n", dist[j]);
      }
    }
    else {
      fprintf(fp, "%lf\n", dist[j]);
    }
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

    INFO_message("RMS error: %.2f (%d censored, %ld total)",
        rms, testLabels.n_cnsrs, testLabels.n);

    INFO_message("--------------------------------------------------------------"
          "----------------\n++");
  }
  if(verbosity >= 1) INFO_message("Predictions written to %s\n", 
      predictionsFile);

  
  /*----- FREE MEMORY -----*/   
  fclose( fp );
  free( dist );
  freeModel( model, afniModel);
  freeAfniModel( afniModel );
  freeDOCwords( docsTest, nt );
  free( docsTest );
  free2DT( dsetTestArray, nt );
  free2DT( dsetModelArray, nt_mod );
  if( options->testLabelFile[0] ) {
    freeLabels( &testLabels );
    free( target );
  }
  DSET_unload( dsetModel );
  DSET_unload( dsetTest );
  if( !(options->outModelNoMask) ){
    DSET_unload( dsetMask );
  }
  
  EXRETURN;
}

/* SL & JL Feb. 2009: Included 'long *kernel_cache_size' as an argument to
 * support non-linear kernels. */

void train_routine(MODEL *model, LEARN_PARM *learn_parm, KERNEL_PARM *kernel_parm, 
    long *kernel_cache_size, ASLoptions *options, THD_3dim_dataset *dsetTrain, 
    THD_3dim_dataset *dsetMask, MaskType *dsetMaskArray, int argc, char **argv)
{
  LABELS labels;
  AFNI_MODEL afniModel;			  /* holds everything required to write out model.Head */
  MODEL_MAPS maps;                /* holds the maps (e.g., weight-vector maps)
                                     for the bucket */
  long map_index;                 /* index counting number of maps written into maps->data
                                     map_index is updated in addToModelMap_bucket() */ 
  long classCount;			      /* in training loop, keeps track of current pairwise comb */
  long sampleCount = 0;			  /* number of samples used in training */
  LabelType *tmp_labels, *target;  
  DatasetType **dsetTrainArray;		/* array to hold training dataset values */
  DatasetType *tmp_dsetTrainArray;	/* temporary array to hold training dataset values */
  DOC*    docsTrain;                /* SVM-Light data structure used for training */
  KERNEL_CACHE kernel_cache;
  long nt;                       /* number of time points in TRAIN dataset */
  long nvox;                     /* number of voxels per time point in TRAIN dataset */
  long nvox_masked = 0;
  long i,j,k, cc, dd;
  char alphaFile[MAX_FILE_NAME_LENGTH];	  /* naming of alphafile output */	
  char docFileName[MAX_FILE_NAME_LENGTH]; /* nameing of svm-light textfile  output */
   
  ENTRY("train_routine");
   
  
  if (verbosity >= 1) INFO_message("\n++ CLASSIFICATION (training):\n++");


  /*----- LOAD TRAINING DATA --------*/
  dsetTrain = THD_open_one_dataset( options->trainFile );
  if ( dsetTrain == NULL ) {
    ERROR_exit("Failed to open training dataset: %s", options->trainFile );
  }
  DSET_load( dsetTrain );
  nt = DSET_NUM_TIMES( dsetTrain );
  nvox = DSET_NVOX( dsetTrain );
  nvox_masked = nvox; /* this will be modified later if mask used */

  /* JL Jan 2009: Quick bugfix -thanks Cameron: Datum type short is the only
   * datum type, that is supported in 3dsvm for now. We need to change how we
   * cast input data in the future.
   */
  if( DSET_BRICK_TYPE(dsetTrain,0) != MRI_short ){
	  ERROR_exit("Sorry, datum type short is the only datum type that is supported for now!");
  }

  if(verbosity >= 1)  
    INFO_message( "Number of time samples is %ld, and voxels %ld in training "
        "dataset.",nt,nvox );

  /*----- GET MASK ARRAY, IF SELECTED AND DETECT nvox_masked --------*/
  if( options->maskFile[0] ) {
    nvox_masked = 0;
    dsetMask = THD_open_one_dataset( options->maskFile );
    if ( dsetMask == NULL ) {
      ERROR_exit("Failed to open mask file: %s", options->maskFile );
    }
    DSET_load(dsetMask);
    if( DSET_BRICK_TYPE(dsetMask,0) != MRI_byte ) {
      ERROR_exit("Mask file: %s is not a byte-format brick.\n",
          options->maskFile );
    }
    dsetMaskArray = (MaskType*)DSET_ARRAY(dsetMask,0);
    for( i=0 ; i<nvox ; ++i ) {
      if( dsetMaskArray[i] )
        nvox_masked++;
    }
    if(verbosity >= 1) 
      INFO_message( "The number of non-zero elements in mask is: %ld\n",
          nvox_masked );
  }
  else if( !(options->outModelNoMask) ){
    ERROR_exit("No mask file specified (use -mask file). "
        "If not using a mask file must use option -nomodelmask");
  }

  /*----- RETRIEVE LABELS --------------*/
  labels.n = nt;
  getLabels(&labels,options->labelFile, options->censorFile);

  /*----- ALLOCATE afniModel --------------*/
  allocateAfniModel(&afniModel,&labels, options);

  /*----- ALLOCATE tmp_labels --------------*/
  /* for timeseries data */
  tmp_labels = (LabelType*)malloc(sizeof(LabelType)*labels.n);
  if( tmp_labels == NULL ) {
    ERROR_exit("Memory allocation error in train_routine! " 
        "Could not allocate temporary labels.");
  }
  
  /*----- ALLOCATE maps -----*/
  if ( options->modelWeightFile[0] ) {
    allocateModelMaps(&maps, (long)labels.n_classes, nvox, options->kernelName);
  }
  
  /*----- svmlearn for all combinations of pairwise training --------*/
  /* cc indexes -1 class; dd indexes +1 class - over multiple classes */
  classCount = 0; /* could figure it out from cc and dd, but easier just to keep track */
  map_index = 0;

  for( cc=0 ; cc < labels.n_classes-1; ++cc ) {
    for( dd=cc+1 ; dd < labels.n_classes; ++dd ) {
      
      if(verbosity >= 1)  { 
         INFO_message("\n++ Preparing classes %d and %d:", 
             labels.class_list[cc], labels.class_list[dd]);
         if (verbosity > 1) MCHECK ; fflush(stdout) ; /* ZSS */
      }   

      getTmpLabels(tmp_labels, &sampleCount, &labels, cc, dd);
      if(verbosity >= 1) INFO_message( "SampleCount = %ld\n", sampleCount );

      /*----- ALLOCATE MEMORY for svmlight arrays -----------*/
      docsTrain = (DOC*)malloc(sizeof(DOC)*sampleCount);
      target = (LabelType*)malloc(sizeof(LabelType)*sampleCount);
      if( docsTrain == NULL || target == NULL ) {
        ERROR_exit("Memory allocation error in train_routine! " 
            "Could not allocate docsTrain and/or target.\n");
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

      /* alpha file output may not be required if training and testing */
      /* performed at same time. */
      if(options->modelFile[0]) {
        if( options->modelAlphaFile[0] ) { 	/* user defined alpha file name */
          sprintf( alphaFile, "%s_%d_%d.1D", options->modelAlphaFile, labels.class_list[cc], labels.class_list[dd] );
          strcpy( learn_parm->alphafile, alphaFile);
        }
      }

      AllocateDOCwords(docsTrain, sampleCount, nvox_masked);

      afni_dset_to_svm_doc( docsTrain, dsetTrainArray, dsetMaskArray, 
          options, sampleCount, nvox, nvox_masked );

      /* SL & JL Feb. 2009: Added this part to initialize the kernel parameters
       * in case of non-linear kernels. */
      if(kernel_parm->kernel_type == LINEAR) { 
        /* don't need the cache if linear*/


        svm_learn_classification( docsTrain, target, sampleCount, nvox_masked, 
            learn_parm, kernel_parm, NULL, model );
      
      }
      else { 
        /* Always get a new kernel cache. It is not possible to use the
         * same cache for two different training runs */
        kernel_cache_init(&kernel_cache,sampleCount,*kernel_cache_size);
        
        
        svm_learn_classification( docsTrain, target, sampleCount, nvox_masked, 
            learn_parm, kernel_parm, &kernel_cache, model );
      
      }
      fflush(stdout);
      
      addToAfniModel(&afniModel, model, learn_parm, tmp_labels, options, classCount,
          sampleCount, labels.class_list[cc], labels.class_list[dd]);

      if( options->modelWeightFile[0] ) {
        addToModelMap_bucket(&maps, &afniModel, dsetTrainArray, dsetMaskArray, 
            options->maskFile, classCount, &map_index);

        /* JL: before addToModelMap_bucket:
         * addToModelMap_brick(&afniModel, model, dsetTrain, dsetMask, dsetMaskArray,
         * options->modelWeightFile, nvox, labels.class_list[cc],labels.class_list[dd]);
         */
      }
      
      /* --- svm-light textfile ---*/
      if (options->docFile[0]) {
          sprintf( docFileName, "%s_%d_%d.1D", options->docFile, labels.class_list[cc], labels.class_list[dd] );
          write_svmLight_doc(docsTrain, sampleCount, nvox_masked, target, 
          docFileName, VERSION);
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

  /* JL Jan 2009: To be memory efficient: dsetTrain is loaded in writeModelBrik
   * which might cause memory problems for large training datasets,
   * if not freed here */
   DSET_unload( dsetTrain );

  writeModelBrik(&afniModel, options, options->modelFile, argc, argv);

  if( options->modelWeightFile[0] ) {
    writeModelMap_bucket(&maps, dsetMaskArray, dsetTrain, options->maskFile, 
        options->modelWeightFile, options, argc, argv); 
  }

  /* in the future, may want to add some flexibility, such as making the mask a 
   * subbrick in the model (but that would mix two different data types. Another 
   * possibility is to add the name of the mask as one of the model variables */

  if( options->maskFile[0] ) {
    writeModelMask(dsetMask, dsetMaskArray, options->modelFile);
  }

  if ( options->modelWeightFile[0] ) freeModelMaps(&maps);
  freeLabels(&labels);
  freeAfniModel(&afniModel);
  free(tmp_labels);
  
  EXRETURN;
}

/* JL May 2009: Implemented this funciton to support sv-regression in 3dsvm. 
 * It is very similar to train_routine() (a lot of code has been recycled). 
 * However, major differences are:
 *
 *    - No need for mulitclheader
 *    - New function to read in the labelfile ( getRegressionLabels() )
 * 
 *
 * - New function to get the array with training data ( getRegressionArray() )
 *    - Using svm-light's function call: svm_learn_regression() instead of
 *      svm_learn_classification()
 */ 
void train_regression(MODEL *model, LEARN_PARM *learn_parm, 
    KERNEL_PARM *kernel_parm, long *kernel_cache_size, ASLoptions *options,
    THD_3dim_dataset *dsetTrain,  THD_3dim_dataset *dsetMask, 
    MaskType *dsetMaskArray, int argc, char **argv)
{
  
  LABELS labels;
  AFNI_MODEL afniModel;	        /* holds everything required to write out 
                                   model.Head */
  MODEL_MAPS maps;              /* holds the maps (e.g., weight-vector maps)
                                   for the bucket */
  long map_index = 0;           /* index counting number of maps written into
                                   maps->data; map_index is updated in 
                                   addToModelMap_bucket() */ 
  long sampleCount = 0;			/* number of samples used in training */
  LabelType *target = NULL;       
  DatasetType 
    **dsetTrainArray = NULL;    /* array to hold training dataset values */
  DOC    *docsTrain = NULL;     /* SVM-Light data structure used for training */
  KERNEL_CACHE kernel_cache;
  long nt = 0;                  /* number of time points in TRAIN dataset */
  long nvox = 0;                /* number of voxels per time point in TRAIN 
                                   dataset */
  long nvox_masked = 0;
  long i,j,k;
  char alphaFile[MAX_FILE_NAME_LENGTH];	  /* naming of alphafile output */	
  char docFileName[MAX_FILE_NAME_LENGTH];  /* nameing of svm-light textfile  output */


  ENTRY("train_regression");

  if (verbosity >= 1) INFO_message("\n++ REGRESSION (training):\n++");

  
  /*----- LOAD TRAINING DATA ---*/
  dsetTrain = THD_open_one_dataset( options->trainFile );
  if ( dsetTrain == NULL ) {
    ERROR_exit("Failed to open training dataset: %s", options->trainFile );
  }
  DSET_load( dsetTrain ); 

  nt = DSET_NUM_TIMES( dsetTrain );
  nvox = DSET_NVOX( dsetTrain );
  nvox_masked = nvox; /* this will be modified later if mask used */

  /* JL Jan 2009: Quick bugfix -thanks Cameron: Datum type short is the only
   * datum type, that is supported in 3dsvm for now. We need to change how we
   * cast input data in the future.
   */
  if( DSET_BRICK_TYPE(dsetTrain,0) != MRI_short ) {
	  ERROR_exit("Sorry, datum type short is the only datum type that is supported for now!");
  }

  if(verbosity >= 1)  
    INFO_message( "Number of time samples is %ld, and voxels %ld in training "
        "dataset.",nt,nvox );


  /*------ GET MASK ARRAY, IF SELECTED AND DETECT nvox_masked ---*/
  if( options->maskFile[0] ) {
    nvox_masked = 0;
    dsetMask = THD_open_one_dataset( options->maskFile );
    if ( dsetMask == NULL ) {
      ERROR_exit("Failed to open mask file: %s", options->maskFile );
    }
    DSET_load(dsetMask);
    if( DSET_BRICK_TYPE(dsetMask,0) != MRI_byte ) {
      ERROR_exit("Mask file: %s is not a byte-format brick.\n",
          options->maskFile );
    }
    dsetMaskArray = (MaskType*)DSET_ARRAY(dsetMask,0);
    for( i=0 ; i<nvox ; ++i ) {
      if( dsetMaskArray[i] )
        nvox_masked++;
    }
    if(verbosity >= 1) 
      INFO_message( "The number of non-zero elements in mask is: %ld\n",
          nvox_masked );
  }
  else if( !(options->outModelNoMask) ){
    ERROR_exit("No mask file specified (use -mask file). "
        "If not using a mask file must use option -nomodelmask");
  }

  /*----- RETRIEVE LABELS ----*/
  target = getRegressionLabels(&labels,options->labelFile, 
      options->censorFile);

    if (labels.n != nt) 
    ERROR_exit("Number of labels %ld in labelfile '%s' does not match\n"
        "   number of timepoints %ld in train dataset '%s'!", labels.n,
        options->labelFile, nt, options->trainFile);
  
  sampleCount=labels.n-labels.n_cnsrs; /* number of uncensored timepoints */

  /*----- GET TRAINING ARRAY WITHOUT CENSORED TIMEPOINTS -----*/
  dsetTrainArray = getRegressionArray(dsetTrain, &labels);
  
  /*----- ALLOCATE afniModel -----*/
  allocateAfniModel(&afniModel, &labels, options);

  /*----- ALLOCATE maps ------*/
  allocateModelMaps(&maps, (long)labels.n_classes, nvox, options->kernelName);

  /*----- ALLOCATE DOCs & WORDs------*/
  docsTrain = (DOC*)malloc(sizeof(DOC)*sampleCount);
  AllocateDOCwords(docsTrain, sampleCount, nvox_masked);

  /*----- CONVERT TRAINING ARRAY TO SVM-LIGHT DATASTRUCTURE ------*/
  afni_dset_to_svm_doc( docsTrain, dsetTrainArray, dsetMaskArray, 
          options, sampleCount, nvox, nvox_masked );

  /*----- PERFORM THE SV-REGRESSION -----*/
  if ( !strcmp(options->kernelName, "linear") ) {

    svm_learn_regression ( docsTrain, target, sampleCount, nvox_masked,
        learn_parm, kernel_parm, NULL, model);

  }
  else {
    WARNING_message("At this time regression with kernels is untested"
        "proceed at your own risk - (and let us know how it goes)");
    
    kernel_cache_init(&kernel_cache, sampleCount, *kernel_cache_size);

    svm_learn_regression ( docsTrain, target, sampleCount, nvox_masked,
        learn_parm, kernel_parm, &kernel_cache, model);
  }
  
  /*----- UPDATE AFNI-MODEL -----*/
  addToAfniModel(&afniModel, model, learn_parm,  &(labels.cnsrs[0]), options,
      0, sampleCount*2, 0, 0);

  /*---- UPDATE MODEL-MAPS -----*/
  if( options->modelWeightFile[0] ) {
    addToModelMap_bucket(&maps, &afniModel, dsetTrainArray, dsetMaskArray,
        options->maskFile, 0, &map_index);
  }

  /*---- WRITE OUTPUT FILES TO DISC ----*/
  /* might not be neccessary if testing and training are performed all at once */

  
  /* JL Jan 2009: To be memory efficient: dsetTrain is loaded in writeModelBrik
   * which might cause memory problems for large training datasets,
   * if not freed here */
   DSET_unload( dsetTrain );

  /* --- model --- */
  writeModelBrik(&afniModel, options, options->modelFile, argc, argv);
    
  /* --- (model) mask --- */
  /* would be great to change the mask datatype to short and include the mask 
   * as a sub-brick of the model! */
  if( options->maskFile[0] ) {
    writeModelMask(dsetMask, dsetMaskArray, options->modelFile);
  }

  /* --- maps --- */
  if( options->modelWeightFile[0] ) {
    writeModelMap_bucket(&maps, dsetMaskArray, dsetTrain, options->maskFile, 
        options->modelWeightFile, options, argc, argv); 
  }

  /* --- svm-light textfile ---*/
  if (options->docFile[0]) {
    sprintf( docFileName, "%s.1D", options->docFile);
    write_svmLight_doc(docsTrain, sampleCount, nvox_masked, target, 
        docFileName, VERSION);
  }
  
  /*----- FREE MEMORY -----*/
  freeDOCwords(docsTrain, sampleCount);
  free(docsTrain);
  freeLabels(&labels);
  free(target);
  freeAfniModel(&afniModel);
  free2DT( dsetTrainArray, sampleCount );
  freeModelMaps(&maps);

  EXRETURN;
}

/* JL Sep. 2009: Error checking for options with argument. 
 * Avoid out of bound error if last option and no argument
 * ppi = ++i */
int ppi (int argc, int i, char *optionString)
{
  if ( optionString[strlen(optionString)+1] == '-' ) {
    ERROR_exit("Argument for %s must not start with '-'!\n", optionString);
  }
  else if ( i<argc-1 ) return(++i);
  else ERROR_exit("No argument after %s!", optionString);
}

int input_parse(int argc,char *argv[],long *main_verbosity,
    long *kernel_cache_size,LEARN_PARM *learn_parm, KERNEL_PARM *kernel_parm,
    ASLoptions* optionsData, enum modes *mode, int *svm_type, char *errorString)
{
  long i;
  char type[200];       /* svm-light type (REGRESSION, CLASSIFICATION or RANKING) */
  int parseFlag = 0;    /*ZSS: init to 0*/
  int aFlag = 0; 
  int alphaFlag = 0;
  int tFlag = 0;       /* JL: flag for svm-light option -t (kernel_type) */
  int zFlag = 0;       /* JL: flag for svm-light option -z (type) */
  *mode = NOTHING;

  ENTRY("input_parse");
  
  /* svmlight defaults */
  strcpy (learn_parm->predfile, "trans_predictions");
  strcpy (learn_parm->alphafile, "");
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
  kernel_parm->kernel_type=LINEAR;
  kernel_parm->poly_degree=3; 
  kernel_parm->rbf_gamma=1.0; 
  kernel_parm->coef_lin=1.0;
  kernel_parm->coef_const=1.0;
  strcpy(kernel_parm->custom,"empty");
  strcpy(type,"c");

  /* 3dsvm specific */
  strcpy(optionsData->labelFile, "");
  strcpy(optionsData->censorFile, "");
  strcpy(optionsData->trainFile, "");
  strcpy(optionsData->maskFile, "");
  strcpy(optionsData->modelFile, "");
  strcpy(optionsData->docFile, "");
  optionsData->outModelNoMask = 0;
  optionsData->noPredDetrend = 0;
  optionsData->classout = 0;
  strcpy(optionsData->multiclass, "");
  strcpy(optionsData->kernelName, "");
  strcpy(optionsData->modelAlphaFile, "");
  strcpy(optionsData->modelWeightFile, "");
  strcpy(optionsData->testFile, "");
  strcpy(optionsData->testLabelFile, "");
  strcpy(optionsData->predFile, "");
  strcpy(optionsData->svmType,""); 

  for( i=1; i<argc; ++i ) { 
    parseFlag = 0;

    if ( argv[i][0] != '-' ) { snprintf(errorString, LONG_STRING,
        "Option %s must start with '-'!", argv[i]); RETURN(1); }

    /* svm-light options: */
    if( !strcmp(argv[i],"-z") ) { parseFlag=1; i=ppi(argc,i,argv[i]); 
                                  strcpy(type,argv[i]); zFlag=1; } 
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
                                  strcpy(kernel_parm->custom,argv[i]); }
    if( !strcmp(argv[i],"-l") ) { parseFlag=1; i=ppi(argc,i,argv[i]); 
                                  strcpy(learn_parm->predfile,argv[i]); }
    /* if( !strcmp(argv[i],"-a") ) { parseFlag=1; i=ppi(argc,i,argv[i]); 
     *                               strcpy(learn_parm->alphafile,argv[i]); }
     *
     * as an easy solution, we are fixing the svmLight's output file name and 
     * letting 3dsvm write out the desired file */
         
    /* 3dsvm options with arguments: */
    if( !strcmp(argv[i],"-type") )          { parseFlag=1; i=ppi(argc,i,argv[i]); 
                                              strcpy(optionsData->svmType,argv[i]); }
    if( !strcmp(argv[i],"-a") )             { parseFlag=1; i=ppi(argc,i,argv[i]); 
                                              strcpy(optionsData->modelAlphaFile,argv[i]); aFlag=1;}
    if( !strcmp(argv[i],"-alpha") )         { parseFlag=1; i=ppi(argc,i,argv[i]); 
                                              strcpy(optionsData->modelAlphaFile,argv[i]); alphaFlag=1;}
    if( !strcmp(argv[i],"-trainvol") )      { parseFlag=1; i=ppi(argc,i,argv[i]); 
                                              strcpy(optionsData->trainFile,argv[i]); }
    if( !strcmp(argv[i],"-testvol") )       { parseFlag=1; i=ppi(argc,i,argv[i]); 
                                              strcpy(optionsData->testFile,argv[i]); }
    if( !strcmp(argv[i],"-multiclass") )    { parseFlag=1; i=ppi(argc,i,argv[i]); 
                                              strcpy(optionsData->multiclass,argv[i]); }
    if( !strcmp(argv[i],"-trainlabels") )   { parseFlag=1; i=ppi(argc,i,argv[i]); 
                                              strcpy(optionsData->labelFile,argv[i]); }
    if( !strcmp(argv[i],"-censor") )        { parseFlag=1; i=ppi(argc,i,argv[i]); 
                                              strcpy(optionsData->censorFile,argv[i]); }
    if( !strcmp(argv[i],"-mask") )          { parseFlag=1; i=ppi(argc,i,argv[i]); 
                                              strcpy(optionsData->maskFile,argv[i]); }
    if( !strcmp(argv[i],"-model") )         { parseFlag=1; i=ppi(argc,i,argv[i]); 
                                              strcpy(optionsData->modelFile,argv[i]); }
    if( !strcmp(argv[i],"-bucket") )        { parseFlag=1; i=ppi(argc,i,argv[i]); 
                                              strcpy(optionsData->modelWeightFile,argv[i]); }
    if( !strcmp(argv[i],"-testlabels") )    { parseFlag=1; i=ppi(argc,i,argv[i]); 
                                              strcpy(optionsData->testLabelFile,argv[i]); }
    if( !strcmp(argv[i],"-predictions") )   { parseFlag=1; i=ppi(argc,i,argv[i]); 
                                              strcpy(optionsData->predFile,argv[i]); }
    if( !strcmp(argv[i],"-docout") )        { parseFlag=1; i=ppi(argc,i,argv[i]); 
                                              strcpy(optionsData->docFile,argv[i]); }
    /* for kernel below, using svm-light options for kernel parameters */
    if( !strcmp(argv[i],"-kernel") )        { parseFlag=1; i=ppi(argc,i,argv[i]); 
                                              strcpy(optionsData->kernelName,argv[i]); }

    /* AFNI, 3dsvm options without arguments: */
    if( !strcmp(argv[i],"-trace")) { parseFlag = 1; 
      #ifdef USE_TRACING 
        DBG_trace = 1; 
      #endif 
    }
    if( !strcmp(argv[i],"-no_memcheck") )   { pause_mcw_malloc(); /* ZSS */ }
    if( !strcmp(argv[i],"-nomodelmask") )   { parseFlag=1; optionsData->outModelNoMask = 1; }
    if( !strcmp(argv[i],"-nodetrend") )     { parseFlag=1; optionsData->noPredDetrend = 1; }
    if( !strcmp(argv[i],"-classout") )      { parseFlag=1; optionsData->classout = 1; } 
    if( !strcmp(argv[i],"-change_summary")) { printf( change_string ); exit(0); }
    if( !strcmp(argv[i],"-help") )          
    {  
      printf( cl_helpstring ); 
      printf("\n\n-------------------- SVM-light learn help -----------------------------\n");
      print_help_learn();
      printf("\n\n-------------------- SVM-light classify help -----------------------------\n");
      print_help_classify();
      printf("\n\n--------------------------------------------------------------------------\n");
      printf( contribution_string ); 

      RETURN(0); 
    }
  }
  
  if( !parseFlag ) {
    if ( argc == 1 ) { 
      printf( cl_helpstring ); 
      printf("\n\n-------------------- SVM-light learn help -----------------------------\n");
      print_help_learn();
      printf("\n\n-------------------- SVM-light classify help -----------------------------\n");
      print_help_classify();
      printf("\n\n--------------------------------------------------------------------------\n");
      printf( contribution_string );
    }
    else snprintf(errorString, LONG_STRING, "Illegal option %s!", argv[i-1]);
    RETURN(1);
  }

  /* JL Feb. 2009: Some error checking and initialization for kernel options */
  if ( tFlag && optionsData->kernelName[0] ) {
   WARNING_message("Both svm-light option: -t and 3dsvm option: -kernel "
       "were used.\n   Using -kernel %s\n", optionsData->kernelName);
  }

  if ( optionsData->kernelName[0] ) {
    if ( !strcmp(optionsData->kernelName, "complex1") ) {
      kernel_parm->kernel_type = CUSTOM;
      strcpy(kernel_parm->custom, "complex1");
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
    strcpy(optionsData->kernelName, "linear"); /* (matches default 
                                                  for kernel_type) */

  /* JL May 2009: Some error checking and initialization for svm learning type */
  if ( (zFlag) && (optionsData->svmType[0]) ) {
   WARNING_message("Both svm-light option: -z and 3dsvm option: -type "
       "were used.\n   Using -type %s\n", optionsData->svmType);
  }
  
  if( optionsData->svmType[0] ) {
    if( !strcmp(optionsData->svmType, "classification") ) {
        learn_parm->type=CLASSIFICATION;
        *svm_type=CLASSIFICATION;
        strcpy(type,"c");
    }
    else if ( !strcmp(optionsData->svmType, "regression") ) {
      learn_parm->type=REGRESSION;
      *svm_type=REGRESSION; 
      strcpy(type,"r");
    }
    else {
      snprintf(errorString, LONG_STRING, "Unknown option -type %s!\n",
       optionsData->svmType); RETURN(1);
    }
  }
  else 
    strcpy(optionsData->svmType,"classification"); /* (matches default 
                                                  for learn_parm->type) */ 
    
  /* the following corresponds to -t option in SVM-Light's original logic */
  if(strcmp(type,"c")==0) {
    learn_parm->type=CLASSIFICATION;
    *svm_type=CLASSIFICATION;
    strcpy(optionsData->svmType, "classification");
  }
  else if(strcmp(type,"r")==0) {
    learn_parm->type=REGRESSION;
    *svm_type=REGRESSION;
    strcpy(optionsData->svmType, "regression");
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

  /* JL May. 2009: Regression only with linear kernels for now */
  if( (!strcmp(optionsData->svmType, "regression")) || 
       (!strcmp(type,"r" )) ) {

    if( (strcmp(optionsData->kernelName, "linear")) || 
        (kernel_parm->kernel_type != LINEAR) ) {
      snprintf(errorString, LONG_STRING,"-type regression is only supported with " 
          "-kernel linear!"); RETURN(1);
    }
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

  if( !(*mode == TRAIN_AND_TEST) && !(optionsData->modelFile[0]) ) { 
      snprintf(errorString, LONG_STRING, "Must specify a model file!"); RETURN (1);  
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
        
        strcpy(optionsData->modelWeightFile, "");
        }
      }
  }
  if ( !(optionsData->trainFile[0]) && (optionsData->modelWeightFile[0])) { /* JL */ 
      WARNING_message("Maps (-bucket option) only can be generated "
          "during training.");
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
    strcpy(optionsData->maskFile, "");
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

  RETURN(0);
}

