#include "3dsvm_common.h"
#include "svm_learn.c"
#include "debugtrace.h"

# define PROGRAM_NAME   "3dsvm"   /* name of this program -
                                     used to include commandline history in model */

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
  printf("\nSVM-light %s: Support Vector Machine, learning module     %s\n",
      VERSION_SVMLIGHT, VERSION_DATE_SVMLIGHT);
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
  printf("*** 3dsvm: %s (%s), SVM-Light: %s ***\n",
      VERSION_3DSVM, VERSION_DATE_3DSVM, VERSION_SVMLIGHT);
  printf("*************************************************\n\n");
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

/* JL Mar. 2009 */
void free2c(char **x, long index1)
{
  long i;

  ENTRY("free2c");

  for(i=0; i<index1; i++) free(x[i]);
  free(x);

  EXRETURN;
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


void AllocateDOCwords(DOC *docs, long ndocsTime, long nvoxelWords)
{
  long i = 0;

  ENTRY("AllocateDOCwords");

  for( i=0; i < ndocsTime; ++i ) {
    docs[i].words = (WORD*)malloc(sizeof(WORD)*(nvoxelWords+1));
  }

  EXRETURN;
}

void freeDOCwords(DOC *docs, long ndocsTime)
{

  long i = 0;

  ENTRY("freeDOCwords");

  for( i=0; i < ndocsTime; ++i )
    free(docs[i].words);

  EXRETURN;
}

void allocateModel( MODEL *model, AFNI_MODEL *afni_model)
{
  long nsv   = 0;   /* number of support vectors */
  long sv    = 0;   /* index over nsv */
  long nt    = 0;   /* number of timepoints */
  long t     = 0;   /* index over nt */


  ENTRY("allocateModel");

  /* our approach to multiclass is to keep all training timepoints 
   * with non-support vectors as alpha = 0
   * thus the model "documents" and number of support vectors is
   * always the number of timepoints in in the training data
   */

  nt=afni_model->timepoints;
  nsv=afni_model->total_support_vectors[0];

  /* JL July 2009: Added this part to support sv-regression */
  if ( !strcmp(afni_model->svm_type, "regression") ) {

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

void freeModel( MODEL *model, AFNI_MODEL *afni_model)
{
  long nsv  = 0;  /* number of support vectors */
  long sv   = 0;  /* index over nsv */
  long nt   = 0;  /* number of timepoints */
  long t    = 0;  /* index over nt */

  ENTRY("freeModel");
  
  nt=afni_model->timepoints;
  nsv=afni_model->total_support_vectors[0];

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
  strncpy(model->kernel_parm.custom, afni_model->kernel_custom[i], 50);
  model->b = (double) afni_model->b[comb];

  /* our approach to multiclass is to keep all training timepoints 
   * with non-support vectors as alpha = 0
   * thus the model "documents" and number of support vectors is
   * always the number of timepoints in in the training data
   *
   * JL July 2009: For sv-regression (and testing only!) the number of support 
   * vectors is the number of non-zero alphas and only non-zero alphas are
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
  else { /* before sv-regression */
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
            if( noMaskFlag ) { /* no mask */
              (model->supvec[sv])->words[vmsk].wnum = vmsk + 1;
              (model->supvec[sv])->words[vmsk].weight = 
                (float)dsetModelArray[th][v];
            
              ++vmsk;
            }
            else if( dsetMaskArray[v] ) { /* mask */
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

  EXRETURN;

}


void readAllocateAfniModel( THD_3dim_dataset *dsetModel,  AFNI_MODEL *afniModel)
{
  ATR_float *atr_float   = NULL;
  ATR_int *atr_int       = NULL;
  ATR_string *atr_string = NULL;
  long i,j,c             = 0;
  int cc, dd             = 0;


  /* used for strtok magic */
  long p_string_size     = 0;    /* string size p, dependent on the
                                  number of class combinations */
  char *p                = NULL;
  char *q                = NULL;
  char headernames[LONG_STRING];

  long max_comb = CLASS_MAX*(CLASS_MAX-1)/2;


  ENTRY("readAllocateAfniModel");


  /* JL Oct 2009: The naming and number of model parameters in the
   * model header has changed. We added "3DSVM" in front of each parameter name
   * to avoid collisions with header entries from other afni programs.
   *
   * Trying to be backwards compatible:
   * Assuming old (before Oct. 2009) naming for model parameters
   * if "3DSVM_SVM_TYPE" is not present in the header.
   *
   * JL Apr. 2010: Allocating p string (for strtok) dynamically
   * Replaced all string functions by its equivalent that also takes the
   * string size as an argument
   *
   * JL Apr. 2010: Allocating combNames and kernel_custome dynamically
   * based on CLASS_MAX and CSV_STRING
   *
   * TODO: Error checking for each model parameter would be good
   *
   */

  /* ---- retrieve class-combinations from header ---- */
  atr_int = THD_find_int_atr( dsetModel->dblk, "3DSVM_CLASS_COMBINATIONS" );
  if (atr_int == NULL) {

     /* maybe old naming */
     atr_int = THD_find_int_atr( dsetModel->dblk, "CLASS_COMBINATIONS" );
     if (atr_int == NULL) {
       ERROR_exit("Can not read model header. Did you modify the model header?");
     }
  }

  /* --- allocate CSV strings --- */
  p_string_size = atr_int->in[0]*CSV_STRING;

  if ( (p = (char *)malloc(p_string_size*sizeof(char))) == NULL ) {
    ERROR_exit("Memory allocation for csv-string in readAllocateAfniModel failed!");
   }

  afniModel->combName = Allocate2c(max_comb, (long)CSV_STRING);
  afniModel->kernel_custom = Allocate2c(max_comb, (long)CSV_STRING);
  if (afniModel->combName == NULL  || afniModel->kernel_custom == NULL ) {
    ERROR_exit("Memory allocation for csv-string in readAllocateAfniModel failed!");
  }

  Clear2c(afniModel->combName, max_comb);
  Clear2c(afniModel->kernel_custom, max_comb);

  /* ---- read header information ---- */
  /* distinguishing between old and new parameter naming based on
   * 3DSVM_SVM_TYPE. If present: new naming, if not: old naming */

  atr_string = THD_find_string_atr( dsetModel->dblk, "3DSVM_SVM_TYPE" );

  /* ---- naming for model parameters before Oct. 2009 ---*/
  if( atr_string == NULL ) {
    WARNING_message("Can not find 3DSVM_SVM_TYPE in model header! "
        "You must have used an older version for training!");
    strncpy(afniModel->svm_type, "classification", LONG_STRING);

    atr_int = THD_find_int_atr( dsetModel->dblk, "CLASS_COMBINATIONS" );
    afniModel->combinations = *atr_int->in;

    q = strtok(p,",");
    if (q != NULL) strncpy(afniModel->combName[0], q, CSV_STRING);
    else {
      ERROR_exit("Reading model combinations in header file failed");
    }
    for(i = 1; i < afniModel->combinations; ++i) {
      q=strtok(NULL, ",");
      if (q != NULL) strncpy(afniModel->combName[i], q, CSV_STRING);
      else {
        ERROR_exit("Reading model combinations in header file failed\n"
            "   Number does not match expected(%d)", afniModel->combinations);
      }
    }

    atr_int = THD_find_int_atr( dsetModel->dblk, "CLASS_COUNT" );
    afniModel->class_count = *atr_int->in;

    atr_int = THD_find_int_atr( dsetModel->dblk, "TIMEPOINTS" );
    afniModel->timepoints = *atr_int->in;

  
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
        strncpy(p,atr_string->ch, p_string_size);
        q = strtok(p,",");
        if (q != NULL) strncpy(afniModel->kernel_custom[0], q, CSV_STRING);
        else ERROR_exit("Can't find KERNEL_CUSTOM in header file");
        
        for ( i=1; i<afniModel->combinations; ++i) {
          q=strtok(NULL,",");
          if (q != NULL) strncpy(afniModel->kernel_custom[i], q, p_string_size);
          else {
            ERROR_exit("Reading KERNEL_CUSTOM in header file number of class-"
                "combinations does not match expected(%d)\n", 
                afniModel->combinations);
          }
        }
      }
      else {
        for ( i=1; i<afniModel->combinations; ++i) {
          strncpy(afniModel->kernel_custom[i], "empty", CSV_STRING);
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
  
      
    afniModel->alphas  = Allocate2f((long) afniModel->combinations,
                                    (long) afniModel->timepoints);
    afniModel->cAlphas = Allocate2f((long) afniModel->combinations,
                                    (long) afniModel->timepoints);
    for(i = 0; i < afniModel->combinations; ++i ) {
      snprintf(headernames, LONG_STRING, "ALPHAS_%s", afniModel->combName[i]);
      atr_float = THD_find_float_atr( dsetModel->dblk, headernames); 
      for(j = 0; j < afniModel->timepoints; ++j ) {
        afniModel->alphas[i][j] = (double)atr_float->fl[j];
      }
    }
  }

  /* --- naming for model parameters (Oct. 2009) --- */
  else {
    strncpy(afniModel->svm_type, atr_string->ch, LONG_STRING);
  
    atr_int = THD_find_int_atr( dsetModel->dblk, "3DSVM_CLASS_COUNT" );
    afniModel->class_count = *atr_int->in;

    atr_int = THD_find_int_atr( dsetModel->dblk, "3DSVM_CLASS_COMBINATIONS" );
    afniModel->combinations = *atr_int->in;

    atr_int = THD_find_int_atr( dsetModel->dblk, "3DSVM_TIMEPOINTS" );
    afniModel->timepoints = *atr_int->in;

    atr_string = THD_find_string_atr( dsetModel->dblk, "3DSVM_COMBO_NAMES" );
    strncpy(p, atr_string->ch, p_string_size);
    q = strtok(p,",");
    if (q != NULL) strncpy(afniModel->combName[0], q, CSV_STRING);
    else {
      ERROR_exit("Reading model combinations in header file failed");
    }
    for(i = 1; i < afniModel->combinations; ++i) {
      q=strtok(NULL,",");

      if (q != NULL) strncpy(afniModel->combName[i], q, CSV_STRING);
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
    strncpy(p, atr_string->ch, p_string_size);
    q = strtok(p,",");
    if (q != NULL) strncpy(afniModel->kernel_custom[0],q, CSV_STRING);
    else ERROR_exit("Can't find KERNEL_CUSTOM in model header file");
    
    for ( i=1; i<afniModel->combinations; ++i) {
      q=strtok(NULL,",");
      if (q != NULL) strncpy(afniModel->kernel_custom[i], q, CSV_STRING);
      else {
        ERROR_exit("Reading KERNEL_CUSTOM in model header file number of class"
            "combinations does not match expected(%d)", afniModel->combinations);
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
      snprintf(headernames,LONG_STRING, "3DSVM_ALPHAS_%s",afniModel->combName[i]);
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
  
  /* --- free p string used for strtok ---*/
  free(p);

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
      else strncat(options->modelFile,"+orig", LONG_STRING);
    }

    else if ( strstr(options->trainFile,"+tlrc") != NULL ) {
      if ( strstr(options->testFile,"+tlrc") == NULL ) {
        ERROR_exit("Viewtype of train dataset: %s does not match\n"
            "   Viewtype of test dataset: %s!", options->trainFile,
            options->testFile);
      }
      else strncat(options->modelFile,"+tlrc", LONG_STRING);
    }

    else if ( strstr(options->trainFile,"+acpc") != NULL ) {
      if ( strstr(options->testFile,"+acpc") == NULL ) {
        ERROR_exit("Viewtype of train dataset: %s does not match\n"
            "   Viewtype of test dataset: %s!", options->trainFile,
            options->testFile);
      }
      else strncat(options->modelFile,"+acpc", LONG_STRING);
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

  /* TODO: would be great to pass dsetModel to the test function and avoid
   * reading it in twice */

  EXRETURN;
}

allocateModelMaps(MODEL_MAPS *maps, long n_classes, long n_vox, char *kernelName)
{
  long i = 0;
  long class_comb = 0;
    
  ENTRY("allocateModelMaps");

  /* --- initialize --- */
  class_comb = n_classes*(n_classes-1)/2;
  maps->index=0;

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
   * JL Aug 2009: Added regression maps
   * JL Apr 2010: Incorporated map_index into the MPDEL_MAPS structure*/

  /* --- initialization ---*/
  iMap=maps->index; /* prone for errors, should do something better than that */
  
  /* --- calculate weight-vector map for regression --- */
  if( !strcmp(afni_model->svm_type, "regression")) {  
    nt = afni_model->total_samples[cc];

    if ( nt%2 != 0 ) { /* I'm being redundant... */
      ERROR_exit("Adding to model map failed for sv-regression"
          "The number of time-points (samples) is not a multiple of 2!");
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
    snprintf(maps->names[iMap], LONG_STRING, "RegresMapRea_%s", afni_model->combName[cc]);
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
      snprintf(maps->names[iMap], LONG_STRING, "RealWvMapRea_%s", afni_model->combName[cc]);
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
      snprintf(maps->names[iMap+4], LONG_STRING, "CpxWvMapMag2_%s", afni_model->combName[cc]);
      snprintf(maps->names[iMap+5], LONG_STRING, "CpxWvMapPha2_%s", afni_model->combName[cc]);
      iMap=iMap+6;
    }
  }
  
  maps->index=iMap;

  EXRETURN;
}

void writeModelMap_bucket ( MODEL_MAPS *maps, MaskType *dsetMaskArray, 
    THD_3dim_dataset *dsetTrain,  char *maskFile, char *modelFile, 
    float *b, long combinations, ASLoptions* options, int argc, char **argv)
{
  long v     = 0;
  long t     = 0;
  long iMap  = 0;
  long nx    = 0;
  long ny    = 0;
  long nz    = 0;
  long nx_ny = 0;

  THD_ivec3 iv_nxyz; 
  int ierror  = 0;
  int ityp    = 0;

  THD_3dim_dataset*  dsetModelMapBucket = NULL;
  float*              scaled_map        = NULL;
    

  ENTRY("writeModelMap_bucket");

  /* Changes:
   * JL Apr. 2010: Writing VERSION_3DSVM and VERSION_3DSVM into the header
   * JL Apr. 2010: Writing B value into the bucket
   */


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
                            ADN_datum_all,       MRI_float,
                            ADN_ntt,             0,   /* no time axis */
                            ADN_nvals,           maps->nmaps,
                            ADN_nxyz,            iv_nxyz, 
                            ADN_malloc_type,     DATABLOCK_MEM_MALLOC ,
                            ADN_none ) ;

  if( ierror > 0 ) {
    ERROR_exit("%d errors in attempting to create bucket dataset!", ierror );
  }

 /* --- scale and write maps into bucket --- */
  for (iMap=0; iMap<maps->nmaps; ++iMap) {
    
    /* -- allocate scaled_map  -- */
    scaled_map = (float *) malloc(sizeof(float)*maps->nvox);
    if (scaled_map == NULL) {
      ERROR_exit("Memory allocation in writeModelMap failed!");
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
    EDIT_substitute_brick( dsetModelMapBucket, iMap, MRI_float, scaled_map);
    EDIT_BRICK_LABEL( dsetModelMapBucket, iMap, maps->names[iMap]);
  }

  /* --- add information to the header --- */
  THD_set_string_atr( dsetModelMapBucket->dblk, "3DSVM_VERSION", VERSION_3DSVM);
  THD_set_string_atr( dsetModelMapBucket->dblk, "3DSVM_VERSION_DATE", VERSION_DATE_3DSVM);
  THD_set_float_atr( dsetModelMapBucket->dblk, "3DSVM_B", combinations, b );

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
  INFO_message("Writing model dataset mask...");
  THD_write_3dim_dataset( "./", maskCopyName, dsetMaskCopy, True );

  EXRETURN;
}

/*-----------------------------------------------------------*/
void writeModelBrik(AFNI_MODEL *afniModel, ASLoptions *options, char *fileName,
		int argc, char **argv)
{

  THD_3dim_dataset *dsetModel = NULL;
  char* csv_combName          = NULL;  /* comma separated  "names" of class
                                        * category combinations */
  char* csv_kernelCustom      = NULL;

  char headernames[LONG_STRING];       /* comma separated "names" for each alpha
                                        *  set */
  long csv_string_size        = 0;     /* size of csv strings, dependent on number
                                          of class-combinations */
  char * commandline          = NULL;  /* for history */
  long i                      = 0;


  ENTRY("writeModelBrik");
 

  /* JL Oct. 2009: The naming and the number of parameters written into the
   * model header has changed. Now, we are writting all svm parameters 
   * (that can be specified using the command-line) into the header. 
   * We also added  "3DSVM" in front of each parameter name to avoid 
   * collisions with header entries from other afni programs.
   * Trying to be backwards compatible.
   *
   * JL July 2009: EDIT_full_copy was copying brick 0 into brick n.
   * Replaced it by THD_open_one_dataset
   *
   * JL Apr. 2010: Changed allocation of strings holding comma separated values
   * to be dynamic.  Replaced all string functions by its equivalent that also
   * takes the string size as an argument.
   *
   */


  dsetModel = THD_open_one_dataset( options->trainFile );
  if ( dsetModel == NULL ) {
    ERROR_exit("Failed to open training dataset for copying: %s", options->trainFile );
    }
  DSET_load( dsetModel );

  /* JL Sep 2009: Assigning new id-code to avoid problems with
   * duplicated id-codes */
  dsetModel->idcode=MCW_new_idcode();


  /* ---- allocating strings holding csv ---- */
  csv_string_size = afniModel->combinations*CSV_STRING;
  if ( (csv_combName = (char *) malloc(csv_string_size * sizeof(char))) == NULL ) {
    ERROR_exit("Could not allocate csv-string in writeModelBrik!");

  }
  if ( (csv_kernelCustom = (char *) malloc(csv_string_size * sizeof(char))) == NULL ) {
    ERROR_exit("Could not allocate csv-string in writeModelBrik!");
  }

  /* ---- record history of dataset ---- */
  commandline = tross_commandline(PROGRAM_NAME, argc, argv);
  if (commandline == NULL) {
    WARNING_message("Can not copy command-line into model header!");
  }
  else tross_Append_History (dsetModel, commandline);
  free(commandline);

  /* ---- write header ---- */
  strncpy(csv_combName, afniModel->combName[0], csv_string_size);
  strncpy(csv_kernelCustom, afniModel->kernel_custom[0], csv_string_size);

  for(i = 1; i < afniModel->combinations; ++i) {
    strncat(csv_combName, ",", csv_string_size);
    strncat(csv_combName, afniModel->combName[i], csv_string_size);
    strncat(csv_kernelCustom, ",", csv_string_size);
    strncat(csv_kernelCustom, afniModel->kernel_custom[i], csv_string_size);
  }


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
    THD_set_float_atr( dsetModel->dblk, headernames, afniModel->timepoints, afniModel->alphas[i] );
  }
  
  /* --- write brick --- */
  fflush(stdout);
  INFO_message( "Writing model dataset..." );
  THD_write_3dim_dataset( "./", fileName, dsetModel, True );

  /* --- free csv strings ---*/
  free(csv_combName);
  free(csv_kernelCustom);

  EXRETURN;
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
    
  if ( !strncmp(options->svmType, "regression", LONG_STRING) ) {
    /* Should always be a multiple of two for
     * sv-regression. Just a redundant check here. */
    if ( nt%2 != 0 ) {
      ERROR_exit("Adding to afni model failed for sv-regression"
          "The number of timepoints (samples) is not a mulitple of 2!");
    }
     nth=nt/2;
  } 
  
  /* --- determine timorder of alphas --- */ 
  /* -- for sv-regression -- */
   if ( !strncmp(options->svmType, "regression", LONG_STRING) ) {
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

DatasetType** getAllocateDsetArray(THD_3dim_dataset *dset)
{
  long  v         = 0;    /* index over nvox */
  long  t         = 0;    /* index over nt */
  long  nt        = 0;    /* number of observations (time-points) total */
  long  nvox      = 0;    /* number of voxels */
  int   datum;            /* datum type */

  DatasetType**
    dsetArray     = NULL;

  ENTRY("getAllocateDsetArray");

  /* --- initialize and allocate ---*/
  nvox = DSET_NVOX( dset );
  nt = DSET_NUM_TIMES( dset );
  dsetArray = Allocate2DT( nt, nvox);

  /* ---  make sure all bricks have same datum --- */
  if ( !DSET_datum_constant(dset) ) {
    ERROR_exit("Training dataset has sub-bricks with different types");
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
       ERROR_exit("Sorry, datum-type MRI_byte (%d) is not supported!", datum);
       break;
     case MRI_rgb:
       ERROR_exit("Sorry, datum-type MRI_rgb (%d) is not supported!", datum);
       break;
     case MRI_complex:
       ERROR_exit("Sorry, datum-type MRI_complex (%d) is not supported!", datum);
       break;
     default:
       ERROR_exit("Unknown datum-type (%d)", datum);
       break;
  }

  RETURN(dsetArray);
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


  ENTRY("getAllocateRegressionArray");

  nt = labels->n;
  ntc = labels->n_cnsrs;

  /* allocate */
  dsetArrayCensored = Allocate2DT(nt-ntc, nvox);

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
/* JL Apr. 2010: This function was previously named getTmpLabels. Changed name
 * to reflect that time-points which do no belong to the current class-combination are
 * censored (labeled with 9999).
 *
 */
void getCensoredClassTarget(LabelType *censoredTarget, long *sampleCount,
    LABELS *labels, long classIndex0, long classIndex1)
{
  long i = 0;
  short labelWarningFlag = 0;  /* warn users if unknown class label
                                  - probably from multi-class */

  int class0 = labels->class_list[classIndex0];
  int class1 = labels->class_list[classIndex1];

  /* Changes:
   * JL Apr. 2010: Fixed a bug for calculation of prediction accuracies in case of
   * non-continues true-labels
   *
   */

 /* printf("DBG: class0 = %d, class1 = %d, classIndex0 = %ld, classIndex1 = %ld\n",
      class0, class1, classIndex0, classIndex1); */


  ENTRY("getCensoredClassTarget");

  /* JL Apr. 2010: Bugfix: This only applies for testing!
   * (Calculation of prediction accuracies for individual binary classifiers)
   *
   * If labels are not continues and we don't know the "true" label
   * for either classIndex0 or classIndex1, we still want to know how well
   * classifier (classIndex0_classIndex1) predicts (i.e., let's say we have
   * classifier (0_1), and we only know the "true" label for class 1, we still
   * want to know how well classifier (0_1) predicts class 1 (even though we
   * have no information about class 0).
   */

  *sampleCount = 0;

  if (class1 == 9999.0) { /* only one class (testing only) */
    if (class0 != classIndex0) {
      if (class0 != classIndex1) {
        for(i=0; i<labels->n; ++i) {
          censoredTarget[i] = 9999.0;
        }

        EXRETURN;
      }
      else {
        class1 = class0;
        class0 = 9999;
      }
    }
  }

  /* JL. Apr. 2010: Bugfix: This only applies to testing!
   * (Calculation of prediction accuracies for individual binary classifiers)
   *
   * If labels are not continues and we don't know the "true" label
   * for classIndex0 and classIndex1, classifier (classIndex0_classIndex1)
   * should give 0% accuracy (i.e., let's say we have trained with class
   * 0, 1, 2, 3 and we only know the "true" label for class 0 and 1,
   * classifier (2_3) should give accuracy 0%) */

  if( (class0 != classIndex0) && (class1 != classIndex1) ) {

    for(i=0; i<labels->n; ++i) {
      censoredTarget[i] = 9999.0;
    }

    EXRETURN;
  }

  if(verbosity >= 2) printf("++ ");

  for( i=0 ; i<labels->n ; ++i) {   /* convert timeseries input to one that
                                       can be used with svm light. */
    if( (int)(labels->cnsrs[i]) ) {
      if( (labels->lbls[i] == class0) && (labels->lbls[i] != 9999) ) { /*class a */
        censoredTarget[i] = -1.0;
        (*sampleCount)++;
      }
      else if( (labels->lbls[i] == class1) && (labels->lbls[i] != 9999) ) { /* class b */
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

    /*printf("** DBG: label[%ld] = %ld, censoredTarget[%ld] = %5.1f, class0 = %d, "
        "class1 = %d, censor[%ld] = %ld, sampleCount = %ld\n",
        i, lround(labels->lbls[i]), i, censoredTarget[i], class0, class1,
        i, lround(labels->cnsrs[i]), *sampleCount); */

  }
  if( labelWarningFlag && (verbosity >= 1) ) {
    INFO_message("Time points ignored. If not using multi-class, check for bad labels.");
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


  ENTRY("getTmpLabels");

  
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

  
  EXRETURN;

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
  free2f(afniModel->alphas,afniModel->combinations);
  free2f(afniModel->cAlphas,afniModel->combinations);

  /* JL Apr. 2010 */
  free2c(afniModel->combName, max_comb);
  free2c(afniModel->kernel_custom, max_comb);
  
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
  long max_comb = CLASS_MAX*(CLASS_MAX-1)/2;


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
  
  /* JL Apr 2010: */
  afniModel->combName = Allocate2c(max_comb, (long)CSV_STRING);
  afniModel->kernel_custom = Allocate2c(max_comb, (long)CSV_STRING);
  Clear2c(afniModel->combName, max_comb);
  Clear2c(afniModel->kernel_custom, max_comb);
  
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
    afniModel->xa_depth == NULL ||

    /* JL Apr 2010: */
    afniModel->combName == NULL ||
    afniModel->kernel_custom == NULL ) {

    ERROR_exit("Memory allocation in allocateAfniModel failed! "
        "Could not allocate afniModel members.\n");
  }
 
  EXRETURN;

}

/* JL: Apr. 2010: This functions makes the class_list continues (setting
 * non-existing classes to 9999) to make sure prediction accuracies are
 * calculated correctly */
void checkTestLabels(LABELS *labels, char* labelFile, int n_classes_model) {

  int cl, cm            = 0;
  int *tmp_class_list  = NULL;
  int class_recognized = 0;

  ENTRY("checkTestLabels");

  tmp_class_list = (int *)malloc(sizeof(int)*labels->n_classes);
  if (tmp_class_list == NULL) {
    ERROR_exit("Memory allocation in checkTestLabels failed!");
  }

  for (cl=0; cl<labels->n_classes; ++cl) {
    tmp_class_list[cl]=labels->class_list[cl];
  }

  class_recognized=0;
  for (cm=0; cm<n_classes_model; ++cm) {
    for (cl=0; cl<labels->n_classes; ++cl) {
      if (cm == tmp_class_list[cl]) {
        labels->class_list[cm] = cm;
        class_recognized++;
        break;
      }
      else labels->class_list[cm] = 9999;
    }
  }

  if (class_recognized < labels->n_classes) {
    WARNING_message("Some (or all) of the classes in labelfile: '%s' can not be classified\n"
        "   because no classifier was trained", labelFile);
  }

  free(tmp_class_list);

  EXRETURN;
}

/* JL: Apr. 2010: This functions checks if train labels are continues
 * starting with class label 0*/
void checkTrainLabels(LABELS *labels, char* labelFile ) {

  int nl    = 0;      /* number of labels */
  int l     = 0;      /* index over nl */
  int nc    = 0;      /* number of classes */
  int c     = 0;      /* index over nc */

  ENTRY("checkTrainLabels");

  nl=labels->n;
  nc=labels->n_classes;

  for (c=0; c<nc; ++c) {
    for (l=0; l<nl; ++l) {
      if (labels->lbls[l] == c) break;
      if ( (l == nl-1) && (labels->lbls[l] != c) ) {
        ERROR_exit("For training, labelfile: '%s' must have continues class labels "
            "(i.e., 0, 1, 2, ...)!", labelFile);
      }
    }
  }

  EXRETURN;
}
  
void freeLabels(LABELS *labels) {
  ENTRY("freeLabels");

  free(labels->lbls);
  free(labels->cnsrs);
  free(labels->class_list);
  
  EXRETURN;
}
   
void getLabels(LABELS *labels, char *labelFile, char *censorFile)
{
  FILE *fp = NULL;
  int class_exists_flag = 0;
  int nine_exists_flag = 0;
  long i,j,k = 0;
  char labelString[LONG_STRING];
  int strLength = 0;

  ENTRY("getLabels");

  /* Changes:
   * JL April 2010: Added checking for empty lines in  labels
   *
   */


   /*----- RETRIEVE LABEL FILE --------------*/
  if( (fp = fopen(labelFile,"r")) == NULL ) {
    ERROR_exit("Could not open .1D label file: %s",labelFile);
  }
  
  labels->n = getFileSize(labelFile);

  /* --- allocate --- */
  labels->lbls = (LabelType*)malloc(sizeof(LabelType)*labels->n);
  if( labels->lbls == NULL ) {
    ERROR_exit("Memory allocation in getLabels failed! Could not allocate labels.");
  }

  labels->class_list = (int *)malloc(sizeof(int)*CLASS_MAX);
  if (labels->class_list == NULL) {
    ERROR_exit("Memory allocation in getLabels failed! Could not allocate class list.");
  }

  /* --- read labels from file --- */
  for(i = 0; i < labels->n; i++) {
    fgets(labelString, LONG_STRING, fp);

    if ( (strLength = strlen(labelString)) == 1 ) {
        ERROR_exit("Labelfile: '%s' contains empty entry in line %ld!",
            labelFile, i+1);
      }
      else labels->lbls[i] = (LabelType) atof(labelString);
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
     ERROR_exit("Memory allocation in getLabels failed! Could not allocate censors.");
   }

  for(i=0; i<labels->n; ++i) {
    labels->cnsrs[i] = 1.0;
  }

  if( censorFile[0] ) {
    labels->n_cnsrs = 0;

    if( (fp = fopen(censorFile,"r")) == NULL ) {
      ERROR_exit("Could not open .1D censor file: %s",censorFile);
    }
    if (nine_exists_flag == 1) {
      ERROR_exit("Labelfile '%s' contains censor information and\n"
        "   censorfile '%s' was specified as well. "
        "Please use either censor or labelfile\n", labelFile, censorFile);
    }

    i=0;
    while( !feof(fp) ) {
      if( i<labels->n ) {
        fgets(labelString, LONG_STRING, fp);

        if ( (strLength = strlen(labelString)) == 1 ) {
          ERROR_exit("Censorfile: '%s' contains empty entry in line %ld!",
                    censorFile, i+1);
         }
         else labels->cnsrs[i] = (LabelType) atof(labelString);
      }
      else  {
        ERROR_exit("Censorfile (%s) is longer than expected length( %ld )",
            censorFile, labels->n);
      }
      if ( (int)labels->cnsrs[i] == 1 ) {;}
      else if ( (int)labels->cnsrs[i] == 0) {labels->n_cnsrs++;}
      else {
        ERROR_exit("Consorfile: '%s' contains invalid entry in line %d",
            censorFile, i+1);
      }
      ++i;
    }
   fclose(fp);
  }

  fflush(stdout);
}

/* JL May 2009: This function may duplicate getLabels a bit, but for regression
 * a few things can be simplified:
 *
 *  - we are only supporting censoring with a separate censor file (not 9999s)
 *  - we don't have to worry about multi-class, 
 *
 */
LabelType* getAllocateRegressionLabels(LABELS *labels, char *labelFile, char *censorFile)
{
  FILE *fp          = NULL;
  long i, j         = 0;
  LabelType *target = NULL;
  long n9999        = 0;
  char labelString[LONG_STRING];
  int strLength = 0;
  
  ENTRY("getAllocateRegressionLabels");
 
  /*--- open labelfile ---*/
  if( (fp = fopen(labelFile, "r") ) == NULL ) {
    ERROR_exit("Could not open .1D label file: %s",labelFile);
  }

  /*--- initialize ---*/
  labels->n = getFileSize(labelFile);
  labels->n_cnsrs = 0;

  /* -- allocate lbls -- */
  labels->lbls = (LabelType*)malloc(sizeof(LabelType)*labels->n);
  if( labels->lbls == NULL ) {
    ERROR_exit("Memory allocation in getAllocateRegressionLabels failed! Could not allocate labels.");
  }

  labels->class_list = (int *)malloc(sizeof(int)*CLASS_MAX);
    if (labels->class_list == NULL) {
      ERROR_exit("Memory allocation in getLabels failed! Could not allocate class list.");
    }

 /* to be able to use existing auxiliary functions: */
  labels->n_classes = 2;
  for( j=0 ; j<CLASS_MAX ; ++j ) {
    labels->class_list[j] = 9999;
  }

  /*--- read labelfile ---*/
  for(i=0; i<labels->n; i++) {
    fgets(labelString, LONG_STRING, fp);
    if ( (strLength = strlen(labelString)) == 1 ) {
      ERROR_exit("Labelfile: '%s' contains empty entry in line %ld!",
          labelFile, i+1);
    }
    else labels->lbls[i] = (LabelType) atof(labelString);
  }
  fclose(fp);

  /*--- allocate cnsrs ---*/
  labels->cnsrs = (LabelType*)malloc(sizeof(LabelType)*labels->n);
  if( labels->cnsrs == NULL ) {
    ERROR_exit("Memory allocation in getAllocateRegressionLabels failed! Could not allocate censors.");
  }

  for(i=0; i<labels->n; ++i) labels->cnsrs[i] = 1.0;

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
        fgets(labelString, LONG_STRING, fp);
        if ( (strLength = strlen(labelString)) == 1 ) {
          ERROR_exit("Censorfile: '%s' contains empty entry in line %ld!",
              censorFile, i+1);
        }
        else labels->cnsrs[i] = (LabelType) atof(labelString);

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

  /*--- allocate target ---*/
  target = (LabelType *) malloc( (labels->n-labels->n_cnsrs)*sizeof(LabelType) );
  if( labels->cnsrs == NULL ) {
    ERROR_exit("Memory allocation in getAllocateRegressionLabels failed! Could not allocate target.");
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


void test_routine (ASLoptions *options, MODEL *model, AFNI_MODEL *afniModel, 
    THD_3dim_dataset *dsetTest, THD_3dim_dataset *dsetMask, 
    THD_3dim_dataset *dsetModel, int argc, char **argv)
{

  DOC* docsTest           = NULL; /* svm-light datastructure used for testing */

  DatasetType**
    dsetTestArray         = NULL; /* array to hold test dataset values */

  DatasetType**
    dsetModelArray        = NULL; /* array to hold model dataset values */

  MaskType
    *dsetMaskArray        = NULL; /* array to hold mask datset */

  long nt                 = 0;    /* number of time points in TEST dataset */
  long nvox               = 0;    /* number of voxels per time point in TEST dataset */
  long nt_mod             = 0;    /* number of time points in  model dataset */
  long nvox_mod           = 0;    /* number of voxels per time point in MODEL dataset */

  float dist_tmp          = 0;
  float *dist             = NULL;  /* really want this to be double, but am
                                    * detrending - should do something smarter soon!*/
  float *dist_cnsrs       = NULL;
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
  long combCount            = 0;   /* temporary variable to determine cc,dd */

  float correct             = 0.0;
  float incorrect           = 0.0;
  float no_accuracy         = 0.0;

  long res_a                = 0;
  long res_b                = 0;
  long res_c                = 0;
  long res_d                = 0;

  int DAG                   = 0;   /* abbreviation for Directed Acyclic Graph:
                                    * index variable for traversing multiclass_dist */
  short edgeFlag            = 0;   /* DAG related */
  int classAssignment       = 0;   /* multi-class related */
  float *classCorrect       = NULL;
  float *classIncorrect     = NULL;

  long *nClass              = NULL;
  int *classVote            = NULL; /* mulit-class vote */
  int currentComb           = 0;
  int class0, class1        = 0;
  int winningCount          = 0;    /* mulit-class vote */
  long classCountMax        = 0;    /* maximum number of classes, needed to for
                                     * allocation of multi-class arrays */

  enum mctypes { MCTYPE_DAG,
    MCTYPE_VOTE };                  /* types for multiclass */
  enum mctypes mctype       = MCTYPE_DAG; /* default value */

  /* labels: */
  LABELS testLabels;
  LabelType *censoredTargets= NULL; /* contains labels in svm-light readable
    * format. Here it is only used to calculate prediction accuracies and is
    * updated for class combination (named tmp_Labels previously)*/

  /* used for strtok magic and csv strings: */
  char* p                   = NULL;
  char* q                   = NULL;
  long p_string_size        = 0;    /* size of p string, dependent on number of
                                     * number of class-combinations */

  /* used for modelfile naming: */
  char* inModelFile          = NULL;
  char  inModelFileMask[LONG_STRING];
  char* inModelFileMaskExt   = MODEL_MSK_EXT;

  /* etc: */
  FILE *fp                   = NULL;
  long i,j,c                 = 0;
  char predictionsFile[LONG_STRING];


  ENTRY("test_routine");


  /* JL Apr. 2010: Allocating p string (for strtok) dynamically.
   * Replaced all string functions by its equivalent that takes takes the
   * string size as an argument
   *
   * JL Apr. 2010: Initialized all variables
   *
   */

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

  /*----- GET TEST LABELS ------- */
  if( options->testLabelFile[0] ) {

    /* JL: included censor-file for testing in getLabels */
    getLabels(&testLabels, options->testLabelFile, options->censorFile);     
    if(testLabels.n != nt) {
        ERROR_exit("Number of labels do not match the length of the test dataset:\n"
	    "   labelfile '%s' contains %ld labels, but the \n"
	    "   testvolume '%s' contains %ld brick(s). ",options->testLabelFile,
        testLabels.n, options->testFile, nt);
    }

    /*----- ALLOCATE censoredTargets ---- */
    censoredTargets = (LabelType*)malloc(sizeof(LabelType)*testLabels.n);
    if( censoredTargets == NULL ) {
      ERROR_exit("Memory allocation in test_routine! Could not allocate temporary labels.");
    }
  }

  /*----- PRODUCE TEST DATA ARRAY -------*/
  dsetTestArray = getAllocateDsetArray(dsetTest);

  /*----- LOAD AFNI-SVM MODEL --------*/
  dsetModel = THD_open_one_dataset( options->modelFile );
  DSET_load( dsetModel );
  nt_mod = DSET_NUM_TIMES( dsetModel );
  nvox_mod = DSET_NVOX( dsetModel );

  /*----- PRODUCE MODEL ARRAY ------------------*/
  dsetModelArray = getAllocateDsetArray(dsetModel);

  /*----- LOAD MODEL MASK --------*/
  /* would be great to change the mask datatype to short and include it
   * as a sub-brick of the model */
  
  if( !(options->outModelNoMask) ) {
    /* ---- determine file name for model-mask (JL) ---- */ 
    inModelFile = DSET_PREFIX( dsetModel );
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
      ERROR_exit("Viewtype of model: %s unknown!", options->modelFile);
    }
    
    /*----- OPEN MASK DATASET ---- */
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

  /*----- FILL DOC STRUCTURE FROM TEST DATASET -----*/
  docsTest = (DOC*)malloc(sizeof(DOC)*nt); /* svm-light data structure */
  AllocateDOCwords(docsTest, nt, afniModel->total_masked_features[0]);

  /* assuming total_masked_features are all the same and same mask for training
   * and testing */
  afni_dset_to_svm_doc( docsTest, dsetTestArray, dsetMaskArray, options, nt,
      nvox, afniModel->total_masked_features[0]);

  allocateModel( model, afniModel);
  get_svm_model(model, dsetModelArray, dsetMaskArray, afniModel, nvox_mod,
      options->outModelNoMask);

  /*----- ALLOCATE TEST PREDICTION ARRAYS -------*/
  dist = (float *)malloc(sizeof(float)*nt);
  if( (options->testLabelFile[0]) && (testLabels.n_cnsrs != 0) ) {
  dist_cnsrs = (float *)malloc(sizeof(float)*(nt-testLabels.n_cnsrs));
  }

  /* JL Mar. 2009: Check if the number of classes in labelfile is grater than
   * the number of classes in model and allocate multi-class arrays based on that.
   */
  classCountMax = afniModel->class_count;
  if( options->testLabelFile[0] ) {
    if (testLabels.n_classes > afniModel->class_count) {
	  classCountMax = testLabels.n_classes;
    }
  }

  /* JL Apr. 2010: Allocate p string for strtok */
  p_string_size = afniModel->combinations*CSV_STRING;
  if ( (p = (char *) malloc(p_string_size * sizeof (char))) == NULL ) {
    ERROR_exit("Could no allocate csv-string in test_routine!");
  }

  /* Note: if not multiclass these may not get used - moreover, if only one
   * multiclass approach still not everything will get used. So perhaps being a
   * little inneficient here */
  /* multiclass_dist = (double *)calloc(sizeof(double),nt); -- SL Aug. 08*/
  multiclass_dist = Allocate2f((long) afniModel->combinations, (long) nt);
  classCorrect = (float *)malloc(sizeof(float)*classCountMax);
  classIncorrect = (float *)malloc(sizeof(float)*classCountMax);
  nClass = (long *)malloc(sizeof(long)*classCountMax);
  classVote = (int *)malloc(sizeof(long)*classCountMax);
  
  if (options->testLabelFile[0]) {
    checkTestLabels(&testLabels, options->testLabelFile, afniModel->class_count);
  }

  for(i = 0; i < afniModel->combinations; ++i ) {
    if(verbosity >= 1) {
      INFO_message(" ");
      INFO_message("--------------------------------------------------------------"
          "------------------");
      INFO_message("Category combination = %ld  (%s)", i, afniModel->combName[i]);
    }
 
    if( options->testLabelFile[0] ) {
      /* use strtok to recover combination name integers so that we can use
       * the test label data */
      strncpy(p, afniModel->combName[i], p_string_size);
      q = strtok(p,"_");
      cc = atol(q);
      q = strtok(NULL,"_");
      dd = atol(q);

      getCensoredClassTarget(censoredTargets, &sampleCount, &testLabels, cc, dd);
      correct=0.0; 
      incorrect=0.0;
      no_accuracy=0.0;
      res_a=0.0;
      res_b=0.0;
      res_c=0.0;
      res_d=0.0;
    }
  
    /*----- GET SVM-LIGHT MODEL STRUCTURE -----*/
    updateModel(model, afniModel, options, (int) i); 

    if(afniModel->class_count == 2) {
      snprintf(predictionsFile, LONG_STRING, "%s.1D", options->predFile);
    }
    else {
      snprintf(predictionsFile, LONG_STRING, "%s_%s.1D", options->predFile,
          afniModel->combName[i]);
    }
    if( (fp = fopen( predictionsFile, "w" )) == NULL ) {
      ERROR_exit("Could not open file for writing predictions: %s", predictionsFile );
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
    if( (options->testLabelFile[0]) && (testLabels.n_cnsrs != 0) &&
        (!options->noPredDetrend) ) {
      detrend_linear_cnsrs(dist, dist_cnsrs, &testLabels);
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

     /* printf("DBG: censoredTargets[%ld] = %f, dist[%ld]= %f\n",
           j, censoredTargets[j], j, dist[j]); */


        if( abs(censoredTargets[j]) != 9999) {
          if(dist[j]>0) {
            if(censoredTargets[j]>0) correct++; else incorrect++;
            if(censoredTargets[j]>0) res_a++; else res_b++;
          }
          else {
            if(censoredTargets[j]<0) correct++; else incorrect++;
            if(censoredTargets[j]>0) res_c++; else res_d++;
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
     * noPredCensor: Only write predictions for current class-combination and
     * without censored timepoints
     * noPredScale: Do not scale predictions to {0,1}
     */
    for(j = 0; j < nt; ++j) {
      /* multiclass_dist[j] += dist[j]; -- SL Aug. 08*/
      multiclass_dist[i][j] += dist[j];

      /* convert output prediction to {0,1} class scale */
      if (!options->noPredScale) dist[j] = 0.5*( dist[j] + 1 );

      /* output integer class memberships */
      if( (options->classout) && (!options->noPredScale) ){
        dist[j] = rint(dist[j]); /* round (no rintf) */
	if(dist[j] > 1) dist[j] = 1.0;
 	if(dist[j] < 0) dist[j] = 0.0;
      }

      /* only write non-censored predicitons */
      if (options->noPredCensor) {
        if( abs(censoredTargets[j]) != 9999) fprintf(fp,"%.8g\n",dist[j]);
      }
      else fprintf(fp,"%.8g\n",dist[j]);
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
      snprintf(predictionsFile, LONG_STRING, "%s_overall_DAG.1D", options->predFile);
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
    fclose(fp);
    }
  }
  if(verbosity >= 1)  INFO_message("Predictions for all categories written to %s",
    predictionsFile);


/* this was original multiclass. For now only supporting 2-class DAG and voting -
 * will add different methods in future. */
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
      /* JL: Apr. Check for nan */
      if (nClass[c] == 0) {
        INFO_message("                       "
            "classLabel=%ld: 0.00 (0 correct, 0 incorrect, 0 total)", c);

      }
      else {
	  INFO_message("                       "
	      "classLabel=%ld: %.2f%% (%d correct, %d incorrect, %ld total)\n",
	      c, (float)(classCorrect[c])*100.0/nClass[c],
	      (int)rint(classCorrect[c]), (int)rint(classIncorrect[c]), nClass[c] );
      }
    }
  }

  /* JL Mar 2010 */
  if( options->testLabelFile[0] ) {
    if (testLabels.n_classes > afniModel->class_count) {
  	  WARNING_message("Number of classes: %d in labelfile: %s is grater than\n"
  			  "            the number of classes: %d in modelfile: %s",
  			  testLabels.n_classes, options->testLabelFile,
  			  afniModel->class_count, options->modelFile);
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
  if( !(options->outModelNoMask) ) DSET_unload(dsetMask);
  if( (options->testLabelFile[0]) && (testLabels.n_cnsrs != 0) ) free(dist_cnsrs);
  DSET_unload( dsetTest );
  free(p);


  EXRETURN;
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

void test_regression (ASLoptions *options, MODEL *model, AFNI_MODEL *afniModel, 
    THD_3dim_dataset *dsetTest, THD_3dim_dataset *dsetMask, THD_3dim_dataset *dsetModel, 
    int argc, char **argv)
{
  long nt             = 0;        /* number of time points in test dataset */
  long nvox           = 0;        /* number of voxels per time point in test dataset */
  long nt_mod         = 0;        /* nt for model dataset */
  long nvox_mod       = 0;	  /* nvox for model dataset */
  
  DOC*                             /* array to hold test dataset in svm-light data */
    docsTest          = NULL;     /* structure */
  DatasetType**
    dsetTestArray     = NULL;	  /* array to hold test dataset values */
  DatasetType**
    dsetModelArray    = NULL;	  /* array to hold model dataset values */
  DatasetType*
    tmp_dsetArray     = NULL;	  /* temporary array to hold dataset values */

  MaskType*
    dsetMaskArray    = NULL;      /* array to hold mask dataset */
 
  double dist_tmp     = 0;        /* temporary variable */
  double *dist        = NULL;     /* array holding the classification results for
                                     each tinepoint */

  LabelType *target   = NULL;     /* labels without censored timepoints. Assuming 
                                     the 'truth' is known and we want to determine
                                     the error. */ 
  LABELS testLabels;  
  long i,j,c          = 0;
  FILE* fp            = NULL;

  char* inModelFile   = NULL;
  char inModelFileMask[LONG_STRING];
  char* inModelFileMaskExt = MODEL_MSK_EXT;
  char predictionsFile[LONG_STRING];
  
  double rms          = 0;         /* used to calculate rms error */

  ENTRY("test_regression"); 
 
  if (verbosity >= 1) INFO_message("\n++ REGRESSION (testing):\n++");

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

  /*----- GET TEST LABELS -------*/
  if( options->testLabelFile[0] ) {
    target = getAllocateRegressionLabels(&testLabels,options->testLabelFile,
      options->censorFile);

    if( testLabels.n != nt ) {
      ERROR_exit("Number of labels do not match the length of the test dataset:\n" 
          "   labelfile: '%s' contains %ld labels, but the \n"
          "   test dataset: '%s' contains %ld entries. ", 
          options->testLabelFile, testLabels.n, options->testFile, nt);
    }
  }

  /*----- PRODUCE TEST DATA ARRAY -------*/
  dsetTestArray = getAllocateDsetArray(dsetTest);

  /*----- LOAD MODEL ARRAY -----*/
  dsetModel = THD_open_one_dataset( options->modelFile );
  DSET_load ( dsetModel );
  nt_mod = DSET_NUM_TIMES( dsetModel );
  nvox_mod = DSET_NVOX( dsetModel );
  
  /*----- PRODUCE MODEL DATA ARRAY -------*/
  dsetModelArray = getAllocateDsetArray(dsetModel);

  /*----- LOAD MODEL MASK -----*/
  /* TODO: include the mask as a sub-brick of the model */
  
  if( !(options->outModelNoMask) ) {

    /* ---- determine file name for model-mask ---- */ 
    inModelFile = DSET_PREFIX( dsetModel );
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
      ERROR_exit("Viewtype of model: %s unknown!", options->modelFile);
    }
    
    /* ---- open mask dataset ---- */
    dsetMask = THD_open_one_dataset( inModelFileMask );
    if ( dsetMask == NULL ) {
      ERROR_exit("Failed to open mask dataset: %s. If not using a mask file, " 
          "you must use option -nomodelmask\n", inModelFileMask );
    }
    DSET_load( dsetMask );

    /*----- PRODUCE MASK DATA ARRAY -------*/
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
  snprintf(predictionsFile, LONG_STRING, "%s.1D", options->predFile);
  if( (fp = fopen(predictionsFile, "w" )) == NULL ) {
      ERROR_exit("Could not open file for writing predictions: %s", 
          predictionsFile );
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
        if( testLabels.cnsrs[j] == 1 ) fprintf(fp,"%.8g\n",dist[j]);
      }
      else fprintf(fp,"%.8g\n",dist[j]);
    }
    else fprintf(fp,"%.8g\n",dist[j]);
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
  DOC*   docsClassTrain = NULL;  /* svm-light data structure used for training
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



  long i,j,k, cc, dd     =  0;
  char alphaFile[LONG_STRING];	  /* naming of alphafile output */
  char docFileName[LONG_STRING];  /* naming of svm-light textfile output */
   

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

  if(verbosity >= 1)  
    INFO_message( "Number of time samples is %ld, and voxels %ld in training "
        "dataset.",nt,nvox );

  /*---- CRATE TRAINIG DATASET ARRAY ----*/
  dsetTrainArray = getAllocateDsetArray(dsetTrain);

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

  /*----- RETRIEVE AND CHECK LABELS --------------*/
  labels.n = nt;

  /* --- retrieve training labels --- */
  getLabels(&labels,options->labelFile, options->censorFile);

  /* JL Apr. 2010: Added some error checking */
  if (labels.n_classes < 2) {
     ERROR_exit("There is only one class in labelfile: '%s'. Need at least two!",
         options->labelFile);
  }

  if(labels.n != nt) {
    ERROR_exit("Number of labels do not match the length of the train dataset:\n"
          "   labelfile: '%s' contains %ld labels, but the \n"
          "   trainvol:  '%s' contains %ld bricks.", options->labelFile,
          labels.n, options->trainFile, nt);
  }

  /* --- check if labels are continues --- */
  checkTrainLabels(&labels, options->labelFile);


  /*----- ALLOCATE afniModel --------------*/
  allocateAfniModel(&afniModel,&labels, options);

  /*----- ALLOCATE censoredTarget --------------*/
  censoredTarget = (LabelType*)malloc(sizeof(LabelType)*labels.n);
  if( censoredTarget == NULL ) {
    ERROR_exit("Memory allocation error in train_routine! " 
        "Could not allocate classLabels.");
  }
  
  /*----- ALLOCATE maps -----*/
  if ( options->modelWeightFile[0] ) {
    allocateModelMaps(&maps, (long)labels.n_classes, nvox, options->kernelName);
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

      getCensoredClassTarget(censoredTarget, &sampleCount, &labels, cc, dd);
      if(verbosity >= 1) INFO_message( "SampleCount = %ld\n", sampleCount );

      /*----- ALLOCATE MEMORY for svm-light arrays -----------*/
      docsClassTrain = (DOC*)malloc(sizeof(DOC)*sampleCount);
      classTarget = (LabelType*)malloc(sizeof(LabelType)*sampleCount);
      if( docsClassTrain == NULL || classTarget == NULL ) {
        ERROR_exit("Memory allocation error in train_routine! " 
            "Could not allocate docsClassTrain and/or classTarget.\n");
      }

      /*----- GET TRAINING ARRAY AND CLASSTARGET FOR CURRENT CLASS COMBINATION -----*/
      dsetClassTrainArray = Allocate2DT( sampleCount, nvox);
      getClassTrainArrayAndTarget(dsetTrainArray, censoredTarget,
          dsetClassTrainArray, classTarget, nt, nvox);


      /*----- ALPHA FILE OUTPUT -----*/
      /* alpha file output may not be required if training and testing */
      /* performed at same time. */
      if(options->modelFile[0]) {
        if( options->modelAlphaFile[0] ) { /* user defined alpha file name */
          snprintf( alphaFile, LONG_STRING, "%s_%d_%d.1D", options->modelAlphaFile,
              labels.class_list[cc], labels.class_list[dd] );
          strncpy( learn_parm->alphafile, alphaFile, LONG_STRING);
        }
      }

      /* ----- ALLOCATE DOCS -----*/
      AllocateDOCwords(docsClassTrain, sampleCount, nvox_masked);

      /* ---- MASK DATA AND CONVERT TO SVM-LIGHT DOC STRUCTURE */
      afni_dset_to_svm_doc( docsClassTrain, dsetClassTrainArray, dsetMaskArray,
          options, sampleCount, nvox, nvox_masked );


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
      
        }
        fflush(stdout);

        /* ---- SAVE RESULTS FOR CURRENT CLASS COMBINATION ---*/
        addToAfniModel(&afniModel, model, learn_parm, censoredTarget, options,
            classCount, sampleCount, labels.class_list[cc], labels.class_list[dd]);

        if( options->modelWeightFile[0] ) {
          addToModelMap_bucket(&maps, &afniModel, dsetClassTrainArray,
              dsetMaskArray, options->maskFile, classCount);
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

      if ( !options->docFileOnly[0] ) free(model->supvec);
      if ( !options->docFileOnly[0] ) free(model->alpha);
      if ( !options->docFileOnly[0] ) free(model->index);

      freeDOCwords(docsClassTrain, sampleCount);
      free2DT( dsetClassTrainArray, sampleCount );
      free(docsClassTrain);
      free(classTarget);
    }
  }


  /* JL Jan 2009: To be memory efficient: dsetTrain is loaded in writeModelBrik
   * which might cause memory problems for large training datasets,
   * if not freed here */
  DSET_unload( dsetTrain );


  /* ----- WRITE MODEL AND BUCKET TO DISC ----- */
  if ( !options->docFileOnly[0] ) {
    writeModelBrik(&afniModel, options, options->modelFile, argc, argv);
  }
  /* TODO: in the future, may want to add some flexibility, such as making the
   * mask sub-brick in the model (but that would mix two different data types.
   * Another possibility is to add the name of the mask as one of the model
   * variables */

  if( (options->maskFile[0]) && (!options->docFileOnly[0]) ) {
    writeModelMask(dsetMask, dsetMaskArray, options->modelFile);
  }

  if( (options->modelWeightFile[0]) && (!options->docFileOnly[0]) ) {
    writeModelMap_bucket(&maps, dsetMaskArray, dsetTrain, options->maskFile,
        options->modelWeightFile, afniModel.b, afniModel.combinations,
        options, argc, argv);
  }

  if ( options->modelWeightFile[0] ) freeModelMaps(&maps);
  freeLabels(&labels);
  freeAfniModel(&afniModel);
  free(censoredTarget);
  free2DT(dsetTrainArray, nt);
  
  EXRETURN;
}

/* JL May 2009: Added this function for sv-regression.
 * It is very similar to train_routine() (a lot of code has been recycled). 
 * However, major differences are:
 *
 *    - No need for multi-class
 *    - New function to read in the labelfile (getAllocateRegressionLabels())
 *    - New function to get the array with training data
 *      (getAllocateCensoredRegressionArray())
 *    - Using svm-light's function call: svm_learn_regression() instead of
 *      svm_learn_classification()
 */ 
void train_regression(MODEL *model, LEARN_PARM *learn_parm, 
    KERNEL_PARM *kernel_parm, long *kernel_cache_size, ASLoptions *options,
    THD_3dim_dataset *dsetTrain, THD_3dim_dataset *dsetMask,
    MaskType *dsetMaskArray, int argc, char **argv)
{
  
  LABELS labels;
  AFNI_MODEL afniModel;	         /* holds everything required to write out
                                    model.Head */
  MODEL_MAPS maps;               /* holds the maps (e.g., weight-vector maps)
                                    for the bucket */

  LabelType* target       = NULL; /* array to hold labels for svm-light */

  DatasetType**
    dsetTrainArray        = NULL; /* array to hold training dataset values */

  DatasetType**
   dsetTrainArrayCensored = NULL; /* array to hold training dataset values */

  DOC    *docsTrain       = NULL; /* svm-light data structure used for training */
  KERNEL_CACHE kernel_cache;     /* svm-light data structure holding kernel
                                  * paramters */


  long nt                 = 0;   /* number of time points in TRAIN dataset */
  long nvox               = 0;   /* number of voxels per time point in TRAIN
                                    dataset */
  long nvox_masked        = 0;    /* number of voxels in mask dataset */

  long sampleCount        = 0;    /* number of samples used in training */

  long i,j,k              = 0;
  char alphaFile[LONG_STRING];	  /* naming of alphafile output */
  char docFileName[LONG_STRING];  /* nameing of svm-light textfile  output */


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
  target = getAllocateRegressionLabels(&labels,options->labelFile,
      options->censorFile);

    if (labels.n != nt) 
    ERROR_exit("Number of labels %ld in labelfile '%s' does not match\n"
        "   number of timepoints %ld in train dataset '%s'!", labels.n,
        options->labelFile, nt, options->trainFile);
  
  sampleCount=labels.n-labels.n_cnsrs; /* number of uncensored timepoints */

  /*----  GET TRAINING ARRAAY -----*/
  dsetTrainArray = getAllocateDsetArray(dsetTrain);

  /*----- GET TRAINING ARRAY WITHOUT CENSORED TIMEPOINTS -----*/
  dsetTrainArrayCensored = getAllocateCensoredRegressionArray(dsetTrainArray,
      &labels, nvox);
  
  /*----- ALLOCATE afniModel -----*/
  allocateAfniModel(&afniModel, &labels, options);

  /*----- ALLOCATE maps ------*/
  allocateModelMaps(&maps, (long)labels.n_classes, nvox, options->kernelName);

  /*----- ALLOCATE DOCs & WORDs------*/
  docsTrain = (DOC*)malloc(sizeof(DOC)*sampleCount);
  AllocateDOCwords(docsTrain, sampleCount, nvox_masked);

  /*----- CONVERT TRAINING ARRAY TO SVM-LIGHT DATASTRUCTURE ------*/
  afni_dset_to_svm_doc( docsTrain, dsetTrainArrayCensored, dsetMaskArray,
          options, sampleCount, nvox, nvox_masked );

  /* JL Apr. 2010: No training if we want to write out the svm-light
   * formated textfile only */
  if ( !options->docFileOnly[0] ) {

    /*----- PERFORM THE SV-REGRESSION -----*/
    if ( !strcmp(options->kernelName, "linear") ) {

      svm_learn_regression ( docsTrain, target, sampleCount, nvox_masked,
          learn_parm, kernel_parm, NULL, model);

    }
    else { /* non-linar kernel */
      kernel_cache_init(&kernel_cache, sampleCount, *kernel_cache_size);

      svm_learn_regression ( docsTrain, target, sampleCount, nvox_masked,
          learn_parm, kernel_parm, &kernel_cache, model);
    }
  }
  
  /*----- UPDATE AFNI-MODEL -----*/
  if ( !options->docFileOnly[0] ) {
  addToAfniModel(&afniModel, model, learn_parm,  &(labels.cnsrs[0]), options,
      0, sampleCount*2, 0, 0);
  }

  /*---- UPDATE MODEL-MAPS -----*/
  if( (options->modelWeightFile[0]) && (!options->docFileOnly[0]) ) {
    addToModelMap_bucket(&maps, &afniModel, dsetTrainArrayCensored, dsetMaskArray,
        options->maskFile, 0);
  }

  /*---- WRITE OUTPUT FILES TO DISC ----*/
  /* might not be neccessary if testing and training are performed all at once */

  /* JL Jan 2009: To be memory efficient: dsetTrain is loaded in writeModelBrik
   * which might cause memory problems for large training datasets,
   * if not freed here */
   DSET_unload( dsetTrain );

  /* --- model --- */
  if ( !options->docFileOnly[0] ) {
    writeModelBrik(&afniModel, options, options->modelFile, argc, argv);
  }

    
  /* --- (model) mask --- */
  /* would be great to change the mask datatype to short and include the mask 
   * as a sub-brick of the model! */
  if( (options->maskFile[0]) && (!options->docFileOnly[0]) ) {
    writeModelMask(dsetMask, dsetMaskArray, options->modelFile);
  }

  /* --- maps --- */
  if( (options->modelWeightFile[0]) && (!options->docFileOnly[0]) ) {
    writeModelMap_bucket(&maps, dsetMaskArray, dsetTrain, options->maskFile, 
        options->modelWeightFile, afniModel.b, (long)afniModel.combinations,
        options, argc, argv);
  }

  /* --- svm-light textfile ---*/
  if (options->docFile[0]) {
    snprintf( docFileName, LONG_STRING, "%s.svml", options->docFile);
    write_svmLight_doc(docsTrain, sampleCount, nvox_masked, target, 
        docFileName, VERSION_SVMLIGHT);
  }
  
  /*----- FREE MEMORY -----*/
  freeDOCwords(docsTrain, sampleCount);
  free(docsTrain);
  freeLabels(&labels);
  free(target);
  freeAfniModel(&afniModel);
  free2DT( dsetTrainArray, nt);
  free2DT( dsetTrainArrayCensored, sampleCount);
  freeModelMaps(&maps);
  if ( !options->docFileOnly[0] ) free(model->supvec);
  if ( !options->docFileOnly[0] ) free(model->alpha);
  if ( !options->docFileOnly[0] ) free(model->index);

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
  int typeFlag = 0;    /* JL: flag for 3dsvm options -type */
  *mode = NOTHING;

  ENTRY("input_parse");
  
  /* svmlight defaults */
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
  strncpy(optionsData->predFile, "",        LONG_STRING);
  strncpy(optionsData->svmType,"",          LONG_STRING);
  optionsData->outModelNoMask = 0;
  optionsData->noPredDetrend  = 0;
  optionsData->classout       = 0;
  optionsData->noPredCensor   = 0;
  optionsData->noPredScale    = 0;

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
    if( !strcmp(argv[i],"-doconly") )        { parseFlag=1; i=ppi(argc,i,argv[i]);
                                               strncpy(optionsData->docFileOnly,
                                               argv[i], LONG_STRING); }
    /* for kernel below, using svm-light options for kernel parameters */
    if( !strcmp(argv[i],"-kernel") )        { parseFlag=1; i=ppi(argc,i,argv[i]); 
                                              strncpy(optionsData->kernelName,
                                              argv[i], LONG_STRING); }

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
    if( !strcmp(argv[i],"-nopredcensored") ){ parseFlag=1; optionsData->noPredCensor = 1; }
    if( !strcmp(argv[i],"-nopredscale") )   { parseFlag=1; optionsData->noPredScale = 1; }

    if( !strcmp(argv[i],"-change_summary")) { printf( change_string ); RETURN(0); }
    if( !strcmp(argv[i],"-version")) { print_version();  RETURN(0); }
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
  
    if( !parseFlag ) {
      snprintf(errorString, LONG_STRING, "Illegal option: %s !", argv[i]);
      RETURN(1);
    }
  }

  if ( argc == 1 ) {
    printf( cl_helpstring );
    printf("\n\n-------------------- SVM-light learn help -----------------------------\n");
    print_help_learn();
    printf("\n\n-------------------- SVM-light classify help -----------------------------\n");
    print_help_classify();
    printf("\n\n--------------------------------------------------------------------------\n");
    printf( contribution_string );
    RETURN(0);
  }

  /* JL May 2009: Some error checking and initialization for svm learning type */
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
    strncpy(optionsData->svmType,"classification", LONG_STRING); /* (matches default
                                                  for learn_parm->type) */ 
    
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

  /* JL Apr. 2010: Added the ability to write out svm-light textfile without
   * having to go through training or to testing.
   *
   * Still going through train function for classification or regression
   * (-type mandatory!) but ONLY the doc textfile is written.
   *
   */
  if (optionsData->docFileOnly[0]) {

    /* some error checking for docout only */
    if ( (!typeFlag) && (!zFlag) ) {
      snprintf(errorString, LONG_STRING, "Must specify -type for -doconly!");
      RETURN(1);
    }

    if ( (optionsData->trainFile[0]) && (optionsData->testFile[0]) ){
      snprintf(errorString, LONG_STRING, "Please specify either -trainvol or "
          "-testvol for -doconly!"); RETURN(1);
    }

    if ( (optionsData->labelFile[0]) && (optionsData->testLabelFile[0]) ){
      snprintf(errorString, LONG_STRING, "Please specify either -tainlabels or"
          " -testlabels for -doconly!"); RETURN(1);
     }

    /* make sure this works for -testvol as well */
    if ( optionsData->testFile[0] ) {
      snprintf(optionsData->trainFile, LONG_STRING, optionsData->testFile);
    }

    if ( optionsData->testLabelFile[0] ) {
          snprintf(optionsData->labelFile, LONG_STRING, optionsData->testLabelFile);
    }

    /* set mode */
    *mode=TRAIN;

    /* check for mask */
    if ( !optionsData->maskFile[0] ) {
      optionsData->outModelNoMask = 1;
    }

    snprintf(optionsData->docFile, LONG_STRING, optionsData->docFileOnly);

    RETURN(0);
  }

  if( (optionsData->docFile[0]) && (optionsData->testFile[0]) ) {
    snprintf(errorString, LONG_STRING, "Sorry, option  -testvol toghether with "
        "-docout is not supported. Please use option -doconly instead!"); RETURN(1);
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
    strncpy(optionsData->kernelName, "linear", LONG_STRING); /* (matches default
                                                  for kernel_type) */

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
        
        strncpy(optionsData->modelWeightFile, "", LONG_STRING);
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

