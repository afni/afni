/************************************************************************/
/*                                                                      */
/*   svm_common.h                                                       */
/*                                                                      */
/*   Definitions and functions used in both svm_learn and svm_classify. */
/*                                                                      */
/*   Author: Thorsten Joachims                                          */
/*   Date: 02.07.02                                                     */
/*                                                                      */
/*   Copyright (c) 2002  Thorsten Joachims - All rights reserved        */
/*                                                                      */
/*   This software is available for non-commercial use only. It must    */
/*   not be modified and distributed without prior permission of the    */
/*   author. The author is not responsible for implications from the    */
/*   use of this software.                                              */
/*                                                                      */
/************************************************************************/

/* # define MICROSOFT */  /* uncomment, if compiling with Visual C++ */

# define MAXSHRINK 50000    /* maximum number of shrinking rounds */

# include <stdio.h>
# include <ctype.h>
# include <math.h>
# include <string.h>
# include <stdlib.h>
# include <time.h> 
# include <float.h>

/* JL: changed VERSION      to VERSION_SVMLIGHT      and
 *             VERSION_DATE to VERSIOM_DATE_SVMLING
 * to avoid conflicts with afni
 */
# define VERSION_SVMLIGHT       "V5.00"
# define VERSION_DATE_SVMLIGHT  "30.06.02"

# define CFLOAT  float       /* the type of float to use for caching */
                             /* kernel evaluations. Using float saves */
                             /* us some memory, but you can use double, too */
# define FNUM    long        /* the type used for storing feature ids */
# define FVAL    float       /* the type used for storing feature values */

# define LINEAR  0           /* linear kernel type */
# define POLY    1           /* polynoial kernel type */
# define RBF     2           /* rbf kernel type */
# define SIGMOID 3           /* sigmoid kernel type */

# define CLASSIFICATION 1    /* train classification model */
# define REGRESSION     2    /* train regression model */
# define RANKING        3    /* train ranking model */

typedef struct word {
  FNUM    wnum;	               /* word number */
  FVAL    weight;              /* word weight */
} WORD;

typedef struct doc {
  long    docnum;              /* Document ID. This has to be the position of 
                                  the document in the training set array. */
  long    queryid;             /* for learning rankings, constraints are 
				  generated for documents with the same 
				  queryID. */
  double  costfactor;          /* Scales the cost of misclassifying this
				  document by this factor. The effect of this
				  value is, that the upper bound on the alpha
				  for this example is scaled by this factor.
				  The factors are set by the feature 
				  'cost:<val>' in the training data. */
  double  twonorm_sq;          /* The squared euclidian length of the document
                                  vector. */ 
  WORD    *words;              /* The words/values in the document by  
				  increasing word-number. */
} DOC;

typedef struct learn_parm {
  long   type;                 /* selects between regression and
				  classification */
  double svm_c;                /* upper bound C on alphas */
  double eps;                  /* regression epsilon (eps=1.0 for
				  classification */
  double svm_costratio;        /* factor to multiply C for positive examples */
  double transduction_posratio;/* fraction of unlabeled examples to be */
                               /* classified as positives */
  long   biased_hyperplane;    /* if nonzero, use hyperplane w*x+b=0 
				  otherwise w*x=0 */
  long   svm_maxqpsize;        /* size q of working set */
  long   svm_newvarsinqp;      /* new variables to enter the working set 
				  in each iteration */
  double epsilon_crit;         /* tolerable error for distances used 
				  in stopping criterion */
  double epsilon_shrink;       /* how much a multiplier should be above 
				  zero for shrinking */
  long   svm_iter_to_shrink;   /* iterations h after which an example can
				  be removed by shrinking */
  long   remove_inconsistent;  /* exclude examples with alpha at C and 
				  retrain */
  long   skip_final_opt_check; /* do not check KT-Conditions at the end of
				  optimization for examples removed by 
				  shrinking. WARNING: This might lead to 
				  sub-optimal solutions! */
  long   compute_loo;          /* if nonzero, computes leave-one-out
				  estimates */
  double rho;                  /* parameter in xi/alpha-estimates and for
				  pruning leave-one-out range [1..2] */
  long   xa_depth;             /* parameter in xi/alpha-estimates upper
				  bounding the number of SV the current
				  alpha_t is distributed over */
  char predfile[200];          /* file for predictions on unlabeled examples
				  in transduction */
  char alphafile[200];         /* file to store optimal alphas in. use  
				  empty string if alphas should not be 
				  output */

  /* you probably do not want to touch the following */
  double epsilon_const;        /* tolerable error on eq-constraint */
  double epsilon_a;            /* tolerable error on alphas at bounds */
  double opt_precision;        /* precision of solver, set to e.g. 1e-21 
				  if you get convergence problems */

  /* the following are only for internal use */
  long   svm_c_steps;          /* do so many steps for finding optimal C */
  double svm_c_factor;         /* increase C by this factor every step */
  double svm_costratio_unlab;
  double svm_unlabbound;
  double *svm_cost;            /* individual upper bounds for each var */
  long   totwords;             /* number of features */
  
  /* JL July 2011: Added maximum number of iterations */
  long max_iterations;

} LEARN_PARM;

typedef struct kernel_parm {
  long    kernel_type;   /* 0=linear, 1=poly, 2=rbf, 3=sigmoid, 4=custom */
  long    poly_degree;
  double  rbf_gamma;
  double  coef_lin;
  double  coef_const;
  char    custom[50];    /* for user supplied kernel */
} KERNEL_PARM;

typedef struct model {
  long    sv_num;	
  long    at_upper_bound;
  double  b;
  DOC     **supvec;
  double  *alpha;
  long    *index;       /* index from docnum to position in model */
  long    totwords;     /* number of features */
  long    totdoc;       /* number of training documents */
  KERNEL_PARM kernel_parm; /* kernel */

  /* the following values are not written to file */
  double  loo_error,loo_recall,loo_precision; /* leave-one-out estimates */
  double  xa_error,xa_recall,xa_precision;    /* xi/alpha estimates */
  double  *lin_weights;                       /* weights for linear case using
						 folding */
} MODEL;

typedef struct quadratic_program {
  long   opt_n;            /* number of variables */
  long   opt_m;            /* number of linear equality constraints */
  double *opt_ce,*opt_ce0; /* linear equality constraints */
  double *opt_g;           /* hessian of objective */
  double *opt_g0;          /* linear part of objective */
  double *opt_xinit;       /* initial value for variables */
  double *opt_low,*opt_up; /* box constraints */
} QP;

typedef struct kernel_cache {
  long   *index;  /* cache some kernel evaluations */
  CFLOAT *buffer; /* to improve speed */
  long   *invindex;
  long   *active2totdoc;
  long   *totdoc2active;
  long   *lru;
  long   *occu;
  long   elems;
  long   max_elems;
  long   time;
  long   activenum;
  long   buffsize;
} KERNEL_CACHE;


typedef struct timing_profile {
  long   time_kernel;
  long   time_opti;
  long   time_shrink;
  long   time_update;
  long   time_model;
  long   time_check;
  long   time_select;
} TIMING;

typedef struct shrink_state {
  long   *active;
  long   *inactive_since;
  long   deactnum;
  double **a_history;  /* for shrinking with non-linear kernel */
  long   maxhistory;
  double *last_a;      /* for shrinking with linear kernel */
  double *last_lin;    /* for shrinking with linear kernel */
} SHRINK_STATE;

double classify_example(MODEL *, DOC *);
double classify_example_linear(MODEL *, DOC *);
CFLOAT kernel(KERNEL_PARM *, DOC *, DOC *); 
double custom_kernel(KERNEL_PARM *, DOC *, DOC *); 
double sprod_ss(WORD *, WORD *);
WORD*  sub_ss(WORD *, WORD *); 
double model_length_s(MODEL *, KERNEL_PARM *);
void   clear_vector_n(double *, long);
void   add_vector_ns(double *, WORD *, double);
double sprod_ns(double *, WORD *);
void   add_weight_vector_to_linear_model(MODEL *);
void   read_model(char *, MODEL *, long, long);
void   read_documents(char *, DOC *, double *, long, long, long *, long *);
int    parse_document(char *, DOC *, double *, long *, long);
void   nol_ll(char *, long *, long *, long *);
long   minl(long, long);
long   maxl(long, long);
long   get_runtime(void);
void   *my_malloc(size_t); 
void   copyright_notice(void);
# ifdef MICROSOFT
   int isnan(double);
# endif

static long   verbosity;              /* verbosity level (0-4) */
extern long   kernel_cache_statistic;


