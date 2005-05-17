#ifndef _NIFTI_STATLIB_HEADER_

#ifndef NIFTI_INTENT_CORREL          /* abstracted from nifti1.h */
#  define NIFTI_INTENT_CORREL      2   /* NIfTI p1 DOF = AFNI p1-p2-p3 */
#  define NIFTI_INTENT_TTEST       3
#  define NIFTI_INTENT_FTEST       4
#  define NIFTI_INTENT_ZSCORE      5
#  define NIFTI_INTENT_CHISQ       6
#  define NIFTI_INTENT_BETA        7
#  define NIFTI_INTENT_BINOM       8
#  define NIFTI_INTENT_GAMMA       9
#  define NIFTI_INTENT_POISSON    10
#  define NIFTI_INTENT_NORMAL     11
#  define NIFTI_INTENT_FTEST_NONC 12
#  define NIFTI_INTENT_CHISQ_NONC 13
#  define NIFTI_INTENT_LOGISTIC   14
#  define NIFTI_INTENT_LAPLACE    15
#  define NIFTI_INTENT_UNIFORM    16
#  define NIFTI_INTENT_TTEST_NONC 17
#  define NIFTI_INTENT_WEIBULL    18
#  define NIFTI_INTENT_CHI        19
#  define NIFTI_INTENT_INVGAUSS   20
#  define NIFTI_INTENT_EXTVAL     21
#  define NIFTI_INTENT_PVAL       22
#  define NIFTI_INTENT_LOGPVAL    23
#  define NIFTI_INTENT_LOG10PVAL  24
#endif

/** Number of auxiliary parameters needed for each distribution **/

static int NIFTI_need_stat_aux[] = { 0 , 0 ,
                                     3 , 1 , 2 , 0 , 1 , 2 , 2 , 2 , 1 ,
                                     2 , 3 , 2 , 2 , 2 , 2 , 3 , 1 , 2 ,
                                     2 , 0 , 0 , 0
                                   } ;

extern float NIFTI_stat_to_pval  ( float thr , int statcode, float *stataux );
extern float NIFTI_pval_to_stat  ( float pval, int statcode, float *stataux );
extern float NIFTI_stat_to_zscore( float thr , int statcode, float *stataux );

#endif
