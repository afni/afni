
 /************************************************************************/
 /**  Functions to compute cumulative distributions and their inverses  **/
 /**  for the NIfTI-1 statistical types.  Much of this code is taken    **/
 /**  from other sources.  In particular, the cdflib functions by       **/
 /**  Brown and Lovato make up the bulk of this file.  That code        **/
 /**  was placed in the public domain.  The code by K. Krishnamoorthy   **/
 /**  is also released for unrestricted use.  Finally, the other parts  **/
 /**  of this file (by RW Cox) are released to the public domain.       **/
 /**                                                                    **/
 /**  Most of this file comprises a set of "static" functions, to be    **/
 /**  called by the user-level functions at the very end of the file.   **/
 /**  At the end of the file is a simple main program to drive these    **/
 /**  functions.                                                        **/
 /**                                                                    **/
 /**  To find the user-level functions, search forward for the string   **/
 /**  "nifti_", which will be at about line 11000.                      **/
 /************************************************************************/
 /*****==============================================================*****/
 /***** Neither the National Institutes of Health (NIH), the DFWG,   *****/
 /***** nor any of the members or employees of these institutions    *****/
 /***** imply any warranty of usefulness of this material for any    *****/
 /***** purpose, and do not assume any liability for damages,        *****/
 /***** incidental or otherwise, caused by any use of this document. *****/
 /***** If these conditions are not acceptable, do not use this!     *****/
 /*****==============================================================*****/
 /************************************************************************/

#include <stdlib.h>

/****************************************************************************
 Statistical codes implemented :

     NIFTI_INTENT_CORREL     = correlation statistic
     NIFTI_INTENT_TTEST      = t statistic (central)
     NIFTI_INTENT_FTEST      = F statistic (central)
     NIFTI_INTENT_ZSCORE     = N(0,1) statistic
     NIFTI_INTENT_CHISQ      = Chi-squared (central)
     NIFTI_INTENT_BETA       = Beta variable (central)
     NIFTI_INTENT_BINOM      = Binomial variable
     NIFTI_INTENT_GAMMA      = Gamma distribution
     NIFTI_INTENT_POISSON    = Poisson distribution
     NIFTI_INTENT_FTEST_NONC = noncentral F statistic
     NIFTI_INTENT_CHISQ_NONC = noncentral chi-squared
     NIFTI_INTENT_TTEST_NONC = noncentral t statistic
     NIFTI_INTENT_CHI        = Chi statistic (central)
     NIFTI_INTENT_INVGAUSS   = inverse Gaussian variable
     NIFTI_INTENT_WEIBULL    = Weibull distribution
     NIFTI_INTENT_EXTVAL     = Extreme value type I
     NIFTI_INTENT_NORMAL     = N(mu,variance) normal
     NIFTI_INTENT_LOGISTIC   = Logistic distribution
     NIFTI_INTENT_LAPLACE    = Laplace distribution
     NIFTI_INTENT_UNIFORM    = Uniform distribution
     NIFTI_INTENT_PVAL       = "p-value"
     NIFTI_INTENT_LOGPVAL    = -ln(p)
     NIFTI_INTENT_LOG10PVAL  = -log10(p)
*****************************************************************************/

extern char *inam[];

int nifti_intent_code( char *name );
double nifti_stat2cdf( double val, int code, double p1,double p2,double p3 );
double nifti_stat2rcdf( double val, int code, double p1,double p2,double p3 );
double nifti_cdf2stat( double p , int code, double p1,double p2,double p3 );
#if defined(__COMPILE_UNUSED_FUNCTIONS__)
double nifti_rcdf2stat( double q , int code, double p1,double p2,double p3 );
#endif/*(__COMPILE_UNUSED_FUNCTIONS__)*/
double nifti_stat2zscore( double val , int code, double p1,double p2,double p3);
double nifti_stat2hzscore( double val, int code, double p1,double p2,double p3);

