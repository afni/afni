#include "niml_private.h"

/****************************************************************************/
/********* Statistics stuff for NIML ****************************************/
/****************************************************************************/

#if 0
# define NI_STAT_CORREL      2   /* Samples, fits, orts   */
# define NI_STAT_TTEST       3   /* DOF                   */
# define NI_STAT_FTEST       4   /* 2 DOF                 */
# define NI_STAT_ZSCORE      5   /* no params             */
# define NI_STAT_CHISQ       6   /* DOF                   */
# define NI_STAT_BETA        7   /* a and b params        */
# define NI_STAT_BINOM       8   /* # trials, p per trial */
# define NI_STAT_GAMMA       9   /* shape, scale params   */
# define NI_STAT_POISSON    10   /* mean                  */
# define NI_STAT_NORMAL     11   /* mean, variance        */
# define NI_STAT_FTEST_NONC 12   /* 2 DOF, noncentrality  */
# define NI_STAT_CHISQ_NONC 13   /* DOF, noncentrality    */
# define NI_STAT_LOGISTIC   14   /* location, scale       */
# define NI_STAT_LAPLACE    15   /* location, scale       */
# define NI_STAT_UNIFORM    16   /* start, end            */
# define NI_STAT_TTEST_NONC 17   /* DOF, noncentrality    */
# define NI_STAT_WEIBULL    18   /* location, scale, power*/
# define NI_STAT_CHI        19   /* DOF                   */
# define NI_STAT_INVGAUSS   20   /* mu, lambda            */
# define NI_STAT_EXTVAL     21   /* location, scale       */
#endif

static int numparam[] = { 0,0,3,1,2,0,1,2,2,2,
                          1,2,3,2,2,2,2,2,3,1,
                          2,2,
                        -1 } ;

static char *distname[] = { "none"     , "none"    , "Correl"     , "Ttest"      , "Ftest"    ,
                            "Zscore"   , "Chisq"   , "Beta"       , "Binom"      , "Gamma"    ,
                            "Poisson"  , "Normal"  , "Ftest_nonc" , "Chisq_nonc" , "Logistic" ,
                            "Laplace"  , "Uniform" , "Ttest_nonc" , "Weibull"    , "Chi"      ,
                            "Invgauss" , "Extval"  ,
                          NULL } ;

int NI_stat_numparam( int scode )
{
   return numparam[(scode >=0 && scode <= NI_STAT_LASTCODE) ? scode : 0] ;
}

char * NI_stat_distname( int scode )
{
   return distname[(scode >=0 && scode <= NI_STAT_LASTCODE) ? scode : 0] ;
}
