#include "cdflib.h"
void dstzr(double *zxlo,double *zxhi,double *zabstl,double *zreltl)
/*
**********************************************************************
     void dstzr(double *zxlo,double *zxhi,double *zabstl,double *zreltl)
     Double precision SeT ZeRo finder - Reverse communication version
                              Function
     Sets quantities needed by ZROR.  The function of ZROR
     and the quantities set is given here.
     Concise Description - Given a function F
     find XLO such that F(XLO) = 0.
          More Precise Description -
     Input condition. F is a double precision function of a single
     double precision argument and XLO and XHI are such that
          F(XLO)*F(XHI)  .LE.  0.0
     If the input condition is met, QRZERO returns .TRUE.
     and output values of XLO and XHI satisfy the following
          F(XLO)*F(XHI)  .LE. 0.
          ABS(F(XLO)  .LE. ABS(F(XHI)
          ABS(XLO-XHI)  .LE. TOL(X)
     where
          TOL(X) = MAX(ABSTOL,RELTOL*ABS(X))
     If this algorithm does not find XLO and XHI satisfying
     these conditions then QRZERO returns .FALSE.  This
     implies that the input condition was not met.
                              Arguments
     XLO --> The left endpoint of the interval to be
           searched for a solution.
                    XLO is DOUBLE PRECISION
     XHI --> The right endpoint of the interval to be
           for a solution.
                    XHI is DOUBLE PRECISION
     ABSTOL, RELTOL --> Two numbers that determine the accuracy
                      of the solution.  See function for a
                      precise definition.
                    ABSTOL is DOUBLE PRECISION
                    RELTOL is DOUBLE PRECISION
                              Method
     Algorithm R of the paper 'Two Efficient Algorithms with
     Guaranteed Convergence for Finding a Zero of a Function'
     by J. C. P. Bus and T. J. Dekker in ACM Transactions on
     Mathematical Software, Volume 1, no. 4 page 330
     (Dec. '75) is employed to find the zero of F(X)-Y.
**********************************************************************
*/
{
    E0001(1,NULL,NULL,NULL,NULL,NULL,NULL,NULL,zabstl,zreltl,zxhi,zxlo);
} /* END */
