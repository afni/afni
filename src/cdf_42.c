#include "cdflib.h"
void dstinv(double *zsmall,double *zbig,double *zabsst,
	    double *zrelst,double *zstpmu,double *zabsto,
	    double *zrelto)
/*
**********************************************************************
      void dstinv(double *zsmall,double *zbig,double *zabsst,
            double *zrelst,double *zstpmu,double *zabsto,
            double *zrelto)

      Double Precision - SeT INverse finder - Reverse Communication
                              Function
     Concise Description - Given a monotone function F finds X
     such that F(X) = Y.  Uses Reverse communication -- see invr.
     This routine sets quantities needed by INVR.
          More Precise Description of INVR -
     F must be a monotone function, the results of QMFINV are
     otherwise undefined.  QINCR must be .TRUE. if F is non-
     decreasing and .FALSE. if F is non-increasing.
     QMFINV will return .TRUE. if and only if F(SMALL) and
     F(BIG) bracket Y, i. e.,
          QINCR is .TRUE. and F(SMALL).LE.Y.LE.F(BIG) or
          QINCR is .FALSE. and F(BIG).LE.Y.LE.F(SMALL)
     if QMFINV returns .TRUE., then the X returned satisfies
     the following condition.  let
               TOL(X) = MAX(ABSTOL,RELTOL*ABS(X))
     then if QINCR is .TRUE.,
          F(X-TOL(X)) .LE. Y .LE. F(X+TOL(X))
     and if QINCR is .FALSE.
          F(X-TOL(X)) .GE. Y .GE. F(X+TOL(X))
                              Arguments
     SMALL --> The left endpoint of the interval to be
          searched for a solution.
                    SMALL is DOUBLE PRECISION
     BIG --> The right endpoint of the interval to be
          searched for a solution.
                    BIG is DOUBLE PRECISION
     ABSSTP, RELSTP --> The initial step size in the search
          is MAX(ABSSTP,RELSTP*ABS(X)). See algorithm.
                    ABSSTP is DOUBLE PRECISION
                    RELSTP is DOUBLE PRECISION
     STPMUL --> When a step doesn't bound the zero, the step
                size is multiplied by STPMUL and another step
                taken.  A popular value is 2.0
                    DOUBLE PRECISION STPMUL
     ABSTOL, RELTOL --> Two numbers that determine the accuracy
          of the solution.  See function for a precise definition.
                    ABSTOL is DOUBLE PRECISION
                    RELTOL is DOUBLE PRECISION
                              Method
     Compares F(X) with Y for the input value of X then uses QINCR
     to determine whether to step left or right to bound the
     desired x.  the initial step size is
          MAX(ABSSTP,RELSTP*ABS(S)) for the input value of X.
     Iteratively steps right or left until it bounds X.
     At each step which doesn't bound X, the step size is doubled.
     The routine is careful never to step beyond SMALL or BIG.  If
     it hasn't bounded X at SMALL or BIG, QMFINV returns .FALSE.
     after setting QLEFT and QHI.
     If X is successfully bounded then Algorithm R of the paper
     'Two Efficient Algorithms with Guaranteed Convergence for
     Finding a Zero of a Function' by J. C. P. Bus and
     T. J. Dekker in ACM Transactions on Mathematical
     Software, Volume 1, No. 4 page 330 (DEC. '75) is employed
     to find the zero of the function F(X)-Y. This is routine
     QRZERO.
**********************************************************************
*/
{
    E0000(1,NULL,NULL,NULL,NULL,NULL,zabsst,zabsto,zbig,zrelst,zrelto,zsmall,
    zstpmu);
} /* END */
