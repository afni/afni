#include "cdflib.h"
void cumbet(double *x,double *y,double *a,double *b,double *cum,
	    double *ccum)
/*
**********************************************************************
 
     void cumbet(double *x,double *y,double *a,double *b,double *cum,
            double *ccum)

          Double precision cUMulative incomplete BETa distribution
 
 
                              Function
 
 
     Calculates the cdf to X of the incomplete beta distribution
     with parameters a and b.  This is the integral from 0 to x
     of (1/B(a,b))*f(t)) where f(t) = t**(a-1) * (1-t)**(b-1)
 
 
                              Arguments
 
 
     X --> Upper limit of integration.
                                        X is DOUBLE PRECISION
 
     Y --> 1 - X.
                                        Y is DOUBLE PRECISION
 
     A --> First parameter of the beta distribution.
                                        A is DOUBLE PRECISION
 
     B --> Second parameter of the beta distribution.
                                        B is DOUBLE PRECISION
 
     CUM <-- Cumulative incomplete beta distribution.
                                        CUM is DOUBLE PRECISION
 
     CCUM <-- Compliment of Cumulative incomplete beta distribution.
                                        CCUM is DOUBLE PRECISION
 
 
                              Method
 
 
     Calls the routine BRATIO.
 
                                   References
 
     Didonato, Armido R. and Morris, Alfred H. Jr. (1992) Algorithim
     708 Significant Digit Computation of the Incomplete Beta Function
     Ratios. ACM ToMS, Vol.18, No. 3, Sept. 1992, 360-373.
 
**********************************************************************
*/
{
static int ierr;
/*
     ..
     .. Executable Statements ..
*/
    if(!(*x <= 0.0e0)) goto S10;
    *cum = 0.0e0;
    *ccum = 1.0e0;
    return;
S10:
    if(!(*y <= 0.0e0)) goto S20;
    *cum = 1.0e0;
    *ccum = 0.0e0;
    return;
S20:
    bratio(a,b,x,y,cum,ccum,&ierr);
/*
     Call bratio routine
*/
    return;
} /* END */
