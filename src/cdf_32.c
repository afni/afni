#include "cdflib.h"
void cumnbn(double *s,double *xn,double *pr,double *ompr,
	    double *cum,double *ccum)
/*
**********************************************************************
 
     void cumnbn(double *s,double *xn,double *pr,double *ompr,
            double *cum,double *ccum)

                    CUmulative Negative BINomial distribution
 
 
                              Function
 
 
     Returns the probability that it there will be S or fewer failures
     before there are XN successes, with each binomial trial having
     a probability of success PR.
 
     Prob(# failures = S | XN successes, PR)  =
                        ( XN + S - 1 )
                        (            ) * PR^XN * (1-PR)^S
                        (      S     )
 
 
                              Arguments
 
 
     S --> The number of failures
                                                  S is DOUBLE PRECISION
 
     XN --> The number of successes
                                                  XN is DOUBLE PRECISIO
 
     PR --> The probability of success in each binomial trial.
                                                  PR is DOUBLE PRECISIO
 
     OMPR --> 1 - PR
                                                  OMPR is DOUBLE PRECIS
 
     CUM <-- Cumulative negative binomial distribution.
                                                  CUM is DOUBLE PRECISI
 
     CCUM <-- Compliment of Cumulative negative binomial distribution.
                                                  CCUM is DOUBLE PRECIS
 
 
                              Method
 
 
     Formula  26.5.26    of   Abramowitz  and    Stegun,  Handbook   of
     Mathematical   Functions (1966) is   used  to reduce the  negative
     binomial distribution to the cumulative beta distribution.
 
**********************************************************************
*/
{
static double T1;
/*
     ..
     .. Executable Statements ..
*/
    T1 = *s+1.e0;
    cumbet(pr,ompr,xn,&T1,cum,ccum);
    return;
} /* END */
