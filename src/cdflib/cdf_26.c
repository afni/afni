#include "cdflib.h"
void cumbin(double *s,double *xn,double *pr,double *ompr,
	    double *cum,double *ccum)
/*
**********************************************************************
 
     void cumbin(double *s,double *xn,double *pr,double *ompr,
            double *cum,double *ccum)

                    CUmulative BINomial distribution
 
 
                              Function
 
 
     Returns the probability   of 0  to  S  successes in  XN   binomial
     trials, each of which has a probability of success, PBIN.
 
 
                              Arguments
 
 
     S --> The upper limit of cumulation of the binomial distribution.
                                                  S is DOUBLE PRECISION
 
     XN --> The number of binomial trials.
                                                  XN is DOUBLE PRECISIO
 
     PBIN --> The probability of success in each binomial trial.
                                                  PBIN is DOUBLE PRECIS
 
     OMPR --> 1 - PBIN
                                                  OMPR is DOUBLE PRECIS
 
     CUM <-- Cumulative binomial distribution.
                                                  CUM is DOUBLE PRECISI
 
     CCUM <-- Compliment of Cumulative binomial distribution.
                                                  CCUM is DOUBLE PRECIS
 
 
                              Method
 
 
     Formula  26.5.24    of   Abramowitz  and    Stegun,  Handbook   of
     Mathematical   Functions (1966) is   used  to reduce the  binomial
     distribution  to  the  cumulative    beta distribution.
 
**********************************************************************
*/
{
static double T1,T2;
/*
     ..
     .. Executable Statements ..
*/
    if(!(*s < *xn)) goto S10;
    T1 = *s+1.0e0;
    T2 = *xn-*s;
    cumbet(pr,ompr,&T1,&T2,ccum,cum);
    goto S20;
S10:
    *cum = 1.0e0;
    *ccum = 0.0e0;
S20:
    return;
} /* END */
