#include "cdflib.h"
double esum(int *mu,double *x)
/*
-----------------------------------------------------------------------
                    EVALUATION OF EXP(MU + X)
-----------------------------------------------------------------------
*/
{
static double esum,w;
/*
     ..
     .. Executable Statements ..
*/
    if(*x > 0.0e0) goto S10;
    if(*mu < 0) goto S20;
    w = (double)*mu+*x;
    if(w > 0.0e0) goto S20;
    esum = exp(w);
    return esum;
S10:
    if(*mu > 0) goto S20;
    w = (double)*mu+*x;
    if(w < 0.0e0) goto S20;
    esum = exp(w);
    return esum;
S20:
    w = *mu;
    esum = exp(w)*exp(*x);
    return esum;
} /* END */
