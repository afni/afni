#include "cdflib.h"
double dt1(double *p,double *q,double *df)
/*
**********************************************************************
 
     double dt1(double *p,double *q,double *df)
     Double precision Initalize Approximation to
           INVerse of the cumulative T distribution
 
 
                              Function
 
 
     Returns  the  inverse   of  the T   distribution   function, i.e.,
     the integral from 0 to INVT of the T density is P. This is an
     initial approximation
 
 
                              Arguments
 
 
     P --> The p-value whose inverse from the T distribution is
          desired.
                    P is DOUBLE PRECISION
 
     Q --> 1-P.
                    Q is DOUBLE PRECISION
 
     DF --> Degrees of freedom of the T distribution.
                    DF is DOUBLE PRECISION
 
**********************************************************************
*/
{
static double coef[4][5] = {
    1.0e0,1.0e0,0.0e0,0.0e0,0.0e0,3.0e0,16.0e0,5.0e0,0.0e0,0.0e0,-15.0e0,17.0e0,
    19.0e0,3.0e0,0.0e0,-945.0e0,-1920.0e0,1482.0e0,776.0e0,79.0e0
};
static double denom[4] = {
    4.0e0,96.0e0,384.0e0,92160.0e0
};
static int ideg[4] = {
    2,3,4,5
};
static double dt1,denpow,sum,term,x,xp,xx;
static int i;
/*
     ..
     .. Executable Statements ..
*/
    x = fabs(dinvnr(p,q));
    xx = x*x;
    sum = x;
    denpow = 1.0e0;
    for(i=0; i<4; i++) {
        term = devlpl(&coef[i][0],&ideg[i],&xx)*x;
        denpow *= *df;
        sum += (term/(denpow*denom[i]));
    }
    if(!(*p >= 0.5e0)) goto S20;
    xp = sum;
    goto S30;
S20:
    xp = -sum;
S30:
    dt1 = xp;
    return dt1;
} /* END */
