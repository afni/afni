#include "cdflib.h"
void cumf(double *f,double *dfn,double *dfd,double *cum,double *ccum)
/*
**********************************************************************
 
     void cumf(double *f,double *dfn,double *dfd,double *cum,double *ccum)
                    CUMulative F distribution
 
 
                              Function
 
 
     Computes  the  integral from  0  to  F of  the f-density  with DFN
     and DFD degrees of freedom.
 
 
                              Arguments
 
 
     F --> Upper limit of integration of the f-density.
                                                  F is DOUBLE PRECISION
 
     DFN --> Degrees of freedom of the numerator sum of squares.
                                                  DFN is DOUBLE PRECISI
 
     DFD --> Degrees of freedom of the denominator sum of squares.
                                                  DFD is DOUBLE PRECISI
 
     CUM <-- Cumulative f distribution.
                                                  CUM is DOUBLE PRECISI
 
     CCUM <-- Compliment of Cumulative f distribution.
                                                  CCUM is DOUBLE PRECIS
 
 
                              Method
 
 
     Formula  26.5.28 of  Abramowitz and   Stegun   is  used to  reduce
     the cumulative F to a cumulative beta distribution.
 
 
                              Note
 
 
     If F is less than or equal to 0, 0 is returned.
 
**********************************************************************
*/
{
#define half 0.5e0
#define done 1.0e0
static double dsum,prod,xx,yy;
static int ierr;
static double T1,T2;
/*
     ..
     .. Executable Statements ..
*/
    if(!(*f <= 0.0e0)) goto S10;
    *cum = 0.0e0;
    *ccum = 1.0e0;
    return;
S10:
    prod = *dfn**f;
/*
     XX is such that the incomplete beta with parameters
     DFD/2 and DFN/2 evaluated at XX is 1 - CUM or CCUM
     YY is 1 - XX
     Calculate the smaller of XX and YY accurately
*/
    dsum = *dfd+prod;
    xx = *dfd/dsum;
    if(xx > half) {
        yy = prod/dsum;
        xx = done-yy;
    }
    else  yy = done-xx;
    T1 = *dfd*half;
    T2 = *dfn*half;
    bratio(&T1,&T2,&xx,&yy,ccum,cum,&ierr);
    return;
#undef half
#undef done
} /* END */
