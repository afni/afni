#include "cdflib.h"
void cumt(double *t,double *df,double *cum,double *ccum)
/*
**********************************************************************
 
     void cumt(double *t,double *df,double *cum,double *ccum)
                    CUMulative T-distribution
 
 
                              Function
 
 
     Computes the integral from -infinity to T of the t-density.
 
 
                              Arguments
 
 
     T --> Upper limit of integration of the t-density.
                                                  T is DOUBLE PRECISION
 
     DF --> Degrees of freedom of the t-distribution.
                                                  DF is DOUBLE PRECISIO
 
     CUM <-- Cumulative t-distribution.
                                                  CCUM is DOUBLE PRECIS
 
     CCUM <-- Compliment of Cumulative t-distribution.
                                                  CCUM is DOUBLE PRECIS
 
 
                              Method
 
 
     Formula 26.5.27   of     Abramowitz  and   Stegun,    Handbook  of
     Mathematical Functions  is   used   to  reduce the  t-distribution
     to an incomplete beta.
 
**********************************************************************
*/
{
static double K2 = 0.5e0;
static double xx,a,oma,tt,yy,dfptt,T1;
/*
     ..
     .. Executable Statements ..
*/
    tt = *t**t;
    dfptt = *df+tt;
    xx = *df/dfptt;
    yy = tt/dfptt;
    T1 = 0.5e0**df;
    cumbet(&xx,&yy,&T1,&K2,&a,&oma);
    if(!(*t <= 0.0e0)) goto S10;
    *cum = 0.5e0*a;
    *ccum = oma+*cum;
    goto S20;
S10:
    *ccum = 0.5e0*a;
    *cum = oma+*ccum;
S20:
    return;
} /* END */
