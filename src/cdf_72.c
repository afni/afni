#include "cdflib.h"
double stvaln(double *p)
/*
**********************************************************************
 
     double stvaln(double *p)
                    STarting VALue for Neton-Raphon
                calculation of Normal distribution Inverse
 
 
                              Function
 
 
     Returns X  such that CUMNOR(X)  =   P,  i.e., the  integral from -
     infinity to X of (1/SQRT(2*PI)) EXP(-U*U/2) dU is P
 
 
                              Arguments
 
 
     P --> The probability whose normal deviate is sought.
                    P is DOUBLE PRECISION
 
 
                              Method
 
 
     The  rational   function   on  page 95    of Kennedy  and  Gentle,
     Statistical Computing, Marcel Dekker, NY , 1980.
 
**********************************************************************
*/
{
static double xden[5] = {
    0.993484626060e-1,0.588581570495e0,0.531103462366e0,0.103537752850e0,
    0.38560700634e-2
};
static double xnum[5] = {
    -0.322232431088e0,-1.000000000000e0,-0.342242088547e0,-0.204231210245e-1,
    -0.453642210148e-4
};
static int K1 = 5;
static double stvaln,sign,y,z;
/*
     ..
     .. Executable Statements ..
*/
    if(!(*p <= 0.5e0)) goto S10;
    sign = -1.0e0;
    z = *p;
    goto S20;
S10:
    sign = 1.0e0;
    z = 1.0e0-*p;
S20:
    y = sqrt(-(2.0e0*log(z)));
    stvaln = y+devlpl(xnum,&K1,&y)/devlpl(xden,&K1,&y);
    stvaln = sign*stvaln;
    return stvaln;
} /* END */
