#include "cdflib.h"
double bcorr(double *a0,double *b0)
/*
-----------------------------------------------------------------------
 
     EVALUATION OF  DEL(A0) + DEL(B0) - DEL(A0 + B0)  WHERE
     LN(GAMMA(A)) = (A - 0.5)*LN(A) - A + 0.5*LN(2*PI) + DEL(A).
     IT IS ASSUMED THAT A0 .GE. 8 AND B0 .GE. 8.
 
-----------------------------------------------------------------------
*/
{
static double c0 = .833333333333333e-01;
static double c1 = -.277777777760991e-02;
static double c2 = .793650666825390e-03;
static double c3 = -.595202931351870e-03;
static double c4 = .837308034031215e-03;
static double c5 = -.165322962780713e-02;
static double bcorr,a,b,c,h,s11,s3,s5,s7,s9,t,w,x,x2;
/*
     ..
     .. Executable Statements ..
*/
    a = fifdmin1(*a0,*b0);
    b = fifdmax1(*a0,*b0);
    h = a/b;
    c = h/(1.0e0+h);
    x = 1.0e0/(1.0e0+h);
    x2 = x*x;
/*
                SET SN = (1 - X**N)/(1 - X)
*/
    s3 = 1.0e0+(x+x2);
    s5 = 1.0e0+(x+x2*s3);
    s7 = 1.0e0+(x+x2*s5);
    s9 = 1.0e0+(x+x2*s7);
    s11 = 1.0e0+(x+x2*s9);
/*
                SET W = DEL(B) - DEL(A + B)
*/
    t = pow(1.0e0/b,2.0);
    w = ((((c5*s11*t+c4*s9)*t+c3*s7)*t+c2*s5)*t+c1*s3)*t+c0;
    w *= (c/b);
/*
                   COMPUTE  DEL(A) + W
*/
    t = pow(1.0e0/a,2.0);
    bcorr = (((((c5*t+c4)*t+c3)*t+c2)*t+c1)*t+c0)/a+w;
    return bcorr;
} /* END */
