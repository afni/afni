#include "cdflib.h"
double algdiv(double *a,double *b)
/*
-----------------------------------------------------------------------
 
     COMPUTATION OF LN(GAMMA(B)/GAMMA(A+B)) WHEN B .GE. 8
 
                         --------
 
     IN THIS ALGORITHM, DEL(X) IS THE FUNCTION DEFINED BY
     LN(GAMMA(X)) = (X - 0.5)*LN(X) - X + 0.5*LN(2*PI) + DEL(X).
 
-----------------------------------------------------------------------
*/
{
static double c0 = .833333333333333e-01;
static double c1 = -.277777777760991e-02;
static double c2 = .793650666825390e-03;
static double c3 = -.595202931351870e-03;
static double c4 = .837308034031215e-03;
static double c5 = -.165322962780713e-02;
static double algdiv,c,d,h,s11,s3,s5,s7,s9,t,u,v,w,x,x2,T1;
/*
     ..
     .. Executable Statements ..
*/
    if(*a <= *b) goto S10;
    h = *b/ *a;
    c = 1.0e0/(1.0e0+h);
    x = h/(1.0e0+h);
    d = *a+(*b-0.5e0);
    goto S20;
S10:
    h = *a/ *b;
    c = h/(1.0e0+h);
    x = 1.0e0/(1.0e0+h);
    d = *b+(*a-0.5e0);
S20:
/*
                SET SN = (1 - X**N)/(1 - X)
*/
    x2 = x*x;
    s3 = 1.0e0+(x+x2);
    s5 = 1.0e0+(x+x2*s3);
    s7 = 1.0e0+(x+x2*s5);
    s9 = 1.0e0+(x+x2*s7);
    s11 = 1.0e0+(x+x2*s9);
/*
                SET W = DEL(B) - DEL(A + B)
*/
    t = pow(1.0e0/ *b,2.0);
    w = ((((c5*s11*t+c4*s9)*t+c3*s7)*t+c2*s5)*t+c1*s3)*t+c0;
    w *= (c/ *b);
/*
                    COMBINE THE RESULTS
*/
    T1 = *a/ *b;
    u = d*alnrel(&T1);
    v = *a*(log(*b)-1.0e0);
    if(u <= v) goto S30;
    algdiv = w-v-u;
    return algdiv;
S30:
    algdiv = w-u-v;
    return algdiv;
} /* END */
