#include "cdflib.h"
double dbetrm(double *a,double *b)
/*
**********************************************************************
 
     double dbetrm(double *a,double *b)
          Double Precision Sterling Remainder for Complete
                    Beta Function
 
 
                              Function
 
 
     Log(Beta(A,B)) = Lgamma(A) + Lgamma(B) - Lgamma(A+B)
     where Lgamma is the log of the (complete) gamma function
 
     Let ZZ be approximation obtained if each log gamma is approximated
     by Sterling's formula, i.e.,
     Sterling(Z) = LOG( SQRT( 2*PI ) ) + ( Z-0.5 ) * LOG( Z ) - Z
 
     Returns Log(Beta(A,B)) - ZZ
 
 
                              Arguments
 
 
     A --> One argument of the Beta
                    DOUBLE PRECISION A
 
     B --> The other argument of the Beta
                    DOUBLE PRECISION B
 
**********************************************************************
*/
{
static double dbetrm,T1,T2,T3;
/*
     ..
     .. Executable Statements ..
*/
/*
     Try to sum from smallest to largest
*/
    T1 = *a+*b;
    dbetrm = -dstrem(&T1);
    T2 = fifdmax1(*a,*b);
    dbetrm += dstrem(&T2);
    T3 = fifdmin1(*a,*b);
    dbetrm += dstrem(&T3);
    return dbetrm;
} /* END */
