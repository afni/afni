#include "cdflib.h"
double devlpl(double a[],int *n,double *x)
/*
**********************************************************************
 
     double devlpl(double a[],int *n,double *x)
              Double precision EVALuate a PoLynomial at X
 
 
                              Function
 
 
     returns
          A(1) + A(2)*X + ... + A(N)*X**(N-1)
 
 
                              Arguments
 
 
     A --> Array of coefficients of the polynomial.
                                        A is DOUBLE PRECISION(N)
 
     N --> Length of A, also degree of polynomial - 1.
                                        N is INTEGER
 
     X --> Point at which the polynomial is to be evaluated.
                                        X is DOUBLE PRECISION
 
**********************************************************************
*/
{
static double devlpl,term;
static int i;
/*
     ..
     .. Executable Statements ..
*/
    term = a[*n-1];
    for(i= *n-1-1; i>=0; i--) term = a[i]+term**x;
    devlpl = term;
    return devlpl;
} /* END */
