#include "cdflib.h"
double gsumln(double *a,double *b)
/*
-----------------------------------------------------------------------
          EVALUATION OF THE FUNCTION LN(GAMMA(A + B))
          FOR 1 .LE. A .LE. 2  AND  1 .LE. B .LE. 2
-----------------------------------------------------------------------
*/
{
static double gsumln,x,T1,T2;
/*
     ..
     .. Executable Statements ..
*/
    x = *a+*b-2.e0;
    if(x > 0.25e0) goto S10;
    T1 = 1.0e0+x;
    gsumln = gamln1(&T1);
    return gsumln;
S10:
    if(x > 1.25e0) goto S20;
    gsumln = gamln1(&x)+alnrel(&x);
    return gsumln;
S20:
    T2 = x-1.0e0;
    gsumln = gamln1(&T2)+log(x*(1.0e0+x));
    return gsumln;
} /* END */
