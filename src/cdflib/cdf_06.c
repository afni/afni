#include "cdflib.h"
double betaln(double *a0,double *b0)
/*
-----------------------------------------------------------------------
     EVALUATION OF THE LOGARITHM OF THE BETA FUNCTION
-----------------------------------------------------------------------
     E = 0.5*LN(2*PI)
--------------------------
*/
{
static double e = .918938533204673e0;
static double betaln,a,b,c,h,u,v,w,z;
static int i,n;
static double T1;
/*
     ..
     .. Executable Statements ..
*/
    a = fifdmin1(*a0,*b0);
    b = fifdmax1(*a0,*b0);
    if(a >= 8.0e0) goto S100;
    if(a >= 1.0e0) goto S20;
/*
-----------------------------------------------------------------------
                   PROCEDURE WHEN A .LT. 1
-----------------------------------------------------------------------
*/
    if(b >= 8.0e0) goto S10;
    T1 = a+b;
    betaln = gamln(&a)+(gamln(&b)-gamln(&T1));
    return betaln;
S10:
    betaln = gamln(&a)+algdiv(&a,&b);
    return betaln;
S20:
/*
-----------------------------------------------------------------------
                PROCEDURE WHEN 1 .LE. A .LT. 8
-----------------------------------------------------------------------
*/
    if(a > 2.0e0) goto S40;
    if(b > 2.0e0) goto S30;
    betaln = gamln(&a)+gamln(&b)-gsumln(&a,&b);
    return betaln;
S30:
    w = 0.0e0;
    if(b < 8.0e0) goto S60;
    betaln = gamln(&a)+algdiv(&a,&b);
    return betaln;
S40:
/*
                REDUCTION OF A WHEN B .LE. 1000
*/
    if(b > 1000.0e0) goto S80;
    n = a-1.0e0;
    w = 1.0e0;
    for(i=1; i<=n; i++) {
        a -= 1.0e0;
        h = a/b;
        w *= (h/(1.0e0+h));
    }
    w = log(w);
    if(b < 8.0e0) goto S60;
    betaln = w+gamln(&a)+algdiv(&a,&b);
    return betaln;
S60:
/*
                 REDUCTION OF B WHEN B .LT. 8
*/
    n = b-1.0e0;
    z = 1.0e0;
    for(i=1; i<=n; i++) {
        b -= 1.0e0;
        z *= (b/(a+b));
    }
    betaln = w+log(z)+(gamln(&a)+(gamln(&b)-gsumln(&a,&b)));
    return betaln;
S80:
/*
                REDUCTION OF A WHEN B .GT. 1000
*/
    n = a-1.0e0;
    w = 1.0e0;
    for(i=1; i<=n; i++) {
        a -= 1.0e0;
        w *= (a/(1.0e0+a/b));
    }
    betaln = log(w)-(double)n*log(b)+(gamln(&a)+algdiv(&a,&b));
    return betaln;
S100:
/*
-----------------------------------------------------------------------
                   PROCEDURE WHEN A .GE. 8
-----------------------------------------------------------------------
*/
    w = bcorr(&a,&b);
    h = a/b;
    c = h/(1.0e0+h);
    u = -((a-0.5e0)*log(c));
    v = b*alnrel(&h);
    if(u <= v) goto S110;
    betaln = -(0.5e0*log(b))+e+w-v-u;
    return betaln;
S110:
    betaln = -(0.5e0*log(b))+e+w-u-v;
    return betaln;
} /* END */
