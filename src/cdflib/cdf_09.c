#include "cdflib.h"
double bpser(double *a,double *b,double *x,double *eps)
/*
-----------------------------------------------------------------------
     POWER SERIES EXPANSION FOR EVALUATING IX(A,B) WHEN B .LE. 1
     OR B*X .LE. 0.7.  EPS IS THE TOLERANCE USED.
-----------------------------------------------------------------------
*/
{
static double bpser,a0,apb,b0,c,n,sum,t,tol,u,w,z;
static int i,m;
/*
     ..
     .. Executable Statements ..
*/
    bpser = 0.0e0;
    if(*x == 0.0e0) return bpser;
/*
-----------------------------------------------------------------------
            COMPUTE THE FACTOR X**A/(A*BETA(A,B))
-----------------------------------------------------------------------
*/
    a0 = fifdmin1(*a,*b);
    if(a0 < 1.0e0) goto S10;
    z = *a*log(*x)-betaln(a,b);
    bpser = exp(z)/ *a;
    goto S100;
S10:
    b0 = fifdmax1(*a,*b);
    if(b0 >= 8.0e0) goto S90;
    if(b0 > 1.0e0) goto S40;
/*
            PROCEDURE FOR A0 .LT. 1 AND B0 .LE. 1
*/
    bpser = pow(*x,*a);
    if(bpser == 0.0e0) return bpser;
    apb = *a+*b;
    if(apb > 1.0e0) goto S20;
    z = 1.0e0+gam1(&apb);
    goto S30;
S20:
    u = *a+*b-1.e0;
    z = (1.0e0+gam1(&u))/apb;
S30:
    c = (1.0e0+gam1(a))*(1.0e0+gam1(b))/z;
    bpser *= (c*(*b/apb));
    goto S100;
S40:
/*
         PROCEDURE FOR A0 .LT. 1 AND 1 .LT. B0 .LT. 8
*/
    u = gamln1(&a0);
    m = b0-1.0e0;
    if(m < 1) goto S60;
    c = 1.0e0;
    for(i=1; i<=m; i++) {
        b0 -= 1.0e0;
        c *= (b0/(a0+b0));
    }
    u = log(c)+u;
S60:
    z = *a*log(*x)-u;
    b0 -= 1.0e0;
    apb = a0+b0;
    if(apb > 1.0e0) goto S70;
    t = 1.0e0+gam1(&apb);
    goto S80;
S70:
    u = a0+b0-1.e0;
    t = (1.0e0+gam1(&u))/apb;
S80:
    bpser = exp(z)*(a0/ *a)*(1.0e0+gam1(&b0))/t;
    goto S100;
S90:
/*
            PROCEDURE FOR A0 .LT. 1 AND B0 .GE. 8
*/
    u = gamln1(&a0)+algdiv(&a0,&b0);
    z = *a*log(*x)-u;
    bpser = a0/ *a*exp(z);
S100:
    if(bpser == 0.0e0 || *a <= 0.1e0**eps) return bpser;
/*
-----------------------------------------------------------------------
                     COMPUTE THE SERIES
-----------------------------------------------------------------------
*/
    sum = n = 0.0e0;
    c = 1.0e0;
    tol = *eps/ *a;
S110:
    n += 1.0e0;
    c *= ((0.5e0+(0.5e0-*b/n))**x);
    w = c/(*a+n);
    sum += w;
    if(fabs(w) > tol) goto S110;
    bpser *= (1.0e0+*a*sum);
    return bpser;
} /* END */
