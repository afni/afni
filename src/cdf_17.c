#include "cdflib.h"
void cdfchn(int *which,double *p,double *q,double *x,double *df,
	    double *pnonc,int *status,double *bound)
/**********************************************************************

      void cdfchn(int *which,double *p,double *q,double *x,double *df,
            double *pnonc,int *status,double *bound)

               Cumulative Distribution Function
               Non-central Chi-Square


                              Function


     Calculates any one parameter of the non-central chi-square
     distribution given values for the others.


                              Arguments


     WHICH --> Integer indicating which of the next three argument
               values is to be calculated from the others.
               Input range: 1..4
               iwhich = 1 : Calculate P and Q from X and DF
               iwhich = 2 : Calculate X from P,DF and PNONC
               iwhich = 3 : Calculate DF from P,X and PNONC
               iwhich = 3 : Calculate PNONC from P,X and DF

     P <--> The integral from 0 to X of the non-central chi-square
            distribution.
            Input range: [0, 1-1E-16).

     Q <--> 1-P.
            Q is not used by this subroutine and is only included
            for similarity with other cdf* routines.

     X <--> Upper limit of integration of the non-central
            chi-square distribution.
            Input range: [0, +infinity).
            Search range: [0,1E300]

     DF <--> Degrees of freedom of the non-central
             chi-square distribution.
             Input range: (0, +infinity).
             Search range: [ 1E-300, 1E300]

     PNONC <--> Non-centrality parameter of the non-central
                chi-square distribution.
                Input range: [0, +infinity).
                Search range: [0,1E4]

     STATUS <-- 0 if calculation completed correctly
               -I if input parameter number I is out of range
                1 if answer appears to be lower than lowest
                  search bound
                2 if answer appears to be higher than greatest
                  search bound

     BOUND <-- Undefined if STATUS is 0

               Bound exceeded by parameter number I if STATUS
               is negative.

               Lower search bound if STATUS is 1.

               Upper search bound if STATUS is 2.


                              Method


     Formula  26.4.25   of   Abramowitz   and   Stegun,  Handbook  of
     Mathematical  Functions (1966) is used to compute the cumulative
     distribution function.

     Computation of other parameters involve a seach for a value that
     produces  the desired  value  of P.   The search relies  on  the
     monotinicity of P with the other parameter.


                            WARNING

     The computation time  required for this  routine is proportional
     to the noncentrality  parameter  (PNONC).  Very large  values of
     this parameter can consume immense  computer resources.  This is
     why the search range is bounded by 10,000.

**********************************************************************/
{
#define tent4 1.0e4
#define tol (1.0e-8)
#define atol (1.0e-50)
#define zero (1.0e-300)
#define one (1.0e0-1.0e-16)
#define inf 1.0e300
static double K1 = 0.0e0;
static double K3 = 0.5e0;
static double K4 = 5.0e0;
static double fx,cum,ccum;
static unsigned long qhi,qleft;
static double T2,T5,T6,T7,T8,T9,T10,T11,T12,T13;
/*
     ..
     .. Executable Statements ..
*/
/*
     Check arguments
*/
    if(!(*which < 1 || *which > 4)) goto S30;
    if(!(*which < 1)) goto S10;
    *bound = 1.0e0;
    goto S20;
S10:
    *bound = 4.0e0;
S20:
    *status = -1;
    return;
S30:
    if(*which == 1) goto S70;
/*
     P
*/
    if(!(*p < 0.0e0 || *p > one)) goto S60;
    if(!(*p < 0.0e0)) goto S40;
    *bound = 0.0e0;
    goto S50;
S40:
    *bound = one;
S50:
    *status = -2;
    return;
S70:
S60:
    if(*which == 2) goto S90;
/*
     X
*/
    if(!(*x < 0.0e0)) goto S80;
    *bound = 0.0e0;
    *status = -4;
    return;
S90:
S80:
    if(*which == 3) goto S110;
/*
     DF
*/
    if(!(*df <= 0.0e0)) goto S100;
    *bound = 0.0e0;
    *status = -5;
    return;
S110:
S100:
    if(*which == 4) goto S130;
/*
     PNONC
*/
    if(!(*pnonc < 0.0e0)) goto S120;
    *bound = 0.0e0;
    *status = -6;
    return;
S130:
S120:
/*
     Calculate ANSWERS
*/
    if(1 == *which) {
/*
     Calculating P and Q
*/
        cumchn(x,df,pnonc,p,q);
        *status = 0;
    }
    else if(2 == *which) {
/*
     Calculating X
*/
        *x = 5.0e0;
        T2 = inf;
        T5 = atol;
        T6 = tol;
        dstinv(&K1,&T2,&K3,&K3,&K4,&T5,&T6);
        *status = 0;
        dinvr(status,x,&fx,&qleft,&qhi);
S140:
        if(!(*status == 1)) goto S150;
        cumchn(x,df,pnonc,&cum,&ccum);
        fx = cum-*p;
        dinvr(status,x,&fx,&qleft,&qhi);
        goto S140;
S150:
        if(!(*status == -1)) goto S180;
        if(!qleft) goto S160;
        *status = 1;
        *bound = 0.0e0;
        goto S170;
S160:
        *status = 2;
        *bound = inf;
S180:
S170:
        ;
    }
    else if(3 == *which) {
/*
     Calculating DF
*/
        *df = 5.0e0;
        T7 = zero;
        T8 = inf;
        T9 = atol;
        T10 = tol;
        dstinv(&T7,&T8,&K3,&K3,&K4,&T9,&T10);
        *status = 0;
        dinvr(status,df,&fx,&qleft,&qhi);
S190:
        if(!(*status == 1)) goto S200;
        cumchn(x,df,pnonc,&cum,&ccum);
        fx = cum-*p;
        dinvr(status,df,&fx,&qleft,&qhi);
        goto S190;
S200:
        if(!(*status == -1)) goto S230;
        if(!qleft) goto S210;
        *status = 1;
        *bound = zero;
        goto S220;
S210:
        *status = 2;
        *bound = inf;
S230:
S220:
        ;
    }
    else if(4 == *which) {
/*
     Calculating PNONC
*/
        *pnonc = 5.0e0;
        T11 = tent4;
        T12 = atol;
        T13 = tol;
        dstinv(&K1,&T11,&K3,&K3,&K4,&T12,&T13);
        *status = 0;
        dinvr(status,pnonc,&fx,&qleft,&qhi);
S240:
        if(!(*status == 1)) goto S250;
        cumchn(x,df,pnonc,&cum,&ccum);
        fx = cum-*p;
        dinvr(status,pnonc,&fx,&qleft,&qhi);
        goto S240;
S250:
        if(!(*status == -1)) goto S280;
        if(!qleft) goto S260;
        *status = 1;
        *bound = zero;
        goto S270;
S260:
        *status = 2;
        *bound = tent4;
S270:
        ;
    }
S280:
    return;
#undef tent4
#undef tol
#undef atol
#undef zero
#undef one
#undef inf
} /* END */
