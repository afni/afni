#include "cdflib.h"
void cdfbet(int *which,double *p,double *q,double *x,double *y,
	    double *a,double *b,int *status,double *bound)
/**********************************************************************

      void cdfbet(int *which,double *p,double *q,double *x,double *y,
            double *a,double *b,int *status,double *bound)

               Cumulative Distribution Function
                         BETa Distribution


                              Function


     Calculates any one parameter of the beta distribution given
     values for the others.


                              Arguments


     WHICH --> Integer indicating which of the next four argument
               values is to be calculated from the others.
               Legal range: 1..4
               iwhich = 1 : Calculate P and Q from X,Y,A and B
               iwhich = 2 : Calculate X and Y from P,Q,A and B
               iwhich = 3 : Calculate A from P,Q,X,Y and B
               iwhich = 4 : Calculate B from P,Q,X,Y and A

     P <--> The integral from 0 to X of the chi-square
            distribution.
            Input range: [0, 1].

     Q <--> 1-P.
            Input range: [0, 1].
            P + Q = 1.0.

     X <--> Upper limit of integration of beta density.
            Input range: [0,1].
            Search range: [0,1]

     Y <--> 1-X.
            Input range: [0,1].
            Search range: [0,1]
            X + Y = 1.0.

     A <--> The first parameter of the beta density.
            Input range: (0, +infinity).
            Search range: [1D-300,1D300]

     B <--> The second parameter of the beta density.
            Input range: (0, +infinity).
            Search range: [1D-300,1D300]

     STATUS <-- 0 if calculation completed correctly
               -I if input parameter number I is out of range
                1 if answer appears to be lower than lowest
                  search bound
                2 if answer appears to be higher than greatest
                  search bound
                3 if P + Q .ne. 1
                4 if X + Y .ne. 1

     BOUND <-- Undefined if STATUS is 0

               Bound exceeded by parameter number I if STATUS
               is negative.

               Lower search bound if STATUS is 1.

               Upper search bound if STATUS is 2.


                              Method


     Cumulative distribution function  (P)  is calculated directly by
     code associated with the following reference.

     DiDinato, A. R. and Morris,  A.   H.  Algorithm 708: Significant
     Digit Computation of the Incomplete  Beta  Function Ratios.  ACM
     Trans. Math.  Softw. 18 (1993), 360-373.

     Computation of other parameters involve a seach for a value that
     produces  the desired  value  of P.   The search relies  on  the
     monotinicity of P with the other parameter.


                              Note


     The beta density is proportional to
               t^(A-1) * (1-t)^(B-1)

**********************************************************************/
{
#define tol (1.0e-8)
#define atol (1.0e-50)
#define zero (1.0e-300)
#define inf 1.0e300
#define one 1.0e0
static int K1 = 1;
static double K2 = 0.0e0;
static double K3 = 1.0e0;
static double K8 = 0.5e0;
static double K9 = 5.0e0;
static double fx,xhi,xlo,cum,ccum,xy,pq;
static unsigned long qhi,qleft,qporq;
static double T4,T5,T6,T7,T10,T11,T12,T13,T14,T15;
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
    if(!(*p < 0.0e0 || *p > 1.0e0)) goto S60;
    if(!(*p < 0.0e0)) goto S40;
    *bound = 0.0e0;
    goto S50;
S40:
    *bound = 1.0e0;
S50:
    *status = -2;
    return;
S70:
S60:
    if(*which == 1) goto S110;
/*
     Q
*/
    if(!(*q < 0.0e0 || *q > 1.0e0)) goto S100;
    if(!(*q < 0.0e0)) goto S80;
    *bound = 0.0e0;
    goto S90;
S80:
    *bound = 1.0e0;
S90:
    *status = -3;
    return;
S110:
S100:
    if(*which == 2) goto S150;
/*
     X
*/
    if(!(*x < 0.0e0 || *x > 1.0e0)) goto S140;
    if(!(*x < 0.0e0)) goto S120;
    *bound = 0.0e0;
    goto S130;
S120:
    *bound = 1.0e0;
S130:
    *status = -4;
    return;
S150:
S140:
    if(*which == 2) goto S190;
/*
     Y
*/
    if(!(*y < 0.0e0 || *y > 1.0e0)) goto S180;
    if(!(*y < 0.0e0)) goto S160;
    *bound = 0.0e0;
    goto S170;
S160:
    *bound = 1.0e0;
S170:
    *status = -5;
    return;
S190:
S180:
    if(*which == 3) goto S210;
/*
     A
*/
    if(!(*a <= 0.0e0)) goto S200;
    *bound = 0.0e0;
    *status = -6;
    return;
S210:
S200:
    if(*which == 4) goto S230;
/*
     B
*/
    if(!(*b <= 0.0e0)) goto S220;
    *bound = 0.0e0;
    *status = -7;
    return;
S230:
S220:
    if(*which == 1) goto S270;
/*
     P + Q
*/
    pq = *p+*q;
    if(!(fabs(pq-0.5e0-0.5e0) > 3.0e0*spmpar(&K1))) goto S260;
    if(!(pq < 0.0e0)) goto S240;
    *bound = 0.0e0;
    goto S250;
S240:
    *bound = 1.0e0;
S250:
    *status = 3;
    return;
S270:
S260:
    if(*which == 2) goto S310;
/*
     X + Y
*/
    xy = *x+*y;
    if(!(fabs(xy-0.5e0-0.5e0) > 3.0e0*spmpar(&K1))) goto S300;
    if(!(xy < 0.0e0)) goto S280;
    *bound = 0.0e0;
    goto S290;
S280:
    *bound = 1.0e0;
S290:
    *status = 4;
    return;
S310:
S300:
    if(!(*which == 1)) qporq = *p <= *q;
/*
     Select the minimum of P or Q
     Calculate ANSWERS
*/
    if(1 == *which) {
/*
     Calculating P and Q
*/
        cumbet(x,y,a,b,p,q);
        *status = 0;
    }
    else if(2 == *which) {
/*
     Calculating X and Y
*/
        T4 = atol;
        T5 = tol;
        dstzr(&K2,&K3,&T4,&T5);
        if(!qporq) goto S340;
        *status = 0;
        dzror(status,x,&fx,&xlo,&xhi,&qleft,&qhi);
        *y = one-*x;
S320:
        if(!(*status == 1)) goto S330;
        cumbet(x,y,a,b,&cum,&ccum);
        fx = cum-*p;
        dzror(status,x,&fx,&xlo,&xhi,&qleft,&qhi);
        *y = one-*x;
        goto S320;
S330:
        goto S370;
S340:
        *status = 0;
        dzror(status,y,&fx,&xlo,&xhi,&qleft,&qhi);
        *x = one-*y;
S350:
        if(!(*status == 1)) goto S360;
        cumbet(x,y,a,b,&cum,&ccum);
        fx = ccum-*q;
        dzror(status,y,&fx,&xlo,&xhi,&qleft,&qhi);
        *x = one-*y;
        goto S350;
S370:
S360:
        if(!(*status == -1)) goto S400;
        if(!qleft) goto S380;
        *status = 1;
        *bound = 0.0e0;
        goto S390;
S380:
        *status = 2;
        *bound = 1.0e0;
S400:
S390:
        ;
    }
    else if(3 == *which) {
/*
     Computing A
*/
        *a = 5.0e0;
        T6 = zero;
        T7 = inf;
        T10 = atol;
        T11 = tol;
        dstinv(&T6,&T7,&K8,&K8,&K9,&T10,&T11);
        *status = 0;
        dinvr(status,a,&fx,&qleft,&qhi);
S410:
        if(!(*status == 1)) goto S440;
        cumbet(x,y,a,b,&cum,&ccum);
        if(!qporq) goto S420;
        fx = cum-*p;
        goto S430;
S420:
        fx = ccum-*q;
S430:
        dinvr(status,a,&fx,&qleft,&qhi);
        goto S410;
S440:
        if(!(*status == -1)) goto S470;
        if(!qleft) goto S450;
        *status = 1;
        *bound = zero;
        goto S460;
S450:
        *status = 2;
        *bound = inf;
S470:
S460:
        ;
    }
    else if(4 == *which) {
/*
     Computing B
*/
        *b = 5.0e0;
        T12 = zero;
        T13 = inf;
        T14 = atol;
        T15 = tol;
        dstinv(&T12,&T13,&K8,&K8,&K9,&T14,&T15);
        *status = 0;
        dinvr(status,b,&fx,&qleft,&qhi);
S480:
        if(!(*status == 1)) goto S510;
        cumbet(x,y,a,b,&cum,&ccum);
        if(!qporq) goto S490;
        fx = cum-*p;
        goto S500;
S490:
        fx = ccum-*q;
S500:
        dinvr(status,b,&fx,&qleft,&qhi);
        goto S480;
S510:
        if(!(*status == -1)) goto S540;
        if(!qleft) goto S520;
        *status = 1;
        *bound = zero;
        goto S530;
S520:
        *status = 2;
        *bound = inf;
S530:
        ;
    }
S540:
    return;
#undef tol
#undef atol
#undef zero
#undef inf
#undef one
} /* END */
