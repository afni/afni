#include "cdflib.h"
void cdfbin(int *which,double *p,double *q,double *s,double *xn,
	    double *pr,double *ompr,int *status,double *bound)
/**********************************************************************

      void cdfbin(int *which,double *p,double *q,double *s,double *xn,
            double *pr,double *ompr,int *status,double *bound)

               Cumulative Distribution Function
                         BINomial distribution


                              Function


     Calculates any one parameter of the binomial
     distribution given values for the others.


                              Arguments


     WHICH --> Integer indicating which of the next four argument
               values is to be calculated from the others.
               Legal range: 1..4
               iwhich = 1 : Calculate P and Q from S,XN,PR and OMPR
               iwhich = 2 : Calculate S from P,Q,XN,PR and OMPR
               iwhich = 3 : Calculate XN from P,Q,S,PR and OMPR
               iwhich = 4 : Calculate PR and OMPR from P,Q,S and XN

     P <--> The cumulation from 0 to S of the binomial distribution.
            (Probablility of S or fewer successes in XN trials each
            with probability of success PR.)
            Input range: [0,1].

     Q <--> 1-P.
            Input range: [0, 1].
            P + Q = 1.0.

     S <--> The number of successes observed.
            Input range: [0, XN]
            Search range: [0, XN]

     XN  <--> The number of binomial trials.
              Input range: (0, +infinity).
              Search range: [1E-300, 1E300]

     PR  <--> The probability of success in each binomial trial.
              Input range: [0,1].
              Search range: [0,1]

     OMPR  <--> 1-PR
              Input range: [0,1].
              Search range: [0,1]
              PR + OMPR = 1.0

     STATUS <-- 0 if calculation completed correctly
               -I if input parameter number I is out of range
                1 if answer appears to be lower than lowest
                  search bound
                2 if answer appears to be higher than greatest
                  search bound
                3 if P + Q .ne. 1
                4 if PR + OMPR .ne. 1

     BOUND <-- Undefined if STATUS is 0

               Bound exceeded by parameter number I if STATUS
               is negative.

               Lower search bound if STATUS is 1.

               Upper search bound if STATUS is 2.


                              Method


     Formula  26.5.24    of   Abramowitz  and    Stegun,  Handbook   of
     Mathematical   Functions (1966) is   used  to reduce the  binomial
     distribution  to  the  cumulative incomplete    beta distribution.

     Computation of other parameters involve a seach for a value that
     produces  the desired  value  of P.   The search relies  on  the
     monotinicity of P with the other parameter.


**********************************************************************/
{
#define atol (1.0e-50)
#define tol (1.0e-8)
#define zero (1.0e-300)
#define inf 1.0e300
#define one 1.0e0
static int K1 = 1;
static double K2 = 0.0e0;
static double K3 = 0.5e0;
static double K4 = 5.0e0;
static double K11 = 1.0e0;
static double fx,xhi,xlo,cum,ccum,pq,prompr;
static unsigned long qhi,qleft,qporq;
static double T5,T6,T7,T8,T9,T10,T12,T13;
/*
     ..
     .. Executable Statements ..
*/
/*
     Check arguments
*/
    if(!(*which < 1 && *which > 4)) goto S30;
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
    if(*which == 3) goto S130;
/*
     XN
*/
    if(!(*xn <= 0.0e0)) goto S120;
    *bound = 0.0e0;
    *status = -5;
    return;
S130:
S120:
    if(*which == 2) goto S170;
/*
     S
*/
    if(!(*s < 0.0e0 || *which != 3 && *s > *xn)) goto S160;
    if(!(*s < 0.0e0)) goto S140;
    *bound = 0.0e0;
    goto S150;
S140:
    *bound = *xn;
S150:
    *status = -4;
    return;
S170:
S160:
    if(*which == 4) goto S210;
/*
     PR
*/
    if(!(*pr < 0.0e0 || *pr > 1.0e0)) goto S200;
    if(!(*pr < 0.0e0)) goto S180;
    *bound = 0.0e0;
    goto S190;
S180:
    *bound = 1.0e0;
S190:
    *status = -6;
    return;
S210:
S200:
    if(*which == 4) goto S250;
/*
     OMPR
*/
    if(!(*ompr < 0.0e0 || *ompr > 1.0e0)) goto S240;
    if(!(*ompr < 0.0e0)) goto S220;
    *bound = 0.0e0;
    goto S230;
S220:
    *bound = 1.0e0;
S230:
    *status = -7;
    return;
S250:
S240:
    if(*which == 1) goto S290;
/*
     P + Q
*/
    pq = *p+*q;
    if(!(fabs(pq-0.5e0-0.5e0) > 3.0e0*spmpar(&K1))) goto S280;
    if(!(pq < 0.0e0)) goto S260;
    *bound = 0.0e0;
    goto S270;
S260:
    *bound = 1.0e0;
S270:
    *status = 3;
    return;
S290:
S280:
    if(*which == 4) goto S330;
/*
     PR + OMPR
*/
    prompr = *pr+*ompr;
    if(!(fabs(prompr-0.5e0-0.5e0) > 3.0e0*spmpar(&K1))) goto S320;
    if(!(prompr < 0.0e0)) goto S300;
    *bound = 0.0e0;
    goto S310;
S300:
    *bound = 1.0e0;
S310:
    *status = 4;
    return;
S330:
S320:
    if(!(*which == 1)) qporq = *p <= *q;
/*
     Select the minimum of P or Q
     Calculate ANSWERS
*/
    if(1 == *which) {
/*
     Calculating P
*/
        cumbin(s,xn,pr,ompr,p,q);
        *status = 0;
    }
    else if(2 == *which) {
/*
     Calculating S
*/
        *s = 5.0e0;
        T5 = atol;
        T6 = tol;
        dstinv(&K2,xn,&K3,&K3,&K4,&T5,&T6);
        *status = 0;
        dinvr(status,s,&fx,&qleft,&qhi);
S340:
        if(!(*status == 1)) goto S370;
        cumbin(s,xn,pr,ompr,&cum,&ccum);
        if(!qporq) goto S350;
        fx = cum-*p;
        goto S360;
S350:
        fx = ccum-*q;
S360:
        dinvr(status,s,&fx,&qleft,&qhi);
        goto S340;
S370:
        if(!(*status == -1)) goto S400;
        if(!qleft) goto S380;
        *status = 1;
        *bound = 0.0e0;
        goto S390;
S380:
        *status = 2;
        *bound = *xn;
S400:
S390:
        ;
    }
    else if(3 == *which) {
/*
     Calculating XN
*/
        *xn = 5.0e0;
        T7 = zero;
        T8 = inf;
        T9 = atol;
        T10 = tol;
        dstinv(&T7,&T8,&K3,&K3,&K4,&T9,&T10);
        *status = 0;
        dinvr(status,xn,&fx,&qleft,&qhi);
S410:
        if(!(*status == 1)) goto S440;
        cumbin(s,xn,pr,ompr,&cum,&ccum);
        if(!qporq) goto S420;
        fx = cum-*p;
        goto S430;
S420:
        fx = ccum-*q;
S430:
        dinvr(status,xn,&fx,&qleft,&qhi);
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
     Calculating PR and OMPR
*/
        T12 = atol;
        T13 = tol;
        dstzr(&K2,&K11,&T12,&T13);
        if(!qporq) goto S500;
        *status = 0;
        dzror(status,pr,&fx,&xlo,&xhi,&qleft,&qhi);
        *ompr = one-*pr;
S480:
        if(!(*status == 1)) goto S490;
        cumbin(s,xn,pr,ompr,&cum,&ccum);
        fx = cum-*p;
        dzror(status,pr,&fx,&xlo,&xhi,&qleft,&qhi);
        *ompr = one-*pr;
        goto S480;
S490:
        goto S530;
S500:
        *status = 0;
        dzror(status,ompr,&fx,&xlo,&xhi,&qleft,&qhi);
        *pr = one-*ompr;
S510:
        if(!(*status == 1)) goto S520;
        cumbin(s,xn,pr,ompr,&cum,&ccum);
        fx = ccum-*q;
        dzror(status,ompr,&fx,&xlo,&xhi,&qleft,&qhi);
        *pr = one-*ompr;
        goto S510;
S530:
S520:
        if(!(*status == -1)) goto S560;
        if(!qleft) goto S540;
        *status = 1;
        *bound = 0.0e0;
        goto S550;
S540:
        *status = 2;
        *bound = 1.0e0;
S550:
        ;
    }
S560:
    return;
#undef atol
#undef tol
#undef zero
#undef inf
#undef one
} /* END */
