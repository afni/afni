#include "cdflib.h"
void cdfnbn(int *which,double *p,double *q,double *s,double *xn,
	    double *pr,double *ompr,int *status,double *bound)
/**********************************************************************

      void cdfnbn(int *which,double *p,double *q,double *s,double *xn,
            double *pr,double *ompr,int *status,double *bound)

               Cumulative Distribution Function
               Negative BiNomial distribution


                              Function


     Calculates any one parameter of the negative binomial
     distribution given values for the others.

     The  cumulative  negative   binomial  distribution  returns  the
     probability that there  will be  F or fewer failures before  the
     XNth success in binomial trials each of which has probability of
     success PR.

     The individual term of the negative binomial is the probability of
     S failures before XN successes and is
          Choose( S, XN+S-1 ) * PR^(XN) * (1-PR)^S


                              Arguments


     WHICH --> Integer indicating which of the next four argument
               values is to be calculated from the others.
               Legal range: 1..4
               iwhich = 1 : Calculate P and Q from S,XN,PR and OMPR
               iwhich = 2 : Calculate S from P,Q,XN,PR and OMPR
               iwhich = 3 : Calculate XN from P,Q,S,PR and OMPR
               iwhich = 4 : Calculate PR and OMPR from P,Q,S and XN

     P <--> The cumulation from 0 to S of the  negative
            binomial distribution.
            Input range: [0,1].

     Q <--> 1-P.
            Input range: (0, 1].
            P + Q = 1.0.

     S <--> The upper limit of cumulation of the binomial distribution.
            There are F or fewer failures before the XNth success.
            Input range: [0, +infinity).
            Search range: [0, 1E300]

     XN  <--> The number of successes.
              Input range: [0, +infinity).
              Search range: [0, 1E300]

     PR  <--> The probability of success in each binomial trial.
              Input range: [0,1].
              Search range: [0,1].

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


     Formula   26.5.26   of   Abramowitz  and  Stegun,  Handbook   of
     Mathematical Functions (1966) is used  to  reduce calculation of
     the cumulative distribution  function to that of  an  incomplete
     beta.

     Computation of other parameters involve a seach for a value that
     produces  the desired  value  of P.   The search relies  on  the
     monotinicity of P with the other parameter.

**********************************************************************/
{
#define tol (1.0e-8)
#define atol (1.0e-50)
#define inf 1.0e300
#define one 1.0e0
static int K1 = 1;
static double K2 = 0.0e0;
static double K4 = 0.5e0;
static double K5 = 5.0e0;
static double K11 = 1.0e0;
static double fx,xhi,xlo,pq,prompr,cum,ccum;
static unsigned long qhi,qleft,qporq;
static double T3,T6,T7,T8,T9,T10,T12,T13;
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
    if(!(*q <= 0.0e0 || *q > 1.0e0)) goto S100;
    if(!(*q <= 0.0e0)) goto S80;
    *bound = 0.0e0;
    goto S90;
S80:
    *bound = 1.0e0;
S90:
    *status = -3;
    return;
S110:
S100:
    if(*which == 2) goto S130;
/*
     S
*/
    if(!(*s < 0.0e0)) goto S120;
    *bound = 0.0e0;
    *status = -4;
    return;
S130:
S120:
    if(*which == 3) goto S150;
/*
     XN
*/
    if(!(*xn < 0.0e0)) goto S140;
    *bound = 0.0e0;
    *status = -5;
    return;
S150:
S140:
    if(*which == 4) goto S190;
/*
     PR
*/
    if(!(*pr < 0.0e0 || *pr > 1.0e0)) goto S180;
    if(!(*pr < 0.0e0)) goto S160;
    *bound = 0.0e0;
    goto S170;
S160:
    *bound = 1.0e0;
S170:
    *status = -6;
    return;
S190:
S180:
    if(*which == 4) goto S230;
/*
     OMPR
*/
    if(!(*ompr < 0.0e0 || *ompr > 1.0e0)) goto S220;
    if(!(*ompr < 0.0e0)) goto S200;
    *bound = 0.0e0;
    goto S210;
S200:
    *bound = 1.0e0;
S210:
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
    if(*which == 4) goto S310;
/*
     PR + OMPR
*/
    prompr = *pr+*ompr;
    if(!(fabs(prompr-0.5e0-0.5e0) > 3.0e0*spmpar(&K1))) goto S300;
    if(!(prompr < 0.0e0)) goto S280;
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
     Calculating P
*/
        cumnbn(s,xn,pr,ompr,p,q);
        *status = 0;
    }
    else if(2 == *which) {
/*
     Calculating S
*/
        *s = 5.0e0;
        T3 = inf;
        T6 = atol;
        T7 = tol;
        dstinv(&K2,&T3,&K4,&K4,&K5,&T6,&T7);
        *status = 0;
        dinvr(status,s,&fx,&qleft,&qhi);
S320:
        if(!(*status == 1)) goto S350;
        cumnbn(s,xn,pr,ompr,&cum,&ccum);
        if(!qporq) goto S330;
        fx = cum-*p;
        goto S340;
S330:
        fx = ccum-*q;
S340:
        dinvr(status,s,&fx,&qleft,&qhi);
        goto S320;
S350:
        if(!(*status == -1)) goto S380;
        if(!qleft) goto S360;
        *status = 1;
        *bound = 0.0e0;
        goto S370;
S360:
        *status = 2;
        *bound = inf;
S380:
S370:
        ;
    }
    else if(3 == *which) {
/*
     Calculating XN
*/
        *xn = 5.0e0;
        T8 = inf;
        T9 = atol;
        T10 = tol;
        dstinv(&K2,&T8,&K4,&K4,&K5,&T9,&T10);
        *status = 0;
        dinvr(status,xn,&fx,&qleft,&qhi);
S390:
        if(!(*status == 1)) goto S420;
        cumnbn(s,xn,pr,ompr,&cum,&ccum);
        if(!qporq) goto S400;
        fx = cum-*p;
        goto S410;
S400:
        fx = ccum-*q;
S410:
        dinvr(status,xn,&fx,&qleft,&qhi);
        goto S390;
S420:
        if(!(*status == -1)) goto S450;
        if(!qleft) goto S430;
        *status = 1;
        *bound = 0.0e0;
        goto S440;
S430:
        *status = 2;
        *bound = inf;
S450:
S440:
        ;
    }
    else if(4 == *which) {
/*
     Calculating PR and OMPR
*/
        T12 = atol;
        T13 = tol;
        dstzr(&K2,&K11,&T12,&T13);
        if(!qporq) goto S480;
        *status = 0;
        dzror(status,pr,&fx,&xlo,&xhi,&qleft,&qhi);
        *ompr = one-*pr;
S460:
        if(!(*status == 1)) goto S470;
        cumnbn(s,xn,pr,ompr,&cum,&ccum);
        fx = cum-*p;
        dzror(status,pr,&fx,&xlo,&xhi,&qleft,&qhi);
        *ompr = one-*pr;
        goto S460;
S470:
        goto S510;
S480:
        *status = 0;
        dzror(status,ompr,&fx,&xlo,&xhi,&qleft,&qhi);
        *pr = one-*ompr;
S490:
        if(!(*status == 1)) goto S500;
        cumnbn(s,xn,pr,ompr,&cum,&ccum);
        fx = ccum-*q;
        dzror(status,ompr,&fx,&xlo,&xhi,&qleft,&qhi);
        *pr = one-*ompr;
        goto S490;
S510:
S500:
        if(!(*status == -1)) goto S540;
        if(!qleft) goto S520;
        *status = 1;
        *bound = 0.0e0;
        goto S530;
S520:
        *status = 2;
        *bound = 1.0e0;
S530:
        ;
    }
S540:
    return;
#undef tol
#undef atol
#undef inf
#undef one
} /* END */
