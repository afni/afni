#include "cdflib.h"
void cdfpoi(int *which,double *p,double *q,double *s,double *xlam,
	    int *status,double *bound)
/**********************************************************************

      void cdfpoi(int *which,double *p,double *q,double *s,double *xlam,
            int *status,double *bound)

               Cumulative Distribution Function
               POIsson distribution


                              Function


     Calculates any one parameter of the Poisson
     distribution given values for the others.


                              Arguments


     WHICH --> Integer indicating which  argument
               value is to be calculated from the others.
               Legal range: 1..3
               iwhich = 1 : Calculate P and Q from S and XLAM
               iwhich = 2 : Calculate A from P,Q and XLAM
               iwhich = 3 : Calculate XLAM from P,Q and S

        P <--> The cumulation from 0 to S of the poisson density.
               Input range: [0,1].

        Q <--> 1-P.
               Input range: (0, 1].
               P + Q = 1.0.

        S <--> Upper limit of cumulation of the Poisson.
               Input range: [0, +infinity).
               Search range: [0,1E300]

     XLAM <--> Mean of the Poisson distribution.
               Input range: [0, +infinity).
               Search range: [0,1E300]

     STATUS <-- 0 if calculation completed correctly
               -I if input parameter number I is out of range
                1 if answer appears to be lower than lowest
                  search bound
                2 if answer appears to be higher than greatest
                  search bound
                3 if P + Q .ne. 1

     BOUND <-- Undefined if STATUS is 0

               Bound exceeded by parameter number I if STATUS
               is negative.

               Lower search bound if STATUS is 1.

               Upper search bound if STATUS is 2.


                              Method


     Formula   26.4.21  of   Abramowitz  and   Stegun,   Handbook  of
     Mathematical Functions (1966) is used  to reduce the computation
     of  the cumulative distribution function to that  of computing a
     chi-square, hence an incomplete gamma function.

     Cumulative  distribution function  (P) is  calculated  directly.
     Computation of other parameters involve a seach for a value that
     produces  the desired value of  P.   The  search relies  on  the
     monotinicity of P with the other parameter.

**********************************************************************/
{
#define tol (1.0e-8)
#define atol (1.0e-50)
#define inf 1.0e300
static int K1 = 1;
static double K2 = 0.0e0;
static double K4 = 0.5e0;
static double K5 = 5.0e0;
static double fx,cum,ccum,pq;
static unsigned long qhi,qleft,qporq;
static double T3,T6,T7,T8,T9,T10;
/*
     ..
     .. Executable Statements ..
*/
/*
     Check arguments
*/
    if(!(*which < 1 || *which > 3)) goto S30;
    if(!(*which < 1)) goto S10;
    *bound = 1.0e0;
    goto S20;
S10:
    *bound = 3.0e0;
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
     XLAM
*/
    if(!(*xlam < 0.0e0)) goto S140;
    *bound = 0.0e0;
    *status = -5;
    return;
S150:
S140:
    if(*which == 1) goto S190;
/*
     P + Q
*/
    pq = *p+*q;
    if(!(fabs(pq-0.5e0-0.5e0) > 3.0e0*spmpar(&K1))) goto S180;
    if(!(pq < 0.0e0)) goto S160;
    *bound = 0.0e0;
    goto S170;
S160:
    *bound = 1.0e0;
S170:
    *status = 3;
    return;
S190:
S180:
    if(!(*which == 1)) qporq = *p <= *q;
/*
     Select the minimum of P or Q
     Calculate ANSWERS
*/
    if(1 == *which) {
/*
     Calculating P
*/
        cumpoi(s,xlam,p,q);
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
S200:
        if(!(*status == 1)) goto S230;
        cumpoi(s,xlam,&cum,&ccum);
        if(!qporq) goto S210;
        fx = cum-*p;
        goto S220;
S210:
        fx = ccum-*q;
S220:
        dinvr(status,s,&fx,&qleft,&qhi);
        goto S200;
S230:
        if(!(*status == -1)) goto S260;
        if(!qleft) goto S240;
        *status = 1;
        *bound = 0.0e0;
        goto S250;
S240:
        *status = 2;
        *bound = inf;
S260:
S250:
        ;
    }
    else if(3 == *which) {
/*
     Calculating XLAM
*/
        *xlam = 5.0e0;
        T8 = inf;
        T9 = atol;
        T10 = tol;
        dstinv(&K2,&T8,&K4,&K4,&K5,&T9,&T10);
        *status = 0;
        dinvr(status,xlam,&fx,&qleft,&qhi);
S270:
        if(!(*status == 1)) goto S300;
        cumpoi(s,xlam,&cum,&ccum);
        if(!qporq) goto S280;
        fx = cum-*p;
        goto S290;
S280:
        fx = ccum-*q;
S290:
        dinvr(status,xlam,&fx,&qleft,&qhi);
        goto S270;
S300:
        if(!(*status == -1)) goto S330;
        if(!qleft) goto S310;
        *status = 1;
        *bound = 0.0e0;
        goto S320;
S310:
        *status = 2;
        *bound = inf;
S320:
        ;
    }
S330:
    return;
#undef tol
#undef atol
#undef inf
} /* END */
