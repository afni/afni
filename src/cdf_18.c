#include "cdflib.h"
void cdff(int *which,double *p,double *q,double *f,double *dfn,
	  double *dfd,int *status,double *bound)
/**********************************************************************

      void cdff(int *which,double *p,double *q,double *f,double *dfn,
          double *dfd,int *status,double *bound)

               Cumulative Distribution Function
               F distribution


                              Function


     Calculates any one parameter of the F distribution
     given values for the others.


                              Arguments


     WHICH --> Integer indicating which of the next four argument
               values is to be calculated from the others.
               Legal range: 1..4
               iwhich = 1 : Calculate P and Q from F,DFN and DFD
               iwhich = 2 : Calculate F from P,Q,DFN and DFD
               iwhich = 3 : Calculate DFN from P,Q,F and DFD
               iwhich = 4 : Calculate DFD from P,Q,F and DFN

       P <--> The integral from 0 to F of the f-density.
              Input range: [0,1].

       Q <--> 1-P.
              Input range: (0, 1].
              P + Q = 1.0.

       F <--> Upper limit of integration of the f-density.
              Input range: [0, +infinity).
              Search range: [0,1E300]

     DFN < --> Degrees of freedom of the numerator sum of squares.
               Input range: (0, +infinity).
               Search range: [ 1E-300, 1E300]

     DFD < --> Degrees of freedom of the denominator sum of squares.
               Input range: (0, +infinity).
               Search range: [ 1E-300, 1E300]

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


     Formula   26.6.2   of   Abramowitz   and   Stegun,  Handbook  of
     Mathematical  Functions (1966) is used to reduce the computation
     of the  cumulative  distribution function for the  F  variate to
     that of an incomplete beta.

     Computation of other parameters involve a seach for a value that
     produces  the desired  value  of P.   The search relies  on  the
     monotinicity of P with the other parameter.

                              WARNING

     The value of the  cumulative  F distribution is  not necessarily
     monotone in  either degrees of freedom.  There  thus may  be two
     values  that  provide a given CDF  value.   This routine assumes
     monotonicity and will find an arbitrary one of the two values.

**********************************************************************/
{
#define tol (1.0e-8)
#define atol (1.0e-50)
#define zero (1.0e-300)
#define inf 1.0e300
static int K1 = 1;
static double K2 = 0.0e0;
static double K4 = 0.5e0;
static double K5 = 5.0e0;
static double pq,fx,cum,ccum;
static unsigned long qhi,qleft,qporq;
static double T3,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15;
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
     F
*/
    if(!(*f < 0.0e0)) goto S120;
    *bound = 0.0e0;
    *status = -4;
    return;
S130:
S120:
    if(*which == 3) goto S150;
/*
     DFN
*/
    if(!(*dfn <= 0.0e0)) goto S140;
    *bound = 0.0e0;
    *status = -5;
    return;
S150:
S140:
    if(*which == 4) goto S170;
/*
     DFD
*/
    if(!(*dfd <= 0.0e0)) goto S160;
    *bound = 0.0e0;
    *status = -6;
    return;
S170:
S160:
    if(*which == 1) goto S210;
/*
     P + Q
*/
    pq = *p+*q;
    if(!(fabs(pq-0.5e0-0.5e0) > 3.0e0*spmpar(&K1))) goto S200;
    if(!(pq < 0.0e0)) goto S180;
    *bound = 0.0e0;
    goto S190;
S180:
    *bound = 1.0e0;
S190:
    *status = 3;
    return;
S210:
S200:
    if(!(*which == 1)) qporq = *p <= *q;
/*
     Select the minimum of P or Q
     Calculate ANSWERS
*/
    if(1 == *which) {
/*
     Calculating P
*/
        cumf(f,dfn,dfd,p,q);
        *status = 0;
    }
    else if(2 == *which) {
/*
     Calculating F
*/
        *f = 5.0e0;
        T3 = inf;
        T6 = atol;
        T7 = tol;
        dstinv(&K2,&T3,&K4,&K4,&K5,&T6,&T7);
        *status = 0;
        dinvr(status,f,&fx,&qleft,&qhi);
S220:
        if(!(*status == 1)) goto S250;
        cumf(f,dfn,dfd,&cum,&ccum);
        if(!qporq) goto S230;
        fx = cum-*p;
        goto S240;
S230:
        fx = ccum-*q;
S240:
        dinvr(status,f,&fx,&qleft,&qhi);
        goto S220;
S250:
        if(!(*status == -1)) goto S280;
        if(!qleft) goto S260;
        *status = 1;
        *bound = 0.0e0;
        goto S270;
S260:
        *status = 2;
        *bound = inf;
S280:
S270:
        ;
    }
    else if(3 == *which) {
/*
     Calculating DFN
*/
        *dfn = 5.0e0;
        T8 = zero;
        T9 = inf;
        T10 = atol;
        T11 = tol;
        dstinv(&T8,&T9,&K4,&K4,&K5,&T10,&T11);
        *status = 0;
        dinvr(status,dfn,&fx,&qleft,&qhi);
S290:
        if(!(*status == 1)) goto S320;
        cumf(f,dfn,dfd,&cum,&ccum);
        if(!qporq) goto S300;
        fx = cum-*p;
        goto S310;
S300:
        fx = ccum-*q;
S310:
        dinvr(status,dfn,&fx,&qleft,&qhi);
        goto S290;
S320:
        if(!(*status == -1)) goto S350;
        if(!qleft) goto S330;
        *status = 1;
        *bound = zero;
        goto S340;
S330:
        *status = 2;
        *bound = inf;
S350:
S340:
        ;
    }
    else if(4 == *which) {
/*
     Calculating DFD
*/
        *dfd = 5.0e0;
        T12 = zero;
        T13 = inf;
        T14 = atol;
        T15 = tol;
        dstinv(&T12,&T13,&K4,&K4,&K5,&T14,&T15);
        *status = 0;
        dinvr(status,dfd,&fx,&qleft,&qhi);
S360:
        if(!(*status == 1)) goto S390;
        cumf(f,dfn,dfd,&cum,&ccum);
        if(!qporq) goto S370;
        fx = cum-*p;
        goto S380;
S370:
        fx = ccum-*q;
S380:
        dinvr(status,dfd,&fx,&qleft,&qhi);
        goto S360;
S390:
        if(!(*status == -1)) goto S420;
        if(!qleft) goto S400;
        *status = 1;
        *bound = zero;
        goto S410;
S400:
        *status = 2;
        *bound = inf;
S410:
        ;
    }
S420:
    return;
#undef tol
#undef atol
#undef zero
#undef inf
} /* END */
