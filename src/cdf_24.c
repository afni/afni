#include "cdflib.h"
void cdft(int *which,double *p,double *q,double *t,double *df,
	  int *status,double *bound)
/**********************************************************************

      void cdft(int *which,double *p,double *q,double *t,double *df,
          int *status,double *bound)

               Cumulative Distribution Function
                         T distribution


                              Function


     Calculates any one parameter of the t distribution given
     values for the others.


                              Arguments


     WHICH --> Integer indicating which  argument
               values is to be calculated from the others.
               Legal range: 1..3
               iwhich = 1 : Calculate P and Q from T and DF
               iwhich = 2 : Calculate T from P,Q and DF
               iwhich = 3 : Calculate DF from P,Q and T

        P <--> The integral from -infinity to t of the t-density.
               Input range: (0,1].

        Q <--> 1-P.
               Input range: (0, 1].
               P + Q = 1.0.

        T <--> Upper limit of integration of the t-density.
               Input range: ( -infinity, +infinity).
               Search range: [ -1E300, 1E300 ]

        DF <--> Degrees of freedom of the t-distribution.
                Input range: (0 , +infinity).
                Search range: [1e-300, 1E10]

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


     Formula  26.5.27  of   Abramowitz   and  Stegun,   Handbook   of
     Mathematical Functions  (1966) is used to reduce the computation
     of the cumulative distribution function to that of an incomplete
     beta.

     Computation of other parameters involve a seach for a value that
     produces  the desired  value  of P.   The search relies  on  the
     monotinicity of P with the other parameter.

**********************************************************************/
{
#define tol (1.0e-8)
#define atol (1.0e-50)
#define zero (1.0e-300)
#define inf 1.0e300
#define maxdf 1.0e10
static int K1 = 1;
static double K4 = 0.5e0;
static double K5 = 5.0e0;
static double fx,cum,ccum,pq;
static unsigned long qhi,qleft,qporq;
static double T2,T3,T6,T7,T8,T9,T10,T11;
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
    if(!(*p <= 0.0e0 || *p > 1.0e0)) goto S60;
    if(!(*p <= 0.0e0)) goto S40;
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
    if(*which == 3) goto S130;
/*
     DF
*/
    if(!(*df <= 0.0e0)) goto S120;
    *bound = 0.0e0;
    *status = -5;
    return;
S130:
S120:
    if(*which == 1) goto S170;
/*
     P + Q
*/
    pq = *p+*q;
    if(!(fabs(pq-0.5e0-0.5e0) > 3.0e0*spmpar(&K1))) goto S160;
    if(!(pq < 0.0e0)) goto S140;
    *bound = 0.0e0;
    goto S150;
S140:
    *bound = 1.0e0;
S150:
    *status = 3;
    return;
S170:
S160:
    if(!(*which == 1)) qporq = *p <= *q;
/*
     Select the minimum of P or Q
     Calculate ANSWERS
*/
    if(1 == *which) {
/*
     Computing P and Q
*/
        cumt(t,df,p,q);
        *status = 0;
    }
    else if(2 == *which) {
/*
     Computing T
     .. Get initial approximation for T
*/
        *t = dt1(p,q,df);
        T2 = -inf;
        T3 = inf;
        T6 = atol;
        T7 = tol;
        dstinv(&T2,&T3,&K4,&K4,&K5,&T6,&T7);
        *status = 0;
        dinvr(status,t,&fx,&qleft,&qhi);
S180:
        if(!(*status == 1)) goto S210;
        cumt(t,df,&cum,&ccum);
        if(!qporq) goto S190;
        fx = cum-*p;
        goto S200;
S190:
        fx = ccum-*q;
S200:
        dinvr(status,t,&fx,&qleft,&qhi);
        goto S180;
S210:
        if(!(*status == -1)) goto S240;
        if(!qleft) goto S220;
        *status = 1;
        *bound = -inf;
        goto S230;
S220:
        *status = 2;
        *bound = inf;
S240:
S230:
        ;
    }
    else if(3 == *which) {
/*
     Computing DF
*/
        *df = 5.0e0;
        T8 = zero;
        T9 = maxdf;
        T10 = atol;
        T11 = tol;
        dstinv(&T8,&T9,&K4,&K4,&K5,&T10,&T11);
        *status = 0;
        dinvr(status,df,&fx,&qleft,&qhi);
S250:
        if(!(*status == 1)) goto S280;
        cumt(t,df,&cum,&ccum);
        if(!qporq) goto S260;
        fx = cum-*p;
        goto S270;
S260:
        fx = ccum-*q;
S270:
        dinvr(status,df,&fx,&qleft,&qhi);
        goto S250;
S280:
        if(!(*status == -1)) goto S310;
        if(!qleft) goto S290;
        *status = 1;
        *bound = zero;
        goto S300;
S290:
        *status = 2;
        *bound = maxdf;
S300:
        ;
    }
S310:
    return;
#undef tol
#undef atol
#undef zero
#undef inf
#undef maxdf
} /* END */
