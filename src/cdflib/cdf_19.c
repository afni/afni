#include "cdflib.h"
void cdffnc(int *which,double *p,double *q,double *f,double *dfn,
	    double *dfd,double *phonc,int *status,double *bound)
/**********************************************************************

      void cdffnc(int *which,double *p,double *q,double *f,double *dfn,
            double *dfd,double *phonc,int *status,double *bound)

               Cumulative Distribution Function
               Non-central F distribution


                              Function


     Calculates any one parameter of the Non-central F
     distribution given values for the others.


                              Arguments


     WHICH --> Integer indicating which of the next five argument
               values is to be calculated from the others.
               Legal range: 1..5
               iwhich = 1 : Calculate P and Q from F,DFN,DFD and PNONC
               iwhich = 2 : Calculate F from P,Q,DFN,DFD and PNONC
               iwhich = 3 : Calculate DFN from P,Q,F,DFD and PNONC
               iwhich = 4 : Calculate DFD from P,Q,F,DFN and PNONC
               iwhich = 5 : Calculate PNONC from P,Q,F,DFN and DFD

       P <--> The integral from 0 to F of the non-central f-density.
              Input range: [0,1-1E-16).

       Q <--> 1-P.
              Q is not used by this subroutine and is only included
              for similarity with other cdf* routines.

       F <--> Upper limit of integration of the non-central f-density.
              Input range: [0, +infinity).
              Search range: [0,1E300]

     DFN < --> Degrees of freedom of the numerator sum of squares.
               Input range: (0, +infinity).
               Search range: [ 1E-300, 1E300]

     DFD < --> Degrees of freedom of the denominator sum of squares.
               Must be in range: (0, +infinity).
               Input range: (0, +infinity).
               Search range: [ 1E-300, 1E300]

     PNONC <-> The non-centrality parameter
               Input range: [0,infinity)
               Search range: [0,1E4]

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


     Formula  26.6.20   of   Abramowitz   and   Stegun,  Handbook  of
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

                              WARNING

     The  value  of the  cumulative  noncentral F distribution is not
     necessarily monotone in either degrees  of freedom.  There  thus
     may be two values that provide a given  CDF value.  This routine
     assumes monotonicity  and will find  an arbitrary one of the two
     values.

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
static double T2,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17;
/*
     ..
     .. Executable Statements ..
*/
/*
     Check arguments
*/
    if(!(*which < 1 || *which > 5)) goto S30;
    if(!(*which < 1)) goto S10;
    *bound = 1.0e0;
    goto S20;
S10:
    *bound = 5.0e0;
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
     F
*/
    if(!(*f < 0.0e0)) goto S80;
    *bound = 0.0e0;
    *status = -4;
    return;
S90:
S80:
    if(*which == 3) goto S110;
/*
     DFN
*/
    if(!(*dfn <= 0.0e0)) goto S100;
    *bound = 0.0e0;
    *status = -5;
    return;
S110:
S100:
    if(*which == 4) goto S130;
/*
     DFD
*/
    if(!(*dfd <= 0.0e0)) goto S120;
    *bound = 0.0e0;
    *status = -6;
    return;
S130:
S120:
    if(*which == 5) goto S150;
/*
     PHONC
*/
    if(!(*phonc < 0.0e0)) goto S140;
    *bound = 0.0e0;
    *status = -7;
    return;
S150:
S140:
/*
     Calculate ANSWERS
*/
    if(1 == *which) {
/*
     Calculating P
*/
        cumfnc(f,dfn,dfd,phonc,p,q);
        *status = 0;
    }
    else if(2 == *which) {
/*
     Calculating F
*/
        *f = 5.0e0;
        T2 = inf;
        T5 = atol;
        T6 = tol;
        dstinv(&K1,&T2,&K3,&K3,&K4,&T5,&T6);
        *status = 0;
        dinvr(status,f,&fx,&qleft,&qhi);
S160:
        if(!(*status == 1)) goto S170;
        cumfnc(f,dfn,dfd,phonc,&cum,&ccum);
        fx = cum-*p;
        dinvr(status,f,&fx,&qleft,&qhi);
        goto S160;
S170:
        if(!(*status == -1)) goto S200;
        if(!qleft) goto S180;
        *status = 1;
        *bound = 0.0e0;
        goto S190;
S180:
        *status = 2;
        *bound = inf;
S200:
S190:
        ;
    }
    else if(3 == *which) {
/*
     Calculating DFN
*/
        *dfn = 5.0e0;
        T7 = zero;
        T8 = inf;
        T9 = atol;
        T10 = tol;
        dstinv(&T7,&T8,&K3,&K3,&K4,&T9,&T10);
        *status = 0;
        dinvr(status,dfn,&fx,&qleft,&qhi);
S210:
        if(!(*status == 1)) goto S220;
        cumfnc(f,dfn,dfd,phonc,&cum,&ccum);
        fx = cum-*p;
        dinvr(status,dfn,&fx,&qleft,&qhi);
        goto S210;
S220:
        if(!(*status == -1)) goto S250;
        if(!qleft) goto S230;
        *status = 1;
        *bound = zero;
        goto S240;
S230:
        *status = 2;
        *bound = inf;
S250:
S240:
        ;
    }
    else if(4 == *which) {
/*
     Calculating DFD
*/
        *dfd = 5.0e0;
        T11 = zero;
        T12 = inf;
        T13 = atol;
        T14 = tol;
        dstinv(&T11,&T12,&K3,&K3,&K4,&T13,&T14);
        *status = 0;
        dinvr(status,dfd,&fx,&qleft,&qhi);
S260:
        if(!(*status == 1)) goto S270;
        cumfnc(f,dfn,dfd,phonc,&cum,&ccum);
        fx = cum-*p;
        dinvr(status,dfd,&fx,&qleft,&qhi);
        goto S260;
S270:
        if(!(*status == -1)) goto S300;
        if(!qleft) goto S280;
        *status = 1;
        *bound = zero;
        goto S290;
S280:
        *status = 2;
        *bound = inf;
S300:
S290:
        ;
    }
    else if(5 == *which) {
/*
     Calculating PHONC
*/
        *phonc = 5.0e0;
        T15 = tent4;
        T16 = atol;
        T17 = tol;
        dstinv(&K1,&T15,&K3,&K3,&K4,&T16,&T17);
        *status = 0;
        dinvr(status,phonc,&fx,&qleft,&qhi);
S310:
        if(!(*status == 1)) goto S320;
        cumfnc(f,dfn,dfd,phonc,&cum,&ccum);
        fx = cum-*p;
        dinvr(status,phonc,&fx,&qleft,&qhi);
        goto S310;
S320:
        if(!(*status == -1)) goto S350;
        if(!qleft) goto S330;
        *status = 1;
        *bound = 0.0e0;
        goto S340;
S330:
        *status = 2;
        *bound = tent4;
S340:
        ;
    }
S350:
    return;
#undef tent4
#undef tol
#undef atol
#undef zero
#undef one
#undef inf
} /* END */
