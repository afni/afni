#include "cdflib.h"
void cdfgam(int *which,double *p,double *q,double *x,double *shape,
	    double *scale,int *status,double *bound)
/**********************************************************************

      void cdfgam(int *which,double *p,double *q,double *x,double *shape,
            double *scale,int *status,double *bound)

               Cumulative Distribution Function
                         GAMma Distribution


                              Function


     Calculates any one parameter of the gamma
     distribution given values for the others.


                              Arguments


     WHICH --> Integer indicating which of the next four argument
               values is to be calculated from the others.
               Legal range: 1..4
               iwhich = 1 : Calculate P and Q from X,SHAPE and SCALE
               iwhich = 2 : Calculate X from P,Q,SHAPE and SCALE
               iwhich = 3 : Calculate SHAPE from P,Q,X and SCALE
               iwhich = 4 : Calculate SCALE from P,Q,X and SHAPE

     P <--> The integral from 0 to X of the gamma density.
            Input range: [0,1].

     Q <--> 1-P.
            Input range: (0, 1].
            P + Q = 1.0.

     X <--> The upper limit of integration of the gamma density.
            Input range: [0, +infinity).
            Search range: [0,1E300]

     SHAPE <--> The shape parameter of the gamma density.
                Input range: (0, +infinity).
                Search range: [1E-300,1E300]

     SCALE <--> The scale parameter of the gamma density.
                Input range: (0, +infinity).
                Search range: (1E-300,1E300]

     STATUS <-- 0 if calculation completed correctly
               -I if input parameter number I is out of range
                1 if answer appears to be lower than lowest
                  search bound
                2 if answer appears to be higher than greatest
                  search bound
                3 if P + Q .ne. 1
                10 if the gamma or inverse gamma routine cannot
                   compute the answer.  Usually happens only for
                   X and SHAPE very large (gt 1E10 or more)

     BOUND <-- Undefined if STATUS is 0

               Bound exceeded by parameter number I if STATUS
               is negative.

               Lower search bound if STATUS is 1.

               Upper search bound if STATUS is 2.


                              Method


     Cumulative distribution function (P) is calculated directly by
     the code associated with:

     DiDinato, A. R. and Morris, A. H. Computation of the  incomplete
     gamma function  ratios  and their  inverse.   ACM  Trans.  Math.
     Softw. 12 (1986), 377-393.

     Computation of other parameters involve a seach for a value that
     produces  the desired  value  of P.   The search relies  on  the
     monotinicity of P with the other parameter.


                              Note



     The gamma density is proportional to
       T**(SHAPE - 1) * EXP(- SCALE * T)

**********************************************************************/
{
#define tol (1.0e-8)
#define atol (1.0e-50)
#define zero (1.0e-300)
#define inf 1.0e300
static int K1 = 1;
static double K5 = 0.5e0;
static double K6 = 5.0e0;
static double xx,fx,xscale,cum,ccum,pq,porq;
static int ierr;
static unsigned long qhi,qleft,qporq;
static double T2,T3,T4,T7,T8,T9;
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
     X
*/
    if(!(*x < 0.0e0)) goto S120;
    *bound = 0.0e0;
    *status = -4;
    return;
S130:
S120:
    if(*which == 3) goto S150;
/*
     SHAPE
*/
    if(!(*shape <= 0.0e0)) goto S140;
    *bound = 0.0e0;
    *status = -5;
    return;
S150:
S140:
    if(*which == 4) goto S170;
/*
     SCALE
*/
    if(!(*scale <= 0.0e0)) goto S160;
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
    if(*which == 1) goto S240;
/*
     Select the minimum of P or Q
*/
    qporq = *p <= *q;
    if(!qporq) goto S220;
    porq = *p;
    goto S230;
S220:
    porq = *q;
S240:
S230:
/*
     Calculate ANSWERS
*/
    if(1 == *which) {
/*
     Calculating P
*/
        *status = 0;
        xscale = *x**scale;
        cumgam(&xscale,shape,p,q);
        if(porq > 1.5e0) *status = 10;
    }
    else if(2 == *which) {
/*
     Computing X
*/
        T2 = -1.0e0;
        gaminv(shape,&xx,&T2,p,q,&ierr);
        if(ierr < 0.0e0) {
            *status = 10;
            return;
        }
        else  {
            *x = xx/ *scale;
            *status = 0;
        }
    }
    else if(3 == *which) {
/*
     Computing SHAPE
*/
        *shape = 5.0e0;
        xscale = *x**scale;
        T3 = zero;
        T4 = inf;
        T7 = atol;
        T8 = tol;
        dstinv(&T3,&T4,&K5,&K5,&K6,&T7,&T8);
        *status = 0;
        dinvr(status,shape,&fx,&qleft,&qhi);
S250:
        if(!(*status == 1)) goto S290;
        cumgam(&xscale,shape,&cum,&ccum);
        if(!qporq) goto S260;
        fx = cum-*p;
        goto S270;
S260:
        fx = ccum-*q;
S270:
        if(!(qporq && cum > 1.5e0 || !qporq && ccum > 1.5e0)) goto S280;
        *status = 10;
        return;
S280:
        dinvr(status,shape,&fx,&qleft,&qhi);
        goto S250;
S290:
        if(!(*status == -1)) goto S320;
        if(!qleft) goto S300;
        *status = 1;
        *bound = zero;
        goto S310;
S300:
        *status = 2;
        *bound = inf;
S320:
S310:
        ;
    }
    else if(4 == *which) {
/*
     Computing SCALE
*/
        T9 = -1.0e0;
        gaminv(shape,&xx,&T9,p,q,&ierr);
        if(ierr < 0.0e0) {
            *status = 10;
            return;
        }
        else  {
            *scale = xx/ *x;
            *status = 0;
        }
    }
    return;
#undef tol
#undef atol
#undef zero
#undef inf
} /* END */
