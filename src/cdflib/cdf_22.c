#include "cdflib.h"
void cdfnor(int *which,double *p,double *q,double *x,double *mean,
	    double *sd,int *status,double *bound)
/**********************************************************************

      void cdfnor(int *which,double *p,double *q,double *x,double *mean,
            double *sd,int *status,double *bound)

               Cumulative Distribution Function
               NORmal distribution


                              Function


     Calculates any one parameter of the normal
     distribution given values for the others.


                              Arguments


     WHICH  --> Integer indicating  which of the  next  parameter
     values is to be calculated using values  of the others.
     Legal range: 1..4
               iwhich = 1 : Calculate P and Q from X,MEAN and SD
               iwhich = 2 : Calculate X from P,Q,MEAN and SD
               iwhich = 3 : Calculate MEAN from P,Q,X and SD
               iwhich = 4 : Calculate SD from P,Q,X and MEAN

     P <--> The integral from -infinity to X of the normal density.
            Input range: (0,1].

     Q <--> 1-P.
            Input range: (0, 1].
            P + Q = 1.0.

     X < --> Upper limit of integration of the normal-density.
             Input range: ( -infinity, +infinity)

     MEAN <--> The mean of the normal density.
               Input range: (-infinity, +infinity)

     SD <--> Standard Deviation of the normal density.
             Input range: (0, +infinity).

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




     A slightly modified version of ANORM from

     Cody, W.D. (1993). "ALGORITHM 715: SPECFUN - A Portabel FORTRAN
     Package of Special Function Routines and Test Drivers"
     acm Transactions on Mathematical Software. 19, 22-32.

     is used to calulate the  cumulative standard normal distribution.

     The rational functions from pages  90-95  of Kennedy and Gentle,
     Statistical  Computing,  Marcel  Dekker, NY,  1980 are  used  as
     starting values to Newton's Iterations which compute the inverse
     standard normal.  Therefore no  searches  are necessary for  any
     parameter.

     For X < -15, the asymptotic expansion for the normal is used  as
     the starting value in finding the inverse standard normal.
     This is formula 26.2.12 of Abramowitz and Stegun.


                              Note


      The normal density is proportional to
      exp( - 0.5 * (( X - MEAN)/SD)**2)

**********************************************************************/
{
static int K1 = 1;
static double z,pq;
/*
     ..
     .. Executable Statements ..
*/
/*
     Check arguments
*/
    *status = 0;
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
    if(*which == 1) goto S150;
/*
     P + Q
*/
    pq = *p+*q;
    if(!(fabs(pq-0.5e0-0.5e0) > 3.0e0*spmpar(&K1))) goto S140;
    if(!(pq < 0.0e0)) goto S120;
    *bound = 0.0e0;
    goto S130;
S120:
    *bound = 1.0e0;
S130:
    *status = 3;
    return;
S150:
S140:
    if(*which == 4) goto S170;
/*
     SD
*/
    if(!(*sd <= 0.0e0)) goto S160;
    *bound = 0.0e0;
    *status = -6;
    return;
S170:
S160:
/*
     Calculate ANSWERS
*/
    if(1 == *which) {
/*
     Computing P
*/
        z = (*x-*mean)/ *sd;
        cumnor(&z,p,q);
    }
    else if(2 == *which) {
/*
     Computing X
*/
        z = dinvnr(p,q);
        *x = *sd*z+*mean;
    }
    else if(3 == *which) {
/*
     Computing the MEAN
*/
        z = dinvnr(p,q);
        *mean = *x-*sd*z;
    }
    else if(4 == *which) {
/*
     Computing SD
*/
        z = dinvnr(p,q);
        *sd = (*x-*mean)/z;
    }
    return;
} /* END */
