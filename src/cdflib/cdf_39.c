#include "cdflib.h"
double dinvnr(double *p,double *q)
/*
**********************************************************************
 
     double dinvnr(double *p,double *q)
     Double precision NoRmal distribution INVerse
 
 
                              Function
 
 
     Returns X  such that CUMNOR(X)  =   P,  i.e., the  integral from -
     infinity to X of (1/SQRT(2*PI)) EXP(-U*U/2) dU is P
 
 
                              Arguments
 
 
     P --> The probability whose normal deviate is sought.
                    P is DOUBLE PRECISION
 
     Q --> 1-P
                    P is DOUBLE PRECISION
 
 
                              Method
 
 
     The  rational   function   on  page 95    of Kennedy  and  Gentle,
     Statistical Computing, Marcel Dekker, NY , 1980 is used as a start
     value for the Newton method of finding roots.
 
 
                              Note
 
 
     If P or Q .lt. machine EPS returns +/- DINVNR(EPS)
 
**********************************************************************
*/
{
#define maxit 100
#define eps (1.0e-13)
#define r2pi 0.3989422804014326e0
#define nhalf (-0.5e0)
#define dennor(x) (r2pi*exp(nhalf*(x)*(x)))
static double dinvnr,strtx,xcur,cum,ccum,pp,dx;
static int i;
static unsigned long qporq;
/*
     ..
     .. Executable Statements ..
*/
/*
     FIND MINIMUM OF P AND Q
*/
    qporq = *p <= *q;
    if(!qporq) goto S10;
    pp = *p;
    goto S20;
S10:
    pp = *q;
S20:
/*
     INITIALIZATION STEP
*/
    strtx = stvaln(&pp);
    xcur = strtx;
/*
     NEWTON INTERATIONS
*/
    for(i=1; i<=maxit; i++) {
        cumnor(&xcur,&cum,&ccum);
        dx = (cum-pp)/dennor(xcur);
        xcur -= dx;
        if(fabs(dx/xcur) < eps) goto S40;
    }
    dinvnr = strtx;
/*
     IF WE GET HERE, NEWTON HAS FAILED
*/
    if(!qporq) dinvnr = -dinvnr;
    return dinvnr;
S40:
/*
     IF WE GET HERE, NEWTON HAS SUCCEDED
*/
    dinvnr = xcur;
    if(!qporq) dinvnr = -dinvnr;
    return dinvnr;
#undef maxit
#undef eps
#undef r2pi
#undef nhalf
#undef dennor
} /* END */
