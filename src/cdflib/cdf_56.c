#include "cdflib.h"
double exparg(int *l)
/*
--------------------------------------------------------------------
     IF L = 0 THEN  EXPARG(L) = THE LARGEST POSITIVE W FOR WHICH
     EXP(W) CAN BE COMPUTED.
 
     IF L IS NONZERO THEN  EXPARG(L) = THE LARGEST NEGATIVE W FOR
     WHICH THE COMPUTED VALUE OF EXP(W) IS NONZERO.
 
     NOTE... ONLY AN APPROXIMATE VALUE FOR EXPARG(L) IS NEEDED.
--------------------------------------------------------------------
*/
{
static int K1 = 4;
static int K2 = 9;
static int K3 = 10;
static double exparg,lnb;
static int b,m;
/*
     ..
     .. Executable Statements ..
*/
    b = ipmpar(&K1);
    if(b != 2) goto S10;
    lnb = .69314718055995e0;
    goto S40;
S10:
    if(b != 8) goto S20;
    lnb = 2.0794415416798e0;
    goto S40;
S20:
    if(b != 16) goto S30;
    lnb = 2.7725887222398e0;
    goto S40;
S30:
    lnb = log((double)b);
S40:
    if(*l == 0) goto S50;
    m = ipmpar(&K2)-1;
    exparg = 0.99999e0*((double)m*lnb);
    return exparg;
S50:
    m = ipmpar(&K3);
    exparg = 0.99999e0*((double)m*lnb);
    return exparg;
} /* END */
