#include "cdflib.h"
double dln1mx(double *x)
/*
**********************************************************************
 
     double dln1mx(double *x)
               Double precision LN(1-X)
 
 
                              Function
 
 
     Returns ln(1-x) for small x (good accuracy if x .le. 0.1).
     Note that the obvious code of
               LOG(1.0-X)
     won't work for small X because 1.0-X loses accuracy
 
 
                              Arguments
 
 
     X --> Value for which ln(1-x) is desired.
                                        X is DOUBLE PRECISION
 
 
                              Method
 
 
     If X > 0.1, the obvious code above is used ELSE
     The Taylor series for 1-x is expanded to 20 terms.
 
**********************************************************************
*/
{
static double dln1mx,T1;
/*
     ..
     .. Executable Statements ..
*/
    T1 = -*x;
    dln1mx = dln1px(&T1);
    return dln1mx;
} /* END */
