#include "cdflib.h"
void cumgam(double *x,double *a,double *cum,double *ccum)
/*
**********************************************************************
 
     void cumgam(double *x,double *a,double *cum,double *ccum)
           Double precision cUMulative incomplete GAMma distribution
 
 
                              Function
 
 
     Computes   the  cumulative        of    the     incomplete   gamma
     distribution, i.e., the integral from 0 to X of
          (1/GAM(A))*EXP(-T)*T**(A-1) DT
     where GAM(A) is the complete gamma function of A, i.e.,
          GAM(A) = integral from 0 to infinity of
                    EXP(-T)*T**(A-1) DT
 
 
                              Arguments
 
 
     X --> The upper limit of integration of the incomplete gamma.
                                                X is DOUBLE PRECISION
 
     A --> The shape parameter of the incomplete gamma.
                                                A is DOUBLE PRECISION
 
     CUM <-- Cumulative incomplete gamma distribution.
                                        CUM is DOUBLE PRECISION
 
     CCUM <-- Compliment of Cumulative incomplete gamma distribution.
                                                CCUM is DOUBLE PRECISIO
 
 
                              Method
 
 
     Calls the routine GRATIO.
 
**********************************************************************
*/
{
static int K1 = 0;
/*
     ..
     .. Executable Statements ..
*/
    if(!(*x <= 0.0e0)) goto S10;
    *cum = 0.0e0;
    *ccum = 1.0e0;
    return;
S10:
    gratio(a,x,cum,ccum,&K1);
/*
     Call gratio routine
*/
    return;
} /* END */
