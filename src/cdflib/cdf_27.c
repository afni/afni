#include "cdflib.h"
void cumchi(double *x,double *df,double *cum,double *ccum)
/*
**********************************************************************
 
     void cumchi(double *x,double *df,double *cum,double *ccum)
             CUMulative of the CHi-square distribution
 
 
                              Function
 
 
     Calculates the cumulative chi-square distribution.
 
 
                              Arguments
 
 
     X       --> Upper limit of integration of the
                 chi-square distribution.
                                                 X is DOUBLE PRECISION
 
     DF      --> Degrees of freedom of the
                 chi-square distribution.
                                                 DF is DOUBLE PRECISION
 
     CUM <-- Cumulative chi-square distribution.
                                                 CUM is DOUBLE PRECISIO
 
     CCUM <-- Compliment of Cumulative chi-square distribution.
                                                 CCUM is DOUBLE PRECISI
 
 
                              Method
 
 
     Calls incomplete gamma function (CUMGAM)
 
**********************************************************************
*/
{
static double a,xx;
/*
     ..
     .. Executable Statements ..
*/
    a = *df*0.5e0;
    xx = *x*0.5e0;
    cumgam(&xx,&a,cum,ccum);
    return;
} /* END */
