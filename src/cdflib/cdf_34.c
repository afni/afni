#include "cdflib.h"
void cumpoi(double *s,double *xlam,double *cum,double *ccum)
/*
**********************************************************************
 
     void cumpoi(double *s,double *xlam,double *cum,double *ccum)
                    CUMulative POIsson distribution
 
 
                              Function
 
 
     Returns the  probability  of  S   or  fewer events in  a   Poisson
     distribution with mean XLAM.
 
 
                              Arguments
 
 
     S --> Upper limit of cumulation of the Poisson.
                                                  S is DOUBLE PRECISION
 
     XLAM --> Mean of the Poisson distribution.
                                                  XLAM is DOUBLE PRECIS
 
     CUM <-- Cumulative poisson distribution.
                                        CUM is DOUBLE PRECISION
 
     CCUM <-- Compliment of Cumulative poisson distribution.
                                                  CCUM is DOUBLE PRECIS
 
 
                              Method
 
 
     Uses formula  26.4.21   of   Abramowitz and  Stegun,  Handbook  of
     Mathematical   Functions  to reduce   the   cumulative Poisson  to
     the cumulative chi-square distribution.
 
**********************************************************************
*/
{
static double chi,df;
/*
     ..
     .. Executable Statements ..
*/
    df = 2.0e0*(*s+1.0e0);
    chi = 2.0e0**xlam;
    cumchi(&chi,&df,ccum,cum);
    return;
} /* END */
