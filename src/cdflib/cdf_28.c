#include "cdflib.h"
void cumchn(double *x,double *df,double *pnonc,double *cum,
	    double *ccum)
/*
**********************************************************************
 
     void cumchn(double *x,double *df,double *pnonc,double *cum,
            double *ccum)

             CUMulative of the Non-central CHi-square distribution
 
 
                              Function
 
 
     Calculates     the       cumulative      non-central    chi-square
     distribution, i.e.,  the probability   that  a   random   variable
     which    follows  the  non-central chi-square  distribution,  with
     non-centrality  parameter    PNONC  and   continuous  degrees   of
     freedom DF, is less than or equal to X.
 
 
                              Arguments
 
 
     X       --> Upper limit of integration of the non-central
                 chi-square distribution.
                                                 X is DOUBLE PRECISION
 
     DF      --> Degrees of freedom of the non-central
                 chi-square distribution.
                                                 DF is DOUBLE PRECISION
 
     PNONC   --> Non-centrality parameter of the non-central
                 chi-square distribution.
                                                 PNONC is DOUBLE PRECIS
 
     CUM <-- Cumulative non-central chi-square distribution.
                                                 CUM is DOUBLE PRECISIO
 
     CCUM <-- Compliment of Cumulative non-central chi-square distribut
                                                 CCUM is DOUBLE PRECISI
 
 
                              Method
 
 
     Uses  formula  26.4.25   of  Abramowitz  and  Stegun, Handbook  of
     Mathematical    Functions,  US   NBS   (1966)    to calculate  the
     non-central chi-square.
 
 
                              Variables
 
 
     EPS     --- Convergence criterion.  The sum stops when a
                 term is less than EPS*SUM.
                                                 EPS is DOUBLE PRECISIO
 
     NTIRED  --- Maximum number of terms to be evaluated
                 in each sum.
                                                 NTIRED is INTEGER
 
     QCONV   --- .TRUE. if convergence achieved -
                 i.e., program did not stop on NTIRED criterion.
                                                 QCONV is LOGICAL
 
     CCUM <-- Compliment of Cumulative non-central
              chi-square distribution.
                                                 CCUM is DOUBLE PRECISI
 
**********************************************************************
*/
{
#define dg(i) (*df+2.0e0*(double)(i))
#define qsmall(xx) (int)(sum < 1.0e-20 || (xx) < eps*sum)
#define qtired(i) (int)((i) > ntired)
static double eps = 1.0e-5;
static int ntired = 1000;
static double adj,centaj,centwt,chid2,dfd2,lcntaj,lcntwt,lfact,pcent,pterm,sum,
    sumadj,term,wt,xnonc;
static int i,icent,iterb,iterf;
static double T1,T2,T3;
/*
     ..
     .. Executable Statements ..
*/
    if(!(*x <= 0.0e0)) goto S10;
    *cum = 0.0e0;
    *ccum = 1.0e0;
    return;
S10:
    if(!(*pnonc <= 1.0e-10)) goto S20;
/*
     When non-centrality parameter is (essentially) zero,
     use cumulative chi-square distribution
*/
    cumchi(x,df,cum,ccum);
    return;
S20:
    xnonc = *pnonc/2.0e0;
/*
**********************************************************************
     The following code calcualtes the weight, chi-square, and
     adjustment term for the central term in the infinite series.
     The central term is the one in which the poisson weight is
     greatest.  The adjustment term is the amount that must
     be subtracted from the chi-square to move up two degrees
     of freedom.
**********************************************************************
*/
    icent = fifidint(xnonc);
    if(icent == 0) icent = 1;
    chid2 = *x/2.0e0;
/*
     Calculate central weight term
*/
    T1 = (double)(icent+1);
    lfact = alngam(&T1);
    lcntwt = -xnonc+(double)icent*log(xnonc)-lfact;
    centwt = exp(lcntwt);
/*
     Calculate central chi-square
*/
    T2 = dg(icent);
    cumchi(x,&T2,&pcent,ccum);
/*
     Calculate central adjustment term
*/
    dfd2 = dg(icent)/2.0e0;
    T3 = 1.0e0+dfd2;
    lfact = alngam(&T3);
    lcntaj = dfd2*log(chid2)-chid2-lfact;
    centaj = exp(lcntaj);
    sum = centwt*pcent;
/*
**********************************************************************
     Sum backwards from the central term towards zero.
     Quit whenever either
     (1) the zero term is reached, or
     (2) the term gets small relative to the sum, or
     (3) More than NTIRED terms are totaled.
**********************************************************************
*/
    iterb = 0;
    sumadj = 0.0e0;
    adj = centaj;
    wt = centwt;
    i = icent;
    goto S40;
S30:
    if(qtired(iterb) || qsmall(term) || i == 0) goto S50;
S40:
    dfd2 = dg(i)/2.0e0;
/*
     Adjust chi-square for two fewer degrees of freedom.
     The adjusted value ends up in PTERM.
*/
    adj = adj*dfd2/chid2;
    sumadj += adj;
    pterm = pcent+sumadj;
/*
     Adjust poisson weight for J decreased by one
*/
    wt *= ((double)i/xnonc);
    term = wt*pterm;
    sum += term;
    i -= 1;
    iterb += 1;
    goto S30;
S50:
    iterf = 0;
/*
**********************************************************************
     Now sum forward from the central term towards infinity.
     Quit when either
     (1) the term gets small relative to the sum, or
     (2) More than NTIRED terms are totaled.
**********************************************************************
*/
    sumadj = adj = centaj;
    wt = centwt;
    i = icent;
    goto S70;
S60:
    if(qtired(iterf) || qsmall(term)) goto S80;
S70:
/*
     Update weights for next higher J
*/
    wt *= (xnonc/(double)(i+1));
/*
     Calculate PTERM and add term to sum
*/
    pterm = pcent-sumadj;
    term = wt*pterm;
    sum += term;
/*
     Update adjustment term for DF for next iteration
*/
    i += 1;
    dfd2 = dg(i)/2.0e0;
    adj = adj*chid2/dfd2;
    sumadj += adj;
    iterf += 1;
    goto S60;
S80:
    *cum = sum;
    *ccum = 0.5e0+(0.5e0-*cum);
    return;
#undef dg
#undef qsmall
#undef qtired
} /* END */
