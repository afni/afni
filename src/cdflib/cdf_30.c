#include "cdflib.h"
void cumfnc(double *f,double *dfn,double *dfd,double *pnonc,
	    double *cum,double *ccum)
/*
**********************************************************************
 
               F -NON- -C-ENTRAL F DISTRIBUTION
 
 
 
                              Function
 
 
     COMPUTES NONCENTRAL F DISTRIBUTION WITH DFN AND DFD
     DEGREES OF FREEDOM AND NONCENTRALITY PARAMETER PNONC
 
 
                              Arguments
 
 
     X --> UPPER LIMIT OF INTEGRATION OF NONCENTRAL F IN EQUATION
 
     DFN --> DEGREES OF FREEDOM OF NUMERATOR
 
     DFD -->  DEGREES OF FREEDOM OF DENOMINATOR
 
     PNONC --> NONCENTRALITY PARAMETER.
 
     CUM <-- CUMULATIVE NONCENTRAL F DISTRIBUTION
 
     CCUM <-- COMPLIMENT OF CUMMULATIVE
 
 
                              Method
 
 
     USES FORMULA 26.6.20 OF REFERENCE FOR INFINITE SERIES.
     SERIES IS CALCULATED BACKWARD AND FORWARD FROM J = LAMBDA/2
     (THIS IS THE TERM WITH THE LARGEST POISSON WEIGHT) UNTIL
     THE CONVERGENCE CRITERION IS MET.
 
     FOR SPEED, THE INCOMPLETE BETA FUNCTIONS ARE EVALUATED
     BY FORMULA 26.5.16.
 
 
               REFERENCE
 
 
     HANDBOOD OF MATHEMATICAL FUNCTIONS
     EDITED BY MILTON ABRAMOWITZ AND IRENE A. STEGUN
     NATIONAL BUREAU OF STANDARDS APPLIED MATEMATICS SERIES - 55
     MARCH 1965
     P 947, EQUATIONS 26.6.17, 26.6.18
 
 
                              Note
 
 
     THE SUM CONTINUES UNTIL A SUCCEEDING TERM IS LESS THAN EPS
     TIMES THE SUM (OR THE SUM IS LESS THAN 1.0E-20).  EPS IS
     SET TO 1.0E-4 IN A DATA STATEMENT WHICH CAN BE CHANGED.
 
**********************************************************************
*/
{
#define qsmall(x) (int)(sum < 1.0e-20 || (x) < eps*sum)
#define half 0.5e0
#define done 1.0e0
static double eps = 1.0e-4;
static double dsum,dummy,prod,xx,yy,adn,aup,b,betdn,betup,centwt,dnterm,sum,
    upterm,xmult,xnonc;
static int i,icent,ierr;
static double T1,T2,T3,T4,T5,T6;
/*
     ..
     .. Executable Statements ..
*/
    if(!(*f <= 0.0e0)) goto S10;
    *cum = 0.0e0;
    *ccum = 1.0e0;
    return;
S10:
    if(!(*pnonc < 1.0e-10)) goto S20;
/*
     Handle case in which the non-centrality parameter is
     (essentially) zero.
*/
    cumf(f,dfn,dfd,cum,ccum);
    return;
S20:
    xnonc = *pnonc/2.0e0;
/*
     Calculate the central term of the poisson weighting factor.
*/
    icent = xnonc;
    if(icent == 0) icent = 1;
/*
     Compute central weight term
*/
    T1 = (double)(icent+1);
    centwt = exp(-xnonc+(double)icent*log(xnonc)-alngam(&T1));
/*
     Compute central incomplete beta term
     Assure that minimum of arg to beta and 1 - arg is computed
          accurately.
*/
    prod = *dfn**f;
    dsum = *dfd+prod;
    yy = *dfd/dsum;
    if(yy > half) {
        xx = prod/dsum;
        yy = done-xx;
    }
    else  xx = done-yy;
    T2 = *dfn*half+(double)icent;
    T3 = *dfd*half;
    bratio(&T2,&T3,&xx,&yy,&betdn,&dummy,&ierr);
    adn = *dfn/2.0e0+(double)icent;
    aup = adn;
    b = *dfd/2.0e0;
    betup = betdn;
    sum = centwt*betdn;
/*
     Now sum terms backward from icent until convergence or all done
*/
    xmult = centwt;
    i = icent;
    T4 = adn+b;
    T5 = adn+1.0e0;
    dnterm = exp(alngam(&T4)-alngam(&T5)-alngam(&b)+adn*log(xx)+b*log(yy));
S30:
    if(qsmall(xmult*betdn) || i <= 0) goto S40;
    xmult *= ((double)i/xnonc);
    i -= 1;
    adn -= 1.0;
    dnterm = (adn+1.0)/((adn+b)*xx)*dnterm;
    betdn += dnterm;
    sum += (xmult*betdn);
    goto S30;
S40:
    i = icent+1;
/*
     Now sum forwards until convergence
*/
    xmult = centwt;
    if(aup-1.0+b == 0) upterm = exp(-alngam(&aup)-alngam(&b)+(aup-1.0)*log(xx)+
      b*log(yy));
    else  {
        T6 = aup-1.0+b;
        upterm = exp(alngam(&T6)-alngam(&aup)-alngam(&b)+(aup-1.0)*log(xx)+b*
          log(yy));
    }
    goto S60;
S50:
    if(qsmall(xmult*betup)) goto S70;
S60:
    xmult *= (xnonc/(double)i);
    i += 1;
    aup += 1.0;
    upterm = (aup+b-2.0e0)*xx/(aup-1.0)*upterm;
    betup -= upterm;
    sum += (xmult*betup);
    goto S50;
S70:
    *cum = sum;
    *ccum = 0.5e0+(0.5e0-*cum);
    return;
#undef qsmall
#undef half
#undef done
} /* END */
