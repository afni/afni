#include "cdflib.h"
double psi(double *xx)
/*
---------------------------------------------------------------------
 
                 EVALUATION OF THE DIGAMMA FUNCTION
 
                           -----------
 
     PSI(XX) IS ASSIGNED THE VALUE 0 WHEN THE DIGAMMA FUNCTION CANNOT
     BE COMPUTED.
 
     THE MAIN COMPUTATION INVOLVES EVALUATION OF RATIONAL CHEBYSHEV
     APPROXIMATIONS PUBLISHED IN MATH. COMP. 27, 123-127(1973) BY
     CODY, STRECOK AND THACHER.
 
---------------------------------------------------------------------
     PSI WAS WRITTEN AT ARGONNE NATIONAL LABORATORY FOR THE FUNPACK
     PACKAGE OF SPECIAL FUNCTION SUBROUTINES. PSI WAS MODIFIED BY
     A.H. MORRIS (NSWC).
---------------------------------------------------------------------
*/
{
static double dx0 = 1.461632144968362341262659542325721325e0;
static double piov4 = .785398163397448e0;
static double p1[7] = {
    .895385022981970e-02,.477762828042627e+01,.142441585084029e+03,
    .118645200713425e+04,.363351846806499e+04,.413810161269013e+04,
    .130560269827897e+04
};
static double p2[4] = {
    -.212940445131011e+01,-.701677227766759e+01,-.448616543918019e+01,
    -.648157123766197e+00
};
static double q1[6] = {
    .448452573429826e+02,.520752771467162e+03,.221000799247830e+04,
    .364127349079381e+04,.190831076596300e+04,.691091682714533e-05
};
static double q2[4] = {
    .322703493791143e+02,.892920700481861e+02,.546117738103215e+02,
    .777788548522962e+01
};
static int K1 = 3;
static int K2 = 1;
static double psi,aug,den,sgn,upper,w,x,xmax1,xmx0,xsmall,z;
static int i,m,n,nq;
/*
     ..
     .. Executable Statements ..
*/
/*
---------------------------------------------------------------------
     MACHINE DEPENDENT CONSTANTS ...
        XMAX1  = THE SMALLEST POSITIVE FLOATING POINT CONSTANT
                 WITH ENTIRELY INTEGER REPRESENTATION.  ALSO USED
                 AS NEGATIVE OF LOWER BOUND ON ACCEPTABLE NEGATIVE
                 ARGUMENTS AND AS THE POSITIVE ARGUMENT BEYOND WHICH
                 PSI MAY BE REPRESENTED AS ALOG(X).
        XSMALL = ABSOLUTE ARGUMENT BELOW WHICH PI*COTAN(PI*X)
                 MAY BE REPRESENTED BY 1/X.
---------------------------------------------------------------------
*/
    xmax1 = ipmpar(&K1);
    xmax1 = fifdmin1(xmax1,1.0e0/spmpar(&K2));
    xsmall = 1.e-9;
    x = *xx;
    aug = 0.0e0;
    if(x >= 0.5e0) goto S50;
/*
---------------------------------------------------------------------
     X .LT. 0.5,  USE REFLECTION FORMULA
     PSI(1-X) = PSI(X) + PI * COTAN(PI*X)
---------------------------------------------------------------------
*/
    if(fabs(x) > xsmall) goto S10;
    if(x == 0.0e0) goto S100;
/*
---------------------------------------------------------------------
     0 .LT. ABS(X) .LE. XSMALL.  USE 1/X AS A SUBSTITUTE
     FOR  PI*COTAN(PI*X)
---------------------------------------------------------------------
*/
    aug = -(1.0e0/x);
    goto S40;
S10:
/*
---------------------------------------------------------------------
     REDUCTION OF ARGUMENT FOR COTAN
---------------------------------------------------------------------
*/
    w = -x;
    sgn = piov4;
    if(w > 0.0e0) goto S20;
    w = -w;
    sgn = -sgn;
S20:
/*
---------------------------------------------------------------------
     MAKE AN ERROR EXIT IF X .LE. -XMAX1
---------------------------------------------------------------------
*/
    if(w >= xmax1) goto S100;
    nq = fifidint(w);
    w -= (double)nq;
    nq = fifidint(w*4.0e0);
    w = 4.0e0*(w-(double)nq*.25e0);
/*
---------------------------------------------------------------------
     W IS NOW RELATED TO THE FRACTIONAL PART OF  4.0 * X.
     ADJUST ARGUMENT TO CORRESPOND TO VALUES IN FIRST
     QUADRANT AND DETERMINE SIGN
---------------------------------------------------------------------
*/
    n = nq/2;
    if(n+n != nq) w = 1.0e0-w;
    z = piov4*w;
    m = n/2;
    if(m+m != n) sgn = -sgn;
/*
---------------------------------------------------------------------
     DETERMINE FINAL VALUE FOR  -PI*COTAN(PI*X)
---------------------------------------------------------------------
*/
    n = (nq+1)/2;
    m = n/2;
    m += m;
    if(m != n) goto S30;
/*
---------------------------------------------------------------------
     CHECK FOR SINGULARITY
---------------------------------------------------------------------
*/
    if(z == 0.0e0) goto S100;
/*
---------------------------------------------------------------------
     USE COS/SIN AS A SUBSTITUTE FOR COTAN, AND
     SIN/COS AS A SUBSTITUTE FOR TAN
---------------------------------------------------------------------
*/
    aug = sgn*(cos(z)/sin(z)*4.0e0);
    goto S40;
S30:
    aug = sgn*(sin(z)/cos(z)*4.0e0);
S40:
    x = 1.0e0-x;
S50:
    if(x > 3.0e0) goto S70;
/*
---------------------------------------------------------------------
     0.5 .LE. X .LE. 3.0
---------------------------------------------------------------------
*/
    den = x;
    upper = p1[0]*x;
    for(i=1; i<=5; i++) {
        den = (den+q1[i-1])*x;
        upper = (upper+p1[i+1-1])*x;
    }
    den = (upper+p1[6])/(den+q1[5]);
    xmx0 = x-dx0;
    psi = den*xmx0+aug;
    return psi;
S70:
/*
---------------------------------------------------------------------
     IF X .GE. XMAX1, PSI = LN(X)
---------------------------------------------------------------------
*/
    if(x >= xmax1) goto S90;
/*
---------------------------------------------------------------------
     3.0 .LT. X .LT. XMAX1
---------------------------------------------------------------------
*/
    w = 1.0e0/(x*x);
    den = w;
    upper = p2[0]*w;
    for(i=1; i<=3; i++) {
        den = (den+q2[i-1])*w;
        upper = (upper+p2[i+1-1])*w;
    }
    aug = upper/(den+q2[3])-0.5e0/x+aug;
S90:
    psi = aug+log(x);
    return psi;
S100:
/*
---------------------------------------------------------------------
     ERROR RETURN
---------------------------------------------------------------------
*/
    psi = 0.0e0;
    return psi;
} /* END */
