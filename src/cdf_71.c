#include "cdflib.h"
double spmpar(int *i)
/*
-----------------------------------------------------------------------
 
     SPMPAR PROVIDES THE SINGLE PRECISION MACHINE CONSTANTS FOR
     THE COMPUTER BEING USED. IT IS ASSUMED THAT THE ARGUMENT
     I IS AN INTEGER HAVING ONE OF THE VALUES 1, 2, OR 3. IF THE
     SINGLE PRECISION ARITHMETIC BEING USED HAS M BASE B DIGITS AND
     ITS SMALLEST AND LARGEST EXPONENTS ARE EMIN AND EMAX, THEN
 
        SPMPAR(1) = B**(1 - M), THE MACHINE PRECISION,
 
        SPMPAR(2) = B**(EMIN - 1), THE SMALLEST MAGNITUDE,
 
        SPMPAR(3) = B**EMAX*(1 - B**(-M)), THE LARGEST MAGNITUDE.
 
-----------------------------------------------------------------------
     WRITTEN BY
        ALFRED H. MORRIS, JR.
        NAVAL SURFACE WARFARE CENTER
        DAHLGREN VIRGINIA
-----------------------------------------------------------------------
-----------------------------------------------------------------------
     MODIFIED BY BARRY W. BROWN TO RETURN DOUBLE PRECISION MACHINE
     CONSTANTS FOR THE COMPUTER BEING USED.  THIS MODIFICATION WAS
     MADE AS PART OF CONVERTING BRATIO TO DOUBLE PRECISION
-----------------------------------------------------------------------
*/
{
static int K1 = 4;
static int K2 = 8;
static int K3 = 9;
static int K4 = 10;
static double spmpar,b,binv,bm1,one,w,z;
static int emax,emin,ibeta,m;
/*
     ..
     .. Executable Statements ..
*/
    if(*i > 1) goto S10;
    b = ipmpar(&K1);
    m = ipmpar(&K2);
    spmpar = pow(b,(double)(1-m));
    return spmpar;
S10:
    if(*i > 2) goto S20;
    b = ipmpar(&K1);
    emin = ipmpar(&K3);
    one = 1.0;
    binv = one/b;
    w = pow(b,(double)(emin+2));
    spmpar = w*binv*binv*binv;
    return spmpar;
S20:
    ibeta = ipmpar(&K1);
    m = ipmpar(&K2);
    emax = ipmpar(&K4);
    b = ibeta;
    bm1 = ibeta-1;
    one = 1.0;
    z = pow(b,(double)(m-1));
    w = ((z-one)*b+bm1)/(b*z);
    z = pow(b,(double)(emax-2));
    spmpar = w*z*b*b;
    return spmpar;
} /* END */
