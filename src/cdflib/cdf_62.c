#include "cdflib.h"
double Xgamm(double *a)
/*
-----------------------------------------------------------------------
 
         EVALUATION OF THE GAMMA FUNCTION FOR REAL ARGUMENTS
 
                           -----------
 
     GAMMA(A) IS ASSIGNED THE VALUE 0 WHEN THE GAMMA FUNCTION CANNOT
     BE COMPUTED.
 
-----------------------------------------------------------------------
     WRITTEN BY ALFRED H. MORRIS, JR.
          NAVAL SURFACE WEAPONS CENTER
          DAHLGREN, VIRGINIA
-----------------------------------------------------------------------
*/
{
static double d = .41893853320467274178e0;
static double pi = 3.1415926535898e0;
static double r1 = .820756370353826e-03;
static double r2 = -.595156336428591e-03;
static double r3 = .793650663183693e-03;
static double r4 = -.277777777770481e-02;
static double r5 = .833333333333333e-01;
static double p[7] = {
    .539637273585445e-03,.261939260042690e-02,.204493667594920e-01,
    .730981088720487e-01,.279648642639792e+00,.553413866010467e+00,1.0e0
};
static double q[7] = {
    -.832979206704073e-03,.470059485860584e-02,.225211131035340e-01,
    -.170458969313360e+00,-.567902761974940e-01,.113062953091122e+01,1.0e0
};
static int K2 = 3;
static int K3 = 0;
static double Xgamm,bot,g,lnx,s,t,top,w,x,z;
static int i,j,m,n,T1;
/*
     ..
     .. Executable Statements ..
*/
    Xgamm = 0.0e0;
    x = *a;
    if(fabs(*a) >= 15.0e0) goto S110;
/*
-----------------------------------------------------------------------
            EVALUATION OF GAMMA(A) FOR ABS(A) .LT. 15
-----------------------------------------------------------------------
*/
    t = 1.0e0;
    m = fifidint(*a)-1;
/*
     LET T BE THE PRODUCT OF A-J WHEN A .GE. 2
*/
    T1 = m;
    if(T1 < 0) goto S40;
    else if(T1 == 0) goto S30;
    else  goto S10;
S10:
    for(j=1; j<=m; j++) {
        x -= 1.0e0;
        t = x*t;
    }
S30:
    x -= 1.0e0;
    goto S80;
S40:
/*
     LET T BE THE PRODUCT OF A+J WHEN A .LT. 1
*/
    t = *a;
    if(*a > 0.0e0) goto S70;
    m = -m-1;
    if(m == 0) goto S60;
    for(j=1; j<=m; j++) {
        x += 1.0e0;
        t = x*t;
    }
S60:
    x += (0.5e0+0.5e0);
    t = x*t;
    if(t == 0.0e0) return Xgamm;
S70:
/*
     THE FOLLOWING CODE CHECKS IF 1/T CAN OVERFLOW. THIS
     CODE MAY BE OMITTED IF DESIRED.
*/
    if(fabs(t) >= 1.e-30) goto S80;
    if(fabs(t)*spmpar(&K2) <= 1.0001e0) return Xgamm;
    Xgamm = 1.0e0/t;
    return Xgamm;
S80:
/*
     COMPUTE GAMMA(1 + X) FOR  0 .LE. X .LT. 1
*/
    top = p[0];
    bot = q[0];
    for(i=1; i<7; i++) {
        top = p[i]+x*top;
        bot = q[i]+x*bot;
    }
    Xgamm = top/bot;
/*
     TERMINATION
*/
    if(*a < 1.0e0) goto S100;
    Xgamm *= t;
    return Xgamm;
S100:
    Xgamm /= t;
    return Xgamm;
S110:
/*
-----------------------------------------------------------------------
            EVALUATION OF GAMMA(A) FOR ABS(A) .GE. 15
-----------------------------------------------------------------------
*/
    if(fabs(*a) >= 1.e3) return Xgamm;
    if(*a > 0.0e0) goto S120;
    x = -*a;
    n = x;
    t = x-(double)n;
    if(t > 0.9e0) t = 1.0e0-t;
    s = sin(pi*t)/pi;
    if(fifmod(n,2) == 0) s = -s;
    if(s == 0.0e0) return Xgamm;
S120:
/*
     COMPUTE THE MODIFIED ASYMPTOTIC SUM
*/
    t = 1.0e0/(x*x);
    g = ((((r1*t+r2)*t+r3)*t+r4)*t+r5)/x;
/*
     ONE MAY REPLACE THE NEXT STATEMENT WITH  LNX = ALOG(X)
     BUT LESS ACCURACY WILL NORMALLY BE OBTAINED.
*/
    lnx = log(x);
/*
     FINAL ASSEMBLY
*/
    z = x;
    g = d+g+(z-0.5e0)*(lnx-1.e0);
    w = g;
    t = g-w;
    if(w > 0.99999e0*exparg(&K3)) return Xgamm;
    Xgamm = exp(w)*(1.0e0+t);
    if(*a < 0.0e0) Xgamm = 1.0e0/(Xgamm*s)/x;
    return Xgamm;
} /* END */
