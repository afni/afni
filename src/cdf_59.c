#include "cdflib.h"
void gaminv(double *a,double *x,double *x0,double *p,double *q,
	    int *ierr)
/*
 ----------------------------------------------------------------------
            INVERSE INCOMPLETE GAMMA RATIO FUNCTION
 
     GIVEN POSITIVE A, AND NONEGATIVE P AND Q WHERE P + Q = 1.
     THEN X IS COMPUTED WHERE P(A,X) = P AND Q(A,X) = Q. SCHRODER
     ITERATION IS EMPLOYED. THE ROUTINE ATTEMPTS TO COMPUTE X
     TO 10 SIGNIFICANT DIGITS IF THIS IS POSSIBLE FOR THE
     PARTICULAR COMPUTER ARITHMETIC BEING USED.
 
                      ------------
 
     X IS A VARIABLE. IF P = 0 THEN X IS ASSIGNED THE VALUE 0,
     AND IF Q = 0 THEN X IS SET TO THE LARGEST FLOATING POINT
     NUMBER AVAILABLE. OTHERWISE, GAMINV ATTEMPTS TO OBTAIN
     A SOLUTION FOR P(A,X) = P AND Q(A,X) = Q. IF THE ROUTINE
     IS SUCCESSFUL THEN THE SOLUTION IS STORED IN X.
 
     X0 IS AN OPTIONAL INITIAL APPROXIMATION FOR X. IF THE USER
     DOES NOT WISH TO SUPPLY AN INITIAL APPROXIMATION, THEN SET
     X0 .LE. 0.
 
     IERR IS A VARIABLE THAT REPORTS THE STATUS OF THE RESULTS.
     WHEN THE ROUTINE TERMINATES, IERR HAS ONE OF THE FOLLOWING
     VALUES ...
 
       IERR =  0    THE SOLUTION WAS OBTAINED. ITERATION WAS
                    NOT USED.
       IERR.GT.0    THE SOLUTION WAS OBTAINED. IERR ITERATIONS
                    WERE PERFORMED.
       IERR = -2    (INPUT ERROR) A .LE. 0
       IERR = -3    NO SOLUTION WAS OBTAINED. THE RATIO Q/A
                    IS TOO LARGE.
       IERR = -4    (INPUT ERROR) P + Q .NE. 1
       IERR = -6    20 ITERATIONS WERE PERFORMED. THE MOST
                    RECENT VALUE OBTAINED FOR X IS GIVEN.
                    THIS CANNOT OCCUR IF X0 .LE. 0.
       IERR = -7    ITERATION FAILED. NO VALUE IS GIVEN FOR X.
                    THIS MAY OCCUR WHEN X IS APPROXIMATELY 0.
       IERR = -8    A VALUE FOR X HAS BEEN OBTAINED, BUT THE
                    ROUTINE IS NOT CERTAIN OF ITS ACCURACY.
                    ITERATION CANNOT BE PERFORMED IN THIS
                    CASE. IF X0 .LE. 0, THIS CAN OCCUR ONLY
                    WHEN P OR Q IS APPROXIMATELY 0. IF X0 IS
                    POSITIVE THEN THIS CAN OCCUR WHEN A IS
                    EXCEEDINGLY CLOSE TO X AND A IS EXTREMELY
                    LARGE (SAY A .GE. 1.E20).
 ----------------------------------------------------------------------
     WRITTEN BY ALFRED H. MORRIS, JR.
        NAVAL SURFACE WEAPONS CENTER
        DAHLGREN, VIRGINIA
     -------------------
*/
{
static double a0 = 3.31125922108741e0;
static double a1 = 11.6616720288968e0;
static double a2 = 4.28342155967104e0;
static double a3 = .213623493715853e0;
static double b1 = 6.61053765625462e0;
static double b2 = 6.40691597760039e0;
static double b3 = 1.27364489782223e0;
static double b4 = .036117081018842e0;
static double c = .577215664901533e0;
static double ln10 = 2.302585e0;
static double tol = 1.e-5;
static double amin[2] = {
    500.0e0,100.0e0
};
static double bmin[2] = {
    1.e-28,1.e-13
};
static double dmin[2] = {
    1.e-06,1.e-04
};
static double emin[2] = {
    2.e-03,6.e-03
};
static double eps0[2] = {
    1.e-10,1.e-08
};
static int K1 = 1;
static int K2 = 2;
static int K3 = 3;
static int K8 = 0;
static double am1,amax,ap1,ap2,ap3,apn,b,c1,c2,c3,c4,c5,d,e,e2,eps,g,h,pn,qg,qn,
    r,rta,s,s2,sum,t,u,w,xmax,xmin,xn,y,z;
static int iop;
static double T4,T5,T6,T7,T9;
/*
     ..
     .. Executable Statements ..
*/
/*
     ****** E, XMIN, AND XMAX ARE MACHINE DEPENDENT CONSTANTS.
            E IS THE SMALLEST NUMBER FOR WHICH 1.0 + E .GT. 1.0.
            XMIN IS THE SMALLEST POSITIVE NUMBER AND XMAX IS THE
            LARGEST POSITIVE NUMBER.
*/
    e = spmpar(&K1);
    xmin = spmpar(&K2);
    xmax = spmpar(&K3);
    *x = 0.0e0;
    if(*a <= 0.0e0) goto S300;
    t = *p+*q-1.e0;
    if(fabs(t) > e) goto S320;
    *ierr = 0;
    if(*p == 0.0e0) return;
    if(*q == 0.0e0) goto S270;
    if(*a == 1.0e0) goto S280;
    e2 = 2.0e0*e;
    amax = 0.4e-10/(e*e);
    iop = 1;
    if(e > 1.e-10) iop = 2;
    eps = eps0[iop-1];
    xn = *x0;
    if(*x0 > 0.0e0) goto S160;
/*
        SELECTION OF THE INITIAL APPROXIMATION XN OF X
                       WHEN A .LT. 1
*/
    if(*a > 1.0e0) goto S80;
    T4 = *a+1.0e0;
    g = Xgamm(&T4);
    qg = *q*g;
    if(qg == 0.0e0) goto S360;
    b = qg/ *a;
    if(qg > 0.6e0**a) goto S40;
    if(*a >= 0.30e0 || b < 0.35e0) goto S10;
    t = exp(-(b+c));
    u = t*exp(t);
    xn = t*exp(u);
    goto S160;
S10:
    if(b >= 0.45e0) goto S40;
    if(b == 0.0e0) goto S360;
    y = -log(b);
    s = 0.5e0+(0.5e0-*a);
    z = log(y);
    t = y-s*z;
    if(b < 0.15e0) goto S20;
    xn = y-s*log(t)-log(1.0e0+s/(t+1.0e0));
    goto S220;
S20:
    if(b <= 0.01e0) goto S30;
    u = ((t+2.0e0*(3.0e0-*a))*t+(2.0e0-*a)*(3.0e0-*a))/((t+(5.0e0-*a))*t+2.0e0);
    xn = y-s*log(t)-log(u);
    goto S220;
S30:
    c1 = -(s*z);
    c2 = -(s*(1.0e0+c1));
    c3 = s*((0.5e0*c1+(2.0e0-*a))*c1+(2.5e0-1.5e0**a));
    c4 = -(s*(((c1/3.0e0+(2.5e0-1.5e0**a))*c1+((*a-6.0e0)**a+7.0e0))*c1+(
      (11.0e0**a-46.0)**a+47.0e0)/6.0e0));
    c5 = -(s*((((-(c1/4.0e0)+(11.0e0**a-17.0e0)/6.0e0)*c1+((-(3.0e0**a)+13.0e0)*
      *a-13.0e0))*c1+0.5e0*(((2.0e0**a-25.0e0)**a+72.0e0)**a-61.0e0))*c1+((
      (25.0e0**a-195.0e0)**a+477.0e0)**a-379.0e0)/12.0e0));
    xn = (((c5/y+c4)/y+c3)/y+c2)/y+c1+y;
    if(*a > 1.0e0) goto S220;
    if(b > bmin[iop-1]) goto S220;
    *x = xn;
    return;
S40:
    if(b**q > 1.e-8) goto S50;
    xn = exp(-(*q/ *a+c));
    goto S70;
S50:
    if(*p <= 0.9e0) goto S60;
    T5 = -*q;
    xn = exp((alnrel(&T5)+gamln1(a))/ *a);
    goto S70;
S60:
    xn = exp(log(*p*g)/ *a);
S70:
    if(xn == 0.0e0) goto S310;
    t = 0.5e0+(0.5e0-xn/(*a+1.0e0));
    xn /= t;
    goto S160;
S80:
/*
        SELECTION OF THE INITIAL APPROXIMATION XN OF X
                       WHEN A .GT. 1
*/
    if(*q <= 0.5e0) goto S90;
    w = log(*p);
    goto S100;
S90:
    w = log(*q);
S100:
    t = sqrt(-(2.0e0*w));
    s = t-(((a3*t+a2)*t+a1)*t+a0)/((((b4*t+b3)*t+b2)*t+b1)*t+1.0e0);
    if(*q > 0.5e0) s = -s;
    rta = sqrt(*a);
    s2 = s*s;
    xn = *a+s*rta+(s2-1.0e0)/3.0e0+s*(s2-7.0e0)/(36.0e0*rta)-((3.0e0*s2+7.0e0)*
      s2-16.0e0)/(810.0e0**a)+s*((9.0e0*s2+256.0e0)*s2-433.0e0)/(38880.0e0**a*
      rta);
    xn = fifdmax1(xn,0.0e0);
    if(*a < amin[iop-1]) goto S110;
    *x = xn;
    d = 0.5e0+(0.5e0-*x/ *a);
    if(fabs(d) <= dmin[iop-1]) return;
S110:
    if(*p <= 0.5e0) goto S130;
    if(xn < 3.0e0**a) goto S220;
    y = -(w+gamln(a));
    d = fifdmax1(2.0e0,*a*(*a-1.0e0));
    if(y < ln10*d) goto S120;
    s = 1.0e0-*a;
    z = log(y);
    goto S30;
S120:
    t = *a-1.0e0;
    T6 = -(t/(xn+1.0e0));
    xn = y+t*log(xn)-alnrel(&T6);
    T7 = -(t/(xn+1.0e0));
    xn = y+t*log(xn)-alnrel(&T7);
    goto S220;
S130:
    ap1 = *a+1.0e0;
    if(xn > 0.70e0*ap1) goto S170;
    w += gamln(&ap1);
    if(xn > 0.15e0*ap1) goto S140;
    ap2 = *a+2.0e0;
    ap3 = *a+3.0e0;
    *x = exp((w+*x)/ *a);
    *x = exp((w+*x-log(1.0e0+*x/ap1*(1.0e0+*x/ap2)))/ *a);
    *x = exp((w+*x-log(1.0e0+*x/ap1*(1.0e0+*x/ap2)))/ *a);
    *x = exp((w+*x-log(1.0e0+*x/ap1*(1.0e0+*x/ap2*(1.0e0+*x/ap3))))/ *a);
    xn = *x;
    if(xn > 1.e-2*ap1) goto S140;
    if(xn <= emin[iop-1]*ap1) return;
    goto S170;
S140:
    apn = ap1;
    t = xn/apn;
    sum = 1.0e0+t;
S150:
    apn += 1.0e0;
    t *= (xn/apn);
    sum += t;
    if(t > 1.e-4) goto S150;
    t = w-log(sum);
    xn = exp((xn+t)/ *a);
    xn *= (1.0e0-(*a*log(xn)-xn-t)/(*a-xn));
    goto S170;
S160:
/*
                 SCHRODER ITERATION USING P
*/
    if(*p > 0.5e0) goto S220;
S170:
    if(*p <= 1.e10*xmin) goto S350;
    am1 = *a-0.5e0-0.5e0;
S180:
    if(*a <= amax) goto S190;
    d = 0.5e0+(0.5e0-xn/ *a);
    if(fabs(d) <= e2) goto S350;
S190:
    if(*ierr >= 20) goto S330;
    *ierr += 1;
    gratio(a,&xn,&pn,&qn,&K8);
    if(pn == 0.0e0 || qn == 0.0e0) goto S350;
    r = rcomp(a,&xn);
    if(r == 0.0e0) goto S350;
    t = (pn-*p)/r;
    w = 0.5e0*(am1-xn);
    if(fabs(t) <= 0.1e0 && fabs(w*t) <= 0.1e0) goto S200;
    *x = xn*(1.0e0-t);
    if(*x <= 0.0e0) goto S340;
    d = fabs(t);
    goto S210;
S200:
    h = t*(1.0e0+w*t);
    *x = xn*(1.0e0-h);
    if(*x <= 0.0e0) goto S340;
    if(fabs(w) >= 1.0e0 && fabs(w)*t*t <= eps) return;
    d = fabs(h);
S210:
    xn = *x;
    if(d > tol) goto S180;
    if(d <= eps) return;
    if(fabs(*p-pn) <= tol**p) return;
    goto S180;
S220:
/*
                 SCHRODER ITERATION USING Q
*/
    if(*q <= 1.e10*xmin) goto S350;
    am1 = *a-0.5e0-0.5e0;
S230:
    if(*a <= amax) goto S240;
    d = 0.5e0+(0.5e0-xn/ *a);
    if(fabs(d) <= e2) goto S350;
S240:
    if(*ierr >= 20) goto S330;
    *ierr += 1;
    gratio(a,&xn,&pn,&qn,&K8);
    if(pn == 0.0e0 || qn == 0.0e0) goto S350;
    r = rcomp(a,&xn);
    if(r == 0.0e0) goto S350;
    t = (*q-qn)/r;
    w = 0.5e0*(am1-xn);
    if(fabs(t) <= 0.1e0 && fabs(w*t) <= 0.1e0) goto S250;
    *x = xn*(1.0e0-t);
    if(*x <= 0.0e0) goto S340;
    d = fabs(t);
    goto S260;
S250:
    h = t*(1.0e0+w*t);
    *x = xn*(1.0e0-h);
    if(*x <= 0.0e0) goto S340;
    if(fabs(w) >= 1.0e0 && fabs(w)*t*t <= eps) return;
    d = fabs(h);
S260:
    xn = *x;
    if(d > tol) goto S230;
    if(d <= eps) return;
    if(fabs(*q-qn) <= tol**q) return;
    goto S230;
S270:
/*
                       SPECIAL CASES
*/
    *x = xmax;
    return;
S280:
    if(*q < 0.9e0) goto S290;
    T9 = -*p;
    *x = -alnrel(&T9);
    return;
S290:
    *x = -log(*q);
    return;
S300:
/*
                       ERROR RETURN
*/
    *ierr = -2;
    return;
S310:
    *ierr = -3;
    return;
S320:
    *ierr = -4;
    return;
S330:
    *ierr = -6;
    return;
S340:
    *ierr = -7;
    return;
S350:
    *x = xn;
    *ierr = -8;
    return;
S360:
    *x = xmax;
    *ierr = -8;
    return;
} /* END */
