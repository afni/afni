#include "cdflib.h"
void E0001(int IENTRY,int *status,double *x,double *fx,
		  double *xlo,double *xhi,unsigned long *qleft,
		  unsigned long *qhi,double *zabstl,double *zreltl,
		  double *zxhi,double *zxlo)
{
#define ftol(zx) (0.5e0*fifdmax1(abstol,reltol*fabs((zx))))
static double a,abstol,b,c,d,fa,fb,fc,fd,fda,fdb,m,mb,p,q,reltol,tol,w,xxhi,xxlo;
static int ext,i99999;
static unsigned long first,qrzero;
    switch(IENTRY){case 0: goto DZROR; case 1: goto DSTZR;}
DZROR:
    if(*status > 0) goto S280;
    *xlo = xxlo;
    *xhi = xxhi;
    b = *x = *xlo;
/*
     GET-FUNCTION-VALUE
*/
    i99999 = 1;
    goto S270;
S10:
    fb = *fx;
    *xlo = *xhi;
    a = *x = *xlo;
/*
     GET-FUNCTION-VALUE
*/
    i99999 = 2;
    goto S270;
S20:
/*
     Check that F(ZXLO) < 0 < F(ZXHI)  or
                F(ZXLO) > 0 > F(ZXHI)
*/
    if(!(fb < 0.0e0)) goto S40;
    if(!(*fx < 0.0e0)) goto S30;
    *status = -1;
    *qleft = *fx < fb;
    *qhi = 0;
    return;
S40:
S30:
    if(!(fb > 0.0e0)) goto S60;
    if(!(*fx > 0.0e0)) goto S50;
    *status = -1;
    *qleft = *fx > fb;
    *qhi = 1;
    return;
S60:
S50:
    fa = *fx;
    first = 1;
S70:
    c = a;
    fc = fa;
    ext = 0;
S80:
    if(!(fabs(fc) < fabs(fb))) goto S100;
    if(!(c != a)) goto S90;
    d = a;
    fd = fa;
S90:
    a = b;
    fa = fb;
    *xlo = c;
    b = *xlo;
    fb = fc;
    c = a;
    fc = fa;
S100:
    tol = ftol(*xlo);
    m = (c+b)*.5e0;
    mb = m-b;
    if(!(fabs(mb) > tol)) goto S240;
    if(!(ext > 3)) goto S110;
    w = mb;
    goto S190;
S110:
    tol = fifdsign(tol,mb);
    p = (b-a)*fb;
    if(!first) goto S120;
    q = fa-fb;
    first = 0;
    goto S130;
S120:
    fdb = (fd-fb)/(d-b);
    fda = (fd-fa)/(d-a);
    p = fda*p;
    q = fdb*fa-fda*fb;
S130:
    if(!(p < 0.0e0)) goto S140;
    p = -p;
    q = -q;
S140:
    if(ext == 3) p *= 2.0e0;
    if(!(p*1.0e0 == 0.0e0 || p <= q*tol)) goto S150;
    w = tol;
    goto S180;
S150:
    if(!(p < mb*q)) goto S160;
    w = p/q;
    goto S170;
S160:
    w = mb;
S190:
S180:
S170:
    d = a;
    fd = fa;
    a = b;
    fa = fb;
    b += w;
    *xlo = b;
    *x = *xlo;
/*
     GET-FUNCTION-VALUE
*/
    i99999 = 3;
    goto S270;
S200:
    fb = *fx;
    if(!(fc*fb >= 0.0e0)) goto S210;
    goto S70;
S210:
    if(!(w == mb)) goto S220;
    ext = 0;
    goto S230;
S220:
    ext += 1;
S230:
    goto S80;
S240:
    *xhi = c;
    qrzero = fc >= 0.0e0 && fb <= 0.0e0 || fc < 0.0e0 && fb >= 0.0e0;
    if(!qrzero) goto S250;
    *status = 0;
    goto S260;
S250:
    *status = -1;
S260:
    return;
DSTZR:
    xxlo = *zxlo;
    xxhi = *zxhi;
    abstol = *zabstl;
    reltol = *zreltl;
    return;
S270:
/*
     TO GET-FUNCTION-VALUE
*/
    *status = 1;
    return;
S280:
    switch((int)i99999){case 1: goto S10;case 2: goto S20;case 3: goto S200;
      default: break;}
#undef ftol
} /* END */
