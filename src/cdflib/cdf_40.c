#include "cdflib.h"
void E0000(int IENTRY,int *status,double *x,double *fx,
		  unsigned long *qleft,unsigned long *qhi,double *zabsst,
		  double *zabsto,double *zbig,double *zrelst,
		  double *zrelto,double *zsmall,double *zstpmu)
{
#define qxmon(zx,zy,zz) (int)((zx) <= (zy) && (zy) <= (zz))
static double absstp,abstol,big,fbig,fsmall,relstp,reltol,small,step,stpmul,xhi,
    xlb,xlo,xsave,xub,yy;
static int i99999;
static unsigned long qbdd,qcond,qdum1,qdum2,qincr,qlim,qok,qup;
    switch(IENTRY){case 0: goto DINVR; case 1: goto DSTINV;}
DINVR:
    if(*status > 0) goto S310;
    qcond = !qxmon(small,*x,big);
    if(qcond) ftnstop(" SMALL, X, BIG not monotone in INVR");
    xsave = *x;
/*
     See that SMALL and BIG bound the zero and set QINCR
*/
    *x = small;
/*
     GET-FUNCTION-VALUE
*/
    i99999 = 1;
    goto S300;
S10:
    fsmall = *fx;
    *x = big;
/*
     GET-FUNCTION-VALUE
*/
    i99999 = 2;
    goto S300;
S20:
    fbig = *fx;
    qincr = fbig > fsmall;
    if(!qincr) goto S50;
    if(fsmall <= 0.0e0) goto S30;
    *status = -1;
    *qleft = *qhi = 1;
    return;
S30:
    if(fbig >= 0.0e0) goto S40;
    *status = -1;
    *qleft = *qhi = 0;
    return;
S40:
    goto S80;
S50:
    if(fsmall >= 0.0e0) goto S60;
    *status = -1;
    *qleft = 1;
    *qhi = 0;
    return;
S60:
    if(fbig <= 0.0e0) goto S70;
    *status = -1;
    *qleft = 0;
    *qhi = 1;
    return;
S80:
S70:
    *x = xsave;
    step = fifdmax1(absstp,relstp*fabs(*x));
/*
      YY = F(X) - Y
     GET-FUNCTION-VALUE
*/
    i99999 = 3;
    goto S300;
S90:
    yy = *fx;
    if(!(yy == 0.0e0)) goto S100;
    *status = 0;
    qok = 1;
    return;
S100:
    qup = qincr && yy < 0.0e0 || !qincr && yy > 0.0e0;
/*
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
     HANDLE CASE IN WHICH WE MUST STEP HIGHER
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*/
    if(!qup) goto S170;
    xlb = xsave;
    xub = fifdmin1(xlb+step,big);
    goto S120;
S110:
    if(qcond) goto S150;
S120:
/*
      YY = F(XUB) - Y
*/
    *x = xub;
/*
     GET-FUNCTION-VALUE
*/
    i99999 = 4;
    goto S300;
S130:
    yy = *fx;
    qbdd = qincr && yy >= 0.0e0 || !qincr && yy <= 0.0e0;
    qlim = xub >= big;
    qcond = qbdd || qlim;
    if(qcond) goto S140;
    step = stpmul*step;
    xlb = xub;
    xub = fifdmin1(xlb+step,big);
S140:
    goto S110;
S150:
    if(!(qlim && !qbdd)) goto S160;
    *status = -1;
    *qleft = 0;
    *qhi = !qincr;
    *x = big;
    return;
S160:
    goto S240;
S170:
/*
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
     HANDLE CASE IN WHICH WE MUST STEP LOWER
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*/
    xub = xsave;
    xlb = fifdmax1(xub-step,small);
    goto S190;
S180:
    if(qcond) goto S220;
S190:
/*
      YY = F(XLB) - Y
*/
    *x = xlb;
/*
     GET-FUNCTION-VALUE
*/
    i99999 = 5;
    goto S300;
S200:
    yy = *fx;
    qbdd = qincr && yy <= 0.0e0 || !qincr && yy >= 0.0e0;
    qlim = xlb <= small;
    qcond = qbdd || qlim;
    if(qcond) goto S210;
    step = stpmul*step;
    xub = xlb;
    xlb = fifdmax1(xub-step,small);
S210:
    goto S180;
S220:
    if(!(qlim && !qbdd)) goto S230;
    *status = -1;
    *qleft = 1;
    *qhi = qincr;
    *x = small;
    return;
S240:
S230:
    dstzr(&xlb,&xub,&abstol,&reltol);
/*
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
     IF WE REACH HERE, XLB AND XUB BOUND THE ZERO OF F.
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*/
    *status = 0;
    goto S260;
S250:
    if(!(*status == 1)) goto S290;
S260:
    dzror(status,x,fx,&xlo,&xhi,&qdum1,&qdum2);
    if(!(*status == 1)) goto S280;
/*
     GET-FUNCTION-VALUE
*/
    i99999 = 6;
    goto S300;
S280:
S270:
    goto S250;
S290:
    *x = xlo;
    *status = 0;
    return;
DSTINV:
    small = *zsmall;
    big = *zbig;
    absstp = *zabsst;
    relstp = *zrelst;
    stpmul = *zstpmu;
    abstol = *zabsto;
    reltol = *zrelto;
    return;
S300:
/*
     TO GET-FUNCTION-VALUE
*/
    *status = 1;
    return;
S310:
    switch((int)i99999){case 1: goto S10;case 2: goto S20;case 3: goto S90;case 
      4: goto S130;case 5: goto S200;case 6: goto S270;default: break;}
#undef qxmon
} /* END */
