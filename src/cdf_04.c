#include "cdflib.h"
double basym(double *a,double *b,double *lambda,double *eps)
/*
-----------------------------------------------------------------------
     ASYMPTOTIC EXPANSION FOR IX(A,B) FOR LARGE A AND B.
     LAMBDA = (A + B)*Y - B  AND EPS IS THE TOLERANCE USED.
     IT IS ASSUMED THAT LAMBDA IS NONNEGATIVE AND THAT
     A AND B ARE GREATER THAN OR EQUAL TO 15.
-----------------------------------------------------------------------
*/
{
static double e0 = 1.12837916709551e0;
static double e1 = .353553390593274e0;
static int num = 20;
/*
------------------------
     ****** NUM IS THE MAXIMUM VALUE THAT N CAN TAKE IN THE DO LOOP
            ENDING AT STATEMENT 50. IT IS REQUIRED THAT NUM BE EVEN.
            THE ARRAYS A0, B0, C, D HAVE DIMENSION NUM + 1.
------------------------
     E0 = 2/SQRT(PI)
     E1 = 2**(-3/2)
------------------------
*/
static int K3 = 1;
static double basym,bsum,dsum,f,h,h2,hn,j0,j1,r,r0,r1,s,sum,t,t0,t1,u,w,w0,z,z0,
    z2,zn,znm1;
static int i,im1,imj,j,m,mm1,mmj,n,np1;
static double a0[21],b0[21],c[21],d[21],T1,T2;
/*
     ..
     .. Executable Statements ..
*/
    basym = 0.0e0;
    if(*a >= *b) goto S10;
    h = *a/ *b;
    r0 = 1.0e0/(1.0e0+h);
    r1 = (*b-*a)/ *b;
    w0 = 1.0e0/sqrt(*a*(1.0e0+h));
    goto S20;
S10:
    h = *b/ *a;
    r0 = 1.0e0/(1.0e0+h);
    r1 = (*b-*a)/ *a;
    w0 = 1.0e0/sqrt(*b*(1.0e0+h));
S20:
    T1 = -(*lambda/ *a);
    T2 = *lambda/ *b;
    f = *a*rlog1(&T1)+*b*rlog1(&T2);
    t = exp(-f);
    if(t == 0.0e0) return basym;
    z0 = sqrt(f);
    z = 0.5e0*(z0/e1);
    z2 = f+f;
    a0[0] = 2.0e0/3.0e0*r1;
    c[0] = -(0.5e0*a0[0]);
    d[0] = -c[0];
    j0 = 0.5e0/e0*erfc1(&K3,&z0);
    j1 = e1;
    sum = j0+d[0]*w0*j1;
    s = 1.0e0;
    h2 = h*h;
    hn = 1.0e0;
    w = w0;
    znm1 = z;
    zn = z2;
    for(n=2; n<=num; n+=2) {
        hn = h2*hn;
        a0[n-1] = 2.0e0*r0*(1.0e0+h*hn)/((double)n+2.0e0);
        np1 = n+1;
        s += hn;
        a0[np1-1] = 2.0e0*r1*s/((double)n+3.0e0);
        for(i=n; i<=np1; i++) {
            r = -(0.5e0*((double)i+1.0e0));
            b0[0] = r*a0[0];
            for(m=2; m<=i; m++) {
                bsum = 0.0e0;
                mm1 = m-1;
                for(j=1; j<=mm1; j++) {
                    mmj = m-j;
                    bsum += (((double)j*r-(double)mmj)*a0[j-1]*b0[mmj-1]);
                }
                b0[m-1] = r*a0[m-1]+bsum/(double)m;
            }
            c[i-1] = b0[i-1]/((double)i+1.0e0);
            dsum = 0.0e0;
            im1 = i-1;
            for(j=1; j<=im1; j++) {
                imj = i-j;
                dsum += (d[imj-1]*c[j-1]);
            }
            d[i-1] = -(dsum+c[i-1]);
        }
        j0 = e1*znm1+((double)n-1.0e0)*j0;
        j1 = e1*zn+(double)n*j1;
        znm1 = z2*znm1;
        zn = z2*zn;
        w = w0*w;
        t0 = d[n-1]*w*j0;
        w = w0*w;
        t1 = d[np1-1]*w*j1;
        sum += (t0+t1);
        if(fabs(t0)+fabs(t1) <= *eps*sum) goto S80;
    }
S80:
    u = exp(-bcorr(a,b));
    basym = e0*t*u*sum;
    return basym;
} /* END */
