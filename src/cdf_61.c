#include "cdflib.h"
double gamln1(double *a)
/*
-----------------------------------------------------------------------
     EVALUATION OF LN(GAMMA(1 + A)) FOR -0.2 .LE. A .LE. 1.25
-----------------------------------------------------------------------
*/
{
static double p0 = .577215664901533e+00;
static double p1 = .844203922187225e+00;
static double p2 = -.168860593646662e+00;
static double p3 = -.780427615533591e+00;
static double p4 = -.402055799310489e+00;
static double p5 = -.673562214325671e-01;
static double p6 = -.271935708322958e-02;
static double q1 = .288743195473681e+01;
static double q2 = .312755088914843e+01;
static double q3 = .156875193295039e+01;
static double q4 = .361951990101499e+00;
static double q5 = .325038868253937e-01;
static double q6 = .667465618796164e-03;
static double r0 = .422784335098467e+00;
static double r1 = .848044614534529e+00;
static double r2 = .565221050691933e+00;
static double r3 = .156513060486551e+00;
static double r4 = .170502484022650e-01;
static double r5 = .497958207639485e-03;
static double s1 = .124313399877507e+01;
static double s2 = .548042109832463e+00;
static double s3 = .101552187439830e+00;
static double s4 = .713309612391000e-02;
static double s5 = .116165475989616e-03;
static double gamln1,w,x;
/*
     ..
     .. Executable Statements ..
*/
    if(*a >= 0.6e0) goto S10;
    w = ((((((p6**a+p5)**a+p4)**a+p3)**a+p2)**a+p1)**a+p0)/((((((q6**a+q5)**a+
      q4)**a+q3)**a+q2)**a+q1)**a+1.0e0);
    gamln1 = -(*a*w);
    return gamln1;
S10:
    x = *a-0.5e0-0.5e0;
    w = (((((r5*x+r4)*x+r3)*x+r2)*x+r1)*x+r0)/(((((s5*x+s4)*x+s3)*x+s2)*x+s1)*x
      +1.0e0);
    gamln1 = x*w;
    return gamln1;
} /* END */
