#ifndef _CDFLIB_HEADER_
#define _CDFLIB_HEADER_
#include <stdio.h>
#include <stdlib.h>
#include <math.h>

extern double algdiv(double*,double*);
extern double alngam(double*);
extern double alnrel(double*);
extern double apser(double*,double*,double*,double*);
extern double basym(double*,double*,double*,double*);
extern double bcorr(double*,double*);
extern double betaln(double*,double*);
extern double bfrac(double*,double*,double*,double*,double*,double*);
extern void bgrat(double*,double*,double*,double*,double*,double*,int*i);
extern double bpser(double*,double*,double*,double*);
extern void bratio(double*,double*,double*,double*,double*,double*,int*);
extern double brcmp1(int*,double*,double*,double*,double*);
extern double brcomp(double*,double*,double*,double*);
extern double bup(double*,double*,double*,double*,int*,double*);
extern void cdfbet(int*,double*,double*,double*,double*,double*,double*,
                   int*,double*);
extern void cdfbin(int*,double*,double*,double*,double*,double*,double*,
                   int*,double*);
extern void cdfchi(int*,double*,double*,double*,double*,int*,double*);
extern void cdfchn(int*,double*,double*,double*,double*,double*,int*,double*);
extern void cdff(int*,double*,double*,double*,double*,double*,int*,double*);
extern void cdffnc(int*,double*,double*,double*,double*,double*,double*,
                   int*s,double*);
extern void cdfgam(int*,double*,double*,double*,double*,double*,int*,double*);
extern void cdfnbn(int*,double*,double*,double*,double*,double*,double*,
                   int*,double*);
extern void cdfnor(int*,double*,double*,double*,double*,double*,int*,double*);
extern void cdfpoi(int*,double*,double*,double*,double*,int*,double*);
extern void cdft(int*,double*,double*,double*,double*,int*,double*);
extern void cumbet(double*,double*,double*,double*,double*,double*);
extern void cumbin(double*,double*,double*,double*,double*,double*);
extern void cumchi(double*,double*,double*,double*);
extern void cumchn(double*,double*,double*,double*,double*);
extern void cumf(double*,double*,double*,double*,double*);
extern void cumfnc(double*,double*,double*,double*,double*,double*);
extern void cumgam(double*,double*,double*,double*);
extern void cumnbn(double*,double*,double*,double*,double*,double*);
extern void cumnor(double*,double*,double*);
extern void cumpoi(double*,double*,double*,double*);
extern void cumt(double*,double*,double*,double*);
extern double dbetrm(double*,double*);
extern double devlpl(double [],int*,double*);
extern double dexpm1(double*);
extern double dinvnr(double *p,double *q);
extern void E0000(int,int*,double*,double*,unsigned long*,
                  unsigned long*,double*,double*,double*,
                  double*,double*,double*,double*);
extern void dinvr(int*,double*,double*,unsigned long*,unsigned long*);
extern void dstinv(double*,double*,double*,double*,double*,double*,
                   double*);
extern double dlanor(double*);
extern double dln1mx(double*);
extern double dln1px(double*);
extern double dlnbet(double*,double*);
extern double dlngam(double*);
extern double dstrem(double*);
extern double dt1(double*,double*,double*);
extern void E0001(int,int*,double*,double*,double*,double*,
                  unsigned long*,unsigned long*,double*,double*,
                  double*,double*);
extern void dzror(int*,double*,double*,double*,double *,
                  unsigned long*,unsigned long*);
extern void dstzr(double *zxlo,double *zxhi,double *zabstl,double *zreltl);
extern double erf1(double*);
extern double erfc1(int*,double*);
extern double esum(int*,double*);
extern double exparg(int*);
extern double fpser(double*,double*,double*,double*);
extern double gam1(double*);
extern void gaminv(double*,double*,double*,double*,double*,int*);
extern double gamln(double*);
extern double gamln1(double*);
extern double Xgamm(double*);
extern void grat1(double*,double*,double*,double*,double*,double*);
extern void gratio(double*,double*,double*,double*,int*);
extern double gsumln(double*,double*);
extern double psi(double*);
extern double rcomp(double*,double*);
extern double rexp(double*);
extern double rlog(double*);
extern double rlog1(double*);
extern double spmpar(int*);
extern double stvaln(double*);
extern double fifdint(double);
extern double fifdmax1(double,double);
extern double fifdmin1(double,double);
extern double fifdsign(double,double);
extern long fifidint(double);
extern long fifmod(long,long);
extern void ftnstop(char*);
extern int ipmpar(int*);
#endif
