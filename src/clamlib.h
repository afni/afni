extern int cgeir_(complex *a, integer *lda, integer *n, complex *v, integer *itask, integer *ind, complex *work, integer *iwork);
/*:ref: ccopy_ 14 5 4 8 4 8 4 */
/*:ref: cgefa_ 14 5 8 4 4 4 4 */
/*:ref: cgesl_ 14 6 8 4 4 4 8 4 */
/*:ref: scasum_ 6 3 4 8 4 */
/*:ref: cdcdot_ 8 7 8 4 8 8 4 8 4 */
/*:ref: r1mach_ 6 1 4 */
/*:ref: xerror_ 14 5 13 4 4 4 124 */
/*:ref: xerrwv_ 14 11 13 4 4 4 4 4 4 4 4 4 124 */
extern int dbesj_(doublereal *x, doublereal *alpha, integer *n, doublereal *y, integer *nz);
/*:ref: d1mach_ 7 1 4 */
/*:ref: i1mach_ 4 1 4 */
/*:ref: dasyjy_ 14 8 200 7 7 7 4 7 7 4 */
/*:ref: dlngam_ 7 1 7 */
/*:ref: xerror_ 14 5 13 4 4 4 124 */
extern int dintp_(doublereal *x, doublereal *y, doublereal *xout, doublereal *yout, doublereal *ypout, integer *neqn, integer *kold, doublereal *phi, integer *ivc, integer *iv, integer *kgi, doublereal *gi, doublereal *alpha, doublereal *og, doublereal *ow, doublereal *ox, doublereal *oy);
extern int drkfs_(S_fp df, integer *neq, doublereal *t, doublereal *y, doublereal *tout, integer *info, doublereal *rtol, doublereal *atol, integer *idid, doublereal *h__, doublereal *tolfac, doublereal *yp, doublereal *f1, doublereal *f2, doublereal *f3, doublereal *f4, doublereal *f5, doublereal *ys, doublereal *told, doublereal *dtsign, doublereal *u26, doublereal *rer, integer *init, integer *ksteps, integer *kop, integer *iquit, logical *stiff, logical *nonstf, integer *ntstep, integer *nstifs, doublereal *rpar, integer *ipar);
/*:ref: d1mach_ 7 1 4 */
/*:ref: xerrwv_ 14 11 13 4 4 4 4 4 4 4 6 6 124 */
/*:ref: dvnorm_ 7 2 7 4 */
/*:ref: dhstrt_ 14 17 214 4 7 7 7 7 7 4 7 7 7 7 7 7 7 4 7 */
/*:ref: dfehl_ 14 14 214 4 7 7 7 7 7 7 7 7 7 7 7 4 */
extern integer iploc_(integer *loc, real *sx, integer *ix);
/*:ref: xerror_ 14 5 13 4 4 4 124 */
/*:ref: prwpge_ 14 5 4 4 4 6 4 */
extern int qawf_(E_fp f, real *a, real *omega, integer *integr, real *epsabs, real *result, real *abserr, integer *neval, integer *ier, integer *limlst, integer *lst, integer *leniw, integer *maxp1, integer *lenw, integer *iwork, real *work);
/*:ref: qawfe_ 14 23 206 6 6 4 6 4 4 4 6 6 4 4 6 6 4 4 6 6 6 6 4 4 6 */
/*:ref: xerror_ 14 5 13 4 4 4 124 */
extern int sinqf_(integer *n, real *x, real *wsave);
/*:ref: cosqf_ 14 3 4 6 6 */
extern int cgesl_(complex *a, integer *lda, integer *n, integer *ipvt, complex *b, integer *job);
/*:ref: caxpy_ 14 6 4 8 8 4 8 4 */
/*:ref: cdotc_ 8 6 8 4 8 4 8 4 */
extern doublereal dbesj0_(doublereal *x);
/*:ref: initds_ 4 3 7 4 6 */
/*:ref: d1mach_ 7 1 4 */
/*:ref: dcsevl_ 7 3 7 7 4 */
/*:ref: d9b0mp_ 14 3 7 7 7 */
extern int dintrp_(doublereal *x, doublereal *y, doublereal *xout, doublereal *yout, doublereal *ypout, integer *neqn, integer *kold, doublereal *phi, doublereal *psi);
extern int drot_(integer *n, doublereal *dx, integer *incx, doublereal *dy, integer *incy, doublereal *dc, doublereal *ds);
extern integer isamax_(integer *n, real *sx, integer *incx);
extern int qawfe_(U_fp f, real *a, real *omega, integer *integr, real *epsabs, integer *limlst, integer *limit, integer *maxp1, real *result, real *abserr, integer *neval, integer *ier, real *rslst, real *erlst, integer *ierlst, integer *lst, real *alist__, real *blist, real *rlist, real *elist, integer *iord, integer *nnlog, real *chebmo);
/*:ref: qagie_ 14 16 200 6 4 6 6 4 6 6 4 4 6 6 6 6 4 4 */
/*:ref: r1mach_ 6 1 4 */
/*:ref: qawoe_ 14 23 200 6 6 6 4 6 6 4 4 4 6 6 4 4 4 6 6 6 6 4 4 4 6 */
/*:ref: qelg_ 14 6 4 6 6 6 6 4 */
extern int sinqi_(integer *n, real *wsave);
/*:ref: cosqi_ 14 2 4 6 */
extern int cgtsl_(integer *n, complex *c__, complex *d__, complex *e, complex *b, integer *info);
extern doublereal dbesj1_(doublereal *x);
/*:ref: initds_ 4 3 7 4 6 */
/*:ref: d1mach_ 7 1 4 */
/*:ref: xerror_ 14 5 13 4 4 4 124 */
/*:ref: dcsevl_ 7 3 7 7 4 */
/*:ref: d9b1mp_ 14 3 7 7 7 */
extern int dintrv_(doublereal *xt, integer *lxt, doublereal *x, integer *ilo, integer *ileft, integer *mflag);
extern int drotg_(doublereal *da, doublereal *db, doublereal *dc, doublereal *ds);
extern integer isamin_(integer *n, real *sx, integer *incx);
extern int qawo_(E_fp f, real *a, real *b, real *omega, integer *integr, real *epsabs, real *epsrel, real *result, real *abserr, integer *neval, integer *ier, integer *leniw, integer *maxp1, integer *lenw, integer *last, integer *iwork, real *work);
/*:ref: qawoe_ 14 23 206 6 6 6 4 6 6 4 4 4 6 6 4 4 4 6 6 6 6 4 4 4 6 */
/*:ref: xerror_ 14 5 13 4 4 4 124 */
extern int sint_(integer *n, real *x, real *wsave);
/*:ref: rfftf_ 14 3 4 6 6 */
extern int aaaaaa_(void);
extern int ch_(integer *nm, integer *n, real *ar, real *ai, real *w, integer *matz, real *zr, real *zi, real *fv1, real *fv2, real *fm1, integer *ierr);
/*:ref: htridi_ 14 8 4 4 6 6 6 6 6 6 */
/*:ref: tqlrat_ 14 4 4 6 6 4 */
/*:ref: tql2_ 14 6 4 4 6 6 6 4 */
/*:ref: htribk_ 14 8 4 4 6 6 6 4 6 6 */
extern int dbesk_(doublereal *x, doublereal *fnu, integer *kode, integer *n, doublereal *y, integer *nz);
/*:ref: i1mach_ 4 1 4 */
/*:ref: d1mach_ 7 1 4 */
/*:ref: dasyik_ 14 8 7 7 4 7 7 7 4 7 */
/*:ref: dbesk0_ 7 1 7 */
/*:ref: dbsk0e_ 7 1 7 */
/*:ref: dbesk1_ 7 1 7 */
/*:ref: dbsk1e_ 7 1 7 */
/*:ref: dbsknu_ 14 6 7 7 4 4 7 4 */
/*:ref: xerror_ 14 5 13 4 4 4 124 */
extern int dintyd_(doublereal *t, integer *k, doublereal *yh, integer *nyh, doublereal *dky, integer *iflag);
/* comlen ddebd1_ 1876 */
extern int drotm_(integer *n, doublereal *dx, integer *incx, doublereal *dy, integer *incy, doublereal *dparam);
extern integer ismax_(integer *n, real *sx, integer *incx);
extern int qawoe_(E_fp f, real *a, real *b, real *omega, integer *integr, real *epsabs, real *epsrel, integer *limit, integer *icall, integer *maxp1, real *result, real *abserr, integer *neval, integer *ier, integer *last, real *alist__, real *blist, real *rlist, real *elist, integer *iord, integer *nnlog, integer *momcom, real *chebmo);
/*:ref: r1mach_ 6 1 4 */
/*:ref: qc25f_ 14 15 206 6 6 6 4 4 4 4 6 6 4 6 6 4 6 */
/*:ref: qpsrt_ 14 7 4 4 4 6 6 4 4 */
/*:ref: qelg_ 14 6 4 6 6 6 6 4 */
extern int sinti_(integer *n, real *wsave);
/*:ref: rffti_ 14 2 4 6 */
extern E_f acosh_(real *x);
/*:ref: r1mach_ 6 1 4 */
/*:ref: xerror_ 14 5 13 4 4 4 124 */
extern int chfdv_(real *x1, real *x2, real *f1, real *f2, real *d1, real *d2, integer *ne, real *xe, real *fe, real *de, integer *next, integer *ierr);
/*:ref: xerror_ 14 5 13 4 4 4 124 */
extern doublereal dbesk0_(doublereal *x);
/*:ref: initds_ 4 3 7 4 6 */
/*:ref: d1mach_ 7 1 4 */
/*:ref: xerror_ 14 5 13 4 4 4 124 */
/*:ref: dbesi0_ 7 1 7 */
/*:ref: dcsevl_ 7 3 7 7 4 */
/*:ref: dbsk0e_ 7 1 7 */
extern int djairy_(doublereal *x, doublereal *rx, doublereal *c__, doublereal *ai, doublereal *dai);
extern int drotmg_(doublereal *dd1, doublereal *dd2, doublereal *dx1, doublereal *dy1, doublereal *dparam);
extern integer ismin_(integer *n, real *sx, integer *incx);
extern int qaws_(E_fp f, real *a, real *b, real *alfa, real *beta, integer *integr, real *epsabs, real *epsrel, real *result, real *abserr, integer *neval, integer *ier, integer *limit, integer *lenw, integer *last, integer *iwork, real *work);
/*:ref: qawse_ 14 19 206 6 6 6 6 4 6 6 4 6 6 4 4 6 6 6 6 4 4 */
/*:ref: xerror_ 14 5 13 4 4 4 124 */
extern int sintrp_(real *x, real *y, real *xout, real *yout, real *ypout, integer *neqn, integer *kold, real *phi, integer *ivc, integer *iv, integer *kgi, real *gi, real *alpha, real *og, real *ow, real *ox, real *oy);
extern E_f ai_(real *x);
/*:ref: inits_ 4 3 6 4 6 */
/*:ref: r1mach_ 6 1 4 */
/*:ref: r9aimp_ 14 3 6 6 6 */
/*:ref: csevl_ 6 3 6 6 4 */
/*:ref: aie_ 6 1 6 */
/*:ref: xerror_ 14 5 13 4 4 4 124 */
extern int chfev_(real *x1, real *x2, real *f1, real *f2, real *d1, real *d2, integer *ne, real *xe, real *fe, integer *next, integer *ierr);
/*:ref: xerror_ 14 5 13 4 4 4 124 */
extern doublereal dbesk1_(doublereal *x);
/*:ref: initds_ 4 3 7 4 6 */
/*:ref: d1mach_ 7 1 4 */
/*:ref: xerror_ 14 5 13 4 4 4 124 */
/*:ref: dbesi1_ 7 1 7 */
/*:ref: dcsevl_ 7 3 7 7 4 */
/*:ref: dbsk1e_ 7 1 7 */
extern doublereal dlbeta_(doublereal *a, doublereal *b);
/*:ref: xerror_ 14 5 13 4 4 4 124 */
/*:ref: dgamma_ 7 1 7 */
/*:ref: d9lgmc_ 7 1 7 */
/*:ref: dlngam_ 7 1 7 */
/*:ref: dlnrel_ 7 1 7 */
extern int drsco_(doublereal *rsav, integer *isav);
/* comlen ddebd1_ 1876 */
extern int isort_(integer *x, integer *y, integer *n, integer *kflag);
/*:ref: xerror_ 14 5 13 4 4 4 124 */
extern int qawse_(E_fp f, real *a, real *b, real *alfa, real *beta, integer *integr, real *epsabs, real *epsrel, integer *limit, real *result, real *abserr, integer *neval, integer *ier, real *alist__, real *blist, real *rlist, real *elist, integer *iord, integer *last);
/*:ref: r1mach_ 6 1 4 */
/*:ref: qmomo_ 14 7 6 6 6 6 6 6 4 */
/*:ref: qc25s_ 14 16 206 6 6 6 6 6 6 6 6 6 6 6 6 6 4 4 */
/*:ref: qpsrt_ 14 7 4 4 4 6 6 4 4 */
extern int slvs_(real *wm, integer *iwm, real *x, real *tem);
/* comlen debdf1_ 1004 */
/*:ref: sgesl_ 14 6 6 4 4 4 6 4 */
/*:ref: sgbsl_ 14 8 6 4 4 4 4 4 6 4 */
extern E_f aie_(real *x);
/*:ref: r1mach_ 6 1 4 */
/*:ref: inits_ 4 3 6 4 6 */
/*:ref: r9aimp_ 14 3 6 6 6 */
/*:ref: csevl_ 6 3 6 6 4 */
extern E_f chfiv_(real *x1, real *x2, real *f1, real *f2, real *d1, real *d2, real *a, real *b, integer *ierr);
/*:ref: xerror_ 14 5 13 4 4 4 124 */
extern int dbesks_(doublereal *xnu, doublereal *x, integer *nin, doublereal *bk);
/*:ref: d1mach_ 7 1 4 */
/*:ref: xerror_ 14 5 13 4 4 4 124 */
/*:ref: dbskes_ 14 4 7 7 4 7 */
extern int dlgams_(doublereal *x, doublereal *dlgam, doublereal *sgngam);
/*:ref: dlngam_ 7 1 7 */
extern int dscal_(integer *n, doublereal *da, doublereal *dx, integer *incx);
extern int iswap_(integer *n, integer *ix, integer *incx, integer *iy, integer *incy);
extern int qc25c_(E_fp f, real *a, real *b, real *c__, real *result, real *abserr, integer *krul, integer *neval);
/*:ref: qk15w_ 14 13 206 206 6 6 6 6 4 6 6 6 6 6 6 */
/*:ref: qwgtc_ 6 :*/
/*:ref: qcheb_ 14 4 6 6 6 6 */
extern int smout_(integer *m, integer *n, integer *lda, real *a, char *ifmt, integer *idigit, ftnlen ifmt_len);
/*:ref: i1mach_ 4 1 4 */
extern E_f albeta_(real *a, real *b);
/*:ref: xerror_ 14 5 13 4 4 4 124 */
/*:ref: gamma_ 6 1 6 */
/*:ref: r9lgmc_ 6 1 6 */
/*:ref: alngam_ 6 1 6 */
/*:ref: alnrel_ 6 1 6 */
extern integer chfmc_(real *d1, real *d2, real *delta);
/*:ref: r1mach_ 6 1 4 */
extern int dbesy_(doublereal *x, doublereal *fnu, integer *n, doublereal *y);
/*:ref: i1mach_ 4 1 4 */
/*:ref: d1mach_ 7 1 4 */
/*:ref: dasyjy_ 14 8 200 7 7 7 4 7 7 4 */
/*:ref: dbesy0_ 7 1 7 */
/*:ref: dbesy1_ 7 1 7 */
/*:ref: dbsynu_ 14 4 7 7 4 7 */
/*:ref: xerror_ 14 5 13 4 4 4 124 */
extern doublereal dli_(doublereal *x);
/*:ref: xerror_ 14 5 13 4 4 4 124 */
/*:ref: dei_ 7 1 7 */
extern doublereal dsdot_(integer *n, real *sx, integer *incx, real *sy, integer *incy);
extern int ivout_(integer *n, integer *ix, char *ifmt, integer *idigit, ftnlen ifmt_len);
/*:ref: i1mach_ 4 1 4 */
extern int qc25f_(E_fp f, real *a, real *b, real *omega, integer *integr, integer *nrmom, integer *maxp1, integer *ksave, real *result, real *abserr, integer *neval, real *resabs, real *resasc, integer *momcom, real *chebmo);
/*:ref: r1mach_ 6 1 4 */
/*:ref: qk15w_ 14 13 206 206 6 6 6 6 4 6 6 6 6 6 6 */
/*:ref: qwgtf_ 6 :*/
/*:ref: sgtsl_ 14 6 4 6 6 6 6 4 */
/*:ref: qcheb_ 14 4 6 6 6 6 */
extern int snbco_(real *abe, integer *lda, integer *n, integer *ml, integer *mu, integer *ipvt, real *rcond, real *z__);
/*:ref: sasum_ 6 3 4 6 4 */
/*:ref: snbfa_ 14 7 6 4 4 4 4 4 4 */
/*:ref: sscal_ 14 4 4 6 6 4 */
/*:ref: sdot_ 6 5 4 6 4 6 4 */
/*:ref: saxpy_ 14 6 4 6 6 4 6 4 */
extern int algams_(real *x, real *algam, real *sgngam);
/*:ref: alngam_ 6 1 6 */
extern int chico_(complex *a, integer *lda, integer *n, integer *kpvt, real *rcond, complex *z__);
/*:ref: scasum_ 6 3 4 8 4 */
/*:ref: chifa_ 14 5 8 4 4 4 4 */
/*:ref: caxpy_ 14 6 4 8 8 4 8 4 */
/*:ref: csscal_ 14 4 4 6 8 4 */
/*:ref: cdotc_ 8 6 8 4 8 4 8 4 */
extern doublereal dbesy0_(doublereal *x);
/*:ref: initds_ 4 3 7 4 6 */
/*:ref: d1mach_ 7 1 4 */
/*:ref: xerror_ 14 5 13 4 4 4 124 */
/*:ref: dbesj0_ 7 1 7 */
/*:ref: dcsevl_ 7 3 7 7 4 */
/*:ref: d9b0mp_ 14 3 7 7 7 */
extern int dllsia_(doublereal *a, integer *mda, integer *m, integer *n, doublereal *b, integer *mdb, integer *nb, doublereal *re, doublereal *ae, integer *key, integer *mode, integer *np, integer *krank, integer *ksure, doublereal *rnorm, doublereal *w, integer *lw, integer *iwork, integer *liw, integer *info);
/*:ref: d1mach_ 7 1 4 */
/*:ref: du11ls_ 14 15 7 4 4 4 7 7 4 4 4 4 7 7 7 4 4 */
/*:ref: du12ls_ 14 14 7 4 4 4 7 4 4 4 4 7 7 7 4 4 */
/*:ref: xerror_ 14 5 13 4 4 4 124 */
extern int dsico_(doublereal *a, integer *lda, integer *n, integer *kpvt, doublereal *rcond, doublereal *z__);
/*:ref: dasum_ 7 3 4 7 4 */
/*:ref: dsifa_ 14 5 7 4 4 4 4 */
/*:ref: daxpy_ 14 6 4 7 7 4 7 4 */
/*:ref: dscal_ 14 4 4 7 7 4 */
/*:ref: ddot_ 7 5 4 7 4 7 4 */
extern integer j4save_(integer *iwhich, integer *ivalue, logical *iset);
extern int qc25s_(E_fp f, real *a, real *b, real *bl, real *br, real *alfa, real *beta, real *ri, real *rj, real *rg, real *rh, real *result, real *abserr, real *resasc, integer *integr, integer *nev);
/*:ref: qk15w_ 14 13 206 206 6 6 6 6 4 6 6 6 6 6 6 */
/*:ref: qwgts_ 6 :*/
/*:ref: qcheb_ 14 4 6 6 6 6 */
extern int snbdi_(real *abe, integer *lda, integer *n, integer *ml, integer *mu, integer *ipvt, real *det);
extern E_f ali_(real *x);
/*:ref: xerror_ 14 5 13 4 4 4 124 */
/*:ref: ei_ 6 1 6 */
extern int chidi_(complex *a, integer *lda, integer *n, integer *kpvt, real *det, integer *inert, complex *work, integer *job);
/*:ref: ccopy_ 14 5 4 8 4 8 4 */
/*:ref: cdotc_ 8 6 8 4 8 4 8 4 */
/*:ref: caxpy_ 14 6 4 8 8 4 8 4 */
/*:ref: cswap_ 14 5 4 8 4 8 4 */
extern doublereal dbesy1_(doublereal *x);
/*:ref: initds_ 4 3 7 4 6 */
/*:ref: d1mach_ 7 1 4 */
/*:ref: xerror_ 14 5 13 4 4 4 124 */
/*:ref: dbesj1_ 7 1 7 */
/*:ref: dcsevl_ 7 3 7 7 4 */
/*:ref: d9b1mp_ 14 3 7 7 7 */
extern doublereal dlngam_(doublereal *x);
/*:ref: d1mach_ 7 1 4 */
/*:ref: dgamma_ 7 1 7 */
/*:ref: xerror_ 14 5 13 4 4 4 124 */
/*:ref: d9lgmc_ 7 1 7 */
extern int dsidi_(doublereal *a, integer *lda, integer *n, integer *kpvt, doublereal *det, integer *inert, doublereal *work, integer *job);
/*:ref: dcopy_ 14 5 4 7 4 7 4 */
/*:ref: ddot_ 7 5 4 7 4 7 4 */
/*:ref: daxpy_ 14 6 4 7 7 4 7 4 */
/*:ref: dswap_ 14 5 4 7 4 7 4 */
extern int jairy_(real *x, real *rx, real *c__, real *ai, real *dai);
extern int qcheb_(real *x, real *fval, real *cheb12, real *cheb24);
extern int snbfa_(real *abe, integer *lda, integer *n, integer *ml, integer *mu, integer *ipvt, integer *info);
/*:ref: isamax_ 4 3 4 6 4 */
/*:ref: sswap_ 14 5 4 6 4 6 4 */
/*:ref: sscal_ 14 4 4 6 6 4 */
/*:ref: saxpy_ 14 6 4 6 6 4 6 4 */
extern E_f alngam_(real *x);
/*:ref: r1mach_ 6 1 4 */
/*:ref: gamma_ 6 1 6 */
/*:ref: xerror_ 14 5 13 4 4 4 124 */
/*:ref: r9lgmc_ 6 1 6 */
extern int chiev_(real *a, integer *lda, integer *n, real *e, real *v, integer *ldv, real *work, integer *job, integer *info);
/*:ref: xerror_ 14 5 13 4 4 4 124 */
/*:ref: scopy_ 14 5 4 6 4 6 4 */
/*:ref: scopym_ 14 5 4 6 4 6 4 */
/*:ref: htridi_ 14 8 4 4 6 6 6 6 6 6 */
/*:ref: tqlrat_ 14 4 4 6 6 4 */
/*:ref: imtql2_ 14 6 4 4 6 6 6 4 */
/*:ref: htribk_ 14 8 4 4 6 6 6 4 6 6 */
extern doublereal dbeta_(doublereal *a, doublereal *b);
/*:ref: dgamlm_ 14 2 7 7 */
/*:ref: d1mach_ 7 1 4 */
/*:ref: xerror_ 14 5 13 4 4 4 124 */
/*:ref: dgamma_ 7 1 7 */
/*:ref: dlbeta_ 7 2 7 7 */
extern doublereal dlnrel_(doublereal *x);
/*:ref: initds_ 4 3 7 4 6 */
/*:ref: d1mach_ 7 1 4 */
/*:ref: xerror_ 14 5 13 4 4 4 124 */
/*:ref: dcsevl_ 7 3 7 7 4 */
extern int dsifa_(doublereal *a, integer *lda, integer *n, integer *kpvt, integer *info);
/*:ref: idamax_ 4 3 4 7 4 */
/*:ref: dswap_ 14 5 4 7 4 7 4 */
/*:ref: daxpy_ 14 6 4 7 7 4 7 4 */
extern int la05ad_(doublereal *a, integer *ind, integer *nz, integer *ia, integer *n, integer *ip, integer *iw, doublereal *w, doublereal *g, doublereal *u);
/* comlen la05dd_ 32 */
/*:ref: xsetun_ 14 1 4 */
/*:ref: mc20ad_ 14 7 4 4 7 4 4 4 4 */
/*:ref: la05ed_ 14 7 7 4 4 4 4 4 12 */
/*:ref: xerrwv_ 14 11 13 4 4 4 4 4 4 4 6 6 124 */
/*:ref: xerror_ 14 5 13 4 4 4 124 */
extern int qelg_(integer *n, real *epstab, real *result, real *abserr, real *res3la, integer *nres);
/*:ref: r1mach_ 6 1 4 */
extern int snbfs_(real *abe, integer *lda, integer *n, integer *ml, integer *mu, real *v, integer *itask, integer *ind, real *work, integer *iwork);
/*:ref: snbco_ 14 8 6 4 4 4 4 4 6 6 */
/*:ref: r1mach_ 6 1 4 */
/*:ref: xerror_ 14 5 13 4 4 4 124 */
/*:ref: snbsl_ 14 8 6 4 4 4 4 4 6 4 */
/*:ref: xerrwv_ 14 11 13 4 4 4 4 4 4 4 4 4 124 */
extern E_f alnrel_(real *x);
/*:ref: inits_ 4 3 6 4 6 */
/*:ref: r1mach_ 6 1 4 */
/*:ref: xerror_ 14 5 13 4 4 4 124 */
/*:ref: csevl_ 6 3 6 6 4 */
extern int chifa_(complex *a, integer *lda, integer *n, integer *kpvt, integer *info);
/*:ref: icamax_ 4 3 4 8 4 */
/*:ref: cswap_ 14 5 4 8 4 8 4 */
/*:ref: caxpy_ 14 6 4 8 8 4 8 4 */
extern doublereal dbetai_(doublereal *x, doublereal *pin, doublereal *qin);
/*:ref: d1mach_ 7 1 4 */
/*:ref: xerror_ 14 5 13 4 4 4 124 */
/*:ref: dlbeta_ 7 2 7 7 */
extern int dlpdp_(doublereal *a, integer *mda, integer *m, integer *n1, integer *n2, doublereal *prgopt, doublereal *x, doublereal *wnorm, integer *mode, doublereal *ws, integer *is);
/*:ref: dcopy_ 14 5 4 7 4 7 4 */
/*:ref: dnrm2_ 7 3 4 7 4 */
/*:ref: dscal_ 14 4 4 7 7 4 */
/*:ref: dwnnls_ 14 12 7 4 4 4 4 4 7 7 7 4 4 7 */
/*:ref: ddot_ 7 5 4 7 4 7 4 */
extern doublereal dsindg_(doublereal *x);
extern int la05as_(real *a, integer *ind, integer *nz, integer *ia, integer *n, integer *ip, integer *iw, real *w, real *g, real *u);
/* comlen la05ds_ 28 */
/*:ref: xsetun_ 14 1 4 */
/*:ref: mc20as_ 14 7 4 4 6 4 4 4 4 */
/*:ref: la05es_ 14 7 6 4 4 4 4 4 12 */
/*:ref: xerrwv_ 14 11 13 4 4 4 4 4 4 4 6 6 124 */
/*:ref: xerror_ 14 5 13 4 4 4 124 */
extern int qform_(integer *m, integer *n, real *q, integer *ldq, real *wa);
extern int snbir_(real *abe, integer *lda, integer *n, integer *ml, integer *mu, real *v, integer *itask, integer *ind, real *work, integer *iwork);
/*:ref: scopy_ 14 5 4 6 4 6 4 */
/*:ref: snbfa_ 14 7 6 4 4 4 4 4 4 */
/*:ref: snbsl_ 14 8 6 4 4 4 4 4 6 4 */
/*:ref: sasum_ 6 3 4 6 4 */
/*:ref: sdsdot_ 6 6 4 6 6 4 6 4 */
/*:ref: r1mach_ 6 1 4 */
/*:ref: xerror_ 14 5 13 4 4 4 124 */
/*:ref: xerrwv_ 14 11 13 4 4 4 4 4 4 4 4 4 124 */
extern E_f asinh_(real *x);
/*:ref: inits_ 4 3 6 4 6 */
/*:ref: r1mach_ 6 1 4 */
/*:ref: csevl_ 6 3 6 6 4 */
extern int chisl_(complex *a, integer *lda, integer *n, integer *kpvt, complex *b);
/*:ref: caxpy_ 14 6 4 8 8 4 8 4 */
/*:ref: cdotc_ 8 6 8 4 8 4 8 4 */
extern int dbfqad_(D_fp f, doublereal *t, doublereal *bcoef, integer *n, integer *k, integer *id, doublereal *x1, doublereal *x2, doublereal *tol, doublereal *quad, integer *ierr, doublereal *work);
/*:ref: d1mach_ 7 1 4 */
/*:ref: dintrv_ 14 6 7 4 7 4 4 4 */
/*:ref: dbsgq8_ 14 13 207 7 7 4 4 4 7 7 4 7 7 4 7 */
/*:ref: xerror_ 14 5 13 4 4 4 124 */
extern int dlsei_(doublereal *w, integer *mdw, integer *me, integer *ma, integer *mg, integer *n, doublereal *prgopt, doublereal *x, doublereal *rnorme, doublereal *rnorml, integer *mode, doublereal *ws, integer *ip);
/*:ref: xerrwv_ 14 11 13 4 4 4 4 4 4 4 6 6 124 */
/*:ref: xerror_ 14 5 13 4 4 4 124 */
/*:ref: dasum_ 7 3 4 7 4 */
/*:ref: ddot_ 7 5 4 7 4 7 4 */
/*:ref: dswap_ 14 5 4 7 4 7 4 */
/*:ref: dh12_ 14 11 4 4 4 4 7 4 7 7 4 4 4 */
/*:ref: dcopy_ 14 5 4 7 4 7 4 */
/*:ref: dnrm2_ 7 3 4 7 4 */
/*:ref: dlsi_ 14 11 7 4 4 4 4 7 7 7 4 7 4 */
/*:ref: daxpy_ 14 6 4 7 7 4 7 4 */
/*:ref: dscal_ 14 4 4 7 7 4 */
extern int dsisl_(doublereal *a, integer *lda, integer *n, integer *kpvt, doublereal *b);
/*:ref: daxpy_ 14 6 4 7 7 4 7 4 */
/*:ref: ddot_ 7 5 4 7 4 7 4 */
extern int la05bd_(doublereal *a, integer *ind, integer *ia, integer *n, integer *ip, integer *iw, doublereal *w, doublereal *g, doublereal *b, logical *trans);
/* comlen la05dd_ 32 */
/*:ref: xsetun_ 14 1 4 */
/*:ref: xerror_ 14 5 13 4 4 4 124 */
extern int qk15_(E_fp f, real *a, real *b, real *result, real *abserr, real *resabs, real *resasc);
/*:ref: r1mach_ 6 1 4 */
extern int snbsl_(real *abe, integer *lda, integer *n, integer *ml, integer *mu, integer *ipvt, real *b, integer *job);
/*:ref: saxpy_ 14 6 4 6 6 4 6 4 */
/*:ref: sdot_ 6 5 4 6 4 6 4 */
extern int asyik_(real *x, real *fnu, integer *kode, real *flgik, real *ra, real *arg, integer *in, real *y);
/*:ref: r1mach_ 6 1 4 */
extern int chkder_(integer *m, integer *n, real *x, real *fvec, real *fjac, integer *ldfjac, real *xp, real *fvecp, integer *mode, real *err);
/*:ref: r1mach_ 6 1 4 */
extern doublereal dbi_(doublereal *x);
/*:ref: d1mach_ 7 1 4 */
/*:ref: initds_ 4 3 7 4 6 */
/*:ref: d9aimp_ 14 3 7 7 7 */
/*:ref: dcsevl_ 7 3 7 7 4 */
/*:ref: xerror_ 14 5 13 4 4 4 124 */
/*:ref: dbie_ 7 1 7 */
extern int dlsi_(doublereal *w, integer *mdw, integer *ma, integer *mg, integer *n, doublereal *prgopt, doublereal *x, doublereal *rnorm, integer *mode, doublereal *ws, integer *ip);
/*:ref: dasum_ 7 3 4 7 4 */
/*:ref: dcopy_ 14 5 4 7 4 7 4 */
/*:ref: dhfti_ 14 13 7 4 4 4 7 4 4 7 4 7 7 7 4 */
/*:ref: ddot_ 7 5 4 7 4 7 4 */
/*:ref: dswap_ 14 5 4 7 4 7 4 */
/*:ref: dh12_ 14 11 4 4 4 4 7 4 7 7 4 4 4 */
/*:ref: dlpdp_ 14 11 7 4 4 4 4 7 7 7 4 7 4 */
/*:ref: daxpy_ 14 6 4 7 7 4 7 4 */
/*:ref: dscal_ 14 4 4 7 7 4 */
extern int dslvs_(doublereal *wm, integer *iwm, doublereal *x, doublereal *tem);
/* comlen ddebd1_ 1876 */
/*:ref: dgesl_ 14 6 7 4 4 4 7 4 */
/*:ref: dgbsl_ 14 8 7 4 4 4 4 4 7 4 */
extern int la05bs_(real *a, integer *ind, integer *ia, integer *n, integer *ip, integer *iw, real *w, real *g, real *b, logical *trans);
/* comlen la05ds_ 28 */
/*:ref: xsetun_ 14 1 4 */
/*:ref: xerror_ 14 5 13 4 4 4 124 */
extern int qk15i_(E_fp f, real *boun, integer *inf, real *a, real *b, real *result, real *abserr, real *resabs, real *resasc);
/*:ref: r1mach_ 6 1 4 */
extern int snls_(S_fp fcn, S_fp jac, integer *iopt, integer *m, integer *n, real *x, real *fvec, real *fjac, integer *ldfjac, real *ftol, real *xtol, real *gtol, integer *maxfev, real *epsfcn, real *diag, integer *mode, real *factor, integer *nprint, integer *info, integer *nfev, integer *njev, integer *ipvt, real *qtf, real *wa1, real *wa2, real *wa3, real *wa4);
/*:ref: xerror_ 14 5 13 4 4 4 124 */
/*:ref: r1mach_ 6 1 4 */
/*:ref: enorm_ 6 2 4 6 */
/*:ref: fdjac2_ 14 10 214 4 4 6 6 6 4 4 6 6 */
/*:ref: qrfac_ 14 10 4 4 6 4 12 4 4 6 6 6 */
/*:ref: rwupdt_ 14 8 4 6 4 6 6 6 6 6 */
/*:ref: lmpar_ 14 12 4 6 4 4 6 6 6 6 6 6 6 6 */
extern int asyjy_(S_fp funjy, real *x, real *fnu, real *flgjy, integer *in, real *y, real *wk, integer *iflw);
/*:ref: r1mach_ 6 1 4 */
/*:ref: i1mach_ 4 1 4 */
extern int chkpr4_(integer *iorder, real *a, real *b, integer *m, integer *mbdcnd, real *c__, real *d__, integer *n, integer *nbdcnd, S_fp cofx, integer *idmn, integer *ierror);
extern doublereal dbie_(doublereal *x);
/*:ref: d1mach_ 7 1 4 */
/*:ref: initds_ 4 3 7 4 6 */
/*:ref: d9aimp_ 14 3 7 7 7 */
/*:ref: dcsevl_ 7 3 7 7 4 */
extern int dlsod_(S_fp df, integer *neq, doublereal *t, doublereal *y, doublereal *tout, doublereal *rtol, doublereal *atol, integer *idid, doublereal *ypout, doublereal *yh, doublereal *yh1, doublereal *ewt, doublereal *savf, doublereal *acor, doublereal *wm, integer *iwm, U_fp djac, logical *intout, doublereal *tstop, doublereal *tolfac, doublereal *delsgn, doublereal *rpar, integer *ipar);
/* comlen ddebd1_ 1876 */
/*:ref: d1mach_ 7 1 4 */
/*:ref: xerrwv_ 14 11 13 4 4 4 4 4 4 4 6 6 124 */
/*:ref: dhstrt_ 14 17 214 4 7 7 7 7 7 4 7 7 7 7 7 7 7 4 7 */
/*:ref: dintyd_ 14 6 7 4 7 4 7 4 */
/*:ref: dvnrms_ 7 3 4 7 7 */
/*:ref: dstod_ 14 14 4 7 7 4 7 7 7 7 7 4 214 200 7 4 */
extern int dsort_(doublereal *x, doublereal *y, integer *n, integer *kflag);
/*:ref: xerror_ 14 5 13 4 4 4 124 */
extern int la05cd_(doublereal *a, integer *ind, integer *ia, integer *n, integer *ip, integer *iw, doublereal *w, doublereal *g, doublereal *u, integer *mm);
/* comlen la05dd_ 32 */
/*:ref: xsetun_ 14 1 4 */
/*:ref: la05ed_ 14 7 7 4 4 4 4 4 12 */
/*:ref: xerrwv_ 14 11 13 4 4 4 4 4 4 4 6 6 124 */
/*:ref: xerror_ 14 5 13 4 4 4 124 */
extern int qk15w_(E_fp f, E_fp w, real *p1, real *p2, real *p3, real *p4, integer *kp, real *a, real *b, real *result, real *abserr, real *resabs, real *resasc);
/*:ref: r1mach_ 6 1 4 */
extern int snls1_(S_fp fcn, integer *iopt, integer *m, integer *n, real *x, real *fvec, real *fjac, integer *ldfjac, real *ftol, real *xtol, real *gtol, integer *maxfev, real *epsfcn, real *diag, integer *mode, real *factor, integer *nprint, integer *info, integer *nfev, integer *njev, integer *ipvt, real *qtf, real *wa1, real *wa2, real *wa3, real *wa4);
/*:ref: r1mach_ 6 1 4 */
/*:ref: enorm_ 6 2 4 6 */
/*:ref: chkder_ 14 10 4 4 6 6 6 4 6 6 4 6 */
/*:ref: xerrwv_ 14 11 13 4 4 4 4 4 4 4 6 6 124 */
/*:ref: fdjac3_ 14 10 214 4 4 6 6 6 4 4 6 6 */
/*:ref: qrfac_ 14 10 4 4 6 4 12 4 4 6 6 6 */
/*:ref: rwupdt_ 14 8 4 6 4 6 6 6 6 6 */
/*:ref: lmpar_ 14 12 4 6 4 4 6 6 6 6 6 6 6 6 */
/*:ref: xerror_ 14 5 13 4 4 4 124 */
extern E_f atanh_(real *x);
/*:ref: inits_ 4 3 6 4 6 */
/*:ref: r1mach_ 6 1 4 */
/*:ref: xerror_ 14 5 13 4 4 4 124 */
/*:ref: csevl_ 6 3 6 6 4 */
extern int chkprm_(integer *intl, integer *iorder, real *a, real *b, integer *m, integer *mbdcnd, real *c__, real *d__, integer *n, integer *nbdcnd, S_fp cofx, S_fp cofy, integer *idmn, integer *ierror);
extern doublereal dbinom_(integer *n, integer *m);
/*:ref: d1mach_ 7 1 4 */
/*:ref: xerror_ 14 5 13 4 4 4 124 */
/*:ref: d9lgmc_ 7 1 7 */
/*:ref: dlnrel_ 7 1 7 */
extern int dlssud_(doublereal *a, doublereal *x, doublereal *b, integer *n, integer *m, integer *nrda, doublereal *u, integer *nrdu, integer *iflag, integer *mlso, integer *irank, integer *iscale, doublereal *q, doublereal *diag, integer *kpivot, doublereal *s, doublereal *div, doublereal *td, integer *isflg, doublereal *scales);
/*:ref: d1mach_ 7 1 4 */
/*:ref: xgetf_ 14 1 4 */
/*:ref: j4save_ 4 3 4 4 12 */
/*:ref: xsetf_ 14 1 4 */
/*:ref: xermax_ 14 1 4 */
/*:ref: dorthr_ 14 12 7 4 4 4 4 4 4 7 4 7 7 7 */
/*:ref: dohtrl_ 14 7 7 4 4 7 4 7 7 */
/*:ref: xerror_ 14 5 13 4 4 4 124 */
/*:ref: ddot_ 7 5 4 7 4 7 4 */
extern int dsos_(D_fp fnc, integer *neq, doublereal *x, doublereal *rtolx, doublereal *atolx, doublereal *tolf, integer *iflag, doublereal *rw, integer *lrw, integer *iw, integer *liw);
/*:ref: xerrwv_ 14 11 13 4 4 4 4 4 4 4 6 6 124 */
/*:ref: dsoseq_ 14 22 207 4 7 7 7 7 4 4 4 4 4 4 7 7 4 7 7 7 7 7 7 4 */
extern int la05cs_(real *a, integer *ind, integer *ia, integer *n, integer *ip, integer *iw, real *w, real *g, real *u, integer *mm);
/* comlen la05ds_ 28 */
/*:ref: xsetun_ 14 1 4 */
/*:ref: la05es_ 14 7 6 4 4 4 4 4 12 */
/*:ref: xerrwv_ 14 11 13 4 4 4 4 4 4 4 6 6 124 */
/*:ref: xerror_ 14 5 13 4 4 4 124 */
extern int qk21_(E_fp f, real *a, real *b, real *result, real *abserr, real *resabs, real *resasc);
/*:ref: r1mach_ 6 1 4 */
extern int snls1e_(U_fp fcn, integer *iopt, integer *m, integer *n, real *x, real *fvec, real *tol, integer *nprint, integer *info, integer *iw, real *wa, integer *lwa);
/*:ref: snls1_ 14 26 200 4 4 4 6 6 6 4 6 6 6 4 6 6 4 6 4 4 4 4 4 6 6 6 6 6 */
/*:ref: xerror_ 14 5 13 4 4 4 124 */
extern int avint_(real *x, real *y, integer *n, real *xlo, real *xup, real *ans, integer *ierr);
/*:ref: xerror_ 14 5 13 4 4 4 124 */
extern int chksn4_(integer *mbdcnd, integer *nbdcnd, real *alpha, real *beta, S_fp cofx, logical *singlr);
/* comlen spl4_ 80 */
extern int dbint4_(doublereal *x, doublereal *y, integer *ndata, integer *ibcl, integer *ibcr, doublereal *fbcl, doublereal *fbcr, integer *kntopt, doublereal *t, doublereal *bcoef, integer *n, integer *k, doublereal *w);
/*:ref: d1mach_ 7 1 4 */
/*:ref: dbspvd_ 14 8 7 4 4 7 4 4 7 7 */
/*:ref: dbnfac_ 14 6 7 4 4 4 4 4 */
/*:ref: dbnslv_ 14 6 7 4 4 4 4 7 */
/*:ref: xerror_ 14 5 13 4 4 4 124 */
extern int dmacon_(void);
/* comlen dml5mc_ 52 */
/*:ref: d1mach_ 7 1 4 */
extern int dsoseq_(D_fp fnc, integer *n, doublereal *s, doublereal *rtolx, doublereal *atolx, doublereal *tolf, integer *iflag, integer *mxit, integer *ncjs, integer *nsrrc, integer *nsri, integer *iprint, doublereal *fmax, doublereal *c__, integer *nc, doublereal *b, doublereal *p, doublereal *temp, doublereal *x, doublereal *y, doublereal *fac, integer *is);
/*:ref: d1mach_ 7 1 4 */
/*:ref: i1mach_ 4 1 4 */
/*:ref: dsossl_ 14 7 4 4 4 7 7 7 4 */
extern int la05ed_(doublereal *a, integer *irn, integer *ip, integer *n, integer *iw, integer *ia, logical *reals);
/* comlen la05dd_ 32 */
extern int qk31_(E_fp f, real *a, real *b, real *result, real *abserr, real *resabs, real *resasc);
/*:ref: r1mach_ 6 1 4 */
extern int snlse_(U_fp fcn, U_fp jac, integer *iopt, integer *m, integer *n, real *x, real *fvec, real *tol, integer *nprint, integer *info, integer *iw, real *wa, integer *lwa);
/*:ref: xerror_ 14 5 13 4 4 4 124 */
/*:ref: snls_ 14 27 200 200 4 4 4 6 6 6 4 6 6 6 4 6 6 4 6 4 4 4 4 4 6 6 6 6 6 */
extern int bakvec_(integer *nm, integer *n, real *t, real *e, integer *m, real *z__, integer *ierr);
extern int chksng_(integer *mbdcnd, integer *nbdcnd, real *alpha, real *beta, real *gama, real *xnu, S_fp cofx, S_fp cofy, logical *singlr);
/* comlen splpzz_ 80 */
extern int dbintk_(doublereal *x, doublereal *y, doublereal *t, integer *n, integer *k, doublereal *bcoef, doublereal *q, doublereal *work);
/*:ref: dbspvn_ 14 9 7 4 4 4 7 4 7 7 4 */
/*:ref: dbnfac_ 14 6 7 4 4 4 4 4 */
/*:ref: dbnslv_ 14 6 7 4 4 4 4 7 */
/*:ref: xerror_ 14 5 13 4 4 4 124 */
extern int dmgsbv_(integer *m, integer *n, doublereal *a, integer *ia, integer *niv, integer *iflag, doublereal *s, doublereal *p, integer *ip, integer *inhomo, doublereal *v, doublereal *w, doublereal *wcnd);
/* comlen dml18j_ 84 */
/* comlen dml5mc_ 52 */
/*:ref: ddot_ 7 5 4 7 4 7 4 */
/*:ref: dprvec_ 7 3 4 7 7 */
extern int dsossl_(integer *k, integer *n, integer *l, doublereal *x, doublereal *c__, doublereal *b, integer *m);
extern int la05es_(real *a, integer *irn, integer *ip, integer *n, integer *iw, integer *ia, logical *reals);
/* comlen la05ds_ 28 */
extern int qk41_(E_fp f, real *a, real *b, real *result, real *abserr, real *resabs, real *resasc);
/*:ref: r1mach_ 6 1 4 */
extern E_f snrm2_(integer *n, real *sx, integer *incx);
extern int balanc_(integer *nm, integer *n, real *a, integer *low, integer *igh, real *scale);
extern int chpco_(complex *ap, integer *n, integer *kpvt, real *rcond, complex *z__);
/*:ref: scasum_ 6 3 4 8 4 */
/*:ref: chpfa_ 14 4 8 4 4 4 */
/*:ref: caxpy_ 14 6 4 8 8 4 8 4 */
/*:ref: csscal_ 14 4 4 6 8 4 */
/*:ref: cdotc_ 8 6 8 4 8 4 8 4 */
extern int dbkias_(doublereal *x, integer *n, integer *ktrms, doublereal *t, doublereal *ans, integer *ind, integer *ms, doublereal *gmrn, doublereal *h__, integer *ierr);
/*:ref: d1mach_ 7 1 4 */
/*:ref: dgamrn_ 7 1 7 */
/*:ref: dhkseq_ 14 4 7 4 7 4 */
/*:ref: dbdiff_ 14 2 4 7 */
extern int dmout_(integer *m, integer *n, integer *lda, doublereal *a, char *ifmt, integer *idigit, ftnlen ifmt_len);
/*:ref: i1mach_ 4 1 4 */
extern int dspco_(doublereal *ap, integer *n, integer *kpvt, doublereal *rcond, doublereal *z__);
/*:ref: dasum_ 7 3 4 7 4 */
/*:ref: dspfa_ 14 4 7 4 4 4 */
/*:ref: daxpy_ 14 6 4 7 7 4 7 4 */
/*:ref: dscal_ 14 4 4 7 7 4 */
/*:ref: ddot_ 7 5 4 7 4 7 4 */
extern int lagre_(integer *m, integer *k, real *aa, real *z__);
extern int qk51_(E_fp f, real *a, real *b, real *result, real *abserr, real *resabs, real *resasc);
/*:ref: r1mach_ 6 1 4 */
extern int snsq_(S_fp fcn, S_fp jac, integer *iopt, integer *n, real *x, real *fvec, real *fjac, integer *ldfjac, real *xtol, integer *maxfev, integer *ml, integer *mu, real *epsfcn, real *diag, integer *mode, real *factor, integer *nprint, integer *info, integer *nfev, integer *njev, real *r__, integer *lr, real *qtf, real *wa1, real *wa2, real *wa3, real *wa4);
/*:ref: r1mach_ 6 1 4 */
/*:ref: enorm_ 6 2 4 6 */
/*:ref: fdjac1_ 14 12 214 4 6 6 6 4 4 4 4 6 6 6 */
/*:ref: qrfac_ 14 10 4 4 6 4 12 4 4 6 6 6 */
/*:ref: qform_ 14 5 4 4 6 4 6 */
/*:ref: dogleg_ 14 9 4 6 4 6 6 6 6 6 6 */
/*:ref: r1updt_ 14 8 4 4 6 4 6 6 6 12 */
/*:ref: r1mpyq_ 14 6 4 4 6 4 6 6 */
/*:ref: xerror_ 14 5 13 4 4 4 124 */
extern int balbak_(integer *nm, integer *n, integer *low, integer *igh, real *scale, integer *m, real *z__);
extern int chpdi_(complex *ap, integer *n, integer *kpvt, real *det, integer *inert, complex *work, integer *job);
/*:ref: ccopy_ 14 5 4 8 4 8 4 */
/*:ref: cdotc_ 8 6 8 4 8 4 8 4 */
/*:ref: caxpy_ 14 6 4 8 8 4 8 4 */
/*:ref: cswap_ 14 5 4 8 4 8 4 */
extern int dbkisr_(doublereal *x, integer *n, doublereal *sum, integer *ierr);
/*:ref: d1mach_ 7 1 4 */
/*:ref: dpsixn_ 7 1 4 */
extern int dmpar_(integer *n, doublereal *r__, integer *ldr, integer *ipvt, doublereal *diag, doublereal *qtb, doublereal *delta, doublereal *par, doublereal *x, doublereal *sigma, doublereal *wa1, doublereal *wa2);
/*:ref: d1mach_ 7 1 4 */
/*:ref: denorm_ 7 2 4 7 */
/*:ref: dqrslv_ 14 9 4 7 4 4 7 7 7 7 7 */
extern int dspdi_(doublereal *ap, integer *n, integer *kpvt, doublereal *det, integer *inert, doublereal *work, integer *job);
/*:ref: dcopy_ 14 5 4 7 4 7 4 */
/*:ref: ddot_ 7 5 4 7 4 7 4 */
/*:ref: daxpy_ 14 6 4 7 7 4 7 4 */
/*:ref: dswap_ 14 5 4 7 4 7 4 */
extern int lgndr_(integer *m, integer *k, real *aa, real *z__);
extern int qk61_(E_fp f, real *a, real *b, real *result, real *abserr, real *resabs, real *resasc);
/*:ref: r1mach_ 6 1 4 */
extern int snsqe_(U_fp fcn, U_fp jac, integer *iopt, integer *n, real *x, real *fvec, real *tol, integer *nprint, integer *info, real *wa, integer *lwa);
/*:ref: snsq_ 14 27 200 200 4 4 6 6 6 4 6 4 4 4 6 6 4 6 4 4 4 4 6 4 6 6 6 6 6 */
/*:ref: xerror_ 14 5 13 4 4 4 124 */
extern int bandr_(integer *nm, integer *n, integer *mb, real *a, real *d__, real *e, real *e2, logical *matz, real *z__);
extern int chpfa_(complex *ap, integer *n, integer *kpvt, integer *info);
/*:ref: icamax_ 4 3 4 8 4 */
/*:ref: cswap_ 14 5 4 8 4 8 4 */
/*:ref: caxpy_ 14 6 4 8 8 4 8 4 */
extern int dbksol_(integer *n, doublereal *a, doublereal *x);
/*:ref: ddot_ 7 5 4 7 4 7 4 */
extern int dnbco_(doublereal *abe, integer *lda, integer *n, integer *ml, integer *mu, integer *ipvt, doublereal *rcond, doublereal *z__);
/*:ref: dasum_ 7 3 4 7 4 */
/*:ref: dnbfa_ 14 7 7 4 4 4 4 4 4 */
/*:ref: dscal_ 14 4 4 7 7 4 */
/*:ref: ddot_ 7 5 4 7 4 7 4 */
/*:ref: daxpy_ 14 6 4 7 7 4 7 4 */
extern doublereal dspenc_(doublereal *x);
/*:ref: initds_ 4 3 7 4 7 */
/*:ref: d1mach_ 7 1 4 */
/*:ref: dcsevl_ 7 3 7 7 4 */
extern int lgndrx_(integer *n, integer *k, real *aa, real *z__);
/*:ref: xerror_ 14 5 13 4 4 4 124 */
extern int qmomo_(real *alfa, real *beta, real *ri, real *rj, real *rg, real *rh, integer *integr);
extern int sods_(real *a, real *x, real *b, integer *neq, integer *nuk, integer *nrda, integer *iflag, real *work, integer *iwork);
/*:ref: lssods_ 14 20 6 6 6 4 4 4 4 4 4 6 6 4 4 6 6 6 6 6 6 6 */
extern int bandv_(integer *nm, integer *n, integer *mbw, real *a, real *e21, integer *m, real *w, real *z__, integer *ierr, integer *nv, real *rv, real *rv6);
extern int chpsl_(complex *ap, integer *n, integer *kpvt, complex *b);
/*:ref: caxpy_ 14 6 4 8 8 4 8 4 */
/*:ref: cdotc_ 8 6 8 4 8 4 8 4 */
extern int dbndac_(doublereal *g, integer *mdg, integer *nb, integer *ip, integer *ir, integer *mt, integer *jt);
/*:ref: xerror_ 14 5 13 4 4 4 124 */
/*:ref: dh12_ 14 11 4 4 4 4 7 4 7 7 4 4 4 */
extern int dnbdi_(doublereal *abe, integer *lda, integer *n, integer *ml, integer *mu, integer *ipvt, doublereal *det);
extern int dspfa_(doublereal *ap, integer *n, integer *kpvt, integer *info);
/*:ref: idamax_ 4 3 4 7 4 */
/*:ref: dswap_ 14 5 4 7 4 7 4 */
/*:ref: daxpy_ 14 6 4 7 7 4 7 4 */
extern int llsia_(real *a, integer *mda, integer *m, integer *n, real *b, integer *mdb, integer *nb, real *re, real *ae, integer *key, integer *mode, integer *np, integer *krank, integer *ksure, real *rnorm, real *w, integer *lw, integer *iwork, integer *liw, integer *info);
/*:ref: r1mach_ 6 1 4 */
/*:ref: u11ls_ 14 15 6 4 4 4 6 6 4 4 4 4 6 6 6 4 4 */
/*:ref: u12ls_ 14 14 6 4 4 4 6 4 4 4 4 6 6 6 4 4 */
/*:ref: xerror_ 14 5 13 4 4 4 124 */
extern int qnc79_(E_fp fun, real *a, real *b, real *err, real *ans, integer *ierr, integer *k);
/*:ref: xerror_ 14 5 13 4 4 4 124 */
/*:ref: r1mach_ 6 1 4 */
/*:ref: i1mach_ 4 1 4 */
extern int sopenm_(integer *ipage, integer *lpage);
/*:ref: xerrwv_ 14 11 13 4 4 4 4 4 4 4 6 6 124 */
extern E_f bcrh_(real *xll, real *xrr, integer *iz, real *c__, real *a, real *bh, E_fp f, real *sgn);
/* comlen ccblk_ 28 */
extern E_f chu_(real *a, real *b, real *x);
/*:ref: r1mach_ 6 1 4 */
/*:ref: xerror_ 14 5 13 4 4 4 124 */
/*:ref: poch_ 6 2 6 6 */
/*:ref: gamma_ 6 1 6 */
/*:ref: gamr_ 6 1 6 */
/*:ref: poch1_ 6 2 6 6 */
/*:ref: exprel_ 6 1 6 */
/*:ref: r9chu_ 6 3 6 6 6 */
extern int dbndsl_(integer *mode, doublereal *g, integer *mdg, integer *nb, integer *ip, integer *ir, doublereal *x, integer *n, doublereal *rnorm);
/*:ref: xerror_ 14 5 13 4 4 4 124 */
extern int dnbfa_(doublereal *abe, integer *lda, integer *n, integer *ml, integer *mu, integer *ipvt, integer *info);
/*:ref: idamax_ 4 3 4 7 4 */
/*:ref: dswap_ 14 5 4 7 4 7 4 */
/*:ref: dscal_ 14 4 4 7 7 4 */
/*:ref: daxpy_ 14 6 4 7 7 4 7 4 */
extern int dsplp_(U_fp dusrmt, integer *mrelas, integer *nvars__, doublereal *costs, doublereal *prgopt, doublereal *dattrv, doublereal *bl, doublereal *bu, integer *ind, integer *info, doublereal *primal, doublereal *duals, integer *ibasis, doublereal *work, integer *lw, integer *iwork, integer *liw);
/*:ref: xerrwv_ 14 11 13 4 4 4 4 4 4 4 7 7 124 */
/*:ref: xerror_ 14 5 13 4 4 4 124 */
/*:ref: dplpmn_ 14 32 200 4 4 7 7 7 7 7 4 4 7 7 7 7 7 7 7 7 7 7 7 7 7 7 4 4 4 4 4 4 4 4 */
extern int lmpar_(integer *n, real *r__, integer *ldr, integer *ipvt, real *diag, real *qtb, real *delta, real *par, real *x, real *sigma, real *wa1, real *wa2);
/*:ref: r1mach_ 6 1 4 */
/*:ref: enorm_ 6 2 4 6 */
/*:ref: qrsolv_ 14 9 4 6 4 4 6 6 6 6 6 */
extern int qng_(E_fp f, real *a, real *b, real *epsabs, real *epsrel, real *result, real *abserr, integer *neval, integer *ier);
/*:ref: r1mach_ 6 1 4 */
/*:ref: xerror_ 14 5 13 4 4 4 124 */
extern int sos_(U_fp fnc, integer *neq, real *x, real *rtolx, real *atolx, real *tolf, integer *iflag, real *rw, integer *lrw, integer *iw, integer *liw);
/*:ref: xerrwv_ 14 11 13 4 4 4 4 4 4 4 6 6 124 */
/*:ref: soseqs_ 14 22 200 4 6 6 6 6 4 4 4 4 4 4 6 6 4 6 6 6 6 6 6 4 */
extern int bdiff_(integer *l, real *v);
extern int cinvit_(integer *nm, integer *n, real *ar, real *ai, real *wr, real *wi, logical *select, integer *mm, integer *m, real *zr, real *zi, integer *ierr, real *rm1, real *rm2, real *rv1, real *rv2);
/*:ref: pythag_ 6 2 6 6 */
/*:ref: cdiv_ 14 6 6 6 6 6 6 6 */
extern int dbnfac_(doublereal *w, integer *nroww, integer *nrow, integer *nbandl, integer *nbandu, integer *iflag);
extern int dnbfs_(doublereal *abe, integer *lda, integer *n, integer *ml, integer *mu, doublereal *v, integer *itask, integer *ind, doublereal *work, integer *iwork);
/*:ref: dnbco_ 14 8 7 4 4 4 4 4 7 7 */
/*:ref: d1mach_ 7 1 4 */
/*:ref: xerror_ 14 5 13 4 4 4 124 */
/*:ref: dnbsl_ 14 8 7 4 4 4 4 4 7 4 */
/*:ref: xerrwv_ 14 11 13 4 4 4 4 4 4 4 4 4 124 */
extern int dspsl_(doublereal *ap, integer *n, integer *kpvt, doublereal *b);
/*:ref: daxpy_ 14 6 4 7 7 4 7 4 */
/*:ref: ddot_ 7 5 4 7 4 7 4 */
extern int lpdp_(real *a, integer *mda, integer *m, integer *n1, integer *n2, real *prgopt, real *x, real *wnorm, integer *mode, real *ws, integer *is);
/*:ref: scopy_ 14 5 4 6 4 6 4 */
/*:ref: snrm2_ 6 3 4 6 4 */
/*:ref: sscal_ 14 4 4 6 6 4 */
/*:ref: wnnls_ 14 12 6 4 4 4 4 4 6 6 6 4 4 6 */
/*:ref: sdot_ 6 5 4 6 4 6 4 */
extern int qpdoc_(void);
extern int soseqs_(E_fp fnc, integer *n, real *s, real *rtolx, real *atolx, real *tolf, integer *iflag, integer *mxit, integer *ncjs, integer *nsrrc, integer *nsri, integer *iprint, real *fmax, real *c__, integer *nc, real *b, real *p, real *temp, real *x, real *y, real *fac, integer *is);
/*:ref: r1mach_ 6 1 4 */
/*:ref: i1mach_ 4 1 4 */
/*:ref: sossol_ 14 7 4 4 4 6 6 6 4 */
extern int besi_(real *x, real *alpha, integer *kode, integer *n, real *y, integer *nz);
/*:ref: r1mach_ 6 1 4 */
/*:ref: i1mach_ 4 1 4 */
/*:ref: asyik_ 14 8 6 6 4 6 6 6 4 6 */
/*:ref: alngam_ 6 1 6 */
/*:ref: xerror_ 14 5 13 4 4 4 124 */
extern C_f clbeta_(complex * ret_val, complex *a, complex *b);
/*:ref: xerror_ 14 5 13 4 4 4 124 */
/*:ref: clngam_ 8 2 8 8 */
extern int dbnslv_(doublereal *w, integer *nroww, integer *nrow, integer *nbandl, integer *nbandu, doublereal *b);
extern int dnbsl_(doublereal *abe, integer *lda, integer *n, integer *ml, integer *mu, integer *ipvt, doublereal *b, integer *job);
/*:ref: daxpy_ 14 6 4 7 7 4 7 4 */
/*:ref: ddot_ 7 5 4 7 4 7 4 */
extern int dstep2_(S_fp df, integer *neqn, doublereal *y, doublereal *x, doublereal *h__, doublereal *eps, doublereal *wt, logical *start, doublereal *hold, integer *k, integer *kold, logical *crash, doublereal *phi, doublereal *p, doublereal *yp, doublereal *psi, doublereal *alpha, doublereal *beta, doublereal *sig, doublereal *v, doublereal *w, doublereal *g, logical *phase1, integer *ns, logical *nornd, integer *ksteps, doublereal *twou, doublereal *fouru, doublereal *rpar, integer *ipar);
/*:ref: d1mach_ 7 1 4 */
/*:ref: dhstrt_ 14 17 214 4 7 7 7 7 7 4 7 7 7 7 7 7 7 4 7 */
extern int lsei_(real *w, integer *mdw, integer *me, integer *ma, integer *mg, integer *n, real *prgopt, real *x, real *rnorme, real *rnorml, integer *mode, real *ws, integer *ip);
/*:ref: xerrwv_ 14 11 13 4 4 4 4 4 4 4 6 6 124 */
/*:ref: xerror_ 14 5 13 4 4 4 124 */
/*:ref: sasum_ 6 3 4 6 4 */
/*:ref: sdot_ 6 5 4 6 4 6 4 */
/*:ref: sswap_ 14 5 4 6 4 6 4 */
/*:ref: h12_ 14 11 4 4 4 4 6 4 6 6 4 4 4 */
/*:ref: scopy_ 14 5 4 6 4 6 4 */
/*:ref: snrm2_ 6 3 4 6 4 */
/*:ref: lsi_ 14 11 6 4 4 4 4 6 6 6 4 6 4 */
/*:ref: saxpy_ 14 6 4 6 6 4 6 4 */
/*:ref: sscal_ 14 4 4 6 6 4 */
extern int qpsrt_(integer *limit, integer *last, integer *maxerr, real *ermax, real *elist, integer *iord, integer *nrmax);
extern int sossol_(integer *k, integer *n, integer *l, real *x, real *c__, real *b, integer *m);
extern E_f besi0_(real *x);
/*:ref: inits_ 4 3 6 4 6 */
/*:ref: r1mach_ 6 1 4 */
/*:ref: csevl_ 6 3 6 6 4 */
/*:ref: xerror_ 14 5 13 4 4 4 124 */
/*:ref: besi0e_ 6 1 6 */
extern C_f clngam_(complex * ret_val, complex *zin);
/*:ref: r1mach_ 6 1 4 */
/*:ref: xerror_ 14 5 13 4 4 4 124 */
/*:ref: clnrel_ 8 2 8 8 */
/*:ref: c9lgmc_ 8 2 8 8 */
/*:ref: carg_ 6 1 8 */
extern int dbocls_(doublereal *w, integer *mdw, integer *mcon, integer *mrows, integer *ncols, doublereal *bl, doublereal *bu, integer *ind, integer *iopt, doublereal *x, doublereal *rnormc, doublereal *rnorm, integer *mode, doublereal *rw, integer *iw);
/*:ref: d1mach_ 7 1 4 */
/*:ref: dcopy_ 14 5 4 7 4 7 4 */
/*:ref: dbols_ 14 13 7 4 4 4 7 7 4 4 7 7 4 7 4 */
/*:ref: ddot_ 7 5 4 7 4 7 4 */
/*:ref: dasum_ 7 3 4 7 4 */
/*:ref: dnrm2_ 7 3 4 7 4 */
/*:ref: dscal_ 14 4 4 7 7 4 */
extern int dnls1_(S_fp fcn, integer *iopt, integer *m, integer *n, doublereal *x, doublereal *fvec, doublereal *fjac, integer *ldfjac, doublereal *ftol, doublereal *xtol, doublereal *gtol, integer *maxfev, doublereal *epsfcn, doublereal *diag, integer *mode, doublereal *factor, integer *nprint, integer *info, integer *nfev, integer *njev, integer *ipvt, doublereal *qtf, doublereal *wa1, doublereal *wa2, doublereal *wa3, doublereal *wa4);
/*:ref: d1mach_ 7 1 4 */
/*:ref: denorm_ 7 2 4 7 */
/*:ref: dckder_ 14 10 4 4 7 7 7 4 7 7 4 7 */
/*:ref: xerrwv_ 14 11 13 4 4 4 4 4 4 4 7 7 124 */
/*:ref: dfdjc3_ 14 10 214 4 4 7 7 7 4 4 7 7 */
/*:ref: dqrfac_ 14 10 4 4 7 4 12 4 4 7 7 7 */
/*:ref: dwupdt_ 14 8 4 7 4 7 7 7 7 7 */
/*:ref: dmpar_ 14 12 4 7 4 4 7 7 7 7 7 7 7 7 */
/*:ref: xerror_ 14 5 13 4 4 4 124 */
extern int dsteps_(S_fp df, integer *neqn, doublereal *y, doublereal *x, doublereal *h__, doublereal *eps, doublereal *wt, logical *start, doublereal *hold, integer *k, integer *kold, logical *crash, doublereal *phi, doublereal *p, doublereal *yp, doublereal *psi, doublereal *alpha, doublereal *beta, doublereal *sig, doublereal *v, doublereal *w, doublereal *g, logical *phase1, integer *ns, logical *nornd, integer *ksteps, doublereal *twou, doublereal *fouru, doublereal *xold, integer *kprev, integer *ivc, integer *iv, integer *kgi, doublereal *gi, doublereal *rpar, integer *ipar);
/*:ref: d1mach_ 7 1 4 */
/*:ref: dhstrt_ 14 17 214 4 7 7 7 7 7 4 7 7 7 7 7 7 7 4 7 */
extern int lsi_(real *w, integer *mdw, integer *ma, integer *mg, integer *n, real *prgopt, real *x, real *rnorm, integer *mode, real *ws, integer *ip);
/*:ref: sasum_ 6 3 4 6 4 */
/*:ref: scopy_ 14 5 4 6 4 6 4 */
/*:ref: hfti_ 14 13 6 4 4 4 6 4 4 6 4 6 6 6 4 */
/*:ref: sdot_ 6 5 4 6 4 6 4 */
/*:ref: sswap_ 14 5 4 6 4 6 4 */
/*:ref: h12_ 14 11 4 4 4 4 6 4 6 6 4 4 4 */
/*:ref: lpdp_ 14 11 6 4 4 4 4 6 6 6 4 6 4 */
/*:ref: saxpy_ 14 6 4 6 6 4 6 4 */
/*:ref: sscal_ 14 4 4 6 6 4 */
extern int qqsort_(integer *n, real *x, integer *j, integer *l, real *y);
extern int spbco_(real *abd, integer *lda, integer *n, integer *m, real *rcond, real *z__, integer *info);
/*:ref: sasum_ 6 3 4 6 4 */
/*:ref: spbfa_ 14 5 6 4 4 4 4 */
/*:ref: sscal_ 14 4 4 6 6 4 */
/*:ref: saxpy_ 14 6 4 6 6 4 6 4 */
/*:ref: sdot_ 6 5 4 6 4 6 4 */
extern E_f besi0e_(real *x);
/*:ref: inits_ 4 3 6 4 6 */
/*:ref: r1mach_ 6 1 4 */
/*:ref: csevl_ 6 3 6 6 4 */
extern C_f clnrel_(complex * ret_val, complex *z__);
/*:ref: r1mach_ 6 1 4 */
/*:ref: xerror_ 14 5 13 4 4 4 124 */
/*:ref: alnrel_ 6 1 6 */
/*:ref: carg_ 6 1 8 */
extern int dbols_(doublereal *w, integer *mdw, integer *mrows, integer *ncols, doublereal *bl, doublereal *bu, integer *ind, integer *iopt, doublereal *x, doublereal *rnorm, integer *mode, doublereal *rw, integer *iw);
/*:ref: dcopy_ 14 5 4 7 4 7 4 */
/*:ref: idamax_ 4 3 4 7 4 */
/*:ref: drotg_ 14 4 7 7 7 7 */
/*:ref: drot_ 14 7 4 7 4 7 4 7 7 */
/*:ref: dnrm2_ 7 3 4 7 4 */
/*:ref: dbolsm_ 14 16 7 4 4 4 7 7 4 4 7 7 4 7 7 7 4 4 */
extern int dnls1e_(U_fp fcn, integer *iopt, integer *m, integer *n, doublereal *x, doublereal *fvec, doublereal *tol, integer *nprint, integer *info, integer *iw, doublereal *wa, integer *lwa);
/*:ref: dnls1_ 14 26 200 4 4 4 7 7 7 4 7 7 7 4 7 7 4 7 4 4 4 4 4 7 7 7 7 7 */
/*:ref: xerror_ 14 5 13 4 4 4 124 */
extern int dstod_(integer *neq, doublereal *y, doublereal *yh, integer *nyh, doublereal *yh1, doublereal *ewt, doublereal *savf, doublereal *acor, doublereal *wm, integer *iwm, S_fp df, U_fp djac, doublereal *rpar, integer *ipar);
/* comlen ddebd1_ 1876 */
/*:ref: dcfod_ 14 3 4 7 7 */
/*:ref: dvnrms_ 7 3 4 7 7 */
/*:ref: dpjac_ 14 13 4 7 7 4 7 7 7 7 4 214 200 7 4 */
/*:ref: dslvs_ 14 4 7 4 7 7 */
extern int lsod_(S_fp f, integer *neq, real *t, real *y, real *tout, real *rtol, real *atol, integer *idid, real *ypout, real *yh, real *yh1, real *ewt, real *savf, real *acor, real *wm, integer *iwm, U_fp jac, logical *intout, real *tstop, real *tolfac, real *delsgn, real *rpar, integer *ipar);
/* comlen debdf1_ 1004 */
/*:ref: r1mach_ 6 1 4 */
/*:ref: xerrwv_ 14 11 13 4 4 4 4 4 4 4 6 6 124 */
/*:ref: hstart_ 14 17 214 4 6 6 6 6 6 4 6 6 6 6 6 6 6 4 6 */
/*:ref: intyd_ 14 6 6 4 6 4 6 4 */
/*:ref: vnwrms_ 6 3 4 6 6 */
/*:ref: stod_ 14 14 4 6 6 4 6 6 6 6 6 4 214 200 6 4 */
extern int qrfac_(integer *m, integer *n, real *a, integer *lda, logical *pivot, integer *ipvt, integer *lipvt, real *sigma, real *acnorm, real *wa);
/*:ref: r1mach_ 6 1 4 */
/*:ref: enorm_ 6 2 4 6 */
extern int spbdi_(real *abd, integer *lda, integer *n, integer *m, real *det);
extern E_f besi1_(real *x);
/*:ref: inits_ 4 3 6 4 6 */
/*:ref: r1mach_ 6 1 4 */
/*:ref: xerror_ 14 5 13 4 4 4 124 */
/*:ref: csevl_ 6 3 6 6 4 */
/*:ref: besi1e_ 6 1 6 */
extern C_f clog10_(complex * ret_val, complex *z__);
extern int dbolsm_(doublereal *w, integer *mdw, integer *minput, integer *ncols, doublereal *bl, doublereal *bu, integer *ind, integer *iopt, doublereal *x, doublereal *rnorm, integer *mode, doublereal *rw, doublereal *ww, doublereal *scl, integer *ibasis, integer *ibb);
/*:ref: dmout_ 14 7 4 4 4 7 13 4 124 */
/*:ref: dvout_ 14 5 4 7 13 4 124 */
/*:ref: dcopy_ 14 5 4 7 4 7 4 */
/*:ref: dnrm2_ 7 3 4 7 4 */
/*:ref: ddot_ 7 5 4 7 4 7 4 */
/*:ref: ivout_ 14 5 4 4 13 4 124 */
/*:ref: dswap_ 14 5 4 7 4 7 4 */
/*:ref: drot_ 14 7 4 7 4 7 4 7 7 */
/*:ref: daxpy_ 14 6 4 7 7 4 7 4 */
/*:ref: d1mach_ 7 1 4 */
extern doublereal dnrm2_(integer *n, doublereal *dx, integer *incx);
extern int dstor1_(doublereal *u, doublereal *yh, doublereal *v, doublereal *yp, integer *ntemp, integer *ndisk, integer *ntape);
/* comlen dml8sz_ 36 */
extern int lssods_(real *a, real *x, real *b, integer *m, integer *n, integer *nrda, integer *iflag, integer *irank, integer *iscale, real *q, real *diag, integer *kpivot, integer *iter, real *resnrm, real *xnorm, real *z__, real *r__, real *div, real *td, real *scales);
/*:ref: r1mach_ 6 1 4 */
/*:ref: xerror_ 14 5 13 4 4 4 124 */
/*:ref: xgetf_ 14 1 4 */
/*:ref: j4save_ 4 3 4 4 12 */
/*:ref: xsetf_ 14 1 4 */
/*:ref: xermax_ 14 1 4 */
/*:ref: orthol_ 14 12 6 4 4 4 4 4 4 6 4 6 6 6 */
/*:ref: ohtror_ 14 7 6 4 4 6 4 6 6 */
/*:ref: sdot_ 6 5 4 6 4 6 4 */
/*:ref: sdsdot_ 6 6 4 6 6 4 6 4 */
extern int qrsolv_(integer *n, real *r__, integer *ldr, integer *ipvt, real *diag, real *qtb, real *x, real *sigma, real *wa);
extern int spbfa_(real *abd, integer *lda, integer *n, integer *m, integer *info);
/*:ref: sdot_ 6 5 4 6 4 6 4 */
extern E_f besi1e_(real *x);
/*:ref: inits_ 4 3 6 4 6 */
/*:ref: r1mach_ 6 1 4 */
/*:ref: xerror_ 14 5 13 4 4 4 124 */
/*:ref: csevl_ 6 3 6 6 4 */
extern int cmgnbn_(integer *nperod, integer *n, integer *mperod, integer *m, complex *a, complex *b, complex *c__, integer *idimy, complex *y, integer *ierror, complex *w);
/*:ref: cmposp_ 14 16 4 4 8 8 8 8 4 8 8 8 8 8 8 8 8 8 */
/*:ref: cmposd_ 14 13 4 4 4 8 8 8 8 4 8 8 8 8 8 */
/*:ref: cmposn_ 14 18 4 4 4 4 8 8 8 8 4 8 8 8 8 8 8 8 8 8 */
extern int dbsgq8_(D_fp fun, doublereal *xt, doublereal *bc, integer *n, integer *kk, integer *id, doublereal *a, doublereal *b, integer *inbv, doublereal *err, doublereal *ans, integer *ierr, doublereal *work);
/*:ref: xerror_ 14 5 13 4 4 4 124 */
/*:ref: i1mach_ 4 1 4 */
/*:ref: d1mach_ 7 1 4 */
/*:ref: dbvalu_ 7 8 7 7 4 4 4 7 4 7 */
extern int dnsq_(S_fp fcn, S_fp jac, integer *iopt, integer *n, doublereal *x, doublereal *fvec, doublereal *fjac, integer *ldfjac, doublereal *xtol, integer *maxfev, integer *ml, integer *mu, doublereal *epsfcn, doublereal *diag, integer *mode, doublereal *factor, integer *nprint, integer *info, integer *nfev, integer *njev, doublereal *r__, integer *lr, doublereal *qtf, doublereal *wa1, doublereal *wa2, doublereal *wa3, doublereal *wa4);
/*:ref: d1mach_ 7 1 4 */
/*:ref: denorm_ 7 2 4 7 */
/*:ref: dfdjc1_ 14 12 214 4 7 7 7 4 4 4 4 7 7 7 */
/*:ref: dqrfac_ 14 10 4 4 7 4 12 4 4 7 7 7 */
/*:ref: dqform_ 14 5 4 4 7 4 7 */
/*:ref: ddoglg_ 14 9 4 7 4 7 7 7 7 7 7 */
/*:ref: d1updt_ 14 8 4 4 7 4 7 7 7 12 */
/*:ref: d1mpyq_ 14 6 4 4 7 4 7 7 */
/*:ref: xerror_ 14 5 13 4 4 4 124 */
extern int dstway_(doublereal *u, doublereal *v, doublereal *yhp, integer *inout, doublereal *stowa);
/* comlen dml8sz_ 36 */
/* comlen dml15t_ 148 */
/* comlen dml18j_ 84 */
/*:ref: dstor1_ 14 7 7 7 7 7 4 4 4 */
extern int lssuds_(real *a, real *x, real *b, integer *n, integer *m, integer *nrda, real *u, integer *nrdu, integer *iflag, integer *mlso, integer *irank, integer *iscale, real *q, real *diag, integer *kpivot, real *s, real *div, real *td, integer *isflg, real *scales);
/*:ref: r1mach_ 6 1 4 */
/*:ref: xerror_ 14 5 13 4 4 4 124 */
/*:ref: xgetf_ 14 1 4 */
/*:ref: j4save_ 4 3 4 4 12 */
/*:ref: xsetf_ 14 1 4 */
/*:ref: xermax_ 14 1 4 */
/*:ref: orthor_ 14 12 6 4 4 4 4 4 4 6 4 6 6 6 */
/*:ref: ohtrol_ 14 7 6 4 4 6 4 6 6 */
/*:ref: sdot_ 6 5 4 6 4 6 4 */
extern E_f qwgtc_(real *x, real *c__, real *p2, real *p3, real *p4, integer *kp);
extern int spbsl_(real *abd, integer *lda, integer *n, integer *m, real *b);
/*:ref: sdot_ 6 5 4 6 4 6 4 */
/*:ref: saxpy_ 14 6 4 6 6 4 6 4 */
extern int besj_(real *x, real *alpha, integer *n, real *y, integer *nz);
/*:ref: r1mach_ 6 1 4 */
/*:ref: i1mach_ 4 1 4 */
/*:ref: asyjy_ 14 8 200 6 6 6 4 6 6 4 */
/*:ref: alngam_ 6 1 6 */
/*:ref: xerror_ 14 5 13 4 4 4 124 */
extern int cmpcsg_(integer *n, integer *ijump, real *fnum, real *fden, complex *a);
/*:ref: pimach_ 6 1 6 */
extern doublereal dbsi0e_(doublereal *x);
/*:ref: d1mach_ 7 1 4 */
/*:ref: initds_ 4 3 7 4 6 */
/*:ref: dcsevl_ 7 3 7 7 4 */
extern int dnsqe_(U_fp fcn, U_fp jac, integer *iopt, integer *n, doublereal *x, doublereal *fvec, doublereal *tol, integer *nprint, integer *info, doublereal *wa, integer *lwa);
/*:ref: dnsq_ 14 27 200 200 4 4 7 7 7 4 7 4 4 4 7 7 4 7 4 4 4 4 7 4 7 7 7 7 7 */
/*:ref: xerror_ 14 5 13 4 4 4 124 */
extern int dsuds_(doublereal *a, doublereal *x, doublereal *b, integer *neq, integer *nuk, integer *nrda, integer *iflag, integer *mlso, doublereal *work, integer *iwork);
/*:ref: dlssud_ 14 20 7 7 7 4 4 4 7 4 4 4 4 4 7 7 4 7 7 7 4 7 */
extern int macon_(void);
/* comlen ml5mco_ 28 */
/*:ref: r1mach_ 6 1 4 */
extern E_f qwgtf_(real *x, real *omega, real *p2, real *p3, real *p4, integer *integr);
extern int speli4_(integer *iorder, real *a, real *b, integer *m, integer *mbdcnd, real *bda, real *alpha, real *bdb, real *beta, real *c__, real *d__, integer *n, integer *nbdcnd, real *bdc, real *bdd, S_fp cofx, real *an, real *bn, real *cn, real *dn, real *un, real *zn, real *am, real *bm, real *cm, real *dm, real *um, real *zm, real *grhs, real *usol, integer *idmn, real *w, real *pertrb, integer *ierror);
/* comlen spl4_ 80 */
/*:ref: chksn4_ 14 6 4 4 6 6 214 12 */
/*:ref: tris4_ 14 7 4 6 6 6 6 6 6 */
/*:ref: ortho4_ 14 5 6 4 6 6 6 */
/*:ref: genbun_ 14 11 4 4 4 4 6 6 6 4 6 4 6 */
/*:ref: minso4_ 14 5 6 4 6 6 6 */
/*:ref: defe4_ 14 4 214 4 6 6 */
extern E_f besj0_(real *x);
/*:ref: inits_ 4 3 6 4 6 */
/*:ref: r1mach_ 6 1 4 */
/*:ref: csevl_ 6 3 6 6 4 */
/*:ref: xerror_ 14 5 13 4 4 4 124 */
extern int cmpmrg_(complex *tcos, integer *i1, integer *m1, integer *i2, integer *m2, integer *i3);
extern doublereal dbsi1e_(doublereal *x);
/*:ref: d1mach_ 7 1 4 */
/*:ref: initds_ 4 3 7 4 6 */
/*:ref: xerror_ 14 5 13 4 4 4 124 */
/*:ref: dcsevl_ 7 3 7 7 4 */
extern int dnterv_(doublereal *xt, integer *lxt, doublereal *x, integer *ileft, integer *mflag);
extern int dsvco_(doublereal *rsav, integer *isav);
/* comlen ddebd1_ 1876 */
extern int mc20ad_(integer *nc, integer *maxa, doublereal *a, integer *inum, integer *jptr, integer *jnum, integer *jdisp);
extern E_f qwgts_(real *x, real *a, real *b, real *alfa, real *beta, integer *integr);
extern int spelip_(integer *intl, integer *iorder, real *a, real *b, integer *m, integer *mbdcnd, real *bda, real *alpha, real *bdb, real *beta, real *c__, real *d__, integer *n, integer *nbdcnd, real *bdc, real *gama, real *bdd, real *xnu, S_fp cofx, S_fp cofy, real *an, real *bn, real *cn, real *dn, real *un, real *zn, real *am, real *bm, real *cm, real *dm, real *um, real *zm, real *grhs, real *usol, integer *idmn, real *w, real *pertrb, integer *ierror);
/* comlen splpzz_ 80 */
/*:ref: chksng_ 14 9 4 4 6 6 6 6 214 214 12 */
/*:ref: trisp_ 14 7 4 6 6 6 6 6 6 */
/*:ref: blktri_ 14 15 4 4 4 6 6 6 4 4 6 6 6 4 6 4 6 */
/*:ref: orthog_ 14 5 6 4 6 6 6 */
/*:ref: minsol_ 14 5 6 4 6 6 6 */
/*:ref: defer_ 14 5 214 214 4 6 6 */
extern E_f besj1_(real *x);
/*:ref: inits_ 4 3 6 4 6 */
/*:ref: r1mach_ 6 1 4 */
/*:ref: xerror_ 14 5 13 4 4 4 124 */
/*:ref: csevl_ 6 3 6 6 4 */
extern int cmposd_(integer *mr, integer *nr, integer *istag, complex *ba, complex *bb, complex *bc, complex *q, integer *idimq, complex *b, complex *w, complex *d__, complex *tcos, complex *p);
/*:ref: cmptrx_ 14 10 4 4 4 8 8 8 8 8 8 8 */
/*:ref: cmpcsg_ 14 5 4 4 6 6 8 */
/*:ref: cmpmrg_ 14 6 8 4 4 4 4 4 */
extern doublereal dbsk0e_(doublereal *x);
/*:ref: d1mach_ 7 1 4 */
/*:ref: initds_ 4 3 7 4 6 */
/*:ref: xerror_ 14 5 13 4 4 4 124 */
/*:ref: dbesi0_ 7 1 7 */
/*:ref: dcsevl_ 7 3 7 7 4 */
extern int dogleg_(integer *n, real *r__, integer *lr, real *diag, real *qtb, real *delta, real *x, real *wa1, real *wa2);
/*:ref: r1mach_ 6 1 4 */
/*:ref: enorm_ 6 2 4 6 */
extern int dsvdc_(doublereal *x, integer *ldx, integer *n, integer *p, doublereal *s, doublereal *e, doublereal *u, integer *ldu, doublereal *v, integer *ldv, doublereal *work, integer *job, integer *info);
/*:ref: dnrm2_ 7 3 4 7 4 */
/*:ref: dscal_ 14 4 4 7 7 4 */
/*:ref: ddot_ 7 5 4 7 4 7 4 */
/*:ref: daxpy_ 14 6 4 7 7 4 7 4 */
/*:ref: drotg_ 14 4 7 7 7 7 */
/*:ref: drot_ 14 7 4 7 4 7 4 7 7 */
/*:ref: dswap_ 14 5 4 7 4 7 4 */
extern int mc20as_(integer *nc, integer *maxa, real *a, integer *inum, integer *jptr, integer *jnum, integer *jdisp);
extern int qzhes_(integer *nm, integer *n, real *a, real *b, logical *matz, real *z__);
extern E_f spenc_(real *x);
/*:ref: inits_ 4 3 6 4 6 */
/*:ref: r1mach_ 6 1 4 */
/*:ref: csevl_ 6 3 6 6 4 */
extern int besk_(real *x, real *fnu, integer *kode, integer *n, real *y, integer *nz);
/*:ref: i1mach_ 4 1 4 */
/*:ref: r1mach_ 6 1 4 */
/*:ref: asyik_ 14 8 6 6 4 6 6 6 4 6 */
/*:ref: besk0_ 6 1 6 */
/*:ref: besk0e_ 6 1 6 */
/*:ref: besk1_ 6 1 6 */
/*:ref: besk1e_ 6 1 6 */
/*:ref: besknu_ 14 6 6 6 4 4 6 4 */
/*:ref: xerror_ 14 5 13 4 4 4 124 */
extern int cmposn_(integer *m, integer *n, integer *istag, integer *mixbnd, complex *a, complex *bb, complex *c__, complex *q, integer *idimq, complex *b, complex *b2, complex *b3, complex *w, complex *w2, complex *w3, complex *d__, complex *tcos, complex *p);
/*:ref: cmpcsg_ 14 5 4 4 6 6 8 */
/*:ref: cmptrx_ 14 10 4 4 4 8 8 8 8 8 8 8 */
/*:ref: cmpmrg_ 14 6 8 4 4 4 4 4 */
/*:ref: cmptr3_ 14 13 4 8 8 8 4 8 8 8 8 8 8 8 8 */
extern doublereal dbsk1e_(doublereal *x);
/*:ref: d1mach_ 7 1 4 */
/*:ref: initds_ 4 3 7 4 6 */
/*:ref: xerror_ 14 5 13 4 4 4 124 */
/*:ref: dbesi1_ 7 1 7 */
/*:ref: dcsevl_ 7 3 7 7 4 */
extern int dohtrl_(doublereal *q, integer *n, integer *nrda, doublereal *diag, integer *irank, doublereal *div, doublereal *td);
/*:ref: ddot_ 7 5 4 7 4 7 4 */
extern int dswap_(integer *n, doublereal *dx, integer *incx, doublereal *dy, integer *incy);
extern int menu_(void);
extern int qzit_(integer *nm, integer *n, real *a, real *b, real *eps1, logical *matz, real *z__, integer *ierr);
extern int spincw_(integer *mrelas, integer *nvars__, integer *lmx, integer *lbm, integer *npp, integer *jstrt, integer *ibasis, integer *imat, integer *ibrc, integer *ipr, integer *iwr, integer *ind, integer *ibb, real *costsc, real *gg, real *erdnrm, real *dulnrm, real *amat, real *basmat, real *csc, real *wr, real *ww, real *rz, real *rg, real *costs, real *colnrm, real *duals, logical *stpedg);
/*:ref: scopy_ 14 5 4 6 4 6 4 */
/*:ref: iploc_ 4 3 4 6 4 */
/*:ref: prwpge_ 14 5 4 4 4 6 4 */
/*:ref: la05bs_ 14 10 6 4 4 4 4 4 6 6 6 12 */
/*:ref: sdot_ 6 5 4 6 4 6 4 */
extern E_f besk0_(real *x);
/*:ref: inits_ 4 3 6 4 6 */
/*:ref: r1mach_ 6 1 4 */
/*:ref: xerror_ 14 5 13 4 4 4 124 */
/*:ref: besi0_ 6 1 6 */
/*:ref: csevl_ 6 3 6 6 4 */
/*:ref: besk0e_ 6 1 6 */
extern int cmposp_(integer *m, integer *n, complex *a, complex *bb, complex *c__, complex *q, integer *idimq, complex *b, complex *b2, complex *b3, complex *w, complex *w2, complex *w3, complex *d__, complex *tcos, complex *p);
/*:ref: cmposd_ 14 13 4 4 4 8 8 8 8 4 8 8 8 8 8 */
/*:ref: cmposn_ 14 18 4 4 4 4 8 8 8 8 4 8 8 8 8 8 8 8 8 8 */
extern int dbskes_(doublereal *xnu, doublereal *x, integer *nin, doublereal *bke);
/*:ref: d1mach_ 7 1 4 */
/*:ref: xerror_ 14 5 13 4 4 4 124 */
/*:ref: d9knus_ 14 5 7 7 7 7 4 */
extern int dorthr_(doublereal *a, integer *n, integer *m, integer *nrda, integer *iflag, integer *irank, integer *iscale, doublereal *diag, integer *kpivot, doublereal *scales, doublereal *rows, doublereal *rs);
/*:ref: d1mach_ 7 1 4 */
/*:ref: xerror_ 14 5 13 4 4 4 124 */
/*:ref: ddot_ 7 5 4 7 4 7 4 */
/*:ref: dcscal_ 14 12 7 4 4 4 7 7 7 7 7 7 4 4 */
extern int dtrco_(doublereal *t, integer *ldt, integer *n, doublereal *rcond, doublereal *z__, integer *job);
/*:ref: dasum_ 7 3 4 7 4 */
/*:ref: dscal_ 14 4 4 7 7 4 */
/*:ref: daxpy_ 14 6 4 7 7 4 7 4 */
extern int merge_(real *tcos, integer *i1, integer *m1, integer *i2, integer *m2, integer *i3);
extern int qzval_(integer *nm, integer *n, real *a, real *b, real *alfr, real *alfi, real *beta, logical *matz, real *z__);
extern int spinit_(integer *mrelas, integer *nvars__, real *costs, real *bl, real *bu, integer *ind, real *primal, integer *info, real *amat, real *csc, real *costsc, real *colnrm, real *xlamda, real *anorm, real *rhs, real *rhsnrm, integer *ibasis, integer *ibb, integer *imat, logical *lopt);
/*:ref: pnnzrs_ 14 6 4 6 4 6 4 4 */
/*:ref: scopy_ 14 5 4 6 4 6 4 */
/*:ref: sasum_ 6 3 4 6 4 */
extern E_f besk0e_(real *x);
/*:ref: inits_ 4 3 6 4 6 */
/*:ref: r1mach_ 6 1 4 */
/*:ref: xerror_ 14 5 13 4 4 4 124 */
/*:ref: besi0_ 6 1 6 */
/*:ref: csevl_ 6 3 6 6 4 */
extern int cmptr3_(integer *m, complex *a, complex *b, complex *c__, integer *k, complex *y1, complex *y2, complex *y3, complex *tcos, complex *d__, complex *w1, complex *w2, complex *w3);
extern int dbskin_(doublereal *x, integer *n, integer *kode, integer *m, doublereal *y, integer *nz, integer *ierr);
/*:ref: i1mach_ 4 1 4 */
/*:ref: d1mach_ 7 1 4 */
/*:ref: dbkisr_ 14 4 7 4 7 4 */
/*:ref: dbkias_ 14 10 7 4 4 7 7 4 4 7 7 4 */
/*:ref: dexint_ 14 8 7 4 4 4 7 7 4 4 */
/*:ref: dgamrn_ 7 1 7 */
extern int dp1vlu_(integer *l, integer *nder, doublereal *x, doublereal *yfit, doublereal *yp, doublereal *a);
/*:ref: xerrwv_ 14 11 13 4 4 4 4 4 4 4 6 6 124 */
/*:ref: xerror_ 14 5 13 4 4 4 124 */
extern int dtrdi_(doublereal *t, integer *ldt, integer *n, doublereal *det, integer *job, integer *info);
/*:ref: dscal_ 14 4 4 7 7 4 */
/*:ref: daxpy_ 14 6 4 7 7 4 7 4 */
extern int mgsbv_(integer *m, integer *n, real *a, integer *ia, integer *niv, integer *iflag, real *s, real *p, integer *ip, integer *inhomo, real *v, real *w, real *wcnd);
/* comlen ml18jr_ 72 */
/* comlen ml5mco_ 28 */
/*:ref: sdot_ 6 5 4 6 4 6 4 */
/*:ref: prvec_ 6 3 4 6 6 */
extern int qzvec_(integer *nm, integer *n, real *a, real *b, real *alfr, real *alfi, real *beta, real *z__);
extern E_f spintg_(integer *l, real *v1, real *v2, real *x, real *f, real *w, integer *n, integer *ij);
extern E_f besk1_(real *x);
/*:ref: inits_ 4 3 6 4 6 */
/*:ref: r1mach_ 6 1 4 */
/*:ref: xerror_ 14 5 13 4 4 4 124 */
/*:ref: besi1_ 6 1 6 */
/*:ref: csevl_ 6 3 6 6 4 */
/*:ref: besk1e_ 6 1 6 */
extern int cmptrx_(integer *idegbr, integer *idegcr, integer *m, complex *a, complex *b, complex *c__, complex *y, complex *tcos, complex *d__, complex *w);
extern int dbsknu_(doublereal *x, doublereal *fnu, integer *kode, integer *n, doublereal *y, integer *nz);
/*:ref: i1mach_ 4 1 4 */
/*:ref: d1mach_ 7 1 4 */
/*:ref: dgamma_ 7 1 7 */
/*:ref: xerror_ 14 5 13 4 4 4 124 */
extern int dpbco_(doublereal *abd, integer *lda, integer *n, integer *m, doublereal *rcond, doublereal *z__, integer *info);
/*:ref: dasum_ 7 3 4 7 4 */
/*:ref: dpbfa_ 14 5 7 4 4 4 4 */
/*:ref: dscal_ 14 4 4 7 7 4 */
/*:ref: daxpy_ 14 6 4 7 7 4 7 4 */
/*:ref: ddot_ 7 5 4 7 4 7 4 */
extern int dtrsl_(doublereal *t, integer *ldt, integer *n, doublereal *b, integer *job, integer *info);
/*:ref: daxpy_ 14 6 4 7 7 4 7 4 */
/*:ref: ddot_ 7 5 4 7 4 7 4 */
extern int minfit_(integer *nm, integer *m, integer *n, real *a, real *w, integer *ip, real *b, integer *ierr, real *rv1);
/*:ref: pythag_ 6 2 6 6 */
extern E_f r1mach_(integer *i__);
/*:ref: xerror_ 14 5 13 4 4 4 124 */
extern int spl1d1_(integer *n, real *x, real *f, real *w, integer *iop, integer *ij, real *a, real *b, real *c__);
/*:ref: xerror_ 14 5 13 4 4 4 124 */
extern E_f besk1e_(real *x);
/*:ref: inits_ 4 3 6 4 6 */
/*:ref: r1mach_ 6 1 4 */
/*:ref: xerror_ 14 5 13 4 4 4 124 */
/*:ref: besi1_ 6 1 6 */
/*:ref: csevl_ 6 3 6 6 4 */
extern int cnbco_(complex *abe, integer *lda, integer *n, integer *ml, integer *mu, integer *ipvt, real *rcond, complex *z__);
/*:ref: scasum_ 6 3 4 8 4 */
/*:ref: cnbfa_ 14 7 8 4 4 4 4 4 4 */
/*:ref: csscal_ 14 4 4 6 8 4 */
/*:ref: cdotc_ 8 6 8 4 8 4 8 4 */
/*:ref: caxpy_ 14 6 4 8 8 4 8 4 */
extern int dbspdr_(doublereal *t, doublereal *a, integer *n, integer *k, integer *nderiv, doublereal *ad);
/*:ref: xerror_ 14 5 13 4 4 4 124 */
extern int dpbdi_(doublereal *abd, integer *lda, integer *n, integer *m, doublereal *det);
extern int du11ls_(doublereal *a, integer *mda, integer *m, integer *n, doublereal *ub, doublereal *db, integer *mode, integer *np, integer *krank, integer *ksure, doublereal *h__, doublereal *w, doublereal *eb, integer *ic, integer *ir);
/*:ref: dnrm2_ 7 3 4 7 4 */
/*:ref: xerror_ 14 5 13 4 4 4 124 */
/*:ref: dswap_ 14 5 4 7 4 7 4 */
/*:ref: iswap_ 14 5 4 4 4 4 4 */
/*:ref: idamax_ 4 3 4 7 4 */
/*:ref: dscal_ 14 4 4 7 7 4 */
/*:ref: ddot_ 7 5 4 7 4 7 4 */
/*:ref: daxpy_ 14 6 4 7 7 4 7 4 */
extern int minso4_(real *usol, integer *idmn, real *zn, real *zm, real *pertb);
/* comlen spl4_ 80 */
extern int r1mpyq_(integer *m, integer *n, real *a, integer *lda, real *v, real *w);
extern int spl1d2_(integer *n, real *x, real *f, real *w, integer *ij, real *y, real *tab);
/*:ref: interv_ 14 5 6 4 6 4 4 */
extern int beskes_(real *xnu, real *x, integer *nin, real *bke);
/*:ref: r1mach_ 6 1 4 */
/*:ref: xerror_ 14 5 13 4 4 4 124 */
/*:ref: r9knus_ 14 5 6 6 6 6 4 */
extern int cnbdi_(complex *abe, integer *lda, integer *n, integer *ml, integer *mu, integer *ipvt, complex *det);
extern int dbspev_(doublereal *t, doublereal *ad, integer *n, integer *k, integer *nderiv, doublereal *x, integer *inev, doublereal *svalue, doublereal *work);
/*:ref: dintrv_ 14 6 7 4 7 4 4 4 */
/*:ref: dbspvn_ 14 9 7 4 4 4 7 4 7 7 4 */
/*:ref: xerror_ 14 5 13 4 4 4 124 */
extern int dpbfa_(doublereal *abd, integer *lda, integer *n, integer *m, integer *info);
/*:ref: ddot_ 7 5 4 7 4 7 4 */
extern int du11us_(doublereal *a, integer *mda, integer *m, integer *n, doublereal *ub, doublereal *db, integer *mode, integer *np, integer *krank, integer *ksure, doublereal *h__, doublereal *w, doublereal *eb, integer *ir, integer *ic);
/*:ref: dnrm2_ 7 3 4 7 4 */
/*:ref: xerror_ 14 5 13 4 4 4 124 */
/*:ref: dswap_ 14 5 4 7 4 7 4 */
/*:ref: iswap_ 14 5 4 4 4 4 4 */
/*:ref: idamax_ 4 3 4 7 4 */
/*:ref: dscal_ 14 4 4 7 7 4 */
/*:ref: ddot_ 7 5 4 7 4 7 4 */
/*:ref: daxpy_ 14 6 4 7 7 4 7 4 */
extern int minsol_(real *usol, integer *idmn, real *zn, real *zm, real *pertb);
/* comlen splpzz_ 80 */
extern int r1updt_(integer *m, integer *n, real *s, integer *ls, real *u, real *v, real *w, logical *sing);
/*:ref: r1mach_ 6 1 4 */
extern int spl2d1_(integer *nbrx, real *x, integer *nbry, real *y, real *f, real *fx, real *fy, real *fxy, integer *maxy, integer *ibd, real *t1, real *t2, real *t3);
/*:ref: spl1d1_ 14 9 4 6 6 6 4 4 6 6 6 */
extern int besknu_(real *x, real *fnu, integer *kode, integer *n, real *y, integer *nz);
/*:ref: i1mach_ 4 1 4 */
/*:ref: r1mach_ 6 1 4 */
/*:ref: gamma_ 6 1 6 */
/*:ref: xerror_ 14 5 13 4 4 4 124 */
extern int cnbfa_(complex *abe, integer *lda, integer *n, integer *ml, integer *mu, integer *ipvt, integer *info);
/*:ref: icamax_ 4 3 4 8 4 */
/*:ref: cswap_ 14 5 4 8 4 8 4 */
/*:ref: cscal_ 14 4 4 8 8 4 */
/*:ref: caxpy_ 14 6 4 8 8 4 8 4 */
extern int dbsppp_(doublereal *t, doublereal *a, integer *n, integer *k, integer *ldc, doublereal *c__, doublereal *xi, integer *lxi, doublereal *work);
/*:ref: dbspdr_ 14 6 7 7 4 4 4 7 */
/*:ref: dbspev_ 14 9 7 7 4 4 4 7 4 7 7 */
/*:ref: xerror_ 14 5 13 4 4 4 124 */
extern int dpbsl_(doublereal *abd, integer *lda, integer *n, integer *m, doublereal *b);
/*:ref: ddot_ 7 5 4 7 4 7 4 */
/*:ref: daxpy_ 14 6 4 7 7 4 7 4 */
extern int du12ls_(doublereal *a, integer *mda, integer *m, integer *n, doublereal *b, integer *mdb, integer *nb, integer *mode, integer *krank, doublereal *rnorm, doublereal *h__, doublereal *w, integer *ic, integer *ir);
/*:ref: dnrm2_ 7 3 4 7 4 */
/*:ref: ddot_ 7 5 4 7 4 7 4 */
/*:ref: daxpy_ 14 6 4 7 7 4 7 4 */
/*:ref: dswap_ 14 5 4 7 4 7 4 */
extern int mpadd_(integer *x, integer *y, integer *z__);
/*:ref: mpadd2_ 14 5 4 4 4 4 4 */
extern int r9aimp_(real *x, real *ampl, real *theta);
/*:ref: r1mach_ 6 1 4 */
/*:ref: inits_ 4 3 6 4 6 */
/*:ref: csevl_ 6 3 6 6 4 */
/*:ref: xerror_ 14 5 13 4 4 4 124 */
extern E_f spl2d2_(real *xb, real *yb, integer *nbrx, real *x, integer *nbry, real *y, real *f, real *fx, real *fy, real *fxy, integer *maxy, integer *ixd, integer *iyd);
/* comlen spl2d4_ 160 */
/*:ref: interv_ 14 5 6 4 6 4 4 */
/*:ref: spl2d3_ 6 2 4 4 */
extern int besks_(real *xnu, real *x, integer *nin, real *bk);
/*:ref: r1mach_ 6 1 4 */
/*:ref: xerror_ 14 5 13 4 4 4 124 */
/*:ref: beskes_ 14 4 6 6 4 6 */
extern int cnbfs_(complex *abe, integer *lda, integer *n, integer *ml, integer *mu, complex *v, integer *itask, integer *ind, complex *work, integer *iwork);
/*:ref: cnbco_ 14 8 8 4 4 4 4 4 6 8 */
/*:ref: r1mach_ 6 1 4 */
/*:ref: xerror_ 14 5 13 4 4 4 124 */
/*:ref: cnbsl_ 14 8 8 4 4 4 4 4 8 4 */
/*:ref: xerrwv_ 14 11 13 4 4 4 4 4 4 4 4 4 124 */
extern int dbspvd_(doublereal *t, integer *k, integer *nderiv, doublereal *x, integer *ileft, integer *ldvnik, doublereal *vnikx, doublereal *work);
/*:ref: dbspvn_ 14 9 7 4 4 4 7 4 7 7 4 */
/*:ref: xerror_ 14 5 13 4 4 4 124 */
extern int dpchce_(integer *ic, doublereal *vc, integer *n, doublereal *x, doublereal *h__, doublereal *slope, doublereal *d__, integer *incfd, integer *ierr);
/*:ref: dpchdf_ 7 4 4 7 7 4 */
/*:ref: dpchst_ 7 2 7 7 */
/*:ref: xerror_ 14 5 13 4 4 4 124 */
extern int du12us_(doublereal *a, integer *mda, integer *m, integer *n, doublereal *b, integer *mdb, integer *nb, integer *mode, integer *krank, doublereal *rnorm, doublereal *h__, doublereal *w, integer *ir, integer *ic);
/*:ref: dnrm2_ 7 3 4 7 4 */
/*:ref: ddot_ 7 5 4 7 4 7 4 */
/*:ref: daxpy_ 14 6 4 7 7 4 7 4 */
/*:ref: dswap_ 14 5 4 7 4 7 4 */
extern int mpadd2_(integer *x, integer *y, integer *z__, integer *y1, integer *trunc);
/* comlen mpcom_ 68 */
/*:ref: mpstr_ 14 2 4 4 */
/*:ref: mpchk_ 14 2 4 4 */
/*:ref: mperr_ 14 0 */
/*:ref: mpadd3_ 14 5 4 4 4 4 4 */
/*:ref: mpnzr_ 14 4 4 4 4 4 */
extern E_f r9atn1_(real *x);
/*:ref: r1mach_ 6 1 4 */
/*:ref: inits_ 4 3 6 4 6 */
/*:ref: csevl_ 6 3 6 6 4 */
/*:ref: xerror_ 14 5 13 4 4 4 124 */
extern E_f spl2d3_(integer *ixd, integer *iyd);
/* comlen spl2d4_ 160 */
extern int besy_(real *x, real *fnu, integer *n, real *y);
/*:ref: i1mach_ 4 1 4 */
/*:ref: r1mach_ 6 1 4 */
/*:ref: asyjy_ 14 8 200 6 6 6 4 6 6 4 */
/*:ref: besy0_ 6 1 6 */
/*:ref: besy1_ 6 1 6 */
/*:ref: besynu_ 14 4 6 6 4 6 */
/*:ref: xerror_ 14 5 13 4 4 4 124 */
extern int cnbir_(complex *abe, integer *lda, integer *n, integer *ml, integer *mu, complex *v, integer *itask, integer *ind, complex *work, integer *iwork);
/*:ref: ccopy_ 14 5 4 8 4 8 4 */
/*:ref: cnbfa_ 14 7 8 4 4 4 4 4 4 */
/*:ref: cnbsl_ 14 8 8 4 4 4 4 4 8 4 */
/*:ref: scasum_ 6 3 4 8 4 */
/*:ref: cdcdot_ 8 7 8 4 8 8 4 8 4 */
/*:ref: r1mach_ 6 1 4 */
/*:ref: xerror_ 14 5 13 4 4 4 124 */
/*:ref: xerrwv_ 14 11 13 4 4 4 4 4 4 4 4 4 124 */
extern int dbspvn_(doublereal *t, integer *jhigh, integer *k, integer *index, doublereal *x, integer *ileft, doublereal *vnikx, doublereal *work, integer *iwork);
/*:ref: xerror_ 14 5 13 4 4 4 124 */
extern int dpchci_(integer *n, doublereal *h__, doublereal *slope, doublereal *d__, integer *incfd);
/*:ref: dpchst_ 7 2 7 7 */
extern int dulsia_(doublereal *a, integer *mda, integer *m, integer *n, doublereal *b, integer *mdb, integer *nb, doublereal *re, doublereal *ae, integer *key, integer *mode, integer *np, integer *krank, integer *ksure, doublereal *rnorm, doublereal *w, integer *lw, integer *iwork, integer *liw, integer *info);
/*:ref: d1mach_ 7 1 4 */
/*:ref: du11us_ 14 15 7 4 4 4 7 7 4 4 4 4 7 7 7 4 4 */
/*:ref: du12us_ 14 14 7 4 4 4 7 4 4 4 4 7 7 7 4 4 */
/*:ref: xerror_ 14 5 13 4 4 4 124 */
extern int mpadd3_(integer *x, integer *y, integer *s, integer *med, integer *re);
/* comlen mpcom_ 68 */
extern E_f r9chu_(real *a, real *b, real *z__);
/*:ref: r1mach_ 6 1 4 */
/*:ref: xerror_ 14 5 13 4 4 4 124 */
extern E_f splin2_(integer *nx, real *x, integer *ny, real *y, real *f, real *fxx, real *fyy, real *fxxyy, integer *maxy, real *a, real *b, real *c__, real *d__, real *t1, real *t2);
/*:ref: interv_ 14 5 6 4 6 4 4 */
/*:ref: splint_ 6 7 4 6 6 6 4 6 6 */
/*:ref: xerror_ 14 5 13 4 4 4 124 */
extern E_f besy0_(real *x);
/*:ref: inits_ 4 3 6 4 6 */
/*:ref: r1mach_ 6 1 4 */
/*:ref: xerror_ 14 5 13 4 4 4 124 */
/*:ref: besj0_ 6 1 6 */
/*:ref: csevl_ 6 3 6 6 4 */
extern int cnbsl_(complex *abe, integer *lda, integer *n, integer *ml, integer *mu, integer *ipvt, complex *b, integer *job);
/*:ref: caxpy_ 14 6 4 8 8 4 8 4 */
/*:ref: cdotc_ 8 6 8 4 8 4 8 4 */
extern int dbsqad_(doublereal *t, doublereal *bcoef, integer *n, integer *k, doublereal *x1, doublereal *x2, doublereal *bquad, doublereal *work);
/*:ref: dintrv_ 14 6 7 4 7 4 4 4 */
/*:ref: dbvalu_ 7 8 7 7 4 4 4 7 4 7 */
/*:ref: xerror_ 14 5 13 4 4 4 124 */
extern int dpchcs_(doublereal *switch__, integer *n, doublereal *h__, doublereal *slope, doublereal *d__, integer *incfd, integer *ierr);
/*:ref: dpchst_ 7 2 7 7 */
/*:ref: dpchsw_ 14 7 7 4 7 7 7 7 4 */
extern int dusrmt_(integer *i__, integer *j, doublereal *aij, integer *indcat, doublereal *prgopt, doublereal *dattrv, integer *iflag);
extern int mpblas_(integer *i1);
/* comlen mpcom_ 68 */
/*:ref: i1mach_ 4 1 4 */
extern E_f r9gmic_(real *a, real *x, real *alx);
/*:ref: r1mach_ 6 1 4 */
/*:ref: xerror_ 14 5 13 4 4 4 124 */
/*:ref: alngam_ 6 1 6 */
extern E_f splint_(integer *n, real *x, real *f, real *w, integer *ij, real *a, real *b);
/*:ref: xerror_ 14 5 13 4 4 4 124 */
/*:ref: interv_ 14 5 6 4 6 4 4 */
/*:ref: spintg_ 6 8 4 6 6 6 6 6 4 4 */
extern E_f besy1_(real *x);
/*:ref: inits_ 4 3 6 4 6 */
/*:ref: r1mach_ 6 1 4 */
/*:ref: xerror_ 14 5 13 4 4 4 124 */
/*:ref: besj1_ 6 1 6 */
/*:ref: csevl_ 6 3 6 6 4 */
extern int combak_(integer *nm, integer *low, integer *igh, real *ar, real *ai, integer *int__, integer *m, real *zr, real *zi);
extern int dbsynu_(doublereal *x, doublereal *fnu, integer *n, doublereal *y);
/*:ref: d1mach_ 7 1 4 */
/*:ref: dgamma_ 7 1 7 */
/*:ref: xerror_ 14 5 13 4 4 4 124 */
extern doublereal dpchdf_(integer *k, doublereal *x, doublereal *s, integer *ierr);
/*:ref: xerror_ 14 5 13 4 4 4 124 */
extern int dvecs_(integer *ncomp, integer *lnfc, doublereal *yhp, doublereal *work, integer *iwork, integer *inhomo, integer *iflag);
/* comlen dml18j_ 84 */
/*:ref: dmgsbv_ 14 13 4 4 7 4 4 4 7 7 4 4 7 7 7 */
extern int mpcdm_(doublereal *dx, integer *z__);
/* comlen mpcom_ 68 */
/*:ref: mpchk_ 14 2 4 4 */
/*:ref: mpnzr_ 14 4 4 4 4 4 */
/*:ref: mpdivi_ 14 3 4 4 4 */
/*:ref: mpmuli_ 14 3 4 4 4 */
extern E_f r9gmit_(real *a, real *x, real *algap1, real *sgngam, real *alx);
/*:ref: r1mach_ 6 1 4 */
/*:ref: xerror_ 14 5 13 4 4 4 124 */
/*:ref: alngam_ 6 1 6 */
extern int splp_(U_fp usrmat, integer *mrelas, integer *nvars__, real *costs, real *prgopt, real *dattrv, real *bl, real *bu, integer *ind, integer *info, real *primal, real *duals, integer *ibasis, real *work, integer *lw, integer *iwork, integer *liw);
/*:ref: xerrwv_ 14 11 13 4 4 4 4 4 4 4 6 6 124 */
/*:ref: xerror_ 14 5 13 4 4 4 124 */
/*:ref: splpmn_ 14 32 200 4 4 6 6 6 6 6 4 4 6 6 6 6 6 6 6 6 6 6 6 6 6 6 4 4 4 4 4 4 4 4 */
extern int besynu_(real *x, real *fnu, integer *n, real *y);
/*:ref: r1mach_ 6 1 4 */
/*:ref: gamma_ 6 1 6 */
/*:ref: xerror_ 14 5 13 4 4 4 124 */
extern int comhes_(integer *nm, integer *n, integer *low, integer *igh, real *ar, real *ai, integer *int__);
/*:ref: cdiv_ 14 6 6 6 6 6 6 6 */
extern doublereal dbvalu_(doublereal *t, doublereal *a, integer *n, integer *k, integer *ideriv, doublereal *x, integer *inbv, doublereal *work);
/*:ref: dintrv_ 14 6 7 4 7 4 4 4 */
/*:ref: xerror_ 14 5 13 4 4 4 124 */
extern int dpchfd_(integer *n, doublereal *x, doublereal *f, doublereal *d__, integer *incfd, logical *skip, integer *ne, doublereal *xe, doublereal *fe, doublereal *de, integer *ierr);
/*:ref: dchfdv_ 14 12 7 7 7 7 7 7 4 7 7 7 4 4 */
/*:ref: xerror_ 14 5 13 4 4 4 124 */
extern doublereal dvnorm_(doublereal *v, integer *ncomp);
extern int mpchk_(integer *i__, integer *j);
/* comlen mpcom_ 68 */
/*:ref: i1mach_ 4 1 4 */
/*:ref: mperr_ 14 0 */
extern int r9knus_(real *xnu, real *x, real *bknu, real *bknu1, integer *iswtch);
/*:ref: inits_ 4 3 6 4 6 */
/*:ref: r1mach_ 6 1 4 */
/*:ref: xerror_ 14 5 13 4 4 4 124 */
/*:ref: gamma_ 6 1 6 */
/*:ref: csevl_ 6 3 6 6 4 */
extern int splpce_(integer *mrelas, integer *nvars__, integer *lmx, integer *lbm, integer *itlp, integer *itbrc, integer *ibasis, integer *imat, integer *ibrc, integer *ipr, integer *iwr, integer *ind, integer *ibb, real *erdnrm, real *eps, real *tune, real *gg, real *amat, real *basmat, real *csc, real *wr, real *ww, real *primal, real *erd, real *erp, logical *singlr, logical *redbas);
/*:ref: la05bs_ 14 10 6 4 4 4 4 4 6 6 6 12 */
/*:ref: sasum_ 6 3 4 6 4 */
/*:ref: scopy_ 14 5 4 6 4 6 4 */
/*:ref: iploc_ 4 3 4 6 4 */
/*:ref: prwpge_ 14 5 4 4 4 6 4 */
extern E_f beta_(real *a, real *b);
/*:ref: gamlim_ 14 2 6 6 */
/*:ref: r1mach_ 6 1 4 */
/*:ref: xerror_ 14 5 13 4 4 4 124 */
/*:ref: gamma_ 6 1 6 */
/*:ref: albeta_ 6 2 6 6 */
extern int comlr_(integer *nm, integer *n, integer *low, integer *igh, real *hr, real *hi, real *wr, real *wi, integer *ierr);
/*:ref: csroot_ 14 4 6 6 6 6 */
/*:ref: cdiv_ 14 6 6 6 6 6 6 6 */
extern int dbvder_(doublereal *x, doublereal *y, doublereal *yp, doublereal *g, integer *ipar);
/* comlen dml8sz_ 36 */
/* comlen dmlivp_ 4 */
/*:ref: duivp_ 14 3 7 7 7 */
/*:ref: dfmat_ 14 3 7 7 7 */
/*:ref: dgvec_ 14 2 7 7 */
/*:ref: duvec_ 14 3 7 7 7 */
extern int dpchfe_(integer *n, doublereal *x, doublereal *f, doublereal *d__, integer *incfd, logical *skip, integer *ne, doublereal *xe, doublereal *fe, integer *ierr);
/*:ref: dchfev_ 14 11 7 7 7 7 7 7 4 7 7 4 4 */
/*:ref: xerror_ 14 5 13 4 4 4 124 */
extern doublereal dvnrms_(integer *n, doublereal *v, doublereal *w);
extern int mpcmd_(integer *x, doublereal *dz);
/* comlen mpcom_ 68 */
/*:ref: mpchk_ 14 2 4 4 */
/*:ref: mperr_ 14 0 */
extern E_f r9lgic_(real *a, real *x, real *alx);
/*:ref: r1mach_ 6 1 4 */
/*:ref: xerror_ 14 5 13 4 4 4 124 */
extern int splpdm_(integer *mrelas, integer *nvars__, integer *lmx, integer *lbm, integer *nredc, integer *info, integer *iopt, integer *ibasis, integer *imat, integer *ibrc, integer *ipr, integer *iwr, integer *ind, integer *ibb, real *anorm, real *eps, real *uu, real *gg, real *amat, real *basmat, real *csc, real *wr, logical *singlr, logical *redbas);
/* comlen la05ds_ 28 */
/*:ref: pnnzrs_ 14 6 4 6 4 6 4 4 */
/*:ref: sasum_ 6 3 4 6 4 */
/*:ref: la05as_ 14 10 6 4 4 4 4 4 4 6 6 6 */
/*:ref: xerror_ 14 5 13 4 4 4 124 */
/*:ref: xerrwv_ 14 11 13 4 4 4 4 4 4 4 6 6 124 */
extern E_f betai_(real *x, real *pin, real *qin);
/*:ref: r1mach_ 6 1 4 */
/*:ref: xerror_ 14 5 13 4 4 4 124 */
/*:ref: albeta_ 6 2 6 6 */
extern int comlr2_(integer *nm, integer *n, integer *low, integer *igh, integer *int__, real *hr, real *hi, real *wr, real *wi, real *zr, real *zi, integer *ierr);
/*:ref: csroot_ 14 4 6 6 6 6 */
/*:ref: cdiv_ 14 6 6 6 6 6 6 6 */
extern doublereal dbvlue_(doublereal *t, doublereal *a, integer *n, integer *k, doublereal *x, integer *ideriv);
/*:ref: dnterv_ 14 5 7 4 7 4 4 */
extern doublereal dpchia_(integer *n, doublereal *x, doublereal *f, doublereal *d__, integer *incfd, logical *skip, doublereal *a, doublereal *b, integer *ierr);
/*:ref: dchfiv_ 7 9 7 7 7 7 7 7 7 7 4 */
/*:ref: dpchid_ 7 9 4 7 7 7 4 12 4 4 4 */
/*:ref: xerror_ 14 5 13 4 4 4 124 */
extern int dvout_(integer *n, doublereal *dx, char *ifmt, integer *idigit, ftnlen ifmt_len);
/*:ref: i1mach_ 4 1 4 */
extern int mpdivi_(integer *x, integer *iy, integer *z__);
/* comlen mpcom_ 68 */
/*:ref: mpstr_ 14 2 4 4 */
/*:ref: mpnzr_ 14 4 4 4 4 4 */
/*:ref: mpchk_ 14 2 4 4 */
/*:ref: mperr_ 14 0 */
/*:ref: mpunfl_ 14 1 4 */
extern E_f r9lgit_(real *a, real *x, real *algap1);
/*:ref: r1mach_ 6 1 4 */
/*:ref: xerror_ 14 5 13 4 4 4 124 */
extern int splpfe_(integer *mrelas, integer *nvars__, integer *lmx, integer *lbm, integer *ienter, integer *ibasis, integer *imat, integer *ibrc, integer *ipr, integer *iwr, integer *ind, integer *ibb, real *erdnrm, real *eps, real *gg, real *dulnrm, real *dirnrm, real *amat, real *basmat, real *csc, real *wr, real *ww, real *bl, real *bu, real *rz, real *rg, real *colnrm, real *duals, logical *found);
/*:ref: scopy_ 14 5 4 6 4 6 4 */
/*:ref: iploc_ 4 3 4 6 4 */
/*:ref: prwpge_ 14 5 4 4 4 6 4 */
/*:ref: la05bs_ 14 10 6 4 4 4 4 4 6 6 6 12 */
/*:ref: sasum_ 6 3 4 6 4 */
extern int bfqad_(E_fp f, real *t, real *bcoef, integer *n, integer *k, integer *id, real *x1, real *x2, real *tol, real *quad, integer *ierr, real *work);
/*:ref: r1mach_ 6 1 4 */
/*:ref: intrv_ 14 6 6 4 6 4 4 4 */
/*:ref: bsgq8_ 14 13 206 6 6 4 4 4 6 6 4 6 6 4 6 */
/*:ref: xerror_ 14 5 13 4 4 4 124 */
extern int compb_(integer *n, integer *ierror, real *an, real *bn, real *cn, real *b, real *ah, real *bh);
/* comlen cblkt_ 28 */
/*:ref: epmach_ 6 1 6 */
/*:ref: indxb_ 14 4 4 4 4 4 */
/*:ref: tevls_ 14 4 4 6 6 4 */
/*:ref: ppadd_ 14 7 4 4 6 6 6 6 6 */
extern int dbvpor_(doublereal *y, integer *nrowy, integer *ncomp, doublereal *xpts, integer *nxpts, doublereal *a, integer *nrowa, doublereal *alpha, integer *nic, doublereal *b, integer *nrowb, doublereal *beta, integer *nfc, integer *iflag, doublereal *z__, integer *mxnon, doublereal *p, integer *ntp, integer *ip, doublereal *w, integer *niv, doublereal *yhp, doublereal *u, doublereal *v, doublereal *coef, doublereal *s, doublereal *stowa, doublereal *g, doublereal *work, integer *iwork, integer *nfcc);
/* comlen dml8sz_ 36 */
/* comlen dml15t_ 148 */
/* comlen dml18j_ 84 */
/*:ref: dlssud_ 14 20 7 7 7 4 4 4 7 4 4 4 4 4 7 7 4 7 7 7 4 7 */
/*:ref: dvecs_ 14 7 4 4 7 7 4 4 4 */
/*:ref: dstor1_ 14 7 7 7 7 7 4 4 4 */
/*:ref: dstway_ 14 5 7 7 7 4 7 */
/*:ref: drkfab_ 14 21 4 7 4 4 4 7 4 7 4 4 7 4 7 7 7 7 7 7 7 4 4 */
/*:ref: dcoef_ 14 18 7 7 4 4 4 4 7 7 7 4 7 7 7 7 7 4 4 4 */
/*:ref: dbksol_ 14 3 4 7 7 */
/*:ref: ddot_ 7 5 4 7 4 7 4 */
extern int dpchic_(integer *ic, doublereal *vc, doublereal *switch__, integer *n, doublereal *x, doublereal *f, doublereal *d__, integer *incfd, doublereal *wk, integer *nwk, integer *ierr);
/*:ref: dpchci_ 14 5 4 7 7 7 4 */
/*:ref: dpchcs_ 14 7 7 4 7 7 7 4 4 */
/*:ref: dpchce_ 14 9 4 7 4 7 7 7 7 4 4 */
/*:ref: xerror_ 14 5 13 4 4 4 124 */
extern int dwnlit_(doublereal *w, integer *mdw, integer *m, integer *n, integer *l, integer *ipivot, integer *itype, doublereal *h__, doublereal *scale, doublereal *rnorm, integer *idope, doublereal *dope, logical *done);
/*:ref: drotmg_ 14 5 7 7 7 7 7 */
/*:ref: drotm_ 14 6 4 7 4 7 4 7 */
/*:ref: dcopy_ 14 5 4 7 4 7 4 */
/*:ref: dswap_ 14 5 4 7 4 7 4 */
/*:ref: dh12_ 14 11 4 4 4 4 7 4 7 7 4 4 4 */
/*:ref: idamax_ 4 3 4 7 4 */
extern int mperr_(void);
/* comlen mpcom_ 68 */
extern E_f r9lgmc_(real *x);
/*:ref: inits_ 4 3 6 4 6 */
/*:ref: r1mach_ 6 1 4 */
/*:ref: xerror_ 14 5 13 4 4 4 124 */
/*:ref: csevl_ 6 3 6 6 4 */
extern int splpfl_(integer *mrelas, integer *nvars__, integer *ienter, integer *ileave, integer *ibasis, integer *ind, integer *ibb, real *theta, real *dirnrm, real *rprnrm, real *csc, real *ww, real *bl, real *bu, real *erp, real *rprim, real *primal, logical *finite, logical *zerolv);
extern E_f bi_(real *x);
/*:ref: r1mach_ 6 1 4 */
/*:ref: inits_ 4 3 6 4 6 */
/*:ref: r9aimp_ 14 3 6 6 6 */
/*:ref: csevl_ 6 3 6 6 4 */
/*:ref: xerror_ 14 5 13 4 4 4 124 */
/*:ref: bie_ 6 1 6 */
extern int comqr_(integer *nm, integer *n, integer *low, integer *igh, real *hr, real *hi, real *wr, real *wi, integer *ierr);
/*:ref: pythag_ 6 2 6 6 */
/*:ref: csroot_ 14 4 6 6 6 6 */
/*:ref: cdiv_ 14 6 6 6 6 6 6 6 */
extern int dbvsup_(doublereal *y, integer *nrowy, integer *ncomp, doublereal *xpts, integer *nxpts, doublereal *a, integer *nrowa, doublereal *alpha, integer *nic, doublereal *b, integer *nrowb, doublereal *beta, integer *nfc, integer *igofx, doublereal *re, doublereal *ae, integer *iflag, doublereal *work, integer *ndw, integer *iwork, integer *ndiw, integer *neqivp);
/* comlen dml8sz_ 36 */
/* comlen dml18j_ 84 */
/* comlen dml17b_ 72 */
/* comlen dml15t_ 148 */
/* comlen dml5mc_ 52 */
/*:ref: dmacon_ 14 0 */
/*:ref: xerrwv_ 14 11 13 4 4 4 4 4 4 4 6 6 124 */
/*:ref: dexbvp_ 14 12 7 4 7 7 4 7 7 4 7 4 7 4 */
extern doublereal dpchid_(integer *n, doublereal *x, doublereal *f, doublereal *d__, integer *incfd, logical *skip, integer *ia, integer *ib, integer *ierr);
/*:ref: xerror_ 14 5 13 4 4 4 124 */
extern int dwnlsm_(doublereal *w, integer *mdw, integer *mme, integer *ma, integer *n, integer *l, doublereal *prgopt, doublereal *x, doublereal *rnorm, integer *mode, integer *ipivot, integer *itype, doublereal *wd, doublereal *h__, doublereal *scale, doublereal *z__, doublereal *temp, doublereal *d__);
/*:ref: dcopy_ 14 5 4 7 4 7 4 */
/*:ref: xerror_ 14 5 13 4 4 4 124 */
/*:ref: dnrm2_ 7 3 4 7 4 */
/*:ref: dscal_ 14 4 4 7 7 4 */
/*:ref: dasum_ 7 3 4 7 4 */
/*:ref: idamax_ 4 3 4 7 4 */
/*:ref: d1mach_ 7 1 4 */
/*:ref: dwnlit_ 14 13 7 4 4 4 4 4 4 7 7 7 4 7 12 */
/*:ref: dswap_ 14 5 4 7 4 7 4 */
/*:ref: drotmg_ 14 5 7 7 7 7 7 */
/*:ref: drotm_ 14 6 4 7 4 7 4 7 */
/*:ref: dh12_ 14 11 4 4 4 4 7 4 7 7 4 4 4 */
/*:ref: daxpy_ 14 6 4 7 7 4 7 4 */
extern int mpmaxr_(integer *x);
/* comlen mpcom_ 68 */
/*:ref: mpchk_ 14 2 4 4 */
extern E_f r9ln2r_(real *x);
/*:ref: r1mach_ 6 1 4 */
/*:ref: inits_ 4 3 6 4 6 */
/*:ref: csevl_ 6 3 6 6 4 */
/*:ref: xerror_ 14 5 13 4 4 4 124 */
extern int splpmn_(U_fp usrmat, integer *mrelas, integer *nvars__, real *costs, real *prgopt, real *dattrv, real *bl, real *bu, integer *ind, integer *info, real *primal, real *duals, real *amat, real *csc, real *colnrm, real *erd, real *erp, real *basmat, real *wr, real *rz, real *rg, real *rprim, real *rhs, real *ww, integer *lmx, integer *lbm, integer *ibasis, integer *ibb, integer *imat, integer *ibrc, integer *ipr, integer *iwr);
/* comlen la05ds_ 28 */
/*:ref: spopt_ 14 9 6 4 4 4 6 4 6 4 12 */
/*:ref: pinitm_ 14 6 4 4 6 4 4 4 */
/*:ref: splpup_ 14 14 200 4 4 6 6 6 6 4 4 6 4 12 6 6 */
/*:ref: spinit_ 14 20 4 4 6 6 6 4 6 4 6 6 6 6 6 6 6 6 4 4 4 12 */
/*:ref: xerror_ 14 5 13 4 4 4 124 */
/*:ref: ivout_ 14 5 4 4 13 4 124 */
/*:ref: sasum_ 6 3 4 6 4 */
/*:ref: scopy_ 14 5 4 6 4 6 4 */
/*:ref: pnnzrs_ 14 6 4 6 4 6 4 4 */
/*:ref: svout_ 14 5 4 6 13 4 124 */
/*:ref: prwpge_ 14 5 4 4 4 6 4 */
/*:ref: splpdm_ 14 24 4 4 4 4 4 4 4 4 4 4 4 4 4 4 6 6 6 6 6 6 6 6 12 12 */
/*:ref: splpce_ 14 27 4 4 4 4 4 4 4 4 4 4 4 4 4 6 6 6 6 6 6 6 6 6 6 6 6 12 12 */
/*:ref: spincw_ 14 28 4 4 4 4 4 4 4 4 4 4 4 4 4 6 6 6 6 6 6 6 6 6 6 6 6 6 6 12 */
/*:ref: xerrwv_ 14 11 13 4 4 4 4 4 4 4 6 6 124 */
/*:ref: la05bs_ 14 10 6 4 4 4 4 4 6 6 6 12 */
/*:ref: splpfe_ 14 29 4 4 4 4 4 4 4 4 4 4 4 4 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 12 */
/*:ref: splpfl_ 14 19 4 4 4 4 4 4 4 6 6 6 6 6 6 6 6 6 6 12 12 */
/*:ref: splpmu_ 14 50 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 12 12 12 12 */
/*:ref: sdot_ 6 5 4 6 4 6 4 */
/*:ref: sclosm_ 14 1 4 */
extern E_f bie_(real *x);
/*:ref: r1mach_ 6 1 4 */
/*:ref: inits_ 4 3 6 4 6 */
/*:ref: r9aimp_ 14 3 6 6 6 */
/*:ref: csevl_ 6 3 6 6 4 */
extern int comqr2_(integer *nm, integer *n, integer *low, integer *igh, real *ortr, real *orti, real *hr, real *hi, real *wr, real *wi, real *zr, real *zi, integer *ierr);
/*:ref: pythag_ 6 2 6 6 */
/*:ref: csroot_ 14 4 6 6 6 6 */
/*:ref: cdiv_ 14 6 6 6 6 6 6 6 */
extern doublereal dcbrt_(doublereal *x);
/*:ref: d1mach_ 7 1 4 */
/*:ref: d9upak_ 14 3 7 7 4 */
/*:ref: d9pak_ 7 2 7 4 */
extern int dpchim_(integer *n, doublereal *x, doublereal *f, doublereal *d__, integer *incfd, integer *ierr);
/*:ref: dpchst_ 7 2 7 7 */
/*:ref: xerror_ 14 5 13 4 4 4 124 */
extern int dwnnls_(doublereal *w, integer *mdw, integer *me, integer *ma, integer *n, integer *l, doublereal *prgopt, doublereal *x, doublereal *rnorm, integer *mode, integer *iwork, doublereal *work);
/*:ref: xerrwv_ 14 11 13 4 4 4 4 4 4 4 7 7 124 */
/*:ref: xerror_ 14 5 13 4 4 4 124 */
/*:ref: dwnlsm_ 14 18 7 4 4 4 4 4 7 7 7 4 4 4 7 7 7 7 7 7 */
extern int mpmlp_(integer *u, integer *v, integer *w, integer *j);
extern E_f r9pak_(real *y, integer *n);
/*:ref: i1mach_ 4 1 4 */
/*:ref: r1mach_ 6 1 4 */
/*:ref: r9upak_ 14 3 6 6 4 */
/*:ref: xerror_ 14 5 13 4 4 4 124 */
extern int splpmu_(integer *mrelas, integer *nvars__, integer *lmx, integer *lbm, integer *nredc, integer *info, integer *ienter, integer *ileave, integer *iopt, integer *npp, integer *jstrt, integer *ibasis, integer *imat, integer *ibrc, integer *ipr, integer *iwr, integer *ind, integer *ibb, real *anorm, real *eps, real *uu, real *gg, real *rprnrm, real *erdnrm, real *dulnrm, real *theta, real *costsc, real *xlamda, real *rhsnrm, real *amat, real *basmat, real *csc, real *wr, real *rprim, real *ww, real *bu, real *bl, real *rhs, real *erd, real *erp, real *rz, real *rg, real *colnrm, real *costs, real *primal, real *duals, logical *singlr, logical *redbas, logical *zerolv, logical *stpedg);
/* comlen la05ds_ 28 */
/*:ref: sasum_ 6 3 4 6 4 */
/*:ref: sdot_ 6 5 4 6 4 6 4 */
/*:ref: la05bs_ 14 10 6 4 4 4 4 4 6 6 6 12 */
/*:ref: la05cs_ 14 10 6 4 4 4 4 4 6 6 6 4 */
/*:ref: splpdm_ 14 24 4 4 4 4 4 4 4 4 4 4 4 4 4 4 6 6 6 6 6 6 6 6 12 12 */
/*:ref: xerror_ 14 5 13 4 4 4 124 */
/*:ref: scopy_ 14 5 4 6 4 6 4 */
/*:ref: iploc_ 4 3 4 6 4 */
/*:ref: prwpge_ 14 5 4 4 4 6 4 */
/*:ref: pnnzrs_ 14 6 4 6 4 6 4 4 */
extern E_f binom_(integer *n, integer *m);
/*:ref: r1mach_ 6 1 4 */
/*:ref: xerror_ 14 5 13 4 4 4 124 */
/*:ref: r9lgmc_ 6 1 6 */
/*:ref: alnrel_ 6 1 6 */
extern int cortb_(integer *nm, integer *low, integer *igh, real *ar, real *ai, real *ortr, real *orti, integer *m, real *zr, real *zi);
extern int dcdot_(integer *n, doublereal *fm, complex *cx, integer *incx, complex *cy, integer *incy, doublereal *dcr, doublereal *dci);
extern int dpchmc_(integer *n, doublereal *x, doublereal *f, doublereal *d__, integer *incfd, logical *skip, integer *ismon, integer *ierr);
/*:ref: dchfmc_ 4 3 7 7 7 */
/*:ref: xerror_ 14 5 13 4 4 4 124 */
extern int dwritp_(integer *ipage, integer *list, doublereal *rlist, integer *lpage, integer *irec);
/*:ref: xerrwv_ 14 11 13 4 4 4 4 4 4 4 7 7 124 */
extern int mpmul_(integer *x, integer *y, integer *z__);
/* comlen mpcom_ 68 */
/*:ref: mpchk_ 14 2 4 4 */
/*:ref: mpmlp_ 14 4 4 4 4 4 */
/*:ref: mpnzr_ 14 4 4 4 4 4 */
/*:ref: mperr_ 14 0 */
extern int r9upak_(real *x, real *y, integer *n);
extern int splpup_(S_fp usrmat, integer *mrelas, integer *nvars__, real *prgopt, real *dattrv, real *bl, real *bu, integer *ind, integer *info, real *amat, integer *imat, logical *sizeup, real *asmall, real *abig);
/*:ref: xerror_ 14 5 13 4 4 4 124 */
/*:ref: xerrwv_ 14 11 13 4 4 4 4 4 4 4 6 6 124 */
/*:ref: pchngs_ 14 6 4 6 4 6 4 4 */
/*:ref: pnnzrs_ 14 6 4 6 4 6 4 4 */
extern int bint4_(real *x, real *y, integer *ndata, integer *ibcl, integer *ibcr, real *fbcl, real *fbcr, integer *kntopt, real *t, real *bcoef, integer *n, integer *k, real *w);
/*:ref: r1mach_ 6 1 4 */
/*:ref: bspvd_ 14 8 6 4 4 6 4 4 6 6 */
/*:ref: bnfac_ 14 6 6 4 4 4 4 4 */
/*:ref: bnslv_ 14 6 6 4 4 4 4 6 */
/*:ref: xerror_ 14 5 13 4 4 4 124 */
extern int corth_(integer *nm, integer *n, integer *low, integer *igh, real *ar, real *ai, real *ortr, real *orti);
/*:ref: pythag_ 6 2 6 6 */
extern int dcfod_(integer *meth, doublereal *elco, doublereal *tesco);
extern int dpchng_(integer *ii, doublereal *xval, integer *iplace, doublereal *sx, integer *ix, integer *ircx);
/*:ref: xerror_ 14 5 13 4 4 4 124 */
/*:ref: iploc_ 4 3 4 7 4 */
/*:ref: dprwpg_ 14 5 4 4 4 7 4 */
extern int dwupdt_(integer *n, doublereal *r__, integer *ldr, doublereal *w, doublereal *b, doublereal *alpha, doublereal *cos__, doublereal *sin__);
extern int mpmul2_(integer *x, integer *iy, integer *z__, integer *trunc);
/* comlen mpcom_ 68 */
/*:ref: mpchk_ 14 2 4 4 */
/*:ref: mpovfl_ 14 1 4 */
/*:ref: mpstr_ 14 2 4 4 */
/*:ref: mpnzr_ 14 4 4 4 4 4 */
/*:ref: mperr_ 14 0 */
extern int radb2_(integer *ido, integer *l1, real *cc, real *ch, real *wa1);
extern int spoco_(real *a, integer *lda, integer *n, real *rcond, real *z__, integer *info);
/*:ref: sasum_ 6 3 4 6 4 */
/*:ref: spofa_ 14 4 6 4 4 4 */
/*:ref: sscal_ 14 4 4 6 6 4 */
/*:ref: saxpy_ 14 6 4 6 6 4 6 4 */
/*:ref: sdot_ 6 5 4 6 4 6 4 */
extern int bintk_(real *x, real *y, real *t, integer *n, integer *k, real *bcoef, real *q, real *work);
/*:ref: bspvn_ 14 9 6 4 4 4 6 4 6 6 4 */
/*:ref: bnfac_ 14 6 6 4 4 4 4 4 */
/*:ref: bnslv_ 14 6 6 4 4 4 4 6 */
/*:ref: xerror_ 14 5 13 4 4 4 124 */
extern E_f cosdg_(real *x);
extern int dchdc_(doublereal *a, integer *lda, integer *p, doublereal *work, integer *jpvt, integer *job, integer *info);
/*:ref: dswap_ 14 5 4 7 4 7 4 */
/*:ref: daxpy_ 14 6 4 7 7 4 7 4 */
extern int dpchsp_(integer *ic, doublereal *vc, integer *n, doublereal *x, doublereal *f, doublereal *d__, integer *incfd, doublereal *wk, integer *nwk, integer *ierr);
/*:ref: dpchdf_ 7 4 4 7 7 4 */
/*:ref: xerror_ 14 5 13 4 4 4 124 */
extern int dx_(real *u, integer *idmn, integer *i__, integer *j, real *uxxx, real *uxxxx);
/* comlen splpzz_ 80 */
extern int mpmuli_(integer *x, integer *iy, integer *z__);
/*:ref: mpmul2_ 14 4 4 4 4 4 */
extern int radb3_(integer *ido, integer *l1, real *cc, real *ch, real *wa1, real *wa2);
extern int spodi_(real *a, integer *lda, integer *n, real *det, integer *job);
/*:ref: sscal_ 14 4 4 6 6 4 */
/*:ref: saxpy_ 14 6 4 6 6 4 6 4 */
extern int bisect_(integer *n, real *eps1, real *d__, real *e, real *e2, real *lb, real *ub, integer *mm, integer *m, real *w, integer *ind, integer *ierr, real *rv4, real *rv5);
extern int cosgen_(integer *n, integer *ijump, real *fnum, real *fden, real *a);
/*:ref: pimach_ 6 1 6 */
extern int dchdd_(doublereal *r__, integer *ldr, integer *p, doublereal *x, doublereal *z__, integer *ldz, integer *nz, doublereal *y, doublereal *rho, doublereal *c__, doublereal *s, integer *info);
/*:ref: ddot_ 7 5 4 7 4 7 4 */
/*:ref: dnrm2_ 7 3 4 7 4 */
extern doublereal dpchst_(doublereal *arg1, doublereal *arg2);
extern int dx4_(real *u, integer *idmn, integer *i__, integer *j, real *uxxx, real *uxxxx);
/* comlen spl4_ 80 */
extern int mpnzr_(integer *rs, integer *re, integer *z__, integer *trunc);
/* comlen mpcom_ 68 */
/*:ref: mperr_ 14 0 */
/*:ref: mpovfl_ 14 1 4 */
/*:ref: mpunfl_ 14 1 4 */
extern int radb4_(integer *ido, integer *l1, real *cc, real *ch, real *wa1, real *wa2, real *wa3);
extern int spofa_(real *a, integer *lda, integer *n, integer *info);
/*:ref: sdot_ 6 5 4 6 4 6 4 */
extern int bkias_(real *x, integer *n, integer *ktrms, real *t, real *ans, integer *ind, integer *ms, real *gmrn, real *h__, integer *ierr);
/*:ref: r1mach_ 6 1 4 */
/*:ref: gamrn_ 6 1 6 */
/*:ref: hkseq_ 14 4 6 4 6 4 */
/*:ref: bdiff_ 14 2 4 6 */
extern int cosqb_(integer *n, real *x, real *wsave);
/*:ref: cosqb1_ 14 4 4 6 6 6 */
extern int dchex_(doublereal *r__, integer *ldr, integer *p, integer *k, integer *l, doublereal *z__, integer *ldz, integer *nz, doublereal *c__, doublereal *s, integer *job);
/*:ref: drotg_ 14 4 7 7 7 7 */
extern int dpchsw_(doublereal *dfmax, integer *iextrm, doublereal *d1, doublereal *d2, doublereal *h__, doublereal *slope, integer *ierr);
/*:ref: d1mach_ 6 1 4 */
/*:ref: xerror_ 14 5 13 4 4 4 124 */
extern int dy_(real *u, integer *idmn, integer *i__, integer *j, real *uyyy, real *uyyyy);
/* comlen splpzz_ 80 */
extern int mpovfl_(integer *x);
/* comlen mpcom_ 68 */
/*:ref: mpchk_ 14 2 4 4 */
/*:ref: mpmaxr_ 14 1 4 */
/*:ref: mperr_ 14 0 */
extern int radb5_(integer *ido, integer *l1, real *cc, real *ch, real *wa1, real *wa2, real *wa3, real *wa4);
extern int spofs_(real *a, integer *lda, integer *n, real *v, integer *itask, integer *ind, real *work);
/*:ref: spoco_ 14 6 6 4 4 6 6 4 */
/*:ref: r1mach_ 6 1 4 */
/*:ref: xerror_ 14 5 13 4 4 4 124 */
/*:ref: sposl_ 14 4 6 4 4 6 */
/*:ref: xerrwv_ 14 11 13 4 4 4 4 4 4 4 4 4 124 */
extern int bkisr_(real *x, integer *n, real *sum, integer *ierr);
/*:ref: r1mach_ 6 1 4 */
/*:ref: psixn_ 6 1 4 */
extern int cosqb1_(integer *n, real *x, real *w, real *xh);
/*:ref: rfftb_ 14 3 4 6 6 */
extern int dchfdv_(doublereal *x1, doublereal *x2, doublereal *f1, doublereal *f2, doublereal *d1, doublereal *d2, integer *ne, doublereal *xe, doublereal *fe, doublereal *de, integer *next, integer *ierr);
/*:ref: xerror_ 14 5 13 4 4 4 124 */
extern int dpcoef_(integer *l, doublereal *c__, doublereal *tc, doublereal *a);
/*:ref: dp1vlu_ 14 6 4 4 7 7 7 7 */
extern int dy4_(real *u, integer *idmn, integer *i__, integer *j, real *uyyy, real *uyyyy);
/* comlen spl4_ 80 */
extern int mpstr_(integer *x, integer *y);
/* comlen mpcom_ 68 */
extern int radbg_(integer *ido, integer *ip, integer *l1, integer *idl1, real *cc, real *c1, real *c2, real *ch, real *ch2, real *wa);
extern int spoir_(real *a, integer *lda, integer *n, real *v, integer *itask, integer *ind, real *work);
/*:ref: scopy_ 14 5 4 6 4 6 4 */
/*:ref: spofa_ 14 4 6 4 4 4 */
/*:ref: sposl_ 14 4 6 4 4 6 */
/*:ref: sasum_ 6 3 4 6 4 */
/*:ref: dsdot_ 7 5 4 6 4 6 4 */
/*:ref: r1mach_ 6 1 4 */
/*:ref: xerror_ 14 5 13 4 4 4 124 */
/*:ref: xerrwv_ 14 11 13 4 4 4 4 4 4 4 4 4 124 */
extern int bksol_(integer *n, real *a, real *x);
/*:ref: sdot_ 6 5 4 6 4 6 4 */
extern int cosqf_(integer *n, real *x, real *wsave);
/*:ref: cosqf1_ 14 4 4 6 6 6 */
extern int dchfev_(doublereal *x1, doublereal *x2, doublereal *f1, doublereal *f2, doublereal *d1, doublereal *d2, integer *ne, doublereal *xe, doublereal *fe, integer *next, integer *ierr);
/*:ref: xerror_ 14 5 13 4 4 4 124 */
extern int dpfqad_(D_fp f, integer *ldc, doublereal *c__, doublereal *xi, integer *lxi, integer *k, integer *id, doublereal *x1, doublereal *x2, doublereal *tol, doublereal *quad, integer *ierr);
/*:ref: d1mach_ 7 1 4 */
/*:ref: dintrv_ 14 6 7 4 7 4 4 4 */
/*:ref: dppgq8_ 14 13 207 4 7 7 4 4 4 7 7 4 7 7 4 */
/*:ref: xerror_ 14 5 13 4 4 4 124 */
extern int dyairy_(doublereal *x, doublereal *rx, doublereal *c__, doublereal *bi, doublereal *dbi);
extern int mpunfl_(integer *x);
/*:ref: mpchk_ 14 2 4 4 */
extern int radf2_(integer *ido, integer *l1, real *cc, real *ch, real *wa1);
extern int spopt_(real *prgopt, integer *mrelas, integer *nvars__, integer *info, real *csc, integer *ibasis, real *ropt, integer *intopt, logical *lopt);
/*:ref: r1mach_ 6 1 4 */
/*:ref: xerror_ 14 5 13 4 4 4 124 */
extern int blktr1_(integer *n, real *an, real *bn, real *cn, integer *m, real *am, real *bm, real *cm, integer *idimy, real *y, real *b, real *w1, real *w2, real *w3, real *wd, real *ww, real *wu, S_fp prdct, S_fp cprdct);
/* comlen cblkt_ 28 */
/*:ref: indxb_ 14 4 4 4 4 4 */
/*:ref: indxc_ 14 4 4 4 4 4 */
/*:ref: indxa_ 14 4 4 4 4 4 */
extern int cosqf1_(integer *n, real *x, real *w, real *xh);
/*:ref: rfftf_ 14 3 4 6 6 */
extern doublereal dchfiv_(doublereal *x1, doublereal *x2, doublereal *f1, doublereal *f2, doublereal *d1, doublereal *d2, doublereal *a, doublereal *b, integer *ierr);
/*:ref: xerror_ 14 5 13 4 4 4 124 */
extern int dpincw_(integer *mrelas, integer *nvars__, integer *lmx, integer *lbm, integer *npp, integer *jstrt, integer *ibasis, integer *imat, integer *ibrc, integer *ipr, integer *iwr, integer *ind, integer *ibb, doublereal *costsc, doublereal *gg, doublereal *erdnrm, doublereal *dulnrm, doublereal *amat, doublereal *basmat, doublereal *csc, doublereal *wr, doublereal *ww, doublereal *rz, doublereal *rg, doublereal *costs, doublereal *colnrm, doublereal *duals, logical *stpedg);
/*:ref: dcopy_ 14 5 4 7 4 7 4 */
/*:ref: iploc_ 4 3 4 7 4 */
/*:ref: dprwpg_ 14 5 4 4 4 7 4 */
/*:ref: la05bd_ 14 10 7 4 4 4 4 4 7 7 7 12 */
/*:ref: ddot_ 7 5 4 7 4 7 4 */
extern E_f e1_(real *x);
/*:ref: r1mach_ 6 1 4 */
/*:ref: inits_ 4 3 6 4 6 */
/*:ref: csevl_ 6 3 6 6 4 */
/*:ref: xerror_ 14 5 13 4 4 4 124 */
extern integer numxer_(integer *nerr);
/*:ref: j4save_ 4 3 4 4 12 */
extern int radf3_(integer *ido, integer *l1, real *cc, real *ch, real *wa1, real *wa2);
extern int sposl_(real *a, integer *lda, integer *n, real *b);
/*:ref: sdot_ 6 5 4 6 4 6 4 */
/*:ref: saxpy_ 14 6 4 6 6 4 6 4 */
extern int blktri_(integer *iflg, integer *np, integer *n, real *an, real *bn, real *cn, integer *mp, integer *m, real *am, real *bm, real *cm, integer *idimy, real *y, integer *ierror, real *w);
/* comlen cblkt_ 28 */
/*:ref: compb_ 14 8 4 4 6 6 6 6 6 6 */
/*:ref: blktr1_ 14 19 4 6 6 6 4 6 6 6 4 6 6 6 6 6 6 6 6 200 200 */
extern int cosqi_(integer *n, real *wsave);
/*:ref: rffti_ 14 2 4 6 */
extern integer dchfmc_(doublereal *d1, doublereal *d2, doublereal *delta);
/*:ref: d1mach_ 7 1 4 */
extern int dpinit_(integer *mrelas, integer *nvars__, doublereal *costs, doublereal *bl, doublereal *bu, integer *ind, doublereal *primal, integer *info, doublereal *amat, doublereal *csc, doublereal *costsc, doublereal *colnrm, doublereal *xlamda, doublereal *anorm, doublereal *rhs, doublereal *rhsnrm, integer *ibasis, integer *ibb, integer *imat, logical *lopt);
/*:ref: dpnnzr_ 14 6 4 7 4 7 4 4 */
/*:ref: dcopy_ 14 5 4 7 4 7 4 */
/*:ref: dasum_ 7 3 4 7 4 */
extern int efc_(integer *ndata, real *xdata, real *ydata, real *sddata, integer *nord, integer *nbkpt, real *bkpt, integer *mdein, integer *mdeout, real *coeff, integer *lw, real *w);
/*:ref: efcmn_ 14 19 4 6 6 6 4 4 6 4 4 6 6 6 6 6 6 4 6 4 4 */
extern int ohtrol_(real *q, integer *n, integer *nrda, real *diag, integer *irank, real *div, real *td);
/*:ref: sdot_ 6 5 4 6 4 6 4 */
extern int radf4_(integer *ido, integer *l1, real *cc, real *ch, real *wa1, real *wa2, real *wa3);
extern int sppco_(real *ap, integer *n, real *rcond, real *z__, integer *info);
/*:ref: sasum_ 6 3 4 6 4 */
/*:ref: sppfa_ 14 3 6 4 4 */
/*:ref: sscal_ 14 4 4 6 6 4 */
/*:ref: saxpy_ 14 6 4 6 6 4 6 4 */
/*:ref: sdot_ 6 5 4 6 4 6 4 */
extern int bndacc_(real *g, integer *mdg, integer *nb, integer *ip, integer *ir, integer *mt, integer *jt);
/*:ref: xerror_ 14 5 13 4 4 4 124 */
/*:ref: h12_ 14 11 4 4 4 4 6 4 6 6 4 4 4 */
extern int cost_(integer *n, real *x, real *wsave);
/*:ref: rfftf_ 14 3 4 6 6 */
extern doublereal dchu_(doublereal *a, doublereal *b, doublereal *x);
/*:ref: d1mach_ 7 1 4 */
/*:ref: xerror_ 14 5 13 4 4 4 124 */
/*:ref: dpoch_ 7 2 7 7 */
/*:ref: dgamma_ 7 1 7 */
/*:ref: dgamr_ 7 1 7 */
/*:ref: dpoch1_ 7 2 7 7 */
/*:ref: dexprl_ 7 1 7 */
/*:ref: d9chu_ 7 3 7 7 7 */
extern int dpintm_(integer *m, integer *n, doublereal *sx, integer *ix, integer *lmx, integer *ipagef);
/*:ref: xerror_ 14 5 13 4 4 4 124 */
extern int efcmn_(integer *ndata, real *xdata, real *ydata, real *sddata, integer *nord, integer *nbkpt, real *bkptin, integer *mdein, integer *mdeout, real *coeff, real *bf, real *xtemp, real *ptemp, real *bkpt, real *g, integer *mdg, real *w, integer *mdw, integer *lw);
/*:ref: scopy_ 14 5 4 6 4 6 4 */
/*:ref: xerror_ 14 5 13 4 4 4 124 */
/*:ref: xerrwv_ 14 11 13 4 4 4 4 4 4 4 6 6 124 */
/*:ref: ssort_ 14 4 6 6 4 4 */
/*:ref: bndacc_ 14 7 6 4 4 4 4 4 4 */
/*:ref: bsplvn_ 14 6 6 4 4 6 4 6 */
/*:ref: sscal_ 14 4 4 6 6 4 */
/*:ref: bndsol_ 14 9 4 6 4 4 4 4 6 4 6 */
extern int ohtror_(real *q, integer *n, integer *nrda, real *diag, integer *irank, real *div, real *td);
/*:ref: sdot_ 6 5 4 6 4 6 4 */
extern int radf5_(integer *ido, integer *l1, real *cc, real *ch, real *wa1, real *wa2, real *wa3, real *wa4);
extern int sppdi_(real *ap, integer *n, real *det, integer *job);
/*:ref: sscal_ 14 4 4 6 6 4 */
/*:ref: saxpy_ 14 6 4 6 6 4 6 4 */
extern int bndsol_(integer *mode, real *g, integer *mdg, integer *nb, integer *ip, integer *ir, real *x, integer *n, real *rnorm);
/*:ref: xerror_ 14 5 13 4 4 4 124 */
extern int costi_(integer *n, real *wsave);
/*:ref: rffti_ 14 2 4 6 */
extern int dchud_(doublereal *r__, integer *ldr, integer *p, doublereal *x, doublereal *z__, integer *ldz, integer *nz, doublereal *y, doublereal *rho, doublereal *c__, doublereal *s);
/*:ref: drotg_ 14 4 7 7 7 7 */
extern int dpjac_(integer *neq, doublereal *y, doublereal *yh, integer *nyh, doublereal *ewt, doublereal *ftem, doublereal *savf, doublereal *wm, integer *iwm, S_fp df, S_fp djac, doublereal *rpar, integer *ipar);
/* comlen ddebd1_ 1876 */
/*:ref: dvnrms_ 7 3 4 7 7 */
/*:ref: dgefa_ 14 5 7 4 4 4 4 */
/*:ref: dgbfa_ 14 7 7 4 4 4 4 4 4 */
extern E_f ei_(real *x);
/*:ref: e1_ 6 1 6 */
extern int ortbak_(integer *nm, integer *low, integer *igh, real *a, real *ort, integer *m, real *z__);
extern int radfg_(integer *ido, integer *ip, integer *l1, integer *idl1, real *cc, real *c1, real *c2, real *ch, real *ch2, real *wa);
extern int sppfa_(real *ap, integer *n, integer *info);
/*:ref: sdot_ 6 5 4 6 4 6 4 */
extern int bnfac_(real *w, integer *nroww, integer *nrow, integer *nbandl, integer *nbandu, integer *iflag);
extern E_f cot_(real *x);
/*:ref: inits_ 4 3 6 4 6 */
/*:ref: r1mach_ 6 1 4 */
/*:ref: xerror_ 14 5 13 4 4 4 124 */
/*:ref: csevl_ 6 3 6 6 4 */
extern int dckder_(integer *m, integer *n, doublereal *x, doublereal *fvec, doublereal *fjac, integer *ldfjac, doublereal *xp, doublereal *fvecp, integer *mode, doublereal *err);
/*:ref: d1mach_ 7 1 4 */
extern int dplint_(integer *n, doublereal *x, doublereal *y, doublereal *c__);
/*:ref: xerror_ 14 5 13 4 4 4 124 */
extern int eisdoc_(void);
extern int orthes_(integer *nm, integer *n, integer *low, integer *igh, real *a, real *ort);
extern E_f rand_(real *r__);
extern int sppsl_(real *ap, integer *n, real *b);
/*:ref: sdot_ 6 5 4 6 4 6 4 */
/*:ref: saxpy_ 14 6 4 6 6 4 6 4 */
extern int bnslv_(real *w, integer *nroww, integer *nrow, integer *nbandl, integer *nbandu, real *b);
extern int cpadd_(integer *n, integer *ierror, real *a, real *c__, complex *cbp, real *bp, real *bh);
/* comlen ccblk_ 28 */
/*:ref: pgsf_ 6 5 6 4 6 6 6 */
/*:ref: bcrh_ 6 8 6 6 4 6 6 6 206 6 */
/*:ref: ppgsf_ 6 5 6 4 6 6 6 */
extern int dcoef_(doublereal *yh, doublereal *yp, integer *ncomp, integer *nrowb, integer *nfc, integer *nic, doublereal *b, doublereal *beta, doublereal *coef, integer *inhomo, doublereal *re, doublereal *ae, doublereal *by, doublereal *cvec, doublereal *work, integer *iwork, integer *iflag, integer *nfcc);
/* comlen dml5mc_ 52 */
/*:ref: ddot_ 7 5 4 7 4 7 4 */
/*:ref: xgetf_ 14 1 4 */
/*:ref: xsetf_ 14 1 4 */
/*:ref: dsuds_ 14 10 7 7 7 4 4 4 4 4 7 4 */
extern int dplpce_(integer *mrelas, integer *nvars__, integer *lmx, integer *lbm, integer *itlp, integer *itbrc, integer *ibasis, integer *imat, integer *ibrc, integer *ipr, integer *iwr, integer *ind, integer *ibb, doublereal *erdnrm, doublereal *eps, doublereal *tune, doublereal *gg, doublereal *amat, doublereal *basmat, doublereal *csc, doublereal *wr, doublereal *ww, doublereal *primal, doublereal *erd, doublereal *erp, logical *singlr, logical *redbas);
/*:ref: la05bd_ 14 10 7 4 4 4 4 4 7 7 7 12 */
/*:ref: dasum_ 7 3 4 7 4 */
/*:ref: dcopy_ 14 5 4 7 4 7 4 */
/*:ref: iploc_ 4 3 4 7 4 */
/*:ref: dprwpg_ 14 5 4 4 4 7 4 */
extern int elmbak_(integer *nm, integer *low, integer *igh, real *a, integer *int__, integer *m, real *z__);
extern int ortho4_(real *usol, integer *idmn, real *zn, real *zm, real *pertrb);
/* comlen spl4_ 80 */
extern int ratqr_(integer *n, real *eps1, real *d__, real *e, real *e2, integer *m, real *w, integer *ind, real *bd, logical *type__, integer *idef, integer *ierr);
extern int sptsl_(integer *n, real *d__, real *e, real *b);
extern int bqr_(integer *nm, integer *n, integer *mb, real *a, real *t, real *r__, integer *ierr, integer *nv, real *rv);
/*:ref: pythag_ 6 2 6 6 */
extern int cpbco_(complex *abd, integer *lda, integer *n, integer *m, real *rcond, complex *z__, integer *info);
/*:ref: scasum_ 6 3 4 8 4 */
/*:ref: cpbfa_ 14 5 8 4 4 4 4 */
/*:ref: csscal_ 14 4 4 6 8 4 */
/*:ref: caxpy_ 14 6 4 8 8 4 8 4 */
/*:ref: cdotc_ 8 6 8 4 8 4 8 4 */
extern int dcopy_(integer *n, doublereal *dx, integer *incx, doublereal *dy, integer *incy);
extern int dplpdm_(integer *mrelas, integer *nvars__, integer *lmx, integer *lbm, integer *nredc, integer *info, integer *iopt, integer *ibasis, integer *imat, integer *ibrc, integer *ipr, integer *iwr, integer *ind, integer *ibb, doublereal *anorm, doublereal *eps, doublereal *uu, doublereal *gg, doublereal *amat, doublereal *basmat, doublereal *csc, doublereal *wr, logical *singlr, logical *redbas);
/* comlen la05dd_ 32 */
/*:ref: dpnnzr_ 14 6 4 7 4 7 4 4 */
/*:ref: dasum_ 6 3 4 7 4 */
/*:ref: la05ad_ 14 10 7 4 4 4 4 4 4 7 7 7 */
/*:ref: xerror_ 14 5 13 4 4 4 124 */
/*:ref: xerrwv_ 14 11 13 4 4 4 4 4 4 4 7 6 124 */
extern int elmhes_(integer *nm, integer *n, integer *low, integer *igh, real *a, integer *int__);
extern int orthog_(real *usol, integer *idmn, real *zn, real *zm, real *pertrb);
/* comlen splpzz_ 80 */
extern E_f rc_(real *x, real *y, integer *ier);
/*:ref: r1mach_ 6 1 4 */
/*:ref: xerror_ 14 5 13 4 4 4 124 */
extern int sqrdc_(real *x, integer *ldx, integer *n, integer *p, real *qraux, integer *jpvt, real *work, integer *job);
/*:ref: sswap_ 14 5 4 6 4 6 4 */
/*:ref: snrm2_ 6 3 4 6 4 */
/*:ref: sscal_ 14 4 4 6 6 4 */
/*:ref: sdot_ 6 5 4 6 4 6 4 */
/*:ref: saxpy_ 14 6 4 6 6 4 6 4 */
extern int bsgq8_(E_fp fun, real *xt, real *bc, integer *n, integer *kk, integer *id, real *a, real *b, integer *inbv, real *err, real *ans, integer *ierr, real *work);
/*:ref: xerror_ 14 5 13 4 4 4 124 */
/*:ref: i1mach_ 4 1 4 */
/*:ref: r1mach_ 6 1 4 */
/*:ref: bvalu_ 6 8 6 6 4 4 4 6 4 6 */
extern int cpbdi_(complex *abd, integer *lda, integer *n, integer *m, real *det);
extern int dcopym_(integer *n, doublereal *dx, integer *incx, doublereal *dy, integer *incy);
extern int dplpfe_(integer *mrelas, integer *nvars__, integer *lmx, integer *lbm, integer *ienter, integer *ibasis, integer *imat, integer *ibrc, integer *ipr, integer *iwr, integer *ind, integer *ibb, doublereal *erdnrm, doublereal *eps, doublereal *gg, doublereal *dulnrm, doublereal *dirnrm, doublereal *amat, doublereal *basmat, doublereal *csc, doublereal *wr, doublereal *ww, doublereal *bl, doublereal *bu, doublereal *rz, doublereal *rg, doublereal *colnrm, doublereal *duals, logical *found);
/*:ref: dcopy_ 14 5 4 7 4 7 4 */
/*:ref: iploc_ 4 3 4 7 4 */
/*:ref: dprwpg_ 14 5 4 4 4 7 4 */
/*:ref: la05bd_ 14 10 7 4 4 4 4 4 7 7 7 12 */
/*:ref: dasum_ 7 3 4 7 4 */
extern int eltran_(integer *nm, integer *n, integer *low, integer *igh, real *a, integer *int__, real *z__);
extern int orthol_(real *a, integer *m, integer *n, integer *nrda, integer *iflag, integer *irank, integer *iscale, real *diag, integer *kpivot, real *scales, real *cols, real *cs);
/*:ref: r1mach_ 6 1 4 */
/*:ref: xerror_ 14 5 13 4 4 4 124 */
/*:ref: sdot_ 6 5 4 6 4 6 4 */
/*:ref: cscale_ 14 12 6 4 4 4 6 6 6 6 6 6 4 4 */
extern E_f rd_(real *x, real *y, real *z__, integer *ier);
/*:ref: r1mach_ 6 1 4 */
/*:ref: xerror_ 14 5 13 4 4 4 124 */
extern int sqrsl_(real *x, integer *ldx, integer *n, integer *k, real *qraux, real *y, real *qy, real *qty, real *b, real *rsd, real *xb, integer *job, integer *info);
/*:ref: scopy_ 14 5 4 6 4 6 4 */
/*:ref: sdot_ 6 5 4 6 4 6 4 */
/*:ref: saxpy_ 14 6 4 6 6 4 6 4 */
extern int bskin_(real *x, integer *n, integer *kode, integer *m, real *y, integer *nz, integer *ierr);
/*:ref: i1mach_ 4 1 4 */
/*:ref: r1mach_ 6 1 4 */
/*:ref: bkisr_ 14 4 6 4 6 4 */
/*:ref: bkias_ 14 10 6 4 4 6 6 4 4 6 6 4 */
/*:ref: exint_ 14 8 6 4 4 4 6 6 4 4 */
/*:ref: gamrn_ 6 1 6 */
extern int cpbfa_(complex *abd, integer *lda, integer *n, integer *m, integer *info);
/*:ref: cdotc_ 8 6 8 4 8 4 8 4 */
extern doublereal dcosdg_(doublereal *x);
extern int dplpfl_(integer *mrelas, integer *nvars__, integer *ienter, integer *ileave, integer *ibasis, integer *ind, integer *ibb, doublereal *theta, doublereal *dirnrm, doublereal *rprnrm, doublereal *csc, doublereal *ww, doublereal *bl, doublereal *bu, doublereal *erp, doublereal *rprim, doublereal *primal, logical *finite, logical *zerolv);
extern E_f enorm_(integer *n, real *x);
extern int orthor_(real *a, integer *n, integer *m, integer *nrda, integer *iflag, integer *irank, integer *iscale, real *diag, integer *kpivot, real *scales, real *rows, real *rs);
/*:ref: r1mach_ 6 1 4 */
/*:ref: xerror_ 14 5 13 4 4 4 124 */
/*:ref: sdot_ 6 5 4 6 4 6 4 */
/*:ref: cscale_ 14 12 6 4 4 4 6 6 6 6 6 6 4 4 */
extern int rebak_(integer *nm, integer *n, real *b, real *dl, integer *m, real *z__);
extern int sreadp_(integer *ipage, integer *list, real *rlist, integer *lpage, integer *irec);
/*:ref: xerrwv_ 14 11 13 4 4 4 4 4 4 4 6 6 124 */
extern int bspdoc_(void);
extern int cpbsl_(complex *abd, integer *lda, integer *n, integer *m, complex *b);
/*:ref: cdotc_ 8 6 8 4 8 4 8 4 */
/*:ref: caxpy_ 14 6 4 8 8 4 8 4 */
extern doublereal dcot_(doublereal *x);
/*:ref: initds_ 4 3 7 4 6 */
/*:ref: d1mach_ 7 1 4 */
/*:ref: xerror_ 14 5 13 4 4 4 124 */
/*:ref: dcsevl_ 7 3 7 7 4 */
extern int dplpmn_(U_fp dusrmt, integer *mrelas, integer *nvars__, doublereal *costs, doublereal *prgopt, doublereal *dattrv, doublereal *bl, doublereal *bu, integer *ind, integer *info, doublereal *primal, doublereal *duals, doublereal *amat, doublereal *csc, doublereal *colnrm, doublereal *erd, doublereal *erp, doublereal *basmat, doublereal *wr, doublereal *rz, doublereal *rg, doublereal *rprim, doublereal *rhs, doublereal *ww, integer *lmx, integer *lbm, integer *ibasis, integer *ibb, integer *imat, integer *ibrc, integer *ipr, integer *iwr);
/* comlen la05dd_ 32 */
/*:ref: dpopt_ 14 9 7 4 4 4 7 4 7 4 12 */
/*:ref: dpintm_ 14 6 4 4 7 4 4 4 */
/*:ref: dplpup_ 14 14 200 4 4 7 7 7 7 4 4 7 4 12 7 7 */
/*:ref: dpinit_ 14 20 4 4 7 7 7 4 7 4 7 7 7 7 7 7 7 7 4 4 4 12 */
/*:ref: xerror_ 14 5 13 4 4 4 124 */
/*:ref: ivout_ 14 5 4 4 13 4 124 */
/*:ref: dasum_ 7 3 4 7 4 */
/*:ref: dcopy_ 14 5 4 7 4 7 4 */
/*:ref: dpnnzr_ 14 6 4 7 4 7 4 4 */
/*:ref: dvout_ 14 5 4 7 13 4 124 */
/*:ref: dprwpg_ 14 5 4 4 4 7 4 */
/*:ref: dplpdm_ 14 24 4 4 4 4 4 4 4 4 4 4 4 4 4 4 7 7 7 7 7 7 7 7 12 12 */
/*:ref: dplpce_ 14 27 4 4 4 4 4 4 4 4 4 4 4 4 4 7 7 7 7 7 7 7 7 7 7 7 7 12 12 */
/*:ref: dpincw_ 14 28 4 4 4 4 4 4 4 4 4 4 4 4 4 7 7 7 7 7 7 7 7 7 7 7 7 7 7 12 */
/*:ref: xerrwv_ 14 11 13 4 4 4 4 4 4 4 7 7 124 */
/*:ref: la05bd_ 14 10 7 4 4 4 4 4 7 7 7 12 */
/*:ref: dplpfe_ 14 29 4 4 4 4 4 4 4 4 4 4 4 4 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 12 */
/*:ref: dplpfl_ 14 19 4 4 4 4 4 4 4 7 7 7 7 7 7 7 7 7 7 12 12 */
/*:ref: dplpmu_ 14 50 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 12 12 12 12 */
/*:ref: ddot_ 7 5 4 7 4 7 4 */
/*:ref: sclosm_ 14 1 4 */
extern E_f epmach_(real *dum);
/*:ref: r1mach_ 6 1 4 */
extern int ortran_(integer *nm, integer *n, integer *low, integer *igh, real *a, real *ort, real *z__);
extern int rebakb_(integer *nm, integer *n, real *b, real *dl, integer *m, real *z__);
extern int srot_(integer *n, real *sx, integer *incx, real *sy, integer *incy, real *sc, real *ss);
extern int bspdr_(real *t, real *a, integer *n, integer *k, integer *nderiv, real *ad);
/*:ref: xerror_ 14 5 13 4 4 4 124 */
extern int cpevl_(integer *n, integer *m, complex *a, complex *z__, complex *c__, complex *b, logical *kbd);
/*:ref: i1mach_ 4 1 4 */
extern int dcov_(S_fp fcn, integer *iopt, integer *m, integer *n, doublereal *x, doublereal *fvec, doublereal *r__, integer *ldr, integer *info, doublereal *wa1, doublereal *wa2, doublereal *wa3, doublereal *wa4);
/*:ref: denorm_ 7 2 4 7 */
/*:ref: dfdjc3_ 14 10 214 4 4 7 7 7 4 4 7 7 */
/*:ref: dqrfac_ 14 10 4 4 7 4 12 4 4 7 7 7 */
/*:ref: dwupdt_ 14 8 4 7 4 7 7 7 7 7 */
/*:ref: xerror_ 14 5 13 4 4 4 124 */
extern int dplpmu_(integer *mrelas, integer *nvars__, integer *lmx, integer *lbm, integer *nredc, integer *info, integer *ienter, integer *ileave, integer *iopt, integer *npp, integer *jstrt, integer *ibasis, integer *imat, integer *ibrc, integer *ipr, integer *iwr, integer *ind, integer *ibb, doublereal *anorm, doublereal *eps, doublereal *uu, doublereal *gg, doublereal *rprnrm, doublereal *erdnrm, doublereal *dulnrm, doublereal *theta, doublereal *costsc, doublereal *xlamda, doublereal *rhsnrm, doublereal *amat, doublereal *basmat, doublereal *csc, doublereal *wr, doublereal *rprim, doublereal *ww, doublereal *bu, doublereal *bl, doublereal *rhs, doublereal *erd, doublereal *erp, doublereal *rz, doublereal *rg, doublereal *colnrm, doublereal *costs, doublereal *primal, doublereal *duals, logical *singlr, logical *redbas, logical *zerolv, logical *stpedg);
/* comlen la05dd_ 32 */
/*:ref: dasum_ 7 3 4 7 4 */
/*:ref: ddot_ 7 5 4 7 4 7 4 */
/*:ref: la05bd_ 14 10 7 4 4 4 4 4 7 7 7 12 */
/*:ref: la05cd_ 14 10 7 4 4 4 4 4 7 7 7 4 */
/*:ref: dplpdm_ 14 24 4 4 4 4 4 4 4 4 4 4 4 4 4 4 7 7 7 7 7 7 7 7 12 12 */
/*:ref: xerror_ 14 5 13 4 4 4 124 */
/*:ref: dcopy_ 14 5 4 7 4 7 4 */
/*:ref: iploc_ 4 3 4 7 4 */
/*:ref: dprwpg_ 14 5 4 4 4 7 4 */
/*:ref: dpnnzr_ 14 6 4 7 4 7 4 4 */
extern E_f erf_(real *x);
/*:ref: inits_ 4 3 6 4 6 */
/*:ref: r1mach_ 6 1 4 */
/*:ref: csevl_ 6 3 6 6 4 */
/*:ref: erfc_ 6 1 6 */
extern int passb_(integer *nac, integer *ido, integer *ip, integer *l1, integer *idl1, real *cc, real *c1, real *c2, real *ch, real *ch2, real *wa);
extern int reduc_(integer *nm, integer *n, real *a, real *b, real *dl, integer *ierr);
extern int srotg_(real *sa, real *sb, real *sc, real *ss);
extern int bspev_(real *t, real *ad, integer *n, integer *k, integer *nderiv, real *x, integer *inev, real *svalue, real *work);
/*:ref: intrv_ 14 6 6 4 6 4 4 4 */
/*:ref: bspvn_ 14 9 6 4 4 4 6 4 6 6 4 */
/*:ref: xerror_ 14 5 13 4 4 4 124 */
extern int cpevlr_(integer *n, integer *m, real *a, real *x, real *c__);
extern int dcscal_(doublereal *a, integer *nrda, integer *nrow, integer *ncol, doublereal *cols, doublereal *colsav, doublereal *rows, doublereal *rowsav, doublereal *anorm, doublereal *scales, integer *iscale, integer *ic);
/*:ref: ddot_ 7 5 4 7 4 7 4 */
extern int dplpup_(S_fp dusrmt, integer *mrelas, integer *nvars__, doublereal *prgopt, doublereal *dattrv, doublereal *bl, doublereal *bu, integer *ind, integer *info, doublereal *amat, integer *imat, logical *sizeup, doublereal *asmall, doublereal *abig);
/*:ref: xerror_ 14 5 13 4 4 4 124 */
/*:ref: xerrwv_ 14 11 13 4 4 4 4 4 4 4 7 7 124 */
/*:ref: dpchng_ 14 6 4 7 4 7 4 4 */
/*:ref: dpnnzr_ 14 6 4 7 4 7 4 4 */
extern E_f erfc_(real *x);
/*:ref: r1mach_ 6 1 4 */
/*:ref: inits_ 4 3 6 4 6 */
/*:ref: csevl_ 6 3 6 6 4 */
/*:ref: xerror_ 14 5 13 4 4 4 124 */
extern int passb2_(integer *ido, integer *l1, real *cc, real *ch, real *wa1);
extern int reduc2_(integer *nm, integer *n, real *a, real *b, real *dl, integer *ierr);
extern int srotm_(integer *n, real *sx, integer *incx, real *sy, integer *incy, real *sparam);
extern int bsplvd_(real *t, integer *k, real *x, integer *ileft, real *vnikx, integer *nderiv);
/*:ref: bsplvn_ 14 6 6 4 4 6 4 6 */
extern int cpoco_(complex *a, integer *lda, integer *n, real *rcond, complex *z__, integer *info);
/*:ref: scasum_ 6 3 4 8 4 */
/*:ref: cpofa_ 14 4 8 4 4 4 */
/*:ref: csscal_ 14 4 4 6 8 4 */
/*:ref: caxpy_ 14 6 4 8 8 4 8 4 */
/*:ref: cdotc_ 8 6 8 4 8 4 8 4 */
extern doublereal dcsevl_(doublereal *x, doublereal *a, integer *n);
/*:ref: xerror_ 14 5 13 4 4 4 124 */
extern int dpnnzr_(integer *i__, doublereal *xval, integer *iplace, doublereal *sx, integer *ix, integer *ircx);
/*:ref: xerror_ 14 5 13 4 4 4 124 */
/*:ref: iploc_ 4 3 4 7 4 */
extern int exbvp_(real *y, integer *nrowy, real *xpts, real *a, integer *nrowa, real *alpha, real *b, integer *nrowb, real *beta, integer *iflag, real *work, integer *iwork);
/* comlen ml8sz_ 28 */
/* comlen ml18jr_ 72 */
/* comlen ml15to_ 116 */
/* comlen ml17bw_ 72 */
/* comlen ml5mco_ 28 */
/*:ref: bvpor_ 14 31 6 4 4 6 4 6 4 6 4 6 4 6 4 4 6 4 6 4 4 6 4 6 6 6 6 6 6 6 6 4 4 */
/*:ref: xerrwv_ 14 11 13 4 4 4 4 4 4 4 6 6 124 */
extern int passb3_(integer *ido, integer *l1, real *cc, real *ch, real *wa1, real *wa2);
extern int reort_(integer *ncomp, real *y, real *yp, real *yhp, integer *niv, real *w, real *s, real *p, integer *ip, real *stowa, integer *iflag);
/* comlen ml8sz_ 28 */
/* comlen ml15to_ 116 */
/* comlen ml18jr_ 72 */
/*:ref: stor1_ 14 7 6 6 6 6 4 4 4 */
/*:ref: mgsbv_ 14 13 4 4 6 4 4 4 6 6 4 4 6 6 6 */
/*:ref: stway_ 14 5 6 6 6 4 6 */
/*:ref: sdot_ 6 5 4 6 4 6 4 */
extern int srotmg_(real *sd1, real *sd2, real *sx1, real *sy1, real *sparam);
extern int bsplvn_(real *t, integer *jhigh, integer *index, real *x, integer *ileft, real *vnikx);
extern int cpodi_(complex *a, integer *lda, integer *n, real *det, integer *job);
/*:ref: cscal_ 14 4 4 8 8 4 */
/*:ref: caxpy_ 14 6 4 8 8 4 8 4 */
extern doublereal dcv_(doublereal *xval, integer *ndata, integer *nconst, integer *nord, integer *nbkpt, doublereal *bkpt, doublereal *w);
/*:ref: dfspvn_ 14 6 7 4 4 7 4 7 */
/*:ref: ddot_ 7 5 4 7 4 7 4 */
extern doublereal dpoch_(doublereal *a, doublereal *x);
/*:ref: xerror_ 14 5 13 4 4 4 124 */
/*:ref: dfac_ 7 1 4 */
/*:ref: dlnrel_ 7 1 7 */
/*:ref: d9lgmc_ 7 1 7 */
/*:ref: dgamma_ 7 1 7 */
/*:ref: dgamr_ 7 1 7 */
/*:ref: dcot_ 7 1 7 */
/*:ref: dlgams_ 14 3 7 7 7 */
extern int exint_(real *x, integer *n, integer *kode, integer *m, real *tol, real *en, integer *ierr);
/*:ref: r1mach_ 6 1 4 */
/*:ref: i1mach_ 4 1 4 */
/*:ref: psixn_ 6 1 4 */
/*:ref: xerror_ 14 5 13 4 4 4 124 */
extern int passb4_(integer *ido, integer *l1, real *cc, real *ch, real *wa1, real *wa2, real *wa3);
extern E_f rf_(real *x, real *y, real *z__, integer *ier);
/*:ref: r1mach_ 6 1 4 */
/*:ref: xerror_ 14 5 13 4 4 4 124 */
extern int sscal_(integer *n, real *sa, real *sx, integer *incx);
extern int bsppp_(real *t, real *a, integer *n, integer *k, integer *ldc, real *c__, real *xi, integer *lxi, real *work);
/*:ref: bspdr_ 14 6 6 6 4 4 4 6 */
/*:ref: bspev_ 14 9 6 6 4 4 4 6 4 6 6 */
/*:ref: xerror_ 14 5 13 4 4 4 124 */
extern int cpofa_(complex *a, integer *lda, integer *n, integer *info);
/*:ref: cdotc_ 8 6 8 4 8 4 8 4 */
extern doublereal ddaws_(doublereal *x);
/*:ref: d1mach_ 7 1 4 */
/*:ref: initds_ 4 3 7 4 6 */
/*:ref: dcsevl_ 7 3 7 7 4 */
/*:ref: xerror_ 14 5 13 4 4 4 124 */
extern doublereal dpoch1_(doublereal *a, doublereal *x);
/*:ref: d1mach_ 7 1 4 */
/*:ref: dpsi_ 7 1 7 */
/*:ref: xerror_ 14 5 13 4 4 4 124 */
/*:ref: dexprl_ 7 1 7 */
/*:ref: dcot_ 7 1 7 */
/*:ref: dpoch_ 7 2 7 7 */
extern E_f exprel_(real *x);
/*:ref: r1mach_ 6 1 4 */
extern int passb5_(integer *ido, integer *l1, real *cc, real *ch, real *wa1, real *wa2, real *wa3, real *wa4);
extern int rfftb_(integer *n, real *r__, real *wsave);
/*:ref: rfftb1_ 14 5 4 6 6 6 6 */
extern int ssico_(real *a, integer *lda, integer *n, integer *kpvt, real *rcond, real *z__);
/*:ref: sasum_ 6 3 4 6 4 */
/*:ref: ssifa_ 14 5 6 4 4 4 4 */
/*:ref: saxpy_ 14 6 4 6 6 4 6 4 */
/*:ref: sscal_ 14 4 4 6 6 4 */
/*:ref: sdot_ 6 5 4 6 4 6 4 */
extern int bspvd_(real *t, integer *k, integer *nderiv, real *x, integer *ileft, integer *ldvnik, real *vnikx, real *work);
/*:ref: bspvn_ 14 9 6 4 4 4 6 4 6 6 4 */
/*:ref: xerror_ 14 5 13 4 4 4 124 */
extern int cpofs_(complex *a, integer *lda, integer *n, complex *v, integer *itask, integer *ind, complex *work);
/*:ref: cpoco_ 14 6 8 4 4 6 8 4 */
/*:ref: r1mach_ 6 1 4 */
/*:ref: xerror_ 14 5 13 4 4 4 124 */
/*:ref: cposl_ 14 4 8 4 4 8 */
/*:ref: xerrwv_ 14 11 13 4 4 4 4 4 4 4 4 4 124 */
extern int ddcorb_(doublereal *dfdy, doublereal *el, S_fp fa, doublereal *h__, integer *impl, integer *ipvt, integer *matdim, integer *miter, integer *ml, integer *mu, integer *n, integer *nde, integer *nq, doublereal *t, doublereal *y, doublereal *yh, doublereal *ywt, logical *evalfa, doublereal *save1, doublereal *save2, doublereal *a, doublereal *d__);
/*:ref: dnrm2_ 7 3 4 7 4 */
/*:ref: dgesl_ 14 6 7 4 4 4 7 4 */
/*:ref: dgbsl_ 14 8 7 4 4 4 4 4 7 4 */
/*:ref: users_ 14 12 7 7 7 7 7 7 7 7 4 4 4 4 */
extern int dpoco_(doublereal *a, integer *lda, integer *n, doublereal *rcond, doublereal *z__, integer *info);
/*:ref: dasum_ 7 3 4 7 4 */
/*:ref: dpofa_ 14 4 7 4 4 4 */
/*:ref: dscal_ 14 4 4 7 7 4 */
/*:ref: daxpy_ 14 6 4 7 7 4 7 4 */
/*:ref: ddot_ 7 5 4 7 4 7 4 */
extern int ezfft1_(integer *n, real *wa, integer *ifac);
extern int passf_(integer *nac, integer *ido, integer *ip, integer *l1, integer *idl1, real *cc, real *c1, real *c2, real *ch, real *ch2, real *wa);
extern int rfftb1_(integer *n, real *c__, real *ch, real *wa, integer *ifac);
/*:ref: radb4_ 14 7 4 4 6 6 6 6 6 */
/*:ref: radb2_ 14 5 4 4 6 6 6 */
/*:ref: radb3_ 14 6 4 4 6 6 6 6 */
/*:ref: radb5_ 14 8 4 4 6 6 6 6 6 6 */
/*:ref: radbg_ 14 10 4 4 4 4 6 6 6 6 6 6 */
extern int ssidi_(real *a, integer *lda, integer *n, integer *kpvt, real *det, integer *inert, real *work, integer *job);
/*:ref: scopy_ 14 5 4 6 4 6 4 */
/*:ref: sdot_ 6 5 4 6 4 6 4 */
/*:ref: saxpy_ 14 6 4 6 6 4 6 4 */
/*:ref: sswap_ 14 5 4 6 4 6 4 */
extern int bspvn_(real *t, integer *jhigh, integer *k, integer *index, real *x, integer *ileft, real *vnikx, real *work, integer *iwork);
/*:ref: xerror_ 14 5 13 4 4 4 124 */
extern int cpoir_(complex *a, integer *lda, integer *n, complex *v, integer *itask, integer *ind, complex *work);
/*:ref: ccopy_ 14 5 4 8 4 8 4 */
/*:ref: cpofa_ 14 4 8 4 4 4 */
/*:ref: cposl_ 14 4 8 4 4 8 */
/*:ref: scasum_ 6 3 4 8 4 */
/*:ref: dcdot_ 14 8 4 7 8 4 8 4 7 7 */
/*:ref: r1mach_ 6 1 4 */
/*:ref: xerror_ 14 5 13 4 4 4 124 */
/*:ref: xerrwv_ 14 11 13 4 4 4 4 4 4 4 4 4 124 */
extern int ddcstb_(integer *maxord, integer *mint, integer *iswflg, doublereal *el, doublereal *tq);
extern int dpodi_(doublereal *a, integer *lda, integer *n, doublereal *det, integer *job);
/*:ref: dscal_ 14 4 4 7 7 4 */
/*:ref: daxpy_ 14 6 4 7 7 4 7 4 */
extern int ezfftb_(integer *n, real *r__, real *azero, real *a, real *b, real *wsave);
/*:ref: rfftb_ 14 3 4 6 6 */
extern int passf2_(integer *ido, integer *l1, real *cc, real *ch, real *wa1);
extern int rfftf_(integer *n, real *r__, real *wsave);
/*:ref: rfftf1_ 14 5 4 6 6 6 6 */
extern int ssiev_(real *a, integer *lda, integer *n, real *e, real *work, integer *job, integer *info);
/*:ref: xerror_ 14 5 13 4 4 4 124 */
/*:ref: tred1_ 14 6 4 4 6 6 6 6 */
/*:ref: tqlrat_ 14 4 4 6 6 4 */
/*:ref: tred2_ 14 6 4 4 6 6 6 6 */
/*:ref: imtql2_ 14 6 4 4 6 6 6 4 */
extern int bsqad_(real *t, real *bcoef, integer *n, integer *k, real *x1, real *x2, real *bquad, real *work);
/*:ref: intrv_ 14 6 6 4 6 4 4 4 */
/*:ref: bvalu_ 6 8 6 6 4 4 4 6 4 6 */
/*:ref: xerror_ 14 5 13 4 4 4 124 */
extern int cposl_(complex *a, integer *lda, integer *n, complex *b);
/*:ref: cdotc_ 8 6 8 4 8 4 8 4 */
/*:ref: caxpy_ 14 6 4 8 8 4 8 4 */
extern int dde2_(S_fp df, integer *neq, doublereal *t, doublereal *y, doublereal *tout, integer *info, doublereal *rtol, doublereal *atol, integer *idid, doublereal *ypout, doublereal *yp, doublereal *yy, doublereal *wt, doublereal *p, doublereal *phi, doublereal *alpha, doublereal *beta, doublereal *psi, doublereal *v, doublereal *w, doublereal *sig, doublereal *g, doublereal *h__, doublereal *eps, doublereal *x, doublereal *hold, doublereal *told, doublereal *delsgn, doublereal *tstop, doublereal *twou, doublereal *fouru, logical *start, logical *phase1, logical *nornd, logical *stiff, logical *intout, integer *ns, integer *kord, integer *kold, integer *init, integer *ksteps, integer *kle4, integer *iquit, doublereal *rpar, integer *ipar);
/*:ref: d1mach_ 7 1 4 */
/*:ref: xerrwv_ 14 11 13 4 4 4 4 4 4 4 6 6 124 */
/*:ref: dintrp_ 14 9 7 7 7 7 7 4 4 7 7 */
/*:ref: dstep2_ 14 30 214 4 7 7 7 7 7 12 7 4 4 12 7 7 7 7 7 7 7 7 7 7 12 4 12 4 7 7 7 4 */
extern int dpofa_(doublereal *a, integer *lda, integer *n, integer *info);
/*:ref: ddot_ 7 5 4 7 4 7 4 */
extern int ezfftf_(integer *n, real *r__, real *azero, real *a, real *b, real *wsave);
/*:ref: rfftf_ 14 3 4 6 6 */
extern int passf3_(integer *ido, integer *l1, real *cc, real *ch, real *wa1, real *wa2);
extern int rfftf1_(integer *n, real *c__, real *ch, real *wa, integer *ifac);
/*:ref: radf4_ 14 7 4 4 6 6 6 6 6 */
/*:ref: radf2_ 14 5 4 4 6 6 6 */
/*:ref: radf3_ 14 6 4 4 6 6 6 6 */
/*:ref: radf5_ 14 8 4 4 6 6 6 6 6 6 */
/*:ref: radfg_ 14 10 4 4 4 4 6 6 6 6 6 6 */
extern int ssifa_(real *a, integer *lda, integer *n, integer *kpvt, integer *info);
/*:ref: isamax_ 4 3 4 6 4 */
/*:ref: sswap_ 14 5 4 6 4 6 4 */
/*:ref: saxpy_ 14 6 4 6 6 4 6 4 */
extern E_f bsrh_(real *xll, real *xrr, integer *iz, real *c__, real *a, real *bh, E_fp f, real *sgn);
/* comlen cblkt_ 28 */
extern int cppco_(complex *ap, integer *n, real *rcond, complex *z__, integer *info);
/*:ref: scasum_ 6 3 4 8 4 */
/*:ref: cppfa_ 14 3 8 4 4 */
/*:ref: csscal_ 14 4 4 6 8 4 */
/*:ref: caxpy_ 14 6 4 8 8 4 8 4 */
/*:ref: cdotc_ 8 6 8 4 8 4 8 4 */
extern int ddeabm_(U_fp df, integer *neq, doublereal *t, doublereal *y, doublereal *tout, integer *info, doublereal *rtol, doublereal *atol, integer *idid, doublereal *rwork, integer *lrw, integer *iwork, integer *liw, doublereal *rpar, integer *ipar);
/*:ref: xerrwv_ 14 11 13 4 4 4 4 4 4 4 6 6 124 */
/*:ref: ddes_ 14 51 200 4 7 7 7 4 7 7 4 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 12 12 12 12 12 4 4 4 4 4 4 4 4 4 4 4 7 4 */
extern int dpofs_(doublereal *a, integer *lda, integer *n, doublereal *v, integer *itask, integer *ind, doublereal *work);
/*:ref: dpoco_ 14 6 7 4 4 7 7 4 */
/*:ref: d1mach_ 7 1 4 */
/*:ref: xerror_ 14 5 13 4 4 4 124 */
/*:ref: dposl_ 14 4 7 4 4 7 */
/*:ref: xerrwv_ 14 11 13 4 4 4 4 4 4 4 4 4 124 */
extern int ezffti_(integer *n, real *wsave);
/*:ref: ezfft1_ 14 3 4 6 6 */
extern int passf4_(integer *ido, integer *l1, real *cc, real *ch, real *wa1, real *wa2, real *wa3);
extern int rffti_(integer *n, real *wsave);
/*:ref: rffti1_ 14 3 4 6 6 */
extern int ssisl_(real *a, integer *lda, integer *n, integer *kpvt, real *b);
/*:ref: saxpy_ 14 6 4 6 6 4 6 4 */
/*:ref: sdot_ 6 5 4 6 4 6 4 */
extern E_f bvalu_(real *t, real *a, integer *n, integer *k, integer *ideriv, real *x, integer *inbv, real *work);
/*:ref: intrv_ 14 6 6 4 6 4 4 4 */
/*:ref: xerror_ 14 5 13 4 4 4 124 */
extern int cppdi_(complex *ap, integer *n, real *det, integer *job);
/*:ref: cscal_ 14 4 4 8 8 4 */
/*:ref: caxpy_ 14 6 4 8 8 4 8 4 */
extern int ddebdf_(U_fp df, integer *neq, doublereal *t, doublereal *y, doublereal *tout, integer *info, doublereal *rtol, doublereal *atol, integer *idid, doublereal *rwork, integer *lrw, integer *iwork, integer *liw, doublereal *rpar, integer *ipar, U_fp djac);
/* comlen ddebd1_ 1876 */
/*:ref: xerrwv_ 14 11 13 4 4 4 4 4 4 4 6 6 124 */
/*:ref: dlsod_ 14 23 200 4 7 7 7 7 7 4 7 7 7 7 7 7 7 4 200 12 7 7 7 7 4 */
extern int dpolcf_(doublereal *xx, integer *n, doublereal *x, doublereal *c__, doublereal *d__, doublereal *work);
extern E_f fac_(integer *n);
/*:ref: gamlim_ 14 2 6 6 */
/*:ref: xerror_ 14 5 13 4 4 4 124 */
/*:ref: r9lgmc_ 6 1 6 */
extern int passf5_(integer *ido, integer *l1, real *cc, real *ch, real *wa1, real *wa2, real *wa3, real *wa4);
extern int rffti1_(integer *n, real *wa, integer *ifac);
extern int ssort_(real *x, real *y, integer *n, integer *kflag);
/*:ref: xerror_ 14 5 13 4 4 4 124 */
extern E_f bvalue_(real *t, real *a, integer *n, integer *k, real *x, integer *ideriv);
/*:ref: interv_ 14 5 6 4 6 4 4 */
extern int cppfa_(complex *ap, integer *n, integer *info);
/*:ref: cdotc_ 8 6 8 4 8 4 8 4 */
extern int dderkf_(U_fp df, integer *neq, doublereal *t, doublereal *y, doublereal *tout, integer *info, doublereal *rtol, doublereal *atol, integer *idid, doublereal *rwork, integer *lrw, integer *iwork, integer *liw, doublereal *rpar, integer *ipar);
/*:ref: xerrwv_ 14 11 13 4 4 4 4 4 4 4 6 6 124 */
/*:ref: drkfs_ 14 32 200 4 7 7 7 4 7 7 4 7 7 7 7 7 7 7 7 7 7 7 7 7 4 4 4 4 12 12 4 4 7 4 */
extern int dpolft_(integer *n, doublereal *x, doublereal *y, doublereal *w, integer *maxdeg, integer *ndeg, doublereal *eps, doublereal *r__, integer *ierr, doublereal *a);
/*:ref: xerror_ 14 5 13 4 4 4 124 */
/*:ref: dp1vlu_ 14 6 4 4 7 7 6 7 */
extern int fc_(integer *ndata, real *xdata, real *ydata, real *sddata, integer *nord, integer *nbkpt, real *bkpt, integer *nconst, real *xconst, real *yconst, integer *nderiv, integer *mode, real *coeff, real *w, integer *iw);
/*:ref: fcmn_ 14 23 4 6 6 6 4 4 6 4 6 6 4 4 6 6 6 6 6 6 4 6 4 6 4 */
extern int pchce_(integer *ic, real *vc, integer *n, real *x, real *h__, real *slope, real *d__, integer *incfd, integer *ierr);
/*:ref: pchdf_ 6 4 4 6 6 4 */
/*:ref: pchst_ 6 2 6 6 */
/*:ref: xerror_ 14 5 13 4 4 4 124 */
extern int rg_(integer *nm, integer *n, real *a, real *wr, real *wi, integer *matz, real *z__, integer *iv1, real *fv1, integer *ierr);
/*:ref: balanc_ 14 6 4 4 6 4 4 6 */
/*:ref: elmhes_ 14 6 4 4 4 4 6 4 */
/*:ref: hqr_ 14 8 4 4 4 4 6 6 6 4 */
/*:ref: eltran_ 14 7 4 4 4 4 6 4 6 */
/*:ref: hqr2_ 14 9 4 4 4 4 6 6 6 6 4 */
/*:ref: balbak_ 14 7 4 4 4 4 6 4 6 */
extern int sspco_(real *ap, integer *n, integer *kpvt, real *rcond, real *z__);
/*:ref: sasum_ 6 3 4 6 4 */
/*:ref: sspfa_ 14 4 6 4 4 4 */
/*:ref: saxpy_ 14 6 4 6 6 4 6 4 */
/*:ref: sscal_ 14 4 4 6 6 4 */
/*:ref: sdot_ 6 5 4 6 4 6 4 */
extern int bvder_(real *x, real *y, real *yp, real *g, integer *ipar);
/* comlen ml8sz_ 28 */
/* comlen mlivp_ 4 */
/*:ref: uivp_ 14 3 6 6 6 */
/*:ref: fmat_ 14 3 6 6 6 */
/*:ref: gvec_ 14 2 6 6 */
/*:ref: uvec_ 14 3 6 6 6 */
extern int cppsl_(complex *ap, integer *n, complex *b);
/*:ref: cdotc_ 8 6 8 4 8 4 8 4 */
/*:ref: caxpy_ 14 6 4 8 8 4 8 4 */
extern int ddes_(S_fp df, integer *neq, doublereal *t, doublereal *y, doublereal *tout, integer *info, doublereal *rtol, doublereal *atol, integer *idid, doublereal *ypout, doublereal *yp, doublereal *yy, doublereal *wt, doublereal *p, doublereal *phi, doublereal *alpha, doublereal *beta, doublereal *psi, doublereal *v, doublereal *w, doublereal *sig, doublereal *g, real *gi, doublereal *h__, doublereal *eps, doublereal *x, real *xold, doublereal *hold, doublereal *told, doublereal *delsgn, doublereal *tstop, doublereal *twou, doublereal *fouru, logical *start, logical *phase1, logical *nornd, logical *stiff, logical *intout, integer *ns, integer *kord, integer *kold, integer *init, integer *ksteps, integer *kle4, integer *iquit, integer *kprev, integer *ivc, integer *iv, integer *kgi, doublereal *rpar, integer *ipar);
/*:ref: d1mach_ 7 1 4 */
/*:ref: xerrwv_ 14 11 13 4 4 4 4 4 4 4 6 6 124 */
/*:ref: dintp_ 14 17 7 7 7 7 7 4 4 7 4 4 4 6 7 7 7 6 7 */
/*:ref: dsteps_ 14 36 214 4 7 7 7 7 7 12 7 4 4 12 7 7 7 7 7 7 7 7 7 7 12 4 12 4 7 7 6 4 4 4 4 6 7 4 */
extern int dpolvl_(integer *nder, doublereal *xx, doublereal *yfit, doublereal *yp, integer *n, doublereal *x, doublereal *c__, doublereal *work, integer *ierr);
extern int fcmn_(integer *ndata, real *xdata, real *ydata, real *sddata, integer *nord, integer *nbkpt, real *bkptin, integer *nconst, real *xconst, real *yconst, integer *nderiv, integer *mode, real *coeff, real *bf, real *xtemp, real *ptemp, real *bkpt, real *g, integer *mdg, real *w, integer *mdw, real *work, integer *iwork);
/*:ref: bndsol_ 14 9 4 6 4 4 4 4 6 4 6 */
/*:ref: xerrwv_ 14 11 13 4 4 4 4 4 4 4 6 6 124 */
/*:ref: lsei_ 14 13 6 4 4 4 4 4 6 6 6 6 4 6 4 */
/*:ref: xerror_ 14 5 13 4 4 4 124 */
/*:ref: scopy_ 14 5 4 6 4 6 4 */
/*:ref: ssort_ 14 4 6 6 4 4 */
/*:ref: bndacc_ 14 7 6 4 4 4 4 4 4 */
/*:ref: bsplvn_ 14 6 6 4 4 6 4 6 */
/*:ref: sscal_ 14 4 4 6 6 4 */
/*:ref: bsplvd_ 14 6 6 4 6 4 6 4 */
/*:ref: saxpy_ 14 6 4 6 6 4 6 4 */
extern int pchci_(integer *n, real *h__, real *slope, real *d__, integer *incfd);
/*:ref: pchst_ 6 2 6 6 */
extern E_f rgauss_(real *xmean, real *sd);
/*:ref: rand_ 6 1 6 */
extern int sspdi_(real *ap, integer *n, integer *kpvt, real *det, integer *inert, real *work, integer *job);
/*:ref: scopy_ 14 5 4 6 4 6 4 */
/*:ref: sdot_ 6 5 4 6 4 6 4 */
/*:ref: saxpy_ 14 6 4 6 6 4 6 4 */
/*:ref: sswap_ 14 5 4 6 4 6 4 */
extern int bvpor_(real *y, integer *nrowy, integer *ncomp, real *xpts, integer *nxpts, real *a, integer *nrowa, real *alpha, integer *nic, real *b, integer *nrowb, real *beta, integer *nfc, integer *iflag, real *z__, integer *mxnon, real *p, integer *ntp, integer *ip, real *w, integer *niv, real *yhp, real *u, real *v, real *coef, real *s, real *stowa, real *g, real *work, integer *iwork, integer *nfcc);
/* comlen ml8sz_ 28 */
/* comlen ml15to_ 116 */
/* comlen ml18jr_ 72 */
/*:ref: lssuds_ 14 20 6 6 6 4 4 4 6 4 4 4 4 4 6 6 4 6 6 6 4 6 */
/*:ref: svecs_ 14 7 4 4 6 6 4 4 4 */
/*:ref: stor1_ 14 7 6 6 6 6 4 4 4 */
/*:ref: stway_ 14 5 6 6 6 4 6 */
/*:ref: rkfab_ 14 21 4 6 4 4 4 6 4 6 4 4 6 4 6 6 6 6 6 6 6 4 4 */
/*:ref: scoef_ 14 18 6 6 4 4 4 4 6 6 6 4 6 6 6 6 6 4 4 4 */
/*:ref: bksol_ 14 3 4 6 6 */
/*:ref: sdot_ 6 5 4 6 4 6 4 */
extern int cpqr79_(integer *ndeg, complex *coeff, complex *root, integer *ierr, real *work);
/*:ref: comqr_ 14 9 4 4 4 4 6 6 6 6 4 */
/*:ref: xerror_ 14 5 13 4 4 4 124 */
extern doublereal ddiff_(doublereal *x, doublereal *y);
extern int dpopt_(doublereal *prgopt, integer *mrelas, integer *nvars__, integer *info, doublereal *csc, integer *ibasis, doublereal *ropt, integer *intopt, logical *lopt);
/*:ref: d1mach_ 7 1 4 */
/*:ref: xerror_ 14 5 13 4 4 4 124 */
extern int fdjac1_(S_fp fcn, integer *n, real *x, real *fvec, real *fjac, integer *ldfjac, integer *iflag, integer *ml, integer *mu, real *epsfcn, real *wa1, real *wa2);
/*:ref: r1mach_ 6 1 4 */
extern int pchcs_(real *switch__, integer *n, real *h__, real *slope, real *d__, integer *incfd, integer *ierr);
/*:ref: pchst_ 6 2 6 6 */
/*:ref: pchsw_ 14 7 6 4 6 6 6 6 4 */
extern int rgg_(integer *nm, integer *n, real *a, real *b, real *alfr, real *alfi, real *beta, integer *matz, real *z__, integer *ierr);
/*:ref: qzhes_ 14 6 4 4 6 6 12 6 */
/*:ref: qzit_ 14 8 4 4 6 6 6 12 6 4 */
/*:ref: qzval_ 14 9 4 4 6 6 6 6 6 12 6 */
/*:ref: qzvec_ 14 8 4 4 6 6 6 6 6 6 */
extern int sspev_(real *a, integer *n, real *e, real *v, integer *ldv, real *work, integer *job, integer *info);
/*:ref: xerror_ 14 5 13 4 4 4 124 */
/*:ref: tred3_ 14 6 4 4 6 6 6 6 */
/*:ref: tqlrat_ 14 4 4 6 6 4 */
/*:ref: imtql2_ 14 6 4 4 6 6 6 4 */
/*:ref: trbak3_ 14 6 4 4 4 6 4 6 */
extern int bvsup_(real *y, integer *nrowy, integer *ncomp, real *xpts, integer *nxpts, real *a, integer *nrowa, real *alpha, integer *nic, real *b, integer *nrowb, real *beta, integer *nfc, integer *igofx, real *re, real *ae, integer *iflag, real *work, integer *ndw, integer *iwork, integer *ndiw, integer *neqivp);
/* comlen ml8sz_ 28 */
/* comlen ml18jr_ 72 */
/* comlen ml17bw_ 72 */
/* comlen ml15to_ 116 */
/* comlen ml5mco_ 28 */
/*:ref: macon_ 14 0 */
/*:ref: xerrwv_ 14 11 13 4 4 4 4 4 4 4 6 6 124 */
/*:ref: exbvp_ 14 12 6 4 6 6 4 6 6 4 6 4 6 4 */
extern int cproc_(integer *nd, complex *bd, integer *nm1, real *bm1, integer *nm2, real *bm2, integer *na, real *aa, complex *x, complex *y, integer *m, complex *a, complex *b, complex *c__, complex *d__, complex *w, real *yy);
extern int ddntlb_(doublereal *eps, S_fp f, S_fp fa, doublereal *hmax, doublereal *hold, integer *impl, integer *jtask, integer *matdim, integer *maxord, integer *mint, integer *miter, integer *ml, integer *mu, integer *n, integer *nde, doublereal *save1, doublereal *t, doublereal *y, doublereal *ywt, doublereal *h__, integer *mntold, integer *mtrold, integer *nfe, doublereal *rc, doublereal *yh, doublereal *a, logical *convrg, doublereal *el, logical *ier, integer *ipvt, integer *nq, integer *nwait, doublereal *rh, doublereal *rmax, doublereal *save2, doublereal *tq, doublereal *trend, integer *iswflg);
/*:ref: ddcstb_ 14 5 4 4 4 7 7 */
/*:ref: users_ 14 12 7 7 7 7 7 7 7 7 4 4 4 4 */
/*:ref: dgefa_ 14 5 7 4 4 4 4 */
/*:ref: dgesl_ 14 6 7 4 4 4 7 4 */
/*:ref: dgbfa_ 14 7 7 4 4 4 4 4 4 */
/*:ref: dgbsl_ 14 8 7 4 4 4 4 4 7 4 */
/*:ref: dnrm2_ 7 3 4 7 4 */
/*:ref: ddsclb_ 14 8 7 4 4 7 7 7 7 7 */
extern int dposl_(doublereal *a, integer *lda, integer *n, doublereal *b);
/*:ref: ddot_ 7 5 4 7 4 7 4 */
/*:ref: daxpy_ 14 6 4 7 7 4 7 4 */
extern int fdjac2_(S_fp fcn, integer *m, integer *n, real *x, real *fvec, real *fjac, integer *ldfjac, integer *iflag, real *epsfcn, real *wa);
/*:ref: r1mach_ 6 1 4 */
extern E_f pchdf_(integer *k, real *x, real *s, integer *ierr);
/*:ref: xerror_ 14 5 13 4 4 4 124 */
extern E_f rj_(real *x, real *y, real *z__, real *p, integer *ier);
/*:ref: r1mach_ 6 1 4 */
/*:ref: xerror_ 14 5 13 4 4 4 124 */
/*:ref: rc_ 6 3 6 6 4 */
extern int sspfa_(real *ap, integer *n, integer *kpvt, integer *info);
/*:ref: isamax_ 4 3 4 6 4 */
/*:ref: sswap_ 14 5 4 6 4 6 4 */
/*:ref: saxpy_ 14 6 4 6 6 4 6 4 */
extern C_f c0lgmc_(complex * ret_val, complex *z__);
/*:ref: r1mach_ 6 1 4 */
/*:ref: c9ln2r_ 8 2 8 8 */
extern int cprocp_(integer *nd, complex *bd, integer *nm1, real *bm1, integer *nm2, real *bm2, integer *na, real *aa, complex *x, complex *y, integer *m, complex *a, complex *b, complex *c__, complex *d__, complex *u, real *yy);
extern int ddntpb_(doublereal *h__, integer *k, integer *n, integer *nq, doublereal *t, doublereal *tout, doublereal *yh, doublereal *y);
extern int dppco_(doublereal *ap, integer *n, doublereal *rcond, doublereal *z__, integer *info);
/*:ref: dasum_ 7 3 4 7 4 */
/*:ref: dppfa_ 14 3 7 4 4 */
/*:ref: dscal_ 14 4 4 7 7 4 */
/*:ref: daxpy_ 14 6 4 7 7 4 7 4 */
/*:ref: ddot_ 7 5 4 7 4 7 4 */
extern int fdjac3_(S_fp fcn, integer *m, integer *n, real *x, real *fvec, real *fjac, integer *ldfjac, integer *iflag, real *epsfcn, real *wa);
/*:ref: r1mach_ 6 1 4 */
extern int pchdoc_(void);
extern int rkfab_(integer *ncomp, real *xpts, integer *nxpts, integer *nfc, integer *iflag, real *z__, integer *mxnon, real *p, integer *ntp, integer *ip, real *yhp, integer *niv, real *u, real *v, real *w, real *s, real *stowa, real *g, real *work, integer *iwork, integer *nfcc);
/* comlen ml8sz_ 28 */
/* comlen ml15to_ 116 */
/* comlen ml18jr_ 72 */
/* comlen ml17bw_ 72 */
/*:ref: derkf_ 14 15 200 4 6 6 6 4 6 6 4 6 4 4 4 6 4 */
/*:ref: deabm_ 14 15 200 4 6 6 6 4 6 6 4 6 4 4 4 6 4 */
/*:ref: reort_ 14 11 4 6 6 6 4 6 6 6 4 6 4 */
/*:ref: stor1_ 14 7 6 6 6 6 4 4 4 */
extern int sspsl_(real *ap, integer *n, integer *kpvt, real *b);
/*:ref: saxpy_ 14 6 4 6 6 4 6 4 */
/*:ref: sdot_ 6 5 4 6 4 6 4 */
extern C_f c9lgmc_(complex * ret_val, complex *zin);
/*:ref: r1mach_ 6 1 4 */
/*:ref: xerror_ 14 5 13 4 4 4 124 */
extern int cprod_(integer *nd, complex *bd, integer *nm1, real *bm1, integer *nm2, real *bm2, integer *na, real *aa, real *x, real *yy, integer *m, real *a, real *b, real *c__, complex *d__, complex *w, complex *y);
extern int ddoglg_(integer *n, doublereal *r__, integer *lr, doublereal *diag, doublereal *qtb, doublereal *delta, doublereal *x, doublereal *wa1, doublereal *wa2);
/*:ref: d1mach_ 7 1 4 */
/*:ref: denorm_ 7 2 4 7 */
extern int dppdi_(doublereal *ap, integer *n, doublereal *det, integer *job);
/*:ref: dscal_ 14 4 4 7 7 4 */
/*:ref: daxpy_ 14 6 4 7 7 4 7 4 */
extern int fdump_(void);
extern int pchfd_(integer *n, real *x, real *f, real *d__, integer *incfd, logical *skip, integer *ne, real *xe, real *fe, real *de, integer *ierr);
/*:ref: chfdv_ 14 12 6 6 6 6 6 6 4 6 6 6 4 4 */
/*:ref: xerror_ 14 5 13 4 4 4 124 */
extern int rpqr79_(integer *ndeg, real *coeff, complex *root, integer *ierr, real *work);
/*:ref: hqr_ 14 8 4 4 4 4 6 6 6 4 */
/*:ref: xerror_ 14 5 13 4 4 4 124 */
extern int ssvdc_(real *x, integer *ldx, integer *n, integer *p, real *s, real *e, real *u, integer *ldu, real *v, integer *ldv, real *work, integer *job, integer *info);
/*:ref: snrm2_ 6 3 4 6 4 */
/*:ref: sscal_ 14 4 4 6 6 4 */
/*:ref: sdot_ 6 5 4 6 4 6 4 */
/*:ref: saxpy_ 14 6 4 6 6 4 6 4 */
/*:ref: srotg_ 14 4 6 6 6 6 */
/*:ref: srot_ 14 7 4 6 4 6 4 6 6 */
/*:ref: sswap_ 14 5 4 6 4 6 4 */
extern C_f c9ln2r_(complex * ret_val, complex *z__);
/*:ref: r9ln2r_ 6 1 6 */
/*:ref: r9atn1_ 6 1 6 */
extern int cprodp_(integer *nd, complex *bd, integer *nm1, real *bm1, integer *nm2, real *bm2, integer *na, real *aa, real *x, real *yy, integer *m, real *a, real *b, real *c__, complex *d__, complex *u, complex *y);
extern doublereal ddot_(integer *n, doublereal *dx, integer *incx, doublereal *dy, integer *incy);
extern int dppfa_(doublereal *ap, integer *n, integer *info);
/*:ref: ddot_ 7 5 4 7 4 7 4 */
extern int fftdoc_(void);
extern int pchfe_(integer *n, real *x, real *f, real *d__, integer *incfd, logical *skip, integer *ne, real *xe, real *fe, integer *ierr);
/*:ref: chfev_ 14 11 6 6 6 6 6 6 4 6 6 4 4 */
/*:ref: xerror_ 14 5 13 4 4 4 124 */
extern int rpzero_(integer *n, real *a, complex *r__, complex *t, integer *iflg, real *s);
/*:ref: cpzero_ 14 6 4 8 8 8 4 6 */
extern int sswap_(integer *n, real *sx, integer *incx, real *sy, integer *incy);
extern C_f cacos_(complex * ret_val, complex *z__);
/*:ref: casin_ 8 2 8 8 */
extern C_f cpsi_(complex * ret_val, complex *zin);
/*:ref: r1mach_ 6 1 4 */
/*:ref: ccot_ 8 2 8 8 */
/*:ref: xerror_ 14 5 13 4 4 4 124 */
extern int ddpscb_(integer *ksgn, integer *n, integer *nq, doublereal *yh);
extern int dppgq8_(D_fp fun, integer *ldc, doublereal *c__, doublereal *xi, integer *lxi, integer *kk, integer *id, doublereal *a, doublereal *b, integer *inppv, doublereal *err, doublereal *ans, integer *ierr);
/*:ref: xerror_ 14 5 13 4 4 4 124 */
/*:ref: i1mach_ 4 1 4 */
/*:ref: d1mach_ 7 1 4 */
/*:ref: dppval_ 7 8 4 7 7 4 4 4 7 4 */
extern int figi_(integer *nm, integer *n, real *t, real *d__, real *e, real *e2, integer *ierr);
extern E_f pchia_(integer *n, real *x, real *f, real *d__, integer *incfd, logical *skip, real *a, real *b, integer *ierr);
/*:ref: chfiv_ 6 9 6 6 6 6 6 6 6 6 4 */
/*:ref: pchid_ 6 9 4 6 6 6 4 12 4 4 4 */
/*:ref: xerror_ 14 5 13 4 4 4 124 */
extern int rs_(integer *nm, integer *n, real *a, real *w, integer *matz, real *z__, real *fv1, real *fv2, integer *ierr);
/*:ref: tred1_ 14 6 4 4 6 6 6 6 */
/*:ref: tqlrat_ 14 4 4 6 6 4 */
/*:ref: tred2_ 14 6 4 4 6 6 6 6 */
/*:ref: tql2_ 14 6 4 4 6 6 6 4 */
extern int step2_(S_fp f, integer *neqn, real *y, real *x, real *h__, real *eps, real *wt, logical *start, real *hold, integer *k, integer *kold, logical *crash, real *phi, real *p, real *yp, real *psi, real *alpha, real *beta, real *sig, real *v, real *w, real *g, logical *phase1, integer *ns, logical *nornd, integer *ksteps, real *twou, real *fouru, real *rpar, integer *ipar);
/*:ref: r1mach_ 6 1 4 */
/*:ref: hstart_ 14 17 214 4 6 6 6 6 6 4 6 6 6 6 6 6 6 4 6 */
extern C_f cacosh_(complex * ret_val, complex *z__);
/*:ref: cacos_ 8 2 8 8 */
extern int cptsl_(integer *n, complex *d__, complex *e, complex *b);
extern int ddpstb_(doublereal *el, S_fp f, S_fp fa, doublereal *h__, integer *impl, S_fp jacobn, integer *matdim, integer *miter, integer *ml, integer *mu, integer *n, integer *nde, integer *nq, doublereal *save2, doublereal *t, doublereal *y, doublereal *yh, doublereal *ywt, doublereal *uround, integer *nfe, integer *nje, doublereal *a, doublereal *dfdy, logical *ier, integer *ipvt, doublereal *save1, integer *iswflg, doublereal *bnd);
/*:ref: dnrm2_ 7 3 4 7 4 */
/*:ref: dgefa_ 14 5 7 4 4 4 4 */
/*:ref: dgbfa_ 14 7 7 4 4 4 4 4 4 */
/*:ref: users_ 14 12 7 7 7 7 7 7 7 7 4 4 4 4 */
extern int dppqad_(integer *ldc, doublereal *c__, doublereal *xi, integer *lxi, integer *k, doublereal *x1, doublereal *x2, doublereal *pquad);
/*:ref: dintrv_ 14 6 7 4 7 4 4 4 */
/*:ref: xerror_ 14 5 13 4 4 4 124 */
extern int figi2_(integer *nm, integer *n, real *t, real *d__, real *e, real *z__, integer *ierr);
extern int pchic_(integer *ic, real *vc, real *switch__, integer *n, real *x, real *f, real *d__, integer *incfd, real *wk, integer *nwk, integer *ierr);
/*:ref: pchci_ 14 5 4 6 6 6 4 */
/*:ref: pchcs_ 14 7 6 4 6 6 6 4 4 */
/*:ref: pchce_ 14 9 4 6 4 6 6 6 6 4 4 */
/*:ref: xerror_ 14 5 13 4 4 4 124 */
extern int rsb_(integer *nm, integer *n, integer *mb, real *a, real *w, integer *matz, real *z__, real *fv1, real *fv2, integer *ierr);
/*:ref: bandr_ 14 9 4 4 4 6 6 6 6 12 6 */
/*:ref: tqlrat_ 14 4 4 6 6 4 */
/*:ref: tql2_ 14 6 4 4 6 6 6 4 */
extern int steps_(S_fp f, integer *neqn, real *y, real *x, real *h__, real *eps, real *wt, logical *start, real *hold, integer *k, integer *kold, logical *crash, real *phi, real *p, real *yp, real *psi, real *alpha, real *beta, real *sig, real *v, real *w, real *g, logical *phase1, integer *ns, logical *nornd, integer *ksteps, real *twou, real *fouru, real *xold, integer *kprev, integer *ivc, integer *iv, integer *kgi, real *gi, real *rpar, integer *ipar);
/*:ref: r1mach_ 6 1 4 */
/*:ref: hstart_ 14 17 214 4 6 6 6 6 6 4 6 6 6 6 6 6 6 4 6 */
extern E_f carg_(complex *z__);
extern int cpzero_(integer *in, complex *a, complex *r__, complex *t, integer *iflg, real *s);
/*:ref: cpevl_ 14 7 4 4 8 8 8 8 12 */
extern int ddrvb1_(integer *n, doublereal *t, doublereal *y, doublereal *tout, integer *mstate, doublereal *eps, doublereal *work, integer *lenw);
/*:ref: xerror_ 14 5 13 4 4 4 124 */
/*:ref: d1mach_ 7 1 4 */
/*:ref: ddrvb3_ 14 27 4 7 7 200 4 7 4 4 7 7 4 4 4 4 4 4 4 7 7 4 4 4 200 200 4 4 207 */
/*:ref: g_ 7 :*/
extern int dppsl_(doublereal *ap, integer *n, doublereal *b);
/*:ref: ddot_ 7 5 4 7 4 7 4 */
/*:ref: daxpy_ 14 6 4 7 7 4 7 4 */
extern int fnlibd_(void);
extern E_f pchid_(integer *n, real *x, real *f, real *d__, integer *incfd, logical *skip, integer *ia, integer *ib, integer *ierr);
/*:ref: xerror_ 14 5 13 4 4 4 124 */
extern int rsco_(real *rsav, integer *isav);
/* comlen debdf1_ 1004 */
extern int stltjs_(integer *m, integer *n, doublereal *b, doublereal *g, doublereal *fm, integer *jsng, D_fp wf, D_fp phi, real *endptl, real *endptr);
/*:ref: d1mach_ 6 1 4 */
extern C_f casin_(complex * ret_val, complex *zinp);
/*:ref: r1mach_ 6 1 4 */
extern int cqrdc_(complex *x, integer *ldx, integer *n, integer *p, complex *qraux, integer *jpvt, complex *work, integer *job);
/*:ref: cswap_ 14 5 4 8 4 8 4 */
/*:ref: scnrm2_ 6 3 4 8 4 */
/*:ref: cscal_ 14 4 4 8 8 4 */
/*:ref: cdotc_ 8 6 8 4 8 4 8 4 */
/*:ref: caxpy_ 14 6 4 8 8 4 8 4 */
extern int ddrvb2_(integer *n, doublereal *t, doublereal *y, U_fp f, doublereal *tout, integer *mstate, integer *nroot, doublereal *eps, doublereal *ewt, integer *mint, doublereal *work, integer *lenw, integer *iwork, integer *leniw, D_fp g);
/*:ref: xerror_ 14 5 13 4 4 4 124 */
/*:ref: d1mach_ 7 1 4 */
/*:ref: ddrvb3_ 14 27 4 7 7 200 4 7 4 4 7 7 4 4 4 4 4 4 4 7 7 4 4 4 200 200 4 4 207 */
extern doublereal dppval_(integer *ldc, doublereal *c__, doublereal *xi, integer *lxi, integer *k, integer *ideriv, doublereal *x, integer *inppv);
/*:ref: dintrv_ 14 6 7 4 7 4 4 4 */
/*:ref: xerror_ 14 5 13 4 4 4 124 */
extern int fulmat_(integer *i__, integer *j, real *aij, integer *indcat, real *prgopt, real *dattrv, integer *iflag);
/*:ref: xerror_ 14 5 13 4 4 4 124 */
extern int pchim_(integer *n, real *x, real *f, real *d__, integer *incfd, integer *ierr);
/*:ref: pchst_ 6 2 6 6 */
/*:ref: xerror_ 14 5 13 4 4 4 124 */
extern int rsg_(integer *nm, integer *n, real *a, real *b, real *w, integer *matz, real *z__, real *fv1, real *fv2, integer *ierr);
/*:ref: reduc_ 14 6 4 4 6 6 6 4 */
/*:ref: tred1_ 14 6 4 4 6 6 6 6 */
/*:ref: tqlrat_ 14 4 4 6 6 4 */
/*:ref: tred2_ 14 6 4 4 6 6 6 6 */
/*:ref: tql2_ 14 6 4 4 6 6 6 4 */
/*:ref: rebak_ 14 6 4 4 6 6 4 6 */
extern int stod_(integer *neq, real *y, real *yh, integer *nyh, real *yh1, real *ewt, real *savf, real *acor, real *wm, integer *iwm, S_fp f, U_fp jac, real *rpar, integer *ipar);
/* comlen debdf1_ 1004 */
/*:ref: cfod_ 14 3 4 6 6 */
/*:ref: vnwrms_ 6 3 4 6 6 */
/*:ref: pjac_ 14 13 4 6 6 4 6 6 6 6 4 214 200 6 4 */
/*:ref: slvs_ 14 4 6 4 6 6 */
extern C_f casinh_(complex * ret_val, complex *z__);
/*:ref: casin_ 8 2 8 8 */
extern int cqrsl_(complex *x, integer *ldx, integer *n, integer *k, complex *qraux, complex *y, complex *qy, complex *qty, complex *b, complex *rsd, complex *xb, integer *job, integer *info);
/*:ref: ccopy_ 14 5 4 8 4 8 4 */
/*:ref: cdotc_ 8 6 8 4 8 4 8 4 */
/*:ref: caxpy_ 14 6 4 8 8 4 8 4 */
extern int ddrvb3_(integer *n, doublereal *t, doublereal *y, S_fp f, integer *nstate, doublereal *tout, integer *ntask, integer *nroot, doublereal *eps, doublereal *ewt, integer *ierror, integer *mint, integer *miter, integer *impl, integer *ml, integer *mu, integer *mxord, doublereal *hmax, doublereal *work, integer *lenw, integer *iwork, integer *leniw, U_fp jacobn, S_fp fa, integer *nde, integer *mxstep, D_fp g);
/*:ref: d1mach_ 7 1 4 */
/*:ref: xerror_ 14 5 13 4 4 4 124 */
/*:ref: ddntpb_ 14 8 7 4 4 4 7 7 7 7 */
/*:ref: users_ 14 12 7 7 7 7 7 7 7 7 4 4 4 4 */
/*:ref: dgefa_ 14 5 7 4 4 4 4 */
/*:ref: dgesl_ 14 6 7 4 4 4 7 4 */
/*:ref: dgbfa_ 14 7 7 4 4 4 4 4 4 */
/*:ref: dgbsl_ 14 8 7 4 4 4 4 4 7 4 */
/*:ref: dnrm2_ 7 3 4 7 4 */
/*:ref: ddstpb_ 14 48 7 214 214 7 4 200 4 4 4 4 4 4 4 4 7 7 7 7 7 7 4 4 4 4 4 4 4 7 7 7 7 12 7 7 7 4 4 4 4 7 7 7 7 7 7 4 4 4 */
/*:ref: ddzrob_ 14 15 7 207 7 4 4 4 7 7 7 7 7 7 7 7 7 */
extern doublereal dprvec_(integer *m, doublereal *u, doublereal *v);
/*:ref: ddot_ 6 5 4 7 4 7 4 */
extern int fzero_(E_fp f, real *b, real *c__, real *r__, real *re, real *ae, integer *iflag);
/*:ref: r1mach_ 6 1 4 */
extern int pchmc_(integer *n, real *x, real *f, real *d__, integer *incfd, logical *skip, integer *ismon, integer *ierr);
/*:ref: chfmc_ 4 3 6 6 6 */
/*:ref: xerror_ 14 5 13 4 4 4 124 */
extern int rsgab_(integer *nm, integer *n, real *a, real *b, real *w, integer *matz, real *z__, real *fv1, real *fv2, integer *ierr);
/*:ref: reduc2_ 14 6 4 4 6 6 6 4 */
/*:ref: tred1_ 14 6 4 4 6 6 6 6 */
/*:ref: tqlrat_ 14 4 4 6 6 4 */
/*:ref: tred2_ 14 6 4 4 6 6 6 6 */
/*:ref: tql2_ 14 6 4 4 6 6 6 4 */
/*:ref: rebak_ 14 6 4 4 6 6 4 6 */
extern int stor1_(real *u, real *yh, real *v, real *yp, integer *ntemp, integer *ndisk, integer *ntape);
/* comlen ml8sz_ 28 */
extern C_f catan_(complex * ret_val, complex *z__);
/*:ref: r1mach_ 6 1 4 */
/*:ref: xerror_ 14 5 13 4 4 4 124 */
extern int crotg_(complex *ca, complex *cb, real *c__, complex *s);
extern int ddsclb_(doublereal *hmax, integer *n, integer *nq, doublereal *rmax, doublereal *h__, doublereal *rc, doublereal *rh, doublereal *yh);
extern int dprwpg_(integer *key, integer *ipage, integer *lpg, doublereal *sx, integer *ix);
/*:ref: xerror_ 14 5 13 4 4 4 124 */
/*:ref: dprwvr_ 14 5 4 4 4 7 4 */
extern E_f gami_(real *a, real *x);
/*:ref: xerror_ 14 5 13 4 4 4 124 */
/*:ref: alngam_ 6 1 6 */
/*:ref: gamit_ 6 2 6 6 */
extern int pchngs_(integer *ii, real *xval, integer *iplace, real *sx, integer *ix, integer *ircx);
/*:ref: xerror_ 14 5 13 4 4 4 124 */
/*:ref: iploc_ 4 3 4 6 4 */
/*:ref: prwpge_ 14 5 4 4 4 6 4 */
extern int rsgba_(integer *nm, integer *n, real *a, real *b, real *w, integer *matz, real *z__, real *fv1, real *fv2, integer *ierr);
/*:ref: reduc2_ 14 6 4 4 6 6 6 4 */
/*:ref: tred1_ 14 6 4 4 6 6 6 6 */
/*:ref: tqlrat_ 14 4 4 6 6 4 */
/*:ref: tred2_ 14 6 4 4 6 6 6 6 */
/*:ref: tql2_ 14 6 4 4 6 6 6 4 */
/*:ref: rebakb_ 14 6 4 4 6 6 4 6 */
extern int strco_(real *t, integer *ldt, integer *n, real *rcond, real *z__, integer *job);
/*:ref: sasum_ 6 3 4 6 4 */
/*:ref: sscal_ 14 4 4 6 6 4 */
/*:ref: saxpy_ 14 6 4 6 6 4 6 4 */
extern C_f catan2_(complex * ret_val, complex *csn, complex *ccs);
/*:ref: catan_ 8 2 8 8 */
/*:ref: xerror_ 14 5 13 4 4 4 124 */
extern int cscal_(integer *n, complex *ca, complex *cx, integer *incx);
extern int ddstpb_(doublereal *eps, S_fp f, U_fp fa, doublereal *hmax, integer *impl, U_fp jacobn, integer *matdim, integer *maxord, integer *mint, integer *miter, integer *ml, integer *mu, integer *n, integer *nde, doublereal *ywt, doublereal *uround, doublereal *avgh, doublereal *avgord, doublereal *h__, doublereal *hused, integer *jtask, integer *mntold, integer *mtrold, integer *nfe, integer *nje, integer *nqused, integer *nstep, doublereal *t, doublereal *y, doublereal *yh, doublereal *a, logical *convrg, doublereal *dfdy, doublereal *el, doublereal *hold, integer *ipvt, integer *jstate, integer *nq, integer *nwait, doublereal *rc, doublereal *rmax, doublereal *save1, doublereal *save2, doublereal *tq, doublereal *trend, integer *iswflg, integer *mtrsv, integer *mxrdsv);
/*:ref: ddntlb_ 14 38 7 214 200 7 7 4 4 4 4 4 4 4 4 4 4 7 7 7 7 7 4 4 4 7 7 7 12 7 12 4 4 4 7 7 7 7 7 4 */
/*:ref: ddpscb_ 14 4 4 4 4 7 */
/*:ref: ddpstb_ 14 28 7 214 200 7 4 200 4 4 4 4 4 4 4 7 7 7 7 7 7 4 4 7 7 12 4 7 4 7 */
/*:ref: ddcorb_ 14 22 7 7 200 7 4 4 4 4 4 4 4 4 4 7 7 7 7 12 7 7 7 7 */
/*:ref: dnrm2_ 7 3 4 7 4 */
/*:ref: ddsclb_ 14 8 7 4 4 7 7 7 7 7 */
/*:ref: ddcstb_ 14 5 4 4 4 7 7 */
extern int dprwvr_(integer *key, integer *ipage, integer *lpg, doublereal *sx, integer *ix);
/*:ref: sopenm_ 14 2 4 4 */
/*:ref: dreadp_ 14 5 4 4 7 4 4 */
/*:ref: dwritp_ 14 5 4 4 7 4 4 */
extern E_f gamic_(real *a, real *x);
/*:ref: r1mach_ 6 1 4 */
/*:ref: xerror_ 14 5 13 4 4 4 124 */
/*:ref: alngam_ 6 1 6 */
/*:ref: r9gmic_ 6 3 6 6 6 */
/*:ref: algams_ 14 3 6 6 6 */
/*:ref: r9gmit_ 6 5 6 6 6 6 6 */
/*:ref: r9lgic_ 6 3 6 6 6 */
/*:ref: r9lgit_ 6 3 6 6 6 */
/*:ref: xerclr_ 14 0 */
extern int pchsp_(integer *ic, real *vc, integer *n, real *x, real *f, real *d__, integer *incfd, real *wk, integer *nwk, integer *ierr);
/*:ref: pchdf_ 6 4 4 6 6 4 */
/*:ref: xerror_ 14 5 13 4 4 4 124 */
extern int rsp_(integer *nm, integer *n, integer *nv, real *a, real *w, integer *matz, real *z__, real *fv1, real *fv2, integer *ierr);
/*:ref: tred3_ 14 6 4 4 6 6 6 6 */
/*:ref: tqlrat_ 14 4 4 6 6 4 */
/*:ref: tql2_ 14 6 4 4 6 6 6 4 */
/*:ref: trbak3_ 14 6 4 4 4 6 4 6 */
extern int strdi_(real *t, integer *ldt, integer *n, real *det, integer *job, integer *info);
/*:ref: sscal_ 14 4 4 6 6 4 */
/*:ref: saxpy_ 14 6 4 6 6 4 6 4 */
extern C_f catanh_(complex * ret_val, complex *z__);
/*:ref: catan_ 8 2 8 8 */
extern int cscale_(real *a, integer *nrda, integer *nrow, integer *ncol, real *cols, real *colsav, real *rows, real *rowsav, real *anorm, real *scales, integer *iscale, integer *ic);
/*:ref: sdot_ 6 5 4 6 4 6 4 */
extern int ddzrob_(doublereal *ae, D_fp f, doublereal *h__, integer *n, integer *nq, integer *iroot, doublereal *re, doublereal *t, doublereal *yh, doublereal *uround, doublereal *b, doublereal *c__, doublereal *fb, doublereal *fc, doublereal *y);
/*:ref: ddntpb_ 14 8 7 4 4 4 7 7 7 7 */
extern doublereal dpsi_(doublereal *x);
/*:ref: initds_ 4 3 7 4 6 */
/*:ref: d1mach_ 7 1 4 */
/*:ref: dcsevl_ 7 3 7 7 4 */
/*:ref: xerror_ 14 5 13 4 4 4 124 */
/*:ref: dcot_ 7 1 7 */
extern E_f gamit_(real *a, real *x);
/*:ref: r1mach_ 6 1 4 */
/*:ref: xerror_ 14 5 13 4 4 4 124 */
/*:ref: gamr_ 6 1 6 */
/*:ref: algams_ 14 3 6 6 6 */
/*:ref: r9gmit_ 6 5 6 6 6 6 6 */
/*:ref: r9lgit_ 6 3 6 6 6 */
/*:ref: alngam_ 6 1 6 */
/*:ref: xerclr_ 14 0 */
/*:ref: r9lgic_ 6 3 6 6 6 */
extern E_f pchst_(real *arg1, real *arg2);
extern int rst_(integer *nm, integer *n, real *w, real *e, integer *matz, real *z__, integer *ierr);
/*:ref: imtql1_ 14 4 4 6 6 4 */
/*:ref: imtql2_ 14 6 4 4 6 6 6 4 */
extern int strsl_(real *t, integer *ldt, integer *n, real *b, integer *job, integer *info);
/*:ref: saxpy_ 14 6 4 6 6 4 6 4 */
/*:ref: sdot_ 6 5 4 6 4 6 4 */
extern int caxpy_(integer *n, complex *ca, complex *cx, integer *incx, complex *cy, integer *incy);
extern E_f csevl_(real *x, real *cs, integer *n);
/*:ref: xerror_ 14 5 13 4 4 4 124 */
extern doublereal de1_(doublereal *x);
/*:ref: d1mach_ 7 1 4 */
/*:ref: initds_ 4 3 7 4 6 */
/*:ref: dcsevl_ 7 3 7 7 4 */
/*:ref: xerror_ 14 5 13 4 4 4 124 */
extern int dpsifn_(doublereal *x, integer *n, integer *kode, integer *m, doublereal *ans, integer *nz, integer *ierr);
/*:ref: i1mach_ 4 1 4 */
/*:ref: d1mach_ 7 1 4 */
extern int gamlim_(real *xmin, real *xmax);
/*:ref: r1mach_ 6 1 4 */
/*:ref: xerror_ 14 5 13 4 4 4 124 */
extern int pchsw_(real *dfmax, integer *iextrm, real *d1, real *d2, real *h__, real *slope, integer *ierr);
/*:ref: r1mach_ 6 1 4 */
/*:ref: xerror_ 14 5 13 4 4 4 124 */
extern int rt_(integer *nm, integer *n, real *a, real *w, integer *matz, real *z__, real *fv1, integer *ierr);
/*:ref: figi_ 14 7 4 4 6 6 6 6 4 */
/*:ref: imtql1_ 14 4 4 6 6 4 */
/*:ref: figi2_ 14 7 4 4 6 6 6 6 4 */
/*:ref: imtql2_ 14 6 4 4 6 6 6 4 */
extern int stway_(real *u, real *v, real *yhp, integer *inout, real *stowa);
/* comlen ml8sz_ 28 */
/* comlen ml15to_ 116 */
/* comlen ml18jr_ 72 */
/*:ref: stor1_ 14 7 6 6 6 6 4 4 4 */
extern int cbabk2_(integer *nm, integer *n, integer *low, integer *igh, real *scale, integer *m, real *zr, real *zi);
extern int csico_(complex *a, integer *lda, integer *n, integer *kpvt, real *rcond, complex *z__);
/*:ref: scasum_ 6 3 4 8 4 */
/*:ref: csifa_ 14 5 8 4 4 4 4 */
/*:ref: caxpy_ 14 6 4 8 8 4 8 4 */
/*:ref: csscal_ 14 4 4 6 8 4 */
/*:ref: cdotu_ 8 6 8 4 8 4 8 4 */
extern int de2_(S_fp f, integer *neq, real *t, real *y, real *tout, integer *info, real *rtol, real *atol, integer *idid, real *ypout, real *yp, real *yy, real *wt, real *p, real *phi, real *alpha, real *beta, real *psi, real *v, real *w, real *sig, real *g, real *h__, real *eps, real *x, real *hold, real *told, real *delsgn, real *tstop, real *twou, real *fouru, logical *start, logical *phase1, logical *nornd, logical *stiff, logical *intout, integer *ns, integer *kord, integer *kold, integer *init, integer *ksteps, integer *kle4, integer *iquit, real *rpar, integer *ipar);
/*:ref: r1mach_ 6 1 4 */
/*:ref: xerrwv_ 14 11 13 4 4 4 4 4 4 4 6 6 124 */
/*:ref: intrp_ 14 9 6 6 6 6 6 4 4 6 6 */
/*:ref: step2_ 14 30 214 4 6 6 6 6 6 12 6 4 4 12 6 6 6 6 6 6 6 6 6 6 12 4 12 4 6 6 6 4 */
extern doublereal dpsixn_(integer *n);
/*:ref: d1mach_ 7 1 4 */
extern E_f gamma_(real *x);
/*:ref: inits_ 4 3 6 4 6 */
/*:ref: r1mach_ 6 1 4 */
/*:ref: gamlim_ 14 2 6 6 */
/*:ref: csevl_ 6 3 6 6 4 */
/*:ref: xerror_ 14 5 13 4 4 4 124 */
/*:ref: r9lgmc_ 6 1 6 */
extern int pcoef_(integer *l, real *c__, real *tc, real *a);
/*:ref: pvalue_ 14 6 4 4 6 6 6 6 */
extern E_f runif_(real *t, integer *n);
/*:ref: rand_ 6 1 6 */
extern int suds_(real *a, real *x, real *b, integer *neq, integer *nuk, integer *nrda, integer *iflag, integer *mlso, real *work, integer *iwork);
/*:ref: lssuds_ 14 20 6 6 6 4 4 4 6 4 4 4 4 4 6 6 4 6 6 6 4 6 */
extern int cbal_(integer *nm, integer *n, real *ar, real *ai, integer *low, integer *igh, real *scale);
extern int csidi_(complex *a, integer *lda, integer *n, integer *kpvt, complex *det, complex *work, integer *job);
/*:ref: ccopy_ 14 5 4 8 4 8 4 */
/*:ref: cdotu_ 8 6 8 4 8 4 8 4 */
/*:ref: caxpy_ 14 6 4 8 8 4 8 4 */
/*:ref: cswap_ 14 5 4 8 4 8 4 */
extern int deabm_(U_fp f, integer *neq, real *t, real *y, real *tout, integer *info, real *rtol, real *atol, integer *idid, real *rwork, integer *lrw, integer *iwork, integer *liw, real *rpar, integer *ipar);
/*:ref: xerrwv_ 14 11 13 4 4 4 4 4 4 4 6 6 124 */
/*:ref: des_ 14 51 200 4 6 6 6 4 6 6 4 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 12 12 12 12 12 4 4 4 4 4 4 4 4 4 4 4 6 4 */
extern int dptsl_(integer *n, doublereal *d__, doublereal *e, doublereal *b);
extern E_f gamr_(real *x);
/*:ref: xgetf_ 14 1 4 */
/*:ref: xsetf_ 14 1 4 */
/*:ref: gamma_ 6 1 6 */
/*:ref: xerclr_ 14 0 */
/*:ref: algams_ 14 3 6 6 6 */
extern int pfqad_(E_fp f, integer *ldc, real *c__, real *xi, integer *lxi, integer *k, integer *id, real *x1, real *x2, real *tol, real *quad, integer *ierr);
/*:ref: r1mach_ 6 1 4 */
/*:ref: intrv_ 14 6 6 4 6 4 4 4 */
/*:ref: ppgq8_ 14 13 206 4 6 6 4 4 4 6 6 4 6 6 4 */
/*:ref: xerror_ 14 5 13 4 4 4 124 */
extern int rwupdt_(integer *n, real *r__, integer *ldr, real *w, real *b, real *alpha, real *cos__, real *sin__);
extern int svco_(real *rsav, integer *isav);
/* comlen debdf1_ 1004 */
extern C_f cbeta_(complex * ret_val, complex *a, complex *b);
/*:ref: gamlim_ 14 2 6 6 */
/*:ref: xerror_ 14 5 13 4 4 4 124 */
/*:ref: cgamma_ 8 2 8 8 */
/*:ref: clbeta_ 8 3 8 8 8 */
extern int csifa_(complex *a, integer *lda, integer *n, integer *kpvt, integer *info);
/*:ref: icamax_ 4 3 4 8 4 */
/*:ref: cswap_ 14 5 4 8 4 8 4 */
/*:ref: caxpy_ 14 6 4 8 8 4 8 4 */
extern int debdf_(U_fp f, integer *neq, real *t, real *y, real *tout, integer *info, real *rtol, real *atol, integer *idid, real *rwork, integer *lrw, integer *iwork, integer *liw, real *rpar, integer *ipar, U_fp jac);
/* comlen debdf1_ 1004 */
/*:ref: xerrwv_ 14 11 13 4 4 4 4 4 4 4 6 6 124 */
/*:ref: lsod_ 14 23 200 4 6 6 6 6 6 4 6 6 6 6 6 6 6 4 200 12 6 6 6 6 4 */
extern int dqag_(D_fp f, doublereal *a, doublereal *b, doublereal *epsabs, doublereal *epsrel, integer *key, doublereal *result, doublereal *abserr, integer *neval, integer *ier, integer *limit, integer *lenw, integer *last, integer *iwork, doublereal *work);
/*:ref: dqage_ 14 17 207 7 7 7 7 4 4 7 7 4 4 7 7 7 7 4 4 */
/*:ref: xerror_ 14 5 13 4 4 4 124 */
extern E_f gamrn_(real *x);
/*:ref: r1mach_ 6 1 4 */
/*:ref: i1mach_ 4 1 4 */
extern E_f pgsf_(real *x, integer *iz, real *c__, real *a, real *bh);
extern int s88fmt_(integer *n, integer *ivalue, integer *ifmt);
extern int svd_(integer *nm, integer *m, integer *n, real *a, real *w, logical *matu, real *u, logical *matv, real *v, integer *ierr, real *rv1);
/*:ref: pythag_ 6 2 6 6 */
extern int cblkt1_(integer *n, real *an, real *bn, real *cn, integer *m, complex *am, complex *bm, complex *cm, integer *idimy, complex *y, real *b, complex *w1, complex *w2, complex *w3, complex *wd, complex *ww, complex *wu, S_fp prdct, S_fp cprdct);
/* comlen ccblk_ 28 */
/*:ref: inxcb_ 14 4 4 4 4 4 */
/*:ref: inxcc_ 14 4 4 4 4 4 */
/*:ref: inxca_ 14 4 4 4 4 4 */
extern C_f csinh_(complex * ret_val, complex *z__);
extern int defc_(integer *ndata, doublereal *xdata, doublereal *ydata, doublereal *sddata, integer *nord, integer *nbkpt, doublereal *bkpt, integer *mdein, integer *mdeout, doublereal *coeff, integer *lw, doublereal *w);
/*:ref: defcmn_ 14 19 4 7 7 7 4 4 7 4 4 7 7 7 7 7 7 4 7 4 4 */
extern int dqage_(D_fp f, doublereal *a, doublereal *b, doublereal *epsabs, doublereal *epsrel, integer *key, integer *limit, doublereal *result, doublereal *abserr, integer *neval, integer *ier, doublereal *alist__, doublereal *blist, doublereal *rlist, doublereal *elist, integer *iord, integer *last);
/*:ref: d1mach_ 7 1 4 */
/*:ref: dqk15_ 14 7 207 7 7 7 7 7 7 */
/*:ref: dqk21_ 14 7 207 7 7 7 7 7 7 */
/*:ref: dqk31_ 14 7 207 7 7 7 7 7 7 */
/*:ref: dqk41_ 14 7 207 7 7 7 7 7 7 */
/*:ref: dqk51_ 14 7 207 7 7 7 7 7 7 */
/*:ref: dqk61_ 14 7 207 7 7 7 7 7 7 */
/*:ref: dqpsrt_ 14 7 4 4 4 7 7 4 4 */
extern int gaus8_(E_fp fun, real *a, real *b, real *err, real *ans, integer *ierr);
/*:ref: xerror_ 14 5 13 4 4 4 124 */
/*:ref: i1mach_ 4 1 4 */
/*:ref: r1mach_ 6 1 4 */
extern doublereal phi_(doublereal *x, doublereal *phiprm, real *a, real *b);
extern E_f sasum_(integer *n, real *sx, integer *incx);
extern int svecs_(integer *ncomp, integer *lnfc, real *yhp, real *work, integer *iwork, integer *inhomo, integer *iflag);
/* comlen ml18jr_ 72 */
/*:ref: mgsbv_ 14 13 4 4 6 4 4 4 6 6 4 4 6 6 6 */
extern int cblktr_(integer *iflg, integer *np, integer *n, real *an, real *bn, real *cn, integer *mp, integer *m, complex *am, complex *bm, complex *cm, integer *idimy, complex *y, integer *ierror, real *w);
/* comlen ccblk_ 28 */
/*:ref: ccmpb_ 14 8 4 4 6 6 6 6 6 6 */
/*:ref: cblkt1_ 14 19 4 6 6 6 4 8 8 8 4 8 6 6 6 6 6 6 6 200 200 */
extern int csisl_(complex *a, integer *lda, integer *n, integer *kpvt, complex *b);
/*:ref: caxpy_ 14 6 4 8 8 4 8 4 */
/*:ref: cdotu_ 8 6 8 4 8 4 8 4 */
extern int defcmn_(integer *ndata, doublereal *xdata, doublereal *ydata, doublereal *sddata, integer *nord, integer *nbkpt, doublereal *bkptin, integer *mdein, integer *mdeout, doublereal *coeff, doublereal *bf, doublereal *xtemp, doublereal *ptemp, doublereal *bkpt, doublereal *g, integer *mdg, doublereal *w, integer *mdw, integer *lw);
/*:ref: dcopy_ 14 5 4 7 4 7 4 */
/*:ref: xerror_ 14 5 13 4 4 4 124 */
/*:ref: xerrwv_ 14 11 13 4 4 4 4 4 4 4 7 7 124 */
/*:ref: dsort_ 14 4 7 7 4 4 */
/*:ref: dbndac_ 14 7 7 4 4 4 4 4 4 */
/*:ref: dfspvn_ 14 6 7 4 4 7 4 7 */
/*:ref: dscal_ 14 4 4 7 7 4 */
/*:ref: dbndsl_ 14 9 4 7 4 4 4 4 7 4 7 */
extern int dqagi_(D_fp f, doublereal *bound, integer *inf, doublereal *epsabs, doublereal *epsrel, doublereal *result, doublereal *abserr, integer *neval, integer *ier, integer *limit, integer *lenw, integer *last, integer *iwork, doublereal *work);
/*:ref: dqagie_ 14 16 207 7 4 7 7 4 7 7 4 4 7 7 7 7 4 4 */
/*:ref: xerror_ 14 5 13 4 4 4 124 */
extern E_f gauss_(integer *n, E_fp y, integer *m, S_fp ltbl);
/*:ref: xerror_ 14 5 13 4 4 4 124 */
extern E_f pimach_(real *dum);
extern int saxpy_(integer *n, real *sa, real *sx, integer *incx, real *sy, integer *incy);
extern int svout_(integer *n, real *sx, char *ifmt, integer *idigit, ftnlen ifmt_len);
/*:ref: i1mach_ 4 1 4 */
extern E_f cbrt_(real *x);
/*:ref: r1mach_ 6 1 4 */
/*:ref: r9upak_ 14 3 6 6 4 */
/*:ref: r9pak_ 6 2 6 4 */
extern int cspco_(complex *ap, integer *n, integer *kpvt, real *rcond, complex *z__);
/*:ref: scasum_ 6 3 4 8 4 */
/*:ref: cspfa_ 14 4 8 4 4 4 */
/*:ref: caxpy_ 14 6 4 8 8 4 8 4 */
/*:ref: csscal_ 14 4 4 6 8 4 */
/*:ref: cdotu_ 8 6 8 4 8 4 8 4 */
extern int defe4_(S_fp cofx, integer *idmn, real *usol, real *grhs);
/* comlen spl4_ 80 */
/*:ref: dx4_ 14 6 6 4 4 4 6 6 */
/*:ref: dy4_ 14 6 6 4 4 4 6 6 */
extern int dqagie_(D_fp f, doublereal *bound, integer *inf, doublereal *epsabs, doublereal *epsrel, integer *limit, doublereal *result, doublereal *abserr, integer *neval, integer *ier, doublereal *alist__, doublereal *blist, doublereal *rlist, doublereal *elist, integer *iord, integer *last);
/*:ref: d1mach_ 7 1 4 */
/*:ref: dqk15i_ 14 9 207 7 4 7 7 7 7 7 7 */
/*:ref: dqpsrt_ 14 7 4 4 4 7 7 4 4 */
/*:ref: dqelg_ 14 6 4 7 7 7 7 4 */
extern int gaussq_(integer *m, doublereal *t, doublereal *w, doublereal *b, doublereal *g, doublereal *fm);
/*:ref: d1mach_ 6 1 4 */
extern int pinitm_(integer *m, integer *n, real *sx, integer *ix, integer *lmx, integer *ipagef);
/*:ref: xerror_ 14 5 13 4 4 4 124 */
extern int sbocls_(real *w, integer *mdw, integer *mcon, integer *mrows, integer *ncols, real *bl, real *bu, integer *ind, integer *iopt, real *x, real *rnormc, real *rnorm, integer *mode, real *rw, integer *iw);
/*:ref: r1mach_ 6 1 4 */
/*:ref: scopy_ 14 5 4 6 4 6 4 */
/*:ref: sbols_ 14 13 6 4 4 4 6 6 4 4 6 6 4 6 4 */
/*:ref: sdot_ 6 5 4 6 4 6 4 */
/*:ref: sasum_ 6 3 4 6 4 */
/*:ref: snrm2_ 6 3 4 6 4 */
/*:ref: sscal_ 14 4 4 6 6 4 */
extern int swritp_(integer *ipage, integer *list, real *rlist, integer *lpage, integer *irec);
/*:ref: xerrwv_ 14 11 13 4 4 4 4 4 4 4 6 6 124 */
extern int cbshv_(integer *m, integer *k, real *aa, real *z__);
extern int cspdi_(complex *ap, integer *n, integer *kpvt, complex *det, complex *work, integer *job);
/*:ref: ccopy_ 14 5 4 8 4 8 4 */
/*:ref: cdotu_ 8 6 8 4 8 4 8 4 */
/*:ref: caxpy_ 14 6 4 8 8 4 8 4 */
/*:ref: cswap_ 14 5 4 8 4 8 4 */
extern int defehl_(S_fp f, integer *neq, real *t, real *y, real *h__, real *yp, real *f1, real *f2, real *f3, real *f4, real *f5, real *ys, real *rpar, integer *ipar);
extern int dqagp_(D_fp f, doublereal *a, doublereal *b, integer *npts2, doublereal *points, doublereal *epsabs, doublereal *epsrel, doublereal *result, doublereal *abserr, integer *neval, integer *ier, integer *leniw, integer *lenw, integer *last, integer *iwork, doublereal *work);
/*:ref: dqagpe_ 14 21 207 7 7 4 7 7 7 4 7 7 4 4 7 7 7 7 7 4 4 4 4 */
/*:ref: xerror_ 14 5 13 4 4 4 124 */
extern int genbun_(integer *nperod, integer *n, integer *mperod, integer *m, real *a, real *b, real *c__, integer *idimy, real *y, integer *ierror, real *w);
/*:ref: poisp2_ 14 16 4 4 6 6 6 6 4 6 6 6 6 6 6 6 6 6 */
/*:ref: poisd2_ 14 13 4 4 4 6 6 6 6 4 6 6 6 6 6 */
/*:ref: poisn2_ 14 18 4 4 4 4 6 6 6 6 4 6 6 6 6 6 6 6 6 6 */
extern int pjac_(integer *neq, real *y, real *yh, integer *nyh, real *ewt, real *ftem, real *savf, real *wm, integer *iwm, S_fp f, S_fp jac, real *rpar, integer *ipar);
/* comlen debdf1_ 1004 */
/*:ref: vnwrms_ 6 3 4 6 6 */
/*:ref: sgefa_ 14 5 6 4 4 4 4 */
/*:ref: sgbfa_ 14 7 6 4 4 4 4 4 4 */
extern int sbols_(real *w, integer *mdw, integer *mrows, integer *ncols, real *bl, real *bu, integer *ind, integer *iopt, real *x, real *rnorm, integer *mode, real *rw, integer *iw);
/*:ref: scopy_ 14 5 4 6 4 6 4 */
/*:ref: isamax_ 4 3 4 6 4 */
/*:ref: srotg_ 14 4 6 6 6 6 */
/*:ref: srot_ 14 7 4 6 4 6 4 6 6 */
/*:ref: snrm2_ 6 3 4 6 4 */
/*:ref: sbolsm_ 14 16 6 4 4 4 6 6 4 4 6 6 4 6 6 6 4 4 */
extern int tevlc_(integer *n, real *d__, real *e2, integer *ierr);
/* comlen ccblk_ 28 */
extern C_f ccbrt_(complex * ret_val, complex *z__);
/*:ref: carg_ 6 1 8 */
/*:ref: cbrt_ 6 1 6 */
extern int cspfa_(complex *ap, integer *n, integer *kpvt, integer *info);
/*:ref: icamax_ 4 3 4 8 4 */
/*:ref: cswap_ 14 5 4 8 4 8 4 */
/*:ref: caxpy_ 14 6 4 8 8 4 8 4 */
extern int defer_(S_fp cofx, S_fp cofy, integer *idmn, real *usol, real *grhs);
/* comlen splpzz_ 80 */
/*:ref: dx_ 14 6 6 4 4 4 6 6 */
/*:ref: dy_ 14 6 6 4 4 4 6 6 */
extern int dqagpe_(D_fp f, doublereal *a, doublereal *b, integer *npts2, doublereal *points, doublereal *epsabs, doublereal *epsrel, integer *limit, doublereal *result, doublereal *abserr, integer *neval, integer *ier, doublereal *alist__, doublereal *blist, doublereal *rlist, doublereal *elist, doublereal *pts, integer *iord, integer *level, integer *ndin, integer *last);
/*:ref: d1mach_ 7 1 4 */
/*:ref: dqk21_ 14 7 207 7 7 7 7 7 7 */
/*:ref: dqpsrt_ 14 7 4 4 4 7 7 4 4 */
/*:ref: dqelg_ 14 6 4 7 7 7 7 4 */
extern int gengsq_(integer *m, doublereal *b, doublereal *g, doublereal *a, doublereal *w, doublereal *wf, integer *js, real *el, real *er, doublereal *fm, integer *jorth, real *eps, integer *iswtch, integer *nit);
/*:ref: stltjs_ 14 10 4 4 7 7 7 4 7 207 6 6 */
/*:ref: phi_ 7 :*/
/*:ref: gaussq_ 14 6 4 7 7 7 7 7 */
extern int pnnzrs_(integer *i__, real *xval, integer *iplace, real *sx, integer *ix, integer *ircx);
/*:ref: xerror_ 14 5 13 4 4 4 124 */
/*:ref: iploc_ 4 3 4 6 4 */
extern int sbolsm_(real *w, integer *mdw, integer *minput, integer *ncols, real *bl, real *bu, integer *ind, integer *iopt, real *x, real *rnorm, integer *mode, real *rw, real *ww, real *scl, integer *ibasis, integer *ibb);
/*:ref: xerrwv_ 14 11 13 4 4 4 4 4 4 4 6 6 124 */
/*:ref: smout_ 14 7 4 4 4 6 13 4 124 */
/*:ref: svout_ 14 5 4 6 13 4 124 */
/*:ref: scopy_ 14 5 4 6 4 6 4 */
/*:ref: snrm2_ 6 3 4 6 4 */
/*:ref: sdot_ 6 5 4 6 4 6 4 */
/*:ref: ivout_ 14 5 4 4 13 4 124 */
/*:ref: srotg_ 14 4 6 6 6 6 */
/*:ref: sswap_ 14 5 4 6 4 6 4 */
/*:ref: srot_ 14 7 4 6 4 6 4 6 6 */
/*:ref: saxpy_ 14 6 4 6 6 4 6 4 */
/*:ref: r1mach_ 6 1 4 */
extern int tevls_(integer *n, real *d__, real *e2, integer *ierr);
/* comlen cblkt_ 28 */
extern int cchdc_(complex *a, integer *lda, integer *p, complex *work, integer *jpvt, integer *job, integer *info);
/*:ref: cswap_ 14 5 4 8 4 8 4 */
/*:ref: caxpy_ 14 6 4 8 8 4 8 4 */
extern int cspsl_(complex *ap, integer *n, integer *kpvt, complex *b);
/*:ref: caxpy_ 14 6 4 8 8 4 8 4 */
/*:ref: cdotu_ 8 6 8 4 8 4 8 4 */
extern doublereal dei_(doublereal *x);
/*:ref: de1_ 7 1 7 */
extern int dqags_(D_fp f, doublereal *a, doublereal *b, doublereal *epsabs, doublereal *epsrel, doublereal *result, doublereal *abserr, integer *neval, integer *ier, integer *limit, integer *lenw, integer *last, integer *iwork, doublereal *work);
/*:ref: dqagse_ 14 16 207 7 7 7 7 4 7 7 4 4 7 7 7 7 4 4 */
/*:ref: xerror_ 14 5 13 4 4 4 124 */
extern int h12_(integer *mode, integer *lpivot, integer *l1, integer *m, real *u, integer *iue, real *up, real *c__, integer *ice, integer *icv, integer *ncv);
/*:ref: sswap_ 14 5 4 6 4 6 4 */
/*:ref: sdot_ 6 5 4 6 4 6 4 */
/*:ref: saxpy_ 14 6 4 6 6 4 6 4 */
extern E_f poch_(real *a, real *x);
/*:ref: xerror_ 14 5 13 4 4 4 124 */
/*:ref: fac_ 6 1 4 */
/*:ref: alnrel_ 6 1 6 */
/*:ref: r9lgmc_ 6 1 6 */
/*:ref: gamma_ 6 1 6 */
/*:ref: gamr_ 6 1 6 */
/*:ref: cot_ 6 1 6 */
/*:ref: algams_ 14 3 6 6 6 */
extern E_f scasum_(integer *n, complex *cx, integer *incx);
extern int tinvit_(integer *nm, integer *n, real *d__, real *e, real *e2, integer *m, real *w, integer *ind, real *z__, integer *ierr, real *rv1, real *rv2, real *rv3, real *rv4, real *rv6);
extern int cchdd_(complex *r__, integer *ldr, integer *p, complex *x, complex *z__, integer *ldz, integer *nz, complex *y, real *rho, real *c__, complex *s, integer *info);
/*:ref: cdotc_ 8 6 8 4 8 4 8 4 */
/*:ref: scnrm2_ 6 3 4 8 4 */
extern int csroot_(real *xr, real *xi, real *yr, real *yi);
/*:ref: pythag_ 6 2 6 6 */
extern doublereal denorm_(integer *n, doublereal *x);
extern int dqagse_(D_fp f, doublereal *a, doublereal *b, doublereal *epsabs, doublereal *epsrel, integer *limit, doublereal *result, doublereal *abserr, integer *neval, integer *ier, doublereal *alist__, doublereal *blist, doublereal *rlist, doublereal *elist, integer *iord, integer *last);
/*:ref: d1mach_ 7 1 4 */
/*:ref: dqk21_ 14 7 207 7 7 7 7 7 7 */
/*:ref: dqpsrt_ 14 7 4 4 4 7 7 4 4 */
/*:ref: dqelg_ 14 6 4 7 7 7 7 4 */
extern int hfti_(real *a, integer *mda, integer *m, integer *n, real *b, integer *mdb, integer *nb, real *tau, integer *krank, real *rnorm, real *h__, real *g, integer *ip);
/*:ref: xerror_ 14 5 13 4 4 4 124 */
/*:ref: diff_ 6 2 6 6 */
/*:ref: h12_ 14 11 4 4 4 4 6 4 6 6 4 4 4 */
extern E_f poch1_(real *a, real *x);
/*:ref: r1mach_ 6 1 4 */
/*:ref: psi_ 6 1 6 */
/*:ref: xerror_ 14 5 13 4 4 4 124 */
/*:ref: exprel_ 6 1 6 */
/*:ref: cot_ 6 1 6 */
/*:ref: poch_ 6 2 6 6 */
extern int schdc_(real *a, integer *lda, integer *p, real *work, integer *jpvt, integer *job, integer *info);
/*:ref: sswap_ 14 5 4 6 4 6 4 */
/*:ref: saxpy_ 14 6 4 6 6 4 6 4 */
extern int tql1_(integer *n, real *d__, real *e, integer *ierr);
/*:ref: pythag_ 6 2 6 6 */
extern int cchex_(complex *r__, integer *ldr, integer *p, integer *k, integer *l, complex *z__, integer *ldz, integer *nz, real *c__, complex *s, integer *job);
/*:ref: crotg_ 14 4 8 8 6 8 */
extern int csrot_(integer *n, complex *cx, integer *incx, complex *cy, integer *incy, real *c__, real *s);
extern doublereal derf_(doublereal *x);
/*:ref: initds_ 4 3 7 4 6 */
/*:ref: d1mach_ 7 1 4 */
/*:ref: dcsevl_ 7 3 7 7 4 */
/*:ref: derfc_ 7 1 7 */
extern int dqawc_(D_fp f, doublereal *a, doublereal *b, doublereal *c__, doublereal *epsabs, doublereal *epsrel, doublereal *result, doublereal *abserr, integer *neval, integer *ier, integer *limit, integer *lenw, integer *last, integer *iwork, doublereal *work);
/*:ref: dqawce_ 14 17 207 7 7 7 7 7 4 7 7 4 4 7 7 7 7 4 4 */
/*:ref: xerror_ 14 5 13 4 4 4 124 */
extern int hkseq_(real *x, integer *m, real *h__, integer *ierr);
/*:ref: r1mach_ 6 1 4 */
/*:ref: i1mach_ 4 1 4 */
extern int pois3d_(integer *lperod, integer *l, real *c1, integer *mperod, integer *m, real *c2, integer *nperod, integer *n, real *a, real *b, real *c__, integer *ldimf, integer *mdimf, real *f, integer *ierror, real *w);
/*:ref: pos3d1_ 14 20 4 4 4 4 4 6 6 6 4 4 6 6 6 6 6 6 6 6 6 6 */
extern int schdd_(real *r__, integer *ldr, integer *p, real *x, real *z__, integer *ldz, integer *nz, real *y, real *rho, real *c__, real *s, integer *info);
/*:ref: sdot_ 6 5 4 6 4 6 4 */
/*:ref: snrm2_ 6 3 4 6 4 */
extern int tql2_(integer *nm, integer *n, real *d__, real *e, real *z__, integer *ierr);
/*:ref: pythag_ 6 2 6 6 */
extern int cchud_(complex *r__, integer *ldr, integer *p, complex *x, complex *z__, integer *ldz, integer *nz, complex *y, real *rho, real *c__, complex *s);
/*:ref: crotg_ 14 4 8 8 6 8 */
extern int csscal_(integer *n, real *sa, complex *cx, integer *incx);
extern doublereal derfc_(doublereal *x);
/*:ref: d1mach_ 7 1 4 */
/*:ref: initds_ 4 3 7 4 6 */
/*:ref: dcsevl_ 7 3 7 7 4 */
/*:ref: xerror_ 14 5 13 4 4 4 124 */
extern int dqawce_(D_fp f, doublereal *a, doublereal *b, doublereal *c__, doublereal *epsabs, doublereal *epsrel, integer *limit, doublereal *result, doublereal *abserr, integer *neval, integer *ier, doublereal *alist__, doublereal *blist, doublereal *rlist, doublereal *elist, integer *iord, integer *last);
/*:ref: d1mach_ 7 1 4 */
/*:ref: dqc25c_ 14 8 207 7 7 7 7 7 4 4 */
/*:ref: dqpsrt_ 14 7 4 4 4 7 7 4 4 */
extern int hqr_(integer *nm, integer *n, integer *low, integer *igh, real *h__, real *wr, real *wi, integer *ierr);
extern int poisd2_(integer *mr, integer *nr, integer *istag, real *ba, real *bb, real *bc, real *q, integer *idimq, real *b, real *w, real *d__, real *tcos, real *p);
/*:ref: trix_ 14 10 4 4 4 6 6 6 6 6 6 6 */
/*:ref: cosgen_ 14 5 4 4 6 6 6 */
/*:ref: merge_ 14 6 6 4 4 4 4 4 */
extern int schex_(real *r__, integer *ldr, integer *p, integer *k, integer *l, real *z__, integer *ldz, integer *nz, real *c__, real *s, integer *job);
/*:ref: srotg_ 14 4 6 6 6 6 */
extern int tqlrat_(integer *n, real *d__, real *e2, integer *ierr);
/*:ref: pythag_ 6 2 6 6 */
extern int ccmpb_(integer *n, integer *ierror, real *an, real *bn, real *cn, real *b, real *ah, real *bh);
/* comlen ccblk_ 28 */
/*:ref: epmach_ 6 1 6 */
/*:ref: inxcb_ 14 4 4 4 4 4 */
/*:ref: tevlc_ 14 4 4 6 6 4 */
/*:ref: cpadd_ 14 7 4 4 6 6 6 6 6 */
extern int csvdc_(complex *x, integer *ldx, integer *n, integer *p, complex *s, complex *e, complex *u, integer *ldu, complex *v, integer *ldv, complex *work, integer *job, integer *info);
/*:ref: scnrm2_ 6 3 4 8 4 */
/*:ref: cscal_ 14 4 4 8 8 4 */
/*:ref: cdotc_ 8 6 8 4 8 4 8 4 */
/*:ref: caxpy_ 14 6 4 8 8 4 8 4 */
/*:ref: srotg_ 14 4 6 6 6 6 */
/*:ref: csrot_ 14 7 4 8 4 8 4 6 6 */
/*:ref: cswap_ 14 5 4 8 4 8 4 */
extern int derkf_(U_fp f, integer *neq, real *t, real *y, real *tout, integer *info, real *rtol, real *atol, integer *idid, real *rwork, integer *lrw, integer *iwork, integer *liw, real *rpar, integer *ipar);
/*:ref: xerrwv_ 14 11 13 4 4 4 4 4 4 4 6 6 124 */
/*:ref: derkfs_ 14 32 200 4 6 6 6 4 6 6 4 6 6 6 6 6 6 6 6 6 6 6 6 6 4 4 4 4 12 12 4 4 6 4 */
extern int dqawf_(D_fp f, doublereal *a, doublereal *omega, integer *integr, doublereal *epsabs, doublereal *result, doublereal *abserr, integer *neval, integer *ier, integer *limlst, integer *lst, integer *leniw, integer *maxp1, integer *lenw, integer *iwork, doublereal *work);
/*:ref: dqawfe_ 14 23 207 7 7 4 7 4 4 4 7 7 4 4 7 7 4 4 7 7 7 7 4 4 7 */
/*:ref: xerror_ 14 5 13 4 4 4 124 */
extern int hqr2_(integer *nm, integer *n, integer *low, integer *igh, real *h__, real *wr, real *wi, real *z__, integer *ierr);
/*:ref: cdiv_ 14 6 6 6 6 6 6 6 */
extern int poisn2_(integer *m, integer *n, integer *istag, integer *mixbnd, real *a, real *bb, real *c__, real *q, integer *idimq, real *b, real *b2, real *b3, real *w, real *w2, real *w3, real *d__, real *tcos, real *p);
/*:ref: cosgen_ 14 5 4 4 6 6 6 */
/*:ref: trix_ 14 10 4 4 4 6 6 6 6 6 6 6 */
/*:ref: merge_ 14 6 6 4 4 4 4 4 */
/*:ref: tri3_ 14 13 4 6 6 6 4 6 6 6 6 6 6 6 6 */
extern int schud_(real *r__, integer *ldr, integer *p, real *x, real *z__, integer *ldz, integer *nz, real *y, real *rho, real *c__, real *s);
/*:ref: srotg_ 14 4 6 6 6 6 */
extern int trbak1_(integer *nm, integer *n, real *a, real *e, integer *m, real *z__);
extern int ccopy_(integer *n, complex *cx, integer *incx, complex *cy, integer *incy);
extern int cswap_(integer *n, complex *cx, integer *incx, complex *cy, integer *incy);
extern int derkfs_(S_fp f, integer *neq, real *t, real *y, real *tout, integer *info, real *rtol, real *atol, integer *idid, real *h__, real *tolfac, real *yp, real *f1, real *f2, real *f3, real *f4, real *f5, real *ys, real *told, real *dtsign, real *u26, real *rer, integer *init, integer *ksteps, integer *kop, integer *iquit, logical *stiff, logical *nonstf, integer *ntstep, integer *nstifs, real *rpar, integer *ipar);
/*:ref: r1mach_ 6 1 4 */
/*:ref: xerrwv_ 14 11 13 4 4 4 4 4 4 4 6 6 124 */
/*:ref: vnorm_ 6 2 6 4 */
/*:ref: hstart_ 14 17 214 4 6 6 6 6 6 4 6 6 6 6 6 6 6 4 6 */
/*:ref: defehl_ 14 14 214 4 6 6 6 6 6 6 6 6 6 6 6 4 */
extern int dqawfe_(D_fp f, doublereal *a, doublereal *omega, integer *integr, doublereal *epsabs, integer *limlst, integer *limit, integer *maxp1, doublereal *result, doublereal *abserr, integer *neval, integer *ier, doublereal *rslst, doublereal *erlst, integer *ierlst, integer *lst, doublereal *alist__, doublereal *blist, doublereal *rlist, doublereal *elist, integer *iord, integer *nnlog, doublereal *chebmo);
/*:ref: dqagie_ 14 16 207 7 4 7 7 4 7 7 4 4 7 7 7 7 4 4 */
/*:ref: d1mach_ 7 1 4 */
/*:ref: dqawoe_ 14 23 207 7 7 7 4 7 7 4 4 4 7 7 4 4 4 7 7 7 7 4 4 4 7 */
/*:ref: dqelg_ 14 6 4 7 7 7 7 4 */
extern int hrmte_(integer *m, integer *k, real *aa, real *z__);
extern int poisp2_(integer *m, integer *n, real *a, real *bb, real *c__, real *q, integer *idimq, real *b, real *b2, real *b3, real *w, real *w2, real *w3, real *d__, real *tcos, real *p);
/*:ref: poisd2_ 14 13 4 4 4 6 6 6 6 4 6 6 6 6 6 */
/*:ref: poisn2_ 14 18 4 4 4 4 6 6 6 6 4 6 6 6 6 6 6 6 6 6 */
extern int sclosm_(integer *ipage);
/*:ref: xerrwv_ 14 11 13 4 4 4 4 4 4 4 6 6 124 */
extern int trbak3_(integer *nm, integer *n, integer *nv, real *a, integer *m, real *z__);
extern C_f ccosh_(complex * ret_val, complex *z__);
extern C_f ctan_(complex * ret_val, complex *z__);
/*:ref: r1mach_ 6 1 4 */
/*:ref: xerclr_ 14 0 */
/*:ref: xerror_ 14 5 13 4 4 4 124 */
extern int des_(S_fp f, integer *neq, real *t, real *y, real *tout, integer *info, real *rtol, real *atol, integer *idid, real *ypout, real *yp, real *yy, real *wt, real *p, real *phi, real *alpha, real *beta, real *psi, real *v, real *w, real *sig, real *g, real *gi, real *h__, real *eps, real *x, real *xold, real *hold, real *told, real *delsgn, real *tstop, real *twou, real *fouru, logical *start, logical *phase1, logical *nornd, logical *stiff, logical *intout, integer *ns, integer *kord, integer *kold, integer *init, integer *ksteps, integer *kle4, integer *iquit, integer *kprev, integer *ivc, integer *iv, integer *kgi, real *rpar, integer *ipar);
/*:ref: r1mach_ 6 1 4 */
/*:ref: xerrwv_ 14 11 13 4 4 4 4 4 4 4 6 6 124 */
/*:ref: sintrp_ 14 17 6 6 6 6 6 4 4 6 4 4 4 6 6 6 6 6 6 */
/*:ref: steps_ 14 36 214 4 6 6 6 6 6 12 6 4 4 12 6 6 6 6 6 6 6 6 6 6 12 4 12 4 6 6 6 4 4 4 4 6 6 4 */
extern int dqawo_(D_fp f, doublereal *a, doublereal *b, doublereal *omega, integer *integr, doublereal *epsabs, doublereal *epsrel, doublereal *result, doublereal *abserr, integer *neval, integer *ier, integer *leniw, integer *maxp1, integer *lenw, integer *last, integer *iwork, doublereal *work);
/*:ref: dqawoe_ 14 23 207 7 7 7 4 7 7 4 4 4 7 7 4 4 4 7 7 7 7 4 4 4 7 */
/*:ref: xerror_ 14 5 13 4 4 4 124 */
extern int hstart_(S_fp f, integer *neq, real *a, real *b, real *y, real *yprime, real *etol, integer *morder, real *small, real *big, real *spy, real *pv, real *yp, real *sf, real *rpar, integer *ipar, real *h__);
/*:ref: vnorm_ 6 2 6 4 */
extern int poistg_(integer *nperod, integer *n, integer *mperod, integer *m, real *a, real *b, real *c__, integer *idimy, real *y, integer *ierror, real *w);
/*:ref: postg2_ 14 17 4 4 4 6 6 6 4 6 6 6 6 6 6 6 6 6 6 */
extern E_f scnrm2_(integer *n, complex *cx, integer *incx);
extern int tred1_(integer *nm, integer *n, real *a, real *d__, real *e, real *e2);
extern C_f ccot_(complex * ret_val, complex *z__);
/*:ref: r1mach_ 6 1 4 */
/*:ref: xerclr_ 14 0 */
/*:ref: xerror_ 14 5 13 4 4 4 124 */
extern C_f ctanh_(complex * ret_val, complex *z__);
/*:ref: ctan_ 8 2 8 8 */
extern int dexbvp_(doublereal *y, integer *nrowy, doublereal *xpts, doublereal *a, integer *nrowa, doublereal *alpha, doublereal *b, integer *nrowb, doublereal *beta, integer *iflag, doublereal *work, integer *iwork);
/* comlen dml8sz_ 36 */
/* comlen dml18j_ 84 */
/* comlen dml15t_ 148 */
/* comlen dml17b_ 72 */
/* comlen dml5mc_ 52 */
/*:ref: dbvpor_ 14 31 7 4 4 7 4 7 4 7 4 7 4 7 4 4 7 4 7 4 4 7 4 7 7 7 7 7 7 7 7 4 4 */
/*:ref: xerrwv_ 14 11 13 4 4 4 4 4 4 4 6 6 124 */
extern int dqawoe_(D_fp f, doublereal *a, doublereal *b, doublereal *omega, integer *integr, doublereal *epsabs, doublereal *epsrel, integer *limit, integer *icall, integer *maxp1, doublereal *result, doublereal *abserr, integer *neval, integer *ier, integer *last, doublereal *alist__, doublereal *blist, doublereal *rlist, doublereal *elist, integer *iord, integer *nnlog, integer *momcom, doublereal *chebmo);
/*:ref: d1mach_ 7 1 4 */
/*:ref: dqc25f_ 14 15 207 7 7 7 4 4 4 4 7 7 4 7 7 4 7 */
/*:ref: dqpsrt_ 14 7 4 4 4 7 7 4 4 */
/*:ref: dqelg_ 14 6 4 7 7 7 7 4 */
extern int hstcrt_(real *a, real *b, integer *m, integer *mbdcnd, real *bda, real *bdb, real *c__, real *d__, integer *n, integer *nbdcnd, real *bdc, real *bdd, real *elmbda, real *f, integer *idimf, real *pertrb, integer *ierror, real *w);
/*:ref: poistg_ 14 11 4 4 4 4 6 6 6 4 6 4 6 */
/*:ref: genbun_ 14 11 4 4 4 4 6 6 6 4 6 4 6 */
extern int polcof_(real *xx, integer *n, real *x, real *c__, real *d__, real *work);
extern int scoef_(real *yh, real *yp, integer *ncomp, integer *nrowb, integer *nfc, integer *nic, real *b, real *beta, real *coef, integer *inhomo, real *re, real *ae, real *by, real *cvec, real *work, integer *iwork, integer *iflag, integer *nfcc);
/* comlen ml5mco_ 28 */
/*:ref: sdot_ 6 5 4 6 4 6 4 */
/*:ref: xgetf_ 14 1 4 */
/*:ref: xsetf_ 14 1 4 */
/*:ref: suds_ 14 10 6 6 6 4 4 4 4 4 6 4 */
extern int tred2_(integer *nm, integer *n, real *a, real *d__, real *e, real *z__);
extern C_f cdcdot_(complex * ret_val, integer *n, complex *cb, complex *cx, integer *incx, complex *cy, integer *incy);
extern int ctrco_(complex *t, integer *ldt, integer *n, real *rcond, complex *z__, integer *job);
/*:ref: scasum_ 6 3 4 8 4 */
/*:ref: csscal_ 14 4 4 6 8 4 */
/*:ref: caxpy_ 14 6 4 8 8 4 8 4 */
extern int dexint_(doublereal *x, integer *n, integer *kode, integer *m, doublereal *tol, doublereal *en, integer *ierr);
/*:ref: d1mach_ 7 1 4 */
/*:ref: i1mach_ 4 1 4 */
/*:ref: dpsixn_ 7 1 4 */
/*:ref: xerror_ 14 5 13 4 4 4 124 */
extern int dqaws_(D_fp f, doublereal *a, doublereal *b, doublereal *alfa, doublereal *beta, integer *integr, doublereal *epsabs, doublereal *epsrel, doublereal *result, doublereal *abserr, integer *neval, integer *ier, integer *limit, integer *lenw, integer *last, integer *iwork, doublereal *work);
/*:ref: dqawse_ 14 19 207 7 7 7 7 4 7 7 4 7 7 4 4 7 7 7 7 4 4 */
/*:ref: xerror_ 14 5 13 4 4 4 124 */
extern int hstcs1_(integer *intl, real *a, real *b, integer *m, integer *mbdcnd, real *bda, real *bdb, real *c__, real *d__, integer *n, integer *nbdcnd, real *bdc, real *bdd, real *elmbda, real *f, integer *idimf, real *pertrb, integer *ierr1, real *am, real *bm, real *cm, real *an, real *bn, real *cn, real *snth, real *rsq, real *wrk);
/*:ref: blktri_ 14 15 4 4 4 6 6 6 4 4 6 6 6 4 6 4 6 */
extern int polfit_(integer *n, real *x, real *y, real *w, integer *maxdeg, integer *ndeg, real *eps, real *r__, integer *ierr, real *a);
/*:ref: xerror_ 14 5 13 4 4 4 124 */
/*:ref: pvalue_ 14 6 4 4 6 6 6 6 */
extern int scopy_(integer *n, real *sx, integer *incx, real *sy, integer *incy);
extern int tred3_(integer *n, integer *nv, real *a, real *d__, real *e, real *e2);
extern int cdcorb_(complex *dfdy, real *el, S_fp fa, real *h__, integer *impl, integer *ipvt, integer *matdim, integer *miter, integer *ml, integer *mu, integer *n, integer *nde, integer *nq, real *t, complex *y, complex *yh, complex *ywt, logical *evalfa, complex *save1, complex *save2, complex *a, real *d__);
/*:ref: scnrm2_ 6 3 4 8 4 */
/*:ref: cgesl_ 14 6 8 4 4 4 8 4 */
/*:ref: cgbsl_ 14 8 8 4 4 4 4 4 8 4 */
/*:ref: users_ 14 12 8 8 8 8 8 6 6 6 4 4 4 4 */
extern int ctrdi_(complex *t, integer *ldt, integer *n, complex *det, integer *job, integer *info);
/*:ref: cscal_ 14 4 4 8 8 4 */
/*:ref: caxpy_ 14 6 4 8 8 4 8 4 */
extern doublereal dexprl_(doublereal *x);
/*:ref: d1mach_ 7 1 4 */
extern int dqawse_(D_fp f, doublereal *a, doublereal *b, doublereal *alfa, doublereal *beta, integer *integr, doublereal *epsabs, doublereal *epsrel, integer *limit, doublereal *result, doublereal *abserr, integer *neval, integer *ier, doublereal *alist__, doublereal *blist, doublereal *rlist, doublereal *elist, integer *iord, integer *last);
/*:ref: d1mach_ 7 1 4 */
/*:ref: dqmomo_ 14 7 7 7 7 7 7 7 4 */
/*:ref: dqc25s_ 14 16 207 7 7 7 7 7 7 7 7 7 7 7 7 7 4 4 */
/*:ref: dqpsrt_ 14 7 4 4 4 7 7 4 4 */
extern int hstcsp_(integer *intl, real *a, real *b, integer *m, integer *mbdcnd, real *bda, real *bdb, real *c__, real *d__, integer *n, integer *nbdcnd, real *bdc, real *bdd, real *elmbda, real *f, integer *idimf, real *pertrb, integer *ierror, real *w);
/*:ref: pimach_ 6 1 6 */
/*:ref: hstcs1_ 14 27 4 6 6 4 4 6 6 6 6 4 4 6 6 6 6 4 6 4 6 6 6 6 6 6 6 6 6 */
extern int polint_(integer *n, real *x, real *y, real *c__);
/*:ref: xerror_ 14 5 13 4 4 4 124 */
extern int scopym_(integer *n, real *sx, integer *incx, real *sy, integer *incy);
extern int tri3_(integer *m, real *a, real *b, real *c__, integer *k, real *y1, real *y2, real *y3, real *tcos, real *d__, real *w1, real *w2, real *w3);
extern int cdcstb_(integer *maxord, integer *mint, integer *iswflg, real *el, real *tq);
extern int ctrsl_(complex *t, integer *ldt, integer *n, complex *b, integer *job, integer *info);
/*:ref: caxpy_ 14 6 4 8 8 4 8 4 */
/*:ref: cdotc_ 8 6 8 4 8 4 8 4 */
extern doublereal dfac_(integer *n);
/*:ref: dgamlm_ 14 2 7 7 */
/*:ref: xerror_ 14 5 13 4 4 4 124 */
/*:ref: d9lgmc_ 7 1 7 */
extern int dqc25c_(D_fp f, doublereal *a, doublereal *b, doublereal *c__, doublereal *result, doublereal *abserr, integer *krul, integer *neval);
/*:ref: dqk15w_ 14 13 207 207 7 7 7 7 4 7 7 7 7 7 7 */
/*:ref: dqwgtc_ 7 :*/
/*:ref: dqcheb_ 14 4 7 7 7 7 */
extern int hstcyl_(real *a, real *b, integer *m, integer *mbdcnd, real *bda, real *bdb, real *c__, real *d__, integer *n, integer *nbdcnd, real *bdc, real *bdd, real *elmbda, real *f, integer *idimf, real *pertrb, integer *ierror, real *w);
/*:ref: poistg_ 14 11 4 4 4 4 6 6 6 4 6 4 6 */
/*:ref: genbun_ 14 11 4 4 4 4 6 6 6 4 6 4 6 */
extern int polyvl_(integer *nder, real *xx, real *yfit, real *yp, integer *n, real *x, real *c__, real *work, integer *ierr);
extern int scov_(S_fp fcn, integer *iopt, integer *m, integer *n, real *x, real *fvec, real *r__, integer *ldr, integer *info, real *wa1, real *wa2, real *wa3, real *wa4);
/*:ref: enorm_ 6 2 4 6 */
/*:ref: fdjac3_ 14 10 214 4 4 6 6 6 4 4 6 6 */
/*:ref: qrfac_ 14 10 4 4 6 4 12 4 4 6 6 6 */
/*:ref: rwupdt_ 14 8 4 6 4 6 6 6 6 6 */
/*:ref: xerror_ 14 5 13 4 4 4 124 */
extern int tridib_(integer *n, real *eps1, real *d__, real *e, real *e2, real *lb, real *ub, integer *m11, integer *m, real *w, integer *ind, integer *ierr, real *rv4, real *rv5);
extern int cdiv_(real *ar, real *ai, real *br, real *bi, real *cr, real *ci);
extern E_f cv_(real *xval, integer *ndata, integer *nconst, integer *nord, integer *nbkpt, real *bkpt, real *w);
/*:ref: bsplvn_ 14 6 6 4 4 6 4 6 */
/*:ref: sdot_ 6 5 4 6 4 6 4 */
extern int dfc_(integer *ndata, doublereal *xdata, doublereal *ydata, doublereal *sddata, integer *nord, integer *nbkpt, doublereal *bkpt, integer *nconst, doublereal *xconst, doublereal *yconst, integer *nderiv, integer *mode, doublereal *coeff, doublereal *w, integer *iw);
/*:ref: dfcmn_ 14 23 4 7 7 7 4 4 7 4 7 7 4 4 7 7 7 7 7 7 4 7 4 7 4 */
extern int dqc25f_(D_fp f, doublereal *a, doublereal *b, doublereal *omega, integer *integr, integer *nrmom, integer *maxp1, integer *ksave, doublereal *result, doublereal *abserr, integer *neval, doublereal *resabs, doublereal *resasc, integer *momcom, doublereal *chebmo);
/*:ref: d1mach_ 7 1 4 */
/*:ref: dqk15w_ 14 13 207 207 7 7 7 7 4 7 7 7 7 7 7 */
/*:ref: dqwgtf_ 7 :*/
/*:ref: dgtsl_ 14 6 4 7 7 7 7 4 */
/*:ref: dqcheb_ 14 4 7 7 7 7 */
extern int hstplr_(real *a, real *b, integer *m, integer *mbdcnd, real *bda, real *bdb, real *c__, real *d__, integer *n, integer *nbdcnd, real *bdc, real *bdd, real *elmbda, real *f, integer *idimf, real *pertrb, integer *ierror, real *w);
/*:ref: poistg_ 14 11 4 4 4 4 6 6 6 4 6 4 6 */
/*:ref: genbun_ 14 11 4 4 4 4 6 6 6 4 6 4 6 */
extern int pos3d1_(integer *lp, integer *l, integer *mp, integer *m, integer *n, real *a, real *b, real *c__, integer *ldimf, integer *mdimf, real *f, real *xrt, real *yrt, real *t, real *d__, real *wx, real *wy, real *c1, real *c2, real *bb);
/*:ref: pimach_ 6 1 6 */
/*:ref: sinti_ 14 2 4 6 */
/*:ref: costi_ 14 2 4 6 */
/*:ref: rffti_ 14 2 4 6 */
/*:ref: sinqi_ 14 2 4 6 */
/*:ref: cosqi_ 14 2 4 6 */
/*:ref: rfftf_ 14 3 4 6 6 */
/*:ref: rfftb_ 14 3 4 6 6 */
/*:ref: sint_ 14 3 4 6 6 */
/*:ref: sinqf_ 14 3 4 6 6 */
/*:ref: sinqb_ 14 3 4 6 6 */
/*:ref: cost_ 14 3 4 6 6 */
/*:ref: cosqf_ 14 3 4 6 6 */
/*:ref: cosqb_ 14 3 4 6 6 */
/*:ref: tridq_ 14 6 4 6 6 6 6 6 */
extern int sdcorb_(real *dfdy, real *el, S_fp fa, real *h__, integer *impl, integer *ipvt, integer *matdim, integer *miter, integer *ml, integer *mu, integer *n, integer *nde, integer *nq, real *t, real *y, real *yh, real *ywt, logical *evalfa, real *save1, real *save2, real *a, real *d__);
/*:ref: snrm2_ 6 3 4 6 4 */
/*:ref: sgesl_ 14 6 6 4 4 4 6 4 */
/*:ref: sgbsl_ 14 8 6 4 4 4 4 4 6 4 */
/*:ref: users_ 14 12 6 6 6 6 6 6 6 6 4 4 4 4 */
extern int tridq_(integer *mr, real *a, real *b, real *c__, real *y, real *d__);
extern int cdntlb_(real *eps, S_fp f, S_fp fa, real *hmax, real *hold, integer *impl, integer *jtask, integer *matdim, integer *maxord, integer *mint, integer *miter, integer *ml, integer *mu, integer *n, integer *nde, complex *save1, real *t, complex *y, complex *ywt, real *h__, integer *mntold, integer *mtrold, integer *nfe, real *rc, complex *yh, complex *a, logical *convrg, real *el, logical *ier, integer *ipvt, integer *nq, integer *nwait, real *rh, real *rmax, complex *save2, real *tq, real *trend, integer *iswflg);
/*:ref: cdcstb_ 14 5 4 4 4 6 6 */
/*:ref: users_ 14 12 8 8 8 8 8 6 6 6 4 4 4 4 */
/*:ref: cgefa_ 14 5 8 4 4 4 4 */
/*:ref: cgesl_ 14 6 8 4 4 4 8 4 */
/*:ref: cgbfa_ 14 7 8 4 4 4 4 4 4 */
/*:ref: cgbsl_ 14 8 8 4 4 4 4 4 8 4 */
/*:ref: scnrm2_ 6 3 4 8 4 */
/*:ref: cdsclb_ 14 8 6 4 4 6 6 6 6 8 */
extern doublereal d1mach_(integer *i__);
/*:ref: xerror_ 14 5 13 4 4 4 124 */
extern int dfcmn_(integer *ndata, doublereal *xdata, doublereal *ydata, doublereal *sddata, integer *nord, integer *nbkpt, doublereal *bkptin, integer *nconst, doublereal *xconst, doublereal *yconst, integer *nderiv, integer *mode, doublereal *coeff, doublereal *bf, doublereal *xtemp, doublereal *ptemp, doublereal *bkpt, doublereal *g, integer *mdg, doublereal *w, integer *mdw, doublereal *work, integer *iwork);
/*:ref: dbndsl_ 14 9 4 7 4 4 4 4 7 4 7 */
/*:ref: xerrwv_ 14 11 13 4 4 4 4 4 4 4 7 7 124 */
/*:ref: dlsei_ 14 13 7 4 4 4 4 4 7 7 7 7 4 7 4 */
/*:ref: xerror_ 14 5 13 4 4 4 124 */
/*:ref: dcopy_ 14 5 4 7 4 7 4 */
/*:ref: dsort_ 14 4 7 7 4 4 */
/*:ref: dbndac_ 14 7 7 4 4 4 4 4 4 */
/*:ref: dfspvn_ 14 6 7 4 4 7 4 7 */
/*:ref: dscal_ 14 4 4 7 7 4 */
/*:ref: dfspvd_ 14 6 7 4 7 4 7 4 */
/*:ref: daxpy_ 14 6 4 7 7 4 7 4 */
extern int dqc25s_(D_fp f, doublereal *a, doublereal *b, doublereal *bl, doublereal *br, doublereal *alfa, doublereal *beta, doublereal *ri, doublereal *rj, doublereal *rg, doublereal *rh, doublereal *result, doublereal *abserr, doublereal *resasc, integer *integr, integer *nev);
/*:ref: dqk15w_ 14 13 207 207 7 7 7 7 4 7 7 7 7 7 7 */
/*:ref: dqwgts_ 7 :*/
/*:ref: dqcheb_ 14 4 7 7 7 7 */
extern int hstssp_(real *a, real *b, integer *m, integer *mbdcnd, real *bda, real *bdb, real *c__, real *d__, integer *n, integer *nbdcnd, real *bdc, real *bdd, real *elmbda, real *f, integer *idimf, real *pertrb, integer *ierror, real *w);
/*:ref: pimach_ 6 1 6 */
/*:ref: poistg_ 14 11 4 4 4 4 6 6 6 4 6 4 6 */
/*:ref: genbun_ 14 11 4 4 4 4 6 6 6 4 6 4 6 */
extern int postg2_(integer *nperod, integer *n, integer *m, real *a, real *bb, real *c__, integer *idimq, real *q, real *b, real *b2, real *b3, real *w, real *w2, real *w3, real *d__, real *tcos, real *p);
/*:ref: cosgen_ 14 5 4 4 6 6 6 */
/*:ref: trix_ 14 10 4 4 4 6 6 6 6 6 6 6 */
/*:ref: merge_ 14 6 6 4 4 4 4 4 */
/*:ref: tri3_ 14 13 4 6 6 6 4 6 6 6 6 6 6 6 6 */
extern int sdcstb_(integer *maxord, integer *mint, integer *iswflg, real *el, real *tq);
extern int tris4_(integer *n, real *a, real *b, real *c__, real *d__, real *u, real *z__);
extern int cdntpb_(real *h__, integer *k, integer *n, integer *nq, real *t, real *tout, complex *yh, complex *y);
extern int d1mpyq_(integer *m, integer *n, doublereal *a, integer *lda, doublereal *v, doublereal *w);
extern int dfdjc1_(S_fp fcn, integer *n, doublereal *x, doublereal *fvec, doublereal *fjac, integer *ldfjac, integer *iflag, integer *ml, integer *mu, doublereal *epsfcn, doublereal *wa1, doublereal *wa2);
/*:ref: d1mach_ 7 1 4 */
extern int dqcheb_(doublereal *x, doublereal *fval, doublereal *cheb12, doublereal *cheb24);
extern int htrib3_(integer *nm, integer *n, real *a, real *tau, integer *m, real *zr, real *zi);
extern int ppadd_(integer *n, integer *ierror, real *a, real *c__, complex *cbp, real *bp, real *bh);
/* comlen cblkt_ 28 */
/*:ref: psgf_ 6 5 6 4 6 6 6 */
/*:ref: bsrh_ 6 8 6 6 4 6 6 6 206 6 */
/*:ref: ppsgf_ 6 5 6 4 6 6 6 */
extern int sdntlb_(real *eps, S_fp f, S_fp fa, real *hmax, real *hold, integer *impl, integer *jtask, integer *matdim, integer *maxord, integer *mint, integer *miter, integer *ml, integer *mu, integer *n, integer *nde, real *save1, real *t, real *y, real *ywt, real *h__, integer *mntold, integer *mtrold, integer *nfe, real *rc, real *yh, real *a, logical *convrg, real *el, logical *ier, integer *ipvt, integer *nq, integer *nwait, real *rh, real *rmax, real *save2, real *tq, real *trend, integer *iswflg);
/*:ref: sdcstb_ 14 5 4 4 4 6 6 */
/*:ref: users_ 14 12 6 6 6 6 6 6 6 6 4 4 4 4 */
/*:ref: sgefa_ 14 5 6 4 4 4 4 */
/*:ref: sgesl_ 14 6 6 4 4 4 6 4 */
/*:ref: sgbfa_ 14 7 6 4 4 4 4 4 4 */
/*:ref: sgbsl_ 14 8 6 4 4 4 4 4 6 4 */
/*:ref: snrm2_ 6 3 4 6 4 */
/*:ref: sdsclb_ 14 8 6 4 4 6 6 6 6 6 */
extern int trisp_(integer *n, real *a, real *b, real *c__, real *d__, real *u, real *z__);
extern C_f cdotc_(complex * ret_val, integer *n, complex *cx, integer *incx, complex *cy, integer *incy);
extern int d1updt_(integer *m, integer *n, doublereal *s, integer *ls, doublereal *u, doublereal *v, doublereal *w, logical *sing);
/*:ref: d1mach_ 7 1 4 */
extern int dfdjc3_(S_fp fcn, integer *m, integer *n, doublereal *x, doublereal *fvec, doublereal *fjac, integer *ldfjac, integer *iflag, doublereal *epsfcn, doublereal *wa);
/*:ref: d1mach_ 7 1 4 */
extern doublereal dqdota_(integer *n, doublereal *db, integer *qc, doublereal *dx, integer *incx, doublereal *dy, integer *incy);
/* comlen mpcom_ 68 */
/*:ref: mpblas_ 14 1 4 */
/*:ref: mpcdm_ 14 2 7 4 */
/*:ref: mpadd_ 14 3 4 4 4 */
/*:ref: mpmul_ 14 3 4 4 4 */
/*:ref: mpcmd_ 14 2 4 7 */
extern int htribk_(integer *nm, integer *n, real *ar, real *ai, real *tau, integer *m, real *zr, real *zi);
extern int ppgq8_(E_fp fun, integer *ldc, real *c__, real *xi, integer *lxi, integer *kk, integer *id, real *a, real *b, integer *inppv, real *err, real *ans, integer *ierr);
/*:ref: xerror_ 14 5 13 4 4 4 124 */
/*:ref: i1mach_ 4 1 4 */
/*:ref: r1mach_ 6 1 4 */
/*:ref: ppval_ 6 8 4 6 6 4 4 4 6 4 */
extern int sdntpb_(real *h__, integer *k, integer *n, integer *nq, real *t, real *tout, real *yh, real *y);
extern int trix_(integer *idegbr, integer *idegcr, integer *m, real *a, real *b, real *c__, real *y, real *tcos, real *d__, real *w);
extern C_f cdotu_(complex * ret_val, integer *n, complex *cx, integer *incx, complex *cy, integer *incy);
extern int d9aimp_(doublereal *x, doublereal *ampl, doublereal *theta);
/*:ref: d1mach_ 7 1 4 */
/*:ref: initds_ 4 3 7 4 6 */
/*:ref: dcsevl_ 7 3 7 7 4 */
/*:ref: xerror_ 14 5 13 4 4 4 124 */
extern int dfehl_(S_fp df, integer *neq, doublereal *t, doublereal *y, doublereal *h__, doublereal *yp, doublereal *f1, doublereal *f2, doublereal *f3, doublereal *f4, doublereal *f5, doublereal *ys, doublereal *rpar, integer *ipar);
extern doublereal dqdoti_(integer *n, doublereal *db, integer *qc, doublereal *dx, integer *incx, doublereal *dy, integer *incy);
/* comlen mpcom_ 68 */
/*:ref: mpblas_ 14 1 4 */
/*:ref: mpcdm_ 14 2 7 4 */
/*:ref: mpadd_ 14 3 4 4 4 */
/*:ref: mpmul_ 14 3 4 4 4 */
/*:ref: mpcmd_ 14 2 4 7 */
extern int htrid3_(integer *nm, integer *n, real *a, real *d__, real *e, real *e2, real *tau);
/*:ref: pythag_ 6 2 6 6 */
extern E_f ppgsf_(real *x, integer *iz, real *c__, real *a, real *bh);
extern E_f sdot_(integer *n, real *sx, integer *incx, real *sy, integer *incy);
extern int tsturm_(integer *nm, integer *n, real *eps1, real *d__, real *e, real *e2, real *lb, real *ub, integer *mm, integer *m, real *w, real *z__, integer *ierr, real *rv1, real *rv2, real *rv3, real *rv4, real *rv5, real *rv6);
extern int cdpscb_(integer *ksgn, integer *n, integer *nq, complex *yh);
extern doublereal d9atn1_(doublereal *x);
/*:ref: d1mach_ 7 1 4 */
/*:ref: initds_ 4 3 7 4 6 */
/*:ref: dcsevl_ 7 3 7 7 4 */
/*:ref: xerror_ 14 5 13 4 4 4 124 */
extern int dfspvd_(doublereal *t, integer *k, doublereal *x, integer *ileft, doublereal *vnikx, integer *nderiv);
/*:ref: dfspvn_ 14 6 7 4 4 7 4 7 */
extern int dqelg_(integer *n, doublereal *epstab, doublereal *result, doublereal *abserr, doublereal *res3la, integer *nres);
/*:ref: d1mach_ 7 1 4 */
extern int htridi_(integer *nm, integer *n, real *ar, real *ai, real *d__, real *e, real *e2, real *tau);
/*:ref: pythag_ 6 2 6 6 */
extern E_f pppsf_(real *x, integer *iz, real *c__, real *a, real *bh);
extern int sdpscb_(integer *ksgn, integer *n, integer *nq, real *yh);
extern int u11ls_(real *a, integer *mda, integer *m, integer *n, real *ub, real *db, integer *mode, integer *np, integer *krank, integer *ksure, real *h__, real *w, real *eb, integer *ic, integer *ir);
/*:ref: snrm2_ 6 3 4 6 4 */
/*:ref: xerror_ 14 5 13 4 4 4 124 */
/*:ref: sswap_ 14 5 4 6 4 6 4 */
/*:ref: iswap_ 14 5 4 4 4 4 4 */
/*:ref: isamax_ 4 3 4 6 4 */
/*:ref: sscal_ 14 4 4 6 6 4 */
/*:ref: sdot_ 6 5 4 6 4 6 4 */
/*:ref: saxpy_ 14 6 4 6 6 4 6 4 */
extern int cdpstb_(real *el, S_fp f, S_fp fa, real *h__, integer *impl, S_fp jacobn, integer *matdim, integer *miter, integer *ml, integer *mu, integer *n, integer *nde, integer *nq, complex *save2, real *t, complex *y, complex *yh, complex *ywt, real *uround, integer *nfe, integer *nje, complex *a, complex *dfdy, logical *ier, integer *ipvt, complex *save1, integer *iswflg, real *bnd);
/*:ref: scnrm2_ 6 3 4 8 4 */
/*:ref: cgefa_ 14 5 8 4 4 4 4 */
/*:ref: cgbfa_ 14 7 8 4 4 4 4 4 4 */
/*:ref: users_ 14 12 8 8 8 8 8 6 6 6 4 4 4 4 */
extern int d9b0mp_(doublereal *x, doublereal *ampl, doublereal *theta);
/*:ref: d1mach_ 7 1 4 */
/*:ref: initds_ 4 3 7 4 6 */
/*:ref: xerror_ 14 5 13 4 4 4 124 */
/*:ref: dcsevl_ 7 3 7 7 4 */
extern int dfspvn_(doublereal *t, integer *jhigh, integer *index, doublereal *x, integer *ileft, doublereal *vnikx);
extern int dqform_(integer *m, integer *n, doublereal *q, integer *ldq, doublereal *wa);
extern int hw3crt_(real *xs, real *xf, integer *l, integer *lbdcnd, real *bdxs, real *bdxf, real *ys, real *yf, integer *m, integer *mbdcnd, real *bdys, real *bdyf, real *zs, real *zf, integer *n, integer *nbdcnd, real *bdzs, real *bdzf, real *elmbda, integer *ldimf, integer *mdimf, real *f, real *pertrb, integer *ierror, real *w);
/*:ref: pois3d_ 14 16 4 4 6 4 4 6 4 4 6 6 6 4 4 6 4 6 */
extern int ppqad_(integer *ldc, real *c__, real *xi, integer *lxi, integer *k, real *x1, real *x2, real *pquad);
/*:ref: intrv_ 14 6 6 4 6 4 4 4 */
/*:ref: xerror_ 14 5 13 4 4 4 124 */
extern int sdpstb_(real *el, S_fp f, S_fp fa, real *h__, integer *impl, S_fp jacobn, integer *matdim, integer *miter, integer *ml, integer *mu, integer *n, integer *nde, integer *nq, real *save2, real *t, real *y, real *yh, real *ywt, real *uround, integer *nfe, integer *nje, real *a, real *dfdy, logical *ier, integer *ipvt, real *save1, integer *iswflg, real *bnd);
/*:ref: snrm2_ 6 3 4 6 4 */
/*:ref: sgefa_ 14 5 6 4 4 4 4 */
/*:ref: sgbfa_ 14 7 6 4 4 4 4 4 4 */
/*:ref: users_ 14 12 6 6 6 6 6 6 6 6 4 4 4 4 */
extern int u11us_(real *a, integer *mda, integer *m, integer *n, real *ub, real *db, integer *mode, integer *np, integer *krank, integer *ksure, real *h__, real *w, real *eb, integer *ir, integer *ic);
/*:ref: snrm2_ 6 3 4 6 4 */
/*:ref: xerror_ 14 5 13 4 4 4 124 */
/*:ref: sswap_ 14 5 4 6 4 6 4 */
/*:ref: iswap_ 14 5 4 4 4 4 4 */
/*:ref: isamax_ 4 3 4 6 4 */
/*:ref: sscal_ 14 4 4 6 6 4 */
/*:ref: sdot_ 6 5 4 6 4 6 4 */
/*:ref: saxpy_ 14 6 4 6 6 4 6 4 */
extern int cdrvb1_(integer *n, real *t, complex *y, real *tout, integer *mstate, real *eps, complex *work, integer *lenw);
/*:ref: xerror_ 14 5 13 4 4 4 124 */
/*:ref: r1mach_ 6 1 4 */
/*:ref: cdrvb3_ 14 27 4 6 8 200 4 6 4 4 6 6 4 4 4 4 4 4 4 6 8 4 4 4 200 200 4 4 206 */
/*:ref: g_ 6 :*/
extern int d9b1mp_(doublereal *x, doublereal *ampl, doublereal *theta);
/*:ref: d1mach_ 7 1 4 */
/*:ref: initds_ 4 3 7 4 6 */
/*:ref: xerror_ 14 5 13 4 4 4 124 */
/*:ref: dcsevl_ 7 3 7 7 4 */
extern int dfulmt_(integer *i__, integer *j, doublereal *aij, integer *indcat, doublereal *prgopt, doublereal *dattrv, integer *iflag);
/*:ref: xerror_ 14 5 13 4 4 4 124 */
extern int dqk15_(D_fp f, doublereal *a, doublereal *b, doublereal *result, doublereal *abserr, doublereal *resabs, doublereal *resasc);
/*:ref: d1mach_ 7 1 4 */
extern int hwscrt_(real *a, real *b, integer *m, integer *mbdcnd, real *bda, real *bdb, real *c__, real *d__, integer *n, integer *nbdcnd, real *bdc, real *bdd, real *elmbda, real *f, integer *idimf, real *pertrb, integer *ierror, real *w);
/*:ref: genbun_ 14 11 4 4 4 4 6 6 6 4 6 4 6 */
extern E_f ppsgf_(real *x, integer *iz, real *c__, real *a, real *bh);
extern int sdrvb1_(integer *n, real *t, real *y, real *tout, integer *mstate, real *eps, real *work, integer *lenw);
/*:ref: xerror_ 14 5 13 4 4 4 124 */
/*:ref: r1mach_ 6 1 4 */
/*:ref: sdrvb3_ 14 27 4 6 6 200 4 6 4 4 6 6 4 4 4 4 4 4 4 6 6 4 4 4 200 200 4 4 206 */
/*:ref: g_ 6 :*/
extern int u12ls_(real *a, integer *mda, integer *m, integer *n, real *b, integer *mdb, integer *nb, integer *mode, integer *krank, real *rnorm, real *h__, real *w, integer *ic, integer *ir);
/*:ref: snrm2_ 6 3 4 6 4 */
/*:ref: sdot_ 6 5 4 6 4 6 4 */
/*:ref: saxpy_ 14 6 4 6 6 4 6 4 */
/*:ref: sswap_ 14 5 4 6 4 6 4 */
extern int cdrvb2_(integer *n, real *t, complex *y, U_fp f, real *tout, integer *mstate, integer *nroot, real *eps, real *ewt, integer *mint, complex *work, integer *lenw, integer *iwork, integer *leniw, E_fp g);
/*:ref: xerror_ 14 5 13 4 4 4 124 */
/*:ref: r1mach_ 6 1 4 */
/*:ref: cdrvb3_ 14 27 4 6 8 200 4 6 4 4 6 6 4 4 4 4 4 4 4 6 8 4 4 4 200 200 4 4 206 */
extern doublereal d9chu_(doublereal *a, doublereal *b, doublereal *z__);
/*:ref: d1mach_ 7 1 4 */
/*:ref: xerror_ 14 5 13 4 4 4 124 */
extern int dfzero_(D_fp f, doublereal *b, doublereal *c__, doublereal *r__, doublereal *re, doublereal *ae, integer *iflag);
/*:ref: d1mach_ 7 1 4 */
extern int dqk15i_(D_fp f, doublereal *boun, integer *inf, doublereal *a, doublereal *b, doublereal *result, doublereal *abserr, doublereal *resabs, doublereal *resasc);
/*:ref: d1mach_ 7 1 4 */
extern int hwscs1_(integer *intl, real *ts, real *tf, integer *m, integer *mbdcnd, real *bdts, real *bdtf, real *rs, real *rf, integer *n, integer *nbdcnd, real *bdrs, real *bdrf, real *elmbda, real *f, integer *idimf, real *pertrb, real *w, real *s, real *an, real *bn, real *cn, real *r__, real *am, real *bm, real *cm, real *sint, real *bmh);
/*:ref: pimach_ 6 1 6 */
/*:ref: epmach_ 6 1 6 */
/*:ref: blktri_ 14 15 4 4 4 6 6 6 4 4 6 6 6 4 6 4 6 */
extern E_f ppspf_(real *x, integer *iz, real *c__, real *a, real *bh);
extern int sdrvb2_(integer *n, real *t, real *y, U_fp f, real *tout, integer *mstate, integer *nroot, real *eps, real *ewt, integer *mint, real *work, integer *lenw, integer *iwork, integer *leniw, E_fp g);
/*:ref: xerror_ 14 5 13 4 4 4 124 */
/*:ref: r1mach_ 6 1 4 */
/*:ref: sdrvb3_ 14 27 4 6 6 200 4 6 4 4 6 6 4 4 4 4 4 4 4 6 6 4 4 4 200 200 4 4 206 */
extern int u12us_(real *a, integer *mda, integer *m, integer *n, real *b, integer *mdb, integer *nb, integer *mode, integer *krank, real *rnorm, real *h__, real *w, integer *ir, integer *ic);
/*:ref: snrm2_ 6 3 4 6 4 */
/*:ref: sdot_ 6 5 4 6 4 6 4 */
/*:ref: saxpy_ 14 6 4 6 6 4 6 4 */
/*:ref: sswap_ 14 5 4 6 4 6 4 */
extern int cdrvb3_(integer *n, real *t, complex *y, S_fp f, integer *nstate, real *tout, integer *ntask, integer *nroot, real *eps, real *ewt, integer *ierror, integer *mint, integer *miter, integer *impl, integer *ml, integer *mu, integer *mxord, real *hmax, complex *work, integer *lenw, integer *iwork, integer *leniw, U_fp jacobn, S_fp fa, integer *nde, integer *mxstep, E_fp g);
/*:ref: r1mach_ 6 1 4 */
/*:ref: xerror_ 14 5 13 4 4 4 124 */
/*:ref: cdntpb_ 14 8 6 4 4 4 6 6 8 8 */
/*:ref: users_ 14 12 8 8 8 8 8 6 6 8 4 4 4 4 */
/*:ref: cgefa_ 14 5 8 4 4 4 4 */
/*:ref: cgesl_ 14 6 8 4 4 4 8 4 */
/*:ref: cgbfa_ 14 7 8 4 4 4 4 4 4 */
/*:ref: cgbsl_ 14 8 8 4 4 4 4 4 8 4 */
/*:ref: scnrm2_ 6 3 4 8 4 */
/*:ref: cdstpb_ 14 48 6 214 214 6 4 200 4 4 4 4 4 4 4 4 8 6 6 6 6 6 4 4 4 4 4 4 4 6 8 8 8 12 8 6 6 4 4 4 4 6 6 8 8 6 6 4 4 4 */
/*:ref: cdzrob_ 14 15 6 206 6 4 4 4 6 6 8 6 6 6 6 6 8 */
extern doublereal d9gmic_(doublereal *a, doublereal *x, doublereal *alx);
/*:ref: d1mach_ 7 1 4 */
/*:ref: xerror_ 14 5 13 4 4 4 124 */
/*:ref: dlngam_ 7 1 7 */
extern doublereal dgami_(doublereal *a, doublereal *x);
/*:ref: xerror_ 14 5 13 4 4 4 124 */
/*:ref: dlngam_ 7 1 7 */
/*:ref: dgamit_ 7 2 7 7 */
extern int dqk15w_(D_fp f, D_fp w, doublereal *p1, doublereal *p2, doublereal *p3, doublereal *p4, integer *kp, doublereal *a, doublereal *b, doublereal *result, doublereal *abserr, doublereal *resabs, doublereal *resasc);
/*:ref: d1mach_ 7 1 4 */
extern int hwscsp_(integer *intl, real *ts, real *tf, integer *m, integer *mbdcnd, real *bdts, real *bdtf, real *rs, real *rf, integer *n, integer *nbdcnd, real *bdrs, real *bdrf, real *elmbda, real *f, integer *idimf, real *pertrb, integer *ierror, real *w);
/*:ref: pimach_ 6 1 6 */
/*:ref: hwscs1_ 14 28 4 6 6 4 4 6 6 6 6 4 4 6 6 6 6 4 6 6 6 6 6 6 6 6 6 6 6 6 */
extern E_f ppval_(integer *ldc, real *c__, real *xi, integer *lxi, integer *k, integer *ideriv, real *x, integer *inppv);
/*:ref: intrv_ 14 6 6 4 6 4 4 4 */
/*:ref: xerror_ 14 5 13 4 4 4 124 */
extern int sdrvb3_(integer *n, real *t, real *y, S_fp f, integer *nstate, real *tout, integer *ntask, integer *nroot, real *eps, real *ewt, integer *ierror, integer *mint, integer *miter, integer *impl, integer *ml, integer *mu, integer *mxord, real *hmax, real *work, integer *lenw, integer *iwork, integer *leniw, U_fp jacobn, S_fp fa, integer *nde, integer *mxstep, E_fp g);
/*:ref: r1mach_ 6 1 4 */
/*:ref: xerror_ 14 5 13 4 4 4 124 */
/*:ref: sdntpb_ 14 8 6 4 4 4 6 6 6 6 */
/*:ref: users_ 14 12 6 6 6 6 6 6 6 6 4 4 4 4 */
/*:ref: sgefa_ 14 5 6 4 4 4 4 */
/*:ref: sgesl_ 14 6 6 4 4 4 6 4 */
/*:ref: sgbfa_ 14 7 6 4 4 4 4 4 4 */
/*:ref: sgbsl_ 14 8 6 4 4 4 4 4 6 4 */
/*:ref: snrm2_ 6 3 4 6 4 */
/*:ref: sdstpb_ 14 48 6 214 214 6 4 200 4 4 4 4 4 4 4 4 6 6 6 6 6 6 4 4 4 4 4 4 4 6 6 6 6 12 6 6 6 4 4 4 4 6 6 6 6 6 6 4 4 4 */
/*:ref: sdzrob_ 14 15 6 206 6 4 4 4 6 6 6 6 6 6 6 6 6 */
extern int ulsia_(real *a, integer *mda, integer *m, integer *n, real *b, integer *mdb, integer *nb, real *re, real *ae, integer *key, integer *mode, integer *np, integer *krank, integer *ksure, real *rnorm, real *w, integer *lw, integer *iwork, integer *liw, integer *info);
/*:ref: r1mach_ 6 1 4 */
/*:ref: u11us_ 14 15 6 4 4 4 6 6 4 4 4 4 6 6 6 4 4 */
/*:ref: u12us_ 14 14 6 4 4 4 6 4 4 4 4 6 6 6 4 4 */
/*:ref: xerror_ 14 5 13 4 4 4 124 */
extern int cdsclb_(real *hmax, integer *n, integer *nq, real *rmax, real *h__, real *rc, real *rh, complex *yh);
extern doublereal d9gmit_(doublereal *a, doublereal *x, doublereal *algap1, doublereal *sgngam, doublereal *alx);
/*:ref: d1mach_ 7 1 4 */
/*:ref: xerror_ 14 5 13 4 4 4 124 */
/*:ref: dlngam_ 7 1 7 */
extern doublereal dgamic_(doublereal *a, doublereal *x);
/*:ref: d1mach_ 7 1 4 */
/*:ref: xerror_ 14 5 13 4 4 4 124 */
/*:ref: dlngam_ 7 1 7 */
/*:ref: d9gmic_ 7 3 7 7 7 */
/*:ref: dlgams_ 14 3 7 7 7 */
/*:ref: d9gmit_ 7 5 7 7 7 7 7 */
/*:ref: d9lgic_ 7 3 7 7 7 */
/*:ref: d9lgit_ 7 3 7 7 7 */
/*:ref: xerclr_ 14 0 */
extern int dqk21_(D_fp f, doublereal *a, doublereal *b, doublereal *result, doublereal *abserr, doublereal *resabs, doublereal *resasc);
/*:ref: d1mach_ 7 1 4 */
extern int hwscyl_(real *a, real *b, integer *m, integer *mbdcnd, real *bda, real *bdb, real *c__, real *d__, integer *n, integer *nbdcnd, real *bdc, real *bdd, real *elmbda, real *f, integer *idimf, real *pertrb, integer *ierror, real *w);
/*:ref: genbun_ 14 11 4 4 4 4 6 6 6 4 6 4 6 */
extern int proc_(integer *nd, real *bd, integer *nm1, real *bm1, integer *nm2, real *bm2, integer *na, real *aa, complex *x, complex *y, integer *m, complex *a, complex *b, complex *c__, complex *d__, complex *w, complex *u);
extern int sdsclb_(real *hmax, integer *n, integer *nq, real *rmax, real *h__, real *rc, real *rh, real *yh);
extern int usrmat_(integer *i__, integer *j, real *aij, integer *indcat, real *prgopt, real *dattrv, integer *iflag);
extern int cdstpb_(real *eps, S_fp f, U_fp fa, real *hmax, integer *impl, U_fp jacobn, integer *matdim, integer *maxord, integer *mint, integer *miter, integer *ml, integer *mu, integer *n, integer *nde, complex *ywt, real *uround, real *avgh, real *avgord, real *h__, real *hused, integer *jtask, integer *mntold, integer *mtrold, integer *nfe, integer *nje, integer *nqused, integer *nstep, real *t, complex *y, complex *yh, complex *a, logical *convrg, complex *dfdy, real *el, real *hold, integer *ipvt, integer *jstate, integer *nq, integer *nwait, real *rc, real *rmax, complex *save1, complex *save2, real *tq, real *trend, integer *iswflg, integer *mtrsv, integer *mxrdsv);
/*:ref: cdntlb_ 14 38 6 214 200 6 6 4 4 4 4 4 4 4 4 4 4 8 6 8 8 6 4 4 4 6 8 8 12 6 12 4 4 4 6 6 8 6 6 4 */
/*:ref: cdpscb_ 14 4 4 4 4 8 */
/*:ref: cdpstb_ 14 28 6 214 200 6 4 200 4 4 4 4 4 4 4 8 6 8 8 8 6 4 4 8 8 12 4 8 4 6 */
/*:ref: cdcorb_ 14 22 8 6 200 6 4 4 4 4 4 4 4 4 4 6 8 8 8 12 8 8 8 6 */
/*:ref: scnrm2_ 6 3 4 8 4 */
/*:ref: cdsclb_ 14 8 6 4 4 6 6 6 6 8 */
/*:ref: cdcstb_ 14 5 4 4 4 6 6 */
extern int d9knus_(doublereal *xnu, doublereal *x, doublereal *bknu, doublereal *bknu1, integer *iswtch);
/*:ref: d1mach_ 7 1 4 */
/*:ref: initds_ 4 3 7 4 6 */
/*:ref: xerror_ 14 5 13 4 4 4 124 */
/*:ref: dgamma_ 7 1 7 */
/*:ref: dcsevl_ 7 3 7 7 4 */
extern doublereal dgamit_(doublereal *a, doublereal *x);
/*:ref: d1mach_ 7 1 4 */
/*:ref: xerror_ 14 5 13 4 4 4 124 */
/*:ref: dgamr_ 7 1 7 */
/*:ref: dlgams_ 14 3 7 7 7 */
/*:ref: d9gmit_ 7 5 7 7 7 7 7 */
/*:ref: d9lgit_ 7 3 7 7 7 */
/*:ref: dlngam_ 7 1 7 */
/*:ref: xerclr_ 14 0 */
/*:ref: d9lgic_ 7 3 7 7 7 */
extern int dqk31_(D_fp f, doublereal *a, doublereal *b, doublereal *result, doublereal *abserr, doublereal *resabs, doublereal *resasc);
/*:ref: d1mach_ 7 1 4 */
extern int hwsplr_(real *a, real *b, integer *m, integer *mbdcnd, real *bda, real *bdb, real *c__, real *d__, integer *n, integer *nbdcnd, real *bdc, real *bdd, real *elmbda, real *f, integer *idimf, real *pertrb, integer *ierror, real *w);
/*:ref: genbun_ 14 11 4 4 4 4 6 6 6 4 6 4 6 */
extern int procp_(integer *nd, real *bd, integer *nm1, real *bm1, integer *nm2, real *bm2, integer *na, real *aa, complex *x, complex *y, integer *m, complex *a, complex *b, complex *c__, complex *d__, complex *u, complex *w);
extern E_f sdsdot_(integer *n, real *sb, real *sx, integer *incx, real *sy, integer *incy);
extern E_f vnorm_(real *v, integer *ncomp);
extern int cdzrob_(real *ae, E_fp f, real *h__, integer *n, integer *nq, integer *iroot, real *re, real *t, complex *yh, real *uround, real *b, real *c__, real *fb, real *fc, complex *y);
/*:ref: cdntpb_ 14 8 6 4 4 4 6 6 8 8 */
extern doublereal d9lgic_(doublereal *a, doublereal *x, doublereal *alx);
/*:ref: d1mach_ 7 1 4 */
/*:ref: xerror_ 14 5 13 4 4 4 124 */
extern int dgamlm_(doublereal *xmin, doublereal *xmax);
/*:ref: d1mach_ 7 1 4 */
/*:ref: xerror_ 14 5 13 4 4 4 124 */
extern int dqk41_(D_fp f, doublereal *a, doublereal *b, doublereal *result, doublereal *abserr, doublereal *resabs, doublereal *resasc);
/*:ref: d1mach_ 7 1 4 */
extern int hwsss1_(real *ts, real *tf, integer *m, integer *mbdcnd, real *bdts, real *bdtf, real *ps, real *pf, integer *n, integer *nbdcnd, real *bdps, real *bdpf, real *elmbda, real *f, integer *idimf, real *pertrb, real *am, real *bm, real *cm, real *sn, real *ss, real *sint, real *d__);
/*:ref: pimach_ 6 1 6 */
/*:ref: genbun_ 14 11 4 4 4 4 6 6 6 4 6 4 6 */
extern int prod_(integer *nd, real *bd, integer *nm1, real *bm1, integer *nm2, real *bm2, integer *na, real *aa, real *x, real *y, integer *m, real *a, real *b, real *c__, real *d__, real *w, real *u);
extern int sdstpb_(real *eps, S_fp f, U_fp fa, real *hmax, integer *impl, U_fp jacobn, integer *matdim, integer *maxord, integer *mint, integer *miter, integer *ml, integer *mu, integer *n, integer *nde, real *ywt, real *uround, real *avgh, real *avgord, real *h__, real *hused, integer *jtask, integer *mntold, integer *mtrold, integer *nfe, integer *nje, integer *nqused, integer *nstep, real *t, real *y, real *yh, real *a, logical *convrg, real *dfdy, real *el, real *hold, integer *ipvt, integer *jstate, integer *nq, integer *nwait, real *rc, real *rmax, real *save1, real *save2, real *tq, real *trend, integer *iswflg, integer *mtrsv, integer *mxrdsv);
/*:ref: sdntlb_ 14 38 6 214 200 6 6 4 4 4 4 4 4 4 4 4 4 6 6 6 6 6 4 4 4 6 6 6 12 6 12 4 4 4 6 6 6 6 6 4 */
/*:ref: sdpscb_ 14 4 4 4 4 6 */
/*:ref: sdpstb_ 14 28 6 214 200 6 4 200 4 4 4 4 4 4 4 6 6 6 6 6 6 4 4 6 6 12 4 6 4 6 */
/*:ref: sdcorb_ 14 22 6 6 200 6 4 4 4 4 4 4 4 4 4 6 6 6 6 12 6 6 6 6 */
/*:ref: snrm2_ 6 3 4 6 4 */
/*:ref: sdsclb_ 14 8 6 4 4 6 6 6 6 6 */
/*:ref: sdcstb_ 14 5 4 4 4 6 6 */
extern E_f vnwrms_(integer *n, real *v, real *w);
extern C_f cexprl_(complex * ret_val, complex *z__);
/*:ref: r1mach_ 6 1 4 */
extern doublereal d9lgit_(doublereal *a, doublereal *x, doublereal *algap1);
/*:ref: d1mach_ 7 1 4 */
/*:ref: xerror_ 14 5 13 4 4 4 124 */
extern doublereal dgamma_(doublereal *x);
/*:ref: initds_ 4 3 7 4 6 */
/*:ref: d1mach_ 7 1 4 */
/*:ref: dgamlm_ 14 2 7 7 */
/*:ref: dcsevl_ 7 3 7 7 4 */
/*:ref: xerror_ 14 5 13 4 4 4 124 */
/*:ref: d9lgmc_ 7 1 7 */
extern int dqk51_(D_fp f, doublereal *a, doublereal *b, doublereal *result, doublereal *abserr, doublereal *resabs, doublereal *resasc);
/*:ref: d1mach_ 7 1 4 */
extern int hwsssp_(real *ts, real *tf, integer *m, integer *mbdcnd, real *bdts, real *bdtf, real *ps, real *pf, integer *n, integer *nbdcnd, real *bdps, real *bdpf, real *elmbda, real *f, integer *idimf, real *pertrb, integer *ierror, real *w);
/*:ref: pimach_ 6 1 6 */
/*:ref: hwsss1_ 14 23 6 6 4 4 6 6 6 6 4 4 6 6 6 6 4 6 6 6 6 6 6 6 6 */
extern int prodp_(integer *nd, real *bd, integer *nm1, real *bm1, integer *nm2, real *bm2, integer *na, real *aa, real *x, real *y, integer *m, real *a, real *b, real *c__, real *d__, real *u, real *w);
extern int sdzrob_(real *ae, E_fp f, real *h__, integer *n, integer *nq, integer *iroot, real *re, real *t, real *yh, real *uround, real *b, real *c__, real *fb, real *fc, real *y);
/*:ref: sdntpb_ 14 8 6 4 4 4 6 6 6 6 */
extern int wnlit_(real *w, integer *mdw, integer *m, integer *n, integer *l, integer *ipivot, integer *itype, real *h__, real *scale, real *rnorm, integer *idope, real *dope, logical *done);
/*:ref: srotmg_ 14 5 6 6 6 6 6 */
/*:ref: srotm_ 14 6 4 6 4 6 4 6 */
/*:ref: isamax_ 4 3 4 6 4 */
/*:ref: scopy_ 14 5 4 6 4 6 4 */
/*:ref: sswap_ 14 5 4 6 4 6 4 */
/*:ref: sscal_ 14 4 4 6 6 4 */
/*:ref: h12_ 14 11 4 4 4 4 6 4 6 6 4 4 4 */
extern int cfftb_(integer *n, complex *c__, real *wsave);
/*:ref: cfftb1_ 14 5 4 8 6 6 6 */
extern doublereal d9lgmc_(doublereal *x);
/*:ref: initds_ 4 3 7 4 6 */
/*:ref: d1mach_ 7 1 4 */
/*:ref: xerror_ 14 5 13 4 4 4 124 */
/*:ref: dcsevl_ 7 3 7 7 4 */
extern doublereal dgamr_(doublereal *x);
/*:ref: xgetf_ 14 1 4 */
/*:ref: xsetf_ 14 1 4 */
/*:ref: dgamma_ 7 1 7 */
/*:ref: xerclr_ 14 0 */
/*:ref: dlgams_ 14 3 7 7 7 */
extern int dqk61_(D_fp f, doublereal *a, doublereal *b, doublereal *result, doublereal *abserr, doublereal *resabs, doublereal *resasc);
/*:ref: d1mach_ 7 1 4 */
extern integer i1mach_(integer *i__);
/*:ref: xerror_ 14 5 13 4 4 4 124 */
extern E_f prvec_(integer *m, real *u, real *v);
/*:ref: sdot_ 6 5 4 6 4 6 4 */
extern int sepeli_(integer *intl, integer *iorder, real *a, real *b, integer *m, integer *mbdcnd, real *bda, real *alpha, real *bdb, real *beta, real *c__, real *d__, integer *n, integer *nbdcnd, real *bdc, real *gama, real *bdd, real *xnu, U_fp cofx, U_fp cofy, real *grhs, real *usol, integer *idmn, real *w, real *pertrb, integer *ierror);
/*:ref: chkprm_ 14 14 4 4 6 6 4 4 6 6 4 4 200 200 4 4 */
/*:ref: spelip_ 14 38 4 4 6 6 4 4 6 6 6 6 6 6 4 4 6 6 6 6 200 200 6 6 6 6 6 6 6 6 6 6 6 6 6 6 4 6 6 4 */
extern int wnlsm_(real *w, integer *mdw, integer *mme, integer *ma, integer *n, integer *l, real *prgopt, real *x, real *rnorm, integer *mode, integer *ipivot, integer *itype, real *wd, real *h__, real *scale, real *z__, real *temp, real *d__);
/*:ref: scopy_ 14 5 4 6 4 6 4 */
/*:ref: xerror_ 14 5 13 4 4 4 124 */
/*:ref: snrm2_ 6 3 4 6 4 */
/*:ref: sscal_ 14 4 4 6 6 4 */
/*:ref: sasum_ 6 3 4 6 4 */
/*:ref: isamax_ 4 3 4 6 4 */
/*:ref: wnlit_ 14 13 6 4 4 4 4 4 4 6 6 6 4 6 12 */
/*:ref: sswap_ 14 5 4 6 4 6 4 */
/*:ref: srotmg_ 14 5 6 6 6 6 6 */
/*:ref: srotm_ 14 6 4 6 4 6 4 6 */
/*:ref: h12_ 14 11 4 4 4 4 6 4 6 6 4 4 4 */
/*:ref: saxpy_ 14 6 4 6 6 4 6 4 */
extern int cfftb1_(integer *n, real *c__, real *ch, real *wa, integer *ifac);
/*:ref: passb4_ 14 7 4 4 6 6 6 6 6 */
/*:ref: passb2_ 14 5 4 4 6 6 6 */
/*:ref: passb3_ 14 6 4 4 6 6 6 6 */
/*:ref: passb5_ 14 8 4 4 6 6 6 6 6 6 */
/*:ref: passb_ 14 11 4 4 4 4 4 6 6 6 6 6 6 */
extern doublereal d9ln2r_(doublereal *x);
/*:ref: d1mach_ 7 1 4 */
/*:ref: initds_ 4 3 7 4 6 */
/*:ref: dcsevl_ 7 3 7 7 4 */
/*:ref: xerror_ 14 5 13 4 4 4 124 */
extern doublereal dgamrn_(doublereal *x);
/*:ref: d1mach_ 7 1 4 */
/*:ref: i1mach_ 4 1 4 */
extern int dqmomo_(doublereal *alfa, doublereal *beta, doublereal *ri, doublereal *rj, doublereal *rg, doublereal *rh, integer *integr);
extern integer icamax_(integer *n, complex *cx, integer *incx);
extern int prwpge_(integer *key, integer *ipage, integer *lpg, real *sx, integer *ix);
/*:ref: xerror_ 14 5 13 4 4 4 124 */
/*:ref: prwvir_ 14 5 4 4 4 6 4 */
extern int sepx4_(integer *iorder, real *a, real *b, integer *m, integer *mbdcnd, real *bda, real *alpha, real *bdb, real *beta, real *c__, real *d__, integer *n, integer *nbdcnd, real *bdc, real *bdd, U_fp cofx, real *grhs, real *usol, integer *idmn, real *w, real *pertrb, integer *ierror);
/*:ref: chkpr4_ 14 12 4 6 6 4 4 6 6 4 4 200 4 4 */
/*:ref: speli4_ 14 34 4 6 6 4 4 6 6 6 6 6 6 4 4 6 6 200 6 6 6 6 6 6 6 6 6 6 6 6 6 6 4 6 6 4 */
extern int wnnls_(real *w, integer *mdw, integer *me, integer *ma, integer *n, integer *l, real *prgopt, real *x, real *rnorm, integer *mode, integer *iwork, real *work);
/*:ref: xerrwv_ 14 11 13 4 4 4 4 4 4 4 6 6 124 */
/*:ref: xerror_ 14 5 13 4 4 4 124 */
/*:ref: wnlsm_ 14 18 6 4 4 4 4 4 6 6 6 4 4 4 6 6 6 6 6 6 */
extern int cfftf_(integer *n, complex *c__, real *wsave);
/*:ref: cfftf1_ 14 5 4 8 6 6 6 */
extern doublereal d9pak_(doublereal *y, integer *n);
/*:ref: i1mach_ 4 1 4 */
/*:ref: d1mach_ 7 1 4 */
/*:ref: d9upak_ 14 3 7 7 4 */
/*:ref: xerror_ 14 5 13 4 4 4 124 */
extern int dgaus8_(D_fp fun, doublereal *a, doublereal *b, doublereal *err, doublereal *ans, integer *ierr);
/*:ref: xerror_ 14 5 13 4 4 4 124 */
/*:ref: d1mach_ 7 1 4 */
/*:ref: i1mach_ 4 1 4 */
extern int dqnc79_(D_fp fun, doublereal *a, doublereal *b, doublereal *err, doublereal *ans, integer *ierr, integer *k);
/*:ref: xerror_ 14 5 13 4 4 4 124 */
/*:ref: d1mach_ 7 1 4 */
/*:ref: i1mach_ 4 1 4 */
extern integer idamax_(integer *n, doublereal *dx, integer *incx);
extern int prwvir_(integer *key, integer *ipage, integer *lpg, real *sx, integer *ix);
/*:ref: sopenm_ 14 2 4 4 */
/*:ref: sreadp_ 14 5 4 4 6 4 4 */
/*:ref: swritp_ 14 5 4 4 6 4 4 */
extern int sgbco_(real *abd, integer *lda, integer *n, integer *ml, integer *mu, integer *ipvt, real *rcond, real *z__);
/*:ref: sasum_ 6 3 4 6 4 */
/*:ref: sgbfa_ 14 7 6 4 4 4 4 4 4 */
/*:ref: sscal_ 14 4 4 6 6 4 */
/*:ref: sdot_ 6 5 4 6 4 6 4 */
/*:ref: saxpy_ 14 6 4 6 6 4 6 4 */
extern int xerabt_(char *messg, integer *nmessg, ftnlen messg_len);
extern int cfftf1_(integer *n, real *c__, real *ch, real *wa, integer *ifac);
/*:ref: passf4_ 14 7 4 4 6 6 6 6 6 */
/*:ref: passf2_ 14 5 4 4 6 6 6 */
/*:ref: passf3_ 14 6 4 4 6 6 6 6 */
/*:ref: passf5_ 14 8 4 4 6 6 6 6 6 6 */
/*:ref: passf_ 14 11 4 4 4 4 4 6 6 6 6 6 6 */
extern int d9upak_(doublereal *x, doublereal *y, integer *n);
extern int dgbco_(doublereal *abd, integer *lda, integer *n, integer *ml, integer *mu, integer *ipvt, doublereal *rcond, doublereal *z__);
/*:ref: dasum_ 7 3 4 7 4 */
/*:ref: dgbfa_ 14 7 7 4 4 4 4 4 4 */
/*:ref: dscal_ 14 4 4 7 7 4 */
/*:ref: ddot_ 7 5 4 7 4 7 4 */
/*:ref: daxpy_ 14 6 4 7 7 4 7 4 */
extern int dqng_(D_fp f, doublereal *a, doublereal *b, doublereal *epsabs, doublereal *epsrel, doublereal *result, doublereal *abserr, integer *neval, integer *ier);
/*:ref: d1mach_ 7 1 4 */
/*:ref: xerror_ 14 5 13 4 4 4 124 */
extern int imtql1_(integer *n, real *d__, real *e, integer *ierr);
/*:ref: pythag_ 6 2 6 6 */
extern E_f psgf_(real *x, integer *iz, real *c__, real *a, real *bh);
extern int sgbdi_(real *abd, integer *lda, integer *n, integer *ml, integer *mu, integer *ipvt, real *det);
extern int xerclr_(void);
/*:ref: j4save_ 4 3 4 4 12 */
extern int cffti_(integer *n, real *wsave);
/*:ref: cffti1_ 14 3 4 6 6 */
extern doublereal dacosh_(doublereal *x);
/*:ref: d1mach_ 7 1 4 */
/*:ref: xerror_ 14 5 13 4 4 4 124 */
extern int dgbdi_(doublereal *abd, integer *lda, integer *n, integer *ml, integer *mu, integer *ipvt, doublereal *det);
extern int dqpsrt_(integer *limit, integer *last, integer *maxerr, doublereal *ermax, doublereal *elist, integer *iord, integer *nrmax);
extern int imtql2_(integer *nm, integer *n, real *d__, real *e, real *z__, integer *ierr);
/*:ref: pythag_ 6 2 6 6 */
extern E_f psi_(real *x);
/*:ref: inits_ 4 3 6 4 6 */
/*:ref: r1mach_ 6 1 4 */
/*:ref: csevl_ 6 3 6 6 4 */
/*:ref: xerror_ 14 5 13 4 4 4 124 */
/*:ref: cot_ 6 1 6 */
extern int sgbfa_(real *abd, integer *lda, integer *n, integer *ml, integer *mu, integer *ipvt, integer *info);
/*:ref: isamax_ 4 3 4 6 4 */
/*:ref: sscal_ 14 4 4 6 6 4 */
/*:ref: saxpy_ 14 6 4 6 6 4 6 4 */
extern int xerctl_(char *messg1, integer *nmessg, integer *nerr, integer *level, integer *kontrl, ftnlen messg1_len);
extern int cffti1_(integer *n, real *wa, integer *ifac);
extern doublereal dai_(doublereal *x);
/*:ref: initds_ 4 3 7 4 6 */
/*:ref: d1mach_ 7 1 4 */
/*:ref: d9aimp_ 14 3 7 7 7 */
/*:ref: dcsevl_ 7 3 7 7 4 */
/*:ref: daie_ 7 1 7 */
/*:ref: xerror_ 14 5 13 4 4 4 124 */
extern int dgbfa_(doublereal *abd, integer *lda, integer *n, integer *ml, integer *mu, integer *ipvt, integer *info);
/*:ref: idamax_ 4 3 4 7 4 */
/*:ref: dscal_ 14 4 4 7 7 4 */
/*:ref: daxpy_ 14 6 4 7 7 4 7 4 */
extern int dqrdc_(doublereal *x, integer *ldx, integer *n, integer *p, doublereal *qraux, integer *jpvt, doublereal *work, integer *job);
/*:ref: dswap_ 14 5 4 7 4 7 4 */
/*:ref: dnrm2_ 7 3 4 7 4 */
/*:ref: dscal_ 14 4 4 7 7 4 */
/*:ref: ddot_ 7 5 4 7 4 7 4 */
/*:ref: daxpy_ 14 6 4 7 7 4 7 4 */
extern int imtqlv_(integer *n, real *d__, real *e, real *e2, real *w, integer *ind, integer *ierr, real *rv1);
/*:ref: pythag_ 6 2 6 6 */
extern int psifn_(real *x, integer *n, integer *kode, integer *m, real *ans, integer *nz, integer *ierr);
/*:ref: i1mach_ 4 1 4 */
/*:ref: r1mach_ 6 1 4 */
extern int sgbsl_(real *abd, integer *lda, integer *n, integer *ml, integer *mu, integer *ipvt, real *b, integer *job);
/*:ref: saxpy_ 14 6 4 6 6 4 6 4 */
/*:ref: sdot_ 6 5 4 6 4 6 4 */
extern int xerdmp_(void);
/*:ref: xersav_ 14 6 13 4 4 4 4 124 */
extern int cfod_(integer *meth, real *elco, real *tesco);
extern doublereal daie_(doublereal *x);
/*:ref: d1mach_ 7 1 4 */
/*:ref: initds_ 4 3 7 4 6 */
/*:ref: d9aimp_ 14 3 7 7 7 */
/*:ref: dcsevl_ 7 3 7 7 4 */
extern int dgbsl_(doublereal *abd, integer *lda, integer *n, integer *ml, integer *mu, integer *ipvt, doublereal *b, integer *job);
/*:ref: daxpy_ 14 6 4 7 7 4 7 4 */
/*:ref: ddot_ 7 5 4 7 4 7 4 */
extern int dqrfac_(integer *m, integer *n, doublereal *a, integer *lda, logical *pivot, integer *ipvt, integer *lipvt, doublereal *sigma, doublereal *acnorm, doublereal *wa);
/*:ref: d1mach_ 7 1 4 */
/*:ref: denorm_ 7 2 4 7 */
extern int indxa_(integer *i__, integer *ir, integer *idxa, integer *na);
/* comlen cblkt_ 28 */
extern E_f psixn_(integer *n);
/*:ref: r1mach_ 6 1 4 */
extern int sgeco_(real *a, integer *lda, integer *n, integer *ipvt, real *rcond, real *z__);
/*:ref: sasum_ 6 3 4 6 4 */
/*:ref: sgefa_ 14 5 6 4 4 4 4 */
/*:ref: sscal_ 14 4 4 6 6 4 */
/*:ref: sdot_ 6 5 4 6 4 6 4 */
/*:ref: saxpy_ 14 6 4 6 6 4 6 4 */
extern int xermax_(integer *max__);
/*:ref: j4save_ 4 3 4 4 12 */
extern int cg_(integer *nm, integer *n, real *ar, real *ai, real *wr, real *wi, integer *matz, real *zr, real *zi, real *fv1, real *fv2, real *fv3, integer *ierr);
/*:ref: cbal_ 14 7 4 4 6 6 4 4 6 */
/*:ref: corth_ 14 8 4 4 4 4 6 6 6 6 */
/*:ref: comqr_ 14 9 4 4 4 4 6 6 6 6 4 */
/*:ref: comqr2_ 14 13 4 4 4 4 6 6 6 6 6 6 6 6 4 */
/*:ref: cbabk2_ 14 8 4 4 4 4 6 4 6 6 */
extern doublereal dasinh_(doublereal *x);
/*:ref: initds_ 4 3 7 4 6 */
/*:ref: d1mach_ 7 1 4 */
/*:ref: dcsevl_ 7 3 7 7 4 */
extern int dgeco_(doublereal *a, integer *lda, integer *n, integer *ipvt, doublereal *rcond, doublereal *z__);
/*:ref: dasum_ 7 3 4 7 4 */
/*:ref: dgefa_ 14 5 7 4 4 4 4 */
/*:ref: dscal_ 14 4 4 7 7 4 */
/*:ref: ddot_ 7 5 4 7 4 7 4 */
/*:ref: daxpy_ 14 6 4 7 7 4 7 4 */
extern int dqrsl_(doublereal *x, integer *ldx, integer *n, integer *k, doublereal *qraux, doublereal *y, doublereal *qy, doublereal *qty, doublereal *b, doublereal *rsd, doublereal *xb, integer *job, integer *info);
/*:ref: dcopy_ 14 5 4 7 4 7 4 */
/*:ref: ddot_ 7 5 4 7 4 7 4 */
/*:ref: daxpy_ 14 6 4 7 7 4 7 4 */
extern int indxb_(integer *i__, integer *ir, integer *idx, integer *idp);
/* comlen cblkt_ 28 */
extern int pvalue_(integer *l, integer *nder, real *x, real *yfit, real *yp, real *a);
/*:ref: xerrwv_ 14 11 13 4 4 4 4 4 4 4 6 6 124 */
/*:ref: xerror_ 14 5 13 4 4 4 124 */
extern int sgedi_(real *a, integer *lda, integer *n, integer *ipvt, real *det, real *work, integer *job);
/*:ref: sscal_ 14 4 4 6 6 4 */
/*:ref: saxpy_ 14 6 4 6 6 4 6 4 */
/*:ref: sswap_ 14 5 4 6 4 6 4 */
extern int xerprt_(char *messg, integer *nmessg, ftnlen messg_len);
/*:ref: xgetua_ 14 2 4 4 */
/*:ref: i1mach_ 4 1 4 */
extern C_f cgamma_(complex * ret_val, complex *z__);
/*:ref: clngam_ 8 2 8 8 */
extern doublereal dasum_(integer *n, doublereal *dx, integer *incx);
extern int dgedi_(doublereal *a, integer *lda, integer *n, integer *ipvt, doublereal *det, doublereal *work, integer *job);
/*:ref: dscal_ 14 4 4 7 7 4 */
/*:ref: daxpy_ 14 6 4 7 7 4 7 4 */
/*:ref: dswap_ 14 5 4 7 4 7 4 */
extern int dqrslv_(integer *n, doublereal *r__, integer *ldr, integer *ipvt, doublereal *diag, doublereal *qtb, doublereal *x, doublereal *sigma, doublereal *wa);
extern int indxc_(integer *i__, integer *ir, integer *idxc, integer *nc);
/* comlen cblkt_ 28 */
extern E_f pythag_(real *a, real *b);
extern int sgeev_(real *a, integer *lda, integer *n, real *e, real *v, integer *ldv, real *work, integer *job, integer *info);
/*:ref: xerror_ 14 5 13 4 4 4 124 */
/*:ref: scopy_ 14 5 4 6 4 6 4 */
/*:ref: balanc_ 14 6 4 4 6 4 4 6 */
/*:ref: orthes_ 14 6 4 4 4 4 6 6 */
/*:ref: hqr_ 14 8 4 4 4 4 6 6 6 4 */
/*:ref: ortran_ 14 7 4 4 4 4 6 6 6 */
/*:ref: hqr2_ 14 9 4 4 4 4 6 6 6 6 4 */
/*:ref: balbak_ 14 7 4 4 4 4 6 4 6 */
/*:ref: scopym_ 14 5 4 6 4 6 4 */
extern int xerror_(char *messg, integer *nmessg, integer *nerr, integer *level, ftnlen messg_len);
/*:ref: xerrwv_ 14 11 13 4 4 4 4 4 4 4 6 6 124 */
extern C_f cgamr_(complex * ret_val, complex *z__);
/*:ref: xgetf_ 14 1 4 */
/*:ref: xsetf_ 14 1 4 */
/*:ref: clngam_ 8 2 8 8 */
/*:ref: xerclr_ 14 0 */
extern int dasyik_(doublereal *x, doublereal *fnu, integer *kode, doublereal *flgik, doublereal *ra, doublereal *arg, integer *in, doublereal *y);
/*:ref: d1mach_ 7 1 4 */
extern int dgefa_(doublereal *a, integer *lda, integer *n, integer *ipvt, integer *info);
/*:ref: idamax_ 4 3 4 7 4 */
/*:ref: dscal_ 14 4 4 7 7 4 */
/*:ref: daxpy_ 14 6 4 7 7 4 7 4 */
extern doublereal dqwgtc_(doublereal *x, doublereal *c__, doublereal *p2, doublereal *p3, doublereal *p4, integer *kp);
extern integer initds_(doublereal *dos, integer *nos, real *eta);
/*:ref: xerror_ 14 5 13 4 4 4 124 */
extern int qag_(E_fp f, real *a, real *b, real *epsabs, real *epsrel, integer *key, real *result, real *abserr, integer *neval, integer *ier, integer *limit, integer *lenw, integer *last, integer *iwork, real *work);
/*:ref: qage_ 14 17 206 6 6 6 6 4 4 6 6 4 4 6 6 6 6 4 4 */
/*:ref: xerror_ 14 5 13 4 4 4 124 */
extern int sgefa_(real *a, integer *lda, integer *n, integer *ipvt, integer *info);
/*:ref: isamax_ 4 3 4 6 4 */
/*:ref: sscal_ 14 4 4 6 6 4 */
/*:ref: saxpy_ 14 6 4 6 6 4 6 4 */
extern int xerrwv_(char *messg, integer *nmessg, integer *nerr, integer *level, integer *ni, integer *i1, integer *i2, integer *nr, real *r1, real *r2, ftnlen messg_len);
/*:ref: j4save_ 4 3 4 4 12 */
/*:ref: xerprt_ 14 3 13 4 124 */
/*:ref: fdump_ 14 0 */
/*:ref: xersav_ 14 6 13 4 4 4 4 124 */
/*:ref: xerabt_ 14 3 13 4 124 */
/*:ref: xerctl_ 14 6 13 4 4 4 4 124 */
/*:ref: xgetua_ 14 2 4 4 */
/*:ref: i1mach_ 4 1 4 */
extern int cgbco_(complex *abd, integer *lda, integer *n, integer *ml, integer *mu, integer *ipvt, real *rcond, complex *z__);
/*:ref: scasum_ 6 3 4 8 4 */
/*:ref: cgbfa_ 14 7 8 4 4 4 4 4 4 */
/*:ref: csscal_ 14 4 4 6 8 4 */
/*:ref: cdotc_ 8 6 8 4 8 4 8 4 */
/*:ref: caxpy_ 14 6 4 8 8 4 8 4 */
extern int dasyjy_(S_fp funjy, doublereal *x, doublereal *fnu, doublereal *flgjy, integer *in, doublereal *y, doublereal *wk, integer *iflw);
/*:ref: d1mach_ 7 1 4 */
/*:ref: i1mach_ 4 1 4 */
extern int dgefs_(doublereal *a, integer *lda, integer *n, doublereal *v, integer *itask, integer *ind, doublereal *work, integer *iwork);
/*:ref: dgeco_ 14 6 7 4 4 4 7 7 */
/*:ref: d1mach_ 7 1 4 */
/*:ref: xerror_ 14 5 13 4 4 4 124 */
/*:ref: dgesl_ 14 6 7 4 4 4 7 4 */
/*:ref: xerrwv_ 14 11 13 4 4 4 4 4 4 4 4 4 124 */
extern doublereal dqwgtf_(doublereal *x, doublereal *omega, doublereal *p2, doublereal *p3, doublereal *p4, integer *integr);
extern integer inits_(real *os, integer *nos, real *eta);
/*:ref: xerror_ 14 5 13 4 4 4 124 */
extern int qage_(E_fp f, real *a, real *b, real *epsabs, real *epsrel, integer *key, integer *limit, real *result, real *abserr, integer *neval, integer *ier, real *alist__, real *blist, real *rlist, real *elist, integer *iord, integer *last);
/*:ref: r1mach_ 6 1 4 */
/*:ref: qk15_ 14 7 206 6 6 6 6 6 6 */
/*:ref: qk21_ 14 7 206 6 6 6 6 6 6 */
/*:ref: qk31_ 14 7 206 6 6 6 6 6 6 */
/*:ref: qk41_ 14 7 206 6 6 6 6 6 6 */
/*:ref: qk51_ 14 7 206 6 6 6 6 6 6 */
/*:ref: qk61_ 14 7 206 6 6 6 6 6 6 */
/*:ref: qpsrt_ 14 7 4 4 4 6 6 4 4 */
extern int sgefs_(real *a, integer *lda, integer *n, real *v, integer *itask, integer *ind, real *work, integer *iwork);
/*:ref: sgeco_ 14 6 6 4 4 4 6 6 */
/*:ref: r1mach_ 6 1 4 */
/*:ref: xerror_ 14 5 13 4 4 4 124 */
/*:ref: sgesl_ 14 6 6 4 4 4 6 4 */
/*:ref: xerrwv_ 14 11 13 4 4 4 4 4 4 4 4 4 124 */
extern int xersav_(char *messg, integer *nmessg, integer *nerr, integer *level, integer *icount, ftnlen messg_len);
/*:ref: xgetua_ 14 2 4 4 */
/*:ref: i1mach_ 4 1 4 */
extern int cgbdi_(complex *abd, integer *lda, integer *n, integer *ml, integer *mu, integer *ipvt, complex *det);
extern doublereal datanh_(doublereal *x);
/*:ref: initds_ 4 3 7 4 6 */
/*:ref: d1mach_ 7 1 4 */
/*:ref: xerror_ 14 5 13 4 4 4 124 */
/*:ref: dcsevl_ 7 3 7 7 4 */
extern int dgesl_(doublereal *a, integer *lda, integer *n, integer *ipvt, doublereal *b, integer *job);
/*:ref: daxpy_ 14 6 4 7 7 4 7 4 */
/*:ref: ddot_ 7 5 4 7 4 7 4 */
extern doublereal dqwgts_(doublereal *x, doublereal *a, doublereal *b, doublereal *alfa, doublereal *beta, integer *integr);
extern int interv_(real *xt, integer *lxt, real *x, integer *ileft, integer *mflag);
extern int qagi_(E_fp f, real *bound, integer *inf, real *epsabs, real *epsrel, real *result, real *abserr, integer *neval, integer *ier, integer *limit, integer *lenw, integer *last, integer *iwork, real *work);
/*:ref: qagie_ 14 16 206 6 4 6 6 4 6 6 4 4 6 6 6 6 4 4 */
/*:ref: xerror_ 14 5 13 4 4 4 124 */
extern int sgeir_(real *a, integer *lda, integer *n, real *v, integer *itask, integer *ind, real *work, integer *iwork);
/*:ref: scopy_ 14 5 4 6 4 6 4 */
/*:ref: sgefa_ 14 5 6 4 4 4 4 */
/*:ref: sgesl_ 14 6 6 4 4 4 6 4 */
/*:ref: sasum_ 6 3 4 6 4 */
/*:ref: sdsdot_ 6 6 4 6 6 4 6 4 */
/*:ref: r1mach_ 6 1 4 */
/*:ref: xerror_ 14 5 13 4 4 4 124 */
/*:ref: xerrwv_ 14 11 13 4 4 4 4 4 4 4 4 4 124 */
extern int xgetf_(integer *kontrl);
/*:ref: j4save_ 4 3 4 4 12 */
extern int cgbfa_(complex *abd, integer *lda, integer *n, integer *ml, integer *mu, integer *ipvt, integer *info);
/*:ref: icamax_ 4 3 4 8 4 */
/*:ref: cscal_ 14 4 4 8 8 4 */
/*:ref: caxpy_ 14 6 4 8 8 4 8 4 */
extern int davint_(doublereal *x, doublereal *y, integer *n, doublereal *xlo, doublereal *xup, doublereal *ans, integer *ierr);
/*:ref: xerror_ 14 5 13 4 4 4 124 */
extern int dglss_(doublereal *a, integer *mda, integer *m, integer *n, doublereal *b, integer *mdb, integer *nb, doublereal *rnorm, doublereal *work, integer *lw, integer *iwork, integer *liw, integer *info);
/*:ref: dllsia_ 14 20 7 4 4 4 7 4 4 7 7 4 4 4 4 4 7 7 4 4 4 4 */
/*:ref: dulsia_ 14 20 7 4 4 4 7 4 4 7 7 4 4 4 4 4 7 7 4 4 4 4 */
extern doublereal drc_(doublereal *x, doublereal *y, integer *ier);
/*:ref: d1mach_ 7 1 4 */
/*:ref: xerror_ 14 5 13 4 4 4 124 */
extern int intrp_(real *x, real *y, real *xout, real *yout, real *ypout, integer *neqn, integer *kold, real *phi, real *psi);
extern int qagie_(E_fp f, real *bound, integer *inf, real *epsabs, real *epsrel, integer *limit, real *result, real *abserr, integer *neval, integer *ier, real *alist__, real *blist, real *rlist, real *elist, integer *iord, integer *last);
/*:ref: r1mach_ 6 1 4 */
/*:ref: qk15i_ 14 9 206 6 4 6 6 6 6 6 6 */
/*:ref: qpsrt_ 14 7 4 4 4 6 6 4 4 */
/*:ref: qelg_ 14 6 4 6 6 6 6 4 */
extern int sgemm_(integer *m, integer *n, integer *l, real *a, integer *ia, real *b, integer *ib, real *c__, integer *ic, integer *jtrpos, integer *job);
/*:ref: sgemv_ 14 9 4 4 6 4 6 4 6 4 4 */
extern int xgetua_(integer *iunita, integer *n);
/*:ref: j4save_ 4 3 4 4 12 */
extern int cgbsl_(complex *abd, integer *lda, integer *n, integer *ml, integer *mu, integer *ipvt, complex *b, integer *job);
/*:ref: caxpy_ 14 6 4 8 8 4 8 4 */
/*:ref: cdotc_ 8 6 8 4 8 4 8 4 */
extern E_f daws_(real *x);
/*:ref: r1mach_ 6 1 4 */
/*:ref: inits_ 4 3 6 4 6 */
/*:ref: csevl_ 6 3 6 6 4 */
/*:ref: xerror_ 14 5 13 4 4 4 124 */
extern int dgtsl_(integer *n, doublereal *c__, doublereal *d__, doublereal *e, doublereal *b, integer *info);
extern doublereal drd_(doublereal *x, doublereal *y, doublereal *z__, integer *ier);
/*:ref: d1mach_ 7 1 4 */
/*:ref: xerror_ 14 5 13 4 4 4 124 */
extern int intrv_(real *xt, integer *lxt, real *x, integer *ilo, integer *ileft, integer *mflag);
extern int qagp_(E_fp f, real *a, real *b, integer *npts2, real *points, real *epsabs, real *epsrel, real *result, real *abserr, integer *neval, integer *ier, integer *leniw, integer *lenw, integer *last, integer *iwork, real *work);
/*:ref: qagpe_ 14 21 206 6 6 4 6 6 6 4 6 6 4 4 6 6 6 6 6 4 4 4 4 */
/*:ref: xerror_ 14 5 13 4 4 4 124 */
extern int sgemv_(integer *m, integer *n, real *a, integer *ia, real *x, integer *ix, real *y, integer *iy, integer *job);
/*:ref: saxpy_ 14 6 4 6 6 4 6 4 */
extern int xgetun_(integer *iunit);
/*:ref: j4save_ 4 3 4 4 12 */
extern int cgeco_(complex *a, integer *lda, integer *n, integer *ipvt, real *rcond, complex *z__);
/*:ref: scasum_ 6 3 4 8 4 */
/*:ref: cgefa_ 14 5 8 4 4 4 4 */
/*:ref: csscal_ 14 4 4 6 8 4 */
/*:ref: cdotc_ 8 6 8 4 8 4 8 4 */
/*:ref: caxpy_ 14 6 4 8 8 4 8 4 */
extern int daxpy_(integer *n, doublereal *da, doublereal *dx, integer *incx, doublereal *dy, integer *incy);
extern int dh12_(integer *mode, integer *lpivot, integer *l1, integer *m, doublereal *u, integer *iue, doublereal *up, doublereal *c__, integer *ice, integer *icv, integer *ncv);
/*:ref: dswap_ 14 5 4 7 4 7 4 */
/*:ref: ddot_ 6 5 4 7 4 7 4 */
/*:ref: daxpy_ 14 6 4 7 7 4 7 4 */
extern int dreadp_(integer *ipage, integer *list, doublereal *rlist, integer *lpage, integer *irec);
/*:ref: xerrwv_ 14 11 13 4 4 4 4 4 4 4 7 7 124 */
extern int intyd_(real *t, integer *k, real *yh, integer *nyh, real *dky, integer *iflag);
/* comlen debdf1_ 1004 */
extern int qagpe_(E_fp f, real *a, real *b, integer *npts2, real *points, real *epsabs, real *epsrel, integer *limit, real *result, real *abserr, integer *neval, integer *ier, real *alist__, real *blist, real *rlist, real *elist, real *pts, integer *iord, integer *level, integer *ndin, integer *last);
/*:ref: r1mach_ 6 1 4 */
/*:ref: qk21_ 14 7 206 6 6 6 6 6 6 */
/*:ref: qpsrt_ 14 7 4 4 4 6 6 4 4 */
/*:ref: qelg_ 14 6 4 6 6 6 6 4 */
extern int sgesl_(real *a, integer *lda, integer *n, integer *ipvt, real *b, integer *job);
/*:ref: saxpy_ 14 6 4 6 6 4 6 4 */
/*:ref: sdot_ 6 5 4 6 4 6 4 */
extern int xsetf_(integer *kontrl);
/*:ref: xerrwv_ 14 11 13 4 4 4 4 4 4 4 6 6 124 */
/*:ref: j4save_ 4 3 4 4 12 */
extern int cgedi_(complex *a, integer *lda, integer *n, integer *ipvt, complex *det, complex *work, integer *job);
/*:ref: cscal_ 14 4 4 8 8 4 */
/*:ref: caxpy_ 14 6 4 8 8 4 8 4 */
/*:ref: cswap_ 14 5 4 8 4 8 4 */
extern int dbdiff_(integer *l, doublereal *v);
extern int dhfti_(doublereal *a, integer *mda, integer *m, integer *n, doublereal *b, integer *mdb, integer *nb, doublereal *tau, integer *krank, doublereal *rnorm, doublereal *h__, doublereal *g, integer *ip);
/*:ref: xerror_ 14 5 13 4 4 4 124 */
/*:ref: ddiff_ 7 2 7 7 */
/*:ref: dh12_ 14 11 4 4 4 4 7 4 7 7 4 4 4 */
extern int dreort_(integer *ncomp, doublereal *y, doublereal *yp, doublereal *yhp, integer *niv, doublereal *w, doublereal *s, doublereal *p, integer *ip, doublereal *stowa, integer *iflag);
/* comlen dml8sz_ 36 */
/* comlen dml15t_ 148 */
/* comlen dml18j_ 84 */
/*:ref: dstor1_ 14 7 7 7 7 7 4 4 4 */
/*:ref: dmgsbv_ 14 13 4 4 7 4 4 4 7 7 4 4 7 7 7 */
/*:ref: dstway_ 14 5 7 7 7 4 7 */
/*:ref: ddot_ 7 5 4 7 4 7 4 */
extern int invit_(integer *nm, integer *n, real *a, real *wr, real *wi, logical *select, integer *mm, integer *m, real *z__, integer *ierr, real *rm1, real *rv1, real *rv2);
/*:ref: cdiv_ 14 6 6 6 6 6 6 6 */
/*:ref: pythag_ 6 2 6 6 */
extern int qags_(E_fp f, real *a, real *b, real *epsabs, real *epsrel, real *result, real *abserr, integer *neval, integer *ier, integer *limit, integer *lenw, integer *last, integer *iwork, real *work);
/*:ref: qagse_ 14 16 206 6 6 6 6 4 6 6 4 4 6 6 6 6 4 4 */
/*:ref: xerror_ 14 5 13 4 4 4 124 */
extern int sglss_(real *a, integer *mda, integer *m, integer *n, real *b, integer *mdb, integer *nb, real *rnorm, real *work, integer *lw, integer *iwork, integer *liw, integer *info);
/*:ref: llsia_ 14 20 6 4 4 4 6 4 4 6 6 4 4 4 4 4 6 6 4 4 4 4 */
/*:ref: ulsia_ 14 20 6 4 4 4 6 4 4 6 6 4 4 4 4 4 6 6 4 4 4 4 */
extern int xsetua_(integer *iunita, integer *n);
/*:ref: xerrwv_ 14 11 13 4 4 4 4 4 4 4 6 6 124 */
/*:ref: j4save_ 4 3 4 4 12 */
extern int cgeev_(real *a, integer *lda, integer *n, real *e, real *v, integer *ldv, real *work, integer *job, integer *info);
/*:ref: xerror_ 14 5 13 4 4 4 124 */
/*:ref: scopy_ 14 5 4 6 4 6 4 */
/*:ref: cbal_ 14 7 4 4 6 6 4 4 6 */
/*:ref: corth_ 14 8 4 4 4 4 6 6 6 6 */
/*:ref: comqr_ 14 9 4 4 4 4 6 6 6 6 4 */
/*:ref: comqr2_ 14 13 4 4 4 4 6 6 6 6 6 6 6 6 4 */
/*:ref: cbabk2_ 14 8 4 4 4 4 6 4 6 6 */
extern int dbesi_(doublereal *x, doublereal *alpha, integer *kode, integer *n, doublereal *y, integer *nz);
/*:ref: d1mach_ 7 1 4 */
/*:ref: i1mach_ 4 1 4 */
/*:ref: dasyik_ 14 8 7 7 4 7 7 7 4 7 */
/*:ref: dlngam_ 7 1 7 */
/*:ref: xerror_ 14 5 13 4 4 4 124 */
extern int dhkseq_(doublereal *x, integer *m, doublereal *h__, integer *ierr);
/*:ref: d1mach_ 7 1 4 */
/*:ref: i1mach_ 4 1 4 */
extern doublereal drf_(doublereal *x, doublereal *y, doublereal *z__, integer *ier);
/*:ref: d1mach_ 7 1 4 */
/*:ref: xerror_ 14 5 13 4 4 4 124 */
extern int inxca_(integer *i__, integer *ir, integer *idxa, integer *na);
/* comlen ccblk_ 28 */
extern int qagse_(E_fp f, real *a, real *b, real *epsabs, real *epsrel, integer *limit, real *result, real *abserr, integer *neval, integer *ier, real *alist__, real *blist, real *rlist, real *elist, integer *iord, integer *last);
/*:ref: r1mach_ 6 1 4 */
/*:ref: qk21_ 14 7 206 6 6 6 6 6 6 */
/*:ref: qpsrt_ 14 7 4 4 4 6 6 4 4 */
/*:ref: qelg_ 14 6 4 6 6 6 6 4 */
extern int sgtsl_(integer *n, real *c__, real *d__, real *e, real *b, integer *info);
extern int xsetun_(integer *iunit);
/*:ref: j4save_ 4 3 4 4 12 */
extern int cgefa_(complex *a, integer *lda, integer *n, integer *ipvt, integer *info);
/*:ref: icamax_ 4 3 4 8 4 */
/*:ref: cscal_ 14 4 4 8 8 4 */
/*:ref: caxpy_ 14 6 4 8 8 4 8 4 */
extern doublereal dbesi0_(doublereal *x);
/*:ref: initds_ 4 3 7 4 6 */
/*:ref: d1mach_ 7 1 4 */
/*:ref: dcsevl_ 7 3 7 7 4 */
/*:ref: xerror_ 14 5 13 4 4 4 124 */
/*:ref: dbsi0e_ 7 1 7 */
extern int dhstrt_(S_fp df, integer *neq, doublereal *a, doublereal *b, doublereal *y, doublereal *yprime, doublereal *etol, integer *morder, doublereal *small, doublereal *big, doublereal *spy, doublereal *pv, doublereal *yp, doublereal *sf, doublereal *rpar, integer *ipar, doublereal *h__);
/*:ref: dvnorm_ 7 2 7 4 */
extern doublereal drj_(doublereal *x, doublereal *y, doublereal *z__, doublereal *p, integer *ier);
/*:ref: d1mach_ 7 1 4 */
/*:ref: xerror_ 14 5 13 4 4 4 124 */
/*:ref: drc_ 7 3 7 7 4 */
extern int inxcb_(integer *i__, integer *ir, integer *idx, integer *idp);
/* comlen ccblk_ 28 */
extern int qawc_(E_fp f, real *a, real *b, real *c__, real *epsabs, real *epsrel, real *result, real *abserr, integer *neval, integer *ier, integer *limit, integer *lenw, integer *last, integer *iwork, real *work);
/*:ref: qawce_ 14 17 206 6 6 6 6 6 4 6 6 4 4 6 6 6 6 4 4 */
/*:ref: xerror_ 14 5 13 4 4 4 124 */
extern E_f sindg_(real *x);
extern int yairy_(real *x, real *rx, real *c__, real *bi, real *dbi);
extern int cgefs_(complex *a, integer *lda, integer *n, complex *v, integer *itask, integer *ind, complex *work, integer *iwork);
/*:ref: cgeco_ 14 6 8 4 4 4 6 8 */
/*:ref: r1mach_ 6 1 4 */
/*:ref: xerror_ 14 5 13 4 4 4 124 */
/*:ref: cgesl_ 14 6 8 4 4 4 8 4 */
/*:ref: xerrwv_ 14 11 13 4 4 4 4 4 4 4 4 4 124 */
extern doublereal dbesi1_(doublereal *x);
/*:ref: initds_ 4 3 7 4 6 */
/*:ref: d1mach_ 7 1 4 */
/*:ref: xerror_ 14 5 13 4 4 4 124 */
/*:ref: dcsevl_ 7 3 7 7 4 */
/*:ref: dbsi1e_ 7 1 7 */
extern E_f diff_(real *x, real *y);
extern int drkfab_(integer *ncomp, doublereal *xpts, integer *nxpts, integer *nfc, integer *iflag, doublereal *z__, integer *mxnon, doublereal *p, integer *ntp, integer *ip, doublereal *yhp, integer *niv, doublereal *u, doublereal *v, doublereal *w, doublereal *s, doublereal *stowa, doublereal *g, doublereal *work, integer *iwork, integer *nfcc);
/* comlen dml8sz_ 36 */
/* comlen dml15t_ 148 */
/* comlen dml18j_ 84 */
/* comlen dml17b_ 72 */
/*:ref: dderkf_ 14 15 200 4 7 7 7 4 7 7 4 7 4 4 4 7 4 */
/*:ref: ddeabm_ 14 15 200 4 7 7 7 4 7 7 4 7 4 4 4 7 4 */
/*:ref: dreort_ 14 11 4 7 7 7 4 7 7 7 4 7 4 */
/*:ref: dstor1_ 14 7 7 7 7 7 4 4 4 */
extern int inxcc_(integer *i__, integer *ir, integer *idxc, integer *nc);
/* comlen ccblk_ 28 */
extern int qawce_(E_fp f, real *a, real *b, real *c__, real *epsabs, real *epsrel, integer *limit, real *result, real *abserr, integer *neval, integer *ier, real *alist__, real *blist, real *rlist, real *elist, integer *iord, integer *last);
/*:ref: r1mach_ 6 1 4 */
/*:ref: qc25c_ 14 8 206 6 6 6 6 6 4 4 */
/*:ref: qpsrt_ 14 7 4 4 4 6 6 4 4 */
extern int sinqb_(integer *n, real *x, real *wsave);
/*:ref: cosqb_ 14 3 4 6 6 */
