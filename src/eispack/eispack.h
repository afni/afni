#ifndef _MCW_EISPACK_HEADER_
#define _MCW_EISPACK_HEADER_

#include "converted_from_fortran.h"

/*---------------------- prototypes from "f2c -P"   ---------------------------*/

extern int bakvec_(integer *nm, integer *n, doublereal *t, doublereal *e, integer *m, doublereal *z__, integer *ierr);
extern int balanc_(integer *nm, integer *n, doublereal *a, integer *low, integer *igh, doublereal *scale);
extern int balbak_(integer *nm, integer *n, integer *low, integer *igh, doublereal *scale, integer *m, doublereal *z__);
extern int bandr_(integer *nm, integer *n, integer *mb, doublereal *a, doublereal *d__, doublereal *e, doublereal *e2, logical *matz, doublereal *z__);
extern int bandv_(integer *nm, integer *n, integer *mbw, doublereal *a, doublereal *e21, integer *m, doublereal *w, doublereal *z__, integer *ierr, integer *nv, doublereal *rv, doublereal *rv6);
extern int bisect_(integer *n, doublereal *eps1, doublereal *d__, doublereal *e, doublereal *e2, doublereal *lb, doublereal *ub, integer *mm, integer *m, doublereal *w, integer *ind, integer *ierr, doublereal *rv4, doublereal *rv5);
extern int bqr_(integer *nm, integer *n, integer *mb, doublereal *a, doublereal *t, doublereal *r__, integer *ierr, integer *nv, doublereal *rv);
extern int cbabk2_(integer *nm, integer *n, integer *low, integer *igh, doublereal *scale, integer *m, doublereal *zr, doublereal *zi);
extern int cbal_(integer *nm, integer *n, doublereal *ar, doublereal *ai, integer *low, integer *igh, doublereal *scale);
extern int cdiv_(doublereal *ar, doublereal *ai, doublereal *br, doublereal *bi, doublereal *cr, doublereal *ci);
extern int cg_(integer *nm, integer *n, doublereal *ar, doublereal *ai, doublereal *wr, doublereal *wi, integer *matz, doublereal *zr, doublereal *zi, doublereal *fv1, doublereal *fv2, doublereal *fv3, integer *ierr);
extern int ch_(integer *nm, integer *n, doublereal *ar, doublereal *ai, doublereal *w, integer *matz, doublereal *zr, doublereal *zi, doublereal *fv1, doublereal *fv2, doublereal *fm1, integer *ierr);
extern int cinvit_(integer *nm, integer *n, doublereal *ar, doublereal *ai, doublereal *wr, doublereal *wi, logical *select, integer *mm, integer *m, doublereal *zr, doublereal *zi, integer *ierr, doublereal *rm1, doublereal *rm2, doublereal *rv1, doublereal *rv2);
extern int combak_(integer *nm, integer *low, integer *igh, doublereal *ar, doublereal *ai, integer *int__, integer *m, doublereal *zr, doublereal *zi);
extern int comhes_(integer *nm, integer *n, integer *low, integer *igh, doublereal *ar, doublereal *ai, integer *int__);
extern int comlr_(integer *nm, integer *n, integer *low, integer *igh, doublereal *hr, doublereal *hi, doublereal *wr, doublereal *wi, integer *ierr);
extern int comlr2_(integer *nm, integer *n, integer *low, integer *igh, integer *int__, doublereal *hr, doublereal *hi, doublereal *wr, doublereal *wi, doublereal *zr, doublereal *zi, integer *ierr);
extern int comqr_(integer *nm, integer *n, integer *low, integer *igh, doublereal *hr, doublereal *hi, doublereal *wr, doublereal *wi, integer *ierr);
extern int comqr2_(integer *nm, integer *n, integer *low, integer *igh, doublereal *ortr, doublereal *orti, doublereal *hr, doublereal *hi, doublereal *wr, doublereal *wi, doublereal *zr, doublereal *zi, integer *ierr);
extern int cortb_(integer *nm, integer *low, integer *igh, doublereal *ar, doublereal *ai, doublereal *ortr, doublereal *orti, integer *m, doublereal *zr, doublereal *zi);
extern int corth_(integer *nm, integer *n, integer *low, integer *igh, doublereal *ar, doublereal *ai, doublereal *ortr, doublereal *orti);
extern int csroot_(doublereal *xr, doublereal *xi, doublereal *yr, doublereal *yi);
extern int elmbak_(integer *nm, integer *low, integer *igh, doublereal *a, integer *int__, integer *m, doublereal *z__);
extern int elmhes_(integer *nm, integer *n, integer *low, integer *igh, doublereal *a, integer *int__);
extern int eltran_(integer *nm, integer *n, integer *low, integer *igh, doublereal *a, integer *int__, doublereal *z__);
extern doublereal epslon_(doublereal *x);
extern int figi_(integer *nm, integer *n, doublereal *t, doublereal *d__, doublereal *e, doublereal *e2, integer *ierr);
extern int figi2_(integer *nm, integer *n, doublereal *t, doublereal *d__, doublereal *e, doublereal *z__, integer *ierr);
extern int hqr_(integer *nm, integer *n, integer *low, integer *igh, doublereal *h__, doublereal *wr, doublereal *wi, integer *ierr);
extern int hqr2_(integer *nm, integer *n, integer *low, integer *igh, doublereal *h__, doublereal *wr, doublereal *wi, doublereal *z__, integer *ierr);
extern int htrib3_(integer *nm, integer *n, doublereal *a, doublereal *tau, integer *m, doublereal *zr, doublereal *zi);
extern int htribk_(integer *nm, integer *n, doublereal *ar, doublereal *ai, doublereal *tau, integer *m, doublereal *zr, doublereal *zi);
extern int htrid3_(integer *nm, integer *n, doublereal *a, doublereal *d__, doublereal *e, doublereal *e2, doublereal *tau);
extern int htridi_(integer *nm, integer *n, doublereal *ar, doublereal *ai, doublereal *d__, doublereal *e, doublereal *e2, doublereal *tau);
extern int imtql1_(integer *n, doublereal *d__, doublereal *e, integer *ierr);
extern int imtql2_(integer *nm, integer *n, doublereal *d__, doublereal *e, doublereal *z__, integer *ierr);
extern int imtqlv_(integer *n, doublereal *d__, doublereal *e, doublereal *e2, doublereal *w, integer *ind, integer *ierr, doublereal *rv1);
extern int invit_(integer *nm, integer *n, doublereal *a, doublereal *wr, doublereal *wi, logical *select, integer *mm, integer *m, doublereal *z__, integer *ierr, doublereal *rm1, doublereal *rv1, doublereal *rv2);
extern int minfit_(integer *nm, integer *m, integer *n, doublereal *a, doublereal *w, integer *ip, doublereal *b, integer *ierr, doublereal *rv1);
extern int ortbak_(integer *nm, integer *low, integer *igh, doublereal *a, doublereal *ort, integer *m, doublereal *z__);
extern int orthes_(integer *nm, integer *n, integer *low, integer *igh, doublereal *a, doublereal *ort);
extern int ortran_(integer *nm, integer *n, integer *low, integer *igh, doublereal *a, doublereal *ort, doublereal *z__);
extern doublereal pythag_(doublereal *a, doublereal *b);
extern int qzhes_(integer *nm, integer *n, doublereal *a, doublereal *b, logical *matz, doublereal *z__);
extern int qzit_(integer *nm, integer *n, doublereal *a, doublereal *b, doublereal *eps1, logical *matz, doublereal *z__, integer *ierr);
extern int qzval_(integer *nm, integer *n, doublereal *a, doublereal *b, doublereal *alfr, doublereal *alfi, doublereal *beta, logical *matz, doublereal *z__);
extern int qzvec_(integer *nm, integer *n, doublereal *a, doublereal *b, doublereal *alfr, doublereal *alfi, doublereal *beta, doublereal *z__);
extern int ratqr_(integer *n, doublereal *eps1, doublereal *d__, doublereal *e, doublereal *e2, integer *m, doublereal *w, integer *ind, doublereal *bd, logical *type__, integer *idef, integer *ierr);
extern int rebak_(integer *nm, integer *n, doublereal *b, doublereal *dl, integer *m, doublereal *z__);
extern int rebakb_(integer *nm, integer *n, doublereal *b, doublereal *dl, integer *m, doublereal *z__);
extern int reduc_(integer *nm, integer *n, doublereal *a, doublereal *b, doublereal *dl, integer *ierr);
extern int reduc2_(integer *nm, integer *n, doublereal *a, doublereal *b, doublereal *dl, integer *ierr);
extern int rg_(integer *nm, integer *n, doublereal *a, doublereal *wr, doublereal *wi, integer *matz, doublereal *z__, integer *iv1, doublereal *fv1, integer *ierr);
extern int rgg_(integer *nm, integer *n, doublereal *a, doublereal *b, doublereal *alfr, doublereal *alfi, doublereal *beta, integer *matz, doublereal *z__, integer *ierr);
extern int rs_(integer *nm, integer *n, doublereal *a, doublereal *w, integer *matz, doublereal *z__, doublereal *fv1, doublereal *fv2, integer *ierr);
extern int rsb_(integer *nm, integer *n, integer *mb, doublereal *a, doublereal *w, integer *matz, doublereal *z__, doublereal *fv1, doublereal *fv2, integer *ierr);
extern int rsg_(integer *nm, integer *n, doublereal *a, doublereal *b, doublereal *w, integer *matz, doublereal *z__, doublereal *fv1, doublereal *fv2, integer *ierr);
extern int rsgab_(integer *nm, integer *n, doublereal *a, doublereal *b, doublereal *w, integer *matz, doublereal *z__, doublereal *fv1, doublereal *fv2, integer *ierr);
extern int rsgba_(integer *nm, integer *n, doublereal *a, doublereal *b, doublereal *w, integer *matz, doublereal *z__, doublereal *fv1, doublereal *fv2, integer *ierr);
extern int rsm_(integer *nm, integer *n, doublereal *a, doublereal *w, integer *m, doublereal *z__, doublereal *fwork, integer *iwork, integer *ierr);
extern int rsp_(integer *nm, integer *n, integer *nv, doublereal *a, doublereal *w, integer *matz, doublereal *z__, doublereal *fv1, doublereal *fv2, integer *ierr);
extern int rst_(integer *nm, integer *n, doublereal *w, doublereal *e, integer *matz, doublereal *z__, integer *ierr);
extern int rt_(integer *nm, integer *n, doublereal *a, doublereal *w, integer *matz, doublereal *z__, doublereal *fv1, integer *ierr);
extern int svd_(integer *m, integer *n, integer *lda, doublereal *a, doublereal *w, logical *matu, integer *ldu, doublereal *u, logical * matv, integer *ldv, doublereal *v, integer *ierr, doublereal *rv1);
extern int svd_slow_(integer *m, integer *n, integer *lda, doublereal *a, doublereal *w, logical *matu, integer *ldu, doublereal *u, logical * matv, integer *ldv, doublereal *v, integer *ierr, doublereal *rv1);
extern int tql1_(integer *n, doublereal *d__, doublereal *e, integer *ierr);
extern int tql2_(integer *nm, integer *n, doublereal *d__, doublereal *e, doublereal *z__, integer *ierr);
extern int tqlrat_(integer *n, doublereal *d__, doublereal *e2, integer *ierr);
extern int trbak1_(integer *nm, integer *n, doublereal *a, doublereal *e, integer *m, doublereal *z__);
extern int trbak3_(integer *nm, integer *n, integer *nv, doublereal *a, integer *m, doublereal *z__);
extern int tred1_(integer *nm, integer *n, doublereal *a, doublereal *d__, doublereal *e, doublereal *e2);
extern int tred2_(integer *nm, integer *n, doublereal *a, doublereal *d__, doublereal *e, doublereal *z__);
extern int tred3_(integer *n, integer *nv, doublereal *a, doublereal *d__, doublereal *e, doublereal *e2);
extern int tridib_(integer *n, doublereal *eps1, doublereal *d__, doublereal *e, doublereal *e2, doublereal *lb, doublereal *ub, integer *m11, integer *m, doublereal *w, integer *ind, integer *ierr, doublereal *rv4, doublereal *rv5);
extern int tsturm_(integer *nm, integer *n, doublereal *eps1, doublereal *d__, doublereal *e, doublereal *e2, doublereal *lb, doublereal *ub, integer *mm, integer *m, doublereal *w, doublereal *z__, integer *ierr, doublereal *rv1, doublereal *rv2, doublereal *rv3, doublereal *rv4, doublereal *rv5, doublereal *rv6);
extern int tinvit_(integer *nm, integer *n, doublereal *d__,
        doublereal *e, doublereal *e2, integer *m, doublereal *w, integer *
        ind, doublereal *z__, integer *ierr, doublereal *rv1, doublereal *rv2,
         doublereal *rv3, doublereal *rv4, doublereal *rv6);


#endif
