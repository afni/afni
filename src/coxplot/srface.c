/* srface.f -- translated by f2c (version 19961017).
   You must link the resulting object file with the libraries:
	-lconverted_from_fortran -lm   (in that order)
*/

#include "converted_from_fortran.h"

/* Common Block Declarations */

struct srfblk_1_ {
    integer limu[1024], liml[1024];
    real cl[41];
    integer ncl, ll;
    real fact;
    integer irot, ndrz, nupper, nrswt;
    real bigd, umin, umax, vmin, vmax, rzero;
    integer ioffp, nspval;
    real spval, bigest;
};

#define srfblk_1 (*(struct srfblk_1_ *) &srfblk_)

struct {
    real xxmin, xxmax, yymin, yymax, zzmin, zzmax, delcrt, eyex, eyey, eyez;
} pwrz1s_;

#define pwrz1s_1 pwrz1s_

struct srfip1_1_ {
    integer ifr, istp, irots, idrx, idry, idrz, iupper, iskirt, ncla;
    real theta, hskirt, chi, clo, cinc;
    integer ispval;
};

#define srfip1_1 (*(struct srfip1_1_ *) &srfip1_)

/* Initialized data */

struct {
    integer fill_1[2095];
    integer e_2;
    integer fill_3[6];
    integer e_4;
    integer fill_5[1];
    real e_6;
    integer fill_7[1];
    } srfblk_ = { {0}, 0, {0}, 0, {0}, 0.f };

struct {
    integer e_1[9];
    real e_2[5];
    integer e_3;
    } srfip1_ = { 1, 0, 0, 1, 1, 0, 0, 0, 6, .02f, 0.f, 0.f, 0.f, 0.f, -999 };


/* Table of constant values */

static real c_b2 = 0.f;
static real c_b3 = 1.f;
static real c_b7 = 1024.f;
static integer c__1 = 1;
static integer c__40 = 40;
static integer c__0 = 0;
static real c_b128 = 10.f;
static integer c__2 = 2;

/* ======================================================================= */



/* Subroutine */ int srface_(real *x, real *y, real *z__, integer *m, integer 
	*mx, integer *nx, integer *ny, real *s, real *stereo)
{
    /* Initialized data */

    static integer jf = 1;
    static integer if__ = 1;
    static integer ly = 2;
    static integer lx = 2;
    static integer icnst = 0;

    /* System generated locals */
    integer z_dim1, z_offset, m_dim2, m_offset, i__1, i__2, i__3, i__4, i__5, 
	    i__6, i__7, i__8, i__9, i__10;

    /* Local variables */
    static integer ipic, npic, ipli, jplj;
    static real ster, poix, poiy, poiz, xeye;
    static integer mmxx, nnxx;
    static real yeye;
    static integer nnyy;
    static real zeye, ynow, xnow, sign1;
    static integer i__, j, k, l;
    extern /* Subroutine */ int frame_(void);
    static real hight;
    extern /* Subroutine */ int clset_(real *, integer *, integer *, integer *
	    , real *, real *, real *, integer *, integer *, real *, integer *,
	     integer *, integer *, real *, real *);
    static real width;
    extern /* Subroutine */ int draws_(integer *, integer *, integer *, 
	    integer *, integer *, integer *);
    static integer jpass, ipass;
    static real d1, d2;
    extern /* Subroutine */ int trn32s_(real *, real *, real *, real *, real *
	    , real *, integer *);
    static real dummy;
    static integer nxstp, nystp, ii, jj, li, mi, in, jn, ni, lj;
    static real dx, dy;
    static integer mj, nj;
    extern /* Subroutine */ int srfabd_(void);
    static real ctheta, rx, ry, rz, ut, vt, qu, qv, ru, zz, rv;
    extern /* Subroutine */ int ctcell_(real *, integer *, integer *, integer 
	    *, integer *, integer *, integer *);
    static real stheta;
    static integer nxpass, nypass;
    static real ux1, vx1, ux2, vx2, uy1, vy1, uy2, vy2, dif, agl;
    static integer nla, mxf[2], myf[2];
    extern /* Subroutine */ int set_(real *, real *, real *, real *, real *, 
	    real *, real *, real *, integer *);
    static integer mxj[2], myj[2], mxs[2], mys[2], nxp1, nyp1;


/*  Surface plotting package from NCAR -- the only high level NCAR */
/*  routine in this library at present (Aug 17, 1990). */

/*cc      DIMENSION       X(NX)      ,Y(NY)      ,Z(MX,NY)   ,M(2,NX,NY) ,
*/
/* cc     1                S(6) */
    /* Parameter adjustments */
    --x;
    m_dim2 = *nx;
    m_offset = (m_dim2 + 1 << 1) + 1;
    m -= m_offset;
    z_dim1 = *mx;
    z_offset = z_dim1 + 1;
    z__ -= z_offset;
    --y;
    --s;

    /* Function Body */
/* +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
 */
    srfabd_();
    set_(&c_b2, &c_b3, &c_b2, &c_b3, &c_b3, &c_b7, &c_b3, &c_b7, &c__1);
    srfblk_1.bigest = 1e38f;
/* CC      BIGEST = R1MACH(2) */
    mmxx = *mx;
    nnxx = *nx;
    nnyy = *ny;
    ster = *stereo;
    nxp1 = nnxx + 1;
    nyp1 = nnyy + 1;
    nla = srfip1_1.ncla;
    srfblk_1.nspval = srfip1_1.ispval;
    srfblk_1.ndrz = srfip1_1.idrz;
    if (srfip1_1.idrz != 0) {
	clset_(&z__[z_offset], &mmxx, &nnxx, &nnyy, &srfip1_1.chi, &
		srfip1_1.clo, &srfip1_1.cinc, &nla, &c__40, srfblk_1.cl, &
		srfblk_1.ncl, &icnst, &srfblk_1.ioffp, &srfblk_1.spval, &
		srfblk_1.bigest);
    }
    if (srfip1_1.idrz != 0) {
	srfblk_1.ndrz = 1 - icnst;
    }
    stheta = sin(ster * srfip1_1.theta);
    ctheta = cos(ster * srfip1_1.theta);
    rx = s[1] - s[4];
    ry = s[2] - s[5];
    rz = s[3] - s[6];
    d1 = sqrt(rx * rx + ry * ry + rz * rz);
    d2 = sqrt(rx * rx + ry * ry);
    dx = 0.f;
    dy = 0.f;
    if (*stereo == 0.f) {
	goto L20;
    }
    d1 = d1 * *stereo * srfip1_1.theta;
    if (d2 > 0.f) {
	goto L10;
    }
    dx = d1;
    goto L20;
L10:
    agl = atan2(rx, -ry);
    dx = d1 * cos(agl);
    dy = d1 * sin(agl);
L20:
    srfblk_1.irot = srfip1_1.irots;
    npic = 1;
    if (ster != 0.f) {
	npic = 2;
    }
    srfblk_1.fact = 1.f;
    if (srfblk_1.nrswt != 0) {
	srfblk_1.fact = srfblk_1.rzero / d1;
    }
    if (srfip1_1.istp == 0 && ster != 0.f) {
	srfblk_1.irot = 1;
    }
    i__1 = npic;
    for (ipic = 1; ipic <= i__1; ++ipic) {
	srfblk_1.nupper = srfip1_1.iupper;
	if (srfip1_1.ifr < 0) {
	    frame_();
	}

/* SET UP MAPING FROM FLOATING POINT 3-SPACE TO CRT SPACE. */

	sign1 = (real) ((ipic << 1) - 3);
	pwrz1s_1.eyex = s[1] + sign1 * dx;
	poix = s[4] + sign1 * dx;
	pwrz1s_1.eyey = s[2] + sign1 * dy;
	poiy = s[5] + sign1 * dy;
	pwrz1s_1.eyez = s[3];
	poiz = s[6];
	srfblk_1.ll = 0;
	xeye = pwrz1s_1.eyex;
	yeye = pwrz1s_1.eyey;
	zeye = pwrz1s_1.eyez;
	trn32s_(&poix, &poiy, &poiz, &xeye, &yeye, &zeye, &c__0);
	srfblk_1.ll = ipic + (srfip1_1.istp << 1) + 3;
	if (ster == 0.f) {
	    srfblk_1.ll = 1;
	}
	if (srfblk_1.nrswt != 0) {
	    goto L100;
	}
	pwrz1s_1.xxmin = x[1];
	pwrz1s_1.xxmax = x[nnxx];
	pwrz1s_1.yymin = y[1];
	pwrz1s_1.yymax = y[nnyy];
	srfblk_1.umin = srfblk_1.bigest;
	srfblk_1.vmin = srfblk_1.bigest;
	pwrz1s_1.zzmin = srfblk_1.bigest;
	srfblk_1.umax = -srfblk_1.umin;
	srfblk_1.vmax = -srfblk_1.vmin;
	pwrz1s_1.zzmax = -pwrz1s_1.zzmin;
	i__2 = nnyy;
	for (j = 1; j <= i__2; ++j) {
	    i__3 = nnxx;
	    for (i__ = 1; i__ <= i__3; ++i__) {
		zz = z__[i__ + j * z_dim1];
		if (srfblk_1.ioffp == 1 && zz == srfblk_1.spval) {
		    goto L30;
		}
		pwrz1s_1.zzmax = dmax(pwrz1s_1.zzmax,zz);
		pwrz1s_1.zzmin = dmin(pwrz1s_1.zzmin,zz);
		trn32s_(&x[i__], &y[j], &z__[i__ + j * z_dim1], &ut, &vt, &
			dummy, &c__1);
		srfblk_1.umax = dmax(srfblk_1.umax,ut);
		srfblk_1.umin = dmin(srfblk_1.umin,ut);
		srfblk_1.vmax = dmax(srfblk_1.vmax,vt);
		srfblk_1.vmin = dmin(srfblk_1.vmin,vt);
L30:
		;
	    }
/* L40: */
	}
	if (srfip1_1.iskirt != 1) {
	    goto L70;
	}
	nxstp = nnxx - 1;
	nystp = nnyy - 1;
	i__2 = nnyy;
	i__3 = nystp;
	for (j = 1; i__3 < 0 ? j >= i__2 : j <= i__2; j += i__3) {
	    i__4 = nnxx;
	    i__5 = nxstp;
	    for (i__ = 1; i__5 < 0 ? i__ >= i__4 : i__ <= i__4; i__ += i__5) {
		trn32s_(&x[i__], &y[j], &srfip1_1.hskirt, &ut, &vt, &dummy, &
			c__1);
		srfblk_1.umax = dmax(srfblk_1.umax,ut);
		srfblk_1.umin = dmin(srfblk_1.umin,ut);
		srfblk_1.vmax = dmax(srfblk_1.vmax,vt);
		srfblk_1.vmin = dmin(srfblk_1.vmin,vt);
/* L50: */
	    }
/* L60: */
	}
L70:
	width = srfblk_1.umax - srfblk_1.umin;
	hight = srfblk_1.vmax - srfblk_1.vmin;
	dif = (width - hight) * .5f;
	if (dif < 0.f) {
	    goto L80;
	} else if (dif == 0) {
	    goto L100;
	} else {
	    goto L90;
	}
L80:
	srfblk_1.umin += dif;
	srfblk_1.umax -= dif;
	goto L100;
L90:
	srfblk_1.vmin -= dif;
	srfblk_1.vmax += dif;
L100:
	xeye = pwrz1s_1.eyex;
	yeye = pwrz1s_1.eyey;
	zeye = pwrz1s_1.eyez;
	trn32s_(&poix, &poiy, &poiz, &xeye, &yeye, &zeye, &c__0);
	i__3 = nnyy;
	for (j = 1; j <= i__3; ++j) {
	    i__2 = nnxx;
	    for (i__ = 1; i__ <= i__2; ++i__) {
		trn32s_(&x[i__], &y[j], &z__[i__ + j * z_dim1], &ut, &vt, &
			dummy, &c__1);
		m[(i__ + j * m_dim2 << 1) + 1] = ut;
		m[(i__ + j * m_dim2 << 1) + 2] = vt;
/* L110: */
	    }
/* L120: */
	}

/* INITIALIZE UPPER AND LOWER VISIBILITY ARRAYS */

	for (k = 1; k <= 1024; ++k) {
	    srfblk_1.limu[k - 1] = 0;
	    srfblk_1.liml[k - 1] = 1024;
/* L130: */
	}

/* FIND ORDER TO DRAW LINES */

	nxpass = 1;
	if (s[1] >= x[nnxx]) {
	    goto L160;
	}
	if (s[1] <= x[1]) {
	    goto L170;
	}
	i__3 = nnxx;
	for (i__ = 2; i__ <= i__3; ++i__) {
	    lx = i__;
	    if (s[1] <= x[i__]) {
		goto L150;
	    }
/* L140: */
	}
L150:
	mxs[0] = lx - 1;
	mxj[0] = -1;
	mxf[0] = 1;
	mxs[1] = lx;
	mxj[1] = 1;
	mxf[1] = nnxx;
	nxpass = 2;
	goto L180;
L160:
	mxs[0] = nnxx;
	mxj[0] = -1;
	mxf[0] = 1;
	goto L180;
L170:
	mxs[0] = 1;
	mxj[0] = 1;
	mxf[0] = nnxx;
L180:
	nypass = 1;
	if (s[2] >= y[nnyy]) {
	    goto L210;
	}
	if (s[2] <= y[1]) {
	    goto L220;
	}
	i__3 = nnyy;
	for (j = 2; j <= i__3; ++j) {
	    ly = j;
	    if (s[2] <= y[j]) {
		goto L200;
	    }
/* L190: */
	}
L200:
	mys[0] = ly - 1;
	myj[0] = -1;
	myf[0] = 1;
	mys[1] = ly;
	myj[1] = 1;
	myf[1] = nnyy;
	nypass = 2;
	goto L230;
L210:
	mys[0] = nnyy;
	myj[0] = -1;
	myf[0] = 1;
	goto L230;
L220:
	mys[0] = 1;
	myj[0] = 1;
	myf[0] = nnyy;

/* PUT ON SKIRT ON FRONT SIDE IF WANTED */

L230:
	if (nxpass == 2 && nypass == 2) {
	    goto L490;
	}
	if (srfip1_1.iskirt == 0) {
	    goto L290;
	}
	in = mxs[0];
	if__ = mxf[0];
	jn = mys[0];
	jf = myf[0];
	if (nypass != 1) {
	    goto L260;
	}
	trn32s_(&x[1], &y[jn], &srfip1_1.hskirt, &ux1, &vx1, &dummy, &c__1);
	trn32s_(&x[nnxx], &y[jn], &srfip1_1.hskirt, &ux2, &vx2, &dummy, &c__1)
		;
	qu = (ux2 - ux1) / (x[nnxx] - x[1]);
	qv = (vx2 - vx1) / (x[nnxx] - x[1]);
	ynow = y[jn];
	i__3 = nnxx;
	for (i__ = 1; i__ <= i__3; ++i__) {
	    trn32s_(&x[i__], &ynow, &srfip1_1.hskirt, &ru, &rv, &dummy, &c__1)
		    ;
	    i__2 = (integer) ru;
	    i__5 = (integer) rv;
	    draws_(&i__2, &i__5, &m[(i__ + jn * m_dim2 << 1) + 1], &m[(i__ + 
		    jn * m_dim2 << 1) + 2], &c__1, &c__0);
/* L240: */
	}
	i__3 = (integer) ux1;
	i__2 = (integer) vx1;
	i__5 = (integer) ux2;
	i__4 = (integer) vx2;
	draws_(&i__3, &i__2, &i__5, &i__4, &c__1, &c__1);
	if (srfip1_1.idry != 0) {
	    goto L260;
	}
	i__3 = nnxx;
	for (i__ = 2; i__ <= i__3; ++i__) {
	    draws_(&m[(i__ - 1 + jn * m_dim2 << 1) + 1], &m[(i__ - 1 + jn * 
		    m_dim2 << 1) + 2], &m[(i__ + jn * m_dim2 << 1) + 1], &m[(
		    i__ + jn * m_dim2 << 1) + 2], &c__1, &c__1);
/* L250: */
	}
L260:
	if (nxpass != 1) {
	    goto L290;
	}
	trn32s_(&x[in], &y[1], &srfip1_1.hskirt, &uy1, &vy1, &dummy, &c__1);
	trn32s_(&x[in], &y[nnyy], &srfip1_1.hskirt, &uy2, &vy2, &dummy, &c__1)
		;
	qu = (uy2 - uy1) / (y[nnyy] - y[1]);
	qv = (vy2 - vy1) / (y[nnyy] - y[1]);
	xnow = x[in];
	i__3 = nnyy;
	for (j = 1; j <= i__3; ++j) {
	    trn32s_(&xnow, &y[j], &srfip1_1.hskirt, &ru, &rv, &dummy, &c__1);
	    i__2 = (integer) ru;
	    i__5 = (integer) rv;
	    draws_(&i__2, &i__5, &m[(in + j * m_dim2 << 1) + 1], &m[(in + j * 
		    m_dim2 << 1) + 2], &c__1, &c__0);
/* L270: */
	}
	i__3 = (integer) uy1;
	i__2 = (integer) vy1;
	i__5 = (integer) uy2;
	i__4 = (integer) vy2;
	draws_(&i__3, &i__2, &i__5, &i__4, &c__1, &c__1);
	if (srfip1_1.idrx != 0) {
	    goto L290;
	}
	i__3 = nnyy;
	for (j = 2; j <= i__3; ++j) {
	    draws_(&m[(in + (j - 1) * m_dim2 << 1) + 1], &m[(in + (j - 1) * 
		    m_dim2 << 1) + 2], &m[(in + j * m_dim2 << 1) + 1], &m[(in 
		    + j * m_dim2 << 1) + 2], &c__1, &c__1);
/* L280: */
	}

/* PICK PROPER ALGORITHM */

L290:
	li = mxj[0];
	mi = mxs[0] - li;
	ni = (i__3 = mi - mxf[0], abs(i__3));
	lj = myj[0];
	mj = mys[0] - lj;
	nj = (i__3 = mj - myf[0], abs(i__3));

/* WHEN LINE OF SIGHT IS NEARER TO PARALLEL TO THE X AXIS, */
/* HAVE J LOOP OUTER-MOST, OTHERWISE HAVE I LOOP OUTER-MOST. */

	if (dabs(rx) <= dabs(ry)) {
	    goto L360;
	}
	if (srfip1_1.iskirt != 0 || nypass != 1) {
	    goto L310;
	}
	i__ = mxs[0];
	i__3 = nnyy;
	for (j = 2; j <= i__3; ++j) {
	    draws_(&m[(i__ + (j - 1) * m_dim2 << 1) + 1], &m[(i__ + (j - 1) * 
		    m_dim2 << 1) + 2], &m[(i__ + j * m_dim2 << 1) + 1], &m[(
		    i__ + j * m_dim2 << 1) + 2], &c__0, &c__1);
/* L300: */
	}
L310:
	i__3 = nnxx;
	for (ii = 1; ii <= i__3; ++ii) {
	    i__ = mi + ii * li;
	    ipli = i__ + li;
	    if (nypass == 1) {
		goto L320;
	    }
	    k = mys[0];
	    l = mys[1];
	    if (srfip1_1.idrx != 0) {
		draws_(&m[(i__ + k * m_dim2 << 1) + 1], &m[(i__ + k * m_dim2 
			<< 1) + 2], &m[(i__ + l * m_dim2 << 1) + 1], &m[(i__ 
			+ l * m_dim2 << 1) + 2], &c__1, &c__1);
	    }
	    if (srfblk_1.ndrz != 0 && ii != ni) {
/* Computing MIN */
		i__5 = i__, i__4 = i__ + li;
		i__2 = min(i__5,i__4);
		ctcell_(&z__[z_offset], &mmxx, &nnxx, &nnyy, &m[m_offset], &
			i__2, &k);
	    }
L320:
	    i__2 = nypass;
	    for (jpass = 1; jpass <= i__2; ++jpass) {
		lj = myj[jpass - 1];
		mj = mys[jpass - 1] - lj;
		nj = (i__5 = mj - myf[jpass - 1], abs(i__5));
		i__5 = nj;
		for (jj = 1; jj <= i__5; ++jj) {
		    j = mj + jj * lj;
		    jplj = j + lj;
		    if (srfip1_1.idrx != 0 && jj != nj) {
			draws_(&m[(i__ + j * m_dim2 << 1) + 1], &m[(i__ + j * 
				m_dim2 << 1) + 2], &m[(i__ + jplj * m_dim2 << 
				1) + 1], &m[(i__ + jplj * m_dim2 << 1) + 2], &
				c__1, &c__1);
		    }
		    if (i__ != mxf[0] && srfip1_1.idry != 0) {
			draws_(&m[(ipli + j * m_dim2 << 1) + 1], &m[(ipli + j 
				* m_dim2 << 1) + 2], &m[(i__ + j * m_dim2 << 
				1) + 1], &m[(i__ + j * m_dim2 << 1) + 2], &
				c__1, &c__1);
		    }
		    if (srfblk_1.ndrz != 0 && jj != nj && ii != nnxx) {
/* Computing MIN */
			i__6 = i__, i__7 = i__ + li;
			i__4 = min(i__6,i__7);
/* Computing MIN */
			i__9 = j, i__10 = j + lj;
			i__8 = min(i__9,i__10);
			ctcell_(&z__[z_offset], &mmxx, &nnxx, &nnyy, &m[
				m_offset], &i__4, &i__8);
		    }
/* L330: */
		}
/* L340: */
	    }
/* L350: */
	}
	goto L430;
L360:
	if (srfip1_1.iskirt != 0 || nxpass != 1) {
	    goto L380;
	}
	j = mys[0];
	i__3 = nnxx;
	for (i__ = 2; i__ <= i__3; ++i__) {
	    draws_(&m[(i__ - 1 + j * m_dim2 << 1) + 1], &m[(i__ - 1 + j * 
		    m_dim2 << 1) + 2], &m[(i__ + j * m_dim2 << 1) + 1], &m[(
		    i__ + j * m_dim2 << 1) + 2], &c__0, &c__1);
/* L370: */
	}
L380:
	i__3 = nnyy;
	for (jj = 1; jj <= i__3; ++jj) {
	    j = mj + jj * lj;
	    jplj = j + lj;
	    if (nxpass == 1) {
		goto L390;
	    }
	    k = mxs[0];
	    l = mxs[1];
	    if (srfip1_1.idry != 0) {
		draws_(&m[(k + j * m_dim2 << 1) + 1], &m[(k + j * m_dim2 << 1)
			 + 2], &m[(l + j * m_dim2 << 1) + 1], &m[(l + j * 
			m_dim2 << 1) + 2], &c__1, &c__1);
	    }
	    if (srfblk_1.ndrz != 0 && jj != nj) {
/* Computing MIN */
		i__5 = j, i__4 = j + lj;
		i__2 = min(i__5,i__4);
		ctcell_(&z__[z_offset], &mmxx, &nnxx, &nnyy, &m[m_offset], &k,
			 &i__2);
	    }
L390:
	    i__2 = nxpass;
	    for (ipass = 1; ipass <= i__2; ++ipass) {
		li = mxj[ipass - 1];
		mi = mxs[ipass - 1] - li;
		ni = (i__5 = mi - mxf[ipass - 1], abs(i__5));
		i__5 = ni;
		for (ii = 1; ii <= i__5; ++ii) {
		    i__ = mi + ii * li;
		    ipli = i__ + li;
		    if (srfip1_1.idry != 0 && ii != ni) {
			draws_(&m[(i__ + j * m_dim2 << 1) + 1], &m[(i__ + j * 
				m_dim2 << 1) + 2], &m[(ipli + j * m_dim2 << 1)
				 + 1], &m[(ipli + j * m_dim2 << 1) + 2], &
				c__1, &c__1);
		    }
		    if (j != myf[0] && srfip1_1.idrx != 0) {
			draws_(&m[(i__ + jplj * m_dim2 << 1) + 1], &m[(i__ + 
				jplj * m_dim2 << 1) + 2], &m[(i__ + j * 
				m_dim2 << 1) + 1], &m[(i__ + j * m_dim2 << 1) 
				+ 2], &c__1, &c__1);
		    }
		    if (srfblk_1.ndrz != 0 && ii != ni && jj != nnyy) {
/* Computing MIN */
			i__6 = i__, i__7 = i__ + li;
			i__4 = min(i__6,i__7);
/* Computing MIN */
			i__9 = j, i__10 = j + lj;
			i__8 = min(i__9,i__10);
			ctcell_(&z__[z_offset], &mmxx, &nnxx, &nnyy, &m[
				m_offset], &i__4, &i__8);
		    }
/* L400: */
		}
/* L410: */
	    }
/* L420: */
	}
L430:
	if (srfip1_1.iskirt == 0) {
	    goto L520;
	}

/* FIX UP IF SKIRT IS USED WITH LINES ONE WAY. */

	if (srfip1_1.idrx != 0) {
	    goto L460;
	}
	i__3 = nxpass;
	for (ipass = 1; ipass <= i__3; ++ipass) {
	    if (nxpass == 2) {
		if__ = (ipass - 1) * (nnxx - 1) + 1;
	    }
	    i__2 = nnyy;
	    for (j = 2; j <= i__2; ++j) {
		draws_(&m[(if__ + (j - 1) * m_dim2 << 1) + 1], &m[(if__ + (j 
			- 1) * m_dim2 << 1) + 2], &m[(if__ + j * m_dim2 << 1) 
			+ 1], &m[(if__ + j * m_dim2 << 1) + 2], &c__1, &c__0);
/* L440: */
	    }
/* L450: */
	}
L460:
	if (srfip1_1.idry != 0) {
	    goto L520;
	}
	i__3 = nypass;
	for (jpass = 1; jpass <= i__3; ++jpass) {
	    if (nypass == 2) {
		jf = (jpass - 1) * (nnyy - 1) + 1;
	    }
	    i__2 = nnxx;
	    for (i__ = 2; i__ <= i__2; ++i__) {
		draws_(&m[(i__ - 1 + jf * m_dim2 << 1) + 1], &m[(i__ - 1 + jf 
			* m_dim2 << 1) + 2], &m[(i__ + jf * m_dim2 << 1) + 1],
			 &m[(i__ + jf * m_dim2 << 1) + 2], &c__1, &c__0);
/* L470: */
	    }
/* L480: */
	}
	goto L520;

/* ALL VISIBLE IF VIEWED FROM DIRECTLY ABOVE OR BELOW. */

L490:
	if (srfblk_1.nupper > 0 && s[3] < s[6]) {
	    goto L520;
	}
	if (srfblk_1.nupper < 0 && s[3] > s[6]) {
	    goto L520;
	}
	srfblk_1.nupper = 1;
	if (s[3] < s[6]) {
	    srfblk_1.nupper = -1;
	}
	i__3 = nnxx;
	for (i__ = 1; i__ <= i__3; ++i__) {
	    i__2 = nnyy;
	    for (j = 1; j <= i__2; ++j) {
		if (srfip1_1.idrx != 0 && j != nnyy) {
		    draws_(&m[(i__ + j * m_dim2 << 1) + 1], &m[(i__ + j * 
			    m_dim2 << 1) + 2], &m[(i__ + (j + 1) * m_dim2 << 
			    1) + 1], &m[(i__ + (j + 1) * m_dim2 << 1) + 2], &
			    c__1, &c__0);
		}
		if (srfip1_1.idry != 0 && i__ != nnxx) {
		    draws_(&m[(i__ + j * m_dim2 << 1) + 1], &m[(i__ + j * 
			    m_dim2 << 1) + 2], &m[(i__ + 1 + j * m_dim2 << 1) 
			    + 1], &m[(i__ + 1 + j * m_dim2 << 1) + 2], &c__1, 
			    &c__0);
		}
		if (srfip1_1.idrz != 0 && i__ != nnxx && j != nnyy) {
		    ctcell_(&z__[z_offset], &mmxx, &nnxx, &nnyy, &m[m_offset],
			     &i__, &j);
		}
/* L500: */
	    }
/* L510: */
	}
L520:
	if (ster == 0.f) {
	    goto L560;
	}
	if (srfip1_1.istp < 0) {
	    goto L540;
	} else if (srfip1_1.istp == 0) {
	    goto L530;
	} else {
	    goto L550;
	}
L530:
	frame_();
L540:
	frame_();
	goto L570;
L550:
	if (ipic != 2) {
	    goto L570;
	}
L560:
	if (srfip1_1.ifr > 0) {
	    frame_();
	}
L570:
	;
    }
    return 0;
} /* srface_ */




/* Subroutine */ int srfpl_(integer *n, real *px, real *py)
{
    extern /* Subroutine */ int line_(real *, real *, real *, real *);

    /* Parameter adjustments */
    --py;
    --px;

    /* Function Body */
    line_(&px[1], &py[1], &px[2], &py[2]);
    return 0;
} /* srfpl_ */




/* Subroutine */ int clset_(real *z__, integer *mx, integer *nx, integer *ny, 
	real *chi, real *clo, real *cinc, integer *nla, integer *nlm, real *
	cl, integer *ncl, integer *icnst, integer *ioffp, real *spval, real *
	bigest)
{
    /* Initialized data */

    static integer kk = 0;

    /* System generated locals */
    integer z_dim1, z_offset, i__1, i__2;
    real r__1;

    /* Builtin functions */
    double r_lg10(real *), pow_ri(real *, integer *), r_int(real *);

    /* Local variables */
    static real fanc, crat;
    static integer i__, j, k;
    static real p, cc, ha, glo;

/* cc      DIMENSION       Z(MX,NY)   ,CL(NLM) */
    /* Parameter adjustments */
    z_dim1 = *mx;
    z_offset = z_dim1 + 1;
    z__ -= z_offset;
    --cl;

    /* Function Body */
/* +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
 */
/* CLSET PUTS THE VALUS OF THE CONTOUR LEVELS IN CL */

    *icnst = 0;
    glo = *clo;
    ha = *chi;
    fanc = *cinc;
    crat = (real) (*nla);
    if ((r__1 = ha - glo) < 0.f) {
	goto L10;
    } else if (r__1 == 0) {
	goto L20;
    } else {
	goto L50;
    }
L10:
    glo = ha;
    ha = *clo;
    goto L50;
L20:
    glo = *bigest;
    ha = -glo;
    i__1 = *ny;
    for (j = 1; j <= i__1; ++j) {
	i__2 = *nx;
	for (i__ = 1; i__ <= i__2; ++i__) {
	    if (*ioffp == 1 && z__[i__ + j * z_dim1] == *spval) {
		goto L30;
	    }
/* Computing MIN */
	    r__1 = z__[i__ + j * z_dim1];
	    glo = dmin(r__1,glo);
/* Computing MAX */
	    r__1 = z__[i__ + j * z_dim1];
	    ha = dmax(r__1,ha);
L30:
	    ;
	}
/* L40: */
    }
L50:
    if (fanc < 0.f) {
	goto L60;
    } else if (fanc == 0) {
	goto L70;
    } else {
	goto L90;
    }
L60:
    crat = -fanc;
L70:
    fanc = (ha - glo) / crat;
    if (fanc <= 0.f) {
	goto L140;
    } else {
	goto L80;
    }
L80:
    i__1 = (integer) (r_lg10(&fanc) + 500.f) - 500;
    p = pow_ri(&c_b128, &i__1);
    r__1 = fanc / p;
    fanc = r_int(&r__1) * p;
L90:
    if (*chi - *clo != 0.f) {
	goto L110;
    } else {
	goto L100;
    }
L100:
    r__1 = glo / fanc;
    glo = r_int(&r__1) * fanc;
    r__1 = ha / fanc;
    ha = r_int(&r__1) * fanc;
L110:
    i__1 = *nlm;
    for (k = 1; k <= i__1; ++k) {
	cc = glo + (real) (k - 1) * fanc;
	if (cc > ha) {
	    goto L130;
	}
	kk = k;
	cl[k] = cc;
/* L120: */
    }
L130:
    *ncl = kk;
    return 0;
L140:
    *icnst = 1;
    return 0;
} /* clset_ */




/* Subroutine */ int ctcell_(real *z__, integer *mx, integer *nx, integer *ny,
	 integer *m, integer *i0, integer *j0)
{
    /* Initialized data */

    static integer idub = 0;

    /* System generated locals */
    integer z_dim1, z_offset, m_dim2, m_offset, i__1, i__2;
    real r__1;

    /* Builtin functions */
    double r_sign(real *, real *);

    /* Local variables */
    static integer jump, k;
    extern /* Subroutine */ int color_(integer *), draws_(integer *, integer *
	    , integer *, integer *, integer *, integer *);
    static integer i1, j1;
    static real h1, h2, h3, h4;
    static integer k1, k2, k3, k4;
    static real ra, rb, cv;
    static logical lcolor;
    static integer i1p1, j1p1, mua, mva, mub, mvb;


/* CC      DIMENSION       Z(MX,NY)   ,M(2,NX,NY) */

    /* Parameter adjustments */
    m_dim2 = *nx;
    m_offset = (m_dim2 + 1 << 1) + 1;
    m -= m_offset;
    z_dim1 = *mx;
    z_offset = z_dim1 + 1;
    z__ -= z_offset;

    /* Function Body */
/* +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
 */
/* +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
 */
    i1 = *i0;
    i1p1 = i1 + 1;
    j1 = *j0;
    j1p1 = j1 + 1;
    h1 = z__[i1 + j1 * z_dim1];
    h2 = z__[i1 + j1p1 * z_dim1];
    h3 = z__[i1p1 + j1p1 * z_dim1];
    h4 = z__[i1p1 + j1 * z_dim1];
    if (srfblk_1.ioffp != 1) {
	goto L10;
    }
    if (h1 == srfblk_1.spval || h2 == srfblk_1.spval || h3 == srfblk_1.spval 
	    || h4 == srfblk_1.spval) {
	return 0;
    }
L10:
/* Computing MIN */
    r__1 = min(h1,h2), r__1 = min(r__1,h3);
    if (dmin(r__1,h4) > srfblk_1.cl[srfblk_1.ncl - 1]) {
	return 0;
    }

    lcolor = FALSE_;
    i__1 = srfblk_1.ncl;
    for (k = 1; k <= i__1; ++k) {

/* FOR EACH CONTOUR LEVEL, DESIDE WHICH OF THE 16 BASIC SIT- */
/* UATIONS EXISTS, THEN INTERPOLATE IN TWO-SPACE TO FIND THE */
/* END POINTS OF THE CONTOUR LINE SEGMENT WITHIN THIS CELL. */

	cv = srfblk_1.cl[k - 1];
	r__1 = h1 - cv;
	k1 = ((integer) r_sign(&c_b3, &r__1) + 1) / 2;
	r__1 = h2 - cv;
	k2 = ((integer) r_sign(&c_b3, &r__1) + 1) / 2;
	r__1 = h3 - cv;
	k3 = ((integer) r_sign(&c_b3, &r__1) + 1) / 2;
	r__1 = h4 - cv;
	k4 = ((integer) r_sign(&c_b3, &r__1) + 1) / 2;
	jump = k1 + 1 + (k2 << 1) + (k3 << 2) + (k4 << 3);

/*  17/Apr/91:  plot contours in different colors */

	if (jump > 1 && jump < 16) {
	    i__2 = k % 6 + 2;
	    color_(&i__2);
	}

	switch (jump) {
	    case 1:  goto L120;
	    case 2:  goto L30;
	    case 3:  goto L50;
	    case 4:  goto L60;
	    case 5:  goto L70;
	    case 6:  goto L20;
	    case 7:  goto L80;
	    case 8:  goto L90;
	    case 9:  goto L90;
	    case 10:  goto L80;
	    case 11:  goto L40;
	    case 12:  goto L70;
	    case 13:  goto L60;
	    case 14:  goto L50;
	    case 15:  goto L30;
	    case 16:  goto L110;
	}

L20:
	idub = 1;
L30:
	ra = (h1 - cv) / (h1 - h2);
	mua = (real) m[(i1 + j1 * m_dim2 << 1) + 1] + ra * (real) (m[(i1 + 
		j1p1 * m_dim2 << 1) + 1] - m[(i1 + j1 * m_dim2 << 1) + 1]);
	mva = (real) m[(i1 + j1 * m_dim2 << 1) + 2] + ra * (real) (m[(i1 + 
		j1p1 * m_dim2 << 1) + 2] - m[(i1 + j1 * m_dim2 << 1) + 2]);
	rb = (h1 - cv) / (h1 - h4);
	mub = (real) m[(i1 + j1 * m_dim2 << 1) + 1] + rb * (real) (m[(i1p1 + 
		j1 * m_dim2 << 1) + 1] - m[(i1 + j1 * m_dim2 << 1) + 1]);
	mvb = (real) m[(i1 + j1 * m_dim2 << 1) + 2] + rb * (real) (m[(i1p1 + 
		j1 * m_dim2 << 1) + 2] - m[(i1 + j1 * m_dim2 << 1) + 2]);
	goto L100;
L40:
	idub = -1;
L50:
	ra = (h2 - cv) / (h2 - h1);
	mua = (real) m[(i1 + j1p1 * m_dim2 << 1) + 1] + ra * (real) (m[(i1 + 
		j1 * m_dim2 << 1) + 1] - m[(i1 + j1p1 * m_dim2 << 1) + 1]);
	mva = (real) m[(i1 + j1p1 * m_dim2 << 1) + 2] + ra * (real) (m[(i1 + 
		j1 * m_dim2 << 1) + 2] - m[(i1 + j1p1 * m_dim2 << 1) + 2]);
	rb = (h2 - cv) / (h2 - h3);
	mub = (real) m[(i1 + j1p1 * m_dim2 << 1) + 1] + rb * (real) (m[(i1p1 
		+ j1p1 * m_dim2 << 1) + 1] - m[(i1 + j1p1 * m_dim2 << 1) + 1])
		;
	mvb = (real) m[(i1 + j1p1 * m_dim2 << 1) + 2] + rb * (real) (m[(i1p1 
		+ j1p1 * m_dim2 << 1) + 2] - m[(i1 + j1p1 * m_dim2 << 1) + 2])
		;
	goto L100;
L60:
	ra = (h2 - cv) / (h2 - h3);
	mua = (real) m[(i1 + j1p1 * m_dim2 << 1) + 1] + ra * (real) (m[(i1p1 
		+ j1p1 * m_dim2 << 1) + 1] - m[(i1 + j1p1 * m_dim2 << 1) + 1])
		;
	mva = (real) m[(i1 + j1p1 * m_dim2 << 1) + 2] + ra * (real) (m[(i1p1 
		+ j1p1 * m_dim2 << 1) + 2] - m[(i1 + j1p1 * m_dim2 << 1) + 2])
		;
	rb = (h1 - cv) / (h1 - h4);
	mub = (real) m[(i1 + j1 * m_dim2 << 1) + 1] + rb * (real) (m[(i1p1 + 
		j1 * m_dim2 << 1) + 1] - m[(i1 + j1 * m_dim2 << 1) + 1]);
	mvb = (real) m[(i1 + j1 * m_dim2 << 1) + 2] + rb * (real) (m[(i1p1 + 
		j1 * m_dim2 << 1) + 2] - m[(i1 + j1 * m_dim2 << 1) + 2]);
	goto L100;
L70:
	ra = (h3 - cv) / (h3 - h2);
	mua = (real) m[(i1p1 + j1p1 * m_dim2 << 1) + 1] + ra * (real) (m[(i1 
		+ j1p1 * m_dim2 << 1) + 1] - m[(i1p1 + j1p1 * m_dim2 << 1) + 
		1]);
	mva = (real) m[(i1p1 + j1p1 * m_dim2 << 1) + 2] + ra * (real) (m[(i1 
		+ j1p1 * m_dim2 << 1) + 2] - m[(i1p1 + j1p1 * m_dim2 << 1) + 
		2]);
	rb = (h3 - cv) / (h3 - h4);
	mub = (real) m[(i1p1 + j1p1 * m_dim2 << 1) + 1] + rb * (real) (m[(
		i1p1 + j1 * m_dim2 << 1) + 1] - m[(i1p1 + j1p1 * m_dim2 << 1) 
		+ 1]);
	mvb = (real) m[(i1p1 + j1p1 * m_dim2 << 1) + 2] + rb * (real) (m[(
		i1p1 + j1 * m_dim2 << 1) + 2] - m[(i1p1 + j1p1 * m_dim2 << 1) 
		+ 2]);
	idub = 0;
	goto L100;
L80:
	ra = (h2 - cv) / (h2 - h1);
	mua = (real) m[(i1 + j1p1 * m_dim2 << 1) + 1] + ra * (real) (m[(i1 + 
		j1 * m_dim2 << 1) + 1] - m[(i1 + j1p1 * m_dim2 << 1) + 1]);
	mva = (real) m[(i1 + j1p1 * m_dim2 << 1) + 2] + ra * (real) (m[(i1 + 
		j1 * m_dim2 << 1) + 2] - m[(i1 + j1p1 * m_dim2 << 1) + 2]);
	rb = (h3 - cv) / (h3 - h4);
	mub = (real) m[(i1p1 + j1p1 * m_dim2 << 1) + 1] + rb * (real) (m[(
		i1p1 + j1 * m_dim2 << 1) + 1] - m[(i1p1 + j1p1 * m_dim2 << 1) 
		+ 1]);
	mvb = (real) m[(i1p1 + j1p1 * m_dim2 << 1) + 2] + rb * (real) (m[(
		i1p1 + j1 * m_dim2 << 1) + 2] - m[(i1p1 + j1p1 * m_dim2 << 1) 
		+ 2]);
	goto L100;
L90:
	ra = (h4 - cv) / (h4 - h1);
	mua = (real) m[(i1p1 + j1 * m_dim2 << 1) + 1] + ra * (real) (m[(i1 + 
		j1 * m_dim2 << 1) + 1] - m[(i1p1 + j1 * m_dim2 << 1) + 1]);
	mva = (real) m[(i1p1 + j1 * m_dim2 << 1) + 2] + ra * (real) (m[(i1 + 
		j1 * m_dim2 << 1) + 2] - m[(i1p1 + j1 * m_dim2 << 1) + 2]);
	rb = (h4 - cv) / (h4 - h3);
	mub = (real) m[(i1p1 + j1 * m_dim2 << 1) + 1] + rb * (real) (m[(i1p1 
		+ j1p1 * m_dim2 << 1) + 1] - m[(i1p1 + j1 * m_dim2 << 1) + 1])
		;
	mvb = (real) m[(i1p1 + j1 * m_dim2 << 1) + 2] + rb * (real) (m[(i1p1 
		+ j1p1 * m_dim2 << 1) + 2] - m[(i1p1 + j1 * m_dim2 << 1) + 2])
		;
	idub = 0;
L100:
	draws_(&mua, &mva, &mub, &mvb, &c__1, &c__0);
	lcolor = TRUE_;
	if (idub < 0) {
	    goto L90;
	} else if (idub == 0) {
	    goto L110;
	} else {
	    goto L70;
	}
L110:
	;
    }

L120:
    if (lcolor) {
	color_(&c__1);
    }
    return 0;
} /* ctcell_ */




/* Subroutine */ int draws_(integer *mx1, integer *my1, integer *mx2, integer 
	*my2, integer *idraw, integer *imark)
{
    /* Initialized data */

    static real steep = 5.f;
    static integer mx = 0;
    static integer my = 0;

    /* System generated locals */
    integer i__1, i__2;

    /* Local variables */
    static integer nx1p1, k, ltemp;
    extern /* Subroutine */ int srfpl_(integer *, real *, real *);
    static real dy;
    static integer nx1, ny1, nx2, ny2;
    static real pxs[2], pys[2], fny1;
    static logical vis1, vis2;
    static integer mmx1, mmy1, mmx2, mmy2;


/* THIS ROUTINE DRAWS THE VISIBLE PART OF THE LINE CONNECTING */
/* (MX1,MY1) AND (MX2,MY2).  IF IDRAW .NE. 0, THE LINE IS DRAWN. */
/* IF IMARK .NE. 0, THE VISIBILITY ARRAY IS MARKED. */

/* +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
 */
/* MAKE LINE LEFT TO RIGHT. */

    mmx1 = *mx1;
    mmy1 = *my1;
    mmx2 = *mx2;
    mmy2 = *my2;
    if (mmx1 == srfblk_1.nspval || mmx2 == srfblk_1.nspval) {
	return 0;
    }
    if (mmx1 > mmx2) {
	goto L10;
    }
    nx1 = mmx1;
    ny1 = mmy1;
    nx2 = mmx2;
    ny2 = mmy2;
    goto L20;
L10:
    nx1 = mmx2;
    ny1 = mmy2;
    nx2 = mmx1;
    ny2 = mmy1;
L20:
    if (srfblk_1.nupper < 0) {
	goto L180;
    }

/* CHECK UPPER VISIBILITY. */

    vis1 = ny1 >= srfblk_1.limu[nx1 - 1] - 1;
    vis2 = ny2 >= srfblk_1.limu[nx2 - 1] - 1;

/* VIS1 AND VIS2 TRUE MEANS VISIBLE. */

    if (vis1 && vis2) {
	goto L120;
    }

/* VIS1 AND VIS2 FALSE MEANS INVISIBLE. */

    if (! (vis1 || vis2)) {
	goto L180;
    }

/* FIND CHANGE POINT. */

    if (nx1 == nx2) {
	goto L110;
    }
    dy = (real) (ny2 - ny1) / (real) (nx2 - nx1);
    nx1p1 = nx1 + 1;
    fny1 = (real) ny1;
    if (vis1) {
	goto L60;
    }
    i__1 = nx2;
    for (k = nx1p1; k <= i__1; ++k) {
	mx = k;
	my = fny1 + (real) (k - nx1) * dy;
	if (my > srfblk_1.limu[k - 1]) {
	    goto L40;
	}
/* L30: */
    }
L40:
    if (dabs(dy) >= steep) {
	goto L90;
    }
L50:
    nx1 = mx;
    ny1 = my;
    goto L120;
L60:
    i__1 = nx2;
    for (k = nx1p1; k <= i__1; ++k) {
	mx = k;
	my = fny1 + (real) (k - nx1) * dy;
	if (my <= srfblk_1.limu[k - 1]) {
	    goto L80;
	}
/* L70: */
    }
L80:
    if (dabs(dy) >= steep) {
	goto L100;
    }
    nx2 = mx - 1;
    ny2 = my;
    goto L120;
L90:
    if (srfblk_1.limu[mx - 1] == 0) {
	goto L50;
    }
    nx1 = mx;
    ny1 = srfblk_1.limu[nx1 - 1];
    goto L120;
L100:
    nx2 = mx - 1;
    ny2 = srfblk_1.limu[nx2 - 1];
    goto L120;
L110:
    if (vis1) {
/* Computing MIN */
	i__1 = srfblk_1.limu[nx1 - 1], i__2 = srfblk_1.limu[nx2 - 1];
	ny2 = min(i__1,i__2);
    }
    if (vis2) {
/* Computing MIN */
	i__1 = srfblk_1.limu[nx1 - 1], i__2 = srfblk_1.limu[nx2 - 1];
	ny1 = min(i__1,i__2);
    }
L120:
    if (*idraw == 0) {
	goto L150;
    }

/* DRAW VISIBLE PART OF LINE. */

    if (srfblk_1.irot != 0) {
	goto L130;
    } else {
	goto L140;
    }
L130:
    pxs[0] = (real) ny1;
    pxs[1] = (real) ny2;
    pys[0] = (real) (1024 - nx1);
    pys[1] = (real) (1024 - nx2);
    srfpl_(&c__2, pxs, pys);
    goto L150;
L140:
    pxs[0] = (real) nx1;
    pxs[1] = (real) nx2;
    pys[0] = (real) ny1;
    pys[1] = (real) ny2;
    srfpl_(&c__2, pxs, pys);
L150:
    if (*imark == 0) {
	goto L180;
    }
    if (nx1 == nx2) {
	goto L170;
    }
    dy = (real) (ny2 - ny1) / (real) (nx2 - nx1);
    fny1 = (real) ny1;
    i__1 = nx2;
    for (k = nx1; k <= i__1; ++k) {
	ltemp = fny1 + (real) (k - nx1) * dy;
	if (ltemp > srfblk_1.limu[k - 1]) {
	    srfblk_1.limu[k - 1] = ltemp;
	}
/* L160: */
    }
    goto L180;
L170:
    ltemp = max(ny1,ny2);
    if (ltemp > srfblk_1.limu[nx1 - 1]) {
	srfblk_1.limu[nx1 - 1] = ltemp;
    }
L180:
    if (srfblk_1.nupper <= 0) {
	goto L190;
    } else {
	goto L370;
    }

/* SAME IDEA AS ABOVE, BUT FOR LOWER SIDE. */

L190:
    if (mmx1 > mmx2) {
	goto L200;
    }
    nx1 = mmx1;
    ny1 = mmy1;
    nx2 = mmx2;
    ny2 = mmy2;
    goto L210;
L200:
    nx1 = mmx2;
    ny1 = mmy2;
    nx2 = mmx1;
    ny2 = mmy1;
L210:
    vis1 = ny1 <= srfblk_1.liml[nx1 - 1] + 1;
    vis2 = ny2 <= srfblk_1.liml[nx2 - 1] + 1;
    if (vis1 && vis2) {
	goto L310;
    }
    if (! (vis1 || vis2)) {
	goto L370;
    }
    if (nx1 == nx2) {
	goto L300;
    }
    dy = (real) (ny2 - ny1) / (real) (nx2 - nx1);
    nx1p1 = nx1 + 1;
    fny1 = (real) ny1;
    if (vis1) {
	goto L250;
    }
    i__1 = nx2;
    for (k = nx1p1; k <= i__1; ++k) {
	mx = k;
	my = fny1 + (real) (k - nx1) * dy;
	if (my < srfblk_1.liml[k - 1]) {
	    goto L230;
	}
/* L220: */
    }
L230:
    if (dabs(dy) >= steep) {
	goto L280;
    }
L240:
    nx1 = mx;
    ny1 = my;
    goto L310;
L250:
    i__1 = nx2;
    for (k = nx1p1; k <= i__1; ++k) {
	mx = k;
	my = fny1 + (real) (k - nx1) * dy;
	if (my >= srfblk_1.liml[k - 1]) {
	    goto L270;
	}
/* L260: */
    }
L270:
    if (dabs(dy) >= steep) {
	goto L290;
    }
    nx2 = mx - 1;
    ny2 = my;
    goto L310;
L280:
    if (srfblk_1.liml[mx - 1] == 1024) {
	goto L240;
    }
    nx1 = mx;
    ny1 = srfblk_1.liml[nx1 - 1];
    goto L310;
L290:
    nx2 = mx - 1;
    ny2 = srfblk_1.liml[nx2 - 1];
    goto L310;
L300:
    if (vis1) {
/* Computing MAX */
	i__1 = srfblk_1.liml[nx1 - 1], i__2 = srfblk_1.liml[nx2 - 1];
	ny2 = max(i__1,i__2);
    }
    if (vis2) {
/* Computing MAX */
	i__1 = srfblk_1.liml[nx1 - 1], i__2 = srfblk_1.liml[nx2 - 1];
	ny1 = max(i__1,i__2);
    }
L310:
    if (*idraw == 0) {
	goto L340;
    }
    if (srfblk_1.irot != 0) {
	goto L320;
    } else {
	goto L330;
    }
L320:
    pxs[0] = (real) ny1;
    pxs[1] = (real) ny2;
    pys[0] = (real) (1024 - nx1);
    pys[1] = (real) (1024 - nx2);
    srfpl_(&c__2, pxs, pys);
    goto L340;
L330:
    pxs[0] = (real) nx1;
    pxs[1] = (real) nx2;
    pys[0] = (real) ny1;
    pys[1] = (real) ny2;
    srfpl_(&c__2, pxs, pys);
L340:
    if (*imark == 0) {
	goto L370;
    }
    if (nx1 == nx2) {
	goto L360;
    }
    dy = (real) (ny2 - ny1) / (real) (nx2 - nx1);
    fny1 = (real) ny1;
    i__1 = nx2;
    for (k = nx1; k <= i__1; ++k) {
	ltemp = fny1 + (real) (k - nx1) * dy;
	if (ltemp < srfblk_1.liml[k - 1]) {
	    srfblk_1.liml[k - 1] = ltemp;
	}
/* L350: */
    }
    return 0;
L360:
    ltemp = min(ny1,ny2);
    if (ltemp < srfblk_1.liml[nx1 - 1]) {
	srfblk_1.liml[nx1 - 1] = ltemp;
    }
L370:
    return 0;
} /* draws_ */




/* Subroutine */ int setr_(real *xmin, real *xmax, real *ymin, real *ymax, 
	real *zmin, real *zmax, real *r0)
{
    /* System generated locals */
    real r__1, r__2, r__3;

    /* Local variables */
    static real yeye, xeye, zeye, alpha;
    extern /* Subroutine */ int trn32s_(real *, real *, real *, real *, real *
	    , real *, integer *);
    static real dummy, dummie, xat, yat, zat, umn, vmn, xmn, ymn, zmn, umx, 
	    vmx, xmx, ymx, zmx;


/* +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
 */
    if (*r0 <= 0.f) {
	goto L10;
    } else {
	goto L20;
    }
L10:
    srfblk_1.nrswt = 0;
    return 0;
L20:
    srfblk_1.nrswt = 1;
    pwrz1s_1.xxmin = *xmin;
    pwrz1s_1.xxmax = *xmax;
    pwrz1s_1.yymin = *ymin;
    pwrz1s_1.yymax = *ymax;
    pwrz1s_1.zzmin = *zmin;
    pwrz1s_1.zzmax = *zmax;
    srfblk_1.rzero = *r0;
    srfblk_1.ll = 0;
    xat = (pwrz1s_1.xxmax + pwrz1s_1.xxmin) * .5f;
    yat = (pwrz1s_1.yymax + pwrz1s_1.yymin) * .5f;
    zat = (pwrz1s_1.zzmax + pwrz1s_1.zzmin) * .5f;
    alpha = -(pwrz1s_1.yymin - yat) / (pwrz1s_1.xxmin - xat);
    yeye = -srfblk_1.rzero / sqrt(alpha * alpha + 1.f);
    xeye = yeye * alpha;
    yeye += yat;
    xeye += xat;
    zeye = zat;
    trn32s_(&xat, &yat, &zat, &xeye, &yeye, &zeye, &c__0);
    xmn = pwrz1s_1.xxmin;
    xmx = pwrz1s_1.xxmax;
    ymn = pwrz1s_1.yymin;
    ymx = pwrz1s_1.yymax;
    zmn = pwrz1s_1.zzmin;
    zmx = pwrz1s_1.zzmax;
    trn32s_(&xmn, &ymn, &zat, &umn, &dummy, &dummie, &c__1);
    trn32s_(&xmx, &ymn, &zmn, &dummy, &vmn, &dummie, &c__1);
    trn32s_(&xmx, &ymx, &zat, &umx, &dummy, &dummie, &c__1);
    trn32s_(&xmx, &ymn, &zmx, &dummy, &vmx, &dummie, &c__1);
    srfblk_1.umin = umn;
    srfblk_1.umax = umx;
    srfblk_1.vmin = vmn;
    srfblk_1.vmax = vmx;
/* Computing 2nd power */
    r__1 = pwrz1s_1.xxmax - pwrz1s_1.xxmin;
/* Computing 2nd power */
    r__2 = pwrz1s_1.yymax - pwrz1s_1.yymin;
/* Computing 2nd power */
    r__3 = pwrz1s_1.zzmax - pwrz1s_1.zzmin;
    srfblk_1.bigd = sqrt(r__1 * r__1 + r__2 * r__2 + r__3 * r__3) * .5f;
    return 0;
} /* setr_ */




/* Subroutine */ int trn32s_(real *x, real *y, real *z__, real *xt, real *yt, 
	real *zt, integer *iflag)
{
    /* Initialized data */

    static integer nlu[7] = { 10,10,100,10,10,10,512 };
    static integer nru[7] = { 1014,924,1014,1014,1014,512,1014 };
    static integer nbv[7] = { 10,50,50,10,10,256,256 };
    static integer ntv[7] = { 1014,964,964,1014,1014,758,758 };

    /* Format strings */
    static char fmt_60[] = "";
    static char fmt_50[] = "";
    static char fmt_120[] = "";
    static char fmt_100[] = "";
    static char fmt_70[] = "";
    static char fmt_80[] = "";

    /* System generated locals */
    real r__1, r__2, r__3, r__4;

    /* Local variables */
    static integer jump, jump2, jump3;
    static real d__, q, r__, cosbe, cosga, sinbe, cosal, singa, u0, v0, u1, 
	    v1, u2, v2, u3, v3, u4, v4, ax, ay, az, dx, ex, ey, ez, dy, dz, 
	    xx, yy, zz;

    /* Assigned format variables */
    static char *jump3_fmt, *jump2_fmt, *jump_fmt;



/* PICTURE CORNER COORDINATES FOR LL=1 */


/* PICTURE CORNER COORDINATES FOR LL=2 */


/* PICTURE CORNER COORDINATES FOR LL=3 */


/* PICTURE CORNER COORDINATES FOR LL=4 */


/* PICTURE CORNER COORDINATES FOR LL=5 */


/* PICTURE CORNER COORDINATES FOR LL=6 */


/* PICTURE CORNER COORDINATES FOR LL=7 */

/* +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
 */
/* STORE THE PARAMETERS OF THE SET32 CALL FOR USE WHEN */
/* TRN32 IS CALLED. */

    if (*iflag != 0) {
	goto L40;
    } else {
	goto L10;
    }
L10:
    jump3 = 0;
    jump3_fmt = fmt_60;
    if (srfblk_1.ioffp == 1) {
	jump3 = 1;
	jump3_fmt = fmt_50;
    }
    ax = *x;
    ay = *y;
    az = *z__;
    ex = *xt;
    ey = *yt;
    ez = *zt;

/* AS MUCH COMPUTATION AS POSSIBLE IS DONE DURING EXECUTION */
/* THIS ROUTINE WHEN IFLAG=0 BECAUSE CALLS IN THAT MODE ARE INFREQUENT. */

    dx = ax - ex;
    dy = ay - ey;
    dz = az - ez;
    d__ = sqrt(dx * dx + dy * dy + dz * dz);
    cosal = dx / d__;
    cosbe = dy / d__;
    cosga = dz / d__;
    singa = sqrt(1.f - cosga * cosga);
    jump2 = 0;
    jump2_fmt = fmt_120;
    if (srfblk_1.ll == 0) {
	goto L20;
    }
    jump2 = 1;
    jump2_fmt = fmt_100;
    pwrz1s_1.delcrt = (real) (nru[srfblk_1.ll - 1] - nlu[srfblk_1.ll - 1]);
    u0 = srfblk_1.umin;
    v0 = srfblk_1.vmin;
    u1 = (real) nlu[srfblk_1.ll - 1];
    v1 = (real) nbv[srfblk_1.ll - 1];
    u2 = (real) (nru[srfblk_1.ll - 1] - nlu[srfblk_1.ll - 1]);
    v2 = (real) (ntv[srfblk_1.ll - 1] - nbv[srfblk_1.ll - 1]);
    u3 = u2 / (srfblk_1.umax - srfblk_1.umin);
    v3 = v2 / (srfblk_1.vmax - srfblk_1.vmin);
    u4 = (real) nru[srfblk_1.ll - 1];
    v4 = (real) ntv[srfblk_1.ll - 1];
    if (srfblk_1.nrswt == 0) {
	goto L20;
    }
    u0 = -srfblk_1.bigd;
    v0 = -srfblk_1.bigd;
    u3 = u2 / (srfblk_1.bigd * 2.f);
    v3 = v2 / (srfblk_1.bigd * 2.f);

/* THE 3-SPACE POINT LOOKED AT IS TRANSFORMED INTO (0,0) OF */
/* THE 2-SPACE.  THE 3-SPACE Z AXIS IS TRANSFORMED INTO THE */
/* 2-SPACE Y AXIS.  IF THE LINE OF SIGHT IS CLOSE TO PARALLEL */
/* TO THE 3-SPACE Z AXIS, THE 3-SPACE Y AXIS IS CHOSEN (IN- */
/* STEAD OF THE 3-SPACE Z AXIS) TO BE TRANSFORMED INTO THE */
/* 2-SPACE Y AXIS. */

L20:
    if (singa < 1e-4f) {
	goto L30;
    }
    r__ = 1.f / singa;
    jump = 0;
    jump_fmt = fmt_70;
    return 0;
L30:
    sinbe = sqrt(1.f - cosbe * cosbe);
    r__ = 1.f / sinbe;
    jump = 1;
    jump_fmt = fmt_80;
    return 0;
L40:
    xx = *x;
    yy = *y;
    zz = *z__;
    switch (jump3) {
	case 0: goto L60;
	case 1: goto L50;
    }
L50:
    if (zz == srfblk_1.spval) {
	goto L110;
    }
L60:
    q = d__ / ((xx - ex) * cosal + (yy - ey) * cosbe + (zz - ez) * cosga);
    switch (jump) {
	case 0: goto L70;
	case 1: goto L80;
    }
L70:
    xx = ((ex + q * (xx - ex) - ax) * cosbe - (ey + q * (yy - ey) - ay) * 
	    cosal) * r__;
    yy = (ez + q * (zz - ez) - az) * r__;
    goto L90;
L80:
    xx = ((ez + q * (zz - ez) - az) * cosal - (ex + q * (xx - ex) - ax) * 
	    cosga) * r__;
    yy = (ey + q * (yy - ey) - ay) * r__;
L90:
    switch (jump2) {
	case 0: goto L120;
	case 1: goto L100;
    }
L100:
/* Computing MIN */
/* Computing MAX */
    r__3 = u1, r__4 = u1 + u3 * (srfblk_1.fact * xx - u0);
    r__1 = u4, r__2 = dmax(r__3,r__4);
    xx = dmin(r__1,r__2);
/* Computing MIN */
/* Computing MAX */
    r__3 = v1, r__4 = v1 + v3 * (srfblk_1.fact * yy - v0);
    r__1 = v4, r__2 = dmax(r__3,r__4);
    yy = dmin(r__1,r__2);
    goto L120;
L110:
    xx = (real) srfblk_1.nspval;
    yy = (real) srfblk_1.nspval;

L120:
    *xt = xx;
    *yt = yy;
    return 0;
} /* trn32s_ */


/* cc      BLOCKDATA SRFABD */
/* Subroutine */ int srfabd_(void)
{

/*  INITIALIZATION OF INTERNAL PARAMETERS */


    return 0;
} /* srfabd_ */

