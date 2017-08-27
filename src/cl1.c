#include <math.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include "converted_from_fortran.h"

/* prototype for function at end (from TOMS, via Netlib and f2c) */

static int cl1_fort(integer *k, integer *l, integer *m, integer *n,
                    integer *klmd, integer *klm2d, integer *nklmd,
                    integer *n2d, real *q,
                    integer *kode, real *toler, integer *iter, real *x,
                    real *res, real * error, real *cu, integer *iu, integer *s) ;

/*---------------------------------------------------------------------
  Approximately (L1) solve equations

             j=nvec-1
    z[i] = SUM        A[j][i] * y[j]   (i=0..ndim-1)
             j=0

  for y[j] (j=0..nvec-1), subject to constraints (based on y[j] input)

  If input cony != 0, then you can supply constraints on the values
  of the output y[j] by putting values into the input y[j]:

  input y[j] =  0 ==> unconstrained
             =  1 ==> y[j] must be non-negative on output
             = -1 ==> y[j] must be non-positive on output

  If cony == 0, then the input y[j] is ignored.

  The return value of the function is E = sum |z[i]-SUM[i]| >= 0
  if everything worked, and is a negative number if an error occured.
-----------------------------------------------------------------------*/

float cl1_solve( int ndim, int nvec, float *z, float **A, float *y, int cony )
{
   /* loop counters */

   int jj , ii ;

   /* variables for CL1 (types declared in f2c.h) */

   integer k,l,m,n,klmd,klm2d,nklmd,n2d , kode=0,iter , *iu,*s ;
   real *q , toler , *x , *res , error , *cu ;

   /*-- check inputs --*/

   if( ndim < 1 || nvec < 1 )                         kode = 4 ;
   if( A == NULL || y == NULL || z == NULL )          kode = 4 ;
   for( jj=0 ; jj < nvec ; jj++ ) if( A[jj] == NULL ) kode = 4 ;

   if( kode ){
     fprintf(stderr,"** cl1_solve ERROR: illegal inputs!\n") ;
     return (float)(-kode) ;
   }

   /*-- setup call to CL1 --*/

   k     = ndim ;
   l     = 0 ;     /* no linear equality constraints */
   m     = 0 ;     /* no linear inequality constraints */
   n     = nvec ;

   klmd  = k+l+m ;
   klm2d = k+l+m+2 ;
   nklmd = n+k+l+m ;
   n2d   = n+2 ;

   kode  = (cony != 0) ; /* enforce implicit constraints on x[] */
   iter  = 11*klmd ;

   toler = 0.0001 ;
   error = 0.0 ;

   /* input/output matrices & vectors */

   q     = (real *) calloc( 1, sizeof(real) * klm2d*n2d ) ;
   x     = (real *) calloc( 1, sizeof(real) * n2d ) ;
   res   = (real *) calloc( 1, sizeof(real) * klmd ) ;

   /* workspaces */

   cu    = (real *)    calloc( 1, sizeof(real) * 2*nklmd ) ;
   iu    = (integer *) calloc( 1, sizeof(integer) * 2*nklmd ) ;
   s     = (integer *) calloc( 1, sizeof(integer) * klmd ) ;

   /* load matrices & vectors */

   for( jj=0 ; jj < nvec ; jj++ )
     for( ii=0 ; ii < ndim ; ii++ )
       q[ii+jj*klm2d] = A[jj][ii] ;   /* matrix */

   for( ii=0 ; ii < ndim ; ii++ )
     q[ii+nvec*klm2d] = z[ii] ;        /* vector */

   if( cony ){
     for( jj=0 ; jj < nvec ; jj++ )     /* constraints on solution */
       x[jj] = (y[jj] < 0.0) ? -1.0
              :(y[jj] > 0.0) ?  1.0 : 0.0 ;
   }

   for( ii=0 ; ii < klmd ; ii++ )       /* no constraints on resids */
     res[ii] = 0.0 ;

   /*-- do the work --*/

   cl1_fort( &k, &l, &m, &n,
             &klmd, &klm2d, &nklmd, &n2d,
             q, &kode, &toler, &iter, x, res, &error, cu, iu, s) ;

   free(q) ; free(res) ; free(cu) ; free(iu) ; free(s) ;

   if( kode != 0 ){
     free(x) ;
#if 0
     switch( kode ){
       case 1: fprintf(stderr,"** cl1_solve ERROR: no feasible solution!\n"); break;
       case 2: fprintf(stderr,"** cl1_solve ERROR: rounding errors!\n")     ; break;
       case 3: fprintf(stderr,"** cl1_solve ERROR: max iterations!\n")      ; break;
      default: fprintf(stderr,"** cl1_solve ERROR: unknown problem!\n")     ; break;
     }
#endif
     return (float)(-kode) ;
   }

   /*-- copy results into output --*/

   for( jj=0 ; jj < nvec ; jj++ ) y[jj] = (float) x[jj] ;

   free(x) ; return (float)error ;
}

/*---------------------------------------------------------------------
  Approximately (L1) solve equations

             j=nvec-1
    z[i] = SUM        A[j][i] * y[j]   (i=0..ndim-1)
             j=0

  for y[j] (j=0..nvec-1), subject to constraints on the signs of y[j]
  (based on y[j] input) AND subject to constraints on the signs of
  the residuals z[i]-SUM[i] (based on rez[i] input).

  If input cony != 0, then you can supply constraints on the values
  of the output y[j] by putting values into the input y[j]:

  input y[j] =  0 ==> unconstrained
             =  1 ==> y[j] must be non-negative on output
             = -1 ==> y[j] must be non-positive on output

  If cony == 0, then the input y[j] is ignored.

  If input conr != 0, then you can supply constraints on the values
  of the residuals z[i]-SUM[i] by putting values into the input rez[i]:

  input rez[i] =  0 ==> unconstrained
               =  1 ==> z[i]-SUM[i] must be non-negative
               = -1 ==> z[i]-SUM[i] must be non-positive

  If conr == 0, then the input values in rez[] are ignored.

  The outputs are in y[] and rez[].

  The return value of the function is E = sum |z[i]-SUM[i]| >= 0
  if everything worked, and is a negative number if an error occured.
-----------------------------------------------------------------------*/

float cl1_solve_res( int ndim, int nvec, float *z, float **A,
                     float *y, int cony , float *rez , int conr )
{
   /* loop counters */

   int jj , ii ;

   /* variables for CL1 (types declared in f2c.h) */

   integer k,l,m,n,klmd,klm2d,nklmd,n2d , kode=0,iter , *iu,*s ;
   real *q , toler , *x , *res , error , *cu ;

   /*-- check inputs --*/

   if( ndim < 1 || nvec < 1 )                         kode = 4 ;
   if( A == NULL || y == NULL || z == NULL )          kode = 4 ;
   for( jj=0 ; jj < nvec ; jj++ ) if( A[jj] == NULL ) kode = 4 ;

   if( kode ){
     fprintf(stderr,"** cl1_solve ERROR: illegal inputs!\n") ;
     return (float)(-kode) ;
   }

   /*-- setup call to CL1 --*/

   k     = ndim ;
   l     = 0 ;     /* no linear equality constraints */
   m     = 0 ;     /* no linear inequality constraints */
   n     = nvec ;

   klmd  = k+l+m ;
   klm2d = k+l+m+2 ;
   nklmd = n+k+l+m ;
   n2d   = n+2 ;

   kode  = (cony != 0 || conr != 0) ; /* enforce implicit constraints on x[] */
   iter  = 11*klmd ;

   toler = 0.0001 ;
   error = 0.0 ;

   /* input/output matrices & vectors */

   q     = (real *) calloc( 1, sizeof(real) * klm2d*n2d ) ;
   x     = (real *) calloc( 1, sizeof(real) * n2d ) ;
   res   = (real *) calloc( 1, sizeof(real) * klmd ) ;

   /* workspaces */

   cu    = (real *)    calloc( 1, sizeof(real) * 2*nklmd ) ;
   iu    = (integer *) calloc( 1, sizeof(integer) * 2*nklmd ) ;
   s     = (integer *) calloc( 1, sizeof(integer) * klmd ) ;

   /* load matrices & vectors */

   for( jj=0 ; jj < nvec ; jj++ )
      for( ii=0 ; ii < ndim ; ii++ )
         q[ii+jj*klm2d] = A[jj][ii] ;   /* matrix */

   for( ii=0 ; ii < ndim ; ii++ )
      q[ii+nvec*klm2d] = z[ii] ;        /* vector */

   if( cony ){
     for( jj=0 ; jj < nvec ; jj++ )     /* constraints on solution */
       x[jj] = (y[jj] < 0.0) ? -1.0
              :(y[jj] > 0.0) ?  1.0 : 0.0 ;
   }

   if( conr ){
     for( ii=0 ; ii < ndim ; ii++ )     /* constraints on resids */
       res[ii] = (rez[ii] < 0.0) ? -1.0
                :(rez[ii] > 0.0) ?  1.0 : 0.0 ;
   }

   /*-- do the work --*/

   cl1_fort( &k, &l, &m, &n,
             &klmd, &klm2d, &nklmd, &n2d,
             q, &kode, &toler, &iter, x, res, &error, cu, iu, s) ;

   free(q) ; free(cu) ; free(iu) ; free(s) ;

   if( kode != 0 ){
     free(x) ; free(res) ;
#if 0
     switch( kode ){
       case 1: fprintf(stderr,"** cl1_solve ERROR: no feasible solution!\n"); break;
       case 2: fprintf(stderr,"** cl1_solve ERROR: rounding errors!\n")     ; break;
       case 3: fprintf(stderr,"** cl1_solve ERROR: max iterations!\n")      ; break;
      default: fprintf(stderr,"** cl1_solve ERROR: unknown problem!\n")     ; break;
     }
#endif
     return (float)(-kode) ;
   }

   /*-- copy results into output --*/

   for( jj=0 ; jj < nvec ; jj++ ) y[jj] = (float) x[jj] ;

   for( ii=0 ; ii < ndim ; ii++ ) rez[ii] = (float) res[ii] ;

   free(res); free(x); return (float)error;
}

#if 0
/*---------------------------------------------------------------------
  Find a set of coefficients ec[] so that

                              j=nvec-1 {                         }
      S[i] =  base_vec[i] + SUM        { ec[j] * extra_vec[j][i] }
                              j=0      {                         }

  is non-negative for i=0..ndim-1, and so that

        i=ndim-1 {      }
      SUM        { S[i] }
        i=0      {      }

  is as small as possible.

  The return value of the function is 0 if everything worked, and
  nonzero if an error occured.  If the function worked, then the
  coefficients are returned in ec[] (which must be allocated by
  the caller).

  Method: uses the CL1 subroutine from the TOMS library;
          the f2c translation appears at the end of this file

  N.B.: NOT TESTED
-----------------------------------------------------------------------*/

int cl1_pos_sum( int ndim , int nvec ,
                 float * base_vec , float ** extra_vec , float * ec )
{
   /* internal variables */

   int jj , ii ;

   /* variables for CL1 */

   integer k,l,m,n,klmd,klm2d,nklmd,n2d , kode,iter , *iu,*s ;
   real *q , toler , *x , *res , error , *cu ;

   /*-- check inputs --*/

   if( ndim < 1 || nvec < 1 )                                 return 4 ;
   if( base_vec == NULL || extra_vec == NULL || ec == NULL )  return 4 ;
   for( jj=0 ; jj < nvec ; jj++ ) if( extra_vec[jj] == NULL ) return 4 ;

   /*-- setup call to CL1 --*/

   k     = ndim ;
   l     = 0 ;
   m     = 0 ;
   n     = nvec ;

   klmd  = k+l+m ;
   klm2d = k+l+m+2 ;
   nklmd = n+k+l+m ;
   n2d   = n+2 ;

   kode  = 1 ;
   iter  = 10*klmd ;

   toler = 0.0001 ;
   error = 0.0 ;

   /* input/output matrices & vectors */

   q     = (real *) malloc( sizeof(real) * klm2d*n2d ) ;
   x     = (real *) malloc( sizeof(real) * n2d ) ;
   res   = (real *) malloc( sizeof(real) * klmd ) ;

   /* workspaces */

   cu    = (real *) malloc( sizeof(real) * 2*nklmd ) ;
   iu    = (integer *) malloc( sizeof(integer) * 2*nklmd ) ;
   s     = (integer *) malloc( sizeof(integer) * klmd ) ;

   /* load matrices & vectors */

   for( jj=0 ; jj < nvec ; jj++ )
      for( ii=0 ; ii < ndim ; ii++ )
         q[ii+jj*klm2d] = extra_vec[jj][ii] ;

   for( ii=0 ; ii < ndim ; ii++ )
      q[ii+nvec*klm2d] = base_vec[ii] ;

   for( jj=0 ; jj < nvec ; jj++ ) x[jj] = 0.0 ;

   for( ii=0 ; ii < ndim ; ii++ ) res[ii] = 1.0 ;

   /*-- do the work --*/

   cl1_fort( &k, &l, &m, &n,
             &klmd, &klm2d, &nklmd, &n2d,
             q, &kode, &toler, &iter, x, res, &error, cu, iu, s) ;

   free(q) ; free(res) ; free(cu) ; free(iu) ; free(s) ;

   if( kode != 0 ){ free(x) ; return (int)kode ; }

   /*-- copy results into output --*/

   for( jj=0 ; jj < nvec ; jj++ ) ec[jj] = (float)(-x[jj]) ;

   free(x) ; return 0 ;
}
#endif

/*======================================================================
   Translated from the TOMS library CL1 algorithm
========================================================================*/

/* cl1.f -- translated by f2c (version 19970805).
   You must link the resulting object file with the libraries:
	-lconverted_from_fortran -lm   (in that order)
*/

static int cl1_fort(integer *k, integer *l, integer *m, integer *n,
	integer *klmd, integer *klm2d, integer *nklmd, integer *n2d, real *q,
	integer *kode, real *toler, integer *iter, real *x, real *res, real *
	error, real *cu, integer *iu, integer *s)
{
    /* System generated locals */
    integer q_dim1, q_offset, i__1, i__2;
    real r__1;

    /* Local variables ['static' removed by RWCox] */
    integer iimn, nklm;
    real xmin, xmax;
    integer iout=0, i__, j;
    real z__;
    integer iineg, maxit, n1, n2;
    real pivot;
    integer ia, ii, kk, in=0, nk, js;
    real sn;
    integer iphase, kforce;
    real zu, zv;
    integer nk1;
    real tpivot;
    integer klm, jmn, nkl, jpn;
    real cuv;
    doublereal sum;
    integer klm1, klm2, nkl1;


/* THIS SUBROUTINE USES A MODIFICATION OF THE SIMPLEX */
/* METHOD OF LINEAR PROGRAMMING TO CALCULATE AN L1 SOLUTION */
/* TO A K BY N SYSTEM OF LINEAR EQUATIONS */
/*             AX=B */
/* SUBJECT TO L LINEAR EQUALITY CONSTRAINTS */
/*             CX=D */
/* AND M LINEAR INEQUALITY CONSTRAINTS */
/*             EX.LE.F. */

/* DESCRIPTION OF PARAMETERS */

/* K      NUMBER OF ROWS OF THE MATRIX A (K.GE.1). */
/* L      NUMBER OF ROWS OF THE MATRIX C (L.GE.0). */
/* M      NUMBER OF ROWS OF THE MATRIX E (M.GE.0). */
/* N      NUMBER OF COLUMNS OF THE MATRICES A,C,E (N.GE.1). */
/* KLMD   SET TO AT LEAST K+L+M FOR ADJUSTABLE DIMENSIONS. */
/* KLM2D  SET TO AT LEAST K+L+M+2 FOR ADJUSTABLE DIMENSIONS. */
/* NKLMD  SET TO AT LEAST N+K+L+M FOR ADJUSTABLE DIMENSIONS. */
/* N2D    SET TO AT LEAST N+2 FOR ADJUSTABLE DIMENSIONS */

/* Q      TWO DIMENSIONAL REAL ARRAY WITH KLM2D ROWS AND */
/*        AT LEAST N2D COLUMNS. */
/*        ON ENTRY THE MATRICES A,C AND E, AND THE VECTORS */
/*        B,D AND F MUST BE STORED IN THE FIRST K+L+M ROWS */
/*        AND N+1 COLUMNS OF Q AS FOLLOWS */
/*             A B */
/*         Q = C D */
/*             E F */
/*        THESE VALUES ARE DESTROYED BY THE SUBROUTINE. */

/* KODE   A CODE USED ON ENTRY TO, AND EXIT */
/*        FROM, THE SUBROUTINE. */
/*        ON ENTRY, THIS SHOULD NORMALLY BE SET TO 0. */
/*        HOWEVER, IF CERTAIN NONNEGATIVITY CONSTRAINTS */
/*        ARE TO BE INCLUDED IMPLICITLY, RATHER THAN */
/*        EXPLICITLY IN THE CONSTRAINTS EX.LE.F, THEN KODE */
/*        SHOULD BE SET TO 1, AND THE NONNEGATIVITY */
/*        CONSTRAINTS INCLUDED IN THE ARRAYS X AND */
/*        RES (SEE BELOW). */
/*        ON EXIT, KODE HAS ONE OF THE */
/*        FOLLOWING VALUES */
/*             0- OPTIMAL SOLUTION FOUND, */
/*             1- NO FEASIBLE SOLUTION TO THE */
/*                CONSTRAINTS, */
/*             2- CALCULATIONS TERMINATED */
/*                PREMATURELY DUE TO ROUNDING ERRORS, */
/*             3- MAXIMUM NUMBER OF ITERATIONS REACHED. */

/* TOLER  A SMALL POSITIVE TOLERANCE. EMPIRICAL */
/*        EVIDENCE SUGGESTS TOLER = 10**(-D*2/3), */
/*        WHERE D REPRESENTS THE NUMBER OF DECIMAL */
/*        DIGITS OF ACCURACY AVAILABLE. ESSENTIALLY, */
/*        THE SUBROUTINE CANNOT DISTINGUISH BETWEEN ZERO */
/*        AND ANY QUANTITY WHOSE MAGNITUDE DOES NOT EXCEED */
/*        TOLER. IN PARTICULAR, IT WILL NOT PIVOT ON ANY */
/*        NUMBER WHOSE MAGNITUDE DOES NOT EXCEED TOLER. */

/* ITER   ON ENTRY ITER MUST CONTAIN AN UPPER BOUND ON */
/*        THE MAXIMUM NUMBER OF ITERATIONS ALLOWED. */
/*        A SUGGESTED VALUE IS 10*(K+L+M). ON EXIT ITER */
/*        GIVES THE NUMBER OF SIMPLEX ITERATIONS. */

/* X      ONE DIMENSIONAL REAL ARRAY OF SIZE AT LEAST N2D. */
/*        ON EXIT THIS ARRAY CONTAINS A */
/*        SOLUTION TO THE L1 PROBLEM. IF KODE=1 */
/*        ON ENTRY, THIS ARRAY IS ALSO USED TO INCLUDE */
/*        SIMPLE NONNEGATIVITY CONSTRAINTS ON THE */
/*        VARIABLES. THE VALUES -1, 0, OR 1 */
/*        FOR X(J) INDICATE THAT THE J-TH VARIABLE */
/*        IS RESTRICTED TO BE .LE.0, UNRESTRICTED, */
/*        OR .GE.0 RESPECTIVELY. */

/* RES    ONE DIMENSIONAL REAL ARRAY OF SIZE AT LEAST KLMD. */
/*        ON EXIT THIS CONTAINS THE RESIDUALS B-AX */
/*        IN THE FIRST K COMPONENTS, D-CX IN THE */
/*        NEXT L COMPONENTS (THESE WILL BE =0),AND */
/*        F-EX IN THE NEXT M COMPONENTS. IF KODE=1 ON */
/*        ENTRY, THIS ARRAY IS ALSO USED TO INCLUDE SIMPLE */
/*        NONNEGATIVITY CONSTRAINTS ON THE RESIDUALS */
/*        B-AX. THE VALUES -1, 0, OR 1 FOR RES(I) */
/*        INDICATE THAT THE I-TH RESIDUAL (1.LE.I.LE.K) IS */
/*        RESTRICTED TO BE .LE.0, UNRESTRICTED, OR .GE.0 */
/*        RESPECTIVELY. */

/* ERROR  ON EXIT, THIS GIVES THE MINIMUM SUM OF */
/*        ABSOLUTE VALUES OF THE RESIDUALS. */

/* CU     A TWO DIMENSIONAL REAL ARRAY WITH TWO ROWS AND */
/*        AT LEAST NKLMD COLUMNS USED FOR WORKSPACE. */

/* IU     A TWO DIMENSIONAL INTEGER ARRAY WITH TWO ROWS AND */
/*        AT LEAST NKLMD COLUMNS USED FOR WORKSPACE. */

/* S      INTEGER ARRAY OF SIZE AT LEAST KLMD, USED FOR */
/*        WORKSPACE. */

/* IF YOUR FORTRAN COMPILER PERMITS A SINGLE COLUMN OF A TWO */
/* DIMENSIONAL ARRAY TO BE PASSED TO A ONE DIMENSIONAL ARRAY */
/* THROUGH A SUBROUTINE CALL, CONSIDERABLE SAVINGS IN */
/* EXECUTION TIME MAY BE ACHIEVED THROUGH THE USE OF THE */
/* FOLLOWING SUBROUTINE, WHICH OPERATES ON COLUMN VECTORS. */
/*     SUBROUTINE COL(V1, V2, XMLT, NOTROW, K) */
/* THIS SUBROUTINE ADDS TO THE VECTOR V1 A MULTIPLE OF THE */
/* VECTOR V2 (ELEMENTS 1 THROUGH K EXCLUDING NOTROW). */
/*     DIMENSION V1(K), V2(K) */
/*     KEND = NOTROW - 1 */
/*     KSTART = NOTROW + 1 */
/*     IF (KEND .LT. 1) GO TO 20 */
/*     DO 10 I=1,KEND */
/*        V1(I) = V1(I) + XMLT*V2(I) */
/*  10 CONTINUE */
/*     IF(KSTART .GT. K) GO TO 40 */
/*  20 DO 30 I=KSTART,K */
/*       V1(I) = V1(I) + XMLT*V2(I) */
/*  30 CONTINUE */
/*  40 RETURN */
/*     END */
/* SEE COMMENTS FOLLOWING STATEMENT LABELLED 440 FOR */
/* INSTRUCTIONS ON THE IMPLEMENTATION OF THIS MODIFICATION. */
/* -----------------------------------------------------------------------
 */

/* INITIALIZATION. */

    /* Parameter adjustments */
    --s;
    --res;
    iu -= 3;
    cu -= 3;
    --x;
    q_dim1 = *klm2d;
    q_offset = q_dim1 + 1;
    q -= q_offset;

    /* Function Body */
    maxit = *iter;
    n1 = *n + 1;
    n2 = *n + 2;
    nk = *n + *k;
    nk1 = nk + 1;
    nkl = nk + *l;
    nkl1 = nkl + 1;
    klm = *k + *l + *m;
    klm1 = klm + 1;
    klm2 = klm + 2;
    nklm = *n + klm;
    kforce = 1;
    *iter = 0;
    js = 1;
    ia = 0;
/* SET UP LABELS IN Q. */
    i__1 = *n;
    for (j = 1; j <= i__1; ++j) {
	q[klm2 + j * q_dim1] = (real) j;
/* L10: */
    }
    i__1 = klm;
    for (i__ = 1; i__ <= i__1; ++i__) {
	q[i__ + n2 * q_dim1] = (real) (*n + i__);
	if (q[i__ + n1 * q_dim1] >= 0.f) {
	    goto L30;
	}
	i__2 = n2;
	for (j = 1; j <= i__2; ++j) {
	    q[i__ + j * q_dim1] = -q[i__ + j * q_dim1];
/* L20: */
	}
L30:
	;
    }
/* SET UP PHASE 1 COSTS. */
    iphase = 2;
    i__1 = nklm;
    for (j = 1; j <= i__1; ++j) {
	cu[(j << 1) + 1] = 0.f;
	cu[(j << 1) + 2] = 0.f;
	iu[(j << 1) + 1] = 0;
	iu[(j << 1) + 2] = 0;
/* L40: */
    }
    if (*l == 0) {
	goto L60;
    }
    i__1 = nkl;
    for (j = nk1; j <= i__1; ++j) {
	cu[(j << 1) + 1] = 1.f;
	cu[(j << 1) + 2] = 1.f;
	iu[(j << 1) + 1] = 1;
	iu[(j << 1) + 2] = 1;
/* L50: */
    }
    iphase = 1;
L60:
    if (*m == 0) {
	goto L80;
    }
    i__1 = nklm;
    for (j = nkl1; j <= i__1; ++j) {
	cu[(j << 1) + 2] = 1.f;
	iu[(j << 1) + 2] = 1;
	jmn = j - *n;
	if (q[jmn + n2 * q_dim1] < 0.f) {
	    iphase = 1;
	}
/* L70: */
    }
L80:
    if (*kode == 0) {
	goto L150;
    }
    i__1 = *n;
    for (j = 1; j <= i__1; ++j) {
	if ((r__1 = x[j]) < 0.f) {
	    goto L90;
	} else if (r__1 == 0) {
	    goto L110;
	} else {
	    goto L100;
	}
L90:
	cu[(j << 1) + 1] = 1.f;
	iu[(j << 1) + 1] = 1;
	goto L110;
L100:
	cu[(j << 1) + 2] = 1.f;
	iu[(j << 1) + 2] = 1;
L110:
	;
    }
    i__1 = *k;
    for (j = 1; j <= i__1; ++j) {
	jpn = j + *n;
	if ((r__1 = res[j]) < 0.f) {
	    goto L120;
	} else if (r__1 == 0) {
	    goto L140;
	} else {
	    goto L130;
	}
L120:
	cu[(jpn << 1) + 1] = 1.f;
	iu[(jpn << 1) + 1] = 1;
	if (q[j + n2 * q_dim1] > 0.f) {
	    iphase = 1;
	}
	goto L140;
L130:
	cu[(jpn << 1) + 2] = 1.f;
	iu[(jpn << 1) + 2] = 1;
	if (q[j + n2 * q_dim1] < 0.f) {
	    iphase = 1;
	}
L140:
	;
    }
L150:
    if (iphase == 2) {
	goto L500;
    }
/* COMPUTE THE MARGINAL COSTS. */
L160:
    i__1 = n1;
    for (j = js; j <= i__1; ++j) {
	sum = 0.;
	i__2 = klm;
	for (i__ = 1; i__ <= i__2; ++i__) {
	    ii = q[i__ + n2 * q_dim1];
	    if (ii < 0) {
		goto L170;
	    }
	    z__ = cu[(ii << 1) + 1];
	    goto L180;
L170:
	    iineg = -ii;
	    z__ = cu[(iineg << 1) + 2];
L180:
	    sum += (doublereal) q[i__ + j * q_dim1] * (doublereal) z__;
/* L190: */
	}
	q[klm1 + j * q_dim1] = sum;
/* L200: */
    }
    i__1 = *n;
    for (j = js; j <= i__1; ++j) {
	ii = q[klm2 + j * q_dim1];
	if (ii < 0) {
	    goto L210;
	}
	z__ = cu[(ii << 1) + 1];
	goto L220;
L210:
	iineg = -ii;
	z__ = cu[(iineg << 1) + 2];
L220:
	q[klm1 + j * q_dim1] -= z__;
/* L230: */
    }
/* DETERMINE THE VECTOR TO ENTER THE BASIS. */
L240:
    xmax = 0.f;
    if (js > *n) {
	goto L490;
    }
    i__1 = *n;
    for (j = js; j <= i__1; ++j) {
	zu = q[klm1 + j * q_dim1];
	ii = q[klm2 + j * q_dim1];
	if (ii > 0) {
	    goto L250;
	}
	ii = -ii;
	zv = zu;
	zu = -zu - cu[(ii << 1) + 1] - cu[(ii << 1) + 2];
	goto L260;
L250:
	zv = -zu - cu[(ii << 1) + 1] - cu[(ii << 1) + 2];
L260:
	if (kforce == 1 && ii > *n) {
	    goto L280;
	}
	if (iu[(ii << 1) + 1] == 1) {
	    goto L270;
	}
	if (zu <= xmax) {
	    goto L270;
	}
	xmax = zu;
	in = j;
L270:
	if (iu[(ii << 1) + 2] == 1) {
	    goto L280;
	}
	if (zv <= xmax) {
	    goto L280;
	}
	xmax = zv;
	in = j;
L280:
	;
    }
    if (xmax <= *toler) {
	goto L490;
    }
    if (q[klm1 + in * q_dim1] == xmax) {
	goto L300;
    }
    i__1 = klm2;
    for (i__ = 1; i__ <= i__1; ++i__) {
	q[i__ + in * q_dim1] = -q[i__ + in * q_dim1];
/* L290: */
    }
    q[klm1 + in * q_dim1] = xmax;
/* DETERMINE THE VECTOR TO LEAVE THE BASIS. */
L300:
    if (iphase == 1 || ia == 0) {
	goto L330;
    }
    xmax = 0.f;
    i__1 = ia;
    for (i__ = 1; i__ <= i__1; ++i__) {
	z__ = (r__1 = q[i__ + in * q_dim1], dabs(r__1));
	if (z__ <= xmax) {
	    goto L310;
	}
	xmax = z__;
	iout = i__;
L310:
	;
    }
    if (xmax <= *toler) {
	goto L330;
    }
    i__1 = n2;
    for (j = 1; j <= i__1; ++j) {
	z__ = q[ia + j * q_dim1];
	q[ia + j * q_dim1] = q[iout + j * q_dim1];
	q[iout + j * q_dim1] = z__;
/* L320: */
    }
    iout = ia;
    --ia;
    pivot = q[iout + in * q_dim1];
    goto L420;
L330:
    kk = 0;
    i__1 = klm;
    for (i__ = 1; i__ <= i__1; ++i__) {
	z__ = q[i__ + in * q_dim1];
	if (z__ <= *toler) {
	    goto L340;
	}
	++kk;
	res[kk] = q[i__ + n1 * q_dim1] / z__;
	s[kk] = i__;
L340:
	;
    }
L350:
    if (kk > 0) {
	goto L360;
    }
    *kode = 2;
    goto L590;
L360:
    xmin = res[1];
    iout = s[1];
    j = 1;
    if (kk == 1) {
	goto L380;
    }
    i__1 = kk;
    for (i__ = 2; i__ <= i__1; ++i__) {
	if (res[i__] >= xmin) {
	    goto L370;
	}
	j = i__;
	xmin = res[i__];
	iout = s[i__];
L370:
	;
    }
    res[j] = res[kk];
    s[j] = s[kk];
L380:
    --kk;
    pivot = q[iout + in * q_dim1];
    ii = q[iout + n2 * q_dim1];
    if (iphase == 1) {
	goto L400;
    }
    if (ii < 0) {
	goto L390;
    }
    if (iu[(ii << 1) + 2] == 1) {
	goto L420;
    }
    goto L400;
L390:
    iineg = -ii;
    if (iu[(iineg << 1) + 1] == 1) {
	goto L420;
    }
L400:
    ii = abs(ii);
    cuv = cu[(ii << 1) + 1] + cu[(ii << 1) + 2];
    if (q[klm1 + in * q_dim1] - pivot * cuv <= *toler) {
	goto L420;
    }
/* BYPASS INTERMEDIATE VERTICES. */
    i__1 = n1;
    for (j = js; j <= i__1; ++j) {
	z__ = q[iout + j * q_dim1];
	q[klm1 + j * q_dim1] -= z__ * cuv;
	q[iout + j * q_dim1] = -z__;
/* L410: */
    }
    q[iout + n2 * q_dim1] = -q[iout + n2 * q_dim1];
    goto L350;
/* GAUSS-JORDAN ELIMINATION. */
L420:
    if (*iter < maxit) {
	goto L430;
    }
    *kode = 3;
    goto L590;
L430:
    ++(*iter);
    i__1 = n1;
    for (j = js; j <= i__1; ++j) {
	if (j != in) {
	    q[iout + j * q_dim1] /= pivot;
	}
/* L440: */
    }
/* IF PERMITTED, USE SUBROUTINE COL OF THE DESCRIPTION */
/* SECTION AND REPLACE THE FOLLOWING SEVEN STATEMENTS DOWN */
/* TO AND INCLUDING STATEMENT NUMBER 460 BY.. */
/*     DO 460 J=JS,N1 */
/*        IF(J .EQ. IN) GO TO 460 */
/*        Z = -Q(IOUT,J) */
/*        CALL COL(Q(1,J), Q(1,IN), Z, IOUT, KLM1) */
/* 460 CONTINUE */
    i__1 = n1;
    for (j = js; j <= i__1; ++j) {
	if (j == in) {
	    goto L460;
	}
	z__ = -q[iout + j * q_dim1];
	i__2 = klm1;
	for (i__ = 1; i__ <= i__2; ++i__) {
	    if (i__ != iout) {
		q[i__ + j * q_dim1] += z__ * q[i__ + in * q_dim1];
	    }
/* L450: */
	}
L460:
	;
    }
    tpivot = -pivot;
    i__1 = klm1;
    for (i__ = 1; i__ <= i__1; ++i__) {
	if (i__ != iout) {
	    q[i__ + in * q_dim1] /= tpivot;
	}
/* L470: */
    }
    q[iout + in * q_dim1] = 1.f / pivot;
    z__ = q[iout + n2 * q_dim1];
    q[iout + n2 * q_dim1] = q[klm2 + in * q_dim1];
    q[klm2 + in * q_dim1] = z__;
    ii = dabs(z__);
    if (iu[(ii << 1) + 1] == 0 || iu[(ii << 1) + 2] == 0) {
	goto L240;
    }
    i__1 = klm2;
    for (i__ = 1; i__ <= i__1; ++i__) {
	z__ = q[i__ + in * q_dim1];
	q[i__ + in * q_dim1] = q[i__ + js * q_dim1];
	q[i__ + js * q_dim1] = z__;
/* L480: */
    }
    ++js;
    goto L240;
/* TEST FOR OPTIMALITY. */
L490:
    if (kforce == 0) {
	goto L580;
    }
    if (iphase == 1 && q[klm1 + n1 * q_dim1] <= *toler) {
	goto L500;
    }
    kforce = 0;
    goto L240;
/* SET UP PHASE 2 COSTS. */
L500:
    iphase = 2;
    i__1 = nklm;
    for (j = 1; j <= i__1; ++j) {
	cu[(j << 1) + 1] = 0.f;
	cu[(j << 1) + 2] = 0.f;
/* L510: */
    }
    i__1 = nk;
    for (j = n1; j <= i__1; ++j) {
	cu[(j << 1) + 1] = 1.f;
	cu[(j << 1) + 2] = 1.f;
/* L520: */
    }
    i__1 = klm;
    for (i__ = 1; i__ <= i__1; ++i__) {
	ii = q[i__ + n2 * q_dim1];
	if (ii > 0) {
	    goto L530;
	}
	ii = -ii;
	if (iu[(ii << 1) + 2] == 0) {
	    goto L560;
	}
	cu[(ii << 1) + 2] = 0.f;
	goto L540;
L530:
	if (iu[(ii << 1) + 1] == 0) {
	    goto L560;
	}
	cu[(ii << 1) + 1] = 0.f;
L540:
	++ia;
	i__2 = n2;
	for (j = 1; j <= i__2; ++j) {
	    z__ = q[ia + j * q_dim1];
	    q[ia + j * q_dim1] = q[i__ + j * q_dim1];
	    q[i__ + j * q_dim1] = z__;
/* L550: */
	}
L560:
	;
    }
    goto L160;
L570:
    if (q[klm1 + n1 * q_dim1] <= *toler) {
	goto L500;
    }
    *kode = 1;
    goto L590;
L580:
    if (iphase == 1) {
	goto L570;
    }
/* PREPARE OUTPUT. */
    *kode = 0;
L590:
    sum = 0.;
    i__1 = *n;
    for (j = 1; j <= i__1; ++j) {
	x[j] = 0.f;
/* L600: */
    }
    i__1 = klm;
    for (i__ = 1; i__ <= i__1; ++i__) {
	res[i__] = 0.f;
/* L610: */
    }
    i__1 = klm;
    for (i__ = 1; i__ <= i__1; ++i__) {
	ii = q[i__ + n2 * q_dim1];
	sn = 1.f;
	if (ii > 0) {
	    goto L620;
	}
	ii = -ii;
	sn = -1.f;
L620:
	if (ii > *n) {
	    goto L630;
	}
	x[ii] = sn * q[i__ + n1 * q_dim1];
	goto L640;
L630:
	iimn = ii - *n;
	res[iimn] = sn * q[i__ + n1 * q_dim1];
	if (ii >= n1 && ii <= nk) {
	    sum += (doublereal) q[i__ + n1 * q_dim1];
	}
L640:
	;
    }
    *error = sum;
    return 0;
} /* cl1_ */

/*===================================================================
      SUBROUTINE CL1(K, L, M, N, KLMD, KLM2D, NKLMD, N2D,
     * Q, KODE, TOLER, ITER, X, RES, ERROR, CU, IU, S)
C
C THIS SUBROUTINE USES A MODIFICATION OF THE SIMPLEX
C METHOD OF LINEAR PROGRAMMING TO CALCULATE AN L1 SOLUTION
C TO A K BY N SYSTEM OF LINEAR EQUATIONS
C             AX=B
C SUBJECT TO L LINEAR EQUALITY CONSTRAINTS
C             CX=D
C AND M LINEAR INEQUALITY CONSTRAINTS
C             EX.LE.F.
C
C DESCRIPTION OF PARAMETERS
C
C K      NUMBER OF ROWS OF THE MATRIX A (K.GE.1).
C L      NUMBER OF ROWS OF THE MATRIX C (L.GE.0).
C M      NUMBER OF ROWS OF THE MATRIX E (M.GE.0).
C N      NUMBER OF COLUMNS OF THE MATRICES A,C,E (N.GE.1).
C KLMD   SET TO AT LEAST K+L+M FOR ADJUSTABLE DIMENSIONS.
C KLM2D  SET TO AT LEAST K+L+M+2 FOR ADJUSTABLE DIMENSIONS.
C NKLMD  SET TO AT LEAST N+K+L+M FOR ADJUSTABLE DIMENSIONS.
C N2D    SET TO AT LEAST N+2 FOR ADJUSTABLE DIMENSIONS
C
C Q      TWO DIMENSIONAL REAL ARRAY WITH KLM2D ROWS AND
C        AT LEAST N2D COLUMNS.
C        ON ENTRY THE MATRICES A,C AND E, AND THE VECTORS
C        B,D AND F MUST BE STORED IN THE FIRST K+L+M ROWS
C        AND N+1 COLUMNS OF Q AS FOLLOWS
C             A B
C         Q = C D
C             E F
C        THESE VALUES ARE DESTROYED BY THE SUBROUTINE.
C
C KODE   A CODE USED ON ENTRY TO, AND EXIT
C        FROM, THE SUBROUTINE.
C        ON ENTRY, THIS SHOULD NORMALLY BE SET TO 0.
C        HOWEVER, IF CERTAIN NONNEGATIVITY CONSTRAINTS
C        ARE TO BE INCLUDED IMPLICITLY, RATHER THAN
C        EXPLICITLY IN THE CONSTRAINTS EX.LE.F, THEN KODE
C        SHOULD BE SET TO 1, AND THE NONNEGATIVITY
C        CONSTRAINTS INCLUDED IN THE ARRAYS X AND
C        RES (SEE BELOW).
C        ON EXIT, KODE HAS ONE OF THE
C        FOLLOWING VALUES
C             0- OPTIMAL SOLUTION FOUND,
C             1- NO FEASIBLE SOLUTION TO THE
C                CONSTRAINTS,
C             2- CALCULATIONS TERMINATED
C                PREMATURELY DUE TO ROUNDING ERRORS,
C             3- MAXIMUM NUMBER OF ITERATIONS REACHED.
C
C TOLER  A SMALL POSITIVE TOLERANCE. EMPIRICAL
C        EVIDENCE SUGGESTS TOLER = 10**(-D*2/3),
C        WHERE D REPRESENTS THE NUMBER OF DECIMAL
C        DIGITS OF ACCURACY AVAILABLE. ESSENTIALLY,
C        THE SUBROUTINE CANNOT DISTINGUISH BETWEEN ZERO
C        AND ANY QUANTITY WHOSE MAGNITUDE DOES NOT EXCEED
C        TOLER. IN PARTICULAR, IT WILL NOT PIVOT ON ANY
C        NUMBER WHOSE MAGNITUDE DOES NOT EXCEED TOLER.
C
C ITER   ON ENTRY ITER MUST CONTAIN AN UPPER BOUND ON
C        THE MAXIMUM NUMBER OF ITERATIONS ALLOWED.
C        A SUGGESTED VALUE IS 10*(K+L+M). ON EXIT ITER
C        GIVES THE NUMBER OF SIMPLEX ITERATIONS.
C
C X      ONE DIMENSIONAL REAL ARRAY OF SIZE AT LEAST N2D.
C        ON EXIT THIS ARRAY CONTAINS A
C        SOLUTION TO THE L1 PROBLEM. IF KODE=1
C        ON ENTRY, THIS ARRAY IS ALSO USED TO INCLUDE
C        SIMPLE NONNEGATIVITY CONSTRAINTS ON THE
C        VARIABLES. THE VALUES -1, 0, OR 1
C        FOR X(J) INDICATE THAT THE J-TH VARIABLE
C        IS RESTRICTED TO BE .LE.0, UNRESTRICTED,
C        OR .GE.0 RESPECTIVELY.
C
C RES    ONE DIMENSIONAL REAL ARRAY OF SIZE AT LEAST KLMD.
C        ON EXIT THIS CONTAINS THE RESIDUALS B-AX
C        IN THE FIRST K COMPONENTS, D-CX IN THE
C        NEXT L COMPONENTS (THESE WILL BE =0),AND
C        F-EX IN THE NEXT M COMPONENTS. IF KODE=1 ON
C        ENTRY, THIS ARRAY IS ALSO USED TO INCLUDE SIMPLE
C        NONNEGATIVITY CONSTRAINTS ON THE RESIDUALS
C        B-AX. THE VALUES -1, 0, OR 1 FOR RES(I)
C        INDICATE THAT THE I-TH RESIDUAL (1.LE.I.LE.K) IS
C        RESTRICTED TO BE .LE.0, UNRESTRICTED, OR .GE.0
C        RESPECTIVELY.
C
C ERROR  ON EXIT, THIS GIVES THE MINIMUM SUM OF
C        ABSOLUTE VALUES OF THE RESIDUALS.
C
C CU     A TWO DIMENSIONAL REAL ARRAY WITH TWO ROWS AND
C        AT LEAST NKLMD COLUMNS USED FOR WORKSPACE.
C
C IU     A TWO DIMENSIONAL INTEGER ARRAY WITH TWO ROWS AND
C        AT LEAST NKLMD COLUMNS USED FOR WORKSPACE.
C
C S      INTEGER ARRAY OF SIZE AT LEAST KLMD, USED FOR
C        WORKSPACE.
C
C IF YOUR FORTRAN COMPILER PERMITS A SINGLE COLUMN OF A TWO
C DIMENSIONAL ARRAY TO BE PASSED TO A ONE DIMENSIONAL ARRAY
C THROUGH A SUBROUTINE CALL, CONSIDERABLE SAVINGS IN
C EXECUTION TIME MAY BE ACHIEVED THROUGH THE USE OF THE
C FOLLOWING SUBROUTINE, WHICH OPERATES ON COLUMN VECTORS.
C     SUBROUTINE COL(V1, V2, XMLT, NOTROW, K)
C THIS SUBROUTINE ADDS TO THE VECTOR V1 A MULTIPLE OF THE
C VECTOR V2 (ELEMENTS 1 THROUGH K EXCLUDING NOTROW).
C     DIMENSION V1(K), V2(K)
C     KEND = NOTROW - 1
C     KSTART = NOTROW + 1
C     IF (KEND .LT. 1) GO TO 20
C     DO 10 I=1,KEND
C        V1(I) = V1(I) + XMLT*V2(I)
C  10 CONTINUE
C     IF(KSTART .GT. K) GO TO 40
C  20 DO 30 I=KSTART,K
C       V1(I) = V1(I) + XMLT*V2(I)
C  30 CONTINUE
C  40 RETURN
C     END
C SEE COMMENTS FOLLOWING STATEMENT LABELLED 440 FOR
C INSTRUCTIONS ON THE IMPLEMENTATION OF THIS MODIFICATION.
C-----------------------------------------------------------------------
      DOUBLE PRECISION SUM
      DOUBLE PRECISION DBLE
      REAL Q, X, Z, CU, SN, ZU, ZV, CUV, RES, XMAX, XMIN,
     * ERROR, PIVOT, TOLER, TPIVOT
      REAL ABS
      INTEGER I, J, K, L, M, N, S, IA, II, IN, IU, JS, KK,
     * NK, N1, N2, JMN, JPN, KLM, NKL, NK1, N2D, IIMN,
     * IOUT, ITER, KLMD, KLM1, KLM2, KODE, NKLM, NKL1,
     * KLM2D, MAXIT, NKLMD, IPHASE, KFORCE, IINEG
      INTEGER IABS
      DIMENSION Q(KLM2D,N2D), X(N2D), RES(KLMD),
     * CU(2,NKLMD), IU(2,NKLMD), S(KLMD)
C
C INITIALIZATION.
C
      MAXIT = ITER
      N1 = N + 1
      N2 = N + 2
      NK = N + K
      NK1 = NK + 1
      NKL = NK + L
      NKL1 = NKL + 1
      KLM = K + L + M
      KLM1 = KLM + 1
      KLM2 = KLM + 2
      NKLM = N + KLM
      KFORCE = 1
      ITER = 0
      JS = 1
      IA = 0
C SET UP LABELS IN Q.
      DO 10 J=1,N
         Q(KLM2,J) = J
   10 CONTINUE
      DO 30 I=1,KLM
         Q(I,N2) = N + I
         IF (Q(I,N1).GE.0.) GO TO 30
         DO 20 J=1,N2
            Q(I,J) = -Q(I,J)
   20    CONTINUE
   30 CONTINUE
C SET UP PHASE 1 COSTS.
      IPHASE = 2
      DO 40 J=1,NKLM
         CU(1,J) = 0.
         CU(2,J) = 0.
         IU(1,J) = 0
         IU(2,J) = 0
   40 CONTINUE
      IF (L.EQ.0) GO TO 60
      DO 50 J=NK1,NKL
         CU(1,J) = 1.
         CU(2,J) = 1.
         IU(1,J) = 1
         IU(2,J) = 1
   50 CONTINUE
      IPHASE = 1
   60 IF (M.EQ.0) GO TO 80
      DO 70 J=NKL1,NKLM
         CU(2,J) = 1.
         IU(2,J) = 1
         JMN = J - N
         IF (Q(JMN,N2).LT.0.) IPHASE = 1
   70 CONTINUE
   80 IF (KODE.EQ.0) GO TO 150
      DO 110 J=1,N
         IF (X(J)) 90, 110, 100
   90    CU(1,J) = 1.
         IU(1,J) = 1
         GO TO 110
  100    CU(2,J) = 1.
         IU(2,J) = 1
  110 CONTINUE
      DO 140 J=1,K
         JPN = J + N
         IF (RES(J)) 120, 140, 130
  120    CU(1,JPN) = 1.
         IU(1,JPN) = 1
         IF (Q(J,N2).GT.0.0) IPHASE = 1
         GO TO 140
  130    CU(2,JPN) = 1.
         IU(2,JPN) = 1
         IF (Q(J,N2).LT.0.0) IPHASE = 1
  140 CONTINUE
  150 IF (IPHASE.EQ.2) GO TO 500
C COMPUTE THE MARGINAL COSTS.
  160 DO 200 J=JS,N1
         SUM = 0.D0
         DO 190 I=1,KLM
            II = Q(I,N2)
            IF (II.LT.0) GO TO 170
            Z = CU(1,II)
            GO TO 180
  170       IINEG = -II
            Z = CU(2,IINEG)
  180       SUM = SUM + DBLE(Q(I,J))*DBLE(Z)
  190    CONTINUE
         Q(KLM1,J) = SUM
  200 CONTINUE
      DO 230 J=JS,N
         II = Q(KLM2,J)
         IF (II.LT.0) GO TO 210
         Z = CU(1,II)
         GO TO 220
  210    IINEG = -II
         Z = CU(2,IINEG)
  220    Q(KLM1,J) = Q(KLM1,J) - Z
  230 CONTINUE
C DETERMINE THE VECTOR TO ENTER THE BASIS.
  240 XMAX = 0.
      IF (JS.GT.N) GO TO 490
      DO 280 J=JS,N
         ZU = Q(KLM1,J)
         II = Q(KLM2,J)
         IF (II.GT.0) GO TO 250
         II = -II
         ZV = ZU
         ZU = -ZU - CU(1,II) - CU(2,II)
         GO TO 260
  250    ZV = -ZU - CU(1,II) - CU(2,II)
  260    IF (KFORCE.EQ.1 .AND. II.GT.N) GO TO 280
         IF (IU(1,II).EQ.1) GO TO 270
         IF (ZU.LE.XMAX) GO TO 270
         XMAX = ZU
         IN = J
  270    IF (IU(2,II).EQ.1) GO TO 280
         IF (ZV.LE.XMAX) GO TO 280
         XMAX = ZV
         IN = J
  280 CONTINUE
      IF (XMAX.LE.TOLER) GO TO 490
      IF (Q(KLM1,IN).EQ.XMAX) GO TO 300
      DO 290 I=1,KLM2
         Q(I,IN) = -Q(I,IN)
  290 CONTINUE
      Q(KLM1,IN) = XMAX
C DETERMINE THE VECTOR TO LEAVE THE BASIS.
  300 IF (IPHASE.EQ.1 .OR. IA.EQ.0) GO TO 330
      XMAX = 0.
      DO 310 I=1,IA
         Z = ABS(Q(I,IN))
         IF (Z.LE.XMAX) GO TO 310
         XMAX = Z
         IOUT = I
  310 CONTINUE
      IF (XMAX.LE.TOLER) GO TO 330
      DO 320 J=1,N2
         Z = Q(IA,J)
         Q(IA,J) = Q(IOUT,J)
         Q(IOUT,J) = Z
  320 CONTINUE
      IOUT = IA
      IA = IA - 1
      PIVOT = Q(IOUT,IN)
      GO TO 420
  330 KK = 0
      DO 340 I=1,KLM
         Z = Q(I,IN)
         IF (Z.LE.TOLER) GO TO 340
         KK = KK + 1
         RES(KK) = Q(I,N1)/Z
         S(KK) = I
  340 CONTINUE
  350 IF (KK.GT.0) GO TO 360
      KODE = 2
      GO TO 590
  360 XMIN = RES(1)
      IOUT = S(1)
      J = 1
      IF (KK.EQ.1) GO TO 380
      DO 370 I=2,KK
         IF (RES(I).GE.XMIN) GO TO 370
         J = I
         XMIN = RES(I)
         IOUT = S(I)
  370 CONTINUE
      RES(J) = RES(KK)
      S(J) = S(KK)
  380 KK = KK - 1
      PIVOT = Q(IOUT,IN)
      II = Q(IOUT,N2)
      IF (IPHASE.EQ.1) GO TO 400
      IF (II.LT.0) GO TO 390
      IF (IU(2,II).EQ.1) GO TO 420
      GO TO 400
  390 IINEG = -II
      IF (IU(1,IINEG).EQ.1) GO TO 420
  400 II = IABS(II)
      CUV = CU(1,II) + CU(2,II)
      IF (Q(KLM1,IN)-PIVOT*CUV.LE.TOLER) GO TO 420
C BYPASS INTERMEDIATE VERTICES.
      DO 410 J=JS,N1
         Z = Q(IOUT,J)
         Q(KLM1,J) = Q(KLM1,J) - Z*CUV
         Q(IOUT,J) = -Z
  410 CONTINUE
      Q(IOUT,N2) = -Q(IOUT,N2)
      GO TO 350
C GAUSS-JORDAN ELIMINATION.
  420 IF (ITER.LT.MAXIT) GO TO 430
      KODE = 3
      GO TO 590
  430 ITER = ITER + 1
      DO 440 J=JS,N1
         IF (J.NE.IN) Q(IOUT,J) = Q(IOUT,J)/PIVOT
  440 CONTINUE
C IF PERMITTED, USE SUBROUTINE COL OF THE DESCRIPTION
C SECTION AND REPLACE THE FOLLOWING SEVEN STATEMENTS DOWN
C TO AND INCLUDING STATEMENT NUMBER 460 BY..
C     DO 460 J=JS,N1
C        IF(J .EQ. IN) GO TO 460
C        Z = -Q(IOUT,J)
C        CALL COL(Q(1,J), Q(1,IN), Z, IOUT, KLM1)
C 460 CONTINUE
      DO 460 J=JS,N1
         IF (J.EQ.IN) GO TO 460
         Z = -Q(IOUT,J)
         DO 450 I=1,KLM1
            IF (I.NE.IOUT) Q(I,J) = Q(I,J) + Z*Q(I,IN)
  450    CONTINUE
  460 CONTINUE
      TPIVOT = -PIVOT
      DO 470 I=1,KLM1
         IF (I.NE.IOUT) Q(I,IN) = Q(I,IN)/TPIVOT
  470 CONTINUE
      Q(IOUT,IN) = 1./PIVOT
      Z = Q(IOUT,N2)
      Q(IOUT,N2) = Q(KLM2,IN)
      Q(KLM2,IN) = Z
      II = ABS(Z)
      IF (IU(1,II).EQ.0 .OR. IU(2,II).EQ.0) GO TO 240
      DO 480 I=1,KLM2
         Z = Q(I,IN)
         Q(I,IN) = Q(I,JS)
         Q(I,JS) = Z
  480 CONTINUE
      JS = JS + 1
      GO TO 240
C TEST FOR OPTIMALITY.
  490 IF (KFORCE.EQ.0) GO TO 580
      IF (IPHASE.EQ.1 .AND. Q(KLM1,N1).LE.TOLER) GO TO 500
      KFORCE = 0
      GO TO 240
C SET UP PHASE 2 COSTS.
  500 IPHASE = 2
      DO 510 J=1,NKLM
         CU(1,J) = 0.
         CU(2,J) = 0.
  510 CONTINUE
      DO 520 J=N1,NK
         CU(1,J) = 1.
         CU(2,J) = 1.
  520 CONTINUE
      DO 560 I=1,KLM
         II = Q(I,N2)
         IF (II.GT.0) GO TO 530
         II = -II
         IF (IU(2,II).EQ.0) GO TO 560
         CU(2,II) = 0.
         GO TO 540
  530    IF (IU(1,II).EQ.0) GO TO 560
         CU(1,II) = 0.
  540    IA = IA + 1
         DO 550 J=1,N2
            Z = Q(IA,J)
            Q(IA,J) = Q(I,J)
            Q(I,J) = Z
  550    CONTINUE
  560 CONTINUE
      GO TO 160
  570 IF (Q(KLM1,N1).LE.TOLER) GO TO 500
      KODE = 1
      GO TO 590
  580 IF (IPHASE.EQ.1) GO TO 570
C PREPARE OUTPUT.
      KODE = 0
  590 SUM = 0.D0
      DO 600 J=1,N
         X(J) = 0.
  600 CONTINUE
      DO 610 I=1,KLM
         RES(I) = 0.
  610 CONTINUE
      DO 640 I=1,KLM
         II = Q(I,N2)
         SN = 1.
         IF (II.GT.0) GO TO 620
         II = -II
         SN = -1.
  620    IF (II.GT.N) GO TO 630
         X(II) = SN*Q(I,N1)
         GO TO 640
  630    IIMN = II - N
         RES(IIMN) = SN*Q(I,N1)
         IF (II.GE.N1 .AND. II.LE.NK) SUM = SUM +
     *    DBLE(Q(I,N1))
  640 CONTINUE
      ERROR = SUM
      RETURN
      END
================================================================*/
