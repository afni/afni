/***** This code is part of the AFNI software package, which is   *****
 ***** partly in the public domain and partly covered by the GPL. *****
 ***** See https://afni.nimh.nih.gov/afni for more information.    *****/

#include <math.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include "converted_from_fortran.h"

/*---------------------------------------------------------------------------*/
/**--- prototypes for functions in translated part of this file, at end ----**/

static int wnnls_fort(real *w, integer *mdw, integer *me, integer *ma,
	integer *n, integer *l, real *prgopt, real *x, real *rnorm, integer *
	mode, integer *iwork, real *work) ;

static int h12_(integer *, integer *, integer *, integer *,
         real *, integer *, real *, real *, integer *, integer *, integer *);

static int lsi_(real *, integer *, integer *, integer *,
	    integer *, real *, real *, real *, integer *, real *, integer *);

static real sdot_(integer *, real *, integer *, real *, integer *);
static real snrm2_(integer *, real *, integer *);
static int sscal_(integer *, real *, real *, integer *);
static real sasum_(integer *, real *, integer *);
static int sswap_(integer *, real *, integer *, real *, integer *);
static int scopy_(integer *, real *, integer *, real *, integer *);
static int saxpy_(integer *, real *, real *, integer *, real *, integer *);
static int srotm_(integer *, real *, integer *, real *, integer *, real *);
static integer isamax_(integer *, real *, integer *);
static int srotmg_(real *, real *, real *, real *, real *);

static int hfti_(real *, integer *, integer *, integer *,
	     real *, integer *, integer *, real *, integer *, real *, real *,
	    real *, integer *);
static int lpdp_(real *, integer *, integer *, integer *,
	     integer *, real *, real *, real *, integer *, real *, integer *);
static int wnlsm_(real *, integer *, integer *, integer * ,
	    integer *, integer *, real *, real *, real *, integer *,
	    integer *, integer *, real *, real *, real *, real *, real *, real *);
static int wnlit_(real *, integer *, integer *, integer *,
	    integer *, integer *, integer *, real *, real *, real *, integer * ,
	    real *, logical *);

/****************************************************************************/

/*--------------------------------------------------------------------------
  Approximately (L2) solve equations

             j=nvec-1
    z[i] = SUM        A[j][i] * y[j]   (i=0..ndim-1)
             j=0

  for y[j] (j=0..nvec-1), subject to constraints (based on y[j] input)

  If input cony != 0, then you can supply sign constraints on the values
  of the output y[j] by putting values into the input y[j]:

  input y[j] =  0 ==> unconstrained
             =  1 ==> y[j] must be non-negative on output
             = -1 ==> y[j] must be non-positive on output

  If cony == 0, then the input y[j] is ignored.
  (But see note below for the case cony == 0!)

  The return value of the function is E = size of residuals >= 0.0f,
  if everything worked, and is a negative number if an error occured.

  ** If cony == 0, there is no real reason to call this function,
     since it will only then do standard least squares fitting,
     which a number of other AFNI library functions do (e.g., lsqfit).
  ** Also see the analogous cl1_solve() function in cl1.c, which does
     the same thing but with an L1 cost function instead of L2.
  ** Also see thd_fitter.c which can handle both L1 and L2 fitting,
     with and without constraints.
*//*------------------------------------------------------------------------*/

float cl2_solve( int ndim, int nvec, float *z, float **A, float *y, int cony )
{
   int ii,jj,kk ;  /* local loop counters */

   /* rmap[j] = index of input column #j in wnnls_fort call */
   /* vmap[j] = index of wnnls_fort column #j in input */
   /* rsgn[j] = whether to flip the sign of input column #j */
   /* nres    = number of restraints */

   int nres=0, *vmap=NULL, *rmap=NULL, *rsgn=NULL ;

   /* parameters for call to wnnls_fort() */

   real *w=NULL , prgopt[1] , *x=NULL , rnorm , *work=NULL ;
   integer mdw , me , ma , n , l , mode , *iwork=NULL ;

#undef  FREEIF
#define FREEIF(x) do{ if( (x) != NULL ) free((x)); } while(0)

#undef  CLEANUP
#define CLEANUP do{ FREEIF(w)   ; FREEIF(x)    ;  \
                    FREEIF(work); FREEIF(iwork);  \
                    FREEIF(rmap); FREEIF(rsgn) ; FREEIF(vmap); } while(0)

   /* check inputs */

   if( ndim < 1 || nvec < 1 )                         return -7.0f ;
   if( A == NULL || y == NULL || z == NULL )          return -8.0f ;
   for( jj=0 ; jj < nvec ; jj++ ) if( A[jj] == NULL ) return -9.0f ;

   /* compute reordering of input columns for wnnls_fort,
      which requires all the restraints be at the end of the list */

   if( cony ){  /* count number of restraints */
     for( jj=0 ; jj < nvec ; jj++ ) if( y[jj] != 0.0f ) nres++ ;
   }

   if( nres > 0 ){
     rmap = (int *)calloc(sizeof(int),nvec) ;
     vmap = (int *)calloc(sizeof(int),nvec) ;
     rsgn = (int *)calloc(sizeof(int),nvec) ;
     for( ii=jj=0 ; jj < nvec ; jj++ ){      /* unconstrained goes first */
       if( y[jj] == 0.0f ) rmap[jj] = ii++ ;
     }
     for( jj=0 ; jj < nvec ; jj++ ){         /* constrained go afterwards */
       if( y[jj] != 0.0f ){ rmap[jj] = ii++; rsgn[jj] = (y[jj]>0.0f) ? 1 : -1; }
     }
     for( jj=0 ; jj < nvec ; jj++ ){         /* find the inverse map */
       for( ii=0 ; ii < nvec ; ii++ ){
         if( rmap[ii] == jj ){ vmap[jj] = ii; break; }
       }
       if( ii == nvec )
         fprintf(stderr,"** ERROR cl2_solve: vmap[%d] is bad\n",jj) ;
     }
   }

   /* setup call to wnnls_fort() */

   ma  = ndim ;
   me  = 0 ;           /* no equality constraints */
   n   = nvec ;
   l   = nvec - nres ; /* number of unconstrained outputs */
   x   = (real *)calloc(sizeof(real),nvec+1) ;

   jj    = 2*(ndim+5*nvec+1) ;                      /* workspaces */
   work  = (real *)calloc(sizeof(real),jj) ;
   iwork = (integer *)calloc(sizeof(integer),jj) ;
   iwork[0] = iwork[1] = jj ;

   prgopt[0] = 1.0 ;  /* indicates no options being set in wnnls_fort */

   /* load matrix and vector */

   mdw = ndim+1 ;
   w   = (real *)calloc(sizeof(real),(nvec+1)*mdw) ;

   if( nres > 0 ){  /* reordering of matrix columns needed */
     for( jj=0 ; jj < nvec ; jj++ ){
       kk = rmap[jj] ;      /* input col #jj goes to wnnls col #kk */
       if( rsgn[jj] >= 0 ){ /* with sign given by rsgn[jj] */
         for( ii=0 ; ii < ndim ; ii++ ) w[ii+kk*mdw] =  A[jj][ii] ;
       } else {
         for( ii=0 ; ii < ndim ; ii++ ) w[ii+kk*mdw] = -A[jj][ii] ;
       }
     }
   } else {                          /* no reordering needed */
     for( jj=0 ; jj < nvec ; jj++ )
       for( ii=0 ; ii < ndim ; ii++ ) w[ii+jj*mdw] = A[jj][ii] ;
   }

   for( ii=0 ; ii < ndim ; ii++ ) w[ii+nvec*mdw] = z[ii] ;  /* RHS vector */

   /****** solve system of equations ******/

   mode = 0 ;
   (void)wnnls_fort( w , &mdw , &me , &ma , &n , &l ,
                     prgopt , x , &rnorm , &mode , iwork , work ) ;

   if( mode != 0 ){ CLEANUP; return (float)(-mode); }

   /* put result back in output vector y */

   if( nres == 0 ){
     for( jj=0 ; jj < nvec ; jj++ ) y[jj] = x[jj] ;
   } else {
     for( jj=0 ; jj < nvec ; jj++ ){
       kk = vmap[jj] ;  /* wnnls col #jj goes to input col #kk */
       y[kk] = (rsgn[kk] >= 0) ? x[jj] : -x[jj] ;
     }
   }

   CLEANUP ; return (float)rnorm ;
}

/*======================================================================
   Translated from the TOMS library algorithm #587
========================================================================*/

/* cl2.f -- translated by f2c (version 20030320).
   You must link the resulting object file with the libraries:
	-lconverted_from_fortran -lm   (in that order)
*/

#undef INLINE
#ifdef __GNUC__
# define INLINE __inline__
#else
# define INLINE /*nada*/
#endif

/* CC */
/* CC---------------------------------------------------------------------------- */
/* CC */
/*<       REAL             FUNCTION DIFF(X,Y)                                >*/
static INLINE real diff_(real *x, real *y)
{
    /* System generated locals */
    real ret_val;


/*     THE EDITING REQUIRED TO CONVERT THIS SUBROUTINE FROM SINGLE TO */
/*     DOUBLE PRECISION INVOLVES THE FOLLOWING CHARACTER STRING CHANGES. */
/*     USE AN EDITING COMMAND (CHANGE) /STRING-1/(TO)STRING-2/. */
/*     (APPLY CHANGES TO ENTIRE PROGRAM UNIT.) */
/*     /REAL (12 BLANKS)/DOUBLE PRECISION/ */

/*     C.L.LAWSON AND R.J.HANSON, JET PROPULSION LABORATORY, 1973 JUNE 7 */
/*     TO APPEAR IN 'SOLVING LEAST SQUARES PROBLEMS', PRENTICE-HALL, 1974 */
/*<       REAL              X, Y >*/
/*<       DIFF=X-Y >*/
#line 3059 "cl2.f"
    ret_val = *x - *y;
/*<       RETURN >*/
#line 3060 "cl2.f"
    return ret_val;
/*<       END >*/
} /* diff_ */

#line 1 "cl2.f"
/* Table of constant values */

static integer c__1 = 1;
static integer c__2 = 2;
static integer c__0 = 0;

/*     ALGORITHM 587, COLLECTED ALGORITHMS FROM ACM. */
/*     ALGORITHM APPEARED IN ACM-TRANS. MATH. SOFTWARE, VOL. 8, NO. 3, */
/*     SEP., 1982, P.323. */
/* CC */
/* CC---------------------------------------------------------------------------- */
/* CC */
/*<    >*/
static /* Subroutine */ int lsei_(real *w, integer *mdw, integer *me, integer *ma,
	integer *mg, integer *n, real *prgopt, real *x, real *rnorme, real *
	rnorml, integer *mode, real *ws, integer *ip)
{
    /* Initialized data */

    static real zero = 0.f;
    static real srelpr = 0.f;
    static real half = .5f;
    static real one = 1.f;

    /* Format strings */
    static char fmt_90[] = "";

    /* System generated locals */
    integer w_dim1, w_offset, i__1, i__2, i__3;
    real r__1, r__2;

    /* Builtin functions */
    double sqrt(doublereal);

    /* Local variables */
    static integer i__, j, k, l, m;
    static real t;
    static integer n1, n2;
    static integer ii;
    static real rb;
    static integer kk, jj;
    static real uj, rn, sn, vj, up;
    static integer jp1, np1;
    static real gam;
    static logical cov;
    static real tau;
    static integer key, mep1, lchk, mend, link, imax, last, nerr;
    static real size;
    static integer iopt, next, nopt, igo990;
    static integer mdeqc;
    static integer nlink;
    static real enorm, fnorm, rnmax, snmax;
    static real xnrme;
    static real xnorm;
    static integer mapke1, kranke, ntimes;

    /* Assigned format variables */
    static char *igo990_fmt;


/*     DIMENSION W(MDW,N+1),PRGOPT(*),X(N), */
/*     WS(2*(ME+N)+K+(MG+2)*(N+7)),IP(MG+2*N+2) */
/*     ABOVE, K=MAX(MA+MG,N). */

/*     ABSTRACT */

/*     THIS SUBPROGRAM SOLVES A LINEARLY CONSTRAINED LEAST SQUARES */
/*     PROBLEM WITH BOTH EQUALITY AND INEQUALITY CONSTRAINTS, AND, IF THE */
/*     USER REQUESTS, OBTAINS A COVARIANCE MATRIX OF THE SOLUTION */
/*     PARAMETERS. */

/*     SUPPOSE THERE ARE GIVEN MATRICES E, A AND G OF RESPECTIVE */
/*     DIMENSIONS ME BY N, MA BY N AND MG BY N, AND VECTORS F, B AND H OF */
/*     RESPECTIVE LENGTHS ME, MA AND MG.  THIS SUBROUTINE SOLVES THE */
/*     LINEARLY CONSTRAINED LEAST SQUARES PROBLEM */

/*                   EX = F, (E ME BY N) (EQUATIONS TO BE EXACTLY */
/*                                       SATISFIED) */
/*                   AX = B, (A MA BY N) (EQUATIONS TO BE */
/*                                       APPROXIMATELY SATISFIED, */
/*                                       LEAST SQUARES SENSE) */
/*                   GX.GE.H,(G MG BY N) (INEQUALITY CONSTRAINTS) */

/*     THE INEQUALITIES GX.GE.H MEAN THAT EVERY COMPONENT OF THE PRODUCT */
/*     GX MUST BE .GE. THE CORRESPONDING COMPONENT OF H. */

/*     IN CASE THE EQUALITY CONSTRAINTS CANNOT BE SATISFIED, A */
/*     GENERALIZED INVERSE SOLUTION RESIDUAL VECTOR LENGTH IS OBTAINED */
/*     FOR F-EX. THIS IS THE MINIMAL LENGTH POSSIBLE FOR F-EX. */


/*     ANY VALUES ME.GE.0, MA.GE.0, OR MG.GE.0 ARE PERMITTED.  THE */
/*     RANK OF THE MATRIX E IS ESTIMATED DURING THE COMPUTATION. WE CALL */
/*     THIS VALUE KRANKE. IT IS AN OUTPUT PARAMETER IN IP(1) DEFINED */
/*     BELOW. USING A GENERALIZED INVERSE SOLUTION OF EX=F, A REDUCED */
/*     LEAST SQUARES PROBLEM WITH INEQUALITY CONSTRAINTS IS OBTAINED. */
/*     THE TOLERANCES USED IN THESE TESTS FOR DETERMINING THE RANK */
/*     OF E AND THE RANK OF THE REDUCED LEAST SQUARES PROBLEM ARE */
/*     GIVEN IN SANDIA TECH. REPT. SAND 78-1290. THEY CAN BE */
/*     MODIFIED BY THE USER IF NEW VALUES ARE PROVIDED IN */
/*     THE OPTION LIST OF THE ARRAY PRGOPT(*). */

/*     THE EDITING REQUIRED TO CONVERT THIS SUBROUTINE FROM SINGLE TO */
/*     DOUBLE PRECISION INVOLVES THE FOLLOWING CHARACTER STRING CHANGES. */
/*     USE AN EDITING COMMAND (CHANGE) /STRING-1/(TO)STRING-2/. */
/*     (START EDITING AT LINE WITH C++ IN COLS. 1-3.) */
/*     /REAL (12 BLANKS)/DOUBLE PRECISION/,/SASUM/DASUM/,/SDOT/DDOT/, */
/*     /SNRM2/DNRM2/,/ SQRT/ DSQRT/,/ ABS/ DABS/,/AMAX1/DMAX1/, */
/*     /SCOPY/DCOPY/,/SSCAL/DSCAL/,/SAXPY/DAXPY/,/SSWAP/DSWAP/,/E0/D0/, */
/*     /, DUMMY/,SNGL(DUMMY)/,/SRELPR/DRELPR/ */

/*     WRITTEN BY R. J. HANSON AND K. H. HASKELL.  FOR FURTHER MATH. */
/*     AND ALGORITHMIC DETAILS SEE SANDIA LABORATORIES TECH. REPTS. */
/*     SAND 77-0552, (1978), SAND 78-1290, (1979), AND */
/*     MATH. PROGRAMMING, VOL. 21, (1981), P.98-118. */

/*     THE USER MUST DIMENSION ALL ARRAYS APPEARING IN THE CALL LIST.. */
/*     W(MDW,N+1),PRGOPT(*),X(N),WS(2*(ME+N)+K+(MG+2)*(N+7)),IP(MG+2*N+2) */
/*     WHERE K=MAX(MA+MG,N).  THIS ALLOWS FOR A SOLUTION OF A RANGE OF */
/*     PROBLEMS IN THE GIVEN WORKING SPACE.  THE DIMENSION OF WS(*) */
/*     GIVEN IS A NECESSARY OVERESTIMATE.  ONCE A PARTICULAR PROBLEM */
/*     HAS BEEN RUN, THE OUTPUT PARAMETER IP(3) GIVES THE ACTUAL */
/*     DIMENSION REQUIRED FOR THAT PROBLEM. */

/*     THE PARAMETERS FOR LSEI( ) ARE */

/*     INPUT.. */

/*     W(*,*),MDW,   THE ARRAY W(*,*) IS DOUBLY SUBSCRIPTED WITH */
/*     ME,MA,MG,N    FIRST DIMENSIONING PARAMETER EQUAL TO MDW. */
/*                   FOR THIS DISCUSSION LET US CALL M = ME+MA+MG.  THEN */
/*                   MDW MUST SATISFY MDW.GE.M.  THE CONDITION */
/*                   MDW.LT.M IS AN ERROR. */

/*                   THE ARRAY W(*,*) CONTAINS THE MATRICES AND VECTORS */

/*                                  (E  F) */
/*                                  (A  B) */
/*                                  (G  H) */

/*                   IN ROWS AND COLUMNS 1,...,M AND 1,...,N+1 */
/*                   RESPECTIVELY. */

/*                   THE INTEGERS ME, MA, AND MG ARE THE */
/*                   RESPECTIVE MATRIX ROW DIMENSIONS */
/*                   OF E, A AND G. EACH MATRIX HAS N COLUMNS. */

/*     PRGOPT(*)    THIS ARRAY IS THE OPTION VECTOR. */
/*                  IF THE USER IS SATISFIED WITH THE NOMINAL */
/*                  SUBPROGRAM FEATURES SET */

/*                  PRGOPT(1)=1 (OR PRGOPT(1)=1.0) */

/*                  OTHERWISE PRGOPT(*) IS A LINKED LIST CONSISTING OF */
/*                  GROUPS OF DATA OF THE FOLLOWING FORM */

/*                  LINK */
/*                  KEY */
/*                  DATA SET */

/*                  THE PARAMETERS LINK AND KEY ARE EACH ONE WORD. */
/*                  THE DATA SET CAN BE COMPRISED OF SEVERAL WORDS. */
/*                  THE NUMBER OF ITEMS DEPENDS ON THE VALUE OF KEY. */
/*                  THE VALUE OF LINK POINTS TO THE FIRST */
/*                  ENTRY OF THE NEXT GROUP OF DATA WITHIN */
/*                  PRGOPT(*).  THE EXCEPTION IS WHEN THERE ARE */
/*                  NO MORE OPTIONS TO CHANGE.  IN THAT */
/*                  CASE LINK=1 AND THE VALUES KEY AND DATA SET */
/*                  ARE NOT REFERENCED. THE GENERAL LAYOUT OF */
/*                  PRGOPT(*) IS AS FOLLOWS. */

/*               ...PRGOPT(1)=LINK1 (LINK TO FIRST ENTRY OF NEXT GROUP) */
/*               .  PRGOPT(2)=KEY1 (KEY TO THE OPTION CHANGE) */
/*               .  PRGOPT(3)=DATA VALUE (DATA VALUE FOR THIS CHANGE) */
/*               .       . */
/*               .       . */
/*               .       . */
/*               ...PRGOPT(LINK1)=LINK2 (LINK TO THE FIRST ENTRY OF */
/*               .                       NEXT GROUP) */
/*               .  PRGOPT(LINK1+1)=KEY2 (KEY TO THE OPTION CHANGE) */
/*               .  PRGOPT(LINK1+2)=DATA VALUE */
/*               ...     . */
/*               .       . */
/*               .       . */
/*               ...PRGOPT(LINK)=1 (NO MORE OPTIONS TO CHANGE) */

/*                  VALUES OF LINK THAT ARE NONPOSITIVE ARE ERRORS. */
/*                  A VALUE OF LINK.GT.NLINK=100000 IS ALSO AN ERROR. */
/*                  THIS HELPS PREVENT USING INVALID BUT POSITIVE */
/*                  VALUES OF LINK THAT WILL PROBABLY EXTEND */
/*                  BEYOND THE PROGRAM LIMITS OF PRGOPT(*). */
/*                  UNRECOGNIZED VALUES OF KEY ARE IGNORED.  THE */
/*                  ORDER OF THE OPTIONS IS ARBITRARY AND ANY NUMBER */
/*                  OF OPTIONS CAN BE CHANGED WITH THE FOLLOWING */
/*                  RESTRICTION.  TO PREVENT CYCLING IN THE */
/*                  PROCESSING OF THE OPTION ARRAY A COUNT OF THE */
/*                  NUMBER OF OPTIONS CHANGED IS MAINTAINED. */
/*                  WHENEVER THIS COUNT EXCEEDS NOPT=1000 AN ERROR */
/*                  MESSAGE IS PRINTED AND THE SUBPROGRAM RETURNS. */

/*                  OPTIONS.. */

/*                  KEY=1 */
/*                         COMPUTE IN W(*,*) THE N BY N */
/*                  COVARIANCE MATRIX OF THE SOLUTION VARIABLES */
/*                  AS AN OUTPUT PARAMETER.  NOMINALLY THE */
/*                  COVARIANCE MATRIX WILL NOT BE COMPUTED. */
/*                  (THIS REQUIRES NO USER INPUT.) */
/*                  THE DATA SET FOR THIS OPTION IS A SINGLE VALUE. */
/*                  IT MUST BE NONZERO WHEN THE COVARIANCE MATRIX */
/*                  IS DESIRED.  IF IT IS ZERO, THE COVARIANCE */
/*                  MATRIX IS NOT COMPUTED.  WHEN THE COVARIANCE MATRIX */
/*                  IS COMPUTED, THE FIRST DIMENSIONING PARAMETER */
/*                  OF THE ARRAY W(*,*) MUST SATISFY MDW.GE.MAX0(M,N). */

/*                  KEY=2 */
/*                         SCALE THE NONZERO COLUMNS OF THE */
/*                         ENTIRE DATA MATRIX. */
/*                  (E) */
/*                  (A) */
/*                  (G) */

/*                  TO HAVE LENGTH ONE.  THE DATA SET FOR THIS */
/*                  OPTION IS A SINGLE VALUE.  IT MUST BE */
/*                  NONZERO IF UNIT LENGTH COLUMN SCALING */
/*                  IS DESIRED. */

/*                  KEY=3 */
/*                         SCALE COLUMNS OF THE ENTIRE DATA MATRIX */
/*                  (E) */
/*                  (A) */
/*                  (G) */

/*                  WITH A USER-PROVIDED DIAGONAL MATRIX. */
/*                  THE DATA SET FOR THIS OPTION CONSISTS */
/*                  OF THE N DIAGONAL SCALING FACTORS, ONE FOR */
/*                  EACH MATRIX COLUMN. */

/*                  KEY=4 */
/*                         CHANGE THE RANK DETERMINATION TOLERANCE FOR */
/*                  THE EQUALITY CONSTRAINT EQUATIONS FROM */
/*                  THE NOMINAL VALUE OF SQRT(SRELPR).  THIS QUANTITY CAN */
/*                  BE NO SMALLER THAN SRELPR, THE ARITHMETIC- */
/*                  STORAGE PRECISION.  THE QUANTITY SRELPR IS THE */
/*                  LARGEST POSITIVE NUMBER SUCH THAT T=1.+SRELPR */
/*                  SATISFIES T.EQ.1.  THE QUANTITY USED */
/*                  HERE IS INTERNALLY RESTRICTED TO BE AT */
/*                  LEAST SRELPR.  THE DATA SET FOR THIS OPTION */
/*                  IS THE NEW TOLERANCE. */

/*                  KEY=5 */
/*                         CHANGE THE RANK DETERMINATION TOLERANCE FOR */
/*                  THE REDUCED LEAST SQUARES EQUATIONS FROM */
/*                  THE NOMINAL VALUE OF SQRT(SRELPR).  THIS QUANTITY CAN */
/*                  BE NO SMALLER THAN SRELPR, THE ARITHMETIC- */
/*                  STORAGE PRECISION.  THE QUANTITY USED */
/*                  HERE IS INTERNALLY RESTRICTED TO BE AT */
/*                  LEAST SRELPR.  THE DATA SET FOR THIS OPTION */
/*                  IS THE NEW TOLERANCE. */

/*                  FOR EXAMPLE, SUPPOSE WE WANT TO CHANGE */
/*                  THE TOLERANCE FOR THE REDUCED LEAST SQUARES */
/*                  PROBLEM, COMPUTE THE COVARIANCE MATRIX OF */
/*                  THE SOLUTION PARAMETERS, AND PROVIDE */
/*                  COLUMN SCALING FOR THE DATA MATRIX.  FOR */
/*                  THESE OPTIONS THE DIMENSION OF PRGOPT(*) */
/*                  MUST BE AT LEAST N+9.  THE FORTRAN STATEMENTS */
/*                  DEFINING THESE OPTIONS WOULD BE AS FOLLOWS. */

/*                  PRGOPT(1)=4 (LINK TO ENTRY 4 IN PRGOPT(*)) */
/*                  PRGOPT(2)=1 (COVARIANCE MATRIX KEY) */
/*                  PRGOPT(3)=1 (COVARIANCE MATRIX WANTED) */

/*                  PRGOPT(4)=7 (LINK TO ENTRY 7 IN PRGOPT(*)) */
/*                  PRGOPT(5)=5 (LEAST SQUARES EQUAS. TOLERANCE KEY) */
/*                  PRGOPT(6)=... (NEW VALUE OF THE TOLERANCE) */

/*                  PRGOPT(7)=N+9 (LINK TO ENTRY N+9 IN PRGOPT(*)) */
/*                  PRGOPT(8)=3 (USER-PROVIDED COLUMN SCALING KEY) */

/*                  CALL SCOPY(N,D,1,PRGOPT(9),1)  (COPY THE N */
/*                    SCALING FACTORS FROM THE USER ARRAY D(*) */
/*                    TO PRGOPT(9)-PRGOPT(N+8)) */

/*                  PRGOPT(N+9)=1 (NO MORE OPTIONS TO CHANGE) */

/*                  THE CONTENTS OF PRGOPT(*) ARE NOT MODIFIED */
/*                  BY THE SUBPROGRAM. */
/*                  THE OPTIONS FOR WNNLS( ) CAN ALSO BE INCLUDED */
/*                  IN THIS ARRAY.  THE VALUES OF KEY RECOGNIZED */
/*                  BY WNNLS( ) ARE 6, 7 AND 8.  THEIR FUNCTIONS */
/*                  ARE DOCUMENTED IN THE USAGE INSTRUCTIONS FOR */
/*                  SUBROUTINE WNNLS( ).  NORMALLY THESE OPTIONS */
/*                  DO NOT NEED TO BE MODIFIED WHEN USING LSEI( ). */

/*     IP(1),       THE AMOUNTS OF WORKING STORAGE ACTUALLY */
/*     IP(2)        ALLOCATED FOR THE WORKING ARRAYS WS(*) AND */
/*                  IP(*), RESPECTIVELY.  THESE QUANTITIES ARE */
/*                  COMPARED WITH THE ACTUAL AMOUNTS OF STORAGE */
/*                  NEEDED BY LSEI( ).  INSUFFICIENT STORAGE */
/*                  ALLOCATED FOR EITHER WS(*) OR IP(*) IS AN */
/*                  ERROR.  THIS FEATURE WAS INCLUDED IN LSEI( ) */
/*                  BECAUSE MISCALCULATING THE STORAGE FORMULAS */
/*                  FOR WS(*) AND IP(*) MIGHT VERY WELL LEAD TO */
/*                  SUBTLE AND HARD-TO-FIND EXECUTION ERRORS. */

/*                  THE LENGTH OF WS(*) MUST BE AT LEAST */

/*                  LW = 2*(ME+N)+K+(MG+2)*(N+7) */


/*                  WHERE K = MAX(MA+MG,N) */
/*                  THIS TEST WILL NOT BE MADE IF IP(1).LE.0. */

/*                  THE LENGTH OF IP(*) MUST BE AT LEAST */

/*                  LIP = MG+2*N+2 */
/*                  THIS TEST WILL NOT BE MADE IF IP(2).LE.0. */

/*     OUTPUT.. */

/*     X(*),RNORME,  THE ARRAY X(*) CONTAINS THE SOLUTION PARAMETERS */
/*     RNORML        IF THE INTEGER OUTPUT FLAG MODE = 0 OR 1. */
/*                   THE DEFINITION OF MODE IS GIVEN DIRECTLY BELOW. */
/*                   WHEN MODE = 0 OR 1, RNORME AND RNORML */
/*                   RESPECTIVELY CONTAIN THE RESIDUAL VECTOR */
/*                   EUCLIDEAN LENGTHS OF F - EX AND B - AX.  WHEN */
/*                   MODE=1 THE EQUALITY CONSTRAINT EQUATIONS EX=F */
/*                   ARE CONTRADICTORY, SO RNORME.NE.0. THE RESIDUAL */
/*                   VECTOR F-EX HAS MINIMAL EUCLIDEAN LENGTH. FOR */
/*                   MODE.GE.2, NONE OF THESE PARAMETERS ARE */
/*                   DEFINED. */

/*     MODE          INTEGER FLAG THAT INDICATES THE SUBPROGRAM */
/*                   STATUS AFTER COMPLETION.  IF MODE.GE.2, NO */
/*                   SOLUTION HAS BEEN COMPUTED. */

/*                   MODE = */

/*                   0  BOTH EQUALITY AND INEQUALITY CONSTRAINTS */
/*                      ARE COMPATIBLE AND HAVE BEEN SATISFIED. */

/*                   1  EQUALITY CONSTRAINTS ARE CONTRADICTORY. */
/*                      A GENERALIZED INVERSE SOLUTION OF EX=F WAS USED */
/*                      TO MINIMIZE THE RESIDUAL VECTOR LENGTH F-EX. */
/*                      IN THIS SENSE, THE SOLUTION IS STILL MEANINGFUL. */

/*                   2  INEQUALITY CONSTRAINTS ARE CONTRADICTORY. */

/*                   3  BOTH EQUALITY AND INEQUALITY CONSTRAINTS */
/*                      ARE CONTRADICTORY. */

/*                   THE FOLLOWING INTERPRETATION OF */
/*                   MODE=1,2 OR 3 MUST BE MADE.  THE */
/*                   SETS CONSISTING OF ALL SOLUTIONS */
/*                   OF THE EQUALITY CONSTRAINTS EX=F */
/*                   AND ALL VECTORS SATISFYING GX.GE.H */
/*                   HAVE NO POINTS ON COMMON.  (IN */
/*                   PARTICULAR THIS DOES NOT SAY THAT */
/*                   EACH INDIVIDUAL SET HAS NO POINTS */
/*                   AT ALL, ALTHOUGH THIS COULD BE THE */
/*                   CASE.) */

/*                   4  USAGE ERROR OCCURRED.  THE VALUE */
/*                      OF MDW IS .LT. ME+MA+MG, MDW IS */
/*                      .LT. N AND A COVARIANCE MATRIX IS */
/*                      REQUESTED, THE OPTION VECTOR */
/*                      PRGOPT(*) IS NOT PROPERLY DEFINED, */
/*                      OR THE LENGTHS OF THE WORKING ARRAYS */
/*                      WS(*) AND IP(*), WHEN SPECIFIED IN */
/*                      IP(1) AND IP(2) RESPECTIVELY, ARE NOT */
/*                      LONG ENOUGH. */

/*     W(*,*)        THE ARRAY W(*,*) CONTAINS THE N BY N SYMMETRIC */
/*                   COVARIANCE MATRIX OF THE SOLUTION PARAMETERS, */
/*                   PROVIDED THIS WAS REQUESTED ON INPUT WITH */
/*                   THE OPTION VECTOR PRGOPT(*) AND THE OUTPUT */
/*                   FLAG IS RETURNED WITH MODE = 0 OR 1. */

/*     IP(*)         THE INTEGER WORKING ARRAY HAS THREE ENTRIES */
/*                   THAT PROVIDE RANK AND WORKING ARRAY LENGTH */
/*                   INFORMATION AFTER COMPLETION. */

/*                      IP(1) = RANK OF EQUALITY CONSTRAINT */
/*                              MATRIX.  DEFINE THIS QUANTITY */
/*                              AS KRANKE. */

/*                      IP(2) = RANK OF REDUCED LEAST SQUARES */
/*                              PROBLEM. */

/*                      IP(3) = THE AMOUNT OF STORAGE IN THE */
/*                              WORKING ARRAY WS(*) THAT WAS */
/*                              ACTUALLY USED BY THE SUBPROGRAM. */
/*                              THE FORMULA GIVEN ABOVE FOR THE LENGTH */
/*                              OF WS(*) IS A NECESSARY OVERESTIMATE. */
/*     USER DESIGNATED */
/*     WORKING ARRAYS.. */

/*     WS(*),IP(*)              THESE ARE RESP. TYPE FLOATING POINT */
/*                              AND TYPE INTEGER WORKING ARRAYS. */
/*                              THEIR REQUIRED MINIMAL LENGTHS ARE */
/*                              GIVEN ABOVE. */


/*     SUBROUTINES CALLED */

/*     LSI           PART OF THIS PACKAGE.  SOLVES A */
/*                   CONSTRAINED LEAST SQUARES PROBLEM WITH */
/*                   INEQUALITY CONSTRAINTS. */

/* ++ */
/*     SDOT,SSCAL,   SUBROUTINES FROM THE BLAS PACKAGE. */
/*     SAXPY,SASUM,  SEE TRANS. MATH. SOFT., VOL. 5, NO. 3, P. 308. */
/*     SCOPY,SNRM2, */
/*     SSWAP */

/*     H12           SUBROUTINE TO CONSTRUCT AND APPLY A */
/*                   HOUSEHOLDER TRANSFORMATION. */

/*     XERROR        FROM SLATEC ERROR PROCESSING PACKAGE. */
/*                   THIS IS DOCUMENTED IN SANDIA TECH. REPT., */
/*                   SAND78-1189. */

/*     SUBROUTINE LSEI(W,MDW,ME,MA,MG,N,PRGOPT,X,RNORME,RNORML,MODE,WS, */
/*    1 IP) */

/*     REVISED OCT. 1, 1981. */

/*<       REAL             W(MDW,1), PRGOPT(1), X(1), WS(1), RNORME, RNORML >*/
/*<       INTEGER IP(3) >*/
/*<    >*/
/*<       REAL             SASUM, SDOT, SNRM2, SQRT, ABS, AMAX1 >*/
/*<       LOGICAL COV >*/
/*<       DATA ZERO /0.E0/, SRELPR /0.E0/, HALF /0.5E0/, ONE /1.E0/ >*/
#line 384 "cl2.f"
    /* Parameter adjustments */
#line 384 "cl2.f"
    w_dim1 = *mdw;
#line 384 "cl2.f"
    w_offset = 1 + w_dim1;
#line 384 "cl2.f"
    w -= w_offset;
#line 384 "cl2.f"
    --prgopt;
#line 384 "cl2.f"
    --x;
#line 384 "cl2.f"
    --ws;
#line 384 "cl2.f"
    --ip;
#line 384 "cl2.f"

#line 384 "cl2.f"
    /* Function Body */

/*     CHECK THAT ENOUGH STORAGE WAS ALLOCATED IN WS(*) AND IP(*). */
/*<       IF (.NOT.(IP(1).GT.0)) GO TO 20 >*/
#line 387 "cl2.f"
    if (! (ip[1] > 0)) {
#line 387 "cl2.f"
	goto L20;
#line 387 "cl2.f"
    }
/*<       LCHK = 2*(ME+N) + MAX0(MA+MG,N) + (MG+2)*(N+7) >*/
/* Computing MAX */
#line 388 "cl2.f"
    i__1 = *ma + *mg;
#line 388 "cl2.f"
    lchk = (*me + *n << 1) + max(i__1,*n) + (*mg + 2) * (*n + 7);
/*<       IF (.NOT.(IP(1).LT.LCHK)) GO TO 10 >*/
#line 389 "cl2.f"
    if (! (ip[1] < lchk)) {
#line 389 "cl2.f"
	goto L10;
#line 389 "cl2.f"
    }
/*<       MODE = 4 >*/
#line 390 "cl2.f"
    *mode = 4;
/*<       NERR = 2 >*/
#line 391 "cl2.f"
    nerr = 2;
/*<       IOPT = 1 >*/
#line 392 "cl2.f"
    iopt = 1;
/* cc      CALL XERRWV(67HLSEI( ), INSUFFICIENT STORAGE ALLOCATED FOR WS(*), */
/* cc     *NEED LW=I1 BELOW, 67, NERR, IOPT, 1, LCHK, 0, */
/* cc     * 0, DUMMY, DUMMY) */
/*<       RETURN >*/
#line 396 "cl2.f"
    return 0;
/*<    10 CONTINUE >*/
#line 397 "cl2.f"
L10:
/*<    20 IF (.NOT.(IP(2).GT.0)) GO TO 40 >*/
#line 398 "cl2.f"
L20:
#line 398 "cl2.f"
    if (! (ip[2] > 0)) {
#line 398 "cl2.f"
	goto L40;
#line 398 "cl2.f"
    }
/*<       LCHK = MG + 2*N + 2 >*/
#line 399 "cl2.f"
    lchk = *mg + (*n << 1) + 2;
/*<       IF (.NOT.(IP(2).LT.LCHK)) GO TO 30 >*/
#line 400 "cl2.f"
    if (! (ip[2] < lchk)) {
#line 400 "cl2.f"
	goto L30;
#line 400 "cl2.f"
    }
/*<       MODE = 4 >*/
#line 401 "cl2.f"
    *mode = 4;
/*<       NERR = 2 >*/
#line 402 "cl2.f"
    nerr = 2;
/*<       IOPT = 1 >*/
#line 403 "cl2.f"
    iopt = 1;
/* cc      CALL XERRWV(68HLSEI( ), INSUFFICIENT STORAGE ALLOCATED FOR IP(*), */
/* cc     *NEED LIP=I1 BELOW, 68, NERR, IOPT, 1, LCHK, 0, */
/* cc     * 0, DUMMY, DUMMY) */
/*<       RETURN >*/
#line 407 "cl2.f"
    return 0;
/*<    30 CONTINUE >*/
#line 408 "cl2.f"
L30:

/*     COMPUTE MACHINE PRECISION=SRELPR ONLY WHEN NECESSARY. */
/*<    40 IF (.NOT.(SRELPR.EQ.ZERO)) GO TO 70 >*/
#line 411 "cl2.f"
L40:
#line 411 "cl2.f"
    if (! (srelpr == zero)) {
#line 411 "cl2.f"
	goto L70;
#line 411 "cl2.f"
    }
/*<       SRELPR = ONE >*/
#line 412 "cl2.f"
    srelpr = one;
/*<    50 IF (ONE+SRELPR.EQ.ONE) GO TO 60 >*/
#line 413 "cl2.f"
L50:
#line 413 "cl2.f"
    if (one + srelpr == one) {
#line 413 "cl2.f"
	goto L60;
#line 413 "cl2.f"
    }
/*<       SRELPR = SRELPR*HALF >*/
#line 414 "cl2.f"
    srelpr *= half;
/*<       GO TO 50 >*/
#line 415 "cl2.f"
    goto L50;
/*<    60 SRELPR = SRELPR + SRELPR >*/
#line 416 "cl2.f"
L60:
#line 416 "cl2.f"
    srelpr += srelpr;

/*     COMPUTE NUMBER OF POSSIBLE RIGHT MULTIPLYING HOUSEHOLDER */
/*     TRANSFORMATIONS. */
/*<    70 M = ME + MA + MG >*/
#line 420 "cl2.f"
L70:
#line 420 "cl2.f"
    m = *me + *ma + *mg;
/*<       MODE = 0 >*/
#line 421 "cl2.f"
    *mode = 0;
/*<       IF (N.LE.0 .OR. M.LE.0) RETURN >*/
#line 422 "cl2.f"
    if (*n <= 0 || m <= 0) {
#line 422 "cl2.f"
	return 0;
#line 422 "cl2.f"
    }
/*<       IF (.NOT.(MDW.LT.M)) GO TO 80 >*/
#line 423 "cl2.f"
    if (! (*mdw < m)) {
#line 423 "cl2.f"
	goto L80;
#line 423 "cl2.f"
    }
/*<       NERR = 1 >*/
#line 424 "cl2.f"
    nerr = 1;
/*<       IOPT = 1 >*/
#line 425 "cl2.f"
    iopt = 1;
/* cc      CALL XERROR(36HLSEI( ), MDW.LT.ME+MA+MG IS AN ERROR, 36, NERR, */
/* cc     * IOPT) */
/*<       MODE = 4 >*/
#line 428 "cl2.f"
    *mode = 4;
/*<       RETURN >*/
#line 429 "cl2.f"
    return 0;
/*<    80 NP1 = N + 1 >*/
#line 430 "cl2.f"
L80:
#line 430 "cl2.f"
    np1 = *n + 1;
/*<       KRANKE = MIN0(ME,N) >*/
#line 431 "cl2.f"
    kranke = min(*me,*n);
/*<       N1 = 2*KRANKE + 1 >*/
#line 432 "cl2.f"
    n1 = (kranke << 1) + 1;
/*<       N2 = N1 + N >*/
#line 433 "cl2.f"
    n2 = n1 + *n;

/*     PROCESS-OPTION-VECTOR */
/*<       ASSIGN 90 TO IGO990 >*/
#line 436 "cl2.f"
    igo990 = 0;
#line 436 "cl2.f"
    igo990_fmt = fmt_90;
/*<       GO TO 480 >*/
#line 437 "cl2.f"
    goto L480;
/*<    90 IF (.NOT.(COV .AND. MDW.LT.N)) GO TO 100 >*/
#line 438 "cl2.f"
L90:
#line 438 "cl2.f"
    if (! (cov && *mdw < *n)) {
#line 438 "cl2.f"
	goto L100;
#line 438 "cl2.f"
    }
/*<       NERR = 2 >*/
#line 439 "cl2.f"
    nerr = 2;
/*<       IOPT = 1 >*/
#line 440 "cl2.f"
    iopt = 1;
/* cc      CALL XERROR( */
/* cc     * 54HLSEI( ), MDW.LT.N, WHEN COV MATRIX NEEDED, IS AN ERROR, 54, */
/* cc     * NERR, IOPT) */
/*<       MODE = 4 >*/
#line 444 "cl2.f"
    *mode = 4;
/*<       RETURN >*/
#line 445 "cl2.f"
    return 0;
/*<   100 L = KRANKE >*/
#line 446 "cl2.f"
L100:
#line 446 "cl2.f"
    l = kranke;

/*     COMPUTE NORM OF EQUALITY CONSTRAINT MATRIX AND RT SIDE. */
/*<       ENORM = ZERO >*/
#line 449 "cl2.f"
    enorm = zero;
/*<       DO 110 J=1,N >*/
#line 450 "cl2.f"
    i__1 = *n;
#line 450 "cl2.f"
    for (j = 1; j <= i__1; ++j) {
/*<         ENORM = AMAX1(ENORM,SASUM(ME,W(1,J),1)) >*/
/* Computing MAX */
#line 451 "cl2.f"
	r__1 = enorm, r__2 = sasum_(me, &w[j * w_dim1 + 1], &c__1);
#line 451 "cl2.f"
	enorm = max(r__1,r__2);
/*<   110 CONTINUE >*/
#line 452 "cl2.f"
/* L110: */
#line 452 "cl2.f"
    }
/*<       FNORM = SASUM(ME,W(1,NP1),1) >*/
#line 453 "cl2.f"
    fnorm = sasum_(me, &w[np1 * w_dim1 + 1], &c__1);
/*<       IF (.NOT.(L.GT.0)) GO TO 190 >*/
#line 454 "cl2.f"
    if (! (l > 0)) {
#line 454 "cl2.f"
	goto L190;
#line 454 "cl2.f"
    }
/*<       SNMAX = ZERO >*/
#line 455 "cl2.f"
    snmax = zero;
/*<       RNMAX = ZERO >*/
#line 456 "cl2.f"
    rnmax = zero;
/*<       DO 180 I=1,L >*/
#line 457 "cl2.f"
    i__1 = l;
#line 457 "cl2.f"
    for (i__ = 1; i__ <= i__1; ++i__) {

/*     COMPUTE MAXIMUM RATIO OF VECTOR LENGTHS. PARTITION */
/*     IS AT COL. I. */
/*<         DO 150 K=I,ME >*/
#line 461 "cl2.f"
	i__2 = *me;
#line 461 "cl2.f"
	for (k = i__; k <= i__2; ++k) {
/*<           SN = SDOT(N-I+1,W(K,I),MDW,W(K,I),MDW) >*/
#line 462 "cl2.f"
	    i__3 = *n - i__ + 1;
#line 462 "cl2.f"
	    sn = sdot_(&i__3, &w[k + i__ * w_dim1], mdw, &w[k + i__ * w_dim1],
		     mdw);
/*<           RN = SDOT(I-1,W(K,1),MDW,W(K,1),MDW) >*/
#line 463 "cl2.f"
	    i__3 = i__ - 1;
#line 463 "cl2.f"
	    rn = sdot_(&i__3, &w[k + w_dim1], mdw, &w[k + w_dim1], mdw);
/*<           IF (.NOT.(RN.EQ.ZERO .AND. SN.GT.SNMAX)) GO TO 120 >*/
#line 464 "cl2.f"
	    if (! (rn == zero && sn > snmax)) {
#line 464 "cl2.f"
		goto L120;
#line 464 "cl2.f"
	    }
/*<           SNMAX = SN >*/
#line 465 "cl2.f"
	    snmax = sn;
/*<           IMAX = K >*/
#line 466 "cl2.f"
	    imax = k;
/*<           GO TO 140 >*/
#line 467 "cl2.f"
	    goto L140;
/*<   120     IF (.NOT.(K.EQ.I .OR. (SN*RNMAX.GT.RN*SNMAX))) GO TO 130 >*/
#line 468 "cl2.f"
L120:
#line 468 "cl2.f"
	    if (! (k == i__ || sn * rnmax > rn * snmax)) {
#line 468 "cl2.f"
		goto L130;
#line 468 "cl2.f"
	    }
/*<           SNMAX = SN >*/
#line 469 "cl2.f"
	    snmax = sn;
/*<           RNMAX = RN >*/
#line 470 "cl2.f"
	    rnmax = rn;
/*<           IMAX = K >*/
#line 471 "cl2.f"
	    imax = k;
/*<   130     CONTINUE >*/
#line 472 "cl2.f"
L130:
/*<   140     CONTINUE >*/
#line 473 "cl2.f"
L140:
/*<   150   CONTINUE >*/
#line 474 "cl2.f"
/* L150: */
#line 474 "cl2.f"
	    ;
#line 474 "cl2.f"
	}

/*     INTERCHANGE ROWS IF NECESSARY. */
/*<         IF (I.NE.IMAX) CALL SSWAP(NP1, W(I,1), MDW, W(IMAX,1), MDW) >*/
#line 477 "cl2.f"
	if (i__ != imax) {
#line 477 "cl2.f"
	    sswap_(&np1, &w[i__ + w_dim1], mdw, &w[imax + w_dim1], mdw);
#line 477 "cl2.f"
	}
/*<         IF (.NOT.(SNMAX.GT.TAU**2*RNMAX)) GO TO 160 >*/
/* Computing 2nd power */
#line 478 "cl2.f"
	r__1 = tau;
#line 478 "cl2.f"
	if (! (snmax > r__1 * r__1 * rnmax)) {
#line 478 "cl2.f"
	    goto L160;
#line 478 "cl2.f"
	}

/*     ELIMINATE ELEMS I+1,...,N IN ROW I. */
/*<    >*/
#line 481 "cl2.f"
	i__2 = i__ + 1;
#line 481 "cl2.f"
	i__3 = m - i__;
#line 481 "cl2.f"
	h12_(&c__1, &i__, &i__2, n, &w[i__ + w_dim1], mdw, &ws[i__], &w[i__ +
		1 + w_dim1], mdw, &c__1, &i__3);
/*<         GO TO 170 >*/
#line 483 "cl2.f"
	goto L170;
/*<   160   KRANKE = I - 1 >*/
#line 484 "cl2.f"
L160:
#line 484 "cl2.f"
	kranke = i__ - 1;
/*<         GO TO 200 >*/
#line 485 "cl2.f"
	goto L200;
/*<   170   CONTINUE >*/
#line 486 "cl2.f"
L170:
/*<   180 CONTINUE >*/
#line 487 "cl2.f"
/* L180: */
#line 487 "cl2.f"
	;
#line 487 "cl2.f"
    }
/*<   190 CONTINUE >*/
#line 488 "cl2.f"
L190:
/*<   200 CONTINUE >*/
#line 489 "cl2.f"
L200:

/*     SAVE DIAG. TERMS OF LOWER TRAP. MATRIX. */
/*<       CALL SCOPY(KRANKE, W, MDW+1, WS(KRANKE+1), 1) >*/
#line 492 "cl2.f"
    i__1 = *mdw + 1;
#line 492 "cl2.f"
    scopy_(&kranke, &w[w_offset], &i__1, &ws[kranke + 1], &c__1);

/*     USE HOUSEHOLDER TRANS FROM LEFT TO ACHIEVE KRANKE BY KRANKE UPPER */
/*     TRIANGULAR FORM. */
/*<       IF (.NOT.(KRANKE.GT.0 .AND. KRANKE.LT.ME)) GO TO 220 >*/
#line 496 "cl2.f"
    if (! (kranke > 0 && kranke < *me)) {
#line 496 "cl2.f"
	goto L220;
#line 496 "cl2.f"
    }
/*<       DO 210 KK=1,KRANKE >*/
#line 497 "cl2.f"
    i__1 = kranke;
#line 497 "cl2.f"
    for (kk = 1; kk <= i__1; ++kk) {
/*<         K = KRANKE + 1 - KK >*/
#line 498 "cl2.f"
	k = kranke + 1 - kk;

/*     APPLY TRANFORMATION TO MATRIX COLS. 1,...,K-1. */
/*<         CALL H12(1, K, KRANKE+1, ME, W(1,K), 1, UP, W, 1, MDW, K-1) >*/
#line 501 "cl2.f"
	i__2 = kranke + 1;
#line 501 "cl2.f"
	i__3 = k - 1;
#line 501 "cl2.f"
	h12_(&c__1, &k, &i__2, me, &w[k * w_dim1 + 1], &c__1, &up, &w[
		w_offset], &c__1, mdw, &i__3);

/*     APPLY TO RT SIDE VECTOR. */
/*<         CALL H12(2, K, KRANKE+1, ME, W(1,K), 1, UP, W(1,NP1), 1, 1, 1) >*/
#line 504 "cl2.f"
	i__2 = kranke + 1;
#line 504 "cl2.f"
	h12_(&c__2, &k, &i__2, me, &w[k * w_dim1 + 1], &c__1, &up, &w[np1 *
		w_dim1 + 1], &c__1, &c__1, &c__1);
/*<   210 CONTINUE >*/
#line 505 "cl2.f"
/* L210: */
#line 505 "cl2.f"
    }
/*<   220 IF (.NOT.(KRANKE.GT.0)) GO TO 240 >*/
#line 506 "cl2.f"
L220:
#line 506 "cl2.f"
    if (! (kranke > 0)) {
#line 506 "cl2.f"
	goto L240;
#line 506 "cl2.f"
    }

/*     SOLVE FOR VARIABLES 1,...,KRANKE IN NEW COORDINATES. */
/*<       CALL SCOPY(KRANKE, W(1,NP1), 1, X, 1) >*/
#line 509 "cl2.f"
    scopy_(&kranke, &w[np1 * w_dim1 + 1], &c__1, &x[1], &c__1);
/*<       DO 230 I=1,KRANKE >*/
#line 510 "cl2.f"
    i__1 = kranke;
#line 510 "cl2.f"
    for (i__ = 1; i__ <= i__1; ++i__) {
/*<         X(I) = (X(I)-SDOT(I-1,W(I,1),MDW,X,1))/W(I,I) >*/
#line 511 "cl2.f"
	i__2 = i__ - 1;
#line 511 "cl2.f"
	x[i__] = (x[i__] - sdot_(&i__2, &w[i__ + w_dim1], mdw, &x[1], &c__1))
		/ w[i__ + i__ * w_dim1];
/*<   230 CONTINUE >*/
#line 512 "cl2.f"
/* L230: */
#line 512 "cl2.f"
    }

/*     COMPUTE RESIDUALS FOR REDUCED PROBLEM. */
/*<   240 MEP1 = ME + 1 >*/
#line 515 "cl2.f"
L240:
#line 515 "cl2.f"
    mep1 = *me + 1;
/*<       RNORML = ZERO >*/
#line 516 "cl2.f"
    *rnorml = zero;
/*<       IF (.NOT.(ME.LT.M)) GO TO 270 >*/
#line 517 "cl2.f"
    if (! (*me < m)) {
#line 517 "cl2.f"
	goto L270;
#line 517 "cl2.f"
    }
/*<       DO 260 I=MEP1,M >*/
#line 518 "cl2.f"
    i__1 = m;
#line 518 "cl2.f"
    for (i__ = mep1; i__ <= i__1; ++i__) {
/*<         W(I,NP1) = W(I,NP1) - SDOT(KRANKE,W(I,1),MDW,X,1) >*/
#line 519 "cl2.f"
	w[i__ + np1 * w_dim1] -= sdot_(&kranke, &w[i__ + w_dim1], mdw, &x[1],
		&c__1);
/*<         SN = SDOT(KRANKE,W(I,1),MDW,W(I,1),MDW) >*/
#line 520 "cl2.f"
	sn = sdot_(&kranke, &w[i__ + w_dim1], mdw, &w[i__ + w_dim1], mdw);
/*<         RN = SDOT(N-KRANKE,W(I,KRANKE+1),MDW,W(I,KRANKE+1),MDW) >*/
#line 521 "cl2.f"
	i__2 = *n - kranke;
#line 521 "cl2.f"
	rn = sdot_(&i__2, &w[i__ + (kranke + 1) * w_dim1], mdw, &w[i__ + (
		kranke + 1) * w_dim1], mdw);
/*<         IF (.NOT.(RN.LE.TAU**2*SN .AND. KRANKE.LT.N)) GO TO 250 >*/
/* Computing 2nd power */
#line 522 "cl2.f"
	r__1 = tau;
#line 522 "cl2.f"
	if (! (rn <= r__1 * r__1 * sn && kranke < *n)) {
#line 522 "cl2.f"
	    goto L250;
#line 522 "cl2.f"
	}
/*<         W(I,KRANKE+1) = ZERO >*/
#line 523 "cl2.f"
	w[i__ + (kranke + 1) * w_dim1] = zero;
/*<         CALL SCOPY(N-KRANKE, W(I,KRANKE+1), 0, W(I,KRANKE+1), MDW) >*/
#line 524 "cl2.f"
	i__2 = *n - kranke;
#line 524 "cl2.f"
	scopy_(&i__2, &w[i__ + (kranke + 1) * w_dim1], &c__0, &w[i__ + (
		kranke + 1) * w_dim1], mdw);
/*<   250   CONTINUE >*/
#line 525 "cl2.f"
L250:
/*<   260 CONTINUE >*/
#line 526 "cl2.f"
/* L260: */
#line 526 "cl2.f"
	;
#line 526 "cl2.f"
    }

/*     COMPUTE EQUAL. CONSTRAINT EQUAS. RESIDUAL LENGTH. */
/*<   270 RNORME = SNRM2(ME-KRANKE,W(KRANKE+1,NP1),1) >*/
#line 529 "cl2.f"
L270:
#line 529 "cl2.f"
    i__1 = *me - kranke;
#line 529 "cl2.f"
    *rnorme = snrm2_(&i__1, &w[kranke + 1 + np1 * w_dim1], &c__1);

/*     MOVE REDUCED PROBLEM DATA UPWARD IF KRANKE.LT.ME. */
/*<       IF (.NOT.(KRANKE.LT.ME)) GO TO 290 >*/
#line 532 "cl2.f"
    if (! (kranke < *me)) {
#line 532 "cl2.f"
	goto L290;
#line 532 "cl2.f"
    }
/*<       DO 280 J=1,NP1 >*/
#line 533 "cl2.f"
    i__1 = np1;
#line 533 "cl2.f"
    for (j = 1; j <= i__1; ++j) {
/*<         CALL SCOPY(M-ME, W(ME+1,J), 1, W(KRANKE+1,J), 1) >*/
#line 534 "cl2.f"
	i__2 = m - *me;
#line 534 "cl2.f"
	scopy_(&i__2, &w[*me + 1 + j * w_dim1], &c__1, &w[kranke + 1 + j *
		w_dim1], &c__1);
/*<   280 CONTINUE >*/
#line 535 "cl2.f"
/* L280: */
#line 535 "cl2.f"
    }

/*     COMPUTE SOLN OF REDUCED PROBLEM. */
/*<    >*/
#line 538 "cl2.f"
L290:
#line 538 "cl2.f"
    i__1 = *n - kranke;
#line 538 "cl2.f"
    lsi_(&w[kranke + 1 + (kranke + 1) * w_dim1], mdw, ma, mg, &i__1, &prgopt[
	    1], &x[kranke + 1], rnorml, mode, &ws[n2], &ip[2]);
/*<       IF (.NOT.(ME.GT.0)) GO TO 330 >*/
#line 540 "cl2.f"
    if (! (*me > 0)) {
#line 540 "cl2.f"
	goto L330;
#line 540 "cl2.f"
    }

/*     TEST FOR CONSISTENCY OF EQUALITY CONSTRAINTS. */
/*<       MDEQC = 0 >*/
#line 543 "cl2.f"
    mdeqc = 0;
/*<       XNRME = SASUM(KRANKE,W(1,NP1),1) >*/
#line 544 "cl2.f"
    xnrme = sasum_(&kranke, &w[np1 * w_dim1 + 1], &c__1);
/*<       IF (RNORME.GT.TAU*(ENORM*XNRME+FNORM)) MDEQC = 1 >*/
#line 545 "cl2.f"
    if (*rnorme > tau * (enorm * xnrme + fnorm)) {
#line 545 "cl2.f"
	mdeqc = 1;
#line 545 "cl2.f"
    }
/*<       MODE = MODE + MDEQC >*/
#line 546 "cl2.f"
    *mode += mdeqc;

/*     CHECK IF SOLN TO EQUAL. CONSTRAINTS SATISFIES INEQUAL. */
/*     CONSTRAINTS WHEN THERE ARE NO DEGREES OF FREEDOM LEFT. */
/*<       IF (.NOT.(KRANKE.EQ.N .AND. MG.GT.0)) GO TO 320 >*/
#line 550 "cl2.f"
    if (! (kranke == *n && *mg > 0)) {
#line 550 "cl2.f"
	goto L320;
#line 550 "cl2.f"
    }
/*<       XNORM = SASUM(N,X,1) >*/
#line 551 "cl2.f"
    xnorm = sasum_(n, &x[1], &c__1);
/*<       MAPKE1 = MA + KRANKE + 1 >*/
#line 552 "cl2.f"
    mapke1 = *ma + kranke + 1;
/*<       MEND = MA + KRANKE + MG >*/
#line 553 "cl2.f"
    mend = *ma + kranke + *mg;
/*<       DO 310 I=MAPKE1,MEND >*/
#line 554 "cl2.f"
    i__1 = mend;
#line 554 "cl2.f"
    for (i__ = mapke1; i__ <= i__1; ++i__) {
/*<         SIZE = SASUM(N,W(I,1),MDW)*XNORM + ABS(W(I,NP1)) >*/
#line 555 "cl2.f"
	size = sasum_(n, &w[i__ + w_dim1], mdw) * xnorm + (r__1 = w[i__ + np1
		* w_dim1], abs(r__1));
/*<         IF (.NOT.(W(I,NP1).GT.TAU*SIZE)) GO TO 300 >*/
#line 556 "cl2.f"
	if (! (w[i__ + np1 * w_dim1] > tau * size)) {
#line 556 "cl2.f"
	    goto L300;
#line 556 "cl2.f"
	}
/*<         MODE = MODE + 2 >*/
#line 557 "cl2.f"
	*mode += 2;
/*<         GO TO 450 >*/
#line 558 "cl2.f"
	goto L450;
/*<   300   CONTINUE >*/
#line 559 "cl2.f"
L300:
/*<   310 CONTINUE >*/
#line 560 "cl2.f"
/* L310: */
#line 560 "cl2.f"
	;
#line 560 "cl2.f"
    }
/*<   320 CONTINUE >*/
#line 561 "cl2.f"
L320:
/*<   330 IF (.NOT.(KRANKE.GT.0)) GO TO 420 >*/
#line 562 "cl2.f"
L330:
#line 562 "cl2.f"
    if (! (kranke > 0)) {
#line 562 "cl2.f"
	goto L420;
#line 562 "cl2.f"
    }

/*     REPLACE DIAG. TERMS OF LOWER TRAP. MATRIX. */
/*<       CALL SCOPY(KRANKE, WS(KRANKE+1), 1, W, MDW+1) >*/
#line 565 "cl2.f"
    i__1 = *mdw + 1;
#line 565 "cl2.f"
    scopy_(&kranke, &ws[kranke + 1], &c__1, &w[w_offset], &i__1);

/*     REAPPLY TRANS TO PUT SOLN IN ORIGINAL COORDINATES. */
/*<       DO 340 II=1,KRANKE >*/
#line 568 "cl2.f"
    i__1 = kranke;
#line 568 "cl2.f"
    for (ii = 1; ii <= i__1; ++ii) {
/*<         I = KRANKE + 1 - II >*/
#line 569 "cl2.f"
	i__ = kranke + 1 - ii;
/*<         CALL H12(2, I, I+1, N, W(I,1), MDW, WS(I), X, 1, 1, 1) >*/
#line 570 "cl2.f"
	i__2 = i__ + 1;
#line 570 "cl2.f"
	h12_(&c__2, &i__, &i__2, n, &w[i__ + w_dim1], mdw, &ws[i__], &x[1], &
		c__1, &c__1, &c__1);
/*<   340 CONTINUE >*/
#line 571 "cl2.f"
/* L340: */
#line 571 "cl2.f"
    }

/*     COMPUTE COV MATRIX OF EQUAL. CONSTRAINED PROBLEM. */
/*<       IF (.NOT.(COV)) GO TO 410 >*/
#line 574 "cl2.f"
    if (! cov) {
#line 574 "cl2.f"
	goto L410;
#line 574 "cl2.f"
    }
/*<       DO 400 JJ=1,KRANKE >*/
#line 575 "cl2.f"
    i__1 = kranke;
#line 575 "cl2.f"
    for (jj = 1; jj <= i__1; ++jj) {
/*<         J = KRANKE + 1 - JJ >*/
#line 576 "cl2.f"
	j = kranke + 1 - jj;
/*<         IF (.NOT.(J.LT.N)) GO TO 390 >*/
#line 577 "cl2.f"
	if (! (j < *n)) {
#line 577 "cl2.f"
	    goto L390;
#line 577 "cl2.f"
	}
/*<         RB = WS(J)*W(J,J) >*/
#line 578 "cl2.f"
	rb = ws[j] * w[j + j * w_dim1];
/*<         IF (RB.NE.ZERO) RB = ONE/RB >*/
#line 579 "cl2.f"
	if (rb != zero) {
#line 579 "cl2.f"
	    rb = one / rb;
#line 579 "cl2.f"
	}
/*<         JP1 = J + 1 >*/
#line 580 "cl2.f"
	jp1 = j + 1;
/*<         DO 350 I=JP1,N >*/
#line 581 "cl2.f"
	i__2 = *n;
#line 581 "cl2.f"
	for (i__ = jp1; i__ <= i__2; ++i__) {
/*<           W(I,J) = SDOT(N-J,W(I,JP1),MDW,W(J,JP1),MDW)*RB >*/
#line 582 "cl2.f"
	    i__3 = *n - j;
#line 582 "cl2.f"
	    w[i__ + j * w_dim1] = sdot_(&i__3, &w[i__ + jp1 * w_dim1], mdw, &
		    w[j + jp1 * w_dim1], mdw) * rb;
/*<   350   CONTINUE >*/
#line 583 "cl2.f"
/* L350: */
#line 583 "cl2.f"
	}
/*<         GAM = SDOT(N-J,W(JP1,J),1,W(J,JP1),MDW)*RB >*/
#line 584 "cl2.f"
	i__2 = *n - j;
#line 584 "cl2.f"
	gam = sdot_(&i__2, &w[jp1 + j * w_dim1], &c__1, &w[j + jp1 * w_dim1],
		mdw) * rb;
/*<         GAM = HALF*GAM >*/
#line 585 "cl2.f"
	gam = half * gam;
/*<         CALL SAXPY(N-J, GAM, W(J,JP1), MDW, W(JP1,J), 1) >*/
#line 586 "cl2.f"
	i__2 = *n - j;
#line 586 "cl2.f"
	saxpy_(&i__2, &gam, &w[j + jp1 * w_dim1], mdw, &w[jp1 + j * w_dim1], &
		c__1);
/*<         DO 370 I=JP1,N >*/
#line 587 "cl2.f"
	i__2 = *n;
#line 587 "cl2.f"
	for (i__ = jp1; i__ <= i__2; ++i__) {
/*<           DO 360 K=I,N >*/
#line 588 "cl2.f"
	    i__3 = *n;
#line 588 "cl2.f"
	    for (k = i__; k <= i__3; ++k) {
/*<             W(I,K) = W(I,K) + W(J,I)*W(K,J) + W(I,J)*W(J,K) >*/
#line 589 "cl2.f"
		w[i__ + k * w_dim1] = w[i__ + k * w_dim1] + w[j + i__ *
			w_dim1] * w[k + j * w_dim1] + w[i__ + j * w_dim1] * w[
			j + k * w_dim1];
/*<             W(K,I) = W(I,K) >*/
#line 590 "cl2.f"
		w[k + i__ * w_dim1] = w[i__ + k * w_dim1];
/*<   360     CONTINUE >*/
#line 591 "cl2.f"
/* L360: */
#line 591 "cl2.f"
	    }
/*<   370   CONTINUE >*/
#line 592 "cl2.f"
/* L370: */
#line 592 "cl2.f"
	}
/*<         UJ = WS(J) >*/
#line 593 "cl2.f"
	uj = ws[j];
/*<         VJ = GAM*UJ >*/
#line 594 "cl2.f"
	vj = gam * uj;
/*<         W(J,J) = UJ*VJ + UJ*VJ >*/
#line 595 "cl2.f"
	w[j + j * w_dim1] = uj * vj + uj * vj;
/*<         DO 380 I=JP1,N >*/
#line 596 "cl2.f"
	i__2 = *n;
#line 596 "cl2.f"
	for (i__ = jp1; i__ <= i__2; ++i__) {
/*<           W(J,I) = UJ*W(I,J) + VJ*W(J,I) >*/
#line 597 "cl2.f"
	    w[j + i__ * w_dim1] = uj * w[i__ + j * w_dim1] + vj * w[j + i__ *
		    w_dim1];
/*<   380   CONTINUE >*/
#line 598 "cl2.f"
/* L380: */
#line 598 "cl2.f"
	}
/*<         CALL SCOPY(N-J, W(J,JP1), MDW, W(JP1,J), 1) >*/
#line 599 "cl2.f"
	i__2 = *n - j;
#line 599 "cl2.f"
	scopy_(&i__2, &w[j + jp1 * w_dim1], mdw, &w[jp1 + j * w_dim1], &c__1);
/*<   390   CONTINUE >*/
#line 600 "cl2.f"
L390:
/*<   400 CONTINUE >*/
#line 601 "cl2.f"
/* L400: */
#line 601 "cl2.f"
	;
#line 601 "cl2.f"
    }
/*<   410 CONTINUE >*/
#line 602 "cl2.f"
L410:

/*     APPLY THE SCALING TO THE COVARIANCE MATRIX. */
/*<   420 IF (.NOT.(COV)) GO TO 440 >*/
#line 605 "cl2.f"
L420:
#line 605 "cl2.f"
    if (! cov) {
#line 605 "cl2.f"
	goto L440;
#line 605 "cl2.f"
    }
/*<       DO 430 I=1,N >*/
#line 606 "cl2.f"
    i__1 = *n;
#line 606 "cl2.f"
    for (i__ = 1; i__ <= i__1; ++i__) {
/*<         L = N1 + I >*/
#line 607 "cl2.f"
	l = n1 + i__;
/*<         CALL SSCAL(N, WS(L-1), W(I,1), MDW) >*/
#line 608 "cl2.f"
	sscal_(n, &ws[l - 1], &w[i__ + w_dim1], mdw);
/*<         CALL SSCAL(N, WS(L-1), W(1,I), 1) >*/
#line 609 "cl2.f"
	sscal_(n, &ws[l - 1], &w[i__ * w_dim1 + 1], &c__1);
/*<   430 CONTINUE >*/
#line 610 "cl2.f"
/* L430: */
#line 610 "cl2.f"
    }
/*<   440 CONTINUE >*/
#line 611 "cl2.f"
L440:
/*<   450 CONTINUE >*/
#line 612 "cl2.f"
L450:

/*     RESCALE SOLN. VECTOR. */
/*<       IF (.NOT.(MODE.LE.1)) GO TO 470 >*/
#line 615 "cl2.f"
    if (! (*mode <= 1)) {
#line 615 "cl2.f"
	goto L470;
#line 615 "cl2.f"
    }
/*<       DO 460 J=1,N >*/
#line 616 "cl2.f"
    i__1 = *n;
#line 616 "cl2.f"
    for (j = 1; j <= i__1; ++j) {
/*<         L = N1 + J >*/
#line 617 "cl2.f"
	l = n1 + j;
/*<         X(J) = X(J)*WS(L-1) >*/
#line 618 "cl2.f"
	x[j] *= ws[l - 1];
/*<   460 CONTINUE >*/
#line 619 "cl2.f"
/* L460: */
#line 619 "cl2.f"
    }
/*<   470 IP(1) = KRANKE >*/
#line 620 "cl2.f"
L470:
#line 620 "cl2.f"
    ip[1] = kranke;
/*<       IP(3) = IP(3) + 2*KRANKE + N >*/
#line 621 "cl2.f"
    ip[3] = ip[3] + (kranke << 1) + *n;
/*<       RETURN >*/
#line 622 "cl2.f"
    return 0;
/*<   480 CONTINUE >*/
#line 623 "cl2.f"
L480:
/*     TO PROCESS-OPTION-VECTOR */

/*     THE NOMINAL TOLERANCE USED IN THE CODE */
/*     FOR THE EQUALITY CONSTRAINT EQUATIONS. */
/*<       TAU = SQRT(SRELPR) >*/
#line 628 "cl2.f"
    tau = sqrt(srelpr);

/*     THE NOMINAL COLUMN SCALING USED IN THE CODE IS */
/*     THE IDENTITY SCALING. */
/*<       WS(N1) = ONE >*/
#line 632 "cl2.f"
    ws[n1] = one;
/*<       CALL SCOPY(N, WS(N1), 0, WS(N1), 1) >*/
#line 633 "cl2.f"
    scopy_(n, &ws[n1], &c__0, &ws[n1], &c__1);

/*     NO COVARIANCE MATRIX IS NOMINALLY COMPUTED. */
/*<       COV = .FALSE. >*/
#line 636 "cl2.f"
    cov = FALSE_;

/*     DEFINE BOUND FOR NUMBER OF OPTIONS TO CHANGE. */
/*<       NOPT = 1000 >*/
#line 639 "cl2.f"
    nopt = 1000;
/*<       NTIMES = 0 >*/
#line 640 "cl2.f"
    ntimes = 0;

/*     DEFINE BOUND FOR POSITIVE VALUES OF LINK. */
/*<       NLINK = 100000 >*/
#line 643 "cl2.f"
    nlink = 100000;
/*<       LAST = 1 >*/
#line 644 "cl2.f"
    last = 1;
/*<       LINK = PRGOPT(1) >*/
#line 645 "cl2.f"
    link = prgopt[1];
/*<       IF (.NOT.(LINK.LE.0 .OR. LINK.GT.NLINK)) GO TO 490 >*/
#line 646 "cl2.f"
    if (! (link <= 0 || link > nlink)) {
#line 646 "cl2.f"
	goto L490;
#line 646 "cl2.f"
    }
/*<       NERR = 3 >*/
#line 647 "cl2.f"
    nerr = 3;
/*<       IOPT = 1 >*/
#line 648 "cl2.f"
    iopt = 1;
/* cc      CALL XERROR(38HLSEI( ) THE OPTION VECTOR IS UNDEFINED, 38, NERR, */
/* cc     * IOPT) */
/*<       MODE = 4 >*/
#line 651 "cl2.f"
    *mode = 4;
/*<       RETURN >*/
#line 652 "cl2.f"
    return 0;
/*<   490 IF (.NOT.(LINK.GT.1)) GO TO 540 >*/
#line 653 "cl2.f"
L490:
#line 653 "cl2.f"
    if (! (link > 1)) {
#line 653 "cl2.f"
	goto L540;
#line 653 "cl2.f"
    }
/*<       NTIMES = NTIMES + 1 >*/
#line 654 "cl2.f"
    ++ntimes;
/*<       IF (.NOT.(NTIMES.GT.NOPT)) GO TO 500 >*/
#line 655 "cl2.f"
    if (! (ntimes > nopt)) {
#line 655 "cl2.f"
	goto L500;
#line 655 "cl2.f"
    }
/*<       NERR = 3 >*/
#line 656 "cl2.f"
    nerr = 3;
/*<       IOPT = 1 >*/
#line 657 "cl2.f"
    iopt = 1;
/* cc      CALL XERROR( */
/* cc     * 52HLSEI( ). THE LINKS IN THE OPTION VECTOR ARE CYCLING., 52, */
/* cc     * NERR, IOPT) */
/*<       MODE = 4 >*/
#line 661 "cl2.f"
    *mode = 4;
/*<       RETURN >*/
#line 662 "cl2.f"
    return 0;
/*<   500 KEY = PRGOPT(LAST+1) >*/
#line 663 "cl2.f"
L500:
#line 663 "cl2.f"
    key = prgopt[last + 1];
/*<       IF (KEY.EQ.1) COV = PRGOPT(LAST+2).NE.ZERO >*/
#line 664 "cl2.f"
    if (key == 1) {
#line 664 "cl2.f"
	cov = prgopt[last + 2] != zero;
#line 664 "cl2.f"
    }
/*<       IF (.NOT.(KEY.EQ.2 .AND. PRGOPT(LAST+2).NE.ZERO)) GO TO 520 >*/
#line 665 "cl2.f"
    if (! (key == 2 && prgopt[last + 2] != zero)) {
#line 665 "cl2.f"
	goto L520;
#line 665 "cl2.f"
    }
/*<       DO 510 J=1,N >*/
#line 666 "cl2.f"
    i__1 = *n;
#line 666 "cl2.f"
    for (j = 1; j <= i__1; ++j) {
/*<         T = SNRM2(M,W(1,J),1) >*/
#line 667 "cl2.f"
	t = snrm2_(&m, &w[j * w_dim1 + 1], &c__1);
/*<         IF (T.NE.ZERO) T = ONE/T >*/
#line 668 "cl2.f"
	if (t != zero) {
#line 668 "cl2.f"
	    t = one / t;
#line 668 "cl2.f"
	}
/*<         L = N1 + J >*/
#line 669 "cl2.f"
	l = n1 + j;
/*<         WS(L-1) = T >*/
#line 670 "cl2.f"
	ws[l - 1] = t;
/*<   510 CONTINUE >*/
#line 671 "cl2.f"
/* L510: */
#line 671 "cl2.f"
    }
/*<   520 IF (KEY.EQ.3) CALL SCOPY(N, PRGOPT(LAST+2), 1, WS(N1), 1) >*/
#line 672 "cl2.f"
L520:
#line 672 "cl2.f"
    if (key == 3) {
#line 672 "cl2.f"
	scopy_(n, &prgopt[last + 2], &c__1, &ws[n1], &c__1);
#line 672 "cl2.f"
    }
/*<       IF (KEY.EQ.4) TAU = AMAX1(SRELPR,PRGOPT(LAST+2)) >*/
#line 673 "cl2.f"
    if (key == 4) {
/* Computing MAX */
#line 673 "cl2.f"
	r__1 = srelpr, r__2 = prgopt[last + 2];
#line 673 "cl2.f"
	tau = max(r__1,r__2);
#line 673 "cl2.f"
    }
/*<       NEXT = PRGOPT(LINK) >*/
#line 674 "cl2.f"
    next = prgopt[link];
/*<       IF (.NOT.(NEXT.LE.0 .OR. NEXT.GT.NLINK)) GO TO 530 >*/
#line 675 "cl2.f"
    if (! (next <= 0 || next > nlink)) {
#line 675 "cl2.f"
	goto L530;
#line 675 "cl2.f"
    }
/*<       NERR = 3 >*/
#line 676 "cl2.f"
    nerr = 3;
/*<       IOPT = 1 >*/
#line 677 "cl2.f"
    iopt = 1;
/* cc      CALL XERROR(38HLSEI( ) THE OPTION VECTOR IS UNDEFINED, 38, NERR, */
/* cc     * IOPT) */
/*<       MODE = 4 >*/
#line 680 "cl2.f"
    *mode = 4;
/*<       RETURN >*/
#line 681 "cl2.f"
    return 0;
/*<   530 LAST = LINK >*/
#line 682 "cl2.f"
L530:
#line 682 "cl2.f"
    last = link;
/*<       LINK = NEXT >*/
#line 683 "cl2.f"
    link = next;
/*<       GO TO 490 >*/
#line 684 "cl2.f"
    goto L490;
/*<   540 DO 550 J=1,N >*/
#line 685 "cl2.f"
L540:
#line 685 "cl2.f"
    i__1 = *n;
#line 685 "cl2.f"
    for (j = 1; j <= i__1; ++j) {
/*<         L = N1 + J >*/
#line 686 "cl2.f"
	l = n1 + j;
/*<         CALL SSCAL(M, WS(L-1), W(1,J), 1) >*/
#line 687 "cl2.f"
	sscal_(&m, &ws[l - 1], &w[j * w_dim1 + 1], &c__1);
/*<   550 CONTINUE >*/
#line 688 "cl2.f"
/* L550: */
#line 688 "cl2.f"
    }
/*<       GO TO 560 >*/
#line 689 "cl2.f"
    goto L560;
/*<   560 GO TO IGO990, (90) >*/
#line 690 "cl2.f"
L560:
#line 690 "cl2.f"
    switch (igo990) {
#line 690 "cl2.f"
	case 0: goto L90;
#line 690 "cl2.f"
    }
    return 0 ;
/*<       END >*/
} /* lsei_ */

/* CC */
/* CC---------------------------------------------------------------------------- */
/* CC */
/*<       SUBROUTINE LSI(W, MDW, MA, MG, N, PRGOPT, X, RNORM, MODE, WS, IP)  >*/
static /* Subroutine */ int lsi_(real *w, integer *mdw, integer *ma, integer *mg,
	integer *n, real *prgopt, real *x, real *rnorm, integer *mode, real *
	ws, integer *ip)
{
    /* Initialized data */

    static real zero = 0.f;
    static real srelpr = 0.f;
    static real one = 1.f;
    static real half = .5f;

    /* Format strings */
    static char fmt_40[] = "";
    static char fmt_60[] = "";

    /* System generated locals */
    integer w_dim1, w_offset, i__1, i__2, i__3, i__4;
    real r__1, r__2;

    /* Builtin functions */
    double sqrt(doublereal);

    /* Local variables */
    static integer i__, j, k, l, m, n1, n2, n3;
    static integer ii;
    static real rb;
    static integer il, im1, ip1, np1;
    static real fac, gam, tau;
    static logical cov;
    static integer key;
    static real tol;
    static integer map1, krm1, krp1;
    static integer link;
    static integer last;
    static integer next, igo990, igo994;
    static integer krank;
    static real anorm;
    static real xnorm;
    static integer minman, mdlpdp;

    /* Assigned format variables */
    static char *igo994_fmt, *igo990_fmt;


/*     THE EDITING REQUIRED TO CONVERT THIS SUBROUTINE FROM SINGLE TO */
/*     DOUBLE PRECISION INVOLVES THE FOLLOWING CHARACTER STRING CHANGES. */
/*     USE AN EDITING COMMAND (CHANGE) /STRING-1/(TO)STRING-2/. */
/*     (START EDITING AT LINE WITH C++ IN COLS. 1-3.) */
/*     /REAL (12 BLANKS)/DOUBLE PRECISION/,/SASUM/DASUM/,/SDOT/DDOT/, */
/*     / SQRT/ DSQRT/,/AMAX1/DMAX1/,/SSWAP/DSWAP/, */
/*     /SCOPY/DCOPY/,/SSCAL/DSCAL/,/SAXPY/DAXPY/,/E0/D0/,/SRELPR/DRELPR/ */

/*     THIS IS A COMPANION SUBPROGRAM TO LSEI( ). */
/*     THE DOCUMENTATION FOR LSEI( ) HAS MORE COMPLETE */
/*     USAGE INSTRUCTIONS. */
/*     WRITTEN BY R. J. HANSON, SLA. */

/*     SOLVE.. */
/*              AX = B,  A  MA BY N  (LEAST SQUARES EQUATIONS) */
/*     SUBJECT TO.. */

/*              GX.GE.H, G  MG BY N  (INEQUALITY CONSTRAINTS) */

/*     INPUT.. */

/*      W(*,*) CONTAINS  (A B) IN ROWS 1,...,MA+MG, COLS 1,...,N+1. */
/*                       (G H) */

/*     MDW,MA,MG,N */
/*              CONTAIN (RESP) VAR. DIMENSION OF W(*,*), */
/*              AND MATRIX DIMENSIONS. */

/*     PRGOPT(*), */
/*              PROGRAM OPTION VECTOR. */

/*     OUTPUT.. */

/*      X(*),RNORM */

/*              SOLUTION VECTOR(UNLESS MODE=2), LENGTH OF AX-B. */

/*      MODE */
/*              =0   INEQUALITY CONSTRAINTS ARE COMPATIBLE. */
/*              =2   INEQUALITY CONSTRAINTS CONTRADICTORY. */

/*      WS(*), */
/*              WORKING STORAGE OF DIMENSION K+N+(MG+2)*(N+7), */
/*              WHERE K=MAX(MA+MG,N). */
/*      IP(MG+2*N+1) */
/*              INTEGER WORKING STORAGE */
/*      REVISED OCT. 1, 1981. */

/*     SUBROUTINES CALLED */

/*     LPDP          THIS SUBPROGRAM MINIMIZES A SUM OF SQUARES */
/*                   OF UNKNOWNS SUBJECT TO LINEAR INEQUALITY */
/*                   CONSTRAINTS.  PART OF THIS PACKAGE. */

/* ++ */
/*     SDOT,SSCAL    SUBROUTINES FROM THE BLAS PACKAGE. */
/*     SAXPY,SASUM,  SEE TRANS. MATH. SOFT., VOL. 5, NO. 3, P. 308. */
/*     SCOPY,SSWAP */

/*     HFTI          SOLVES AN UNCONSTRAINED LINEAR LEAST SQUARES */
/*                   PROBLEM.  PART OF THIS PACKAGE. */

/*     H12           SUBROUTINE TO CONSTRUCT AND APPLY A HOUSEHOLDER */
/*                   TRANSFORMATION. */

/*     SUBROUTINE LSI(W,MDW,MA,MG,N,PRGOPT,X,RNORM,MODE,WS,IP) */

/*<       REAL             W(MDW,1), PRGOPT(1), RNORM, WS(1), X(1) >*/
/*<       INTEGER IP(1) >*/
/*<    >*/
/*<       REAL             SASUM, SDOT, SQRT, AMAX1 >*/
/*<       LOGICAL COV >*/

/*<       DATA ZERO /0.E0/, SRELPR /0.E0/, ONE /1.E0/, HALF /.5E0/ >*/
#line 771 "cl2.f"
    /* Parameter adjustments */
#line 771 "cl2.f"
    w_dim1 = *mdw;
#line 771 "cl2.f"
    w_offset = 1 + w_dim1;
#line 771 "cl2.f"
    w -= w_offset;
#line 771 "cl2.f"
    --prgopt;
#line 771 "cl2.f"
    --x;
#line 771 "cl2.f"
    --ws;
#line 771 "cl2.f"
    --ip;
#line 771 "cl2.f"

#line 771 "cl2.f"
    /* Function Body */

/*     COMPUTE MACHINE PRECISION=SRELPR ONLY WHEN NECESSARY. */
/*<       IF (.NOT.(SRELPR.EQ.ZERO)) GO TO 30 >*/
#line 774 "cl2.f"
    if (! (srelpr == zero)) {
#line 774 "cl2.f"
	goto L30;
#line 774 "cl2.f"
    }
/*<       SRELPR = ONE >*/
#line 775 "cl2.f"
    srelpr = one;
/*<    10 IF (ONE+SRELPR.EQ.ONE) GO TO 20 >*/
#line 776 "cl2.f"
L10:
#line 776 "cl2.f"
    if (one + srelpr == one) {
#line 776 "cl2.f"
	goto L20;
#line 776 "cl2.f"
    }
/*<       SRELPR = SRELPR*HALF >*/
#line 777 "cl2.f"
    srelpr *= half;
/*<       GO TO 10 >*/
#line 778 "cl2.f"
    goto L10;
/*<    20 SRELPR = SRELPR + SRELPR >*/
#line 779 "cl2.f"
L20:
#line 779 "cl2.f"
    srelpr += srelpr;
/*<    30 MODE = 0 >*/
#line 780 "cl2.f"
L30:
#line 780 "cl2.f"
    *mode = 0;
/*<       RNORM = ZERO >*/
#line 781 "cl2.f"
    *rnorm = zero;
/*<       M = MA + MG >*/
#line 782 "cl2.f"
    m = *ma + *mg;
/*<       NP1 = N + 1 >*/
#line 783 "cl2.f"
    np1 = *n + 1;
/*<       KRANK = 0 >*/
#line 784 "cl2.f"
    krank = 0;
/*<       IF (N.LE.0 .OR. M.LE.0) GO TO 70 >*/
#line 785 "cl2.f"
    if (*n <= 0 || m <= 0) {
#line 785 "cl2.f"
	goto L70;
#line 785 "cl2.f"
    }
/*<       ASSIGN 40 TO IGO994 >*/
#line 786 "cl2.f"
    igo994 = 0;
#line 786 "cl2.f"
    igo994_fmt = fmt_40;
/*<       GO TO 500 >*/
#line 787 "cl2.f"
    goto L500;

/*     PROCESS-OPTION-VECTOR */

/*     COMPUTE MATRIX NORM OF LEAST SQUARES EQUAS. */
/*<    40 ANORM = ZERO >*/
#line 792 "cl2.f"
L40:
#line 792 "cl2.f"
    anorm = zero;
/*<       DO 50 J=1,N >*/
#line 793 "cl2.f"
    i__1 = *n;
#line 793 "cl2.f"
    for (j = 1; j <= i__1; ++j) {
/*<         ANORM = AMAX1(ANORM,SASUM(MA,W(1,J),1)) >*/
/* Computing MAX */
#line 794 "cl2.f"
	r__1 = anorm, r__2 = sasum_(ma, &w[j * w_dim1 + 1], &c__1);
#line 794 "cl2.f"
	anorm = max(r__1,r__2);
/*<    50 CONTINUE >*/
#line 795 "cl2.f"
/* L50: */
#line 795 "cl2.f"
    }

/*     SET TOL FOR HFTI( ) RANK TEST. */
/*<       TAU = TOL*ANORM >*/
#line 798 "cl2.f"
    tau = tol * anorm;

/*     COMPUTE HOUSEHOLDER ORTHOGONAL DECOMP OF MATRIX. */
/*<       IF (N.GT.0) WS(1) = ZERO >*/
#line 801 "cl2.f"
    if (*n > 0) {
#line 801 "cl2.f"
	ws[1] = zero;
#line 801 "cl2.f"
    }
/*<       CALL SCOPY(N, WS, 0, WS, 1) >*/
#line 802 "cl2.f"
    scopy_(n, &ws[1], &c__0, &ws[1], &c__1);
/*<       CALL SCOPY(MA, W(1,NP1), 1, WS, 1) >*/
#line 803 "cl2.f"
    scopy_(ma, &w[np1 * w_dim1 + 1], &c__1, &ws[1], &c__1);
/*<       K = MAX0(M,N) >*/
#line 804 "cl2.f"
    k = max(m,*n);
/*<       MINMAN = MIN0(MA,N) >*/
#line 805 "cl2.f"
    minman = min(*ma,*n);
/*<       N1 = K + 1 >*/
#line 806 "cl2.f"
    n1 = k + 1;
/*<       N2 = N1 + N >*/
#line 807 "cl2.f"
    n2 = n1 + *n;
/*<    >*/
#line 808 "cl2.f"
    hfti_(&w[w_offset], mdw, ma, n, &ws[1], &c__1, &c__1, &tau, &krank, rnorm,
	     &ws[n2], &ws[n1], &ip[1]);
/*<       FAC = ONE >*/
#line 810 "cl2.f"
    fac = one;
/*<       GAM=MA-KRANK >*/
#line 811 "cl2.f"
    gam = (real) (*ma - krank);
/*<       IF (KRANK.LT.MA) FAC = RNORM**2/GAM >*/
#line 812 "cl2.f"
    if (krank < *ma) {
/* Computing 2nd power */
#line 812 "cl2.f"
	r__1 = *rnorm;
#line 812 "cl2.f"
	fac = r__1 * r__1 / gam;
#line 812 "cl2.f"
    }
/*<       ASSIGN 60 TO IGO990 >*/
#line 813 "cl2.f"
    igo990 = 0;
#line 813 "cl2.f"
    igo990_fmt = fmt_60;
/*<       GO TO 80 >*/
#line 814 "cl2.f"
    goto L80;

/*     REDUCE-TO-LPDP-AND-SOLVE */
/*<    60 CONTINUE >*/
#line 817 "cl2.f"
L60:
/*<    70 IP(1) = KRANK >*/
#line 818 "cl2.f"
L70:
#line 818 "cl2.f"
    ip[1] = krank;
/*<       IP(2) = N + MAX0(M,N) + (MG+2)*(N+7) >*/
#line 819 "cl2.f"
    ip[2] = *n + max(m,*n) + (*mg + 2) * (*n + 7);
/*<       RETURN >*/
#line 820 "cl2.f"
    return 0;
/*<    80 CONTINUE >*/
#line 821 "cl2.f"
L80:

/*     TO REDUCE-TO-LPDP-AND-SOLVE */
/*<       MAP1 = MA + 1 >*/
#line 824 "cl2.f"
    map1 = *ma + 1;

/*     COMPUTE INEQ. RT-HAND SIDE FOR LPDP. */
/*<       IF (.NOT.(MA.LT.M)) GO TO 260 >*/
#line 827 "cl2.f"
    if (! (*ma < m)) {
#line 827 "cl2.f"
	goto L260;
#line 827 "cl2.f"
    }
/*<       IF (.NOT.(MINMAN.GT.0)) GO TO 160 >*/
#line 828 "cl2.f"
    if (! (minman > 0)) {
#line 828 "cl2.f"
	goto L160;
#line 828 "cl2.f"
    }
/*<       DO 90 I=MAP1,M >*/
#line 829 "cl2.f"
    i__1 = m;
#line 829 "cl2.f"
    for (i__ = map1; i__ <= i__1; ++i__) {
/*<         W(I,NP1) = W(I,NP1) - SDOT(N,W(I,1),MDW,WS,1) >*/
#line 830 "cl2.f"
	w[i__ + np1 * w_dim1] -= sdot_(n, &w[i__ + w_dim1], mdw, &ws[1], &
		c__1);
/*<    90 CONTINUE >*/
#line 831 "cl2.f"
/* L90: */
#line 831 "cl2.f"
    }
/*<       DO 100 I=1,MINMAN >*/
#line 832 "cl2.f"
    i__1 = minman;
#line 832 "cl2.f"
    for (i__ = 1; i__ <= i__1; ++i__) {
/*<         J = IP(I) >*/
#line 833 "cl2.f"
	j = ip[i__];

/*     APPLY PERMUTATIONS TO COLS OF INEQ. CONSTRAINT MATRIX. */
/*<         CALL SSWAP(MG, W(MAP1,I), 1, W(MAP1,J), 1) >*/
#line 836 "cl2.f"
	sswap_(mg, &w[map1 + i__ * w_dim1], &c__1, &w[map1 + j * w_dim1], &
		c__1);
/*<   100 CONTINUE >*/
#line 837 "cl2.f"
/* L100: */
#line 837 "cl2.f"
    }

/*     APPLY HOUSEHOLDER TRANSFORMATIONS TO CONSTRAINT MATRIX. */
/*<       IF (.NOT.(0.LT.KRANK .AND. KRANK.LT.N)) GO TO 120 >*/
#line 840 "cl2.f"
    if (! (0 < krank && krank < *n)) {
#line 840 "cl2.f"
	goto L120;
#line 840 "cl2.f"
    }
/*<       DO 110 II=1,KRANK >*/
#line 841 "cl2.f"
    i__1 = krank;
#line 841 "cl2.f"
    for (ii = 1; ii <= i__1; ++ii) {
/*<         I = KRANK + 1 - II >*/
#line 842 "cl2.f"
	i__ = krank + 1 - ii;
/*<         L = N1 + I >*/
#line 843 "cl2.f"
	l = n1 + i__;
/*<    >*/
#line 844 "cl2.f"
	i__2 = krank + 1;
#line 844 "cl2.f"
	h12_(&c__2, &i__, &i__2, n, &w[i__ + w_dim1], mdw, &ws[l - 1], &w[
		map1 + w_dim1], mdw, &c__1, mg);
/*<   110 CONTINUE >*/
#line 846 "cl2.f"
/* L110: */
#line 846 "cl2.f"
    }

/*     COMPUTE PERMUTED INEQ. CONSTR. MATRIX TIMES R-INVERSE. */
/*<   120 DO 150 I=MAP1,M >*/
#line 849 "cl2.f"
L120:
#line 849 "cl2.f"
    i__1 = m;
#line 849 "cl2.f"
    for (i__ = map1; i__ <= i__1; ++i__) {
/*<         IF (.NOT.(0.LT.KRANK)) GO TO 140 >*/
#line 850 "cl2.f"
	if (! (0 < krank)) {
#line 850 "cl2.f"
	    goto L140;
#line 850 "cl2.f"
	}
/*<         DO 130 J=1,KRANK >*/
#line 851 "cl2.f"
	i__2 = krank;
#line 851 "cl2.f"
	for (j = 1; j <= i__2; ++j) {
/*<           W(I,J) = (W(I,J)-SDOT(J-1,W(1,J),1,W(I,1),MDW))/W(J,J) >*/
#line 852 "cl2.f"
	    i__3 = j - 1;
#line 852 "cl2.f"
	    w[i__ + j * w_dim1] = (w[i__ + j * w_dim1] - sdot_(&i__3, &w[j *
		    w_dim1 + 1], &c__1, &w[i__ + w_dim1], mdw)) / w[j + j *
		    w_dim1];
/*<   130   CONTINUE >*/
#line 853 "cl2.f"
/* L130: */
#line 853 "cl2.f"
	}
/*<   140   CONTINUE >*/
#line 854 "cl2.f"
L140:
/*<   150 CONTINUE >*/
#line 855 "cl2.f"
/* L150: */
#line 855 "cl2.f"
	;
#line 855 "cl2.f"
    }

/*     SOLVE THE REDUCED PROBLEM WITH LPDP ALGORITHM, */
/*     THE LEAST PROJECTED DISTANCE PROBLEM. */
/*<    >*/
#line 859 "cl2.f"
L160:
#line 859 "cl2.f"
    i__1 = *n - krank;
#line 859 "cl2.f"
    lpdp_(&w[map1 + w_dim1], mdw, mg, &krank, &i__1, &prgopt[1], &x[1], &
	    xnorm, &mdlpdp, &ws[n2], &ip[*n + 1]);
/*<       IF (.NOT.(MDLPDP.EQ.1)) GO TO 240 >*/
#line 861 "cl2.f"
    if (! (mdlpdp == 1)) {
#line 861 "cl2.f"
	goto L240;
#line 861 "cl2.f"
    }
/*<       IF (.NOT.(KRANK.GT.0)) GO TO 180 >*/
#line 862 "cl2.f"
    if (! (krank > 0)) {
#line 862 "cl2.f"
	goto L180;
#line 862 "cl2.f"
    }

/*     COMPUTE SOLN IN ORIGINAL COORDINATES. */
/*<       DO 170 II=1,KRANK >*/
#line 865 "cl2.f"
    i__1 = krank;
#line 865 "cl2.f"
    for (ii = 1; ii <= i__1; ++ii) {
/*<         I = KRANK + 1 - II >*/
#line 866 "cl2.f"
	i__ = krank + 1 - ii;
/*<         X(I) = (X(I)-SDOT(II-1,W(I,I+1),MDW,X(I+1),1))/W(I,I) >*/
#line 867 "cl2.f"
	i__2 = ii - 1;
#line 867 "cl2.f"
	x[i__] = (x[i__] - sdot_(&i__2, &w[i__ + (i__ + 1) * w_dim1], mdw, &x[
		i__ + 1], &c__1)) / w[i__ + i__ * w_dim1];
/*<   170 CONTINUE >*/
#line 868 "cl2.f"
/* L170: */
#line 868 "cl2.f"
    }

/*     APPLY HOUSEHOLDER TRANS. TO SOLN VECTOR. */
/*<   180 IF (.NOT.(0.LT.KRANK .AND. KRANK.LT.N)) GO TO 200 >*/
#line 871 "cl2.f"
L180:
#line 871 "cl2.f"
    if (! (0 < krank && krank < *n)) {
#line 871 "cl2.f"
	goto L200;
#line 871 "cl2.f"
    }
/*<       DO 190 I=1,KRANK >*/
#line 872 "cl2.f"
    i__1 = krank;
#line 872 "cl2.f"
    for (i__ = 1; i__ <= i__1; ++i__) {
/*<         L = N1 + I >*/
#line 873 "cl2.f"
	l = n1 + i__;
/*<         CALL H12(2, I, KRANK+1, N, W(I,1), MDW, WS(L-1), X, 1, 1, 1) >*/
#line 874 "cl2.f"
	i__2 = krank + 1;
#line 874 "cl2.f"
	h12_(&c__2, &i__, &i__2, n, &w[i__ + w_dim1], mdw, &ws[l - 1], &x[1],
		&c__1, &c__1, &c__1);
/*<   190 CONTINUE >*/
#line 875 "cl2.f"
/* L190: */
#line 875 "cl2.f"
    }
/*<   200 IF (.NOT.(MINMAN.GT.0)) GO TO 230 >*/
#line 876 "cl2.f"
L200:
#line 876 "cl2.f"
    if (! (minman > 0)) {
#line 876 "cl2.f"
	goto L230;
#line 876 "cl2.f"
    }

/*     REPERMUTE VARIABLES TO THEIR INPUT ORDER. */
/*<       DO 210 II=1,MINMAN >*/
#line 879 "cl2.f"
    i__1 = minman;
#line 879 "cl2.f"
    for (ii = 1; ii <= i__1; ++ii) {
/*<         I = MINMAN + 1 - II >*/
#line 880 "cl2.f"
	i__ = minman + 1 - ii;
/*<         J = IP(I) >*/
#line 881 "cl2.f"
	j = ip[i__];
/*<         CALL SSWAP(1, X(I), 1, X(J), 1) >*/
#line 882 "cl2.f"
	sswap_(&c__1, &x[i__], &c__1, &x[j], &c__1);
/*<   210 CONTINUE >*/
#line 883 "cl2.f"
/* L210: */
#line 883 "cl2.f"
    }

/*     VARIABLES ARE NOW IN ORIG. COORDINATES. */
/*     ADD SOLN OF UNSCONSTRAINED PROB. */
/*<       DO 220 I=1,N >*/
#line 887 "cl2.f"
    i__1 = *n;
#line 887 "cl2.f"
    for (i__ = 1; i__ <= i__1; ++i__) {
/*<         X(I) = X(I) + WS(I) >*/
#line 888 "cl2.f"
	x[i__] += ws[i__];
/*<   220 CONTINUE >*/
#line 889 "cl2.f"
/* L220: */
#line 889 "cl2.f"
    }

/*     COMPUTE THE RESIDUAL VECTOR NORM. */
/*<       RNORM = SQRT(RNORM**2+XNORM**2) >*/
/* Computing 2nd power */
#line 892 "cl2.f"
    r__1 = *rnorm;
/* Computing 2nd power */
#line 892 "cl2.f"
    r__2 = xnorm;
#line 892 "cl2.f"
    *rnorm = sqrt(r__1 * r__1 + r__2 * r__2);
/*<   230 GO TO 250 >*/
#line 893 "cl2.f"
L230:
#line 893 "cl2.f"
    goto L250;
/*<   240 MODE = 2 >*/
#line 894 "cl2.f"
L240:
#line 894 "cl2.f"
    *mode = 2;
/*<   250 GO TO 270 >*/
#line 895 "cl2.f"
L250:
#line 895 "cl2.f"
    goto L270;
/*<   260 CALL SCOPY(N, WS, 1, X, 1) >*/
#line 896 "cl2.f"
L260:
#line 896 "cl2.f"
    scopy_(n, &ws[1], &c__1, &x[1], &c__1);
/*<   270 IF (.NOT.(COV .AND. KRANK.GT.0)) GO TO 490 >*/
#line 897 "cl2.f"
L270:
#line 897 "cl2.f"
    if (! (cov && krank > 0)) {
#line 897 "cl2.f"
	goto L490;
#line 897 "cl2.f"
    }

/*     COMPUTE COVARIANCE MATRIX BASED ON THE ORTHOGONAL DECOMP. */
/*     FROM HFTI( ). */

/*<       KRM1 = KRANK - 1 >*/
#line 902 "cl2.f"
    krm1 = krank - 1;
/*<       KRP1 = KRANK + 1 >*/
#line 903 "cl2.f"
    krp1 = krank + 1;

/*     COPY DIAG. TERMS TO WORKING ARRAY. */
/*<       CALL SCOPY(KRANK, W, MDW+1, WS(N2), 1) >*/
#line 906 "cl2.f"
    i__1 = *mdw + 1;
#line 906 "cl2.f"
    scopy_(&krank, &w[w_offset], &i__1, &ws[n2], &c__1);

/*     RECIPROCATE DIAG. TERMS. */
/*<       DO 280 J=1,KRANK >*/
#line 909 "cl2.f"
    i__1 = krank;
#line 909 "cl2.f"
    for (j = 1; j <= i__1; ++j) {
/*<         W(J,J) = ONE/W(J,J) >*/
#line 910 "cl2.f"
	w[j + j * w_dim1] = one / w[j + j * w_dim1];
/*<   280 CONTINUE >*/
#line 911 "cl2.f"
/* L280: */
#line 911 "cl2.f"
    }
/*<       IF (.NOT.(KRANK.GT.1)) GO TO 310 >*/
#line 912 "cl2.f"
    if (! (krank > 1)) {
#line 912 "cl2.f"
	goto L310;
#line 912 "cl2.f"
    }

/*     INVERT THE UPPER TRIANGULAR QR FACTOR ON ITSELF. */
/*<       DO 300 I=1,KRM1 >*/
#line 915 "cl2.f"
    i__1 = krm1;
#line 915 "cl2.f"
    for (i__ = 1; i__ <= i__1; ++i__) {
/*<         IP1 = I + 1 >*/
#line 916 "cl2.f"
	ip1 = i__ + 1;
/*<         DO 290 J=IP1,KRANK >*/
#line 917 "cl2.f"
	i__2 = krank;
#line 917 "cl2.f"
	for (j = ip1; j <= i__2; ++j) {
/*<           W(I,J) = -SDOT(J-I,W(I,I),MDW,W(I,J),1)*W(J,J) >*/
#line 918 "cl2.f"
	    i__3 = j - i__;
#line 918 "cl2.f"
	    w[i__ + j * w_dim1] = -sdot_(&i__3, &w[i__ + i__ * w_dim1], mdw, &
		    w[i__ + j * w_dim1], &c__1) * w[j + j * w_dim1];
/*<   290   CONTINUE >*/
#line 919 "cl2.f"
/* L290: */
#line 919 "cl2.f"
	}
/*<   300 CONTINUE >*/
#line 920 "cl2.f"
/* L300: */
#line 920 "cl2.f"
    }

/*     COMPUTE THE INVERTED FACTOR TIMES ITS TRANSPOSE. */
/*<   310 DO 330 I=1,KRANK >*/
#line 923 "cl2.f"
L310:
#line 923 "cl2.f"
    i__1 = krank;
#line 923 "cl2.f"
    for (i__ = 1; i__ <= i__1; ++i__) {
/*<         DO 320 J=I,KRANK >*/
#line 924 "cl2.f"
	i__2 = krank;
#line 924 "cl2.f"
	for (j = i__; j <= i__2; ++j) {
/*<           W(I,J) = SDOT(KRANK+1-J,W(I,J),MDW,W(J,J),MDW) >*/
#line 925 "cl2.f"
	    i__3 = krank + 1 - j;
#line 925 "cl2.f"
	    w[i__ + j * w_dim1] = sdot_(&i__3, &w[i__ + j * w_dim1], mdw, &w[
		    j + j * w_dim1], mdw);
/*<   320   CONTINUE >*/
#line 926 "cl2.f"
/* L320: */
#line 926 "cl2.f"
	}
/*<   330 CONTINUE >*/
#line 927 "cl2.f"
/* L330: */
#line 927 "cl2.f"
    }
/*<       IF (.NOT.(KRANK.LT.N)) GO TO 450 >*/
#line 928 "cl2.f"
    if (! (krank < *n)) {
#line 928 "cl2.f"
	goto L450;
#line 928 "cl2.f"
    }

/*     ZERO OUT LOWER TRAPEZOIDAL PART. */
/*     COPY UPPER TRI. TO LOWER TRI. PART. */
/*<       DO 340 J=1,KRANK >*/
#line 932 "cl2.f"
    i__1 = krank;
#line 932 "cl2.f"
    for (j = 1; j <= i__1; ++j) {
/*<         CALL SCOPY(J, W(1,J), 1, W(J,1), MDW) >*/
#line 933 "cl2.f"
	scopy_(&j, &w[j * w_dim1 + 1], &c__1, &w[j + w_dim1], mdw);
/*<   340 CONTINUE >*/
#line 934 "cl2.f"
/* L340: */
#line 934 "cl2.f"
    }
/*<       DO 350 I=KRP1,N >*/
#line 935 "cl2.f"
    i__1 = *n;
#line 935 "cl2.f"
    for (i__ = krp1; i__ <= i__1; ++i__) {
/*<         W(I,1) = ZERO >*/
#line 936 "cl2.f"
	w[i__ + w_dim1] = zero;
/*<         CALL SCOPY(I, W(I,1), 0, W(I,1), MDW) >*/
#line 937 "cl2.f"
	scopy_(&i__, &w[i__ + w_dim1], &c__0, &w[i__ + w_dim1], mdw);
/*<   350 CONTINUE >*/
#line 938 "cl2.f"
/* L350: */
#line 938 "cl2.f"
    }

/*     APPLY RIGHT SIDE TRANSFORMATIONS TO LOWER TRI. */
/*<       N3 = N2 + KRP1 >*/
#line 941 "cl2.f"
    n3 = n2 + krp1;
/*<       DO 430 I=1,KRANK >*/
#line 942 "cl2.f"
    i__1 = krank;
#line 942 "cl2.f"
    for (i__ = 1; i__ <= i__1; ++i__) {
/*<         L = N1 + I >*/
#line 943 "cl2.f"
	l = n1 + i__;
/*<         K = N2 + I >*/
#line 944 "cl2.f"
	k = n2 + i__;
/*<         RB = WS(L-1)*WS(K-1) >*/
#line 945 "cl2.f"
	rb = ws[l - 1] * ws[k - 1];
/*<         IF (.NOT.(RB.LT.ZERO)) GO TO 420 >*/
#line 946 "cl2.f"
	if (! (rb < zero)) {
#line 946 "cl2.f"
	    goto L420;
#line 946 "cl2.f"
	}

/*     IF RB.GE.ZERO, TRANSFORMATION CAN BE REGARDED AS ZERO. */
/*<         RB = ONE/RB >*/
#line 949 "cl2.f"
	rb = one / rb;

/*     STORE UNSCALED RANK-ONE HOUSEHOLDER UPDATE IN WORK ARRAY. */
/*<         WS(N3) = ZERO >*/
#line 952 "cl2.f"
	ws[n3] = zero;
/*<         CALL SCOPY(N, WS(N3), 0, WS(N3), 1) >*/
#line 953 "cl2.f"
	scopy_(n, &ws[n3], &c__0, &ws[n3], &c__1);
/*<         L = N1 + I >*/
#line 954 "cl2.f"
	l = n1 + i__;
/*<         K = N3 + I >*/
#line 955 "cl2.f"
	k = n3 + i__;
/*<         WS(K-1) = WS(L-1) >*/
#line 956 "cl2.f"
	ws[k - 1] = ws[l - 1];
/*<         DO 360 J=KRP1,N >*/
#line 957 "cl2.f"
	i__2 = *n;
#line 957 "cl2.f"
	for (j = krp1; j <= i__2; ++j) {
/*<           K = N3 + J >*/
#line 958 "cl2.f"
	    k = n3 + j;
/*<           WS(K-1) = W(I,J) >*/
#line 959 "cl2.f"
	    ws[k - 1] = w[i__ + j * w_dim1];
/*<   360   CONTINUE >*/
#line 960 "cl2.f"
/* L360: */
#line 960 "cl2.f"
	}
/*<         DO 370 J=1,N >*/
#line 961 "cl2.f"
	i__2 = *n;
#line 961 "cl2.f"
	for (j = 1; j <= i__2; ++j) {
/*<           L = N3 + I >*/
#line 962 "cl2.f"
	    l = n3 + i__;
/*<           K = N3 + J >*/
#line 963 "cl2.f"
	    k = n3 + j;
/*<    >*/
#line 964 "cl2.f"
	    i__3 = j - i__;
#line 964 "cl2.f"
	    i__4 = *n - j + 1;
#line 964 "cl2.f"
	    ws[j] = sdot_(&i__3, &w[j + i__ * w_dim1], mdw, &ws[l - 1], &c__1)
		     + sdot_(&i__4, &w[j + j * w_dim1], &c__1, &ws[k - 1], &
		    c__1);
/*<           WS(J) = WS(J)*RB >*/
#line 966 "cl2.f"
	    ws[j] *= rb;
/*<   370   CONTINUE >*/
#line 967 "cl2.f"
/* L370: */
#line 967 "cl2.f"
	}
/*<         L = N3 + I >*/
#line 968 "cl2.f"
	l = n3 + i__;
/*<         GAM = SDOT(N-I+1,WS(L-1),1,WS(I),1)*RB >*/
#line 969 "cl2.f"
	i__2 = *n - i__ + 1;
#line 969 "cl2.f"
	gam = sdot_(&i__2, &ws[l - 1], &c__1, &ws[i__], &c__1) * rb;
/*<         GAM = GAM*HALF >*/
#line 970 "cl2.f"
	gam *= half;
/*<         CALL SAXPY(N-I+1, GAM, WS(L-1), 1, WS(I), 1) >*/
#line 971 "cl2.f"
	i__2 = *n - i__ + 1;
#line 971 "cl2.f"
	saxpy_(&i__2, &gam, &ws[l - 1], &c__1, &ws[i__], &c__1);
/*<         DO 410 J=I,N >*/
#line 972 "cl2.f"
	i__2 = *n;
#line 972 "cl2.f"
	for (j = i__; j <= i__2; ++j) {
/*<           IF (.NOT.(I.GT.1)) GO TO 390 >*/
#line 973 "cl2.f"
	    if (! (i__ > 1)) {
#line 973 "cl2.f"
		goto L390;
#line 973 "cl2.f"
	    }
/*<           IM1 = I - 1 >*/
#line 974 "cl2.f"
	    im1 = i__ - 1;
/*<           K = N3 + J >*/
#line 975 "cl2.f"
	    k = n3 + j;
/*<           DO 380 L=1,IM1 >*/
#line 976 "cl2.f"
	    i__3 = im1;
#line 976 "cl2.f"
	    for (l = 1; l <= i__3; ++l) {
/*<             W(J,L) = W(J,L) + WS(K-1)*WS(L) >*/
#line 977 "cl2.f"
		w[j + l * w_dim1] += ws[k - 1] * ws[l];
/*<   380     CONTINUE >*/
#line 978 "cl2.f"
/* L380: */
#line 978 "cl2.f"
	    }
/*<   390     K = N3 + J >*/
#line 979 "cl2.f"
L390:
#line 979 "cl2.f"
	    k = n3 + j;
/*<           DO 400 L=I,J >*/
#line 980 "cl2.f"
	    i__3 = j;
#line 980 "cl2.f"
	    for (l = i__; l <= i__3; ++l) {
/*<             IL = N3 + L >*/
#line 981 "cl2.f"
		il = n3 + l;
/*<             W(J,L) = W(J,L) + WS(J)*WS(IL-1) + WS(L)*WS(K-1) >*/
#line 982 "cl2.f"
		w[j + l * w_dim1] = w[j + l * w_dim1] + ws[j] * ws[il - 1] +
			ws[l] * ws[k - 1];
/*<   400     CONTINUE >*/
#line 983 "cl2.f"
/* L400: */
#line 983 "cl2.f"
	    }
/*<   410   CONTINUE >*/
#line 984 "cl2.f"
/* L410: */
#line 984 "cl2.f"
	}
/*<   420   CONTINUE >*/
#line 985 "cl2.f"
L420:
/*<   430 CONTINUE >*/
#line 986 "cl2.f"
/* L430: */
#line 986 "cl2.f"
	;
#line 986 "cl2.f"
    }

/*     COPY LOWER TRI. TO UPPER TRI. TO SYMMETRIZE THE COVARIANCE MATRIX. */
/*<       DO 440 I=1,N >*/
#line 989 "cl2.f"
    i__1 = *n;
#line 989 "cl2.f"
    for (i__ = 1; i__ <= i__1; ++i__) {
/*<         CALL SCOPY(I, W(I,1), MDW, W(1,I), 1) >*/
#line 990 "cl2.f"
	scopy_(&i__, &w[i__ + w_dim1], mdw, &w[i__ * w_dim1 + 1], &c__1);
/*<   440 CONTINUE >*/
#line 991 "cl2.f"
/* L440: */
#line 991 "cl2.f"
    }

/*     REPERMUTE ROWS AND COLS. */
/*<   450 DO 470 II=1,MINMAN >*/
#line 994 "cl2.f"
L450:
#line 994 "cl2.f"
    i__1 = minman;
#line 994 "cl2.f"
    for (ii = 1; ii <= i__1; ++ii) {
/*<         I = MINMAN + 1 - II >*/
#line 995 "cl2.f"
	i__ = minman + 1 - ii;
/*<         K = IP(I) >*/
#line 996 "cl2.f"
	k = ip[i__];
/*<         IF (.NOT.(I.NE.K)) GO TO 460 >*/
#line 997 "cl2.f"
	if (! (i__ != k)) {
#line 997 "cl2.f"
	    goto L460;
#line 997 "cl2.f"
	}
/*<         CALL SSWAP(1, W(I,I), 1, W(K,K), 1) >*/
#line 998 "cl2.f"
	sswap_(&c__1, &w[i__ + i__ * w_dim1], &c__1, &w[k + k * w_dim1], &
		c__1);
/*<         CALL SSWAP(I-1, W(1,I), 1, W(1,K), 1) >*/
#line 999 "cl2.f"
	i__2 = i__ - 1;
#line 999 "cl2.f"
	sswap_(&i__2, &w[i__ * w_dim1 + 1], &c__1, &w[k * w_dim1 + 1], &c__1);
/*<         CALL SSWAP(K-I-1, W(I,I+1), MDW, W(I+1,K), 1) >*/
#line 1000 "cl2.f"
	i__2 = k - i__ - 1;
#line 1000 "cl2.f"
	sswap_(&i__2, &w[i__ + (i__ + 1) * w_dim1], mdw, &w[i__ + 1 + k *
		w_dim1], &c__1);
/*<         CALL SSWAP(N-K, W(I,K+1), MDW, W(K,K+1), MDW) >*/
#line 1001 "cl2.f"
	i__2 = *n - k;
#line 1001 "cl2.f"
	sswap_(&i__2, &w[i__ + (k + 1) * w_dim1], mdw, &w[k + (k + 1) *
		w_dim1], mdw);
/*<   460   CONTINUE >*/
#line 1002 "cl2.f"
L460:
/*<   470 CONTINUE >*/
#line 1003 "cl2.f"
/* L470: */
#line 1003 "cl2.f"
	;
#line 1003 "cl2.f"
    }

/*     PUT IN NORMALIZED RESIDUAL SUM OF SQUARES SCALE FACTOR */
/*     AND SYMMETRIZE THE RESULTING COVARIANCE MARIX. */
/*<       DO 480 J=1,N >*/
#line 1007 "cl2.f"
    i__1 = *n;
#line 1007 "cl2.f"
    for (j = 1; j <= i__1; ++j) {
/*<         CALL SSCAL(J, FAC, W(1,J), 1) >*/
#line 1008 "cl2.f"
	sscal_(&j, &fac, &w[j * w_dim1 + 1], &c__1);
/*<         CALL SCOPY(J, W(1,J), 1, W(J,1), MDW) >*/
#line 1009 "cl2.f"
	scopy_(&j, &w[j * w_dim1 + 1], &c__1, &w[j + w_dim1], mdw);
/*<   480 CONTINUE >*/
#line 1010 "cl2.f"
/* L480: */
#line 1010 "cl2.f"
    }
/*<   490 GO TO 540 >*/
#line 1011 "cl2.f"
L490:
#line 1011 "cl2.f"
    goto L540;
/*<   500 CONTINUE >*/
#line 1012 "cl2.f"
L500:

/*     TO PROCESS-OPTION-VECTOR */

/*     THE NOMINAL TOLERANCE USED IN THE CODE, */
/*<       TOL = SQRT(SRELPR) >*/
#line 1017 "cl2.f"
    tol = sqrt(srelpr);
/*<       COV = .FALSE. >*/
#line 1018 "cl2.f"
    cov = FALSE_;
/*<       LAST = 1 >*/
#line 1019 "cl2.f"
    last = 1;
/*<       LINK = PRGOPT(1) >*/
#line 1020 "cl2.f"
    link = prgopt[1];
/*<   510 IF (.NOT.(LINK.GT.1)) GO TO 520 >*/
#line 1021 "cl2.f"
L510:
#line 1021 "cl2.f"
    if (! (link > 1)) {
#line 1021 "cl2.f"
	goto L520;
#line 1021 "cl2.f"
    }
/*<       KEY = PRGOPT(LAST+1) >*/
#line 1022 "cl2.f"
    key = prgopt[last + 1];
/*<       IF (KEY.EQ.1) COV = PRGOPT(LAST+2).NE.ZERO >*/
#line 1023 "cl2.f"
    if (key == 1) {
#line 1023 "cl2.f"
	cov = prgopt[last + 2] != zero;
#line 1023 "cl2.f"
    }
/*<       IF (KEY.EQ.5) TOL = AMAX1(SRELPR,PRGOPT(LAST+2)) >*/
#line 1024 "cl2.f"
    if (key == 5) {
/* Computing MAX */
#line 1024 "cl2.f"
	r__1 = srelpr, r__2 = prgopt[last + 2];
#line 1024 "cl2.f"
	tol = max(r__1,r__2);
#line 1024 "cl2.f"
    }
/*<       NEXT = PRGOPT(LINK) >*/
#line 1025 "cl2.f"
    next = prgopt[link];
/*<       LAST = LINK >*/
#line 1026 "cl2.f"
    last = link;
/*<       LINK = NEXT >*/
#line 1027 "cl2.f"
    link = next;
/*<       GO TO 510 >*/
#line 1028 "cl2.f"
    goto L510;
/*<   520 GO TO 530 >*/
#line 1029 "cl2.f"
L520:
#line 1029 "cl2.f"
    goto L530;
/*<   530 GO TO IGO994, (40) >*/
#line 1030 "cl2.f"
L530:
#line 1030 "cl2.f"
    switch (igo994) {
#line 1030 "cl2.f"
	case 0: goto L40;
#line 1030 "cl2.f"
    }
/*<   540 GO TO IGO990, (60) >*/
#line 1031 "cl2.f"
L540:
#line 1031 "cl2.f"
    switch (igo990) {
#line 1031 "cl2.f"
	case 0: goto L60;
#line 1031 "cl2.f"
    }
   return 0 ;
/*<       END >*/
} /* lsi_ */

/* CC */
/* CC---------------------------------------------------------------------------- */
/* CC */
/*<       SUBROUTINE LPDP(A, MDA, M, N1, N2, PRGOPT, X, WNORM, MODE, WS, IS) >*/
static /* Subroutine */ int lpdp_(real *a, integer *mda, integer *m, integer *n1,
	integer *n2, real *prgopt, real *x, real *wnorm, integer *mode, real *
	ws, integer *is)
{
    /* Initialized data */

    static real zero = 0.f;
    static real one = 1.f;
    static real fac = .1f;

    /* System generated locals */
    integer a_dim1, a_offset, i__1, i__2;

    /* Local variables */
    static integer i__, j, l, n;
    static real sc;
    static integer iw, ix, np1;
    static integer modew;
    static real rnorm;
    static real ynorm;


/*     THE EDITING REQUIRED TO CONVERT THIS SUBROUTINE FROM SINGLE TO */
/*     DOUBLE PRECISION INVOLVES THE FOLLOWING CHARACTER STRING CHANGES. */
/*     USE AN EDITING COMMAND (CHANGE) /STRING-1/(TO)STRING-2/. */
/*     (START EDITING AT LINE WITH C++ IN COLS. 1-3.) */
/*     /REAL (12 BLANKS)/DOUBLE PRECISION/,/SNRM2/DNRM2/,/SDOT/DDOT/, */
/*     /SCOPY/DCOPY/,/SSCAL/DSCAL/,/ABS(/DABS(/, ABS/, DABS/,/E0/D0/ */

/*     DIMENSION A(MDA,N+1),PRGOPT(*),X(N),WS((M+2)*(N+7)),IS(M+N+1), */
/*     WHERE N=N1+N2.  THIS IS A SLIGHT OVERESTIMATE FOR WS(*). */

/*     WRITTEN BY R. J. HANSON AND K. H. HASKELL, SANDIA LABS */
/*     REVISED OCT. 1, 1981. */

/*     DETERMINE AN N1-VECTOR W, AND */
/*               AN N2-VECTOR Z */
/*     WHICH MINIMIZES THE EUCLIDEAN LENGTH OF W */
/*     SUBJECT TO G*W+H*Z .GE. Y. */
/*     THIS IS THE LEAST PROJECTED DISTANCE PROBLEM, LPDP. */
/*     THE MATRICES G AND H ARE OF RESPECTIVE */
/*     DIMENSIONS M BY N1 AND M BY N2. */

/*     CALLED BY SUBPROGRAM LSI( ). */

/*     THE MATRIX */
/*                (G H Y) */

/*     OCCUPIES ROWS 1,...,M AND COLS 1,...,N1+N2+1 OF A(*,*). */

/*     THE SOLUTION (W) IS RETURNED IN X(*). */
/*                  (Z) */

/*     THE VALUE OF MODE INDICATES THE STATUS OF */
/*     THE COMPUTATION AFTER RETURNING TO THE USER. */

/*          MODE=1  THE SOLUTION WAS SUCCESSFULLY OBTAINED. */

/*          MODE=2  THE INEQUALITIES ARE INCONSISTENT. */

/*     SUBROUTINES CALLED */

/*     WNNLS         SOLVES A NONNEGATIVELY CONSTRAINED LINEAR LEAST */
/*                   SQUARES PROBLEM WITH LINEAR EQUALITY CONSTRAINTS. */
/*                   PART OF THIS PACKAGE. */

/* ++ */
/*     SDOT,         SUBROUTINES FROM THE BLAS PACKAGE. */
/*     SSCAL,SNRM2,  SEE TRANS. MATH. SOFT., VOL. 5, NO. 3, P. 308. */
/*     SCOPY */

/*<       REAL             A(MDA,1), PRGOPT(1), WS(1), WNORM, X(1) >*/
/*<       INTEGER IS(1) >*/
/*<       REAL             FAC, ONE, RNORM, SC, YNORM, ZERO >*/
/*<       REAL             SDOT, SNRM2, ABS >*/
/*<       DATA ZERO, ONE /0.E0,1.E0/, FAC /0.1E0/ >*/
#line 1091 "cl2.f"
    /* Parameter adjustments */
#line 1091 "cl2.f"
    a_dim1 = *mda;
#line 1091 "cl2.f"
    a_offset = 1 + a_dim1;
#line 1091 "cl2.f"
    a -= a_offset;
#line 1091 "cl2.f"
    --prgopt;
#line 1091 "cl2.f"
    --x;
#line 1091 "cl2.f"
    --ws;
#line 1091 "cl2.f"
    --is;
#line 1091 "cl2.f"

#line 1091 "cl2.f"
    /* Function Body */
/*<       N = N1 + N2 >*/
#line 1092 "cl2.f"
    n = *n1 + *n2;
/*<       MODE = 1 >*/
#line 1093 "cl2.f"
    *mode = 1;
/*<       IF (.NOT.(M.LE.0)) GO TO 20 >*/
#line 1094 "cl2.f"
    if (! (*m <= 0)) {
#line 1094 "cl2.f"
	goto L20;
#line 1094 "cl2.f"
    }
/*<       IF (.NOT.(N.GT.0)) GO TO 10 >*/
#line 1095 "cl2.f"
    if (! (n > 0)) {
#line 1095 "cl2.f"
	goto L10;
#line 1095 "cl2.f"
    }
/*<       X(1) = ZERO >*/
#line 1096 "cl2.f"
    x[1] = zero;
/*<       CALL SCOPY(N, X, 0, X, 1) >*/
#line 1097 "cl2.f"
    scopy_(&n, &x[1], &c__0, &x[1], &c__1);
/*<    10 WNORM = ZERO >*/
#line 1098 "cl2.f"
L10:
#line 1098 "cl2.f"
    *wnorm = zero;
/*<       RETURN >*/
#line 1099 "cl2.f"
    return 0;
/*<    20 NP1 = N + 1 >*/
#line 1100 "cl2.f"
L20:
#line 1100 "cl2.f"
    np1 = n + 1;

/*     SCALE NONZERO ROWS OF INEQUALITY MATRIX TO HAVE LENGTH ONE. */
/*<       DO 40 I=1,M >*/
#line 1103 "cl2.f"
    i__1 = *m;
#line 1103 "cl2.f"
    for (i__ = 1; i__ <= i__1; ++i__) {
/*<         SC = SNRM2(N,A(I,1),MDA) >*/
#line 1104 "cl2.f"
	sc = snrm2_(&n, &a[i__ + a_dim1], mda);
/*<         IF (.NOT.(SC.NE.ZERO)) GO TO 30 >*/
#line 1105 "cl2.f"
	if (! (sc != zero)) {
#line 1105 "cl2.f"
	    goto L30;
#line 1105 "cl2.f"
	}
/*<         SC = ONE/SC >*/
#line 1106 "cl2.f"
	sc = one / sc;
/*<         CALL SSCAL(NP1, SC, A(I,1), MDA) >*/
#line 1107 "cl2.f"
	sscal_(&np1, &sc, &a[i__ + a_dim1], mda);
/*<    30   CONTINUE >*/
#line 1108 "cl2.f"
L30:
/*<    40 CONTINUE >*/
#line 1109 "cl2.f"
/* L40: */
#line 1109 "cl2.f"
	;
#line 1109 "cl2.f"
    }

/*     SCALE RT.-SIDE VECTOR TO HAVE LENGTH ONE (OR ZERO). */
/*<       YNORM = SNRM2(M,A(1,NP1),1) >*/
#line 1112 "cl2.f"
    ynorm = snrm2_(m, &a[np1 * a_dim1 + 1], &c__1);
/*<       IF (.NOT.(YNORM.NE.ZERO)) GO TO 50 >*/
#line 1113 "cl2.f"
    if (! (ynorm != zero)) {
#line 1113 "cl2.f"
	goto L50;
#line 1113 "cl2.f"
    }
/*<       SC = ONE/YNORM >*/
#line 1114 "cl2.f"
    sc = one / ynorm;
/*<       CALL SSCAL(M, SC, A(1,NP1), 1) >*/
#line 1115 "cl2.f"
    sscal_(m, &sc, &a[np1 * a_dim1 + 1], &c__1);

/*     SCALE COLS OF MATRIX H. */
/*<    50 J = N1 + 1 >*/
#line 1118 "cl2.f"
L50:
#line 1118 "cl2.f"
    j = *n1 + 1;
/*<    60 IF (.NOT.(J.LE.N)) GO TO 70 >*/
#line 1119 "cl2.f"
L60:
#line 1119 "cl2.f"
    if (! (j <= n)) {
#line 1119 "cl2.f"
	goto L70;
#line 1119 "cl2.f"
    }
/*<       SC = SNRM2(M,A(1,J),1) >*/
#line 1120 "cl2.f"
    sc = snrm2_(m, &a[j * a_dim1 + 1], &c__1);
/*<       IF (SC.NE.ZERO) SC = ONE/SC >*/
#line 1121 "cl2.f"
    if (sc != zero) {
#line 1121 "cl2.f"
	sc = one / sc;
#line 1121 "cl2.f"
    }
/*<       CALL SSCAL(M, SC, A(1,J), 1) >*/
#line 1122 "cl2.f"
    sscal_(m, &sc, &a[j * a_dim1 + 1], &c__1);
/*<       X(J) = SC >*/
#line 1123 "cl2.f"
    x[j] = sc;
/*<       J = J + 1 >*/
#line 1124 "cl2.f"
    ++j;
/*<       GO TO 60 >*/
#line 1125 "cl2.f"
    goto L60;
/*<    70 IF (.NOT.(N1.GT.0)) GO TO 130 >*/
#line 1126 "cl2.f"
L70:
#line 1126 "cl2.f"
    if (! (*n1 > 0)) {
#line 1126 "cl2.f"
	goto L130;
#line 1126 "cl2.f"
    }

/*     COPY TRANSPOSE OF (H G Y) TO WORK ARRAY WS(*). */
/*<       IW = 0 >*/
#line 1129 "cl2.f"
    iw = 0;
/*<       DO 80 I=1,M >*/
#line 1130 "cl2.f"
    i__1 = *m;
#line 1130 "cl2.f"
    for (i__ = 1; i__ <= i__1; ++i__) {

/*     MOVE COL OF TRANSPOSE OF H INTO WORK ARRAY. */
/*<         CALL SCOPY(N2, A(I,N1+1), MDA, WS(IW+1), 1) >*/
#line 1133 "cl2.f"
	scopy_(n2, &a[i__ + (*n1 + 1) * a_dim1], mda, &ws[iw + 1], &c__1);
/*<         IW = IW + N2 >*/
#line 1134 "cl2.f"
	iw += *n2;

/*     MOVE COL OF TRANSPOSE OF G INTO WORK ARRAY. */
/*<         CALL SCOPY(N1, A(I,1), MDA, WS(IW+1), 1) >*/
#line 1137 "cl2.f"
	scopy_(n1, &a[i__ + a_dim1], mda, &ws[iw + 1], &c__1);
/*<         IW = IW + N1 >*/
#line 1138 "cl2.f"
	iw += *n1;

/*     MOVE COMPONENT OF VECTOR Y INTO WORK ARRAY. */
/*<         WS(IW+1) = A(I,NP1) >*/
#line 1141 "cl2.f"
	ws[iw + 1] = a[i__ + np1 * a_dim1];
/*<         IW = IW + 1 >*/
#line 1142 "cl2.f"
	++iw;
/*<    80 CONTINUE >*/
#line 1143 "cl2.f"
/* L80: */
#line 1143 "cl2.f"
    }
/*<       WS(IW+1) = ZERO >*/
#line 1144 "cl2.f"
    ws[iw + 1] = zero;
/*<       CALL SCOPY(N, WS(IW+1), 0, WS(IW+1), 1) >*/
#line 1145 "cl2.f"
    scopy_(&n, &ws[iw + 1], &c__0, &ws[iw + 1], &c__1);
/*<       IW = IW + N >*/
#line 1146 "cl2.f"
    iw += n;
/*<       WS(IW+1) = ONE >*/
#line 1147 "cl2.f"
    ws[iw + 1] = one;
/*<       IW = IW + 1 >*/
#line 1148 "cl2.f"
    ++iw;

/*     SOLVE EU=F SUBJECT TO (TRANSPOSE OF H)U=0, U.GE.0.  THE */
/*     MATRIX E = TRANSPOSE OF (G Y), AND THE (N+1)-VECTOR */
/*     F = TRANSPOSE OF (0,...,0,1). */
/*<       IX = IW + 1 >*/
#line 1153 "cl2.f"
    ix = iw + 1;
/*<       IW = IW + M >*/
#line 1154 "cl2.f"
    iw += *m;

/*     DO NOT CHECK LENGTHS OF WORK ARRAYS IN THIS USAGE OF WNNLS( ). */
/*<       IS(1) = 0 >*/
#line 1157 "cl2.f"
    is[1] = 0;
/*<       IS(2) = 0 >*/
#line 1158 "cl2.f"
    is[2] = 0;
/*<    >*/
#line 1159 "cl2.f"
    i__1 = np1 - *n2;
#line 1159 "cl2.f"
    wnnls_fort(&ws[1], &np1, n2, &i__1, m, &c__0, &prgopt[1], &ws[ix], &rnorm, &
	    modew, &is[1], &ws[iw + 1]);

/*     COMPUTE THE COMPONENTS OF THE SOLN DENOTED ABOVE BY W. */
/*<       SC = ONE - SDOT(M,A(1,NP1),1,WS(IX),1) >*/
#line 1163 "cl2.f"
    sc = one - sdot_(m, &a[np1 * a_dim1 + 1], &c__1, &ws[ix], &c__1);
/*<       IF (.NOT.(ONE+FAC*ABS(SC).NE.ONE .AND. RNORM.GT.ZERO)) GO TO 110 >*/
#line 1164 "cl2.f"
    if (! (one + fac * abs(sc) != one && rnorm > zero)) {
#line 1164 "cl2.f"
	goto L110;
#line 1164 "cl2.f"
    }
/*<       SC = ONE/SC >*/
#line 1165 "cl2.f"
    sc = one / sc;
/*<       DO 90 J=1,N1 >*/
#line 1166 "cl2.f"
    i__1 = *n1;
#line 1166 "cl2.f"
    for (j = 1; j <= i__1; ++j) {
/*<         X(J) = SC*SDOT(M,A(1,J),1,WS(IX),1) >*/
#line 1167 "cl2.f"
	x[j] = sc * sdot_(m, &a[j * a_dim1 + 1], &c__1, &ws[ix], &c__1);
/*<    90 CONTINUE >*/
#line 1168 "cl2.f"
/* L90: */
#line 1168 "cl2.f"
    }

/*     COMPUTE THE VECTOR Q=Y-GW.  OVERWRITE Y WITH THIS VECTOR. */
/*<       DO 100 I=1,M >*/
#line 1171 "cl2.f"
    i__1 = *m;
#line 1171 "cl2.f"
    for (i__ = 1; i__ <= i__1; ++i__) {
/*<         A(I,NP1) = A(I,NP1) - SDOT(N1,A(I,1),MDA,X,1) >*/
#line 1172 "cl2.f"
	a[i__ + np1 * a_dim1] -= sdot_(n1, &a[i__ + a_dim1], mda, &x[1], &
		c__1);
/*<   100 CONTINUE >*/
#line 1173 "cl2.f"
/* L100: */
#line 1173 "cl2.f"
    }
/*<       GO TO 120 >*/
#line 1174 "cl2.f"
    goto L120;
/*<   110 MODE = 2 >*/
#line 1175 "cl2.f"
L110:
#line 1175 "cl2.f"
    *mode = 2;
/*<       RETURN >*/
#line 1176 "cl2.f"
    return 0;
/*<   120 CONTINUE >*/
#line 1177 "cl2.f"
L120:
/*<   130 IF (.NOT.(N2.GT.0)) GO TO 180 >*/
#line 1178 "cl2.f"
L130:
#line 1178 "cl2.f"
    if (! (*n2 > 0)) {
#line 1178 "cl2.f"
	goto L180;
#line 1178 "cl2.f"
    }

/*     COPY TRANSPOSE OF (H Q) TO WORK ARRAY WS(*). */
/*<       IW = 0 >*/
#line 1181 "cl2.f"
    iw = 0;
/*<       DO 140 I=1,M >*/
#line 1182 "cl2.f"
    i__1 = *m;
#line 1182 "cl2.f"
    for (i__ = 1; i__ <= i__1; ++i__) {
/*<         CALL SCOPY(N2, A(I,N1+1), MDA, WS(IW+1), 1) >*/
#line 1183 "cl2.f"
	scopy_(n2, &a[i__ + (*n1 + 1) * a_dim1], mda, &ws[iw + 1], &c__1);
/*<         IW = IW + N2 >*/
#line 1184 "cl2.f"
	iw += *n2;
/*<         WS(IW+1) = A(I,NP1) >*/
#line 1185 "cl2.f"
	ws[iw + 1] = a[i__ + np1 * a_dim1];
/*<         IW = IW + 1 >*/
#line 1186 "cl2.f"
	++iw;
/*<   140 CONTINUE >*/
#line 1187 "cl2.f"
/* L140: */
#line 1187 "cl2.f"
    }
/*<       WS(IW+1) = ZERO >*/
#line 1188 "cl2.f"
    ws[iw + 1] = zero;
/*<       CALL SCOPY(N2, WS(IW+1), 0, WS(IW+1), 1) >*/
#line 1189 "cl2.f"
    scopy_(n2, &ws[iw + 1], &c__0, &ws[iw + 1], &c__1);
/*<       IW = IW + N2 >*/
#line 1190 "cl2.f"
    iw += *n2;
/*<       WS(IW+1) = ONE >*/
#line 1191 "cl2.f"
    ws[iw + 1] = one;
/*<       IW = IW + 1 >*/
#line 1192 "cl2.f"
    ++iw;
/*<       IX = IW + 1 >*/
#line 1193 "cl2.f"
    ix = iw + 1;
/*<       IW = IW + M >*/
#line 1194 "cl2.f"
    iw += *m;

/*     SOLVE RV=S SUBJECT TO V.GE.0.  THE MATRIX R =(TRANSPOSE */
/*     OF (H Q)), WHERE Q=Y-GW.  THE (N2+1)-VECTOR S =(TRANSPOSE */
/*     OF (0,...,0,1)). */

/*     DO NOT CHECK LENGTHS OF WORK ARRAYS IN THIS USAGE OF WNNLS( ). */
/*<       IS(1) = 0 >*/
#line 1201 "cl2.f"
    is[1] = 0;
/*<       IS(2) = 0 >*/
#line 1202 "cl2.f"
    is[2] = 0;
/*<    >*/
#line 1203 "cl2.f"
    i__1 = *n2 + 1;
#line 1203 "cl2.f"
    i__2 = *n2 + 1;
#line 1203 "cl2.f"
    wnnls_fort(&ws[1], &i__1, &c__0, &i__2, m, &c__0, &prgopt[1], &ws[ix], &rnorm,
	     &modew, &is[1], &ws[iw + 1]);

/*     COMPUTE THE COMPONENTS OF THE SOLN DENOTED ABOVE BY Z. */
/*<       SC = ONE - SDOT(M,A(1,NP1),1,WS(IX),1) >*/
#line 1207 "cl2.f"
    sc = one - sdot_(m, &a[np1 * a_dim1 + 1], &c__1, &ws[ix], &c__1);
/*<       IF (.NOT.(ONE+FAC*ABS(SC).NE.ONE .AND. RNORM.GT.ZERO)) GO TO 160 >*/
#line 1208 "cl2.f"
    if (! (one + fac * abs(sc) != one && rnorm > zero)) {
#line 1208 "cl2.f"
	goto L160;
#line 1208 "cl2.f"
    }
/*<       SC = ONE/SC >*/
#line 1209 "cl2.f"
    sc = one / sc;
/*<       DO 150 J=1,N2 >*/
#line 1210 "cl2.f"
    i__1 = *n2;
#line 1210 "cl2.f"
    for (j = 1; j <= i__1; ++j) {
/*<         L = N1 + J >*/
#line 1211 "cl2.f"
	l = *n1 + j;
/*<         X(L) = SC*SDOT(M,A(1,L),1,WS(IX),1)*X(L) >*/
#line 1212 "cl2.f"
	x[l] = sc * sdot_(m, &a[l * a_dim1 + 1], &c__1, &ws[ix], &c__1) * x[l]
		;
/*<   150 CONTINUE >*/
#line 1213 "cl2.f"
/* L150: */
#line 1213 "cl2.f"
    }
/*<       GO TO 170 >*/
#line 1214 "cl2.f"
    goto L170;
/*<   160 MODE = 2 >*/
#line 1215 "cl2.f"
L160:
#line 1215 "cl2.f"
    *mode = 2;
/*<       RETURN >*/
#line 1216 "cl2.f"
    return 0;
/*<   170 CONTINUE >*/
#line 1217 "cl2.f"
L170:

/*     ACCOUNT FOR SCALING OF RT.-SIDE VECTOR IN SOLUTION. */
/*<   180 CALL SSCAL(N, YNORM, X, 1) >*/
#line 1220 "cl2.f"
L180:
#line 1220 "cl2.f"
    sscal_(&n, &ynorm, &x[1], &c__1);
/*<       WNORM = SNRM2(N1,X,1) >*/
#line 1221 "cl2.f"
    *wnorm = snrm2_(n1, &x[1], &c__1);
/*<       RETURN >*/
#line 1222 "cl2.f"
    return 0;
/*<       END >*/
} /* lpdp_ */

/* CC */
/* CC---------------------------------------------------------------------------- */
/* CC */
/*<    >*/
static /* Subroutine */ int wnnls_fort(real *w, integer *mdw, integer *me, integer *ma,
	integer *n, integer *l, real *prgopt, real *x, real *rnorm, integer *
	mode, integer *iwork, real *work)
{
    /* System generated locals */
    integer w_dim1, w_offset;

    /* Local variables */
    static integer l1, l2, l3, l4, l5, lw, liw, nerr, iopt;


/*     DIMENSION W(MDW,N+1),PRGOPT(*),X(N),IWORK(M+N),WORK(M+5*N) */

/*     ABSTRACT */

/*     THIS SUBPROGRAM SOLVES A LINEARLY CONSTRAINED LEAST SQUARES */
/*     PROBLEM.  SUPPOSE THERE ARE GIVEN MATRICES E AND A OF */
/*     RESPECTIVE DIMENSIONS ME BY N AND MA BY N, AND VECTORS F */
/*     AND B OF RESPECTIVE LENGTHS ME AND MA.  THIS SUBROUTINE */
/*     SOLVES THE PROBLEM */

/*               EX = F, (EQUATIONS TO BE EXACTLY SATISFIED) */

/*               AX = B, (EQUATIONS TO BE APPROXIMATELY SATISFIED, */
/*                        IN THE LEAST SQUARES SENSE) */

/*               SUBJECT TO COMPONENTS L+1,...,N NONNEGATIVE */

/*     ANY VALUES ME.GE.0, MA.GE.0 AND 0.LE. L .LE.N ARE PERMITTED. */

/*     THE PROBLEM IS REPOSED AS PROBLEM WNNLS */

/*               (WT*E)X = (WT*F) */
/*               (   A)    (   B), (LEAST SQUARES) */
/*               SUBJECT TO COMPONENTS L+1,...,N NONNEGATIVE. */

/*     THE SUBPROGRAM CHOOSES THE HEAVY WEIGHT (OR PENALTY PARAMETER) WT. */

/*     THE PARAMETERS FOR WNNLS ARE */

/*     INPUT.. */

/*     W(*,*),MDW,  THE ARRAY W(*,*) IS DOUBLE SUBSCRIPTED WITH FIRST */
/*     ME,MA,N,L    DIMENSIONING PARAMETER EQUAL TO MDW.  FOR THIS */
/*                  DISCUSSION LET US CALL M = ME + MA.  THEN MDW */
/*                  MUST SATISFY MDW.GE.M.  THE CONDITION MDW.LT.M */
/*                  IS AN ERROR. */

/*                  THE ARRAY W(*,*) CONTAINS THE MATRICES AND VECTORS */

/*                       (E  F) */
/*                       (A  B) */

/*                  IN ROWS AND COLUMNS 1,...,M AND 1,...,N+1 */
/*                  RESPECTIVELY.  COLUMNS 1,...,L CORRESPOND TO */
/*                  UNCONSTRAINED VARIABLES X(1),...,X(L).  THE */
/*                  REMAINING VARIABLES ARE CONSTRAINED TO BE */
/*                  NONNEGATIVE.  THE CONDITION L.LT.0 .OR. L.GT.N IS */
/*                  AN ERROR. */

/*     PRGOPT(*)    THIS ARRAY IS THE OPTION VECTOR. */
/*                  IF THE USER IS SATISFIED WITH THE NOMINAL */
/*                  SUBPROGRAM FEATURES SET */

/*                  PRGOPT(1)=1 (OR PRGOPT(1)=1.0) */

/*                  OTHERWISE PRGOPT(*) IS A LINKED LIST CONSISTING OF */
/*                  GROUPS OF DATA OF THE FOLLOWING FORM */

/*                  LINK */
/*                  KEY */
/*                  DATA SET */

/*                  THE PARAMETERS LINK AND KEY ARE EACH ONE WORD. */
/*                  THE DATA SET CAN BE COMPRISED OF SEVERAL WORDS. */
/*                  THE NUMBER OF ITEMS DEPENDS ON THE VALUE OF KEY. */
/*                  THE VALUE OF LINK POINTS TO THE FIRST */
/*                  ENTRY OF THE NEXT GROUP OF DATA WITHIN */
/*                  PRGOPT(*).  THE EXCEPTION IS WHEN THERE ARE */
/*                  NO MORE OPTIONS TO CHANGE.  IN THAT */
/*                  CASE LINK=1 AND THE VALUES KEY AND DATA SET */
/*                  ARE NOT REFERENCED. THE GENERAL LAYOUT OF */
/*                  PRGOPT(*) IS AS FOLLOWS. */

/*               ...PRGOPT(1)=LINK1 (LINK TO FIRST ENTRY OF NEXT GROUP) */
/*               .  PRGOPT(2)=KEY1 (KEY TO THE OPTION CHANGE) */
/*               .  PRGOPT(3)=DATA VALUE (DATA VALUE FOR THIS CHANGE) */
/*               .       . */
/*               .       . */
/*               .       . */
/*               ...PRGOPT(LINK1)=LINK2 (LINK TO THE FIRST ENTRY OF */
/*               .                       NEXT GROUP) */
/*               .  PRGOPT(LINK1+1)=KEY2 (KEY TO THE OPTION CHANGE) */
/*               .  PRGOPT(LINK1+2)=DATA VALUE */
/*               ...     . */
/*               .       . */
/*               .       . */
/*               ...PRGOPT(LINK)=1 (NO MORE OPTIONS TO CHANGE) */

/*                  VALUES OF LINK THAT ARE NONPOSITIVE ARE ERRORS. */
/*                  A VALUE OF LINK.GT.NLINK=100000 IS ALSO AN ERROR. */
/*                  THIS HELPS PREVENT USING INVALID BUT POSITIVE */
/*                  VALUES OF LINK THAT WILL PROBABLY EXTEND */
/*                  BEYOND THE PROGRAM LIMITS OF PRGOPT(*). */
/*                  UNRECOGNIZED VALUES OF KEY ARE IGNORED.  THE */
/*                  ORDER OF THE OPTIONS IS ARBITRARY AND ANY NUMBER */
/*                  OF OPTIONS CAN BE CHANGED WITH THE FOLLOWING */
/*                  RESTRICTION.  TO PREVENT CYCLING IN THE */
/*                  PROCESSING OF THE OPTION ARRAY A COUNT OF THE */
/*                  NUMBER OF OPTIONS CHANGED IS MAINTAINED. */
/*                  WHENEVER THIS COUNT EXCEEDS NOPT=1000 AN ERROR */
/*                  MESSAGE IS PRINTED AND THE SUBPROGRAM RETURNS. */

/*                  OPTIONS.. */

/*                  KEY=6 */
/*                         SCALE THE NONZERO COLUMNS OF THE */
/*                  ENTIRE DATA MATRIX */
/*                  (E) */
/*                  (A) */
/*                  TO HAVE LENGTH ONE.  THE DATA SET FOR */
/*                  THIS OPTION IS A SINGLE VALUE.  IT MUST */
/*                  BE NONZERO IF UNIT LENGTH COLUMN SCALING IS */
/*                  DESIRED. */

/*                  KEY=7 */
/*                         SCALE COLUMNS OF THE ENTIRE DATA MATRIX */
/*                  (E) */
/*                  (A) */
/*                  WITH A USER-PROVIDED DIAGONAL MATRIX. */
/*                  THE DATA SET FOR THIS OPTION CONSISTS */
/*                  OF THE N DIAGONAL SCALING FACTORS, ONE FOR */
/*                  EACH MATRIX COLUMN. */

/*                  KEY=8 */
/*                         CHANGE THE RANK DETERMINATION TOLERANCE FROM */
/*                  THE NOMINAL VALUE OF SQRT(EPS).  THIS QUANTITY CAN */
/*                  BE NO SMALLER THAN EPS, THE ARITHMETIC- */
/*                  STORAGE PRECISION.  THE QUANTITY USED */
/*                  HERE IS INTERNALLY RESTRICTED TO BE AT */
/*                  LEAST EPS.  THE DATA SET FOR THIS OPTION */
/*                  IS THE NEW TOLERANCE. */

/*                  KEY=9 */
/*                         CHANGE THE BLOW-UP PARAMETER FROM THE */
/*                  NOMINAL VALUE OF SQRT(EPS).  THE RECIPROCAL OF */
/*                  THIS PARAMETER IS USED IN REJECTING SOLUTION */
/*                  COMPONENTS AS TOO LARGE WHEN A VARIABLE IS */
/*                  FIRST BROUGHT INTO THE ACTIVE SET.  TOO LARGE */
/*                  MEANS THAT THE PROPOSED COMPONENT TIMES THE */
/*                  RECIPROCAL OF THE PARAMETERIS NOT LESS THAN */
/*                  THE RATIO OF THE NORMS OF THE RIGHT-SIDE */
/*                  VECTOR AND THE DATA MATRIX. */
/*                  THIS PARAMETER CAN BE NO SMALLER THAN EPS, */
/*                  THE ARITHMETIC-STORAGE PRECISION. */

/*                  FOR EXAMPLE, SUPPOSE WE WANT TO PROVIDE */
/*                  A DIAGONAL MATRIX TO SCALE THE PROBLEM */
/*                  MATRIX AND CHANGE THE TOLERANCE USED FOR */
/*                  DETERMINING LINEAR DEPENDENCE OF DROPPED COL */
/*                  VECTORS.  FOR THESE OPTIONS THE DIMENSIONS OF */
/*                  PRGOPT(*) MUST BE AT LEAST N+6.  THE FORTRAN */
/*                  STATEMENTS DEFINING THESE OPTIONS WOULD */
/*                  BE AS FOLLOWS. */

/*                  PRGOPT(1)=N+3 (LINK TO ENTRY N+3 IN PRGOPT(*)) */
/*                  PRGOPT(2)=7 (USER-PROVIDED SCALING KEY) */

/*                  CALL SCOPY(N,D,1,PRGOPT(3),1) (COPY THE N */
/*                  SCALING FACTORS FROM A USER ARRAY CALLED D(*) */
/*                  INTO PRGOPT(3)-PRGOPT(N+2)) */

/*                  PRGOPT(N+3)=N+6 (LINK TO ENTRY N+6 OF PRGOPT(*)) */
/*                  PRGOPT(N+4)=8 (LINEAR DEPENDENCE TOLERANCE KEY) */
/*                  PRGOPT(N+5)=... (NEW VALUE OF THE TOLERANCE) */

/*                  PRGOPT(N+6)=1 (NO MORE OPTIONS TO CHANGE) */

/*     IWORK(1),    THE AMOUNTS OF WORKING STORAGE ACTUALLY ALLOCATED */
/*     IWORK(2)     FOR THE WORKING ARRAYS WORK(*) AND IWORK(*), */
/*                  RESPECTIVELY.  THESE QUANTITIES ARE COMPARED WITH */
/*                  THE ACTUAL AMOUNTS OF STORAGE NEEDED FOR WNNLS( ). */
/*                  INSUFFICIENT STORAGE ALLOCATED FOR EITHER WORK(*) */
/*                  OR IWORK(*) IS CONSIDERED AN ERROR.  THIS FEATURE */
/*                  WAS INCLUDED IN WNNLS( ) BECAUSE MISCALCULATING */
/*                  THE STORAGE FORMULAS FOR WORK(*) AND IWORK(*) */
/*                  MIGHT VERY WELL LEAD TO SUBTLE AND HARD-TO-FIND */
/*                  EXECUTION ERRORS. */

/*                  THE LENGTH OF WORK(*) MUST BE AT LEAST */

/*                  LW = ME+MA+5*N */
/*                  THIS TEST WILL NOT BE MADE IF IWORK(1).LE.0. */

/*                  THE LENGTH OF IWORK(*) MUST BE AT LEAST */

/*                  LIW = ME+MA+N */
/*                  THIS TEST WILL NOT BE MADE IF IWORK(2).LE.0. */

/*     OUTPUT.. */

/*     X(*)         AN ARRAY DIMENSIONED AT LEAST N, WHICH WILL */
/*                  CONTAIN THE N COMPONENTS OF THE SOLUTION VECTOR */
/*                  ON OUTPUT. */

/*     RNORM        THE RESIDUAL NORM OF THE SOLUTION.  THE VALUE OF */
/*                  RNORM CONTAINS THE RESIDUAL VECTOR LENGTH OF THE */
/*                  EQUALITY CONSTRAINTS AND LEAST SQUARES EQUATIONS. */

/*     MODE         THE VALUE OF MODE INDICATES THE SUCCESS OR FAILURE */
/*                  OF THE SUBPROGRAM. */

/*                  MODE = 0  SUBPROGRAM COMPLETED SUCCESSFULLY. */

/*                       = 1  MAX. NUMBER OF ITERATIONS (EQUAL TO */
/*                            3*(N-L)) EXCEEDED. NEARLY ALL PROBLEMS */
/*                            SHOULD COMPLETE IN FEWER THAN THIS */
/*                            NUMBER OF ITERATIONS. AN APPROXIMATE */
/*                            SOLUTION AND ITS CORRESPONDING RESIDUAL */
/*                            VECTOR LENGTH ARE IN X(*) AND RNORM. */

/*                       = 2  USAGE ERROR OCCURRED.  THE OFFENDING */
/*                            CONDITION IS NOTED WITH THE ERROR */
/*                            PROCESSING SUBPROGRAM, XERROR( ). */

/*     USER-DESIGNATED */
/*     WORKING ARRAYS.. */

/*     WORK(*)      A WORKING ARRAY OF LENGTH AT LEAST */
/*                  M + 5*N. */

/*     IWORK(*)     AN INTEGER-VALUED WORKING ARRAY OF LENGTH AT LEAST */
/*                  M+N. */

/*     THE EDITING REQUIRED TO CONVERT THIS SUBROUTINE FROM SINGLE TO */
/*     DOUBLE PRECISION INVOLVES THE FOLLOWING CHARACTER STRING CHANGES. */
/*     USE AN EDITING COMMAND (CHANGE) /STRING-1/(TO)STRING-2/. */
/*     (START AT LINE WITH C++ IN COLS. 1-3.) */
/*     /REAL (12 BLANKS)/DOUBLE PRECISION/,/, DUMMY/,SNGL(DUMMY)/ */

/*     WRITTEN BY KAREN H. HASKELL, SANDIA LABORATORIES, */
/*     AND R.J. HANSON, SANDIA LABORATORIES. */
/*     REVISED FEB.25, 1982. */

/*     SUBROUTINES CALLED BY WNNLS( ) */

/* ++ */
/*     WNLSM         COMPANION SUBROUTINE TO WNNLS( ), WHERE */
/*                   MOST OF THE COMPUTATION TAKES PLACE. */

/*     XERROR,XERRWV FROM SLATEC ERROR PROCESSING PACKAGE. */
/*                   THIS IS DOCUMENTED IN SANDIA TECH. REPT., */
/*                   SAND78-1189. */

/*     REFERENCES */

/*     1. SOLVING LEAST SQUARES PROBLEMS, BY C.L. LAWSON */
/*        AND R.J. HANSON.  PRENTICE-HALL, INC. (1974). */

/*     2. BASIC LINEAR ALGEBRA SUBPROGRAMS FOR FORTRAN USAGE, BY */
/*        C.L. LAWSON, R.J. HANSON, D.R. KINCAID, AND F.T. KROGH. */
/*        TOMS, V. 5, NO. 3, P. 308.  ALSO AVAILABLE AS */
/*        SANDIA TECHNICAL REPORT NO. SAND77-0898. */

/*     3. AN ALGORITHM FOR LINEAR LEAST SQUARES WITH EQUALITY */
/*        AND NONNEGATIVITY CONSTRAINTS, BY K.H. HASKELL AND */
/*        R.J. HANSON.  AVAILABLE AS SANDIA TECHNICAL REPORT NO. */
/*        SAND77-0552, AND MATH. PROGRAMMING, VOL. 21, (1981), P. 98-118. */

/*     4. SLATEC COMMON MATH. LIBRARY ERROR HANDLING */
/*        PACKAGE.  BY R. E. JONES.  AVAILABLE AS SANDIA */
/*        TECHNICAL REPORT SAND78-1189. */

/*<       REAL              DUMMY, W(MDW,1), PRGOPT(1), X(1), WORK(1), RNORM >*/
/*<       INTEGER IWORK(1) >*/


/*<       MODE = 0 >*/
#line 1496 "cl2.f"
    /* Parameter adjustments */
#line 1496 "cl2.f"
    w_dim1 = *mdw;
#line 1496 "cl2.f"
    w_offset = 1 + w_dim1;
#line 1496 "cl2.f"
    w -= w_offset;
#line 1496 "cl2.f"
    --prgopt;
#line 1496 "cl2.f"
    --x;
#line 1496 "cl2.f"
    --iwork;
#line 1496 "cl2.f"
    --work;
#line 1496 "cl2.f"

#line 1496 "cl2.f"
    /* Function Body */
#line 1496 "cl2.f"
    *mode = 0;
/*<       IF (MA+ME.LE.0 .OR. N.LE.0) RETURN >*/
#line 1497 "cl2.f"
    if (*ma + *me <= 0 || *n <= 0) {
#line 1497 "cl2.f"
	return 0;
#line 1497 "cl2.f"
    }
/*<       IF (.NOT.(IWORK(1).GT.0)) GO TO 20 >*/
#line 1498 "cl2.f"
    if (! (iwork[1] > 0)) {
#line 1498 "cl2.f"
	goto L20;
#line 1498 "cl2.f"
    }
/*<       LW = ME + MA + 5*N >*/
#line 1499 "cl2.f"
    lw = *me + *ma + *n * 5;
/*<       IF (.NOT.(IWORK(1).LT.LW)) GO TO 10 >*/
#line 1500 "cl2.f"
    if (! (iwork[1] < lw)) {
#line 1500 "cl2.f"
	goto L10;
#line 1500 "cl2.f"
    }
/*<       NERR = 2 >*/
#line 1501 "cl2.f"
    nerr = 2;
/*<       IOPT = 1 >*/
#line 1502 "cl2.f"
    iopt = 1;
/* cc      CALL XERRWV(70HWNNLS( ), INSUFFICIENT STORAGE ALLOCATED FOR WORK(* */
/* cc     *), NEED LW=I1 BELOW, 70, NERR, IOPT, 1, LW, 0, 0, DUMMY, DUMMY) */
/*<       MODE = 2 >*/
#line 1505 "cl2.f"
    *mode = 2;
/*<       RETURN >*/
#line 1506 "cl2.f"
    return 0;
/*<    10 CONTINUE >*/
#line 1507 "cl2.f"
L10:
/*<    20 IF (.NOT.(IWORK(2).GT.0)) GO TO 40 >*/
#line 1508 "cl2.f"
L20:
#line 1508 "cl2.f"
    if (! (iwork[2] > 0)) {
#line 1508 "cl2.f"
	goto L40;
#line 1508 "cl2.f"
    }
/*<       LIW = ME + MA + N >*/
#line 1509 "cl2.f"
    liw = *me + *ma + *n;
/*<       IF (.NOT.(IWORK(2).LT.LIW)) GO TO 30 >*/
#line 1510 "cl2.f"
    if (! (iwork[2] < liw)) {
#line 1510 "cl2.f"
	goto L30;
#line 1510 "cl2.f"
    }
/*<       NERR = 2 >*/
#line 1511 "cl2.f"
    nerr = 2;
/*<       IOPT = 1 >*/
#line 1512 "cl2.f"
    iopt = 1;
/* cc      CALL XERRWV(72HWNNLS( ), INSUFFICIENT STORAGE ALLOCATED FOR IWORK( */
/* cc     **), NEED LIW=I1 BELOW, 72, NERR, IOPT, 1, LIW, 0, 0, DUMMY, DUMMY) */
/*<       MODE = 2 >*/
#line 1515 "cl2.f"
    *mode = 2;
/*<       RETURN >*/
#line 1516 "cl2.f"
    return 0;
/*<    30 CONTINUE >*/
#line 1517 "cl2.f"
L30:
/*<    40 IF (.NOT.(MDW.LT.ME+MA)) GO TO 50 >*/
#line 1518 "cl2.f"
L40:
#line 1518 "cl2.f"
    if (! (*mdw < *me + *ma)) {
#line 1518 "cl2.f"
	goto L50;
#line 1518 "cl2.f"
    }
/*<       NERR = 1 >*/
#line 1519 "cl2.f"
    nerr = 1;
/*<       IOPT = 1 >*/
#line 1520 "cl2.f"
    iopt = 1;
/* cc      CALL XERROR(44HWNNLS( ), THE VALUE MDW.LT.ME+MA IS AN ERROR, 44, */
/* cc     * NERR, IOPT) */
/*<       MODE = 2 >*/
#line 1523 "cl2.f"
    *mode = 2;
/*<       RETURN >*/
#line 1524 "cl2.f"
    return 0;
/*<    50 IF (0.LE.L .AND. L.LE.N) GO TO 60 >*/
#line 1525 "cl2.f"
L50:
#line 1525 "cl2.f"
    if (0 <= *l && *l <= *n) {
#line 1525 "cl2.f"
	goto L60;
#line 1525 "cl2.f"
    }
/*<       NERR = 2 >*/
#line 1526 "cl2.f"
    nerr = 2;
/*<       IOPT = 1 >*/
#line 1527 "cl2.f"
    iopt = 1;
/* cc      CALL XERROR(39HWNNLS( ), L.LE.0.AND.L.LE.N IS REQUIRED, 39, NERR, */
/* cc     * IOPT) */
/*<       MODE = 2 >*/
#line 1530 "cl2.f"
    *mode = 2;
/*<       RETURN >*/
#line 1531 "cl2.f"
    return 0;

/*     THE PURPOSE OF THIS SUBROUTINE IS TO BREAK UP THE ARRAYS */
/*     WORK(*) AND IWORK(*) INTO SEPARATE WORK ARRAYS */
/*     REQUIRED BY THE MAIN SUBROUTINE WNLSM( ). */

/*<    60 L1 = N + 1 >*/
#line 1537 "cl2.f"
L60:
#line 1537 "cl2.f"
    l1 = *n + 1;
/*<       L2 = L1 + N >*/
#line 1538 "cl2.f"
    l2 = l1 + *n;
/*<       L3 = L2 + ME + MA >*/
#line 1539 "cl2.f"
    l3 = l2 + *me + *ma;
/*<       L4 = L3 + N >*/
#line 1540 "cl2.f"
    l4 = l3 + *n;
/*<       L5 = L4 + N >*/
#line 1541 "cl2.f"
    l5 = l4 + *n;

/*<    >*/
#line 1543 "cl2.f"
    wnlsm_(&w[w_offset], mdw, me, ma, n, l, &prgopt[1], &x[1], rnorm, mode, &
	    iwork[1], &iwork[l1], &work[1], &work[l1], &work[l2], &work[l3], &
	    work[l4], &work[l5]);
/*<       RETURN >*/
#line 1546 "cl2.f"
    return 0;
/*<       END >*/
} /* wnnls_fort */

/* CC */
/* CC---------------------------------------------------------------------------- */
/* CC */
/*<    >*/
static /* Subroutine */ int wnlsm_(real *w, integer *mdw, integer *mme, integer *ma,
	integer *n, integer *l, real *prgopt, real *x, real *rnorm, integer *
	mode, integer *ipivot, integer *itype, real *wd, real *h__, real *
	scale, real *z__, real *temp, real *d__)
{
    /* Initialized data */

    static real zero = 0.f;
    static real one = 1.f;
    static real two = 2.f;
    static real srelpr = 0.f;

    /* Format strings */
    static char fmt_10[] = "";
    static char fmt_20[] = "";
    static char fmt_30[] = "";
    static char fmt_40[] = "";
    static char fmt_60[] = "";
    static char fmt_90[] = "";
    static char fmt_220[] = "";
    static char fmt_310[] = "";
    static char fmt_460[] = "";
    static char fmt_480[] = "";
    static char fmt_500[] = "";
    static char fmt_520[] = "";
    static char fmt_880[] = "";
    static char fmt_900[] = "";
    static char fmt_1010[] = "";

    /* System generated locals */
    integer w_dim1, w_offset, i__1, i__2;
    real r__1, r__2;

    /* Builtin functions */
    double sqrt(doublereal);

    /* Local variables */
    static integer i__, j, m;
    static real t;
    static integer l1;
    static real z2;
    static integer me, jj, jp;
    static real sm, zz;
    static integer jm1, nm1, lp1, np1;
    static real fac;
    static integer key;
    static real tau;
    static integer niv;
    static logical pos;
    static integer mep1, krp1, niv1, nsp1;
    static logical done;
    static real amax, dope[4];
    static integer jcon, link, imax;
    static real alsq;
    static integer last, iter, nerr, isol, iopt;
    static real wmax;
    static integer next, nopt, igo980, igo991, igo983, igo938, igo958, igo995,
	     igo986, igo977, igo998, igo897;
    static real alpha;
    static integer idope[8];
    static integer krank, nlink;
    static real bnorm;
    static integer itemp, itmax, iwmax;
    static integer nsoln;
    static real alamda;
    static logical feasbl;
    static real eanorm;
    static real sparam[5];
    static logical hitcon;
    static integer ntimes;
    static real blowup;

    /* Assigned format variables */
    static char *igo998_fmt, *igo995_fmt, *igo991_fmt, *igo986_fmt, *
	    igo983_fmt, *igo980_fmt, *igo977_fmt, *igo958_fmt, *igo938_fmt, *
	    igo897_fmt;



/*     THE EDITING REQUIRED TO CONVERT THIS SUBROUTINE FROM SINGLE TO */
/*     DOUBLE PRECISION INVOLVES THE FOLLOWING CHARACTER STRING CHANGES. */
/*     USE AN EDITING COMMAND (CHANGE) /STRING-1/(TO)STRING-2/. */
/*     (START CHANGES AT LINE WITH C++ IN COLS. 1-3.) */
/*     /REAL (12 BLANKS)/DOUBLE PRECISION/,/SASUM/DASUM/,/SROTMG/DROTMG/, */
/*     /SNRM2/DNRM2/,/ SQRT/ DSQRT/,/SROTM/DROTM/,/AMAX1/DMAX1/, */
/*     /SCOPY/DCOPY/,/SSCAL/DSCAL/,/SAXPY/DAXPY/,/E0/D0/,/SSWAP/DSWAP/, */
/*     /ISAMAX/IDAMAX/,/SRELPR/DRELPR/,/.E-/.D-/                   REMK */

/*     THIS IS A COMPANION SUBPROGRAM TO WNNLS( ). */
/*     THE DOCUMENTATION FOR WNNLS( ) HAS MORE COMPLETE */
/*     USAGE INSTRUCTIONS. */

/*     WRITTEN BY KAREN H. HASKELL, SANDIA LABORATORIES, */
/*     WITH THE HELP OF R.J. HANSON, SANDIA LABORATORIES, */
/*     DECEMBER 1976 - JANUARY 1978. */
/*     REVISED MAR. 4, 1982. */

/*     IN ADDITION TO THE PARAMETERS DISCUSSED IN THE PROLOGUE TO */
/*     SUBROUTINE WNNLS, THE FOLLOWING WORK ARRAYS ARE USED IN */
/*     SUBROUTINE WNLSM  (THEY ARE PASSED THROUGH THE CALLING */
/*     SEQUENCE FROM WNNLS FOR PURPOSES OF VARIABLE DIMENSIONING). */
/*     THEIR CONTENTS WILL IN GENERAL BE OF NO INTEREST TO THE USER. */

/*         IPIVOT(*) */
/*            AN ARRAY OF LENGTH N.  UPON COMPLETION IT CONTAINS THE */
/*         PIVOTING INFORMATION FOR THE COLS OF W(*,*). */

/*         ITYPE(*) */
/*            AN ARRAY OF LENGTH M WHICH IS USED TO KEEP TRACK */
/*         OF THE CLASSIFICATION OF THE EQUATIONS.  ITYPE(I)=0 */
/*         DENOTES EQUATION I AS AN EQUALITY CONSTRAINT. */
/*         ITYPE(I)=1 DENOTES EQUATION I AS A LEAST SQUARES */
/*         EQUATION. */

/*         WD(*) */
/*            AN ARRAY OF LENGTH N.  UPON COMPLETION IT CONTAINS THE */
/*         DUAL SOLUTION VECTOR. */

/*         H(*) */
/*            AN ARRAY OF LENGTH N.  UPON COMPLETION IT CONTAINS THE */
/*         PIVOT SCALARS OF THE HOUSEHOLDER TRANSFORMATIONS PERFORMED */
/*         IN THE CASE KRANK.LT.L. */

/*         SCALE(*) */
/*            AN ARRAY OF LENGTH M WHICH IS USED BY THE SUBROUTINE */
/*         TO STORE THE DIAGONAL MATRIX OF WEIGHTS. */
/*         THESE ARE USED TO APPLY THE MODIFIED GIVENS */
/*         TRANSFORMATIONS. */

/*         Z(*),TEMP(*) */
/*            WORKING ARRAYS OF LENGTH N. */

/*         D(*) */
/*            AN ARRAY OF LENGTH N THAT CONTAINS THE */
/*         COLUMN SCALING FOR THE MATRIX (E). */
/*                                       (A) */

/*     SUBROUTINE WNLSM (W,MDW,MME,MA,N,L,PRGOPT,X,RNORM,MODE, */
/*    1                  IPIVOT,ITYPE,WD,H,SCALE,Z,TEMP,D) */
/* ++ */
/*<       REAL             W(MDW,1), X(1), WD(1), H(1), SCALE(1), DOPE(4) >*/
/*<       REAL             Z(1), TEMP(1), PRGOPT(1), D(1), SPARAM(5) >*/
/*<       REAL             ALAMDA, ALPHA, ALSQ, AMAX, BNORM, EANORM >*/
/*<       REAL             SRELPR, FAC, ONE, BLOWUP >*/
/*<       REAL             RNORM, SM, T, TAU, TWO, WMAX, ZERO, ZZ, Z2 >*/
/*<       REAL             AMAX1, SQRT, SNRM2, SASUM >*/
/*<       INTEGER IPIVOT(1), ITYPE(1), ISAMAX, IDOPE(8) >*/
/*<       LOGICAL HITCON, FEASBL, DONE, POS >*/
/*<       DATA ZERO /0.E0/, ONE /1.E0/, TWO /2.E0/, SRELPR /0.E0/ >*/
#line 1624 "cl2.f"
    /* Parameter adjustments */
#line 1624 "cl2.f"
    w_dim1 = *mdw;
#line 1624 "cl2.f"
    w_offset = 1 + w_dim1;
#line 1624 "cl2.f"
    w -= w_offset;
#line 1624 "cl2.f"
    --prgopt;
#line 1624 "cl2.f"
    --x;
#line 1624 "cl2.f"
    --ipivot;
#line 1624 "cl2.f"
    --itype;
#line 1624 "cl2.f"
    --wd;
#line 1624 "cl2.f"
    --h__;
#line 1624 "cl2.f"
    --scale;
#line 1624 "cl2.f"
    --z__;
#line 1624 "cl2.f"
    --temp;
#line 1624 "cl2.f"
    --d__;
#line 1624 "cl2.f"

#line 1624 "cl2.f"
    /* Function Body */

/*     INITIALIZE-VARIABLES */
/*<       ASSIGN 10 TO IGO998 >*/
#line 1627 "cl2.f"
    igo998 = 0;
#line 1627 "cl2.f"
    igo998_fmt = fmt_10;
/*<       GO TO 180 >*/
#line 1628 "cl2.f"
    goto L180;

/*     PERFORM INITIAL TRIANGULARIZATION IN THE SUBMATRIX */
/*     CORRESPONDING TO THE UNCONSTRAINED VARIABLES USING */
/*     THE PROCEDURE INITIALLY-TRIANGULARIZE. */
/*<    10 ASSIGN 20 TO IGO995 >*/
#line 1633 "cl2.f"
L10:
#line 1633 "cl2.f"
    igo995 = 0;
#line 1633 "cl2.f"
    igo995_fmt = fmt_20;
/*<       GO TO 280 >*/
#line 1634 "cl2.f"
    goto L280;

/*     PERFORM WNNLS ALGORITHM USING THE FOLLOWING STEPS. */

/*     UNTIL(DONE) */

/*        COMPUTE-SEARCH-DIRECTION-AND-FEASIBLE-POINT */

/*        WHEN (HITCON) ADD-CONSTRAINTS */

/*        ELSE PERFORM-MULTIPLIER-TEST-AND-DROP-A-CONSTRAINT */

/*        FIN */

/*     COMPUTE-FINAL-SOLUTION */

/*<    20 IF (DONE) GO TO 80 >*/
#line 1650 "cl2.f"
L20:
#line 1650 "cl2.f"
    if (done) {
#line 1650 "cl2.f"
	goto L80;
#line 1650 "cl2.f"
    }

/*<       ASSIGN 30 TO IGO991 >*/
#line 1652 "cl2.f"
    igo991 = 0;
#line 1652 "cl2.f"
    igo991_fmt = fmt_30;
/*<       GO TO 300 >*/
#line 1653 "cl2.f"
    goto L300;

/*     COMPUTE-SEARCH-DIRECTION-AND-FEASIBLE-POINT */

/*<    30 IF (.NOT.(HITCON)) GO TO 50 >*/
#line 1657 "cl2.f"
L30:
#line 1657 "cl2.f"
    if (! hitcon) {
#line 1657 "cl2.f"
	goto L50;
#line 1657 "cl2.f"
    }
/*<       ASSIGN 40 TO IGO986 >*/
#line 1658 "cl2.f"
    igo986 = 0;
#line 1658 "cl2.f"
    igo986_fmt = fmt_40;
/*<       GO TO 370 >*/
#line 1659 "cl2.f"
    goto L370;
/*<    40 GO TO 70 >*/
#line 1660 "cl2.f"
L40:
#line 1660 "cl2.f"
    goto L70;

/*     WHEN (HITCON) ADD-CONSTRAINTS */

/*<    50 ASSIGN 60 TO IGO983 >*/
#line 1664 "cl2.f"
L50:
#line 1664 "cl2.f"
    igo983 = 0;
#line 1664 "cl2.f"
    igo983_fmt = fmt_60;
/*<       GO TO 640 >*/
#line 1665 "cl2.f"
    goto L640;
/*<    60 CONTINUE >*/
#line 1666 "cl2.f"
L60:

/*     ELSE PERFORM-MULTIPLIER-TEST-AND-DROP-A-CONSTRAINT */

/*<    70 GO TO 20 >*/
#line 1670 "cl2.f"
L70:
#line 1670 "cl2.f"
    goto L20;

/*<    80 ASSIGN 90 TO IGO980 >*/
#line 1672 "cl2.f"
L80:
#line 1672 "cl2.f"
    igo980 = 0;
#line 1672 "cl2.f"
    igo980_fmt = fmt_90;
/*<       GO TO 1000 >*/
#line 1673 "cl2.f"
    goto L1000;

/*     COMPUTE-FINAL-SOLUTION */

/*<    90 RETURN >*/
#line 1677 "cl2.f"
L90:
#line 1677 "cl2.f"
    return 0;
/*<   100 CONTINUE >*/
#line 1678 "cl2.f"
L100:

/*     TO PROCESS-OPTION-VECTOR */
/*<       FAC = 1.E-4 >*/
#line 1681 "cl2.f"
    fac = 1e-4f;

/*     THE NOMINAL TOLERANCE USED IN THE CODE, */
/*<       TAU = SQRT(SRELPR) >*/
#line 1684 "cl2.f"
    tau = sqrt(srelpr);

/*     THE NOMINAL BLOW-UP FACTOR USED IN THE CODE. */
/*<       BLOWUP = TAU >*/
#line 1687 "cl2.f"
    blowup = tau;

/*     THE NOMINAL COLUMN SCALING USED IN THE CODE IS */
/*     THE IDENTITY SCALING. */
/*<       D(1) = ONE >*/
#line 1691 "cl2.f"
    d__[1] = one;
/*<       CALL SCOPY(N, D, 0, D, 1) >*/
#line 1692 "cl2.f"
    scopy_(n, &d__[1], &c__0, &d__[1], &c__1);

/*     DEFINE BOUND FOR NUMBER OF OPTIONS TO CHANGE. */
/*<       NOPT = 1000 >*/
#line 1695 "cl2.f"
    nopt = 1000;

/*     DEFINE BOUND FOR POSITIVE VALUE OF LINK. */
/*<       NLINK = 100000 >*/
#line 1698 "cl2.f"
    nlink = 100000;
/*<       NTIMES = 0 >*/
#line 1699 "cl2.f"
    ntimes = 0;
/*<       LAST = 1 >*/
#line 1700 "cl2.f"
    last = 1;
/*<       LINK = PRGOPT(1) >*/
#line 1701 "cl2.f"
    link = prgopt[1];
/*<       IF (.NOT.(LINK.LE.0 .OR. LINK.GT.NLINK)) GO TO 110 >*/
#line 1702 "cl2.f"
    if (! (link <= 0 || link > nlink)) {
#line 1702 "cl2.f"
	goto L110;
#line 1702 "cl2.f"
    }
/*<       NERR = 3 >*/
#line 1703 "cl2.f"
    nerr = 3;
/*<       IOPT = 1 >*/
#line 1704 "cl2.f"
    iopt = 1;
/* cc      CALL XERROR(39HWNNLS( ) THE OPTION VECTOR IS UNDEFINED, 39, NERR, */
/* cc     * IOPT) */
/*<       MODE = 2 >*/
#line 1707 "cl2.f"
    *mode = 2;
/*<       RETURN >*/
#line 1708 "cl2.f"
    return 0;
/*<   110 IF (.NOT.(LINK.GT.1)) GO TO 160 >*/
#line 1709 "cl2.f"
L110:
#line 1709 "cl2.f"
    if (! (link > 1)) {
#line 1709 "cl2.f"
	goto L160;
#line 1709 "cl2.f"
    }
/*<       NTIMES = NTIMES + 1 >*/
#line 1710 "cl2.f"
    ++ntimes;
/*<       IF (.NOT.(NTIMES.GT.NOPT)) GO TO 120 >*/
#line 1711 "cl2.f"
    if (! (ntimes > nopt)) {
#line 1711 "cl2.f"
	goto L120;
#line 1711 "cl2.f"
    }
/*<       NERR = 3 >*/
#line 1712 "cl2.f"
    nerr = 3;
/*<       IOPT = 1 >*/
#line 1713 "cl2.f"
    iopt = 1;
/* cc      CALL XERROR( */
/* cc     * 53HWNNLS( ). THE LINKS IN THE OPTION VECTOR ARE CYCLING., 53, */
/* cc     * NERR, IOPT) */
/*<       MODE = 2 >*/
#line 1717 "cl2.f"
    *mode = 2;
/*<       RETURN >*/
#line 1718 "cl2.f"
    return 0;
/*<   120 KEY = PRGOPT(LAST+1) >*/
#line 1719 "cl2.f"
L120:
#line 1719 "cl2.f"
    key = prgopt[last + 1];
/*<       IF (.NOT.(KEY.EQ.6 .AND. PRGOPT(LAST+2).NE.ZERO)) GO TO 140 >*/
#line 1720 "cl2.f"
    if (! (key == 6 && prgopt[last + 2] != zero)) {
#line 1720 "cl2.f"
	goto L140;
#line 1720 "cl2.f"
    }
/*<       DO 130 J=1,N >*/
#line 1721 "cl2.f"
    i__1 = *n;
#line 1721 "cl2.f"
    for (j = 1; j <= i__1; ++j) {
/*<         T = SNRM2(M,W(1,J),1) >*/
#line 1722 "cl2.f"
	t = snrm2_(&m, &w[j * w_dim1 + 1], &c__1);
/*<         IF (T.NE.ZERO) T = ONE/T >*/
#line 1723 "cl2.f"
	if (t != zero) {
#line 1723 "cl2.f"
	    t = one / t;
#line 1723 "cl2.f"
	}
/*<         D(J) = T >*/
#line 1724 "cl2.f"
	d__[j] = t;
/*<   130 CONTINUE >*/
#line 1725 "cl2.f"
/* L130: */
#line 1725 "cl2.f"
    }
/*<   140 IF (KEY.EQ.7) CALL SCOPY(N, PRGOPT(LAST+2), 1, D, 1) >*/
#line 1726 "cl2.f"
L140:
#line 1726 "cl2.f"
    if (key == 7) {
#line 1726 "cl2.f"
	scopy_(n, &prgopt[last + 2], &c__1, &d__[1], &c__1);
#line 1726 "cl2.f"
    }
/*<       IF (KEY.EQ.8) TAU = AMAX1(SRELPR,PRGOPT(LAST+2)) >*/
#line 1727 "cl2.f"
    if (key == 8) {
/* Computing MAX */
#line 1727 "cl2.f"
	r__1 = srelpr, r__2 = prgopt[last + 2];
#line 1727 "cl2.f"
	tau = max(r__1,r__2);
#line 1727 "cl2.f"
    }
/*<       IF (KEY.EQ.9) BLOWUP = AMAX1(SRELPR,PRGOPT(LAST+2)) >*/
#line 1728 "cl2.f"
    if (key == 9) {
/* Computing MAX */
#line 1728 "cl2.f"
	r__1 = srelpr, r__2 = prgopt[last + 2];
#line 1728 "cl2.f"
	blowup = max(r__1,r__2);
#line 1728 "cl2.f"
    }
/*<       NEXT = PRGOPT(LINK) >*/
#line 1729 "cl2.f"
    next = prgopt[link];
/*<       IF (.NOT.(NEXT.LE.0 .OR. NEXT.GT.NLINK)) GO TO 150 >*/
#line 1730 "cl2.f"
    if (! (next <= 0 || next > nlink)) {
#line 1730 "cl2.f"
	goto L150;
#line 1730 "cl2.f"
    }
/*<       NERR = 3 >*/
#line 1731 "cl2.f"
    nerr = 3;
/*<       IOPT = 1 >*/
#line 1732 "cl2.f"
    iopt = 1;
/* cc      CALL XERROR(39HWNNLS( ) THE OPTION VECTOR IS UNDEFINED, 39, NERR, */
/* cc     * IOPT) */
/*<       MODE = 2 >*/
#line 1735 "cl2.f"
    *mode = 2;
/*<       RETURN >*/
#line 1736 "cl2.f"
    return 0;
/*<   150 LAST = LINK >*/
#line 1737 "cl2.f"
L150:
#line 1737 "cl2.f"
    last = link;
/*<       LINK = NEXT >*/
#line 1738 "cl2.f"
    link = next;
/*<       GO TO 110 >*/
#line 1739 "cl2.f"
    goto L110;
/*<   160 DO 170 J=1,N >*/
#line 1740 "cl2.f"
L160:
#line 1740 "cl2.f"
    i__1 = *n;
#line 1740 "cl2.f"
    for (j = 1; j <= i__1; ++j) {
/*<         CALL SSCAL(M, D(J), W(1,J), 1) >*/
#line 1741 "cl2.f"
	sscal_(&m, &d__[j], &w[j * w_dim1 + 1], &c__1);
/*<   170 CONTINUE >*/
#line 1742 "cl2.f"
/* L170: */
#line 1742 "cl2.f"
    }
/*<       GO TO 1260 >*/
#line 1743 "cl2.f"
    goto L1260;
/*<   180 CONTINUE >*/
#line 1744 "cl2.f"
L180:

/*     TO INITIALIZE-VARIABLES */

/*     SRELPR IS THE PRECISION FOR THE PARTICULAR MACHINE */
/*     BEING USED.  THIS LOGIC AVOIDS RECOMPUTING IT EVERY ENTRY. */
/*<       IF (.NOT.(SRELPR.EQ.ZERO)) GO TO 210 >*/
#line 1750 "cl2.f"
    if (! (srelpr == zero)) {
#line 1750 "cl2.f"
	goto L210;
#line 1750 "cl2.f"
    }
/*<       SRELPR = ONE >*/
#line 1751 "cl2.f"
    srelpr = one;
/*<   190 IF (ONE+SRELPR.EQ.ONE) GO TO 200 >*/
#line 1752 "cl2.f"
L190:
#line 1752 "cl2.f"
    if (one + srelpr == one) {
#line 1752 "cl2.f"
	goto L200;
#line 1752 "cl2.f"
    }
/*<       SRELPR = SRELPR/TWO >*/
#line 1753 "cl2.f"
    srelpr /= two;
/*<       GO TO 190 >*/
#line 1754 "cl2.f"
    goto L190;
/*<   200 SRELPR = SRELPR*TWO >*/
#line 1755 "cl2.f"
L200:
#line 1755 "cl2.f"
    srelpr *= two;
/*<   210 M = MA + MME >*/
#line 1756 "cl2.f"
L210:
#line 1756 "cl2.f"
    m = *ma + *mme;
/*<       ME = MME >*/
#line 1757 "cl2.f"
    me = *mme;
/*<       MEP1 = ME + 1 >*/
#line 1758 "cl2.f"
    mep1 = me + 1;
/*<       ASSIGN 220 TO IGO977 >*/
#line 1759 "cl2.f"
    igo977 = 0;
#line 1759 "cl2.f"
    igo977_fmt = fmt_220;
/*<       GO TO 100 >*/
#line 1760 "cl2.f"
    goto L100;

/*     PROCESS-OPTION-VECTOR */
/*<   220 DONE = .FALSE. >*/
#line 1763 "cl2.f"
L220:
#line 1763 "cl2.f"
    done = FALSE_;
/*<       ITER = 0 >*/
#line 1764 "cl2.f"
    iter = 0;
/*<       ITMAX = 3*(N-L) >*/
#line 1765 "cl2.f"
    itmax = (*n - *l) * 3;
/*<       MODE = 0 >*/
#line 1766 "cl2.f"
    *mode = 0;
/*<       LP1 = L + 1 >*/
#line 1767 "cl2.f"
    lp1 = *l + 1;
/*<       NSOLN = L >*/
#line 1768 "cl2.f"
    nsoln = *l;
/*<       NSP1 = NSOLN + 1 >*/
#line 1769 "cl2.f"
    nsp1 = nsoln + 1;
/*<       NP1 = N + 1 >*/
#line 1770 "cl2.f"
    np1 = *n + 1;
/*<       NM1 = N - 1 >*/
#line 1771 "cl2.f"
    nm1 = *n - 1;
/*<       L1 = MIN0(M,L) >*/
#line 1772 "cl2.f"
    l1 = min(m,*l);

/*     COMPUTE SCALE FACTOR TO APPLY TO EQUAL. CONSTRAINT EQUAS. */
/*<       DO 230 J=1,N >*/
#line 1775 "cl2.f"
    i__1 = *n;
#line 1775 "cl2.f"
    for (j = 1; j <= i__1; ++j) {
/*<         WD(J) = SASUM(M,W(1,J),1) >*/
#line 1776 "cl2.f"
	wd[j] = sasum_(&m, &w[j * w_dim1 + 1], &c__1);
/*<   230 CONTINUE >*/
#line 1777 "cl2.f"
/* L230: */
#line 1777 "cl2.f"
    }
/*<       IMAX = ISAMAX(N,WD,1) >*/
#line 1778 "cl2.f"
    imax = isamax_(n, &wd[1], &c__1);
/*<       EANORM = WD(IMAX) >*/
#line 1779 "cl2.f"
    eanorm = wd[imax];
/*<       BNORM = SASUM(M,W(1,NP1),1) >*/
#line 1780 "cl2.f"
    bnorm = sasum_(&m, &w[np1 * w_dim1 + 1], &c__1);
/*<       ALAMDA = EANORM/(SRELPR*FAC) >*/
#line 1781 "cl2.f"
    alamda = eanorm / (srelpr * fac);

/*     DEFINE SCALING DIAG MATRIX FOR MOD GIVENS USAGE AND */
/*     CLASSIFY EQUATION TYPES. */
/*<       ALSQ = ALAMDA**2 >*/
/* Computing 2nd power */
#line 1785 "cl2.f"
    r__1 = alamda;
#line 1785 "cl2.f"
    alsq = r__1 * r__1;
/*<       DO 260 I=1,M >*/
#line 1786 "cl2.f"
    i__1 = m;
#line 1786 "cl2.f"
    for (i__ = 1; i__ <= i__1; ++i__) {

/*     WHEN EQU I IS HEAVILY WEIGHTED ITYPE(I)=0, ELSE ITYPE(I)=1. */
/*<         IF (.NOT.(I.LE.ME)) GO TO 240 >*/
#line 1789 "cl2.f"
	if (! (i__ <= me)) {
#line 1789 "cl2.f"
	    goto L240;
#line 1789 "cl2.f"
	}
/*<         T = ALSQ >*/
#line 1790 "cl2.f"
	t = alsq;
/*<         ITEMP = 0 >*/
#line 1791 "cl2.f"
	itemp = 0;
/*<         GO TO 250 >*/
#line 1792 "cl2.f"
	goto L250;
/*<   240   T = ONE >*/
#line 1793 "cl2.f"
L240:
#line 1793 "cl2.f"
	t = one;
/*<         ITEMP = 1 >*/
#line 1794 "cl2.f"
	itemp = 1;
/*<   250   SCALE(I) = T >*/
#line 1795 "cl2.f"
L250:
#line 1795 "cl2.f"
	scale[i__] = t;
/*<         ITYPE(I) = ITEMP >*/
#line 1796 "cl2.f"
	itype[i__] = itemp;
/*<   260 CONTINUE >*/
#line 1797 "cl2.f"
/* L260: */
#line 1797 "cl2.f"
    }

/*     SET THE SOLN VECTOR X(*) TO ZERO AND THE COL INTERCHANGE */
/*     MATRIX TO THE IDENTITY. */
/*<       X(1) = ZERO >*/
#line 1801 "cl2.f"
    x[1] = zero;
/*<       CALL SCOPY(N, X, 0, X, 1) >*/
#line 1802 "cl2.f"
    scopy_(n, &x[1], &c__0, &x[1], &c__1);
/*<       DO 270 I=1,N >*/
#line 1803 "cl2.f"
    i__1 = *n;
#line 1803 "cl2.f"
    for (i__ = 1; i__ <= i__1; ++i__) {
/*<         IPIVOT(I) = I >*/
#line 1804 "cl2.f"
	ipivot[i__] = i__;
/*<   270 CONTINUE >*/
#line 1805 "cl2.f"
/* L270: */
#line 1805 "cl2.f"
    }
/*<       GO TO 1230 >*/
#line 1806 "cl2.f"
    goto L1230;
/*<   280 CONTINUE >*/
#line 1807 "cl2.f"
L280:

/*     TO INITIALLY-TRIANGULARIZE */

/*     SET FIRST L COMPS. OF DUAL VECTOR TO ZERO BECAUSE */
/*     THESE CORRESPOND TO THE UNCONSTRAINED VARIABLES. */
/*<       IF (.NOT.(L.GT.0)) GO TO 290 >*/
#line 1813 "cl2.f"
    if (! (*l > 0)) {
#line 1813 "cl2.f"
	goto L290;
#line 1813 "cl2.f"
    }
/*<       WD(1) = ZERO >*/
#line 1814 "cl2.f"
    wd[1] = zero;
/*<       CALL SCOPY(L, WD, 0, WD, 1) >*/
#line 1815 "cl2.f"
    scopy_(l, &wd[1], &c__0, &wd[1], &c__1);

/*     THE ARRAYS IDOPE(*) AND DOPE(*) ARE USED TO PASS */
/*     INFORMATION TO WNLIT().  THIS WAS DONE TO AVOID */
/*     A LONG CALLING SEQUENCE OR THE USE OF COMMON. */
/*<   290 IDOPE(1) = ME >*/
#line 1820 "cl2.f"
L290:
#line 1820 "cl2.f"
    idope[0] = me;
/*<       IDOPE(2) = MEP1 >*/
#line 1821 "cl2.f"
    idope[1] = mep1;
/*<       IDOPE(3) = 0 >*/
#line 1822 "cl2.f"
    idope[2] = 0;
/*<       IDOPE(4) = 1 >*/
#line 1823 "cl2.f"
    idope[3] = 1;
/*<       IDOPE(5) = NSOLN >*/
#line 1824 "cl2.f"
    idope[4] = nsoln;
/*<       IDOPE(6) = 0 >*/
#line 1825 "cl2.f"
    idope[5] = 0;
/*<       IDOPE(7) = 1 >*/
#line 1826 "cl2.f"
    idope[6] = 1;
/*<       IDOPE(8) = L1 >*/
#line 1827 "cl2.f"
    idope[7] = l1;

/*<       DOPE(1) = ALSQ >*/
#line 1829 "cl2.f"
    dope[0] = alsq;
/*<       DOPE(2) = EANORM >*/
#line 1830 "cl2.f"
    dope[1] = eanorm;
/*<       DOPE(3) = FAC >*/
#line 1831 "cl2.f"
    dope[2] = fac;
/*<       DOPE(4) = TAU >*/
#line 1832 "cl2.f"
    dope[3] = tau;
/*<    >*/
#line 1833 "cl2.f"
    wnlit_(&w[w_offset], mdw, &m, n, l, &ipivot[1], &itype[1], &h__[1], &
	    scale[1], rnorm, idope, dope, &done);
/*<       ME = IDOPE(1) >*/
#line 1835 "cl2.f"
    me = idope[0];
/*<       MEP1 = IDOPE(2) >*/
#line 1836 "cl2.f"
    mep1 = idope[1];
/*<       KRANK = IDOPE(3) >*/
#line 1837 "cl2.f"
    krank = idope[2];
/*<       KRP1 = IDOPE(4) >*/
#line 1838 "cl2.f"
    krp1 = idope[3];
/*<       NSOLN = IDOPE(5) >*/
#line 1839 "cl2.f"
    nsoln = idope[4];
/*<       NIV = IDOPE(6) >*/
#line 1840 "cl2.f"
    niv = idope[5];
/*<       NIV1 = IDOPE(7) >*/
#line 1841 "cl2.f"
    niv1 = idope[6];
/*<       L1 = IDOPE(8) >*/
#line 1842 "cl2.f"
    l1 = idope[7];
/*<       GO TO 1240 >*/
#line 1843 "cl2.f"
    goto L1240;
/*<   300 CONTINUE >*/
#line 1844 "cl2.f"
L300:

/*     TO COMPUTE-SEARCH-DIRECTION-AND-FEASIBLE-POINT */

/*     SOLVE THE TRIANGULAR SYSTEM OF CURRENTLY NON-ACTIVE */
/*     VARIABLES AND STORE THE SOLUTION IN Z(*). */

/*     SOLVE-SYSTEM */
/*<       ASSIGN 310 TO IGO958 >*/
#line 1852 "cl2.f"
    igo958 = 0;
#line 1852 "cl2.f"
    igo958_fmt = fmt_310;
/*<       GO TO 1110 >*/
#line 1853 "cl2.f"
    goto L1110;

/*     INCREMENT ITERATION COUNTER AND CHECK AGAINST MAX. NUMBER */
/*     OF ITERATIONS. */
/*<   310 ITER = ITER + 1 >*/
#line 1857 "cl2.f"
L310:
#line 1857 "cl2.f"
    ++iter;
/*<       IF (.NOT.(ITER.GT.ITMAX)) GO TO 320 >*/
#line 1858 "cl2.f"
    if (! (iter > itmax)) {
#line 1858 "cl2.f"
	goto L320;
#line 1858 "cl2.f"
    }
/*<       MODE = 1 >*/
#line 1859 "cl2.f"
    *mode = 1;
/*<       DONE = .TRUE. >*/
#line 1860 "cl2.f"
    done = TRUE_;

/*     CHECK TO SEE IF ANY CONSTRAINTS HAVE BECOME ACTIVE. */
/*     IF SO, CALCULATE AN INTERPOLATION FACTOR SO THAT ALL */
/*     ACTIVE CONSTRAINTS ARE REMOVED FROM THE BASIS. */
/*<   320 ALPHA = TWO >*/
#line 1865 "cl2.f"
L320:
#line 1865 "cl2.f"
    alpha = two;
/*<       HITCON = .FALSE. >*/
#line 1866 "cl2.f"
    hitcon = FALSE_;
/*<       IF (.NOT.(L.LT.NSOLN)) GO TO 360 >*/
#line 1867 "cl2.f"
    if (! (*l < nsoln)) {
#line 1867 "cl2.f"
	goto L360;
#line 1867 "cl2.f"
    }
/*<       DO 350 J=LP1,NSOLN >*/
#line 1868 "cl2.f"
    i__1 = nsoln;
#line 1868 "cl2.f"
    for (j = lp1; j <= i__1; ++j) {
/*<         ZZ = Z(J) >*/
#line 1869 "cl2.f"
	zz = z__[j];
/*<         IF (.NOT.(ZZ.LE.ZERO)) GO TO 340 >*/
#line 1870 "cl2.f"
	if (! (zz <= zero)) {
#line 1870 "cl2.f"
	    goto L340;
#line 1870 "cl2.f"
	}
/*<         T = X(J)/(X(J)-ZZ) >*/
#line 1871 "cl2.f"
	t = x[j] / (x[j] - zz);
/*<         IF (.NOT.(T.LT.ALPHA)) GO TO 330 >*/
#line 1872 "cl2.f"
	if (! (t < alpha)) {
#line 1872 "cl2.f"
	    goto L330;
#line 1872 "cl2.f"
	}
/*<         ALPHA = T >*/
#line 1873 "cl2.f"
	alpha = t;
/*<         JCON = J >*/
#line 1874 "cl2.f"
	jcon = j;
/*<   330   HITCON = .TRUE. >*/
#line 1875 "cl2.f"
L330:
#line 1875 "cl2.f"
	hitcon = TRUE_;
/*<   340   CONTINUE >*/
#line 1876 "cl2.f"
L340:
/*<   350 CONTINUE >*/
#line 1877 "cl2.f"
/* L350: */
#line 1877 "cl2.f"
	;
#line 1877 "cl2.f"
    }
/*<   360 GO TO 1220 >*/
#line 1878 "cl2.f"
L360:
#line 1878 "cl2.f"
    goto L1220;
/*<   370 CONTINUE >*/
#line 1879 "cl2.f"
L370:

/*     TO ADD-CONSTRAINTS */

/*     USE COMPUTED ALPHA TO INTERPOLATE BETWEEN LAST */
/*     FEASIBLE SOLUTION X(*) AND CURRENT UNCONSTRAINED */
/*     (AND INFEASIBLE) SOLUTION Z(*). */
/*<       IF (.NOT.(LP1.LE.NSOLN)) GO TO 390 >*/
#line 1886 "cl2.f"
    if (! (lp1 <= nsoln)) {
#line 1886 "cl2.f"
	goto L390;
#line 1886 "cl2.f"
    }
/*<       DO 380 J=LP1,NSOLN >*/
#line 1887 "cl2.f"
    i__1 = nsoln;
#line 1887 "cl2.f"
    for (j = lp1; j <= i__1; ++j) {
/*<         X(J) = X(J) + ALPHA*(Z(J)-X(J)) >*/
#line 1888 "cl2.f"
	x[j] += alpha * (z__[j] - x[j]);
/*<   380 CONTINUE >*/
#line 1889 "cl2.f"
/* L380: */
#line 1889 "cl2.f"
    }
/*<   390 FEASBL = .FALSE. >*/
#line 1890 "cl2.f"
L390:
#line 1890 "cl2.f"
    feasbl = FALSE_;
/*<       GO TO 410 >*/
#line 1891 "cl2.f"
    goto L410;
/*<   400 IF (FEASBL) GO TO 610 >*/
#line 1892 "cl2.f"
L400:
#line 1892 "cl2.f"
    if (feasbl) {
#line 1892 "cl2.f"
	goto L610;
#line 1892 "cl2.f"
    }

/*     REMOVE COL JCON AND SHIFT COLS JCON+1 THROUGH N TO THE */
/*     LEFT. SWAP COL JCON INTO THE N-TH POSITION.  THIS ACHIEVES */
/*     UPPER HESSENBERG FORM FOR THE NONACTIVE CONSTRAINTS AND */
/*     LEAVES AN UPPER HESSENBERG MATRIX TO RETRIANGULARIZE. */
/*<   410 DO 420 I=1,M >*/
#line 1898 "cl2.f"
L410:
#line 1898 "cl2.f"
    i__1 = m;
#line 1898 "cl2.f"
    for (i__ = 1; i__ <= i__1; ++i__) {
/*<         T = W(I,JCON) >*/
#line 1899 "cl2.f"
	t = w[i__ + jcon * w_dim1];
/*<         CALL SCOPY(N-JCON, W(I,JCON+1), MDW, W(I,JCON), MDW) >*/
#line 1900 "cl2.f"
	i__2 = *n - jcon;
#line 1900 "cl2.f"
	scopy_(&i__2, &w[i__ + (jcon + 1) * w_dim1], mdw, &w[i__ + jcon *
		w_dim1], mdw);
/*<         W(I,N) = T >*/
#line 1901 "cl2.f"
	w[i__ + *n * w_dim1] = t;
/*<   420 CONTINUE >*/
#line 1902 "cl2.f"
/* L420: */
#line 1902 "cl2.f"
    }

/*     UPDATE PERMUTED INDEX VECTOR TO REFLECT THIS SHIFT AND SWAP. */
/*<       ITEMP = IPIVOT(JCON) >*/
#line 1905 "cl2.f"
    itemp = ipivot[jcon];
/*<       IF (.NOT.(JCON.LT.N)) GO TO 440 >*/
#line 1906 "cl2.f"
    if (! (jcon < *n)) {
#line 1906 "cl2.f"
	goto L440;
#line 1906 "cl2.f"
    }
/*<       DO 430 I=JCON,NM1 >*/
#line 1907 "cl2.f"
    i__1 = nm1;
#line 1907 "cl2.f"
    for (i__ = jcon; i__ <= i__1; ++i__) {
/*<         IPIVOT(I) = IPIVOT(I+1) >*/
#line 1908 "cl2.f"
	ipivot[i__] = ipivot[i__ + 1];
/*<   430 CONTINUE >*/
#line 1909 "cl2.f"
/* L430: */
#line 1909 "cl2.f"
    }
/*<   440 IPIVOT(N) = ITEMP >*/
#line 1910 "cl2.f"
L440:
#line 1910 "cl2.f"
    ipivot[*n] = itemp;

/*     SIMILARLY REPERMUTE X(*) VECTOR. */
/*<       CALL SCOPY(N-JCON, X(JCON+1), 1, X(JCON), 1) >*/
#line 1913 "cl2.f"
    i__1 = *n - jcon;
#line 1913 "cl2.f"
    scopy_(&i__1, &x[jcon + 1], &c__1, &x[jcon], &c__1);
/*<       X(N) = ZERO >*/
#line 1914 "cl2.f"
    x[*n] = zero;
/*<       NSP1 = NSOLN >*/
#line 1915 "cl2.f"
    nsp1 = nsoln;
/*<       NSOLN = NSOLN - 1 >*/
#line 1916 "cl2.f"
    --nsoln;
/*<       NIV1 = NIV >*/
#line 1917 "cl2.f"
    niv1 = niv;
/*<       NIV = NIV - 1 >*/
#line 1918 "cl2.f"
    --niv;

/*     RETRIANGULARIZE UPPER HESSENBERG MATRIX AFTER ADDING CONSTRAINTS. */
/*<       J = JCON >*/
#line 1921 "cl2.f"
    j = jcon;
/*<       I = KRANK + JCON - L >*/
#line 1922 "cl2.f"
    i__ = krank + jcon - *l;
/*<   450 IF (.NOT.(J.LE.NSOLN)) GO TO 570 >*/
#line 1923 "cl2.f"
L450:
#line 1923 "cl2.f"
    if (! (j <= nsoln)) {
#line 1923 "cl2.f"
	goto L570;
#line 1923 "cl2.f"
    }
/*<       IF (.NOT.(ITYPE(I).EQ.0 .AND. ITYPE(I+1).EQ.0)) GO TO 470 >*/
#line 1924 "cl2.f"
    if (! (itype[i__] == 0 && itype[i__ + 1] == 0)) {
#line 1924 "cl2.f"
	goto L470;
#line 1924 "cl2.f"
    }
/*<       ASSIGN 460 TO IGO938 >*/
#line 1925 "cl2.f"
    igo938 = 0;
#line 1925 "cl2.f"
    igo938_fmt = fmt_460;
/*<       GO TO 620 >*/
#line 1926 "cl2.f"
    goto L620;

/*     (ITYPE(I).EQ.0 .AND. ITYPE(I+1).EQ.0) ZERO-IP1-TO-I-IN-COL-J */
/*<   460 GO TO 560 >*/
#line 1929 "cl2.f"
L460:
#line 1929 "cl2.f"
    goto L560;
/*<   470 IF (.NOT.(ITYPE(I).EQ.1 .AND. ITYPE(I+1).EQ.1)) GO TO 490 >*/
#line 1930 "cl2.f"
L470:
#line 1930 "cl2.f"
    if (! (itype[i__] == 1 && itype[i__ + 1] == 1)) {
#line 1930 "cl2.f"
	goto L490;
#line 1930 "cl2.f"
    }
/*<       ASSIGN 480 TO IGO938 >*/
#line 1931 "cl2.f"
    igo938 = 1;
#line 1931 "cl2.f"
    igo938_fmt = fmt_480;
/*<       GO TO 620 >*/
#line 1932 "cl2.f"
    goto L620;

/*     (ITYPE(I).EQ.1 .AND. ITYPE(I+1).EQ.1) ZERO-IP1-TO-I-IN-COL-J */
/*<   480 GO TO 560 >*/
#line 1935 "cl2.f"
L480:
#line 1935 "cl2.f"
    goto L560;
/*<   490 IF (.NOT.(ITYPE(I).EQ.1 .AND. ITYPE(I+1).EQ.0)) GO TO 510 >*/
#line 1936 "cl2.f"
L490:
#line 1936 "cl2.f"
    if (! (itype[i__] == 1 && itype[i__ + 1] == 0)) {
#line 1936 "cl2.f"
	goto L510;
#line 1936 "cl2.f"
    }
/*<       CALL SSWAP(NP1, W(I,1), MDW, W(I+1,1), MDW) >*/
#line 1937 "cl2.f"
    sswap_(&np1, &w[i__ + w_dim1], mdw, &w[i__ + 1 + w_dim1], mdw);
/*<       CALL SSWAP(1, SCALE(I), 1, SCALE(I+1), 1) >*/
#line 1938 "cl2.f"
    sswap_(&c__1, &scale[i__], &c__1, &scale[i__ + 1], &c__1);
/*<       ITEMP = ITYPE(I+1) >*/
#line 1939 "cl2.f"
    itemp = itype[i__ + 1];
/*<       ITYPE(I+1) = ITYPE(I) >*/
#line 1940 "cl2.f"
    itype[i__ + 1] = itype[i__];
/*<       ITYPE(I) = ITEMP >*/
#line 1941 "cl2.f"
    itype[i__] = itemp;

/*     SWAPPED ROW WAS FORMERLY A PIVOT ELT., SO IT WILL */
/*     BE LARGE ENOUGH TO PERFORM ELIM. */
/*<       ASSIGN 500 TO IGO938 >*/
#line 1945 "cl2.f"
    igo938 = 2;
#line 1945 "cl2.f"
    igo938_fmt = fmt_500;
/*<       GO TO 620 >*/
#line 1946 "cl2.f"
    goto L620;

/*     ZERO-IP1-TO-I-IN-COL-J */
/*<   500 GO TO 560 >*/
#line 1949 "cl2.f"
L500:
#line 1949 "cl2.f"
    goto L560;
/*<   510 IF (.NOT.(ITYPE(I).EQ.0 .AND. ITYPE(I+1).EQ.1)) GO TO 550 >*/
#line 1950 "cl2.f"
L510:
#line 1950 "cl2.f"
    if (! (itype[i__] == 0 && itype[i__ + 1] == 1)) {
#line 1950 "cl2.f"
	goto L550;
#line 1950 "cl2.f"
    }
/*<       T = SCALE(I)*W(I,J)**2/ALSQ >*/
/* Computing 2nd power */
#line 1951 "cl2.f"
    r__1 = w[i__ + j * w_dim1];
#line 1951 "cl2.f"
    t = scale[i__] * (r__1 * r__1) / alsq;
/*<       IF (.NOT.(T.GT.TAU**2*EANORM**2)) GO TO 530 >*/
/* Computing 2nd power */
#line 1952 "cl2.f"
    r__1 = tau;
/* Computing 2nd power */
#line 1952 "cl2.f"
    r__2 = eanorm;
#line 1952 "cl2.f"
    if (! (t > r__1 * r__1 * (r__2 * r__2))) {
#line 1952 "cl2.f"
	goto L530;
#line 1952 "cl2.f"
    }
/*<       ASSIGN 520 TO IGO938 >*/
#line 1953 "cl2.f"
    igo938 = 3;
#line 1953 "cl2.f"
    igo938_fmt = fmt_520;
/*<       GO TO 620 >*/
#line 1954 "cl2.f"
    goto L620;
/*<   520 GO TO 540 >*/
#line 1955 "cl2.f"
L520:
#line 1955 "cl2.f"
    goto L540;
/*<   530 CALL SSWAP(NP1, W(I,1), MDW, W(I+1,1), MDW) >*/
#line 1956 "cl2.f"
L530:
#line 1956 "cl2.f"
    sswap_(&np1, &w[i__ + w_dim1], mdw, &w[i__ + 1 + w_dim1], mdw);
/*<       CALL SSWAP(1, SCALE(I), 1, SCALE(I+1), 1) >*/
#line 1957 "cl2.f"
    sswap_(&c__1, &scale[i__], &c__1, &scale[i__ + 1], &c__1);
/*<       ITEMP = ITYPE(I+1) >*/
#line 1958 "cl2.f"
    itemp = itype[i__ + 1];
/*<       ITYPE(I+1) = ITYPE(I) >*/
#line 1959 "cl2.f"
    itype[i__ + 1] = itype[i__];
/*<       ITYPE(I) = ITEMP >*/
#line 1960 "cl2.f"
    itype[i__] = itemp;
/*<       W(I+1,J) = ZERO >*/
#line 1961 "cl2.f"
    w[i__ + 1 + j * w_dim1] = zero;
/*<   540 CONTINUE >*/
#line 1962 "cl2.f"
L540:
/*<   550 CONTINUE >*/
#line 1963 "cl2.f"
L550:
/*<   560 I = I + 1 >*/
#line 1964 "cl2.f"
L560:
#line 1964 "cl2.f"
    ++i__;
/*<       J = J + 1 >*/
#line 1965 "cl2.f"
    ++j;
/*<       GO TO 450 >*/
#line 1966 "cl2.f"
    goto L450;

/*     SEE IF THE REMAINING COEFFS IN THE SOLN SET ARE FEASIBLE.  THEY */
/*     SHOULD BE BECAUSE OF THE WAY ALPHA WAS DETERMINED.  IF ANY ARE */
/*     INFEASIBLE IT IS DUE TO ROUNDOFF ERROR.  ANY THAT ARE NON- */
/*     POSITIVE WILL BE SET TO ZERO AND REMOVED FROM THE SOLN SET. */
/*<   570 IF (.NOT.(LP1.LE.NSOLN)) GO TO 590 >*/
#line 1972 "cl2.f"
L570:
#line 1972 "cl2.f"
    if (! (lp1 <= nsoln)) {
#line 1972 "cl2.f"
	goto L590;
#line 1972 "cl2.f"
    }
/*<       DO 580 JCON=LP1,NSOLN >*/
#line 1973 "cl2.f"
    i__1 = nsoln;
#line 1973 "cl2.f"
    for (jcon = lp1; jcon <= i__1; ++jcon) {
/*<         IF (X(JCON).LE.ZERO) GO TO 600 >*/
#line 1974 "cl2.f"
	if (x[jcon] <= zero) {
#line 1974 "cl2.f"
	    goto L600;
#line 1974 "cl2.f"
	}
/*<   580 CONTINUE >*/
#line 1975 "cl2.f"
/* L580: */
#line 1975 "cl2.f"
    }
/*<   590 FEASBL = .TRUE. >*/
#line 1976 "cl2.f"
L590:
#line 1976 "cl2.f"
    feasbl = TRUE_;
/*<   600 CONTINUE >*/
#line 1977 "cl2.f"
L600:
/*<       GO TO 400 >*/
#line 1978 "cl2.f"
    goto L400;
/*<   610 GO TO 1200 >*/
#line 1979 "cl2.f"
L610:
#line 1979 "cl2.f"
    goto L1200;
/*<   620 CONTINUE >*/
#line 1980 "cl2.f"
L620:

/*     TO ZERO-IP1-TO-I-IN-COL-J */
/*<       IF (.NOT.(W(I+1,J).NE.ZERO)) GO TO 630 >*/
#line 1983 "cl2.f"
    if (! (w[i__ + 1 + j * w_dim1] != zero)) {
#line 1983 "cl2.f"
	goto L630;
#line 1983 "cl2.f"
    }
/*<       CALL SROTMG(SCALE(I), SCALE(I+1), W(I,J), W(I+1,J), SPARAM) >*/
#line 1984 "cl2.f"
    srotmg_(&scale[i__], &scale[i__ + 1], &w[i__ + j * w_dim1], &w[i__ + 1 +
	    j * w_dim1], sparam);
/*<       W(I+1,J) = ZERO >*/
#line 1985 "cl2.f"
    w[i__ + 1 + j * w_dim1] = zero;
/*<       CALL SROTM(NP1-J, W(I,J+1), MDW, W(I+1,J+1), MDW, SPARAM) >*/
#line 1986 "cl2.f"
    i__1 = np1 - j;
#line 1986 "cl2.f"
    srotm_(&i__1, &w[i__ + (j + 1) * w_dim1], mdw, &w[i__ + 1 + (j + 1) *
	    w_dim1], mdw, sparam);
/*<   630 GO TO 1290 >*/
#line 1987 "cl2.f"
L630:
#line 1987 "cl2.f"
    goto L1290;
/*<   640 CONTINUE >*/
#line 1988 "cl2.f"
L640:

/*     TO PERFORM-MULTIPLIER-TEST-AND-DROP-A-CONSTRAINT */
/*<       CALL SCOPY(NSOLN, Z, 1, X, 1) >*/
#line 1991 "cl2.f"
    scopy_(&nsoln, &z__[1], &c__1, &x[1], &c__1);
/*<       IF (.NOT.(NSOLN.LT.N)) GO TO 650 >*/
#line 1992 "cl2.f"
    if (! (nsoln < *n)) {
#line 1992 "cl2.f"
	goto L650;
#line 1992 "cl2.f"
    }
/*<       X(NSP1) = ZERO >*/
#line 1993 "cl2.f"
    x[nsp1] = zero;
/*<       CALL SCOPY(N-NSOLN, X(NSP1), 0, X(NSP1), 1) >*/
#line 1994 "cl2.f"
    i__1 = *n - nsoln;
#line 1994 "cl2.f"
    scopy_(&i__1, &x[nsp1], &c__0, &x[nsp1], &c__1);
/*<   650 I = NIV1 >*/
#line 1995 "cl2.f"
L650:
#line 1995 "cl2.f"
    i__ = niv1;
/*<   660 IF (.NOT.(I.LE.ME)) GO TO 690 >*/
#line 1996 "cl2.f"
L660:
#line 1996 "cl2.f"
    if (! (i__ <= me)) {
#line 1996 "cl2.f"
	goto L690;
#line 1996 "cl2.f"
    }

/*     RECLASSIFY LEAST SQUARES EQATIONS AS EQUALITIES AS */
/*     NECESSARY. */
/*<       IF (.NOT.(ITYPE(I).EQ.0)) GO TO 670 >*/
#line 2000 "cl2.f"
    if (! (itype[i__] == 0)) {
#line 2000 "cl2.f"
	goto L670;
#line 2000 "cl2.f"
    }
/*<       I = I + 1 >*/
#line 2001 "cl2.f"
    ++i__;
/*<       GO TO 680 >*/
#line 2002 "cl2.f"
    goto L680;
/*<   670 CALL SSWAP(NP1, W(I,1), MDW, W(ME,1), MDW) >*/
#line 2003 "cl2.f"
L670:
#line 2003 "cl2.f"
    sswap_(&np1, &w[i__ + w_dim1], mdw, &w[me + w_dim1], mdw);
/*<       CALL SSWAP(1, SCALE(I), 1, SCALE(ME), 1) >*/
#line 2004 "cl2.f"
    sswap_(&c__1, &scale[i__], &c__1, &scale[me], &c__1);
/*<       ITEMP = ITYPE(I) >*/
#line 2005 "cl2.f"
    itemp = itype[i__];
/*<       ITYPE(I) = ITYPE(ME) >*/
#line 2006 "cl2.f"
    itype[i__] = itype[me];
/*<       ITYPE(ME) = ITEMP >*/
#line 2007 "cl2.f"
    itype[me] = itemp;
/*<       MEP1 = ME >*/
#line 2008 "cl2.f"
    mep1 = me;
/*<       ME = ME - 1 >*/
#line 2009 "cl2.f"
    --me;
/*<   680 GO TO 660 >*/
#line 2010 "cl2.f"
L680:
#line 2010 "cl2.f"
    goto L660;

/*     FORM INNER PRODUCT VECTOR WD(*) OF DUAL COEFFS. */
/*<   690 IF (.NOT.(NSP1.LE.N)) GO TO 730 >*/
#line 2013 "cl2.f"
L690:
#line 2013 "cl2.f"
    if (! (nsp1 <= *n)) {
#line 2013 "cl2.f"
	goto L730;
#line 2013 "cl2.f"
    }
/*<       DO 720 J=NSP1,N >*/
#line 2014 "cl2.f"
    i__1 = *n;
#line 2014 "cl2.f"
    for (j = nsp1; j <= i__1; ++j) {
/*<         SM = ZERO >*/
#line 2015 "cl2.f"
	sm = zero;
/*<         IF (.NOT.(NSOLN.LT.M)) GO TO 710 >*/
#line 2016 "cl2.f"
	if (! (nsoln < m)) {
#line 2016 "cl2.f"
	    goto L710;
#line 2016 "cl2.f"
	}
/*<         DO 700 I=NSP1,M >*/
#line 2017 "cl2.f"
	i__2 = m;
#line 2017 "cl2.f"
	for (i__ = nsp1; i__ <= i__2; ++i__) {
/*<           SM = SM + SCALE(I)*W(I,J)*W(I,NP1) >*/
#line 2018 "cl2.f"
	    sm += scale[i__] * w[i__ + j * w_dim1] * w[i__ + np1 * w_dim1];
/*<   700   CONTINUE >*/
#line 2019 "cl2.f"
/* L700: */
#line 2019 "cl2.f"
	}
/*<   710   WD(J) = SM >*/
#line 2020 "cl2.f"
L710:
#line 2020 "cl2.f"
	wd[j] = sm;
/*<   720 CONTINUE >*/
#line 2021 "cl2.f"
/* L720: */
#line 2021 "cl2.f"
    }
/*<   730 GO TO 750 >*/
#line 2022 "cl2.f"
L730:
#line 2022 "cl2.f"
    goto L750;
/*<   740 IF (POS .OR. DONE) GO TO 970 >*/
#line 2023 "cl2.f"
L740:
#line 2023 "cl2.f"
    if (pos || done) {
#line 2023 "cl2.f"
	goto L970;
#line 2023 "cl2.f"
    }

/*     FIND J SUCH THAT WD(J)=WMAX IS MAXIMUM.  THIS DETERMINES */
/*     THAT THE INCOMING COL J WILL REDUCE THE RESIDUAL VECTOR */
/*     AND BE POSITIVE. */
/*<   750 WMAX = ZERO >*/
#line 2028 "cl2.f"
L750:
#line 2028 "cl2.f"
    wmax = zero;
/*<       IWMAX = NSP1 >*/
#line 2029 "cl2.f"
    iwmax = nsp1;
/*<       IF (.NOT.(NSP1.LE.N)) GO TO 780 >*/
#line 2030 "cl2.f"
    if (! (nsp1 <= *n)) {
#line 2030 "cl2.f"
	goto L780;
#line 2030 "cl2.f"
    }
/*<       DO 770 J=NSP1,N >*/
#line 2031 "cl2.f"
    i__1 = *n;
#line 2031 "cl2.f"
    for (j = nsp1; j <= i__1; ++j) {
/*<         IF (.NOT.(WD(J).GT.WMAX)) GO TO 760 >*/
#line 2032 "cl2.f"
	if (! (wd[j] > wmax)) {
#line 2032 "cl2.f"
	    goto L760;
#line 2032 "cl2.f"
	}
/*<         WMAX = WD(J) >*/
#line 2033 "cl2.f"
	wmax = wd[j];
/*<         IWMAX = J >*/
#line 2034 "cl2.f"
	iwmax = j;
/*<   760   CONTINUE >*/
#line 2035 "cl2.f"
L760:
/*<   770 CONTINUE >*/
#line 2036 "cl2.f"
/* L770: */
#line 2036 "cl2.f"
	;
#line 2036 "cl2.f"
    }
/*<   780 IF (.NOT.(WMAX.LE.ZERO)) GO TO 790 >*/
#line 2037 "cl2.f"
L780:
#line 2037 "cl2.f"
    if (! (wmax <= zero)) {
#line 2037 "cl2.f"
	goto L790;
#line 2037 "cl2.f"
    }
/*<       DONE = .TRUE. >*/
#line 2038 "cl2.f"
    done = TRUE_;
/*<       GO TO 960 >*/
#line 2039 "cl2.f"
    goto L960;

/*     SET DUAL COEFF TO ZERO FOR INCOMING COL. */
/*<   790 WD(IWMAX) = ZERO >*/
#line 2042 "cl2.f"
L790:
#line 2042 "cl2.f"
    wd[iwmax] = zero;

/*     WMAX .GT. ZERO, SO OKAY TO MOVE COL IWMAX TO SOLN SET. */
/*     PERFORM TRANSFORMATION TO RETRIANGULARIZE, AND TEST */
/*     FOR NEAR LINEAR DEPENDENCE. */
/*     SWAP COL IWMAX INTO NSOLN-TH POSITION TO MAINTAIN UPPER */
/*     HESSENBERG FORM OF ADJACENT COLS, AND ADD NEW COL TO */
/*     TRIANGULAR DECOMPOSITION. */
/*<       NSOLN = NSP1 >*/
#line 2050 "cl2.f"
    nsoln = nsp1;
/*<       NSP1 = NSOLN + 1 >*/
#line 2051 "cl2.f"
    nsp1 = nsoln + 1;
/*<       NIV = NIV1 >*/
#line 2052 "cl2.f"
    niv = niv1;
/*<       NIV1 = NIV + 1 >*/
#line 2053 "cl2.f"
    niv1 = niv + 1;
/*<       IF (.NOT.(NSOLN.NE.IWMAX)) GO TO 800 >*/
#line 2054 "cl2.f"
    if (! (nsoln != iwmax)) {
#line 2054 "cl2.f"
	goto L800;
#line 2054 "cl2.f"
    }
/*<       CALL SSWAP(M, W(1,NSOLN), 1, W(1,IWMAX), 1) >*/
#line 2055 "cl2.f"
    sswap_(&m, &w[nsoln * w_dim1 + 1], &c__1, &w[iwmax * w_dim1 + 1], &c__1);
/*<       WD(IWMAX) = WD(NSOLN) >*/
#line 2056 "cl2.f"
    wd[iwmax] = wd[nsoln];
/*<       WD(NSOLN) = ZERO >*/
#line 2057 "cl2.f"
    wd[nsoln] = zero;
/*<       ITEMP = IPIVOT(NSOLN) >*/
#line 2058 "cl2.f"
    itemp = ipivot[nsoln];
/*<       IPIVOT(NSOLN) = IPIVOT(IWMAX) >*/
#line 2059 "cl2.f"
    ipivot[nsoln] = ipivot[iwmax];
/*<       IPIVOT(IWMAX) = ITEMP >*/
#line 2060 "cl2.f"
    ipivot[iwmax] = itemp;

/*     REDUCE COL NSOLN SO THAT THE MATRIX OF NONACTIVE */
/*     CONSTRAINTS VARIABLES IS TRIANGULAR. */
/*<   800 J = M >*/
#line 2064 "cl2.f"
L800:
#line 2064 "cl2.f"
    j = m;
/*<   810 IF (.NOT.(J.GT.NIV)) GO TO 870 >*/
#line 2065 "cl2.f"
L810:
#line 2065 "cl2.f"
    if (! (j > niv)) {
#line 2065 "cl2.f"
	goto L870;
#line 2065 "cl2.f"
    }
/*<       JM1 = J - 1 >*/
#line 2066 "cl2.f"
    jm1 = j - 1;
/*<       JP = JM1 >*/
#line 2067 "cl2.f"
    jp = jm1;

/*     WHEN OPERATING NEAR THE ME LINE, TEST TO SEE IF THE PIVOT ELT. */
/*     IS NEAR ZERO.  IF SO, USE THE LARGEST ELT. ABOVE IT AS THE PIVOT. */
/*     THIS IS TO MAINTAIN THE SHARP INTERFACE BETWEEN WEIGHTED AND */
/*     NON-WEIGHTED ROWS IN ALL CASES. */
/*<       IF (.NOT.(J.EQ.MEP1)) GO TO 850 >*/
#line 2073 "cl2.f"
    if (! (j == mep1)) {
#line 2073 "cl2.f"
	goto L850;
#line 2073 "cl2.f"
    }
/*<       IMAX = ME >*/
#line 2074 "cl2.f"
    imax = me;
/*<       AMAX = SCALE(ME)*W(ME,NSOLN)**2 >*/
/* Computing 2nd power */
#line 2075 "cl2.f"
    r__1 = w[me + nsoln * w_dim1];
#line 2075 "cl2.f"
    amax = scale[me] * (r__1 * r__1);
/*<   820 IF (.NOT.(JP.GE.NIV)) GO TO 840 >*/
#line 2076 "cl2.f"
L820:
#line 2076 "cl2.f"
    if (! (jp >= niv)) {
#line 2076 "cl2.f"
	goto L840;
#line 2076 "cl2.f"
    }
/*<       T = SCALE(JP)*W(JP,NSOLN)**2 >*/
/* Computing 2nd power */
#line 2077 "cl2.f"
    r__1 = w[jp + nsoln * w_dim1];
#line 2077 "cl2.f"
    t = scale[jp] * (r__1 * r__1);
/*<       IF (.NOT.(T.GT.AMAX)) GO TO 830 >*/
#line 2078 "cl2.f"
    if (! (t > amax)) {
#line 2078 "cl2.f"
	goto L830;
#line 2078 "cl2.f"
    }
/*<       IMAX = JP >*/
#line 2079 "cl2.f"
    imax = jp;
/*<       AMAX = T >*/
#line 2080 "cl2.f"
    amax = t;
/*<   830 JP = JP - 1 >*/
#line 2081 "cl2.f"
L830:
#line 2081 "cl2.f"
    --jp;
/*<       GO TO 820 >*/
#line 2082 "cl2.f"
    goto L820;
/*<   840 JP = IMAX >*/
#line 2083 "cl2.f"
L840:
#line 2083 "cl2.f"
    jp = imax;
/*<   850 IF (.NOT.(W(J,NSOLN).NE.ZERO)) GO TO 860 >*/
#line 2084 "cl2.f"
L850:
#line 2084 "cl2.f"
    if (! (w[j + nsoln * w_dim1] != zero)) {
#line 2084 "cl2.f"
	goto L860;
#line 2084 "cl2.f"
    }
/*<       CALL SROTMG(SCALE(JP), SCALE(J), W(JP,NSOLN), W(J,NSOLN), SPARAM) >*/
#line 2085 "cl2.f"
    srotmg_(&scale[jp], &scale[j], &w[jp + nsoln * w_dim1], &w[j + nsoln *
	    w_dim1], sparam);
/*<       W(J,NSOLN) = ZERO >*/
#line 2086 "cl2.f"
    w[j + nsoln * w_dim1] = zero;
/*<       CALL SROTM(NP1-NSOLN, W(JP,NSP1), MDW, W(J,NSP1), MDW, SPARAM) >*/
#line 2087 "cl2.f"
    i__1 = np1 - nsoln;
#line 2087 "cl2.f"
    srotm_(&i__1, &w[jp + nsp1 * w_dim1], mdw, &w[j + nsp1 * w_dim1], mdw,
	    sparam);
/*<   860 J = JM1 >*/
#line 2088 "cl2.f"
L860:
#line 2088 "cl2.f"
    j = jm1;
/*<       GO TO 810 >*/
#line 2089 "cl2.f"
    goto L810;

/*     SOLVE FOR Z(NSOLN)=PROPOSED NEW VALUE FOR X(NSOLN). */
/*     TEST IF THIS IS NONPOSITIVE OR TOO LARGE. */
/*     IF THIS WAS TRUE OR IF THE PIVOT TERM WAS ZERO REJECT */
/*     THE COL AS DEPENDENT. */
/*<   870 IF (.NOT.(W(NIV,NSOLN).NE.ZERO)) GO TO 890 >*/
#line 2095 "cl2.f"
L870:
#line 2095 "cl2.f"
    if (! (w[niv + nsoln * w_dim1] != zero)) {
#line 2095 "cl2.f"
	goto L890;
#line 2095 "cl2.f"
    }
/*<       ISOL = NIV >*/
#line 2096 "cl2.f"
    isol = niv;
/*<       ASSIGN 880 TO IGO897 >*/
#line 2097 "cl2.f"
    igo897 = 0;
#line 2097 "cl2.f"
    igo897_fmt = fmt_880;
/*<       GO TO 980 >*/
#line 2098 "cl2.f"
    goto L980;

/*     TEST-PROPOSED-NEW-COMPONENT */
/*<   880 GO TO 940 >*/
#line 2101 "cl2.f"
L880:
#line 2101 "cl2.f"
    goto L940;
/*<   890 IF (.NOT.(NIV.LE.ME .AND. W(MEP1,NSOLN).NE.ZERO)) GO TO 920 >*/
#line 2102 "cl2.f"
L890:
#line 2102 "cl2.f"
    if (! (niv <= me && w[mep1 + nsoln * w_dim1] != zero)) {
#line 2102 "cl2.f"
	goto L920;
#line 2102 "cl2.f"
    }

/*     TRY TO ADD ROW MEP1 AS AN ADDITIONAL EQUALITY CONSTRAINT. */
/*     CHECK SIZE OF PROPOSED NEW SOLN COMPONENT. */
/*     REJECT IT IF IT IS TOO LARGE. */
/*<       ISOL = MEP1 >*/
#line 2107 "cl2.f"
    isol = mep1;
/*<       ASSIGN 900 TO IGO897 >*/
#line 2108 "cl2.f"
    igo897 = 1;
#line 2108 "cl2.f"
    igo897_fmt = fmt_900;
/*<       GO TO 980 >*/
#line 2109 "cl2.f"
    goto L980;

/*     TEST-PROPOSED-NEW-COMPONENT */
/*<   900 IF (.NOT.(POS)) GO TO 910 >*/
#line 2112 "cl2.f"
L900:
#line 2112 "cl2.f"
    if (! pos) {
#line 2112 "cl2.f"
	goto L910;
#line 2112 "cl2.f"
    }

/*     SWAP ROWS MEP1 AND NIV, AND SCALE FACTORS FOR THESE ROWS. */
/*<       CALL SSWAP(NP1, W(MEP1,1), MDW, W(NIV,1), MDW) >*/
#line 2115 "cl2.f"
    sswap_(&np1, &w[mep1 + w_dim1], mdw, &w[niv + w_dim1], mdw);
/*<       CALL SSWAP(1, SCALE(MEP1), 1, SCALE(NIV), 1) >*/
#line 2116 "cl2.f"
    sswap_(&c__1, &scale[mep1], &c__1, &scale[niv], &c__1);
/*<       ITEMP = ITYPE(MEP1) >*/
#line 2117 "cl2.f"
    itemp = itype[mep1];
/*<       ITYPE(MEP1) = ITYPE(NIV) >*/
#line 2118 "cl2.f"
    itype[mep1] = itype[niv];
/*<       ITYPE(NIV) = ITEMP >*/
#line 2119 "cl2.f"
    itype[niv] = itemp;
/*<       ME = MEP1 >*/
#line 2120 "cl2.f"
    me = mep1;
/*<       MEP1 = ME + 1 >*/
#line 2121 "cl2.f"
    mep1 = me + 1;
/*<   910 GO TO 930 >*/
#line 2122 "cl2.f"
L910:
#line 2122 "cl2.f"
    goto L930;
/*<   920 POS = .FALSE. >*/
#line 2123 "cl2.f"
L920:
#line 2123 "cl2.f"
    pos = FALSE_;
/*<   930 CONTINUE >*/
#line 2124 "cl2.f"
L930:
/*<   940 IF (POS) GO TO 950 >*/
#line 2125 "cl2.f"
L940:
#line 2125 "cl2.f"
    if (pos) {
#line 2125 "cl2.f"
	goto L950;
#line 2125 "cl2.f"
    }
/*<       NSP1 = NSOLN >*/
#line 2126 "cl2.f"
    nsp1 = nsoln;
/*<       NSOLN = NSOLN - 1 >*/
#line 2127 "cl2.f"
    --nsoln;
/*<       NIV1 = NIV >*/
#line 2128 "cl2.f"
    niv1 = niv;
/*<       NIV = NIV - 1 >*/
#line 2129 "cl2.f"
    --niv;
/*<   950 CONTINUE >*/
#line 2130 "cl2.f"
L950:
/*<   960 GO TO 740 >*/
#line 2131 "cl2.f"
L960:
#line 2131 "cl2.f"
    goto L740;
/*<   970 GO TO 1250 >*/
#line 2132 "cl2.f"
L970:
#line 2132 "cl2.f"
    goto L1250;
/*<   980 CONTINUE >*/
#line 2133 "cl2.f"
L980:

/*     TO TEST-PROPOSED-NEW-COMPONENT */
/*<       Z2 = W(ISOL,NP1)/W(ISOL,NSOLN) >*/
#line 2136 "cl2.f"
    z2 = w[isol + np1 * w_dim1] / w[isol + nsoln * w_dim1];
/*<       Z(NSOLN) = Z2 >*/
#line 2137 "cl2.f"
    z__[nsoln] = z2;
/*<       POS = Z2.GT.ZERO >*/
#line 2138 "cl2.f"
    pos = z2 > zero;
/*<       IF (.NOT.(Z2*EANORM.GE.BNORM .AND. POS)) GO TO 990 >*/
#line 2139 "cl2.f"
    if (! (z2 * eanorm >= bnorm && pos)) {
#line 2139 "cl2.f"
	goto L990;
#line 2139 "cl2.f"
    }
/*<       POS = .NOT.(BLOWUP*Z2*EANORM.GE.BNORM) >*/
#line 2140 "cl2.f"
    pos = ! (blowup * z2 * eanorm >= bnorm);
/*<   990 GO TO 1280 >*/
#line 2141 "cl2.f"
L990:
#line 2141 "cl2.f"
    goto L1280;
/*<  1000 CONTINUE >*/
#line 2142 "cl2.f"
L1000:
/*     TO COMPUTE-FINAL-SOLUTION */

/*     SOLVE SYSTEM, STORE RESULTS IN X(*). */

/*<       ASSIGN 1010 TO IGO958 >*/
#line 2147 "cl2.f"
    igo958 = 1;
#line 2147 "cl2.f"
    igo958_fmt = fmt_1010;
/*<       GO TO 1110 >*/
#line 2148 "cl2.f"
    goto L1110;
/*     SOLVE-SYSTEM */
/*<  1010 CALL SCOPY(NSOLN, Z, 1, X, 1) >*/
#line 2150 "cl2.f"
L1010:
#line 2150 "cl2.f"
    scopy_(&nsoln, &z__[1], &c__1, &x[1], &c__1);

/*     APPLY HOUSEHOLDER TRANSFORMATIONS TO X(*) IF KRANK.LT.L */
/*<       IF (.NOT.(0.LT.KRANK .AND. KRANK.LT.L)) GO TO 1030 >*/
#line 2153 "cl2.f"
    if (! (0 < krank && krank < *l)) {
#line 2153 "cl2.f"
	goto L1030;
#line 2153 "cl2.f"
    }
/*<       DO 1020 I=1,KRANK >*/
#line 2154 "cl2.f"
    i__1 = krank;
#line 2154 "cl2.f"
    for (i__ = 1; i__ <= i__1; ++i__) {
/*<         CALL H12(2, I, KRP1, L, W(I,1), MDW, H(I), X, 1, 1, 1) >*/
#line 2155 "cl2.f"
	h12_(&c__2, &i__, &krp1, l, &w[i__ + w_dim1], mdw, &h__[i__], &x[1], &
		c__1, &c__1, &c__1);
/*<  1020 CONTINUE >*/
#line 2156 "cl2.f"
/* L1020: */
#line 2156 "cl2.f"
    }

/*     FILL IN TRAILING ZEROES FOR CONSTRAINED VARIABLES NOT IN SOLN. */
/*<  1030 IF (.NOT.(NSOLN.LT.N)) GO TO 1040 >*/
#line 2159 "cl2.f"
L1030:
#line 2159 "cl2.f"
    if (! (nsoln < *n)) {
#line 2159 "cl2.f"
	goto L1040;
#line 2159 "cl2.f"
    }
/*<       X(NSP1) = ZERO >*/
#line 2160 "cl2.f"
    x[nsp1] = zero;
/*<       CALL SCOPY(N-NSOLN, X(NSP1), 0, X(NSP1), 1) >*/
#line 2161 "cl2.f"
    i__1 = *n - nsoln;
#line 2161 "cl2.f"
    scopy_(&i__1, &x[nsp1], &c__0, &x[nsp1], &c__1);

/*     REPERMUTE SOLN VECTOR TO NATURAL ORDER. */
/*<  1040 DO 1070 I=1,N >*/
#line 2164 "cl2.f"
L1040:
#line 2164 "cl2.f"
    i__1 = *n;
#line 2164 "cl2.f"
    for (i__ = 1; i__ <= i__1; ++i__) {
/*<         J = I >*/
#line 2165 "cl2.f"
	j = i__;
/*<  1050   IF (IPIVOT(J).EQ.I) GO TO 1060 >*/
#line 2166 "cl2.f"
L1050:
#line 2166 "cl2.f"
	if (ipivot[j] == i__) {
#line 2166 "cl2.f"
	    goto L1060;
#line 2166 "cl2.f"
	}
/*<         J = J + 1 >*/
#line 2167 "cl2.f"
	++j;
/*<         GO TO 1050 >*/
#line 2168 "cl2.f"
	goto L1050;
/*<  1060   IPIVOT(J) = IPIVOT(I) >*/
#line 2169 "cl2.f"
L1060:
#line 2169 "cl2.f"
	ipivot[j] = ipivot[i__];
/*<         IPIVOT(I) = J >*/
#line 2170 "cl2.f"
	ipivot[i__] = j;
/*<         CALL SSWAP(1, X(J), 1, X(I), 1) >*/
#line 2171 "cl2.f"
	sswap_(&c__1, &x[j], &c__1, &x[i__], &c__1);
/*<  1070 CONTINUE >*/
#line 2172 "cl2.f"
/* L1070: */
#line 2172 "cl2.f"
    }

/*     RESCALE THE SOLN USING THE COL SCALING. */
/*<       DO 1080 J=1,N >*/
#line 2175 "cl2.f"
    i__1 = *n;
#line 2175 "cl2.f"
    for (j = 1; j <= i__1; ++j) {
/*<         X(J) = X(J)*D(J) >*/
#line 2176 "cl2.f"
	x[j] *= d__[j];
/*<  1080 CONTINUE >*/
#line 2177 "cl2.f"
/* L1080: */
#line 2177 "cl2.f"
    }
/*     IF (.NOT.(NSOLN.LT.M)) GO TO 1100                           REMK */
/*     DO 1090 I=NSP1,M                                            REMK */
/*<       IF (.NOT.(NIV.LT.M)) GO TO 1100 >*/
#line 2180 "cl2.f"
    if (! (niv < m)) {
#line 2180 "cl2.f"
	goto L1100;
#line 2180 "cl2.f"
    }
/*<       DO 1090 I = NIV1,M >*/
#line 2181 "cl2.f"
    i__1 = m;
#line 2181 "cl2.f"
    for (i__ = niv1; i__ <= i__1; ++i__) {
/*<         T = W(I,NP1) >*/
#line 2182 "cl2.f"
	t = w[i__ + np1 * w_dim1];
/*<         IF (I.LE.ME) T = T/ALAMDA >*/
#line 2183 "cl2.f"
	if (i__ <= me) {
#line 2183 "cl2.f"
	    t /= alamda;
#line 2183 "cl2.f"
	}
/*<         T = (SCALE(I)*T)*T >*/
#line 2184 "cl2.f"
	t = scale[i__] * t * t;
/*<         RNORM = RNORM + T >*/
#line 2185 "cl2.f"
	*rnorm += t;
/*<  1090 CONTINUE >*/
#line 2186 "cl2.f"
/* L1090: */
#line 2186 "cl2.f"
    }
/*<  1100 RNORM = SQRT(RNORM) >*/
#line 2187 "cl2.f"
L1100:
#line 2187 "cl2.f"
    *rnorm = sqrt(*rnorm);
/*<       GO TO 1210 >*/
#line 2188 "cl2.f"
    goto L1210;

/*     TO SOLVE-SYSTEM */

/*<  1110 CONTINUE >*/
#line 2192 "cl2.f"
L1110:
/*<       IF (.NOT.(DONE)) GO TO 1120 >*/
#line 2193 "cl2.f"
    if (! done) {
#line 2193 "cl2.f"
	goto L1120;
#line 2193 "cl2.f"
    }
/*<       ISOL = 1 >*/
#line 2194 "cl2.f"
    isol = 1;
/*<       GO TO 1130 >*/
#line 2195 "cl2.f"
    goto L1130;
/*<  1120 ISOL = LP1 >*/
#line 2196 "cl2.f"
L1120:
#line 2196 "cl2.f"
    isol = lp1;
/*<  1130 IF (.NOT.(NSOLN.GE.ISOL)) GO TO 1190 >*/
#line 2197 "cl2.f"
L1130:
#line 2197 "cl2.f"
    if (! (nsoln >= isol)) {
#line 2197 "cl2.f"
	goto L1190;
#line 2197 "cl2.f"
    }

/*     COPY RT. HAND SIDE INTO TEMP VECTOR TO USE OVERWRITING METHOD. */
/*<       CALL SCOPY(NIV, W(1,NP1), 1, TEMP, 1) >*/
#line 2200 "cl2.f"
    scopy_(&niv, &w[np1 * w_dim1 + 1], &c__1, &temp[1], &c__1);
/*<       DO 1180 JJ=ISOL,NSOLN >*/
#line 2201 "cl2.f"
    i__1 = nsoln;
#line 2201 "cl2.f"
    for (jj = isol; jj <= i__1; ++jj) {
/*<         J = NSOLN - JJ + ISOL >*/
#line 2202 "cl2.f"
	j = nsoln - jj + isol;
/*<         IF (.NOT.(J.GT.KRANK)) GO TO 1140 >*/
#line 2203 "cl2.f"
	if (! (j > krank)) {
#line 2203 "cl2.f"
	    goto L1140;
#line 2203 "cl2.f"
	}
/*<         I = NIV - JJ + ISOL >*/
#line 2204 "cl2.f"
	i__ = niv - jj + isol;
/*<         GO TO 1150 >*/
#line 2205 "cl2.f"
	goto L1150;
/*<  1140   I = J >*/
#line 2206 "cl2.f"
L1140:
#line 2206 "cl2.f"
	i__ = j;
/*<  1150   IF (.NOT.(J.GT.KRANK .AND. J.LE.L)) GO TO 1160 >*/
#line 2207 "cl2.f"
L1150:
#line 2207 "cl2.f"
	if (! (j > krank && j <= *l)) {
#line 2207 "cl2.f"
	    goto L1160;
#line 2207 "cl2.f"
	}
/*<         Z(J) = ZERO >*/
#line 2208 "cl2.f"
	z__[j] = zero;
/*<         GO TO 1170 >*/
#line 2209 "cl2.f"
	goto L1170;
/*<  1160   Z(J) = TEMP(I)/W(I,J) >*/
#line 2210 "cl2.f"
L1160:
#line 2210 "cl2.f"
	z__[j] = temp[i__] / w[i__ + j * w_dim1];
/*<         CALL SAXPY(I-1, -Z(J), W(1,J), 1, TEMP, 1) >*/
#line 2211 "cl2.f"
	i__2 = i__ - 1;
#line 2211 "cl2.f"
	r__1 = -z__[j];
#line 2211 "cl2.f"
	saxpy_(&i__2, &r__1, &w[j * w_dim1 + 1], &c__1, &temp[1], &c__1);
/*<  1170   CONTINUE >*/
#line 2212 "cl2.f"
L1170:
/*<  1180 CONTINUE >*/
#line 2213 "cl2.f"
/* L1180: */
#line 2213 "cl2.f"
	;
#line 2213 "cl2.f"
    }
/*<  1190 GO TO 1270 >*/
#line 2214 "cl2.f"
L1190:
#line 2214 "cl2.f"
    goto L1270;
/*<  1200 GO TO IGO986, (40) >*/
#line 2215 "cl2.f"
L1200:
#line 2215 "cl2.f"
    switch (igo986) {
#line 2215 "cl2.f"
	case 0: goto L40;
#line 2215 "cl2.f"
    }
/*<  1210 GO TO IGO980, (90) >*/
#line 2216 "cl2.f"
L1210:
#line 2216 "cl2.f"
    switch (igo980) {
#line 2216 "cl2.f"
	case 0: goto L90;
#line 2216 "cl2.f"
    }
/*<  1220 GO TO IGO991, (30) >*/
#line 2217 "cl2.f"
L1220:
#line 2217 "cl2.f"
    switch (igo991) {
#line 2217 "cl2.f"
	case 0: goto L30;
#line 2217 "cl2.f"
    }
/*<  1230 GO TO IGO998, (10) >*/
#line 2218 "cl2.f"
L1230:
#line 2218 "cl2.f"
    switch (igo998) {
#line 2218 "cl2.f"
	case 0: goto L10;
#line 2218 "cl2.f"
    }
/*<  1240 GO TO IGO995, (20) >*/
#line 2219 "cl2.f"
L1240:
#line 2219 "cl2.f"
    switch (igo995) {
#line 2219 "cl2.f"
	case 0: goto L20;
#line 2219 "cl2.f"
    }
/*<  1250 GO TO IGO983, (60) >*/
#line 2220 "cl2.f"
L1250:
#line 2220 "cl2.f"
    switch (igo983) {
#line 2220 "cl2.f"
	case 0: goto L60;
#line 2220 "cl2.f"
    }
/*<  1260 GO TO IGO977, (220) >*/
#line 2221 "cl2.f"
L1260:
#line 2221 "cl2.f"
    switch (igo977) {
#line 2221 "cl2.f"
	case 0: goto L220;
#line 2221 "cl2.f"
    }
/*<  1270 GO TO IGO958, (310, 1010) >*/
#line 2222 "cl2.f"
L1270:
#line 2222 "cl2.f"
    switch (igo958) {
#line 2222 "cl2.f"
	case 0: goto L310;
#line 2222 "cl2.f"
	case 1: goto L1010;
#line 2222 "cl2.f"
    }
/*<  1280 GO TO IGO897, (880, 900) >*/
#line 2223 "cl2.f"
L1280:
#line 2223 "cl2.f"
    switch (igo897) {
#line 2223 "cl2.f"
	case 0: goto L880;
#line 2223 "cl2.f"
	case 1: goto L900;
#line 2223 "cl2.f"
    }
/*<  1290 GO TO IGO938, (460, 480, 500, 520) >*/
#line 2224 "cl2.f"
L1290:
#line 2224 "cl2.f"
    switch (igo938) {
#line 2224 "cl2.f"
	case 0: goto L460;
#line 2224 "cl2.f"
	case 1: goto L480;
#line 2224 "cl2.f"
	case 2: goto L500;
#line 2224 "cl2.f"
	case 3: goto L520;
#line 2224 "cl2.f"
    }
  return 0 ;
/*<       END >*/
} /* wnlsm_ */

/* CC */
/* CC---------------------------------------------------------------------------- */
/* CC */
/*<    >*/
static /* Subroutine */ int wnlit_(real *w, integer *mdw, integer *m, integer *n,
	integer *l, integer *ipivot, integer *itype, real *h__, real *scale,
	real *rnorm, integer *idope, real *dope, logical *done)
{
    /* Initialized data */

    static real tenm3 = .001f;
    static real zero = 0.f;
    static real one = 1.f;

    /* Format strings */
    static char fmt_20[] = "";
    static char fmt_30[] = "";
    static char fmt_40[] = "";
    static char fmt_120[] = "";
    static char fmt_190[] = "";
    static char fmt_200[] = "";
    static char fmt_240[] = "";
    static char fmt_320[] = "";
    static char fmt_330[] = "";

    /* System generated locals */
    integer w_dim1, w_offset, i__1, i__2;
    real r__1, r__2, r__3;

    /* Builtin functions */
    double sqrt(doublereal);

    /* Local variables */
    static integer i__, j, k;
    static real t;
    static integer i1, j1, l1;
    static integer ic, lb, me, jj, kk, jp, ir;
    static real rn, sn;
    static integer jm1, ip1, lp1, np1, max__;
    static real tau;
    static integer niv, mep1, irp1, krp1, niv1;
    static real hbar;
    static integer lend, mend;
    static real alsq;
    static integer igo990, igo993, igo996;
    static logical indep;
    static integer krank, itemp, nsoln;
    static logical recalc;
    static real factor, eanorm;
    static real sparam[5];

    /* Assigned format variables */
    static char *igo996_fmt, *igo993_fmt, *igo990_fmt;


/*     THE EDITING REQUIRED TO CONVERT THIS SUBROUTINE FROM SINGLE TO */
/*     DOUBLE PRECISION INVOLVES THE FOLLOWING CHARACTER STRING CHANGES. */
/*     USE AN EDITING COMMAND (CHANGE) /STRING-1/(TO)STRING-2/. */
/*     (BEGIN CHANGES AT LINE WITH C++ IN COLS. 1-3.) */
/*     /REAL (12 BLANKS)/DOUBLE PRECISION/,/SCOPY/DCOPY/,/SROTM/DROTM/, */
/*     /SSCAL/DSCAL/,/SQRT/DSQRT/,                                  REMK */
/*     /SSWAP/DSWAP/,/AMAX1/DMAX1/,/ISAMAX/IDAMAX/,/.E-/.D-/,/E0/D0/ */

/*     THIS IS A COMPANION SUBPROGRAM TO WNNLS( ). */
/*     THE DOCUMENTATION FOR WNNLS( ) HAS MORE COMPLETE */
/*     USAGE INSTRUCTIONS. */

/*     NOTE  THE M BY (N+1) MATRIX W( , ) CONTAINS THE RT. HAND SIDE */
/*           B AS THE (N+1)ST COL. */


/*     TRIANGULARIZE L1 BY L1 SUBSYSTEM, WHERE L1=MIN(M,L), WITH */
/*     COL INTERCHANGES. */
/*     REVISED MARCH 4, 1982 */

/* ++ */
/*<       REAL             W(MDW,1), H(1), SCALE(1), DOPE(4), SPARAM(5) >*/
/*     REAL             ALSQ, AMAX, EANORM, FAC, FACTOR, HBAR, ONE, RN */
/*<       REAL             ALSQ, EANORM, FACTOR, HBAR, ONE, RN >*/
/*<       REAL             RNORM, SN, T, TAU, TENM3, ZERO >*/
/*<       REAL             AMAX1 >*/
/*<       INTEGER ITYPE(1), IPIVOT(1), IDOPE(8) >*/
/*<       INTEGER ISAMAX >*/
/*<       LOGICAL INDEP, DONE, RECALC >*/
/*<       DATA TENM3 /1.E-3/, ZERO /0.E0/, ONE /1.E0/ >*/
#line 2261 "cl2.f"
    /* Parameter adjustments */
#line 2261 "cl2.f"
    w_dim1 = *mdw;
#line 2261 "cl2.f"
    w_offset = 1 + w_dim1;
#line 2261 "cl2.f"
    w -= w_offset;
#line 2261 "cl2.f"
    --ipivot;
#line 2261 "cl2.f"
    --itype;
#line 2261 "cl2.f"
    --h__;
#line 2261 "cl2.f"
    --scale;
#line 2261 "cl2.f"
    --idope;
#line 2261 "cl2.f"
    --dope;
#line 2261 "cl2.f"

#line 2261 "cl2.f"
    /* Function Body */

/*<       ME = IDOPE(1) >*/
#line 2263 "cl2.f"
    me = idope[1];
/*<       MEP1 = IDOPE(2) >*/
#line 2264 "cl2.f"
    mep1 = idope[2];
/*<       KRANK = IDOPE(3) >*/
#line 2265 "cl2.f"
    krank = idope[3];
/*<       KRP1 = IDOPE(4) >*/
#line 2266 "cl2.f"
    krp1 = idope[4];
/*<       NSOLN = IDOPE(5) >*/
#line 2267 "cl2.f"
    nsoln = idope[5];
/*<       NIV = IDOPE(6) >*/
#line 2268 "cl2.f"
    niv = idope[6];
/*<       NIV1 = IDOPE(7) >*/
#line 2269 "cl2.f"
    niv1 = idope[7];
/*<       L1 = IDOPE(8) >*/
#line 2270 "cl2.f"
    l1 = idope[8];

/*<       ALSQ = DOPE(1) >*/
#line 2272 "cl2.f"
    alsq = dope[1];
/*<       EANORM = DOPE(2) >*/
#line 2273 "cl2.f"
    eanorm = dope[2];
/*     FAC = DOPE(3)                                               REMK */
/*<       TAU = DOPE(4) >*/
#line 2275 "cl2.f"
    tau = dope[4];
/*<       NP1 = N + 1 >*/
#line 2276 "cl2.f"
    np1 = *n + 1;
/*<       LB = MIN0(M-1,L) >*/
/* Computing MIN */
#line 2277 "cl2.f"
    i__1 = *m - 1;
#line 2277 "cl2.f"
    lb = min(i__1,*l);
/*<       RECALC = .TRUE. >*/
#line 2278 "cl2.f"
    recalc = TRUE_;
/*<       RNORM = ZERO >*/
#line 2279 "cl2.f"
    *rnorm = zero;
/*<       KRANK = 0 >*/
#line 2280 "cl2.f"
    krank = 0;
/*     WE SET FACTOR=1.E0 SO THAT THE HEAVY WEIGHT ALAMDA WILL BE */
/*     INCLUDED IN THE TEST FOR COL INDEPENDENCE. */
/*<       FACTOR = 1.E0 >*/
#line 2283 "cl2.f"
    factor = 1.f;
/*<       I = 1 >*/
#line 2284 "cl2.f"
    i__ = 1;
/*<       IP1 = 2 >*/
#line 2285 "cl2.f"
    ip1 = 2;
/*<       LEND = L >*/
#line 2286 "cl2.f"
    lend = *l;
/*<    10 IF (.NOT.(I.LE.LB)) GO TO 150 >*/
#line 2287 "cl2.f"
L10:
#line 2287 "cl2.f"
    if (! (i__ <= lb)) {
#line 2287 "cl2.f"
	goto L150;
#line 2287 "cl2.f"
    }
/*<       IF (.NOT.(I.LE.ME)) GO TO 130 >*/
#line 2288 "cl2.f"
    if (! (i__ <= me)) {
#line 2288 "cl2.f"
	goto L130;
#line 2288 "cl2.f"
    }

/*     SET IR TO POINT TO THE I-TH ROW. */
/*<       IR = I >*/
#line 2291 "cl2.f"
    ir = i__;
/*<       MEND = M >*/
#line 2292 "cl2.f"
    mend = *m;
/*<       ASSIGN 20 TO IGO996 >*/
#line 2293 "cl2.f"
    igo996 = 0;
#line 2293 "cl2.f"
    igo996_fmt = fmt_20;
/*<       GO TO 460 >*/
#line 2294 "cl2.f"
    goto L460;

/*     UPDATE-COL-SS-AND-FIND-PIVOT-COL */
/*<    20 ASSIGN 30 TO IGO993 >*/
#line 2297 "cl2.f"
L20:
#line 2297 "cl2.f"
    igo993 = 0;
#line 2297 "cl2.f"
    igo993_fmt = fmt_30;
/*<       GO TO 560 >*/
#line 2298 "cl2.f"
    goto L560;

/*     PERFORM-COL-INTERCHANGE */

/*     SET IC TO POINT TO I-TH COL. */
/*<    30 IC = I >*/
#line 2303 "cl2.f"
L30:
#line 2303 "cl2.f"
    ic = i__;
/*<       ASSIGN 40 TO IGO990 >*/
#line 2304 "cl2.f"
    igo990 = 0;
#line 2304 "cl2.f"
    igo990_fmt = fmt_40;
/*<       GO TO 520 >*/
#line 2305 "cl2.f"
    goto L520;

/*     TEST-INDEP-OF-INCOMING-COL */
/*<    40 IF (.NOT.(INDEP)) GO TO 110 >*/
#line 2308 "cl2.f"
L40:
#line 2308 "cl2.f"
    if (! indep) {
#line 2308 "cl2.f"
	goto L110;
#line 2308 "cl2.f"
    }

/*     ELIMINATE I-TH COL BELOW DIAG. USING MOD. GIVENS TRANSFORMATIONS */
/*     APPLIED TO (A B). */
/*<       J = M >*/
#line 2312 "cl2.f"
    j = *m;
/*<       DO 100 JJ=IP1,M >*/
#line 2313 "cl2.f"
    i__1 = *m;
#line 2313 "cl2.f"
    for (jj = ip1; jj <= i__1; ++jj) {
/*<         JM1 = J - 1 >*/
#line 2314 "cl2.f"
	jm1 = j - 1;
/*<         JP = JM1 >*/
#line 2315 "cl2.f"
	jp = jm1;
/*     WHEN OPERATING NEAR THE ME LINE, USE THE LARGEST ELT.        REMK */
/*     ABOVE IT AS THE PIVOT.                                       REMK */
/*       IF (.NOT.(J.EQ.MEP1)) GO TO 80                             REMK */
/*       IMAX = ME                                                  REMK */
/*       AMAX = SCALE(ME)*W(ME,I)**2                                REMK */
/*  50   IF (.NOT.(JP.GE.I)) GO TO 70                               REMK */
/*       T = SCALE(JP)*W(JP,I)**2                                   REMK */
/*       IF (.NOT.(T.GT.AMAX)) GO TO 60                             REMK */
/*       IMAX = JP                                                  REMK */
/*       AMAX = T                                                   REMK */
/*  60   JP = JP - 1                                                REMK */
/*       GO TO 50                                                   REMK */
/*  70   JP = IMAX                                                  REMK */
/*<         IF (.NOT. (JJ.EQ.M)) GO TO 70 >*/
#line 2329 "cl2.f"
	if (! (jj == *m)) {
#line 2329 "cl2.f"
	    goto L70;
#line 2329 "cl2.f"
	}
/*<         IF (.NOT. (I.LT.MEP1)) GO TO 80 >*/
#line 2330 "cl2.f"
	if (! (i__ < mep1)) {
#line 2330 "cl2.f"
	    goto L80;
#line 2330 "cl2.f"
	}
/*<         J = MEP1 >*/
#line 2331 "cl2.f"
	j = mep1;
/*<         JP = I >*/
#line 2332 "cl2.f"
	jp = i__;
/*<         T = SCALE(JP)*W(JP,I)**2*TAU**2 >*/
/* Computing 2nd power */
#line 2333 "cl2.f"
	r__1 = w[jp + i__ * w_dim1];
/* Computing 2nd power */
#line 2333 "cl2.f"
	r__2 = tau;
#line 2333 "cl2.f"
	t = scale[jp] * (r__1 * r__1) * (r__2 * r__2);
/*<         IF (.NOT.(T.GT.SCALE(J)*W(J,I)**2)) GO TO 130 >*/
/* Computing 2nd power */
#line 2334 "cl2.f"
	r__1 = w[j + i__ * w_dim1];
#line 2334 "cl2.f"
	if (! (t > scale[j] * (r__1 * r__1))) {
#line 2334 "cl2.f"
	    goto L130;
#line 2334 "cl2.f"
	}
/*<         GO TO 80 >*/
#line 2335 "cl2.f"
	goto L80;
/*<    70   IF (.NOT.(J.EQ.MEP1)) GO TO 80 >*/
#line 2336 "cl2.f"
L70:
#line 2336 "cl2.f"
	if (! (j == mep1)) {
#line 2336 "cl2.f"
	    goto L80;
#line 2336 "cl2.f"
	}
/*<         J = JM1 >*/
#line 2337 "cl2.f"
	j = jm1;
/*<         JM1 = J - 1 >*/
#line 2338 "cl2.f"
	jm1 = j - 1;
/*<         JP = JM1 >*/
#line 2339 "cl2.f"
	jp = jm1;
/*<    80   IF (.NOT.(W(J,I).NE.ZERO)) GO TO 90 >*/
#line 2340 "cl2.f"
L80:
#line 2340 "cl2.f"
	if (! (w[j + i__ * w_dim1] != zero)) {
#line 2340 "cl2.f"
	    goto L90;
#line 2340 "cl2.f"
	}
/*<         CALL SROTMG(SCALE(JP), SCALE(J), W(JP,I), W(J,I), SPARAM) >*/
#line 2341 "cl2.f"
	srotmg_(&scale[jp], &scale[j], &w[jp + i__ * w_dim1], &w[j + i__ *
		w_dim1], sparam);
/*<         W(J,I) = ZERO >*/
#line 2342 "cl2.f"
	w[j + i__ * w_dim1] = zero;
/*<         CALL SROTM(NP1-I, W(JP,IP1), MDW, W(J,IP1), MDW, SPARAM) >*/
#line 2343 "cl2.f"
	i__2 = np1 - i__;
#line 2343 "cl2.f"
	srotm_(&i__2, &w[jp + ip1 * w_dim1], mdw, &w[j + ip1 * w_dim1], mdw,
		sparam);
/*<    90   J = JM1 >*/
#line 2344 "cl2.f"
L90:
#line 2344 "cl2.f"
	j = jm1;
/*<   100 CONTINUE >*/
#line 2345 "cl2.f"
/* L100: */
#line 2345 "cl2.f"
    }
/*<       GO TO 140 >*/
#line 2346 "cl2.f"
    goto L140;
/*<   110 CONTINUE >*/
#line 2347 "cl2.f"
L110:
/*<       IF (.NOT.(LEND.GT.I)) GO TO 130 >*/
#line 2348 "cl2.f"
    if (! (lend > i__)) {
#line 2348 "cl2.f"
	goto L130;
#line 2348 "cl2.f"
    }

/*     COL I IS DEPENDENT. SWAP WITH COL LEND. */
/*<       MAX = LEND >*/
#line 2351 "cl2.f"
    max__ = lend;

/*     PERFORM-COL-INTERCHANGE */
/*<       ASSIGN 120 TO IGO993 >*/
#line 2354 "cl2.f"
    igo993 = 1;
#line 2354 "cl2.f"
    igo993_fmt = fmt_120;
/*<       GO TO 560 >*/
#line 2355 "cl2.f"
    goto L560;
/*<   120 CONTINUE >*/
#line 2356 "cl2.f"
L120:
/*<       LEND = LEND - 1 >*/
#line 2357 "cl2.f"
    --lend;

/*     FIND COL IN REMAINING SET WITH LARGEST SS. */
/*<       MAX = ISAMAX(LEND-I+1,H(I),1) + I - 1 >*/
#line 2360 "cl2.f"
    i__1 = lend - i__ + 1;
#line 2360 "cl2.f"
    max__ = isamax_(&i__1, &h__[i__], &c__1) + i__ - 1;
/*<       HBAR = H(MAX) >*/
#line 2361 "cl2.f"
    hbar = h__[max__];
/*<       GO TO 30 >*/
#line 2362 "cl2.f"
    goto L30;
/*<   130 CONTINUE >*/
#line 2363 "cl2.f"
L130:
/*<       KRANK = I - 1 >*/
#line 2364 "cl2.f"
    krank = i__ - 1;
/*<       GO TO 160 >*/
#line 2365 "cl2.f"
    goto L160;
/*<   140 I = IP1 >*/
#line 2366 "cl2.f"
L140:
#line 2366 "cl2.f"
    i__ = ip1;
/*<       IP1 = IP1 + 1 >*/
#line 2367 "cl2.f"
    ++ip1;
/*<       GO TO 10 >*/
#line 2368 "cl2.f"
    goto L10;
/*<   150 KRANK = L1 >*/
#line 2369 "cl2.f"
L150:
#line 2369 "cl2.f"
    krank = l1;
/*<   160 CONTINUE >*/
#line 2370 "cl2.f"
L160:
/*<       KRP1 = KRANK + 1 >*/
#line 2371 "cl2.f"
    krp1 = krank + 1;
/*<       IF (.NOT.(KRANK.LT.ME)) GO TO 290 >*/
#line 2372 "cl2.f"
    if (! (krank < me)) {
#line 2372 "cl2.f"
	goto L290;
#line 2372 "cl2.f"
    }
/*<       FACTOR = ALSQ >*/
#line 2373 "cl2.f"
    factor = alsq;
/*<       DO 170 I=KRP1,ME >*/
#line 2374 "cl2.f"
    i__1 = me;
#line 2374 "cl2.f"
    for (i__ = krp1; i__ <= i__1; ++i__) {
/*<         IF (L.GT.0) W(I,1) = ZERO >*/
#line 2375 "cl2.f"
	if (*l > 0) {
#line 2375 "cl2.f"
	    w[i__ + w_dim1] = zero;
#line 2375 "cl2.f"
	}
/*<         CALL SCOPY(L, W(I,1), 0, W(I,1), MDW) >*/
#line 2376 "cl2.f"
	scopy_(l, &w[i__ + w_dim1], &c__0, &w[i__ + w_dim1], mdw);
/*<   170 CONTINUE >*/
#line 2377 "cl2.f"
/* L170: */
#line 2377 "cl2.f"
    }

/*     DETERMINE THE RANK OF THE REMAINING EQUALITY CONSTRAINT */
/*     EQUATIONS BY ELIMINATING WITHIN THE BLOCK OF CONSTRAINED */
/*     VARIABLES.  REMOVE ANY REDUNDANT CONSTRAINTS. */
/*<       IR = KRP1 >*/
#line 2382 "cl2.f"
    ir = krp1;
/*<       IF (.NOT. (L.LT.N)) GO TO 245 >*/
#line 2383 "cl2.f"
    if (! (*l < *n)) {
#line 2383 "cl2.f"
	goto L245;
#line 2383 "cl2.f"
    }
/*<       LP1 = L + 1 >*/
#line 2384 "cl2.f"
    lp1 = *l + 1;
/*<       RECALC = .TRUE. >*/
#line 2385 "cl2.f"
    recalc = TRUE_;
/*<       LB = MIN0(L+ME-KRANK,N) >*/
/* Computing MIN */
#line 2386 "cl2.f"
    i__1 = *l + me - krank;
#line 2386 "cl2.f"
    lb = min(i__1,*n);
/*<       I = LP1 >*/
#line 2387 "cl2.f"
    i__ = lp1;
/*<       IP1 = I + 1 >*/
#line 2388 "cl2.f"
    ip1 = i__ + 1;
/*<   180 IF (.NOT.(I.LE.LB)) GO TO 280 >*/
#line 2389 "cl2.f"
L180:
#line 2389 "cl2.f"
    if (! (i__ <= lb)) {
#line 2389 "cl2.f"
	goto L280;
#line 2389 "cl2.f"
    }
/*<       IR = KRANK + I - L >*/
#line 2390 "cl2.f"
    ir = krank + i__ - *l;
/*<       LEND = N >*/
#line 2391 "cl2.f"
    lend = *n;
/*<       MEND = ME >*/
#line 2392 "cl2.f"
    mend = me;
/*<       ASSIGN 190 TO IGO996 >*/
#line 2393 "cl2.f"
    igo996 = 1;
#line 2393 "cl2.f"
    igo996_fmt = fmt_190;
/*<       GO TO 460 >*/
#line 2394 "cl2.f"
    goto L460;

/*     UPDATE-COL-SS-AND-FIND-PIVOT-COL */
/*<   190 ASSIGN 200 TO IGO993 >*/
#line 2397 "cl2.f"
L190:
#line 2397 "cl2.f"
    igo993 = 2;
#line 2397 "cl2.f"
    igo993_fmt = fmt_200;
/*<       GO TO 560 >*/
#line 2398 "cl2.f"
    goto L560;

/*     PERFORM-COL-INTERCHANGE */

/*     ELIMINATE ELEMENTS IN THE I-TH COL. */
/*<   200 J = ME >*/
#line 2403 "cl2.f"
L200:
#line 2403 "cl2.f"
    j = me;
/*<   210 IF (.NOT.(J.GT.IR)) GO TO 230 >*/
#line 2404 "cl2.f"
L210:
#line 2404 "cl2.f"
    if (! (j > ir)) {
#line 2404 "cl2.f"
	goto L230;
#line 2404 "cl2.f"
    }
/*<       JM1 = J - 1 >*/
#line 2405 "cl2.f"
    jm1 = j - 1;
/*<       IF (.NOT.(W(J,I).NE.ZERO)) GO TO 220 >*/
#line 2406 "cl2.f"
    if (! (w[j + i__ * w_dim1] != zero)) {
#line 2406 "cl2.f"
	goto L220;
#line 2406 "cl2.f"
    }
/*<       CALL SROTMG(SCALE(JM1), SCALE(J), W(JM1,I), W(J,I), SPARAM) >*/
#line 2407 "cl2.f"
    srotmg_(&scale[jm1], &scale[j], &w[jm1 + i__ * w_dim1], &w[j + i__ *
	    w_dim1], sparam);
/*<       W(J,I) = ZERO >*/
#line 2408 "cl2.f"
    w[j + i__ * w_dim1] = zero;
/*<       CALL SROTM(NP1-I, W(JM1,IP1), MDW, W(J,IP1), MDW, SPARAM) >*/
#line 2409 "cl2.f"
    i__1 = np1 - i__;
#line 2409 "cl2.f"
    srotm_(&i__1, &w[jm1 + ip1 * w_dim1], mdw, &w[j + ip1 * w_dim1], mdw,
	    sparam);
/*<   220 J = JM1 >*/
#line 2410 "cl2.f"
L220:
#line 2410 "cl2.f"
    j = jm1;
/*<       GO TO 210 >*/
#line 2411 "cl2.f"
    goto L210;

/*     SET IC=I=COL BEING ELIMINATED */
/*<   230 IC = I >*/
#line 2414 "cl2.f"
L230:
#line 2414 "cl2.f"
    ic = i__;
/*<       ASSIGN 240 TO IGO990 >*/
#line 2415 "cl2.f"
    igo990 = 1;
#line 2415 "cl2.f"
    igo990_fmt = fmt_240;
/*<       GO TO 520 >*/
#line 2416 "cl2.f"
    goto L520;

/*     TEST-INDEP-OF-INCOMING-COL */
/*<   240 IF (INDEP) GO TO 270 >*/
#line 2419 "cl2.f"
L240:
#line 2419 "cl2.f"
    if (indep) {
#line 2419 "cl2.f"
	goto L270;
#line 2419 "cl2.f"
    }

/*     REMOVE ANY REDUNDANT OR DEPENDENT EQUALITY CONSTRAINTS. */
/*<   245 CONTINUE >*/
#line 2422 "cl2.f"
L245:
/*<       JJ = IR >*/
#line 2423 "cl2.f"
    jj = ir;
/*<   250 IF (.NOT.(IR.LE.ME)) GO TO 260 >*/
#line 2424 "cl2.f"
L250:
#line 2424 "cl2.f"
    if (! (ir <= me)) {
#line 2424 "cl2.f"
	goto L260;
#line 2424 "cl2.f"
    }
/*<       W(IR,1) = ZERO >*/
#line 2425 "cl2.f"
    w[ir + w_dim1] = zero;
/*<       CALL SCOPY(N, W(IR,1), 0, W(IR,1), MDW) >*/
#line 2426 "cl2.f"
    scopy_(n, &w[ir + w_dim1], &c__0, &w[ir + w_dim1], mdw);
/*<       RNORM = RNORM + (SCALE(IR)*W(IR,NP1)/ALSQ)*W(IR,NP1) >*/
#line 2427 "cl2.f"
    *rnorm += scale[ir] * w[ir + np1 * w_dim1] / alsq * w[ir + np1 * w_dim1];
/*<       W(IR,NP1) = ZERO >*/
#line 2428 "cl2.f"
    w[ir + np1 * w_dim1] = zero;
/*<       SCALE(IR) = ONE >*/
#line 2429 "cl2.f"
    scale[ir] = one;
/*     RECLASSIFY THE ZEROED ROW AS A LEAST SQUARES EQUATION. */
/*<       ITYPE(IR) = 1 >*/
#line 2431 "cl2.f"
    itype[ir] = 1;
/*<       IR = IR + 1 >*/
#line 2432 "cl2.f"
    ++ir;
/*<       GO TO 250 >*/
#line 2433 "cl2.f"
    goto L250;

/*     REDUCE ME TO REFLECT ANY DISCOVERED DEPENDENT EQUALITY */
/*     CONSTRAINTS. */
/*<   260 CONTINUE >*/
#line 2437 "cl2.f"
L260:
/*<       ME = JJ - 1 >*/
#line 2438 "cl2.f"
    me = jj - 1;
/*<       MEP1 = ME + 1 >*/
#line 2439 "cl2.f"
    mep1 = me + 1;
/*<       GO TO 300 >*/
#line 2440 "cl2.f"
    goto L300;
/*<   270 I = IP1 >*/
#line 2441 "cl2.f"
L270:
#line 2441 "cl2.f"
    i__ = ip1;
/*<       IP1 = IP1 + 1 >*/
#line 2442 "cl2.f"
    ++ip1;
/*<       GO TO 180 >*/
#line 2443 "cl2.f"
    goto L180;
/*<   280 CONTINUE >*/
#line 2444 "cl2.f"
L280:
/*<   290 CONTINUE >*/
#line 2445 "cl2.f"
L290:
/*<   300 CONTINUE >*/
#line 2446 "cl2.f"
L300:
/*<       IF (.NOT.(KRANK.LT.L1)) GO TO 420 >*/
#line 2447 "cl2.f"
    if (! (krank < l1)) {
#line 2447 "cl2.f"
	goto L420;
#line 2447 "cl2.f"
    }

/*     TRY TO DETERMINE THE VARIABLES KRANK+1 THROUGH L1 FROM THE */
/*     LEAST SQUARES EQUATIONS.  CONTINUE THE TRIANGULARIZATION WITH */
/*     PIVOT ELEMENT W(MEP1,I). */

/*<       RECALC = .TRUE. >*/
#line 2453 "cl2.f"
    recalc = TRUE_;

/*     SET FACTOR=ALSQ TO REMOVE EFFECT OF HEAVY WEIGHT FROM */
/*     TEST FOR COL INDEPENDENCE. */
/*<       FACTOR = ALSQ >*/
#line 2457 "cl2.f"
    factor = alsq;
/*<       KK = KRP1 >*/
#line 2458 "cl2.f"
    kk = krp1;
/*<       I = KK >*/
#line 2459 "cl2.f"
    i__ = kk;
/*<       IP1 = I + 1 >*/
#line 2460 "cl2.f"
    ip1 = i__ + 1;
/*<   310 IF (.NOT.(I.LE.L1)) GO TO 410 >*/
#line 2461 "cl2.f"
L310:
#line 2461 "cl2.f"
    if (! (i__ <= l1)) {
#line 2461 "cl2.f"
	goto L410;
#line 2461 "cl2.f"
    }

/*     SET IR TO POINT TO THE MEP1-ST ROW. */
/*<       IR = MEP1 >*/
#line 2464 "cl2.f"
    ir = mep1;
/*<       LEND = L >*/
#line 2465 "cl2.f"
    lend = *l;
/*<       MEND = M >*/
#line 2466 "cl2.f"
    mend = *m;
/*<       ASSIGN 320 TO IGO996 >*/
#line 2467 "cl2.f"
    igo996 = 2;
#line 2467 "cl2.f"
    igo996_fmt = fmt_320;
/*<       GO TO 460 >*/
#line 2468 "cl2.f"
    goto L460;

/*     UPDATE-COL-SS-AND-FIND-PIVOT-COL */
/*<   320 ASSIGN 330 TO IGO993 >*/
#line 2471 "cl2.f"
L320:
#line 2471 "cl2.f"
    igo993 = 3;
#line 2471 "cl2.f"
    igo993_fmt = fmt_330;
/*<       GO TO 560 >*/
#line 2472 "cl2.f"
    goto L560;

/*     PERFORM-COL-INTERCHANGE */

/*     ELIMINATE I-TH COL BELOW THE IR-TH ELEMENT. */
/*<   330 IRP1 = IR + 1 >*/
#line 2477 "cl2.f"
L330:
#line 2477 "cl2.f"
    irp1 = ir + 1;
/*<       IF (.NOT.(IRP1.LE.M)) GO TO 355 >*/
#line 2478 "cl2.f"
    if (! (irp1 <= *m)) {
#line 2478 "cl2.f"
	goto L355;
#line 2478 "cl2.f"
    }
/*<       J = M >*/
#line 2479 "cl2.f"
    j = *m;
/*<       DO 350 JJ=IRP1,M >*/
#line 2480 "cl2.f"
    i__1 = *m;
#line 2480 "cl2.f"
    for (jj = irp1; jj <= i__1; ++jj) {
/*<         JM1 = J - 1 >*/
#line 2481 "cl2.f"
	jm1 = j - 1;
/*<         IF (.NOT.(W(J,I).NE.ZERO)) GO TO 340 >*/
#line 2482 "cl2.f"
	if (! (w[j + i__ * w_dim1] != zero)) {
#line 2482 "cl2.f"
	    goto L340;
#line 2482 "cl2.f"
	}
/*<         CALL SROTMG(SCALE(JM1), SCALE(J), W(JM1,I), W(J,I), SPARAM) >*/
#line 2483 "cl2.f"
	srotmg_(&scale[jm1], &scale[j], &w[jm1 + i__ * w_dim1], &w[j + i__ *
		w_dim1], sparam);
/*<         W(J,I) = ZERO >*/
#line 2484 "cl2.f"
	w[j + i__ * w_dim1] = zero;
/*<         CALL SROTM(NP1-I, W(JM1,IP1), MDW, W(J,IP1), MDW, SPARAM) >*/
#line 2485 "cl2.f"
	i__2 = np1 - i__;
#line 2485 "cl2.f"
	srotm_(&i__2, &w[jm1 + ip1 * w_dim1], mdw, &w[j + ip1 * w_dim1], mdw,
		sparam);
/*<   340   J = JM1 >*/
#line 2486 "cl2.f"
L340:
#line 2486 "cl2.f"
	j = jm1;
/*<   350 CONTINUE >*/
#line 2487 "cl2.f"
/* L350: */
#line 2487 "cl2.f"
    }
/*<   355 CONTINUE >*/
#line 2488 "cl2.f"
L355:

/*     TEST IF NEW PIVOT ELEMENT IS NEAR ZERO. IF SO, THE COL IS */
/*     DEPENDENT. */
/*<       T = SCALE(IR)*W(IR,I)**2 >*/
/* Computing 2nd power */
#line 2492 "cl2.f"
    r__1 = w[ir + i__ * w_dim1];
#line 2492 "cl2.f"
    t = scale[ir] * (r__1 * r__1);
/*<       INDEP = T.GT.TAU**2*EANORM**2 >*/
/* Computing 2nd power */
#line 2493 "cl2.f"
    r__1 = tau;
/* Computing 2nd power */
#line 2493 "cl2.f"
    r__2 = eanorm;
#line 2493 "cl2.f"
    indep = t > r__1 * r__1 * (r__2 * r__2);
/*<       IF (.NOT.INDEP) GO TO 380 >*/
#line 2494 "cl2.f"
    if (! indep) {
#line 2494 "cl2.f"
	goto L380;
#line 2494 "cl2.f"
    }

/*     COL TEST PASSED. NOW MUST PASS ROW NORM TEST TO BE CLASSIFIED */
/*     AS INDEPENDENT. */
/*<       RN = ZERO >*/
#line 2498 "cl2.f"
    rn = zero;
/*<       DO 370 I1=IR,M >*/
#line 2499 "cl2.f"
    i__1 = *m;
#line 2499 "cl2.f"
    for (i1 = ir; i1 <= i__1; ++i1) {
/*<         DO 360 J1=IP1,N >*/
#line 2500 "cl2.f"
	i__2 = *n;
#line 2500 "cl2.f"
	for (j1 = ip1; j1 <= i__2; ++j1) {
/*<           RN = AMAX1(RN,SCALE(I1)*W(I1,J1)**2) >*/
/* Computing MAX */
/* Computing 2nd power */
#line 2501 "cl2.f"
	    r__3 = w[i1 + j1 * w_dim1];
#line 2501 "cl2.f"
	    r__1 = rn, r__2 = scale[i1] * (r__3 * r__3);
#line 2501 "cl2.f"
	    rn = max(r__1,r__2);
/*<   360   CONTINUE >*/
#line 2502 "cl2.f"
/* L360: */
#line 2502 "cl2.f"
	}
/*<   370 CONTINUE >*/
#line 2503 "cl2.f"
/* L370: */
#line 2503 "cl2.f"
    }
/*<       INDEP = T.GT.TAU**2*RN >*/
/* Computing 2nd power */
#line 2504 "cl2.f"
    r__1 = tau;
#line 2504 "cl2.f"
    indep = t > r__1 * r__1 * rn;

/*     IF INDEPENDENT, SWAP THE IR-TH AND KRP1-ST ROWS TO MAINTAIN THE */
/*     TRIANGULAR FORM.  UPDATE THE RANK INDICATOR KRANK AND THE */
/*     EQUALITY CONSTRAINT POINTER ME. */
/*<   380 IF (.NOT.(INDEP)) GO TO 390 >*/
#line 2509 "cl2.f"
L380:
#line 2509 "cl2.f"
    if (! indep) {
#line 2509 "cl2.f"
	goto L390;
#line 2509 "cl2.f"
    }
/*<       CALL SSWAP(NP1, W(KRP1,1), MDW, W(IR,1), MDW) >*/
#line 2510 "cl2.f"
    sswap_(&np1, &w[krp1 + w_dim1], mdw, &w[ir + w_dim1], mdw);
/*<       CALL SSWAP(1, SCALE(KRP1), 1, SCALE(IR), 1) >*/
#line 2511 "cl2.f"
    sswap_(&c__1, &scale[krp1], &c__1, &scale[ir], &c__1);
/*     RECLASSIFY THE LEAST SQ. EQUATION AS AN EQUALITY CONSTRAINT AND */
/*     RESCALE IT. */
/*<       ITYPE(IR) = 0 >*/
#line 2514 "cl2.f"
    itype[ir] = 0;
/*<       T = SQRT(SCALE(KRP1)) >*/
#line 2515 "cl2.f"
    t = sqrt(scale[krp1]);
/*<       CALL SSCAL(NP1, T, W(KRP1,1), MDW) >*/
#line 2516 "cl2.f"
    sscal_(&np1, &t, &w[krp1 + w_dim1], mdw);
/*<       SCALE(KRP1) = ALSQ >*/
#line 2517 "cl2.f"
    scale[krp1] = alsq;
/*<       ME = MEP1 >*/
#line 2518 "cl2.f"
    me = mep1;
/*<       MEP1 = ME + 1 >*/
#line 2519 "cl2.f"
    mep1 = me + 1;
/*<       KRANK = KRP1 >*/
#line 2520 "cl2.f"
    krank = krp1;
/*<       KRP1 = KRANK + 1 >*/
#line 2521 "cl2.f"
    krp1 = krank + 1;
/*<       GO TO 400 >*/
#line 2522 "cl2.f"
    goto L400;
/*<   390 GO TO 430 >*/
#line 2523 "cl2.f"
L390:
#line 2523 "cl2.f"
    goto L430;
/*<   400 I = IP1 >*/
#line 2524 "cl2.f"
L400:
#line 2524 "cl2.f"
    i__ = ip1;
/*<       IP1 = IP1 + 1 >*/
#line 2525 "cl2.f"
    ++ip1;
/*<       GO TO 310 >*/
#line 2526 "cl2.f"
    goto L310;
/*<   410 CONTINUE >*/
#line 2527 "cl2.f"
L410:
/*<   420 CONTINUE >*/
#line 2528 "cl2.f"
L420:
/*<   430 CONTINUE >*/
#line 2529 "cl2.f"
L430:

/*     IF PSEUDORANK IS LESS THAN L, APPLY HOUSEHOLDER TRANS. */
/*     FROM RIGHT. */
/*<       IF (.NOT.(KRANK.LT.L)) GO TO 450 >*/
#line 2533 "cl2.f"
    if (! (krank < *l)) {
#line 2533 "cl2.f"
	goto L450;
#line 2533 "cl2.f"
    }
/*<       DO 440 I=1,KRANK >*/
#line 2534 "cl2.f"
    i__1 = krank;
#line 2534 "cl2.f"
    for (i__ = 1; i__ <= i__1; ++i__) {
/*<         J = KRP1 - I >*/
#line 2535 "cl2.f"
	j = krp1 - i__;
/*<         CALL H12(1, J, KRP1, L, W(J,1), MDW, H(J), W, MDW, 1, J-1) >*/
#line 2536 "cl2.f"
	i__2 = j - 1;
#line 2536 "cl2.f"
	h12_(&c__1, &j, &krp1, l, &w[j + w_dim1], mdw, &h__[j], &w[w_offset],
		mdw, &c__1, &i__2);
/*<   440 CONTINUE >*/
#line 2537 "cl2.f"
/* L440: */
#line 2537 "cl2.f"
    }
/*<   450 NIV = KRANK + NSOLN - L >*/
#line 2538 "cl2.f"
L450:
#line 2538 "cl2.f"
    niv = krank + nsoln - *l;
/*<       NIV1 = NIV + 1 >*/
#line 2539 "cl2.f"
    niv1 = niv + 1;
/*<       IF (L.EQ.N) DONE = .TRUE. >*/
#line 2540 "cl2.f"
    if (*l == *n) {
#line 2540 "cl2.f"
	*done = TRUE_;
#line 2540 "cl2.f"
    }

/*  END OF INITIAL TRIANGULARIZATION. */
/*<       IDOPE(1) = ME >*/
#line 2543 "cl2.f"
    idope[1] = me;
/*<       IDOPE(2) = MEP1 >*/
#line 2544 "cl2.f"
    idope[2] = mep1;
/*<       IDOPE(3) = KRANK >*/
#line 2545 "cl2.f"
    idope[3] = krank;
/*<       IDOPE(4) = KRP1 >*/
#line 2546 "cl2.f"
    idope[4] = krp1;
/*<       IDOPE(5) = NSOLN >*/
#line 2547 "cl2.f"
    idope[5] = nsoln;
/*<       IDOPE(6) = NIV >*/
#line 2548 "cl2.f"
    idope[6] = niv;
/*<       IDOPE(7) = NIV1 >*/
#line 2549 "cl2.f"
    idope[7] = niv1;
/*<       IDOPE(8) = L1 >*/
#line 2550 "cl2.f"
    idope[8] = l1;
/*<       RETURN >*/
#line 2551 "cl2.f"
    return 0;
/*<   460 CONTINUE >*/
#line 2552 "cl2.f"
L460:

/*     TO UPDATE-COL-SS-AND-FIND-PIVOT-COL */

/*     THE COL SS VECTOR WILL BE UPDATED AT EACH STEP. WHEN */
/*     NUMERICALLY NECESSARY, THESE VALUES WILL BE RECOMPUTED. */

/*<       IF (.NOT.(IR.NE.1 .AND. (.NOT.RECALC))) GO TO 480 >*/
#line 2559 "cl2.f"
    if (! (ir != 1 && ! recalc)) {
#line 2559 "cl2.f"
	goto L480;
#line 2559 "cl2.f"
    }
/*     UPDATE COL SS =SUM OF SQUARES. */
/*<       DO 470 J=I,LEND >*/
#line 2561 "cl2.f"
    i__1 = lend;
#line 2561 "cl2.f"
    for (j = i__; j <= i__1; ++j) {
/*<         H(J) = H(J) - SCALE(IR-1)*W(IR-1,J)**2 >*/
/* Computing 2nd power */
#line 2562 "cl2.f"
	r__1 = w[ir - 1 + j * w_dim1];
#line 2562 "cl2.f"
	h__[j] -= scale[ir - 1] * (r__1 * r__1);
/*<   470 CONTINUE >*/
#line 2563 "cl2.f"
/* L470: */
#line 2563 "cl2.f"
    }

/*     TEST FOR NUMERICAL ACCURACY. */
/*<       MAX = ISAMAX(LEND-I+1,H(I),1) + I - 1 >*/
#line 2566 "cl2.f"
    i__1 = lend - i__ + 1;
#line 2566 "cl2.f"
    max__ = isamax_(&i__1, &h__[i__], &c__1) + i__ - 1;
/*<       RECALC = HBAR + TENM3*H(MAX).EQ.HBAR >*/
#line 2567 "cl2.f"
    recalc = hbar + tenm3 * h__[max__] == hbar;

/*     IF REQUIRED, RECALCULATE COL SS, USING ROWS IR THROUGH MEND. */
/*<   480 IF (.NOT.(RECALC)) GO TO 510 >*/
#line 2570 "cl2.f"
L480:
#line 2570 "cl2.f"
    if (! recalc) {
#line 2570 "cl2.f"
	goto L510;
#line 2570 "cl2.f"
    }
/*<       DO 500 J=I,LEND >*/
#line 2571 "cl2.f"
    i__1 = lend;
#line 2571 "cl2.f"
    for (j = i__; j <= i__1; ++j) {
/*<         H(J) = ZERO >*/
#line 2572 "cl2.f"
	h__[j] = zero;
/*<         DO 490 K=IR,MEND >*/
#line 2573 "cl2.f"
	i__2 = mend;
#line 2573 "cl2.f"
	for (k = ir; k <= i__2; ++k) {
/*<           H(J) = H(J) + SCALE(K)*W(K,J)**2 >*/
/* Computing 2nd power */
#line 2574 "cl2.f"
	    r__1 = w[k + j * w_dim1];
#line 2574 "cl2.f"
	    h__[j] += scale[k] * (r__1 * r__1);
/*<   490   CONTINUE >*/
#line 2575 "cl2.f"
/* L490: */
#line 2575 "cl2.f"
	}
/*<   500 CONTINUE >*/
#line 2576 "cl2.f"
/* L500: */
#line 2576 "cl2.f"
    }

/*     FIND COL WITH LARGEST SS. */
/*<       MAX = ISAMAX(LEND-I+1,H(I),1) + I - 1 >*/
#line 2579 "cl2.f"
    i__1 = lend - i__ + 1;
#line 2579 "cl2.f"
    max__ = isamax_(&i__1, &h__[i__], &c__1) + i__ - 1;
/*<       HBAR = H(MAX) >*/
#line 2580 "cl2.f"
    hbar = h__[max__];
/*<   510 GO TO 600 >*/
#line 2581 "cl2.f"
L510:
#line 2581 "cl2.f"
    goto L600;
/*<   520 CONTINUE >*/
#line 2582 "cl2.f"
L520:

/*     TO TEST-INDEP-OF-INCOMING-COL */

/*     TEST THE COL IC TO DETERMINE IF IT IS LINEARLY INDEPENDENT */
/*     OF THE COLS ALREADY IN THE BASIS.  IN THE INIT TRI */
/*     STEP, WE USUALLY WANT THE HEAVY WEIGHT ALAMDA TO */
/*     BE INCLUDED IN THE TEST FOR INDEPENDENCE.  IN THIS CASE THE */
/*     VALUE OF FACTOR WILL HAVE BEEN SET TO 1.E0 BEFORE THIS */
/*     PROCEDURE IS INVOKED.  IN THE POTENTIALLY RANK DEFICIENT */
/*     PROBLEM, THE VALUE OF FACTOR WILL HAVE BEEN */
/*     SET TO ALSQ=ALAMDA**2 TO REMOVE THE EFFECT OF THE HEAVY WEIGHT */
/*     FROM THE TEST FOR INDEPENDENCE. */

/*     WRITE NEW COL AS PARTITIONED VECTOR */
/*             (A1)  NUMBER OF COMPONENTS IN SOLN SO FAR = NIV */
/*             (A2)  M-NIV COMPONENTS */
/*     AND COMPUTE  SN = INVERSE WEIGHTED LENGTH OF A1 */
/*                  RN = INVERSE WEIGHTED LENGTH OF A2 */
/*     CALL THE COL INDEPENDENT WHEN RN .GT. TAU*SN */
/*<       SN = ZERO >*/
#line 2602 "cl2.f"
    sn = zero;
/*<       RN = ZERO >*/
#line 2603 "cl2.f"
    rn = zero;
/*<       DO 550 J=1,MEND >*/
#line 2604 "cl2.f"
    i__1 = mend;
#line 2604 "cl2.f"
    for (j = 1; j <= i__1; ++j) {
/*<         T = SCALE(J) >*/
#line 2605 "cl2.f"
	t = scale[j];
/*<         IF (J.LE.ME) T = T/FACTOR >*/
#line 2606 "cl2.f"
	if (j <= me) {
#line 2606 "cl2.f"
	    t /= factor;
#line 2606 "cl2.f"
	}
/*<         T = T*W(J,IC)**2 >*/
/* Computing 2nd power */
#line 2607 "cl2.f"
	r__1 = w[j + ic * w_dim1];
#line 2607 "cl2.f"
	t *= r__1 * r__1;
/*<         IF (.NOT.(J.LT.IR)) GO TO 530 >*/
#line 2608 "cl2.f"
	if (! (j < ir)) {
#line 2608 "cl2.f"
	    goto L530;
#line 2608 "cl2.f"
	}
/*<         SN = SN + T >*/
#line 2609 "cl2.f"
	sn += t;
/*<         GO TO 540 >*/
#line 2610 "cl2.f"
	goto L540;
/*<   530   RN = RN + T >*/
#line 2611 "cl2.f"
L530:
#line 2611 "cl2.f"
	rn += t;
/*<   540   CONTINUE >*/
#line 2612 "cl2.f"
L540:
/*<   550 CONTINUE >*/
#line 2613 "cl2.f"
/* L550: */
#line 2613 "cl2.f"
	;
#line 2613 "cl2.f"
    }
/*<       INDEP = RN.GT.TAU**2*SN >*/
/* Computing 2nd power */
#line 2614 "cl2.f"
    r__1 = tau;
#line 2614 "cl2.f"
    indep = rn > r__1 * r__1 * sn;
/*<       GO TO 590 >*/
#line 2615 "cl2.f"
    goto L590;
/*<   560 CONTINUE >*/
#line 2616 "cl2.f"
L560:

/*     TO PERFORM-COL-INTERCHANGE */

/*<       IF (.NOT.(MAX.NE.I)) GO TO 570 >*/
#line 2620 "cl2.f"
    if (! (max__ != i__)) {
#line 2620 "cl2.f"
	goto L570;
#line 2620 "cl2.f"
    }
/*     EXCHANGE ELEMENTS OF PERMUTED INDEX VECTOR AND PERFORM COL */
/*     INTERCHANGES. */
/*<       ITEMP = IPIVOT(I) >*/
#line 2623 "cl2.f"
    itemp = ipivot[i__];
/*<       IPIVOT(I) = IPIVOT(MAX) >*/
#line 2624 "cl2.f"
    ipivot[i__] = ipivot[max__];
/*<       IPIVOT(MAX) = ITEMP >*/
#line 2625 "cl2.f"
    ipivot[max__] = itemp;
/*<       CALL SSWAP(M, W(1,MAX), 1, W(1,I), 1) >*/
#line 2626 "cl2.f"
    sswap_(m, &w[max__ * w_dim1 + 1], &c__1, &w[i__ * w_dim1 + 1], &c__1);
/*<       T = H(MAX) >*/
#line 2627 "cl2.f"
    t = h__[max__];
/*<       H(MAX) = H(I) >*/
#line 2628 "cl2.f"
    h__[max__] = h__[i__];
/*<       H(I) = T >*/
#line 2629 "cl2.f"
    h__[i__] = t;
/*<   570 GO TO 580 >*/
#line 2630 "cl2.f"
L570:
#line 2630 "cl2.f"
    goto L580;
/*<   580 GO TO IGO993, (30, 200, 330, 120) >*/
#line 2631 "cl2.f"
L580:
#line 2631 "cl2.f"
    switch (igo993) {
#line 2631 "cl2.f"
	case 0: goto L30;
#line 2631 "cl2.f"
	case 1: goto L120;
#line 2631 "cl2.f"
	case 2: goto L200;
#line 2631 "cl2.f"
	case 3: goto L330;
#line 2631 "cl2.f"
    }
/*<   590 GO TO IGO990, (40, 240) >*/
#line 2632 "cl2.f"
L590:
#line 2632 "cl2.f"
    switch (igo990) {
#line 2632 "cl2.f"
	case 0: goto L40;
#line 2632 "cl2.f"
	case 1: goto L240;
#line 2632 "cl2.f"
    }
/*<   600 GO TO IGO996, (20, 190, 320) >*/
#line 2633 "cl2.f"
L600:
#line 2633 "cl2.f"
    switch (igo996) {
#line 2633 "cl2.f"
	case 0: goto L20;
#line 2633 "cl2.f"
	case 1: goto L190;
#line 2633 "cl2.f"
	case 2: goto L320;
#line 2633 "cl2.f"
    }
  return 0 ;
/*<       END >*/
} /* wnlit_ */

/* CC */
/* CC---------------------------------------------------------------------------- */
/* CC */
/*<    >*/
static /* Subroutine */ int hfti_(real *a, integer *mda, integer *m, integer *n,
	real *b, integer *mdb, integer *nb, real *tau, integer *krank, real *
	rnorm, real *h__, real *g, integer *ip)
{
    /* System generated locals */
    integer a_dim1, a_offset, b_dim1, b_offset, i__1, i__2, i__3;
    real r__1;

    /* Builtin functions */
    double sqrt(doublereal);

    /* Local variables */
    static integer i__, j, k, l;
    static integer jb, ii, jj;
    static real sm;
    static integer ip1, kp1;
    static real sm1, tmp;
    static integer jcol;
    static real hmax;
    static integer lmax, nerr, iopt;
    static real zero;
    static integer ldiag;
    static real factor;


/*     THE EDITING REQUIRED TO CONVERT THIS SUBROUTINE FROM SINGLE TO */
/*     DOUBLE PRECISION INVOLVES THE FOLLOWING CHARACTER STRING CHANGES. */
/*     USE AN EDITING COMMAND (CHANGE) /STRING-1/(TO)STRING-2/. */
/*     (BEGIN CHANGES AT LINE WITH C++ IN COLS. 1-3.) */
/*     /REAL (12 BLANKS)/DOUBLE PRECISION/,/ SQRT/ DSQRT/, */
/*     /, ABS/, DABS/,/ABS(/DABS(/,/E0/D0/ */

/*     DIMENSION A(MDA,N),(B(MDB,NB) OR B(M)),RNORM(NB),H(N),G(N),IP(N) */

/*     WRITTEN BY C. L. LAWSON AND R. J. HANSON.  FROM THE BOOK SOLVING */
/*     LEAST SQUARES PROBLEMS, PRENTICE-HALL, INC. (1974). FOR FURTHER */
/*     ALGORITHMIC DETAILS SEE ALGORITHM HFTI IN CHAPTER 14. */

/*     ABSTRACT */

/*     THIS SUBROUTINE SOLVES A LINEAR LEAST SQUARES PROBLEM OR A SET OF */
/*     LINEAR LEAST SQUARES PROBLEMS HAVING THE SAME MATRIX BUT DIFFERENT */
/*     RIGHT-SIDE VECTORS.  THE PROBLEM DATA CONSISTS OF AN M BY N MATRIX */
/*     A, AN M BY NB MATRIX B, AND AN ABSOLUTE TOLERANCE PARAMETER TAU */
/*     WHOSE USAGE IS DESCRIBED BELOW.  THE NB COLUMN VECTORS OF B */
/*     REPRESENT RIGHT-SIDE VECTORS FOR NB DISTINCT LINEAR LEAST SQUARES */
/*     PROBLEMS. */

/*     THIS SET OF PROBLEMS CAN ALSO BE WRITTEN AS THE MATRIX LEAST */
/*     SQUARES PROBLEM */

/*                       AX = B, */

/*     WHERE X IS THE N BY NB SOLUTION MATRIX. */

/*     NOTE THAT IF B IS THE M BY M IDENTITY MATRIX, THEN X WILL BE THE */
/*     PSEUDO-INVERSE OF A. */

/*     THIS SUBROUTINE FIRST TRANSFORMS THE AUGMENTED MATRIX (A B) TO A */
/*     MATRIX (R C) USING PREMULTIPLYING HOUSEHOLDER TRANSFORMATIONS WITH */
/*     COLUMN INTERCHANGES.  ALL SUBDIAGONAL ELEMENTS IN THE MATRIX R ARE */
/*     ZERO AND ITS DIAGONAL ELEMENTS SATISFY */

/*                       ABS(R(I,I)).GE.ABS(R(I+1,I+1)), */

/*                       I = 1,...,L-1, WHERE */

/*                       L = MIN(M,N). */

/*     THE SUBROUTINE WILL COMPUTE AN INTEGER, KRANK, EQUAL TO THE NUMBER */
/*     OF DIAGONAL TERMS OF R THAT EXCEED TAU IN MAGNITUDE.  THEN A */
/*     SOLUTION OF MINIMUM EUCLIDEAN LENGTH IS COMPUTED USING THE FIRST */
/*     KRANK ROWS OF (R C). */

/*     TO BE SPECIFIC WE SUGGEST THAT THE USER CONSIDER AN EASILY */
/*     COMPUTABLE MATRIX NORM, SUCH AS, THE MAXIMUM OF ALL COLUMN SUMS OF */
/*     MAGNITUDES. */

/*     NOW IF THE RELATIVE UNCERTAINTY OF B IS EPS, (NORM OF UNCERTAINTY/ */
/*     NORM OF B), IT IS SUGGESTED THAT TAU BE SET APPROXIMATELY EQUAL TO */
/*     EPS*(NORM OF A). */

/*     THE USER MUST DIMENSION ALL ARRAYS APPEARING IN THE CALL LIST.. */
/*     A(MDA,N),(B(MDB,NB) OR B(M)),RNORM(NB),H(N),G(N),IP(N).  THIS */
/*     PERMITS THE SOLUTION OF A RANGE OF PROBLEMS IN THE SAME ARRAY */
/*     SPACE. */

/*     THE ENTIRE SET OF PARAMETERS FOR HFTI ARE */

/*     INPUT.. */

/*     A(*,*),MDA,M,N    THE ARRAY A(*,*) INITIALLY CONTAINS THE M BY N */
/*                       MATRIX A OF THE LEAST SQUARES PROBLEM AX = B. */
/*                       THE FIRST DIMENSIONING PARAMETER OF THE ARRAY */
/*                       A(*,*) IS MDA, WHICH MUST SATISFY MDA.GE.M */
/*                       EITHER M.GE.N OR M.LT.N IS PERMITTED.  THERE */
/*                       IS NO RESTRICTION ON THE RANK OF A.  THE */
/*                       CONDITION MDA.LT.M IS CONSIDERED AN ERROR. */

/*     B(*),MDB,NB       IF NB = 0 THE SUBROUTINE WILL PERFORM THE */
/*                       ORTHOGONAL DECOMPOSITION BUT WILL MAKE NO */
/*                       REFERENCES TO THE ARRAY B(*).  IF NB.GT.0 */
/*                       THE ARRAY B(*) MUST INITIALLY CONTAIN THE M BY */
/*                       NB MATRIX B OF THE LEAST SQUARES PROBLEM AX = */
/*                       B.  IF NB.GE.2 THE ARRAY B(*) MUST BE DOUBLY */
/*                       SUBSCRIPTED WITH FIRST DIMENSIONING PARAMETER */
/*                       MDB.GE.MAX(M,N).  IF NB = 1 THE ARRAY B(*) MAY */
/*                       BE EITHER DOUBLY OR SINGLY SUBSCRIPTED.  IN */
/*                       THE LATTER CASE THE VALUE OF MDB IS ARBITRARY */
/*                       BUT IT SHOULD BE SET TO SOME VALID INTEGER */
/*                       VALUE SUCH AS MDB = M. */

/*                       THE CONDITION OF NB.GT.1.AND.MDB.LT. MAX(M,N) */
/*                       IS CONSIDERED AN ERROR. */

/*     TAU               ABSOLUTE TOLERANCE PARAMETER PROVIDED BY USER */
/*                       FOR PSEUDORANK DETERMINATION. */

/*     H(*),G(*),IP(*)   ARRAYS OF WORKING SPACE USED BY HFTI. */

/*     OUTPUT.. */

/*     A(*,*)            THE CONTENTS OF THE ARRAY A(*,*) WILL BE */
/*                       MODIFIED BY THE SUBROUTINE.  THESE CONTENTS */
/*                       ARE NOT GENERALLY REQUIRED BY THE USER. */

/*     B(*)              ON RETURN THE ARRAY B(*) WILL CONTAIN THE N BY */
/*                       NB SOLUTION MATRIX X. */

/*     KRANK             SET BY THE SUBROUTINE TO INDICATE THE */
/*                       PSEUDORANK OF A. */

/*     RNORM(*)          ON RETURN, RNORM(J) WILL CONTAIN THE EUCLIDEAN */
/*                       NORM OF THE RESIDUAL VECTOR FOR THE PROBLEM */
/*                       DEFINED BY THE J-TH COLUMN VECTOR OF THE ARRAY */
/*                       B(*,*) FOR J = 1,...,NB. */

/*     H(*),G(*)         ON RETURN THESE ARRAYS RESPECTIVELY CONTAIN */
/*                       ELEMENTS OF THE PRE- AND POST-MULTIPLYING */
/*                       HOUSEHOLDER TRANSFORMATIONS USED TO COMPUTE */
/*                       THE MINIMUM EUCLIDEAN LENGTH SOLUTION. */

/*     IP(*)             ARRAY IN WHICH THE SUBROUTINE RECORDS INDICES */
/*                       DESCRIBING THE PERMUTATION OF COLUMN VECTORS. */
/*                       THE CONTENTS OF ARRAYS H(*),G(*) AND IP(*) */
/*                       ARE NOT GENERALLY REQUIRED BY THE USER. */

/* ++ */
/*<       REAL             A(MDA,N), B(MDB,1), H(N), G(N), RNORM(NB), TAU >*/
/*<       REAL             FACTOR, HMAX, SM1, ZERO, SM, TMP >*/
/*<       REAL             DIFF, SQRT, ABS >*/
/*<       INTEGER   IP(N) >*/
/*<       ZERO = 0.E0 >*/
#line 2768 "cl2.f"
    /* Parameter adjustments */
#line 2768 "cl2.f"
    --ip;
#line 2768 "cl2.f"
    --g;
#line 2768 "cl2.f"
    --h__;
#line 2768 "cl2.f"
    a_dim1 = *mda;
#line 2768 "cl2.f"
    a_offset = 1 + a_dim1;
#line 2768 "cl2.f"
    a -= a_offset;
#line 2768 "cl2.f"
    b_dim1 = *mdb;
#line 2768 "cl2.f"
    b_offset = 1 + b_dim1;
#line 2768 "cl2.f"
    b -= b_offset;
#line 2768 "cl2.f"
    --rnorm;
#line 2768 "cl2.f"

#line 2768 "cl2.f"
    /* Function Body */
#line 2768 "cl2.f"
    zero = 0.f;
/*<       FACTOR = 0.001E0 >*/
#line 2769 "cl2.f"
    factor = .001f;

/*<       K = 0 >*/
#line 2771 "cl2.f"
    k = 0;
/*<       LDIAG = MIN0(M,N) >*/
#line 2772 "cl2.f"
    ldiag = min(*m,*n);
/*<       IF (LDIAG.LE.0) GO TO 310 >*/
#line 2773 "cl2.f"
    if (ldiag <= 0) {
#line 2773 "cl2.f"
	goto L310;
#line 2773 "cl2.f"
    }
/*<       IF (.NOT.MDA.LT.M) GO TO 10 >*/
#line 2774 "cl2.f"
    if (! (*mda < *m)) {
#line 2774 "cl2.f"
	goto L10;
#line 2774 "cl2.f"
    }
/*<       NERR = 2 >*/
#line 2775 "cl2.f"
    nerr = 2;
/*<       IOPT = 2 >*/
#line 2776 "cl2.f"
    iopt = 2;
/* cc      CALL XERROR(31HHFTI MDA.LT.M.. PROBABLE ERROR., 31, NERR, IOPT) */
/* cc      RETURN */
/*<    10 CONTINUE >*/
#line 2779 "cl2.f"
L10:

/*<       IF (.NOT.(NB.GT.1 .AND. MAX0(M,N).GT.MDB)) GO TO 20 >*/
#line 2781 "cl2.f"
    if (! (*nb > 1 && max(*m,*n) > *mdb)) {
#line 2781 "cl2.f"
	goto L20;
#line 2781 "cl2.f"
    }
/*<       NERR = 2 >*/
#line 2782 "cl2.f"
    nerr = 2;
/*<       IOPT = 2 >*/
#line 2783 "cl2.f"
    iopt = 2;
/* cc      CALL XERROR(49HHFTI MDB.LT.MAX(M,N).AND.NB.GT.1. PROBABLE ERROR., */
/* cc     * 49, NERR, IOPT) */
/*<       RETURN >*/
#line 2786 "cl2.f"
    return 0;
/*<    20 CONTINUE >*/
#line 2787 "cl2.f"
L20:

/*<       DO 100 J=1,LDIAG >*/
#line 2789 "cl2.f"
    i__1 = ldiag;
#line 2789 "cl2.f"
    for (j = 1; j <= i__1; ++j) {
/*<         IF (J.EQ.1) GO TO 40 >*/
#line 2790 "cl2.f"
	if (j == 1) {
#line 2790 "cl2.f"
	    goto L40;
#line 2790 "cl2.f"
	}

/*     UPDATE SQUARED COLUMN LENGTHS AND FIND LMAX */
/*    .. */
/*<         LMAX = J >*/
#line 2794 "cl2.f"
	lmax = j;
/*<         DO 30 L=J,N >*/
#line 2795 "cl2.f"
	i__2 = *n;
#line 2795 "cl2.f"
	for (l = j; l <= i__2; ++l) {
/*<           H(L) = H(L) - A(J-1,L)**2 >*/
/* Computing 2nd power */
#line 2796 "cl2.f"
	    r__1 = a[j - 1 + l * a_dim1];
#line 2796 "cl2.f"
	    h__[l] -= r__1 * r__1;
/*<           IF (H(L).GT.H(LMAX)) LMAX = L >*/
#line 2797 "cl2.f"
	    if (h__[l] > h__[lmax]) {
#line 2797 "cl2.f"
		lmax = l;
#line 2797 "cl2.f"
	    }
/*<    30   CONTINUE >*/
#line 2798 "cl2.f"
/* L30: */
#line 2798 "cl2.f"
	}
/*<         IF (DIFF(HMAX+FACTOR*H(LMAX),HMAX)) 40, 40, 70 >*/
#line 2799 "cl2.f"
	r__1 = hmax + factor * h__[lmax];
#line 2799 "cl2.f"
	if (diff_(&r__1, &hmax) <= 0.f) {
#line 2799 "cl2.f"
	    goto L40;
#line 2799 "cl2.f"
	} else {
#line 2799 "cl2.f"
	    goto L70;
#line 2799 "cl2.f"
	}

/*     COMPUTE SQUARED COLUMN LENGTHS AND FIND LMAX */
/*    .. */
/*<    40   LMAX = J >*/
#line 2803 "cl2.f"
L40:
#line 2803 "cl2.f"
	lmax = j;
/*<         DO 60 L=J,N >*/
#line 2804 "cl2.f"
	i__2 = *n;
#line 2804 "cl2.f"
	for (l = j; l <= i__2; ++l) {
/*<           H(L) = ZERO >*/
#line 2805 "cl2.f"
	    h__[l] = zero;
/*<           DO 50 I=J,M >*/
#line 2806 "cl2.f"
	    i__3 = *m;
#line 2806 "cl2.f"
	    for (i__ = j; i__ <= i__3; ++i__) {
/*<             H(L) = H(L) + A(I,L)**2 >*/
/* Computing 2nd power */
#line 2807 "cl2.f"
		r__1 = a[i__ + l * a_dim1];
#line 2807 "cl2.f"
		h__[l] += r__1 * r__1;
/*<    50     CONTINUE >*/
#line 2808 "cl2.f"
/* L50: */
#line 2808 "cl2.f"
	    }
/*<           IF (H(L).GT.H(LMAX)) LMAX = L >*/
#line 2809 "cl2.f"
	    if (h__[l] > h__[lmax]) {
#line 2809 "cl2.f"
		lmax = l;
#line 2809 "cl2.f"
	    }
/*<    60   CONTINUE >*/
#line 2810 "cl2.f"
/* L60: */
#line 2810 "cl2.f"
	}
/*<         HMAX = H(LMAX) >*/
#line 2811 "cl2.f"
	hmax = h__[lmax];
/*    .. */
/*     LMAX HAS BEEN DETERMINED */

/*     DO COLUMN INTERCHANGES IF NEEDED. */
/*    .. */
/*<    70   CONTINUE >*/
#line 2817 "cl2.f"
L70:
/*<         IP(J) = LMAX >*/
#line 2818 "cl2.f"
	ip[j] = lmax;
/*<         IF (IP(J).EQ.J) GO TO 90 >*/
#line 2819 "cl2.f"
	if (ip[j] == j) {
#line 2819 "cl2.f"
	    goto L90;
#line 2819 "cl2.f"
	}
/*<         DO 80 I=1,M >*/
#line 2820 "cl2.f"
	i__2 = *m;
#line 2820 "cl2.f"
	for (i__ = 1; i__ <= i__2; ++i__) {
/*<           TMP = A(I,J) >*/
#line 2821 "cl2.f"
	    tmp = a[i__ + j * a_dim1];
/*<           A(I,J) = A(I,LMAX) >*/
#line 2822 "cl2.f"
	    a[i__ + j * a_dim1] = a[i__ + lmax * a_dim1];
/*<           A(I,LMAX) = TMP >*/
#line 2823 "cl2.f"
	    a[i__ + lmax * a_dim1] = tmp;
/*<    80   CONTINUE >*/
#line 2824 "cl2.f"
/* L80: */
#line 2824 "cl2.f"
	}
/*<         H(LMAX) = H(J) >*/
#line 2825 "cl2.f"
	h__[lmax] = h__[j];
/*<    90 JCOL = MIN0(J+1,N) >*/
#line 2826 "cl2.f"
L90:
/* Computing MIN */
#line 2826 "cl2.f"
	i__2 = j + 1;
#line 2826 "cl2.f"
	jcol = min(i__2,*n);

/*     COMPUTE THE J-TH TRANSFORMATION AND APPLY IT TO A AND B. */
/*    .. */
/*<         CALL H12(1, J, J+1, M, A(1,J), 1, H(J), A(1,JCOL), 1, MDA, N-J) >*/
#line 2830 "cl2.f"
	i__2 = j + 1;
#line 2830 "cl2.f"
	i__3 = *n - j;
#line 2830 "cl2.f"
	h12_(&c__1, &j, &i__2, m, &a[j * a_dim1 + 1], &c__1, &h__[j], &a[jcol
		* a_dim1 + 1], &c__1, mda, &i__3);
/*<         CALL H12(2, J, J+1, M, A(1,J), 1, H(J), B, 1, MDB, NB) >*/
#line 2831 "cl2.f"
	i__2 = j + 1;
#line 2831 "cl2.f"
	h12_(&c__2, &j, &i__2, m, &a[j * a_dim1 + 1], &c__1, &h__[j], &b[
		b_offset], &c__1, mdb, nb);
/*<   100 CONTINUE >*/
#line 2832 "cl2.f"
/* L100: */
#line 2832 "cl2.f"
    }

/*     DETERMINE THE PSEUDORANK, K, USING THE TOLERANCE, TAU. */
/*    .. */
/*<       DO 110 J=1,LDIAG >*/
#line 2836 "cl2.f"
    i__1 = ldiag;
#line 2836 "cl2.f"
    for (j = 1; j <= i__1; ++j) {
/*<         IF (ABS(A(J,J)).LE.TAU) GO TO 120 >*/
#line 2837 "cl2.f"
	if ((r__1 = a[j + j * a_dim1], abs(r__1)) <= *tau) {
#line 2837 "cl2.f"
	    goto L120;
#line 2837 "cl2.f"
	}
/*<   110 CONTINUE >*/
#line 2838 "cl2.f"
/* L110: */
#line 2838 "cl2.f"
    }
/*<       K = LDIAG >*/
#line 2839 "cl2.f"
    k = ldiag;
/*<       GO TO 130 >*/
#line 2840 "cl2.f"
    goto L130;
/*<   120 K = J - 1 >*/
#line 2841 "cl2.f"
L120:
#line 2841 "cl2.f"
    k = j - 1;
/*<   130 KP1 = K + 1 >*/
#line 2842 "cl2.f"
L130:
#line 2842 "cl2.f"
    kp1 = k + 1;

/*     COMPUTE THE NORMS OF THE RESIDUAL VECTORS. */

/*<       IF (NB.LE.0) GO TO 170 >*/
#line 2846 "cl2.f"
    if (*nb <= 0) {
#line 2846 "cl2.f"
	goto L170;
#line 2846 "cl2.f"
    }
/*<       DO 160 JB=1,NB >*/
#line 2847 "cl2.f"
    i__1 = *nb;
#line 2847 "cl2.f"
    for (jb = 1; jb <= i__1; ++jb) {
/*<         TMP = ZERO >*/
#line 2848 "cl2.f"
	tmp = zero;
/*<         IF (KP1.GT.M) GO TO 150 >*/
#line 2849 "cl2.f"
	if (kp1 > *m) {
#line 2849 "cl2.f"
	    goto L150;
#line 2849 "cl2.f"
	}
/*<         DO 140 I=KP1,M >*/
#line 2850 "cl2.f"
	i__2 = *m;
#line 2850 "cl2.f"
	for (i__ = kp1; i__ <= i__2; ++i__) {
/*<           TMP = TMP + B(I,JB)**2 >*/
/* Computing 2nd power */
#line 2851 "cl2.f"
	    r__1 = b[i__ + jb * b_dim1];
#line 2851 "cl2.f"
	    tmp += r__1 * r__1;
/*<   140   CONTINUE >*/
#line 2852 "cl2.f"
/* L140: */
#line 2852 "cl2.f"
	}
/*<   150   RNORM(JB) = SQRT(TMP) >*/
#line 2853 "cl2.f"
L150:
#line 2853 "cl2.f"
	rnorm[jb] = sqrt(tmp);
/*<   160 CONTINUE >*/
#line 2854 "cl2.f"
/* L160: */
#line 2854 "cl2.f"
    }
/*<   170 CONTINUE >*/
#line 2855 "cl2.f"
L170:
/*                                           SPECIAL FOR PSEUDORANK = 0 */
/*<       IF (K.GT.0) GO TO 200 >*/
#line 2857 "cl2.f"
    if (k > 0) {
#line 2857 "cl2.f"
	goto L200;
#line 2857 "cl2.f"
    }
/*<       IF (NB.LE.0) GO TO 310 >*/
#line 2858 "cl2.f"
    if (*nb <= 0) {
#line 2858 "cl2.f"
	goto L310;
#line 2858 "cl2.f"
    }
/*<       DO 190 JB=1,NB >*/
#line 2859 "cl2.f"
    i__1 = *nb;
#line 2859 "cl2.f"
    for (jb = 1; jb <= i__1; ++jb) {
/*<         DO 180 I=1,N >*/
#line 2860 "cl2.f"
	i__2 = *n;
#line 2860 "cl2.f"
	for (i__ = 1; i__ <= i__2; ++i__) {
/*<           B(I,JB) = ZERO >*/
#line 2861 "cl2.f"
	    b[i__ + jb * b_dim1] = zero;
/*<   180   CONTINUE >*/
#line 2862 "cl2.f"
/* L180: */
#line 2862 "cl2.f"
	}
/*<   190 CONTINUE >*/
#line 2863 "cl2.f"
/* L190: */
#line 2863 "cl2.f"
    }
/*<       GO TO 310 >*/
#line 2864 "cl2.f"
    goto L310;

/*     IF THE PSEUDORANK IS LESS THAN N COMPUTE HOUSEHOLDER */
/*     DECOMPOSITION OF FIRST K ROWS. */
/*    .. */
/*<   200 IF (K.EQ.N) GO TO 220 >*/
#line 2869 "cl2.f"
L200:
#line 2869 "cl2.f"
    if (k == *n) {
#line 2869 "cl2.f"
	goto L220;
#line 2869 "cl2.f"
    }
/*<       DO 210 II=1,K >*/
#line 2870 "cl2.f"
    i__1 = k;
#line 2870 "cl2.f"
    for (ii = 1; ii <= i__1; ++ii) {
/*<         I = KP1 - II >*/
#line 2871 "cl2.f"
	i__ = kp1 - ii;
/*<         CALL H12(1, I, KP1, N, A(I,1), MDA, G(I), A, MDA, 1, I-1) >*/
#line 2872 "cl2.f"
	i__2 = i__ - 1;
#line 2872 "cl2.f"
	h12_(&c__1, &i__, &kp1, n, &a[i__ + a_dim1], mda, &g[i__], &a[
		a_offset], mda, &c__1, &i__2);
/*<   210 CONTINUE >*/
#line 2873 "cl2.f"
/* L210: */
#line 2873 "cl2.f"
    }
/*<   220 CONTINUE >*/
#line 2874 "cl2.f"
L220:


/*<       IF (NB.LE.0) GO TO 310 >*/
#line 2877 "cl2.f"
    if (*nb <= 0) {
#line 2877 "cl2.f"
	goto L310;
#line 2877 "cl2.f"
    }
/*<       DO 300 JB=1,NB >*/
#line 2878 "cl2.f"
    i__1 = *nb;
#line 2878 "cl2.f"
    for (jb = 1; jb <= i__1; ++jb) {

/*     SOLVE THE K BY K TRIANGULAR SYSTEM. */
/*    .. */
/*<         DO 250 L=1,K >*/
#line 2882 "cl2.f"
	i__2 = k;
#line 2882 "cl2.f"
	for (l = 1; l <= i__2; ++l) {
/*<           SM = ZERO >*/
#line 2883 "cl2.f"
	    sm = zero;
/*<           I = KP1 - L >*/
#line 2884 "cl2.f"
	    i__ = kp1 - l;
/*<           IF (I.EQ.K) GO TO 240 >*/
#line 2885 "cl2.f"
	    if (i__ == k) {
#line 2885 "cl2.f"
		goto L240;
#line 2885 "cl2.f"
	    }
/*<           IP1 = I + 1 >*/
#line 2886 "cl2.f"
	    ip1 = i__ + 1;
/*<           DO 230 J=IP1,K >*/
#line 2887 "cl2.f"
	    i__3 = k;
#line 2887 "cl2.f"
	    for (j = ip1; j <= i__3; ++j) {
/*<             SM = SM + A(I,J)*B(J,JB) >*/
#line 2888 "cl2.f"
		sm += a[i__ + j * a_dim1] * b[j + jb * b_dim1];
/*<   230     CONTINUE >*/
#line 2889 "cl2.f"
/* L230: */
#line 2889 "cl2.f"
	    }
/*<   240     SM1 = SM >*/
#line 2890 "cl2.f"
L240:
#line 2890 "cl2.f"
	    sm1 = sm;
/*<           B(I,JB) = (B(I,JB)-SM1)/A(I,I) >*/
#line 2891 "cl2.f"
	    b[i__ + jb * b_dim1] = (b[i__ + jb * b_dim1] - sm1) / a[i__ + i__
		    * a_dim1];
/*<   250   CONTINUE >*/
#line 2892 "cl2.f"
/* L250: */
#line 2892 "cl2.f"
	}

/*     COMPLETE COMPUTATION OF SOLUTION VECTOR. */
/*    .. */
/*<         IF (K.EQ.N) GO TO 280 >*/
#line 2896 "cl2.f"
	if (k == *n) {
#line 2896 "cl2.f"
	    goto L280;
#line 2896 "cl2.f"
	}
/*<         DO 260 J=KP1,N >*/
#line 2897 "cl2.f"
	i__2 = *n;
#line 2897 "cl2.f"
	for (j = kp1; j <= i__2; ++j) {
/*<           B(J,JB) = ZERO >*/
#line 2898 "cl2.f"
	    b[j + jb * b_dim1] = zero;
/*<   260   CONTINUE >*/
#line 2899 "cl2.f"
/* L260: */
#line 2899 "cl2.f"
	}
/*<         DO 270 I=1,K >*/
#line 2900 "cl2.f"
	i__2 = k;
#line 2900 "cl2.f"
	for (i__ = 1; i__ <= i__2; ++i__) {
/*<           CALL H12(2, I, KP1, N, A(I,1), MDA, G(I), B(1,JB), 1, MDB, 1) >*/
#line 2901 "cl2.f"
	    h12_(&c__2, &i__, &kp1, n, &a[i__ + a_dim1], mda, &g[i__], &b[jb *
		     b_dim1 + 1], &c__1, mdb, &c__1);
/*<   270   CONTINUE >*/
#line 2902 "cl2.f"
/* L270: */
#line 2902 "cl2.f"
	}

/*      RE-ORDER THE SOLUTION VECTOR TO COMPENSATE FOR THE */
/*      COLUMN INTERCHANGES. */
/*    .. */
/*<   280   DO 290 JJ=1,LDIAG >*/
#line 2907 "cl2.f"
L280:
#line 2907 "cl2.f"
	i__2 = ldiag;
#line 2907 "cl2.f"
	for (jj = 1; jj <= i__2; ++jj) {
/*<           J = LDIAG + 1 - JJ >*/
#line 2908 "cl2.f"
	    j = ldiag + 1 - jj;
/*<           IF (IP(J).EQ.J) GO TO 290 >*/
#line 2909 "cl2.f"
	    if (ip[j] == j) {
#line 2909 "cl2.f"
		goto L290;
#line 2909 "cl2.f"
	    }
/*<           L = IP(J) >*/
#line 2910 "cl2.f"
	    l = ip[j];
/*<           TMP = B(L,JB) >*/
#line 2911 "cl2.f"
	    tmp = b[l + jb * b_dim1];
/*<           B(L,JB) = B(J,JB) >*/
#line 2912 "cl2.f"
	    b[l + jb * b_dim1] = b[j + jb * b_dim1];
/*<           B(J,JB) = TMP >*/
#line 2913 "cl2.f"
	    b[j + jb * b_dim1] = tmp;
/*<   290   CONTINUE >*/
#line 2914 "cl2.f"
L290:
#line 2914 "cl2.f"
	    ;
#line 2914 "cl2.f"
	}
/*<   300 CONTINUE >*/
#line 2915 "cl2.f"
/* L300: */
#line 2915 "cl2.f"
    }
/*    .. */
/*     THE SOLUTION VECTORS, X, ARE NOW */
/*     IN THE FIRST  N  ROWS OF THE ARRAY B(,). */

/*<   310 KRANK = K >*/
#line 2920 "cl2.f"
L310:
#line 2920 "cl2.f"
    *krank = k;
/*<       RETURN >*/
#line 2921 "cl2.f"
    return 0;
/*<       END >*/
} /* hfti_ */

/* CC */
/* CC---------------------------------------------------------------------------- */
/* CC */
/*<       SUBROUTINE H12 (MODE,LPIVOT,L1,M,U,IUE,UP,C,ICE,ICV,NCV)           >*/
static /* Subroutine */ int h12_(integer *mode, integer *lpivot, integer *l1,
	integer *m, real *u, integer *iue, real *up, real *c__, integer *ice,
	integer *icv, integer *ncv)
{
    /* System generated locals */
    integer u_dim1, u_offset, i__1, i__2;
    real r__1, r__2;

    /* Builtin functions */
    double sqrt(doublereal);

    /* Local variables */
    static real b;
    static integer i__, j, i2, i3, i4;
    static real cl, sm;
    static integer kl1, kl2, l1m1;
    static real one;
    static integer klp, incr;
    static real ul1m1;
    static integer mml1p2;
    static real clinv;


/*     THE EDITING REQUIRED TO CONVERT THIS SUBROUTINE FROM SINGLE TO */
/*     DOUBLE PRECISION INVOLVES THE FOLLOWING CHARACTER STRING CHANGES. */
/*     USE AN EDITING COMMAND (CHANGE) /STRING-1/(TO)STRING-2/. */
/*     (START CHANGES AT LINE WITH C++ IN COLS. 1-3.) */
/*     /REAL (12 BLANKS)/DOUBLE PRECISION/,/SDOT/DDOT/,/ABS,/DABS,/, */
/*     /SSWAP/DSWAP/,/SQRT/DSQRT/,/ABS(/ DABS(/,/AMAX1/DMAX1/, */
/*     /SAXPY/DAXPY/,/E0/D0/ */


/*     C.L.LAWSON AND R.J.HANSON, JET PROPULSION LABORATORY, 1973 JUN 12 */
/*     TO APPEAR IN 'SOLVING LEAST SQUARES PROBLEMS', PRENTICE-HALL, 1974 */

/*     MODIFIED AT SANDIA LABS., MAY 1977, TO -- */

/*     1)  REMOVE DOUBLE PRECISION ACCUMULATION, AND */
/*     2)  INCLUDE USAGE OF THE BASIC LINEAR ALGEBRA PACKAGE FOR */
/*         VECTORS LONGER THAN A PARTICULAR THRESHOLD. */

/*     CONSTRUCTION AND/OR APPLICATION OF A SINGLE */
/*     HOUSEHOLDER TRANSFORMATION..     Q = I + U*(U**T)/B */

/*     MODE    = 1 OR 2   TO SELECT ALGORITHM  H1  OR  H2 . */
/*     LPIVOT IS THE INDEX OF THE PIVOT ELEMENT. */
/*     L1,M   IF L1 .LE. M   THE TRANSFORMATION WILL BE CONSTRUCTED TO */
/*            ZERO ELEMENTS INDEXED FROM L1 THROUGH M.   IF L1 GT. M */
/*            THE SUBROUTINE DOES AN IDENTITY TRANSFORMATION. */
/*     U(),IUE,UP    ON ENTRY TO H1 U() CONTAINS THE PIVOT VECTOR. */
/*                   IUE IS THE STORAGE INCREMENT BETWEEN ELEMENTS. */
/*                                       ON EXIT FROM H1 U() AND UP */
/*                   CONTAIN QUANTITIES DEFINING THE VECTOR U OF THE */
/*                   HOUSEHOLDER TRANSFORMATION.   ON ENTRY TO H2 U() */
/*                   AND UP SHOULD CONTAIN QUANTITIES PREVIOUSLY COMPUTED */
/*                   BY H1.  THESE WILL NOT BE MODIFIED BY H2. */
/*     C()    ON ENTRY TO H1 OR H2 C() CONTAINS A MATRIX WHICH WILL BE */
/*            REGARDED AS A SET OF VECTORS TO WHICH THE HOUSEHOLDER */
/*            TRANSFORMATION IS TO BE APPLIED.  ON EXIT C() CONTAINS THE */
/*            SET OF TRANSFORMED VECTORS. */
/*     ICE    STORAGE INCREMENT BETWEEN ELEMENTS OF VECTORS IN C(). */
/*     ICV    STORAGE INCREMENT BETWEEN VECTORS IN C(). */
/*     NCV    NUMBER OF VECTORS IN C() TO BE TRANSFORMED. IF NCV .LE. 0 */
/*            NO OPERATIONS WILL BE DONE ON C(). */

/*     SUBROUTINE H12 (MODE,LPIVOT,L1,M,U,IUE,UP,C,ICE,ICV,NCV) */
/* ++ */
/*<       REAL             U(IUE,M), C(1), UP >*/
/*<       REAL             B, CL, CLINV, ONE, SM, UL1M1 >*/
/*<       REAL             ABS, AMAX1, SQRT, SDOT >*/
/*<       ONE=1.E0 >*/
#line 2975 "cl2.f"
    /* Parameter adjustments */
#line 2975 "cl2.f"
    u_dim1 = *iue;
#line 2975 "cl2.f"
    u_offset = 1 + u_dim1;
#line 2975 "cl2.f"
    u -= u_offset;
#line 2975 "cl2.f"
    --c__;
#line 2975 "cl2.f"

#line 2975 "cl2.f"
    /* Function Body */
#line 2975 "cl2.f"
    one = 1.f;

/*<       IF (0.GE.LPIVOT.OR.LPIVOT.GE.L1.OR.L1.GT.M) RETURN >*/
#line 2977 "cl2.f"
    if (0 >= *lpivot || *lpivot >= *l1 || *l1 > *m) {
#line 2977 "cl2.f"
	return 0;
#line 2977 "cl2.f"
    }
/*<       CL=ABS(U(1,LPIVOT)) >*/
#line 2978 "cl2.f"
    cl = (r__1 = u[*lpivot * u_dim1 + 1], abs(r__1));
/*<       IF (MODE.EQ.2) GO TO 60 >*/
#line 2979 "cl2.f"
    if (*mode == 2) {
#line 2979 "cl2.f"
	goto L60;
#line 2979 "cl2.f"
    }
/*                            ****** CONSTRUCT THE TRANSFORMATION. ****** */
/*<           DO 10 J=L1,M >*/
#line 2981 "cl2.f"
    i__1 = *m;
#line 2981 "cl2.f"
    for (j = *l1; j <= i__1; ++j) {
/*<    10     CL=AMAX1(ABS(U(1,J)),CL) >*/
#line 2982 "cl2.f"
/* L10: */
/* Computing MAX */
#line 2982 "cl2.f"
	r__2 = (r__1 = u[j * u_dim1 + 1], abs(r__1));
#line 2982 "cl2.f"
	cl = max(r__2,cl);
#line 2982 "cl2.f"
    }
/*<       IF (CL) 130,130,20 >*/
#line 2983 "cl2.f"
    if (cl <= 0.f) {
#line 2983 "cl2.f"
	goto L130;
#line 2983 "cl2.f"
    } else {
#line 2983 "cl2.f"
	goto L20;
#line 2983 "cl2.f"
    }
/*<    20 CLINV=ONE/CL >*/
#line 2984 "cl2.f"
L20:
#line 2984 "cl2.f"
    clinv = one / cl;
/*<       SM=(U(1,LPIVOT)*CLINV)**2 >*/
/* Computing 2nd power */
#line 2985 "cl2.f"
    r__1 = u[*lpivot * u_dim1 + 1] * clinv;
#line 2985 "cl2.f"
    sm = r__1 * r__1;
/*<           DO 30 J=L1,M >*/
#line 2986 "cl2.f"
    i__1 = *m;
#line 2986 "cl2.f"
    for (j = *l1; j <= i__1; ++j) {
/*<    30     SM=SM+(U(1,J)*CLINV)**2 >*/
#line 2987 "cl2.f"
/* L30: */
/* Computing 2nd power */
#line 2987 "cl2.f"
	r__1 = u[j * u_dim1 + 1] * clinv;
#line 2987 "cl2.f"
	sm += r__1 * r__1;
#line 2987 "cl2.f"
    }
/*<       CL=CL*SQRT(SM) >*/
#line 2988 "cl2.f"
    cl *= sqrt(sm);
/*<       IF (U(1,LPIVOT)) 50,50,40 >*/
#line 2989 "cl2.f"
    if (u[*lpivot * u_dim1 + 1] <= 0.f) {
#line 2989 "cl2.f"
	goto L50;
#line 2989 "cl2.f"
    } else {
#line 2989 "cl2.f"
	goto L40;
#line 2989 "cl2.f"
    }
/*<    40 CL=-CL >*/
#line 2990 "cl2.f"
L40:
#line 2990 "cl2.f"
    cl = -cl;
/*<    50 UP=U(1,LPIVOT)-CL >*/
#line 2991 "cl2.f"
L50:
#line 2991 "cl2.f"
    *up = u[*lpivot * u_dim1 + 1] - cl;
/*<       U(1,LPIVOT)=CL >*/
#line 2992 "cl2.f"
    u[*lpivot * u_dim1 + 1] = cl;
/*<       GO TO 70 >*/
#line 2993 "cl2.f"
    goto L70;
/*            ****** APPLY THE TRANSFORMATION  I+U*(U**T)/B  TO C. ****** */

/*<    60 IF (CL) 130,130,70 >*/
#line 2996 "cl2.f"
L60:
#line 2996 "cl2.f"
    if (cl <= 0.f) {
#line 2996 "cl2.f"
	goto L130;
#line 2996 "cl2.f"
    } else {
#line 2996 "cl2.f"
	goto L70;
#line 2996 "cl2.f"
    }
/*<    70 IF (NCV.LE.0) RETURN >*/
#line 2997 "cl2.f"
L70:
#line 2997 "cl2.f"
    if (*ncv <= 0) {
#line 2997 "cl2.f"
	return 0;
#line 2997 "cl2.f"
    }
/*<       B=UP*U(1,LPIVOT) >*/
#line 2998 "cl2.f"
    b = *up * u[*lpivot * u_dim1 + 1];
/*                       B  MUST BE NONPOSITIVE HERE.  IF B = 0., RETURN. */

/*<       IF (B) 80,130,130 >*/
#line 3001 "cl2.f"
    if (b >= 0.f) {
#line 3001 "cl2.f"
	goto L130;
#line 3001 "cl2.f"
    } else {
#line 3001 "cl2.f"
	goto L80;
#line 3001 "cl2.f"
    }
/*<    80 B=ONE/B >*/
#line 3002 "cl2.f"
L80:
#line 3002 "cl2.f"
    b = one / b;
/*<       MML1P2=M-L1+2 >*/
#line 3003 "cl2.f"
    mml1p2 = *m - *l1 + 2;
/*<       IF (MML1P2.GT.20) GO TO 140 >*/
#line 3004 "cl2.f"
    if (mml1p2 > 20) {
#line 3004 "cl2.f"
	goto L140;
#line 3004 "cl2.f"
    }
/*<       I2=1-ICV+ICE*(LPIVOT-1) >*/
#line 3005 "cl2.f"
    i2 = 1 - *icv + *ice * (*lpivot - 1);
/*<       INCR=ICE*(L1-LPIVOT) >*/
#line 3006 "cl2.f"
    incr = *ice * (*l1 - *lpivot);
/*<           DO 120 J=1,NCV >*/
#line 3007 "cl2.f"
    i__1 = *ncv;
#line 3007 "cl2.f"
    for (j = 1; j <= i__1; ++j) {
/*<           I2=I2+ICV >*/
#line 3008 "cl2.f"
	i2 += *icv;
/*<           I3=I2+INCR >*/
#line 3009 "cl2.f"
	i3 = i2 + incr;
/*<           I4=I3 >*/
#line 3010 "cl2.f"
	i4 = i3;
/*<           SM=C(I2)*UP >*/
#line 3011 "cl2.f"
	sm = c__[i2] * *up;
/*<               DO 90 I=L1,M >*/
#line 3012 "cl2.f"
	i__2 = *m;
#line 3012 "cl2.f"
	for (i__ = *l1; i__ <= i__2; ++i__) {
/*<               SM=SM+C(I3)*U(1,I) >*/
#line 3013 "cl2.f"
	    sm += c__[i3] * u[i__ * u_dim1 + 1];
/*<    90         I3=I3+ICE >*/
#line 3014 "cl2.f"
/* L90: */
#line 3014 "cl2.f"
	    i3 += *ice;
#line 3014 "cl2.f"
	}
/*<           IF (SM) 100,120,100 >*/
#line 3015 "cl2.f"
	if (sm != 0.f) {
#line 3015 "cl2.f"
	    goto L100;
#line 3015 "cl2.f"
	} else {
#line 3015 "cl2.f"
	    goto L120;
#line 3015 "cl2.f"
	}
/*<   100     SM=SM*B >*/
#line 3016 "cl2.f"
L100:
#line 3016 "cl2.f"
	sm *= b;
/*<           C(I2)=C(I2)+SM*UP >*/
#line 3017 "cl2.f"
	c__[i2] += sm * *up;
/*<               DO 110 I=L1,M >*/
#line 3018 "cl2.f"
	i__2 = *m;
#line 3018 "cl2.f"
	for (i__ = *l1; i__ <= i__2; ++i__) {
/*<               C(I4)=C(I4)+SM*U(1,I) >*/
#line 3019 "cl2.f"
	    c__[i4] += sm * u[i__ * u_dim1 + 1];
/*<   110         I4=I4+ICE >*/
#line 3020 "cl2.f"
/* L110: */
#line 3020 "cl2.f"
	    i4 += *ice;
#line 3020 "cl2.f"
	}
/*<   120     CONTINUE >*/
#line 3021 "cl2.f"
L120:
#line 3021 "cl2.f"
	;
#line 3021 "cl2.f"
    }
/*<   130 RETURN >*/
#line 3022 "cl2.f"
L130:
#line 3022 "cl2.f"
    return 0;
/*<   140 CONTINUE >*/
#line 3023 "cl2.f"
L140:
/*<       L1M1=L1-1 >*/
#line 3024 "cl2.f"
    l1m1 = *l1 - 1;
/*<       KL1=1+(L1M1-1)*ICE >*/
#line 3025 "cl2.f"
    kl1 = (l1m1 - 1) * *ice + 1;
/*<       KL2=KL1 >*/
#line 3026 "cl2.f"
    kl2 = kl1;
/*<       KLP=1+(LPIVOT-1)*ICE >*/
#line 3027 "cl2.f"
    klp = (*lpivot - 1) * *ice + 1;
/*<       UL1M1=U(1,L1M1) >*/
#line 3028 "cl2.f"
    ul1m1 = u[l1m1 * u_dim1 + 1];
/*<       U(1,L1M1)=UP >*/
#line 3029 "cl2.f"
    u[l1m1 * u_dim1 + 1] = *up;
/*<       IF (LPIVOT.EQ.L1M1) GO TO 150 >*/
#line 3030 "cl2.f"
    if (*lpivot == l1m1) {
#line 3030 "cl2.f"
	goto L150;
#line 3030 "cl2.f"
    }
/*<       CALL SSWAP(NCV,C(KL1),ICV,C(KLP),ICV) >*/
#line 3031 "cl2.f"
    sswap_(ncv, &c__[kl1], icv, &c__[klp], icv);
/*<   150 CONTINUE >*/
#line 3032 "cl2.f"
L150:
/*<           DO 160 J=1,NCV >*/
#line 3033 "cl2.f"
    i__1 = *ncv;
#line 3033 "cl2.f"
    for (j = 1; j <= i__1; ++j) {
/*<           SM=SDOT(MML1P2,U(1,L1M1),IUE,C(KL1),ICE) >*/
#line 3034 "cl2.f"
	sm = sdot_(&mml1p2, &u[l1m1 * u_dim1 + 1], iue, &c__[kl1], ice);
/*<           SM=SM*B >*/
#line 3035 "cl2.f"
	sm *= b;
/*<           CALL SAXPY (MML1P2,SM,U(1,L1M1),IUE,C(KL1),ICE) >*/
#line 3036 "cl2.f"
	saxpy_(&mml1p2, &sm, &u[l1m1 * u_dim1 + 1], iue, &c__[kl1], ice);
/*<           KL1=KL1+ICV >*/
#line 3037 "cl2.f"
	kl1 += *icv;
/*<   160 CONTINUE >*/
#line 3038 "cl2.f"
/* L160: */
#line 3038 "cl2.f"
    }
/*<       U(1,L1M1)=UL1M1 >*/
#line 3039 "cl2.f"
    u[l1m1 * u_dim1 + 1] = ul1m1;
/*<       IF (LPIVOT.EQ.L1M1) RETURN >*/
#line 3040 "cl2.f"
    if (*lpivot == l1m1) {
#line 3040 "cl2.f"
	return 0;
#line 3040 "cl2.f"
    }
/*<       KL1=KL2 >*/
#line 3041 "cl2.f"
    kl1 = kl2;
/*<       CALL SSWAP(NCV,C(KL1),ICV,C(KLP),ICV) >*/
#line 3042 "cl2.f"
    sswap_(ncv, &c__[kl1], icv, &c__[klp], icv);
/*<       RETURN >*/
#line 3043 "cl2.f"
    return 0;
/*<       END >*/
} /* h12_ */

/* CC */
/* CC---------------------------------------------------------------------------- */
/* CC */
/*<       SUBROUTINE SROTMG (SD1,SD2,SX1,SY1,SPARAM)                         >*/
static /* Subroutine */ int srotmg_(real *sd1, real *sd2, real *sx1, real *sy1, real
	*sparam)
{
    /* Initialized data */

    static real zero = 0.f;
    static real one = 1.f;
    static real two = 2.f;
    static integer iflag = 1;
    static real gam = 4096.f;
    static real gamsq = 1.678e7f;
    static real rgam = 2.441e-4f;
    static real rgamsq = 5.96e-8f;

    /* Format strings */
    static char fmt_120[] = "";
    static char fmt_150[] = "";
    static char fmt_180[] = "";
    static char fmt_210[] = "";

    /* System generated locals */
    real r__1;

    /* Local variables */
    static real su, sp1, sp2, sq2, sq1, sh11, sh21, sh12, sh22;
    static integer igo;
    static real sflag, stemp;

    /* Assigned format variables */
    static char *igo_fmt;


/*     CONSTRUCT THE MODIFIED GIVENS TRANSFORMATION MATRIX H WHICH ZEROS */
/*     THE SECOND COMPONENT OF THE 2-VECTOR  (SQRT(SD1)*SX1,SQRT(SD2)* */
/*     SY2)**T. */
/*     WITH SPARAM(1)=SFLAG, H HAS ONE OF THE FOLLOWING FORMS.. */

/*     SFLAG=-1.E0     SFLAG=0.E0        SFLAG=1.E0     SFLAG=-2.E0 */

/*       (SH11  SH12)    (1.E0  SH12)    (SH11  1.E0)    (1.E0  0.E0) */
/*     H=(          )    (          )    (          )    (          ) */
/*       (SH21  SH22),   (SH21  1.E0),   (-1.E0 SH22),   (0.E0  1.E0). */

/* END OF ABSTRACT */
/*<       DIMENSION SPARAM(5) >*/

/*<       DATA ZERO,ONE,TWO /0.E0,1.E0,2.E0/,IFLAG/1/ >*/
#line 3081 "cl2.f"
    /* Parameter adjustments */
#line 3081 "cl2.f"
    --sparam;
#line 3081 "cl2.f"

#line 3081 "cl2.f"
    /* Function Body */
/*<       DATA GAM,GAMSQ,RGAM,RGAMSQ/4096.E0,1.678E7,2.441E-4,5.960E-8/ >*/

/*<       IF(.NOT. SD1 .LT. ZERO) GO TO 10 >*/
#line 3084 "cl2.f"
    if (! (*sd1 < zero)) {
#line 3084 "cl2.f"
	goto L10;
#line 3084 "cl2.f"
    }
/*       GO ZERO-H-D-AND-SX1.. */
/*<           GO TO 60 >*/
#line 3086 "cl2.f"
    goto L60;
/*<    10 CONTINUE >*/
#line 3087 "cl2.f"
L10:
/*     CASE-SD1-NONNEGATIVE */
/*<       SP2=SD2*SY1 >*/
#line 3089 "cl2.f"
    sp2 = *sd2 * *sy1;
/*<       IF(.NOT. SP2 .EQ. ZERO) GO TO 20 >*/
#line 3090 "cl2.f"
    if (! (sp2 == zero)) {
#line 3090 "cl2.f"
	goto L20;
#line 3090 "cl2.f"
    }
/*<           SFLAG=-TWO >*/
#line 3091 "cl2.f"
    sflag = -two;
/*<           GO TO 260 >*/
#line 3092 "cl2.f"
    goto L260;
/*     REGULAR-CASE.. */
/*<    20 CONTINUE >*/
#line 3094 "cl2.f"
L20:
/*<       SP1=SD1*SX1 >*/
#line 3095 "cl2.f"
    sp1 = *sd1 * *sx1;
/*<       SQ2=SP2*SY1 >*/
#line 3096 "cl2.f"
    sq2 = sp2 * *sy1;
/*<       SQ1=SP1*SX1 >*/
#line 3097 "cl2.f"
    sq1 = sp1 * *sx1;

/*<       IF(.NOT. ABS(SQ1) .GT. ABS(SQ2)) GO TO 40 >*/
#line 3099 "cl2.f"
    if (! (abs(sq1) > abs(sq2))) {
#line 3099 "cl2.f"
	goto L40;
#line 3099 "cl2.f"
    }
/*<           SH21=-SY1/SX1 >*/
#line 3100 "cl2.f"
    sh21 = -(*sy1) / *sx1;
/*<           SH12=SP2/SP1 >*/
#line 3101 "cl2.f"
    sh12 = sp2 / sp1;

/*<           SU=ONE-SH12*SH21 >*/
#line 3103 "cl2.f"
    su = one - sh12 * sh21;

/*<           IF(.NOT. SU .LE. ZERO) GO TO 30 >*/
#line 3105 "cl2.f"
    if (! (su <= zero)) {
#line 3105 "cl2.f"
	goto L30;
#line 3105 "cl2.f"
    }
/*         GO ZERO-H-D-AND-SX1.. */
/*<                GO TO 60 >*/
#line 3107 "cl2.f"
    goto L60;
/*<    30     CONTINUE >*/
#line 3108 "cl2.f"
L30:
/*<                SFLAG=ZERO >*/
#line 3109 "cl2.f"
    sflag = zero;
/*<                SD1=SD1/SU >*/
#line 3110 "cl2.f"
    *sd1 /= su;
/*<                SD2=SD2/SU >*/
#line 3111 "cl2.f"
    *sd2 /= su;
/*<                SX1=SX1*SU >*/
#line 3112 "cl2.f"
    *sx1 *= su;
/*         GO SCALE-CHECK.. */
/*<                GO TO 100 >*/
#line 3114 "cl2.f"
    goto L100;
/*<    40 CONTINUE >*/
#line 3115 "cl2.f"
L40:
/*<           IF(.NOT. SQ2 .LT. ZERO) GO TO 50 >*/
#line 3116 "cl2.f"
    if (! (sq2 < zero)) {
#line 3116 "cl2.f"
	goto L50;
#line 3116 "cl2.f"
    }
/*         GO ZERO-H-D-AND-SX1.. */
/*<                GO TO 60 >*/
#line 3118 "cl2.f"
    goto L60;
/*<    50     CONTINUE >*/
#line 3119 "cl2.f"
L50:
/*<                SFLAG=ONE >*/
#line 3120 "cl2.f"
    sflag = one;
/*<                SH11=SP1/SP2 >*/
#line 3121 "cl2.f"
    sh11 = sp1 / sp2;
/*<                SH22=SX1/SY1 >*/
#line 3122 "cl2.f"
    sh22 = *sx1 / *sy1;
/*<                SU=ONE+SH11*SH22 >*/
#line 3123 "cl2.f"
    su = one + sh11 * sh22;
/*<                STEMP=SD2/SU >*/
#line 3124 "cl2.f"
    stemp = *sd2 / su;
/*<                SD2=SD1/SU >*/
#line 3125 "cl2.f"
    *sd2 = *sd1 / su;
/*<                SD1=STEMP >*/
#line 3126 "cl2.f"
    *sd1 = stemp;
/*<                SX1=SY1*SU >*/
#line 3127 "cl2.f"
    *sx1 = *sy1 * su;
/*         GO SCALE-CHECK */
/*<                GO TO 100 >*/
#line 3129 "cl2.f"
    goto L100;
/*     PROCEDURE..ZERO-H-D-AND-SX1.. */
/*<    60 CONTINUE >*/
#line 3131 "cl2.f"
L60:
/*<           SFLAG=-ONE >*/
#line 3132 "cl2.f"
    sflag = -one;
/*<           SH11=ZERO >*/
#line 3133 "cl2.f"
    sh11 = zero;
/*<           SH12=ZERO >*/
#line 3134 "cl2.f"
    sh12 = zero;
/*<           SH21=ZERO >*/
#line 3135 "cl2.f"
    sh21 = zero;
/*<           SH22=ZERO >*/
#line 3136 "cl2.f"
    sh22 = zero;

/*<           SD1=ZERO >*/
#line 3138 "cl2.f"
    *sd1 = zero;
/*<           SD2=ZERO >*/
#line 3139 "cl2.f"
    *sd2 = zero;
/*<           SX1=ZERO >*/
#line 3140 "cl2.f"
    *sx1 = zero;
/*         RETURN.. */
/*<           GO TO 220 >*/
#line 3142 "cl2.f"
    goto L220;
/*     PROCEDURE..FIX-H.. */
/*<    70 CONTINUE >*/
#line 3144 "cl2.f"
L70:
/*<       IF(.NOT. SFLAG .GE. ZERO) GO TO 90 >*/
#line 3145 "cl2.f"
    if (! (sflag >= zero)) {
#line 3145 "cl2.f"
	goto L90;
#line 3145 "cl2.f"
    }

/*<           IF(.NOT. SFLAG .EQ. ZERO) GO TO 80 >*/
#line 3147 "cl2.f"
    if (! (sflag == zero)) {
#line 3147 "cl2.f"
	goto L80;
#line 3147 "cl2.f"
    }
/*<           SH11=ONE >*/
#line 3148 "cl2.f"
    sh11 = one;
/*<           SH22=ONE >*/
#line 3149 "cl2.f"
    sh22 = one;
/*<           SFLAG=-ONE >*/
#line 3150 "cl2.f"
    sflag = -one;
/*<           GO TO 90 >*/
#line 3151 "cl2.f"
    goto L90;
/*<    80     CONTINUE >*/
#line 3152 "cl2.f"
L80:
/*<           SH21=-ONE >*/
#line 3153 "cl2.f"
    sh21 = -one;
/*<           SH12=ONE >*/
#line 3154 "cl2.f"
    sh12 = one;
/*<           SFLAG=-ONE >*/
#line 3155 "cl2.f"
    sflag = -one;
/*<    90 CONTINUE >*/
#line 3156 "cl2.f"
L90:
/*<       GO TO IGO,(120,150,180,210) >*/
#line 3157 "cl2.f"
    switch (igo) {
#line 3157 "cl2.f"
	case 0: goto L120;
#line 3157 "cl2.f"
	case 1: goto L150;
#line 3157 "cl2.f"
	case 2: goto L180;
#line 3157 "cl2.f"
	case 3: goto L210;
#line 3157 "cl2.f"
    }
/*     PROCEDURE..SCALE-CHECK */
/*<   100 CONTINUE >*/
#line 3159 "cl2.f"
L100:
/*<                IF(.NOT. IFLAG.EQ.1) GO TO 105 >*/
#line 3160 "cl2.f"
    if (! (iflag == 1)) {
#line 3160 "cl2.f"
	goto L105;
#line 3160 "cl2.f"
    }

/*                   RECOMPUTE RESCALING PARAMETERS */
/*                   MORE ACCURATELY.. */

/*<                     RGAM = ONE/GAM >*/
#line 3165 "cl2.f"
    rgam = one / gam;
/*<                     GAMSQ = GAM**2 >*/
/* Computing 2nd power */
#line 3166 "cl2.f"
    r__1 = gam;
#line 3166 "cl2.f"
    gamsq = r__1 * r__1;
/*<                     RGAMSQ = RGAM**2 >*/
/* Computing 2nd power */
#line 3167 "cl2.f"
    r__1 = rgam;
#line 3167 "cl2.f"
    rgamsq = r__1 * r__1;
/*<                     IFLAG = 2 >*/
#line 3168 "cl2.f"
    iflag = 2;
/*<   105          CONTINUE >*/
#line 3169 "cl2.f"
L105:
/*<   110     CONTINUE >*/
#line 3170 "cl2.f"
L110:
/*<           IF(.NOT. SD1 .LE. RGAMSQ) GO TO 130 >*/
#line 3171 "cl2.f"
    if (! (*sd1 <= rgamsq)) {
#line 3171 "cl2.f"
	goto L130;
#line 3171 "cl2.f"
    }
/*<                IF(SD1 .EQ. ZERO) GO TO 160 >*/
#line 3172 "cl2.f"
    if (*sd1 == zero) {
#line 3172 "cl2.f"
	goto L160;
#line 3172 "cl2.f"
    }
/*<                ASSIGN 120 TO IGO >*/
#line 3173 "cl2.f"
    igo = 0;
#line 3173 "cl2.f"
    igo_fmt = fmt_120;
/*              FIX-H.. */
/*<                GO TO 70 >*/
#line 3175 "cl2.f"
    goto L70;
/*<   120          CONTINUE >*/
#line 3176 "cl2.f"
L120:
/*<                SD1=SD1*GAMSQ >*/
#line 3177 "cl2.f"
    *sd1 *= gamsq;
/*<                SX1=SX1*RGAM >*/
#line 3178 "cl2.f"
    *sx1 *= rgam;
/*<                SH11=SH11*RGAM >*/
#line 3179 "cl2.f"
    sh11 *= rgam;
/*<                SH12=SH12*RGAM >*/
#line 3180 "cl2.f"
    sh12 *= rgam;
/*<           GO TO 110 >*/
#line 3181 "cl2.f"
    goto L110;
/*<   130 CONTINUE >*/
#line 3182 "cl2.f"
L130:
/*<   140     CONTINUE >*/
#line 3183 "cl2.f"
L140:
/*<           IF(.NOT. SD1 .GE. GAMSQ) GO TO 160 >*/
#line 3184 "cl2.f"
    if (! (*sd1 >= gamsq)) {
#line 3184 "cl2.f"
	goto L160;
#line 3184 "cl2.f"
    }
/*<                ASSIGN 150 TO IGO >*/
#line 3185 "cl2.f"
    igo = 1;
#line 3185 "cl2.f"
    igo_fmt = fmt_150;
/*              FIX-H.. */
/*<                GO TO 70 >*/
#line 3187 "cl2.f"
    goto L70;
/*<   150          CONTINUE >*/
#line 3188 "cl2.f"
L150:
/*<                SD1=SD1*RGAMSQ >*/
#line 3189 "cl2.f"
    *sd1 *= rgamsq;
/*<                SX1=SX1*GAM >*/
#line 3190 "cl2.f"
    *sx1 *= gam;
/*<                SH11=SH11*GAM >*/
#line 3191 "cl2.f"
    sh11 *= gam;
/*<                SH12=SH12*GAM >*/
#line 3192 "cl2.f"
    sh12 *= gam;
/*<           GO TO 140 >*/
#line 3193 "cl2.f"
    goto L140;
/*<   160 CONTINUE >*/
#line 3194 "cl2.f"
L160:
/*<   170     CONTINUE >*/
#line 3195 "cl2.f"
L170:
/*<           IF(.NOT. ABS(SD2) .LE. RGAMSQ) GO TO 190 >*/
#line 3196 "cl2.f"
    if (! (abs(*sd2) <= rgamsq)) {
#line 3196 "cl2.f"
	goto L190;
#line 3196 "cl2.f"
    }
/*<                IF(SD2 .EQ. ZERO) GO TO 220 >*/
#line 3197 "cl2.f"
    if (*sd2 == zero) {
#line 3197 "cl2.f"
	goto L220;
#line 3197 "cl2.f"
    }
/*<                ASSIGN 180 TO IGO >*/
#line 3198 "cl2.f"
    igo = 2;
#line 3198 "cl2.f"
    igo_fmt = fmt_180;
/*              FIX-H.. */
/*<                GO TO 70 >*/
#line 3200 "cl2.f"
    goto L70;
/*<   180          CONTINUE >*/
#line 3201 "cl2.f"
L180:
/*<                SD2=SD2*GAMSQ >*/
#line 3202 "cl2.f"
    *sd2 *= gamsq;
/*<                SH21=SH21*RGAM >*/
#line 3203 "cl2.f"
    sh21 *= rgam;
/*<                SH22=SH22*RGAM >*/
#line 3204 "cl2.f"
    sh22 *= rgam;
/*<           GO TO 170 >*/
#line 3205 "cl2.f"
    goto L170;
/*<   190 CONTINUE >*/
#line 3206 "cl2.f"
L190:
/*<   200     CONTINUE >*/
#line 3207 "cl2.f"
L200:
/*<           IF(.NOT. ABS(SD2) .GE. GAMSQ) GO TO 220 >*/
#line 3208 "cl2.f"
    if (! (abs(*sd2) >= gamsq)) {
#line 3208 "cl2.f"
	goto L220;
#line 3208 "cl2.f"
    }
/*<                ASSIGN 210 TO IGO >*/
#line 3209 "cl2.f"
    igo = 3;
#line 3209 "cl2.f"
    igo_fmt = fmt_210;
/*              FIX-H.. */
/*<                GO TO 70 >*/
#line 3211 "cl2.f"
    goto L70;
/*<   210          CONTINUE >*/
#line 3212 "cl2.f"
L210:
/*<                SD2=SD2*RGAMSQ >*/
#line 3213 "cl2.f"
    *sd2 *= rgamsq;
/*<                SH21=SH21*GAM >*/
#line 3214 "cl2.f"
    sh21 *= gam;
/*<                SH22=SH22*GAM >*/
#line 3215 "cl2.f"
    sh22 *= gam;
/*<           GO TO 200 >*/
#line 3216 "cl2.f"
    goto L200;
/*<   220 CONTINUE >*/
#line 3217 "cl2.f"
L220:
/*<           IF(SFLAG)250,230,240 >*/
#line 3218 "cl2.f"
    if (sflag < 0.f) {
#line 3218 "cl2.f"
	goto L250;
#line 3218 "cl2.f"
    } else if (sflag == 0) {
#line 3218 "cl2.f"
	goto L230;
#line 3218 "cl2.f"
    } else {
#line 3218 "cl2.f"
	goto L240;
#line 3218 "cl2.f"
    }
/*<   230     CONTINUE >*/
#line 3219 "cl2.f"
L230:
/*<                SPARAM(3)=SH21 >*/
#line 3220 "cl2.f"
    sparam[3] = sh21;
/*<                SPARAM(4)=SH12 >*/
#line 3221 "cl2.f"
    sparam[4] = sh12;
/*<                GO TO 260 >*/
#line 3222 "cl2.f"
    goto L260;
/*<   240     CONTINUE >*/
#line 3223 "cl2.f"
L240:
/*<                SPARAM(2)=SH11 >*/
#line 3224 "cl2.f"
    sparam[2] = sh11;
/*<                SPARAM(5)=SH22 >*/
#line 3225 "cl2.f"
    sparam[5] = sh22;
/*<                GO TO 260 >*/
#line 3226 "cl2.f"
    goto L260;
/*<   250     CONTINUE >*/
#line 3227 "cl2.f"
L250:
/*<                SPARAM(2)=SH11 >*/
#line 3228 "cl2.f"
    sparam[2] = sh11;
/*<                SPARAM(3)=SH21 >*/
#line 3229 "cl2.f"
    sparam[3] = sh21;
/*<                SPARAM(4)=SH12 >*/
#line 3230 "cl2.f"
    sparam[4] = sh12;
/*<                SPARAM(5)=SH22 >*/
#line 3231 "cl2.f"
    sparam[5] = sh22;
/*<   260 CONTINUE >*/
#line 3232 "cl2.f"
L260:
/*<           SPARAM(1)=SFLAG >*/
#line 3233 "cl2.f"
    sparam[1] = sflag;
/*<           RETURN >*/
#line 3234 "cl2.f"
    return 0;
/*<       END >*/
} /* srotmg_ */

/* CC */
/* CC---------------------------------------------------------------------------- */
/* CC */
/*<       SUBROUTINE  SCOPY(N,SX,INCX,SY,INCY)                               >*/
static /* Subroutine */ int scopy_(integer *n, real *sx, integer *incx, real *sy,
	integer *incy)
{
    /* System generated locals */
    integer i__1, i__2;

    /* Local variables */
    static integer i__, m, ix, iy, ns, mp1;


/*     COPY SINGLE PRECISION SX TO SINGLE PRECISION SY. */

/* END OF ABSTRACT */
/*<       REAL SX(1),SY(1) >*/

/*<       IF(N.LE.0)RETURN >*/
#line 3246 "cl2.f"
    /* Parameter adjustments */
#line 3246 "cl2.f"
    --sy;
#line 3246 "cl2.f"
    --sx;
#line 3246 "cl2.f"

#line 3246 "cl2.f"
    /* Function Body */
#line 3246 "cl2.f"
    if (*n <= 0) {
#line 3246 "cl2.f"
	return 0;
#line 3246 "cl2.f"
    }
/*<       IF(INCX.EQ.INCY) IF(INCX-1) 5,20,60 >*/
#line 3247 "cl2.f"
    if (*incx == *incy) {
#line 3247 "cl2.f"
	if ((i__1 = *incx - 1) < 0) {
#line 3247 "cl2.f"
	    goto L5;
#line 3247 "cl2.f"
	} else if (i__1 == 0) {
#line 3247 "cl2.f"
	    goto L20;
#line 3247 "cl2.f"
	} else {
#line 3247 "cl2.f"
	    goto L60;
#line 3247 "cl2.f"
	}
#line 3247 "cl2.f"
    }
/*<     5 CONTINUE >*/
#line 3248 "cl2.f"
L5:

/*        CODE FOR UNEQUAL OR NONPOSITIVE INCREMENTS. */

/*<       IX = 1 >*/
#line 3252 "cl2.f"
    ix = 1;
/*<       IY = 1 >*/
#line 3253 "cl2.f"
    iy = 1;
/*<       IF(INCX.LT.0)IX = (-N+1)*INCX + 1 >*/
#line 3254 "cl2.f"
    if (*incx < 0) {
#line 3254 "cl2.f"
	ix = (-(*n) + 1) * *incx + 1;
#line 3254 "cl2.f"
    }
/*<       IF(INCY.LT.0)IY = (-N+1)*INCY + 1 >*/
#line 3255 "cl2.f"
    if (*incy < 0) {
#line 3255 "cl2.f"
	iy = (-(*n) + 1) * *incy + 1;
#line 3255 "cl2.f"
    }
/*<       DO 10 I = 1,N >*/
#line 3256 "cl2.f"
    i__1 = *n;
#line 3256 "cl2.f"
    for (i__ = 1; i__ <= i__1; ++i__) {
/*<         SY(IY) = SX(IX) >*/
#line 3257 "cl2.f"
	sy[iy] = sx[ix];
/*<         IX = IX + INCX >*/
#line 3258 "cl2.f"
	ix += *incx;
/*<         IY = IY + INCY >*/
#line 3259 "cl2.f"
	iy += *incy;
/*<    10 CONTINUE >*/
#line 3260 "cl2.f"
/* L10: */
#line 3260 "cl2.f"
    }
/*<       RETURN >*/
#line 3261 "cl2.f"
    return 0;

/*        CODE FOR BOTH INCREMENTS EQUAL TO 1 */


/*        CLEAN-UP LOOP SO REMAINING VECTOR LENGTH IS A MULTIPLE OF 7. */

/*<    20 M = N - (N/7)*7 >*/
#line 3268 "cl2.f"
L20:
#line 3268 "cl2.f"
    m = *n - *n / 7 * 7;
/*<       IF( M .EQ. 0 ) GO TO 40 >*/
#line 3269 "cl2.f"
    if (m == 0) {
#line 3269 "cl2.f"
	goto L40;
#line 3269 "cl2.f"
    }
/*<       DO 30 I = 1,M >*/
#line 3270 "cl2.f"
    i__1 = m;
#line 3270 "cl2.f"
    for (i__ = 1; i__ <= i__1; ++i__) {
/*<         SY(I) = SX(I) >*/
#line 3271 "cl2.f"
	sy[i__] = sx[i__];
/*<    30 CONTINUE >*/
#line 3272 "cl2.f"
/* L30: */
#line 3272 "cl2.f"
    }
/*<       IF( N .LT. 7 ) RETURN >*/
#line 3273 "cl2.f"
    if (*n < 7) {
#line 3273 "cl2.f"
	return 0;
#line 3273 "cl2.f"
    }
/*<    40 MP1 = M + 1 >*/
#line 3274 "cl2.f"
L40:
#line 3274 "cl2.f"
    mp1 = m + 1;
/*<       DO 50 I = MP1,N,7 >*/
#line 3275 "cl2.f"
    i__1 = *n;
#line 3275 "cl2.f"
    for (i__ = mp1; i__ <= i__1; i__ += 7) {
/*<         SY(I) = SX(I) >*/
#line 3276 "cl2.f"
	sy[i__] = sx[i__];
/*<         SY(I + 1) = SX(I + 1) >*/
#line 3277 "cl2.f"
	sy[i__ + 1] = sx[i__ + 1];
/*<         SY(I + 2) = SX(I + 2) >*/
#line 3278 "cl2.f"
	sy[i__ + 2] = sx[i__ + 2];
/*<         SY(I + 3) = SX(I + 3) >*/
#line 3279 "cl2.f"
	sy[i__ + 3] = sx[i__ + 3];
/*<         SY(I + 4) = SX(I + 4) >*/
#line 3280 "cl2.f"
	sy[i__ + 4] = sx[i__ + 4];
/*<         SY(I + 5) = SX(I + 5) >*/
#line 3281 "cl2.f"
	sy[i__ + 5] = sx[i__ + 5];
/*<         SY(I + 6) = SX(I + 6) >*/
#line 3282 "cl2.f"
	sy[i__ + 6] = sx[i__ + 6];
/*<    50 CONTINUE >*/
#line 3283 "cl2.f"
/* L50: */
#line 3283 "cl2.f"
    }
/*<       RETURN >*/
#line 3284 "cl2.f"
    return 0;

/*        CODE FOR EQUAL, POSITIVE, NONUNIT INCREMENTS. */

/*<    60 CONTINUE >*/
#line 3288 "cl2.f"
L60:
/*<       NS = N*INCX >*/
#line 3289 "cl2.f"
    ns = *n * *incx;
/*<           DO 70 I=1,NS,INCX >*/
#line 3290 "cl2.f"
    i__1 = ns;
#line 3290 "cl2.f"
    i__2 = *incx;
#line 3290 "cl2.f"
    for (i__ = 1; i__2 < 0 ? i__ >= i__1 : i__ <= i__1; i__ += i__2) {
/*<           SY(I) = SX(I) >*/
#line 3291 "cl2.f"
	sy[i__] = sx[i__];
/*<    70     CONTINUE >*/
#line 3292 "cl2.f"
/* L70: */
#line 3292 "cl2.f"
    }
/*<       RETURN >*/
#line 3293 "cl2.f"
    return 0;
/*<       END >*/
} /* scopy_ */

/* CC */
/* CC---------------------------------------------------------------------------- */
/* CC */
/*<       SUBROUTINE  SSWAP (N,SX,INCX,SY,INCY)                              >*/
static /* Subroutine */ int sswap_(integer *n, real *sx, integer *incx, real *sy,
	integer *incy)
{
    /* System generated locals */
    integer i__1, i__2;

    /* Local variables */
    static integer i__, m, ix, iy, ns, mp1;
    static real stemp1, stemp2, stemp3;


/*     INTERCHANGE SINGLE PRECISION SX AND SINGLE PRECISION SY. */

/* END OF ABSTRACT */
/*<       REAL SX(1),SY(1),STEMP1,STEMP2,STEMP3 >*/

/*<       IF(N.LE.0)RETURN >*/
#line 3305 "cl2.f"
    /* Parameter adjustments */
#line 3305 "cl2.f"
    --sy;
#line 3305 "cl2.f"
    --sx;
#line 3305 "cl2.f"

#line 3305 "cl2.f"
    /* Function Body */
#line 3305 "cl2.f"
    if (*n <= 0) {
#line 3305 "cl2.f"
	return 0;
#line 3305 "cl2.f"
    }
/*<       IF(INCX.EQ.INCY) IF(INCX-1) 5,20,60 >*/
#line 3306 "cl2.f"
    if (*incx == *incy) {
#line 3306 "cl2.f"
	if ((i__1 = *incx - 1) < 0) {
#line 3306 "cl2.f"
	    goto L5;
#line 3306 "cl2.f"
	} else if (i__1 == 0) {
#line 3306 "cl2.f"
	    goto L20;
#line 3306 "cl2.f"
	} else {
#line 3306 "cl2.f"
	    goto L60;
#line 3306 "cl2.f"
	}
#line 3306 "cl2.f"
    }
/*<     5 CONTINUE >*/
#line 3307 "cl2.f"
L5:

/*       CODE FOR UNEQUAL OR NONPOSITIVE INCREMENTS. */

/*<       IX = 1 >*/
#line 3311 "cl2.f"
    ix = 1;
/*<       IY = 1 >*/
#line 3312 "cl2.f"
    iy = 1;
/*<       IF(INCX.LT.0)IX = (-N+1)*INCX + 1 >*/
#line 3313 "cl2.f"
    if (*incx < 0) {
#line 3313 "cl2.f"
	ix = (-(*n) + 1) * *incx + 1;
#line 3313 "cl2.f"
    }
/*<       IF(INCY.LT.0)IY = (-N+1)*INCY + 1 >*/
#line 3314 "cl2.f"
    if (*incy < 0) {
#line 3314 "cl2.f"
	iy = (-(*n) + 1) * *incy + 1;
#line 3314 "cl2.f"
    }
/*<       DO 10 I = 1,N >*/
#line 3315 "cl2.f"
    i__1 = *n;
#line 3315 "cl2.f"
    for (i__ = 1; i__ <= i__1; ++i__) {
/*<         STEMP1 = SX(IX) >*/
#line 3316 "cl2.f"
	stemp1 = sx[ix];
/*<         SX(IX) = SY(IY) >*/
#line 3317 "cl2.f"
	sx[ix] = sy[iy];
/*<         SY(IY) = STEMP1 >*/
#line 3318 "cl2.f"
	sy[iy] = stemp1;
/*<         IX = IX + INCX >*/
#line 3319 "cl2.f"
	ix += *incx;
/*<         IY = IY + INCY >*/
#line 3320 "cl2.f"
	iy += *incy;
/*<    10 CONTINUE >*/
#line 3321 "cl2.f"
/* L10: */
#line 3321 "cl2.f"
    }
/*<       RETURN >*/
#line 3322 "cl2.f"
    return 0;

/*       CODE FOR BOTH INCREMENTS EQUAL TO 1 */


/*       CLEAN-UP LOOP SO REMAINING VECTOR LENGTH IS A MULTIPLE OF 3. */

/*<    20 M = N - (N/3)*3 >*/
#line 3329 "cl2.f"
L20:
#line 3329 "cl2.f"
    m = *n - *n / 3 * 3;
/*<       IF( M .EQ. 0 ) GO TO 40 >*/
#line 3330 "cl2.f"
    if (m == 0) {
#line 3330 "cl2.f"
	goto L40;
#line 3330 "cl2.f"
    }
/*<       DO 30 I = 1,M >*/
#line 3331 "cl2.f"
    i__1 = m;
#line 3331 "cl2.f"
    for (i__ = 1; i__ <= i__1; ++i__) {
/*<         STEMP1 = SX(I) >*/
#line 3332 "cl2.f"
	stemp1 = sx[i__];
/*<         SX(I) = SY(I) >*/
#line 3333 "cl2.f"
	sx[i__] = sy[i__];
/*<         SY(I) = STEMP1 >*/
#line 3334 "cl2.f"
	sy[i__] = stemp1;
/*<    30 CONTINUE >*/
#line 3335 "cl2.f"
/* L30: */
#line 3335 "cl2.f"
    }
/*<       IF( N .LT. 3 ) RETURN >*/
#line 3336 "cl2.f"
    if (*n < 3) {
#line 3336 "cl2.f"
	return 0;
#line 3336 "cl2.f"
    }
/*<    40 MP1 = M + 1 >*/
#line 3337 "cl2.f"
L40:
#line 3337 "cl2.f"
    mp1 = m + 1;
/*<       DO 50 I = MP1,N,3 >*/
#line 3338 "cl2.f"
    i__1 = *n;
#line 3338 "cl2.f"
    for (i__ = mp1; i__ <= i__1; i__ += 3) {
/*<         STEMP1 = SX(I) >*/
#line 3339 "cl2.f"
	stemp1 = sx[i__];
/*<         STEMP2 = SX(I+1) >*/
#line 3340 "cl2.f"
	stemp2 = sx[i__ + 1];
/*<         STEMP3 = SX(I+2) >*/
#line 3341 "cl2.f"
	stemp3 = sx[i__ + 2];
/*<         SX(I) = SY(I) >*/
#line 3342 "cl2.f"
	sx[i__] = sy[i__];
/*<         SX(I+1) = SY(I+1) >*/
#line 3343 "cl2.f"
	sx[i__ + 1] = sy[i__ + 1];
/*<         SX(I+2) = SY(I+2) >*/
#line 3344 "cl2.f"
	sx[i__ + 2] = sy[i__ + 2];
/*<         SY(I) = STEMP1 >*/
#line 3345 "cl2.f"
	sy[i__] = stemp1;
/*<         SY(I+1) = STEMP2 >*/
#line 3346 "cl2.f"
	sy[i__ + 1] = stemp2;
/*<         SY(I+2) = STEMP3 >*/
#line 3347 "cl2.f"
	sy[i__ + 2] = stemp3;
/*<    50 CONTINUE >*/
#line 3348 "cl2.f"
/* L50: */
#line 3348 "cl2.f"
    }
/*<       RETURN >*/
#line 3349 "cl2.f"
    return 0;
/*<    60 CONTINUE >*/
#line 3350 "cl2.f"
L60:

/*     CODE FOR EQUAL, POSITIVE, NONUNIT INCREMENTS. */

/*<       NS = N*INCX >*/
#line 3354 "cl2.f"
    ns = *n * *incx;
/*<         DO 70 I=1,NS,INCX >*/
#line 3355 "cl2.f"
    i__1 = ns;
#line 3355 "cl2.f"
    i__2 = *incx;
#line 3355 "cl2.f"
    for (i__ = 1; i__2 < 0 ? i__ >= i__1 : i__ <= i__1; i__ += i__2) {
/*<         STEMP1 = SX(I) >*/
#line 3356 "cl2.f"
	stemp1 = sx[i__];
/*<         SX(I) = SY(I) >*/
#line 3357 "cl2.f"
	sx[i__] = sy[i__];
/*<         SY(I) = STEMP1 >*/
#line 3358 "cl2.f"
	sy[i__] = stemp1;
/*<    70   CONTINUE >*/
#line 3359 "cl2.f"
/* L70: */
#line 3359 "cl2.f"
    }
/*<       RETURN >*/
#line 3360 "cl2.f"
    return 0;
/*<       END >*/
} /* sswap_ */

/* CC */
/* CC---------------------------------------------------------------------------- */
/* CC */
/*<       REAL FUNCTION SNRM2 ( N, SX, INCX)                                 >*/
static real snrm2_(integer *n, real *sx, integer *incx)
{
    /* Initialized data */

    static real zero = 0.f;
    static real one = 1.f;
    static real cutlo = 4.441e-16f;
    static real cuthi = 1.304e19f;

    /* Format strings */
    static char fmt_30[] = "";
    static char fmt_50[] = "";
    static char fmt_70[] = "";
    static char fmt_110[] = "";

    /* System generated locals */
    integer i__1, i__2;
    real ret_val, r__1;

    /* Builtin functions */
    double sqrt(doublereal);

    /* Local variables */
    static integer i__, j, nn;
    static real sum, xmax;
    static integer next;
    static real hitest;

    /* Assigned format variables */
    static char *next_fmt;

/*<       INTEGER          NEXT >*/
/*<       REAL   SX(1),  CUTLO, CUTHI, HITEST, SUM, XMAX, ZERO, ONE >*/
/*<       DATA   ZERO, ONE /0.0E0, 1.0E0/ >*/
#line 3368 "cl2.f"
    /* Parameter adjustments */
#line 3368 "cl2.f"
    --sx;
#line 3368 "cl2.f"

#line 3368 "cl2.f"
    /* Function Body */

/*     EUCLIDEAN NORM OF THE N-VECTOR STORED IN SX() WITH STORAGE */
/*     INCREMENT INCX . */
/*     IF    N .LE. 0 RETURN WITH RESULT = 0. */
/*     IF N .GE. 1 THEN INCX MUST BE .GE. 1 */

/*           C.L.LAWSON, 1978 JAN 08 */

/*     FOUR PHASE METHOD     USING TWO BUILT-IN CONSTANTS THAT ARE */
/*     HOPEFULLY APPLICABLE TO ALL MACHINES. */
/*         CUTLO = MAXIMUM OF  SQRT(U/EPS)  OVER ALL KNOWN MACHINES. */
/*         CUTHI = MINIMUM OF  SQRT(V)      OVER ALL KNOWN MACHINES. */
/*     WHERE */
/*         EPS = SMALLEST NO. SUCH THAT EPS + 1. .GT. 1. */
/*         U   = SMALLEST POSITIVE NO.   (UNDERFLOW LIMIT) */
/*         V   = LARGEST  NO.            (OVERFLOW  LIMIT) */

/*     BRIEF OUTLINE OF ALGORITHM.. */

/*     PHASE 1    SCANS ZERO COMPONENTS. */
/*     MOVE TO PHASE 2 WHEN A COMPONENT IS NONZERO AND .LE. CUTLO */
/*     MOVE TO PHASE 3 WHEN A COMPONENT IS .GT. CUTLO */
/*     MOVE TO PHASE 4 WHEN A COMPONENT IS .GE. CUTHI/M */
/*     WHERE M = N FOR X() REAL AND M = 2*N FOR COMPLEX. */

/*     VALUES FOR CUTLO AND CUTHI.. */
/*     FROM THE ENVIRONMENTAL PARAMETERS LISTED IN THE IMSL CONVERTER */
/*     DOCUMENT THE LIMITING VALUES ARE AS FOLLOWS.. */
/*     CUTLO, S.P.   U/EPS = 2**(-102) FOR  HONEYWELL.  CLOSE SECONDS ARE */
/*                   UNIVAC AND DEC AT 2**(-103) */
/*                   THUS CUTLO = 2**(-51) = 4.44089E-16 */
/*     CUTHI, S.P.   V = 2**127 FOR UNIVAC, HONEYWELL, AND DEC. */
/*                   THUS CUTHI = 2**(63.5) = 1.30438E19 */
/*     CUTLO, D.P.   U/EPS = 2**(-67) FOR HONEYWELL AND DEC. */
/*                   THUS CUTLO = 2**(-33.5) = 8.23181D-11 */
/*     CUTHI, D.P.   SAME AS S.P.  CUTHI = 1.30438D19 */
/*     DATA CUTLO, CUTHI / 8.232D-11,  1.304D19 / */
/*     DATA CUTLO, CUTHI / 4.441E-16,  1.304E19 / */
/* END OF ABSTRACT */
/*<       DATA CUTLO, CUTHI / 4.441E-16,  1.304E19 / >*/

/*<       IF(N .GT. 0) GO TO 10 >*/
#line 3410 "cl2.f"
    if (*n > 0) {
#line 3410 "cl2.f"
	goto L10;
#line 3410 "cl2.f"
    }
/*<          SNRM2  = ZERO >*/
#line 3411 "cl2.f"
    ret_val = zero;
/*<          GO TO 300 >*/
#line 3412 "cl2.f"
    goto L300;

/*<    10 ASSIGN 30 TO NEXT >*/
#line 3414 "cl2.f"
L10:
#line 3414 "cl2.f"
    next = 0;
#line 3414 "cl2.f"
    next_fmt = fmt_30;
/*<       SUM = ZERO >*/
#line 3415 "cl2.f"
    sum = zero;
/*<       NN = N * INCX >*/
#line 3416 "cl2.f"
    nn = *n * *incx;
/*                                                 BEGIN MAIN LOOP */
/*<       I = 1 >*/
#line 3418 "cl2.f"
    i__ = 1;
/*<    20    GO TO NEXT,(30, 50, 70, 110) >*/
#line 3419 "cl2.f"
L20:
#line 3419 "cl2.f"
    switch (next) {
#line 3419 "cl2.f"
	case 0: goto L30;
#line 3419 "cl2.f"
	case 1: goto L50;
#line 3419 "cl2.f"
	case 2: goto L70;
#line 3419 "cl2.f"
	case 3: goto L110;
#line 3419 "cl2.f"
    }
/*<    30 IF( ABS(SX(I)) .GT. CUTLO) GO TO 85 >*/
#line 3420 "cl2.f"
L30:
#line 3420 "cl2.f"
    if ((r__1 = sx[i__], abs(r__1)) > cutlo) {
#line 3420 "cl2.f"
	goto L85;
#line 3420 "cl2.f"
    }
/*<       ASSIGN 50 TO NEXT >*/
#line 3421 "cl2.f"
    next = 1;
#line 3421 "cl2.f"
    next_fmt = fmt_50;
/*<       XMAX = ZERO >*/
#line 3422 "cl2.f"
    xmax = zero;

/*                        PHASE 1.  SUM IS ZERO */

/*<    50 IF( SX(I) .EQ. ZERO) GO TO 200 >*/
#line 3426 "cl2.f"
L50:
#line 3426 "cl2.f"
    if (sx[i__] == zero) {
#line 3426 "cl2.f"
	goto L200;
#line 3426 "cl2.f"
    }
/*<       IF( ABS(SX(I)) .GT. CUTLO) GO TO 85 >*/
#line 3427 "cl2.f"
    if ((r__1 = sx[i__], abs(r__1)) > cutlo) {
#line 3427 "cl2.f"
	goto L85;
#line 3427 "cl2.f"
    }

/*                                PREPARE FOR PHASE 2. */
/*<       ASSIGN 70 TO NEXT >*/
#line 3430 "cl2.f"
    next = 2;
#line 3430 "cl2.f"
    next_fmt = fmt_70;
/*<       GO TO 105 >*/
#line 3431 "cl2.f"
    goto L105;

/*                                PREPARE FOR PHASE 4. */

/*<   100 I = J >*/
#line 3435 "cl2.f"
L100:
#line 3435 "cl2.f"
    i__ = j;
/*<       ASSIGN 110 TO NEXT >*/
#line 3436 "cl2.f"
    next = 3;
#line 3436 "cl2.f"
    next_fmt = fmt_110;
/*<       SUM = (SUM / SX(I)) / SX(I) >*/
#line 3437 "cl2.f"
    sum = sum / sx[i__] / sx[i__];
/*<   105 XMAX = ABS(SX(I)) >*/
#line 3438 "cl2.f"
L105:
#line 3438 "cl2.f"
    xmax = (r__1 = sx[i__], abs(r__1));
/*<       GO TO 115 >*/
#line 3439 "cl2.f"
    goto L115;

/*                   PHASE 2.  SUM IS SMALL. */
/*                             SCALE TO AVOID DESTRUCTIVE UNDERFLOW. */

/*<    70 IF( ABS(SX(I)) .GT. CUTLO ) GO TO 75 >*/
#line 3444 "cl2.f"
L70:
#line 3444 "cl2.f"
    if ((r__1 = sx[i__], abs(r__1)) > cutlo) {
#line 3444 "cl2.f"
	goto L75;
#line 3444 "cl2.f"
    }

/*                     COMMON CODE FOR PHASES 2 AND 4. */
/*                     IN PHASE 4 SUM IS LARGE.  SCALE TO AVOID OVERFLOW. */

/*<   110 IF( ABS(SX(I)) .LE. XMAX ) GO TO 115 >*/
#line 3449 "cl2.f"
L110:
#line 3449 "cl2.f"
    if ((r__1 = sx[i__], abs(r__1)) <= xmax) {
#line 3449 "cl2.f"
	goto L115;
#line 3449 "cl2.f"
    }
/*<          SUM = ONE + SUM * (XMAX / SX(I))**2 >*/
/* Computing 2nd power */
#line 3450 "cl2.f"
    r__1 = xmax / sx[i__];
#line 3450 "cl2.f"
    sum = one + sum * (r__1 * r__1);
/*<          XMAX = ABS(SX(I)) >*/
#line 3451 "cl2.f"
    xmax = (r__1 = sx[i__], abs(r__1));
/*<          GO TO 200 >*/
#line 3452 "cl2.f"
    goto L200;

/*<   115 SUM = SUM + (SX(I)/XMAX)**2 >*/
#line 3454 "cl2.f"
L115:
/* Computing 2nd power */
#line 3454 "cl2.f"
    r__1 = sx[i__] / xmax;
#line 3454 "cl2.f"
    sum += r__1 * r__1;
/*<       GO TO 200 >*/
#line 3455 "cl2.f"
    goto L200;


/*                  PREPARE FOR PHASE 3. */

/*<    75 SUM = (SUM * XMAX) * XMAX >*/
#line 3460 "cl2.f"
L75:
#line 3460 "cl2.f"
    sum = sum * xmax * xmax;


/*     FOR REAL OR D.P. SET HITEST = CUTHI/N */
/*     FOR COMPLEX      SET HITEST = CUTHI/(2*N) */

/*<    85 HITEST = CUTHI/FLOAT( N ) >*/
#line 3466 "cl2.f"
L85:
#line 3466 "cl2.f"
    hitest = cuthi / (real) (*n);

/*                   PHASE 3.  SUM IS MID-RANGE.  NO SCALING. */

/*<       DO 95 J =I,NN,INCX >*/
#line 3470 "cl2.f"
    i__1 = nn;
#line 3470 "cl2.f"
    i__2 = *incx;
#line 3470 "cl2.f"
    for (j = i__; i__2 < 0 ? j >= i__1 : j <= i__1; j += i__2) {
/*<       IF(ABS(SX(J)) .GE. HITEST) GO TO 100 >*/
#line 3471 "cl2.f"
	if ((r__1 = sx[j], abs(r__1)) >= hitest) {
#line 3471 "cl2.f"
	    goto L100;
#line 3471 "cl2.f"
	}
/*<    95    SUM = SUM + SX(J)**2 >*/
#line 3472 "cl2.f"
/* L95: */
/* Computing 2nd power */
#line 3472 "cl2.f"
	r__1 = sx[j];
#line 3472 "cl2.f"
	sum += r__1 * r__1;
#line 3472 "cl2.f"
    }
/*<       SNRM2 = SQRT( SUM ) >*/
#line 3473 "cl2.f"
    ret_val = sqrt(sum);
/*<       GO TO 300 >*/
#line 3474 "cl2.f"
    goto L300;

/*<   200 CONTINUE >*/
#line 3476 "cl2.f"
L200:
/*<       I = I + INCX >*/
#line 3477 "cl2.f"
    i__ += *incx;
/*<       IF ( I .LE. NN ) GO TO 20 >*/
#line 3478 "cl2.f"
    if (i__ <= nn) {
#line 3478 "cl2.f"
	goto L20;
#line 3478 "cl2.f"
    }

/*              END OF MAIN LOOP. */

/*              COMPUTE SQUARE ROOT AND ADJUST FOR SCALING. */

/*<       SNRM2 = XMAX * SQRT(SUM) >*/
#line 3484 "cl2.f"
    ret_val = xmax * sqrt(sum);
/*<   300 CONTINUE >*/
#line 3485 "cl2.f"
L300:
/*<       RETURN >*/
#line 3486 "cl2.f"
    return ret_val;
/*<       END >*/
} /* snrm2_ */

/* CC */
/* CC---------------------------------------------------------------------------- */
/* CC */
/*<       REAL FUNCTION SASUM(N,SX,INCX)                                     >*/
static real sasum_(integer *n, real *sx, integer *incx)
{
    /* System generated locals */
    integer i__1, i__2;
    real ret_val, r__1, r__2, r__3, r__4, r__5, r__6;

    /* Local variables */
    static integer i__, m, ns, mp1;


/*     RETURNS SUM OF MAGNITUDES OF SINGLE PRECISION SX. */

/* END OF ABSTRACT */
/*<       REAL SX(1) >*/

/*<       SASUM = 0.0E0 >*/
#line 3498 "cl2.f"
    /* Parameter adjustments */
#line 3498 "cl2.f"
    --sx;
#line 3498 "cl2.f"

#line 3498 "cl2.f"
    /* Function Body */
#line 3498 "cl2.f"
    ret_val = 0.f;
/*<       IF(N.LE.0)RETURN >*/
#line 3499 "cl2.f"
    if (*n <= 0) {
#line 3499 "cl2.f"
	return ret_val;
#line 3499 "cl2.f"
    }
/*<       IF(INCX.EQ.1)GOTO 20 >*/
#line 3500 "cl2.f"
    if (*incx == 1) {
#line 3500 "cl2.f"
	goto L20;
#line 3500 "cl2.f"
    }

/*        CODE FOR INCREMENTS NOT EQUAL TO 1. */

/*<       NS = N*INCX >*/
#line 3504 "cl2.f"
    ns = *n * *incx;
/*<           DO 10 I=1,NS,INCX >*/
#line 3505 "cl2.f"
    i__1 = ns;
#line 3505 "cl2.f"
    i__2 = *incx;
#line 3505 "cl2.f"
    for (i__ = 1; i__2 < 0 ? i__ >= i__1 : i__ <= i__1; i__ += i__2) {
/*<           SASUM = SASUM + ABS(SX(I)) >*/
#line 3506 "cl2.f"
	ret_val += (r__1 = sx[i__], abs(r__1));
/*<    10     CONTINUE >*/
#line 3507 "cl2.f"
/* L10: */
#line 3507 "cl2.f"
    }
/*<       RETURN >*/
#line 3508 "cl2.f"
    return ret_val;

/*        CODE FOR INCREMENTS EQUAL TO 1. */


/*        CLEAN-UP LOOP SO REMAINING VECTOR LENGTH IS A MULTIPLE OF 6. */

/*<    20 M = N - (N/6)*6 >*/
#line 3515 "cl2.f"
L20:
#line 3515 "cl2.f"
    m = *n - *n / 6 * 6;
/*<       IF( M .EQ. 0 ) GO TO 40 >*/
#line 3516 "cl2.f"
    if (m == 0) {
#line 3516 "cl2.f"
	goto L40;
#line 3516 "cl2.f"
    }
/*<       DO 30 I = 1,M >*/
#line 3517 "cl2.f"
    i__2 = m;
#line 3517 "cl2.f"
    for (i__ = 1; i__ <= i__2; ++i__) {
/*<         SASUM = SASUM + ABS(SX(I)) >*/
#line 3518 "cl2.f"
	ret_val += (r__1 = sx[i__], abs(r__1));
/*<    30 CONTINUE >*/
#line 3519 "cl2.f"
/* L30: */
#line 3519 "cl2.f"
    }
/*<       IF( N .LT. 6 ) RETURN >*/
#line 3520 "cl2.f"
    if (*n < 6) {
#line 3520 "cl2.f"
	return ret_val;
#line 3520 "cl2.f"
    }
/*<    40 MP1 = M + 1 >*/
#line 3521 "cl2.f"
L40:
#line 3521 "cl2.f"
    mp1 = m + 1;
/*<       DO 50 I = MP1,N,6 >*/
#line 3522 "cl2.f"
    i__2 = *n;
#line 3522 "cl2.f"
    for (i__ = mp1; i__ <= i__2; i__ += 6) {
/*<    >*/
#line 3523 "cl2.f"
	ret_val = ret_val + (r__1 = sx[i__], abs(r__1)) + (r__2 = sx[i__ + 1],
		 abs(r__2)) + (r__3 = sx[i__ + 2], abs(r__3)) + (r__4 = sx[
		i__ + 3], abs(r__4)) + (r__5 = sx[i__ + 4], abs(r__5)) + (
		r__6 = sx[i__ + 5], abs(r__6));
/*<    50 CONTINUE >*/
#line 3525 "cl2.f"
/* L50: */
#line 3525 "cl2.f"
    }
/*<       RETURN >*/
#line 3526 "cl2.f"
    return ret_val;
/*<       END >*/
} /* sasum_ */

/* CC */
/* CC---------------------------------------------------------------------------- */
/* CC */
/*<       SUBROUTINE  SSCAL(N,SA,SX,INCX)                                    >*/
static /* Subroutine */ int sscal_(integer *n, real *sa, real *sx, integer *incx)
{
    /* System generated locals */
    integer i__1, i__2;

    /* Local variables */
    static integer i__, m, ns, mp1;


/*     REPLACE SINGLE PRECISION SX BY SINGLE PRECISION SA*SX. */

/* END OF ABSTRACT */
/*<       REAL SA,SX(1) >*/

/*<       IF(N.LE.0)RETURN >*/
#line 3538 "cl2.f"
    /* Parameter adjustments */
#line 3538 "cl2.f"
    --sx;
#line 3538 "cl2.f"

#line 3538 "cl2.f"
    /* Function Body */
#line 3538 "cl2.f"
    if (*n <= 0) {
#line 3538 "cl2.f"
	return 0;
#line 3538 "cl2.f"
    }
/*<       IF(INCX.EQ.1)GOTO 20 >*/
#line 3539 "cl2.f"
    if (*incx == 1) {
#line 3539 "cl2.f"
	goto L20;
#line 3539 "cl2.f"
    }

/*        CODE FOR INCREMENTS NOT EQUAL TO 1. */

/*<       NS = N*INCX >*/
#line 3543 "cl2.f"
    ns = *n * *incx;
/*<           DO 10 I = 1,NS,INCX >*/
#line 3544 "cl2.f"
    i__1 = ns;
#line 3544 "cl2.f"
    i__2 = *incx;
#line 3544 "cl2.f"
    for (i__ = 1; i__2 < 0 ? i__ >= i__1 : i__ <= i__1; i__ += i__2) {
/*<           SX(I) = SA*SX(I) >*/
#line 3545 "cl2.f"
	sx[i__] = *sa * sx[i__];
/*<    10     CONTINUE >*/
#line 3546 "cl2.f"
/* L10: */
#line 3546 "cl2.f"
    }
/*<       RETURN >*/
#line 3547 "cl2.f"
    return 0;

/*        CODE FOR INCREMENTS EQUAL TO 1. */


/*        CLEAN-UP LOOP SO REMAINING VECTOR LENGTH IS A MULTIPLE OF 5. */

/*<    20 M = N - (N/5)*5 >*/
#line 3554 "cl2.f"
L20:
#line 3554 "cl2.f"
    m = *n - *n / 5 * 5;
/*<       IF( M .EQ. 0 ) GO TO 40 >*/
#line 3555 "cl2.f"
    if (m == 0) {
#line 3555 "cl2.f"
	goto L40;
#line 3555 "cl2.f"
    }
/*<       DO 30 I = 1,M >*/
#line 3556 "cl2.f"
    i__2 = m;
#line 3556 "cl2.f"
    for (i__ = 1; i__ <= i__2; ++i__) {
/*<         SX(I) = SA*SX(I) >*/
#line 3557 "cl2.f"
	sx[i__] = *sa * sx[i__];
/*<    30 CONTINUE >*/
#line 3558 "cl2.f"
/* L30: */
#line 3558 "cl2.f"
    }
/*<       IF( N .LT. 5 ) RETURN >*/
#line 3559 "cl2.f"
    if (*n < 5) {
#line 3559 "cl2.f"
	return 0;
#line 3559 "cl2.f"
    }
/*<    40 MP1 = M + 1 >*/
#line 3560 "cl2.f"
L40:
#line 3560 "cl2.f"
    mp1 = m + 1;
/*<       DO 50 I = MP1,N,5 >*/
#line 3561 "cl2.f"
    i__2 = *n;
#line 3561 "cl2.f"
    for (i__ = mp1; i__ <= i__2; i__ += 5) {
/*<         SX(I) = SA*SX(I) >*/
#line 3562 "cl2.f"
	sx[i__] = *sa * sx[i__];
/*<         SX(I + 1) = SA*SX(I + 1) >*/
#line 3563 "cl2.f"
	sx[i__ + 1] = *sa * sx[i__ + 1];
/*<         SX(I + 2) = SA*SX(I + 2) >*/
#line 3564 "cl2.f"
	sx[i__ + 2] = *sa * sx[i__ + 2];
/*<         SX(I + 3) = SA*SX(I + 3) >*/
#line 3565 "cl2.f"
	sx[i__ + 3] = *sa * sx[i__ + 3];
/*<         SX(I + 4) = SA*SX(I + 4) >*/
#line 3566 "cl2.f"
	sx[i__ + 4] = *sa * sx[i__ + 4];
/*<    50 CONTINUE >*/
#line 3567 "cl2.f"
/* L50: */
#line 3567 "cl2.f"
    }
/*<       RETURN >*/
#line 3568 "cl2.f"
    return 0;
/*<       END >*/
} /* sscal_ */

/* CC */
/* CC---------------------------------------------------------------------------- */
/* CC */
/*<       INTEGER FUNCTION ISAMAX(N,SX,INCX)                                 >*/
static integer isamax_(integer *n, real *sx, integer *incx)
{
    /* System generated locals */
    integer ret_val, i__1, i__2;
    real r__1;

    /* Local variables */
    static integer i__, ii, ns;
    static real xmag, smax;


/*     FIND SMALLEST INDEX OF MAXIMUM MAGNITUDE OF SINGLE PRECISION SX. */

/* END OF ABSTRACT */
/*<       REAL SX(1),SMAX,XMAG >*/

/*<       ISAMAX = 0 >*/
#line 3580 "cl2.f"
    /* Parameter adjustments */
#line 3580 "cl2.f"
    --sx;
#line 3580 "cl2.f"

#line 3580 "cl2.f"
    /* Function Body */
#line 3580 "cl2.f"
    ret_val = 0;
/*<       IF(N.LE.0) RETURN >*/
#line 3581 "cl2.f"
    if (*n <= 0) {
#line 3581 "cl2.f"
	return ret_val;
#line 3581 "cl2.f"
    }
/*<       ISAMAX = 1 >*/
#line 3582 "cl2.f"
    ret_val = 1;
/*<       IF(N.LE.1)RETURN >*/
#line 3583 "cl2.f"
    if (*n <= 1) {
#line 3583 "cl2.f"
	return ret_val;
#line 3583 "cl2.f"
    }
/*<       IF(INCX.EQ.1)GOTO 20 >*/
#line 3584 "cl2.f"
    if (*incx == 1) {
#line 3584 "cl2.f"
	goto L20;
#line 3584 "cl2.f"
    }

/*        CODE FOR INCREMENTS NOT EQUAL TO 1. */

/*<       SMAX = ABS(SX(1)) >*/
#line 3588 "cl2.f"
    smax = abs(sx[1]);
/*<       NS = N*INCX >*/
#line 3589 "cl2.f"
    ns = *n * *incx;
/*<       II = 1 >*/
#line 3590 "cl2.f"
    ii = 1;
/*<           DO 10 I=1,NS,INCX >*/
#line 3591 "cl2.f"
    i__1 = ns;
#line 3591 "cl2.f"
    i__2 = *incx;
#line 3591 "cl2.f"
    for (i__ = 1; i__2 < 0 ? i__ >= i__1 : i__ <= i__1; i__ += i__2) {
/*<           XMAG = ABS(SX(I)) >*/
#line 3592 "cl2.f"
	xmag = (r__1 = sx[i__], abs(r__1));
/*<           IF(XMAG.LE.SMAX) GO TO 5 >*/
#line 3593 "cl2.f"
	if (xmag <= smax) {
#line 3593 "cl2.f"
	    goto L5;
#line 3593 "cl2.f"
	}
/*<           ISAMAX = II >*/
#line 3594 "cl2.f"
	ret_val = ii;
/*<           SMAX = XMAG >*/
#line 3595 "cl2.f"
	smax = xmag;
/*<     5     II = II + 1 >*/
#line 3596 "cl2.f"
L5:
#line 3596 "cl2.f"
	++ii;
/*<    10     CONTINUE >*/
#line 3597 "cl2.f"
/* L10: */
#line 3597 "cl2.f"
    }
/*<       RETURN >*/
#line 3598 "cl2.f"
    return ret_val;

/*        CODE FOR INCREMENTS EQUAL TO 1. */

/*<    20 SMAX = ABS(SX(1)) >*/
#line 3602 "cl2.f"
L20:
#line 3602 "cl2.f"
    smax = abs(sx[1]);
/*<       DO 30 I = 2,N >*/
#line 3603 "cl2.f"
    i__2 = *n;
#line 3603 "cl2.f"
    for (i__ = 2; i__ <= i__2; ++i__) {
/*<          XMAG = ABS(SX(I)) >*/
#line 3604 "cl2.f"
	xmag = (r__1 = sx[i__], abs(r__1));
/*<          IF(XMAG.LE.SMAX) GO TO 30 >*/
#line 3605 "cl2.f"
	if (xmag <= smax) {
#line 3605 "cl2.f"
	    goto L30;
#line 3605 "cl2.f"
	}
/*<          ISAMAX = I >*/
#line 3606 "cl2.f"
	ret_val = i__;
/*<          SMAX = XMAG >*/
#line 3607 "cl2.f"
	smax = xmag;
/*<    30 CONTINUE >*/
#line 3608 "cl2.f"
L30:
#line 3608 "cl2.f"
	;
#line 3608 "cl2.f"
    }
/*<       RETURN >*/
#line 3609 "cl2.f"
    return ret_val;
/*<       END >*/
} /* isamax_ */

/* CC */
/* CC---------------------------------------------------------------------------- */
/* CC */
/*<       REAL FUNCTION SDOT(N,SX,INCX,SY,INCY)                              >*/
static real sdot_(integer *n, real *sx, integer *incx, real *sy, integer *incy)
{
    /* System generated locals */
    integer i__1, i__2;
    real ret_val;

    /* Local variables */
    static integer i__, m, ix, iy, ns, mp1;


/*     RETURNS THE DOT PRODUCT OF SINGLE PRECISION SX AND SY. */

/*<       REAL SX(1),SY(1) >*/
/*<       SDOT = 0.0E0 >*/
#line 3619 "cl2.f"
    /* Parameter adjustments */
#line 3619 "cl2.f"
    --sy;
#line 3619 "cl2.f"
    --sx;
#line 3619 "cl2.f"

#line 3619 "cl2.f"
    /* Function Body */
#line 3619 "cl2.f"
    ret_val = 0.f;
/*<       IF(N.LE.0)RETURN >*/
#line 3620 "cl2.f"
    if (*n <= 0) {
#line 3620 "cl2.f"
	return ret_val;
#line 3620 "cl2.f"
    }
/*<       IF(INCX.EQ.INCY) IF(INCX-1)5,20,60 >*/
#line 3621 "cl2.f"
    if (*incx == *incy) {
#line 3621 "cl2.f"
	if ((i__1 = *incx - 1) < 0) {
#line 3621 "cl2.f"
	    goto L5;
#line 3621 "cl2.f"
	} else if (i__1 == 0) {
#line 3621 "cl2.f"
	    goto L20;
#line 3621 "cl2.f"
	} else {
#line 3621 "cl2.f"
	    goto L60;
#line 3621 "cl2.f"
	}
#line 3621 "cl2.f"
    }
/*<     5 CONTINUE >*/
#line 3622 "cl2.f"
L5:

/*        CODE FOR UNEQUAL INCREMENTS OR NONPOSITIVE INCREMENTS. */

/*<       IX = 1 >*/
#line 3626 "cl2.f"
    ix = 1;
/*<       IY = 1 >*/
#line 3627 "cl2.f"
    iy = 1;
/*<       IF(INCX.LT.0)IX = (-N+1)*INCX + 1 >*/
#line 3628 "cl2.f"
    if (*incx < 0) {
#line 3628 "cl2.f"
	ix = (-(*n) + 1) * *incx + 1;
#line 3628 "cl2.f"
    }
/*<       IF(INCY.LT.0)IY = (-N+1)*INCY + 1 >*/
#line 3629 "cl2.f"
    if (*incy < 0) {
#line 3629 "cl2.f"
	iy = (-(*n) + 1) * *incy + 1;
#line 3629 "cl2.f"
    }
/*<       DO 10 I = 1,N >*/
#line 3630 "cl2.f"
    i__1 = *n;
#line 3630 "cl2.f"
    for (i__ = 1; i__ <= i__1; ++i__) {
/*<         SDOT = SDOT + SX(IX)*SY(IY) >*/
#line 3631 "cl2.f"
	ret_val += sx[ix] * sy[iy];
/*<         IX = IX + INCX >*/
#line 3632 "cl2.f"
	ix += *incx;
/*<         IY = IY + INCY >*/
#line 3633 "cl2.f"
	iy += *incy;
/*<    10 CONTINUE >*/
#line 3634 "cl2.f"
/* L10: */
#line 3634 "cl2.f"
    }
/*<       RETURN >*/
#line 3635 "cl2.f"
    return ret_val;

/*        CODE FOR BOTH INCREMENTS EQUAL TO 1 */


/*        CLEAN-UP LOOP SO REMAINING VECTOR LENGTH IS A MULTIPLE OF 5. */

/*<    20 M = N - (N/5)*5 >*/
#line 3642 "cl2.f"
L20:
#line 3642 "cl2.f"
    m = *n - *n / 5 * 5;
/*<       IF( M .EQ. 0 ) GO TO 40 >*/
#line 3643 "cl2.f"
    if (m == 0) {
#line 3643 "cl2.f"
	goto L40;
#line 3643 "cl2.f"
    }
/*<       DO 30 I = 1,M >*/
#line 3644 "cl2.f"
    i__1 = m;
#line 3644 "cl2.f"
    for (i__ = 1; i__ <= i__1; ++i__) {
/*<         SDOT = SDOT + SX(I)*SY(I) >*/
#line 3645 "cl2.f"
	ret_val += sx[i__] * sy[i__];
/*<    30 CONTINUE >*/
#line 3646 "cl2.f"
/* L30: */
#line 3646 "cl2.f"
    }
/*<       IF( N .LT. 5 ) RETURN >*/
#line 3647 "cl2.f"
    if (*n < 5) {
#line 3647 "cl2.f"
	return ret_val;
#line 3647 "cl2.f"
    }
/*<    40 MP1 = M + 1 >*/
#line 3648 "cl2.f"
L40:
#line 3648 "cl2.f"
    mp1 = m + 1;
/*<       DO 50 I = MP1,N,5 >*/
#line 3649 "cl2.f"
    i__1 = *n;
#line 3649 "cl2.f"
    for (i__ = mp1; i__ <= i__1; i__ += 5) {
/*<    >*/
#line 3650 "cl2.f"
	ret_val = ret_val + sx[i__] * sy[i__] + sx[i__ + 1] * sy[i__ + 1] +
		sx[i__ + 2] * sy[i__ + 2] + sx[i__ + 3] * sy[i__ + 3] + sx[
		i__ + 4] * sy[i__ + 4];
/*<    50 CONTINUE >*/
#line 3652 "cl2.f"
/* L50: */
#line 3652 "cl2.f"
    }
/*<       RETURN >*/
#line 3653 "cl2.f"
    return ret_val;

/*        CODE FOR POSITIVE EQUAL INCREMENTS .NE.1. */

/*<    60 CONTINUE >*/
#line 3657 "cl2.f"
L60:
/*<       NS=N*INCX >*/
#line 3658 "cl2.f"
    ns = *n * *incx;
/*<       DO 70 I=1,NS,INCX >*/
#line 3659 "cl2.f"
    i__1 = ns;
#line 3659 "cl2.f"
    i__2 = *incx;
#line 3659 "cl2.f"
    for (i__ = 1; i__2 < 0 ? i__ >= i__1 : i__ <= i__1; i__ += i__2) {
/*<         SDOT = SDOT + SX(I)*SY(I) >*/
#line 3660 "cl2.f"
	ret_val += sx[i__] * sy[i__];
/*<    70   CONTINUE >*/
#line 3661 "cl2.f"
/* L70: */
#line 3661 "cl2.f"
    }
/*<       RETURN >*/
#line 3662 "cl2.f"
    return ret_val;
/*<       END >*/
} /* sdot_ */

/* CC */
/* CC---------------------------------------------------------------------------- */
/* CC */
/*<       SUBROUTINE SAXPY(N,SA,SX,INCX,SY,INCY)                             >*/
static /* Subroutine */ int saxpy_(integer *n, real *sa, real *sx, integer *incx,
	real *sy, integer *incy)
{
    /* System generated locals */
    integer i__1, i__2;

    /* Local variables */
    static integer i__, m, ix, iy, ns, mp1;


/*     OVERWRITE SINGLE PRECISION SY WITH SINGLE PRECISION SA*SX +SY. */

/*<       REAL SX(1),SY(1),SA >*/
/*<       IF(N.LE.0.OR.SA.EQ.0.E0) RETURN >*/
#line 3672 "cl2.f"
    /* Parameter adjustments */
#line 3672 "cl2.f"
    --sy;
#line 3672 "cl2.f"
    --sx;
#line 3672 "cl2.f"

#line 3672 "cl2.f"
    /* Function Body */
#line 3672 "cl2.f"
    if (*n <= 0 || *sa == 0.f) {
#line 3672 "cl2.f"
	return 0;
#line 3672 "cl2.f"
    }
/*<       IF(INCX.EQ.INCY) IF(INCX-1) 5,20,60 >*/
#line 3673 "cl2.f"
    if (*incx == *incy) {
#line 3673 "cl2.f"
	if ((i__1 = *incx - 1) < 0) {
#line 3673 "cl2.f"
	    goto L5;
#line 3673 "cl2.f"
	} else if (i__1 == 0) {
#line 3673 "cl2.f"
	    goto L20;
#line 3673 "cl2.f"
	} else {
#line 3673 "cl2.f"
	    goto L60;
#line 3673 "cl2.f"
	}
#line 3673 "cl2.f"
    }
/*<     5 CONTINUE >*/
#line 3674 "cl2.f"
L5:

/*        CODE FOR NONEQUAL OR NONPOSITIVE INCREMENTS. */

/*<       IX = 1 >*/
#line 3678 "cl2.f"
    ix = 1;
/*<       IY = 1 >*/
#line 3679 "cl2.f"
    iy = 1;
/*<       IF(INCX.LT.0)IX = (-N+1)*INCX + 1 >*/
#line 3680 "cl2.f"
    if (*incx < 0) {
#line 3680 "cl2.f"
	ix = (-(*n) + 1) * *incx + 1;
#line 3680 "cl2.f"
    }
/*<       IF(INCY.LT.0)IY = (-N+1)*INCY + 1 >*/
#line 3681 "cl2.f"
    if (*incy < 0) {
#line 3681 "cl2.f"
	iy = (-(*n) + 1) * *incy + 1;
#line 3681 "cl2.f"
    }
/*<       DO 10 I = 1,N >*/
#line 3682 "cl2.f"
    i__1 = *n;
#line 3682 "cl2.f"
    for (i__ = 1; i__ <= i__1; ++i__) {
/*<         SY(IY) = SY(IY) + SA*SX(IX) >*/
#line 3683 "cl2.f"
	sy[iy] += *sa * sx[ix];
/*<         IX = IX + INCX >*/
#line 3684 "cl2.f"
	ix += *incx;
/*<         IY = IY + INCY >*/
#line 3685 "cl2.f"
	iy += *incy;
/*<    10 CONTINUE >*/
#line 3686 "cl2.f"
/* L10: */
#line 3686 "cl2.f"
    }
/*<       RETURN >*/
#line 3687 "cl2.f"
    return 0;

/*        CODE FOR BOTH INCREMENTS EQUAL TO 1 */


/*        CLEAN-UP LOOP SO REMAINING VECTOR LENGTH IS A MULTIPLE OF 4. */

/*<    20 M = N - (N/4)*4 >*/
#line 3694 "cl2.f"
L20:
#line 3694 "cl2.f"
    m = *n - (*n / 4 << 2);
/*<       IF( M .EQ. 0 ) GO TO 40 >*/
#line 3695 "cl2.f"
    if (m == 0) {
#line 3695 "cl2.f"
	goto L40;
#line 3695 "cl2.f"
    }
/*<       DO 30 I = 1,M >*/
#line 3696 "cl2.f"
    i__1 = m;
#line 3696 "cl2.f"
    for (i__ = 1; i__ <= i__1; ++i__) {
/*<         SY(I) = SY(I) + SA*SX(I) >*/
#line 3697 "cl2.f"
	sy[i__] += *sa * sx[i__];
/*<    30 CONTINUE >*/
#line 3698 "cl2.f"
/* L30: */
#line 3698 "cl2.f"
    }
/*<       IF( N .LT. 4 ) RETURN >*/
#line 3699 "cl2.f"
    if (*n < 4) {
#line 3699 "cl2.f"
	return 0;
#line 3699 "cl2.f"
    }
/*<    40 MP1 = M + 1 >*/
#line 3700 "cl2.f"
L40:
#line 3700 "cl2.f"
    mp1 = m + 1;
/*<       DO 50 I = MP1,N,4 >*/
#line 3701 "cl2.f"
    i__1 = *n;
#line 3701 "cl2.f"
    for (i__ = mp1; i__ <= i__1; i__ += 4) {
/*<         SY(I) = SY(I) + SA*SX(I) >*/
#line 3702 "cl2.f"
	sy[i__] += *sa * sx[i__];
/*<         SY(I + 1) = SY(I + 1) + SA*SX(I + 1) >*/
#line 3703 "cl2.f"
	sy[i__ + 1] += *sa * sx[i__ + 1];
/*<         SY(I + 2) = SY(I + 2) + SA*SX(I + 2) >*/
#line 3704 "cl2.f"
	sy[i__ + 2] += *sa * sx[i__ + 2];
/*<         SY(I + 3) = SY(I + 3) + SA*SX(I + 3) >*/
#line 3705 "cl2.f"
	sy[i__ + 3] += *sa * sx[i__ + 3];
/*<    50 CONTINUE >*/
#line 3706 "cl2.f"
/* L50: */
#line 3706 "cl2.f"
    }
/*<       RETURN >*/
#line 3707 "cl2.f"
    return 0;

/*        CODE FOR EQUAL, POSITIVE, NONUNIT INCREMENTS. */

/*<    60 CONTINUE >*/
#line 3711 "cl2.f"
L60:
/*<       NS = N*INCX >*/
#line 3712 "cl2.f"
    ns = *n * *incx;
/*<           DO 70 I=1,NS,INCX >*/
#line 3713 "cl2.f"
    i__1 = ns;
#line 3713 "cl2.f"
    i__2 = *incx;
#line 3713 "cl2.f"
    for (i__ = 1; i__2 < 0 ? i__ >= i__1 : i__ <= i__1; i__ += i__2) {
/*<           SY(I) = SA*SX(I) + SY(I) >*/
#line 3714 "cl2.f"
	sy[i__] = *sa * sx[i__] + sy[i__];
/*<    70     CONTINUE >*/
#line 3715 "cl2.f"
/* L70: */
#line 3715 "cl2.f"
    }
/*<       RETURN >*/
#line 3716 "cl2.f"
    return 0;
/*<       END >*/
} /* saxpy_ */

/* CC */
/* CC---------------------------------------------------------------------------- */
/* CC */
/*<       SUBROUTINE SROTM (N,SX,INCX,SY,INCY,SPARAM)                        >*/
static /* Subroutine */ int srotm_(integer *n, real *sx, integer *incx, real *sy,
	integer *incy, real *sparam)
{
    /* Initialized data */

    static real zero = 0.f;
    static real two = 2.f;

    /* System generated locals */
    integer i__1, i__2;

    /* Local variables */
    static integer i__;
    static real w, z__;
    static integer kx, ky;
    static real sh11, sh12, sh21, sh22, sflag;
    static integer nsteps;


/*     APPLY THE MODIFIED GIVENS TRANSFORMATION, H, TO THE 2 BY N MATRIX */

/*     (SX(1)     SX(N)) */
/*     (      ...      ) */
/*     (SY(1)     SY(N)) */

/*     WITH SPARAM(1)=SFLAG, H HAS ONE OF THE FOLLOWING FORMS.. */

/*     SFLAG=-1.E0     SFLAG=0.E0        SFLAG=1.E0     SFLAG=-2.E0 */

/*       (SH11  SH12)    (1.E0  SH12)    (SH11  1.E0)    (1.E0  0.E0) */
/*     H=(          )    (          )    (          )    (          ) */
/*       (SH21  SH22),   (SH21  1.E0),   (-1.E0 SH22),   (0.E0  1.E0). */

/*<       DIMENSION SX(1),SY(1),SPARAM(5) >*/
/*<       DATA ZERO,TWO /0.E0,2.E0/ >*/
#line 3738 "cl2.f"
    /* Parameter adjustments */
#line 3738 "cl2.f"
    --sparam;
#line 3738 "cl2.f"
    --sy;
#line 3738 "cl2.f"
    --sx;
#line 3738 "cl2.f"

#line 3738 "cl2.f"
    /* Function Body */

/*<       SFLAG=SPARAM(1) >*/
#line 3740 "cl2.f"
    sflag = sparam[1];
/*<       IF(N .LE. 0 .OR.(SFLAG+TWO.EQ.ZERO)) GO TO 140 >*/
#line 3741 "cl2.f"
    if (*n <= 0 || sflag + two == zero) {
#line 3741 "cl2.f"
	goto L140;
#line 3741 "cl2.f"
    }
/*<           IF(.NOT.(INCX.EQ.INCY.AND. INCX .GT.0)) GO TO 70 >*/
#line 3742 "cl2.f"
    if (! (*incx == *incy && *incx > 0)) {
#line 3742 "cl2.f"
	goto L70;
#line 3742 "cl2.f"
    }

/*<                NSTEPS=N*INCX >*/
#line 3744 "cl2.f"
    nsteps = *n * *incx;
/*<                IF(SFLAG) 50,10,30 >*/
#line 3745 "cl2.f"
    if (sflag < 0.f) {
#line 3745 "cl2.f"
	goto L50;
#line 3745 "cl2.f"
    } else if (sflag == 0) {
#line 3745 "cl2.f"
	goto L10;
#line 3745 "cl2.f"
    } else {
#line 3745 "cl2.f"
	goto L30;
#line 3745 "cl2.f"
    }
/*<    10          CONTINUE >*/
#line 3746 "cl2.f"
L10:
/*<                SH12=SPARAM(4) >*/
#line 3747 "cl2.f"
    sh12 = sparam[4];
/*<                SH21=SPARAM(3) >*/
#line 3748 "cl2.f"
    sh21 = sparam[3];
/*<                     DO 20 I=1,NSTEPS,INCX >*/
#line 3749 "cl2.f"
    i__1 = nsteps;
#line 3749 "cl2.f"
    i__2 = *incx;
#line 3749 "cl2.f"
    for (i__ = 1; i__2 < 0 ? i__ >= i__1 : i__ <= i__1; i__ += i__2) {
/*<                     W=SX(I) >*/
#line 3750 "cl2.f"
	w = sx[i__];
/*<                     Z=SY(I) >*/
#line 3751 "cl2.f"
	z__ = sy[i__];
/*<                     SX(I)=W+Z*SH12 >*/
#line 3752 "cl2.f"
	sx[i__] = w + z__ * sh12;
/*<                     SY(I)=W*SH21+Z >*/
#line 3753 "cl2.f"
	sy[i__] = w * sh21 + z__;
/*<    20               CONTINUE >*/
#line 3754 "cl2.f"
/* L20: */
#line 3754 "cl2.f"
    }
/*<                GO TO 140 >*/
#line 3755 "cl2.f"
    goto L140;
/*<    30          CONTINUE >*/
#line 3756 "cl2.f"
L30:
/*<                SH11=SPARAM(2) >*/
#line 3757 "cl2.f"
    sh11 = sparam[2];
/*<                SH22=SPARAM(5) >*/
#line 3758 "cl2.f"
    sh22 = sparam[5];
/*<                     DO 40 I=1,NSTEPS,INCX >*/
#line 3759 "cl2.f"
    i__2 = nsteps;
#line 3759 "cl2.f"
    i__1 = *incx;
#line 3759 "cl2.f"
    for (i__ = 1; i__1 < 0 ? i__ >= i__2 : i__ <= i__2; i__ += i__1) {
/*<                     W=SX(I) >*/
#line 3760 "cl2.f"
	w = sx[i__];
/*<                     Z=SY(I) >*/
#line 3761 "cl2.f"
	z__ = sy[i__];
/*<                     SX(I)=W*SH11+Z >*/
#line 3762 "cl2.f"
	sx[i__] = w * sh11 + z__;
/*<                     SY(I)=-W+SH22*Z >*/
#line 3763 "cl2.f"
	sy[i__] = -w + sh22 * z__;
/*<    40               CONTINUE >*/
#line 3764 "cl2.f"
/* L40: */
#line 3764 "cl2.f"
    }
/*<                GO TO 140 >*/
#line 3765 "cl2.f"
    goto L140;
/*<    50          CONTINUE >*/
#line 3766 "cl2.f"
L50:
/*<                SH11=SPARAM(2) >*/
#line 3767 "cl2.f"
    sh11 = sparam[2];
/*<                SH12=SPARAM(4) >*/
#line 3768 "cl2.f"
    sh12 = sparam[4];
/*<                SH21=SPARAM(3) >*/
#line 3769 "cl2.f"
    sh21 = sparam[3];
/*<                SH22=SPARAM(5) >*/
#line 3770 "cl2.f"
    sh22 = sparam[5];
/*<                     DO 60 I=1,NSTEPS,INCX >*/
#line 3771 "cl2.f"
    i__1 = nsteps;
#line 3771 "cl2.f"
    i__2 = *incx;
#line 3771 "cl2.f"
    for (i__ = 1; i__2 < 0 ? i__ >= i__1 : i__ <= i__1; i__ += i__2) {
/*<                     W=SX(I) >*/
#line 3772 "cl2.f"
	w = sx[i__];
/*<                     Z=SY(I) >*/
#line 3773 "cl2.f"
	z__ = sy[i__];
/*<                     SX(I)=W*SH11+Z*SH12 >*/
#line 3774 "cl2.f"
	sx[i__] = w * sh11 + z__ * sh12;
/*<                     SY(I)=W*SH21+Z*SH22 >*/
#line 3775 "cl2.f"
	sy[i__] = w * sh21 + z__ * sh22;
/*<    60               CONTINUE >*/
#line 3776 "cl2.f"
/* L60: */
#line 3776 "cl2.f"
    }
/*<                GO TO 140 >*/
#line 3777 "cl2.f"
    goto L140;
/*<    70     CONTINUE >*/
#line 3778 "cl2.f"
L70:
/*<           KX=1 >*/
#line 3779 "cl2.f"
    kx = 1;
/*<           KY=1 >*/
#line 3780 "cl2.f"
    ky = 1;
/*<           IF(INCX .LT. 0) KX=1+(1-N)*INCX >*/
#line 3781 "cl2.f"
    if (*incx < 0) {
#line 3781 "cl2.f"
	kx = (1 - *n) * *incx + 1;
#line 3781 "cl2.f"
    }
/*<           IF(INCY .LT. 0) KY=1+(1-N)*INCY >*/
#line 3782 "cl2.f"
    if (*incy < 0) {
#line 3782 "cl2.f"
	ky = (1 - *n) * *incy + 1;
#line 3782 "cl2.f"
    }

/*<           IF(SFLAG)120,80,100 >*/
#line 3784 "cl2.f"
    if (sflag < 0.f) {
#line 3784 "cl2.f"
	goto L120;
#line 3784 "cl2.f"
    } else if (sflag == 0) {
#line 3784 "cl2.f"
	goto L80;
#line 3784 "cl2.f"
    } else {
#line 3784 "cl2.f"
	goto L100;
#line 3784 "cl2.f"
    }
/*<    80     CONTINUE >*/
#line 3785 "cl2.f"
L80:
/*<           SH12=SPARAM(4) >*/
#line 3786 "cl2.f"
    sh12 = sparam[4];
/*<           SH21=SPARAM(3) >*/
#line 3787 "cl2.f"
    sh21 = sparam[3];
/*<                DO 90 I=1,N >*/
#line 3788 "cl2.f"
    i__2 = *n;
#line 3788 "cl2.f"
    for (i__ = 1; i__ <= i__2; ++i__) {
/*<                W=SX(KX) >*/
#line 3789 "cl2.f"
	w = sx[kx];
/*<                Z=SY(KY) >*/
#line 3790 "cl2.f"
	z__ = sy[ky];
/*<                SX(KX)=W+Z*SH12 >*/
#line 3791 "cl2.f"
	sx[kx] = w + z__ * sh12;
/*<                SY(KY)=W*SH21+Z >*/
#line 3792 "cl2.f"
	sy[ky] = w * sh21 + z__;
/*<                KX=KX+INCX >*/
#line 3793 "cl2.f"
	kx += *incx;
/*<                KY=KY+INCY >*/
#line 3794 "cl2.f"
	ky += *incy;
/*<    90          CONTINUE >*/
#line 3795 "cl2.f"
/* L90: */
#line 3795 "cl2.f"
    }
/*<           GO TO 140 >*/
#line 3796 "cl2.f"
    goto L140;
/*<   100     CONTINUE >*/
#line 3797 "cl2.f"
L100:
/*<           SH11=SPARAM(2) >*/
#line 3798 "cl2.f"
    sh11 = sparam[2];
/*<           SH22=SPARAM(5) >*/
#line 3799 "cl2.f"
    sh22 = sparam[5];
/*<                DO 110 I=1,N >*/
#line 3800 "cl2.f"
    i__2 = *n;
#line 3800 "cl2.f"
    for (i__ = 1; i__ <= i__2; ++i__) {
/*<                W=SX(KX) >*/
#line 3801 "cl2.f"
	w = sx[kx];
/*<                Z=SY(KY) >*/
#line 3802 "cl2.f"
	z__ = sy[ky];
/*<                SX(KX)=W*SH11+Z >*/
#line 3803 "cl2.f"
	sx[kx] = w * sh11 + z__;
/*<                SY(KY)=-W+SH22*Z >*/
#line 3804 "cl2.f"
	sy[ky] = -w + sh22 * z__;
/*<                KX=KX+INCX >*/
#line 3805 "cl2.f"
	kx += *incx;
/*<                KY=KY+INCY >*/
#line 3806 "cl2.f"
	ky += *incy;
/*<   110          CONTINUE >*/
#line 3807 "cl2.f"
/* L110: */
#line 3807 "cl2.f"
    }
/*<           GO TO 140 >*/
#line 3808 "cl2.f"
    goto L140;
/*<   120     CONTINUE >*/
#line 3809 "cl2.f"
L120:
/*<           SH11=SPARAM(2) >*/
#line 3810 "cl2.f"
    sh11 = sparam[2];
/*<           SH12=SPARAM(4) >*/
#line 3811 "cl2.f"
    sh12 = sparam[4];
/*<           SH21=SPARAM(3) >*/
#line 3812 "cl2.f"
    sh21 = sparam[3];
/*<           SH22=SPARAM(5) >*/
#line 3813 "cl2.f"
    sh22 = sparam[5];
/*<                DO 130 I=1,N >*/
#line 3814 "cl2.f"
    i__2 = *n;
#line 3814 "cl2.f"
    for (i__ = 1; i__ <= i__2; ++i__) {
/*<                W=SX(KX) >*/
#line 3815 "cl2.f"
	w = sx[kx];
/*<                Z=SY(KY) >*/
#line 3816 "cl2.f"
	z__ = sy[ky];
/*<                SX(KX)=W*SH11+Z*SH12 >*/
#line 3817 "cl2.f"
	sx[kx] = w * sh11 + z__ * sh12;
/*<                SY(KY)=W*SH21+Z*SH22 >*/
#line 3818 "cl2.f"
	sy[ky] = w * sh21 + z__ * sh22;
/*<                KX=KX+INCX >*/
#line 3819 "cl2.f"
	kx += *incx;
/*<                KY=KY+INCY >*/
#line 3820 "cl2.f"
	ky += *incy;
/*<   130          CONTINUE >*/
#line 3821 "cl2.f"
/* L130: */
#line 3821 "cl2.f"
    }
/*<   140     CONTINUE >*/
#line 3822 "cl2.f"
L140:
/*<           RETURN >*/
#line 3823 "cl2.f"
    return 0;
/*<           END >*/
} /* srotm_ */
