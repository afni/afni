/******* This file is meant to be #include-d into another application! *******/

#include "mrilib.h"

#undef  MTYPE
#undef  MPAIR
#ifndef FLOATIZE
# include "matrix.h"
# define MTYPE double
# define MPAIR double_pair
#else
# include "matrix_f.h"
# define MTYPE float
# define MPAIR float_pair
#endif

#undef DEBUG

/*****
  Struct to hold a sparse square matrix that is either symmetric,
  or upper or lower triangular.
*****/

typedef struct {
  int nrc ;        /* # of rows and columns */
  short *len ;     /* in row/column #i, there are len[i] elements */
  MTYPE **rc ;     /* so the first column/row index is i+1-len[i] */
} rcmat ;

#define ISVALID_RCMAT(rr)                                      \
  ( (rr) != NULL && (rr)->len != NULL && (rr)->len[0] == 1 &&  \
                    (rr)->rc  != NULL && (rr)->rc[0]  != NULL )

/*****
  Struct to hold the information needed to compute
  the REML log-likelihood for a given ARMA(1,1) case.
*****/

#undef  LAMBDA
#define LAMBDA(a,b) ((b+a)*(1.0+a*b)/(1.0+2.0*a*b+b*b))

typedef struct {
  int neq , mreg ;
  MTYPE rho , lam , barm ;
  rcmat  *cc ;        /* banded matrix */
  matrix *dd ;        /* upper triangular */
  MTYPE cc_logdet , dd_logdet ;
} reml_setup ;

/*****
  Struct to hold the information needed for all
  ARMA(1,1) cases, so that the best one can be found.
*****/

typedef struct {
  int nset ;
  matrix *X ;
  reml_setup **rs ;
} reml_collection ;

/****************************************************************************/
/****** Generic functions to process a sparse matrix in rcmat format. *******/
/****************************************************************************/

/*--------------------------------------------------------------------------*/
/*! Create a rcmat struct, ready to be filled up. */

rcmat * rcmat_init( int n )
{
   rcmat *rcm ;

   if( n <= 1 ) return NULL ;

   rcm      = (rcmat *)malloc( sizeof(rcmat) ) ;
   rcm->nrc = n ;
   rcm->len = (short  *)calloc( n , sizeof(short  ) ) ;
   rcm->rc  = (MTYPE **)calloc( n , sizeof(MTYPE *) ) ;
   return rcm ;
}

/*--------------------------------------------------------------------------*/
/*! Duplicate a rcmat struct. */

rcmat * rcmat_copy( rcmat *rcm )
{
   rcmat *qcm ;
   int ii,nn ;

   if( !ISVALID_RCMAT(rcm) ) return NULL ;

   nn  = rcm->nrc ;
   qcm = rcmat_init(nn) ;
   memcpy( qcm->len , rcm->len , sizeof(short)*nn ) ;
   for( ii=0 ; ii < nn ; ii++ ){
     qcm->rc[ii] = malloc( sizeof(MTYPE)*qcm->len[ii] ) ;
     memcpy( qcm->rc[ii] , rcm->rc[ii] , sizeof(MTYPE)*qcm->len[ii] ) ;
   }
   return qcm ;
}

/*--------------------------------------------------------------------------*/
/*! Delete a rcmat structure. */

void rcmat_destroy( rcmat *rcm )
{
   int      nn = rcm->nrc , ii ;
   MTYPE ** rc = rcm->rc ;
   short  *len = rcm->len ;

   if( rc != NULL ){
     for( ii=0 ; ii < nn ; ii++ ) if( rc[ii] != NULL ) free((void *)rc[ii]) ;
     free((void *)rc) ;
   }
   if( len != NULL ) free((void *)len) ;
   free((void *)rcm) ;
   return ;
}

/*--------------------------------------------------------------------------*/
/*! Consider a rcmat struct as a symmetric matrix, and
    Choleski factor it in place.  Return value is 0 if all is OK.
    A positive return indicates the row/column that had trouble. */

int rcmat_choleski( rcmat *rcm )
{
   int ii,jj,kk , nn , jbot,kbot ; short *len ;
   MTYPE sum , **rc , *rii , *rjj ;

   if( !ISVALID_RCMAT(rcm) ) return 999999999 ;

   nn  = rcm->nrc ;
   rc  = rcm->rc ;
   len = rcm->len ;

   for( ii=0 ; ii < nn ; ii++ ){
     if( len[ii] == 1 ){
       if( rc[ii][0] <= 0.0 ) return (ii+1) ;
       rc[ii][0] = sqrt(rc[ii][0]) ; continue ;
     }
     jbot = ii - len[ii] + 1 ;
     rii  = rc[ii] - jbot ;
     for( jj=jbot ; jj <= ii ; jj++ ){
       if( len[jj] == 1 ){
         rii[jj] = rii[jj] / rc[jj][0] ;
         continue ;
       }
       kbot = jj - len[jj] + 1 ;
       rjj  = rc[jj] - kbot ;
       sum  = rii[jj] ;
       if( kbot < jbot ) kbot = jbot ;
       for( kk=kbot ; kk < jj ; kk++ ) sum -= rii[kk]*rjj[kk] ;
       if( jj < ii ){
         rii[jj] = sum / rjj[jj] ;
       } else {
         if( sum <= 0.0 ) return (ii+1) ;
         rii[ii] = sqrt(sum) ;
       }
     }
   }
   return 0 ;
}

/*--------------------------------------------------------------------------*/
/*! Consider a rcmat struct as a lower triangular matrix,
    and solve the matrix-vector equation [rcm][x] = [vec], in place. */

void rcmat_lowert_solve( rcmat *rcm , MTYPE *vec )
{
   int nn , ii,jj , jbot ; short *len ;
   MTYPE **rc , *rii , sum ;

   if( !ISVALID_RCMAT(rcm) || vec == NULL ) return ;

   nn  = rcm->nrc ;
   rc  = rcm->rc ;
   len = rcm->len ;

   for( ii=0 ; ii < nn ; ii++ ){
     if( len[ii] == 1 ){
       vec[ii] = vec[ii] / rc[ii][0] ; continue ;
     }
     jbot = ii - len[ii] + 1 ;
     rii  = rc[ii] - jbot ;
     sum  = vec[ii] ;
     for( jj=jbot ; jj < ii ; jj++ ) sum -= rii[jj]*vec[jj] ;
     vec[ii] = sum / rii[ii] ;
   }
   return ;
}

/*--------------------------------------------------------------------------*/
/*! Consider a rcmat struct as an upper triangular matrix,
    and solve the matrix-vector equation [rcm][x] = [vec], in place. */

void rcmat_uppert_solve( rcmat *rcm , MTYPE *vec )
{
   int nn , ii,jj , ibot ; short *len ;
   MTYPE **rc , *rjj , xj ;

   if( !ISVALID_RCMAT(rcm) || vec == NULL ) return ;

   nn  = rcm->nrc ;
   rc  = rcm->rc ;
   len = rcm->len ;

   for( jj=nn-1 ; jj >= 0 ; jj-- ){
     ibot = jj - len[jj] + 1 ;
     rjj  = rc[jj] - ibot ;
     xj = vec[jj] = vec[jj] / rjj[jj] ;
     for( ii=ibot ; ii < jj ; ii++ ) vec[ii] -= rjj[ii] * xj ;
   }
   return ;
}

/*--------------------------------------------------------------------------*/
/*! Setup sparse correlation matrix
      [ 1 lam lam*rho lam*rho^2 lam*rho^3 ... ]
    which is the ARMA(1,1) model with the AR parameter a = rho,
    and the MA parameter b such that (b+a)*(1+a*b)/(1+2*a*b+b*b) = lam.
    * For reasonable models of FMRI noise, 0 < lam < rho < 0.9.
    * The maximum bandwidth of the matrix is chosen so that the last
      correlation element is about 0.01.
    * tau[i] is the 'true' time index of the i-th data point.  This
      lets you allow for censoring and for inter-run gaps.
*//*------------------------------------------------------------------------*/

rcmat * rcmat_arma11( int nt, int *tau, MTYPE rho, MTYPE lam )
{
   rcmat *rcm ;
   short *len ;
   MTYPE **rc , *rii , alam ;
   int ii , jj , bmax , jbot , itt,jtt ;

   if( nt < 2 || tau == NULL || bmax < 1 ) return NULL ;

   rcm = rcmat_init( nt ) ;  /* create sparse matrix struct */
   len = rcm->len ;
   rc  = rcm->rc ;

        if( rho >  0.9 ) rho =  0.9 ;  /* max allowed NN correlation */
   else if( rho < -0.9 ) rho = -0.9 ;

   /* set maximum bandwidth */

   alam = fabs(lam) ;
   if( alam > .01 ){
     if( rho != 0.0 ) /* so that last element is about 0.01 */
       bmax = 1 + (int)ceil( log(0.01/alam) / log(fabs(rho)) ) ;
     else
       bmax = 1 ;     /* pure MA(1) case */
   } else {
     bmax = 0 ;       /* identity matrix case */
   }

   /* special case: identity matrix */

   if( bmax == 0 ){
     for( ii=0 ; ii < nt ; ii++ ){
       len[ii] = 1 ; rc[ii] = malloc(sizeof(MTYPE)) ; rc[ii][0] = 1.0 ;
     }
     return rcm ;
   }

   /* First row/column has only 1 entry = diagonal value = 1 */

   len[0] = 1 ; rc[0] = malloc(sizeof(MTYPE)) ; rc[0][0] = 1.0 ;

   /* Subsequent rows/columns: */

   for( ii=1 ; ii < nt ; ii++ ){
     itt  = tau[ii] ;                            /* 'time' of the i'th index */
     jbot = ii-bmax ; if( jbot < 0 ) jbot = 0 ;      /* earliest allow index */
     for( jj=jbot ; jj < ii ; jj++ ){               /* scan to find bandwith */
       jtt = itt - tau[jj] ;                     /* 'time' difference i-to-j */
       if( jtt <= bmax ) break ;                /* if in OK region, stop now */
     }
     jbot = jj ;      /* this is the earliest index to be correlated with #i */
     if( jbot == ii ){       /* a purely diagonal row/colum (inter-run gap?) */
       len[ii] = 1 ; rc[ii] = malloc(sizeof(MTYPE)) ; rc[ii][0] = 1.0 ;
       continue ;
     }
     len[ii] = ii + 1 - jbot ;            /* number of entries in row/column */
     rc[ii]  = calloc(sizeof(MTYPE),len[ii]) ;      /* space for the entries */
     rii     = rc[ii] - jbot ;         /* shifted pointer to this row/column */
     rii[ii] = 1.0 ;                                       /* diagonal entry */
     for( jj=jbot ; jj < ii ; jj++ ){        /* compute off diagonal entries */
       jtt = itt - tau[jj] ;                      /* 'time' difference again */
            if( jtt == 1 ) rii[jj] = lam ;               /* lag==1 means lam */
       else if( jtt >  1 ) rii[jj] = lam * pow( rho , jtt-1.0 ) ;
     }
   }

#if 0
{ if( rho > 0.6 && lam/rho > 0.8 ){
    INFO_message("profile for rho=%g lam=%g",rho,lam) ;
    for( ii=0 ; ii < nt ; ii++ ) fprintf(stderr," %d",len[ii]) ;
    fprintf(stderr,"\n") ;
  }
}
#endif

   return rcm ;
}

/*--------------------------------------------------------------------------*/
/*! Create the struct for REML calculations for a particular ARMA(1,1)
    set of parameters, and for a particular regression matrix X.       */

static MTYPE logdet_Dzero = 0.0 ;
static MTYPE fixed_cost   = 0.0 ;

reml_setup * setup_arma11_reml( int nt, int *tau,
                                MTYPE rho, MTYPE lam , matrix *X )
{
   int ii , jj , mm ;
   reml_setup *rset ;
   rcmat *rcm ;
   matrix *W , *D ;
   MTYPE *vec , csum,dsum,val ;

   if( nt < 2 || tau == NULL || X == NULL || X->rows != nt ) return NULL ;

   mm = X->cols ;  /* number of regression parameters */
   if( mm >= nt || mm <= 0 ) return NULL ;

   /* Form R = correlation matrix for ARMA(1,1) model */

   rcm = rcmat_arma11( nt , tau , rho , lam ) ;
   if( rcm == NULL ) return NULL ;  /* should not transpire */

   /* form C = Choleski factor of R (in place) */

   ii = rcmat_choleski( rcm ) ;
   if( ii != 0 ){
#ifdef DEBUG
     ERROR_message("rcmat_choleski fails with code=%d: rho=%f lam=%f",ii,rho,lam) ;
#endif
     rcmat_destroy(rcm); return NULL;
   }

   /* prewhiten each column of X into W matrix */

   W = (matrix *)malloc(sizeof(matrix)) ; matrix_initialize(W) ;
   matrix_create(nt,mm,W) ;
   vec = (MTYPE *)malloc(sizeof(MTYPE)*nt) ;
   for( jj=0 ; jj < X->cols ; jj++ ){
     for( ii=0 ; ii < nt ; ii++ ) vec[ii] = X->elts[ii][jj] ; /* extract col */
     rcmat_lowert_solve( rcm , vec ) ;                          /* prewhiten */
     for( ii=0 ; ii < nt ; ii++ ) W->elts[ii][jj] = vec[ii] ;  /* put into W */
   }
   free((void *)vec) ;

   /* compute QR decomposition of W, save R factor into D, toss W */

   D = (matrix *)malloc(sizeof(matrix)) ; matrix_initialize(D) ;
   matrix_qrr( *W , D ) ;
   matrix_destroy(W) ; free((void *)W) ;
   if( D->rows <= 0 ){
#ifdef DEBUG
     ERROR_message("matrix_qrr fails") ;
#endif
     matrix_destroy(D) ; free((void *)D) ; rcmat_destroy(rcm) ; return NULL ;
   }

   /* create the setup struct, save stuff into it */

   rset = (reml_setup *)malloc(sizeof(reml_setup)) ;
   rset->neq  = nt ;
   rset->mreg = mm ;
   rset->rho  = rho ;
   rset->lam  = lam ;
   rset->cc   = rcm ;
   rset->dd   = D ;

   /* compute 2 * log det[D] */

   for( dsum=0.0,ii=0 ; ii < mm ; ii++ ){
     val = D->elts[ii][ii] ;
     if( val > 0.0 ) dsum += log(val) ;
   }
   rset->dd_logdet = 2.0 * dsum ;

   if( rho == 0.0 && lam == 0.0 ) logdet_Dzero = dsum ;

   /* and 2 * log det[C] */

   for( csum=0.0,ii=0 ; ii < nt ; ii++ ){
     jj  = rcm->len[ii] ;
     val = rcm->rc[ii][jj-1] ;
     if( val > 0.0 ) csum += log(val) ;
   }
   rset->cc_logdet = 2.0 * csum ;

#if 0
INFO_message("REML setup: rho=%.3f lam=%.3f logdet[D]=%g logdet[C]=%g cost=%g",
             rho,lam,dsum,csum,dsum+csum-logdet_Dzero ) ;
#endif

   fixed_cost = dsum+csum-logdet_Dzero ;

   return rset ;
}

/*--------------------------------------------------------------------------*/

/** intermediate vectors to be saved in case of later need **/

static vector *bb1=NULL,*bb2,*bb3,*bb4,*bb5,*bb6,*bb7 ;
static MTYPE rsumq=0.0 ;

/*--------------------------------------------------------------------------*/
/*! Compute the REML -log(likelihood) function for a particular case,
    given the case's setup and the data and the regression matrix X. */

MTYPE reml_func( vector *y , reml_setup *rset , matrix *X )
{
   int n=rset->neq , ii ;
   MTYPE val ;

   if( y == NULL ){
     if( bb1 != NULL ){
       vector_destroy(bb1) ; bb1 = NULL ;
       vector_destroy(bb2) ; vector_destroy(bb3) ;
       vector_destroy(bb4) ; vector_destroy(bb5) ;
       vector_destroy(bb6) ; vector_destroy(bb7) ;
     }
     return -666.0 ;
   }

   if( bb1 == NULL ){
     bb1 = (vector *)malloc(sizeof(vector)) ; vector_initialize(bb1) ;
     bb2 = (vector *)malloc(sizeof(vector)) ; vector_initialize(bb2) ;
     bb3 = (vector *)malloc(sizeof(vector)) ; vector_initialize(bb3) ;
     bb4 = (vector *)malloc(sizeof(vector)) ; vector_initialize(bb4) ;
     bb5 = (vector *)malloc(sizeof(vector)) ; vector_initialize(bb5) ;
     bb6 = (vector *)malloc(sizeof(vector)) ; vector_initialize(bb6) ;
     bb7 = (vector *)malloc(sizeof(vector)) ; vector_initialize(bb7) ;
   }

   /** Seven steps to compute the prewhitened residuals */

   vector_equate( *y , bb1 ) ;
   rcmat_lowert_solve( rset->cc , bb1->elts ) ;      /* bb1 = C^(-T) y */
                                                     /* prewhitened data */
   vector_equate( *bb1 , bb2 ) ;
   rcmat_uppert_solve( rset->cc , bb2->elts ) ;      /* bb2 = C^(-1) bb1 */

   vector_multiply_transpose( *X , *bb2 , bb3 ) ;    /* bb3 = X^T bb2 */

   vector_rrtran_solve( *(rset->dd) , *bb3 , bb4 ) ; /* bb4 = D^(-T) bb3 */

   vector_rr_solve( *(rset->dd) , *bb4 , bb5 ) ;     /* bb5 = D^(-1) bb4 */
                                                     /*    = beta_hat */

   vector_multiply( *X , *bb5 , bb6 ) ;              /* bb6 = X bb5 */
                                                     /* fitted model */
   vector_equate( *bb6 , bb7 ) ;
   rcmat_lowert_solve( rset->cc , bb7->elts ) ;      /* bb7 = C^(-T) bb6 */
                                                     /* prewhitened fit */
   vector_subtract( *bb1 , *bb7 , bb2 ) ;            /* result = bb1 - bb7 */
                                                     /* prewhitened residual */
   rsumq = vector_dotself(*bb2) ;                    /* sum of squares */

   if( rsumq > 0.0 )
     val = (n - rset->mreg) * log(rsumq)             /* the REML function! */
          + rset->dd_logdet + rset->cc_logdet ;      /* -log(likelihood) */
   else
     val = 0.0 ;

   return val ;
}

/*--------------------------------------------------------------------------*/

void reml_setup_destroy( reml_setup *rset )
{
   if( rset == NULL ) return ;
   if( rset->cc != NULL ) rcmat_destroy( rset->cc ) ;
   if( rset->dd != NULL ) matrix_destroy( rset->dd ) ;
   free((void *)rset) ;
   return ;
}

/*--------------------------------------------------------------------------*/

void reml_collection_destroy( reml_collection *rcol )
{
   int ii ;

   if( rcol == NULL ) return ;
   if( rcol->X != NULL ) matrix_destroy( rcol->X ) ;
   if( rcol->rs != NULL ){
     for( ii=0 ; ii < rcol->nset ; ii++ ) reml_setup_destroy(rcol->rs[ii]) ;
     free((void *)rcol->rs) ;
   }
   free((void *)rcol) ;
   return ;
}

/*--------------------------------------------------------------------------*/

static reml_collection *rrcol = NULL ;

void REML_setup( matrix *X , int *tau , int nrho, MTYPE rhotop,
                                        int nb  , MTYPE btop   )
{
   int ii,jj,kk , nt , *ttau , nset ;
   MTYPE drho , db , bb , rho , lam , bbot ;

   if( X == NULL ) return ;

   nt = X->rows ; if( nt < 9 ) return ;

   if( tau != NULL ){
     ttau = tau ;
   } else {
     ttau = (int *)malloc(sizeof(int)*nt) ;
     for( ii=0 ; ii < nt ; ii++ ) ttau[ii] = ii ;
   }

   if( btop < 0.0 || btop >= 0.99 ) btop = 0.9 ;
   if( btop > 0.0 ){
     if( nb < 2 )         nb = 10 ;
     else if( nb%2 == 1 ) nb++ ;   /* must be even */
   } else {
     nb = 0 ;
   }

   if( rhotop < 0.0 || rhotop >= 0.99 ) rhotop = 0.7 ;
   if( rhotop > 0.0 ){
     if( nrho < 2 ) nrho = 9 ;
   } else {
     nrho = 0 ;
   }

   if( nrho == 0 && nb == 0 ){
     ERROR_message("REML_setup: max values of rho and b are both 0?") ;
     return ;
   }

   drho = rhotop / MAX(1,nrho) ;
   db   = 2.0*btop / MAX(1,nb) ;
   bbot = -btop ;

   if( rrcol != NULL ) reml_collection_destroy( rrcol ) ;

   rrcol = (reml_collection *)malloc(sizeof(reml_collection)) ;

   rrcol->X = (matrix *)malloc(sizeof(matrix)) ; matrix_initialize(rrcol->X) ;
   matrix_equate( *X , rrcol->X ) ;

   nset = (nb+1)*(nrho+1) ;
   rrcol->rs = (reml_setup **)calloc(sizeof(reml_setup *),nset) ;

   rrcol->rs[0] = setup_arma11_reml( nt , ttau , 0.0 , 0.0 , X ) ;

   for( kk=1,ii=0 ; ii <= nrho ; ii++ ){
     rho = ii*drho ;                        /* AR parameter */
     for( jj=0 ; jj <= nb ; jj++ ){
       bb  = jj*db + bbot ;                 /* MA parameter */
       lam = LAMBDA(rho,bb) ;               /* +1 super-diagonal element */
       if( lam >= 0.05 ){
         rrcol->rs[kk] = setup_arma11_reml( nt,ttau, rho,lam , X ) ;
         if( rrcol->rs[kk] != NULL ){
           rrcol->rs[kk]->barm = bb ;
           kk++ ;
#ifdef DEBUG
ININFO_message("setup finished for rho=%g b=%g lam=%g  fixed_cost=%g",
               rho,bb,lam,fixed_cost) ;
#endif
         }
       }
#ifdef DEBUG
else ININFO_message("skipping rho=%g b=%g because lam=%g",rho,bb,lam) ;
#endif
     }
   }

   rrcol->nset = kk ;
#ifdef DEBUG
INFO_message("setup %d REML cases",kk) ;
#endif

   if( tau == NULL ) free((void *)ttau) ;
   return ;
}

/*--------------------------------------------------------------------------*/

static int    REML_status   = -1 ;

static MTYPE  REML_best_rho = 0.0 ;
static MTYPE  REML_best_lam = 0.0 ;
static MTYPE  REML_best_bb  = 0.0 ;
static MTYPE  REML_best_ssq = 0.0 ;
static int    REML_best_ind = 0   ;
static vector REML_best_prewhitened_data_vector ;     /* in bb1 */
static vector REML_best_beta_vector ;                 /* in bb5 */
static vector REML_best_fitts_vector ;                /* in bb6 */
static vector REML_best_prewhitened_fitts_vector ;    /* in bb7 */
static vector REML_best_prewhitened_errts_vector ;    /* in bb2 */

static MTYPE  REML_olsq_rho = 0.0 ;
static MTYPE  REML_olsq_lam = 0.0 ;
static MTYPE  REML_olsq_bb  = 0.0 ;
static MTYPE  REML_olsq_ssq = 0.0 ;
static vector REML_olsq_prewhitened_data_vector ;
static vector REML_olsq_beta_vector ;
static vector REML_olsq_fitts_vector ;
static vector REML_olsq_prewhitened_fitts_vector ;
static vector REML_olsq_prewhitened_errts_vector ;

/*--------------------------------------------------------------------------*/

MTYPE REML_find_best_case( vector *y ) /* input=data vector; output is above */
{
   MTYPE rbest , rval ;
   int   ibest , ii ;

   if( y == NULL ){  /* memory cleanup */
     vector_destroy( &REML_best_prewhitened_data_vector ) ;
     vector_destroy( &REML_best_beta_vector ) ;
     vector_destroy( &REML_best_fitts_vector ) ;
     vector_destroy( &REML_best_prewhitened_fitts_vector ) ;
     vector_destroy( &REML_best_prewhitened_errts_vector ) ;
     reml_func( NULL,NULL,NULL ) ;
     reml_collection_destroy( rrcol ) ;
     REML_status = -1 ; return -666.0 ;
   }

   if( REML_status == -1 ){ /* memory setup */
     vector_initialize( &REML_best_prewhitened_data_vector ) ;
     vector_initialize( &REML_best_beta_vector ) ;
     vector_initialize( &REML_best_fitts_vector ) ;
     vector_initialize( &REML_best_prewhitened_fitts_vector ) ;
     vector_initialize( &REML_best_prewhitened_errts_vector ) ;
   }
   REML_status = 0 ;

   /* do the Ordinary Least Squares (olsq) case */

   rbest = reml_func( y , rrcol->rs[0] , rrcol->X ) ; ibest = 0 ;
   vector_equate( *bb1 , &REML_olsq_prewhitened_data_vector ) ;
   vector_equate( *bb2 , &REML_olsq_prewhitened_errts_vector ) ;
   vector_equate( *bb5 , &REML_olsq_beta_vector ) ;
   vector_equate( *bb6 , &REML_olsq_fitts_vector ) ;
   vector_equate( *bb7 , &REML_olsq_prewhitened_fitts_vector ) ;
   REML_olsq_ssq = rsumq ;

   /* find the best case */

   for( ii=1 ; ii < rrcol->nset ; ii++ ){
     rval = reml_func( y , rrcol->rs[ii] , rrcol->X ) ;
     if( rval < rbest ){
       rbest = rval ; ibest = ii ;
       vector_equate( *bb1 , &REML_best_prewhitened_data_vector ) ;
       vector_equate( *bb2 , &REML_best_prewhitened_errts_vector ) ;
       vector_equate( *bb5 , &REML_best_beta_vector ) ;
       vector_equate( *bb6 , &REML_best_fitts_vector ) ;
       vector_equate( *bb7 , &REML_best_prewhitened_fitts_vector ) ;
       REML_best_ssq = rsumq ;
     }
   }

   if( ibest == 0 ){                 /** OLSQ won! **/
     REML_best_ssq = REML_olsq_ssq ;
     vector_equate( REML_olsq_prewhitened_data_vector,
                   &REML_best_prewhitened_data_vector ) ;
     vector_equate( REML_olsq_prewhitened_errts_vector,
                   &REML_best_prewhitened_errts_vector ) ;
     vector_equate( REML_olsq_beta_vector , &REML_best_beta_vector ) ;
     vector_equate( REML_olsq_fitts_vector, &REML_best_fitts_vector ) ;
     vector_equate( REML_olsq_prewhitened_fitts_vector,
                   &REML_best_prewhitened_fitts_vector ) ;
   }

   REML_best_rho = rrcol->rs[ibest]->rho ;
   REML_best_lam = rrcol->rs[ibest]->lam ;
   REML_best_bb  = rrcol->rs[ibest]->barm;
   REML_best_ind = ibest ;

   REML_status = 1 ; return rbest ;
}

/*--------------------------------------------------------------------------*/

void REML_clear_setup(void){
  (void)REML_find_best_case(NULL) ; return ;
}
