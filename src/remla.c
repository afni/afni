/******* This file is meant to be #include-d into another application! *******/

#include "mrilib.h"

#ifndef FLOATIZE
# include "matrix.h"
# define MTYPE double
# define MPAIR double_pair
#else
# include "matrix_f.h"
# define MTYPE float
# define MPAIR float_pair
#endif

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

typedef struct {
  int neq , mreg ;
  MTYPE rho , lam ;
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

   for( jj=nn ; jj >= 0 ; jj-- ){
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
      correlation element is about 0.02.
    * tau[i] is the 'true' time index of the i-th data point.  This
      lets you allow for censoring and for inter-run gaps.
*//*------------------------------------------------------------------------*/

rcmat * rcmat_arma11( int nt, int *tau, MTYPE rho, MTYPE lam )
{
   rcmat *rcm ;
   short *len ;
   MTYPE **rc , *rii ;
   int ii , jj , bmax , jbot , itt,jtt ;

   if( nt < 2 || tau == NULL || bmax < 1 ) return NULL ;

   rcm = rcmat_init( nt ) ;  /* create sparse matrix struct */
   len = rcm->len ;
   rc  = rcm->rc ;

        if( rho >  0.9 ) rho =  0.9 ;  /* max allowed NN correlation */
   else if( rho < -0.9 ) rho = -0.9 ;

   /* set maximum bandwidth */

   if( lam > 0.0 ){
     if( rho != 0.0 ) /* so that last element is about 0.02 */
       bmax = 1 + (int)ceil( log(0.02/lam) / log(fabsf(rho)) ) ;
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

   return rcm ;
}

/*--------------------------------------------------------------------------*/
/*! Create the struct for REML calculations for a particular ARMA(1,1)
    set of parameters, and for a particular regression matrix X.       */

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
     ERROR_message("rcmat_choleski fails with code=%d",ii) ;
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
     ERROR_message("matrix_qrr fails") ;
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

   /* and 2 * log det[C] */

   for( csum=0.0,ii=0 ; ii < nt ; ii++ ){
     jj  = rcm->len[ii] ;
     val = rcm->rc[ii][jj+1] ;
     if( val > 0.0 ) csum += log(val) ;
   }
   rset->cc_logdet = 2.0 * csum ;

   return rset ;
}

/*--------------------------------------------------------------------------*/

static vector *b1=NULL,*b2=NULL,*b3=NULL,*b4=NULL,*b5=NULL,*b6=NULL,*b7=NULL ;

/*--------------------------------------------------------------------------*/
/*! Compute the REML -log(likelihood) function for a particular case,
    given the case's setup and the data and the regression matrix X. */

MTYPE reml_func( vector *y , reml_setup *rset , matrix *X )
{
   int n=rset->neq , ii ;
   MTYPE val ;

   if( b1 == NULL ){
     b1 = (vector *)malloc(sizeof(vector)) ; vector_initialize(b1) ;
     b2 = (vector *)malloc(sizeof(vector)) ; vector_initialize(b2) ;
     b3 = (vector *)malloc(sizeof(vector)) ; vector_initialize(b3) ;
     b4 = (vector *)malloc(sizeof(vector)) ; vector_initialize(b4) ;
     b5 = (vector *)malloc(sizeof(vector)) ; vector_initialize(b5) ;
     b6 = (vector *)malloc(sizeof(vector)) ; vector_initialize(b6) ;
     b7 = (vector *)malloc(sizeof(vector)) ; vector_initialize(b7) ;
   }

   /** Seven steps to compute the prewhitened residuals */

   vector_equate( *y , b1 ) ;
   rcmat_lowert_solve( rset->cc , b1->elts ) ;      /* b1 = C^(-T) y */
                                                    /* prewhitened data */
   vector_equate( *b1 , b2 ) ;
   rcmat_uppert_solve( rset->cc , b2->elts ) ;      /* b2 = C^(-1) b1 */

   vector_mutiply_transpose( *X , *b2 , b3 ) ;      /* b3 = X^T b2 */

   vector_rrtran_solve( *(rset->dd) , *b3 , b4 ) ;  /* b4 = D^(-T) b3 */

   vector_rr_solve( *(rset->dd) , *b4 , b5 ) ;      /* b5 = D^(-1) b4 */
                                                    /*    = beta_hat */

   vector_multiply( *X , *b5 , b6 ) ;               /* b6 = X b5 */
                                                    /* fitted model */
   vector_equate( *b6 , b7 ) ;
   rcmat_lowert_solve( rset->cc , b7->elts ) ;      /* b7 = C^(-T) b6 */
                                                    /* prewhitened fit */

   vector_subtract( *b1 , *b7 , b2 ) ;              /* result = b1 - b7 */
                                                    /* prewhitened residual */

   val = (n - rset->mreg) * vector_dotself(*b2)     /* the REML function! */
        + rset->dd_logdet + rset->cc_logdet ;

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

static reml_collection *rcol = NULL ;

void setup_reml_collection( matrix *X , int *tau ,
                            int nrho, MTYPE rhotop, int ndel )
{
   int ii,jj,kk , nt , *ttau ;
   MTYPE drho , ddel ;

   if( X == NULL ) return ;

   nt = X->rows ; if( nt < 9 ) return ;

   if( tau != NULL ){
     ttau = tau ;
   } else {
     ttau = (int *)malloc(sizeof(int)*nt) ;
     for( ii=0 ; ii < nt ; ii++ ) ttau[ii] = ii ;
   }

   if( nrho < 2 )                       nrho   = 7   ; /* defaults */
   if( rhotop <= 0.0 || rhotop >= 1.0 ) rhotop = 0.7 ;
   if( ndel < 2 )                       ndel   = 10  ;

   drho = rhotop / nrho ;
   ddel = 1.0    / ndel ;

   if( rcol != NULL ) reml_collection_destroy( rcol ) ;

   rcol = (reml_collection *)malloc(sizeof(reml_collection)) ;

   rcol->X = (matrix *)malloc(sizeof(matrix)) ; matrix_initialize(rcol->X) ;
   matrix_equate( *X , rcol->X ) ;

   rcol->nset = nrho * ndel + 1 ;
   rcol->rs   = (reml_setup **)malloc(sizeof(reml_setup *)*rcol->nset) ;

   rcol->rs[0] = setup_arma11_reml( nt , ttau , 0.0 , 0.0 , X ) ;

   for( kk=1,ii=0 ; ii < nrho ; ii++ ){
     for( jj=0 ; jj < ndel ; jj++,kk++ ){
       rcol->rs[kk] = setup_arma11_reml( nt,ttau, (ii+1)*drho, (jj+1)*ddel, X );
     }
   }

   if( tau == NULL ) free((void *)ttau) ;
   return ;
}

/*--------------------------------------------------------------------------*/

static int    first_reml_call = 1 ;

static MTYPE  REML_best_rho ;
static MTYPE  REML_best_lam ;
static vector REML_best_prewhitened_data_vector ;
static vector REML_best_beta_vector ;
static vector REML_best_fitts_vector ;
static vector REML_best_prewhitened_fitts_vector ;
static vector REML_best_prewhitened_errts_vector ;

void reml_find_best_case( vector *y )
{
   if( y == NULL || rcol == NULL ) return ;

   if( first_reml_call ){
     vector_initialize( &REML_best_prewhitened_data_vector ) ;
     vector_initialize( &REML_best_beta_vector ) ;
     vector_initialize( &REML_best_fitts_vector ) ;
     vector_initialize( &REML_best_prewhitened_fitts_vector ) ;
     vector_initialize( &REML_best_prewhitened_errts_vector ) ;
     first_reml_call = 0 ;
   }
}
