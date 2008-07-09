#include "mrilib.h"

#ifndef FLOATIZE
# include "matrix.h"
# define MTYPE double
#else
# include "matrix_f.h"
# define MTYPE float
#endif

typedef struct {
  int nrc ;        /* # of rows and columns */
  short *len ;     /* in row/column #i, there are len[i] elements */
  MTYPE **rc ;     /* so the first column/row index is i+1-len[i] */
} rcmat ;

#define ISVALID_RCMAT(rr)                                      \
  ( (rr) != NULL && (rr)->len != NULL && (rr)->len[0] == 1 &&  \
                    (rr)->rc  != NULL && (rr)->rc[0]  != NULL )

typedef struct {
  int neq , mreg ;
  int cor_type , ncor_param ;
  MTYPE *cor_param ;
  rcmat  *cc ;
  matrix *dd , *xx ;
  MTYPE cc_logdet , dd_logdet ;
} reml_setup ;

#define COR_ARMA11 1

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
    Choleski factor it in place.  Return value is 0 if all is OK. */

int rcmat_choleski( rcmat *rcm )
{
   int ii,jj,kk , nn , jbot,kbot ; short *len ;
   MTYPE sum , **rc , *rii , *rjj ;

   if( !ISVALID_RCMAT(rcm) ) return -999999999 ;

   nn  = rcm->nrc ;
   rc  = rcm->rc ;
   len = rcm->len ;

   for( ii=0 ; ii < nn ; ii++ ){
     if( len[ii] == 1 ){
       if( rc[ii][0] <= 0.0 ) return -(ii+1) ;
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
         if( sum <= 0.0 ) return -(ii+1) ;
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
/*! Setup correlation matrix
      [ 1 lam lam*rho lam*rho^2 lam*rho^3 ... ]
    which is the ARMA(1,1) model with the AR parameter a = rho,
    and the MA parameter b such that (b+a)*(1+a*b)/(1+2*a*b+b*b) = lam.
    For reasonable models of FMRI noise, 0 < lam < rho < 1.
*//*------------------------------------------------------------------------*/

rcmat * rcmat_arma11( int nt, int *itrue, MTYPE rho, MTYPE lam )
{
   rcmat *rcm ;
   short *len ;
   MTYPE **rc , *rii ;
   int ii , jj , bmax , jbot , itt,jtt ;

   if( nt < 2 || itrue == NULL || bmax < 1 ) return NULL ;

   rcm = rcmat_init( nt ) ;
   len = rcm->len ;
   rc  = rcm->rc ;

        if( rho >  0.9f ) rho =  0.9f ;
   else if( rho < -0.9f ) rho = -0.9f ;

   if( lam > 0.0 ){
     if( rho > 0.0 )
       bmax = 1 + (int)ceil( log(0.02/lam) / log(fabsf(rho)) ) ;
     else
       bmax = 1 ;
   } else {
     bmax = 0 ;
   }

   if( bmax == 0 ){
     for( ii=0 ; ii < nt ; ii++ ){
       len[ii] = 1 ; rc[ii] = malloc(sizeof(MTYPE)) ; rc[ii][0] = 1.0 ;
     }
     return rcm ;
   }

   len[0] = 1 ; rc[0] = malloc(sizeof(MTYPE)) ; rc[0][0] = 1.0 ;

   for( ii=1 ; ii < nt ; ii++ ){
     itt  = itrue[ii] ;
     jbot = ii-bmax ; if( jbot < 0 ) jbot = 0 ;
     for( jj=jbot ; jj < ii ; jj++ ){
       jtt = itt - itrue[jj] ; if( jtt <= bmax ) break ;
     }
     jbot = jj ;
     if( jbot == ii ){
       len[ii] = 1 ; rc[ii] = malloc(sizeof(MTYPE)) ; rc[ii][0] = 1.0 ;
       continue ;
     }
     len[ii] = ii + 1 - jbot ;
     rc[ii]  = calloc(sizeof(MTYPE),len[ii]) ;
     rii     = rc[ii] - jbot ;
     rii[ii] = 1.0 ;
     for( jj=jbot ; jj < ii ; jj++ ){
       jtt = itt - itrue[jj] ;
            if( jtt == 1 ) rii[jj] = lam ;
       else if( jtt >  1 ) rii[jj] = lam * powf( rho , jtt-1.0 ) ;
     }
   }

   return rcm ;
}

/*--------------------------------------------------------------------------*/

reml_setup * setup_arma11_reml( int nt, int *itrue,
                                MTYPE rho, MTYPE lam , matrix *X )
{
   int ii , jj , mm ;
   reml_setup *rset ;
   rcmat *rcm ;
   matrix *W , *D ;
   MTYPE *vec , csum,dsum,val ;

   if( nt < 2 || itrue == NULL || X == NULL || X->rows != nt ) return NULL ;

   mm = X->cols ;

   /* Form R = correlation matrix for ARMA(1,1) model */

   rcm = rcmat_arma11( nt , itrue , rho , lam ) ;
   if( rcm == NULL ) return NULL ;

   /* form C = Choleski factor of R (in place) */

   ii = rcmat_choleski( rcm ) ;
   if( ii != 0 ){
     ERROR_message("rcmat_choleski fails with code=%d",ii) ;
     rcmat_destroy(rcm); return NULL;
   }

   /* prewhiten each column of X into W */

   matrix_initialize(W) ;
   matrix_create(nt,mm,W) ;
   vec = (MTYPE *)malloc(sizeof(MTYPE)*nt) ;
   for( jj=0 ; jj < X->cols ; jj++ ){
     for( ii=0 ; ii < nt ; ii++ ) vec[ii] = X->elts[ii][jj] ;
     rcmat_lowert_solve( rcm , vec ) ;
     for( ii=0 ; ii < nt ; ii++ ) W->elts[ii][jj] = vec[ii] ;
   }
   free((void *)vec) ;

   /* compute QR decomposition of W, save R factor into D */

   matrix_initialize(D) ;
   matrix_qrr( *W , D ) ;
   matrix_destroy(W) ;
   if( D->rows == 0 ){
     ERROR_message("matrix_qrr fails") ;
     matrix_destroy(D) ; rcmat_destroy(rcm) ; return NULL ;
   }

   /* create the setup struct */

   rset = (reml_setup *)malloc(sizeof(reml_setup)) ;
   rset->neq          = nt ;
   rset->mreg         = mm ;
   rset->cor_type     = COR_ARMA11 ;
   rset->ncor_param   = 2 ;
   rset->cor_param    = (MTYPE *)malloc(sizeof(MTYPE)*2) ;
   rset->cor_param[0] = rho ;
   rset->cor_param[1] = lam ;
   rset->cc           = rcm ;
   rset->dd           = D ;

   for( dsum=0.0,ii=0 ; ii < mm ; ii++ ){
     val = D->elts[ii][ii] ;
     if( val > 0.0 ) dsum += log(val) ;
   }
   rset->dd_logdet = 2.0 * dsum ;

   for( csum=0.0,ii=0 ; ii < nt ; ii++ ){
     jj  = rcm->len[ii] ;
     val = rcm->rc[ii][jj+1] ;
     if( val > 0.0 ) csum += log(val) ;
   }
   rset->cc_logdet = 2.0 * csum ;

   return rset ;
}

MTYPE reml_func( vector *y , reml_setup *rset )
{
   int n=rset->neq , ii ;
   MTYPE val ;

   vector *b1,*b2,*b3,*b4,*b5,*b6,*b7 ;

   vector_initialize(b1) ; vector_initialize(b2) ;
   vector_initialize(b3) ; vector_initialize(b4) ;
   vector_initialize(b5) ; vector_initialize(b6) ;
   vector_initialize(b7) ;

   vector_equate( *y , b1 ) ;
   rcmat_lowert_solve( rset->cc , b1->elts ) ;

   vector_equate( *b1 , b2 ) ;
   rcmat_uppert_solve( rset->cc , b2->elts ) ;

   vector_mutiply_transpose( rset->xx , *b2 , b3 ) ;

   vector_rrtran_solve( *(rset->dd) , *b3 , b4 ) ;

   vector_rr_solve( *(rset->dd) , *b4 , b5 ) ;

   vector_multiply( *(rset->xx) , *b5 , b6 ) ;

   vector_equate( *b6 , b7 ) ;
   rcmat_lowert_solve( rset->cc , b7->elts ) ;

   vector_subtract( *b1 , *b7 , b2 ) ;

   val = (n - rset->mreg) * vector_dotself(*b2)
        + rset->dd_logdet + rset->cc_logdet ;

   vector_destroy(b7) ; vector_destroy(b6) ;
   vector_destroy(b5) ; vector_destroy(b4) ;
   vector_destroy(b3) ; vector_destroy(b2) ;
   vector_destroy(b1) ;

   return val ;
}
