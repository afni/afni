#include "mrilib.h"

typedef struct {
  int nrc ;        /* # of rows and columns */
  short *len ;     /* in row/column #i, there are len[i] elements */
  float **rc ;     /* so the first column/row index is i+1-len[i] */
} rcmat_float ;

#define ISVALID_RCMAT(rr)                                      \
  ( (rr) != NULL && (rr)->len != NULL && (rr)->len[0] == 1 &&  \
                    (rr)->rc  != NULL && (rr)->rc[0] != NULL  )

/*--------------------------------------------------------------------------*/

rcmat_float * rcmat_float_init( int n )
{
   rcmat_float *rcm ;

   if( n <= 1 ) return NULL ;

   rcm      = (rcmat_float *)alloc( sizeof(rcmat_float) ) ;
   rcm->nrc = n ;
   rcm->len = (short  *)calloc( n , sizeof(short  ) ) ;
   rcm->rc  = (float **)calloc( n , sizeof(float *) ) ;
   return rcm ;
}

/*--------------------------------------------------------------------------*/

rcmat_float * rcmat_float_copy( rcmat_float *rcm )
{
   rcmat_float *qcm ;
   int ii,jj,nn ;

   if( !ISVALID_RCMAT(rcm) ) return NULL ;

   nn  = rcm->nrc ;
   qcm = rcmat_float_init(nn) ;
   memcpy( qcm->len , rcm->len , sizeof(short)*nn ) ;
   for( ii=0 ; ii < nn ; ii++ ){
     qcm->rc[ii] = malloc( sizeof(float)*qcm->len[ii] ) ;
     memcpy( qcm->rc[ii] , rcm->rc[ii] , sizeof(float)*qcm->len[ii] ) ;
   }
   return qcm ;
}

/*--------------------------------------------------------------------------*/

int rcmat_float_choleski( rcmat_float *rcm )
{
   int ii,jj,kk , nn , jbot,kbot ; short *len ;
   float sum , **rc , *rii , *rjj ;

   if( !ISVALID_RCMAT(rcm) ) return -999999999 ;

   nn  = rcm->nrc ;
   rc  = rcm->rc ;
   len = rcm->len ;

   for( ii=0 ; ii < nn ; ii++ ){
     if( len[ii] == 1 ){
       if( rc[ii][0] <= 0.0f ) return -(ii+1) ;
       rc[ii][0] = sqrtf(rc[ii][0]) ; continue ;
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
         if( sum <= 0.0f ) return -(ii+1) ;
         rii[ii] = sqrtf(sum) ;
       }
     }
   }
   return 0 ;
}

/*--------------------------------------------------------------------------*/

void rcmat_float_lowert_solve( rcmat_float *rcm , float *vec )
{
   int nn , ii,jj , jbot ; short *len ;
   float **rc , *rii , sum ;

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

void rcmat_float_uppert_solve( rcmat_float *rcm , float *vec )
{
   int nn , ii,jj , ibot ; short *len ;
   float **rc , *rjj , xj ;

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

rcmat_float * rcmat_float_arma11( int nt, int *itrue, float rho, float lam )
{
   rcmat_float *rcm ;
   short *len ;
   float **rc , *rii ;
   int ii , jj , bmax , jbot , itt,jtt ;

   if( nt < 2 || itrue == NULL || bmax < 1 ) return NULL ;

   rcm = rcmat_float_init( nt ) ;
   len = rcm->len ;
   rc  = rcm->rc ;

        if( rho >  0.9f ) rho =  0.9f ;
   else if( rho < -0.9f ) rho = -0.9f ;

   if( lam > 0.0f ){
     if( rho > 0.0f )
       bmax = 1 + (int)ceilf( logf(0.02/lam) / logf(fabsf(rho)) ) ;
     else
       bmax = 1 ;
   } else {
     bmax = 0 ;
   }

   if( bmax == 0 ){
     for( ii=0 ; ii < nt ; ii++ ){
       len[ii] = 1 ; rc[ii] = malloc(sizeof(float)) ; rc[ii][0] = 1.0f ;
     }
     return rcm ;
   }

   len[0] = 1 ; rc[0] = malloc(sizeof(float)) ; rc[0][0] = 1.0f ;

   for( ii=1 ; ii < nt ; ii++ ){
     itt  = itrue[ii] ;
     jbot = ii-bmax ; if( jbot < 0 ) jbot = 0 ;
     for( jj=jbot ; jj < ii ; jj++ ){
       jtt = itt - itrue[jj] ; if( jtt <= bmax ) break ;
     }
     jbot = jj ;
     if( jbot == ii ){
       len[ii] = 1 ; rc[ii] = malloc(sizeof(float)) ; rc[ii][0] = 1.0f ;
       continue ;
     }
     len[ii] = ii + 1 - jbot ;
     rc[ii] = malloc(sizeof(float)*len[ii]) ;
     rii = rc[ii] - jbot ;
     rii[ii] = 1.0f ; rii[ii-1] = lam ;
     for( jj=ii-2 ; jj >= jbot ; jj-- ) rii[jj] = rii[jj+1]*rho ;
   }

   return rcm ;
}
