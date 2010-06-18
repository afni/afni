#include "mrilib.h"
#include "zgaussian.c"
#include <time.h>
#include <sys/types.h>
#include <unistd.h>

#undef  MTYPE
#define MTYPE double

static MTYPE corcut = 0.000001 ;

#undef  TAU
#define TAU(i) ((tau==NULL) ? (i) : tau[i])

/*--------------------------------------------------------------------------*/
/*! Setup sparse banded correlation matrix (as an rcmat struct):
      [ 1 lam lam*rho lam*rho^2 lam*rho^3 ... ]
    which is the ARMA(1,1) model with the AR parameter a = rho,
    and the MA parameter b such that (b+a)*(1+a*b)/(1+2*a*b+b*b) = lam.
    * For reasonable models of FMRI noise, 0 < lam < rho < 0.9.
    * The maximum bandwidth of the matrix is chosen so that the last
      correlation element is about 0.01.
    * tau[i] is the 'true' time index of the i-th data point.  This
      lets you allow for censoring and for inter-run gaps.
    * If tau==NULL, tau[i] is taken to be i -- that is, no censoring/gaps.
*//*------------------------------------------------------------------------*/

static rcmat * rcmat_arma11( int nt, int *tau, MTYPE rho, MTYPE lam )
{
   rcmat  *rcm ;
   LENTYP *len ;
   MTYPE **rc , *rii , alam ;
   int ii , jj , bmax , jbot , itt,jtt ;

   if( nt < 2 ) return NULL ;

   rcm = rcmat_init( nt ) ;  /* create sparse matrix struct */
   len = rcm->len ;
   rc  = rcm->rc ;

        if( rho >  0.99 ) rho =  0.99 ;  /* max allowed NN correlation */
   else if( rho < -0.99 ) rho = -0.99 ;

   /* set maximum bandwidth */

   alam = fabs(lam) ;
   if( alam >= corcut ){
     if( rho != 0.0 ) /* bmax is such that last element is about 'corcut' */
       bmax = 1 + (int)ceil( log(corcut/alam) / log(fabs(rho)) ) ;
     else
       bmax = 1 ;     /* pure MA(1) case */
   } else {
     bmax = 0 ;       /* identity matrix case */
   }

   /* special and trivial case: identity matrix */

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
     itt  = TAU(ii) ;                            /* 'time' of the i'th index */
     jbot = ii-bmax ; if( jbot < 0 ) jbot = 0 ;      /* earliest allow index */
     for( jj=jbot ; jj < ii ; jj++ ){               /* scan to find bandwith */
       jtt = itt - TAU(jj) ;                     /* 'time' difference i-to-j */
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
       jtt = itt - TAU(jj) ;                      /* 'time' difference again */
            if( jtt == 1 ) rii[jj] = lam ;               /* lag==1 means lam */
       else if( jtt >  1 ) rii[jj] = lam * pow( rho , jtt-1.0 ) ;
     }
   }

   return rcm ;
}

/*---------------------------------------------------------------------------*/
/* Generate ARMA11(a=ap,lam=lm) vectors of length nlen, nvec of them, with
   standard deviation set to sg.  If sg <= 0, then the vectors are
   L2-normalized (sum of squares=1).
*//*-------------------------------------------------------------------------*/

MRI_IMAGE * mri_genARMA11( int nlen, int nvec, float ap, float lm, float sg )
{
   int kk,ii ;
   double aa=ap, lam=lm , sig=sg ; int do_norm = (sg<=0.0f) ;
   double *rvec ;
   rcmat *rcm ;
   MRI_IMAGE *outim ;
   float     *outar , *vv ;
#if 0
   long seed=0 ;
   seed = (long)time(NULL)+(long)getpid() ;
   srand48(seed) ;
#endif

ENTRY("mri_genARMA11") ;

   if( nlen      <= 3    ){ ERROR_message("ARMA11 nlen < 4");    RETURN(NULL); }
   if( fabs(aa)  >= 0.98 ){ ERROR_message("ARMA11 a too big");   RETURN(NULL); }
   if( fabs(lam) >= 0.97 ){ ERROR_message("ARMA11 lam too big"); RETURN(NULL); }

   /* setup */

   rcm = rcmat_arma11( nlen , NULL , aa , lam ) ;
   if( rcm == NULL ){
     ERROR_message("Can't setup ARMA11 matrix?!"); RETURN(NULL);
   }

   kk = rcmat_choleski( rcm ) ;
   if( kk > 0 ){
     ERROR_message("ARMA11 Choleski fails at row %d",kk); RETURN(NULL);
   }

   /* simulate */

   outim = mri_new( nlen , nvec , MRI_float ) ; outar = MRI_FLOAT_PTR(outim) ;
   rvec  = (double *)malloc(sizeof(double)*nlen) ;

   for( kk=0 ; kk < nvec ; kk++ ){
     for( ii=0 ; ii < nlen ; ii++ ) rvec[ii] = zgaussian() ;
     rcmat_lowert_vecmul( rcm , rvec ) ;
     vv = outar + kk*nlen ;
     if( do_norm ){
       sig = 0.0 ;
       for( ii=0 ; ii < nlen ; ii++ ) sig += rvec[ii]*rvec[ii] ;
       sig = 1.0 / sqrt(sig) ;
     }
     if( sig != 1.0 ){ for( ii=0 ; ii < nlen ; ii++ ) vv[ii] = sig * rvec[ii]; }
   }

   rcmat_destroy(rcm) ; free(rvec) ;

   RETURN(outim) ;
}
