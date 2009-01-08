#include "mrilib.h"

/****************************************************************************/
/****** Generic functions to process a sparse matrix in rcmat format. *******/
/****************************************************************************/

/*--------------------------------------------------------------------------*/
/*! Create a rcmat struct, ready to be filled up. */

rcmat * rcmat_init( int n )
{
   rcmat *rcm ;

ENTRY("rcmat_init") ;

   if( n <= 1 ) RETURN(NULL) ;

   rcm      = (rcmat *)malloc( sizeof(rcmat) ) ;
   rcm->nrc = n ;
   rcm->len = (LENTYP * )calloc( n , sizeof(LENTYP  ) ) ;
   rcm->rc  = (double **)calloc( n , sizeof(double *) ) ;
   RETURN(rcm) ;
}

/*--------------------------------------------------------------------------*/
/*! Delete a rcmat structure. */

void rcmat_destroy( rcmat *rcm )
{
   int      nn = rcm->nrc , ii ;
   double **rc = rcm->rc ;
   LENTYP *len = rcm->len ;

   if( rc != NULL ){
     for( ii=0 ; ii < nn ; ii++ ) if( rc[ii] != NULL ) free((void *)rc[ii]) ;
     free((void *)rc) ;
   }
   if( len != NULL ) free((void *)len) ;
   free((void *)rcm) ;
   return ;
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
   memcpy( qcm->len , rcm->len , sizeof(LENTYP)*nn ) ;
   for( ii=0 ; ii < nn ; ii++ ){
     qcm->rc[ii] = malloc( sizeof(double)*qcm->len[ii] ) ;
     memcpy( qcm->rc[ii] , rcm->rc[ii] , sizeof(double)*qcm->len[ii] ) ;
   }
   return qcm ;
}

/*--------------------------------------------------------------------------*/
/*! Consider a rcmat struct as a symmetric matrix, and
    Choleski factor it in place.  Return value is 0 if all is OK.
    A positive return indicates the row/column that had trouble. */

int rcmat_choleski( rcmat *rcm )
{
   int ii,jj,kk , nn , jbot,kbot ; LENTYP *len ;
   double sum , **rc , *rii , *rjj ;

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

void rcmat_lowert_solve( rcmat *rcm , double *vec )
{
   int nn , jbot ; LENTYP *len ; register int ii,jj ;
   double **rc ; register double *rii , sum , *vv ;

   if( !ISVALID_RCMAT(rcm) || vec == NULL ) return ;

   nn  = rcm->nrc ;
   rc  = rcm->rc ;
   len = rcm->len ;
   vv  = vec ;

#undef  RV
#define RV(k) rii[k]*vv[k]

   for( ii=0 ; ii < nn ; ii++ ){
     if( len[ii] == 1 ){
       vv[ii] = vv[ii] / rc[ii][0] ; continue ;
     }
     jbot = ii - len[ii] + 1 ; rii = rc[ii] - jbot ;
#undef  UNROLL  /* unrolling this inner loop doesn't seem to help */
#ifdef  UNROLL
     switch( len[ii] ){
       default:
#endif
         sum = vv[ii] ;
         for( jj=jbot ; jj < ii ; jj++ ) sum -= RV(jj) ;
#ifdef UNROLL
       break ;
       case 2:
         sum = vv[ii]-RV(ii-1) ; break ;
       case 3:
         sum = vv[ii]-RV(ii-2)-RV(ii-1) ; break ;
       case 4:
         sum = vv[ii]-RV(ii-3)-RV(ii-2)-RV(ii-1) ; break ;
       case 5:
         sum = vv[ii]-RV(ii-4)-RV(ii-3)-RV(ii-2)-RV(ii-1) ; break ;
       case 6:
         sum = vv[ii]-RV(ii-5)-RV(ii-4)-RV(ii-3)-RV(ii-2)-RV(ii-1) ; break ;
       case 7:
         sum = vv[ii]-RV(ii-6)-RV(ii-5)-RV(ii-4)-RV(ii-3)-RV(ii-2)-RV(ii-1) ; break ;
       case 8:
         sum = vv[ii]-RV(ii-7)-RV(ii-6)-RV(ii-5)-RV(ii-4)-RV(ii-3)-RV(ii-2)-RV(ii-1) ;
       break ;
       case 9:
         sum = vv[ii]-RV(ii-8)-RV(ii-7)-RV(ii-6)-RV(ii-5)
                     -RV(ii-4)-RV(ii-3)-RV(ii-2)-RV(ii-1) ;
       break ;
       case 10:
         sum = vv[ii]-RV(ii-9)-RV(ii-8)-RV(ii-7)-RV(ii-6)-RV(ii-5)
                     -RV(ii-4)-RV(ii-3)-RV(ii-2)-RV(ii-1) ;
       break ;
       case 11:
         sum = vv[ii]-RV(ii-10)-RV(ii-9)-RV(ii-8)-RV(ii-7)-RV(ii-6)-RV(ii-5)
                     -RV(ii-4)-RV(ii-3)-RV(ii-2)-RV(ii-1) ;
       break ;
       case 12:
         sum = vv[ii]-RV(ii-11)-RV(ii-10)-RV(ii-9)
                     -RV(ii-8)-RV(ii-7)-RV(ii-6)-RV(ii-5)
                     -RV(ii-4)-RV(ii-3)-RV(ii-2)-RV(ii-1) ;
       break ;
     }
#endif
     vv[ii] = sum / rii[ii] ;
   }
   return ;
}

/*--------------------------------------------------------------------------*/
/*! Consider a rcmat struct as an upper triangular matrix,
    and solve the matrix-vector equation [rcm][x] = [vec], in place. */

void rcmat_uppert_solve( rcmat *rcm , double *vec )
{
   int nn , ibot ; LENTYP *len ; register int ii,jj ;
   double **rc ; register double *rjj , *vv , xj ;

   if( !ISVALID_RCMAT(rcm) || vec == NULL ) return ;

   nn  = rcm->nrc ;
   rc  = rcm->rc ;
   len = rcm->len ;
   vv  = vec ;

   for( jj=nn-1 ; jj >= 0 ; jj-- ){
     ibot = jj - len[jj] + 1 ;
     rjj  = rc[jj] - ibot ;
     xj = vv[jj] = vv[jj] / rjj[jj] ;
     for( ii=ibot ; ii < jj ; ii++ ) vv[ii] -= rjj[ii] * xj ;
   }
   return ;
}

/*--------------------------------------------------------------------------*/

#undef  EPS
#define EPS 1.e-12
#undef  DM
#define DM(a,b) ((double)(a))*((double)(b))  /* double multiply */

static int   *rbot = NULL ;
static int   *rtop = NULL ;
static float *rfac = NULL ;

static void free_rbotop(void)
{
  if( rbot != NULL ){ free(rbot) ; rbot = NULL ; }
  if( rtop != NULL ){ free(rtop) ; rtop = NULL ; }
  if( rfac != NULL ){ free(rfac) ; rfac = NULL ; }
}

static void set_rbotop( int npt , int nref , float *ref[] )
{
   int jj , ii , nerr ; float *rj ; register double sum ;

   free_rbotop() ;
   if( npt < 1 || nref < 1 || ref == NULL ) return ;

   /* find first and last nonzero entries in each column;
      N.B.: if column #j is all zero, then rbot[j] > rtop[j] */

   rbot = (int *)malloc(sizeof(int)*nref) ;
   for( jj=0 ; jj < nref ; jj++ ){
     rj = ref[jj] ; if( rj == NULL ){ free_rbotop(); return; }
     for( ii=0 ; ii < npt && rj[ii] == 0.0f ; ii++ ) ; /*nada*/
     rbot[jj] = ii ;
   }

   rtop = (int *)malloc(sizeof(int)*nref) ;
   for( jj=0 ; jj < nref ; jj++ ){
     rj = ref[jj] ;
     for( ii=npt-1; ii >= 0 && rj[ii] == 0.0f ; ii-- ) ; /*nada */
     rtop[jj] = ii ;
   }

   /* compute reciprocal of L2 norm of each column;
      any zero value will abort the calculations and kill everything */

   rfac = (float *)malloc(sizeof(float)*nref) ;
   for( jj=0 ; jj < nref ; jj++ ){
     rj = ref[jj] ;
     for( sum=0.0,ii=rbot[jj] ; ii <= rtop[jj] ; ii++ ) sum += DM(rj[ii],rj[ii]) ;
     if( sum == 0.0 ){ free_rbotop(); return; }
     rfac[jj] = 1.0/sqrt(sum) ;
   }

   return ;
}

/*--------------------------------------------------------------------------*/
/*! Construct sparse normal equations. */

rcmat * rcmat_normeqn( int npt , int nref , float *ref[] )
{
   rcmat  *rcm=NULL ;
   LENTYP *len ;
   double **rc ;
   int jj,kk , kbot,ibot,itop,rjb,rjt,rkb,rkt ; register int ii ;
   register float *rj, *rk ;
   register double sum , *rcjj ;

ENTRY("rcmat_normeqn") ;

   if( npt < 1 || nref < 1 || ref == NULL ) RETURN(NULL) ;

   /* find first and last nonzero entries in each column;
      N.B.: if column #j is all zero, then rbot[j] > rtop[j] */

   set_rbotop(npt,nref,ref) ; if( rbot == NULL ) RETURN(NULL) ;

   /* create sparse matrix */

   rcm = rcmat_init( nref ) ;
   len = rcm->len ;
   rc  = rcm->rc ;

   /* create first row */

   jj = 0 ;
   len[jj] = 1 ; rc[jj] = malloc(sizeof(double)) ;
   rc[jj][0] = 1.0 + EPS ;

   /* create subsequent rows */

   for( jj=1 ; jj < nref ; jj++ ){
     rj = ref[jj] ; rjb = rbot[jj] ; rjt = rtop[jj] ;

     for( kk=0 ; kk < jj ; kk++ ){ /* find 1st ref[kk] that overlaps ref[jj] */
       rkb = rbot[kk] ; rkt = rtop[kk] ;
       if( rkb > rkt || rkb > rjt || rkt < rjb ) continue ;
       break ;
     }

     kbot    = kk ;                           /* earliest index in jj-th row */
     len[jj] = jj+1-kbot ;                 /* number of entries in jj-th row */
     rc[jj]  = (double *)calloc(sizeof(double),len[jj]) ;   /* the jj-th row */
     rcjj    = rc[jj] - kbot ;           /* shifted pointer to the jj-th row */

     for( kk=kbot ; kk < jj ; kk++ ){    /* dot product of ref[kk] & ref[jj] */
       rkb = rbot[kk] ; rkt = rtop[kk] ; if( rkb > rkt ) continue ;  /* zero */
       ibot = MAX(rkb,rjb) ; itop = MIN(rkt,rjt) ; rk = ref[kk] ;
       for( sum=0.0,ii=ibot ; ii <= itop ; ii++ ) sum += DM(rj[ii],rk[ii]) ;
       rcjj[kk] = sum * rfac[jj] * rfac[kk] ;           /* scale by L2 norms */
     }

     rcjj[jj] = 1.0 + EPS ;          /* diagonal element, scaled by L2 norms */
   }

#if 0
fprintf(stderr,"rcmat_normeqn\n") ;
for( ii=0 ; ii < nref ; ii++ ){
  fprintf(stderr,"%2d:",ii) ;
  kk = ii - len[ii] + 1 ;
  for( jj=0 ; jj <= ii ; jj++ )
    fprintf(stderr," %11.4g", (jj < kk) ? 0.0 : rc[ii][jj-kk] ) ;
  fprintf(stderr,"\n") ;
}
for( ii=0 ; ii < nref ; ii++ ){
  fprintf(stderr,"%2d: rbot=%d rtop=%d rfac=%11.4g len=%d\n",
          ii,rbot[ii],rtop[ii],rfac[ii],len[ii]) ;
}
#endif

   RETURN(rcm) ;
}

/*--------------------------------------------------------------------------*/

float * rcmat_lsqfit( int npt  , float *far   ,
                      int nref , float *ref[]  )
{
   rcmat *rcm ; int ii,jj,rib,rit ; float *wt,*ri ; double *dt , sum ;

ENTRY("rcmat_lsqfit") ;

   if( far == NULL ) RETURN(NULL) ;

   free_rbotop() ;

STATUS("normal eqns") ;
   rcm = rcmat_normeqn( npt , nref , ref ) ; if( rcm == NULL ) RETURN(NULL) ;

STATUS("choleski") ;
   ii = rcmat_choleski( rcm ) ;
   if( ii != 0 ){
     ERROR_message("Choleski fails in rcmat_lsqfit at row %d",ii) ;
     free_rbotop() ; rcmat_destroy(rcm) ; RETURN(NULL) ;
   }

STATUS("make dt") ;
   dt = (double *)calloc(sizeof(double),nref) ;
   for( ii=0 ; ii < nref ; ii++ ){
     ri = ref[ii] ; rib = rbot[ii] ; rit = rtop[ii] ;
     for( sum=0.0,jj=rib ; jj <= rit ; jj++ ) sum += DM(far[jj],ri[jj]) ;
     dt[ii] = sum * rfac[ii] ;
   }
STATUS("solve") ;
   rcmat_lowert_solve( rcm , dt ) ;
   rcmat_uppert_solve( rcm , dt ) ;
   rcmat_destroy(rcm) ;

STATUS("copy") ;
   wt = (float *)malloc(sizeof(float)*nref) ;
   for( ii=0 ; ii < nref ; ii++ ) wt[ii] = dt[ii] * rfac[ii] ;

   free_rbotop() ; free(dt) ; RETURN(wt) ;
}
