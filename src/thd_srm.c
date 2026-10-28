/*----------------------------------------------------------------------------
  Shared Response Model numerics.  See thd_srm.h for the model, conventions,
  and the contract of every function.  This file holds, so far, the fit
  (deterministic and probabilistic), model memory management, and the
  linear-algebra kernels they stand on.
                                                     -- P Molfese, Oct 2026
------------------------------------------------------------------------------*/

#include <float.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <errno.h>
#include <unistd.h>

#ifdef USE_OMP
#include <omp.h>
#define SRM_PRAGMA(x) _Pragma(#x)
#else
#define SRM_PRAGMA(x)
#endif

#include "thd_srm.h"

#define SRM_EIG_MAXSWEEP     60
#define SRM_EIG_TINY         1.0e-28    /* off-diagonal energy fraction       */
#define SRM_POLAR_RANKTOL    1.0e-12    /* eigenvalue ratio of A'A            */
#define SRM_RHO2_FLOOR       1.0e-8     /* of the view's total variance       */

/*============================================================================
  Small utilities
==============================================================================*/

const char * SRM_strerror( int code )
{
   switch( code ){
     case SRM_OK:           return "ok" ;
     case SRM_ERR_ARG:      return "bad argument" ;
     case SRM_ERR_NFEAT:    return "nfeat must be between 1 and the smallest of "
                                   "every view's feature count and the number "
                                   "of columns" ;
     case SRM_ERR_NONFINITE:return "data contain NaN or Inf" ;
     case SRM_ERR_MEMORY:   return "out of memory" ;
     case SRM_ERR_SINGULAR: return "a view has no variance, or a Procrustes "
                                   "step could not be completed" ;
     case SRM_ERR_MISMATCH: return "model and data disagree on sizes" ;
     case SRM_ERR_FORMAT:   return "not a valid model file (malformed or incomplete)" ;
     case SRM_ERR_IO:       return "the model file could not be read or written" ;
     case SRM_ERR_VERSION:  return "the model file is from a newer version of this program" ;
   }
   return "unknown error" ;
}

static void * srm_calloc( size_t n , size_t sz )
{
   if( n==0 ) n=1 ;
   return calloc(n,sz) ;
}

/*! splitmix64: portable, so a seed gives the same start on every platform. */
typedef struct { unsigned long long s ; int have ; double spare ; } srm_rng ;

static unsigned long long srm_next( srm_rng *r )
{
   unsigned long long z = (r->s += 0x9E3779B97F4A7C15ULL) ;
   z = (z ^ (z >> 30)) * 0xBF58476D1CE4E5B9ULL ;
   z = (z ^ (z >> 27)) * 0x94D049BB133111EBULL ;
   return z ^ (z >> 31) ;
}

static double srm_unif( srm_rng *r )         /* uniform on (0,1) */
{
   return (double)((srm_next(r) >> 11) + 0.5) * (1.0/9007199254740992.0) ;
}

static double srm_normal( srm_rng *r )
{
   double u,v,m ;
   if( r->have ){ r->have=0 ; return r->spare ; }
   u = srm_unif(r) ; v = srm_unif(r) ;
   m = sqrt(-2.0*log(u)) ;
   r->spare = m*sin(6.283185307179586*v) ; r->have = 1 ;
   return m*cos(6.283185307179586*v) ;
}

static void srm_rng_init( srm_rng *r , unsigned long long seed , int stream )
{
   r->s = seed*0x2545F4914F6CDD1DULL + (unsigned long long)(stream+1)*0x9E3779B97F4A7C15ULL ;
   r->have = 0 ; r->spare = 0.0 ;
   (void)srm_next(r) ; (void)srm_next(r) ;
}

/*============================================================================
  Symmetric eigenproblem (cyclic Jacobi).  k is at most a few hundred, the
  matrices are tiny next to the data, and Jacobi is accurate to the last digit
  and has no failure modes worth guarding.
==============================================================================*/

/*! a: n x n symmetric, row-major, destroyed.  ev[n]: eigenvalues, descending.
    q: n x n, column j is the eigenvector of ev[j].  Returns 0, or 1 if the
    matrix has non-finite entries. */
static int srm_eigsym( int n , double *a , double *ev , double *q )
{
   int sweep,p,r,i,j ; double total=0.0 ;

   for( i=0 ; i<n*n ; i++ ){ if( !isfinite(a[i]) ) return 1 ; total += a[i]*a[i] ; }
   for( i=0 ; i<n ; i++ ) for( j=0 ; j<n ; j++ ) q[i*n+j] = (i==j) ? 1.0 : 0.0 ;

   for( sweep=0 ; sweep<SRM_EIG_MAXSWEEP ; sweep++ ){
     double off=0.0 ;
     for( p=0 ; p<n-1 ; p++ ) for( r=p+1 ; r<n ; r++ ) off += a[p*n+r]*a[p*n+r] ;
     if( off <= SRM_EIG_TINY*total || off==0.0 ) break ;

     for( p=0 ; p<n-1 ; p++ ){
       for( r=p+1 ; r<n ; r++ ){
         double apq=a[p*n+r], theta, t, c, s ; int m ;
         if( fabs(apq) < 1.0e-300 ) continue ;
         theta = (a[r*n+r]-a[p*n+p]) / (2.0*apq) ;
         t = (theta>=0.0 ? 1.0 : -1.0) / (fabs(theta)+sqrt(theta*theta+1.0)) ;
         c = 1.0/sqrt(t*t+1.0) ; s = t*c ;
         for( m=0 ; m<n ; m++ ){                     /* A <- A J */
           double x=a[m*n+p], y=a[m*n+r] ;
           a[m*n+p] = c*x - s*y ; a[m*n+r] = s*x + c*y ;
         }
         for( m=0 ; m<n ; m++ ){                     /* A <- J' A */
           double x=a[p*n+m], y=a[r*n+m] ;
           a[p*n+m] = c*x - s*y ; a[r*n+m] = s*x + c*y ;
         }
         for( m=0 ; m<n ; m++ ){                     /* V <- V J */
           double x=q[m*n+p], y=q[m*n+r] ;
           q[m*n+p] = c*x - s*y ; q[m*n+r] = s*x + c*y ;
         }
       }
     }
   }
   for( i=0 ; i<n ; i++ ) ev[i] = a[i*n+i] ;

   for( i=0 ; i<n-1 ; i++ ){                   /* sort descending */
     int best=i ;
     for( j=i+1 ; j<n ; j++ ) if( ev[j] > ev[best] ) best=j ;
     if( best != i ){
       double tmp=ev[i] ; ev[i]=ev[best] ; ev[best]=tmp ;
       for( r=0 ; r<n ; r++ ){
         tmp=q[r*n+i] ; q[r*n+i]=q[r*n+best] ; q[r*n+best]=tmp ;
       }
     }
   }
   return 0 ;
}

/*============================================================================
  Matrix kernels.  X is nvox x ntime, W is nvox x nfeat, S is nfeat x ntime,
  all row-major.  Float data, double accumulation.
==============================================================================*/

/*! P (k x Tr, row stride ldp, double) += weight * W' X, where X is nvox x Tr
    with row stride ldx (a column block of a wider matrix).  X is streamed once,
    row by row. */
static void srm_wtx_acc_ld( const float *W , const float *X , size_t ldx , int V ,
                            int k , int Tr , double weight , double *P , size_t ldp )
{
   int f,c,t ;
   for( f=0 ; f<V ; f++ ){
     const float *xr = X + (size_t)f*ldx ;
     const float *wr = W + (size_t)f*k ;
     for( c=0 ; c<k ; c++ ){
       double w = (double)wr[c]*weight ;
       double *pc = P + (size_t)c*ldp ;
       if( w == 0.0 ) continue ;
       for( t=0 ; t<Tr ; t++ ) pc[t] += w*(double)xr[t] ;
     }
   }
}

static void srm_wtx_acc( const float *W , const float *X , int V , int k , int T ,
                         double weight , double *P )
{
   srm_wtx_acc_ld(W,X,(size_t)T,V,k,T,weight,P,(size_t)T) ;
}

/*! A (V x k, double) = X S', X being V x Tr with row stride ldx and S being
    k x Tr with row stride lds.  Rows are independent. */
static void srm_xst_ld( const float *X , size_t ldx , const double *S , size_t lds ,
                        int V , int k , int Tr , double *A )
{
   int f,c,t ;
   for( f=0 ; f<V ; f++ ){
     const float *xr = X + (size_t)f*ldx ;
     for( c=0 ; c<k ; c++ ){
       const double *sc = S + (size_t)c*lds ;
       double acc=0.0 ;
       for( t=0 ; t<Tr ; t++ ) acc += (double)xr[t]*sc[t] ;
       A[(size_t)f*k+c] = acc ;
     }
   }
}

static void srm_xst( const float *X , const double *S , int V , int k , int T ,
                     double *A )
{
   srm_xst_ld(X,(size_t)T,S,(size_t)T,V,k,T,A) ;
}

/*! The eigen half of a polar factor.  K = A'A (k x k, destroyed).  Fills Q
    (eigenvectors in columns) and ev (descending), counts in *nbad the
    eigenvalues at or below SRM_POLAR_RANKTOL of the largest, and when there
    are none sets M = K^(-1/2), so that polar(A) = A M. */
static int srm_polar_eig( int k , double *K , double *Q , double *ev , double *M ,
                          int *nbad )
{
   int c,d,j ; double lmax ;
   *nbad = 0 ;
   if( srm_eigsym(k,K,ev,Q) ) return SRM_ERR_SINGULAR ;
   lmax = ev[0] ;
   if( !(lmax > 0.0) || !isfinite(lmax) ) return SRM_ERR_SINGULAR ;
   for( j=0 ; j<k ; j++ ) if( ev[j] <= SRM_POLAR_RANKTOL*lmax ) (*nbad)++ ;
   if( *nbad == 0 )
     for( c=0 ; c<k ; c++ ) for( d=0 ; d<k ; d++ ){
       double acc=0.0 ;
       for( j=0 ; j<k ; j++ ) acc += Q[c*k+j]*Q[d*k+j]/sqrt(ev[j]) ;
       M[c*k+d] = acc ;
     }
   return SRM_OK ;
}

/*! The orthonormal factor of A (nrow x k, double): W = A (A'A)^(-1/2), the
    U V' of the thin SVD.  The eigenproblem is the k x k Gram matrix, so the
    cost is nrow*k^2 and nothing larger than A is formed.  If A is rank
    deficient (a direction with no signal), the missing columns of W are filled
    with deterministic random vectors orthogonal to the rest, so W always has
    orthonormal columns.  wk: 4*k*k doubles of scratch.  Returns 0, or
    SRM_ERR_SINGULAR if A is zero or non-finite. */
static int srm_polar( const double *A , int nrow , int k , float *W ,
                      double *wk , unsigned long long seed )
{
   double *G=wk, *Q=wk+k*k, *M=wk+2*k*k, *ev=wk+3*k*k ;   /* ev uses k of k*k */
   int f,c,d,j,nbad=0,rc ; double lmax ;

   memset(G,0,sizeof(double)*(size_t)k*k) ;
   for( f=0 ; f<nrow ; f++ ){
     const double *a = A + (size_t)f*k ;
     for( c=0 ; c<k ; c++ ){
       double ac=a[c] ; double *gr=G+(size_t)c*k ;
       for( d=c ; d<k ; d++ ) gr[d] += ac*a[d] ;
     }
   }
   for( c=0 ; c<k ; c++ ) for( d=0 ; d<c ; d++ ) G[c*k+d] = G[d*k+c] ;

   rc = srm_polar_eig(k,G,Q,ev,M,&nbad) ;
   if( rc != SRM_OK ) return rc ;
   lmax = ev[0] ;

   if( nbad == 0 ){
     /* common case: W = A * (Q diag(1/sqrt(ev)) Q') */
     for( f=0 ; f<nrow ; f++ ){
       const double *a = A + (size_t)f*k ;
       for( c=0 ; c<k ; c++ ){
         double acc=0.0 ;
         for( d=0 ; d<k ; d++ ) acc += a[d]*M[d*k+c] ;
         W[(size_t)f*k+c] = (float)acc ;
       }
     }
     return SRM_OK ;
   } else {
     /* rank-deficient: U = [A q_j / sqrt(ev_j) for good j ; orthogonal fill],
        then W = U Q'.  Rare, so the nrow x k temporary is acceptable. */
     double *U = (double *)srm_calloc((size_t)nrow*k,sizeof(double)) ;
     srm_rng rng ; int pass ;
     if( U == NULL ) return SRM_ERR_MEMORY ;
     srm_rng_init(&rng,seed,7) ;
     for( j=0 ; j<k ; j++ ){
       if( ev[j] > SRM_POLAR_RANKTOL*lmax ){
         double sc = 1.0/sqrt(ev[j]) ;
         for( f=0 ; f<nrow ; f++ ){
           const double *a = A + (size_t)f*k ; double acc=0.0 ;
           for( d=0 ; d<k ; d++ ) acc += a[d]*Q[d*k+j] ;
           U[(size_t)f*k+j] = acc*sc ;
         }
       }
     }
     for( j=0 ; j<k ; j++ ){
       if( ev[j] > SRM_POLAR_RANKTOL*lmax ) continue ;
       for( f=0 ; f<nrow ; f++ ) U[(size_t)f*k+j] = srm_normal(&rng) ;
       for( pass=0 ; pass<2 ; pass++ ){           /* twice is enough (GS) */
         int m ; double nrm=0.0 ;
         for( m=0 ; m<k ; m++ ){
           double dot=0.0 ;
           if( m==j ) continue ;    /* unfilled columns are zero: harmless */
           for( f=0 ; f<nrow ; f++ ) dot += U[(size_t)f*k+m]*U[(size_t)f*k+j] ;
           for( f=0 ; f<nrow ; f++ ) U[(size_t)f*k+j] -= dot*U[(size_t)f*k+m] ;
         }
         for( f=0 ; f<nrow ; f++ ) nrm += U[(size_t)f*k+j]*U[(size_t)f*k+j] ;
         nrm = sqrt(nrm) ;
         if( nrm < 1.0e-12 ){ free(U) ; return SRM_ERR_SINGULAR ; }
         for( f=0 ; f<nrow ; f++ ) U[(size_t)f*k+j] /= nrm ;
       }
     }
     for( f=0 ; f<nrow ; f++ ){
       const double *u = U + (size_t)f*k ;
       for( c=0 ; c<k ; c++ ){
         double acc=0.0 ;
         for( j=0 ; j<k ; j++ ) acc += u[j]*Q[c*k+j] ;
         W[(size_t)f*k+c] = (float)acc ;
       }
     }
     free(U) ;
     return SRM_OK ;
   }
}

/*! Random orthonormal nrow x k (modified Gram-Schmidt of a Gaussian matrix). */
static int srm_random_orthonormal( float *W , int nrow , int k ,
                                   unsigned long long seed , int stream )
{
   srm_rng rng ; double *U ; int f,c,m ;
   /* each column contiguous (U[c*nrow+f]); the draws and every sum are in the same order as ever, so the
      start is bit for bit what it was, only faster */
   U = (double *)srm_calloc((size_t)nrow*k,sizeof(double)) ;
   if( U == NULL ) return SRM_ERR_MEMORY ;
   srm_rng_init(&rng,seed,stream) ;
   for( c=0 ; c<k ; c++ ){
     double nrm=0.0 , *uc=U+(size_t)c*nrow ;
     for( f=0 ; f<nrow ; f++ ) uc[f] = srm_normal(&rng) ;
     for( m=0 ; m<c ; m++ ){
       const double *um=U+(size_t)m*nrow ; double dot=0.0 ;
       for( f=0 ; f<nrow ; f++ ) dot += um[f]*uc[f] ;
       for( f=0 ; f<nrow ; f++ ) uc[f] -= dot*um[f] ;
     }
     for( f=0 ; f<nrow ; f++ ) nrm += uc[f]*uc[f] ;
     nrm = sqrt(nrm) ;
     if( nrm < 1.0e-12 ){ free(U) ; return SRM_ERR_SINGULAR ; }
     for( f=0 ; f<nrow ; f++ ) uc[f] /= nrm ;
   }
   for( f=0 ; f<nrow ; f++ ) for( c=0 ; c<k ; c++ )
     W[(size_t)f*k+c] = (float)U[(size_t)c*nrow+f] ;
   free(U) ;
   return SRM_OK ;
}

/*============================================================================
  Parameters, memory
==============================================================================*/

void SRM_params_default( SRM_params *p )
{
   if( p == NULL ) return ;
   memset(p,0,sizeof(*p)) ;
   p->algo     = SRM_ALGO_PROB ;
   p->nfeat    = 0 ;
   p->niter    = 500 ;
   p->tol      = 1.0e-7 ;
   p->seed     = 1234567u ;
   p->init     = SRM_INIT_RANDOM ;
   p->center   = 1 ;
   p->coupling = SRM_COUPLING_SHARED ;
}

void SRM_free_fitinfo( SRM_fitinfo *fi )
{
   if( fi == NULL ) return ;
   free(fi->objective) ; fi->objective=NULL ;
   fi->niter_done=0 ; fi->converged=0 ;
}

void SRM_free_model( SRM_model *m )
{
   int ii,jj ;
   if( m == NULL ) return ;
   if( m->w != NULL ){
     for( ii=0 ; ii<m->nview ; ii++ ) free(m->w[ii]) ;
     free(m->w) ;
   }
   if( m->mu != NULL ){
     for( ii=0 ; ii<m->nview ; ii++ ) free(m->mu[ii]) ;
     free(m->mu) ;
   }
   if( m->wrun != NULL ){
     for( ii=0 ; ii<m->nview ; ii++ )
       for( jj=0 ; jj<m->nseg ; jj++ ) free(m->wrun[(size_t)ii*m->nseg+jj]) ;
     free(m->wrun) ;
   }
   free(m->nvox) ; free(m->s) ; free(m->rho2) ; free(m->sigma_s) ; free(m->a_sum) ;
   free(m->nsubj_t) ; free(m->rho_inv_t) ;
   free(m) ;
}

static int srm_max_threads( void )
{
#ifdef USE_OMP
   return omp_get_max_threads() ;
#else
   return 1 ;
#endif
}

double SRM_estimate_bytes( int nview , const int *nvox , int ntime ,
                           int nfeat , int algo , int copy_x )
{
   double bytes=0.0 ; int ii,vmax=0 ;
   if( nview<1 || nvox==NULL || ntime<1 || nfeat<1 ) return 0.0 ;
   for( ii=0 ; ii<nview ; ii++ ){
     if( nvox[ii] > vmax ) vmax=nvox[ii] ;
     bytes += 4.0*nvox[ii]*nfeat ;                         /* W_i              */
     if( copy_x ) bytes += 4.0*nvox[ii]*ntime ;            /* working X_i      */
     if( nvox[ii] >= ntime+nfeat )                         /* G_i, C_i, GC_i   */
       bytes += 8.0*(ntime+nfeat)*(ntime+nfeat) + 16.0*(ntime+nfeat)*nfeat ;
   }
   /* per thread: A_i (double V x k), loc (double k x T), 4 k x k scratch */
   bytes += (double)srm_max_threads() *
            ( 8.0*vmax*nfeat + 8.0*nfeat*ntime + 32.0*nfeat*nfeat ) ;
   bytes += 8.0*nfeat*ntime*3.0 ;                          /* a_sum, S, stats  */
   bytes += (algo==SRM_ALGO_PROB) ? 8.0*nfeat*nfeat*4.0 : 0.0 ;
   return bytes ;
}

/*! 1 if every value of x (nvox x T) at a time point that obs marks missing is already 0, so that a view
    with missing points can be used as given (no working copy) when it is not centered either. */
static int srm_missing_zero( const float *x , int nvox , int T , const unsigned char *obs )
{
   int t , f ;
   if( obs==NULL ) return 1 ;
   for( t=0 ; t<T ; t++ ) if( !obs[t] )
     for( f=0 ; f<nvox ; f++ ) if( x[(size_t)f*T+t]!=0.0f ) return 0 ;   /* NaN is not 0 either */
   return 1 ;
}

/*! A copy of n doubles (NULL on failure). */
static double * srm_dup_d( const double *a , int n )
{
   double *c=(double *)malloc(sizeof(double)*(size_t)n) ;
   if( c!=NULL ) memcpy(c,a,sizeof(double)*(size_t)n) ;
   return c ;
}

SRM_model * SRM_model_copy( const SRM_model *m , int drop )
{
   SRM_model *c ; int ii,jj,nn ; size_t sz ;
   if( m == NULL || drop >= m->nview ) return NULL ;
   c = (SRM_model *)srm_calloc(1,sizeof(*c)) ;
   if( c == NULL ) return NULL ;
   nn = m->nview - (drop>=0 ? 1 : 0) ;
   c->algo=m->algo ; c->nview=nn ; c->ntime=m->ntime ; c->nfeat=m->nfeat ;
   c->nseg=m->nseg ; c->rho_inv_sum=m->rho_inv_sum ;
   c->center=m->center ; c->coupled=m->coupled ; c->coupling=m->coupling ;
   c->nupdate=m->nupdate ; c->drift=m->drift ;
   c->nvox  = (int *)srm_calloc(nn,sizeof(int)) ;
   c->w     = (float **)srm_calloc(nn,sizeof(float *)) ;
   if( c->nvox==NULL || c->w==NULL ){ SRM_free_model(c) ; return NULL ; }
   if( m->rho2 != NULL ) c->rho2 = (double *)srm_calloc(nn,sizeof(double)) ;
   if( m->mu   != NULL ) c->mu   = (float **)srm_calloc(nn,sizeof(float *)) ;
   if( m->wrun != NULL ) c->wrun = (float **)srm_calloc((size_t)nn*(m->nseg>0?m->nseg:1),sizeof(float *)) ;
   for( ii=0,nn=0 ; ii<m->nview ; ii++,nn++ ){
     if( ii == drop ){ nn-- ; continue ; }
     c->nvox[nn] = m->nvox[ii] ;
     sz = (size_t)m->nvox[ii]*m->nfeat*sizeof(float) ;
     if( m->w[ii] != NULL ){
       c->w[nn] = (float *)malloc(sz) ;
       if( c->w[nn]==NULL ){ SRM_free_model(c) ; return NULL ; }
       memcpy(c->w[nn],m->w[ii],sz) ;
     }
     if( c->rho2 != NULL ) c->rho2[nn] = m->rho2[ii] ;
     if( c->mu != NULL && m->mu[ii] != NULL ){
       c->mu[nn] = (float *)malloc((size_t)m->nvox[ii]*sizeof(float)) ;
       if( c->mu[nn]==NULL ){ SRM_free_model(c) ; return NULL ; }
       memcpy(c->mu[nn],m->mu[ii],(size_t)m->nvox[ii]*sizeof(float)) ;
     }
     if( c->wrun != NULL ) for( jj=0 ; jj<m->nseg ; jj++ ){
       float *src = m->wrun[(size_t)ii*m->nseg+jj] ;
       if( src != NULL ){
         float *dst = (float *)malloc(sz) ;
         if( dst==NULL ){ SRM_free_model(c) ; return NULL ; }
         memcpy(dst,src,sz) ; c->wrun[(size_t)nn*m->nseg+jj]=dst ;
       }
     }
   }
   if( m->s != NULL ){
     sz = (size_t)m->nfeat*m->ntime*sizeof(float) ;
     c->s = (float *)malloc(sz) ;
     if( c->s==NULL ){ SRM_free_model(c) ; return NULL ; }
     memcpy(c->s,m->s,sz) ;
   }
   if( m->sigma_s != NULL ){
     sz = (size_t)m->nfeat*m->nfeat*sizeof(float) ;
     c->sigma_s = (float *)malloc(sz) ;
     if( c->sigma_s==NULL ){ SRM_free_model(c) ; return NULL ; }
     memcpy(c->sigma_s,m->sigma_s,sz) ;
   }
   /* The running sums include every view, so they cannot be split: a model
      with a view dropped keeps S as a starting value and loses the sums. */
   if( drop < 0 && m->a_sum != NULL ){
     sz = (size_t)m->nfeat*m->ntime*sizeof(double) ;
     c->a_sum = (double *)malloc(sz) ;
     if( c->a_sum==NULL ){ SRM_free_model(c) ; return NULL ; }
     memcpy(c->a_sum,m->a_sum,sz) ;
     c->nsubj_eff = m->nsubj_eff ;
     if( m->nsubj_t != NULL ){
       c->nsubj_t = srm_dup_d(m->nsubj_t,m->ntime) ;
       if( c->nsubj_t==NULL ){ SRM_free_model(c) ; return NULL ; }
     }
     if( m->rho_inv_t != NULL ){
       c->rho_inv_t = srm_dup_d(m->rho_inv_t,m->ntime) ;
       if( c->rho_inv_t==NULL ){ SRM_free_model(c) ; return NULL ; }
     }
   } else {
     c->nsubj_eff = 0 ; c->rho_inv_sum = 0.0 ;
   }
   return c ;
}

/*============================================================================
  Fit
==============================================================================*/

typedef struct {
   int          nvox ;
   const float *x ;        /* the data the fit iterates on (missing: zero)   */
   float       *xc ;       /* owned centered copy, or NULL                   */
   double       xx ;       /* sum of squares of x                            */
   const unsigned char *obs ;   /* observed time points, or NULL for all     */
   int          nobs ;     /* how many                                       */
   double       ndim ;     /* features it stands for (SRM_params.ndim)       */
} srm_view ;

/*! Per-thread scratch: A_i (V x k), a k x T accumulator, and eigen scratch. */
typedef struct { double *A , *loc , *wk ; } srm_ws ;

static void srm_ws_free( srm_ws *w )
{
   free(w->A) ; free(w->loc) ; free(w->wk) ;
   w->A=w->loc=w->wk=NULL ;
}

static int srm_ws_init( srm_ws *w , int vmax , int k , int T , int want_loc )
{
   w->A   = (double *)srm_calloc((size_t)vmax*k,sizeof(double)) ;
   w->wk  = (double *)srm_calloc((size_t)4*k*k,sizeof(double)) ;
   w->loc = want_loc ? (double *)srm_calloc((size_t)k*T,sizeof(double)) : NULL ;
   if( w->A==NULL || w->wk==NULL || (want_loc && w->loc==NULL) ){
     srm_ws_free(w) ; return SRM_ERR_MEMORY ;
   }
   return SRM_OK ;
}

/*! asum (k x T) = sum_i weight[i] * W_i' X_i  (weight NULL means 1).
    The views are computed in parallel but added to asum in view order, so the
    rounding, and hence the whole fit, is identical for any thread count. */
static int srm_pass_sums( int nview , const srm_view *vw , float * const *W ,
                          const double *weight , int vmax , int k , int T ,
                          double *asum )
{
   int err=SRM_OK ;
   memset(asum,0,sizeof(double)*(size_t)k*T) ;
SRM_PRAGMA(omp parallel)
   {
     srm_ws ws ; int ii ;
     int rc = srm_ws_init(&ws,1,k,T,1) ;       /* A is not used in this pass */
     if( rc != SRM_OK ){
SRM_PRAGMA(omp critical(srm_err))
       err = rc ;
     }
SRM_PRAGMA(omp for schedule(dynamic) ordered)
     for( ii=0 ; ii<nview ; ii++ ){
       int ok = (err == SRM_OK && ws.loc != NULL) ;
       if( ok ){
         memset(ws.loc,0,sizeof(double)*(size_t)k*T) ;
         srm_wtx_acc(W[ii],vw[ii].x,vw[ii].nvox,k,T,
                     (weight!=NULL) ? weight[ii] : 1.0,ws.loc) ;
       }
SRM_PRAGMA(omp ordered)
       if( ok ){
         size_t jj ;
         for( jj=0 ; jj<(size_t)k*T ; jj++ ) asum[jj] += ws.loc[jj] ;
       }
     }
     srm_ws_free(&ws) ;
   }
   (void)vmax ;
   return err ;
}

/*! Procrustes step for every view: W_i = polar(X_i S').  If a_out is
    non-NULL, ab_out[i] receives <W_i,X_i S'>  (needed by the PROB variance
    update).  S is k x T, double. */
static int srm_pass_procrustes( int nview , const srm_view *vw , float **W ,
                                const double *S , int vmax , int k , int T ,
                                unsigned long long seed , double *wa_out )
{
   int err=SRM_OK ;
SRM_PRAGMA(omp parallel)
   {
     srm_ws ws ; int ii ;
     int rc = srm_ws_init(&ws,vmax,k,T,0) ;
     if( rc != SRM_OK ){
SRM_PRAGMA(omp critical(srm_err))
       err = rc ;
     }
SRM_PRAGMA(omp for schedule(dynamic))
     for( ii=0 ; ii<nview ; ii++ ){
       int rc2 ;
       if( err != SRM_OK || ws.A==NULL ) continue ;
       srm_xst(vw[ii].x,S,vw[ii].nvox,k,T,ws.A) ;
       rc2 = srm_polar(ws.A,vw[ii].nvox,k,W[ii],ws.wk,seed+(unsigned long long)ii) ;
       if( rc2 != SRM_OK ){
SRM_PRAGMA(omp critical(srm_err))
         err = rc2 ;
       } else if( wa_out != NULL ){
         size_t jj,nn=(size_t)vw[ii].nvox*k ; double acc=0.0 ;
         for( jj=0 ; jj<nn ; jj++ ) acc += (double)W[ii][jj]*ws.A[jj] ;
         wa_out[ii] = acc ;
       }
     }
     srm_ws_free(&ws) ;
   }
   return err ;
}

/*! post = Q diag(lam/(1+rho0 lam)) Q' for symmetric PSD sigma (k x k), and the
    log-determinant of I + rho0*sigma.  Never inverts sigma, so a rank-deficient
    prior covariance is fine. */
static int srm_posterior( int k , const double *sigma , double rho0 ,
                          double *post , double *logdet , double *wk )
{
   double *a=wk, *Q=wk+k*k, *ev=wk+2*k*k ;     /* needs 3*k*k doubles */
   int c,d,j ; double ld=0.0 ;
   memcpy(a,sigma,sizeof(double)*(size_t)k*k) ;
   if( srm_eigsym(k,a,ev,Q) ) return SRM_ERR_SINGULAR ;
   for( j=0 ; j<k ; j++ ){ if( ev[j]<0.0 ) ev[j]=0.0 ; ld += log1p(rho0*ev[j]) ; }
   for( c=0 ; c<k ; c++ ) for( d=c ; d<k ; d++ ){
     double acc=0.0 ;
     for( j=0 ; j<k ; j++ ) acc += Q[c*k+j]*Q[d*k+j]*ev[j]/(1.0+rho0*ev[j]) ;
     post[c*k+d] = post[d*k+c] = acc ;
   }
   if( logdet != NULL ) *logdet = ld ;
   return SRM_OK ;
}

/*! The PROB E-step with missing data.  At each t the posterior of s_t given the views observed
    there has precision Sigma_s^-1 + rho0t[t] I (rho0t: the sum of 1/rho2 over those views), so one
    eigendecomposition Sigma_s = Q diag(ev) Q' serves every t: Cov(s_t) = Q diag(ev/(1+rho0t ev)) Q'.
    Sd (k x T) = E[s_t] = Cov(s_t) asum_t; trt[t] = tr Cov(s_t); postsum (k x k) = sum_t Cov(s_t);
    *logdet = sum_t log det(I + rho0t[t] Sigma_s).  wk: 3*k*k doubles. */
static int srm_posterior_obs( int k , int T , const double *sigma , const double *rho0t ,
                              const double *asum , double *Sd , double *trt , double *postsum ,
                              double *logdet , double *wk )
{
   double *a=wk , *Q=wk+k*k , *ev=wk+2*k*k , *b , *f , *g , ld=0.0 ;
   int c,d,j,t ;
   memcpy(a,sigma,sizeof(double)*(size_t)k*k) ;
   if( srm_eigsym(k,a,ev,Q) ) return SRM_ERR_SINGULAR ;
   b=(double *)srm_calloc(k,sizeof(double)) ; f=(double *)srm_calloc(k,sizeof(double)) ; g=(double *)srm_calloc(k,sizeof(double)) ;
   if( !b || !f || !g ){ free(b) ; free(f) ; free(g) ; return SRM_ERR_MEMORY ; }
   for( j=0 ; j<k ; j++ ) if( ev[j]<0.0 ) ev[j]=0.0 ;
   for( t=0 ; t<T ; t++ ){
     double r=rho0t[t] , tr=0.0 ;
     for( j=0 ; j<k ; j++ ){
       double bj=0.0 ;
       f[j]=ev[j]/(1.0+r*ev[j]) ; ld+=log1p(r*ev[j]) ; g[j]+=f[j] ; tr+=f[j] ;
       for( c=0 ; c<k ; c++ ) bj+=Q[c*k+j]*asum[(size_t)c*T+t] ;
       b[j]=f[j]*bj ;
     }
     for( c=0 ; c<k ; c++ ){
       double acc=0.0 ;
       for( j=0 ; j<k ; j++ ) acc+=Q[c*k+j]*b[j] ;
       Sd[(size_t)c*T+t]=acc ;
     }
     trt[t]=tr ;
   }
   for( c=0 ; c<k ; c++ ) for( d=c ; d<k ; d++ ){
     double acc=0.0 ;
     for( j=0 ; j<k ; j++ ) acc+=Q[c*k+j]*Q[d*k+j]*g[j] ;
     postsum[c*k+d]=postsum[d*k+c]=acc ;
   }
   *logdet=ld ;
   free(b) ; free(f) ; free(g) ;
   return SRM_OK ;
}

/*! dst (k x T) = M (k x k) * src (k x T). */
static void srm_matmul_kk( int k , int T , const double *M , const double *src ,
                           double *dst )
{
   int c ;
SRM_PRAGMA(omp parallel for schedule(static))
   for( c=0 ; c<k ; c++ ){            /* d and t are loop-local: they must not be shared */
     double *out = dst + (size_t)c*T ;
     int d,t ;
     for( t=0 ; t<T ; t++ ) out[t]=0.0 ;
     for( d=0 ; d<k ; d++ ){
       double m = M[(size_t)c*k+d] ; const double *in = src + (size_t)d*T ;
       if( m == 0.0 ) continue ;
       for( t=0 ; t<T ; t++ ) out[t] += m*in[t] ;
     }
   }
}

static double srm_dot( size_t n , const double *a , const double *b )
{
   size_t i ; double acc=0.0 ;
   for( i=0 ; i<n ; i++ ) acc += a[i]*b[i] ;
   return acc ;
}

/*============================================================================
  The fit on Gram matrices (SRM_KERNEL_GRAM).  Every W the fit makes lies in
  the span of Z_i = [X_i | W_i(start)] (V x nz, nz = T+k): a Procrustes step
  gives polar(A) = A M with A = X S' (plus pull * anchor, for runs), an anchor
  is polar of a sum of run W, and the start is in the span by construction.
  So W_i = Z_i C_i with C_i nz x k, and with G_i = Z_i'Z_i
      W'X = ((G C)[0:T,:])'     A = Z B  ->  A'A = B'(G B)     <Z C,Z D> = sum C.*(G D)
  Each view keeps C and GC = G C; X is read once to form G and once at the
  end to form W.  The work in a view is serial and views are added in order,
  so, as for the voxel kernels, the result does not depend on the threads.
==============================================================================*/

#define SRM_GRAM_NB  64     /* voxels per panel when forming G */

typedef struct {
   int      nz , ncoef ;   /* T+k ; coefficient sets per view: 1, or 1+nseg
                              (the anchor, then the runs)                     */
   double **G ;            /* [nview] nz x nz                                 */
   double **C , **GC ;     /* [nview*ncoef] nz x k                            */
} srm_gstate ;

/*! Whether to iterate on Gram matrices (see SRM_params.kernel). */
static int srm_use_gram( const SRM_params *p , int nview , const int *nvox ,
                         int T , int k )
{
   double nz=(double)T+k , cv=0.0 , cg=0.0 ; int ii ;
   if( p->kernel == SRM_KERNEL_VOXEL ) return 0 ;
   if( p->kernel == SRM_KERNEL_GRAM  ) return 1 ;
   for( ii=0 ; ii<nview ; ii++ ){
     double V=nvox[ii] ;
     if( V < nz ) return 0 ;
     cv += (double)p->niter * 2.0*V*T*k ;
     cg += 0.5*V*nz*nz + V*nz*k + (double)p->niter * (nz*T*k + 2.0*nz*k*k) ;
   }
   return (cg < cv) ;
}

/*! G (nz x nz, nz = T+k) = Z'Z, Z = [X | W0] (V x nz).  Panels of voxels are
    copied to double and G is accumulated in 4 x 4 tiles of its upper
    triangle; every entry adds the voxels in order.  Serial: the caller runs
    the views in parallel. */
static int srm_gram_make( const float *X , const float *W0 , int V , int T ,
                          int k , double *G )
{
   int nz=T+k , nzp=(nz+3)&~3 , f0 , nb , j , t , u , a , b ;
   double *P  = (double *)malloc(sizeof(double)*SRM_GRAM_NB*(size_t)nzp) ;
   double *Gp = (double *)calloc((size_t)nzp*nzp,sizeof(double)) ;
   if( P==NULL || Gp==NULL ){ free(P) ; free(Gp) ; return SRM_ERR_MEMORY ; }
   for( f0=0 ; f0<V ; f0+=SRM_GRAM_NB ){
     nb = (V-f0 < SRM_GRAM_NB) ? V-f0 : SRM_GRAM_NB ;
     for( j=0 ; j<nb ; j++ ){
       const float *xr=X+(size_t)(f0+j)*T , *wr=W0+(size_t)(f0+j)*k ;
       double *pr=P+(size_t)j*nzp ;
       for( t=0 ; t<T ; t++ ) pr[t] = xr[t] ;
       for( t=0 ; t<k ; t++ ) pr[T+t] = wr[t] ;
       for( t=nz ; t<nzp ; t++ ) pr[t] = 0.0 ;
     }
     for( t=0 ; t<nzp ; t+=4 ) for( u=t ; u<nzp ; u+=4 ){
       double acc[16] ;
       for( a=0 ; a<16 ; a++ ) acc[a] = 0.0 ;
       for( j=0 ; j<nb ; j++ ){
         const double *pt=P+(size_t)j*nzp+t , *pu=P+(size_t)j*nzp+u ;
         for( a=0 ; a<4 ; a++ ) for( b=0 ; b<4 ; b++ ) acc[a*4+b] += pt[a]*pu[b] ;
       }
       for( a=0 ; a<4 ; a++ ) for( b=0 ; b<4 ; b++ ) Gp[(size_t)(t+a)*nzp+u+b] += acc[a*4+b] ;
     }
   }
   for( t=0 ; t<nz ; t++ ) for( u=t ; u<nz ; u++ )
     G[(size_t)t*nz+u] = G[(size_t)u*nz+t] = Gp[(size_t)t*nzp+u] ;
   free(P) ; free(Gp) ;
   return SRM_OK ;
}

static void srm_gstate_free( srm_gstate *g , int nview )
{
   int ii ;
   if( g->G != NULL ){ for( ii=0 ; ii<nview ; ii++ ) free(g->G[ii]) ; free(g->G) ; }
   if( g->C != NULL ){ for( ii=0 ; ii<nview*g->ncoef ; ii++ ) free(g->C[ii]) ; free(g->C) ; }
   if( g->GC!= NULL ){ for( ii=0 ; ii<nview*g->ncoef ; ii++ ) free(g->GC[ii]) ; free(g->GC) ; }
   g->G=g->C=g->GC=NULL ;
}

/*! G for every view (Z = [X | W0]), and every coefficient set at C = [0 ; I],
    which is W = W0. */
static int srm_gstate_init( srm_gstate *g , int nview , const srm_view *vw ,
                            float * const *W0 , int k , int T , int ncoef )
{
   int ii , q , err=SRM_OK , nz=T+k ;
   g->nz=nz ; g->ncoef=ncoef ;
   g->G  = (double **)srm_calloc(nview,sizeof(double *)) ;
   g->C  = (double **)srm_calloc((size_t)nview*ncoef,sizeof(double *)) ;
   g->GC = (double **)srm_calloc((size_t)nview*ncoef,sizeof(double *)) ;
   if( g->G==NULL || g->C==NULL || g->GC==NULL ) return SRM_ERR_MEMORY ;
   for( ii=0 ; ii<nview ; ii++ ){
     g->G[ii] = (double *)malloc(sizeof(double)*(size_t)nz*nz) ;
     if( g->G[ii]==NULL ) return SRM_ERR_MEMORY ;
     for( q=0 ; q<ncoef ; q++ ){
       g->C [(size_t)ii*ncoef+q] = (double *)malloc(sizeof(double)*(size_t)nz*k) ;
       g->GC[(size_t)ii*ncoef+q] = (double *)malloc(sizeof(double)*(size_t)nz*k) ;
       if( g->C[(size_t)ii*ncoef+q]==NULL || g->GC[(size_t)ii*ncoef+q]==NULL ) return SRM_ERR_MEMORY ;
     }
   }
SRM_PRAGMA(omp parallel for schedule(dynamic))
   for( ii=0 ; ii<nview ; ii++ ){
     int rc , qq , z , c ;
     if( err != SRM_OK ) continue ;
     rc = srm_gram_make(vw[ii].x,W0[ii],vw[ii].nvox,T,k,g->G[ii]) ;
     if( rc != SRM_OK ){
SRM_PRAGMA(omp critical(srm_err))
       err = rc ;
       continue ;
     }
     for( qq=0 ; qq<ncoef ; qq++ ){
       double *C=g->C[(size_t)ii*ncoef+qq] , *GC=g->GC[(size_t)ii*ncoef+qq] ;
       for( z=0 ; z<nz ; z++ ) for( c=0 ; c<k ; c++ ){
         C [(size_t)z*k+c] = (z==T+c) ? 1.0 : 0.0 ;
         GC[(size_t)z*k+c] = g->G[ii][(size_t)z*nz+T+c] ;
       }
     }
   }
   return err ;
}

/*! out (n x k) = A (n x k) M (k x k). */
static void srm_mul_nk_kk( int n , int k , const double *A , const double *M ,
                           double *out )
{
   int z,c,d ;
   for( z=0 ; z<n ; z++ ){
     const double *a=A+(size_t)z*k ; double *o=out+(size_t)z*k ;
     for( c=0 ; c<k ; c++ ) o[c] = 0.0 ;
     for( d=0 ; d<k ; d++ ){
       double ad=a[d] ; const double *mr=M+(size_t)d*k ;
       if( ad == 0.0 ) continue ;
       for( c=0 ; c<k ; c++ ) o[c] += ad*mr[c] ;
     }
   }
}

/*! H (nz x k) = G[:,t0:t0+Tr] Bt[t0:t0+Tr,:], G being nz x nz and Bt having
    rows of length k. */
static void srm_gmul( int nz , int k , const double *G , const double *Bt ,
                      int t0 , int Tr , double *H )
{
   int z,t,c ;
   for( z=0 ; z<nz ; z++ ){
     const double *gr=G+(size_t)z*nz+t0 ; double *h=H+(size_t)z*k ;
     for( c=0 ; c<k ; c++ ) h[c] = 0.0 ;
     for( t=0 ; t<Tr ; t++ ){
       double gv=gr[t] ; const double *br=Bt+(size_t)(t0+t)*k ;
       for( c=0 ; c<k ; c++ ) h[c] += gv*br[c] ;
     }
   }
}

/*! polar(A) in coefficients: A = Z B, H = G B (both nz x k).  C gets the
    coefficients of W = polar(A), and GC = G C.  As in srm_polar, a direction
    with no signal is filled with a deterministic random vector, drawn here in
    the coefficients and made G-orthonormal to the rest, so W stays in the span
    of Z.  wk: 4*k*k doubles. */
static int srm_polar_coef( int nz , int k , const double *G , const double *B ,
                           const double *H , double *C , double *GC , double *wk ,
                           unsigned long long seed )
{
   double *K=wk, *Q=wk+k*k, *M=wk+2*k*k, *ev=wk+3*k*k ;   /* ev uses k of k*k */
   double *U , *GU , lmax ; srm_rng rng ;
   int z,c,d,j,m,pass,nbad,rc ;

   memset(K,0,sizeof(double)*(size_t)k*k) ;
   for( z=0 ; z<nz ; z++ ){
     const double *b=B+(size_t)z*k , *h=H+(size_t)z*k ;
     for( c=0 ; c<k ; c++ ){
       double bc=b[c] ; double *kr=K+(size_t)c*k ;
       if( bc == 0.0 ) continue ;
       for( d=c ; d<k ; d++ ) kr[d] += bc*h[d] ;
     }
   }
   for( c=0 ; c<k ; c++ ) for( d=0 ; d<c ; d++ ) K[c*k+d] = K[d*k+c] ;

   rc = srm_polar_eig(k,K,Q,ev,M,&nbad) ;
   if( rc != SRM_OK ) return rc ;
   if( nbad == 0 ){
     srm_mul_nk_kk(nz,k,B,M,C) ; srm_mul_nk_kk(nz,k,H,M,GC) ;
     return SRM_OK ;
   }

   /* rank-deficient: U = [B q_j / sqrt(ev_j) for good j ; G-orthogonal fill],
      GU = G U, then C = U Q' and GC = GU Q' */
   U  = (double *)srm_calloc((size_t)nz*k,sizeof(double)) ;
   GU = (double *)srm_calloc((size_t)nz*k,sizeof(double)) ;
   if( U==NULL || GU==NULL ){ free(U) ; free(GU) ; return SRM_ERR_MEMORY ; }
   lmax = ev[0] ;
   srm_rng_init(&rng,seed,7) ;
   for( j=0 ; j<k ; j++ ){
     double sc ;
     if( ev[j] <= SRM_POLAR_RANKTOL*lmax ) continue ;
     sc = 1.0/sqrt(ev[j]) ;
     for( z=0 ; z<nz ; z++ ){
       double au=0.0 , ag=0.0 ;
       for( d=0 ; d<k ; d++ ){ au += B[(size_t)z*k+d]*Q[d*k+j] ; ag += H[(size_t)z*k+d]*Q[d*k+j] ; }
       U[(size_t)z*k+j] = au*sc ; GU[(size_t)z*k+j] = ag*sc ;
     }
   }
   for( j=0 ; j<k ; j++ ){
     double n0=0.0 ;
     if( ev[j] > SRM_POLAR_RANKTOL*lmax ) continue ;
     for( z=0 ; z<nz ; z++ ) U[(size_t)z*k+j] = srm_normal(&rng) ;
     for( z=0 ; z<nz ; z++ ){
       const double *gr=G+(size_t)z*nz ; double acc=0.0 ; int y ;
       for( y=0 ; y<nz ; y++ ) acc += gr[y]*U[(size_t)y*k+j] ;
       GU[(size_t)z*k+j] = acc ;
     }
     for( z=0 ; z<nz ; z++ ) n0 += U[(size_t)z*k+j]*GU[(size_t)z*k+j] ;
     if( !(n0 > 0.0) || !isfinite(n0) ){ free(U) ; free(GU) ; return SRM_ERR_SINGULAR ; }
     for( pass=0 ; pass<2 ; pass++ ){           /* twice is enough (GS) */
       double nrm=0.0 ;
       for( m=0 ; m<k ; m++ ){
         double dot=0.0 ;
         if( m==j ) continue ;    /* unfilled columns are zero: harmless */
         for( z=0 ; z<nz ; z++ ) dot += U[(size_t)z*k+m]*GU[(size_t)z*k+j] ;
         for( z=0 ; z<nz ; z++ ){
           U [(size_t)z*k+j] -= dot*U [(size_t)z*k+m] ;
           GU[(size_t)z*k+j] -= dot*GU[(size_t)z*k+m] ;
         }
       }
       for( z=0 ; z<nz ; z++ ) nrm += U[(size_t)z*k+j]*GU[(size_t)z*k+j] ;
       if( !(nrm > 1.0e-20*n0) ){ free(U) ; free(GU) ; return SRM_ERR_SINGULAR ; }
       nrm = 1.0/sqrt(nrm) ; n0 = 1.0 ;
       for( z=0 ; z<nz ; z++ ){ U[(size_t)z*k+j] *= nrm ; GU[(size_t)z*k+j] *= nrm ; }
     }
   }
   for( c=0 ; c<k ; c++ ) for( d=0 ; d<k ; d++ ) M[c*k+d] = Q[d*k+c] ;   /* Q' */
   srm_mul_nk_kk(nz,k,U,M,C) ; srm_mul_nk_kk(nz,k,GU,M,GC) ;
   free(U) ; free(GU) ;
   return SRM_OK ;
}

/*! [S' ; 0] (nz x k) from S (k x T). */
static double * srm_gram_st( const double *S , int k , int T , int nz )
{
   double *St=(double *)srm_calloc((size_t)nz*k,sizeof(double)) ; int c,t ;
   if( St != NULL )
     for( t=0 ; t<T ; t++ ) for( c=0 ; c<k ; c++ ) St[(size_t)t*k+c] = S[(size_t)c*T+t] ;
   return St ;
}

/*! asum (k x T) = sum_i weight[i] W_i' X_i on Gram matrices; columns
    c0[r] .. c0[r]+seglen[r]-1 use coefficient set q0+r.  Views added in order. */
static int srm_gpass_sums( int nview , const srm_gstate *g , const double *weight ,
                           int q0 , int nseg , const int *c0 , const int *seglen ,
                           int k , int T , double *asum )
{
   int ii,rr,t,c ;
   memset(asum,0,sizeof(double)*(size_t)k*T) ;
   for( ii=0 ; ii<nview ; ii++ ){
     double w = (weight!=NULL) ? weight[ii] : 1.0 ;
     for( rr=0 ; rr<nseg ; rr++ ){
       const double *GC=g->GC[(size_t)ii*g->ncoef+q0+rr] ;
       for( t=c0[rr] ; t<c0[rr]+seglen[rr] ; t++ )
         for( c=0 ; c<k ; c++ ) asum[(size_t)c*T+t] += w*GC[(size_t)t*k+c] ;
     }
   }
   return SRM_OK ;
}

/*! Procrustes step for every view on Gram matrices: B = [S' ; 0], H = G B,
    C = polar.  wa_out[i] = <W_i , X_i S'> if non-NULL. */
static int srm_gpass_procrustes( int nview , srm_gstate *g , const double *S ,
                                 int k , int T , unsigned long long seed ,
                                 double *wa_out )
{
   int err=SRM_OK , nz=g->nz ;
   double *St=srm_gram_st(S,k,T,nz) ;
   if( St==NULL ) return SRM_ERR_MEMORY ;
SRM_PRAGMA(omp parallel)
   {
     double *H=(double *)srm_calloc((size_t)nz*k,sizeof(double)) ;
     double *wk=(double *)srm_calloc((size_t)4*k*k,sizeof(double)) ;
     int ii ;
     if( H==NULL || wk==NULL ){
SRM_PRAGMA(omp critical(srm_err))
       err = SRM_ERR_MEMORY ;
     }
SRM_PRAGMA(omp for schedule(dynamic))
     for( ii=0 ; ii<nview ; ii++ ){
       double *C=g->C[(size_t)ii*g->ncoef] , *GC=g->GC[(size_t)ii*g->ncoef] ;
       int rc2 ;
       if( err != SRM_OK || H==NULL || wk==NULL ) continue ;
       srm_gmul(nz,k,g->G[ii],St,0,T,H) ;
       rc2 = srm_polar_coef(nz,k,g->G[ii],St,H,C,GC,wk,seed+(unsigned long long)ii) ;
       if( rc2 != SRM_OK ){
SRM_PRAGMA(omp critical(srm_err))
         err = rc2 ;
       } else if( wa_out != NULL ){
         size_t jj ; double acc=0.0 ;
         for( jj=0 ; jj<(size_t)T*k ; jj++ ) acc += GC[jj]*St[jj] ;
         wa_out[ii] = acc ;
       }
     }
     free(H) ; free(wk) ;
   }
   free(St) ;
   return err ;
}

/*! pen[i] = sum_r <W_ir , W_i> on Gram matrices (set 0 the anchor, 1+r the runs). */
static void srm_gpass_pen( int nview , const srm_gstate *g , int nseg , int k ,
                           double *pen )
{
   int ii,rr ; size_t jj , n=(size_t)g->nz*k ;
   for( ii=0 ; ii<nview ; ii++ ){
     const double *GCa=g->GC[(size_t)ii*g->ncoef] ; double tot=0.0 ;
     for( rr=0 ; rr<nseg ; rr++ ){
       const double *Cr=g->C[(size_t)ii*g->ncoef+1+rr] ; double acc=0.0 ;
       for( jj=0 ; jj<n ; jj++ ) acc += Cr[jj]*GCa[jj] ;
       tot += acc ;
     }
     pen[ii] = tot ;
   }
}

/*! srm_cpass_mstep on Gram matrices: W_ir = polar(X_ir S_r' + pull_i W_i)
    with the old anchor, then W_i = polar(sum_r W_ir). */
static int srm_gpass_mstep( int nview , srm_gstate *g , const double *pull ,
                            const double *S , int nseg , const int *c0 ,
                            const int *seglen , int k , int T ,
                            unsigned long long seed , double *wa_out )
{
   int err=SRM_OK , nz=g->nz ; size_t n=(size_t)nz*k ;
   double *St=srm_gram_st(S,k,T,nz) ;
   if( St==NULL ) return SRM_ERR_MEMORY ;
SRM_PRAGMA(omp parallel)
   {
     double *B  =(double *)srm_calloc(n,sizeof(double)) ;
     double *H  =(double *)srm_calloc(n,sizeof(double)) ;
     double *Cs =(double *)srm_calloc(n,sizeof(double)) ;
     double *GCs=(double *)srm_calloc(n,sizeof(double)) ;
     double *wk =(double *)srm_calloc((size_t)4*k*k,sizeof(double)) ;
     int ii ;
     if( !B || !H || !Cs || !GCs || !wk ){
SRM_PRAGMA(omp critical(srm_err))
       err = SRM_ERR_MEMORY ;
     }
SRM_PRAGMA(omp for schedule(dynamic))
     for( ii=0 ; ii<nview ; ii++ ){
       double *Ca=g->C[(size_t)ii*g->ncoef] , *GCa=g->GC[(size_t)ii*g->ncoef] ;
       double wsum=0.0 , pl=pull[ii] ; int rr , rc2=SRM_OK ; size_t jj ;
       if( err!=SRM_OK || !B || !H || !Cs || !GCs || !wk ) continue ;
       memset(Cs,0,sizeof(double)*n) ; memset(GCs,0,sizeof(double)*n) ;
       for( rr=0 ; rr<nseg && rc2==SRM_OK ; rr++ ){
         double *Cr=g->C[(size_t)ii*g->ncoef+1+rr] , *GCr=g->GC[(size_t)ii*g->ncoef+1+rr] ;
         size_t j0=(size_t)c0[rr]*k , j1=(size_t)(c0[rr]+seglen[rr])*k ;
         for( jj=0 ; jj<n ; jj++ ) B[jj] = pl*Ca[jj] + ((jj>=j0 && jj<j1) ? St[jj] : 0.0) ;
         srm_gmul(nz,k,g->G[ii],St,c0[rr],seglen[rr],H) ;
         for( jj=0 ; jj<n ; jj++ ) H[jj] += pl*GCa[jj] ;
         rc2 = srm_polar_coef(nz,k,g->G[ii],B,H,Cr,GCr,wk,seed+(unsigned long long)ii*64u+rr) ;
         if( rc2 != SRM_OK ) break ;
         for( jj=j0 ; jj<j1 ; jj++ ) wsum += GCr[jj]*St[jj] ;
         for( jj=0 ; jj<n ; jj++ ){ Cs[jj] += Cr[jj] ; GCs[jj] += GCr[jj] ; }
       }
       if( rc2 == SRM_OK )
         rc2 = srm_polar_coef(nz,k,g->G[ii],Cs,GCs,Ca,GCa,wk,seed+(unsigned long long)ii*64u+63u) ;
       if( rc2 != SRM_OK ){
SRM_PRAGMA(omp critical(srm_err))
         err = rc2 ;
       } else wa_out[ii] = wsum ;
     }
     free(B) ; free(H) ; free(Cs) ; free(GCs) ; free(wk) ;
   }
   free(St) ;
   return err ;
}

/*! W (V x k) = Z C = X C[0:T,:] + W0 C[T:T+k,:], row by row; W may be W0.
    Rows are independent, so the threads cannot change the result. */
static int srm_gram_form_w( const float *X , const float *W0 , int V , int T ,
                            int k , const double *C , float *W )
{
   int err=SRM_OK ;
SRM_PRAGMA(omp parallel)
   {
     double *acc=(double *)srm_calloc(k,sizeof(double)) ; int f ;
     if( acc==NULL ){
SRM_PRAGMA(omp critical(srm_err))
       err = SRM_ERR_MEMORY ;
     }
SRM_PRAGMA(omp for schedule(static))
     for( f=0 ; f<V ; f++ ){
       const float *xr=X+(size_t)f*T , *wr=W0+(size_t)f*k ; int t,c ;
       if( acc==NULL ) continue ;
       for( c=0 ; c<k ; c++ ) acc[c] = 0.0 ;
       for( t=0 ; t<T ; t++ ){
         double xv=xr[t] ; const double *cr=C+(size_t)t*k ;
         for( c=0 ; c<k ; c++ ) acc[c] += xv*cr[c] ;
       }
       for( t=0 ; t<k ; t++ ){
         double wv=wr[t] ; const double *cr=C+(size_t)(T+t)*k ;
         for( c=0 ; c<k ; c++ ) acc[c] += wv*cr[c] ;
       }
       for( c=0 ; c<k ; c++ ) W[(size_t)f*k+c] = (float)acc[c] ;
     }
     free(acc) ;
   }
   return err ;
}

static int srm_fit_coupled( int nview , int ntime , const int *nvox ,
                            float * const *x , const SRM_params *p ,
                            SRM_model **model , SRM_fitinfo *info ) ;

int SRM_fit( int nview , int ntime , const int *nvox , float * const *x ,
             const SRM_params *p , SRM_model **model , SRM_fitinfo *info )
{
   int rc=SRM_OK , ii,jj,kk , vmax=0 , it , converged=0 , prob ;
   int T=ntime ;
   srm_view *vw=NULL ; float **W=NULL , **mu=NULL ;
   double *rho2=NULL , *asum=NULL , *Sd=NULL , *sigma_s=NULL , *post=NULL ;
   double *wk=NULL , *wa=NULL , *objective=NULL ;
   double rho0=0.0 , xxsum=0.0 , Lprev=0.0 ;
   SRM_model *m=NULL ;
   srm_gstate gs={0,0,NULL,NULL,NULL} ; int gram=0 , zero=0 ;
   int miss=0 , *nt=NULL ; double *rho0t=NULL , *trt=NULL , *postsum=NULL ;

   if( model!=NULL ) *model=NULL ;
   if( info!=NULL ){ info->niter_done=0 ; info->converged=0 ; info->objective=NULL ; }
   if( nview<1 || ntime<1 || nvox==NULL || x==NULL || p==NULL || model==NULL )
     return SRM_ERR_ARG ;
   if( p->algo!=SRM_ALGO_PROB && p->algo!=SRM_ALGO_DET ) return SRM_ERR_ARG ;
   if( p->niter<1 || !(p->tol>=0.0) ) return SRM_ERR_ARG ;
   kk = p->nfeat ;
   if( kk<1 || kk>ntime ) return SRM_ERR_NFEAT ;
   for( ii=0 ; ii<nview ; ii++ ){
     if( nvox[ii]<1 || x[ii]==NULL ) return SRM_ERR_ARG ;
     if( kk > nvox[ii] ) return SRM_ERR_NFEAT ;
     if( nvox[ii] > vmax ) vmax = nvox[ii] ;
   }
   prob = (p->algo == SRM_ALGO_PROB) ;
   if( p->init==SRM_INIT_MODEL ){
     const SRM_model *im = p->init_model ;
     if( im==NULL || im->nview!=nview || im->nfeat!=kk || im->algo!=p->algo )
       return SRM_ERR_MISMATCH ;
     for( ii=0 ; ii<nview ; ii++ )
       if( im->nvox[ii]!=nvox[ii] || im->w==NULL || im->w[ii]==NULL )
         return SRM_ERR_MISMATCH ;
     if( prob && (im->rho2==NULL || im->sigma_s==NULL) ) return SRM_ERR_MISMATCH ;
   } else if( p->init != SRM_INIT_RANDOM ) return SRM_ERR_ARG ;

   if( p->coupling != SRM_COUPLING_SHARED )
     return srm_fit_coupled(nview,ntime,nvox,x,p,model,info) ;

   vw        = (srm_view *)srm_calloc(nview,sizeof(*vw)) ;
   W         = (float **)srm_calloc(nview,sizeof(float *)) ;
   rho2      = (double *)srm_calloc(nview,sizeof(double)) ;
   wa        = (double *)srm_calloc(nview,sizeof(double)) ;
   asum      = (double *)srm_calloc((size_t)kk*T,sizeof(double)) ;
   Sd        = (double *)srm_calloc((size_t)kk*T,sizeof(double)) ;
   sigma_s   = (double *)srm_calloc((size_t)kk*kk,sizeof(double)) ;
   post      = (double *)srm_calloc((size_t)kk*kk,sizeof(double)) ;
   wk        = (double *)srm_calloc((size_t)4*kk*kk,sizeof(double)) ;
   objective = (double *)srm_calloc((size_t)p->niter+1,sizeof(double)) ;
   if( p->center ) mu = (float **)srm_calloc(nview,sizeof(float *)) ;
   if( !vw||!W||!rho2||!wa||!asum||!Sd||!sigma_s||!post||!wk||!objective
       || (p->center && !mu) ){ rc=SRM_ERR_MEMORY ; goto done ; }

   /* Views: centered working copies if asked, and the total variance of each.  A view with
      missing time points gets a copy too, with its missing values zero, so that every pass can
      run over all columns: a zero column adds nothing to W'X or to X S'. */
   for( ii=0 ; ii<nview ; ii++ ){
     size_t n=(size_t)nvox[ii]*T , jn ; const float *src=x[ii] ; double xx=0.0 ;
     const unsigned char *ob=(p->obs!=NULL) ? p->obs[ii] : NULL ; int nob=T , t ;
     vw[ii].nvox = nvox[ii] ; vw[ii].ndim = (p->ndim!=NULL) ? p->ndim[ii] : (double)nvox[ii] ;
     if( ob!=NULL ){
       for( nob=0,t=0 ; t<T ; t++ ) nob+=(ob[t]!=0) ;
       if( nob<1 ){ rc=SRM_ERR_ARG ; goto done ; }
       if( nob==T ) ob=NULL ; else miss=1 ;
     }
     vw[ii].obs = ob ; vw[ii].nobs = nob ;
     if( p->center || (ob!=NULL && !srm_missing_zero(src,nvox[ii],T,ob)) ){   /* else the data as given */
       float *xc = (float *)malloc(n*sizeof(float)) ;
       if( p->center ) mu[ii] = (float *)malloc((size_t)nvox[ii]*sizeof(float)) ;
       if( xc==NULL || (p->center && mu[ii]==NULL) ){ free(xc) ; rc=SRM_ERR_MEMORY ; goto done ; }
       for( jn=0 ; jn<(size_t)nvox[ii] ; jn++ ){
         const float *r = src + jn*T ; double s=0.0 , mm=0.0 ;
         if( p->center ){
           for( t=0 ; t<T ; t++ ) if( ob==NULL || ob[t] ) s += r[t] ;
           if( !isfinite(s) ){ free(xc) ; rc=SRM_ERR_NONFINITE ; goto done ; }
           mu[ii][jn] = (float)(s/nob) ; mm = mu[ii][jn] ;
         }
         for( t=0 ; t<T ; t++ ) xc[jn*T+t] = (ob==NULL || ob[t]) ? (float)(r[t] - mm) : 0.0f ;
       }
       vw[ii].xc = xc ; src = xc ;
     }
     vw[ii].x = src ;
     for( jn=0 ; jn<n ; jn++ ) xx += (double)src[jn]*src[jn] ;
     if( !isfinite(xx) ){ rc=SRM_ERR_NONFINITE ; goto done ; }
     if( !(xx > 0.0) ){ rc=SRM_ERR_SINGULAR ; goto done ; }
     vw[ii].xx = xx ; xxsum += xx ;
   }

   if( miss ){                                        /* per time point: views observed, and their precision */
     int t ;
     nt=(int *)srm_calloc(T,sizeof(int)) ; rho0t=(double *)srm_calloc(T,sizeof(double)) ;
     trt=(double *)srm_calloc(T,sizeof(double)) ; postsum=(double *)srm_calloc((size_t)kk*kk,sizeof(double)) ;
     if( !nt || !rho0t || !trt || !postsum ){ rc=SRM_ERR_MEMORY ; goto done ; }
     for( ii=0 ; ii<nview ; ii++ ) for( t=0 ; t<T ; t++ ) nt[t]+=(vw[ii].obs==NULL || vw[ii].obs[t]) ;
   }

   /* Start. */
   for( ii=0 ; ii<nview ; ii++ ){
     size_t sz=(size_t)nvox[ii]*kk ;
     W[ii] = (float *)malloc(sz*sizeof(float)) ;
     if( W[ii]==NULL ){ rc=SRM_ERR_MEMORY ; goto done ; }
     if( p->init == SRM_INIT_MODEL ){
       memcpy(W[ii],p->init_model->w[ii],sz*sizeof(float)) ;
     } else if( p->w0!=NULL && p->w0[ii]!=NULL ){   /* a given start (SRM_compress_start) */
       memcpy(W[ii],p->w0[ii],sz*sizeof(float)) ;
     } else {
       rc = srm_random_orthonormal(W[ii],nvox[ii],kk,p->seed,(p->stream!=NULL) ? p->stream[ii] : ii) ;
       if( rc != SRM_OK ) goto done ;
     }
     rho2[ii] = 1.0 ;
   }
   for( jj=0 ; jj<kk ; jj++ ) sigma_s[jj*kk+jj] = 1.0 ;
   if( p->init==SRM_INIT_MODEL && prob ){
     for( ii=0 ; ii<nview ; ii++ ) rho2[ii] = p->init_model->rho2[ii] ;
     for( jj=0 ; jj<kk*kk ; jj++ ) sigma_s[jj] = p->init_model->sigma_s[jj] ;
   }
   gram = srm_use_gram(p,nview,nvox,T,kk) ;
   if( gram ){
     rc = srm_gstate_init(&gs,nview,vw,W,kk,T,1) ;
     if( rc != SRM_OK ) goto done ;
   }

   /* Iterate.  Each pass: sufficient statistics from the current W, rho2;
      objective of the current parameters; stop test; then the update. */
   for( it=0 ; ; it++ ){
     double L ;
     size_t nkt=(size_t)kk*T ;

     if( prob ){
       double logdet=0.0 , logpsi=0.0 , xxw=0.0 ;
       double *wgt=(double *)srm_calloc(nview,sizeof(double)) ;
       if( wgt==NULL ){ rc=SRM_ERR_MEMORY ; goto done ; }
       rho0 = 0.0 ;
       for( ii=0 ; ii<nview ; ii++ ){
         wgt[ii] = 1.0/rho2[ii] ; rho0 += wgt[ii] ;
         logpsi += vw[ii].ndim*log(rho2[ii])*(miss ? (double)vw[ii].nobs/T : 1.0) ;
         xxw += vw[ii].xx/rho2[ii] ;
       }
       rc = gram ? srm_gpass_sums(nview,&gs,wgt,0,1,&zero,&T,kk,T,asum)
                 : srm_pass_sums(nview,vw,W,wgt,vmax,kk,T,asum) ;
       if( rc != SRM_OK ){ free(wgt) ; goto done ; }
       if( !miss ){
         rc = srm_posterior(kk,sigma_s,rho0,post,&logdet,wk) ;
         if( rc != SRM_OK ){ free(wgt) ; goto done ; }
         srm_matmul_kk(kk,T,post,asum,Sd) ;
         L = -0.5*T*(logpsi+logdet) - 0.5*xxw + 0.5*srm_dot(nkt,asum,Sd) ;
       } else {                                       /* each t: the views observed there */
         int t ;
         for( t=0 ; t<T ; t++ ) rho0t[t]=0.0 ;
         for( ii=0 ; ii<nview ; ii++ ) for( t=0 ; t<T ; t++ )
           if( vw[ii].obs==NULL || vw[ii].obs[t] ) rho0t[t]+=wgt[ii] ;
         rc = srm_posterior_obs(kk,T,sigma_s,rho0t,asum,Sd,trt,postsum,&logdet,wk) ;
         if( rc != SRM_OK ){ free(wgt) ; goto done ; }
         L = -0.5*(T*logpsi+logdet) - 0.5*xxw + 0.5*srm_dot(nkt,asum,Sd) ;
       }
       free(wgt) ;
     } else {
       double ss ;
       rc = gram ? srm_gpass_sums(nview,&gs,NULL,0,1,&zero,&T,kk,T,asum)
                 : srm_pass_sums(nview,vw,W,NULL,vmax,kk,T,asum) ;
       if( rc != SRM_OK ) goto done ;
       if( !miss ){
         for( jj=0 ; jj<(int)nkt ; jj++ ) Sd[jj] = asum[jj]/nview ;
         ss = srm_dot(nkt,Sd,Sd) ;
         L = xxsum - nview*ss ;
       } else {                                       /* s_t: the mean over the views observed at t */
         int c,t ;
         L = xxsum ;
         for( t=0 ; t<T ; t++ ){
           double q=0.0 ;
           for( c=0 ; c<kk ; c++ ){
             double v = (nt[t]>0) ? asum[(size_t)c*T+t]/nt[t] : 0.0 ;
             Sd[(size_t)c*T+t]=v ; q+=v*v ;
           }
           L -= nt[t]*q ;
         }
       }
     }
     if( !isfinite(L) ){ rc=SRM_ERR_NONFINITE ; goto done ; }
     objective[it] = L ;
     if( p->verb ) fprintf(stderr,"SRM iteration %d  objective %.10g\n",it,L) ;
     if( p->progress != NULL ) p->progress(it,p->niter,L,p->progress_ctx) ;

     if( it>0 && fabs(L-Lprev) <= p->tol*fabs(L) ){ converged=1 ; break ; }
     if( it >= p->niter ) break ;
     Lprev = L ;

     if( prob ){
       double trpost=0.0 , ss=srm_dot(nkt,Sd,Sd) ;
       int c,d,t ;
       for( c=0 ; c<kk ; c++ ) trpost += post[c*kk+c] ;
       rc = gram ? srm_gpass_procrustes(nview,&gs,Sd,kk,T,p->seed+1000003ULL*(it+1),wa)
                 : srm_pass_procrustes(nview,vw,W,Sd,vmax,kk,T,p->seed+1000003ULL*(it+1),wa) ;
       if( rc != SRM_OK ) goto done ;
       if( !miss ){
         for( ii=0 ; ii<nview ; ii++ ){
           double r = (vw[ii].xx - 2.0*wa[ii] + ss + (double)T*trpost)
                      / ((double)T*vw[ii].ndim) ;
           double rmin = SRM_RHO2_FLOOR*vw[ii].xx/((double)T*vw[ii].ndim) ;
           rho2[ii] = (r>rmin) ? r : rmin ;
         }
       } else {                                       /* each view over its own observed time points */
         double *sq=(double *)srm_calloc(T,sizeof(double)) ;
         if( sq==NULL ){ rc=SRM_ERR_MEMORY ; goto done ; }
         for( c=0 ; c<kk ; c++ ) for( t=0 ; t<T ; t++ ) sq[t] += Sd[(size_t)c*T+t]*Sd[(size_t)c*T+t] ;
         for( ii=0 ; ii<nview ; ii++ ){
           double so=0.0 , r , rmin , den=(double)vw[ii].nobs*vw[ii].ndim ;
           for( t=0 ; t<T ; t++ ) if( vw[ii].obs==NULL || vw[ii].obs[t] ) so += sq[t]+trt[t] ;
           r = (vw[ii].xx - 2.0*wa[ii] + so)/den ;
           rmin = SRM_RHO2_FLOOR*vw[ii].xx/den ;
           rho2[ii] = (r>rmin) ? r : rmin ;
         }
         free(sq) ;
       }
       /* sigma_s <- mean posterior covariance + Sd Sd' / T */
       for( c=0 ; c<kk ; c++ ) for( d=c ; d<kk ; d++ ){
         double acc=0.0 ;
         for( t=0 ; t<T ; t++ ) acc += Sd[(size_t)c*T+t]*Sd[(size_t)d*T+t] ;
         sigma_s[c*kk+d] = sigma_s[d*kk+c] = (miss ? postsum[c*kk+d]/T : post[c*kk+d]) + acc/T ;
       }
     } else {
       rc = gram ? srm_gpass_procrustes(nview,&gs,Sd,kk,T,p->seed+1000003ULL*(it+1),NULL)
                 : srm_pass_procrustes(nview,vw,W,Sd,vmax,kk,T,p->seed+1000003ULL*(it+1),NULL) ;
       if( rc != SRM_OK ) goto done ;
     }
   }

   /* Package.  Sd, asum, post, sigma_s, rho2 are all consistent with the final
      W here (the loop stops before it updates anything). */
   if( gram ){
     for( ii=0 ; ii<nview && rc==SRM_OK ; ii++ )
       rc = srm_gram_form_w(vw[ii].x,W[ii],nvox[ii],T,kk,gs.C[ii],W[ii]) ;
     srm_gstate_free(&gs,nview) ;
     if( rc != SRM_OK ) goto done ;
   }
   m = (SRM_model *)srm_calloc(1,sizeof(*m)) ;
   if( m==NULL ){ rc=SRM_ERR_MEMORY ; goto done ; }
   m->algo=p->algo ; m->nview=nview ; m->ntime=T ; m->nfeat=kk ; m->nseg=1 ;
   m->center = (p->center != 0) ; m->coupled=0 ; m->coupling=SRM_COUPLING_SHARED ;
   m->nvox = (int *)srm_calloc(nview,sizeof(int)) ;
   m->s    = (float *)srm_calloc((size_t)kk*T,sizeof(float)) ;
   m->a_sum= (double *)srm_calloc((size_t)kk*T,sizeof(double)) ;
   if( prob ){
     m->rho2    = (double *)srm_calloc(nview,sizeof(double)) ;
     m->sigma_s = (float *)srm_calloc((size_t)kk*kk,sizeof(float)) ;
   }
   if( !m->nvox || !m->s || !m->a_sum || (prob && (!m->rho2 || !m->sigma_s)) ){
     rc=SRM_ERR_MEMORY ; goto done ;
   }
   for( ii=0 ; ii<nview ; ii++ ) m->nvox[ii]=nvox[ii] ;
   for( jj=0 ; jj<kk*T ; jj++ ){ m->s[jj]=(float)Sd[jj] ; m->a_sum[jj]=asum[jj] ; }
   if( prob ){
     for( ii=0 ; ii<nview ; ii++ ) m->rho2[ii]=rho2[ii] ;
     for( jj=0 ; jj<kk*kk ; jj++ ) m->sigma_s[jj]=(float)sigma_s[jj] ;
     m->rho_inv_sum = rho0 ;
   }
   m->nsubj_eff = nview ;
   if( miss ){                              /* the sums per time point */
     int t ;
     m->nsubj_t = (double *)srm_calloc(T,sizeof(double)) ;
     if( prob ) m->rho_inv_t = srm_dup_d(rho0t,T) ;
     if( m->nsubj_t==NULL || (prob && m->rho_inv_t==NULL) ){ rc=SRM_ERR_MEMORY ; goto done ; }
     for( t=0 ; t<T ; t++ ) m->nsubj_t[t]=nt[t] ;
   }
   m->w = W ; W=NULL ;                      /* ownership moves to the model */
   if( p->center ){ m->mu = mu ; mu=NULL ; }

   if( info != NULL ){
     info->niter_done = it ;
     info->converged  = converged ;
     info->objective  = objective ; objective=NULL ;
   }
   *model = m ; m=NULL ;

done:
   if( vw != NULL ){
     for( ii=0 ; ii<nview ; ii++ ) free(vw[ii].xc) ;
     free(vw) ;
   }
   if( W != NULL ){ for( ii=0 ; ii<nview ; ii++ ) free(W[ii]) ; free(W) ; }
   if( mu != NULL ){ for( ii=0 ; ii<nview ; ii++ ) free(mu[ii]) ; free(mu) ; }
   free(rho2) ; free(wa) ; free(asum) ; free(Sd) ; free(sigma_s) ; free(post) ;
   free(wk) ; free(objective) ; free(nt) ; free(rho0t) ; free(trt) ; free(postsum) ;
   srm_gstate_free(&gs,nview) ;
   SRM_free_model(m) ;
   return rc ;
}

int SRM_fit_multistart( int nview , int ntime , const int *nvox ,
                        float * const *x , const SRM_params *p ,
                        int nstart , SRM_model **model , SRM_fitinfo *info ,
                        double *start_obj , int *best )
{
   SRM_model *keep=NULL ; SRM_fitinfo kinfo ;
   double kobj=0.0 ; int kidx=-1 , ss , rc ;

   if( model!=NULL ) *model=NULL ;
   if( info!=NULL ){ info->niter_done=0 ; info->converged=0 ; info->objective=NULL ; }
   if( best!=NULL ) *best=-1 ;
   if( p==NULL || model==NULL || nstart<1 ) return SRM_ERR_ARG ;
   if( nstart>1 && p->init==SRM_INIT_MODEL ) return SRM_ERR_ARG ;
   memset(&kinfo,0,sizeof(kinfo)) ;

   for( ss=0 ; ss<nstart ; ss++ ){
     SRM_params q = *p ; SRM_model *m=NULL ; SRM_fitinfo fi ; double obj ; int better ;
     q.seed = p->seed + (unsigned int)ss*7919u ;
     if( ss>0 ) q.w0 = NULL ;                       /* a given start is the first start only */
     rc = SRM_fit(nview,ntime,nvox,x,&q,&m,&fi) ;
     if( rc != SRM_OK ){
       SRM_free_model(keep) ; SRM_free_fitinfo(&kinfo) ;
       return rc ;
     }
     obj = fi.objective[fi.niter_done] ;
     if( start_obj != NULL ) start_obj[ss] = obj ;
     better = (kidx<0) || ( (p->algo==SRM_ALGO_PROB) ? (obj>kobj) : (obj<kobj) ) ;
     if( better ){
       SRM_free_model(keep) ; SRM_free_fitinfo(&kinfo) ;
       keep=m ; kinfo=fi ; kobj=obj ; kidx=ss ;
     } else {
       SRM_free_model(m) ; SRM_free_fitinfo(&fi) ;
     }
   }
   *model = keep ;
   if( info != NULL ) *info = kinfo ; else SRM_free_fitinfo(&kinfo) ;
   if( best != NULL ) *best = kidx ;
   return SRM_OK ;
}

/*============================================================================
  Step 3: a separate W per run, tied to a per-subject anchor
==============================================================================*/

/*! asum (k x T) = sum_i weight[i] * sum_r W_ir' X_ir, run r being columns
    c0[r] .. c0[r]+seglen[r]-1.  Views in parallel, added in view order. */
static int srm_cpass_sums( int nview , const srm_view *vw , float * const *Wr ,
                           const double *weight , int nseg , const int *c0 ,
                           const int *seglen , int k , int T , double *asum )
{
   int err=SRM_OK ;
   memset(asum,0,sizeof(double)*(size_t)k*T) ;
SRM_PRAGMA(omp parallel)
   {
     double *loc=(double *)srm_calloc((size_t)k*T,sizeof(double)) ; int ii ;
     if( loc==NULL ){
SRM_PRAGMA(omp critical(srm_err))
       err=SRM_ERR_MEMORY ;
     }
SRM_PRAGMA(omp for schedule(dynamic) ordered)
     for( ii=0 ; ii<nview ; ii++ ){
       int ok=(err==SRM_OK && loc!=NULL) ;
       if( ok ){
         int rr ;
         memset(loc,0,sizeof(double)*(size_t)k*T) ;
         for( rr=0 ; rr<nseg ; rr++ )
           srm_wtx_acc_ld(Wr[(size_t)ii*nseg+rr],vw[ii].x+c0[rr],(size_t)T,vw[ii].nvox,k,
                          seglen[rr],(weight!=NULL) ? weight[ii] : 1.0,loc+c0[rr],(size_t)T) ;
       }
SRM_PRAGMA(omp ordered)
       if( ok ){
         size_t jj ;
         for( jj=0 ; jj<(size_t)k*T ; jj++ ) asum[jj] += loc[jj] ;
       }
     }
     free(loc) ;
   }
   return err ;
}

/*! pen[i] = sum_r <W_ir , W_i> */
static void srm_cpass_pen( int nview , const int *nvox , float * const *Wa ,
                           float * const *Wr , int nseg , int k , double *pen )
{
   int ii ;
SRM_PRAGMA(omp parallel for schedule(dynamic))
   for( ii=0 ; ii<nview ; ii++ ){
     int rr ; double tot=0.0 ; size_t jj , n=(size_t)nvox[ii]*k ;
     for( rr=0 ; rr<nseg ; rr++ ){
       const float *w=Wr[(size_t)ii*nseg+rr] ; double acc=0.0 ;
       for( jj=0 ; jj<n ; jj++ ) acc += (double)w[jj]*Wa[ii][jj] ;
       tot += acc ;
     }
     pen[ii]=tot ;
   }
}

/*! The M-step for W: for every view, W_ir = polar(X_ir S_r' + pull_i W_i) for
    each run, with the OLD anchor, then the anchor W_i = polar(sum_r W_ir).
    wa_out[i] = sum_r <W_ir , X_ir S_r'> (the data part), for the variance update. */
static int srm_cpass_mstep( int nview , const srm_view *vw , float **Wa ,
                            float **Wr , const double *pull , const double *S ,
                            int nseg , const int *c0 , const int *seglen ,
                            int vmax , int k , int T , unsigned long long seed ,
                            double *wa_out )
{
   int err=SRM_OK ;
SRM_PRAGMA(omp parallel)
   {
     srm_ws ws ; int ii ;
     double *Atot=(double *)srm_calloc((size_t)vmax*k,sizeof(double)) ;
     double *Aanc=(double *)srm_calloc((size_t)vmax*k,sizeof(double)) ;
     int rc=srm_ws_init(&ws,vmax,k,T,0) ;
     if( rc!=SRM_OK || Atot==NULL || Aanc==NULL ){
SRM_PRAGMA(omp critical(srm_err))
       err=SRM_ERR_MEMORY ;
     }
SRM_PRAGMA(omp for schedule(dynamic))
     for( ii=0 ; ii<nview ; ii++ ){
       int rr , rc2=SRM_OK ; size_t jj , n=(size_t)vw[ii].nvox*k ; double wsum=0.0 ;
       if( err!=SRM_OK || ws.A==NULL || Atot==NULL || Aanc==NULL ) continue ;
       memset(Aanc,0,sizeof(double)*n) ;
       for( rr=0 ; rr<nseg && rc2==SRM_OK ; rr++ ){
         float *wnew=Wr[(size_t)ii*nseg+rr] ;
         srm_xst_ld(vw[ii].x+c0[rr],(size_t)T,S+c0[rr],(size_t)T,vw[ii].nvox,k,seglen[rr],ws.A) ;
         for( jj=0 ; jj<n ; jj++ ) Atot[jj] = ws.A[jj] + pull[ii]*(double)Wa[ii][jj] ;
         rc2=srm_polar(Atot,vw[ii].nvox,k,wnew,ws.wk,seed+(unsigned long long)ii*64u+rr) ;
         if( rc2!=SRM_OK ) break ;
         for( jj=0 ; jj<n ; jj++ ){ wsum += (double)wnew[jj]*ws.A[jj] ; Aanc[jj] += (double)wnew[jj] ; }
       }
       if( rc2==SRM_OK )
         rc2=srm_polar(Aanc,vw[ii].nvox,k,Wa[ii],ws.wk,seed+(unsigned long long)ii*64u+63u) ;
       if( rc2!=SRM_OK ){
SRM_PRAGMA(omp critical(srm_err))
         err=rc2 ;
       } else wa_out[ii]=wsum ;
     }
     srm_ws_free(&ws) ; free(Atot) ; free(Aanc) ;
   }
   return err ;
}

static int srm_fit_coupled( int nview , int ntime , const int *nvox ,
                            float * const *x , const SRM_params *p ,
                            SRM_model **model , SRM_fitinfo *info )
{
   int rc=SRM_OK , ii,rr,jj,it , vmax=0 , converged=0 ;
   int kk=p->nfeat , T=ntime , nseg=p->nseg , prob=(p->algo==SRM_ALGO_PROB) ;
   double lam=p->coupling , rho0=0.0 , xxsum=0.0 , Lprev=0.0 ;
   srm_view *vw=NULL ; float **Wa=NULL , **Wr=NULL , **mu=NULL ;
   double *rho2=NULL , *asum=NULL , *Sd=NULL , *sigma_s=NULL , *post=NULL ;
   double *wk=NULL , *wa=NULL , *pen=NULL , *objective=NULL , *tau=NULL , *pull=NULL ;
   int *c0=NULL ;
   SRM_model *start=NULL , *m=NULL ; const SRM_model *ini ;
   srm_gstate gs={0,0,NULL,NULL,NULL} ; int gram=0 , wr_given=0 , gnow , nmstep=0 ;
   int miss=0 , *nt=NULL ; double *rho0t=NULL , *trt=NULL , *postsum=NULL ;

   if( model!=NULL ) *model=NULL ;
   if( info!=NULL ){ info->niter_done=0 ; info->converged=0 ; info->objective=NULL ; }
   if( !isfinite(lam) || lam<0.0 ) return SRM_ERR_ARG ;
   if( nseg<2 || p->seglen==NULL ) return SRM_ERR_ARG ;
   { int sum=0 ;
     for( rr=0 ; rr<nseg ; rr++ ){ if( p->seglen[rr]<1 ) return SRM_ERR_ARG ; sum+=p->seglen[rr] ; }
     if( sum != T ) return SRM_ERR_ARG ; }
   if( p->obs!=NULL && lam==0.0 ){                      /* free runs: each W_ir has only its own samples */
     int tt , c=0 , n ;
     for( rr=0 ; rr<nseg ; c+=p->seglen[rr],rr++ ) for( ii=0 ; ii<nview ; ii++ ) if( p->obs[ii]!=NULL ){
       for( n=0,tt=c ; tt<c+p->seglen[rr] ; tt++ ) n+=(p->obs[ii][tt]!=0) ;
       if( n<kk ) return SRM_ERR_ARG ;
     }
   }
   for( ii=0 ; ii<nview ; ii++ ) if( nvox[ii]>vmax ) vmax=nvox[ii] ;

   /* Where to start: a shared-W fit (or the caller's model), so that large
      lambda returns the shared solution. */
   if( p->init == SRM_INIT_RANDOM ){
     SRM_params q=*p ; SRM_fitinfo f0 ;
     q.coupling=SRM_COUPLING_SHARED ; q.nseg=0 ; q.seglen=NULL ;
     rc = SRM_fit(nview,ntime,nvox,x,&q,&start,&f0) ;
     SRM_free_fitinfo(&f0) ;
     if( rc != SRM_OK ) return rc ;
     ini = start ;
   } else ini = p->init_model ;

   c0 = (int *)srm_calloc(nseg,sizeof(int)) ;
   vw = (srm_view *)srm_calloc(nview,sizeof(*vw)) ;
   Wa = (float **)srm_calloc(nview,sizeof(float *)) ;
   Wr = (float **)srm_calloc((size_t)nview*nseg,sizeof(float *)) ;
   rho2 = (double *)srm_calloc(nview,sizeof(double)) ;
   wa  = (double *)srm_calloc(nview,sizeof(double)) ;
   pen = (double *)srm_calloc(nview,sizeof(double)) ;
   tau = (double *)srm_calloc(nview,sizeof(double)) ;
   pull = (double *)srm_calloc(nview,sizeof(double)) ;
   asum = (double *)srm_calloc((size_t)kk*T,sizeof(double)) ;
   Sd   = (double *)srm_calloc((size_t)kk*T,sizeof(double)) ;
   sigma_s = (double *)srm_calloc((size_t)kk*kk,sizeof(double)) ;
   post = (double *)srm_calloc((size_t)kk*kk,sizeof(double)) ;
   wk   = (double *)srm_calloc((size_t)4*kk*kk,sizeof(double)) ;
   objective = (double *)srm_calloc((size_t)p->niter+1,sizeof(double)) ;
   if( p->center ) mu = (float **)srm_calloc(nview,sizeof(float *)) ;
   if( !c0||!vw||!Wa||!Wr||!rho2||!wa||!pen||!tau||!pull||!asum||!Sd||!sigma_s||!post||!wk||!objective
       || (p->center && !mu) ){ rc=SRM_ERR_MEMORY ; goto done ; }
   for( rr=1 ; rr<nseg ; rr++ ) c0[rr]=c0[rr-1]+p->seglen[rr-1] ;

   for( ii=0 ; ii<nview ; ii++ ){                     /* data: centered copies, energies; missing values 0 */
     size_t n=(size_t)nvox[ii]*T , jn ; const float *src=x[ii] ; double xx=0.0 ;
     const unsigned char *ob=(p->obs!=NULL) ? p->obs[ii] : NULL ; int nob=T , t ;
     vw[ii].nvox=nvox[ii] ; vw[ii].ndim = (p->ndim!=NULL) ? p->ndim[ii] : (double)nvox[ii] ;
     if( ob!=NULL ){
       for( nob=0,t=0 ; t<T ; t++ ) nob+=(ob[t]!=0) ;
       if( nob<1 ){ rc=SRM_ERR_ARG ; goto done ; }
       if( nob==T ) ob=NULL ; else miss=1 ;
     }
     vw[ii].obs=ob ; vw[ii].nobs=nob ;
     if( p->center || (ob!=NULL && !srm_missing_zero(src,nvox[ii],T,ob)) ){   /* else the data as given */
       float *xc=(float *)malloc(n*sizeof(float)) ;
       if( p->center ) mu[ii]=(float *)malloc((size_t)nvox[ii]*sizeof(float)) ;
       if( xc==NULL || (p->center && mu[ii]==NULL) ){ free(xc) ; rc=SRM_ERR_MEMORY ; goto done ; }
       for( jn=0 ; jn<(size_t)nvox[ii] ; jn++ ){
         const float *r=src+jn*T ; double s=0.0 , mm=0.0 ;
         if( p->center ){
           for( t=0 ; t<T ; t++ ) if( ob==NULL || ob[t] ) s += r[t] ;
           if( !isfinite(s) ){ free(xc) ; rc=SRM_ERR_NONFINITE ; goto done ; }
           mu[ii][jn]=(float)(s/nob) ; mm=mu[ii][jn] ;
         }
         for( t=0 ; t<T ; t++ ) xc[jn*T+t] = (ob==NULL || ob[t]) ? (float)(r[t]-mm) : 0.0f ;
       }
       vw[ii].xc=xc ; src=xc ;
     }
     vw[ii].x=src ;
     for( jn=0 ; jn<n ; jn++ ) xx += (double)src[jn]*src[jn] ;
     if( !isfinite(xx) ){ rc=SRM_ERR_NONFINITE ; goto done ; }
     if( !(xx>0.0) ){ rc=SRM_ERR_SINGULAR ; goto done ; }
     vw[ii].xx=xx ; xxsum+=xx ;
   }
   if( miss ){                                        /* per time point: views observed, and their precision */
     int t ;
     nt=(int *)srm_calloc(T,sizeof(int)) ; rho0t=(double *)srm_calloc(T,sizeof(double)) ;
     trt=(double *)srm_calloc(T,sizeof(double)) ; postsum=(double *)srm_calloc((size_t)kk*kk,sizeof(double)) ;
     if( !nt || !rho0t || !trt || !postsum ){ rc=SRM_ERR_MEMORY ; goto done ; }
     for( ii=0 ; ii<nview ; ii++ ) for( t=0 ; t<T ; t++ ) nt[t]+=(vw[ii].obs==NULL || vw[ii].obs[t]) ;
   }

   for( ii=0 ; ii<nview ; ii++ ){                     /* start state */
     size_t sz=(size_t)nvox[ii]*kk ;
     Wa[ii]=(float *)malloc(sz*sizeof(float)) ;
     if( Wa[ii]==NULL ){ rc=SRM_ERR_MEMORY ; goto done ; }
     memcpy(Wa[ii],ini->w[ii],sz*sizeof(float)) ;
     for( rr=0 ; rr<nseg ; rr++ ){
       const float *src = ini->w[ii] ;
       if( ini->wrun!=NULL && ini->nseg==nseg && ini->wrun[(size_t)ii*nseg+rr]!=NULL )
         src = ini->wrun[(size_t)ii*nseg+rr] ;
       Wr[(size_t)ii*nseg+rr]=(float *)malloc(sz*sizeof(float)) ;
       if( Wr[(size_t)ii*nseg+rr]==NULL ){ rc=SRM_ERR_MEMORY ; goto done ; }
       memcpy(Wr[(size_t)ii*nseg+rr],src,sz*sizeof(float)) ;
     }
     rho2[ii] = (prob && ini->rho2!=NULL) ? ini->rho2[ii] : 1.0 ;
   }
   if( prob && ini->sigma_s!=NULL ){
     for( jj=0 ; jj<kk*kk ; jj++ ) sigma_s[jj]=ini->sigma_s[jj] ;
   } else for( jj=0 ; jj<kk ; jj++ ) sigma_s[jj*kk+jj]=1.0 ;

   /* On Gram matrices the basis is [X_i | anchor at the start], and every run
      W starts as the anchor.  Run W given apart from the anchor are not in that
      span: the first pass, which is the only one that uses them, then reads the
      data directly. */
   gram = srm_use_gram(p,nview,nvox,T,kk) ;
   wr_given = (ini->wrun!=NULL && ini->nseg==nseg) ;
   if( gram ){
     rc = srm_gstate_init(&gs,nview,vw,Wa,kk,T,1+nseg) ;
     if( rc != SRM_OK ) goto done ;
   }

   /* The prior on W_ir is worth `lam` time points of data.  A run of T_r
      samples gives X_r S_r' ~ T_r s2 W, with s2 the mean variance of S's rows, so the
      matching pull is lam*s2*W_i.  s2 is taken once, at the start, so the objective
      being optimized does not change under the optimizer.  For PROB the data enter
      divided by rho2, so the prior weight is tau_i = lam*s2/rho2_i(0), fixed. */
   for( it=0 ; ; it++ ){
     double L , penalty=0.0 ; size_t nkt=(size_t)kk*T ;

     gnow = gram && !(it==0 && wr_given) ;
     if( gnow ) srm_gpass_pen(nview,&gs,nseg,kk,pen) ;
     else       srm_cpass_pen(nview,nvox,Wa,Wr,nseg,kk,pen) ;

     if( prob ){
       double logdet=0.0 , logpsi=0.0 , xxw=0.0 ;
       double *wgt=(double *)srm_calloc(nview,sizeof(double)) ;
       if( wgt==NULL ){ rc=SRM_ERR_MEMORY ; goto done ; }
       rho0=0.0 ;
       for( ii=0 ; ii<nview ; ii++ ){
         wgt[ii]=1.0/rho2[ii] ; rho0+=wgt[ii] ;
         logpsi += vw[ii].ndim*log(rho2[ii])*(miss ? (double)vw[ii].nobs/T : 1.0) ;
         xxw += vw[ii].xx/rho2[ii] ;
       }
       rc = gnow ? srm_gpass_sums(nview,&gs,wgt,1,nseg,c0,p->seglen,kk,T,asum)
                 : srm_cpass_sums(nview,vw,Wr,wgt,nseg,c0,p->seglen,kk,T,asum) ;
       if( rc!=SRM_OK ){ free(wgt) ; goto done ; }
       if( !miss ){
         rc=srm_posterior(kk,sigma_s,rho0,post,&logdet,wk) ;
         if( rc!=SRM_OK ){ free(wgt) ; goto done ; }
         srm_matmul_kk(kk,T,post,asum,Sd) ;
       } else {                                       /* each t: the views observed there */
         int t ;
         for( t=0 ; t<T ; t++ ) rho0t[t]=0.0 ;
         for( ii=0 ; ii<nview ; ii++ ) for( t=0 ; t<T ; t++ )
           if( vw[ii].obs==NULL || vw[ii].obs[t] ) rho0t[t]+=wgt[ii] ;
         rc=srm_posterior_obs(kk,T,sigma_s,rho0t,asum,Sd,trt,postsum,&logdet,wk) ;
         if( rc!=SRM_OK ){ free(wgt) ; goto done ; }
       }
       free(wgt) ;
       if( it==0 ){
         double s2=srm_dot(nkt,Sd,Sd)/((double)kk*T) ;
         for( ii=0 ; ii<nview ; ii++ ) tau[ii]=lam*s2/rho2[ii] ;
       }
       for( ii=0 ; ii<nview ; ii++ ) penalty += tau[ii]*pen[ii] ;
       L = miss ? -0.5*(T*logpsi+logdet) - 0.5*xxw + 0.5*srm_dot(nkt,asum,Sd) + penalty
                : -0.5*T*(logpsi+logdet) - 0.5*xxw + 0.5*srm_dot(nkt,asum,Sd) + penalty ;
     } else {
       double ss=0.0 ;
       rc = gnow ? srm_gpass_sums(nview,&gs,NULL,1,nseg,c0,p->seglen,kk,T,asum)
                 : srm_cpass_sums(nview,vw,Wr,NULL,nseg,c0,p->seglen,kk,T,asum) ;
       if( rc!=SRM_OK ) goto done ;
       if( !miss ){
         for( jj=0 ; jj<(int)nkt ; jj++ ) Sd[jj]=asum[jj]/nview ;
         ss=srm_dot(nkt,Sd,Sd) ;
         L = xxsum - nview*ss ;
       } else {                                       /* s_t: the mean over the views observed at t */
         int c,t ;
         L = xxsum ;
         for( t=0 ; t<T ; t++ ){
           double q=0.0 ;
           for( c=0 ; c<kk ; c++ ){
             double v = (nt[t]>0) ? asum[(size_t)c*T+t]/nt[t] : 0.0 ;
             Sd[(size_t)c*T+t]=v ; q+=v*v ;
           }
           L -= nt[t]*q ; ss+=q ;
         }
       }
       if( it==0 ){
         double s2=ss/((double)kk*T) ;                       /* ss is |S|^2 */
         for( ii=0 ; ii<nview ; ii++ ) tau[ii]=lam*s2 ;
       }
       for( ii=0 ; ii<nview ; ii++ ) penalty += 2.0*tau[ii]*((double)nseg*kk - pen[ii]) ;
       L += penalty ;
     }
     if( !isfinite(L) ){ rc=SRM_ERR_NONFINITE ; goto done ; }
     objective[it]=L ;
     if( p->verb ) fprintf(stderr,"SRM coupled iteration %d  objective %.10g\n",it,L) ;
     if( p->progress != NULL ) p->progress(it,p->niter,L,p->progress_ctx) ;
     if( it>0 && fabs(L-Lprev) <= p->tol*fabs(L) ){ converged=1 ; break ; }
     if( it >= p->niter ) break ;
     Lprev=L ;

     for( ii=0 ; ii<nview ; ii++ ) pull[ii] = prob ? tau[ii]*rho2[ii] : tau[ii] ;
     rc = gram ? srm_gpass_mstep(nview,&gs,pull,Sd,nseg,c0,p->seglen,
                                 kk,T,p->seed+1000003ULL*(it+1),wa)
               : srm_cpass_mstep(nview,vw,Wa,Wr,pull,Sd,nseg,c0,p->seglen,
                                 vmax,kk,T,p->seed+1000003ULL*(it+1),wa) ;
     if( rc!=SRM_OK ) goto done ;
     nmstep++ ;
     if( prob ){
       double trpost=0.0 , ss=srm_dot(nkt,Sd,Sd) ; int c,d,t ;
       for( c=0 ; c<kk ; c++ ) trpost += post[c*kk+c] ;
       if( !miss ){
         for( ii=0 ; ii<nview ; ii++ ){
           double r=(vw[ii].xx - 2.0*wa[ii] + ss + (double)T*trpost) / ((double)T*vw[ii].ndim) ;
           double rmin=SRM_RHO2_FLOOR*vw[ii].xx/((double)T*vw[ii].ndim) ;
           rho2[ii]=(r>rmin) ? r : rmin ;
         }
       } else {                                       /* each view over its own observed time points */
         double *sq=(double *)srm_calloc(T,sizeof(double)) ;
         if( sq==NULL ){ rc=SRM_ERR_MEMORY ; goto done ; }
         for( c=0 ; c<kk ; c++ ) for( t=0 ; t<T ; t++ ) sq[t] += Sd[(size_t)c*T+t]*Sd[(size_t)c*T+t] ;
         for( ii=0 ; ii<nview ; ii++ ){
           double so=0.0 , r , rmin , den=(double)vw[ii].nobs*vw[ii].ndim ;
           for( t=0 ; t<T ; t++ ) if( vw[ii].obs==NULL || vw[ii].obs[t] ) so += sq[t]+trt[t] ;
           r=(vw[ii].xx - 2.0*wa[ii] + so)/den ;
           rmin=SRM_RHO2_FLOOR*vw[ii].xx/den ;
           rho2[ii]=(r>rmin) ? r : rmin ;
         }
         free(sq) ;
       }
       for( c=0 ; c<kk ; c++ ) for( d=c ; d<kk ; d++ ){
         double acc=0.0 ;
         for( t=0 ; t<T ; t++ ) acc += Sd[(size_t)c*T+t]*Sd[(size_t)d*T+t] ;
         sigma_s[c*kk+d]=sigma_s[d*kk+c]=(miss ? postsum[c*kk+d]/T : post[c*kk+d])+acc/T ;
       }
     }
   }

   if( gram ){                      /* run W first: the anchor is their basis */
     for( ii=0 ; ii<nview && rc==SRM_OK && nmstep>0 ; ii++ ){
       for( rr=0 ; rr<nseg && rc==SRM_OK ; rr++ )
         rc = srm_gram_form_w(vw[ii].x,Wa[ii],nvox[ii],T,kk,gs.C[(size_t)ii*(1+nseg)+1+rr],
                              Wr[(size_t)ii*nseg+rr]) ;
       if( rc==SRM_OK )
         rc = srm_gram_form_w(vw[ii].x,Wa[ii],nvox[ii],T,kk,gs.C[(size_t)ii*(1+nseg)],Wa[ii]) ;
     }
     srm_gstate_free(&gs,nview) ;
     if( rc != SRM_OK ) goto done ;
   }
   m=(SRM_model *)srm_calloc(1,sizeof(*m)) ;
   if( m==NULL ){ rc=SRM_ERR_MEMORY ; goto done ; }
   m->algo=p->algo ; m->nview=nview ; m->ntime=T ; m->nfeat=kk ; m->nseg=nseg ;
   m->center=(p->center!=0) ; m->coupled=1 ; m->coupling=lam ;
   m->nvox=(int *)srm_calloc(nview,sizeof(int)) ;
   m->s=(float *)srm_calloc((size_t)kk*T,sizeof(float)) ;
   m->a_sum=(double *)srm_calloc((size_t)kk*T,sizeof(double)) ;
   if( prob ){
     m->rho2=(double *)srm_calloc(nview,sizeof(double)) ;
     m->sigma_s=(float *)srm_calloc((size_t)kk*kk,sizeof(float)) ;
   }
   if( !m->nvox||!m->s||!m->a_sum||(prob&&(!m->rho2||!m->sigma_s)) ){ rc=SRM_ERR_MEMORY ; goto done ; }
   for( ii=0 ; ii<nview ; ii++ ) m->nvox[ii]=nvox[ii] ;
   for( jj=0 ; jj<kk*T ; jj++ ){ m->s[jj]=(float)Sd[jj] ; m->a_sum[jj]=asum[jj] ; }
   if( prob ){
     for( ii=0 ; ii<nview ; ii++ ) m->rho2[ii]=rho2[ii] ;
     for( jj=0 ; jj<kk*kk ; jj++ ) m->sigma_s[jj]=(float)sigma_s[jj] ;
     m->rho_inv_sum=rho0 ;
   }
   m->nsubj_eff=nview ;
   if( miss ){                                        /* the sums per time point */
     int t ;
     m->nsubj_t=(double *)srm_calloc(T,sizeof(double)) ;
     if( prob ) m->rho_inv_t=srm_dup_d(rho0t,T) ;
     if( m->nsubj_t==NULL || (prob && m->rho_inv_t==NULL) ){ rc=SRM_ERR_MEMORY ; goto done ; }
     for( t=0 ; t<T ; t++ ) m->nsubj_t[t]=nt[t] ;
   }
   m->w=Wa ; Wa=NULL ;
   m->wrun=Wr ; Wr=NULL ;
   if( p->center ){ m->mu=mu ; mu=NULL ; }
   if( info!=NULL ){
     info->niter_done=it ; info->converged=converged ; info->objective=objective ; objective=NULL ;
   }
   if( model!=NULL ){ *model=m ; m=NULL ; }

done:
   if( vw!=NULL ){ for( ii=0 ; ii<nview ; ii++ ) free(vw[ii].xc) ; free(vw) ; }
   if( Wa!=NULL ){ for( ii=0 ; ii<nview ; ii++ ) free(Wa[ii]) ; free(Wa) ; }
   if( Wr!=NULL ){ for( ii=0 ; ii<nview*nseg ; ii++ ) free(Wr[ii]) ; free(Wr) ; }
   if( mu!=NULL ){ for( ii=0 ; ii<nview ; ii++ ) free(mu[ii]) ; free(mu) ; }
   free(c0) ; free(rho2) ; free(wa) ; free(pen) ; free(tau) ; free(pull) ; free(asum) ; free(Sd) ; free(sigma_s) ;
   free(post) ; free(wk) ; free(objective) ; free(nt) ; free(rho0t) ; free(trt) ; free(postsum) ;
   srm_gstate_free(&gs,nview) ;
   SRM_free_model(start) ; SRM_free_model(m) ;
   return rc ;
}

int SRM_fit_view_coupled( const float *s , int nfeat , int ntime , const float *x ,
                          int nvox , int nseg , const int *seglen ,
                          const float *w_anchor , double coupling ,
                          float * const *wrun_out )
{
   double *Sd=NULL , *A=NULL , *wk=NULL ; int rr , rc=SRM_OK , sum=0 , c0=0 ; size_t jj ;
   double s2=0.0 , pull ;
   if( s==NULL || x==NULL || seglen==NULL || w_anchor==NULL || wrun_out==NULL ) return SRM_ERR_ARG ;
   if( nfeat<1 || ntime<1 || nvox<1 || nseg<1 ) return SRM_ERR_ARG ;
   if( !isfinite(coupling) || coupling<0.0 ) return SRM_ERR_ARG ;
   for( rr=0 ; rr<nseg ; rr++ ){ if( seglen[rr]<1 || wrun_out[rr]==NULL ) return SRM_ERR_ARG ; sum+=seglen[rr] ; }
   if( sum != ntime ) return SRM_ERR_ARG ;
   if( nfeat>nvox ) return SRM_ERR_NFEAT ;

   Sd=(double *)srm_calloc((size_t)nfeat*ntime,sizeof(double)) ;
   A =(double *)srm_calloc((size_t)nvox*nfeat,sizeof(double)) ;
   wk=(double *)srm_calloc((size_t)4*nfeat*nfeat,sizeof(double)) ;
   if( !Sd||!A||!wk ){ rc=SRM_ERR_MEMORY ; goto done ; }
   for( jj=0 ; jj<(size_t)nfeat*ntime ; jj++ ){
     if( !isfinite(s[jj]) ){ rc=SRM_ERR_NONFINITE ; goto done ; }
     Sd[jj]=s[jj] ; s2 += (double)s[jj]*s[jj] ;
   }
   s2 /= (double)nfeat*ntime ;
   pull = coupling*s2 ;       /* the prior is worth `coupling` time points of data */
   for( rr=0 ; rr<nseg ; rr++ ){
     int f , Tr=seglen[rr] ;
SRM_PRAGMA(omp parallel for schedule(static))
     for( f=0 ; f<nvox ; f++ )
       srm_xst_ld(x+(size_t)f*ntime+c0,(size_t)ntime,Sd+c0,(size_t)ntime,1,nfeat,Tr,
                  A+(size_t)f*nfeat) ;
     for( jj=0 ; jj<(size_t)nvox*nfeat ; jj++ ){
       A[jj] += pull*(double)w_anchor[jj] ;
       if( !isfinite(A[jj]) ){ rc=SRM_ERR_NONFINITE ; goto done ; }
     }
     rc=srm_polar(A,nvox,nfeat,wrun_out[rr],wk,4242ULL+rr) ;
     if( rc!=SRM_OK ) goto done ;
     c0 += Tr ;
   }
done:
   free(Sd) ; free(A) ; free(wk) ;
   return rc ;
}

int SRM_run_deviation( const SRM_model *m , int iview , int irun , double *deviation )
{
   const float *wr ; size_t jj , n ; double acc=0.0 ;
   if( m==NULL || deviation==NULL ) return SRM_ERR_ARG ;
   if( iview<0 || iview>=m->nview || irun<0 || irun>=m->nseg ) return SRM_ERR_ARG ;
   if( m->wrun==NULL || m->w==NULL || m->w[iview]==NULL ) return SRM_ERR_MISMATCH ;
   wr=m->wrun[(size_t)iview*m->nseg+irun] ;
   if( wr==NULL ) return SRM_ERR_MISMATCH ;
   n=(size_t)m->nvox[iview]*m->nfeat ;
   for( jj=0 ; jj<n ; jj++ ){ double d=(double)wr[jj]-m->w[iview][jj] ; acc+=d*d ; }
   *deviation=acc/m->nfeat ;
   return SRM_OK ;
}

/*============================================================================
  Step 4: letting S absorb new subjects
==============================================================================*/

/*! Grow every per-view array of m by nnew views, taking ownership of the new
    W (and mu, W_ir).  Nothing in m changes unless every allocation succeeds. */
static int srm_model_append( SRM_model *m , int nnew , const int *nvox_n ,
                             float **W_n , const double *rho2_n , float **mu_n ,
                             float **Wr_n )
{
   int n=m->nview , N=n+nnew , nseg=(m->coupled) ? m->nseg : 0 , ii,rr ;
   int *nv=(int *)malloc(sizeof(int)*N) ;
   float **w=(float **)malloc(sizeof(float *)*N) ;
   double *r2=(m->rho2!=NULL) ? (double *)malloc(sizeof(double)*N) : NULL ;
   float **mu=(m->center) ? (float **)calloc(N,sizeof(float *)) : NULL ;
   float **wr=(nseg>0) ? (float **)calloc((size_t)N*nseg,sizeof(float *)) : NULL ;
   if( nv==NULL || w==NULL || (m->rho2!=NULL && r2==NULL) || (m->center && mu==NULL)
       || (nseg>0 && wr==NULL) ){
     free(nv) ; free(w) ; free(r2) ; free(mu) ; free(wr) ; return SRM_ERR_MEMORY ;
   }
   for( ii=0 ; ii<n ; ii++ ){
     nv[ii]=m->nvox[ii] ; w[ii]=m->w[ii] ;
     if( r2!=NULL ) r2[ii]=m->rho2[ii] ;
     if( mu!=NULL && m->mu!=NULL ) mu[ii]=m->mu[ii] ;
     if( m->wrun!=NULL ) for( rr=0 ; rr<nseg ; rr++ ) wr[(size_t)ii*nseg+rr]=m->wrun[(size_t)ii*nseg+rr] ;
   }
   for( ii=0 ; ii<nnew ; ii++ ){
     nv[n+ii]=nvox_n[ii] ; w[n+ii]=W_n[ii] ;
     if( r2!=NULL ) r2[n+ii]=rho2_n[ii] ;
     if( mu!=NULL ) mu[n+ii]=mu_n[ii] ;
     for( rr=0 ; rr<nseg ; rr++ ) wr[(size_t)(n+ii)*nseg+rr]=Wr_n[(size_t)ii*nseg+rr] ;
   }
   free(m->nvox) ; free(m->w) ; free(m->rho2) ; free(m->mu) ; free(m->wrun) ;
   m->nvox=nv ; m->w=w ; m->rho2=r2 ; m->mu=mu ; m->wrun=wr ; m->nview=N ;
   return SRM_OK ;
}

int SRM_update( SRM_model *m , int nnew , const int *nvox , float * const *x ,
                const SRM_params *p , SRM_fitinfo *info )
{
   int rc=SRM_OK , ii,jj,rr,it , converged=0 , vmax=0 ;
   int kk , T , prob , coupled , nseg=1 , nold ;
   double lam=0.0 , rho0_old , rho0=0.0 , xxnew=0.0 , Lprev=0.0 ;
   SRM_params dflt ; const SRM_params *q ;
   srm_view *vw=NULL ; float **Wa=NULL , **Wr=NULL , **mu=NULL ;
   double *rho2=NULL , *asum=NULL , *Aold=NULL , *Sd=NULL , *sigma_s=NULL , *post=NULL ;
   double *wk=NULL , *wa=NULL , *pen=NULL , *tau=NULL , *pull=NULL , *objective=NULL ;
   int *c0=NULL ; const int *seglen=NULL ; int one_seg[1] ;
   double s_old2=0.0 , s_diff2=0.0 ;
   int miss=0 ; double *nt=NULL , *rho0t=NULL , *rold_t=NULL , *trt=NULL , *postsum=NULL , *new_nt=NULL , *new_rt=NULL ;

   if( info!=NULL ){ info->niter_done=0 ; info->converged=0 ; info->objective=NULL ; }
   if( m==NULL || nnew<1 || nvox==NULL || x==NULL ) return SRM_ERR_ARG ;
   if( p==NULL ){ SRM_params_default(&dflt) ; q=&dflt ; } else q=p ;
   if( q->niter<1 || !(q->tol>=0.0) ) return SRM_ERR_ARG ;
   kk=m->nfeat ; T=m->ntime ; prob=(m->algo==SRM_ALGO_PROB) ; coupled=(m->coupled!=0) ;
   if( m->s==NULL || m->a_sum==NULL || m->nsubj_eff<1 || kk<1 || T<1 ) return SRM_ERR_MISMATCH ;
   if( prob && (m->rho2==NULL || m->sigma_s==NULL || !(m->rho_inv_sum>0.0)) ) return SRM_ERR_MISMATCH ;
   for( ii=0 ; ii<nnew ; ii++ ){
     if( nvox[ii]<1 || x[ii]==NULL ) return SRM_ERR_ARG ;
     if( nvox[ii]<kk ) return SRM_ERR_NFEAT ;
     if( nvox[ii]>vmax ) vmax=nvox[ii] ;
   }
   if( coupled ){
     int sum=0 ;
     lam=q->coupling ; nseg=m->nseg ; seglen=q->seglen ;
     if( !isfinite(lam) || lam<0.0 ) return SRM_ERR_ARG ;
     if( q->nseg!=nseg || seglen==NULL ) return SRM_ERR_ARG ;
     for( rr=0 ; rr<nseg ; rr++ ){ if( seglen[rr]<1 ) return SRM_ERR_ARG ; sum+=seglen[rr] ; }
     if( sum!=T ) return SRM_ERR_ARG ;
   } else {
     if( q->coupling!=SRM_COUPLING_SHARED || q->nseg>1 ) return SRM_ERR_ARG ;
     nseg=1 ; one_seg[0]=T ; seglen=one_seg ;
   }
   nold=m->nsubj_eff ; rho0_old=m->rho_inv_sum ;

   c0=(int *)srm_calloc(nseg,sizeof(int)) ;
   vw=(srm_view *)srm_calloc(nnew,sizeof(*vw)) ;
   Wa=(float **)srm_calloc(nnew,sizeof(float *)) ;
   Wr=(coupled) ? (float **)srm_calloc((size_t)nnew*nseg,sizeof(float *)) : NULL ;
   mu=(m->center) ? (float **)srm_calloc(nnew,sizeof(float *)) : NULL ;
   rho2=(double *)srm_calloc(nnew,sizeof(double)) ;
   wa=(double *)srm_calloc(nnew,sizeof(double)) ;
   pen=(double *)srm_calloc(nnew,sizeof(double)) ;
   tau=(double *)srm_calloc(nnew,sizeof(double)) ;
   pull=(double *)srm_calloc(nnew,sizeof(double)) ;
   asum=(double *)srm_calloc((size_t)kk*T,sizeof(double)) ;
   Aold=(double *)srm_calloc((size_t)kk*T,sizeof(double)) ;
   Sd=(double *)srm_calloc((size_t)kk*T,sizeof(double)) ;
   sigma_s=(double *)srm_calloc((size_t)kk*kk,sizeof(double)) ;
   post=(double *)srm_calloc((size_t)kk*kk,sizeof(double)) ;
   wk=(double *)srm_calloc((size_t)4*kk*kk,sizeof(double)) ;
   objective=(double *)srm_calloc((size_t)q->niter+1,sizeof(double)) ;
   if( !c0||!vw||!Wa||(coupled&&!Wr)||(m->center&&!mu)||!rho2||!wa||!pen||!tau||!pull||
       !asum||!Aold||!Sd||!sigma_s||!post||!wk||!objective ){ rc=SRM_ERR_MEMORY ; goto done ; }
   for( rr=1 ; rr<nseg ; rr++ ) c0[rr]=c0[rr-1]+seglen[rr-1] ;
   for( jj=0 ; jj<kk*T ; jj++ ) Aold[jj]=m->a_sum[jj] ;
   if( prob ) for( jj=0 ; jj<kk*kk ; jj++ ) sigma_s[jj]=m->sigma_s[jj] ;

   for( ii=0 ; ii<nnew ; ii++ ){                    /* data, and the start for each new view */
     size_t n=(size_t)nvox[ii]*T , jn ; const float *src=x[ii] ; double xx=0.0 ;
     const unsigned char *ob=(q->obs!=NULL) ? q->obs[ii] : NULL ; int nob=T , t ;
     vw[ii].nvox=nvox[ii] ;
     if( ob!=NULL ){
       for( nob=0,t=0 ; t<T ; t++ ) nob+=(ob[t]!=0) ;
       if( nob<kk ){ rc=SRM_ERR_ARG ; goto done ; }
       if( nob==T ) ob=NULL ; else miss=1 ;
     }
     vw[ii].obs=ob ; vw[ii].nobs=nob ;
     if( m->center || (ob!=NULL && !srm_missing_zero(src,nvox[ii],T,ob)) ){   /* centered over the observed points; missing values 0 */
       float *xc=(float *)malloc(n*sizeof(float)) ;
       if( m->center ) mu[ii]=(float *)malloc((size_t)nvox[ii]*sizeof(float)) ;
       if( xc==NULL || (m->center && mu[ii]==NULL) ){ free(xc) ; rc=SRM_ERR_MEMORY ; goto done ; }
       for( jn=0 ; jn<(size_t)nvox[ii] ; jn++ ){
         const float *r=src+jn*T ; double s=0.0 , mm=0.0 ;
         if( m->center ){
           for( t=0 ; t<T ; t++ ) if( ob==NULL || ob[t] ) s += r[t] ;
           if( !isfinite(s) ){ free(xc) ; rc=SRM_ERR_NONFINITE ; goto done ; }
           mu[ii][jn]=(float)(s/nob) ; mm=mu[ii][jn] ;
         }
         for( t=0 ; t<T ; t++ ) xc[jn*T+t] = (ob==NULL || ob[t]) ? (float)(r[t]-mm) : 0.0f ;
       }
       vw[ii].xc=xc ; src=xc ;
     }
     vw[ii].x=src ;
     for( jn=0 ; jn<n ; jn++ ) xx += (double)src[jn]*src[jn] ;
     if( !isfinite(xx) ){ rc=SRM_ERR_NONFINITE ; goto done ; }
     if( !(xx>0.0) ){ rc=SRM_ERR_SINGULAR ; goto done ; }
     vw[ii].xx=xx ; xxnew+=xx ;

     Wa[ii]=(float *)malloc((size_t)nvox[ii]*kk*sizeof(float)) ;
     if( Wa[ii]==NULL ){ rc=SRM_ERR_MEMORY ; goto done ; }
     rc=SRM_fit_view(m->s,kk,T,vw[ii].x,nvox[ii],0,Wa[ii],&rho2[ii],NULL) ;
     if( rc!=SRM_OK ) goto done ;
     if( ob!=NULL && prob ){                        /* the start's noise variance over the observed points only */
       double e=0.0 ; int c ; const float *w=Wa[ii] ;
       for( jn=0 ; jn<(size_t)nvox[ii] ; jn++ ) for( t=0 ; t<T ; t++ ) if( ob[t] ){
         double r=vw[ii].x[jn*T+t] ;
         for( c=0 ; c<kk ; c++ ) r -= (double)w[jn*kk+c]*m->s[(size_t)c*T+t] ;
         e += r*r ;
       }
       rho2[ii]=e/((double)nob*nvox[ii]) ;
       if( !(rho2[ii]>SRM_RHO2_FLOOR*xx/((double)nob*nvox[ii])) ) rho2[ii]=SRM_RHO2_FLOOR*xx/((double)nob*nvox[ii]) ;
     }
     if( !prob ) rho2[ii]=1.0 ;
     if( coupled ){
       for( rr=0 ; rr<nseg ; rr++ ){
         Wr[(size_t)ii*nseg+rr]=(float *)malloc((size_t)nvox[ii]*kk*sizeof(float)) ;
         if( Wr[(size_t)ii*nseg+rr]==NULL ){ rc=SRM_ERR_MEMORY ; goto done ; }
       }
       rc=SRM_fit_view_coupled(m->s,kk,T,vw[ii].x,nvox[ii],nseg,seglen,Wa[ii],lam,
                               &Wr[(size_t)ii*nseg]) ;
       if( rc!=SRM_OK ) goto done ;
     }
   }

   if( m->nsubj_t!=NULL ) miss=1 ;
   if( miss ){                                      /* per time point: the old sums, then the new views observed */
     int t ;
     nt=(double *)srm_calloc(T,sizeof(double)) ; rho0t=(double *)srm_calloc(T,sizeof(double)) ;
     rold_t=(double *)srm_calloc(T,sizeof(double)) ; trt=(double *)srm_calloc(T,sizeof(double)) ;
     postsum=(double *)srm_calloc((size_t)kk*kk,sizeof(double)) ;
     if( !nt || !rho0t || !rold_t || !trt || !postsum ){ rc=SRM_ERR_MEMORY ; goto done ; }
     for( t=0 ; t<T ; t++ ){
       nt[t] = (m->nsubj_t!=NULL) ? m->nsubj_t[t] : (double)nold ;
       rold_t[t] = (m->rho_inv_t!=NULL) ? m->rho_inv_t[t] : rho0_old ;
       for( ii=0 ; ii<nnew ; ii++ ) nt[t] += (vw[ii].obs==NULL || vw[ii].obs[t]) ;
     }
   }

   for( it=0 ; ; it++ ){
     double L , penalty=0.0 , *anew ; size_t nkt=(size_t)kk*T ;

     anew=(double *)srm_calloc(nkt,sizeof(double)) ;
     if( anew==NULL ){ rc=SRM_ERR_MEMORY ; goto done ; }
     if( coupled ) srm_cpass_pen(nnew,nvox,Wa,Wr,nseg,kk,pen) ;
     if( prob ){
       double logdet=0.0 , logpsi=0.0 , xxw=0.0 ;
       double *wgt=(double *)srm_calloc(nnew,sizeof(double)) ;
       if( wgt==NULL ){ free(anew) ; rc=SRM_ERR_MEMORY ; goto done ; }
       rho0=rho0_old ;
       for( ii=0 ; ii<nnew ; ii++ ){
         wgt[ii]=1.0/rho2[ii] ; rho0+=wgt[ii] ;
         logpsi += (double)nvox[ii]*log(rho2[ii])*(miss ? (double)vw[ii].nobs/T : 1.0) ;
         xxw += vw[ii].xx/rho2[ii] ;
       }
       rc = coupled ? srm_cpass_sums(nnew,vw,Wr,wgt,nseg,c0,seglen,kk,T,anew)
                    : srm_pass_sums(nnew,vw,Wa,wgt,vmax,kk,T,anew) ;
       if( rc!=SRM_OK ){ free(wgt) ; free(anew) ; goto done ; }
       for( jj=0 ; jj<(int)nkt ; jj++ ) asum[jj]=Aold[jj]+anew[jj] ;
       if( !miss ){
         rc=srm_posterior(kk,sigma_s,rho0,post,&logdet,wk) ;
         if( rc!=SRM_OK ){ free(wgt) ; free(anew) ; goto done ; }
         srm_matmul_kk(kk,T,post,asum,Sd) ;
       } else {                                     /* each t: the old views' precision there, and the new observed */
         int t ;
         for( t=0 ; t<T ; t++ ){
           rho0t[t]=rold_t[t] ;
           for( ii=0 ; ii<nnew ; ii++ ) if( vw[ii].obs==NULL || vw[ii].obs[t] ) rho0t[t]+=wgt[ii] ;
         }
         rc=srm_posterior_obs(kk,T,sigma_s,rho0t,asum,Sd,trt,postsum,&logdet,wk) ;
         if( rc!=SRM_OK ){ free(wgt) ; free(anew) ; goto done ; }
       }
       free(wgt) ;
       if( coupled && it==0 ){
         double s2=srm_dot(nkt,Sd,Sd)/((double)kk*T) ;
         for( ii=0 ; ii<nnew ; ii++ ) tau[ii]=lam*s2/rho2[ii] ;
       }
       if( coupled ) for( ii=0 ; ii<nnew ; ii++ ) penalty += tau[ii]*pen[ii] ;
       L = miss ? -0.5*(T*logpsi+logdet) - 0.5*xxw + 0.5*srm_dot(nkt,asum,Sd) + penalty
                : -0.5*T*(logpsi+logdet) - 0.5*xxw + 0.5*srm_dot(nkt,asum,Sd) + penalty ;
     } else {
       double ss=0.0 ;
       rc = coupled ? srm_cpass_sums(nnew,vw,Wr,NULL,nseg,c0,seglen,kk,T,anew)
                    : srm_pass_sums(nnew,vw,Wa,NULL,vmax,kk,T,anew) ;
       if( rc!=SRM_OK ){ free(anew) ; goto done ; }
       if( !miss ){
         for( jj=0 ; jj<(int)nkt ; jj++ ){ asum[jj]=Aold[jj]+anew[jj] ; Sd[jj]=asum[jj]/(nold+nnew) ; }
         ss=srm_dot(nkt,Sd,Sd) ;
         L = xxnew - (double)(nold+nnew)*ss ;
       } else {                                     /* s_t: the mean over the views observed at t */
         int c , t ;
         L = xxnew ;
         for( t=0 ; t<T ; t++ ){
           double qq=0.0 ;
           for( c=0 ; c<kk ; c++ ){
             size_t q2=(size_t)c*T+t ;
             asum[q2]=Aold[q2]+anew[q2] ; Sd[q2] = (nt[t]>0.0) ? asum[q2]/nt[t] : 0.0 ; qq+=Sd[q2]*Sd[q2] ;
           }
           L -= nt[t]*qq ; ss+=qq ;
         }
       }
       if( coupled && it==0 ){
         double s2=ss/((double)kk*T) ;
         for( ii=0 ; ii<nnew ; ii++ ) tau[ii]=lam*s2 ;
       }
       if( coupled ) for( ii=0 ; ii<nnew ; ii++ ) penalty += 2.0*tau[ii]*((double)nseg*kk-pen[ii]) ;
       L += penalty ;
     }
     free(anew) ;
     if( !isfinite(L) ){ rc=SRM_ERR_NONFINITE ; goto done ; }
     objective[it]=L ;
     if( q->verb ) fprintf(stderr,"SRM update iteration %d  objective %.10g\n",it,L) ;
     if( q->progress != NULL ) q->progress(it,q->niter,L,q->progress_ctx) ;
     if( it>0 && fabs(L-Lprev) <= q->tol*fabs(L) ){ converged=1 ; break ; }
     if( it >= q->niter ) break ;
     Lprev=L ;

     if( coupled ){
       for( ii=0 ; ii<nnew ; ii++ ) pull[ii] = prob ? tau[ii]*rho2[ii] : tau[ii] ;
       rc=srm_cpass_mstep(nnew,vw,Wa,Wr,pull,Sd,nseg,c0,seglen,vmax,kk,T,
                          q->seed+1000003ULL*(it+1),wa) ;
     } else {
       rc=srm_pass_procrustes(nnew,vw,Wa,Sd,vmax,kk,T,q->seed+1000003ULL*(it+1),wa) ;
     }
     if( rc!=SRM_OK ) goto done ;
     if( prob ){
       double trpost=0.0 , ss=srm_dot(nkt,Sd,Sd) ; int c,d,t ;
       for( c=0 ; c<kk ; c++ ) trpost += post[c*kk+c] ;
       if( !miss ){
         for( ii=0 ; ii<nnew ; ii++ ){
           double r=(vw[ii].xx-2.0*wa[ii]+ss+(double)T*trpost)/((double)T*nvox[ii]) ;
           double rmin=SRM_RHO2_FLOOR*vw[ii].xx/((double)T*nvox[ii]) ;
           rho2[ii]=(r>rmin) ? r : rmin ;
         }
       } else {                                     /* each new view over its own observed time points */
         double *sq=(double *)srm_calloc(T,sizeof(double)) ;
         if( sq==NULL ){ rc=SRM_ERR_MEMORY ; goto done ; }
         for( c=0 ; c<kk ; c++ ) for( t=0 ; t<T ; t++ ) sq[t] += Sd[(size_t)c*T+t]*Sd[(size_t)c*T+t] ;
         for( ii=0 ; ii<nnew ; ii++ ){
           double so=0.0 , r , rmin , den=(double)vw[ii].nobs*nvox[ii] ;
           for( t=0 ; t<T ; t++ ) if( vw[ii].obs==NULL || vw[ii].obs[t] ) so += sq[t]+trt[t] ;
           r=(vw[ii].xx-2.0*wa[ii]+so)/den ;
           rmin=SRM_RHO2_FLOOR*vw[ii].xx/den ;
           rho2[ii]=(r>rmin) ? r : rmin ;
         }
         free(sq) ;
       }
       for( c=0 ; c<kk ; c++ ) for( d=c ; d<kk ; d++ ){
         double acc=0.0 ;
         for( t=0 ; t<T ; t++ ) acc += Sd[(size_t)c*T+t]*Sd[(size_t)d*T+t] ;
         sigma_s[c*kk+d]=sigma_s[d*kk+c]=(miss ? postsum[c*kk+d]/T : post[c*kk+d])+acc/T ;
       }
     }
   }

   /* commit: nothing above has touched m */
   if( miss ){                                      /* room for the per-time sums, before anything changes */
     if( m->nsubj_t==NULL && (new_nt=(double *)srm_calloc(T,sizeof(double)))==NULL ){ rc=SRM_ERR_MEMORY ; goto done ; }
     if( prob && m->rho_inv_t==NULL && (new_rt=(double *)srm_calloc(T,sizeof(double)))==NULL ){ rc=SRM_ERR_MEMORY ; goto done ; }
   }
   for( jj=0 ; jj<kk*T ; jj++ ){ double dd=Sd[jj]-m->s[jj] ; s_diff2+=dd*dd ; s_old2+=(double)m->s[jj]*m->s[jj] ; }
   rc=srm_model_append(m,nnew,nvox,Wa,rho2,mu,Wr) ;
   if( rc!=SRM_OK ) goto done ;
   Wa=NULL ; Wr=NULL ; mu=NULL ;                    /* now owned by m */
   if( new_nt!=NULL ){ m->nsubj_t=new_nt ; new_nt=NULL ; }
   if( new_rt!=NULL ){ m->rho_inv_t=new_rt ; new_rt=NULL ; }
   for( jj=0 ; jj<kk*T ; jj++ ){ m->s[jj]=(float)Sd[jj] ; m->a_sum[jj]=asum[jj] ; }
   if( prob ){
     for( jj=0 ; jj<kk*kk ; jj++ ) m->sigma_s[jj]=(float)sigma_s[jj] ;
     m->rho_inv_sum=rho0 ;
   }
   m->nsubj_eff=nold+nnew ;
   if( miss ){                                      /* the per-time sums; allocated before this, so they cannot fail here */
     int t ;
     for( t=0 ; t<T ; t++ ) m->nsubj_t[t]=nt[t] ;
     if( prob ) for( t=0 ; t<T ; t++ ) m->rho_inv_t[t]=rho0t[t] ;
   }
   m->nupdate += 1 ;
   m->drift += (s_old2>0.0) ? s_diff2/s_old2 : 0.0 ;
   if( info!=NULL ){
     info->niter_done=it ; info->converged=converged ; info->objective=objective ; objective=NULL ;
   }
done:
   if( vw!=NULL ){ for( ii=0 ; ii<nnew ; ii++ ) free(vw[ii].xc) ; free(vw) ; }
   if( Wa!=NULL ){ for( ii=0 ; ii<nnew ; ii++ ) free(Wa[ii]) ; free(Wa) ; }
   if( Wr!=NULL ){ for( ii=0 ; ii<nnew*nseg ; ii++ ) free(Wr[ii]) ; free(Wr) ; }
   if( mu!=NULL ){ for( ii=0 ; ii<nnew ; ii++ ) free(mu[ii]) ; free(mu) ; }
   free(c0) ; free(rho2) ; free(wa) ; free(pen) ; free(tau) ; free(pull) ; free(asum) ;
   free(Aold) ; free(Sd) ; free(sigma_s) ; free(post) ; free(wk) ; free(objective) ;
   free(nt) ; free(rho0t) ; free(rold_t) ; free(trt) ; free(postsum) ; free(new_nt) ; free(new_rt) ;
   return rc ;
}

/*============================================================================
  Step 2: using a model.  Projection, shared response from held-out views,
  reconstruction, a new view's W, the gauge.
==============================================================================*/

#define SRM_CHUNK 256      /* rows per task in the single-view kernels */

/*! P (k x T, double) = W' X for ONE view, parallel over fixed chunks of rows.
    Chunk partial sums are added in chunk order, so the result is the same for
    any number of threads, with or without OpenMP. */
static int srm_wtx_par( const float *W , const float *X , int V , int k , int T ,
                        double *P )
{
   int nchunk=(V+SRM_CHUNK-1)/SRM_CHUNK , err=SRM_OK ;
   memset(P,0,sizeof(double)*(size_t)k*T) ;
SRM_PRAGMA(omp parallel)
   {
     double *loc=(double *)srm_calloc((size_t)k*T,sizeof(double)) ; int ic ;
     if( loc==NULL ){
SRM_PRAGMA(omp critical(srm_err))
       err=SRM_ERR_MEMORY ;
     }
SRM_PRAGMA(omp for schedule(dynamic) ordered)
     for( ic=0 ; ic<nchunk ; ic++ ){
       int ok = (loc!=NULL && err==SRM_OK) ;
       if( ok ){
         int f0=ic*SRM_CHUNK , nr=(V-f0<SRM_CHUNK) ? V-f0 : SRM_CHUNK ;
         memset(loc,0,sizeof(double)*(size_t)k*T) ;
         srm_wtx_acc(W+(size_t)f0*k,X+(size_t)f0*T,nr,k,T,1.0,loc) ;
       }
SRM_PRAGMA(omp ordered)
       if( ok ){
         size_t jj ;
         for( jj=0 ; jj<(size_t)k*T ; jj++ ) P[jj] += loc[jj] ;
       }
     }
     free(loc) ;
   }
   return err ;
}

/*! A (V x k, double) = X S' for one view; rows are independent. */
static void srm_xst_par( const float *X , const double *S , int V , int k , int T ,
                         double *A )
{
   int f ;
SRM_PRAGMA(omp parallel for schedule(static))
   for( f=0 ; f<V ; f++ ) srm_xst(X+(size_t)f*T,S,1,k,T,A+(size_t)f*k) ;
}

/*! P (k x Tn, double) = W' (x - mu) without forming x - mu:
    W' x  minus  (W' mu) 1'.  mu may be NULL. */
static int srm_project_dbl( const float *W , const float *mu , int V , int k ,
                            const float *x , int Tn , double *P )
{
   int rc = srm_wtx_par(W,x,V,k,Tn,P) , c,t ;
   if( rc != SRM_OK ) return rc ;
   if( mu != NULL ){
     for( c=0 ; c<k ; c++ ){
       double wmu=0.0 ; int f ;
       for( f=0 ; f<V ; f++ ) wmu += (double)W[(size_t)f*k+c]*mu[f] ;
       for( t=0 ; t<Tn ; t++ ) P[(size_t)c*Tn+t] -= wmu ;
     }
   }
   for( c=0 ; c<k*Tn ; c++ ) if( !isfinite(P[c]) ) return SRM_ERR_NONFINITE ;
   return SRM_OK ;
}

static int srm_project_w( const float *W , const float *mu , int V , int k ,
                          const float *x , int Tn , float *s_out )
{
   double *P ; int rc ; size_t jj ;
   P = (double *)srm_calloc((size_t)k*Tn,sizeof(double)) ;
   if( P==NULL ) return SRM_ERR_MEMORY ;
   rc = srm_project_dbl(W,mu,V,k,x,Tn,P) ;
   if( rc==SRM_OK ) for( jj=0 ; jj<(size_t)k*Tn ; jj++ ) s_out[jj]=(float)P[jj] ;
   free(P) ;
   return rc ;
}

int SRM_project( const SRM_model *m , int iview , const float *x , int ntime_new ,
                 float *s_out )
{
   if( m==NULL || x==NULL || s_out==NULL || ntime_new<1 ) return SRM_ERR_ARG ;
   if( iview<0 || iview>=m->nview ) return SRM_ERR_ARG ;
   if( m->w==NULL || m->w[iview]==NULL ) return SRM_ERR_MISMATCH ;
   return srm_project_w(m->w[iview],(m->mu!=NULL) ? m->mu[iview] : NULL,
                        m->nvox[iview],m->nfeat,x,ntime_new,s_out) ;
}

int SRM_project_run( const SRM_model *m , int iview , int irun , const float *x ,
                     int ntime_new , float *s_out )
{
   const float *W ;
   if( m==NULL || x==NULL || s_out==NULL || ntime_new<1 ) return SRM_ERR_ARG ;
   if( iview<0 || iview>=m->nview ) return SRM_ERR_ARG ;
   if( m->w==NULL || m->w[iview]==NULL ) return SRM_ERR_MISMATCH ;
   W = m->w[iview] ;
   if( irun>=0 && m->wrun!=NULL && irun<m->nseg && m->wrun[(size_t)iview*m->nseg+irun]!=NULL )
     W = m->wrun[(size_t)iview*m->nseg+irun] ;
   return srm_project_w(W,(m->mu!=NULL) ? m->mu[iview] : NULL,
                        m->nvox[iview],m->nfeat,x,ntime_new,s_out) ;
}

int SRM_shared_response( const SRM_model *m , int nsel , const int *iview ,
                         float * const *x , int ntime_new , int weighted ,
                         float *s_out )
{
   int k , j , rc=SRM_OK ; double *acc=NULL , *tmp=NULL ; double rho0=0.0 ;
   int use_post ;
   size_t nkt , jj ;

   if( m==NULL || iview==NULL || x==NULL || s_out==NULL || nsel<1 || ntime_new<1 )
     return SRM_ERR_ARG ;
   k = m->nfeat ; nkt=(size_t)k*ntime_new ;
   use_post = (weighted && m->algo==SRM_ALGO_PROB) ;
   if( use_post && (m->rho2==NULL || m->sigma_s==NULL) ) return SRM_ERR_MISMATCH ;
   for( j=0 ; j<nsel ; j++ ){
     if( iview[j]<0 || iview[j]>=m->nview || x[j]==NULL ) return SRM_ERR_ARG ;
     if( m->w==NULL || m->w[iview[j]]==NULL ) return SRM_ERR_MISMATCH ;
   }
   acc = (double *)srm_calloc(nkt,sizeof(double)) ;
   tmp = (double *)srm_calloc(nkt,sizeof(double)) ;
   if( acc==NULL || tmp==NULL ){ free(acc) ; free(tmp) ; return SRM_ERR_MEMORY ; }

   for( j=0 ; j<nsel ; j++ ){
     int iv=iview[j] ; double wgt ;
     rc = srm_project_dbl(m->w[iv],(m->mu!=NULL) ? m->mu[iv] : NULL,m->nvox[iv],k,
                          x[j],ntime_new,tmp) ;
     if( rc != SRM_OK ) goto done ;
     if( use_post ){ wgt = 1.0/m->rho2[iv] ; rho0 += wgt ; }
     else          { wgt = 1.0/nsel ; }
     for( jj=0 ; jj<nkt ; jj++ ) acc[jj] += wgt*tmp[jj] ;
   }
   if( use_post ){
     double *sig=(double *)srm_calloc((size_t)k*k,sizeof(double)) ;
     double *post=(double *)srm_calloc((size_t)k*k,sizeof(double)) ;
     double *wk=(double *)srm_calloc((size_t)3*k*k,sizeof(double)) ;
     if( sig==NULL || post==NULL || wk==NULL ) rc=SRM_ERR_MEMORY ;
     else {
       for( jj=0 ; jj<(size_t)k*k ; jj++ ) sig[jj]=m->sigma_s[jj] ;
       rc = srm_posterior(k,sig,rho0,post,NULL,wk) ;
       if( rc==SRM_OK ) srm_matmul_kk(k,ntime_new,post,acc,tmp) ;   /* tmp = post * acc */
     }
     free(sig) ; free(post) ; free(wk) ;
     if( rc != SRM_OK ) goto done ;
     for( jj=0 ; jj<nkt ; jj++ ) s_out[jj]=(float)tmp[jj] ;
   } else {
     for( jj=0 ; jj<nkt ; jj++ ) s_out[jj]=(float)acc[jj] ;
   }
done:
   free(acc) ; free(tmp) ;
   return rc ;
}

int SRM_shared_from_projections( const SRM_model *m , int nsel , const int *iview ,
                                 float * const *proj , int ntime_new , int weighted ,
                                 float *s_out )
{
   int k , j , rc=SRM_OK ; double *acc=NULL , *tmp=NULL ; double rho0=0.0 ; int use_post ;
   size_t nkt , jj ;
   if( m==NULL || iview==NULL || proj==NULL || s_out==NULL || nsel<1 || ntime_new<1 ) return SRM_ERR_ARG ;
   k=m->nfeat ; nkt=(size_t)k*ntime_new ;
   use_post=(weighted && m->algo==SRM_ALGO_PROB) ;
   if( use_post && (m->rho2==NULL || m->sigma_s==NULL) ) return SRM_ERR_MISMATCH ;
   for( j=0 ; j<nsel ; j++ ) if( iview[j]<0 || iview[j]>=m->nview || proj[j]==NULL ) return SRM_ERR_ARG ;
   acc=(double *)srm_calloc(nkt,sizeof(double)) ; tmp=(double *)srm_calloc(nkt,sizeof(double)) ;
   if( acc==NULL || tmp==NULL ){ free(acc) ; free(tmp) ; return SRM_ERR_MEMORY ; }
   for( j=0 ; j<nsel ; j++ ){
     double wgt ;
     if( use_post ){ wgt=1.0/m->rho2[iview[j]] ; rho0+=wgt ; } else wgt=1.0/nsel ;
     for( jj=0 ; jj<nkt ; jj++ ) acc[jj]+=wgt*(double)proj[j][jj] ;
   }
   if( use_post ){
     double *sig=(double *)srm_calloc((size_t)k*k,sizeof(double)) ;
     double *post=(double *)srm_calloc((size_t)k*k,sizeof(double)) ;
     double *wk=(double *)srm_calloc((size_t)3*k*k,sizeof(double)) ;
     if( sig==NULL || post==NULL || wk==NULL ) rc=SRM_ERR_MEMORY ;
     else {
       for( jj=0 ; jj<(size_t)k*k ; jj++ ) sig[jj]=m->sigma_s[jj] ;
       rc=srm_posterior(k,sig,rho0,post,NULL,wk) ;
       if( rc==SRM_OK ) srm_matmul_kk(k,ntime_new,post,acc,tmp) ;
     }
     free(sig) ; free(post) ; free(wk) ;
     if( rc==SRM_OK ) for( jj=0 ; jj<nkt ; jj++ ) s_out[jj]=(float)tmp[jj] ;
   } else for( jj=0 ; jj<nkt ; jj++ ) s_out[jj]=(float)acc[jj] ;
   free(acc) ; free(tmp) ;
   return rc ;
}

int SRM_shared_from_projections_obs( const SRM_model *m , int nsel , const int *iview ,
                                     float * const *proj , const unsigned char * const *obs ,
                                     int ntime_new , int weighted , float *s_out )
{
   int k , j , t , c , d , rc=SRM_OK , use_post ; double *acc=NULL , *w=NULL ;
   if( obs==NULL ) return SRM_shared_from_projections(m,nsel,iview,proj,ntime_new,weighted,s_out) ;
   if( m==NULL || iview==NULL || proj==NULL || s_out==NULL || nsel<1 || ntime_new<1 ) return SRM_ERR_ARG ;
   k=m->nfeat ;
   use_post=(weighted && m->algo==SRM_ALGO_PROB) ;
   if( use_post && (m->rho2==NULL || m->sigma_s==NULL) ) return SRM_ERR_MISMATCH ;
   for( j=0 ; j<nsel ; j++ ) if( iview[j]<0 || iview[j]>=m->nview || proj[j]==NULL ) return SRM_ERR_ARG ;
   acc=(double *)srm_calloc(k,sizeof(double)) ; w=(double *)srm_calloc((size_t)3*k*k+3*k,sizeof(double)) ;
   if( acc==NULL || w==NULL ){ free(acc) ; free(w) ; return SRM_ERR_MEMORY ; }
   if( use_post ){                                       /* Sigma_s = Q diag(ev) Q', once */
     double *a=w , *Q=w+k*k , *ev=w+2*k*k ;
     for( j=0 ; j<k*k ; j++ ) a[j]=m->sigma_s[j] ;
     if( srm_eigsym(k,a,ev,Q) ){ free(acc) ; free(w) ; return SRM_ERR_SINGULAR ; }
     for( j=0 ; j<k ; j++ ) if( ev[j]<0.0 ) ev[j]=0.0 ;
   }
   for( t=0 ; t<ntime_new ; t++ ){
     double r=0.0 ; int n=0 ;
     for( c=0 ; c<k ; c++ ) acc[c]=0.0 ;
     for( j=0 ; j<nsel ; j++ ) if( obs[j]==NULL || obs[j][t] ){
       double wg = use_post ? 1.0/m->rho2[iview[j]] : 1.0 ;
       for( c=0 ; c<k ; c++ ) acc[c]+=wg*(double)proj[j][(size_t)c*ntime_new+t] ;
       r+=wg ; n++ ;
     }
     if( n==0 ){ for( c=0 ; c<k ; c++ ) s_out[(size_t)c*ntime_new+t]=0.0f ; continue ; }
     if( use_post ){                                     /* (Sigma^-1 + r I)^-1 acc */
       double *Q=w+k*k , *ev=w+2*k*k , *b=w+3*k*k ;
       for( d=0 ; d<k ; d++ ){
         double bd=0.0 ;
         for( c=0 ; c<k ; c++ ) bd+=Q[c*k+d]*acc[c] ;
         b[d]=bd*ev[d]/(1.0+r*ev[d]) ;
       }
       for( c=0 ; c<k ; c++ ){
         double v=0.0 ;
         for( d=0 ; d<k ; d++ ) v+=Q[c*k+d]*b[d] ;
         s_out[(size_t)c*ntime_new+t]=(float)v ;
       }
     } else for( c=0 ; c<k ; c++ ) s_out[(size_t)c*ntime_new+t]=(float)(acc[c]/n) ;
   }
   free(acc) ; free(w) ;
   return rc ;
}

int SRM_reconstruct( const SRM_model *m , int iview , const float *s ,
                     int ntime_new , float *xhat )
{
   int V , k , f ; const float *W , *mu ;
   if( m==NULL || s==NULL || xhat==NULL || ntime_new<1 ) return SRM_ERR_ARG ;
   if( iview<0 || iview>=m->nview ) return SRM_ERR_ARG ;
   if( m->w==NULL || m->w[iview]==NULL ) return SRM_ERR_MISMATCH ;
   V=m->nvox[iview] ; k=m->nfeat ; W=m->w[iview] ;
   mu = (m->mu!=NULL) ? m->mu[iview] : NULL ;
SRM_PRAGMA(omp parallel)
   {
     double *row=(double *)srm_calloc(ntime_new,sizeof(double)) ;
SRM_PRAGMA(omp for schedule(static))
     for( f=0 ; f<V ; f++ ){
       int c,t ;
       if( row==NULL ) continue ;
       for( t=0 ; t<ntime_new ; t++ ) row[t] = (mu!=NULL) ? (double)mu[f] : 0.0 ;
       for( c=0 ; c<k ; c++ ){
         double w=W[(size_t)f*k+c] ; const float *sc=s+(size_t)c*ntime_new ;
         for( t=0 ; t<ntime_new ; t++ ) row[t] += w*(double)sc[t] ;
       }
       for( t=0 ; t<ntime_new ; t++ ) xhat[(size_t)f*ntime_new+t]=(float)row[t] ;
     }
     free(row) ;
   }
   return SRM_OK ;
}

int SRM_fit_view( const float *s , int nfeat , int ntime , const float *x ,
                  int nvox , int center , float *w_out , double *rho2_out ,
                  float *mu_out )
{
   double *Sd=NULL , *A=NULL , *wk=NULL , *ssum=NULL , *mu=NULL ;
   double ss=0.0 , xxc=0.0 , wa=0.0 ; int rc=SRM_OK , c,f,t ; size_t jj ;

   if( s==NULL || x==NULL || w_out==NULL || nfeat<1 || ntime<1 || nvox<1 )
     return SRM_ERR_ARG ;
   if( nfeat>ntime || nfeat>nvox ) return SRM_ERR_NFEAT ;

   Sd   = (double *)srm_calloc((size_t)nfeat*ntime,sizeof(double)) ;
   A    = (double *)srm_calloc((size_t)nvox*nfeat,sizeof(double)) ;
   wk   = (double *)srm_calloc((size_t)4*nfeat*nfeat,sizeof(double)) ;
   ssum = (double *)srm_calloc(nfeat,sizeof(double)) ;
   mu   = (double *)srm_calloc(nvox,sizeof(double)) ;
   if( !Sd||!A||!wk||!ssum||!mu ){ rc=SRM_ERR_MEMORY ; goto done ; }

   for( c=0 ; c<nfeat ; c++ ) for( t=0 ; t<ntime ; t++ ){
     double v=s[(size_t)c*ntime+t] ;
     if( !isfinite(v) ){ rc=SRM_ERR_NONFINITE ; goto done ; }
     Sd[(size_t)c*ntime+t]=v ; ssum[c]+=v ; ss+=v*v ;
   }
   for( f=0 ; f<nvox ; f++ ){                       /* means, and centered energy */
     const float *r=x+(size_t)f*ntime ; double m1=0.0 , e=0.0 ;
     if( center ){
       for( t=0 ; t<ntime ; t++ ) m1 += r[t] ;
       if( !isfinite(m1) ){ rc=SRM_ERR_NONFINITE ; goto done ; }
       m1 /= ntime ;
     }
     for( t=0 ; t<ntime ; t++ ){ double d=r[t]-m1 ; e += d*d ; }
     if( !isfinite(e) ){ rc=SRM_ERR_NONFINITE ; goto done ; }
     mu[f]=m1 ; xxc += e ;
   }
   if( !(xxc>0.0) ){ rc=SRM_ERR_SINGULAR ; goto done ; }

   srm_xst_par(x,Sd,nvox,nfeat,ntime,A) ;           /* A = X S' */
   if( center )                                     /* ... then X_c S' = X S' - mu (S 1)' */
     for( f=0 ; f<nvox ; f++ ) for( c=0 ; c<nfeat ; c++ )
       A[(size_t)f*nfeat+c] -= mu[f]*ssum[c] ;

   rc = srm_polar(A,nvox,nfeat,w_out,wk,12345ULL) ;
   if( rc != SRM_OK ) goto done ;

   for( jj=0 ; jj<(size_t)nvox*nfeat ; jj++ ) wa += (double)w_out[jj]*A[jj] ;
   if( rho2_out != NULL ){
     double r = (xxc - 2.0*wa + ss) / ((double)ntime*nvox) ;
     double rmin = SRM_RHO2_FLOOR*xxc/((double)ntime*nvox) ;
     *rho2_out = (r>rmin) ? r : rmin ;
   }
   if( mu_out != NULL ) for( f=0 ; f<nvox ; f++ ) mu_out[f]=(float)mu[f] ;
done:
   free(Sd) ; free(A) ; free(wk) ; free(ssum) ; free(mu) ;
   return rc ;
}

/*! Attach a fitted view (w, mu, rho2; ownership moves to m on success; freed on failure). */
static int srm_model_attach_view( SRM_model *m , int nvox , float *w , float *mu , double rho2 )
{
   int n=m->nview ;
   int *nv2 ; float **w2 ; double *r2=NULL ; float **mu2=NULL ; float **wr2=NULL ;

   /* grow every per-view array first; commit only if all of that succeeded */
   nv2 = (int *)realloc(m->nvox,sizeof(int)*(n+1)) ;
   if( nv2==NULL ){ free(w) ; free(mu) ; return SRM_ERR_MEMORY ; }
   m->nvox = nv2 ;
   w2 = (float **)realloc(m->w,sizeof(float *)*(n+1)) ;
   if( w2==NULL ){ free(w) ; free(mu) ; return SRM_ERR_MEMORY ; }
   m->w = w2 ;
   if( m->rho2 != NULL ){
     r2=(double *)realloc(m->rho2,sizeof(double)*(n+1)) ;
     if( r2==NULL ){ free(w) ; free(mu) ; return SRM_ERR_MEMORY ; }
     m->rho2=r2 ;
   }
   if( m->center ){
     if( m->mu==NULL ) mu2=(float **)calloc(n+1,sizeof(float *)) ;
     else              mu2=(float **)realloc(m->mu,sizeof(float *)*(n+1)) ;
     if( mu2==NULL ){ free(w) ; free(mu) ; return SRM_ERR_MEMORY ; }
     m->mu=mu2 ;
   }
   if( m->wrun != NULL ){
     int nseg = m->nseg>0 ? m->nseg : 1 ;
     wr2=(float **)realloc(m->wrun,sizeof(float *)*(size_t)(n+1)*nseg) ;
     if( wr2==NULL ){ free(w) ; free(mu) ; return SRM_ERR_MEMORY ; }
     { int q ; for( q=0 ; q<nseg ; q++ ) wr2[(size_t)n*nseg+q]=NULL ; }
     m->wrun=wr2 ;
   }
   m->nvox[n]=nvox ; m->w[n]=w ;
   if( m->rho2 != NULL ) m->rho2[n]=rho2 ;
   if( m->center ) m->mu[n]=mu ; else free(mu) ;
   m->nview = n+1 ;
   return SRM_OK ;
}

int SRM_model_add_view( SRM_model *m , const float *x , int nvox )
{
   return SRM_model_add_view_obs(m,x,nvox,NULL) ;
}

int SRM_model_add_view_obs( SRM_model *m , const float *x , int nvox , const unsigned char *obs )
{
   int rc , T , k , n=0 , t , f , c ; float *w=NULL , *mu=NULL , *xg=NULL , *sg=NULL ; double rho2=1.0 ;
   int *idx=NULL ;

   if( m==NULL || x==NULL || nvox<1 || m->s==NULL ) return SRM_ERR_ARG ;
   T=m->ntime ; k=m->nfeat ;
   w = (float *)malloc((size_t)nvox*k*sizeof(float)) ;
   if( w==NULL ) return SRM_ERR_MEMORY ;
   if( m->center ){
     mu=(float *)malloc((size_t)nvox*sizeof(float)) ;
     if( mu==NULL ){ free(w) ; return SRM_ERR_MEMORY ; }
   }
   if( obs!=NULL ){                                 /* only the observed columns, of the data and of S */
     idx=(int *)malloc(sizeof(int)*(size_t)T) ;
     if( idx==NULL ){ free(w) ; free(mu) ; return SRM_ERR_MEMORY ; }
     for( t=0 ; t<T ; t++ ) if( obs[t] ) idx[n++]=t ;
     if( n<k ){ free(idx) ; free(w) ; free(mu) ; return SRM_ERR_NFEAT ; }
     if( n==T ){ free(idx) ; idx=NULL ; }
   }
   if( idx!=NULL ){
     xg=(float *)malloc(sizeof(float)*(size_t)nvox*n) ; sg=(float *)malloc(sizeof(float)*(size_t)k*n) ;
     if( xg==NULL || sg==NULL ){ free(xg) ; free(sg) ; free(idx) ; free(w) ; free(mu) ; return SRM_ERR_MEMORY ; }
     for( f=0 ; f<nvox ; f++ ) for( t=0 ; t<n ; t++ ) xg[(size_t)f*n+t]=x[(size_t)f*T+idx[t]] ;
     for( c=0 ; c<k ; c++ ) for( t=0 ; t<n ; t++ ) sg[(size_t)c*n+t]=m->s[(size_t)c*T+idx[t]] ;
     rc = SRM_fit_view(sg,k,n,xg,nvox,m->center,w,&rho2,mu) ;
     free(xg) ; free(sg) ; free(idx) ;
   } else
     rc = SRM_fit_view(m->s,k,T,x,nvox,m->center,w,&rho2,mu) ;
   if( rc != SRM_OK ){ free(w) ; free(mu) ; return rc ; }
   rc = srm_model_attach_view(m,nvox,w,mu,rho2) ;
   return rc ;
}

/*============================================================================
  Compressed views: R (rank x T) with R'R = X'X stands in for X (nvox x T)
==============================================================================*/

/*! [X W0]'[X W0] ((T+k) x (T+k), double, both halves filled) for one view, in parallel over voxel blocks;
    k = 0 (W0 NULL) is X'X. */
static int srm_gram_of_xw( const float *x , const float *W0 , int k , int nvox , int T , double *G )
{
   int nth=srm_max_threads() , err=SRM_OK , th , nz=T+k ; size_t nn=(size_t)nz*nz , q ;
   double **part=(double **)srm_calloc(nth,sizeof(double *)) ; float dummy=0.0f ;
   if( part==NULL ) return SRM_ERR_MEMORY ;
SRM_PRAGMA(omp parallel for schedule(static))
   for( th=0 ; th<nth ; th++ ){
     int f0=(int)((long long)nvox*th/nth) , f1=(int)((long long)nvox*(th+1)/nth) , rc ;
     if( f1<=f0 ) continue ;
     part[th]=(double *)malloc(sizeof(double)*nn) ;
     if( part[th]==NULL ){
SRM_PRAGMA(omp critical(srm_err))
       err=SRM_ERR_MEMORY ;
       continue ;
     }
     rc=srm_gram_make(x+(size_t)f0*T,(k>0) ? W0+(size_t)f0*k : &dummy,f1-f0,T,k,part[th]) ;
     if( rc!=SRM_OK ){
SRM_PRAGMA(omp critical(srm_err))
       err=rc ;
     }
   }
   if( err==SRM_OK ){
     memset(G,0,sizeof(double)*nn) ;
     for( th=0 ; th<nth ; th++ ) if( part[th]!=NULL ) for( q=0 ; q<nn ; q++ ) G[q]+=part[th][q] ;
   }
   for( th=0 ; th<nth ; th++ ) free(part[th]) ;
   free(part) ;
   return err ;
}

static int srm_rrt_solve( const float *R , int rank , int T , double *B , int ncol ) ;

int SRM_compress( const float *x , int nvox , int ntime , double tol ,
                  float **R_out , int *rank_out )
{
   return SRM_compress_ex(x,nvox,ntime,tol,0,0,0,R_out,rank_out,NULL) ;
}

int SRM_compress_ex( const float *x , int nvox , int ntime , double tol , int nfeat , unsigned int seed , int stream ,
                     float **R_out , int *rank_out , float *w0_out )
{
   int T=ntime , r , i , j , q , rc , k=(w0_out!=NULL) ? nfeat : 0 , nz ; double *G , *Gz=NULL , *L , *dg , tr=0.0 , thr ;
   unsigned char *used ; float *R , *W0=NULL ;
   if( x==NULL || R_out==NULL || rank_out==NULL || nvox<1 || ntime<1 ) return SRM_ERR_ARG ;
   if( w0_out!=NULL && (nfeat<1 || nfeat>nvox) ) return SRM_ERR_ARG ;
   *R_out=NULL ; *rank_out=0 ;
   if( !(tol>0.0) ) tol=1.0e-12 ;
   nz=T+k ;
   G=(double *)malloc(sizeof(double)*(size_t)T*T) ; L=(double *)calloc((size_t)T*T,sizeof(double)) ;
   dg=(double *)malloc(sizeof(double)*(size_t)T) ; used=(unsigned char *)calloc((size_t)T,1) ;
   if( G==NULL || L==NULL || dg==NULL || used==NULL ){ free(G) ; free(L) ; free(dg) ; free(used) ; return SRM_ERR_MEMORY ; }
   if( k>0 ){                                         /* the start, and X'[X W0] in one pass over the voxels */
     W0=(float *)malloc(sizeof(float)*(size_t)nvox*k) ; Gz=(double *)malloc(sizeof(double)*(size_t)nz*nz) ;
     if( W0==NULL || Gz==NULL ){ free(W0) ; free(Gz) ; free(G) ; free(L) ; free(dg) ; free(used) ; return SRM_ERR_MEMORY ; }
     rc=srm_random_orthonormal(W0,nvox,k,seed,stream) ;
     if( rc==SRM_OK ) rc=srm_gram_of_xw(x,W0,k,nvox,T,Gz) ;
     free(W0) ;
     if( rc==SRM_OK ) for( i=0 ; i<T ; i++ ) memcpy(G+(size_t)i*T,Gz+(size_t)i*nz,sizeof(double)*(size_t)T) ;
   } else rc=srm_gram_of_xw(x,NULL,0,nvox,T,G) ;
   if( rc!=SRM_OK ){ free(Gz) ; free(G) ; free(L) ; free(dg) ; free(used) ; return rc ; }
   for( i=0 ; i<T ; i++ ){ dg[i]=G[(size_t)i*T+i] ; tr+=dg[i] ; }
   if( !isfinite(tr) ){ free(Gz) ; free(G) ; free(L) ; free(dg) ; free(used) ; return SRM_ERR_NONFINITE ; }
   if( !(tr>0.0) ){ free(Gz) ; free(G) ; free(L) ; free(dg) ; free(used) ; return SRM_ERR_SINGULAR ; }
   thr=tol*tr ;
   /* pivoted Cholesky, L[i*T+r] = column r of the factor: G ~ L L' over the original indices */
   for( r=0 ; r<T ; r++ ){
     int jm=-1 ; double dm=0.0 , lrr ;
     for( i=0 ; i<T ; i++ ) if( !used[i] && dg[i]>dm ){ dm=dg[i] ; jm=i ; }
     if( jm<0 || dm<=thr ) break ;
     j=jm ; used[j]=1 ; lrr=sqrt(dm) ; L[(size_t)j*T+r]=lrr ;
SRM_PRAGMA(omp parallel for schedule(static))
     for( i=0 ; i<T ; i++ ){
       double a ; int qq ;
       if( used[i] ) continue ;
       a=G[(size_t)i*T+j] ;
       for( qq=0 ; qq<r ; qq++ ) a-=L[(size_t)i*T+qq]*L[(size_t)j*T+qq] ;
       L[(size_t)i*T+r]=a/lrr ;
       dg[i]-=L[(size_t)i*T+r]*L[(size_t)i*T+r] ;
     }
   }
   if( r<1 ){ free(Gz) ; free(G) ; free(L) ; free(dg) ; free(used) ; return SRM_ERR_SINGULAR ; }
   R=(float *)malloc(sizeof(float)*(size_t)r*T) ;
   if( R==NULL ){ free(Gz) ; free(G) ; free(L) ; free(dg) ; free(used) ; return SRM_ERR_MEMORY ; }
   for( q=0 ; q<r ; q++ ) for( i=0 ; i<T ; i++ ) R[(size_t)q*T+i]=(float)L[(size_t)i*T+q] ;
   free(G) ; free(L) ; free(dg) ; free(used) ;
   if( k>0 ){                                         /* w0 = U'W0 = (R R')^-1 R (X'W0) */
     double *RB=(double *)calloc((size_t)r*k,sizeof(double)) ; int a , c , t ;
     if( RB==NULL ){ free(Gz) ; free(R) ; return SRM_ERR_MEMORY ; }
     for( a=0 ; a<r ; a++ ) for( c=0 ; c<k ; c++ ){
       double acc=0.0 ;
       for( t=0 ; t<T ; t++ ) acc+=(double)R[(size_t)a*T+t]*Gz[(size_t)t*nz+T+c] ;
       RB[(size_t)a*k+c]=acc ;
     }
     rc=srm_rrt_solve(R,r,T,RB,k) ;
     if( rc==SRM_OK ) for( a=0 ; a<r*k ; a++ ) w0_out[a]=(float)RB[a] ;
     free(RB) ; free(Gz) ;
     if( rc!=SRM_OK ){ free(R) ; return rc ; }
   }
   *R_out=R ; *rank_out=r ;
   return SRM_OK ;
}

/*! Solve (R R') Y = B in place (B: rank x ncol, double), R rank x T float.  0 if ok. */
static int srm_rrt_solve( const float *R , int rank , int T , double *B , int ncol )
{
   double *A=(double *)malloc(sizeof(double)*(size_t)rank*rank) ; int a , b , t , c ;
   if( A==NULL ) return SRM_ERR_MEMORY ;
SRM_PRAGMA(omp parallel for schedule(dynamic) private(b,t))
   for( a=0 ; a<rank ; a++ ) for( b=0 ; b<=a ; b++ ){
     double acc=0.0 ;
     for( t=0 ; t<T ; t++ ) acc+=(double)R[(size_t)a*T+t]*R[(size_t)b*T+t] ;
     A[(size_t)a*rank+b]=A[(size_t)b*rank+a]=acc ;
   }
   for( a=0 ; a<rank ; a++ ){                          /* Cholesky, lower, in place */
     double d=A[(size_t)a*rank+a] ;
     for( t=0 ; t<a ; t++ ) d-=A[(size_t)a*rank+t]*A[(size_t)a*rank+t] ;
     if( !(d>0.0) ){ free(A) ; return SRM_ERR_SINGULAR ; }
     d=sqrt(d) ; A[(size_t)a*rank+a]=d ;
     for( b=a+1 ; b<rank ; b++ ){
       double e=A[(size_t)b*rank+a] ;
       for( t=0 ; t<a ; t++ ) e-=A[(size_t)b*rank+t]*A[(size_t)a*rank+t] ;
       A[(size_t)b*rank+a]=e/d ;
     }
   }
   for( c=0 ; c<ncol ; c++ ){                          /* L y = b, then L' x = y */
     for( a=0 ; a<rank ; a++ ){
       double e=B[(size_t)a*ncol+c] ;
       for( t=0 ; t<a ; t++ ) e-=A[(size_t)a*rank+t]*B[(size_t)t*ncol+c] ;
       B[(size_t)a*ncol+c]=e/A[(size_t)a*rank+a] ;
     }
     for( a=rank-1 ; a>=0 ; a-- ){
       double e=B[(size_t)a*ncol+c] ;
       for( t=a+1 ; t<rank ; t++ ) e-=A[(size_t)t*rank+a]*B[(size_t)t*ncol+c] ;
       B[(size_t)a*ncol+c]=e/A[(size_t)a*rank+a] ;
     }
   }
   free(A) ;
   return SRM_OK ;
}

int SRM_compress_start( const float *x , int nvox , int ntime , const float *R , int rank ,
                        int nfeat , unsigned int seed , int stream , float *w0_out )
{
   int T=ntime , k=nfeat , nth=srm_max_threads() , th , err=SRM_OK , a , c , t ;
   float *W0 ; double *B , *RB , **part ;
   if( x==NULL || R==NULL || w0_out==NULL || nvox<k || rank<1 || k<1 ) return SRM_ERR_ARG ;
   W0=(float *)malloc(sizeof(float)*(size_t)nvox*k) ; B=(double *)calloc((size_t)T*k,sizeof(double)) ;
   RB=(double *)calloc((size_t)rank*k,sizeof(double)) ; part=(double **)srm_calloc(nth,sizeof(double *)) ;
   if( W0==NULL || B==NULL || RB==NULL || part==NULL ){ free(W0) ; free(B) ; free(RB) ; free(part) ; return SRM_ERR_MEMORY ; }
   err=srm_random_orthonormal(W0,nvox,k,seed,stream) ;     /* exactly SRM_fit's random start */
SRM_PRAGMA(omp parallel for schedule(static))
   for( th=0 ; th<nth ; th++ ){                             /* B = X' W0, over voxel blocks */
     int f0=(int)((long long)nvox*th/nth) , f1=(int)((long long)nvox*(th+1)/nth) , f , tt , cc ;
     double *P ;
     if( err!=SRM_OK || f1<=f0 ) continue ;
     P=(double *)calloc((size_t)T*k,sizeof(double)) ;
     if( P==NULL ){
SRM_PRAGMA(omp critical(srm_err))
       err=SRM_ERR_MEMORY ;
       continue ;
     }
     for( f=f0 ; f<f1 ; f++ ){
       const float *xr=x+(size_t)f*T , *wr=W0+(size_t)f*k ;
       for( tt=0 ; tt<T ; tt++ ){ double v=xr[tt] ; if( v!=0.0 ) for( cc=0 ; cc<k ; cc++ ) P[(size_t)tt*k+cc]+=v*wr[cc] ; }
     }
     part[th]=P ;
   }
   if( err==SRM_OK ) for( th=0 ; th<nth ; th++ ) if( part[th]!=NULL ){ size_t q ; for( q=0 ; q<(size_t)T*k ; q++ ) B[q]+=part[th][q] ; }
   for( th=0 ; th<nth ; th++ ) free(part[th]) ;
   free(part) ; free(W0) ;
   if( err!=SRM_OK ){ free(B) ; free(RB) ; return err ; }
   for( a=0 ; a<rank ; a++ ) for( c=0 ; c<k ; c++ ){           /* R B */
     double acc=0.0 ;
     for( t=0 ; t<T ; t++ ) acc+=(double)R[(size_t)a*T+t]*B[(size_t)t*k+c] ;
     RB[(size_t)a*k+c]=acc ;
   }
   err=srm_rrt_solve(R,rank,T,RB,k) ;                          /* U' W0 = (R R')^-1 R X' W0 */
   if( err==SRM_OK ) for( a=0 ; a<rank*k ; a++ ) w0_out[a]=(float)RB[a] ;
   free(B) ; free(RB) ;
   return err ;
}

int SRM_compress_map( const float *R , int rank , int ntime , const float *v , int ncol , double *M_out )
{
   int a , c , t , rc ; double *Y ;
   if( R==NULL || v==NULL || M_out==NULL || rank<1 || ncol<1 ) return SRM_ERR_ARG ;
   Y=(double *)malloc(sizeof(double)*(size_t)rank*ncol) ;
   if( Y==NULL ) return SRM_ERR_MEMORY ;
   for( a=0 ; a<rank*ncol ; a++ ) Y[a]=v[a] ;
   rc=srm_rrt_solve(R,rank,ntime,Y,ncol) ;
   if( rc==SRM_OK ) for( t=0 ; t<ntime ; t++ ) for( c=0 ; c<ncol ; c++ ){
     double acc=0.0 ;
     for( a=0 ; a<rank ; a++ ) acc+=(double)R[(size_t)a*ntime+t]*Y[(size_t)a*ncol+c] ;
     M_out[(size_t)t*ncol+c]=acc ;
   }
   free(Y) ;
   return rc ;
}

int SRM_compress_expand( const float *x , int nvox , int ntime , const double *M , int ncol , float *out )
{
   int f ;
   if( x==NULL || M==NULL || out==NULL || nvox<1 || ntime<1 || ncol<1 ) return SRM_ERR_ARG ;
SRM_PRAGMA(omp parallel for schedule(static))
   for( f=0 ; f<nvox ; f++ ){
     const float *xr=x+(size_t)f*ntime ; int t , c ; double acc[64] ;
     int c0 ;
     for( c0=0 ; c0<ncol ; c0+=64 ){                    /* 64 columns at a time, accumulated in double */
       int nc=(ncol-c0<64) ? ncol-c0 : 64 ;
       for( c=0 ; c<nc ; c++ ) acc[c]=0.0 ;
       for( t=0 ; t<ntime ; t++ ){
         double v=xr[t] ; const double *mr=M+(size_t)t*ncol+c0 ;
         if( v==0.0 ) continue ;
         for( c=0 ; c<nc ; c++ ) acc[c]+=v*mr[c] ;
       }
       for( c=0 ; c<nc ; c++ ) out[(size_t)f*ncol+c0+c]=(float)acc[c] ;
     }
   }
   return SRM_OK ;
}

SRM_model * SRM_model_from_w( int nvox , int nfeat , const float *w , const float *mu )
{
   SRM_model *m ;
   if( nvox<1 || nfeat<1 || w==NULL ) return NULL ;
   m = (SRM_model *)srm_calloc(1,sizeof(*m)) ;
   if( m==NULL ) return NULL ;
   m->nview=1 ; m->nfeat=nfeat ; m->nseg=1 ; m->coupling=SRM_COUPLING_SHARED ;
   m->nvox=(int *)malloc(sizeof(int)) ;
   m->w=(float **)calloc(1,sizeof(float *)) ;
   if( m->nvox==NULL || m->w==NULL ){ SRM_free_model(m) ; return NULL ; }
   m->nvox[0]=nvox ;
   m->w[0]=(float *)malloc((size_t)nvox*nfeat*sizeof(float)) ;
   if( m->w[0]==NULL ){ SRM_free_model(m) ; return NULL ; }
   memcpy(m->w[0],w,(size_t)nvox*nfeat*sizeof(float)) ;
   if( mu != NULL ){
     m->mu=(float **)calloc(1,sizeof(float *)) ;
     if( m->mu!=NULL ) m->mu[0]=(float *)malloc((size_t)nvox*sizeof(float)) ;
     if( m->mu==NULL || m->mu[0]==NULL ){ SRM_free_model(m) ; return NULL ; }
     memcpy(m->mu[0],mu,(size_t)nvox*sizeof(float)) ;
     m->center=1 ;
   }
   return m ;
}

/* V <- V Q for a nvox x k float matrix, Q k x k double */
static void srm_rotate_rows( float *V , int nvox , int k , const double *Q )
{
   int f ;
SRM_PRAGMA(omp parallel for schedule(static))
   for( f=0 ; f<nvox ; f++ ){
     double tmp[1024] , *row=tmp ; int c,d ; int heap=0 ;
     float *r=V+(size_t)f*k ;
     if( k>1024 ){ row=(double *)malloc(sizeof(double)*k) ; heap=1 ; if( row==NULL ) continue ; }
     for( c=0 ; c<k ; c++ ){
       double acc=0.0 ;
       for( d=0 ; d<k ; d++ ) acc += (double)r[d]*Q[(size_t)d*k+c] ;
       row[c]=acc ;
     }
     for( c=0 ; c<k ; c++ ) r[c]=(float)row[c] ;
     if( heap ) free(row) ;
   }
}

int SRM_canonicalize( SRM_model *m )
{
   int k , T , c , d , t , ii , jj , rc=SRM_OK ;
   double *Sd=NULL , *C=NULL , *Q=NULL , *ev=NULL , *Sn=NULL , *tmp=NULL ;

   if( m==NULL || m->s==NULL ) return SRM_ERR_ARG ;
   if( m->nupdate > 0 ) return SRM_ERR_ARG ;          /* would move the gauge of earlier outputs */
   k=m->nfeat ; T=m->ntime ;
   Sd=(double *)srm_calloc((size_t)k*T,sizeof(double)) ;
   Sn=(double *)srm_calloc((size_t)k*T,sizeof(double)) ;
   C =(double *)srm_calloc((size_t)k*k,sizeof(double)) ;
   Q =(double *)srm_calloc((size_t)k*k,sizeof(double)) ;
   ev=(double *)srm_calloc(k,sizeof(double)) ;
   tmp=(double *)srm_calloc((size_t)k*k,sizeof(double)) ;
   if( !Sd||!Sn||!C||!Q||!ev||!tmp ){ rc=SRM_ERR_MEMORY ; goto done ; }

   for( c=0 ; c<k*T ; c++ ) Sd[c]=m->s[c] ;
   for( c=0 ; c<k ; c++ ) for( d=c ; d<k ; d++ ){          /* C = S S' */
     double acc=0.0 ;
     for( t=0 ; t<T ; t++ ) acc += Sd[(size_t)c*T+t]*Sd[(size_t)d*T+t] ;
     C[c*k+d]=C[d*k+c]=acc ;
   }
   if( srm_eigsym(k,C,ev,Q) ){ rc=SRM_ERR_SINGULAR ; goto done ; }   /* Q: columns, descending */

   /* signs, decided on S' = Q' S */
   for( c=0 ; c<k ; c++ ){
     double m2=0.0 , m3=0.0 , pk=0.0 ; int flip ;
     for( t=0 ; t<T ; t++ ){
       double v=0.0 ;
       for( d=0 ; d<k ; d++ ) v += Q[d*k+c]*Sd[(size_t)d*T+t];
       m2 += v*v ; m3 += v*v*v ;
       if( fabs(v) > fabs(pk) ) pk=v ;
     }
     if( fabs(m3) > 1.0e-10*pow(m2>0.0?m2:1.0,1.5) ) flip = (m3<0.0) ;
     else                                              flip = (pk<0.0) ;
     if( flip ) for( d=0 ; d<k ; d++ ) Q[d*k+c] = -Q[d*k+c] ;
   }

   for( c=0 ; c<k ; c++ ) for( t=0 ; t<T ; t++ ){            /* S' = Q' S */
     double acc=0.0 ;
     for( d=0 ; d<k ; d++ ) acc += Q[d*k+c]*Sd[(size_t)d*T+t] ;
     Sn[(size_t)c*T+t]=acc ;
   }
   for( ii=0 ; ii<m->nview ; ii++ ){                         /* W' = W Q */
     if( m->w!=NULL && m->w[ii]!=NULL ) srm_rotate_rows(m->w[ii],m->nvox[ii],k,Q) ;
     if( m->wrun!=NULL ) for( jj=0 ; jj<m->nseg ; jj++ ){
       float *wr=m->wrun[(size_t)ii*m->nseg+jj] ;
       if( wr!=NULL ) srm_rotate_rows(wr,m->nvox[ii],k,Q) ;
     }
   }
   if( m->sigma_s != NULL ){                                 /* Sigma' = Q' Sigma Q */
     for( c=0 ; c<k ; c++ ) for( d=0 ; d<k ; d++ ){
       double acc=0.0 ; int p,q ;
       for( p=0 ; p<k ; p++ ) for( q=0 ; q<k ; q++ )
         acc += Q[p*k+c]*(double)m->sigma_s[p*k+q]*Q[q*k+d] ;
       tmp[c*k+d]=acc ;
     }
     for( c=0 ; c<k*k ; c++ ) m->sigma_s[c]=(float)tmp[c] ;
   }
   if( m->a_sum != NULL ){                                   /* A' = Q' A */
     double *An=(double *)srm_calloc((size_t)k*T,sizeof(double)) ;
     if( An==NULL ){ rc=SRM_ERR_MEMORY ; goto done ; }
     for( c=0 ; c<k ; c++ ) for( t=0 ; t<T ; t++ ){
       double acc=0.0 ;
       for( d=0 ; d<k ; d++ ) acc += Q[d*k+c]*m->a_sum[(size_t)d*T+t] ;
       An[(size_t)c*T+t]=acc ;
     }
     memcpy(m->a_sum,An,sizeof(double)*(size_t)k*T) ; free(An) ;
   }
   for( c=0 ; c<k*T ; c++ ) m->s[c]=(float)Sn[c] ;
done:
   free(Sd) ; free(Sn) ; free(C) ; free(Q) ; free(ev) ; free(tmp) ;
   return rc ;
}

int SRM_procrustes_rotation( const float *a , const float *b , int nfeat , int ntime ,
                             float *r_out )
{
   double *M=NULL , *wk=NULL ; int c,d,t , rc ;
   if( a==NULL || b==NULL || r_out==NULL || nfeat<1 || ntime<1 ) return SRM_ERR_ARG ;
   M =(double *)srm_calloc((size_t)nfeat*nfeat,sizeof(double)) ;
   wk=(double *)srm_calloc((size_t)4*nfeat*nfeat,sizeof(double)) ;
   if( M==NULL || wk==NULL ){ free(M) ; free(wk) ; return SRM_ERR_MEMORY ; }
   for( c=0 ; c<nfeat ; c++ ) for( d=0 ; d<nfeat ; d++ ){      /* M = b a' */
     double acc=0.0 ;
     for( t=0 ; t<ntime ; t++ ) acc += (double)b[(size_t)c*ntime+t]*a[(size_t)d*ntime+t] ;
     M[(size_t)c*nfeat+d]=acc ;
   }
   rc = srm_polar(M,nfeat,nfeat,r_out,wk,777ULL) ;           /* R = U V' of M */
   free(M) ; free(wk) ;
   return rc ;
}

/*============================================================================
  Step 5: held-out metrics, reliability, preprocessing
==============================================================================*/

/*! Pearson correlation of two double vectors of length n; 0 if either has no
    variance.  *valid is set to 0 in that case, 1 otherwise (valid may be NULL). */
static double srm_pearson( const double *a , const double *b , int n , int *valid )
{
   double ma=0.0 , mb=0.0 , saa=0.0 , sbb=0.0 , sab=0.0 ; int i ;
   for( i=0 ; i<n ; i++ ){ ma+=a[i] ; mb+=b[i] ; }
   ma/=n ; mb/=n ;
   for( i=0 ; i<n ; i++ ){
     double da=a[i]-ma , db=b[i]-mb ;
     saa+=da*da ; sbb+=db*db ; sab+=da*db ;
   }
   if( !(saa>0.0) || !(sbb>0.0) ){ if( valid!=NULL ) *valid=0 ; return 0.0 ; }
   if( valid!=NULL ) *valid=1 ;
   return sab/sqrt(saa*sbb) ;
}

static double srm_fisher_mean( const double *r , const int *valid , int n )
{
   double z=0.0 ; int i , cnt=0 ;
   for( i=0 ; i<n ; i++ ) if( valid==NULL || valid[i] ){
     double v=r[i] ;
     if( v>0.9999999 ) v=0.9999999 ;
     if( v<-0.9999999 ) v=-0.9999999 ;
     z += atanh(v) ; cnt++ ;
   }
   return (cnt>0) ? tanh(z/cnt) : 0.0 ;
}

int SRM_reconstruction_r2( const SRM_model *m , int iview , const float *x ,
                           const float *s , int ntime_new , double *r2 ,
                           float *vox_r2 )
{
   int V , k , f , rc=SRM_OK ; const float *W , *mu ;
   double *sse=NULL , *sst=NULL , tot_sse=0.0 , tot_sst=0.0 ;
   if( m==NULL || x==NULL || s==NULL || r2==NULL || ntime_new<1 ) return SRM_ERR_ARG ;
   if( iview<0 || iview>=m->nview ) return SRM_ERR_ARG ;
   if( m->w==NULL || m->w[iview]==NULL ) return SRM_ERR_MISMATCH ;
   V=m->nvox[iview] ; k=m->nfeat ; W=m->w[iview] ;
   mu=(m->mu!=NULL) ? m->mu[iview] : NULL ;
   sse=(double *)srm_calloc(V,sizeof(double)) ; sst=(double *)srm_calloc(V,sizeof(double)) ;
   if( sse==NULL || sst==NULL ){ free(sse) ; free(sst) ; return SRM_ERR_MEMORY ; }
SRM_PRAGMA(omp parallel)
   {
     double *row=(double *)srm_calloc(ntime_new,sizeof(double)) ;
SRM_PRAGMA(omp for schedule(static))
     for( f=0 ; f<V ; f++ ){
       int c,t ; double mean=0.0 , a=0.0 , b=0.0 ;
       const float *xr=x+(size_t)f*ntime_new ;
       if( row==NULL ) continue ;
       for( t=0 ; t<ntime_new ; t++ ) row[t]=(mu!=NULL) ? (double)mu[f] : 0.0 ;
       for( c=0 ; c<k ; c++ ){
         double w=W[(size_t)f*k+c] ; const float *sc=s+(size_t)c*ntime_new ;
         for( t=0 ; t<ntime_new ; t++ ) row[t] += w*(double)sc[t] ;
       }
       for( t=0 ; t<ntime_new ; t++ ) mean += xr[t] ;
       mean /= ntime_new ;
       for( t=0 ; t<ntime_new ; t++ ){
         double e=(double)xr[t]-row[t] , d=(double)xr[t]-mean ;
         a += e*e ; b += d*d ;
       }
       sse[f]=a ; sst[f]=b ;
     }
     free(row) ;
   }
   for( f=0 ; f<V ; f++ ){ tot_sse+=sse[f] ; tot_sst+=sst[f] ; }
   if( !isfinite(tot_sse) || !isfinite(tot_sst) ) rc=SRM_ERR_NONFINITE ;
   else if( !(tot_sst>0.0) ) rc=SRM_ERR_SINGULAR ;
   else {
     *r2 = 1.0 - tot_sse/tot_sst ;
     if( vox_r2 != NULL )
       for( f=0 ; f<V ; f++ ) vox_r2[f]=(sst[f]>0.0) ? (float)(1.0-sse[f]/sst[f]) : 0.0f ;
   }
   free(sse) ; free(sst) ;
   return rc ;
}

int SRM_segment_match( const float *a , const float *b , int nfeat , int ntime ,
                       int win , double *accuracy , double *chance )
{
   return SRM_segment_match_obs(a,b,nfeat,ntime,win,NULL,accuracy,chance) ;
}

int SRM_segment_match_obs( const float *a , const float *b , int nfeat , int ntime ,
                           int win , const unsigned char *valid ,
                           double *accuracy , double *chance )
{
   int nseg , len , i , correct=0 , nq=0 ;
   double *Za=NULL , *Zb=NULL , *chs=NULL ; int *hit=NULL ; double chsum=0.0 ; unsigned char *ok=NULL ;
   if( a==NULL || b==NULL || accuracy==NULL || chance==NULL ) return SRM_ERR_ARG ;
   if( nfeat<1 || win<1 || ntime<=win ) return SRM_ERR_ARG ;
   nseg=ntime-win ;
   if( nseg<=win ) return SRM_ERR_ARG ;                  /* no non-overlapping candidates */
   len=nfeat*win ;
   ok=(unsigned char *)srm_calloc(nseg,1) ;            /* a window is usable if all of it is valid */
   if( ok==NULL ) return SRM_ERR_MEMORY ;
   for( i=0 ; i<nseg ; i++ ){
     int ww ; ok[i]=1 ;
     if( valid!=NULL ) for( ww=0 ; ww<win ; ww++ ) if( !valid[i+ww] ){ ok[i]=0 ; break ; }
   }
   Za=(double *)srm_calloc((size_t)nseg*len,sizeof(double)) ;
   Zb=(double *)srm_calloc((size_t)nseg*len,sizeof(double)) ;
   chs=(double *)srm_calloc(nseg,sizeof(double)) ;
   hit=(int *)srm_calloc(nseg,sizeof(int)) ;
   if( !Za||!Zb||!chs||!hit ){ free(Za) ; free(Zb) ; free(chs) ; free(hit) ; free(ok) ; return SRM_ERR_MEMORY ; }

SRM_PRAGMA(omp parallel for schedule(static))
   for( i=0 ; i<nseg ; i++ ){                    /* each window, centered and unit length */
     int src ;
     for( src=0 ; src<2 ; src++ ){
       const float *x = src ? b : a ; double *z = (src ? Zb : Za) + (size_t)i*len ;
       double mean=0.0 , nrm=0.0 ; int q=0 , dd , ww ;
       for( dd=0 ; dd<nfeat ; dd++ ) for( ww=0 ; ww<win ; ww++ ) z[q++]=x[(size_t)dd*ntime+i+ww] ;
       for( q=0 ; q<len ; q++ ) mean+=z[q] ;
       mean/=len ;
       for( q=0 ; q<len ; q++ ){ z[q]-=mean ; nrm+=z[q]*z[q] ; }
       nrm=sqrt(nrm) ;
       for( q=0 ; q<len ; q++ ) z[q]=(nrm>0.0) ? z[q]/nrm : 0.0 ;
     }
   }
SRM_PRAGMA(omp parallel for schedule(dynamic))
   for( i=0 ; i<nseg ; i++ ){
     double best=-HUGE_VAL ; int bestj=-1 , ncand=0 , jj ;
     const double *za=Za+(size_t)i*len ;
     if( !ok[i] ){ hit[i]=0 ; chs[i]=0.0 ; continue ; }
     for( jj=0 ; jj<nseg ; jj++ ){
       const double *zb ; double c=0.0 ; int q ;
       if( jj!=i && abs(i-jj)<win ) continue ;   /* overlapping windows are not candidates */
       if( !ok[jj] ) continue ;
       ncand++ ;
       zb=Zb+(size_t)jj*len ;
       for( q=0 ; q<len ; q++ ) c += za[q]*zb[q] ;
       if( c>best ){ best=c ; bestj=jj ; }
     }
     hit[i]=(bestj==i) ; chs[i]=1.0/ncand ;
   }
   for( i=0 ; i<nseg ; i++ ) if( ok[i] ){ correct+=hit[i] ; chsum+=chs[i] ; nq++ ; }
   free(Za) ; free(Zb) ; free(chs) ; free(hit) ; free(ok) ;
   if( nq<1 ) return SRM_ERR_ARG ;                       /* no window without a missing sample */
   *accuracy=(double)correct/nq ; *chance=chsum/nq ;
   return SRM_OK ;
}

int SRM_item_identify( const float *a , const float *b , int nfeat , int nitem ,
                       double *accuracy , double *chance )
{
   double *A=NULL , *B=NULL ; int *hit=NULL ; int i , j , correct=0 ;
   if( a==NULL || b==NULL || accuracy==NULL || chance==NULL ) return SRM_ERR_ARG ;
   if( nfeat<2 || nitem<2 ) return SRM_ERR_ARG ;
   A=(double *)srm_calloc((size_t)nitem*nfeat,sizeof(double)) ;
   B=(double *)srm_calloc((size_t)nitem*nfeat,sizeof(double)) ;
   hit=(int *)srm_calloc(nitem,sizeof(int)) ;
   if( !A||!B||!hit ){ free(A) ; free(B) ; free(hit) ; return SRM_ERR_MEMORY ; }
   for( i=0 ; i<nitem ; i++ ){                          /* item i, centered over components, unit length */
     int src , c ;
     for( src=0 ; src<2 ; src++ ){
       const float *x=src ? b : a ; double *z=(src ? B : A)+(size_t)i*nfeat ;
       double mean=0.0 , nrm=0.0 ;
       for( c=0 ; c<nfeat ; c++ ){ z[c]=x[(size_t)c*nitem+i] ; mean+=z[c] ; }
       mean/=nfeat ;
       for( c=0 ; c<nfeat ; c++ ){ z[c]-=mean ; nrm+=z[c]*z[c] ; }
       nrm=sqrt(nrm) ;
       for( c=0 ; c<nfeat ; c++ ) z[c]=(nrm>0.0) ? z[c]/nrm : 0.0 ;
     }
   }
SRM_PRAGMA(omp parallel for schedule(static))
   for( i=0 ; i<nitem ; i++ ){
     double best=-HUGE_VAL ; int bestj=-1 , jj , c ;
     for( jj=0 ; jj<nitem ; jj++ ){
       double v=0.0 ;
       for( c=0 ; c<nfeat ; c++ ) v += A[(size_t)i*nfeat+c]*B[(size_t)jj*nfeat+c] ;
       if( v>best ){ best=v ; bestj=jj ; }
     }
     hit[i]=(bestj==i) ;
   }
   for( j=0 ; j<nitem ; j++ ) correct+=hit[j] ;
   *accuracy=(double)correct/nitem ; *chance=1.0/nitem ;
   free(A) ; free(B) ; free(hit) ;
   return SRM_OK ;
}

int SRM_component_corr( const float *a , const float *b , int nfeat , int ntime ,
                        float *r_out , double *r_mean )
{
   double *ra=NULL , *rb=NULL , *r=NULL ; int *valid=NULL ; int c , t ;
   if( a==NULL || b==NULL || nfeat<1 || ntime<2 ) return SRM_ERR_ARG ;
   ra=(double *)srm_calloc(ntime,sizeof(double)) ; rb=(double *)srm_calloc(ntime,sizeof(double)) ;
   r=(double *)srm_calloc(nfeat,sizeof(double)) ; valid=(int *)srm_calloc(nfeat,sizeof(int)) ;
   if( !ra||!rb||!r||!valid ){ free(ra) ; free(rb) ; free(r) ; free(valid) ; return SRM_ERR_MEMORY ; }
   for( c=0 ; c<nfeat ; c++ ){
     for( t=0 ; t<ntime ; t++ ){ ra[t]=a[(size_t)c*ntime+t] ; rb[t]=b[(size_t)c*ntime+t] ; }
     r[c]=srm_pearson(ra,rb,ntime,&valid[c]) ;
     if( r_out!=NULL ) r_out[c]=(float)r[c] ;
   }
   if( r_mean!=NULL ) *r_mean=srm_fisher_mean(r,NULL,nfeat) ;
   free(ra) ; free(rb) ; free(r) ; free(valid) ;
   return SRM_OK ;
}

/*! G^(-1/2) for a symmetric positive definite k x k matrix; SRM_ERR_SINGULAR if
    it is numerically rank deficient. */
static int srm_inv_sqrt( int k , const double *G , double *out , double *wk )
{
   double *a=wk , *Q=wk+k*k , *ev=wk+2*k*k ; int c,d,j ;
   memcpy(a,G,sizeof(double)*(size_t)k*k) ;
   if( srm_eigsym(k,a,ev,Q) ) return SRM_ERR_SINGULAR ;
   if( !(ev[0]>0.0) || !(ev[k-1] > 1.0e-12*ev[0]) ) return SRM_ERR_SINGULAR ;
   for( c=0 ; c<k ; c++ ) for( d=0 ; d<k ; d++ ){
     double acc=0.0 ;
     for( j=0 ; j<k ; j++ ) acc += Q[c*k+j]*Q[d*k+j]/sqrt(ev[j]) ;
     out[c*k+d]=acc ;
   }
   return SRM_OK ;
}

int SRM_shared_similarity( const float *s1 , const float *s2 , int nfeat , int nsamp ,
                           double *mean_cos2 , float *cos_out )
{
   int k=nfeat , c,d,t,j , rc=SRM_OK ;
   double *G1=NULL , *G2=NULL , *G12=NULL , *I1=NULL , *I2=NULL , *C=NULL , *M=NULL ;
   double *tmp=NULL , *wk=NULL , *ev=NULL , *Q=NULL , tr=0.0 ;
   if( s1==NULL || s2==NULL || mean_cos2==NULL || k<1 || nsamp<1 ) return SRM_ERR_ARG ;
   if( k>nsamp ) return SRM_ERR_NFEAT ;
   G1=(double *)srm_calloc((size_t)k*k,sizeof(double)) ; G2=(double *)srm_calloc((size_t)k*k,sizeof(double)) ;
   G12=(double *)srm_calloc((size_t)k*k,sizeof(double)) ; I1=(double *)srm_calloc((size_t)k*k,sizeof(double)) ;
   I2=(double *)srm_calloc((size_t)k*k,sizeof(double)) ; C=(double *)srm_calloc((size_t)k*k,sizeof(double)) ;
   M=(double *)srm_calloc((size_t)k*k,sizeof(double)) ; tmp=(double *)srm_calloc((size_t)k*k,sizeof(double)) ;
   wk=(double *)srm_calloc((size_t)3*k*k,sizeof(double)) ; ev=(double *)srm_calloc(k,sizeof(double)) ;
   Q=(double *)srm_calloc((size_t)k*k,sizeof(double)) ;
   if( !G1||!G2||!G12||!I1||!I2||!C||!M||!tmp||!wk||!ev||!Q ){ rc=SRM_ERR_MEMORY ; goto done ; }
   for( c=0 ; c<k ; c++ ) for( d=0 ; d<k ; d++ ){
     double a11=0.0 , a22=0.0 , a12=0.0 ;
     for( t=0 ; t<nsamp ; t++ ){
       double x1=s1[(size_t)c*nsamp+t] , y1=s1[(size_t)d*nsamp+t] ;
       double x2=s2[(size_t)c*nsamp+t] , y2=s2[(size_t)d*nsamp+t] ;
       a11+=x1*y1 ; a22+=x2*y2 ; a12+=x1*(double)s2[(size_t)d*nsamp+t] ;
     }
     G1[c*k+d]=a11 ; G2[c*k+d]=a22 ; G12[c*k+d]=a12 ;
   }
   rc=srm_inv_sqrt(k,G1,I1,wk) ; if( rc!=SRM_OK ) goto done ;
   rc=srm_inv_sqrt(k,G2,I2,wk) ; if( rc!=SRM_OK ) goto done ;
   for( c=0 ; c<k ; c++ ) for( d=0 ; d<k ; d++ ){          /* C = I1 G12 I2 */
     double acc=0.0 ; for( j=0 ; j<k ; j++ ) acc += I1[c*k+j]*G12[j*k+d] ;
     tmp[c*k+d]=acc ;
   }
   for( c=0 ; c<k ; c++ ) for( d=0 ; d<k ; d++ ){
     double acc=0.0 ; for( j=0 ; j<k ; j++ ) acc += tmp[c*k+j]*I2[j*k+d] ;
     C[c*k+d]=acc ;
   }
   for( c=0 ; c<k ; c++ ) for( d=0 ; d<k ; d++ ){          /* M = C'C: eigenvalues are cos^2 */
     double acc=0.0 ; for( j=0 ; j<k ; j++ ) acc += C[j*k+c]*C[j*k+d] ;
     M[c*k+d]=acc ;
   }
   for( c=0 ; c<k ; c++ ) tr += M[c*k+c] ;
   if( srm_eigsym(k,M,ev,Q) ){ rc=SRM_ERR_SINGULAR ; goto done ; }
   *mean_cos2 = tr/k ;
   if( cos_out != NULL ) for( c=0 ; c<k ; c++ ){
     double v=ev[c] ; if( v<0.0 ) v=0.0 ; if( v>1.0 ) v=1.0 ;
     cos_out[c]=(float)sqrt(v) ;
   }
done:
   free(G1) ; free(G2) ; free(G12) ; free(I1) ; free(I2) ; free(C) ; free(M) ; free(tmp) ;
   free(wk) ; free(ev) ; free(Q) ;
   return rc ;
}

int SRM_split_reliability( const float *xa , const float *xb , int nrow , int ncol ,
                           double *r_pattern , double *r_series )
{
   double *u=NULL , *v=NULL , *rr=NULL ; int *ok=NULL ; int i , j , rc=SRM_OK ;
   int n=(nrow>ncol) ? nrow : ncol ;
   if( xa==NULL || xb==NULL || nrow<2 || ncol<2 ) return SRM_ERR_ARG ;
   u=(double *)srm_calloc(n,sizeof(double)) ; v=(double *)srm_calloc(n,sizeof(double)) ;
   rr=(double *)srm_calloc(n,sizeof(double)) ; ok=(int *)srm_calloc(n,sizeof(int)) ;
   if( !u||!v||!rr||!ok ){ free(u) ; free(v) ; free(rr) ; free(ok) ; return SRM_ERR_MEMORY ; }
   if( r_pattern != NULL ){                       /* each column across rows */
     int nv=0 ;
     for( j=0 ; j<ncol ; j++ ){
       for( i=0 ; i<nrow ; i++ ){ u[i]=xa[(size_t)i*ncol+j] ; v[i]=xb[(size_t)i*ncol+j] ; }
       rr[j]=srm_pearson(u,v,nrow,&ok[j]) ; nv+=ok[j] ;
     }
     if( nv==0 ) rc=SRM_ERR_SINGULAR ; else *r_pattern=srm_fisher_mean(rr,ok,ncol) ;
   }
   if( rc==SRM_OK && r_series != NULL ){          /* each row across columns */
     int nv=0 ;
     for( i=0 ; i<nrow ; i++ ){
       for( j=0 ; j<ncol ; j++ ){ u[j]=xa[(size_t)i*ncol+j] ; v[j]=xb[(size_t)i*ncol+j] ; }
       rr[i]=srm_pearson(u,v,ncol,&ok[i]) ; nv+=ok[i] ;
     }
     if( nv==0 ) rc=SRM_ERR_SINGULAR ; else *r_series=srm_fisher_mean(rr,ok,nrow) ;
   }
   free(u) ; free(v) ; free(rr) ; free(ok) ;
   return rc ;
}

double SRM_spearman_brown( double r , double factor )
{
   double den = 1.0 + (factor-1.0)*r ;
   if( !(den>0.0) ) return NAN ;
   return factor*r/den ;
}

int SRM_gather_columns( const float *x , int nrow , int ncol ,
                        const unsigned char *sel , float *out )
{
   int j , i , n=0 ;
   if( x==NULL || sel==NULL || out==NULL || nrow<1 || ncol<1 ) return -1 ;
   for( j=0 ; j<ncol ; j++ ) if( sel[j] ) n++ ;
   if( n==0 ) return 0 ;
   for( i=0 ; i<nrow ; i++ ){
     int q=0 ;
     for( j=0 ; j<ncol ; j++ ) if( sel[j] ) out[(size_t)i*n+q++]=x[(size_t)i*ncol+j] ;
   }
   return n ;
}

int SRM_zscore_segments( float *x , int nvox , int ntime , int nseg ,
                         const int *seglen , unsigned char *bad )
{
   int f , r , sum=0 , nbad=0 , err=SRM_OK ;
   if( x==NULL || nvox<1 || ntime<1 || nseg<1 || seglen==NULL ) return SRM_ERR_ARG ;
   for( r=0 ; r<nseg ; r++ ){ if( seglen[r]<2 ) return SRM_ERR_ARG ; sum+=seglen[r] ; }
   if( sum!=ntime ) return SRM_ERR_ARG ;
SRM_PRAGMA(omp parallel for schedule(static) reduction(+:nbad))
   for( f=0 ; f<nvox ; f++ ){
     float *row=x+(size_t)f*ntime ; int c0=0 , isbad=0 , rr , t , nonfin=0 ;
     for( rr=0 ; rr<nseg ; rr++ ){
       int L=seglen[rr] ; double mean=0.0 , var=0.0 , sd ;
       for( t=0 ; t<L ; t++ ){ double v=row[c0+t] ; if( !isfinite(v) ) nonfin=1 ; mean+=v ; }
       mean/=L ;
       for( t=0 ; t<L ; t++ ){ double d=row[c0+t]-mean ; var+=d*d ; }
       sd=sqrt(var/L) ;
       if( !nonfin && (sd==0.0 || sd <= 1.0e-7*fabs(mean)) ) isbad=1 ;
       if( !nonfin && !isbad )
         for( t=0 ; t<L ; t++ ) row[c0+t]=(float)((row[c0+t]-mean)/sd) ;
       c0+=L ;
     }
     if( nonfin ){
SRM_PRAGMA(omp critical(srm_err))
       err=SRM_ERR_NONFINITE ;
     }
     if( isbad && !nonfin ){
       for( t=0 ; t<ntime ; t++ ) row[t]=0.0f ;
       nbad++ ;
     }
     if( bad!=NULL ) bad[f]=(unsigned char)(isbad && !nonfin) ;
   }
   if( err!=SRM_OK ) return err ;
   return nbad ;
}

/*============================================================================
  Step 6: the model file, and the checks that guard its use
==============================================================================*/

SRM_provenance * SRM_provenance_new( void )
{
   SRM_provenance *pv=(SRM_provenance *)srm_calloc(1,sizeof(*pv)) ;
   if( pv==NULL ) return NULL ;
   pv->version=SRM_FILE_VERSION ; pv->axis=SRM_AXIS_TIME ; pv->zscore=SRM_ZSCORE_RUN ;
   return pv ;
}

void SRM_provenance_free( SRM_provenance *pv )
{
   int ii ;
   if( pv==NULL ) return ;
   free(pv->keep) ; free(pv->seglen) ;
   if( pv->label!=NULL ){ for( ii=0 ; ii<pv->nsubj ; ii++ ) free(pv->label[ii]) ; free(pv->label) ; }
   if( pv->item_key!=NULL ){ for( ii=0 ; ii<pv->nitem ; ii++ ) free(pv->item_key[ii]) ; free(pv->item_key) ; }
   free(pv) ;
}

/*! A label or key that can live on one whitespace-delimited line. */
static int srm_token_ok( const char *s )
{
   if( s==NULL || s[0]=='\0' || s[0]=='#' ) return 0 ;
   for( ; *s ; s++ ) if( isspace((unsigned char)*s) || iscntrl((unsigned char)*s) ) return 0 ;
   return 1 ;
}

int SRM_provenance_check( const SRM_provenance *pv , int ntime_in , int nseg ,
                          const int *seglen )
{
   int rr ;
   if( pv==NULL || seglen==NULL || nseg<1 ) return SRM_ERR_ARG ;
   if( pv->axis != SRM_AXIS_TIME ) return SRM_ERR_MISMATCH ;
   if( ntime_in != pv->ntime_in ) return SRM_ERR_MISMATCH ;
   if( nseg != pv->nseg || pv->seglen==NULL ) return SRM_ERR_MISMATCH ;
   for( rr=0 ; rr<nseg ; rr++ ) if( seglen[rr] != pv->seglen[rr] ) return SRM_ERR_MISMATCH ;
   return SRM_OK ;
}

typedef struct { const char *key ; int idx ; } srm_keyidx ;

static int srm_keyidx_cmp( const void *a , const void *b )
{
   return strcmp(((const srm_keyidx *)a)->key,((const srm_keyidx *)b)->key) ;
}

int SRM_provenance_match_items( const SRM_provenance *pv , int nkey ,
                                char * const *key , int *perm )
{
   srm_keyidx *tab ; unsigned char *seen ; int j , rc=SRM_OK ;
   if( pv==NULL || key==NULL || perm==NULL || nkey<1 ) return SRM_ERR_ARG ;
   if( pv->axis != SRM_AXIS_ITEM || pv->item_key==NULL ) return SRM_ERR_MISMATCH ;
   if( nkey != pv->nitem ) return SRM_ERR_MISMATCH ;
   tab=(srm_keyidx *)srm_calloc(pv->nitem,sizeof(*tab)) ;
   seen=(unsigned char *)srm_calloc(pv->nitem,1) ;
   if( tab==NULL || seen==NULL ){ free(tab) ; free(seen) ; return SRM_ERR_MEMORY ; }
   for( j=0 ; j<pv->nitem ; j++ ){ tab[j].key=pv->item_key[j] ; tab[j].idx=j ; }
   qsort(tab,pv->nitem,sizeof(*tab),srm_keyidx_cmp) ;
   for( j=1 ; j<pv->nitem ; j++ )                       /* the model's own keys must be distinct */
     if( strcmp(tab[j-1].key,tab[j].key)==0 ){ rc=SRM_ERR_MISMATCH ; goto done ; }
   for( j=0 ; j<nkey ; j++ ){
     srm_keyidx probe , *hit ;
     if( key[j]==NULL ){ rc=SRM_ERR_ARG ; goto done ; }
     probe.key=key[j] ; probe.idx=0 ;
     hit=(srm_keyidx *)bsearch(&probe,tab,pv->nitem,sizeof(*tab),srm_keyidx_cmp) ;
     if( hit==NULL || seen[hit->idx] ){ rc=SRM_ERR_MISMATCH ; goto done ; }   /* extra or repeated */
     seen[hit->idx]=1 ; perm[j]=hit->idx ;
   }
done:
   free(tab) ; free(seen) ;
   return rc ;
}

/*---------------------------------------------------------------- writing */

static int srm_all_finite_f( const float *a , size_t n )
{ size_t i ; for( i=0 ; i<n ; i++ ) if( !isfinite(a[i]) ) return 0 ; return 1 ; }
static int srm_all_finite_d( const double *a , size_t n )
{ size_t i ; for( i=0 ; i<n ; i++ ) if( !isfinite(a[i]) ) return 0 ; return 1 ; }

static void srm_put_floats( FILE *fp , const char *name , int r , int c , const float *a )
{
   int i , j ;
   fprintf(fp,"data %s %d %d\n",name,r,c) ;
   for( i=0 ; i<r ; i++ ) for( j=0 ; j<c ; j++ )
     fprintf(fp,"%.9g%c",(double)a[(size_t)i*c+j],(j==c-1) ? '\n' : ' ') ;
}

static void srm_put_doubles( FILE *fp , const char *name , int r , int c , const double *a )
{
   int i , j ;
   fprintf(fp,"data %s %d %d\n",name,r,c) ;
   for( i=0 ; i<r ; i++ ) for( j=0 ; j<c ; j++ )
     fprintf(fp,"%.17g%c",a[(size_t)i*c+j],(j==c-1) ? '\n' : ' ') ;
}

int SRM_save_model( const char *fname , const SRM_model *m , const SRM_provenance *pv )
{
   int ii , k , T , prob , kept=0 ; size_t jj ; char tmp[4200] ; FILE *fp ;
   if( fname==NULL || fname[0]=='\0' || m==NULL || pv==NULL ) return SRM_ERR_ARG ;
   if( m->s==NULL || m->a_sum==NULL || m->nview<1 || m->nfeat<1 || m->ntime<1 || m->nvox==NULL )
     return SRM_ERR_ARG ;
   k=m->nfeat ; T=m->ntime ; prob=(m->algo==SRM_ALGO_PROB) ;
   if( m->algo!=SRM_ALGO_PROB && m->algo!=SRM_ALGO_DET ) return SRM_ERR_ARG ;
   if( prob && (m->rho2==NULL || m->sigma_s==NULL) ) return SRM_ERR_MISMATCH ;
   if( pv->nsubj != m->nview || pv->label==NULL ) return SRM_ERR_MISMATCH ;
   for( ii=0 ; ii<m->nview ; ii++ ) if( !srm_token_ok(pv->label[ii]) || m->nvox[ii]<1 ) return SRM_ERR_MISMATCH ;
   if( pv->axis==SRM_AXIS_TIME ){
     int sum=0 , rr ;
     if( pv->nseg<1 || pv->seglen==NULL ) return SRM_ERR_MISMATCH ;
     for( rr=0 ; rr<pv->nseg ; rr++ ){ if( pv->seglen[rr]<1 ) return SRM_ERR_MISMATCH ; sum+=pv->seglen[rr] ; }
     if( sum!=T ) return SRM_ERR_MISMATCH ;
     if( pv->keep!=NULL ){
       if( pv->ntime_in<T ) return SRM_ERR_MISMATCH ;
       for( ii=0 ; ii<pv->ntime_in ; ii++ ) kept += (pv->keep[ii]!=0) ;
       if( kept!=T ) return SRM_ERR_MISMATCH ;
     } else if( pv->ntime_in!=T ) return SRM_ERR_MISMATCH ;
   } else if( pv->axis==SRM_AXIS_ITEM ){
     if( pv->nitem!=T || pv->item_key==NULL ) return SRM_ERR_MISMATCH ;
     for( ii=0 ; ii<pv->nitem ; ii++ ) if( !srm_token_ok(pv->item_key[ii]) ) return SRM_ERR_MISMATCH ;
   } else return SRM_ERR_ARG ;
   if( pv->zscore!=SRM_ZSCORE_NONE && pv->zscore!=SRM_ZSCORE_RUN && pv->zscore!=SRM_ZSCORE_ALL ) return SRM_ERR_ARG ;
   if( !srm_all_finite_f(m->s,(size_t)k*T) || !srm_all_finite_d(m->a_sum,(size_t)k*T) ||
       !isfinite(m->drift) || !isfinite(m->rho_inv_sum) || !isfinite(m->coupling) ) return SRM_ERR_NONFINITE ;
   if( prob && ( !srm_all_finite_f(m->sigma_s,(size_t)k*k) || !srm_all_finite_d(m->rho2,m->nview) ) )
     return SRM_ERR_NONFINITE ;

   snprintf(tmp,sizeof(tmp),"%s.tmp%ld",fname,(long)getpid()) ;
   fp=fopen(tmp,"w") ;
   if( fp==NULL ) return SRM_ERR_IO ;
   fprintf(fp,"# 3dSRM model file (shared response model). Plain text; see thd_srm.h for the format.\n") ;
   fprintf(fp,"srm_version %d\n",SRM_FILE_VERSION) ;
   fprintf(fp,"algo %s\n",prob ? "prob" : "det") ;
   fprintf(fp,"axis %s\n",pv->axis==SRM_AXIS_TIME ? "time" : "item") ;
   fprintf(fp,"zscore %s\n",pv->zscore==SRM_ZSCORE_RUN ? "run" : pv->zscore==SRM_ZSCORE_ALL ? "all" : "none") ;
   fprintf(fp,"nfeat %d\nntime %d\nnsubj %d\nwseg %d\ncenter %d\ncoupled %d\n",k,T,m->nview,
           m->nseg>0 ? m->nseg : 1,m->center ? 1 : 0,m->coupled ? 1 : 0) ;
   fprintf(fp,"coupling %.17g\nnsubj_eff %d\nnupdate %d\ndrift %.17g\nrho_inv_sum %.17g\n",
           m->coupling,m->nsubj_eff,m->nupdate,m->drift,m->rho_inv_sum) ;
   if( pv->axis==SRM_AXIS_TIME ){
     int rr ;
     fprintf(fp,"ntime_in %d\nnseg %d\nseglen",pv->ntime_in,pv->nseg) ;
     for( rr=0 ; rr<pv->nseg ; rr++ ) fprintf(fp," %d",pv->seglen[rr]) ;
     fprintf(fp,"\nkeep ") ;
     if( pv->keep==NULL ) fprintf(fp,"all") ;
     else for( ii=0 ; ii<pv->ntime_in ; ii++ ) fputc(pv->keep[ii] ? '1' : '0',fp) ;
     fprintf(fp,"\n") ;
   } else {
     fprintf(fp,"nitem %d\n",pv->nitem) ;
     for( ii=0 ; ii<pv->nitem ; ii++ ) fprintf(fp,"item %s\n",pv->item_key[ii]) ;
   }
   for( ii=0 ; ii<m->nview ; ii++ ) fprintf(fp,"subject %s %d\n",pv->label[ii],m->nvox[ii]) ;
   srm_put_floats(fp,"S",k,T,m->s) ;
   if( prob ){
     srm_put_floats(fp,"sigma_s",k,k,m->sigma_s) ;
     srm_put_doubles(fp,"rho2",m->nview,1,m->rho2) ;
   }
   srm_put_doubles(fp,"a_sum",k,T,m->a_sum) ;
   if( m->nsubj_t!=NULL ) srm_put_doubles(fp,"nsubj_t",1,T,m->nsubj_t) ;      /* missing data: the sums per time point */
   if( prob && m->rho_inv_t!=NULL ) srm_put_doubles(fp,"rho_inv_t",1,T,m->rho_inv_t) ;
   fprintf(fp,"end\n") ;
   (void)jj ;
   if( ferror(fp) || fclose(fp)!=0 ){ remove(tmp) ; return SRM_ERR_IO ; }
   if( rename(tmp,fname)!=0 ){
     remove(fname) ;                                  /* platforms where rename will not overwrite */
     if( rename(tmp,fname)!=0 ){ remove(tmp) ; return SRM_ERR_IO ; }
   }
   return SRM_OK ;
}

/*---------------------------------------------------------------- reading */

static const char * srm_skip_ws( const char *p )
{
   while( *p==' ' || *p=='\t' || *p=='\n' || *p=='\r' ) p++ ;
   return p ;
}

static const char * srm_eol( const char *p )
{
   while( *p && *p!='\n' ) p++ ;
   return p ;
}

/*! One finite number from the stream, wherever the line breaks fall.  A token
    must begin like a number, so a keyword (or "inf"/"nan") is a format error
    rather than something strtod would accept. */
static int srm_read_num( const char **pp , double *out )
{
   const char *p=srm_skip_ws(*pp) ; char *end ; double v ;
   if( !( isdigit((unsigned char)*p) || *p=='+' || *p=='-' || *p=='.' ) ) return 0 ;
   errno=0 ; v=strtod(p,&end) ;
   if( end==p || !isfinite(v) ) return 0 ;
   *pp=end ; *out=v ;
   return 1 ;
}

static int srm_line_int( const char *p , const char *eol , int *out )
{
   char *end ; long v ;
   p=srm_skip_ws(p) ;
   if( p>=eol ) return 0 ;
   errno=0 ; v=strtol(p,&end,10) ;
   if( end==p || end>eol || errno!=0 || v<-2147483647L || v>2147483647L ) return 0 ;
   *out=(int)v ; return 1 ;
}

static int srm_line_double( const char *p , const char *eol , double *out )
{
   char *end ; double v ;
   p=srm_skip_ws(p) ;
   if( p>=eol ) return 0 ;
   errno=0 ; v=strtod(p,&end) ;
   if( end==p || end>eol || !isfinite(v) ) return 0 ;
   *out=v ; return 1 ;
}

/*! First whitespace-delimited word of [p,eol), copied; *next is set past it. */
static char * srm_line_word( const char *p , const char *eol , const char **next )
{
   const char *q ; char *w ;
   p=srm_skip_ws(p) ;
   if( p>=eol ) return NULL ;
   q=p ; while( q<eol && *q!=' ' && *q!='\t' && *q!='\r' ) q++ ;
   w=(char *)malloc((size_t)(q-p)+1) ;
   if( w==NULL ) return NULL ;
   memcpy(w,p,(size_t)(q-p)) ; w[q-p]='\0' ;
   if( next!=NULL ) *next=q ;
   return w ;
}

typedef struct {
   int have_version ;
   int algo , axis , zscore , nfeat , ntime , nsubj , wseg , center , coupled , nsubj_eff , nupdate ;
   int ntime_in , nseg , nitem ;
   double coupling , drift , rho_inv ;
   int *seglen ; unsigned char *keep ; int have_keep ;
   char **item , **label ; int *nvox ; int nitem_read , nsub_read ;
   float *S , *sigma ; double *rho2 , *A ; int got_S , got_sigma , got_rho2 , got_A , got_end ;
   double *nt , *rt ; int got_nt , got_rt ;
} srm_parsed ;

static void srm_parsed_free( srm_parsed *ps )
{
   int ii ;
   free(ps->seglen) ; free(ps->keep) ; free(ps->nvox) ;
   if( ps->item!=NULL ){ for( ii=0 ; ii<ps->nitem_read ; ii++ ) free(ps->item[ii]) ; free(ps->item) ; }
   if( ps->label!=NULL ){ for( ii=0 ; ii<ps->nsub_read ; ii++ ) free(ps->label[ii]) ; free(ps->label) ; }
   free(ps->S) ; free(ps->sigma) ; free(ps->rho2) ; free(ps->A) ; free(ps->nt) ; free(ps->rt) ;
}

/*! Read r*c numbers into a float or double array allocated here (when dst is
    non-NULL) or just skip them. */
static int srm_read_block( const char **pp , long n , float **fdst , double **ddst )
{
   long i ; double v ;
   if( fdst!=NULL ){ *fdst=(float *)malloc(sizeof(float)*(size_t)(n>0?n:1)) ; if( *fdst==NULL ) return 0 ; }
   if( ddst!=NULL ){ *ddst=(double *)malloc(sizeof(double)*(size_t)(n>0?n:1)) ; if( *ddst==NULL ) return 0 ; }
   for( i=0 ; i<n ; i++ ){
     if( !srm_read_num(pp,&v) ) return 0 ;
     if( fdst!=NULL ) (*fdst)[i]=(float)v ;
     if( ddst!=NULL ) (*ddst)[i]=v ;
   }
   return 1 ;
}

int SRM_load_model( const char *fname , SRM_model **model , SRM_provenance **pvout )
{
   FILE *fp ; long len ; char *buf=NULL ; const char *p ;
   srm_parsed ps ; SRM_model *m=NULL ; SRM_provenance *pv=NULL ;
   int rc=SRM_ERR_FORMAT , ii , k , T ;

   if( model!=NULL ) *model=NULL ;
   if( pvout!=NULL ) *pvout=NULL ;
   if( fname==NULL || model==NULL || pvout==NULL ) return SRM_ERR_ARG ;
   fp=fopen(fname,"rb") ;
   if( fp==NULL ) return SRM_ERR_IO ;
   if( fseek(fp,0,SEEK_END)!=0 || (len=ftell(fp))<0 || fseek(fp,0,SEEK_SET)!=0 ){ fclose(fp) ; return SRM_ERR_IO ; }
   buf=(char *)malloc((size_t)len+1) ;
   if( buf==NULL ){ fclose(fp) ; return SRM_ERR_MEMORY ; }
   if( fread(buf,1,(size_t)len,fp)!=(size_t)len ){ fclose(fp) ; free(buf) ; return SRM_ERR_IO ; }
   fclose(fp) ;
   buf[len]='\0' ;

   memset(&ps,0,sizeof(ps)) ;
   ps.coupling=SRM_COUPLING_SHARED ; ps.wseg=1 ;

   for( p=buf ; *p ; ){
     const char *eol , *rest ; char *key ;
     p=srm_skip_ws(p) ;
     if( *p=='\0' ) break ;
     eol=srm_eol(p) ;
     if( *p=='#' ){ p=(*eol) ? eol+1 : eol ; continue ; }
     key=srm_line_word(p,eol,&rest) ;
     if( key==NULL ){ p=(*eol) ? eol+1 : eol ; continue ; }

#define KEYIS(s) (strcmp(key,(s))==0)
#define GETINT(var) do{ if( !srm_line_int(rest,eol,&(var)) ){ free(key) ; goto bad ; } }while(0)
     if( KEYIS("srm_version") ){
       int v ; if( !srm_line_int(rest,eol,&v) ){ free(key) ; goto bad ; }
       if( v>SRM_FILE_VERSION ){ free(key) ; rc=SRM_ERR_VERSION ; goto bad ; }
       if( v<1 ){ free(key) ; goto bad ; }
       ps.have_version=1 ;
     } else if( !ps.have_version ){ free(key) ; goto bad ;          /* the version must come first */
     } else if( KEYIS("algo") ){
       char *w=srm_line_word(rest,eol,NULL) ;
       if( w!=NULL && !strcmp(w,"prob") ) ps.algo=SRM_ALGO_PROB ;
       else if( w!=NULL && !strcmp(w,"det") ) ps.algo=SRM_ALGO_DET ;
       else { free(w) ; free(key) ; goto bad ; }
       free(w) ;
     } else if( KEYIS("axis") ){
       char *w=srm_line_word(rest,eol,NULL) ;
       if( w!=NULL && !strcmp(w,"time") ) ps.axis=SRM_AXIS_TIME ;
       else if( w!=NULL && !strcmp(w,"item") ) ps.axis=SRM_AXIS_ITEM ;
       else { free(w) ; free(key) ; goto bad ; }
       free(w) ;
     } else if( KEYIS("zscore") ){
       char *w=srm_line_word(rest,eol,NULL) ;
       if( w!=NULL && !strcmp(w,"run") ) ps.zscore=SRM_ZSCORE_RUN ;
       else if( w!=NULL && !strcmp(w,"all") ) ps.zscore=SRM_ZSCORE_ALL ;
       else if( w!=NULL && !strcmp(w,"none") ) ps.zscore=SRM_ZSCORE_NONE ;
       else { free(w) ; free(key) ; goto bad ; }
       free(w) ;
     } else if( KEYIS("nfeat") ) GETINT(ps.nfeat) ;
     else if( KEYIS("ntime") )  GETINT(ps.ntime) ;
     else if( KEYIS("nsubj") )  GETINT(ps.nsubj) ;
     else if( KEYIS("wseg") )   GETINT(ps.wseg) ;
     else if( KEYIS("center") ) GETINT(ps.center) ;
     else if( KEYIS("coupled") ) GETINT(ps.coupled) ;
     else if( KEYIS("nsubj_eff") ) GETINT(ps.nsubj_eff) ;
     else if( KEYIS("nupdate") ) GETINT(ps.nupdate) ;
     else if( KEYIS("ntime_in") ) GETINT(ps.ntime_in) ;
     else if( KEYIS("nseg") )   GETINT(ps.nseg) ;
     else if( KEYIS("nitem") )  GETINT(ps.nitem) ;
     else if( KEYIS("coupling") ){ if( !srm_line_double(rest,eol,&ps.coupling) ){ free(key) ; goto bad ; } }
     else if( KEYIS("drift") ){ if( !srm_line_double(rest,eol,&ps.drift) ){ free(key) ; goto bad ; } }
     else if( KEYIS("rho_inv_sum") ){ if( !srm_line_double(rest,eol,&ps.rho_inv) ){ free(key) ; goto bad ; } }
     else if( KEYIS("seglen") ){
       int cnt=0 , v ; const char *q=rest ; char *end ;
       if( ps.nseg<1 ){ free(key) ; goto bad ; }                       /* nseg precedes seglen */
       ps.seglen=(int *)malloc(sizeof(int)*(size_t)ps.nseg) ;
       if( ps.seglen==NULL ){ free(key) ; rc=SRM_ERR_MEMORY ; goto bad ; }
       for( ; cnt<ps.nseg ; cnt++ ){
         q=srm_skip_ws(q) ; if( q>=eol ) break ;
         v=(int)strtol(q,&end,10) ;
         if( end==q ){ free(key) ; goto bad ; }
         ps.seglen[cnt]=v ; q=end ;
       }
       if( cnt!=ps.nseg ){ free(key) ; goto bad ; }
     } else if( KEYIS("keep") ){
       char *w=srm_line_word(rest,eol,NULL) ; size_t n ;
       if( w==NULL ){ free(key) ; goto bad ; }
       if( !strcmp(w,"all") ){ ps.have_keep=1 ; ps.keep=NULL ; }
       else {
         n=strlen(w) ;
         ps.keep=(unsigned char *)malloc(n+1) ;
         if( ps.keep==NULL ){ free(w) ; free(key) ; rc=SRM_ERR_MEMORY ; goto bad ; }
         for( ii=0 ; ii<(int)n ; ii++ ){
           if( w[ii]!='0' && w[ii]!='1' ){ free(w) ; free(key) ; goto bad ; }
           ps.keep[ii]=(unsigned char)(w[ii]=='1') ;
         }
         if( ps.ntime_in!=(int)n ){ free(w) ; free(key) ; goto bad ; }
         ps.have_keep=1 ;
       }
       free(w) ;
     } else if( KEYIS("item") ){
       char *w=srm_line_word(rest,eol,NULL) ; char **nw ;
       if( w==NULL ){ free(key) ; goto bad ; }
       nw=(char **)realloc(ps.item,sizeof(char *)*(size_t)(ps.nitem_read+1)) ;
       if( nw==NULL ){ free(w) ; free(key) ; rc=SRM_ERR_MEMORY ; goto bad ; }
       ps.item=nw ; ps.item[ps.nitem_read++]=w ;
     } else if( KEYIS("subject") ){
       const char *after ; char *w=srm_line_word(rest,eol,&after) ; int nv ; char **nl ; int *nn ;
       if( w==NULL || !srm_line_int(after,eol,&nv) || nv<1 ){ free(w) ; free(key) ; goto bad ; }
       nl=(char **)realloc(ps.label,sizeof(char *)*(size_t)(ps.nsub_read+1)) ;
       if( nl==NULL ){ free(w) ; free(key) ; rc=SRM_ERR_MEMORY ; goto bad ; }
       ps.label=nl ;
       nn=(int *)realloc(ps.nvox,sizeof(int)*(size_t)(ps.nsub_read+1)) ;
       if( nn==NULL ){ free(w) ; free(key) ; rc=SRM_ERR_MEMORY ; goto bad ; }
       ps.nvox=nn ;
       ps.label[ps.nsub_read]=w ; ps.nvox[ps.nsub_read]=nv ; ps.nsub_read++ ;
     } else if( KEYIS("data") ){
       const char *a1 , *a2 ; char *name=srm_line_word(rest,eol,&a1) ; int r , c ; long n ;
       if( name==NULL || !srm_line_int(a1,eol,&r) ){ free(name) ; free(key) ; goto bad ; }
       a2=srm_skip_ws(a1) ; while( a2<eol && *a2!=' ' && *a2!='\t' ) a2++ ;      /* past the rows count */
       if( !srm_line_int(a2,eol,&c) || r<1 || c<1 ){ free(name) ; free(key) ; goto bad ; }
       n=(long)r*(long)c ;
       p=(*eol) ? eol+1 : eol ;
       if( !strcmp(name,"S") ){
         if( ps.got_S || r!=ps.nfeat || c!=ps.ntime || !srm_read_block(&p,n,&ps.S,NULL) ){ free(name) ; free(key) ; goto bad ; }
         ps.got_S=1 ;
       } else if( !strcmp(name,"sigma_s") ){
         if( ps.got_sigma || r!=ps.nfeat || c!=ps.nfeat || !srm_read_block(&p,n,&ps.sigma,NULL) ){ free(name) ; free(key) ; goto bad ; }
         ps.got_sigma=1 ;
       } else if( !strcmp(name,"rho2") ){
         if( ps.got_rho2 || r!=ps.nsubj || c!=1 || !srm_read_block(&p,n,NULL,&ps.rho2) ){ free(name) ; free(key) ; goto bad ; }
         ps.got_rho2=1 ;
       } else if( !strcmp(name,"a_sum") ){
         if( ps.got_A || r!=ps.nfeat || c!=ps.ntime || !srm_read_block(&p,n,NULL,&ps.A) ){ free(name) ; free(key) ; goto bad ; }
         ps.got_A=1 ;
       } else if( !strcmp(name,"nsubj_t") ){
         if( ps.got_nt || r!=1 || c!=ps.ntime || !srm_read_block(&p,n,NULL,&ps.nt) ){ free(name) ; free(key) ; goto bad ; }
         ps.got_nt=1 ;
       } else if( !strcmp(name,"rho_inv_t") ){
         if( ps.got_rt || r!=1 || c!=ps.ntime || !srm_read_block(&p,n,NULL,&ps.rt) ){ free(name) ; free(key) ; goto bad ; }
         ps.got_rt=1 ;
       } else {                                                             /* a block from the future: skip */
         if( !srm_read_block(&p,n,NULL,NULL) ){ free(name) ; free(key) ; goto bad ; }
       }
       free(name) ; free(key) ;
       continue ;                                                           /* p already advanced */
     } else if( KEYIS("end") ){
       ps.got_end=1 ; free(key) ; break ;
     }
     /* any other key is from a later version: ignored */
     free(key) ;
     p=(*eol) ? eol+1 : eol ;
   }
#undef KEYIS
#undef GETINT

   /* completeness and consistency */
   if( !ps.have_version || !ps.got_end ) goto bad ;                          /* truncated */
   if( ps.algo==0 || ps.axis==0 || ps.nfeat<1 || ps.ntime<1 || ps.nsubj<1 ) goto bad ;
   if( ps.nfeat>ps.ntime || ps.wseg<1 || ps.nsubj_eff<1 || ps.nupdate<0 ) goto bad ;
   if( ps.nsub_read!=ps.nsubj || !ps.got_S || !ps.got_A ) goto bad ;
   if( ps.algo==SRM_ALGO_PROB && ( !ps.got_sigma || !ps.got_rho2 || !(ps.rho_inv>0.0) ) ) goto bad ;
   if( ps.coupled && ps.wseg<2 ) goto bad ;
   if( ps.algo==SRM_ALGO_PROB && ps.got_nt!=ps.got_rt ) goto bad ;           /* per-time sums come together */
   if( ps.got_nt ) for( ii=0 ; ii<ps.ntime ; ii++ ) if( !(ps.nt[ii]>=0.0 && ps.nt[ii]<=ps.nsubj_eff) ) goto bad ;
   if( ps.got_rt ) for( ii=0 ; ii<ps.ntime ; ii++ ) if( !(ps.rt[ii]>=0.0) ) goto bad ;
   for( ii=0 ; ii<ps.nsubj ; ii++ ) if( !srm_token_ok(ps.label[ii]) ) goto bad ;
   if( ps.algo==SRM_ALGO_PROB ) for( ii=0 ; ii<ps.nsubj ; ii++ ) if( !(ps.rho2[ii]>0.0) ) goto bad ;
   if( ps.axis==SRM_AXIS_TIME ){
     int sum=0 , rr , kept=0 ;
     if( ps.nseg<1 || ps.seglen==NULL || !ps.have_keep ) goto bad ;
     for( rr=0 ; rr<ps.nseg ; rr++ ){ if( ps.seglen[rr]<1 ) goto bad ; sum+=ps.seglen[rr] ; }
     if( sum!=ps.ntime ) goto bad ;
     if( ps.keep!=NULL ){
       for( ii=0 ; ii<ps.ntime_in ; ii++ ) kept+=ps.keep[ii] ;
       if( kept!=ps.ntime ) goto bad ;
     } else if( ps.ntime_in!=ps.ntime ) goto bad ;
   } else {
     if( ps.nitem!=ps.ntime || ps.nitem_read!=ps.nitem ) goto bad ;
     for( ii=0 ; ii<ps.nitem ; ii++ ) if( !srm_token_ok(ps.item[ii]) ) goto bad ;
   }

   /* build the outputs */
   k=ps.nfeat ; T=ps.ntime ; rc=SRM_ERR_MEMORY ;
   m=(SRM_model *)srm_calloc(1,sizeof(*m)) ;
   pv=SRM_provenance_new() ;
   if( m==NULL || pv==NULL ) goto bad ;
   m->algo=ps.algo ; m->nview=ps.nsubj ; m->ntime=T ; m->nfeat=k ; m->nseg=ps.wseg ;
   m->center=ps.center ? 1 : 0 ; m->coupled=ps.coupled ? 1 : 0 ; m->coupling=ps.coupling ;
   m->nsubj_eff=ps.nsubj_eff ; m->nupdate=ps.nupdate ; m->drift=ps.drift ;
   m->rho_inv_sum=ps.rho_inv ;
   m->nvox=ps.nvox ; ps.nvox=NULL ;
   m->w=(float **)srm_calloc(m->nview,sizeof(float *)) ;
   if( m->w==NULL ) goto bad ;
   m->s=ps.S ; ps.S=NULL ;
   m->a_sum=ps.A ; ps.A=NULL ;
   m->nsubj_t=ps.nt ; ps.nt=NULL ;
   if( ps.algo==SRM_ALGO_PROB ){
     m->sigma_s=ps.sigma ; ps.sigma=NULL ;
     m->rho2=ps.rho2 ; ps.rho2=NULL ;
     m->rho_inv_t=ps.rt ; ps.rt=NULL ;
   }
   pv->axis=ps.axis ; pv->zscore=ps.zscore ; pv->nsubj=ps.nsubj ;
   pv->label=ps.label ; ps.label=NULL ; ps.nsub_read=0 ;
   if( ps.axis==SRM_AXIS_TIME ){
     pv->ntime_in=ps.ntime_in ; pv->nseg=ps.nseg ;
     pv->seglen=ps.seglen ; ps.seglen=NULL ;
     if( ps.keep!=NULL ){ pv->keep=ps.keep ; ps.keep=NULL ; }
     else {                                              /* "keep all": make it explicit */
       pv->keep=(unsigned char *)malloc((size_t)ps.ntime_in) ;
       if( pv->keep==NULL ) goto bad ;
       memset(pv->keep,1,(size_t)ps.ntime_in) ;
     }
   } else {
     pv->nitem=ps.nitem ; pv->item_key=ps.item ; ps.item=NULL ; ps.nitem_read=0 ;
   }
   srm_parsed_free(&ps) ; free(buf) ;
   *model=m ; *pvout=pv ;
   return SRM_OK ;

bad:
   srm_parsed_free(&ps) ; free(buf) ;
   SRM_free_model(m) ; SRM_provenance_free(pv) ;
   return rc ;
}
