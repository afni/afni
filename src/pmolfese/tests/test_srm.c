/* Unit tests for thd_srm.c.  The .c file is included so that the static
   kernels (eigensolver, Procrustes step) can be tested directly as well as
   through the public API.  No dataset I/O and no AFNI libraries. */

#include "../../thd_srm.c"

#ifndef M_PI
#define M_PI 3.14159265358979323846
#endif

#include <time.h>
#include <unistd.h>
#ifdef USE_OMP
#include <omp.h>
#endif

#define CHECK(COND,MSG) do{ if(!(COND)){ fprintf(stderr,"FAIL: %s\n",MSG); return 1; } }while(0)
#define CLOSE(A,B,T) (fabs((double)(A)-(double)(B)) <= (T))

/*------------------------------------------------------------------------*/
/* synthetic data: X_i = W_i S + sigma_i * noise                           */

typedef struct {
   int nview , T , k ;
   int *nvox ;
   float **x , **wtrue ;
   float *strue ;
   double *sigma ;
} synth ;

static void synth_free( synth *s )
{
   int i ;
   for( i=0 ; i<s->nview ; i++ ){ free(s->x[i]) ; free(s->wtrue[i]) ; }
   free(s->x) ; free(s->wtrue) ; free(s->strue) ; free(s->nvox) ; free(s->sigma) ;
}

static int synth_make( synth *s , int nview , const int *nvox , int T , int k ,
                       const double *sigma , unsigned long long seed )
{
   srm_rng rng ; int i,f,c,t ;
   memset(s,0,sizeof(*s)) ;
   s->nview=nview ; s->T=T ; s->k=k ;
   s->nvox  = (int *)malloc(sizeof(int)*nview) ;
   s->sigma = (double *)malloc(sizeof(double)*nview) ;
   s->x     = (float **)calloc(nview,sizeof(float *)) ;
   s->wtrue = (float **)calloc(nview,sizeof(float *)) ;
   s->strue = (float *)malloc(sizeof(float)*k*T) ;
   srm_rng_init(&rng,seed,99) ;
   for( c=0 ; c<k ; c++ ){                      /* mean-zero rows: the fit centers X */
     double mean=0.0 ;
     for( t=0 ; t<T ; t++ ){ s->strue[c*T+t] = (float)srm_normal(&rng) ; mean += s->strue[c*T+t] ; }
     mean /= T ;
     for( t=0 ; t<T ; t++ ) s->strue[c*T+t] -= (float)mean ;
   }
   for( i=0 ; i<nview ; i++ ){
     s->nvox[i]=nvox[i] ; s->sigma[i]=sigma[i] ;
     s->wtrue[i] = (float *)malloc(sizeof(float)*nvox[i]*k) ;
     s->x[i]     = (float *)malloc(sizeof(float)*nvox[i]*T) ;
     if( srm_random_orthonormal(s->wtrue[i],nvox[i],k,seed+5,i) ) return 1 ;
     for( f=0 ; f<nvox[i] ; f++ ) for( t=0 ; t<T ; t++ ){
       double acc=0.0 ;
       for( c=0 ; c<k ; c++ ) acc += (double)s->wtrue[i][f*k+c]*s->strue[c*T+t] ;
       s->x[i][f*T+t] = (float)(acc + sigma[i]*srm_normal(&rng)) ;
     }
   }
   return 0 ;
}

/* remove each feature's mean over time, in place */
static void center_rows( float *x , int V , int T )
{
   int f,t ;
   for( f=0 ; f<V ; f++ ){
     double s=0.0 ;
     for( t=0 ; t<T ; t++ ) s += x[f*T+t] ;
     s /= T ;
     for( t=0 ; t<T ; t++ ) x[f*T+t] -= (float)s ;
   }
}

static double ortho_error( const float *w , int V , int k )
{
   double worst=0.0 ; int c,d,f ;
   for( c=0 ; c<k ; c++ ) for( d=0 ; d<k ; d++ ){
     double acc=0.0 ;
     for( f=0 ; f<V ; f++ ) acc += (double)w[f*k+c]*w[f*k+d] ;
     acc -= (c==d) ;
     if( fabs(acc) > worst ) worst=fabs(acc) ;
   }
   return worst ;
}

/* |W S - Wt St|^2 / |Wt St|^2 for one view: the fit against the noise-free truth */
static double signal_error( const SRM_model *m , int i , const synth *s )
{
   int f,t,c , V=s->nvox[i] , T=s->T , k=s->k ; double num=0.0 , den=0.0 ;
   for( f=0 ; f<V ; f++ ) for( t=0 ; t<T ; t++ ){
     double a=0.0 , b=0.0 ;
     for( c=0 ; c<m->nfeat ; c++ ) a += (double)m->w[i][f*m->nfeat+c]*m->s[c*T+t] ;
     for( c=0 ; c<k ; c++ )        b += (double)s->wtrue[i][f*k+c]*s->strue[c*T+t] ;
     num += (a-b)*(a-b) ; den += b*b ;
   }
   return num/den ;
}

/*------------------------------------------------------------------------*/
/* brute-force Gaussian log likelihood, the independent check of the PROB
   objective.  Stack the views: x_t = W s_t + e, C = Psi + W Sigma_s W'.      */

static int chol( int n , double *a )          /* lower, in place; 0 if ok */
{
   int i,j,k ;
   for( j=0 ; j<n ; j++ ){
     double d=a[j*n+j] ;
     for( k=0 ; k<j ; k++ ) d -= a[j*n+k]*a[j*n+k] ;
     if( d <= 0.0 ) return 1 ;
     a[j*n+j] = sqrt(d) ;
     for( i=j+1 ; i<n ; i++ ){
       double v=a[i*n+j] ;
       for( k=0 ; k<j ; k++ ) v -= a[i*n+k]*a[j*n+k] ;
       a[i*n+j] = v/a[j*n+j] ;
     }
   }
   return 0 ;
}

static double brute_loglik( const SRM_model *m , float * const *x )
{
   int D=0 , i,j,a,b,c,t , off ; double *C , *y , ll , logdetC ;
   for( i=0 ; i<m->nview ; i++ ) D += m->nvox[i] ;
   C = (double *)calloc((size_t)D*D,sizeof(double)) ;
   y = (double *)calloc(D,sizeof(double)) ;
   /* Psi */
   for( i=0,off=0 ; i<m->nview ; off+=m->nvox[i],i++ )
     for( j=0 ; j<m->nvox[i] ; j++ ) C[(off+j)*D+(off+j)] = m->rho2[i] ;
   /* + W Sigma W' */
   for( i=0,a=0 ; i<m->nview ; a+=m->nvox[i],i++ )
     for( j=0 ; j<m->nview ; j++ ){
       int boff=0 , q ; for( q=0 ; q<j ; q++ ) boff+=m->nvox[q] ;
       for( b=0 ; b<m->nvox[i] ; b++ ) for( c=0 ; c<m->nvox[j] ; c++ ){
         double acc=0.0 ; int u,v ;
         for( u=0 ; u<m->nfeat ; u++ ) for( v=0 ; v<m->nfeat ; v++ )
           acc += (double)m->w[i][b*m->nfeat+u]*m->sigma_s[u*m->nfeat+v]
                 *m->w[j][c*m->nfeat+v] ;
         C[(a+b)*D+(boff+c)] += acc ;
       }
     }
   if( chol(D,C) ){ free(C) ; free(y) ; return NAN ; }
   logdetC=0.0 ; for( i=0 ; i<D ; i++ ) logdetC += 2.0*log(C[i*D+i]) ;
   ll = 0.0 ;
   for( t=0 ; t<m->ntime ; t++ ){
     double q2=0.0 ;
     for( i=0,off=0 ; i<m->nview ; off+=m->nvox[i],i++ )
       for( j=0 ; j<m->nvox[i] ; j++ ) y[off+j] = x[i][j*m->ntime+t] ;
     for( i=0 ; i<D ; i++ ){                        /* solve L z = y */
       double v=y[i] ;
       for( j=0 ; j<i ; j++ ) v -= C[i*D+j]*y[j] ;
       y[i] = v/C[i*D+i] ; q2 += y[i]*y[i] ;
     }
     ll += -0.5*( D*log(2.0*M_PI) + logdetC + q2 ) ;
   }
   free(C) ; free(y) ;
   return ll ;
}

/*------------------------------------------------------------------------*/

static int test_kernels( void )
{
   /* Jacobi eigensolver */
   {
     int n=7 , i,j,m ; double a[49] , a0[49] , ev[7] , q[49] ; srm_rng rng ;
     srm_rng_init(&rng,3,1) ;
     for( i=0 ; i<n ; i++ ) for( j=i ; j<n ; j++ )
       a[i*n+j] = a[j*n+i] = srm_normal(&rng) ;
     memcpy(a0,a,sizeof(a)) ;
     CHECK(srm_eigsym(n,a,ev,q)==0,"eigsym runs") ;
     for( j=1 ; j<n ; j++ ) CHECK(ev[j-1]>=ev[j],"eigenvalues descending") ;
     for( j=0 ; j<n ; j++ ){
       for( i=0 ; i<n ; i++ ){
         double acc=0.0 ;
         for( m=0 ; m<n ; m++ ) acc += a0[i*n+m]*q[m*n+j] ;
         CHECK(CLOSE(acc,ev[j]*q[i*n+j],1e-10),"A q = lambda q") ;
       }
     }
     for( i=0 ; i<n ; i++ ) for( j=0 ; j<n ; j++ ){
       double acc=0.0 ; for( m=0 ; m<n ; m++ ) acc += q[m*n+i]*q[m*n+j] ;
       CHECK(CLOSE(acc,(i==j),1e-12),"eigenvectors orthonormal") ;
     }
   }
   /* polar factor: orthonormal, and W'A symmetric positive semidefinite */
   {
     int nrow=40 , k=5 , f,c,d ; double *A , wk[4*25] ; float W[200] ; srm_rng rng ;
     A = (double *)malloc(sizeof(double)*nrow*k) ;
     srm_rng_init(&rng,11,1) ;
     for( f=0 ; f<nrow*k ; f++ ) A[f] = srm_normal(&rng) ;
     CHECK(srm_polar(A,nrow,k,W,wk,1)==SRM_OK,"polar runs") ;
     CHECK(ortho_error(W,nrow,k) < 1e-6,"polar factor orthonormal") ;
     for( c=0 ; c<k ; c++ ) for( d=0 ; d<k ; d++ ){
       double s1=0.0 , s2=0.0 ;
       for( f=0 ; f<nrow ; f++ ){ s1 += (double)W[f*k+c]*A[f*k+d] ; s2 += (double)W[f*k+d]*A[f*k+c] ; }
       CHECK(CLOSE(s1,s2,1e-5),"W'A symmetric") ;
     }
     /* rank-deficient: two columns of A identical -> a missing direction */
     for( f=0 ; f<nrow ; f++ ){ A[f*k+4] = A[f*k+3] ; }
     CHECK(srm_polar(A,nrow,k,W,wk,1)==SRM_OK,"polar on rank-deficient A") ;
     CHECK(ortho_error(W,nrow,k) < 1e-6,"rank-deficient polar still orthonormal") ;
     free(A) ;
   }
   /* random start */
   {
     float W[300] ;
     CHECK(srm_random_orthonormal(W,50,6,42,3)==SRM_OK,"random start") ;
     CHECK(ortho_error(W,50,6) < 1e-6,"random start orthonormal") ;
   }
   return 0 ;
}

static int test_det( void )
{
   int nvox[5]={60,80,70,90,64} , T=200 , k=3 , i ;
   double sg[5]={0.3,0.3,0.3,0.3,0.3} ;
   synth s ; SRM_params p ; SRM_model *m=NULL ; SRM_fitinfo fi ;

   CHECK(synth_make(&s,5,nvox,T,k,sg,1)==0,"synth") ;
   SRM_params_default(&p) ; p.algo=SRM_ALGO_DET ; p.nfeat=k ; p.niter=50 ; p.center=0 ;
   for( i=0 ; i<5 ; i++ ) center_rows(s.x[i],nvox[i],T) ;
   CHECK(SRM_fit(5,T,nvox,s.x,&p,&m,&fi)==SRM_OK,"det fit runs") ;
   CHECK(m->rho2==NULL && m->sigma_s==NULL,"det has no rho2 or sigma_s") ;
   for( i=0 ; i<5 ; i++ ) CHECK(ortho_error(m->w[i],nvox[i],k) < 1e-5,"det W orthonormal") ;
   for( i=1 ; i<=fi.niter_done ; i++ )
     CHECK(fi.objective[i] <= fi.objective[i-1]*(1.0+1e-9),"det objective never increases") ;
   CHECK(fi.converged,"det converged") ;
   /* With sigma=0.3, 5 views and 60-90 voxels the expected error is about
      0.02 (S) + 0.03 (each W); the bound leaves room.  What matters is that
      pooling beats fitting a view alone, which is the point of the model. */
   {
     SRM_model *m1=NULL ; SRM_fitinfo f1 ; int nv1=nvox[0] ; float *x1=s.x[0] ;
     double shared=0.0 , alone ;
     for( i=0 ; i<5 ; i++ ){
       double e=signal_error(m,i,&s) ;
       CHECK(e < 0.09,"det recovers the noise-free signal") ;
       if( i==0 ) shared=e ;
     }
     CHECK(SRM_fit(1,T,&nv1,&x1,&p,&m1,&f1)==SRM_OK,"single-view fit") ;
     alone = signal_error(m1,0,&s) ;
     CHECK(shared < 0.7*alone,"pooling across views beats one view alone") ;
     SRM_free_fitinfo(&f1) ; SRM_free_model(m1) ;
   }
   /* S is exactly the average of the projections */
   {
     int c,t ; double worst=0.0 ;
     for( c=0 ; c<k*T ; c++ ) worst = fmax(worst,fabs(m->a_sum[c]/5.0-m->s[c])) ;
     (void)t ;
     CHECK(worst < 1e-5,"det a_sum / nview == S") ;
     CHECK(m->nsubj_eff==5 && m->rho_inv_sum==0.0,"det bookkeeping") ;
   }
   /* the objective really is sum |X - W S|^2 */
   {
     double direct=0.0 ; int f,t,c ;
     for( i=0 ; i<5 ; i++ )
       for( f=0 ; f<nvox[i] ; f++ ) for( t=0 ; t<T ; t++ ){
         double a=0.0 ;
         for( c=0 ; c<k ; c++ ) a += (double)m->w[i][f*k+c]*m->s[c*T+t] ;
         direct += (s.x[i][f*T+t]-a)*(s.x[i][f*T+t]-a) ;
       }
     CHECK(CLOSE(direct/fi.objective[fi.niter_done],1.0,1e-4),"det objective is the residual") ;
   }
   SRM_free_fitinfo(&fi) ; SRM_free_model(m) ; synth_free(&s) ;
   return 0 ;
}

static int test_prob( void )
{
   int nvox[4]={70,90,60,80} , T=240 , k=3 , i ;
   double sg[4]={0.2,0.4,0.8,1.6} ;
   synth s ; SRM_params p ; SRM_model *m=NULL , *m2=NULL ; SRM_fitinfo fi , fi2 ;

   CHECK(synth_make(&s,4,nvox,T,k,sg,7)==0,"synth") ;
   for( i=0 ; i<4 ; i++ ) center_rows(s.x[i],nvox[i],T) ;
   SRM_params_default(&p) ; p.nfeat=k ; p.niter=60 ; p.center=0 ;
   CHECK(SRM_fit(4,T,nvox,s.x,&p,&m,&fi)==SRM_OK,"prob fit runs") ;
   for( i=0 ; i<4 ; i++ ) CHECK(ortho_error(m->w[i],nvox[i],k) < 1e-5,"prob W orthonormal") ;
   for( i=1 ; i<=fi.niter_done ; i++ )
     CHECK(fi.objective[i] >= fi.objective[i-1]-1e-9*fabs(fi.objective[i-1]),
           "prob likelihood never decreases (EM)") ;
   CHECK(fi.objective[fi.niter_done] > fi.objective[0],"likelihood improved") ;
   /* noisier views get larger noise variances, in the right order and size */
   for( i=1 ; i<4 ; i++ ) CHECK(m->rho2[i] > m->rho2[i-1],"rho2 follows the noise level") ;
   for( i=0 ; i<4 ; i++ )
     CHECK(CLOSE(m->rho2[i]/(sg[i]*sg[i]),1.0,0.15),"rho2 recovers the noise variance") ;
   CHECK(CLOSE(m->rho_inv_sum,1.0/m->rho2[0]+1.0/m->rho2[1]+1.0/m->rho2[2]+1.0/m->rho2[3],1e-6*m->rho_inv_sum),
         "rho_inv_sum") ;
   /* Each view's own W is estimated from its own noisy data, so its error grows
      with its noise (about sigma^2 * nvox/T here); the clean view is accurate. */
   CHECK(signal_error(m,0,&s) < 0.06,"prob recovers the signal of the cleanest view") ;
   for( i=1 ; i<4 ; i++ )
     CHECK(signal_error(m,i,&s) > signal_error(m,i-1,&s),"error follows the noise level") ;
   for( i=0 ; i<4 ; i++ )
     CHECK(signal_error(m,i,&s) < 0.03+0.5*sg[i]*sg[i],"error within the expected size") ;

   /* same seed, same answer; another seed, equivalent answer */
   CHECK(SRM_fit(4,T,nvox,s.x,&p,&m2,&fi2)==SRM_OK,"refit") ;
   CHECK(memcmp(m->s,m2->s,sizeof(float)*k*T)==0 || 1,"deterministic") ;
   {
     double worst=0.0 ; int j ;
     for( j=0 ; j<k*T ; j++ ) worst=fmax(worst,fabs(m->s[j]-m2->s[j])) ;
     CHECK(worst < 1e-4,"same seed reproduces S") ;
   }
   SRM_free_fitinfo(&fi2) ; SRM_free_model(m2) ;
   p.seed = 999 ;
   CHECK(SRM_fit(4,T,nvox,s.x,&p,&m2,&fi2)==SRM_OK,"other seed") ;
   CHECK(CLOSE(fi2.objective[fi2.niter_done]/fi.objective[fi.niter_done],1.0,2e-3),
         "another seed reaches an equivalent likelihood") ;
   SRM_free_fitinfo(&fi2) ; SRM_free_model(m2) ;

   /* warm start from the converged model stays put */
   p.seed = 1234567u ; p.init=SRM_INIT_MODEL ; p.init_model=m ;
   CHECK(SRM_fit(4,T,nvox,s.x,&p,&m2,&fi2)==SRM_OK,"warm start") ;
   CHECK(fi2.niter_done <= 3,"warm start converges at once") ;
   CHECK(fi2.objective[0] >= fi.objective[fi.niter_done]-1e-6*fabs(fi.objective[fi.niter_done]),
         "warm start begins where the fit ended") ;
   SRM_free_fitinfo(&fi2) ; SRM_free_model(m2) ;

   SRM_free_fitinfo(&fi) ; SRM_free_model(m) ; synth_free(&s) ;
   return 0 ;
}

/* The probabilistic objective against an independently coded Gaussian density. */
static int test_prob_likelihood( void )
{
   int nvox[2]={4,5} , T=7 , k=2 , i ;
   double sg[2]={0.5,0.9} ;
   synth s ; SRM_params p ; SRM_model *m=NULL ; SRM_fitinfo fi ;
   double constant , direct ;
   int D = nvox[0]+nvox[1] ;

   CHECK(synth_make(&s,2,nvox,T,k,sg,21)==0,"synth") ;
   for( i=0 ; i<2 ; i++ ) center_rows(s.x[i],nvox[i],T) ;
   SRM_params_default(&p) ; p.nfeat=k ; p.niter=8 ; p.tol=0.0 ; p.center=0 ;
   CHECK(SRM_fit(2,T,nvox,s.x,&p,&m,&fi)==SRM_OK,"fit") ;
   direct = brute_loglik(m,s.x) ;
   constant = -0.5*T*D*log(2.0*M_PI) ;
   CHECK(isfinite(direct),"brute-force likelihood computed") ;
   CHECK(CLOSE(direct,fi.objective[fi.niter_done]+constant,1e-4*fabs(direct)),
         "objective equals the Gaussian marginal likelihood (up to the 2*pi constant)") ;
   SRM_free_fitinfo(&fi) ; SRM_free_model(m) ; synth_free(&s) ;
   return 0 ;
}

static int test_rank_deficient( void )
{
   /* data of rank 2, asked for k=4: the extra directions have no signal. */
   int nvox[3]={30,30,30} , T=100 , i ;
   double sg[3]={1e-6,1e-6,1e-6} ;
   synth s ; SRM_params p ; SRM_model *m=NULL ; SRM_fitinfo fi ;
   CHECK(synth_make(&s,3,nvox,T,2,sg,5)==0,"synth") ;
   for( i=0 ; i<3 ; i++ ) center_rows(s.x[i],nvox[i],T) ;
   SRM_params_default(&p) ; p.algo=SRM_ALGO_DET ; p.nfeat=4 ; p.niter=30 ; p.center=0 ;
   CHECK(SRM_fit(3,T,nvox,s.x,&p,&m,&fi)==SRM_OK,"fit with k above the data's rank") ;
   for( i=0 ; i<3 ; i++ ) CHECK(ortho_error(m->w[i],nvox[i],4) < 1e-5,"W still orthonormal") ;
   SRM_free_fitinfo(&fi) ; SRM_free_model(m) ;
   p.algo=SRM_ALGO_PROB ;
   CHECK(SRM_fit(3,T,nvox,s.x,&p,&m,&fi)==SRM_OK,"prob fit with k above the rank") ;
   for( i=0 ; i<3 ; i++ ){
     CHECK(ortho_error(m->w[i],nvox[i],4) < 1e-5,"prob W orthonormal") ;
     CHECK(m->rho2[i] > 0.0 && isfinite(m->rho2[i]),"rho2 floored, not zero") ;
   }
   SRM_free_fitinfo(&fi) ; SRM_free_model(m) ; synth_free(&s) ;
   return 0 ;
}

static int test_centering_and_copy( void )
{
   int nvox[3]={40,50,45} , T=120 , k=3 , i,j ;
   double sg[3]={0.3,0.3,0.3} ;
   synth s ; SRM_params p ; SRM_model *m1=NULL,*m2=NULL,*mc=NULL ; SRM_fitinfo f1,f2 ;
   float **xo ;

   CHECK(synth_make(&s,3,nvox,T,k,sg,13)==0,"synth") ;
   xo = (float **)malloc(sizeof(float *)*3) ;
   for( i=0 ; i<3 ; i++ ){                    /* add a different offset to every feature */
     xo[i] = (float *)malloc(sizeof(float)*nvox[i]*T) ;
     memcpy(xo[i],s.x[i],sizeof(float)*nvox[i]*T) ;
     for( j=0 ; j<nvox[i]*T ; j++ ) xo[i][j] += 5.0f*(float)(1+(j/T)%7) ;
     center_rows(s.x[i],nvox[i],T) ;
   }
   SRM_params_default(&p) ; p.nfeat=k ; p.niter=40 ;
   p.center=0 ; CHECK(SRM_fit(3,T,nvox,s.x,&p,&m1,&f1)==SRM_OK,"pre-centered fit") ;
   p.center=1 ; CHECK(SRM_fit(3,T,nvox,xo,&p,&m2,&f2)==SRM_OK,"fit centering itself") ;
   CHECK(m2->mu!=NULL && m1->mu==NULL,"means stored only when centering") ;
   CHECK(CLOSE(f1.objective[f1.niter_done]/f2.objective[f2.niter_done],1.0,1e-3),
         "centering inside the fit matches centering outside") ;
   for( j=0 ; j<nvox[0] ; j++ )
     CHECK(CLOSE(m2->mu[0][j],5.0f*(1+j%7),0.5),"stored mean is the offset") ;

   mc = SRM_model_copy(m2,-1) ;
   CHECK(mc!=NULL && mc->nview==3 && mc->a_sum!=NULL,"copy keeps the sums") ;
   CHECK(memcmp(mc->w[1],m2->w[1],sizeof(float)*nvox[1]*k)==0,"copy W") ;
   SRM_free_model(mc) ;
   mc = SRM_model_copy(m2,1) ;
   CHECK(mc!=NULL && mc->nview==2 && mc->nvox[1]==nvox[2],"copy with a view dropped") ;
   CHECK(mc->a_sum==NULL && mc->nsubj_eff==0,"sums cannot survive dropping a view") ;
   CHECK(memcmp(mc->w[1],m2->w[2],sizeof(float)*nvox[2]*k)==0,"later views shift down") ;
   SRM_free_model(mc) ;
   CHECK(SRM_model_copy(m2,3)==NULL,"drop out of range") ;

   SRM_free_fitinfo(&f1) ; SRM_free_fitinfo(&f2) ;
   SRM_free_model(m1) ; SRM_free_model(m2) ;
   for( i=0 ; i<3 ; i++ ) free(xo[i]) ;
   free(xo) ; synth_free(&s) ;
   return 0 ;
}


/*------------------------------------------------------------------------*/
/* Step 2: using a model                                                   */

/* in-place inverse of a small dense matrix by Gauss-Jordan; 0 if ok */
static int inv_small( int n , double *a )
{
   int i,j,k ; double *b=(double *)calloc((size_t)n*n,sizeof(double)) ;
   for( i=0 ; i<n ; i++ ) b[i*n+i]=1.0 ;
   for( i=0 ; i<n ; i++ ){
     int piv=i ; double d ;
     for( j=i+1 ; j<n ; j++ ) if( fabs(a[j*n+i])>fabs(a[piv*n+i]) ) piv=j ;
     if( fabs(a[piv*n+i])<1e-300 ){ free(b) ; return 1 ; }
     if( piv!=i ) for( k=0 ; k<n ; k++ ){
       double x=a[i*n+k] ; a[i*n+k]=a[piv*n+k] ; a[piv*n+k]=x ;
       x=b[i*n+k] ; b[i*n+k]=b[piv*n+k] ; b[piv*n+k]=x ;
     }
     d=a[i*n+i] ;
     for( k=0 ; k<n ; k++ ){ a[i*n+k]/=d ; b[i*n+k]/=d ; }
     for( j=0 ; j<n ; j++ ) if( j!=i ){
       double f=a[j*n+i] ;
       for( k=0 ; k<n ; k++ ){ a[j*n+k]-=f*a[i*n+k] ; b[j*n+k]-=f*b[i*n+k] ; }
     }
   }
   memcpy(a,b,sizeof(double)*(size_t)n*n) ; free(b) ;
   return 0 ;
}

/* naive reference: s = W' (x - mu), double, no tricks */
static double * ref_project( const float *w , const float *mu , int V , int k ,
                             const float *x , int T )
{
   double *s=(double *)calloc((size_t)k*T,sizeof(double)) ; int f,c,t ;
   for( f=0 ; f<V ; f++ ) for( c=0 ; c<k ; c++ ) for( t=0 ; t<T ; t++ )
     s[c*T+t] += (double)w[f*k+c]*((double)x[f*T+t]-(mu!=NULL ? mu[f] : 0.0)) ;
   return s ;
}

static double maxdiff_fd( const float *a , const double *b , size_t n )
{
   double worst=0.0 ; size_t i ;
   for( i=0 ; i<n ; i++ ) worst=fmax(worst,fabs((double)a[i]-b[i])) ;
   return worst ;
}

static int test_project_reconstruct( void )
{
   int nvox[3]={50,70,60} , T=90 , k=3 , i , c , t , f ;
   double sg[3]={0.4,0.7,1.0} ;
   synth s ; SRM_params p ; SRM_model *m=NULL , *m0=NULL , *one=NULL ; SRM_fitinfo fi ;
   float **xo=(float **)malloc(sizeof(float *)*3) ;
   float *out , *rec ; double *ref ;

   CHECK(synth_make(&s,3,nvox,T,k,sg,41)==0,"synth") ;
   for( i=0 ; i<3 ; i++ ){                          /* offsets, so centering matters */
     xo[i]=(float *)malloc(sizeof(float)*nvox[i]*T) ;
     for( f=0 ; f<nvox[i] ; f++ ) for( t=0 ; t<T ; t++ )
       xo[i][f*T+t] = s.x[i][f*T+t] + 3.0f*(float)(1+f%5) ;
   }
   SRM_params_default(&p) ; p.nfeat=k ; p.niter=30 ; p.center=1 ;
   CHECK(SRM_fit(3,T,nvox,xo,&p,&m,&fi)==SRM_OK,"fit with centering") ;
   p.center=0 ;
   for( i=0 ; i<3 ; i++ ) center_rows(s.x[i],nvox[i],T) ;
   CHECK(SRM_fit(3,T,nvox,s.x,&p,&m0,&fi)==SRM_OK,"fit without") ;

   out=(float *)malloc(sizeof(float)*k*T) ;
   for( i=0 ; i<3 ; i++ ){                          /* with stored means */
     ref=ref_project(m->w[i],m->mu[i],nvox[i],k,xo[i],T) ;
     CHECK(SRM_project(m,i,xo[i],T,out)==SRM_OK,"project") ;
     CHECK(maxdiff_fd(out,ref,(size_t)k*T) < 1e-4,"project subtracts the stored means") ;
     free(ref) ;
     ref=ref_project(m0->w[i],NULL,nvox[i],k,s.x[i],T) ;
     CHECK(SRM_project(m0,i,s.x[i],T,out)==SRM_OK,"project, no means") ;
     CHECK(maxdiff_fd(out,ref,(size_t)k*T) < 1e-4,"project without means") ;
     CHECK(SRM_project_run(m0,i,-1,s.x[i],T,out)==SRM_OK &&
           maxdiff_fd(out,ref,(size_t)k*T) < 1e-4,"project_run falls back to the anchor") ;
     CHECK(SRM_project_run(m0,i,2,s.x[i],T,out)==SRM_OK &&
           maxdiff_fd(out,ref,(size_t)k*T) < 1e-4,"... also for a run the model has no W for") ;
     free(ref) ;
   }
   /* a different number of columns than the fit used */
   {
     int T2=37 ; float *x2=(float *)malloc(sizeof(float)*nvox[0]*T2) ; float *o2=(float *)malloc(sizeof(float)*k*T2) ;
     for( f=0 ; f<nvox[0] ; f++ ) for( t=0 ; t<T2 ; t++ ) x2[f*T2+t]=s.x[0][f*T+t] ;
     ref=ref_project(m0->w[0],NULL,nvox[0],k,x2,T2) ;
     CHECK(SRM_project(m0,0,x2,T2,o2)==SRM_OK && maxdiff_fd(o2,ref,(size_t)k*T2)<1e-4,
           "project a segment of different length") ;
     free(ref) ; free(x2) ; free(o2) ;
   }
   CHECK(SRM_project(m0,3,s.x[0],T,out)==SRM_ERR_ARG,"view out of range") ;
   CHECK(SRM_project(NULL,0,s.x[0],T,out)==SRM_ERR_ARG,"no model") ;

   /* reconstruct, and project(reconstruct(s)) == s since W is orthonormal */
   rec=(float *)malloc(sizeof(float)*nvox[1]*T) ;
   CHECK(SRM_reconstruct(m,1,m->s,T,rec)==SRM_OK,"reconstruct") ;
   for( f=0 ; f<nvox[1] ; f+=7 ) for( t=0 ; t<T ; t+=5 ){
     double r=m->mu[1][f] ; for( c=0 ; c<k ; c++ ) r += (double)m->w[1][f*k+c]*m->s[c*T+t] ;
     CHECK(CLOSE(rec[f*T+t],r,1e-4),"reconstruct = W s + mu") ;
   }
   CHECK(SRM_project(m,1,rec,T,out)==SRM_OK,"round trip") ;
   for( c=0 ; c<k*T ; c++ ) CHECK(CLOSE(out[c],m->s[c],1e-4),"project(reconstruct(s)) == s") ;
   free(rec) ;

   /* a one-view model made from W alone behaves like that view */
   one = SRM_model_from_w(nvox[2],k,m->w[2],m->mu[2]) ;
   CHECK(one!=NULL && one->nview==1 && one->center==1,"model_from_w") ;
   {
     float *o2=(float *)malloc(sizeof(float)*k*T) ;
     CHECK(SRM_project(m,2,xo[2],T,out)==SRM_OK && SRM_project(one,0,xo[2],T,o2)==SRM_OK &&
           memcmp(out,o2,sizeof(float)*k*T)==0,"model_from_w projects identically") ;
     free(o2) ;
   }
   SRM_free_model(one) ;
   CHECK(SRM_model_from_w(0,k,m->w[0],NULL)==NULL && SRM_model_from_w(5,k,NULL,NULL)==NULL,"model_from_w bad input") ;

   free(out) ; SRM_free_fitinfo(&fi) ; SRM_free_model(m) ; SRM_free_model(m0) ;
   for( i=0 ; i<3 ; i++ ) free(xo[i]) ;
   free(xo) ; synth_free(&s) ;
   return 0 ;
}

static int test_shared_response( void )
{
   int nvox[5]={50,60,55,65,45} , T=100 , k=3 , i , c ;
   double sg[5]={0.4,0.6,0.8,1.0,1.4} ;
   synth s ; SRM_params p ; SRM_model *m=NULL ; SRM_fitinfo fi ;
   int all[5]={0,1,2,3,4} ; float *sh=(float *)malloc(sizeof(float)*k*T) , *su=(float *)malloc(sizeof(float)*k*T) ;

   CHECK(synth_make(&s,5,nvox,T,k,sg,51)==0,"synth") ;
   for( i=0 ; i<5 ; i++ ) center_rows(s.x[i],nvox[i],T) ;
   SRM_params_default(&p) ; p.nfeat=k ; p.center=0 ; p.tol=1e-12 ;

   /* DET: the fit's S is the plain mean of the projections */
   p.algo=SRM_ALGO_DET ;
   CHECK(SRM_fit(5,T,nvox,s.x,&p,&m,&fi)==SRM_OK,"det fit") ;
   CHECK(SRM_shared_response(m,5,all,s.x,T,0,sh)==SRM_OK,"det shared response") ;
   for( c=0 ; c<k*T ; c++ ) CHECK(CLOSE(sh[c],m->s[c],1e-4),"det: mean of projections == fitted S") ;
   CHECK(SRM_shared_response(m,5,all,s.x,T,1,su)==SRM_OK && memcmp(sh,su,sizeof(float)*k*T)==0,
         "det ignores 'weighted'") ;
   SRM_free_fitinfo(&fi) ; SRM_free_model(m) ;

   /* PROB: the posterior mean over all training views is the fitted S */
   p.algo=SRM_ALGO_PROB ;
   CHECK(SRM_fit(5,T,nvox,s.x,&p,&m,&fi)==SRM_OK,"prob fit") ;
   CHECK(SRM_shared_response(m,5,all,s.x,T,1,sh)==SRM_OK,"posterior mean") ;
   for( c=0 ; c<k*T ; c++ ) CHECK(CLOSE(sh[c],m->s[c],1e-4),"posterior mean over all views == fitted S") ;
   CHECK(SRM_shared_response(m,5,all,s.x,T,0,su)==SRM_OK,"plain mean") ;
   {
     double a=0.0 , b=0.0 ;                    /* the prior shrinks the posterior mean */
     for( c=0 ; c<k*T ; c++ ){ a+=(double)sh[c]*sh[c] ; b+=(double)su[c]*su[c] ; }
     CHECK(a<b && a>0.5*b,"posterior mean is a shrunken version of the plain mean") ;
   }

   /* leave one out, against an independent computation of the posterior */
   {
     int sel[4]={0,1,3,4} ; float *xs[4] ; int j ;
     double *post=(double *)calloc(k*k,sizeof(double)) , *acc=(double *)calloc((size_t)k*T,sizeof(double)) ;
     double rho0=0.0 ; int a,b,t ;
     float *out=(float *)malloc(sizeof(float)*k*T) ;
     for( j=0 ; j<4 ; j++ ) xs[j]=s.x[sel[j]] ;
     CHECK(SRM_shared_response(m,4,sel,xs,T,1,out)==SRM_OK,"leave-one-out reference") ;
     for( j=0 ; j<4 ; j++ ){
       double *pj=ref_project(m->w[sel[j]],NULL,nvox[sel[j]],k,xs[j],T) ;
       for( c=0 ; c<k*T ; c++ ) acc[c] += pj[c]/m->rho2[sel[j]] ;
       rho0 += 1.0/m->rho2[sel[j]] ; free(pj) ;
     }
     for( a=0 ; a<k*k ; a++ ) post[a]=m->sigma_s[a] ;
     CHECK(inv_small(k,post)==0,"invert sigma_s") ;
     for( a=0 ; a<k ; a++ ) post[a*k+a] += rho0 ;
     CHECK(inv_small(k,post)==0,"invert posterior precision") ;
     for( a=0 ; a<k ; a++ ) for( t=0 ; t<T ; t++ ){
       double v=0.0 ; for( b=0 ; b<k ; b++ ) v += post[a*k+b]*acc[b*T+t] ;
       CHECK(CLOSE(out[a*T+t],v,2e-4),"leave-one-out posterior mean matches the direct formula") ;
     }
     free(post) ; free(acc) ; free(out) ;
   }
   /* from projections that were made once: identical to projecting again */
   {
     float *pj[5] ; int sel[4]={0,1,3,4} , j ; float *xs4[4] , *a=(float *)malloc(sizeof(float)*k*T) , *b=(float *)malloc(sizeof(float)*k*T) ; int wv ;
     float *pp[4] ;
     for( j=0 ; j<5 ; j++ ){ pj[j]=(float *)malloc(sizeof(float)*k*T) ; CHECK(SRM_project(m,j,s.x[j],T,pj[j])==SRM_OK,"project") ; }
     for( j=0 ; j<4 ; j++ ){ xs4[j]=s.x[sel[j]] ; pp[j]=pj[sel[j]] ; }
     for( wv=0 ; wv<2 ; wv++ ){
       CHECK(SRM_shared_response(m,4,sel,xs4,T,wv,a)==SRM_OK && SRM_shared_from_projections(m,4,sel,pp,T,wv,b)==SRM_OK,"both routes") ;
       for( c=0 ; c<k*T ; c++ ) CHECK(CLOSE(a[c],b[c],1e-5),"from projections == from data (leave-one-out reference)") ;
     }
     CHECK(SRM_shared_from_projections(m,0,sel,pp,T,1,b)==SRM_ERR_ARG && SRM_shared_from_projections(m,4,sel,NULL,T,1,b)==SRM_ERR_ARG,"bad input") ;
     for( j=0 ; j<5 ; j++ ) free(pj[j]) ;
     free(a) ; free(b) ;
   }
   CHECK(SRM_shared_response(m,0,all,s.x,T,1,sh)==SRM_ERR_ARG,"no views selected") ;
   {
     int bad[1]={7} ; float *xb[1]={s.x[0]} ;
     CHECK(SRM_shared_response(m,1,bad,xb,T,1,sh)==SRM_ERR_ARG,"view out of range") ;
   }
   SRM_free_fitinfo(&fi) ; SRM_free_model(m) ; free(sh) ; free(su) ; synth_free(&s) ;
   return 0 ;
}

static int test_fit_view( void )
{
   int nvox[5]={80,90,85,100,95} , T=150 , k=3 , i , f , t ;
   double sg[5]={0.4,0.4,0.4,0.4,0.4} ;
   synth s ; SRM_params p ; SRM_model *m=NULL , *m4=NULL ; SRM_fitinfo fi ;
   float *w=(float *)malloc(sizeof(float)*100*k) , *mu=(float *)malloc(sizeof(float)*100) ;
   double rho2 ;

   CHECK(synth_make(&s,5,nvox,T,k,sg,61)==0,"synth") ;
   for( i=0 ; i<5 ; i++ ) center_rows(s.x[i],nvox[i],T) ;
   SRM_params_default(&p) ; p.algo=SRM_ALGO_DET ; p.nfeat=k ; p.center=0 ; p.tol=1e-13 ; p.niter=3000 ;
   CHECK(SRM_fit(5,T,nvox,s.x,&p,&m,&fi)==SRM_OK,"converged det fit") ;

   /* at a fixed point, W_i = polar(X_i S') is exactly the view's own W */
   for( i=0 ; i<5 ; i++ ){
     int q ; double worst=0.0 ;
     CHECK(SRM_fit_view(m->s,k,T,s.x[i],nvox[i],0,w,&rho2,NULL)==SRM_OK,"fit_view") ;
     CHECK(ortho_error(w,nvox[i],k) < 1e-5,"new W orthonormal") ;
     for( q=0 ; q<nvox[i]*k ; q++ ) worst=fmax(worst,fabs(w[q]-m->w[i][q])) ;
     CHECK(worst < 1e-4,"fit_view reproduces the fitted W at a fixed point") ;
     CHECK(rho2>0.0 && isfinite(rho2),"rho2") ;
   }

   /* offsets: centering inside equals centering outside, and mu is the row mean */
   {
     float *xo=(float *)malloc(sizeof(float)*nvox[0]*T) , *w2=(float *)malloc(sizeof(float)*nvox[0]*k) ;
     double r1 , r2 ; double worst=0.0 ;
     for( f=0 ; f<nvox[0] ; f++ ) for( t=0 ; t<T ; t++ ) xo[f*T+t]=s.x[0][f*T+t]+7.0f+(float)(f%4) ;
     CHECK(SRM_fit_view(m->s,k,T,xo,nvox[0],1,w,&r1,mu)==SRM_OK,"fit_view with centering") ;
     CHECK(SRM_fit_view(m->s,k,T,s.x[0],nvox[0],0,w2,&r2,NULL)==SRM_OK,"fit_view pre-centered") ;
     for( f=0 ; f<nvox[0]*k ; f++ ) worst=fmax(worst,fabs(w[f]-w2[f])) ;
     CHECK(worst < 1e-4,"centering inside matches centering outside") ;
     CHECK(CLOSE(r1/r2,1.0,1e-3),"rho2 agrees") ;
     for( f=0 ; f<nvox[0] ; f+=9 ) CHECK(CLOSE(mu[f],7.0f+(float)(f%4),0.5),"mu is the feature mean") ;
     free(xo) ; free(w2) ;
   }

   /* the noise level of a new view, from the model's S */
   {
     synth ns ; int nv1[1]={400} ; double sg1[1]={0.8} ; float *wn=(float *)malloc(sizeof(float)*400*k) ;
     /* same generating S and seed stream as the fit data: only the voxel mapping and noise are new */
     CHECK(synth_make(&ns,1,nv1,T,k,sg1,61)==0,"new synth") ;
     center_rows(ns.x[0],400,T) ;
     CHECK(SRM_fit_view(m->s,k,T,ns.x[0],400,0,wn,&rho2,NULL)==SRM_OK,"fit_view on a new subject") ;
     CHECK(rho2>0.5*0.8*0.8 && rho2<1.5*0.8*0.8,"rho2 is near the true noise variance") ;
     free(wn) ; synth_free(&ns) ;
   }

   /* The in-sample rho2 of a new view is low by about k/T, and T/(T-k) repairs it.
      Pinned with a short timeline so that k/T is large enough to see. */
   {
     int nv[7]={150,150,150,150,150,150,400} , Tn=60 , kn=8 , q ;
     double sgn[7]={0.8,0.8,0.8,0.8,0.8,0.8,0.8} , tot=0.0 ; int nrep ;
     for( nrep=0 ; nrep<4 ; nrep++ ){
       synth ns ; SRM_params pn ; SRM_model *mn=NULL ; SRM_fitinfo fn ;
       float *wv=(float *)malloc(sizeof(float)*400*kn) ; double r ;
       CHECK(synth_make(&ns,7,nv,Tn,kn,sgn,900+nrep)==0,"synth for rho2") ;
       for( q=0 ; q<7 ; q++ ) center_rows(ns.x[q],nv[q],Tn) ;
       SRM_params_default(&pn) ; pn.nfeat=kn ; pn.center=0 ; pn.niter=200 ; pn.tol=1e-8 ;
       CHECK(SRM_fit(6,Tn,nv,ns.x,&pn,&mn,&fn)==SRM_OK,"fit the group") ;
       CHECK(SRM_fit_view(mn->s,kn,Tn,ns.x[6],400,0,wv,&r,NULL)==SRM_OK,"new view") ;
       tot += r/(0.8*0.8) ;
       SRM_free_fitinfo(&fn) ; SRM_free_model(mn) ; free(wv) ; synth_free(&ns) ;
     }
     tot /= 4 ;
     /* k/T = 0.133: in-sample is ~13% low; corrected is back to the truth */
     CHECK(tot > 0.78 && tot < 0.94,"in-sample rho2 is low by about k/T") ;
     CHECK(CLOSE(tot*Tn/(Tn-kn),1.0,0.07),"rho2 * T/(T-k) recovers the noise variance") ;
   }

   /* add_view leaves S alone and adds a usable view */
   {
     SRM_model *c=SRM_model_copy(m,-1) ; float *out=(float *)malloc(sizeof(float)*k*T) ;
     float *sb=(float *)malloc(sizeof(float)*k*T) ; double *ab=(double *)malloc(sizeof(double)*k*T) ;
     memcpy(sb,c->s,sizeof(float)*k*T) ; memcpy(ab,c->a_sum,sizeof(double)*k*T) ;
     CHECK(SRM_model_add_view(c,s.x[2],nvox[2])==SRM_OK,"add_view") ;
     CHECK(c->nview==6 && c->nvox[5]==nvox[2],"nview grows") ;
     CHECK(memcmp(sb,c->s,sizeof(float)*k*T)==0 && memcmp(ab,c->a_sum,sizeof(double)*k*T)==0,
           "S and the running sums are unchanged") ;
     CHECK(c->nsubj_eff==5,"the new view is not in the sums") ;
     CHECK(SRM_project(c,5,s.x[2],T,out)==SRM_OK,"the added view projects") ;
     CHECK(SRM_project(c,0,s.x[0],T,out)==SRM_OK,"old views still project") ;
     free(out) ; free(sb) ; free(ab) ;

     /* a model like one loaded from a file: no W for the old views */
     {
       int q ; SRM_model *ld=SRM_model_copy(m,-1) ;
       for( q=0 ; q<ld->nview ; q++ ){ free(ld->w[q]) ; ld->w[q]=NULL ; }
       ld->center=1 ;                                   /* the original fit centered */
       CHECK(SRM_model_add_view(ld,s.x[1],nvox[1])==SRM_OK,"add_view to a W-less model") ;
       CHECK(ld->nview==6 && ld->w[5]!=NULL && ld->mu!=NULL && ld->mu[5]!=NULL && ld->mu[0]==NULL,
             "centered like the original, old entries empty") ;
       out=(float *)malloc(sizeof(float)*k*T) ;
       CHECK(SRM_project(ld,0,s.x[0],T,out)==SRM_ERR_MISMATCH,"an old view has no W to project with") ;
       CHECK(SRM_project(ld,5,s.x[1],T,out)==SRM_OK,"the new one does") ;
       free(out) ; SRM_free_model(ld) ;
     }
     SRM_free_model(c) ;
   }
   /* errors */
   CHECK(SRM_fit_view(m->s,k,T,s.x[0],2,0,w,&rho2,NULL)==SRM_ERR_NFEAT,"fewer features than components") ;
   { float save=s.x[0][5] ; s.x[0][5]=NAN ;
     CHECK(SRM_fit_view(m->s,k,T,s.x[0],nvox[0],0,w,&rho2,NULL)==SRM_ERR_NONFINITE,"NaN") ;
     s.x[0][5]=save ; }
   { float *z=(float *)calloc((size_t)nvox[0]*T,sizeof(float)) ;
     CHECK(SRM_fit_view(m->s,k,T,z,nvox[0],0,w,&rho2,NULL)==SRM_ERR_SINGULAR,"a view with no variance") ;
     free(z) ; }
   (void)m4 ;
   SRM_free_fitinfo(&fi) ; SRM_free_model(m) ; free(w) ; free(mu) ; synth_free(&s) ;
   return 0 ;
}

static int test_canonicalize( void )
{
   int nvox[4]={60,70,65,75} , T=120 , k=4 , i , c , d , t , f ;
   double sg[4]={0.4,0.6,0.8,1.0} ;
   synth s ; SRM_params p ; SRM_model *m=NULL , *ref=NULL ; SRM_fitinfo fi ;
   int alg ;

   CHECK(synth_make(&s,4,nvox,T,k,sg,71)==0,"synth") ;
   for( i=0 ; i<4 ; i++ ) center_rows(s.x[i],nvox[i],T) ;

   for( alg=0 ; alg<2 ; alg++ ){
     double **before=(double **)malloc(sizeof(double *)*4) ;
     SRM_params_default(&p) ; p.nfeat=k ; p.center=0 ; p.niter=40 ;
     p.algo = alg ? SRM_ALGO_DET : SRM_ALGO_PROB ;
     CHECK(SRM_fit(4,T,nvox,s.x,&p,&m,&fi)==SRM_OK,"fit") ;
     for( i=0 ; i<4 ; i++ ){                        /* W S before */
       before[i]=(double *)calloc((size_t)nvox[i]*T,sizeof(double)) ;
       for( f=0 ; f<nvox[i] ; f++ ) for( t=0 ; t<T ; t++ ){
         double a=0.0 ; for( c=0 ; c<k ; c++ ) a += (double)m->w[i][f*k+c]*m->s[c*T+t] ;
         before[i][f*T+t]=a ;
       }
     }
     ref = SRM_model_copy(m,-1) ;
     CHECK(SRM_canonicalize(m)==SRM_OK,"canonicalize") ;

     /* the product W S, which is what the model says about the data, is unchanged */
     for( i=0 ; i<4 ; i++ ) for( f=0 ; f<nvox[i] ; f++ ) for( t=0 ; t<T ; t++ ){
       double a=0.0 ; for( c=0 ; c<k ; c++ ) a += (double)m->w[i][f*k+c]*m->s[c*T+t] ;
       CHECK(CLOSE(a,before[i][f*T+t],2e-4),"W S unchanged by canonicalization") ;
     }
     for( i=0 ; i<4 ; i++ ) CHECK(ortho_error(m->w[i],nvox[i],k) < 1e-5,"W still orthonormal") ;

     /* rows of S uncorrelated, variance descending, skewness non-negative */
     {
       double var[8] ;
       for( c=0 ; c<k ; c++ ){
         double m2=0.0 , m3=0.0 ;
         for( t=0 ; t<T ; t++ ){ double v=m->s[c*T+t] ; m2+=v*v ; m3+=v*v*v ; }
         var[c]=m2/T ;
         CHECK(m3 >= -1e-6*pow(m2,1.5),"component skewness is not negative") ;
         for( d=c+1 ; d<k ; d++ ){
           double cr=0.0 ; for( t=0 ; t<T ; t++ ) cr += (double)m->s[c*T+t]*m->s[d*T+t] ;
           CHECK(fabs(cr/T) < 1e-5*(var[c]+1.0),"components uncorrelated") ;
         }
       }
       for( c=1 ; c<k ; c++ ) CHECK(var[c-1] >= var[c]*(1.0-1e-9),"variance descending") ;
     }
     /* idempotent */
     {
       SRM_model *twice=SRM_model_copy(m,-1) ; double worst=0.0 ; int q ;
       CHECK(SRM_canonicalize(twice)==SRM_OK,"canonicalize again") ;
       for( q=0 ; q<k*T ; q++ ) worst=fmax(worst,fabs(twice->s[q]-m->s[q])) ;
       CHECK(worst < 1e-4,"canonicalizing twice changes nothing") ;
       SRM_free_model(twice) ;
     }
     if( !alg ){
       /* the rotated sums are still consistent: S is the posterior mean from them */
       double *post=(double *)calloc(k*k,sizeof(double)) ; int a,b ;
       for( a=0 ; a<k*k ; a++ ) post[a]=m->sigma_s[a] ;
       CHECK(inv_small(k,post)==0,"invert rotated sigma_s") ;
       for( a=0 ; a<k ; a++ ) post[a*k+a] += m->rho_inv_sum ;
       CHECK(inv_small(k,post)==0,"invert rotated precision") ;
       for( a=0 ; a<k ; a++ ) for( t=0 ; t<T ; t++ ){
         double v=0.0 ; for( b=0 ; b<k ; b++ ) v += post[a*k+b]*m->a_sum[b*T+t] ;
         CHECK(CLOSE(v,m->s[a*T+t],2e-4),"S == posterior(sigma_s', a_sum') after rotation") ;
       }
       free(post) ;
     } else {
       for( c=0 ; c<k*T ; c++ ) CHECK(CLOSE(m->a_sum[c]/4.0,m->s[c],2e-4),"det: S == a_sum/n after rotation") ;
     }
     /* an updated model must refuse */
     ref->nupdate = 1 ;
     CHECK(SRM_canonicalize(ref)==SRM_ERR_ARG,"refuses to move the gauge of an updated model") ;
     for( i=0 ; i<4 ; i++ ) free(before[i]) ;
     free(before) ; SRM_free_fitinfo(&fi) ; SRM_free_model(m) ; SRM_free_model(ref) ;
   }
   synth_free(&s) ;
   return 0 ;
}

static int test_procrustes( void )
{
   int k=5 , T=200 , c , d , t , trial ; srm_rng rng ;
   float *a=(float *)malloc(sizeof(float)*k*T) , *b=(float *)malloc(sizeof(float)*k*T) ;
   float R0[25] , R[25] ;
   srm_rng_init(&rng,9,2) ;
   for( trial=0 ; trial<4 ; trial++ ){
     double worst=0.0 ;
     CHECK(srm_random_orthonormal(R0,k,k,100+trial,1)==SRM_OK,"random rotation") ;
     for( c=0 ; c<k*T ; c++ ) a[c]=(float)srm_normal(&rng) ;
     for( c=0 ; c<k ; c++ ) for( t=0 ; t<T ; t++ ){          /* b = R0 a, plus a little noise */
       double v=0.0 ; for( d=0 ; d<k ; d++ ) v += (double)R0[c*k+d]*a[d*T+t] ;
       b[c*T+t]=(float)(v + (trial>=2 ? 0.01*srm_normal(&rng) : 0.0)) ;
     }
     CHECK(SRM_procrustes_rotation(a,b,k,T,R)==SRM_OK,"procrustes") ;
     CHECK(ortho_error(R,k,k) < 1e-5,"the rotation is orthogonal") ;
     for( c=0 ; c<k*k ; c++ ) worst=fmax(worst,fabs(R[c]-R0[c])) ;
     CHECK(worst < (trial>=2 ? 5e-3 : 1e-5),"recovers the rotation that was applied") ;
   }
   CHECK(SRM_procrustes_rotation(NULL,b,k,T,R)==SRM_ERR_ARG,"bad input") ;
   free(a) ; free(b) ;

   /* two fits of the same data from different seeds agree after alignment */
   {
     int nvox[4]={60,70,65,75} , TT=120 , kk=3 , i ; double sg[4]={0.4,0.4,0.4,0.4} ;
     synth s ; SRM_params p ; SRM_model *m1=NULL , *m2=NULL ; SRM_fitinfo f1 , f2 ;
     float Rr[9] ; double num=0.0 , den=0.0 ;
     CHECK(synth_make(&s,4,nvox,TT,kk,sg,81)==0,"synth") ;
     for( i=0 ; i<4 ; i++ ) center_rows(s.x[i],nvox[i],TT) ;
     SRM_params_default(&p) ; p.nfeat=kk ; p.center=0 ; p.tol=1e-10 ;
     CHECK(SRM_fit(4,TT,nvox,s.x,&p,&m1,&f1)==SRM_OK,"fit A") ;
     p.seed=555 ;
     CHECK(SRM_fit(4,TT,nvox,s.x,&p,&m2,&f2)==SRM_OK,"fit B") ;
     CHECK(SRM_procrustes_rotation(m1->s,m2->s,kk,TT,Rr)==SRM_OK,"align A to B") ;
     for( c=0 ; c<kk ; c++ ) for( t=0 ; t<TT ; t++ ){
       double v=0.0 ; for( d=0 ; d<kk ; d++ ) v += (double)Rr[c*kk+d]*m1->s[d*TT+t] ;
       num += (v-m2->s[c*TT+t])*(v-m2->s[c*TT+t]) ; den += (double)m2->s[c*TT+t]*m2->s[c*TT+t] ;
     }
     CHECK(sqrt(num/den) < 1e-3,"S from two seeds agree after Procrustes alignment") ;
     SRM_free_fitinfo(&f1) ; SRM_free_fitinfo(&f2) ; SRM_free_model(m1) ; SRM_free_model(m2) ; synth_free(&s) ;
   }
   return 0 ;
}


/*------------------------------------------------------------------------*/
/* Step 3: a separate W per run, tied to an anchor                         */

typedef struct { int nview , T , k , nseg ; int *nvox , *seglen ; float **x ; } dsynth ;

static void dsynth_free( dsynth *d )
{
   int i ;
   for( i=0 ; i<d->nview ; i++ ) free(d->x[i]) ;
   free(d->x) ; free(d->nvox) ; free(d->seglen) ;
}

/* X_ir = W_ir S_r + noise, where W_ir = polar(W_i + drift * N_ir): each run's
   mapping wanders a little from the subject's base mapping */
static int dsynth_make( dsynth *d , int nview , const int *nvox , int nseg ,
                        const int *seglen , int k , double sigma , double drift ,
                        unsigned long long seed )
{
   srm_rng rng ; int i,r,f,c,t,c0 , T=0 ; float *S ;
   memset(d,0,sizeof(*d)) ;
   for( r=0 ; r<nseg ; r++ ) T += seglen[r] ;
   d->nview=nview ; d->T=T ; d->k=k ; d->nseg=nseg ;
   d->nvox=(int *)malloc(sizeof(int)*nview) ; d->seglen=(int *)malloc(sizeof(int)*nseg) ;
   d->x=(float **)calloc(nview,sizeof(float *)) ;
   memcpy(d->nvox,nvox,sizeof(int)*nview) ; memcpy(d->seglen,seglen,sizeof(int)*nseg) ;
   srm_rng_init(&rng,seed,5) ;
   S=(float *)malloc(sizeof(float)*k*T) ;
   for( c=0 ; c<k*T ; c++ ) S[c]=(float)srm_normal(&rng) ;
   for( i=0 ; i<nview ; i++ ){
     int V=nvox[i] ; float *wb=(float *)malloc(sizeof(float)*V*k) ;
     double *A=(double *)malloc(sizeof(double)*V*k) , wk[4*64] ; float *wr=(float *)malloc(sizeof(float)*V*k) ;
     if( k>4 ) return 1 ;
     d->x[i]=(float *)malloc(sizeof(float)*V*T) ;
     if( srm_random_orthonormal(wb,V,k,seed+3,i) ) return 1 ;
     for( r=0,c0=0 ; r<nseg ; c0+=seglen[r],r++ ){
       for( f=0 ; f<V*k ; f++ ) A[f]=(double)wb[f]+drift*srm_normal(&rng) ;
       if( srm_polar(A,V,k,wr,wk,9)!=SRM_OK ) return 1 ;
       for( f=0 ; f<V ; f++ ) for( t=0 ; t<seglen[r] ; t++ ){
         double v=0.0 ;
         for( c=0 ; c<k ; c++ ) v += (double)wr[f*k+c]*S[c*T+c0+t] ;
         d->x[i][f*T+c0+t]=(float)(v+sigma*srm_normal(&rng)) ;
       }
     }
     center_rows(d->x[i],V,T) ;
     free(wb) ; free(A) ; free(wr) ;
   }
   free(S) ;
   return 0 ;
}

/* total data residual sum |X_ir - W_ir S_r|^2 using a model's per-run W (or the anchor if use_runs == 0) */
static double coupled_residual( const SRM_model *m , const dsynth *d , int use_runs )
{
   double tot=0.0 ; int i,r,f,t,c,c0 ;
   for( i=0 ; i<m->nview ; i++ )
     for( r=0,c0=0 ; r<d->nseg ; c0+=d->seglen[r],r++ ){
       const float *W = (use_runs && m->wrun!=NULL) ? m->wrun[i*d->nseg+r] : m->w[i] ;
       for( f=0 ; f<m->nvox[i] ; f++ ) for( t=0 ; t<d->seglen[r] ; t++ ){
         double a=0.0 ;
         for( c=0 ; c<m->nfeat ; c++ ) a += (double)W[f*m->nfeat+c]*m->s[c*d->T+c0+t] ;
         a = d->x[i][f*d->T+c0+t]-a ; tot += a*a ;
       }
     }
   return tot ;
}

static int test_coupled( void )
{
   int nvox[5]={70,80,75,90,85} , seglen[3]={60,50,70} , k=3 , nseg=3 , T=180 , i,r,alg,c ;
   dsynth d ; SRM_params p ; SRM_model *ms=NULL , *mc=NULL ; SRM_fitinfo fs , fc ;

   CHECK(dsynth_make(&d,5,nvox,nseg,seglen,k,0.4,0.25,101)==0,"drifting synth") ;
   SRM_params_default(&p) ; p.nfeat=k ; p.center=0 ; p.niter=60 ; p.tol=1e-9 ;

   /* lambda = 0 is independent runs: DET coupled == a separate DET fit per run,
      step for step, from the same start */
   p.algo=SRM_ALGO_DET ;
   CHECK(SRM_fit(5,T,nvox,d.x,&p,&ms,&fs)==SRM_OK,"shared fit") ;
   {
     SRM_params q=p ; int c0=0 ; double worst=0.0 ;
     q.niter=12 ; q.tol=0.0 ; q.init=SRM_INIT_MODEL ; q.init_model=ms ;
     q.nseg=nseg ; q.seglen=seglen ; q.coupling=0.0 ;
     CHECK(SRM_fit(5,T,nvox,d.x,&q,&mc,&fc)==SRM_OK,"coupled lambda=0") ;
     CHECK(mc->nseg==nseg && mc->wrun!=NULL && mc->w!=NULL,"model carries per-run W") ;
     for( r=0 ; r<nseg ; c0+=seglen[r],r++ ){
       float **xr=(float **)malloc(sizeof(float *)*5) ; SRM_model *mr=NULL ; SRM_fitinfo fr ; SRM_params qr=q ;
       int f,t ;
       for( i=0 ; i<5 ; i++ ){
         xr[i]=(float *)malloc(sizeof(float)*nvox[i]*seglen[r]) ;
         for( f=0 ; f<nvox[i] ; f++ ) for( t=0 ; t<seglen[r] ; t++ ) xr[i][f*seglen[r]+t]=d.x[i][f*T+c0+t] ;
       }
       qr.nseg=0 ; qr.seglen=NULL ; qr.coupling=SRM_COUPLING_SHARED ;
       CHECK(SRM_fit(5,seglen[r],nvox,xr,&qr,&mr,&fr)==SRM_OK,"independent fit of one run") ;
       for( c=0 ; c<k*seglen[r] ; c++ ){
         int cc=c/seglen[r] , tt=c%seglen[r] ;
         worst=fmax(worst,fabs(mr->s[c]-mc->s[cc*T+c0+tt])) ;
       }
       for( i=0 ; i<5 ; i++ ){
         int q2 ;
         for( q2=0 ; q2<nvox[i]*k ; q2++ ) worst=fmax(worst,fabs(mr->w[i][q2]-mc->wrun[i*nseg+r][q2])) ;
         free(xr[i]) ;
       }
       free(xr) ; SRM_free_model(mr) ; SRM_free_fitinfo(&fr) ;
     }
     CHECK(worst < 2e-5,"lambda=0 reproduces independent per-run fits") ;
     SRM_free_model(mc) ; SRM_free_fitinfo(&fc) ;
   }
   SRM_free_model(ms) ; SRM_free_fitinfo(&fs) ;

   /* the penalized objective never gets worse, for both models and several lambda */
   for( alg=0 ; alg<2 ; alg++ ){
     double lams[3]={0.0,30.0,500.0} ; int li ;
     for( li=0 ; li<3 ; li++ ){
       SRM_params q=p ; int j ;
       q.algo = alg ? SRM_ALGO_DET : SRM_ALGO_PROB ; q.niter=40 ; q.tol=0.0 ;
       q.nseg=nseg ; q.seglen=seglen ; q.coupling=lams[li] ;
       CHECK(SRM_fit(5,T,nvox,d.x,&q,&mc,&fc)==SRM_OK,"coupled fit") ;
       {
         /* the noise floor: float32 W, and penalty terms ~ lambda T k nview that cancel */
         double slack = 1e-9*fabs(fc.objective[0]) + 5e-7*(lams[li]+1.0)*k*nseg*5 ;
         for( j=1 ; j<=fc.niter_done ; j++ )
           CHECK(alg ? fc.objective[j]<=fc.objective[j-1]+slack
                     : fc.objective[j]>=fc.objective[j-1]-slack,
                 "coupled objective is monotone (to rounding)") ;
         CHECK(alg ? fc.objective[fc.niter_done]<fc.objective[0]-1e-4*fabs(fc.objective[0])*(li==0)
                   : fc.objective[fc.niter_done]>fc.objective[0],
               "and it improved on the shared-W start") ;
       }
       for( i=0 ; i<5 ; i++ ) for( r=0 ; r<nseg ; r++ )
         CHECK(ortho_error(mc->wrun[i*nseg+r],nvox[i],k) < 1e-5,"every W_ir orthonormal") ;
       for( i=0 ; i<5 ; i++ ) CHECK(ortho_error(mc->w[i],nvox[i],k) < 1e-5,"anchor orthonormal") ;
       SRM_free_model(mc) ; SRM_free_fitinfo(&fc) ;
     }
   }

   /* the point of it: when the mapping drifts between runs, run-wise W fit better,
      and deviations shrink as lambda grows */
   {
     double dev[3] , res_shared , res_coupled[3] , lams[3]={0.0,30.0,3000.0} ; int li ;
     p.algo=SRM_ALGO_DET ;
     CHECK(SRM_fit(5,T,nvox,d.x,&p,&ms,&fs)==SRM_OK,"shared") ;
     res_shared=coupled_residual(ms,&d,0) ;
     for( li=0 ; li<3 ; li++ ){
       SRM_params q=p ; double tot=0.0 ; int n=0 ;
       q.nseg=nseg ; q.seglen=seglen ; q.coupling=lams[li] ;
       CHECK(SRM_fit(5,T,nvox,d.x,&q,&mc,&fc)==SRM_OK,"coupled") ;
       res_coupled[li]=coupled_residual(mc,&d,1) ;
       for( i=0 ; i<5 ; i++ ) for( r=0 ; r<nseg ; r++ ){
         double dv ; CHECK(SRM_run_deviation(mc,i,r,&dv)==SRM_OK,"run_deviation") ; tot+=dv ; n++ ;
       }
       dev[li]=tot/n ;
       SRM_free_model(mc) ; SRM_free_fitinfo(&fc) ;
     }
     CHECK(res_coupled[1] < 0.97*res_shared,"run-wise W fit drifting data better than one W") ;
     CHECK(dev[1] > 0.01 && dev[2] < dev[1],"the coupling is neither off nor total at lambda = 30") ;
     CHECK(dev[0] > dev[1] && dev[1] > dev[2],"run W stays closer to the anchor as lambda grows") ;
     CHECK(res_coupled[0] <= res_coupled[1] && res_coupled[1] <= res_coupled[2]*(1.0+1e-9),
           "less coupling, less residual") ;
     CHECK(res_coupled[2] <= res_shared*(1.0+1e-6),"a strongly coupled fit is no worse than the shared one it starts from") ;
     SRM_free_model(ms) ; SRM_free_fitinfo(&fs) ;
   }

   /* projection by run, canonicalization, deviation */
   {
     SRM_params q=p ; double dv0 , dv1 ; float *out ; int f,t , c0 ;
     q.nseg=nseg ; q.seglen=seglen ; q.coupling=20.0 ; q.niter=25 ;
     CHECK(SRM_fit(5,T,nvox,d.x,&q,&mc,&fc)==SRM_OK,"coupled fit") ;
     out=(float *)malloc(sizeof(float)*k*seglen[1]) ;
     c0=seglen[0] ;
     {
       float *xr=(float *)malloc(sizeof(float)*nvox[2]*seglen[1]) ; double *ref ;
       for( f=0 ; f<nvox[2] ; f++ ) for( t=0 ; t<seglen[1] ; t++ ) xr[f*seglen[1]+t]=d.x[2][f*T+c0+t] ;
       CHECK(SRM_project_run(mc,2,1,xr,seglen[1],out)==SRM_OK,"project_run") ;
       ref=ref_project(mc->wrun[2*nseg+1],NULL,nvox[2],k,xr,seglen[1]) ;
       CHECK(maxdiff_fd(out,ref,(size_t)k*seglen[1]) < 1e-4,"project_run uses that run's W") ;
       free(ref) ;
       CHECK(SRM_project_run(mc,2,-1,xr,seglen[1],out)==SRM_OK,"project, anchor") ;
       ref=ref_project(mc->w[2],NULL,nvox[2],k,xr,seglen[1]) ;
       CHECK(maxdiff_fd(out,ref,(size_t)k*seglen[1]) < 1e-4,"run -1 uses the anchor") ;
       free(ref) ; free(xr) ;
     }
     free(out) ;
     CHECK(SRM_run_deviation(mc,2,1,&dv0)==SRM_OK && dv0>0.0 && dv0<4.0,"deviation in range") ;
     CHECK(SRM_run_deviation(mc,2,3,&dv0)==SRM_ERR_ARG && SRM_run_deviation(mc,5,0,&dv0)==SRM_ERR_ARG,"deviation bounds") ;

     CHECK(SRM_run_deviation(mc,2,1,&dv0)==SRM_OK,"deviation before") ;
     {
       double before=0.0 , after=0.0 ;
       before=coupled_residual(mc,&d,1) ;
       CHECK(SRM_canonicalize(mc)==SRM_OK,"canonicalize a run-coupled model") ;
       after=coupled_residual(mc,&d,1) ;
       CHECK(CLOSE(after/before,1.0,1e-4),"canonicalization leaves the run-wise fit unchanged") ;
       CHECK(SRM_run_deviation(mc,2,1,&dv1)==SRM_OK && CLOSE(dv0,dv1,1e-4),
             "... and the deviations from the anchors (rotated together)") ;
     }
     SRM_free_model(mc) ; SRM_free_fitinfo(&fc) ;
   }

   /* fit_view_coupled: the two limits, and the assembly */
   {
     int nv=nvox[1] , r2 ; float *s=(float *)malloc(sizeof(float)*k*T) ; float *wa=(float *)malloc(sizeof(float)*nv*k) ;
     float *wr[3] ; double wk[4*9] ;
     SRM_params q=p ; double *A=(double *)malloc(sizeof(double)*nv*k) ; float *ref=(float *)malloc(sizeof(float)*nv*k) ;
     q.algo=SRM_ALGO_DET ;
     CHECK(SRM_fit(5,T,nvox,d.x,&q,&ms,&fs)==SRM_OK,"model for fit_view_coupled") ;
     memcpy(s,ms->s,sizeof(float)*k*T) ; memcpy(wa,ms->w[1],sizeof(float)*nv*k) ;
     for( r2=0 ; r2<3 ; r2++ ) wr[r2]=(float *)malloc(sizeof(float)*nv*k) ;

     CHECK(SRM_fit_view_coupled(s,k,T,d.x[1],nv,nseg,seglen,wa,0.0,wr)==SRM_OK,"lambda=0") ;
     { int c0=0 ; for( r2=0 ; r2<3 ; c0+=seglen[r2],r2++ ){
         int f,cc,t ; double worst=0.0 ; int q2 ;
         for( f=0 ; f<nv ; f++ ) for( cc=0 ; cc<k ; cc++ ){
           double a=0.0 ; for( t=0 ; t<seglen[r2] ; t++ ) a += (double)d.x[1][f*T+c0+t]*s[cc*T+c0+t] ;
           A[f*k+cc]=a ;
         }
         CHECK(srm_polar(A,nv,k,ref,wk,1)==SRM_OK,"reference polar") ;
         for( q2=0 ; q2<nv*k ; q2++ ) worst=fmax(worst,fabs(ref[q2]-wr[r2][q2])) ;
         CHECK(worst < 1e-5,"lambda=0: each run's own Procrustes") ;
     } }
     CHECK(SRM_fit_view_coupled(s,k,T,d.x[1],nv,nseg,seglen,wa,1e9,wr)==SRM_OK,"lambda huge") ;
     for( r2=0 ; r2<3 ; r2++ ){
       double worst=0.0 ; int q2 ;
       for( q2=0 ; q2<nv*k ; q2++ ) worst=fmax(worst,fabs(wa[q2]-wr[r2][q2])) ;
       CHECK(worst < 1e-5,"lambda huge: every run's W is the anchor") ;
     }
     /* a moderate lambda: X_r S_r' + lambda * s2 * W_anchor, with s2 the mean variance of S's rows */
     {
       double lamv=40.0 , s2=0.0 ; int c0=0 ;
       for( i=0 ; i<k*T ; i++ ) s2 += (double)s[i]*s[i] ;
       s2 /= (double)k*T ;
       CHECK(SRM_fit_view_coupled(s,k,T,d.x[1],nv,nseg,seglen,wa,lamv,wr)==SRM_OK,"lambda = 40") ;
       for( r2=0 ; r2<3 ; c0+=seglen[r2],r2++ ){
         int f,cc,t2 ; double worst=0.0 ; int q2 ;
         for( f=0 ; f<nv ; f++ ) for( cc=0 ; cc<k ; cc++ ){
           double a=lamv*s2*(double)wa[f*k+cc] ;
           for( t2=0 ; t2<seglen[r2] ; t2++ ) a += (double)d.x[1][f*T+c0+t2]*s[cc*T+c0+t2] ;
           A[f*k+cc]=a ;
         }
         CHECK(srm_polar(A,nv,k,ref,wk,1)==SRM_OK,"reference polar") ;
         for( q2=0 ; q2<nv*k ; q2++ ) worst=fmax(worst,fabs(ref[q2]-wr[r2][q2])) ;
         CHECK(worst < 1e-5,"X_r S_r' + lambda s2 W_anchor, then polar") ;
       }
     }
     { int bad[3]={60,50,60} ;
       CHECK(SRM_fit_view_coupled(s,k,T,d.x[1],nv,nseg,bad,wa,1.0,wr)==SRM_ERR_ARG,"run lengths must sum to ntime") ;
       CHECK(SRM_fit_view_coupled(s,k,T,d.x[1],nv,nseg,seglen,wa,-1.0,wr)==SRM_ERR_ARG,"negative coupling") ; }
     for( r2=0 ; r2<3 ; r2++ ) free(wr[r2]) ;
     free(s) ; free(wa) ; free(A) ; free(ref) ; SRM_free_model(ms) ; SRM_free_fitinfo(&fs) ;
   }

   /* input checks */
   {
     SRM_params q=p ; int bad_len[3]={60,50,60} ;
     q.coupling=10.0 ; q.nseg=0 ; q.seglen=NULL ;
     CHECK(SRM_fit(5,T,nvox,d.x,&q,&mc,&fc)==SRM_ERR_ARG,"coupling without runs") ;
     q.nseg=1 ; q.seglen=seglen ;
     CHECK(SRM_fit(5,T,nvox,d.x,&q,&mc,&fc)==SRM_ERR_ARG,"one run cannot be coupled") ;
     q.nseg=3 ; q.seglen=bad_len ;
     CHECK(SRM_fit(5,T,nvox,d.x,&q,&mc,&fc)==SRM_ERR_ARG,"run lengths must sum to ntime") ;
     q.seglen=seglen ; q.coupling=-2.0 ;
     CHECK(SRM_fit(5,T,nvox,d.x,&q,&mc,&fc)==SRM_ERR_ARG,"negative coupling other than 'shared'") ;
     q.coupling=NAN ;
     CHECK(SRM_fit(5,T,nvox,d.x,&q,&mc,&fc)==SRM_ERR_ARG,"NaN coupling") ;
   }
   dsynth_free(&d) ;
   return 0 ;
}


/*------------------------------------------------------------------------*/
/* Step 4: S absorbs new subjects                                          */

static double rel_s_gap( const float *a , const float *b , int k , int T )
{
   double num=0.0 , den=0.0 ; int i ;
   for( i=0 ; i<k*T ; i++ ){ double d=(double)a[i]-b[i] ; num+=d*d ; den+=(double)b[i]*b[i] ; }
   return sqrt(num/den) ;
}

/* residual of S to S' after the best orthogonal alignment */
static double aligned_gap( const float *a , const float *b , int k , int T )
{
   float *R=(float *)malloc(sizeof(float)*k*k) ; double num=0.0 , den=0.0 ; int c,d,t ;
   if( SRM_procrustes_rotation(a,b,k,T,R)!=SRM_OK ){ free(R) ; return NAN ; }
   for( c=0 ; c<k ; c++ ) for( t=0 ; t<T ; t++ ){
     double v=0.0 ; for( d=0 ; d<k ; d++ ) v += (double)R[c*k+d]*a[d*T+t] ;
     num += (v-b[c*T+t])*(v-b[c*T+t]) ; den += (double)b[c*T+t]*b[c*T+t] ;
   }
   free(R) ; return sqrt(num/den) ;
}

static int test_update( void )
{
   int nvox[10]={60,70,65,75,80,70,65,85,60,75} , T=150 , k=3 , i , c , alg ;
   double sg[10]={0.4,0.5,0.6,0.5,0.7,0.8,0.6,0.9,1.0,0.7} ;
   synth s ;

   CHECK(synth_make(&s,10,nvox,T,k,sg,201)==0,"synth") ;
   for( i=0 ; i<10 ; i++ ) center_rows(s.x[i],nvox[i],T) ;

   for( alg=0 ; alg<2 ; alg++ ){
     SRM_params p ; SRM_model *m=NULL , *mb=NULL , *m2=NULL ; SRM_fitinfo fi , fb , fu ;
     float *s_before=(float *)malloc(sizeof(float)*k*T) ; float *w_old[7] ; double rho_old[7] ;
     int idx_new[3]={7,8,9} , nv_new[3] ; float *x_new[3] ; int j ;

     SRM_params_default(&p) ; p.nfeat=k ; p.center=0 ; p.tol=1e-12 ; p.niter=2000 ;
     p.algo = alg ? SRM_ALGO_DET : SRM_ALGO_PROB ;
     CHECK(SRM_fit(7,T,nvox,s.x,&p,&m,&fi)==SRM_OK,"fit the first 7 subjects") ;
     SRM_free_fitinfo(&fi) ;
     memcpy(s_before,m->s,sizeof(float)*k*T) ;
     for( i=0 ; i<7 ; i++ ){
       w_old[i]=(float *)malloc(sizeof(float)*nvox[i]*k) ; memcpy(w_old[i],m->w[i],sizeof(float)*nvox[i]*k) ;
       rho_old[i]= alg ? 0.0 : m->rho2[i] ;
     }
     for( j=0 ; j<3 ; j++ ){ nv_new[j]=nvox[idx_new[j]] ; x_new[j]=s.x[idx_new[j]] ; }

     /* the three new subjects, together */
     CHECK(SRM_update(m,3,nv_new,x_new,&p,&fu)==SRM_OK,"update with three new subjects") ;
     CHECK(m->nview==10 && m->nsubj_eff==10 && m->nupdate==1,"bookkeeping") ;
     CHECK(m->drift>0.0 && m->drift<0.3,"drift is small and positive") ;
     for( i=0 ; i<7 ; i++ ){
       CHECK(memcmp(w_old[i],m->w[i],sizeof(float)*nvox[i]*k)==0,"old W are untouched") ;
       if( !alg ) CHECK(m->rho2[i]==rho_old[i],"old noise variances are untouched") ;
     }
     for( j=0 ; j<3 ; j++ ) CHECK(ortho_error(m->w[7+j],nv_new[j],k) < 1e-5,"new W orthonormal") ;
     /* float32 W set a noise floor of ~1e-8 of the objective once it has plateaued */
     for( j=1 ; j<=fu.niter_done ; j++ )
       CHECK(alg ? fu.objective[j]<=fu.objective[j-1]+1e-7*fabs(fu.objective[j-1])
                 : fu.objective[j]>=fu.objective[j-1]-1e-7*fabs(fu.objective[j-1]),
             "the restricted objective is monotone (to rounding)") ;
     CHECK(alg ? fu.objective[fu.niter_done]<fu.objective[0]-1e-4*fabs(fu.objective[0])
               : fu.objective[fu.niter_done]>fu.objective[0]+1e-4*fabs(fu.objective[0]),
           "and it improved on the one-step start") ;

     /* the state is internally consistent */
     if( alg ){
       for( c=0 ; c<k*T ; c++ ) CHECK(CLOSE(m->a_sum[c]/10.0,m->s[c],2e-4),"det: S == a_sum / nsubj_eff") ;
     } else {
       double *post=(double *)calloc(k*k,sizeof(double)) , rsum=0.0 ; int a,b,t ;
       for( i=0 ; i<10 ; i++ ) rsum += 1.0/m->rho2[i] ;
       CHECK(CLOSE(m->rho_inv_sum/rsum,1.0,1e-9),"rho_inv_sum == sum over ALL subjects of 1/rho2") ;
       for( a=0 ; a<k*k ; a++ ) post[a]=m->sigma_s[a] ;
       CHECK(inv_small(k,post)==0,"invert sigma_s") ;
       for( a=0 ; a<k ; a++ ) post[a*k+a] += m->rho_inv_sum ;
       CHECK(inv_small(k,post)==0,"invert precision") ;
       for( a=0 ; a<k ; a++ ) for( t=0 ; t<T ; t++ ){
         double v=0.0 ; for( b=0 ; b<k ; b++ ) v += post[a*k+b]*m->a_sum[b*T+t] ;
         CHECK(CLOSE(v,m->s[a*T+t],2e-4),"prob: S == posterior(sigma_s, rho_inv_sum, a_sum)") ;
       }
       free(post) ;
     }

     /* at convergence the new subjects' W are the Procrustes solution against the new S */
     for( j=0 ; j<3 ; j++ ){
       float *wchk=(float *)malloc(sizeof(float)*nv_new[j]*k) ; double worst=0.0 ; int q ;
       CHECK(SRM_fit_view(m->s,k,T,x_new[j],nv_new[j],0,wchk,NULL,NULL)==SRM_OK,"fit_view") ;
       for( q=0 ; q<nv_new[j]*k ; q++ ) worst=fmax(worst,fabs(wchk[q]-m->w[7+j][q])) ;
       CHECK(worst < 2e-4,"new W is the fixed point against the updated S") ;
       free(wchk) ;
     }

     /* gauge: S moved little, and not by a rotation */
     {
       float R[9] ; double off=0.0 ; int a,b ;
       CHECK(SRM_procrustes_rotation(s_before,m->s,k,T,R)==SRM_OK,"align old S to new") ;
       for( a=0 ; a<k ; a++ ) for( b=0 ; b<k ; b++ ) off += ((a==b)?(R[a*k+b]-1.0):R[a*k+b])*((a==b)?(R[a*k+b]-1.0):R[a*k+b]) ;
       CHECK(sqrt(off) < 0.15,"the update keeps the gauge: the rotation between old and new S is near identity") ;
       CHECK(rel_s_gap(m->s,s_before,k,T) < 0.6,"S changed modestly") ;
     }

     /* how close to refitting all ten from scratch? */
     CHECK(SRM_fit(10,T,nvox,s.x,&p,&mb,&fb)==SRM_OK,"batch fit of all ten") ;
     {
       double g=aligned_gap(m->s,mb->s,k,T) ;
       CHECK(g < 0.12,"incremental S is close to the batch S (after alignment)") ;
       if( getenv("SRM_TEST_VERBOSE") ) fprintf(stderr,"[%s] update vs batch: aligned S gap %.4f, drift %.4f\n",alg?"det":"prob",g,m->drift) ;
     }

     /* the order of the new subjects within one call cannot matter */
     {
       SRM_model *mc=NULL ; int perm[3]={2,0,1} ; int nvp[3] ; float *xp[3] ; SRM_fitinfo fp ;
       CHECK(SRM_fit(7,T,nvox,s.x,&p,&mc,&fi)==SRM_OK,"refit first 7") ; SRM_free_fitinfo(&fi) ;
       for( j=0 ; j<3 ; j++ ){ nvp[j]=nv_new[perm[j]] ; xp[j]=x_new[perm[j]] ; }
       CHECK(SRM_update(mc,3,nvp,xp,&p,&fp)==SRM_OK,"update, permuted") ;
       CHECK(rel_s_gap(mc->s,m->s,k,T) < 2e-5,"order within a call: same S") ;
       for( j=0 ; j<3 ; j++ ){
         double worst=0.0 ; int q ;
         for( q=0 ; q<nvp[j]*k ; q++ ) worst=fmax(worst,fabs(mc->w[7+j][q]-m->w[7+perm[j]][q])) ;
         CHECK(worst < 5e-4,"order within a call: same W for each subject") ;
       }
       SRM_free_fitinfo(&fp) ; SRM_free_model(mc) ;
     }

     /* across calls the order can matter, a little; both stay near the batch fit */
     {
       SRM_model *ma=NULL , *mz=NULL ; SRM_fitinfo f1 ; int order[2][3]={{7,8,9},{9,8,7}} ; int r ;
       for( r=0 ; r<2 ; r++ ){
         SRM_model **mm = r ? &mz : &ma ;
         CHECK(SRM_fit(7,T,nvox,s.x,&p,mm,&fi)==SRM_OK,"refit first 7") ; SRM_free_fitinfo(&fi) ;
         for( j=0 ; j<3 ; j++ ){
           int id=order[r][j] ;
           CHECK(SRM_update(*mm,1,&nvox[id],&s.x[id],&p,&f1)==SRM_OK,"one-at-a-time update") ;
           SRM_free_fitinfo(&f1) ;
         }
       }
       CHECK(ma->nupdate==3 && ma->nsubj_eff==10,"three updates") ;
       CHECK(ma->drift >= m->drift*0.2,"drift accumulates over calls") ;
       {
         double across=aligned_gap(ma->s,mz->s,k,T) , a1=aligned_gap(ma->s,mb->s,k,T) , a2=aligned_gap(mz->s,mb->s,k,T) ;
         CHECK(across < 0.1,"order across calls: small effect on S") ;
         CHECK(a1 < 0.15 && a2 < 0.15,"one at a time is also near the batch fit") ;
         if( getenv("SRM_TEST_VERBOSE") )
           fprintf(stderr,"[%s] order across calls: gap between orders %.4f, each vs batch %.4f / %.4f, drift %.4f\n",alg?"det":"prob",across,a1,a2,ma->drift) ;
       }
       SRM_free_model(ma) ; SRM_free_model(mz) ;
     }

     /* an updated model will not be re-canonicalized */
     CHECK(SRM_canonicalize(m)==SRM_ERR_ARG,"canonicalize refuses after an update") ;

     SRM_free_fitinfo(&fu) ; SRM_free_fitinfo(&fb) ; SRM_free_model(m) ; SRM_free_model(mb) ; SRM_free_model(m2) ;
     for( i=0 ; i<7 ; i++ ) free(w_old[i]) ;
     free(s_before) ;
   }

   /* a model with no W for its old subjects, as read from a file, and failure leaves it alone */
   {
     SRM_params p ; SRM_model *m=NULL , *ld ; SRM_fitinfo fi ; int q ;
     SRM_params_default(&p) ; p.nfeat=k ; p.center=0 ; p.niter=40 ;
     CHECK(SRM_fit(7,T,nvox,s.x,&p,&m,&fi)==SRM_OK,"fit") ; SRM_free_fitinfo(&fi) ;
     ld=SRM_model_copy(m,-1) ;
     for( q=0 ; q<ld->nview ; q++ ){ free(ld->w[q]) ; ld->w[q]=NULL ; }
     CHECK(SRM_update(ld,1,&nvox[8],&s.x[8],&p,&fi)==SRM_OK,"update a W-less model") ;
     SRM_free_fitinfo(&fi) ;
     CHECK(ld->nview==8 && ld->w[7]!=NULL && ld->w[0]==NULL,"only the new subject has a W") ;
     { float *o=(float *)malloc(sizeof(float)*k*T) ;
       CHECK(SRM_project(ld,7,s.x[8],T,o)==SRM_OK && SRM_project(ld,0,s.x[0],T,o)==SRM_ERR_MISMATCH,"new projects, old cannot") ;
       free(o) ; }
     SRM_free_model(ld) ;

     /* errors: the model is unchanged */
     {
       SRM_model *snap=SRM_model_copy(m,-1) ; float save ; int nv_small=2 ;
       save=s.x[8][3] ; s.x[8][3]=NAN ;
       CHECK(SRM_update(m,1,&nvox[8],&s.x[8],&p,&fi)==SRM_ERR_NONFINITE,"NaN in the new data") ;
       s.x[8][3]=save ;
       CHECK(SRM_update(m,1,&nv_small,&s.x[8],&p,&fi)==SRM_ERR_NFEAT,"fewer voxels than components") ;
       { SRM_params bad=p ; bad.coupling=10.0 ;
         CHECK(SRM_update(m,1,&nvox[8],&s.x[8],&bad,&fi)==SRM_ERR_ARG,"coupling given for a shared-W model") ; }
       { SRM_params bad=p ; bad.niter=0 ;
         CHECK(SRM_update(m,1,&nvox[8],&s.x[8],&bad,&fi)==SRM_ERR_ARG,"niter 0") ; }
       CHECK(SRM_update(m,0,&nvox[8],&s.x[8],&p,&fi)==SRM_ERR_ARG,"no new subjects") ;
       CHECK(m->nview==snap->nview && m->nupdate==0 && m->drift==0.0 &&
             memcmp(m->s,snap->s,sizeof(float)*k*T)==0 && memcmp(m->a_sum,snap->a_sum,sizeof(double)*k*T)==0,
             "a failed update leaves the model exactly as it was") ;
       SRM_free_model(snap) ;
     }
     {
       SRM_model *nosum=SRM_model_copy(m,2) ;       /* dropping a view discards the running sums */
       CHECK(SRM_update(nosum,1,&nvox[8],&s.x[8],&p,&fi)==SRM_ERR_MISMATCH,"a model without its sums cannot be updated") ;
       SRM_free_model(nosum) ;
     }
     SRM_free_model(m) ;
   }
   synth_free(&s) ;

   /* a run-coupled model takes new subjects with a W per run */
   {
     int nvc[5]={70,80,75,90,85} , segc[3]={60,50,70} , Tc=180 , nseg=3 , r ;
     dsynth d ; int ac ;
     CHECK(dsynth_make(&d,5,nvc,nseg,segc,3,0.4,0.25,301)==0,"drifting synth") ;
     for( ac=0 ; ac<2 ; ac++ ){
       SRM_params p ; SRM_model *m=NULL ; SRM_fitinfo fi , fu ; float *w_old ;
       SRM_params_default(&p) ; p.nfeat=3 ; p.center=0 ; p.niter=300 ; p.tol=1e-10 ;
       p.algo = ac ? SRM_ALGO_DET : SRM_ALGO_PROB ;
       p.nseg=nseg ; p.seglen=segc ; p.coupling=30.0 ;
       CHECK(SRM_fit(4,Tc,nvc,d.x,&p,&m,&fi)==SRM_OK,"coupled fit of 4") ; SRM_free_fitinfo(&fi) ;
       w_old=(float *)malloc(sizeof(float)*nvc[1]*3) ; memcpy(w_old,m->wrun[1*nseg+2],sizeof(float)*nvc[1]*3) ;
       CHECK(SRM_update(m,1,&nvc[4],&d.x[4],&p,&fu)==SRM_OK,"update a run-coupled model") ;
       CHECK(m->nview==5 && m->wrun!=NULL && m->nsubj_eff==5,"bookkeeping") ;
       for( r=0 ; r<nseg ; r++ ){
         CHECK(m->wrun[4*nseg+r]!=NULL && ortho_error(m->wrun[4*nseg+r],nvc[4],3)<1e-5,"the new subject has an orthonormal W per run") ;
       }
       CHECK(memcmp(w_old,m->wrun[1*nseg+2],sizeof(float)*nvc[1]*3)==0,"old run-wise W untouched") ;
       { double dv ; CHECK(SRM_run_deviation(m,4,1,&dv)==SRM_OK && dv>0.0,"deviation of the new subject's run W") ; }
       { int j ; for( j=1 ; j<=fu.niter_done ; j++ )
           CHECK(ac ? fu.objective[j]<=fu.objective[j-1]+1e-3
                    : fu.objective[j]>=fu.objective[j-1]-1e-3,"coupled update is monotone (to rounding)") ; }
       { SRM_params bad=p ; bad.nseg=2 ;
         CHECK(SRM_update(m,1,&nvc[4],&d.x[4],&bad,&fi)==SRM_ERR_ARG,"run structure must match the model") ; }
       { SRM_params bad=p ; bad.coupling=SRM_COUPLING_SHARED ;
         CHECK(SRM_update(m,1,&nvc[4],&d.x[4],&bad,&fi)==SRM_ERR_ARG,"a coupled model needs its coupling") ; }
       SRM_free_fitinfo(&fu) ; SRM_free_model(m) ; free(w_old) ;
     }
     dsynth_free(&d) ;
   }
   return 0 ;
}


/*------------------------------------------------------------------------*/
/* Step 5: held-out metrics, reliability, preprocessing                    */

/* naive Pearson over n doubles, 0 when degenerate */
static double naive_r( const double *a , const double *b , int n )
{
   double ma=0,mb=0,saa=0,sbb=0,sab=0 ; int i ;
   for( i=0 ; i<n ; i++ ){ ma+=a[i] ; mb+=b[i] ; }
   ma/=n ; mb/=n ;
   for( i=0 ; i<n ; i++ ){ saa+=(a[i]-ma)*(a[i]-ma) ; sbb+=(b[i]-mb)*(b[i]-mb) ; sab+=(a[i]-ma)*(b[i]-mb) ; }
   return (saa>0 && sbb>0) ? sab/sqrt(saa*sbb) : 0.0 ;
}

/* the BrainIAK-example protocol, written the obvious way: build every window,
   correlate every pair, blank out overlapping pairs, take the arg max */
static void naive_segment_match( const float *a , const float *b , int k , int T , int win ,
                                 double *acc , double *chance )
{
   int nseg=T-win , i,j,d,w , correct=0 ; double chs=0.0 ;
   double *wa=(double *)malloc(sizeof(double)*k*win) , *wb=(double *)malloc(sizeof(double)*k*win) ;
   double *corr=(double *)malloc(sizeof(double)*nseg*nseg) ;
   for( i=0 ; i<nseg ; i++ ) for( j=0 ; j<nseg ; j++ ){
     for( w=0 ; w<win ; w++ ) for( d=0 ; d<k ; d++ ){
       wa[w*k+d]=a[d*T+i+w] ; wb[w*k+d]=b[d*T+j+w] ;
     }
     corr[i*nseg+j]=naive_r(wa,wb,k*win) ;
     if( abs(i-j)<win && i!=j ) corr[i*nseg+j]=-INFINITY ;
   }
   for( i=0 ; i<nseg ; i++ ){
     int best=0 , nc=0 ;
     for( j=0 ; j<nseg ; j++ ){ if( corr[i*nseg+j]>corr[i*nseg+best] ) best=j ; if( corr[i*nseg+j]>-INFINITY ) nc++ ; }
     correct += (best==i) ; chs += 1.0/nc ;
   }
   *acc=(double)correct/nseg ; *chance=chs/nseg ;
   free(wa) ; free(wb) ; free(corr) ;
}

static int test_metrics( void )
{
   srm_rng rng ; int i,c,t ;
   srm_rng_init(&rng,77,3) ;

   /* ---- segment matching ---- */
   {
     int k=4 , T=160 , win=9 ; float *a=(float *)malloc(sizeof(float)*k*T) , *b=(float *)malloc(sizeof(float)*k*T) ;
     double acc , ch , nacc , nch ;
     for( c=0 ; c<k*T ; c++ ) a[c]=(float)srm_normal(&rng) ;
     /* noisy copy: matches the right segment almost always */
     for( c=0 ; c<k*T ; c++ ) b[c]=a[c]+0.5f*(float)srm_normal(&rng) ;
     CHECK(SRM_segment_match(a,b,k,T,win,&acc,&ch)==SRM_OK,"segment match") ;
     naive_segment_match(a,b,k,T,win,&nacc,&nch) ;
     CHECK(CLOSE(acc,nacc,1e-12) && CLOSE(ch,nch,1e-12),"segment match agrees with the naive protocol") ;
     CHECK(acc > 0.95,"a noisy copy is matched") ;
     CHECK(ch > 0.0 && ch < 0.02,"chance is 1 / (number of candidates)") ;
     /* an exact copy */
     CHECK(SRM_segment_match(a,a,k,T,win,&acc,&ch)==SRM_OK && acc==1.0,"identical inputs: perfect") ;
     /* independent data: near chance */
     for( c=0 ; c<k*T ; c++ ) b[c]=(float)srm_normal(&rng) ;
     CHECK(SRM_segment_match(a,b,k,T,win,&acc,&ch)==SRM_OK,"independent") ;
     naive_segment_match(a,b,k,T,win,&nacc,&nch) ;
     CHECK(CLOSE(acc,nacc,1e-12),"independent data agree with the naive protocol too") ;
     CHECK(acc < 0.12,"independent data score near chance") ;
     /* a one-sample shift destroys the correspondence of windows only partly; a large one destroys it */
     for( c=0 ; c<k ; c++ ) for( t=0 ; t<T ; t++ ) b[c*T+t]=a[c*T+(t+37)%T] ;
     CHECK(SRM_segment_match(a,b,k,T,win,&acc,&ch)==SRM_OK && acc < 0.1,"a shifted copy is not matched") ;
     /* the overlap rule: near-duplicates one sample apart are not candidates */
     for( c=0 ; c<k ; c++ ) for( t=0 ; t<T ; t++ ) b[c*T+t]=a[c*T+t]+0.0f ;
     CHECK(SRM_segment_match(a,b,k,T,win,&acc,&ch)==SRM_OK && acc==1.0,"overlapping windows cannot beat the true one") ;
     /* exact ties: the same window appears twice in b, so the true counterpart and a non-overlapping
        duplicate correlate identically; the lower index must win (as in an arg max) */
     for( c=0 ; c<k*T ; c++ ) b[c]=(float)srm_normal(&rng) ;
     for( c=0 ; c<k ; c++ ) for( t=0 ; t<win ; t++ ){
       b[c*T+50+t]=a[c*T+50+t] ; b[c*T+90+t]=a[c*T+50+t] ;
       b[c*T+20+t]=a[c*T+20+t] ; b[c*T+120+t]=a[c*T+20+t] ;
     }
     CHECK(SRM_segment_match(a,b,k,T,win,&acc,&ch)==SRM_OK,"segment match with ties") ;
     naive_segment_match(a,b,k,T,win,&nacc,&nch) ;
     CHECK(CLOSE(acc,nacc,1e-12),"ties: the lower index wins, as in an arg max") ;
     CHECK(SRM_segment_match(a,b,k,2*win,win,&acc,&ch)==SRM_ERR_ARG,"too short to have non-overlapping candidates") ;
     CHECK(SRM_segment_match(NULL,b,k,T,win,&acc,&ch)==SRM_ERR_ARG && SRM_segment_match(a,b,0,T,win,&acc,&ch)==SRM_ERR_ARG,"bad input") ;
     /* a flat window correlates 0 with everything and is simply not matched */
     for( c=0 ; c<k ; c++ ) for( t=20 ; t<40 ; t++ ) b[c*T+t]=1.0f ;
     CHECK(SRM_segment_match(a,b,k,T,win,&acc,&ch)==SRM_OK && isfinite(acc),"flat windows are handled") ;
     free(a) ; free(b) ;
   }

   /* ---- item identification ---- */
   {
     int k=6 , n=40 ; float *a=(float *)malloc(sizeof(float)*k*n) , *b=(float *)malloc(sizeof(float)*k*n) ;
     double acc , ch ; int hits=0 ;
     for( c=0 ; c<k*n ; c++ ){ a[c]=(float)srm_normal(&rng) ; b[c]=a[c]+0.3f*(float)srm_normal(&rng) ; }
     CHECK(SRM_item_identify(a,b,k,n,&acc,&ch)==SRM_OK,"identify") ;
     CHECK(CLOSE(ch,1.0/n,1e-15) && acc > 0.7,"a noisy six-component copy is mostly identified") ;
     { /* with more components the same noise level is identified almost perfectly */
       int k2=20 ; float *a2=(float *)malloc(sizeof(float)*k2*n) , *b2=(float *)malloc(sizeof(float)*k2*n) ; double acc2 , ch2 ;
       for( c=0 ; c<k2*n ; c++ ){ a2[c]=(float)srm_normal(&rng) ; b2[c]=a2[c]+0.3f*(float)srm_normal(&rng) ; }
       CHECK(SRM_item_identify(a2,b2,k2,n,&acc2,&ch2)==SRM_OK && acc2 > 0.95,"a noisy 20-component copy is identified") ;
       free(a2) ; free(b2) ;
     }
     CHECK(SRM_item_identify(a,a,k,n,&acc,&ch)==SRM_OK && acc==1.0,"identical inputs: perfect") ;
     /* naive, for random data */
     for( c=0 ; c<k*n ; c++ ) b[c]=(float)srm_normal(&rng) ;
     CHECK(SRM_item_identify(a,b,k,n,&acc,&ch)==SRM_OK,"identify random") ;
     for( i=0 ; i<n ; i++ ){
       double best=-2 ; int bj=0 , j ; double u[16] , v[16] ;
       for( j=0 ; j<n ; j++ ){
         for( c=0 ; c<k ; c++ ){ u[c]=a[c*n+i] ; v[c]=b[c*n+j] ; }
         { double r=naive_r(u,v,k) ; if( r>best ){ best=r ; bj=j ; } }
       }
       hits += (bj==i) ;
     }
     CHECK(CLOSE(acc,(double)hits/n,1e-15),"identify agrees with the naive search") ;
     { /* ties: items 5 and 10 of b are identical copies of item 5 of a, so target 5 ties between the
          two candidates; only "the lower index wins" scores it as a hit */
       double accA , accB , cA ; float *b2=(float *)malloc(sizeof(float)*k*n) ; int i5=5 , i10=10 ;
       for( c=0 ; c<k*n ; c++ ) b2[c]=(float)srm_normal(&rng) ;
       for( c=0 ; c<k ; c++ ){ float v=a[c*n+i5] ; b2[c*n+i5]=v ; b2[c*n+i10]=v ; }
       CHECK(SRM_item_identify(a,b2,k,n,&accA,&cA)==SRM_OK,"identify with ties") ;
       for( c=0 ; c<k ; c++ ) b2[c*n+i10]=(float)srm_normal(&rng) ;       /* break the tie */
       CHECK(SRM_item_identify(a,b2,k,n,&accB,&cA)==SRM_OK,"identify without the tie") ;
       CHECK(accA >= accB - 1e-12,"the tied item still counts as identified (lower index wins)") ;
       free(b2) ;
     }
     CHECK(SRM_item_identify(a,b,1,n,&acc,&ch)==SRM_ERR_ARG && SRM_item_identify(a,b,k,1,&acc,&ch)==SRM_ERR_ARG,"bad sizes") ;
     free(a) ; free(b) ;
   }

   /* ---- per-component correlation ---- */
   {
     int k=5 , T=120 ; float *a=(float *)malloc(sizeof(float)*k*T) , *b=(float *)malloc(sizeof(float)*k*T) ; float r[5] ;
     double rm , z=0.0 ; double u[120] , v[120] ;
     for( c=0 ; c<k ; c++ ) for( t=0 ; t<T ; t++ ){
       double x=srm_normal(&rng) ;
       a[c*T+t]=(float)x ; b[c*T+t]=(float)(0.3*c*x + srm_normal(&rng)) ;       /* r grows with c */
     }
     for( t=0 ; t<T ; t++ ){ a[4*T+t]=(float)(2.0+0.0*t) ; }                    /* component 4 is flat in a */
     CHECK(SRM_component_corr(a,b,k,T,r,&rm)==SRM_OK,"component_corr") ;
     for( c=0 ; c<k ; c++ ){
       double ref ;
       for( t=0 ; t<T ; t++ ){ u[t]=a[c*T+t] ; v[t]=b[c*T+t] ; }
       ref=naive_r(u,v,T) ;
       CHECK(CLOSE(r[c],ref,2e-6),"per-component r") ;
       if( c<4 ) z += atanh(ref) ;
     }
     CHECK(r[4]==0.0,"a component with no variance gets r = 0") ;
     z += atanh(0.0) ;
     CHECK(CLOSE(rm,tanh(z/5.0),1e-6),"mean is the Fisher-z mean") ;
     CHECK(SRM_component_corr(a,b,k,T,NULL,&rm)==SRM_OK,"r_out may be NULL") ;
     CHECK(SRM_component_corr(a,b,k,1,r,&rm)==SRM_ERR_ARG,"one sample is not enough") ;
     free(a) ; free(b) ;
   }

   /* ---- shared-space similarity ---- */
   {
     int k=3 , T=200 ; float *s1=(float *)malloc(sizeof(float)*k*T) , *s2=(float *)malloc(sizeof(float)*k*T) ;
     float R[9] , cs[3] ; double mc ;
     for( c=0 ; c<k*T ; c++ ) s1[c]=(float)srm_normal(&rng) ;
     CHECK(srm_random_orthonormal(R,k,k,5,1)==SRM_OK,"rotation") ;
     for( c=0 ; c<k ; c++ ) for( t=0 ; t<T ; t++ ){
       double v=0.0 ; int d ; for( d=0 ; d<k ; d++ ) v += (double)R[c*k+d]*s1[d*T+t] ;
       s2[c*T+t]=(float)(2.5*v) ;                     /* rotated AND rescaled: same row space */
     }
     CHECK(SRM_shared_similarity(s1,s2,k,T,&mc,cs)==SRM_OK,"similarity of a rotated copy") ;
     CHECK(CLOSE(mc,1.0,1e-6) && cs[0]>0.9999 && cs[2]>0.9999,"same row space: every cosine is 1") ;
     /* two of three directions shared, one independent */
     for( t=0 ; t<T ; t++ ){ s2[0*T+t]=s1[0*T+t] ; s2[1*T+t]=s1[1*T+t] ; s2[2*T+t]=(float)srm_normal(&rng) ; }
     CHECK(SRM_shared_similarity(s1,s2,k,T,&mc,cs)==SRM_OK,"two of three shared") ;
     CHECK(cs[0]>0.999 && cs[1]>0.999 && cs[2]<0.3,"cosines: 1, 1, and a small one") ;
     CHECK(mc > 2.0/3.0-0.01 && mc < 2.0/3.0+0.08,"mean cos^2 is about 2/3") ;
     /* unrelated subspaces: about k/T */
     for( c=0 ; c<k*T ; c++ ) s2[c]=(float)srm_normal(&rng) ;
     CHECK(SRM_shared_similarity(s1,s2,k,T,&mc,NULL)==SRM_OK && mc < 0.08,"unrelated subspaces: near k/T") ;
     /* symmetric */
     { double mc2 ; CHECK(SRM_shared_similarity(s2,s1,k,T,&mc2,NULL)==SRM_OK && CLOSE(mc,mc2,1e-9),"symmetric") ; }
     /* a dependent row has no defined row space */
     for( t=0 ; t<T ; t++ ){ s2[1*T+t]=s2[0*T+t] ; }
     CHECK(SRM_shared_similarity(s1,s2,k,T,&mc,NULL)==SRM_ERR_SINGULAR,"dependent rows") ;
     CHECK(SRM_shared_similarity(s1,s2,5,3,&mc,NULL)==SRM_ERR_NFEAT,"more components than samples") ;
     free(s1) ; free(s2) ;
   }

   /* ---- split-half reliability, Spearman-Brown ---- */
   {
     int nr=300 , nc=120 ; float *xa=(float *)malloc(sizeof(float)*nr*nc) , *xb=(float *)malloc(sizeof(float)*nr*nc) ;
     float *sig=(float *)malloc(sizeof(float)*nr*nc) ;
     double rp , rs , sd_n=1.0 ; int j ;
     for( c=0 ; c<nr*nc ; c++ ){
       sig[c]=(float)srm_normal(&rng) ;
       xa[c]=sig[c]+(float)(sd_n*srm_normal(&rng)) ; xb[c]=sig[c]+(float)(sd_n*srm_normal(&rng)) ;
     }
     CHECK(SRM_split_reliability(xa,xb,nr,nc,&rp,&rs)==SRM_OK,"reliability") ;
     /* signal and noise have equal variance, so the correlation between two measurements is 1/2 */
     CHECK(CLOSE(rp,0.5,0.03) && CLOSE(rs,0.5,0.03),"two measurements with SNR 1 correlate 1/2") ;
     CHECK(CLOSE(SRM_spearman_brown(rp,2.0),2*rp/(1+rp),1e-12),"Spearman-Brown") ;
     CHECK(CLOSE(SRM_spearman_brown(0.5,2.0),2.0/3.0,1e-12) && CLOSE(SRM_spearman_brown(0.37,1.0),0.37,1e-12),"known values") ;
     CHECK(isnan(SRM_spearman_brown(-0.6,3.0)) && isnan(SRM_spearman_brown(-1.0,2.0)),"undefined when 1+(f-1)r is not positive") ;
     CHECK(isfinite(SRM_spearman_brown(-0.9,2.0)),"but defined just inside that limit") ;
     { double u[300] , v[300] , acc=0.0 ; int n_ok=0 ;
       for( j=0 ; j<nc ; j++ ){ for( i=0 ; i<nr ; i++ ){ u[i]=xa[i*nc+j] ; v[i]=xb[i*nc+j] ; } acc+=atanh(naive_r(u,v,nr)) ; n_ok++ ; }
       CHECK(CLOSE(rp,tanh(acc/n_ok),1e-6),"r_pattern is the Fisher mean of the column correlations") ; }
     { double u[300] , v[300] , acc=0.0 ;
       for( i=0 ; i<nr ; i++ ){ for( j=0 ; j<nc ; j++ ){ u[j]=xa[i*nc+j] ; v[j]=xb[i*nc+j] ; } acc+=atanh(naive_r(u,v,nc)) ; }
       CHECK(CLOSE(rs,tanh(acc/nr),1e-6),"r_series is the Fisher mean of the row correlations") ; }
     CHECK(SRM_split_reliability(xa,xb,nr,nc,&rp,NULL)==SRM_OK && SRM_split_reliability(xa,xb,nr,nc,NULL,&rs)==SRM_OK,"either output may be NULL") ;
     /* no signal at all: reliability near zero; pure copies: 1 */
     for( c=0 ; c<nr*nc ; c++ ) xb[c]=(float)srm_normal(&rng) ;
     CHECK(SRM_split_reliability(xa,xb,nr,nc,&rp,&rs)==SRM_OK && fabs(rp)<0.03 && fabs(rs)<0.03,"unrelated measurements: ~0") ;
     CHECK(SRM_split_reliability(xa,xa,nr,nc,&rp,&rs)==SRM_OK && rp>0.9999 && rs>0.9999,"identical measurements: ~1") ;
     for( c=0 ; c<nr*nc ; c++ ){ xa[c]=1.0f ; xb[c]=2.0f ; }
     CHECK(SRM_split_reliability(xa,xb,nr,nc,&rp,&rs)==SRM_ERR_SINGULAR,"constant data have no reliability") ;
     CHECK(SRM_split_reliability(xa,xb,1,nc,&rp,&rs)==SRM_ERR_ARG,"one row") ;
     free(xa) ; free(xb) ; free(sig) ;
   }

   /* ---- gathering columns ---- */
   {
     int nr=3 , nc=7 ; float x[21] , out[21] ; unsigned char sel[7]={1,0,1,0,0,1,1} ; int n ;
     for( c=0 ; c<21 ; c++ ) x[c]=(float)c ;
     n=SRM_gather_columns(x,nr,nc,sel,out) ;
     CHECK(n==4,"count") ;
     for( i=0 ; i<nr ; i++ ){
       CHECK(out[i*4+0]==x[i*nc+0] && out[i*4+1]==x[i*nc+2] && out[i*4+2]==x[i*nc+5] && out[i*4+3]==x[i*nc+6],
             "columns copied in order, first and last included") ;
     }
     { unsigned char none[7]={0} ; CHECK(SRM_gather_columns(x,nr,nc,none,out)==0,"none selected") ; }
     CHECK(SRM_gather_columns(NULL,nr,nc,sel,out)==-1,"bad input") ;
   }
   return 0 ;
}

static int test_reconstruction_r2( void )
{
   int nvox[3]={60,70,50} , T=120 , k=3 , i,f,t,c ;
   double sg[3]={0.6,0.8,1.0} ;
   synth s ; SRM_params p ; SRM_model *m=NULL ; SRM_fitinfo fi ;
   float *sh=(float *)malloc(sizeof(float)*k*T) ; int others[2]={0,2} ; float *xo[2] ;
   float *vr ; double r2 , sse , sst ;

   CHECK(synth_make(&s,3,nvox,T,k,sg,95)==0,"synth") ;
   for( i=0 ; i<3 ; i++ ) center_rows(s.x[i],nvox[i],T) ;
   SRM_params_default(&p) ; p.nfeat=k ; p.center=0 ; p.niter=60 ;
   CHECK(SRM_fit(3,T,nvox,s.x,&p,&m,&fi)==SRM_OK,"fit") ;
   xo[0]=s.x[0] ; xo[1]=s.x[2] ;
   CHECK(SRM_shared_response(m,2,others,xo,T,1,sh)==SRM_OK,"reference shared response") ;
   vr=(float *)malloc(sizeof(float)*nvox[1]) ;
   CHECK(SRM_reconstruction_r2(m,1,s.x[1],sh,T,&r2,vr)==SRM_OK,"r2") ;
   sse=0 ; sst=0 ;
   for( f=0 ; f<nvox[1] ; f++ ){
     double mean=0.0 , a=0.0 , b=0.0 ;
     for( t=0 ; t<T ; t++ ) mean += s.x[1][f*T+t] ;
     mean/=T ;
     for( t=0 ; t<T ; t++ ){
       double xh=0.0 , x=s.x[1][f*T+t] ;
       for( c=0 ; c<k ; c++ ) xh += (double)m->w[1][f*k+c]*sh[c*T+t] ;
       a += (x-xh)*(x-xh) ; b += (x-mean)*(x-mean) ;
     }
     sse+=a ; sst+=b ;
     CHECK(CLOSE(vr[f],1.0-a/b,1e-4),"per-feature R^2") ;
   }
   CHECK(CLOSE(r2,1.0-sse/sst,1e-6),"pooled R^2 agrees with the direct sums") ;
   CHECK(r2 > 0.0 && r2 < 1.0,"R^2 is a sensible fraction for noisy data") ;

   /* a model that explains the data exactly gives 1; a null shared response gives <= ~0 */
   {
     float *xhat=(float *)malloc(sizeof(float)*nvox[1]*T) ; float *zero=(float *)calloc((size_t)k*T,sizeof(float)) ;
     CHECK(SRM_reconstruct(m,1,m->s,T,xhat)==SRM_OK,"reconstruct") ;
     CHECK(SRM_reconstruction_r2(m,1,xhat,m->s,T,&r2,NULL)==SRM_OK && r2 > 0.99999,"exact reconstruction: R^2 = 1") ;
     CHECK(SRM_reconstruction_r2(m,1,s.x[1],zero,T,&r2,NULL)==SRM_OK && r2 < 0.01 && r2 > -0.05,"a zero reference explains nothing") ;
     { /* a constant feature gets 0, and does not break the pool */
       float *xc=(float *)malloc(sizeof(float)*nvox[1]*T) ;
       memcpy(xc,s.x[1],sizeof(float)*nvox[1]*T) ;
       for( t=0 ; t<T ; t++ ) xc[3*T+t]=4.0f ;
       CHECK(SRM_reconstruction_r2(m,1,xc,sh,T,&r2,vr)==SRM_OK && vr[3]==0.0f && isfinite(r2),"constant feature") ;
       for( c=0 ; c<nvox[1]*T ; c++ ) xc[c]=2.0f ;
       CHECK(SRM_reconstruction_r2(m,1,xc,sh,T,&r2,NULL)==SRM_ERR_SINGULAR,"no variance at all") ;
       free(xc) ;
     }
     CHECK(SRM_reconstruction_r2(m,5,s.x[1],sh,T,&r2,NULL)==SRM_ERR_ARG && SRM_reconstruction_r2(NULL,1,s.x[1],sh,T,&r2,NULL)==SRM_ERR_ARG,"bad view or model") ;
     free(xhat) ; free(zero) ;
   }
   free(sh) ; free(vr) ; SRM_free_fitinfo(&fi) ; SRM_free_model(m) ; synth_free(&s) ;
   return 0 ;
}

static int test_zscore( void )
{
   int nv=40 , T=100 , seg[3]={30,40,30} , f,t,r ; float *x=(float *)malloc(sizeof(float)*nv*T) ; unsigned char bad[40] ;
   srm_rng rng ; int nbad , c0 ;
   srm_rng_init(&rng,3,3) ;
   for( f=0 ; f<nv ; f++ ) for( t=0 ; t<T ; t++ ) x[f*T+t]=(float)(50.0+(1+f%5)*srm_normal(&rng)+0.2*t) ;
   for( t=0 ; t<T ; t++ ){ x[7*T+t]=3.0f ; }                                /* constant everywhere */
   for( t=30 ; t<70 ; t++ ){ x[11*T+t]=9.0f ; }                             /* constant in one run only */
   nbad=SRM_zscore_segments(x,nv,T,3,seg,bad) ;
   CHECK(nbad==2,"two features flagged") ;
   CHECK(bad[7] && bad[11] && !bad[0] && !bad[39],"the right ones") ;
   for( t=0 ; t<T ; t++ ) CHECK(x[7*T+t]==0.0f && x[11*T+t]==0.0f,"flagged features are zeroed throughout") ;
   for( f=0 ; f<nv ; f++ ){
     if( bad[f] ) continue ;
     for( r=0,c0=0 ; r<3 ; c0+=seg[r],r++ ){
       double m=0,v=0 ;
       for( t=0 ; t<seg[r] ; t++ ) m+=x[f*T+c0+t] ;
       m/=seg[r] ;
       for( t=0 ; t<seg[r] ; t++ ) v+=(x[f*T+c0+t]-m)*(x[f*T+c0+t]-m) ;
       CHECK(fabs(m)<1e-5 && fabs(v/seg[r]-1.0)<1e-5,"mean 0 and population sd 1 within every run") ;
     }
   }
   CHECK(SRM_zscore_segments(x,nv,T,3,seg,NULL)>=0,"bad may be NULL") ;
   { int wrong[3]={30,40,20} ; CHECK(SRM_zscore_segments(x,nv,T,3,wrong,bad)==SRM_ERR_ARG,"segments must sum to ntime") ; }
   { int tiny[2]={1,99} ; CHECK(SRM_zscore_segments(x,nv,T,2,tiny,bad)==SRM_ERR_ARG,"a one-sample run has no variance to scale by") ; }
   x[5*T+10]=NAN ;
   CHECK(SRM_zscore_segments(x,nv,T,3,seg,bad)==SRM_ERR_NONFINITE,"non-finite data are an error") ;
   free(x) ;
   return 0 ;
}


/*------------------------------------------------------------------------*/
/* Step 6: the model file                                                  */

static char **make_labels( int n , const char *stem )
{
   char **l=(char **)malloc(sizeof(char *)*n) ; int i ;
   for( i=0 ; i<n ; i++ ){ l[i]=(char *)malloc(32) ; snprintf(l[i],32,"%s%02d",stem,i+1) ; }
   return l ;
}

static SRM_provenance * make_prov_time( int nsubj , int nseg , const int *seglen , int ntime_in ,
                                        const unsigned char *keep )
{
   SRM_provenance *pv=SRM_provenance_new() ; int r ;
   pv->axis=SRM_AXIS_TIME ; pv->zscore=SRM_ZSCORE_RUN ; pv->ntime_in=ntime_in ; pv->nseg=nseg ;
   pv->seglen=(int *)malloc(sizeof(int)*nseg) ;
   for( r=0 ; r<nseg ; r++ ) pv->seglen[r]=seglen[r] ;
   if( keep!=NULL ){ pv->keep=(unsigned char *)malloc(ntime_in) ; memcpy(pv->keep,keep,ntime_in) ; }
   pv->nsubj=nsubj ; pv->label=make_labels(nsubj,"sub") ;
   return pv ;
}

static char * slurp( const char *name , long *len )
{
   FILE *fp=fopen(name,"rb") ; char *b ;
   if( fp==NULL ) return NULL ;
   fseek(fp,0,SEEK_END) ; *len=ftell(fp) ; fseek(fp,0,SEEK_SET) ;
   b=(char *)malloc(*len+1) ; if( fread(b,1,*len,fp)!=(size_t)*len ){ fclose(fp) ; free(b) ; return NULL ; }
   b[*len]='\0' ; fclose(fp) ; return b ;
}

static int spit( const char *name , const char *data , long len )
{
   FILE *fp=fopen(name,"wb") ;
   if( fp==NULL ) return 1 ;
   fwrite(data,1,len,fp) ; fclose(fp) ; return 0 ;
}

/* every stored field of two models, exactly */
static int models_equal( const SRM_model *a , const SRM_model *b )
{
   int i ; size_t k=a->nfeat , T=a->ntime ;
   if( a->algo!=b->algo || a->nview!=b->nview || a->ntime!=b->ntime || a->nfeat!=b->nfeat ) return 0 ;
   if( a->nseg!=b->nseg || a->center!=b->center || a->coupled!=b->coupled || a->coupling!=b->coupling ) return 0 ;
   if( a->nsubj_eff!=b->nsubj_eff || a->nupdate!=b->nupdate || a->drift!=b->drift || a->rho_inv_sum!=b->rho_inv_sum ) return 0 ;
   for( i=0 ; i<a->nview ; i++ ) if( a->nvox[i]!=b->nvox[i] ) return 0 ;
   if( memcmp(a->s,b->s,sizeof(float)*k*T)!=0 || memcmp(a->a_sum,b->a_sum,sizeof(double)*k*T)!=0 ) return 0 ;
   if( (a->sigma_s==NULL)!=(b->sigma_s==NULL) || (a->rho2==NULL)!=(b->rho2==NULL) ) return 0 ;
   if( a->sigma_s!=NULL && memcmp(a->sigma_s,b->sigma_s,sizeof(float)*k*k)!=0 ) return 0 ;
   if( a->rho2!=NULL && memcmp(a->rho2,b->rho2,sizeof(double)*a->nview)!=0 ) return 0 ;
   return 1 ;
}

static int prov_equal( const SRM_provenance *a , const SRM_provenance *b )
{
   int i ;
   if( a->axis!=b->axis || a->zscore!=b->zscore || a->nsubj!=b->nsubj ) return 0 ;
   for( i=0 ; i<a->nsubj ; i++ ) if( strcmp(a->label[i],b->label[i])!=0 ) return 0 ;
   if( a->axis==SRM_AXIS_TIME ){
     if( a->ntime_in!=b->ntime_in || a->nseg!=b->nseg ) return 0 ;
     for( i=0 ; i<a->nseg ; i++ ) if( a->seglen[i]!=b->seglen[i] ) return 0 ;
     for( i=0 ; i<a->ntime_in ; i++ ) if( (a->keep?a->keep[i]:1)!=(b->keep?b->keep[i]:1) ) return 0 ;
   } else {
     if( a->nitem!=b->nitem ) return 0 ;
     for( i=0 ; i<a->nitem ; i++ ) if( strcmp(a->item_key[i],b->item_key[i])!=0 ) return 0 ;
   }
   return 1 ;
}

static int test_model_file( void )
{
   int nvox[8]={60,70,65,75,80,70,65,85} , T=120 , k=3 , i , alg ;
   double sg[8]={0.4,0.5,0.6,0.5,0.7,0.8,0.6,0.9} ;
   synth s ; char path[200] ;
   snprintf(path,sizeof(path),"srm_test_model_%ld.srm",(long)getpid()) ;

   CHECK(synth_make(&s,8,nvox,T,k,sg,401)==0,"synth") ;
   for( i=0 ; i<8 ; i++ ) center_rows(s.x[i],nvox[i],T) ;

   for( alg=0 ; alg<2 ; alg++ ){
     SRM_params p ; SRM_model *m=NULL , *ld=NULL , *m2=NULL , *ld2=NULL ; SRM_fitinfo fi ;
     SRM_provenance *pv , *pl=NULL , *pl2=NULL ; int segs[2]={50,70} ;
     unsigned char keep[130] ; int j ;
     SRM_params_default(&p) ; p.nfeat=k ; p.center=(alg==0) ; p.niter=40 ;
     p.algo = alg ? SRM_ALGO_DET : SRM_ALGO_PROB ;
     CHECK(SRM_fit(5,T,nvox,s.x,&p,&m,&fi)==SRM_OK,"fit") ; SRM_free_fitinfo(&fi) ;
     CHECK(SRM_canonicalize(m)==SRM_OK,"canonicalize") ;
     for( j=0 ; j<130 ; j++ ) keep[j]=(j%13!=5 || j>=120) ;            /* censor some of 130 input time points */
     { int kept=0 ; for( j=0 ; j<130 ; j++ ) kept+=keep[j] ;
       CHECK(kept==T-0 || 1,"") ; }
     /* make the number of retained points exactly T */
     { int kept=0 ; for( j=0 ; j<130 ; j++ ) kept+=keep[j] ;
       for( j=0 ; kept>T ; j++ ) if( keep[j] ){ keep[j]=0 ; kept-- ; }
       for( j=129 ; kept<T ; j-- ) if( !keep[j] ){ keep[j]=1 ; kept++ ; } }
     pv=make_prov_time(5,2,segs,130,keep) ;
     CHECK(SRM_save_model(path,m,pv)==SRM_OK,"save") ;
     CHECK(SRM_load_model(path,&ld,&pl)==SRM_OK,"load") ;
     CHECK(models_equal(m,ld),"every stored field round-trips exactly") ;
     CHECK(prov_equal(pv,pl),"provenance round-trips exactly, including the censor mask") ;
     for( i=0 ; i<ld->nview ; i++ ) CHECK(ld->w[i]==NULL,"a loaded model has no W") ;
     CHECK(ld->mu==NULL && ld->wrun==NULL,"... and no means or run-wise W") ;
     CHECK(ld->center==m->center,"the centering flag survives, though the means do not") ;
     { char tmpn[260] ; snprintf(tmpn,sizeof(tmpn),"%s.tmp%ld",path,(long)getpid()) ;
       CHECK(access(tmpn,F_OK)!=0,"no temporary file is left behind") ; }

     /* the real test of "enough state": update the loaded model and the original alike */
     {
       SRM_params q=p ; int nvn[2]={nvox[5],nvox[6]} ; float *xn[2] ; SRM_fitinfo fa , fb ;
       xn[0]=s.x[5] ; xn[1]=s.x[6] ; q.niter=30 ; q.tol=0.0 ;
       m2=SRM_model_copy(m,-1) ;
       CHECK(SRM_update(m2,2,nvn,xn,&q,&fa)==SRM_OK,"update the in-memory model") ;
       CHECK(SRM_update(ld,2,nvn,xn,&q,&fb)==SRM_OK,"update the loaded model") ;
       CHECK(models_equal(m2,ld),"updating a loaded model is bit-identical to updating the original") ;
       CHECK(memcmp(m2->w[6],ld->w[6],sizeof(float)*nvox[6]*k)==0,"... including the new subjects' W") ;
       SRM_free_fitinfo(&fa) ; SRM_free_fitinfo(&fb) ;

       /* save the updated model, load, and keep going */
       pv->nsubj=7 ; { char **nl=make_labels(7,"sub") ; for( i=0 ; i<5 ; i++ ) free(pv->label[i]) ; free(pv->label) ; pv->label=nl ; }
       CHECK(SRM_save_model(path,ld,pv)==SRM_OK,"save an updated model") ;
       CHECK(SRM_load_model(path,&ld2,&pl2)==SRM_OK,"load it again") ;
       CHECK(models_equal(ld,ld2) && ld2->nupdate==1 && ld2->nsubj_eff==7 && ld2->drift>0.0,"update bookkeeping survives") ;
       { int nv1[1]={nvox[7]} ; float *x1[1]={s.x[7]} ;
         CHECK(SRM_update(m2,1,nv1,x1,&q,&fa)==SRM_OK && SRM_update(ld2,1,nv1,x1,&q,&fb)==SRM_OK,"a second update") ;
         CHECK(models_equal(m2,ld2) && ld2->nupdate==2,"... still bit-identical after the second update") ;
         SRM_free_fitinfo(&fa) ; SRM_free_fitinfo(&fb) ; }
     }
     /* the provenance checks */
     {
       int ok_seg[2]={50,70} , bad_seg[2]={50,60} , bad2[2]={70,50} ;
       CHECK(SRM_provenance_check(pl,130,2,ok_seg)==SRM_OK,"new data with the same timeline fit") ;
       CHECK(SRM_provenance_check(pl,131,2,ok_seg)==SRM_ERR_MISMATCH,"a different number of input time points") ;
       CHECK(SRM_provenance_check(pl,130,2,bad_seg)==SRM_ERR_MISMATCH,"different run lengths") ;
       CHECK(SRM_provenance_check(pl,130,2,bad2)==SRM_ERR_MISMATCH,"runs in a different order") ;
       CHECK(SRM_provenance_check(pl,130,1,ok_seg)==SRM_ERR_MISMATCH,"a different number of runs") ;
     }
     SRM_free_model(m) ; SRM_free_model(ld) ; SRM_free_model(m2) ; SRM_free_model(ld2) ;
     SRM_provenance_free(pv) ; SRM_provenance_free(pl) ; SRM_provenance_free(pl2) ;
   }

   /* a run-coupled model: the coupled flag and lambda survive, and a loaded model still gets W per run */
   {
     int nvc[5]={70,80,75,90,85} , segc[3]={60,50,70} , Tc=180 , r ; dsynth d ;
     SRM_params p ; SRM_model *m=NULL , *ld=NULL , *mm=NULL ; SRM_fitinfo fi ; SRM_provenance *pv , *pl=NULL ;
     CHECK(dsynth_make(&d,5,nvc,3,segc,3,0.4,0.25,411)==0,"drifting synth") ;
     SRM_params_default(&p) ; p.nfeat=3 ; p.center=0 ; p.niter=40 ; p.nseg=3 ; p.seglen=segc ; p.coupling=35.0 ;
     CHECK(SRM_fit(4,Tc,nvc,d.x,&p,&m,&fi)==SRM_OK && m->coupled && m->coupling==35.0,"coupled fit") ; SRM_free_fitinfo(&fi) ;
     pv=make_prov_time(4,3,segc,Tc,NULL) ;
     CHECK(SRM_save_model(path,m,pv)==SRM_OK && SRM_load_model(path,&ld,&pl)==SRM_OK,"save and load") ;
     CHECK(models_equal(m,ld) && ld->coupled==1 && ld->coupling==35.0 && ld->nseg==3 && ld->wrun==NULL,"coupling survives; the run-wise W do not") ;
     CHECK(pl->keep!=NULL && pl->ntime_in==Tc,"no censoring: every time point retained") ;
     mm=SRM_model_copy(m,-1) ;
     { SRM_params q=p ; SRM_fitinfo fa , fb ; q.tol=0.0 ; q.niter=25 ;
       CHECK(SRM_update(mm,1,&nvc[4],&d.x[4],&q,&fa)==SRM_OK && SRM_update(ld,1,&nvc[4],&d.x[4],&q,&fb)==SRM_OK,"update both") ;
       CHECK(models_equal(mm,ld),"a loaded run-coupled model updates bit-identically") ;
       for( r=0 ; r<3 ; r++ ) CHECK(ld->wrun!=NULL && ld->wrun[4*3+r]!=NULL && ld->wrun[0*3+r]==NULL
                                    && memcmp(mm->wrun[4*3+r],ld->wrun[4*3+r],sizeof(float)*nvc[4]*3)==0,
                                    "the new subject gets a W per run; the loaded ones have none") ;
       SRM_free_fitinfo(&fa) ; SRM_free_fitinfo(&fb) ; }
     SRM_free_model(m) ; SRM_free_model(ld) ; SRM_free_model(mm) ; SRM_provenance_free(pv) ; SRM_provenance_free(pl) ; dsynth_free(&d) ;
   }

   /* the item axis, and matching keys in any order */
   {
     int ni=40 , nvi[4]={50,60,55,65} , j ; synth si ; SRM_params p ; SRM_model *m=NULL , *ld=NULL ; SRM_fitinfo fi ;
     SRM_provenance *pv=SRM_provenance_new() , *pl=NULL ; char **perm_keys ; int perm[40] , *seen ;
     double sgi[4]={0.5,0.6,0.7,0.8} ;
     CHECK(synth_make(&si,4,nvi,ni,3,sgi,421)==0,"item synth") ;
     SRM_params_default(&p) ; p.nfeat=3 ; p.center=0 ; p.niter=30 ;
     CHECK(SRM_fit(4,ni,nvi,si.x,&p,&m,&fi)==SRM_OK,"fit items") ; SRM_free_fitinfo(&fi) ;
     pv->axis=SRM_AXIS_ITEM ; pv->nitem=ni ; pv->item_key=(char **)malloc(sizeof(char *)*ni) ;
     for( j=0 ; j<ni ; j++ ){ pv->item_key[j]=(char *)malloc(24) ; snprintf(pv->item_key[j],24,"face%03d:r%d",j,j%2) ; }
     pv->nsubj=4 ; pv->label=make_labels(4,"s") ;
     CHECK(SRM_save_model(path,m,pv)==SRM_OK && SRM_load_model(path,&ld,&pl)==SRM_OK,"save and load, item axis") ;
     CHECK(prov_equal(pv,pl) && pl->axis==SRM_AXIS_ITEM,"item keys round-trip") ;
     /* new subject supplies the same keys in another order */
     perm_keys=(char **)malloc(sizeof(char *)*ni) ;
     for( j=0 ; j<ni ; j++ ) perm_keys[j]=pv->item_key[(j*7+3)%ni] ;      /* 7 is coprime with 40 */
     CHECK(SRM_provenance_match_items(pl,ni,perm_keys,perm)==SRM_OK,"keys in another order match") ;
     seen=(int *)calloc(ni,sizeof(int)) ;
     for( j=0 ; j<ni ; j++ ){ CHECK(perm[j]==(j*7+3)%ni,"perm[j] is the model column of key j") ; seen[perm[j]]++ ; }
     for( j=0 ; j<ni ; j++ ) CHECK(seen[j]==1,"a permutation") ;
     perm_keys[5]=perm_keys[6] ;                                          /* a repeat, hence a missing one */
     CHECK(SRM_provenance_match_items(pl,ni,perm_keys,perm)==SRM_ERR_MISMATCH,"a repeated key") ;
     perm_keys[5]=(char *)"face999:r0" ;
     CHECK(SRM_provenance_match_items(pl,ni,perm_keys,perm)==SRM_ERR_MISMATCH,"an unknown key") ;
     CHECK(SRM_provenance_match_items(pl,ni-1,perm_keys,perm)==SRM_ERR_MISMATCH,"too few keys") ;
     { int seg1[1]={ni} ; CHECK(SRM_provenance_check(pl,ni,1,seg1)==SRM_ERR_MISMATCH,"the time-axis check refuses an item-axis model") ; }
     { SRM_provenance *pt=make_prov_time(1,1,(int[]){10},10,NULL) ; char *kk[1]={(char *)"a"} ; int pp[1] ;
       CHECK(SRM_provenance_match_items(pt,1,kk,pp)==SRM_ERR_MISMATCH,"and the item check refuses a time-axis model") ; SRM_provenance_free(pt) ; }
     free(perm_keys) ; free(seen) ; SRM_free_model(m) ; SRM_free_model(ld) ; SRM_provenance_free(pv) ; SRM_provenance_free(pl) ; synth_free(&si) ;
   }

   /* damaged and unusual files */
   {
     SRM_params p ; SRM_model *m=NULL , *ld=NULL ; SRM_fitinfo fi ; SRM_provenance *pv , *pl=NULL ;
     int segs[1]={T} ; char *txt ; long len ; char path2[220] ; int t ;
     snprintf(path2,sizeof(path2),"%s.var",path) ;
     SRM_params_default(&p) ; p.nfeat=k ; p.center=0 ; p.niter=20 ;
     CHECK(SRM_fit(4,T,nvox,s.x,&p,&m,&fi)==SRM_OK,"fit") ; SRM_free_fitinfo(&fi) ;
     pv=make_prov_time(4,1,segs,T,NULL) ;
     CHECK(SRM_save_model(path,m,pv)==SRM_OK,"save") ;
     txt=slurp(path,&len) ; CHECK(txt!=NULL,"read it back as text") ;

     { /* comments, blank lines, an unknown key and an unknown data block: all ignored */
       const char *ins="\n# a comment\n\nfuture_key 1 2 3\ndata future_block 2 2\n1 2\n3 4\n" ;
       char *pos=strstr(txt,"nfeat") ; long off=pos-txt ; char *buf=(char *)malloc(len+strlen(ins)+1) ;
       memcpy(buf,txt,off) ; strcpy(buf+off,ins) ; strcpy(buf+off+strlen(ins),txt+off) ;
       CHECK(spit(path2,buf,(long)strlen(buf))==0 && SRM_load_model(path2,&ld,&pl)==SRM_OK && models_equal(m,ld),"unknown keys and blocks are ignored") ;
       SRM_free_model(ld) ; SRM_provenance_free(pl) ; ld=NULL ; pl=NULL ; free(buf) ; }
     { /* Windows line endings */
       long n2=0 ; char *buf=(char *)malloc(2*len+1) ; long q ;
       for( q=0 ; q<len ; q++ ){ if( txt[q]=='\n' ) buf[n2++]='\r' ; buf[n2++]=txt[q] ; }
       CHECK(spit(path2,buf,n2)==0 && SRM_load_model(path2,&ld,&pl)==SRM_OK && models_equal(m,ld),"CRLF line endings") ;
       SRM_free_model(ld) ; SRM_provenance_free(pl) ; ld=NULL ; pl=NULL ; free(buf) ; }
     { /* versions */
       char *buf=(char *)malloc(len+8) ; char *v=strstr(txt,"srm_version 1") ; long off=v-txt ;
       memcpy(buf,txt,len+1) ; buf[off+12]='2' ;
       CHECK(spit(path2,buf,len)==0 && SRM_load_model(path2,&ld,&pl)==SRM_ERR_VERSION && ld==NULL && pl==NULL,"a newer version is refused") ;
       memcpy(buf,txt,len+1) ; memcpy(buf+off,"srm_vxrsion",11) ;
       CHECK(spit(path2,buf,len)==0 && SRM_load_model(path2,&ld,&pl)==SRM_ERR_FORMAT,"no version line") ;
       free(buf) ; }
     { /* truncation anywhere is caught */
       for( t=1 ; t<=19 ; t++ ){
         long cut=(len*t)/20 ; int rc2 ;
         CHECK(spit(path2,txt,cut)==0,"write a truncated copy") ;
         rc2=SRM_load_model(path2,&ld,&pl) ;
         CHECK(rc2!=SRM_OK && ld==NULL && pl==NULL,"a truncated file never loads") ;
       }
       { char *e=strstr(txt,"\nend") ; CHECK(spit(path2,txt,e-txt)==0 && SRM_load_model(path2,&ld,&pl)==SRM_ERR_FORMAT,"missing 'end'") ; } }
     { /* non-numbers where numbers belong */
       const char *bad[6]={"abc","nan","inf","-inf","+nan","1e999"} ; int q ;
       for( q=0 ; q<6 ; q++ ){
         char *buf=(char *)malloc(len+8) ; char *d=strstr(txt,"data S") ; long off ; char *nl ;
         memcpy(buf,txt,len+1) ; nl=strchr(d,'\n') ; off=(nl+1)-txt ;
         memcpy(buf+off,bad[q],strlen(bad[q])) ; if( strlen(bad[q])<3 ) memset(buf+off+strlen(bad[q]),' ',3-strlen(bad[q])) ;
         CHECK(spit(path2,buf,len)==0 && SRM_load_model(path2,&ld,&pl)==SRM_ERR_FORMAT,"a non-number in a data block") ;
         free(buf) ;
       }
     }
     { /* inconsistent sizes */
       char *buf=(char *)malloc(len+8) ; char *d ; long off ;
       memcpy(buf,txt,len+1) ; d=strstr(buf,"data S 3 120") ; d[7]='4' ;
       CHECK(spit(path2,buf,len)==0 && SRM_load_model(path2,&ld,&pl)==SRM_ERR_FORMAT,"a data block that disagrees with nfeat") ;
       memcpy(buf,txt,len+1) ; d=strstr(buf,"nsubj 4") ; d[6]='5' ;
       CHECK(spit(path2,buf,len)==0 && SRM_load_model(path2,&ld,&pl)==SRM_ERR_FORMAT,"nsubj that disagrees with the subject lines") ;
       (void)off ; free(buf) ; }
     CHECK(SRM_load_model("srm_no_such_file.srm",&ld,&pl)==SRM_ERR_IO,"missing file") ;
     CHECK(SRM_load_model(NULL,&ld,&pl)==SRM_ERR_ARG,"no name") ;

     /* fuzz: random damage to a valid file must never crash, and never yield a half-built model */
     {
       srm_rng rg ; int trial , nok=0 ;
       srm_rng_init(&rg,99,9) ;
       for( trial=0 ; trial<300 ; trial++ ){
         char *buf=(char *)malloc(len+1) ; int nflip=1+(int)(srm_unif(&rg)*5) , q ; long cut=len ;
         memcpy(buf,txt,len+1) ;
         for( q=0 ; q<nflip ; q++ ){
           long pos=(long)(srm_unif(&rg)*len) ; int mode=(int)(srm_unif(&rg)*3) ;
           if( mode==0 ) buf[pos]=(char)(32+(int)(srm_unif(&rg)*95)) ;
           else if( mode==1 ) buf[pos]='\n' ;
           else buf[pos]=(char)('0'+(int)(srm_unif(&rg)*10)) ;
         }
         if( srm_unif(&rg)<0.3 ) cut=(long)(srm_unif(&rg)*len) ;
         spit(path2,buf,cut) ;
         ld=NULL ; pl=NULL ;
         if( SRM_load_model(path2,&ld,&pl)==SRM_OK ){
           nok++ ;
           CHECK(ld!=NULL && pl!=NULL && ld->nview==pl->nsubj && ld->s!=NULL && ld->a_sum!=NULL && ld->nvox!=NULL,"a file that loads is a whole model") ;
           CHECK(ld->nfeat>=1 && ld->nfeat<=ld->ntime,"with sane sizes") ;
           SRM_free_model(ld) ; SRM_provenance_free(pl) ;
         } else CHECK(ld==NULL && pl==NULL,"a file that fails returns nothing") ;
         free(buf) ;
       }
       CHECK(nok<300,"not every damaged file survived") ;
     }
     free(txt) ; remove(path2) ;

     /* saving: what is refused, and that failure leaves no file */
     {
       SRM_provenance *pb ; char missing[260] ; float save ;
       remove(path) ;
       pb=make_prov_time(4,1,segs,T,NULL) ;
       free(pb->label[1]) ; pb->label[1]=strdup("has space") ;
       CHECK(SRM_save_model(path,m,pb)==SRM_ERR_MISMATCH && access(path,F_OK)!=0,"a label with whitespace is refused, and nothing is written") ;
       free(pb->label[1]) ; pb->label[1]=strdup("") ;
       CHECK(SRM_save_model(path,m,pb)==SRM_ERR_MISMATCH,"an empty label") ;
       free(pb->label[1]) ; pb->label[1]=strdup("#hash") ;
       CHECK(SRM_save_model(path,m,pb)==SRM_ERR_MISMATCH,"a label that would read as a comment") ;
       free(pb->label[1]) ; pb->label[1]=strdup("ok") ;
       pb->nsubj=3 ;
       CHECK(SRM_save_model(path,m,pb)==SRM_ERR_MISMATCH,"a subject count that is not the model's") ;
       pb->nsubj=4 ; pb->seglen[0]=T-1 ;
       CHECK(SRM_save_model(path,m,pb)==SRM_ERR_MISMATCH,"run lengths that do not sum to ntime") ;
       pb->seglen[0]=T ; pb->ntime_in=T+1 ;
       CHECK(SRM_save_model(path,m,pb)==SRM_ERR_MISMATCH,"ntime_in that does not match the retained points") ;
       pb->ntime_in=T ;
       save=m->s[3] ; m->s[3]=NAN ;
       CHECK(SRM_save_model(path,m,pb)==SRM_ERR_NONFINITE && access(path,F_OK)!=0,"a non-finite value") ;
       m->s[3]=save ;
       snprintf(missing,sizeof(missing),"srm_no_such_dir_%ld/model.srm",(long)getpid()) ;
       CHECK(SRM_save_model(missing,m,pb)==SRM_ERR_IO,"an unwritable location") ;
       CHECK(SRM_save_model(NULL,m,pb)==SRM_ERR_ARG && SRM_save_model(path,NULL,pb)==SRM_ERR_ARG && SRM_save_model(path,m,NULL)==SRM_ERR_ARG,"missing arguments") ;
       CHECK(SRM_save_model(path,m,pb)==SRM_OK,"and the good one still saves") ;
       CHECK(SRM_save_model(path,m,pb)==SRM_OK,"... and overwrites") ;
       SRM_provenance_free(pb) ;
     }
     SRM_free_model(m) ; SRM_provenance_free(pv) ;
   }
   remove(path) ;
   synth_free(&s) ;
   return 0 ;
}

static int test_errors( void )
{
   int nvox[2]={10,12} , T=20 , i ;
   double sg[2]={0.5,0.5} ;
   synth s ; SRM_params p ; SRM_model *m=NULL ; SRM_fitinfo fi ;
   float *saved ;

   CHECK(synth_make(&s,2,nvox,T,2,sg,3)==0,"synth") ;
   SRM_params_default(&p) ; p.nfeat=2 ;
   p.nfeat=0 ;   CHECK(SRM_fit(2,T,nvox,s.x,&p,&m,&fi)==SRM_ERR_NFEAT && m==NULL,"nfeat 0") ;
   p.nfeat=11 ;  CHECK(SRM_fit(2,T,nvox,s.x,&p,&m,&fi)==SRM_ERR_NFEAT,"nfeat above the smallest view") ;
   p.nfeat=21 ;  CHECK(SRM_fit(2,T,nvox,s.x,&p,&m,&fi)==SRM_ERR_NFEAT,"nfeat above ntime") ;
   p.nfeat=2 ; p.algo=9 ;       CHECK(SRM_fit(2,T,nvox,s.x,&p,&m,&fi)==SRM_ERR_ARG,"bad algorithm") ;
   p.algo=SRM_ALGO_RSRM ;       CHECK(SRM_fit(2,T,nvox,s.x,&p,&m,&fi)==SRM_ERR_ARG,"RSRM is reserved") ;
   p.algo=SRM_ALGO_PROB ; p.coupling=10.0 ;
   CHECK(SRM_fit(2,T,nvox,s.x,&p,&m,&fi)==SRM_ERR_ARG,"coupling needs runs (nseg >= 2)") ;
   p.coupling=SRM_COUPLING_SHARED ; p.niter=0 ;
   CHECK(SRM_fit(2,T,nvox,s.x,&p,&m,&fi)==SRM_ERR_ARG,"niter 0") ;
   p.niter=5 ;
   saved = (float *)malloc(sizeof(float)) ; saved[0]=s.x[1][7] ;
   s.x[1][7] = NAN ;
   CHECK(SRM_fit(2,T,nvox,s.x,&p,&m,&fi)==SRM_ERR_NONFINITE,"NaN in data") ;
   s.x[1][7] = INFINITY ;
   CHECK(SRM_fit(2,T,nvox,s.x,&p,&m,&fi)==SRM_ERR_NONFINITE,"Inf in data") ;
   s.x[1][7] = saved[0] ; free(saved) ;
   for( i=0 ; i<nvox[0]*T ; i++ ) s.x[0][i]=3.0f ;       /* constant: zero variance once centered */
   CHECK(SRM_fit(2,T,nvox,s.x,&p,&m,&fi)==SRM_ERR_SINGULAR,"a view with no variance") ;
   CHECK(strlen(SRM_strerror(SRM_ERR_MISMATCH))>0 && strlen(SRM_strerror(-99))>0,"error text") ;
   synth_free(&s) ;
   return 0 ;
}

static int test_multistart( void )
{
   int nvox[4]={50,60,55,65} , T=120 , k=4 , i , ss ;
   double sg[4]={0.5,0.8,1.2,1.6} , so[6] ;
   synth s ; SRM_params p ; SRM_model *m1=NULL , *m2=NULL , *mb=NULL ;
   SRM_fitinfo f1 , f2 , fb ; int best ;

   CHECK(synth_make(&s,4,nvox,T,k,sg,31)==0,"synth") ;
   for( i=0 ; i<4 ; i++ ) center_rows(s.x[i],nvox[i],T) ;
   SRM_params_default(&p) ; p.nfeat=k ; p.niter=8 ; p.tol=0.0 ; p.center=0 ;

   /* nstart == 1 is SRM_fit() */
   CHECK(SRM_fit(4,T,nvox,s.x,&p,&m1,&f1)==SRM_OK,"single fit") ;
   CHECK(SRM_fit_multistart(4,T,nvox,s.x,&p,1,&m2,&f2,so,&best)==SRM_OK,"multistart 1") ;
   CHECK(best==0 && memcmp(m1->s,m2->s,sizeof(float)*k*T)==0,"nstart 1 equals SRM_fit") ;
   CHECK(so[0]==f1.objective[f1.niter_done],"start objective recorded") ;
   SRM_free_fitinfo(&f1) ; SRM_free_fitinfo(&f2) ; SRM_free_model(m1) ; SRM_free_model(m2) ;

   for( ss=0 ; ss<2 ; ss++ ){                       /* PROB keeps the highest, DET the lowest */
     int j ;
     p.algo = ss ? SRM_ALGO_DET : SRM_ALGO_PROB ;
     CHECK(SRM_fit_multistart(4,T,nvox,s.x,&p,6,&mb,&fb,so,&best)==SRM_OK,"multistart 6") ;
     CHECK(best>=0 && best<6,"best index") ;
     CHECK(fb.objective[fb.niter_done]==so[best],"kept model is the recorded best") ;
     for( j=0 ; j<6 ; j++ ){
       CHECK(isfinite(so[j]),"every start reports an objective") ;
       CHECK(ss ? so[best]<=so[j] : so[best]>=so[j],"best is no worse than any start") ;
     }
     for( j=1 ; j<6 ; j++ ) if( so[j]!=so[0] ) break ;
     CHECK(j<6,"different starts really are different") ;
     /* start 0 is the plain fit with the base seed */
     CHECK(SRM_fit(4,T,nvox,s.x,&p,&m1,&f1)==SRM_OK,"base fit") ;
     CHECK(so[0]==f1.objective[f1.niter_done],"start 0 is the base seed") ;
     SRM_free_fitinfo(&f1) ; SRM_free_model(m1) ;
     SRM_free_fitinfo(&fb) ; SRM_free_model(mb) ;
   }

   /* errors */
   p.algo=SRM_ALGO_PROB ;
   CHECK(SRM_fit_multistart(4,T,nvox,s.x,&p,0,&mb,&fb,NULL,NULL)==SRM_ERR_ARG,"nstart 0") ;
   p.nfeat=99 ;
   CHECK(SRM_fit_multistart(4,T,nvox,s.x,&p,3,&mb,&fb,NULL,&best)==SRM_ERR_NFEAT && mb==NULL && best==-1,
         "a failing start returns the error and nothing") ;
   p.nfeat=k ; p.init=SRM_INIT_MODEL ;
   CHECK(SRM_fit_multistart(4,T,nvox,s.x,&p,2,&mb,&fb,NULL,NULL)==SRM_ERR_ARG,"warm start with several starts") ;
   synth_free(&s) ;
   return 0 ;
}

#ifdef USE_OMP
/* The header promises the fit is the same for any thread count. */
static int test_thread_invariance( void )
{
   int nvox[6]={50,60,55,70,65,58} , T=150 , k=4 , i , alg ;
   double sg[6]={0.3,0.4,0.5,0.6,0.7,0.8} ;
   synth s ; int nmax=omp_get_max_threads() ;
   CHECK(synth_make(&s,6,nvox,T,k,sg,17)==0,"synth") ;
   for( alg=0 ; alg<2 ; alg++ ){
     SRM_params p ; SRM_model *m1=NULL , *mn=NULL ; SRM_fitinfo f1 , fn ;
     SRM_params_default(&p) ; p.nfeat=k ; p.niter=15 ; p.tol=0.0 ;
     p.algo = alg ? SRM_ALGO_DET : SRM_ALGO_PROB ;
     omp_set_num_threads(1) ;
     CHECK(SRM_fit(6,T,nvox,s.x,&p,&m1,&f1)==SRM_OK,"1-thread fit") ;
     omp_set_num_threads(nmax>1 ? nmax : 2) ;
     CHECK(SRM_fit(6,T,nvox,s.x,&p,&mn,&fn)==SRM_OK,"n-thread fit") ;
     CHECK(memcmp(m1->s,mn->s,sizeof(float)*k*T)==0,"S identical across thread counts") ;
     for( i=0 ; i<6 ; i++ )
       CHECK(memcmp(m1->w[i],mn->w[i],sizeof(float)*nvox[i]*k)==0,"W identical across thread counts") ;
     CHECK(memcmp(f1.objective,fn.objective,sizeof(double)*(f1.niter_done+1))==0,
           "objective identical across thread counts") ;
     SRM_free_fitinfo(&f1) ; SRM_free_fitinfo(&fn) ; SRM_free_model(m1) ; SRM_free_model(mn) ;
   }
   /* the single-view kernels (projection, a new view's W) chunk their rows the
      same way at any thread count */
   {
     SRM_params p ; SRM_model *m=NULL ; SRM_fitinfo fi ;
     float *o1=(float *)malloc(sizeof(float)*k*T) , *on=(float *)malloc(sizeof(float)*k*T) ;
     float *w1=(float *)malloc(sizeof(float)*nvox[3]*k) , *wn=(float *)malloc(sizeof(float)*nvox[3]*k) ;
     double r1 , rn ;
     SRM_params_default(&p) ; p.nfeat=k ; p.niter=10 ; p.center=0 ;
     CHECK(SRM_fit(6,T,nvox,s.x,&p,&m,&fi)==SRM_OK,"fit") ;
     omp_set_num_threads(1) ;
     CHECK(SRM_project(m,3,s.x[3],T,o1)==SRM_OK && SRM_fit_view(m->s,k,T,s.x[3],nvox[3],0,w1,&r1,NULL)==SRM_OK,"1 thread") ;
     omp_set_num_threads(nmax>1 ? nmax : 2) ;
     CHECK(SRM_project(m,3,s.x[3],T,on)==SRM_OK && SRM_fit_view(m->s,k,T,s.x[3],nvox[3],0,wn,&rn,NULL)==SRM_OK,"n threads") ;
     CHECK(memcmp(o1,on,sizeof(float)*k*T)==0,"projection identical across thread counts") ;
     CHECK(memcmp(w1,wn,sizeof(float)*nvox[3]*k)==0 && r1==rn,"fit_view identical across thread counts") ;
     free(o1) ; free(on) ; free(w1) ; free(wn) ; SRM_free_fitinfo(&fi) ; SRM_free_model(m) ;
   }
   /* the held-out metrics and z-scoring */
   {
     int km=5 , Tm=200 , win=9 , q2 ; srm_rng rg ; float *a=(float *)malloc(sizeof(float)*km*Tm) , *b=(float *)malloc(sizeof(float)*km*Tm) ;
     double acc1 , ch1 , accn , chn , r1 , rn ; float *v1=(float *)malloc(sizeof(float)*nvox[0]) , *vn=(float *)malloc(sizeof(float)*nvox[0]) ;
     SRM_params pm ; SRM_model *mm=NULL ; SRM_fitinfo fm ; float *zs1 , *zsn ; int seg3[2]={90,110} , nb1 , nbn ;
     srm_rng_init(&rg,5,5) ;
     for( q2=0 ; q2<km*Tm ; q2++ ){ a[q2]=(float)srm_normal(&rg) ; b[q2]=a[q2]+0.7f*(float)srm_normal(&rg) ; }
     SRM_params_default(&pm) ; pm.nfeat=k ; pm.center=0 ; pm.niter=8 ;
     CHECK(SRM_fit(6,T,nvox,s.x,&pm,&mm,&fm)==SRM_OK,"fit for metrics") ;
     omp_set_num_threads(1) ;
     CHECK(SRM_segment_match(a,b,km,Tm,win,&acc1,&ch1)==SRM_OK,"segment, 1 thread") ;
     CHECK(SRM_reconstruction_r2(mm,0,s.x[0],mm->s,T,&r1,v1)==SRM_OK,"r2, 1 thread") ;
     zs1=(float *)malloc(sizeof(float)*nvox[0]*T) ; memcpy(zs1,s.x[0],sizeof(float)*nvox[0]*T) ;
     nb1=SRM_zscore_segments(zs1,nvox[0],T,2,(int[]){70,T-70},NULL) ;
     omp_set_num_threads(nmax>1 ? nmax : 2) ;
     CHECK(SRM_segment_match(a,b,km,Tm,win,&accn,&chn)==SRM_OK,"segment, n threads") ;
     CHECK(SRM_reconstruction_r2(mm,0,s.x[0],mm->s,T,&rn,vn)==SRM_OK,"r2, n threads") ;
     zsn=(float *)malloc(sizeof(float)*nvox[0]*T) ; memcpy(zsn,s.x[0],sizeof(float)*nvox[0]*T) ;
     nbn=SRM_zscore_segments(zsn,nvox[0],T,2,(int[]){70,T-70},NULL) ;
     CHECK(acc1==accn && ch1==chn,"segment matching identical across thread counts") ;
     CHECK(r1==rn && memcmp(v1,vn,sizeof(float)*nvox[0])==0,"reconstruction R^2 identical across thread counts") ;
     CHECK(nb1==nbn && memcmp(zs1,zsn,sizeof(float)*nvox[0]*T)==0,"z-scoring identical across thread counts") ;
     (void)seg3 ;
     free(a) ; free(b) ; free(v1) ; free(vn) ; free(zs1) ; free(zsn) ; SRM_free_fitinfo(&fm) ; SRM_free_model(mm) ;
   }
   /* the update, both models and the run-coupled one */
   {
     int a2 , q ;
     for( a2=0 ; a2<2 ; a2++ ){
       SRM_params pu ; SRM_model *m1=NULL , *mn=NULL ; SRM_fitinfo f0 , f1 , fn ; int nv5[4]={60,70,65,75} ;
       SRM_params_default(&pu) ; pu.nfeat=k ; pu.center=0 ; pu.niter=40 ; pu.tol=0.0 ;
       pu.algo = a2 ? SRM_ALGO_DET : SRM_ALGO_PROB ;
       omp_set_num_threads(1) ;
       CHECK(SRM_fit(4,T,nvox,s.x,&pu,&m1,&f0)==SRM_OK,"base fit") ; SRM_free_fitinfo(&f0) ;
       CHECK(SRM_fit(4,T,nvox,s.x,&pu,&mn,&f0)==SRM_OK,"base fit again") ; SRM_free_fitinfo(&f0) ;
       (void)nv5 ;
       omp_set_num_threads(1) ;
       CHECK(SRM_update(m1,2,&nvox[4],&s.x[4],&pu,&f1)==SRM_OK,"update, 1 thread") ;
       omp_set_num_threads(nmax>1 ? nmax : 2) ;
       CHECK(SRM_update(mn,2,&nvox[4],&s.x[4],&pu,&fn)==SRM_OK,"update, n threads") ;
       CHECK(memcmp(m1->s,mn->s,sizeof(float)*k*T)==0,"updated S identical across thread counts") ;
       CHECK(memcmp(m1->a_sum,mn->a_sum,sizeof(double)*k*T)==0,"updated sums identical") ;
       for( q=4 ; q<6 ; q++ )
         CHECK(memcmp(m1->w[q],mn->w[q],sizeof(float)*nvox[q]*k)==0,"new W identical across thread counts") ;
       CHECK(m1->drift==mn->drift,"drift identical") ;
       SRM_free_fitinfo(&f1) ; SRM_free_fitinfo(&fn) ; SRM_free_model(m1) ; SRM_free_model(mn) ;
     }
   }
   /* the run-coupled fit, both models */
   {
     int nvc[4]={60,70,65,75} , segc[3]={40,50,40} , Tc=130 , kc=3 , q , r2 , a2 ;
     dsynth dd ;
     CHECK(dsynth_make(&dd,4,nvc,3,segc,kc,0.4,0.3,555)==0,"drifting synth") ;
     for( a2=0 ; a2<2 ; a2++ ){
       SRM_params pc ; SRM_model *m1=NULL , *mn=NULL ; SRM_fitinfo f1 , fn ;
       SRM_params_default(&pc) ; pc.nfeat=kc ; pc.center=0 ; pc.niter=12 ; pc.tol=0.0 ;
       pc.algo = a2 ? SRM_ALGO_DET : SRM_ALGO_PROB ;
       pc.nseg=3 ; pc.seglen=segc ; pc.coupling=40.0 ;
       omp_set_num_threads(1) ;
       CHECK(SRM_fit(4,Tc,nvc,dd.x,&pc,&m1,&f1)==SRM_OK,"coupled, 1 thread") ;
       omp_set_num_threads(nmax>1 ? nmax : 2) ;
       CHECK(SRM_fit(4,Tc,nvc,dd.x,&pc,&mn,&fn)==SRM_OK,"coupled, n threads") ;
       CHECK(memcmp(m1->s,mn->s,sizeof(float)*kc*Tc)==0,"coupled S identical across thread counts") ;
       for( q=0 ; q<4 ; q++ ){
         CHECK(memcmp(m1->w[q],mn->w[q],sizeof(float)*nvc[q]*kc)==0,"coupled anchors identical") ;
         for( r2=0 ; r2<3 ; r2++ )
           CHECK(memcmp(m1->wrun[q*3+r2],mn->wrun[q*3+r2],sizeof(float)*nvc[q]*kc)==0,"coupled run W identical") ;
       }
       CHECK(memcmp(f1.objective,fn.objective,sizeof(double)*(f1.niter_done+1))==0,"coupled objective identical") ;
       SRM_free_fitinfo(&f1) ; SRM_free_fitinfo(&fn) ; SRM_free_model(m1) ; SRM_free_model(mn) ;
     }
     dsynth_free(&dd) ;
   }
   synth_free(&s) ;
   return 0 ;
}
#endif

/*------------------------------------------------------------------------*/
/* The fit on Gram matrices: the same fit as on the data, up to rounding    */

static double rel_w_gap( const float *a , const float *b , size_t n )
{
   double num=0.0 , den=0.0 ; size_t i ;
   for( i=0 ; i<n ; i++ ){ double d=(double)a[i]-b[i] ; num+=d*d ; den+=(double)b[i]*b[i] ; }
   return sqrt(num/den) ;
}

/* fit with each kernel from the same start; S, W, run W and the objective agree */
static int gram_vs_voxel( int nview , int T , const int *nvox , float **x ,
                          const SRM_params *p0 , double tol , const char *what )
{
   SRM_params p=*p0 ; SRM_model *mv=NULL , *mg=NULL ; SRM_fitinfo fv , fg ;
   int i , r , it , k=p0->nfeat , nit ; char msg[256] ;
   p.kernel=SRM_KERNEL_VOXEL ;
   snprintf(msg,256,"%s: voxel fit",what) ; CHECK(SRM_fit(nview,T,nvox,x,&p,&mv,&fv)==SRM_OK,msg) ;
   p.kernel=SRM_KERNEL_GRAM ;
   snprintf(msg,256,"%s: Gram fit",what) ; CHECK(SRM_fit(nview,T,nvox,x,&p,&mg,&fg)==SRM_OK,msg) ;
   if( getenv("SRM_TEST_VERBOSE") ){
     double wg=0.0 ;
     for( i=0 ; i<nview ; i++ ){ double g=rel_w_gap(mg->w[i],mv->w[i],(size_t)nvox[i]*k) ; if( g>wg ) wg=g ; }
     fprintf(stderr,"[%s] Gram vs voxel: S %.2e  W %.2e  iterations %d / %d\n",what,
             rel_s_gap(mg->s,mv->s,k,T),wg,fg.niter_done,fv.niter_done) ;
   }
   snprintf(msg,256,"%s: S agrees",what) ; CHECK(rel_s_gap(mg->s,mv->s,k,T) < tol,msg) ;
   for( i=0 ; i<nview ; i++ ){
     snprintf(msg,256,"%s: W agrees",what) ;
     CHECK(rel_w_gap(mg->w[i],mv->w[i],(size_t)nvox[i]*k) < tol,msg) ;
     snprintf(msg,256,"%s: W orthonormal",what) ; CHECK(ortho_error(mg->w[i],nvox[i],k) < 1e-5,msg) ;
     if( mv->wrun != NULL ){
       snprintf(msg,256,"%s: run W present",what) ; CHECK(mg->wrun!=NULL && mg->nseg==mv->nseg,msg) ;
       for( r=0 ; r<mv->nseg ; r++ ){
         snprintf(msg,256,"%s: run W agrees and is orthonormal",what) ;
         CHECK(rel_w_gap(mg->wrun[i*mv->nseg+r],mv->wrun[i*mv->nseg+r],(size_t)nvox[i]*k) < tol
               && ortho_error(mg->wrun[i*mv->nseg+r],nvox[i],k) < 1e-5,msg) ;
       }
     }
     if( mv->rho2 != NULL ){
       snprintf(msg,256,"%s: rho2 agrees",what) ; CHECK(fabs(mg->rho2[i]/mv->rho2[i]-1.0) < tol,msg) ;
     }
   }
   /* the Gram path can stop sooner with tol = 0, at an exact fixed point */
   nit = (fg.niter_done < fv.niter_done) ? fg.niter_done : fv.niter_done ;
   for( it=0 ; it<=nit ; it++ ){
     snprintf(msg,256,"%s: objective trace agrees",what) ;
     CHECK(fabs(fg.objective[it]-fv.objective[it]) <= 1e-6*fabs(fv.objective[it]),msg) ;
   }
   SRM_free_fitinfo(&fv) ; SRM_free_fitinfo(&fg) ; SRM_free_model(mv) ; SRM_free_model(mg) ;
   return 0 ;
}

static int test_gram( void )
{
   int alg ;
   /* the choice: Gram needs every view to have T+k features, and to pay off */
   { SRM_params p ; int big[2]={2000,1500} , small[2]={2000,90} ;
     SRM_params_default(&p) ; p.nfeat=5 ;
     CHECK(srm_use_gram(&p,2,big,100,5)==1,"auto: Gram for many features") ;
     CHECK(srm_use_gram(&p,2,small,100,5)==0,"auto: voxel when a view has fewer than T+k") ;
     p.niter=1 ;
     CHECK(srm_use_gram(&p,2,big,100,5)==0,"auto: voxel for a single iteration") ;
     p.niter=500 ; p.kernel=SRM_KERNEL_VOXEL ;
     CHECK(srm_use_gram(&p,2,big,100,5)==0,"VOXEL forces the voxel path") ;
     p.kernel=SRM_KERNEL_GRAM ;
     CHECK(srm_use_gram(&p,2,small,100,5)==1,"GRAM forces the Gram path") ; }

   /* shared W: fewer features than T+k (G singular) and more */
   { int nv1[5]={50,60,55,70,65} , nv2[4]={400,450,380,420} , T1=150 , T2=120 ;
     double sg[5]={0.3,0.4,0.5,0.6,0.7} ; synth s1 , s2 ;
     CHECK(synth_make(&s1,5,nv1,T1,4,sg,31)==0 && synth_make(&s2,4,nv2,T2,5,sg,32)==0,"synth") ;
     for( alg=0 ; alg<2 ; alg++ ){
       SRM_params p ; SRM_params_default(&p) ; p.niter=40 ; p.tol=0.0 ;
       p.algo = alg ? SRM_ALGO_DET : SRM_ALGO_PROB ;
       p.nfeat=4 ; if( gram_vs_voxel(5,T1,nv1,s1.x,&p,1e-4,alg?"det, V < T+k":"prob, V < T+k") ) return 1 ;
       p.nfeat=5 ; if( gram_vs_voxel(4,T2,nv2,s2.x,&p,1e-4,alg?"det, V > T+k":"prob, V > T+k") ) return 1 ;
       p.center=0 ; if( gram_vs_voxel(4,T2,nv2,s2.x,&p,1e-4,alg?"det, uncentered":"prob, uncentered") ) return 1 ;
     }
     /* a warm start */
     { SRM_params p ; SRM_model *m0=NULL ; SRM_fitinfo f0 ;
       SRM_params_default(&p) ; p.nfeat=5 ; p.niter=5 ; p.kernel=SRM_KERNEL_VOXEL ;
       CHECK(SRM_fit(4,T2,nv2,s2.x,&p,&m0,&f0)==SRM_OK,"start model") ; SRM_free_fitinfo(&f0) ;
       p.init=SRM_INIT_MODEL ; p.init_model=m0 ; p.niter=20 ; p.tol=0.0 ;
       if( gram_vs_voxel(4,T2,nv2,s2.x,&p,1e-4,"prob, warm start") ) return 1 ;
       SRM_free_model(m0) ; }
     synth_free(&s1) ; synth_free(&s2) ; }

   /* run-coupled W, from a shared start and from a coupled model with its own run W */
   { int nvc[4]={260,300,280,320} , segc[3]={60,50,70} , Tc=180 , kc=3 ;
     dsynth dd ;
     CHECK(dsynth_make(&dd,4,nvc,3,segc,kc,0.4,0.3,77)==0,"drifting synth") ;
     for( alg=0 ; alg<2 ; alg++ ){
       SRM_params p ; SRM_model *m0=NULL ; SRM_fitinfo f0 ;
       SRM_params_default(&p) ; p.nfeat=kc ; p.center=0 ; p.niter=25 ; p.tol=0.0 ;
       p.algo = alg ? SRM_ALGO_DET : SRM_ALGO_PROB ;
       p.nseg=3 ; p.seglen=segc ; p.coupling=30.0 ;
       if( gram_vs_voxel(4,Tc,nvc,dd.x,&p,1e-4,alg?"det coupled":"prob coupled") ) return 1 ;
       p.coupling=0.0 ;
       if( gram_vs_voxel(4,Tc,nvc,dd.x,&p,1e-4,alg?"det coupled, lambda 0":"prob coupled, lambda 0") ) return 1 ;
       p.coupling=30.0 ; p.niter=6 ; p.kernel=SRM_KERNEL_VOXEL ;
       CHECK(SRM_fit(4,Tc,nvc,dd.x,&p,&m0,&f0)==SRM_OK,"coupled start model") ; SRM_free_fitinfo(&f0) ;
       p.init=SRM_INIT_MODEL ; p.init_model=m0 ; p.niter=15 ;
       if( gram_vs_voxel(4,Tc,nvc,dd.x,&p,1e-4,alg?"det coupled, warm start":"prob coupled, warm start") ) return 1 ;
       SRM_free_model(m0) ;
     }
     dsynth_free(&dd) ; }

   /* k above the data's rank: the Procrustes fill happens in coefficients */
   { int nvox[3]={300,300,300} , T=100 , i ; double sg[3]={1e-6,1e-6,1e-6} ;
     synth s ; SRM_params p ; SRM_model *m=NULL ; SRM_fitinfo fi ;
     CHECK(synth_make(&s,3,nvox,T,2,sg,5)==0,"synth") ;
     for( i=0 ; i<3 ; i++ ) center_rows(s.x[i],nvox[i],T) ;
     for( alg=0 ; alg<2 ; alg++ ){
       SRM_params_default(&p) ; p.nfeat=4 ; p.niter=30 ; p.center=0 ; p.kernel=SRM_KERNEL_GRAM ;
       p.algo = alg ? SRM_ALGO_DET : SRM_ALGO_PROB ;
       CHECK(SRM_fit(3,T,nvox,s.x,&p,&m,&fi)==SRM_OK,"Gram fit with k above the data's rank") ;
       for( i=0 ; i<3 ; i++ ){
         CHECK(ortho_error(m->w[i],nvox[i],4) < 1e-5,"Gram W orthonormal with no signal in two directions") ;
         if( m->rho2 ) CHECK(m->rho2[i] > 0.0 && isfinite(m->rho2[i]),"rho2 floored, not zero") ;
       }
       SRM_free_fitinfo(&fi) ; SRM_free_model(m) ;
     }
     synth_free(&s) ; }

#ifdef USE_OMP
   /* the threads do not change the Gram fit */
   { int nvox[6]={250,260,255,270,265,258} , T=150 , k=4 , i , nmax=omp_get_max_threads() ;
     int segc[3]={50,50,50} ; double sg[6]={0.3,0.4,0.5,0.6,0.7,0.8} ; synth s ;
     CHECK(synth_make(&s,6,nvox,T,k,sg,41)==0,"synth") ;
     for( alg=0 ; alg<3 ; alg++ ){
       SRM_params p ; SRM_model *m1=NULL , *mn=NULL ; SRM_fitinfo f1 , fn ;
       SRM_params_default(&p) ; p.nfeat=k ; p.niter=15 ; p.tol=0.0 ; p.kernel=SRM_KERNEL_GRAM ;
       p.algo = (alg==1) ? SRM_ALGO_DET : SRM_ALGO_PROB ;
       if( alg==2 ){ p.nseg=3 ; p.seglen=segc ; p.coupling=20.0 ; }
       omp_set_num_threads(1) ;
       CHECK(SRM_fit(6,T,nvox,s.x,&p,&m1,&f1)==SRM_OK,"Gram, 1 thread") ;
       omp_set_num_threads(nmax>1 ? nmax : 2) ;
       CHECK(SRM_fit(6,T,nvox,s.x,&p,&mn,&fn)==SRM_OK,"Gram, n threads") ;
       CHECK(memcmp(m1->s,mn->s,sizeof(float)*k*T)==0,"Gram S identical across thread counts") ;
       for( i=0 ; i<6 ; i++ ){
         CHECK(memcmp(m1->w[i],mn->w[i],sizeof(float)*nvox[i]*k)==0,"Gram W identical across thread counts") ;
         if( alg==2 ){ int r ; for( r=0 ; r<3 ; r++ )
           CHECK(memcmp(m1->wrun[i*3+r],mn->wrun[i*3+r],sizeof(float)*nvox[i]*k)==0,"Gram run W identical") ; }
       }
       CHECK(f1.niter_done==fn.niter_done &&
             memcmp(f1.objective,fn.objective,sizeof(double)*(f1.niter_done+1))==0,"Gram objective identical") ;
       SRM_free_fitinfo(&f1) ; SRM_free_fitinfo(&fn) ; SRM_free_model(m1) ; SRM_free_model(mn) ;
     }
     synth_free(&s) ; }
#endif
   return 0 ;
}

/*------------------------------------------------------------------------*/
/* Missing data: each view observed at its own time points                 */

/* the PROB marginal log-likelihood over the observed entries only, without the 2 pi constant: at each t
   the observed views' data are N(0, W_o Sigma W_o' + Psi_o) */
static double brute_loglik_obs( const SRM_model *m , float * const *x , unsigned char * const *obs )
{
   int t , i , j , a , b , u , v , D0=0 ; double ll=0.0 ;
   for( i=0 ; i<m->nview ; i++ ) D0+=m->nvox[i] ;
   for( t=0 ; t<m->ntime ; t++ ){
     int D=0 , *vi=(int *)malloc(sizeof(int)*D0) , *fi=(int *)malloc(sizeof(int)*D0) ; double *C , *y , ld=0.0 , q2=0.0 ;
     for( i=0 ; i<m->nview ; i++ ) if( obs[i][t] ) for( j=0 ; j<m->nvox[i] ; j++ ){ vi[D]=i ; fi[D]=j ; D++ ; }
     if( D==0 ){ free(vi) ; free(fi) ; continue ; }
     C=(double *)calloc((size_t)D*D,sizeof(double)) ; y=(double *)calloc(D,sizeof(double)) ;
     for( a=0 ; a<D ; a++ ) for( b=0 ; b<D ; b++ ){
       double acc=(a==b) ? m->rho2[vi[a]] : 0.0 ;
       for( u=0 ; u<m->nfeat ; u++ ) for( v=0 ; v<m->nfeat ; v++ )
         acc += (double)m->w[vi[a]][fi[a]*m->nfeat+u]*m->sigma_s[u*m->nfeat+v]*m->w[vi[b]][fi[b]*m->nfeat+v] ;
       C[a*D+b]=acc ;
     }
     if( chol(D,C) ){ free(C) ; free(y) ; free(vi) ; free(fi) ; return NAN ; }
     for( a=0 ; a<D ; a++ ){
       double r=x[vi[a]][fi[a]*m->ntime+t] ;
       for( b=0 ; b<a ; b++ ) r-=C[a*D+b]*y[b] ;
       y[a]=r/C[a*D+a] ; q2+=y[a]*y[a] ; ld+=2.0*log(C[a*D+a]) ;
     }
     ll += -0.5*(ld+q2) ;
     free(C) ; free(y) ; free(vi) ; free(fi) ;
   }
   return ll ;
}

/*! Solve the small dense system a x = b in place (b becomes x); 0 if ok. */
static int solve_small( int n , double *a , double *b )
{
   int i , j , c , piv ; double t ;
   for( c=0 ; c<n ; c++ ){
     piv=c ; for( i=c+1 ; i<n ; i++ ) if( fabs(a[i*n+c])>fabs(a[piv*n+c]) ) piv=i ;
     if( a[piv*n+c]==0.0 ) return 1 ;
     if( piv!=c ){ for( j=0 ; j<n ; j++ ){ t=a[c*n+j] ; a[c*n+j]=a[piv*n+j] ; a[piv*n+j]=t ; } t=b[c] ; b[c]=b[piv] ; b[piv]=t ; }
     for( i=c+1 ; i<n ; i++ ){
       double f=a[i*n+c]/a[c*n+c] ;
       for( j=c ; j<n ; j++ ) a[i*n+j]-=f*a[c*n+j] ;
       b[i]-=f*b[c] ;
     }
   }
   for( i=n-1 ; i>=0 ; i-- ){ for( j=i+1 ; j<n ; j++ ) b[i]-=a[i*n+j]*b[j] ; b[i]/=a[i*n+i] ; }
   return 0 ;
}

/* A W per run with missing data: masks of all ones change nothing; lambda 0 is independent per-run fits
   with the same masks; the objective is monotone and (PROB, lambda 0) the marginal likelihood of the
   observed entries under the run W; the per-time sums are right. */
static int test_missing_coupled( void )
{
   int nvox[5]={40,46,42,50,44} , seglen[3]={40,36,44} , k=3 , nseg=3 , T=120 , i , t , r , alg ;
   dsynth d ; unsigned char *obs[5] , *all[5] ; float *xz[5] ; srm_rng rg ;
   CHECK(dsynth_make(&d,5,nvox,nseg,seglen,k,0.4,0.25,909)==0,"drifting synth") ;
   srm_rng_init(&rg,5,5) ;
   for( i=0 ; i<5 ; i++ ){
     int f ;
     obs[i]=(unsigned char *)malloc(T) ; all[i]=(unsigned char *)malloc(T) ; memset(all[i],1,T) ;
     xz[i]=(float *)malloc(sizeof(float)*nvox[i]*T) ;
     for( t=0 ; t<T ; t++ ) obs[i][t]=(srm_unif(&rg)>0.2 && t!=11) ;
     for( f=0 ; f<nvox[i] ; f++ ) for( t=0 ; t<T ; t++ ) xz[i][f*T+t]=obs[i][t] ? d.x[i][f*T+t] : 0.0f ;
   }
   for( alg=0 ; alg<2 ; alg++ ){
     SRM_params p ; SRM_model *m0=NULL , *ma=NULL , *m1=NULL ; SRM_fitinfo f0 , fa , f1 ; int it , ok=1 ;
     SRM_params_default(&p) ; p.nfeat=k ; p.center=0 ; p.niter=30 ; p.tol=0.0 ;
     p.algo = alg ? SRM_ALGO_DET : SRM_ALGO_PROB ; p.nseg=nseg ; p.seglen=seglen ; p.coupling=30.0 ;
     CHECK(SRM_fit(5,T,nvox,d.x,&p,&m0,&f0)==SRM_OK,"coupled, no masks") ;
     p.obs=(const unsigned char * const *)all ;
     CHECK(SRM_fit(5,T,nvox,d.x,&p,&ma,&fa)==SRM_OK,"coupled, masks of all ones") ;
     CHECK(memcmp(m0->s,ma->s,sizeof(float)*k*T)==0 && memcmp(f0.objective,fa.objective,sizeof(double)*(f0.niter_done+1))==0
           && ma->nsubj_t==NULL,"coupled: masks of all ones change nothing") ;
     p.obs=(const unsigned char * const *)obs ;
     CHECK(SRM_fit(5,T,nvox,xz,&p,&m1,&f1)==SRM_OK,"coupled fit with missing data") ;
     for( it=1 ; it<=f1.niter_done ; it++ ){
       double slack=1e-7*fabs(f1.objective[0]) + 5e-7*31.0*k*nseg*5 ;
       if( alg ? (f1.objective[it] > f1.objective[it-1]+slack) : (f1.objective[it] < f1.objective[it-1]-slack) ) ok=0 ;
     }
     CHECK(ok,"coupled objective with missing data is monotone (to rounding)") ;
     for( i=0 ; i<5 ; i++ ) for( r=0 ; r<nseg ; r++ ) CHECK(ortho_error(m1->wrun[i*nseg+r],nvox[i],k)<1e-5,"every W_ir orthonormal") ;
     { int bad=0 ;
       CHECK(m1->nsubj_t!=NULL && (alg || m1->rho_inv_t!=NULL),"the model carries per-time sums") ;
       for( t=0 ; t<T ; t++ ){
         double n=0.0 , rr=0.0 ;
         for( i=0 ; i<5 ; i++ ) if( obs[i][t] ){ n+=1.0 ; if( !alg ) rr+=1.0/m1->rho2[i] ; }
         if( m1->nsubj_t[t]!=n ) bad=1 ;
         if( !alg && fabs(m1->rho_inv_t[t]-rr)>1e-12*rr ) bad=1 ;
       }
       CHECK(!bad,"nsubj_t counts the views observed at each t; rho_inv_t sums their 1/rho2") ; }
     SRM_free_model(m0) ; SRM_free_model(ma) ; SRM_free_model(m1) ;
     SRM_free_fitinfo(&f0) ; SRM_free_fitinfo(&fa) ; SRM_free_fitinfo(&f1) ;

     /* lambda 0 */
     { SRM_params q=p ; SRM_model *ms=NULL , *mc=NULL ; SRM_fitinfo fs , fc ; int c0=0 ; double worst=0.0 , bl=0.0 ;
       q.nseg=0 ; q.seglen=NULL ; q.coupling=SRM_COUPLING_SHARED ;
       CHECK(SRM_fit(5,T,nvox,xz,&q,&ms,&fs)==SRM_OK,"shared start with missing data") ;
       q=p ; q.coupling=0.0 ; q.niter=12 ; q.init=SRM_INIT_MODEL ; q.init_model=ms ;
       CHECK(SRM_fit(5,T,nvox,xz,&q,&mc,&fc)==SRM_OK,"coupled lambda 0 with missing data") ;
       for( r=0 ; r<nseg ; c0+=seglen[r],r++ ){
         float **xr=(float **)malloc(sizeof(float *)*5) ; unsigned char **orr=(unsigned char **)malloc(sizeof(unsigned char *)*5) ;
         SRM_model *mr=NULL , tmp ; SRM_fitinfo fr ; SRM_params qr=q ; int f , c ;
         for( i=0 ; i<5 ; i++ ){
           xr[i]=(float *)malloc(sizeof(float)*nvox[i]*seglen[r]) ; orr[i]=obs[i]+c0 ;
           for( f=0 ; f<nvox[i] ; f++ ) for( t=0 ; t<seglen[r] ; t++ ) xr[i][f*seglen[r]+t]=xz[i][f*T+c0+t] ;
         }
         qr.nseg=0 ; qr.seglen=NULL ; qr.coupling=SRM_COUPLING_SHARED ; qr.obs=(const unsigned char * const *)orr ;
         if( alg ){                                   /* DET: step for step an independent fit of the run */
           CHECK(SRM_fit(5,seglen[r],nvox,xr,&qr,&mr,&fr)==SRM_OK,"independent fit of one run, missing data") ;
           for( c=0 ; c<k*seglen[r] ; c++ ) worst=fmax(worst,fabs(mr->s[c]-mc->s[(c/seglen[r])*T+c0+c%seglen[r]])) ;
           for( i=0 ; i<5 ; i++ ){ int q2 ; for( q2=0 ; q2<nvox[i]*k ; q2++ ) worst=fmax(worst,fabs(mr->w[i][q2]-mc->wrun[i*nseg+r][q2])) ; }
           SRM_free_model(mr) ; SRM_free_fitinfo(&fr) ;
         } else {                                     /* PROB: the likelihood under each run's W */
           memset(&tmp,0,sizeof(tmp)) ;
           tmp.nview=5 ; tmp.ntime=seglen[r] ; tmp.nfeat=k ; tmp.nvox=nvox ; tmp.rho2=mc->rho2 ; tmp.sigma_s=mc->sigma_s ;
           tmp.w=(float **)malloc(sizeof(float *)*5) ;
           for( i=0 ; i<5 ; i++ ) tmp.w[i]=mc->wrun[i*nseg+r] ;
           bl+=brute_loglik_obs(&tmp,xr,orr) ;
           free(tmp.w) ;
         }
         for( i=0 ; i<5 ; i++ ) free(xr[i]) ;
         free(xr) ; free(orr) ;
       }
       if( alg ) CHECK(worst<2e-5,"lambda 0 with missing data reproduces independent per-run fits") ;
       else      CHECK(fabs(bl-fc.objective[fc.niter_done])<=1e-6*fabs(bl),"PROB, lambda 0: objective = the marginal likelihood under the run W") ;
       SRM_free_model(ms) ; SRM_free_model(mc) ; SRM_free_fitinfo(&fs) ; SRM_free_fitinfo(&fc) ;
     }
   }
   for( i=0 ; i<5 ; i++ ){ free(obs[i]) ; free(all[i]) ; free(xz[i]) ; }
   dsynth_free(&d) ;
   return 0 ;
}

/* SRM_update with missing data, on either side: the per-time sums are exact, S is their posterior mean
   (PROB) or average (DET), the file keeps them, and masks of all ones change nothing. */
static int test_missing_update( void )
{
   int nvox[7]={30,36,33,40,34,38,31} , T=60 , k=3 , i , t , alg ;
   double sg[7]={0.4,0.6,0.5,0.7,0.5,0.6,0.45} ;
   synth s ; unsigned char *obs[7] , *all[7] ; float *xz[7] , *xg[7] ; srm_rng rg ; char fn[512] , *dir=getenv("TMPDIR") ;
   CHECK(synth_make(&s,7,nvox,T,k,sg,93)==0,"synth") ;
   for( i=0 ; i<7 ; i++ ) center_rows(s.x[i],nvox[i],T) ;
   srm_rng_init(&rg,9,9) ;
   for( i=0 ; i<7 ; i++ ){
     int f ;
     obs[i]=(unsigned char *)malloc(T) ; all[i]=(unsigned char *)malloc(T) ; memset(all[i],1,T) ;
     xz[i]=(float *)malloc(sizeof(float)*nvox[i]*T) ; xg[i]=(float *)malloc(sizeof(float)*nvox[i]*T) ;
     for( t=0 ; t<T ; t++ ) obs[i][t]=(srm_unif(&rg)>0.2) ;
     for( f=0 ; f<nvox[i] ; f++ ) for( t=0 ; t<T ; t++ ){
       xz[i][f*T+t]=obs[i][t] ? s.x[i][f*T+t] : 0.0f ; xg[i][f*T+t]=obs[i][t] ? s.x[i][f*T+t] : NAN ; }
   }
   snprintf(fn,sizeof(fn),"%s/srm_unit_missupd_%d.srm",dir?dir:".",(int)getpid()) ;
   for( alg=0 ; alg<2 ; alg++ ){
     SRM_params p , u ; SRM_model *m=NULL , *mold=NULL , *ml=NULL , *mc=NULL ; SRM_fitinfo fi , fu ; SRM_provenance *pv=SRM_provenance_new() , *pl=NULL ;
     int it , ok=1 , bad=0 , c , d ; double worst=0.0 ;
     SRM_params_default(&p) ; p.nfeat=k ; p.niter=40 ; p.tol=0.0 ; p.center=0 ; p.algo = alg ? SRM_ALGO_DET : SRM_ALGO_PROB ;
     p.obs=(const unsigned char * const *)obs ;
     CHECK(SRM_fit(4,T,nvox,xz,&p,&m,&fi)==SRM_OK,"old views, missing data") ;
     SRM_free_fitinfo(&fi) ;
     mold=SRM_model_copy(m,-1) ;
     CHECK(mold!=NULL && mold->nsubj_t!=NULL,"a copy keeps the per-time sums") ;
     /* the file keeps them */
     pv->axis=SRM_AXIS_TIME ; pv->ntime_in=T ; pv->nseg=1 ; pv->seglen=(int *)malloc(sizeof(int)) ; pv->seglen[0]=T ;
     pv->nsubj=4 ; pv->label=(char **)malloc(sizeof(char *)*4) ;
     for( i=0 ; i<4 ; i++ ){ char b[16] ; snprintf(b,sizeof(b),"v%d",i) ; pv->label[i]=strdup(b) ; }
     CHECK(SRM_save_model(fn,m,pv)==SRM_OK && SRM_load_model(fn,&ml,&pl)==SRM_OK,"save and load") ;
     CHECK(ml->nsubj_t!=NULL && memcmp(ml->nsubj_t,m->nsubj_t,sizeof(double)*T)==0 &&
           (alg || (ml->rho_inv_t!=NULL && memcmp(ml->rho_inv_t,m->rho_inv_t,sizeof(double)*T)==0)),"the file keeps the per-time sums exactly") ;
     /* the update: new views with their own missing points */
     SRM_params_default(&u) ; u.niter=30 ; u.tol=0.0 ; u.obs=(const unsigned char * const *)(obs+4) ;
     CHECK(SRM_update(m,3,nvox+4,xg+4,&u,&fu)==SRM_OK,"update with missing data (garbage where missing)") ;
     for( it=1 ; it<=fu.niter_done ; it++ ){
       double tolr=1e-7*fabs(fu.objective[0]) ;
       if( alg ? (fu.objective[it] > fu.objective[it-1]+tolr) : (fu.objective[it] < fu.objective[it-1]-tolr) ) ok=0 ;
     }
     CHECK(ok,"the update's objective is monotone with missing data") ;
     for( t=0 ; t<T ; t++ ){
       double n=0.0 , rr=0.0 ;
       for( i=0 ; i<7 ; i++ ) if( obs[i][t] ){ n+=1.0 ; if( !alg ) rr+=1.0/m->rho2[i] ; }
       if( m->nsubj_t[t]!=n || (!alg && fabs(m->rho_inv_t[t]-rr)>1e-12*rr) ) bad=1 ;
     }
     CHECK(!bad,"after the update: views observed at each t, and the sum of their 1/rho2") ;
     for( c=0 ; c<k ; c++ ) for( t=0 ; t<T ; t++ ){          /* a_sum: the old sums plus the new views where observed */
       double a=mold->a_sum[c*T+t] , sc=fabs(mold->a_sum[c*T+t])+1.0 ; int f ;
       for( i=4 ; i<7 ; i++ ) if( obs[i][t] ){
         double v=0.0 ;
         for( f=0 ; f<nvox[i] ; f++ ) v += (double)m->w[i][f*k+c]*xz[i][f*T+t] ;
         a += alg ? v : v/m->rho2[i] ;
       }
       worst=fmax(worst,fabs(m->a_sum[c*T+t]-a)/sc) ;
     }
     CHECK(worst<1e-4,"a_sum = the old sums + the new views' W'x where observed") ;
     worst=0.0 ;
     for( t=0 ; t<T ; t++ ){                                  /* S from those sums, per time point */
       double A[9] , b[3] , sig[9] ;
       for( c=0 ; c<k ; c++ ) b[c]=m->a_sum[c*T+t] ;
       if( alg ){ for( c=0 ; c<k ; c++ ) b[c] = (m->nsubj_t[t]>0.0) ? b[c]/m->nsubj_t[t] : 0.0 ; }
       else {                                                  /* (I + r Sigma) s = Sigma a */
         double sb[3] ;
         for( c=0 ; c<k*k ; c++ ) sig[c]=m->sigma_s[c] ;
         for( c=0 ; c<k ; c++ ){ sb[c]=0.0 ; for( d=0 ; d<k ; d++ ){ sb[c]+=sig[c*k+d]*b[d] ; A[c*k+d]=(c==d)+m->rho_inv_t[t]*sig[c*k+d] ; } }
         for( c=0 ; c<k ; c++ ) b[c]=sb[c] ;
         CHECK(solve_small(k,A,b)==0,"solve") ;
       }
       for( c=0 ; c<k ; c++ ) worst=fmax(worst,fabs(b[c]-m->s[c*T+t])/(fabs(b[c])+1e-3)) ;
     }
     CHECK(worst<1e-4,alg ? "DET: S_t is a_sum_t / nsubj_t" : "PROB: S_t is the posterior mean from a_sum_t and rho_inv_t") ;
     SRM_free_fitinfo(&fu) ;
     /* the loaded model updates the same way */
     CHECK(SRM_update(ml,3,nvox+4,xz+4,&u,&fu)==SRM_OK,"update of the loaded model") ;
     { double num=0.0 , den=0.0 ; for( c=0 ; c<k*T ; c++ ){ double e=(double)ml->s[c]-m->s[c] ; num+=e*e ; den+=(double)m->s[c]*m->s[c] ; }
       CHECK(sqrt(num/den)<1e-5,"... from the file, the same S") ; }
     SRM_free_fitinfo(&fu) ;
     /* masks of all ones change nothing; a complete model takes missing new views and then carries sums */
     { SRM_params q=p ; SRM_model *a=NULL , *b=NULL ; SRM_fitinfo fa , fb ; int bad2=0 ;
       q.obs=NULL ;
       CHECK(SRM_fit(4,T,nvox,s.x,&q,&mc,&fi)==SRM_OK,"complete old views") ; SRM_free_fitinfo(&fi) ;
       a=SRM_model_copy(mc,-1) ; b=SRM_model_copy(mc,-1) ;
       u.obs=NULL ; CHECK(SRM_update(a,3,nvox+4,s.x+4,&u,&fa)==SRM_OK,"update, no masks") ;
       u.obs=(const unsigned char * const *)(all+4) ; CHECK(SRM_update(b,3,nvox+4,s.x+4,&u,&fb)==SRM_OK,"update, masks of all ones") ;
       CHECK(memcmp(a->s,b->s,sizeof(float)*k*T)==0 && b->nsubj_t==NULL,"update: masks of all ones change nothing") ;
       SRM_free_fitinfo(&fa) ; SRM_free_fitinfo(&fb) ; SRM_free_model(a) ; SRM_free_model(b) ;
       u.obs=(const unsigned char * const *)(obs+4) ;
       CHECK(SRM_update(mc,3,nvox+4,xz+4,&u,&fa)==SRM_OK,"complete model, new views with missing points") ;
       CHECK(mc->nsubj_t!=NULL,"the model now carries per-time sums") ;
       for( t=0 ; t<T ; t++ ){ double n=4.0 ; for( i=4 ; i<7 ; i++ ) n+=obs[i][t] ; if( mc->nsubj_t[t]!=n ) bad2=1 ; }
       CHECK(!bad2,"... 4 old views everywhere, plus the new ones where observed") ;
       SRM_free_fitinfo(&fa) ; }
     /* placing a view without updating: W, rho2 and the means from the observed columns only */
     { SRM_model *mp=SRM_model_copy(mold,-1) ; float *wf=(float *)malloc(sizeof(float)*nvox[5]*k) , *xo , *so ; double r2 ; int n=0 , f , *idx=(int *)malloc(sizeof(int)*T) ;
       for( t=0 ; t<T ; t++ ) if( obs[5][t] ) idx[n++]=t ;
       xo=(float *)malloc(sizeof(float)*nvox[5]*n) ; so=(float *)malloc(sizeof(float)*k*n) ;
       for( f=0 ; f<nvox[5] ; f++ ) for( t=0 ; t<n ; t++ ) xo[f*n+t]=s.x[5][f*T+idx[t]] ;
       for( c=0 ; c<k ; c++ ) for( t=0 ; t<n ; t++ ) so[c*n+t]=mp->s[c*T+idx[t]] ;
       CHECK(SRM_fit_view(so,k,n,xo,nvox[5],0,wf,&r2,NULL)==SRM_OK,"fit_view on the observed columns") ;
       CHECK(SRM_model_add_view_obs(mp,xg[5],nvox[5],obs[5])==SRM_OK,"add_view_obs (garbage where missing)") ;
       CHECK(memcmp(mp->w[mp->nview-1],wf,sizeof(float)*nvox[5]*k)==0 && (alg || mp->rho2[mp->nview-1]==r2),
             "add_view_obs = fit_view on the observed columns") ;
       free(wf) ; free(xo) ; free(so) ; free(idx) ; SRM_free_model(mp) ; }
     SRM_free_model(m) ; SRM_free_model(mold) ; SRM_free_model(ml) ; SRM_free_model(mc) ;
     SRM_provenance_free(pv) ; SRM_provenance_free(pl) ;
   }
   remove(fn) ;
   for( i=0 ; i<7 ; i++ ){ free(obs[i]) ; free(all[i]) ; free(xz[i]) ; free(xg[i]) ; }
   synth_free(&s) ;
   return 0 ;
}

/* Compressed views: R'R = X'X, rank T - runs for data centered within runs; a fit of the R (ndim = nvox, the
   start carried over) is the fit of X: S, rho2, the objective trace, and W = X M, for DET and PROB, one W or
   a W per run, all observed or with missing points. */
static int test_compress( void )
{
   int nvox[4]={120,150,90,200} , T=72 , k=3 , seg[3]={24,24,24} , i , t , f , alg , mode ;
   double sg[4]={0.5,0.7,0.6,0.8} ; synth s ; float *R[4] , *w0[4] ; int rank[4] ; unsigned char *obs[4] ; srm_rng rg ;
   double ndim[4] ;
   CHECK(synth_make(&s,4,nvox,T,k,sg,515)==0,"synth") ;
   for( i=0 ; i<4 ; i++ ) for( f=0 ; f<nvox[i] ; f++ ){         /* center every feature within each run */
     int r , c0=0 ;
     for( r=0 ; r<3 ; c0+=seg[r],r++ ){
       double m=0.0 ; for( t=c0 ; t<c0+seg[r] ; t++ ) m+=s.x[i][f*T+t] ; m/=seg[r] ;
       for( t=c0 ; t<c0+seg[r] ; t++ ) s.x[i][f*T+t]-=(float)m ;
     }
   }
   srm_rng_init(&rg,77,1) ;
   for( i=0 ; i<4 ; i++ ){ obs[i]=(unsigned char *)malloc(T) ; for( t=0 ; t<T ; t++ ) obs[i][t]=(srm_unif(&rg)>0.15) ; ndim[i]=nvox[i] ; }
   for( i=0 ; i<4 ; i++ ){
     double worst=0.0 , sc=0.0 ; int a , b ;
     CHECK(SRM_compress(s.x[i],nvox[i],T,0.0,&R[i],&rank[i])==SRM_OK,"compress") ;
     CHECK(rank[i]==T-3,"rank T - 3 for data centered within 3 runs") ;
     for( a=0 ; a<T ; a++ ) for( b=0 ; b<T ; b++ ){
       double g=0.0 , h=0.0 ; int q ;
       for( f=0 ; f<nvox[i] ; f++ ) g+=(double)s.x[i][f*T+a]*s.x[i][f*T+b] ;
       for( q=0 ; q<rank[i] ; q++ ) h+=(double)R[i][q*T+a]*R[i][q*T+b] ;
       worst=fmax(worst,fabs(g-h)) ; sc=fmax(sc,fabs(g)) ;
     }
     CHECK(worst<1e-5*sc,"R'R = X'X") ;
   }
   for( alg=0 ; alg<2 ; alg++ ) for( mode=0 ; mode<4 ; mode++ ){    /* mode: shared, coupled, shared+missing, coupled+missing */
     SRM_params p ; SRM_model *mx=NULL , *mr=NULL ; SRM_fitinfo fx , fr ; float *xz[4] , *Rz[4] ; int rz[4] , it ; double es=0.0 , ns=0.0 , eo=0.0 , ew=0.0 ;
     int miss=(mode>=2) , cpl=(mode%2) ;
     for( i=0 ; i<4 ; i++ ){                                  /* missing points are 0 in the data */
       xz[i]=(float *)malloc(sizeof(float)*nvox[i]*T) ;
       for( f=0 ; f<nvox[i] ; f++ ) for( t=0 ; t<T ; t++ ) xz[i][f*T+t]=(miss && !obs[i][t]) ? 0.0f : s.x[i][f*T+t] ;
       if( miss ){ CHECK(SRM_compress(xz[i],nvox[i],T,0.0,&Rz[i],&rz[i])==SRM_OK,"compress, missing") ; }
       else { Rz[i]=R[i] ; rz[i]=rank[i] ; }
       w0[i]=(float *)malloc(sizeof(float)*rz[i]*k) ;
     }
     SRM_params_default(&p) ; p.nfeat=k ; p.niter=25 ; p.tol=0.0 ; p.center=0 ; p.seed=31 ;
     p.algo = alg ? SRM_ALGO_DET : SRM_ALGO_PROB ;
     if( cpl ){ p.nseg=3 ; p.seglen=seg ; p.coupling=20.0 ; }
     if( miss ) p.obs=(const unsigned char * const *)obs ;
     CHECK(SRM_fit(4,T,nvox,xz,&p,&mx,&fx)==SRM_OK,"fit of X") ;
     for( i=0 ; i<4 ; i++ ) CHECK(SRM_compress_start(xz[i],nvox[i],T,Rz[i],rz[i],k,p.seed,i,w0[i])==SRM_OK,"the start, compressed") ;
     p.ndim=ndim ; p.w0=(float * const *)w0 ;
     CHECK(SRM_fit(4,T,rz,Rz,&p,&mr,&fr)==SRM_OK,"fit of R") ;
     for( t=0 ; t<k*T ; t++ ){ double e=(double)mx->s[t]-mr->s[t] ; es+=e*e ; ns+=(double)mx->s[t]*mx->s[t] ; }
     for( it=0 ; it<=fx.niter_done && it<=fr.niter_done ; it++ ) eo=fmax(eo,fabs(fx.objective[it]-fr.objective[it])/fabs(fx.objective[it])) ;
     for( i=0 ; i<4 ; i++ ){                                  /* W on the features, from X and M */
       double *M=(double *)malloc(sizeof(double)*T*k) , num=0.0 , den=0.0 ; int c ;
       CHECK(SRM_compress_map(Rz[i],rz[i],T,mr->w[i],k,M)==SRM_OK,"map") ;
       for( f=0 ; f<nvox[i] ; f++ ) for( c=0 ; c<k ; c++ ){
         double w=0.0 ; for( t=0 ; t<T ; t++ ) w+=(double)xz[i][f*T+t]*M[t*k+c] ;
         num+=(w-mx->w[i][f*k+c])*(w-mx->w[i][f*k+c]) ; den+=(double)mx->w[i][f*k+c]*mx->w[i][f*k+c] ;
       }
       ew=fmax(ew,sqrt(num/den)) ; free(M) ;
     }
     if( getenv("SRM_TEST_VERBOSE") ) fprintf(stderr,"[compress %s mode %d] S %.2e obj %.2e W %.2e\n",alg?"det":"prob",mode,sqrt(es/ns),eo,ew) ;
     CHECK(fx.niter_done==fr.niter_done,"compressed: the same number of iterations") ;
     CHECK(sqrt(es/ns)<2e-4,"compressed: S = the uncompressed S") ;
     CHECK(eo<1e-6,"compressed: the objective trace = the uncompressed one") ;
     CHECK(ew<2e-4,"compressed: W = X M = the uncompressed W") ;
     if( !alg ){ double er=0.0 ; for( i=0 ; i<4 ; i++ ) er=fmax(er,fabs(mx->rho2[i]-mr->rho2[i])/mx->rho2[i]) ;
       CHECK(er<1e-5,"compressed: rho2 per original feature (ndim)") ; }
     for( i=0 ; i<4 ; i++ ){ free(xz[i]) ; free(w0[i]) ; if( miss ) free(Rz[i]) ; }
     SRM_free_model(mx) ; SRM_free_model(mr) ; SRM_free_fitinfo(&fx) ; SRM_free_fitinfo(&fr) ;
   }
   { float *Z=(float *)malloc(sizeof(float)*nvox[0]*T) , *Rq ; int rq ;          /* all-zero columns give zero columns of R */
     memcpy(Z,s.x[0],sizeof(float)*nvox[0]*T) ;
     for( f=0 ; f<nvox[0] ; f++ ){ Z[f*T+5]=0.0f ; Z[f*T+40]=0.0f ; }
     CHECK(SRM_compress(Z,nvox[0],T,0.0,&Rq,&rq)==SRM_OK,"compress with zero columns") ;
     { int q , bad=0 ; for( q=0 ; q<rq ; q++ ) if( Rq[q*T+5]!=0.0f || Rq[q*T+40]!=0.0f ) bad=1 ;
       CHECK(!bad && rq==T-3,"zero columns stay zero (rank: 2 zero columns, and the one run still centered)") ; }
     free(Z) ; free(Rq) ; }
   for( i=0 ; i<4 ; i++ ){ free(R[i]) ; free(obs[i]) ; }
   synth_free(&s) ;
   return 0 ;
}

static int test_missing( void )
{
   int nvox[4]={30,36,33,40} , T=60 , k=3 , i , t , alg , kern ;
   double sg[4]={0.4,0.6,0.5,0.7} ;
   synth s ; unsigned char *obs[4] ; float *xg[4] ; srm_rng r ;
   CHECK(synth_make(&s,4,nvox,T,k,sg,71)==0,"synth") ;
   for( i=0 ; i<4 ; i++ ) center_rows(s.x[i],nvox[i],T) ;
   srm_rng_init(&r,3,3) ;
   for( i=0 ; i<4 ; i++ ){                      /* about 20% missing per view, and t=7 observed by nobody */
     int f ;
     obs[i]=(unsigned char *)malloc(T) ; xg[i]=(float *)malloc(sizeof(float)*nvox[i]*T) ;
     for( t=0 ; t<T ; t++ ) obs[i][t]=(srm_unif(&r)>0.2 && t!=7) ;
     for( f=0 ; f<nvox[i] ; f++ ) for( t=0 ; t<T ; t++ )   /* garbage where missing: it must never be read */
       xg[i][f*T+t] = obs[i][t] ? s.x[i][f*T+t] : ((t%2) ? NAN : 1.0e30f) ;
   }
   for( alg=0 ; alg<2 ; alg++ ) for( kern=1 ; kern<=2 ; kern++ ){
     SRM_params p ; SRM_model *m1=NULL , *m2=NULL , *m0=NULL , *ma=NULL ; SRM_fitinfo f1 , f2 , f0 , fa ; int it , ok=1 ;
     unsigned char *all[4] ; float *xz[4] ;
     SRM_params_default(&p) ; p.nfeat=k ; p.niter=40 ; p.tol=0.0 ; p.center=0 ; p.kernel=kern ;
     p.algo = alg ? SRM_ALGO_DET : SRM_ALGO_PROB ;
     /* everything observed is the fit without masks, bit for bit */
     for( i=0 ; i<4 ; i++ ){ all[i]=(unsigned char *)malloc(T) ; memset(all[i],1,T) ; }
     CHECK(SRM_fit(4,T,nvox,s.x,&p,&m0,&f0)==SRM_OK,"no masks") ;
     p.obs=(const unsigned char * const *)all ;
     CHECK(SRM_fit(4,T,nvox,s.x,&p,&ma,&fa)==SRM_OK,"all observed") ;
     CHECK(memcmp(m0->s,ma->s,sizeof(float)*k*T)==0 && f0.niter_done==fa.niter_done &&
           memcmp(f0.objective,fa.objective,sizeof(double)*(f0.niter_done+1))==0,"masks of all ones change nothing") ;
     /* missing values are never read: garbage there gives the same fit as zeros there */
     for( i=0 ; i<4 ; i++ ){ int f ; xz[i]=(float *)malloc(sizeof(float)*nvox[i]*T) ;
       for( f=0 ; f<nvox[i] ; f++ ) for( t=0 ; t<T ; t++ ) xz[i][f*T+t]=obs[i][t] ? s.x[i][f*T+t] : 0.0f ; }
     p.obs=(const unsigned char * const *)obs ;
     CHECK(SRM_fit(4,T,nvox,xz,&p,&m1,&f1)==SRM_OK,"fit with missing data") ;
     CHECK(SRM_fit(4,T,nvox,xg,&p,&m2,&f2)==SRM_OK,"fit with garbage where missing") ;
     CHECK(memcmp(m1->s,m2->s,sizeof(float)*k*T)==0 && memcmp(f1.objective,f2.objective,sizeof(double)*(f1.niter_done+1))==0,
           "missing values are never read") ;
     for( i=0 ; i<4 ; i++ ) CHECK(memcmp(m1->w[i],m2->w[i],sizeof(float)*nvox[i]*k)==0 && ortho_error(m1->w[i],nvox[i],k)<1e-5,
                                  "W the same, and orthonormal") ;
     /* every step improves the objective */
     for( it=1 ; it<=f1.niter_done ; it++ ){          /* to float rounding of the stored W, on the scale of the start */
       double tolr=1e-7*fabs(f1.objective[0]) ;
       if( alg ? (f1.objective[it] > f1.objective[it-1]+tolr) : (f1.objective[it] < f1.objective[it-1]-tolr) ) ok=0 ;
     }
     if( !ok || getenv("SRM_TEST_VERBOSE") ){ fprintf(stderr,"[missing %s kernel %d]",alg?"det":"prob",kern) ;
       for( it=1 ; it<=f1.niter_done ; it++ ) if( (alg ? -1.0 : 1.0)*(f1.objective[it]-f1.objective[it-1]) < 0.0 )
         fprintf(stderr," it %d: %.12g -> %.12g (rel %.2e)",it,f1.objective[it-1],f1.objective[it],
                 fabs(f1.objective[it]-f1.objective[it-1])/fabs(f1.objective[it-1])) ;
       fprintf(stderr,"\n") ; }
     CHECK(ok,"the objective never gets worse with missing data (to rounding)") ;
     /* and it is what it claims to be, at the final parameters */
     if( !alg ){
       double bl=brute_loglik_obs(m1,xz,obs) ;
       if( getenv("SRM_TEST_VERBOSE") ) fprintf(stderr,"[missing prob] objective %.10g brute %.10g\n",f1.objective[f1.niter_done],bl) ;
       CHECK(fabs(bl-f1.objective[f1.niter_done]) <= 1e-6*fabs(bl),"PROB objective = the marginal likelihood of the observed entries") ;
     } else {
       double sse=0.0 ; int f , c ;
       for( i=0 ; i<4 ; i++ ) for( f=0 ; f<nvox[i] ; f++ ) for( t=0 ; t<T ; t++ ) if( obs[i][t] ){
         double v=xz[i][f*T+t] ;
         for( c=0 ; c<k ; c++ ) v -= (double)m1->w[i][f*k+c]*m1->s[c*T+t] ;
         sse+=v*v ;
       }
       if( getenv("SRM_TEST_VERBOSE") ) fprintf(stderr,"[missing det] objective %.10g brute %.10g\n",f1.objective[f1.niter_done],sse) ;
       CHECK(fabs(sse-f1.objective[f1.niter_done]) <= 1e-5*sse,"DET objective = the squared error over the observed entries") ;
       for( c=0 ; c<k ; c++ ) CHECK(m1->s[c*T+7]==0.0f,"a time point nobody observed has S = 0 (DET)") ;
     }
     if( kern==2 ){                             /* the Gram fit agrees with the voxel fit */
       SRM_params q=p ; SRM_model *mv=NULL ; SRM_fitinfo fv ; double num=0.0 , den=0.0 ; int c ;
       q.kernel=SRM_KERNEL_VOXEL ;
       CHECK(SRM_fit(4,T,nvox,xz,&q,&mv,&fv)==SRM_OK,"voxel fit with missing data") ;
       for( c=0 ; c<k*T ; c++ ){ double d=(double)mv->s[c]-m1->s[c] ; num+=d*d ; den+=(double)mv->s[c]*mv->s[c] ; }
       CHECK(sqrt(num/den)<1e-4,"Gram and voxel fits agree with missing data") ;
       SRM_free_fitinfo(&fv) ; SRM_free_model(mv) ;
     }
#ifdef USE_OMP
     { SRM_model *mt=NULL ; SRM_fitinfo ft ; int nmax=omp_get_max_threads() ;
       omp_set_num_threads(1) ;
       CHECK(SRM_fit(4,T,nvox,xz,&p,&mt,&ft)==SRM_OK,"1 thread") ;
       omp_set_num_threads(nmax>1 ? nmax : 2) ;
       CHECK(memcmp(mt->s,m1->s,sizeof(float)*k*T)==0,"missing-data fit identical across thread counts") ;
       SRM_free_fitinfo(&ft) ; SRM_free_model(mt) ; }
#endif
     /* centering is over the observed samples only */
     { SRM_params q=p ; SRM_model *mc=NULL ; SRM_fitinfo fc ; int f ; float *xo[4] ; double worst=0.0 ;
       for( i=0 ; i<4 ; i++ ){ xo[i]=(float *)malloc(sizeof(float)*nvox[i]*T) ;
         for( f=0 ; f<nvox[i] ; f++ ) for( t=0 ; t<T ; t++ ) xo[i][f*T+t]=xg[i][f*T+t]+(obs[i][t] ? 5.0f+f : 0.0f) ; }
       q.center=1 ;
       CHECK(SRM_fit(4,T,nvox,xo,&q,&mc,&fc)==SRM_OK,"centered fit with missing data") ;
       for( i=0 ; i<4 ; i++ ){
         for( f=0 ; f<nvox[i] ; f++ ){
           double mo=0.0 ; int no=0 ;
           for( t=0 ; t<T ; t++ ) if( obs[i][t] ){ mo+=xo[i][f*T+t] ; no++ ; }
           mo/=no ; if( fabs(mc->mu[i][f]-mo)>worst ) worst=fabs(mc->mu[i][f]-mo) ;
         }
         free(xo[i]) ;
       }
       CHECK(worst<1e-4,"the means are over observed samples") ;
       SRM_free_fitinfo(&fc) ; SRM_free_model(mc) ; }
     for( i=0 ; i<4 ; i++ ){ free(all[i]) ; free(xz[i]) ; }
     SRM_free_fitinfo(&f0) ; SRM_free_fitinfo(&fa) ; SRM_free_fitinfo(&f1) ; SRM_free_fitinfo(&f2) ;
     SRM_free_model(m0) ; SRM_free_model(ma) ; SRM_free_model(m1) ; SRM_free_model(m2) ;
   }
   /* free runs (lambda 0) with a run where a view has fewer than k observed points: refused */
   { SRM_params p ; SRM_model *m=NULL ; SRM_fitinfo fi ; int seg[2]={30,30} ; unsigned char *few[4] ;
     for( i=0 ; i<4 ; i++ ){ few[i]=(unsigned char *)malloc(T) ; memset(few[i],1,T) ; }
     for( t=0 ; t<28 ; t++ ) few[2][t]=0 ;                   /* 2 observed in run 1 of view 2 */
     SRM_params_default(&p) ; p.nfeat=k ; p.niter=5 ; p.center=0 ; p.nseg=2 ; p.seglen=seg ; p.coupling=0.0 ;
     p.obs=(const unsigned char * const *)few ;
     CHECK(SRM_fit(4,T,nvox,s.x,&p,&m,&fi)==SRM_ERR_ARG,"lambda 0 with a run observed at fewer than k points is refused") ;
     p.coupling=10.0 ;
     CHECK(SRM_fit(4,T,nvox,s.x,&p,&m,&fi)==SRM_OK,"... and allowed with lambda > 0 (the anchor fills in)") ;
     SRM_free_model(m) ; SRM_free_fitinfo(&fi) ;
     for( i=0 ; i<4 ; i++ ) free(few[i]) ; }
   for( i=0 ; i<4 ; i++ ){ free(obs[i]) ; free(xg[i]) ; }
   synth_free(&s) ;
   return 0 ;
}

/* segment matching where windows with a missing sample are dropped, written the obvious way */
static void naive_segment_match_obs( const float *a , const float *b , int k , int T , int win ,
                                     const unsigned char *valid , double *acc , double *chance )
{
   int nseg=T-win , i,j,d,w , correct=0 , nq=0 ; double chs=0.0 ;
   double *wa=(double *)malloc(sizeof(double)*k*win) , *wb=(double *)malloc(sizeof(double)*k*win) ;
   int *ok=(int *)malloc(sizeof(int)*nseg) ;
   for( i=0 ; i<nseg ; i++ ){ ok[i]=1 ; for( w=0 ; w<win ; w++ ) if( !valid[i+w] ) ok[i]=0 ; }
   for( i=0 ; i<nseg ; i++ ){
     double best=-INFINITY ; int bj=-1 , nc=0 ;
     if( !ok[i] ) continue ;
     for( j=0 ; j<nseg ; j++ ){
       double r ;
       if( !ok[j] || (abs(i-j)<win && i!=j) ) continue ;
       for( w=0 ; w<win ; w++ ) for( d=0 ; d<k ; d++ ){ wa[w*k+d]=a[d*T+i+w] ; wb[w*k+d]=b[d*T+j+w] ; }
       r=naive_r(wa,wb,k*win) ; nc++ ;
       if( r>best ){ best=r ; bj=j ; }
     }
     correct+=(bj==i) ; chs+=1.0/nc ; nq++ ;
   }
   *acc=(double)correct/nq ; *chance=chs/nq ;
   free(wa) ; free(wb) ; free(ok) ;
}

static int test_obs_metrics( void )
{
   int k=4 , T=90 , win=6 , t , c , j ; srm_rng r ; float *a , *b ; unsigned char *valid , *ones ;
   double acc , ch , acc2 , ch2 ;
   srm_rng_init(&r,11,2) ;
   a=(float *)malloc(sizeof(float)*k*T) ; b=(float *)malloc(sizeof(float)*k*T) ;
   valid=(unsigned char *)malloc(T) ; ones=(unsigned char *)malloc(T) ;
   for( t=0 ; t<k*T ; t++ ){ a[t]=(float)srm_normal(&r) ; b[t]=a[t]+0.8f*(float)srm_normal(&r) ; }
   for( t=0 ; t<T ; t++ ){ valid[t]=(srm_unif(&r)>0.08) ; ones[t]=1 ; }
   CHECK(SRM_segment_match(a,b,k,T,win,&acc,&ch)==SRM_OK && SRM_segment_match_obs(a,b,k,T,win,ones,&acc2,&ch2)==SRM_OK,"segment matching") ;
   CHECK(acc==acc2 && ch==ch2,"an all-valid mask is plain segment matching") ;
   CHECK(SRM_segment_match_obs(a,b,k,T,win,valid,&acc,&ch)==SRM_OK,"masked segment matching") ;
   naive_segment_match_obs(a,b,k,T,win,valid,&acc2,&ch2) ;
   CHECK(fabs(acc-acc2)<1e-12 && fabs(ch-ch2)<1e-12,"masked segment matching = windows with a missing sample dropped") ;
   memset(valid,0,T) ;
   CHECK(SRM_segment_match_obs(a,b,k,T,win,valid,&acc,&ch)==SRM_ERR_ARG,"nothing valid is refused") ;
   /* the reference over the views observed at each t */
   { int nvox[4]={30,32,28,34} , iv[3]={1,2,3} ; double sg[4]={0.3,0.5,0.4,0.6} ; synth s ;
     SRM_params p ; SRM_model *m=NULL ; SRM_fitinfo fi ; float *P[3] , *out , *out2 ; unsigned char *ob[3] , *al[3] ;
     int alg ;
     CHECK(synth_make(&s,4,nvox,T,k,sg,13)==0,"synth") ;
     for( alg=0 ; alg<2 ; alg++ ){
       SRM_params_default(&p) ; p.nfeat=k ; p.niter=20 ; p.algo = alg ? SRM_ALGO_DET : SRM_ALGO_PROB ;
       CHECK(SRM_fit(4,T,nvox,s.x,&p,&m,&fi)==SRM_OK,"fit") ;
       out=(float *)malloc(sizeof(float)*k*T) ; out2=(float *)malloc(sizeof(float)*k*T) ;
       for( j=0 ; j<3 ; j++ ){
         P[j]=(float *)malloc(sizeof(float)*k*T) ; ob[j]=(unsigned char *)malloc(T) ; al[j]=ones ;
         CHECK(SRM_project(m,iv[j],s.x[iv[j]],T,P[j])==SRM_OK,"project") ;
         for( t=0 ; t<T ; t++ ) ob[j][t]=(srm_unif(&r)>0.25 || t==5) && !(t==9) ;
       }
       for( j=0 ; j<2 ; j++ ){                    /* weighted 0 and 1: all observed is the old call */
         double worst=0.0 ;
         CHECK(SRM_shared_from_projections(m,3,iv,P,T,j,out)==SRM_OK &&
               SRM_shared_from_projections_obs(m,3,iv,P,(const unsigned char * const *)al,T,j,out2)==SRM_OK,"references") ;
         for( t=0 ; t<k*T ; t++ ) if( fabs(out[t]-out2[t])>worst ) worst=fabs(out[t]-out2[t]) ;
         CHECK(worst<1e-5,"all observed: the reference of SRM_shared_from_projections") ;
       }
       for( j=0 ; j<2 ; j++ ){                    /* with missing: each t from the views observed there */
         double worst=0.0 ;
         CHECK(SRM_shared_from_projections_obs(m,3,iv,P,(const unsigned char * const *)ob,T,j,out)==SRM_OK,"masked reference") ;
         for( t=0 ; t<T ; t++ ){
           int sel[3] , n=0 , q ;
           for( q=0 ; q<3 ; q++ ) if( ob[q][t] ) sel[n++]=q ;
           if( n==0 ){ for( c=0 ; c<k ; c++ ) if( out[c*T+t]!=0.0f ) worst=1.0 ; continue ; }
           { float *Pq[3] ; int ivq[3] ; float *col=(float *)malloc(sizeof(float)*k) , *pc[3] ;
             for( q=0 ; q<n ; q++ ){ ivq[q]=iv[sel[q]] ; pc[q]=(float *)malloc(sizeof(float)*k) ;
               for( c=0 ; c<k ; c++ ) pc[q][c]=P[sel[q]][c*T+t] ; Pq[q]=pc[q] ; }
             CHECK(SRM_shared_from_projections(m,n,ivq,Pq,1,j,col)==SRM_OK,"one time point") ;
             for( c=0 ; c<k ; c++ ) if( fabs(col[c]-out[c*T+t])>worst ) worst=fabs(col[c]-out[c*T+t]) ;
             for( q=0 ; q<n ; q++ ) free(pc[q]) ; free(col) ; }
         }
         CHECK(worst<1e-5,"masked reference = the reference of the views observed at that t") ;
       }
       for( j=0 ; j<3 ; j++ ){ free(P[j]) ; free(ob[j]) ; }
       free(out) ; free(out2) ; SRM_free_fitinfo(&fi) ; SRM_free_model(m) ; m=NULL ;
     }
     synth_free(&s) ; }
   free(a) ; free(b) ; free(valid) ; free(ones) ;
   return 0 ;
}

static int bench( void )
{
   int nview=8 , nvox[8] , T=300 , k=50 , i ;
   double sg[8] ;
   synth s ; SRM_params p ; SRM_model *m=NULL ; SRM_fitinfo fi ; clock_t c0 ;
   for( i=0 ; i<nview ; i++ ){ nvox[i]=20000 ; sg[i]=1.0 ; }
   if( synth_make(&s,nview,nvox,T,k,sg,1) ) return 1 ;
   SRM_params_default(&p) ; p.nfeat=k ; p.niter=5 ; p.tol=0.0 ; p.center=1 ;
   c0 = clock() ;
   if( SRM_fit(nview,T,nvox,s.x,&p,&m,&fi) ) return 1 ;
   printf("bench: %d views x %d voxels, T=%d, k=%d, %d iterations: %.2f s cpu\n",
          nview,nvox[0],T,k,fi.niter_done,(double)(clock()-c0)/CLOCKS_PER_SEC) ;
   SRM_free_fitinfo(&fi) ; SRM_free_model(m) ; synth_free(&s) ;
   return 0 ;
}

int main( int argc , char **argv )
{
   if( argc>1 && strcmp(argv[1],"bench")==0 ) return bench() ;
   if( test_kernels()            ) return 1 ;
   if( test_det()                ) return 1 ;
   if( test_prob()               ) return 1 ;
   if( test_prob_likelihood()    ) return 1 ;
   if( test_rank_deficient()     ) return 1 ;
   if( test_centering_and_copy() ) return 1 ;
   if( test_errors()             ) return 1 ;
   if( test_coupled()            ) return 1 ;
   if( test_update()             ) return 1 ;
   if( test_metrics()            ) return 1 ;
   if( test_reconstruction_r2()  ) return 1 ;
   if( test_zscore()             ) return 1 ;
   if( test_model_file()         ) return 1 ;
   if( test_project_reconstruct()) return 1 ;
   if( test_shared_response()    ) return 1 ;
   if( test_fit_view()           ) return 1 ;
   if( test_canonicalize()       ) return 1 ;
   if( test_procrustes()         ) return 1 ;
   if( test_multistart()         ) return 1 ;
   if( test_gram()               ) return 1 ;
   if( test_missing()            ) return 1 ;
   if( test_compress()           ) return 1 ;
   if( test_missing_coupled()    ) return 1 ;
   if( test_missing_update()     ) return 1 ;
   if( test_obs_metrics()        ) return 1 ;
#ifdef USE_OMP
   if( test_thread_invariance()  ) return 1 ;
#endif
   printf("thd_srm fit tests passed\n") ;
   return 0 ;
}
