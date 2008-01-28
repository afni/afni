/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/
   
#include "mrilib.h"
#include "betafit.c"

#undef SINGLET

typedef struct {
  int bot , top ;
  float pval ;
} spanfit ;

typedef struct {
   int ndim ;
   float * cmat , * cfac , * mvec ;
} covmat ;

#define IFREE(x) do{if((x)!=NULL)free(x);}while(0)

#define FREE_COVMAT(cc)                          \
  do{ if(cc != NULL){                            \
        IFREE(cc->cmat); IFREE(cc->cfac);        \
        IFREE(cc->mvec); free(cc); } } while(0)

#define CM(i,j) cmat[(i)+(j)*ndim]
#define CH(i,j) cfac[(i)+(j)*ndim]

/*-----------------------------------------------------------------*/

void forward_solve_inplace( covmat * cv , float * vec )
{
   register int     ndim=cv->ndim , ii,jj ;
   register float * cfac=cv->cfac , sum ;

   for( ii=0 ; ii < ndim ; ii++ ){
      sum = vec[ii] ;
      for( jj=0 ; jj < ii ; jj++ ) sum -= CH(ii,jj) * vec[jj] ;
      vec[ii] = sum / CH(ii,ii) ;
   }
   return ;
}

/*-----------------------------------------------------------------*/

void backward_solve_inplace( covmat * cv , float * vec )
{
   register int     ndim=cv->ndim , ii,jj ;
   register float * cfac=cv->cfac , sum ;

   for( ii=ndim-1 ; ii >= 0 ; ii-- ){
      sum = vec[ii] ;
      for( jj=ii+1 ; jj < ndim ; jj++ ) sum -= CH(jj,ii) * vec[jj] ;
      vec[ii] = sum / CH(ii,ii) ;
   }
   return ;
}

/*-----------------------------------------------------------------*/

void compute_choleski( covmat * cv )
{
   register int     ndim=cv->ndim ,          ii,jj,kk ;
   register float * cmat=cv->cmat , * cfac , sum ;

   if( ndim < 1 || cmat == NULL ) return ;

   if( cv->cfac == NULL )
      cv->cfac = (float *) malloc(sizeof(float)*ndim*ndim) ;

   cfac = cv->cfac ;

   if( ndim == 1 ){
      if( cmat[0] <= 0.0 ){ free(cv->cfac); cv->cfac = NULL; return; }
      cfac[0] = sqrt(cmat[0]) ;
      return ;
   }

   for( ii=0 ; ii < ndim ; ii++ ){
      for( jj=0 ; jj < ii ; jj++ ){
         sum = CM(ii,jj) ;
         for( kk=0 ; kk < jj ; kk++ ) sum -= CH(ii,kk) * CH(jj,kk) ;
         CH(ii,jj) = sum / CH(jj,jj) ;
      }
      sum = CM(ii,ii) ;
      for( kk=0 ; kk < ii ; kk++ ) sum -= CH(ii,kk) * CH(ii,kk) ;
      if( sum <= 0.0 ){ free(cv->cfac); cv->cfac = NULL; return; }
      CH(ii,ii) = sqrt(sum) ;
      for( jj=ii+1 ; jj < ndim ; jj++ ) CH(ii,jj) = 0.0 ;
   }
   return ;
}

/*-----------------------------------------------------------------*/

#define CCUT 3.5
#define EPS  1.e-4

covmat * robust_covar( int ndim , int nvec , float ** vec )
{
   covmat * cv ;
   float *nmat, *cmat , fnvec,fndim,cnorm,csum , *tv , *vv , *mv , *wv ;
   int ii , jj , kk , nite ;
   float bcut , cwt ;

#if 0
fprintf(stderr,"Enter robust_covar:  ndim=%d  nvec=%d\n",ndim,nvec) ;
#endif

   if( ndim < 2 || nvec < ndim || vec == NULL ) return NULL ;

   cv = (covmat *) malloc(sizeof(covmat)) ;
   cv->ndim = ndim ;
   cv->cmat = NULL ;
   cv->cfac = NULL ;
   cv->mvec = NULL ;

   nmat = (float *) malloc(sizeof(float)*ndim*ndim) ;  /* matrix     */
   tv   = (float *) malloc(sizeof(float)*ndim) ;       /* temp vector */
   mv   = (float *) malloc(sizeof(float)*ndim) ;       /* mean vector  */
   wv   = (float *) malloc(sizeof(float)*nvec) ;       /* weight vector */

   fnvec = 1.0/nvec ; fndim = 1.0/ndim ;
   bcut  = 1.0 + CCUT*sqrt(fndim) ;

   /* compute initial mean & covariance matrix with all weights = 1 */

   for( jj=0 ; jj < ndim ; jj++ ) mv[jj] = 0.0 ;

   for( kk=0 ; kk < nvec ; kk++ ){   /* mean vector sum */
      vv = vec[kk] ;
      for( jj=0 ; jj < ndim ; jj++ ) mv[jj] += vv[jj] ;
   }
   for( jj=0 ; jj < ndim ; jj++ ) mv[jj] *= fnvec ;  /* scale mean vector */

   for( jj=0 ; jj < ndim ; jj++ )
      for( ii=0 ; ii < ndim ; ii++ ) nmat[ii+jj*ndim] = 0.0 ;

   for( kk=0 ; kk < nvec ; kk++ ){   /* covariance matrix sum */
      vv = vec[kk] ;
      for( jj=0 ; jj < ndim ; jj++ ){
         for( ii=0 ; ii <= jj ; ii++ )
            nmat[ii+jj*ndim] += (vv[ii]-mv[ii])*(vv[jj]-mv[jj]) ;
      }
   }
   for( jj=0 ; jj < ndim ; jj++ ){   /* scale covariance matrix */
      for( ii=0 ; ii < jj ; ii++ )
         nmat[jj+ii*ndim] = (nmat[ii+jj*ndim] *= fnvec) ;
      nmat[jj+jj*ndim] *= fnvec ;
   }

   /* now iterate until convergence, or something */

   nite = 0 ;

   while(1){

      nite++ ;

#if 0
fprintf(stderr,"\niteration %2d:\n",nite) ;
#endif

      cmat = cv->cmat = nmat ;  /* put old matrix into cv */
      cv->mvec = mv ;           /* and old mean vector   */
      compute_choleski(cv) ;    /* decompose matrix     */

      if( cv->cfac == NULL ){
         free(cv->cmat); free(cv->mvec); free(cv); free(tv); free(wv);
         return NULL ;
      }

      nmat = (float *) malloc(sizeof(float)*ndim*ndim) ; /* new matrix */
      mv   = (float *) malloc(sizeof(float)*ndim) ;      /* new mean vector */

      for( jj=0 ; jj < ndim ; jj++ ){  /* initialize new things to zero */
         mv[jj] = 0.0 ;
         for( ii=0 ; ii < ndim ; ii++ ) nmat[ii+jj*ndim] = 0.0 ;
      }

      /* update mean */

      csum = 0.0 ;
      for( kk=0 ; kk < nvec ; kk++ ){
         vv = vec[kk] ;

         /*                    -1/2          */
         /* compute tv = [cmat]    (vv-mvec) */

         for( jj=0 ; jj < ndim ; jj++ ) tv[jj] = vv[jj] - cv->mvec[jj] ;
         forward_solve_inplace(cv,tv) ;

         /* compute norm of tv, then weighting factor for this vector */

         cnorm = 0.0 ; for( ii=0 ; ii < ndim ; ii++ ) cnorm += tv[ii]*tv[ii] ;
         cnorm = cnorm*fndim ;
         cnorm = (cnorm <= bcut) ? 1.0 : bcut/cnorm ;
         wv[kk] = cnorm ; csum += cnorm ;

         /* add vv into accumulating mean, with weight cnorm */

         for( jj=0 ; jj < ndim ; jj++ ) mv[jj] += cnorm*vv[jj] ;
      }
      csum = 1.0 / csum ; cwt = nvec*csum ;
      for( jj=0 ; jj < ndim ; jj++ ) mv[jj] *= csum ;  /* scale new mean */

      /* update covariance */

      for( kk=0 ; kk < nvec ; kk++ ){
         vv = vec[kk] ; cnorm = wv[kk] ;
         for( jj=0 ; jj < ndim ; jj++ ){
            for( ii=0 ; ii <= jj ; ii++ )
               nmat[ii+jj*ndim] +=
                  cnorm*(vv[ii]-cv->mvec[ii])*(vv[jj]-cv->mvec[jj]) ;
         }
      }
#define DDD csum
      for( jj=0 ; jj < ndim ; jj++ ){
         for( ii=0 ; ii < jj ; ii++ )
            nmat[jj+ii*ndim] = (nmat[ii+jj*ndim] *= DDD) ;
         nmat[jj+jj*ndim] *= DDD ;
      }

      /* check for convergence - L1 norm */

      cnorm = csum = 0.0 ;
      for( jj=0 ; jj < ndim ; jj++ ){
         for( ii=0 ; ii <= jj ; ii++ ){
            cnorm += fabs( nmat[ii+jj*ndim] - cmat[ii+jj*ndim] ) ;
            csum  += fabs( nmat[ii+jj*ndim] ) ;
         }
      }

#if 0
fprintf(stderr,"  |dif|=%12.4g  |mat|=%12.4g   cwt=%12.4g\n",cnorm,csum,cwt) ;
fprintf(stderr,"  matrix:\n") ;
for( ii=0 ; ii < ndim ; ii++ ){
   fprintf(stderr,"  Row%2d: %12.4g    ",ii,mv[ii]) ;
   for( jj=0 ; jj < ndim ; jj++ )
      fprintf(stderr," %12.4g",
        (jj<=ii) ? nmat[ii+jj*ndim] :
                   nmat[ii+jj*ndim]/sqrt(nmat[ii+ii*ndim]*nmat[jj+jj*ndim]) );
   fprintf(stderr,"\n") ;
}
#endif

      free(cv->cmat) ; free(cv->mvec) ;
      if( cnorm <= EPS*csum || nite > 3*ndim ){
         cv->cmat = nmat; cv->mvec = mv; break;  /* exit loop */
      }
   }

   free(wv) ; free(tv) ; compute_choleski(cv) ; return cv ;
}

/*-------------------------------------------------------------------*/

float evaluate_span( int ndim, int nvec,
                     int bot , int top ,
                     int nbasis , float ** basis ,
                     float * cvec , float ** bvec )
{
   int ii,kk,jj , npt=top-bot+1 , nbd ;
   float ** svec , **ee,*xx,*cc,s,t,xd,tinv , bd,dx ;
   covmat * cv , * ce ;

   if( nbasis >= npt ) return 0.0 ;

   /* make pointers to subvectors of bvec */

   svec = (float **) malloc(sizeof(float *)*nvec) ;
   for( kk=0 ; kk < nvec ; kk++ ) svec[kk] = bvec[kk] + bot ;

   /* estimate covariance of subvectors */

   cv = robust_covar( npt , nvec , svec ) ;
   free(svec) ;
   if( cv == NULL ) return 0.0 ;                  /* shouldn't happen */

   /* compute normalized cvec and basis into xx, ee */

#if 0
fprintf(stderr,"Normalizing cvec, basis\n") ;
#endif

   ee = (float **) malloc(sizeof(float *)*nbasis) ;    /* make space */
   for( kk=0 ; kk < nbasis ; kk++ )
      ee[kk] = (float *) malloc(sizeof(float)*npt) ;
   xx = (float *) malloc(sizeof(float)*npt) ;
   for( ii=0 ; ii < npt ; ii++ ){                      /* copy in */
      xx[ii] = cvec[ii+bot] ;
      for( kk=0 ; kk < nbasis ; kk++ ) ee[kk][ii] = basis[kk][ii+bot] ;
   }
   forward_solve_inplace( cv , xx ) ;                  /* normalize */
   for( kk=0 ; kk < nbasis ; kk++ )
      forward_solve_inplace( cv , ee[kk] ) ;

#if 0
fprintf(stderr,"Computing [ee]**T [ee]\n") ;
#endif

   /*             T     */
   /* compute [ee] [ee] */

   ce = (covmat *) malloc(sizeof(covmat)) ;
   ce->ndim = nbasis ;
   ce->cmat = (float *) malloc(sizeof(float)*nbasis*nbasis) ;
   ce->cfac = NULL ;
   ce->mvec = NULL ;                  /* won't be used */
   for( kk=0 ; kk < nbasis ; kk++ ){
      for( jj=0 ; jj <= kk ; jj++ ){
         s = 0.0 ;
         for( ii=0 ; ii < npt ; ii++ ) s += ee[kk][ii] * ee[jj][ii] ;
         ce->cmat[jj+kk*nbasis] = s ;
         if( jj < kk ) ce->cmat[kk+jj*nbasis] = s ;
      }
   }

   /* project [xx] onto space orthogonal to [ee] */

   compute_choleski(ce) ;
if( ce->cfac == NULL )fprintf(stderr,"choleski failed!\n") ;
   cc = (float *) malloc(sizeof(float)*nbasis) ;

   for( kk=0 ; kk < nbasis ; kk++ ){
      s = 0.0 ;
      for( ii=0 ; ii < npt ; ii++ ) s += ee[kk][ii] * xx[ii] ;
      cc[kk] = s ;
   }
   forward_solve_inplace( ce , cc ) ;
   backward_solve_inplace( ce , cc ) ;
   for( ii=0 ; ii < npt ; ii++ ){
      s = xx[ii] ;
      for( kk=0 ; kk < nbasis ; kk++ ) s -= ee[kk][ii] * cc[kk] ;
      xx[ii] = s ;
   }

   /* normalize each bvec, dot into residual vector, count negatives */
   /* (don't have to project bvec onto space orthog)                */
   /* (to [ee], since [xx] is already in that space)               */

   nbd = 0 ;
   for( kk=0 ; kk < nvec ; kk++ ){
      memcpy( ee[0] , bvec[kk]+bot , sizeof(float)*npt ) ; /* bvec */
      forward_solve_inplace( cv , ee[0] ) ;                /* normalized */
      bd = 0.0 ;
      for( ii=0 ; ii < npt ; ii++ ) bd += xx[ii] * ee[0][ii] ;
      if( bd <= 0.0 ) nbd++ ;
#if 0
fprintf(stderr," %12.4g",bd);
#endif
   }
   s = (float)nbd / (float)nvec ;
#if 0
fprintf(stderr," => nbd=%d\n",nbd) ;
#endif

   for( kk=0 ; kk < nbasis ; kk++ ) free(ee[kk]) ;
   free(ee) ; free(cc) ; free(xx); FREE_COVMAT(cv) ; FREE_COVMAT(ce) ;
   return s ;
}

/*-------------------------------------------------------------------*/

spanfit find_best_span( int ndim , int nvec , int minspan ,
                        int nbasis , float ** basis ,
                        float * cvec , float ** bvec       )
{
   spanfit result = {0,0,0.0} ;
   int ii,kk , bot,top , bot_best,top_best ;
   float val , val_best ;

   if( minspan < 3 || ndim < minspan || nvec < 100 ) return result ;
   if( cvec == NULL || bvec == NULL )                return result ;
   if( nbasis < 1 || basis == NULL )                 return result ;

   val_best = -1.0 ;
   for( bot=0 ; bot < ndim+1-minspan ; bot++ ){

for( top=0 ; top < bot+minspan-1 ; top++ ) printf(" 0") ;

      for( top=bot+minspan-1 ; top < ndim ; top++ ){
         val = evaluate_span( ndim,nvec , bot,top , nbasis,basis , cvec,bvec ) ;

printf(" %g",val) ;

         if( val > val_best ){
            val_best = val ; bot_best = bot ; top_best = top ;
         }
#if 1
         if( val >= 0.10 ) fprintf(stderr,"bot=%2d top=%2d: %.4f\n",bot,top,val) ;
#endif
      }

printf("\n") ;
   }

   result.bot = bot_best; result.top = top_best; result.pval = val_best;
   return result ;
}

/*-----------------------------------------------------------------------*/

static int nran=1000 ;
static float abot= 0.5 , atop=  4.0 ;
static float bbot=10.0 , btop=200.0 ;
static float pbot=50.0 , ptop= 80.0 ;
static double pthr=1.e-4 ;
static int sqr=0 ;

#define OUT_THR 1
#define OUT_BBB 2
#define OUT_AAA 3
#define OUT_AB  4

static int outmode = OUT_THR ;
static int nbasis  = 1 ;

#define FVECTOR_DIM 2
typedef struct {
   float v[FVECTOR_DIM] ;
} fvector ;

/*-----------------------------------------------------------------------*/

fvector process_sample( float pcut , BFIT_data * bfd )
{
   BFIT_result * bfr ;
   double xth ;
   fvector result ;

   static double aold,bold ;
   static BFIT_data * bfdold=NULL ;

#if 1
   if( bfd == bfdold ){
      beta_init( aold , bold ) ;
      nran = 400 ;
      abot = aold*0.5 ; atop = aold*2.0 ; if( abot <= 0.1 ) abot = 0.101 ;
      bbot = bold*0.5 ; btop = bold*2.0 ; if( bbot <= 9.9 ) bbot = 9.999 ;
   } else {
      beta_init( 0.0 , 0.0 ) ;
      bfdold = bfd ;
      nran   = 1000 ;
      abot   =  0.5 ; atop =  4.0 ;
      bbot   = 10.0 ; btop =200.0 ;
   }
#endif

   bfr = BFIT_compute( bfd , pcut , abot,atop , bbot,btop , nran,0 ) ;
   if( bfr == NULL ){
      fprintf(stderr,"*** Can't compute betafit at pcut=%f\n",pcut) ;
      exit(1) ;
   }

   aold = bfr->a ; bold = bfr->b ;

   switch( outmode ){

      default:
      case OUT_THR: /* use the threshold as the output parameter */
         xth = beta_p2t( pthr , bfr->a,bfr->b ) ;
         if( sqr ) xth = sqrt(xth) ;
         result.v[0] = xth ;
      break ;

      case OUT_BBB: result.v[0] = bold ; break ;
      case OUT_AAA: result.v[0] = aold ; break ;

      case OUT_AB:  result.v[0] = aold ; result.v[1] = bold ; break ;
   }

   BFIT_free_result(bfr) ;
   return result ;
}

/*-----------------------------------------------------------------------*/

int main( int argc , char * argv[] )
{
   BFIT_data   * bfd , * nfd ;
   float       * bf_tvec , ** boot_tvec ;
   int ndim , nvec ;

   int nvals,ival , nvox , nbin , miv ;
   float pcut , eps,eps1 ;
   float *bval , *cval ;
   double aa,bb,xc,xth ;
   fvector bfit ;
   float ** basis ;

   int mcount,mgood , ii , jj , kk , ibot,itop , qq ;

   int narg=1 ;
   int nboot=0 ;
   double  aboot,bboot,tboot , pthr=1.e-4 ;
   float   asig , bsig , tsig , abcor ;

   THD_3dim_dataset * input_dset , * mask_dset=NULL ;
   float mask_bot=666.0 , mask_top=-666.0 ;
   byte * mmm=NULL ;

   if( argc < 2 || strcmp(argv[1],"-help") == 0 ){
      fprintf(stderr,"Usage: 3dbetafit3 [options] dataset\n"
             "Fits a beta distribution to the values in a brick.\n"
             "\n"
             "Options:\n"
             "  -arange abot atop = Sets the search range for parameter\n"
             "                        'a' to abot..atop.\n"
             "                        [default is 0.5 .. 4.0]\n"
             "\n"
             "  -brange bbot btop = Sets the search range for parameter\n"
             "                        'b' to bbot..btop\n"
             "                        [default is 10 .. 200]\n"
             "\n"
             "  -prange pbot ptop = Will evaluate for percent cutoffs\n"
             "                        from pbot to ptop (steps of 1%%)\n"
             "                        [default is 50 .. 80]\n"
             "\n"
             "  -bootstrap N      = Does N bootstrap evaluations\n"
             "\n"
             "  -mask mset  = A mask dataset to indicate which\n"
             "                 voxels are to be used\n"
             "  -mrange b t = Use only mask values in range from\n"
             "                 'b' to 't' (inclusive)\n"
             "\n"
             "  -sqr    = Flag to square the data from the dataset\n"
             "  -pthr p = Sets p-value of cutoff for threshold evaluation\n"
             "              [default = 1.e-4]\n"
             "  -bout   = Use 'b' for the output, instead of thr\n"
             "  -aout   = Use 'a' for the output, instead of thr\n"
             "  -about  = Use 'a' AND 'b' for the ouput.\n"
         ) ;
         PRINT_COMPILE_DATE ; exit(0) ;
   }

   /* scan command-line args */

   while( narg < argc && argv[narg][0] == '-' ){

      if( strcmp(argv[narg],"-aout") == 0 ){
         outmode = OUT_AAA ; nbasis = 1 ; narg++ ; continue ;
      }

      if( strcmp(argv[narg],"-bout") == 0 ){
         outmode = OUT_BBB ; nbasis = 1 ; narg++ ; continue ;
      }

      if( strcmp(argv[narg],"-about") == 0 ){
         outmode = OUT_AB ; nbasis = 2 ; narg++ ; continue ;
      }

      if( strcmp(argv[narg],"-pthr") == 0 ){
         pthr = strtod(argv[++narg],NULL) ;
         if( pthr <= 0.0 || pthr >= 1.0 ){
            fprintf(stderr,"*** Illegal value after -pthr!\n");exit(1);
         }
         narg++ ; continue;
      }

      if( strcmp(argv[narg],"-sqr") == 0 ){
         sqr = 1 ; narg++ ; continue;
      }

      if( strcmp(argv[narg],"-arange") == 0 ){
         abot = strtod(argv[++narg],NULL) ;
         atop = strtod(argv[++narg],NULL) ;
         if( abot < 0.1 || abot > atop ){
            fprintf(stderr,"*** Illegal value after -arange!\n");exit(1);
         }
         narg++ ; continue;
      }

      if( strcmp(argv[narg],"-brange") == 0 ){
         bbot = strtod(argv[++narg],NULL) ;
         btop = strtod(argv[++narg],NULL) ;
         if( bbot < 0.1 || bbot > btop ){
            fprintf(stderr,"*** Illegal value after -brange!\n");exit(1);
         }
         narg++ ; continue;
      }

      if( strcmp(argv[narg],"-prange") == 0 ){
         pbot = (int) strtod(argv[++narg],NULL) ;
         ptop = (int) strtod(argv[++narg],NULL) ;
         if( pbot < 30.0 || pbot > ptop || ptop > 99.0 ){
            fprintf(stderr,"*** Illegal value after -prange!\n");exit(1);
         }
         narg++ ; continue;
      }

      if( strcmp(argv[narg],"-bootstrap") == 0 ){
         nboot = (int) strtod(argv[++narg],NULL) ;
         if( nboot < 100 ){
            fprintf(stderr,"*** Illegal value after -bootstrap!\n");exit(1);
         }
         narg++ ; continue;
      }

      if( strncmp(argv[narg],"-mask",5) == 0 ){
         if( mask_dset != NULL ){
            fprintf(stderr,"*** Cannot have two -mask options!\n") ; exit(1) ;
         }
         if( narg+1 >= argc ){
            fprintf(stderr,"*** -mask option requires a following argument!\n");
            exit(1) ;
         }
         mask_dset = THD_open_dataset( argv[++narg] ) ;
         if( mask_dset == NULL ){
            fprintf(stderr,"*** Cannot open mask dataset!\n") ; exit(1) ;
         }
         if( DSET_BRICK_TYPE(mask_dset,0) == MRI_complex ){
            fprintf(stderr,"*** Cannot deal with complex-valued mask dataset!\n");
            exit(1) ;
         }
         narg++ ; continue ;
      }

      if( strncmp(argv[narg],"-mrange",5) == 0 ){
         if( narg+2 >= argc ){
            fprintf(stderr,"*** -mrange option requires 2 following arguments!\n") ;
            exit(1) ;
         }
         mask_bot = strtod( argv[++narg] , NULL ) ;
         mask_top = strtod( argv[++narg] , NULL ) ;
         if( mask_top < mask_top ){
            fprintf(stderr,"*** -mrange inputs are illegal!\n") ; exit(1) ;
         }
         narg++ ; continue ;
      }

      fprintf(stderr,"*** Illegal option: %s\n",argv[narg]) ; exit(1) ;
   }

   if( nboot < 100 ){
      fprintf(stderr,"*** Must use -bootstrap 'option'!\n"); exit(1);
   }

   if( narg >= argc ){
      fprintf(stderr,"*** No dataset argument on command line!?\n"); exit(1);
   }

   input_dset = THD_open_dataset( argv[narg] ) ;
   if( input_dset == NULL ){
      fprintf(stderr,"*** Can't open dataset %s\n",argv[narg]); exit(1);
   }

   nvox = DSET_NVOX(input_dset) ;

   /* load data from dataset */

   DSET_load(input_dset) ;  CHECK_LOAD_ERROR((input_dset) ;

   if( DSET_BRICK_STATCODE(input_dset,0) == FUNC_COR_TYPE ) sqr = 1 ;

   bfd = BFIT_prepare_dataset( input_dset , 0 , sqr ,
                               mask_dset , 0 , mask_bot , mask_top ) ;

   if( bfd == NULL ){
      fprintf(stderr,"*** Couldn't prepare data from input dataset!\n");
      exit(1) ;
   }

   DSET_delete(mask_dset) ; DSET_delete(input_dset) ;

   /*--*/

   fprintf(stderr,"Computing bootstrap") ;

   ndim  = (ptop - pbot + 1.0)*nbasis ;

   basis = (float **) malloc(sizeof(float *)*nbasis) ;
   for( qq=0 ; qq < nbasis ; qq++ )
      basis[qq] = (float *) malloc(sizeof(float)*ndim) ;
   switch(nbasis){
      case 1:
         for( jj=0 ; jj < ndim ; jj++ ) basis[0][jj] = 1.0 ;
      break ;

      case 2:
         for( jj=0 ; jj < ndim ; jj++ ){
            basis[0][jj] = ((jj%2) == 0) ;
            basis[1][jj] = ((jj%2) == 1) ;
         }
      break ;
   }

   bf_tvec = (float *) malloc(sizeof(float)*ndim) ;
   for( pcut=pbot ; pcut <= ptop ; pcut += 1.0 ){
      bfit = process_sample( pcut , bfd ) ;
      qq = (int)(pcut-pbot) ;
      switch(nbasis){
         case 1: bf_tvec[qq] = bfit.v[0] ; break ;
         case 2: bf_tvec[2*qq] = bfit.v[0] ; bf_tvec[2*qq+1] = bfit.v[1] ; break ;
      }
   }

   nvec = nboot ;
   boot_tvec = (float **) malloc(sizeof(float *)*nvec) ;
   for( jj=0 ; jj < nboot ; jj++ ){
      boot_tvec[jj] = (float *) malloc(sizeof(float)*ndim) ;
      nfd = BFIT_bootstrap_sample( bfd ) ;
      for( pcut=pbot ; pcut <= ptop ; pcut += 1.0 ){
         bfit = process_sample( pcut , nfd ) ;
         qq = (int)(pcut-pbot) ;
         switch(nbasis){
            case 1: boot_tvec[jj][qq] = bfit.v[0] ; break ;
            case 2: boot_tvec[jj][2*qq] = bfit.v[0] ; boot_tvec[jj][2*qq+1] = bfit.v[1] ; break ;
         }
      }
      BFIT_free_data(nfd) ;
      if( jj%10 == 0 ) fprintf(stderr,".") ;
   }
   fprintf(stderr,"\n") ;
   BFIT_free_data(bfd) ;

#ifdef SINGLET
   while(1){
      fprintf(stderr,"Enter ibot itop [ndim=%d]: ",ndim) ;
      ibot = itop = 0 ;
      fscanf(stdin,"%d%d",&ibot,&itop) ;
      if( itop < 0 || itop-ibot+1 < 3 || itop >= ndim ) continue ;

      eps = evaluate_span( ndim,nvec , ibot,itop , nbasis,basis , bf_tvec,boot_tvec ) ;
      fprintf(stderr,"Evaluate = %f\n\n",eps) ;
   }
#else
   { spanfit sf = find_best_span( ndim,nvec , 10*nbasis , nbasis,basis , bf_tvec,boot_tvec ) ;
     float tbar = 0.0 ;
     for( ii=sf.bot ; ii <= sf.top ; ii++ ) tbar += bf_tvec[ii] ;
     tbar /= (sf.top-sf.bot+1.0) ;
     fprintf(stderr,"\nBEST bot=%2d top=%2d: %.4f %12.4g\n",sf.bot,sf.top,sf.pval,tbar) ;
   }
#endif

#if 0
   { float xx,ss ;
      for( pcut=pbot ; pcut <= ptop ; pcut += 1.0 ){
         kk = (int)(pcut-pbot) ;
         xx = bf_tvec[kk] ;
         ss = 0.0 ;
         for( jj=0 ; jj < nboot ; jj++ ) ss += SQR((boot_tvec[jj][kk]-xx)) ;
         ss = sqrt(ss/nboot) ;
         printf("%.1f %12.4g %12.4g\n",pcut,xx,ss) ;
      }
   }
#endif

   exit(0) ;
}
