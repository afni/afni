#include "mrilib.h"

/* prototypes */

void qsort_double( int n , double * a ) ;
void gaussians( int n , double * g ) ;
void normalize_doublevector( int n , double * far ) ;
double rtest( int msam , int nvec , double * lamq , double * rho ) ;

/*----------------------------------------------------------------------------
  Generate a bunch of N(0,1) deviates
------------------------------------------------------------------------------*/

void gaussians( int n , double * g )
{
   double u1,u2 ;
   int ii ;

   for( ii=0 ; ii < n ; ii+=2 ){
      do{ u1 = drand48() ; } while( u1==0.0 ) ;
      u1 = sqrt(-2.0*log(u1)) ;
      u2 = 6.2831853 * drand48() ;
      g[ii] = u1 * sin(u2) ;
      if( ii < n-1 ) g[ii+1] = u1 * cos(u2) ;
   }
   return ;
}

/*-----------------------------------------------------------------------------
   Remove the mean, and make it have L2 norm = 1
-------------------------------------------------------------------------------*/

void normalize_doublevector( int n , double * far )
{
   int ii ;
   double ff,gg ;

   for( ff=0.0,ii=0 ; ii < n ; ii++ ) ff += far[ii] ;
   ff /= n ;
   for( gg=0.0,ii=0 ; ii < n ; ii++ ) gg += SQR( (far[ii]-ff) ) ;
   if( gg <= 0.0 ) return ;
   gg = 1.0 / sqrt(gg) ;
   for( ii=0 ; ii < n ; ii++ ) far[ii] = (far[ii]-ff)*gg ;

   return ;
}

/*-----------------------------------------------------------------------------
   El Supremo function!!!
-------------------------------------------------------------------------------*/

#define BBOT 10

int main( int argc , char * argv[] )
{
   int nopt , mdev , jsize , ii,jj , numj,jset[100] ;
   double * gset , * pset , * rr ;
   double pp , dm ;

   srand48((long)time(NULL)) ;  /* initialize randomness */

   /* read command line */

   if( argc < 3 || strcmp(argv[1],"-help") == 0 ){
      printf("Usage: rtest1 M J [j1 j2 j3 ...]\n"
             "   M = # of deviates per sample sets\n"
             "   J = # of sample sets to generate\n"
             "  j1 = first statistic to output, etc.\n"
            ) ;
      exit(0) ;
   }

   nopt = 1 ;
   mdev = (int) strtod(argv[nopt++],NULL) ;
   if( mdev < BBOT ){ fprintf(stderr,"M=%d is too small!\n",mdev); exit(1); }
   jsize = (int) strtod(argv[nopt++],NULL) ;
   if( jsize < BBOT ){ fprintf(stderr,"J=%d is too small!\n",jsize); exit(1); }

   for( numj=0 ; numj < 100 && nopt < argc ; ){
      ii = (int) strtod(argv[nopt++],NULL) ;
      if( ii < 0 || ii >= jsize )
         fprintf(stderr,"** j%d = %d is illegal!\n",nopt-2,ii) ;
      else
         jset[numj++] = ii ;
   }

   /* make space for sample sets and results */

   gset = (double *) malloc(sizeof(double)*mdev) ;  /* 1 sample set */
   pset = (double *) malloc(sizeof(double)*mdev) ;  /* cdf-inverses */
   rr   = (double *) malloc(sizeof(double)*jsize) ; /* results      */

   /* initialize pset to quantile points on the N(0,1) cdf */

   dm = 1.0 / (double) mdev ;
   for( ii=0 ; ii < mdev ; ii++ ){
      pp = dm * (ii+0.5) ; pset[ii] = qginv( 1.0 - pp ) ;
   }
   normalize_doublevector( mdev , pset ) ;  /* prepare for correlation */

   /* loop over realization of sample sets */

   for( jj=0 ; jj < jsize ; jj++ ){
      gaussians( mdev , gset ) ;              /* make a sample set       */
      qsort_double( mdev , gset ) ;           /* sort it                 */
      normalize_doublevector( mdev , gset ) ; /* prepare for correlation */
      for( pp=0.0,ii=0 ; ii < mdev ; ii++ )   /* compute 1.0-correlation */
         pp += dm - gset[ii]*pset[ii] ;
      rr[jj] = -pp ;                          /* save it (or minus it)   */

      if( jj > 0 && jj%10000 == 0 ) fprintf(stderr,"at #%d rr=%g\n",jj,pp) ;
   }

   free(gset) ; free(pset) ;                  /* toss some trash */

   /* sort results */

   qsort_double( jsize , rr ) ;
   for( jj=0 ; jj < jsize ; jj++ ) rr[jj] = -rr[jj] ;  /* back to 1-corr */

   /* print results */

   if( numj > 0 ){
      dm = 1.0 / jsize ;
      for( jj=0 ; jj < numj ; jj++ )
         printf(" %.4f %g\n", dm*(jset[jj]+1.0) , rr[jset[jj]] ) ;
   } else {
      for( jj=0 ; jj < jsize ; jj++ ) printf(" %g",rr[jj]) ;
      printf("\n");
   }

   exit(0) ;
}

double rtest( int msam , int nvec , double * lamq , double * rho )
{
   int ii , jj ;
   double * zz , * gg ;

   zz = (double *) malloc(sizeof(double)*msam) ;
   gg = (double *) malloc(sizeof(double)*nvec) ;
}

/*------------------------------------------------------------------------------*/
/*------------- insertion sort : sort an array of double in-place --------------*/

void isort_double( int n , double * ar )
{
   register int  j , p ;  /* array indices */
   register double temp ;  /* a[j] holding place */
   register double * a = ar ;

   if( n < 2 || ar == NULL ) return ;

   for( j=1 ; j < n ; j++ ){

     if( a[j] < a[j-1] ){  /* out of order */
        p    = j ;
        temp = a[j] ;
        do{
          a[p] = a[p-1] ;       /* at this point, a[p-1] > temp, so move it up */
          p-- ;
        } while( p > 0 && temp < a[p-1] ) ;
        a[p] = temp ;           /* finally, put temp in its place */
     }
   }
   return ;
}

#undef  QS_CUTOFF
#undef  QS_SWAP
#undef  QS_SWAPI
#define QS_CUTOFF     20       /* cutoff to switch from qsort to isort */
#define QS_SWAP(x,y)  (temp=(x), (x)=(y),(y)=temp )
#define QS_SWAPI(x,y) (itemp=(x),(x)=(y),(y)=itemp)
#ifndef QS_STACK
# define QS_STACK 9999
#endif

static int stack[QS_STACK] ;   /* stack for qsort "recursion" */

/*--------- qsrec : recursive part of quicksort (stack implementation) ----------*/

void qsrec_double( int n , double * ar , int cutoff )
{
   register int i , j ;            /* scanning indices */
   register double temp , pivot ;  /* holding places */
   register double * a = ar ;

   int left , right , mst , itemp,nnew ;

   /* return if too short (insertion sort will clean up) */

   if( cutoff < 3 ) cutoff = 3 ;
   if( n < cutoff || ar == NULL ) return ;

   /* initialize stack to start with whole array */

   stack[0] = 0   ;
   stack[1] = n-1 ;
   mst      = 2   ;

   /* loop while the stack is nonempty */

   while( mst > 0 ){
      right = stack[--mst] ;  /* work on subarray from left -> right */
      left  = stack[--mst] ;

      i = ( left + right ) / 2 ;           /* middle of subarray */

      /*----- sort the left, middle, and right a[]'s -----*/

      if( a[left] > a[i]     ) QS_SWAP( a[left]  , a[i]     ) ;
      if( a[left] > a[right] ) QS_SWAP( a[left]  , a[right] ) ;
      if( a[i] > a[right]    ) QS_SWAP( a[right] , a[i]     ) ;

      pivot = a[i] ;                       /* a[i] is the median-of-3 pivot! */
      a[i]  = a[right] ;

      i = left ; j = right ;               /* initialize scanning */

      /*----- partition:  move elements bigger than pivot up and elements
                          smaller than pivot down, scanning in from ends -----*/

      do{
        for( ; a[++i] < pivot ; ) ;  /* scan i up,   until a[i] >= pivot */
        for( ; a[--j] > pivot ; ) ;  /* scan j down, until a[j] <= pivot */

        if( j <= i ) break ;         /* if j meets i, quit */

        QS_SWAP( a[i] , a[j] ) ;
      } while( 1 ) ;

      /*----- at this point, the array is partitioned -----*/

      a[right] = a[i] ; a[i] = pivot ;  /* restore the pivot */

      /*----- signal the subarrays that need further work -----*/

      nnew = 0 ;
      if( (i-left)  > cutoff ){ stack[mst++] = left; stack[mst++] = i-1  ; nnew++; }
      if( (right-i) > cutoff ){ stack[mst++] = i+1 ; stack[mst++] = right; nnew++; }

      /* if just added two subarrays to stack, make sure shorter one comes first */

      if( nnew == 2 && stack[mst-3] - stack[mst-4] > stack[mst-1] - stack[mst-2] ){
         QS_SWAPI( stack[mst-4] , stack[mst-2] ) ;
         QS_SWAPI( stack[mst-3] , stack[mst-1] ) ;
      }

   }  /* end of while stack is non-empty */
   return ;
}

/* sort an array partially recursively, and partially insertion */

void qsort_double( int n , double * a )
{
   qsrec_double( n , a , QS_CUTOFF ) ;
   isort_double( n , a ) ;
   return ;
}
