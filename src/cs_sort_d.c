#include "mrilib.h"

/*------------------------------------------------------------------------------*/
/*------------- insertion sort : sort an array of double in-place --------------*/

static void isort_double( int n , double * ar )
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

static void qsrec_double( int n , double * ar , int cutoff )
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

/*------- sort an array partially recursively, and partially insertion -------*/

/* quick_sort :  sort an array partially recursively, and partially insertion */

#undef   DTYPE
#define  DTYPE double
#include "cs_qsort_small.h"

void qsort_double( int n , double *a )
{
   if( n <= 1 ) return ;
   switch(n){
     default:                     break ;  /* handled below */
     case  2:  qsort2_double(a) ; return ;
     case  3:  qsort3_double(a) ; return ;
     case  4:  qsort4_double(a) ; return ;
     case  5:  qsort5_double(a) ; return ;
     case  6:  qsort6_double(a) ; return ;
     case  7:  qsort7_double(a) ; return ;
     case  8:  qsort8_double(a) ; return ;
     case  9:  qsort9_double(a) ; return ;
     case 10:  qsort10_double(a); return ;
     case 11:  qsort11_double(a); return ;
     case 12:  qsort12_double(a); return ;
     case 13:  qsort13_double(a); return ;
     case 14:  qsort14_double(a); return ;
     case 15:  qsort15_double(a); return ;
     case 16:  qsort16_double(a); return ;
     case 17:  qsort17_double(a); return ;
     case 18:  qsort18_double(a); return ;
     case 19:  qsort19_double(a); return ;
     case 20:  qsort20_double(a); return ;
     case 21:  qsort21_double(a); return ;
     case 25:  qsort25_double(a); return ;
     case 27:  qsort27_double(a); return ;
   }
   qsrec_double( n , a , QS_CUTOFF ) ;
   isort_double( n , a ) ;
   return ;
}
