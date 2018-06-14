#include "cs.h"

/********************************************************************************/
/* insertion_sort : sort an array of string + void*                             */

#undef  BEFORE
#undef  AFTER
#undef  EQUALS
#define BEFORE(a,b) ( strcmp((a),(b)) <  0 )
#define AFTER(a,b)  ( strcmp((a),(b)) >  0 )
#define EQUALS(a,b) ( strcmp((a),(b)) == 0 )

static void isort_string_void( int n , char **ar , void **iar )
{
   register int  j , p ;  /* array indices */
   register char *temp ;  /* a[j] holding place */
   register void *itemp ;
   register char  **a = ar ;
   register void **ia = iar ;

   if( n < 2 ) return ;

   for( j=1 ; j < n ; j++ ){

     if( BEFORE(a[j],a[j-1]) ){   /* out of order */
        p    = j ;
        temp = a[j] ; itemp = ia[j] ;

       do{
           a[p] =  a[p-1] ; /* at this point, a[p-1] > temp, so move it up */
          ia[p] = ia[p-1] ;
          p-- ;
        } while( p > 0 && temp < a[p-1] ) ;

        a[p] = temp ;       /* finally, put temp in its place */
       ia[p] = itemp ;
     }
   }
}

/********************************************************************************/
/* qsrec : recursive part of quicksort (stack implementation)                   */

#undef  QS_SWAPF
#undef  QS_SWAPI
#undef  QS_SWAPV
#define QS_SWAPS(x,y) ( temp=(x),(x)=(y),(y)= temp)
#define QS_SWAPI(i,j) (itemp=(i),(i)=(j),(j)=itemp)
#define QS_SWAPV(i,j) (vtemp=(i),(i)=(j),(j)=vtemp)
#ifndef QS_STACK
# define QS_STACK 9999
#endif

static void qsrec_string_void( int n , char **ar , void **iar , int cutoff )
{
   register int i , j ;           /* scanning indices */
   register char *temp , *pivot ; /* holding places */
   register void *ipivot ;
   register char **a = ar ;
   register void **ia = iar ;
   int itemp ;
   void *vtemp ;

   int left , right , mst , stack[QS_STACK] , nnew ;

   /* return if too short (insertion sort will clean up) */

   if( cutoff < 3 ) cutoff = 3 ;
   if( n < cutoff ) return ;

   /* initialize stack to start with whole array */

   stack[0] = 0   ;
   stack[1] = n-1 ;
   mst      = 2   ;

   /* loop while the stack is nonempty */

   while( mst > 0 ){
      right = stack[--mst] ;  /* work on subarray from left -> right */
      left  = stack[--mst] ;

      i = ( left + right ) / 2 ;           /* middle of subarray */

      /* sort the left, middle, and right a[]'s */

      if( AFTER(a[left],a[i])     ){ QS_SWAPS(a[left] ,a[i]    ); QS_SWAPV(ia[left] ,ia[i]    ); }
      if( AFTER(a[left],a[right]) ){ QS_SWAPS(a[left] ,a[right]); QS_SWAPV(ia[left] ,ia[right]); }
      if( AFTER(a[i],a[right])    ){ QS_SWAPS(a[right],a[i]    ); QS_SWAPV(ia[right],ia[i]    ); }

      pivot  = a[i] ;                      /* a[i] is the median-of-3 pivot! */
      a[i]   = a[right] ;
      ipivot = ia[i] ;
      ia[i]  = ia[right] ;

      i = left ;                           /* initialize scanning */
      j = right ;

      /*----- partition:  move elements bigger than pivot up and elements
                          smaller than pivot down, scanning in from ends -----*/

      do{
        for( ; BEFORE(a[++i],pivot) ; ) ;  /* scan i up,   until a[i] >= pivot */
        for( ; AFTER(a[--j],pivot)  ; ) ;  /* scan j down, until a[j] <= pivot */

        if( j <= i ) break ;         /* if j meets i, quit */

        QS_SWAPS( a[i] , a[j] ) ; QS_SWAPV( ia[i] , ia[j] ) ;
      } while( 1 ) ;

      /*----- at this point, the array is partitioned -----*/

      a[right]  = a[i] ;           /*restore the pivot*/
      a[i]      = pivot ;
      ia[right] = ia[i] ;
      ia[i]     = ipivot ;

      /*----- push subarrays [left..i-1] and [i+1..right] onto stack, if big -----*/

      nnew = 0 ;
      if( (i-left)  > cutoff ){ stack[mst++] = left ; stack[mst++] = i-1   ; nnew++ ; }
      if( (right-i) > cutoff ){ stack[mst++] = i+1  ; stack[mst++] = right ; nnew++ ; }

      /* if just added two subarrays to stack, make sure shorter one comes first */

      if( nnew == 2 && stack[mst-3] - stack[mst-4] > stack[mst-1] - stack[mst-2] ){
         QS_SWAPI( stack[mst-4] , stack[mst-2] ) ;
         QS_SWAPI( stack[mst-3] , stack[mst-1] ) ;
      }

   }  /* end of while stack is non-empty */

}

/********************************************************************************/
/* quick_sort :  sort an array partially recursively, and partially insertion   */

#ifndef QS_CUTOFF
#define QS_CUTOFF 8
#endif

void qsort_string_void( int n , char **a , void **ia )
{
   qsrec_string_void( n , a , ia , QS_CUTOFF ) ;
   isort_string_void( n , a , ia ) ;
   return ;
}
