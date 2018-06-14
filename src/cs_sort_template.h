
/*-----------------------------------------------------------------------------
  Generic qsort functions.  To generate, you must
    1) #define STYPE    datatypetobesorted
    2) #define SLT(a,b) (a<b)  must not evaluate a,b more than once!
    3) #define SNAME    nameoffunctiontobecreated
    4) #include "cs_qsort_template.h"
  Then you can call qsort_nameoffunctiontobecreated( int n , STYPE *a )
-------------------------------------------------------------------------------*/

#ifndef STYPE
# error "STYPE macro not defined"
#endif

#ifndef SLT
# error "SLT macro not defined"
#endif

#ifndef SNAME
# error "SNAME macro not defined"
#endif

#ifndef TWO_TWO
# define TWO_ONE(x,y) x ## y
# define TWO_TWO(x,y) TWO_ONE(x,y)
#endif

#define ISORT_NAME TWO_TWO(isort_,SNAME)
#define QSREC_NAME TWO_TWO(qsrec_,SNAME)
#define QSORT_NAME TWO_TWO(qsort_,SNAME)

/********************************************************************************/
/* insertion_sort : sort an array of STYPE                                      */

static void ISORT_NAME( int n , STYPE * ar )
{
   register int  j , p ;  /* array indices */
   STYPE temp ;           /* a[j] holding place */
   register STYPE *a = ar ;

   if( n < 2 ) return ;

   for( j=1 ; j < n ; j++ ){

     if( SLT(a[j],a[j-1]) ){   /* out of order */
        p    = j ;
        temp = a[j] ;

       do{
          a[p] =  a[p-1] ; /* at this point, a[p-1] > temp, so move it up */
          p-- ;
       } while( p > 0 && SLT(temp,a[p-1]) ) ;

        a[p] = temp ;       /* finally, put temp in its place */
     }
   }
}

/********************************************************************************/
/* qsrec : recursive part of quicksort (stack implementation)                   */

#undef  QS_SWAPF
#undef  QS_SWAPI
#define QS_SWAPF(x,y) ( temp=(x),(x)=(y),(y)= temp)
#define QS_SWAPI(i,j) (itemp=(i),(i)=(j),(j)=itemp)
#ifndef QS_STACK
# define QS_STACK 9999
#endif

static void QSREC_NAME( int n , STYPE * ar , int cutoff )
{
   register int i , j ;       /* scanning indices */
   STYPE temp , pivot ;       /* holding places */
   register STYPE *a = ar ;
   int itemp ;

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
      right = stack[--mst] ;  /* work on subarray from left..right */
      left  = stack[--mst] ;

      i = ( left + right ) / 2 ;        /* middle of subarray */

      /* sort the left, middle, and right a[]'s */

      if( SLT(a[i],a[left])     ){ QS_SWAPF(a[left] ,a[i]    ); }
      if( SLT(a[right],a[left]) ){ QS_SWAPF(a[left] ,a[right]); }
      if( SLT(a[right],a[left]) ){ QS_SWAPF(a[right],a[i]    ); }

      pivot = a[i] ;                    /* a[i] is the median-of-3 pivot */
      a[i]  = a[right] ;

      i = left ;                        /* initialize scanning */
      j = right ;

      /*----- partition:  move elements bigger than pivot up and elements
                          smaller than pivot down, scanning in from ends -----*/

      do{
        for( ; SLT(a[++i],pivot) ; ) ;  /* scan i up,   until a[i] >= pivot */
        for( ; SLT(pivot,a[--j]) ; ) ;  /* scan j down, until a[j] <= pivot */

        if( j <= i ) break ;            /* if j meets i, quit */

        QS_SWAPF( a[i] , a[j] ) ;       /* put a[i], a[j] into correct halves */
      } while( 1 ) ;

      /*----- at this point, the array is partitioned -----*/

      a[right]  = a[i] ;                /* restore the pivot */
      a[i]      = pivot ;

      /*----- push subarrays [left..i-1] and [i+1..right] onto stack, if big -----*/

      nnew = 0 ;
      if( (i-left)  > cutoff ){ stack[mst++] = left; stack[mst++] = i-1  ; nnew++; }
      if( (right-i) > cutoff ){ stack[mst++] = i+1 ; stack[mst++] = right; nnew++; }

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
# define QS_CUTOFF 10
#endif

void QSORT_NAME( int n , STYPE *a )
{
   QSREC_NAME( n , a , QS_CUTOFF ) ;
   ISORT_NAME( n , a ) ;
}

#undef ISORT_NAME
#undef QSREC_NAME
#undef QS_SWAPF
#undef QS_SWAPI
