#include "qsort.h"

#ifdef DEBUG
#include <time.h>
#define FCLOCK ((float)clock()/((float)CLOCKS_PER_SEC))
#endif

/********************************************************************************/
/* insertion_sort : sort an array of short */

void isort_sh( int n , short * ar )
{
   register int  j , p ;  /* array indices */
   register short temp ;  /* a[j] holding place */
   register short * a = ar ;

   if( n < 2 ) return ;

   for( j=1 ; j < n ; j++ ){

     if( a[j] < a[j-1] ){  /* out of order */
        p    = j ;
        temp = a[j] ;

       do{
          a[p] = a[p-1] ;       /*at this point, a[p-1] > temp, so move it up*/
         p-- ;
        } while( p > 0 && temp < a[p-1] ) ;

        a[p] = temp ;           /*finally, put temp in its place*/
     }
   }
}


/********************************************************************************/
/* qsrec : recursive part of quicksort (stack implementation) */

#define QS_SWAP(x,y) (temp=(x),(x)=(y),(y)=temp)
#ifndef QS_STACK
# define QS_STACK 9999
#endif

void qsrec_sh( int n , short * ar , int cutoff )
{
   register int i , j ;           /* scanning indices */
   register short temp , pivot ;  /* holding places */
   register short * a = ar ;

   int left , right , mst , stack[QS_STACK] ;

#ifdef DEBUG
   int max_st = 2 ;
#endif

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

      if( a[left] > a[i]     ) QS_SWAP( a[left]  , a[i]     ) ;
      if( a[left] > a[right] ) QS_SWAP( a[left]  , a[right] ) ;
      if( a[i] > a[right]    ) QS_SWAP( a[right] , a[i]     ) ;

      pivot = a[i] ;                        /* a[i] is the median-of-3 pivot! */
      a[i]  = a[right] ;

      i = left ;                            /* initialize scanning */
      j = right ;

      /*----- partition:  move elements bigger than pivot up and elements
                          smaller than pivot down, scanning in from ends -----*/

      do{
        for( ; a[++i] < pivot ; ) ;  /* scan i up,   until a[i] >= pivot */
        for( ; a[--j] > pivot ; ) ;  /* scan j down, until a[j] <= pivot */

        if( j <= i ) break ;         /* if j meets i, quit */

        QS_SWAP( a[i] , a[j] ) ;
      } while( 1 ) ;

      /*----- at this point, the array is partitioned -----*/

      a[right] = a[i] ;           /*restore the pivot*/
      a[i]     = pivot ;

      if( (i-left)  > cutoff ){ stack[mst++] = left ; stack[mst++] = i-1   ; }
      if( (right-i) > cutoff ){ stack[mst++] = i+1  ; stack[mst++] = right ; }

#ifdef DEBUG
   if( mst > max_st ) max_st = mst ;
#endif

   }  /* end of while stack is non-empty */

#ifdef DEBUG
   printf("qsort: for n=%d max stack depth=%d\n",n,max_st) ;
#endif

} /*qsrec*/

/********************************************************************************/
/* quick_sort :  sort an array partially recursively, and partially insertion */

#ifndef QS_CUTOFF
#define QS_CUTOFF 40
#endif

void qsort_sh( int n , short * a )
{
#ifdef DEBUG
   float clk1 , clk2 , clk3 ;
   clk1 = FCLOCK ;
#endif

   qsrec_sh( n , a , QS_CUTOFF ) ;

#ifdef DEBUG
   clk2 = FCLOCK ;
#endif

   isort_sh( n , a ) ;

#ifdef DEBUG
   clk3 = FCLOCK ;
   printf("qsort_sh cpu: qsrec=%f isort=%f\n",clk2-clk1,clk3-clk2) ;
#endif

}

void qsort_partial_sh( int n , short * a , int cutoff )
{
   qsrec_sh( n , a , cutoff ) ;
   if( cutoff < 2 ) isort_sh( n , a ) ;
}

/*-----------------------------------------------------------------------
   compute an approximation to the inverse histogram of an array of shorts:

     n    = number of input shorts
     ar   = array of input shorts
     nbin = number of output bins (e.g., 101 for percentage points)
     ih   = array [nbin] of shorts:
            ih[k] = value that has k/(nbin-1) fraction of the
                    points in ar below it (approximately) for k=0..nbin-1;
            ih[0] = min of ar, ih[nbin-1] = max of ar
-------------------------------------------------------------------------*/

int MCW_inverse_histogram_sh( int n , short * ar , int nbin , short * ih )
{
   int k , i , nwin,nhalf , sum,ibot,itop ;
   float fwin ;
   short * at ;

   if( n < 2 || nbin < 2 || ar == NULL || ih == NULL ) return 0 ;  /* bad inputs */

   fwin = ((float) n) / (float) nbin ;
   nwin = (int) fwin ; nhalf = (int) (0.5*fwin) ;

   at = (short *) malloc( n * sizeof(short) ) ; if( at == NULL ) return 0 ;
   memcpy( at , ar , n * sizeof(short) ) ;

   qsort_partial_sh( n , at , nhalf ) ;

   ih[0] = at[0] ;
   for( i=1 ; i < nwin ; i++ ) if( ih[0] > at[i] ) ih[0] = at[i] ;

   ih[nbin-1] = at[n-1] ;
   for( i=1 ; i < nwin ; i++ ) if( ih[nbin-1] < at[n-1-i] ) ih[nbin-1] = at[n-1-i] ;

   for( k=1 ; k < nbin-1 ; k++ ){

      sum  = 0 ;
      ibot = (k-0.5) * fwin ;
      itop = (k+0.5) * fwin + 1.49 ;
      if( itop > nbin ) itop = nbin ; if( itop <= ibot ) itop = ibot+1 ;

      for( i=ibot ; i < itop ; i++ ) sum += at[i] ;

      ih[k] = (short)( sum / (itop-ibot) ) ;
#ifdef DEBUG
   printf("MCW_inverse_historgram: k=%d ibot=%d itop=%d sum=%d ih[k]=%d\n",
	  k,ibot,itop,ih[k]) ;
#endif
   }

   free( at ) ;
   return 1 ;
}
