/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/

#include "mrilib.h"

/*------------------------------------------------------------------------
   Compute the median of an array of floats.  Will rearrange (partially
   sort) the array in the process.  The algorithm is based on Quicksort,
   where we only keep the partition that has the middle element.
   For large n, this is faster than finishing the whole sort.
--------------------------------------------------------------------------*/

static float median_float4(float,float,float,float) ;
static float median_float5(float *) ;  /* 5,7,9 added 20 Oct 2000 */
static float median_float7(float *) ;
static float median_float9(float *) ;

/* macro for median-of-3 */

#undef  MED3
#define MED3(a,b,c) ( (a < b) ? ( (a > c) ? a : (b > c) ? c : b )  \
                              : ( (b > c) ? b : (a > c) ? c : a ) )

/* macro for swap (duh) */

#undef  SWAP
#define SWAP(x,y) (temp=x,x=y,y=temp)

float qmed_float( int n , float * ar )
{
   register int i , j ;           /* scanning indices */
   register float temp , pivot ;  /* holding places */
   register float * a = ar ;

   int left , right , mid , nodd ;

   switch( n ){
      case 1: return ar[0] ;
      case 2: return 0.5*(ar[0]+ar[1]) ;
      case 3: return MED3( ar[0] , ar[1] , ar[2] ) ;
      case 4: return median_float4( ar[0],ar[1],ar[2],ar[3] ) ;
      case 5: return median_float5( ar ) ;
      case 7: return median_float7( ar ) ;
      case 9: return median_float9( ar ) ;
   }

   left = 0 ; right = n-1 ; mid = n/2 ; nodd = ((n & 1) != 0) ;

   /* loop while the subarray is at least 3 long */

   while( right-left > 1  ){  /* work on subarray from left -> right */

      i = ( left + right ) / 2 ;   /* middle of subarray */

      /* sort the left, middle, and right a[]'s */

      if( a[left] > a[i]     ) SWAP( a[left]  , a[i]     ) ;
      if( a[left] > a[right] ) SWAP( a[left]  , a[right] ) ;
      if( a[i] > a[right]    ) SWAP( a[right] , a[i]     ) ;

      pivot = a[i] ;                 /* a[i] is the median-of-3 pivot! */
      a[i]  = a[right] ;

      i = left ;                     /* initialize scanning */
      j = right ;

      /*----- partition:  move elements bigger than pivot up and elements
                          smaller than pivot down, scanning in from ends -----*/

      do{
        for( ; a[++i] < pivot ; ) ;  /* scan i up,   until a[i] >= pivot */
        for( ; a[--j] > pivot ; ) ;  /* scan j down, until a[j] <= pivot */

        if( j <= i ) break ;         /* if j meets i, quit */

        SWAP( a[i] , a[j] ) ;
      } while( 1 ) ;

      /*----- at this point, the array is partitioned -----*/

      a[right] = a[i] ;           /* restore the pivot */
      a[i]     = pivot ;

      if( i == mid ){             /* good luck */
         if( nodd ) return pivot ;   /* exact middle of array */

         temp = a[left] ;            /* must find next largest element below */
         for( j=left+1 ; j < i ; j++ )
            if( a[j] > temp ) temp = a[j] ;

         return 0.5*(pivot+temp) ;   /* return average */
      }

      if( i <  mid ) left  = i ; /* throw away bottom partition */
      else           right = i ; /* throw away top partition    */

   }  /* end of while sub-array is long */

   return (nodd) ? a[mid] : 0.5*(a[mid]+a[mid-1]) ;
}

/*---------------------------------------------------------------
  Return median and MAD, nondestructively -- 08 Mar 2001 - RWCox
-----------------------------------------------------------------*/

void qmedmad_float( int n, float *ar, float *med, float *mad )
{
   float * q = (float *) malloc(sizeof(float)*n) ;
   float me,ma ;
   register int ii ;

   memcpy(q,ar,sizeof(float)*n) ;  /* duplicate input array */
   me = qmed_float( n , q ) ;      /* compute median */

   for( ii=0 ; ii < n ; ii++ )     /* subtract off median */
      q[ii] = fabs(q[ii]-me) ;     /* (absolute deviation) */

   ma = qmed_float( n , q ) ;      /* MAD = median absolute deviation */

   *med = me ; *mad = ma ; return ;
}

/*---------------------------------------------------------------*/

static float median_float4(float a, float b, float c, float d)
{
  register float t1,t2,t3;

  if (a > b){t1 = a; a = b;} else t1 = b;  /* t1 = max(a,b) */
  if (c > d){t2 = d; d = c;} else t2 = c;  /* t2 = min(c,d) */

  /* now have: a <= t1 and t2 <= d */

  t3 = (a > t2) ? a : t2 ;
  t2 = (d < t1) ? d : t1 ;

  return 0.5*(t2+t3) ;
}

/*=======================================================================
 * Median sorting:
 * These highly optimized functions are used to find the median
 * value out of an array of 5, 7, or 9 pixel values.
 * They are a simple implementation of the minimum number of comparisons
 * required to find the median.
 *
 * Reference used: Implementing median filters in XC4000E FPGA's
 * John L. Smith <jsmith@univision.com>
 * http://www.xilinx.com/xcell/xcell23.htm/xl23_16.pdf
 *
 * N. Devillard <nDevil@eso.org> - September 1997
 * Please distribute this code freely -- no warranty
 *=======================================================================*/

/*-----------------------------------------------------------------------
 * Function: median_float5(), median_float7(), median_float9()
 * In      : A pointer to an array containing resp. 5, 7, or 9 pixel values
 * Out     : The median pixel value of the input array
 * Job     : Fast computation of the median of 5, 7, or 9 values
 * Notice  : The input array is modified: partly sorted so that the
 *            middle element is the median
 *            No bound checking to gain time, it is up to the caller
 *            to make sure arrays are allocated
 *---------------------------------------------------------------------*/

#undef  SORT2
#define SORT2(a,b) if(a>b) SWAP(a,b);

static float median_float5(float *p)
{
    register float temp ;
    SORT2(p[0],p[1]) ; SORT2(p[3],p[4]) ; SORT2(p[0],p[3]) ;
    SORT2(p[1],p[4]) ; SORT2(p[1],p[2]) ; SORT2(p[2],p[3]) ;
    SORT2(p[1],p[2]) ; return(p[2]) ;
}

static float median_float7(float *p)
{
    register float temp ;
    SORT2(p[0],p[1]) ; SORT2(p[4],p[5]) ; SORT2(p[1],p[2]) ;
    SORT2(p[5],p[6]) ; SORT2(p[0],p[1]) ; SORT2(p[4],p[5]) ;
    SORT2(p[0],p[4]) ; SORT2(p[2],p[6]) ; SORT2(p[1],p[3]) ;
    SORT2(p[3],p[5]) ; SORT2(p[1],p[3]) ; SORT2(p[2],p[3]) ;
    SORT2(p[3],p[4]) ; SORT2(p[2],p[3]) ; return(p[3]) ;
}

static float median_float9(float *p)
{
    register float temp ;
    SORT2(p[1],p[2]) ; SORT2(p[4],p[5]) ; SORT2(p[7],p[8]) ;
    SORT2(p[0],p[1]) ; SORT2(p[3],p[4]) ; SORT2(p[6],p[7]) ;
    SORT2(p[1],p[2]) ; SORT2(p[4],p[5]) ; SORT2(p[7],p[8]) ;
    SORT2(p[0],p[3]) ; SORT2(p[5],p[8]) ; SORT2(p[4],p[7]) ;
    SORT2(p[3],p[6]) ; SORT2(p[1],p[4]) ; SORT2(p[2],p[5]) ;
    SORT2(p[4],p[7]) ; SORT2(p[4],p[2]) ; SORT2(p[6],p[4]) ;
    SORT2(p[4],p[2]) ; return(p[4]) ;
}
