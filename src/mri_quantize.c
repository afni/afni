/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/
   
#include "mrilib.h"

MRI_IMAGE * mri_quantize( int newcolors , MRI_IMAGE * im ) ;

int main( int argc , char * argv[] )
{
   MRI_IMAGE * fred ;
   fred = mri_read_ppm( "fred.pnm" ) ;
   mri_quantize( 100 , fred ) ;
   exit(0) ;
}

#define RGB_TO_INT(r,g,b) ((((int)(r))<<16) | (((int)(g))<< 8) | ((int)(b)))
#define INT_TO_RRR(i)     (((i) & 0xff0000) >> 16)
#define INT_TO_GGG(i)     (((i) & 0xff00)   >>  8)
#define INT_TO_BBB(i)     ( (i) & 0xff)

#define RGB_MASK          RGB_TO_INT( 0xff , 0xff , 0xff )
#define RRR_MASK          RGB_TO_INT( 0xff , 0x00 , 0x00 )
#define GGG_MASK          RGB_TO_INT( 0x00 , 0xff , 0x00 )
#define BBB_MASK          RGB_TO_INT( 0x00 , 0x00 , 0xff )

#define MAXCOL 32767

static void qsort_int_mask( int , int * , int ) ;

MRI_IMAGE * mri_quantize( int newcolors , MRI_IMAGE * im )
{
   MRI_IMAGE * intim , * outim ;
   int * intar ;
   byte * inar , * outar ;
   byte mask ;
   int ii , ncol , imask , smask ;
   int * color , * count ;

   /** sanity checks **/

   if( im == NULL             || newcolors < 2      ||   
      MRI_RGB_PTR(im) == NULL || im->kind != MRI_rgb  ) return NULL ;

   /** make copy of input image as ints **/

   inar  = MRI_RGB_PTR(im) ;
   intim = mri_new_conforming( im , MRI_int ) ;
   intar = MRI_INT_PTR(intim) ;

   for( ii=0 ; ii < im->nvox ; ii++ )
      intar[ii] = RGB_TO_INT( inar[3*ii] , inar[3*ii+1] , inar[3*ii+2] ) ;

   /** sort copy, count unique colors **/

   qsort_int_mask( im->nvox , intar , RGB_MASK ) ;
   ncol = 1 ;
   for( ii=1 ; ii < im->nvox ; ii++ )
      if( intar[ii] != intar[ii-1] ) ncol++ ;

   /** if too many colors, get rid of some by masking out low order bits **/

fprintf(stderr,"%d colors in input image\n",ncol) ;

   imask = 0 ;
   while( ncol > MAXCOL ){
      imask++ ; mask = (byte)( 256 - (1<<imask) ) ;
      smask = RGB_TO_INT( mask , mask , mask ) ;

      for( ii=0 ; ii < im->nvox ; ii++ )
#if 0
         intar[ii] &= smask ;
#else
         intar[ii] = RGB_TO_INT( inar[3*ii]   & mask ,
                                 inar[3*ii+1] & mask , inar[3*ii+2] & mask ) ;
#endif

      qsort_int_mask( im->nvox , intar , RGB_MASK ) ;
      ncol = 1 ;
      for( ii=1 ; ii < im->nvox ; ii++ )
         if( intar[ii] != intar[ii-1] ) ncol++ ;

fprintf(stderr,"%d colors in input image with mask=%d\n",ncol,(int)mask) ;
   }

   /** make color histogram **/

   color = (int *) malloc( sizeof(int) * ncol ) ;
   count = (int *) malloc( sizeof(int) * ncol ) ;

   color[0] = intar[0] ; count[0] = 1 ; ncol = 0 ;
   for( ii=1 ; ii < im->nvox ; ii++ ){
      if( intar[ii] != intar[ii-1] ){
         ncol++ ;
         color[ncol] = intar[ii] ;
         count[ncol] = 1 ;
      } else {
         count[ncol]++ ;
      }
   }
   ncol++ ;
   mri_free( intim ) ;
}

/******************************************************************************/

#undef  QS_CUTOFF
#undef  QS_SWAP
#define QS_CUTOFF     40       /* cutoff to switch from qsort to isort */
#define QS_SWAP(x,y)  (temp=(x), (x)=(y),(y)=temp)
#ifndef QS_STACK
# define QS_STACK 9999
#endif

static int stack[QS_STACK] ;  /* stack for qsort "recursion" */

/*----------------------------------------------------------------------------*/
/*------------- insertion sort : sort an array of int in-place ---------------*/

#define ILESS(a,b)  ( ((a)&smask) <  ((b)&smask) )
#define IMORE(a,b)  ( ((a)&smask) >  ((b)&smask) )
#define IEQUAL(a,b) ( ((a)&smask) == ((b)&smask) )

static void isort_int( int n , int * ar , int smask )
{
   register int  j , p ;  /* array indices */
   register int temp ;  /* a[j] holding place */
   register int * a = ar ;

   if( n < 2 || ar == NULL ) return ;

   for( j=1 ; j < n ; j++ ){

     if( ILESS(a[j],a[j-1]) ){  /* out of order */
        p    = j ;
        temp = a[j] ;
        do{
          a[p] = a[p-1] ;       /* at this point, a[p-1] > temp, so move it up */
          p-- ;
        } while( p > 0 && ILESS(temp,a[p-1]) ) ;
        a[p] = temp ;           /* finally, put temp in its place */
     }
   }
   return ;
}

/*--------- qsrec : recursive part of quicksort (stack implementation) ----------*/

static void qsrec_int( int n , int * ar , int cutoff , int smask )
{
   register int i , j ;         /* scanning indices */
   register int temp , pivot ;  /* holding places */
   register int * a = ar ;

   int left , right , mst ;

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

      if( IMORE(a[left],a[i]    ) ) QS_SWAP( a[left]  , a[i]     ) ;
      if( IMORE(a[left],a[right]) ) QS_SWAP( a[left]  , a[right] ) ;
      if( IMORE(a[i]   ,a[right]) ) QS_SWAP( a[right] , a[i]     ) ;

      pivot = a[i] ;                       /* a[i] is the median-of-3 pivot! */
      a[i]  = a[right] ;

      i = left ; j = right ;               /* initialize scanning */

      /*----- partition:  move elements bigger than pivot up and elements
                          smaller than pivot down, scanning in from ends -----*/

      do{
        for( ; ILESS(a[++i],pivot) ; ) ;  /* scan i up,   until a[i] >= pivot */
        for( ; IMORE(a[--j],pivot) ; ) ;  /* scan j down, until a[j] <= pivot */

        if( j <= i ) break ;         /* if j meets i, quit */

        QS_SWAP( a[i] , a[j] ) ;
      } while( 1 ) ;

      /*----- at this point, the array is partitioned -----*/

      a[right] = a[i] ; a[i] = pivot ;  /* restore the pivot */

      /*----- signal the subarrays that need further work -----*/

      if( (i-left)  > cutoff ){ stack[mst++] = left ; stack[mst++] = i-1   ; }
      if( (right-i) > cutoff ){ stack[mst++] = i+1  ; stack[mst++] = right ; }

   }  /* end of while stack is non-empty */
   return ;
}

/* quick_sort :  sort an array partially recursively, and partially insertion */

static void qsort_int_mask( int n , int * a , int smask )
{
   qsrec_int( n , a , QS_CUTOFF , smask ) ;
   isort_int( n , a , smask ) ;
   return ;
}
