/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/
   
#include "mrilib.h"

/*** 7D SAFE ***/

/* prototypes for internal sorting routines:
     array of shorts
     array of ints
     array of floats
     array of floats, with an integer array carried along in the swaps */

void qsrec_short( int , short * , int ) ;
void qsrec_int  ( int , int   * , int ) ;
void qsrec_float( int , float * , int ) ;
void qsrec_pair ( int , float * , int * , int ) ;

#define QS_CUTOFF     40       /* cutoff to switch from qsort to isort */
#define QS_STACK      4096     /* qsort stack size */
#define QS_SWAP(x,y)  (temp=(x), (x)=(y),(y)=temp)

/***************************************************************************
     Each qsort_TYPE routine (TYPE=short, int, float, or pair) has two
     pieces.  The isort_TYPE routine does an insertion sort routine,
     which is only fast for nearly sorted routines.  The qsrec_TYPE
     routine carries out the quicksort algorithm down to the level
     QS_CUTOFF -- at that point, the array is nearly sorted.

     In the qsrec_TYPE routines, the fundamental operation is
     the SWAP of two items.  In the isort_TYPE routines, the
     value at the j index is determined to be out of place, so
     it is stored in a temporary variable and the values below
     it are moved up in the array until the proper place for
     the former a[j] is found.  Then it is stored where it belongs.
     Compare the isort_short and isort_pair routines to see how
     this should be generalized to more complex objects.

                                                      -- Robert W. Cox
***************************************************************************/

/*------------------------------------------------------------------------------*/
/*------------- insertion sort : sort an array of short in-place ---------------*/

void isort_short( int n , short * ar )
{
   register int  j , p ;  /* array indices */
   register short temp ;  /* a[j] holding place */
   register short * a = ar ;

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

/*--------- qsrec : recursive part of quicksort (stack implementation) ----------*/

void qsrec_short( int n , short * ar , int cutoff )
{
   register int i , j ;           /* scanning indices */
   register short temp , pivot ;  /* holding places */
   register short * a = ar ;

   int left , right , mst ;
   int stack[QS_STACK] ;  /* stack for qsort "recursion" */

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

      if( (i-left)  > cutoff ){ stack[mst++] = left ; stack[mst++] = i-1   ; }
      if( (right-i) > cutoff ){ stack[mst++] = i+1  ; stack[mst++] = right ; }

   }  /* end of while stack is non-empty */
   return ;
}

/* quick_sort :  sort an array partially recursively, and partially insertion */

void qsort_short( int n , short * a )
{
   qsrec_short( n , a , QS_CUTOFF ) ;
   isort_short( n , a ) ;
   return ;
}

/*----------------------------------------------------------------------------*/
/*------------- insertion sort : sort an array of int in-place ---------------*/

void isort_int( int n , int * ar )
{
   register int  j , p ;  /* array indices */
   register int temp ;    /* a[j] holding place */
   register int * a = ar ;

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

/*--------- qsrec : recursive part of quicksort (stack implementation) ----------*/

void qsrec_int( int n , int * ar , int cutoff )
{
   register int i , j ;         /* scanning indices */
   register int temp , pivot ;  /* holding places */
   register int * a = ar ;

   int left , right , mst ;
   int stack[QS_STACK] ;  /* stack for qsort "recursion" */

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

      if( (i-left)  > cutoff ){ stack[mst++] = left ; stack[mst++] = i-1   ; }
      if( (right-i) > cutoff ){ stack[mst++] = i+1  ; stack[mst++] = right ; }

   }  /* end of while stack is non-empty */
   return ;
}

/* quick_sort:  sort an array partially recursively, and partially insertion */

void qsort_int( int n , int * a )
{
   qsrec_int( n , a , QS_CUTOFF ) ;
   isort_int( n , a ) ;
   return ;
}

/*------------------------------------------------------------------------------*/
/*------------- insertion sort : sort an array of float in-place ---------------*/

void isort_float( int n , float * ar )
{
   register int  j , p ;  /* array indices */
   register float temp ;  /* a[j] holding place */
   register float * a = ar ;

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

/*--------- qsrec : recursive part of quicksort (stack implementation) ----------*/

void qsrec_float( int n , float * ar , int cutoff )
{
   register int i , j ;           /* scanning indices */
   register float temp , pivot ;  /* holding places */
   register float * a = ar ;

   int left , right , mst ;
   int stack[QS_STACK] ;  /* stack for qsort "recursion" */

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

      if( (i-left)  > cutoff ){ stack[mst++] = left ; stack[mst++] = i-1   ; }
      if( (right-i) > cutoff ){ stack[mst++] = i+1  ; stack[mst++] = right ; }

   }  /* end of while stack is non-empty */
   return ;
}

/* quick_sort :  sort an array partially recursively, and partially insertion */

void qsort_float( int n , float *a )
{
   qsrec_float( n , a , QS_CUTOFF ) ;
   isort_float( n , a ) ;
   return ;
}

void qsort_float_rev( int n , float *a )
{
   register int ii ;
   if( n < 2 || a == NULL ) return ;
   for( ii=0 ; ii < n ; ii++ ) a[ii] = -a[ii] ;
   qsort_float(n,a) ;
   for( ii=0 ; ii < n ; ii++ ) a[ii] = -a[ii] ;
   return ;
}

/*------------------------------------------------------------------------------*/
/*--------------- insertion sort of a float-int pair of arrays -----------------*/

void isort_pair( int n , float * ar , int * iar )
{
   register int  j , p ;  /* array indices */
   register float temp ;  /* a[j] holding place */
   register int  itemp ;
   register float * a = ar ;
   register int   *ia = iar ;

   if( n < 2 || ar == NULL || iar == NULL ) return ;

   for( j=1 ; j < n ; j++ ){

     if( a[j] < a[j-1] ){  /* out of order */
        p    = j ;
        temp = a[j] ; itemp = ia[j] ;
        do{
          a[p] = a[p-1] ; ia[p] = ia[p-1] ;
          p-- ;
        } while( p > 0 && temp < a[p-1] ) ;
        a[p] = temp ; ia[p] = itemp ;
     }
   }
   return ;
}

/*--------- qsrec : recursive part of quicksort (stack implementation) ----------*/

#define QS_ISWAP(x,y) (itemp=(x),(x)=(y),(y)=itemp)

void qsrec_pair( int n , float * ar , int * iar , int cutoff )
{
   register int i , j ;           /* scanning indices */
   register float temp , pivot ;  /* holding places */
   register int  itemp ,ipivot ;
   register float * a = ar ;
   register int   *ia = iar ;

   int left , right , mst ;
   int stack[QS_STACK] ;  /* stack for qsort "recursion" */

   /* return if too short (insertion sort will clean up) */

   if( cutoff < 3 ) cutoff = 3 ;
   if( n < cutoff || ar == NULL || iar == NULL ) return ;

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

      if( a[left] > a[i]     ){ QS_SWAP ( a[left]  , a[i]     ) ;
                                QS_ISWAP(ia[left]  ,ia[i]     ) ; }
      if( a[left] > a[right] ){ QS_SWAP ( a[left]  , a[right] ) ;
                                QS_ISWAP(ia[left]  ,ia[right] ) ; }
      if( a[i] > a[right]    ){ QS_SWAP ( a[right] , a[i]     ) ;
                                QS_ISWAP(ia[right] ,ia[i]     ) ; }

       pivot = a[i] ;  a[i] = a[right] ;
      ipivot =ia[i] ; ia[i] =ia[right] ;

      i = left ; j = right ;               /* initialize scanning */

      /*----- partition:  move elements bigger than pivot up and elements
                          smaller than pivot down, scanning in from ends -----*/

      do{
        for( ; a[++i] < pivot ; ) ;  /* scan i up,   until a[i] >= pivot */
        for( ; a[--j] > pivot ; ) ;  /* scan j down, until a[j] <= pivot */

        if( j <= i ) break ;         /* if j meets i, quit */

        QS_SWAP ( a[i] , a[j] ) ;
        QS_ISWAP(ia[i] ,ia[j] ) ;
      } while( 1 ) ;

      /*----- at this point, the array is partitioned -----*/

      a[right] = a[i] ; a[i] = pivot ;  /* restore the pivot */
      ia[right]=ia[i] ;ia[i] =ipivot ;

      /*----- signal the subarrays that need further work -----*/

      if( (i-left)  > cutoff ){ stack[mst++] = left ; stack[mst++] = i-1   ; }
      if( (right-i) > cutoff ){ stack[mst++] = i+1  ; stack[mst++] = right ; }

   }  /* end of while stack is non-empty */
   return ;
}

/* quick_sort :  sort an array partially recursively, and partially insertion */

void qsort_pair( int n , float *a , int *ia )
{
   qsrec_pair( n , a , ia , QS_CUTOFF ) ;
   isort_pair( n , a , ia ) ;
   return ;
}

/*******************************************************************************
  Compute the "percentage points" of the histogram of an image.
  "per" should be a pre-allocated array of "nper+1" floats; per[i] will be set
  to the image intensity below which fraction i/nper of the pixels fall,
  for i=0..nper.
  N.B.:  per[0] = image minimum and per[nper] = maximum.
        "per" is float, no matter what the input image type is.
********************************************************************************/

void mri_percents( MRI_IMAGE *im , int nper , float per[] )
{
   register int pp , ii , nvox ;
   register float fi , frac ;

   /*** sanity checks ***/

   if( im == NULL || per == NULL || nper < 2 ) return ;

   nvox = im->nvox ;
   frac = nvox / ((float) nper) ;

   switch( im->kind ){

      /*** create a float image copy of the data,
           sort it, then interpolate the percentage points ***/

      default:{
         MRI_IMAGE *inim ;
         float *far ;

         inim = mri_to_float( im ) ;
         far  = MRI_FLOAT_PTR(inim) ;
         qsort_float( nvox , far ) ;

         per[0] = far[0] ;
         for( pp=1 ; pp < nper ; pp++ ){
            fi = frac * pp ; ii = fi ; fi = fi - ii ;
            per[pp] = (1.0-fi) * far[ii] + fi * far[ii+1] ;
         }
         per[nper] = far[nvox-1] ;
         mri_free( inim ) ;
      }
      break ;

      /*** create a short image copy of the data,
           sort it, then interpolate the percentage points ***/

      case MRI_short:
      case MRI_byte:{
         MRI_IMAGE *inim ;
         short *sar ;

         inim = mri_to_short( 1.0 , im ) ;
         sar  = MRI_SHORT_PTR(inim) ;
         qsort_short( nvox , sar ) ;

         per[0] = sar[0] ;
         for( pp=1 ; pp < nper ; pp++ ){
            fi = frac * pp ; ii = fi ; fi = fi - ii ;
            per[pp] = (1.0-fi) * sar[ii] + fi * sar[ii+1] ;
         }
         per[nper] = sar[nvox-1] ;
         mri_free( inim ) ;
      }
   }

   return ;
}

/****************************************************************************
   Produce a "histogram flattened" version of the input image.  That is, the
   output value at each pixel will be proportional to its place in the
   histogram of intensities.  That is, if the value at a pixel is the 173rd
   in intensity (0th is darkest) out of 1000 pixels total, then the flattened
   image value will be 0.173.  The output image is in MRI_float format.
*****************************************************************************/

MRI_IMAGE * mri_flatten( MRI_IMAGE * im )
{
   MRI_IMAGE * flim , * intim , * outim ;
   float * far , * outar ;
   int * iar ;
   int ii , nvox , ibot,itop , nvox1 ;
   float fac , val ;

#ifdef DEBUG
printf("Entry: mri_flatten\n") ;
#endif

   if( im == NULL ) return NULL ;

   /*** make an image that is just the voxel index in its array ***/
   /*** also, make the output image while we are at it          ***/

   nvox  = im->nvox ;
   intim = mri_new_conforming( im , MRI_int ) ;
   outim = mri_new_conforming( im , MRI_float ) ;

   iar = MRI_INT_PTR(intim) ; outar = MRI_FLOAT_PTR(outim) ;

   for( ii=0 ; ii < nvox ; ii++ ) iar[ii] = ii ;

   /*** copy the input data to a floating point image ***/

   flim = mri_to_float( im ) ; far = MRI_FLOAT_PTR(flim) ;

   /*** sort this image, with the index array being carried along
        so that we know where every pixel came from originally  ***/

   qsort_pair( nvox , far , iar ) ;

   /*** The "far" array is now sorted.  Thus, if the pixel that was in
        voxel i is now in voxel j, then its place in the histogram is
        j/nvox.  The only difficulty is that there may be ties.  We need
        to resolve these ties so that pixels with the same intensity
        don't get different output values.  We do this by scanning
        through far, finding blocks of equal values, and replacing
        them by their average position in the histogram.
   ***/

   fac = 1.0 / nvox ; nvox1 = nvox - 1 ;

   for( ibot=0 ; ibot < nvox1 ; ){

      /** if this value is unique, just set the value and move on **/

      val = far[ibot] ; itop = ibot+1 ;
      if( val != far[itop] ){
         far[ibot] = fac * ibot ;
         ibot++ ; continue ;
      }

      /** scan itop up until value is distinct **/

      for( ; itop < nvox1 && val == far[itop] ; itop++ ) ; /* nada */

      val = 0.5*fac * (ibot+itop-1) ;
      for( ii=ibot ; ii < itop ; ii++ ) far[ii] = val ;
      ibot = itop ;
   }
   far[nvox1] = 1.0 ;

   /*** now propagate these values back to the output image ***/

   for( ii=0 ; ii < nvox ; ii++ ) outar[iar[ii]] = far[ii] ;

   mri_free( flim ) ; mri_free( intim ) ;

   MRI_COPY_AUX( outim , im ) ;
   return outim ;
}

/*-------------------------------------------------------------------*/
/*! Find the intensity in an image that is at the alpha-th
    quantile of the distribution.  That is, for 0 <= alpha <= 1,
    alpha*npix of the image values are below, and (1-alpha)*npix
    are above.  If alpha is 0, this is the minimum.  If alpha
    is 1, this is the maximum.
---------------------------------------------------------------------*/

float mri_quantile( MRI_IMAGE *im , float alpha )
{
   int ii , nvox ;
   float fi , quan ;

ENTRY("mri_quantile") ;

   /*** sanity checks ***/

   if( im == NULL ) RETURN( 0.0 );

   if( alpha <= 0.0 ) RETURN( (float) mri_min(im) );
   if( alpha >= 1.0 ) RETURN( (float) mri_max(im) );

   nvox = im->nvox ;

   switch( im->kind ){

      /*** create a float image copy of the data,
           sort it, then interpolate the percentage points ***/

      default:{
         MRI_IMAGE *inim ;
         float *far ;

         inim = mri_to_float( im ) ;
         far  = MRI_FLOAT_PTR(inim) ;
         qsort_float( nvox , far ) ;

         fi   = alpha * nvox ;
         ii   = (int) fi ; if( ii >= nvox ) ii = nvox-1 ;
         fi   = fi - ii ;
         quan = (1.0-fi) * far[ii] + fi * far[ii+1] ;
         mri_free( inim ) ;
      }
      break ;

      /*** create a short image copy of the data,
           sort it, then interpolate the percentage points ***/

      case MRI_short:
      case MRI_byte:{
         MRI_IMAGE *inim ;
         short *sar ;

         inim = mri_to_short( 1.0 , im ) ;
         sar  = MRI_SHORT_PTR(inim) ;
         qsort_short( nvox , sar ) ;

         fi   = alpha * nvox ;
         ii   = (int) fi ; if( ii >= nvox ) ii = nvox-1 ;
         fi   = fi - ii ;
         quan = (1.0-fi) * sar[ii] + fi * sar[ii+1] ;
         mri_free( inim ) ;
      }
      break ;
   }

   RETURN( quan );
}

/*-------------------------------------------------------------------*/
/*! Return TWO quantile levels at once; cf. mri_quantile().
---------------------------------------------------------------------*/

float_pair mri_twoquantiles( MRI_IMAGE *im, float alpha, float beta )
{
   int ii , nvox ;
   float fi ;
   float_pair qt = {0.0f,0.0f} ;
   float qalph=WAY_BIG,qbeta=WAY_BIG ;

ENTRY("mri_twoquantiles") ;

   /*** sanity checks ***/

   if( im == NULL ) RETURN( qt );

   if( alpha == beta ){
     qt.a = qt.b = mri_quantile(im,alpha) ; RETURN( qt );
   }

        if( alpha <= 0.0f ) qalph = (float) mri_min(im) ;
   else if( alpha >= 1.0f ) qalph = (float) mri_max(im) ;
        if( beta  <= 0.0f ) qbeta = (float) mri_min(im) ;
   else if( beta  >= 1.0f ) qbeta = (float) mri_max(im) ;

   if( qalph != WAY_BIG && qbeta != WAY_BIG ){
     qt.a = qalph; qt.b = qbeta; RETURN(qt);
   }

   nvox = im->nvox ;

   switch( im->kind ){

      /*** create a float image copy of the data,
           sort it, then interpolate the percentage points ***/

      default:{
         MRI_IMAGE *inim ;
         float *far ;

         inim = mri_to_float( im ) ;
         far  = MRI_FLOAT_PTR(inim) ;
         qsort_float( nvox , far ) ;

         if( alpha > 0.0f && alpha < 1.0f ){
           fi    = alpha * nvox ;
           ii    = (int) fi ; if( ii >= nvox ) ii = nvox-1 ;
           fi    = fi - ii ;
           qalph = (1.0-fi) * far[ii] + fi * far[ii+1] ;
         }
         if( beta > 0.0f && beta < 1.0f ){
           fi    = beta * nvox ;
           ii    = (int) fi ; if( ii >= nvox ) ii = nvox-1 ;
           fi    = fi - ii ;
           qbeta = (1.0-fi) * far[ii] + fi * far[ii+1] ;
         }
         mri_free( inim ) ;
      }
      break ;

      /*** create a short image copy of the data,
           sort it, then interpolate the percentage points ***/

      case MRI_short:
      case MRI_byte:{
         MRI_IMAGE *inim ;
         short *sar ;

         inim = mri_to_short( 1.0 , im ) ;
         sar  = MRI_SHORT_PTR(inim) ;
         qsort_short( nvox , sar ) ;

         if( alpha > 0.0f && alpha < 1.0f ){
           fi    = alpha * nvox ;
           ii    = (int) fi ; if( ii >= nvox ) ii = nvox-1 ;
           fi    = fi - ii ;
           qalph = (1.0-fi) * sar[ii] + fi * sar[ii+1] ;
         }
         if( beta > 0.0f && beta < 1.0f ){
           fi    = beta * nvox ;
           ii    = (int) fi ; if( ii >= nvox ) ii = nvox-1 ;
           fi    = fi - ii ;
           qbeta = (1.0-fi) * sar[ii] + fi * sar[ii+1] ;
         }
         mri_free( inim ) ;
      }
      break ;
   }

   qt.a = qalph; qt.b = qbeta; RETURN(qt);
}
