/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/

#include "mrilib.h"

/*------------------------------------------------------------------------*/

float qmean_float( int n , float *ar )
{
   int ii ; float sum=0.0f ;

   if( n <= 0 || ar == NULL ) return sum ;

   for( ii=0 ; ii < n ; ii++ ) sum += ar[ii] ;
   sum /= n ; return sum ;
}


/*------------------------------------------------------------------------
    Compute non-zero mean of an array of floats 
-------------------------------------------------------------------------*/

float qnzmean_float( int n , float *ar )
{
   int ii ; float sum=0.0f, v ;
   int ngood = 0;

   if( n <= 0 || ar == NULL ) return sum ;

   for( ii=0 ; ii < n ; ii++ ){
	    v = ar[ii];
	    if(v!=0.0) {
			sum += ar[ii] ;
			ngood++;
	    }
   }
   if( ngood <=0 ) sum = 0.0f ;   /* might have no nonzero voxels */
   else            sum /= ngood ; /* otherwise return average */
   return sum ;
}


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
static float median_float25(float *) ;

/* macro for median-of-3 */

#undef  MED3
#define MED3(a,b,c) ( (a < b) ? ( (a > c) ? a : (b > c) ? c : b )  \
                              : ( (b > c) ? b : (a > c) ? c : a ) )

/* macro for swap (duh) */

#undef  SWAP
#define SWAP(x,y) (temp=x,x=y,y=temp)

float qmed_float( int n , float *ar )
{
   register int i , j ;           /* scanning indices */
   register float temp , pivot ;  /* holding places */
   register float *a = ar ;

   int left , right , mid , nodd ;

   /* special cases */

   switch( n ){
      case 0: return 0.0f ;  /* should not happen */
      case 1: return ar[0] ;
      case 2: return 0.5*(ar[0]+ar[1]) ;
      case 3: return MED3( ar[0] , ar[1] , ar[2] ) ;
      case 4: return median_float4( ar[0],ar[1],ar[2],ar[3] ) ;
      case 5: return median_float5( ar ) ;
      case 7: return median_float7( ar ) ;
      case 9: return median_float9( ar ) ;
      /*** case 25: return median_float25( ar ) ; ***/
   }

   /* general case */

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

/*-------------------------------------------------------------------------*/
/* find the modal value in a list of numbers
   mostly useful for integers, but not limiting here.
   doing this brute force for now */

float qmode_float( int n , float *ar )
{
   register int i , j ;           /* scanning indices */
   register float temp, mode ;
   register float *a = ar, *b;
   register int cnt, maxcnt ;

   /* no values, get out of here */
   if (n==0) return(0.0f);


   /* general case */
   maxcnt = 0; mode = -9999;
   for(i=0;i<n;i++) {
      temp = *a++;    /* value in list */
      if(temp==mode) continue;   /* same value as the mode already, skip to next */
      b = a;
      cnt = 1;   /* the value itself is the first time in the list */
      /* count how many times that number occurs in the rest of the list */
      for(j=i+1;j<n;j++){  /* only have to check remaining values*/
         if(temp==*b++)
            cnt++;
      }

      /* new mode value if value shows up more than the previous mode */
      if(cnt>maxcnt) {
         maxcnt = cnt;
         mode = temp;
      }
   }

   return(mode) ;
}

/*-------------------------------------------------------------------------*/
/* find the non-zero modal value in a list of numbers
   mostly useful for integers, but not limiting here.
   doing this brute force for now */

float qnzmode_float( int n , float *ar )
{
   register int i , j ;           /* scanning indices */
   register float temp, mode ;
   register float *a = ar, *b;
   register int cnt, maxcnt ;

   /* no values, get out of here */
   if (n==0) return(0.0f);

   /* general case */
   maxcnt = 0; mode = -9999;
   for(i=0;i<n;i++) {
      temp = *a++;    /* value in list */
      if((temp==mode)||(temp==0)) continue;   /* ignore zero or current mode value */
      b = a;
      cnt = 1;   /* the value itself is the first time in the list */
      /* count how many times that number occurs in the rest of the list */
      for(j=i+1;j<n;j++){  /* only have to check remaining values*/
         if(temp==*b++)
            cnt++;
      }

      /* new mode value if value shows up more than the previous mode */
      if(cnt>maxcnt) {
         maxcnt = cnt;
         mode = temp;
      }
   }

   /* if didn't find anything non-zero,return 0.0 */
   if(maxcnt==0) return(0.0f);
   /* normal case - return mode */
   return(mode) ;
}

/*---------------------------------------------------------------
  Return mean and sigma of a float array -- 07 Dec 2006
-----------------------------------------------------------------*/

void meansigma_float( int n, float *ar, float *mnn, float *sig )
{
   register double ss , sq , vv ;
   register int ii , nn ;

   if( n <= 0 || ar == NULL || (mnn == NULL && sig == NULL) ) return ;

   ss = sq = 0.0 ; nn = n ;
   for( ii=0 ; ii < nn ; ii++ ){
     vv = (double)ar[ii] ; ss += vv ; sq += vv*vv ;
   }
   ss /= nn ;
   if( nn > 1 ){
     sq = (sq - nn*ss*ss) / (nn-1.0) ; if( sq > 0.0 ) sq = sqrt(sq) ;
   } else {
     sq = 0.0 ;
   }

   if( mnn != NULL ) *mnn = (float)ss ;
   if( sig != NULL ) *sig = (float)sq ;
   return ;
}

/*---------------------------------------------------------------
  Return median and MAD, nondestructively -- 08 Mar 2001 - RWCox
-----------------------------------------------------------------*/

void qmedmad_float( int n, float *ar, float *med, float *mad )
{
   float me=0.0f , ma=0.0f , *q ;
   register int ii ;

   if( (med == NULL && mad == NULL) || n <= 0 || ar == NULL ) return ;

#pragma omp critical (MALLOC)
   q = (float *)malloc(sizeof(float)*n) ;  /* workspace */
   AAmemcpy(q,ar,sizeof(float)*n) ;  /* duplicate input array */
   me = qmed_float( n , q ) ;      /* compute median (partially sorts q) */

   if( mad != NULL && n > 1 ){
     for( ii=0 ; ii < n ; ii++ )   /* subtract off median */
       q[ii] = fabsf(q[ii]-me) ;   /* (absolute deviation) */
     ma = qmed_float( n , q ) ;    /* MAD = median absolute deviation */
   }

#pragma omp critical (MALLOC)
   free(q) ;                       /* 05 Nov 2001 - oopsie */

   if( med != NULL ) *med = me ;   /* 19 Aug 2005 - assign only */
   if( mad != NULL ) *mad = ma ;   /*               if not NULL */
   return ;
}

/*---------------------------------------------------------------*/
/* Return median, MAD, and 0.989*sqrt(biweight midvariance)
   (factor of 0.989 makes this = stdev for normal distribution).
*//*-------------------------------------------------------------*/

#undef  BC
#define BC 9.0f  /* cutoff for biweight midvariance */

void qmedmadbmv_float( int n, float *ar, float *med, float *mad, float *bmv )
{
   register int ii ;
   float lmed , lmad , nbmv , dbmv , xx , uu,u1 , fac ;

   if( n <= 0 || ar == NULL || (med==NULL && mad==NULL && bmv==NULL) ) return;

   if( med != NULL ) *med = 0.0f ; /* initialize outputs [24 Jun 2019] */
   if( mad != NULL ) *mad = 0.0f ;
   if( bmv != NULL ) *bmv = 0.0f ;

   qmedmad_float( n , ar , &lmed , &lmad ) ;
   if( med != NULL ) *med = lmed ;
   if( mad != NULL ) *mad = lmad ;
   if( bmv == NULL || lmad <= 0.0f ) return ;

   fac = 1.0f / ((BC*BC)*(lmad*lmad)) ;
   nbmv = dbmv = 0.0f ;
   for( ii=0 ; ii < n ; ii++ ){
     xx = ar[ii] - lmed ; xx = xx*xx ; uu = xx*fac ;
     if( uu < 1.0f ){
       u1 = 1.0f - uu ;
       nbmv += xx * (u1*u1)*(u1*u1) ;
       dbmv += u1 * (1.0f-5.0f*uu) ;
     }
   }
   if( dbmv != 0.0f ) dbmv = 0.989f * sqrtf(n*nbmv) / fabsf(dbmv) ;
   *bmv = dbmv ;
   return ;
}

/*---------------------------------------------------------------
  Return median and MAD and mean AD -- 11 Aug 2015 - RWCox
-----------------------------------------------------------------*/

void qmedmadmeanad_float( int n, float *ar, float *med, float *mad , float *meanad )
{
   float me=0.0f , ma=0.0f , md=0.0f , *q ;
   register int ii ;

   if( (med == NULL && mad == NULL && meanad == NULL ) || n <= 0 || ar == NULL ) return ;

   if( med    != NULL ) *med    = 0.0f ; /* initialize outputs [24 Jun 2019] */
   if( mad    != NULL ) *mad    = 0.0f ;
   if( meanad != NULL ) *meanad = 0.0f ;

#pragma omp critical (MALLOC)
   q = (float *)malloc(sizeof(float)*n) ;  /* workspace */
   AAmemcpy(q,ar,sizeof(float)*n) ;  /* duplicate input array */
   me = qmed_float( n , q ) ;      /* compute median (partially sorts q) */

   if( (mad != NULL || meanad != NULL) && n > 1 ){
     for( ii=0 ; ii < n ; ii++ ){   /* subtract off median */
       q[ii] = fabsf(q[ii]-me) ;   /* (absolute deviation) */
       md += q[ii] ;
     }
     md /= n ;
     if( mad != NULL ){
       ma = qmed_float( n , q ) ;    /* MAD = median absolute deviation */
     }
   }

#pragma omp critical (MALLOC)
   free(q) ;

   if( med    != NULL ) *med    = me ;
   if( mad    != NULL ) *mad    = ma ;
   if( meanad != NULL ) *meanad = md ;
   return ;
}

/*---------------------------------------------------------------*/
/* mean of central 50% (or so) of array;
   the actual number averaged is m = n-2*floor(n/4)
   -- edited 11 Aug 2021 by RWC to change m a little
   -- note that the sorting operation reorders array ar[],
      so if this is not good, you'll have to supply a copy
*//*-------------------------------------------------------------*/

float centromean_float( int n , float *ar )  /* 01 Nov 2010 */
{
   int ii ; float sum=0.0f ;
   int ibot , itop , m ;

   if( n <= 0 || ar == NULL ) return 0.0f ; /* bad inputs */

   qsort_float( n , ar ) ;
   ibot = n / 4 ;               /* bottom of sum range */
   itop = n - ibot - 1 ;        /* top = symmetric */
   for( ii=ibot ; ii <= itop ; ii++ ) sum += ar[ii] ;
   sum /= (itop-ibot+1) ;
   return sum ;
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
#define SORT2(a,b) if(a>b) SWAP(a,b)

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

/*--------------------------------------------------------------
 * median25(A) partitions the array A[0..24] such that element
 * A[12] is the median and subarrays A[0..11] and A[13..24]
 * are partitions containing elements of smaller and larger
 * value (rank), respectively.
 *
 * The exchange table lists element indices on the range 1..25,
 * this accounts for the "-1" offsets in the macro S2 and in
 * the final return value used to adjust subscripts to C-code
 * conventions (array indices begin at zero).
 *------- Code mildly adapted from Al Paeth's Median.c --------*
 *//*----------------------------------------------------------*/
#undef  S2
#define S2(i,j) if(A[i-1]>A[j-1]){temp=A[i-1];A[i-1]=A[j-1];A[j-1]=temp;}

static float median_float25( float *A )
{
  register float temp ;
  S2( 1, 2); S2( 4, 5); S2( 3, 5); S2( 3, 4); S2( 7, 8);
  S2( 6, 8); S2( 6, 7); S2(10,11); S2( 9,11); S2( 9,10);
  S2(13,14); S2(12,14); S2(12,13); S2(16,17); S2(15,17);
  S2(15,16); S2(19,20); S2(18,20); S2(18,19); S2(22,23);
  S2(21,23); S2(21,22); S2(24,25); S2( 3, 6); S2( 4, 7);
  S2( 1, 7); S2( 1, 4); S2( 5, 8); S2( 2, 8); S2( 2, 5);
  S2(12,15); S2( 9,15); S2( 9,12); S2(13,16); S2(10,16);
  S2(10,13); S2(14,17); S2(11,17); S2(11,14); S2(21,24);
  S2(18,24); S2(18,21); S2(22,25); S2(19,25); S2(19,22);
  S2(20,23); S2( 9,18); S2(10,19); S2( 1,19); S2( 1,10);
  S2(11,20); S2( 2,20); S2( 2,11); S2(12,21); S2( 3,21);
  S2( 3,12); S2(13,22); S2( 4,22); S2( 4,13); S2(14,23);
  S2( 5,23); S2( 5,14); S2(15,24); S2( 6,24); S2( 6,15);
  S2(16,25); S2( 7,25); S2( 7,16); S2( 8,17); S2( 8,20);
  S2(14,22); S2(16,24); S2( 8,14); S2( 8,16); S2( 2,10);
  S2( 4,12); S2( 6,18); S2(12,18); S2(10,18); S2( 5,11);
  S2( 7,13); S2( 8,15); S2( 5, 7); S2( 5, 8); S2(13,15);
  S2(11,15); S2( 7, 8); S2(11,13); S2( 7,11); S2( 7,18);
  S2(13,18); S2( 8,18); S2( 8,11); S2(13,19); S2( 8,13);
  S2(11,19); S2(13,21); S2(11,21); S2(11,13);
  return(A[13-1]);
}

/*************** General purpose sorty functions ZSS 06********************/

int compare_Z_IQSORT_DOUBLE (Z_QSORT_DOUBLE *a, Z_QSORT_DOUBLE *b )
{
   if (a->x < b->x)
      return (1);
   else if (a->x == b->x)
      return (0);
   else if (a->x > b->x)
      return (-1);
   /* this will never be reached but it will shut the compiler up */
   return (0);
}

int compare_Z_IQSORT_FLOAT (Z_QSORT_FLOAT *a, Z_QSORT_FLOAT *b )
{
   if (a->x < b->x)
      return (1);
   else if (a->x == b->x)
      return (0);
   else if (a->x > b->x)
      return (-1);
   /* this will never be reached but it will shut the compiler up */
   return (0);
}

int compare_Z_IQSORT_INT (Z_QSORT_INT *a, Z_QSORT_INT *b )
{
   if (a->x < b->x)
      return (1);
   else if (a->x == b->x)
      return (0);
   else if (a->x > b->x)
      return (-1);
   /* this will never be reached but it will shut the compiler up */
   return (0);
}

int compare_Z_QSORT_INT (Z_QSORT_INT *a, Z_QSORT_INT *b )
{
   if (a->x < b->x)
      return (-1);
   else if (a->x == b->x)
      return (0);
   else if (a->x > b->x)
      return (1);
   /* this will never be reached but it will shut the compiler up */
   return (0);
}

int compare_Z_IQSORT_STRING (Z_QSORT_STRING *a, Z_QSORT_STRING *b )
{
   if (!a->x && !b->x) return(0);
   if (!a->x) return(-1);
   if (!b->x) return(1);
   return (strcmp(a->x, b->x));
}

int compare_string (const void *a, const void *b )
{/* compare_string*/
   const char **ia = (const char **)a;
   const char **ib = (const char **)b;
   if (!*ia && !*ib) return(0);
   if (!*ia) return(-1);
   if (!*ib) return(1);
   return (strcmp(*ia, *ib));
}

int compare_double (double *a, double *b )
{/* compare_double*/
    if (*a < *b)
      return (-1);
   else if (*a == *b)
      return (0);
   else
      return (1);

}/* compare_double*/

int compare_float (float *a, float *b )
{/* compare_float*/
    if (*a < *b)
      return (-1);
   else if (*a == *b)
      return (0);
   else
      return (1);

}/* compare_float*/

int compare_int (int *a, int *b )
{/* compare_int*/
    if (*a < *b)
      return (-1);
   else if (*a == *b)
      return (0);
   else
      return (1);

}/* compare_int*/

int compare_short (short *a, short *b )
{/* compare_short*/
    if (*a < *b)
      return (-1);
   else if (*a == *b)
      return (0);
   else
      return (1);

}/* compare_short*/

int compare_char (char *a, char *b )
{/* compare_char*/
    if (*a < *b)
      return (-1);
   else if (*a == *b)
      return (0);
   else
      return (1);

}/* compare_char*/

int compare_byte (byte *a, byte *b )
{/* compare_byte*/
    if (*a < *b)
      return (-1);
   else if (*a == *b)
      return (0);
   else
      return (1);

}/* compare_byte*/

/*----------------------------------------------------------------------*/
/*!
   \brief find the percentiles of values in vec


   void *Percentate (void *vec, byte *mm, int nxyz,
                  int type, float *mpv, int N_mp,
                  int option, float *perc )
   \param vec (void *) pointer to nxyz values
   \param mm (byte *) pointer to byte mask (NULL to include everything)
   \param nxyz (int) number of values in vec and mm
   \param type (int) think DSET_BRICK_TYPE(dset,isb)
   \param mp (double*) vector of percentiles like [0.10 0.20 0.50 0.60]
   \param N_mp (int) number of percentiles in mp (4 for example above)
   \param option (int)  0: vec is not sorted, but will be (partially if mm is not all 1s)
                           when the function returns
                        1: vec is not sorted, but a copy will be made and returned
                        2: vec is sorted already
   \param perc (double *) percentile values in vec, should be big enough to contain
                          N_mp values
   \param zero_flag (int) if 1 then keep  zero values
   \param positive_flag (int) if 1 then keep positive ( > 0) values
   \param negative_flag (int) if 1 then keep negative ( < 0) values
   \return vvec (void *)   NULL in case of error.
                           If option is 0, vvec = vec so don't free vvec!
                           If option is 1, vvec is a sorted copy of vec, need to free it separately.
                           If option is 2, vvec = vec so don't free vvec!
   *****
   NOTE: For efficiency, sorting is done only for jth values where mm[j] != 0.
   ***** So when vec is sorted only the first mmvox values are usable, where mmvox is the number
         of voxels where mm[j] != 0. If your mask changes between calls, do not reuse a vec that
         had been (partially) sorted by this function.
         Also, keep in mind this if using the flags, the mask will differ from mm
*/

void *Percentate (void *vec, byte *mm, int nxyz,
                  int type, double *mpv, int N_mp,
                  int option, double *perc ,
                  int zero_flag, int positive_flag, int negative_flag)
{
   void *vvec=NULL;
   byte *bv=NULL;
   short *sv=NULL;
   int *iv=NULL;
   float *fv=NULL;
   double *dv=NULL;
   byte *mmf=NULL;
   int i,n, mmvox, doall=0 ;
   double di;

   ENTRY("Percentate");

   if (!perc || !N_mp) {
      ERROR_message("No return carrier or no values."); RETURN(NULL);
   }
   if (!vec || !nxyz) {
      ERROR_message("Null input or nxyz = 0."); RETURN(NULL);
   }
   if (option < 0 || option > 2) {
      ERROR_message("RTFM please"); RETURN(NULL);
   }

   /* prep input */
   if (option == 0 || option == 2) {
      vvec = vec;  /* use input vector directly (gets munged later) */
   } else {
      /* make copy */
      switch (type) {
         case MRI_byte:
#pragma omp critical (MALLOC)
            vvec = (void *)malloc(sizeof(byte)*nxyz);
            if (!vvec) {
               ERROR_message("Failed to allocate 1."); RETURN(NULL);
            }
            AAmemcpy(vvec, vec, nxyz*sizeof(byte));
            break;
         case MRI_short:
#pragma omp critical (MALLOC)
            vvec = (void *)malloc(sizeof(short)*nxyz);
            if (!vvec) {
               ERROR_message("Failed to allocate 2."); RETURN(NULL);
            }
            AAmemcpy(vvec, vec, nxyz*sizeof(short));
            break;
         case MRI_int:
#pragma omp critical (MALLOC)
            vvec = (void *)malloc(sizeof(int)*nxyz);
            if (!vvec) {
               ERROR_message("Failed to allocate 3."); RETURN(NULL);
            }
            AAmemcpy(vvec, vec, nxyz*sizeof(int));
            break;
         case MRI_float:
#pragma omp critical (MALLOC)
            vvec = (void *)malloc(sizeof(float)*nxyz);
            if (!vvec) {
               ERROR_message("Failed to allocate 4."); RETURN(NULL);
            }
            AAmemcpy(vvec, vec, nxyz*sizeof(float));
            break;
         case MRI_double:
#pragma omp critical (MALLOC)
            vvec = (void *)malloc(sizeof(double)*nxyz);
            if (!vvec) {
               ERROR_message("Failed to allocate 5."); RETURN(NULL);
            }
            AAmemcpy(vvec, vec, nxyz*sizeof(double));
            break;
         default:
            ERROR_message("Data type not supported."); RETURN(NULL);
      }
   }

   /* From now on, work with vvec */

   /* do the flag masking */

   if( zero_flag == 1 && positive_flag == 1 && negative_flag == 1 ){
     doall = 1 ; /* 17 Jul 2009: avoid malloc of mmf in this case */

   } else if (zero_flag == 1 || positive_flag == 1 || negative_flag == 1) {
#pragma omp critical (MALLOC)
      mmf = (byte *)calloc(nxyz, sizeof(byte));/* everything is rejected at first */
      if (!mmf) {
         ERROR_message("Failed to allocate for mmf -- O misery"); RETURN(NULL);
      }
   }

   if (zero_flag == 1 && positive_flag == 1 && negative_flag == 1) {
      doall = 1 ;  /* 17 Jul 2009 */

   } else if (positive_flag == 1 && negative_flag == 1) {
      switch (type) {
         case MRI_byte:
            bv = (byte *)vvec;
            for (i=0; i<nxyz; ++i) if (bv[i] != 0) { mmf[i] = 1; }
            break;
         case MRI_short:
            sv = (short *)vvec;
            for (i=0; i<nxyz; ++i) if (sv[i] != 0) { mmf[i] = 1; }
            break;
         case MRI_int:
            iv = (int *)vvec;
            for (i=0; i<nxyz; ++i) if (iv[i] != 0) { mmf[i] = 1; }
            break;
         case MRI_float:
            fv = (float *)vvec;
            for (i=0; i<nxyz; ++i) if (fv[i] != 0.0f) { mmf[i] = 1; }
            break;
         case MRI_double:
            dv = (double *)vvec;
            for (i=0; i<nxyz; ++i) if (dv[i] != 0.0f) { mmf[i] = 1; }
            break;
         default:
            ERROR_message("Data type not supported. Boo hooo hooo.");
            RETURN(NULL);
      }
   } else {
      /* do zeros ? */
      if (zero_flag == 1) {
         switch (type) {
            case MRI_byte:
               bv = (byte *)vvec;
               for (i=0; i<nxyz; ++i) if (bv[i] == 0) { mmf[i] = 1; }
               break;
            case MRI_short:
               sv = (short *)vvec;
               for (i=0; i<nxyz; ++i) if (sv[i] == 0) { mmf[i] = 1; }
               break;
            case MRI_int:
               iv = (int *)vvec;
               for (i=0; i<nxyz; ++i) if (iv[i] == 0) { mmf[i] = 1; }
               break;
            case MRI_float:
               fv = (float *)vvec;
               for (i=0; i<nxyz; ++i) if (fv[i] == 0.0f) { mmf[i] = 1; }
               break;
            case MRI_double:
               dv = (double *)vvec;
               for (i=0; i<nxyz; ++i) if (dv[i] == 0.0f) { mmf[i] = 1; }
               break;
            default:
               ERROR_message("Data type not supported. Boo hooo hooo.");
               RETURN(NULL);
         }
      }
      /* do positives ? */
      if (positive_flag == 1) {
         switch (type) {
            case MRI_byte:
               bv = (byte *)vvec;
               for (i=0; i<nxyz; ++i) if (bv[i] > 0) { mmf[i] = 1; }
               break;
            case MRI_short:
               sv = (short *)vvec;
               for (i=0; i<nxyz; ++i) if (sv[i] > 0) { mmf[i] = 1; }
               break;
            case MRI_int:
               iv = (int *)vvec;
               for (i=0; i<nxyz; ++i) if (iv[i] > 0) { mmf[i] = 1; }
               break;
            case MRI_float:
               fv = (float *)vvec;
               for (i=0; i<nxyz; ++i) if (fv[i] > 0.0f) { mmf[i] = 1; }
               break;
            case MRI_double:
               dv = (double *)vvec;
               for (i=0; i<nxyz; ++i) if (dv[i] > 0.0f) { mmf[i] = 1; }
               break;
            default:
               ERROR_message("Data type not supported. Boo hooo hooo.");
               RETURN(NULL);
         }
      }
      /* do negatives ? */
      if (negative_flag == 1) {
         switch (type) {
            case MRI_byte:
               WARNING_message("Not possible to have negative values with byte data.\nYou'll get zeros for that.");
               bv = (byte *)vvec;
               for (i=0; i<nxyz; ++i) { mmf[i] = 0; }
               break;
            case MRI_short:
               sv = (short *)vvec;
               for (i=0; i<nxyz; ++i) if (sv[i] < 0) { mmf[i] = 1; }
               break;
            case MRI_int:
               iv = (int *)vvec;
               for (i=0; i<nxyz; ++i) if (iv[i] < 0) { mmf[i] = 1; }
               break;
            case MRI_float:
               fv = (float *)vvec;
               for (i=0; i<nxyz; ++i) if (fv[i] < 0.0f) { mmf[i] = 1; }
               break;
            case MRI_double:
               dv = (double *)vvec;
               for (i=0; i<nxyz; ++i) if (dv[i] < 0.0f) { mmf[i] = 1; }
               break;
            default:
               ERROR_message("Data type not supported. Boo hooo hooo.");
               RETURN(NULL);
         }
      }
   }

   /* include mm mask, if needed */
   if (mm) {
      if (mmf) {
         for (i=0; i<nxyz; ++i) mmf[i] = mmf[i]*mm[i];
      } else {
         mmf = mm;
      }
      doall = 0 ;
   }

#pragma omp critical (MALLOC)
   { if( doall && mmf != NULL ){ free(mmf) ; mmf = NULL ; } } /* 17 Jul 2009 */

   /* mask vvec with final mask*/
   if (mmf) {
      mmvox = 0;
      switch (type) {
         case MRI_byte:
            bv = (byte*)vvec;
            for (i=0; i<nxyz; ++i) {
               if (mmf[i]) { bv[mmvox] = bv[i]; ++mmvox;}
            }
            break;
         case MRI_short:
            sv = (short*)vvec;
            for (i=0; i<nxyz; ++i) {
               if (mmf[i]) { sv[mmvox] = sv[i]; ++mmvox;}
            }
            break;
         case MRI_int:
            iv = (int*)vvec;
            for (i=0; i<nxyz; ++i) {
               if (mmf[i]) { iv[mmvox] = iv[i]; ++mmvox;}
            }
            break;
         case MRI_float:
            fv = (float*)vvec;
            for (i=0; i<nxyz; ++i) {
               if (mmf[i]) { fv[mmvox] = fv[i]; ++mmvox;}
            }
            break;
         case MRI_double:
            dv = (double *)vvec;
            for (i=0; i<nxyz; ++i) {
               if (mmf[i]) { dv[mmvox] = dv[i]; ++mmvox;}
            }
            break;
         default:
            ERROR_message("Bad type! Should not be here hon.");
            RETURN(NULL);
      }
#pragma omp critical (MALLOC)
      { if (mmf != mm){ free(mmf); mmf = NULL; } }
   } else {
      mmvox = nxyz;
   }

   if (option != 2) { /* partial sort of vvec */
      switch (type) {
         case MRI_byte:
            qsort(vvec, mmvox, sizeof(byte), (int(*) (const void *, const void *))compare_byte);
            break;
         case MRI_short:
#if 0
            qsort(vvec, mmvox, sizeof(short), (int(*) (const void *, const void *))compare_short);
#else
            qsort_short( mmvox , vvec ) ;  /* 13 Jul 2009 */
#endif
            break;
         case MRI_int:
            qsort(vvec, mmvox, sizeof(int), (int(*) (const void *, const void *))compare_int);
            break;
         case MRI_float:
#if 0
            qsort(vvec, mmvox, sizeof(float), (int(*) (const void *, const void *))compare_float);
#else
            qsort_float( mmvox , vvec ) ;  /* 13 Jul 2009 */
#endif
            break;
         case MRI_double:
            qsort(vvec, mmvox, sizeof(double), (int(*) (const void *, const void *))compare_double);
            break;
         default:
            ERROR_message("Bad type (%d). No support in this function", type);
            RETURN(NULL);
      }
   }

   #if 0
      switch (type) {
         case MRI_byte:
            fprintf(stderr,"bv:\n");
            for (n=0; n<mmvox; ++n) { fprintf(stderr,"%d ", bv[n]); }
            fprintf(stderr,"\n");
            break;
         case MRI_short:
            fprintf(stderr,"sv:\n");
            for (n=0; n<mmvox; ++n) { fprintf(stderr,"%d ", sv[n]); }
            fprintf(stderr,"\n");
            break;
         case MRI_int:
            fprintf(stderr,"iv:\n");
            for (n=0; n<mmvox; ++n) { fprintf(stderr,"%d ", iv[n]); }
            fprintf(stderr,"\n");
            break;
         case MRI_float:
            fprintf(stderr,"fv:\n");
            for (n=0; n<mmvox; ++n) { fprintf(stderr,"%f ", fv[n]); }
            fprintf(stderr,"\n");
            break;
         case MRI_double:
            fprintf(stderr,"dv:\n");
            for (n=0; n<mmvox; ++n) { fprintf(stderr,"%lf ", dv[n]); }
            fprintf(stderr,"\n");
            break;
         default:
            ERROR_message("Bad type! Should bot be here honhon.");
            RETURN(NULL);
      }
   #endif

   /* vvec is now sorted, now we're ready to get the percentile values */
   for (n=0; n<N_mp; ++n) {
      di = mpv[n]*mmvox -1;
      i = ( ( ((di) - (int)(di)) < 0.5f ) ? (int)(di) : ((int)(di)+1) );
      if (i<0) i = 0;
      else if (i >= mmvox) i = mmvox-1;
      switch (type) {
         case MRI_byte:
            bv = (byte *)vvec;
            perc[n] = (double)(bv[i]);
            break;
         case MRI_short:
            sv = (short *)vvec;
            perc[n] = (double)(sv[i]);
            break;
         case MRI_int:
            iv = (int *)vvec;
            perc[n] = (double)(iv[i]);
            break;
         case MRI_float:
            fv = (float *)vvec;
            perc[n] = (double)(fv[i]);
            break;
         case MRI_double:
            dv = (double *)vvec;
            perc[n] = (double)(dv[i]);
            break;
         default:
            ERROR_message("Bad type! Should not be here Joe.");
            RETURN(NULL);
      }
      /* fprintf(stderr,"mpv[%d] = %f, di = %f, i = %d, mmvox = %d\nperc[%d] = %f\n", n, mpv[n], di, i, mmvox, n, perc[n]); */
   }

   RETURN(vvec);
}

/*----------------------------------------------------------------------*/
/* Implementation not finished :( */

float wtmed_float( int n , float *x , float *w )
{
   float wsum,wth ; int ii ;

   if( n < 0 || x == NULL ) return 0.0f ;
   if( n == 1             ) return x[0] ;
   if( w == NULL          ) return qmed_float( n , x ) ;

   qsort_floatfloat( n , x , w ) ;

   wth = 0.0f ; for( ii=0 ; ii < n ; ii++ ) wth += w[ii] ;
   wth *= 0.5f ;

   wsum = 0.0f ;
   for( ii=0 ; ii < n && wsum < wth ; ii++,wsum+=w[ii] ) ; /*nada*/
   ii-- ;

   /* wsum = sum of weights up to and including final value of ii ;
      at this point, the sum of weights up to ii-1 is
       less than wth==wtot/2 and up to ii is greater than wtot/2;
      therefore, the weighted median is between x[ii-1] and x[ii]; */

   return 0.0f ;
}

/*------------------------------------------------------------------------*/

float qfrac_float( int n , float frac , float *ar )
{
   int i , j ;           /* scanning indices */
   float temp , pivot ;  /* holding places */
   float *a=ar ;
   float ni, fmid, ft,fb ;
   int left, right, mid ;

   /* special cases */

   if( n <= 0 ) return 0.0f ;
   if( n == 1 ) return a[0] ;

   ni = 1.0f/(float)n ;

   if( frac <= ni ){                    /* min */
     temp = a[0] ;
     for( i=1 ; i < n ; i++ ) if( temp > a[i] ) temp = a[i] ;
     return temp ;
   } else if( frac >= 1.0f-ni ){        /* max */
     temp = a[0] ;
     for( i=1 ; i < n ; i++ ) if( temp < a[i] ) temp = a[i] ;
     return temp ;
   } else if( fabsf(frac-0.5f) < ni ){  /* median */
     return qmed_float(n,a) ;
   }

   fmid = frac * n - 0.5f ;
    mid = (int)fmid ; if( mid >= n-1 ) mid = n-2 ;  /* shouldn't happen */
     ft = fmid-mid ;
     fb = 1.0f-ft ;

   if( n <= 21 ){        /* fast sorting for small arrays */
     qsort_float(n,a) ;
     return (fb*a[mid]+ft*a[mid+1]) ;
   }

   /* general (longer) case */

   left = 0 ; right = n-1 ;

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

      if( i == mid ) break ;      /* good luck == hit the target exactly */

      if( i <  mid ) left  = i ;  /* throw away bottom partition */
      else           right = i ;  /* throw away top partition    */

   }  /* end of while sub-array is long */

   /* find smallest element in the fraction above */

   temp = a[mid+1] ;
   for( j=mid+2 ; j < n ; j++ ) if( temp > a[j] ) temp = a[j] ;

#if 0
INFO_message("qfrac(%g): fmid=%g mid=%d a[mid]=%g temp=%g",frac,fmid,mid,a[mid],temp) ;
#endif

   return (fb*a[mid]+ft*temp) ;
}

/*-------------------------------------------------------------------------*/
/* The Mean Square Successive Difference
   J von Neumann, RH Kent, HR Bellinson and BI Hart.
   The Annals of Mathematical Statistics 12:153-162 (1941).
*//*-----------------------------------------------------------------------*/

float cs_mean_square_sd( int npt , float *x )
{
   float sum=0.0f , val ; int ii ;

   if( npt < 2 || x == NULL ) return sum ;

   for( ii=1 ; ii < npt ; ii++ ){
     val = x[ii] - x[ii-1] ; sum += val*val ;
   }
   return (sum/(npt-1.0f)) ;
}

/*--- similar thing, but median absolute value of successive differences ---*/

float cs_median_abs_sd( int npt , float *x , float *wks )
{
   float *dd=wks , val ; int ii ;

   if( npt < 2 || x == NULL ) return 0.0f ;

   if( dd == NULL ) dd = (float *)malloc(sizeof(float)*npt) ; /* local wks */

   for( ii=1 ; ii < npt ; ii++ ){
     dd[ii-1] = fabsf( x[ii] - x[ii-1] ) ;
   }

   val = qmed_float( npt-1 , dd ) ;
   if( dd != wks ) free(dd) ;
   return val ;
}
