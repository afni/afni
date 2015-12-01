#ifdef USE_OMP
#include <omp.h>
#endif
#include "mrilib.h"

#include "csfft_OMP.c"

#define NFFT 100000

int main( int argc , char *argv[] )
{
  int nn , ii , kk ;
  complex *xcold , *xcnew ;
  float sum , max , maxrel , val , xmax ;
  int        imax , imaxrel , nfft ;
  double cpu1 ;

  if( argc < 2 ){
    printf("Usage: %s N\n",argv[0]) ; exit(0) ;
  }

  nn = (int)strtod(argv[1],NULL) ;
  if( nn < 2 ) exit(0) ;

  nfft = csfft_nextup(nn) ;
  if( nfft != nn ){
    INFO_message("bumped FFT len to %d",nfft) ; nn = nfft ;
  }

  AFNI_SETUP_OMP(0) ;
#ifdef USE_OMP
#pragma omp parallel
 {
  if( omp_get_thread_num() == 0 )
    INFO_message("OpenMP thread count = %d",omp_get_num_threads()) ;
 }
#endif

  xcold = (complex *)malloc(sizeof(complex)*nn) ;
  xcnew = (complex *)malloc(sizeof(complex)*nn) ;

  for( ii=0 ; ii < nn ; ii++ ){
    xcnew[ii].r = xcold[ii].r = ii%7  - 3.5f ;
    xcnew[ii].i = xcold[ii].i = ii%11 - 5.5f ;
  }

  INFO_message("csfft_cox") ;
  csfft_cox    ( -1 , nn , xcold ) ;
  INFO_message("csfft_cox_OMP_SETUP") ;
  csfft_cox_OMP_SETUP() ;
  INFO_message("csfft_cox_OMP") ;
  csfft_cox_OMP( -1 , nn , xcnew ) ;

  sum = max = maxrel = xmax = 0.0f ; imax = imaxrel = -1 ;
  for( ii=0 ; ii < nn ; ii++ ){
    val =  fabsf( xcnew[ii].r - xcold[ii].r ) ;
          +fabsf( xcnew[ii].i - xcold[ii].i ) ;
    sum += val ;
    if( max < val ){
      max = val ; imax = ii ;
      maxrel = val / ( fabsf(xcnew[ii].r) + fabsf(xcnew[ii].i) + 1.e-9f ) ;
    }
    val = sqrtf(xcnew[ii].r*xcnew[ii].r+xcnew[ii].i*xcnew[ii].i) ;
    if( xmax < val ) xmax = val ;
  }
  sum /= nn ;
  INFO_message("averr=%g   maxerr[%d]=%g  maxrel=%g  maxFFT=%g",sum,max,imax,maxrel,xmax) ;

  nfft = NFFT ;
  if( nn < 128 ) nfft *= 10 ;

  cpu1 = COX_cpu_time() ;
  for( kk=0 ; kk < nfft ; kk++ ){
    for( ii=0 ; ii < nn ; ii++ ){
     xcold[ii].r = ii%7  - 3.5f ;
     xcold[ii].i = ii%11 - 5.5f ;
    }
   csfft_cox( -1 , nn , xcold ) ;
  }
  INFO_message("%d csfft     = %.3g CPU sec",nfft,COX_cpu_time()-cpu1) ;

  cpu1 = COX_cpu_time() ;
  for( kk=0 ; kk < nfft ; kk++ ){
    for( ii=0 ; ii < nn ; ii++ ){
     xcold[ii].r = ii%7  - 3.5f ;
     xcold[ii].i = ii%11 - 5.5f ;
    }
   csfft_cox_OMP( -1 , nn , xcold ) ;
  }
  INFO_message("%d csfft_OMP = %.3g CPU sec",nfft,COX_cpu_time()-cpu1) ;

  exit(0) ;
}
