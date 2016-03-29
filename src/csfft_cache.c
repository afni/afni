#ifdef SPARKY
#undef _POSIX_SOURCE
#endif
#include <sys/types.h>
#include <sys/time.h>
#include <sys/errno.h>
#include <sys/times.h>
#include <limits.h>

#include "mrilib.h"

#define FFT_LEN  128
#define FFT_POW  7
#define FFT_NUM  100000
#define MAX_NVEC 256

double COX_cpu_time(void) ;

int main( int argc , char * argv[] )
{
   int nvec , kk , ii , nvec_now , itop , iarg , fft_len,fft_num,max_nvec ;
   complex *cx ;
   float fac ;
   double cptime , mflops , fft_pow,fft_ops ;

   if( argc > 1 && strncmp(argv[1],"-help",5) == 0 ){
      printf("Usage: csfft_cache [-len #] [-num #] [-maxvec #]\n"
             "* Does lots of FFTs and determines how many at once are efficient.\n"
             "Options:\n"
             "  -len #     sets FFT length to '#' (power of 2); default = 128\n"
             "  -num #     number of FFTs to do for timing;  default = 100000\n"
             "  -maxvec #  maximum number of FFTs to do at once; default = 256\n"
            ) ;
      exit(0) ;
   }

   fft_len  = FFT_LEN ;
   fft_pow  = FFT_POW ;
   fft_num  = FFT_NUM ;
   max_nvec = MAX_NVEC ;
   iarg = 1 ;
   while( iarg < argc && argv[iarg][0] == '-' ){
      if( strncmp(argv[iarg],"-len",4) == 0 ){
         fft_len = strtol( argv[++iarg] , NULL , 10 ) ;
         if( fft_len < 4 ){fprintf(stderr,"illegal -len\a\n");exit(1);}
         kk = csfft_nextup(fft_len) ;
         if( kk != fft_len ){fprintf(stderr,"illegal -len\a\n");exit(1);}
         fft_pow = log2((double)kk) ;
         iarg++ ; continue ;
      }

      if( strncmp(argv[iarg],"-num",4) == 0 ){
         fft_num = strtol( argv[++iarg] , NULL , 10 ) ;
         if( fft_num < 1 ){fprintf(stderr,"illegal -num\a\n");exit(1);}
         iarg++ ; continue ;
      }

      if( strncmp(argv[iarg],"-maxvec",5) == 0 ){
         max_nvec = strtol( argv[++iarg] , NULL , 10 ) ;
         if( max_nvec < 2 ){fprintf(stderr,"illegal -maxvec\a\n");exit(1);}
         iarg++ ; continue ;
      }
   }
   fft_ops = (int)(5.0*fft_pow+2.0)*fft_len ;  /* only valid for powers of 2! */
   fac = sqrt(1.0/fft_len) ;
  
   cx = (complex *) malloc( sizeof(complex) * fft_len * max_nvec ) ;
   for( ii=0 ; ii < fft_len ; ii++ ){ cx[ii].r = ii ; cx[ii].i = -0.3*ii ; }

   csfft_cox( -1 , fft_len , cx ) ;  /* to initialize trig constants */

   for( nvec=1 ; nvec <= max_nvec ; nvec = (nvec<4) ? (nvec+1) : (1.414*nvec+0.5) ){
      for( ii=0 ; ii < fft_len*nvec ; ii++ ){ cx[ii].r = ii ; cx[ii].i = -0.3*ii ; }

      cptime = COX_cpu_time() ;
#if 0
      if( nvec == 1 ){
#else
      if( nvec == 0 ){
#endif
         for( kk=0 ; kk < fft_num ; kk++ ){
            csfft_cox( -1 , fft_len , cx ) ;
            for( ii=0 ; ii < fft_len ; ii++ ){ cx[ii].r *= fac ; cx[ii].i *= fac ; }
         }
      } else {
         int nvec_now ;
         for( kk=0 ; kk < fft_num ; kk+=nvec ){
            nvec_now = fft_num - kk ; if( nvec_now > nvec ) nvec_now = nvec ;
            csfft_many( -1 , fft_len , nvec_now , cx ) ;
            itop = fft_len*nvec_now ;
            for( ii=0 ; ii < itop ; ii++ ){ cx[ii].r *= fac ; cx[ii].i *= fac ; }
         }
      }
      cptime = COX_cpu_time() - cptime ;
      mflops = (double)fft_num * fft_ops / ( 1.e6 * cptime ) ;
      printf("FFT cache size %6lu bytes gives %10.2f Mflops"
             " (%d FFTs, size %d, %g ops, %d at a time, in %g sec)\n",
             sizeof(complex)*fft_len*nvec , mflops , fft_num,fft_len,fft_ops,nvec,cptime) ;
   }

   exit(0) ;
}

#if 0
double COX_cpu_time(void)  /* in seconds */
{
   struct tms ttt ;
   (void) times( &ttt ) ;
   return (  (double) (ttt.tms_utime) / (double) CLK_TCK ) ;
}
#endif
