#include "mrilib.h"

#define ERR (fprintf(stderr,"\aerror\n"),exit(1))

int main( int argc , char * argv[] )
{
   byte *b64 , *bin;
   int  nb64 , nbin ;
   double cpu , clk ;

   if( argc < 3 ) ERR ;

   if( argv[1][0] == 'd' ){
     clk = COX_clock_time() ;
     b64 = AFNI_suck_file( argv[2] ) ;
     clk = COX_clock_time() - clk ;
     if( b64 == NULL ) ERR ;
     nb64 = strlen(b64) ; if( nb64 < 4 ) ERR ;

     cpu = COX_cpu_time() ;
     B64_to_binary( nb64,b64 , &nbin,&bin ) ;
     cpu = COX_cpu_time() - cpu ;
     fprintf(stderr,"nbin=%d nb64=%d cpu=%g I/O=%g\n",nbin,nb64,cpu,clk) ;
     if( nbin == 0 ) ERR ;
     fwrite( bin,1,nbin,stdout ) ; exit(0) ;
   } else if( argv[1][0] == 'e' ){
      B64_set_crlf(1) ;
      B64_set_linelen( 72 ) ;
      clk = COX_clock_time() ;
      bin = AFNI_suck_file( argv[2] ) ;
      clk = COX_clock_time() - clk ;
      if( bin == NULL ) ERR ;
      nbin = THD_filesize( argv[2] ) ;
      if( nbin <= 0 ) ERR ;
      cpu = COX_cpu_time() ;
      B64_to_base64( nbin,bin , &nb64,&b64 ) ;
      cpu = COX_cpu_time() - cpu ;
      fprintf(stderr,"nb64=%d nbin=%d cpu=%g I/O=%g\n",nb64,nbin,cpu,clk) ;
      if( nb64 == 0 ) ERR ;
      fwrite( b64,1,nb64,stdout ) ; exit(0) ;
   } else {
      ERR ;
   }
}
