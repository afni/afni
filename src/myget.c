#include "thd_iochan.h"

int main( int argc , char * argv[] )
{
   char * buf ;
   int    nbuf ;

   if( argc < 2 ){ fprintf(stderr,"Usage: myget URL\n"); exit(0) ; }

   nbuf = read_URL( argv[1] , 5000 , &buf ) ;
   if( nbuf <= 0 ){ fprintf(stderr,"Can't open URL\n") ; exit(1) ; }

   fwrite( buf , 1 , nbuf , stdout ) ;
   fflush(stdout) ;
   free(buf) ;
   exit(0) ;
}
