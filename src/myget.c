#include "thd_iochan.h"

int main( int argc , char * argv[] )
{
   char * buf ;
   int    nbuf ;

   if( argc < 2 ){ fprintf(stderr,"Usage: myget URL > filename\n"); exit(0) ; }

   nbuf = read_URL( argv[1] , &buf ) ;
   if( nbuf <= 0 ){ fprintf(stderr,"Can't open URL %s\n",argv[1]) ; exit(1) ; }

   fwrite( buf , 1 , nbuf , stdout ) ;
   fflush(stdout) ;
   free(buf) ;
   fprintf(stderr,"Wrote out URL %s\n",argv[1]) ;
   exit(0) ;
}
