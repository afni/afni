#include "thd_iochan.h"

int main( int argc , char * argv[] )
{
   char * buf ;
   int    nbuf , iarg=1 ;

   if( argc < 2 ){ fprintf(stderr,"Usage: myget [-1] URL > filename\n"); exit(0) ; }

   if( strcmp(argv[iarg],"-1") == 0 ){
     set_HTTP_10( 1 ) ; set_HTTP_user_agent( "myget" ) ; iarg++ ;
     if( iarg >= argc ) exit(1) ;
   }

   set_URL_progress(1) ;
   nbuf = read_URL( argv[iarg] , &buf ) ;
   if( nbuf <= 0 ){ fprintf(stderr,"Can't open URL %s\n",argv[iarg]) ; exit(1) ; }

   fwrite( buf , 1 , nbuf , stdout ) ;
   fflush(stdout) ;
   free(buf) ;
   fprintf(stderr,"Wrote out URL %s\n",argv[iarg]) ;
   exit(0) ;
}
