#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int main( int argc , char * argv[] )
{
   int mm,ii ;
   char * cc ;

   if( argc < 2 || strcmp(argv[1],"-help")==0 ){
      printf("Usage: mbig n\n"
             "Will allocate n Megabytes of memory, then quit.\n") ;
      exit(0) ;
   }

   mm = strtol( argv[1] , NULL , 10 ) ;
   if( mm <= 0 ) exit(1) ;

   cc = (char *)malloc( mm*1024*1024 * sizeof(char) ) ;
   printf("Malloc-ed %d Megabytes",mm) ; fflush(stdout) ;
   for( ii=0 ; ii < mm*1024*1024 ; ii++ ) cc[ii] = (char) (ii%128) ;

   for( ii=0 ; ii < 3 ; ii++ ){
      sleep(1) ; printf(".") ; fflush(stdout) ;
   }
   printf("\n") ; exit(0) ;
}
