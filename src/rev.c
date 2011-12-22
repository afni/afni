#include <stdio.h>
#include <string.h>

int main( int argc , char * argv )
{
   char buf[1024] , rrr[1024] ;
   char * b ;
   int ii , lb , jj ;

   do{
      b = fgets( buf , 1024 , stdin ) ;
      if( b == NULL ) exit(0) ;
      lb = strlen(b) ;
      if( lb <= 1 ){
         printf("\n") ;
      } else {
         for( ii=lb-2,jj=0 ; ii >= 0 ; ii--,jj++ ) rrr[jj] = buf[ii] ;
         rrr[jj] = '\0' ;
         printf("%s\n",rrr) ;
      }
   } while(1) ;
   exit(0) ;
}
