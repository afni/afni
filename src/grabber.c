#include <stdio.h>
#include <stdlib.h>

#define MEG (1 << 20)

int main( int argc , char * argv[] )
{
   int ii , jj ;
   char * qqq ;

   for( ii=1 ; ; ii++ ){
      fprintf(stderr," %d",ii) ;
      qqq = (char *) malloc( sizeof(char) * MEG ) ;
      for( jj=0 ; jj < MEG ; jj++ ) qqq[jj] = 0 ;
   }
   exit(0) ;
}
