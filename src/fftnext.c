#include "mrilib.h"

int main(int argc,char * argv[])
{
   int ii , jj , bot=1,top=20000 ;

   if( argc > 1 ){
      if( argv[1][0] == '-' ){printf("Usage: fftnext [bot [top]]\n");exit(0);}
      bot = strtol(argv[1],NULL,10) ;
      if( bot < 1 ) bot = 1 ;
      if( argc > 2 ){
         top = strtol(argv[2],NULL,10) ;
         if( top < bot ) top = bot ;
      }
   }
   ii = bot ;
   do{
      jj = csfft_nextup(ii) ;
      printf(" %d",jj) ; fflush(stdout) ;
      ii = jj+1 ;
   } while( ii < top ) ;
   printf("\n") ; exit(0) ;
}
