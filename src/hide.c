#include <stdio.h>
#include <string.h>

int main( int argc , char * argv[] )
{
   int nkey , ikey ;
   unsigned char *key ;
   int chin , chout ;

   if( argc < 2 || strncmp(argv[1],"-help",5) == 0 ){
      fprintf(stderr,"Usage: hide key < input > output\n") ;
      exit(1) ;
   }

   key = (unsigned char *) argv[1] ; nkey = strlen(argv[1]) ;
   if( nkey < 1 ){ fprintf(stderr,"Illegal key\n") ; exit(1) ; }

   ikey = 0 ;
   do{
      chin = getchar() ; if( chin == EOF ) break ;
      chout = ((unsigned char)chin) ^ key[ikey++] ;
      putchar(chout) ;
      ikey = ikey % nkey ;
   } while(1) ;

   exit(0) ;
}
