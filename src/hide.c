#include <stdio.h>
#include <string.h>

int main( int argc , char * argv[] )
{
   int add , nkey , ikey ;
   unsigned char * key ;
   int chin , chout ;

   if( argc != 3 || strncmp(argv[1],"-help",5) == 0 ){
      fprintf(stderr,"Usage: hide {+|-} key < input > output\n") ;
      exit(-1) ;
   }

   add = (argv[1][0] == '+') ;
   key = (unsigned char *) argv[2] ; nkey = strlen(argv[2]) ;
   if( nkey < 1 ){ fprintf(stderr,"Illegal key\n") ; exit(-1) ; }

   ikey = 0 ;
   do{
      chin = getchar() ; if( chin == EOF ) break ;
      if( add ) chout = (chin + key[ikey++]      ) % 256 ;
      else      chout = (chin - key[ikey++] + 256) % 256 ;
      putchar(chout) ;
      ikey = ikey % nkey ;
   } while(1) ;

   exit(0) ;
}
