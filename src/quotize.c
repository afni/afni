#include <stdio.h>

#define NBUF 256

int main( int argc , char * argv[] )
{
   char buf[NBUF] , buf2[NBUF*2];
   int ll , num=0 , ii , jj ;

   if( argc < 2 ){
      fprintf(stderr,"Usage: quotize name < input > output\n"
                     "Turns a text file into a C array of strings\n"
                     "initialized into an array 'char * name[]'.\n"
             ) ;
      exit(0) ;
   }

   printf("static char * %s[] = {\n",argv[1]) ;
   while( fgets(buf,NBUF,stdin) != NULL ){
      ll = strlen(buf) ; if( ll == 0 ) break ;
      if( buf[ll-1] == '\n' ) buf[ll-1] = '\0' ;
      for( ii=0,jj=0 ; buf[ii] != '\0' ; ){
         if( buf[ii] == '"' ) buf2[jj++] = '\\' ;
         buf2[jj++] = buf[ii++] ;
      }
      buf2[jj] = '\0' ;
      printf("   \"%s\\n\" ,\n" , buf2 ) ;
      num++ ;
   }
   printf("   NULL } ;\n") ;
   printf("#define NUM_%s %d\n",argv[1],num) ;
   exit(0) ;
}
