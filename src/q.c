#include <stdio.h>
#include <string.h>
#include <ctype.h>

#define IS_STRING_CHAR(c) ( isgraph(c) && !isspace(c) &&  \
                            (c) != '>' && (c) != '/'  &&  \
                            (c) != '='                  )

int main( int argc , char *argv[] )
{
   int ii,ll ;

   if( argc < 2 ) exit(0) ;

   ll = strlen(argv[1]) ;
   for( ii=0 ; ii < ll ; ii++ )
      printf("%c %d\n",argv[1][ii],IS_STRING_CHAR(argv[1][ii])) ;
   exit(0) ;
}
