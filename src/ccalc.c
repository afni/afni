#include "parser.h"
#include <ctype.h>

int main( int argc , char * argv[] )
{
   PARSER_code * pcode ;
   char expr[200] , * cexp ;
   double atoz[26] , value ;
   int ii , kvar ;

   for( ii=0 ; ii < 25 ; ii++ ) atoz[ii] = 0.0 ;

   do{
      printf("calc> ") ; fflush(stdout) ;
      gets(expr) ;
      if( strlen(expr) == 0 ) continue ;
      if( strcmp(expr,"quit") == 0 ) exit(0) ;

      if( strstr(expr,"=") != NULL ){
         kvar = toupper(expr[0]) - 'A' ;
         cexp = strstr(expr,"=") + 1 ;
      } else {
         kvar = -1 ;
         cexp = expr ;
      }

      pcode = PARSER_generate_code( cexp ) ;
      if( pcode == NULL ){
         printf("parser error!\n") ; fflush(stdout) ;
         continue ;
      }

      value = PARSER_evaluate_one( pcode , atoz ) ; free(pcode) ;

      if( kvar >= 0 && kvar < 26 ){
        printf("%c", kvar+'A' ) ;
        atoz[kvar] = value ;
      } else {
        printf(" ") ;
      }
      printf(" = %g\n",value) ; fflush(stdout) ;
   } while(1) ;
}
