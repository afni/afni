/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/

#include "parser.h"
#include <ctype.h>

int main( int argc , char * argv[] )
{
   PARSER_code * pcode ;
   char expr[200] , * cexp ;
   double atoz[26] , value ;
   int ii , kvar ;

   if( argc > 1 && strcmp(argv[1],"-help") == 0 ){
      printf("Usage: ccalc\n"
             "Interactive numerical calculator, using the same\n"
             "expression syntax as 3dcalc.  Mostly for playing.\n" ) ;
      exit(0) ;
   }

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

#if 0
      if( PARSER_has_symbol( "I" , pcode ) )
         printf("  [contains symbol I]\n") ;
#endif

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
