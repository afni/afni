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
   char expr[900] , * cexp ;
   double atoz[26] , value ;
   int ii , kvar, kar, brk ;
   int DoOnce;
   
   DoOnce = 0;
   
   kar = 1;
   brk = 0;
   DoOnce = 0; /* flag used to indicate that program is running in batch or command line modes */ 
   expr[0] = '\0';
   
   while (kar < argc) { 
      if (strcmp(argv[1],"-help") == 0 ){
         printf("Usage: ccalc [-eval <expr>]\n"
                "With no command line parameters:\n"
                "Interactive numerical calculator, using the same\n"
                "expression syntax as 3dcalc.  Mostly for playing.\n" 
                "With -eval <expr> option:\n"
                "Calculates expr and quits. \n"
                "Do not use variables in expr.\n"
                "Example: ccalc -eval '3 + 5 * sin(22)' \n"
                "or: ccalc -eval 3 +5 '*' 'sin(22)'\n") ;
         exit(0) ;
      }
      
      if (!brk && strcmp(argv[1],"-eval") == 0) {
         ++kar;
         if (kar >= argc)  {
		  		fprintf (stderr, "need argument after -eval ");
				exit (1);
			}
         /* anything after eval gets put inot an expression */
         while (kar < argc)  {
            sprintf(expr,"%s %s", expr, argv[kar]); 
            ++ kar;
         }
         /* fprintf (stdout, "%s\n", expr);*/
         DoOnce = 1;
         brk = 1;
      }
 		
      if (!brk) {
			fprintf (stderr,"Error: Option %s not understood. Try -help for usage\n", argv[kar]);
			exit (1);
		} else {	
			brk = 0;
			kar ++;
		}
		
   }
   
   for( ii=0 ; ii < 25 ; ii++ ) atoz[ii] = 0.0 ;

   do{
      if (!DoOnce){
         printf("calc> ") ; fflush(stdout) ;
         fgets(expr,900,stdin) ;
      }
      
      if( strlen(expr) == 0 ) continue ;
      if( strstr(expr,"quit") != NULL ) exit(0) ;

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
         if (!DoOnce) continue ; 
            else exit(1);
      }

#if 0
      if( PARSER_has_symbol( "I" , pcode ) )
         printf("  [contains symbol I]\n") ;
#endif

      value = PARSER_evaluate_one( pcode , atoz ) ; free(pcode) ;

      if (!DoOnce) {
         if( kvar >= 0 && kvar < 26 ){
           printf("%c", kvar+'A' ) ;
           atoz[kvar] = value ;
         } else {
           printf(" ") ;
         }
         printf(" = %g\n",value) ; fflush(stdout) ;
      } else {
         printf("%g\n",value) ; 
         exit (0);
      }
   } while(1) ;
}
