/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/

#include "parser.h"
#include <ctype.h>

#undef USE_READLINE
#ifdef USE_READLINE
#include "readline.h"
#endif
typedef enum { CCALC_DOUBLE = 1, CCALC_NICE, CCALC_INT, CCALC_FINT, CCALC_CINT};
int main( int argc , char * argv[] )
{
   PARSER_code * pcode ;
   char expr[900] , * cexp ;
   double atoz[26] , value ;
   int ii , kvar, kar, brk, strt, oform ;
   int DoOnce;

   DoOnce = 0;

   kar = 1;
   brk = 0;
   DoOnce = 0; /* flag used to indicate that program is running in batch or command line modes */
   expr[0] = '\0';
   oform = CCALC_DOUBLE; /* double is default */
   while (kar < argc) {
      if (strcmp(argv[1],"-help") == 0 ){
         printf("Usage: ccalc [-form FORM] [-eval 'expr']\n"
                "Usage mode 1: Interactive numerical calculator\n"
                "    Interactive numerical calculator, using the \n"
                "    same expression syntax as 3dcalc. \n"
                "    No command line parameters are permitted in\n"
                "    usage 1 mode.\n"
                "Usage mode 2: Command line expression calculator\n"
                "    Evaluate an expression specified on command\n"
                "    line, return answer and quit.\n"
                "    Optional parameters: (must come first)\n"
                "    -form FORM: Format output in a nice form\n"
                "                Choose from:\n"
                "                double: Macho numbers (default).\n"
                "                nice: Metrosexual output.\n"
                "                int (or rint): Rounded to nearest integer.\n"
                "                cint: Rounded up.\n"
                "                fint: Rounded down.\n"
                "    Mandatory parameter: (must come last on command line)\n"
                "    -eval EXPR: EXPR is the expression to evaluate.\n" 
                "                Example: ccalc -eval '3 + 5 * sin(22)' \n"
                "                     or: ccalc -eval 3 +5 '*' 'sin(22)'\n"
                "                You can't not use variables in EXPR\n"
                "                like you do with 3dcalc.\n"
                ) ;
         exit(0) ;
      }

      if ( !brk && strcmp(argv[kar],"-form") == 0 ) {
         ++kar;
         if (kar >= argc)  {
	         fprintf (stderr, "need argument after -form ");
	         exit (1);
         }
         if (strcmp(argv[kar],"double") == 0 ) oform = CCALC_DOUBLE;
         else if (strcmp(argv[kar],"nice") == 0 ) oform = CCALC_NICE;
         else if (strcmp(argv[kar],"int") == 0 ) oform = CCALC_INT;
         else if (strcmp(argv[kar],"rint") == 0 ) oform = CCALC_INT;
         else if (strcmp(argv[kar],"fint") == 0 ) oform = CCALC_FINT;
         else if (strcmp(argv[kar],"cint") == 0 ) oform = CCALC_CINT;
         else {
            fprintf (stderr, "Format type '%s' not supported.\nSee -help for details.\n", argv[kar]);
            exit (1);
         }
         DoOnce = 1;
         brk = 1;
      }
      if( !brk &&  ( strcmp(argv[kar],"-eval") == 0 || strcmp(argv[kar],"-expr") == 0) ){
         ++kar;
         if (kar >= argc)  {
	         fprintf (stderr, "need argument after -eval (or -expr) ");
	         exit (1);
         }
         /* anything after eval gets put into an expression */
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
#ifdef USE_READLINE
         { char *lin = readline("ccalc> ") ;
           if(  lin == NULL ) continue ;
           if( *lin == '\0' ){ free(lin); continue; }
           add_history(lin) ;
           strncpy(expr,lin,899); expr[899]='\0'; free(lin);
         }
#else
         printf("calc> ") ; fflush(stdout) ;
         fgets(expr,900,stdin) ;
#endif
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
         switch (oform) {
            case CCALC_DOUBLE: /* double */
               printf("%f\n",value) ;
               break;
            case CCALC_NICE:
               printf("%g\n",value) ;
               break;
            case CCALC_INT:
               if ( (value - (int)value) < 0.5) value = (int)value;
               else value = (int)value + 1;
               printf("%d\n",(int)value) ;
               break;
            case CCALC_FINT:
               printf("%d\n",(int)value) ;
               break;
            case CCALC_CINT:
               printf("%d\n",(int)ceil(value)) ;
               break;
            default:
               printf("%f\n",value) ;
               break; 
         }
         exit (0);
      }
   } while(1) ;
}
