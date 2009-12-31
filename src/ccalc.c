/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/

#include "parser.h"
#include <ctype.h>
#include "cs.h"
#include <string.h>

#undef USE_READLINE
#ifdef USE_READLINE
#include "readline.h"
#endif

#ifdef SOLARIS
# define strcasestr strstr  /* stupid Solaris */
#endif

int main( int argc , char * argv[] )
{
   PARSER_code * pcode ;
   char expr[9000] , * cexp ;
   double atoz[26] , value ;
   int ii , kvar, kar, brk, strt, oform , len;
   int DoOnce;
   char *formatstr=NULL, *strptr=NULL;
   char ch;

   DoOnce = 0;

   kar = 1;
   brk = 0;
   DoOnce = 0; /* flag used to indicate that program is running in batch or command line modes */
   expr[0] = '\0';
   oform = CCALC_DOUBLE; /* double is default */
   while (kar < argc) {
      if (strcmp(argv[1],"-help") == 0 ){
         printf(
"Usage: ccalc [-form FORM] [-eval 'expr']\n"
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
 "                %%n.mf: custom format string, used as in printf.\n"
 "                   format string can contain %%%%, \\n and other\n"
 "                   regular characters.\n"
 "                   See man fprintf and man printf for details.\n"
 "                You can also replace:\n"
 "                   -form int    with    -i\n"
 "                   -form nice   with    -n\n"
 "                   -form double with    -d\n"
 "                   -form fint   with    -f\n" /* no float are evoked */
 "                   -form cint   with    -c\n"
 "    Mandatory parameter: (must come last on command line)\n"
 "    -eval EXPR: EXPR is the expression to evaluate.\n"
 "                Example: ccalc -eval '3 + 5 * sin(22)' \n"
 "                     or: ccalc -eval 3 +5 '*' 'sin(22)'\n"
 "                You can not use variables in EXPR\n"
 "                as you do with 3dcalc.\n"
 "    Example with formatting:\n"
 "        ccalc -form '********\\n%%6.4f%%%%\\n********' -eval '100*328/457'\n"
 "    gives:\n"
 "        ********\n"
 "        0.7177%%\n"
 "        ********\n"
 "    Try also:\n"
 "        ccalc -i 3.6\n"
 "        ccalc -f 3.6\n"
 "        ccalc -c 3.6\n"
 "        ccalc -form '%%3.5d' 3.3\n"
 "        ccalc -form '**%%5d**' 3.3\n"
 "        ccalc -form '**%%-5d**' 3.3\n"
 "\n"
 " ** SECRET: You don't need to use -eval if you are \n"
 "            not using any other options. I hate typing\n"
 "            it for quick command line calculations. \n"
 "            But that feature might be removed in the\n"
 "            future, so always use -eval when you are \n"
 "            using this program in your scripts.\n"
                ) ;
         exit(0) ;
      }

      if ( !brk && strcmp(argv[kar],"-form") == 0 ) {
         ++kar;
         if (kar >= argc) ERROR_exit("need argument after -form ");
         if (strcmp(argv[kar],"double") == 0 ) oform = CCALC_DOUBLE;
         else if (strcmp(argv[kar],"nice") == 0 ) oform = CCALC_NICE;
         else if (strcmp(argv[kar],"int") == 0 ) oform = CCALC_INT;
         else if (strcmp(argv[kar],"rint") == 0 ) oform = CCALC_INT;
         else if (strcmp(argv[kar],"fint") == 0 ) oform = CCALC_FINT;
         else if (strcmp(argv[kar],"cint") == 0 ) oform = CCALC_CINT;
         else if (strlen(argv[kar])<=256) {
            oform = CCALC_CUSTOM;
            formatstr = argv[kar];
         }
         else {
            fprintf (stderr, "Format type '%s' not supported.\nSee -help for details.\n", argv[kar]);
            exit (1);
         }
         DoOnce = 1;
         brk = 1;
      }

      if (!brk && strncmp(argv[kar],"-d",2) == 0 ) {
         oform = CCALC_DOUBLE;
         brk = 1; DoOnce = 1;
      }
      if (!brk && strncmp(argv[kar],"-n",2) == 0 ) {
         oform = CCALC_NICE;
         brk = 1; DoOnce = 1;
      }
      if (!brk && strncmp(argv[kar],"-i",2) == 0 ) {
         oform = CCALC_INT;
         brk = 1; DoOnce = 1;
      }
      if (!brk && strncmp(argv[kar],"-r",2) == 0 ) {
         oform = CCALC_INT;
         brk = 1; DoOnce = 1;
      }
      if (!brk && strncmp(argv[kar],"-f",2) == 0 ) {
         oform = CCALC_FINT;
         brk = 1; DoOnce = 1;
      }
      if (!brk && strncmp(argv[kar],"-c",2) == 0 ) {
         oform = CCALC_CINT;
         brk = 1; DoOnce = 1;
      }
      if( !brk &&  ( strcmp(argv[kar],"-eval") == 0 || strcmp(argv[kar],"-expr") == 0) ){
         ++kar;
         if (kar >= argc) ERROR_exit("need argument after -eval (or -expr) ");
         /* anything after eval gets put into an expression */
         while (kar < argc)  {
            if (  strcmp(argv[kar],"-eval") == 0 ||
                  strcmp(argv[kar],"-expr") == 0 ||
                  strcmp(argv[kar],"-form") == 0   ) {
               fprintf (stderr,  "Error:\n"
                                 "You have optional parameters (%s) following \n"
                                 "the expression to evaluate.\n"
                                 "Stop it!\n", argv[kar]);
               exit (1);
            }
            sprintf(expr,"%s %s", expr, argv[kar]);
            ++ kar;
         }
         /* fprintf (stdout, "%s\n", expr);*/
         DoOnce = 1;
         brk = 1;
      }

      if (!brk) {
         /* if nothing is understood, assume the expression follows, instead of moaning and quitting*/
         while (kar < argc)  {
            if (  strcmp(argv[kar],"-eval") == 0 ||
                  strcmp(argv[kar],"-expr") == 0 ||
                  strcmp(argv[kar],"-form") == 0   ) {
               fprintf (stderr,  "Error:\n"
                                 "You have optional parameters (%s) following \n"
                                 "the expression to evaluate.\n"
                                 "Stop it!\n", argv[kar]);
               exit (1);
            }
            sprintf(expr,"%s %s", expr, argv[kar]);
            ++ kar;
         }
         /* fprintf (stdout, "%s\n", expr);*/
         DoOnce = 1;
         brk = 1;
      }

      if (!brk) {
        ERROR_exit("Option %s not understood. Try -help for usage\n", argv[kar]);
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
         if( fgets(expr,900,stdin) == NULL ) {   /* quit on ctrl-D */
            putchar('\n') ;
            exit(0) ;
         }
#endif
      }

      if( strlen(expr) == 0 || *expr == '\n' ) continue ;
      if( strcasestr(expr,"quit") != NULL ) exit(0) ;
      if( strcasestr(expr,"exit") != NULL ) exit(0) ;
      if( strcasestr(expr,"help") != NULL || expr[0] == '?' ){
        printf(PARSER_HELP_STRING) ; continue ;  /* 22 Jan 2008 */
      }

      if( strstr(expr,"=") != NULL ){
         kvar = toupper(expr[0]) - 'A' ;
         cexp = strstr(expr,"=") + 1 ;
      } else {
         kvar = 25 ;    /* assign result to letter Z */
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
         printf("%s\n", format_value_4print(value, oform, formatstr ));
         exit (0);
      }
   } while(1) ;
}
