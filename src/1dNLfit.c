#include "mrilib.h"
#include "parser.h"

static int nparse_err = 0 ;

static double atoz[26] ;

/*-------------------------------------------------------------------------*/

static float_pair parse_parameter( char *parg )
{
   PARSER_code *pcode ;
   char *expr , *cpt ;
   float_pair fp = {0.0f,0.0f} ;

                     cpt = strchr(parg,':') ;
   if( cpt == NULL ) cpt = strchr(parg,';') ;
   if( cpt == NULL ){
     pcode = PARSER_generate_code(parg) ;
     if( pcode == NULL ){
       nparse_err++ ;
     } else {
       fp.a = fp.b = PARSER_evaluate_one( pcode , atoz ) ;
       free(pcode) ;
     }
   } else {
     char *ex1 , *ex2 ;
     ex1 = strdup(parg) ;
                       cpt = strchr(ex1,':') ;
     if( cpt == NULL ) cpt = strchr(ex1,';') ;
     *cpt = '\0' ;
     pcode = PARSER_generate_code(ex1) ;
     if( pcode == NULL ){
       nparse_err++ ;
     } else {
       fp.a = PARSER_evaluate_one( pcode , atoz ) ;
       free(pcode) ;
     }
     pcode = PARSER_generate_code(cpt+1) ;
     if( pcode == NULL ){
       nparse_err++ ;
     } else {
       fp.b = PARSER_evaluate_one( pcode , atoz ) ;
       free(pcode) ;
     }
   }
   return fp ;
}

/*-------------------------------------------------------------------------*/

void print_1dNLfit_help(void)
{
   printf("\n"
    "Program to fit a model to a vector of data.  The model is given by a\n"
    "symbolic expression, with parameters to be estimated.\n"
    "\n"
    "Usage: 1dNLfit OPTIONS\n"
    "\n"
    "Options: [all but '-meth' are actually mandatory]\n"
    "--------\n"
    "\n"
    " -expr eee   = The expression for the fit.  It must contain one symbol from\n"
    "               'a' to 'z' which is marked as the independent variable by\n"
    "               option '-indvar', and at least one more symbol which is\n"
    "               a parameter to be estimated.\n"
    "\n"
    " -indvar c d = Indicates which variable in '-expr' is the independent\n"
    "               variable.  All other symbols are parameters, which are\n"
    "               either fixed (constants) or variables to be estimated.\n"
    "               Then, read the values of the independent variable from\n"
    "               1D file 'd' (first column only)\n"
    "               ++ If the independent variable has a constant step size,\n"
    "                  you can input it via with 'd' replaced by a string like\n"
    "                    '1D: 100%0:2.1'\n"
    "                  which creates an array with 100 value, starting at 0,\n"
    "                  then adding 2.1 for each step:\n"
    "                    0 2.1 4.2 6.3 8.4 ...\n"
    "\n"
    " -param ppp  = Set fixed value or estimating range for a particular\n"
    "               symbol.\n"
    "               ++ For a fixed value, 'ppp' takes the form 'a=3.14', where the\n"
    "                  first letter is the symbol name, which must be followed by\n"
    "                  an '=', then followed by a constant expression.  This\n"
    "                  expression can be symbolic, as in 'a=cbrt(3)'.\n"
    "               ++ For a parameter to be estimated, 'ppp' takes the form of\n"
    "                  two constant expressions separated by a ':', as in\n"
    "                  'q=-sqrt(2):sqrt(2)'.\n"
    "               ++ All symbols in '-expr' must have a corresponding '-param'\n"
    "                 option, except for the '-indvar' symbol.\n"
    "\n"
    " -depdata v  = Read the values of the dependent variable (to be fitted to\n"
    "               '-expr') from 1D file 'v'.\n"
    "               ++ File 'v' must have the same number of rows as file 'd'\n"
    "                  from the '-indvar' option!\n"
    "\n"
    " -meth m     = Set the method for fitting: '1' for L1, '2' for L2.\n"
    "               (The default method is L2.)\n"
    "\n"
   ) ;
   exit(0) ;
}

/*-------------------------------------------------------------------------*/

int main( int argc , char *argv[] )
{
   int nopt=1 ;
   char *expr=NULL ; PARSER_code *pcode ;
   char cind='\0' ; int jind=-1 , meth=2 , jj,kk ;
   MRI_IMAGE *indvar_im , *depvar_im ;;
   int  nfree=0   , nfix=0 ;
   char cfree[26] , cfix[26] ;
   float vbot[26] , vtop[26] ;

   if( argc < 5 || strcasecmp(argv[1],"-help") == 0 )
     print_1dNLfit_help() ;

   PARSER_set_printout(1) ;
   for( jj=0 ; jj < 26 ; jj++ ) atoz[jj] = vbot[jj] = vtop[jj] = 0.0 ;

   /*--- scan option ---*/

   while( nopt < argc ){

     /*---*/

     if( strcasecmp(argv[nopt],"-meth") == 0 ){
       if( ++nopt >= argc )
         ERROR_exit("Need argument after '%s'",argv[nopt-1]) ;
       meth = (int)strtod(argv[nopt],NULL) ;
       if( meth != 1 ) meth = 2 ;
       nopt++ ; continue ;
     }

     /*---*/

     if( strcasecmp(argv[nopt],"-expr") == 0 ){
       if( expr != NULL )
         ERROR_exit("Can't use '%s' more than once",argv[nopt]) ;
       if( ++nopt >= argc )
         ERROR_exit("Need argument after '%s'",argv[nopt-1]) ;
       expr = strdup(argv[nopt]) ;
       pcode = PARSER_generate_code(expr) ;
       if( pcode == NULL ) ERROR_exit("Bad -expr :-(") ;
       nopt++ ; continue ;
     }

     /*---*/

     if( strcasecmp(argv[nopt],"-indvar") == 0 ){
       if( jind >= 0 )
         ERROR_exit("Can't use '%s' more than once",argv[nopt]) ;
       if( ++nopt >= argc-1 )
         ERROR_exit("Need 2 arguments after '%s'",argv[nopt-1]) ;
       cind = toupper(argv[nopt][0]) ;
       jind = cind - 'A' ;
       if( jind < 0 || jind >= 26 )
         ERROR_exit("Symbol after '-indvar' is invalid (not in a..z or A..Z)") ;
       nopt++ ;
       indvar_im = mri_read_1D(argv[nopt]) ;
       if( indvar_im == NULL )
         ERROR_exit("Can't read 1D file after '-indvar' ('%s')\n",argv[nopt]) ;
       nopt++ ; continue ;
     }

     /*---*/

     if( strcasecmp(argv[nopt],"-depdata") == 0 ){
       if( depvar_im != NULL )
         ERROR_exit("Can't use '%s' more than once",argv[nopt]) ;
       if( ++nopt >= argc )
         ERROR_exit("Need an argument after '%s'",argv[nopt-1]) ;
       depvar_im = mri_read_1D(argv[nopt]) ;
       if( depvar_im == NULL )
         ERROR_exit("Can't read 1D file after '-depdata' ('%s')\n",argv[nopt]) ;
       nopt++ ; continue ;
     }

     /*---*/

     if( strcasecmp(argv[nopt],"-param") == 0 ){
       int jpar , npe ; char cpar ; float_pair fp ;
       if( ++nopt >= argc )
         ERROR_exit("Need an argument after '%s'",argv[nopt-1]) ;
       nopt++ ; continue ;
       cpar = toupper(argv[nopt][0]) ;
       jpar = cpar - 'A' ;
       if( jpar < 0 || jpar >= 26 )
         ERROR_exit("Symbol '%c' after '-param' is invalid (not in a..z or A..Z)",argv[nopt][0]) ;
       for( jj=0 ; jj < nfix ; jj++ ){
         if( cpar == cfix[jj] )
           ERROR_exit("'-param %s': Symbol '%c' is already marked as a fixed parameter",argv[nopt],cpar) ;
       }
       for( jj=0 ; jj < nfree ; jj++ ){
         if( cpar == cfree[jj] )
           ERROR_exit("'-param %s': Symbol '%c' is already marked as a variable parameter",argv[nopt],cpar) ;
       }
       if( argv[nopt][1] != '=' )
         ERROR_exit("'-param %s' does not have '=' as second character",argv[nopt]) ;
       npe = nparse_err ;
       fp = parse_parameter( argv[nopt]+2 ) ;
       if( nparse_err > npe )
         ERROR_exit("Can't parse expressions in '-param %s'",argv[nopt]) ;
       atoz[jpar] = 0.5f * ( fp.a + fp.b ) ;
       vbot[jpar] = fp.a ;
       vtop[jpar] = fp.b ;
       if( fp.a >= fp.b ) cfix [nfix++]  = cpar ;
       else               cfree[nfree++] = cpar ;
       nopt++ ; continue ;
     }

     /*---*/

     ERROR_exit("Unknown option '%s'",argv[nopt]) ;
   }

   /*--- check for errors ---*/

   if( pcode == NULL )
     ERROR_exit("No -expr option?!?") ;

   if( depvar_im == NULL )
     ERROR_exit("No -depdata option?!") ;

   if( indvar_im == NULL )
     ERROR_exit("No -indvar option?!") ;

   if( nfree == 0 )
     ERROR_exit("No -param option with a range of allowed values?!") ;

   for( jj=0 ; jj < nfix ; jj++ ){
     if( cind == cfix[jj] )
       ERROR_exit("Independent variable '%' is also marked as a fixed parameter!",cind) ;
   }
   for( jj=0 ; jj < nfree ; jj++ ){
     if( cind == cfree[jj] )
       ERROR_exit("Independent variable '%' is also marked as a variable parameter (to estimate)!",cind) ;
   }
}
