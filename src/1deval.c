#include "parser.h"
#include <ctype.h>

int main( int argc , char * argv[] )
{
   PARSER_code * pcode = NULL ;
   char sym[4] ;
   double atoz[26] , value , del=1.0 ;
   int ii , kvar , nopt , qvar , num=100 ;

   /*-- help? --*/

   if( argc < 3 ){
      printf("Usage: 1dcalc -expr 'expression' [-del d] [-num n]\n"
             "Evaluates the expression at 'n' points, spaced 'd'\n"
             "apart, and writes the result to stdout.\n" ) ;
      exit(0) ;
   }

   /*-- initialize --*/

   for( ii=0 ; ii < 26 ; ii++ ) atoz[ii] = 0.0 ;

   /*-- read options --*/

   nopt = 1 ;
   while( nopt < argc ){

      if( strcmp(argv[nopt],"-expr") == 0 ){
         if( pcode != NULL ){
            fprintf(stderr,"*** Can't have 2 -expr options!\n") ;
            exit(1) ;
         }
         nopt++ ;
         if( nopt >= argc ){
            fprintf(stderr,"*** -expr needs an argument!\n") ;
            exit(1) ;
         }
         pcode = PARSER_generate_code( argv[nopt] ) ;  /* compile */
         if( pcode == NULL ){
            fprintf(stderr,"*** Illegal expression!\n") ;
            exit(1) ;
         }

         qvar = 0 ; kvar = -1 ;                       /* find symbol */
         for( ii=0 ; ii < 26 ; ii++ ){
            sym[0] = 'A' + ii ; sym[1] = '\0' ;
            if( PARSER_has_symbol(sym,pcode) ){
               qvar++ ; if( kvar < 0 ) kvar = ii ;
            }
         }
         if( qvar != 1 ){
            fprintf(stderr,"*** Expression should have just one symbol!\n") ;
            exit(1) ;
         }

         nopt++ ; continue ;
      }

      if( strcmp(argv[nopt],"-del") == 0 ){
         nopt++ ;
         if( nopt >= argc ){
            fprintf(stderr,"*** -del needs an argument!\n") ;
            exit(1) ;
         }
         del = strtod( argv[nopt] , NULL ) ;
         if( del == 0 ){
            fprintf(stderr,"*** -del value must not be zero!\n") ;
            exit(1) ;
         }
         nopt++ ; continue ;
      }

      if( strcmp(argv[nopt],"-num") == 0 ){
         nopt++ ;
         if( nopt >= argc ){
            fprintf(stderr,"*** -num needs an argument!\n") ;
            exit(1) ;
         }
         num = strtol( argv[nopt] , NULL , 10 ) ;
         if( num < 1 ){
            fprintf(stderr,"*** -num value must be positive!\n") ;
            exit(1) ;
         }
         nopt++ ; continue ;
      }

      fprintf(stderr,"*** %s = unknown command line option!\n") ;
      exit(1) ;
   }

   if( pcode == NULL ){
      fprintf(stderr,"*** -expr is missing!\n") ; exit(1) ;
   }

   /*-- evaluate --*/

   for( ii=0 ; ii < num ; ii++ ){
      atoz[kvar] = ii * del ;
      value = PARSER_evaluate_one( pcode , atoz ) ;
      printf(" %g\n",value) ;
   }
   exit(0) ;
}
