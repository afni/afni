/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/

#include "parser.h"
#include <ctype.h>
#include <stdlib.h>
#include "mrilib.h"

int main( int argc , char * argv[] )
{
   PARSER_code * pcode = NULL ;
   char sym[4] ;
   double atoz[26] , value , del=1.0 ;
   int ii,jj , kvar , nopt , qvar , num=-1 , verbose=0 ;
   MRI_IMAGE * inim[26] ;
   float     * inar[26] ;
   char abet[] = "abcdefghijklmnopqrstuvwxyz" ;

   /*-- help? --*/

   if( argc < 3 ){
      printf("Usage: 1deval [options] -expr 'expression'\n"
             "Evaluates the expression and writes the result to stdout.\n"
             "Any single letter from a-z can be used as the independent\n"
             "variable in the expression.\n"
             "\n"
             "Options:\n"
             "  -del d   = Use 'd' as the step for the variable in the\n"
             "               expression [default = 1.0]\n"
             "  -num n   = Evaluate the expression 'n' times.\n"
             "               If -num is not used, then the length of an\n"
             "               input time series is used.  If there are no\n"
             "               time series input, then -num is required.\n"
             "  -a q.1D  = Read time series file q.1D and assign it\n"
             "               to the symbol 'a' (as in 3dcalc).\n"
             "Examples:\n"
             "  1deval -expr 'sin(2*PI*t)' -del 0.01 -num 101 > sin.1D\n"
             "  1deval -expr 'a*b*x' -a fred.1D -b ethel.1D > x.1D\n"
            ) ;
      exit(0) ;
   }

   /*-- initialize --*/

   for( ii=0 ; ii < 26 ; ii++ ){
      atoz[ii] = 0.0 ; inim[ii] = NULL ; inar[ii] = NULL ;
   }

   /*-- read options --*/

   nopt = 1 ;
   while( nopt < argc ){

      if( strcmp(argv[nopt],"-verb") == 0 ){
         verbose++ ;
         nopt++ ; continue ;
      }

      if( strlen(argv[nopt]) == 2   && argv[nopt][0] == '-' &&
          argv[nopt][1]      >= 'a' && argv[nopt][1] <= 'z'   ){

         int ival = argv[nopt][1] - 'a' ;

         if( inim[ival] != NULL ){
            fprintf(stderr,"** Can't define symbol %c twice!\n",argv[nopt][1]);
            exit(1) ;
         }

         nopt++ ;
         if( nopt >= argc ){
            fprintf(stderr,"** -%c needs an argument!\n",argv[nopt][1]); exit(1);
         }

         inim[ival] = mri_read_1D( argv[nopt] ) ;
         if( inim[ival] == NULL ){
            fprintf(stderr,"** Can't read time series file %s\n",argv[nopt]);
            exit(1) ;
         }

         inar[ival] = MRI_FLOAT_PTR(inim[ival]) ;
         nopt++ ; continue ;
      }

      if( strcmp(argv[nopt],"-expr") == 0 ){
         if( pcode != NULL ){
            fprintf(stderr,"** Can't have 2 -expr options!\n") ;
            exit(1) ;
         }
         nopt++ ;
         if( nopt >= argc ){
            fprintf(stderr,"** -expr needs an argument!\n") ; exit(1) ;
         }
         pcode = PARSER_generate_code( argv[nopt] ) ;  /* compile */
         if( pcode == NULL ){
            fprintf(stderr,"** Illegal expression!\n") ;
            exit(1) ;
         }
         nopt++ ; continue ;
      }

      if( strcmp(argv[nopt],"-del") == 0 ){
         nopt++ ;
         if( nopt >= argc ){
            fprintf(stderr,"** -del needs an argument!\n") ;
            exit(1) ;
         }
         del = strtod( argv[nopt] , NULL ) ;
         if( del == 0 ){
            fprintf(stderr,"** -del value must not be zero!\n") ;
            exit(1) ;
         }
         if( verbose ) fprintf(stderr,"del set to %g\n",del) ;
         nopt++ ; continue ;
      }

      if( strcmp(argv[nopt],"-num") == 0 ){
         nopt++ ;
         if( nopt >= argc ){
            fprintf(stderr,"** -num needs an argument!\n") ;
            exit(1) ;
         }
         num = strtol( argv[nopt] , NULL , 10 ) ;
         if( num <= 0 ){
            fprintf(stderr,"** -num value must be positive!\n") ;
            exit(1) ;
         }
         nopt++ ; continue ;
      }

      fprintf(stderr,"** %s = unknown command line option!\n",argv[nopt]) ;
      exit(1) ;
   }

   if( num <= 0 ){
      for( ii=0 ; ii < 26 ; ii++ ){
         if( inim[ii] != NULL ){ num = inim[ii]->nx ; break ; }
      }
      if( num > 0 ){
         if( verbose )
            fprintf(stderr,"++ Set num = %d from input time series\n",num);
      } else {
         fprintf(stderr,"** Need to supply -num on command line!\n"); exit(1);
      }
   }

   for( qvar=ii=0 ; ii < 26 ; ii++ ){
      if( inim[ii] != NULL && inim[ii]->nx < num ){
         fprintf(stderr,"** Time series file %s is too short!\n",inim[ii]->name);
         qvar++ ;
      }
   }
   if( qvar > 0 ) exit(1) ;

   if( pcode == NULL ){
      fprintf(stderr,"** -expr is missing!\n") ; exit(1) ;
   }

   qvar = 0 ; kvar = -1 ;                       /* find symbol */
   for( ii=0 ; ii < 26 ; ii++ ){
      sym[0] = 'A' + ii ; sym[1] = '\0' ;
      if( PARSER_has_symbol(sym,pcode) ){
         if( inim[ii] == NULL ){
            qvar++ ; if( kvar < 0 ) kvar = ii ;
         }
      } else if( inim[ii] != NULL ){
         fprintf(stderr,"++ Symbol %c defined but not used!\n",abet[ii]) ;
      }
   }
   if( qvar > 1 ){
      fprintf(stderr,"++ Found %d indeterminate symbols in expression,\n"
                     "++   but there should only be 0 or 1.\n"
                     "++   Will use symbol %c as the variable.\n" ,
              qvar , abet[kvar] ) ;
   }

   /*-- evaluate --*/

   for( ii=0 ; ii < num ; ii++ ){
      for( jj=0 ; jj < 26 ; jj++ )
         if( inar[jj] != NULL ) atoz[jj] = inar[jj][ii] ;
      if( kvar >= 0 ) atoz[kvar] = ii * del ;
      value = PARSER_evaluate_one( pcode , atoz ) ;
      printf(" %g\n",value) ;
   }
   exit(0) ;
}
