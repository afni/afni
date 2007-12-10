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
   double atoz[26] , value , del=1.0 , dzero=0.0 ;
   int ii,jj , kvar , nopt , qvar , num=-1 , verbose=0 ;
   MRI_IMAGE * inim[26] ;
   float     * inar[26] ;
   MRI_IMAGE *dindex_im = NULL;
   float     *dindex = NULL;
   char abet[] = "abcdefghijklmnopqrstuvwxyz" ;

   /*-- help? --*/

   if( argc < 3 ){
      printf("Usage: 1deval [options] -expr 'expression'\n"
             "Evaluates an expression that may include columns of data\n"
             "from one or more text files and writes the result to stdout.\n\n"
             "* Any single letter from a-z can be used as the independent\n"
             "   variable in the expression. Only a single column can be\n"
             "   used for each variable.\n"
             "* Unless specified using the '[]' notation (cf. 1dplot -help),\n"
             "   only the first column of an input 1D file is used, and other\n"
             "   columns are ignored.\n"
             "* Only one column of output will be produced -- if you want to\n"
             "   calculate a multi-column output file, you'll have to run 1deval\n"
             "   separately for each column, and then glue the results together\n"
             "   using program 1dcat.\n"
             "\n"
             "Options:\n"
             "  -del d   = Use 'd' as the step for a single undetermined variable\n"
             "               in the expression [default = 1.0]\n"
             "  -start z = Start at value 'z' for a single undetermined variable\n"
             "               in the expression [default = 0.0]\n"
             "  -num n   = Evaluate the expression 'n' times.\n"
             "               If -num is not used, then the length of an\n"
             "               input time series is used.  If there are no\n"
             "               time series input, then -num is required.\n"
             "  -a q.1D  = Read time series file q.1D and assign it\n"
             "               to the symbol 'a' (as in 3dcalc).\n"
             "  -index i.1D = Read index column from file i.1D and\n"
             "                 write it out as 1st column of output.\n"
             "                 This option is useful when working with\n"
             "                 surface data.\n"
             "Examples:\n"
             " 1deval -expr 'sin(2*PI*t)' -del 0.01 -num 101 > sin.1D\n"
             " 1deval -expr 'a*b' -a fred.1D -b ethel.1D > ab.1D\n"
             " 1deval -start 10 -num 90 -expr 'fift_p2t(0.001,n,2*n)' | 1dplot -xzero 10 -stdin\n"
             " 1deval -x '1D: 1 4 9 16' -expr 'sqrt(x)'\n"
             "\n"
             "* Program 3dcalc operates on 3D and 3D+time datasets in a similar way.\n"
             "* Program ccalc can be used to evaluate a single numeric expression.\n"
             "* For generic 1D file usage help, see '1dplot -help'\n"
             "* For help with expression format, see '3dcalc -help'\n"
             "* If I had any sense, this program would have been called 1dcalc!\n"
             "  -- RW Cox\n"
            ) ;
      exit(0) ;
   }

   machdep() ;

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

      if( strcmp(argv[nopt],"-start") == 0 ){  /* 29 Nov 2007 */
         nopt++ ;
         if( nopt >= argc ){
            fprintf(stderr,"** -start needs an argument!\n") ;
            exit(1) ;
         }
         dzero = strtod( argv[nopt] , NULL ) ;
         if( verbose ) fprintf(stderr,"start set to %g\n",dzero) ;
         nopt++ ; continue ;
      }

      if( strcmp(argv[nopt],"-index") == 0 ){
         nopt++ ;
         if( nopt >= argc ){
            fprintf(stderr,"** -index needs an argument!\n") ;
            exit(1) ;
         }

         dindex_im = mri_read_1D( argv[nopt] ) ;
         if( dindex_im == NULL ){
            fprintf(stderr,"** Can't read time series file %s\n",argv[nopt]);
            exit(1) ;
         }
         if (dindex_im->ny != 1) {
            fprintf(stderr,"** Only one column allowed for indexing.\n   Found %d columns\n", dindex_im->ny);
            exit(1) ;
         }
         dindex = MRI_FLOAT_PTR(dindex_im) ;
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

   if (dindex) {
      if ( dindex_im->nx != num) {
         fprintf(stderr,"** Number of values in index column (%d) \n   not equal to number of values in data (%d)\n", dindex_im->nx, num );
         exit(1) ;
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
      if( kvar >= 0 ) atoz[kvar] = ii * del + dzero ;
      value = PARSER_evaluate_one( pcode , atoz ) ;
      if (dindex) printf(" %d\t%g\n", (int)dindex[ii], value) ;
      else printf(" %g\n",value) ;
   }
   exit(0) ;
}
