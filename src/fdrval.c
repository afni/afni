#include "mrilib.h"

int main( int argc , char *argv[] )
{
   int iarg , ival , do_pval=0 , do_qinput=0 ;
   THD_3dim_dataset *dset ;
   floatvec *fv ;
   float val , zval , qval=0.0f , pval=0.0f ;

   if( argc < 4 || strcmp(argv[1],"-help") == 0 ){
     printf(
       "Usage: fdrval [options] dset sub val [val ...]\n"
       "\n"
       "Reads FDR curve data from the header of dset for sub-brick\n"
       "#sub and computes the q-value when the sub-brick statistical\n"
       "threshold is set to val.\n"
       "\n"
       "OPTIONS\n"
       "-------\n"
       " -pval   = also output the p-value (on the same line, after q)\n"
       " -ponly  = don't output q-values, just p-values\n"
       " -qonly  = don't output p-values, just q-values [the default]\n"
       "\n"
       " -qinput = The 'val' inputs are taken to be q-values and then the\n"
       "  *OR*     outputs are the corresponding statistical thresholds.\n"
       " -inverse  This is the inverse of the usual operation.\n"
       "           * With this option, all 'val' inputs must be between 0 and 1\n"
       "             (exclusive), or bad things will happen and the program will\n"
       "             send e-mail to your mother explaining how stupid you are.\n"
       "           * You cannot use '-ponly' or '-pval' with this option.\n"
       "           * For example, if you do\n"
       "               fdrval dset+orig 1 1.2\n"
       "             and get a q-value of 0.234, then\n"
       "               fdrval -qinput dset+orig 1 0.234\n"
       "             should return the value 1.2 -- the original threshold.\n"
       "             (There may be a small discrepancy, due to the differences)\n"
       "             (between forward interpolation and inverse interpolation.)\n"
       "           * To set a (csh) variable to use in a script for thresholding\n"
       "             via 3dcalc, you could do something like\n"
       "               set tval = `fdrval -qinput dset+orig 1 0.05`\n"
       "               3dcalc -expr \"step(a-$tval)\" -a dset+orig'[1]' -prefix dmask\n"
       "\n"
       "NOTES\n"
       "-----\n"
       "* Output for each 'val' is written to stdout.\n"
       "* If the q-value can't be computed, then 1.0 will be output.\n"
       "* If you input an absurdly high threshold, you will get the smallest\n"
       "    q-value stored in the dataset header. (This is not necessarily exactly\n"
       "    the smallest q-value that was computed originally, due to the way the\n"
       "    FDR curves are calculated and interpolated.)\n"
       "* If you use '-qinput' and input a q-value that is too small for the\n"
       "    FDR curve in the dataset header, you will get a threshold at or above\n"
       "    the largest value in that sub-brick.\n"
       "* Sample usage:\n"
       "      fdrval Fred_REML+orig 0 `count -scale 0.1 10 20` | 1dplot -stdin\n"
       "    Uses the 'count' program to input a sequence of values, and then\n"
       "    pipes into the 1dplot program to make a graph of F vs. q.\n"
       "* See the link below for information on how AFNI computes FDR curves:\n"
       "      https://afni.nimh.nih.gov/pub/dist/doc/misc/FDR/FDR_Jan2008.pdf\n"
       "* Also see the output of '3dFDR -help'\n"
       "\n"
       "-- A quick hack by RWCox -- 15 Oct 2008 -- PG Wodehouse's birthday!\n"
       "-- Quick re-hack to add '-qinput' option -- 20 Dec 2011 -- RWCox\n"
       "-- Re-re-hack to make super-small '-qinput' values work right -- 14 Mar 2014\n"
     ) ;
     PRINT_COMPILE_DATE ; exit(0) ;
   }

   mainENTRY("fdrval") ; machdep() ;

   iarg = 1 ;

   /* check for options */

   while( iarg < argc && argv[iarg][0] == '-' ){

     if( strcasecmp(argv[iarg],"-qinput")  == 0 ||
         strcasecmp(argv[iarg],"-inverse") == 0   ){  /* 20 Dec 2011 */
       do_qinput = 1 ; iarg++ ; continue ;
     }

     if( strcasecmp(argv[iarg],"-pval") == 0 ){
       do_pval = 1 ; iarg++ ; continue ;
     }
     if( strcasecmp(argv[iarg],"-ponly") == 0 ){
       do_pval = 2 ; iarg++ ; continue ;
     }
     if( strcasecmp(argv[iarg],"-qonly") == 0 ){
       do_pval = 0 ; iarg++ ; continue ;
     }

     ERROR_exit("Unknown option '%s'",argv[iarg]) ;
   }

   if( do_qinput ) do_pval = 0 ;

   /* read the required 'dset' and 'sub' arguments */

   if( iarg >= argc ) ERROR_exit("No 'dset' argument?!") ;
   dset = THD_open_dataset( argv[iarg] ) ;
   CHECK_OPEN_ERROR(dset,argv[iarg]) ;
   iarg++ ;

   if( iarg >= argc ) ERROR_exit("No 'sub' argument?!") ;
   ival = (int)strtod(argv[iarg],NULL) ;
   if( ival < 0 || ival >= DSET_NVALS(dset) )
     ERROR_exit("fdrval dataset '%s' doesn't have sub-brick #%d",
                argv[iarg-1],ival) ;

   /* check if there is an FDR curve for this sub-brick */

   if( do_pval != 2 ){
     fv = DSET_BRICK_FDRCURVE(dset,ival) ;
     if( fv == NULL )
       ERROR_exit("fdrval dataset '%s[%d]' doesn't have FDR curve",
                  argv[iarg-1],ival) ;
   }
   if( do_pval != 0 ){
     if( !FUNC_IS_STAT(DSET_BRICK_STATCODE(dset,ival)) ){
       WARNING_message("fdrval dataset '%s[%d]' doesn't have statistic codes",
                       argv[iarg-1],ival) ;
       if( do_pval == 2 ) ERROR_exit("Nothing left to do!") ;
       else               do_pval = 0 ;
     }
   }
   iarg++ ;

   /* read val, convert to z(q)-score, convert to q-value, print, loop back */

   if( iarg >= argc ) ERROR_exit("No 'val' argument?!") ;
   while( iarg < argc ){
     val = (float)strtod( argv[iarg] , NULL ) ;
     if( do_pval != 2 ){
       if( !do_qinput ){
         zval = THD_fdrcurve_zval( dset , ival , val ) ;
         if( zval > 0.0f ) qval = 2.0*qg(zval) ;
         else              qval = 1.0f ;
       } else {
              if( val <= 0.0f ) val = 1.e-9f   ;  /* q must be between */
         else if( val >= 1.0f ) val = 0.99999f ;  /* 0 and 1, stoopid! */
         zval = qginv(0.5*val) ;
         qval = THD_fdrcurve_zqtot( dset , ival , zval ) ;
       }
     }
     if( do_pval != 0 ){
       pval = THD_stat_to_pval( val , DSET_BRICK_STATCODE(dset,ival) ,
                                      DSET_BRICK_STATAUX (dset,ival)  ) ;
     }
     switch( do_pval ){
       default: printf(" %.5g\n"      ,qval)      ; break ;
       case 1:  printf(" %.5g  %.5g\n",qval,pval) ; break ;
       case 2:  printf(" %.5g\n"      ,pval)      ; break ;
     }
     iarg++ ;                   /* next arg */
   }

   exit(0) ;  /* exit, pursued by a bear */
}
