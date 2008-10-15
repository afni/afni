#include "mrilib.h"

int main( int argc , char *argv[] )
{
   int iarg , ival ;
   THD_3dim_dataset *dset ;
   floatvec *fv ;
   float val , zval , qval ;

   if( argc < 4 || strcmp(argv[1],"-help") == 0 ){
     printf(
       "Usage: fdrval dset sub val [val ...]\n"
       "\n"
       "Reads FDR curve data from the header of dset for sub-brick\n"
       "#sub and computes the q-value when the sub-brick statistical\n"
       "threshold is set to val.\n"
       "\n"
       "* Output for each 'val' is written to stdout.\n"
       "* If the q-value can't be computed, then 1.0 is output.\n"
       "* Example:\n"
       "    fdrval Fred_REML+orig 0 `count -scale 0.1 10 20` | 1dplot -stdin\n"
       "  Uses the 'count' program to input a sequence of values, and then\n"
       "  pipes into the 1dplot program to make a graph of F vs. q.\n"
       "* See the link below for information on how AFNI computes FDR curves:\n"
       "    http://afni.nimh.nih.gov/pub/dist/doc/misc/FDR/FDR_Jan2008.pdf\n"
       "* Also see the output of '3dFDR -help'\n"
       "\n"
       "-- A quick hack by RWCox -- 15 Oct 2008 -- PG Wodehouse's birthday!\n"
     ) ;
     PRINT_COMPILE_DATE ; exit(0) ;
   }

   mainENTRY("fdrval") ; machdep() ;

   iarg = 1 ;

   /* if there were any options, they'd be scanned for here */

   /* read the required 'dset' and 'sub' arguments */

   dset = THD_open_dataset( argv[iarg] ) ;
   CHECK_OPEN_ERROR(dset,argv[iarg]) ;
   iarg++ ;

   ival = (int)strtod(argv[iarg],NULL) ;
   if( ival < 0 || ival >= DSET_NVALS(dset) )
     ERROR_exit("fdrval dataset '%s' doesn't have sub-brick #%d",
                argv[iarg-1],ival) ;

   /* check if there is an FDR curve for this sub-brick */

   fv = DSET_BRICK_FDRCURVE(dset,ival) ;
   if( fv == NULL )
     ERROR_exit("fdrval dataset '%s' doesn't have FDR curve #%d",
                argv[iarg-1],ival) ;
   iarg++ ;

   /* read val, convert to z-score, convert to q-value, print, loop back */

   while( iarg < argc ){
     val = (float)strtod( argv[iarg] , NULL ) ;
     zval = THD_fdrcurve_zval( dset , ival , val ) ;
     if( zval > 0.0f ) qval = 2.0*qg(zval) ;
     else              qval = 1.0f ;
     printf(" %.5g\n",qval) ;
     iarg++ ;                   /* next arg */
   }

   exit(0) ;  /* exit, pursued by a bear */
}
