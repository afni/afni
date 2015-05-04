#include "mrilib.h"

int main( int argc , char *argv[] )
{
   int nopt , nbad=0 ; char *vlist, *expr , *mess ;

   /*-------------------------------------------------------------------------*/
   if( argc < 2 || strcasecmp(argv[1],"-help") == 0 ){
     printf("\n"
       "The function of program GLTsymtest is to test a set of '-gltsym'\n"
       "strings -- for use with 3dDeconvolve or 3dREMLfit -- for validity.\n"
       "\n"
       "Usage:  GLTsymtest varlist expr [expr ...]\n"
       "\n"
       "* 'varlist' is a list of allowed variable names in the expression.\n"
       "  These names can be separated by commans, semicolons, and/or\n"
       "  spaces (varlist would have to be in quotes if it contains spaces).\n"
       "\n"
       "* Each 'expr' is a GLT symbolic expression, which should be in quotes\n"
       "  since different components are separated by blanks.\n"
       "\n"
       "EXAMPLE\n"
       "-------\n"
       "  GLTsymtest 'Vrel Arel' 'Vrel -Arel' 'Verl + +aud'\n"
       "\n"
       "  The first expression is good, but the second has both variable names\n"
       "  mis-typed; the output from this program would include these messages:\n"
       "\n"
       "    ***** Scanned GLT messages *****\n"
       "    ++ -gltsym is: 'Vrel -Arel'\n"
       "    ++ INFO: Allowed variable list is 'Vrel Arel'\n"
       "    ++ INFO: This gltsym appears to be OKAY :-)\n"
       "\n"
       "    ***** Scanned GLT messages *****\n"
       "    ++ -gltsym is: 'Verl + +aud'\n"
       "    ++ INFO: Allowed variable list is 'Vrel Arel'\n"
       "    ++ INFO: -gltsym: isolated '+' is being ignored\n"
       "    ** ERROR: -gltsym: can't match symbolic name 'Verl'\n"
       "    ** ERROR: -gltsym: can't match symbolic name 'aud'\n"
       "    ** SORRY: This gltsym appears to be BAD :-(\n"
       "\n"
       "NOTES\n"
       "-----\n"
       "* GLTsymtest does not check subscripts on variable names against the legal\n"
       "  range for the name, since the information about the dimensionality of\n"
       "  the beta vector associated with each name is not available here.\n"
       "\n"
       "* The exit status for this program is the number of expressions that had\n"
       "  at least one ERROR message.  In the example above, this status would be 1.\n"
       "\n"
       "* The text output goes to stdout.\n"
       "\n"
       "* This program has no options, which is very unusual in the AFNI world!\n"
       "\n"
       "* Authored by RWCox on May Day 2015 to aid Rick Reynolds in detecting such\n"
       "  problems, induced for example when his boss does someting stupid during\n"
       "  an AFNI bootcamp in South Africa (a purely hypothetical case, I assure you).\n"
     ) ;
     exit(0) ;
   }
   /*-------------------------------------------------------------------------*/

   mainENTRY("GLTsymtest"); machdep();

   if( argc < 3 )
     ERROR_exit("GLTsymtest: only 1 argument?  You need at least 2!") ;

   vlist = argv[1] ;
   for( nopt=2 ; nopt < argc ; nopt++ ){
     expr = argv[nopt] ;
     mess = SYM_test_gltsym(vlist,expr) ;
     if( mess != NULL ){
       puts(mess) ;
       if( strstr(mess,"ERROR") != NULL ) nbad++ ;
     }
   }

   exit(nbad) ;
}
