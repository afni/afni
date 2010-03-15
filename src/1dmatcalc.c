#include "mrilib.h"

int main( int argc , char *argv[] )
{
   char *expr ;
   int ii , ll , ebas=1 ;

   if( argc < 2 || strcmp(argv[1],"-help") == 0 ){
     printf("Usage: 1dmatcalc [-verb] expression\n\n") ;
     printf("%s\n",mri_matrix_evalrpn_help()) ;
     printf("SIMPLE EXAMPLES\n"
            "---------------\n"
            "* Multiply each element of an input 1D file\n"
            "  by a constant factor and write to disk.\n"
            "    1dmatcalc \"&read(in.1D) 3.1416 * &write(out.1D)\"\n"
            "\n"
            "* Subtract two 1D files\n"
            "    1dmatcalc \"&read(a.1D) &read(b.1D) - &write(stdout:)\"\n"
           ) ;
     exit(0) ;
   }

   if( strcmp(argv[1],"-verb") == 0 ){
     mri_matrix_evalrpn_verb(1) ; ebas++ ;
   }

   ll=16 ;
   for( ii=ebas ; ii < argc ; ii++ ) ll += strlen(argv[ii])+1 ;
   expr = calloc(1,ll) ;
   for( ii=ebas ; ii < argc ; ii++ ){
     strcat(expr,argv[ii]) ; strcat(expr," ") ;
   }

   (void)mri_matrix_evalrpn(expr) ;
   exit(0) ;
}
