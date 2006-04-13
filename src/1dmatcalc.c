#include "mrilib.h"

int main( int argc , char *argv[] )
{
   char *expr ;
   int ii , ll , ebas=1 ;

   if( argc < 2 || strcmp(argv[1],"-help") == 0 ){
     printf("Usage: 1dmatcalc [-verb] expression\n\n") ;
     printf("%s\n",mri_matrix_evalrpn_help()) ;
     exit(1) ;
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
