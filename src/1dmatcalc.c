#include "mrilib.h"

int main( int argc , char *argv[] )
{
   char *expr ;
   int ii , ll ;

   if( argc < 2 || strcmp(argv[1],"-help") == 0 ){
     printf("Usage: 1dmatcalc expression\n\n") ;
     printf("%s\n",mri_matrix_evalrpn_help()) ;
     exit(1) ;
   }

   ll=16 ;
   for( ii=1 ; ii < argc ; ii++ ) ll += strlen(argv[ii])+1 ;
   expr = calloc(1,ll) ;
   for( ii=1 ; ii < argc ; ii++ ){
     strcat(expr,argv[ii]) ; strcat(expr," ") ;
   }

   (void)mri_matrix_evalrpn(expr) ;
   exit(0) ;
}
