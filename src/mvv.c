#include "mrilib.h"

int main( int argc , char * argv[] )
{
   multivector * mv ; int j ;

   mv = multivector_read( argv[1] ) ;
   if( mv == NULL ){fprintf(stderr,"read fails\n");exit(1);}
   multivector_set_name( mv , NULL ) ;
   j = multivector_write( argv[2] , mv ) ;
   if( j == 0 ){fprintf(stderr,"write fails\n");exit(1);}
   multivector_free(mv) ; exit(0) ;
}
