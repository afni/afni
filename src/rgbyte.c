
#include "mrilib.h"

typedef struct { byte r,g,b ; } rgbyte ;

int main( int argc , char * argv[] )
{
   rgbyte fred ;
   int ii = sizeof(fred) ;
   printf("sizeof(rgbyte) = %d\n",ii) ;
   exit(0);
}
