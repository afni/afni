#include "mrilib.h"
#include <string.h>

static byte map[26] =
  {  30,  59,  76,  93, 107, 118, 130, 141, 146, 152, 161, 166, 171,
    174, 177, 182, 192, 206, 221, 231, 237, 242, 246, 250, 252, 253 } ;

static char * alpha = "ABCDEFGHIJKLMNOPQRSTUVWXYZ" ;

#define NOUT 60

int main( int argc , char * argv[] )
{
   MRI_IMAGE * im ;
   byte * bp , bb ;
   int ii , jj , nn , rr ;
   char out[NOUT+1] ;

   im = mri_read( "cox3.pgm" ) ;
   bp = MRI_BYTE_PTR(im) ;

fprintf(stderr,"image dimensions = %d %d\n",im->nx,im->ny) ;

   nn = 0 ; rr = 0 ;
   printf( "#define NCOX3_NX %d\n",im->nx) ;
   printf( "#define NCOX3_NY %d\n",im->ny) ;
   printf( "static char * bcox3[] = {\n" ) ;
   for( ii=0 ; ii < im->nvox ; ii++ ){
      bb = bp[ii] ;
      for( jj=0 ; jj < 26 ; jj++ ) if( bb == map[jj] ) break ;

      out[nn++] = (jj<26) ? alpha[jj] : 'Z' ;

      if( nn == NOUT && ii < im->nvox-1 ){
         out[nn] = '\0' ; nn = 0 ;
         printf("   \"%s\",\n",out) ;
      }
   }
   out[nn] = '\0' ;
   printf("   \"%s\"\n};\n",out) ;
   exit(0) ;
}
