#include "mrilib.h"
#include <string.h>

static byte map[26] =
  {  30,  50,  70,  90, 106, 118, 130, 140, 146, 152, 158, 164, 170,
    176, 182, 190, 198, 206, 212, 218, 224, 230, 236, 242, 248, 254 } ;

static byte bk[26] ;

static char * alpha = "ABCDEFGHIJKLMNOPQRSTUVWXYZ" ;
static char * num   = "0123456789" ;

#define NOUT 61

int main( int argc , char * argv[] )
{
   MRI_IMAGE * im ;
   byte * bp , bb ;
   int ii , jj , nn , rr , kk , nlin=0 ;
   char out[NOUT+1] , zout[NOUT+1] , cc,nc ;

   if( argc < 2 ){ fprintf(stderr,"Usage: to26 input.pgm > output.26\n"); exit(0); }

   im = mri_read( argv[1] ) ; if( im == NULL ) exit(1) ;
   bp = MRI_BYTE_PTR(im) ;

   for( ii=0 ; ii < 25 ; ii++ ) bk[ii] = (map[ii]+map[ii+1])/2 ;
   bk[25] = 255 ;

fprintf(stderr,"image dimensions = %d %d\n",im->nx,im->ny) ;

   nn = 0 ; rr = 0 ;
   printf( "#undef  NX26\n") ;
   printf( "#undef  NY26\n") ;
   printf( "#define NX26 %d\n",im->nx) ;
   printf( "#define NY26 %d\n",im->ny) ;
   printf( "static char * im26[] = {\n" ) ;
   for( ii=0 ; ii < im->nvox ; ii++ ){
      bb = bp[ii] ;
      for( jj=0 ; jj < 26 ; jj++ ) if( bb <= bk[jj] ) break ;

      out[nn++] = (jj<26) ? alpha[jj] : 'Z' ;

      if( nn == NOUT && ii < im->nvox-1 ){
         out[nn] = '\0' ;
         cc = out[0] ; rr = 1 ; kk = 0 ;
         for( jj=1 ; jj <= nn ; jj++ ){
            nc = out[jj] ;
            if( nc == cc ){  /* same character */
               if( rr == 9 ){ zout[kk++] = num[rr] ; zout[kk++] = cc ; rr = 0 ; }
               rr++ ;
            } else {         /* new character */
               if( rr == 1 ){
                  zout[kk++] = cc ; rr = 1 ;
               } else if( rr == 2 ){
                  zout[kk++] = cc ; zout[kk++] = cc ; rr = 1 ;
               } else {
                  zout[kk++] = num[rr] ; zout[kk++] = cc ; rr = 1 ;
               }
            }
            cc = nc ;
         }
         zout[kk] = '\0' ;
         printf("   \"%s\",\n",zout) ; nlin++ ; nn = 0 ;
      }
   }

   out[nn] = '\0' ;
   printf("   \"%s\"\n};\n",out) ; nlin++ ;
   printf("#undef  NLINE26\n") ;
   printf("#define NLINE26 %d\n",nlin) ;
   exit(0) ;
}
