#include "mrilib.h"
#include <string.h>

#define MMAX 82   /* max # colors */
#define NOUT 61   /* max # chars per output line */

static byte rmap[MMAX], gmap[MMAX], bmap[MMAX] ;          /* color map */

static char alpha[MMAX] = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"    /* color codes */
                          "abcdefghijklmnopqrstuvwxyz"
                          ",<.>/?;:'[{]}|=+-_)(*&^%$#@!`~" ;

static char num[10] = "0123456789" ;                      /* number codes */

int main( int argc , char * argv[] )
{
   MRI_IMAGE *im ;
   byte *bp , rr,bb,gg ;
   int ii,jj,nmap,kk , nn , qq , nlin=0 ;
   char out[NOUT+1] , zout[NOUT+1] , cc,nc , *nam ;

   if( argc < 2 ){
     printf("Usage: toxx NAME input.ppm > output.xx\n"
            "Probably get the input by\n"
            "  ppmquant xx image.ppm > input.ppm\n"
            "where 'xx' is the number of colors to use.\n"
            "Max value of 'xx' is %d.\n" , MMAX
           ) ;
     exit(0);
   }

   nam = argv[1] ;
   im = mri_read( argv[2] ) ;
   if( im == NULL ){
     fprintf(stderr,"** Can't read file %s\n",argv[2]); exit(1);
   }
   switch( im->kind ){
     case MRI_rgb:
       fprintf(stderr,"++ Input is color %dx%d\n",im->nx,im->ny) ;
     break ;
     case MRI_byte:{
       MRI_IMAGE *qim = mri_to_rgb(im) ;
       mri_free(im) ; im = qim ;
       fprintf(stderr,"++ Input is grayscale %dx%d\n",im->nx,im->ny) ;
     }
     break ;
     default:
       fprintf(stderr,"** Input %s is wrong kind of image!\n",argv[2]); exit(1);
   }
   bp = MRI_RGB_PTR(im) ;

   /* build color map (MMAX or fewer entries allowed) */

   rmap[0] = bp[0] ; gmap[0] = bp[1] ; bmap[0] = bp[2] ;

   for( ii=nmap=1 ; ii < im->nvox ; ii++ ){
     rr = bp[3*ii] ; gg = bp[3*ii+1] ; bb = bp[3*ii+2] ;
     for( kk=0 ; kk < nmap ; kk++ ){
       if( rr==rmap[kk] && gg==gmap[kk] && bb==bmap[kk] ) break ;
     }
     if( kk == nmap ){  /* new color */
       if( nmap == MMAX ){
         fprintf(stderr,"** Too many colors in input %s\n",argv[2]); exit(1);
       }
       rmap[nmap] = rr ; gmap[nmap] = gg ; bmap[nmap] = bb ;
       nmap++ ;
     }
   }

   fprintf(stderr,"++ Input has %d distinct colors\n",nmap) ;

   nn = 0 ; qq = 0 ;
   printf( "#define NX_%s %d\n",nam,im->nx) ;
   printf( "#define NY_%s %d\n",nam,im->ny) ;
   printf( "#define NC_%s %d\n",nam,nmap  ) ;

   printf( "static byte RMAP_%s[] = {\n " , nam ) ;
   for( kk=0 ; kk < nmap ; kk++ )
     printf("%d%s",(int)rmap[kk] , (kk==nmap-1) ? "};\n" : "," ) ;

   printf( "static byte GMAP_%s[] = {\n " , nam ) ;
   for( kk=0 ; kk < nmap ; kk++ )
     printf("%d%s",(int)gmap[kk] , (kk==nmap-1) ? "};\n" : "," ) ;

   printf( "static byte BMAP_%s[] = {\n " , nam ) ;
   for( kk=0 ; kk < nmap ; kk++ )
     printf("%d%s",(int)bmap[kk] , (kk==nmap-1) ? "};\n" : "," ) ;

   printf( "static char *BAR_%s[] = {\n" , nam ) ;
   for( ii=0 ; ii < im->nvox ; ii++ ){
      rr = bp[3*ii] ; gg = bp[3*ii+1] ; bb = bp[3*ii+2] ;
      for( kk=0 ; kk < nmap ; kk++ ){
        if( rr==rmap[kk] && gg==gmap[kk] && bb==bmap[kk] ) break ;
      }

      out[nn++] = alpha[kk] ;

      if( nn == NOUT && ii < im->nvox-1 ){  /* output line is full; RLE it */
        out[nn] = '\0' ;
        cc = out[0] ; qq = 1 ; kk = 0 ;
        for( jj=1 ; jj <= nn ; jj++ ){
          nc = out[jj] ;
          if( nc == cc ){  /* same character */
            if( qq == 9 ){ zout[kk++] = num[qq] ; zout[kk++] = cc ; qq = 0 ; }
            qq++ ;
          } else {         /* new character */
            if( qq == 1 ){
              zout[kk++] = cc ; qq = 1 ;
            } else if( qq == 2 ){
              zout[kk++] = cc ; zout[kk++] = cc ; qq = 1 ;
            } else {
              zout[kk++] = num[qq] ; zout[kk++] = cc ; qq = 1 ;
            }
          }
          cc = nc ;
        }
        zout[kk] = '\0' ;
        printf("   \"%s\",\n",zout) ; nlin++ ; nn = 0 ;
      }
   }

   /* put the last line out (no RLE here) */

   out[nn] = '\0' ;
   printf("   \"%s\"\n};\n",out) ; nlin++ ;
   printf("#define NLINE_%s %d\n",nam,nlin) ;
   exit(0) ;
}
