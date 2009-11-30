#include "mrilib.h"
#include <string.h>

#define MMAX 82   /* max # colors */
#define NOUT 61   /* max # chars per output line */

static byte rmap[MMAX], gmap[MMAX], bmap[MMAX] ;          /* color map */

static char alpha[MMAX] = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"    /* color codes */
                          "abcdefghijklmnopqrstuvwxyz"    /* do not use */
                          ",<.>/?;:'[{]}|=+-_)(*&^%$#@!`~" ; /* digits */

static char num[10] = "0123456789" ;                      /* number codes */

static MRI_IMAGE * SPLASH_decodexx( int , int , int , int ,
                                    byte *, byte *, byte * , char ** ) ;

#include "splash_blank_new.h"

int main( int argc , char * argv[] )
{
   MRI_IMAGE *im ;
   byte *bp , rr,bb,gg ;
   int ii,jj,nmap,kk , nn , qq , nlin=0 ;
   char out[NOUT+1] , zout[NOUT+1] , cc,nc , *nam ;

   if( argc < 2 || strcmp(argv[1],"-help") == 0 ){
     printf("Usage: toxx NAME input.ppm > output.xx\n"
            "Converts a PPM image to an 'xx' (C header) image\n"
            "for use in afni_splash.c -- NAME is the string to\n"
            "use in the output file variables defining the data.\n"
            "Probably get the input by\n"
            "  ppmquant xx image.ppm > input.ppm\n"
            "where 'xx' is the number of colors to use.\n"
            "Max value of 'xx' is %d.\n\n" , MMAX
           ) ;
     printf("Alternative usage:\n"
            "  toxx -splash > output.ppm\n"
            "Outputs the blank AFNI splash screen to a PPM image.\n"
            "The .xx data for this is stored in splash_blank_new.h\n"
           ) ;
     exit(0);
   }

   /*----- alternative usage -----*/

   if( strcasecmp(argv[1],"-splash") == 0 ){  /* 30 Nov 2009 */
     MRI_IMAGE *imspl ;
     imspl = SPLASH_decodexx( NX_blank, NY_blank, NLINE_blank, NC_blank,
                              RMAP_blank,GMAP_blank,BMAP_blank, BAR_blank ) ;
     if( imspl == NULL ) ERROR_exit("Can't create splash image?!") ;
     mri_write_pnm( "-" , imspl ) ; /* to stdout */
     exit(0) ;
   }

   /*----- normal usage -----*/

   nam = argv[1] ;
   im = mri_read( argv[2] ) ;
   if( im == NULL ){
     fprintf(stderr,"** Can't read file %s\n",argv[2]); exit(1);
   }
   switch( im->kind ){
     case MRI_rgb:
       INFO_message("Input is color %dx%d",im->nx,im->ny) ;
     break ;
     case MRI_byte:{
       MRI_IMAGE *qim = mri_to_rgb(im) ;
       mri_free(im) ; im = qim ;
       INFO_message("Input is grayscale %dx%d",im->nx,im->ny) ;
     }
     break ;
     default:
       ERROR_exit("Input %s is wrong kind of image!",argv[2]) ;
   }
   bp = MRI_RGB_PTR(im) ;

   /*--- build color map (MMAX or fewer entries allowed) ---*/

   rmap[0] = bp[0] ; gmap[0] = bp[1] ; bmap[0] = bp[2] ;

   for( ii=nmap=1 ; ii < im->nvox ; ii++ ){
     rr = bp[3*ii] ; gg = bp[3*ii+1] ; bb = bp[3*ii+2] ;
     for( kk=0 ; kk < nmap ; kk++ ){
       if( rr==rmap[kk] && gg==gmap[kk] && bb==bmap[kk] ) break ;
     }
     if( kk == nmap ){  /* new color */
       if( nmap == MMAX )
         ERROR_exit("Too many colors (> %d) in input %s\n",MMAX,argv[2]) ;
       rmap[nmap] = rr ; gmap[nmap] = gg ; bmap[nmap] = bb ;
       nmap++ ;
     }
   }

   INFO_message("Input has %d distinct colors",nmap) ;

   /*--- print header ---*/

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

   /*--- print image data (BAR = byte array) ---*/

   printf( "static char *BAR_%s[] = {\n" , nam ) ;
   for( ii=0 ; ii < im->nvox ; ii++ ){

      /* find color index kk of ii-th pixel */

      rr = bp[3*ii] ; gg = bp[3*ii+1] ; bb = bp[3*ii+2] ;
      for( kk=0 ; kk < nmap ; kk++ ){
        if( rr==rmap[kk] && gg==gmap[kk] && bb==bmap[kk] ) break ;
      }

      out[nn++] = alpha[kk] ;  /* save character code for this color */

      /* when output line buffer is filled, write it */

      if( nn == NOUT && ii < im->nvox-1 ){  /* output line is full; RLE it */
        out[nn] = '\0' ;
        cc = out[0] ;      /* first character in output line */
        qq = 1 ; kk = 0 ;  /* qq = repetition count; kk = pos in zout buffer */
        for( jj=1 ; jj <= nn ; jj++ ){
          nc = out[jj] ;   /* next character (cc = last character) */
          if( nc == cc ){  /* same character ==> store */
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

   /* put the last line out (no RLE here, because I'm lazy scum) */

   out[nn] = '\0' ;
   printf("   \"%s\"\n};\n",out) ; nlin++ ;
   printf("#define NLINE_%s %d\n",nam,nlin) ;  /* number of lines */
   exit(0) ;
}

/*--------------------------------------------------------------------------
  Decode 'xx' data into an image.
----------------------------------------------------------------------------*/

static MRI_IMAGE * SPLASH_decodexx( int nx, int ny, int nl, int nmap,
                                    byte *rmap, byte *gmap, byte *bmap ,
                                    char **imxx )
{
   MRI_IMAGE *im ;
   byte *bim ;
   int ii,jj , cc,qq , dd,ee , kk ;
   char bb ;
   static int first=1 , ainv[256] ;

ENTRY("SPLASH_decodexx") ;

   if( nmap == 0 ) RETURN(NULL) ;

   if( nx < 3       || ny < 3       || nl < 3 ||
       rmap == NULL || gmap == NULL ||
       bmap == NULL || imxx == NULL             ) RETURN(NULL) ;

   if( first ){
     for( ii=0 ; ii < 256 ; ii++ ) ainv[ii] = -1 ;
     for( ii=0 ; ii < MMAX ; ii++ ){
       bb = alpha[ii] ; ainv[bb] = ii ;
     }
     first = 0 ;
   }

   im  = mri_new( nx , ny , MRI_rgb ) ;
   bim = MRI_RGB_PTR(im) ;

   /* decode the RLE image data into a real image array */

   cc = qq = 0 ;
   for( ii=0 ; ii < 3*im->nvox && qq < nl ; ){
     bb = imxx[qq][cc++] ; if( bb == '\0' ) break ;
     jj = ainv[bb] ;
     if( jj >= 0 ){
       bim[ii++] = rmap[jj]; bim[ii++] = gmap[jj]; bim[ii++] = bmap[jj];
     } else {
       dd = bb - '0' ;
       bb = imxx[qq][cc++] ; if( bb == '\0' ) break ;
       jj = ainv[bb] ;
       for( ee=0 ; ee < dd && ii < 3*im->nvox ; ee++ ){
         bim[ii++] = rmap[jj]; bim[ii++] = gmap[jj]; bim[ii++] = bmap[jj];
       }
     }
     if( imxx[qq][cc] == '\0' ){ cc = 0 ; qq++ ; }
   }

   RETURN(im) ;
}
