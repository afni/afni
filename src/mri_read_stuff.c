#include "mrilib.h"
#include <signal.h>

#undef  QBUF
#define QBUF 4096

MRI_IMAGE * mri_read_stuff( char *fname )
{
   static int first=1 ;
   static char *jpeg_filter = NULL ;  /* djpeg     */
   static char *gif_filter  = NULL ;  /* giftopnm  */
   static char *tiff_filter = NULL ;  /* tifftopnm */
   static char *bmp_filter  = NULL ;  /* bmptoppm  */
   static char *png_filter  = NULL ;  /* pngtopnm  */

   char *pg , *pg2 , *filt=NULL ;
   int nf , nbuf , ipos , nx,ny,maxval , bper ;
   FILE *fp ;
   MRI_IMAGE *im ;
   byte *imar , *buf ;

   /*--- check input for OK-ness ---*/

   if( fname == NULL || *fname == '\0' ) return NULL ;

   /*--- first time in, setup up filters to PNM format ---*/

   if( first ){
     first = 0 ;

     pg = THD_find_executable( "djpeg" ) ;
     if( pg != NULL ){
       jpeg_filter = malloc(strlen(pg)+32) ;
       sprintf( jpeg_filter , "%s %%s" , pg ) ;
     }

     pg = THD_find_executable( "giftopnm" ) ;
     if( pg != NULL ){
       gif_filter = malloc(strlen(pg)+32) ;
       sprintf( gif_filter , "%s %%s" , pg ) ;
     }

     pg = THD_find_executable( "tifftopnm" ) ;
     if( pg != NULL ){
       tiff_filter = malloc(strlen(pg)+32) ;
       sprintf( tiff_filter , "%s %%s" , pg ) ;
     }

     pg = THD_find_executable( "bmptoppm" ) ;
     if( pg != NULL ){
       bmp_filter = malloc(strlen(pg)+32) ;
       sprintf( bmp_filter , "%s %%s" , pg ) ;
     }

     pg = THD_find_executable( "pngtopnm" ) ;
     if( pg != NULL ){
       png_filter = malloc(strlen(pg)+32) ;
       sprintf( png_filter , "%s %%s" , pg ) ;
     }
   }

   /*--- determine filter based on file suffix ---*/

   nf = strlen(fname) ;
   if( nf < 5 ) return NULL ;  /* filename too short! */

   pg  = fname + (nf-4);       /* points to last 4 chars */
   pg2 = pg - 1;               /* points to last 5 chars */

        if( strcmp(pg ,".jpg" ) == 0 ||
            strcmp(pg ,".JPG" ) == 0 ||
            strcmp(pg2,".jpeg") == 0 ||
            strcmp(pg2,".JPEG") == 0   ) filt = jpeg_filter ;

   else if( strcmp(pg ,".gif" ) == 0 ||
            strcmp(pg ,".GIF" ) == 0   ) filt = gif_filter  ;

   else if( strcmp(pg ,".tif" ) == 0 ||
            strcmp(pg ,".TIF" ) == 0 ||
            strcmp(pg2,".tiff") == 0 ||
            strcmp(pg2,".TIFF") == 0   ) filt = tiff_filter ;

   else if( strcmp(pg ,".bmp" ) == 0 ||
            strcmp(pg ,".BMP" ) == 0   ) filt = bmp_filter  ;

   else if( strcmp(pg ,".png" ) == 0 ||
            strcmp(pg ,".PNG" ) == 0   ) filt = png_filter  ;

   if( filt == NULL ) return NULL ;  /* didn't match */

   /*--- create the filter for this file and open the pipe ---*/

   pg = malloc(nf+strlen(filt)+32) ;
   sprintf( pg , filt , fname ) ;

   signal( SIGPIPE , SIG_IGN ) ;  /* don't like this signal */
   fp = popen( pg , "r" ) ;
   if( fp == NULL ){ free(pg); return NULL; }  /* bad pipe */

   buf = malloc(QBUF) ;  /* read buffer for initial data */

   /*--- read 1st block from pipe ---*/

   nbuf = fread( buf , 1 , QBUF , fp ) ;

   if( nbuf < 16 ){  /* bad read */
     free(buf); free(pg); pclose(fp); return NULL;
   }

   if( buf[0] != 'P' ){  /* not a P?M file */
     free(buf); free(pg); pclose(fp); return NULL;
   }

        if( buf[1] == '6' ) bper = 3 ; /* PPM */
   else if( buf[1] == '5' ) bper = 1 ; /* PGM */
   else {                              /* bad */
     free(buf); free(pg); pclose(fp); return NULL;
   }

   ipos = 2 ;

   /* skip comment lines in the buffer */

#define SKIPCOM                                                     \
 { if(buf[ipos]=='#')                                               \
     do{ ipos++; } while( ipos<nbuf && buf[ipos]!='\n' ) ; }

   /* find an ASCII number in the buffer */

#define NUMSCAN(var)                                                \
{ SKIPCOM ;                                                         \
  while( ipos<nbuf && !isdigit(buf[ipos]) ){ipos++; SKIPCOM;}       \
  if( ipos >= nbuf ){ var = -1; }                                   \
  else {                                                            \
    int nch; char chb[32];                                          \
    for( nch=0 ; ipos<nbuf && isdigit(buf[ipos]) ; nch++,ipos++ ){  \
      chb[nch] = buf[ipos]; }                                       \
    chb[nch]='\0'; var = strtol(chb,NULL,10);                       \
  } }

  NUMSCAN(nx) ;
  if( nx < 2 || ipos >= nbuf ){                      /* bad */
    free(buf); free(pg); pclose(fp); return NULL;
  }

  NUMSCAN(ny) ;
  if( ny < 2 || ipos >= nbuf ){                      /* bad */
    free(buf); free(pg); pclose(fp); return NULL;
  }

  NUMSCAN(maxval) ;
  if( maxval <= 0 || maxval > 255 || ipos >= nbuf ){ /* bad */
    free(buf); free(pg); pclose(fp); return NULL;
  }

  ipos++ ;   /* ipos now points at 1st byte of image data */

  /*--- create output image struct  ---*/

  if( bper == 3 ){                        /* PPM */
    im = mri_new( nx , ny , MRI_rgb ) ;
    imar = MRI_RGB_PTR(im) ;
  } else {                                /* PGM */
    im = mri_new( nx , ny , MRI_byte ) ;
    imar = MRI_BYTE_PTR(im) ;
  }
  mri_add_name( fname , im ) ;

  /*--- copy remaining data in buf to image array ---*/

  if( ipos < nbuf )
    memcpy( imar , buf , nbuf-ipos ) ;

  free(buf) ;          /* have used this up now */
  ipos = nbuf-ipos ;   /* how many bytes we've read into image */

  /*--- read rest of image directly from pipe ---*/

  fread( imar+ipos , 1 , bper*nx*ny-ipos , fp ) ;

  /*--- toss the trash and vamoose the ranch ---*/

  free(pg); pclose(fp); return im;
}
