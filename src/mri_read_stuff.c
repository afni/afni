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
   static char *pnm_filter  = NULL ;  /* cat */

   char *pg , *pg2 , *filt=NULL ;
   int nf , nbuf , ipos , nx,ny,maxval=255 , bper,nbim, pbm=0 ;
   FILE *fp ;
   MRI_IMAGE *im ;
   byte *imar , *buf ;

   /*--- check input for OK-ness ---*/

   if( fname == NULL || *fname == '\0' ) return NULL ;

   /*--- first time in, setup up filters to PNM format ---*/

   if( first ){
     first = 0 ;

     pg = THD_find_executable( "cat" ) ;    /* cheap, but works */
     if( pg != NULL ){
       pnm_filter = AFMALL(char, strlen(pg)+32) ;
       sprintf( pnm_filter , "%s %%s" , pg ) ;
     }

     pg = THD_find_executable( "djpeg" ) ;
     if( pg != NULL ){
       jpeg_filter = AFMALL(char, strlen(pg)+32) ;
       sprintf( jpeg_filter , "%s %%s" , pg ) ;
     }

     pg = THD_find_executable( "giftopnm" ) ;
     if( pg != NULL ){
       gif_filter = AFMALL(char, strlen(pg)+32) ;
       sprintf( gif_filter , "%s %%s" , pg ) ;
     }

     pg = THD_find_executable( "tifftopnm" ) ;
     if( pg != NULL ){
       tiff_filter = AFMALL(char, strlen(pg)+32) ;
       sprintf( tiff_filter , "%s %%s" , pg ) ;
     }

     pg = THD_find_executable( "bmptoppm" ) ;
     if( pg != NULL ){
       bmp_filter = AFMALL(char, strlen(pg)+32) ;
       sprintf( bmp_filter , "%s %%s" , pg ) ;
     }

     pg = THD_find_executable( "pngtopnm" ) ;
     if( pg != NULL ){
       png_filter = AFMALL(char, strlen(pg)+32) ;
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

   else if( strcmp(pg ,".pbm" ) == 0 ||
            strcmp(pg ,".PBM" ) == 0 ||
            strcmp(pg ,".pgm" ) == 0 ||
            strcmp(pg ,".PGM" ) == 0 ||
            strcmp(pg ,".ppm" ) == 0 ||
            strcmp(pg ,".PPM" ) == 0   ) filt = pnm_filter  ;

   else if( strcmp(pg ,".tif" ) == 0 ||
            strcmp(pg ,".TIF" ) == 0 ||
            strcmp(pg2,".tiff") == 0 ||
            strcmp(pg2,".TIFF") == 0   ) filt = tiff_filter ;

   else if( strcmp(pg ,".bmp" ) == 0 ||
            strcmp(pg ,".BMP" ) == 0   ) filt = bmp_filter  ;

   else if( strcmp(pg ,".png" ) == 0 ||
            strcmp(pg ,".PNG" ) == 0   ) filt = png_filter  ;

   if( filt == NULL ) return NULL ;  /* didn't match, or no filter */

   /*--- create the filter for this file and open the pipe ---*/

   pg = AFMALL(char, nf+strlen(filt)+32) ;  /* string to hold filter */
   sprintf( pg , filt , fname ) ;

   signal( SIGPIPE , SIG_IGN ) ;  /* ignore this signal */
   fp = popen( pg , "r" ) ;
   if( fp == NULL ){ free(pg); return NULL; }  /* bad pipe */

   buf = AFMALL(byte, QBUF) ;  /* read buffer for initial data from pipe */

   /*--- read 1st block from pipe ---*/

   nbuf = fread( buf , 1 , QBUF , fp ) ;

   if( nbuf < 16 ){  /* bad read */
     free(buf); free(pg); pclose(fp); return NULL;
   }

   if( buf[0] != 'P' ){  /* not a P?M file */
     free(buf); free(pg); pclose(fp); return NULL;
   }

        if( buf[1] == '6' ) bper = 3 ;              /* PPM from pipe */
   else if( buf[1] == '5' ) bper = 1 ;              /* PGM from pipe */
   else if( buf[1] == '4' ){bper = 1 ; pbm=1; }     /* PBM from pipe */
   else {
     free(buf); free(pg); pclose(fp); return NULL;  /* bad bad bad!! */
   }

   ipos = 2 ;  /* start scanning for PNM header stuff at position 2 in buf */

   /* skip comment lines in the buffer */

#undef  SKIPCOM
#define SKIPCOM                                                     \
 { if(buf[ipos]=='#')                                               \
     do{ ipos++; } while( ipos<nbuf && buf[ipos]!='\n' ) ; }

   /* find an ASCII number in the buffer */

#undef  NUMSCAN
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

  /* scan for the nx variable */

  NUMSCAN(nx) ;
  if( nx < 2 || ipos >= nbuf ){                      /* bad */
    free(buf); free(pg); pclose(fp); return NULL;
  }

  /* scan for the ny variable */

  NUMSCAN(ny) ;
  if( ny < 2 || ipos >= nbuf ){                      /* bad */
    free(buf); free(pg); pclose(fp); return NULL;
  }

  /* scan for the maxval variable */

  if( !pbm ){
    NUMSCAN(maxval) ;
    if( maxval <= 0 || maxval > 255 || ipos >= nbuf ){ /* bad */
      free(buf); free(pg); pclose(fp); return NULL;
    }
  }

  ipos++ ;   /* skip byte after maxval;                   */
             /* ipos now points at 1st byte of image data */

  /*--- create output image struct  ---*/

  if( bper == 3 ){                        /* PPM */
    im   = mri_new( nx , ny , MRI_rgb ) ;
    imar = MRI_RGB_PTR(im) ;
  } else {                                /* PGM or PBM */
    im   = mri_new( nx , ny , MRI_byte ) ;
    imar = MRI_BYTE_PTR(im) ;
  }
  mri_add_name( fname , im ) ;
  nbim = bper * nx * ny ;        /* num bytes in image array imar */

  /*--- copy remaining data in buf (if any) to image array ---*/

  nbuf = nbuf - ipos ;             /* num bytes left in buf */
  if( nbuf > nbim ) nbuf = nbim ;  /* but don't want too much */
  if( nbuf > 0 )
    memcpy( imar , buf+ipos , nbuf ) ;

  free(buf) ;     /* have used this up now */

  /*--- read rest of image array directly from pipe ---*/

  if( nbuf < nbim )
    fread( imar+nbuf , 1 , nbim-nbuf , fp ) ;

  free(pg) ; pclose(fp) ;  /* toss out the trash */

  /*--- if was really a PBM image, inflate to PGM now ---*/

  /*--- if maxval < 255, scale byte data up to that level ---*/

  if( pbm ) mri_inflate_pbm( im ) ;  /* 02 Jan 2003 */

  if( maxval < 255 ){
    int ii ; float fac = 255.4/maxval ;
    for( ii=0 ; ii < nbim ; ii++ ) imar[ii] = (byte)( imar[ii]*fac ) ;
  }

  /*--- vamoose the ranch ---*/

  return im;
}

/*--------------------------------------------------------------------------*/
/*! Inflate data from PBM to PGM, in place.
----------------------------------------------------------------------------*/

void mri_inflate_pbm( MRI_IMAGE *im )  /* 02 Jan 2002 */
{
   MRI_IMAGE *qim ;
   byte *qimar , *imar ;
   int ii,jj , nx,ny , nbrow , i8 ;
   byte bmask[8] = { 1<<7 , 1<<6 , 1<<5 , 1<<4 , 1<<3 , 1<<2 , 1<<1 , 1 } ;

   if( im == NULL || im->kind != MRI_byte ) return ;

   nx = im->nx ; ny = im->ny ;
   qim   = mri_new( nx , ny , MRI_byte ) ;
   qimar = MRI_BYTE_PTR(qim) ;
   imar  = MRI_BYTE_PTR(im) ;

   nbrow = nx/8 ; if( 8*nbrow < nx ) nbrow++ ;

   for( jj=0 ; jj < ny ; jj++ )
     for( ii=0 ; ii < nx ; ii++ ){
       i8 = ii >> 3 ;
       qimar[ii+jj*nx] = ( imar[(i8)+jj*nbrow] & bmask[ii&7] ) != 0 ;
     }

   memcpy( imar , qimar , nx*ny ) ; mri_free( qim ) ;
}
