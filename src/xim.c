#include "xim.h"

/*------------------------------------------------------------------------*/

void MCW_kill_XImage( XImage * image )
{
   if( image != NULL ){
      if( image->data != NULL ){
         XtFree( image->data ) ; image->data = NULL ;
      }
      XDestroyImage( image ) ;
   }
   return ;
}

/*-------------------------------------------------------------------------
  Create an XImage from an MRI_IMAGE of shorts:
    values >= 0 draw from the "image" palette
    values <  0 draw from the "overlay" palette
---------------------------------------------------------------------------*/

XImage * mri_to_XImage( MCW_DC * dc , MRI_IMAGE * im )
{
   int  iN, w2, width, height ;
   char      * Image;
   XImage    * ximage;

   register int     i , hw , sk , k ;
   register short * sar ;
   register Pixel * ppix , * npix ;
   register char  * ptr;

   if( im->kind != MRI_short ){
      fprintf(stderr,"\a\n*** ILLEGAL image input to mri_to_XImage\n") ;
      sleep(1) ; exit(1) ;
   }
   sar  = MRI_SHORT_PTR(im) ;
   ppix = dc->pix_im ;       /* array for positive pixels */
   npix = dc->ovc->pix_ov ;  /* array for negative pixels */

   width  = im->nx ;
   height = im->ny ;

   iN = (dc->planes + 7) / 8;       /* 1 or 2, bytes per pixel */
   w2 = width * iN;                 /* rowlength in bytes */

   Image = (char *) XtMalloc( (size_t) (w2*height) );

   ximage = XCreateImage( dc->display , dc->visual , dc->planes ,
                          ZPixmap , 0 , Image , width,height , 8 , w2 ) ;

   if( ximage == NULL ){
      fprintf(stderr,"\a\n*** CANNOT create new XImage for display\n") ;
      sleep(1) ; exit(1) ;
   }

   ptr = Image;
   k   = 0;
   hw  = height * width ;

   switch( iN ){

      case 1:                             /* 1 byte data goes into Image. */
         for( i=0 ; i < hw ; i++ ){
            sk = sar[k++] ;
            *ptr++ = (sk >= 0) ? ppix[sk] : npix[-sk] ;
         }
      break ;

                                          /* [Note byte order assumption] */
      case 2:                             /* 2 byte data goes into Image. */
         for( i=0 ; i < hw ; i++ ){
            sk = sar[k++] ;
            if( sk >= 0 ){
               *ptr++ = ppix[sk] >> 8 ;  /* MSB */
               *ptr++ = ppix[sk] ;       /* LSB */
            } else {
               *ptr++ = npix[-sk] >> 8 ;
               *ptr++ = npix[-sk] ;
            }
         }
      break ;

      default:
         fprintf(stderr,"\a\n*** ILLEGAL value of iN in mri_to_XImage\n");
         sleep(1) ; exit(1) ;
   }

   return ximage;
}

/*--------------------------------------------------------------------------
   Input:  an XImage of one size
   Output: an XImage of another size
   method: nearest neighbor resampling
----------------------------------------------------------------------------*/

XImage * resize_XImage( MCW_DC * dc , XImage * image ,
                        int new_width , int new_height )
{
   static int * lt = NULL ;       /* lookup table stuff */
   static int old_width = -1 ;

   register int iy, ex, ey, iW, iH, iN, w2 ;
   char         *ximag;
   char         *Ep, *El, *Ip, *Il, *Id , *Ed ; /* d=data, l=row, p=pixel */
   int          Erow , Irow ;

   XImage * emage ;  /* to be output image */

   /*** sanity check ***/

   if( new_width <= 0 || new_height <= 0 ){
      fprintf(stderr ,
              "\a\n***ILLEGAL new width %d or height %d in resize\n",
              new_width , new_height ) ;
      sleep(1) ; exit(1) ;
   }

   /*** data about input image ***/

   iN = image->bits_per_pixel / 8;    /* 1 or 2, bytes per pixel */
   iW = image->width ;                /* input width and height */
   iH = image->height ;

   if( iW == new_width && iH == new_height ){ /* very special case */
        return image ;
   }

   /*** create emage of the appropriate size ***/

   w2    = new_width * iN ;
   ximag = (char *) XtMalloc( (size_t) (w2 * new_height) );

   if( ximag == NULL ){
      fprintf(stderr,"\a\n***CANNOT allocate memory for resizing XImage\n") ;
      sleep(1) ; exit(1) ;
   }

   emage = XCreateImage( dc->display , dc->visual , dc->planes ,
                         ZPixmap, 0, ximag, new_width,new_height, 8, w2 ) ;

   if( emage == NULL ){
      fprintf(stderr,"\a\n*** CANNOT create new XImage for resizing\n") ;
      sleep(1) ; exit(1) ;
   }

   /*** make lookup table for xnew -> xold ***/

   if( new_width > old_width ){
      lt = (int *) XtRealloc( (char *)lt,(size_t)(new_width * sizeof(int)) );
      old_width = new_width ;
   }

   for( ex=0 ; ex < new_width ; ex++ )
      lt[ex] = MAP_XY(ex,new_width,iW) * iN ;

   /*** get ready to go ***/

   Ed = (char *) emage->data ; Erow = emage->bytes_per_line ;
   Id = (char *) image->data ; Irow = image->bytes_per_line ;

   switch( iN ){

      case 1:                                 /* 1 byte per pixel */
         for( ey=0 ; ey < new_height ; ey++ ){

            iy = MAP_XY(ey,new_height,iH) ;   /* row index in input image */
            Il = Id + Irow * iy ;             /* start of that row */
            El = Ed + Erow * ey ;             /* start of row in output */
            Ep = El ;
            for( ex=0 ; ex < new_width ; ex++ ){
               Ip = Il + lt[ex] ;             /* data pointer in input */
               *Ep++ = *Ip ;
            }
         }
      break ;

      case 2:                                 /* 2 bytes per pixel */
         for( ey=0 ; ey < new_height ; ey++ ){

            iy = MAP_XY(ey,new_height,iH) ;   /* row index in input image */
            Il = Id + Irow * iy ;             /* start of that row */
            El = Ed + Erow * ey ;             /* start of row in output */
            Ep = El ;
            for( ex=0 ; ex < new_width ; ex++ ){
               Ip = Il + lt[ex] ;             /* data pointer in input */
               *Ep++ = *Ip ;
               *Ep++ = *(Ip+1) ;
            }
         }
      break ;

      default:
         fprintf(stderr,"\a\n***ILLEGAL planes for resizing\n") ;
         sleep(1) ; exit(1) ;
   }

   return emage ;
}

/*---------------------------------------------------------------------------
   Input = XImage (with pixels from dc)
   Ouput = RGB or Grayscale image
-----------------------------------------------------------------------------*/

MRI_IMAGE * XImage_to_mri(  MCW_DC * dc , XImage * ximage )
{
   int nx , ny , npix , ii,jj , kk , pix_bytes , allgray , pp , lsize ;
   byte * rgb , * gray ;
   byte rr,gg,bb ;
   byte * ptr ;
   XColor * xc ;
   MRI_IMAGE * outim ;

   if( ximage == NULL || ximage->data == NULL ) return NULL ;

#if 0
fprintf(stderr,
        "XImage bitmap_unit   =%3d  bitmap_pad=%3d  depth =%3d\n"
        "       bytes_per_line=%3d  width     =%3d  height=%3d\n"
        "       bits_per_pixel=%3d  xoffset   =%3d\n" ,
 ximage->bitmap_unit    , ximage->bitmap_pad , ximage->depth ,
 ximage->bytes_per_line , ximage->width      , ximage->height ,
 ximage->bits_per_pixel , ximage->xoffset ) ;
#endif

   nx = ximage->width ; ny = ximage->height ; npix = nx * ny ;

   lsize = ximage->bytes_per_line ;

   pix_bytes = ximage->bits_per_pixel / 8;    /* 1 or 2 bytes per pixel */
   ptr       = (byte *) ximage->data ;        /* pointer to pixels */

   rgb = (byte *) malloc( sizeof(byte) * 3*npix ) ;
   if( rgb == NULL ){
      fprintf(stderr,"\n*** malloc failure in XImage_to_mri\n") ;
      sleep(1) ; exit(1) ;
   }

   switch( pix_bytes ){

      case 1:                              /* 1 byte per pixel */
         kk = 0 ; allgray = 1 ;
         for( jj=0 ; jj < ny ; jj++ ){
            for( ii=0 ; ii < nx ; ii++ ){
               pp = ptr[ii+jj*lsize] ;                     /* pixel value */
               xc = DCpix_to_XColor( dc , pp ) ;           /* XColor */
               rr = rgb[kk++] = INTEN_TO_BYTE( xc->red ) ;
               gg = rgb[kk++] = INTEN_TO_BYTE( xc->green ) ;
               bb = rgb[kk++] = INTEN_TO_BYTE( xc->blue ) ;
               allgray = allgray && (rr==gg) && (gg=bb) ;
            }
         }
      break ;

      case 2:                               /* 2 bytes per pixel */
         kk = 0 ; allgray = 1 ;
         for( jj=0 ; jj < ny ; jj++ ){
            for( ii=0 ; ii < nx ; ii++ ){
               pp = (ptr[2*ii+jj*lsize] << 8) + ptr[2*ii+jj*lsize+1] ;
               xc = DCpix_to_XColor( dc , pp ) ;
               rr = rgb[kk++] = INTEN_TO_BYTE( xc->red ) ;
               gg = rgb[kk++] = INTEN_TO_BYTE( xc->green ) ;
               bb = rgb[kk++] = INTEN_TO_BYTE( xc->blue ) ;
               allgray = allgray && (rr==gg) && (gg=bb) ;
            }
         }
      break ;

      default:
         fprintf(stderr,"\n*** ILLEGAL value of pix_bytes in XImage_to_mri\n");
         sleep(1) ; exit(1) ;
   }

   /*** if all pixels are gray, return a grayscale image ***/

   if( allgray ){

      gray = (byte *) malloc( sizeof(byte) * npix ) ;
      if( gray == NULL ){
         fprintf(stderr,"\n*** malloc failure in XImage_to_mri\n") ;
         sleep(1) ; exit(1) ;
      }
      for( ii=0 , kk=0 ; ii < npix ; ii++ , kk+=3) gray[ii] = rgb[kk] ;
      free(rgb) ;
      outim = mri_new_vol_empty( nx , ny , 1 , MRI_byte ) ;
      mri_fix_data_pointer( gray , outim ) ;

   } else {

   /*** not all gray --> return color RGB image ***/

      outim = mri_new_vol_empty( nx , ny , 1 , MRI_rgb ) ;
      mri_fix_data_pointer( rgb , outim ) ;
   }

   return outim ;
}
