/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/

#include "xim.h"

/*********************************************************************/
/***** 22 Aug 1998: modified to allow for 3 and 4 byte visuals,  *****/
/*****              and for either possible byte order -- RW Cox *****/
/*****                                                           *****/
/***** 11 Feb 1999: added ability to deal with MRI_rgb images    *****/
/*********************************************************************/

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
   int  w2, width, height ;
   unsigned char * Image;
   XImage        * ximage;
   int  border ;        /* 22 Aug 1998 */

   register int     i , hw , sk , k ;
   register short * sar ;
   register Pixel * ppix , * npix ;
   register unsigned char * ptr;

   if( im->kind == MRI_rgb ) return rgb_to_XImage( dc , im ) ;  /* 11 Feb 1999 */

   if( im->kind != MRI_short ){
      fprintf(stderr,"\n*** ILLEGAL image input to mri_to_XImage\n") ;
      exit(1) ;
   }
   sar  = MRI_SHORT_PTR(im) ;
   ppix = dc->pix_im ;       /* array for positive pixels */
   npix = dc->ovc->pix_ov ;  /* array for negative pixels */

   width  = im->nx ;
   height = im->ny ;

   w2 = width * dc->byper ;  /* rowlength in bytes */

   Image = (unsigned char *) XtMalloc( (size_t) (w2*height) );

   ximage = XCreateImage( dc->display , dc->visual , dc->depth ,
                          ZPixmap , 0 , Image , width,height , 8 , w2 ) ;

   if( ximage == NULL ){
      fprintf(stderr,"\n*** CANNOT create new XImage for display\n") ;
      exit(1) ;
   }

   border = ximage->byte_order ;          /* 22 Aug 1998 */

   ptr = Image;
   k   = 0;
   hw  = height * width ;

   switch( dc->byper ){

      case 1:                             /* 1 byte data goes into Image */
         for( i=0 ; i < hw ; i++ ){
            sk = sar[k++] ;
            *ptr++ = (sk >= 0) ? (ppix[sk]  & 0xff)
                               : (npix[-sk] & 0xff) ;
         }
      break ;

      case 2:                             /* 2 byte data goes into Image */
         if( border == MSBFirst ){        /* 22 Aug 1998 */
            for( i=0 ; i < hw ; i++ ){
               sk = sar[k++] ;
               if( sk >= 0 ){
                  *ptr++ = (ppix[sk] >> 8) & 0xff ;  /* MSB */
                  *ptr++ = (ppix[sk])      & 0xff ;  /* LSB */
               } else {
                  *ptr++ = (npix[-sk] >> 8) & 0xff ;
                  *ptr++ = (npix[-sk])      & 0xff ;
               }
            }
         } else {                          /* LSBFirst */
            for( i=0 ; i < hw ; i++ ){
               sk = sar[k++] ;
               if( sk >= 0 ){
                  *ptr++ = (ppix[sk])      & 0xff ;  /* LSB */
                  *ptr++ = (ppix[sk] >> 8) & 0xff ;  /* MSB */
               } else {
                  *ptr++ = (npix[-sk])      & 0xff ;
                  *ptr++ = (npix[-sk] >> 8) & 0xff ;
               }
            }
         }
      break ;

      case 3:                            /* 3 & 4 byte data: 22 Aug 1998 */
         if( border == MSBFirst ){
            for( i=0 ; i < hw ; i++ ){
               sk = sar[k++] ;
               if( sk >= 0 ){
                  *ptr++ = (ppix[sk] >> 16) & 0xff ;  /* MSB */
                  *ptr++ = (ppix[sk] >>  8) & 0xff ;
                  *ptr++ = (ppix[sk])       & 0xff ;  /* LSB */
               } else {
                  *ptr++ = (npix[-sk] >> 16) & 0xff ;
                  *ptr++ = (npix[-sk] >>  8) & 0xff ;
                  *ptr++ = (npix[-sk])       & 0xff ;
               }
            }
         } else {                          /* LSBFirst */
            for( i=0 ; i < hw ; i++ ){
               sk = sar[k++] ;
               if( sk >= 0 ){
                  *ptr++ = (ppix[sk])       & 0xff ;  /* LSB */
                  *ptr++ = (ppix[sk] >>  8) & 0xff ;
                  *ptr++ = (ppix[sk] >> 16) & 0xff ;  /* MSB */
               } else {
                  *ptr++ = (npix[-sk])       & 0xff ;
                  *ptr++ = (npix[-sk] >>  8) & 0xff ;
                  *ptr++ = (npix[-sk] >> 16) & 0xff ;
               }
            }
         }
      break ;

      case 4:
         if( border == MSBFirst ){
            for( i=0 ; i < hw ; i++ ){
               sk = sar[k++] ;
               if( sk >= 0 ){
                  *ptr++ = (ppix[sk] >> 24) & 0xff ;  /* MSB */
                  *ptr++ = (ppix[sk] >> 16) & 0xff ;
                  *ptr++ = (ppix[sk] >>  8) & 0xff ;
                  *ptr++ = (ppix[sk])       & 0xff ;  /* LSB */
               } else {
                  *ptr++ = (npix[-sk] >> 24) & 0xff ;
                  *ptr++ = (npix[-sk] >> 16) & 0xff ;
                  *ptr++ = (npix[-sk] >>  8) & 0xff ;
                  *ptr++ = (npix[-sk])       & 0xff ;
               }
            }
         } else {                          /* LSBFirst */
            for( i=0 ; i < hw ; i++ ){
               sk = sar[k++] ;
               if( sk >= 0 ){
                  *ptr++ = (ppix[sk])       & 0xff ;  /* LSB */
                  *ptr++ = (ppix[sk] >>  8) & 0xff ;
                  *ptr++ = (ppix[sk] >> 16) & 0xff ;
                  *ptr++ = (ppix[sk] >> 24) & 0xff ;  /* MSB */
               } else {
                  *ptr++ = (npix[-sk])       & 0xff ;
                  *ptr++ = (npix[-sk] >>  8) & 0xff ;
                  *ptr++ = (npix[-sk] >> 16) & 0xff ;
                  *ptr++ = (npix[-sk] >> 24) & 0xff ;
               }
            }
         }
      break ;

      default:
         fprintf(stderr,
                 "\n*** ILLEGAL value of display bytes/pix=%d in mri_to_XImage\n",
                 dc->byper);
         exit(1) ;
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

   register int iy, ex, ey, iW, iH, w2 ;
   char         *ximag;
   char         *Ep, *El, *Ip, *Il, *Id , *Ed ; /* d=data, l=row, p=pixel */
   int          Erow , Irow ;

   XImage * emage ;  /* to be output image */

   /*** sanity check ***/

   if( new_width <= 0 || new_height <= 0 ){
      fprintf(stderr ,
              "\n***ILLEGAL new width %d or height %d in resize\n",
              new_width , new_height ) ;
      exit(1) ;
   }

   /*** data about input image ***/

   iW = image->width ;                /* input width and height */
   iH = image->height ;

   if( iW == new_width && iH == new_height ){ /* very special case */
        return image ;
   }

   /*** create emage of the appropriate size ***/

   w2    = new_width * dc->byper ;
   ximag = (char *) XtMalloc( (size_t) (w2 * new_height) );

   if( ximag == NULL ){
      fprintf(stderr,"\n***CANNOT allocate memory for resizing XImage\n") ;
      exit(1) ;
   }

   emage = XCreateImage( dc->display , dc->visual , dc->depth ,
                         ZPixmap, 0, ximag, new_width,new_height, 8, w2 ) ;

   if( emage == NULL ){
      fprintf(stderr,"\n*** CANNOT create new XImage for resizing\n") ;
      exit(1) ;
   }

   /*** make lookup table for xnew -> xold ***/

   if( new_width > old_width ){
      lt = (int *) XtRealloc( (char *)lt,(size_t)(new_width * sizeof(int)) );
      old_width = new_width ;
   }

   for( ex=0 ; ex < new_width ; ex++ )
      lt[ex] = MAP_XY(ex,new_width,iW) * dc->byper ;

   /*** get ready to go ***/

   Ed = (char *) emage->data ; Erow = emage->bytes_per_line ;
   Id = (char *) image->data ; Irow = image->bytes_per_line ;

   switch( dc->byper ){

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

      case 3:                                 /* 3 & 4 added 22 Aug 1998 */
         for( ey=0 ; ey < new_height ; ey++ ){

            iy = MAP_XY(ey,new_height,iH) ;   /* row index in input image */
            Il = Id + Irow * iy ;             /* start of that row */
            El = Ed + Erow * ey ;             /* start of row in output */
            Ep = El ;
            for( ex=0 ; ex < new_width ; ex++ ){
               Ip = Il + lt[ex] ;             /* data pointer in input */
               *Ep++ = *Ip ;
               *Ep++ = *(Ip+1) ;
               *Ep++ = *(Ip+2) ;
            }
         }
      break ;

      case 4:
         for( ey=0 ; ey < new_height ; ey++ ){

            iy = MAP_XY(ey,new_height,iH) ;   /* row index in input image */
            Il = Id + Irow * iy ;             /* start of that row */
            El = Ed + Erow * ey ;             /* start of row in output */
            Ep = El ;
            for( ex=0 ; ex < new_width ; ex++ ){
               Ip = Il + lt[ex] ;             /* data pointer in input */
               *Ep++ = *Ip ;
               *Ep++ = *(Ip+1) ;
               *Ep++ = *(Ip+2) ;
               *Ep++ = *(Ip+3) ;
            }
         }
      break ;

      default:
         fprintf(stderr,"\n***ILLEGAL bytes/pix=%d for resizing\n",dc->byper) ;
         exit(1) ;
   }

   return emage ;
}

/*---------------------------------------------------------------------------
   Input = XImage (with pixels from dc)
           use_cmap == 0 ==> use only the Pixels in dc [the old way]
                    != 0 ==> use the entire colormap   [11 Feb 1999]
   Ouput = RGB or Grayscale image
-----------------------------------------------------------------------------*/

MRI_IMAGE * XImage_to_mri(  MCW_DC * dc , XImage * ximage , int use_cmap )
{
   int nx , ny , npix , ii,jj , kk , allgray , lsize ;
   Pixel pp ;
   byte * rgb , * gray ;
   byte rr,gg,bb ;
   byte * ptr ;
   XColor * xc ;
   MRI_IMAGE * outim ;
   int  border ;        /* 22 Aug 1998 */

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

   ptr = (byte *) ximage->data ;        /* pointer to pixels */

   rgb = (byte *) malloc( sizeof(byte) * 3*npix ) ;
   if( rgb == NULL ){
      fprintf(stderr,"\n*** malloc failure in XImage_to_mri\n") ;
      exit(1) ;
   }

   border = ximage->byte_order ;           /* 22 Aug 1998 */

   switch( dc->byper ){

      case 1:                              /* 1 byte per pixel */
         kk = 0 ; allgray = 1 ;
         for( jj=0 ; jj < ny ; jj++ ){
            for( ii=0 ; ii < nx ; ii++ ){
               pp = ptr[ii+jj*lsize] ;                       /* pixel value */
               xc = DCpix_to_XColor( dc , pp , use_cmap ) ;  /* XColor */
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
               if( border == MSBFirst )
                  pp = (ptr[2*ii+jj*lsize]   << 8) | ptr[2*ii+jj*lsize+1] ;
               else
                  pp = (ptr[2*ii+jj*lsize+1] << 8) | ptr[2*ii+jj*lsize] ;

               xc = DCpix_to_XColor( dc , pp , use_cmap ) ;
               rr = rgb[kk++] = INTEN_TO_BYTE( xc->red ) ;
               gg = rgb[kk++] = INTEN_TO_BYTE( xc->green ) ;
               bb = rgb[kk++] = INTEN_TO_BYTE( xc->blue ) ;
               allgray = allgray && (rr==gg) && (gg=bb) ;
            }
         }
      break ;

      case 3:                               /* 3 & 4 added 22 Aug 1998 */
         kk = 0 ; allgray = 1 ;
         for( jj=0 ; jj < ny ; jj++ ){
            for( ii=0 ; ii < nx ; ii++ ){
               if( border == MSBFirst )
                  pp = (ptr[3*ii+jj*lsize]   << 16) |
                       (ptr[3*ii+jj*lsize+1] <<  8) | ptr[3*ii+jj*lsize+2] ;
               else
                  pp = (ptr[3*ii+jj*lsize+2] << 16) |
                       (ptr[3*ii+jj*lsize+1] <<  8) | ptr[3*ii+jj*lsize] ;

               xc = DCpix_to_XColor( dc , pp , use_cmap ) ;
               rr = rgb[kk++] = INTEN_TO_BYTE( xc->red ) ;
               gg = rgb[kk++] = INTEN_TO_BYTE( xc->green ) ;
               bb = rgb[kk++] = INTEN_TO_BYTE( xc->blue ) ;
               allgray = allgray && (rr==gg) && (gg=bb) ;
            }
         }
      break ;

      case 4:
         kk = 0 ; allgray = 1 ;
         for( jj=0 ; jj < ny ; jj++ ){
            for( ii=0 ; ii < nx ; ii++ ){
               if( border == MSBFirst )
                  pp = (ptr[4*ii+jj*lsize]   << 24) | (ptr[4*ii+jj*lsize+1] << 16) |
                       (ptr[4*ii+jj*lsize+2] <<  8) |  ptr[4*ii+jj*lsize+3] ;
               else
                  pp = (ptr[4*ii+jj*lsize+3] << 24) | (ptr[4*ii+jj*lsize+2] << 16) |
                       (ptr[4*ii+jj*lsize+1] <<  8) |  ptr[4*ii+jj*lsize] ;

               xc = DCpix_to_XColor( dc , pp , use_cmap ) ;
               rr = rgb[kk++] = INTEN_TO_BYTE( xc->red ) ;
               gg = rgb[kk++] = INTEN_TO_BYTE( xc->green ) ;
               bb = rgb[kk++] = INTEN_TO_BYTE( xc->blue ) ;
               allgray = allgray && (rr==gg) && (gg=bb) ;
            }
         }
      break ;

      default:
         fprintf(stderr,
                 "\n*** ILLEGAL value of bytes/pix=%d in XImage_to_mri\n",
                 dc->byper);
         exit(1) ;
   }

   /*** if all pixels are gray, return a grayscale image ***/

   if( allgray ){

      gray = (byte *) malloc( sizeof(byte) * npix ) ;
      if( gray == NULL ){
         fprintf(stderr,"\n*** malloc failure in XImage_to_mri\n") ;
         exit(1) ;
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

/*-----------------------------------------------------------------------
   Convert an array of X11 Pixel values to an XImage for display.
   Adapted from mri_to_XImage by RWCox -- 11 Feb 1999
-------------------------------------------------------------------------*/

extern XImage * pixar_to_XImage( MCW_DC * dc, int nx, int ny, Pixel * par )
{
   int  w2, width, height , border ;
   unsigned char * Image ;
   XImage        * ximage ;
   register int i , hw  ;
   register unsigned char * ptr;

   /*-- sanity checks --*/

   if( dc == NULL || nx < 1 || ny < 1 || par == NULL ) return NULL ;

   width = nx ; height = ny ;

   w2 = width * dc->byper ;  /* rowlength in bytes */

   Image = (unsigned char *) XtMalloc( (size_t) (w2*height) );
   if( Image == NULL ) return NULL ;

   ximage = XCreateImage( dc->display , dc->visual , dc->depth ,
                          ZPixmap , 0 , Image , width,height , 8 , w2 ) ;
   if( ximage == NULL ){ XtFree((char *)Image) ; return NULL ; }

   border = ximage->byte_order ;  /* byte order */

   ptr = Image ;                  /* pointer to image bytes */
   hw  = height * width ;         /* total number of pixels */

   switch( dc->byper ){           /* load data into Image */

      case 1:                             /* 1 byte data goes into Image */
         for( i=0 ; i < hw ; i++ ){
            *ptr++ = par[i] & 0xff ;
         }
      break ;

      case 2:                             /* 2 byte data goes into Image */
         if( border == MSBFirst ){
            for( i=0 ; i < hw ; i++ ){
               *ptr++ = (par[i] >> 8) & 0xff ;  /* MSB */
               *ptr++ = (par[i]     ) & 0xff ;  /* LSB */
            }
         } else {                          /* LSBFirst */
            for( i=0 ; i < hw ; i++ ){
               *ptr++ = (par[i]     ) & 0xff ;  /* LSB */
               *ptr++ = (par[i] >> 8) & 0xff ;  /* MSB */
            }
         }
      break ;

      case 3:                            /* 3 byte data */
         if( border == MSBFirst ){
            for( i=0 ; i < hw ; i++ ){
               *ptr++ = (par[i] >> 16) & 0xff ;  /* MSB */
               *ptr++ = (par[i] >>  8) & 0xff ;
               *ptr++ = (par[i]      ) & 0xff ;  /* LSB */
            }
         } else {                           /* LSBFirst */
            for( i=0 ; i < hw ; i++ ){
               *ptr++ = (par[i]      ) & 0xff ;  /* LSB */
               *ptr++ = (par[i] >>  8) & 0xff ;
               *ptr++ = (par[i] >> 16) & 0xff ;  /* MSB */
            }
         }
      break ;

      case 4:                            /* 4 byte data */
         if( border == MSBFirst ){
            for( i=0 ; i < hw ; i++ ){
               *ptr++ = (par[i] >> 24) & 0xff ;  /* MSB */
               *ptr++ = (par[i] >> 16) & 0xff ;
               *ptr++ = (par[i] >>  8) & 0xff ;
               *ptr++ = (par[i]      ) & 0xff ;  /* LSB */
            }
         } else {                           /* LSBFirst */
            for( i=0 ; i < hw ; i++ ){
               *ptr++ = (par[i]      ) & 0xff ;  /* LSB */
               *ptr++ = (par[i] >>  8) & 0xff ;
               *ptr++ = (par[i] >> 16) & 0xff ;
               *ptr++ = (par[i] >> 24) & 0xff ;  /* MSB */
            }
         }
      break ;

      default:
         fprintf(stderr,
                 "\n*** ILLEGAL value of display bytes/pix=%d in pixar_to_XImage\n",
                 dc->byper);
         exit(1) ;
   }

   return ximage;
}

/*----------------------------------------------------------------------------
   Convert an MRI_IMAGE of rgb bytes to an XImage
------------------------------------------------------------------------------*/

#undef BRUTE_FORCE
#ifdef BRUTE_FORCE
XImage * rgb_to_XImage( MCW_DC * dc , MRI_IMAGE * im )
{
   int nxy , ii ;
   byte * rgb ;
   Pixel * par ;
   XImage * xim ;

   /*-- sanity check --*/

   if( dc == NULL || im == NULL || im->kind != MRI_rgb ) return NULL ;

   nxy = im->nx * im->ny ;
   rgb = MRI_RGB_PTR(im) ;

   par = (Pixel *) malloc( sizeof(Pixel) * nxy ) ; if( par == NULL ) return NULL ;

   for( ii=0 ; ii < nxy ; ii++ )
      par[ii] = DC_rgb_to_pixel( dc , rgb[3*ii], rgb[3*ii+1], rgb[3*ii+2] ) ;

   xim = pixar_to_XImage( dc , im->nx , im->ny , par ) ;

   free(par) ; return xim ;
}

#else /* not BRUTE_FORCE */

XImage * rgb_to_XImage( MCW_DC * dc , MRI_IMAGE * im )
{
   int nxy , ii , c ;
   byte * rgb , r,g,b ;
   Pixel * par , p ;
   XImage * xim ;
   int * col_ar , * ii_ar ;

   /*-- sanity check --*/

   if( dc == NULL || im == NULL || im->kind != MRI_rgb ) return NULL ;

   nxy = im->nx * im->ny ;
   rgb = MRI_RGB_PTR(im) ;

   col_ar = (int *)  malloc( sizeof(int)   * nxy ) ;
   ii_ar  = (int *)  malloc( sizeof(int)   * nxy ) ;
   par    = (Pixel *)malloc( sizeof(Pixel) * nxy ) ;

   if( col_ar == NULL )                             return NULL ;
   if( ii_ar  == NULL ){ free(col_ar);              return NULL; }
   if( par    == NULL ){ free(col_ar); free(ii_ar); return NULL; }

   for( ii=0 ; ii < nxy ; ii++ ){  /* convert RGB triples to ints */
      ii_ar[ii]  = ii ;            /* and save original location  */
      col_ar[ii] = rgb[3*ii] << 16 | rgb[3*ii+1] << 8 | rgb[3*ii+2] ;
   }

   qsort_intint( nxy , col_ar , ii_ar ) ; /* sort to bring like colors together */

   c = -1 ; /* a color that can't occur */

   for( ii=0 ; ii < nxy ; ii++ ){
      if( col_ar[ii] != c ){         /* have a new color, so compute its pixel */
         c = col_ar[ii] ;
         r = (c >> 16) & 0xff ; g = (c >> 8) & 0xff ; b = c & 0xff ;
         p = DC_rgb_to_pixel( dc , r,g,b ) ;
      }
      par[ii_ar[ii]] = p ;           /* store it where it came from */
   }

   free(col_ar) ; free(ii_ar) ;      /* toss some trash */

   xim = pixar_to_XImage( dc , im->nx , im->ny , par ) ;

   free(par) ; return xim ;
}
#endif /* BRUTE_FORCE */
