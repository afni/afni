#include "mrilib.h"
#include "mayo_analyze.h"

#if 0
/*---------------------------------------------------------------*/

static void swap_4(void *ppp)
{
   unsigned char *pntr = (unsigned char *) ppp ;
   unsigned char b0, b1, b2, b3;

   b0 = *pntr; b1 = *(pntr+1); b2 = *(pntr+2); b3 = *(pntr+3);
   *pntr = b3; *(pntr+1) = b2; *(pntr+2) = b1; *(pntr+3) = b0;
}

/*---------------------------------------------------------------*/

static void swap_8(void *ppp)
{
   unsigned char *pntr = (unsigned char *) ppp ;
   unsigned char b0, b1, b2, b3;
   unsigned char b4, b5, b6, b7;

   b0 = *pntr    ; b1 = *(pntr+1); b2 = *(pntr+2); b3 = *(pntr+3);
   b4 = *(pntr+4); b5 = *(pntr+5); b6 = *(pntr+6); b7 = *(pntr+7);

   *pntr     = b7; *(pntr+1) = b6; *(pntr+2) = b5; *(pntr+3) = b4;
   *(pntr+4) = b3; *(pntr+5) = b2; *(pntr+6) = b1; *(pntr+7) = b0;
}

/*---------------------------------------------------------------*/

static void swap_2(void *ppp)
{
   unsigned char *pntr = (unsigned char *) ppp ;
   unsigned char b0, b1;

   b0 = *pntr; b1 = *(pntr+1);
   *pntr = b1; *(pntr+1) = b0;
}

/*---------------------------------------------------------------*/

static void swap_analyze_hdr( struct dsr *pntr )
{
   swap_4(&pntr->hk.sizeof_hdr) ;
   swap_4(&pntr->hk.extents) ;
   swap_2(&pntr->hk.session_error) ;
   swap_2(&pntr->dime.dim[0]) ;
   swap_2(&pntr->dime.dim[1]) ;
   swap_2(&pntr->dime.dim[2]) ;
   swap_2(&pntr->dime.dim[3]) ;
   swap_2(&pntr->dime.dim[4]) ;
   swap_2(&pntr->dime.dim[5]) ;
   swap_2(&pntr->dime.dim[6]) ;
   swap_2(&pntr->dime.dim[7]) ;
#if 0
   swap_2(&pntr->dime.unused1) ;
#endif
   swap_2(&pntr->dime.datatype) ;
   swap_2(&pntr->dime.bitpix) ;
   swap_4(&pntr->dime.pixdim[0]) ;
   swap_4(&pntr->dime.pixdim[1]) ;
   swap_4(&pntr->dime.pixdim[2]) ;
   swap_4(&pntr->dime.pixdim[3]) ;
   swap_4(&pntr->dime.pixdim[4]) ;
   swap_4(&pntr->dime.pixdim[5]) ;
   swap_4(&pntr->dime.pixdim[6]) ;
   swap_4(&pntr->dime.pixdim[7]) ;
   swap_4(&pntr->dime.vox_offset) ;
   swap_4(&pntr->dime.funused1) ;
   swap_4(&pntr->dime.funused2) ;
   swap_4(&pntr->dime.cal_max) ;
   swap_4(&pntr->dime.cal_min) ;
   swap_4(&pntr->dime.compressed) ;
   swap_4(&pntr->dime.verified) ;
   swap_2(&pntr->dime.dim_un0) ;
   swap_4(&pntr->dime.glmax) ;
   swap_4(&pntr->dime.glmin) ;
   return ;
}
#endif

/*-----------------------------------------------------------------
  Write an image in ANALYZE 7.5 format;
  fname.hdr and fname.img will be written, in native byte order.
                                                29 Nov 2001 - RWCox
-------------------------------------------------------------------*/

void mri_write_analyze( char *fname , MRI_IMAGE *im )
{
   struct dsr hdr ;  /* ANALYZE .hdr */
   void *ip ;
   FILE *fp ;
   char *fff ;

ENTRY("mri_write_analyze") ;

   /*-- check inputs for sanity --*/

   if( fname == NULL || fname[0] == '\0' || im == NULL ) EXRETURN ;
   ip = mri_data_pointer(im) ;

   /*-- clear header --*/

   memset( &hdr , 0 , sizeof(struct dsr) ) ;

   /*-- set random header stuff --*/

   hdr.hk.sizeof_hdr = sizeof(struct dsr) ;
   hdr.hk.regular    = 'r' ;

   sprintf( hdr.hk.db_name   , "%.17s"           , fname ) ;
#if 0
   sprintf( hdr.hist.descrip , "via AFNI: %.68s" , fname ) ;
#endif

   /*-- set data dimensions --*/

   hdr.dime.dim[0] = MRI_DIMENSIONALITY(im) ;
   hdr.dime.dim[1] = im->nx ; hdr.dime.pixdim[1] = im->dx ;
   hdr.dime.dim[2] = im->ny ; hdr.dime.pixdim[2] = im->dy ;
   hdr.dime.dim[3] = im->nz ; hdr.dime.pixdim[3] = im->dz ;
   hdr.dime.dim[4] = im->nt ; hdr.dime.pixdim[4] = im->dt ;
   hdr.dime.dim[5] = im->nu ; hdr.dime.pixdim[5] = im->du ;
   hdr.dime.dim[6] = im->nv ; hdr.dime.pixdim[6] = im->dv ;
   hdr.dime.dim[7] = im->nw ; hdr.dime.pixdim[7] = im->dw ;

   hdr.dime.funused1 = 1.0 ;  /* SPM scale factor */

   /*-- set data type --*/

   switch( im->kind ){

      default: EXRETURN ;    /* bad */

      case MRI_byte:    hdr.dime.datatype = ANDT_UNSIGNED_CHAR; break;
      case MRI_short:   hdr.dime.datatype = ANDT_SIGNED_SHORT ; break;
      case MRI_int:     hdr.dime.datatype = ANDT_SIGNED_INT   ; break;
      case MRI_float:   hdr.dime.datatype = ANDT_FLOAT        ; break;
      case MRI_complex: hdr.dime.datatype = ANDT_COMPLEX      ; break;
      case MRI_double:  hdr.dime.datatype = ANDT_DOUBLE       ; break;
      case MRI_rgb:     hdr.dime.datatype = ANDT_RGB          ; break;
   }

   hdr.dime.bitpix = 8*im->pixel_size ;

   if( ip != NULL ){
     hdr.dime.glmin = mri_min( im ) ;
     hdr.dime.glmax = mri_max( im ) ;
   } else {
     hdr.dime.glmin = 0.0 ;
     hdr.dime.glmax = 0.0 ;
   }

   /*--KRH 03/11/04 writing out originator field from AFNI origin--*/
   /*- adding 1.5 to value for rounding (+0.5) and conversion      */
   /*                           to 1-based matlab arrays (+1.0)  - */
   /*--change abs() to -()                    25 Mar 2004  [rickr] */

  if( AFNI_yesenv("AFNI_ANALYZE_ORIGINATOR") ){
    short xyzuv[5] = {0};
    xyzuv[0] = -im->xo/im->dx + 1.5;
    xyzuv[1] = -im->yo/im->dy + 1.5;
    xyzuv[2] = -im->zo/im->dz + 1.5;
    memcpy( hdr.hist.originator, xyzuv, 10 );
  }

   /*-- write header --*/

   fff = AFMALL(char, strlen(fname)+16 ) ;

   sprintf(fff,"%s.hdr",fname) ;
   fp = fopen( fff , "wb" ) ;
   if( fp == NULL ){
      fprintf(stderr,"** Can't open file %s for output!\n",fff) ;
      free(fff) ; EXRETURN ;
   }
   fwrite( &hdr , sizeof(struct dsr) , 1 , fp ) ;
   fclose(fp) ;

   if( ip == NULL ){      /* 30 Sep 2002: skip .img if data not present */
     free(fff); EXRETURN;
   }

   /*-- write image --*/

   sprintf(fff,"%s.img",fname) ;
   fp = fopen( fff , "wb" ) ;
   if( fp == NULL ){
      fprintf(stderr,"** Can't open file %s for output!\n",fff) ;
      free(fff) ; EXRETURN ;
   }
   fwrite( ip , im->nvox , im->pixel_size , fp ) ;
   fclose(fp) ;

   free(fff) ; EXRETURN ;
}
