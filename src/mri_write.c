/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/

#include "mrilib.h"

/*** 7D SAFE ***/

/*-------------------------------------------------------------------------*/
/*! Open a file for writing, if practicable.  Use fclose_maybe() to close. */

static FILE * fopen_maybe( char *fname )  /* 05 Feb 2008 */
{
   FILE *imfile ;

   if( fname == NULL || *fname == '\0' ) return NULL ;  /* bad input */

   /* special case -- be sure not to fclose() stdout! */

   if( strcmp(fname,"-") == 0 || strcmp(fname,"-.1D")    == 0
                              || strcmp(fname,"stdout")  == 0 
                              || strcmp(fname,"stdout:") == 0 ) return stdout ;

   if( THD_is_ondisk(fname) ){   /* check for existing file */
     if( !THD_ok_overwrite() ){  /* if not allowed to overwrite */
       ERROR_message("(FAILED) attempt to over-write file %s",fname) ;
       return NULL ;
     } else {
       WARNING_message("over-writing file %s",fname) ;  /* tell the user */
     }
   }

   imfile = fopen(fname,"w") ;
   if( imfile == NULL ) ERROR_message("Can't open for output: %s",fname) ;
   return imfile ;
}

/*---------------------------------------------------------------------------*/

static void fclose_maybe( FILE *fp )  /* 05 Feb 2008 */
{
   if( fp != NULL && fp != stdout || fp != stderr ) fclose(fp) ;
   return ;
}

/*---------------------------------------------------------------------------*/
/* 28 Aug 1996: return value changed from void to int,
                which will be the number of files written to disk
                (either 0 or 1 at the present time).             */

int mri_write( char *fname , MRI_IMAGE *im )
{
   FILE  *imfile ;
   void  *data ;
   int   dsize , noheader = FALSE ;

ENTRY("mri_write") ;

   /* bad inputs? */

   if( im == NULL || fname == NULL || *fname == '\0' ) RETURN(0) ;

   /* special cases */

   if( ! MRI_IS_2D(im)      ){ RETURN(mri_write_7D ( fname , im )) ; }
   if( im->kind == MRI_rgb  ){ RETURN(mri_write_pnm( fname , im )) ; }
   if( im->kind == MRI_byte ){ RETURN(mri_write_pnm( fname , im )) ; }

   /* open the file for output */

   imfile = fopen_maybe(fname) ;
   if( imfile == NULL ) RETURN(0) ;

   /*** possibly write MRI header, unless a standard image type ***/

   dsize = im->pixel_size * im->nx * im->ny ;

   if( im->kind == MRI_short ){
      switch( dsize ){
         default: noheader = FALSE ; break ;

         case 8192:    /* raw 64x64 short */
         case 32768:   /* raw 128x128 short */
         case 131072:  /* raw 256x256 short */
         case 524288:  /* raw 512x512 short -- RWC 3/21/95 */
         case 2097152: /* raw 1024x1024 short -- RWC 3/21/95 */
            noheader = TRUE ;
            break ;
      }
   } else if( im->kind == MRI_byte ){
      switch( dsize ){
         default: noheader = FALSE ; break ;

         case 4096:    /* raw 64x64 byte -- RWC 3/21/95 */
         case 16384:   /* raw 128x128 byte -- RWC 3/21/95 */
         case 65536:   /* raw 256x256 8-bit -- Matthew Belmonte March 1995 */
         case 262144:  /* raw 512x512 byte -- RWC 3/21/95 */
         case 1048576: /* raw 1024x1024 byte -- RWC 3/21/95 */
            noheader = TRUE ;
            break ;
      }
   }

   if( !noheader ) fprintf( imfile , "MRI %d %d %d\n" ,
                                     im->kind , im->nx , im->ny ) ;

   /*** special case: add Signa 4.x header (of all zeros) ***/

   if( noheader && dsize == 131072 && im->kind == MRI_short ){
#define HEADER (28*256)
      int ii ;
      short * qq ;
      qq = (short *) malloc(sizeof(short)*HEADER) ;  /* header 256 x 256 */
      for( ii=0 ; ii < HEADER ; ii++ ) qq[ii] = 0 ;
      fwrite( qq , sizeof(short) , HEADER , imfile ) ;
      free(qq) ;
   }

   /*** write rest of data now ***/

   data = mri_data_pointer( im ) ;
   fwrite( data , im->pixel_size , im->nx * im->ny , imfile ) ;

   fclose_maybe(imfile) ;
   RETURN(1) ;
}

/**************************************************************************/

int mri_write_7D( char *fname , MRI_IMAGE *im )
{
   FILE  *imfile ;
   void  *data ;

ENTRY("mri_write_7D") ;

   if( im == NULL ) RETURN( 0 );

   imfile = fopen_maybe(fname) ;
   if( imfile == NULL ) RETURN(0) ;

   /*** write MR7 header ***/

   switch( mri_dimensionality(im) ){
      default:
      case 1:
         fprintf( imfile , "MR1 %d %d\n" ,
                  im->kind , im->nx ) ;
      break ;

      case 2:
         fprintf( imfile , "MR2 %d %d %d\n" ,
                  im->kind , im->nx,im->ny ) ;
      break ;

      case 3:
         fprintf( imfile , "MR2 %d %d %d %d\n" ,
                  im->kind , im->nx,im->ny,im->nz ) ;
      break ;

      case 4:
         fprintf( imfile , "MR2 %d %d %d %d %d\n" ,
                  im->kind , im->nx,im->ny,im->nz,im->nt ) ;
      break ;

      case 5:
         fprintf( imfile , "MR2 %d %d %d %d %d %d\n" ,
                  im->kind , im->nx,im->ny,im->nz,im->nt,im->nu ) ;
      break ;

      case 6:
         fprintf( imfile , "MR2 %d %d %d %d %d %d %d\n" ,
                  im->kind , im->nx,im->ny,im->nz,im->nt,im->nu,im->nv ) ;
      break ;

      case 7:
         fprintf( imfile , "MR2 %d %d %d %d %d %d %d %d\n" ,
                  im->kind , im->nx,im->ny,im->nz,im->nt,im->nu,im->nv,im->nw ) ;
      break ;
   }

   /*** write rest of data now ***/

   data = mri_data_pointer( im ) ;
   fwrite( data , im->pixel_size , im->nvox , imfile ) ;
   fclose_maybe(imfile) ;

   RETURN( 1 );
}

/**************************************************************************/

int mri_write_pnm( char *fname , MRI_IMAGE *im )
{
   FILE  *imfile ;
   void  *data ;
   int   dsize , noheader = FALSE ;

ENTRY("mri_write_pnm") ;

   if( im == NULL || fname == NULL || *fname == '\0' ) RETURN( 0 );
   if( im->nz > 1 ) RETURN( 0 );
   if( im->kind != MRI_byte && im->kind != MRI_rgb   ) RETURN( 0 );

   if( STRING_HAS_SUFFIX_CASE(fname,".jpg") ){   /* 15 Apr 2005: quick hack */
     RETURN( mri_write_jpg(fname,im) ) ;
   } else if( STRING_HAS_SUFFIX_CASE(fname,".png") ){  /* 11 Dec 2006 */
     RETURN( mri_write_png(fname,im) ) ;
   } else if( *fname == '|' ){                   /* 15 Dec 2006: pipe */
     RETURN( mri_write_filtered(fname+1,im) ) ;
   }

   imfile = fopen_maybe(fname) ;
   if( imfile == NULL ) RETURN(0) ;

   switch( im->kind ){

     case MRI_byte:
       fprintf( imfile , "P5\n%d %d\n255\n" , im->nx,im->ny ) ;     /* header */
       fwrite( MRI_BYTE_PTR(im), sizeof(byte), im->nvox, imfile ) ; /* bytes */
     break ;

     case MRI_rgb:
       fprintf( imfile , "P6\n%d %d\n255\n" , im->nx,im->ny ) ;      /* header */
       fwrite( MRI_RGB_PTR(im), sizeof(byte), 3*im->nvox, imfile ) ; /* bytes */
     break ;

   }

   fclose_maybe(imfile) ;
   RETURN( 1 );
}

/*---------------------------------------------------------------------------------------*/

int mri_write_1D( char *fname , MRI_IMAGE *im )  /* 16 Nov 1999 */
{
   MRI_IMAGE *fim ;
   int jj ;

ENTRY("mri_write_1D") ;

   if( im == NULL || im->nz > 1 ) RETURN( 0 ) ; /* stoopid user */

   fim = mri_transpose( im ) ;
   jj  = mri_write_ascii( fname , fim ) ;
   mri_free(fim) ;
   RETURN( jj );
}

/**------------------------ Only good for 1D and 2D images ---------------------------**/

int mri_write_ascii( char *fname, MRI_IMAGE *im )
{
   int ii , jj , nx , ny ;
   FILE  *imfile ;

ENTRY("mri_write_ascii") ;

   if( im == NULL || im->nz > 1 ) RETURN( 0 ) ; /* stoopid user */

   if( fname == NULL || *fname == '\0' ) fname = "-" ; /* to stdout */
   imfile = fopen_maybe(fname) ;
   if( imfile == NULL ) RETURN(0) ;

   nx = im->nx ; ny = im->ny ;

   for( jj=0 ; jj < ny ; jj++ ){

      switch( im->kind ){

         case MRI_float:{
           float *iar = MRI_FLOAT_PTR(im) + (jj*nx) ;
           for( ii=0 ; ii < nx ; ii++ )
             fprintf(imfile," %14g",iar[ii]) ;
         }
         break ;

         case MRI_short:{
           short *iar = MRI_SHORT_PTR(im) + (jj*nx) ;
           for( ii=0 ; ii < nx ; ii++ )
             fprintf(imfile," %6d",iar[ii]) ;
         }
         break ;

         case MRI_byte:{
           byte *iar = MRI_BYTE_PTR(im) + (jj*nx) ;
           for( ii=0 ; ii < nx ; ii++ )
             fprintf(imfile," %3d",iar[ii]) ;
         }
         break ;

         case MRI_int:{
           int *iar = MRI_INT_PTR(im) + (jj*nx) ;
           for( ii=0 ; ii < nx ; ii++ )
             fprintf(imfile," %6d",iar[ii]) ;
         }
         break ;

         case MRI_double:{
           double *iar = MRI_DOUBLE_PTR(im) + (jj*nx) ;
           for( ii=0 ; ii < nx ; ii++ )
             fprintf(imfile," %16g",iar[ii]) ;
         }
         break ;

         case MRI_complex:{
           complex *iar = MRI_COMPLEX_PTR(im) + (jj*nx) ;
           for( ii=0 ; ii < nx ; ii++ )
             fprintf(imfile," %-1.7g;%-1.7g",iar[ii].r,iar[ii].i) ;
         }
         break ;

         case MRI_rgb:{
           byte *iar = MRI_RGB_PTR(im) + (3*jj*nx) ;
           for( ii=0 ; ii < nx ; ii++ )
             fprintf(imfile," %3d %3d %3d",iar[3*ii],iar[3*ii+1],iar[3*ii+2]) ;
         }
         break ;
      }

      fprintf(imfile,"\n") ;
   }

   fclose_maybe(imfile) ;
   RETURN( 1 );
}

/*------------------------------------------------------------
   05 Jan 2000: write raw data from image
--------------------------------------------------------------*/

int mri_write_raw( char *fname , MRI_IMAGE *im )
{
   FILE *imfile ;
   void *data ;
   int  dsize ;

ENTRY("mri_write_raw") ;

   if( im == NULL || fname == NULL || fname[0] == '\0' ) RETURN( 0 );

   dsize = im->pixel_size * im->nvox ;
   data  = mri_data_pointer( im ) ;

   if( dsize <= 0 || data == NULL ) RETURN( 0 );

   if( THD_is_file(fname) )
     WARNING_message("Over-writing file %s",fname) ;

   imfile = fopen_maybe(fname) ;
   if( imfile == NULL ) RETURN(0) ;

   fwrite( data , 1 , dsize , imfile ) ;
   fclose_maybe( imfile ) ;
   RETURN( 1 );
}

/*---------------------------------------------------------------*/

#include <signal.h>

int mri_write_jpg( char *fname , MRI_IMAGE *im )  /* 15 Apr 2005 */
{
   char *pg , *jpfilt, *eee ;
   FILE *fp ;
   int jpeg_compress;

   if( fname == NULL || *fname == '\0' || im == NULL ) return 0 ;
   if( im->kind != MRI_rgb && im->kind != MRI_byte   ) return 0 ;

   if( STRING_HAS_SUFFIX_CASE(fname,".png") ){  /* 07 Dec 2007 */
     RETURN( mri_write_png(fname,im) ) ;
   }

   pg = THD_find_executable( "cjpeg" ) ;
   if( pg == NULL ) return 0 ;
   /* user environment variable compression quality - mod 5/10/2006 drg */
   eee = my_getenv("AFNI_JPEG_COMPRESS");
   if(eee!=NULL){
     jpeg_compress = strtod(eee, NULL);
     if((jpeg_compress<=0) || (jpeg_compress>100)) jpeg_compress = 95;
   }
   else jpeg_compress = 95;

   jpfilt = (char *)malloc( sizeof(char)*(strlen(pg)+strlen(fname)+32) ) ;
   sprintf( jpfilt , "%s -quality %d > %s" , pg , jpeg_compress, fname ) ;
#ifndef CYGWIN
   signal( SIGPIPE , SIG_IGN ) ;
#endif
   fp = popen( jpfilt , "w" ) ;
   if( fp == NULL ){ free((void *)jpfilt); return 0; }

   if( im->kind == MRI_rgb ){
     fprintf(fp,"P6\n%d %d\n255\n" , im->nx,im->ny ) ;
     fwrite( MRI_RGB_PTR(im), sizeof(byte), 3*im->nvox, fp ) ;
   } else if( im->kind == MRI_byte ){
     fprintf(fp,"P5\n%d %d\n255\n" , im->nx,im->ny ) ;
     fwrite( MRI_BYTE_PTR(im), sizeof(byte), im->nvox, fp ) ;
   }
   (void) pclose(fp) ; free((void *)jpfilt) ; return 1 ;
}

/*---------------------------------------------------------------*/

int mri_write_png( char *fname , MRI_IMAGE *im )  /* 11 Dec 2006 */
{
   char *pg , *pgfilt ;
   FILE *fp ;

   if( fname == NULL || *fname == '\0' || im == NULL ) return 0 ;
   if( im->kind != MRI_rgb && im->kind != MRI_byte   ) return 0 ;

   if( STRING_HAS_SUFFIX_CASE(fname,".jpg") ){  /* 07 Dec 2007 */
     RETURN( mri_write_jpg(fname,im) ) ;
   }

   pg = THD_find_executable( "pnmtopng" ) ; if( pg == NULL ) return 0 ;
   pgfilt = (char *)malloc( sizeof(char)*(strlen(pg)+strlen(fname)+32) ) ;
   sprintf( pgfilt , "%s -compression 9 > %s" , pg , fname ) ;
#ifndef CYGWIN
   signal( SIGPIPE , SIG_IGN ) ;
#endif
   fp = popen( pgfilt , "w" ) ;
   if( fp == NULL ){ free((void *)pgfilt); return 0; }

   if( im->kind == MRI_rgb ){
     fprintf(fp,"P6\n%d %d\n255\n" , im->nx,im->ny ) ;
     fwrite( MRI_RGB_PTR(im), sizeof(byte), 3*im->nvox, fp ) ;
   } else if( im->kind == MRI_byte ){
     fprintf(fp,"P5\n%d %d\n255\n" , im->nx,im->ny ) ;
     fwrite( MRI_BYTE_PTR(im), sizeof(byte), im->nvox, fp ) ;
   }
   (void) pclose(fp) ; free((void *)pgfilt) ; return 1 ;
}

/*---------------------------------------------------------------*/

int mri_write_filtered( char *fname , MRI_IMAGE *im )  /* 15 Dec 2006 */
{
   FILE *fp ;

   if( fname == NULL || im == NULL )                   return 0 ;
   if( im->kind != MRI_rgb && im->kind != MRI_byte   ) return 0 ;
   if( *fname == '|' ) fname++ ;   /* skip pipe character, if present */
   if( *fname == '\0' )                                return 0 ;

#ifndef CYGWIN
   signal( SIGPIPE , SIG_IGN ) ;
#endif
   fp = popen( fname , "w" ) ; if( fp == NULL ) return 0 ;

   if( im->kind == MRI_rgb ){
     fprintf(fp,"P6\n%d %d\n255\n" , im->nx,im->ny ) ;
     fwrite( MRI_RGB_PTR(im), sizeof(byte), 3*im->nvox, fp ) ;
   } else if( im->kind == MRI_byte ){
     fprintf(fp,"P5\n%d %d\n255\n" , im->nx,im->ny ) ;
     fwrite( MRI_BYTE_PTR(im), sizeof(byte), im->nvox, fp ) ;
   }
   (void) pclose(fp) ;
   return 1 ;
}
