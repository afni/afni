/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/

#include "mrilib.h"

/*** 7D SAFE ***/

/* 28 Aug 1996: return value changed from void to int,
                which will be the number of files written to disk
                (either 0 or 1 at the present time).             */

int mri_write( char *fname , MRI_IMAGE *im )
{
   FILE  *imfile ;
   void  *data ;
   int   dsize , noheader = FALSE ;

ENTRY("mri_write") ;

   if( im == NULL ) RETURN(0) ;
   if( ! MRI_IS_2D(im) ){
      RETURN(mri_write_7D( fname , im )) ;
   }

   if( im->kind == MRI_rgb  ){ RETURN(mri_write_pnm( fname , im )) ; }
   if( im->kind == MRI_byte ){ RETURN(mri_write_pnm( fname , im )) ; }

   imfile = fopen( fname , "r" ) ;
   if( imfile != NULL ){
      fclose( imfile ) ;
      fprintf(stderr,"(FAILED) attempt to overwrite file %s\n",fname) ;
      RETURN(0) ;
   }

   imfile = fopen( fname , "w" ) ;

   if( imfile == NULL ){
      fprintf( stderr , "couldn't open for output file %s\n" , fname ) ;
      RETURN(0) ;
   }

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
   fclose( imfile ) ;

   RETURN(1) ;
}

/**************************************************************************/

int mri_write_7D( char *fname , MRI_IMAGE *im )
{
   FILE  *imfile ;
   void  *data ;

ENTRY("mri_write_7D") ;

   if( im == NULL ) RETURN( 0 );

   imfile = fopen( fname , "r" ) ;
   if( imfile != NULL ){
      fclose( imfile ) ;
      fprintf(stderr,"(FAILED) attempt to overwrite file %s\n",fname) ;
      RETURN( 0 );
   }

   imfile = fopen( fname , "w" ) ;

   if( imfile == NULL ){
      fprintf( stderr , "couldn't open for output file %s\n" , fname ) ;
      RETURN( 0 );
   }

   /*** write MR7 header ***/

   switch( MRI_DIMENSIONALITY(im) ){
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
   fclose( imfile ) ;

   RETURN( 1 );
}

/**************************************************************************/

int mri_write_pnm( char * fname , MRI_IMAGE * im )
{
   FILE  *imfile ;
   void  *data ;
   int   dsize , noheader = FALSE ;

ENTRY("mri_write_pnm") ;

   if( im == NULL ) RETURN( 0 );
   if( im->nz > 1 ) RETURN( 0 );
   if( im->kind != MRI_byte && im->kind != MRI_rgb ) RETURN( 0 );

   imfile = fopen( fname , "r" ) ;
   if( imfile != NULL ){
      fclose( imfile ) ;
      fprintf(stderr,"(FAILED) attempt to overwrite file %s\n",fname) ;
      RETURN( 0 );
   }

   imfile = fopen( fname , "w" ) ;
   if( imfile == NULL ){
      fprintf( stderr , "couldn't open for output file %s\n" , fname ) ;
      RETURN( 0 );
   }

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
   fclose( imfile ) ;
   RETURN( 1 );
}

/*---------------------------------------------------------------------------------------*/

int mri_write_1D( char * fname , MRI_IMAGE * im )  /* 16 Nov 1999 */
{
   MRI_IMAGE * fim ;
   int jj ;

ENTRY("mri_write_1D") ;

   if( fname == NULL || strlen(fname) == 0 ||
       im == NULL    || im->nz > 1           ) RETURN( 0 );

   fim = mri_transpose( im ) ;
   jj  = mri_write_ascii( fname , fim ) ;
   mri_free(fim) ;
   RETURN( jj );
}

/**-------------------------- Only good for 1D and 2D images ---------------------------**/

int mri_write_ascii( char * fname, MRI_IMAGE * im )
{
   int ii , jj , nx , ny ;
   FILE  *imfile ;

ENTRY("mri_write_ascii") ;

   if( fname == NULL || strlen(fname) == 0 ||
       im == NULL    || im->nz > 1           ) RETURN( 0 );

   if( strcmp(fname,"-") == 0 ){
     imfile = stdout ;
   } else {
     imfile = fopen( fname , "r" ) ;
     if( imfile != NULL ){
       fclose( imfile ) ;
       fprintf(stderr,"(FAILED) attempt to overwrite file %s\n",fname) ;
       RETURN( 0 );
     }
     imfile = fopen( fname , "w" ) ;
     if( imfile == NULL ){
       fprintf( stderr , "couldn't open for output file %s\n" , fname ) ;
       RETURN( 0 );
     }
   }

   nx = im->nx ; ny = im->ny ;

   for( jj=0 ; jj < ny ; jj++ ){

      switch( im->kind ){

         case MRI_float:{
            float * iar = MRI_FLOAT_PTR(im) + (jj*nx) ;
            for( ii=0 ; ii < nx ; ii++ )
               fprintf(imfile," %14.7g",iar[ii]) ;
         }
         break ;

         case MRI_short:{
            short * iar = MRI_SHORT_PTR(im) + (jj*nx) ;
            for( ii=0 ; ii < nx ; ii++ )
               fprintf(imfile," %6d",iar[ii]) ;
         }
         break ;

         case MRI_byte:{
            byte * iar = MRI_BYTE_PTR(im) + (jj*nx) ;
            for( ii=0 ; ii < nx ; ii++ )
               fprintf(imfile," %3d",iar[ii]) ;
         }
         break ;

         case MRI_int:{
            int * iar = MRI_INT_PTR(im) + (jj*nx) ;
            for( ii=0 ; ii < nx ; ii++ )
               fprintf(imfile," %6d",iar[ii]) ;
         }
         break ;
      }

      fprintf(imfile,"\n") ;
   }

   if( imfile != stdout ) fclose(imfile) ;
   RETURN( 1 );
}

/*------------------------------------------------------------
   05 Jan 2000: write raw data from image
--------------------------------------------------------------*/

int mri_write_raw( char *fname , MRI_IMAGE *im )
{
   FILE  *imfile ;
   void  *data ;
   int   dsize ;

ENTRY("mri_write_raw") ;

   if( im == NULL || fname == NULL || fname[0] == '\0' ) RETURN( 0 );

   dsize = im->pixel_size * im->nvox ;
   data = mri_data_pointer( im ) ;

   if( dsize <= 0 || data == NULL ) RETURN( 0 );

   imfile = fopen( fname , "w" ) ;

   if( imfile == NULL ){
      fprintf(stderr,"** Can't open for output: %s\n",fname) ; RETURN( 0 );
   }

   fwrite( data , 1 , dsize , imfile ) ;
   fclose( imfile ) ;
   RETURN( 1 );
}
