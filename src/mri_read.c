/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/

/*! \file
    This file contains all the functions for reading image files.
    It is primarily used by to3d.c.  It reads 2D images (into MRI_IMAGE struct)
    and arrays of 2D images (into MRI_IMARR struct).
*/

#include <stdio.h>
#include <ctype.h>
#include <string.h>
#include <sys/stat.h>

/*** for non ANSI compilers ***/

#ifndef SEEK_END
#define SEEK_END 2
#endif

#ifndef SEEK_SET
#define SEEK_SET 0
#endif

#include "mrilib.h"

/*! Global variable to signal image orientation, if possible. */

char MRILIB_orients[8] = "\0" ;  /* 12 Mar 2001 */

/*! Global variable to signal image slice offset, if possible. */

float MRILIB_zoff      = 0.0 ;

/*! Global variable to signal image TR, if possible. */

float MRILIB_tr        = 0.0 ;   /* 03 Dec 2001 */

/*! Global variable to signal image x offset, if possible. */

float MRILIB_xoff      = 0.0 ;   /* 07 Dec 2001 */

/*! Global variable to signal image y offset, if possible. */

float MRILIB_yoff      = 0.0 ;

/*** 7D SAFE (but most routines only return 2D images!) ***/

MRI_IMAGE *mri_try_mri( FILE * , int * ) ;  /* prototypes */
MRI_IMAGE *mri_try_7D ( FILE * , int * ) ;
MRI_IMAGE *mri_try_pgm( FILE * , int * ) ;

/*-----------------------------------------------------------------*/

/*! \brief Database of preset file sizes for auto-use of 3D.

  If( head < 0 ) then file length must match size
  else file length must == n*size + head for some
       integer n, which will be the number of slices
       to read from the file.
*/

typedef struct {
   int size ;       /*!< file size in bytes if head < 0; image size in bytes if head >= 0 */
   int head ;       /*!< global header size if >= 0 */
   char * prefix ;  /*!< character string to prefix to filename */
} MCW_imsize ;

/*! Max number of preset file sizes to allow. */

#define MAX_MCW_IMSIZE 99

/*! Array of preset file sizes to use when reading image files. */

static MCW_imsize imsize[MAX_MCW_IMSIZE] ;

/*! If this < 0 ==> must initialize array of preset file sizes. */

static int MCW_imsize_good = -1 ;

/*---------------------------------------------------------------*/

/*! Swap the 4 bytes pointed to by ppp: abcd -> dcba. */

static void swap_4(void *ppp)
{
   unsigned char *pntr = (unsigned char *) ppp ;
   unsigned char b0, b1, b2, b3;

   b0 = *pntr; b1 = *(pntr+1); b2 = *(pntr+2); b3 = *(pntr+3);
   *pntr = b3; *(pntr+1) = b2; *(pntr+2) = b1; *(pntr+3) = b0;
}

/*---------------------------------------------------------------*/

/*! Swap the 8 bytes pointed to by ppp: abcdefgh -> hgfedcba. */

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

/*! Swap the 2 bytes pointed to by ppp: ab -> ba. */

static void swap_2(void *ppp)
{
   unsigned char *pntr = (unsigned char *) ppp ;
   unsigned char b0, b1;

   b0 = *pntr; b1 = *(pntr+1);
   *pntr = b1; *(pntr+1) = b0;
}

/******************************************************************/

/*! \brief Earliest image reading function in the AFNI package.

    \param  fname is the name of the file to try to read
    \return NULL if an image couldn't be read, otherwise
            a pointer to an MRI_IMAGE with data, dimensions, etc.
*/

MRI_IMAGE *mri_read( char *fname )
{
   FILE      *imfile ;
   MRI_IMAGE *im ;
   int       length , skip=0 , swap=0 ;
   void      *data ;

WHOAMI ;

   imfile = fopen( fname , "r" ) ;
   if( imfile == NULL ){
      fprintf( stderr , "couldn't open image file %s\n" , fname ) ;
      return NULL ;
   }

   fseek( imfile , 0L , SEEK_END ) ;  /* get the length of the file */
   length = ftell( imfile ) ;         /* (the AJ way) */

   /*--- 03 Dec 2001: check for GEMS format file "IMGF"   ---*/
   /*[[[ Information herein from Medical Image Format FAQ ]]]*/

   { char str[5]="AFNI" ;
     int nx , ny , bpp , cflag , hdroff ;
     rewind(imfile) ; fread(str,1,4,imfile) ;      /* check for "IMGF" */

     if( str[0]=='I' && str[1]=='M' && str[2]=='G' && str[3]=='F' ){

       fread( &skip , 4,1, imfile ) ;  /* read next 5 ints */
       fread( &nx   , 4,1, imfile ) ;
       fread( &ny   , 4,1, imfile ) ;
       fread( &bpp  , 4,1, imfile ) ;
       fread( &cflag, 4,1, imfile ) ;

       if( nx < 0 || nx > 8192 ){      /* maybe have to byte swap 5 ints */
         swap = 1 ;                    /* flag that we are swapping data */
         swap_4(&skip); swap_4(&nx) ;
         swap_4(&ny)  ; swap_4(&bpp); swap_4(&cflag);
       }
       if( nx < 0 || nx > 8192 || ny < 0 || ny > 8192 ) goto The_Old_Way ;
       if( skip < 0  || skip  >= length )               goto The_Old_Way ;
       if( bpp != 16 || cflag != 1      )               goto The_Old_Way ;

       /* make image space */

       im = mri_new( nx , ny , MRI_short ) ;

       /* try to read image auxiliary data as well (not mandatory) */

       length = fseek( imfile , 148L , SEEK_SET ) ; /* magic GEMS offset */
       if( length == 0 ){
          fread( &hdroff , 4,1 , imfile ) ;  /* location of image header */
          if( swap ) swap_4(&hdroff) ;
          if( hdroff > 0 ){                  /* read from image header */
             float dx,dy,dz , xyz[9] , zz ; int itr , ii,jj,kk ;
             static int nzoff=0 ;
             static float zoff ;

             /* get voxel grid sizes */

             fseek( imfile , hdroff+26 , SEEK_SET ) ;
             fread( &dz , 4,1 , imfile ) ;

             fseek( imfile , hdroff+50 , SEEK_SET ) ;
             fread( &dx , 4,1 , imfile ) ;
             fread( &dy , 4,1 , imfile ) ;

             if( swap ){ swap_4(&dx); swap_4(&dy); swap_4(&dz); }

             /* save into image header */

             if( dx > 0.0 && dy > 0.0 && dz > 0.0 ){
               im->dx = dx; im->dy = dy; im->dz = dz; im->dw = 1.0;
             }

             /* grid orientation: from 3 sets of LPI corner coordinates: */
             /*   xyz[0..2] = top left hand corner of image     (TLHC)   */
             /*   xyz[3..5] = top right hand corner of image    (TRHC)   */
             /*   xyz[6..8] = bottom right hand corner of image (BRHC)   */
             /* GEMS coordinate orientation here is LPI                  */
             /* Orientation is saved into global string MRILIB_orients   */

             fseek( imfile , hdroff+154 , SEEK_SET ) ;
             fread( xyz , 4,9 , imfile ) ;
             if( swap ) swap_fourbytes(9,xyz) ;

             /* x-axis orientation */
             /* ii determines which spatial direction is x-axis  */
             /* and is the direction that has the biggest change */
             /* between the TLHC and TRHC                        */

             dx = fabs(xyz[3]-xyz[0]) ; ii = 1 ;
             dy = fabs(xyz[4]-xyz[1]) ; if( dy > dx ){ ii=2; dx=dy; }
             dz = fabs(xyz[5]-xyz[2]) ; if( dz > dx ){ ii=3;        }
             dx = xyz[ii+2]-xyz[ii-1] ; if( dx < 0. ){ ii = -ii;    }
             switch( ii ){
               case  1: MRILIB_orients[0] = 'L'; MRILIB_orients[1] = 'R'; break;
               case -1: MRILIB_orients[0] = 'R'; MRILIB_orients[1] = 'L'; break;
               case  2: MRILIB_orients[0] = 'P'; MRILIB_orients[1] = 'A'; break;
               case -2: MRILIB_orients[0] = 'A'; MRILIB_orients[1] = 'P'; break;
               case  3: MRILIB_orients[0] = 'I'; MRILIB_orients[1] = 'S'; break;
               case -3: MRILIB_orients[0] = 'S'; MRILIB_orients[1] = 'I'; break;
               default: MRILIB_orients[0] ='\0'; MRILIB_orients[1] ='\0'; break;
             }

             /* y-axis orientation */
             /* jj determines which spatial direction is y-axis  */
             /* and is the direction that has the biggest change */
             /* between the BRHC and TRHC                        */

             dx = fabs(xyz[6]-xyz[3]) ; jj = 1 ;
             dy = fabs(xyz[7]-xyz[4]) ; if( dy > dx ){ jj=2; dx=dy; }
             dz = fabs(xyz[8]-xyz[5]) ; if( dz > dx ){ jj=3;        }
             dx = xyz[jj+5]-xyz[jj+2] ; if( dx < 0. ){ jj = -jj;    }
             switch( jj ){
               case  1: MRILIB_orients[2] = 'L'; MRILIB_orients[3] = 'R'; break;
               case -1: MRILIB_orients[2] = 'R'; MRILIB_orients[3] = 'L'; break;
               case  2: MRILIB_orients[2] = 'P'; MRILIB_orients[3] = 'A'; break;
               case -2: MRILIB_orients[2] = 'A'; MRILIB_orients[3] = 'P'; break;
               case  3: MRILIB_orients[2] = 'I'; MRILIB_orients[3] = 'S'; break;
               case -3: MRILIB_orients[2] = 'S'; MRILIB_orients[3] = 'I'; break;
               default: MRILIB_orients[2] ='\0'; MRILIB_orients[3] ='\0'; break;
             }

             MRILIB_orients[6] = '\0' ;   /* terminate orientation string */

             kk = 6 - abs(ii)-abs(jj) ;   /* which spatial direction is z-axis */
                                          /* where 1=L-R, 2=P-A, 3=I-S */
             zz = xyz[kk-1] ;             /* z-coordinate of this slice */

             /* getting orientation of z-axis requires 2 images in a row -*/

             if( nzoff == 0 ){  /* from 1st GEMS image */

               zoff = zz ;      /* save this for 2nd image calculation */
               switch( kk ){    /* may be changed on second image */
                case  1: MRILIB_orients[4] = 'L'; MRILIB_orients[5] = 'R'; break;
                case  2: MRILIB_orients[4] = 'P'; MRILIB_orients[5] = 'A'; break;
                case  3: MRILIB_orients[4] = 'I'; MRILIB_orients[5] = 'S'; break;
                default: MRILIB_orients[4] ='\0'; MRILIB_orients[5] ='\0'; break;
               }

             } else if( nzoff == 1 ){   /* from 2nd GEMS image */

               float qoff = zz - zoff ;  /* vive la difference */
               if( qoff < 0 ) kk = -kk ; /* kk determines z-axis orientation */
               switch( kk ){
                case  1: MRILIB_orients[4] = 'L'; MRILIB_orients[5] = 'R'; break;
                case -1: MRILIB_orients[4] = 'R'; MRILIB_orients[5] = 'L'; break;
                case  2: MRILIB_orients[4] = 'P'; MRILIB_orients[5] = 'A'; break;
                case -2: MRILIB_orients[4] = 'A'; MRILIB_orients[5] = 'P'; break;
                case  3: MRILIB_orients[4] = 'I'; MRILIB_orients[5] = 'S'; break;
                case -3: MRILIB_orients[4] = 'S'; MRILIB_orients[5] = 'I'; break;
                default: MRILIB_orients[4] ='\0'; MRILIB_orients[5] ='\0'; break;
               }

               /* save spatial offset of first slice         */
               /* [this is positive in the direction of the] */
               /* [-z axis, so may need to change its sign ] */

               MRILIB_zoff = zoff ;
               if( kk == 1 || kk == 2 || kk == 3 ) MRILIB_zoff = -MRILIB_zoff ;

               MRILIB_xoff = xyz[abs(ii)-1] ;  /* 07 Dec 2001 */
               MRILIB_yoff = xyz[abs(jj)-1] ;
               if( ii == 1 || ii == 2 || ii == 3 ) MRILIB_xoff = -MRILIB_xoff ;
               if( jj == 1 || jj == 2 || jj == 3 ) MRILIB_yoff = -MRILIB_yoff ;
             }
             nzoff++ ;  /* 3rd and later images don't count for z-orientation */

             /* get TR, save into global variable */

             if( MRILIB_tr <= 0.0 ){
               fseek( imfile , hdroff+194 , SEEK_SET ) ;
               fread( &itr , 4,1 , imfile ) ;
               if( swap ) swap_4(&itr) ;
               MRILIB_tr = im->dt = 1.0e-6 * itr ;
             }
          }
       } /* end of trying to read image header */

       goto Ready_To_Roll ;  /* skip to the reading place */
     }
   } /* end of GEMS */

   /*--- OK, do it the old way ---*/

The_Old_Way:

#if 0
   MRILIB_orients[0] = '\0' ; MRILIB_zoff = MRILIB_tr = 0.0 ;  /* 03 Dec 2001 */
#endif

   switch( length ){

      case 512:    /* raw 16x16 short -- RWCox: 06 Dec 2001 */
         im = mri_new( 16 , 16 , MRI_short ) ;
         break ;

      case 2048:   /* raw 32x32 short -- RWCox: 19 Sep 2000 */
         im = mri_new( 32 , 32 , MRI_short ) ;
         break ;

      case 4096:   /* raw 64x64 byte -- RWC 3/21/95 */
         im = mri_new( 64 , 64 , MRI_byte ) ;
         break ;

      case 8192:   /* raw 64x64 short */
      case 16096:  /* with Signa 5.x header */
         im = mri_new( 64 , 64 , MRI_short ) ;
         skip = length - 8192 ;
         break ;

#if 0
      case 18432:  /* raw 96x96 short */
         im = mri_new( 96 , 96 , MRI_short ) ;
         break ;
#endif

      case 16384:  /* raw 128x128 byte -- RWC 3/21/95 */
         im = mri_new( 128 , 128 , MRI_byte ) ;
         break ;

      case 32768:  /* raw 128x128 short */
      case 40672:  /* with Signa 5.x header */
         im = mri_new( 128 , 128 , MRI_short ) ;
         skip = length - 32768 ;
         break ;

      case 65536:  /* raw 256x256 8-bit -- Matthew Belmonte March 1995 */
         im = mri_new( 256 , 256 , MRI_byte ) ;
         break ;

      case 131072:  /* raw 256x256 short */
      case 138976:  /* Signa 5.x */
      case 145408:  /* Signa 4.x */

         im   = mri_new( 256 , 256 , MRI_short ) ;
         skip = length - 131072 ;
         break ;

#if 0
      case 262144:  /* raw 256x256 float */
         im = mri_new( 256 , 256 , MRI_float ) ;
         break ;
#else
      case 262144:  /* raw 512x512 byte -- RWC 3/21/95 */
         im = mri_new( 512 , 512 , MRI_byte ) ;
         break ;

      case 524288:  /* raw 512x512 short -- RWC 3/21/95 */
         im = mri_new( 512 , 512 , MRI_short ) ;
         break ;

      case 1048576: /* raw 1024x1024 byte -- RWC 3/21/95 */
         im = mri_new( 1024 , 1024 , MRI_byte ) ;
         break ;

      case 2097152: /* raw 1024x1024 short -- RWC 3/21/95 */
         im = mri_new( 1024 , 1024 , MRI_short ) ;
         break ;
#endif

      /** not a canonical length: try something else **/

      default:
                          im = mri_try_mri( imfile , &skip ) ;  /* Cox format */
         if( im == NULL ) im = mri_try_7D ( imfile , &skip ) ;  /* 7D format  */
         if( im == NULL ) im = mri_try_pgm( imfile , &skip ) ;  /* PGM format */
         if( im != NULL ) break ;

         fclose( imfile ) ; /* close it, since we failed (so far) */

         im = mri_read_ascii( fname ) ;    /* list of ASCII numbers */
         if( im != NULL ) return im ;

         im = mri_read_ppm( fname ) ;      /* 15 Apr 1999 */
         if( im != NULL ) return im ;

         fprintf( stderr , "do not recognize image file %s\n" , fname );
         fprintf( stderr , "length seen as %d\n" , length ) ;
         return NULL ;
   }

   /*-- Actually read the data from disk --*/

Ready_To_Roll:

   data = mri_data_pointer( im ) ;

   length = fseek( imfile , skip , SEEK_SET ) ;
   if( length != 0 ){
      fprintf( stderr , "mri_read error in skipping in file %s\n" , fname ) ;
      mri_free( im ) ;
      return NULL ;
   }

   length = fread( data , im->pixel_size , im->nvox , imfile ) ;
   fclose( imfile ) ;

   if( length != im->nvox ){
      mri_free( im ) ;
      fprintf( stderr , "couldn't read image data from file %s\n" , fname ) ;
      return NULL ;
   }

   mri_add_name( fname , im ) ;

   /*-- 03 Dec 2001: maybe need to swap bytes --*/

   if( swap ){
     switch( im->pixel_size ){
       default: break ;
       case 2:  swap_twobytes (   im->nvox, data ) ; break ;  /* short */
       case 4:  swap_fourbytes(   im->nvox, data ) ; break ;  /* int, float */
       case 8:  swap_fourbytes( 2*im->nvox, data ) ; break ;  /* complex */
     }
   }

   return im ;
}

/*********************************************************************/

#if 0
#define NUMSCAN(var)                                                       \
   { while( isspace(ch) ) {ch = getc(imfile) ;}                            \
     if(ch == '#') do{ch = getc(imfile) ;}while(ch != '\n' && ch != EOF) ; \
     if(ch =='\n') ch = getc(imfile) ;                                     \
     for( nch=0 ; isdigit(ch) ; nch++,ch=getc(imfile) ) {buf[nch] = ch ;}  \
     buf[nch]='\0';                                                        \
     var = strtol( buf , NULL , 10 ) ; }
#else

/*! Skip comments in a PPM file. */

#define SKIPCOM                                                            \
    {if(ch == '#') do{ch = getc(imfile) ;}while(ch != '\n' && ch != EOF);}

/*! Scan for a number in the imfile stream. */

#define NUMSCAN(var)                                                       \
   { SKIPCOM ;                                                             \
     while( ch!=EOF && !isdigit(ch) ){ch = getc(imfile); SKIPCOM; }        \
     for( nch=0 ; isdigit(ch) ; nch++,ch=getc(imfile) ) {buf[nch] = ch ;}  \
     buf[nch]='\0';                                                        \
     var = strtol( buf , NULL , 10 ) ; }
#endif

/*! \brief Try to read an file in the "Cox MRI" format.

    \param imfile is a pointer to an open FILE.
    \param skip is a pointer to an int that will be set to the number
           of bytes to skip from the file start to find the image data
    \return NULL if the file doesn't work for "Cox MRI" format;
            otherwise, the return is a pointer to an MRI_IMAGE ready
            to have its data read from imfile.
*/

MRI_IMAGE *mri_try_mri( FILE *imfile , int *skip )
{
   int ch , nch , nx,ny,imcode ;
   char buf[64] ;
   MRI_IMAGE *im ;

   fseek( imfile , 0 , SEEK_SET ) ;  /* rewind file */

   ch = getc( imfile ) ; if( ch != 'M' ) return NULL ;  /* check for MRI */
   ch = getc( imfile ) ; if( ch != 'R' ) return NULL ;
   ch = getc( imfile ) ; if( ch != 'I' ) return NULL ;

   /* magic MRI found, so read numbers */

   ch = getc(imfile) ;

   NUMSCAN(imcode) ;
   NUMSCAN(nx) ;  if( nx <= 0 ) return NULL ;
   NUMSCAN(ny) ;  if( ny <= 0 ) return NULL ;

   *skip = ftell(imfile) ;
   im    = mri_new( nx , ny , imcode ) ;
   return im ;
}

/**************************************************************************
   7D format: MRn kind n-dimensions data, where 'n' = 1-7.
***************************************************************************/

/*! \brief Try to read a "Cox nD MRI" image file (fat chance). */

MRI_IMAGE *mri_try_7D( FILE *imfile , int *skip )
{
   int ch , nch , nx,ny,nz,nt,nu,nv,nw , imcode , ndim ;
   char buf[64] ;
   MRI_IMAGE *im ;

   fseek( imfile , 0 , SEEK_SET ) ;  /* rewind file */

   ch = getc( imfile ) ; if( ch != 'M' ) return NULL ;  /* check for MR[1-7] */
   ch = getc( imfile ) ; if( ch != 'R' ) return NULL ;
   ch = getc( imfile ) ;
   switch( ch ){
      default:  return NULL ;   /* not what I expected */

      case '1': ndim = 1 ; break ;
      case '2': ndim = 2 ; break ;
      case '3': ndim = 3 ; break ;
      case '4': ndim = 4 ; break ;
      case '5': ndim = 5 ; break ;
      case '6': ndim = 6 ; break ;
      case '7': ndim = 7 ; break ;
   }
   /* magic MR? found, so read numbers */

   ch = getc(imfile) ;
   NUMSCAN(imcode) ;

   nx = ny = nz = nt = nu = nv = nw = 1 ;

                   NUMSCAN(nx) ;  if( nx <= 0 ) return NULL ;
   if( ndim > 1 ){ NUMSCAN(ny) ;  if( ny <= 0 ) return NULL ; }
   if( ndim > 2 ){ NUMSCAN(nz) ;  if( nz <= 0 ) return NULL ; }
   if( ndim > 3 ){ NUMSCAN(nt) ;  if( nt <= 0 ) return NULL ; }
   if( ndim > 4 ){ NUMSCAN(nu) ;  if( nu <= 0 ) return NULL ; }
   if( ndim > 5 ){ NUMSCAN(nv) ;  if( nv <= 0 ) return NULL ; }
   if( ndim > 6 ){ NUMSCAN(nw) ;  if( nw <= 0 ) return NULL ; }

   *skip = ftell(imfile) ;
   im    = mri_new_7D_generic( nx,ny,nz,nt,nu,nv,nw , imcode , TRUE ) ;
   return im ;
}


/*********************************************************************/

/*! \brief Try to read a raw PGM format image file.

    \param imfile is a pointer to an open FILE
    \param skip is a pointer to an int; *skip will be set to the
           byte offset at which to start reading data
    \return A pointer to an MRI_IMAGE ready to have its data read in
            (if the file is a PGM file), or NULL.
*/

MRI_IMAGE *mri_try_pgm( FILE *imfile , int *skip )
{
   int ch , nch , nx,ny,maxval ;
   char buf[64] ;
   MRI_IMAGE *im ;

   fseek( imfile , 0 , SEEK_SET ) ;  /* rewind file */

   ch = getc( imfile ) ; if( ch != 'P' ) return NULL ;  /* check for magic */
   ch = getc( imfile ) ; if( ch != '5' ) return NULL ;

   /* magic P5 found, so read numbers */

   ch = getc(imfile) ;

   NUMSCAN(nx)     ; if( nx     <= 0 ) return NULL ;
   NUMSCAN(ny)     ; if( ny     <= 0 ) return NULL ;
   NUMSCAN(maxval) ; if( maxval <= 0 || maxval >  255 ) return NULL ;

   *skip = ftell(imfile) ;
   im    = mri_new( nx , ny , MRI_byte ) ;
   return im ;
}

/*--------------------------------------------------------------
   Read a pile of images from one file.
   Modified 4/4/95 to read short or byte data.
   Modified 10/02/95 to allow byte swapping with 3Ds:
   Modified 11/06/95 to allow float images with 3Df:
                 and to allow int images with 3Di:
                 and to allow complex images with 3Dc:

   [N.B.: if this routine is altered, don't forget mri_imcount!]
----------------------------------------------------------------*/

/*! \brief Read one or more 2D slices from a "3D:" formatted image file. */

MRI_IMARR * mri_read_3D( char * tname )
{
   int hglobal , himage , nx , ny , nz ;
   char fname[256] , buf[512] ;
   int ngood , length , kim , koff , datum_type , datum_len , swap ;
   MRI_IMARR * newar ;
   MRI_IMAGE * newim ;
   void      * imar ;
   FILE      * imfile ;

   /*** get info from 3D tname ***/

   if( tname == NULL || strlen(tname) < 10 ) return NULL ;

   switch( tname[2] ){  /* allow for 3D: or 3Ds: or 3Db: */

      default:
      case ':':
         ngood = sscanf( tname , "3D:%d:%d:%d:%d:%d:%s" ,
                         &hglobal , &himage , &nx , &ny , &nz , fname ) ;

         swap       = 0 ;
         datum_type = MRI_short ;
         datum_len  = sizeof(short) ;  /* better be 2 */
         break ;

      case 's':
         ngood = sscanf( tname , "3Ds:%d:%d:%d:%d:%d:%s" ,
                         &hglobal , &himage , &nx , &ny , &nz , fname ) ;

         swap       = 1 ;
         datum_type = MRI_short ;
         datum_len  = sizeof(short) ;  /* better be 2 */
         break ;

      case 'b':
         ngood = sscanf( tname , "3Db:%d:%d:%d:%d:%d:%s" ,
                         &hglobal , &himage , &nx , &ny , &nz , fname ) ;

         swap       = 0 ;
         datum_type = MRI_byte ;
         datum_len  = sizeof(byte) ;  /* better be 1 */
         break ;

      case 'f':
         ngood = sscanf( tname , "3Df:%d:%d:%d:%d:%d:%s" ,
                         &hglobal , &himage , &nx , &ny , &nz , fname ) ;

         swap       = 0 ;
         datum_type = MRI_float ;
         datum_len  = sizeof(float) ;  /* better be 4 */
         break ;

      case 'i':
         ngood = sscanf( tname , "3Di:%d:%d:%d:%d:%d:%s" ,
                         &hglobal , &himage , &nx , &ny , &nz , fname ) ;

         swap       = 0 ;
         datum_type = MRI_int ;
         datum_len  = sizeof(int) ;  /* better be 4 */
         break ;

      case 'c':
         ngood = sscanf( tname , "3Dc:%d:%d:%d:%d:%d:%s" ,
                         &hglobal , &himage , &nx , &ny , &nz , fname ) ;

         swap       = 0 ;
         datum_type = MRI_complex ;
         datum_len  = sizeof(complex) ;  /* better be 8 */
         break ;
   }

   if( ngood < 6 || himage < 0 ||
       nx <= 0   || ny <= 0    || nz <= 0 ||
       strlen(fname) <= 0                   ) return NULL ;   /* bad info */

   /*** 06 Mar 2001: special case of fname ***/

   if( strcmp(fname,"ALLZERO") == 0 ){
      INIT_IMARR(newar) ;
      for( kim=0 ; kim < nz ; kim++ ){
         newim = mri_new( nx , ny , datum_type ) ;
         imar  = mri_data_pointer( newim ) ;
         memset( imar , 0 , newim->nvox * newim->pixel_size ) ;
         sprintf( buf , "%s#%d" , fname,kim ) ;
         mri_add_name( buf , newim ) ;
         ADDTO_IMARR(newar,newim) ;
      }
      return newar ;
   }

   /*** open the input file and position it ***/

   imfile = fopen( fname , "r" ) ;
   if( imfile == NULL ){
      fprintf( stderr , "couldn't open image file %s\n" , fname ) ;
      return NULL ;
   }

   fseek( imfile , 0L , SEEK_END ) ;  /* get the length of the file */
   length = ftell( imfile ) ;

   /** 13 Apr 1999: modified to allow actual hglobal < -1
                    as long as hglobal+himage >= 0       **/

#if 0                 /* the old code */
   if( hglobal < 0 ){
      hglobal = length - nz*(datum_len*nx*ny+himage) ;
      if( hglobal < 0 ) hglobal = 0 ;
   }
#else                 /* 13 Apr 1999 */
   if( hglobal == -1 || hglobal+himage < 0 ){
      hglobal = length - nz*(datum_len*nx*ny+himage) ;
      if( hglobal < 0 ) hglobal = 0 ;
   }
#endif

   ngood = hglobal + nz*(datum_len*nx*ny+himage) ;
   if( length < ngood ){
      fprintf( stderr,
        "image file %s is %d bytes long but must be at least %d bytes long\n"
        "for hglobal=%d himage=%d nx=%d ny=%d nz=%d and voxel=%d bytes\n",
        fname,length,ngood,hglobal,himage,nx,ny,nz,datum_len ) ;
      fclose( imfile ) ;
      return NULL ;
   }

   /*** read images from the file ***/

   INIT_IMARR(newar) ;

   for( kim=0 ; kim < nz ; kim++ ){
      koff = hglobal + (kim+1)*himage + datum_len*nx*ny*kim ;
      fseek( imfile , koff , SEEK_SET ) ;

      newim  = mri_new( nx , ny , datum_type ) ;
      imar   = mri_data_pointer( newim ) ;
      length = fread( imar , datum_len , nx * ny , imfile ) ;
      if( swap ) mri_swapbytes( newim ) ;

      if( nz == 1 ) mri_add_name( fname , newim ) ;
      else {
         sprintf( buf , "%s#%d" , fname,kim ) ;
         mri_add_name( buf , newim ) ;
      }

      ADDTO_IMARR(newar,newim) ;
   }

   fclose(imfile) ;
   return newar ;
}

/*--------------------------------------------------------------*/

/*! \brief Read one or more 2D images from a file.

   This function is the main point of input for to3d.c.
   \param fname is the name of the file to read.  This file
          might be in one of these formats:
           - "3D:" format (implicitly or explicitly)
           - "3A:" format
           - *.hdr (ANALYZE 2D-4D) format
           - *.ima (Siemens 2D array) format
           - I.*   (GEMS) format
           - PGM format
           - PPM format
           - List of ASCII numbers
           - pre-defined 2D file size in mri_read()
           - "Cox MRI" (god help you, no one else can)

   \return A pointer to an array of 2D images.  If nothing
           could be read, NULL is returned.
*/

MRI_IMARR * mri_read_file( char * fname )
{
   MRI_IMARR * newar ;
   MRI_IMAGE * newim ;
   char * new_fname ;

   new_fname = imsized_fname( fname ) ;
   if( new_fname == NULL ) return NULL ;

   if( strlen(new_fname) > 9 && new_fname[0] == '3' && new_fname[1] == 'D' ){

      newar = mri_read_3D( new_fname ) ;   /* read from a 3D file */

   } else if( strlen(new_fname) > 9 &&
              new_fname[0] == '3' && new_fname[1] == 'A' && new_fname[3] == ':' ){

      newar = mri_read_3A( new_fname ) ;

   } else if( strstr(new_fname,".hdr") != NULL ||
              strstr(new_fname,".HDR") != NULL   ){  /* 05 Feb 2001 */

      newar = mri_read_analyze75( new_fname ) ;

   } else if( strstr(new_fname,".ima") != NULL ||
              strstr(new_fname,".IMA") != NULL   ){  /* 12 Mar 2001 */

      newar = mri_read_siemens( new_fname ) ;

   } else {
      newim = mri_read( new_fname ) ;      /* read from a 2D file */
      if( newim == NULL ){ free(new_fname) ; return NULL ; }
      INIT_IMARR(newar) ;
      ADDTO_IMARR(newar,newim) ;
   }
   free(new_fname) ;
   return newar ;
}

/*-----------------------------------------------------------------*/

/*! \brief Like mri_read_file(), but will only return 1 2D image.

    If the input file has more than 1 slice, or cannot be read,
    then NULL is returned.
*/

MRI_IMAGE * mri_read_just_one( char * fname )
{
   MRI_IMARR * imar ;
   MRI_IMAGE * im ;
   char * new_fname ;

   new_fname = imsized_fname( fname ) ;
   if( new_fname == NULL ) return NULL ;

   imar = mri_read_file( new_fname ) ; free(new_fname) ;
   if( imar == NULL ) return NULL ;
   if( imar->num != 1 ){ DESTROY_IMARR(imar) ; return NULL ; }
   im = IMAGE_IN_IMARR(imar,0) ;
   FREE_IMARR(imar) ;
   return im ;
}

/*-----------------------------------------------------------------
  return a count of how many 2D images will be read from this file
-------------------------------------------------------------------*/

/*! \brief Return a count of how many 2D images are in a file.

    Used by to3d.c to figure out how many slices will be read
    later using mri_read_file().  Return value is 0 if the images
    can't be counted.  If you add a new file type to mri_read_file(),
    then you need to modify this function as well!
*/

static int mri_imcount_analyze75( char * ) ;  /* prototype */
static int mri_imcount_siemens( char * ) ;

int mri_imcount( char * tname )
{
   int hglobal , himage , nx , ny , nz , ngood ;
   char fname[256]="\0" ;
   char * new_fname ;

   if( tname == NULL ) return 0 ;
   new_fname = imsized_fname( tname ) ;
   if( new_fname == NULL ) return 0 ;

   /*** a 3D filename ***/

   if( strlen(new_fname) > 9 && new_fname[0] == '3' && new_fname[1] == 'D' ){

      switch( new_fname[2] ){

         default:
         case ':':
            ngood = sscanf( new_fname , "3D:%d:%d:%d:%d:%d:%s" ,
                            &hglobal , &himage , &nx , &ny , &nz , fname ) ;
            break ;

         case 's':
            ngood = sscanf( new_fname , "3Ds:%d:%d:%d:%d:%d:%s" ,
                            &hglobal , &himage , &nx , &ny , &nz , fname ) ;
            break ;

         case 'b':
            ngood = sscanf( new_fname , "3Db:%d:%d:%d:%d:%d:%s" ,
                            &hglobal , &himage , &nx , &ny , &nz , fname ) ;
            break ;

         case 'f':
            ngood = sscanf( new_fname , "3Df:%d:%d:%d:%d:%d:%s" ,
                            &hglobal , &himage , &nx , &ny , &nz , fname ) ;
            break ;

         case 'i':
            ngood = sscanf( new_fname , "3Di:%d:%d:%d:%d:%d:%s" ,
                            &hglobal , &himage , &nx , &ny , &nz , fname ) ;
            break ;

         case 'c':
            ngood = sscanf( new_fname , "3Dc:%d:%d:%d:%d:%d:%s" ,
                            &hglobal , &himage , &nx , &ny , &nz , fname ) ;
            break ;
      }

      free( new_fname ) ;
      if( ngood < 6 || himage < 0 ||
          nx <= 0   || ny <= 0    || nz <= 0 ||
          strlen(fname) <= 0                       ) return 0 ;
      else                                           return nz ;
   }

   /*** a 3A filename ***/

   if( strlen(new_fname) > 9 &&
       new_fname[0] == '3' && new_fname[1] == 'A' && new_fname[3] == ':' ){

      switch( new_fname[2] ){

         default: ngood = 0 ; break ;

         case 's':
            ngood = sscanf( new_fname, "3As:%d:%d:%d:%s", &nx, &ny, &nz, fname ) ;
            break ;

         case 'b':
            ngood = sscanf( new_fname, "3Ab:%d:%d:%d:%s", &nx, &ny, &nz, fname ) ;
            break ;

         case 'f':
            ngood = sscanf( new_fname, "3Af:%d:%d:%d:%s", &nx, &ny, &nz, fname ) ;
            break ;
      }

      free( new_fname ) ;
      if( ngood < 4 || nx <= 0 || ny <= 0 || nz <= 0 || strlen(fname) <= 0 ) return 0 ;
      else                                                                   return nz ;
   }

   /*** 05 Feb 2001: deal with ANALYZE .hdr files ***/

   if( strstr(new_fname,".hdr") != NULL ||
       strstr(new_fname,".HDR") != NULL   ){

      return mri_imcount_analyze75( new_fname ) ;
   }

   if( strstr(new_fname,".ima") != NULL ||
       strstr(new_fname,".IMA") != NULL   ){        /* 12 Mar 2001 */

      return mri_imcount_siemens( new_fname ) ;
   }

   /*** not a 3D filename ***/

   free( new_fname ) ;
   return 1 ;
}

/*--------------------------------------------------------------*/

/*! \brief Like mri_read_file(), but returns images from many files.

    \param nf = Number of file names
    \param fn = Array of file name strings
    \return An array of 2D images (NULL if nothing was found)

    Added 07 Mar 1995
*/

MRI_IMARR * mri_read_many_files( int nf , char * fn[] )
{
   MRI_IMARR * newar , * outar ;
   int kf , ii ;

   if( nf <= 0 ) return NULL ;  /* no inputs! */
   INIT_IMARR(outar) ;          /* initialize output array */

   for( kf=0 ; kf < nf ; kf++ ){
      newar = mri_read_file( fn[kf] ) ;  /* read all images in this file */

      if( newar == NULL ){  /* none?  flush the output array! */
         fprintf(stderr,"cannot read images from file %s\n",fn[kf]) ;
         for( ii=0 ; ii < outar->num ; ii++ ) mri_free(outar->imarr[ii]) ;
         FREE_IMARR(outar) ;
         return NULL ;
      }

      for( ii=0 ; ii < newar->num ; ii++ )  /* move images to output array */
         ADDTO_IMARR( outar , newar->imarr[ii] ) ;

      FREE_IMARR(newar) ;  /* don't need this no more */
   }
   return outar ;
}

/*---------------------------------------------------------------*/

/*! \brief Read a raw PPM file into 3 byte-valued MRI_IMAGEs.

    \date 16 May 1995
*/
-----------------------------------------------------------------*/

MRI_IMARR * mri_read_ppm3( char * fname )
{
   int ch , nch , nx,ny,maxval , length , npix,ii ;
   char buf[512] ;
   MRI_IMAGE *rim , *gim , *bim ;
   MRI_IMARR * outar ;
   FILE * imfile ;
   byte * rby , * gby , * bby , * rgby ;

WHOAMI ;

   /*** open input file ***/

   imfile = fopen( fname , "r" ) ;
   if( imfile == NULL ){
      fprintf(stderr,"couldn't open file %s in mri_read_ppm3\n",fname); return NULL ;
   }

   /*** check if a raw PPM file ***/

   ch = getc( imfile ) ; if( ch != 'P' ) { fclose(imfile) ; return NULL ; }
   ch = getc( imfile ) ; if( ch != '6' ) { fclose(imfile) ; return NULL ; }

   /* magic P6 found, so read numbers in header */

   ch = getc(imfile) ;

   NUMSCAN(nx)     ; if( nx     <= 0 )   { fclose(imfile) ; return NULL ; }
   NUMSCAN(ny)     ; if( ny     <= 0 )   { fclose(imfile) ; return NULL ; }
   NUMSCAN(maxval) ; if( maxval <= 0 ||
                         maxval >  255 ) { fclose(imfile) ; return NULL ; }

   /*** create output images and workspace array ***/

   rim = mri_new( nx , ny , MRI_byte ) ; rby = mri_data_pointer( rim ) ;
   gim = mri_new( nx , ny , MRI_byte ) ; gby = mri_data_pointer( gim ) ;
   bim = mri_new( nx , ny , MRI_byte ) ; bby = mri_data_pointer( bim ) ;

   sprintf(buf,"%s#R",fname) ; mri_add_name( buf , rim ) ;
   sprintf(buf,"%s#G",fname) ; mri_add_name( buf , gim ) ;
   sprintf(buf,"%s#B",fname) ; mri_add_name( buf , bim ) ;

   rgby = (byte *) malloc( sizeof(byte) * 3*nx*ny ) ;
   if( rgby == NULL ){
      fprintf(stderr,"couldn't malloc workspace in mri_read_ppm3!\n") ; EXIT(1) ;
   }

   /*** read all data into workspace array ***/

   length = fread( rgby , sizeof(byte) , 3*nx*ny , imfile ) ;
   fclose( imfile ) ;

   if( length != 3*nx*ny ){
      free(rgby) ; mri_free(rim) ; mri_free(gim) ; mri_free(bim) ;
      fprintf(stderr,"couldn't read data from file %s in mri_read_ppm3\n",fname) ;
      return NULL ;
   }

   /*** put data from workspace array into output images ***/

   npix = nx*ny ;
   for( ii=0 ; ii < npix ; ii++ ){
      rby[ii] = rgby[3*ii  ] ;
      gby[ii] = rgby[3*ii+1] ;
      bby[ii] = rgby[3*ii+2] ;
   }
   free( rgby ) ;

   /*** create output image array ***/

   INIT_IMARR(outar) ;
   ADDTO_IMARR( outar , rim ) ;
   ADDTO_IMARR( outar , gim ) ;
   ADDTO_IMARR( outar , bim ) ;
   return outar ;
}

/*-----------------------------------------------------------------
   routines added 1 Oct 1995
-------------------------------------------------------------------*/

/*! \brief Read 1 2D image, then "nsize" it - make it a power of 2 in sizes.

    This was developed in the days when FD/FD2/fim ruled the world, and
    those programs (AJ's legacy) only deal with square images that are
    a power of 2 in size.
    \date 01 Oct 1995
*/

MRI_IMAGE *mri_read_nsize( char * fname )
{
   MRI_IMARR *imar ;
   MRI_IMAGE *imout ;

   imar = mri_read_file( fname ) ;
   if( imar == NULL ) return NULL ;
   if( imar->num != 1 ){ DESTROY_IMARR(imar) ; return NULL ; }

   imout = mri_nsize( IMAGE_IN_IMARR(imar,0) ) ;
   mri_add_name( IMAGE_IN_IMARR(imar,0)->name , imout ) ;

   DESTROY_IMARR(imar) ;
   return imout ;
}

/*! \brief Read many 2D images from many files. */

MRI_IMARR *mri_read_many_nsize( int nf , char * fn[] )
{
   MRI_IMARR * newar , * outar ;
   MRI_IMAGE * im ;
   int ii ;

   newar = mri_read_many_files( nf , fn ) ;
   if( newar == NULL ) return NULL ;

   INIT_IMARR(outar) ;
   for( ii=0 ; ii < newar->num ; ii++ ){
      im = mri_nsize( IMAGE_IN_IMARR(newar,ii) ) ;
      mri_add_name( IMAGE_IN_IMARR(newar,ii)->name , im ) ;
      ADDTO_IMARR(outar,im) ;
      mri_free( IMAGE_IN_IMARR(newar,ii) ) ;
   }
   FREE_IMARR(newar) ;
   return outar ;
}

/*------------------------------------------------------------------------*/

/*! \brief Set up MCW_SIZE_# database for input.

    This implements the facility for the user to define MCW_IMSIZE_1
    (or AFNI_IMSIZE_1) et cetera, for pre-defining a relationship between
    a file size in bytes and a 3D: prefix.  This function is only called
    once to setup the table.
    \date 07 Nov 95
*/

void init_MCW_sizes(void)
{
   int num , count ;
   char ename[32] ;
   char * str ;

   if( MCW_imsize_good >= 0 ) return ;

   MCW_imsize_good = 0 ;

   for( num=0 ; num < MAX_MCW_IMSIZE ; num++ ){ /* look for environment string */

      imsize[num].size = -1 ;

      /* try to find environment variable with the num-th name */

      sprintf( ename , "AFNI_IMSIZE_%d" , num+1 ) ;
      str = my_getenv( ename ) ;

      if( str == NULL ){
         sprintf( ename , "MCW_IMSIZE_%d" , num+1 ) ;
         str = my_getenv( ename ) ;
         if( str == NULL ) continue ;
      }

      imsize[num].prefix = (char *) malloc( sizeof(char) * strlen(str) ) ;
      if( imsize[num].prefix == NULL ){
         fprintf(stderr,"\n*** Can't malloc in init_MCW_sizes! ***\a\n");
         EXIT(1) ;
      }

      if( str[0] != '%' ){  /* e.g., 16096=3D:-1:0:64:64:1: */

         imsize[num].head = -1 ;
         count = sscanf( str , "%d=%s" , &(imsize[num].size) , imsize[num].prefix ) ;
         if( count != 2 || imsize[num].size < 2 || strlen(imsize[num].prefix) < 2 ){
            free( imsize[num].prefix ) ;
            fprintf(stderr,"bad environment %s = %s\n" ,
                    ename , str ) ;
         }

      } else {              /* e.g., %16096+0=3D:0:7904:64:64: */

         count = sscanf( str+1 , "%d+%d=%s" ,
                         &(imsize[num].size) , &(imsize[num].head) , imsize[num].prefix ) ;

         if( count != 3 || imsize[num].size < 2 ||
             imsize[num].head < 0 || strlen(imsize[num].prefix) < 2 ){

            free( imsize[num].prefix ) ;
            fprintf(stderr,"bad environment %s = %s\n" ,
                    ename , str ) ;
         }
      }

      MCW_imsize_good ++ ;
   }

   return ;
}

/*------------------------------------------------------------------------------*/
/*! \brief My version of strdup(), which won't fail if the input is NULL. */

char * my_strdup( char * str )
{
   char * new_str ;
   if( str == NULL ) return NULL ;
   new_str = (char *) malloc( sizeof(char) * (strlen(str)+1) ) ;
   if( new_str != NULL ) strcpy( new_str , str ) ;
   return new_str ;
}

/*------------------------------------------------------------------------------*/

/*! \brief Check if a filesize fits an MCW_IMSIZE setup.

    \param fname = Filename
    \return A new "filename" with 3D header attached if it fits.
            If not, return a copy of the filename.  In any case the
            returned string should be free()-d when it is no longer needed.
*/

char * imsized_fname( char * fname )
{
   int num , lll ;
   long len ;
   char * new_name ;

   init_MCW_sizes() ;
   if( MCW_imsize_good == 0 ){
      new_name = my_strdup(fname) ;  /* nothing to fit */
      return new_name ;              /* --> return copy of old name */
   }

   len = mri_filesize( fname ) ;
   if( len <= 0 ){
      new_name = my_strdup(fname) ;  /* not an existing filename */
      return new_name ;              /* --> return copy of old name */
   }

   for( num=0 ; num < MAX_MCW_IMSIZE ; num++ ){     /* check each possibility */

      if( imsize[num].size <= 0 ) continue ;        /* skip to next one */

      if( imsize[num].head < 0 && len == imsize[num].size ){  /* fixed size fit */

         lll = strlen(fname) + strlen(imsize[num].prefix) + 4 ;
         new_name = (char *) malloc( sizeof(char) * lll ) ;
         if( new_name == NULL ){
            fprintf(stderr,"\n*** Can't malloc in imsized_fname! ***\a\n");
            EXIT(1) ;
         }
         sprintf( new_name , "%s%s" , imsize[num].prefix , fname ) ;
         return new_name ;

      } else if( (len-imsize[num].head) % imsize[num].size == 0 ){
         int count = (len-imsize[num].head) / imsize[num].size ;

         if( count < 1 ) continue ;  /* skip to next one */

         lll = strlen(fname) + strlen(imsize[num].prefix) + 32 ;
         new_name = (char *) malloc( sizeof(char) * lll ) ;
         if( new_name == NULL ){
            fprintf(stderr,"\n*** Can't malloc in imsized_fname! ***\a\n");
            EXIT(1) ;
         }
         sprintf( new_name , "%s%d:%s" , imsize[num].prefix , count , fname ) ;
         return new_name ;
      }

   }

   new_name = my_strdup(fname) ;  /* no fit --> return copy of old name */
   return new_name ;
}

/*------------------------------------------------------------------------*/
/*!\brief Return the size of a file in bytes.

  \param pathname = input filename
  \return File length if file exists; -1 if it doesn't.
  \see THD_filesize() in thd_filestuff.c.
*/

long mri_filesize( char * pathname )
{
   static struct stat buf ;
   int ii ;

   if( pathname == NULL ) return -1 ;
   ii = stat( pathname , &buf ) ; if( ii != 0 ) return -1 ;
   return buf.st_size ;
}

/*---------------------------------------------------------------*/

/*!\brief Read the header from PPM file and return its info.

  \param fname = file name
  \return *nx and *ny are set to the image dimensions;
          if they are set to 0, something bad happened
          (e.g., the file isn't a PPM file, or doesn't exist).
  \date 17 Sep 2001
*/

void mri_read_ppm_header( char *fname , int *nx, int *ny )
{
   FILE *imfile ;
   int ch , nch , nxx,nyy ;
   char buf[256] ;

ENTRY("mri_read_ppm_header") ;

   if( fname == NULL || nx == NULL || ny == NULL ) EXRETURN ;

   *nx = *ny = 0 ;  /* default returns */

   /*** open input file ***/

   imfile = fopen( fname , "r" ) ; if( imfile == NULL ) EXRETURN ;

   /*** check if a raw PPM file ***/

   ch = getc( imfile ) ; if( ch != 'P' ) { fclose(imfile) ; EXRETURN ; }
   ch = getc( imfile ) ; if( ch != '6' ) { fclose(imfile) ; EXRETURN ; }

   /* magic P6 found, so read numbers in header */

   ch = getc(imfile) ;

   NUMSCAN(nxx) ; if( nxx <= 0 ){ fclose(imfile) ; EXRETURN ; }
   NUMSCAN(nyy) ; if( nyy <= 0 ){ fclose(imfile) ; EXRETURN ; }

   /* return dimensions */

   fclose(imfile) ; *nx = nxx ; *ny = nyy ; EXRETURN ;
}

/*---------------------------------------------------------------
  May 13, 1996: reads a raw PPM file into 1 image
-----------------------------------------------------------------*/

MRI_IMAGE * mri_read_ppm( char * fname )
{
   int ch , nch , nx,ny,maxval , length ;
   MRI_IMAGE * rgbim ;
   FILE      * imfile ;
   byte      * rgby ;
   char        buf[256] ;

WHOAMI ;

   /*** open input file ***/

   imfile = fopen( fname , "r" ) ;
   if( imfile == NULL ) return NULL ;

   /*** check if a raw PPM file ***/

   ch = getc( imfile ) ; if( ch != 'P' ) { fclose(imfile) ; return NULL ; }
   ch = getc( imfile ) ; if( ch != '6' ) { fclose(imfile) ; return NULL ; }

   /* magic P6 found, so read numbers in header */

   ch = getc(imfile) ;

   NUMSCAN(nx)    ; if( nx     <= 0 )  { fclose(imfile); return NULL; }
   NUMSCAN(ny)    ; if( ny     <= 0 )  { fclose(imfile); return NULL; }
   NUMSCAN(maxval); if( maxval <= 0 ||
                        maxval >  255 ){ fclose(imfile); return NULL; }

   /*** create output image ***/

   rgbim = mri_new( nx , ny , MRI_rgb ) ; mri_add_name( fname , rgbim ) ;
   rgby  = MRI_RGB_PTR(rgbim) ;

   /*** read all data into image array */

   length = fread( rgby , sizeof(byte) , 3*nx*ny , imfile ) ;
   fclose( imfile ) ;

   if( length != 3*nx*ny ){ mri_free(rgbim) ; return NULL ; }

   /* 17 Sep 2001: scale to maxval=255, if needed */

   if( maxval < 255 ){
      int ii ; float fac = 255.4/maxval ;
      for( ii=0 ; ii < 3*nx*ny ; ii++ ) rgby[ii] = (byte)( rgby[ii]*fac ) ;
   }

   return rgbim ;
}

/*---------------------------------------------------------------
  June 1996: read a 1D or 2D file formatted in ASCII;
             will always return in MRI_float format.
-----------------------------------------------------------------*/

#define INC_TSARSIZE 128
#define LBUF         32768

MRI_IMAGE * mri_read_ascii( char * fname )
{
   MRI_IMAGE * outim ;
   int ii,jj,val , used_tsar , alloc_tsar ;
   float * tsar ;
   float ftemp ;
   FILE * fts ;
   char buf[LBUF] ;
   char * ptr ;
   int  ncol , bpos , blen , nrow ;

   if( fname == NULL || fname[0] == '\0' ) return NULL ;

   fts = fopen( fname , "r" ) ; if( fts == NULL ) return NULL ;

   /** step 1: read in the first line and see how many numbers are in it **/

   do{
     ptr = fgets( buf , LBUF , fts ) ;
     if( ptr == NULL ){ fclose( fts ) ; return NULL ; }  /* bad read? */
   } while( *ptr == '\0' || *ptr == '\n' || *ptr == '#' ) ;

   blen = strlen(buf) ;
   bpos = 0 ;
   ncol = 0 ;
   do{
       for( ; bpos < blen && (isspace(buf[bpos])||buf[bpos]==',') ; bpos++ ) ; /* nada */
       if( bpos == blen ) break ;
       ii = sscanf( buf+bpos , "%f%n" , &ftemp , &jj ) ;
       if( ii < 1 ){ ncol = 0 ; break ; }           /* bad scan? */
       ncol++ ; bpos += jj ;
   } while( bpos < blen ) ;
   if( ncol == 0 ){ fclose( fts ) ; return NULL ; } /* couldn't read? */

   /** At this point, ncol is the number of floats to be read from each line **/

   rewind( fts ) ;  /* will start over */

   used_tsar  = 0 ;
   alloc_tsar = INC_TSARSIZE ;
   tsar       = (float *) malloc( sizeof(float) * alloc_tsar ) ;
   if( tsar == NULL ){
      fprintf(stderr,"\n*** malloc error in mri_read_ascii ***\n") ; EXIT(1) ;
   }

   /** read lines, convert to floats, store **/

   nrow = 0 ;
   while( 1 ){
      ptr = fgets( buf , LBUF , fts ) ;  /* read */
      if( ptr == NULL ) break ;          /* failure --> end of data */

      if( *ptr == '\0' || *ptr == '\n' || *ptr == '#' ) continue ; /* skip line */
      blen = strlen(buf) ;

      /* convert commas to blanks */

      for( ii=0 ; ii < blen ; ii++ ) if( buf[ii] == ',' ) buf[ii] = ' ' ;

      for( ii=0,bpos=0 ; ii < ncol && bpos < blen ; ii++ ){
         val = sscanf( buf+bpos , "%f%n" , &ftemp , &jj ) ;  /* read from string */
         if( val < 1 ) break ;                               /* bad read? */
         bpos += jj ;                                        /* start of next read */

         if( used_tsar == alloc_tsar ){
            alloc_tsar += INC_TSARSIZE ;
            tsar        = (float *)realloc( tsar,sizeof(float)*alloc_tsar );
            if( tsar == NULL ){
               fprintf(stderr,"\n*** realloc error in mri_read_ascii ***\n") ; EXIT(1) ;
            }
         }

         tsar[used_tsar++] = ftemp ;  /* store input */
      }

      if( ii != ncol ) break ;  /* didn't get all of them? */

      nrow++ ;                  /* got one more complete row! */
   }
   fclose( fts ) ; /* finished with this file! */

   if( used_tsar <= 1 ){ free(tsar) ; return NULL ; }

   tsar = (float *) realloc( tsar , sizeof(float) * used_tsar ) ;
   if( tsar == NULL ){
      fprintf(stderr,"\n*** final realloc error in mri_read_ascii ***\n") ; EXIT(1) ;
   }

   outim = mri_new_vol_empty( ncol , nrow , 1 , MRI_float ) ;
   mri_fix_data_pointer( tsar , outim ) ;
   mri_add_name( fname , outim ) ;

   return outim ;
}

/*---------------------------------------------------------------------------
  16 Nov 1999: read an ASCII file as columns, transpose to rows,
               and allow column selectors.
-----------------------------------------------------------------------------*/

MRI_IMAGE * mri_read_1D( char * fname )
{
   MRI_IMAGE * inim , * outim , * flim ;
   char dname[256] , subv[256] , *cpt ;
   int ii,nx,ny,nts , *ivlist , *ivl ;
   float * far , * oar ;

   if( fname == NULL || fname[0] == '\0' || strlen(fname) > 255 ) return NULL ;

   /*-- split filename and subvector list --*/

   cpt = strstr(fname,"[") ;

   if( cpt == NULL ){                   /* no subvector list */
      strcpy( dname , fname ) ;
      subv[0] = '\0' ;
   } else if( cpt == fname ){           /* can't be at start of filename! */
      fprintf(stderr,"*** Illegal filename in mri_read_1D: %s\n",fname) ;
      return NULL ;
   } else {                             /* got a subvector list */
      ii = cpt - fname ;
      memcpy(dname,fname,ii) ; dname[ii] = '\0' ;
      strcpy(subv,cpt) ;
   }

   /*-- read file in --*/

   inim = mri_read_ascii(dname) ;
   if( inim == NULL ) return NULL ;
   flim = mri_transpose(inim) ; mri_free(inim) ;
   if( subv[0] == '\0' ) return flim ;             /* no subvector => am done */

   /*-- get the subvector list --*/

   nx = flim->nx ;
   ny = flim->ny ;

   ivlist = MCW_get_intlist( ny , subv ) ;         /* in thd_intlist.c */
   if( ivlist == NULL || ivlist[0] < 1 ){
      fprintf(stderr,"*** Illegal subvector list in mri_read_1D: %s\n",fname) ;
      if( ivlist != NULL ) free(ivlist) ;
      mri_free(flim) ; return NULL ;
   }

   nts = ivlist[0] ;                          /* number of subvectors */
   ivl = ivlist + 1 ;                         /* start of array of subvectors */

   for( ii=0 ; ii < nts ; ii++ ){            /* check them out */
      if( ivl[ii] < 0 || ivl[ii] >= ny ){
         fprintf(stderr,"*** Out-of-range subvector list in mri_read_1D: %s\n",fname) ;
         mri_free(flim) ; free(ivlist) ; return NULL ;
      }
   }

   outim = mri_new( nx , nts , MRI_float ) ;   /* make output image */
   far   = MRI_FLOAT_PTR( flim ) ;
   oar   = MRI_FLOAT_PTR( outim ) ;

   for( ii=0 ; ii < nts ; ii++ )               /* copy desired rows */
      memcpy( oar + ii*nx , far + ivl[ii]*nx , sizeof(float)*nx ) ;

   mri_free(flim) ; free(ivlist) ; return outim ;
}

/*---------------------------------------------------------------------------
  Read in an ASCII file to a float array.
-----------------------------------------------------------------------------*/

static void read_ascii_floats( char * fname, int * nff , float ** ff )
{
   int ii,jj,val , used_tsar , alloc_tsar ;
   float * tsar ;
   float ftemp ;
   FILE * fts ;
   char buf[LBUF] ;
   char * ptr ;
   int  bpos , blen , nrow ;

   /* check inputs */

   if( nff == NULL || ff == NULL ) return ;
   if( fname == NULL || fname[0] == '\0' ){ *nff=0 ; *ff=NULL ; return ; }

   fts = fopen( fname , "r" ) ;
   if( fts == NULL ){ *nff=0 ; *ff=NULL ; return ; }

   /* make some space */

   used_tsar  = 0 ;
   alloc_tsar = INC_TSARSIZE ;
   tsar       = (float *) malloc( sizeof(float) * alloc_tsar ) ;
   if( tsar == NULL ){
      fprintf(stderr,"\n*** malloc fails: read_ascii_floats ***\n"); EXIT(1);
   }

   /** read lines, convert to floats, store **/

   nrow = 0 ;
   while( 1 ){
      ptr = fgets( buf , LBUF , fts ) ;  /* read */
      if( ptr == NULL ) break ;          /* failure --> end of data */
      blen = strlen(buf) ;
      if( blen <= 0 ) break ;            /* nothing --> end of data */

      for( ii=0 ; ii < blen && isspace(buf[ii]) ; ii++ ) ; /* skip blanks */

      if( ii      == blen ) continue ;    /* skip all blank line */
      if( buf[ii] == '#'  ) continue ;    /* skip a comment line */
      if( buf[ii] == '!'  ) continue ;

      /* convert commas to blanks */

      for( jj=ii ; jj < blen ; jj++ ) if( buf[jj] == ',' ) buf[jj] = ' ' ;

      for( bpos=ii ; bpos < blen ; ){
         val = sscanf( buf+bpos , "%f%n" , &ftemp , &jj ) ;  /* read from string */
         if( val < 1 ) break ;                               /* bad read? */
         bpos += jj ;                                        /* start of next read */

         if( used_tsar == alloc_tsar ){
            alloc_tsar += INC_TSARSIZE ;
            tsar        = (float *)realloc( tsar,sizeof(float)*alloc_tsar );
            if( tsar == NULL ){
               fprintf(stderr,"\n*** realloc fails: read_ascii_floats ***\n"); EXIT(1);
            }
         }

         tsar[used_tsar++] = ftemp ;  /* store input */
      }

      nrow++ ;                  /* got one more complete row! */
   }
   fclose( fts ) ; /* finished with this file! */

   if( used_tsar <= 1 ){ free(tsar) ; *nff=0 ; *ff=NULL ; return ; }

   tsar = (float *) realloc( tsar , sizeof(float) * used_tsar ) ;
   if( tsar == NULL ){
      fprintf(stderr,"\n*** final realloc fails: read_ascii_floats ***\n"); EXIT(1);
   }

   *nff = used_tsar ; *ff  = tsar ; return ;
}

/*--------------------------------------------------------------
   Read a pile of images from one ASCII file.
   Adapted from mri_read_3D - Feb 2000 - RWCox.
   [N.B.: if this routine is altered, don't forget mri_imcount!]
----------------------------------------------------------------*/

MRI_IMARR * mri_read_3A( char * tname )
{
   int nx , ny , nz , ii , nxyz,nxy , nff ;
   int ngood , length , kim , datum_type ;
   char fname[256]="\0" , buf[512] ;
   MRI_IMARR * newar ;
   MRI_IMAGE * newim , * flim ;
   float * ff ;

   /*** get info from 3A tname ***/

   if( tname == NULL || strlen(tname) < 10 ) return NULL ;

   switch( tname[2] ){  /* allow for 3As:, 3Ab:, 3Af: */

      default: ngood = 0 ; break ;

      case 's':
         ngood = sscanf( tname, "3As:%d:%d:%d:%s", &nx, &ny, &nz, fname ) ;
         datum_type = MRI_short ;
         break ;

      case 'b':
         ngood = sscanf( tname, "3Ab:%d:%d:%d:%s", &nx, &ny, &nz, fname ) ;
         datum_type = MRI_byte ;
         break ;

      case 'f':
         ngood = sscanf( tname, "3Af:%d:%d:%d:%s", &nx, &ny, &nz, fname ) ;
         datum_type = MRI_float ;
         break ;
   }

   if( ngood < 4 || nx <= 0 || ny <= 0 || nz <= 0 || strlen(fname) <= 0 ) return NULL ;

   /* read the input file */

   read_ascii_floats( fname , &nff , &ff ) ;

   if( nff <= 0 || ff == NULL ) return NULL ;

   nxy = nx*ny ; nxyz = nxy*nz ;

   if( nff < nxyz ){
      fprintf(stderr,
                "\n** WARNING: %s is too short - padding with %d zeros\n",
                tname,nxyz-nff) ;
      ff = (float *) realloc( ff , sizeof(float) * nxyz ) ;
      for( ii=nff ; ii < nxyz ; ii++ ) ff[ii] = 0.0 ;
      nff = nxyz ;
   } else if( nff > nxyz ){
      fprintf(stderr,
                "\n** WARNING: %s is too long - truncating off last %d values\n",
                tname,nff-nxyz) ;
   }

   /* put the input data into MRI_IMAGEs */

   INIT_IMARR(newar) ;

   for( kim=0 ; kim < nz ; kim++ ){
      flim = mri_new( nx,ny , MRI_float ) ;
      memcpy( MRI_FLOAT_PTR(flim) , ff+nxy*kim , sizeof(float)*nxy ) ;
      switch( datum_type ){
         case MRI_float: newim = flim                                           ; break ;
         case MRI_short: newim = mri_to_short(1.0,flim)        ; mri_free(flim) ; break ;
         case MRI_byte:  newim = mri_to_byte_scl(1.0,0.0,flim) ; mri_free(flim) ; break ;
      }

      if( nz == 1 ) mri_add_name( fname , newim ) ;
      else {
         sprintf( buf , "%s#%d" , fname,kim ) ;
         mri_add_name( buf , newim ) ;
      }

      ADDTO_IMARR(newar,newim) ;
   }

   free(ff) ; return newar ;
}

/*---------------------------------------------------------------------------
   Stuff to read an ANALYZE 7.5 .img file, given the .hdr filename
   -- 05 Feb 2001 - RWCox
   -- 27 Nov 2001 - modified to use funused1 as a scale factor
-----------------------------------------------------------------------------*/

#include "mayo_analyze.h"

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

/*---------------------------------------------------------------*/

static int mri_imcount_analyze75( char * hname )
{
   FILE * fp ;
   struct dsr hdr ;    /* ANALYZE .hdr format */
   int doswap , nz ;

   fp = fopen( hname , "rb" ) ;
   if( fp == NULL ) return 0 ;
   hdr.dime.dim[0] = 0 ;
   fread( &hdr , 1 , sizeof(struct dsr) , fp ) ;
   fclose(fp) ;
   if( hdr.dime.dim[0] == 0 ) return 0 ;
   doswap = (hdr.dime.dim[0] < 0 || hdr.dime.dim[0] > 15) ;
   if( doswap ) swap_analyze_hdr( &hdr ) ;

   switch( hdr.dime.dim[0] ){
      case 2:  nz = 1                                 ; break ;
      case 3:  nz = hdr.dime.dim[3]                   ; break ;

      default:
      case 4:  nz = hdr.dime.dim[3] * hdr.dime.dim[4] ; break ;
   }
   if( nz < 1 ) nz == 1 ;

   /** fprintf(stderr,"mri_imcount_analyze75: %s %d\n",hname,nz) ; **/
   return nz ;
}

/*---------------------------------------------------------------*/

MRI_IMARR * mri_read_analyze75( char * hname )
{
   FILE * fp ;
   char iname[1024] , buf[1024] ;
   int ii , jj , doswap ;
   struct dsr hdr ;    /* ANALYZE .hdr format */
   int ngood , length , kim , koff , datum_type , datum_len , swap ;
   int   nx,ny,nz , hglobal=0 , himage=0 ;
   float dx,dy,dz ;
   MRI_IMARR * newar ;
   MRI_IMAGE * newim ;
   void      * imar ;
   float fac=0.0 ;    /* 27 Nov 2001 */
   int floatize ;     /* 28 Nov 2001 */
   int spmorg=0 ;     /* 28 Nov 2001 */

   /* check & prepare filenames */

   if( hname == NULL ) return NULL ;
   jj = strlen(hname) ;
   if( jj < 5 ) return NULL ;
   if( strcmp(hname+jj-3,"hdr") != 0 ) return NULL ;
   strcpy(iname,hname) ; strcpy(iname+jj-3,"img") ;

   /* read header file into struct */

   fp = fopen( hname , "rb" ) ;
   if( fp == NULL ) return NULL ;
   hdr.dime.dim[0] = 0 ;
   fread( &hdr , 1 , sizeof(struct dsr) , fp ) ;
   fclose(fp) ;
   if( hdr.dime.dim[0] == 0 ) return NULL ;

   /* check for swap-age */

   doswap = (hdr.dime.dim[0] < 0 || hdr.dime.dim[0] > 15) ;
   if( doswap ) swap_analyze_hdr( &hdr ) ;

   /* 28 Nov 2001: attempt to decode originator a la SPM */

   { short xyzuv[5] , xx,yy,zz ;
     memcpy( xyzuv , hdr.hist.originator , 10 ) ;
     if( xyzuv[3] == 0 && xyzuv[4] == 0 ){
        xx = xyzuv[0] ; yy = xyzuv[1] ; zz = xyzuv[2] ;
        if( doswap ){ swap_2(&xx); swap_2(&yy); swap_2(&zz); }
        if( xx > 0 && xx < hdr.dime.dim[1] &&
            yy > 0 && yy < hdr.dime.dim[2] &&
            zz > 0 && zz < hdr.dime.dim[3]   ) spmorg = 1 ;
     }
   }
   if( spmorg ) strcpy( MRILIB_orients , "LRPAIS" ) ;

   /* 27 Nov 2001: get a scale factor for images */

   if( !AFNI_noenv("AFNI_ANALYZE_SCALE") ){
      fac = hdr.dime.funused1 ;
      (void) thd_floatscan( 1 , &fac ) ;
      if( fac < 0.0 || fac == 1.0 ) fac = 0.0 ;
   }

   floatize = AFNI_yesenv("AFNI_ANALYZE_FLOATIZE") ; /* 28 Nov 2001 */

   /* get data type into mrilib MRI_* form */

   switch( hdr.dime.datatype ){
      default:
         fprintf(stderr,"*** %s: Unknown ANALYZE datatype=%d\n",
                 hname,hdr.dime.datatype) ;
      return NULL ;

      case ANDT_UNSIGNED_CHAR: datum_type = MRI_byte   ;               break;
      case ANDT_SIGNED_SHORT:  datum_type = MRI_short  ;               break;
      case ANDT_SIGNED_INT:    datum_type = MRI_int    ;               break;
      case ANDT_FLOAT:         datum_type = MRI_float  ; floatize = 0; break;
      case ANDT_COMPLEX:       datum_type = MRI_complex; floatize = 0; break;
      case ANDT_RGB:           datum_type = MRI_rgb    ; floatize = 0; break;
   }

   datum_len = mri_datum_size(datum_type) ;

   /* compute dimensions of images, and number of images */

   nx = hdr.dime.dim[1] ;
   ny = hdr.dime.dim[2] ;
   if( nx < 2 || ny < 2 ) return NULL ;

   switch( hdr.dime.dim[0] ){
      case 2:  nz = 1                                 ; break ;
      case 3:  nz = hdr.dime.dim[3]                   ; break ;

      default:
      case 4:  nz = hdr.dime.dim[3] * hdr.dime.dim[4] ; break ;
   }
   if( nz < 1 ) nz == 1 ;

   dx = hdr.dime.pixdim[1] ;
   dy = hdr.dime.pixdim[2] ;
   dz = hdr.dime.pixdim[3] ;

   /** fprintf(stderr,"mri_read_analyze75: nx=%d ny=%d nz=%d\n",nx,ny,nz) ; **/
   /** fprintf(stderr,"mri_read_analyze75: dx=%g dy=%g dz=%g\n",dx,dy,dz) ; **/

   /* open .img file and read images from it */

   length = THD_filesize(iname) ;
   if( length <= 0 ){
      fprintf(stderr,"*** Can't find ANALYZE file %s\n",iname) ;
      return NULL ;
   }

   fp = fopen( iname , "rb" ) ;
   if( fp == NULL ){
      fprintf(stderr,"*** Can't open ANALYZE file %s\n",iname) ;
      return NULL ;
   }

   ngood = datum_len*nx*ny*nz ;
   if( length < ngood ){
      fprintf( stderr,
        "*** ANALYZE file %s is %d bytes long but must be at least %d bytes long\n"
        "*** for nx=%d ny=%d nz=%d and voxel=%d bytes\n",
        iname,length,ngood,nx,ny,nz,datum_len ) ;
      return NULL ;
   }

   /*** read images from the file ***/

   INIT_IMARR(newar) ;

   for( kim=0 ; kim < nz ; kim++ ){
      koff = hglobal + (kim+1)*himage + datum_len*nx*ny*kim ;
   /** fprintf(stderr,"mri_read_analyze75: kim=%d koff=%d\n",kim,koff) ; **/
      fseek( fp , koff , SEEK_SET ) ;

      newim  = mri_new( nx , ny , datum_type ) ;
      imar   = mri_data_pointer( newim ) ;
      length = fread( imar , datum_len , nx * ny , fp ) ;

      if( doswap ){
         switch( datum_len ){
            default: break ;
            case 2:  swap_twobytes (   nx*ny , imar ) ; break ;  /* short */
            case 4:  swap_fourbytes(   nx*ny , imar ) ; break ;  /* int, float */
            case 8:  swap_fourbytes( 2*nx*ny , imar ) ; break ;  /* complex */
         }
      }

      /* 28 Nov 2001: convert to floats? */

      if( floatize ){
         MRI_IMAGE *qim = mri_to_float(newim) ;
         mri_free(newim) ; newim = qim ;
      }

      if( nz == 1 ) mri_add_name( iname , newim ) ;
      else {
         sprintf( buf , "%s#%d" , iname,kim ) ;
         mri_add_name( buf , newim ) ;
      }

      newim->dx = dx ; newim->dy = dy ; newim->dz = dz ; newim->dw = 1.0 ;
      ADDTO_IMARR(newar,newim) ;

      /* 27 Nov 2001: scale image? */

      if( fac != 0.0 ) mri_scale_inplace( fac , newim ) ;
   }

   fclose(fp) ; return newar ;
}

/*---------------------------------------------------------------------------
  12 Mar 2001 - stuff to read a Siemens Vision .ima file
-----------------------------------------------------------------------------*/

#include "siemens_vision.h"

static int mri_imcount_siemens( char * hname )
{
   struct Siemens_vision_header head ;
   FILE * fp ;
   int i,j,xx,yy , matrix , swap , imagesize,nxx,blank , slices ;
   struct stat file_stat ;
   short *imar ;

   /*--- check file size ---*/

   if( hname == NULL ) return 0 ;

   i = stat( hname , &file_stat ) ;
   if( i < 0 ) return 0 ;

   /*--- read header data ---*/

   fp = fopen( hname , "rb" ) ;
   if( fp == NULL ) return 0 ;
   fread( &head , sizeof(struct Siemens_vision_header) , 1 , fp ) ;

   /*-- check some integer in header to determine if we need to byteswap --*/

   swap = ( head.SiemensStudyDateMM < 0 || head.SiemensStudyDateMM > 13 ) ;
   if( swap ){
      swap_4( &(head.SiemensStudyDateMM) ) ;
      if( head.SiemensStudyDateMM < 0 || head.SiemensStudyDateMM > 13 ){
         swap = 0 ;
      }
   }

   /*-- find image size from header --*/

   if( swap ) swap_4( &(head.DisplayMatrixSize) ) ;
   imagesize = head.DisplayMatrixSize ;

   /*-- determine number of sub-images in file --*/

#undef  MATRIX_MAX
#define MATRIX_MAX 9

   i = 2*imagesize*imagesize ;
   for( matrix=1 ; matrix < MATRIX_MAX ; matrix++ )
     if( file_stat.st_size == i*matrix*matrix + SIEMENS_HEADERSIZE ) break ;

   if( matrix == MATRIX_MAX ){
     fclose(fp) ; return 0 ; /* didn't recognize file format */
   }
#undef MATRIX_MAX

   /*-- read image data from file (but don't byteswap it) --*/

   imar = (short *) calloc(sizeof(short),matrix*matrix*imagesize*imagesize) ;
   fseek( fp , SIEMENS_HEADERSIZE , SEEK_SET ) ;
   fread( imar , sizeof(short) , matrix*matrix*imagesize*imagesize , fp ) ;
   fclose(fp) ;

   /*-- count slices - all zero (blank) slices at end are skipped --*/

   slices = 0 ; nxx = matrix*imagesize ;

   for( yy=0 ; yy < matrix ; yy++ ){      /* rows in array of sub-images */
      for( xx=0 ; xx < matrix ; xx++ ){   /* cols in array of sub-images */
         blank = 1 ;
         for( j=0 ; j < imagesize ; j++ ){    /* row in sub-image */
            for( i=0 ; i < imagesize ; i++ ){ /* col in sub-image */
               if( imar[i+xx*imagesize+(j+yy*imagesize)*nxx] ) blank = 0 ;
            }
         }
         if( !blank ) slices = 1 + xx + yy*matrix ;
      }
   }

   free(imar) ; return slices ;
}

/*---------------------------------------------------------------------------*/

MRI_IMARR * mri_read_siemens( char * hname )
{
   struct Siemens_vision_header head ;
   FILE * fp ;
   int i,j,xx,yy , matrix , swap , imagesize,nxx,blank , slices,nz ;
   struct stat file_stat ;
   short *imar ;
   MRI_IMARR * newar ;
   MRI_IMAGE * newim ;
   short     * nar ;
   char buf[256] ;
   float dx,dy,dz ;
   char *eee ; int ileave=0 ;  /* 25 Sep 2001 */

   /*--- check file size ---*/

   if( hname == NULL ) return NULL ;

   i = stat( hname , &file_stat ) ;
   if( i < 0 ) return NULL ;

   /*--- read header data ---*/

   fp = fopen( hname , "rb" ) ;
   if( fp == NULL ) return NULL ;
   fread( &head , sizeof(struct Siemens_vision_header) , 1 , fp ) ;

   /*-- check some integer in header to determine if we need to byteswap --*/

   swap = ( head.SiemensStudyDateMM < 0 || head.SiemensStudyDateMM > 13 ) ;
   if( swap ){
      swap_4( &(head.SiemensStudyDateMM) ) ;
      if( head.SiemensStudyDateMM < 0 || head.SiemensStudyDateMM > 13 ){
         swap = 0 ;
      }
   }

   /*-- find image size from header --*/

   if( swap ) swap_4( &(head.DisplayMatrixSize) ) ;
   imagesize = head.DisplayMatrixSize ;

   /*-- determine number of sub-images in file --*/

#undef  MATRIX_MAX
#define MATRIX_MAX 16

   i = 2*imagesize*imagesize ;
   for( matrix=1 ; matrix < MATRIX_MAX ; matrix++ )
     if( file_stat.st_size == i*matrix*matrix + SIEMENS_HEADERSIZE ) break ;

   if( matrix == MATRIX_MAX ){
     fclose(fp) ; return NULL ; /* didn't recognize file format */
   }
#undef MATRIX_MAX

   /*-- read image data from file and byteswap it, if needed --*/

   imar = (short *) calloc(sizeof(short),matrix*matrix*imagesize*imagesize) ;
   fseek( fp , SIEMENS_HEADERSIZE , SEEK_SET ) ;
   fread( imar , sizeof(short) , matrix*matrix*imagesize*imagesize , fp ) ;
   fclose(fp) ;

   if( swap ) swap_twobytes( matrix*matrix*imagesize*imagesize , imar ) ;

   /*-- count slices - all zero (blank) slices at end are skipped --*/

   slices = 0 ; nxx = matrix*imagesize ;

   for( yy=0 ; yy < matrix ; yy++ ){      /* rows in array of sub-images */
      for( xx=0 ; xx < matrix ; xx++ ){   /* cols in array of sub-images */
         blank = 1 ;
         for( j=0 ; j < imagesize ; j++ ){    /* row in sub-image */
            for( i=0 ; i < imagesize ; i++ ){ /* col in sub-image */
               if( imar[i+xx*imagesize+(j+yy*imagesize)*nxx] ) blank = 0 ;
            }
         }
         if( !blank ) slices = 1 + xx + yy*matrix ;
      }
   }

   if( slices == 0 ){ free(imar) ; return NULL ; }  /* bad news */

   /*-- get image dimensions, etc --*/

   if( swap ){
      swap_8(&(head.FOVRow));
      swap_8(&(head.FOVColumn));
      swap_8(&(head.SliceThickness));
   }
   dx = head.FOVRow    / imagesize ;
   dy = head.FOVColumn / imagesize ;
   dz = head.SliceThickness ;

   /*-- save orientation and offset in global variables --*/

   MRILIB_orients[0] = head.OrientationSet1Left[0] ;
   MRILIB_orients[1] = head.OrientationSet2Right[0];
   MRILIB_orients[2] = head.OrientationSet1Top[0]  ;
   MRILIB_orients[3] = head.OrientationSet2Down[0] ;
   MRILIB_orients[4] = head.OrientationSet1Back[0] ;
   MRILIB_orients[5] = head.OrientationSet2Front[0];
   for (i=0; i<6; i++) {
      if (MRILIB_orients[i]=='H') MRILIB_orients[i]='S';
      if (MRILIB_orients[i]=='F') MRILIB_orients[i]='I';
   }
   MRILIB_orients[6] = '\0' ;
   MRILIB_zoff = fabs(strtod(head.TextSlicePosition,NULL)) ;

   /*-- create output --*/

   INIT_IMARR(newar) ;

   for( yy=0 ; yy < matrix ; yy++ ){      /* rows in array of sub-images */
      for( xx=0 ; xx < matrix ; xx++ ){   /* cols in array of sub-images */

         newim = mri_new( imagesize , imagesize , MRI_short ) ;
         nar   = MRI_SHORT_PTR( newim ) ;

         for( j=0 ; j < imagesize ; j++ )    /* row in sub-image */
           memcpy( nar+j*imagesize ,
                   imar+xx*imagesize+(j+yy*imagesize)*nxx , 2*imagesize ) ;

         sprintf( buf , "%s#%d:%d" , hname,xx,yy ) ;
         mri_add_name( buf , newim ) ;

         newim->dx = dx ; newim->dy = dy ; newim->dz = dz ; newim->dw = 1.0 ;
         ADDTO_IMARR(newar,newim) ;
         if( IMARR_COUNT(newar) == slices ) goto Done ;  /* Aauugghh!!! */
      }
   }

Done:

   /*-- 25 Sep 2001: possibly interleave the images --*/

   eee = getenv("AFNI_SIEMENS_INTERLEAVE") ;
   ileave = ( (eee != NULL) && (*eee=='Y' || *eee=='y') ) ;
   if( ileave && slices > 2 ){
      int mid = (slices-1)/2 ;  /* midpoint */
      MRI_IMARR *qar ;          /* new image array */
      INIT_IMARR(qar) ;
      for( i=0 ; i < slices ; i++ ){
         if( i%2 == 0 ) j = i/2 ;           /* slice #i is in newar #j */
         else           j = mid + (i+1)/2 ;
         ADDTO_IMARR(qar,IMARR_SUBIM(newar,j)) ; /* move image to new array */
      }
      FREE_IMARR(newar) ; newar = qar ;
   }

   free(imar) ; return newar ;
}

/*---------------------------------------------------------------------------
   Stuff to read a file in "delay" mode -- 01 Jan 1997.
-----------------------------------------------------------------------------*/

#ifdef USE_MRI_DELAY

/**** If possible, throw the data away for later retrieval from disk ****/

void mri_purge_delay( MRI_IMAGE * im )
{
   void * ar ;

   /** if no delay filename,
       or if it is marked as already set for delay input, do nothing **/

   if( im->fname == NULL ||
       (im->fondisk & INPUT_DELAY) != 0 ) return ;

   /** get the data pointer, throw data way, clear the data pointer **/

   ar = mri_data_pointer( im ) ;
   if( ar != NULL ){ free(ar) ; mri_clear_data_pointer(im) ; }

   /** mark as set for delay input **/

   im->fondisk |= INPUT_DELAY ;
   return ;
}

/**** if possible, read data from delay input file ****/

void mri_input_delay( MRI_IMAGE * im )
{
   FILE * imfile=NULL ;
   void * imar ;

   /** if no delay input file,
       or is marked as already read in, do nothing **/

   if( im->fname == NULL ||
       (im->fondisk & INPUT_DELAY) == 0 ) return ;

   /** open the delay input file [06 Mar 2001: maybe not] **/

   if( strcmp(im->fname,"ALLZERO") != 0 ){
      imfile = fopen( im->fname , "r" ) ;
      if( imfile == NULL ){
         fprintf( stderr , "couldn't open delayed image file %s\n" , im->fname ) ;
         return ;
      }
   }

   /** make space for the array **/

   imar = (void *) malloc( im->nvox * im->pixel_size ) ;
   if( imar == NULL ){
      fprintf( stderr ,
               "malloc fails for delayed image from file %s\n" , im->fname ) ;
      if( imfile != NULL ) fclose( imfile ) ;
      return ;
   }
   mri_fix_data_pointer( imar , im ) ;

   /** read from the file into the array **/

   if( imfile != NULL ){
      fseek( imfile , im->foffset , SEEK_SET ) ;
      fread( imar , im->pixel_size , im->nvox , imfile ) ;
      fclose( imfile ) ;
   } else {
      memset( imar , 0 , im->nvox * im->pixel_size ) ;  /* 06 Mar 2001 */
   }

   /** swap bytes, if so marked **/

   if( (im->fondisk & BSWAP_DELAY) ) mri_swapbytes( im ) ;

   /** mark as already read from disk **/

   im->fondisk ^= INPUT_DELAY ;

#if 0
fprintf(stderr,"delayed input from file %s at offset %d\n",im->fname,im->foffset);
#endif
   return ;
}

/**********************************************************************/
/**** like mri_read_file, but returns delayed images for 3D: files ****/
/**** (all others are read in now anyhoo, so there)                ****/

MRI_IMARR * mri_read_file_delay( char * fname )
{
   MRI_IMARR * newar ;
   MRI_IMAGE * newim ;
   char * new_fname ;

   new_fname = imsized_fname( fname ) ;
   if( new_fname == NULL ) return NULL ;

   if( strlen(new_fname) > 9 && new_fname[0] == '3' && new_fname[1] == 'D' ){

      newar = mri_read_3D_delay( new_fname ) ;   /* read from a 3D file, later */

   } else if( strlen(new_fname) > 9 &&
              new_fname[0] == '3' && new_fname[1] == 'A' && new_fname[3] == ':' ){

      newar = mri_read_3A( new_fname ) ;

   } else if( strstr(new_fname,".hdr") != NULL ||
              strstr(new_fname,".HDR") != NULL   ){ /* 05 Feb 2001 - ANALYZE header */

      newar = mri_read_analyze75( new_fname ) ;

   } else if( strstr(new_fname,".ima") != NULL ||
              strstr(new_fname,".IMA") != NULL   ){ /* 12 Mar 2001 - Siemens */

      newar = mri_read_siemens( new_fname ) ;

   } else {
      newim = mri_read( new_fname ) ;      /* read from a 2D file */
      if( newim == NULL ){ free(new_fname) ; return NULL ; }
      INIT_IMARR(newar) ;
      ADDTO_IMARR(newar,newim) ;
   }
   free(new_fname) ;
   return newar ;
}

/**** like mri_read_3D, but returns delayed images ****/

MRI_IMARR * mri_read_3D_delay( char * tname )
{
   int hglobal , himage , nx , ny , nz ;
   char fname[256] , buf[512] ;
   int ngood , length , kim , datum_type , datum_len , swap ;
   MRI_IMARR * newar ;
   MRI_IMAGE * newim ;
   FILE      * imfile ;

   /*** get info from 3D tname ***/

   if( tname == NULL || strlen(tname) < 10 ) return NULL ;

   switch( tname[2] ){  /* allow for 3D: or 3Ds: or 3Db: */

      default:
      case ':':
         ngood = sscanf( tname , "3D:%d:%d:%d:%d:%d:%s" ,
                         &hglobal , &himage , &nx , &ny , &nz , fname ) ;

         swap       = 0 ;
         datum_type = MRI_short ;
         datum_len  = sizeof(short) ;  /* better be 2 */
         break ;

      case 's':
         ngood = sscanf( tname , "3Ds:%d:%d:%d:%d:%d:%s" ,
                         &hglobal , &himage , &nx , &ny , &nz , fname ) ;

         swap       = 1 ;
         datum_type = MRI_short ;
         datum_len  = sizeof(short) ;  /* better be 2 */
         break ;

      case 'b':
         ngood = sscanf( tname , "3Db:%d:%d:%d:%d:%d:%s" ,
                         &hglobal , &himage , &nx , &ny , &nz , fname ) ;

         swap       = 0 ;
         datum_type = MRI_byte ;
         datum_len  = sizeof(byte) ;  /* better be 1 */
         break ;

      case 'f':
         ngood = sscanf( tname , "3Df:%d:%d:%d:%d:%d:%s" ,
                         &hglobal , &himage , &nx , &ny , &nz , fname ) ;

         swap       = 0 ;
         datum_type = MRI_float ;
         datum_len  = sizeof(float) ;  /* better be 4 */
         break ;

      case 'i':
         ngood = sscanf( tname , "3Di:%d:%d:%d:%d:%d:%s" ,
                         &hglobal , &himage , &nx , &ny , &nz , fname ) ;

         swap       = 0 ;
         datum_type = MRI_int ;
         datum_len  = sizeof(int) ;  /* better be 4 */
         break ;

      case 'c':
         ngood = sscanf( tname , "3Dc:%d:%d:%d:%d:%d:%s" ,
                         &hglobal , &himage , &nx , &ny , &nz , fname ) ;

         swap       = 0 ;
         datum_type = MRI_complex ;
         datum_len  = sizeof(complex) ;  /* better be 8 */
         break ;
   }

   if( ngood < 6 || himage < 0 ||
       nx <= 0   || ny <= 0    || nz <= 0 ||
       strlen(fname) <= 0                   ) return NULL ;   /* bad info */

   /*** open the input file and position it [06 Mar 2001: maybe not] ***/

   if( strcmp(fname,"ALLZERO") != 0 ){
      imfile = fopen( fname , "r" ) ;
      if( imfile == NULL ){
         fprintf( stderr , "couldn't open delayed image file %s\n" , fname ) ;
         return NULL ;
      }
   } else {
      imfile = NULL ;
   }

   if( imfile != NULL ){
      fseek( imfile , 0L , SEEK_END ) ;  /* get the length of the file */
      length = ftell( imfile ) ;

   /** 13 Apr 1999: modified to allow actual hglobal < -1
                    as long as hglobal+himage >= 0       **/

#if 0                 /* the old code */
      if( hglobal < 0 ){
         hglobal = length - nz*(datum_len*nx*ny+himage) ;
         if( hglobal < 0 ) hglobal = 0 ;
      }
#else                 /* 13 Apr 1999 */
      if( hglobal == -1 || hglobal+himage < 0 ){
         hglobal = length - nz*(datum_len*nx*ny+himage) ;
         if( hglobal < 0 ) hglobal = 0 ;
      }
#endif

      ngood = hglobal + nz*(datum_len*nx*ny+himage) ;
      if( length < ngood ){
         fprintf( stderr,
           "file %s is %d bytes long but must be at least %d bytes long\n"
           "for hglobal=%d himage=%d nx=%d ny=%d nz=%d and voxel=%d bytes\n",
           fname,length,ngood,hglobal,himage,nx,ny,nz,datum_len ) ;
         fclose( imfile ) ;
         return NULL ;
      }
      fclose( imfile ) ;
   }

   /*** put pointers to data in the file into the images ***/

   INIT_IMARR(newar) ;

   for( kim=0 ; kim < nz ; kim++ ){
      newim = mri_new_vol_empty( nx,ny,1 , datum_type ) ;  /* empty image */
      mri_add_fname_delay( fname , newim ) ;               /* put filename in */
      newim->fondisk = (swap) ? (INPUT_DELAY | BSWAP_DELAY) /* mark read type */
                              : (INPUT_DELAY) ;
      newim->foffset = hglobal + (kim+1)*himage + datum_len*nx*ny*kim ;

      if( nz == 1 ) mri_add_name( fname , newim ) ;
      else {
         sprintf( buf , "%s#%d" , fname,kim ) ;
         mri_add_name( buf , newim ) ;
      }

      ADDTO_IMARR(newar,newim) ;
   }

   return newar ;
}
#endif /* USE_MRI_DELAY */
