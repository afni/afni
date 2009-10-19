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

/*--------------------------------------------------------*/
/*** 7D SAFE (but most routines only return 2D images!) ***/
/*--------------------------------------------------------*/

#include <stdio.h>
#include <ctype.h>
#include <string.h>
#include <sys/stat.h>

/*** for non ANSI compilers ***/

#ifndef SEEK_SET
#define SEEK_SET 0
#endif

#include "mrilib.h"
#include "ge4_header.h"

/*---------------------------------------------------------------*/
static MRI_IMAGE * mri_try_mri( FILE * , int * ) ;  /* prototypes */
static MRI_IMAGE * mri_try_7D ( FILE * , int * ) ;
static MRI_IMAGE * mri_try_pgm( FILE * , int * ) ;
static int         check_dicom_magic_num( char * ) ;
/*---------------------------------------------------------------*/

/*! Global variable to signal image orientation, if possible. */

char MRILIB_orients[8] = "\0\0\0\0\0\0\0\0" ;  /* 12 Mar 2001 */

/*! Global variable to signal image slice offset, if possible. */

float MRILIB_zoff      = 0.0 ;

/*! Global variable to signal image TR, if possible. */

float MRILIB_tr        = 0.0 ;   /* 03 Dec 2001 */

/*! Global variable to signal image x offset, if possible. */

float MRILIB_xoff      = 0.0 ;   /* 07 Dec 2001 */

/*! Global variable to signal image y offset, if possible. */

float MRILIB_yoff      = 0.0 ;

/*! Global variable saying whether to use MRILIB_xoff. */

int use_MRILIB_xoff    = 0 ;

/*! Global variable saying whether to use MRILIB_yoff. */

int use_MRILIB_yoff    = 0 ;

/*! Global variable saying whether to use MRILIB_zoff. */

int use_MRILIB_zoff    = 0 ;

/*! Global variable saying whether to use MRILIB_xcos. */

int use_MRILIB_xcos    = 0 ;

/*! Global vector pointing in direction of x-axis. */

float MRILIB_xcos[3]   = { 1.0 , 0.0 , 0.0 } ;

/*! Global variable saying whether to use MRILIB_ycos. */

int use_MRILIB_ycos    = 0 ;

/*! Global vector pointing in direction of y-axis. */

float MRILIB_ycos[3]   = { 0.0 , 1.0 , 0.0 } ;

/*! Global variable saying whether to use MRILIB_zcos. */

int use_MRILIB_zcos    = 0 ;

/*! Global vector pointing in direction of z-axis. */

float MRILIB_zcos[3]   = { 0.0 , 0.0 , 1.0 } ;

/*! Global variable saying whether to use MRILIB_slicespacing. */

int use_MRILIB_slicespacing = 0 ;

/*! Global variable giving the spacing between slice centers. */

float MRILIB_slicespacing = 0.0 ;

/*! Global variable saying whether to use DICOM matrix below. */

int use_MRILIB_dicom_matrix = 0 ;

/*! Global variable defining 3D image position and orientation. */

mat44   MRILIB_dicom_matrix     ;

/*! Global variable counting number of DICOM files read. */

int     MRILIB_dicom_count = 0 ;

/*! Global variable containing DICOM header info for each file. */

AFD_dicom_header **MRILIB_dicom_header = NULL ;

/*-----------------------------------------------------------------*/

/*! Database of preset file sizes for auto-use of 3D.

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

#undef swap_4
#undef swap_2

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

/*! Earliest image reading function in the AFNI package.
    Reads a single 2D image.

    \param  fname is the name of the file to try to read
    \return NULL if an image couldn't be read, otherwise
            a pointer to an MRI_IMAGE with data, dimensions, etc.
*/

MRI_IMAGE *mri_read( char *fname )
{
   FILE      *imfile ;
   MRI_IMAGE *im=NULL ;
   int       length , skip=0 , swap=0 ;
   void      *data ;

ENTRY("mri_read") ;

   if( fname == NULL || *fname == '\0' ) RETURN(NULL) ;  /* bad user */

   /**-- 27 Apr 2005: check here for special filenames --**/

   if( strstr(fname,".jpg" ) != NULL ||  /* various formats  */
       strstr(fname,".JPG" ) != NULL ||  /* that we convert  */
       strstr(fname,".jpeg") != NULL ||  /* to PPG/PGM using */
       strstr(fname,".JPEG") != NULL ||  /* external filters */
       strstr(fname,".gif" ) != NULL ||
       strstr(fname,".GIF" ) != NULL ||
       strstr(fname,".tif" ) != NULL ||
       strstr(fname,".TIF" ) != NULL ||
       strstr(fname,".tiff") != NULL ||
       strstr(fname,".TIFF") != NULL ||
       strstr(fname,".bmp" ) != NULL ||
       strstr(fname,".BMP" ) != NULL ||
       strstr(fname,".pbm" ) != NULL ||
       strstr(fname,".PBM" ) != NULL ||
       strstr(fname,".pgm" ) != NULL ||
       strstr(fname,".PGM" ) != NULL ||
       strstr(fname,".ppm" ) != NULL ||
       strstr(fname,".PPM" ) != NULL ||
       strstr(fname,".png" ) != NULL ||
       strstr(fname,".PNG" ) != NULL   ){

     im = mri_read_stuff(fname) ; if( im != NULL ) RETURN(im) ;
   }

   /*-- 16 Aug 2006: AFNI dataset? --*/

   if( strstr(fname,".HEAD") != NULL || strstr(fname,".nii") != NULL ){
     THD_3dim_dataset *dset = THD_open_dataset(fname) ;
     if( dset != NULL ){
      if( DSET_NVALS(dset) == 1 ){
       DSET_load(dset) ;
       if( DSET_BRICK(dset,0) != NULL && DSET_ARRAY(dset,0) != NULL )
         im = mri_copy( DSET_BRICK(dset,0) ) ;
         im->dx = fabs(DSET_DX(dset)) ;
         im->dy = fabs(DSET_DY(dset)) ;
         im->dz = fabs(DSET_DZ(dset)) ;
      }
      DSET_delete(dset) ;
      if( im != NULL ) RETURN(im) ;
     }
   }

   /*-- check if file exists and is readable --*/

   imfile = fopen( fname , "r" ) ;
   if( imfile == NULL ){
     fprintf( stderr , "couldn't open image file %s\n" , fname ) ;
     RETURN( NULL );
   }

   length = THD_filesize(fname) ;     /* 22 Mar 2007 */

   /*--- 03 Dec 2001: check for GEMS format file "IMGF"   ---*/
   /*[[[ Information herein from Medical Image Format FAQ ]]]*/

   { char str[5]="AFNI" ;
     int nx , ny , bpp , cflag , hdroff , extraskip=0 ;
     rewind(imfile) ; fread(str,1,4,imfile) ;   /* check for "IMGF" or "GEMS" */

     if( str[0]=='G' && str[1]=='E' && str[2]=='M' && str[3]=='S' ){ /* 12 Feb 2004 */
       char buf[4096]; int bb,cc;                /* search for IMGF in 1st 4K */
       rewind(imfile); cc=fread(buf,1,4096,imfile); cc-=4 ;
       for( bb=4; bb < cc ; bb++ )
        if( buf[bb]=='I' && buf[bb+1]=='M' && buf[bb+2]=='G' && buf[bb+3]=='F' ) break ;
       if( bb < cc ){
         fseek( imfile , (long)bb , SEEK_SET ) ; extraskip = bb ;
         fread(str,1,4,imfile) ;
       }
     }

     /* 12 Feb 2004: modified to allow for starting at extraskip */

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
       skip += extraskip ;             /* location of image data in file */
       if( nx < 0 || nx > 8192 || ny < 0 || ny > 8192 ) goto The_Old_Way ;
       if( skip < 0  || skip  >= length )               goto The_Old_Way ;
       if( bpp != 16 || cflag != 1      )               goto The_Old_Way ;

       /* make image space */

       im = mri_new( nx , ny , MRI_short ) ;

       /* try to read image auxiliary data as well (not mandatory) */

       length = fseek( imfile , 148L+extraskip , SEEK_SET ) ; /* magic GEMS offset */
       if( length == 0 ){
          fread( &hdroff , 4,1 , imfile ) ;  /* location of image header */
          if( swap ) swap_4(&hdroff) ;
          if( hdroff > 0 ){                  /* read from image header */
             float dx,dy,dz, dxx,dyy,dzz, xyz[9], zz ; int itr, ii,jj,kk, qq ;
             static int nzoff=0 ;
             static float zoff ;

             /* get voxel grid sizes */

             fseek( imfile , hdroff+26+extraskip , SEEK_SET ) ;
             fread( &dzz , 4,1 , imfile ) ;

             fseek( imfile , hdroff+50+extraskip , SEEK_SET ) ;
             fread( &dxx , 4,1 , imfile ) ;
             fread( &dyy , 4,1 , imfile ) ;

             if( swap ){ swap_4(&dxx); swap_4(&dyy); swap_4(&dzz); }

             /* save into image header [dw > 0 is signal that dx,dy,dz are OK] */

             if( dxx > 0.01 && dyy > 0.01 && dzz > 0.01 ){
               im->dx = dxx; im->dy = dyy; im->dz = dzz; im->dw = 1.0;
             }

             /* grid orientation: from 3 sets of LPI corner coordinates: */
             /*   xyz[0..2] = top left hand corner of image     (TLHC)   */
             /*   xyz[3..5] = top right hand corner of image    (TRHC)   */
             /*   xyz[6..8] = bottom right hand corner of image (BRHC)   */
             /* GEMS coordinate orientation here is LPI.                 */
             /* Orientation is saved into global string MRILIB_orients.  */
             /* N.B.: AFNI coordinates are RAI orientation.              */

             fseek( imfile , hdroff+154+extraskip , SEEK_SET ) ;
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
             zz = xyz[kk-1] ;             /* z-coordinate of this slice (from TLHC) */

             im->zo = zz ;                /* 07 Aug 2002: save slice offset */

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

               if( !use_MRILIB_slicespacing && qoff != 0.0 ){ /* 10 Jan 2004 */
                 use_MRILIB_slicespacing = 1 ;
                     MRILIB_slicespacing = fabs(qoff) ;
               }

               switch( kk ){
                case  1: MRILIB_orients[4] = 'L'; MRILIB_orients[5] = 'R'; break;
                case -1: MRILIB_orients[4] = 'R'; MRILIB_orients[5] = 'L'; break;
                case  2: MRILIB_orients[4] = 'P'; MRILIB_orients[5] = 'A'; break;
                case -2: MRILIB_orients[4] = 'A'; MRILIB_orients[5] = 'P'; break;
                case  3: MRILIB_orients[4] = 'I'; MRILIB_orients[5] = 'S'; break;
                case -3: MRILIB_orients[4] = 'S'; MRILIB_orients[5] = 'I'; break;
                default: MRILIB_orients[4] ='\0'; MRILIB_orients[5] ='\0'; break;
               }

               /* save spatial offset of first slice              */
               /* [this needs to be positive in the direction of] */
               /* [the -z axis, so may need to change its sign  ] */

               MRILIB_zoff = zoff ; use_MRILIB_zoff = 1 ;
               if( kk == 1 || kk == 2 || kk == 3 ) MRILIB_zoff = -MRILIB_zoff ;

               /* Same for x offset; [20 Dec 2001]
                  This must be at the middle of the TLHC voxel,
                    so we must move a little bit towards the TRHC edge;
                  We only use the result if the x-coordinate doesn't
                    change significantly between the TRHC and BRHC,
                    to avoid problems with oblique slices.         */

               qq = abs(ii) ;
               MRILIB_xoff = ( xyz[qq-1]*(nx-0.5) + xyz[qq+2]*0.5 ) / nx ;
               if( ii == 1 || ii == 2 || ii == 3 ) MRILIB_xoff = -MRILIB_xoff ;
               use_MRILIB_xoff = ( fabs(xyz[qq+2]-xyz[qq+5]) < 0.01*dxx ) ;

               /* Same for y offset;
                  This must be at the middle of the TLHC voxel,
                    so we must move a little bit towards the BRHC edge;
                  We only use the result if the y-coordinate doesn't
                    change significantly between the TLHC and TRHC. */

               qq = abs(jj) ;
               MRILIB_yoff = ( xyz[qq-1]*(ny-0.5) + xyz[qq+5]*0.5 ) / ny ;
               if( jj == 1 || jj == 2 || jj == 3 ) MRILIB_yoff = -MRILIB_yoff ;
               use_MRILIB_yoff = ( fabs(xyz[qq-1]-xyz[qq+2]) < 0.01*dyy ) ;
             }
             nzoff++ ;  /* 3rd and later images don't count for z-orientation */

             /* get TR, save into global variable */

             if( MRILIB_tr <= 0.0 ){
               fseek( imfile , hdroff+194+extraskip , SEEK_SET ) ;
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
         if( im != NULL ) RETURN( im );

         im = mri_read_ppm( fname ) ;      /* 15 Apr 1999 */
         if( im != NULL ) RETURN( im );

         im = mri_read_stuff( fname ) ;    /* 22 Nov 2002 */
         if( im != NULL ) RETURN( im );

         fprintf( stderr , "do not recognize image file %s\n" , fname );
         fprintf( stderr , "length seen as %d\n" , length ) ;
         RETURN( NULL );
   }

   /*-- Actually read the data from disk --*/

Ready_To_Roll:

   data = mri_data_pointer( im ) ;

   length = fseek( imfile , skip , SEEK_SET ) ;
   if( length != 0 ){
      fprintf( stderr , "mri_read error in skipping in file %s\n" , fname ) ;
      mri_free( im ) ;
      RETURN( NULL );
   }

   length = fread( data , im->pixel_size , im->nvox , imfile ) ;
   fclose( imfile ) ;

   if( length != im->nvox ){
      mri_free( im ) ;
      fprintf( stderr , "couldn't read image data from file %s\n" , fname ) ;
      RETURN( NULL );
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

     im->was_swapped = 1 ;  /* 07 Mar 2002 */
   }

   RETURN( im );
}


/******************************************************************/

/* GEMS 4.x image reading   - rickr  03 Jun 2003 */

/*! Read a single 2D GEMS 4.x image.

    \param  filename is the name of the file to try to read
    \return NULL if an image could not be read, otherwise
            the address of a new MRI_IMAGE structure
*/

MRI_IMAGE * mri_read_ge4( char * filename )
{
    MRI_IMAGE * im;
    ge4_header  H;

ENTRY( "mri_read_ge4" );

    if ( filename == NULL )
    {
        fprintf( stderr, "** mri_read_ge4 - missing filename\n" );
        RETURN( NULL );
    }

    /* try to read image file - return with image */
    if ( ge4_read_header( &H, filename, True ) != 0 )
        RETURN( NULL );

    /* these dimensions are fixed */
    if ( (im = mri_new(256, 256, MRI_short)) == NULL )
    {
        free(H.image);
        RETURN( NULL );
    }

    /* fill im struct with data from H */
    im->zo = H.im_h.im_loc;        /* this may well be incorrect */
    im->dt = H.im_h.tr;
    im->was_swapped = H.swap;

    if ( ( H.ser_h.fov >    1.0      ) &&
         ( H.ser_h.fov < 1000.0      ) &&
         ( H.ser_h.scan_mat_x >    0 ) &&
         ( H.ser_h.scan_mat_x < 1000 ) &&
         ( H.ser_h.scan_mat_y >    0 ) &&
         ( H.ser_h.scan_mat_y < 1000 ) )
    {
        /* attempt to set dx, dy and dz from these */

        im->dx = 2 * H.ser_h.fov / H.ser_h.scan_mat_x;
        im->dy = im->dx;
        im->dz = 2 * H.ser_h.fov / H.ser_h.scan_mat_y;
        im->dw = 1;
    }

    memcpy( mri_data_pointer(im), H.image, H.im_bytes );

    mri_add_name( filename, im );

    free(H.image);        /* your services are no longer required */

    RETURN( im );
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

/*! Try to read an file in the "Cox MRI" format.

    \param imfile is a pointer to an open FILE.
    \param skip is a pointer to an int that will be set to the number
           of bytes to skip from the file start to find the image data
    \return NULL if the file doesn't work for "Cox MRI" format;
            otherwise, the return is a pointer to an MRI_IMAGE ready
            to have its data read from imfile.
*/

static MRI_IMAGE *mri_try_mri( FILE *imfile , int *skip )
{
   int ch , nch , nx,ny,imcode ;
   char buf[64] ;
   MRI_IMAGE *im ;

ENTRY("mri_try_mri") ;

   fseek( imfile , 0 , SEEK_SET ) ;  /* rewind file */

   ch = getc( imfile ) ; if( ch != 'M' ) RETURN( NULL );  /* check for MRI */
   ch = getc( imfile ) ; if( ch != 'R' ) RETURN( NULL );
   ch = getc( imfile ) ; if( ch != 'I' ) RETURN( NULL );

   /* magic MRI found, so read numbers */

   ch = getc(imfile) ;

   NUMSCAN(imcode) ;
   NUMSCAN(nx) ;  if( nx <= 0 ) RETURN( NULL );
   NUMSCAN(ny) ;  if( ny <= 0 ) RETURN( NULL );

   *skip = ftell(imfile) ;
   im    = mri_new( nx , ny , imcode ) ;
   RETURN( im );
}

/**************************************************************************
   7D format: MRn kind n-dimensions data, where 'n' = 1-7.
***************************************************************************/

/*! Try to read a "Cox nD MRI" image file (fat chance). */

static MRI_IMAGE *mri_try_7D( FILE *imfile , int *skip )
{
   int ch , nch , nx,ny,nz,nt,nu,nv,nw , imcode , ndim ;
   char buf[64] ;
   MRI_IMAGE *im ;

ENTRY("mri_try_7D") ;

   fseek( imfile , 0 , SEEK_SET ) ;  /* rewind file */

   ch = getc( imfile ) ; if( ch != 'M' ) RETURN( NULL );  /* check for MR[1-7] */
   ch = getc( imfile ) ; if( ch != 'R' ) RETURN( NULL );
   ch = getc( imfile ) ;
   switch( ch ){
      default:  RETURN( NULL );   /* not what I expected */

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

                   NUMSCAN(nx) ;  if( nx <= 0 ) RETURN( NULL );
   if( ndim > 1 ){ NUMSCAN(ny) ;  if( ny <= 0 ) RETURN( NULL ); }
   if( ndim > 2 ){ NUMSCAN(nz) ;  if( nz <= 0 ) RETURN( NULL ); }
   if( ndim > 3 ){ NUMSCAN(nt) ;  if( nt <= 0 ) RETURN( NULL ); }
   if( ndim > 4 ){ NUMSCAN(nu) ;  if( nu <= 0 ) RETURN( NULL ); }
   if( ndim > 5 ){ NUMSCAN(nv) ;  if( nv <= 0 ) RETURN( NULL ); }
   if( ndim > 6 ){ NUMSCAN(nw) ;  if( nw <= 0 ) RETURN( NULL ); }

   *skip = ftell(imfile) ;
   im    = mri_new_7D_generic( nx,ny,nz,nt,nu,nv,nw , imcode , TRUE ) ;
   RETURN( im );
}

/*********************************************************************/

/*! Try to read a raw PGM format image file.

    \param imfile is a pointer to an open FILE
    \param skip is a pointer to an int; *skip will be set to the
           byte offset at which to start reading data
    \return A pointer to an MRI_IMAGE ready to have its data read in
            (if the file is a PGM file), or NULL.
*/

static MRI_IMAGE *mri_try_pgm( FILE *imfile , int *skip )
{
   int ch , nch , nx,ny,maxval ;
   char buf[64] ;
   MRI_IMAGE *im ;

ENTRY("mri_try_pgm") ;

   fseek( imfile , 0 , SEEK_SET ) ;  /* rewind file */

   ch = getc( imfile ) ; if( ch != 'P' ) RETURN(NULL);  /* check for magic */
   ch = getc( imfile ) ; if( ch != '5' ) RETURN(NULL);

   /* magic P5 found, so read numbers */

   ch = getc(imfile) ;

   NUMSCAN(nx)     ; if( nx     <= 0 ) RETURN(NULL);
   NUMSCAN(ny)     ; if( ny     <= 0 ) RETURN(NULL);
   NUMSCAN(maxval) ; if( maxval <= 0 || maxval >  255 ) RETURN(NULL);

   *skip = ftell(imfile) ;
   im    = mri_new( nx , ny , MRI_byte ) ;
   RETURN(im);
}

/*--------------------------------------------------------------
   Read a pile of images from one file.
   Modified 4/4/95 to read short or byte data.
   Modified 10/02/95 to allow byte swapping with 3Ds:
   Modified 11/06/95 to allow float images with 3Df:
                 and to allow int images with 3Di:
                 and to allow complex images with 3Dc:
   Modified 16 Apr 2002 to allow RGB input with 3Dr:

   [N.B.: if this routine is altered, don't forget mri_imcount!]
----------------------------------------------------------------*/

/*! Read one or more 2D slices from a "3D:" formatted image file. */

MRI_IMARR * mri_read_3D( char *tname )
{
   int hglobal , himage , nx , ny , nz ;
   char fname[256] , buf[512] ;
   int ngood , kim , datum_type , datum_len , swap ;
   MRI_IMARR *newar ;
   MRI_IMAGE *newim ;
   void      *imar ;
   FILE      *imfile ;
   long long length , nneed , koff , hglob ;  /* 22 Mar 2007 */

ENTRY("mri_read_3D") ;

   /*** get info from 3D tname ***/

   if( tname == NULL || strlen(tname) < 10 ) RETURN(NULL) ;

   switch( tname[2] ){  /* allow for 3D: or 3Ds: or 3Db:, etc */

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

      case 'd':                                            /* 06 Feb 2003 */
         ngood = sscanf( tname , "3Dd:%d:%d:%d:%d:%d:%s" ,
                         &hglobal , &himage , &nx , &ny , &nz , fname ) ;

         swap       = 0 ;
         datum_type = MRI_double ;
         datum_len  = sizeof(double) ;  /* better be 8 */
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

      case 'r':
         ngood = sscanf( tname , "3Dr:%d:%d:%d:%d:%d:%s" ,
                         &hglobal , &himage , &nx , &ny , &nz , fname ) ;

         swap       = 0 ;
         datum_type = MRI_rgb ;
         datum_len  = 3*sizeof(byte) ;  /* better be 3 */
         break ;
   }

   if( ngood < 6 || himage < 0 ||
       nx <= 0   || ny <= 0    || nz <= 0 ||
       strlen(fname) <= 0                   ) RETURN(NULL);   /* bad info */

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
      RETURN(newar);
   }

   /*** open the input file and position it ***/

   imfile = fopen( fname , "r" ) ;
   if( imfile == NULL ){
     fprintf( stderr , "couldn't open image file %s\n" , fname ) ;
     RETURN(NULL);
   }

   length = THD_filesize(fname) ;     /* 22 Mar 2007 */

   /** 13 Apr 1999: modified to allow actual hglobal < -1
                    as long as hglobal+himage >= 0       **/

   hglob = hglobal ;
   if( hglob == -1 || hglob+himage < 0 ){
      hglob = length - (datum_len*nx*ny+himage) * (long long)nz ;
      if( hglob < 0 ) hglob = 0 ;
   }

   nneed = hglob + (datum_len*nx*ny+himage) * (long long)nz ;
   if( length < nneed ){
      ERROR_message(
        "image file %s is %lld bytes long but must be at least %lld bytes long\n"
        "  for hglobal=%lld himage=%d nx=%d ny=%d nz=%d and voxel=%d bytes\n",
        fname,length,nneed,hglob,himage,nx,ny,nz,datum_len ) ;
      fclose( imfile ) ;
      RETURN(NULL);
   }

   /*** read images from the file ***/

   INIT_IMARR(newar) ;

   for( kim=0 ; kim < nz ; kim++ ){
      koff = hglob + (kim+1)*himage + datum_len*nx*ny * (long long)kim ;
      fseeko( imfile, (off_t)koff, SEEK_SET ) ; /* 22 Mar 2007: fseek->fseeko */

      newim  = mri_new( nx , ny , datum_type ) ;
      imar   = mri_data_pointer( newim ) ;
      (void)fread( imar , datum_len , nx * ny , imfile ) ;
      if( swap ){
         mri_swapbytes( newim ) ;
         newim->was_swapped = 1 ;  /* 07 Mar 2002 */
      }

      if( nz == 1 ) mri_add_name( fname , newim ) ;
      else {
         sprintf( buf , "%s#%d" , fname,kim ) ;
         mri_add_name( buf , newim ) ;
      }

      ADDTO_IMARR(newar,newim) ;
   }

   fclose(imfile) ;
   RETURN(newar);
}

/*------------------------------------------------------------------------------*/

/*! Read one or more 2D images from a file.

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
           - GIF, TIFF, JPEG, BMP, PNG formats (thru filters)
           - List of ASCII numbers
           - pre-defined 2D file size in mri_read()
           - "Cox MRI" (if this is what you have, God help you, no one else can)

   \return A pointer to an array of 2D images.  If nothing
           could be read, NULL is returned.
*/

MRI_IMARR * mri_read_file( char * fname )
{
   MRI_IMARR *newar=NULL ;
   MRI_IMAGE *newim ;
   char *new_fname ;
   int tried_dicom=0 ;

ENTRY("mri_read_file") ;

   /* convert fname to new_fname, based on environment */

   new_fname = imsized_fname( fname ) ;
   if( new_fname == NULL ) RETURN( NULL );

   /* input method is based on filename */

   if( strlen(new_fname) > 9 &&
       new_fname[0] == '3'   &&
       new_fname[1] == 'D'   &&
      (new_fname[2] == ':' || new_fname[3] == ':') ){

      newar = mri_read_3D( new_fname ) ;   /* read from a 3D: file */

   } else if( strlen(new_fname) > 9 &&
              new_fname[0] == '3' && new_fname[1] == 'A' && new_fname[3] == ':' ){

      newar = mri_read_3A( new_fname ) ;   /* from a 3A: file */

   /*-- from a dataset? [10 Dec 2007] --*/

   } else if( strstr(fname,".HEAD") != NULL || strstr(fname,".nii") != NULL ){

     THD_3dim_dataset *dset = THD_open_dataset(fname) ;
     if( dset != NULL ){
       int ii,jj ; MRI_IMAGE *qim ; void *qar ; MRI_IMARR *qimar ;
       DSET_load(dset) ; INIT_IMARR(newar) ;
       for( ii=0 ; ii < DSET_NVALS(dset) ; ii++ ){
         qim = DSET_BRICK(dset,ii) ; qar = mri_data_pointer(qim) ;
         if( qim != NULL && qar != NULL ){
           qimar = mri_to_imarr( qim ) ;
           if( qimar != NULL ){
             for( jj=0 ; jj < IMARR_COUNT(qimar) ; jj++ )
               ADDTO_IMARR(newar,IMARR_SUBIM(qimar,jj)) ;
             FREE_IMARR(qimar) ;
           }
         }
       }
       DSET_delete(dset) ; RETURN(newar) ;
     }

   } else if( check_dicom_magic_num( new_fname ) ) { /* 10 Aug 2004 */

     newar = mri_read_dicom( new_fname );  tried_dicom=2 ;

   } else if( strstr(new_fname,".hdr") != NULL ||
              strstr(new_fname,".HDR") != NULL   ){  /* 05 Feb 2001 */

      newar = mri_read_analyze75( new_fname ) ;      /* ANALYZE .hdr/.img filepair */

   } else if( strstr(new_fname,".ima") != NULL ||
              strstr(new_fname,".IMA") != NULL   ){  /* 12 Mar 2001 */

      newar = mri_read_siemens( new_fname ) ;        /* Siemens file */

   } else if( strncmp(new_fname,"I.",2) == 0    ||  /* GE I.* files */
              strstr(new_fname,"/I.")   != NULL ||
              strstr(new_fname,".ppm")  != NULL ||  /* raw PPM or PGM files */
              strstr(new_fname,".pgm")  != NULL ||
              strstr(new_fname,".pnm")  != NULL ||
              strstr(new_fname,".PPM")  != NULL ||
              strstr(new_fname,".PNM")  != NULL ||
              strstr(new_fname,".PGM")  != NULL   ){ /* 05 Nov 2002 */

      newim = mri_read( new_fname ) ;      /* read from a 2D file with 1 slice */

      if ( newim == NULL )                 /* GEMS 4.x - 03 Jun 2003 [rickr] */
         newim = mri_read_ge4( new_fname ) ;

      if( newim != NULL ){
        INIT_IMARR(newar) ;
        ADDTO_IMARR(newar,newim) ;
      }

   } else if( strncmp(new_fname,"i.",2) == 0    ||  /* GEMS 4.x i.* files  */
              strstr(new_fname,"/i.")   != NULL ){  /* 03 Jun 2003 [rickr] */

      newim = mri_read_ge4( new_fname ) ;          /* 2D file with 1 slice */

      if( newim != NULL ){
        INIT_IMARR(newar) ;
        ADDTO_IMARR(newar,newim) ;
      }

   } else if( strstr(new_fname,".jpg" ) != NULL ||  /* various formats  */
              strstr(new_fname,".JPG" ) != NULL ||  /* that we convert  */
              strstr(new_fname,".jpeg") != NULL ||  /* to PPG/PGM using */
              strstr(new_fname,".JPEG") != NULL ||  /* external filters */
              strstr(new_fname,".gif" ) != NULL ||
              strstr(new_fname,".GIF" ) != NULL ||
              strstr(new_fname,".tif" ) != NULL ||
              strstr(new_fname,".TIF" ) != NULL ||
              strstr(new_fname,".tiff") != NULL ||
              strstr(new_fname,".TIFF") != NULL ||
              strstr(new_fname,".bmp" ) != NULL ||
              strstr(new_fname,".BMP" ) != NULL ||
              strstr(new_fname,".pbm" ) != NULL ||
              strstr(new_fname,".PBM" ) != NULL ||
              strstr(new_fname,".png" ) != NULL ||
              strstr(new_fname,".PNG" ) != NULL   ){ /* 22 Nov 2002 */

      newim = mri_read_stuff( new_fname ) ;
      if( newim != NULL ){
        INIT_IMARR(newar) ;
        ADDTO_IMARR(newar,newim) ;
      }

   } else if( strstr(new_fname,".mpg" ) != NULL ||  /* 03 Dec 2003 */
              strstr(new_fname,".MPG" ) != NULL ||  /* read MPEGs  */
              strstr(new_fname,".mpeg") != NULL ||
              strstr(new_fname,".MPEG") != NULL   ){

      newar = mri_read_mpeg( new_fname ) ;  /* cf. mri_read_mpeg.c */
   }

   /** failed to read anything?  try DICOM format (doesn't have a fixed suffix) **/
   /* 05 May 2003 added option to try DICOM last                    KRH          */

   if( newar == NULL ){

      if ( !AFNI_yesenv("AFNI_TRY_DICOM_LAST")) {
        if( !tried_dicom ){
          newar = mri_read_dicom( new_fname ) ; tried_dicom = 1 ;
        }
      }

      /** if DICOM failed, try a 2D slice file, hope for the best **/

      if( newar == NULL && tried_dicom != 2 ){
        newim = mri_read( new_fname ) ;
        if( newim == NULL ){ free(new_fname); RETURN( NULL ); }  /* give up */
        INIT_IMARR(newar) ;
        ADDTO_IMARR(newar,newim) ;
      }

      if( newar == NULL && AFNI_yesenv("AFNI_TRY_DICOM_LAST") ){
        if( !tried_dicom ){
          newar = mri_read_dicom( new_fname ) ; tried_dicom = 1 ;
        }
      }
   }

   free(new_fname) ;  /* done with the mangled filename */

   /* 07 Mar 2002: add fname to the images, if needed */

   if( newar != NULL && newar->num > 0 ){
     int ii ;
     for( ii=0 ; ii < newar->num ; ii++ ){
       newim = IMARR_SUBIM(newar,ii) ;
       if( newim != NULL && newim->fname == NULL )
         newim->fname = strdup(fname) ;
     }
   }

   RETURN( newar );
}

/*-----------------------------------------------------------------*/

/*! Like mri_read_file(), but will only return 1 2D image.

    If the input file has more than 1 slice, or cannot be read,
    then NULL is returned.
*/

MRI_IMAGE * mri_read_just_one( char * fname )
{
   MRI_IMARR * imar ;
   MRI_IMAGE * im ;
   char * new_fname ;

ENTRY("mri_read_just_one") ;

   new_fname = imsized_fname( fname ) ;
   if( new_fname == NULL ) RETURN( NULL );

   imar = mri_read_file( new_fname ) ; free(new_fname) ;
   if( imar == NULL ) RETURN( NULL );
   if( imar->num != 1 ){ DESTROY_IMARR(imar) ; RETURN( NULL ); }
   im = IMAGE_IN_IMARR(imar,0) ;
   FREE_IMARR(imar) ;
   RETURN( im );
}

/*-----------------------------------------------------------------
  return a count of how many 2D images will be read from this file
-------------------------------------------------------------------*/

/*! Return a count of how many 2D images are in a file.

    Used by to3d.c to figure out how many slices will be read
    later using mri_read_file().  Return value is 0 if the images
    can't be counted.  If you add a new file type to mri_read_file(),
    then you need to modify this function as well!
*/

static int mri_imcount_analyze75( char * ) ;  /* prototype */
static int mri_imcount_siemens( char * ) ;

int mri_imcount( char *tname )
{
   int hglobal , himage , nx , ny , nz , ngood ;
   char fname[256]="\0" ;
   char *new_fname ;

ENTRY("mri_imcount") ;

   if( tname == NULL ) RETURN( 0 );
   new_fname = imsized_fname( tname ) ;
   if( new_fname == NULL ) RETURN( 0 );

   /*** a 3D filename ***/

   if( strlen(new_fname) > 9 && new_fname[0] == '3' && new_fname[1] == 'D' &&
       (new_fname[2] == ':' || new_fname[3] == ':') ){
                               /* check for ':', too   3 Jan 2005 [rickr] */
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

         case 'd':                                            /* 06 Feb 2003 */
            ngood = sscanf( new_fname , "3Dd:%d:%d:%d:%d:%d:%s" ,
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

         case 'r':
            ngood = sscanf( new_fname , "3Dr:%d:%d:%d:%d:%d:%s" ,
                            &hglobal , &himage , &nx , &ny , &nz , fname ) ;
            break ;
      }

      free( new_fname ) ;
      if( ngood < 6 || himage < 0 ||
          nx <= 0   || ny <= 0    || nz <= 0 ||
          strlen(fname) <= 0                       ) RETURN( 0 );
      else                                           RETURN( nz );
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
      if( ngood < 4 || nx <= 0 || ny <= 0 || nz <= 0 || strlen(fname) <= 0 ) RETURN( 0 );
      else                                                                   RETURN( nz );
   }

   /*** 05 Feb 2001: deal with ANALYZE .hdr files ***/

   if( strstr(new_fname,".hdr") != NULL ||
       strstr(new_fname,".HDR") != NULL   ){

      nz = mri_imcount_analyze75( new_fname ) ;
      if( nz > 0 ){ free(new_fname); RETURN(nz); }
   }

   if( strstr(new_fname,".ima") != NULL ||
       strstr(new_fname,".IMA") != NULL   ){        /* 12 Mar 2001 */

      nz = mri_imcount_siemens( new_fname ) ;
      if( nz > 0 ){ free(new_fname); RETURN(nz); }
   }

   if( strstr(new_fname,".mpg" ) != NULL ||  /* 03 Dec 2003 */
       strstr(new_fname,".MPG" ) != NULL ||
       strstr(new_fname,".mpeg") != NULL ||
       strstr(new_fname,".MPEG") != NULL   ){

      nz = mri_imcount_mpeg( new_fname ) ;
      if( nz > 0 ){ free(new_fname); RETURN(nz); }
   }

   /*** 19 Jul 2002: see if it is a DICOM file ***/

   mri_dicom_seterr(0) ;
   nz = mri_imcount_dicom( new_fname ) ;  /* cf. mri_read_dicom.c */
   mri_dicom_seterr(1) ;
   if( nz > 0 ){ free(new_fname); RETURN(nz); }

   /*** not recognized ***/

   free(new_fname) ; RETURN(1) ;    /* assume it has 1 image in it, somewhere */
}

/*--------------------------------------------------------------*/

/*! Like mri_read_file(), but returns images from many files.

    \param nf = Number of file names
    \param fn = Array of file name strings
    \return An array of 2D images (NULL if nothing was found)

    Added 07 Mar 1995
*/

MRI_IMARR * mri_read_many_files( int nf , char * fn[] )
{
   MRI_IMARR * newar , * outar ;
   int kf , ii ;

ENTRY("mri_read_many_files") ;

   if( nf <= 0 ) RETURN( NULL );  /* no inputs! */
   INIT_IMARR(outar) ;          /* initialize output array */

   for( kf=0 ; kf < nf ; kf++ ){
      newar = mri_read_file( fn[kf] ) ;  /* read all images in this file */

      if( newar == NULL ){  /* none?  flush the output array! */
         fprintf(stderr,"cannot read images from file %s\n",fn[kf]) ;
         for( ii=0 ; ii < outar->num ; ii++ ) mri_free(outar->imarr[ii]) ;
         FREE_IMARR(outar) ;
         RETURN( NULL );
      }

      for( ii=0 ; ii < newar->num ; ii++ )  /* move images to output array */
         ADDTO_IMARR( outar , newar->imarr[ii] ) ;

      FREE_IMARR(newar) ;  /* don't need this no more */
   }
   RETURN( outar );
}

/*! Like mri_read_many_files(), but forces images to a certain resolution.

    \param nf = Number of file names
    \param fn = Array of file name strings
    \param nx = number of pixels
    \param ny   in x and y directions
               if nx is negative, then nx and ny are set
               to be the dimensions of the very first image
               read.
    \return An array of 2D images (NULL if nothing was found)

    Added Jan 07
*/
MRI_IMARR * mri_read_resamp_many_files( int nf, char * fn[] , int nxnew, int nynew)
{
   MRI_IMARR * newar , * outar ;
   int kf , ii, nxi, nyi ;
   MRI_IMAGE * bim, *qim, *imin;

   ENTRY("mri_read_resamp_many_files") ;

   if( nf <= 0 ) RETURN( NULL );  /* no inputs! */
   INIT_IMARR(outar) ;          /* initialize output array */

   for( kf=0 ; kf < nf ; kf++ ){
      newar = mri_read_file( fn[kf] ) ;  /* read all images in this file */

      if( newar == NULL ){  /* none?  flush the output array! */
         fprintf(stderr,"cannot read images from file %s\n",fn[kf]) ;
         for( ii=0 ; ii < outar->num ; ii++ ) mri_free(outar->imarr[ii]) ;
         FREE_IMARR(outar) ;
         RETURN( NULL );
      }
      if (nxnew < 0 && kf == 0) { /* set dimensions based on 1st image */
         nxnew = newar->imarr[0]->nx;
         nynew = newar->imarr[0]->ny;
      }

      for( ii=0 ; ii < newar->num ; ii++ )  {/* move images to output array */
         imin = newar->imarr[ii];
         nxi = imin->nx;
         nyi = imin->ny;
         if (nxi != nxnew || nyi != nynew) { /* resampling needed (adapted from galler.c)*/
            float fx , fy ;
            fx = nxnew / (float)nxi ; fy = nynew / (float)nyi ;
            fx = MIN(fx,fy) ;
            /* fprintf(stderr,"Resizing from %dx%d to %dx%d.\n fx = %.3f\n", nxi, nyi, nxnew, nynew, fx); */
            if( fx < 0.95f ){
               float sigma = 0.3456789f/fx ;
               /* fprintf(stderr,"sigma %f\n", sigma); */
               if (imin->kind == MRI_rgb) {
                  bim = mri_rgb_blur2D( sigma , imin ) ;
               } else {
                  bim = mri_byte_blur2D( sigma , imin ) ;
               }
            } else bim = imin ;
            qim = mri_resize( bim , nxnew , nynew ) ;
            /* fprintf(stderr,"qim now %dx%d\n", qim->nx, qim->ny); */
            ADDTO_IMARR( outar , qim ) ;
            if( bim != imin ) mri_free(bim) ;
            mri_free( imin );
         } else {
            ADDTO_IMARR( outar , imin ) ;
         }
      }

      FREE_IMARR(newar) ;  /* don't need this no more */
   }

   RETURN( outar );
}

/*---------------------------------------------------------------*/

/*! Read a raw PPM file into 3 byte-valued MRI_IMAGEs.

    \date 16 May 1995
*/

MRI_IMARR * mri_read_ppm3( char * fname )
{
   int ch , nch , nx,ny,maxval , length , npix,ii ;
   char buf[512] ;
   MRI_IMAGE *rim , *gim , *bim ;
   MRI_IMARR * outar ;
   FILE * imfile ;
   byte * rby , * gby , * bby , * rgby ;

ENTRY("mri_read_ppm3") ;

   /*** open input file ***/

   imfile = fopen( fname , "r" ) ;
   if( imfile == NULL ){
      fprintf(stderr,"couldn't open file %s in mri_read_ppm3\n",fname); RETURN(NULL) ;
   }

   /*** check if a raw PPM file ***/

   ch = getc( imfile ) ; if( ch != 'P' ) { fclose(imfile) ; RETURN(NULL); }
   ch = getc( imfile ) ; if( ch != '6' ) { fclose(imfile) ; RETURN(NULL); }

   /* magic P6 found, so read numbers in header */

   ch = getc(imfile) ;

   NUMSCAN(nx)     ; if( nx     <= 0 )   { fclose(imfile) ; RETURN(NULL); }
   NUMSCAN(ny)     ; if( ny     <= 0 )   { fclose(imfile) ; RETURN(NULL); }
   NUMSCAN(maxval) ; if( maxval <= 0 ||
                         maxval >  255 ) { fclose(imfile) ; RETURN(NULL); }

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
      RETURN(NULL);
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
   RETURN(outar);
}

/*-----------------------------------------------------------------
   routines added 1 Oct 1995
-------------------------------------------------------------------*/

/*! Read 1 2D image, then "nsize" it - make it a power of 2 in sizes.

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

/*! Read many 2D images from many files. */

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

/*! Set up MCW_SIZE_# database for input.

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
/*! My version of strdup(), which won't fail if the input is NULL. */

char * my_strdup( char * str )
{
   char * new_str ;
   if( str == NULL ) return NULL ;
   new_str = (char *) malloc( sizeof(char) * (strlen(str)+1) ) ;
   if( new_str != NULL ) strcpy( new_str , str ) ;
   return new_str ;
}

/*------------------------------------------------------------------------------*/

/*! Check if a filesize fits an MCW_IMSIZE setup.

    \param fname = Filename
    \return A new "filename" with 3D header attached if it fits.
            If not, return a copy of the filename.  In any case the
            returned string should be free()-d when it is no longer needed.
*/

char * imsized_fname( char * fname )
{
   int num , lll ;
   long long len ;  /* 22 Mar 2007 */
   char * new_name ;

   init_MCW_sizes() ;
   if( MCW_imsize_good == 0 ){
      new_name = my_strdup(fname) ;  /* nothing to fit */
      return new_name ;              /* --> return copy of old name */
   }

   len = THD_filesize( fname ) ;
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

#if 0  /* removed on 22 Mar 2007 */
/*------------------------------------------------------------------------*/
/*! Return the size of a file in bytes.

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
#endif

/*---------------------------------------------------------------*/

/*! Read the header from PPM file and return its info.

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

/*---------------------------------------------------------------*/

/*! Reads a raw PPM file into 1 2D MRI_rgb-valued image.

   \param fname = Image filename
   \return An MRI_IMAGE if things worked OK; NULL if not
   \date 13 May 1996
*/

MRI_IMAGE * mri_read_ppm( char * fname )
{
   int ch , nch , nx,ny,maxval , length ;
   MRI_IMAGE * rgbim ;
   FILE      * imfile ;
   byte      * rgby ;
   char        buf[256] ;

ENTRY("mri_read_ppm") ;

   /*** open input file ***/

   imfile = fopen( fname , "r" ) ;
   if( imfile == NULL ) RETURN(NULL);

   /*** check if a raw PPM file ***/

   ch = getc( imfile ) ; if( ch != 'P' ) { fclose(imfile) ; RETURN(NULL); }
   ch = getc( imfile ) ; if( ch != '6' ) { fclose(imfile) ; RETURN(NULL); }

   /* magic P6 found, so read numbers in header */

   ch = getc(imfile) ;

   NUMSCAN(nx)    ; if( nx     <= 0 )  { fclose(imfile); RETURN(NULL); }
   NUMSCAN(ny)    ; if( ny     <= 0 )  { fclose(imfile); RETURN(NULL); }
   NUMSCAN(maxval); if( maxval <= 0 ||
                        maxval >  255 ){ fclose(imfile); RETURN(NULL); }

   /*** create output image ***/

   rgbim = mri_new( nx , ny , MRI_rgb ) ; mri_add_name( fname , rgbim ) ;
   rgby  = MRI_RGB_PTR(rgbim) ;

   /*** read all data into image array */

   length = fread( rgby , sizeof(byte) , 3*nx*ny , imfile ) ;
   fclose( imfile ) ;

   if( length != 3*nx*ny ){ mri_free(rgbim) ; RETURN(NULL) ; }

   /* 17 Sep 2001: scale to maxval=255, if needed */

   if( maxval < 255 ){
      int ii ; float fac = 255.4/maxval ;
      for( ii=0 ; ii < 3*nx*ny ; ii++ ) rgby[ii] = (byte)( rgby[ii]*fac ) ;
   }

   RETURN(rgbim) ;
}
/*---------------------------------------------------------------*/

/*! Length of line buffer for mri_read_ascii() */
/* rcr - improve this */
#define LBUF 2524288  /* 08 Jul 2004: increased to 512K from 64K */

/*! Free a buffer and set it to NULL */
#define FRB(b) do{ if( (b)!=NULL ){free((b)); (b)=NULL;} }while(0)

#undef USE_LASTBUF

/*---------------------------------------------------------------*/
/*! [20 Jun 2002] Like fgets, but also
     - skips blank or comment lines
     - skips leading and trailing whitespace
     - catenates lines that end in '\' (replacing '\' with ' ')
     - returns duplicate of last line if first 2
        nonblank input characters are "" [20 Jul 2004]
-----------------------------------------------------------------*/

static char * my_fgets( char *buf , int size , FILE *fts )
{
   char *ptr ;
   int nbuf , ll,ii , cflag ;
   static char *qbuf=NULL ;

#ifdef USE_LASTBUF
   static char *lastbuf = NULL ;   /* 20 Jul 2004 */
   static int  nlastbuf = 0 ;

   if( buf == NULL && lastbuf != NULL ){    /* 20 Jul 2004 */
     free((void *)lastbuf); lastbuf = NULL; nlastbuf = 0 ;
   }
#endif

   if( buf == NULL && qbuf != NULL ){ free((void *)qbuf); qbuf = NULL; }

   if( buf == NULL || size < 1 || fts == NULL ) return NULL ;

   if( qbuf == NULL ) qbuf = AFMALL(char, LBUF) ;  /* 1st time in */

   nbuf  = 0 ;  /* num bytes stored in buf so far */
   cflag = 0 ;  /* flag if we're catenating lines */

   while(1){   /* loop and read lines, creating a logical line */

     ptr = fgets( qbuf , LBUF , fts ) ; /* read next whole line */

     if( ptr == NULL ) break ;          /* must be end-of-file */

     /* skip leading whitespace */

     for( ; *ptr != '\0' && isspace(*ptr) ; ptr++ ) ; /* nada */

     /* skip entirely blank lines, unless we are catenating */

     if( *ptr == '\0' ){ if(cflag) break; else continue; }

#ifdef USE_LASTBUF
     /* if a duplicate is requested, return it now [20 Jul 2004] */

     if( *ptr == '"' && *(ptr+1) == '"' && nlastbuf > 0 && nbuf == 0 ){
       ll = strlen(lastbuf) ; if( ll >= size ) ll = size-1 ;
       memcpy(buf,lastbuf,ll-1) ; buf[ll] = '\0' ;
       return buf ;
     }
#endif

     /* skip comment lines (even if we are catenating) */

     if( *ptr == '#' || (*ptr == '/' && *(ptr+1) == '/') ) continue ;

     /* strip trailing whitespace */

     ll = strlen(ptr) ;                                  /* will be > 0 */
     for( ii=ll-1 ; isspace(ptr[ii]) && ii > 0 ; ii-- )  /* blank => NUL */
       ptr[ii] = '\0' ;

     ll = strlen(ptr) ;                 /* number of chars left */
     if( ll == 0 ) continue ;           /* should not happen */

     cflag = (ptr[ll-1] == '\\') ;      /* catenate next line? */
     if( cflag ) ptr[ll-1] = ' ' ;      /* replace '\' with ' ' */

     /* now copy what's left (ll+1 bytes) at tail of output buffer */

     if( nbuf+ll+1 > size ){   /* too much for output buffer? */
       ll = size - (nbuf+1) ;
       if( ll <= 0 ) break ;   /* should not happen */
     }

     memcpy(buf+nbuf,ptr,ll+1) ; nbuf += ll ;
     if( !cflag ) break ;

   } /* loop to get next line if catenation is turned on */

#ifdef LASTBUF
   /* make a copy of result in lastbuf [20 Jul 2004] */

   ll = strlen(buf) ;
   if( ll+1 > nlastbuf ){
     nlastbuf = ll+2 ; lastbuf = (char *)realloc((void *)lastbuf,nlastbuf) ;
   }
   memcpy(lastbuf,buf,ll+1) ;
#endif

   /* and we is done */

   if( nbuf > 0 ) return buf ;      /* return what we read already */
   return NULL ;                    /* signal of failure get data  */
}

/*--------------------------------------------------------------*/
static float lbfill = 0.0f ;  /* 10 Aug 2004 */
static int oktext = 0;
/*--------------------------------------------------------------*/

/* return a 1 if c is a not a valid first non-white char of a 
   non-comment 1D line */
byte iznogood_1D (char c) 
{
   if ( (c < '0' || c > '9')  &&
         c != '+' && c != '-' && c != '.' && c != 'e' && 
         c != 'i' && c != ',' && /* allow for complex input */
         c != '@' && c != '*'    /* allow for special 1D trickery */ 
      ) return 1;
   else return 0;
          
}
/*! Decode a line buffer into an array of floats.               */
static floatvec * decode_linebuf( char *buf )  /* 20 Jul 2004 */
{
   floatvec *fv=NULL ;
   int blen, bpos, ncol, ii, jj, temppos, count;
   int alloc_chunk, alloc_unit = 10000, incr;
   int n_alloced = 0, slowmo = 0 ; /* ZSS speedups */
   char sep, vbuf[64] , *cpt, *ope=NULL;
   float val ;
   
   if( buf == NULL || *buf == '\0' ) return fv ;

   blen = strlen(buf) ;
   ncol = 0 ;

   /* convert commas (or 'i' for complex numbers ZSS Oct 06) to blanks */
   /* note 'e' is commonly found in numeric files as in scientific notation*/
   for( ii=0 ; ii < blen ; ii++ ) {
         temppos = ii; incr = 0;
         if(isalpha(buf[ii])){
            /* skip past alphabetics in a row*/
            jj = ii;
            for( ; jj < blen && isalpha(buf[jj]) ; jj++ ) ;
            incr = jj - ii - 1; /* only move if more than 1 char long */
            if(incr) ii = jj;
        }

      /* convert some alphabetic characters to space (:,i)
         if they are not followed by other alphabetics */
         if((incr<=0) &&( buf[temppos] == ',' || buf[temppos] == 'i') ||
            buf[temppos] == ':' )
             buf[temppos] = ' ' ;
         /* turn on "slow mo" reading if non-numeric */
         if( !slowmo &&
           (buf[temppos] == '*' || buf[temppos] == '@' ||
            isalpha(buf[temppos])) ) slowmo = 1;
   }

   fv = (floatvec *)malloc(sizeof(floatvec)) ;
   fv->nar = 0 ;
   fv->ar  = (float *)NULL ;

   for( bpos=0 ; bpos < blen ; ){
     /* skip to next nonblank character */
     for( ; bpos < blen && isspace(buf[bpos]) ; bpos++ ) ; /* nada */
     if( bpos == blen ) break ;    /* end of line */


     val = 0.0 ; count = 1 ;
     if (slowmo) {   /* trickery */
        sscanf( buf+bpos , "%63s" , vbuf ) ;
        
        if (!oktext && iznogood_1D(vbuf[0])) {/* Morality Police Oct 16 09 */
            fv->nar = 0; /* this will cause a clean up on the way out */
                         /* If you do not do this, you would not get consistent*/
                         /* behavior if there is text in a matrix and the env */
                         /* AFNI_1D_ZERO_TEXT is not YES */
            break;   
        }
        
        if( vbuf[0] == '*' || isalpha(vbuf[0]) ){    /* 10 Aug 2004 */
          val = lbfill ;
        } else if( (cpt=strchr(vbuf,'@')) != NULL ){
          sscanf( vbuf , "%d%c%f" , &count , &sep , &val ) ;
          if( count < 1 ) count = 1 ;
          if( *(cpt+1) == '*' ) val = lbfill ;  /* 10 Aug 2004 */
        } else {
          sscanf( vbuf , "%f" , &val ) ;
        }
        incr = strlen(vbuf) ;
     } else {     /* no muss no fuss, take it straight */
        /* sscanf( vbuf , "%f" , &val ) ; slow, slow, tan go close*/
        val = strtod(buf+bpos, &ope);
        incr = ope - (buf+bpos);
     }
     if( incr <= 0 ) break ; /* 16 Oct 2007 */
     if (fv->nar+count > n_alloced) {
      /* fprintf(stderr,"reallocing past %d with count %d...\n", n_alloced, count); */
      if (count > alloc_unit) alloc_chunk = count;
      else alloc_chunk = alloc_unit;
      fv->ar = (float *)realloc( (void *)fv->ar , sizeof(float)*(n_alloced+alloc_chunk) );
      n_alloced = n_alloced + alloc_chunk;
     }
     for( ii=0 ; ii < count ; ii++ ) fv->ar[ii+fv->nar] = val ;
     fv->nar += count ;
     bpos += incr ;
   }

   if( fv->nar == 0 ){ KILL_floatvec(fv); fv = NULL; }
   else { if (fv->nar < n_alloced) fv->ar = (float *)realloc( (void *)fv->ar , sizeof(float)*(fv->nar) ); }
   return fv ;
}

static doublevec * decode_double_linebuf( char *buf )  /* 20 Jul 2004 */
{
   doublevec *dv=NULL ;
   int blen, bpos, ncol, ii, jj, temppos, count ;
   int alloc_chunk, alloc_unit = 10000, incr;
   int n_alloced = 0, slowmo = 0 ; /* ZSS speedups */
   char sep, vbuf[64] , *cpt , *ope=NULL;
   double val ;

   if( buf == NULL || *buf == '\0' ) return dv ;

   blen = strlen(buf) ;
   ncol = 0 ;

   /* convert commas (or 'i' for complex numbers ZSS Oct 06) to blanks */
   /* note 'e' is commonly found in numeric files as in scientific notation*/
   for( ii=0 ; ii < blen ; ii++ ) {
         temppos = ii; incr = 0;
         if(isalpha(buf[ii])){
            /* skip past alphabetics in a row*/
            jj = ii;
            for( ; jj < blen && isalpha(buf[jj]) ; jj++ ) ;
            incr = jj - ii - 1; /* only move if more than 1 char long */
            if(incr) ii = jj;
        }

      /* convert some alphabetic characters to space (:,i)
         if they are not followed by other alphabetics */
         if((incr<=0) &&( buf[temppos] == ',' || buf[temppos] == 'i') ||
            buf[temppos] == ':' )
             buf[temppos] = ' ' ;
         /* turn on "slow mo" reading if non-numeric */
         if( !slowmo &&
           (buf[temppos] == '*' || buf[temppos] == '@' ||
            isalpha(buf[temppos])) ) slowmo = 1;
   }

   dv = (doublevec *)malloc(sizeof(doublevec)) ;
   dv->nar = 0 ;
   dv->ar  = (double *)NULL ;

   for( bpos=0 ; bpos < blen ; ){
     /* skip to next nonblank character */

     for( ; bpos < blen && isspace(buf[bpos]) ; bpos++ ) ; /* nada */
     if( bpos == blen ) break ;    /* end of line */


     val = 0.0 ; count = 1 ;
     if (slowmo) {   /* trickery */
        sscanf( buf+bpos , "%63s" , vbuf ) ;
        if( vbuf[0] == '*' ){    /* 10 Aug 2004 */
          val = (double)lbfill ;
        } else if( (cpt=strchr(vbuf,'@')) != NULL ){
          sscanf( vbuf , "%d%c%lf" , &count , &sep , &val ) ;
          if( count < 1 ) count = 1 ;
          if( *(cpt+1) == '*' ) val = (double)lbfill ;  /* 10 Aug 2004 */
        } else {
          sscanf( vbuf , "%lf" , &val ) ;
        }
        incr = strlen(vbuf) ;
     } else {     /* no muss no fuss, take it straight */
        /* sscanf( vbuf , "%f" , &val ) ; slow, slow, tan go close*/
        val = strtod(buf+bpos, &ope);
        incr = ope - (buf+bpos);
     }
     if (dv->nar+count > n_alloced) {
      /* fprintf(stderr,"reallocing past %d with count %d...\n", n_alloced, count); */
      if (count > alloc_unit) alloc_chunk = count;
      else alloc_chunk = alloc_unit;
      dv->ar = (double *)realloc( (void *)dv->ar , sizeof(double)*(n_alloced+alloc_chunk) );
      n_alloced = n_alloced + alloc_chunk;
     }

     for( ii=0 ; ii < count ; ii++ ) dv->ar[ii+dv->nar] = val ;
     dv->nar += count ;
     bpos += incr ;
   }

   if( dv->nar == 0 ){ KILL_doublevec(dv); dv = NULL; }
   else { if (dv->nar < n_alloced) dv->ar = (double *)realloc( (void *)dv->ar , sizeof(double)*(dv->nar) ); }
   return dv ;
}

/*---------------------------------------------------------------*/

/*! Increment for time series array size for mri_read_ascii() */
#define INC_TSARSIZE 128

/*! Read an array of ASCII numbers into a 1D or 2D image.

  \param fname = input filename
  \return Pointer to MRI_IMAGE (in MRI_float) format if things
          are cool; NULL if not.
  \date Jun 1996

  Example input:
     - Line 1:  3 4 6
     - Line 2:  2 2 2
     - Line 3:  7 2 1
     - Line 4:  9 9 6
  This produces an image with nx=3 and ny=4.  The first row
  is read to determine nx; all subsequent rows must have nx
  values.  A line whose very first character is a '#' will
  be skipped as a comment.  A line with no characters (just
  the '\n') will also be skipped.

  20 Jun 2002: modified to use my_fgets() instead of fgets().
*/

MRI_IMAGE * mri_read_ascii( char *fname )
{
   MRI_IMAGE *outim ;
   int ii,jj,val , used_tsar , alloc_tsar ;
   float *tsar ;
   float ftemp ;
   FILE *fts ;
   char *ptr ;
   int  ncol , bpos , blen , nrow ;
   static char *buf=NULL ;            /* 20 Jun 2002: make a ptr */

   floatvec *fvec ;                   /* 20 Jul 2004 */
   int incts ;

ENTRY("mri_read_ascii") ;

   if (AFNI_yesenv("AFNI_1D_ZERO_TEXT")) oktext = 1;  /* ZSS Oct 16 09 */
   else oktext = 0;
   
   if( fname == NULL || fname[0] == '\0' ) RETURN(NULL) ;

STATUS(fname) ;  /* 16 Oct 2007 */

   if( strncmp(fname,"1D:",3) == 0 ){         /* 28 Apr 2003 */
     MRI_IMAGE *qim = mri_1D_fromstring( fname+3 ) ;
     if( qim != NULL ){
       outim = mri_transpose(qim); mri_free(qim); RETURN(outim);
     }
   }

   fts = fopen( fname , "r" ); if( fts == NULL ) RETURN(NULL);

   if( buf == NULL ) buf = AFMALL(char, LBUF) ; /* create buffer */

   /** step 1: read in the first line and see how many numbers are in it
               (skipping lines that are comments or entirely blank)     */

   (void) my_fgets( NULL , 0 , NULL ) ;  /* reset [20 Jul 2004] */
   ptr = my_fgets( buf , LBUF , fts ) ;
   if( ptr==NULL || *ptr=='\0' ){ FRB(buf); fclose(fts); RETURN(NULL); }  /* bad read? */

   lbfill = 0.0f ;                          /* 10 Aug 2004 */

   fvec = decode_linebuf( buf ) ;           /* 20 Jul 2004 */
   if( fvec == NULL || fvec->nar == 0 ){
     if( fvec != NULL ) KILL_floatvec(fvec) ;
     FRB(buf); fclose(fts); RETURN(NULL);
   }
   ncol = fvec->nar ; KILL_floatvec(fvec) ;

   /** At this point, ncol is the number of floats to be read from each line **/

   rewind( fts ) ;  /* will start over */

   incts      = MAX(INC_TSARSIZE,ncol) ;
   used_tsar  = 0 ;
   alloc_tsar = incts ;
   tsar       = (float *) malloc( sizeof(float) * alloc_tsar ) ;
   if( tsar == NULL ){
      fprintf(stderr,"\n*** malloc error in mri_read_ascii ***\n"); EXIT(1);
   }

   /** read lines, convert to floats, store **/

   nrow = 0 ;
   while( 1 ){
     ptr = my_fgets( buf , LBUF , fts ) ;  /* read, skipping comments*/
     if( ptr==NULL || *ptr=='\0' ) break ; /* failure --> end of data */

     fvec = decode_linebuf( buf ) ;
     if( fvec == NULL ) break ;
     if( fvec->nar == 0 ){ KILL_floatvec(fvec); break; }

     if( used_tsar + ncol >= alloc_tsar ){
        alloc_tsar += incts ;
        tsar        = (float *)realloc( (void *)tsar,sizeof(float)*alloc_tsar );
        if( tsar == NULL ){
          fprintf(stderr,"\n*** realloc error in mri_read_ascii ***\n"); EXIT(1);
        }
     }
     for( ii=0 ; ii < fvec->nar && ii < ncol ; ii++ )
       tsar[used_tsar+ii] = fvec->ar[ii] ;
     for( ; ii < ncol ; ii++ )
       tsar[used_tsar+ii] = 0.0 ;
     used_tsar += ncol ;
     KILL_floatvec(fvec) ;

     nrow++ ;                  /* got one more complete row! */
   }
   fclose( fts ) ; /* finished with this file! */
   (void) my_fgets( NULL , 0 , NULL ) ;  /* reset [20 Jul 2004] */

   /* from <= 1 to < 1 (allow 1x1 image) 25 Jan 2006 [rickr] */
   if( used_tsar < 1 ){ FRB(buf); free(tsar); RETURN(NULL); }

   tsar = (float *) realloc( tsar , sizeof(float) * used_tsar ) ;
   if( tsar == NULL ){
      fprintf(stderr,"\n*** final realloc error in mri_read_ascii ***\n"); EXIT(1);
   }

   outim = mri_new_vol_empty( ncol , nrow , 1 , MRI_float ) ;
   mri_fix_data_pointer( tsar , outim ) ;
   mri_add_name( fname , outim ) ;

   FRB(buf) ; RETURN(outim) ;
}

/*---------------------------------------------------------------*/

MRI_IMAGE * mri_read_double_ascii( char * fname )
{
   MRI_IMAGE * outim ;
   int ii,jj,val , used_tsar , alloc_tsar ;
   double * dtsar ;
   double dtemp ;
   FILE * fts ;
   char * ptr ;
   int  ncol , bpos , blen , nrow ;
   static char *buf=NULL ;            /* 20 Jun 2002: make a ptr */

   doublevec *dvec ;                   /* 20 Jul 2004 */
   int incts ;

ENTRY("mri_read_double_ascii") ;

   if( fname == NULL || fname[0] == '\0' ) RETURN(NULL) ;

   if( strncmp(fname,"1D:",3) == 0 ){         /* 28 Apr 2003 */
     /*
     MRI_IMAGE *qim = mri_1D_double_fromstring( fname+3 ) ;
     if( qim != NULL ){
       outim = mri_transpose(qim); mri_free(qim); RETURN(outim);
     }*/
     fprintf(stderr,"Somebody was too lazy to allow this option here.\n"); RETURN(NULL);
   }

   fts = fopen( fname , "r" ); if( fts == NULL ) RETURN(NULL);

   if( buf == NULL ) buf = AFMALL(char, LBUF) ; /* create buffer */

   /** step 1: read in the first line and see how many numbers are in it
               (skipping lines that are comments or entirely blank)     */

   (void) my_fgets( NULL , 0 , NULL ) ;  /* reset [20 Jul 2004] */
   ptr = my_fgets( buf , LBUF , fts ) ;
   if( ptr==NULL || *ptr=='\0' ){ FRB(buf); fclose(fts); RETURN(NULL); }  /* bad read? */

   lbfill = 0.0f ;                          /* 10 Aug 2004 */

   dvec = decode_double_linebuf( buf ) ;           /* 20 Jul 2004 */
   if( dvec == NULL || dvec->nar == 0 ){
     if( dvec != NULL ) KILL_doublevec(dvec) ;
     FRB(buf); fclose(fts); RETURN(NULL);
   }
   ncol = dvec->nar ; KILL_doublevec(dvec) ;

   /** At this point, ncol is the number of floats to be read from each line **/

   rewind( fts ) ;  /* will start over */

   incts      = MAX(INC_TSARSIZE,ncol) ;
   used_tsar  = 0 ;
   alloc_tsar = incts ;
   dtsar       = (double *) malloc( sizeof(double) * alloc_tsar ) ;
   if( dtsar == NULL ){
      fprintf(stderr,"\n*** malloc error in mri_read_double_ascii ***\n"); EXIT(1);
   }

   /** read lines, convert to floats, store **/

   nrow = 0 ;
   while( 1 ){
     ptr = my_fgets( buf , LBUF , fts ) ;  /* read */
     if( ptr==NULL || *ptr=='\0' ) break ; /* failure --> end of data */

     dvec = decode_double_linebuf( buf ) ;
     if( dvec == NULL ) break ;
     if( dvec->nar == 0 ){ KILL_doublevec(dvec); break; }

     if( used_tsar + ncol >= alloc_tsar ){
        alloc_tsar += incts ;
        dtsar        = (double *)realloc( (void *)dtsar,sizeof(double)*alloc_tsar );
        if( dtsar == NULL ){
          fprintf(stderr,"\n*** realloc error in mri_read_double_ascii ***\n"); EXIT(1);
        }
     }
     for( ii=0 ; ii < dvec->nar && ii < ncol ; ii++ )
       dtsar[used_tsar+ii] = dvec->ar[ii] ;
     for( ; ii < ncol ; ii++ )
       dtsar[used_tsar+ii] = 0.0 ;
     used_tsar += ncol ;
     KILL_doublevec(dvec) ;

     nrow++ ;                  /* got one more complete row! */
   }
   fclose( fts ) ; /* finished with this file! */
   (void) my_fgets( NULL , 0 , NULL ) ;  /* reset [20 Jul 2004] */

   /* from <= 1 to < 1 (allow 1x1 image) 25 Jan 2006 [rickr] */
   if( used_tsar < 1 ){ FRB(buf); free(dtsar); RETURN(NULL); }

   dtsar = (double *) realloc( dtsar , sizeof(double) * used_tsar ) ;
   if( dtsar == NULL ){
      fprintf(stderr,"\n*** final realloc error in mri_read_double_ascii ***\n"); EXIT(1);
   }

   outim = mri_new_vol_empty( ncol , nrow , 1 , MRI_double ) ;
   mri_fix_data_pointer( dtsar , outim ) ;
   mri_add_name( fname , outim ) ;

   FRB(buf) ; RETURN(outim) ;
}

/*---------------------------------------------------------------*/

MRI_IMAGE * mri_read_complex_ascii( char * fname )
{
   MRI_IMAGE * outim ;
   int ii,jj,val , used_tsar , alloc_tsar, ih ;
   float * tsar ;
   complex *ctsar;
   float temp ;
   FILE * fts ;
   char * ptr ;
   int  ncol , bpos , blen , nrow ;
   static char *buf=NULL ;            /* 20 Jun 2002: make a ptr */

   floatvec *vec ;                   /* 20 Jul 2004 */
   int incts ;

ENTRY("mri_read_complex_ascii") ;

   if( fname == NULL || fname[0] == '\0' ) RETURN(NULL) ;

   if( strncmp(fname,"1D:",3) == 0 ){         /* 28 Apr 2003 */
     /*
     MRI_IMAGE *qim = mri_1D_complex_fromstring( fname+3 ) ;
     if( qim != NULL ){
       outim = mri_transpose(qim); mri_free(qim); RETURN(outim);
     }
     */
     fprintf(stderr,"Somebody was too lazy to allow this option here.\n"); RETURN(NULL);
   }

   fts = fopen( fname , "r" ); if( fts == NULL ) RETURN(NULL);

   if( buf == NULL ) buf = AFMALL(char, LBUF) ; /* create buffer */

   /** step 1: read in the first line and see how many numbers are in it
               (skipping lines that are comments or entirely blank)     */

   (void) my_fgets( NULL , 0 , NULL ) ;  /* reset [20 Jul 2004] */
   ptr = my_fgets( buf , LBUF , fts ) ;
   if( ptr==NULL || *ptr=='\0' ){ FRB(buf); fclose(fts); RETURN(NULL); }  /* bad read? */

   lbfill = 0.0f ;                          /* 10 Aug 2004 */

   vec = decode_linebuf( buf ) ;           /* 20 Jul 2004 */
   if( vec == NULL || vec->nar == 0 ){
     if( vec != NULL ) KILL_floatvec(vec) ;
     FRB(buf); fclose(fts); RETURN(NULL);
   }
   ncol = vec->nar ; KILL_floatvec(vec) ;
   if (ncol % 2) {
      fprintf(stderr,"\n*** File does not have even number of columns."
                     "\n    That is a must for complex 1D files.\n");
      RETURN(NULL);
   }
   /* fprintf(stderr,"Have ncol = %d\n", ncol);*/
   /** At this point, ncol is the number of floats to be read from each line **/

   rewind( fts ) ;  /* will start over */

   incts      = MAX(INC_TSARSIZE,ncol) ;
   used_tsar  = 0 ;
   alloc_tsar = incts ;
   tsar       = (float *) malloc( sizeof(float) * alloc_tsar ) ;
   if( tsar == NULL ){
      fprintf(stderr,"\n*** malloc error in mri_read_complex_ascii ***\n"); EXIT(1);
   }

   /** read lines, convert to floats, store **/

   nrow = 0 ;
   while( 1 ){
     ptr = my_fgets( buf , LBUF , fts ) ;  /* read */
     if( ptr==NULL || *ptr=='\0' ) break ; /* failure --> end of data */

     vec = decode_linebuf( buf ) ;
     if( vec == NULL ) break ;
     if( vec->nar == 0 ){ KILL_floatvec(vec); break; }

     if( used_tsar + ncol >= alloc_tsar ){
        alloc_tsar += incts ;
        tsar        = (float *)realloc( (void *)tsar,sizeof(float)*alloc_tsar );
        if( tsar == NULL ){
          fprintf(stderr,"\n*** realloc error in mri_read_complex_ascii ***\n"); EXIT(1);
        }
     }
     for( ii=0 ; ii < vec->nar && ii < ncol ; ii++ )
       tsar[used_tsar+ii] = vec->ar[ii] ;
     for( ; ii < ncol ; ii++ )
       tsar[used_tsar+ii] = 0.0 ;
     used_tsar += ncol ;
     KILL_floatvec(vec) ;

     nrow++ ;                  /* got one more complete row! */
   }
   fclose( fts ) ; /* finished with this file! */
   (void) my_fgets( NULL , 0 , NULL ) ;  /* reset [20 Jul 2004] */

   /* from <= 1 to < 1 (allow 1x1 image) 25 Jan 2006 [rickr] */
   if( used_tsar < 1 ){ FRB(buf); free(tsar); RETURN(NULL); }

   tsar = (float *) realloc( tsar , sizeof(float) * used_tsar ) ;
   if( tsar == NULL ){
      fprintf(stderr,"\n*** final realloc error in mri_read_complex_ascii ***\n"); EXIT(1);
   }

   /* now turn tsar into a complex vector */
   ctsar = (complex *) calloc(used_tsar, sizeof(complex));
   for( ii=0 ; ii < used_tsar; ii=ii+2) {
      /* fprintf(stderr,"tsar[%d]=%f\n", ii, tsar[ii]);   */
      ih = ii/2;
      ctsar[ih].r = tsar[ii]; ctsar[ih].i = tsar[ii+1];
   }

   outim = mri_new_vol_empty( ncol/2 , nrow , 1 , MRI_complex ) ;
   mri_fix_data_pointer( tsar , outim ) ;
   mri_add_name( fname , outim ) ;

   FRB(buf) ; RETURN(outim) ;
}

/*---------------------------------------------------------------------------*/

static char *dname=NULL ; static size_t ndname=0 ;  /* 15 Nov 2007 */

#define DNAME_FIX(fn)                                                         \
 do{ size_t qq=strlen(fn)+7 ;                                                 \
     if( ndname < qq ){ dname=(char *)realloc((void *)dname,qq); ndname=qq; } \
 } while(0)

/*---------------------------------------------------------------------------*/
/*! Read an ASCII file as columns, transpose to rows, allow column selectors.

  \param fname = Input filename (max of 255 characters)
  \return Pointer to MRI_IMAGE if all went well; NULL if not.
  \date 16 Nov 1999

  This function builds on mri_read_ascii() in two ways:
    - the input is transposed to rows (so that a 1x100 file becomes a 100x1 image)
    - column selectors [..] and row selectors {..} are allowed in fname
    - if fname ends in a ' character, file will be NOT be transposed
*/

MRI_IMAGE * mri_read_1D( char *fname )
{
   MRI_IMAGE *inim , *outim , *flim ;
   char *cpt , *dpt ;
   int ii,jj,nx,ny,nts , *ivlist , *ivl , *sslist ;
   float *far , *oar ;
   int flip ;  /* 05 Sep 2006 */

ENTRY("mri_read_1D") ;

   if( fname == NULL || fname[0] == '\0' ) RETURN(NULL) ;

   /*-- 25 Jan 2008: read from stdin? --*/

   ii = strlen(fname) ;
   if( (ii <= 2 && fname[0] == '-')              ||
       (ii <= 6 && strncmp(fname,"stdin",5) == 0)  ){
     inim = mri_read_1D_stdin() ;
     if( inim != NULL && fname[ii-1] == '\'' ){
       flim = mri_transpose(inim); mri_free(inim); inim = flim;
     }
     RETURN(inim) ;
   }

   /*-- back to reading from an actual file --*/

   DNAME_FIX(fname) ;
   strcpy(dname,fname); ii = strlen(dname);  /* 05 Sep 2006 */
   flip = (dname[ii-1] == '\''); if( flip ) dname[ii-1] = '\0';

   if( strncmp(dname,"1D:",3) == 0 ){       /* 28 Apr 2003 */
     outim = mri_1D_fromstring( dname+3 ) ;
     /** if( outim == NULL ) ERROR_message("read of '1D:' string fails") ; **/
     if( flip ){ inim=mri_transpose(outim); mri_free(outim); outim=inim; }
     RETURN(outim) ;
   }

   /*-- split filename and subvector list --*/

   cpt = strstr(fname,"[") ;
   dpt = strstr(fname,"{") ;            /* 30 Apr 2003: subsampling list */

   if( cpt == fname || dpt == fname ){  /* can't be at start of filename! */
      ERROR_message("Illegal filename in mri_read_1D('%s')\n",fname) ;
      RETURN(NULL) ;
   } else {                             /* got a subvector list */
      if( cpt != NULL ){ ii = cpt-fname; dname[ii] = '\0'; }
      if( dpt != NULL ){ ii = dpt-fname; dname[ii] = '\0'; }
   }

   /*-- read file in, flip it sideways --*/

   inim = mri_read_ascii(dname) ;
   if( inim == NULL ) RETURN(NULL) ;
   flim = mri_transpose(inim) ; mri_free(inim) ;

   /*-- get the subvector and subsampling lists, if any --*/

   nx = flim->nx ; ny = flim->ny ;

   ivlist = MCW_get_intlist( ny , cpt ) ;   /* subvector list */
   sslist = MCW_get_intlist( nx , dpt ) ;   /* subsampling list */

   /* if either columns(vectors) or rows (subsamples) are specified */
   /* make sure the lists are valid. Return null if not  */
   if( ((cpt!= NULL) && (ivlist==NULL)) ||
       ((dpt!= NULL) && (sslist==NULL)) ) 
      RETURN(NULL);

   /* if have subvector list, extract those rows into a new image */

   if( ivlist != NULL && ivlist[0] > 0 ){
     nts = ivlist[0] ;                         /* number of subvectors */
     ivl = ivlist + 1 ;                        /* start of array of subvectors */

     for( ii=0 ; ii < nts ; ii++ ){            /* check them out */
       if( ivl[ii] < 0 || ivl[ii] >= ny ){
         fprintf(stderr,"*** Out-of-range subvector [list] in mri_read_1D: %s\n",fname) ;
         mri_free(flim) ; free(ivlist) ; RETURN(NULL) ;
       }
     }

     outim = mri_new( nx , nts , MRI_float ) ; /* make output image */
     far   = MRI_FLOAT_PTR( flim ) ;
     oar   = MRI_FLOAT_PTR( outim ) ;

     for( ii=0 ; ii < nts ; ii++ )             /* copy desired rows */
       memcpy( oar + ii*nx , far + ivl[ii]*nx , sizeof(float)*nx ) ;

     mri_free(flim); free(ivlist); flim = outim; ny = nts;
   }

   /* if have subsampling list, extract those columns into a new image */

   if( sslist != NULL && sslist[0] > 0 ){
     nts = sslist[0] ;                         /* number of columns to get */
     ivl = sslist + 1 ;                        /* start of array of column indexes */

     for( ii=0 ; ii < nts ; ii++ ){            /* check them out */
       if( ivl[ii] < 0 || ivl[ii] >= nx ){
         fprintf(stderr,"*** Out-of-range subsampling {list} in mri_read_1D: %s\n",fname) ;
         mri_free(flim) ; free(sslist) ; RETURN(NULL) ;
       }
     }

     if( AFNI_yesenv("AFNI_TEST_SUBSETTER") ){
       INFO_message("Calling mri_subset_x2D") ;
       outim = mri_subset_x2D( nts , ivl , flim ) ;
     } else {
       outim = mri_new( nts , ny , MRI_float ) ; /* make output image */
       far   = MRI_FLOAT_PTR( flim ) ;
       oar   = MRI_FLOAT_PTR( outim ) ;

       for( ii=0 ; ii < nts ; ii++ )             /* copy desired columns */
         for( jj=0 ; jj < ny ; jj++ )
           oar[ii+jj*nts] = far[ivl[ii]+jj*nx] ;
     }

     mri_free(flim); free(sslist); flim = outim;
   }

   if( flip ){ inim=mri_transpose(flim); mri_free(flim); flim=inim; }

   mri_add_name(fname,flim) ; RETURN(flim) ;
}

MRI_IMAGE * mri_read_double_1D( char *fname )
{
   MRI_IMAGE *inim , *outim , *flim ;
   char *cpt , *dpt ;
   int ii,jj,nx,ny,nts , *ivlist , *ivl , *sslist ;
   double *dar , *oar ;
   int flip ;  /* 05 Sep 2006 */

ENTRY("mri_read_double_1D") ;

   if( fname == NULL || fname[0] == '\0' ) RETURN(NULL) ;
   DNAME_FIX(fname) ;
   strcpy(dname,fname); ii = strlen(dname);  /* 05 Sep 2006 */
   flip = (dname[ii-1] == '\''); if( flip ) dname[ii-1] = '\0';

   if( strncmp(dname,"1D:",3) == 0 ){       /* 28 Apr 2003 */
     /*
     outim = mri_1D_double_fromstring( dname+3 ) ;
     if( flip ){ inim=mri_transpose(outim); mri_free(outim); outim=inim; }
     RETURN(outim) ;
     */
     ERROR_message("Somebody was too lazy to allow this option here."); RETURN(NULL);
   }

   /*-- split filename and subvector list --*/

   cpt = strstr(fname,"[") ;
   dpt = strstr(fname,"{") ;            /* 30 Apr 2003: subsampling list */

   if( cpt == fname || dpt == fname ){  /* can't be at start of filename! */
      ERROR_message("Illegal filename in mri_read_double_1D('%s')\n",fname) ;
      RETURN(NULL) ;
   } else {                             /* got a subvector list */
      if( cpt != NULL ){ ii = cpt-fname; dname[ii] = '\0'; }
      if( dpt != NULL ){ ii = dpt-fname; dname[ii] = '\0'; }
   }

   /*-- read file in, flip it sideways --*/

   inim = mri_read_double_ascii(dname) ;
   if( inim == NULL ) RETURN(NULL) ;
   flim = mri_transpose(inim) ; mri_free(inim) ;
   if( flim == NULL ) {
      fprintf(stderr, "Failed to transpose image\n");
      RETURN(NULL) ;
   }
   /*-- get the subvector and subsampling lists, if any --*/

   nx = flim->nx ; ny = flim->ny ;

   ivlist = MCW_get_intlist( ny , cpt ) ;   /* subvector list */
   sslist = MCW_get_intlist( nx , dpt ) ;   /* subsampling list */

   /* if have subvector list, extract those rows into a new image */

   if( ivlist != NULL && ivlist[0] > 0 ){
     nts = ivlist[0] ;                         /* number of subvectors */
     ivl = ivlist + 1 ;                        /* start of array of subvectors */

     for( ii=0 ; ii < nts ; ii++ ){            /* check them out */
       if( ivl[ii] < 0 || ivl[ii] >= ny ){
         fprintf(stderr,"*** Out-of-range subvector [list] in mri_read_double_1D: %s\n",fname) ;
         mri_free(flim) ; free(ivlist) ; RETURN(NULL) ;
       }
     }

     outim = mri_new( nx , nts , MRI_double ) ; /* make output image */
     dar   = MRI_DOUBLE_PTR( flim ) ;
     oar   = MRI_DOUBLE_PTR( outim ) ;

     for( ii=0 ; ii < nts ; ii++ )             /* copy desired rows */
       memcpy( oar + ii*nx , dar + ivl[ii]*nx , sizeof(double)*nx ) ;

     mri_free(flim); free(ivlist); flim = outim; ny = nts;
   }

   /* if have subsampling list, extract those columns into a new image */

   if( sslist != NULL && sslist[0] > 0 ){
     nts = sslist[0] ;                         /* number of columns to get */
     ivl = sslist + 1 ;                        /* start of array of column indexes */

     for( ii=0 ; ii < nts ; ii++ ){            /* check them out */
       if( ivl[ii] < 0 || ivl[ii] >= nx ){
         fprintf(stderr,"*** Out-of-range subsampling {list} in mri_read_double_1D: %s\n",fname) ;
         mri_free(flim) ; free(sslist) ; RETURN(NULL) ;
       }
     }

     outim = mri_new( nts , ny , MRI_double ) ; /* make output image */
     dar   = MRI_DOUBLE_PTR( flim ) ;
     oar   = MRI_DOUBLE_PTR( outim ) ;

     for( ii=0 ; ii < nts ; ii++ )             /* copy desired columns */
       for( jj=0 ; jj < ny ; jj++ )
         oar[ii+jj*nts] = dar[ivl[ii]+jj*nx] ;

     mri_free(flim); free(sslist); flim = outim;
   }

   if( flip ){ inim=mri_transpose(flim); mri_free(flim); flim=inim; }

   mri_add_name(fname,flim) ; RETURN(flim) ;
}

MRI_IMAGE * mri_read_complex_1D( char *fname )
{
   MRI_IMAGE *inim , *outim , *flim ;
   char *cpt , *dpt ;
   int ii,jj,nx,ny,nts , *ivlist , *ivl , *sslist ;
   complex *far , *oar ;
   int flip ;  /* 05 Sep 2006 */

ENTRY("mri_read_complex_1D") ;

   if( fname == NULL || fname[0] == '\0' ) RETURN(NULL) ;
   DNAME_FIX(fname) ;
   strcpy(dname,fname); ii = strlen(dname);  /* 05 Sep 2006 */
   flip = (dname[ii-1] == '\''); if( flip ) dname[ii-1] = '\0';

   if( strncmp(dname,"1D:",3) == 0 ){       /* 28 Apr 2003 */
     /*
     outim = mri_1D_complex_fromstring( dname+3 ) ;
     if( flip ){ inim=mri_transpose(outim); mri_free(outim); outim=inim; }
     RETURN(outim) ;
      */
     ERROR_message("Somebody was too lazy to allow this option here."); RETURN(NULL);
   }

   /*-- split filename and subvector list --*/

   cpt = strstr(fname,"[") ;
   dpt = strstr(fname,"{") ;            /* 30 Apr 2003: subsampling list */

   if( cpt == fname || dpt == fname ){  /* can't be at start of filename! */
      ERROR_message("Illegal filename in mri_read_complex_1D('%s')\n",fname) ;
      RETURN(NULL) ;
   } else {                             /* got a subvector list */
      if( cpt != NULL ){ ii = cpt-fname; dname[ii] = '\0'; }
      if( dpt != NULL ){ ii = dpt-fname; dname[ii] = '\0'; }
   }

   /*-- read file in, flip it sideways --*/

   inim = mri_read_complex_ascii(dname) ;
   if( inim == NULL ) RETURN(NULL) ;
   flim = mri_transpose(inim) ; mri_free(inim) ;

   /*-- get the subvector and subsampling lists, if any --*/

   nx = flim->nx ; ny = flim->ny ;

   ivlist = MCW_get_intlist( ny , cpt ) ;   /* subvector list */
   sslist = MCW_get_intlist( nx , dpt ) ;   /* subsampling list */

   /* if have subvector list, extract those rows into a new image */

   if( ivlist != NULL && ivlist[0] > 0 ){
     nts = ivlist[0] ;                         /* number of subvectors */
     ivl = ivlist + 1 ;                        /* start of array of subvectors */

     for( ii=0 ; ii < nts ; ii++ ){            /* check them out */
       if( ivl[ii] < 0 || ivl[ii] >= ny ){
         fprintf(stderr,"*** Out-of-range subvector [list] in mri_read_complex_1D: %s\n",fname) ;
         mri_free(flim) ; free(ivlist) ; RETURN(NULL) ;
       }
     }

     outim = mri_new( nx , nts , MRI_complex ) ; /* make output image */
     far   = MRI_COMPLEX_PTR( flim ) ;
     oar   = MRI_COMPLEX_PTR( outim ) ;

     for( ii=0 ; ii < nts ; ii++ )             /* copy desired rows */
       memcpy( oar + ii*nx , far + ivl[ii]*nx , sizeof(complex)*nx ) ;

     mri_free(flim); free(ivlist); flim = outim; ny = nts;
   }

   /* if have subsampling list, extract those columns into a new image */

   if( sslist != NULL && sslist[0] > 0 ){
     nts = sslist[0] ;                         /* number of columns to get */
     ivl = sslist + 1 ;                        /* start of array of column indexes */

     for( ii=0 ; ii < nts ; ii++ ){            /* check them out */
       if( ivl[ii] < 0 || ivl[ii] >= nx ){
         fprintf(stderr,"*** Out-of-range subsampling {list} in mri_read_complex_1D: %s\n",fname) ;
         mri_free(flim) ; free(sslist) ; RETURN(NULL) ;
       }
     }

     outim = mri_new( nts , ny , MRI_complex ) ; /* make output image */
     far   = MRI_COMPLEX_PTR( flim ) ;
     oar   = MRI_COMPLEX_PTR( outim ) ;

     for( ii=0 ; ii < nts ; ii++ )             /* copy desired columns */
       for( jj=0 ; jj < ny ; jj++ ) {
         oar[ii+jj*nts].r = far[ivl[ii]+jj*nx].r ;
         oar[ii+jj*nts].i = far[ivl[ii]+jj*nx].i ;
      }

     mri_free(flim); free(sslist); flim = outim;
   }

   if( flip ){ inim=mri_transpose(flim); mri_free(flim); flim=inim; }

   mri_add_name(fname,flim) ; RETURN(flim) ;
}

/*-----------------------------------------------------------------------------------*/
/* Read a 1D file from stdin; adapted from 1dplot.c */

MRI_IMAGE * mri_read_1D_stdin(void)
{
#define SIN_NLBUF 131072
#define SIN_NVMAX 10000
   char *lbuf , *cpt , *dpt ;
   int   nval , ii,nx,ny ;
   float *val , fff , *far ;
   MRI_IMAGE *flim , *inim ;

ENTRY("mri_read_1D_stdin") ;

   lbuf = (char * )malloc(sizeof(char )*SIN_NLBUF) ;
   val  = (float *)malloc(sizeof(float)*SIN_NVMAX) ;

   do{               /* read lines until 1st char is non-blank and non-# */
     cpt = fgets(lbuf,SIN_NLBUF,stdin) ;
     if( cpt==NULL ){ free(val);free(lbuf); RETURN(NULL); }
     for( ii=0 ; cpt[ii] != '\0' && isspace(cpt[ii]) ; ii++ ) ; /* nada */
   } while( cpt[ii] == '\0' || cpt[ii] == '#' ) ;

   nval = 0 ; cpt = lbuf ;   /* read numbers from lbuf into val */
   while(1){
     fff = strtod(cpt,&dpt) ; if( dpt  == cpt       ) break ;
     val[nval++] = fff ;      if( nval == SIN_NVMAX ) break ;
     cpt = dpt; if( *cpt == ','  ) cpt++; if( *cpt == '\0' ) break;
   }
   if( nval < 1 ){ free(val);free(lbuf); RETURN(NULL); }

   nx = nval ; ny = 1 ;
   far = (float *) malloc(sizeof(float)*nx) ;
   memcpy(far,val,sizeof(float)*nx) ;

   while(1){  /* read from stdin */
     cpt = fgets(lbuf,SIN_NLBUF,stdin) ;
     if( cpt == NULL ) break ;            /* done */
     for( ii=0 ; cpt[ii] != '\0' && isspace(cpt[ii]) ; ii++ ) ; /* nada */
     if( cpt[ii] == '\0' || cpt[ii] == '#' ) continue ;         /* skip */

     memset(val,0,sizeof(float)*nx) ;  /* set input buffer to zero */
     nval = 0 ; cpt = lbuf ;   /* read numbers from lbuf into val */
     while(1){
       fff = strtod(cpt,&dpt) ; if( dpt  == cpt ) break ;
       val[nval++] = fff ;      if( nval == nx  ) break ;
       cpt = dpt; if( *cpt == ','  ) cpt++; if( *cpt == '\0' ) break;
     }
     far = (float *) realloc( far , sizeof(float)*(ny+1)*nx ) ;
     memcpy(far+ny*nx,val,sizeof(float)*nx) ; ny++ ;
   }

   flim = mri_new_vol_empty( nx,ny,1 , MRI_float ) ;
   mri_fix_data_pointer( far , flim ) ;
   if( ny > 1 ){      /* more than one row ==> transpose (the usual case) */
     inim = mri_transpose(flim) ; mri_free(flim) ;
   } else {           /* only 1 row ==> am OK this way */
     inim = flim ;
   }
   free((void *)val); free((void *)lbuf); RETURN(inim);
}

/*-----------------------------------------------------------------------------------*/
/* Read ragged rows, with '*' being set to the filler value [28 Jul 2004] */

MRI_IMAGE * mri_read_ascii_ragged( char *fname , float filler )
{
   MRI_IMAGE *outim ;
   int ii,jj , ncol,nrow ;
   float *tsar ;
   FILE *fts ;
   char *ptr ;
   static char *buf=NULL ;
   floatvec *fvec ;

ENTRY("mri_read_ascii_ragged") ;

   if( fname == NULL || *fname == '\0' ){ FRB(buf); RETURN(NULL); }

   if( strncmp(fname,"1D:",3) == 0 ){  /* 05 Jan 2007 */
     outim = mri_read_ragged_fromstring( fname+3 , filler ) ;
     FRB(buf); RETURN(outim) ;
   }

   fts = fopen( fname , "r" ); if( fts == NULL ){ FRB(buf); RETURN(NULL); }

   if( buf == NULL ) buf = AFMALL(char, LBUF) ;

   /** step 1: read in ALL lines, see how many numbers are in each,
               in order to get the maximum row length and # of rows **/

   lbfill = filler ; /* 10 Aug 2004 */

   (void) my_fgets( NULL , 0 , NULL ) ;  /* reset */
   ncol = nrow = 0 ;
   while(1){
     ptr = my_fgets( buf , LBUF , fts ) ;
     if( ptr==NULL || *ptr=='\0' ) break ;
     fvec = decode_linebuf( buf ) ;
     if( fvec != NULL && fvec->nar > 0 ){ nrow++; ncol = MAX(ncol,fvec->nar); }
     if( fvec != NULL ) KILL_floatvec(fvec) ; else break ;
   }
   if( nrow == 0 || ncol == 0 ){ fclose(fts); FRB(buf); lbfill=0.0f; RETURN(NULL); }

   /** At this point, ncol is the number of floats to be read from each line **/

   rewind( fts ) ;  /* will start over */

   outim = mri_new( ncol , nrow , MRI_float ) ;
   tsar  = MRI_FLOAT_PTR(outim) ;

   /** read lines, convert to floats, store **/

   nrow = 0 ;
   while( 1 ){
     ptr = my_fgets( buf , LBUF , fts ) ;  /* read */
     if( ptr==NULL || *ptr=='\0' ) break ; /* failure --> end of data */

     fvec = decode_linebuf( buf ) ;
     if( fvec == NULL ) break ;
     if( fvec->nar == 0 ){ KILL_floatvec(fvec); break; }

     for( ii=0 ; ii < fvec->nar && ii < ncol ; ii++ )
       tsar[nrow*ncol+ii] = fvec->ar[ii] ;
     for( ; ii < ncol ; ii++ )
       tsar[nrow*ncol+ii] = filler ;   /* fill for incomplete lines */
     KILL_floatvec(fvec) ;
     nrow++ ;                  /* got one more complete row! */
   }
   fclose( fts ) ; /* finished with this file! */
   (void) my_fgets( NULL , 0 , NULL ) ;  /* reset */

   mri_add_name( fname , outim ) ;
   FRB(buf) ; lbfill = 0.0f ; RETURN(outim) ;
}

/*---------------------------------------------------------------------------*/
/*! Decode pairs of numbers separated by a single non-space character */

static INLINE complex decode_complex( char *str , float filler )
{
   complex pp ; char ss ; float aa , bb ;

   pp.r = pp.i = filler ;
   if( str == NULL ) return pp ;
   aa = bb = filler ;
   sscanf( str , "%f%c%f" , &aa , &ss , &bb ) ;
   pp.r = aa ; pp.i = bb ; return pp ;
}

/*---------------------------------------------------------------------------*/
/*! Ragged read pairs of values into a complex image. [08 Mar 2007] */

MRI_IMAGE * mri_read_ascii_ragged_complex( char *fname , float filler )
{
   MRI_IMAGE *outim ;
   complex   *cxar , cval ;
   int ii,jj , ncol,nrow ;
   FILE *fts ;
   char *buf , *ptr ;
   NI_str_array *sar ; int nsar ;

ENTRY("mri_read_ascii_ragged_complex") ;

   if( fname == NULL || *fname == '\0' ) RETURN(NULL) ;

   fts = fopen(fname,"r"); if( fts == NULL ) RETURN(NULL) ;

   buf = (char *)malloc(LBUF) ;

   /** step 1: read in ALL lines, see how many numbers are in each,
               in order to get the maximum row length and # of rows **/

   (void) my_fgets( NULL , 0 , NULL ) ;  /* reset */
   ncol = nrow = 0 ;
   while(1){
     ptr = my_fgets( buf , LBUF , fts ) ;       /* read line */
     if( ptr==NULL || *ptr=='\0' ) break ;      /* fails? end of data */
     sar = NI_decode_string_list( buf , "~" ) ; /* break into pieces */
     if( sar != NULL ){
       nsar = sar->num ;                        /* number of pieces */
       if( nsar > 0 ){ nrow++; ncol = MAX(ncol,nsar); }
       NI_delete_str_array(sar) ;               /* recycle this */
     }
   }
   if( nrow == 0 || ncol == 0 ){ fclose(fts); free(buf); RETURN(NULL); }

   /** At this point, ncol is the number of pairs to be read from each line **/

   rewind(fts) ;  /* start over at top of file */

   outim = mri_new( ncol , nrow , MRI_complex ) ;
   cxar  = MRI_COMPLEX_PTR(outim) ;

   /** read lines, convert to floats, store **/

   nrow = 0 ; cval.r = cval.i = filler ;
   while( 1 ){
     ptr = my_fgets( buf , LBUF , fts ) ;       /* read line */
     if( ptr==NULL || *ptr=='\0' ) break ;      /* failure --> end of data */
     sar = NI_decode_string_list( buf , "~" ) ; /* break up */
     if( sar != NULL ){
       nsar = sar->num ;                        /* number of pieces */
       for( ii=0 ; ii < nsar ; ii++ )           /* decode each piece */
         cxar[nrow*ncol+ii] = decode_complex( sar->str[ii] , filler ) ;
       for( ; ii < ncol ; ii++ )
         cxar[nrow*ncol+ii] = cval ;            /* fill row with junk */
       NI_delete_str_array(sar) ;               /* done with this */
     }
     nrow++ ;                                   /* added one complete row */
   }

   free(buf); fclose( fts ); (void) my_fgets(NULL,0,NULL);  /* cleanup */

   mri_add_name(fname,outim) ; RETURN(outim) ;
}

/*---------------------------------------------------------------------------*/
/*! Decode vectors of numbers separated by a single non-space character;
    return value is number of values actually decoded.
    vec==NULL is OK for testing (then no values are assigned to it).
*//*-------------------------------------------------------------------------*/

static int decode_fvect( char *str, float filler, int vdim, float *vec )
{
   int ii,nn,mm ; float aa ;

   if( vec != NULL ) for( ii=0 ; ii < vdim ; ii++ ) vec[ii] = filler ;
   if( str == NULL || *str == '\0' ) return 0 ;

   if( *str == '*' ) return 1 ;  /* 23 Dec 2008 */

   for( ii=0 ; ii < vdim ; ii++ ){
     nn = 0 ; mm = sscanf( str , "%f%n" , &aa , &nn ) ;
     if( mm == 0 ) return (ii) ;
     if( vec != NULL ) vec[ii] = aa ;
     str += nn ; if( *str == '\0' ) return (ii+1) ;
     str++ ;     if( *str == '\0' ) return (ii+1) ;
   }
   return vdim ;
}

/*---------------------------------------------------------------------------*/
/*! Ragged read tuples of values into a fvect image.
    vdim = length of vectors to be read;
           can be zero, in which case will be determined from the data.
*//*-------------------------------------------------------------------------*/

MRI_IMAGE * mri_read_ascii_ragged_fvect( char *fname, float filler, int vdim )
{
   MRI_IMAGE *outim ; float *var ;
   int ii,jj , ncol,nrow ;
   FILE *fts ;
   char *buf , *ptr ;
   NI_str_array *sar ; int nsar , nvdim ;

ENTRY("mri_read_ascii_ragged_fvect") ;

   if( fname == NULL || *fname == '\0' ) RETURN(NULL) ;

   if( strncmp(fname,"1D:",3) == 0 ){  /* cheap hack for 3dDeconvolve -stim_times */
     outim = mri_read_ragged_fromstring( fname+3 , filler ) ;
     if( outim != NULL && outim->kind == MRI_float){
       outim->kind = MRI_fvect ; outim->vdim = 1 ;
     }
     RETURN(outim) ;
   }

   fts = fopen(fname,"r"); if( fts == NULL ) RETURN(NULL) ;

   buf = (char *)malloc(LBUF) ;

   /** step 1: read in ALL lines, see how many numbers are in each,
               in order to get the maximum row length and # of rows **/

   (void) my_fgets( NULL , 0 , NULL ) ;  /* reset */
   ncol = nrow = 0 ; nvdim = 0 ;
   while(1){
     ptr = my_fgets( buf , LBUF , fts ) ;       /* read line */
     if( ptr==NULL || *ptr=='\0' ) break ;      /* fails? end of data */
     sar = NI_decode_string_list( buf , "~" ) ; /* break into pieces */
     if( sar != NULL ){
       nsar = sar->num ;                        /* number of pieces */
       if( nsar > 0 ){ nrow++; ncol = MAX(ncol,nsar); }
       if( nsar > 0 && vdim == 0 ){             /* number of components */
         for( jj=0 ; jj < nsar ; jj++ ){
           ii = decode_fvect( sar->str[jj] , filler , 9999 , NULL ) ;
           nvdim = MAX(nvdim,ii) ;
         }
       }
       NI_delete_str_array(sar) ;               /* recycle this */
     }
   }
   if( vdim == 0 ) vdim = nvdim ;               /* set vdim from data */
   if( nrow == 0 || ncol == 0 || vdim == 0 ){
     fclose(fts); free(buf); RETURN(NULL);
   }

   /** At this point, ncol is the number of vectors to be read from each line **/

   rewind(fts) ;  /* start over at top of file */

   outim = mri_new_fvectim( ncol , nrow , 1 , vdim ) ;
   var   = (float *)outim->im ;
   for( ii=0 ; ii < ncol*nrow*vdim ; ii++ ) var[ii] = filler ;

   /** read lines, convert to floats, store **/

   nrow = 0 ;
   while( 1 ){
     ptr = my_fgets( buf , LBUF , fts ) ;       /* read line */
     if( ptr==NULL || *ptr=='\0' ) break ;      /* failure --> end of data */
     sar = NI_decode_string_list( buf , "~" ) ; /* break up */
     if( sar != NULL ){
       nsar = sar->num ;                        /* number of pieces */
       for( ii=0 ; ii < nsar ; ii++ )           /* decode each piece */
         (void)decode_fvect( sar->str[ii] , filler , vdim ,
                             var + (nrow*ncol+ii)*vdim     ) ;
       NI_delete_str_array(sar) ;               /* done with this */
     }
     nrow++ ;                                   /* added one complete row */
   }

   free(buf); fclose( fts ); (void) my_fgets(NULL,0,NULL);  /* cleanup */

   mri_add_name(fname,outim) ; RETURN(outim) ;
}

/*---------------------------------------------------------------------------
  Read in an ASCII file to a float array.
-----------------------------------------------------------------------------*/

static void read_ascii_floats( char * fname, int * nff , float ** ff )
{
   int ii,jj,val , used_tsar , alloc_tsar ;
   float *tsar ;
   float ftemp ;
   FILE *fts ;
   char *buf ;  /* 08 Jul 2004: malloc this now, instead of auto */
   char *ptr ;
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
   buf = (char *)malloc(LBUF) ;
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
   free( buf ) ;

   if( used_tsar <= 1 ){ free(tsar); *nff=0; *ff=NULL; return; }

   tsar = (float *) realloc( tsar , sizeof(float) * used_tsar ) ;
   if( tsar == NULL ){
      fprintf(stderr,"\n*** final realloc fails: read_ascii_floats ***\n"); EXIT(1);
   }

   *nff = used_tsar; *ff  = tsar; return;
}

/*--------------------------------------------------------------
   Read a pile of images from one ASCII file.
   Adapted from mri_read_3D - Feb 2000 - RWCox.
   [N.B.: if this routine is altered, don't forget mri_imcount!]
----------------------------------------------------------------*/

MRI_IMARR * mri_read_3A( char *tname )
{
   int nx , ny , nz , ii , nxyz,nxy , nff ;
   int ngood , length , kim , datum_type ;
   char fname[256]="\0" , buf[512] ;
   MRI_IMARR *newar ;
   MRI_IMAGE *newim=NULL , * flim ;
   float * ff ;

ENTRY("mri_read_3A") ;

   /*** get info from 3A tname ***/

   if( tname == NULL || strlen(tname) < 10 ) RETURN(NULL) ;

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

   if( ngood < 4 || nx <= 0 || ny <= 0 || nz <= 0 || strlen(fname) <= 0 ) RETURN(NULL) ;

   /* read the input file */

   read_ascii_floats( fname , &nff , &ff ) ;

   if( nff <= 0 || ff == NULL ) RETURN(NULL) ;

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

   free(ff) ; RETURN(newar) ;
}

/*---------------------------------------------------------------------------
   Stuff to read an ANALYZE 7.5 .img file, given the .hdr filename
   -- 05 Feb 2001 - RWCox
   -- 27 Nov 2001 - modified to use funused1 as a scale factor
-----------------------------------------------------------------------------*/

#include "mayo_analyze.h"

/*---------------------------------------------------------------*/
/*! Byte swap ANALYZE file header in various places */

static void swap_analyze_hdr( struct dsr *pntr )
{
ENTRY("swap_analyze_hdr") ;
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
   EXRETURN ;
}

/*---------------------------------------------------------------*/
/*! Count how many 2D slices are in an ANALYZE file.

   \param hname = the "hdr" file of the hdr/img file pair.
*/

static int mri_imcount_analyze75( char * hname )
{
   FILE * fp ;
   struct dsr hdr ;    /* ANALYZE .hdr format */
   int doswap , nz ;

ENTRY("mri_imcount_analyze75") ;

   fp = fopen( hname , "rb" ) ;
   if( fp == NULL ) RETURN(0) ;
   hdr.dime.dim[0] = 0 ;
   fread( &hdr , 1 , sizeof(struct dsr) , fp ) ;
   fclose(fp) ;
   if( hdr.dime.dim[0] == 0 ) RETURN(0) ;
   doswap = (hdr.dime.dim[0] < 0 || hdr.dime.dim[0] > 15) ;
   if( doswap ) swap_analyze_hdr( &hdr ) ;

   switch( hdr.dime.dim[0] ){
      case 2:  nz = 1                                 ; break ;
      case 3:  nz = hdr.dime.dim[3]                   ; break ;

      default:
      case 4:  nz = hdr.dime.dim[3] * hdr.dime.dim[4] ; break ;
   }
   if( nz < 1 ) nz = 1 ;

   RETURN(nz) ;
}

/*---------------------------------------------------------------*/
/*! Read an ANALYZE file into an ARRAY of 2D images.

   \param hname = the "hdr" file for the hdr/img pair
*/

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

ENTRY("mri_read_analyze75") ;

   /* check & prepare filenames */

   if( hname == NULL ) RETURN(NULL) ;
   jj = strlen(hname) ;
   if( jj < 5 ) RETURN(NULL) ;
   if( strcmp(hname+jj-3,"hdr") != 0 ) RETURN(NULL) ;
   strcpy(iname,hname) ; strcpy(iname+jj-3,"img") ;

   /* read header file into struct */

   fp = fopen( hname , "rb" ) ;
   if( fp == NULL ) RETURN(NULL) ;
   hdr.dime.dim[0] = 0 ;
   fread( &hdr , 1 , sizeof(struct dsr) , fp ) ;
   fclose(fp) ;
   if( hdr.dime.dim[0] == 0 ) RETURN(NULL) ;

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

   floatize = (fac != 0.0 || AFNI_yesenv("AFNI_ANALYZE_FLOATIZE")) ; /* 28 Nov 2001 */

   /* get data type into mrilib MRI_* form */

   switch( hdr.dime.datatype ){
      default:
         fprintf(stderr,"*** %s: Unknown ANALYZE datatype=%d (%s)\n",
                 hname,hdr.dime.datatype,ANDT_string(hdr.dime.datatype) ) ;
      RETURN(NULL) ;

      case ANDT_UNSIGNED_CHAR: datum_type = MRI_byte   ;               break;
      case ANDT_SIGNED_SHORT:  datum_type = MRI_short  ;               break;
      case ANDT_SIGNED_INT:    datum_type = MRI_int    ;               break;
      case ANDT_FLOAT:         datum_type = MRI_float  ; floatize = 0; break;
      case ANDT_COMPLEX:       datum_type = MRI_complex; floatize = 0; break;
      case ANDT_RGB:           datum_type = MRI_rgb    ; floatize = 0; break;
      case ANDT_DOUBLE:        datum_type = MRI_double ; floatize = 0; break;
   }

   datum_len = mri_datum_size(datum_type) ;

   /* compute dimensions of images, and number of images */

   nx = hdr.dime.dim[1] ;
   ny = hdr.dime.dim[2] ;
   if( nx < 2 || ny < 2 ) RETURN(NULL) ;

   switch( hdr.dime.dim[0] ){
      case 2:  nz = 1                                 ; break ;
      case 3:  nz = hdr.dime.dim[3]                   ; break ;

      default:
      case 4:  nz = hdr.dime.dim[3] * hdr.dime.dim[4] ; break ;
   }
   if( nz < 1 ) nz = 1 ;

   dx = hdr.dime.pixdim[1] ;
   dy = hdr.dime.pixdim[2] ;
   dz = hdr.dime.pixdim[3] ;

   /** fprintf(stderr,"mri_read_analyze75: nx=%d ny=%d nz=%d\n",nx,ny,nz) ; **/
   /** fprintf(stderr,"mri_read_analyze75: dx=%g dy=%g dz=%g\n",dx,dy,dz) ; **/

   /* open .img file and read images from it */

   length = THD_filesize(iname) ;
   if( length <= 0 ){
      fprintf(stderr,"*** Can't find ANALYZE file %s\n",iname) ;
      RETURN(NULL) ;
   }

   fp = fopen( iname , "rb" ) ;
   if( fp == NULL ){
      fprintf(stderr,"*** Can't open ANALYZE file %s\n",iname) ;
      RETURN(NULL) ;
   }

   ngood = datum_len*nx*ny*nz ;
   if( length < ngood ){
      fprintf( stderr,
        "*** ANALYZE file %s is %d bytes long but must be at least %d bytes long\n"
        "*** for nx=%d ny=%d nz=%d and voxel=%d bytes\n",
        iname,length,ngood,nx,ny,nz,datum_len ) ;
      fclose(fp) ; RETURN(NULL) ;
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
        newim->was_swapped = 1 ;  /* 07 Mar 2002 */
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

   fclose(fp) ; RETURN(newar) ;
}

/*-----------------------------------------------------------------*/
/*! Read an ANALYZE file into an ARRAY of 3D images [26 Aug 2002].

   \param hname = the "hdr" file for the hdr/img pair
*/

MRI_IMARR * mri_read3D_analyze75( char * hname )
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

   int   nt , nxyz ;  /* 26 Aug 2002 */
   float dt ;

ENTRY("mri_read3D_analyze75") ;

   /* check & prepare filenames */

   if( hname == NULL ) RETURN(NULL) ;
   jj = strlen(hname) ;
   if( jj < 5 ) RETURN(NULL) ;
   if( strcmp(hname+jj-3,"hdr") != 0 ) RETURN(NULL) ;
   strcpy(iname,hname) ; strcpy(iname+jj-3,"img") ;

   /* read header file into struct */

   fp = fopen( hname , "rb" ) ;
   if( fp == NULL ) RETURN(NULL) ;
   hdr.dime.dim[0] = 0 ;
   fread( &hdr , 1 , sizeof(struct dsr) , fp ) ;
   fclose(fp) ;
   if( hdr.dime.dim[0] == 0 ) RETURN(NULL) ;

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

   floatize = (fac != 0.0 || AFNI_yesenv("AFNI_ANALYZE_FLOATIZE")) ; /* 28 Nov 2001 */

   /* get data type into mrilib MRI_* form */

   switch( hdr.dime.datatype ){
      default:
         fprintf(stderr,"*** %s: Unknown ANALYZE datatype=%d (%s)\n",
                 hname,hdr.dime.datatype,ANDT_string(hdr.dime.datatype) ) ;
      RETURN(NULL) ;

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
   if( nx < 2 || ny < 2 ) RETURN(NULL) ;

   switch( hdr.dime.dim[0] ){
      case 2:  nz = 1 ; nt = 1 ;                           ; break ;
      case 3:  nz = hdr.dime.dim[3] ; nt = 1 ;             ; break ;

      default:
      case 4:  nz = hdr.dime.dim[3] ; nt = hdr.dime.dim[4] ; break ;
   }
   if( nz < 1 ) nz = 1 ;
   if( nt < 1 ) nt = 1 ;

   dx = hdr.dime.pixdim[1] ;
   dy = hdr.dime.pixdim[2] ;
   dz = hdr.dime.pixdim[3] ;
   dt = hdr.dime.pixdim[4] ; if( dt <= 0.0 ) dt = 1.0 ;

   /* open .img file and read images from it */

   length = THD_filesize(iname) ;
   if( length <= 0 ){
      fprintf(stderr,"*** Can't find ANALYZE file %s\n",iname) ;
      RETURN(NULL) ;
   }

   fp = fopen( iname , "rb" ) ;
   if( fp == NULL ){
      fprintf(stderr,"*** Can't open ANALYZE file %s\n",iname) ;
      RETURN(NULL) ;
   }

   ngood = datum_len*nx*ny*nz*nt ;
   if( length < ngood ){
      fprintf( stderr,
        "*** ANALYZE file %s is %d bytes long but must be at least %d bytes long\n"
        "*** for nx=%d ny=%d nz=%d nt=%d and voxel=%d bytes\n",
        iname,length,ngood,nx,ny,nz,nt,datum_len ) ;
      fclose(fp) ; RETURN(NULL) ;
   }

   /*** read images from the file ***/

   INIT_IMARR(newar) ;

   nxyz = nx*ny*nz ;
   for( kim=0 ; kim < nt ; kim++ ){
      koff = hglobal + (kim+1)*himage + datum_len*nxyz*kim ;
      fseek( fp , koff , SEEK_SET ) ;

      newim  = mri_new_vol( nx,ny,nz , datum_type ) ;
      imar   = mri_data_pointer( newim ) ;
      length = fread( imar , datum_len , nxyz , fp ) ;

      if( doswap ){
        switch( datum_len ){
          default: break ;
          case 2:  swap_twobytes (   nxyz , imar ) ; break ;  /* short */
          case 4:  swap_fourbytes(   nxyz , imar ) ; break ;  /* int, float */
          case 8:  swap_fourbytes( 2*nxyz , imar ) ; break ;  /* complex */
        }
        newim->was_swapped = 1 ;  /* 07 Mar 2002 */
      }

      /* 28 Nov 2001: convert to floats? */

      if( floatize ){
         MRI_IMAGE *qim = mri_to_float(newim) ;
         mri_free(newim) ; newim = qim ;
      }

      if( nt == 1 ) mri_add_name( iname , newim ) ;
      else {
         sprintf( buf , "%s#%d" , iname,kim ) ;
         mri_add_name( buf , newim ) ;
      }

      newim->dx = dx ; newim->dy = dy ; newim->dz = dz ; newim->dt = dt ; newim->dw = 1.0 ;
      ADDTO_IMARR(newar,newim) ;

      /* 27 Nov 2001: scale image? */

      if( fac != 0.0 ) mri_scale_inplace( fac , newim ) ;
   }

   fclose(fp) ; RETURN(newar) ;
}

/*---------------------------------------------------------------------------
  12 Mar 2001 - stuff to read a Siemens Vision .ima file
-----------------------------------------------------------------------------*/

#include "siemens_vision.h"

/*! Count the number of 2D images in a Siemens Vision .ima file.

   Unfortunately, this requires reading the image data and checking
   for all-zero images.  This is because Siemens stores their data
   in a fixed size file, and so just fills out the empty space with
   blank images if need be.
*/

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
/*! Read an array of 2D images from Siemens Vision .ima file.

   The images are stored in a 2D array, which requires untangling the
   data rows to put them into separate MRI_IMAGE structs.
*/

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

ENTRY("mri_read_siemens") ;

   /*--- check file size ---*/

   if( hname == NULL ) RETURN(NULL) ;

   i = stat( hname , &file_stat ) ;
   if( i < 0 ) RETURN(NULL) ;

   /*--- read header data ---*/

   fp = fopen( hname , "rb" ) ;
   if( fp == NULL ) RETURN(NULL) ;
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
     fclose(fp) ; RETURN(NULL) ; /* didn't recognize file format */
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

   if( slices == 0 ){ free(imar) ; RETURN(NULL) ; }  /* bad news */

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
   MRILIB_zoff = fabs(strtod(head.TextSlicePosition,NULL)) ; use_MRILIB_zoff = 1 ;

   /*-- create output --*/

   INIT_IMARR(newar) ;

   for( yy=0 ; yy < matrix ; yy++ ){      /* rows in array of sub-images */
      for( xx=0 ; xx < matrix ; xx++ ){   /* cols in array of sub-images */

         newim = mri_new( imagesize , imagesize , MRI_short ) ;
         nar   = MRI_SHORT_PTR( newim ) ;

         if( swap ) newim->was_swapped = 1 ; /* 07 Mar 2002 */

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

   free(imar) ; RETURN(newar) ;
}

/*---------------------------------------------------------------------------*/
/*! Check for dicom magic number (string) in file
    Bytes 128-131 should be "DICM" in a Dicom Part 10 file
*/

int check_dicom_magic_num( char *fname )
{
  FILE *fp;
  char test_string[5] ;

  fp = fopen( fname, "rb" ) ;
  if(fp == NULL ) return 0 ;
  fseek( fp, 128 , SEEK_SET ) ;
  fread( test_string , 1 , 4 , fp ) ; test_string[4] = '\0' ;
  fclose( fp ) ;
  if( strcmp(test_string,"DICM") == 0 ) {
    return 1 ;
  } else {
    return 0 ;
  }
}

/*---------------------------------------------------------------------------
   Stuff to read a file in "delay" mode -- 01 Jan 1997.
-----------------------------------------------------------------------------*/

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

   if( (im->fondisk & BSWAP_DELAY) ){
      mri_swapbytes( im ) ;
      im->was_swapped = 1 ;  /* 07 Mar 2002 */
   }

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
   MRI_IMARR *newar=NULL ;
   MRI_IMAGE *newim ;
   char *new_fname ;
   int tried_dicom=0 ;

   new_fname = imsized_fname( fname ) ;
   if( new_fname == NULL ) return NULL ;

   if( strlen(new_fname) > 9 && new_fname[0] == '3' && new_fname[1] == 'D' &&
       (new_fname[2] == ':' || new_fname[3] == ':') ){
                               /* check for ':', too   3 Jan 2005 [rickr] */

      newar = mri_read_3D_delay( new_fname ) ;   /* read from a 3D file, later */

   } else if( strlen(new_fname) > 9 &&
              new_fname[0] == '3' && new_fname[1] == 'A' && new_fname[3] == ':' ){

      newar = mri_read_3A( new_fname ) ;

   } else if( check_dicom_magic_num( new_fname ) ) {

     newar = mri_read_dicom( new_fname );  tried_dicom=1 ;

   } else if( strstr(new_fname,".hdr") != NULL ||
              strstr(new_fname,".HDR") != NULL   ){ /* 05 Feb 2001 - ANALYZE header */

      newar = mri_read_analyze75( new_fname ) ;

   } else if( strstr(new_fname,".ima") != NULL ||
              strstr(new_fname,".IMA") != NULL   ){ /* 12 Mar 2001 - Siemens */

      newar = mri_read_siemens( new_fname ) ;

   } else if( strstr(new_fname,".mpg" ) != NULL ||  /* 03 Dec 2003 */
              strstr(new_fname,".MPG" ) != NULL ||  /* read MPEGs  */
              strstr(new_fname,".mpeg") != NULL ||
              strstr(new_fname,".MPEG") != NULL   ){

      newar = mri_read_mpeg( new_fname ) ;  /* cf. mri_read_mpeg.c */
   }

   /* failed thus far?  try DICOM, unless user has requested DICOM last */
   /* 05 May 2003 added option to try DICOM last         KRH          */

   if ((newar == NULL) && !AFNI_yesenv("AFNI_TRY_DICOM_LAST")) {
     if( !tried_dicom ){
       newar = mri_read_dicom( new_fname ) ; tried_dicom = 1 ;
     }
   }

   /* failed again?  try mri_read() for 1 image */

   if( newar == NULL ){
      newim = mri_read( new_fname ) ;      /* read from a 2D file */
      if( newim == NULL ){ free(new_fname) ; return NULL ; }
      INIT_IMARR(newar) ;
      ADDTO_IMARR(newar,newim) ;
   }

   if ( (newar == NULL) && AFNI_yesenv("AFNI_TRY_DICOM_LAST")) {
     if( !tried_dicom ){
       newar = mri_read_dicom( new_fname ) ; tried_dicom = 1 ;
     }
   }

   free(new_fname) ;
   return newar ;
}

/**** like mri_read_3D, but returns delayed images ****/

MRI_IMARR * mri_read_3D_delay( char * tname )
{
   int hglobal , himage , nx , ny , nz ;
   char fname[256] , buf[512] ;
   int ngood , kim , datum_type , datum_len , swap ;
   MRI_IMARR *newar ;
   MRI_IMAGE *newim ;
   FILE      *imfile ;
   long long length , nneed , hglob=0 ;  /* 22 Mar 2007 */

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

      case 'd':                                            /* 06 Feb 2003 */
         ngood = sscanf( tname , "3Dd:%d:%d:%d:%d:%d:%s" ,
                         &hglobal , &himage , &nx , &ny , &nz , fname ) ;

         swap       = 0 ;
         datum_type = MRI_float ;
         datum_len  = sizeof(double) ;  /* better be 8 */
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

      case 'r':
         ngood = sscanf( tname , "3Dr:%d:%d:%d:%d:%d:%s" ,
                         &hglobal , &himage , &nx , &ny , &nz , fname ) ;

         swap       = 0 ;
         datum_type = MRI_rgb ;
         datum_len  = 3*sizeof(byte) ;  /* better be 3 */
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
      length = THD_filesize(fname) ;     /* 22 Mar 2007 */

   /** 13 Apr 1999: modified to allow actual hglobal < -1
                    as long as hglobal+himage >= 0       **/

      hglob = hglobal ;
      if( hglob == -1 || hglob+himage < 0 ){
        hglob = length - nz*(datum_len*nx*ny+himage) ;
        if( hglob < 0 ) hglob = 0 ;
      }

      nneed = hglob + (datum_len*nx*ny+himage) * (long long)nz ;
      if( length < nneed ){
         fprintf( stderr,
           "file %s is %lld bytes long but must be at least %lld bytes long\n"
           "for hglobal=%lld himage=%d nx=%d ny=%d nz=%d and voxel=%d bytes\n",
           fname,length,nneed,hglob,himage,nx,ny,nz,datum_len ) ;
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
      newim->foffset = hglob + (kim+1)*himage + datum_len*nx*ny*(long long)kim ;

      if( nz == 1 ) mri_add_name( fname , newim ) ;
      else {
        sprintf( buf , "%s#%d" , fname,kim ) ;
        mri_add_name( buf , newim ) ;
      }

      ADDTO_IMARR(newar,newim) ;
   }

   return newar ;
}
