#include <string.h>
#include <stdlib.h>
#include <math.h>
#include <stdio.h>
#include <ctype.h>
#include <sys/stat.h>

#ifndef __u64
#  include <stdint.h>
#  define __u64 uint64_t
#else
#  include <asm/types.h>
#endif

#undef  EXIT
#define EXIT(x) exit(x)

#ifdef  __cplusplus
extern "C" {                    /* care of Greg Balls    7 Aug 2006 [rickr] */
#endif

int  thd_floatscan();
int  fseeko();

extern char MRILIB_orients[] ;          /* 12 Mar 2001 */
extern float MRILIB_zoff ;              /* global variables from mri_read.c */
extern float MRILIB_tr ;                /* 03 Dec 2001 */
extern float MRILIB_xoff ;              /* 07 Dec 2001 */
extern float MRILIB_yoff ;

extern int use_MRILIB_zoff ;            /* 20 Dec 2001 */
extern int use_MRILIB_xoff ;
extern int use_MRILIB_yoff ;

extern int   use_MRILIB_xcos ;         /* 22 Jul 2002 */
extern int   use_MRILIB_ycos ;
extern int   use_MRILIB_zcos ;
extern float MRILIB_xcos[3] ;
extern float MRILIB_ycos[3] ;
extern float MRILIB_zcos[3] ;

extern int   use_MRILIB_slicespacing ;  /* 10 Jan 2004 */
extern float MRILIB_slicespacing ;

#ifdef  __cplusplus
}
#endif

#ifndef PI
#  define PI 3.14159265358979323846
#endif

#ifndef WAY_BIG
/*! A big number (anything over this is infinity). */
#  define WAY_BIG 1.e+10
#endif

#ifndef FLDIF
/*! Are 2 floats significantly different? */
#  define FLDIF(x,y) ( fabs(x-y) > 1.e-4 * (fabs(x)+fabs(y)) )
#endif

#ifndef MAX
#  define MAX(a,b) (((a)<(b)) ? (b) : (a))
#endif

#ifndef MIN
#  define MIN(a,b) (((a)>(b)) ? (b) : (a))
#endif

/**** define types ****/

/*! The MRI_byte data type. */

#ifndef TYPEDEF_byte
#define TYPEDEF_byte
typedef unsigned char byte ;
#endif

/*! RGBA data type; not used anywhere (yet). */

#ifndef TYPEDEF_rgba
#define TYPEDEF_rgba
typedef struct { byte r,g,b,a ; } rgba ;  /* 24 Aug 2001 */
#endif

#define LOAD_rgba(s,rr,gg,bb,aa)   ((s).r=(rr),(s).g=(gg),(s).b=(bb),(s).a=(bb))
#define UNLOAD_rgba(s,rr,gg,bb,aa) ((rr)=(s).r,(gg)=(s).g,(bb)=(s).b,(aa)=(s).a)

/*! Scale a byte [0..255] to a float in [0..1). */

#define BYTE_TO_ZONE(b) (0.00392157*(b))

/*! Scale a float in [0..1] to a byte in [0..255]. */

#define ZONE_TO_BYTE(z) ((byte)(255.49*(z)))

/*! Integer flags for different image types.  Sometimes called the "datum". */

typedef enum MRI_TYPE {
        MRI_byte ,  MRI_short  , MRI_int  ,
        MRI_float , MRI_double , MRI_complex , MRI_rgb , MRI_rgba } MRI_TYPE ;

/*! Force a float into a short. */

#define SHORTIZE(xx) (  ((xx) < -32767.0) ? (short)-32767                    \
                      : ((xx) >  32767.0) ? (short) 32767 : (short)rint(xx) )

/*! I suppose that the next C makes this pleonastic. */

#if defined(_SUNPERF_COMPLEX) || defined(DONT_DECLARE_COMPLEX)
# define TYPEDEF_complex
#endif

#ifndef TYPEDEF_complex
#define TYPEDEF_complex
typedef struct complex { float r , i ; } complex ;
#endif

/*-------*/

# define MRI_DATA void *              /* 21 Dec 2006: the big changeover */

/** Mar 1996: Extended to images up to 7D;
              Not all routines work with images > 2D --
              check top of file for "7D SAFE" comments **/

/*! Stores one image (1D to 7D).
    Why 7D, you ask?  Well, I originally only had 2D images here.
    When extending AFNI from 3D to 3D+time and buckets, I thought
    that I might use 4D images (x,y,z,t) as the basic element.
    Instead, I decided to use an array of 3D images (in a THD_datablock),
    but by then I'd extended this typedef to allow (x,y,z,t,u,v,w) dimensioned
    arrays.  I don't think anyplace ever uses more than 3D images, though.
*/

typedef struct MRI_IMAGE {
          int nx ;            /*!< 1st dimension of image */
          int ny ;            /*!< 2nd dimension of image (1 for 1D image) */
          int nz  ;           /*!< 3rd dimension of image (1 for 2D image) */
          int nt ;            /*!< 4th dimension of image (1 for 3D image) */
          int nu ;            /*!< 5th dimension of image (1 for 4D image) */
          int nv ;            /*!< 6th dimension of image (1 for 5D image) */
          int nw  ;           /*!< 7th dimension of image (1 for 6D image) */
          int nxy ;           /*!< nx*ny */
          int nxyz ;          /*!< nx*ny*nz */
          int nxyzt  ;        /*!< nx*ny*nz*nt */
          int nvox   ;        /*!< number of voxels total */
          int pixel_size ;    /*!< bytes per pixel */

          MRI_TYPE kind ;     /*!< one of the MRI_TYPE codes above */
          MRI_DATA im ;       /*!< pointer to actual pixel data */
          char *name ;        /*!< string attached; may be NULL; might be filename */

          float dx ;          /*!< physical pixel size, if != 0 */
          float dy ;          /*!< physical pixel size, if != 0 */
          float dz ;          /*!< physical pixel size, if != 0 */
          float dt ;          /*!< physical pixel size, if != 0 */
          float du ;          /*!< physical pixel size, if != 0 */
          float dv ;          /*!< physical pixel size, if != 0 */
          float dw ;          /*!< physical pixel size, if != 0 */
          float xo ;          /*!< spatial origin of axis */
          float yo ;          /*!< spatial origin of axis */
          float zo ;          /*!< spatial origin of axis */
          float to ;          /*!< spatial origin of axis */
          float uo ;          /*!< spatial origin of axis */
          float vo ;          /*!< spatial origin of axis */
          float wo ;          /*!< spatial origin of axis */

         char *fname ;   /*!< to read actual image data after delay */
         unsigned int foffset ;   /*!< offset into fname of image data */
         int fondisk ;   /*!< flag to indicate if is on disk (?) */

         int was_swapped ; /* 07 Mar 2002 */
} MRI_IMAGE ;

#  define MRI_COPY_AUX(nn,oo)                                           \
    ( (nn)->dx = (oo)->dx , (nn)->dy = (oo)->dy , (nn)->dz = (oo)->dz , \
      (nn)->dt = (oo)->dt , (nn)->du = (oo)->du , (nn)->dv = (oo)->dv , \
      (nn)->dw = (oo)->dw ,                                             \
      (nn)->xo = (oo)->xo , (nn)->yo = (oo)->yo , (nn)->zo = (oo)->zo , \
      (nn)->to = (oo)->to , (nn)->uo = (oo)->uo , (nn)->vo = (oo)->vo , \
      (nn)->wo = (oo)->wo ,                                             \
      mri_add_name( (oo)->name , (nn) ) )

/*! Check if MRI_IMAGE is 2D (nz=1) */
#define MRI_IS_2D(iq)  ((iq)->ny > 1 && (iq)->nz == 1)

#define MRI_BYTE_PTR(iq)    ((byte *)mri_data_pointer(iq))
#define MRI_SHORT_PTR(iq)   ((short *)mri_data_pointer(iq))
#define MRI_INT_PTR(iq)     ((int *)mri_data_pointer(iq))
#define MRI_FLOAT_PTR(iq)   ((float *)mri_data_pointer(iq))
#define MRI_DOUBLE_PTR(iq)  ((double *)mri_data_pointer(iq))
#define MRI_COMPLEX_PTR(iq) ((complex *)mri_data_pointer(iq))
#define MRI_RGB_PTR(iq)     ((byte *)mri_data_pointer(iq))
#define MRI_RGBA_PTR(iq)    ((rgba *)mri_data_pointer(iq))

/*********** Type: array of MRI_IMAGE pointers ***********/

/*! Array of MRI_IMAGE pointers. */

typedef struct MRI_IMARR {
      int num ;              /*!< Number of actual MRI_IMAGE here */
      int nall ;             /*!< Size of imarr array currently allocated */
      MRI_IMAGE ** imarr ;   /*!< Array of MRI_IMAGE pointers */
} MRI_IMARR ;

/*! Get the nn-th image from the image array "name". */

#define IMAGE_IN_IMARR(name,nn) ((name)->imarr[(nn)])
#define IMARR_SUBIM             IMAGE_IN_IMARR

#define INC_IMARR 32

/*! Initialize an MRI_IMARR struct. */

#define INIT_IMARR(name)                                                           \
   do{ int iq ; (name) = (MRI_IMARR *) malloc(sizeof(MRI_IMARR)) ;                 \
       (name)->num = 0 ; (name)->nall = INC_IMARR ;                                \
       (name)->imarr = (MRI_IMAGE **)malloc(sizeof(MRI_IMAGE *)*INC_IMARR) ;       \
       for( iq=(name)->num ; iq < (name)->nall ; iq++ ) (name)->imarr[iq] = NULL ; \
       break ; } while(0)

/*! Add one MRI_IMAGE to the MRI_IMARR struct. */

#define ADDTO_IMARR(name,imm)                                                           \
   do{ int nn , iq ;                                                                    \
       if( (name)->num == (name)->nall ){                                               \
          nn = (name)->nall = 1.1*(name)->nall + INC_IMARR ;                            \
          (name)->imarr = (MRI_IMAGE **)realloc( (name)->imarr,sizeof(MRI_IMAGE *)*nn );\
          for( iq=(name)->num ; iq < (name)->nall ; iq++ ) (name)->imarr[iq] = NULL ; } \
       nn = (name)->num ; ((name)->num)++ ;                                             \
       (name)->imarr[nn] = (imm) ; break ; } while(0)

/*! Free the MRI_IMARR struct (but not the images within). */

#define FREE_IMARR(name)                                                        \
   do{ if( (name) != NULL ){                                                    \
          free((name)->imarr); free((name)); (name) = NULL; } break; } while(0)

/*! Free the MRI_IMARR struct, including the images within. */

#define DESTROY_IMARR(name)                                                     \
   do{ int nn ;                                                                 \
       if( (name) != NULL ){                                                    \
          for( nn=0 ; nn < (name)->num ; nn++ ) mri_free((name)->imarr[nn]) ;   \
          free((name)->imarr); free((name)); (name) = NULL; } break; } while(0)

#ifndef TRUE
#   define TRUE  (1)
#endif

#ifndef FALSE
#   define FALSE (0)
#endif

#define SQR(x)   ((x)*(x))
#define CSQR(z)  (SQR(z.r)+SQR(z.i))
#define CABS(z)  sqrt(CSQR(z))

#define MRI_FATAL_ERROR \
        {fprintf(stderr,"in file: %s at line %d\n",__FILE__,__LINE__);EXIT(1);}

/**** prototypes ****/

#ifdef  __cplusplus
extern "C" {                    /* care of Greg Balls    7 Aug 2006 [rickr] */
#endif

extern MRI_IMARR * mri_read_analyze75( char * ) ;  /* 05 Feb 2001 */

extern void mri_add_name( char * , MRI_IMAGE * ) ;

extern void * mri_data_pointer( MRI_IMAGE * ) ;
extern void mri_free( MRI_IMAGE * ) ;

# define mri_data_pointer_unvarnished(iq) ((iq)->im)

extern double mri_max( MRI_IMAGE * ) ;
extern double mri_maxabs( MRI_IMAGE * ) ;

extern MRI_IMAGE *mri_new( int , int , MRI_TYPE ) ;
extern MRI_IMAGE *mri_read( char * ) ;
extern MRI_IMAGE *mri_read_ge4( char * ) ;               /* 03 Jun 2003 */
extern int mri_write( char * , MRI_IMAGE * ) ;
extern int mri_write_pnm( char * , MRI_IMAGE * ) ;
extern int mri_write_jpg( char * , MRI_IMAGE * ) ;       /* 15 Apr 2005 */
extern int mri_write_png( char * , MRI_IMAGE * ) ;       /* 11 Dec 2006 */
extern int mri_write_filtered( char * , MRI_IMAGE * ) ;  /* 15 Dec 2006 */
extern int mri_write_7D( char * , MRI_IMAGE * ) ;
extern int mri_datum_size( MRI_TYPE typ ) ;
extern MRI_IMAGE *mri_read_ascii( char * ) ;
extern MRI_IMAGE *mri_read_double_ascii( char * ) ;
extern MRI_IMAGE *mri_read_complex_ascii( char * ) ;
extern MRI_IMAGE *mri_read_ascii_ragged(char *, float) ; /* 28 Jul 2004 */
extern int mri_write_ascii( char * , MRI_IMAGE * ) ;
extern int mri_write_raw( char * , MRI_IMAGE * ) ;       /* 05 Jan 2000 */
extern void mri_write_analyze( char * , MRI_IMAGE * ) ;  /* 29 Nov 2001 */

extern MRI_IMAGE * mri_read_ascii_ragged_complex(char *,float); /* 08 Mar 2007 */


extern MRI_IMAGE * mri_read_ragged_fromstring( char *, float); /* 05 Jan 2007 */

extern MRI_IMAGE * mri_read_1D( char * ) ;               /* 16 Nov 1999 */
extern MRI_IMAGE * mri_read_double_1D( char * ) ;
extern MRI_IMAGE * mri_read_complex_1D( char * ) ;
extern int mri_write_1D( char * , MRI_IMAGE * ) ;        /* 16 Nov 1999 */

extern MRI_IMAGE * mri_1D_fromstring( char * ) ;         /* 28 Apr 2003 */

extern int setup_mri_write_angif( void ) ;               /* 28 Jun 2001 */
extern int mri_write_angif( char *, MRI_IMARR * ) ;
extern MRI_IMAGE * mri_colorsetup( int,int,int,int ) ;   /* 05 Oct 2004 */

MRI_IMAGE *mri_new_7D_generic( int nx, int ny, int nz, int nt,
                               int nu, int nv, int nw,
                               MRI_TYPE kind , int make_space ) ;

/*! Create new MRI_IMAGE of type kk, with same dimensions as iq. */

#define mri_new_conforming(iq,kk)                                   \
   mri_new_7D_generic( (iq)->nx, (iq)->ny, (iq)->nz , (iq)->nt ,    \
                       (iq)->nu, (iq)->nv, (iq)->nw , (kk) , TRUE )

extern MRI_IMARR * mri_read_3D( char * ) ;
extern MRI_IMARR * mri_read_file( char * ) ;
extern int mri_imcount( char * ) ;
extern MRI_IMARR * mri_read_many_files( int nf , char * fn[] ) ;
extern MRI_IMARR * mri_read_resamp_many_files( int nf, char * fn[] , 
                                               int nxnew, int nynew, byte pval);

/** returns array of byte images: red, green, blue **/

MRI_IMAGE *mri_read_just_one( char * fname ) ;
MRI_IMAGE *mri_read_nsize( char * fname ) ;

void init_MCW_sizes(void) ;
char * imsized_fname( char * fname ) ;
char * my_strdup( char * str ) ;

extern unsigned int THD_filesize( char * pathname ) ;

extern void mri_swapbytes( MRI_IMAGE * ) ;

extern void swap_twobytes  ( int n , void * ar ) ;  /* 14 Sep 1998 */
extern void swap_fourbytes ( int n , void * ar ) ;
extern void swap_eightbytes( int n , void * ar ) ;  /* 06 Feb 2003 */

extern MRI_IMAGE *mri_to_float( MRI_IMAGE * ) ;
extern MRI_IMAGE *mri_to_short( double , MRI_IMAGE * ) ;
extern MRI_IMAGE *mri_to_short_scl( double,double , MRI_IMAGE * ) ;
extern MRI_IMAGE *mri_to_short_sclip( double,double , int,int , MRI_IMAGE * ) ;
extern MRI_IMAGE *mri_to_complex( MRI_IMAGE * ) ;

/** 27 Nov 2001: mri_scale.c **/

extern void mri_scale_inplace( float , MRI_IMAGE * ) ;

extern MRI_IMAGE * mri_nsize( MRI_IMAGE * ) ;

/**********************************************************************/

#ifdef  __cplusplus
}
#endif

/*------------------------------------------------------------------*/

#undef INLINE
#ifdef __GNUC__
# define INLINE __inline__
#else
# define INLINE /*nada*/
#endif

#undef RESTRICT
#ifdef __GNUC__
# define RESTRICT __restrict__
#else
# define RESTRICT /*nada*/
#endif

#ifdef  __cplusplus
}
#endif

/*-------------------------------------------------------------------------*/
/*! Return the pointer to the data array in an MRI_IMAGE struct.
---------------------------------------------------------------------------*/

void *mri_data_pointer( MRI_IMAGE *im )
{
   void *data ;

   if( im == NULL ) return NULL ;  /* 27 Jul 2004 */

   data = im->im ;
   return data ;
}

/*-------------------------------------------------------------------------*/
/*! Get rid of an MRI_IMAGE struct and all its contents.
---------------------------------------------------------------------------*/

void mri_free( MRI_IMAGE *im )
{
   void *ptr ;

   if( im == NULL ) return ;
   if( im->fname != NULL ){ free(im->fname) ; im->fname = NULL ; }
   im->fondisk = 0 ;
   if( im->name != NULL ){ free(im->name) ; im->name = NULL ; }
   ptr = mri_data_pointer(im) ;
   if( ptr != NULL ) free(ptr) ;
   free(im) ;
   return ;
}

/*-------------------------------------------------------------------------*/
/*! Return the size (bytes) of one data element of the given type.
---------------------------------------------------------------------------*/

int mri_datum_size( MRI_TYPE typ )
{
   switch( typ ){
     case MRI_byte:    return sizeof(byte) ;
     case MRI_short:   return sizeof(short) ;
     case MRI_int:     return sizeof(int) ;
     case MRI_float:   return sizeof(float) ;
     case MRI_double:  return sizeof(double) ;
     case MRI_complex: return sizeof(complex) ;
     case MRI_rgb:     return 3*sizeof(byte) ;
     case MRI_rgba:    return sizeof(rgba) ;
     default:          return 0 ;
   }
}

MRI_IMAGE *mri_to_short( double scl , MRI_IMAGE *oldim )
{
   MRI_IMAGE *newim ;
   register int ii , npix ;
   register double scale , val ;
   register short *sar ;


   if( oldim == NULL ) return ( NULL );  /* 09 Feb 1999 */

   newim = mri_new_conforming( oldim , MRI_short ) ;
   sar   = MRI_SHORT_PTR(newim) ;
   npix  = oldim->nvox ;

   if( scl == 0.0 ){
      switch( oldim->kind ){
         case MRI_int:
         case MRI_float:
         case MRI_double:
         case MRI_complex:
            scale = mri_maxabs( oldim ) ;
            if( scale != 0.0 ) scale = 10000.0 / scale ;
         break ;

         default:
            scale = 1.0 ;
         break ;
      }
   } else {
      scale = scl ;
   }

   switch( oldim->kind ){

      case MRI_rgb:{
         byte *rgb = MRI_RGB_PTR(oldim) ;
         float rfac=0.299*scale , gfac=0.587*scale , bfac=0.114*scale ;

         for( ii=0 ; ii < npix ; ii++ )
            sar[ii] = (short)(  rfac * rgb[3*ii]
                              + gfac * rgb[3*ii+1]
                              + bfac * rgb[3*ii+2] ) ;
      }
      break ;

      case MRI_byte:{
         byte *qar = MRI_BYTE_PTR(oldim) ;
         if( scale != 1.0 )
            for( ii=0 ; ii < npix ; ii++ ){
               val = scale * qar[ii] ;
               sar[ii] = SHORTIZE(val) ;
            }
         else
            for( ii=0 ; ii < npix ; ii++ )
               sar[ii] = (short) qar[ii] ;
         break ;
      }

      case MRI_short:{
         short *qar = MRI_SHORT_PTR(oldim) ;
         if( scale != 1.0 )
            for( ii=0 ; ii < npix ; ii++ ){
               val = scale * qar[ii] ;
               sar[ii] = SHORTIZE(val) ;
            }
         else
            (void) memcpy( sar , qar , sizeof(short)*npix ) ;
         break ;
      }

      case MRI_int:{
         int *qar = MRI_INT_PTR(oldim) ;
         if( scale != 1.0 )
            for( ii=0 ; ii < npix ; ii++ ){
               val = scale * qar[ii] ;
               sar[ii] = SHORTIZE(val) ;
            }
         else
            for( ii=0 ; ii < npix ; ii++ )
               sar[ii] = SHORTIZE(qar[ii]) ;
         break ;
      }

      case MRI_float:{
         float *qar = MRI_FLOAT_PTR(oldim) ;
         if( scale != 1.0 )
            for( ii=0 ; ii < npix ; ii++ ){
               val = scale * qar[ii] ;
               sar[ii] = SHORTIZE(val) ;
            }
         else
            for( ii=0 ; ii < npix ; ii++ )
               sar[ii] = SHORTIZE(qar[ii]) ;
         break ;
      }

      case MRI_double:{
         double *qar = MRI_DOUBLE_PTR(oldim) ;
         for( ii=0 ; ii < npix ; ii++ )
            sar[ii] = scale * qar[ii] ;
         break ;
      }

      case MRI_complex:{
        complex *qar = MRI_COMPLEX_PTR(oldim) ;
        for( ii=0 ; ii < npix ; ii++ )
           sar[ii] = scale * CABS(qar[ii]) ;
        break ;
     }

      default:
         fprintf( stderr , "mri_to_short:  unrecognized image kind\n" ) ;
         MRI_FATAL_ERROR ;
   }

   MRI_COPY_AUX(newim,oldim) ;
   return ( newim );
}

/*---------------------------------------------------------------------------*/

MRI_IMAGE *mri_to_float( MRI_IMAGE *oldim )
{
   MRI_IMAGE *newim ;
   register int ii , npix ;
   register float *far ;


   if( oldim == NULL || mri_data_pointer(oldim) == NULL ) return (NULL) ;

   newim = mri_new_conforming( oldim , MRI_float ) ;
   npix  = oldim->nvox ;
   far   = MRI_FLOAT_PTR(newim) ;

   switch( oldim->kind ){

      case MRI_byte:{
         byte *qar = MRI_BYTE_PTR(oldim) ;
         for( ii=0 ; ii < npix ; ii++ ) far[ii] = qar[ii] ;
      }
      break ;

      case MRI_short:{
         short *qar = MRI_SHORT_PTR(oldim) ;
         for( ii=0 ; ii < npix ; ii++ ) far[ii] = qar[ii] ;
      }
      break ;

      case MRI_int:{
         int *qar = MRI_INT_PTR(oldim) ;
         for( ii=0 ; ii < npix ; ii++ ) far[ii] = qar[ii] ;
      }
      break ;

      case MRI_float:{
         float *qar = MRI_FLOAT_PTR(oldim) ;
         (void) memcpy( far , qar , sizeof(float) * npix ) ;
      }
      break ;

      case MRI_double:{
         double *qar = MRI_DOUBLE_PTR(oldim) ;
         for( ii=0 ; ii < npix ; ii++ ) far[ii] = qar[ii] ;
      }
      break ;

      case MRI_complex:{
         complex *qar = MRI_COMPLEX_PTR(oldim) ;
         for( ii=0 ; ii < npix ; ii++ ) far[ii] = CABS(qar[ii]) ;
      }
      break ;

      case MRI_rgb:{                          /* 11 Feb 1999 */
         byte *rgb = MRI_RGB_PTR(oldim) ;
         for( ii=0 ; ii < npix ; ii++ )       /* scale to brightness */
            far[ii] =  0.299 * rgb[3*ii]      /* between 0 and 255     */
                     + 0.587 * rgb[3*ii+1]
                     + 0.114 * rgb[3*ii+2] ;
      }
      break ;

      case MRI_rgba:{                         /* 15 Apr 2002 */
         byte *rgb = (byte *)MRI_RGBA_PTR(oldim) ;
         for( ii=0 ; ii < npix ; ii++ )       /* scale to brightness */
            far[ii] =  0.299 * rgb[4*ii]      /* between 0 and 255     */
                     + 0.587 * rgb[4*ii+1]
                     + 0.114 * rgb[4*ii+2] ;
      }
      break ;

      default:
        fprintf(stderr,"mri_to_float: unrecognized image kind %d\n",oldim->kind);
        MRI_FATAL_ERROR ;
   }

   MRI_COPY_AUX(newim,oldim) ;
   return ( newim );
}

/*** Copy a string into the image structure;
     The usual use is to remember the input filename ***/

void mri_add_name( char *str , MRI_IMAGE *im )
{
   int ll ;

   if( im == NULL ) return ;  /* 29 Mar 2002 */

   if( im->name != NULL ){ free( im->name ) ; im->name = NULL ; }

   if( str == NULL ) return ;

   ll = strlen(str) ; if( ll <= 0 ) return ;

   im->name = (char *) malloc( ll+1 ) ;
   strcpy( im->name , str ) ;
   return ;
}

/*** get a new 2D image ***/

MRI_IMAGE *mri_new( int nx , int ny , MRI_TYPE kind )
{
   MRI_IMAGE *newim ;

   newim = mri_new_7D_generic( nx,ny , 1,1,1,1,1 , kind , TRUE ) ;
   return newim ;
}

/*** make a new 7D image ***/

MRI_IMAGE *mri_new_7D_generic(
            int nx, int ny, int nz, int nt, int nu, int nv, int nw,
            MRI_TYPE kind , int make_space )
{
   MRI_IMAGE *newim ;
   int npix ;


   newim = (MRI_IMAGE *)calloc( 1, sizeof(MRI_IMAGE) ) ;

   if( newim == NULL ){
      fprintf( stderr , "malloc failure for new image pointer\n" ) ;
      MRI_FATAL_ERROR ;
   }

   if( nx < 1 ) nx = 1 ;  /* 18 Mar 2005: fix stupid user problems */
   if( ny < 1 ) ny = 1 ;
   if( nz < 1 ) nz = 1 ;
   if( nt < 1 ) nt = 1 ;
   if( nu < 1 ) nu = 1 ;
   if( nv < 1 ) nv = 1 ;
   if( nw < 1 ) nw = 1 ;

   newim->nx   = nx ;
   newim->ny   = ny ; newim->nxy   = nx*ny ;
   newim->nz   = nz ; newim->nxyz  = nx*ny*nz ;
   newim->nt   = nt ; newim->nxyzt = nx*ny*nz*nt ;
   newim->nu   = nu ;
   newim->nv   = nv ;
   newim->nw   = nw ; newim->nvox  = newim->nxyzt * nu*nv*nw ;

   newim->kind = kind ;
   newim->name = NULL ;

   newim->dx = newim->dy = newim->dz =
   newim->dt = newim->du = newim->dv = 1.0f;  /* default dimensions */

   newim->dw = -666.0f;  /* 05 Feb 2001 - flag that dimensions aren't set */

   newim->xo = newim->yo = newim->zo =
   newim->to = newim->uo = newim->vo = newim->wo = 0.0f;  /* default offsets */

   newim->was_swapped = 0 ;  /* 07 Mar 2002 - flag that bytes were swapped */

   newim->fname   = NULL ;
   newim->foffset = newim->fondisk = 0 ;

   npix = newim->nvox ;

   switch( kind ){
      case MRI_byte:    newim->pixel_size = sizeof(byte)     ; break ;
      case MRI_short:   newim->pixel_size = sizeof(short)    ; break ;
      case MRI_int:     newim->pixel_size = sizeof(int)      ; break ;
      case MRI_float:   newim->pixel_size = sizeof(float)    ; break ;
      case MRI_double:  newim->pixel_size = sizeof(double)   ; break ;
      case MRI_complex: newim->pixel_size = sizeof(complex)  ; break ;
      case MRI_rgb:     newim->pixel_size = 3 * sizeof(byte) ; break ;
      case MRI_rgba:    newim->pixel_size = sizeof(rgba)     ; break ;

      default:
        fprintf( stderr , "mri_new: unrecognized image kind %d\n",(int)kind ) ;
        MRI_FATAL_ERROR ;
   }
   if( make_space ) newim->im = calloc( newim->pixel_size , npix ) ;
   else             newim->im = NULL ;

   if( make_space && mri_data_pointer_unvarnished(newim) == NULL ){
     fprintf(stderr,"malloc failure for image space: %d bytes\n",npix*newim->pixel_size);
     MRI_FATAL_ERROR ;
   }

   return (newim) ;
}
/* ------------------------------------------------------------ */
/* GE MR Signa 4.x header type : GEMS 46-021858                 */

#define GE4_HEADER_LENGTH   0x03800    /*  28 x 256 x 2 ( 28 blocks) */
#define GE4_IMAGE_SIZE      0x20000    /* 256 x 256 x 2 (256 blocks) */

/* ---- display constants ---- */

#define GE4_DISP_NONE	       0x00
#define GE4_DISP_IMAGE	       0x01
#define GE4_DISP_SERIES	       0x02
#define GE4_DISP_STUDY	       0x04
#define GE4_DISP_ALL	       0xff

/* ---- string constants ---- */

#define GE4_IMAGE_TITLE   "IMAGE HEADER  04"
#define GE4_SERIES_TITLE  "SERIES HEADER 04"
#define GE4_STUDY_TITLE   "STUDY HEADER  04"

/* ---- series header field offsets (base + 2 * word_num) ---- */

#define GE4_OFF_SER_TITLE		0x1000	/* = (0x1000) 		*/
#define GE4_OFF_SER_SERIES_NUM		0x103e	/* = (0x1000 + 2 * 031) */
#define GE4_OFF_SER_PLANE_TYPE		0x1114	/* = (0x1000 + 2 * 138) */
#define GE4_OFF_SER_PLANE_DESC		0x1116	/* = (0x1000 + 2 * 139) */
#define GE4_OFF_SER_IM_MODE		0x1126	/* = (0x1000 + 2 * 147) */
#define GE4_OFF_SER_PULSE_SEQ		0x112a	/* = (0x1000 + 2 * 149) */
#define GE4_OFF_SER_FOV			0x112e	/* = (0x1000 + 2 * 151) */
#define GE4_OFF_SER_CENTER		0x1132	/* = (0x1000 + 2 * 153) */
#define GE4_OFF_SER_ORIENT		0x113e	/* = (0x1000 + 2 * 159) */
#define GE4_OFF_SER_SCAN_MAT_X		0x118e	/* = (0x1000 + 2 * 199) */
#define GE4_OFF_SER_SCAN_MAT_Y		0x1190	/* = (0x1000 + 2 * 200) */
#define GE4_OFF_SER_IM_MAT		0x1192	/* = (0x1000 + 2 * 201) */

/* ---- string lengths ---- */

#define GE4_L_SER_TITLE		16
#define GE4_L_SER_SER_NUM	 3
#define GE4_L_SER_PL_DESC	12

/* ---- image header field offsets (base + 2 * word_num) ---- */

#define GE4_OFF_IMG_TITLE		0x1400
#define GE4_OFF_IMG_IM_NUM		0x1458	/* = (0x1400 + 2 * 044) */
#define GE4_OFF_IMG_IM_LOCN		0x1492	/* = (0x1400 + 2 * 073) */
#define GE4_OFF_IMG_TABLE_POSN		0x1496	/* = (0x1400 + 2 * 075) */
#define GE4_OFF_IMG_IM_THICK		0x149a	/* = (0x1400 + 2 * 077) */
#define GE4_OFF_IMG_IM_SPACING		0x149e	/* = (0x1400 + 2 * 079) */
#define GE4_OFF_IMG_TR			0x14a4	/* = (0x1400 + 2 * 082) */
#define GE4_OFF_IMG_TE			0x14ac	/* = (0x1400 + 2 * 086) */
#define GE4_OFF_IMG_TI			0x14b0	/* = (0x1400 + 2 * 088) */
#define GE4_OFF_IMG_NUM_ECHOS		0x14c4	/* = (0x1400 + 2 * 098) */
#define GE4_OFF_IMG_ECHO_NUM		0x14c6	/* = (0x1400 + 2 * 099) */
#define GE4_OFF_IMG_NEX_INT		0x14ca	/* = (0x1400 + 2 * 101) */
#define GE4_OFF_IMG_NEX_REAL		0x1524	/* = (0x1400 + 2 * 146) */
#define GE4_OFF_IMG_FLIP_ANGLE		0x155e	/* = (0x1400 + 2 * 175) */

/* ---- image header field lengths ---- */

#define GE4_L_IM_TITLE		16
#define GE4_L_IM_NUM		 3

/* ---- study header field offsets (base + 2 * word_num) ---- */

#define GE4_OFF_STDY_TITLE		0x0c00
#define GE4_OFF_STDY_NUM		0x0c40	/* = (0x0c00 + 2 * 032) */
#define GE4_OFF_STDY_DATE		0x0c4e	/* = (0x0c00 + 2 * 039) */
#define GE4_OFF_STDY_TIME		0x0c5e	/* = (0x0c00 + 2 * 047) */
#define GE4_OFF_STDY_PAT_NAME		0x0c6c	/* = (0x0c00 + 2 * 054) */
#define GE4_OFF_STDY_PAT_ID		0x0c8c	/* = (0x0c00 + 2 * 070) */
#define GE4_OFF_STDY_AGE		0x0c9c	/* = (0x0c00 + 2 * 078) */
#define GE4_OFF_STDY_SEX		0x0ca0	/* = (0x0c00 + 2 * 080) */

/* ---- study header field lengths ---- */

#define GE4_L_STDY_TITLE	16
#define GE4_L_STDY_NUM		 5
#define GE4_L_STDY_DATE		 9
#define GE4_L_STDY_TIME		 8
#define GE4_L_STDY_PAT_NAME	32
#define GE4_L_STDY_PAT_ID	12
#define GE4_L_STDY_AGE		 3

/* ---------------------------------------------------------------------- */

/* ---- actual data structures ---- */
typedef struct
{
    char    title     [GE4_L_STDY_TITLE    + 1];
    char    num       [GE4_L_STDY_NUM      + 1];
    char    date      [GE4_L_STDY_DATE     + 1];
    char    time      [GE4_L_STDY_TIME     + 1];
    char    pat_name  [GE4_L_STDY_PAT_NAME + 1];
    char    pat_id    [GE4_L_STDY_PAT_ID   + 1];
    char    age       [GE4_L_STDY_AGE      + 1];
    char    sex;
} ge4_study_t;

typedef struct
{
    char    title     [GE4_L_SER_TITLE+1];	/* words 000-015 */
    char    series_num[GE4_L_SER_SER_NUM+1];	/* word  031     */
    short   plane_type;				/* word  138     */
    char    plane_desc[GE4_L_SER_PL_DESC+1];	/* word  139     */
    short   im_mode;				/* word  147     */
    short   pulse_seq;				/* word  149     */
    float   fov;				/* words 151,152 */
    float   center[3];				/* words 153-158 */
    short   orient;				/* word  159     */
    short   scan_mat_x;				/* word  199     */
    short   scan_mat_y;				/* word  200     */
    short   im_mat;				/* word  201     */
} ge4_series_t;

typedef struct
{
    char    title [GE4_L_IM_TITLE+1];		/* word  000     */
    char    im_num[GE4_L_IM_NUM+1];		/* words 044-046 */
    float   im_loc;				/* words 073,074 */
    float   table_posn;				/* words 075,076 */
    float   im_thickness;			/* words 077,078 */
    float   im_spacing;				/* words 079,080 */
    float   tr;			/* uS */	/* words 082,083 */
    float   te;			/* uS */	/* words 082,083 */
    float   ti;			/* uS */	/* words 082,083 */
    short   num_echoes;				/* word  098     */
    short   echo_num;				/* word  099     */
    short   iNEX;				/* word  101     */
    float   fNEX;				/* words 146,147 */
    short   flip_angle;				/* word  175     */
} ge4_image_t;

typedef struct
{
    ge4_study_t		std_h;		/* series header data      */
    ge4_series_t	ser_h;		/* series header data      */
    ge4_image_t 	im_h;		/* image header data       */
    short             * image;		/* image data, if non-NULL */
    int                 im_bytes;	/* size of image, in bytes */
    int			swap;		/* was the data swapped?   */
} ge4_header;


/* global prototypes */
int ge4_read_header	     ( ge4_header * H, char * filename, int get_image );


int ge4_swap_all_bytes	     ( ge4_header * h );
int ge4_validate_header      ( ge4_header * h );


/*---------------------------------------------------------------*/

/*** for non ANSI compilers ***/

#ifndef SEEK_SET
#define SEEK_SET 0
#endif

/*---------------------------------------------------------------*/
static MRI_IMAGE * mri_try_mri( FILE * , int * ) ;  /* prototypes */
static MRI_IMAGE * mri_try_7D ( FILE * , int * ) ;
static MRI_IMAGE * mri_try_pgm( FILE * , int * ) ;
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

#undef myswap_4
#undef myswap_2

/*! Swap the 4 bytes pointed to by ppp: abcd -> dcba. */

static void myswap_4(void *ppp)
{
   unsigned char *pntr = (unsigned char *) ppp ;
   unsigned char b0, b1, b2, b3;

   b0 = *pntr; b1 = *(pntr+1); b2 = *(pntr+2); b3 = *(pntr+3);
   *pntr = b3; *(pntr+1) = b2; *(pntr+2) = b1; *(pntr+3) = b0;
}

/*---------------------------------------------------------------*/

/*! Swap the 2 bytes pointed to by ppp: ab -> ba. */

static void myswap_2(void *ppp)
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
   void      *data=NULL ;

   char *eee=NULL ; int fpair=0 , allow_fpair=0 ; MRI_IMAGE *fpim=NULL ; void *fpdata=NULL ;

   if( fname == NULL || *fname == '\0' ) return (NULL) ;  /* bad user */

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

     fprintf(stderr,"Code to read files of type '%s' is commented out!\n",fname) ;
     return (NULL) ;
   }

   /*-- 16 Aug 2006: AFNI dataset? --*/

   /*-- check if file exists and is readable --*/

   imfile = fopen( fname , "r" ) ;
   if( imfile == NULL ){
     fprintf( stderr , "couldn't open image file %s\n" , fname ) ;
     return ( NULL );
   }

   length = THD_filesize(fname) ;     /* 22 Mar 2007 */

   /*--- 03 Dec 2001: check for GEMS format file "IMGF"   ---*/
   /*[[[ Information herein from Medical Image Format FAQ ]]]*/

   { char str[5]="AFNI" ;
     int nx , ny , bpp , cflag , hdroff , extraskip=0, vv ;
     rewind(imfile) ; vv=fread(str,1,4,imfile) ;   /* check for "IMGF" or "GEMS" */

     if( str[0]=='G' && str[1]=='E' && str[2]=='M' && str[3]=='S' ){ /* 12 Feb 2004 */
       char buf[4096]; int bb,cc;                /* search for IMGF in 1st 4K */
       rewind(imfile); cc=fread(buf,1,4096,imfile); cc-=4 ;
       for( bb=4; bb < cc ; bb++ )
        if( buf[bb]=='I' && buf[bb+1]=='M' && buf[bb+2]=='G' && buf[bb+3]=='F' ) break ;
       if( bb < cc ){
         fseek( imfile , (long) bb , SEEK_SET ) ; extraskip = bb ;
         vv=fread(str,1,4,imfile) ;
       }
     }

     /* 12 Feb 2004: modified to allow for starting at extraskip */

     if( str[0]=='I' && str[1]=='M' && str[2]=='G' && str[3]=='F' ){

       vv=fread( &skip , 4,1, imfile ) ;  /* read next 5 ints */
       vv=fread( &nx   , 4,1, imfile ) ;
       vv=fread( &ny   , 4,1, imfile ) ;
       vv=fread( &bpp  , 4,1, imfile ) ;
       vv=fread( &cflag, 4,1, imfile ) ;

       if( nx < 0 || nx > 8192 ){      /* maybe have to byte swap 5 ints */
         swap = 1 ;                    /* flag that we are swapping data */
         myswap_4(&skip); myswap_4(&nx) ;
         myswap_4(&ny)  ; myswap_4(&bpp); myswap_4(&cflag);
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
          vv=fread( &hdroff , 4,1 , imfile ) ;  /* location of image header */
          if( swap ) myswap_4(&hdroff) ;
          if( hdroff > 0 ){                  /* read from image header */
             float dx,dy,dz, dxx,dyy,dzz, xyz[9], zz ; int itr, ii,jj,kk, qq ;
             static int nzoff=0 ;
             static float zoff ;

             /* get voxel grid sizes */

             fseek( imfile , hdroff+26+extraskip , SEEK_SET ) ;
             vv=fread( &dzz , 4,1 , imfile ) ;

             fseek( imfile , hdroff+50+extraskip , SEEK_SET ) ;
             vv=fread( &dxx , 4,1 , imfile ) ;
             vv=fread( &dyy , 4,1 , imfile ) ;

             if( swap ){ myswap_4(&dxx); myswap_4(&dyy); myswap_4(&dzz); }

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
             vv=fread( xyz , 4,9 , imfile ) ;
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
               vv=fread( &itr , 4,1 , imfile ) ;
               if( swap ) myswap_4(&itr) ;
               MRILIB_tr = im->dt = 1.0e-6 * itr ;
             }
          }
       } /* end of trying to read image header */

       goto Ready_To_Roll ;  /* skip to the reading place */
     }
   } /* end of GEMS */

   /*--- OK, do it the old way ---*/

                     eee = getenv("AFNI_FLOATPAIR_IMAGES") ;
   if( eee == NULL ) eee = getenv("FD2_FLOATPAIR_IMAGES") ;
   if( eee == NULL ) eee = getenv("FLOATPAIR_IMAGES") ;
   allow_fpair = ( eee != NULL && (*eee=='Y' || *eee=='y') ) ;

#undef  MKIM
#define MKIM(nn)                             \
 do{ if( !allow_fpair ){                     \
         im = mri_new(nn,nn,MRI_short) ;     \
       fpim = NULL ;                         \
     } else {                                \
         im = mri_new(nn/2,nn/2,MRI_float) ; \
       fpim = mri_new(nn/2,nn/2,MRI_float) ; \
     }                                       \
 } while(0)

The_Old_Way:

   switch( length ){

      case 512:    /* raw 16x16 short -- RWCox: 06 Dec 2001 */
         MKIM(16) ;
         break ;

      case 2048:   /* raw 32x32 short -- RWCox: 19 Sep 2000 */
         MKIM(32) ;
         break ;

      case 4096:   /* raw 64x64 byte -- RWC 3/21/95 */
         im = mri_new( 64 , 64 , MRI_byte ) ;
         break ;

      case 8192:   /* raw 64x64 short */
      case 16096:  /* with Signa 5.x header */
         MKIM(64) ;
         skip = length - 8192 ;
         break ;

      case 16384:  /* raw 128x128 byte -- RWC 3/21/95 */
         im = mri_new( 128 , 128 , MRI_byte ) ;
         break ;

      case 32768:  /* raw 128x128 short */
      case 40672:  /* with Signa 5.x header */
         MKIM(128) ;
         skip = length - 32768 ;
         break ;

      case 65536:  /* raw 256x256 8-bit -- Matthew Belmonte March 1995 */
         im = mri_new( 256 , 256 , MRI_byte ) ;
         break ;

      case 131072:  /* raw 256x256 short */
      case 138976:  /* Signa 5.x */
      case 145408:  /* Signa 4.x */

         MKIM(256) ;
         skip = length - 131072 ;
         break ;

      case 262144:  /* raw 512x512 byte -- RWC 3/21/95 */
         im = mri_new( 512 , 512 , MRI_byte ) ;
         break ;

      case 524288:  /* raw 512x512 short -- RWC 3/21/95 */
         MKIM(512) ;
         break ;

      case 1048576: /* raw 1024x1024 byte -- RWC 3/21/95 */
         im = mri_new( 1024 , 1024 , MRI_byte ) ;
         break ;

      case 2097152: /* raw 1024x1024 short -- RWC 3/21/95 */
         MKIM(1024) ;
         break ;

      /** not a canonical length: try something else **/

      default:
                          im = mri_try_mri( imfile , &skip ) ;  /* Cox format */
         if( im == NULL ) im = mri_try_7D ( imfile , &skip ) ;  /* 7D format  */
         if( im == NULL ) im = mri_try_pgm( imfile , &skip ) ;  /* PGM format */
         if( im != NULL ) break ;

         fclose( imfile ) ; /* close it, since we failed (so far) */

         fprintf( stderr , "do not recognize image file %s\n" , fname );
         fprintf( stderr , "length seen as %d\n" , length ) ;
         return ( NULL );
   }

   /*-- Actually read the data from disk --*/

Ready_To_Roll:

   data = mri_data_pointer( im ) ;

   length = fseek( imfile , skip , SEEK_SET ) ;
   if( length != 0 ){
      fprintf( stderr , "mri_read error in skipping in file %s\n" , fname ) ;
      mri_free( im ) ; if( fpim != NULL ) mri_free(fpim) ;
      fclose(imfile) ; return ( NULL );
   }

   length = fread( data , im->pixel_size , im->nvox , imfile ) ;

   if( fpim != NULL ){
     fpdata = mri_data_pointer(fpim) ;
     length = fread( fpdata , fpim->pixel_size , fpim->nvox , imfile ) ;
   }

   fclose( imfile ) ;

   if( length != im->nvox ){
      mri_free( im ) ; if( fpim != NULL ) mri_free(fpim) ;
      fprintf( stderr , "couldn't read image data from file %s\n" , fname ) ;
      return ( NULL );
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

     if( fpim != NULL ) swap_fourbytes( fpim->nvox , fpdata ) ;

     im->was_swapped = 1 ;  /* 07 Mar 2002 */
   }

   if( fpim != NULL ){
     float *xim = (float *)data , *yim = (float *)fpdata ; int ii ;
     for( ii=0 ; ii < im->nvox ; ii++ )
       xim[ii] = sqrtf( xim[ii]*xim[ii] + yim[ii]*yim[ii] ) ;
     mri_free(fpim) ; fpim = NULL ;
   }

   return ( im );
}


/******************************************************************/

/* GEMS 4.x image reading   - rickr  03 Jun 2003 */

/*! Read a single 2D GEMS 4.x image.

    \param  filename is the name of the file to try to read
    \return NULL if an image could not be read, otherwise
            the address of a new MRI_IMAGE structure
*/

#define True 1

MRI_IMAGE * mri_read_ge4( char * filename )
{
    MRI_IMAGE * im;
    ge4_header  H;


    if ( filename == NULL )
    {
        fprintf( stderr, "** mri_read_ge4 - missing filename\n" );
        return ( NULL );
    }

    /* try to read image file - return with image */
    if ( ge4_read_header( &H, filename, True ) != 0 )
        return ( NULL );

    /* these dimensions are fixed */
    if ( (im = mri_new(256, 256, MRI_short)) == NULL )
    {
        free(H.image);
        return ( NULL );
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

    return ( im );
}


/*********************************************************************/

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


   fseek( imfile , 0 , SEEK_SET ) ;  /* rewind file */

   ch = getc( imfile ) ; if( ch != 'M' ) return ( NULL );  /* check for MRI */
   ch = getc( imfile ) ; if( ch != 'R' ) return ( NULL );
   ch = getc( imfile ) ; if( ch != 'I' ) return ( NULL );

   /* magic MRI found, so read numbers */

   ch = getc(imfile) ;

   NUMSCAN(imcode) ;
   NUMSCAN(nx) ;  if( nx <= 0 ) return ( NULL );
   NUMSCAN(ny) ;  if( ny <= 0 ) return ( NULL );

   *skip = ftell(imfile) ;
   im    = mri_new( nx , ny , imcode ) ;
   return ( im );
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


   fseek( imfile , 0 , SEEK_SET ) ;  /* rewind file */

   ch = getc( imfile ) ; if( ch != 'M' ) return ( NULL );  /* check for MR[1-7] */
   ch = getc( imfile ) ; if( ch != 'R' ) return ( NULL );
   ch = getc( imfile ) ;
   switch( ch ){
      default:  return ( NULL );   /* not what I expected */

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

                   NUMSCAN(nx) ;  if( nx <= 0 ) return ( NULL );
   if( ndim > 1 ){ NUMSCAN(ny) ;  if( ny <= 0 ) return ( NULL ); }
   if( ndim > 2 ){ NUMSCAN(nz) ;  if( nz <= 0 ) return ( NULL ); }
   if( ndim > 3 ){ NUMSCAN(nt) ;  if( nt <= 0 ) return ( NULL ); }
   if( ndim > 4 ){ NUMSCAN(nu) ;  if( nu <= 0 ) return ( NULL ); }
   if( ndim > 5 ){ NUMSCAN(nv) ;  if( nv <= 0 ) return ( NULL ); }
   if( ndim > 6 ){ NUMSCAN(nw) ;  if( nw <= 0 ) return ( NULL ); }

   *skip = ftell(imfile) ;
   im    = mri_new_7D_generic( nx,ny,nz,nt,nu,nv,nw , imcode , TRUE ) ;
   return ( im );
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


   fseek( imfile , 0 , SEEK_SET ) ;  /* rewind file */

   ch = getc( imfile ) ; if( ch != 'P' ) return (NULL);  /* check for magic */
   ch = getc( imfile ) ; if( ch != '5' ) return (NULL);

   /* magic P5 found, so read numbers */

   ch = getc(imfile) ;

   NUMSCAN(nx)     ; if( nx     <= 0 ) return (NULL);
   NUMSCAN(ny)     ; if( ny     <= 0 ) return (NULL);
   NUMSCAN(maxval) ; if( maxval <= 0 || maxval >  255 ) return (NULL);

   *skip = ftell(imfile) ;
   im    = mri_new( nx , ny , MRI_byte ) ;
   return (im);
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
   int hglobal , himage , nx , ny , nz, vv;
   char fname[256] , buf[512] ;
   int ngood , kim , datum_type , datum_len , swap ;
   MRI_IMARR *newar ;
   MRI_IMAGE *newim ;
   void      *imar ;
   FILE      *imfile ;
   __u64     length , nneed , koff , hglob ;  /* 22 Mar 2007 */


   /*** get info from 3D tname ***/

   if( tname == NULL || strlen(tname) < 10 ) return (NULL) ;

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
       strlen(fname) <= 0                   ) return (NULL);   /* bad info */

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
      return (newar);
   }

   /*** open the input file and position it ***/

   imfile = fopen( fname , "r" ) ;
   if( imfile == NULL ){
     fprintf( stderr , "couldn't open image file %s\n" , fname ) ;
     return (NULL);
   }

   length = THD_filesize(fname) ;     /* 22 Mar 2007 */

   /** 13 Apr 1999: modified to allow actual hglobal < -1
                    as long as hglobal+himage >= 0       **/

   hglob = hglobal ;
   if( hglob == -1 || hglob+himage < 0 ){
      hglob = length - (datum_len*nx*ny+himage) * (__u64) nz ;
      if( hglob < 0 ) hglob = 0 ;
   }

   nneed = hglob + (datum_len*nx*ny+himage) * (__u64) nz ;
   if( length < nneed ){
      fprintf(stderr,
        "image file %s is %lld bytes long but must be at least %lld bytes long\n"
        "  for hglobal=%lld himage=%d nx=%d ny=%d nz=%d and voxel=%d bytes\n",
        fname, length, nneed, hglob, himage, nx, ny, nz, datum_len ) ;
      fclose( imfile ) ;
      return (NULL);
   }

   /*** read images from the file ***/

   INIT_IMARR(newar) ;

   for( kim=0 ; kim < nz ; kim++ ){
      koff = hglob + (kim+1)*himage + datum_len*nx*ny * (__u64)kim ;
      fseeko( imfile, (off_t)koff, SEEK_SET ) ; /* 22 Mar 2007: fseek->fseeko */

      newim  = mri_new( nx , ny , datum_type ) ;
      imar   = mri_data_pointer( newim ) ;
      vv = fread( imar , datum_len , nx * ny , imfile ) ;
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
   return (newar);
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


   /* convert fname to new_fname, based on environment */

   new_fname = imsized_fname( fname ) ;
   if( new_fname == NULL ) return ( NULL );

   /* input method is based on filename */

   if( strlen(new_fname) > 9 &&
       new_fname[0] == '3'   &&
       new_fname[1] == 'D'   &&
      (new_fname[2] == ':' || new_fname[3] == ':') ){

      newar = mri_read_3D( new_fname ) ;   /* read from a 3D: file */

   } else if( strstr(new_fname,".hdr") != NULL ||
              strstr(new_fname,".HDR") != NULL   ){  /* 05 Feb 2001 */

      newar = mri_read_analyze75( new_fname ) ;      /* ANALYZE .hdr/.img filepair */

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
   }

   /** failed to read anything?  try DICOM format (doesn't have a fixed suffix) **/
   /* 05 May 2003 added option to try DICOM last                    KRH          */

   if( newar == NULL ){
      /** if DICOM failed, try a 2D slice file, hope for the best **/

      if( newar == NULL && tried_dicom != 2 ){
        newim = mri_read( new_fname ) ;
        if( newim == NULL ){ free(new_fname); return ( NULL ); }  /* give up */
        INIT_IMARR(newar) ;
        ADDTO_IMARR(newar,newim) ;
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

   return ( newar );
}  // end of mri_read_file()

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


   new_fname = imsized_fname( fname ) ;
   if( new_fname == NULL ) return ( NULL );

   imar = mri_read_file( new_fname ) ; free(new_fname) ;
   if( imar == NULL ) return ( NULL );
   if( imar->num != 1 ){ DESTROY_IMARR(imar) ; return ( NULL ); }
   im = IMAGE_IN_IMARR(imar,0) ;
   FREE_IMARR(imar) ;
   return ( im );
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
      str = getenv( ename ) ;

      if( str == NULL ){
         sprintf( ename , "MCW_IMSIZE_%d" , num+1 ) ;
         str = getenv( ename ) ;
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
   int len;  /* 22 Mar 2007 */
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

/*------------------------------------------------------------------------*/
/*! Return the size of a file in bytes.

  \param pathname = input filename
  \return File length if file exists; -1 if it doesn't.
  \see THD_filesize() in thd_filestuff.c.
*/

unsigned int THD_filesize( char * pathname )
{
   static struct stat buf ;
   int ii ;

   if( pathname == NULL ) return -1 ;
   ii = stat( pathname , &buf ) ; if( ii != 0 ) return -1 ;
   return (unsigned int)buf.st_size ;
}

/*---------------------------------------------------------------*/

/*---------------------------------------------------------------------------
   Stuff to read an ANALYZE 7.5 .img file, given the .hdr filename
   -- 05 Feb 2001 - RWCox
   -- 27 Nov 2001 - modified to use funused1 as a scale factor
-----------------------------------------------------------------------------*/

/***************************************************************************
From http://www.mayo.edu/bir/Analyze_Pages/AnalyzeFileInfo.html
Describes the ANALYZE 7.5 file format (.img/.hdr pairs).
----------------------------------------------------------------
The image database is the system of files that the ANALYZE package uses to
organize and access image data on the disk. Facilities are provided for
converting data from a number of sources for use with the package. A
description of the database format is provided to aid developers in porting
images from other sources for use with the ANALYZE TM system. An ANALYZE
image database consists of at least two files:

   * an image file
   * a header file

The files have the same name being distinguished by the extensions .img for
the image file and .hdr for the header file. Thus, for the image database
heart, there are the UNIX files heart.img and heart.hdr. The ANALYZE
programs all refer to this pair of files as a single entity named heart.

Image File

The format of the image file is very simple containing usually uncompressed
pixel data for the images in one of several possible pixel formats:

   * 1 bit            packed binary (slices must begin on byte boundaries)
   * 8 bit            8 bits per pixel (unsigned char)
   * 16 bit           16 bits per pixel (signed short)
   * 32 bit           32 bits per pixel signed integers, or floating point
   * 64 bit           64 bits per pixel; double precision, floating point,
                       or complex.
   * 24 bit           RGB , 8-bits per channel Red, Green, Blue.

Header File

The header file is represented here as a `C' structure which describes the
dimensions and history of the pixel data. The header structure consists of
three substructures:

     header_key         describes the header
     image_dimension    describes image sizes
     data_history       optional
****************************************************************************/

struct header_key                       /* header key   */
       {                                /* off + size      */
       int sizeof_hdr;                  /* 0 + 4           */
       char data_type[10];              /* 4 + 10          */
       char db_name[18];                /* 14 + 18         */
       int extents;                     /* 32 + 4          */
       short int session_error;         /* 36 + 2          */
       char regular;                    /* 38 + 1          */
       char hkey_un0;                   /* 39 + 1          */
       };                               /* total=40 bytes  */
struct image_dimension
       {                                /* off + size      */
       short int dim[8];                /* 0 + 16          */
       short int unused8;               /* 16 + 2          */
       short int unused9;               /* 18 + 2          */
       short int unused10;              /* 20 + 2          */
       short int unused11;              /* 22 + 2          */
       short int unused12;              /* 24 + 2          */
       short int unused13;              /* 26 + 2          */
       short int unused14;              /* 28 + 2          */
       short int datatype;              /* 30 + 2          */
       short int bitpix;                /* 32 + 2          */
       short int dim_un0;               /* 34 + 2          */
       float pixdim[8];                 /* 36 + 32         */
                       /*
                            pixdim[] specifies the voxel dimensitons:
                            pixdim[1] - voxel width
                            pixdim[2] - voxel height
                            pixdim[3] - interslice distance
                                ...etc
                       */
       float vox_offset;                /* 68 + 4          */
       float funused1;                  /* 72 + 4          */
       float funused2;                  /* 76 + 4          */
       float funused3;                  /* 80 + 4          */
       float cal_max;                   /* 84 + 4          */
       float cal_min;                   /* 88 + 4          */
       float compressed;                /* 92 + 4          */
       float verified;                  /* 96 + 4          */
       int glmax,glmin;                 /* 100 + 8         */
       };                               /* total=108 bytes */
struct data_history
       {                                /* off + size      */
       char descrip[80];                /* 0 + 80          */
       char aux_file[24];               /* 80 + 24         */
       char orient;                     /* 104 + 1         */
       char originator[10];             /* 105 + 10        */
       char generated[10];              /* 115 + 10        */
       char scannum[10];                /* 125 + 10        */
       char patient_id[10];             /* 135 + 10        */
       char exp_date[10];               /* 145 + 10        */
       char exp_time[10];               /* 155 + 10        */
       char hist_un0[3];                /* 165 + 3         */
       int views;                       /* 168 + 4         */
       int vols_added;                  /* 172 + 4         */
       int start_field;                 /* 176 + 4         */
       int field_skip;                  /* 180 + 4         */
       int omax, omin;                  /* 184 + 8         */
       int smax, smin;                  /* 192 + 8         */
       };
struct dsr
       {
       struct header_key hk;            /* 0 + 40          */
       struct image_dimension dime;     /* 40 + 108        */
       struct data_history hist;        /* 148 + 200       */
       };                               /* total= 348 bytes*/

/* Acceptable values for datatype */

#define ANDT_NONE             0
#define ANDT_UNKNOWN          0  /* what it says, dude           */
#define ANDT_BINARY           1  /* binary (1 bit/voxel)         */
#define ANDT_UNSIGNED_CHAR    2  /* unsigned char (8 bits/voxel) */
#define ANDT_SIGNED_SHORT     4  /* signed short (16 bits/voxel) */
#define ANDT_SIGNED_INT       8  /* signed int (32 bits/voxel)   */
#define ANDT_FLOAT           16  /* float (32 bits/voxel)        */
#define ANDT_COMPLEX         32  /* complex (64 bits/voxel)      */
#define ANDT_DOUBLE          64  /* double (64 bits/voxel)       */
#define ANDT_RGB            128  /* RGB triple (24 bits/voxel)   */
#define ANDT_ALL            255

#define ANDT_string(aa)                     \
 ((aa)==ANDT_BINARY        ? "binary"       \
 :(aa)==ANDT_UNSIGNED_CHAR ? "byte"         \
 :(aa)==ANDT_SIGNED_SHORT  ? "short"        \
 :(aa)==ANDT_SIGNED_INT    ? "int"          \
 :(aa)==ANDT_FLOAT         ? "float"        \
 :(aa)==ANDT_COMPLEX       ? "complex"      \
 :(aa)==ANDT_DOUBLE        ? "double"       \
 :(aa)==ANDT_RGB           ? "RGB"          \
 :                           "unknown" )

/***************************************************************************
The header format is flexible and can be extended for new user-defined data
types. The essential structures of the header are the header_key and the
image_dimension.

The required elements in the header_key substructure are:

     int sizeof_header      Must indicate the byte size of the header file.
     int extents            Should be 16384, the image file is created as
                              contiguous with a minimum extent size.
     char regular           Must be `r' to indicate that all images and
                            volumes are the same size.

The image_dimension substructure describes the organization and size of the
images. These elements enable the database to reference images by volume and
slice number. Explanation of each element follows:

     short int dim[]; = array of the image dimensions
          dim[0]      = Number of dimensions in database; usually 4
          dim[1]      = Image X dimension; number of pixels in an image row
          dim[2]      = Image Y dimension; number of pixel rows in slice
          dim[3]      = Volume Z dimension; number of slices in a volume
          dim[4]      = Time points, number of volumes in database.

          char vox_units[4] = specifies the spatial units of measure for a
                               voxel
          char cal_units[4] = specifies the name of the calibration unit

         short int datatype = datatype for this image set
                               Acceptable values for datatype are one of
                               the ANDT_* defines above

         short int bitpix   = number of bits per pixel; 1, 8, 16, 32, 64
         short int dim_un0  = unused

         float pixdim[]     = Parallel array to dim[], giving real world
                               measurements in mm and ms.
          pixdim[1]         = voxel width in mm
          pixdim[2]         = voxel height in mm
          pixdim[3]         = slice thickness in mm
         float vox_offset   = byte offset in the .img file at which voxels
                               start. This value can be negative to specify
                               that the absolute value is applied for every image
                               in the file
         float cal_max,     = specify the range of calibration values
               cal_min
         int glmax, glmin   = The maximum and minimum pixel values for the
                               entire database

The data_history substructure is not required, but the orient field is used
to indicate individual slice orientation and determines whether the Movie
program will attempt to flip the images before displaying a movie sequence.

     orient = slice orientation for this dataset.
                0 = transverse unflipped
                1 = coronal unflipped
                2 = sagittal unflipped
                3 = transverse flipped
                4 = coronal flipped
                5 = sagittal flipped
****************************************************************************/

/*---------------------------------------------------------------*/
/*! Byte swap ANALYZE file header in various places */

static void swap_analyze_hdr( struct dsr *pntr )
{
   myswap_4(&pntr->hk.sizeof_hdr) ;
   myswap_4(&pntr->hk.extents) ;
   myswap_2(&pntr->hk.session_error) ;
   myswap_2(&pntr->dime.dim[0]) ;
   myswap_2(&pntr->dime.dim[1]) ;
   myswap_2(&pntr->dime.dim[2]) ;
   myswap_2(&pntr->dime.dim[3]) ;
   myswap_2(&pntr->dime.dim[4]) ;
   myswap_2(&pntr->dime.dim[5]) ;
   myswap_2(&pntr->dime.dim[6]) ;
   myswap_2(&pntr->dime.dim[7]) ;
   myswap_2(&pntr->dime.datatype) ;
   myswap_2(&pntr->dime.bitpix) ;
   myswap_4(&pntr->dime.pixdim[0]) ;
   myswap_4(&pntr->dime.pixdim[1]) ;
   myswap_4(&pntr->dime.pixdim[2]) ;
   myswap_4(&pntr->dime.pixdim[3]) ;
   myswap_4(&pntr->dime.pixdim[4]) ;
   myswap_4(&pntr->dime.pixdim[5]) ;
   myswap_4(&pntr->dime.pixdim[6]) ;
   myswap_4(&pntr->dime.pixdim[7]) ;
   myswap_4(&pntr->dime.vox_offset) ;
   myswap_4(&pntr->dime.funused1) ;
   myswap_4(&pntr->dime.funused2) ;
   myswap_4(&pntr->dime.cal_max) ;
   myswap_4(&pntr->dime.cal_min) ;
   myswap_4(&pntr->dime.compressed) ;
   myswap_4(&pntr->dime.verified) ;
   myswap_2(&pntr->dime.dim_un0) ;
   myswap_4(&pntr->dime.glmax) ;
   myswap_4(&pntr->dime.glmin) ;
   return ;
}

/*---------------------------------------------------------------*/
/*! Read an ANALYZE file into an ARRAY of 2D images.

   \param hname = the "hdr" file for the hdr/img pair
*/

MRI_IMARR * mri_read_analyze75( char * hname )
{
   FILE * fp ;
   char iname[1024] , buf[1024] ;
   int jj , doswap ,vv;
   struct dsr hdr ;    /* ANALYZE .hdr format */
   int ngood , length , kim , koff , datum_type , datum_len;
   int   nx,ny,nz , hglobal=0 , himage=0 ;
   float dx,dy,dz ;
   MRI_IMARR * newar ;
   MRI_IMAGE * newim ;
   void      * imar ;
   float fac=0.0 ;    /* 27 Nov 2001 */
   int floatize ;     /* 28 Nov 2001 */
   int spmorg=0 ;     /* 28 Nov 2001 */


   /* check & prepare filenames */

   if( hname == NULL ) return (NULL) ;
   jj = strlen(hname) ;
   if( jj < 5 ) return (NULL) ;
   if( strcmp(hname+jj-3,"hdr") != 0 ) return (NULL) ;
   strcpy(iname,hname) ; strcpy(iname+jj-3,"img") ;

   /* read header file into struct */

   fp = fopen( hname , "rb" ) ;
   if( fp == NULL ) return (NULL) ;
   hdr.dime.dim[0] = 0 ;
   vv = fread( &hdr , 1 , sizeof(struct dsr) , fp ) ;
   fclose(fp) ;
   if( hdr.dime.dim[0] == 0 ) return (NULL) ;

   /* check for swap-age */

   doswap = (hdr.dime.dim[0] < 0 || hdr.dime.dim[0] > 15) ;
   if( doswap ) swap_analyze_hdr( &hdr ) ;

   /* 28 Nov 2001: attempt to decode originator a la SPM */

   { short xyzuv[5] , xx,yy,zz ;
     memcpy( xyzuv , hdr.hist.originator , 10 ) ;
     if( xyzuv[3] == 0 && xyzuv[4] == 0 ){
        xx = xyzuv[0] ; yy = xyzuv[1] ; zz = xyzuv[2] ;
        if( doswap ){ myswap_2(&xx); myswap_2(&yy); myswap_2(&zz); }
        if( xx > 0 && xx < hdr.dime.dim[1] &&
            yy > 0 && yy < hdr.dime.dim[2] &&
            zz > 0 && zz < hdr.dime.dim[3]   ) spmorg = 1 ;
     }
   }
   if( spmorg ) strcpy( MRILIB_orients , "LRPAIS" ) ;

   /* 27 Nov 2001: get a scale factor for images */

      fac = hdr.dime.funused1 ;
      vv = thd_floatscan( 1 , &fac ) ;
      if( fac < 0.0 || fac == 1.0 ) fac = 0.0 ;

   floatize = (fac != 0.0) ; /* 28 Nov 2001 */

   /* get data type into mrilib MRI_* form */

   switch( hdr.dime.datatype ){
      default:
         fprintf(stderr,"*** %s: Unknown ANALYZE datatype=%d (%s)\n",
                 hname,hdr.dime.datatype,ANDT_string(hdr.dime.datatype) ) ;
      return (NULL) ;

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
   if( nx < 2 || ny < 2 ) return (NULL) ;

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
      return (NULL) ;
   }

   fp = fopen( iname , "rb" ) ;
   if( fp == NULL ){
      fprintf(stderr,"*** Can't open ANALYZE file %s\n",iname) ;
      return (NULL) ;
   }

   ngood = datum_len*nx*ny*nz ;
   if( length < ngood ){
      fprintf( stderr,
        "*** ANALYZE file %s is %d bytes long but must be at least %d bytes long\n"
        "*** for nx=%d ny=%d nz=%d and voxel=%d bytes\n",
        iname,length,ngood,nx,ny,nz,datum_len ) ;
      fclose(fp) ; return (NULL) ;
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

   fclose(fp) ; return (newar) ;
}

/*----------------------------------------------------------------*/
/* 28 Aug 1996: return value changed from void to int,
                which will be the number of files written to disk
                (either 0 or 1 at the present time).             */

int mri_write( char *fname , MRI_IMAGE *im )
{
   FILE  *imfile ;
   void  *data ;
   int   dsize , noheader = FALSE, vv;


   /* bad inputs? */

   if( im == NULL || fname == NULL || *fname == '\0' ) return (0) ;

   /* special cases */

   if( im->kind == MRI_rgb  ){ return (mri_write_pnm( fname , im )) ; }
   if( im->kind == MRI_byte ){ return (mri_write_pnm( fname , im )) ; }

   /* open the file for output */

   if( strcmp(fname,"-") != 0 ){
     imfile = fopen( fname , "r" ) ;
     if( imfile != NULL ){
       fclose( imfile ) ;
       fprintf(stderr,"(FAILED) attempt to overwrite file %s\n",fname) ;
       return (0) ;
     }
   }

   if( strcmp(fname,"-") != 0 )
     imfile = fopen( fname , "w" ) ;
   else
     imfile = stdout ;   /* 18 Apr 2005: write to stdout instead */

   if( imfile == NULL ){
     fprintf( stderr , "couldn't open for output file %s\n" , fname ) ;
     return (0) ;
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
      vv = fwrite( qq , sizeof(short) , HEADER , imfile ) ;
      free(qq) ;
   }

   /*** write rest of data now ***/

   data = mri_data_pointer( im ) ;
   vv = fwrite( data , im->pixel_size , im->nx * im->ny , imfile ) ;

   if( imfile != stdout ) fclose( imfile ) ;
   return (1) ;
}


/**************************************************************************/

int mri_write_pnm( char *fname , MRI_IMAGE *im )
{
   FILE  *imfile ;
   int   vv;


   if( im == NULL || fname == NULL || *fname == '\0' ) return ( 0 );
   if( im->nz > 1 ) return ( 0 );
   if( im->kind != MRI_byte && im->kind != MRI_rgb   ) return ( 0 );

   if( strcmp(fname,"-") != 0 ){
     imfile = fopen( fname , "r" ) ;
     if( imfile != NULL ){
       fclose( imfile ) ;
       fprintf(stderr,"(FAILED) attempt to overwrite image file %s\n",fname) ;
       return ( 0 );
     }
   }

   if( strcmp(fname,"-") != 0 )
     imfile = fopen( fname , "w" ) ;
   else
     imfile = stdout ;     /* 18 Apr 2005: write to stdout */

   if( imfile == NULL ){
     fprintf(stderr,"Couldn't open image file %s for writing\n" , fname ) ;
     return ( 0 );
   }

   switch( im->kind ){

     case MRI_byte:
       fprintf( imfile , "P5\n%d %d\n255\n" , im->nx,im->ny ) ;     /* header */
       vv = fwrite( MRI_BYTE_PTR(im), sizeof(byte), im->nvox, imfile ) ; /* bytes */
     break ;

     case MRI_rgb:
       fprintf( imfile , "P6\n%d %d\n255\n" , im->nx,im->ny ) ;      /* header */
       vv = fwrite( MRI_RGB_PTR(im), sizeof(byte), 3*im->nvox, imfile ) ; /* bytes */
     break ;

     case MRI_short:
     case MRI_int:
     case MRI_float:
     case MRI_double:
     case MRI_complex:
     case MRI_rgba:
     break;

   }

   if( imfile != stdout ) fclose( imfile ) ;
   return ( 1 );
}


/* ----------------------------------------------------------------------
 * This is an interface for processing GEMS 4.x image files
 * (see ge4_header.h for structure contents).
 * ----------------------------------------------------------------------
 * int ge4_read_header( char * filename, ge4_header * H )
 *
 *     The basic point is to pass an empty ge4_header structure in,
 *     along with a file name.  This function fills the structure.
 *
 *     One exception: if H->image is non-null, this function will
 *                    read the image into that location.
 *
 *     - returns 0 on success
 *
 * R. Reynolds  2003 April 29
 * ----------------------------------------------------------------------
*/

/* ----------------------------------------------------------------------
 * history:
 *
 * June 12, 2003
 *   - fixed static warnings
 *
 * June 03, 2003
 *   - added doxygen style header (for ge4_read_header())
 *   - added get_image param to ge4_read_header()
 *   - swap bytes in image
 *   - added local static for swap_[24]()
 * ----------------------------------------------------------------------
*/

/* comes from either Imon.o or libmri.a */

/* local protos */
static int rcrswap_2      ( void * ptr );
static int rcrswap_4      ( void * ptr );
static int rcrswap_2_multi( void * ptr, int num_shorts );

/* ---------------------------------------------------------------------- */

/*!  Validate and read header data from a GEMS 4.x formatted file.

  \param filename is the name of the file to try to read
 
  \param H is the address of a ge4_header struct to be initialized and filled

  \param get_image specifies whether to allocate for and read in the image

  \return   0 : on success
          < 0 : on error

*/
int ge4_read_header( ge4_header * H, char * filename, int get_image )
{
    ge4_image_t  * ih;
    ge4_series_t * sh;
    ge4_study_t  * st;
    FILE         * fp;
    unsigned int   file_len;
    int            rres = 0;		/* read result */

    if ( filename == NULL || H == NULL )
    {
	fprintf( stderr, "** rg4h : bad params: %p, %p\n", filename, H );
	return -1;
    }

    file_len = THD_filesize( filename );

    /* file size must be fixed at 145408 bytes (142 KB) */
    if ( file_len != (GE4_HEADER_LENGTH + GE4_IMAGE_SIZE) ) return 1;

    /* clear structure */
    memset( H, 0, sizeof(ge4_header) );

    if ( (fp = fopen( filename, "r" )) == NULL )
    {
	fprintf( stderr, "ge4_read_header: failed to open '%s' for reading\n",
		 filename);
	return -1;
    }

    /* quickly scan and validate titles */

    sh = &H->ser_h;	/* set helper pointer */
    ih = &H->im_h;	/* set helper pointer */
    st = &H->std_h;	/* set helper pointer */

    fseek( fp, GE4_OFF_STDY_TITLE, SEEK_SET );
    rres |= (1 - fread( st->title, GE4_L_STDY_TITLE, 1, fp ));

    fseek( fp, GE4_OFF_SER_TITLE, SEEK_SET );
    rres |= (1 - fread( sh->title, GE4_L_SER_TITLE, 1, fp ));

    fseek( fp, GE4_OFF_IMG_TITLE, SEEK_SET );
    rres |= (1 - fread( ih->title, GE4_L_IM_TITLE, 1, fp ));

    /* if read failure or bad title fields, we're outta' here */
    if ( rres							  ||
         strncmp( st->title, GE4_STUDY_TITLE,  GE4_L_STDY_TITLE ) ||
         strncmp( sh->title, GE4_SERIES_TITLE, GE4_L_SER_TITLE  ) ||
         strncmp( ih->title, GE4_IMAGE_TITLE,  GE4_L_IM_TITLE   )
       )
	return 1;


    /* study header fields */

    fseek( fp, GE4_OFF_STDY_NUM, SEEK_SET );
    rres |= (1 - fread( st->num, GE4_L_STDY_NUM, 1, fp ));

    fseek( fp, GE4_OFF_STDY_DATE, SEEK_SET );
    rres |= (1 - fread( st->date, GE4_L_STDY_DATE, 1, fp ));

    fseek( fp, GE4_OFF_STDY_TIME, SEEK_SET );
    rres |= (1 - fread( st->time, GE4_L_STDY_TIME, 1, fp ));

    fseek( fp, GE4_OFF_STDY_PAT_NAME, SEEK_SET );
    rres |= (1 - fread( st->pat_name, GE4_L_STDY_PAT_NAME, 1, fp ));

    fseek( fp, GE4_OFF_STDY_PAT_ID, SEEK_SET );
    rres |= (1 - fread( st->pat_id, GE4_L_STDY_PAT_ID, 1, fp ));

    fseek( fp, GE4_OFF_STDY_AGE, SEEK_SET );
    rres |= (1 - fread( st->age, GE4_L_STDY_AGE, 1, fp ));

    fseek( fp, GE4_OFF_STDY_SEX, SEEK_SET );
    rres |= (1 - fread( &st->sex, 1, 1, fp ));

    /* series header fields */

    fseek( fp, GE4_OFF_SER_SERIES_NUM, SEEK_SET );
    rres |= (1 - fread( sh->series_num, GE4_L_SER_SER_NUM, 1, fp ));

    fseek( fp, GE4_OFF_SER_PLANE_TYPE, SEEK_SET );
    rres |= (1 - fread( &sh->plane_type, sizeof(sh->plane_type), 1, fp ));

    fseek( fp, GE4_OFF_SER_PLANE_DESC, SEEK_SET );
    rres |= (1 - fread( sh->plane_desc, GE4_L_SER_PL_DESC, 1, fp ));

    fseek( fp, GE4_OFF_SER_IM_MODE, SEEK_SET );
    rres |= (1 - fread( &sh->im_mode, sizeof(sh->im_mode), 1, fp ));

    fseek( fp, GE4_OFF_SER_PULSE_SEQ, SEEK_SET );
    rres |= (1 - fread( &sh->pulse_seq, sizeof(sh->pulse_seq), 1, fp ));

    fseek( fp, GE4_OFF_SER_FOV, SEEK_SET );
    rres |= (1 - fread( &sh->fov, sizeof(sh->fov), 1, fp ));

    fseek( fp, GE4_OFF_SER_CENTER, SEEK_SET );
    rres |= (1 - fread( sh->center, sizeof(sh->center), 1, fp ));

    fseek( fp, GE4_OFF_SER_ORIENT, SEEK_SET );
    rres |= (1 - fread( &sh->orient, sizeof(sh->orient), 1, fp ));

    fseek( fp, GE4_OFF_SER_SCAN_MAT_X, SEEK_SET );
    rres |= (1 - fread( &sh->scan_mat_x, sizeof(sh->scan_mat_x), 1, fp ));

    fseek( fp, GE4_OFF_SER_SCAN_MAT_Y, SEEK_SET );
    rres |= (1 - fread( &sh->scan_mat_y, sizeof(sh->scan_mat_y), 1, fp ));

    fseek( fp, GE4_OFF_SER_IM_MAT, SEEK_SET );
    rres |= (1 - fread( &sh->im_mat, sizeof(sh->im_mat), 1, fp ));


    /* image header fields */

    fseek( fp, GE4_OFF_IMG_IM_NUM, SEEK_SET );
    rres |= (1 - fread( ih->im_num, GE4_L_IM_NUM, 1, fp ));

    fseek( fp, GE4_OFF_IMG_IM_LOCN, SEEK_SET );
    rres |= (1 - fread( &ih->im_loc, sizeof(ih->im_loc), 1, fp ));

    fseek( fp, GE4_OFF_IMG_TABLE_POSN, SEEK_SET );
    rres |= (1 - fread( &ih->table_posn, sizeof(ih->table_posn), 1, fp ));

    fseek( fp, GE4_OFF_IMG_IM_THICK, SEEK_SET );
    rres |= (1 - fread( &ih->im_thickness, sizeof(ih->im_thickness), 1, fp ));

    fseek( fp, GE4_OFF_IMG_IM_SPACING, SEEK_SET );
    rres |= (1 - fread( &ih->im_spacing, sizeof(ih->im_spacing), 1, fp ));

    fseek( fp, GE4_OFF_IMG_TR, SEEK_SET );
    rres |= (1 - fread( &ih->tr, sizeof(ih->tr), 1, fp ));

    fseek( fp, GE4_OFF_IMG_TE, SEEK_SET );
    rres |= (1 - fread( &ih->te, sizeof(ih->te), 1, fp ));

    fseek( fp, GE4_OFF_IMG_TI, SEEK_SET );
    rres |= (1 - fread( &ih->ti, sizeof(ih->ti), 1, fp ));

    fseek( fp, GE4_OFF_IMG_NUM_ECHOS, SEEK_SET );
    rres |= (1 - fread( &ih->num_echoes, sizeof(ih->num_echoes), 1, fp ));

    fseek( fp, GE4_OFF_IMG_ECHO_NUM, SEEK_SET );
    rres |= (1 - fread( &ih->echo_num, sizeof(ih->echo_num), 1, fp ));

    fseek( fp, GE4_OFF_IMG_NEX_INT, SEEK_SET );
    rres |= (1 - fread( &ih->iNEX, sizeof(ih->iNEX), 1, fp ));

    fseek( fp, GE4_OFF_IMG_NEX_REAL, SEEK_SET );
    rres |= (1 - fread( &ih->fNEX, sizeof(ih->fNEX), 1, fp ));

    fseek( fp, GE4_OFF_IMG_FLIP_ANGLE, SEEK_SET );
    rres |= (1 - fread( &ih->flip_angle, sizeof(ih->flip_angle), 1, fp ));

    if ( rres )
    {
	fprintf( stderr, "** failed to read ge4 header for '%s'\n", filename );
	return -1;
    }

    if ( ge4_validate_header( H ) )
	return 1;

    if ( get_image )
    {
	if ( (H->image = (short *)malloc( GE4_IMAGE_SIZE )) == NULL )
	{
	    fprintf( stderr, "** failed to allocate %d bytes for image\n",
		     GE4_IMAGE_SIZE );
	    return -1;
	}

	fseek( fp, GE4_HEADER_LENGTH, SEEK_SET );
	rres = fread( H->image, GE4_IMAGE_SIZE, 1, fp );

	if ( rres != 1 )
	{
	    fprintf( stderr, "** failed to read ge4 image for file '%s'\n",
		     filename );
	    free( H->image );
	    return -1;
	}

	H->im_bytes = GE4_IMAGE_SIZE;          /* note it for "outsiders" */

	if ( H->swap )
	    rcrswap_2_multi( H->image, GE4_IMAGE_SIZE/2 );
    }

    return 0;
}


/*------------------------------------------------------------
 *  Check for valid data in the header.
 *
 *  If values are out of range, try byte swapping.
 *
 *  series header:
 *	plane_type	: in {0..4}
 *	image_mode	: in {0..4}
 *	pulse_seq	: in {0..25}
 *
 *  return    0 : valid
 *         else : invalid
 *------------------------------------------------------------
*/
int ge4_validate_header( ge4_header * h )
{
    ge4_series_t * s;
    ge4_image_t  * im;

    if ( h == NULL )
	return -1;

    s  = &h->ser_h;
    im = &h->im_h;

    /* note that titles have already been validated */

    if ( (s->plane_type < 0) || (s->plane_type > 4) ||
	 (s->im_mode    < 0) || (s->im_mode    > 4) ||
         (s->pulse_seq  < 0) || (s->pulse_seq  > 25) )
    {
	ge4_swap_all_bytes( h );
    }

    /* if these are still off, we are hosed... */
    if ( (s->plane_type < 0) || (s->plane_type > 4) ||
	 (s->im_mode    < 0) || (s->im_mode    > 4) ||
         (s->pulse_seq  < 0) || (s->pulse_seq  > 25) )
    {
	return -1;
    }

    return 0;
}


/*------------------------------------------------------------
 *  Swap all numeric fields in ge4_header sub-structs.
 *------------------------------------------------------------
*/
int ge4_swap_all_bytes( ge4_header * h )
{
    if ( h == NULL )
    {
	fprintf( stderr, "** ge4_SAB : no header!\n" );
	return -1;
    }

    h->swap = 1;		/* note that we have swapped */

    /* series header */

    rcrswap_2( &h->ser_h.plane_type );
    rcrswap_2( &h->ser_h.im_mode );
    rcrswap_2( &h->ser_h.pulse_seq );

    rcrswap_4( &h->ser_h.fov );
    rcrswap_4( &h->ser_h.center[0] );
    rcrswap_4( &h->ser_h.center[1] );
    rcrswap_4( &h->ser_h.center[2] );

    rcrswap_2( &h->ser_h.orient );
    rcrswap_2( &h->ser_h.scan_mat_x );
    rcrswap_2( &h->ser_h.scan_mat_y );
    rcrswap_2( &h->ser_h.im_mat );

    /* image header */

    rcrswap_4( &h->im_h.im_loc );
    rcrswap_4( &h->im_h.table_posn );
    rcrswap_4( &h->im_h.im_thickness );
    rcrswap_4( &h->im_h.im_spacing );

    rcrswap_4( &h->im_h.tr );
    rcrswap_4( &h->im_h.te );
    rcrswap_4( &h->im_h.ti );

    rcrswap_2( &h->im_h.num_echoes );
    rcrswap_2( &h->im_h.echo_num );

    rcrswap_2( &h->im_h.iNEX );
    rcrswap_4( &h->im_h.fNEX );

    rcrswap_2( &h->im_h.flip_angle );

    return 0;
}

/*------------------------------------------------------------
 *  Swap multiple byte pairs.
 *------------------------------------------------------------
*/
static int rcrswap_2_multi( void * ptr, int num_shorts )
{
    unsigned char * cp0, * cp1;
    unsigned char   tmpc;
    int             index;

    if ( ptr == NULL ) return -1;

    cp0 = (unsigned char *)ptr;
    cp1 = cp0 + 1;

    for ( index = 0; index < num_shorts; index++ )
    {
	tmpc = *cp0;
	*cp0 = *cp1;
	*cp1 = tmpc;

	cp0 += 2;
	cp1 += 2;
    }

    return 0;
}


/*------------------------------------------------------------
 * Reverse the order of the 4 bytes at this address.
 *------------------------------------------------------------
*/
static int rcrswap_4( void * ptr )		/* destructive */
{
   unsigned char * addr = ptr;

   addr[0] ^= addr[3]; addr[3] ^= addr[0]; addr[0] ^= addr[3];
   addr[1] ^= addr[2]; addr[2] ^= addr[1]; addr[1] ^= addr[2];

   return 0;
}

/*------------------------------------------------------------
 * Reverse the order of the 2 bytes at this address.
 *------------------------------------------------------------
*/
static int rcrswap_2( void * ptr )		/* destructive */
{
   unsigned char * addr = ptr;

   addr[0] ^= addr[1]; addr[1] ^= addr[0]; addr[0] ^= addr[1];

   return 0;
}

double mri_max( MRI_IMAGE *im )
{
   register int ii , npix ;
   byte   byte_max   = 0 ;
   short  short_max  = -32767 ;       /* 23 Oct 1998: changed from 0 */
   int    int_max    = -2147483647 ;  /* ditto */
   float  float_max  = -1.e+38 ;      /* changed from -9999999.0 */
   double double_max = -1.e+38 ;      /* ditto */

   npix = im->nvox ;

   switch( im->kind ){

      case MRI_byte:{
         byte *qar = MRI_BYTE_PTR(im) ;
         for( ii=0 ; ii < npix ; ii++ )
            byte_max = MAX( byte_max , qar[ii] ) ;
         return (double) byte_max ;
      }

      case MRI_short:{
         short *qar = MRI_SHORT_PTR(im) ;
         for( ii=0 ; ii < npix ; ii++ )
            short_max = MAX( short_max , qar[ii] ) ;
         return (double) short_max ;
      }

      case MRI_int:{
         int *qar = MRI_INT_PTR(im) ;
         for( ii=0 ; ii < npix ; ii++ )
            int_max = MAX( int_max , qar[ii] ) ;
         return (double) int_max ;
      }

      case MRI_float:{
         float *qar = MRI_FLOAT_PTR(im) ;
         for( ii=0 ; ii < npix ; ii++ )
            float_max = MAX( float_max , qar[ii] ) ;
         return (double) float_max ;
      }

      case MRI_double:{
         double *qar = MRI_DOUBLE_PTR(im) ;
         for( ii=0 ; ii < npix ; ii++ )
            double_max = MAX( double_max , qar[ii] ) ;
         return double_max ;
      }

      case MRI_complex:{
         complex *qar = MRI_COMPLEX_PTR(im) ;
         for( ii=0 ; ii < npix ; ii++ )
            float_max = MAX( float_max , CSQR(qar[ii]) ) ;
         return sqrt(float_max) ;
      }

      case MRI_rgb:{
         byte *rgb = MRI_RGB_PTR(im) ;
         double val , top=0.0 ;
         for( ii=0 ; ii < npix ; ii++ ){  /* scale to brightness */
            val =  0.299 * rgb[3*ii]      /* between 0 and 255     */
                 + 0.587 * rgb[3*ii+1]
                 + 0.114 * rgb[3*ii+2] ;
            if( val > top ) top = val ;
         }
         return top ;
      }

      default:
        fprintf( stderr , "mri_max:  unknown image kind\n" ) ;
   }
   return 0 ;
}

double mri_maxabs( MRI_IMAGE * im )
{
   register int ii , npix ;
   byte   byte_max   = 0 ;
   int    int_max    = 0 ;
   double double_max = 0.0 ;

   npix = im->nvox ;

   switch( im->kind ){

      case MRI_byte:{
         byte *qar = MRI_BYTE_PTR(im) ;
         for( ii=0 ; ii < npix ; ii++ )
            byte_max = MAX( byte_max , qar[ii] ) ;
         return (double) byte_max ;
      }

      case MRI_short:{
         short *qar = MRI_SHORT_PTR(im) ;
         for( ii=0 ; ii < npix ; ii++ )
            int_max = MAX( int_max , abs(qar[ii]) ) ;
         return (double) int_max ;
      }

      case MRI_int:{
         int *qar = MRI_INT_PTR(im) ;
         for( ii=0 ; ii < npix ; ii++ )
            int_max = MAX( int_max , abs(qar[ii]) ) ;
         return (double) int_max ;
      }

      case MRI_float:{
         float *qar = MRI_FLOAT_PTR(im) ;
         for( ii=0 ; ii < npix ; ii++ )
            double_max = MAX( double_max , fabs(qar[ii]) ) ;
         return double_max ;
      }

      case MRI_double:{
         double *qar = MRI_DOUBLE_PTR(im) ;
         for( ii=0 ; ii < npix ; ii++ )
            double_max = MAX( double_max , fabs(qar[ii]) ) ;
         return double_max ;
      }

      case MRI_complex:{
         complex *qar = MRI_COMPLEX_PTR(im) ;
         for( ii=0 ; ii < npix ; ii++ )
            double_max = MAX( double_max , CSQR(qar[ii]) ) ;
         return sqrt(double_max) ;
      }

      case MRI_rgb:
         return mri_max( im ) ;

      default:
         fprintf( stderr , "mri_max:  unknown image kind\n" ) ;
   }
   return 0 ;
}

/** only works on short and byte images **/

MRI_IMAGE * mri_nsize( MRI_IMAGE * imin )
{
   MRI_IMAGE * imout = NULL ;
   int nx , ny , ntop , nxpad , nypad , ix,jy,ioff , ii;

   if( imin == NULL ){
      fprintf(stderr,"\n*** mri_nsize: NULL image passed as input!\n") ;
      return NULL ;
   }

   if( ! MRI_IS_2D(imin) ){
      fprintf(stderr,"\n*** mri_nsize only works on 2D images!\n") ;
      EXIT(1) ;
   }

   nx   = imin->nx ;  ny = imin->ny ;
   ntop = MAX(nx,ny) ;

        if( ntop <=  32 ) ntop =  32 ;  /* next power of 2 */
   else if( ntop <=  64 ) ntop =  64 ;
   else if( ntop <= 128 ) ntop = 128 ;
   else if( ntop <= 256 ) ntop = 256 ;
   else if( ntop <= 512 ) ntop = 512 ;
   else if( ntop <=1024 ) ntop =1024 ;
   else {
      fprintf(stderr,"\n*** mri_nsize: cannot scale up %d x %d images!\n",nx,ny) ;
      return NULL ;
   }

   switch( imin->kind ){

      case MRI_short:{
         short * ptin , * ptout ;
         imout = mri_new( ntop,ntop , MRI_short ) ;
         ptin  = mri_data_pointer( imin ) ;
         ptout = mri_data_pointer( imout ) ;

         for( ii=0 ; ii < ntop*ntop ; ii++ ) ptout[ii] = 0 ;

         nxpad = (ntop-nx) / 2 ;
         nypad = (ntop-ny) / 2 ;

         for( jy=0 ; jy < ny ; jy++ ){
            ioff = (jy+nypad) * ntop + nxpad ;
            for( ix=0 ; ix < nx ; ix++ )
               ptout[ix+ioff] = ptin[ix+jy*nx] ;
         }
      }
      break ;

      case MRI_byte:{
         byte * ptin , * ptout ;
         imout = mri_new( ntop,ntop , MRI_byte ) ;
         ptin  = mri_data_pointer( imin ) ;
         ptout = mri_data_pointer( imout ) ;

         for( ii=0 ; ii < ntop*ntop ; ii++ ) ptout[ii] = 0 ;

         nxpad = (ntop-nx) / 2 ;
         nypad = (ntop-ny) / 2 ;

         for( jy=0 ; jy < ny ; jy++ ){
            ioff = (jy+nypad) * ntop + nxpad ;
            for( ix=0 ; ix < nx ; ix++ )
               ptout[ix+ioff] = ptin[ix+jy*nx] ;
         }
      }
      break ;

      case MRI_int:{
         int * ptin , * ptout ;
         imout = mri_new( ntop,ntop , MRI_int ) ;
         ptin  = mri_data_pointer( imin ) ;
         ptout = mri_data_pointer( imout ) ;

         for( ii=0 ; ii < ntop*ntop ; ii++ ) ptout[ii] = 0 ;

         nxpad = (ntop-nx) / 2 ;
         nypad = (ntop-ny) / 2 ;

         for( jy=0 ; jy < ny ; jy++ ){
            ioff = (jy+nypad) * ntop + nxpad ;
            for( ix=0 ; ix < nx ; ix++ )
               ptout[ix+ioff] = ptin[ix+jy*nx] ;
         }
      }
      break ;

      case MRI_float:{
         float * ptin , * ptout ;
         imout = mri_new( ntop,ntop , MRI_float ) ;
         ptin  = mri_data_pointer( imin ) ;
         ptout = mri_data_pointer( imout ) ;

         for( ii=0 ; ii < ntop*ntop ; ii++ ) ptout[ii] = 0 ;

         nxpad = (ntop-nx) / 2 ;
         nypad = (ntop-ny) / 2 ;

         for( jy=0 ; jy < ny ; jy++ ){
            ioff = (jy+nypad) * ntop + nxpad ;
            for( ix=0 ; ix < nx ; ix++ )
               ptout[ix+ioff] = ptin[ix+jy*nx] ;
         }
      }
      break ;

      case MRI_double:{
         double * ptin , * ptout ;
         imout = mri_new( ntop,ntop , MRI_double ) ;
         ptin  = mri_data_pointer( imin ) ;
         ptout = mri_data_pointer( imout ) ;

         for( ii=0 ; ii < ntop*ntop ; ii++ ) ptout[ii] = 0 ;

         nxpad = (ntop-nx) / 2 ;
         nypad = (ntop-ny) / 2 ;

         for( jy=0 ; jy < ny ; jy++ ){
            ioff = (jy+nypad) * ntop + nxpad ;
            for( ix=0 ; ix < nx ; ix++ )
               ptout[ix+ioff] = ptin[ix+jy*nx] ;
         }
      }
      break ;

      case MRI_complex:{
         complex * ptin , * ptout ;
         imout = mri_new( ntop,ntop , MRI_complex ) ;
         ptin  = mri_data_pointer( imin ) ;
         ptout = mri_data_pointer( imout ) ;

         for( ii=0 ; ii < ntop*ntop ; ii++ ) ptout[ii].r = ptout[ii].i = 0 ;

         nxpad = (ntop-nx) / 2 ;
         nypad = (ntop-ny) / 2 ;

         for( jy=0 ; jy < ny ; jy++ ){
            ioff = (jy+nypad) * ntop + nxpad ;
            for( ix=0 ; ix < nx ; ix++ )
               ptout[ix+ioff] = ptin[ix+jy*nx] ;
         }
      }
      break ;

      case MRI_rgb:
      case MRI_rgba:
      break ;
   }

   MRI_COPY_AUX(imout,imin) ;
   return imout ;
}

/*-------------------------------------------------------------------
  Scale an image in place - 27 Nov 2001
---------------------------------------------------------------------*/

void mri_scale_inplace( float fac , MRI_IMAGE *im )
{
   register int ii , nvox ;
   void *vp ;


   if( im == NULL || fac == 1.0 || fac == 0.0 ) return ;
   vp = mri_data_pointer( im ) ; if( vp == NULL ) return ;
   nvox = im->nvox ;

   switch( im->kind ){

      case MRI_byte:{
         byte *pp = (byte *) vp ;
         for( ii=0 ; ii < nvox ; ii++ ) pp[ii] *= fac ;
      }
      break ;

      case MRI_short:{
         short *pp = (short *) vp ;
         for( ii=0 ; ii < nvox ; ii++ ) pp[ii] *= fac ;
      }
      break ;

      case MRI_float:{
         float *pp = (float *) vp ;
         for( ii=0 ; ii < nvox ; ii++ ) pp[ii] *= fac ;
      }
      break ;

      case MRI_int:{
         int *pp = (int *) vp ;
         for( ii=0 ; ii < nvox ; ii++ ) pp[ii] *= fac ;
      }
      break ;

      case MRI_double:{
         double *pp = (double *) vp ;
         for( ii=0 ; ii < nvox ; ii++ ) pp[ii] *= fac ;
      }
      break ;

      case MRI_complex:{
         complex *pp = (complex  *) vp ;
         for( ii=0 ; ii < nvox ; ii++ ){
           pp[ii].r *= fac; pp[ii].i *= fac;
         }
      }
      break ;

      case MRI_rgb:{
         byte *pp = (byte *) vp ;
         nvox *= 3 ;
         for( ii=0 ; ii < nvox ; ii++ ) pp[ii] *= fac ;
      }
      break ;

      case MRI_rgba:
      break;
   }

   return ;
}
#define SWAB16(x) ( ( ((x)&0x00ffU)<<8 ) | ( ((x)&0xff00U)>>8 ) )

void mri_swapbytes( MRI_IMAGE *im )
{
   register int ii , npix ;
   register short *iar ;


   if( im == NULL || im->kind != MRI_short ){
     fprintf( stderr , "mri_swapbytes called with non-short image kind\n" ) ;
     return ;
   }

   npix = im->nvox ; iar = MRI_SHORT_PTR(im) ;

   for( ii=0 ; ii < npix ; ii++ ) iar[ii] = SWAB16( iar[ii] ) ;

   return ;
}

/*---------------------------------------------------------------------
   Routines to swap byte arrays in pairs and tetrads -- 14 Sep 1998
-----------------------------------------------------------------------*/

typedef struct { unsigned char a,b ; } twobytes ;

void swap_twobytes( int n , void * ar )
{
   register int ii ;
   register twobytes * tb = (twobytes *) ar ;
   register unsigned char tt ;

   for( ii=0 ; ii < n ; ii++ ){
      tt       = tb[ii].a ;
      tb[ii].a = tb[ii].b ;
      tb[ii].b = tt ;
   }
}

typedef struct { unsigned char a,b,c,d ; } fourbytes ;

void swap_fourbytes( int n , void * ar )
{
   register int ii ;
   register fourbytes * tb = (fourbytes *) ar ;
   register unsigned char tt ;

   for( ii=0 ; ii < n ; ii++ ){
      tt       = tb[ii].a ;
      tb[ii].a = tb[ii].d ;
      tb[ii].d = tt ;
      tt       = tb[ii].b ;
      tb[ii].b = tb[ii].c ;
      tb[ii].c = tt ;
   }
}

typedef struct { unsigned char a,b,c,d , D,C,B,A ; } eightbytes ;

void swap_eightbytes( int n , void * ar )
{
   register int ii ;
   register eightbytes * tb = (eightbytes *) ar ;
   register unsigned char tt ;

   for( ii=0 ; ii < n ; ii++ ){
      tt = tb[ii].a ; tb[ii].a = tb[ii].A ; tb[ii].A = tt ;
      tt = tb[ii].b ; tb[ii].b = tb[ii].B ; tb[ii].B = tt ;
      tt = tb[ii].c ; tb[ii].c = tb[ii].C ; tb[ii].C = tt ;
      tt = tb[ii].d ; tb[ii].d = tb[ii].D ; tb[ii].D = tt ;
   }
}

#define IS_GOOD_FLOAT(x) isfinite(x)

/*---------------------------------------------------------------------
   Scan an array of floats for illegal values, replacing them with 0.
   Return the number of illegal values found.
-----------------------------------------------------------------------*/

int thd_floatscan( int nbuf , float *fbuf )
{
   int ii , nerr ;

   if( nbuf <= 0 || fbuf == NULL ) return 0 ;

   for( nerr=ii=0 ; ii < nbuf ; ii++ )
     if( !IS_GOOD_FLOAT(fbuf[ii]) ){ fbuf[ii] = 0.0f ; nerr++ ; }

   return nerr ;
}
