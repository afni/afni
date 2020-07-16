/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/

#ifndef _MCW_MRILIB_HEADER_
#define _MCW_MRILIB_HEADER_

#define MRILIB_7D
#define _GNU_SOURCE 1           /* 23 Jun 2011 */

#define COXEMAIL "rwcox@nih.gov"        /* or /dev/null, if you prefer */

#ifdef  __cplusplus
extern "C" {                    /* care of Greg Balls    7 Aug 2006 [rickr] */
#endif

/*------------------------------------------------------------------*/

#undef INLINE

#ifdef __GNUC__
# define INLINE __inline__
#endif

#ifndef INLINE
# define INLINE /*nada*/
#endif

#undef RESTRICT
#ifdef __GNUC__
# define RESTRICT __restrict__
#else
# define RESTRICT /*nada*/
#endif

/*------------------------------------------------------------------*/

extern int MRILIB_verb ;                /* 01 May 2009 */

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

extern int MRILIB_DomainMaxNodeIndex ;         /* 32 Dec 2007 */

extern int   assume_dicom_mosaic ;   /* mri_read_dicom.c  13 Mar 2006 [rickr] */
extern int   use_new_mosaic_code;    /* mri_process_siemens.c 23 Dec 2010 [r] */

/* siemens slice timing info from mri_read.c         13 Apr 2011 [rickr] */
extern int     g_siemens_timing_nused;  /* number of times used          */
extern float * g_siemens_timing_times;  /* actual list of times          */
extern int     g_siemens_timing_units;  /* time units, UNITS_MSEC_TYPE?  */
extern int     populate_g_siemens_times(int tunits);
extern int     get_and_display_siemens_times(void);
extern int     valid_g_siemens_times(int, float, int, int);

/*----------------------------------------------------------------------------*/

#ifdef  __cplusplus
}
#endif

#include "nifti2_io.h"
extern int use_MRILIB_dicom_matrix ;    /* 26 Jan 2006 */
extern mat44   MRILIB_dicom_matrix ;

#include "mri_dicom_stuff.h"
extern int                MRILIB_dicom_count ;  /* 15 Mar 2006 */
extern int                MRILIB_dicom_s16_overflow ;  /* 9 Jul 2013 [rickr] */
extern AFD_dicom_header **MRILIB_dicom_header ;

/*! Clear the MRILIB globals
    (which transmit info from image files to to3d.c). */

#define CLEAR_MRILIB_globals                              \
 do{ MRILIB_orients[0]='\0';                              \
     MRILIB_zoff=MRILIB_xoff=MRILIB_yoff=MRILIB_tr=0.0;   \
     use_MRILIB_xoff=use_MRILIB_yoff=use_MRILIB_zoff=0;   \
     use_MRILIB_xcos=use_MRILIB_ycos=use_MRILIB_zcos=0;   \
     use_MRILIB_slicespacing=0;                           \
     use_MRILIB_dicom_matrix=0;                           \
     MRILIB_dicom_count=0; MRILIB_dicom_header=NULL;      \
 } while(0)

#include <stdio.h>
#include <stdlib.h>
#include <math.h>

#include "mcw_malloc.h"  /* 06 Mar 1999 addition */
#include "debugtrace.h"  /* 26 Jan 2001 addition */
#include "Amalloc.h"     /* 09 Dec 2003 addition */
#include "Aomp.h"

/* preferentially include f2c header from local directory, otherwise use system
 header */
#include "f2c.h"
/* The following was added to harmonize with system f2c header. Subsequent
typedef for complex is now ignored */
#define TYPEDEF_complex


/*----------------------------------------------------------------------------*/

#ifndef PI
#  define PI  3.1415926535897932
#endif
#ifndef HPI
#  define HPI 1.5707963267948966
#endif

#ifndef WAY_BIG
/*! A big number (anything over this is infinity). */
#  define WAY_BIG 1.e+10
#endif

#ifndef TINY_NUMBER
/*! A tiny, infinitessimal number */
#  define TINY_NUMBER 1.e-10
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

#ifndef TYPEDEF_sbyte
#define TYPEDEF_sbyte
typedef signed char sbyte ;
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
         MRI_byte , MRI_short  , MRI_int  ,
        MRI_float , MRI_double , MRI_complex , MRI_rgb , MRI_rgba ,
        MRI_fvect
 } MRI_TYPE ;

#define MRI_KIND MRI_TYPE ;   /* to alleviate stupidity */
#define MRI_type MRI_TYPE ;
#define MRI_kind MRI_TYPE ;

#define MRI_rgbyte MRI_rgb

/*! The last MRI_TYPE yet defined. */

#define LAST_MRI_TYPE 7

/*! String names for MRI_TYPE. */

static char * MRI_TYPE_name[9] =
  { "byte"  , "short", "int", "float", "double", "complex", "rgb", "RGBA" ,
    "fvect"
  } ;

#define IS_REAL_TYPE(zkq) ((zkq)==MRI_byte || (zkq)==MRI_short || (zkq)==MRI_float)
#define IS_RGB_TYPE(zkq)  ((zkq)==MRI_rgb  || (zkq)==MRI_rgba)

#define IS_REAL_IMAGE(iq) IS_REAL_TYPE((iq)->kind)
#define IS_RGB_IMAGE(iq)  IS_RGB_TYPE((iq)->kind)

#define MRI_type_name MRI_TYPE_name  /* because I forget */

#define MRI_type_string(iq) \
  ( ((iq) < 0 || (iq) > LAST_MRI_TYPE ) ? "unknown" : MRI_TYPE_name[iq] )

#define MRI_TYPE_NAME(iimm) MRI_TYPE_name[(iimm)->kind]  /* 26 Apr 2005 */

/*! Max value of a byte. */

#define MRI_maxbyte         255

/*! Max value of a short. */

#define MRI_maxshort      32767

/*! Max value of an int. */

#define MRI_maxint   2147483647

/*! Max values for various types, if they have them. */

static float MRI_TYPE_maxval[9] =
  { 255.0f, 32767.0f, 2147483647.0f, 0.0f,0.0f,0.0f, 255.0f, 255.0f, 0.0f } ;

/*! Force a float into a short. */

#define SHORTIZE(xx) (  ((xx) < -32767.0f) ? (short)-32767                    \
                      : ((xx) >  32767.0f) ? (short) 32767 : (short)rint(xx) )

/*! Force a float into a byte. */

#define BYTEIZE(xx)  (  ((xx) <   0.0 ) ? (byte)0                             \
                      : ((xx) > 255.0f) ? (byte)255 : (byte)rintf(xx) )

/*! Determine if a MRI_TYPE is an integer type. */

#define MRI_IS_INT_TYPE(typ) ((typ) < 3)

#define MRI_IS_FLOAT_TYPE(typ) ((typ) >=3 && (typ) <= 5)

/*! I suppose that the next C makes this pleonastic. */

#if defined(_SUNPERF_COMPLEX) || defined(DONT_DECLARE_COMPLEX)
# define TYPEDEF_complex
#endif

#ifndef TYPEDEF_complex
#define TYPEDEF_complex
#ifndef complex
typedef struct complex { float r , i ; } complex ;
#endif
#endif

#ifndef TYPEDEF_float_pair
#define TYPEDEF_float_pair
typedef struct { float a,b ; } float_pair ;
#endif

#ifndef TYPEDEF_float_triple
#define TYPEDEF_float_triple
typedef struct { float a,b,c ; } float_triple ;
#define float_trip float_triple
#define ASSIGN_FLOAT_TRIPLE(ft,x,y,z) ( ft.a=(x),ft.b=(y),ft.c=(z) )
#endif

#ifndef TYPEDEF_float_quad
#define TYPEDEF_float_quad
typedef struct { float a,b,c,d ; } float_quad ;
#endif

#ifndef TYPEDEF_float_quint
#define TYPEDEF_float_quint
typedef struct { float a,b,c,d,e ; } float_quint ;  /* 02 Nov 2015 */
#endif

#ifndef TYPEDEF_double_pair
#define TYPEDEF_double_pair
typedef struct { double a,b ; } double_pair ;
#endif

#ifndef TYPEDEF_double_triple
#define TYPEDEF_double_triple
typedef struct { double a,b,c ; } double_triple ;
#endif

#ifndef TYPEDEF_double_quad
#define TYPEDEF_double_quad
typedef struct { double a,b,c,d ; } double_quad ;
#endif

#ifndef TYPEDEF_double_quint
#define TYPEDEF_double_quint
typedef struct { double a,b,c,d,e ; } double_quint ;
#endif

/*-------*/

/*! Triple to hold RGB bytes. */

#ifndef TYPEDEF_rgbyte
#define TYPEDEF_rgbyte
typedef struct rgbyte { byte r,g,b ; } rgbyte ;  /* 15 Feb 1999 */

#undef  RGBZEQ
#undef  RGBZAS
#define RGBZEQ(q) ( (q).r==0 && (q).g==0 && (q).b==0 )  /* is == (0,0,0)? */
#define RGBZAS(q) ( (q).r = (q).g = (q).b = 0 )         /* set = (0,0,0). */
#endif

static rgbyte tEMp_rgbyte_aAa ;

/*! Convert one RBG triple (rgbyte) to a single int. */

#define RGBYTE_TO_INT(rgb) ( (rgb).r << 16 | (rgb).g << 8 | (rgb).b )

/*! Convert one int to a RGB triple (rgbyte). */

#define INT_TO_RGB(q) ( tEMp_rgbyte_aAa.r = ((q) >> 16) & 0xff , \
                        tEMp_rgbyte_aAa.g = ((q) >>  8) & 0xff , \
                        tEMp_rgbyte_aAa.b = (q)         & 0xff , tEMp_rgbyte_aAa )

#define RGB_TO_FLOAT(rgb) (0.299*(rgb).r+0.587*(rgb).g+0.114*(rgb).b)
/*-------*/

/** Mar 1996: Extended to images up to 7D;
              Not all routines work with images > 2D --
              check top of file for "7D SAFE" comments **/

#undef USE_MRI_LABELS
#ifdef USE_MRI_LABELS
#  define MRI_LABEL_SIZE 4
#endif

#define INPUT_DELAY  1
#define BSWAP_DELAY  2
#define IS_PURGED    4

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
          int64_t nxyz ;      /*!< nx*ny*nz */
          int64_t nxyzt  ;    /*!< nx*ny*nz*nt */
          int64_t nvox   ;    /*!< number of voxels total */
          int pixel_size ;    /*!< bytes per pixel */

          MRI_TYPE kind ;     /*!< one of the MRI_TYPE codes above */
          void    *im ;       /*!< pointer to actual pixel data */
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

#ifdef USE_MRI_LABELS
         char xlab[MRI_LABEL_SIZE] ;  /*!< labels for each dimension */
              ylab[MRI_LABEL_SIZE] ;  /*!< labels for each dimension */
              zlab[MRI_LABEL_SIZE] ;  /*!< labels for each dimension */
              tlab[MRI_LABEL_SIZE] ;  /*!< labels for each dimension */
              ulab[MRI_LABEL_SIZE] ;  /*!< labels for each dimension */
              vlab[MRI_LABEL_SIZE] ;  /*!< labels for each dimension */
              wlab[MRI_LABEL_SIZE] ;  /*!< labels for each dimension */
#endif

         char *fname ;   /*!< to read actual image data after delay */
         unsigned int foffset ;   /*!< offset into fname of image data */
         int fondisk ;   /*!< flag to indicate if is on disk (?) */

         int was_swapped ; /* 07 Mar 2002 */
         int vdim ;        /* 28 Nov 2008 */
         int flags ;       /* 21 Mar 2013 */

         char *comments ;  /* 03 Aug 2016 */
} MRI_IMAGE ;

#ifdef USE_MRI_LABELS
/*! Copy auxiliary data from one MRI_IMAGE to another. */
#  define MRI_COPY_AUX_OLD(nn,oo)                                       \
    ( (nn)->dx = (oo)->dx , (nn)->dy = (oo)->dy , (nn)->dz = (oo)->dz , \
      (nn)->dt = (oo)->dt , (nn)->du = (oo)->du , (nn)->dv = (oo)->dv , \
      (nn)->dw = (oo)->dw ,                                             \
      (nn)->xo = (oo)->xo , (nn)->yo = (oo)->yo , (nn)->zo = (oo)->zo , \
      (nn)->to = (oo)->to , (nn)->uo = (oo)->uo , (nn)->vo = (oo)->vo , \
      (nn)->wo = (oo)->wo ,                                             \
      strcpy((nn)->xlab,(oo)->xlab) , strcpy((nn)->ylab,(oo)->ylab) ,   \
      strcpy((nn)->zlab,(oo)->zlab) , strcpy((nn)->tlab,(oo)->tlab) ,   \
      strcpy((nn)->ulab,(oo)->ulab) , strcpy((nn)->vlab,(oo)->vlab) ,   \
      strcpy((nn)->wlab,(oo)->wlab) ,                                   \
      mri_add_name( (oo)->name , (nn) ) )
#else
#  define MRI_COPY_AUX_OLD(nn,oo)                                       \
    ( (nn)->dx = (oo)->dx , (nn)->dy = (oo)->dy , (nn)->dz = (oo)->dz , \
      (nn)->dt = (oo)->dt , (nn)->du = (oo)->du , (nn)->dv = (oo)->dv , \
      (nn)->dw = (oo)->dw ,                                             \
      (nn)->xo = (oo)->xo , (nn)->yo = (oo)->yo , (nn)->zo = (oo)->zo , \
      (nn)->to = (oo)->to , (nn)->uo = (oo)->uo , (nn)->vo = (oo)->vo , \
      (nn)->wo = (oo)->wo ,                                             \
      mri_add_name( (oo)->name , (nn) ) )
#endif

#define MRI_COPY_AUX(nn,oo)                                                  \
  do{ MRI_COPY_AUX_OLD(nn,oo) ;                                              \
      if( (oo)->comments != NULL ) (nn)->comments = strdup((oo)->comments) ; \
      else                         (nn)->comments = NULL ;                   \
  } while(0)

/*! Check if MRI_IMAGE is 1D (ny=1) */
#define MRI_IS_1D(iq)  ((iq)->ny == 1)

/*! Check if MRI_IMAGE is 2D (nz=1) */
#define MRI_IS_2D(iq)  ((iq)->ny > 1 && (iq)->nz == 1)

/*! Check if MRI_IMAGE is 3D (nt=1) */
#define MRI_IS_3D(iq)  ((iq)->nz > 1 && (iq)->nt == 1)

/*! Check if MRI_IMAGE is 4D (nu=1) */
#define MRI_IS_4D(iq)  ((iq)->nt > 1 && (iq)->nu == 1)

/*! Return dimensionality of MRI_IMAGE */

#if 1
extern int mri_dimensionality(MRI_IMAGE *) ;     /* 12 Dec 2007 */
#define MRI_DIMENSIONALITY  mri_dimensionality
#else
#define MRI_DIMENSIONALITY(iq)                     \
 ( ((iq)->ny == 1) ? 1 : ((iq)->nz == 1) ? 2 :     \
   ((iq)->nt == 1) ? 3 : ((iq)->nu == 1) ? 4 :     \
   ((iq)->nv == 1) ? 5 : ((iq)->nw == 1) ? 6 : 7 )
#endif

#define MRI_BYTE_PTR(iq)    ((byte *)mri_data_pointer(iq))
#define MRI_SHORT_PTR(iq)   ((short *)mri_data_pointer(iq))
#define MRI_INT_PTR(iq)     ((int *)mri_data_pointer(iq))
#define MRI_FLOAT_PTR(iq)   ((float *)mri_data_pointer(iq))
#define MRI_DOUBLE_PTR(iq)  ((double *)mri_data_pointer(iq))
#define MRI_COMPLEX_PTR(iq) ((complex *)mri_data_pointer(iq))
#define MRI_RGB_PTR(iq)     ((byte *)mri_data_pointer(iq))
#define MRI_RGBA_PTR(iq)    ((rgba *)mri_data_pointer(iq))

/* only used in afni.c -- don't use these generally! */

#define MRI_BYTE_2D(iq,ix,jy)    MRI_BYTE_PTR(iq)[(ix)+(jy)*(iq)->nx]
#define MRI_SHORT_2D(iq,ix,jy)   MRI_SHORT_PTR(iq)[(ix)+(jy)*(iq)->nx]
#define MRI_INT_2D(iq,ix,jy)     MRI_INT_PTR(iq)[(ix)+(jy)*(iq)->nx]
#define MRI_FLOAT_2D(iq,ix,jy)   MRI_FLOAT_PTR(iq)[(ix)+(jy)*(iq)->nx]
#define MRI_DOUBLE_2D(iq,ix,jy)  MRI_DOUBLE_PTR(iq)[(ix)+(jy)*(iq)->nx]
#define MRI_COMPLEX_2D(iq,ix,jy) MRI_COMPLEX_PTR(iq)[(ix)+(jy)*(iq)->nx]

#define FLOAT_TO_BYTE(fff) \
  ( ((fff)<=0.0) ? (0) : ((fff)>=255.5) ? (255) : (byte)((fff)+0.49) )

#define SHORT_TO_BYTE(fff) \
  ( ((fff)<=0) ? (0) : ((fff)>=255) ? (255) : (byte)(fff) )

#define FLOAT_TO_SHORT(fff) ((short)(fff))

/*********** Type: array of MRI_IMAGE pointers ***********/

/*! Array of MRI_IMAGE pointers. */

typedef struct MRI_IMARR {
      int num ;             /*!< Number of actual MRI_IMAGE here */
      int nall ;            /*!< Size of imarr array currently allocated */
      MRI_IMAGE **imarr ;   /*!< Array of MRI_IMAGE pointers */
} MRI_IMARR ;

/*! Get the nn-th image from the image array "name". */

#define IMAGE_IN_IMARR(name,nn) ((name)->imarr[(nn)])
#define IMARR_SUBIMAGE          IMAGE_IN_IMARR
#define IMARR_SUBIM             IMAGE_IN_IMARR

/*! Get the number of images in the image array "name". */

#define IMARR_COUNT(name)       ((name)->num)

#define IMARR_LASTIM(name)      ((name)->imarr[(name)->num-1])
#define IMARR_FIRSTIM(name)     ((name)->imarr[0])

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

/*! Free all images at-and-after [qq] in the MRI_IMARR struct. */

#define TRUNCATE_IMARR(name,qq)                                                 \
   do{ int nn ;                                                                 \
       if( (name) != NULL && qq < (name)->num ){                                \
          for( nn=qq ; nn < (name)->num ; nn++ ) mri_free((name)->imarr[nn]);   \
          (name)->num = qq ;                                                    \
       } } while(0)

extern MRI_IMARR * mri_to_imarr( MRI_IMAGE *imin ) ; /* 06 Dec 2007 */

/*----------------- Misc other types -------------------------------------*/

typedef struct { int i,j;   } int_pair ;    /* 12 Aug 2002 */
typedef struct { int i,j,k; } int_triple ;
typedef struct { int i; float a; } intfloat ; /* 02 Jun 2014 */

/******* macros for complex arithmetic, using comma operator *******/

static float   MRI_fla ;                      /* float temporaries   */
static complex MRI_cxa , MRI_cxb , MRI_cxc ;  /* complex temporaries */

/*! Return a complex from two floats. */

#define CMPLX(x,y) ( MRI_cxa.r = (x) , MRI_cxa.i = (y) , MRI_cxa )

/*! Return complex u+v */

#define CADD(u,v) ( MRI_cxa.r = u.r + v.r , \
                    MRI_cxa.i = u.i + v.r , MRI_cxa )

/*! complex u += v */

#define CADDTO(u,v) ( u.r += v.r , u.i += v.i )

/*! Return complex u-v */
#define CSUB(u,v) ( MRI_cxa.r = u.r - v.r , \
                    MRI_cxa.i = u.i - v.i , MRI_cxa )

/*! complex u -= v */

#define CSUBFROM(u,v) ( u.r -= v.r , u.i -= v.i )

/*! Return complex u*v */

#define CMULT(u,v) ( MRI_cxb.r = u.r * v.r - u.i * v.i , \
                     MRI_cxb.i = u.r * v.i + u.i * v.r , MRI_cxb )

/*! complex u *= v */

#define CMULTBY(u,v) ( MRI_fla = u.r * v.r - u.i * v.i , \
                       u.i     = u.r * v.i + u.i * v.r , u.r = MRI_fla )

/*! Return complex u * conjg(v) */

#define CJMULT(u,v) ( MRI_cxb.r = u.r * v.r + u.i * v.i , \
                      MRI_cxb.i = u.i * v.r - u.r * v.i , MRI_cxb )

/*! complex u *= conjg(v) */

#define CJMULTBY(u,v) ( MRI_fla = u.r * v.r + u.i * v.i , \
                        u.i     = u.i * v.r - u.r * v.i , u.r = MRI_fla )

/*! complex w += u*v */

#define CMADD(u,v,w) ( w.r += u.r * v.r - u.i * v.i , \
                       w.i += u.r * v.i + u.i * v.r    )

/*! Return complex exp(I*t) */

#define CEXPIT(t)   ( MRI_cxc.r = cos(t) , MRI_cxc.i = sin(t) , MRI_cxc )

/**** macros ****/

static int MRI_mm ;

/*! Median of 3. */

#define MEDIAN(a,b,c) ( MRI_mm = 4*((a)<(b)) + 2*((a)<(c)) + ((b)<(c)) , \
                        (MRI_mm==3||MRI_mm==4) ? (a) :                   \
                        (MRI_mm==7||MRI_mm==0) ? (b) : (c) )

#define ABS(a) ( (a) < 0 ? (-(a)):(a) )
#define SIGN(a) ( (a) < 0 ? -1:1 )

/*! Order-statistic filter of 3. */

#define OSFSUM(p,q,r) (0.70*(p)+0.15*((q)+(r)))

/*! Order-statistic filter of 3. */

#define OSFILT(a,b,c) ( MRI_mm = 4*((a)<(b)) + 2*((a)<(c)) + ((b)<(c)) , \
                        (MRI_mm==3||MRI_mm==4) ? OSFSUM(a,b,c) :         \
                        (MRI_mm==7||MRI_mm==0) ? OSFSUM(b,a,c) : OSFSUM(c,a,b) )

#ifndef TRUE
#   define TRUE  (1)
#endif

#ifndef FALSE
#   define FALSE (0)
#endif

#ifndef MAYBE
#   define MAYBE (-1)  /* 26 Feb 2010 */
#endif

#define MRI_BILINEAR  (1)   /* for the warping function */
#define MRI_LINEAR    (1)
#define MRI_BICUBIC   (2)
#define MRI_CUBIC     (2)
#define MRI_FOURIER   (3)
#define MRI_NN        (0)
#define MRI_QUINTIC   (4)   /* Nov 1998 */
#define MRI_HEPTIC    (5)
#define MRI_TSSHIFT   (6)   /* Dec 1999 */

#define MRI_VARP1    (71)   /* 24 Dec 2008 */
#define MRI_WSINC5   (72)   /* 02 Jan 2009 */
#define MRI_WSINC9   (79)   /* 19 Aug 2019 - for 1D shifting only */

#define MRI_FOURIER_NOPAD (66)  /* 13 May 2003 */

#define MRI_HIGHORDER(x) ((x) != MRI_NN && (x) != MRI_LINEAR)

#define SQR(x)   ((x)*(x))
#define CSQR(z)  (SQR(z.r)+SQR(z.i))
#define CABS(z)  complex_abs(z)
#define CARG(z)  ( ((z).r!=0.0 || (z).i!=0.0) ? atan2f((z).i,(z).r) : 0.0 )

/*! complex z /= abs(z) */

#define CUNITIZE(z) ( MRI_fla=CABS(z) , z.r=z.r/MRI_fla , z.i=z.i/MRI_fla )

#ifdef MRI_DEBUG
#  define WHOAMI fprintf(stderr,"in file: %s at line %d\n",__FILE__,__LINE__);
#else
#  define WHOAMI
#endif

#ifdef MRI_DEBUG
#  define IMHEADER(f) \
          fprintf(stderr,"%s: nx=%d ny=%d kind=%d\n",#f,f->nx,f->ny,f->kind);
#else
#  define IMHEADER(f)
#endif

#define MRI_FATAL_ERROR \
        {fprintf(stderr,"in file: %s at line %d\n",__FILE__,__LINE__);EXIT(1);}

#undef  NULL_CHECK
#define NULL_CHECK(p) \
 do{ if( (p)==NULL ){ ERROR_message("malloc failure: out of RAM?"); EXIT(1); } } while(0)

/**** prototypes ****/

#ifdef  __cplusplus
extern "C" {                    /* care of Greg Balls    7 Aug 2006 [rickr] */
#endif

extern void        mri_input_delay( MRI_IMAGE * ) ;
extern void        mri_purge_delay( MRI_IMAGE * ) ;
extern void        mri_add_fname_delay( char * , MRI_IMAGE * ) ;
extern MRI_IMARR * mri_read_file_delay( char * ) ;
extern MRI_IMARR * mri_read_3D_delay( char * ) ;

extern void   mri_purge    ( MRI_IMAGE * ) ;  /* 20 Dec 2006 */
extern void   mri_unpurge  ( MRI_IMAGE * ) ;
extern void   mri_killpurge( MRI_IMAGE * ) ;
extern char * mri_purge_get_tmpdir(void) ;    /* 21 Dec 2006 */
extern char * mri_purge_get_tsuf(void) ;      /* 02 Aug 2007 */
extern char * mri_get_tempfilename( char * ); /* 27 Jul 2009 */

extern int mri_counter( MRI_IMAGE * , float , float ) ; /* 16 Jul 2007 */

#define MRI_IS_PURGED(iq) \
  ( (iq)!=NULL && (iq)->fondisk==IS_PURGED && (iq)->fname!=NULL )

#define MRI_HAS_DATA(iq)                                    \
  ( (iq)!= NULL &&                                          \
    ( ( (iq)->fondisk==IS_PURGED && (iq)->fname!=NULL ) ||  \
      (iq)->im != NULL                                 )  )

extern int mri_equal( MRI_IMAGE *, MRI_IMAGE * ) ; /* 30 Jun 2003 */

extern MRI_IMARR * mri_read_analyze75( char * ) ;  /* 05 Feb 2001 */
extern MRI_IMARR * mri_read_siemens( char * ) ;    /* 12 Mar 2001 */
extern MRI_IMARR * mri_read3D_analyze75( char * ); /* 26 Aug 2002 */

extern MRI_IMAGE * mri_read_stuff( char * ) ;      /* 22 Nov 2002 */
extern void        mri_inflate_pbm( MRI_IMAGE * ); /* 02 Jan 2002 */

extern void mri_add_name( char * , MRI_IMAGE * ) ;

extern MRI_IMAGE ** mri_stat_seq( MRI_IMAGE * ) ;

extern MRI_IMAGE * mri_extract_from_mask( MRI_IMAGE *, byte *, int ) ;
extern void binarize_mask( int , byte * ) ;

/*--------------------------------*/

#define NSTAT_MEAN        0
#define NSTAT_SUM         1
#define NSTAT_SIGMA       2
#define NSTAT_CVAR        3
#define NSTAT_MEDIAN      4
#define NSTAT_MAD         5
#define NSTAT_MAX         6
#define NSTAT_MIN         7
#define NSTAT_MODE        8       /* DRG 05/21/2015 */
#define NSTAT_NZMODE      9       /* DRG 05/21/2015 */
#define NSTAT_ABSMAX      13
#define NSTAT_VAR         17
#define NSTAT_NUM         18
#define NSTAT_PERCENTILE  19
#define NSTAT_RANK        21      /* ZSS Jan 10 */
#define NSTAT_FRANK       22      /* ZSS Jan 10 */
#define NSTAT_P2SKEW      23      /* ZSS March 04 10*/
#define NSTAT_KURT        24      /* ZSS Jan   04 11*/
#define NSTAT_mMP2s0      25
#define NSTAT_mMP2s1      26
#define NSTAT_mMP2s2      27
#define NSTAT_mmMP2s0     28
#define NSTAT_mmMP2s1     29
#define NSTAT_mmMP2s2     30
#define NSTAT_mmMP2s3     31
#define NSTAT_NZNUM       32
#define NSTAT_FNZNUM      33
#define NSTAT_diffs0      34
#define NSTAT_diffs1      35
#define NSTAT_diffs2      36
#define NSTAT_adiffs0     37
#define NSTAT_adiffs1     38
#define NSTAT_adiffs2     39
#define NSTAT_LIST        40
#define NSTAT_HIST        41
#define NSTAT_FILLED      42
#define NSTAT_UNFILLED    43
#define NSTAT_MASKED      44
#define NSTAT_MASKED2     45

#define NSTAT_FWHMx      63   /*these should be after all other NSTAT_* values */
#define NSTAT_FWHMy      64
#define NSTAT_FWHMz      65
#define NSTAT_FWHMbar    66
#define NSTAT_FWHMbar12  67

#define NBISTAT_BASE               66601
#define NBISTAT_SPEARMAN_CORR      66601
#define NBISTAT_QUADRANT_CORR      66602
#define NBISTAT_PEARSON_CORR       66603
#define NBISTAT_MUTUAL_INFO        66604
#define NBISTAT_NORMUT_INFO        66605
#define NBISTAT_JOINT_ENTROPY      66606
#define NBISTAT_HELLINGER          66607
#define NBISTAT_CORR_RATIO_M       66608
#define NBISTAT_CORR_RATIO_A       66609
#define NBISTAT_CORR_RATIO_U       66610
#define NBISTAT_NUM                66611
#define NBISTAT_NCD                66612
#define NBISTAT_KENDALL_TAUB       66613 /* 29 Apr 2010 */
#define NBISTAT_TICTACTOE_CORR     66614 /* 30 Mar 2011 */
#define NBISTAT_L2SLOPE            66615 /* 26 Apr 2012 */
#define NBISTAT_L1SLOPE            66616 /* 26 Apr 2012 */
#define NBISTAT_QUANTILE_CORR      66617 /* 11 May 2012 */

#define NBISTAT_BC_PEARSON_M       66691
#define NBISTAT_BC_PEARSON_V       66692

#define NBISTAT_EUCLIDIAN_DIST     66693 /* 4 May 2012, ZSS */
#define NBISTAT_CITYBLOCK_DIST     66694 /* 4 May 2012, ZSS */


extern float mri_nbistat( int , MRI_IMAGE *, MRI_IMAGE * ) ; /* 26 Oct 2006 */
extern void mri_nbistat_setclip( float, float , float, float ) ;
extern void mri_bistat_setweight( MRI_IMAGE *wm ) ;  /* 14 Aug 2007 */
extern void set_mri_nstat_fillvalue(float tf);
extern void set_mri_nstat_unfillvalue(float tf);

extern MRI_IMAGE * mri_edit_image( float pthr, float power, MRI_IMAGE * im ) ;

extern MRI_IMARR * mri_read_mpeg( char * ) ;    /* 03 Dec 2003 */
extern int         mri_isgray( MRI_IMAGE * ) ;
extern int         mri_imcount_mpeg( char * ) ;

extern void cfft( int , int , float * , float * ) ;
extern void cfft2d_cox( int , int , int , float * , float * ) ;
extern void csfft_cox( int,int , complex * ) ;
extern void csfft_many( int,int,int , complex * ) ;
extern int  csfft_nextup(int) ;
extern int csfft_nextup_one35(int) ;
extern int csfft_nextup_even(int) ;
extern void csfft_scale_inverse(int) ;
extern void csfft_force_fftn(int) ; /* 08 Oct 2017 */
extern int csfft_allows_anything(void) ; /* Jun 2018 */

extern void mri_fftshift( MRI_IMAGE *, float,float,float, int ) ; /* 13 May 2003 */

extern void * mri_data_pointer(MRI_IMAGE *) ;
extern void mri_free( MRI_IMAGE * ) ;
extern void mri_clear( MRI_IMAGE * ) ;  /* 31 Jan 2007 */
extern void mri_fix_data_pointer( void * , MRI_IMAGE * ) ;
#define mri_set_data_pointer(iq,pt) mri_fix_data_pointer((pt),(iq))

#define MRI_FREE(iq) do{ mri_free(iq); (iq)=NULL; } while(0)

extern char * mri_dicom_header( char * ) ;  /* 15 Jul 2002 */
extern void   mri_dicom_pxlarr( off_t *, unsigned int * ) ;
extern void   mri_dicom_noname( int ) ;
extern void   mri_dicom_nohex ( int ) ;
extern void   mri_dicom_setvm ( int ) ;     /* 28 Oct 2002 */
extern void   mri_dicom_seterr( int ) ;     /* 05 Nov 2002 */
extern void   mri_dicom_header_use_printf( int ) ; /* 02 May 2008 */
extern void   mri_dicom_header_show_size_offset( int ); /* 17 Oct 2012 [rcr] */

extern MRI_IMARR * mri_read_dicom( char * )  ;
extern int         mri_imcount_dicom( char * ) ;
extern char *      mri_dicom_sexinfo( void ) ;   /* 23 Dec 2002 */
extern char *      mri_dicom_sex1010( void ) ;
extern int         mri_possibly_dicom( char * ) ;        /* 07 May 2003 */
extern int         mri_siemens_slice_times( int *, int *, float ** );
extern int         mri_sst_get_verb( void );
extern int         mri_sst_set_verb( int );
extern char *      mri_dicom_hdrinfo( char *fname, int natt, char **att ,
                                      int nposn, char *sepstr) ;
extern char *      mri_dicom_hdrinfo_full( char *fname, int natt, char **att ,
                                      int nposn, char *sepstr ) ;

/*! Set the data pointer in an MRI_IMAGE to NULL. */

#define mri_clear_data_pointer(iq) mri_fix_data_pointer(NULL,(iq))

/*! Clear the data pointer and free the MRI_IMAGE shell */

#define mri_clear_and_free(iq) \
 do{ mri_fix_data_pointer(NULL,(iq)); mri_free((iq)); } while(0)

/*! Set all pixels in MRI_IMAGE to zero. */

#define mri_zero_image(iq) \
   memset(mri_data_pointer(iq),0,(iq)->nvox*(iq)->pixel_size)

extern int mri_allzero( MRI_IMAGE *im ) ;  /* check if all pixels are 0 */
extern int mri_nonzero_count( MRI_IMAGE *im ) ;                   /* 28 Dec 2015 */
extern int mri_nonzero_count_inmask( MRI_IMAGE *im, byte *mmm ) ; /* 12 Dec 2019 */

extern MRI_IMAGE * mri_zeropad_3D( int,int,int,int,int,int , MRI_IMAGE * ) ;
extern MRI_IMAGE * mri_valpad_2D( int,int,int,int, MRI_IMAGE *, byte val ) ;
extern MRI_IMAGE * mri_zeropad_2D( int,int,int,int, MRI_IMAGE * ) ;

extern double mri_max( MRI_IMAGE * ) ;
extern double mri_min( MRI_IMAGE * ) ;
extern double mri_maxabs( MRI_IMAGE * ) ;
extern double_pair mri_minmax   ( MRI_IMAGE *im ) ;  /* Apr 2013 */
extern double_pair mri_minmax_nz( MRI_IMAGE *im ) ;  /* Apr 2013 */

intfloat mri_indmax_nz( MRI_IMAGE * ) ;  /* 02 Jun 2014 */
intfloat mri_indmin_nz( MRI_IMAGE * ) ;

extern MRI_IMAGE * mri_cut_2D( MRI_IMAGE * , int,int,int,int ) ;
extern int mri_cut_many_2D(MRI_IMARR *,  int,int,int,int );

extern MRI_IMAGE * mri_subset_x2D( int , int * , MRI_IMAGE * ) ;

extern MRI_IMAGE * mri_check_2D( int , MRI_IMAGE * , MRI_IMAGE * ) ;

#define WIPER_FROM_LEFT   1
#define WIPER_FROM_BOTTOM 2
#define WIPER_FROM_CENTER 3

extern MRI_IMAGE * mri_wiper_2D( int wcode,float wfac, MRI_IMAGE *ima, MRI_IMAGE *imb ) ;
extern MRI_IMAGE * mri_mix_2D( float , MRI_IMAGE * , MRI_IMAGE * ) ;

extern MRI_IMAGE * mri_cut_3D( MRI_IMAGE * , int,int,int,int,int,int ) ;

/** 15 Apr 1999 **/

extern void upsample_7( int , int , float * , float * ) ;
extern void upsample_1( int , int , float * , float * ) ; /* 12 Mar 2002 */
extern MRI_IMAGE * mri_dup2D( int , MRI_IMAGE * ) ;
extern void        mri_dup2D_mode( int ) ;                /* 12 Mar 2002 */

extern void mri_move_guts( MRI_IMAGE *, MRI_IMAGE * ) ;  /* 28 Mar 2002 */
extern MRI_IMAGE * mri_copy( MRI_IMAGE * ) ;             /* 17 Apr 2000 */
extern MRI_IMAGE * mri_expand_2D( int , MRI_IMAGE * ) ;  /* 22 Feb 2004 */
extern MRI_IMAGE *mri_new( int , int , MRI_TYPE ) ;
extern MRI_IMAGE *mri_read( char * ) ;
extern MRI_IMAGE *mri_read_ge4( char * ) ;               /* 03 Jun 2003 */

extern void   fclose_maybe( FILE *fp ) ;
extern FILE * fopen_maybe ( char *fname ) ;

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
extern char * mri_1D_tostring( MRI_IMAGE *im ) ;          /* 15 Nov 2007 */

extern void mri_adjust_fvectim( MRI_IMAGE *im, int vdim ) ;  /* 28 Nov 2008 */
extern MRI_IMAGE * mri_new_fvectim( int nx, int ny, int nz, int vdim ) ;

extern MRI_IMAGE * mri_read_ascii_ragged_complex(char *,float); /* 08 Mar 2007 */

extern MRI_IMAGE * mri_read_ascii_ragged_fvect( char *, float, int ) ;
extern MRI_IMARR * mri_fvect_to_imarr( MRI_IMAGE *inim ) ;
extern MRI_IMAGE * mri_imarr_to_fvect( MRI_IMARR *imar ) ;
extern MRI_IMAGE * mri_float_arrays_to_image(float **vecs, int vec_len,
                                             int vec_num); /* 16 Apr 2014 */
extern MRI_IMAGE * mri_pair_to_fvect( MRI_IMAGE *, MRI_IMAGE * ) ;
extern MRI_IMAGE * mri_triple_to_fvect( MRI_IMAGE *, MRI_IMAGE *, MRI_IMAGE *) ;
extern MRI_IMAGE * mri_fvect_subimage( MRI_IMAGE *inim , int kk ) ;

extern MRI_IMAGE * mri_read_ragged_fromstring( char *, float); /* 05 Jan 2007 */

extern MRI_IMAGE * mri_read_1D( char * ) ;               /* 16 Nov 1999 */
extern MRI_IMAGE * mri_read_double_1D( char * ) ;
extern MRI_IMAGE * mri_read_complex_1D( char * ) ;
extern int mri_write_1D( char * , MRI_IMAGE * ) ;        /* 16 Nov 1999 */
extern MRI_IMAGE * mri_read_1D_stdin(void) ;             /* 25 Jan 2008 */
extern MRI_IMAGE * mri_read_1D_pipe (FILE *fp) ;         /* 26 Aug 2019 */
extern MRI_IMAGE * mri_copy_1D_stdin(void) ;             /* 05 Mar 2010 */
extern void        mri_clear_1D_stdin(void);
extern char * mri_read_1D_headerlines( char * ) ;        /* 05 Dec 2010 */

extern MRI_IMAGE * mri_read_4x4AffXfrm_1D( char *fname );/* 24 Nov 2009 */
extern MRI_IMAGE * mri_1D_fromstring( char * ) ;         /* 28 Apr 2003 */

extern int setup_mri_write_angif( void ) ;               /* 28 Jun 2001 */
extern int mri_write_angif( char *, MRI_IMARR * ) ;
extern MRI_IMAGE * mri_colorsetup( int,int,int,int ) ;   /* 05 Oct 2004 */

extern MRI_IMAGE *mri_new_vol      ( int,int,int , MRI_TYPE ) ;
extern MRI_IMAGE *mri_new_vol_empty( int,int,int , MRI_TYPE ) ;

MRI_IMAGE *mri_new_7D_generic( int nx, int ny, int nz, int nt,
                               int nu, int nv, int nw,
                               MRI_TYPE kind , int make_space ) ;

/*! Create new MRI_IMAGE of type kk, with same dimensions as iq. */

#define mri_new_conforming(iq,kk)                                   \
   mri_new_7D_generic( (iq)->nx, (iq)->ny, (iq)->nz , (iq)->nt ,    \
                       (iq)->nu, (iq)->nv, (iq)->nw , (kk) , TRUE )

/*! Create new MRI_IMAGE of type kk, with same dimensions as iq,
    and with no data space allocated. */

#define mri_empty_conforming(iq,kk)                                 \
   mri_new_7D_generic( (iq)->nx, (iq)->ny, (iq)->nz , (iq)->nt ,    \
                       (iq)->nu, (iq)->nv, (iq)->nw , (kk) , FALSE )

extern MRI_IMARR * mri_read_3D( char * ) ;
extern MRI_IMARR * mri_read_3A( char * ) ;
extern MRI_IMARR * mri_read_file( char * ) ;
extern int mri_imcount( char * ) ;
extern MRI_IMARR * mri_read_many_files( int nf , char * fn[] ) ;
extern MRI_IMARR * mri_read_resamp_many_files( int nf, char * fn[] ,
                                               int nxnew, int nynew, byte pval);

/** returns array of byte images: red, green, blue **/

extern MRI_IMARR * mri_read_ppm3( char * fname ) ;
extern MRI_IMAGE * mri_read_ppm( char * fname ) ;

extern void mri_read_ppm_header( char *, int *, int *) ; /* 17 Sep 2001 */

MRI_IMAGE *mri_read_just_one( char * fname ) ;
MRI_IMAGE *mri_read_nsize( char * fname ) ;
MRI_IMARR *mri_read_many_nsize( int nf , char * fn[] ) ;

void init_MCW_sizes(void) ;
char * imsized_fname( char * fname ) ;
char * my_strdup( char * str ) ;

#define mri_filesize THD_filesize  /* 22 Mar 2007 */

extern void mri_overlay_2D( MRI_IMAGE *, MRI_IMAGE *, int,int ) ;

extern void mri_swapbytes( MRI_IMAGE * ) ;

extern void swap_twobytes  ( int n , void * ar ) ;  /* 14 Sep 1998 */
extern void swap_fourbytes ( int n , void * ar ) ;
extern void swap_eightbytes( int n , void * ar ) ;  /* 06 Feb 2003 */

extern MRI_IMAGE *mri_to_float( MRI_IMAGE * ) ;
extern MRI_IMAGE *mri_to_short( double , MRI_IMAGE * ) ;
extern MRI_IMAGE *mri_to_short_scl( double,double , MRI_IMAGE * ) ;
extern MRI_IMAGE *mri_to_short_sclip( double,double , int,int , MRI_IMAGE * ) ;
extern MRI_IMAGE *mri_to_complex( MRI_IMAGE * ) ;
extern MRI_IMAGE *mri_to_byte( MRI_IMAGE * ) ;
extern byte      *mri_to_bytemask( MRI_IMAGE *, float,float ) ;
extern MRI_IMAGE *mri_to_byte_scl( double , double , MRI_IMAGE * ) ;

extern MRI_IMAGE * mri_to_rgb( MRI_IMAGE * ) ;
extern MRI_IMAGE * mri_3to_rgb( MRI_IMAGE * , MRI_IMAGE * , MRI_IMAGE * ) ;
extern MRI_IMARR * mri_rgb_to_3float( MRI_IMAGE * ) ;
extern MRI_IMARR * mri_rgb_to_3byte( MRI_IMAGE * ) ;
extern MRI_IMAGE * mri_sharpen_rgb( float , MRI_IMAGE * ) ;
extern MRI_IMAGE * mri_flatten_rgb( MRI_IMAGE * ) ;
extern void mri_invert_inplace( MRI_IMAGE *) ;   /* 07 Apr 2003 */
extern void mri_gamma_rgb_inplace( float gam , MRI_IMAGE *im ) ;

extern MRI_IMAGE * mri_4to_rgba( MRI_IMAGE *rim , MRI_IMAGE *gim , MRI_IMAGE *bim , MRI_IMAGE *aim ) ;
extern MRI_IMARR * mri_rgba_to_4float( MRI_IMAGE *oldim ) ;
extern MRI_IMARR * mri_rgba_to_4byte( MRI_IMAGE *oldim ) ;

extern void mri_sharpen3D_pos( MRI_IMAGE *im , float phi ) ; /* 13 Feb 2017 */

extern MRI_IMAGE * mri_median21( MRI_IMAGE *innim ) ; /* 28 Oct 2014 */
extern MRI_IMAGE * mri_sharpness( MRI_IMAGE *inim ) ;

extern MRI_IMAGE * mri_make_rainbow( int, int, int, rgbyte * ) ;

extern MRI_IMAGE * mri_to_rgba( MRI_IMAGE * ) ;  /* 20 Mar 2002 */

extern MRI_IMAGE *mri_pair_to_complex( MRI_IMAGE * , MRI_IMAGE * ) ;
extern MRI_IMARR *mri_complex_to_pair( MRI_IMAGE * ) ;
extern float complex_abs( complex z ) ;          /* 24 Aug 2009 */

extern MRI_IMAGE *mri_to_complex_ext( MRI_IMAGE * , int , int , int ) ;

extern MRI_IMAGE *mri_scale_to_float( float , MRI_IMAGE * ) ;
extern void mri_threshold( double , double , MRI_IMAGE * , MRI_IMAGE * ) ;
extern MRI_IMAGE * mri_mult_to_float( float * , MRI_IMAGE * ) ;

extern float_pair mri_threshold_minmax( double,double, MRI_IMAGE *, MRI_IMAGE * ) ;
extern int_pair   mri_threshold_minmax_indexes(void) ;

extern void mri_maskify( MRI_IMAGE *im , byte *mask ) ; /* Jul 2010 */

extern MRI_IMAGE * mri_scalize( MRI_IMAGE *, int, float * ) ; /* 20 Oct 2003 */

extern MRI_IMAGE *mri_multiply_complex( int , MRI_IMAGE * , MRI_IMAGE * ) ;
extern MRI_IMAGE *mri_complex_phase( MRI_IMAGE * ) ;

extern MRI_IMAGE *mri_complex_imag( MRI_IMAGE *im ) ;  /* 18 Apr 2011 */
extern MRI_IMAGE *mri_complex_real( MRI_IMAGE *im ) ;

extern MRI_IMAGE *mri_to_mri( int , MRI_IMAGE * ) ;
extern MRI_IMAGE *mri_to_mri_scl( int , double , MRI_IMAGE * ) ;
extern MRI_IMAGE *mri_complex_abs( MRI_IMAGE * ) ;

extern void mri_fft_complex( int , float , MRI_IMAGE * ) ;
extern float *mri_setup_taper( int , float ) ;

extern MRI_IMAGE *mri_warp( MRI_IMAGE * , int , int , int ,
                            void func(float,float,float *,float *) ) ;

extern MRI_IMAGE *mri_warp_bicubic( MRI_IMAGE * , int , int ,
                                    void func(float,float,float *,float *) ) ;

extern MRI_IMAGE *mri_warp_bilinear( MRI_IMAGE * , int , int ,
                                     void func(float,float,float *,float *) ) ;

#undef WARP_POINT_ROUTINES
#ifdef WARP_POINT_ROUTINES
extern float mri_warp_bicubic_point( MRI_IMAGE * , int , int ,
                                     void func( float,float,float *,float *) ) ;

extern float mri_rotate_point( MRI_IMAGE *im, float,float,float,float, int,int ) ;
#endif /* WARP_POINT_ROUTINES */

extern void mri_warp_setpow( float gg ) ;  /* 15 Jan 2007 */

extern MRI_IMAGE *mri_resize( MRI_IMAGE * , int , int ) ;

extern MRI_IMAGE *mri_resize_NN( MRI_IMAGE *, int , int ) ;  /* 08 Jun 2004 */
extern MRI_IMAGE *mri_squareaspect( MRI_IMAGE * ) ;

extern MRI_IMAGE *mri_rotate         ( MRI_IMAGE * , float,float,float,float ) ;
extern MRI_IMAGE *mri_rotate_bilinear( MRI_IMAGE * , float,float,float,float ) ;

extern MRI_IMAGE *mri_rota         ( MRI_IMAGE * , float,float,float ) ;
extern MRI_IMAGE *mri_rota_bilinear( MRI_IMAGE * , float,float,float ) ;
extern MRI_IMAGE *mri_rota_shear   ( MRI_IMAGE * , float,float,float ) ;
extern MRI_IMAGE *mri_rota_variable( int, MRI_IMAGE * , float,float,float ) ;

extern MRI_IMAGE *mri_aff2d_byte( MRI_IMAGE *,int,float,float,float,float) ;
extern MRI_IMAGE *mri_aff2d_rgb ( MRI_IMAGE *,int,float,float,float,float) ;

/** 27 Nov 2001: mri_scale.c **/

extern void mri_scale_inplace( float , MRI_IMAGE * ) ;

extern void ft_shift2( int, int, float, float *, float, float * ) ;

extern MRI_IMAGE *mri_float_func( int,int,
                                  float , float , float , float ,
                                  float (*func)(float,float) ) ;

extern void mri_histogram( MRI_IMAGE * , float,float ,
                                         int,int, int h[] ) ;

extern void mri_histobyte        ( MRI_IMAGE * , int * ) ;
extern void mri_histoshort_all   ( MRI_IMAGE * , int * ) ;    /* 25 Jul 2001 */
extern void mri_histoshort_nonneg( MRI_IMAGE * , int * ) ;

extern void mri_percents( MRI_IMAGE * , int nper , float per[] ) ;
extern MRI_IMAGE * mri_flatten( MRI_IMAGE * ) ;
extern void mri_flatten_set_bfac(float b) ;                   /* 16 Mar 2017 */
extern float mri_quantile( MRI_IMAGE * im , float alpha ) ;
float percentile_nzabs( int nvox , float *far , float per ) ; /* 24 May 2019 */

extern float_pair mri_twoquantiles( MRI_IMAGE * im, float alpha, float beta ) ;

extern void qsort_short( int , short * ) ;
extern void qsort_float( int , float * ) ;
extern void qsort_float_rev( int , float * ) ;

extern void qsort_pair( int , float * , int * ) ;
extern void qsort_pair_rev( int , float * , int * ) ; /* 08 Mar 2019 */
extern void qsort_pairX( int , float * , int * ) ;    /* 08 Mar 2019 */

extern void qsort_int( int , int * ) ;
extern void qsort_int_mostly( int , int * , int ) ;   /* 12 Sep 2017 */

extern void isort_short( int , short * ) ;
extern void isort_float( int , float * ) ;
extern void isort_pair ( int , float * , int * ) ;

extern void mri_xsort_inplace( MRI_IMAGE *im , int rev ) ;
extern void mri_csort_inplace( MRI_IMAGE *im , int rev , int jc ) ; /* 07 Oct 2011 */

extern MRI_IMAGE * mri_nsize( MRI_IMAGE * ) ;

extern float * mri_lsqfit( MRI_IMAGE * fitim , MRI_IMARR * refim , MRI_IMAGE * ) ;
extern double * mri_startup_lsqfit( MRI_IMARR * , MRI_IMAGE * ) ;
extern float * mri_delayed_lsqfit( MRI_IMAGE * , MRI_IMARR * , double * ) ;
extern float * lsqfit( int , float * , float * , int , float *ref[] ) ;
extern double * startup_lsqfit( int , float * , int , float *ref[] ) ;
extern float * delayed_lsqfit( int , float * , int , float *ref[] , double * ) ;

extern void mri_polyfit_verb( int ) ;
extern void mri_polyfit_set_basis( char *str );
extern MRI_IMAGE * mri_polyfit( MRI_IMAGE *, int, MRI_IMARR *, byte *, float, int ) ;
extern MRI_IMAGE * mri_polyfit_byslice( MRI_IMAGE *, int, MRI_IMARR *, byte *, float, int ) ;

extern MRI_IMAGE * mri_pcvector  ( MRI_IMARR *imar , int,int ) ;
extern MRI_IMAGE * mri_meanvector( MRI_IMARR *imar , int,int ) ;
extern MRI_IMAGE * mri_MMBvector ( MRI_IMARR *imar , int,int,int ) ; /* 05 Aug 2010 */

extern MRI_IMAGE * mri_sobel( int , int , MRI_IMAGE * ) ;
extern MRI_IMAGE * mri_sharpen( float , int , MRI_IMAGE * ) ;
extern MRI_IMAGE * mri_transpose( MRI_IMAGE * ) ;
extern MRI_IMAGE * mri_interleave_columns(MRI_IMAGE *, int) ; /* 27 Jul 2009 */
extern MRI_IMAGE * mri_rowmajorize_1D( MRI_IMAGE *im ) ;      /* 14 Mar 2013 */

#define FILT_FFT_WRAPAROUND  1

extern MRI_IMAGE * mri_filt_fft( MRI_IMAGE * im , float,int,int,int ) ;

extern MRI_IMAGE *mri_medianfilter( MRI_IMAGE *, float, byte *, int ); /* 22 Feb 2005 */
extern MRI_IMAGE *mri_flatfilter  ( MRI_IMAGE *, float, byte *, int ); /* 24 Jul 2008 */
extern void mri_medianfilter_usedxyz( int i ) ;                        /* 08 Aug 2006 */
extern void mri_flatfilter_usedxyz  ( int i ) ;

void mri_Set_KO_catwrap(void);
void mri_Set_OK_catwrap(void);
void mri_Set_OK_catrandwrap(void);
void mri_Set_OK_WrapZero(byte vv);
void mri_Set_KO_WrapZero(void);
extern MRI_IMAGE * mri_cat2D( int,int,int,void *,MRI_IMARR *) ;
extern MRI_IMARR * mri_uncat2D( int , int , MRI_IMAGE * im ) ; /* 09 May 2000 */

extern MRI_IMAGE * mri_catvol_1D( MRI_IMARR *imar , int dir ); /* 08 Dec 2010 */
extern MRI_IMAGE * mri_catvol_1D_ab( MRI_IMARR *imar , int dir, int na,int nb );

extern MRI_IMAGE * mri_shift_1D( MRI_IMAGE * im , float shift ) ;

/*** image alignment procedures and constants ***/

#define ALIGN_DFSPACE_TYPE    1
#define ALIGN_DFTIME_TYPE     2

#define ALIGN_VERBOSE_CODE    1   /* verbose output during routine */
#define ALIGN_NOITER_CODE     2   /* don't iterate alignment algorithm */
#define ALIGN_REGISTER_CODE   4   /* return MRI_IMARR * of registered images */
#define ALIGN_DETREND_CODE    8   /* remove trend from registered images (DFTIME only) */
#define ALIGN_DOBOTH_CODE    16   /* do dfspace before dftime (DFTIME only) */
#define ALIGN_DEBUG_CODE     32   /* print out debugging info */
#define ALIGN_FREEUP_CODE    64   /* free input images when no longer needed */
#define ALIGN_BILINEAR_CODE 128   /* use bilinear interpolation in mri_align */
#define ALIGN_FOURIER_CODE  256   /* use Fourier interpolation in mri_align */

extern MRI_IMARR * mri_align_dfspace( MRI_IMAGE *, MRI_IMAGE * , MRI_IMARR *,
                                      int, float *, float *, float * ) ;

extern MRI_IMARR * mri_align_dftime( MRI_IMAGE *, MRI_IMAGE * , MRI_IMARR *,
                                     int, float *, float *, float * ) ;

extern void mri_align_params( int,float,float,float,float,float,float ) ;
extern void mri_align_method( int,int,int ) ;  /* 01 Oct 1998 */

extern void mri_get_cmass_2D( MRI_IMAGE *, float *, float * ); /* 12 Nov 2001 */
extern void mri_get_cmass_3D( MRI_IMAGE *, float *, float * , float *);

extern float mri_spearman_corr( MRI_IMAGE *, MRI_IMAGE * ) ;  /* 08 Mar 2006 */

/*---------------------------------------------------------------------*/
/* 07 April 1998: routines for one-at-a-time alignment (mri_2dalign.c) */

/*! Struct used in 2D image registration. */

typedef struct {
   MRI_IMARR * fitim , * fine_fitim ;
   double * chol_fitim , * chol_fine_fitim ;
} MRI_2dalign_basis ;

extern void mri_2dalign_params( int,float,float,float,float,float,float ) ;
extern void mri_2dalign_method( int,int,int ) ;
extern MRI_2dalign_basis * mri_2dalign_setup( MRI_IMAGE * , MRI_IMAGE * ) ;
extern MRI_IMAGE * mri_2dalign_one( MRI_2dalign_basis * , MRI_IMAGE * ,
                                    float * , float * , float * ) ;
extern MRI_IMARR * mri_2dalign_many( MRI_IMAGE *, MRI_IMAGE * , MRI_IMARR *,
                                     float * , float * , float * ) ;
extern void mri_2dalign_cleanup( MRI_2dalign_basis * ) ;
/*---------------------------------------------------------------------*/

/*** routine to flip 2D images around ***/

#define MRI_ROT_0   1  /* codes for various rotations */
#define MRI_ROT_90  2  /* [do not change these unless */
#define MRI_ROT_180 4  /*  imseq.h is changed also!]  */
#define MRI_ROT_270 8
#define MRI_FLMADD  128

extern MRI_IMAGE * mri_flippo( int rot , int mirror , MRI_IMAGE * im ) ;

extern MRI_IMAGE * mri_flip3D( int,int,int , MRI_IMAGE *inim ) ; /* 19 Mar 2003 */

/*---------------------------------------------------------------------*/
/*--------- 22 April 1998: byte order routines (mri_order.c) ----------*/

#define LSB_FIRST      1
#define MSB_FIRST      2
#define NATIVE_ORDER  -1

#define REVERSE_ORDER(bord) (3-(bord))  /* 21 Jun 2002 */

#define ORDER_LEN        9
#define LSB_FIRST_STRING "LSB_FIRST"
#define MSB_FIRST_STRING "MSB_FIRST"
#define NATIVE_STRING    "NATIVE_ORDER"

#define BYTE_ORDER_STRING(qq) (  ((qq)==LSB_FIRST) ? LSB_FIRST_STRING \
                               : ((qq)==MSB_FIRST) ? MSB_FIRST_STRING \
                                                   : "Illegal Value" )
extern int mri_short_order(void) ;
extern int mri_int_order(void) ;
extern void mri_swap2( int , short * ) ;
extern void mri_swap4( int , int * ) ;

/*---------------------------------------------------------------------*/
/*------------------ 18 Sep 2001: drawing stuff -----------------------*/

extern void mri_drawline( MRI_IMAGE *im, int x0,int y0, int x1,int y1,
                          byte r,byte g,byte b );

extern void mri_drawfilledrectangle( MRI_IMAGE *im ,
                                     int x, int y, int width, int height ,
                                     byte r,byte g,byte b );

extern void mri_drawemptyrectangle( MRI_IMAGE *im ,
                                    int x, int y, int width, int height ,
                                    byte r,byte g,byte b );

extern void mri_drawtext( MRI_IMAGE *im ,
                          int x, int y, int height, int angle, char *s,
                          byte r,byte g,byte b );

extern void mri_draw_opacity( float ) ;
extern void mri_draw_force_opaque(int fo) ;

extern void mri_drawcircle( MRI_IMAGE *im ,
                            int cx, int cy, int radius, byte r,byte g,byte b, int fill ) ;

/**********************************************************************/

#ifdef  __cplusplus
}
#endif

#undef min
#undef max

#ifdef  __cplusplus
extern "C" {                    /* care of Greg Balls    7 Aug 2006 [rickr] */
#endif
extern MRI_IMAGE * mri_downsize_by2( MRI_IMAGE * ) ;    /* 27 Apr 2012 */

/************************ Statistics routines *************************/

/**
  if the math library doesn't have the log(gamma(x))
  function (as on Linux, for example)
**/

#ifdef NO_GAMMA
/* extern double gamma_12    ( double ) ;   these are static functions */
/* extern double gamma_asympt( double ) ;           7 Aug 2006 [rickr] */
extern double gamma       ( double ) ;
#endif

extern double lnbeta         ( double , double ) ;
extern double incbeta        ( double , double , double , double ) ;
extern double incbeta_inverse( double , double , double , double ) ;
extern double qginv          ( double ) ;
extern double qg             ( double ) ;     /* 21 Mar 2001 */
extern double log10qg        ( double ) ;
#define QG(x) (0.5*erfc(x/1.414213562373095))

#define erfcinv(y) (0.70710678*qginv(0.5*y))  /* 07 Oct 1999 */

extern double student_t2p( double , double ) ;
extern double student_p2t( double , double ) ;
extern double student_t2z( double , double ) ;

extern double correl_t2p ( double , double , double , double ) ;
extern double correl_t2z ( double , double , double , double ) ;
extern double correl_p2t ( double , double , double , double ) ;

extern double studave_t2p( double , double , double ) ;  /* not implemented */
extern double studave_t2z( double , double , double ) ;  /* not implemented */
extern double studave_p2t( double , double , double ) ;

extern double fstat_p2t( double , double , double ) ;
extern double fstat_t2p( double , double , double ) ;
extern double fstat_t2z( double , double , double ) ;

extern double normal_t2p  ( double zz ) ;
extern double normal_p2t  ( double qq ) ;
#define       normal_t2z(x) (x)                     /* no function needed here! */

extern double chisq_t2p   ( double xx , double dof ) ;
extern double chisq_t2z   ( double xx , double dof ) ;
extern double chisq_p2t   ( double qq , double dof ) ;

extern double beta_t2p    ( double xx , double aa , double bb ) ;
extern double beta_t2z    ( double xx , double aa , double bb ) ;
extern double beta_p2t    ( double qq , double aa , double bb ) ;

extern double binomial_t2p( double ss , double ntrial , double ptrial ) ;
extern double binomial_t2z( double ss , double ntrial , double ptrial ) ;
extern double binomial_p2t( double qq , double ntrial , double ptrial ) ;

extern double gamma_t2p   ( double xx , double sh , double sc ) ;
extern double gamma_t2z   ( double xx , double sh , double sc ) ;
extern double gamma_p2t   ( double qq , double sh , double sc ) ;

extern double poisson_t2p ( double xx , double lambda ) ;
extern double poisson_t2z ( double xx , double lambda ) ;
extern double poisson_p2t ( double qq , double lambda ) ;

#ifdef  __cplusplus
}
#endif

/*-----------------------------------------------------*/
/* Add extra int 'kk' to floatvec struct [26 Jun 2018] */

typedef struct { int nar ; float  *ar , dx,x0 ; int kk ; } floatvec ;
typedef struct { int nar ; double *ar , dx,x0 ; int kk ; } doublevec ;
#define KILL_floatvec(fv)                      \
  do{ if( (fv) != NULL ){                      \
        if( (fv)->ar != NULL ) free((fv)->ar); \
        free(fv); (fv) = NULL;                 \
  }} while(0)
#define KILL_doublevec KILL_floatvec

#define MAKE_floatvec(fv,n)                             \
  do{ (fv) = (floatvec *)malloc(sizeof(floatvec)) ;     \
      (fv)->nar = (n) ; (fv)->dx=1.0f; (fv)->x0=0.0f;   \
      (fv)->ar  = (float *)calloc(sizeof(float),(n)) ;  \
      (fv)->kk  = 0 ;                                   \
      if( (fv)->ar == NULL ) fprintf(stderr,"** ERROR: MAKE_floatvec malloc fails\n"); \
  } while(0)

#define MAKE_doublevec(dv,n)                              \
  do{ (dv) = (doublevec *)malloc(sizeof(doublevec)) ;     \
      (dv)->nar = (n) ; (dv)->dx=1.0; (dv)->x0=0.0;       \
      (dv)->ar  = (double *)calloc(sizeof(double),(n)) ;  \
      (dv)->kk  = 0 ;                                     \
      if( (dv)->ar == NULL ) fprintf(stderr,"** ERROR: MAKE_doublevec malloc fails\n"); \
  } while(0)

#define COPY_floatvec(ev,fv)                          \
 do{ int n = (fv)->nar ; MAKE_floatvec((ev),n) ;      \
     (ev)->dx = (fv)->dx ; (ev)->x0 = (fv)->x0 ;      \
     memcpy( (ev)->ar, (fv)->ar, sizeof(float)*n ) ;  \
     (ev)->kk = (fv)->kk ;                            \
 } while(0)

#define RESIZE_floatvec(fv,m)                                     \
  do{ if( (fv)->nar != (m) ){                                     \
        (fv)->nar = (m) ;                                         \
        (fv)->ar  = (float *)realloc((fv)->ar,sizeof(float)*(m)); \
        if( (fv)->ar == NULL ) fprintf(stderr,"** ERROR: RESIZE_floatvec malloc fails\n"); \
  }} while(0)

#define COPY_doublevec(ev,fv)                          \
 do{ int n = (fv)->nar ; MAKE_doublevec((ev),n) ;      \
     (ev)->dx = (fv)->dx ; (ev)->x0 = (fv)->x0 ;       \
     memcpy( (ev)->ar, (fv)->ar, sizeof(double)*n ) ;  \
     (ev)->kk = (fv)->kk ;                             \
 } while(0)

#define RESIZE_doublevec(fv,m)                                      \
  do{ if( (fv)->nar != (m) ){                                       \
        (fv)->nar = (m) ;                                           \
        (fv)->ar  = (double *)realloc((fv)->ar,sizeof(double)*(m)); \
        if( (fv)->ar == NULL ) fprintf(stderr,"** ERROR: RESIZE_doublevec malloc fails\n"); \
  }} while(0)

extern float  interp_floatvec ( floatvec  *fv , float  x ) ;
extern double interp_doublevec( doublevec *dv , double x ) ;
extern void mri_write_floatvec( char *fname , floatvec *fv ) ; /* 21 Jan 2016 */

extern float interp_inverse_floatvec( floatvec *fv , float y ) ;

typedef struct { int nvec ; floatvec *fvar ; } floatvecvec ;

extern MRI_IMAGE *mri_to_pval  ( MRI_IMAGE *im , int , float * ) ;
extern MRI_IMAGE *mri_to_zscore( MRI_IMAGE *im , int , float * ) ;
extern MRI_IMAGE *mri_to_qval( MRI_IMAGE * , floatvec * ) ; /* 01 Feb 2020 */

/*-----------------------------------------------------*/

extern floatvec * mri_polyfit_get_fitvec(void) ; /* 26 Feb 2019 */

/*-----------------------------------------------------*/

typedef struct { int nar ; int *ar ; } intvec ;
#define KILL_intvec(iv)                        \
  do{ if( (iv) != NULL ){                      \
        if( (iv)->ar != NULL ) free((iv)->ar); \
        free(iv); (iv) = NULL;                 \
  } } while(0)

typedef struct { int nvec ; intvec *ivar ; } intvecvec ;

#define MAKE_intvec(iv,n)                           \
  do{ (iv) = (intvec *)malloc(sizeof(intvec)) ;     \
      (iv)->nar = (n) ;                             \
      (iv)->ar  = (int *)calloc(sizeof(int),(n)) ;  \
      if( (iv)->ar == NULL ) fprintf(stderr,"** ERROR: MAKE_intvec malloc fails\n"); \
  } while(0)

#define RESIZE_intvec(iv,m)                                   \
  do{ if( (iv)->nar != (m) ){                                 \
        (iv)->nar = (m) ;                                     \
        (iv)->ar  = (int *)realloc((iv)->ar,sizeof(int)*(m)); \
        if( (iv)->ar == NULL ) fprintf(stderr,"** ERROR: RESIZE_intvec malloc fails\n"); \
  }} while(0)

#define APPEND_intvec(iv,jv)                                   \
  do{ int ni = (iv)->nar ;                                     \
      RESIZE_intvec((iv),ni+(jv)->nar) ;                       \
      memcpy( (iv)->ar+ni, (jv)->ar, sizeof(int)*(jv)->nar ) ; \
  } while(0)

/*--------------------------------------------------*/  /* 20 Jan 2016 */

typedef struct { int nar ; int64_t *ar ; } int64vec ;
#define KILL_int64vec(iv)                      \
  do{ if( (iv) != NULL ){                      \
        if( (iv)->ar != NULL ) free((iv)->ar); \
        free(iv); (iv) = NULL;                 \
  } } while(0)


#define MAKE_int64vec(iv,n)                                \
  do{ (iv) = (int64vec *)malloc(sizeof(int64vec)) ;        \
      (iv)->nar = (n) ;                                    \
      (iv)->ar  = (int64_t *)calloc(sizeof(int64_t),(n)) ; \
      if( (iv)->ar == NULL ) fprintf(stderr,"** ERROR: MAKE_int64vec malloc fails\n"); \
  } while(0)

/*--------------------------------------------------*/

typedef struct { int nar ; short *ar ; } shortvec ;
#define KILL_shortvec(iv)                    \
  do{ if( (iv) != NULL ){                     \
        if( (iv)->ar != NULL ) free((iv)->ar); \
        free(iv); (iv) = NULL;                  \
  } } while(0)

#define MAKE_shortvec(iv,n)                          \
  do{ (iv) = (shortvec *)malloc(sizeof(shortvec)) ;   \
      (iv)->nar = (n) ;                                \
      (iv)->ar  = (short *)calloc(sizeof(short),(n)) ;  \
      if( (iv)->ar == NULL ) fprintf(stderr,"** ERROR: MAKE_shortvec malloc fails\n"); \
  } while(0)

#define RESIZE_shortvec(iv,m)                                  \
  do{ if( (iv)->nar != (m) ){                                   \
        (iv)->nar = (m) ;                                        \
        (iv)->ar  = (short *)realloc((iv)->ar,sizeof(short)*(m)); \
        if( (iv)->ar == NULL ) fprintf(stderr,"** ERROR: RESIZE_shortvec malloc fails\n"); \
  }} while(0)

/*--------------------------------------------------*/
/* Jul 2010 */

typedef struct { int nar ; byte *ar ; } bytevec ;
#define KILL_bytevec(iv)                     \
  do{ if( (iv) != NULL ){                     \
        if( (iv)->ar != NULL ) free((iv)->ar); \
        free(iv); (iv) = NULL;                  \
  } } while(0)

#define MAKE_bytevec(iv,n)                         \
  do{ (iv) = (bytevec *)malloc(sizeof(bytevec)) ;   \
      (iv)->nar = (n) ;                              \
      (iv)->ar  = (byte *)calloc(sizeof(byte),(n)) ;  \
      if( (iv)->ar == NULL ) fprintf(stderr,"** ERROR: MAKE_bytevec malloc fails\n"); \
  } while(0)

#define RESIZE_bytevec(iv,m)                                 \
  do{ if( (iv)->nar != (m) ){                                 \
        (iv)->nar = (m) ;                                      \
        (iv)->ar  = (byte *)realloc((iv)->ar,sizeof(byte)*(m)); \
        if( (iv)->ar == NULL ) fprintf(stderr,"** ERROR: RESIZE_bytevec malloc fails\n"); \
  }} while(0)

/*--------------------------------------------------*/

typedef struct {
  int nbot, ntop , gbot ;
  char name[64] ;
} SYM_irange ;

extern floatvecvec * SYM_expand_ranges( int, int, SYM_irange *, char * );
extern int SYM_expand_errcount(void) ;                          /* 03 May 2007 */
extern char * SYM_test_gltsym( char *varlist , char *gltsym ) ; /* 01 May 2015 */

/*-----------------  30 Oct 1996: incorporation of cdflib ----------------*/
/*-----------------  09 May 2007: get them from nifticdf  ----------------*/
#ifndef __COMPILE_UNUSED_FUNCTIONS__
#define __COMPILE_UNUSED_FUNCTIONS__
#endif
#include "nifticdf.h"    /* was cdflib.h */
/*------------------------------------------------------------------------*/

/*-----------------  01 Feb 1998: incoroporation of mcw_glob -------------*/
#include "mcw_glob.h"
/*------------------------------------------------------------------------*/

/*-----------------  06 Dec 2004: incorporation of list_struct  ----------*/
#include "list_struct.h"

/*-----------------  02 Feb 1998:
                     incoroporation of 3ddata, 3dmaker, iochan -----------*/

#include "thd_iochan.h"
#include "3ddata.h"
#include "thd_maker.h"
#include "editvol.h"

#include "cs.h"            /* 17 Aug 1998 addition */

#include "multivector.h"   /* 18 May 1999 addition */

#include "afni_environ.h"  /* 07 Jun 1999 addition */

#include "r_new_resam_dset.h" /* 31 Jul 2007 */
#include "r_idisp.h"
#include "r_misc.h"

#include "rcmat.h"            /* 30 Dec 2008 */

#ifdef HAVE_ZLIB
#include <zlib.h>             /* 02 Mar 2009 */
#endif

#include "misc_math.h"        /* 21 Jun 2010 [rickr] */

#include "thd_atlas.h"        /* 22 Feb 2012 [rickr] */

THD_string_array * mri_read_1D_headerline( char *fname ) ; /* 18 May 2010 */

/* 09 Feb 2017: change the way thresholds are short-ified,
                along with changes in the relevant functions */

#define THRESH_SHORTIZE(ttt) \
  ( AFNI_yesenv("AFNI_OLD_SHORT_THRESH") ?  (float)SHORTIZE(ttt) : (ttt) )

/*------------------------------------------------------------------------*/
/* 13 Feb 2009: generic 4x4 matrix struct stuff */

typedef struct { double m[4][4] ; } dmat44 ;
extern dmat44 generic_dmat44_inverse    ( dmat44 P ) ;
extern double generic_dmat44_determinant( dmat44 P ) ;

/* apply a dmat44 matrix to a 4 vector (x,y,z,w) to produce (a,b,c,d) */

#undef  DMAT44_VEC
#define DMAT44_VEC(A,x,y,z,w,a,b,c,d)                                    \
 ( (a) = A.m[0][0]*(x) + A.m[0][1]*(y) + A.m[0][2]*(z) + A.m[0][3]*(w) , \
   (b) = A.m[1][0]*(x) + A.m[1][1]*(y) + A.m[1][2]*(z) + A.m[1][3]*(w) , \
   (c) = A.m[2][0]*(x) + A.m[2][1]*(y) + A.m[2][2]*(z) + A.m[2][3]*(w) , \
   (d) = A.m[3][0]*(x) + A.m[3][1]*(y) + A.m[3][2]*(z) + A.m[3][3]*(w)  )

/* print a dmat44 struct to stdout (with a string) */

#undef  DUMP_DMAT44
#define DUMP_DMAT44(SS,AA)                             \
    fprintf(stderr,                                    \
            "# dmat44 %s:\n"                           \
            " %13.6g %13.6g %13.6g %13.6g\n"           \
            " %13.6g %13.6g %13.6g %13.6g\n"           \
            " %13.6g %13.6g %13.6g %13.6g\n"           \
            " %13.6g %13.6g %13.6g %13.6g\n" ,         \
  SS, AA.m[0][0], AA.m[0][1], AA.m[0][2], AA.m[0][3],  \
      AA.m[1][0], AA.m[1][1], AA.m[1][2], AA.m[1][3],  \
      AA.m[2][0], AA.m[2][1], AA.m[2][2], AA.m[2][3],  \
      AA.m[3][0], AA.m[3][1], AA.m[3][2], AA.m[3][3] )

/* load elements of a dmat44 */

#undef  LOAD_DMAT44
#define LOAD_DMAT44(A,a11,a12,a13,a14,a21,a22,a23,a24,a31,a32,a33,a34,a41,a42,a43,a44) \
 ( A.m[0][0] = (a11), A.m[0][1] = (a12), A.m[0][2] = (a13), A.m[0][3] = (a14),         \
   A.m[1][0] = (a21), A.m[1][1] = (a22), A.m[1][2] = (a23), A.m[1][3] = (a24),         \
   A.m[2][0] = (a31), A.m[2][1] = (a32), A.m[2][2] = (a33), A.m[2][3] = (a34),         \
   A.m[3][0] = (a41), A.m[3][1] = (a42), A.m[3][2] = (a43), A.m[3][3] = (a44)  )

/*------------------------------------------------------------------------*/

extern MRI_IMAGE * mri_genARMA11( int nlen, int nvec, float ap, float lm, float sg ) ;
extern void mri_genARMA11_set_tdof( float ttt ) ;

/*------------------------------------------------------------------------*/
/* some of these clusterize prototypes require editvol.h */

typedef struct {
  int nvox ;
  float volume , xcm , ycm , zcm ;
  float          xpk , ypk , zpk ;
  float          xmi , ymi , zmi ;  /* 08 May 2019 */
} mri_cluster_detail ;

extern MRI_IMAGE * mri_clusterize( float,float, MRI_IMAGE * ,
                                   float,float, MRI_IMAGE * , int , byte * );
extern char * mri_clusterize_report(void) ;
extern MCW_cluster_array * mri_clusterize_array(int clear) ;
extern mri_cluster_detail mri_clusterize_detailize( MCW_cluster *cl, int icent);

extern MRI_IMAGE * mri_bi_clusterize( float rmm , float vmul , MRI_IMAGE *bim ,
                                      float thb , float tht  , MRI_IMAGE *tim ,
                                      byte *mask ) ;  /* 29 Jan 2015 */

extern void mri_fdr_setmask( byte *mmm ) ;                /* 27 Mar 2009 */
extern int mri_fdrize( MRI_IMAGE *, int, float *, int ) ; /* 17 Jan 2008 */
extern floatvec * mri_fdr_curve( MRI_IMAGE *, int , float * ) ;
extern floatvec * mri_fdr_getmdf(void) ;                  /* 22 Oct 2008 */

/*------------------------------------------------------------------------*/
/*--- Functions in mri_matrix.c (matrix operations, stored as images) ----*/

#ifdef  __cplusplus
extern "C" {                    /* care of Greg Balls    7 Aug 2006 [rickr] */
#endif

extern MRI_IMAGE * mri_matrix_mult     ( MRI_IMAGE *, MRI_IMAGE *);
extern MRI_IMAGE * mri_matrix_multranA ( MRI_IMAGE *, MRI_IMAGE *);
extern MRI_IMAGE * mri_matrix_multranB ( MRI_IMAGE *, MRI_IMAGE *);
extern MRI_IMAGE * mri_matrix_psinv    ( MRI_IMAGE *, float * , float );
extern void        mri_matrix_psinv_svd( int ) ;
extern MRI_IMAGE * mri_matrix_ortproj  ( MRI_IMAGE * , int ) ;
extern MRI_IMAGE * mri_matrix_sadd     ( float, MRI_IMAGE *, float, MRI_IMAGE * ) ;
extern MRI_IMAGE * mri_matrix_scale    ( float, MRI_IMAGE * ) ;
extern MRI_IMAGE * mri_matrix_evalrpn  ( char * ) ;
extern char      * mri_matrix_evalrpn_help(void) ;
extern void        mri_matrix_evalrpn_verb(int) ;
extern float mri_matrix_size( MRI_IMAGE * ) ;
extern MRI_IMARR * mri_matrix_psinv_ortproj( MRI_IMAGE *, int ) ; /* 13 Dec 2011 */

extern MRI_IMARR * mri_matrix_psinv_pair( MRI_IMAGE *, float ) ;
extern MRI_IMAGE * mri_matrix_singvals  ( MRI_IMAGE * ) ;

extern void mri_matrix_detrend( MRI_IMAGE *, MRI_IMAGE *, MRI_IMAGE * ) ;

#define            mri_matrix_transpose(x) mri_transpose(x)

extern double Plegendre( double x , int m ) ;

extern void mri_matrix_print( FILE *fp , MRI_IMAGE *ima , char *label ) ;

/*------------------------------------------------------------------------*/

extern MRI_IMAGE * THD_average_timeseries( MCW_cluster_array *, THD_3dim_dataset *) ;
extern MRI_IMAGE * THD_average_one_timeseries( MCW_cluster *, THD_3dim_dataset *) ;

/** mri_warp3D.c functions: 14 Apr 2003 */

extern MRI_IMAGE *mri_warp3D_cubic ( MRI_IMAGE *, int,int,int ,
                                     void func( float,float,float,
                                                float *,float *,float *) ) ;
extern MRI_IMAGE *mri_warp3D_linear( MRI_IMAGE *, int,int,int ,
                                     void func( float,float,float,
                                                float *,float *,float *) ) ;
extern MRI_IMAGE *mri_warp3D_NN    ( MRI_IMAGE *, int,int,int ,
                                     void func( float,float,float,
                                                float *,float *,float *) ) ;
extern MRI_IMAGE *mri_warp3D       ( MRI_IMAGE *, int,int,int ,
                                     void func( float,float,float,
                                                float *,float *,float *) ) ;
extern void mri_warp3D_method( int ) ;
extern void mri_warp3D_zerout( int ) ;

extern void mri_warp3D_set_womask( MRI_IMAGE * ) ;  /* 19 Nov 2004 */

extern MRI_IMAGE *mri_warp3D_quintic( MRI_IMAGE *, int,int,int ,
                                      void func( float,float,float,
                                                 float *,float *,float *) ) ;

extern MRI_IMAGE * mri_warp3D_affine( MRI_IMAGE * , THD_vecmat ) ;
extern MRI_IMAGE * mri_warp3D_resize( MRI_IMAGE *, int,int,int ) ;

extern double mri_entropy16( MRI_IMAGE * ) ;  /* 09 Jan 2004 */
extern double mri_entropy8 ( MRI_IMAGE * ) ;  /* 09 Jan 2004 */

extern float mri_scaled_diff( MRI_IMAGE *bim, MRI_IMAGE *nim, MRI_IMAGE *msk ) ;

#ifdef  __cplusplus
}
#endif

/*------------------------------------------------------------------*/

#include "AFNI_version.h"
#undef  PRINT_VERSION
#define PRINT_VERSION(pp)                                             \
 do{ if( !machdep_be_quiet() )                                        \
      INFO_message("%s: AFNI version=%s (" __DATE__ ") [%d-bit]",     \
                   (pp),AFNI_VERSION_LABEL,(int)(sizeof(void *)*8)) ; \
 } while(0)

#undef  PRINT_COMPILE_DATE
#if defined(AFNI_VERSION_LABEL) && defined(AFNI_VERSION_PLATFORM)
# define PRINT_COMPILE_DATE                     \
         printf("\n++ Compile date = " __DATE__ \
                " {%s:%s}\n\n",AFNI_VERSION_LABEL,AFNI_VERSION_PLATFORM)
#else
# define PRINT_COMPILE_DATE  \
         printf("\n++ Compile date = " __DATE__ "\n\n")
#endif

#undef  AUTHOR
#define AUTHOR(aa) \
 do{ if( !machdep_be_quiet() ) INFO_message("Authored by: %s",aa) ; } while(0)

#undef  WROTE_DSET_MSG
#define WROTE_DSET_MSG(dd,ss)                                        \
  do{ if( THD_is_file(DSET_BRIKNAME(dd)) && !machdep_be_quiet() )    \
       INFO_message("Output dataset %s {%s}",DSET_BRIKNAME(dd),(ss)); } while(0)

#undef  WROTE_DSET
#define WROTE_DSET(dd)                                                  \
  do{ if( !machdep_be_quiet() && THD_is_file(DSET_BRIKNAME(dd)) )       \
        INFO_message("Output dataset %s",DSET_BRIKNAME(dd)); } while(0)

#undef  WROTE_DSETI
#define WROTE_DSETI(dd)                                                  \
  do{ if( !machdep_be_quiet() && THD_is_file(DSET_BRIKNAME(dd)) )        \
        ININFO_message("Output dataset %s",DSET_BRIKNAME(dd)); } while(0)

#undef  CHECK_OPEN_ERROR
#define CHECK_OPEN_ERROR(dd,nn) \
 do{ if( !ISVALID_DSET(dd) ) ERROR_exit("Can't open dataset '%s'",nn); }while(0)

/* note that the following is a fatal error! */

#undef  CHECK_LOAD_ERROR
#define CHECK_LOAD_ERROR(dd)                                                   \
 do{ if( ISVALID_DSET(dd) && !DSET_LOADED(dd) )                                \
      ERROR_exit("Can't load dataset '%s': is it complete?",DSET_BRIKNAME(dd));\
 } while(0)


/*------------------------------------------------------------------*/

#define METRIC_KULL  0
#define METRIC_HELL  1
#define METRIC_TRIA  2
#define METRIC_JDIV  3
#define METRIC_JSDV  4
#define METRIC_XISQ  5
#define METRIC_XXSQ  6
#define METRIC_AGDV  7
extern void mri_metrics( MRI_IMAGE *, MRI_IMAGE *, float * ) ;

/*--------------------------------------------------------------------*/
/** July 2006: stuff for generic alignment functions: mri_genalign.c **/

#include "mri_warpfield.h"

  /* definition of various convex neighborhoods */

#define GA_BLOK_BALL 1  /* sphere */
#define GA_BLOK_CUBE 2  /* cube */
#define GA_BLOK_RHDD 3  /* rhombic dodecahedron */
#define GA_BLOK_TOHD 4  /* truncated octahedron */

#define GA_BLOK_STRING(b)  ( ((b)==GA_BLOK_BALL) ? "BALL" :          \
                             ((b)==GA_BLOK_CUBE) ? "CUBE" :          \
                             ((b)==GA_BLOK_RHDD) ? "RHDD" :          \
                             ((b)==GA_BLOK_TOHD) ? "TOHD" :          \
                                                            "UNKNOWN" )

 /* method codes for matching scalar-valued images */

#define GA_MATCH_PEARSON_SCALAR     1  /* least squares, more-or-less */
#define GA_MATCH_SPEARMAN_SCALAR    2  /* rank-order correlation */
#define GA_MATCH_KULLBACK_SCALAR    3  /* Mutual Info */
#define GA_MATCH_MUTINFO_SCALAR     3
#define GA_MATCH_CORRATIO_SCALAR    4  /* Correlation Ratio: Sym Mul */
#define GA_MATCH_NORMUTIN_SCALAR    5  /* Normalized Mutual Info */
#define GA_MATCH_JOINTENT_SCALAR    6  /* Joint Entropy */
#define GA_MATCH_HELLINGER_SCALAR   7  /* Hellinger metric */
#define GA_MATCH_CRAT_SADD_SCALAR   8  /* Correlation Ratio: Sym Add */
#define GA_MATCH_CRAT_USYM_SCALAR   9  /* Correlation Ratio: Unsym */

#define GA_MATCH_PEARSON_SIGNED    10  /* experimental */
#define GA_MATCH_PEARSON_LOCALS    11  /* experimental */
#define GA_MATCH_PEARSON_LOCALA    12  /* experimental */

#define GA_MATCH_LPC_MICHO_SCALAR  13  /* 24 Feb 2010 */
#define GA_MATCH_LPA_MICHO_SCALAR  14  /* 28 Nov 2018 */

#define GA_MATCH_NCDZLIB           15  /* very experimental */

#define GA_MATCH_PEARCLP_SCALAR    16

#define GA_MATCH_METHNUM_SCALAR    14  /* Largest useful value in sequence above */

 /* methods for smoothing images */

#define GA_SMOOTH_GAUSSIAN          1
#define GA_SMOOTH_MEDIAN            2

 /* kernels for histogram estimation */

#define GA_KERNEL_GAUSSIAN          1
#define GA_KERNEL_QUADRATIC         2
#define GA_KERNEL_QUARTIC           3

 /* prototype/typedef for a spatial warping function */

typedef void GA_warpfunc( int, float *,
                          int, float *,float *,float *,
                               float *,float *,float * );

typedef MRI_warp3D_param_def GA_param ;  /* cf. 3ddata.h */

#define GA_HIST_EQWIDE 1
#define GA_HIST_EQHIGH 2
#define GA_HIST_CLEQWD 3

/***** struct and macro for local statistics in BLOKs (e.g., LPC) *****/

typedef struct { int num , *nelm , **elm ; } GA_BLOK_set ;

/** delete a GA_BLOK_set struct and its contents **/

#define GA_BLOK_KILL(gbs)                                     \
 do{ int ee ;                                                 \
     if( (gbs)->nelm != NULL ) free((gbs)->nelm) ;            \
     if( (gbs)->elm != NULL ){                                \
       for( ee=0 ; ee < (gbs)->num ; ee++ )                   \
         if( (gbs)->elm[ee] != NULL ) free((gbs)->elm[ee]) ;  \
       free((gbs)->elm) ;                                     \
     }                                                        \
     free((gbs)) ;                                            \
 } while(0)

/** create a GA_BLOK_set; cf. mri_genalign_util.c **/

extern GA_BLOK_set * create_GA_BLOK_set( int   nx , int   ny , int   nz ,
                                         float dx , float dy , float dz ,
                                         int npt, float *im, float *jm, float *km,
                                         int bloktype, float blokrad, int minel,
                                                       float shfac  , int verb ) ;

/** compute correlations in each blok **/

extern floatvec * GA_pearson_vector( GA_BLOK_set *, float *, float *, float * );

extern void GA_pearson_ignore_zero_voxels(int) ; /* 23 Feb 2010 */

/******* end of BLOK-ization stuff here -- also see mri_genalign_util.c *******/

extern float total_rotation_degrees( float ax, float ay, float az ) ; /* 02 Jan 2019 */

 /* struct to control mri_genalign.c optimization */

typedef struct {
  int match_code  ;             /* set by user */
  int smooth_code ;             /* set by user */
  float smooth_radius_base ;    /* set by user */
  float smooth_radius_targ ;    /* set by user */
  int interp_code ;             /* set by user */
  mat44 base_cmat , targ_cmat ; /* set by user */
  mat44 base_imat , targ_imat ;
  float base_di,base_dj,base_dk ;
  float targ_di,targ_dj,targ_dk ;
  int usetemp ;                 /* set by user */

  int   bloktype, blokmin ;     /* set by user */
  float blokrad ;               /* set by user */
  GA_BLOK_set *blokset ;

  int old_sc ; float old_sr_base , old_sr_targ ;

  MRI_IMAGE *bsim , *bsims , *bsmask ;
  float bsbot,bstop , bsclip ;
  int dim_bvec    ;
  int   nmask     ;
  int   nvox_mask ;
  int   nbsmask   ;
  byte *bmask     ;
  MRI_IMAGE *bwght ;

  MRI_IMAGE *ajim , *ajims , *ajmask , *ajimor;
  float ajbot,ajtop , ajclip , aj_ubot,aj_usiz ;
  int dim_avec , abdim , najmask ;
  int ajmask_ranfill ;

  int npt_match   ;            /* set by user */
  floatvec *im, *jm, *km , *bvm , *wvm ;
  float bvstat ;
  int hist_mode ;              /* set by user */
  float hist_param ;           /* set by user */
  int need_hist_setup ;

  int   ccount_do   , ccount_val  ;  /* 22 Feb 2010 */
  float ccount_bthr , ccount_athr ;

#if 0
                             /*** NOT USED YET ***/
  int kernel_code ;            /* set by user */
  float kernel_radius ;        /* set by user */
  int npt_sum ;                /* set by user */
  intvec *is, *js, *ks ;
  floatvec *bvs ;            /********************/
#endif

  int          wfunc_numpar ;  /* set by user */
  GA_param    *wfunc_param ;   /* set by user */
  GA_warpfunc *wfunc ;         /* set by user */
  int          wfunc_numfree ;
  int         *wfunc_pma ;
  int          wfunc_ntrial ;

  int          setup ;
  float        vbest ;
} GA_setup ;

#undef  IFREE
#define IFREE(x) do{ if((x)!=NULL)free(x); (x)=NULL; }while(0)

#undef  FREE_GA_setup
#define FREE_GA_setup(st)                                                   \
 do{ if( (st) != NULL ){                                                    \
       mri_free((st)->bsim); mri_free((st)->ajim); IFREE((st)->bmask);      \
       mri_free((st)->bsims);mri_free((st)->ajims);mri_free((st)->bwght);   \
       KILL_floatvec((st)->im); KILL_floatvec((st)->jm);                    \
       KILL_floatvec((st)->km); KILL_floatvec((st)->bvm);                   \
       KILL_floatvec((st)->wvm); IFREE((st)->wfunc_param) ;                 \
       mri_free((st)->ajmask); mri_free((st)->ajimor);                      \
       mri_free((st)->bsmask);                                              \
     }                                                                      \
 } while(0)

#define GA_LEGENDRE 1
#define GA_HERMITE  2

extern void GA_setup_polywarp(int) ;

extern void mri_genalign_scalar_setup( MRI_IMAGE *, MRI_IMAGE *,
                                       MRI_IMAGE *, GA_setup  * ) ;
extern int mri_genalign_scalar_optim( GA_setup *, double, double, int) ;
extern void mri_genalign_scalar_ransetup( GA_setup *, int ) ;
extern void mri_genalign_affine( int, float *,
                                 int, float *, float *, float *,
                                      float *, float *, float * ) ;
extern MRI_IMAGE * mri_genalign_scalar_warpim( GA_setup * ) ;
extern void mri_genalign_verbose(int) ;
extern void mri_genalign_mat44( int, float *,
                                int, float *, float *, float *,
                                     float *, float *, float * ) ;
extern void mri_genalign_set_pgmat( int ) ;

extern void mri_genalign_bilinear( int, float *,
                                   int, float *, float *, float *,
                                        float *, float *, float * ) ;

extern void mri_genalign_cubic( int, float *,
                                int, float *, float *, float *,
                                     float *, float *, float * ) ;
extern void mri_genalign_quintic( int, float *,
                                  int, float *, float *, float *,
                                       float *, float *, float * ) ;
extern void mri_genalign_heptic( int, float *,
                                 int, float *, float *, float *,
                                      float *, float *, float * ) ;
extern void mri_genalign_nonic( int, float *,
                                int, float *, float *, float *,
                                     float *, float *, float * ) ;

extern int    GA_polywarp_coordcode( int pnum ) ; /* 06 Dec 2010 */
extern char * GA_polywarp_funcname ( int pnum ) ; /* 09 Dec 2010 */

void mri_genalign_set_targmask( MRI_IMAGE *, GA_setup * ) ; /* 07 Aug 2007 */
void mri_genalign_set_basemask( MRI_IMAGE *, GA_setup * ) ; /* 25 Feb 2010 */

extern void GA_reset_fit_callback( void (*fc)(int,double*) ) ;
extern void GA_do_dots(int) ;
extern void GA_do_cost(int, byte) ;
extern void GA_do_params(int) ;
extern float mri_genalign_scalar_cost( GA_setup * , float *) ;
extern void GA_set_outval( float ) ;
extern float GA_get_outval(void) ;
extern void GA_allow_ccount( int ) ; /* 22 Feb 2010 */
extern void GA_setup_micho( double,double,double,double,double ) ; /* 24 Feb 2010 */
extern void GA_set_nperval( int ) ; /* 15 Nov 2010 */

/**------ these functions are now in mri_genalign_util.c [10 Dec 2008] ------**/

extern void GA_interp_NN     ( MRI_IMAGE *fim , int npp,
                               float *ip, float *jp, float *kp, float *vv ) ;
extern void GA_interp_linear ( MRI_IMAGE *fim , int npp,
                               float *ip, float *jp, float *kp, float *vv ) ;
extern void GA_interp_cubic  ( MRI_IMAGE *fim , int npp,
                               float *ip, float *jp, float *kp, float *vv ) ;
extern void GA_interp_quintic( MRI_IMAGE *fim , int npp,
                               float *ip, float *jp, float *kp, float *vv ) ;
extern void GA_interp_varp1  ( MRI_IMAGE *fim , int npp,
                               float *ip, float *jp, float *kp, float *vv ) ;
extern void GA_interp_wsinc5 ( MRI_IMAGE *fim , int npp,
                               float *ip, float *jp, float *kp, float *vv ) ;
extern void GA_interp_wsinc5_2D( MRI_IMAGE *fim ,
                                 int npp, float *ip, float *jp, float *vv ) ;
extern int GA_gcd(int,int) ;
extern int GA_find_relprime_fixed(int) ;
extern MRI_IMAGE * GA_smooth( MRI_IMAGE *im, int meth, float rad ) ;

extern MRI_IMAGE * GA_indexwarp( MRI_IMAGE *, int, MRI_IMAGE * ) ;
extern MRI_IMAGE * GA_indexwarp_plus( MRI_IMAGE *, int, MRI_IMAGE *,
                                      float_triple , byte * ) ;
extern void GA_affine_edit_warp( mat44 aff , MRI_IMAGE *wpim ) ;
/*----------------------------------------------------------------------------*/

extern floatvec * mri_genalign_scalar_allcosts( GA_setup * , float * ); /* 19 Sep 2007 */

#define MATORDER_SDU  1  /* matrix multiplication order: */
#define MATORDER_SUD  2  /* S = shear matrix             */
#define MATORDER_DSU  3  /* D = diagonal scaling matrix  */
#define MATORDER_DUS  4  /* U = rotation matrix          */
#define MATORDER_USD  5
#define MATORDER_UDS  6

#define SMAT_UPPER    1  /* shear matrix is upper */
#define SMAT_LOWER    2  /* or lower triangular  */
#define SMAT_XXX      3  /* x-axis only shears  */
#define SMAT_YYY      4  /* y-axis only shears  */
#define SMAT_ZZZ      5  /* z-axis only shears  */

extern void mri_genalign_affine_setup( int,int,int ) ;
extern void mri_genalign_affine_set_befafter( mat44 *, mat44 * ) ;
extern void mri_genalign_affine_get_befafter( mat44 *, mat44 * ) ;
extern void mri_genalign_affine_get_gammaijk( mat44 * ) ; /* 04 Apr 2007 */
extern void mri_genalign_affine_get_gammaxyz( mat44 * ) ;

void mri_genalign_affine_use_befafter(int,int) ; /* 10 Dec 2010 */

extern MRI_IMAGE * mri_genalign_scalar_warpone(      /* 26 Sep 2006 */
                    int npar, float *wpar, GA_warpfunc *wfunc,
                    MRI_IMAGE *imtarg ,
                    int nnx , int nny , int nnz , int icode ) ;

extern MRI_IMARR * mri_genalign_scalar_xyzwarp(      /* 10 Dec 2010 */
                    int npar, float *wpar, GA_warpfunc *wfunc,
                    int nnx , int nny , int nnz ) ;


extern void mri_genalign_scalar_clrwght( GA_setup * ) ;  /* 18 Oct 2006 */

extern THD_fvec3 mri_estimate_FWHM_1dif( MRI_IMAGE * , byte * ) ;
extern MRI_IMAGE * THD_estimate_FWHM_all( THD_3dim_dataset *, byte *, int,int ) ;
extern void FHWM_1dif_dontcheckplus( int ) ;
extern THD_fvec3 mriarr_estimate_FWHM_1dif( MRI_IMARR *, byte * , int ) ;


extern THD_fvec3 mri_estimate_FWHM_12dif( MRI_IMAGE * , byte * ) ;
extern THD_fvec3 mri_estimate_FWHM_12dif_MAD( MRI_IMAGE * , byte * ) ; /* 24 Mar 2010 */

extern THD_fvec3 mri_FWHM_1dif_mom12( MRI_IMAGE * , byte * ) ; /* 11 Aug 2015 */

extern MCW_cluster * THD_estimate_ACF( THD_3dim_dataset *dset,
                                       byte *mask, int demed, int unif, float radius ) ;
extern float_quad ACF_cluster_to_modelE( MCW_cluster *acf, float dx, float dy, float dz ) ;
extern MRI_IMAGE * ACF_get_1D(void) ;
extern float mriarr_estimate_FWHM_acf( MRI_IMARR *imar, byte *mask, int unif, float radius ) ;

void set_ACF_2D( int nn ) ; /* 25 Oct 2018 */

void mri_fwhm_setfester( THD_fvec3 (*func)(MRI_IMAGE *, byte *) ) ;

extern float mri_nstat  ( int , int , float * , float, MCW_cluster *) ;  /* 19 Aug 2005 */
extern THD_fvec3 mri_nstat_fwhmxyz( int,int,int ,
                                    MRI_IMAGE *, byte *, MCW_cluster * );

extern int mri_nstat_mMP2S( int npt , float *far , float voxval, float *fv5);
extern int mri_nstat_diffs( int npt , float *far , float *fv5, int doabs);
extern void mri_blur3D_variable( MRI_IMAGE * , byte * ,
                                 MRI_IMAGE * , MRI_IMAGE * , MRI_IMAGE * ) ;
extern void mri_blur3D_inmask( MRI_IMAGE *, byte *, float,float,float,int );
extern void mri_blur3D_inmask_speedy( MRI_IMAGE *, byte *,
                                      float,float,float,int );
extern void mri_blur3D_addfwhm( MRI_IMAGE *, byte *, float ) ;
extern void mri_blur3D_addfwhm_speedy( MRI_IMAGE *, byte *, float ) ;
extern void mri_blur3D_inmask_NN( MRI_IMAGE *im, byte *mask, int  ) ;
extern void mri_blur3D_getfac ( float, float, float, float,
                                int *, float *, float *, float * ) ;

extern MRI_IMAGE * mri_rgb_blur2D  ( float sig , MRI_IMAGE *im ) ;
extern MRI_IMAGE * mri_byte_blur2D( float sig , MRI_IMAGE *im );
extern MRI_IMAGE * mri_float_blur2D( float sig , MRI_IMAGE *im ) ;
extern MRI_IMAGE * mri_float_blur3D( float sig , MRI_IMAGE *im ) ;

void *Percentate (void *vec, byte *mm, int nxyz,
                  int type, double *mpv, int N_mp,
                  int option, double *perc ,
                  int zero_flag, int positive_flag, int negative_flag);

/*----------------------------------------------------------------------------*/
/* RBF stuff -- cf. mri_rbfinterp.c -- 05 Feb 2009 */

typedef unsigned short RBFKINT ;
#define RBFKINT_MAX 65535u

typedef struct {
  int nknot ;                    /* number of knots */
  float rad  , rqq ;             /* RBF radius and radius squared */
  float xmid , ymid , zmid ;     /* middle of the knots */
  float xscl , yscl , zscl ;     /* scale reciprocal of the knots */
  float *xknot, *yknot, *zknot ; /* each is an nknot-long vector */
  dmat44 Qmat ;                  /* 4x4 Q matrix for linear coefficents */
  rcmat *Lmat ;                  /* Choleski factor of M matrix */
  int uselin ;                   /* using linear coefficients? */
  float *P0, *Px , *Py , *Pz ;   /* each is an nknot-long vector */
} RBF_knots ;

#undef  DESTROY_RBF_knots
#define DESTROY_RBF_knots(rk)                                            \
 do{ IFREE((rk)->xknot); IFREE((rk)->yknot); IFREE((rk)->zknot);         \
     IFREE((rk)->P0); IFREE((rk)->Px); IFREE((rk)->Py); IFREE((rk)->Pz); \
     rcmat_destroy((rk)->Lmat); free(rk);                                \
 } while(0)

typedef struct {
  int npt ;                   /* number of grid points */
  float *xpt , *ypt , *zpt ;  /* grid points on which to evaluate RBF */
  RBFKINT *kfirst , *klast ;  /* first & last knot indexes for each grid pt */
} RBF_evalgrid ;

#undef  MAKE_RBF_evalgrid
#define MAKE_RBF_evalgrid(rg,nn)                             \
 do{ (rg) = (RBF_evalgrid *)malloc(sizeof(RBF_evalgrid)) ;   \
     (rg)->npt = (nn) ;                                      \
     (rg)->xpt = (float *)calloc(sizeof(float),(nn)) ;       \
     (rg)->ypt = (float *)calloc(sizeof(float),(nn)) ;       \
     (rg)->zpt = (float *)calloc(sizeof(float),(nn)) ;       \
     (rg)->kfirst = (rg)->klast = NULL ;                     \
 } while(0)

#undef  DESTROY_RBF_evalgrid
#define DESTROY_RBF_evalgrid(rg)                             \
 do{ free((rg)->xpt); free((rg)->ypt); free((rg)->zpt);      \
     if( (rg)->klast != NULL ) free((rg)->klast ) ;          \
     if( (rg)->kfirst!= NULL ) free((rg)->kfirst) ;          \
     free(rg) ;                                              \
 } while(0)

typedef struct {
  int code ;                /* 0 ==> val has func values; >0 ==> knot wts */
  float b0 , bx , by , bz ; /* linear polynomial coefficients */
  float *val ;              /* nknot of these */
} RBF_evalues ;

#undef  MAKE_RBF_evalues
#define MAKE_RBF_evalues(rv,nn)                               \
 do{ (rv) = (RBF_evalues *)calloc(1,sizeof(RBF_evalues)) ;    \
     (rv)->code = 0 ;                                         \
     (rv)->val  = (float *)malloc(sizeof(float)*(nn)) ; } while(0)

#undef  DESTROY_RBF_evalues
#define DESTROY_RBF_evalues(rv) do{ free((rv)->val); free(rv); } while(0)

extern RBF_knots * RBF_setup_knots( int, float, int, float *, float *, float * ) ;
extern int RBF_setup_evalues( RBF_knots *rbk, RBF_evalues *rbe ) ;
extern int RBF_evaluate( RBF_knots *, RBF_evalues *, RBF_evalgrid *, float * ) ;
extern void RBF_set_verbosity( int ) ;
extern void RBF_setup_kranges( RBF_knots *rbk , RBF_evalgrid *rbg ) ;

/*----------------------------------------------------------------------------*/
/** Test if a image is vector-valued (fvect, rgb, rgba, or complex) **/

#undef  ISVECTIM
#define ISVECTIM(tim) ((tim)->kind==MRI_fvect || (tim)->kind==MRI_rgb ||   \
                       (tim)->kind==MRI_rgba  || (tim)->kind==MRI_complex)

/** Vectorize a call to an image producing function that takes as input
    1 float image and produces as output 1 float image.  To use this macro,
    first #define the CALLME macro which takes 2 arguments, the input image
    pointer and the output image name.  For example, to median filter a
    possible vector image named 'inim' into the output 'outim' do

 #undef  CALLME
 #define CALLME(inn,out) (out) = mri_medianfilter( (inn), irad,mask,verb )
 if( ISVECTIM(inim) ){ VECTORME(inim,outim) ; return outim ; }
 ... normal processing of a scalar image goes here
 ... idea is that the CALLME macro is recursive to the local function
 ... this example would be inserted into source code mri_medianfilter.c **/

#undef  VECTORME
#define VECTORME(inpp,outp)                                                   \
 do{ int vv ; MRI_IMARR *qxmpq=NULL; MRI_IMAGE *qxm=NULL;                     \
     (outp) = NULL ;                                                          \
     switch( (inpp)->kind ){                                                  \
       default:                                             break ;           \
       case MRI_fvect:   qxmpq = mri_fvect_to_imarr(inpp) ; break ;           \
       case MRI_rgb:     qxmpq = mri_rgb_to_3float (inpp) ; break ;           \
       case MRI_rgba:    qxmpq = mri_rgba_to_4float (inpp); break ;           \
       case MRI_complex: qxmpq = mri_complex_to_pair(inpp); break ;           \
     }                                                                        \
     if( qxmpq == NULL ) break ;                                              \
     for( vv=0 ; vv < IMARR_COUNT(qxmpq) ; vv++ ){                            \
       CALLME( IMARR_SUBIM(qxmpq,vv) , qxm ) ;                                \
       mri_free(IMARR_SUBIM(qxmpq,vv)) ;                                      \
       IMARR_SUBIM(qxmpq,vv) = qxm ;                                          \
     }                                                                        \
     switch( (inpp)->kind ){                                                  \
       default:          break ;                                              \
       case MRI_fvect:   (outp) = mri_imarr_to_fvect(qxmpq) ;                 \
                         break ;                                              \
       case MRI_rgb:     (outp) = mri_3to_rgb(IMARR_SUBIM(qxmpq,0),           \
                                              IMARR_SUBIM(qxmpq,1),           \
                                              IMARR_SUBIM(qxmpq,2) ) ;        \
                         break ;                                              \
       case MRI_rgba:    (outp) = mri_4to_rgba(IMARR_SUBIM(qxmpq,0),          \
                                               IMARR_SUBIM(qxmpq,1),          \
                                               IMARR_SUBIM(qxmpq,2),          \
                                               IMARR_SUBIM(qxmpq,3) ) ;       \
                         break ;                                              \
       case MRI_complex: (outp) = mri_pair_to_complex(IMARR_SUBIM(qxmpq,0),   \
                                                      IMARR_SUBIM(qxmpq,1) ); \
                         break ;                                              \
     }                                                                        \
     DESTROY_IMARR(qxmpq) ;                                                   \
   } while(0)
/*----------------------------------------------------------------------------*/

extern THD_3dim_dataset * THD_svdblur( THD_3dim_dataset *inset, byte *mask,
                                float rad, int pdim, int nort, float **ort ) ;
extern MRI_IMARR * THD_get_dset_nbhd_array( THD_3dim_dataset *dset, byte *mask,
                                            int xx, int yy, int zz, MCW_cluster *nbhd ) ;
extern MRI_IMAGE * mri_svdproj( MRI_IMARR *imar , int nev ) ;
extern MRI_IMAGE * mri_first_principal_vector( MRI_IMARR *imar ) ;
extern int mri_principal_vectors( MRI_IMARR *imar, int nvec, float *sval, float *uvec ) ;

/*----------------------------------------------------------------------------*/
/* for mri_nwarp.c */

typedef struct {
  int    nx ,  ny ,  nz ;
  float *xd , *yd , *zd , *hv , *je , *se ;
  int   use_es ;
  float es_xd_xp, es_xd_xm, es_xd_yp, es_xd_ym, es_xd_zp, es_xd_zm,
        es_yd_xp, es_yd_xm, es_yd_yp, es_yd_ym, es_yd_zp, es_yd_zm,
        es_zd_xp, es_zd_xm, es_zd_yp, es_zd_ym, es_zd_zp, es_zd_zm ;
   /* stuff below here is for conversion to/from 3D dataset format */
  mat44 cmat , imat ;      /* cmat: i->x ; imat: x->i */
  char *geomstring ;
  int view ;
} IndexWarp3D ;

typedef struct {
  int nwarp ;
  IndexWarp3D **warp ;
} IndexWarp3DArray ;

typedef struct {
  MRI_IMAGE *im ;
  IndexWarp3D *warp ;
} Image_plus_Warp ;

typedef struct {
  IndexWarp3D *fwarp ;
  IndexWarp3D *iwarp ;
} IndexWarp3D_pair ;

typedef struct {
  mat44 fwarp ;
  mat44 iwarp ;
} mat44_pair ;

typedef struct { /* 17 Oct 2014 */
  int   nmar ;
  char  fname[128] ;
  mat44 *mar ;
} mat44_vec ;

#define M44V_mat(mmm,iii) ( ((iii) < (mmm)->nmar) ? (mmm)->mar[iii]             \
                                                  : (mmm)->mar[(mmm)->nmar-1] )

#define DESTROY_mat44_vec(mv)                  \
 do{ if( (mv)->mar != NULL ) free((mv)->mar) ; \
     free(mv) ;                                \
 } while(0) ;

typedef struct { /* 17 Oct 2014 */
  int ncat , nvar , flags ;
  THD_3dim_dataset **nwarp ;
  mat44_vec        **awarp ;
  char              *actual_geomstring ;
  char              *master_geomstring ;
  mat44              actual_cmat , actual_imat ;
  int              xpad  ,ypad  ,zpad ;
  float            xshift,yshift,zshift ;
} Nwarp_catlist ;

#define NWC_INVERT_MASK 1  /* for flags field */

#define NWC_nwarp(nnn,iii) ( ((nnn)->nwarp != NULL) ? (nnn)->nwarp[iii] : NULL )
#define NWC_awarp(nnn,iii) ( ((nnn)->awarp != NULL) ? (nnn)->awarp[iii] : NULL )
#define NWC_null(nnn,iii)  ( NWC_nwarp(nnn,iii)==NULL && NWC_awarp(nnn,iii)==NULL )

extern THD_3dim_dataset * IW3D_from_nwarp_catlist( Nwarp_catlist * , int ) ;
extern void IW3D_destroy_nwarp_catlist( Nwarp_catlist * ) ;
extern int IW3D_reduce_nwarp_catlist( Nwarp_catlist * ) ;
extern Nwarp_catlist * IW3D_read_nwarp_catlist( char * ) ;

extern IndexWarp3D * IW3D_create( int nx , int ny , int nz ) ;
extern void IW3D_destroy( IndexWarp3D *AA ) ;
extern float IW3D_normL1  ( IndexWarp3D *AA , IndexWarp3D *BB ) ;
extern float IW3D_normL2  ( IndexWarp3D *AA , IndexWarp3D *BB ) ;
extern float IW3D_normLinf( IndexWarp3D *AA , IndexWarp3D *BB ) ;
extern IndexWarp3D * IW3D_empty_copy( IndexWarp3D *AA ) ;
extern IndexWarp3D * IW3D_copy( IndexWarp3D *AA , float fac ) ;
extern IndexWarp3D * IW3D_sum( IndexWarp3D *AA, float Afac, IndexWarp3D *BB, float Bfac ) ;
extern void IW3D_scale( IndexWarp3D *AA , float fac ) ;
extern IndexWarp3D * IW3D_from_dataset( THD_3dim_dataset *dset , int empty , int ivs ) ;
extern THD_3dim_dataset * IW3D_to_dataset( IndexWarp3D *AA , char *prefix ) ;
extern float IW3D_load_hexvol( IndexWarp3D *AA , float *hv ) ;
extern float IW3D_load_energy( IndexWarp3D *AA ) ;
extern void IW3D_load_bsv( IndexWarp3D *AA , float,float,float, float *bb , float *ss , float *vv ) ;
extern IndexWarp3D * IW3D_compose( IndexWarp3D *AA , IndexWarp3D *BB     , int icode ) ;
extern IndexWarp3D * IW3D_invert ( IndexWarp3D *AA , IndexWarp3D *BBinit , int icode ) ;
extern IndexWarp3D * IW3D_sqrtinv( IndexWarp3D *AA , IndexWarp3D *BBinit , int icode ) ;
extern IndexWarp3D * IW3D_from_poly( int npar, float *par, IndexWarp3D *WW ) ;
extern THD_3dim_dataset * NwarpCalcRPN( char *expr, char *prefix, int icode, int acode ) ;
extern void NwarpCalcRPN_verb(int i) ;

extern void THD_interp_floatim( MRI_IMAGE *fim ,
                                int np , float *ip , float *jp , float *kp ,
                                int code, float *outar ) ;
extern void THD_interp_complexim( MRI_IMAGE *fim ,
                                  int np , float *ip , float *jp , float *kp ,
                                  int code, complex *outar ) ; /* 27 Mar 2018 */
extern MRI_IMARR * THD_setup_nwarp( MRI_IMARR *bimar,
                                    int use_amat    , mat44 amat ,
                                    mat44 cmat_bim  ,
                                    int incode      , float wfac ,
                                    mat44 cmat_src  ,
                                    mat44 cmat_out  ,
                                    int nx_out      , int ny_out , int nz_out  ) ;
extern THD_3dim_dataset * THD_nwarp_dataset( THD_3dim_dataset *dset_nwarp ,
                                             THD_3dim_dataset *dset_src  ,
                                             THD_3dim_dataset *dset_mast ,
                                             char *prefix , int wincode , int dincode ,
                                             float dxyz_mast , float wfac , int nvlim ,
                                             MRI_IMAGE *amatim ) ;

extern THD_3dim_dataset * THD_nwarp_dataset_NEW( Nwarp_catlist    *nwc       ,
                                                 THD_3dim_dataset *dset_src  ,
                                                 THD_3dim_dataset *dset_mast ,
                                                 char *prefix, int wincode, int dincode,
                                                 float dxyz_mast, float wfac, int nvlim ) ;

extern int THD_nwarp_forward_xyz( THD_3dim_dataset *dset_nwarp ,
                                  float dfac , int npt ,
                                  float *xin , float *yin , float *zin ,
                                  float *xut , float *yut , float *zut  ) ;

extern int THD_nwarp_inverse_xyz( THD_3dim_dataset *dset_nwarp ,
                                  float dfac , int npt ,
                                  float *xin , float *yin , float *zin ,
                                  float *xut , float *yut , float *zut  ) ;
/*----------------------------------------------------------------------------*/
/* Aug 2018 - sound stuff - cs_playsound.c */

extern void play_sound_1D( int nn , float *xx ) ;
extern void mri_play_sound( MRI_IMAGE *im , int ignore ) ;
extern void set_sound_note_type( char *typ ) ;
extern void set_sound_gain_value( int ggg ) ;
extern void set_sound_twotone( int ggg ) ;      /* do not use this */
extern char * get_sound_player(void) ;
extern void sound_set_note_ADSR(int) ;

extern void sound_write_au_header( FILE *fp, int nn, int srate, int code ) ;
extern void sound_write_au_ulaw( char *fname, int nn, float *aa, int srate, float scl ) ;
extern void sound_write_au_8PCM( char *fname, int nn, float *aa, int srate, float scl ) ;

extern void sound_write_au_16PCM( char *fname, int nn, float *aa, int srate, float scl ) ;

extern MRI_IMAGE * mri_sound_1D_to_FM( MRI_IMAGE *imin,
                                       float fbot, float ftop, int srate, int nsper ) ;

extern void kill_sound_players(void) ;

extern void mri_sound_play_append( char *app ) ;  /* 09 Aug 2019 */
extern void mri_play_sound_notify( int ) ;
extern void mri_play_sound_rate_fac( float fff ) ;     /* 23 Aug 2019 */

#define SOUND_WAVEFORM_SINE     1
#define SOUND_WAVEFORM_SQUARE   2
#define SOUND_WAVEFORM_TRIANGLE 3
#define SOUND_WAVEFORM_H2SINE   4
#define SOUND_WAVEFORM_SQSINE   5

#define SOUND_WAVECODE_BASE     1048576.0f

extern void sound_set_note_waveform( int nn ) ;
extern void sound_make_note( float frq, int waveform, int srate, int nsam, float *sam ) ;
extern MRI_IMAGE * mri_sound_1D_to_notes( MRI_IMAGE *imin, int srate, int nsper,
                                          int ny, int ignore , int use_wavecodes ) ;

/*----------------------------------------------------------------------------*/

#define CPU_IS_64_BIT() ((sizeof(void *) == 8) ? 1 : 0 )

#endif /* _MCW_MRILIB_HEADER_ */
