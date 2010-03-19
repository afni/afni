
#define MRILIB_7D

#include <string.h>
#include <stdlib.h>
#include <math.h>
#include <stdio.h>
#include <ctype.h>
#include <sys/stat.h>

#undef  AFREALL
#define AFREALL(v,typ,siz) (typ*) realloc((void*)v,sizeof(typ)*(siz))

#undef  EXIT
#define EXIT(x) exit(x)

#ifdef  __cplusplus
extern "C" {                    /* care of Greg Balls    7 Aug 2006 [rickr] */
#endif

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

extern int   assume_dicom_mosaic ;   /* mri_read_dicom.c  13 Mar 2006 [rickr] */

#ifdef  __cplusplus
}
#endif

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
         MRI_byte , MRI_short  , MRI_int  ,
        MRI_float , MRI_double , MRI_complex , MRI_rgb , MRI_rgba } MRI_TYPE ;

#define MRI_KIND MRI_TYPE ;   /* to alleviate stupidity */
#define MRI_type MRI_TYPE ;
#define MRI_kind MRI_TYPE ;

#define MRI_rgbyte MRI_rgb

/*! The last MRI_TYPE yet defined. */

#define LAST_MRI_TYPE 7

/*! String names for MRI_TYPE. */

static char * MRI_TYPE_name[8] =
  { "byte", "short", "int", "float", "double", "complex", "rgb", "RGBA" } ;

#define MRI_type_name MRI_TYPE_name  /* because I forget */

#define MRI_TYPE_NAME(iimm) MRI_TYPE_name[(iimm)->kind]  /* 26 Apr 2005 */

/*! Max value of a byte. */

#define MRI_maxbyte         255

/*! Max value of a short. */

#define MRI_maxshort      32767

/*! Max value of an int. */

#define MRI_maxint   2147483647

/*! Max values for various types, if they have them. */

static float MRI_TYPE_maxval[7] =
  { 255.0 , 32767.0 , 2147483647.0 , 0.0,0.0,0.0 , 255.0 } ;

/*! Force a float into a short. */

#define SHORTIZE(xx) (  ((xx) < -32767.0) ? (short)-32767                    \
                      : ((xx) >  32767.0) ? (short) 32767 : (short)rint(xx) )

/*! Force a float into a byte. */

#define BYTEIZE(xx)  (  ((xx) <   0.0) ? (byte)0                     \
                      : ((xx) > 255.0) ? (byte)255 : (byte)rint(xx) )

/*! Determine if a MRI_TYPE is an integer type. */

#define MRI_IS_INT_TYPE(typ) ((typ) < 3)

/*! I suppose that the next C makes this pleonastic. */

#if defined(_SUNPERF_COMPLEX) || defined(DONT_DECLARE_COMPLEX)
# define TYPEDEF_complex
#endif

#ifndef TYPEDEF_complex
#define TYPEDEF_complex
typedef struct complex { float r , i ; } complex ;
#endif

#ifndef TYPEDEF_float_pair
#define TYPEDEF_float_pair
typedef struct { float a,b ; } float_pair ;
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
          int nxyz ;          /*!< nx*ny*nz */
          int nxyzt  ;        /*!< nx*ny*nz*nt */
          int nvox   ;        /*!< number of voxels total */
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
} MRI_IMAGE ;

#ifdef USE_MRI_LABELS
/*! Copy auxiliary data from one MRI_IMAGE to another. */
#  define MRI_COPY_AUX(nn,oo)                                           \
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
#  define MRI_COPY_AUX(nn,oo)                                           \
    ( (nn)->dx = (oo)->dx , (nn)->dy = (oo)->dy , (nn)->dz = (oo)->dz , \
      (nn)->dt = (oo)->dt , (nn)->du = (oo)->du , (nn)->dv = (oo)->dv , \
      (nn)->dw = (oo)->dw ,                                             \
      (nn)->xo = (oo)->xo , (nn)->yo = (oo)->yo , (nn)->zo = (oo)->zo , \
      (nn)->to = (oo)->to , (nn)->uo = (oo)->uo , (nn)->vo = (oo)->vo , \
      (nn)->wo = (oo)->wo ,                                             \
      mri_add_name( (oo)->name , (nn) ) )
#endif

/*! Check if MRI_IMAGE is 1D (ny=1) */
#define MRI_IS_1D(iq)  ((iq)->ny == 1)

/*! Check if MRI_IMAGE is 2D (nz=1) */
#define MRI_IS_2D(iq)  ((iq)->ny > 1 && (iq)->nz == 1)

/*! Check if MRI_IMAGE is 3D (nt=1) */
#define MRI_IS_3D(iq)  ((iq)->nz > 1 && (iq)->nt == 1)

/*! Check if MRI_IMAGE is 4D (nu=1) */
#define MRI_IS_4D(iq)  ((iq)->nt > 1 && (iq)->nu == 1)

/*! Return dimensionality of MRI_IMAGE */
#define MRI_DIMENSIONALITY(iq)                     \
 ( ((iq)->ny == 1) ? 1 : ((iq)->nz == 1) ? 2 :     \
   ((iq)->nt == 1) ? 3 : ((iq)->nu == 1) ? 4 :     \
   ((iq)->nv == 1) ? 5 : ((iq)->nw == 1) ? 6 : 7 )

#define MRI_BYTE_PTR(iq)    ((byte *)mri_data_pointer(iq))
#define MRI_SHORT_PTR(iq)   ((short *)mri_data_pointer(iq))
#define MRI_INT_PTR(iq)     ((int *)mri_data_pointer(iq))
#define MRI_FLOAT_PTR(iq)   ((float *)mri_data_pointer(iq))
#define MRI_DOUBLE_PTR(iq)  ((double *)mri_data_pointer(iq))
#define MRI_COMPLEX_PTR(iq) ((complex *)mri_data_pointer(iq))
#define MRI_RGB_PTR(iq)     ((byte *)mri_data_pointer(iq))
#define MRI_RGBA_PTR(iq)    ((rgba *)mri_data_pointer(iq))

#define FLOAT_TO_BYTE(fff) \
  ( ((fff)<=0.0) ? (0) : ((fff)>=255.5) ? (255) : (byte)((fff)+0.49) )

#define SHORT_TO_BYTE(fff) \
  ( ((fff)<=0) ? (0) : ((fff)>=255) ? (255) : (byte)(fff) )

#define FLOAT_TO_SHORT(fff) ((short)(fff))

/*********** Type: array of MRI_IMAGE pointers ***********/

/*! Array of MRI_IMAGE pointers. */

typedef struct MRI_IMARR {
      int num ;              /*!< Number of actual MRI_IMAGE here */
      int nall ;             /*!< Size of imarr array currently allocated */
      MRI_IMAGE ** imarr ;   /*!< Array of MRI_IMAGE pointers */
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

#define MRI_BILINEAR  (1)   /* for the warping function */
#define MRI_LINEAR    (1)
#define MRI_BICUBIC   (2)
#define MRI_CUBIC     (2)
#define MRI_FOURIER   (3)
#define MRI_NN        (0)
#define MRI_QUINTIC   (4)   /* Nov 1998 */
#define MRI_HEPTIC    (5)
#define MRI_TSSHIFT   (6)   /* Dec 1999 */

#define MRI_FOURIER_NOPAD (66)  /* 13 May 2003 */

#define MRI_HIGHORDER(x) ((x) != MRI_NN && (x) != MRI_LINEAR)

#define SQR(x)   ((x)*(x))
#define CSQR(z)  (SQR(z.r)+SQR(z.i))
#define CABS(z)  sqrt(CSQR(z))
#define CARG(z)  ( ((z).r!=0.0 || (z).i!=0.0) ? atan2((z).i,(z).r) : 0.0 )

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

extern int mri_counter( MRI_IMAGE * , float , float ) ; /* 16 Jul 2007 */

#define MRI_IS_PURGED(iq) \
  ( (iq)!=NULL && (iq)->fondisk==IS_PURGED && (iq)->fname!=NULL )

#define MRI_HAS_DATA(iq)                                    \
  ( (iq)!= NULL &&                                          \
    ( ( (iq)->fondisk==IS_PURGED && (iq)->fname!=NULL ) ||  \
      mri_data_pointer_unvarnished(iq) != NULL         )  )

extern int mri_equal( MRI_IMAGE *, MRI_IMAGE * ) ; /* 30 Jun 2003 */

extern MRI_IMARR * mri_read_analyze75( char * ) ;  /* 05 Feb 2001 */
extern MRI_IMARR * mri_read_siemens( char * ) ;    /* 12 Mar 2001 */
extern MRI_IMARR * mri_read3D_analyze75( char * ); /* 26 Aug 2002 */

extern MRI_IMAGE * mri_read_stuff( char * ) ;      /* 22 Nov 2002 */
extern void        mri_inflate_pbm( MRI_IMAGE * ); /* 02 Jan 2002 */

extern void mri_add_name( char * , MRI_IMAGE * ) ;

extern MRI_IMAGE ** mri_stat_seq( MRI_IMAGE * ) ;

#define NSTAT_MEAN     0
#define NSTAT_SIGMA    2
#define NSTAT_CVAR     3
#define NSTAT_MEDIAN   4
#define NSTAT_MAD      5
#define NSTAT_MAX      6
#define NSTAT_MIN      7
#define NSTAT_ABSMAX  13
#define NSTAT_VAR     17
#define NSTAT_NUM     18
#define NSTAT_PERCENTILE  19
#define NSTAT_RANK        21      /* ZSS Jan 10 */
#define NSTAT_FRANK       22      /* ZSS Jan 10 */
#define NSTAT_P2SKEW      23      /* ZSS March 04 10*/

#define NSTAT_FWHMx   63
#define NSTAT_FWHMy   64
#define NSTAT_FWHMz   65
#define NSTAT_FWHMbar 66


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

extern float mri_nstat  ( int , int , float     * , float ) ;  /* 19 Aug 2005 */
extern float mri_nbistat( int , MRI_IMAGE *, MRI_IMAGE * ) ; /* 26 Oct 2006 */
extern void mri_nbistat_setclip( float, float , float, float ) ;

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
extern void csfft_use_fftw( int ) ;     /* 20 Oct 2000 */

extern void mri_fftshift( MRI_IMAGE *, float,float,float, int ) ; /* 13 May 2003 */

extern void * mri_data_pointer( MRI_IMAGE * ) ;
extern void mri_free( MRI_IMAGE * ) ;
extern void mri_clear( MRI_IMAGE * ) ;  /* 31 Jan 2007 */
extern void mri_fix_data_pointer( void * , MRI_IMAGE * ) ;
#define mri_set_data_pointer(iq,pt) mri_fix_data_pointer((pt),(iq))

#define MRI_FREE(iq) do{ mri_free(iq); (iq)=NULL; } while(0)

#define mri_data_pointer_unvarnished(iq) ((iq)->im)

extern char * mri_dicom_header( char * ) ;  /* 15 Jul 2002 */
extern void   mri_dicom_pxlarr( off_t *, int * ) ;
extern void   mri_dicom_noname( int ) ;
extern void   mri_dicom_nohex ( int ) ;
extern void   mri_dicom_setvm ( int ) ;     /* 28 Oct 2002 */
extern void   mri_dicom_seterr( int ) ;     /* 05 Nov 2002 */

extern MRI_IMARR * mri_read_dicom( char * )  ;
extern int         mri_imcount_dicom( char * ) ;
extern char *      mri_dicom_sexinfo( void ) ;   /* 23 Dec 2002 */
extern char *      mri_dicom_sex1010( void ) ;
extern int         mri_possibly_dicom( char * ) ;        /* 07 May 2003 */


/*! Set the data pointer in an MRI_IMAGE to NULL. */

#define mri_clear_data_pointer(iq) mri_fix_data_pointer(NULL,(iq))

/*! Set all pixels in MRI_IMAGE to zero. */

#define mri_zero_image(iq) \
   memset(mri_data_pointer(iq),0,(iq)->nvox*(iq)->pixel_size)

extern MRI_IMAGE * mri_zeropad_3D( int,int,int,int,int,int , MRI_IMAGE * ) ;
extern MRI_IMAGE * mri_zeropad_2D( int,int,int,int, MRI_IMAGE * ) ;

extern double mri_max( MRI_IMAGE * ) ;
extern double mri_min( MRI_IMAGE * ) ;
extern double mri_maxabs( MRI_IMAGE * ) ;

extern MRI_IMAGE * mri_cut_2D( MRI_IMAGE * , int,int,int,int ) ;
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
extern MRI_IMARR * mri_read_resamp_many_files( int nf, char * fn[] , int nxnew, int nynew);

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

extern unsigned int THD_filesize( char * pathname ) ;

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
extern MRI_IMAGE *mri_to_byte_scl( double , double , MRI_IMAGE * ) ;

extern MRI_IMAGE * mri_to_rgb( MRI_IMAGE * ) ;
extern MRI_IMAGE * mri_3to_rgb( MRI_IMAGE * , MRI_IMAGE * , MRI_IMAGE * ) ;
extern MRI_IMARR * mri_rgb_to_3float( MRI_IMAGE * ) ;
extern MRI_IMARR * mri_rgb_to_3byte( MRI_IMAGE * ) ;
extern MRI_IMAGE * mri_sharpen_rgb( float , MRI_IMAGE * ) ;
extern MRI_IMAGE * mri_flatten_rgb( MRI_IMAGE * ) ;
extern void mri_invert_inplace( MRI_IMAGE *) ;   /* 07 Apr 2003 */
extern void mri_gamma_rgb_inplace( float gam , MRI_IMAGE *im ) ;

extern MRI_IMAGE * mri_to_rgba( MRI_IMAGE * ) ;  /* 20 Mar 2002 */

extern MRI_IMAGE *mri_pair_to_complex( MRI_IMAGE * , MRI_IMAGE * ) ;
extern MRI_IMARR *mri_complex_to_pair( MRI_IMAGE * ) ;

extern MRI_IMAGE *mri_to_complex_ext( MRI_IMAGE * , int , int , int ) ;

extern MRI_IMAGE *mri_scale_to_float( float , MRI_IMAGE * ) ;
extern void mri_threshold( double , double , MRI_IMAGE * , MRI_IMAGE * ) ;
extern MRI_IMAGE * mri_mult_to_float( float * , MRI_IMAGE * ) ;

extern MRI_IMAGE * mri_scalize( MRI_IMAGE *, int, float * ) ; /* 20 Oct 2003 */

extern MRI_IMAGE *mri_multiply_complex( int , MRI_IMAGE * , MRI_IMAGE * ) ;
extern MRI_IMAGE *mri_complex_phase( MRI_IMAGE * ) ;

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
extern void mri_histoshort_all   ( MRI_IMAGE * , int * ) ;  /* 25 Jul 2001 */
extern void mri_histoshort_nonneg( MRI_IMAGE * , int * ) ;

extern void mri_percents( MRI_IMAGE * , int nper , float per[] ) ;
extern MRI_IMAGE * mri_flatten( MRI_IMAGE * ) ;
extern float mri_quantile( MRI_IMAGE * im , float alpha ) ;

extern float_pair mri_twoquantiles( MRI_IMAGE * im, float alpha, float beta ) ;

extern void qsort_short( int , short * ) ;
extern void qsort_float( int , float * ) ;
extern void qsort_pair( int , float * , int * ) ;
extern void qsort_int( int , int * ) ;

extern void isort_short( int , short * ) ;
extern void isort_float( int , float * ) ;
extern void isort_pair ( int , float * , int * ) ;

extern MRI_IMAGE * mri_nsize( MRI_IMAGE * ) ;

extern float * mri_lsqfit( MRI_IMAGE * fitim , MRI_IMARR * refim , MRI_IMAGE * ) ;
extern double * mri_startup_lsqfit( MRI_IMARR * , MRI_IMAGE * ) ;
extern float * mri_delayed_lsqfit( MRI_IMAGE * , MRI_IMARR * , double * ) ;
extern float * lsqfit( int , float * , float * , int , float *ref[] ) ;
extern double * startup_lsqfit( int , float * , int , float *ref[] ) ;
extern float * delayed_lsqfit( int , float * , int , float *ref[] , double * ) ;

extern MRI_IMAGE * mri_sobel( int , int , MRI_IMAGE * ) ;
extern MRI_IMAGE * mri_sharpen( float , int , MRI_IMAGE * ) ;
extern MRI_IMAGE * mri_transpose( MRI_IMAGE * ) ;

extern MRI_IMAGE * mri_clusterize( float,float, MRI_IMAGE * ,
                                   float,float, MRI_IMAGE * , int );
extern char * mri_clusterize_report(void) ;

#define FILT_FFT_WRAPAROUND  1

extern MRI_IMAGE * mri_filt_fft( MRI_IMAGE * im , float,int,int,int ) ;

extern MRI_IMAGE *mri_medianfilter( MRI_IMAGE *, float, byte *, int ); /* 22 Feb 2005 */
extern void mri_medianfilter_usedxyz( int i ) ;                       /* 08 Aug 2006 */

void mri_Set_KO_catwrap(void);
void mri_Set_OK_catwrap(void);
extern MRI_IMAGE * mri_cat2D( int,int,int,void *,MRI_IMARR *) ;
extern MRI_IMARR * mri_uncat2D( int , int , MRI_IMAGE * im ) ; /* 09 May 2000 */

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

#if 0
   if( im->fname != NULL && (im->fondisk & INPUT_DELAY) )
      mri_input_delay( im ) ;
   else if( MRI_IS_PURGED(im) ) /* 20 Dec 2006 */
      mri_unpurge( im ) ;
#endif

   data = im->im ;
   return data ;
}

/*-------------------------------------------------------------------------*/
/*! Modify the data pointer in an MRI_IMAGE struct.
---------------------------------------------------------------------------*/

void mri_fix_data_pointer( void *ptr , MRI_IMAGE *im )
{
   if( im == NULL ) return ;
   im->im = ptr ;
   return ;
}

#define EXRETURN return
#define RETURN   return
#define ENTRY(s) /*nothing*/

/*-------------------------------------------------------------------------*/
/*! Get rid of an MRI_IMAGE struct and all its contents.
---------------------------------------------------------------------------*/

void mri_free( MRI_IMAGE *im )
{
   void *ptr ;

ENTRY("mri_free") ;
   if( im == NULL ) EXRETURN ;
   if( im->fname != NULL ){ free(im->fname) ; im->fname = NULL ; }
   im->fondisk = 0 ;
   if( im->name != NULL ){ free(im->name) ; im->name = NULL ; }
   ptr = mri_data_pointer(im) ;
   if( ptr != NULL ) free(ptr) ;
   free(im) ;
   EXRETURN ;
}

/*-------------------------------------------------------------------------*/

void mri_clear( MRI_IMAGE *im )  /* 31 Jan 2007 */
{
  void *ptr ;
  if( im == NULL ) return ;
  ptr = mri_data_pointer(im) ;
  if( ptr != NULL ){ free(ptr); mri_fix_data_pointer(NULL,im); }
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

/*-------------------------------------------------------------------------*/
/*! Replace the guts of MRI_IMAGE struct qim with those of zim.
    Afterwards, what's left of zim is mri_free()-ed, so don't ever refer
    to it again. If you want a copy of an image, use mri_copy() instead.
---------------------------------------------------------------------------*/

void mri_move_guts( MRI_IMAGE *qim , MRI_IMAGE *zim )
{
   void *ptr ;

ENTRY("mri_move_guts") ;

   if( qim == NULL || zim == NULL ) EXRETURN ;  /* stupid caller */

   /* destroy the contents inside qim, if any */

   if( qim->fname != NULL ) free(qim->fname) ;
   if( qim->name  != NULL ) free(qim->name) ;
   ptr = mri_data_pointer(qim) ;
   if( ptr != NULL ) free(ptr) ;

   /* put the contents of zim in their place */

   *qim = *zim ;

   /* NULL out the contents of zim, then free() it */

   mri_fix_data_pointer( NULL , zim ) ;
   zim->name  = NULL ;
   zim->fname = NULL ;
   free(zim) ; EXRETURN ;
}

MRI_IMAGE *mri_to_short( double scl , MRI_IMAGE *oldim )
{
   MRI_IMAGE *newim ;
   register int ii , npix ;
   register double scale , val ;
   register short *sar ;

ENTRY("mri_to_short") ;

   if( oldim == NULL ) RETURN( NULL );  /* 09 Feb 1999 */

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
#ifdef MRI_DEBUG
   fprintf( stderr , "mri_to_short: scale factor = %e\n" , scale ) ;
#endif
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
   RETURN( newim );
}

/*---------------------------------------------------------------------------*/

MRI_IMAGE *mri_to_float( MRI_IMAGE *oldim )
{
   MRI_IMAGE *newim ;
   register int ii , npix ;
   register float *far ;

ENTRY("mri_to_float") ;

   if( oldim == NULL || mri_data_pointer(oldim) == NULL ) RETURN(NULL) ;

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
   RETURN( newim );
}

/*** Copy a string into the image structure;
     The usual use is to remember the input filename ***/

void mri_add_name( char *str , MRI_IMAGE *im )
{
   int ll ;

ENTRY("mri_add_name") ;
   if( im == NULL ) EXRETURN ;  /* 29 Mar 2002 */

   if( im->name != NULL ){ free( im->name ) ; im->name = NULL ; }

   if( str == NULL ) EXRETURN ;

   ll = strlen(str) ; if( ll <= 0 ) EXRETURN ;

   im->name = (char *) malloc( ll+1 ) ;
   strcpy( im->name , str ) ;
   EXRETURN ;
}

/*** get a new 2D image ***/

MRI_IMAGE *mri_new( int nx , int ny , MRI_TYPE kind )
{
   MRI_IMAGE *newim ;

   newim = mri_new_7D_generic( nx,ny , 1,1,1,1,1 , kind , TRUE ) ;
   return newim ;
}

/*** get a new 3D image, but with no data inside ***/

MRI_IMAGE *mri_new_vol_empty( int nx , int ny , int nz , MRI_TYPE kind )
{
   MRI_IMAGE *newim ;
   newim = mri_new_7D_generic( nx,ny,nz , 1,1,1,1 , kind , FALSE ) ;
   return newim ;
}

/*** get a new 3D image ***/

MRI_IMAGE *mri_new_vol( int nx , int ny , int nz , MRI_TYPE kind )
{
   MRI_IMAGE *newim ;
   newim = mri_new_7D_generic( nx,ny,nz , 1,1,1,1 , kind , TRUE ) ;
   return newim ;
}

/*** make a new 7D image ***/

MRI_IMAGE *mri_new_7D_generic(
            int nx, int ny, int nz, int nt, int nu, int nv, int nw,
            MRI_TYPE kind , int make_space )
{
   MRI_IMAGE *newim ;
   int npix ;

ENTRY("mri_new_7D_generic") ;

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

#ifdef USE_MRI_LABELS
   newim->xlab[0] = '\0' ;          /* default labels */
   newim->ylab[0] = '\0' ;
   newim->zlab[0] = '\0' ;
   newim->tlab[0] = '\0' ;
   newim->ulab[0] = '\0' ;
   newim->vlab[0] = '\0' ;
   newim->wlab[0] = '\0' ;
#endif

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

   RETURN(newim) ;
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
int idisp_ge4_image_header   ( char * info, ge4_image_t * im );
int idisp_ge4_series_header  ( char * info, ge4_series_t * s );
int idisp_ge4_study_header   ( char * info, ge4_study_t * st );


int ge4_swap_all_bytes	     ( ge4_header * h );
int idisp_ge4_series_header  ( char * info, ge4_series_t * s );
int ge4_validate_header      ( ge4_header * h );

/* ---------------------------------------------------------------------- */

#define GE4M_IND2STR(ind,str_list)					    \
		 ( ind < 0 || (ind > (sizeof(str_list)/sizeof(char *))) ) ? \
			      "out-of-range" : str_list[ind]

/*---------------------------------------------------------------*/


/*** for non ANSI compilers ***/

#ifndef SEEK_SET
#define SEEK_SET 0
#endif

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

#if 0
     im = mri_read_stuff(fname) ; if( im != NULL ) RETURN(im) ;
#else
     fprintf(stderr,"Code to read files of type '%s' is commented out!\n",fname) ;
     RETURN(NULL) ;
#endif
   }

   /*-- 16 Aug 2006: AFNI dataset? --*/

#if 0
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
#endif

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
          fread( &hdroff , 4,1 , imfile ) ;  /* location of image header */
          if( swap ) myswap_4(&hdroff) ;
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
               if( swap ) myswap_4(&itr) ;
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

#if 0
         im = mri_read_ascii( fname ) ;    /* list of ASCII numbers */
         if( im != NULL ) RETURN( im );

         im = mri_read_ppm( fname ) ;      /* 15 Apr 1999 */
         if( im != NULL ) RETURN( im );

         im = mri_read_stuff( fname ) ;    /* 22 Nov 2002 */
         if( im != NULL ) RETURN( im );
#endif

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

#define True 1

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
      fprintf(stderr,
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

#if 0
   } else if( strlen(new_fname) > 9 &&
              new_fname[0] == '3' && new_fname[1] == 'A' && new_fname[3] == ':' ){

      newar = mri_read_3A( new_fname ) ;   /* from a 3A: file */
#endif

#if 0
   } else if( check_dicom_magic_num( new_fname ) ) { /* 10 Aug 2004 */

     newar = mri_read_dicom( new_fname );  tried_dicom=2 ;
#endif

   } else if( strstr(new_fname,".hdr") != NULL ||
              strstr(new_fname,".HDR") != NULL   ){  /* 05 Feb 2001 */

      newar = mri_read_analyze75( new_fname ) ;      /* ANALYZE .hdr/.img filepair */

#if 0
   } else if( strstr(new_fname,".ima") != NULL ||
              strstr(new_fname,".IMA") != NULL   ){  /* 12 Mar 2001 */

      newar = mri_read_siemens( new_fname ) ;        /* Siemens file */
#endif

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

#if 0
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
#endif
   }

   /** failed to read anything?  try DICOM format (doesn't have a fixed suffix) **/
   /* 05 May 2003 added option to try DICOM last                    KRH          */

   if( newar == NULL ){

#if 0
      if ( !AFNI_yesenv("AFNI_TRY_DICOM_LAST")) {
        if( !tried_dicom ){
          newar = mri_read_dicom( new_fname ) ; tried_dicom = 1 ;
        }
      }
#endif

      /** if DICOM failed, try a 2D slice file, hope for the best **/

      if( newar == NULL && tried_dicom != 2 ){
        newim = mri_read( new_fname ) ;
        if( newim == NULL ){ free(new_fname); RETURN( NULL ); }  /* give up */
        INIT_IMARR(newar) ;
        ADDTO_IMARR(newar,newim) ;
      }

#if 0
      if( newar == NULL && AFNI_yesenv("AFNI_TRY_DICOM_LAST") ){
        if( !tried_dicom ){
          newar = mri_read_dicom( new_fname ) ; tried_dicom = 1 ;
        }
      }
#endif
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

#if 0
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
#endif

#if 0
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
#endif

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

#if 0
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
#endif

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
#if 0
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
static float lbfill = 0.0 ;  /* 10 Aug 2004 */

/*--------------------------------------------------------------*/
/*! Decode a line buffer into an array of floats.               */

static floatvec * decode_linebuf( char *buf )  /* 20 Jul 2004 */
{
   floatvec *fv=NULL ;
   int blen, bpos, ncol, ii, count ;
   char sep, vbuf[64] , *cpt ;
   float val ;

   if( buf == NULL || *buf == '\0' ) return fv ;

   blen = strlen(buf) ;
   ncol = 0 ;

   /* convert commas (or 'i' for complex numbers ZSS Oct 06) to blanks */

   for( ii=0 ; ii < blen ; ii++ ) if( buf[ii] == ',' || buf[ii] == 'i') buf[ii] = ' ' ;
   
   fv = (floatvec *)malloc(sizeof(floatvec)) ;
   fv->nar = 0 ;
   fv->ar  = (float *)NULL ;

   for( bpos=0 ; bpos < blen ; ){
     /* skip to next nonblank character */

     for( ; bpos < blen && (isspace(buf[bpos])||buf[bpos]==','||buf[bpos]=='i') ; bpos++ ) ; /* nada */
     if( bpos == blen ) break ;    /* end of line */

     sscanf( buf+bpos , "%63s" , vbuf ) ;

     val = 0.0 ; count = 1 ;
     if( vbuf[0] == '*' ){    /* 10 Aug 2004 */
       val = lbfill ;
     } else if( (cpt=strchr(vbuf,'@')) != NULL ){
       sscanf( vbuf , "%d%c%f" , &count , &sep , &val ) ;
       if( count < 1 ) count = 1 ;
       if( *(cpt+1) == '*' ) val = lbfill ;  /* 10 Aug 2004 */
     } else {
       sscanf( vbuf , "%f" , &val ) ;
     }

     fv->ar = (float *)realloc( (void *)fv->ar , sizeof(float)*(fv->nar+count) ) ;
     for( ii=0 ; ii < count ; ii++ ) fv->ar[ii+fv->nar] = val ;
     fv->nar += count ;
     bpos += strlen(vbuf) ;
   }

   if( fv->nar == 0 ){ KILL_floatvec(fv); fv = NULL; }
   return fv ;
}

static doublevec * decode_double_linebuf( char *buf )  /* 20 Jul 2004 */
{
   doublevec *dv=NULL ;
   int blen, bpos, ncol, ii, count ;
   char sep, vbuf[64] , *cpt ;
   double val ;

   if( buf == NULL || *buf == '\0' ) return dv ;

   blen = strlen(buf) ;
   ncol = 0 ;

   /* convert commas (or 'i' for complex numbers ZSS Oct 06) to blanks */

   for( ii=0 ; ii < blen ; ii++ ) if( buf[ii] == ',' || buf[ii] == 'i') buf[ii] = ' ' ;

   dv = (doublevec *)malloc(sizeof(doublevec)) ;
   dv->nar = 0 ;
   dv->ar  = (double *)NULL ;

   for( bpos=0 ; bpos < blen ; ){
     /* skip to next nonblank character */

     for( ; bpos < blen && (isspace(buf[bpos])||buf[bpos]==','||buf[bpos]=='i') ; bpos++ ) ; /* nada */
     if( bpos == blen ) break ;    /* end of line */

     sscanf( buf+bpos , "%63s" , vbuf ) ;

     val = 0.0 ; count = 1 ;
     if( vbuf[0] == '*' ){    /* 10 Aug 2004 */
       val = (double)lbfill ;
     } else if( (cpt=strchr(vbuf,'@')) != NULL ){
       sscanf( vbuf , "%d%c%lf" , &count , &sep , &val ) ;
       if( count < 1 ) count = 1 ;
       if( *(cpt+1) == '*' ) val = (double)lbfill ;  /* 10 Aug 2004 */
     } else {
       sscanf( vbuf , "%lf" , &val ) ;
     }

     dv->ar = (double *)realloc( (void *)dv->ar , sizeof(double)*(dv->nar+count) ) ;
     for( ii=0 ; ii < count ; ii++ ) dv->ar[ii+dv->nar] = val ;
     dv->nar += count ;
     bpos += strlen(vbuf) ;
   }

   if( dv->nar == 0 ){ KILL_doublevec(dv); dv = NULL; }
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

MRI_IMAGE * mri_read_ascii( char * fname )
{
   MRI_IMAGE * outim ;
   int ii,jj,val , used_tsar , alloc_tsar ;
   float * tsar ;
   float ftemp ;
   FILE * fts ;
   char * ptr ;
   int  ncol , bpos , blen , nrow ;
   static char *buf=NULL ;            /* 20 Jun 2002: make a ptr */

   floatvec *fvec ;                   /* 20 Jul 2004 */
   int incts ;

ENTRY("mri_read_ascii") ;

   if( fname == NULL || fname[0] == '\0' ) RETURN(NULL) ;

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
     ptr = my_fgets( buf , LBUF , fts ) ;  /* read */
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
   /** At this point, ncol is the number of floats to be read from each line **/

   rewind( fts ) ;  /* will start over */

   incts      = MAX(INC_TSARSIZE,ncol) ;
   used_tsar  = 0 ;
   alloc_tsar = incts ;
   tsar       = (float *) malloc( sizeof(float) * alloc_tsar ) ;
   if( tsar == NULL ){
      fprintf(stderr,"\n*** malloc error in mri_read_float_ascii ***\n"); EXIT(1);
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
          fprintf(stderr,"\n*** realloc error in mri_read_float_ascii ***\n"); EXIT(1);
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
      fprintf(stderr,"\n*** final realloc error in mri_read_float_ascii ***\n"); EXIT(1);
   }
   
   /* now turn tsar into a complex vector */
   ctsar = (complex *) calloc(used_tsar, sizeof(complex));
   for( ii=0 ; ii < used_tsar; ii=ii+2) {
      /* fprintf(stderr,"tsar[%d]=%f\n", ii, tsar[ii]);  */
      ih = ii/2;
      ctsar[ih].r = tsar[ii]; ctsar[ih].i = tsar[ii+1];
   }

   outim = mri_new_vol_empty( ncol/2 , nrow , 1 , MRI_complex ) ;
   mri_fix_data_pointer( tsar , outim ) ;
   mri_add_name( fname , outim ) ;

   FRB(buf) ; RETURN(outim) ;
}
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
   char dname[1024] , *cpt , *dpt ;
   int ii,jj,nx,ny,nts , *ivlist , *ivl , *sslist ;
   float *far , *oar ;
   int flip ;  /* 05 Sep 2006 */

ENTRY("mri_read_1D") ;

   if( fname == NULL || fname[0] == '\0' || strlen(fname) > 511 ) RETURN(NULL) ;

   strcpy(dname,fname); ii = strlen(dname);  /* 05 Sep 2006 */
   flip = (dname[ii-1] == '\''); if( flip ) dname[ii-1] = '\0';

   if( strncmp(dname,"1D:",3) == 0 ){       /* 28 Apr 2003 */
     outim = mri_1D_fromstring( dname+3 ) ;
     if( flip ){ inim=mri_transpose(outim); mri_free(outim); outim=inim; }
     RETURN(outim) ;
   }

   /*-- split filename and subvector list --*/

   cpt = strstr(fname,"[") ;
   dpt = strstr(fname,"{") ;            /* 30 Apr 2003: subsampling list */

   if( cpt == fname || dpt == fname ){  /* can't be at start of filename! */
      fprintf(stderr,"Illegal filename in mri_read_1D('%s')\n",fname) ;
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

     outim = mri_new( nts , ny , MRI_float ) ; /* make output image */
     far   = MRI_FLOAT_PTR( flim ) ;
     oar   = MRI_FLOAT_PTR( outim ) ;

     for( ii=0 ; ii < nts ; ii++ )             /* copy desired columns */
       for( jj=0 ; jj < ny ; jj++ )
         oar[ii+jj*nts] = far[ivl[ii]+jj*nx] ;

     mri_free(flim); free(sslist); flim = outim;
   }

   if( flip ){ inim=mri_transpose(flim); mri_free(flim); flim=inim; }

   mri_add_name(fname,flim) ; RETURN(flim) ;
}

MRI_IMAGE * mri_read_double_1D( char *fname )
{
   MRI_IMAGE *inim , *outim , *flim ;
   char dname[1024] , *cpt , *dpt ;
   int ii,jj,nx,ny,nts , *ivlist , *ivl , *sslist ;
   double *dar , *oar ;
   int flip ;  /* 05 Sep 2006 */

ENTRY("mri_read_double_1D") ;

   if( fname == NULL || fname[0] == '\0' || strlen(fname) > 511 ) RETURN(NULL) ;
   strcpy(dname,fname); ii = strlen(dname);  /* 05 Sep 2006 */
   flip = (dname[ii-1] == '\''); if( flip ) dname[ii-1] = '\0';

   if( strncmp(dname,"1D:",3) == 0 ){       /* 28 Apr 2003 */
     /*
     outim = mri_1D_double_fromstring( dname+3 ) ;
     if( flip ){ inim=mri_transpose(outim); mri_free(outim); outim=inim; }
     RETURN(outim) ;
     */
     fprintf(stderr,"Somebody was too lazy to allow this option here.\n"); RETURN(NULL);
   }

   /*-- split filename and subvector list --*/

   cpt = strstr(fname,"[") ;
   dpt = strstr(fname,"{") ;            /* 30 Apr 2003: subsampling list */

   if( cpt == fname || dpt == fname ){  /* can't be at start of filename! */
      fprintf(stderr,"Illegal filename in mri_read_double_1D('%s')\n",fname) ;
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
   char dname[1024] , *cpt , *dpt ;
   int ii,jj,nx,ny,nts , *ivlist , *ivl , *sslist ;
   complex *far , *oar ;
   int flip ;  /* 05 Sep 2006 */

ENTRY("mri_read_complex_1D") ;

   if( fname == NULL || fname[0] == '\0' || strlen(fname) > 511 ) RETURN(NULL) ;

   strcpy(dname,fname); ii = strlen(dname);  /* 05 Sep 2006 */
   flip = (dname[ii-1] == '\''); if( flip ) dname[ii-1] = '\0';

   if( strncmp(dname,"1D:",3) == 0 ){       /* 28 Apr 2003 */
     /*
     outim = mri_1D_complex_fromstring( dname+3 ) ;
     if( flip ){ inim=mri_transpose(outim); mri_free(outim); outim=inim; }
     RETURN(outim) ;
      */
     fprintf(stderr,"Somebody was too lazy to allow this option here.\n"); RETURN(NULL);
   }

   /*-- split filename and subvector list --*/

   cpt = strstr(fname,"[") ;
   dpt = strstr(fname,"{") ;            /* 30 Apr 2003: subsampling list */

   if( cpt == fname || dpt == fname ){  /* can't be at start of filename! */
      fprintf(stderr,"Illegal filename in mri_read_complex_1D('%s')\n",fname) ;
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

ENTRY("mri_read_ascii_complex") ;

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

MRI_IMARR * mri_read_3A( char * tname )
{
   int nx , ny , nz , ii , nxyz,nxy , nff ;
   int ngood , length , kim , datum_type ;
   char fname[256]="\0" , buf[512] ;
   MRI_IMARR * newar ;
   MRI_IMAGE * newim , * flim ;
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
#endif

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
ENTRY("swap_analyze_hdr") ;
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
#if 0
   myswap_2(&pntr->dime.unused1) ;
#endif
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
   EXRETURN ;
}

#if 0
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
#endif

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
        if( doswap ){ myswap_2(&xx); myswap_2(&yy); myswap_2(&zz); }
        if( xx > 0 && xx < hdr.dime.dim[1] &&
            yy > 0 && yy < hdr.dime.dim[2] &&
            zz > 0 && zz < hdr.dime.dim[3]   ) spmorg = 1 ;
     }
   }
   if( spmorg ) strcpy( MRILIB_orients , "LRPAIS" ) ;

   /* 27 Nov 2001: get a scale factor for images */

      fac = hdr.dime.funused1 ;
      (void) thd_floatscan( 1 , &fac ) ;
      if( fac < 0.0 || fac == 1.0 ) fac = 0.0 ;

   floatize = (fac != 0.0) ; /* 28 Nov 2001 */

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

#if 0
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
        if( doswap ){ myswap_2(&xx); myswap_2(&yy); myswap_2(&zz); }
        if( xx > 0 && xx < hdr.dime.dim[1] &&
            yy > 0 && yy < hdr.dime.dim[2] &&
            zz > 0 && zz < hdr.dime.dim[3]   ) spmorg = 1 ;
     }
   }
   if( spmorg ) strcpy( MRILIB_orients , "LRPAIS" ) ;

   /* 27 Nov 2001: get a scale factor for images */

      fac = hdr.dime.funused1 ;
      (void) thd_floatscan( 1 , &fac ) ;
      if( fac < 0.0 || fac == 1.0 ) fac = 0.0 ;

   floatize = (fac != 0.0) ; /* 28 Nov 2001 */

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
#endif

#if 0
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
      myswap_4( &(head.SiemensStudyDateMM) ) ;
      if( head.SiemensStudyDateMM < 0 || head.SiemensStudyDateMM > 13 ){
         swap = 0 ;
      }
   }

   /*-- find image size from header --*/

   if( swap ) myswap_4( &(head.DisplayMatrixSize) ) ;
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
      myswap_4( &(head.SiemensStudyDateMM) ) ;
      if( head.SiemensStudyDateMM < 0 || head.SiemensStudyDateMM > 13 ){
         swap = 0 ;
      }
   }

   /*-- find image size from header --*/

   if( swap ) myswap_4( &(head.DisplayMatrixSize) ) ;
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
#endif

#if 0
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
#endif

#if 0
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

#if 0
   if ((newar == NULL) && !AFNI_yesenv("AFNI_TRY_DICOM_LAST")) {
     if( !tried_dicom ){
       newar = mri_read_dicom( new_fname ) ; tried_dicom = 1 ;
     }
   }
#endif

   /* failed again?  try mri_read() for 1 image */

   if( newar == NULL ){
      newim = mri_read( new_fname ) ;      /* read from a 2D file */
      if( newim == NULL ){ free(new_fname) ; return NULL ; }
      INIT_IMARR(newar) ;
      ADDTO_IMARR(newar,newim) ;
   }

#if 0
   if ( (newar == NULL) && AFNI_yesenv("AFNI_TRY_DICOM_LAST")) {
     if( !tried_dicom ){
       newar = mri_read_dicom( new_fname ) ; tried_dicom = 1 ;
     }
   }
#endif

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
   long long length , nneed , hglob ;  /* 22 Mar 2007 */

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
#endif

/*----------------------------------------------------------------*/
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

#if 0
   if( ! MRI_IS_2D(im)      ){ RETURN(mri_write_7D ( fname , im )) ; }
#endif
   if( im->kind == MRI_rgb  ){ RETURN(mri_write_pnm( fname , im )) ; }
   if( im->kind == MRI_byte ){ RETURN(mri_write_pnm( fname , im )) ; }

   /* open the file for output */

   if( strcmp(fname,"-") != 0 ){
     imfile = fopen( fname , "r" ) ;
     if( imfile != NULL ){
       fclose( imfile ) ;
       fprintf(stderr,"(FAILED) attempt to overwrite file %s\n",fname) ;
       RETURN(0) ;
     }
   }

   if( strcmp(fname,"-") != 0 )
     imfile = fopen( fname , "w" ) ;
   else
     imfile = stdout ;   /* 18 Apr 2005: write to stdout instead */

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

   if( imfile != stdout ) fclose( imfile ) ;
   RETURN(1) ;
}

#if 0
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
#endif

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

   if( strcmp(fname,"-") != 0 ){
     imfile = fopen( fname , "r" ) ;
     if( imfile != NULL ){
       fclose( imfile ) ;
       fprintf(stderr,"(FAILED) attempt to overwrite image file %s\n",fname) ;
       RETURN( 0 );
     }
   }

   if( strcmp(fname,"-") != 0 )
     imfile = fopen( fname , "w" ) ;
   else
     imfile = stdout ;     /* 18 Apr 2005: write to stdout */

   if( imfile == NULL ){
     fprintf(stderr,"Couldn't open image file %s for writing\n" , fname ) ;
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

   if( imfile != stdout ) fclose( imfile ) ;
   RETURN( 1 );
}

#if 0
/*---------------------------------------------------------------------------------------*/

int mri_write_1D( char *fname , MRI_IMAGE *im )  /* 16 Nov 1999 */
{
   MRI_IMAGE *fim ;
   int jj ;

ENTRY("mri_write_1D") ;

   if( fname == NULL || strlen(fname) == 0 ||
       im == NULL    || im->nz > 1           ) RETURN( 0 );

   fim = mri_transpose( im ) ;
   jj  = mri_write_ascii( fname , fim ) ;
   mri_free(fim) ;
   RETURN( jj );
}
#endif

#if 0
/**------------------------ Only good for 1D and 2D images ---------------------------**/

int mri_write_ascii( char *fname, MRI_IMAGE *im )
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

   if( imfile != stdout ) fclose(imfile) ;
   RETURN( 1 );
}
#endif

#if 0
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

/*---------------------------------------------------------------*/

#include <signal.h>

int mri_write_jpg( char *fname , MRI_IMAGE *im )  /* 15 Apr 2005 */
{
   char *pg , *jpfilt, *eee ;
   FILE *fp ;
   int jpeg_compress;

   if( fname == NULL || *fname == '\0' || im == NULL ) return 0 ;
   if( im->kind != MRI_rgb && im->kind != MRI_byte   ) return 0 ;

   pg = THD_find_executable( "cjpeg" ) ;
   if( pg == NULL ) return 0 ;
   /* user environment variable compression quality - mod 5/10/2006 drg */
   eee = getenv("AFNI_JPEG_COMPRESS");
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
#endif

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
/* series header value descriptions - for display */

static char * g_ge4_sl_im_modes[] =
			{ "2D single", "2D multiple", "3D volume", "cine",
			  "spectroscopy" };
static char * g_ge4_sl_pulse_seqs[] =
			{ "memp", "ir", "ps", "rm", "rmge", "gre", "vemp",
			  "mpgr", "mpgrv", "mpirs", "mpiri", "3d/gre",
			  "cine/gre", "spgr", "sspf", "cin/spgr", "3d/spgr",
			  "fse", "fve", "fspr", "fgr", "fmpspgr", "fmpgr",
			  "fmpir", "probe.s", "probe.p" };
static char * g_ge4_sl_orient[] = { "supine", "prone", "Lt", "Rt" };

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
    long long      file_len;
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
 *  Display the contents of the ge4_image_t struct.
 *------------------------------------------------------------
*/
int idisp_ge4_study_header( char * info, ge4_study_t * st )
{
    if ( info )
	fputs( info, stdout );

    if ( st == NULL )
    {
	printf( "r_idisp_ge4_study_t: st == NULL" );
	return -1;
    }

    printf( " ge4_study_t at %p :\n"
	    "    title                    = %s\n"
	    "    num                      = %s\n"
	    "    date                     = %s\n"
	    "    time                     = %s\n"
	    "    pat_name                 = %s\n"
	    "    pat_id                   = %s\n"
	    "    age                      = %s\n"
	    "    sex                      = %c\n",
	    st, st->title, st->num, st->date, st->time,
	    st->pat_name, st->pat_id, st->age, st->sex
	    );

    return 0;
}


/*------------------------------------------------------------
 *  Display the contents of the ge4_image_t struct.
 *------------------------------------------------------------
*/
int idisp_ge4_image_header( char * info, ge4_image_t * im )
{
    if ( info )
	fputs( info, stdout );

    if ( im == NULL )
    {
	printf( "r_idisp_ge4_image_t: im == NULL" );
	return -1;
    }

    printf( " ge4_image_t at %p :\n"
	    "    title                    = %s\n"
	    "    im_num                   = %s\n"
	    "    im_loc                   = %.3f\n"
	    "    table_posn               = %.3f\n"
	    "    im_thickness             = %.3f\n"
	    "    im_spacing               = %.3f\n"
	    "    tr (in ms)               = %.3f\n"
	    "    te (in ms)               = %.3f\n"
	    "    ti (in ms)               = %.3f\n"
	    "    num_echoes               = %d\n"
	    "    echo_num                 = %d\n"
	    "    iNEX                     = %d\n"
	    "    fNEX                     = %.3f\n"
	    "    flip_angle               = %d\n",
	    im, im->title, im->im_num, im->im_loc, im->table_posn,
	    im->im_thickness, im->im_spacing, im->tr, im->te, im->ti,
	    im->num_echoes, im->echo_num, im->iNEX, im->fNEX, im->flip_angle
	    );

    return 0;
}


/*------------------------------------------------------------
 *  Display the contents of the ge4_series_t struct.
 *------------------------------------------------------------
*/
int idisp_ge4_series_header( char * info, ge4_series_t * s )
{
    if ( info )
	fputs( info, stdout );

    if ( s == NULL )
    {
	printf( "r_idisp_ge4_series_t: s == NULL" );
	return -1;
    }

    printf( " ge4_series_t at %p :\n"
	    "    title                    = %s\n"
	    "    series_num               = %s\n"
	    "    plane_type, plane_desc   = %d, %s\n"
	    "    image_mode               = %d (%s)\n"
	    "    pulse_seq                = %d (%s)\n"
	    "    FOV (in mm)              = %.3f\n"
	    "    center[0], c[1], c[2]    = %.3f, %.3f, %.3f\n"
	    "    orient                   = %d (%s)\n"
	    "    scan_mat_x, scan_mat_y   = %d, %d\n"
	    "    im_mat                   = %d\n",
	    s, s->title, s->series_num, s->plane_type, s->plane_desc,
	    s->im_mode, GE4M_IND2STR(s->im_mode, g_ge4_sl_im_modes),
	    s->pulse_seq, GE4M_IND2STR(s->pulse_seq, g_ge4_sl_pulse_seqs),
	    s->fov, s->center[0], s->center[1], s->center[2],
	    s->orient, GE4M_IND2STR(s->orient,g_ge4_sl_orient),
	    s->scan_mat_x, s->scan_mat_y, s->im_mat
	    );

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

double mri_min( MRI_IMAGE *im )
{
   register int ii , npix ;
   byte   byte_min   = 255 ;
   short  short_min  = 32767 ;
   int    int_min    = 2147483647 ;
   float  float_min  = 1.e+38 ;
   double double_min = 1.e+38 ;

   npix = im->nvox ;

   switch( im->kind ){

      case MRI_byte:{
         byte *qar = MRI_BYTE_PTR(im) ;
         for( ii=0 ; ii < npix ; ii++ )
            byte_min = MIN( byte_min , qar[ii] ) ;
         return (double) byte_min ;
      }

      case MRI_short:{
         short *qar = MRI_SHORT_PTR(im) ;
         for( ii=0 ; ii < npix ; ii++ )
            short_min = MIN( short_min , qar[ii] ) ;
         return (double) short_min ;
      }

      case MRI_int:{
         int *qar = MRI_INT_PTR(im) ;
         for( ii=0 ; ii < npix ; ii++ )
            int_min = MIN( int_min , qar[ii] ) ;
         return (double) int_min ;
      }

      case MRI_float:{
         float *qar = MRI_FLOAT_PTR(im) ;
         for( ii=0 ; ii < npix ; ii++ )
            float_min = MIN( float_min , qar[ii] ) ;
         return (double) float_min ;
      }

      case MRI_double:{
         double *qar = MRI_DOUBLE_PTR(im) ;
         for( ii=0 ; ii < npix ; ii++ )
            double_min = MIN( double_min , qar[ii] ) ;
         return double_min ;
      }

      case MRI_complex:{
         complex *qar = MRI_COMPLEX_PTR(im) ;
         for( ii=0 ; ii < npix ; ii++ )
            float_min = MIN( float_min , CSQR(qar[ii]) ) ;
         return sqrt(float_min) ;
      }

      case MRI_rgb:{
         byte *rgb = MRI_RGB_PTR(im) ;
         double val , bot=255.9 ;
         for( ii=0 ; ii < npix ; ii++ ){  /* scale to brightness */
            val =  0.299 * rgb[3*ii]      /* between 0 and 255     */
                 + 0.587 * rgb[3*ii+1]
                 + 0.114 * rgb[3*ii+2] ;
            if( val < bot ) bot = val ;
         }
         return bot ;
      }

      default:
         fprintf( stderr , "mri_min:  unknown image kind\n" ) ;
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

ENTRY("mri_scale_inplace") ;

   if( im == NULL || fac == 1.0 || fac == 0.0 ) EXRETURN ;
   vp = mri_data_pointer( im ) ; if( vp == NULL ) EXRETURN ;
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
   }

   EXRETURN ;
}
#define SWAB16(x) ( ( ((x)&0x00ffU)<<8 ) | ( ((x)&0xff00U)>>8 ) )

void mri_swapbytes( MRI_IMAGE *im )
{
   register int ii , npix ;
   register short *iar ;

ENTRY("mri_swapbytes") ;

   if( im == NULL || im->kind != MRI_short ){
     fprintf( stderr , "mri_swapbytes called with non-short image kind\n" ) ;
     EXRETURN ;
   }

   npix = im->nvox ; iar = MRI_SHORT_PTR(im) ;

   for( ii=0 ; ii < npix ; ii++ ) iar[ii] = SWAB16( iar[ii] ) ;

   EXRETURN ;
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

#if 0
#ifdef isfinite
# define IS_GOOD_FLOAT(x) isfinite(x) /* 28 Aug 2003: use C99 macro if exists */
#else
# define IS_GOOD_FLOAT(x) finite(x)
#endif
#endif

#define IS_GOOD_FLOAT(x) finite(x)

#if 0
# define IS_GOOD_FLOAT(x) isnan(x)
#endif

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
