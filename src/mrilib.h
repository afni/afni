/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/

#ifndef _MCW_MRILIB_HEADER_
#define _MCW_MRILIB_HEADER_

#define MRILIB_7D

#define COXEMAIL "rwcox@nih.gov"        /* or /dev/null, if you prefer */

extern char MRILIB_orients[] ;          /* 12 Mar 2001 */
extern float MRILIB_zoff ;              /* global variables from mri_read.c */

#define CLEAR_MRILIB_globals \
 do{ MRILIB_orients[0] = '\0' ; MRILIB_zoff = 0.0 ; } while(0)

#include <stdio.h>
#include <stdlib.h>
#include <math.h>

#include "mcw_malloc.h"  /* 06 Mar 1999 addition */
#include "debugtrace.h"  /* 26 Jan 2001 addition */

#ifndef PI
#  define PI 3.14159265358979323846
#endif

#ifndef WAY_BIG
#  define WAY_BIG 1.e+10
#endif

#ifndef MAX
#  define MAX(a,b) (((a)<(b)) ? (b) : (a))
#endif

#ifndef MIN
#  define MIN(a,b) (((a)>(b)) ? (b) : (a))
#endif

#undef DONT_USE_MEMCPY

/**** define types ****/

typedef unsigned char byte ;

typedef struct { byte r,g,b,a ; } rgba ;  /* 24 Aug 2001 */

#define LOAD_rgba(s,rr,gg,bb,aa)   ((s).r=(rr),(s).g=(gg),(s).b=(bb),(s).a=(bb))
#define UNLOAD_rgba(s,rr,gg,bb,aa) ((rr)=(s).r,(gg)=(s).g,(bb)=(s).b,(aa)=(s).a)

#define BYTE_TO_ZONE(b) (0.0392157*(b))
#define ZONE_TO_BYTE(z) ((byte)(255.49*(z)))

typedef enum MRI_TYPE {
         MRI_byte , MRI_short  , MRI_int  ,
        MRI_float , MRI_double , MRI_complex , MRI_rgb } MRI_TYPE ;

#define MRI_KIND MRI_TYPE ;   /* to alleviate stupidity */
#define MRI_type MRI_TYPE ;
#define MRI_kind MRI_TYPE ;

#define LAST_MRI_TYPE 6

static char * MRI_TYPE_name[7] =
  { "byte" , "short" , "int" , "float" , "double" , "complex" , "rgb" } ;

#define MRI_type_name MRI_TYPE_name  /* because I forget */

#define MRI_maxbyte         255
#define MRI_maxshort      32767
#define MRI_maxint   2147483647

static float MRI_TYPE_maxval[7] =
  { 255.0 , 32767.0 , 2147483647.0 , 0.0,0.0,0.0 , 255.0 } ;

#define SHORTIZE(xx) (  ((xx) < -32767.0) ? (short)-32767                    \
                      : ((xx) >  32767.0) ? (short) 32767 : (short)rint(xx) )

#define BYTEIZE(xx)  (  ((xx) <   0.0) ? (byte)0                     \
                      : ((xx) > 255.0) ? (byte)255 : (byte)rint(xx) )

#define MRI_IS_INT_TYPE(typ) ((typ) < 3)

typedef struct complex { float r , i ; } complex ;

/*-------*/
typedef struct rgbyte { byte r,g,b ; } rgbyte ;  /* 15 Feb 1999 */

static rgbyte tEMp_rgbyte_aAa ;

#define RGBYTE_TO_INT(rgb) ( (rgb).r << 16 | (rgb).g << 8 | (rgb).b )

#define INT_TO_RGB(q) ( tEMp_rgbyte_aAa.r = ((q) >> 16) & 0xff , \
                        tEMp_rgbyte_aAa.g = ((q) >>  8) & 0xff , \
                        tEMp_rgbyte_aAa.b = (q)         & 0xff , tEMp_rgbyte_aAa )
/*-------*/

typedef union MRI_DATA {
         byte     *byte_data ;
         short    *short_data ;
         int      *int_data ;
         float    *float_data ;
         double   *double_data ;
         complex  *complex_data ;
         byte     *rgb_data ;      /* Apr 1996: not well supported yet */
} MRI_DATA ;

/** Mar 1996: Extended to images up to 7D;
              Not all routines work with images > 2D --
              check top of file for "7D SAFE" comments **/

#undef USE_MRI_LABELS
#ifdef USE_MRI_LABELS
#  define MRI_LABEL_SIZE 4
#endif

#define USE_MRI_DELAY   /* 01 Jan 1997 */
#ifdef USE_MRI_DELAY
#  define INPUT_DELAY  1
#  define BSWAP_DELAY  2
#endif

typedef struct MRI_IMAGE {
         int nx,ny,nz , nt,nu,nv,nw ,   /* dimensions of array */
             nxy,nxyz,nxyzt , nvox  ,   /* nx*ny , nxy*nz , nxyz*nt , # voxels */
             pixel_size ;               /* bytes per pixel */
         MRI_TYPE kind ;                /* one of the type codes above */
         MRI_DATA im ;                  /* pointer to pixel data */
         char * name ;                  /* string attached; may be NULL */
         float dx,dy,dz, dt,du,dv,dw ,  /* physical pixel dimensions, if != 0 */
               xo,yo,zo, to,uo,vo,wo  ; /* offsets for dimensions */
#ifdef USE_MRI_LABELS
         char xlab[MRI_LABEL_SIZE] ,    /* labels for each dimension */
              ylab[MRI_LABEL_SIZE] , zlab[MRI_LABEL_SIZE] ,
              tlab[MRI_LABEL_SIZE] , ulab[MRI_LABEL_SIZE] ,
              vlab[MRI_LABEL_SIZE] , wlab[MRI_LABEL_SIZE]  ;
#endif

#ifdef USE_MRI_DELAY
         char * fname ;
         int foffset , fondisk ;
#endif
} MRI_IMAGE ;

#ifdef USE_MRI_LABELS
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

#define MRI_IS_1D(iq)  ((iq)->ny == 1)
#define MRI_IS_2D(iq)  ((iq)->ny > 1 && (iq)->nz == 1)
#define MRI_IS_3D(iq)  ((iq)->nz > 1 && (iq)->nt == 1)
#define MRI_IS_4D(iq)  ((iq)->nt > 1 && (iq)->nu == 1)

#define MRI_DIMENSIONALITY(iq)                     \
 ( ((iq)->ny == 1) ? 1 : ((iq)->nz == 1) ? 2 :     \
   ((iq)->nt == 1) ? 3 : ((iq)->nu == 1) ? 4 :     \
   ((iq)->nv == 1) ? 5 : ((iq)->nw == 1) ? 6 : 7 )

#define MRI_BYTE_PTR(iq)    ((iq)->im.byte_data)
#define MRI_SHORT_PTR(iq)   ((iq)->im.short_data)
#define MRI_INT_PTR(iq)     ((iq)->im.int_data)
#define MRI_FLOAT_PTR(iq)   ((iq)->im.float_data)
#define MRI_DOUBLE_PTR(iq)  ((iq)->im.double_data)
#define MRI_COMPLEX_PTR(iq) ((iq)->im.complex_data)
#define MRI_RGB_PTR(iq)     ((iq)->im.rgb_data)

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

typedef struct MRI_IMARR {
      int num , nall ;
      MRI_IMAGE ** imarr ;
} MRI_IMARR ;

#define IMAGE_IN_IMARR(name,nn) ((name)->imarr[(nn)])
#define IMARR_SUBIMAGE          IMAGE_IN_IMARR
#define IMARR_SUBIM             IMAGE_IN_IMARR
#define IMARR_COUNT(name)       ((name)->num)

#define INC_IMARR 32

#define INIT_IMARR(name)                                                           \
   do{ int iq ; (name) = (MRI_IMARR *) malloc(sizeof(MRI_IMARR)) ;                 \
       (name)->num = 0 ; (name)->nall = INC_IMARR ;                                \
       (name)->imarr = (MRI_IMAGE **)malloc(sizeof(MRI_IMAGE *)*INC_IMARR) ;       \
       for( iq=(name)->num ; iq < (name)->nall ; iq++ ) (name)->imarr[iq] = NULL ; \
       break ; } while(0)

#define ADDTO_IMARR(name,imm)                                                           \
   do{ int nn , iq ;                                                                    \
       if( (name)->num == (name)->nall ){                                               \
          nn = (name)->nall = 1.1*(name)->nall + INC_IMARR ;                            \
          (name)->imarr = realloc( (name)->imarr,sizeof(MRI_IMAGE *)*nn );              \
          for( iq=(name)->num ; iq < (name)->nall ; iq++ ) (name)->imarr[iq] = NULL ; } \
       nn = (name)->num ; ((name)->num)++ ;                                             \
       (name)->imarr[nn] = (imm) ; break ; } while(0)

#define FREE_IMARR(name)                                                        \
   do{ if( (name) != NULL ){                                                    \
          free((name)->imarr); free((name)); (name) = NULL; } break; } while(0)

#define DESTROY_IMARR(name)                                                     \
   do{ int nn ;                                                                 \
       if( (name) != NULL ){                                                    \
          for( nn=0 ; nn < (name)->num ; nn++ ) mri_free((name)->imarr[nn]) ;   \
          free((name)->imarr); free((name)); (name) = NULL; } break; } while(0)

#define TRUNCATE_IMARR(name,qq)                                                 \
   do{ int nn ;                                                                 \
       if( (name) != NULL && qq < (name)->num ){                                \
          for( nn=qq ; nn < (name)->num ; nn++ ) mri_free((name)->imarr[nn]);   \
          (name)->num = qq ;                                                    \
       } } while(0)

/******* macros for complex arithmetic, using comma operator *******/

static float   MRI_fla ;                      /* float temporaries   */
static complex MRI_cxa , MRI_cxb , MRI_cxc ;  /* complex temporaries */

#define CMPLX(x,y) ( MRI_cxa.r = (x) , MRI_cxa.i = (y) , MRI_cxa )

#define CADD(u,v) ( MRI_cxa.r = u.r + v.r , \
                    MRI_cxa.i = u.i + v.r , MRI_cxa )

#define CADDTO(u,v) ( u.r += v.r , u.i += v.i )

#define CSUB(u,v) ( MRI_cxa.r = u.r - v.r , \
                    MRI_cxa.i = u.i - v.i , MRI_cxa )

#define CSUBFROM(u,v) ( u.r -= v.r , u.i -= v.i )

#define CMULT(u,v) ( MRI_cxb.r = u.r * v.r - u.i * v.i , \
                     MRI_cxb.i = u.r * v.i + u.i * v.r , MRI_cxb )

#define CMULTBY(u,v) ( MRI_fla = u.r * v.r - u.i * v.i , \
                       u.i     = u.r * v.i + u.i * v.r , u.r = MRI_fla )

/** output of CJMULT is u * conjg(v) **/

#define CJMULT(u,v) ( MRI_cxb.r = u.r * v.r + u.i * v.i , \
                      MRI_cxb.i = u.i * v.r - u.r * v.i , MRI_cxb )

#define CJMULTBY(u,v) ( MRI_fla = u.r * v.r + u.i * v.i , \
                        u.i     = u.i * v.r - u.r * v.i , u.r = MRI_fla )

#define CMADD(u,v,w) ( w.r += u.r * v.r - u.i * v.i , \
                       w.i += u.r * v.i + u.i * v.r    )

#define CEXPIT(t)   ( MRI_cxc.r = cos(t) , MRI_cxc.i = sin(t) , MRI_cxc )

/**** macros ****/

static int MRI_mm ;

#define MEDIAN(a,b,c) ( MRI_mm = 4*((a)<(b)) + 2*((a)<(c)) + ((b)<(c)) , \
                        (MRI_mm==3||MRI_mm==4) ? (a) :                   \
                        (MRI_mm==7||MRI_mm==0) ? (b) : (c) )

#define OSFSUM(p,q,r) (0.70*(p)+0.15*((q)+(r)))

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

#define SQR(x)   ((x)*(x))
#define CSQR(z)  (SQR(z.r)+SQR(z.i))
#define CABS(z)  sqrt(CSQR(z))
#define CARG(z)  ( ((z).r!=0.0 || (z).i!=0.0) ? atan2((z).i,(z).r) : 0.0 )

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

#ifdef USE_MRI_DELAY
  extern void        mri_input_delay( MRI_IMAGE * ) ;
  extern void        mri_purge_delay( MRI_IMAGE * ) ;
  extern void        mri_add_fname_delay( char * , MRI_IMAGE * ) ;
  extern MRI_IMARR * mri_read_file_delay( char * ) ;
  extern MRI_IMARR * mri_read_3D_delay( char * ) ;
#endif

extern MRI_IMARR * mri_read_analyze75( char * ) ;  /* 05 Feb 2001 */
extern MRI_IMARR * mri_read_siemens( char * ) ;    /* 12 Mar 2001 */

extern void mri_add_name( char * , MRI_IMAGE * ) ;

extern MRI_IMAGE ** mri_stat_seq( MRI_IMAGE * ) ;

extern MRI_IMAGE * mri_edit_image( float pthr, float power, MRI_IMAGE * im ) ;

extern void cfft( int , int , float * , float * ) ;
extern void cfft2d( int , int , int , float * , float * ) ;
extern void csfft_cox( int,int , complex * ) ;
extern void csfft_many( int,int,int , complex * ) ;
extern int  csfft_nextup(int) ;
extern int csfft_nextup_one35(int) ;
extern void csfft_scale_inverse(int) ;
extern void csfft_use_fftw( int ) ;     /* 20 Oct 2000 */

extern void *mri_data_pointer( MRI_IMAGE * ) ;
extern void mri_free( MRI_IMAGE * ) ;
extern void mri_fix_data_pointer( void * , MRI_IMAGE * ) ;
#define mri_clear_data_pointer(iq) mri_fix_data_pointer(NULL,(iq))

#define mri_zero_image(iq) \
   memset(mri_data_pointer(iq),0,(iq)->nvox*(iq)->pixel_size)

extern MRI_IMAGE * mri_zeropad_3D( int,int,int,int,int,int , MRI_IMAGE * ) ;
extern MRI_IMAGE * mri_zeropad_2D( int,int,int,int, MRI_IMAGE * ) ;

extern double mri_max( MRI_IMAGE * ) ;
extern double mri_min( MRI_IMAGE * ) ;
extern double mri_maxabs( MRI_IMAGE * ) ;

extern MRI_IMAGE * mri_cut_2D( MRI_IMAGE * , int,int,int,int ) ;

/** 15 Apr 1999 **/

extern void mri_upsample( int , int , float * , float * ) ;
extern MRI_IMAGE * mri_dup2D( int , MRI_IMAGE * ) ;

extern MRI_IMAGE * mri_copy( MRI_IMAGE * ) ;             /* 17 Apr 2000 */
extern MRI_IMAGE *mri_new( int , int , MRI_TYPE ) ;
extern MRI_IMAGE *mri_read( char * ) ;
extern int mri_write( char * , MRI_IMAGE * ) ;
extern int mri_write_pnm( char * , MRI_IMAGE * ) ;
extern int mri_write_7D( char * , MRI_IMAGE * ) ;
extern int mri_datum_size( MRI_TYPE typ ) ;
extern MRI_IMAGE *mri_read_ascii( char * ) ;
extern int mri_write_ascii( char * , MRI_IMAGE * ) ;
extern int mri_write_raw( char * , MRI_IMAGE * ) ;       /* 05 Jan 2000 */
extern void mri_write_analyze( char * , MRI_IMAGE * ) ;  /* 29 Nov 2001 */

extern MRI_IMAGE * mri_read_1D( char * ) ;               /* 16 Nov 1999 */
extern int mri_write_1D( char * , MRI_IMAGE * ) ;        /* 16 Nov 1999 */

extern int setup_mri_write_angif( void ) ;               /* 28 Jun 2001 */
extern int mri_write_angif( char *, MRI_IMARR * ) ;

extern MRI_IMAGE *mri_new_vol      ( int,int,int , MRI_TYPE ) ;
extern MRI_IMAGE *mri_new_vol_empty( int,int,int , MRI_TYPE ) ;

MRI_IMAGE *mri_new_7D_generic( int nx, int ny, int nz, int nt,
                               int nu, int nv, int nw,
                               MRI_TYPE kind , int make_space ) ;

#define mri_new_conforming(iq,kk)                                   \
   mri_new_7D_generic( (iq)->nx, (iq)->ny, (iq)->nz , (iq)->nt ,    \
                       (iq)->nu, (iq)->nv, (iq)->nw , (kk) , TRUE )

#define mri_empty_conforming(iq,kk)                                 \
   mri_new_7D_generic( (iq)->nx, (iq)->ny, (iq)->nz , (iq)->nt ,    \
                       (iq)->nu, (iq)->nv, (iq)->nw , (kk) , FALSE )

extern MRI_IMARR * mri_read_3D( char * ) ;
extern MRI_IMARR * mri_read_3A( char * ) ;
extern MRI_IMARR * mri_read_file( char * ) ;
extern int mri_imcount( char * ) ;
extern MRI_IMARR * mri_read_many_files( int nf , char * fn[] ) ;

/** returns array of byte images: red, green, blue **/

extern MRI_IMARR * mri_read_ppm3( char * fname ) ;
extern MRI_IMAGE * mri_read_ppm( char * fname ) ;

extern void mri_read_ppm_header( char *, int *, int *) ; /* 17 Sep 2001 */

MRI_IMAGE *mri_read_just_one( char * fname ) ;
MRI_IMAGE *mri_read_nsize( char * fname ) ;
MRI_IMARR *mri_read_many_nsize( int nf , char * fn[] ) ;

void init_MCW_sizes(void) ;
char * imsized_fname( char * fname ) ;
long mri_filesize( char * pathname ) ;
char * my_strdup( char * str ) ;

extern void mri_overlay( MRI_IMAGE *, MRI_IMAGE *, int,int ) ;

extern void mri_swapbytes( MRI_IMAGE * ) ;

extern void swap_twobytes ( int n , void * ar ) ;  /* 14 Sep 1998 */
extern void swap_fourbytes( int n , void * ar ) ;

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

extern MRI_IMAGE *mri_pair_to_complex( MRI_IMAGE * , MRI_IMAGE * ) ;
extern MRI_IMARR *mri_complex_to_pair( MRI_IMAGE * ) ;

extern MRI_IMAGE *mri_to_complex_ext( MRI_IMAGE * , int , int , int ) ;

extern MRI_IMAGE *mri_scale_to_float( float , MRI_IMAGE * ) ;
extern void mri_threshold( double , double , MRI_IMAGE * , MRI_IMAGE * ) ;
extern MRI_IMAGE * mri_mult_to_float( float * , MRI_IMAGE * ) ;

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

extern MRI_IMAGE *mri_resize( MRI_IMAGE * , int , int ) ;

extern MRI_IMAGE *mri_rotate         ( MRI_IMAGE * , float,float,float,float ) ;
extern MRI_IMAGE *mri_rotate_bilinear( MRI_IMAGE * , float,float,float,float ) ;

extern MRI_IMAGE *mri_rota         ( MRI_IMAGE * , float,float,float ) ;
extern MRI_IMAGE *mri_rota_bilinear( MRI_IMAGE * , float,float,float ) ;
extern MRI_IMAGE *mri_rota_shear   ( MRI_IMAGE * , float,float,float ) ;
extern MRI_IMAGE *mri_rota_variable( int, MRI_IMAGE * , float,float,float ) ;

extern MRI_IMAGE *mri_aff2d_byte( MRI_IMAGE *,int,float,float,float,float) ;
extern MRI_IMAGE *mri_aff2d_rgb ( MRI_IMAGE *,int,float,float,float,float) ;

extern void mri_scale_inplace( float , MRI_IMAGE * ) ; /* 27 Nov 2001 */

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

#define FILT_FFT_WRAPAROUND  1

extern MRI_IMAGE * mri_filt_fft( MRI_IMAGE * im , float,int,int,int ) ;

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

/*---------------------------------------------------------------------*/
/* 07 April 1998: routines for one-at-a-time alignment (mri_2dalign.c) */

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

/*---------------------------------------------------------------------*/
/*--------- 22 April 1998: byte order routines (mri_order.c) ----------*/

#define LSB_FIRST      1
#define MSB_FIRST      2
#define NATIVE_ORDER  -1

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

extern void ppmd_filledrectangle( byte *pixels, int cols, int rows,
                           int x, int y, int width, int height , byte r,byte g,byte b );

extern void ppmd_line( byte *pixels, int cols, int rows,
                int x0, int y0, int x1, int y1, byte r,byte g,byte b );

extern void ppmd_spline3( byte *pixels, int cols, int rows,
                   int x0, int y0, int x1, int y1, int x2, int y2, byte r,byte g, byte b);


extern void ppmd_polyspline( byte *pixels, int cols, int rows,
                      int x0,int y0, int nc, int* xc, int* yc, int x1,int y1, byte r,byte g,byte b );

extern void ppmd_circle( byte *pixels, int cols, int rows,
                  int cx, int cy, int radius, byte r,byte g,byte b );

extern void ppmd_text(byte *pixels, int cols, int rows,
               int x, int y, int height, int angle, char *s, byte r,byte g,byte b );

extern void mri_drawline( MRI_IMAGE *im , int x0,int y0, int x1,int y1, byte r,byte g,byte b );

extern void mri_drawfilledrectangle( MRI_IMAGE *im ,
                              int x, int y, int width, int height , byte r,byte g,byte b );

extern void mri_drawemptyrectangle( MRI_IMAGE *im ,
                             int x, int y, int width, int height , byte r,byte g,byte b );

extern void mri_drawtext( MRI_IMAGE *im ,
                   int x, int y, int height, int angle, char *s,
                   byte r,byte g,byte b );

/**********************************************************************/

#include "coxplot.h"
#undef min
#undef max

extern void set_memplot_RGB_box( int xbot, int ybot, int xtop, int ytop ) ;

extern void memplot_to_RGB_sef( MRI_IMAGE *im , MEM_plotdata * mp ,
                                int start , int end , int freee    ) ;

/************************ Statistics routines *************************/

/**
  if the math library doesn't have the log(gamma(x))
  function (as on Linux, for example)
**/

#ifdef NO_GAMMA
extern double gamma_12    ( double ) ;
extern double gamma       ( double ) ;
extern double gamma_asympt( double ) ;
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

/*-----------------  30 Oct 1996: incorporation of cdflib ----------------*/
#include "cdflib.h"
/*------------------------------------------------------------------------*/

/*-----------------  01 Feb 1998: incoroporation of mcw_glob -------------*/
#include "mcw_glob.h"
/*------------------------------------------------------------------------*/

/*-----------------  02 Feb 1998:
                     incoroporation of 3ddata, 3dmaker, iochan -----------*/

#include "3ddata.h"
#include "thd_maker.h"
#include "thd_iochan.h"
#include "editvol.h"

#include "cs.h"            /* 17 Aug 1998 addition */

#include "multivector.h"   /* 18 May 1999 addition */

#include "afni_environ.h"  /* 07 Jun 1999 addition */
/*------------------------------------------------------------------------*/

#endif /* _MCW_MRILIB_HEADER_ */
