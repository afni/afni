/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/

#ifndef _MCW_MRIIMAGE_HEADER_
#define _MCW_MRIIMAGE_HEADER_

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

/*! Max value of a byte. */

#define MRI_maxbyte         255

/*! Max value of a short. */

#define MRI_maxshort      32767

/*! Max value of an int. */

#define MRI_maxint   2147483647

/*! Determine if a MRI_TYPE is an integer type. */

#define MRI_IS_INT_TYPE(typ) ((typ) < 3)

/*! I suppose that the next C makes this pleonastic. */

#ifndef TYPEDEF_complex
#define TYPEDEF_complex
typedef struct complex { float r , i ; } complex ;
#endif

/*-------*/

/*! Triple to hold RGB bytes. */

#ifndef TYPEDEF_rgbyte
#define TYPEDEF_rgbyte
typedef struct rgbyte { byte r,g,b ; } rgbyte ;  /* 15 Feb 1999 */
#endif

/*-------*/

/*! A union type to hold all possible MRI_IMAGE types.
    This was created before I really understood how to use void *. */

typedef union MRI_DATA {
         byte     *byte_data ;
         short    *short_data ;
         int      *int_data ;
         float    *float_data ;
         double   *double_data ;
         complex  *complex_data ;
         byte     *rgb_data ;      /* Apr 1996: not well supported yet */
         rgba     *rgba_data ;     /* Mar 2002 */
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

#ifdef USE_MRI_LABELS
         char xlab[MRI_LABEL_SIZE] ;  /*!< labels for each dimension */
              ylab[MRI_LABEL_SIZE] ;  /*!< labels for each dimension */
              zlab[MRI_LABEL_SIZE] ;  /*!< labels for each dimension */
              tlab[MRI_LABEL_SIZE] ;  /*!< labels for each dimension */
              ulab[MRI_LABEL_SIZE] ;  /*!< labels for each dimension */
              vlab[MRI_LABEL_SIZE] ;  /*!< labels for each dimension */
              wlab[MRI_LABEL_SIZE] ;  /*!< labels for each dimension */
#endif

#ifdef USE_MRI_DELAY
         char *fname ;   /*!< to read actual image data after delay */
         int foffset ;   /*!< offset into fname of image data */
         int fondisk ;   /*!< flag to indicate if is on disk (?) */
#endif

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

#define MRI_BYTE_PTR(iq)    ((iq)->im.byte_data)
#define MRI_SHORT_PTR(iq)   ((iq)->im.short_data)
#define MRI_INT_PTR(iq)     ((iq)->im.int_data)
#define MRI_FLOAT_PTR(iq)   ((iq)->im.float_data)
#define MRI_DOUBLE_PTR(iq)  ((iq)->im.double_data)
#define MRI_COMPLEX_PTR(iq) ((iq)->im.complex_data)
#define MRI_RGB_PTR(iq)     ((iq)->im.rgb_data)
#define MRI_RGBA_PTR(iq)    ((iq)->im.rgba_data)

#define MRI_BYTE_2D(iq,ix,jy)    MRI_BYTE_PTR(iq)[(ix)+(jy)*(iq)->nx]
#define MRI_SHORT_2D(iq,ix,jy)   MRI_SHORT_PTR(iq)[(ix)+(jy)*(iq)->nx]
#define MRI_INT_2D(iq,ix,jy)     MRI_INT_PTR(iq)[(ix)+(jy)*(iq)->nx]
#define MRI_FLOAT_2D(iq,ix,jy)   MRI_FLOAT_PTR(iq)[(ix)+(jy)*(iq)->nx]
#define MRI_DOUBLE_2D(iq,ix,jy)  MRI_DOUBLE_PTR(iq)[(ix)+(jy)*(iq)->nx]
#define MRI_COMPLEX_2D(iq,ix,jy) MRI_COMPLEX_PTR(iq)[(ix)+(jy)*(iq)->nx]

#endif /* _MCW_MRIIMAGE_HEADER_ */
