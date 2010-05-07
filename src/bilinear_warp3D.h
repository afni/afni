#ifndef _AFNI_BILINEAR_WARP_HEADER_
#define _AFNI_BILINEAR_WARP_HEADER_

/** This header is independent of other AFNI header files [we hope] **/

#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <math.h>

#undef INLINE
#ifdef __GNUC__
# define INLINE __inline__
#else
# define INLINE /*nada*/
#endif

/*--------------------------------------------------------------------------*/

typedef struct { float v[3] ; }       BLvec ;
typedef struct { float m[3][3] ; }    BLmat ;
typedef struct { float t[3][3][3] ; } BLten ;

typedef struct {
  BLmat a ;
  BLvec b ;
  BLten c ;
} BL_standard_warp ;  /* inv[I+c.x][a.x+b] */

typedef struct {
  BLmat e , g ;
  BLvec h , t ;
  BLten f ;
} BL_general_warp ;   /* inv[e+f.x][g.x+h]+t */

typedef struct {
  BLmat a ;
  BLvec b ;
} BL_affine_warp ;

extern BL_standard_warp BL_invert_warp     ( BL_standard_warp wi ) ;
extern BL_standard_warp BL_standardize_warp( BL_general_warp  wi ) ;
extern BL_standard_warp BL_warp_from_params( int npar , float *par ) ;


extern BL_affine_warp   BL_affine_from_12_params  ( float *par ) ;
extern BL_affine_warp   BL_affine_from_12_elements( float *par ) ;
extern BL_affine_warp   BL_extract_affine_warp    ( BL_standard_warp wi ) ;
extern BL_standard_warp BL_extend_affine_warp     ( BL_affine_warp wa ) ;

extern BL_standard_warp BL_bilinear_x_affine( BL_standard_warp, BL_affine_warp ) ;
extern BL_standard_warp BL_affine_x_bilinear( BL_affine_warp, BL_standard_warp ) ;

extern void BL_print_standard_warp( char *name , BL_standard_warp ws ) ;
extern int  BL_warp_tensor_status( BL_standard_warp wi ) ;
extern void BL_params_from_warp( BL_standard_warp wi , float *par ) ;

extern void BL_apply_warp( BL_standard_warp wi ,
                           int npt ,
                           float *xi , float *yi , float *zi ,
                           float *xo , float *yo , float *zo  ) ;

#endif /* _AFNI_BILINEAR_WARP_HEADER_ */
