#ifndef _NIFTI_IO_HEADER_
#define _NIFTI_IO_HEADER_

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <ctype.h>
#include "nifti1.h"

/*****===================================================================*****/
/*****         File nifti1_io.h == Declarations for nifti1_io.c          *****/
/*****...................................................................*****/
/*****            This code is released to the public domain.            *****/
/*****...................................................................*****/
/*****  Author: Robert W Cox, SSCC/DIRP/NIMH/NIH/DHHS/USA/EARTH          *****/
/*****  Date:   August 2003                                              *****/
/*****...................................................................*****/
/*****  Neither the National Institutes of Health (NIH), nor any of its  *****/
/*****  employees imply any warranty of usefulness of this software for  *****/
/*****  any purpose, and do not assume any liability for damages,        *****/
/*****  incidental or otherwise, caused by any use of this document.     *****/
/*****===================================================================*****/

/********************** Some sample data structures **************************/

typedef struct {                   /** 4x4 matrix struct **/
  float m[4][4] ;
} mat44 ;

typedef struct {                   /** 3x3 matrix struct **/
  float m[3][3] ;
} mat33 ;

/*...........................................................................*/

typedef struct {                  /** Image storage struct **/

  int ndim ;                      /* last dimension greater than 1 (1..7) */
  int nx,ny,nz , nt,nu,nv,nw ;    /* dimensions of grid array */
  int dim[8] ;                    /* dim[0]=ndim, dim[1]=nx, etc. */
  int nvox ;                      /* number of voxels = nx*ny*nz*... */
  int nbyper ;                    /* bytes per voxel */
  int datatype ;                  /* type of data in voxels: DT_* code */

  float dx,dy,dz , dt,du,dv,dw ;  /* grid spacings */
  float pixdim[8] ;               /* pixdim[1]=dx, etc. */

  float scl_slope , scl_inter ;   /* scaling parameters */

  float cal_min , cal_max ;       /* calibration parameters */

  int qform_code , sform_code ;   /* codes for (x,y,z) space meaning */

  float quatern_b, quatern_c,     /* quaternion transform parameters */
        quatern_d, qoffset_x,     /* [when writing a dataset,  these ] */
        qoffset_y, qoffset_z,     /* [are used for qform, NOT qto_xyz] */
        qfac                 ;

  mat44 qto_xyz ;                 /* qform: transform (i,j,k) to (x,y,z) */
  mat44 qto_ijk ;                 /* qform: transform (x,y,z) to (i,j,k) */

  mat44 sto_xyz ;                 /* sform: transform (i,j,k) to (x,y,z) */
  mat44 sto_ijk ;                 /* sform: transform (x,y,z) to (i,j,k) */

  float toffset ;                 /* time coordinate offset */

  int xyz_units , time_units ;    /* dx,dy,dz & dt units: NIFTI_UNITS_* code */

  int nifti_type ;                /* 0==ANALYZE, 2==NIFTI-1 (2 files),
                                                 1==NIFTI-1 (1 file) ,
                                                 3==NIFTI-ASCII (1 file) */
  int intent_code ;               /* statistic type (or something) */
  float intent_p1, intent_p2,     /* intent parameters */
        intent_p3 ;
  char intent_name[16] ;

  char descrip[80], aux_file[24];

  char *fname ;                   /* header filename (.hdr or .nii) */
  char *iname ;                   /* image filename (.img or .nii)  */
  int   iname_offset ;            /* offset into iname where data starts */
  int   swapsize ;                /* swapping unit in image data (might be 0) */
  int   byteorder ;               /* byte order on disk (MSB_ or LSB_FIRST) */
  void *data ;                    /* pointer to data: nbyper*nvox bytes */

} nifti_image ;

/*****************************************************************************/
/*--------------- Prototypes of functions defined in this file --------------*/

char *nifti_datatype_string( int dt ) ;
char *nifti_units_string   ( int uu ) ;
char *nifti_intent_string  ( int ii ) ;
char *nifti_xform_string   ( int xx ) ;

mat44 mat44_inverse( mat44 R ) ;

mat33 mat33_inverse( mat33 R ) ;
mat33 mat33_polar  ( mat33 A ) ;
float mat33_rownorm( mat33 A ) ;
float mat33_colnorm( mat33 A ) ;
float mat33_determ ( mat33 R ) ;

void swap_2bytes ( int n , void *ar ) ;
void swap_4bytes ( int n , void *ar ) ;
void swap_8bytes ( int n , void *ar ) ;
void swap_16bytes( int n , void *ar ) ;
void swap_Nbytes ( int n , int siz , void *ar ) ;

void swap_nifti_header( struct nifti_1_header *h , int is_nifti ) ;
unsigned int get_filesize( char *pathname ) ;

nifti_image *nifti_image_read    ( char *hname , int read_data ) ;
void         nifti_image_load    ( nifti_image *nim ) ;
void         nifti_image_unload  ( nifti_image *nim ) ;
void         nifti_image_free    ( nifti_image *nim ) ;
void         nifti_image_write   ( nifti_image *nim ) ;
void         nifti_image_infodump( nifti_image *nim ) ;

char *       nifti_image_to_ascii  ( nifti_image *nim ) ;
nifti_image *nifti_image_from_ascii( char *str        ) ;

int          is_nifti_file( char *hname ) ;

void nifti_datatype_sizes( int datatype , int *nbyper, int *swapsize ) ;

void mat44_to_quatern( mat44 R ,
                       float *qb, float *qc, float *qd,
                       float *qx, float *qy, float *qz,
                       float *dx, float *dy, float *dz, float *qfac ) ;

mat44 quatern_to_mat44( float qb, float qc, float qd,
                        float qx, float qy, float qz,
                        float dx, float dy, float dz, float qfac );

mat44 make_orthog_mat44( float r11, float r12, float r13 ,
                         float r21, float r22, float r23 ,
                         float r31, float r32, float r33  ) ;

int unescape_inplace( char *str ) ;  /* string utility functions */
char *quotize_string( char *str ) ;

int short_order(void) ;              /* CPU byte order */

/*-------------------- Some C convenience macros ----------------------------*/

#undef  swap_2
#undef  swap_4
#define swap_2(s) swap_2bytes(1,&(s))  /* s is a 2-byte short; swap in place */
#define swap_4(v) swap_4bytes(1,&(v))  /* v is a 4-byte value; swap in place */

                        /***** isfinite() is a C99 macro, which is
                               present in many C implementations already *****/

#undef IS_GOOD_FLOAT
#undef FIXED_FLOAT

#ifdef isfinite       /* use isfinite() to check floats/doubles for goodness */
#  define IS_GOOD_FLOAT(x) isfinite(x)       /* check if x is a "good" float */
#  define FIXED_FLOAT(x)   (isfinite(x) ? (x) : 0)           /* fixed if bad */
#else
#  define IS_GOOD_FLOAT(x) 1                               /* don't check it */
#  define FIXED_FLOAT(x)   (x)                               /* don't fix it */
#endif

#undef  ASSIF                                 /* assign v to *p, if possible */
#define ASSIF(p,v) if( (p)!=NULL ) *(p) = (v)

#undef  MSB_FIRST
#undef  LSB_FIRST
#undef  REVERSE_ORDER
#define LSB_FIRST 1
#define MSB_FIRST 2
#define REVERSE_ORDER(x) (3-(x))    /* convert MSB_FIRST <--> LSB_FIRST */

#endif /* _NIFTI_IO_HEADER_ */
