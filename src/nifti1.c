#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include "nifti1.h"

/*****===================================================================*****/
/*****      Sample functions to deal with NIFTI-1 and ANALYZE files      *****/
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
                                                 1==NIFTI-1 (1 file)   */
  int intent_code ;               /* statistic type (or something) */
  float intent_p1, intent_p2,     /* intent parameters */
        intent_p3 ;
  char intent_name[16] ;

  char descrip[80], aux_file[24];

  char *fname ;                   /* header filename (.hdr or .nii) */
  char *iname ;                   /* image filename (.img or .nii)  */
  int   iname_offset ;            /* offset into iname where data starts */
  int   swapsize ;                /* swapping unit in image data */
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

nifti_image * nifti_image_read    ( char *hname , int read_data ) ;
void          nifti_image_load    ( nifti_image *nim ) ;
void          nifti_image_unload  ( nifti_image *nim ) ;
void          nifti_image_free    ( nifti_image *nim ) ;
void          nifti_image_infodump( nifti_image *nim ) ;
void          nifti_image_write   ( nifti_image *nim ) ;

void mat44_to_quatern( mat44 R ,
                       float *qb, float *qc, float *qd,
                       float *qx, float *qy, float *qz,
                       float *dx, float *dy, float *dz, float *qfac ) ;

mat44 quatern_to_mat44( float qb, float qc, float qd,
                        float qx, float qy, float qz,
                        float dx, float dy, float dz, float qfac );

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

/*---------------------------------------------------------------------------*/
/* Return a pointer to a string holding the name of a NIFTI datatype.
   Don't free() or modify this string!  It points to static storage.
-----------------------------------------------------------------------------*/

char *nifti_datatype_string( int dt )
{
   switch( dt ){
     case DT_UNKNOWN:    return "UNKNOWN"    ;
     case DT_BINARY:     return "BINARY"     ;
     case DT_INT8:       return "INT8"       ;
     case DT_UINT8:      return "UINT8"      ;
     case DT_INT16:      return "INT16"      ;
     case DT_UINT16:     return "UINT16"     ;
     case DT_INT32:      return "INT32"      ;
     case DT_UINT32:     return "UINT32"     ;
     case DT_INT64:      return "INT64"      ;
     case DT_UINT64:     return "UINT64"     ;
     case DT_FLOAT32:    return "FLOAT32"    ;
     case DT_FLOAT64:    return "FLOAT64"    ;
     case DT_FLOAT128:   return "FLOAT128"   ;
     case DT_COMPLEX64:  return "COMPLEX64"  ;
     case DT_COMPLEX128: return "COMPLEX128" ;
     case DT_COMPLEX256: return "COMPLEX256" ;
     case DT_RGB24:      return "RGB24"      ;
   }

   return "**ILLEGAL**" ;
}

/*---------------------------------------------------------------------------*/
/* Return a pointer to a string holding the name of a NIFTI units type.
   Don't free() or modify this string!  It points to static storage.
-----------------------------------------------------------------------------*/

char *nifti_units_string( int uu )
{
   switch( uu ){
     case NIFTI_UNITS_METER:  return "m" ;
     case NIFTI_UNITS_MM:     return "mm" ;
     case NIFTI_UNITS_MICRON: return "um" ;
     case NIFTI_UNITS_SEC:    return "s" ;
     case NIFTI_UNITS_MSEC:   return "ms" ;
     case NIFTI_UNITS_USEC:   return "us" ;
     case NIFTI_UNITS_HZ:     return "Hz" ;
     case NIFTI_UNITS_PPM:    return "ppm" ;
   }
   return "Unknown" ;
}

/*---------------------------------------------------------------------------*/
/* Return a pointer to a string holding the name of a NIFTI transform type.
   Don't free() or modify this string!  It points to static storage.
-----------------------------------------------------------------------------*/

char *nifti_xform_string( int xx )
{
   switch( xx ){
     case NIFTI_XFORM_SCANNER_ANAT:  return "Scanner Anat" ;
     case NIFTI_XFORM_ALIGNED_ANAT:  return "Aligned Anat" ;
     case NIFTI_XFORM_TALAIRACH:     return "Talairach" ;
     case NIFTI_XFORM_MNI_152:       return "MNI_152" ;
   }
   return "Unknown" ;
}

/*---------------------------------------------------------------------------*/
/* Return a pointer to a string holding the name of a NIFTI intent type.
   Don't free() or modify this string!  It points to static storage.
-----------------------------------------------------------------------------*/

char *nifti_intent_string( int ii )
{
   switch( ii ){
     case NIFTI_INTENT_CORREL:     return "Correlation" ;
     case NIFTI_INTENT_TTEST:      return "T-statistic" ;
     case NIFTI_INTENT_FTEST:      return "F-statistic" ;
     case NIFTI_INTENT_ZSCORE:     return "Z-score"     ;
     case NIFTI_INTENT_CHISQ:      return "Chi-squared" ;
     case NIFTI_INTENT_BETA:       return "Beta distribution" ;
     case NIFTI_INTENT_BINOM:      return "Binomial distribution" ;
     case NIFTI_INTENT_GAMMA:      return "Gamma distribution" ;
     case NIFTI_INTENT_POISSON:    return "Poisson distribution" ;
     case NIFTI_INTENT_NORMAL:     return "Normal distribution" ;
     case NIFTI_INTENT_FTEST_NONC: return "F-statistic noncentral" ;
     case NIFTI_INTENT_CHISQ_NONC: return "Chi-squared noncentral" ;
     case NIFTI_INTENT_LOGISTIC:   return "Logistic distribution" ;
     case NIFTI_INTENT_LAPLACE:    return "Laplace distribution" ;
     case NIFTI_INTENT_UNIFORM:    return "Uniform distribition" ;
     case NIFTI_INTENT_TTEST_NONC: return "T-statistic noncentral" ;
     case NIFTI_INTENT_WEIBULL:    return "Weibull distribution" ;
     case NIFTI_INTENT_CHI:        return "Chi distribution" ;
     case NIFTI_INTENT_INVGAUSS:   return "Inverse Gaussian distribution" ;
     case NIFTI_INTENT_EXTVAL:     return "Extreme Value distribution" ;
     case NIFTI_INTENT_PVAL:       return "P-value" ;

     case NIFTI_INTENT_ESTIMATE:   return "Estimate" ;
     case NIFTI_INTENT_LABEL:      return "Label index" ;
     case NIFTI_INTENT_NEURONAME:  return "NeuroNames index" ;
     case NIFTI_INTENT_GENMATRIX:  return "General matrix" ;
     case NIFTI_INTENT_SYMMATRIX:  return "Symmetric matrix" ;
     case NIFTI_INTENT_DISPVECT:   return "Displacement vector" ;
     case NIFTI_INTENT_VECTOR:     return "Vector" ;
     case NIFTI_INTENT_POINTSET:   return "Pointset" ;
     case NIFTI_INTENT_TRIANGLE:   return "Triangle" ;
     case NIFTI_INTENT_QUATERNION: return "Quaternion" ;
   }
   return "Unknown" ;
}

/*---------------------------------------------------------------------------*/
/* Given the quaternion parameters (etc.), compute a transformation matrix.
   See comments in nifti1.h for details.
     - qb,qc,qd = quaternion parameters
     - qx,qy,qz = offset parameters
     - dx,dy,dz = grid stepsizes (non-negative inputs are set to 1.0)
     - qfac     = sign of dz step (< 0 is negative; >= 0 is positive)
   If qx=qy=qz=0, dx=dy=dz=1, then the output is a rotation matrix.
   For qfac >= 0, the rotation is proper.
   For qfac <  0, the rotation is improper.
-----------------------------------------------------------------------------*/

mat44 quatern_to_mat44( float qb, float qc, float qd,
                        float qx, float qy, float qz,
                        float dx, float dy, float dz, float qfac )
{
   mat44 R ;
   double a,b=qb,c=qc,d=qd , xd,yd,zd ;

   /* last row is always [ 0 0 0 1 ] */

   R.m[3][0]=R.m[3][1]=R.m[3][2] = 0.0 ; R.m[3][3]= 1.0 ;

   /* compute a parameter from b,c,d */

   a = 1.0l - (b*b + c*c + d*d) ;
   if( a < 1.e-7l ){                   /* special case */
     a = 1.0l / sqrt(b*b+c*c+d*d) ;
     b *= a ; c *= a ; d *= a ;        /* normalize (b,c,d) vector */
     a = 0.0l ;                        /* a = 0 ==> 180 degree rotation */
   } else{
     a = sqrt(a) ;                     /* angle = 2*arccos(a) */
   }

   /* load rotation matrix, including scaling factors for voxel sizes */

   xd = (dx > 0.0) ? dx : 1.0l ;       /* make sure are positive */
   yd = (dy > 0.0) ? dy : 1.0l ;
   zd = (dz > 0.0) ? dz : 1.0l ;

   if( qfac < 0.0 ) zd = -zd ;         /* left handedness? */

   R.m[0][0] =        (a*a+b*b-c*c-d*d) * xd ;
   R.m[0][1] = 2.0l * (b*c-a*d        ) * yd ;
   R.m[0][2] = 2.0l * (b*d+a*c        ) * zd ;
   R.m[1][0] = 2.0l * (b*c+a*d        ) * xd ;
   R.m[1][1] =        (a*a+c*c-b*b-d*d) * yd ;
   R.m[1][2] = 2.0l * (c*d-a*b        ) * zd ;
   R.m[2][0] = 2.0l * (b*d-a*c        ) * xd ;
   R.m[2][1] = 2.0l * (c*d+a*b        ) * yd ;
   R.m[2][2] =        (a*a+d*d-c*c-b*b) * zd ;

   /* load offsets */

   R.m[0][3] = qx ; R.m[1][3] = qy ; R.m[2][3] = qz ;

   return R ;
}

/*---------------------------------------------------------------------------*/
/* Given the 3x4 upper corner of the matrix R, compute the quaternion
   parameters that fit it.  See comments in nifti1.h for details.
     - Any NULL pointer on input won't get assigned (e.g., if you don't want
       dx,dy,dz, just pass NULL in for those pointers).
     - If the 3 input matrix columns are NOT orthogonal, they will be
       orthogonalized prior to calculating the parameters, using
       the polar decomposition to find the orthogonal matrix closest
       to the column-normalized input matrix.
     - However, if the 3 input matrix columns are NOT orthogonal, then
       the matrix produces by quatern_to_mat44 WILL have orthogonal
       columns, so it won't be the same as the matrix input here.
       This "feature" is because the NIFTI 'qform' transform is
       deliberately not fully general -- it is intended to model a volume
       with perpendicular axes.
     - If the 3 input matrix columns are not even linearly independent,
       you'll just have to take your luck, won't you?
-----------------------------------------------------------------------------*/

#undef  ASSIF                                 /* assign v to *p, if possible */
#define ASSIF(p,v) if( (p)!=NULL ) *(p) = (v)

void mat44_to_quatern( mat44 R ,
                       float *qb, float *qc, float *qd,
                       float *qx, float *qy, float *qz,
                       float *dx, float *dy, float *dz, float *qfac )
{
   double r11,r12,r13 , r21,r22,r23 , r31,r32,r33 ;
   double xd,yd,zd , a,b,c,d ;
   mat33 P,Q ;

   /* offset outputs are read write out of input matrix  */

   ASSIF(qx,R.m[0][3]) ; ASSIF(qy,R.m[1][3]) ; ASSIF(qz,R.m[2][3]) ;

   /* load 3x3 matrix into local variables */

   r11 = R.m[0][0] ; r12 = R.m[0][1] ; r13 = R.m[0][2] ;
   r21 = R.m[1][0] ; r22 = R.m[1][1] ; r23 = R.m[1][2] ;
   r31 = R.m[2][0] ; r32 = R.m[2][1] ; r33 = R.m[2][2] ;

   /* compute lengths of each column; these determine grid spacings  */

   xd = sqrt( r11*r11 + r21*r21 + r31*r31 ) ;
   yd = sqrt( r12*r12 + r22*r22 + r32*r32 ) ;
   zd = sqrt( r13*r13 + r23*r23 + r33*r33 ) ;

   /* if a column length is zero, patch the trouble */

   if( xd == 0.0l ){ r11 = 1.0l ; r21 = r31 = 0.0l ; xd = 1.0l ; }
   if( yd == 0.0l ){ r22 = 1.0l ; r12 = r32 = 0.0l ; yd = 1.0l ; }
   if( zd == 0.0l ){ r33 = 1.0l ; r13 = r23 = 0.0l ; zd = 1.0l ; }

   /* assign the output lengths */

   ASSIF(dx,xd) ; ASSIF(dy,yd) ; ASSIF(dz,zd) ;

   /* normalize the columns */

   r11 /= xd ; r21 /= xd ; r31 /= xd ;
   r12 /= yd ; r22 /= yd ; r32 /= yd ;
   r13 /= zd ; r23 /= zd ; r33 /= zd ;

   /* At this point, the matrix has normal columns, but we have to allow
      for the fact that the hideous user may not have given us a matrix
      with orthogonal columns.

      So, now find the orthogonal matrix closest to the current matrix.

      One reason for using the polar decomposition to get this
      orthogonal matrix, rather than just directly orthogonalizing
      the columns, is so that inputting the inverse matrix to R
      will result in the inverse orthogonal matrix at this point.
      If we just orthogonalized the columns, this wouldn't necessarily hold. */

   Q.m[0][0] = r11 ; Q.m[0][1] = r12 ; Q.m[0][2] = r13 ; /* load Q */
   Q.m[1][0] = r21 ; Q.m[1][1] = r22 ; Q.m[1][2] = r23 ;
   Q.m[2][0] = r31 ; Q.m[2][1] = r32 ; Q.m[2][2] = r33 ;

   P = mat33_polar(Q) ;  /* P is orthog matrix closest to Q */

   r11 = P.m[0][0] ; r12 = P.m[0][1] ; r13 = P.m[0][2] ; /* unload */
   r21 = P.m[1][0] ; r22 = P.m[1][1] ; r23 = P.m[1][2] ;
   r31 = P.m[2][0] ; r32 = P.m[2][1] ; r33 = P.m[2][2] ;

   /*                            [ r11 r12 r13 ]               */
   /* at this point, the matrix  [ r21 r22 r23 ] is orthogonal */
   /*                            [ r31 r32 r33 ]               */

   /* compute the determinant to determine if it is proper */

   zd = r11*r22*r33-r11*r32*r23-r21*r12*r33
       +r21*r32*r13+r31*r12*r23-r31*r22*r13 ;  /* should be -1 or 1 */

   if( zd > 0 ){             /* proper */
     ASSIF(qfac,1.0) ;
   } else {                  /* improper ==> flip 3rd column */
     ASSIF(qfac,-1.0) ;
     r13 = -r13 ; r23 = -r23 ; r33 = -r33 ;
   }

   /* now, compute quaternion parameters */

   a = r11 + r22 + r33 + 1.0l ;

   if( a > 0.5l ){                /* simplest case */
     a = 0.5l * sqrt(a) ;
     b = 0.25l * (r32-r23) / a ;
     c = 0.25l * (r13-r31) / a ;
     d = 0.25l * (r21-r12) / a ;
   } else {                       /* trickier case */
     xd = 1.0 + r11 - (r22+r33) ;  /* 4*b*b */
     yd = 1.0 + r22 - (r11+r33) ;  /* 4*c*c */
     zd = 1.0 + r33 - (r11+r22) ;  /* 4*d*d */
     if( xd > 1.0 ){
       b = 0.5l * sqrt(xd) ;
       c = 0.25l* (r12+r21) / b ;
       d = 0.25l* (r13+r31) / b ;
       a = 0.25l* (r32-r23) / b ;
     } else if( yd > 1.0 ){
       c = 0.5l * sqrt(yd) ;
       b = 0.25l* (r12+r21) / c ;
       d = 0.25l* (r23+r32) / c ;
       a = 0.25l* (r13-r31) / c ;
     } else {
       d = 0.5l * sqrt(zd) ;
       b = 0.25l* (r13+r31) / d ;
       c = 0.25l* (r23+r32) / d ;
       a = 0.25l* (r21-r12) / d ;
     }
     if( a < 0.0l ){ b=-b ; c=-c ; d=-d; a=-a; }
   }

   ASSIF(qb,b) ; ASSIF(qc,c) ; ASSIF(qd,d) ;
   return ;
}

#undef ASSIF

/*---------------------------------------------------------------------------*/
/* Compute the inverse of a bordered 4x4 matrix.
   Some numerical code fragments were generated by Maple 8.
   If a singular matrix is input, the output matrix will be all zero.
   You can check for this by examining the [3][3] element, which will
   be 1.0 for the normal case and 0.0 for the bad case.
-----------------------------------------------------------------------------*/

mat44 mat44_inverse( mat44 R )
{
   double r11,r12,r13,r21,r22,r23,r31,r32,r33,v1,v2,v3 , deti ;
   mat44 Q ;
                                                       /** INPUT MATRIX IS: **/
   r11 = R.m[0][0]; r12 = R.m[0][1]; r13 = R.m[0][2];  /* [ r11 r12 r13 v1 ] */
   r21 = R.m[1][0]; r22 = R.m[1][1]; r23 = R.m[1][2];  /* [ r21 r22 r23 v2 ] */
   r31 = R.m[2][0]; r32 = R.m[2][1]; r33 = R.m[2][2];  /* [ r31 r32 r33 v3 ] */
   v1  = R.m[0][3]; v2  = R.m[1][3]; v3  = R.m[2][3];  /* [  0   0   0   1 ] */

   deti = r11*r22*r33-r11*r32*r23-r21*r12*r33
         +r21*r32*r13+r31*r12*r23-r31*r22*r13 ;

   if( deti != 0.0l ) deti = 1.0l / deti ;

   Q.m[0][0] = deti*( r22*r33-r32*r23) ;
   Q.m[0][1] = deti*(-r12*r33+r32*r13) ;
   Q.m[0][2] = deti*( r12*r23-r22*r13) ;
   Q.m[0][3] = deti*(-r12*r23*v3+r12*v2*r33+r22*r13*v3
                     -r22*v1*r33-r32*r13*v2+r32*v1*r23) ;

   Q.m[1][0] = deti*(-r21*r33+r31*r23) ;
   Q.m[1][1] = deti*( r11*r33-r31*r13) ;
   Q.m[1][2] = deti*(-r11*r23+r21*r13) ;
   Q.m[1][3] = deti*( r11*r23*v3-r11*v2*r33-r21*r13*v3
                     +r21*v1*r33+r31*r13*v2-r31*v1*r23) ;

   Q.m[2][0] = deti*( r21*r32-r31*r22) ;
   Q.m[2][1] = deti*(-r11*r32+r31*r12) ;
   Q.m[2][2] = deti*( r11*r22-r21*r12) ;
   Q.m[2][3] = deti*(-r11*r22*v3+r11*r32*v2+r21*r12*v3
                     -r21*r32*v1-r31*r12*v2+r31*r22*v1) ;

   Q.m[3][0] = Q.m[3][1] = Q.m[3][2] = 0.0l ;
   Q.m[3][3] = (deti == 0.0l) ? 0.0l : 1.0l ; /* failure flag if deti == 0 */

   return Q ;
}

/*---------------------------------------------------------------------------*/

mat33 mat33_inverse( mat33 R )   /* inverse of 3x3 matrix */
{
   double r11,r12,r13,r21,r22,r23,r31,r32,r33 , deti ;
   mat33 Q ;
                                                       /** INPUT MATRIX: **/
   r11 = R.m[0][0]; r12 = R.m[0][1]; r13 = R.m[0][2];  /* [ r11 r12 r13 ] */
   r21 = R.m[1][0]; r22 = R.m[1][1]; r23 = R.m[1][2];  /* [ r21 r22 r23 ] */
   r31 = R.m[2][0]; r32 = R.m[2][1]; r33 = R.m[2][2];  /* [ r31 r32 r33 ] */

   deti = r11*r22*r33-r11*r32*r23-r21*r12*r33
         +r21*r32*r13+r31*r12*r23-r31*r22*r13 ;

   if( deti != 0.0l ) deti = 1.0l / deti ;

   Q.m[0][0] = deti*( r22*r33-r32*r23) ;
   Q.m[0][1] = deti*(-r12*r33+r32*r13) ;
   Q.m[0][2] = deti*( r12*r23-r22*r13) ;

   Q.m[1][0] = deti*(-r21*r33+r31*r23) ;
   Q.m[1][1] = deti*( r11*r33-r31*r13) ;
   Q.m[1][2] = deti*(-r11*r23+r21*r13) ;

   Q.m[2][0] = deti*( r21*r32-r31*r22) ;
   Q.m[2][1] = deti*(-r11*r32+r31*r12) ;
   Q.m[2][2] = deti*( r11*r22-r21*r12) ;

   return Q ;
}

/*---------------------------------------------------------------------------*/

float mat33_determ( mat33 R )   /* determinant of 3x3 matrix */
{
   double r11,r12,r13,r21,r22,r23,r31,r32,r33 , deti ;
                                                       /** INPUT MATRIX: **/
   r11 = R.m[0][0]; r12 = R.m[0][1]; r13 = R.m[0][2];  /* [ r11 r12 r13 ] */
   r21 = R.m[1][0]; r22 = R.m[1][1]; r23 = R.m[1][2];  /* [ r21 r22 r23 ] */
   r31 = R.m[2][0]; r32 = R.m[2][1]; r33 = R.m[2][2];  /* [ r31 r32 r33 ] */

   return r11*r22*r33-r11*r32*r23-r21*r12*r33
         +r21*r32*r13+r31*r12*r23-r31*r22*r13 ;
}

/*---------------------------------------------------------------------------*/

float mat33_rownorm( mat33 A )  /* max row norm of 3x3 matrix */
{
   float r1,r2,r3 ;

   r1 = fabs(A.m[0][0])+fabs(A.m[0][1])+fabs(A.m[0][2]) ;
   r2 = fabs(A.m[1][0])+fabs(A.m[1][1])+fabs(A.m[1][2]) ;
   r3 = fabs(A.m[2][0])+fabs(A.m[2][1])+fabs(A.m[2][2]) ;
   if( r1 < r2 ) r1 = r2 ;
   if( r1 < r3 ) r1 = r3 ;
   return r1 ;
}

/*---------------------------------------------------------------------------*/

float mat33_colnorm( mat33 A )  /* max column norm of 3x3 matrix */
{
   float r1,r2,r3 ;

   r1 = fabs(A.m[0][0])+fabs(A.m[1][0])+fabs(A.m[2][0]) ;
   r2 = fabs(A.m[0][1])+fabs(A.m[1][1])+fabs(A.m[2][1]) ;
   r3 = fabs(A.m[0][2])+fabs(A.m[1][2])+fabs(A.m[2][2]) ;
   if( r1 < r2 ) r1 = r2 ;
   if( r1 < r3 ) r1 = r3 ;
   return r1 ;
}

/*---------------------------------------------------------------------------*/
/* Polar decomposition of a 3x3 matrix: finds the closest orthogonal matrix
   to input A (in both the Frobenius and L2 norms).  Algorithm is that from
   NJ Higham, SIAM J Sci Stat Comput, 7:1160-1174.
-----------------------------------------------------------------------------*/

mat33 mat33_polar( mat33 A )
{
   mat33 X , Y , Z ;
   float alp,bet,gam,gmi , dif=1.0 ;
   int k=0 ;

   X = A ;

   /* force matrix to be nonsingular */

   gam = mat33_determ(X) ;
   while( gam == 0.0 ){        /* perturb matrix */
     gam = 0.00001 * ( 0.001 + mat33_rownorm(X) ) ;
     X.m[0][0] += gam ; X.m[1][1] += gam ; X.m[2][2] += gam ;
     gam = mat33_determ(X) ;
   }

   while(1){
     Y = mat33_inverse(X) ;
     if( dif > 0.3 ){     /* far from convergence */
       alp = sqrt( mat33_rownorm(X) * mat33_colnorm(X) ) ;
       bet = sqrt( mat33_rownorm(Y) * mat33_colnorm(Y) ) ;
       gam = sqrt( bet / alp ) ;
       gmi = 1.0 / gam ;
     } else {
       gam = gmi = 1.0 ;  /* close to convergence */
     }
     Z.m[0][0] = 0.5 * ( gam*X.m[0][0] + gmi*Y.m[0][0] ) ;
     Z.m[0][1] = 0.5 * ( gam*X.m[0][1] + gmi*Y.m[1][0] ) ;
     Z.m[0][2] = 0.5 * ( gam*X.m[0][2] + gmi*Y.m[2][0] ) ;
     Z.m[1][0] = 0.5 * ( gam*X.m[1][0] + gmi*Y.m[0][1] ) ;
     Z.m[1][1] = 0.5 * ( gam*X.m[1][1] + gmi*Y.m[1][1] ) ;
     Z.m[1][2] = 0.5 * ( gam*X.m[1][2] + gmi*Y.m[2][1] ) ;
     Z.m[2][0] = 0.5 * ( gam*X.m[2][0] + gmi*Y.m[0][2] ) ;
     Z.m[2][1] = 0.5 * ( gam*X.m[2][1] + gmi*Y.m[1][2] ) ;
     Z.m[2][2] = 0.5 * ( gam*X.m[2][2] + gmi*Y.m[2][2] ) ;

     dif = fabs(Z.m[0][0]-X.m[0][0])+fabs(Z.m[0][1]-X.m[0][1])
          +fabs(Z.m[0][2]-X.m[0][2])+fabs(Z.m[1][0]-X.m[1][0])
          +fabs(Z.m[1][1]-X.m[1][1])+fabs(Z.m[1][2]-X.m[1][2])
          +fabs(Z.m[2][0]-X.m[2][0])+fabs(Z.m[2][1]-X.m[2][1])
          +fabs(Z.m[2][2]-X.m[2][2])                          ;

     k = k+1 ;
     if( k > 100 || dif < 3.e-6 ) break ;  /* convergence or exhaustion */
     X = Z ;
   }

   return Z ;
}

/*---------------------------------------------------------------------------*/
/* Routines to swap byte arrays in various ways:
    -  2 at a time:  ab               -> ba               [short]
    -  4 at a time:  abcd             -> dcba             [int, float]
    -  8 at a time:  abcdDCBA         -> ABCDdcba         [long long, double]
    - 16 at a time:  abcdefghHGFEDCBA -> ABCDEFGHhgfedcba [long double]
-----------------------------------------------------------------------------*/

typedef struct { unsigned char a,b ; } twobytes ;

void swap_2bytes( int n , void *ar )    /* 2 bytes at a time */
{
   register int ii ;
   register twobytes *tb = (twobytes *)ar ;
   register unsigned char tt ;

   for( ii=0 ; ii < n ; ii++ ){
     tt = tb[ii].a ; tb[ii].a = tb[ii].b ; tb[ii].b = tt ;
   }
   return ;
}

/*---------------------------------------------------------------------------*/

typedef struct { unsigned char a,b,c,d ; } fourbytes ;

void swap_4bytes( int n , void *ar )    /* 4 bytes at a time */
{
   register int ii ;
   register fourbytes *tb = (fourbytes *)ar ;
   register unsigned char tt ;

   for( ii=0 ; ii < n ; ii++ ){
     tt = tb[ii].a ; tb[ii].a = tb[ii].d ; tb[ii].d = tt ;
     tt = tb[ii].b ; tb[ii].b = tb[ii].c ; tb[ii].c = tt ;
   }
   return ;
}

/*---------------------------------------------------------------------------*/

typedef struct { unsigned char a,b,c,d , D,C,B,A ; } eightbytes ;

void swap_8bytes( int n , void *ar )    /* 8 bytes at a time */
{
   register int ii ;
   register eightbytes *tb = (eightbytes *)ar ;
   register unsigned char tt ;

   for( ii=0 ; ii < n ; ii++ ){
     tt = tb[ii].a ; tb[ii].a = tb[ii].A ; tb[ii].A = tt ;
     tt = tb[ii].b ; tb[ii].b = tb[ii].B ; tb[ii].B = tt ;
     tt = tb[ii].c ; tb[ii].c = tb[ii].C ; tb[ii].C = tt ;
     tt = tb[ii].d ; tb[ii].d = tb[ii].D ; tb[ii].D = tt ;
   }
   return ;
}

/*---------------------------------------------------------------------------*/

typedef struct { unsigned char a,b,c,d,e,f,g,h ,
                               H,G,F,E,D,C,B,A  ; } sixteenbytes ;

void swap_16bytes( int n , void *ar )    /* 16 bytes at a time */
{
   register int ii ;
   register sixteenbytes *tb = (sixteenbytes *)ar ;
   register unsigned char tt ;

   for( ii=0 ; ii < n ; ii++ ){
     tt = tb[ii].a ; tb[ii].a = tb[ii].A ; tb[ii].A = tt ;
     tt = tb[ii].b ; tb[ii].b = tb[ii].B ; tb[ii].B = tt ;
     tt = tb[ii].c ; tb[ii].c = tb[ii].C ; tb[ii].C = tt ;
     tt = tb[ii].d ; tb[ii].d = tb[ii].D ; tb[ii].D = tt ;

     tt = tb[ii].e ; tb[ii].e = tb[ii].E ; tb[ii].E = tt ;
     tt = tb[ii].f ; tb[ii].f = tb[ii].F ; tb[ii].F = tt ;
     tt = tb[ii].g ; tb[ii].g = tb[ii].G ; tb[ii].G = tt ;
     tt = tb[ii].h ; tb[ii].h = tb[ii].H ; tb[ii].H = tt ;
   }
   return ;
}

/*---------------------------------------------------------------------------*/

void swap_Nbytes( int n , int siz , void *ar )  /* subsuming case */
{
   switch( siz ){
     case 2:  swap_2bytes ( n , ar ) ; break ;
     case 4:  swap_4bytes ( n , ar ) ; break ;
     case 8:  swap_8bytes ( n , ar ) ; break ;
     case 16: swap_16bytes( n , ar ) ; break ;
   }
   return ;
}

/*---------------------------------------------------------------*/
/* Byte swap NIFTI-1 file header in various places and ways.
   If is_nifti is nonzero, will also swap the NIFTI-specific
   components of the header; otherwise, only the components
   common to NIFTI and ANALYZE will be swapped.
---------------------------------------------------------------- */

void swap_nifti_header( struct nifti_1_header *h , int is_nifti )
{

#if 0                /* ANALYZE fields not used by this software */
   swap_4(h->sizeof_hdr) ;
   swap_4(h->extents) ;
   swap_2(h->session_error) ;
   swap_4(h->compressed) ;
   swap_4(h->glmax) ; swap_4(h->glmin) ;
#endif

   /* this stuff is always present, for ANALYZE and NIFTI */

   swap_2bytes( 8 , h->dim ) ;
   swap_4bytes( 8 , h->pixdim ) ;

   swap_2(h->datatype) ;
   swap_2(h->bitpix) ;

   swap_4(h->vox_offset); swap_4(h->cal_max); swap_4(h->cal_min);

   /* this stuff is NIFTI specific */

   if( is_nifti ){
     swap_2(h->qform_code); swap_2(h->sform_code);
     swap_4(h->quatern_b); swap_4(h->quatern_c); swap_4(h->quatern_d);
     swap_4(h->qoffset_x); swap_4(h->qoffset_y); swap_4(h->qoffset_z);
     swap_4(h->intent_p1); swap_4(h->intent_p2); swap_4(h->intent_p3);
     swap_4(h->scl_slope); swap_4(h->scl_inter);
     swap_4bytes(4,h->srow_x);
     swap_4bytes(4,h->srow_y);
     swap_4bytes(4,h->srow_z);
     swap_2(h->intent_code); swap_4(h->toffset);
   }
   return ;
}

#define THIS_IS_UNIX
#ifdef  THIS_IS_UNIX
/*---------------------------------------------------------------------------*/
/* Return the file length (0 if file not found or has no contents).
   This is a Unix-specific function, since it uses stat().
-----------------------------------------------------------------------------*/
#include <sys/types.h>
#include <sys/stat.h>

unsigned int get_filesize( char *pathname )
{
   struct stat buf ; int ii ;

   if( pathname == NULL || *pathname == '\0' ) return 0 ;
   ii = stat( pathname , &buf ); if( ii != 0 ) return 0 ;
   return (unsigned int)buf.st_size ;
}

#else  /*---------- non-Unix version of the above, less efficient -----------*/

unsigned int get_filesize( char *pathname )
{
   FILE *fp ; unsigned int len ;

   if( pathname == NULL || *pathname == '\0' ) return 0 ;
   fp = fopen(pathname,"rb"); if( fp == NULL ) return 0 ;
   fseek(fp,0L,SEEK_END) ; len = (unsigned int)ftell(fp) ;
   fclose(fp) ; return len ;
}

#endif /* THIS_IS_UNIX */

/*--------------------------------------------------------------------------*/
/* Read in a NIFTI-1 or ANALYZE-7.5 file (pair) into a nifti_image struct.
    - Input is .hdr or .nii filename.
    - Return value is NULL if something fails badly.
    - If read_data parameter is nonzero, the image data will actually
      be read in; otherwise, it will have to be read later
      (e.g., using the nifti_image_load() function).
    - The image data will be stored in whatever data format the
      input data is; no scaling will be applied.
    - DT_BINARY data is not supported!
----------------------------------------------------------------------------*/

#undef  ERREX
#define ERREX(msg)                                           \
 do{ fprintf(stderr,"** ERROR: nifti_image_read(%s): %s\n",  \
             (hname != NULL) ? hname : "(null)" , (msg) ) ;  \
     return NULL ; } while(0)

nifti_image * nifti_image_read( char *hname , int read_data )
{
   struct nifti_1_header nhdr ;
   nifti_image *nim ;
   FILE *fp ;
   int   ii , doswap , hlen, ilen, ioff ;
   int   nx,ny,nz,nt,nu,nv,nw , ndim,nvox , ntot ;
   int   is_nifti , is_onefile ;
   float dx,dy,dz,dt,du,dv,dw ;
   short ss ;
   char *iname=NULL ;

   /** check input file(s) for sanity **/

   if( hname == NULL || *hname == '\0' ) ERREX("bad filename") ;

   hlen = strlen(hname) ; if( hlen < 5 ) ERREX("too short filename") ;

   /** open input file **/

   fp = fopen( hname , "rb" ) ;
   if( fp == NULL )                      ERREX("can't open header file") ;

   /** read header **/

   ii = fread( &nhdr , 1 , sizeof(nhdr) , fp ) ;          /* read the thing */
   fclose( fp ) ;                                         /* close the file */
   if( ii < sizeof(nhdr) )               ERREX("bad header read") ;

   /** check if have to swap header bytes **/

   doswap = 0 ;                                           /* swap data flag */
   ss = nhdr.dim[0] ;
   if( ss != 0 ){                            /* check dim[0] for good value */
     if( ss < 0 || ss > 7 ){
       swap_2(ss) ;
       if( ss < 0 || ss > 7 )            ERREX("bad dim[0]") ;
       doswap = 1 ;
     }
   } else {                       /* dim[0] == 0 is illegal, but does occur */
     ii = nhdr.sizeof_hdr ;            /* so check sizeof_hdr field instead */
     if( ii != sizeof(nhdr) ){
       swap_4(ii) ;
       if( ii != sizeof(nhdr) )          ERREX("bad sizeof_hdr") ;
       doswap = 1 ;
     }
   }

   /** determine if this is a NIFTI-1 compliant header **/

   is_nifti = NIFTI_VERSION(nhdr) ;
   if( doswap ) swap_nifti_header( &nhdr , is_nifti ) ;

   if( nhdr.datatype == DT_BINARY ||
       nhdr.datatype == DT_UNKNOWN  )    ERREX("bad datatype") ;

   if( nhdr.dim[1] <= 0 )                ERREX("bad dim[1]") ;

   for( ii=2 ; ii <= 7 ; ii++ )
     if( nhdr.dim[ii] <= 0 ) nhdr.dim[ii] = 1 ;  /* fix bad dim[] values */

   /** if dim[0] is 0, get number of dimensions another way **/

   ndim = nhdr.dim[0] ;
   if( ndim == 0 || is_nifti ){
     for( ii=7 ; ii >= 2 ; ii++ )            /* loop backwards until we  */
       if( nhdr.dim[ii] > 1 ) break ;        /* find a dim bigger than 1 */
     ndim = ii ;
   }

   /* set bad grid spacings to 1.0 */

   for( ii=1 ; ii <= 7 ; ii++ ){
     if( nhdr.pixdim[ii] == 0.0         ||
         !IS_GOOD_FLOAT(nhdr.pixdim[ii])  ) nhdr.pixdim[ii] = 1.0 ;
   }

   /** will read image data from file 'iname' starting at offset 'ioff' **/

   is_onefile = is_nifti && NIFTI_ONEFILE(nhdr) ;

   if( is_onefile ){
     ioff = (int)nhdr.vox_offset ;
     if( ioff < sizeof(nhdr) ) ioff = sizeof(nhdr) ;
   } else {
     ioff = 0 ;
   }

   iname = strdup(hname) ;
   if( !is_onefile ) strcpy(iname+hlen-4,".img") ; /* create .img filename */

   ilen = get_filesize(iname) ;            /* find size of image data file */

   if( ilen <= ioff ){ free(iname) ; ERREX("bad data file") ; }

   /*=== create output image struct and start to set it up ===*/

   nim = (nifti_image *) calloc( 1 , sizeof(nifti_image) ) ;

   if( is_nifti ) nim->nifti_type = (is_onefile) ? 1 : 2 ;
   else           nim->nifti_type = 0 ;

   /** dimensions of data array **/

   nim->ndim = nim->dim[0] = ndim ;
   nim->nx   = nim->dim[1] = nhdr.dim[1]; nvox  = nim->nx;
   nim->ny   = nim->dim[2] = nhdr.dim[2]; nvox *= nim->ny;
   nim->nz   = nim->dim[3] = nhdr.dim[3]; nvox *= nim->nz;
   nim->nt   = nim->dim[4] = nhdr.dim[4]; nvox *= nim->nt;
   nim->nu   = nim->dim[5] = nhdr.dim[5]; nvox *= nim->nu;
   nim->nv   = nim->dim[6] = nhdr.dim[6]; nvox *= nim->nv;
   nim->nw   = nim->dim[7] = nhdr.dim[7]; nvox *= nim->nw; nim->nvox = nvox;

   /** type of data in voxels and how many bytes per voxel */

   nim->datatype = nhdr.datatype ;

   switch( nim->datatype ){
     default:
       free(nim) ; free(iname) ; ERREX("bad datatype") ;

     case DT_INT8:
     case DT_UINT8:       nim->nbyper =  1 ; nim->swapsize =  0 ; break ;

     case DT_INT16:
     case DT_UINT16:      nim->nbyper =  2 ; nim->swapsize =  2 ; break ;

     case DT_RGB24:       nim->nbyper =  3 ; nim->swapsize =  0 ; break ;

     case DT_INT32:
     case DT_UINT32:
     case DT_FLOAT32:     nim->nbyper =  4 ; nim->swapsize =  4 ; break ;

     case DT_COMPLEX64:   nim->nbyper =  8 ; nim->swapsize =  4 ; break ;

     case DT_FLOAT64:
     case DT_INT64:
     case DT_UINT64:      nim->nbyper =  8 ; nim->swapsize =  8 ; break ;

     case DT_FLOAT128:    nim->nbyper = 16 ; nim->swapsize = 16 ; break ;

     case DT_COMPLEX128:  nim->nbyper = 16 ; nim->swapsize =  8 ; break ;

     case DT_COMPLEX256:  nim->nbyper = 32 ; nim->swapsize = 16 ; break ;
   }
   if( !doswap ) nim->swapsize = 0 ;

   /** grid spacings **/

   nim->dx = nim->pixdim[1] = nhdr.pixdim[1] ;
   nim->dy = nim->pixdim[2] = nhdr.pixdim[2] ;
   nim->dz = nim->pixdim[3] = nhdr.pixdim[3] ;
   nim->dt = nim->pixdim[4] = nhdr.pixdim[4] ;
   nim->du = nim->pixdim[5] = nhdr.pixdim[5] ;
   nim->dv = nim->pixdim[6] = nhdr.pixdim[6] ;
   nim->dw = nim->pixdim[7] = nhdr.pixdim[7] ;

   /** compute qto_xyz transformation from pixel indexes (i,j,k) to (x,y,z) **/

   if( !is_nifti || nhdr.qform_code <= 0 ){ /** default transformation **/

     nim->qto_xyz.m[0][0] = nim->dx ;  /* grid spacings */
     nim->qto_xyz.m[1][1] = nim->dy ;  /* along diagonal */
     nim->qto_xyz.m[2][2] = nim->dz ;

     /* off diagonal is zero */

     nim->qto_xyz.m[0][1]=nim->qto_xyz.m[0][2]=nim->qto_xyz.m[0][3] = 0.0;
     nim->qto_xyz.m[1][0]=nim->qto_xyz.m[1][2]=nim->qto_xyz.m[1][3] = 0.0;
     nim->qto_xyz.m[2][0]=nim->qto_xyz.m[2][1]=nim->qto_xyz.m[2][3] = 0.0;

     /* last row is always [ 0 0 0 1 ] */

     nim->qto_xyz.m[3][0]=nim->qto_xyz.m[3][1]=nim->qto_xyz.m[3][2] = 0.0;
     nim->qto_xyz.m[3][3]= 1.0 ;

     nim->qform_code = NIFTI_XFORM_UNKNOWN ;

   } else {                 /** NIFTI: quaternion-specified transformation **/

     nim->quatern_b = FIXED_FLOAT( nhdr.quatern_b ) ;
     nim->quatern_c = FIXED_FLOAT( nhdr.quatern_c ) ;
     nim->quatern_d = FIXED_FLOAT( nhdr.quatern_d ) ;

     nim->qoffset_x = FIXED_FLOAT(nhdr.qoffset_x) ;
     nim->qoffset_y = FIXED_FLOAT(nhdr.qoffset_y) ;
     nim->qoffset_z = FIXED_FLOAT(nhdr.qoffset_z) ;

     nim->qfac = (nhdr.pixdim[0] < 0.0) ? -1.0 : 1.0 ;  /* left-handedness? */

     nim->qto_xyz = quatern_to_mat44(
                      nim->quatern_b, nim->quatern_c, nim->quatern_c,
                      nim->qoffset_x, nim->qoffset_y, nim->qoffset_z,
                      nim->dx       , nim->dy       , nim->dz       ,
                      nim->qfac                                      ) ;

     nim->qform_code = nhdr.qform_code ;
   }

   /** load inverse transformation (x,y,z) -> (i,j,k) **/

   nim->qto_ijk = mat44_inverse( nim->qto_xyz ) ;

   /** load sto_xyz affine transformation, if present **/

   if( !is_nifti || nhdr.sform_code <= 0 ){ /** no sto transformation **/

     nim->sform_code = NIFTI_XFORM_UNKNOWN ;

   } else {                            /** sto transformation from srow_*[] **/

     nim->sto_xyz.m[0][0] = nhdr.srow_x[0] ;
     nim->sto_xyz.m[0][1] = nhdr.srow_x[1] ;
     nim->sto_xyz.m[0][2] = nhdr.srow_x[2] ;
     nim->sto_xyz.m[0][3] = nhdr.srow_x[3] ;

     nim->sto_xyz.m[1][0] = nhdr.srow_y[0] ;
     nim->sto_xyz.m[1][1] = nhdr.srow_y[1] ;
     nim->sto_xyz.m[1][2] = nhdr.srow_y[2] ;
     nim->sto_xyz.m[1][3] = nhdr.srow_y[3] ;

     nim->sto_xyz.m[2][0] = nhdr.srow_z[0] ;
     nim->sto_xyz.m[2][1] = nhdr.srow_z[1] ;
     nim->sto_xyz.m[2][2] = nhdr.srow_z[2] ;
     nim->sto_xyz.m[2][3] = nhdr.srow_z[3] ;

     /* last row is always [ 0 0 0 1 ] */

     nim->sto_xyz.m[3][0]=nim->sto_xyz.m[3][1]=nim->sto_xyz.m[3][2] = 0.0;
     nim->sto_xyz.m[3][3]= 1.0 ;

     nim->sto_ijk = mat44_inverse( nim->sto_xyz ) ;

     nim->sform_code = nhdr.sform_code ;
   }

   /* miscellaneous NIFTI stuff */

   if( is_nifti ){
     nim->scl_slope   = FIXED_FLOAT( nhdr.scl_slope ) ;
     nim->scl_inter   = FIXED_FLOAT( nhdr.scl_inter ) ;

     nim->intent_code = nhdr.intent_code ;

     nim->intent_p1 = FIXED_FLOAT( nhdr.intent_p1 ) ;
     nim->intent_p2 = FIXED_FLOAT( nhdr.intent_p2 ) ;
     nim->intent_p3 = FIXED_FLOAT( nhdr.intent_p3 ) ;

     nim->toffset   = FIXED_FLOAT( nhdr.toffset ) ;

     memcpy(nim->intent_name,nhdr.intent_name,15); nim->intent_name[15] = '\0';

     nim->xyz_units  = nhdr.xyz_units  ;
     nim->time_units = nhdr.time_units ;
   }

   /* Miscellaneous ANALYZE stuff */

   nim->cal_min = FIXED_FLOAT(nhdr.cal_min) ;
   nim->cal_max = FIXED_FLOAT(nhdr.cal_max) ;

   memcpy(nim->descrip ,nhdr.descrip ,79) ; nim->descrip [79] = '\0' ;
   memcpy(nim->aux_file,nhdr.aux_file,23) ; nim->aux_file[23] = '\0' ;

   /** read the data if desired, then bug out **/

   nim->fname        = strdup(hname) ;  /* save input filename */
   nim->iname        = iname ;          /* save image filename */
   nim->iname_offset = ioff ;

   if( read_data ) nifti_image_load( nim ) ;
   else            nim->data = NULL ;

   return nim ;
}

/*--------------------------------------------------------------------------*/
/* Load the image data from disk into an already-prepared image struct.
----------------------------------------------------------------------------*/

#undef  ERREX
#define ERREX(msg)                                               \
 do{ fprintf(stderr,"** ERROR: nifti_image_load: %s\n",(msg)) ;  \
     return ; } while(0)

void nifti_image_load( nifti_image *nim )
{
   int ntot , ii ;
   FILE *fp ;

   if( nim == NULL      || nim->iname == NULL ||
       nim->nbyper <= 0 || nim->nvox <= 0       ) ERREX("bad input struct") ;

   /** open image data file **/

   fp = fopen( nim->iname , "rb" ) ;
   if( fp == NULL ) ERREX("Can't open data file") ;
   fseek( fp , nim->iname_offset , SEEK_SET ) ;

   /* make space for data, then read all of it in one operation */

   if( nim->data != NULL ) free(nim->data) ;

   ntot      = nim->nbyper * nim->nvox ;            /* total number of bytes */
   nim->data = malloc( ntot ) ;
   if( nim->data == NULL ) ERREX("can't malloc array space") ;

   ii = fread( nim->data , 1 , ntot , fp ) ;              /*** data input! ***/
   fclose( fp ) ;

   /* if read was short, fill rest of array with 0 bytes */

   if( ii < ntot ){
     fprintf(stderr,"++ WARNING: nifti_image_load(%s):\n"
                    "   data bytes needed = %d\n"
                    "   data bytes input  = %d\n"
                    "   number missing    = %d (set to 0)\n",
             nim->iname , ntot, ii, ntot-ii ) ;
     memset( (char *)(nim->data)+ii , 0 , ntot-ii ) ;
   }

   /** byte swap array if needed **/

   if( nim->swapsize > 0 )
     swap_Nbytes( ntot / nim->swapsize , nim->swapsize , nim->data ) ;

#ifdef isfinite
   /** check input float arrays for goodness, and fix bad numbers **/

   switch( nim->datatype ){

     case NIFTI_TYPE_FLOAT32:
     case NIFTI_TYPE_COMPLEX64:{
       register float *far = (float *)nim->data ; register int jj,nj ;
       nj = ntot / nim->swapsize ;
       for( jj=0 ; jj < nj ; jj++ ) far[jj] = FIXED_FLOAT(far[jj]);
     }
     break ;

     case NIFTI_TYPE_FLOAT64:
     case NIFTI_TYPE_COMPLEX128:{
       register double *far = (double *)nim->data ; register int jj,nj ;
       nj = ntot / nim->swapsize ;
       for( jj=0 ; jj < nj ; jj++ ) far[jj] = FIXED_FLOAT(far[jj]);
     }
     break ;

   }
#endif

   return ;
}

/*--------------------------------------------------------------------------*/
/* Unload the data in a nifti_image struct, but keep the metadata.
----------------------------------------------------------------------------*/

void nifti_image_unload( nifti_image *nim )
{
   if( nim != NULL && nim->data != NULL ){
     free(nim->data) ; nim->data = NULL ;
   }
   return ;
}

/*--------------------------------------------------------------------------*/
/* Free a nifti_image struct that was read by nifti_image_read().
----------------------------------------------------------------------------*/

void nifti_image_free( nifti_image *nim )
{
   if( nim == NULL ) return ;
   if( nim->fname != NULL ) free(nim->fname) ;
   if( nim->iname != NULL ) free(nim->iname) ;
   if( nim->data  != NULL ) free(nim->data ) ;
   free(nim) ; return ;
}

/*--------------------------------------------------------------------------*/
/* Print to stdout some info about a nifti_image struct.
----------------------------------------------------------------------------*/

void nifti_image_infodump( nifti_image *nim )
{
   printf("\n"
          "++++++++++++++++++++++++\n"
          "++ nifti_image_infodump:" ) ;

   if( nim == NULL ){
     printf(" ?? input is NULL ??!!\n\n") ; return ;
   } else
     printf("\n") ;

   printf("  NIFTI storage type = %s\n",
          (nim->nifti_type == 0 ) ? "ANALYZE-7.5"
         :(nim->nifti_type == 1 ) ? "NIFTI-1 in 1 file"
         :                          "NIFTI-1 in 2 files" ) ;
   printf("  header filename = %s\n",nim->fname) ;
   printf("  image  filename = %s  offset = %d\n",
          nim->iname,nim->iname_offset) ;

   printf("  ndim = %3d\n"
          "  nx   = %3d    dx = %g\n"
          "  ny   = %3d    dy = %g\n"
          "  nz   = %3d    dz = %g\n"
          "  nt   = %3d    dt = %g\n"
          "  nu   = %3d    du = %g\n"
          "  nv   = %3d    dv = %g\n"
          "  nw   = %3d    dw = %g\n" ,
       nim->ndim ,
       nim->nx , nim->dx , nim->ny , nim->dy ,
       nim->nz , nim->dz , nim->nt , nim->dt ,
       nim->nu , nim->du , nim->nv , nim->dv , nim->nw , nim->dw ) ;

   printf("  nvox = %d  nbyper = %d  datatype = %d (%s)\n",
          nim->nvox , nim->nbyper ,
          nim->datatype , nifti_datatype_string(nim->datatype) ) ;

   printf("  cal_min = %g  cal_max = %g\n" ,
          nim->cal_min   , nim->cal_max         ) ;

   if( nim->nifti_type > 0 ){
     printf("  scl_slope = %g  scl_inter = %g\n" ,
            nim->scl_slope , nim->scl_inter       ) ;

     printf("  intent_code = %d (%s)\n"
            "  intent_p1=%g  intent_p2=%g  intent_p3=%g\n" ,
            nim->intent_code, nifti_intent_string(nim->intent_code) ,
            nim->intent_p1, nim->intent_p2, nim->intent_p3 ) ;

     if( nim->intent_name[0] != '\0' )
       printf("  intent_name = %s\n",nim->intent_name) ;

     printf("  toffset = %g\n",nim->toffset) ;
     printf("  xyz_units  = %d (%s)\n"
            "  time_units = %d (%s)\n" ,
            nim->xyz_units ,nifti_units_string(nim->xyz_units),
            nim->time_units,nifti_units_string(nim->time_units) ) ;
   }

   if( nim->descrip[0] != '\0' )
     printf("  descrip = %s\n",nim->descrip) ;

   if( nim->aux_file[0] != '\0' )
     printf("  aux_file = %s\n",nim->aux_file) ;

   if( nim->qform_code > 0 ){
     printf("  qform_code = %d (%s)\n"
            "  qto_xyz matrix =\n"
            "    %9.6f %9.6f %9.6f   %9.6f\n"
            "    %9.6f %9.6f %9.6f   %9.6f\n"
            "    %9.6f %9.6f %9.6f   %9.6f\n"
            "    %9.6f %9.6f %9.6f   %9.6f\n" ,
         nim->qform_code      , nifti_xform_string(nim->qform_code) ,
         nim->qto_xyz.m[0][0] , nim->qto_xyz.m[0][1] ,
         nim->qto_xyz.m[0][2] , nim->qto_xyz.m[0][3] ,
         nim->qto_xyz.m[1][0] , nim->qto_xyz.m[1][1] ,
         nim->qto_xyz.m[1][2] , nim->qto_xyz.m[1][3] ,
         nim->qto_xyz.m[2][0] , nim->qto_xyz.m[2][1] ,
         nim->qto_xyz.m[2][2] , nim->qto_xyz.m[2][3] ,
         nim->qto_xyz.m[3][0] , nim->qto_xyz.m[3][1] ,
         nim->qto_xyz.m[3][2] , nim->qto_xyz.m[3][3]  ) ;

     printf("  qto_ijk matrix =\n"
            "    %9.6f %9.6f %9.6f   %9.6f\n"
            "    %9.6f %9.6f %9.6f   %9.6f\n"
            "    %9.6f %9.6f %9.6f   %9.6f\n"
            "    %9.6f %9.6f %9.6f   %9.6f\n" ,
         nim->qto_ijk.m[0][0] , nim->qto_ijk.m[0][1] ,
         nim->qto_ijk.m[0][2] , nim->qto_ijk.m[0][3] ,
         nim->qto_ijk.m[1][0] , nim->qto_ijk.m[1][1] ,
         nim->qto_ijk.m[1][2] , nim->qto_ijk.m[1][3] ,
         nim->qto_ijk.m[2][0] , nim->qto_ijk.m[2][1] ,
         nim->qto_ijk.m[2][2] , nim->qto_ijk.m[2][3] ,
         nim->qto_ijk.m[3][0] , nim->qto_ijk.m[3][1] ,
         nim->qto_ijk.m[3][2] , nim->qto_ijk.m[3][3]  ) ;

     printf("  quatern_b = %g  quatern_c = %g  quatern_c = %g\n"
            "  qoffset_x = %g  qoffset_y = %g  qoffset_z = %g\n"
            "  qfac = %g\n" ,
         nim->quatern_b , nim->quatern_c , nim->quatern_c ,
         nim->qoffset_x , nim->qoffset_y , nim->qoffset_z , nim->qfac ) ;
   }

   if( nim->sform_code != NIFTI_XFORM_UNKNOWN ){
     printf("  sform_code = %d (%s)\n"
            "  sto_xyz matrix =\n"
            "    %9.6f %9.6f %9.6f   %9.6f\n"
            "    %9.6f %9.6f %9.6f   %9.6f\n"
            "    %9.6f %9.6f %9.6f   %9.6f\n"
            "    %9.6f %9.6f %9.6f   %9.6f\n" ,
         nim->sform_code      , nifti_xform_string(nim->sform_code) ,
         nim->sto_xyz.m[0][0] , nim->sto_xyz.m[0][1] ,
         nim->sto_xyz.m[0][2] , nim->sto_xyz.m[0][3] ,
         nim->sto_xyz.m[1][0] , nim->sto_xyz.m[1][1] ,
         nim->sto_xyz.m[1][2] , nim->sto_xyz.m[1][3] ,
         nim->sto_xyz.m[2][0] , nim->sto_xyz.m[2][1] ,
         nim->sto_xyz.m[2][2] , nim->sto_xyz.m[2][3] ,
         nim->sto_xyz.m[3][0] , nim->sto_xyz.m[3][1] ,
         nim->sto_xyz.m[3][2] , nim->sto_xyz.m[3][3]  ) ;

     printf("  sto_ijk matrix =\n"
            "    %9.6f %9.6f %9.6f   %9.6f\n"
            "    %9.6f %9.6f %9.6f   %9.6f\n"
            "    %9.6f %9.6f %9.6f   %9.6f\n"
            "    %9.6f %9.6f %9.6f   %9.6f\n" ,
         nim->sto_ijk.m[0][0] , nim->sto_ijk.m[0][1] ,
         nim->sto_ijk.m[0][2] , nim->sto_ijk.m[0][3] ,
         nim->sto_ijk.m[1][0] , nim->sto_ijk.m[1][1] ,
         nim->sto_ijk.m[1][2] , nim->sto_ijk.m[1][3] ,
         nim->sto_ijk.m[2][0] , nim->sto_ijk.m[2][1] ,
         nim->sto_ijk.m[2][2] , nim->sto_ijk.m[2][3] ,
         nim->sto_ijk.m[3][0] , nim->sto_ijk.m[3][1] ,
         nim->sto_ijk.m[3][2] , nim->sto_ijk.m[3][3]  ) ;
   }

   if( nim->data == NULL )
     printf("  data not loaded\n") ;
   else
     printf("  data loaded at address %p (%u bytes)\n",
            nim->data , (unsigned int)(nim->nbyper*nim->nvox) ) ;

   return ;
}

/*--------------------------------------------------------------------------*/
/* Write a nifti_image to disk.  The following fields of nim affect how
   the output appears:
    - nifti_type = 0 ==> ANALYZE-7.5 format file pair will be written
    - nifti_type = 1 ==> NIFTI-1 format single file will be written
    - nifti_type = 2 ==> NIFTI_1 format file pair will be written
    - fname is the name of the output file (header or header+data)
    - if a file pair is being written, iname is the name of the data file
    - existing files WILL be overwritten with extreme prejudice
    - if qform_code > 0, the quatern_*, qoffset_*, and qfac fields determine
      the qform output, NOT the qto_xyz matrix; if you want to compute these
      fields from the qto_xyz matrix, you can use the utility function
      mat44_to_quatern()
----------------------------------------------------------------------------*/

#undef  ERREX
#define ERREX(msg)                                                \
 do{ fprintf(stderr,"** ERROR: nifti_image_write: %s\n",(msg)) ;  \
     return ; } while(0)

void nifti_image_write( nifti_image *nim )
{
   struct nifti_1_header nhdr ;
   FILE *fp ;
   size_t ss ;

   if( nim        == NULL                          ) ERREX("NULL input") ;
   if( nim->fname == NULL || nim->fname[0] == '\0' ) ERREX("bad fname input") ;
   if( nim->data  == NULL                          ) ERREX("no image data") ;

   memcpy(&nhdr,0,sizeof(nhdr)) ;  /* zero out header, to be safe */

   /* make iname from fname, if needed */

   if( nim->nifti_type != 1 ){            /* writing into 2 files */
     if( nim->iname != NULL && strcmp(nim->fname,nim->fname) == 0 ){
       free(nim->iname) ; nim->iname = NULL ;
     }
     if( nim->iname == NULL ){
       int ll = strlen(nim->fname) ; char *cc ;
       nim->iname = calloc(1,ll+5) ;
       strcpy(nim->iname,nim->fname) ;
       if( ll > 4 ) strcpy(nim->iname+ll-4,".img") ; /* create .img filename */
       else         strcat(nim->iname     ,".img") ;
     }
     nim->iname_offset = 0 ;
   } else {
     nim->iname_offset = sizeof(nhdr) ;   /* writing into 1 file */
   }

   /** load the ANALYZE-7.5 generic parts of the header **/

   nhdr.sizeof_hdr = sizeof(nhdr) ;
   nhdr.regular    = 'r' ;

   nhdr.dim[0] = nim->ndim ;
   nhdr.dim[1] = nim->nx ; nhdr.dim[2] = nim->ny ; nhdr.dim[3] = nim->nz ;
   nhdr.dim[4] = nim->nt ; nhdr.dim[5] = nim->nu ; nhdr.dim[6] = nim->nv ;
   nhdr.dim[7] = nim->nw ;

   nhdr.pixdim[0] = 0.0 ;
   nhdr.pixdim[1] = nim->dx ; nhdr.pixdim[2] = nim->dy ;
   nhdr.pixdim[3] = nim->dz ; nhdr.pixdim[4] = nim->dt ;
   nhdr.pixdim[5] = nim->du ; nhdr.pixdim[6] = nim->dv ;
   nhdr.pixdim[7] = nim->dw ;

   nhdr.datatype = nim->datatype ;
   nhdr.bitpix   = 8 * nim->nbyper ;

   if( nim->cal_max > nim->cal_min ){
     nhdr.cal_max = nim->cal_max ;
     nhdr.cal_min = nim->cal_min ;
   }

   if( nim->scl_slope != 0.0 ){
     nhdr.scl_slope = nim->scl_slope ;
     nhdr.scl_inter = nim->scl_inter ;
   }

   if( nim->descrip[0] != '\0' ){
     memcpy(nhdr.descrip ,nim->descrip ,79) ; nhdr.descrip[79] = '\0' ;
   }
   if( nim->aux_file[0] != '\0' ){
     memcpy(nhdr.aux_file ,nim->aux_file ,23) ; nhdr.aux_file[23] = '\0' ;
   }

   /** Load NIFTI specific stuff into the header **/

   if( nim->nifti_type > 0 ){

     if( nim->nifti_type == 1 ) strcpy(nhdr.magic,"n+1") ;   /* 1 file */
     else                       strcpy(nhdr.magic,"ni1") ;   /* 2 files */

     nhdr.intent_code = nim->intent_code ;
     nhdr.intent_p1   = nim->intent_p1 ;
     nhdr.intent_p2   = nim->intent_p2 ;
     nhdr.intent_p3   = nim->intent_p3 ;
     if( nim->intent_name[0] != '\0' ){
       memcpy(nhdr.intent_name,nim->intent_name,15) ;
       nhdr.intent_name[15] = '\0' ;
     }

     nhdr.vox_offset  = (float) nim->iname_offset ;
     nhdr.xyz_units   = (char) nim->xyz_units ;
     nhdr.time_units  = (char) nim->time_units ;
     nhdr.toffset     = nim->toffset ;

     if( nim->qform_code > 0 ){
       nhdr.qform_code = nim->qform_code ;
       nhdr.quatern_b  = nim->quatern_b ;
       nhdr.quatern_c  = nim->quatern_c ;
       nhdr.quatern_d  = nim->quatern_d ;
       nhdr.qoffset_x  = nim->qoffset_x ;
       nhdr.qoffset_y  = nim->qoffset_y ;
       nhdr.qoffset_z  = nim->qoffset_z ;
       nhdr.pixdim[0]  = nim->qfac ;
     }

     if( nim->sform_code > 0 ){
       nhdr.sform_code = nim->sform_code ;
       nhdr.srow_x[0] = nim->sto_xyz.m[0][0] ;
       nhdr.srow_x[1] = nim->sto_xyz.m[0][1] ;
       nhdr.srow_x[2] = nim->sto_xyz.m[0][2] ;
       nhdr.srow_x[3] = nim->sto_xyz.m[0][3] ;
       nhdr.srow_y[0] = nim->sto_xyz.m[1][0] ;
       nhdr.srow_y[1] = nim->sto_xyz.m[1][1] ;
       nhdr.srow_y[2] = nim->sto_xyz.m[1][2] ;
       nhdr.srow_y[3] = nim->sto_xyz.m[1][3] ;
       nhdr.srow_z[0] = nim->sto_xyz.m[2][0] ;
       nhdr.srow_z[1] = nim->sto_xyz.m[2][1] ;
       nhdr.srow_z[2] = nim->sto_xyz.m[2][2] ;
       nhdr.srow_z[3] = nim->sto_xyz.m[2][3] ;
     }
   }

   /** Open file, write header **/

   fp = fopen( nim->fname , "wb" ) ;
   if( fp == NULL ) ERREX("can't open output file") ;

   ss = fwrite( &nhdr , 1 , sizeof(nhdr) , fp ) ;
   if( ss < sizeof(nhdr) ){
     fclose(fp) ; ERREX("bad write to output file") ;
   }

   /** If not writing 1 file, close header and open image file **/

   if( nim->nifti_type != 1 ){
     fclose(fp) ;
     fp = fopen( nim->iname , "wb" ) ;
     if( fp == NULL ) ERREX("can't open image file") ;
   }

   /** Write all the image data at once **/

   ss = fwrite( nim->data , nim->nbyper , nim->nvox , fp ) ;
   fclose(fp) ;
   if( ss < nim->nvox ) ERREX("bad write to image file") ;

   nim->swapsize = 0 ;  /* don't swap if we read back in */
   return ;
}
