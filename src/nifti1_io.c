#define _NIFTI1_IO_C_

#include "nifti1_io.h"   /*** typedefs, prototypes, macros, etc. ***/

/*****===================================================================*****/
/*****     Sample functions to deal with NIFTI-1 and ANALYZE files       *****/
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


/* 
   Modified by: Mark Jenkinson (FMRIB Centre, University of Oxford, UK)
   Date: July/August 2004 

   Mainly adding low-level IO and changing things to allow gzipped files
   to be read and written
   Full backwards compatability should have been maintained
*/

/* global debug level */
static int gni_debug = 0;

/*---------------------------------------------------------------------------*/
/* Return a pointer to a string holding the name of a NIFTI datatype.
   Don't free() or modify this string!  It points to static storage.
-----------------------------------------------------------------------------*/


char *nifti_strdup(const char *str)
{
  char *dup= (char *)malloc( strlen(str)+1 );
  if (dup) strcpy(dup,str);
  return dup;
}
#ifndef HAVE_STRDUP
#endif


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
/* Determine if the datatype code dt is an integer type (1=YES, 0=NO).
-----------------------------------------------------------------------------*/

int nifti_is_inttype( int dt )
{
   switch( dt ){
     case DT_UNKNOWN:    return 0 ;
     case DT_BINARY:     return 0 ;
     case DT_INT8:       return 1 ;
     case DT_UINT8:      return 1 ;
     case DT_INT16:      return 1 ;
     case DT_UINT16:     return 1 ;
     case DT_INT32:      return 1 ;
     case DT_UINT32:     return 1 ;
     case DT_INT64:      return 1 ;
     case DT_UINT64:     return 1 ;
     case DT_FLOAT32:    return 0 ;
     case DT_FLOAT64:    return 0 ;
     case DT_FLOAT128:   return 0 ;
     case DT_COMPLEX64:  return 0 ;
     case DT_COMPLEX128: return 0 ;
     case DT_COMPLEX256: return 0 ;
     case DT_RGB24:      return 1 ;
   }
   return 0 ;
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
     case NIFTI_UNITS_RADS:   return "rad/s" ;
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
     case NIFTI_INTENT_CORREL:     return "Correlation statistic" ;
     case NIFTI_INTENT_TTEST:      return "T-statistic" ;
     case NIFTI_INTENT_FTEST:      return "F-statistic" ;
     case NIFTI_INTENT_ZSCORE:     return "Z-score"     ;
     case NIFTI_INTENT_CHISQ:      return "Chi-squared distribution" ;
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

     case NIFTI_INTENT_LOGPVAL:    return "Log P-value" ;
     case NIFTI_INTENT_LOG10PVAL:  return "Log10 P-value" ;

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

     case NIFTI_INTENT_DIMLESS:    return "Dimensionless number" ;
   }
   return "Unknown" ;
}

/*---------------------------------------------------------------------------*/
/* Return a pointer to a string holding the name of a NIFTI slice_code
   string.  Don't free() or modify this string!  It points to static storage.
-----------------------------------------------------------------------------*/

char *nifti_slice_string( int ss )
{
   switch( ss ){
     case NIFTI_SLICE_SEQ_INC: return "sequential_increasing"  ;
     case NIFTI_SLICE_SEQ_DEC: return "sequential_decreasing"  ;
     case NIFTI_SLICE_ALT_INC: return "alternating_increasing" ;
     case NIFTI_SLICE_ALT_DEC: return "alternating_decreasing" ;
   }
   return "Unknown" ;
}

/*---------------------------------------------------------------------------*/
/* Return a pointer to a string holding the name of a NIFTI orientation
   string.  Don't free() or modify this string!  It points to static storage.
-----------------------------------------------------------------------------*/

char *nifti_orientation_string( int ii )
{
   switch( ii ){
     case NIFTI_L2R: return "Left-to-Right" ;
     case NIFTI_R2L: return "Right-to-Left" ;
     case NIFTI_P2A: return "Posterior-to-Anterior" ;
     case NIFTI_A2P: return "Anterior-to-Posterior" ;
     case NIFTI_I2S: return "Inferior-to-Superior" ;
     case NIFTI_S2I: return "Superior-to-Inferior" ;
   }
   return "Unknown" ;
}

/*--------------------------------------------------------------------------*/
/* Given a datatype code, set number of bytes per voxel and the swapsize.
   The swapsize is set to 0 if this datatype doesn't ever need swapping.
----------------------------------------------------------------------------*/

void nifti_datatype_sizes( int datatype , int *nbyper, int *swapsize )
{
   int nb=0, ss=0 ;
   switch( datatype ){
     case DT_INT8:
     case DT_UINT8:       nb =  1 ; ss =  0 ; break ;

     case DT_INT16:
     case DT_UINT16:      nb =  2 ; ss =  2 ; break ;

     case DT_RGB24:       nb =  3 ; ss =  0 ; break ;

     case DT_INT32:
     case DT_UINT32:
     case DT_FLOAT32:     nb =  4 ; ss =  4 ; break ;

     case DT_COMPLEX64:   nb =  8 ; ss =  4 ; break ;

     case DT_FLOAT64:
     case DT_INT64:
     case DT_UINT64:      nb =  8 ; ss =  8 ; break ;

     case DT_FLOAT128:    nb = 16 ; ss = 16 ; break ;

     case DT_COMPLEX128:  nb = 16 ; ss =  8 ; break ;

     case DT_COMPLEX256:  nb = 32 ; ss = 16 ; break ;
   }

   ASSIF(nbyper,nb) ; ASSIF(swapsize,ss) ; return ;
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
       the matrix produced by quatern_to_mat44 WILL have orthogonal
       columns, so it won't be the same as the matrix input here.
       This "feature" is because the NIFTI 'qform' transform is
       deliberately not fully general -- it is intended to model a volume
       with perpendicular axes.
     - If the 3 input matrix columns are not even linearly independent,
       you'll just have to take your luck, won't you?
-----------------------------------------------------------------------------*/

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
/* Input 9 floats and make an orthgonal mat44 out of them.
   Each row is normalized, then mat33_polar() is used to orthogonalize them.
   If row #3 (r31,r32,r33) is input as zero, then it will be taken to be
   the cross product of rows #1 and #2.

   This function can be used to create a rotation matrix for transforming
   an oblique volume to anatomical coordinates.  For this application:
    - row #1 (r11,r12,r13) is the direction vector along the image i-axis
    - row #2 (r21,r22,r23) is the direction vector along the image j-axis
    - row #3 (r31,r32,r33) is the direction vector along the slice direction
      (if available; otherwise enter it as 0's)
   The first 2 rows can be taken from the DICOM attribute (0020,0037)
   "Image Orientation (Patient)".

   After forming the rotation matrix, the complete affine transformation from
   (i,j,k) grid indexes to (x,y,z) spatial coordinates can be computed by
   multiplying each column by the appropriate grid spacing:
    - column #1 (R.m[0][0],R.m[1][0],R.m[2][0]) by delta-x
    - column #2 (R.m[0][1],R.m[1][1],R.m[2][1]) by delta-y
    - column #3 (R.m[0][2],R.m[1][2],R.m[2][2]) by delta-z
   And by placing the center (x,y,z) coordinates of voxel (0,0,0) into
   the column #4 (R.m[0][3],R.m[1][3],R.m[2][3]).
-----------------------------------------------------------------------------*/

mat44 make_orthog_mat44( float r11, float r12, float r13 ,
                         float r21, float r22, float r23 ,
                         float r31, float r32, float r33  )
{
   mat44 R ;
   mat33 Q , P ;
   double val ;

   R.m[3][0] = R.m[3][1] = R.m[3][2] = 0.0l ; R.m[3][3] = 1.0l ;

   Q.m[0][0] = r11 ; Q.m[0][1] = r12 ; Q.m[0][2] = r13 ; /* load Q */
   Q.m[1][0] = r21 ; Q.m[1][1] = r22 ; Q.m[1][2] = r23 ;
   Q.m[2][0] = r31 ; Q.m[2][1] = r32 ; Q.m[2][2] = r33 ;

   /* normalize row 1 */

   val = Q.m[0][0]*Q.m[0][0] + Q.m[0][1]*Q.m[0][1] + Q.m[0][2]*Q.m[0][2] ;
   if( val > 0.0l ){
     val = 1.0l / sqrt(val) ;
     Q.m[0][0] *= val ; Q.m[0][1] *= val ; Q.m[0][2] *= val ;
   } else {
     Q.m[0][0] = 1.0l ; Q.m[0][1] = 0.0l ; Q.m[0][2] = 0.0l ;
   }

   /* normalize row 2 */

   val = Q.m[1][0]*Q.m[1][0] + Q.m[1][1]*Q.m[1][1] + Q.m[1][2]*Q.m[1][2] ;
   if( val > 0.0l ){
     val = 1.0l / sqrt(val) ;
     Q.m[1][0] *= val ; Q.m[1][1] *= val ; Q.m[1][2] *= val ;
   } else {
     Q.m[1][0] = 0.0l ; Q.m[1][1] = 1.0l ; Q.m[1][2] = 0.0l ;
   }

   /* normalize row 3 */

   val = Q.m[2][0]*Q.m[2][0] + Q.m[2][1]*Q.m[2][1] + Q.m[2][2]*Q.m[2][2] ;
   if( val > 0.0l ){
     val = 1.0l / sqrt(val) ;
     Q.m[2][0] *= val ; Q.m[2][1] *= val ; Q.m[2][2] *= val ;
   } else {
     Q.m[2][0] = Q.m[0][1]*Q.m[1][2] - Q.m[0][2]*Q.m[1][1] ;  /* cross */
     Q.m[2][1] = Q.m[0][2]*Q.m[1][0] - Q.m[0][0]*Q.m[1][2] ;  /* product */
     Q.m[2][2] = Q.m[0][0]*Q.m[1][1] - Q.m[0][1]*Q.m[1][0] ;
   }

   P = mat33_polar(Q) ;  /* P is orthog matrix closest to Q */

   R.m[0][0] = P.m[0][0] ; R.m[0][1] = P.m[0][1] ; R.m[0][2] = P.m[0][2] ;
   R.m[1][0] = P.m[1][0] ; R.m[1][1] = P.m[1][1] ; R.m[1][2] = P.m[1][2] ;
   R.m[2][0] = P.m[2][0] ; R.m[2][1] = P.m[2][1] ; R.m[2][2] = P.m[2][2] ;

   R.m[0][3] = R.m[1][3] = R.m[2][3] = 0.0 ; return R ;
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
   double r11,r12,r13,r21,r22,r23,r31,r32,r33 ;
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

mat33 mat33_mul( mat33 A , mat33 B )  /* multiply 2 3x3 matrices */
{
   mat33 C ; int i,j ;
   for( i=0 ; i < 3 ; i++ )
    for( j=0 ; j < 3 ; j++ )
      C.m[i][j] =  A.m[i][0] * B.m[0][j]
                 + A.m[i][1] * B.m[1][j]
                 + A.m[i][2] * B.m[2][j] ;
   return C ;
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
/* Input:  4x4 matrix that transforms (i,j,k) indexes to (x,y,z) coordinates,
           where +x=Right, +y=Anterior, +z=Superior.
           (Only the upper-left 3x3 corner of R is used herein.)
   Output: 3 orientation codes that correspond to the closest "standard"
           anatomical orientation of the (i,j,k) axes.
   Method: Find which permutation of (x,y,z) has the smallest angle to the
           (i,j,k) axes directions, which are the columns of the R matrix.
   Errors: The codes returned will be zero.

   For example, an axial volume might get return values of
     *icod = NIFTI_R2L   (i axis is mostly Right to Left)
     *jcod = NIFTI_P2A   (j axis is mostly Posterior to Anterior)
     *kcod = NIFTI_I2S   (k axis is mostly Inferior to Superior)
-----------------------------------------------------------------------------*/

void mat44_to_orientation( mat44 R , int *icod, int *jcod, int *kcod )
{
   float xi,xj,xk , yi,yj,yk , zi,zj,zk , val,detQ,detP ;
   mat33 P , Q , M ;
   int i,j,k=0,p,q,r , ibest,jbest,kbest,pbest,qbest,rbest ;
   float vbest ;

   if( icod == NULL || jcod == NULL || kcod == NULL ) return ; /* bad */

   *icod = *jcod = *kcod = 0 ; /* error returns, if sh*t happens */

   /* load column vectors for each (i,j,k) direction from matrix */

   /*-- i axis --*/ /*-- j axis --*/ /*-- k axis --*/

   xi = R.m[0][0] ; xj = R.m[0][1] ; xk = R.m[0][2] ;
   yi = R.m[1][0] ; yj = R.m[1][1] ; yk = R.m[1][2] ;
   zi = R.m[2][0] ; zj = R.m[2][1] ; zk = R.m[2][2] ;

   /* normalize column vectors to get unit vectors along each ijk-axis */

   /* normalize i axis */

   val = sqrt( xi*xi + yi*yi + zi*zi ) ;
   if( val == 0.0 ) return ;                 /* stupid input */
   xi /= val ; yi /= val ; zi /= val ;

   /* normalize j axis */

   val = sqrt( xj*xj + yj*yj + zj*zj ) ;
   if( val == 0.0 ) return ;                 /* stupid input */
   xj /= val ; yj /= val ; zj /= val ;

   /* orthogonalize j axis to i axis, if needed */

   val = xi*xj + yi*yj + zi*zj ;    /* dot product between i and j */
   if( fabs(val) > 1.e-4 ){
     xj -= val*xi ; yj -= val*yi ; zj -= val*zi ;
     val = sqrt( xj*xj + yj*yj + zj*zj ) ;  /* must renormalize */
     if( val == 0.0 ) return ;              /* j was parallel to i? */
     xj /= val ; yj /= val ; zj /= val ;
   }

   /* normalize k axis; if it is zero, make it the cross product i x j */

   val = sqrt( xk*xk + yk*yk + zk*zk ) ;
   if( val == 0.0 ){ xk = yi*zj-zi*yj; yk = zi*xj-zj*xi ; zk=xi*yj-yi*xj ; }
   else            { xk /= val ; yk /= val ; zk /= val ; }

   /* orthogonalize k to i */

   val = xi*xk + yi*yk + zi*zk ;    /* dot product between i and k */
   if( fabs(val) > 1.e-4 ){
     xk -= val*xi ; yk -= val*yi ; zk -= val*zi ;
     val = sqrt( xk*xk + yk*yk + zk*zk ) ;
     if( val == 0.0 ) return ;      /* bad */
     xk /= val ; yk /= val ; zk /= val ;
   }

   /* orthogonalize k to j */

   val = xj*xk + yj*yk + zj*zk ;    /* dot product between j and k */
   if( fabs(val) > 1.e-4 ){
     xk -= val*xj ; yk -= val*yj ; zk -= val*zj ;
     val = sqrt( xk*xk + yk*yk + zk*zk ) ;
     if( val == 0.0 ) return ;      /* bad */
     xk /= val ; yk /= val ; zk /= val ;
   }

   Q.m[0][0] = xi ; Q.m[0][1] = xj ; Q.m[0][2] = xk ;
   Q.m[1][0] = yi ; Q.m[1][1] = yj ; Q.m[1][2] = yk ;
   Q.m[2][0] = zi ; Q.m[2][1] = zj ; Q.m[2][2] = zk ;

   /* at this point, Q is the rotation matrix from the (i,j,k) to (x,y,z) axes */

   detQ = mat33_determ( Q ) ;
   if( detQ == 0.0 ) return ; /* shouldn't happen unless user is a DUFIS */

   /* Build and test all possible +1/-1 coordinate permutation matrices P;
      then find the P such that the rotation matrix M=PQ is closest to the
      identity, in the sense of M having the smallest total rotation angle. */

   /* Despite the formidable looking 6 nested loops, there are
      only 3*3*3*2*2*2 = 216 passes, which will run very quickly. */

   vbest = -666.0 ; ibest=pbest=qbest=rbest=1 ; jbest=2 ; kbest=3 ;
   for( i=1 ; i <= 3 ; i++ ){     /* i = column number to use for row #1 */
    for( j=1 ; j <= 3 ; j++ ){    /* j = column number to use for row #2 */
     if( i == j ) continue ;
      for( k=1 ; k <= 3 ; k++ ){  /* k = column number to use for row #3 */
       if( i == k || j == k ) continue ;
       P.m[0][0] = P.m[0][1] = P.m[0][2] =
        P.m[1][0] = P.m[1][1] = P.m[1][2] =
         P.m[2][0] = P.m[2][1] = P.m[2][2] = 0.0 ;
       for( p=-1 ; p <= 1 ; p+=2 ){    /* p,q,r are -1 or +1      */
        for( q=-1 ; q <= 1 ; q+=2 ){   /* and go into rows #1,2,3 */
         for( r=-1 ; r <= 1 ; r+=2 ){
           P.m[0][i-1] = p ; P.m[1][j-1] = q ; P.m[2][k-1] = r ;
           detP = mat33_determ(P) ;             /* sign of permutation */
           if( detP * detQ <= 0.0 ) continue ;  /* doesn't match sign of Q */
           M = mat33_mul(P,Q) ;

           /* angle of M rotation = 2.0*acos(0.5*sqrt(1.0+trace(M)))       */
           /* we want largest trace(M) == smallest angle == M nearest to I */

           val = M.m[0][0] + M.m[1][1] + M.m[2][2] ; /* trace */
           if( val > vbest ){
             vbest = val ;
             ibest = i ; jbest = j ; kbest = k ;
             pbest = p ; qbest = q ; rbest = r ;
           }
   }}}}}}

   /* At this point ibest is 1 or 2 or 3; pbest is -1 or +1; etc.

      The matrix P that corresponds is the best permutation approximation
      to Q-inverse; that is, P (approximately) takes (x,y,z) coordinates
      to the (i,j,k) axes.

      For example, the first row of P (which contains pbest in column ibest)
      determines the way the i axis points relative to the anatomical
      (x,y,z) axes.  If ibest is 2, then the i axis is along the y axis,
      which is direction P2A (if pbest > 0) or A2P (if pbest < 0).

      So, using ibest and pbest, we can assign the output code for
      the i axis.  Mutatis mutandis for the j and k axes, of course. */

   switch( ibest*pbest ){
     case  1: i = NIFTI_L2R ; break ;
     case -1: i = NIFTI_R2L ; break ;
     case  2: i = NIFTI_P2A ; break ;
     case -2: i = NIFTI_A2P ; break ;
     case  3: i = NIFTI_I2S ; break ;
     case -3: i = NIFTI_S2I ; break ;
   }

   switch( jbest*qbest ){
     case  1: j = NIFTI_L2R ; break ;
     case -1: j = NIFTI_R2L ; break ;
     case  2: j = NIFTI_P2A ; break ;
     case -2: j = NIFTI_A2P ; break ;
     case  3: j = NIFTI_I2S ; break ;
     case -3: j = NIFTI_S2I ; break ;
   }

   switch( kbest*rbest ){
     case  1: k = NIFTI_L2R ; break ;
     case -1: k = NIFTI_R2L ; break ;
     case  2: k = NIFTI_P2A ; break ;
     case -2: k = NIFTI_A2P ; break ;
     case  3: k = NIFTI_I2S ; break ;
     case -3: k = NIFTI_S2I ; break ;
   }

   *icod = i ; *jcod = j ; *kcod = k ; return ;
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
     swap_2(h->slice_start); swap_2(h->slice_end);
     swap_4(h->slice_duration);
   }
   return ;
}

/* rcr - start I/O routines here */

#define USE_STAT
#ifdef  USE_STAT
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
   znzFile fp ; unsigned int len ;

   if( pathname == NULL || *pathname == '\0' ) return 0 ;
   fp = znzopen(pathname,"rb",0); if( znz_isnull(fp) ) return 0 ;
   znzseek(fp,0L,SEEK_END) ; len = (unsigned int)znztell(fp) ;
   znzclose(fp) ; return len ;
}

#endif /* USE_STAT */


size_t nifti_get_volsize(nifti_image *nim)
{
   return (size_t)(nim->nbyper) * (size_t)(nim->nvox) ; /* total bytes */
}


/*--------------------------------------------------------------------------*/
/* Support functions for filenames in read and write - allows for gzipped
   files
*/


int nifti_fileexists(const char* fname)
{
   znzFile fp;
   fp = znzopen( fname , "rb" , 1 ) ;
   if( !znz_isnull(fp) )  { znzclose(fp);  return 1; }
   return 0;
}

int nifti_validfilename(const char* fname)
{
   char * ext;
   int                         len;

   /** check input file(s) for sanity **/
   if( fname == NULL || *fname == '\0' ) return 0;

   len = strlen(fname);

   if( len < 1 ) {
      if ( gni_debug > 0 )
         fprintf(stderr,"-- empty filename in nifti_validfilename()\n");
      return 0;
   }

   ext = nifti_find_file_extension((char *)fname);

   if ( ext && ext == fname ) {   /* then no filename prefix */
      if ( gni_debug > 0 )
         fprintf(stderr,"-- no prefix for filename '%s'\n", fname);
      return 0;
   }

   return 1;
}

char * nifti_find_file_extension( char * name )
{
   char * ext;
   int    len;

   if ( ! name ) return NULL;

   len = strlen(name);
   if ( len < 4 ) return NULL;

   ext = name + len - 4;

   if ( (strcmp(ext, ".hdr") == 0) || (strcmp(ext, ".img") == 0)
                                   || (strcmp(ext, ".nii") == 0) )
      return ext;

#ifdef HAVE_ZLIB
   if ( len < 7 ) return NULL;

   ext = name - 7;
   if ( (strcmp(ext, ".hdr.gz") == 0) || (strcmp(ext, ".img.gz") == 0)
                                      || (strcmp(ext, ".nii.gz") == 0) )
      return ext;
#endif

   return NULL;
}

int nifti_is_gzfile(const char* fname)
{
  /* return true if the filename ends with .gz */
  if (fname == NULL) { return 0; }
#ifdef HAVE_ZLIB
  if (strcmp(fname + strlen(fname) - 3,".gz")==0) { return 1; }
#endif
  return 0;
}


char * nifti_makebasename(const char* fname)
{
   /*
      NB: it allocates memory for basename which should be freed
	when no longer required
   */
   char *basename, *ext;

   basename=nifti_strdup(fname);

   ext = nifti_find_file_extension(basename);
   if ( ext ) *ext = '\0';  /* clear out extension */
   
   return basename;  /* in either case */
}

void nifti_set_debug_level( int level )
{
    gni_debug = level;
}


char * nifti_findhdrname(const char* fname)
{
   /* returns filename of header on success and NULL if no appropriate file
	could be found 
      NB: it allocates memory for hdrname which should be freed
	when no longer required
   */
   char *basename, *hdrname;

   /** check input file(s) for sanity **/
   if( !nifti_validfilename(fname) ) return NULL;

   basename =  nifti_makebasename(fname);

   /** check if verbatim name exists (except if it contains .img or is a basename) **/
   if ( (strcmp(basename,fname)!=0) && (strstr(fname,".img")==NULL) && 
	nifti_fileexists(fname) ) { 
     /* if so, then just return this name */
     hdrname = nifti_strdup(fname); 
     return hdrname; 
   }

   /** only proceed if the requested name is a basename or contains .img **/
   if ( (strcmp(basename,fname)==0) || (strstr(fname,".img")!=NULL) ) {
     /** look for .nii.gz, .nii, .hdr.gz , .hdr  in that order **/
     hdrname = (char *)calloc(sizeof(char),strlen(basename)+8);
     if (strstr(fname,".img")==NULL) {
       /* only look for .nii / .nii.gz if original name didn't include .img */
#ifdef HAVE_ZLIB
       strcpy(hdrname,basename);
       strcat(hdrname,".nii.gz"); 
       if (nifti_fileexists(hdrname)) { free(basename); return hdrname; }
#endif
       strcpy(hdrname,basename);
       strcat(hdrname,".nii"); 
       if (nifti_fileexists(hdrname)) { free(basename); return hdrname; }
     }
#ifdef HAVE_ZLIB
     strcpy(hdrname,basename);
     strcat(hdrname,".hdr.gz"); 
     if (nifti_fileexists(hdrname)) { free(basename); return hdrname; }
#endif
     strcpy(hdrname,basename);
     strcat(hdrname,".hdr"); 
     if (nifti_fileexists(hdrname)) { free(basename); return hdrname; }
   }

   /** if it gets to here then nothing has been found -> failure! **/
   free(basename); 
   return NULL;
}


char * nifti_findimgname(const char* fname , const int nifti_type)
{
   /* returns filename of data/img file on success and NULL if no appropriate file
	could be found
      NB: it allocates memory for imgname which should be freed
	when no longer required
   */
   char *basename, *imgname;

   /** check input file(s) for sanity **/

   if( !nifti_validfilename(fname) ) return NULL;

   /** use nifti_type to decide whether to look for .img or .nii **/
   /** if using compression libraries, .gz files are looked for first **/
   basename =  nifti_makebasename(fname);
   imgname = (char *)calloc(sizeof(char),strlen(basename)+8);
   if (nifti_type == 1) {
#ifdef HAVE_ZLIB
     strcpy(imgname,basename);
     strcat(imgname,".nii.gz"); 
     if (nifti_fileexists(imgname)) { free(basename); return imgname; }
#endif
     strcpy(imgname,basename);
     strcat(imgname,".nii"); 
     if (nifti_fileexists(imgname)) { free(basename); return imgname; }
   }
   if ( (nifti_type == 0) || (nifti_type == 2) ) {
#ifdef HAVE_ZLIB
     strcpy(imgname,basename);
     strcat(imgname,".img.gz"); 
     if (nifti_fileexists(imgname)) { free(basename); return imgname; }
#endif
     strcpy(imgname,basename);
     strcat(imgname,".img"); 
     if (nifti_fileexists(imgname)) { free(basename); return imgname; }
   }
   /* otherwise - failure */
   free(basename); 
   return NULL;
   }
   
/*--------------------------------------------------------------------------*/
/* Determine if this is a NIFTI-formatted file.
   - returns 0 if file looks like ANALYZE 7.5 [checks sizeof_hdr field == 348]
   - returns 1 if file marked as NIFTI (header+data in 1 file)
   - returns 2 if file marked as NIFTI (header+data in 2 files)
   - returns -1 if it can't tell, file doesn't exist, etc.
----------------------------------------------------------------------------*/

int is_nifti_file( char *hname )
{
   struct nifti_1_header nhdr ;
   znzFile fp ;
   int ii ;
   char *tmpname;

   /* bad input name? */

   if( !nifti_validfilename(hname) ) return -1 ;

   /* open file */

   tmpname = nifti_findhdrname(hname);
   fp = znzopen( tmpname , "rb" , 1 ) ;
   free(tmpname);
   if (znz_isnull(fp))                      return -1 ;  /* bad open? */

   /* read header, close file */

   ii = znzread( &nhdr , 1 , sizeof(nhdr) , fp ) ;
   znzclose( fp ) ;
   if( ii < sizeof(nhdr) )               return -1 ;  /* bad read? */

   /* check for NIFTI-ness */

   if( NIFTI_VERSION(nhdr) != 0 ){
     return ( NIFTI_ONEFILE(nhdr) ) ? 1 : 2 ;
   }

   /* check for ANALYZE-ness (sizeof_hdr field == 348) */

   ii = nhdr.sizeof_hdr ;
   if( ii == sizeof(nhdr) ) return 0 ;  /* matches */

   /* try byte-swapping header */

   swap_4(ii) ;
   if( ii == sizeof(nhdr) ) return 0 ;  /* matches */

   return -1 ;                          /* not good */
}

int print_hex_vals( char * data, int nbytes, FILE * fp )
{
   int c;

   if ( !data || nbytes < 1 || !fp ) return -1;

   fputs("0x", fp);
   for ( c = 0; c < nbytes; c++ )
      fprintf(fp, " %x", data[c]);

   return 0;
}

int disp_nifti_1_header( char * info, nifti_1_header * hp )
{
   int c;

   fputs( "-------------------------------------------------------\n", stderr );
   if ( info )  fputs( info, stderr );
   if ( !hp  ){ fputs(" ** no nifti_1_header to display!\n",stderr); return 1; }

   fprintf(stderr," nifti_1_header :\n"
           "    sizeof_hdr     = %d\n"
           "    data_type[10]  = ", hp->sizeof_hdr);
   print_hex_vals(hp->data_type, 10, stderr);
   fprintf(stderr, "\n"
           "    db_name[18]    = ");
   print_hex_vals(hp->db_name, 18, stderr);
   fprintf(stderr, "\n"
           "    extents        = %d\n"
           "    session_error  = %d\n"
           "    regular        = 0x%x\n"
           "    dim_info       = %d\n",
      hp->extents, hp->session_error, hp->regular, hp->dim_info );
   fprintf(stderr, "    dim[8]         =");
   for ( c = 0; c < 8; c++ ) fprintf(stderr," %d", hp->dim[c]);
   fprintf(stderr, "\n"
           "    intent_p1      = %f\n"
           "    intent_p2      = %f\n"
           "    intent_p3      = %f\n"
           "    intent_code    = %d\n"
           "    datatype       = %d\n"
           "    bitpix         = %d\n"
           "    slice_start    = %d\n"
           "    pixdim[8]      =",
           hp->intent_p1, hp->intent_p2, hp->intent_p3, hp->intent_code,
           hp->datatype, hp->bitpix, hp->slice_start);
   /* break pixdim over 2 lines */
   for ( c = 0; c < 4; c++ ) fprintf(stderr," %f", hp->pixdim[c]);
   fprintf(stderr, "\n                    ");
   for ( c = 4; c < 8; c++ ) fprintf(stderr," %f", hp->pixdim[c]);
   fprintf(stderr, "\n"
           "    vox_offset     = %f\n"
           "    scl_slope      = %f\n"
           "    scl_inter      = %f\n"
           "    slice_end      = %d\n"
           "    slice_code     = %d\n"
           "    xyzt_units     = 0x%x\n"
           "    cal_max        = %f\n"
           "    cal_min        = %f\n"
           "    slice_duration = %f\n"
           "    toffset        = %f\n"
           "    glmax          = %d\n"
           "    glmin          = %d\n",
           hp->vox_offset, hp->scl_slope, hp->scl_inter, hp->slice_end,
           hp->slice_code, hp->xyzt_units, hp->cal_max, hp->cal_min,
           hp->slice_duration, hp->toffset, hp->glmax, hp->glmin);
   fprintf(stderr,
           "    descrip        = '%s'\n"
           "    aux_file       = '%s'\n"
           "    qform_code     = %d\n"
           "    sform_code     = %d\n"
           "    quatern_b      = %f\n"
           "    quatern_c      = %f\n"
           "    quatern_d      = %f\n"
           "    qoffset_x      = %f\n"
           "    qoffset_y      = %f\n"
           "    qoffset_z      = %f\n"
           "    srow_x[4]      = %f, %f, %f, %f\n"
           "    srow_y[4]      = %f, %f, %f, %f\n"
           "    srow_z[4]      = %f, %f, %f, %f\n"
           "    intent_name    = %-16s\n"
           "    magic          = %-4s\n",
           hp->descrip, hp->aux_file, hp->qform_code, hp->sform_code,
           hp->quatern_b, hp->quatern_c, hp->quatern_d,
           hp->qoffset_x, hp->qoffset_y, hp->qoffset_z,
           hp->srow_x[0], hp->srow_x[1], hp->srow_x[2], hp->srow_x[3],
           hp->srow_y[0], hp->srow_y[1], hp->srow_y[2], hp->srow_y[3],
           hp->srow_z[0], hp->srow_z[1], hp->srow_z[2], hp->srow_z[3],
           hp->intent_name, hp->magic);
   fputs( "-------------------------------------------------------\n", stderr );

   return 0;
}


#undef  ERREX
#define ERREX(msg)                                           \
 do{ fprintf(stderr,"** ERROR: nifti_convert_nhdr_to_nim: %s\n", (msg) ) ;  \
     return NULL ; } while(0)



nifti_image* nifti_convert_nhdr2nim(struct nifti_1_header nhdr, const char* fname)
{
  int   ii , doswap , ioff, ndim, nvox ;
  int   is_nifti , is_onefile ;
  short ss ;
  char *iname=NULL;
  nifti_image *nim;

   nim = (nifti_image *) calloc( 1 , sizeof(nifti_image) ) ;
   nim->fname=NULL;
   nim->iname=NULL;

  /** check if have to swap bytes **/
  
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
   if( doswap ) {
      if ( gni_debug > 2 ) disp_nifti_1_header("-d ni1 pre-swap: ", &nhdr);
      swap_nifti_header( &nhdr , is_nifti ) ;
   }

   if ( gni_debug > 1 ) disp_nifti_1_header("-d nhdr2nim : ", &nhdr);

   if( nhdr.datatype == DT_BINARY ||
       nhdr.datatype == DT_UNKNOWN  )    ERREX("bad datatype") ;

   if( nhdr.dim[1] <= 0 )                ERREX("bad dim[1]") ;

   for( ii=2 ; ii <= 7 ; ii++ )
     if( nhdr.dim[ii] <= 0 ) nhdr.dim[ii] = 1 ;  /* fix bad dim[] values */

   /** get number of dimensions (ignoring dim[0] now) **/

   for( ii=7 ; ii >= 2 ; ii-- )            /* loop backwards until we  */
     if( nhdr.dim[ii] > 1 ) break ;        /* find a dim bigger than 1 */
   ndim = ii ;

   /* set bad grid spacings to 1.0 */

   for( ii=1 ; ii <= 7 ; ii++ ){
     if( nhdr.pixdim[ii] == 0.0         ||
         !IS_GOOD_FLOAT(nhdr.pixdim[ii])  ) nhdr.pixdim[ii] = 1.0 ;
   }

  is_onefile = is_nifti && NIFTI_ONEFILE(nhdr) ;

  if( is_nifti ) nim->nifti_type = (is_onefile) ? 1 : 2 ;
  else           nim->nifti_type = 0 ;
  
  ii = short_order() ;
  if( doswap )   nim->byteorder = REVERSE_ORDER(ii) ;
  else           nim->byteorder = ii ;
  

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
  
  nifti_datatype_sizes( nim->datatype , &(nim->nbyper) , &(nim->swapsize) ) ;
  if( nim->nbyper == 0 ){ free(nim); ERREX("bad datatype"); }
  
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
                      nim->quatern_b, nim->quatern_c, nim->quatern_d,
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
    
    nim->xyz_units  = XYZT_TO_SPACE(nhdr.xyzt_units) ;
    nim->time_units = XYZT_TO_TIME (nhdr.xyzt_units) ;
    
    nim->freq_dim  = DIM_INFO_TO_FREQ_DIM ( nhdr.dim_info ) ;
    nim->phase_dim = DIM_INFO_TO_PHASE_DIM( nhdr.dim_info ) ;
    nim->slice_dim = DIM_INFO_TO_SLICE_DIM( nhdr.dim_info ) ;
    
    nim->slice_code     = nhdr.slice_code  ;
    nim->slice_start    = nhdr.slice_start ;
    nim->slice_end      = nhdr.slice_end   ;
    nim->slice_duration = FIXED_FLOAT(nhdr.slice_duration) ;
  }
  
  /* Miscellaneous ANALYZE stuff */
  
  nim->cal_min = FIXED_FLOAT(nhdr.cal_min) ;
  nim->cal_max = FIXED_FLOAT(nhdr.cal_max) ;
  
  memcpy(nim->descrip ,nhdr.descrip ,79) ; nim->descrip [79] = '\0' ;
  memcpy(nim->aux_file,nhdr.aux_file,23) ; nim->aux_file[23] = '\0' ;
  
   /** will read image data from file 'iname' starting at offset 'ioff' **/

   is_onefile = is_nifti && NIFTI_ONEFILE(nhdr) ;

   if( is_onefile ){
     ioff = (int)nhdr.vox_offset ;
     if( ioff < sizeof(nhdr) ) ioff = sizeof(nhdr) ;
   } else {
     ioff = (int)nhdr.vox_offset ;
   }
   nim->iname_offset = ioff ;


   /* deal with file names if set */
   if (fname!=NULL) {
     nim->fname = nifti_strdup(fname);
     /* determine name of image, if not already set */
     if (nim->iname==NULL) {
       if (is_onefile) {
	 iname = nifti_strdup(nim->fname);
       } else {
	 iname = nifti_findimgname(nim->fname,nim->nifti_type);
	 if (iname==NULL)  ERREX("bad filename");
       }
       /* don't free iname, as now nim->iname is using this storage */
       nim->iname        = iname ;          /* save image filename */
     }
   } else { 
     nim->fname = NULL;  
     nim->iname = NULL; 
   }

   return nim;
}


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
    - nifti_image_free() can be used to delete the returned struct,
      when you are done with it.
----------------------------------------------------------------------------*/

#undef  ERREX
#define ERREX(msg)                                           \
 do{ fprintf(stderr,"** ERROR: nifti_image_open(%s): %s\n",  \
             (hname != NULL) ? hname : "(null)" , (msg) ) ;  \
     return fptr ; } while(0)


znzFile nifti_image_open( const char *hname , const char *opts , nifti_image **nim)
{
  znzFile fptr=NULL;
  znz_setnull(fptr);
  /* open the hdr and reading it in, but do not load the data  */
  *nim = nifti_image_read(hname,0);
  /* open the image file, ready for reading - NB: compressed works for all reads */
  if( ((*nim) == NULL)      || ((*nim)->iname == NULL) ||
      ((*nim)->nbyper <= 0) || ((*nim)->nvox <= 0)       ) ERREX("bad header info") ;

  /** open image data file **/  
  fptr = znzopen((*nim)->iname ,opts,1);
  if( znz_isnull(fptr) ) ERREX("Can't open data file") ;
  
  return fptr;
}


#undef  ERREX
#define ERREX(msg)                                           \
 do{ fprintf(stderr,"** ERROR: nifti_image_read(%s): %s\n",  \
             (hname != NULL) ? hname : "(null)" , (msg) ) ;  \
     return NULL ; } while(0)


nifti_image *nifti_image_read( const char *hname , int read_data )
{
   struct nifti_1_header nhdr ;
   nifti_image *nim ;
   znzFile fp ;
   int   ii , ilen;
   char  buf[16] , *workingname=NULL;
   
   /* determine file name to use for header */
   workingname = nifti_findhdrname(hname);
   fp = znzopen(workingname,"rb",1);
   if( znz_isnull(fp) )                      ERREX("can't open header file") ;


   /** test if header file starts with ASCII string "<nifti_image";
       if so, read the dataset that special way and return now
       (this is NOT part of the NIFTI-1 standard!)                **/
   ii = znzread( buf , 1 , 12 , fp ) ;
   if( ii < 12 ){ znzclose( fp ) ;          ERREX("bad header read") ; }
   znzrewind(fp) ;
   buf[12] = '\0' ;
   if( strcmp(buf,"<nifti_image") == 0 ){  /* have an ASCII header! */
     int slen;
     char  *sbuf ;
     if (nifti_is_gzfile(workingname)) {
       ERREX("cannot determine uncompressed file size needed for negative offset");
     }
     slen = get_filesize( workingname ) ;
     if( slen > 65530 ) slen = 65530 ;
     sbuf = (char *)calloc(sizeof(char),slen+1) ;
     znzread( sbuf , 1 , slen , fp ) ; znzclose( fp ) ;
     nim = nifti_image_from_ascii( sbuf ) ; free( sbuf ) ;
     if( nim == NULL )                   ERREX("bad ASCII header read") ;
     nim->nifti_type = 3 ;
     nim->iname_offset = -1 ;
/* rcr - check for extensions */

     if( read_data ) nifti_image_load( nim ) ;
     else            nim->data = NULL ;
     free(workingname);
     return nim ;
   }

   /**================ Normal case:  read binary header ===================*/

   ii = znzread( &nhdr , 1 , sizeof(nhdr) , fp ) ;       /* read the thing */
/* rcr - check for extensions */

   znzclose( fp ) ;                                      /* close the file */
   if( ii < sizeof(nhdr) )               ERREX("bad binary header read") ;

   /*=== create output image struct and set it up ===*/

   /** convert all nhdr fields to nifti_image fields **/
   nim = nifti_convert_nhdr2nim(nhdr,workingname);
   free(workingname);
/* rcr - check for nim == NULL */

   /* check the data file is OK */
   if (!nifti_is_gzfile(nim->iname)) { /* can't use get_filesize on a compressed file   :( */
     ilen = get_filesize(nim->iname) ;      /* find size of image data file */
     if( ilen <= nim->iname_offset ){ ERREX("bad data file") ; }
   }

   /** read the data if desired, then bug out **/
   if( read_data ) nifti_image_load( nim ) ;
   else            nim->data = NULL ;

   return nim ;
}

/*--------------------------------------------------------------------------*/
/* Load the image data from disk into an already-prepared image struct.
----------------------------------------------------------------------------*/

/* rcr - change all of these ERREX definitions */

#undef  ERREX
#define ERREX(msg)                                               \
 do{ fprintf(stderr,"** ERROR: nifti_image_load: %s\n",(msg)) ;  \
     return ; } while(0)

void nifti_image_load( nifti_image *nim )
{
  /** sets up data space, open data file and seek, then call nifti_read_buffer **/
   size_t ntot , ii , ioff ;
   znzFile fp ;

   if( nim == NULL      || nim->iname == NULL ||
       nim->nbyper <= 0 || nim->nvox <= 0       ) ERREX("bad input struct") ;

   ntot = nifti_get_volsize(nim) ; /* total bytes to read */

   /** open image data file **/

   fp = znzopen(nim->iname , "rb",1);
   if( znz_isnull(fp) ) ERREX("Can't open data file") ;

   if( nim->iname_offset < 0 ){          /* negative offset means   */
     if (nifti_is_gzfile(nim->iname)) {
       ERREX("cannot determine uncompressed file size needed for negative offset");
     }
     ii = get_filesize( nim->iname ) ;   /* figure from end of file */
     ioff = (ii > ntot) ? ii-ntot : 0 ;
   } else {                              /* non-negative offset   */
     ioff = nim->iname_offset ;          /* means use it directly */
   }
   znzseek( fp , ioff , SEEK_SET ) ;

   /** if the data pointer is already set then use this space - the onus is 
        on the caller to make sure that enough space has been allocated! **/

   if( nim->data == NULL ) {
     nim->data = (void *)calloc(1,ntot) ;
     if( nim->data == NULL ) ERREX("can't malloc array space") ;
   }

   /* now that everything is set up - do the reading */
   nifti_read_buffer(fp,nim->data,ntot,nim);

   znzclose( fp ) ;

   return ;
}


#undef  ERREX
#define ERREX(msg)                                               \
 do{ fprintf(stderr,"** ERROR: nifti_read_buffer: %s\n",(msg)) ;  \
     return 0; } while(0)


size_t nifti_read_buffer(znzFile fp, void* dataptr, size_t ntot, 
			 const nifti_image *nim)
{
  /** reads ntot bytes of data from an open file and byte swaps if necessary **/
  /** note that nifti_image is required for information on datatype, etc **/
  size_t ii;
  if( dataptr == NULL ) { ERREX("attempted to read into NULL buffer"); }
  ii = znzread( dataptr , 1 , ntot , fp ) ;             /*** data input! ***/
  /** if read was short, fill rest of array with 0 bytes **/
  
  if( ii < ntot ){ 
    fprintf(stderr,"++ WARNING: nifti_read_buffer(%s):\n"
	    "   data bytes needed = %u\n"
	    "   data bytes input  = %u\n"
	    "   number missing    = %u (set to 0)\n",
	    nim->iname , (unsigned int)ntot ,
	    (unsigned int)ii , (unsigned int)(ntot-ii) ) ;
    memset( (char *)(dataptr)+ii , 0 , ntot-ii ) ;
  }
  
  /** byte swap array if needed **/
  
  if( nim->swapsize > 1 && nim->byteorder != short_order() )
    swap_Nbytes( ntot / nim->swapsize , nim->swapsize , dataptr ) ;
  
#ifdef isfinite
  /** check input float arrays for goodness, and fix bad numbers **/
  
  switch( nim->datatype ){
    
  case NIFTI_TYPE_FLOAT32:
  case NIFTI_TYPE_COMPLEX64:{
    register float *far = (float *)dataptr ; register int jj,nj ;
    nj = ntot / sizeof(float) ;
    for( jj=0 ; jj < nj ; jj++ ) far[jj] = FIXED_FLOAT(far[jj]);
  }
    break ;
    
  case NIFTI_TYPE_FLOAT64:
  case NIFTI_TYPE_COMPLEX128:{
    register double *far = (double *)dataptr ; register int jj,nj ;
    nj = ntot / sizeof(double) ;
    for( jj=0 ; jj < nj ; jj++ ) far[jj] = FIXED_FLOAT(far[jj]);
  }
    break ;
    
  }
#endif
  
  return ii;
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
   char *str = nifti_image_to_ascii( nim ) ;
   if( str != NULL ){ fputs(str,stdout) ; free(str) ; }
   return ;
}

/*--------------------------------------------------------------------------*/
/* Write a nifti_image to disk.  The following fields of nim affect how
   the output appears:
    - nifti_type = 0 ==> ANALYZE-7.5 format file pair will be written
    - nifti_type = 1 ==> NIFTI-1 format single file will be written
                         (data offset will be 348)
    - nifti_type = 2 ==> NIFTI_1 format file pair will be written
    - nifti_type = 3 ==> NIFTI_1 ASCII single file will be written
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
 do{ fprintf(stderr,"** ERROR: nifti_write_buffer: %s\n",(msg)) ;  \
     return 0; } while(0)


size_t nifti_write_buffer(znzFile fp, const void *buffer, size_t numbytes)
{
   /** Write all the image data at once (no swapping here) **/
   size_t ss;
   if (znz_isnull(fp)) { ERREX("null file pointer"); }
   ss = znzwrite( (void *) buffer , 1 , numbytes , fp ) ;
   return ss;
}


#undef  ERREX
#define ERREX(msg)                                                \
 do{ fprintf(stderr,"** ERROR: nifti_image_write: %s\n",(msg)) ;  \
     return ; } while(0)


void nifti_write_all_data(znzFile fp, nifti_image *nim)
{
   /** Write all the image data at once (no swapping here) **/
   size_t ss;
   ss = nifti_write_buffer(fp,nim->data,nim->nbyper * nim->nvox);
   if (ss < (nim->nbyper * nim->nvox)) ERREX("incomplete write to image file");

   nim->byteorder = short_order() ;  /* mark as being in this CPU byte order */
   return ;
}



nifti_image* nifti_simple_init_nim()
{
  nifti_image *nim;
  struct nifti_1_header nhdr;
  int nbyper, swapsize;
  
  memset(&nhdr,0,sizeof(nhdr)) ;  /* zero out header, to be safe */
  
   /** load the ANALYZE-7.5 generic parts of the header struct **/

   nhdr.sizeof_hdr = sizeof(nhdr) ;
   nhdr.regular    = 'r' ;             /* for some stupid reason */

   nhdr.dim[0] = 3 ;
   nhdr.dim[1] = 1 ; nhdr.dim[2] = 1 ; nhdr.dim[3] = 1 ;
   nhdr.dim[4] = 0 ;

   nhdr.pixdim[0] = 0.0 ;
   nhdr.pixdim[1] = 1.0 ; nhdr.pixdim[2] = 1.0 ;
   nhdr.pixdim[3] = 1.0 ;

   nhdr.datatype = NIFTI_TYPE_FLOAT32 ;
   nifti_datatype_sizes( nhdr.datatype , &nbyper, &swapsize );
   nhdr.bitpix   = 8 * nbyper ;

   nim = nifti_convert_nhdr2nim(nhdr,NULL);
   nim->fname = NULL;
   nim->iname = NULL;
   return nim;
}




struct nifti_1_header nifti_convert_nim2nhdr(const nifti_image* nim)
{
  struct nifti_1_header nhdr;

   memset(&nhdr,0,sizeof(nhdr)) ;  /* zero out header, to be safe */


   /** load the ANALYZE-7.5 generic parts of the header struct **/

   nhdr.sizeof_hdr = sizeof(nhdr) ;
   nhdr.regular    = 'r' ;             /* for some stupid reason */

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

     /* for nifti files, enforce positive pixdims! */
     nhdr.pixdim[1] = fabs(nhdr.pixdim[1]) ; nhdr.pixdim[2] = fabs(nhdr.pixdim[2]) ;
     nhdr.pixdim[3] = fabs(nhdr.pixdim[3]) ; nhdr.pixdim[4] = fabs(nhdr.pixdim[4]) ;
     nhdr.pixdim[5] = fabs(nhdr.pixdim[5]) ; nhdr.pixdim[6] = fabs(nhdr.pixdim[6]) ;
     nhdr.pixdim[7] = fabs(nhdr.pixdim[7]) ;

     nhdr.intent_code = nim->intent_code ;
     nhdr.intent_p1   = nim->intent_p1 ;
     nhdr.intent_p2   = nim->intent_p2 ;
     nhdr.intent_p3   = nim->intent_p3 ;
     if( nim->intent_name[0] != '\0' ){
       memcpy(nhdr.intent_name,nim->intent_name,15) ;
       nhdr.intent_name[15] = '\0' ;
     }

     nhdr.vox_offset  = (float) nim->iname_offset ;
     nhdr.xyzt_units  = SPACE_TIME_TO_XYZT( nim->xyz_units, nim->time_units ) ;
     nhdr.toffset     = nim->toffset ;

     if( nim->qform_code > 0 ){
       nhdr.qform_code = nim->qform_code ;
       nhdr.quatern_b  = nim->quatern_b ;
       nhdr.quatern_c  = nim->quatern_c ;
       nhdr.quatern_d  = nim->quatern_d ;
       nhdr.qoffset_x  = nim->qoffset_x ;
       nhdr.qoffset_y  = nim->qoffset_y ;
       nhdr.qoffset_z  = nim->qoffset_z ;
       nhdr.pixdim[0]  = (nim->qfac >= 0.0) ? 1.0 : -1.0 ;
     }

     if( nim->sform_code > 0 ){
       nhdr.sform_code = nim->sform_code ;
       nhdr.srow_x[0]  = nim->sto_xyz.m[0][0] ;
       nhdr.srow_x[1]  = nim->sto_xyz.m[0][1] ;
       nhdr.srow_x[2]  = nim->sto_xyz.m[0][2] ;
       nhdr.srow_x[3]  = nim->sto_xyz.m[0][3] ;
       nhdr.srow_y[0]  = nim->sto_xyz.m[1][0] ;
       nhdr.srow_y[1]  = nim->sto_xyz.m[1][1] ;
       nhdr.srow_y[2]  = nim->sto_xyz.m[1][2] ;
       nhdr.srow_y[3]  = nim->sto_xyz.m[1][3] ;
       nhdr.srow_z[0]  = nim->sto_xyz.m[2][0] ;
       nhdr.srow_z[1]  = nim->sto_xyz.m[2][1] ;
       nhdr.srow_z[2]  = nim->sto_xyz.m[2][2] ;
       nhdr.srow_z[3]  = nim->sto_xyz.m[2][3] ;
     }

     nhdr.dim_info = FPS_INTO_DIM_INFO( nim->freq_dim ,
                                        nim->phase_dim , nim->slice_dim ) ;
     nhdr.slice_code     = nim->slice_code ;
     nhdr.slice_start    = nim->slice_start ;
     nhdr.slice_end      = nim->slice_end ;
     nhdr.slice_duration = nim->slice_duration ;
   }

   return nhdr;
}



void nifti_set_iname_offset(nifti_image *nim)
{
  int offset;
   switch( nim->nifti_type ){

     default:  /* writing into 2 files */
       nim->iname_offset = 0 ;    /* we only write files with 0 offset in the 2 file format */
     break ;

     case 1:   /* NIFTI-1 single binary file */
       if (nim->iname_offset < sizeof(struct nifti_1_header))  
	 {
	   offset = sizeof(struct nifti_1_header) ;
	   /* see if this is aligned with a 16 byte word boundary - if not, make it so */
	   if ( ( offset % 16 ) != 0 )  offset = ((offset/16) + 1)*16;
	   nim->iname_offset = offset;
	 }
     break ;

               /* non-standard case: */
     case 3:{  /* NIFTI-1 ASCII header + binary data (single file) */
       nim->iname_offset = -1 ;              /* compute offset from filesize */
     }
   }
}



#undef  ERREX
#define ERREX(msg)                                                \
 do{ fprintf(stderr,"** ERROR: nifti_image_write_hdr_img: %s\n",(msg)) ;  \
     return fp ; } while(0)


znzFile nifti_image_write_hdr_img2( nifti_image *nim , int write_data , 
				    char* opts, znzFile *imgfile )
{
  /** This writes the header (and optionally the image data) to file **/
  /** If the image data file is left open it returns a valid znzFile handle **/
  /** It also uses imgfile as the open image file is not null, and modifies it inside **/

  /** Values for write_data mode are based on two binary flags
      ( 0/1 for no-write/write data, and 0/2 for close/leave-open files ) :
      0 = do not write data and close (do not open data file)
      1 = write data        and close
      2 = do not write data and leave data file open 
      3 = write data        and leave data file open 
  **/
   struct nifti_1_header nhdr ;
   znzFile fp=NULL, hdrfile=NULL;
   size_t ss ;

   if( nim        == NULL                         ) ERREX("NULL input") ;
   if( !nifti_validfilename(nim->fname)           ) ERREX("bad fname input") ;
   if( (nim->data  == NULL)  && ((write_data & 1)!=0) ) ERREX("no image data") ;



   nifti_set_iname_offset(nim);

   /* make iname from fname, if needed */

   switch( nim->nifti_type ){

     default:  /* writing into 2 files */
       if( nim->iname != NULL && strcmp(nim->iname,nim->fname) == 0 ){
         free(nim->iname) ; nim->iname = NULL ;
       }
       if( nim->iname == NULL ){ 
	 /* choose a new name because either no name set, or it wasn't different from hdr */
	 nim->iname = nifti_findimgname(nim->fname,nim->nifti_type);
       }
     break ;

     case 1:   /* NIFTI-1 single binary file */
     break ;

               /* non-standard case: */
     case 3:{  /* NIFTI-1 ASCII header + binary data (single file) */
       char *hstr ;
       nim->byteorder = short_order() ;      /* am writing in current order */
       hstr = nifti_image_to_ascii( nim ) ;  /* get header in ASCII form */
       if( hstr == NULL ) ERREX("bad ASCII header creation?") ;
       fp = znzopen( nim->fname , opts , nifti_is_gzfile(nim->fname) ) ;
       if( znz_isnull(fp) ){ free(hstr); ERREX("can't open output file"); }
       znzputs(hstr,fp) ;
     }

       /* writes the binary data to fp */
       if ( (write_data & 1)!=0) { nifti_write_all_data(fp,nim); }
       if ( (write_data & 2)==0) { znzclose(fp); }
       return fp;  /* returned but may be closed */
   }
     

   /***** Here -- write a binary header *****/
   
   nhdr = nifti_convert_nim2nhdr(nim);



   if (imgfile==NULL) {
     
     /** Open file, write header **/
     
     fp = znzopen( nim->fname , opts , nifti_is_gzfile(nim->fname) ) ;
     if( znz_isnull(fp) ) ERREX("can't open output file") ;
     
     ss = znzwrite( &nhdr , 1 , sizeof(nhdr) , fp ) ;
     if( ss < sizeof(nhdr) ){
       znzclose(fp) ; ERREX("bad write to output file") ;
     }
     
     
     /** if writing data or keeping data file open **/
     if ((write_data & 3)!=0) {
       /** If writing dual files, close header and open image file **/
       if( nim->nifti_type != 1 ){
	 znzclose(fp) ;
	 fp = znzopen( nim->iname , opts , nifti_is_gzfile(nim->iname) ) ;
	 if( znz_isnull(fp) ) ERREX("can't open image file") ;
       }
       /* skip to vox_offset, ready for writing */
       znzseek(fp,nim->iname_offset,SEEK_SET);
       if ( (write_data & 1)!=0)  nifti_write_all_data(fp,nim);
     }
     
     /** close the file if requested **/
     if ( (write_data & 2)==0) { znzclose(fp); }
     return fp;  /* returned but may be closed */
     
   }

   /** ----- otherwise use imgfile for the image -----**/

   /** write header **/

   if (nim->nifti_type==1) {
     if( znz_isnull(*imgfile) ) ERREX("bad image data file passed in") ;
     /* if header and data file are the same then seek to beginning and write */
     znzseek(*imgfile,0,SEEK_SET);
     ss = znzwrite( &nhdr , 1 , sizeof(nhdr) , *imgfile ) ;
     if( ss < sizeof(nhdr) ){ ERREX("bad write to output file") ; }
   } else {
     /* if a dual file type or no file opened, then start a new file */
     hdrfile = znzopen( nim->fname , opts , nifti_is_gzfile(nim->fname) ) ;
     if( znz_isnull(hdrfile) ) ERREX("can't open output file") ;
     /* if the header file was separate then write and close it now */
     ss = znzwrite( &nhdr , 1 , sizeof(nhdr) , hdrfile ) ;
     if( ss < sizeof(nhdr) ){ znzclose(fp) ; ERREX("bad write to output file") ; }
     znzclose(hdrfile);
   }


   /** if writing data or keeping data file open **/
   if ((write_data & 3)!=0) {  
     /* skip to vox_offset, ready for writing in image file */
     znzseek(*imgfile,nim->iname_offset,SEEK_SET);
     if ( (write_data & 1)!=0) nifti_write_all_data(*imgfile,nim);
   }

   /** close the file if requested **/
   if ( (write_data & 2)==0) { znzclose(*imgfile); }
   return *imgfile;  /* returned but may be closed */
}


znzFile nifti_image_write_hdr_img( nifti_image *nim , int write_data , 
				  char* opts)
{
  return nifti_image_write_hdr_img2(nim,write_data,opts,NULL);
}


void nifti_image_write( nifti_image *nim )
{
  nifti_image_write_hdr_img(nim,1,"wb");
}



/* copy for nifti_image is via the nifti_1_header structure */
/* it does not copy data */

nifti_image * nifti_copy_nim_info(const nifti_image* src)
{
  struct nifti_1_header nhdr;
  nifti_image *dest;
  dest = (nifti_image *)calloc(1,sizeof(nifti_image));
  nhdr = nifti_convert_nim2nhdr(src);
  dest = nifti_convert_nhdr2nim(nhdr,src->fname);
  return dest;
}


/*------------------------------------------------------------------------*/
/* Un-escape a C string in place -- that is, convert XML escape sequences
   back into their characters.  (This can be done in place since the
   replacement is always smaller than the input.)  Escapes recognized are:
     -  &lt;   ->  <
     -  &gt;   ->  >
     -  &quot; ->  "
     -  &apos; ->  '
     -  &amp;  ->  &
   Also replace CR LF pair (Microsoft), or CR alone (Macintosh) with
   LF (Unix), per the XML standard.
   Return value is number of replacements made (if you care).
--------------------------------------------------------------------------*/

#undef  CR
#undef  LF
#define CR 0x0D
#define LF 0x0A

int unescape_string( char *str )
{
   int ii,jj , nn,ll ;

   if( str == NULL ) return 0 ;                /* no string? */
   ll = strlen(str) ; if( ll == 0 ) return 0 ;

   /* scan for escapes: &something; */

   for( ii=jj=nn=0 ; ii<ll ; ii++,jj++ ){ /* scan at ii; results go in at jj */

     if( str[ii] == '&' ){  /* start of escape? */

             if( ii+3 < ll        &&   /* &lt; */
                 str[ii+1] == 'l' &&
                 str[ii+2] == 't' &&
                 str[ii+3] == ';'   ){ str[jj] = '<' ; ii += 3 ; nn++ ; }

        else if( ii+3 < ll        &&   /* &gt; */
                 str[ii+1] == 'g' &&
                 str[ii+2] == 't' &&
                 str[ii+3] == ';'   ){ str[jj] = '>' ; ii += 3 ; nn++ ; }

        else if( ii+5 < ll        &&   /* &quot; */
                 str[ii+1] == 'q' &&
                 str[ii+2] == 'u' &&
                 str[ii+3] == 'o' &&
                 str[ii+4] == 't' &&
                 str[ii+5] == ';'   ){ str[jj] = '"' ; ii += 5 ; nn++ ; }

        else if( ii+5 < ll        &&   /* &apos; */
                 str[ii+1] == 'a' &&
                 str[ii+2] == 'p' &&
                 str[ii+3] == 'o' &&
                 str[ii+4] == 's' &&
                 str[ii+5] == ';'   ){ str[jj] = '\'' ; ii += 5 ; nn++ ; }

        else if( ii+4 < ll        &&  /* &amp; */
                 str[ii+1] == 'a' &&
                 str[ii+2] == 'm' &&
                 str[ii+3] == 'p' &&
                 str[ii+4] == ';'   ){ str[jj] = '&' ; ii += 4 ; nn++ ; }

        /* although the comments above don't mention it,
           we also look for XML style numeric escapes
           of the forms &#32; (decimal) and &#xfd; (hex) */

        else if( ii+3 < ll        &&
                 str[ii+1] == '#' &&
                 isdigit(str[ii+2]) ){   /* &#dec; */

           unsigned int val='?' ; int kk=ii+3 ;
           while( kk < ll && kk != ';' ) kk++ ;
           sscanf( str+ii+2 , "%u" , &val ) ;
           str[jj] = (char) val ; ii = kk ; nn++ ;
        }

        else if( ii+4 < ll        &&
                 str[ii+1] == '#' &&
                 str[ii+2] == 'x' &&
                 isxdigit(str[ii+3]) ){   /* &#hex; */

           unsigned int val='?' ; int kk=ii+4 ;
           while( kk < ll && kk != ';' ) kk++ ;
           sscanf( str+ii+3 , "%x" , &val ) ;
           str[jj] = (char) val ; ii = kk ; nn++ ;
        }

        /* didn't start a recognized escape, so just copy as normal */

        else if( jj < ii ){ str[jj] = str[ii] ; }

     } else if( str[ii] == CR ) {  /* is a carriage return */

        if( str[ii+1] == LF ){ str[jj] = LF ; ii++ ; nn++ ; }  /* CR LF */
        else                 { str[jj] = LF ;      ; nn++ ; }  /* CR only */

     } else { /* is a normal character, just copy to output */

             if( jj < ii ){ str[jj] = str[ii] ; }
     }

     /* at this point, ii=index of last character used up in scan
                       jj=index of last character written to (jj <= ii) */
   }

   if( jj < ll ) str[jj] = '\0' ; /* end string properly */

   return nn ;
}

/*------------------------------------------------------------------------*/
/* Quotize (and escapize) one string, returning a new string.
   Approximately speaking, this is the inverse of unescape_string().
   The result should be free()-ed when you are done with it.
--------------------------------------------------------------------------*/

char *escapize_string( char *str )
{
   int ii,jj , lstr,lout ;
   char *out ;

   if( str == NULL || (lstr=strlen(str)) == 0 ){      /* 0 length */
     out = nifti_strdup("''") ; return out ;                /* string?? */
   }

   lout = 4 ;                      /* initialize length of output */
   for( ii=0 ; ii < lstr ; ii++ ){ /* count characters for output */
     switch( str[ii] ){
       case '&':  lout += 5 ; break ;  /* replace '&' with "&amp;" */

       case '<':
       case '>':  lout += 4 ; break ;  /* replace '<' with "&lt;" */

       case '"' :
       case '\'': lout += 6 ; break ;  /* replace '"' with "&quot;" */

       case CR:
       case LF:   lout += 6 ; break ;  /* replace CR with "&#x0d;"
                                                  LF with "&#x0a;" */

       default: lout++ ; break ;      /* copy all other chars */
     }
   }
   out = (char *)calloc(1,lout) ;     /* allocate output string */
   out[0] = '\'' ;                    /* opening quote mark */
   for( ii=0,jj=1 ; ii < lstr ; ii++ ){
      switch( str[ii] ){
         default: out[jj++] = str[ii] ; break ;  /* normal characters */

         case '&':  memcpy(out+jj,"&amp;",5)  ; jj+=5 ; break ;

         case '<':  memcpy(out+jj,"&lt;",4)   ; jj+=4 ; break ;
         case '>':  memcpy(out+jj,"&gt;",4)   ; jj+=4 ; break ;

         case '"' : memcpy(out+jj,"&quot;",6) ; jj+=6 ; break ;

         case '\'': memcpy(out+jj,"&apos;",6) ; jj+=6 ; break ;

         case CR:   memcpy(out+jj,"&#x0d;",6) ; jj+=6 ; break ;
         case LF:   memcpy(out+jj,"&#x0a;",6) ; jj+=6 ; break ;
      }
   }
   out[jj++] = '\''  ;  /* closing quote mark */
   out[jj]   = '\0' ;  /* terminate the string */
   return out ;
}

/*---------------------------------------------------------------------------*/
/* Dump the information in a NIFTI image header to an XML-ish ASCII string
   that can later be converted back into a NIFTI header in
   nifti_image_from_ascii().  The resulting string can be free()-ed when
   you are done with it.
-----------------------------------------------------------------------------*/

char *nifti_image_to_ascii( nifti_image *nim )
{
   char *buf , *ebuf ; int nbuf ;

   if( nim == NULL ) return NULL ;   /* stupid caller */

   buf = (char *)calloc(1,65534); nbuf = 0;  /* longer than needed, to be safe */

   sprintf( buf , "<nifti_image\n" ) ;   /* XML-ish opener */

   sprintf( buf+strlen(buf) , "  nifti_type = '%s'\n" ,
              (nim->nifti_type == 1) ? "NIFTI-1+"
             :(nim->nifti_type == 2) ? "NIFTI-1"
             :(nim->nifti_type == 3) ? "NIFTI-1A"
             :                         "ANALYZE-7.5" ) ;

   /** Strings that we don't control (filenames, etc.) that might
       contain "weird" characters (like quotes) are "escaped":
       - A few special characters are replaced by XML-style escapes, using
         the function escapize_string().
       - On input, function unescape_string() reverses this process.
       - The result is that the NIFTI ASCII-format header is XML-compliant. **/

   ebuf = escapize_string(nim->fname) ;
   sprintf( buf+strlen(buf) , "  header_filename = %s\n",ebuf); free(ebuf);

   ebuf = escapize_string(nim->iname) ;
   sprintf( buf+strlen(buf) , "  image_filename = %s\n", ebuf); free(ebuf);

   sprintf( buf+strlen(buf) , "  image_offset = '%d'\n" , nim->iname_offset );

                       sprintf( buf+strlen(buf), "  ndim = '%d'\n", nim->ndim);
                       sprintf( buf+strlen(buf), "  nx = '%d'\n",   nim->nx  );
   if( nim->ndim > 1 ) sprintf( buf+strlen(buf), "  ny = '%d'\n",   nim->ny  );
   if( nim->ndim > 2 ) sprintf( buf+strlen(buf), "  nz = '%d'\n",   nim->nz  );
   if( nim->ndim > 3 ) sprintf( buf+strlen(buf), "  nt = '%d'\n",   nim->nt  );
   if( nim->ndim > 4 ) sprintf( buf+strlen(buf), "  nu = '%d'\n",   nim->nu  );
   if( nim->ndim > 5 ) sprintf( buf+strlen(buf), "  nv = '%d'\n",   nim->nv  );
   if( nim->ndim > 6 ) sprintf( buf+strlen(buf), "  nw = '%d'\n",   nim->nw  );
                       sprintf( buf+strlen(buf), "  dx = '%g'\n",   nim->dx  );
   if( nim->ndim > 1 ) sprintf( buf+strlen(buf), "  dy = '%g'\n",   nim->dy  );
   if( nim->ndim > 2 ) sprintf( buf+strlen(buf), "  dz = '%g'\n",   nim->dz  );
   if( nim->ndim > 3 ) sprintf( buf+strlen(buf), "  dt = '%g'\n",   nim->dt  );
   if( nim->ndim > 4 ) sprintf( buf+strlen(buf), "  du = '%g'\n",   nim->du  );
   if( nim->ndim > 5 ) sprintf( buf+strlen(buf), "  dv = '%g'\n",   nim->dv  );
   if( nim->ndim > 6 ) sprintf( buf+strlen(buf), "  dw = '%g'\n",   nim->dw  );

   sprintf( buf+strlen(buf) , "  datatype = '%d'\n" , nim->datatype ) ;
   sprintf( buf+strlen(buf) , "  datatype_name = '%s'\n" ,
                              nifti_datatype_string(nim->datatype) ) ;

   sprintf( buf+strlen(buf) , "  nvox = '%d'\n" , nim->nvox ) ;
   sprintf( buf+strlen(buf) , "  nbyper = '%d'\n" , nim->nbyper ) ;

   sprintf( buf+strlen(buf) , "  byteorder = '%s'\n" ,
            (nim->byteorder==MSB_FIRST) ? "MSB_FIRST" : "LSB_FIRST" ) ;

   if( nim->cal_min < nim->cal_max ){
     sprintf( buf+strlen(buf) , "  cal_min = '%g'\n", nim->cal_min ) ;
     sprintf( buf+strlen(buf) , "  cal_max = '%g'\n", nim->cal_max ) ;
   }

   if( nim->scl_slope != 0.0 ){
     sprintf( buf+strlen(buf) , "  scl_slope = '%g'\n" , nim->scl_slope ) ;
     sprintf( buf+strlen(buf) , "  scl_inter = '%g'\n" , nim->scl_inter ) ;
   }

   if( nim->intent_code > 0 ){
     sprintf( buf+strlen(buf) , "  intent_code = '%d'\n", nim->intent_code ) ;
     sprintf( buf+strlen(buf) , "  intent_code_name = '%s'\n" ,
                                nifti_intent_string(nim->intent_code) ) ;
     sprintf( buf+strlen(buf) , "  intent_p1 = '%g'\n" , nim->intent_p1 ) ;
     sprintf( buf+strlen(buf) , "  intent_p2 = '%g'\n" , nim->intent_p2 ) ;
     sprintf( buf+strlen(buf) , "  intent_p3 = '%g'\n" , nim->intent_p3 ) ;

     if( nim->intent_name[0] != '\0' ){
       ebuf = escapize_string(nim->intent_name) ;
       sprintf( buf+strlen(buf) , "  intent_name = %s\n",ebuf) ;
       free(ebuf) ;
     }
   }

   if( nim->toffset != 0.0 )
     sprintf( buf+strlen(buf) , "  toffset = '%g'\n",nim->toffset ) ;

   if( nim->xyz_units > 0 )
     sprintf( buf+strlen(buf) ,
              "  xyz_units = '%d'\n"
              "  xyz_units_name = '%s'\n" ,
              nim->xyz_units , nifti_units_string(nim->xyz_units) ) ;

   if( nim->time_units > 0 )
     sprintf( buf+strlen(buf) ,
              "  time_units = '%d'\n"
              "  time_units_name = '%s'\n" ,
              nim->time_units , nifti_units_string(nim->time_units) ) ;

   if( nim->freq_dim > 0 )
     sprintf( buf+strlen(buf) , "  freq_dim = '%d'\n",nim->freq_dim ) ;
   if( nim->phase_dim > 0 )
     sprintf( buf+strlen(buf) , "  phase_dim = '%d'\n",nim->phase_dim ) ;
   if( nim->slice_dim > 0 )
     sprintf( buf+strlen(buf) , "  slice_dim = '%d'\n",nim->slice_dim ) ;
   if( nim->slice_code > 0 )
     sprintf( buf+strlen(buf) ,
              "  slice_code = '%d'\n"
              "  slice_code_name = '%s'\n" ,
              nim->slice_code , nifti_slice_string(nim->slice_code) ) ;
   if( nim->slice_start >= 0 && nim->slice_end > nim->slice_start )
     sprintf( buf+strlen(buf) ,
              "  slice_start = '%d'\n"
              "  slice_end = '%d'\n"  , nim->slice_start , nim->slice_end ) ;
   if( nim->slice_duration != 0.0 )
     sprintf( buf+strlen(buf) , "  slice_duration = '%g'\n",
              nim->slice_duration ) ;

   if( nim->descrip[0] != '\0' ){
     ebuf = escapize_string(nim->descrip) ;
     sprintf( buf+strlen(buf) , "  descrip = %s\n",ebuf) ;
     free(ebuf) ;
   }

   if( nim->aux_file[0] != '\0' ){
     ebuf = escapize_string(nim->aux_file) ;
     sprintf( buf+strlen(buf) , "  aux_file = %s\n",ebuf) ;
     free(ebuf) ;
   }

   if( nim->qform_code > 0 ){
     int i,j,k ;

     sprintf( buf+strlen(buf) ,
              "  qform_code = '%d'\n"
              "  qform_code_name = '%s'\n"
     "  qto_xyz_matrix = '%g %g %g %g %g %g %g %g %g %g %g %g %g %g %g %g'\n" ,
         nim->qform_code      , nifti_xform_string(nim->qform_code) ,
         nim->qto_xyz.m[0][0] , nim->qto_xyz.m[0][1] ,
         nim->qto_xyz.m[0][2] , nim->qto_xyz.m[0][3] ,
         nim->qto_xyz.m[1][0] , nim->qto_xyz.m[1][1] ,
         nim->qto_xyz.m[1][2] , nim->qto_xyz.m[1][3] ,
         nim->qto_xyz.m[2][0] , nim->qto_xyz.m[2][1] ,
         nim->qto_xyz.m[2][2] , nim->qto_xyz.m[2][3] ,
         nim->qto_xyz.m[3][0] , nim->qto_xyz.m[3][1] ,
         nim->qto_xyz.m[3][2] , nim->qto_xyz.m[3][3]  ) ;

     sprintf( buf+strlen(buf) ,
     "  qto_ijk_matrix = '%g %g %g %g %g %g %g %g %g %g %g %g %g %g %g %g'\n" ,
         nim->qto_ijk.m[0][0] , nim->qto_ijk.m[0][1] ,
         nim->qto_ijk.m[0][2] , nim->qto_ijk.m[0][3] ,
         nim->qto_ijk.m[1][0] , nim->qto_ijk.m[1][1] ,
         nim->qto_ijk.m[1][2] , nim->qto_ijk.m[1][3] ,
         nim->qto_ijk.m[2][0] , nim->qto_ijk.m[2][1] ,
         nim->qto_ijk.m[2][2] , nim->qto_ijk.m[2][3] ,
         nim->qto_ijk.m[3][0] , nim->qto_ijk.m[3][1] ,
         nim->qto_ijk.m[3][2] , nim->qto_ijk.m[3][3]  ) ;

     sprintf( buf+strlen(buf) ,
              "  quatern_b = '%g'\n"
              "  quatern_c = '%g'\n"
              "  quatern_d = '%g'\n"
              "  qoffset_x = '%g'\n"
              "  qoffset_y = '%g'\n"
              "  qoffset_z = '%g'\n"
              "  qfac = '%g'\n" ,
         nim->quatern_b , nim->quatern_c , nim->quatern_d ,
         nim->qoffset_x , nim->qoffset_y , nim->qoffset_z , nim->qfac ) ;

     mat44_to_orientation( nim->qto_xyz , &i,&j,&k ) ;
     if( i > 0 && j > 0 && k > 0 )
       sprintf( buf+strlen(buf) ,
                "  qform_i_orientation = '%s'\n"
                "  qform_j_orientation = '%s'\n"
                "  qform_k_orientation = '%s'\n" ,
                nifti_orientation_string(i) ,
                nifti_orientation_string(j) ,
                nifti_orientation_string(k)  ) ;
   }

   if( nim->sform_code > 0 ){
     int i,j,k ;

     sprintf( buf+strlen(buf) ,
              "  sform_code = '%d'\n"
              "  sform_code_name = '%s'\n"
     "  sto_xyz_matrix = '%g %g %g %g %g %g %g %g %g %g %g %g %g %g %g %g'\n" ,
         nim->sform_code      , nifti_xform_string(nim->sform_code) ,
         nim->sto_xyz.m[0][0] , nim->sto_xyz.m[0][1] ,
         nim->sto_xyz.m[0][2] , nim->sto_xyz.m[0][3] ,
         nim->sto_xyz.m[1][0] , nim->sto_xyz.m[1][1] ,
         nim->sto_xyz.m[1][2] , nim->sto_xyz.m[1][3] ,
         nim->sto_xyz.m[2][0] , nim->sto_xyz.m[2][1] ,
         nim->sto_xyz.m[2][2] , nim->sto_xyz.m[2][3] ,
         nim->sto_xyz.m[3][0] , nim->sto_xyz.m[3][1] ,
         nim->sto_xyz.m[3][2] , nim->sto_xyz.m[3][3]  ) ;

     sprintf( buf+strlen(buf) ,
     "  sto_ijk matrix = '%g %g %g %g %g %g %g %g %g %g %g %g %g %g %g %g'\n" ,
         nim->sto_ijk.m[0][0] , nim->sto_ijk.m[0][1] ,
         nim->sto_ijk.m[0][2] , nim->sto_ijk.m[0][3] ,
         nim->sto_ijk.m[1][0] , nim->sto_ijk.m[1][1] ,
         nim->sto_ijk.m[1][2] , nim->sto_ijk.m[1][3] ,
         nim->sto_ijk.m[2][0] , nim->sto_ijk.m[2][1] ,
         nim->sto_ijk.m[2][2] , nim->sto_ijk.m[2][3] ,
         nim->sto_ijk.m[3][0] , nim->sto_ijk.m[3][1] ,
         nim->sto_ijk.m[3][2] , nim->sto_ijk.m[3][3]  ) ;

     mat44_to_orientation( nim->sto_xyz , &i,&j,&k ) ;
     if( i > 0 && j > 0 && k > 0 )
       sprintf( buf+strlen(buf) ,
                "  sform_i_orientation = '%s'\n"
                "  sform_j_orientation = '%s'\n"
                "  sform_k_orientation = '%s'\n" ,
                nifti_orientation_string(i) ,
                nifti_orientation_string(j) ,
                nifti_orientation_string(k)  ) ;
   }

   sprintf( buf+strlen(buf) , "/>\n" ) ;   /* XML-ish closer */

   nbuf = strlen(buf) ;
   buf  = (char *)realloc((void *)buf, nbuf+1); /* cut back to proper length */
   return buf ;
}

/*---------------------------------------------------------------------------*/

int short_order(void)   /* determine this CPU's byte order */
{
   union { unsigned char bb[2] ;
           short         ss    ; } fred ;

   fred.bb[0] = 1 ; fred.bb[1] = 0 ;

   return (fred.ss == 1) ? LSB_FIRST : MSB_FIRST ;
}

/*---------------------------------------------------------------------------*/

#undef  QQNUM
#undef  QNUM
#undef  QSTR

/* macro to check lhs string against "n1"; if it matches,
   interpret rhs string as a number, and put it into nim->"n2" */

#define QQNUM(n1,n2) if( strcmp(lhs,#n1)==0 ) nim->n2=strtod(rhs,NULL)

/* same, but where "n1" == "n2" */

#define QNUM(nam)    QQNUM(nam,nam)

/* macro to check lhs string against "nam"; if it matches,
   put rhs string into nim->"nam" string, with max length = "ml" */

#define QSTR(nam,ml) if( strcmp(lhs,#nam) == 0 )                           \
                       strncpy(nim->nam,rhs,ml), nim->intent_name[ml]='\0'

/*---------------------------------------------------------------------------*/
/* Take an XML-ish ASCII string and create a NIFTI image header to match.
   NULL is returned if enough information isn't present in the input string.
    - The image data can later be loaded with nifti_image_load().
    - The struct returned here can be liberated with nifti_image_free().
    - Not a lot of error checking is done here to make sure that the
      input values are reasonable!
-----------------------------------------------------------------------------*/

nifti_image *nifti_image_from_ascii( char *str )
{
   char lhs[1024] , rhs[1024] ;
   int ii , spos, nn , slen ;
   nifti_image *nim ;              /* will be output */

   if( str == NULL || *str == '\0' ) return NULL ;  /* bad input!? */

   /* scan for opening string */

   spos = 0 ; slen = strlen(str) ;
   ii = sscanf( str+spos , "%1023s%n" , lhs , &nn ) ; spos += nn ;
   if( ii == 0 || strcmp(lhs,"<nifti_image") != 0 ) return NULL ;

   /* create empty image struct */

   nim = (nifti_image *) calloc( 1 , sizeof(nifti_image) ) ;

   nim->nx = nim->ny = nim->nz = nim->nt
           = nim->nu = nim->nv = nim->nw = 1 ;
   nim->dx = nim->dy = nim->dz = nim->dt
           = nim->du = nim->dv = nim->dw = nim->qfac = 1.0 ;

   nim->byteorder = short_order() ;

   /* starting at str[spos], scan for "equations" of the form
         lhs = 'rhs'
      and assign rhs values into the struct component named by lhs */

   while(1){

     while( isspace(str[spos]) ) spos++ ;  /* skip whitespace */
     if( str[spos] == '\0' ) break ;       /* end of string? */

     /* get lhs string */

     ii = sscanf( str+spos , "%1023s%n" , lhs , &nn ) ; spos += nn ;
     if( ii == 0 || strcmp(lhs,"/>") == 0 ) break ;  /* end of input? */

     /* skip whitespace and the '=' marker */

     while( isspace(str[spos]) || str[spos] == '=' ) spos++ ;
     if( str[spos] == '\0' ) break ;       /* end of string? */

     /* if next character is a quote ', copy everything up to next '
        otherwise, copy everything up to next nonblank              */

     if( str[spos] == '\'' ){
        ii = spos+1 ;
        while( str[ii] != '\0' && str[ii] != '\'' ) ii++ ;
        nn = ii-spos-1 ; if( nn > 1023 ) nn = 1023 ;
        memcpy(rhs,str+spos+1,nn) ; rhs[nn] = '\0' ;
        spos = (str[ii] == '\'') ? ii+1 : ii ;
     } else {
        ii = sscanf( str+spos , "%1023s%n" , rhs , &nn ) ; spos += nn ;
        if( ii == 0 ) break ;  /* nothing found? */
     }
     unescape_string(rhs) ;  /* remove any XML escape sequences */

     /* Now can do the assignment, based on lhs string.
        Start with special cases that don't fit the QNUM/QSTR macros. */

     if( strcmp(lhs,"nifti_type") == 0 ){
            if( strcmp(rhs,"ANALYZE-7.5") == 0 ) nim->nifti_type = 0 ;
       else if( strcmp(rhs,"NIFTI-1+")    == 0 ) nim->nifti_type = 1 ;
       else if( strcmp(rhs,"NIFTI-1")     == 0 ) nim->nifti_type = 2 ;
       else if( strcmp(rhs,"NIFTI-1A")    == 0 ) nim->nifti_type = 3 ;
     }
     else if( strcmp(lhs,"header_filename") == 0 ){
       nim->fname = nifti_strdup(rhs) ;
     }
     else if( strcmp(lhs,"image_filename") == 0 ){
       nim->iname = nifti_strdup(rhs) ;
     }
     else if( strcmp(lhs,"sto_xyz_matrix") == 0 ){
       sscanf( rhs , "%f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f" ,
               &(nim->sto_xyz.m[0][0]) , &(nim->sto_xyz.m[0][1]) ,
               &(nim->sto_xyz.m[0][2]) , &(nim->sto_xyz.m[0][3]) ,
               &(nim->sto_xyz.m[1][0]) , &(nim->sto_xyz.m[1][1]) ,
               &(nim->sto_xyz.m[1][2]) , &(nim->sto_xyz.m[1][3]) ,
               &(nim->sto_xyz.m[2][0]) , &(nim->sto_xyz.m[2][1]) ,
               &(nim->sto_xyz.m[2][2]) , &(nim->sto_xyz.m[2][3]) ,
               &(nim->sto_xyz.m[3][0]) , &(nim->sto_xyz.m[3][1]) ,
               &(nim->sto_xyz.m[3][2]) , &(nim->sto_xyz.m[3][3])  ) ;
     }
     else if( strcmp(lhs,"byteorder") == 0 ){
       if( strcmp(rhs,"MSB_FIRST") == 0 ) nim->byteorder = MSB_FIRST ;
       if( strcmp(rhs,"LSB_FIRST") == 0 ) nim->byteorder = LSB_FIRST ;
     }
     else QQNUM(image_offset,iname_offset) ;
     else QNUM(datatype) ;
     else QNUM(ndim) ;
     else QNUM(nx) ;
     else QNUM(ny) ;
     else QNUM(nz) ;
     else QNUM(nt) ;
     else QNUM(nu) ;
     else QNUM(nv) ;
     else QNUM(nw) ;
     else QNUM(dx) ;
     else QNUM(dy) ;
     else QNUM(dz) ;
     else QNUM(dt) ;
     else QNUM(du) ;
     else QNUM(dv) ;
     else QNUM(dw) ;
     else QNUM(cal_min) ;
     else QNUM(cal_max) ;
     else QNUM(scl_slope) ;
     else QNUM(scl_inter) ;
     else QNUM(intent_code) ;
     else QNUM(intent_p1) ;
     else QNUM(intent_p2) ;
     else QNUM(intent_p3) ;
     else QSTR(intent_name,15) ;
     else QNUM(toffset) ;
     else QNUM(xyz_units) ;
     else QNUM(time_units) ;
     else QSTR(descrip,79) ;
     else QSTR(aux_file,23) ;
     else QNUM(qform_code) ;
     else QNUM(quatern_b) ;
     else QNUM(quatern_c) ;
     else QNUM(quatern_d) ;
     else QNUM(qoffset_x) ;
     else QNUM(qoffset_y) ;
     else QNUM(qoffset_z) ;
     else QNUM(qfac) ;
     else QNUM(sform_code) ;
     else QNUM(freq_dim) ;
     else QNUM(phase_dim) ;
     else QNUM(slice_dim) ;
     else QNUM(slice_code) ;
     else QNUM(slice_start) ;
     else QNUM(slice_end) ;
     else QNUM(slice_duration) ;

   } /* end of while loop */

   /** do miscellaneous checking and cleanup **/

   if( nim->ndim <= 0 ){ nifti_image_free(nim); return NULL; } /** bad! **/

   nifti_datatype_sizes( nim->datatype, &(nim->nbyper), &(nim->swapsize) );
   if( nim->nbyper == 0 ){ nifti_image_free(nim); return NULL; } /** bad! **/

   nim->dim[0] = nim->ndim ;
   nim->dim[1] = nim->nx ; nim->pixdim[1] = nim->dx ;
   nim->dim[2] = nim->ny ; nim->pixdim[2] = nim->dy ;
   nim->dim[3] = nim->nz ; nim->pixdim[3] = nim->dz ;
   nim->dim[4] = nim->nt ; nim->pixdim[4] = nim->dt ;
   nim->dim[5] = nim->nu ; nim->pixdim[5] = nim->du ;
   nim->dim[6] = nim->nv ; nim->pixdim[6] = nim->dv ;
   nim->dim[7] = nim->nw ; nim->pixdim[7] = nim->dw ;

   nim->nvox =  nim->nx * nim->ny * nim->nz
              * nim->nt * nim->nu * nim->nv * nim->nw ;

   if( nim->qform_code > 0 )
     nim->qto_xyz = quatern_to_mat44(
                      nim->quatern_b, nim->quatern_c, nim->quatern_d,
                      nim->qoffset_x, nim->qoffset_y, nim->qoffset_z,
                      nim->dx       , nim->dy       , nim->dz       ,
                      nim->qfac                                      ) ;
   else
     nim->qto_xyz = quatern_to_mat44(
                      0.0 , 0.0 , 0.0 , 0.0 , 0.0 , 0.0 ,
                      nim->dx , nim->dy , nim->dz , 0.0 ) ;


   nim->qto_ijk = mat44_inverse( nim->qto_xyz ) ;

   if( nim->sform_code > 0 )
     nim->sto_ijk = mat44_inverse( nim->sto_xyz ) ;

   return nim ;
}
