#include "mrilib.h"

/*---------------------------------------------------------------------------*/
/* Functions to deal with the mat44 elements in an AFNI dataset dataxes
   structure.  Note that the mat44 (4x4 matrix) type is defined in
   nifti1_io.h, and that the last row of the matrix is ALWAYS [0 0 0 1].
   It is to be applied to column vectors of the form [ x y z 1 ]', to
   define an affine transformation of (x,y,z).

   The library nifti1_io.c includes nifti1_mat44_inverse() and some
   other utility functions.

   -- RWCox - 07 Dec 2005
-----------------------------------------------------------------------------*/

/* load the top 3 rows of a mat44 matrix */

#undef  LOAD_MAT44
#define LOAD_MAT44(AA,a11,a12,a13,a14,a21,a22,a23,a24,a31,a32,a33,a34)   \
  ( AA.m[0][0]=a11 , AA.m[0][1]=a12 , AA.m[0][2]=a13 , AA.m[0][3]=a14 ,  \
    AA.m[1][0]=a21 , AA.m[1][1]=a22 , AA.m[1][2]=a23 , AA.m[1][3]=a24 ,  \
    AA.m[2][0]=a31 , AA.m[2][1]=a32 , AA.m[2][2]=a33 , AA.m[2][3]=a34 ,  \
    AA.m[3][0]=AA.m[3][1]=AA.m[3][2]=0.0f , AA.m[3][3]=1.0f            )

/* negate the top 2 rows of a mat44 matrix */

#undef  XYINVERT_MAT44
#define XYINVERT_MAT44(AA)                                               \
  ( AA.m[0][0] = -AA.m[0][0] , AA.m[0][1] = -AA.m[0][1] ,                \
    AA.m[0][2] = -AA.m[0][2] , AA.m[0][3] = -AA.m[0][3] ,                \
    AA.m[1][0] = -AA.m[1][0] , AA.m[1][1] = -AA.m[1][1] ,                \
    AA.m[1][2] = -AA.m[1][2] , AA.m[1][3] = -AA.m[1][3] ,                \
    AA.m[2][0] = -AA.m[2][0] , AA.m[2][1] = -AA.m[2][1] ,                \
    AA.m[2][2] = -AA.m[2][2] , AA.m[2][3] = -AA.m[2][3]  )

/*---------------------------------------------------------------------------*/
/*! Multiply 2 mat44 matrices (a utility missing from nifti1_io.c).
-----------------------------------------------------------------------------*/

mat44 THD_mat44_mul( mat44 A , mat44 B )
{
   mat44 C ; int i,j ;
   for( i=0 ; i < 3 ; i++ )
    for( j=0 ; j < 4 ; j++ )
     C.m[i][j] =  A.m[i][0] * B.m[0][j] + A.m[i][1] * B.m[1][j]
                + A.m[i][2] * B.m[2][j] + A.m[i][3] * B.m[3][j] ;

   C.m[3][0] = C.m[3][1] = C.m[3][2] = 0.0f ; C.m[3][3] = 1.0f ;
   return C ;
}

/*---------------------------------------------------------------------------*/
/*! Compute the mat44 elements of a dataset dataxes struct, given the
    other elements that define the coordinate system.  Also see
    function THD_daxes_from_mat44(), which does the reverse.
-----------------------------------------------------------------------------*/

void THD_daxes_to_mat44( THD_dataxes *dax )
{
   mat44 ijk_to_dxyz , dxyz_to_dicom ;

   if( dax == NULL ) return ;

   /* ijk_to_dxyz: transforms (i,j,k) to dataset (x,y,z) coords */

   LOAD_MAT44( ijk_to_dxyz ,
               dax->xxdel , 0.0f , 0.0f , dax->xxorg ,
               dax->yydel , 0.0f , 0.0f , dax->yyorg ,
               dax->zzdel , 0.0f , 0.0f , dax->zzorg  ) ;

   /* dxyz_to_dicom: transforms dataset (x,y,z) coords to DICOM coords */

   LOAD_MAT44( dxyz_to_dicom ,
               dax->to_dicomm.mat[0][0] , dax->to_dicomm.mat[0][1] ,
                                          dax->to_dicomm.mat[0][2] , 0.0f ,
               dax->to_dicomm.mat[1][0] , dax->to_dicomm.mat[1][1] ,
                                          dax->to_dicomm.mat[1][2] , 0.0f ,
               dax->to_dicomm.mat[2][0] , dax->to_dicomm.mat[2][1] ,
                                          dax->to_dicomm.mat[2][2] , 0.0f  ) ;

   /* dax->ijk_to_xyz: transforms (i,j,k) to DICOM (x,y,z) */

   dax->ijk_to_xyz = THD_mat44_mul( dxyz_to_dicom , ijk_to_dxyz ) ;

   /* and the inverse transformation: DICOM (x,y,z) to indexes (i,j,k) */

   dax->xyz_to_ijk = nifti_mat44_inverse( dax->ijk_to_xyz ) ;

   return ;
}

/*---------------------------------------------------------------------------*/
/*! Given the ijk_to_xyz index to DICOM transformation in the header, load
    the legacy dataxes information:
      - xxorient  = Orientation code
      - yyorient  = Orientation code
      - zzorient  = Orientation code
      - xxorg     = Center of (0,0,0) voxel
      - yyorg     = Center of (0,0,0) voxel
      - zzorg     = Center of (0,0,0) voxel
      - xxdel     = Spacings between voxel centers (mm) - may be negative
      - yydel     = Spacings between voxel centers (mm) - may be negative
      - zzdel     = Spacings between voxel centers (mm) - may be negative
      - to_dicomm = Orthogonal matrix transforming from dataset coordinates
                    to DICOM coordinates
-----------------------------------------------------------------------------*/

void THD_daxes_from_mat44( THD_dataxes *dax )
{
   int icod , jcod , kcod ;
   mat44 nmat ;

   /* table to convert NIfTI-1 orientation codes (1..6)
      into AFNI orientation codes (0..5, and in a different order) */

   static int orient_nifti2afni[7] =
               { -1 , ORI_L2R_TYPE, ORI_R2L_TYPE, ORI_P2A_TYPE,
                      ORI_A2P_TYPE, ORI_I2S_TYPE, ORI_S2I_TYPE } ;

   if( dax == NULL ) return ;

   /* use the NIfTI-1 library function to determine best orientation;
      but, must remember that NIfTI-1 x and y are reversed from AFNI's,
      so we must negate the x and y rows of the matrix before func call */

   nmat = dax->ijk_to_xyz ; XYINVERT_MAT44(nmat) ;

   nifti_mat44_to_orientation( nmat , &icod, &jcod, &kcod ) ;

   if( icod == 0 || jcod == 0 || kcod == 0 ) return ;

   dax->xxorient = orient_nifti2afni[icod] ;
   dax->yyorient = orient_nifti2afni[jcod] ;
   dax->zzorient = orient_nifti2afni[kcod] ;

   /* grid offsets */

   dax->xxorg = dax->ijk_to_xyz.m[0][3] ;
   dax->yyorg = dax->ijk_to_xyz.m[1][3] ;
   dax->zzorg = dax->ijk_to_xyz.m[2][3] ;

   /* grid spacing along i-direction is length of 1st column of matrix */

   dax->xxdel = sqrt( SQR(dax->ijk_to_xyz.m[0][0])
                     +SQR(dax->ijk_to_xyz.m[1][0])
                     +SQR(dax->ijk_to_xyz.m[2][0]) ) ;
   if( ORIENT_sign[dax->xxorient] == '-' ) dax->xxdel = -dax->xxdel ;

   /* mutatis mutandis for j- and k-directions */

   dax->yydel = sqrt( SQR(dax->ijk_to_xyz.m[0][1])
                     +SQR(dax->ijk_to_xyz.m[1][1])
                     +SQR(dax->ijk_to_xyz.m[2][1]) ) ;
   if( ORIENT_sign[dax->yyorient] == '-' ) dax->yydel = -dax->yydel ;

   dax->zzdel = sqrt( SQR(dax->ijk_to_xyz.m[0][2])
                     +SQR(dax->ijk_to_xyz.m[1][2])
                     +SQR(dax->ijk_to_xyz.m[2][2]) ) ;
   if( ORIENT_sign[dax->zzorient] == '-' ) dax->zzdel = -dax->yydel ;

   /* to_dicomm orthogonal matrix:
      we make an orthogonal matrix out of the columns of ijk_to_xyz */

   nmat = nifti_make_orthog_mat44(
    dax->ijk_to_xyz.m[0][0], dax->ijk_to_xyz.m[1][0], dax->ijk_to_xyz.m[2][0],
    dax->ijk_to_xyz.m[0][1], dax->ijk_to_xyz.m[1][1], dax->ijk_to_xyz.m[2][1],
    dax->ijk_to_xyz.m[0][2], dax->ijk_to_xyz.m[1][2], dax->ijk_to_xyz.m[2][2] );

   dax->to_dicomm.mat[0][0] = nmat.m[0][0] ;
   dax->to_dicomm.mat[0][1] = nmat.m[0][1] ;
   dax->to_dicomm.mat[0][2] = nmat.m[0][2] ;
   dax->to_dicomm.mat[1][0] = nmat.m[1][0] ;
   dax->to_dicomm.mat[1][1] = nmat.m[1][1] ;
   dax->to_dicomm.mat[1][2] = nmat.m[1][2] ;
   dax->to_dicomm.mat[2][0] = nmat.m[2][0] ;
   dax->to_dicomm.mat[2][1] = nmat.m[2][1] ;
   dax->to_dicomm.mat[2][2] = nmat.m[2][2] ;

   return ;
}
