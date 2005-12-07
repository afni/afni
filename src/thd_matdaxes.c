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

#undef  LOAD_MAT44
#define LOAD_MAT44(AA,a11,a12,a13,a14,a21,a22,a23,a24,a31,a32,a33,a34)   \
  ( AA.m[0][0]=a11 , AA.m[0][1]=a12 , AA.m[0][2]=a13 , AA.m[0][3]=a14 ,  \
    AA.m[1][0]=a21 , AA.m[1][1]=a22 , AA.m[1][2]=a23 , AA.m[1][3]=a24 ,  \
    AA.m[2][0]=a31 , AA.m[2][1]=a32 , AA.m[2][2]=a33 , AA.m[2][3]=a34 ,  \
    AA.m[3][0]=AA.m[3][1]=AA.m[3][2]=0.0f , AA.m[3][3]=1.0f            )

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

   LOAD_MAT44( ijk_to_dxyz ,
               dax->xxdel , 0.0f , 0.0f , dax->xxorg ,
               dax->yydel , 0.0f , 0.0f , dax->yyorg ,
               dax->zzdel , 0.0f , 0.0f , dax->zzorg  ) ;

   LOAD_MAT44( dxyz_to_dicom ,
               dax->to_dicomm.mat[0][0] , dax->to_dicomm.mat[0][1] ,
                                          dax->to_dicomm.mat[0][2] , 0.0f ,
               dax->to_dicomm.mat[1][0] , dax->to_dicomm.mat[1][1] ,
                                          dax->to_dicomm.mat[1][2] , 0.0f ,
               dax->to_dicomm.mat[2][0] , dax->to_dicomm.mat[2][1] ,
                                          dax->to_dicomm.mat[2][2] , 0.0f  ) ;

   dax->ijk_to_xyz = THD_mat44_mul( dxyz_to_dicom , ijk_to_dxyz ) ;
   dax->xyz_to_ijk = nifti_mat44_inverse( dax->ijk_to_xyz ) ;

   return ;
}

/*---------------------------------------------------------------------------*/

void THD_daxes_from_mat44( THD_dataxes *dax )
{

   if( dax == NULL || dax->ijk_to_xyz.m[3][3] == 0.0f ) return ;
}
