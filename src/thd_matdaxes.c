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

    On input to this function, the XXdel, XXorg, nXX, and to_dicomm elements
    of dax must have been set already, for XX=xx,yy,zz.  Return value is
    -1 if an error occurs, 0 if all is cool.
-----------------------------------------------------------------------------*/

int THD_daxes_to_mat44( THD_dataxes *dax )
{
   mat44 ijk_to_dxyz , dxyz_to_dicom ;
   float aaa ;

   if( dax == NULL ) return -1 ;

   /* ijk_to_dxyz: transforms (i,j,k) to dataset (x,y,z) coords */

   LOAD_MAT44( ijk_to_dxyz ,
               dax->xxdel , 0.0f       , 0.0f       , dax->xxorg ,
               0.0f       , dax->yydel , 0.0f       , dax->yyorg ,
               0.0f       , 0.0f       , dax->zzdel , dax->zzorg  ) ;

   /* set to_dicomm if not valid */

   aaa = fabsf(dax->to_dicomm.mat[0][0])+fabsf(dax->to_dicomm.mat[0][1])
        +fabsf(dax->to_dicomm.mat[0][2])+fabsf(dax->to_dicomm.mat[1][0])
        +fabsf(dax->to_dicomm.mat[1][1])+fabsf(dax->to_dicomm.mat[1][2])
        +fabsf(dax->to_dicomm.mat[2][0])+fabsf(dax->to_dicomm.mat[2][1])
        +fabsf(dax->to_dicomm.mat[2][2]) ;
   if( aaa == 0.0f ) THD_set_daxes_to_dicomm(dax) ;

   /* dxyz_to_dicom: transforms dataset (x,y,z) coords to DICOM coords */

   LOAD_MAT44( dxyz_to_dicom ,
               dax->to_dicomm.mat[0][0] , dax->to_dicomm.mat[0][1] ,
                                          dax->to_dicomm.mat[0][2] , 0.0f ,
               dax->to_dicomm.mat[1][0] , dax->to_dicomm.mat[1][1] ,
                                          dax->to_dicomm.mat[1][2] , 0.0f ,
               dax->to_dicomm.mat[2][0] , dax->to_dicomm.mat[2][1] ,
                                          dax->to_dicomm.mat[2][2] , 0.0f  ) ;

   /* dax->ijk_to_dicom: transforms (i,j,k) to DICOM (x,y,z) */

   dax->ijk_to_dicom = THD_mat44_mul( dxyz_to_dicom , ijk_to_dxyz ) ;

   /* and the inverse transformation: DICOM (x,y,z) to indexes (i,j,k) */

   dax->dicom_to_ijk = nifti_mat44_inverse( dax->ijk_to_dicom ) ;

   THD_set_dicom_box( dax ) ;

#if 0
   { THD_dataxes new_daxes ;
     new_daxes.type = DATAXES_TYPE;
     THD_edit_dataxes(1.0, dax, &new_daxes);
     printf("edited dataxes:\n"
            " nxx=%d xxorg=%11.4g xxdel=%11.4g\n"
            " nyy=%d yyorg=%11.4g yydel=%11.4g\n"
            " nzz=%d zzorg=%11.4g zzdel=%11.4g\n" ,
      new_daxes.nxx , new_daxes.xxorg , new_daxes.xxdel ,
      new_daxes.nyy , new_daxes.yyorg , new_daxes.yydel ,
      new_daxes.nzz , new_daxes.zzorg , new_daxes.zzdel  ) ;
      DUMP_MAT44("ijk_to_dicom",new_daxes.ijk_to_dicom) ;
   }
#endif

   return 0 ;
}

/*------------------------------------------------------------------------*/
/*! Set the min and max DICOM coords that can occur:
    scan all 8 possible corners of the box the matrix defines.
--------------------------------------------------------------------------*/

void THD_set_dicom_box( THD_dataxes *dax )
{
   float x,y,z , nx1,ny1,nz1 ;
   float xbot,ybot,zbot , xtop,ytop,ztop ;

   if( dax == NULL || !ISVALID_MAT44(dax->ijk_to_dicom) ) return ;

   nx1 = dax->nxx - 1.0f; ny1 = dax->nyy - 1.0f; nz1 = dax->nzz - 1.0f;

   MAT44_VEC(dax->ijk_to_dicom , 0,0,0 , x,y,z ) ;        /* (0,0,0) corner */
   xbot = xtop = x ; ybot = ytop = y ; zbot = ztop = z ;

     /* this macro checks the (a,b,c) corner and updates the bot/top values */

#undef  BT
#define BT(a,b,c)                                        \
 do{ MAT44_VEC(dax->ijk_to_dicom , a,b,c , x,y,z ) ;     \
     xbot = MIN(xbot,x); xtop = MAX(xtop,x) ;            \
     ybot = MIN(ybot,y); ytop = MAX(ytop,y) ;            \
     zbot = MIN(zbot,z); ztop = MAX(ztop,z) ; } while(0)

                     BT(nx1, 0 , 0 ) ; BT( 0 ,ny1, 0 ) ; BT(nx1,ny1, 0 ) ;
   BT( 0 , 0 ,nz1) ; BT(nx1, 0 ,nz1) ; BT( 0 ,ny1,nz1) ; BT(nx1,ny1,nz1) ;

   dax->dicom_xxmin = xbot ; dax->dicom_xxmin = xtop ;
   dax->dicom_yymin = ybot ; dax->dicom_yymin = ytop ;
   dax->dicom_zzmin = zbot ; dax->dicom_zzmin = ztop ;

#undef BT
   return ;
}

/*---------------------------------------------------------------------------*/
/*! Given the ijk_to_dicom index to DICOM transformation in the header, AND
    the nxx,nyy,nzz values, load the legacy dataxes information:
      - xxorient  = Orientation code
      - yyorient  = Orientation code
      - zzorient  = Orientation code
      - xxorg     = Center of (0,0,0) voxel
      - yyorg     = Center of (0,0,0) voxel
      - zzorg     = Center of (0,0,0) voxel
      - xxdel     = Spacings between voxel centers (mm) - may be negative
      - yydel     = Spacings between voxel centers (mm) - may be negative
      - zzdel     = Spacings between voxel centers (mm) - may be negative
      - xxmin     = Min value of x (etc.)
      - xxmax     = Min value of x (etc.)
      - to_dicomm = Orthogonal matrix transforming from dataset coordinates
                    to DICOM coordinates

   Return value is -1 if there is an error, 0 if not.
-----------------------------------------------------------------------------*/

int THD_daxes_from_mat44( THD_dataxes *dax )
{
   int icod , jcod , kcod ;
   mat44 nmat ;
   float xx,yy,zz , ss , aa,bb,cc ;

   /* table to convert NIfTI-1 orientation codes (1..6)
      into AFNI orientation codes (0..5, and in a different order) */

   static int orient_nifti2afni[7] =
               { -1 , ORI_L2R_TYPE, ORI_R2L_TYPE, ORI_P2A_TYPE,
                      ORI_A2P_TYPE, ORI_I2S_TYPE, ORI_S2I_TYPE } ;

   if( dax == NULL ) return -1 ;
   if( dax->nxx < 1 || dax->nyy < 1 || dax->nzz < 1 ) return -1 ;

   /* use the NIfTI-1 library function to determine best orientation;
      but, must remember that NIfTI-1 x and y are reversed from AFNI's,
      so we must negate the x and y rows of the matrix before func call */

   nmat = dax->ijk_to_dicom ; XYINVERT_MAT44(nmat) ;

   nifti_mat44_to_orientation( nmat , &icod, &jcod, &kcod ) ;

   if( icod == 0 || jcod == 0 || kcod == 0 ) return -1 ;

   dax->xxorient = orient_nifti2afni[icod] ;
   dax->yyorient = orient_nifti2afni[jcod] ;
   dax->zzorient = orient_nifti2afni[kcod] ;

   /* grid offset int i-direction is projection of last
      column of ijk_to_dicom matrix (the shifts) along the
      direction of the first column of the matrix (the i-column) */

   aa = dax->ijk_to_dicom.m[0][3] ;  /* extract last column */
   bb = dax->ijk_to_dicom.m[1][3] ;
   cc = dax->ijk_to_dicom.m[2][3] ;

   xx = dax->ijk_to_dicom.m[0][0] ;  /* extract 1st column */
   yy = dax->ijk_to_dicom.m[1][0] ;
   zz = dax->ijk_to_dicom.m[2][0] ;
   ss = sqrt(xx*xx+yy*yy+zz*zz) ; if( ss == 0.0f ) ss = 1.0f ;
   dax->xxorg = (xx*aa+yy*bb+zz*cc) / ss ;
   if( ORIENT_sign[dax->xxorient] == '-' ) dax->xxorg = -dax->xxorg ;

   xx = dax->ijk_to_dicom.m[0][1] ;  /* extract 2nd column */
   yy = dax->ijk_to_dicom.m[1][1] ;
   zz = dax->ijk_to_dicom.m[2][1] ;
   ss = sqrt(xx*xx+yy*yy+zz*zz) ; if( ss == 0.0f ) ss = 1.0f ;
   dax->yyorg = (xx*aa+yy*bb+zz*cc) / ss ;
   if( ORIENT_sign[dax->yyorient] == '-' ) dax->yyorg = -dax->yyorg ;

   xx = dax->ijk_to_dicom.m[0][2] ;  /* extract 3rd column */
   yy = dax->ijk_to_dicom.m[1][2] ;
   zz = dax->ijk_to_dicom.m[2][2] ;
   ss = sqrt(xx*xx+yy*yy+zz*zz) ; if( ss == 0.0f ) ss = 1.0f ;
   dax->zzorg = (xx*aa+yy*bb+zz*cc) / ss ;
   if( ORIENT_sign[dax->zzorient] == '-' ) dax->zzorg = -dax->zzorg ;

   /* grid spacing along i-direction is length of 1st column of matrix */

   dax->xxdel = sqrt( SQR(dax->ijk_to_dicom.m[0][0])
                     +SQR(dax->ijk_to_dicom.m[1][0])
                     +SQR(dax->ijk_to_dicom.m[2][0]) ) ;
   if( ORIENT_sign[dax->xxorient] == '-' ) dax->xxdel = -dax->xxdel ;

   /* mutatis mutandis for j- and k-directions */

   dax->yydel = sqrt( SQR(dax->ijk_to_dicom.m[0][1])
                     +SQR(dax->ijk_to_dicom.m[1][1])
                     +SQR(dax->ijk_to_dicom.m[2][1]) ) ;
   if( ORIENT_sign[dax->yyorient] == '-' ) dax->yydel = -dax->yydel ;

   dax->zzdel = sqrt( SQR(dax->ijk_to_dicom.m[0][2])
                     +SQR(dax->ijk_to_dicom.m[1][2])
                     +SQR(dax->ijk_to_dicom.m[2][2]) ) ;
   if( ORIENT_sign[dax->zzorient] == '-' ) dax->zzdel = -dax->zzdel ;

   /* to_dicomm orthogonal matrix:
      we make an orthogonal matrix out of the columns of ijk_to_dicom */

   nmat = nifti_make_orthog_mat44(
    dax->ijk_to_dicom.m[0][0], dax->ijk_to_dicom.m[1][0], dax->ijk_to_dicom.m[2][0],
    dax->ijk_to_dicom.m[0][1], dax->ijk_to_dicom.m[1][1], dax->ijk_to_dicom.m[2][1],
    dax->ijk_to_dicom.m[0][2], dax->ijk_to_dicom.m[1][2], dax->ijk_to_dicom.m[2][2] );

   dax->to_dicomm.mat[0][0] = nmat.m[0][0] ;
   dax->to_dicomm.mat[0][1] = nmat.m[1][0] ;
   dax->to_dicomm.mat[0][2] = nmat.m[2][0] ;
   dax->to_dicomm.mat[1][0] = nmat.m[0][1] ;
   dax->to_dicomm.mat[1][1] = nmat.m[1][1] ;
   dax->to_dicomm.mat[1][2] = nmat.m[2][1] ;
   dax->to_dicomm.mat[2][0] = nmat.m[0][2] ;
   dax->to_dicomm.mat[2][1] = nmat.m[1][2] ;
   dax->to_dicomm.mat[2][2] = nmat.m[2][2] ;

   /** min and max values of (x,y,z) **/

   dax->xxmin = dax->xxorg ;
   dax->xxmax = dax->xxorg + (dax->nxx-1) * dax->xxdel ;
   if( dax->xxmin > dax->xxmax ){
     float temp = dax->xxmin ;
     dax->xxmin = dax->xxmax ; dax->xxmax = temp ;
   }

   dax->yymin = dax->yyorg ;
   dax->yymax = dax->yyorg + (dax->nyy-1) * dax->yydel ;
   if( dax->yymin > dax->yymax ){
     float temp = dax->yymin ;
     dax->yymin = dax->yymax ; dax->yymax = temp ;
   }

   dax->zzmin = dax->zzorg ;
   dax->zzmax = dax->zzorg + (dax->nzz-1) * dax->zzdel ;
   if( dax->zzmin > dax->zzmax ){
     float temp = dax->zzmin ;
     dax->zzmin = dax->zzmax ; dax->zzmax = temp ;
   }

#ifdef EXTEND_BBOX
   dax->xxmin -= 0.5 * fabsf(dax->xxdel) ;  /* pushes edges back by 1/2  */
   dax->xxmax += 0.5 * fabsf(dax->xxdel) ;  /* voxel dimensions (the box */
   dax->yymin -= 0.5 * fabsf(dax->yydel) ;  /* defined above is based on */
   dax->yymax += 0.5 * fabsf(dax->yydel) ;  /* voxel centers, not edges) */
   dax->zzmin -= 0.5 * fabsf(dax->zzdel) ;
   dax->zzmax += 0.5 * fabsf(dax->zzdel) ;
#endif

   return 0 ;
}

/*-----------------------------------------------------------------------*/
/*! Inputs:
      - matrix old_mat: transforms (i,j,k) indexes to (x,y,z) coords
      - ni,nj,nk: number of grid points along each direction
      - ri,rj,rk: new grid spacing along each direction
    Outputs:
      - ninew,njnew,nknew: number of grid points along each new direction
      - return value: new matrix; the [3][3] element will be 0 on error
-------------------------------------------------------------------------*/

mat44 THD_resample_mat44( mat44 old_mat ,
                          int ni     , int nj     , int nk    ,
                          float ri   , float rj   , float rk  ,
                          int *ninew , int *njnew , int *nknew )
{
   mat44 new_mat ;
   float di,dj,dk , fi,fj,fk , aa,bb,cc , pp,qq,rr ;

   ZZME(new_mat) ;
   new_mat.m[3][3] = 0.0f ;   /* mark output as bad; will fix later */

   if( !ISVALID_MAT44(old_mat)     ||
       ninew == NULL || njnew == NULL || nknew == NULL ||
       ni    <= 0    || nj    <= 0    || nk    <= 0      ) return new_mat ;

   /* spacing along input (i,j,k) directions */

   di = MAT33_CLEN(old_mat,0) ; if( di == 0.0f ) di == 1.0f ;
   dj = MAT33_CLEN(old_mat,1) ; if( dj == 0.0f ) dj == di   ;
   dk = MAT33_CLEN(old_mat,2) ; if( dk == 0.0f ) dk == di   ;

   if( ri <= 0.0f ) ri = 1.0f ;   /* don't allow negative spacing    */
   if( rj <= 0.0f ) rj = ri   ;   /* along output (i,j,k) directions */
   if( rk <= 0.0f ) rk = ri   ;

   fi = ri/di ; fj = rj/dj ; fk = rk/dk ;

   /* copy input matrix to output,
      then scale the upperleft 3x3 to allow for new spacings */

   new_mat = old_mat ;

   new_mat.m[0][0] *= fi ;   /* first column */
   new_mat.m[1][0] *= fi ;
   new_mat.m[2][0] *= fi ;
   new_mat.m[0][1] *= fj ;   /* second column */
   new_mat.m[1][1] *= fj ;
   new_mat.m[2][1] *= fj ;
   new_mat.m[0][2] *= fk ;   /* third column */
   new_mat.m[1][2] *= fk ;
   new_mat.m[2][2] *= fk ;

   /* number of points along each axis in new grid */

   *ninew = (int)rint(ni/fi) ;  /* so that ninew*ri == ni*di */
   *njnew = (int)rint(nj/fj) ;
   *nknew = (int)rint(nk/fk) ;

   /* now compute offset (fourth column) in new_mat so
      that the input and output grids have the same (x,y,z) centers;
      center of input is at (i,j,k) = 0.5*(ni-1,nj-1,nk-1), et cetera */

   di = 0.5*( ni   -1) ; dj = 0.5*( nj   -1) ; dk = 0.5*( nk   -1) ;
   fi = 0.5*(*ninew-1) ; fj = 0.5*(*njnew-1) ; fk = 0.5*(*nknew-1) ;

   MAT33_VEC( old_mat , di,dj,dk , aa,bb,cc ) ;
   MAT33_VEC( new_mat , fi,fj,fk , pp,qq,rr ) ;

   /* adjust so that MAT44_VEC(new_mat,fi,fj,fk,...) is
      same result as MAT44_VEC(old_mat,di,dj,dk,...)    */

   new_mat.m[0][3] += (aa-pp) ;
   new_mat.m[1][3] += (bb-qq) ;
   new_mat.m[2][3] += (cc-rr) ;

   return new_mat ;
}
