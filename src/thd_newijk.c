#include "mrilib.h"

/* cross product of 2 3-vectors (p,q,r) and (x,y,z) */

#define CROSS_PROD_1( p,q,r , x,y,z ) ( (q)*(z)-(r)*(y) )  /* 1st component */
#define CROSS_PROD_2( p,q,r , x,y,z ) ( (r)*(x)-(p)*(z) )  /* 2nd component */
#define CROSS_PROD_3( p,q,r , x,y,z ) ( (p)*(y)-(q)*(x) )  /* 3rd component */

#define IS_SMALL(x) (fabs(x) < 0.001)            /* is this a small number? */

#define DOTPROD( p,q,r , x,y,z ) ((p)*(x) + (q)*(y) + (r)*(z))  /* dot prod */
#define LEN(x,y,z)  sqrt((x)*(x)+(y)*(y)+(z)*(z))                /* L2 norm */

/*---------------------------------------------------------------------------*/
/* Fill the bounding boxes, once the grid sizes and dimensions are setup. */
/*---------------------------------------------------------------------------*/

void THD_fill_bounding_boxes( THD_dataxes *dax )
{
ENTRY("THD_fill_bounding_box") ;

   if( ISVALID_DATAXES(dax) && dax->nxx > 0 && dax->nyy > 0 && dax->nzz > 0 ){

     dax->xxmin = dax->xxorg; dax->xxmax = dax->xxorg+(dax->nxx-1)*dax->xxdel;
     if( dax->xxmin > dax->xxmax ){
       float temp = dax->xxmin; dax->xxmin = dax->xxmax; dax->xxmax = temp;
     }

     dax->yymin = dax->yyorg; dax->yymax = dax->yyorg+(dax->nyy-1)*dax->yydel;
     if( dax->yymin > dax->yymax ){
       float temp = dax->yymin; dax->yymin = dax->yymax; dax->yymax = temp;
     }

     dax->zzmin = dax->zzorg; dax->zzmax = dax->zzorg+(dax->nzz-1)*dax->zzdel;
     if( dax->zzmin > dax->zzmax ){
       float temp = dax->zzmin; dax->zzmin = dax->zzmax; dax->zzmax = temp;
     }

#ifdef EXTEND_BBOX
     dax->xxmin -= 0.5 * fabsf(dax->xxdel) ;  /* pushes edges back by 1/2  */
     dax->xxmax += 0.5 * fabsf(dax->xxdel) ;  /* voxel dimensions (the box */
     dax->yymin -= 0.5 * fabsf(dax->yydel) ;  /* defined above is based on */
     dax->yymax += 0.5 * fabsf(dax->yydel) ;  /* voxel centers, not edges) */
     dax->zzmin -= 0.5 * fabsf(dax->zzdel) ;
     dax->zzmax += 0.5 * fabsf(dax->zzdel) ;
#endif

     THD_set_dicom_box(dax) ;
   }

   EXRETURN ;
}

/*---------------------------------------------------------------------------*/
/*! Given a matrix 'R' that transforms ijk to DICOM-oriented xyz,
    fill in parts of an AFNI dataset dataxes struct 'dax'.
    Pieces of dax to be filled are
      [--- coordinates that are 'cardinal' == parallel to DICOM order ---]
      [---                                 == a permutation of DICOM  ---]
      * xxorg             == Center of (0,0,0) voxel
      * yyorg             == Center of (0,0,0) voxel
      * zzorg             == Center of (0,0,0) voxel
      * xxdel             == Spacing between voxel centers (mm) - maybe negative
      * yydel             == Spacing between voxel centers (mm) - maybe negative
      * zzdel             == Spacing between voxel centers (mm) - maybe negative
      * xxorient          == Orientation code
      * yyorient          == Orientation code
      * zzorient          == Orientation code
      * to_dicomm         == Orthogonal 3x3 matrix transforming from
                               dataset coordinates to DICOM coordinates
      * ijk_to_dicom      == 4x4 matrix taking ijk indexes to DICOM xyz coords
      * dicom_to_ijk      == 4x4 inverse of above
      The following are also set, if nxx,nyy,nzz fields are positive:
        * xxmin & dicom_xxmin  }
        * xxmax & dicom_xxmax  }
        * yymin & dicom_yymin  } These make up the bounding
        * yymax & dicom_yymax  } box for the dataset
        * zzmin & dicom_zzmin  }
        * zzmax & dicom_zzmax  }

      [--- coordinates that can be arbitrarily oriented in space ---]
      * ijk_to_dicom_real == 4x4 matrix that is possibly oblique - unlike the above
                               (columns will be orthogonal; that is,)
                               (the coordinate system is not skewed.)
      * ijk_to_dicom_orig == 4x4 matrix originally input == R
                               (will be different from ijk_to_dicom_real, if)
                               (input matrix doesn't have orthogonal columns)

     The primary purpose of this function is to fill in the older style
     'cardinal' coordinates with the closest approximations possible from
     the newer style 'real' coordinates. It is intended as a stopgap measure
     in an eventual transition to 'real' coordinates everywhere - especially
     within the AFNI image viewers.

     Note that if the input matrix R has only 2 linearly independent columns
     (describes a 2D plane, not a 3D space), this function will still work.
     The 3rd dimension will be made up from the cross product of the 2
     independent columns. But if there is only 1 LI column, then this
     function will not do anything useful. This situation should never
     arise, but who can tell with the kids these days?
*//*-------------------------------------------------------------------------*/

void THD_coord_fill_dataxes( mat44 R , THD_dataxes *dax )
{
   double r11,r12,r13 , r21,r22,r23 , r31,r32,r33 ;
   double xd,yd,zd , a,b,c,d , qx,qy,qz,qxyz[3] ;
   mat33 P,Q ; mat44 P4 , ijk_to_dxyz , dxyz_to_dicom ;
   int nn , icod,jcod,kcod ;

ENTRY("THD_coord_fill_dataxes") ;

   if( ! ISVALID_DATAXES(dax) ) EXRETURN ;  /* loser, not user */

   /* coordinate offsets = last column of R matrix (pretty trivial) */

   qxyz[0] = qx = R.m[0][3]; qxyz[1] = qy = R.m[1][3]; qxyz[2] = qz = R.m[2][3];

   /* unload 3x3 sub-matrix into local variables */

   UNLOAD_MAT33( R , r11,r12,r13 , r21,r22,r23 , r31,r32,r33 ) ;

   /* compute lengths of each column; these determine grid spacings  */
   /* Why columns?
        [ x ]   [ r11 r12 r13 ] [ i ]   [ qx ]
        [ y ] = [ r21 r22 r23 ] [ j ] + [ qy ]
        [ z ]   [ r31 r32 r33 ] [ k ]   [ qz ]
      Or
        [ x ]   [ r11 ]     [ r12 ]     [ r13 ]   + [ qx ]
        [ y ] = [ r21 ] i + [ r22 ] j + [ r23 ] k + [ qy ]
        [ z ] = [ r31 ]     [ r32 ]     [ r33 ]   + [ qz ]
      So adding 1 to i adds a copy of the first column to the xyz location
         adding 1 to j adds a copy of the second column, and
         adding 1 to k adds a copy of the third colum
      So the i-axis grid spacing is the length of the first column, etc. */

   xd = LEN(r11,r21,r31) ; /* these are non-negative lengths */
   yd = LEN(r12,r22,r32) ; /* 0 values are trouble */
   zd = LEN(r13,r23,r33) ;

   /* if 2 or 3 of these lengths are zero, give up */

   nn = IS_SMALL(xd) + IS_SMALL(yd) + IS_SMALL(zd) ;
   if( nn > 1 ){
     ERROR_message("Input coordinate matrix has %d super-small columns :(") ;
     EXRETURN ;
   }

   /* normalize the columns (where possible) */

   if( !IS_SMALL(xd) ){ r11 /= xd ; r21 /= xd ; r31 /= xd ; }
   if( !IS_SMALL(yd) ){ r12 /= yd ; r22 /= yd ; r32 /= yd ; }
   if( !IS_SMALL(zd) ){ r13 /= zd ; r23 /= zd ; r33 /= zd ; }

   /****** The following code deals with a defective 2D input matrix :( ******/
   /* If a column length is 0, patch the trouble - only one of them can be 0 */
   /* Make up a new column that is the cross product of the nonzero columns, */
   /* and assign its arbitrary length to be the geometric mean of the others */
   /* These choices are arbitrary, and the user should be calumniated aloud! */

   if( IS_SMALL(xd) ){
     r11 = CROSS_PROD_1( r12,r22,r32 , r13,r23,r33 ) ; /* col2 cross col3 */
     r21 = CROSS_PROD_2( r12,r22,r32 , r13,r23,r33 ) ;
     r31 = CROSS_PROD_3( r12,r22,r32 , r13,r23,r33 ) ;
     xd  = LEN(r11,r21,r31) ;
     if( IS_SMALL(xd) ){
       ERROR_message("Input coordinate matrix column 1 cannot be fixed :(") ; EXRETURN ;
     } else {
       WARNING_message("Input coordinate matrix - had to fix column 1") ;
     }
     r11 /= xd ; r21 /= xd ; r31 /= xd ;
     xd  = sqrt(yd*zd) ;
   } else if( IS_SMALL(yd) ){
     r12 = CROSS_PROD_1( r13,r23,r33 , r11,r21,r31 ) ; /* col3 cross col1 */
     r22 = CROSS_PROD_2( r13,r23,r33 , r11,r21,r31 ) ;
     r32 = CROSS_PROD_3( r13,r23,r33 , r11,r21,r31 ) ;
     yd  = LEN(r12,r22,r32) ;
     if( IS_SMALL(yd) ){
       ERROR_message("Input coordinate matrix column 2 cannot be fixed :(") ; EXRETURN ;
     } else {
       WARNING_message("Input coordinate matrix - had to fix column 2") ;
     }
     r12 /= yd ; r22 /= yd ; r32 /= yd ;
     yd  = sqrt(xd*zd) ;
   } else if( IS_SMALL(zd) ){
     r13 = CROSS_PROD_1( r11,r21,r31 , r12,r22,r32 ) ; /* col1 cross col2 */
     r23 = CROSS_PROD_2( r11,r21,r31 , r12,r22,r32 ) ;
     r33 = CROSS_PROD_3( r11,r21,r31 , r12,r22,r32 ) ;
     zd  = LEN(r13,r23,r33) ;
     if( IS_SMALL(zd) ){
       ERROR_message("Input coordinate matrix column 3 cannot be fixed :(") ; EXRETURN ;
     } else {
       WARNING_message("Input coordinate matrix - had to fix column 3") ;
     }
     r13 /= zd ; r23 /= zd ; r33 /= zd ;
     zd  = sqrt(xd*yd) ;
   }

   /* copy input matrix unaltered to _orig;
      this is the only completely trivial part of this function */

   dax->ijk_to_dicom_orig = R ;

   /* At this point, the 'r' matrix has normal columns, but we have to allow
      for the fact that the hideous user may not have given us a matrix
      with orthogonal columns (i.e., slanted dataset axes!?).

      So, now find the orthogonal matrix closest to the current matrix.

      One reason for using the polar decomposition to get this orthogonal
      matrix, rather than just directly orthogonalizing the columns
      (e.g., Gram-Schmidt), is so that inputting the inverse matrix to R
      will result in the inverse orthogonal matrix at this point.
      If we just orthogonalized the columns, this wouldn't necessarily hold. */

   LOAD_MAT33( Q , r11,r12,r13 , r21,r22,r23 , r32,r32,r33 ) ;  /* load Q */

   P = nifti_mat33_polar(Q) ;  /* P is orthog matrix closest to Q */

   UNLOAD_MAT33(P , r11,r12,r13 , r21,r22,r23 , r31,r32,r33 ) ; /* unload P */

   /*                            [ r11 r12 r13 ]                   */
   /* at this point, the matrix  [ r21 r22 r23 ] is orthogonal-ish */
   /*                            [ r31 r32 r33 ]                   */

   /* Scale the columns back to have the correct lengths */

   r11 *= xd ; r21 *= xd ; r31 *= xd ;
   r12 *= yd ; r22 *= yd ; r32 *= yd ;
   r13 *= zd ; r23 *= zd ; r33 *= zd ;

   /* Now load the ijk_to_dicom_real matrix from 'r' */
   /* This matrix may well be virtually identical to the input 'R' */

   LOAD_MAT44( dax->ijk_to_dicom_real ,
               r11 , r12 , r13 , qx ,
               r21 , r22 , r23 , qy ,
               r31 , r32 , r33 , qz  ) ;

   /* get the closest cardinal orientation codes */

   MAT33_TO_MAT44(P,P4) ;
   nifti_mat44_to_orientation( P4 , &icod , &jcod , &kcod ) ;
   if( icod == 0 || jcod == 0 || kcod == 0 ){  /* should not happen */
     WARNING_message("Input coordinate matrix - orientation confusion :(") ;
     icod = NIFTI_R2L ;  /* pretend it is axial in DICOM orientation */
     jcod = NIFTI_A2P ;
     kcod = NIFTI_I2S ;
   }
   dax->xxorient = icod = ORIENT_FROM_NIFTI(icod) ; /* convert to AFNI codes */
   dax->yyorient = jcod = ORIENT_FROM_NIFTI(jcod) ;
   dax->zzorient = kcod = ORIENT_FROM_NIFTI(kcod) ;

   /* save grid spacings */

   dax->xxdel = (ORIENT_sign[icod] == '+') ? xd : -xd ;
   dax->yydel = (ORIENT_sign[jcod] == '+') ? yd : -yd ;
   dax->zzdel = (ORIENT_sign[kcod] == '+') ? zd : -zd ;

   /* origins (don't you love the double indexing? No?)  */

   dax->xxorg = qxyz[ ORIENT_xyzint [ icod ] - 1 ] ;
   dax->yyorg = qxyz[ ORIENT_xyzint [ jcod ] - 1 ] ;
   dax->zzorg = qxyz[ ORIENT_xyzint [ kcod ] - 1 ] ;

   /* set the other 'cardinal' matrices */

   THD_set_daxes_to_dicomm( dax ) ; /* set the to_dicomm field */

   /* ijk_to_dxyz: transforms (i,j,k) to cardinal (x,y,z) coords */

   LOAD_MAT44( ijk_to_dxyz ,
               dax->xxdel , 0.0f       , 0.0f       , dax->xxorg ,
               0.0f       , dax->yydel , 0.0f       , dax->yyorg ,
               0.0f       , 0.0f       , dax->zzdel , dax->zzorg  ) ;

   /* dxyz_to_dicom: transforms dataset (x,y,z) coords to DICOM coords */

   THDMAT33_TO_MAT44( dax->to_dicomm , dxyz_to_dicom ) ;

   /* dax->ijk_to_dicom: transforms (i,j,k) to DICOM (x,y,z) */

   dax->ijk_to_dicom = THD_mat44_mul( dxyz_to_dicom , ijk_to_dxyz ) ;

   /* and the inverse transformation: DICOM (x,y,z) to indexes (i,j,k) */

   dax->dicom_to_ijk = MAT44_INV( dax->ijk_to_dicom ) ;

   /* and the bounding boxes (if grid dimensions are set) */

   THD_fill_bounding_boxes(dax) ;

   EXRETURN ;
}
