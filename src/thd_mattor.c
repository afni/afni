#include "mrilib.h"

/*---------------------------------------------------------------------------*/
/*! - Input = matrix that transforms (i,j,k) indexes to RAI (x,y,z) coords.
    - Output = AFNI orientation codes for (i,j,k) axes.
    - The method is to find which permutation of (x,y,z) has the smallest
      angle to the (i,j,k) axes directions.
    - 27 Aug 2003 - RWCox
-----------------------------------------------------------------------------*/

THD_ivec3 THD_matrix_to_orientation( THD_mat33 R )
{
   double xi,xj,xk , yi,yj,yk , zi,zj,zk , val,detQ,detP ;
   THD_dmat33 P , Q , M ;
   THD_ivec3 vor ;
   int i=0,j=0,k=0,p,q,r , ibest,jbest,kbest,pbest,qbest,rbest ;
   double vbest ;

   LOAD_IVEC3(vor,ORI_R2L_TYPE,ORI_A2P_TYPE,ORI_I2S_TYPE) ; /* default */

   UNLOAD_MAT(R,xi,xj,xk,yi,yj,yk,zi,zj,zk) ;

   /* normalize column vectors to get unit vectors along each ijk-axis */

   val = sqrt( xi*xi + yi*yi + zi*zi ) ;
   if( val == 0.0 ){ xi  = 1.0 ; yi  = 0.0 ; zi  = 0.0 ; }
   else            { xi /= val ; yi /= val ; zi /= val ; }

   val = sqrt( xj*xj + yj*yj + zj*zj ) ;
   if( val == 0.0 ){ xj  = 0.0 ; yj  = 1.0 ; zj  = 0.0 ; }
   else            { xj /= val ; yj /= val ; zj /= val ; }

   val = xi*xj + yi*yj + zi*zj ;    /* orthgonalize col #2 to col #1 */
   if( fabs(val) > 1.e-5 ){
     xj -= val*xi ; yj -= val*yi ; zj -= val*zi ;
     val = sqrt( xj*xj + yj*yj + zj*zj ) ;
     xj /= val ; yj /= val ; zj /= val ;
   }

   val = sqrt( xk*xk + yk*yk + zk*zk ) ;
   if( val == 0.0 ){ xk = yi*zj-zi*yj; yk = zi*xj-zj*xi ; zk=xi*yj-yi*xj ; }
   else            { xk /= val ; yk /= val ; zk /= val ; }

   val = xi*xk + yi*yk + zi*zk ;    /* orthogonalize col #3 to col #1 */
   if( fabs(val) > 1.e-5 ){
     xk -= val*xi ; yk -= val*yi ; zk -= val*zi ;
     val = sqrt( xk*xk + yk*yk + zk*zk ) ;
     xk /= val ; yk /= val ; zk /= val ;
   }
   val = xj*xk + yj*yk + zj*zk ;    /* and to col #2 */
   if( fabs(val) > 1.e-5 ){
     xk -= val*xj ; yk -= val*yj ; zk -= val*zj ;
     val = sqrt( xk*xk + yk*yk + zk*zk ) ;
     xk /= val ; yk /= val ; zk /= val ;
   }

   LOAD_DMAT(Q,xi,xj,xk,yi,yj,yk,zi,zj,zk) ;

   /* at this point, Q is the rotation matrix from the (i,j,k) to (x,y,z) axes */

   detQ = DMAT_DET(Q) ;
   if( detQ == 0.0 ) return vor ; /* shouldn't happen unless user is a dufis or oufis */

   /* build and test all possible +1/-1 permutation matrices */

   vbest = -666.0 ; ibest=pbest=qbest=rbest=1 ; jbest=2 ; kbest=3 ;
   for( i=1 ; i <= 3 ; i++ ){     /* i = column number to use for row #1 */
    for( j=1 ; j <= 3 ; j++ ){    /* j = column number to use for row #2 */
     if( i == j ) continue ;
      for( k=1 ; k <= 3 ; k++ ){  /* k = column number to use for row #3 */
       if( i == k || j == k ) continue ;
       LOAD_ZERO_DMAT(P) ;
       for( p=-1 ; p <= 1 ; p+=2 ){    /* p,q,r are -1 or +1      */
        for( q=-1 ; q <= 1 ; q+=2 ){   /* and go into rows #1,2,3 */
         for( r=-1 ; r <= 1 ; r+=2 ){
           P.mat[0][i-1] = p ; P.mat[1][j-1] = q ; P.mat[2][k-1] = r ;
           detP = DMAT_DET(P) ;                 /* sign of permutation */
           if( detP * detQ <= 0.0 ) continue ;  /* doesn't match sign of Q */
           M = DMAT_MUL(P,Q) ;

           /* we want the largest trace(M) == smallest angle to P axes */
           /* actual angle = 2.0*acos(0.5*sqrt(1.0+vbest)) (in radians) */

           val = M.mat[0][0] + M.mat[1][1] + M.mat[2][2] ; /* trace */
           if( val > vbest ){
             vbest = val ;
             ibest = i ; jbest = j ; kbest = k ;
             pbest = p ; qbest = q ; rbest = r ;
#if 0
fprintf(stderr,"mattor: vbest=%g (%g) i=%d j=%d k=%d p=%d q=%d r=%d\n",
        val, 2.0*acos(0.5*sqrt(1.0+vbest))*(180.0/3.14159265) , i,j,k,p,q,r) ;
#endif
           }
   }}}}}}

   switch( ibest*pbest ){
     case  1: i = ORI_R2L_TYPE ; break ;  /* xbest = +x-DICOM */
     case -1: i = ORI_L2R_TYPE ; break ;  /* xbest = -x-DICOM */
     case  2: i = ORI_A2P_TYPE ; break ;  /* xbest = +y-DICOM */
     case -2: i = ORI_P2A_TYPE ; break ;  /* xbest = -y-DICOM */
     case  3: i = ORI_I2S_TYPE ; break ;  /* xbest = +z-DICOM */
     case -3: i = ORI_S2I_TYPE ; break ;  /* xbest = -z-DICOM */
   }

   switch( jbest*qbest ){
     case  1: j = ORI_R2L_TYPE ; break ;
     case -1: j = ORI_L2R_TYPE ; break ;
     case  2: j = ORI_A2P_TYPE ; break ;
     case -2: j = ORI_P2A_TYPE ; break ;
     case  3: j = ORI_I2S_TYPE ; break ;
     case -3: j = ORI_S2I_TYPE ; break ;
   }

   switch( kbest*rbest ){
     case  1: k = ORI_R2L_TYPE ; break ;
     case -1: k = ORI_L2R_TYPE ; break ;
     case  2: k = ORI_A2P_TYPE ; break ;
     case -2: k = ORI_P2A_TYPE ; break ;
     case  3: k = ORI_I2S_TYPE ; break ;
     case -3: k = ORI_S2I_TYPE ; break ;
   }

   LOAD_IVEC3(vor,i,j,k) ; return vor ;
}

/*---------------------------------------------------------------------------*/
#include "thd_shear3d.h"
/*---------------------------------------------------------------------------*/

mat44 MAT44_to_rotation( mat44 amat )
{
   THD_dmat33 dmat,rmat ; mat44 pmat ; float dd ;

   LOAD_IDENT_MAT44(pmat) ;
   dd = MAT44_DET(amat) ; if( dd == 0.0f ) return pmat ;

   /* load amat to double matrix, extract rotation matrix part of it, load back into pmat */

   dmat.mat[0][0] = amat.m[0][0]; dmat.mat[0][1] = amat.m[0][1]; dmat.mat[0][2] = amat.m[0][2];
   dmat.mat[1][0] = amat.m[1][0]; dmat.mat[1][1] = amat.m[1][1]; dmat.mat[1][2] = amat.m[1][2];
   dmat.mat[2][0] = amat.m[2][0]; dmat.mat[2][1] = amat.m[2][1]; dmat.mat[2][2] = amat.m[2][2];
   rmat = DMAT_svdrot_newer(dmat) ;
   pmat.m[0][0] = rmat.mat[0][0]; pmat.m[0][1] = rmat.mat[0][1]; pmat.m[0][2] = rmat.mat[0][2];
   pmat.m[1][0] = rmat.mat[1][0]; pmat.m[1][1] = rmat.mat[1][1]; pmat.m[1][2] = rmat.mat[1][2];
   pmat.m[2][0] = rmat.mat[2][0]; pmat.m[2][1] = rmat.mat[2][1]; pmat.m[2][2] = rmat.mat[2][2];

   return pmat ;
}

/*---------------------------------------------------------------------------*/
/* Return the angle (radians) between the grid directions of the 2 matrices.
*//*-------------------------------------------------------------------------*/

float MAT44_angle( mat44 amat , mat44 bmat )
{
   mat44 pmat,qmat , ipmat,iqmat , zmat ;
   THD_dmat33 dmat , rmat ;
   float v1,v2 ;

   v1 = MAT44_DET(amat) ; if( v1 == 0.0f ) return PI ;  /* not a valid matrix! */
   v2 = MAT44_DET(bmat) ; if( v2 == 0.0f ) return PI ;
   if( v1*v2 < 0.0f ) return PI ;

   pmat = MAT44_to_rotation(amat) ; ipmat = MAT44_INV(pmat) ;
   qmat = MAT44_to_rotation(bmat) ; iqmat = MAT44_INV(qmat) ;

   zmat = MAT44_MUL(pmat,iqmat);
   v1   = zmat.m[0][0] + zmat.m[1][1] + zmat.m[2][2];
   if( v1 > -1.0f && v1 <= 3.0f ) v1 = acosf( 0.5f*(v1-1.0f) ) ; else v1 = PI ;

   zmat = MAT44_MUL(ipmat,qmat);
   v2   = zmat.m[0][0] + zmat.m[1][1] + zmat.m[2][2];
   if( v2 > -1.0f && v2 <= 3.0f ) v2 = acosf( 0.5f*(v2-1.0f) ) ; else v2 = PI ;

   if( v1 < v2 ) v2 = v1 ;
   return v2 ;
}

/*---------------------------------------------------------------------------*/
/* Rotation matrix of angle theta (radians)
   about the axis along vector (ax,ay,az) - right hand rule. [05 Nov 2020 RWC]
   See https://mathworld.wolfram.com/RodriguesRotationFormula.html
*//*-------------------------------------------------------------------------*/

mat33 THD_mat33_generic_rotation( float theta, float ax, float ay, float az )
{
   mat33 mrot ;
   float aqq , cth,sth,omc ;

   LOAD_IDENT_MAT33(mrot) ;                 /* identity matrix */

   if( theta == 0.0f ) return mrot ;        /* that was easy */

   aqq = sqrtf( ax*ax + ay*ay + az*az ) ;
   if( aqq == 0.0f )   return mrot ;        /* hopeless loser user */

   ax /= aqq ; ay /= aqq ; az /= aqq ;      /* L2 normalize axis vector */

   cth = cosf(theta) ;
   sth = sinf(theta) ;
   omc = sin(0.5f*theta) ; omc = 2.0f * omc*omc ;  /* omc = 1 - cos(theta) */

   mrot.m[0][0] = cth + ax*ax*omc          ;       /* first row */
   mrot.m[0][1] =       ax*ay*omc - az*sth ;
   mrot.m[0][2] =       ax*az*omc + ay*sth ;

   mrot.m[1][0] =       ay*ax*omc + az*sth ;       /* second row */
   mrot.m[1][1] = cth + ay*ay*omc          ;
   mrot.m[1][2] =       ay*az*omc - ax*sth ;

   mrot.m[2][0] =       az*ax*omc - ay*sth ;       /* third row */
   mrot.m[2][1] =       az*ay*omc + ax*sth ;
   mrot.m[2][2] = cth + az*az*omc          ;

   return mrot ;
}
