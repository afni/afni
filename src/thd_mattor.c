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
   int i,j,k,p,q,r , ibest,jbest,kbest,pbest,qbest,rbest ;
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
