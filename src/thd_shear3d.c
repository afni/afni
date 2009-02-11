/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/

#include "thd_shear3d.h"

#include "debugtrace.h"  /* 29 Jan 2001 */

/*===========================================================================
  Routines to manipulate 3D shears, used for rotating 3D volumes - RWCox
=============================================================================*/

/*----------------------------------------------------------------------
   "Norm" of a set of shears [maximum stretch factor]
------------------------------------------------------------------------*/

#define BIG_NORM 1.e+38

double norm_3shear( MCW_3shear sh )
{
   double top=0.0 , val ;
   int ii , jj ;

   if( ! ISVALID_3SHEAR(sh) ) return BIG_NORM ;

   for( ii=0 ; ii < 3 ; ii++ ){
     jj  = sh.ax[ii] ;
     val = fabs( sh.scl[ii][(jj+1)%3] ) ; if( val > top ) top = val ;
     val = fabs( sh.scl[ii][(jj+2)%3] ) ; if( val > top ) top = val ;
   }

   return top ;
}

/*----------------------------------------------------------------------
  Create a 3x3 matrix from one shear
------------------------------------------------------------------------*/

THD_dmat33 make_shear_matrix( int ax , double scl[3] )
{
   THD_dmat33 m ;

   switch( ax ){
      case 0:  LOAD_SHEARX_DMAT( m , scl[0],scl[1],scl[2] ) ; break ;
      case 1:  LOAD_SHEARY_DMAT( m , scl[1],scl[0],scl[2] ) ; break ;
      case 2:  LOAD_SHEARZ_DMAT( m , scl[2],scl[0],scl[1] ) ; break ;
      default: LOAD_ZERO_DMAT( m )                          ; break ;
   }
   return m ;
}

/*------------------------------------------------------------------------
  Permute the axes of a shear structure so that what was
  in order (0,1,2) is now in order (ox1,ox2,ox3).
  If P is the 3x3 matrix [ delta(i,ox{j+1}) ] (i,j=0,1,2), then

                                        T
      [output shear] = P [input shear] P
--------------------------------------------------------------------------*/

MCW_3shear permute_3shear( MCW_3shear shin , int ox1, int ox2, int ox3 )
{
   MCW_3shear shout ;
   int ii , ain,aout , pi[3] ;

   ZZME(shout) ;
   /* sanity check */

   if( ! ISVALID_3SHEAR(shin) ){ INVALIDATE_3SHEAR(shout) ; return shout ; }

   pi[0] = ox1 ; pi[1] = ox2 ; pi[2] = ox3 ;

   for( ii=0 ; ii < 4 ; ii++ ){

      ain  = shin.ax[ii] ;   /* axis of input */
      aout = pi[ain] ;       /* axis of output */

      shout.ax[ii]         = aout ;             /* store new axis */
      shout.scl[ii][pi[0]] = shin.scl[ii][0] ;  /* permuted scalings */
      shout.scl[ii][pi[1]] = shin.scl[ii][1] ;
      shout.scl[ii][pi[2]] = shin.scl[ii][2] ;
      shout.sft[ii]        = shin.sft[ii] ;     /* copy shift */
   }

   shout.flip0 = shin.flip0 ; shout.flip1 = shin.flip1 ;

   return shout ;
}

/*------------------------------------------------------------------------
  Permute a 3x3 matrix:             T
                        [output] = P  [q] P
--------------------------------------------------------------------------*/

THD_dmat33 permute_dmat33( THD_dmat33 q , int ox1, int ox2, int ox3 )
{
   THD_dmat33 m ;
   int ii , jj , pi[3] ;

   pi[0] = ox1 ; pi[1] = ox2 ; pi[2] = ox3 ;

   for( ii=0 ; ii < 3 ; ii++ )
      for( jj=0 ; jj < 3 ; jj++ )
         m.mat[ii][jj] = q.mat[ pi[ii] ] [ pi[jj] ] ;

   return m ;
}

/*------------------------------------------------------------------------
  Permute a 3vector:             T
                     [output] = P  [q]
--------------------------------------------------------------------------*/

THD_dfvec3 permute_dfvec3( THD_dfvec3 q , int ox1, int ox2, int ox3 )
{
   THD_dfvec3 m ;
   int ii , pi[3] ;

   pi[0] = ox1 ; pi[1] = ox2 ; pi[2] = ox3 ;

   for( ii=0 ; ii < 3 ; ii++ )
      m.xyz[ii] = q.xyz[ pi[ii] ] ;

   return m ;
}

/*------------------------------------------------------------------------
   Function to compute the decomposition of a 3x3 matrix into a product
   of 4 shears:

     Q = Sx[bx2,cx2,f] Sz[az,bz,f] Sy[ay,cy,f] Sx[bx1,cx1,1]

   where             [ f b c ]              [ 1 0 0 ]              [ 1 0 0 ]
         Sx[b,c,f] = [ 0 1 0 ]  Sy[a,c,f] = [ a f b ]  Sz[a,b,f] = [ 0 f 0 ]
                     [ 0 0 0 ]              [ 0 0 1 ]              [ a b 1 ]

   "f" is a stretching/shrinking factor applied with the 1D shear.
   If det[Q] = 1 , then f = 1 and these are pure shears.

   In addition, a shift is allowed for, so that the total coordinate
   transformation being decomposed is really

       [ x ]          [ x ]     [ xdel ]
       [ y ]   =  [Q] [ y ]   + [ ydel ]
       [ z ]          [ z ]     [ zdel ]
            new            old

   The output shifts are applied to the last three transformations, so
   that the factoring produces this sequence of operations:

       [ x ]           [ x ]
       [ y ]   = [Sx1] [ y ]
       [ z ]           [ z ]
            1               old

       [ x ]           [ x ]    [ 0  ]
       [ y ]   = [Sy]  [ y ]  + [ dy ]
       [ z ]           [ z ]    [ 0  ]
            2               1

       [ x ]           [ x ]    [ 0  ]
       [ y ]   = [Sz]  [ y ]  + [ 0  ]
       [ z ]           [ z ]    [ dz ]
            3               2

       [ x ]           [ x ]    [ dx ]
       [ y ]   = [Sx2] [ y ]  + [ 0  ]
       [ z ]           [ z ]    [ 0  ]
            new             3

   The function returns a set of shears in the MCW_3shear struct type.

   The C code for generating the parameters (f,bx2,cx2,az,bz,ay,cy,bx1,cx1,dx,dy,cz)
   was generated by the following Maple V Release 4 script:

    with(linalg) : readlib(C) :

    Sx := (f,b,c,dx) -> matrix( [ [f,b,c,dx], [0,1,0,0 ], [0,0,1,0 ], [0,0,0,1] ] ) :
    Sy := (f,a,c,dy) -> matrix( [ [1,0,0,0 ], [a,f,c,dy], [0,0,1,0 ], [0,0,0,1] ] ) :
    Sz := (f,a,b,dz) -> matrix( [ [1,0,0,0 ], [0,1,0,0 ], [a,b,f,dz], [0,0,0,1] ] ) :

    SS := evalm( Sx(f,bx2,cx2,dx) &* Sz(f,az,bz,dz) &* Sy(f,ay,cy,dy) &* Sx(1,bx1,cx1,0) ) :

    QQ := matrix( [ [q11,q12,q13,xdel], [q21,q22,q23,ydel], [q31,q32,q33,zdel], [0,0,0,1] ] ) :

    ee := { seq( SS[i,1]-QQ[i,1] , i=1..3 ) ,
            seq( SS[i,2]-QQ[i,2] , i=1..3 ) ,
            seq( SS[i,3]-QQ[i,3] , i=1..3 ) ,
            seq( SS[i,4]-QQ[i,4] , i=1..3 )  } :

    vv := { f,bx2,cx2,az,bz,ay,cy,bx1,cx1,dx,dy,dz } :

    s1 := solve( ee ,vv ) :
    s2 := map( x -> convert(x,radical) , s1 ) :
    ss := [ op(s2) ] :
    C(ss,optimized) ;
--------------------------------------------------------------------------*/

MCW_3shear shear_xzyx( THD_dmat33 *q , THD_dfvec3 *xyzdel )
{
   /* input variables */

   double q11,q12,q13,q21,q22,q23,q31,q32,q33 , xdel,ydel,zdel ;

   /* computed parameters */

   double f,bx2,cx2,az,bz,ay,cy,bx1,cx1 , dx,dy,dz ;

   /* output variable */

   MCW_3shear shr ;
   /* internals (created by Maple) */

   double t1, t3, t4, t5, t6, t7, t8, t9, t10, t11,
          t12, t13, t15, t16, t17, t18, t19, t20, t22, t23,
          t24, t25, t26, t27, t28, t29, t30, t32, t34, t35,
          t36, t37, t38, t44, t45, t47, t50, t51, t53, t54,
          t55, t57, t61, t62, t64, t66, t67, t68, t69, t70,
          t73, t75, t77, t78, t79, t80, t81, t84, t85, t86,
          t87, t89, t90, t92, t94, t96, t102, t107, t109, t113,
          t118, t119, t121, t123, t125, t127, t129, t131, t132, t134,
          t141, t145, t148, t150, t151, t157, t160, t163, t164, t167,
          t185, t190, t193, t194, t195, t203, t206, t207, t210, t220,
          t221, t224, t230, t233, t238, t240, t241, t252, t264, t267,
          t269, t275, t292;

   /* initialize output to an invalid result */

   ZZME(shr) ;
   INVALIDATE_3SHEAR(shr) ;

   /* load inputs into local variables */

   UNLOAD_DMAT( *q , q11,q12,q13,q21,q22,q23,q31,q32,q33 ) ;
   xdel = xyzdel->xyz[0] ; ydel = xyzdel->xyz[1] ; zdel = xyzdel->xyz[2] ;

   /* the code generated by Maple, slightly massaged */

      ay = q21;
      dy = ydel;
      t1 = q21*q12;
      t3 = q13*q22;
      t4 = t3*q31;
      t5 = q21*q13;
      t6 = t5*q32;
      t7 = q23*q11;
      t8 = t7*q32;
      t9 = q12*q23;
      t10 = t9*q31;
      t11 = q22*q11;
      t12 = t11*q33;
      t13 = t1*q33+t4-t6+t8-t10-t12;
      t15 = q32*q32;
      t16 = t15*q32;
      t17 = q21*q21;
      t18 = t17*q21;
      t19 = t16*t18;
      t20 = q22*q22;
      t22 = q31*q31;
      t23 = t22*q32;
      t24 = q21*t20*t23;
      t25 = t20*q22;
      t26 = t22*q31;
      t27 = t25*t26;
      t28 = t15*t17;
      t29 = q22*q31;
      t30 = t28*t29;
      t32 = t13*t13;

      t34 = (-t19-3.0*t24+t27+3.0*t30)*t32 ;
#if 0
      t34 = cbrt(t34) ;
#else
           if( t34 > 0.0 ) t34 =   pow(  t34 , 0.333333333333333 ) ;
      else if( t34 < 0.0 ) t34 = - pow( -t34 , 0.333333333333333 ) ;
      else                 t34 = 0.0 ;
#endif

      if( t13 == 0.0 ) return shr ;

      t35 = 1/t13*t34;
      t36 = t35+q31;
      t37 = t36*q21;
      t38 = q12*q33;
      t44 = t36*q23;
      t45 = q11*q32;
      t47 = t36*q12;
      t50 = t36*q22;
      t51 = q11*q33;
      t53 = q32*t17;
      t54 = t53*q12;
      t55 = q32*q21;
      t57 = q32*q31;
      t61 = q32*q23*q11*q31;
      t62 = q31*q21;
      t64 = q22*q12;
      t66 = t22*q23;
      t67 = t66*q12;
      t68 = t22*q13;
      t69 = t68*q22;
      t70 = t29*t51;
      t73 = -t37*t38-t36*q13*t29+t37*q13*q32-t44*t45+t47*q23*q31+t50*t51+t54-
             t55*t11-t57*t5+t61+t62*t38-t62*t64-t67+t69-t70+q31*t20*q11;
      t75 = t20*t22;

      t77 = (t28-2.0*t29*t55+t75) ;
      if( t77 == 0.0 ) return shr ;
      t77 = 1/t77 ;

      cx2 = t73*t77;
      t78 = t44*q31;
      t79 = t62*q22;
      t80 = t62*q33;
      t81 = t37*q33;
      t84 = t34*t34;

      if( t84 == 0.0 ) return shr ;
      t85 = 1/t84;

      cy = (-t78+t79-t80+t81-t53+t66)*t32*t85;
      t86 = q21*t22;
      t87 = t64*t36;
      t89 = t17*q12;
      t90 = t89*t36;
      t92 = t51*t22;
      t94 = t68*q32;
      t96 = t36*t36;
      t102 = t51*q31;
      t107 = t11*t36;
      t109 = t38*t22;
      t113 = t86*t87-t57*t90+2.0*t50*t92+2.0*t37*t94+t96*t22*t3+t96*q31*t8-3.0*
             t24-t96*q22*t102+3.0*t30+t27-2.0*t36*t26*t3-t19-t62*q32*t107-2.0*t37*t109-t26*
             q21*t64;
      t118 = q32*q22;
      t119 = t118*q11;
      t121 = q11*t36;
      t123 = t26*q13;
      t125 = t26*q23;
      t127 = q33*t26;
      t129 = t96*q12;
      t131 = t96*q21;
      t132 = t38*q31;
      t134 = t22*t22;
      t141 = q31*q13*q32;
      t145 = -q11*t15*t17*q31+t23*t89+t86*t119+t121*t28-t123*t55+t125*t45+t1*
             t127-t129*t66+t131*t132+t134*q13*q22-t9*t134-2.0*t36*t22*t8-t131*t141-t11*t127+
             2.0*t47*t125;

      if( t34 == 0.0 ) return shr ;
      t148 = 1/t34;

      if( q21 == 0.0 ) return shr ;
      t150 = 1/q21;

      t151 = t148*t77*t150;
      bx2 = (t113+t145)*t13*t151;
      az = -t35;
      f = (-t29+t55)*t13*t148;
      t157 = ydel*q12;
      t160 = zdel*t17;
      t163 = ydel*t22;
      t164 = t163*q21;
      t167 = ydel*t26;
      t185 = xdel*q21;
      t190 = ydel*q11;
      t193 = -ydel*q22*t51*t26-t157*q23*t134-t160*t129*q33+t164*t119+t163*t54-
             t167*q21*q22*q12+ydel*t134*t3+t157*q21*q33*t26-t167*t6-3.0*ydel*q21*t75*q32+
             t167*t8+3.0*ydel*t15*t17*q22*q31+t185*t20*t26-ydel*t16*t18-t190*t28*q31;
      t194 = zdel*q21;
      t195 = t125*q12;
      t203 = xdel*t18;
      t206 = xdel*t17;
      t207 = q22*t22;
      t210 = t160*q32;
      t220 = zdel*t18;
      t221 = q32*q12;
      t224 = t123*q22;
      t230 = t194*t96;
      t233 = t194*t195+t194*t22*t12+t160*t94-t194*q32*t7*t22+t203*q31*t15-2.0*
             t206*t207*q32-t210*t107-t194*t75*q11+t194*q31*t20*q11*t36+t210*t11*q31-t220*
             t221*q31-t194*t224+t160*t207*q12+ydel*t25*t26-t230*t8-t160*t109;
      t238 = t194*t36;
      t240 = ydel*t96;
      t241 = t240*q21;
      t252 = ydel*t36;
      t264 = -t203*t36*t15+t230*t12+2.0*t238*t61-t241*t141-t240*q22*t102+t220*
             t221*t36-t160*q31*t87+2.0*t206*t36*t29*q32-2.0*t252*t22*t8+t164*t87+t190*t36*
             t17*t15-t240*t67-2.0*t252*t224+2.0*t252*q22*t92+t241*t132;
      t267 = t160*t36;
      t269 = ydel*q31;
      t275 = t252*q21;
      t292 = -2.0*t238*t67+t240*t69+2.0*t267*t132-t269*q32*t90-t185*t36*t20*t22
             +2.0*t275*t94+t230*t10+2.0*t238*t69+2.0*t252*t195-t230*t4-2.0*t275*t109-2.0*
             t267*t141-2.0*t238*t70+t240*q31*t8-t269*q21*t118*t121+t160*t96*q13*q32;
      dx = -(t193+t233+t264+t292)*t13*t151;
      bz = t36*t150;
      cx1 = -(t78+t79-t80-t96*q23+t81-t53)*t150*t32*t85;
      dz = (-t252+t194)*t150;
      bx1 = -(-t50+t55)*t150*t13*t148;

   /* load computed values into output structure */

   shr.ax[3] = 0; shr.scl[3][0] = f; shr.scl[3][1] = bx2; shr.scl[3][2] = cx2; shr.sft[3] = dx;
   shr.ax[2] = 2; shr.scl[2][2] = f; shr.scl[2][0] = az ; shr.scl[2][1] = bz ; shr.sft[2] = dz;
   shr.ax[1] = 1; shr.scl[1][1] = f; shr.scl[1][0] = ay ; shr.scl[1][2] = cy ; shr.sft[1] = dy;
   shr.ax[0] = 0; shr.scl[0][0] = 1; shr.scl[0][1] = bx1; shr.scl[0][2] = cx1; shr.sft[0] = 0 ;

   shr.flip0 = shr.flip1 = -1 ;  /* no flips now */

   return shr ;
}

/*---------------------------------------------------------------------------------------
   Decompose transformation (q,xyzdel) into a set of 4 shears,
   whose axis order is ox1,ox2,ox3,ox1; that is,
     q = S    S    S    S
          ox1  ox3  ox2  ox1
-----------------------------------------------------------------------------------------*/

MCW_3shear shear_arb( THD_dmat33 *q , THD_dfvec3 *xyzdel , int ox1,int ox2,int ox3 )
{
   THD_dmat33 qq ;
   THD_dfvec3 xx ;
   MCW_3shear sh_xzyx , shout ;

   /* permute the input matrix and vector to the desired order */

   qq = permute_dmat33( *q      , ox1,ox2,ox3 ) ;
   xx = permute_dfvec3( *xyzdel , ox1,ox2,ox3 ) ;

   /* compute the Sx Sz Sy Sx factorization */

   sh_xzyx = shear_xzyx( &qq , &xx ) ;
   if( ! ISVALID_3SHEAR(sh_xzyx) ) return sh_xzyx ;

   /* permute the shear factorization back */

   shout = permute_3shear( sh_xzyx , ox1,ox2,ox3 ) ;

   return shout ;
}

/*-----------------------------------------------------------------------------------
   Find the "best" shear decomposition (smallest stretching factors).
   Input matrix q should have det(q)=1.
-------------------------------------------------------------------------------------*/

MCW_3shear shear_best( THD_dmat33 *q , THD_dfvec3 *xyzdel )
{
   MCW_3shear sh[6] ;
   int ii , jbest ;
   double val , best ;

   double dsum,esum ;   /* 29 Feb 2004 */

   dsum = DMAT_TRACE(*q) ;
   esum = fabs(q->mat[0][1]) + fabs(q->mat[0][2])
         +fabs(q->mat[1][0]) + fabs(q->mat[1][2])
         +fabs(q->mat[2][0]) + fabs(q->mat[2][1]) ;

   if( dsum >= 2.99999 && esum/dsum < 1.e-6 ){
     MCW_3shear shr ; double dx=xyzdel->xyz[0], dy=xyzdel->xyz[1], dz=xyzdel->xyz[2] ;
#if 0
     if( MRILIB_verbose ){
       fprintf(stderr,"shear_best: matrix=I?\n"); DUMP_DMAT33("matrix",*q);
     }
#endif
     shr.ax[3]=0; shr.scl[3][0]=1.0; shr.scl[3][1]=0.0; shr.scl[3][2]=0.0; shr.sft[3]=dx;
     shr.ax[2]=2; shr.scl[2][2]=1.0; shr.scl[2][0]=0.0; shr.scl[2][1]=0.0; shr.sft[2]=dz;
     shr.ax[1]=1; shr.scl[1][1]=1.0; shr.scl[1][0]=0.0; shr.scl[1][2]=0.0; shr.sft[1]=dy;
     shr.ax[0]=0; shr.scl[0][0]=1.0; shr.scl[0][1]=0.0; shr.scl[0][2]=0.0; shr.sft[0]=0.0;
     shr.flip0 = shr.flip1 = -1 ;  /* no flips */
     return shr ;
   }

   /* compute all 6 possible factorizations (the brute force approach) */

   sh[0] = shear_arb( q , xyzdel , 0,1,2 ) ;
   sh[1] = shear_arb( q , xyzdel , 0,2,1 ) ;
   sh[2] = shear_arb( q , xyzdel , 1,0,2 ) ;
   sh[3] = shear_arb( q , xyzdel , 1,2,0 ) ;
   sh[4] = shear_arb( q , xyzdel , 2,0,1 ) ;
   sh[5] = shear_arb( q , xyzdel , 2,1,0 ) ;

   /* find the one with the smallest "norm" */

   jbest = 0 ; best = BIG_NORM ;
   for( ii=0 ; ii < 6 ; ii++ ){
     val = norm_3shear( sh[ii] ) ;
     if( val < best ){ best = val ; jbest = ii ; }
   }

   return sh[jbest] ;
}

/*--------------------------------------------------------------------------
   Compute a rotation matrix specified by 3 angles:
      Q = R3 R2 R1, where Ri is rotation about axis axi by angle thi.
----------------------------------------------------------------------------*/

THD_dmat33 rot_to_matrix( int ax1 , double th1 ,
                          int ax2 , double th2 , int ax3 , double th3  )
{
   THD_dmat33 q , p ;

   LOAD_ROT_DMAT( q , th1 , ax1 ) ;
   LOAD_ROT_DMAT( p , th2 , ax2 ) ; q = DMAT_MUL( p , q ) ;
   LOAD_ROT_DMAT( p , th3 , ax3 ) ; q = DMAT_MUL( p , q ) ;

   return q ;
}

/*--------------------------------------------------------------------------
   Compute a set of shears to carry out a rotation + shift.
   The rotation is specified as the composition of elementary
   rotations about 3 axes.
----------------------------------------------------------------------------*/

MCW_3shear rot_to_shear( int ax1 , double th1 ,
                         int ax2 , double th2 ,
                         int ax3 , double th3 ,
                         int dcode , double dx , double dy , double dz ,
                         double xdel , double ydel , double zdel )
{
   int flip0=-1 , flip1=-1 ;  /* no flips */
   THD_dmat33 q , p ;
   THD_dfvec3 d , c ;

   static MCW_3shear shr ;
   static int old_ax1=-99 , old_ax2=-99 , old_ax3=-99 , old_dcode=-99 ;
   static double old_th1 , old_th2 , old_th3 , old_dx , old_dy , old_dz ,
                 old_xdel, old_ydel, old_zdel ;

   /* check if this is a duplicate call */

   ZZME(p) ;

   if( ax1 == old_ax1  &&  ax2 == old_ax2  &&  ax3 == old_ax3  && dcode == old_dcode &&
       th1 == old_th1  &&  th2 == old_th2  &&  th3 == old_th3  &&
       dx  == old_dx   &&  dy  == old_dy   &&  dz  == old_dz   &&
      xdel == old_xdel && ydel == old_ydel && zdel == old_zdel    ) return shr ;

   old_ax1   = ax1 ; old_ax2  = ax2 ; old_ax3  = ax3 ;
   old_th1   = th1 ; old_th2  = th2 ; old_th3  = th3 ;
   old_dx    = dx  ; old_dy   = dy  ; old_dz   = dz  ;
   old_xdel  = xdel; old_ydel = ydel; old_zdel = zdel; old_dcode = dcode;

   /* compute rotation matrix */

   q = rot_to_matrix( ax1,th1 , ax2,th2 , ax3,th3 ) ;

   /* if trace too small, maybe we should flip a couple axes */

   if( DMAT_TRACE(q) < 1.0 ){
      double top=q.mat[0][0] ; int itop=0 , i1,i2 ;
      if( top < q.mat[1][1] ){ top = q.mat[1][1] ; itop = 1 ; }
      if( top < q.mat[2][2] ){ top = q.mat[2][2] ; itop = 2 ; }
      switch(itop){
         default:
         case 0: i1 = 1 ; i2 = 2 ; LOAD_DIAG_DMAT(p, 1,-1,-1) ; break ;
         case 1: i1 = 0 ; i2 = 2 ; LOAD_DIAG_DMAT(p,-1, 1,-1) ; break ;
         case 2: i1 = 0 ; i2 = 1 ; LOAD_DIAG_DMAT(p,-1,-1, 1) ; break ;
      }
      if( q.mat[i1][i1] + q.mat[i2][i2] < -0.02 ){
         q = DMAT_MUL( q , p ) ;
         flip0 = i1 ; flip1 = i2 ;  /* yes flips */
      }
   }

   LOAD_DFVEC3( d , dx,dy,dz ) ;

   switch( dcode ){
      default: break ;   /* nothing */

      case DELTA_BEFORE:
         d = DMATVEC(q,d) ; break ;

      case DELTA_FIXED:
         c = DMATVEC(q,d) ; d = SUB_DFVEC3(d,c) ; break ;
   }

   /* scale q and d by the voxel dimensions */

   d.xyz[0] = d.xyz[0] / xdel ;  /* d <- inv[D] d, where D = diag[xdel,ydel,zdel] */
   d.xyz[1] = d.xyz[1] / ydel ;
   d.xyz[2] = d.xyz[2] / zdel ;

   q.mat[0][1] *= (ydel/xdel) ;  /* q <- inv[D] q D */
   q.mat[0][2] *= (zdel/xdel) ;
   q.mat[1][0] *= (xdel/ydel) ;  /* q still has det[q]=1 after this */
   q.mat[1][2] *= (zdel/ydel) ;  /* and the diagonal is unaffected */
   q.mat[2][0] *= (xdel/zdel) ;
   q.mat[2][1] *= (ydel/zdel) ;

   /* compute the "best" shear for this q matrix */

   shr = shear_best( &q , &d ) ;

   /* if cannot compute shear, try perturbing the matrix a little */

#undef PER0
#undef PER1
#undef PER2
#define PER0 1.09e-6
#define PER1 1.22e-6
#define PER2 1.37e-6

   if( ! ISVALID_3SHEAR(shr) ){
      THD_dmat33 pt ;
      p  = rot_to_matrix( 0,PER0 , 1,PER1 , 2,PER2 ) ;
      q  = DMAT_MUL( q , p ) ;
      pt = TRANSPOSE_DMAT( p ) ;
      q  = DMAT_MUL( pt , q ) ;

      shr = shear_best( &q , &d ) ;
      if( ! ISVALID_3SHEAR(shr) ) return shr ;  /* give up */
   }

   shr.flip0 = flip0 ; shr.flip1 = flip1 ;

#if 0
   if( MRILIB_verbose ) DUMP_3SHEAR("rot_to_shear",shr) ;
#endif

   return shr ;
}

/*--------------------------------------------------------------------------
   Compute a set of shears to carry out a rotation + shift
   (note the shift is always DELTA_AFTER in this code).
----------------------------------------------------------------------------*/

MCW_3shear rot_to_shear_matvec( THD_dmat33 rmat , THD_dfvec3 tvec ,
                                double xdel , double ydel , double zdel )
{
   int flip0=-1 , flip1=-1 ;  /* no flips */
   THD_dmat33 q , p ;
   THD_dfvec3 d , c ;
   MCW_3shear shr ;

#if 0
   /* compute rotation matrix */

   q = rmat ;
#else
   /* 13 Feb 2001: make sure it is orthogonal;
                   even slightly off produces bad shears */

   p = DMAT_svdrot_newer( rmat ) ;
   q = TRANSPOSE_DMAT( p ) ;
#if 0
   DUMP_MAT33("rmat",rmat) ;
   DUMP_MAT33("q   ",q   ) ;
#endif
#endif

   /* if trace too small, maybe we should flip a couple axes */

   if( DMAT_TRACE(q) < 1.0 ){
      double top=q.mat[0][0] ; int itop=0 , i1,i2 ;
      if( top < q.mat[1][1] ){ top = q.mat[1][1] ; itop = 1 ; }
      if( top < q.mat[2][2] ){ top = q.mat[2][2] ; itop = 2 ; }
      switch(itop){
         default:
         case 0: i1 = 1 ; i2 = 2 ; LOAD_DIAG_DMAT(p, 1,-1,-1) ; break ;
         case 1: i1 = 0 ; i2 = 2 ; LOAD_DIAG_DMAT(p,-1, 1,-1) ; break ;
         case 2: i1 = 0 ; i2 = 1 ; LOAD_DIAG_DMAT(p,-1,-1, 1) ; break ;
      }
      if( q.mat[i1][i1] + q.mat[i2][i2] < -0.02 ){
         q = DMAT_MUL( q , p ) ;
         flip0 = i1 ; flip1 = i2 ;  /* yes flips */
      }
   }

   d = tvec ;

   /* scale q and d by the voxel dimensions */

   d.xyz[0] = d.xyz[0] / xdel ;  /* d <- inv[D] d, where D = diag[xdel,ydel,zdel] */
   d.xyz[1] = d.xyz[1] / ydel ;
   d.xyz[2] = d.xyz[2] / zdel ;

   q.mat[0][1] *= (ydel/xdel) ;  /* q <- inv[D] q D */
   q.mat[0][2] *= (zdel/xdel) ;
   q.mat[1][0] *= (xdel/ydel) ;  /* q still has det[q]=1 after this */
   q.mat[1][2] *= (zdel/ydel) ;
   q.mat[2][0] *= (xdel/zdel) ;
   q.mat[2][1] *= (ydel/zdel) ;

   /* compute the "best" shear for this q matrix */

   shr = shear_best( &q , &d ) ;

   /* if cannot compute shear, try perturbing the matrix a little */

   if( ! ISVALID_3SHEAR(shr) ){
      THD_dmat33 pt ;
      p  = rot_to_matrix( 0,PER0 , 1,PER1 , 2,PER2 ) ;
      q  = DMAT_MUL( q , p ) ;
      pt = TRANSPOSE_DMAT( p ) ;
      q  = DMAT_MUL( pt , q ) ;

      shr = shear_best( &q , &d ) ;
      if( ! ISVALID_3SHEAR(shr) ) return shr ;  /* give up */
   }

   shr.flip0 = flip0 ; shr.flip1 = flip1 ;

#if 0
   if( MRILIB_verbose ) DUMP_3SHEAR("rot_to_shear",shr) ;
#endif

   return shr ;
}

/*=========================================================================
  Routines to compute the rotation+translation to best align a set
  of 3D points to one another -- RWCox - 16 Jul 2000.
===========================================================================*/

/*---------------------------------------------------------------------
   Compute        T
           [inmat]  [inmat]
-----------------------------------------------------------------------*/

THD_dmat33 DMAT_xt_x( THD_dmat33 inmat )
{
   THD_dmat33 tt,mm ;
   tt = TRANSPOSE_DMAT(inmat) ;
   mm = DMAT_MUL(tt,inmat) ;
   return mm ;
}

/*--------------------------------------------------------------------*/
                                           /*                T  */
THD_dmat33 DMAT_x_xt( THD_dmat33 inmat )   /* [inmat] [inmat]   */
{                                          /* 09 Apr 2003 - RWC */
   THD_dmat33 tt,mm ;
   tt = TRANSPOSE_DMAT(inmat) ;
   mm = DMAT_MUL(inmat,tt) ;
   return mm ;
}

/*--------------------------------------------------------------------
   Compute the eigensolution of the symmetric matrix inmat; that is,
   orthogonal [X] and diagonal [D] such that

        [inmat] [X] = [X] [D]
----------------------------------------------------------------------*/

THD_dvecmat DMAT_symeig( THD_dmat33 inmat )
{
   THD_dvecmat out ;
   double a[9] , e[3] ;
   int ii,jj ;

   /* load matrix from inmat into simple array */

   for( jj=0 ; jj < 3 ; jj++ )
      for( ii=0 ; ii < 3 ; ii++ ) a[ii+3*jj] = inmat.mat[ii][jj] ;

   symeig_double( 3 , a , e ) ;     /* eigensolution of array */

   /* load eigenvectors and eigenvalues into output */

   for( jj=0 ; jj < 3 ; jj++ ){
      out.vv.xyz[jj] = e[jj] ;                 /* eigenvalues */
      for( ii=0 ; ii < 3 ; ii++ )
         out.mm.mat[ii][jj] = a[ii+3*jj] ;     /* eigenvectors */
   }

   return out ;
}

/*---------------------------------------------------------------------
   Compute the SVD of matrix inmat.
-----------------------------------------------------------------------*/

typedef struct { THD_dmat33 u,v ; THD_dfvec3 d ; } THD_udv33 ;

THD_udv33 DMAT_svd( THD_dmat33 inmat )
{
   THD_udv33 out ;
   double a[9] , e[3] , u[9] , v[9] ;
   int ii , jj ;

   /* load matrix from inmat into simple array */

   for( jj=0 ; jj < 3 ; jj++ )
     for( ii=0 ; ii < 3 ; ii++ ) a[ii+3*jj] = inmat.mat[ii][jj] ;

   svd_double( 3,3 , a , e,u,v ) ;

   /* load eigenvectors and eigenvalues into output */

   for( jj=0 ; jj < 3 ; jj++ ){
     out.d.xyz[jj] = e[jj] ;                /* eigenvalues */
     for( ii=0 ; ii < 3 ; ii++ ){
       out.u.mat[ii][jj] = u[ii+3*jj] ;     /* eigenvectors */
       out.v.mat[ii][jj] = v[ii+3*jj] ;     /* eigenvectors */
     }
   }

   return out ;
}

/*---------------------------------------------------------------------
   Compute                  pp
            [      T       ]
            [ inmat  inmat ]   for some power pp
-----------------------------------------------------------------------*/

THD_dmat33 DMAT_pow( THD_dmat33 inmat , double pp )
{
   THD_dmat33 out , mm , dd ;
   THD_dvecmat vm ;
   int ii ;

   /* special case */

   if( pp == 0.0 ){ LOAD_DIAG_DMAT(out,1,1,1) ; return out ; }

   mm = DMAT_xt_x( inmat ) ;
   vm = DMAT_symeig( mm ) ;  /* get eigensolution [X] and [D] */

   /* raise [D] to the pp power */

   for( ii=0 ; ii < 3 ; ii++ )
      vm.vv.xyz[ii] = (vm.vv.xyz[ii] <= 0.0) ? 0.0
                                             : pow(vm.vv.xyz[ii],pp) ;

   /*                  pp    T  */
   /* result is [X] [D]   [X]   */

   LOAD_DIAG_DMAT( dd , vm.vv.xyz[0],vm.vv.xyz[1],vm.vv.xyz[2] ) ;

   mm  = DMAT_MUL( vm.mm , dd ) ;
   dd  = TRANSPOSE_DMAT( vm.mm ) ;
   out = DMAT_MUL( mm , dd ) ;
   return out ;
}

/*---------------------------------------------------------------------
   Compute the rotation         T
                         [V] [U]
   from the matrix, where                     T
                         [inmat] = [U] [D] [V]
   is the SVD of the input matrix.  We actually calculate it as

                     -1/2
       [     T      ]           T
       [inmat  inmat]    [inmat]

   which is the same thing (do your linear algebra, dude).
-----------------------------------------------------------------------*/

THD_dmat33 DMAT_svdrot_old( THD_dmat33 inmat )
{
   THD_dmat33 sq , out , tt ;

   sq  = DMAT_pow( inmat , -0.5 ) ;
   tt  = TRANSPOSE_DMAT(inmat) ;
   out = DMAT_MUL( sq , tt ) ;
   return out ;
}

/*---------------------------------------------------------------------*/
/*  Alternative calculation to above, computing U and V directly,
    as eigenmatrices from symmetric eigenvalue problems.
    This avoids a problem that arises when inmat is singular.
-----------------------------------------------------------------------*/

THD_dmat33 DMAT_svdrot_new( THD_dmat33 inmat )
{
   THD_dmat33 mm , nn ;
   THD_dvecmat vm , um ;

   mm = DMAT_xt_x( inmat ) ;
   vm = DMAT_symeig( mm ) ;  /* vm.mm matrix is now V */

   mm = DMAT_x_xt( inmat ) ;
   um = DMAT_symeig( mm ) ;  /* um.mm matrix is now U */

   mm = TRANSPOSE_DMAT(um.mm) ;
   nn = DMAT_MUL( vm.mm , mm ) ;
   return nn ;
}

/*---------------------------------------------------------------------*/
/*  Yet another alternative calculation to above, computing U and V
    even more directly, from the SVD itself.  [22 Oct 2004]
-----------------------------------------------------------------------*/

THD_dmat33 DMAT_svdrot_newer( THD_dmat33 inmat )
{
   THD_udv33 udv ;
   THD_dmat33 vm , um , nn ;

   udv = DMAT_svd( inmat ) ;
   vm = udv.v ;
   um = TRANSPOSE_DMAT( udv.u );
   nn = DMAT_MUL( vm , um ) ;
   return nn ;
}

/*---------------------------------------------------------------------
  Compute proper orthgonal matrix R and vector V to make

     yy = [R] xx + V   (k=0..n-1)
       k        k

  true in the (weighted) least squares sense.  If ww == NULL, then
  weights of all 1 are used.  Method follows
    KS Arun, TS Huang, and SD Blostein, IEEE PAMI, 9:698-700, 1987
  and uses the routines above to compute the matrix [R].
-----------------------------------------------------------------------*/

THD_dvecmat DLSQ_rot_trans( int n, THD_dfvec3 *xx, THD_dfvec3 *yy, double *ww )
{
   THD_dvecmat out ;
   THD_dfvec3  cx,cy , tx,ty ;
   THD_dmat33  cov ;
   double *wt , wsum , dd ;
   int ii,jj,kk ;

   /*- check for bad inputs -*/

   ZZME(out) ;
   if( n < 3 || xx == NULL || yy == NULL ){ LOAD_ZERO_DMAT(out.mm); return out; }

   /*- make a fake weight array, if none supplied -*/

   if( ww == NULL ){
      wt = (double *) malloc(sizeof(double)*n) ;
      for( kk=0 ; kk < n ; kk++ ) wt[kk] = 1.0 ;
   } else {
      wt = ww ;
   }

   /*- compute centroids of each set of vectors -*/

   LOAD_DFVEC3(cx,0,0,0) ; LOAD_DFVEC3(cy,0,0,0) ; wsum = 0.0 ;
   for( kk=0 ; kk < n ; kk++ ){
      cx = SCLADD_DFVEC3(1,cx,wt[kk],xx[kk]) ;  /* weighted sums of vectors */
      cy = SCLADD_DFVEC3(1,cy,wt[kk],yy[kk]) ;
      wsum += wt[kk] ;                          /* sum of weights */
   }
   wsum = 1.0 / wsum ;
   cx.xyz[0] *= wsum ; cx.xyz[1] *= wsum ; cx.xyz[2] *= wsum ;  /* centroids */
   cy.xyz[0] *= wsum ; cy.xyz[1] *= wsum ; cy.xyz[2] *= wsum ;

   /*- compute covariance matrix -*/

   LOAD_DIAG_DMAT(cov,1.e-10,1.e-10,1.e-10) ;
   for( kk=0 ; kk < n ; kk++ ){
      tx = SUB_DFVEC3( xx[kk] , cx ) ;  /* remove centroids */
      ty = SUB_DFVEC3( yy[kk] , cy ) ;
      for( jj=0 ; jj < 3 ; jj++ )
         for( ii=0 ; ii < 3 ; ii++ )
            cov.mat[ii][jj] += wt[kk]*tx.xyz[ii]*ty.xyz[jj] ;
   }
   dd = ( fabs(cov.mat[0][0]) + fabs(cov.mat[1][1]) + fabs(cov.mat[2][2]) ) / 3.0 ;
#if 0
fprintf(stderr,"dd=%g  diag=%g %g %g\n",dd,cov.mat[0][0],cov.mat[1][1],cov.mat[2][2] ) ;
#endif
   dd = dd / 1.e9 ;
#if 0
fprintf(stderr,"dd=%g\n",dd) ;
#endif
   if( cov.mat[0][0] < dd ) cov.mat[0][0] = dd ;
   if( cov.mat[1][1] < dd ) cov.mat[1][1] = dd ;
   if( cov.mat[2][2] < dd ) cov.mat[2][2] = dd ;

#if 0
DUMP_DMAT33( "cov" , cov ) ;
#endif

   out.mm = DMAT_svdrot_newer( cov ) ;  /* compute rotation matrix [R] */

#if 0
DUMP_DMAT33( "out.mm" , out.mm ) ;
#endif

   tx = DMATVEC( out.mm , cx ) ;     /* compute translation vector V */
   out.vv = SUB_DFVEC3( cy , tx ) ;

   if( wt != ww ) free(wt) ;               /* toss the trash, if any */

   return out ;
}

/*--------------------------------------------------------------------------*/
/*! Compute an affine transformation to take one set of vectors into another.
    That is, find general matrix R and vector B so that

     yy = [R] xx + V   (k=0..n-1)
       k        k

    is true in the unweighted least squares sense.
----------------------------------------------------------------------------*/

THD_dvecmat DLSQ_affine( int n, THD_dfvec3 *xx, THD_dfvec3 *yy )
{
   THD_dvecmat out ;
   THD_dfvec3  cx,cy , tx,ty ;
   THD_dmat33  yxt , xtx , xtxinv ;
   int ii,jj,kk ;
   double wsum ;

   /*- check for bad inputs -*/

   ZZME(out) ;
   if( n < 3 || xx == NULL || yy == NULL ){ LOAD_ZERO_DMAT(out.mm); return out; }

   /*- compute centroids of each set of vectors -*/

   LOAD_DFVEC3(cx,0,0,0) ; LOAD_DFVEC3(cy,0,0,0) ; wsum = 0.0 ;
   for( kk=0 ; kk < n ; kk++ ){
     cx = ADD_DFVEC3(cx,xx[kk]) ;  /* sums of vectors */
     cy = ADD_DFVEC3(cy,yy[kk]) ;
   }
   wsum = 1.0 / n ;
   cx.xyz[0] *= wsum ; cx.xyz[1] *= wsum ; cx.xyz[2] *= wsum ;  /* centroids */
   cy.xyz[0] *= wsum ; cy.xyz[1] *= wsum ; cy.xyz[2] *= wsum ;

   /*- compute products of data matrices -*/

   LOAD_DIAG_DMAT(yxt,1.e-9,1.e-9,1.e-9) ;
   LOAD_DIAG_DMAT(xtx,1.e-9,1.e-9,1.e-9) ;
   for( kk=0 ; kk < n ; kk++ ){
     tx = SUB_DFVEC3( xx[kk] , cx ) ;  /* remove centroids */
     ty = SUB_DFVEC3( yy[kk] , cy ) ;
     for( jj=0 ; jj < 3 ; jj++ ){
       for( ii=0 ; ii < 3 ; ii++ ){
         yxt.mat[ii][jj] += ty.xyz[ii]*tx.xyz[jj] ;
         xtx.mat[ii][jj] += tx.xyz[ii]*tx.xyz[jj] ;
       }
     }
   }
   xtxinv = DMAT_INV( xtx ) ;
   out.mm = DMAT_MUL( yxt , xtxinv ) ;
   tx = DMATVEC( out.mm , cx ) ;
   out.vv = SUB_DFVEC3( cy , tx ) ;
   return out ;
}

/*--------------------------------------------------------------------------*/
/*! Compute a transformation to take one set of vectors into another.
    That is, find matrix R and vector B so that

     yy = [R] xx + V   (k=0..n-1)
       k        k

    is true in the unweighted least squares sense.  Here, we restrict R
    to be a scalar multiple of an orthgonal matrix.
----------------------------------------------------------------------------*/

THD_dvecmat DLSQ_rotscl( int n, THD_dfvec3 *xx, THD_dfvec3 *yy , int ndim )
{
   THD_dvecmat out ;
   THD_dfvec3  cx,cy , tx,ty ;
   THD_dmat33  yxt , xtx , xtxinv , aa,bb,cc ;
   int ii,jj,kk ;
   double wsum ;

   /*- check for bad inputs -*/

   ZZME(out) ;
   if( n < 3 || xx == NULL || yy == NULL ){ LOAD_ZERO_DMAT(out.mm); return out; }

   /*- compute centroids of each set of vectors -*/

   LOAD_DFVEC3(cx,0,0,0) ; LOAD_DFVEC3(cy,0,0,0) ; wsum = 0.0 ;
   for( kk=0 ; kk < n ; kk++ ){
     cx = ADD_DFVEC3(cx,xx[kk]) ;  /* sums of vectors */
     cy = ADD_DFVEC3(cy,yy[kk]) ;
   }
   wsum = 1.0 / n ;
   cx.xyz[0] *= wsum ; cx.xyz[1] *= wsum ; cx.xyz[2] *= wsum ;  /* centroids */
   cy.xyz[0] *= wsum ; cy.xyz[1] *= wsum ; cy.xyz[2] *= wsum ;

   /*- compute products of data matrices -*/

   LOAD_DIAG_DMAT(yxt,1.e-9,1.e-9,1.e-9) ;
   LOAD_DIAG_DMAT(xtx,1.e-9,1.e-9,1.e-9) ;
   for( kk=0 ; kk < n ; kk++ ){
     tx = SUB_DFVEC3( xx[kk] , cx ) ;  /* remove centroids */
     ty = SUB_DFVEC3( yy[kk] , cy ) ;
     for( jj=0 ; jj < 3 ; jj++ ){
       for( ii=0 ; ii < 3 ; ii++ ){
         yxt.mat[ii][jj] += ty.xyz[ii]*tx.xyz[jj] ;
         xtx.mat[ii][jj] += tx.xyz[ii]*tx.xyz[jj] ;
       }
     }
   }
   xtxinv = DMAT_INV( xtx ) ;
   aa = DMAT_MUL( yxt , xtxinv ) ;
   bb = DMAT_pow( aa , -0.5 ) ;
   cc = DMAT_MUL( aa , bb) ;
   wsum = DMAT_DET(aa); wsum = fabs(wsum);
   switch( ndim ){
     default:
     case 3: wsum = cbrt(wsum) ; break ;  /* 3D rotation */
     case 2: wsum = sqrt(wsum) ; break ;  /* 2D rotation */
   }
   out.mm = DMAT_SCALAR( cc , wsum ) ;

   tx = DMATVEC( out.mm , cx ) ;
   out.vv = SUB_DFVEC3( cy , tx ) ;
   return out ;
}
