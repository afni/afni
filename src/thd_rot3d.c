
/*****************************************************************************
  This software is copyrighted and owned by the Medical College of Wisconsin.
  See the file README.Copyright for details.
******************************************************************************/

/*===========================================================================
  Routines to rotate and shift a 3D volume using a 4 way shear decomposition,
  coupled with an FFT-based shifting of the rows -- RWCox [October 1998].
=============================================================================*/

#define FLOAT_TYPE double
#include "vecmat.h"
#undef FLOAT_TYPE

#include <stdio.h>
#include <stdlib.h>
#include "mrilib.h"

#define DB fprintf(stderr,"in %s at line %d\n",__FILE__,__LINE__)

/*------------------------------------------------------------------------
   Struct to contain a set of 4 3-shears, used to represent an arbitrary
   transformation; for i=0..3:
      3-shear #i is along axis # ax[i] (0=x, 1=y, 2=z),
      with scaling parameter scl[i][j] for direction j (j=0,1,2),
      and shift parameter sft[i] (of course in the ax[i] direction).
   In addition, a preliminary flipping about two axes may be present.
   These axes are denoted by flip0 and flip1, if flip0 >= 0.
--------------------------------------------------------------------------*/

typedef struct {
   int    ax[4] , flip0,flip1;
   double scl[4][3] , sft[4] ;
} MCW_3shear ;

#define DUMP_3SHEAR(str,sss)                                                      \
  printf("shear %s: flip0=%d flip1=%d\n"                                          \
         " #0: ax=%d scl=%13.6g %13.6g %13.6g sft=%13.6g\n"                       \
         " #1: ax=%d scl=%13.6g %13.6g %13.6g sft=%13.6g\n"                       \
         " #2: ax=%d scl=%13.6g %13.6g %13.6g sft=%13.6g\n"                       \
         " #3: ax=%d scl=%13.6g %13.6g %13.6g sft=%13.6g\n" ,                     \
   str , (sss).flip0 , (sss).flip1 ,                                              \
   (sss).ax[0], (sss).scl[0][0], (sss).scl[0][1], (sss).scl[0][2], (sss).sft[0],  \
   (sss).ax[1], (sss).scl[1][0], (sss).scl[1][1], (sss).scl[1][2], (sss).sft[1],  \
   (sss).ax[2], (sss).scl[2][0], (sss).scl[2][1], (sss).scl[2][2], (sss).sft[2],  \
   (sss).ax[3], (sss).scl[3][0], (sss).scl[3][1], (sss).scl[3][2], (sss).sft[3]  )

#define ISVALID_3SHEAR(sss)    ((sss).ax[0] >= 0)
#define INVALIDATE_3SHEAR(sss) ((sss).ax[0] = -1)

/*----------------------------------------------------------------------
   "Norm" of a set of shears [maximum stretch factor]
------------------------------------------------------------------------*/

#define BIG_NORM 1.e+38

static double norm_3shear( MCW_3shear sh )
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

static THD_mat33 make_shear_matrix( int ax , double scl[3] )
{
   THD_mat33 m ;

   switch( ax ){
      case 0:  LOAD_SHEARX_MAT( m , scl[0],scl[1],scl[2] ) ; break ;
      case 1:  LOAD_SHEARY_MAT( m , scl[1],scl[0],scl[2] ) ; break ;
      case 2:  LOAD_SHEARZ_MAT( m , scl[2],scl[0],scl[1] ) ; break ;
      default: LOAD_ZERO_MAT( m )                          ; break ;
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

static MCW_3shear permute_3shear( MCW_3shear shin , int ox1, int ox2, int ox3 )
{
   MCW_3shear shout ;
   int ii , ain,aout , pi[3] ;

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

static THD_mat33 permute_mat33( THD_mat33 q , int ox1, int ox2, int ox3 )
{
   THD_mat33 m ;
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

static THD_fvec3 permute_fvec3( THD_fvec3 q , int ox1, int ox2, int ox3 )
{
   THD_fvec3 m ;
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

static MCW_3shear shear_xzyx( THD_mat33 *q , THD_fvec3 *xyzdel )
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

   INVALIDATE_3SHEAR(shr) ;

   /* load inputs into local variables */

   UNLOAD_MAT( *q , q11,q12,q13,q21,q22,q23,q31,q32,q33 ) ;
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
           if( t34 > 0.0 ) t34 =   pow(  t34 , 0.333333333333333 ) ;
      else if( t34 < 0.0 ) t34 = - pow( -t34 , 0.333333333333333 ) ;
      else                 t34 = 0.0 ;

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

static MCW_3shear shear_arb( THD_mat33 *q , THD_fvec3 *xyzdel , int ox1,int ox2,int ox3 )
{
   THD_mat33 qq ;
   THD_fvec3 xx ;
   MCW_3shear sh_xzyx , shout ;

   /* permute the input matrix and vector to the desired order */

   qq = permute_mat33( *q      , ox1,ox2,ox3 ) ;
   xx = permute_fvec3( *xyzdel , ox1,ox2,ox3 ) ;

   /* compute the Sx Sz Sy Sx factorization */

   sh_xzyx = shear_xzyx( &qq , &xx ) ;
   if( ! ISVALID_3SHEAR(sh_xzyx) ) return sh_xzyx ;

   /* permute the shear factorization back */

   shout = permute_3shear( sh_xzyx , ox1,ox2,ox3 ) ;

   return shout ;
}

/*-----------------------------------------------------------------------------------
   Find the "best" shear decomposition (smallest stretching factors)
-------------------------------------------------------------------------------------*/

static MCW_3shear shear_best( THD_mat33 * q , THD_fvec3 * xyzdel )
{
   MCW_3shear sh[6] ;
   int ii , jbest ;
   double val , best ;

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

static THD_mat33 rot_to_matrix( int ax1 , double th1 ,
                                int ax2 , double th2 , int ax3 , double th3  )
{
   THD_mat33 q , p ;

   LOAD_ROT_MAT( q , th1 , ax1 ) ;
   LOAD_ROT_MAT( p , th2 , ax2 ) ; q = MAT_MUL( p , q ) ;
   LOAD_ROT_MAT( p , th3 , ax3 ) ; q = MAT_MUL( p , q ) ;

   return q ;
}

/*--------------------------------------------------------------------------
   Compute a set of shears to carry out a rotation + shift.
   The rotation is specified as the composition of elementary
   rotations about 3 axes.
----------------------------------------------------------------------------*/

static MCW_3shear rot_to_shear( int ax1 , double th1 ,
                                int ax2 , double th2 ,
                                int ax3 , double th3 ,
                                int dcode , double dx , double dy , double dz ,
                                double xdel , double ydel , double zdel )
{
   int flip0=-1 , flip1=-1 ;  /* no flips */
   THD_mat33 q , p ;
   THD_fvec3 d , c ;

   static MCW_3shear shr ;
   static int old_ax1=-99 , old_ax2=-99 , old_ax3=-99 , old_dcode=-99 ;
   static double old_th1 , old_th2 , old_th3 , old_dx , old_dy , old_dz ,
                 old_xdel, old_ydel, old_zdel ;

   /* check if this is a duplicate call */

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

   if( MAT_TRACE(q) < 1.0 ){
      double top=q.mat[0][0] ; int itop=0 , i1,i2 ;
      if( top < q.mat[1][1] ){ top = q.mat[1][1] ; itop = 1 ; }
      if( top < q.mat[2][2] ){ top = q.mat[2][2] ; itop = 2 ; }
      switch(itop){
         case 0: i1 = 1 ; i2 = 2 ; LOAD_DIAG_MAT(p, 1,-1,-1) ; break ;
         case 1: i1 = 0 ; i2 = 2 ; LOAD_DIAG_MAT(p,-1, 1,-1) ; break ;
         case 2: i1 = 0 ; i2 = 1 ; LOAD_DIAG_MAT(p,-1,-1, 1) ; break ;
      }
      if( q.mat[i1][i1] + q.mat[i2][i2] < -0.02 ){
         q = MAT_MUL( q , p ) ;
         flip0 = i1 ; flip1 = i2 ;  /* yes flips */
      }
   }

   LOAD_FVEC3( d , dx,dy,dz ) ;

   switch( dcode ){
      default: break ;   /* nothing */

      case DELTA_BEFORE:
         d = MATVEC(q,d) ; break ;

      case DELTA_FIXED:
         c = MATVEC(q,d) ; d = SUB_FVEC3(d,c) ; break ;
   }

#if 0
fprintf(stderr,"dcode=%d  dx=%g  dy=%g  dz=%g\n",
        dcode , d.xyz[0] , d.xyz[1] , d.xyz[2] ) ;
#endif

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
      p = rot_to_matrix( 0,1.0e-7 , 1,0.9e-7 , 2,1.1e-7 ) ;
      q = MAT_MUL( q , p ) ;

      shr = shear_best( &q , &d ) ;
      if( ! ISVALID_3SHEAR(shr) ) return shr ;  /* give up */
   }

   shr.flip0 = flip0 ; shr.flip1 = flip1 ;
   return shr ;
}

/********** 28 Oct 1999: the shifting routines that were here   **********
 **********              have been removed to file thd_shift2.c **********/

/*---------------------------------------------------------------------------
   Set the interpolation method for shifting:
   input is one of MRI_NN, MRI_LINEAR, MRI_CUBIC, or MRI_FOURIER.
-----------------------------------------------------------------------------*/

typedef void (*shift_func)(int,int,float,float *,float,float *) ;
static  shift_func shifter      = fft_shift2 ;
static  int        shift_method = MRI_FOURIER ;

void THD_rota_method( int mode )
{
   shift_method = mode ;
   switch( mode ){
      case MRI_NN:      shifter = nn_shift2    ; break ;
      case MRI_TSSHIFT: shifter = ts_shift2    ; break ;  /* Dec 1999 */

      case MRI_LINEAR:  shifter = lin_shift2   ; break ;
      case MRI_FOURIER: shifter = fft_shift2   ; break ;
      default:
      case MRI_CUBIC:   shifter = cub_shift2   ; break ;
      case MRI_QUINTIC: shifter = quint_shift2 ; break ;  /* Nov 1998 */
      case MRI_HEPTIC:  shifter = hept_shift2  ; break ;  /* Nov 1998 */
   }
   return ;
}

/*---------------------------------------------------------------------------
   Flip a 3D array about the (x,y) axes:
    i <--> nx-1-i    j <--> ny-1-j
-----------------------------------------------------------------------------*/

#define VV(i,j,k) v[(i)+(j)*nx+(k)*nxy]
#define SX(i)     (nx1-(i))
#define SY(j)     (ny1-(j))
#define SZ(k)     (nz1-(k))

static void flip_xy( int nx , int ny , int nz , float * v )
{
   int ii,jj,kk ;
   int nx1=nx-1,nx2=nx/2, ny1=ny-1,ny2=ny/2, nz1=nz-1,nz2=nz/2, nxy=nx*ny ;
   float * r1 ;

   r1 = (float *) malloc(sizeof(float)*nx) ;  /* save 1 row */

   for( kk=0 ; kk < nz ; kk++ ){              /* for each slice */
      for( jj=0 ; jj < ny2 ; jj++ ){          /* first 1/2 of rows */

         /* swap rows jj and ny1-jj, flipping them in ii as well */

         for( ii=0 ; ii < nx ; ii++ ) r1[ii]           = VV(SX(ii),SY(jj),kk) ;
         for( ii=0 ; ii < nx ; ii++ ) VV(ii,SY(jj),kk) = VV(SX(ii),jj    ,kk) ;
         for( ii=0 ; ii < nx ; ii++ ) VV(ii,jj    ,kk) = r1[ii] ;
      }
      if( ny%2 == 1 ){                                             /* central row? */
         for( ii=0 ; ii < nx ; ii++ ) r1[ii]       = VV(SX(ii),jj,kk) ; /* flip it */
         for( ii=0 ; ii < nx ; ii++ ) VV(ii,jj,kk) = r1[ii] ;           /* restore */
      }
   }

   free(r1) ; return ;
}

/*---------------------------------------------------------------------------
   Flip a 3D array about the (y,z) axes:
     j <--> ny-1-j   k <--> nz-1-k
-----------------------------------------------------------------------------*/

static void flip_yz( int nx , int ny , int nz , float * v )
{
   int ii,jj,kk ;
   int nx1=nx-1,nx2=nx/2, ny1=ny-1,ny2=ny/2, nz1=nz-1,nz2=nz/2, nxy=nx*ny ;
   float * r1 ;

   r1 = (float *) malloc(sizeof(float)*ny) ;

   for( ii=0 ; ii < nx ; ii++ ){
      for( kk=0 ; kk < nz2 ; kk++ ){
         for( jj=0 ; jj < ny ; jj++ ) r1[jj]           = VV(ii,SY(jj),SZ(kk)) ;
         for( jj=0 ; jj < ny ; jj++ ) VV(ii,jj,SZ(kk)) = VV(ii,SY(jj),kk    ) ;
         for( jj=0 ; jj < ny ; jj++ ) VV(ii,jj,kk    ) = r1[jj] ;
      }
      if( nz%2 == 1 ){
         for( jj=0 ; jj < ny ; jj++ ) r1[jj]       = VV(ii,SY(jj),kk) ;
         for( jj=0 ; jj < ny ; jj++ ) VV(ii,jj,kk) = r1[jj] ;
      }
   }

   free(r1) ; return ;
}

/*---------------------------------------------------------------------------
   Flip a 3D array about the (x,z) axes:
     i <--> nx-1-i   k <--> nz-1-k
-----------------------------------------------------------------------------*/

static void flip_xz( int nx , int ny , int nz , float * v )
{
   int ii,jj,kk ;
   int nx1=nx-1,nx2=nx/2, ny1=ny-1,ny2=ny/2, nz1=nz-1,nz2=nz/2, nxy=nx*ny ;
   float * r1 ;

   r1 = (float *) malloc(sizeof(float)*nx) ;

   for( jj=0 ; jj < ny ; jj++ ){
      for( kk=0 ; kk < nz2 ; kk++ ){
         for( ii=0 ; ii < nx ; ii++ ) r1[ii]           = VV(SX(ii),jj,SZ(kk)) ;
         for( ii=0 ; ii < nx ; ii++ ) VV(ii,jj,SZ(kk)) = VV(SX(ii),jj,kk    ) ;
         for( ii=0 ; ii < nx ; ii++ ) VV(ii,jj,kk    ) = r1[ii] ;
      }
      if( nz%2 == 1 ){
         for( ii=0 ; ii < nx ; ii++ ) r1[ii]       = VV(SX(ii),jj,kk) ;
         for( ii=0 ; ii < nx ; ii++ ) VV(ii,jj,kk) = r1[ii] ;
      }
   }

   free(r1) ; return ;
}

/*---------------------------------------------------------------------------
   Apply an x-axis shear to a 3D array: x -> x + a*y + b*z + s
   (dilation factor "f" assumed to be 1.0)
-----------------------------------------------------------------------------*/

static void apply_xshear( float a , float b , float s ,
                          int nx , int ny , int nz , float * v )
{
   float * fj0 , * fj1 ;
   int   nx1=nx-1    , ny1=ny-1    , nz1=nz-1    , nxy=nx*ny ;
   float nx2=0.5*nx1 , ny2=0.5*ny1 , nz2=0.5*nz1 ;
   int ii,jj,kk , nup,nst ;
   float a0 , a1 , st ;

   /* don't do anything if shift is less than 0.001 pixel */

   st = fabs(a)*ny2 + fabs(b)*nz2 + fabs(s) ; if( st < 1.e-3 ) return ;

   if( shift_method == MRI_FOURIER ){
      nup = 8 ; nst = 0.95*nx + 0.5*st ; if( nst < nx ) nst = nx ;
#if 0
      while( nup < nst ){ nup *= 2 ; }  /* FFT length */
#else
      nup = csfft_nextup_one35(nst) ;
#endif
   }

   for( kk=0 ; kk < nz ; kk++ ){
      for( jj=0 ; jj < ny ; jj+=2 ){
         fj0 = v + (jj*nx + kk*nxy) ;
         fj1 = (jj < ny1) ? (fj0 + nx) : NULL ;   /* allow for odd ny */
         a0  = a*(jj-ny2) + b*(kk-nz2) + s ;
         a1  = a0 + a ;
         shifter( nx , nup , a0 , fj0 , a1 , fj1 ) ;
      }
   }

   return ;
}

/*---------------------------------------------------------------------------
   Apply a y-axis shear to a 3D array: y -> y + a*x + b*z + s
-----------------------------------------------------------------------------*/

static void apply_yshear( float a , float b , float s ,
                          int nx , int ny , int nz , float * v )
{
   float * fj0 , * fj1 ;
   int   nx1=nx-1    , ny1=ny-1    , nz1=nz-1    , nxy=nx*ny ;
   float nx2=0.5*nx1 , ny2=0.5*ny1 , nz2=0.5*nz1 ;
   int ii,jj,kk , nup,nst ;
   float a0 , a1 , st ;

   /* don't do anything if shift is less than 0.001 pixel */

   st = fabs(a)*nx2 + fabs(b)*nz2 + fabs(s) ; if( st < 1.e-3 ) return ;

   if( shift_method == MRI_FOURIER ){
      nup = 8 ; nst = 0.95*ny + 0.5*st ; if( nst < ny ) nst = ny ;
#if 0
      while( nup < nst ){ nup *= 2 ; }  /* FFT length */
#else
      nup = csfft_nextup_one35(nst) ;
#endif
   }

   fj0 = (float *) malloc( sizeof(float) * 2*ny ) ; fj1 = fj0 + ny ;

   for( kk=0 ; kk < nz ; kk++ ){
      for( ii=0 ; ii < nx1 ; ii+=2 ){
         for( jj=0; jj < ny; jj++ ){ fj0[jj] = VV(ii,jj,kk) ; fj1[jj] = VV(ii+1,jj,kk) ; }
         a0 = a*(ii-nx2) + b*(kk-nz2) + s ;
         a1 = a0 + a ;
         shifter( ny , nup , a0 , fj0 , a1 , fj1 ) ;
         for( jj=0; jj < ny; jj++ ){ VV(ii,jj,kk) = fj0[jj] ; VV(ii+1,jj,kk) = fj1[jj] ; }
      }

      if( ii == nx1 ){                                       /* allow for odd nx */
         for( jj=0; jj < ny; jj++ ) fj0[jj] = VV(ii,jj,kk) ;
         a0 = a*(ii-nx2) + b*(kk-nz2) + s ;
         shifter( ny , nup , a0 , fj0 , a1 , NULL ) ;
         for( jj=0; jj < ny; jj++ ) VV(ii,jj,kk) = fj0[jj] ;
      }
   }

   free(fj0) ; return ;
}

/*---------------------------------------------------------------------------
   Apply a z-axis shear to a 3D array: z -> z + a*x + b*y + s
-----------------------------------------------------------------------------*/

static void apply_zshear( float a , float b , float s ,
                          int nx , int ny , int nz , float * v )
{
   float * fj0 , * fj1 ;
   int   nx1=nx-1    , ny1=ny-1    , nz1=nz-1    , nxy=nx*ny ;
   float nx2=0.5*nx1 , ny2=0.5*ny1 , nz2=0.5*nz1 ;
   int ii,jj,kk , nup,nst ;
   float a0 , a1 , st ;

   /* don't do anything if shift is less than 0.001 pixel */

   st = fabs(a)*nx2 + fabs(b)*ny2 + fabs(s) ; if( st < 1.e-3 ) return ;

   if( shift_method == MRI_FOURIER ){
      nup = 8 ; nst = 0.95*nz + 0.5*st ; if( nst < nz ) nst = nz ;
#if 0
      while( nup < nst ){ nup *= 2 ; }  /* FFT length */
#else
      nup = csfft_nextup_one35(nst) ;
#endif
   }

   fj0 = (float *) malloc( sizeof(float) * 2*nz ) ; fj1 = fj0 + nz ;

   for( jj=0 ; jj < ny ; jj++ ){
      for( ii=0 ; ii < nx1 ; ii+=2 ){
         for( kk=0; kk < nz; kk++ ){ fj0[kk] = VV(ii,jj,kk) ; fj1[kk] = VV(ii+1,jj,kk) ; }
         a0 = a*(ii-nx2) + b*(jj-ny2) + s ;
         a1 = a0 + a ;
         shifter( nz , nup , a0 , fj0 , a1 , fj1 ) ;
         for( kk=0; kk < nz; kk++ ){ VV(ii,jj,kk) = fj0[kk] ; VV(ii+1,jj,kk) = fj1[kk] ; }
      }

      if( ii == nx1 ){                                       /* allow for odd nx */
         for( kk=0; kk < nz; kk++ ) fj0[kk] = VV(ii,jj,kk) ;
         a0 = a*(ii-nx2) + b*(jj-ny2) + s ;
         shifter( nz , nup , a0 , fj0 , a1 , NULL ) ;
         for( kk=0; kk < nz; kk++ ) VV(ii,jj,kk) = fj0[kk] ;
      }
   }

   free(fj0) ; return ;
}

/*---------------------------------------------------------------------------
   Apply a set of shears to a 3D array of floats.
   Note that we assume that the dilation factors ("f") are all 1.
-----------------------------------------------------------------------------*/

static void apply_3shear( MCW_3shear shr ,
                          int   nx   , int   ny   , int   nz   , float * vol )
{
   int qq ;
   float a , b , s ;

   if( ! ISVALID_3SHEAR(shr) ) return ;

   /* carry out a preliminary 180 flippo ? */

   if( shr.flip0 >= 0 ){
      switch( shr.flip0 + shr.flip1 ){
         case 1: flip_xy( nx,ny,nz,vol ) ; break ;
         case 2: flip_xz( nx,ny,nz,vol ) ; break ;
         case 3: flip_yz( nx,ny,nz,vol ) ; break ;
        default:                           return ;  /* should not occur */
      }
   }

   /* apply each shear */

   for( qq=0 ; qq < 4 ; qq++ ){
      switch( shr.ax[qq] ){
         case 0:
            a = shr.scl[qq][1] ;
            b = shr.scl[qq][2] ;
            s = shr.sft[qq]    ;
            apply_xshear( a,b,s , nx,ny,nz , vol ) ;
         break ;

         case 1:
            a = shr.scl[qq][0] ;
            b = shr.scl[qq][2] ;
            s = shr.sft[qq]    ;
            apply_yshear( a,b,s , nx,ny,nz , vol ) ;
         break ;

         case 2:
            a = shr.scl[qq][0] ;
            b = shr.scl[qq][1] ;
            s = shr.sft[qq]    ;
            apply_zshear( a,b,s , nx,ny,nz , vol ) ;
         break ;
      }
   }

   return ;
}

/*---------------------------------------------------------------------------
  Rotate and translate a 3D volume.
-----------------------------------------------------------------------------*/

#undef CLIPIT

void THD_rota_vol( int   nx   , int   ny   , int   nz   ,
                   float xdel , float ydel , float zdel , float * vol ,
                   int ax1,float th1, int ax2,float th2, int ax3,float th3,
                   int dcode , float dx , float dy , float dz )
{
   MCW_3shear shr ;

#ifdef CLIPIT
   register float bot , top ;
   register int   nxyz=nx*ny*nz , ii ;
#endif

   if( nx < 2 || ny < 2 || nz < 2 || vol == NULL ) return ;

   if( xdel == 0.0 ) xdel = 1.0 ;
   if( ydel == 0.0 ) ydel = 1.0 ;
   if( zdel == 0.0 ) zdel = 1.0 ;

   if( th1 == 0.0 && th2 == 0.0 && th3 == 0.0 ){  /* nudge rotation */
      th1 = 1.e-6 ; th2 = 1.1e-6 ; th3 = 0.9e-6 ;
   }

#if 0
fprintf(stderr,"THD_rota_vol:\n") ;
fprintf(stderr,"  th1=%g  th2=%g  th3=%g\n",th1,th2,th3) ;
fprintf(stderr,"  dx=%g  dy=%g  dz=%g\n",dx,dy,dz) ;
fprintf(stderr,"  xdel=%g  ydel=%g  zdel=%g\n",xdel,ydel,zdel) ;
#endif

   shr = rot_to_shear( ax1,-th1 , ax2,-th2 , ax3,-th3 ,
                       dcode,dx,dy,dz , xdel,ydel,zdel ) ;

   if( ! ISVALID_3SHEAR(shr) ){
      fprintf(stderr,"*** THD_rota_vol: can't compute shear transformation!\n") ;
      return ;
   }

#if 0
   DUMP_3SHEAR("Computed shear",shr) ;
#endif

#ifdef CLIPIT
   bot = top = vol[0] ;
   for( ii=1 ; ii < nxyz ; ii++ ){
           if( vol[ii] < bot ) bot = vol[ii] ;
      else if( vol[ii] > top ) top = vol[ii] ;
   }
   if( bot >= top ) return ;
#endif

   /************************************/

   apply_3shear( shr , nx,ny,nz , vol ) ;

   /************************************/

#ifdef CLIPIT
   for( ii=0 ; ii < nxyz ; ii++ ){
           if( vol[ii] < bot ) vol[ii] = bot ;
      else if( vol[ii] > top ) vol[ii] = top ;
   }
#endif

   return ;
}

/*-------------------------------------------------------------------------*/

MRI_IMAGE * THD_rota3D( MRI_IMAGE * im ,
                        int ax1,float th1, int ax2,float th2, int ax3,float th3,
                        int dcode , float dx , float dy , float dz )
{
   MRI_IMAGE * jm ;
   float * jvol ;

   if( ! MRI_IS_3D(im) ){
      fprintf(stderr,"\n*** THD_rota3D: non-3D image input!\n") ;
      return NULL ;
   }

   jm = mri_new_vol( im->nx , im->ny , im->nz , MRI_float ) ;
   MRI_COPY_AUX(jm,im) ;
   jvol = MRI_FLOAT_PTR(jm) ;

   EDIT_coerce_type( im->nvox ,
                     im->kind , mri_data_pointer(im) , MRI_float , jvol ) ;

   THD_rota_vol(      im->nx ,       im->ny ,       im->nz  ,
                 fabs(im->dx) , fabs(im->dy) , fabs(im->dz) , jvol ,
                 ax1,th1 , ax2,th2 , ax3,th3 , dcode , dx,dy,dz ) ;

   return jm ;
}
