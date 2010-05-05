#include "bilinear_warp3D.h"

/*==========================================================================*/
/*==========================================================================*/
/* The BLmat44 stuff below has been copied from the corresponding 'mat44'
   stuff in 3ddata.h, et cetera, with some names changes, to provide a
   self-contained file for dealing with 3D bilinear transformations.
*//*------------------------------------------------------------------------*/

typedef struct { float m[4][4] ; } BLmat44 ;  /* AKA mat44 in AFNI */

/* elementary rotation matrices:
   rotate about axis #ff, from axis #aa toward #bb,
   where ff, aa, and bb are a permutation of {0,1,2} */

#undef  LOAD_ROTGEN_BLMAT44
#define LOAD_ROTGEN_BLMAT44(AA,th,ff,aa,bb)                           \
 ( AA.m[aa][aa] = AA.m[bb][bb] = cosf((th)) ,                         \
   AA.m[aa][bb] = sinf((th)) ,                                        \
   AA.m[bb][aa] = -AA.m[aa][bb] ,                                     \
   AA.m[ff][ff] = 1.0f ,                                              \
   AA.m[aa][ff] = AA.m[bb][ff] = AA.m[ff][aa] = AA.m[ff][bb] = 0.0f , \
   AA.m[0][3]   = AA.m[1][3]   = AA.m[2][3]   =                       \
   AA.m[3][0]   = AA.m[3][1]   = AA.m[3][2]   = 0.0f , AA.m[3][3]=1.0f  )

/* rotations about x,y,z axes, respectively */

#undef  LOAD_ROTX_BLMAT44
#undef  LOAD_ROTY_BLMAT44
#undef  LOAD_ROTZ_BLMAT44
#define LOAD_ROTX_BLMAT44(A,th) LOAD_ROTGEN_BLMAT44(A,th,0,1,2)
#define LOAD_ROTY_BLMAT44(A,th) LOAD_ROTGEN_BLMAT44(A,th,1,2,0)
#define LOAD_ROTZ_BLMAT44(A,th) LOAD_ROTGEN_BLMAT44(A,th,2,0,1)

/* rotation about axis #i, for i=0,1,2 (x,y,z) */

#undef  LOAD_ROT_BLMAT44
#define LOAD_ROT_BLMAT44(A,th,i)                    \
  do{ switch( (i) ){                                \
        case 0: LOAD_ROTX_BLMAT44(A,th)   ; break ; \
        case 1: LOAD_ROTY_BLMAT44(A,th)   ; break ; \
        case 2: LOAD_ROTZ_BLMAT44(A,th)   ; break ; \
       default: LOAD_DIAG_BLMAT44(A,1,1,1); break ; \
      } } while(0)

/* load the top 3 rows of a mat44 matrix,
   and set the 4th row to [ 0 0 0 1], as required */

#undef  LOAD_BLMAT44
#define LOAD_BLMAT44(AA,a11,a12,a13,a14,a21,a22,a23,a24,a31,a32,a33,a34)  \
  ( AA.m[0][0]=a11 , AA.m[0][1]=a12 , AA.m[0][2]=a13 , AA.m[0][3]=a14 ,   \
    AA.m[1][0]=a21 , AA.m[1][1]=a22 , AA.m[1][2]=a23 , AA.m[1][3]=a24 ,   \
    AA.m[2][0]=a31 , AA.m[2][1]=a32 , AA.m[2][2]=a33 , AA.m[2][3]=a34 ,   \
    AA.m[3][0]=AA.m[3][1]=AA.m[3][2]=0.0f , AA.m[3][3]=1.0f            )

#undef  LOAD_DIAG_BLMAT44
#define LOAD_DIAG_BLMAT44(AA,a,b,c)                                       \
  LOAD_BLMAT44( AA , (a),0,0,0 , 0,(b),0,0 , 0,0,(c),0 )

#undef  LOAD_BLMAT44_VEC
#define LOAD_BLMAT44_VEC(AA,x,y,z) ( AA.m[0][3]=(x), AA.m[1][3]=(y), AA.m[2][3]=(z) )

#undef  PI
#undef  D2R
#define PI  3.1415927f
#define D2R (PI/180.0f)

/*--------------------------------------------------------------------------*/
/* The stuff below is stolen directly from mri_genalign.c et cetera.
   Only the serial numbers have been filed off to protect the innocent.
*//*------------------------------------------------------------------------*/

static BLmat44 BL_mat44_mul( BLmat44 A , BLmat44 B )
{
   BLmat44 C ; int i,j ;

   for( i=0 ; i < 3 ; i++ )
    for( j=0 ; j < 4 ; j++ )
     C.m[i][j] =  A.m[i][0] * B.m[0][j] + A.m[i][1] * B.m[1][j]
                + A.m[i][2] * B.m[2][j] + A.m[i][3] * B.m[3][j] ;

   C.m[3][0] = C.m[3][1] = C.m[3][2] = 0.0f ; C.m[3][3] = 1.0f ;
   return C ;
}

static BLmat44 BL_rot_matrix( int ax1, double th1,
                              int ax2, double th2, int ax3, double th3  )
{
   BLmat44 q , p ;

   LOAD_ROT_BLMAT44( q , th1 , ax1 ) ;
   LOAD_ROT_BLMAT44( p , th2 , ax2 ) ; q = BL_mat44_mul( p , q ) ;
   LOAD_ROT_BLMAT44( p , th3 , ax3 ) ; q = BL_mat44_mul( p , q ) ;

   return q ;
}

/*---------------------------------------------------------------------------*/
/* parvec[0..2] = shifts  [3..5] = angles  [6..8] = scales  [9..11] = shears */

static BLmat44 BL_affine44( float *parvec )
{
   BLmat44 ss,dd,uu,aa , gam ;
   float   a,b,c ;

   /* uu = rotation matrix */

   a = D2R*parvec[3] ;
   b = D2R*parvec[4] ;
   c = D2R*parvec[5] ;
   if( a != 0.0f || b != 0.0f || c != 0.0f )
     uu = BL_rot_matrix( 2,a , 0,b , 1,c ) ;
   else
     LOAD_DIAG_BLMAT44( uu , 1.0f,1.0f,1.0f ) ;

   /* dd = scaling matrix */

   a = parvec[6] ; if( a <= 0.10f || a >= 10.0f ) a = 1.0f ;
   b = parvec[7] ; if( b <= 0.10f || b >= 10.0f ) b = 1.0f ;
   c = parvec[8] ; if( c <= 0.10f || c >= 10.0f ) c = 1.0f ;
   LOAD_DIAG_BLMAT44( dd , a,b,c ) ;

   /* ss = shear matrix */

   a = parvec[ 9] ; if( fabsf(a) > 0.3333f ) a = 0.0f ;
   b = parvec[10] ; if( fabsf(b) > 0.3333f ) b = 0.0f ;
   c = parvec[11] ; if( fabsf(c) > 0.3333f ) c = 0.0f ;
   LOAD_BLMAT44( ss , 1.0f , 0.0f , 0.0f , 0.0f ,
                      a    , 1.0f , 0.0f , 0.0f ,
                      b    , c    , 1.0f , 0.0f  ) ;

   /* multiply [shear] [scale] [rotation] */

   aa = BL_mat44_mul(ss,dd) ; gam = BL_mat44_mul(aa,uu) ;

   /* toss the shifts into the mix */

   a = parvec[0] ; b = parvec[1] ; c = parvec[2] ;
   LOAD_BLMAT44_VEC( gam , a,b,c ) ;

   return gam ;
}
/*==========================================================================*/
/*==========================================================================*/

/*--------------------------------------------------------------------------*/

static INLINE float BL_matdet( BLmat mmm )
{
   float val ;

   val =   mmm.m[0][0] * mmm.m[1][1] * mmm.m[2][2]
         - mmm.m[0][0] * mmm.m[1][2] * mmm.m[2][1]
         - mmm.m[1][0] * mmm.m[0][1] * mmm.m[2][2]
         + mmm.m[1][0] * mmm.m[0][2] * mmm.m[2][1]
         + mmm.m[2][0] * mmm.m[0][1] * mmm.m[1][2]
         - mmm.m[2][0] * mmm.m[0][2] * mmm.m[1][1] ;
   return val ;
}

/*--------------------------------------------------------------------------*/

static INLINE BLmat BL_matinv( BLmat mmm )
{
   BLmat ttt ; float det ;

   det = BL_matdet(mmm) ;
   if( det == 0.0f ){ memset(&ttt,0,sizeof(BLmat)) ; return ttt ; }
   det = 1.0f / det ;

   ttt.m[1][1] = ( mmm.m[0][0]*mmm.m[2][2] - mmm.m[0][2]*mmm.m[2][0]) * det ;
   ttt.m[2][2] = ( mmm.m[0][0]*mmm.m[1][1] - mmm.m[0][1]*mmm.m[1][0]) * det ;
   ttt.m[2][0] = ( mmm.m[1][0]*mmm.m[2][1] - mmm.m[1][1]*mmm.m[2][0]) * det ;
   ttt.m[1][2] = (-mmm.m[0][0]*mmm.m[1][2] + mmm.m[0][2]*mmm.m[1][0]) * det ;
   ttt.m[0][1] = (-mmm.m[0][1]*mmm.m[2][2] + mmm.m[0][2]*mmm.m[2][1]) * det ;
   ttt.m[0][0] = ( mmm.m[1][1]*mmm.m[2][2] - mmm.m[1][2]*mmm.m[2][1]) * det ;
   ttt.m[2][1] = (-mmm.m[0][0]*mmm.m[2][1] + mmm.m[0][1]*mmm.m[2][0]) * det ;
   ttt.m[1][0] = (-mmm.m[1][0]*mmm.m[2][2] + mmm.m[1][2]*mmm.m[2][0]) * det ;
   ttt.m[0][2] = ( mmm.m[0][1]*mmm.m[1][2] - mmm.m[0][2]*mmm.m[1][1]) * det ;

   return ttt ;
}

/*--------------------------------------------------------------------------*/

static INLINE BLvec BL_matvec( BLmat a , BLvec x )
{
  BLvec y ;

  y.v[0] = a.m[0][0] * x.v[0] + a.m[0][1] * x.v[1] + a.m[0][2] * x.v[2] ;
  y.v[1] = a.m[1][0] * x.v[0] + a.m[1][1] * x.v[1] + a.m[1][2] * x.v[2] ;
  y.v[2] = a.m[2][0] * x.v[0] + a.m[2][1] * x.v[1] + a.m[2][2] * x.v[2] ;
  return y ;
}

/*--------------------------------------------------------------------------*/

#undef  RDC            /* row #i DOT column #j */
#define RDC(am,bm,i,j) (  (am)[i][0] * (bm)[0][j]   \
                        + (am)[i][1] * (bm)[1][j]   \
                        + (am)[i][2] * (bm)[2][j] )

static INLINE BLmat BL_matmat( BLmat a , BLmat b )
{
   BLmat c ;

   c.m[0][0] = RDC(a.m,b.m,0,0) ;
   c.m[0][1] = RDC(a.m,b.m,0,1) ; c.m[0][2] = RDC(a.m,b.m,0,2) ;

   c.m[1][0] = RDC(a.m,b.m,1,0) ;
   c.m[1][1] = RDC(a.m,b.m,1,1) ; c.m[1][2] = RDC(a.m,b.m,1,2) ;

   c.m[2][0] = RDC(a.m,b.m,2,0) ;
   c.m[2][1] = RDC(a.m,b.m,2,1) ; c.m[2][2] = RDC(a.m,b.m,2,2) ;

   return c ;
}

/*--------------------------------------------------------------------------*/
/* Invert a standard-form bilinear warp. */

BL_standard_warp BL_invert_warp( BL_standard_warp wi )
{
   BL_standard_warp wo ;
   BLvec vv ;
   int i,j,k ;

   wo.a = BL_matinv( wi.a ) ;         /* inv[a] */

   wo.b = BL_matvec( wo.a , wi.b ) ;  /* -inv[a].b */
   wo.b.v[0] = -wo.b.v[0] ; wo.b.v[1] = -wo.b.v[1] ; wo.b.v[2] = -wo.b.v[2] ;

   for( i=0 ; i < 3 ; i++ )           /* -inv[a].c' */
     for( k=0 ; k < 3 ; k++ )
       for( j=0 ; j < 3 ; j++ )
         wo.c.t[j][i][k] = - wo.a.m[j][0]*wi.c.t[0][k][i]   /* Note k and i */
                           - wo.a.m[j][1]*wi.c.t[1][k][i]   /* are flipped  */
                           - wo.a.m[j][2]*wi.c.t[2][k][i] ; /* on L and RHS */

   return wo ;
}

/*--------------------------------------------------------------------------*/
/* Convert a general-form bilinear warp to a standard-form warp. */

BL_standard_warp BL_standardize_warp( BL_general_warp wi )
{
   BL_standard_warp wo ;
   BLmat einv , ft ;
   int i,j,k ;

   einv = BL_matinv( wi.e ) ;

   wo.b = BL_matvec( einv , wi.h ) ;
   wo.b.v[0] += wi.t.v[0] ; wo.b.v[1] += wi.t.v[1] ; wo.b.v[2] += wi.t.v[2] ;

   for( j=0 ; j < 3 ; j++ )
     for( k=0 ; k < 3 ; k++ )
       ft.m[j][k] =  wi.f.t[j][0][k]*wi.t.v[0]
                   + wi.f.t[j][1][k]*wi.t.v[1]
                   + wi.f.t[j][2][k]*wi.t.v[2] + wi.g.m[j][k] ;

   wo.a = BL_matmat( einv , ft ) ;

   for( i=0 ; i < 3 ; i++ )
     for( k=0 ; k < 3 ; k++ )
       for( j=0 ; j < 3 ; j++ )
         wo.c.t[j][i][k] =   einv.m[j][0]*wi.f.t[0][i][k]
                           + einv.m[j][1]*wi.f.t[1][i][k]
                           + einv.m[j][2]*wi.f.t[2][i][k] ;

   return wo ;
}

/*--------------------------------------------------------------------------*/
/* Create affine warp from shifts, angles, scales, shears. */

BL_affine_warp BL_affine_from_12_params( float *par )
{
   BL_affine_warp wa ; BLmat44 ma ; int i,j ;

   ma = BL_affine44( par ) ;  /* the actual calculations */

   /* copy results into the output warp */

   for( i=0 ; i < 3 ; i++ )
     for( j=0 ; j < 3 ; j++ ) wa.a.m[i][j] = ma.m[i][j] ;

   for( i=0 ; i < 3 ; i++ ) wa.b.v[i] = ma.m[i][3] ;

   return wa ;
}

/*--------------------------------------------------------------------------*/
/* Just copy 12 numbers into the affine warp. */

BL_affine_warp BL_affine_from_12_elements( float *par )
{
   BL_affine_warp wa ;

   wa.a.m[0][0] = par[0] ; wa.a.m[0][1] = par[1] ; wa.a.m[0][2] = par[ 2] ;
   wa.a.m[1][0] = par[4] ; wa.a.m[1][1] = par[5] ; wa.a.m[1][2] = par[ 6] ;
   wa.a.m[2][0] = par[8] ; wa.a.m[2][1] = par[9] ; wa.a.m[2][2] = par[10] ;

   wa.b.v[0] = par[3] ; wa.b.v[1] = par[7] ; wa.b.v[1] = par[11] ;

   return wa ;
}

/*--------------------------------------------------------------------------*/
/* Create a bilinear warp from parameters.
   Possible npar values:
    -12 ==> affine  [params = 12 matrix values]
     12 ==> affine  [params = shifts, angles, scales, shears]
     39 ==> standard bilinear
     43 ==> 3dAllineate / 3dWarpDrive bilinear (with cen vector and fac)
   God only knows what will happen to you if npar takes on any other value!
*//*------------------------------------------------------------------------*/

BL_standard_warp BL_warp_from_params( int npar , float *par )
{
   BL_standard_warp ws ; BL_affine_warp waf ;

   /* setup affine part from first 12 parameters */

   if( npar == -12 ){
     waf = BL_affine_from_12_elements( par ) ;
   } else {
     waf = BL_affine_from_12_params( par ) ;
   }

   if( npar < 39 ){                    /* affine only */

     ws.a = waf.a ; ws.b = waf.b ;
     memset( &(ws.c) , 0 , sizeof(BLten) ) ;

   } else if( npar < 43 ){             /* standard bilinear */

     ws.a = waf.a ; ws.b = waf.b ;

     ws.c.t[0][0][0] = par[12]; ws.c.t[0][0][1] = par[13]; ws.c.t[0][0][2] = par[14];
     ws.c.t[0][1][0] = par[15]; ws.c.t[0][1][1] = par[16]; ws.c.t[0][1][2] = par[17];
     ws.c.t[0][2][0] = par[18]; ws.c.t[0][2][1] = par[19]; ws.c.t[0][2][2] = par[20];
     ws.c.t[1][0][0] = par[21]; ws.c.t[1][0][1] = par[22]; ws.c.t[1][0][2] = par[23];
     ws.c.t[1][1][0] = par[24]; ws.c.t[1][1][1] = par[25]; ws.c.t[1][1][2] = par[26];
     ws.c.t[1][2][0] = par[27]; ws.c.t[1][2][1] = par[28]; ws.c.t[1][2][2] = par[29];
     ws.c.t[2][0][0] = par[30]; ws.c.t[2][0][1] = par[31]; ws.c.t[2][0][2] = par[32];
     ws.c.t[2][1][0] = par[33]; ws.c.t[2][1][1] = par[34]; ws.c.t[2][1][2] = par[35];
     ws.c.t[2][2][0] = par[36]; ws.c.t[2][2][1] = par[37]; ws.c.t[2][2][2] = par[38];

   } else {   /* make a general bilinear, then convert it to standard form */

     BL_general_warp wg ;
     float fac=par[42] , xcen=par[39] , ycen=par[40] , zcen=par[41] ;
     int i,j,k ;

     wg.g = waf.a ; wg.h = waf.b ;
     wg.t.v[0] = wg.t.v[1] = wg.t.v[2] = 0.0f ;

     wg.f.t[0][0][0] = par[12] * fac ;
     wg.f.t[0][0][1] = par[13] * fac ; wg.f.t[0][0][2] = par[14] * fac ;

     wg.f.t[0][1][0] = par[15] * fac ;
     wg.f.t[0][1][1] = par[16] * fac ; wg.f.t[0][1][2] = par[17] * fac ;

     wg.f.t[0][2][0] = par[18] * fac ;
     wg.f.t[0][2][1] = par[19] * fac ; wg.f.t[0][2][2] = par[20] * fac ;

     wg.f.t[1][0][0] = par[21] * fac ;
     wg.f.t[1][0][1] = par[22] * fac ; wg.f.t[1][0][2] = par[23] * fac ;

     wg.f.t[1][1][0] = par[24] * fac ;
     wg.f.t[1][1][1] = par[25] * fac ; wg.f.t[1][1][2] = par[26] * fac ;

     wg.f.t[1][2][0] = par[27] * fac ;
     wg.f.t[1][2][1] = par[28] * fac ; wg.f.t[1][2][2] = par[29] * fac ;

     wg.f.t[2][0][0] = par[30] * fac ;
     wg.f.t[2][0][1] = par[31] * fac ; wg.f.t[2][0][2] = par[32] * fac ;

     wg.f.t[2][1][0] = par[33] * fac ;
     wg.f.t[2][1][1] = par[34] * fac ; wg.f.t[2][1][2] = par[35] * fac ;

     wg.f.t[2][2][0] = par[36] * fac ;
     wg.f.t[2][2][1] = par[37] * fac ; wg.f.t[2][2][2] = par[38] * fac ;

     wg.e.m[0][0] = wg.e.m[1][1] = wg.e.m[2][2] = 1.0f ;
     wg.e.m[0][1] = wg.e.m[1][0] =
      wg.e.m[0][2] = wg.e.m[2][0] = wg.e.m[1][2] = wg.e.m[2][1] = 0.0f ;

     for( i=0 ; i < 3 ; i++ )
       for( j=0 ; j < 3 ; j++ )
         wg.e.m[i][j] -=   wg.f.t[i][j][0]*xcen
                         + wg.f.t[i][j][1]*ycen + wg.f.t[i][j][2]*zcen ;

     ws = BL_standardize_warp( wg ) ;
   }

   return ws ;
}

/*--------------------------------------------------------------------------*/
/* Return value depends on denominator tensor zero vs. nonzero situation:
     0 ==> tensor is all zero ==> warp is really affine
     1 ==> tensor is nonzero only for diagonal i==j elements
     2 ==> tensor is nonzero in off-diagonal elements
*//*------------------------------------------------------------------------*/

int BL_warp_tensor_status( BL_standard_warp wi )
{
   int i,j,k , zz , zero_tot=0 , zero_off=0 ;

   for( i=0 ; i < 3 ; i++ ){
     for( j=0 ; j < 3 ; j++ ){
       for( k=0 ; k < 3 ; k++ ){
         zz        = ( wi.c.t[i][j][k] == 0.0f ) ;
         zero_tot += zz ;
         zero_off += (zz && i != j) ;
       }
   }}

   if( zero_tot == 27 ) return 0 ;
   if( zero_off == 18 ) return 1 ;
                        return 2 ;
}

/*--------------------------------------------------------------------------*/

BL_affine_warp BL_extract_affine_warp( BL_standard_warp wi )
{
   BL_affine_warp wa ;
   wa.a = wi.a ; wa.b = wi.b ; return wa ;
}

/*--------------------------------------------------------------------------*/

void BL_print_standard_warp( char *name , BL_standard_warp ws )
{
   printf("++ Bilinear Warp: %s\n",(name==NULL)?" ":name) ;
   printf(" b vector = %10.4f %10.4f %10.4f\n",ws.b.v[0],ws.b.v[1],ws.b.v[2]) ;
   printf(" a matrix = %10.4f %10.4f %10.4f\n"
          "            %10.4f %10.4f %10.4f\n"
          "            %10.4f %10.4f %10.4f\n" ,
          ws.a.m[0][0] , ws.a.m[0][1] , ws.a.m[0][2] ,
          ws.a.m[1][0] , ws.a.m[1][1] , ws.a.m[1][2] ,
          ws.a.m[2][0] , ws.a.m[2][1] , ws.a.m[2][2]  ) ;
   printf(" c tensor = %10.4f %10.4f %10.4f  [00.]\n"
          "            %10.4f %10.4f %10.4f  [01.]\n"
          "            %10.4f %10.4f %10.4f  [02.]\n"
          "            %10.4f %10.4f %10.4f  [10.]\n"
          "            %10.4f %10.4f %10.4f  [11.]\n"
          "            %10.4f %10.4f %10.4f  [12.]\n"
          "            %10.4f %10.4f %10.4f  [20.]\n"
          "            %10.4f %10.4f %10.4f  [21.]\n"
          "            %10.4f %10.4f %10.4f  [22.]\n" ,
          ws.c.t[0][0][0] , ws.c.t[0][0][1] , ws.c.t[0][0][2] ,
          ws.c.t[0][1][0] , ws.c.t[0][1][1] , ws.c.t[0][1][2] ,
          ws.c.t[0][2][0] , ws.c.t[0][2][1] , ws.c.t[0][2][2] ,
          ws.c.t[1][0][0] , ws.c.t[1][0][1] , ws.c.t[1][0][2] ,
          ws.c.t[1][1][0] , ws.c.t[1][1][1] , ws.c.t[1][1][2] ,
          ws.c.t[1][2][0] , ws.c.t[1][2][1] , ws.c.t[1][2][2] ,
          ws.c.t[2][0][0] , ws.c.t[2][0][1] , ws.c.t[2][0][2] ,
          ws.c.t[2][1][0] , ws.c.t[2][1][1] , ws.c.t[2][1][2] ,
          ws.c.t[2][2][0] , ws.c.t[2][2][1] , ws.c.t[2][2][2]  ) ;
   return ;
}

/*--------------------------------------------------------------------------*/

BL_standard_warp BL_bilinear_x_affine( BL_standard_warp blin , BL_affine_warp afin )
{
   BL_general_warp wg ; BL_standard_warp ws ;
   int i,j,k ;

   wg.g = BL_matmat( blin.a , afin.a ) ;
   wg.h = BL_matvec( blin.a , afin.b ) ;
   wg.h.v[0] += blin.b.v[0] ; wg.h.v[1] += blin.b.v[1] ; wg.h.v[2] += blin.b.v[2] ;
   wg.t.v[0] = wg.t.v[1] = wg.t.v[2] = 0.0f ;

   wg.e.m[0][0] = wg.e.m[1][1] = wg.e.m[2][2] = 1.0f ;
   wg.e.m[0][1] = wg.e.m[1][0] =
   wg.e.m[0][2] = wg.e.m[2][0] = wg.e.m[1][2] = wg.e.m[2][1] = 0.0f ;

   for( i=0 ; i < 3 ; i++ )
     for( j=0 ; j < 3 ; j++ )
       wg.e.m[i][j] +=   blin.c.t[i][j][0]*afin.b.v[0]
                       + blin.c.t[i][j][1]*afin.b.v[1]
                       + blin.c.t[i][j][2]*afin.b.v[2] ;

   for( i=0 ; i < 3 ; i++ )
     for( j=0 ; j < 3 ; j++ )
       for( k=0 ; k < 3 ; k++ )
         wg.f.t[i][j][k] =   blin.c.t[i][j][0]*afin.a.m[0][k]
                           + blin.c.t[i][j][1]*afin.a.m[1][k]
                           + blin.c.t[i][j][2]*afin.a.m[2][k] ;

   ws = BL_standardize_warp( wg ) ;
   return ws ;
}

/*--------------------------------------------------------------------------*/

BL_standard_warp BL_affine_x_bilinear( BL_affine_warp afin , BL_standard_warp blin )
{
   BL_general_warp wg ; BL_standard_warp ws ; BLmat dinv ;
   int i,j,k ;

   wg.g = blin.a ; wg.h = blin.b ; wg.t = afin.b ;

   dinv = wg.e = BL_matinv( afin.a ) ;

   for( i=0 ; i < 3 ; i++ )
     for( j=0 ; j < 3 ; j++ )
       for( k=0 ; k < 3 ; k++ )
         wg.f.t[i][j][k] =   dinv.m[0][j]*blin.c.t[i][0][k]
                           + dinv.m[1][j]*blin.c.t[i][1][k]
                           + dinv.m[2][j]*blin.c.t[i][2][k] ;

   ws = BL_standardize_warp( wg ) ;
   return ws ;
}

/*--------------------------------------------------------------------------*/

void BL_apply_warp( BL_standard_warp wi ,
                    int npt ,
                    float *xi , float *yi , float *zi ,
                    float *xo , float *yo , float *zo  )
{
   int ii , tstat ;
   BLvec xx ;
   float aa , bb , cc ;

   if( npt <= 0 || xi == NULL || xo == NULL ) return ;

   tstat = BL_warp_tensor_status(wi) ;

   for( ii=0 ; ii < npt ; ii++ ){
     aa = xx.v[0] = xi[ii] ; bb = xx.v[1] = yi[ii] ; cc = xx.v[2] = zi[ii] ;
     xx = BL_matvec( wi.a , xx ) ;
     xx.v[0] += wi.b.v[0] ; xx.v[1] += wi.b.v[1] ; xx.v[2] += wi.b.v[2] ;

     if( tstat == 2 ){  /* general matrix */
       BLmat dd , ee ;
       dd.m[0][0] = 1.0f + wi.c.t[0][0][0]*aa + wi.c.t[0][0][1]*bb + wi.c.t[0][0][2]*cc ;
       dd.m[0][1] =        wi.c.t[0][1][0]*aa + wi.c.t[0][1][1]*bb + wi.c.t[0][1][2]*cc ;
       dd.m[0][2] =        wi.c.t[0][2][0]*aa + wi.c.t[0][2][1]*bb + wi.c.t[0][2][2]*cc ;
       dd.m[1][0] =        wi.c.t[1][0][0]*aa + wi.c.t[1][0][1]*bb + wi.c.t[1][0][2]*cc ;
       dd.m[1][1] = 1.0f + wi.c.t[1][1][0]*aa + wi.c.t[1][1][1]*bb + wi.c.t[1][1][2]*cc ;
       dd.m[1][2] =        wi.c.t[1][2][0]*aa + wi.c.t[1][2][1]*bb + wi.c.t[1][2][2]*cc ;
       dd.m[2][0] =        wi.c.t[2][0][0]*aa + wi.c.t[2][0][1]*bb + wi.c.t[2][0][2]*cc ;
       dd.m[2][1] =        wi.c.t[2][1][0]*aa + wi.c.t[2][1][1]*bb + wi.c.t[2][1][2]*cc ;
       dd.m[2][2] = 1.0f + wi.c.t[2][2][0]*aa + wi.c.t[2][2][1]*bb + wi.c.t[2][2][2]*cc ;

       ee = BL_matinv(dd) ; xx = BL_matvec(ee,xx) ;

     } else if( tstat == 1 ){  /* diagonal matrix */
       float pp,qq,rr ;
       pp = 1.0f + wi.c.t[0][0][0]*aa + wi.c.t[0][0][1]*bb + wi.c.t[0][0][2]*cc ;
       qq = 1.0f + wi.c.t[1][1][0]*aa + wi.c.t[1][1][1]*bb + wi.c.t[1][1][2]*cc ;
       rr = 1.0f + wi.c.t[2][2][0]*aa + wi.c.t[2][2][1]*bb + wi.c.t[2][2][2]*cc ;
       xx.v[0] /= pp ; xx.v[1] /= qq ; xx.v[2] /= rr ;
     }

     xo[ii] = xx.v[0] ; yo[ii] = xx.v[1] ; zo[ii] = xx.v[2] ;
   }

   return ;
}

/*--------------------------------------------------------------------------*/
#if 1
int main( int argc , char *argv[] )
{
   float par[43] ; int npar , ii ;
   BL_standard_warp wi , wo ;

   npar = argc-1 ; if( npar > 43 ) npar = 43 ;
   if( npar < 12 ) exit(1) ;

   for( ii=0 ; ii < npar ; ii++ ) par[ii] = (float)strtod(argv[ii+1],NULL) ;
   for(      ; ii < 43   ; ii++ ) par[ii] = 0.0f ;

   wi = BL_warp_from_params( npar , par ) ;
   BL_print_standard_warp( "input" , wi ) ;
   wo = BL_invert_warp( wi ) ;
   BL_print_standard_warp( "inverse" , wo ) ;
   wi = BL_invert_warp( wo ) ;
   BL_print_standard_warp( "inv[inv]" , wi ) ;
   exit(0) ;
}
#endif
