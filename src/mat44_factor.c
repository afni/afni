#include "mrilib.h"

/*-----------------------------------------------------------------------*/

typedef struct { mat33 A , B , C ; } mat33_triple ;

/*-----------------------------------------------------------------------*/

#undef  TRANSPOSE_MAT33
#define TRANSPOSE_MAT33(BB,AA)                          \
  LOAD_MAT33(BB, AA.m[0][0] , AA.m[1][0] , AA.m[2][0] , \
                 AA.m[0][1] , AA.m[1][1] , AA.m[2][1] , \
                 AA.m[0][2] , AA.m[1][2] , AA.m[2][2]  )

/*-----------------------------------------------------------------------*/

static mat33 mat_mattran( mat33 A )
{
   mat33 At , B ;
   TRANSPOSE_MAT33( At , A ) ;
   B = nifti_mat33_mul( A , At ) ;
   return B ;
}

/*-----------------------------------------------------------------------*/

static mat33 mattran_mat( mat33 A )
{
   mat33 At , B ;
   TRANSPOSE_MAT33( At , A ) ;
   B = nifti_mat33_mul( At , A ) ;
   return B ;
}

/*-----------------------------------------------------------------------*/

static mat33 choleski( mat33 A )
{
   float a11 , a21 , a22 , a31 , a32 , a33 ;
   float b11 , b21 , b22 , b31 , b32 , b33 ;
   mat33 B ; int bad=1 ;

   a11 = A.m[0][0] ;
   a21 = A.m[1][0] ; a22 = A.m[1][1] ;
   a31 = A.m[2][0] ; a32 = A.m[2][1] ; a33 = A.m[2][2] ;

   if( a11 > 0.0 ){
     b11 = sqrtf(a11) ;
     b21 = a21 / b11 ;
     b22 = a22 - b21*b21 ;
     if( b22 > 0.0 ){
       b22 = sqrtf(b22) ;
       b31 = a31 / b11 ;
       b32 = (a32 - b31*b21) / b22 ;
       b33 = a33 - b31*b31 - b32*b32 ;
       if( b33 > 0.0 ){
         b33 = sqrtf(b33) ; bad = 0 ;
       }
     }
   }
   if( bad ) b11 = b22 = b33 = -1.0 ;

   LOAD_MAT33( B , b11 , 0.0 , 0.0 ,
                   b21 , b22 , 0.0 ,
                   b31 , b32 , b33  ) ;
   return B ;
}

/*-----------------------------------------------------------------------*/

static mat33_triple lt_to_DS( mat33 L )
{
   mat33_triple mmm ;
   float d1,d2,d3 , a,b,c ;

   d1 = L.m[0][0] ; d2 = L.m[1][1] ; d3 = L.m[2][2] ;
   a  = L.m[1][0] / d2 ;
   b  = L.m[2][0] / d3 ;
   c  = L.m[2][1] / d3 ;
   LOAD_MAT33( mmm.A , d1 , 0.0, 0.0,
                       0.0, d2 , 0.0,
                       0.0, 0.0, d3  ) ;
   LOAD_MAT33( mmm.B , 1.0, 0.0, 0.0,
                        a , 1.0, 0.0,
                        b ,  c , 1.0 ) ;
   return mmm ;
}

/*-----------------------------------------------------------------------*/

static mat33_triple lt_to_SD( mat33 L )
{
   mat33_triple mmm ;
   float d1,d2,d3 , a,b,c ;

   d1 = L.m[0][0] ; d2 = L.m[1][1] ; d3 = L.m[2][2] ;
   a  = L.m[1][0] / d1 ;
   b  = L.m[2][0] / d1 ;
   c  = L.m[2][1] / d2 ;
   LOAD_MAT33( mmm.A , d1 , 0.0, 0.0,
                       0.0, d2 , 0.0,
                       0.0, 0.0, d3  ) ;
   LOAD_MAT33( mmm.B , 1.0, 0.0, 0.0,
                        a , 1.0, 0.0,
                        b ,  c , 1.0 ) ;
   return mmm ;
}

/*-----------------------------------------------------------------------*/

static mat33_triple mat33_to_SDU( mat33 M )
{
   mat33_triple mmm ;
   mat33 MMt , LL ;

   MMt = mat_mattran( M ) ;
   LL  = choleski( MMt ) ;
   if( LL.m[0][0] <= 0.0 ){ mmm.A.m[0][0] = -1.0; return mmm; }
   mmm = lt_to_SD( LL ) ;
   MMt = nifti_mat33_inverse( LL ) ;
   LL  = nifti_mat33_mul( MMt , M ) ;
   mmm.C = nifti_mat33_polar( LL ) ;
   return mmm ;
}

/*-----------------------------------------------------------------------*/

static mat33_triple mat33_to_DSU( mat33 M )
{
   mat33_triple mmm ;
   mat33 MMt , LL ;

   MMt = mat_mattran( M ) ;
   LL  = choleski( MMt ) ;
   if( LL.m[0][0] <= 0.0 ){ mmm.A.m[0][0] = -1.0; return mmm; }
   mmm = lt_to_DS( LL ) ;
   MMt = nifti_mat33_inverse( LL ) ;
   LL  = nifti_mat33_mul( MMt , M ) ;
   mmm.C = nifti_mat33_polar( LL ) ;
   return mmm ;
}

/*-----------------------------------------------------------------------*/

static mat33_triple mat33_to_USD( mat33 M )
{
   mat33_triple mmm ;
   return mmm ;
}
