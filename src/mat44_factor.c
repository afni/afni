#include "mrilib.h"

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
