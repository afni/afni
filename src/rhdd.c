#include "mrilib.h"

/*---------------------------------------------------------------------------*/
/* If needed, swap so that first argument ends up as the smaller of the pair */

#undef  ISWAP
#define ISWAP(a,b) if( (b) > (a) ) ( tt=(a), (a)=(b), (b)=tt )

/*---------------------------------------------------------------------------*/
/*! C0 basis function with RHDD(2) support (piecewise linear).
*//*-------------------------------------------------------------------------*/

static INLINE float rhddc0( float x, float y, float z )
{
   register float xx, yy, zz, tt ;

   xx = fabsf(x) ; yy = fabsf(y) ; zz = fabsf(z) ;
   ISWAP(zz,yy) ;  /* sort so xx >= yy >= zz */
   ISWAP(zz,xx) ; ISWAP(yy,xx) ;

   tt = xx+yy ;
   if( tt >= 2.0f ) return ( 0.0f ) ;            /* outside RHDD(2) */
                    return ( 0.5f*(2.0f-tt) ) ;  /* linear inside  */
}

/*---------------------------------------------------------------------------*/

#undef  ALPHA
#undef  BETA
#undef  GAMMA
#define ALPHA  0.0026041667f  /* 1/384 */
#define BETA   0.0052083333f  /* 1/192 */
#define GAMMA  0.0104166667f  /* 1/96  */

/*---------------------------------------------------------------------------*/
/*! C2 basis function with RHDD(4) support (piecewise quintic).
*//*-------------------------------------------------------------------------*/

static INLINE float rhddc2( float x, float y, float z )
{
   register float xx, yy, zz, tt, xz2,yz2,xy2 ;

   xx = fabsf(x) ; yy = fabsf(y) ; zz = fabsf(z) ;
   ISWAP(zz,yy) ;  /* sort so that xx >= yy >= zz */
   ISWAP(zz,xx) ; ISWAP(yy,xx) ;

   tt = xx+yy-4.0f ;
   if( tt >= 0.0f ) return 0.0f ;  /* outside RHDD(4) */

   xz2 = xx+zz-2.0f ; yz2 = yy+zz-2.0f ; xy2 = tt+2.0f ;

#undef  PA
#define PA ALPHA * tt*tt*tt                                           \
           * ( -3.0f*xx*yy - 5.0f*zz*zz + 2.0f*(xx+yy) + 20.0f*zz     \
              + xx*xx + yy*yy - 24.0f )

#undef  PB1
#define PB1 BETA * xz2*xz2*xz2                                        \
            * (  xx*xx - 9.0f*xx - 3.0f*xx*zz + 10.0f*yy - 5.0f*yy*yy \
               + 14.0f + 11.0f*zz + zz*zz )

#undef  PB2
#define PB2 BETA * yz2*yz2*yz2                                        \
            * (  46.0f - 30.0f*xx - zz - yy + 3.0f*zz*yy + 5.0f*xx*xx \
               - yy*yy - zz*zz )

   /* Region 1 */

   if( xy2 <= 0.0f ){
     return (  PA + PB1 + PB2
             - GAMMA * xy2*xy2*xy2
              * ( xx*xx + xx - 3.0f*xx*yy - 5.0f*zz*zz + yy*yy + yy - 6.0f ) ) ;
   }

   /* Region 2 */

   if( xz2 <= 0.0f ){
     return ( PA + PB1 + PB2 ) ;
   }

   /* Region 3 */

   if( yz2 <= 0.0f ){

     if( xx-zz >= 2.0f ){  /* Region 3A */

       return ( ALPHA * tt*tt*tt
               * ( -xx*xx + 8.0f*xx + 3.0f*xx*yy - yy*yy + 5.0f*zz*zz
                   -16.0f - 12.0f*yy ) ) ;

     } else {            /* Region 3B */

       return( PA + PB2 ) ;

     }

   }

   /* Region 4 */

   return ( PA ) ;
}

int main( int argc , char *argv[] )
{
   float x,y,z,val ;

   if( argc < 4 ){ printf("need x y z\n"); exit(0); }
   x = (float)strtod(argv[1],NULL) ;
   y = (float)strtod(argv[2],NULL) ;
   z = (float)strtod(argv[3],NULL) ;
   val = rhddc2(x,y,z) ;
   printf("rhddc2 = %f\n",val) ; exit(0) ;
}
