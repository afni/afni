#include "mrilib.h"

/*----------------------------------------------------------------------
   Inputs: (a,b,xc) for incomplete beta
   Outputs:
   Let Ipq = Int( x**(a-1)*(1-x)**(b-1)*ln(x)**p*ln(1-x)**q, x=0..xc ).
   Then
     bi7[0] = I00     = normalization factor
     bi7[1] = I10/I00 = <ln(x)>
     bi7[2] = I01/I00 = <ln(1-x)>
     bi7[3] = d(bi7[1])/da = (I20*I00-I10**2)/I00**2
     bi7[4] = d(bi7[1])/db = (I11*I00-I10*I01)/I00**2
     bi7[5] = d(bi7[2])/da = (I11*I00-I10*I01)/I00**2
     bi7[6] = d(bi7[2])/db = (I02*I00-I01**2)/I00**2
   The integrals are calculated by transforming to y=a*ln(xc/x), and
   then using Gauss-Laguerre quadrature:

   Int( x**(a-1)*(1-x)**(b-1) * f(x) , x=0..xc )

   transforms to

   xc**a
   ----- * Int( exp(-y)*(1-xc*exp(-y/a))**(b-1)*f(xc*exp(-y/a)), y=0..infty )
     a

   The return value of this function is -1 if an error occurred, and
   is 0 if all is good.
-------------------------------------------------------------------------*/

int bi7func( double a , double b , double xc , double * bi7 )
{
#define NL 20  /* must be between 2 and 20 - see cs_laguerre.c */

   static double *yy=NULL , *ww=NULL ;
   double xx , s00,s10,s01,s20,s11,s02 , ff , l0,l1 ;
   register int ii ;

   if( a  <= 0.0 || b  <= 0.0 ||
       xc <= 0.0 || xc >= 1.0 || bi7 == NULL ) return -1 ;

   if( yy == NULL ) get_laguerre_table( NL , &yy , &ww ) ;

   s00=s10=s01=s20=s11=s02 = 0.0 ;
   for( ii=NL-1 ; ii >= 0 ; ii-- ){
      xx = xc*exp(-yy[ii]/a) ;            /* x transformed from y */
      l0 = log(xx) ; l1 = log(1.0-xx) ;   /* logarithms for Ipq sums */
      ff = pow(1.0-xx,b-1.0) ;            /* (1-x)**(b-1) */
      s00 += ww[ii] * ff ;                /* spq = Ipq sum */
      s10 += ww[ii] * ff * l0 ;
      s20 += ww[ii] * ff * l0 * l0 ;
      s01 += ww[ii] * ff * l1 ;
      s02 += ww[ii] * ff * l1 * l1 ;
      s11 += ww[ii] * ff * l0 * l1 ;
   }

   if( s00 <= 0.0 ) return -1 ;

   bi7[0] = s00 * pow(xc,a) / a ;           /* normalizer */
   bi7[1] = s10/s00 ;                       /* R0 */
   bi7[2] = s01/s00 ;                       /* R1 */
   bi7[3] = (s20*s00-s10*s10)/(s00*s00) ;   /* dR0/da */
   bi7[4] = (s11*s00-s10*s01)/(s00*s00) ;   /* dR0/db */
   bi7[5] = bi7[4] ;                        /* dR1/da */
   bi7[6] = (s02*s00-s01*s01)/(s00*s00) ;   /* dR1/db */

   return 0 ;
}

/*-----------------------------------------------------------------------*/

#define LL   0.2
#define UL   10000.0

static double AL   = 0.21 ;
static double AU   = 9.9 ;
static double BL   = 5.9 ;
static double BU   = 999.9 ;
static int    NRAN = 6666 ;

void betarange( double al,double au , double bl , double bu , int nran )
{
   if( al > 0.0 ) AL = al ;
   if( au > AL  ) AU = au ;
   if( bl > 0.0 ) BL = bl ;
   if( bu > BL  ) BU = bu ;
   if( nran > 1 ) NRAN = nran ;
}

int betasolve( double e0, double e1, double xc, double * ap, double * bp )
{
   double bi7[7] , aa,bb , da,db , m11,m12,m21,m22 , r1,r2 , dd,ee ;
   int nite=0 , ii,jj ;

   if( ap == NULL || bp == NULL ||
       xc <= 0.0  || xc >= 1.0  || e0 >= 0.0 || e1 >= 0.0 ) return -1 ;

   dd = 1.e+20 ; aa = bb = 0.0 ;
   for( jj=0 ; jj < NRAN ; jj++ ){
      da = AL +(AU-AL) * drand48() ;
      db = BL +(BU-BL) * drand48() ;
      ii = bi7func( da , db , xc , bi7 ) ; if( ii ) continue ;
      r1 = bi7[1] - e0 ; r2 = bi7[2] - e1 ;
      ee = fabs(r1/e0) + fabs(r2/e1) ;
      if( ee < dd ){ aa=da ; bb=db ; dd=ee ; }
   }
   if( aa == 0.0 || bb == 0.0 ) return -1 ;
   fprintf(stderr,"%2d: aa=%15.10g  bb=%15.10g  ee=%g\n",nite,aa,bb,ee) ;

   do{
      ii = bi7func( aa , bb , xc , bi7 ) ;
      if( ii ) return -1 ;
      r1  = bi7[1] - e0 ;
      r2  = bi7[2] - e1 ; ee = fabs(r1/e0) + fabs(r2/e1) ;
      m11 = bi7[3] ; m12 = bi7[4] ; m21 = bi7[5] ; m22 = bi7[6] ;
      dd  = m11*m22 - m12*m21 ;
      if( dd == 0.0 ) return -1 ;
      da = ( m22*r1 - m12*r2 ) / dd ;
      db = (-m21*r1 + m11*r2 ) / dd ;
      nite++ ;
      aa -= da ; bb -=db ;
      if( aa < LL ) aa = LL ; else if( aa > UL ) aa = UL ;
      if( bb < LL ) bb = LL ; else if( bb > UL ) bb = UL ;
      fprintf(stderr,"%2d: aa=%15.10g  bb=%15.10g  ee=%g\n",nite,aa,bb,ee) ;

      if( aa == LL || bb == LL || aa == UL || bb == UL ) return -1 ;
   } while( fabs(da)+fabs(db) > 0.02 ) ;

   *ap = aa ; *bp = bb ; return 0 ;
}

/*-----------------------------------------------------------------------*/

void betafill( double a , double b , int n , float * x )
{
   int ii ;
   for( ii=0 ; ii < n ; ii++ )
      x[ii] = beta_p2t( drand48() , a , b ) ;
   return ;
}

/*-----------------------------------------------------------------------*/

int main( int argc , char * argv[] )
{
   double aa=0.5 , bb=31.0 , xc , e0,e1 , at,bt ;
   int narg=1 , npt,n,ii ;
   float * bv ;

   srand48((long)time(NULL)) ;

   if( argc < 5 ){printf("Usage: betafit a b xc n\n"); exit(0); }

   at = strtod(argv[narg++],NULL) ;
   bt = strtod(argv[narg++],NULL) ;
   xc = strtod(argv[narg++],NULL) ;
   npt = (int) strtod(argv[narg++],NULL) ;
   bv = (float *)malloc(sizeof(float)*npt) ;
   betafill( at,bt , npt,bv ) ;
   e0=e1 = 0.0 ;
   for( ii=n=0 ; ii < npt ; ii++ ){
      if( bv[ii] < xc ){
         e0 += log(bv[ii]) ; e1 += log(1.0-bv[ii]) ; n++ ;
      }
   }
   e0 /= n ; e1 /= n ;
   fprintf(stderr,"%d points below cutoff; e0=%g  e1=%g\n",n,e0,e1) ;

   betarange( 0.2*at , 5.0*at , 0.2*bt , 5.0*bt , 66666 ) ;

   betasolve( e0,e1,xc , &aa,&bb ); exit(0) ;
}
