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
   The integrals are calculated by transforming to y=a*ln(xc/a), and
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

int main( int argc , char * argv[] )
{
   double aa=2.0 , bb=100.0 , pp=70.0 , e0,e1 ;
   int narg=1 , nvox,ii ;
   float * bv ;

   THD_3dim_dataset * dset , * mask_dset ;
   float mask_bot=666.0 , mask_top=-666.0 ;
   byte * mmm=NULL ;

   if( argc < 2 ){
      printf("Usage: 3dbetafit [options] dataset\n"
             "Fits a beta distribution to the values in a brick.\n"
             "\n"
             "Options:\n"
             "  -amid aa    = Sets the middle of the search range\n"
             "                 for the 'a' parameter to 'aa'\n"
             "  -bmid bb    = Sets the middleof the search range\n"
             "                 for the 'b' parameter to 'bb'\n"
             "  -mask mset  = A mask dataset to indicate which\n"
             "                 voxels are to be used\n"
             "  -mrange b t = Use only mask values in range from\n"
             "                 'b' to 't' (inclusive)\n"
             "  -pcut pp    = Cut off the cumulative histogram\n"
             "                 at percentage 'pp' [default=70]\n"
         ) ;
         exit(0) ;
   }

   /* scan command-line args */

   while( narg < argc && argv[narg][0] == '-' ){

      if( strcmp(argv[narg],"-amid") == 0 ){
         aa = strtod(argv[++narg],NULL) ;
         if( aa < 0.2 || aa > 10.0 ){
            fprintf(stderr,"*** Illegal value after -amid!\n");exit(1);
         }
         narg++ ; continue;
      }

      if( strcmp(argv[narg],"-bmid") == 0 ){
         bb = strtod(argv[++narg],NULL) ;
         if( bb < 0.2 || aa > 1000.0 ){
            fprintf(stderr,"*** Illegal value after -bmid!\n");exit(1);
         }
         narg++ ; continue;
      }

      if( strcmp(argv[narg],"-pcut") == 0 ){
         pp = strtod(argv[++narg],NULL) ;
         if( pp < 20.0 || pp > 99.0 ){
            fprintf(stderr,"*** Illegal value after -pcut!\n");exit(1);
         }
         narg++ ; continue;
      }

      if( strncmp(argv[narg],"-mask",5) == 0 ){
         if( mask_dset != NULL ){
            fprintf(stderr,"*** Cannot have two -mask options!\n") ; exit(1) ;
         }
         if( narg+1 >= argc ){
            fprintf(stderr,"*** -mask option requires a following argument!\n");
            exit(1) ;
         }
         mask_dset = THD_open_dataset( argv[++narg] ) ;
         if( mask_dset == NULL ){
            fprintf(stderr,"*** Cannot open mask dataset!\n") ; exit(1) ;
         }
         if( DSET_BRICK_TYPE(mask_dset,0) == MRI_complex ){
            fprintf(stderr,"*** Cannot deal with complex-valued mask dataset!\n");
            exit(1) ;
         }
         narg++ ; continue ;
      }

      if( strncmp(argv[narg],"-mrange",5) == 0 ){
         if( narg+2 >= argc ){
            fprintf(stderr,"*** -mrange option requires 2 following arguments!\n")
;
             exit(1) ;
         }
         mask_bot = strtod( argv[++narg] , NULL ) ;
         mask_top = strtod( argv[++narg] , NULL ) ;
         if( mask_top < mask_top ){
            fprintf(stderr,"*** -mrange inputs are illegal!\n") ; exit(1) ;
         }
         narg++ ; continue ;
      }

      fprintf(stderr,"*** Illegal option: %s\n",argv[narg]) ; exit(1) ;
   }

   if( narg >= argc ){
      fprintf(stderr,"*** No dataset argument on command line!?\n");exit(1);
   }

   dset = THD_open_dataset( argv[narg] ) ;
   if( dset == NULL ){
      fprintf(stderr,"*** Can't open dataset %s\n",argv[narg]); exit(1);
   }
   nvox = DSET_NVOX(dset) ;

   /* make a byte mask from mask dataset */

   if( mask_dset != NULL ){
      int mcount ;
      if( DSET_NVOX(mask_dset) != nvox ){
         fprintf(stderr,"*** Input and mask datasets are not same dimensions!\n");
         exit(1) ;
      }
      mmm = THD_makemask( mask_dset , 0 , mask_bot,mask_top ) ;
      mcount = THD_countmask( nvox , mmm ) ;
      fprintf(stderr,"+++ %d voxels in the mask\n",mcount) ;
      if( mcount <= 999 ){
         fprintf(stderr,"*** Mask is too small!\n");exit(1);
      }
      DSET_delete(mask_dset) ;
   }

   /* load data from dataset */

   DSET_load(dset) ;
   if( !DSET_LOADED(dset) ){
      fprintf(stderr,"*** Couldn't load dataset brick!\n");exit(1);
   }

   /* compute expected values of log(x) and log(1-x) */

   e0=e1 = 0.0 ;
   for( ii=n=0 ; ii < npt ; ii++ ){
      if( bv[ii] < xc ){
         e0 += log(bv[ii]) ; e1 += log(1.0-bv[ii]) ; n++ ;
      }
   }
   e0 /= n ; e1 /= n ;
   fprintf(stderr,"%d points below cutoff; e0=%g  e1=%g\n",n,e0,e1) ;

   /* setup range of values over which to solve, then solve  */

   betarange( 0.2*aa , 5.0*aa , 0.2*bb , 5.0*bb , 31416 ) ;

   betasolve( e0,e1,xc , &aa,&bb );

   exit(0) ;
}
