#include "mrilib.h"

/*-----------------------------------------------------------------------*/
/*! Compute the product of two matrices, stored in 2D float images.
-------------------------------------------------------------------------*/

MRI_IMAGE * mri_matrix_mult( MRI_IMAGE *ima , MRI_IMAGE *imb )
{
   int nr , nc , mm ;
   MRI_IMAGE *imc ;
   float *amat , *bmat , *cmat , sum ;
   int ii,jj,kk ;

ENTRY("mri_matrix_mult") ;

   if( ima == NULL            || imb == NULL            ) RETURN( NULL );
   if( ima->kind != MRI_float || imb->kind != MRI_float ) RETURN( NULL );

   if( ima->nx == 1 && ima->ny == 1 ){           /** scalar multiply? **/
     amat = MRI_FLOAT_PTR(ima) ;
     imc = mri_matrix_scale( amat[0] , imb ) ;
     RETURN(imc) ;
   } else if( imb->nx == 1 && imb->ny == 1 ){
     bmat = MRI_FLOAT_PTR(imb) ;
     imc = mri_matrix_scale( bmat[0] , ima ) ;
     RETURN(imc) ;
   }

   nr = ima->nx ; mm = ima->ny ; nc = imb->ny ;
   if( imb->nx != mm ){
     ERROR_message("mri_matrix_mult( %d X %d , %d X %d )?",
                   ima->nx , ima->ny , imb->nx , imb->ny ) ;
     RETURN( NULL);
   }

#undef  A
#undef  B
#undef  C
#define A(i,j) amat[(i)+(j)*nr]   /* nr X mm */
#define B(i,j) bmat[(i)+(j)*mm]   /* mm X nc */
#define C(i,j) cmat[(i)+(j)*nr]   /* nr X nc */

   imc  = mri_new( nr , nc , MRI_float ) ;
   amat = MRI_FLOAT_PTR(ima); bmat = MRI_FLOAT_PTR(imb);
   cmat = MRI_FLOAT_PTR(imc);

   for( jj=0 ; jj < nc ; jj++ ){
     for( ii=0 ; ii < nr ; ii++ ){
       sum = 0.0f ;
       for( kk=0 ; kk < mm ; kk++ ) sum += A(ii,kk)*B(kk,jj) ;
       C(ii,jj) = sum ;
   }}

   RETURN( imc );
}

/*-----------------------------------------------------------------------*/
/*! Compute the product of two matrices, stored in 2D float images.
    The first matrix is transposed.
-------------------------------------------------------------------------*/

MRI_IMAGE * mri_matrix_multranA( MRI_IMAGE *ima , MRI_IMAGE *imb )
{
   int nr , nc , mm ;
   MRI_IMAGE *imc ;
   float *amat , *bmat , *cmat , sum ;
   int ii,jj,kk ;

ENTRY("mri_matrix_multranA") ;

   if( ima == NULL            || imb == NULL            ) RETURN( NULL );
   if( ima->kind != MRI_float || imb->kind != MRI_float ) RETURN( NULL );

   nr = ima->ny ; mm = ima->nx ; nc = imb->ny ;
   if( imb->nx != mm ){
     ERROR_message("mri_matrix_multranA( %d X %d , %d X %d )?",
                   ima->nx , ima->ny , imb->nx , imb->ny ) ;
     RETURN( NULL);
   }

#undef  A
#undef  B
#undef  C
#define A(i,j) amat[(i)+(j)*mm]   /* mm X nr */
#define B(i,j) bmat[(i)+(j)*mm]   /* mm X nc */
#define C(i,j) cmat[(i)+(j)*nr]   /* nr X nc */

   imc  = mri_new( nr , nc , MRI_float ) ;
   amat = MRI_FLOAT_PTR(ima); bmat = MRI_FLOAT_PTR(imb);
   cmat = MRI_FLOAT_PTR(imc);

   for( jj=0 ; jj < nc ; jj++ ){
     for( ii=0 ; ii < nr ; ii++ ){
       sum = 0.0f ;
       for( kk=0 ; kk < mm ; kk++ ) sum += A(kk,ii)*B(kk,jj) ;
       C(ii,jj) = sum ;
   }}

   RETURN( imc );
}

/*-----------------------------------------------------------------------*/
/*! Compute the product of two matrices, stored in 2D float images.
    The second matrix is transposed.
-------------------------------------------------------------------------*/

MRI_IMAGE * mri_matrix_multranB( MRI_IMAGE *ima , MRI_IMAGE *imb )
{
   int nr , nc , mm ;
   MRI_IMAGE *imc ;
   float *amat , *bmat , *cmat , sum ;
   int ii,jj,kk ;

ENTRY("mri_matrix_multranB") ;

   if( ima == NULL            || imb == NULL            ) RETURN( NULL );
   if( ima->kind != MRI_float || imb->kind != MRI_float ) RETURN( NULL );

   nr = ima->nx ; mm = ima->ny ; nc = imb->nx ;
   if( imb->ny != mm ){
     ERROR_message("mri_matrix_multranB( %d X %d , %d X %d )?",
                   ima->nx , ima->ny , imb->nx , imb->ny ) ;
     RETURN( NULL);
   }

#undef  A
#undef  B
#undef  C
#define A(i,j) amat[(i)+(j)*nr]   /* nr X mm */
#define B(i,j) bmat[(i)+(j)*nc]   /* nc X mm */
#define C(i,j) cmat[(i)+(j)*nr]   /* nr X nc */

   imc  = mri_new( nr , nc , MRI_float ) ;
   amat = MRI_FLOAT_PTR(ima); bmat = MRI_FLOAT_PTR(imb);
   cmat = MRI_FLOAT_PTR(imc);

#if 0
   INFO_message("mri_matrix_multranB( %d X %d , %d X %d )",
                ima->nx , ima->ny , imb->nx , imb->ny ) ;
#endif

   for( jj=0 ; jj < nc ; jj++ ){
     for( ii=0 ; ii < nr ; ii++ ){
       sum = 0.0f ;
       for( kk=0 ; kk < mm ; kk++ ) sum += A(ii,kk)*B(jj,kk) ;
       C(ii,jj) = sum ;
   }}

   RETURN( imc );
}

/*-----------------------------------------------------------------------*/

MRI_IMAGE * mri_matrix_sadd( float fa, MRI_IMAGE *ima, float fb, MRI_IMAGE *imb )
{
   int ii , nrc ;
   MRI_IMAGE *imc ;
   float *amat , *bmat , *cmat ;

ENTRY("mri_matrix_sadd") ;

   if( ima == NULL            || imb == NULL            ) RETURN( NULL );
   if( ima->kind != MRI_float || imb->kind != MRI_float ) RETURN( NULL );

   nrc = ima->nvox ;
   if( imb->nvox != nrc ){
     ERROR_message("mri_matrix_sadd( %d X %d , %d X %d ) ?",
                   ima->nx,ima->ny , imb->nx,imb->ny ) ;
     RETURN(NULL) ;
   }

   imc  = mri_new_conforming(ima,MRI_float) ;
   amat = MRI_FLOAT_PTR(ima); bmat = MRI_FLOAT_PTR(imb);
   cmat = MRI_FLOAT_PTR(imc);
   for( ii=0 ; ii < nrc ; ii++ ) cmat[ii] = fa*amat[ii] + fb*bmat[ii] ;

   RETURN(imc) ;
}

/*-----------------------------------------------------------------------*/

MRI_IMAGE * mri_matrix_scale( float fa, MRI_IMAGE *ima )
{
   int ii , nrc ;
   MRI_IMAGE *imc ;
   float *amat , *cmat ;

ENTRY("mri_matrix_scale") ;

   if( ima == NULL            ) RETURN( NULL );
   if( ima->kind != MRI_float ) RETURN( NULL );

   nrc  = ima->nvox ;
   imc  = mri_new_conforming(ima,MRI_float ) ;
   amat = MRI_FLOAT_PTR(ima);
   cmat = MRI_FLOAT_PTR(imc);
   for( ii=0 ; ii < nrc ; ii++ ) cmat[ii] = fa*amat[ii] ;

   RETURN(imc) ;
}

/*-----------------------------------------------------------------------*/
static int force_svd = 0 ;
void mri_matrix_psinv_svd( int i ){ force_svd = i; }
/*-----------------------------------------------------------------------*/
/*! Compute the pseudo-inverse of a matrix stored in a 2D float image.
    If the input is mXn, the output is nXm.  wt[] is an optional array
    of positive weights, m of them.  The result can be used to solve
    the weighted least squares problem
      [imc] [b] = [v]
    where [b] is an n-vector and [v] is an m-vector, where m > n.
    If alpha > 0, then the actual matrix calculated is
                          -1
      [imc' imc + alpha I]   imc'    (where ' = transpose)

    This can be used to solve the penalized least squares problem.
-------------------------------------------------------------------------*/

MRI_IMAGE * mri_matrix_psinv( MRI_IMAGE *imc , float *wt , float alpha )
{
   float *rmat ;
   int m , n , ii,jj,kk ;
   double *amat , *umat , *vmat , *sval , *xfac , smax,del,ww , alp ;
   MRI_IMAGE *imp=NULL ; float *pmat ;
   register double sum ;
   int do_svd= (force_svd || AFNI_yesenv("AFNI_PSINV_SVD")) ;

ENTRY("mri_matrix_psinv") ;

   if( imc == NULL || imc->kind != MRI_float ) RETURN( NULL );
   m    = imc->nx ;
   n    = imc->ny ;
   rmat = MRI_FLOAT_PTR(imc) ;
   amat = (double *)calloc( sizeof(double),m*n ) ;  /* input matrix */
   xfac = (double *)calloc( sizeof(double),n   ) ;  /* column norms of [a] */

#undef  PSINV_EPS
#define PSINV_EPS 1.e-8

   alp  = (alpha <= 0.0f) ? PSINV_EPS : (double)alpha ;

#undef  R
#undef  A
#undef  P
#undef  U
#undef  V
#define R(i,j) rmat[(i)+(j)*m]   /* i=0..m-1 , j=0..n-1 */
#define A(i,j) amat[(i)+(j)*m]   /* i=0..m-1 , j=0..n-1 */
#define P(i,j) pmat[(i)+(j)*n]   /* i=0..n-1 , j=0..m-1 */
#define U(i,j) umat[(i)+(j)*m]
#define V(i,j) vmat[(i)+(j)*n]

   /* copy input matrix into amat */

   for( ii=0 ; ii < m ; ii++ )
     for( jj=0 ; jj < n ; jj++ ) A(ii,jj) = R(ii,jj) ;

   /* weight rows? */

   if( wt != NULL ){
     for( ii=0 ; ii < m ; ii++ ){
       ww = wt[ii] ;
       if( ww > 0.0 ) for( jj=0 ; jj < n ; jj++ ) A(ii,jj) *= ww ;
     }
   }

   /* scale each column to have norm 1 */

   for( jj=0 ; jj < n ; jj++ ){
     sum = 0.0 ;
     for( ii=0 ; ii < m ; ii++ ) sum += A(ii,jj)*A(ii,jj) ;
     if( sum > 0.0 ) sum = 1.0/sqrt(sum) ; else do_svd = 1 ;
     xfac[jj] = sum ;
     for( ii=0 ; ii < m ; ii++ ) A(ii,jj) *= sum ;
   }

   /*** computations follow, via SVD or Choleski ***/

   vmat = (double *)calloc( sizeof(double),n*n );

   if( do_svd ) goto SVD_PLACE ;

   /*** Try the Choleski method first ***/

   for( ii=0 ; ii < n ; ii++ ){       /* form normal equations */
     for( jj=0 ; jj <= ii ; jj++ ){
       sum = 0.0 ;
       for( kk=0 ; kk < m ; kk++ ) sum += A(kk,ii) * A(kk,jj) ;
       V(ii,jj) = sum ;
     }
     V(ii,ii) += alp ;   /* note V(ii,ii)==1 before this */
   }

   /* Choleski factor V in place */

   for( ii=0 ; ii < n ; ii++ ){
     for( jj=0 ; jj < ii ; jj++ ){
       sum = V(ii,jj) ;
       for( kk=0 ; kk < jj ; kk++ ) sum -= V(ii,kk) * V(jj,kk) ;
       V(ii,jj) = sum / V(jj,jj) ;
     }
     sum = V(ii,ii) ;
     for( kk=0 ; kk < ii ; kk++ ) sum -= V(ii,kk) * V(ii,kk) ;
     if( sum <= 0.0 ){
       WARNING_message("Choleski fails in mri_matrix_psinv()!\n");
       do_svd = 1 ; goto SVD_PLACE ;
     }
     V(ii,ii) = sqrt(sum) ;
   }

   /* create pseudo-inverse from what's now in V */

   imp  = mri_new( n , m , MRI_float ) ;   /* recall that m > n */
   pmat = MRI_FLOAT_PTR(imp) ;

   sval = (double *)calloc( sizeof(double),n ) ; /* row #jj of A */

   for( jj=0 ; jj < m ; jj++ ){
     for( ii=0 ; ii < n ; ii++ ) sval[ii] = A(jj,ii) ; /* extract row */

     for( ii=0 ; ii < n ; ii++ ){  /* forward solve */
       sum = sval[ii] ;
       for( kk=0 ; kk < ii ; kk++ ) sum -= V(ii,kk) * sval[kk] ;
       sval[ii] = sum / V(ii,ii) ;
     }
     for( ii=n-1 ; ii >= 0 ; ii-- ){  /* backward solve */
       sum = sval[ii] ;
       for( kk=ii+1 ; kk < n ; kk++ ) sum -= V(kk,ii) * sval[kk] ;
       sval[ii] = sum / V(ii,ii) ;
     }

     for( ii=0 ; ii < n ; ii++ ) P(ii,jj) = (float)sval[ii] ;
   }
   free((void *)amat); free((void *)vmat); free((void *)sval);
   goto RESCALE_PLACE ;

  SVD_PLACE:

#if 0
     vmat = (double *)calloc( sizeof(double),n*n ); /* right singular vectors */
#endif
     umat = (double *)calloc( sizeof(double),m*n ); /* left singular vectors */
     sval = (double *)calloc( sizeof(double),n   ); /* singular values */

     /* compute SVD of scaled matrix */

     svd_double( m , n , amat , sval , umat , vmat ) ;

     free((void *)amat) ;  /* done with this */

     /* find largest singular value */

     smax = sval[0] ;
     for( ii=1 ; ii < n ; ii++ ) if( sval[ii] > smax ) smax = sval[ii] ;

     if( smax <= 0.0 ){                        /* this is bad */
       ERROR_message("SVD fails in mri_matrix_psinv()!\n");
       free((void *)xfac); free((void *)sval);
       free((void *)vmat); free((void *)umat); RETURN( NULL);
     }

     for( ii=0 ; ii < n ; ii++ )
       if( sval[ii] < 0.0 ) sval[ii] = 0.0 ;  /* should not happen */

     /* "reciprocals" of singular values:  1/s is actually s/(s^2+del) */

     del = PSINV_EPS * smax*smax ; if( del < alp ) del = alp ;
     for( ii=0 ; ii < n ; ii++ )
       sval[ii] = sval[ii] / ( sval[ii]*sval[ii] + del ) ;

     /* create pseudo-inverse */

     imp  = mri_new( n , m , MRI_float ) ;   /* recall that m > n */
     pmat = MRI_FLOAT_PTR(imp) ;

     for( ii=0 ; ii < n ; ii++ ){
       for( jj=0 ; jj < m ; jj++ ){
         sum = 0.0 ;
         for( kk=0 ; kk < n ; kk++ ) sum += sval[kk] * V(ii,kk) * U(jj,kk) ;
         P(ii,jj) = (float)sum ;
       }
     }
     free((void *)sval); free((void *)vmat); free((void *)umat);

  RESCALE_PLACE:
   /** from either method, must now rescale rows from norming */

   for( ii=0 ; ii < n ; ii++ ){
     for( jj=0 ; jj < m ; jj++ ) P(ii,jj) *= xfac[ii] ;
   }
   free((void *)xfac);

   /* rescale cols for weight? */

   if( wt != NULL ){
     for( ii=0 ; ii < m ; ii++ ){
       ww = wt[ii] ;
       if( ww > 0.0 ) for( jj=0 ; jj < n ; jj++ ) P(jj,ii) *= ww ;
     }
   }

   RETURN( imp);
}

/*----------------------------------------------------------------------------*/
/*! The output matrix is the orthogonal projection onto the linear space
    spanned by the columns of the input imc.  If pout != 0, then instead
    it is the orthogonal projection onto the complement of this space.
    If the input is NxM, the output is NxN.   [10 Apr 2006]
------------------------------------------------------------------------------*/

MRI_IMAGE * mri_matrix_ortproj( MRI_IMAGE *imc , int pout )
{
   MRI_IMAGE *imp , *imt ;

ENTRY("mri_matrix_ortproj") ;

   if( imc == NULL || imc->kind != MRI_float ) RETURN( NULL );

   imp = mri_matrix_psinv( imc , NULL , 0.0 ) ;
   if( imp == NULL ) RETURN(NULL) ;
   imt = mri_matrix_mult( imc , imp ) ; mri_free(imp) ;

   if( pout ){
     int nn , nq , ii ; float *tar ;
     nn = imt->nx ; nq = nn*nn ; tar = MRI_FLOAT_PTR(imt) ;
     for( ii=0 ; ii < nq ; ii+=(nn+1) ) tar[ii] -= 1.0f ;
     for( ii=0 ; ii < nq ; ii++       ) tar[ii]  = -tar[ii] ;
   }

   RETURN(imt) ;
}

/*----------------------------------------------------------------------------*/
/*! Legendre polynomial of non-negative order m evaluated at x;
    x may be any real number, but the main use is for -1 <= x <= 1 (duh).
------------------------------------------------------------------------------*/

double Plegendre( double x , int m )
{
   double pk, pkm1, pkm2 ; int k ;  /* for the recurrence, when m > 20 */

   if( m < 0 ) return 1.0 ;    /* bad input */

   switch( m ){                /*** direct formulas for P_m(x) for m=0..20 ***/
    case 0: return 1.0 ;       /* that was easy */
    case 1: return x ;
    case 2: return (3.0*x*x-1.0)/2.0 ;
    case 3: return (5.0*x*x-3.0)*x/2.0 ;
    case 4: return ((35.0*x*x-30.0)*x*x+3.0)/8.0 ;
    case 5: return ((63.0*x*x-70.0)*x*x+15.0)*x/8.0 ;
    case 6: return (((231.0*x*x-315.0)*x*x+105.0)*x*x-5.0)/16.0 ;
    case 7: return (((429.0*x*x-693.0)*x*x+315.0)*x*x-35.0)*x/16.0 ;
    case 8: return ((((6435.0*x*x-12012.0)*x*x+6930.0)*x*x-1260.0)*x*x+35.0)/128.0;

           /** 07 Feb 2005: this part generated by Maple, then hand massaged **/

    case 9:
      return (0.24609375e1 + (-0.3609375e2 + (0.140765625e3 + (-0.20109375e3
              + 0.949609375e2 * x * x) * x * x) * x * x) * x * x) * x;

    case 10:
      return -0.24609375e0 + (0.1353515625e2 + (-0.1173046875e3 +
              (0.3519140625e3 + (-0.42732421875e3 + 0.18042578125e3 * x * x)
             * x * x) * x * x) * x * x) * x * x;

    case 11:
      return (-0.270703125e1 + (0.5865234375e2 + (-0.3519140625e3 +
             (0.8546484375e3 + (-0.90212890625e3 + 0.34444921875e3 * x * x)
             * x * x) * x * x) * x * x) * x * x) * x;

    case 12:
      return 0.2255859375e0 + (-0.17595703125e2 + (0.2199462890625e3 +
             (-0.99708984375e3 + (0.20297900390625e4 + (-0.1894470703125e4
             + 0.6601943359375e3 * x * x) * x * x) * x * x) * x * x) * x * x)
             * x * x;

    case 13:
      return (0.29326171875e1 + (-0.87978515625e2 + (0.7478173828125e3 +
             (-0.270638671875e4 + (0.47361767578125e4 + (-0.3961166015625e4
             + 0.12696044921875e4 * x * x) * x * x) * x * x) * x * x) * x * x)
            * x * x) * x;

    case 14:
      return -0.20947265625e0 + (0.2199462890625e2 + (-0.37390869140625e3 +
             (0.236808837890625e4 + (-0.710426513671875e4 +
             (0.1089320654296875e5 + (-0.825242919921875e4 +
            0.244852294921875e4 * x * x) * x * x) * x * x) * x * x) * x * x)
           * x * x) * x * x;

    case 15:
      return (-0.314208984375e1 + (0.12463623046875e3 + (-0.142085302734375e4
            + (0.710426513671875e4 + (-0.1815534423828125e5 +
              (0.2475728759765625e5 + (-0.1713966064453125e5 +
               0.473381103515625e4 * x * x) * x * x) * x * x) * x * x)
             * x * x) * x * x) * x * x) * x;

    case 16:
      return 0.196380615234375e0 + (-0.26707763671875e2 + (0.5920220947265625e3
            + (-0.4972985595703125e4 + (0.2042476226806641e5 +
              (-0.4538836059570312e5 + (0.5570389709472656e5 +
              (-0.3550358276367188e5 + 0.9171758880615234e4 * x * x) * x * x)
            * x * x) * x * x) * x * x) * x * x) * x * x) * x * x;

    case 17:
      return (0.3338470458984375e1 + (-0.1691491699218750e3 +
             (0.2486492797851562e4 + (-0.1633980981445312e5 +
             (0.5673545074462891e5 + (-0.1114077941894531e6 +
             (0.1242625396728516e6 + (-0.7337407104492188e5 +
              0.1780400253295898e5 * x * x) * x * x) * x * x) * x * x)
           * x * x) * x * x) * x * x) * x * x) * x;

    case 18:
      return -0.1854705810546875e0 + (0.3171546936035156e2 +
            (-0.8880331420898438e3 + (0.9531555725097656e4 +
            (-0.5106190567016602e5 + (0.1531857170104980e6 +
            (-0.2692355026245117e6 + (0.2751527664184570e6 +
            (-0.1513340215301514e6 + 0.34618893814086910e5 * x * x) * x * x)
           * x * x) * x * x) * x * x) * x * x) * x * x) * x * x) * x * x;

    case 19:
      return (-0.3523941040039062e1 + (0.2220082855224609e3 +
             (-0.4084952453613281e4 + (0.3404127044677734e5 +
             (-0.1531857170104980e6 + (0.4038532539367676e6 +
             (-0.6420231216430664e6 + (0.6053360861206055e6 +
             (-0.3115700443267822e6 + 0.67415740585327150e5 * x * x) * x * x)
          * x * x) * x * x) * x * x) * x * x) * x * x) * x * x) * x * x) * x;

    case 20:
      return 0.1761970520019531e0 + (-0.3700138092041016e2 +
            (0.1276547641754150e4 + (-0.1702063522338867e5 +
            (0.1148892877578735e6 + (-0.4442385793304443e6 +
            (0.1043287572669983e7 + (-0.1513340215301514e7 +
            (0.1324172688388824e7 + (-0.6404495355606079e6 +
             0.1314606941413879e6 * x * x) * x * x) * x * x) * x * x) * x * x)
            * x * x) * x * x) * x * x) * x * x) * x * x;
   }

   /**----- if here, m > 20 ==> use recurrence relation ----**/

   pkm2 = Plegendre( x , 19 ) ;  /* recursion to start things off! */
   pkm1 = Plegendre( x , 20 ) ;
   for( k=21 ; k <= m ; k++ , pkm2=pkm1 , pkm1=pk )
     pk = ((2.0*k-1.0)*x*pkm1 - (k-1.0)*pkm2)/k ;
   return pk ;
}

/******************************************************************************/
/**** Matrix RPN calculator ***************************************************/
/******************************************************************************/

static MRI_IMARR *matar = NULL ;  /* list of named matrices */

/*----------------------------------------------------------------------------*/

static int matrix_name_lookup( char *nam )
{
   int ii ;
   MRI_IMAGE *im ;

   if( nam == NULL || matar == NULL ) return -1 ;

   for( ii=0 ; ii < IMARR_COUNT(matar) ; ii++ ){
    im = IMARR_SUBIM(matar,ii) ;
    if( im != NULL && strcmp(nam,im->name) == 0 ) return ii;
   }

   return -1 ;
}

/*----------------------------------------------------------------------------*/

static void matrix_name_assign( char *nam , MRI_IMAGE *ima )
{
   int ii ; MRI_IMAGE *imb ;

   if( nam == NULL || *nam == '\0' || ima == NULL ) return ;

   if( matar == NULL ) INIT_IMARR(matar) ;

   ii  = matrix_name_lookup( nam ) ;
   imb = mri_to_float(ima) ; mri_add_name(nam,imb) ;
   if( ii < 0 ){
     ADDTO_IMARR(matar,imb) ;
   } else {
     mri_free( IMARR_SUBIM(matar,ii) ) ;
     IMARR_SUBIM(matar,ii) = imb ;
   }
   return ;
}

/*----------------------------------------------------------------------------*/

#undef  ERREX
#define ERREX(sss)                                                      \
  do{ ERROR_message("matrix_evalrpn('%s') at '%s': %s" , expr,cmd,sss ); \
      DESTROY_IMARR(imstk); NI_delete_str_array(sar); RETURN(NULL);       \
  } while(0)

MRI_IMAGE * mri_matrix_evalrpn( char *expr )
{
   NI_str_array *sar ;
   char         *cmd , mess[512] ;
   MRI_IMARR *imstk ;
   int         nstk , ii , ss ;
   MRI_IMAGE *ima, *imb, *imc ;
   float     *aar, *bar, *car ;

ENTRY("mri_matrix_evalrpn") ;

   sar = NI_decode_string_list( expr , "~" ) ;
   if( sar == NULL ) RETURN(NULL) ;

   INIT_IMARR(imstk) ;
   mri_matrix_psinv_svd(1) ;

   for( ss=0 ; ss < sar->num ; ss++ ){

      cmd = sar->str[ss] ;
      nstk = IMARR_COUNT(imstk) ;

      if( *cmd == '\0' ) continue ;      /* WTF? */

      else if( isdigit(cmd[0]) || (cmd[0]=='-' && isdigit(cmd[1])) ){
        imc = mri_new(1,1,MRI_float) ; car = MRI_FLOAT_PTR(imc) ;
        car[0] = (float)strtod(cmd,NULL) ;
        ADDTO_IMARR(imstk,imc) ;
      }

      else if( cmd[0] == '\'' || cmd[0] == '"' ){
        char *buf = strdup(cmd+1) ;
        imc = mri_new(1,1,MRI_float) ;
        for( ii=0 ; buf[ii] != cmd[0] && buf[ii] != '\0' ; ii++ ) ; /*nada*/
        buf[ii] = '\0' ; mri_add_name(buf,imc) ;
        ADDTO_IMARR(imstk,imc) ;
      }

      else if( cmd[0] == '=' && isalpha(cmd[1]) ){
        if( nstk < 1 ) ERREX("no matrix") ;
        ima = IMARR_SUBIM(imstk,nstk-1) ;
        matrix_name_assign(cmd+1,ima) ;
      }

      else if( strcmp(cmd,"'") == 0 || strncasecmp(cmd,"&trans",6) == 0 ){
        if( nstk < 1 ) ERREX("no matrix") ;
        ima = IMARR_SUBIM(imstk,nstk-1) ;
        imc = mri_matrix_transpose(ima) ;
        TRUNCATE_IMARR(imstk,nstk-1) ;
        ADDTO_IMARR(imstk,imc) ;
      }

      else if( strncasecmp(cmd,"&read",5) == 0 ){
        char *buf = strdup(cmd+5) ;
        if( nstk < 1 ) ERREX("no string") ;
        ima = IMARR_SUBIM(imstk,nstk-1) ;
        if( ima->name == NULL ) ERREX("no string") ;
        imc = mri_read_1D( ima->name ) ;
        if( imc == NULL ){
          sprintf(mess,"can't read file '%s'",ima->name); ERREX(mess);
        }
        TRUNCATE_IMARR(imstk,nstk-1) ;
        ADDTO_IMARR(imstk,imc) ;
        if( buf[0] == '=' && isalpha(buf[1]) ) matrix_name_assign(buf+1,imc) ;
      }

      else if( strncasecmp(cmd,"&write",5) == 0 ){
        if( nstk < 2 ) ERREX("no inputs") ;
        ima = IMARR_SUBIM(imstk,nstk-1) ;
        imb = IMARR_SUBIM(imstk,nstk-2) ;
        if( ima->name == NULL ) ERREX("no string") ;
        mri_write_1D( ima->name , imb ) ;
        TRUNCATE_IMARR(imstk,nstk-1) ;
      }

      else if( strncasecmp(cmd,"&ident(",7) == 0 ){
        int nn=0 ; sscanf(cmd+7,"%d",&nn) ;
        if( nn <= 0 ) ERREX("illegal dimension") ;
        imc = mri_new(nn,nn,MRI_float) ; car = MRI_FLOAT_PTR(imc) ;
        for( ii=0 ; ii < nn ; ii++ ) car[ii+ii*nn] = 1.0f ;
        ADDTO_IMARR(imstk,imc) ;
      }

      else if( strncasecmp(cmd,"&Psinv",6) == 0 ){
        if( nstk < 1 ) ERREX("no matrix") ;
        ima = IMARR_SUBIM(imstk,nstk-1) ;
        imc = mri_matrix_psinv( ima , NULL , 0.0f ) ;
        if( imc == NULL ) ERREX("can't compute") ;
        TRUNCATE_IMARR(imstk,nstk-1) ;
        ADDTO_IMARR(imstk,imc) ;
      }

      else if( strncasecmp(cmd,"&Pproj",6) == 0 ){
        if( nstk < 1 ) ERREX("no matrix") ;
        ima = IMARR_SUBIM(imstk,nstk-1) ;
        imc = mri_matrix_ortproj( ima , 0 ) ;
        if( imc == NULL ) ERREX("can't compute") ;
        TRUNCATE_IMARR(imstk,nstk-1) ;
        ADDTO_IMARR(imstk,imc) ;
      }

      else if( strncasecmp(cmd,"&Qproj",6) == 0 ){
        if( nstk < 1 ) ERREX("no matrix") ;
        ima = IMARR_SUBIM(imstk,nstk-1) ;
        imc = mri_matrix_ortproj( ima , 1 ) ;
        if( imc == NULL ) ERREX("can't compute") ;
        TRUNCATE_IMARR(imstk,nstk-1) ;
        ADDTO_IMARR(imstk,imc) ;
      }

      else if( strcmp(cmd,"*") == 0 ){   /* multiply */
        if( nstk < 2 ) ERREX("no matrices") ;
        ima = IMARR_SUBIM(imstk,nstk-2) ;
        imb = IMARR_SUBIM(imstk,nstk-1) ;
        imc = mri_matrix_mult( ima , imb ) ;
        if( imc == NULL ) ERREX("illegal multiply") ;
        TRUNCATE_IMARR(imstk,nstk-2) ;
        ADDTO_IMARR(imstk,imc) ;
      }

      else if( strcmp(cmd,"+") == 0 ){   /* add */
        if( nstk < 2 ) ERREX("no matrices") ;
        ima = IMARR_SUBIM(imstk,nstk-2) ;
        imb = IMARR_SUBIM(imstk,nstk-1) ;
        imc = mri_matrix_sadd( 1.0f,ima , 1.0f,imb ) ;
        if( imc == NULL ) ERREX("illegal add") ;
        TRUNCATE_IMARR(imstk,nstk-2) ;
        ADDTO_IMARR(imstk,imc) ;
      }

      else if( strcmp(cmd,"-") == 0 ){   /* subtract */
        if( nstk < 2 ) ERREX("no matrices") ;
        ima = IMARR_SUBIM(imstk,nstk-2) ;
        imb = IMARR_SUBIM(imstk,nstk-1) ;
        imc = mri_matrix_sadd( 1.0f,ima , -1.0f,imb ) ;
        if( imc == NULL ) ERREX("illegal subtract") ;
        TRUNCATE_IMARR(imstk,nstk-2) ;
        ADDTO_IMARR(imstk,imc) ;
      }

      else if( isalpha(cmd[0]) ){   /* named matrix */
        ii = matrix_name_lookup(cmd) ;
        if( ii >= 0 ){
          imc = mri_to_float(IMARR_SUBIM(matar,ii)) ;
          ADDTO_IMARR(imstk,imc) ;
        } else {
          sprintf(mess,"can't lookup name '%s'",cmd); ERREX(mess);
        }
      }

      else {
        ERREX("unknown operation!?") ;
      }
   }

   NI_delete_str_array(sar) ;
   nstk = IMARR_COUNT(imstk) ;
   if( nstk > 0 ) ima = mri_to_float( IMARR_SUBIM(imstk,nstk-1) ) ;
   else           ima = NULL ;
   DESTROY_IMARR(imstk) ;
   RETURN(ima) ;
}
