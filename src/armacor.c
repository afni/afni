/******************************************************************************/
/** This file is for computing the restricted ARMA(p,1) correlations for
    the time series noise models used in AFNI. It is meant to be #include-d
    into the file that actually uses these functions.

    What is 'restricted' about these models?
    1) The MA part is limited to a white noise process added to a 'pure' AR
       process.
    2) The AR process is restricted to have 1 real root and 1 (p=3) or 2 (p=5)
       complex roots:
       - the real root models straightforward exponential decay of correlation
       - the complex roots model oscillatory decay of correlation
    These restrictions are here because I feel these cover the useful cases
    for the types of data AFNI deals with, and then the programs don't have
    to waste time/memory trying to optimize over non-useful parameter ranges.

    RWCox - 01 Jul 2020 - https://rb.gy/9t84mb
*//****************************************************************************/

static int armacor_debug = 0 ;

/*----------------------------------------------------------------------------*/
/* Return the vector of correlations for an AR(3) model plus additive white
   noise [=restricted ARMA(3,1)], given the AR(5) generating polynomial as
     phi(z) = (z-a) * (z-r1*exp(+i*t1)) * (z-r1*exp(-i*t1))
   which corresponds to the standard polynomial representation
     phi(z) = z^3 - p1*z^2 - p2*z - p3
   which corresponds to the AR(3) recurrence
     x[n]   = p1*x[n-1] + p2*x[n-2] + p3*x[n-3] + w[n]
   where
     a     = pure exponential decay component
     r1,t1 = decaying oscillation; 0 < r1 < 1;   0 < t1 < PI
             t1 = 2 * PI * TR * f1 [TR = time step; f1 = frequency]
     w[n]  = stationary white noise process driving the linear recurrence
   The first formulation of the model, instead of using the polynomial
   coefficients as in the second formulation above, is simpler to understand
   in terms of the correlation matrix.

   The vrt parameter is the ratio vrt = s^2 / ( s^2 + a^2 )
   where s^2 = variance of AR(5) process
         a^2 = variance of additive white noise process
   and clearly 0 <= vrt <= 1. For a pure AR(3) model, vrt=1.
   Note that this additive white noise has nothing to do with the w[n]
   noise in the AR recurrence! ("vrt" == "variance ratio")

   A maximum of ncmax correlations are computed (ncmax < 4 ==> no limit).
   Correlations are computed until three in a row are below ccut
   (default when ccut <= 0 ==> 0.00001).
*//*--------------------------------------------------------------------------*/

doublevec * arma31_correlations( double a , double r1 , double t1 ,
                                 double vrt , double ccut , int ncmax )
{
   double p1,p2,p3 , cnew , g1,g2 , c1 ;
   int kk , nzz , ncor ;
   doublevec *corvec=NULL ;

   /* check inputs */

   if( a < 0.0 || r1 < 0.0 ) return NULL ; /* bad user */

   if( vrt <= 0.01 ||                      /* no AR(3) noise? */
       (a <= 0.01 && r1 <= 0.01) ){
     MAKE_doublevec( corvec , 1 ) ;
     corvec->ar[0] = 1.0 ;
     return corvec ;
   }

   if( a   > 0.95 ) a   = 0.95 ;  /* limits */
   if( r1  > 0.95 ) r1  = 0.95 ;
   if( vrt > 1.00 ) vrt = 1.00  ;
   if( t1  > PI   ) t1  = PI   ; else if( t1 < 0.0 ) t1 = 0.0 ;

   if( ccut  <= 0.0 ) ccut  = 0.00001 ;
   if( ncmax <  4   ) ncmax = 6666 ;

   ccut = ccut / vrt ; if( ccut > 0.05 ) ccut = 0.05 ;

   /* compute polynomial coefficients from input parameters,
      by expansion of
        phi(z) = (z-a) * (z-r1*exp(+i*t1)) * (z-r1*exp(-i*t1))
      to the standard form
        phi(z) = z^3 - p1*z^2 - p2*z - p3                      */

   c1 = cos(t1) ;
   p1 = a + 2.0*r1*c1 ;
   p2 = -2.0*a*r1*c1 - r1*r1 ;
   p3 = a*r1*r1 ;

   /* solve equations for first two correlations g1 and g2:
        [    1-p2   -p3  ] [ g1 ] = [ p1 ]
        [ -(p3+p1)   1   ] [ g2 ] = [ p2 ]                 */

   cnew = (1.0-p2) - p3*(p1+p3) ;           /* matrix determinant */

   if( fabs(cnew) < 0.001 ){                /* error, should not happen */
     ERROR_message("bad AR(3) setup:\n"
                   "  a  = %g  r1 = %g  t1 = %g\n"
                   "  p1 = %g  p2 = %g  p3 = %g\n"
                   " det = %g"   , a,r1,t1 , p1,p2,p3 , cnew ) ;
     return NULL ;
   }

   /* the correlations */

   g1 = (         p1 + p3      *p2 ) / cnew ;
   g2 = ( (p1+p3)*p1 + (1.0-p2)*p2 ) / cnew ;

   if( fabs(g1) >= 1.0 || fabs(g2) > 1.0 ){  /* error, should not happen */
     WARNING_message("bad AR(3) setup:\n"
                     "  a  = %g  r1 = %g  t1 = %g\n"
                     "  p1 = %g  p2 = %g  p3 = %g\n"
                     " det = %g\n"
                     "  g1 = %g  g2 = %g" ,
                     a,r1,t1 , p1,p2,p3 , cnew , g1,g2 ) ;
   }

#if 1
{ double alp = ( g1 - r1*c1 ) / ( a - r1*c1 ) ;
  INFO_message("a=%g r=%g t=%g  g1=%g g2=%g alp=%g",
               a,r1,t1 , g1,g2,alp ) ;
}
#endif

   /* store them into the output vector */

   MAKE_doublevec( corvec , ncmax+1 ) ;

   corvec->ar[0] = 1.0 ;      /* correlation at lag 0 */
   corvec->ar[1] = g1 ;       /* lag 1 */
   corvec->ar[2] = g2 ;       /* lag 2 */

   /* compute further correlations recursively */

   for( nzz=0,ncor=2,kk=3 ; kk < ncmax ; kk++ ){

     /* next correlation, at lag kk */

     cnew = p1*corvec->ar[kk-1] + p2*corvec->ar[kk-2] + p3*corvec->ar[kk-3] ;

     /* and save new value */

     corvec->ar[kk] = cnew ; ncor++ ;

     /* check to see if we've shrunk as far as needed */

     if( abs(cnew) < ccut ){
       nzz++ ;                /* how many undersized in a row? */
       if( nzz > 2 ) break ;  /* 3 strikes and you are OUT! */
     } else {
       nzz = 0 ;              /* not too small ==> reset counter */
     }

   } /* end of recurrence loop */

   /* rescale lagged correlations to allow for additive white noise;
      note that we do NOT scale down the lag=0 correlation of 1.0   */

   if( vrt < 1.0 ){
     for( kk=1 ; kk <= ncor ; kk++ ) corvec->ar[kk] *= vrt ;
   }

   /* shrink the output vector to fit what was actually stored */

   RESIZE_doublevec( corvec , ncor+1 ) ;

   return corvec ;
}

/*============================================================================*/
#ifdef ALLOW_ARMA51
/*----------------------------------------------------------------------------*/
/* Compute the first 4 gammas (correlations) from
   the AR(5) polynomial coefficients, using a linear system
     phi(z) = z^5 - p1*z^4 - p2*z^3 - p3*z^2 - p4*z - p5
     x[n]   = p1*x[n-1] + p2*x[n-2] + p3*x[n-3] + p4*x[n-4] + p5*x[n-5] + w[n]
*//*--------------------------------------------------------------------------*/

double_quad arma51_gam1234( double p1, double p2, double p3, double p4, double p5 )
{
   dmat44 amat , imat ;
   double_quad g1234 ;

   /* the matrix connecting the pj coefficients to the gammas */

   LOAD_DMAT44( amat ,
                 1.0-p2  ,     -p3  , -p4 , -p5 ,
                -(p1+p3) ,  1.0-p4  , -p5 , 0.0 ,
                -(p2+p4) , -(p1+p5) , 1.0 , 0.0 ,
                -(p3+p5) ,     -p2  , -p1 , 1.0  ) ;

   /* invert the matrix */

   if( armacor_debug ){
     ININFO_message("arma51_gam1234: p1 = %g p2 = %g p3 = %g p4 = %g p5 = %g",p1,p2,p3,p4,p5) ;
     DUMP_DMAT44("matrix",amat) ;
     ININFO_message(" determ = %g",generic_dmat44_determinant(amat)) ;
   }

   imat = generic_dmat44_inverse( amat ) ;

   /* apply inverse to the pj coefficents themselves to get the gammas */

   DMAT44_VEC( imat , p1,p2,p3,p4 ,
               g1234.a , g1234.b , g1234.c , g1234.d ) ;

   if( armacor_debug ){
     DUMP_DMAT44("inverse",imat) ;
     ININFO_message("  g1 = %g g2 = %g g3 = %g g4 = %g" ,
                    g1234.a , g1234.b , g1234.c , g1234.d ) ;
   }

   return g1234 ;
}

/*------------------------------------------------------------------------------*/
/* Return the vector of correlations for an AR(5) model plus additive white
   noise, given the AR(5) generating polynomial as
     phi(z) = (z-a) * (z-r1*exp(+i*t1)) * (z-r2*exp(+i*t2))
                    * (z-r1*exp(-i*t1)) * (z-r2*exp(-i*t2))
   which corresponds to the standard polynomial representation
     phi(z) = z^5 - p1*z^4 - p2*z^3 - p3*z^2 - p4*z - p5
   which corresponds to the AR(5) autoregression
     x[n]   = p1*x[n-1] + p2*x[n-2] + p3*x[n-3] + p4*x[n-4] + p5*x[n-5] + w[n]
   where
     a     = pure exponential decay component
     r1,t1 = first decaying oscillation 0 < t1 < PI
     r2,t2 = second decaying oscillation
   This formulation of the model, instead of using the polynomial coefficients,
   is simpler to understand in terms of the correlation matrix.

   The vrt parameter is the ratio vrt = s^2 / ( s^2 + a^2 )
   where s^2 = variance of AR(5) process
         a^2 = variance of additive white noise process
   and clearly 0 <= vrt <= 1. For a pure AR(5) model, vrt=1.
   Note that this additive white noise has nothing to do with the w[n]
   noise in the AR(5) recurrence! ("vrt" == "variance ratio")

   A maximum of ncmax correlations are computed (ncmax < 4 ==> no limit).
   Correlations are computed until three in a row are below ccut
   (ccut <= 0 ==> 0.00001).

   NOTE WELL: For some combinations of the (a,r1,t1,r2,t2) parameters,
              the function produced by the AR(5) recurrence is NOT a
              valid (positive definite) autocorrelation function.
              In such a case, a scale factor 'f' < 1 is computed to scale
              down vrt for such parameter combinations. Thus, if you
              input vrt=0.8 and then the program decides f=0.8, you will
              actually get a correlation function with vrt=0.64 - that is,
              the white noise 'floor' will be amplified. [RWC - 15 Jul 2020]
*//*----------------------------------------------------------------------------*/

doublevec * arma51_correlations( double a , double r1 , double t1 ,
                                            double r2 , double t2 ,
                                 double vrt , double ccut , int ncmax )
{
   double_quad g1234 ;
   double p1,p2,p3,p4,p5 , cnew , c1,s1,c2,s2 , vrtfac=1.0 ;
   int kk , nzz , ncor ;
   doublevec *corvec=NULL ;

   if( a < 0.0 || r1 < 0.0 || r2 < 0.0 ) return NULL ; /* bad user */

   if( vrt <= 0.01 ||                           /* no AR(5) noise? */
       (a <= 0.01 && r1 <= 0.01 && r2 <= 0.01 ) ){
     MAKE_doublevec( corvec , 1 ) ;
     corvec->ar[0] = 1.0 ;
     return corvec ;
   }

   if( a   > 0.95 ) a   = 0.95 ;  /* limits */
   if( r1  > 0.95 ) r1  = 0.95 ;
   if( r2  > 0.95 ) r2  = 0.95 ;
   if( vrt > 1.0  ) vrt = 1.0  ;
   if( t1  > PI   ) t1  = PI   ; else if( t1 < 0.0 ) t1 = 0.0 ;
   if( t2  > PI   ) t2  = PI   ; else if( t2 < 0.0 ) t2 = 0.0 ;

   if( ccut  <= 0.0 ) ccut  = 0.00001 ;
   if( ncmax <  4   ) ncmax = 6666 ;

   ccut = ccut / vrt ; if( ccut > 0.05 ) ccut = 0.05 ;

   /* compute polynomial coefficients from input parameters,
      by expansion of the product
        phi(z) = (z-a) * (z-r1*exp(+i*t1)) * (z-r2*exp(+i*t2))
                       * (z-r1*exp(-i*t1)) * (z-r2*exp(-i*t2))
      into the form
        phi(z) = z^5 - p1*z^4 - p2*z^3 - p3*z^2 - p4*z - p5  */

   c1 = cos(t1) ; s1 = sin(t1) ;
   c2 = cos(t2) ; s2 = sin(t2) ;

   p1 =  2.0*r1*c1       + 2.0*r2*c2           + a ;
   p2 = -4.0*r1*r2*c1*c2 - 2.0*a*(r1*c1+r2*c2) - r1*r1 - r2*r2 ;
   p3 = a * ( r1*r1+r2*r2 + 4.0*r1*r2*c1*c2 )  + 2.0*r1*r2*(r2*c1+r1*c2) ;
   p4 = -2.0*a*r1*r2*(r2*c1+r1*c2)             - r1*r1*r2*r2 ;
   p5 = a * r1*r1 * r2*r2 ;

   /* compute first 4 gamma coefficients (correlations)
      from the linear equation connecting p1..p5 to g1..g4 */

   g1234 = arma51_gam1234( p1, p2, p3, p4, p5 ) ;

   if( fabs(g1234.a) >= 1.0 ||
       fabs(g1234.b) >= 1.0 ||
       fabs(g1234.c) >= 1.0 ||
       fabs(g1234.d) >= 1.0   ){  /* error, should not happen */

     WARNING_message("bad AR(5) setup:\n"
                     "  a  = %g  r1 = %g  t1 = %g  r2 = %g  t2 = %g\n"
                     "  p1 = %g  p2 = %g  p3 = %g  p4 = %g  p5 = %g\n"
                     "  g1 = %g  g2 = %g  g3 = %g  g4 = %g" ,
                     a,r1,t1,r2,t2 , p1,p2,p3,p4,p5 ,
                     g1234.a , g1234.b , g1234.c , g1234.d ) ;
   }

   /* store them into the output vector */

   MAKE_doublevec( corvec , ncmax+1 ) ;

   corvec->ar[0] = 1.0 ;      /* correlation at lag 0 */
   corvec->ar[1] = g1234.a ;  /* lag 1 */
   corvec->ar[2] = g1234.b ;  /* lag 2 */
   corvec->ar[3] = g1234.c ;  /* I'll let you guess this one */
   corvec->ar[4] = g1234.d ;  /* lag 4 */

   /* compute further correlations recursively */

   for( nzz=0,ncor=4,kk=5 ; kk < ncmax ; kk++ ){

     /* next correlation, at lag kk */

     cnew =  p1 * corvec->ar[kk-1] + p2 * corvec->ar[kk-2]
           + p3 * corvec->ar[kk-3] + p4 * corvec->ar[kk-4]
           + p5 * corvec->ar[kk-5] ;

     /* and save new value */

     corvec->ar[kk] = cnew ; ncor++ ;  /* ncor = last index saved */

     /* check to see if we've shrunk as far as needed */

     if( abs(cnew) < ccut ){
       nzz++ ;                /* how many undersized in a row? */
       if( nzz > 2 ) break ;  /* 3 strikes and you are OUT! */
     } else {
       nzz = 0 ;              /* not too small ==> reset counter */
     }

   } /* end of recurrence loop */

   /*-- Check for legality as a correlation function, via FFT --*/
   /*-- If not legal, have to scale vrt down! [15 Jul 2020]   --*/

   { int    nfft = csfft_nextup_one35( 2*ncor+15 ) , nf2 = nfft/2 ;
     complex *xc = (complex *)calloc( sizeof(complex) , nfft ) ;
     float xcmin ;

     /* load correlations (+reflections) into FFT array (float not double) */

     xc[0].r = 1.0 ; xc[0].i = 0.0 ;
     for( kk=1 ; kk <= ncor ; kk++ ){
       xc[kk].r = corvec->ar[kk] ; xc[kk].i = 0.0 ;
       xc[nfft-kk] = xc[kk] ; /* reflection from nfft downwards */
     }

     csfft_cox( -1 , nfft , xc ) ;  /* FFT */

     /* find smallest value in FFT; for an acceptable
        autocorrelation function, they should all be positive */

     xcmin = xc[0].r ;
     for( kk=1 ; kk < nf2 ; kk++ ){
       if( xc[kk].r < xcmin ) xcmin = xc[kk].r ;
     }
     free(xc) ;  /* no longer needed by this copy of the universe */

     /* if negative, must scale vrt downwards to avoid Choleski failure */

     if( xcmin <= 0.0f )
       vrtfac = 0.99 / ( 1.0 - (double)xcmin ) ;

     INFO_message("ARMA(5) min FFT(%d) = %g   vrtfac = %g",nfft,xcmin,vrtfac) ;
   }

   /* rescale lagged correlations to allow for additive white noise;
      note that we do NOT scale down the lag=0 correlation of 1.0   */

   vrt *= vrtfac ;

   if( vrt < 1.0 ){
     for( kk=1 ; kk <= ncor ; kk++ ) corvec->ar[kk] *= vrt ;
   }

   /* shrink the output vector to fit what was actually stored */

   RESIZE_doublevec( corvec , ncor+1 ) ;  /* have 0..ncor = ncor+1 of them */

   return corvec ;
}
#endif /* ALLOW_ARMA51 */
/*============================================================================*/
