#include "mrilib.h"

/*---------------------------------------------------------------------------*/

#undef  A
#define A(i,j) asym[(i)+(j)*mm]

/*----------------------------------------------------------------------------*/
/*  - Must have fewer columns than rows [fim->nx >= fim->ny >= 2]
    - tau > 0.0
    - sv non-NULL ==> it gets the fim->ny pre-shrink singular values
                      in ascending order
    - return value is number of non-zero singular values after
      shrinkage; if -1 is returned, then something bad happened and
      the input is unchanged
*//*--------------------------------------------------------------------------*/

int mri_svd_shrink( MRI_IMAGE *fim , float tau , float *sv )
{
   int nn , n1 , mm , ii,jj,kk , kbot,mev ;
   double *asym,*sval,*eval, *fv1,*fv2,*fv3,*fv4,*fv5,*fv6,*fv7,*fv8,*vv ;
   register double sum ; register float *xj , *xk ; float *xx ;
   integer imm , imev , *iv1 , ierr ;
   double tcut ;

   if( fim == NULL || fim->kind != MRI_float || tau <= 0.0f ) return -1 ;

   nn = fim->nx ; mm = fim->ny ;      if( nn < mm || mm < 2 ) return -1 ;
   xx = MRI_FLOAT_PTR(fim) ;                 if( xx == NULL ) return -1 ;
   n1 = nn-1 ; tcut = (double)tau ;

   asym = (double *)malloc(sizeof(double)*mm*mm) ;  /* symmetric matrix */
   eval = (double *)calloc(sizeof(double),mm) ;     /* its eigenvalues */
   sval = (double *)malloc(sizeof(double)*mm) ;     /* scaling values */

   /** setup m x m [A] = [X]'[X] matrix to eigensolve **/

   for( jj=0 ; jj < mm ; jj++ ){
     xj = xx + jj*nn ;               /* j-th column */
     for( kk=0 ; kk <= jj ; kk++ ){
       sum = 0.0 ; xk = xx + kk*nn ; /* k-th column */
       for( ii=0 ; ii < n1 ; ii+=2 ) sum += xj[ii]*xk[ii] + xj[ii+1]*xk[ii+1];
       if( ii == n1 ) sum += xj[ii]*xk[ii] ;
       A(jj,kk) = sum ; if( kk < jj ) A(kk,jj) = sum ;
     }
   }

   /** reduction to tridiagonal form (stored in fv1..3) **/

   fv1 = (double *)malloc(sizeof(double)*(mm+9)) ;  /* workspaces */
   fv2 = (double *)malloc(sizeof(double)*(mm+9)) ;
   fv3 = (double *)malloc(sizeof(double)*(mm+9)) ;
   fv4 = (double *)malloc(sizeof(double)*(mm+9)) ;
   fv5 = (double *)malloc(sizeof(double)*(mm+9)) ;
   fv6 = (double *)malloc(sizeof(double)*(mm+9)) ;
   fv7 = (double *)malloc(sizeof(double)*(mm+9)) ;
   fv8 = (double *)malloc(sizeof(double)*(mm+9)) ;
   iv1 = (integer *)malloc(sizeof(integer)*(mm+9)) ;
   imm = (integer)mm ;

   tred1_( &imm , &imm , asym , fv1,fv2,fv3 ) ;

   /** find all the eigenvalues of the tridiagonal matrix **/

   (void)imtqlv_( &imm , fv1,fv2,fv3 , eval , iv1 , &ierr , fv4 ) ;

   /** convert to singular values [ascending order],
       and then to scaling values for eigenvectors  **/

   kbot = -1 ;  /* index of first nonzero scaling value */
   for( ii=0 ; ii < mm ; ii++ ){
     sval[ii] = (eval[ii] <= 0.0) ? 0.0 : sqrt(eval[ii]) ;
     if( sv != NULL ) sv[ii] = (float)sval[ii] ;  /* save singular values */
     if( sval[ii] <= tcut ){   /* too small ==> scaling value is zero */
       sval[ii] = 0.0 ;
     } else {                  /* scale factor for ii-th eigenvector (< 1) */
       sval[ii] = (sval[ii]-tcut) / sval[ii] ;
       if( kbot < 0 ) kbot = ii ;
     }
   }

   if( kbot < 0 ){  /*** all singular values are smaller than tau? ***/
     free(iv1) ;
     free(fv8) ; free(fv7) ; free(fv6) ; free(fv5) ;
     free(fv4) ; free(fv3) ; free(fv2) ; free(fv1) ;
     free(sval) ; free(asym) ; return -1 ;
   }

   /** find eigenvectors, starting at the kbot-th one **/

   mev = mm - kbot ;  /* number of eigenvectors to compute */
   vv  = (double *)calloc(sizeof(double),mm*mev) ;

   if( kbot > 0 ){  /* shift scaling values down to start at index=0 */
     for( kk=0 ; kk < mev ; kk++ ) sval[kk] = sval[kk+kbot] ;
   }

   imev = (integer)mev ;
   (void)tinvit_( &imm , &imm , fv1,fv2,fv3 , &imev , eval+kbot , iv1 ,
                  vv , &ierr , fv4,fv5,fv6,fv7,fv8 ) ;

   /** back transform eigenvectors to original space **/

   (void)trbak1_( &imm , &imm , asym , fv2 , &imev , vv ) ;

   free(iv1) ;
   free(fv8) ; free(fv7) ; free(fv6) ; free(fv5) ;
   free(fv4) ; free(fv3) ; free(fv2) ; free(fv1) ; free(eval) ;

   /** form m x m transformation matrix [V] diag[sval] [V]' into asym **/

   for( ii=0 ; ii < mm ; ii++ ){
     for( jj=0 ; jj <= ii ; jj++ ){
       sum = 0.0 ;
       for( kk=0 ; kk < mev ; kk++ )
         sum += vv[ii+kk*mm]*vv[jj+kk*mm]*sval[kk] ;
       A(ii,jj) = sum ; if( jj < ii ) A(jj,ii) = sum ;
     }
   }
   free(vv) ; free(sval) ;

   /** transform input matrix (in place) **/

   xk = (float *)malloc(sizeof(float)*mm) ;
   for( ii=0 ; ii < nn ; ii++ ){
     for( jj=0 ; jj < mm ; jj++ ){
       sum = 0.0 ;
       for( kk=0 ; kk < mm ; kk++ ) sum += xx[ii+kk*nn]*A(kk,jj) ;
       xk[jj] = (float)sum ;
     }
     for( kk=0 ; kk < mm ; kk++ ) xx[ii+kk*nn] = xk[kk] ;
   }

   /** vamoose the ranch **/

   free(xk) ; free(asym) ;
   return mev ;
}

/*----------------------------------------------------------------------------*/
/* Shrink every entry towards (or all the way to) zero. */

int mri_abs_shrink( MRI_IMAGE *fim , float tau )
{
   register float tpos,tneg,val , *xx ;
   register int nvox , ii , nsh ;

      if( fim == NULL || fim->kind != MRI_float || tau <= 0.0f ) return ;

   nvox = fim->nvox ; xx = MRI_FLOAT_PTR(fim) ; if( xx == NULL ) return ;
   tpos = tau ; tneg = -tpos ;

   for( nsh=ii=0 ; ii < nvox ; ii++ ){
     val = xx[ii] ;
          if( val < tneg ){ val += tpos ; nsh++ ; } /* move up */
     else if( val > tpos ){ val -= tpos ; nsh++ ; } /* move down */
     else                  val  = 0.0f ;            /* too close to zero */
     xx[ii] = val ;
   }
   return nsh ;
}

/*----------------------------------------------------------------------------*/
/* Shrink in SVD, then shrink in abs.
   Inputs lim, sim, and yim are modified in place.
*//*--------------------------------------------------------------------------*/

float_pair mri_pc_pursuit_step( float lam , float mu ,
                                MRI_IMAGE *mim , MRI_IMAGE *lim ,
                                MRI_IMAGE *sim , MRI_IMAGE *yim  )
{
   int nn=mim->nx , mm=mim->ny , nxy=nn*mm , ii ;
   float *mar=MRI_FLOAT_PTR(mim) , *lar=MRI_FLOAT_PTR(lim) ;
   float *sar=MRI_FLOAT_PTR(sim) , *yar=MRI_FLOAT_PTR(yim) ;
   float xmu=mu , xlam=lam , sum ;
   MRI_IMAGE *tim ; float *tar ; float_pair lsdif ;

   tim = mri_copy(lim) ; tar = MRI_FLOAT_PTR(tim) ;

   /* form M - S + mu*Y into L */

   for( ii=0 ; ii < nxy ; ii++ )
     lar[ii] = mar[ii] - sar[ii] + xmu*yar[ii] ;

   /* shrink it */

   ii = mri_svd_shrink( lim , mu , NULL ) ;
fprintf(stderr,"svd_shrink = %d\n",ii) ;

   sum = 0.0f ;
   for( ii=0 ; ii < nxy ; ii++ ) sum += fabsf( lar[ii] - tar[ii] ) ;
   lsdif.a = sum/nxy ;

   /* form M - L + mu*Y into S */

   for( ii=0 ; ii < nxy ; ii++ ){
     tar[ii] = sar[ii] ;
     sar[ii] = mar[ii] - lar[ii] + xmu*yar[ii] ;
   }

   /* shrink it */

   ii = mri_abs_shrink( sim , xlam*mu ) ;
fprintf(stderr,"abs_shrink = %d\n",ii) ;

   sum = 0.0f ;
   for( ii=0 ; ii < nxy ; ii++ ) sum += fabsf( sar[ii] - tar[ii] ) ;
   lsdif.b = sum/nxy ;

   /* form Y += mu*(M-L-S) */

   for( ii=0 ; ii < nxy ; ii++ )
     yar[ii] += xmu*(mar[ii]-lar[ii]-sar[ii]) ;

   mri_free(tim) ; return lsdif ;
}

/*----------------------------------------------------------------------------*/

MRI_IMARR * mri_pc_pursuit( MRI_IMAGE *mim , float lam , float mu )
{
   MRI_IMAGE *sim , *lim , *yim ;
   MRI_IMARR *lsar ;
   float_pair lsdif ;
   int nite ;

   if( mim == NULL || mim->kind != MRI_float ||
       mim->nx < mim->ny || mim->ny < 2         ) return NULL ;

   /* initial setup: L = M, S = Y = 0 */

   lim = mri_copy(mim) ;
   sim = mri_new_conforming(mim,MRI_float) ;
   yim = mri_new_conforming(mim,MRI_float) ;

   for( nite=1 ; nite <= 19 ; nite++ ){
     lsdif = mri_pc_pursuit_step( lam , mu , mim , lim , sim, yim ) ;
fprintf(stderr,"Iter #%02d: ldif = %g  sdif = %g\n",nite,lsdif.a,lsdif.b) ;
   }

   INIT_IMARR(lsar) ;
   ADDTO_IMARR(lsar,lim) ;
   ADDTO_IMARR(lsar,sim) ;
   mri_free(yim) ;
   return lsar ;
}

/*----------------------------------------------------------------------------*/

int main( int argc , char *argv[] )
{
   MRI_IMAGE *fim , *lim , *sim ;
   MRI_IMARR *lsar ;
   float lam , mu ;

   if( argc < 4 ){ printf("%s inim limout simout\n",argv[0]) ; exit(0) ; }
   fim = mri_read_1D(argv[1]) ;
   if( fim == NULL ) exit(0) ;

   lam = 1.0f / sqrtf((float)fim->nx) ;
   mu  = 4.0f * mri_matrix_size(fim) ;

   lsar = mri_pc_pursuit( fim , lam , mu ) ;

   if( lsar == NULL ) exit(0) ;
   lim = IMARR_SUBIM(lsar,0) ; mri_write_1D( argv[2] , lim ) ;
   sim = IMARR_SUBIM(lsar,1) ; mri_write_1D( argv[3] , sim ) ;
   exit(0) ;
}
