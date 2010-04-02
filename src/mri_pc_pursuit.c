#include "mrilib.h"

#ifdef USE_OMP
# include <omp.h>
#endif

#undef  A
#define A(i,j) asym[(i)+(j)*mm]

/*---------------------------------------------------------------------------*/

static MRI_IMAGE * mri_make_xxt( MRI_IMAGE *fim )
{
   MRI_IMAGE *aim ;
   int nn , mm , n1 ; float *xx ;

                if( fim == NULL || fim->kind != MRI_float ) return NULL ;
   nn = fim->nx ; mm = fim->ny ;    if( nn < mm || mm < 2 ) return NULL ;
   xx = MRI_FLOAT_PTR(fim) ;               if( xx == NULL ) return NULL ;
   n1 = nn-1 ;

   aim = mri_new( mm , mm , MRI_double ) ; asym = MRI_DOUBLE_PTR(aim) ;

   /** setup m x m [A] = [X]'[X] matrix to eigensolve **/

#pragma omp parallel if( mm > 7 && nn > 999 )
 { int jj , kk , ii ; register double sum ; register float *xj,*xk ;
   AFNI_OMP_START ;
#pragma omp for
   for( jj=0 ; jj < mm ; jj++ ){
     xj = xx + jj*nn ;               /* j-th column */
     for( kk=0 ; kk <= jj ; kk++ ){
       sum = 0.0 ; xk = xx + kk*nn ; /* k-th column */
       for( ii=0 ; ii < n1 ; ii+=2 ) sum += xj[ii]*xk[ii] + xj[ii+1]*xk[ii+1];
       if( ii == n1 ) sum += xj[ii]*xk[ii] ;
       A(jj,kk) = sum ; if( kk < jj ) A(kk,jj) = sum ;
     }
   }
   AFNI_OMP_END ;
 } /* end OpenMP */

   return aim ;
}

/*----------------------------------------------------------------------------*/

static float mri_svd_max( MRI_IMAGE *fim )
{
   MRI_IMAGE *aim ;
   double ev=0.0 ;
   int mm ; float sv ;

   aim = mri_make_xxt( fim ) ; if( aim == NULL ) return -1.0f ;
   mm  = aim->nx ;

   (void)symeig_irange( mm, MRI_DOUBLE_PTR(aim) , &ev , mm-1,mm-1 , 1 ) ;
   mri_free(aim) ;
   sv = (ev > 0.0) ? (float)sqrt(ev) : 0.0f ;
   return sv ;
}

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
   MRI_IMAGE *aim ;
   double *asym,*sval,*eval, *fv1,*fv2,*fv3,*fv4,*fv5,*fv6,*fv7,*fv8,*vv ;
   register double sum ; register float *xk ; float *xx ;
   integer imm , imev , *iv1 , ierr ;
   double tcut ;

   if( fim == NULL || fim->kind != MRI_float || tau <= 0.0f ) return -1 ;

   nn = fim->nx ; mm = fim->ny ;      if( nn < mm || mm < 2 ) return -1 ;
   xx = MRI_FLOAT_PTR(fim) ;                 if( xx == NULL ) return -1 ;
   n1 = nn-1 ; tcut = (double)tau ;

   aim  = mri_make_xxt( fim ) ;             if( aim == NULL ) return -1 ;

   asym = MRI_DOUBLE_PTR(aim) ;                  /* symmetric matrix */
   eval = (double *)calloc(sizeof(double),mm) ;  /* its eigenvalues */
   sval = (double *)malloc(sizeof(double)*mm) ;  /* scaling values */

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
     free(sval) ; mri_free(aim) ; return -1 ;
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

   free(xk) ; mri_free(aim) ;
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
   Inputs aim, bim, eim, fim are modified in place.
*//*--------------------------------------------------------------------------*/

void mri_pc_pursuit_step( float lam , float mu , float ac ,
                          MRI_IMAGE *dim ,
                          MRI_IMAGE *aim , MRI_IMAGE *eim
                          MRI_IMAGE *bim , MRI_IMAGE *fim )
{
   int nn=dim->nx , mm=dim->ny , nxy=nn*mm ;
   float *dar=MRI_FLOAT_PTR(dim) ;
   float *aar=MRI_FLOAT_PTR(aim) , *bar=MRI_FLOAT_PTR(bim) ;
   float *ear=MRI_FLOAT_PTR(eim) , *far=MRI_FLOAT_PTR(fim) ;
   float xmu=mu , xlam=lam , xac=ac , sum ;

   MRI_IMAGE *yaim , *yeim ;
   float     *yaar , *year ;
   register int ii ; register float val ;

   /** form [yaim] = [aim] + ac*([aim]-[bim])
            [yeim] = [eim] + ac*([eim]-[fim]) **/

   yaim = mri_new_conforming(dim,MRI_float) ; yaar = MRI_FLOAT_PTR(yaim) ;
   yeim = mri_new_conforming(dim,MRI_float) ; year = MRI_FLOAT_PTR(yeim) ;
   if( xac <= 0.0f ){
     memcpy( yaar , aar , sizeof(float)*nxy ) ;
     memcpy( year , ear , sizeof(float)*nxy ) ;
   } else {
     register float af , bf ;
     af = 1.0f + xac ; bf = xac ;
     for( ii=0 ; ii < nxy ; ii++ ){
       yaar[ii] = af*aar[ii] - bf*bar[ii] ;
       year[ii] = af*ear[ii] - bf*far[ii] ;
     }
   }

   /* copy contents of aim into bim, and eim into fim,  */
   /* so on output, bim is the old aim, aim is updated, */
   /* and mutatis mutandum for fim and eim.             */

   memcpy( bar , aar , sizeof(float)*nxy ) ;
   memcpy( far , ear , sizeof(float)*nxy ) ;

   /** form [aim] = [yaim] - 0.5*([yaim]+[yeim]-[dim]) **/
   /**  and [eim] = [yeim] - ditto                     **/

   for( ii=0 ; ii < nxy ; ii++ ){
     val = 0.5f * ( yaar[ii] + year[ii] - dar[ii] ) ;
     aar[ii] = yaar[ii] - val ;
     ear[ii] = year[ii] - val ;
   }

   /** now shrink [aim] and [eim] in their own special ways **/

   ii = mri_shrink_svd( aim , 0.5f*xmu , NULL ) ;
fprintf(stderr,"svd_shrink dimen = %d\n",ii) ;

   ii = mri_abs_shrink( eim , 0.5f*xlam*mu ) ;
fprintf(stderr,"abs_shrink count = %d\n",ii) ;

   mri_free(yeim) ; mri_fee(yaim) ; return ;
}

/*----------------------------------------------------------------------------*/

MRI_IMARR * mri_pc_pursuit( MRI_IMAGE *dim , float lam , float mubot )
{
   MRI_IMARR *lsar ;
   MRI_IMAGE *aim, *bim , *eim, *fim ;
   float mu , *dar ;
   int nite ;

   if( dim == NULL || mim->kind != MRI_float ||
       dim->nx < dim->ny || dim->ny < 2         ) return NULL ;

   if( lam <= 0.0f ) lam = 1.0f / sqrtf((float)dim->nx) ;

   /* get largest singular value of [dim] */

   mu = mri_svd_max( dim ) ;     if( mu <= 0.0f ) return NULL ;
   mu *= 0.911f ; if( mu < mubot ) mu = mubot ;

   /* ..... */

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
