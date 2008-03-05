/***** This code is part of the AFNI software package, which is   *****
 ***** partly in the public domain and partly covered by the GPL. *****
 ***** See http://afni.nimh.nih.gov/afni for more information.    *****/

#include "mrilib.h"

/*------------------------------------------------------------------*/
/* Least squares fitting without constraints. (cf mri_matrix.c) */
/*------------------------------------------------------------------*/

static float * new_lsqfit( int npt  , float *far   ,
                           int nref , float *ref[]  )
{
  int  jj ;
  MRI_IMAGE *rmat,*pmat,*smat ;
  float *rar;

  /* compute pseudo-inverse of matrix into pmat */

  rmat = mri_new(npt,nref,MRI_float ); rar = MRI_FLOAT_PTR(rmat);
  for( jj=0 ; jj < nref ; jj++ )
    memcpy( rar+jj*npt , ref[jj] , sizeof(float)*npt ) ;
  pmat = mri_matrix_psinv(rmat,NULL,0.0f) ;
  mri_free(rmat) ;
  if( pmat == NULL ) return NULL ;  /* should not happen */

  /* create vector of data and multiply by pseudo-inverse */

  rmat = mri_new_vol_empty( npt , 1 , 1 , MRI_float ) ;
  mri_fix_data_pointer( far , rmat ) ;
  smat = mri_matrix_mult( pmat , rmat ) ;
  mri_free(pmat); mri_clear_data_pointer(rmat); mri_free(rmat);
  if( smat == NULL ) return NULL ;  /* should not happen */

  /* get pointer to results array and return it */

  rar = MRI_FLOAT_PTR(smat);
  mri_clear_data_pointer(smat); mri_free(smat);
  return rar ;
}

/*------------------------------------------------------------------*/

#undef  GOOD_METH
#define GOOD_METH(m) ( (m)==1 || (m)==2 )

static floatvec *fitv = NULL ;
static int    do_fitv = 0 ;
void       THD_fitter_do_fitts(int qq){ do_fitv = qq; }
floatvec * THD_retrieve_fitts(void){ return fitv; }  /* 05 Mar 2008 */

/*------------------------------------------------------------------*/
/* Fit the npt-long vector far[] to the nref vectors in ref[].
    - meth=1 ==> L1 fit
    - meth=2 ==> L2 fit (any meth besides 1 or 2 is illegal)
    - ccon != NULL ==> ccon[i] is sign constraint on coef #i
                       ccon[i] = 0 == no constraint
                               > 0 == coef #i must be >= 0
                               < 0 == coef #i must be <= 0
    - Output is vector of coefficiens (nref of them).
    - Output is NULL if some error transpired.
*//*----------------------------------------------------------------*/

floatvec * THD_fitter( int npt , float *far  ,
                       int nref, float *ref[],
                       int meth, float *ccon  )
{
   int jj ;
   float *qfit=NULL, val ;
   floatvec *fv=NULL ;

   KILL_floatvec(fitv) ;  /* 05 Mar 2008 */

   /* check inputs for stupid users */

   if( npt  <= 1 || far == NULL ||
       nref <= 0 || ref == NULL || nref >= npt-1 ) return NULL ;

   for( jj=0 ; jj < nref ; jj++ ) if( ref[jj] == NULL ) return NULL ;

   switch( meth ){

     default: return NULL ;  /* stupid user */

     /*-- least squares --*/

     case 2:
       if( ccon == NULL ){                            /* unconstrained */
         qfit = new_lsqfit( npt, far, nref, ref ) ;
       } else {                                         /* constrained */
         qfit = (float *)malloc(sizeof(float)*nref);   /* output array */
         memcpy(qfit,ccon,sizeof(float)*nref) ;
         val = cl2_solve( npt, nref, far, ref, qfit, 1 ) ; /* cf cl2.c */
         if( val < 0.0f ){ free(qfit); qfit = NULL; }           /* bad */
       }
     break ;

     /*-- L1 fitting --*/

     case 1:
       qfit = (float *)malloc(sizeof(float)*nref) ;          /* output array */
       if( ccon != NULL ) memcpy(qfit,ccon,sizeof(float)*nref) ;
       val = cl1_solve( npt,nref, far,ref, qfit, (ccon!=NULL) ); /* cf cl1.c */
       if( val < 0.0f ){ free(qfit); qfit = NULL; }                   /* bad */
     break ;
   }

   if( qfit == NULL ) return NULL ;  /* bad: didn't get output array */

   MAKE_floatvec(fv,nref) ;                      /* copy output array */
   memcpy( fv->ar, qfit, sizeof(float)*nref ) ;  /* into floatvec and */
   free(qfit) ;                                  /* free the trashola */
   if( do_fitv )
     fitv = THD_fitter_fitts( npt,fv,nref,ref,NULL ) ; /* 05 Mar 2008 */
   return fv ;                                    /* return to caller */
}

/*-------------------------------------------------------------------------*/
/* Get the fitted time series (length npt), given the nref parameters
   in fv (presumably from THD_fitter itself)  and the reference functions
   in ref[0..nref-1].  If far is not NULL, then its contents are
   subtracted from the fit -- thus, giving the residuals.
*//*-----------------------------------------------------------------------*/

floatvec * THD_fitter_fitts( int npt, floatvec *fv,
                             int nref, float *ref[] , float *far )
{
   int ii , jj ;
   float sum , *qar , pval ;
   floatvec *qv ;

   if( npt < 1 || fv == NULL || fv->nar < nref ||
                  nref < 1   || ref == NULL      ) return NULL ;

   MAKE_floatvec(qv,npt) ; qar = qv->ar ;
   for( jj=0 ; jj < nref ; jj++ ){
     pval = fv->ar[jj] ;
     for( ii=0 ; ii < npt ; ii++ ) qar[ii] += ref[jj][ii] * pval ;
   }
   if( far != NULL )
     for( ii=0 ; ii < npt ; ii++ ) qar[ii] -= far[ii] ;

   return qv ;
}

/*-------------------------------------------------------------------------*/

#if 0
# define ERREX(s) do { ERROR_message(s); return NULL; } while(0)
#else
# define ERREX(s) return(NULL)
#endif

/*------------------------------------------------------------------------*/
/* Fit a deconvolution model, using THD_fitter() as the workhorse.
    - npt    = length of time series
    - far    = time series data [0..npt-1]
    - minlag = minimum lag into the past (can be negative) -- usually 0
    - maxlag = maximum lag into the past
    - kern   = kernel to deconvolve = kern[0..maxlag-minlag]
    - nbase  = number of baseline parameters (can be 0)
    - base   = baseline reference functions (can be NULL if nbase==0)
    - meth   = 1 or 2 for L1 or L2 regresstion
    - ccon   = constraints on signs of baselne parameters
    - dcon   = constraint on sign of deconvolved time series s(t)
    - pencode= penalty function for s(t) -- cf. 3dTfitter -help
    - npfac  = number of penalty factors to use
    - pfac   = array of penalty factors (if NULL, program makes them up)

   Return value is an array of npfac floatvec-s, each of which has
   npt+nbase values -- the first npt of which are s(t) and the last
   nbase of which are the baseline parameters.  A NULL return indicates
   some bad input.  An individual NULL floatvec indicates that particular
   value of pfac caused the regression to fail for some hideous reason.

   The purpose of evaluating multiple pfac-s is to implement the L-curve
   methodology for choosing a pfac.  But that part isn't yet written.
*//*----------------------------------------------------------------------*/

floatvec ** THD_deconvolve_multipen( int npt    , float *far   ,
                                     int minlag , int maxlag   , float *kern,
                                     int nbase  , float *base[],
                                     int meth   , float *ccon  , int dcon   ,
                                     int pencode, int npfac    , float *pfac )
{
   int ii , jj , kk ;
   float val,kernmax, *zar , *zcon=NULL ;
   floatvec **fvv ;
   int nref,nlag,npen,nplu ; float **ref ;
   int p0,p1,p2 , np0,np1,np2 , rp0,rp1,rp2 , ipf ;
   float penfac,fmed,fsig , *qfac ;

   /* check inputs for stupid users */

   if( npt <= 3 || far == NULL || npfac < 1 ) ERREX("e1") ;
   nlag = maxlag-minlag+1 ;
   if( nlag <= 1 || kern == NULL || !GOOD_METH(meth) ) ERREX("e2") ;
   if( minlag <= -npt+1 || maxlag >= npt-1 ) ERREX("e3") ;
   if( nbase > 0 ){
     if( base == NULL ) ERREX("e4") ;
     for( jj=0 ; jj < nbase ; jj++ ) if( base[jj] == NULL ) ERREX("e5") ;
   } else if( nbase < 0 ){ /* user = dumb as a brick */
     nbase = 0 ;
   }

   for( kernmax=0.0f,jj=0 ; jj < nlag ; jj++ ){
     val = fabsf(kern[jj]) ; kernmax = MAX(kernmax,val) ;
   }
   if( kernmax == 0.0f ) ERREX("e6") ;

   /* count penalty equations */

   p0   = (pencode & 1) ;    /* which penalty functions are enabled */
   p1   = (pencode & 2) ;
   p2   = (pencode & 4) ;
   if( p0==0 && p1==0 & p2==0 ) p0 = 1 ;  /* must have some penalty */
   np0  = (p0) ? npt   : 0 ;   /* number of equations for each case */
   np1  = (p1) ? npt-1 : 0 ;
   np2  = (p2) ? npt-2 : 0 ;
   rp0  = npt ;                      /* row offset for p0 functions */
   rp1  = rp0 + np0 ;                /* row offset for p1 functions */
   rp2  = rp1 + np1 ;                /* row offset for p2 functions */
   npen = np0+np1+np2 ;        /* total number of penalty equations */

   /* set scale level for negative pfac values */

   qmedmad_float  ( npt , far , NULL , &fmed ) ; fmed *= 1.666f ;
   meansigma_float( npt , far , &val , &fsig ) ;
   if( fmed <  fsig ) fmed = fsig ;
   if( fmed == 0.0f ) fmed = fabsf(val) ; /* data is constant? */
   if( fmed == 0.0f ) ERREX("e7") ;       /* data is all zero? */
   fmed = fmed * 1.666 / kernmax ;

   /* number of equations and number of references */

   nplu = npt + npen ;   /* number of equations */
   nref = npt + nbase ;  /* number of references */
   if( nref > nplu ) ERREX("e8") ;  /* only if user is really stupid */

   /** make new reference vectors (that are nplu long) **/

   ref = (float **)malloc(sizeof(float *)*nref) ;

   /* deconvolution equations (rows and columns #0..npt-1) */

   for( jj=0 ; jj < npt ; jj++ ){
     ref[jj] = (float *)calloc(sizeof(float),nplu) ;
     for( ii=0 ; ii < npt ; ii++ ){
       kk = ii-jj ; if( kk < minlag ) continue; if( kk > maxlag ) break ;
       ref[jj][ii] = kern[kk-minlag] ;
     }
   }

   /* baseline equations, if any (columns #npt..nref-1, rows #0..npt-1) */

   for( jj=0 ; jj < nbase ; jj++ ){
     ref[jj+npt] = (float *)calloc(sizeof(float),nplu) ;
     memcpy( ref[jj+npt] , base[jj] , sizeof(float)*npt ) ;
   }

   /* copy data (rows #0..npt-1) */

   zar = (float *)calloc(sizeof(float),nplu) ;
   memcpy(zar,far,sizeof(float)*npt) ;

   /* copy constraints? */

   if( ccon != NULL || dcon != 0 ){
     zcon = (float *)calloc(sizeof(float),nref) ;
     if( dcon != 0 )  /* sign constraints on deconvolution output */
       for( ii=0 ; ii < npt ; ii++ ) zcon[ii] = (float)dcon ;
     if( nbase > 0 && ccon != NULL ) /* sign constraints on baseline params */
       memcpy(zcon+npt,ccon,sizeof(float)*nbase) ;
   }

   /* make up some penalty factors if not supplied by user */

   if( pfac == NULL ){
     qfac = (float *)malloc(sizeof(float)*npfac) ;
     if( npfac == 1 ){
       qfac[0] = -1.0f ;
     } else {
       float pb,pt,dp ;
       pt = sqrtf((float)npfac); pb = 1.0f/pt;
       dp = powf(pt/pb,1.0f/(npfac-1.0f));
       qfac[0] = -pb ;
       for( ii=1 ; ii < npfac ; ii++ ) qfac[ii] = qfac[ii-1] * dp ;
     }
   } else {
     qfac = pfac ;
   }

   /** loop over different penalty factors **/

   fvv = (floatvec **)calloc(sizeof(floatvec *),npfac) ;

   for( ipf=0 ; ipf < npfac ; ipf++ ){

     /* penalty eqations for deconv (columns #0..npt-1, rows #npt..nplu-1) */

     penfac = qfac[ipf] ;
     if( penfac == 0.0f ) penfac = -0.999f ;
     if( penfac <  0.0f ) penfac = -penfac * fmed ;

     if( p0 ){
       for( jj=0 ; jj < npt   ; jj++ ) ref[jj][rp0+jj]   =  penfac ;
     }
     if( p1 ){
       for( jj=0 ; jj < npt-1 ; jj++ ) ref[jj][rp1+jj]   = -penfac ;
       for( jj=1 ; jj < npt   ; jj++ ) ref[jj][rp1+jj-1] =  penfac ;
     }
     if( p2 ){
       val = -0.5f*penfac ;
       for( jj=0 ; jj < npt-2 ; jj++ ) ref[jj][rp2+jj]   = val ;
       for( jj=1 ; jj < npt-1 ; jj++ ) ref[jj][rp2+jj-1] = penfac ;
       for( jj=2 ; jj < npt   ; jj++ ) ref[jj][rp2+jj-2] = val ;
     }

     /***** actually fit the parameters (deconvolution + baseline) *****/

     fvv[ipf] = THD_fitter( nplu , zar , nref , ref , meth , zcon ) ;

   }

   /* free the enslaved memory and return to the user */

   if( qfac != pfac ) free((void *)qfac) ;
   if( zcon != NULL ) free((void *)zcon) ;
   free((void *)zar) ;
   for( jj=0 ; jj < nref ; jj++ ) free((void *)ref[jj]) ;
   free((void *)ref) ;

   return fvv ;
}

/*-------------------------------------------------------------------------*/
/* The real work is outsourced to the function above. */

floatvec * THD_deconvolve( int npt    , float *far   ,
                           int minlag , int maxlag   , float *kern,
                           int nbase  , float *base[],
                           int meth   , float *ccon  , int dcon   ,
                           int pencode, float penfac               )
{
   floatvec **fvv , *fv ; float pfac=penfac ;

   fvv = THD_deconvolve_multipen( npt , far , minlag , maxlag , kern ,
                                  nbase , base , meth , ccon , dcon ,
                                  pencode , 1 , &pfac ) ;

   if( fvv == NULL ) return NULL ;
   fv = fvv[0] ; free((void *)fvv) ; return fv ;
}
