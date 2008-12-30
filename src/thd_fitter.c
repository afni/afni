/***** This code is part of the AFNI software package, which is   *****
 ***** partly in the public domain and partly covered by the GPL. *****
 ***** See http://afni.nimh.nih.gov/afni for more information.    *****/

#include "mrilib.h"

static floatvec * THD_deconvolve_autopen( int npt    , float *far   ,
                                          int minlag , int maxlag   , float *kern,
                                          int nbase  , float *base[],
                                          int meth   , float *ccon  , int dcon   ,
                                          int pencode, float *penfac_used         );

static int voxid = 0 ;
void THD_fitter_voxid( int i ){ voxid = i; }

/*------------------------------------------------------------------*/
/* Least squares fitting without constraints. (cf mri_matrix.c) */
/*------------------------------------------------------------------*/

static float * new_lsqfit( int npt  , float *far   ,
                           int nref , float *ref[]  )
{
  int  jj ;
  MRI_IMAGE *rmat,*pmat,*smat ;
  float *rar;

ENTRY("new_lsqfit") ;

  /* compute pseudo-inverse of matrix into pmat */

  rmat = mri_new(npt,nref,MRI_float ); rar = MRI_FLOAT_PTR(rmat);
  for( jj=0 ; jj < nref ; jj++ )
    memcpy( rar+jj*npt , ref[jj] , sizeof(float)*npt ) ;
  pmat = mri_matrix_psinv(rmat,NULL,0.0f) ;
  mri_free(rmat) ;
  if( pmat == NULL ) RETURN(NULL) ;  /* should not happen */

  /* create vector of data and multiply by pseudo-inverse */

  rmat = mri_new_vol_empty( npt , 1 , 1 , MRI_float ) ;
  mri_fix_data_pointer( far , rmat ) ;
  smat = mri_matrix_mult( pmat , rmat ) ;
  mri_free(pmat); mri_clear_data_pointer(rmat); mri_free(rmat);
  if( smat == NULL ) RETURN(NULL) ;  /* should not happen */

  /* get pointer to results array and return it */

  rar = MRI_FLOAT_PTR(smat);
  mri_clear_data_pointer(smat); mri_free(smat);
  RETURN(rar) ;
}

/*------------------------------------------------------------------*/

#undef  GOOD_METH
#define GOOD_METH(m) ( (m)==1 || (m)==2 )

/**--- 05 Mar 2008: stuff to get the fitted model back ---**/

static floatvec *gfitv= NULL ;
static int    do_fitv = 0 ;    /* if 1, will compute fitts into fitv */
void       THD_fitter_do_fitts(int qq){ do_fitv = qq; }
floatvec * THD_retrieve_fitts(void){ return gfitv; }

static int       nggfitvv = 0 ;
static floatvec **ggfitvv = NULL ;

static int use_rcmat = 0 ;

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
   int ii,jj , nbad ;
   float *qfit=NULL, val , qmax ;
   floatvec *fv=NULL ;

ENTRY("THD_fitter") ;

   KILL_floatvec(gfitv) ;  /* 05 Mar 2008 */

   /* check inputs for stupid users */

   if( npt  <= 1 || far == NULL ||
       nref <= 0 || ref == NULL || nref >= npt-1 ) RETURN(NULL) ;

   for( jj=0 ; jj < nref ; jj++ ) if( ref[jj] == NULL ) RETURN(NULL) ;

   /*--- 08 Apr 2008: check if some columns are way small;
                      if so, excise them and solve smaller problem ---*/

   qfit = (float *)malloc(sizeof(float)*nref) ;
   for( qmax=0.0f,jj=0 ; jj < nref ; jj++ ){
     for( val=0.0f,ii=0 ; ii < npt ; ii++ ) val += fabsf(ref[jj][ii]) ;
     qfit[jj] = val ; if( val > qmax ) qmax = val ;
   }
   if( qmax == 0.0f ){ free((void *)qfit); RETURN(NULL); }
   qmax *= 0.000333f ;
   for( nbad=jj=0 ; jj < nref ; jj++ ) if( qfit[jj] <= qmax ) nbad++ ;

   if( nbad > 0 ){  /*-- must excise the tiny columns before solving --*/
     int    ngood = nref-nbad ;
     float **qref = (float **)calloc(sizeof(float *),ngood) ;
     floatvec *qv ; float *qcon=NULL ;
     if( ccon != NULL ) qcon = (float *)calloc(sizeof(float),ngood) ;
     for( ii=jj=0 ; jj < nref ; jj++ ){
       if( qfit[jj] > qmax ){
         if( qcon != NULL ) qcon[ii] = ccon[jj] ;
         qref[ii++] = ref[jj] ;
       }
     }
     qv = THD_fitter( npt , far , ngood , qref , meth , qcon ) ;
     if( qcon != NULL ) free((void *)qcon) ;
     free((void *)qref) ;
     if( qv == NULL ){ free((void *)qfit); RETURN(NULL); }
     MAKE_floatvec(fv,nref) ;
     for( ii=jj=0 ; jj < nref ; jj++ ){
       if( qfit[jj] > qmax ) fv->ar[jj] = qv->ar[ii++] ;
     }
     KILL_floatvec(qv) ; free((void *)qfit) ; RETURN(fv) ;
   }

   free((void *)qfit) ; qfit = NULL ;

   /*------- actually solve now -------*/

   switch( meth ){

     default: RETURN(NULL) ;  /* stupid user */

     /*-- least squares --*/

     case 2:
       if( ccon == NULL ){                            /* unconstrained */
         if( use_rcmat )
           qfit = rcmat_lsqfit( npt, far, nref, ref ) ; /* 30 Dec 2008 */
         else
           qfit = new_lsqfit  ( npt, far, nref, ref ) ;
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

   if( qfit == NULL ) RETURN(NULL) ;  /* bad: didn't get output array */

   MAKE_floatvec(fv,nref) ;                      /* copy output array */
   memcpy( fv->ar, qfit, sizeof(float)*nref ) ;  /* into floatvec and */
   free(qfit) ;                                  /* free the trashola */
   if( do_fitv )                                    /* compute fitts? */
     gfitv = THD_fitter_fitts( npt,fv,nref,ref,NULL ); /* 05 Mar 2008 */

   RETURN(fv) ;                                   /* return to caller */
}

/*-------------------------------------------------------------------------*/
/* Get the fitted time series (length npt), given the nref parameters
   in fv (presumably from THD_fitter itself)  and the reference functions
   in ref[0..nref-1].  If far is not NULL, then its contents are
   subtracted from the fit -- thus, giving the residuals.
*//*-----------------------------------------------------------------------*/

floatvec * THD_fitter_fitts( int npt , floatvec *fv ,
                             int nref, float *ref[] , float *far )
{
   int ii , jj ;
   float sum , *qar , pval ;
   floatvec *qv ;

ENTRY("THD_fitter_fitts") ;

   if( npt < 1 || fv == NULL || fv->nar < nref ||
                  nref < 1   || ref == NULL      ) RETURN(NULL) ;

   MAKE_floatvec(qv,npt) ; qar = qv->ar ;
   for( jj=0 ; jj < nref ; jj++ ){
     pval = fv->ar[jj] ;
     for( ii=0 ; ii < npt ; ii++ ) qar[ii] += ref[jj][ii] * pval ;
   }
   if( far != NULL )
     for( ii=0 ; ii < npt ; ii++ ) qar[ii] -= far[ii] ;

   RETURN(qv) ;
}

/*-------------------------------------------------------------------------*/

#if 0
# define ERREX(s) do { ERROR_message(s); RETURN(NULL); } while(0)
#else
# define ERREX(s) RETURN(NULL)
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
    - ccon   = constraints on signs of baseline parameters
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
   methodology for choosing a pfac -- cf. THD_deconvolve_autopen()
*//*----------------------------------------------------------------------*/

floatvec ** THD_deconvolve_multipen( int npt    , float *far   ,
                                     int minlag , int maxlag   , float *kern,
                                     int nbase  , float *base[],
                                     int meth   , float *ccon  , int dcon   ,
                                     int pencode, int npfac    , float *pfac,
                                     float *pres, float *psiz                )
{
   int ii , jj , kk ;
   float val,kernsum, *zar , *zcon=NULL ;
   floatvec **fvv ;
   int nref,nlag,npen,nplu ; float **ref ;
   int p0,p1,p2 , np0,np1,np2 , rp0,rp1,rp2 , ipf ;
   float penfac,fmed,fsig , *qfac , p1scl,p2scl,p1fac,p2fac ;

ENTRY("THD_deconvolve_multipen") ;

   if( nggfitvv > 0 ){
     for( ii=0 ; ii < nggfitvv ; ii++ ) KILL_floatvec(ggfitvv[ii]) ;
     free((void *)ggfitvv) ; nggfitvv = 0 ; ggfitvv = NULL ;
   }

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

   for( kernsum=0.0f,jj=0 ; jj < nlag ; jj++ ){
     val = kern[jj] ; kernsum += val*val ;
   }
   if( kernsum <= 0.0f ) ERREX("e6") ;
   kernsum = sqrtf(kernsum) ;

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

#if 0
   if( AFNI_yesenv("AFNI_TFITTER_VERBOSE") )
     ININFO_message("penalty: P0=%d@%d equations P1=%d@%d P2=%d@%d",
                    np0,rp0,np1,rp1,np2,rp2) ;
#endif

   /* set scale level for negative pfac values */

#if 0
   qmedmad_float  ( npt , far , NULL , &fmed ) ; fmed *= 1.777f ;
   meansigma_float( npt , far , &val , &fsig ) ;
   if( fmed <  fsig ) fmed = fsig ;
   if( fmed == 0.0f ) fmed = fabsf(val) ; /* data is constant? */
   if( fmed == 0.0f ) ERREX("e7") ;       /* data is all zero? */
   fmed = fmed * 1.777 / kernsum ;
#else
   fmed = 0.333f * kernsum ;
#endif
   if( AFNI_yesenv("AFNI_TFITTER_VERBOSE") )
     ININFO_message("default penalty scale factor=%g",fmed) ;

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

   if( do_fitv ){
      ggfitvv = (floatvec **)calloc(sizeof(floatvec *),npfac) ;
     nggfitvv = npfac ;
   }

   p1scl = AFNI_numenv("AFNI_TFITTER_P1SCALE"); if( p1scl <= 0.0f ) p1scl=1.0f ;
   p2scl = AFNI_numenv("AFNI_TFITTER_P2SCALE"); if( p2scl <= 0.0f ) p2scl=1.0f ;

   for( ipf=0 ; ipf < npfac ; ipf++ ){

     /* penalty eqations for deconv (columns #0..npt-1, rows #npt..nplu-1) */

     penfac = qfac[ipf] ;
     if( penfac == 0.0f ) penfac = -0.999f ;
     if( penfac <  0.0f ) penfac = -penfac * fmed ;
     p1fac = p1scl * penfac ;
     p2fac = p2scl * penfac ;

     if( p0 ){
       for( jj=0 ; jj < npt   ; jj++ ) ref[jj][rp0+jj]   =  penfac ;
     }
     if( p1 ){
       for( jj=0 ; jj < npt-1 ; jj++ ) ref[jj][rp1+jj]   = -p1fac ;
       for( jj=1 ; jj < npt   ; jj++ ) ref[jj][rp1+jj-1] =  p1fac ;
     }
     if( p2 ){
       val = -0.5f*p2fac ;
       for( jj=0 ; jj < npt-2 ; jj++ ) ref[jj][rp2+jj]   = val ;
       for( jj=1 ; jj < npt-1 ; jj++ ) ref[jj][rp2+jj-1] = p2fac ;
       for( jj=2 ; jj < npt   ; jj++ ) ref[jj][rp2+jj-2] = val ;
     }

     /***** actually fit the parameters (deconvolution + baseline) *****/

     fvv[ipf] = THD_fitter( nplu , zar , nref , ref , meth , zcon ) ;

     if( do_fitv ) COPY_floatvec( ggfitvv[ipf] , gfitv ) ;

     if( pres != NULL && psiz != NULL && fvv[ipf] != NULL ){
       floatvec *fitv ; float *rar , *sar , rsum=0.0f,ssum=0.0f ;
       fitv = THD_fitter_fitts( nplu,fvv[ipf] , nref,ref , zar ) ;
       rar = fitv->ar ; sar = fvv[ipf]->ar ;
       switch(meth){
         case 1:
           for( ii=0; ii < npt; ii++ ){
             rsum += fabsf(rar[ii]); ssum += fabsf(sar[ii]);
           }
           if( p1 || p2 )
            for( ii=1 ; ii < npt ; ii++ ) ssum += fabsf(sar[ii]-sar[ii-1]) ;
         break ;
         case 2:
           for( ii=0; ii < npt; ii++ ){
             rsum += rar[ii]*rar[ii] ; ssum += sar[ii]*sar[ii] ;
           }
           if( p1 || p2 )
            for( ii=1 ; ii < npt ; ii++ ) ssum += SQR(sar[ii]-sar[ii-1]) ;
           rsum = sqrtf(rsum) ; ssum = sqrtf(ssum) ;
         break ;
       }
       KILL_floatvec(fitv) ; pres[ipf] = rsum ; psiz[ipf] = ssum ;
       if( AFNI_yesenv("AFNI_TFITTER_VERBOSE") )
         ININFO_message("qfac=%g penfac=%g resid=%g norm=%g",
                        qfac[ipf],penfac,rsum,ssum) ;
     }

   }

   /* free the enslaved memory and return to the user */

   if( qfac != pfac ) free((void *)qfac) ;
   if( zcon != NULL ) free((void *)zcon) ;
   free((void *)zar) ;
   for( jj=0 ; jj < nref ; jj++ ) free((void *)ref[jj]) ;
   free((void *)ref) ;

   RETURN(fvv) ;
}

/*-------------------------------------------------------------------------*/
/* The real work is outsourced to the function above, or the one below. */

floatvec * THD_deconvolve( int npt    , float *far   ,
                           int minlag , int maxlag   , float *kern,
                           int nbase  , float *base[],
                           int meth   , float *ccon  , int dcon   ,
                           int pencode, float penfac               )
{
   floatvec **fvv , *fv=NULL ; float pfac=penfac ;

ENTRY("THD_deconvolve") ;

#if 0
   use_rcmat = AFNI_yesenv("RCMAT") ;
#else
   use_rcmat = 1 ;
#endif

   if( pfac == -666.0f || pfac == 0.0f ){
     fv = THD_deconvolve_autopen( npt , far , minlag , maxlag , kern ,
                                  nbase , base , meth , ccon , dcon ,
                                  pencode , NULL ) ;
   } else {
     fvv = THD_deconvolve_multipen( npt , far , minlag , maxlag , kern ,
                                    nbase , base , meth , ccon , dcon ,
                                    pencode , 1 , &pfac , NULL,NULL ) ;

     if( fvv != NULL ){ fv = fvv[0]; free((void *)fvv); }
   }

   use_rcmat = 0 ;

   RETURN(fv) ;
}

/*-------------------------------------------------------------------------*/
/* L curving. */

#undef  NPFAC
#define NPFAC 11

static void fillerup( float bot, float top, int nval, float *val )
{
   int ii ; double fac ;

   if( nval < 1 || val == NULL ) return ;
   if( nval == 1 ){ val[0] = sqrtf(bot*top); return; }
   if( nval == 2 ){ val[0] = bot; val[1] = top; return; }
   fac = pow( fabs(top/bot) , 1.0/(nval-1.0) ) ;
#if 0
   ININFO_message("fillerup: bot=%g top=%g nval=%d fac=%g",bot,top,nval,fac) ;
#endif
   val[0] = bot ;
   for( ii=1 ; ii < nval-1 ; ii++ ) val[ii] = bot * pow(fac,(double)ii) ;
   val[nval-1] = top ; return ;
}

/*-------------------------------------------------------------------------*/

static float ellish( float a , float b , float c ,
                     float d , float e , float f  )
{
   float dd , ee , ss=0.0f ;
   dd = SQR(b-a)+SQR(e-d) ;
   ee = SQR(c-b)+SQR(f-e) ;
   if( dd > 0.0f && ee > 0.0f )
     ss = fabsf((b-a)*(f-e)-(c-b)*(e-d)) / sqrtf(dd*ee) ;
   return ss ;
}

/*-------------------------------------------------------------------------*/
/* Like THD_deconvolve, but choose the penalty factor automatically.
   Will be significantly slower as it searches through penfac space.
*//*-----------------------------------------------------------------------*/

static floatvec * THD_deconvolve_autopen( int npt    , float *far   ,
                                          int minlag , int maxlag   , float *kern,
                                          int nbase  , float *base[],
                                          int meth   , float *ccon  , int dcon   ,
                                          int pencode, float *penfac_used         )
{
   floatvec **fvv=NULL , *fv=NULL , *gv=NULL ;
   float pfac[NPFAC] , pres[NPFAC] , psiz[NPFAC] ;
   float pbot,ptop , ppk , val , rpk,spk ;
   int ii , ipk ;

ENTRY("THD_deconvolve_autopen") ;

   /*--- solve many problems, using a crude mesh in pfac ---*/

   fillerup( -0.01f , -10.0f , NPFAC , pfac ) ;
   memset( (void *)pres , 0 , sizeof(float)*NPFAC ) ;
   memset( (void *)psiz , 0 , sizeof(float)*NPFAC ) ;

   fvv = THD_deconvolve_multipen( npt , far , minlag , maxlag , kern ,
                                  nbase , base , meth , ccon , dcon ,
                                  pencode , NPFAC , pfac , pres,psiz ) ;

   if( fvv == NULL ){
     ERROR_message(
       "THD_deconvolve_autopen failed to solve initial problems: voxel ID=%d",
       voxid ) ;
     RETURN(NULL) ;
   }

   /* find the best combination of residual and solution size */

   ipk = -1 ; ppk = 0.0f ;
#if 0
   for( ii=0 ; ii < NPFAC ; ii++ ){
     val = pres[ii]*psiz[ii] ;
     if( val > ppk ){ ipk = ii; ppk = val; }
   }
#else
   rpk = spk = 0.0f ;
   for( ii=0 ; ii < NPFAC ; ii++ ){
     rpk = MAX(rpk,pres[ii]) ; spk = MAX(rpk,psiz[ii]) ;
   }
   if( rpk > 0.0f ) rpk = 1.0f / rpk ;
   if( spk > 0.0f ) spk = 1.0f / spk ;
   for( ii=0 ; ii < NPFAC ; ii++ ){
     pres[ii] *= rpk ; psiz[ii] *= spk ;
   }
   for( ii=1 ; ii < NPFAC-1 ; ii++ ){
     val = ellish( pres[ii-1],pres[ii],pres[ii+1] ,
                   psiz[ii-1],psiz[ii],psiz[ii+1]  ) ;
#if 0
     if( AFNI_yesenv("AFNI_TFITTER_VERBOSE") ) ININFO_message("ellish[%d] = %g",ii,val) ;
#endif
     if( val > ppk && psiz[ii] > 1.e-5 ){ ipk = ii; ppk = val; }
   }
   if( ipk > 0 && psiz[ipk+1] > 1.e-5 ) ipk++ ;
#endif

   if( ipk < 0 || ppk == 0.0f ){  /* all fits failed?! */
     for( ii=0 ; ii < NPFAC ; ii++ ) KILL_floatvec(fvv[ii]) ;
     free((void *)fvv) ;
     ERROR_message(
       "THD_deconvolve_autopen fails to find initial good fit: voxel ID=%d",
       voxid);
     RETURN(NULL) ;
   }

   fv = fvv[ipk] ; if( do_fitv ){ COPY_floatvec(gv,ggfitvv[ipk]); }
   for( ii=0 ; ii < NPFAC ; ii++ ) if( ii != ipk ) KILL_floatvec(fvv[ii]) ;
   free((void *)fvv) ;
   if( penfac_used != NULL ) *penfac_used = pfac[ipk] ;
   if( AFNI_yesenv("AFNI_TFITTER_VERBOSE") )
     ININFO_message("optimal penfac_used#%d = %g",ipk,pfac[ipk]) ;

   /*--- refinement step in pfac: scan around best one found above ---*/

   pbot = (ipk > 0)       ? 0.8f*pfac[ipk-1] : 0.5f*pfac[0] ;
   ptop = (ipk < NPFAC-1) ? 1.0f*pfac[ipk+1] : 2.0f*pfac[NPFAC-1] ;
   fillerup( pbot , ptop , NPFAC , pfac ) ;
   memset( (void *)pres , 0 , sizeof(float)*NPFAC ) ;
   memset( (void *)psiz , 0 , sizeof(float)*NPFAC ) ;

   fvv = THD_deconvolve_multipen( npt , far , minlag , maxlag , kern ,
                                  nbase , base , meth , ccon , dcon ,
                                  pencode , NPFAC , pfac , pres,psiz ) ;

   if( fvv == NULL ){  /* failed ==> return what we found earlier */
     ERROR_message("THD_deconvolve_autopen semi-failed: voxel ID=%d",voxid) ;
     if( do_fitv ){ KILL_floatvec(gfitv); gfitv = gv; }
     RETURN(fv) ;
   }

   ipk = -1 ; ppk = 0.0f ;
#if 0
   for( ii=0 ; ii < NPFAC ; ii++ ){
     val = pres[ii]*psiz[ii] ;
     if( val > ppk ){ ipk = ii; ppk = val; }
   }
#else
   rpk = spk = 0.0f ;
   for( ii=0 ; ii < NPFAC ; ii++ ){
     rpk = MAX(rpk,pres[ii]) ; spk = MAX(rpk,psiz[ii]) ;
   }
   if( rpk > 0.0f ) rpk = 1.0f / rpk ;
   if( spk > 0.0f ) spk = 1.0f / spk ;
   for( ii=0 ; ii < NPFAC ; ii++ ){
     pres[ii] *= rpk ; psiz[ii] *= spk ;
   }
   for( ii=1 ; ii < NPFAC-1 ; ii++ ){
     val = ellish( pres[ii-1],pres[ii],pres[ii+1] ,
                   psiz[ii-1],psiz[ii],psiz[ii+1]  ) ;
#if 0
     if( AFNI_yesenv("AFNI_TFITTER_VERBOSE") ) ININFO_message("ellish[%d] = %g",ii,val) ;
#endif
     if( val > ppk && psiz[ii] > 1.e-5 ){ ipk = ii; ppk = val; }
   }
   if( ipk > 0 && psiz[ipk+1] > 1.e-5 ) ipk++ ;
#endif

   if( ipk < 0 || ppk == 0.0f ){ /* all failed?  use old result */
     for( ii=0 ; ii < NPFAC ; ii++ ) KILL_floatvec(fvv[ii]) ;
     free((void *)fvv) ;
     ERROR_message("THD_deconvolve_autopen semi-fails: voxel ID=%d",voxid) ;
     if( do_fitv ){ KILL_floatvec(gfitv); gfitv = gv; }
     RETURN(fv) ;
   }

   KILL_floatvec(fv) ; fv = fvv[ipk] ;
   if( do_fitv ){ KILL_floatvec(gv); COPY_floatvec(gv,ggfitvv[ipk]); }
   for( ii=0 ; ii < NPFAC ; ii++ ) if( ii != ipk ) KILL_floatvec(fvv[ii]) ;
   free((void *)fvv) ;
   if( penfac_used != NULL ) *penfac_used = pfac[ipk] ;
   if( AFNI_yesenv("AFNI_TFITTER_VERBOSE") )
     ININFO_message("Optimal penfac_used#%d = %g",ipk,pfac[ipk]) ;

   if( do_fitv ){ KILL_floatvec(gfitv); gfitv = gv; }
   RETURN(fv) ;
}
