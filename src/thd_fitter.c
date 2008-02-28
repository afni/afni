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
   free(qfit) ; return fv ;                      /* return to caller. */
}

/*-------------------------------------------------------------------------*/

#define ERREX(s) do { /** ERROR_message(s); **/ return NULL; } while(0)

floatvec * THD_deconvolve( int npt    , float *far   ,
                           int minlag , int maxlag   , float *kern,
                           int nbase  , float *base[],
                           int meth   , float *ccon  , int dcon   ,
                           int pencode, float penfac               )
{
   int ii , jj , kk ;
   float val,kernmax, *zar , *zcon=NULL ;
   floatvec *fv=NULL ;
   int nref,nlag,npen,nplu ; float **ref ;
   int p0,p1,p2 , np0,np1,np2 , rp0,rp1,rp2 ;

   /* check inputs for stupid users */

   if( npt <= 3 || far == NULL ) ERREX("e1") ;
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

   /* set scale factor for penalty equations */

   if( penfac == 0.0f ) penfac = -0.999f ;
   if( penfac < 0.0f ){
     float fmax ;
     qmedmad_float( npt , far , &val , &fmax ) ;
     if( fmax == 0.0f ) fmax = fabsf(val) ;
     if( fmax == 0.0f ) fmax = 1.0f ;
     penfac = -2.789f * penfac * fmax / kernmax ;
   }

   /* number of equations and number of references */

   nplu = npt + npen ;
   nref = npt + nbase ;
   if( nref > nplu ) ERREX("e8") ;  /* only if user is really stupid */

   /** make new reference vectors **/

   ref = (float **)malloc(sizeof(float *)*nref) ;

   /* deconvolution equations */

   for( jj=0 ; jj < npt ; jj++ ){
     ref[jj] = (float *)calloc(sizeof(float),nplu) ;
     for( ii=0 ; ii < npt ; ii++ ){
       kk = ii-jj ; if( kk < minlag ) continue; if( kk > maxlag ) break ;
       ref[jj][ii] = kern[kk-minlag] ;
     }
   }

   /* penalty eqations */

   if( p0 ){
     for( jj=0 ; jj < npt   ; jj++ ) ref[jj][rp0+jj]   =  penfac ;
   }
   if( p1 ){
     for( jj=0 ; jj < npt-1 ; jj++ ) ref[jj][rp1+jj]   = -penfac ;
     for( jj=1 ; jj < npt   ; jj++ ) ref[jj][rp1+jj-1] =  penfac ;
   }
   if( p2 ){
     float pp = -0.5f*penfac ;
     for( jj=0 ; jj < npt-2 ; jj++ ) ref[jj][rp2+jj]   = pp ;
     for( jj=1 ; jj < npt-1 ; jj++ ) ref[jj][rp2+jj-1] = penfac ;
     for( jj=2 ; jj < npt   ; jj++ ) ref[jj][rp2+jj-2] = pp ;
   }

   /* copy baseline equations (if any) */

   for( jj=0 ; jj < nbase ; jj++ ){
     ref[jj+npt] = (float *)calloc(sizeof(float),nplu) ;
     memcpy( ref[jj+npt] , base[jj] , sizeof(float)*npt ) ;
   }

   /* copy data */

   zar = (float *)calloc(sizeof(float),nplu) ;
   memcpy(zar,far,sizeof(float)*npt) ;

   /* copy constraints? */

   if( ccon != NULL || dcon != 0 ){
     zcon = (float *)calloc(sizeof(float),nref) ;
     if( dcon != 0 )
       for( ii=0 ; ii < npt ; ii++ ) zcon[ii] = (float)dcon ;
     if( nbase > 0 && ccon != NULL )
       memcpy(zcon+npt,ccon,sizeof(float)*nbase) ;
   }

   /** actually fit the parameters (deconvolution + baseline) **/

   fv = THD_fitter( nplu , zar , nref , ref , meth , zcon ) ;

   /* free the enslaved memory and return to the user */

   if( zcon != NULL ) free((void *)zcon) ;
   free((void *)zar) ;
   for( jj=0 ; jj < nref ; jj++ ) free((void *)ref[jj]) ;
   free((void *)ref) ;

   return fv ;
}
